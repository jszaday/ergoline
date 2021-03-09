package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.CheckTypes.MissingSpecializationException
import edu.illinois.cs.ergoline.passes.{CheckTypes, TypeCheckContext}
import edu.illinois.cs.ergoline.proxies.ProxyManager
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichEirNode, RichIntOption, RichOption}
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirType
import edu.illinois.cs.ergoline.util.{Errors, addExplicitSelf, assertValid, extractFunction, sweepInherited}

import scala.collection.View
import scala.reflect.ClassTag

object Find {
  type EirNamedScope = EirScope with EirNamedNode

  def ancestors(x: EirNode): Seq[EirNode] = x.parent match {
    case Some(parent) => parent +: ancestors(parent)
    case None => Nil
  }

  // find where x's ancestors first overlap with y's
  def commonAncestor(x: EirNode, y: EirNode): Option[EirNode] = {
    val ancestors = Find.ancestors(x).toList
    Find.ancestors(y).find(ancestors.contains(_))
  }

  def withName[T <: EirNamedNode](name: String): T => Boolean =
    (n: T) => n.name == name

  def descendant(node: EirNode, predicate: EirNode => Option[Boolean], stack: List[EirNode] = Nil): Iterable[EirNode] = {
    Option.unless(stack.contains(node))({
      node.children.view.zip(node.children.view.map(predicate)).collect({
        case (child, Some(x)) => Option.when(x)(child) ++ descendant(child, predicate, stack :+ node)
      }).flatten
    }).getOrElse(Nil)
  }

  // recursively check all children of a node
  def within[T <: EirNode](node: EirNode, predicate: T => Boolean)(implicit tag: ClassTag[T]): View[T] = {
    Find.child[T](node, predicate).concat(node.children.view.flatMap(within[T](_, predicate)))
  }

  // check only the immediate children of the node (do not descend)
  def child[T <: EirNode](node: EirNode, predicate: T => Boolean)(implicit tag: ClassTag[T]): View[T] = {
    node.children.view.filter(matchesPredicate(predicate)).map(_.asInstanceOf[T])
  }

  def namedChild[T <: EirNamedNode](node: Option[EirNamedNode], name: String)(implicit tag: ClassTag[T]): T = {
    node.flatMap(firstNamedChild[T](_, name))
      .getOrElse(Errors.unableToResolve(s"${node.map(_.name).getOrElse("???")}::$name"))
  }

  private def firstNamedChild[T <: EirNamedNode](node: EirNode, name: String)(implicit tag: ClassTag[T]): Option[T] = {
    node.children.view.collect {
      case n: EirNamedNode if n.name == name => n
    }.map {
      case r: EirResolvable[_] if !r.resolved => Find.uniqueResolution[EirNode](null, r)
      case n => n
    }.collectFirst {
      case t: T => t
    }
  }

  def tryResolve(x: EirResolvable[_])(implicit tyCtx: TypeCheckContext): Option[EirNode] = {
    val found = typedResolve(x)(manifest[EirNode], tyCtx)
    if (found.isEmpty) return None
    found match {
      case (f: EirFunctionArgument) :: _ if f.isSelfAssigning => Some(f)
      case head :: _ if !globals.strict => Some(head)
      // check to ensure unique resolution
      case head :: rest if globals.strict =>
        val all: List[EirNode] = head +: rest
        if (all.length != all.distinct.length) Errors.warn(s"repeated resolutions of $x")
        if (all.length > 1) Errors.warn(s"potential ambiguity, selected $head from $all")
        Some(head)
      case _ => None
    }
  }

  def uniqueResolution[A <: EirNode : Manifest](ctx: TypeCheckContext, x: EirResolvable[_]): A = {
    typedResolve[A](x)(manifest[A], ctx).headOption.getOrElse({
      Errors.unableToResolve(x)
    })
  }

  def typedResolve[A <: EirNode](x: EirResolvable[_])(implicit manifest: Manifest[A], tyCtx: TypeCheckContext): Seq[A] = {
    (x match {
      case t: EirProxyType => Seq(ProxyManager.proxyFor(tyCtx, t))
      case s: EirSymbolLike[_] => fromSymbol(s)
      case _ => x.resolve()
    }).collect { case a: A => a }
  }

  def unionResolvable(x: EirType, rY: EirResolvable[EirType])(implicit tyCtx: TypeCheckContext): Option[EirType] = {
    Find.typedResolve[EirType](rY)
        .headOption.map(y => unionType(x, y)) match {
      case Some(x) => x
      case None => Some(x)
    }
  }

  private def correspond(ctx: TypeCheckContext, s: EirSpecializedSymbol[_], base: EirSpecializable, specs: List[EirSpecialization]): Seq[EirType] = {
    Nil
  }

  def safeResolve(tyCtx: TypeCheckContext, x: EirResolvable[_]): Seq[EirNode] = {
    x match {
      case s: EirSpecializedSymbol[_] =>
        val base: Option[EirSpecializable] =
          s.disambiguation.to[EirSpecializable]
            .orElse(safeResolve(tyCtx, s.base).collectFirst { case s: EirSpecializable => s })

        (base, base.flatMap(tyCtx.checked.get(_))) match {
          case (Some(base), Some(head :: Nil)) => Seq(tyCtx.getTemplatedType(base, head.types.map(assertValid[EirType])))
          case (Some(base), Some(specs)) if specs.length > 1 => correspond(tyCtx, s, base, specs)
          case (Some(base), None) => Seq(makeTemplateType(base, s)(tyCtx))
          case _ => ???
        }
      case s: EirSymbol[_] => fromSymbol(s)(tyCtx)
      case _ => typedResolve(x)(manifest[EirNode], tyCtx)
    }
  }

  def unionType(types: EirType*)(implicit tyCtx: TypeCheckContext): Option[EirType] = unionType(types)

  def unionType(types: Iterable[EirType])(implicit tyCtx: TypeCheckContext): Option[EirType] = {
    val distinct = types.toList.distinct
    distinct.tail.foldRight(distinct.headOption)((x, oY) => oY match {
      case Some(y) => unionType(x, y)
      case None => None
    })
  }

  def unionType(x: EirType, y: EirType)(implicit tyCtx: TypeCheckContext): Option[EirType] = {
    if (x == y) Some(x)
    else if (x.canAssignTo(y)) Some(y)
    else Some(x).filter(y.canAssignTo)
    // TODO seek common ancestor classes and cast
  }

  def parentOf[T <: EirNode : Manifest](node: EirNode): Option[T] =
    node.parent.to[T].orElse(node.parent.flatMap(parentOf[T]))

  def traits(x: EirClassLike)(implicit tyCtx: TypeCheckContext): Set[EirType] = _traits(x).toSet

  def isTopLevel(x: EirNode): Boolean = x match {
    // TODO can this simplify to?
    //      case _ : EirScope => true
    case _: EirBlock => true
    case _: EirForLoop => true
    case _: EirLambdaExpression => true
    case _: EirFunction => true
    case _: EirClassLike => true
    case _: EirNamespace => true
    case _: EirImport => true
    case _: EirMatchCase => true
    case _: EirExpressionPattern => true
    case _: EirSdagWhen => true
    case _ => false
  }

  import FindSyntax.RichPredicate

  // NOTE when trying to resolve members this seems to return
  //      both the EirMember itself and its .member
  //      e.g. EirMember("foo"...), EirFunction("foo"...)
  //      need to figure out a way around this?
  def anywhereAccessible(ctx: EirNode, name: String)(implicit tyCtx: TypeCheckContext): View[EirNamedNode] = {
    val ancestors = Find.ancestors(ctx).filter(isTopLevel)
    val matches = matchesPredicate(withName(name))(_)
    val predicate: EirNode => Option[Boolean] = {
      case _: EirBlock => None
      // only allowed to consider members when within the class?
      case x: EirMember => Option.when(matches(x) && ancestors.contains(x.base))(false)
      case x: EirNamespace => Option.when(matches(x) || ancestors.contains(x))(matches(x))
      case x: EirClassLike => Option.when(!ancestors.contains(x) || matches(x))(matches(x))
      case x: EirImport =>
        Option.when(Find.commonAncestor(ctx, x).exists(ancestors.contains(_)))(matches(x))
      case x if !isTopLevel(x) || x.isInstanceOf[EirFunction] => Option.when(matches(x))(true)
      // TODO how to handle arguments?
//      case x: EirFunction => Option.when(matches(x))(true)
      case x => Some(matches(x))
    }
    ancestors.view.flatMap(ancestor => {
      Find.descendant(ancestor, predicate).filter(x => {
        ancestor match {
          case block: EirBlock =>
            block.findPositionOf(ctx) > block.findPositionOf(x)
          case _ => true
        }
      }) ++ {
        // Descendent searches only include descendents,
        // so we need an explicit check for the last ancestor
        val isLast = ancestors.lastOption.contains(ancestor)
        Option.when(isLast && matches(ancestor))(ancestor)
      }
    }).filter(ctx.canAccess(_)).map({
      case fs: EirFileSymbol => fs.resolve().head
      case x => x
    }).map(_.asInstanceOf[EirNamedNode])
  }

  private def makeTemplateType(x: EirNode, sp: EirSpecializedSymbol[_])
                              (implicit tyCtx: TypeCheckContext): EirTemplatedType = {
    EirTemplatedType(None, assertValid[EirResolvable[EirType]](x), {
      x match {
        case s: EirSpecializable if s.templateArgs.length > sp.types.length =>
          sp.types ++ s.templateArgs.slice(sp.types.length, s.templateArgs.length).flatMap(_.defaultValue)
        case _ => sp.types
      }
    })
  }

  def fromSymbol(s: EirSymbolLike[_])(implicit tyCtx: TypeCheckContext): Seq[EirNode] = {
    s match {
      case sp: EirSpecializedSymbol[_] =>
        val base = sp.disambiguation match {
          case Some(s) => Seq(s)
          case None => fromSymbol(sp.base)
        }
        base.map(makeTemplateType(_, sp))
      case symbol: EirSymbol[_] => fromSymbol(symbol)
//      case s if s.disambiguation.isDefined => s.disambiguation.toSeq
      case _ => Nil
    }
  }

  def fromSymbol(symbol: EirSymbol[_])(implicit tyCtx: TypeCheckContext): Seq[EirNamedNode] = {
    (symbol.qualifiedName match {
      case last +: Nil =>
        anywhereAccessible(symbol, last)
      case head :: tail =>
        anywhereAccessible(symbol, head).flatMap(qualified(symbol, _, tail))
    }).toSeq
  }

  // TODO use this!!
  def uniqueResolution[T <: EirNode : Manifest](iterable: Iterable[EirResolvable[T]])(implicit tyCtx: TypeCheckContext): Iterable[T] = {
    iterable.map(x => typedResolve[T](x).headOption.getOrElse(Errors.unableToResolve(x)))
  }

  private def qualified(usage: EirNode, scope: EirNamedNode, names: List[String])(implicit tyCtx: TypeCheckContext): Seq[EirNamedNode] = {
    if (names.nonEmpty) {
      val last = names.init.foldRight(Iterable(scope))((name, curr) => {
        curr.headOption.view.flatMap(child[EirNamedNode](_, withName(name).and(usage.canAccess)))
      })
      last.flatMap(child(_, withName(names.last).and(usage.canAccess)).map({
        case fs: EirFileSymbol => fs.resolve().head.asInstanceOf[EirNamedNode]
        case x => x
      })).toSeq
    } else {
      Seq(scope)
    }
  }

  def asClassLike(ty: EirType): EirClassLike = tryClassLike(ty) match {
    case Some(c) => c
    case None => Errors.incorrectType(ty, classOf[EirClassLike])
  }

  def tryClassLike(ty: EirNode): Option[EirClassLike] = ty match {
    case c: EirClassLike => Some(c)
    case t: EirTemplatedType => Option(t.base).to[EirType].map(asClassLike)
    case _ => None
  }

  def resolveAccessor(ctx: TypeCheckContext, accessor: EirScopedSymbol[_], _base: Option[EirType]): View[(EirMember, EirType)] = {
    val base = _base.getOrElse(CheckTypes.visit(ctx, accessor.target))
    def helper(x: EirMember) = {
      (accessor.isStatic, x.isStatic) match {
        case (true, true) | (false, false) => (x, CheckTypes.visit(ctx, x))
        case (true, false) if x.member.isInstanceOf[EirFunction] =>
          (x, addExplicitSelf(ctx, base, CheckTypes.visit(ctx, x)))
        case _ => ???
      }
    }
    val cls = asClassLike(base)
    val imm = accessibleMember(cls, accessor)(ctx)
    imm.map(helper) ++ {
      sweepInherited(ctx, cls, other => {
        // TODO filter if overridden?
        resolveAccessor(ctx, accessor, Some(other))
      })
    }
  }

  def accessibleConstructor(base: EirType, x: EirNew, mustBeConcrete: Boolean = false)(implicit tyCtx: TypeCheckContext): View[EirMember] = {
    val c = asClassLike(base)
    if (c.isAbstract && mustBeConcrete) throw new RuntimeException(s"cannot instantiate ${c.name}")
    child[EirMember](c, (y: EirMember) => y.isConstructor && x.canAccess(y))
  }

  def overloads(function: EirFunction): View[EirFunction] = {
    val member = function.parent.to[EirMember]
    val base = member.map(_.base).orElse(function.parent)
    base.map(_.children.view
      .map(extractFunction)
      .flatMap(_.find(child => (child.name == function.name) && child != function)))
      .getOrElse(View())
  }

  private def matchesPredicate[T](predicate: T => Boolean)(x: Any): Boolean = {
    try {
      predicate(x.asInstanceOf[T])
    } catch {
      case _: ClassCastException => false
    }
  }

  private def _traits(x: EirClassLike)(implicit tyCtx: TypeCheckContext): Iterable[EirType] = {
    x.inherited.flatMap(x => {
      val res = uniqueResolution[EirType](tyCtx, x)
      val base = res match {
        case t: EirClassLike => t
        case t: EirTemplatedType =>
          val res = uniqueResolution[EirClassLike](tyCtx, t.base)
          if (res.inherited.nonEmpty) ???
          res
      }
      _traits(base) ++ Option.when(base.isInstanceOf[EirTrait])(res)
    })
  }

  private def accessibleMember(base: EirType, x: EirScopedSymbol[_])(implicit tyCtx: TypeCheckContext): View[EirMember] =
    x.pending match {
      case EirSymbol(_, head :: rest) => accessibleMember(base, x, head).flatMap(qualified(x, _, rest)).collect {
        case m: EirMember => m
      }
      // TODO add support for this
      case s: EirSpecializedSymbol[_] => Errors.unableToResolve(s)
      case _ => Errors.unreachable()
    }

  def accessibleMember(base: EirType, ctx: EirNode, field: String)(implicit tyCtx: TypeCheckContext): View[EirMember] = {
    // TODO check parent classes as well!
    child[EirMember](asClassLike(base), withName(field).and(ctx.canAccess(_)))
  }

  object FindSyntax {

    implicit class RichPredicate[T](predicate: T => Boolean) {
      def and(other: T => Boolean): T => Boolean = (t: T) => predicate(t) && other(t)
    }

  }

}
