package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.passes.{CheckTypes, TypeCheckContext}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{
  RichEirNode,
  RichIntOption,
  RichOption
}
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirType
import edu.illinois.cs.ergoline.util.{Errors, extractFunction, sweepInherited}

import scala.annotation.tailrec
import scala.collection.View
import scala.reflect.ClassTag

object Find {
  type EirNamedScope = EirScope with EirNamedNode

  def ancestors(x: EirNode): Seq[EirNode] = x.parent match {
    case Some(parent) => parent +: ancestors(parent)
    case None         => Nil
  }

  // find where x's ancestors first overlap with y's
  def commonAncestor(x: EirNode, y: EirNode): Option[EirNode] = {
    val ancestors = Find.ancestors(x).toList
    Find.ancestors(y).find(ancestors.contains(_))
  }

  def withName(name: String): EirNode => Boolean = {
    case x: EirNamedNode => x.name == name
    case _               => false
  }

  def descendant(
      node: EirNode,
      predicate: EirNode => Option[Boolean],
      stack: List[EirNode] = Nil
  ): Iterable[EirNode] = {
    Option
      .unless(stack.contains(node))({
        node.children.view
          .zip(node.children.view.map(predicate))
          .collect({ case (child, Some(x)) =>
            Option
              .when(x)(child) ++ descendant(child, predicate, stack :+ node)
          })
          .flatten
      })
      .getOrElse(Nil)
  }

  // recursively check all children of a node
  def within[T <: EirNode: ClassTag](
      node: EirNode,
      predicate: T => Boolean
  ): View[T] = {
    Find
      .child[T](node, predicate)
      .concat(node.children.view.flatMap(within[T](_, predicate)))
  }

  // check only the immediate children of the node (do not descend)
  def child[T <: EirNode: ClassTag](
      node: EirNode,
      predicate: T => Boolean
  ): View[T] = {
    node.children.view.collect {
      case t: T if predicate(t) => t
    }
  }

  def namedChild[T <: EirNamedNode](node: Option[EirScope], name: String)(
      implicit tag: ClassTag[T]
  ): T = {
    node
      .flatMap(firstNamedChild[T](_, name))
      .getOrElse(
        Errors.unableToResolve(List(name), node.getOrElse(???))
      )
  }

  private def firstNamedChild[T <: EirNamedNode: ClassTag](
      node: EirNode,
      name: String
  ): Option[T] = {
    node.children.view collectFirst {
      case n: EirNamedNode if n.name == name => n
    } map {
      case r: EirResolvable[_] if !r.resolved => Find.uniqueResolution[T](r)
      case t: T                               => t
    }
  }

  def resolutions[T <: EirNode: ClassTag](x: EirResolvable[_]): Iterable[T] = {
    x.resolve() collect { case t: T =>
      t
//      case s: EirResolvable[_] if x != s => uniqueResolution[T](s)
    }
  }

  def uniqueResolution[T <: EirNode: ClassTag](x: EirResolvable[_]): T = {
    resolutions(x).headOption getOrElse {
      Errors.unableToResolve(x)
    }
    /* assert(!globals.strict || found.length == found.distinct.length)
    found match {
      case (f: EirFunctionArgument) :: _ if f.isSelfAssigning => f.asInstanceOf[T]
      case head :: _ if !globals.strict => head
      // check to ensure unique resolution
      case head :: rest if globals.strict =>
        val all: List[T] = head +: rest
        if (all.length != all.distinct.length) Errors.warn(s"repeated resolutions of $x")
        if (all.length > 1) Errors.warn(s"potential ambiguity, selected $head from $all")
        head
      case _ => Errors.unableToResolve(x)
    } */
  }

  // TODO use this!!
  def uniqueResolution[T <: EirNode: ClassTag](
      it: Iterable[EirResolvable[_]]
  ): Iterable[T] = {
    it.map(uniqueResolution[T])
  }

  def unionResolvable(x: EirType, rY: EirResolvable[EirType])(implicit
      ctx: TypeCheckContext
  ): Option[EirType] = {
    val oY =
      Option.when(!rY.isInstanceOf[EirPlaceholder[_]])(CheckTypes.visit(rY))
    oY.map(unionType(x, _)) match {
      case Some(x) => x
      case None    => Some(x)
    }
  }

  def unionType(types: EirType*)(implicit
      ctx: TypeCheckContext
  ): Option[EirType] = unionType(types)

  def unionType(
      types: Iterable[EirType]
  )(implicit ctx: TypeCheckContext): Option[EirType] = {
    val distinct = types.toList.distinct
    distinct.tail.foldRight(distinct.headOption)((x, oY) =>
      oY match {
        case Some(y) => unionType(x, y)
        case None    => None
      }
    )
  }

  def unionType(x: EirType, y: EirType)(implicit
      ctx: TypeCheckContext
  ): Option[EirType] = {
    if (x == y) Some(x)
    else if (x.canAssignTo(y)) Some(y)
    else Some(x).filter(y.canAssignTo)
    // TODO seek common ancestor classes and cast
  }

  def parentOf[T <: EirNode: ClassTag](node: EirNode): Option[T] =
    node.parent.to[T].orElse(node.parent.flatMap(parentOf[T]))

  def traits(x: EirClassLike): Set[EirType] = _traits(x).toSet

  def isTopLevel(x: EirNode): Boolean = x match {
    // TODO can this simplify to?
    //      case _ : EirScope => true
    case _: EirBlock             => true
    case _: EirForLoop           => true
    case _: EirLambdaExpression  => true
    case _: EirFunction          => true
    case _: EirClassLike         => true
    case _: EirNamespace         => true
    case _: EirImport            => true
    case _: EirMatchCase         => true
    case _: EirExpressionPattern => true
    case _: EirSdagWhen          => true
    case _                       => false
  }

  import FindSyntax.RichPredicate

  def topLevel(
      ctx: EirNode,
      pred: Option[EirAnnotation => Boolean]
  ): Iterable[EirNode] = {
    val select =
      (node: EirNode) => isTopLevel(node) || node.isInstanceOf[EirMember]
    descendant(
      ctx,
      node => {
        Option.when(select(node))(pred.forall(node.annotations.exists(_)))
      }
    )
  }

  // NOTE when trying to resolve members this seems to return
  //      both the EirMember itself and its .member
  //      e.g. EirMember("foo"...), EirFunction("foo"...)
  //      need to figure out a way around this?
  def anywhereAccessible(ctx: EirNode, name: String): View[EirNamedNode] = {
    val ancestors = Find.ancestors(ctx).filter(isTopLevel)
    val include: EirNode => Boolean = {
      case x: EirMember => !ancestors.contains(x.base)
      case _            => true
    }
    val matches = withName(name).and(_.parent.forall(include))
    val predicate: EirNode => Option[Boolean] = {
      case _: EirBlock   => None
      case x: EirForLoop => Option.when(ancestors.contains(x))(false)
      // only allowed to consider members when within the class?
      case x: EirMember => Option.when(ancestors.contains(x.base))(matches(x))
      case x: EirNamespace =>
        Option.when(matches(x) || ancestors.contains(x))(matches(x))
      case x: EirClassLike =>
        Option.when(matches(x) || !ancestors.contains(x))(matches(x))
      case x: EirImport => Option.when(
          x.isPublic || Find
            .commonAncestor(ctx, x)
            .exists(ancestors.contains(_))
        )(
          matches(x)
        )
      case _: EirMultiDeclaration => Some(false)
      case x if !isTopLevel(x) || x.isInstanceOf[EirFunction] =>
        Option.when(matches(x))(true)
      // TODO how to handle arguments?
//      case x: EirFunction => Option.when(matches(x))(true)
      case x => Some(matches(x))
    }
    ancestors.view
      .flatMap(ancestor => {
        Find
          .descendant(ancestor, predicate)
          .filter(x => {
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
      })
      .filter(ctx.canAccess(_))
      .collect {
        case fs: EirFileSymbol => Find.uniqueResolution[EirNamedNode](fs)
        case x: EirNamedNode   => x
      }
  }

  def fromSymbol(symbol: EirSymbol[_]): Iterable[EirNode] = {
    symbol.qualifiedName match {
      case last :: Nil => anywhereAccessible(symbol, last)
      case "::" :: head :: tail => Modules(head, EirGlobalNamespace)
          .to[EirNamedNode]
          .view
          .flatMap(qualified(symbol, _, tail))
      case head :: tail =>
        anywhereAccessible(symbol, head).flatMap(qualified(symbol, _, tail))
      case Nil => Nil
    }
  }

  private def qualified(
      usage: EirNode,
      scope: EirNamedNode,
      names: List[String]
  ): Seq[EirNamedNode] = {
    if (names.nonEmpty) {
      val last = names.init.foldRight(Iterable(scope))((name, curr) => {
        curr.headOption.view.flatMap(
          child[EirNamedNode](_, withName(name).and(usage.canAccess))
        )
      })
      last
        .flatMap(
          child[EirNamedNode](_, withName(names.last).and(usage.canAccess))
            .map({
              case fs: EirFileSymbol => Find.uniqueResolution[EirNamedNode](fs)
              case x                 => x
            })
        )
        .toSeq
    } else {
      Seq(scope)
    }
  }

  def asClassLike(resolvable: EirResolvable[EirType]): EirClassLike =
    asClassLike(Find.uniqueResolution[EirType](resolvable))

  def asClassLike(ty: EirType): EirClassLike = tryClassLike(ty) match {
    case Some(c) => c
    case None    => Errors.incorrectType(ty, classOf[EirClassLike])
  }

  @tailrec
  def tryClassLike(ty: EirNode): Option[EirClassLike] = ty match {
    case x: EirClassLike     => Some(x)
    case x: EirTemplatedType => Some(asClassLike(x.base))
    case x: EirReferenceType => tryClassLike(x.base)
    case _                   => None
  }

  def resolveAccessor(
      accessor: EirScopedSymbol[_]
  )(_base: Option[EirType], static: Option[Boolean])(implicit
      ctx: TypeCheckContext
  ): View[(EirMember, Option[EirSpecialization])] = {
    val base = _base.getOrElse(CheckTypes.visit(accessor.target))
    checkSpecialized(base)

    def helper(x: EirMember): Option[(EirMember, Option[EirSpecialization])] = {
      // permits pure (non-)static and static access of non-static members (e.g., int::+)
      val isValid = (static, x.isStatic) match {
        case (Some(a), b) => a == b || (a && !b)
        case _            => true
      }

      Option.when(isValid)((x, Option(base).to[EirSpecialization]))
    }

    val cls = asClassLike(base)
    val imm = accessibleMember(cls, accessor)
    imm.flatMap(helper) ++ {
      sweepInherited[EirType, (EirMember, Option[EirSpecialization])](
        ctx,
        cls,
        (ictx, other) => {
          // TODO filter if overridden?
          resolveAccessor(accessor)(Some(other), static)(ictx)
        }
      )
    }
  }

  def accessibleConstructor(
      base: EirType,
      x: EirNew,
      mustBeConcrete: Boolean = false
  ): View[EirMember] = {
    val c = asClassLike(base)
    if (c.isAbstract && mustBeConcrete)
      throw new RuntimeException(s"cannot instantiate ${c.name}")
    child[EirMember](c, (y: EirMember) => y.isConstructor && x.canAccess(y))
  }

  def overloads(function: EirFunction): View[EirFunction] = {
    val member = function.parent.to[EirMember]
    val base = member.map(_.base).orElse(function.parent)
    base
      .map(
        _.children.view
          .map(extractFunction)
          .flatMap(
            _.find(child => (child.name == function.name) && child != function)
          )
      )
      .getOrElse(View())
  }

  private def _traits(x: EirClassLike): Iterable[EirType] = {
    x.inherited.flatMap(x => {
      val res = uniqueResolution[EirType](x)
      val base = res match {
        case t: EirClassLike => t
        case t: EirTemplatedType =>
          val res = uniqueResolution[EirClassLike](t.base)
          if (res.inherited.nonEmpty) ???
          res
      }
      _traits(base) ++ Option.when(base.isInstanceOf[EirTrait])(res)
    })
  }

  private def accessibleMember(
      base: EirType,
      x: EirScopedSymbol[_]
  ): View[EirMember] = x.pending match {
    case EirSymbol(_, head :: rest) =>
      accessibleMember(base, x, head).flatMap(qualified(x, _, rest)).collect {
        case m: EirMember => m
      }
    // TODO add support for this
    case s: EirSpecializedSymbol[_] => Errors.unableToResolve(s)
    case _                          => Errors.unreachable()
  }

  def accessibleMember(
      base: EirType,
      ctx: EirNode,
      field: String
  ): View[EirMember] = {
    // TODO check parent classes as well!
    child[EirMember](asClassLike(base), withName(field).and(ctx.canAccess(_)))
  }

  private def checkSpecialized(x: EirType): Unit = {
    assert(x match {
      case s: EirSpecializable => s.templateArgs.isEmpty
      case _                   => true
    })
  }

  // TODO this needs to be implemented using sweepInherited!
  def implementationOf(x: EirType, y: EirTrait)(
      ctx: TypeCheckContext
  ): Option[EirType] = {
    val c = Find.tryClassLike(x)

    checkSpecialized(x)

    { c.flatMap(z => Option.when(y.eq(z))(x)) } orElse {
      Option
        .when(c.exists(_.inherited.nonEmpty))({
          val spec = c.zip(Some(x).to[EirSpecialization]).flatMap {
            case (s, sp) => ctx.trySpecialize(s, sp)
          }

          val res = sweepInherited[EirType, EirType](
            ctx,
            x,
            (ictx, x) => implementationOf(x, y)(ictx).view
          ).headOption

          spec.foreach(ctx.leave)

          res
        })
        .flatten
    }
  }

  object FindSyntax {

    implicit class RichPredicate[T](predicate: T => Boolean) {
      def and(other: T => Boolean): T => Boolean =
        (t: T) => predicate(t) && other(t)
    }

  }

}
