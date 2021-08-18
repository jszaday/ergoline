package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.EirAccessibility.{
  EirAccessibility,
  Protected
}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.literals.{
  EirIntegerLiteral,
  EirLiteral,
  EirLiteralSymbol,
  EirLiteralType
}
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.{asMember, isFuture}
import edu.illinois.cs.ergoline.passes.Pass.Phase
import edu.illinois.cs.ergoline.passes.TypeCheckContext.ExpressionScope
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.Find.tryClassLike
import edu.illinois.cs.ergoline.resolution.{EirPlaceholder, EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{
  RichOption,
  RichResolvableTypeIterable
}
import edu.illinois.cs.ergoline.util.Errors.EirSubstitutionException
import edu.illinois.cs.ergoline.util.TypeCompatibility.{
  RichEirClassLike,
  RichEirType
}
import edu.illinois.cs.ergoline.util.{
  Errors,
  addExplicitSelf,
  assertValid,
  isSystem,
  validAccessibility
}

import scala.annotation.tailrec
import scala.reflect.ClassTag

class CheckTypes extends Pass {
  override def apply(n: EirNode): Unit = {
    CheckTypes.visit(n)(Processes.typeContext())
  }
  override def after: Seq[Pass] = Seq(Registry.instance[FullyResolve])
  override def phase: Phase = Phase.Load
}

object CheckTypes extends EirVisitor[TypeCheckContext, EirType] {
  // TODO this should consider the current substitutions, and check each unique substitution!
  var classCache: List[EirClass] = Nil

  final case class MissingSelfException[A <: EirNamedNode](symbol: EirSymbol[A])
      extends Exception

  private def generateLval(x: EirArrayReference): EirExpressionNode = {
    makeMemberCall(x.target, "get", x.args)
  }

  private def generateRval(x: EirAssignment): EirExpressionNode = {
    val operator = Option.unless(x.op == "=")(x.op.init)
    val arrayRef = assertValid[EirArrayReference](x.lval)
    val rval = operator match {
      case Some(op) => makeMemberCall(generateLval(arrayRef), op, List(x.rval))
      case None     => x.rval
    }
    makeMemberCall(arrayRef.target, "set", arrayRef.args :+ rval)
  }

  override def visitArrayReference(
      x: EirArrayReference
  )(implicit ctx: TypeCheckContext): EirType = {
    val assignment = x.parent.to[EirAssignment]
    val targetType = visit(x.target)
    if (assignment.exists(_.lval == x)) {
      val f = generateRval(assignment.get)
      x.disambiguation = Some(f)
      visit(f)
      // TODO this is redundant, can we maybe return nothing?
      //      (it gets visited in visitAssignment again)
      visit(assignment.get.rval)
    } else targetType match {
      case tupleType: EirTupleType =>
        val lit = Option
          .when(x.args.length == 1)(x.args.head)
          .map(StaticEvaluator.evaluate(_))
        val idx = lit.map(_.toInt)
        if (idx.exists(_ < tupleType.children.length)) {
          visit(tupleType.children(idx.get))
        } else {
          Errors.invalidTupleIndices(tupleType, x.args)
        }
      case _ =>
        val f = generateLval(x)
        x.disambiguation = Some(f)
        visit(f)
    }
  }

  def handleSpecialization(x: EirType)(implicit
      ctx: TypeCheckContext
  ): Either[EirSpecializable, EirSpecialization] = {
    val base = x match {
      case x: EirTemplatedType => visit(x)
      case x                   => x
    }

    base match {
      case sp @ EirTemplatedType(_, s: EirSpecializable, _) =>
        ctx.trySpecialize(s, sp).toRight(s)
      case _: EirTemplatedType => Errors.missingSpecialization(x)
      case x: EirSpecializable if x.templateArgs.nonEmpty =>
        ctx.trySpecialize(x).toRight(x)
      case _ => Right(null)
    }
  }

  @tailrec
  def isStatic(x: EirExpressionNode): Boolean = {
    x match {
      case x: EirCallArgument    => isStatic(x.expr)
      case x: EirFunctionCall    => isStatic(x.target)
      case x: EirScopedSymbol[_] => x.isStatic
      case x: EirSymbolLike[_] =>
        x.disambiguation.flatMap(Find.tryClassLike) match {
          case Some(c: EirClass)     => !c.objectType
          case Some(_: EirClassLike) => true
          case _                     => false
        }
      case _ => false // TODO figure out what goes here?
    }
  }

  private def checkStaticness(
      lhs: EirExpressionNode,
      found: EirNamedNode
  ): Unit = {
    val staticBase = isStatic(lhs)
    found match {
      case m: EirMember if staticBase && !m.isStatic =>
        Errors.invalidAccess(lhs, m)
      case m: EirMember if !staticBase && m.isStatic =>
        Errors.invalidAccess(lhs, m)
      case _ =>
    }
  }

  // TODO add checks for static-ness?
  override def visitScopedSymbol[A <: EirNode](
      x: EirScopedSymbol[A]
  )(implicit ctx: TypeCheckContext): EirType = {
    val base = visit(x.target)
    val spec = handleSpecialization(base)
    val prevFc: Option[EirFunctionCall] =
      ctx.immediateAncestor[EirFunctionCall].filter(_.target.contains(x))
    val candidates = Find.resolveAccessor(x)(Some(base), Some(x.isStatic))
    val found = screenCandidates(ExpressionScope(prevFc, Some(x)), candidates)
    found match {
      case (member, result) :: _ =>
        spec.foreach(ctx.leave)
        x.disambiguation = Some(member)
        checkStaticness(x.target, member)
        prevFc.foreach(validate(ctx, member, _))
        result
      case _ => Errors.unableToResolve(x.pending)
    }
  }

  override def visitTernaryOperator(
      x: EirTernaryOperator
  )(implicit ctx: TypeCheckContext): EirType = {
    checkCondition(x.test)
    val tty = visit(x.ifTrue)
    val fty = visit(x.ifFalse)
    Find.unionType(tty, fty) match {
      case Some(found) => found
      case None        => Errors.unableToUnify(x, tty, fty)
    }
  }

  override def visitLambdaType(
      x: types.EirLambdaType
  )(implicit ctx: TypeCheckContext): EirType = {
    ctx.lambdaWith(
      x.from.map(visit(_)),
      visit(x.to),
      x.templateArgs,
      x.predicate
    )
  }

  override def visitTemplatedType(
      x: types.EirTemplatedType
  )(implicit ctx: TypeCheckContext): EirType = {
    val base: EirSpecializable = Find.uniqueResolution[EirSpecializable](x.base)
    val spec = ctx.specialize(base, x)
    visit(base)
    ctx.leave(spec)
    // visit our base
    base match {
      case c: EirClassLike => ctx.getTemplatedType(c, x.args.map(visit(_)))
      case _               => error(x, s"unsure how to specialize ${x.base}")
    }
  }

  override def visitProxyType(
      x: types.EirProxyType
  )(implicit ctx: TypeCheckContext): EirType = {
    val ty = ProxyManager.proxyFor(x)
    x.setType(ty)
    visit(ty)
  }

  override def visitImport(eirImport: EirImport)(implicit
      ctx: TypeCheckContext
  ): EirType = null

  @tailrec
  def accessibleMember(
      pair: (EirClassLike, EirClassLike),
      accessibility: EirAccessibility
  ): Boolean = {
    pair match {
      case (a: EirProxy, b: EirProxy) =>
        accessibleMember((a.base, b.base), accessibility)
      case (a, b: EirProxy) => accessibleMember((a, b.base), accessibility)
      case (a: EirProxy, b) => accessibleMember((a.base, b), accessibility)
      case (a, b) =>
        EirAccessibility.compatible(validAccessibility(a, b), accessibility)
    }
  }

  def sharedBase(a: EirClassLike, b: EirClassLike): Boolean =
    accessibleMember((a, b), Protected)

  def sharedBase(a: EirMember, b: EirMember): Boolean =
    sharedBase(a.base, b.base)

  @tailrec
  def targetsSelf(a: EirMember, node: EirExpressionNode): Boolean = node match {
    case s: EirSymbol[_] =>
      isSelf(s) || asMember(s.disambiguation).exists(sharedBase(a, _))
    case f: EirScopedSymbol[_]      => targetsSelf(a, f.target)
    case p: EirPostfixExpression[_] => targetsSelf(a, p.target)
    case _                          => false
  }

  def validate(
      ctx: TypeCheckContext,
      target: EirNamedNode,
      call: EirFunctionCall
  ): Unit = {
    target match {
      case member: EirMember =>
        val current = ctx.ancestor[EirMember]
        val bases = current.map(x => (x.base, member.base))
        if (
          (bases.isEmpty && !member.isPublic) ||
          bases.exists(!accessibleMember(_, member.accessibility))
        ) {
          Errors.inaccessibleMember(member, call)
        }
        val fut = isFuture(member.base) && member.name == "get"
        // NOTE this should probably check to see if self@ only?
        val self = member.isEntryOnly && current.exists(targetsSelf(_, call))
        if (fut || self) {
          current.foreach(_.makeEntryOnly())
        }
      case _ =>
    }
  }

  def isLvalue(node: EirExpressionNode): Boolean = {
    node match {
      case _: EirSymbolLike[_] => true
      case _                   => false
    }
  }

  def checkReference(node: EirExpressionNode, ty: EirType): Unit = {
    if (!isLvalue(node)) {
      Errors.expectedLvalue(node)
    } else if (!Find.tryClassLike(ty).forall(_.isValueType)) {
      Errors.expectedValueType(node, ty)
    }
  }

  override def visitFunctionCall(
      call: EirFunctionCall
  )(implicit ctx: TypeCheckContext): EirType = {
    val target = visit(call.target)
    val ours = call.args

    // screen for correct usage of reference-passing
    ours filter { _.isRef } map { _.expr } foreach { x =>
      checkReference(x, visit(x))
    }

    // everything else should be resolved already, or resolved "above" the specialization
    target match {
      // TODO do we need to track predicates here?
      case EirLambdaType(_, args, retTy, _, _)
          if argumentsMatch(ours, args.map(visit(_))) =>
        retTy match {
          case t: EirType => t
          case _          => visit(retTy)
        }
      case _ => error(
          call,
          s"cannot resolve ((${ours mkString ", "}) => ???) and $target"
        )
    }
  }

  def argumentsMatch(
      x: Iterable[EirType],
      y: Iterable[EirType],
      exact: Boolean
  )(implicit
      ctx: TypeCheckContext
  ): Boolean = argumentsMatch(x.toList, y.toList, exact)

  def tryCast(expr: EirExpressionNode, to: EirType)(implicit
      ctx: TypeCheckContext
  ): Boolean = {
    val from = visit(expr)
    if (from.canAssignTo(to)) {
      (from, to) match {
        case (_: EirLambdaType, _: EirLambdaType) => true
        case (_, _: EirLambdaType)                =>
          // detailed applicator information is only available at this point
          // (i.e., verifying compatible static-ness)
          CheckTypes
            .findApply[EirMember](
              ExpressionScope(None, Some(expr)),
              from,
              Some(from)
            ) exists { case (m, ty) =>
            val valid = ty.canAssignTo(to)
            if (valid) { expr.disambiguation = ctx.makeLambda(expr, m, ty) }
            valid
          }
        case _ => true
      }
    } else {
      false
    }
  }

  def argumentsMatch(
      x: List[EirExpressionNode],
      y: List[EirType]
  )(implicit
      ctx: TypeCheckContext
  ): Boolean = {
    (x.length == y.length) && {
      x.zip(y) forall { case (a, b) => tryCast(a, b) }
    }
  }

  def argumentsMatch(
      x: List[EirType],
      y: List[EirType],
      exact: Boolean = false
  )(implicit
      ctx: TypeCheckContext
  ): Boolean = {
    (x.length == y.length) &&
    x.zip(y)
      .forall({
        case (x, y) if exact => x == y
        case (x, y)          => x.canAssignTo(y)
      })
  }

  def resolveIterator(
      h: EirForAllHeader
  )(implicit ctx: TypeCheckContext): EirType = {
    visit(h.expression) match {
      case t: EirTemplatedType =>
        val iterableTy = globals.iterableType
        val iteratorTy = globals.iteratorType
        val base = visit(t.base)
        if (base.canAssignTo(iterableTy)) {
          h.expression = makeMemberCall(h.expression, "iter")
          visit(h.expression)
        } else if (!base.canAssignTo(iteratorTy)) {
          Errors.unableToUnify(h.expression, List(base, iterableTy, iteratorTy))
        }
        visit(t.args.head)
      case t => Errors.incorrectType(t, EirTemplatedType.getClass)
    }
  }

  override def visitForLoop(
      loop: EirForLoop
  )(implicit ctx: TypeCheckContext): EirType = {

    loop.header match {
      case EirCStyleHeader(decl, test, incr) =>
        visit(decl)
        checkCondition(test)
        visit(incr)
      case h: EirForAllHeader =>
        val iterTy = resolveIterator(h)
        if (h.identifiers.length == 1) {
          h.declarations.head.declaredType = iterTy
        } else ???
      case _ => Errors.unreachable()
    }
    visit(loop.body)
  }

  override def visitLiteral(
      value: EirLiteral[_]
  )(implicit ctx: TypeCheckContext): EirType = {
    value match {
      case EirLiteralType(ty)   => visit(ty)
      case EirLiteralSymbol(ty) => visit(ty)
      case _                    => globals.typeFor(value)
    }
  }

  def isSelf(value: EirSymbol[_]): Boolean = value.qualifiedName match {
    case ("self" | "self@" | "self[@]") :: Nil => true
    case _                                     => false
  }

  /** Merges two maps of type arguments to types, using unification.
    *  Propagates unification errors as null to indicate incompatibility
    */
  private def merge(
      a: Map[EirTemplateArgument, EirType],
      b: Map[EirTemplateArgument, EirType]
  )(implicit
      ctx: TypeCheckContext
  ): Map[EirTemplateArgument, EirType] = {
    (a.toList ++ b.toList).groupMap(_._1)(_._2).map { case (arg, tys) =>
      arg -> {
        tys.reduce((a, b) =>
          Option
            .when(a != null && b != null)(Find.unionType(a, b).orNull)
            .orNull
        )
      }
    }
  }

  /** Finds the value of one or more unknown type args using known types
    *  (Note, there isn't any machine learning going on here...)
    *
    * @param ctx type context under which to operate
    * @param pair pair of unknown and known types
    * @return a map of learned type args and their values
    */
  private def learn(
      pair: (EirResolvable[EirType], EirType)
  )(implicit ctx: TypeCheckContext): Map[EirTemplateArgument, EirType] = {
    pair match {
      case (EirPlaceholder(_, Some(a)), b) => learn((a, b))
      case (a: EirTemplatedType, b: EirTemplatedType)
          if a.args.length == b.args.length =>
        (learn((a.base, visit(b.base))) +: a.args
          .zip(b.args.map(visit(_)))
          .map(learn)).reduce(merge)
      case (a: EirLambdaType, b: EirLambdaType)
          if a.from.length == b.from.length =>
        (learn((a.to, visit(b.to))) +: a.from
          .zip(b.from.map(visit(_)))
          .map(learn)).reduce(merge)
      case (t: EirTemplateArgument, b) => Map(t -> b)
      case (_: EirSymbol[_] | _: EirSpecializedSymbol, b) =>
        learn((Find.uniqueResolution[EirResolvable[EirType]](pair._1), b))
      case (_: EirProxyType, _: EirProxyType) => ???
      case _                                  => Map()
    }
  }

  def inferSpecialization(s: EirSpecializable, args: List[EirType])(implicit
      ctx: TypeCheckContext
  ): Option[EirSpecialization] = {
    val unknowns = s match {
      case s: EirLambdaType => Option(s.from)
      case _                => Option.when(s.templateArgs.nonEmpty)(s.templateArgs)
    } // TODO this needs further testing to ensure it doesn't mess with things!

    val insights = unknowns
      .filter(_.length == args.length)
      .map(_.zip(args).map(learn(_)))
      .map(_.reduce(merge))
      .getOrElse(Map())

    val paired = s.templateArgs.map(t => {
      insights.get(t).orElse(t.defaultValue.map(visit(_)))
    })

    Option.when(paired.forall(x => x.isDefined && !x.contains(null)))(
      ctx.synthesize(paired.flatten)
    )
  }

  @tailrec
  def getImplicitArgs(of: EirNode): List[EirFunctionArgument] = {
    of match {
      case EirMember(_, f: EirFunction, _) => getImplicitArgs(f)
      case f: EirFunction                  => f.implicitArgs
      case _                               => Nil
    }
  }

  def screenImplicitArgs(
      of: EirNode
  )(implicit ctx: TypeCheckContext): List[EirFunctionArgument] = {
    val scoped =
      ctx.ancestor[EirMember].toList.flatMap(_.selfDeclarations).collect {
        case EirMember(_, d: EirImplicitDeclaration, _) if d.isImplicit => d
      }

    getImplicitArgs(of).filterNot(x => {
      val symbol =
        EirSymbol[EirImplicitDeclaration](ctx.currentNode, List(x.name))
      val target = visit(x.declaredType)
      val candidates = scoped.view.filter(_.name == x.name) ++ Find
        .resolutions[EirImplicitDeclaration](symbol)
        .filter(_.isImplicit)

      candidates.exists(d => {
        d.isImplicit && visit(d).canAssignTo(target)
      })
    })
  }

  private def resolve(
      x: EirResolvable[_],
      args: List[EirType]
  )(implicit ctx: TypeCheckContext): Option[EirType] = {
    val candidates = Find.resolutions[EirNamedNode](x)
    val types = candidates map {
      case s: EirSpecializable => s // classes, traits, fns, etc.
      case n: EirNode          => visit(n) // declarations, members, etc.
    }
    val synth = ctx.synthesize(args)

    val found = (candidates.zip(types) flatMap {
      case (n, s: EirType with EirSpecialization) => Some((n, s))
      // TODO this bypass is a bit flaky!
      case (n, s: EirType with EirSpecializable) if s.templateArgs.isEmpty =>
        Some((n, s))
      case (n, s: EirSpecializable) =>
        val ty = ctx.trySpecialize(s, synth)
        ty.foreach(ctx.leave)
        ty.map(_ =>
          (
            n,
            s match {
              case t: EirLambdaType => t
              case _                => ctx.getTemplatedType(s, args)
            }
          )
        )
    }).headOption

    x match {
      case x: EirExpressionNode => x.disambiguation = found.map(_._1)
    }

    found.map(_._2)
  }

  private def resolve(
      x: EirSpecializedSymbol
  )(implicit ctx: TypeCheckContext): Option[EirType] = {
    val found = resolve(x.symbol, x.types.map(visit(_)))
    x.disambiguation = found
    found
  }

  private def resolve(t: EirNode, args: Option[List[EirType]])(implicit
      ctx: TypeCheckContext
  ): Option[EirType] = {
    (t, args) match {
      case (t: EirSpecializedSymbol, _)  => resolve(t)
      case (t: EirType, None)            => Some(t)
      case (t: EirSymbol[_], Some(args)) => resolve(t, args)
      case _                             => ???
    }
  }

  def findApply[A <: EirNamedNode: ClassTag](
      scope: ExpressionScope,
      member: EirType,
      sp: Option[EirType]
  )(implicit
      ctx: TypeCheckContext
  ): Seq[(A, EirType)] = {
    val static = scope.acc.map(isStatic)
    val candidates = {
      val cls = Find.asClassLike(member)
      val accessor = mkAccessor(cls, "apply")(scope.acc)
      Find.resolveAccessor(accessor)(sp, static)
    }

    screenCandidates(
      scope,
      candidates.view.collect { case (a: A, opt) => (a, opt) }
    )
  }

  private def screenCandidate[A <: EirNamedNode: ClassTag](
      scope: ExpressionScope
  )(candidate: A, outer: Option[EirSpecialization])(implicit
      ctx: TypeCheckContext
  ): (Boolean, Option[(A, EirType)]) = {
    val ospec = outer.collect { case t: EirTemplatedType =>
      ctx.specialize(assertValid[EirSpecializable](t.base), t)
    }

    val member = visit(candidate)
    val args = getArguments(scope.args)
    val ispec = handleSpecialization(member) match {
      case Right(sp) => sp
      case Left(s) => args
          .filterNot(_.isEmpty) // TODO make this UNIT in the future?
          .flatMap(inferSpecialization(s, _))
          .flatMap(ctx.trySpecialize(s, _))
          .getOrElse({
            ospec.foreach(ctx.leave)
            return (false, None)
          })
    }

    val found = (member, args) match {
      case (
            EirTemplatedType(_, _: EirClassLike, _) | _: EirClassLike,
            Some(_)
          ) =>
        val sp = {
          scope.args
            .to[EirFunctionCall]
            .map(_.target)
            .flatMap(resolve(_, args.filterNot(_.isEmpty)))
        } orElse {
          Option(ispec).flatMap(resolve(_, None))
        } getOrElse member

        (false, findApply(scope, member, Some(sp)).headOption)
      case (_: EirLambdaType, Some(ours)) =>
        val missing = screenImplicitArgs(candidate)
        if (missing.nonEmpty) {
          (missing.exists(_.name == globals.implicitProxyName), None)
        } else {
          val s = visit(candidate)
          val t = assertValid[EirLambdaType](s)

          if (argumentsMatch(ours, t.from.map(assertValid[EirType]))) {
            // NOTE the double check is necessary here to visit candidate if it hasn't
            // already been visited... and since visitFunction doesn't resolve its types
            (false, Some((candidate, t)))
          } else {
            (false, None)
          }
        }
      case (x, None)    => (false, Some(candidate, visit(x)))
      case (_, Some(_)) => (false, None)
    }

    val prependSelf = (scope.acc, asMember(found._2.map(_._1))) match {
      case (Some(acc), Some(m: EirMember)) =>
        // TODO need something more reliable than ( foundType ) here
        acc.foundType.filter(_ => isStatic(acc) && !m.isStatic)
      case _ => None
    }

    val result = found._2 filter {
      case (_, p: EirSpecializable) =>
        ctx.checkPredicate(p.predicate.filter(_ => p.templateArgs.isEmpty))
      case _ => true // TODO check whether this works for sysParents
    } collect { case (a: A, b) =>
      (a, prependSelf.map(addExplicitSelf(ctx, b, _)).getOrElse(b))
    }

    ctx.leave(ispec)
    ospec.foreach(ctx.leave)

    (found._1, result)
  }

  def screenCandidates[A <: EirNamedNode: ClassTag](
      scope: ExpressionScope,
      candidates: Iterable[(A, Option[EirSpecialization])]
  )(implicit
      ctx: TypeCheckContext
  ): Seq[(A, EirType)] = {
    var missingSelf = false
    val results = (candidates flatMap { case (candidate, pair) =>
      val res = screenCandidate(scope)(candidate, pair)
      missingSelf = missingSelf || res._1
      res._2
    }).toSeq

    if (missingSelf && results.isEmpty) {
      throw MissingSelfException(
        EirSymbol[EirImplicitDeclaration](
          ctx.currentNode,
          List(globals.implicitProxyName)
        )
      )
    } else {
      // TODO implement some safety here, one must hide the others or it's ambiguous!
      results
    }
  }

  def getArguments(
      opt: Option[EirExpressionNode]
  )(implicit ctx: TypeCheckContext): Option[List[EirType]] = {
    val args = opt.collect {
      case f: EirFunctionCall => f.args
      case n: EirNew          => n.args
    }
    args.map(_.map(visit(_)))
  }

  def mkAccessor[A <: EirNamedNode: ClassTag](cls: EirClassLike, name: String)(
      parent: Option[EirNode]
  ): EirScopedSymbol[A] = {
    // TODO need something more reliable than ( null ) here
    EirScopedSymbol[A](null, EirSymbol[A](parent, List(name)))(parent)
  }

  override def visitSymbol[A <: EirNamedNode](
      value: EirSymbol[A]
  )(implicit ctx: TypeCheckContext): EirType = {
    val prevFc = value.parent.collect({
      case f: EirFunctionCall if f.target.contains(value) => f
    })
    val self = Option.when(isSelf(value))(value.qualifiedName.last)
    val candidates: (
        Option[EirScopedSymbol[_]],
        Iterable[(EirNamedNode, Option[EirSpecialization])]
    ) = self match {
      case Some(name) => (
          None,
          ctx.ancestor[EirMember] match {
            case Some(m) => m.selfDeclarations collect {
                case m: EirNamedNode if m.name == name => (m, None)
              }
            case _ => Nil
          }
        )
      case None =>
        val (init, last) = (value.qualifiedName.init, value.qualifiedName.last)
        if (init.nonEmpty) {
          val parent = Find.resolutions[EirNamedNode](
            EirSymbol[EirNamedNode](value.parent, init)
          )
          val asCls = parent.headOption.flatMap(tryClassLike)
          if (asCls.isDefined) {
            // NOTE this may not be reachable with a templated class
            val asType = asCls.map(_.asType).map(visit)
            val accessor = mkAccessor(asCls.get, last)(value.parent)
            accessor.foundType = asType
            // NOTE this is symptomatic of a hack that will persist
            //      until (true) static-ness detection is more mature
            accessor.isStatic = true
            (Some(accessor), Find.resolveAccessor(accessor)(asType, Some(true)))
          } else {
            (None, Find.resolutions[EirNamedNode](value).map((_, None)))
          }
        } else {
          (None, Find.resolutions[EirNamedNode](value).map((_, None)))
        }
    }
    val found =
      screenCandidates(ExpressionScope(prevFc, candidates._1), candidates._2)
    (found, self) match {
      case ((m, ty) :: _, _) =>
        value.disambiguation = Some(m)
        prevFc
          .zip(asMember(m))
          .foreach(x => validate(ctx, x._2, x._1))
        visit(ty)
      case (_, Some(_)) => throw MissingSelfException(value)
      case _            => Errors.unableToResolve(value)
    }
  }

  private def seekType(
      list: List[EirNode]
  )(implicit ctx: TypeCheckContext): Option[EirType] = {
    if (list.nonEmpty) list.init.foreach(visit(_))
    list.lastOption.map(visit(_)).filterNot(_ == null)
  }

  // TODO when the last statement in a block is an expression, put a "return" there
  override def visitBlock(
      node: EirBlock
  )(implicit ctx: TypeCheckContext): EirType = {
    val lastTy = seekType(node.children)

    val retTys: List[EirType] = Find
      .descendant(
        node,
        {
          case _: EirLambdaExpression => None
          case _: EirFunction         => None
          case _: EirReturn           => Some(true)
          case _                      => Some(false)
        }
      )
      .map(_.asInstanceOf[EirReturn])
      .map(r => visit(r.expression))
      .toList

    val implicitReturn = retTys.isEmpty && lastTy.nonEmpty
    node.implicitReturn = implicitReturn

    val types = if (implicitReturn) lastTy.toList else retTys
    if (types.isEmpty) {
      globals.unitType
    } else {
      Find.unionType(types) match {
        case Some(x) => x
        case None    => Errors.unableToUnify(node, types)
      }
    }
  }

  override def visitNamespace(
      node: EirNamespace
  )(implicit ctx: TypeCheckContext): EirType = {
    visit(node.children)
    null
  }

  // TODO fix to be context sensitive! Otherwise this gon' blow up!
  override def visitDeclaration(
      node: EirDeclaration
  )(implicit ctx: TypeCheckContext): EirType = {
    ctx.avail(node).foreach(return _)
    val lval = {
      (node.declaredType match {
        case p: EirPlaceholder[_] => p.expectation
        case t                    => Some(t)
      }).map(visit(_))
    }
    val rval = node.initialValue.map(visit(_))
    val ty = (lval, rval) match {
      case (None, None)                           => Errors.missingType(node)
      case (Some(a), None)                        => a
      case (None, Some(b))                        => node.declaredType = b; b
      case (Some(a), Some(b)) if b.canAssignTo(a) => a
      case (Some(a), Some(b))                     => Errors.cannotCast(node, b, a)
    }
    ctx.cache(node, ty)
    ty
  }

  override def visitTemplateArgument(
      node: EirTemplateArgument
  )(implicit ctx: TypeCheckContext): EirType = {
    if (ctx.staticTyping) {
      node.argumentType.orElse(ctx.hasSubstitution(node)) match {
        case Some(ty) => visit(ty)
        case None     => throw EirSubstitutionException(node)
      }
    } else {
      ctx.hasSubstitution(node) match {
        case Some(x) => visit(x)
        case _       => Errors.missingSpecialization(node)
      }
    }
  }

  // TODO need to check parent classes/traits too
  //      since they may not be reached otherwise
  def visitClassLike(
      node: EirClassLike
  )(implicit ctx: TypeCheckContext): EirType = {
    val (_, _, opt) = handleSpecializable(ctx, node)
    val sys = isSystem(node)

    opt.foreach(subCtx => {
      ctx.start(subCtx)
      if (!sys) CheckClasses.visit(ctx, node)
      node.members.foreach(visit(_))
      node.inherited.foreach(visit(_))
      if (!sys && node.annotation("main").isDefined) {
        visitProxyType(EirProxyType(None, node, None, None))
      }
      ctx.stop(subCtx)
    })
    node
  }

  override def visitClass(node: EirClass)(implicit
      ctx: TypeCheckContext
  ): EirType = visitClassLike(node)

  override def visitTrait(node: EirTrait)(implicit
      ctx: TypeCheckContext
  ): EirType = visitClassLike(node)

  override def visitMember(
      node: EirMember
  )(implicit ctx: TypeCheckContext): EirType = {
    val proxy = ctx.ancestor[EirClassLike].collect { case p: EirProxy => p }
    visit((proxy, node.counterpart) match {
      case (_, Some(m)) =>
        visit(m.member)
        visit(node.member)
      case (Some(p), _) => p.members
          .find(_.counterpart.contains(node))
          .getOrElse(node.member)
      case (_, _) => node.member
    })
  }

  def handleSpecializable(
      ctx: TypeCheckContext,
      s: EirSpecializable
  ): (Boolean, Option[EirMember], Option[TypeCheckContext.Context]) = {
    val member = ctx.immediateAncestor[EirMember]
    val noArgs = s.templateArgs.isEmpty
    val pred = ctx.checkPredicate(s.predicate.filter(_ => noArgs))
    val spec = if (noArgs) None else ctx.findSubstitution(s)
    val isDefined = pred && (noArgs || spec.isDefined)
    (
      isDefined,
      member,
      Option.when(isDefined)(ctx.mkCheckContext(s, spec)).flatten
    )
  }

  def isProxySelf(s: EirSymbol[_]): Boolean = {
    s.qualifiedName.lastOption.exists(x => {
      x.contains("@") || x == globals.implicitProxyName
    })
  }

  override def visitFunction(
      node: EirFunction
  )(implicit ctx: TypeCheckContext): EirLambdaType = {
    val (isDefined, member, opt) = handleSpecializable(ctx, node)
    val proxy = member.flatMap(_.parent).to[EirProxy]

    opt
      .filterNot(_ => proxy.isDefined && node.body.isEmpty)
      .foreach(subCtx => {
        // TODO use a true subctx here
        val current = ctx.currentSubstitution
        try {
          CheckFunctions.visit(ctx, node)
          ctx.start(subCtx)
          val bodyType = node.body.map(visit(_))
          val implicitReturn = node.body.exists(_.implicitReturn)
          val retTy = visit(node.returnType)
          if (!(implicitReturn && retTy == globals.unitType)) {
            if (!bodyType.forall(_.canAssignTo(retTy))) {
              Errors.unableToUnify(node, bodyType.get, retTy)
            }
          }
          ctx.stop(subCtx)
        } catch {
          case e: MissingSelfException[_] =>
            // TODO fix up this logic
            //      needs to consider self[@] being inaccessible in foo@, e.g.
            if (member.exists(!_.isStatic)) {
              assert(isProxySelf(e.symbol))
              member.foreach(_.makeEntryOnly())
              ctx.popUntil(node)
              ctx.rollbackTo(current)
              ctx.stop(subCtx)
              return null
            } else {
              Errors.unableToResolve(e.symbol)
            }
        }
      })

    if (isDefined) {
      ctx.lambdaWith(expand(node.functionArgs), visit(node.returnType))
    } else {
      // NOTE expansions are explicitly not known here...
      //      what should this actually return? a placeholder?
      ctx.lambdaWith(
        node.functionArgs.map(_.declaredType),
        node.returnType,
        node.templateArgs,
        node.predicate
      )
    }
  }

  def annotationsOf(node: EirFunction): List[EirAnnotation] = {
    if (node.parent.exists(_.isInstanceOf[EirMember])) {
      node.parent.get.annotations
    } else {
      node.annotations
    }
  }

  def error(x: EirNode, msg: String)(implicit ctx: TypeCheckContext): EirType =
    Errors.exit(Errors.format(x, msg))

  override def visitAnnotation(
      node: EirAnnotation
  )(implicit ctx: TypeCheckContext): EirType =
    error(node, "annotations are type-less")

  override def visitBinaryExpression(
      node: EirBinaryExpression
  )(implicit ctx: TypeCheckContext): EirType = {
    val op = node.op
    val boolTy = globals.boolType
    val (lhsTy, rhsTy) = (visit(node.lhs), visit(node.rhs))

    if (globals.isIdentityComparator(op)) {
      if (lhsTy == rhsTy) {
        boolTy
      } else {
        Errors.unableToUnify(node, lhsTy, rhsTy)
      }
    } else if (op == "&&" || op == "||") {
      val failure = Option
        .when(!lhsTy.canAssignTo(boolTy))(lhsTy)
        .orElse(Option.when(!rhsTy.canAssignTo(boolTy))(rhsTy))
      failure.foreach(Errors.unableToUnify(node, _, boolTy))
      boolTy
    } else {
      val spaceship = globals.spaceshipOperator
      val lhsClass = Find.asClassLike(lhsTy)

      if (lhsClass.hasMember(op)) {
        val f = makeMemberCall(node.lhs, op, List(node.rhs))
        node.disambiguation = Some(f)
        visit(f)
      } else if (
        globals.isComparisonOperator(op) && lhsClass.hasMember(spaceship)
      ) {
        val f = makeMemberCall(node.lhs, spaceship, List(node.rhs))
        val g = EirBinaryExpression(
          None,
          f,
          op,
          EirIntegerLiteral(0)(None)
        )
        node.disambiguation = Some(g)
        visit(f)
        boolTy
      } else if (globals.isEqualityComparator(op)) {
        val other = globals.hasEqualityComparator(lhsClass)
        other match {
          case Some(invOp) =>
            val f = makeMemberCall(node.lhs, invOp, List(node.rhs))
            val g = EirUnaryExpression(
              None,
              "!",
              f
            )
            node.disambiguation = Some(g)
            val retTy = visit(f)
            if (retTy.canAssignTo(boolTy)) {
              boolTy
            } else {
              Errors.unableToUnify(node, retTy, boolTy)
            }
          case _ => Errors.unknownOperator(node, op)
        }
      } else {
        Errors.unknownOperator(node, op)
      }
    }
  }

  override def visitFunctionArgument(
      node: EirFunctionArgument
  )(implicit ctx: TypeCheckContext): EirType = {
    visit(node.declaredType)
  }

  override def visit(n: EirNode)(implicit ctx: TypeCheckContext): EirType = {
    val node = n match {
      case x: EirCallArgument => x.expr
      case _                  => n
    }

    ctx.enterNode(node)
    val result = super.visit(node)
    node match {
      case x: EirExpressionNode => x.foundType = Option(result) match {
          case None => Errors.missingType(x)
          case o    => o
        }
      case _ =>
    }
    if (ctx.alreadyLeft(node)) result
    else ctx.leaveWith(result)
  }

  private def isValidMemberAssign(node: EirAssignment, m: EirMember) = {
    (m.member match {
      case d: EirDeclaration => !d.isFinal
      case _                 => false
    }) || {
      Find
        .parentOf[EirMember](node)
        .exists(x => x.isConstructor && x.base == m.base)
    }
  }

  def isInvalidFinalAssign(node: EirAssignment): Boolean = {
    node.lval match {
      case s: EirSymbol[_] =>
        val resolution = ((x: EirNode) => asMember(x).getOrElse(x))(
          Find.uniqueResolution[EirNode](s)
        )

        resolution match {
          case m: EirMember           => !isValidMemberAssign(node, m)
          case d: EirDeclaration      => d.isFinal
          case _: EirFunctionArgument => true
          case _                      => false
        }
      case _: EirArrayReference => false
      case s: EirScopedSymbol[_] =>
        asMember(s.disambiguation).orElse(s.disambiguation) match {
          case Some(m: EirMember) => !isValidMemberAssign(node, m)
          case _                  => false
        }
      case _ => true
    }
  }

  override def visitAssignment(
      node: EirAssignment
  )(implicit ctx: TypeCheckContext): EirType = {
    val lval = visit(node.lval)
    val rval = visit(node.rval)
    if (!rval.canAssignTo(lval)) {
      Errors.cannotCast(node, lval, rval)
    } else if (isInvalidFinalAssign(node)) {
      Errors.assignToVal(node)
    } else {
      globals.unitType
    }
  }

  override def visitTupleExpression(
      node: EirTupleExpression
  )(implicit ctx: TypeCheckContext): EirType = {
    EirTupleType(Some(node), visit(node.children).toList)
  }

  private def expand(
      args: List[EirFunctionArgument]
  )(implicit ctx: TypeCheckContext): List[EirType] = {
    args.flatMap(arg => {
      visit(arg.declaredType) match {
        case t: EirTupleType if arg.isExpansion => t.children.map(visit(_))
        case t                                  => List(t)
      }
    })
  }

  override def visitLambdaExpression(
      node: EirLambdaExpression
  )(implicit ctx: TypeCheckContext): EirType = {
    ctx.registerLambda(node)
    ctx.lambdaWith(expand(node.args), visit(node.body))
  }

  override def visitReturn(
      node: EirReturn
  )(implicit ctx: TypeCheckContext): EirType = {
    visit(node.expression)

    null // TODO change to Nothing when it is extant
  }

  final case class TypeCheckException(message: String)
      extends Exception(message)

  override def visitSpecializedSymbol(
      x: EirSpecializedSymbol
  )(implicit ctx: TypeCheckContext): EirType = {
    val prevFc: Option[EirFunctionCall] =
      ctx.immediateAncestor[EirFunctionCall].filter(_.target.contains(x))
    // TODO this should probably return templated types
    val candidates = Find.resolutions[EirNamedNode](x.symbol)
    val found = screenCandidates(
      ExpressionScope(prevFc, None),
      candidates.view.map((_, None))
    )
    found match {
      case (m, ty) :: _ =>
        x.disambiguation = Some(m)
        prevFc.foreach(x => validate(ctx, m, x))
        ty match {
          case t: EirLambdaType => t
          case t: EirSpecializable =>
            ctx.getTemplatedType(t, x.types.map(visit(_)))
        }
      case _ => Errors.missingType(x)
    }
  }

  override def visitIfElse(
      x: EirIfElse
  )(implicit ctx: TypeCheckContext): EirType = {
    checkCondition(x.test)
    x.ifTrue.foreach(visit(_))
    x.ifFalse.foreach(visit(_))
    null
  }

  override def visitWhileLoop(
      x: EirWhileLoop
  )(implicit ctx: TypeCheckContext): EirType = {
    checkCondition(x.condition)
    visit(x.body)
  }

  override def visitNew(x: EirNew)(implicit ctx: TypeCheckContext): EirType = {
    val base = visit(x.target)
    val spec = handleSpecialization(base)
    val candidates = Find.accessibleConstructor(base, x, mustBeConcrete = true)
    val found = screenCandidates(
      ExpressionScope(Some(x), None),
      candidates.map((_, None))
    )

    found match {
      case head :: _ =>
        x.disambiguation = Some(head._1)
        spec.foreach(ctx.leave)
        base
      case _ => error(x, "could not find a suitable constructor")
    }
  }

  override def visitProxy(
      node: EirProxy
  )(implicit ctx: TypeCheckContext): EirType = {
    val element = ProxyManager.elementFor(node).getOrElse(node)
    val (_, _, opt) = handleSpecializable(ctx, element)

    opt.foreach(subCtx => {
      ctx.start(subCtx)

      element.members.map(visit(_))
      visit(element.base)

      ctx.stop(subCtx)
    })

    if (node.templateArgs.nonEmpty) {
      ctx.getTemplatedType(node, node.templateArgs.map(visit(_)))
    } else {
      node
    }
  }

  override def visitMatch(
      x: EirMatch
  )(implicit ctx: TypeCheckContext): EirType = {
    visit(x.expression)
    val cases = x.cases.map(visit(_))
    Find.unionType(cases).getOrElse(Errors.unableToUnify(x, cases))
  }

  override def visitMatchCase(
      x: EirMatchCase
  )(implicit ctx: TypeCheckContext): EirType = {
    ctx.goal.push(
      x.parent
        .to[EirMatch]
        .flatMap(_.expression.foundType)
        .getOrElse(Errors.missingType(x))
    )

    checkCondition(x.condition)

    visit(x.patterns)
    x.body.map(visit(_)).getOrElse(globals.unitType)
  }

  override def visitPatternList(
      x: EirPatternList
  )(implicit ctx: TypeCheckContext): EirType = {
    val goal = ctx.goal.pop()
    val theirs = goal match {
      case EirTupleType(_, progeny) => progeny.map(visit(_))
      case _                        => List(goal)
    }
    ctx.goal.pushAll(theirs.reverse)
    val ours = x.patterns.map(visit(_))
    if (
      (ours.isEmpty && theirs.headOption.contains(
        globals.unitType
      )) || ours.length == theirs.length
    ) null
    else Errors.invalidTupleIndices(EirTupleType(None, theirs), ours)
  }

  override def visitTupleType(
      x: types.EirTupleType
  )(implicit ctx: TypeCheckContext): EirType = {
    val children = x.children.flatMap {
      case x: EirPackExpansion => visit(x) match {
          case EirTupleType(_, ts) => ts
          case t                   => List(t)
        }
      case x => List(x)
    }
    ctx.getTupleType(visit(children))
  }

  def sameTypes(a: EirType, b: EirType)(implicit
      ctx: TypeCheckContext
  ): Boolean = {
    (a == b) || ((a, b) match {
      case (EirTemplatedType(_, b1, as1), EirTemplatedType(_, b2, as2))
          if as1.length == as2.length =>
        sameTypes(visit(b1), visit(b2)) &&
          as1
            .map(visit(_))
            .zip(as2.map(visit(_)))
            .forall(x => sameTypes(x._1, x._2))
      case _ => false
    })
  }

  override def visitIdentifierPattern(
      x: EirIdentifierPattern
  )(implicit ctx: TypeCheckContext): EirType = {
    val goal = ctx.goal.pop()
    val found = x.ty match {
      case _: EirPlaceholder[_] => goal
      // TODO add type-checking to verify:
      //      both goal/theirs and ours are pointer types
      // -or- ours can assign to theirs
      case t => visit(Find.uniqueResolution[EirResolvable[EirType]](t))
    }
    x.needsCasting = !sameTypes(goal, found)
    x.ty = found
    found
  }

  override def visitExpressionPattern(
      x: EirExpressionPattern
  )(implicit ctx: TypeCheckContext): EirType = {
    val goal = ctx.goal.pop()
    // TODO -- make this more reliable
    x.decl.declaredType = goal
    val ours = visit(x.expression)
    if (ours.canAssignTo(globals.boolType)) null
    else Errors.cannotCast(x, ours, globals.boolType)
  }

  @tailrec def hasField(x: EirType, field: String)(implicit
      ctx: TypeCheckContext
  ): Boolean = {
    x match {
      case t: EirTemplatedType => hasField(visit(t.base), field)
      case c: EirClassLike     => c.members.exists(_.name == field)
    }
  }

  override def visitAwait(
      x: EirAwait
  )(implicit ctx: TypeCheckContext): EirType = {
    val f = makeMemberCall(x.target, "get")
    x.disambiguation = Some(f)
    val retTy = visit(f)
    if (f.target.disambiguation.flatMap(_.annotation("sync")).isEmpty) {
      Errors.expectedSync(x, f)
    }
    if (x.target.foundType.exists(hasField(_, "release"))) {
      val f = makeMemberCall(x.target, "release")
      x.release = Some(f)
      visit(f)
    }
    retTy
  }

  def makeMemberCall(
      target: EirExpressionNode,
      field: String,
      args: List[EirExpressionNode] = Nil
  ): EirFunctionCall = {
    val f = EirFunctionCall(
      Some(target),
      null,
      args.map(EirCallArgument(_, isRef = false)(None)),
      Nil
    )
    val s = EirScopedSymbol(target, null)(Some(f))
    s.pending = EirSymbol(Some(s), List(field))
    f.target = s
    f
  }

  override def visitInterpolatedString(
      x: EirInterpolatedString
  )(implicit ctx: TypeCheckContext): EirType = {
    val strTy = globals.stringType
    x.children.foreach(f => {
      val ty = visit(f)
      if (!ty.canAssignTo(strTy)) {
        val fc = makeMemberCall(f, "toString")
        val retTy = visit(fc)
        if (retTy.canAssignTo(strTy)) {
          assert(x.replaceChild(f, fc))
        } else {
          Errors.unableToUnify(f, retTy, strTy)
        }
      }
    })
    globals.stringType
  }

  override def visitTypeAlias(
      x: EirTypeAlias
  )(implicit ctx: TypeCheckContext): EirType = {
    StaticEvaluator.evaluateToType(x.value)
  }

  override def visitTupleMultiply(
      multiply: types.EirTupleMultiply
  )(implicit ctx: TypeCheckContext): EirType = {
    val lhs = visit(multiply.lhs)
    val rhs = StaticEvaluator.evaluate(multiply.rhs)

    rhs match {
      case EirIntegerLiteral(0) => globals.unitType
      case EirIntegerLiteral(1) => lhs
      case EirIntegerLiteral(n) => ctx.getTupleType(List.fill(n)(lhs))
      case _                    => Errors.cannotCast(multiply, rhs.`type`, globals.integerType)
    }
  }

  override def visitConstantFacade(facade: EirConstantFacade)(implicit
      context: TypeCheckContext
  ): EirType = facade

  def checkCondition(
      x: EirExpressionNode
  )(implicit ctx: TypeCheckContext): Unit = {
    val boolTy = globals.boolType
    val condTy = visit(x)
    if (!condTy.canAssignTo(boolTy)) {
      Errors.cannotCast(x, condTy, boolTy)
    }
  }

  def checkCondition(
      x: Option[EirExpressionNode]
  )(implicit ctx: TypeCheckContext): Unit = {
    x.foreach(checkCondition(_))
  }

  override def visitWhen(
      x: EirSdagWhen
  )(implicit ctx: TypeCheckContext): EirType = {
    Find.parentOf[EirMember](x).foreach(_.makeEntryOnly())
    for ((i, p) <- x.patterns) {
      // TODO ensure that target is mailbox
      ctx.goal.push({
        visit(i) match {
          case x: EirLambdaType =>
            visit(x.from.toTupleType(allowUnit = true)(None))
          case _ => Errors.missingType(i)
        }
      })
      visit(p)
    }

    checkCondition(x.condition)

    visit(x.body)
  }

  override def visitSlice(slice: EirSlice)(implicit
      ctx: TypeCheckContext
  ): EirType = {
    val arrRef = slice.parent collect { case x: EirArrayReference => x }
    val isHead = arrRef.flatMap(_.args.headOption).contains(slice)
    val isLast = arrRef.flatMap(_.args.lastOption).contains(slice)
    val targetType = arrRef.map(_.target).map(visit)
    val one = EirIntegerLiteral(1)(None)

    // TODO changeover to begin/end using iterators/indices?
    val start = slice.start getOrElse {
      if (isHead) {
        EirIntegerLiteral(0)(None)
      } else {
        val prev = arrRef
          .map(_.args.indexOf(slice) - 1)
          .filter(_ >= 0)
          .flatMap(x => arrRef.map(_.args(x)))

        prev match {
          case Some(x: EirSlice) => x.end.map(y => {
              EirBinaryExpression(
                None,
                y,
                "-",
                EirBinaryExpression(None, y, "%", x.step getOrElse one)
              )
            }) getOrElse Errors.unboundSlice(slice, targetType)
          case Some(x) => x
          case _       => Errors.unboundSlice(slice, targetType)
        }
      }
    }

    val step = slice.step getOrElse one

    val end = slice.end getOrElse {
      val hasSizer = {
        targetType
          .flatMap(Find.tryClassLike)
          .flatMap(_.members collectFirst {
            case m @ EirMember(_, f: EirFunction, _)
                if f.name == "size" && f.functionArgs.isEmpty && !m.isStatic =>
              m
          })
          .nonEmpty
      }

      if (isLast && hasSizer) {
        EirFunctionCall(
          None,
          EirScopedSymbol(
            arrRef.get.target,
            EirSymbol[EirNamedNode](None, List("size"))
          )(None),
          Nil,
          Nil
        )
      } else {
        val peer = arrRef.map(_.args).flatMap { x =>
          Option.unless(isLast)(x(x.indexOf(slice) + 1))
        }

        peer match {
          case Some(x: EirSlice) =>
            x.start getOrElse Errors.unboundSlice(slice, targetType)
          case Some(x) => x
          case _       => Errors.unboundSlice(slice, targetType)
        }
      }
    }

    val args = List(start, step, end)
    val types = args.map(visit(_))
    val union =
      Find.unionType(types).getOrElse(Errors.unableToUnify(slice, types))
    val range =
      EirNew(None, EirTemplatedType(None, globals.rangeType, List(union)), args)
    slice.disambiguation = Some(range)
    visit(range)
  }

  override def visitAwaitMany(
      x: EirAwaitMany
  )(implicit ctx: TypeCheckContext): EirType = {
    x.children.foreach(visit(_))

    globals.unitType
  }

  override def visitUnaryExpression(x: EirUnaryExpression)(implicit
      ctx: TypeCheckContext
  ): EirType = {
    val name = globals.prefixOperatorMethod(x.op)
    val fc = makeMemberCall(x.rhs, name, Nil)
    x.disambiguation = Some(fc)
    visit(fc)
  }
}
