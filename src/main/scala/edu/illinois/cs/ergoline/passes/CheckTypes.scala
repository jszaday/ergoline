package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.EirAccessibility.{EirAccessibility, Protected}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.asMember
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.Find.tryClassLike
import edu.illinois.cs.ergoline.resolution.{EirPlaceholder, EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichOption, RichResolvableTypeIterable}
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirType
import edu.illinois.cs.ergoline.util.{Errors, assertValid, validAccessibility}

import scala.annotation.tailrec

object CheckTypes extends EirVisitor[TypeCheckContext, EirType] {

  // TODO this should consider the current substitutions, and check each unique substitution!
  var classCache: List[EirClass] = Nil

  final case class MissingSelfException[A <: EirNamedNode](symbol: EirSymbol[A]) extends Exception

  private def generateLval(x: EirArrayReference): EirExpressionNode = {
    makeMemberCall(x.target, "get",  x.args)
  }

  private def generateRval(x: EirAssignment): EirExpressionNode = {
    val operator = Option.unless(x.op == "=")(
      globals.operatorToFunction(x.op.init).getOrElse(Errors.unknownOperator(x, x.op)))
    val arrayRef = assertValid[EirArrayReference](x.lval)
    val rval = operator match {
      case Some(s) => makeMemberCall(generateLval(arrayRef), s, List(x.rval))
      case None => x.rval
    }
    makeMemberCall(arrayRef.target, "set", arrayRef.args :+ rval)
  }

  override def visitArrayReference(ctx: TypeCheckContext, x: EirArrayReference): EirType = {
    val assignment = x.parent.to[EirAssignment]
    val targetType = visit(ctx, x.target)
    if (assignment.exists(_.lval == x)) {
      val f = generateRval(assignment.get)
      x.disambiguation = Some(f)
      visit(ctx, f)
      // TODO this is redundant, can we maybe return nothing?
      //      (it gets visited in visitAssignment again)
      visit(ctx, assignment.get.rval)
    } else targetType match {
      case tupleType: EirTupleType =>
        val lit = Option.when(x.args.length == 1)(x.args.head).map(evaluateConstExpr(ctx, _))
        val idx = lit.collect { case x if x.`type` == EirLiteralTypes.Integer => x.toInt }
        if (idx.exists(_ < tupleType.children.length)) {
          visit(ctx, tupleType.children(idx.get))
        } else {
          Errors.invalidTupleIndices(tupleType, x.args)
        }
      case _ =>
        val f = generateLval(x)
        x.disambiguation = Some(f)
        visit(ctx, f)
    }
  }

  def handleSpecialization(ctx: TypeCheckContext, x : EirType): Either[EirSpecializable, EirSpecialization] = {
    val base = x match {
      case x: EirTemplatedType => visit(ctx, x)
      case x => x
    }

    base match {
      case sp@EirTemplatedType(_, s: EirSpecializable, _) => ctx.trySpecialize(s, sp).toRight(s)
      case _ : EirTemplatedType => Errors.missingSpecialization(x)
      case x : EirSpecializable if x.templateArgs.nonEmpty => ctx.trySpecialize(x).toRight(x)
      case _ => Right(null)
    }
  }

  // TODO add checks for static-ness?
  override def visitScopedSymbol[A <: EirNode](ctx: TypeCheckContext, x: EirScopedSymbol[A]): EirType = {
    val base = visit(ctx, x.target)
    val spec = handleSpecialization(ctx, base)
    val prevFc: Option[EirFunctionCall] =
      ctx.immediateAncestor[EirFunctionCall].filter(_.target.contains(x))
    val candidates = Find.resolveAccessor(ctx, x, Some(base))
    val found = screenCandidates(ctx, prevFc, candidates)
    spec.foreach(ctx.leave)
    found match {
      case Some((member, result)) =>
        x.disambiguation = Some(member)
        prevFc.foreach(validate(ctx, member, _))
        result
      case None =>
        Errors.unableToResolve(x.pending)
    }
  }

  override def visitTernaryOperator(ctx: TypeCheckContext, x: EirTernaryOperator): EirType = {
    val testTy = visit(ctx, x.test)
    val boolean = globals.typeFor(EirLiteralTypes.Boolean)
    if (testTy.canAssignTo(boolean)) {
      val tty = visit(ctx, x.ifTrue)
      val fty = visit(ctx, x.ifFalse)
      Find.unionType(tty, fty) match {
        case Some(found) => found
        case None => Errors.unableToUnify(x, tty, fty)
      }
    } else {
      Errors.cannotCast(x, testTy, boolean)
    }
  }

  override def visitLambdaType(ctx: TypeCheckContext, x: types.EirLambdaType): EirType = {
    x.from = x.from.map(visit(ctx, _))
    x.to = visit(ctx, x.to)
    x
  }

  override def visitTemplatedType(ctx: TypeCheckContext, x: types.EirTemplatedType): EirType = {
    val base: EirSpecializable = assertValid[EirSpecializable](Find.uniqueResolution(x.base))
    val spec = ctx.specialize(base, x)
    visit(ctx, base)
    ctx.leave(spec)
    // visit our base
    base match {
      case c : EirClassLike => ctx.getTemplatedType(c, x.args.map(visit(ctx, _)))
      case _ => error(ctx, x, s"unsure how to specialize ${x.base}")
    }
  }

  override def visitProxyType(ctx: TypeCheckContext, x: types.EirProxyType): EirType = {
    visit(ctx, ProxyManager.proxyFor(x))
  }

  override def visitImport(ctx: TypeCheckContext, eirImport: EirImport): EirType = null

  @tailrec
  def accessibleMember(pair: (EirClassLike, EirClassLike), accessibility: EirAccessibility): Boolean = {
    pair match {
      case (a: EirProxy, b: EirProxy) => accessibleMember((a.base, b.base), accessibility)
      case (a, b: EirProxy) => accessibleMember((a, b.base), accessibility)
      case (a: EirProxy, b) => accessibleMember((a.base, b), accessibility)
      case (a, b) => EirAccessibility.compatible(validAccessibility(a, b), accessibility)
    }
  }

  def sharedBase(a: EirClassLike, b: EirClassLike): Boolean = accessibleMember((a, b), Protected)

  def sharedBase(a: EirMember, b: EirMember): Boolean = sharedBase(a.base, b.base)

  @tailrec
  def targetsSelf(a: EirMember, node: EirExpressionNode): Boolean = node match {
    case s: EirSymbol[_] => isSelf(s) || asMember(s.disambiguation).exists(sharedBase(a, _))
    case f: EirScopedSymbol[_] => targetsSelf(a, f.target)
    case p: EirPostfixExpression => targetsSelf(a, p.target)
    case _ => false
  }

  def validate(ctx: TypeCheckContext, target: EirNamedNode, call: EirFunctionCall): Unit = {
    target match {
      case member: EirMember =>
        val current = ctx.ancestor[EirMember]
        val bases = current.map(x => (x.base, member.base))
        if ((bases.isEmpty && !member.isPublic) ||
          bases.exists(!accessibleMember(_, member.accessibility))) {
          Errors.inaccessibleMember(member, call)
        }
        // NOTE this should probably check to see if self@ only?
        if (member.isEntryOnly && current.exists(targetsSelf(_, call))) {
          current.foreach(_.makeEntryOnly())
        }
      case _ =>
    }
  }

  override def visitFunctionCall(ctx: TypeCheckContext, call: EirFunctionCall): EirType = {
    val target = visit(ctx, call.target)
    val ours = call.args.map(visit(ctx, _))
    // everything else should be resolved already, or resolved "above" the specialization
    target match {
      case EirLambdaType(_, args, retTy, _) if argumentsMatch(ours, args.map(visit(ctx, _))) =>
        retTy match {
          case t: EirType => t
          case _ => visit(ctx, retTy)
        }
      case _ => error(ctx, call, s"cannot resolve ((${ours mkString ", "}) => ???) and $target")
    }
  }

  def argumentsMatch(x: Iterable[EirType], y: Iterable[EirType], exact: Boolean): Boolean =
    argumentsMatch(x.toList, y.toList, exact)

  def argumentsMatch(x: List[EirType], y: List[EirType], exact: Boolean = false): Boolean = {
    (x.length == y.length) &&
      x.zip(y).forall({
        case (x, y) if exact => x == y
        case (x, y) => x.canAssignTo(y)
      })
  }

  def resolveIterator(ctx: TypeCheckContext, h: EirForAllHeader): EirType = {
    visit(ctx, h.expression) match {
      case t: EirTemplatedType =>
        val iterableTy = globals.iterableType
        val iteratorTy = globals.iteratorType
        val base = Find.uniqueResolution(t.base)
        if (base.canAssignTo(iterableTy)) {
          h.expression = makeMemberCall(h.expression, "iter")
          visit(ctx, h.expression)
        } else if (!base.canAssignTo(iteratorTy)) {
          Errors.unableToUnify(h.expression, List(base, iterableTy, iteratorTy))
        }
        visit(ctx, t.args.head)
      case t => Errors.incorrectType(t, EirTemplatedType.getClass)
    }
  }

  override def visitForLoop(ctx: TypeCheckContext, loop: EirForLoop): EirType = {
    loop.header match {
      case EirCStyleHeader(decl, test, incr) =>
        visit(ctx, decl)
        val ttype = test.map(visit(ctx, _))
        val boolean = globals.typeFor(EirLiteralTypes.Boolean)
        if (!ttype.exists(_.canAssignTo(boolean))) {
          Errors.cannotCast(loop, ttype.get, boolean)
        }
        visit(ctx, incr)
      case h: EirForAllHeader =>
        val iterTy = resolveIterator(ctx, h)
        if (h.identifiers.length == 1) {
          h.declarations.head.declaredType = iterTy
        } else ???
      case _ => Errors.unreachable()
    }
    visit(ctx, loop.body)
  }

  override def visitLiteral(ctx: TypeCheckContext, value: EirLiteral): EirType = {
    globals.typeFor(value)
  }

  def isSelf(value: EirSymbol[_]): Boolean =
    value.qualifiedName match {
      case ("self" | "self@" | "self[@]") :: Nil => true
      case _ => false
    }

  def applyFuncArgs(ctx: TypeCheckContext, node: EirNode, types: List[EirResolvable[EirType]]): List[EirType] = {
    val fnArgs = node match {
      case EirMember(_, f: EirFunction, _) => Some(f.functionArgs)
      case f: EirFunction => Some(f.functionArgs)
      case l: EirLambdaExpression => Some(l.args)
      case _ => None
    }
    fnArgs.map(args => args.zip(types.map(visit(ctx, _))).flatMap {
      case (arg, ty : EirTupleType) if arg.isExpansion => ty.children.map(visit(ctx, _))
      case (_, ty) => Some(ty)
    }).getOrElse(visit(ctx, node) match {
      // NOTE this does not consider expansions?
      case t: EirLambdaType => t.from.map(visit(ctx, _))
      case _ => Nil
    })
  }

  def inferSpecialization(s: EirSpecializable, args: List[EirType]): Option[EirSpecialization] = {
    None
  }

  def screenCandidates(ctx: TypeCheckContext, argsrc: Option[EirExpressionNode],
                       candidates: Iterable[(EirNamedNode, EirType)]): Option[(EirNamedNode, EirType)] = {
    val results = candidates.flatMap(pair => {
      val (candidate, member) = pair
      val (ispec, args) = handleSpecialization(ctx, member) match {
        case Left(s) =>
          val args = getArguments(ctx, argsrc)
          val sp = args
            .flatMap(inferSpecialization(s, _))
            .flatMap(ctx.trySpecialize(s, _))
            .getOrElse(Errors.missingSpecialization(s))
          (sp, args)
        case Right(sp) => (sp, getArguments(ctx, argsrc))
      }
      val found = (member, args) match {
        case (EirTemplatedType(_, _ : EirClassLike, _) | _: EirClassLike, Some(_)) =>
          // TODO this should be applicable without the cast (only necessary until SpecializedSymbol::resolve impl'd)
          val sp = Option(member).to[EirTemplatedType].map(s =>
            handleSpecialization(ctx, s) match {
              case Left(s) => Errors.missingSpecialization(s)
              case Right(sp) => sp
            }).orNull
          val candidates = Find.accessibleMember(Find.asClassLike(member), argsrc.get, "apply")
          val found = screenCandidates(ctx, argsrc, candidates.view.zip(candidates.map(visit(ctx, _))))
          ctx.leave(sp)
          found
        case (t : EirLambdaType, Some(ours)) =>
          val theirs = applyFuncArgs(ctx, candidate, t.from)
          if (argumentsMatch(ours, theirs)) {
            Some((candidate, EirLambdaType(t.parent, theirs, visit(ctx, t.to))))
          } else {
            None
          }
        case (x, None) => Some(candidate, visit(ctx, x))
        case (_, Some(_)) => None
      }
      ctx.leave(ispec)
      found
    })
    results.headOption
  }

  def getArguments(ctx: TypeCheckContext, opt: Option[EirExpressionNode]): Option[List[EirType]] = {
    val args = opt.collect{
      case f: EirFunctionCall => f.args
      case n: EirNew => n.args
    }
    args.map(_.map(visit(ctx, _)))
  }

  private def zipWithVisit(ctx: TypeCheckContext, ns: Iterable[EirNamedNode]): Iterable[(EirNamedNode, EirType)] = {
    ns.view.zip(ns.view.map(visit(ctx, _)))
  }

  override def visitSymbol[A <: EirNamedNode](ctx: TypeCheckContext, value: EirSymbol[A]): EirType = {
    val prevFc = value.parent.collect({ case f: EirFunctionCall if f.target.contains(value) => f })
    val self = Option.when(isSelf(value))(value.qualifiedName.last)
    val candidates = self match {
      case Some(name) =>
        ctx.ancestor[EirMember] match {
          case Some(m) => zipWithVisit(ctx, m.selfDeclarations.filter(_.name == name))
          case _ => Nil
        }
      case None =>
        val (init, last) = (value.qualifiedName.init, value.qualifiedName.last)
        if (init.nonEmpty) {
          val parent = EirSymbol[EirNamedNode](value.parent, init).resolve().headOption
          val asCls = parent.flatMap(tryClassLike)
          if (asCls.isDefined) {
            val accessor = EirScopedSymbol(null, EirSymbol(value.parent, List(last)))(value.parent)
            accessor.isStatic = true
            Find.resolveAccessor(ctx, accessor, asCls)
          } else {
            zipWithVisit(ctx, value.resolve())
          }
        } else {
          zipWithVisit(ctx, value.resolve())
        }
    }
    val found = screenCandidates(ctx, prevFc, candidates)
    value.disambiguation = found.map(_._1)
    val retTy = found.map(x => visit(ctx, x._2))
    prevFc
      .zip(asMember(found.map(_._1)))
      .foreach(x => validate(ctx, x._2, x._1))
    retTy.getOrElse(self match {
      case Some(_) => throw MissingSelfException(value)
      case _ => Errors.unableToResolve(value)
    })
  }

  // TODO when the last statement in a block is an expression, put a "return" there
  override def visitBlock(ctx: TypeCheckContext, node: EirBlock): EirType = {
    node.children.foreach(visit(ctx, _))
    val retTys: List[EirType] = Find.descendant(node, {
      case _: EirLambdaExpression => None
      case _: EirFunction => None
      case _: EirReturn => Some(true)
      case _ => Some(false)
    }).map(visit(ctx, _)).toList
    if (retTys.isEmpty) globals.typeFor(EirLiteralTypes.Unit)
    else Find.unionType(retTys) match {
      case Some(x) => x
      case None => Errors.unableToUnify(node, retTys)
    }
  }

  override def visitNamespace(ctx: TypeCheckContext, node: EirNamespace): EirType = {
    visit(ctx, node.children)
    null
  }

  // TODO fix to be context sensitive! Otherwise this gon' blow up!
  override def visitDeclaration(ctx: TypeCheckContext, node: EirDeclaration): EirType = {
    ctx.avail(node).foreach(return _)
    val lval = {
      node.declaredType match {
        case p: EirPlaceholder[_] => p.expectation.map(visit(ctx, _))
        case t => Some(visit(ctx, t))
      }
    }
    val rval = node.initialValue.map(visit(ctx, _))
    val ty = (lval, rval) match {
      case (None, None) => Errors.missingType(node)
      case (Some(a), None) => a
      case (None, Some(b)) => node.declaredType = b; b
      case (Some(a), Some(b)) if b.canAssignTo(a) => a
      case (Some(a), Some(b)) => Errors.cannotCast(node, b, a)
    }
    ctx.cache(node, ty)
    ty
  }

  override def visitTemplateArgument(ctx: TypeCheckContext, node: EirTemplateArgument): EirType = {
    ctx.hasSubstitution(node) match {
      case Some(x) => visit(ctx, x)
      case _ => Errors.missingSpecialization(node)
    }
  }

  def error(ctx: TypeCheckContext, node: EirNode, message: String): EirType = {
    throw TypeCheckException(s"${node.location.map(_.toString).getOrElse(node.toString)}: $message")
  }

  // TODO need to check parent classes/traits too
  //      since they may not be reached otherwise
  def visitClassLike(ctx: TypeCheckContext, node: EirClassLike): EirType = {
    val opt = ctx.shouldCheck(node)
    opt.foreach(subCtx => {
      ctx.start(subCtx)
      CheckClasses.visit(node)
      node.members.foreach(visit(ctx, _))
      node.inherited.foreach(visit(ctx, _))
      if (node.annotation("main").isDefined) {
        visitProxyType(ctx, EirProxyType(None, node, None, isElement = false))
      }
      ctx.stop(subCtx)
    })
    node
  }

  override def visitClass(ctx: TypeCheckContext, node: EirClass): EirType = {
    visitClassLike(ctx, node)
  }

  override def visitTrait(ctx: TypeCheckContext, node: EirTrait): EirType = visitClassLike(ctx, node)

  override def visitMember(ctx: TypeCheckContext, node: EirMember): EirType = {
    val proxy = ctx.ancestor[EirClassLike].collect{ case p: EirProxy => p }
    visit(ctx, (proxy, node.counterpart) match {
      case (_, Some(m)) =>
        visit(ctx, m.member)
        visit(ctx, node.member)
      case (Some(p), _) => p.members
        .find(_.counterpart.contains(node))
        .getOrElse(node.member)
      case (_, _) => node.member
    })
  }

  override def visitFunction(ctx: TypeCheckContext, node: EirFunction): EirLambdaType = {
    val member = ctx.immediateAncestor[EirMember]
    val proxy = member.flatMap(_.parent).to[EirProxy]
    val opt = (proxy, node.body) match {
      case (Some(_), None) => None
      case _ => ctx.shouldCheck(node)
    }
    opt.foreach(subCtx => {
      try {
        CheckFunctions.visit(ctx, node)
        ctx.start(subCtx)
        val bodyType = node.body.map(visit(ctx, _))
        val retTy = visit(ctx, node.returnType)
        if (!bodyType.forall(_.canAssignTo(retTy))) {
          Errors.unableToUnify(node, bodyType.get, retTy)
        }
        ctx.stop(subCtx)
      } catch {
        case e: MissingSelfException[_] =>
          // TODO fix up this logic
          //      needs to consider self[@] being inaccessible in foo@, e.g.
          if (member.exists(!_.isStatic)) {
            member.foreach(_.makeEntryOnly())
            ctx.popUntil(node)
            ctx.stop(subCtx)
            return null
          } else {
            Errors.unableToResolve(e.symbol)
          }
      }
    })
    EirLambdaType(Some(node), node.functionArgs.map(_.declaredType), node.returnType, node.templateArgs)
  }

  def annotationsOf(node: EirFunction): List[EirAnnotation] = {
    if (node.parent.exists(_.isInstanceOf[EirMember])) {
      node.parent.get.annotations
    } else {
      node.annotations
    }
  }

  override def visitAnnotation(ctx: TypeCheckContext, node: EirAnnotation): EirType =
    error(ctx, node, "annotations are type-less")

  override def visitBinaryExpression(ctx: TypeCheckContext, node: EirBinaryExpression): EirType = {
    val op = if (node.op == "!=") "==" else node.op
    val boolTy = globals.typeFor(EirLiteralTypes.Boolean)
    if (op == "===" || op == "!==") {
      val (lhsTy, rhsTy) = (visit(ctx, node.lhs), visit(ctx, node.rhs))
      if (lhsTy == rhsTy) {
        return boolTy
      } else {
        Errors.unableToUnify(node, lhsTy, rhsTy)
      }
    } else if (op == "&&" || op == "||") {
      val (lhsTy, rhsTy) = (visit(ctx, node.lhs), visit(ctx, node.rhs))
      val failure =
        Option.when(!lhsTy.canAssignTo(boolTy))(lhsTy).orElse(
          Option.when(!rhsTy.canAssignTo(boolTy))(rhsTy))
      failure.foreach(Errors.unableToUnify(node, _, boolTy))
      return boolTy
    }
    val func = globals.operatorToFunction(op).getOrElse(Errors.unknownOperator(node, op))
    val f = makeMemberCall(node.lhs, func, List(node.rhs))
    node.disambiguation = Some(f)
    val retTy = visit(ctx, f)
    if (func == "compareTo") {
      val integer = globals.typeFor(EirLiteralTypes.Integer)
      if (retTy.canAssignTo(integer)) {
        boolTy
      } else {
        Errors.cannotCast(node, retTy, integer)
      }
    } else {
      retTy
    }
  }

  override def visitFunctionArgument(ctx: TypeCheckContext, node: EirFunctionArgument): EirType = {
    visit(ctx, node.declaredType)
  }

  override def visit(ctx: TypeCheckContext, node: EirNode): EirType = {
    ctx.enterNode(node)
    val result = super.visit(ctx, node)
    node match {
      case x: EirExpressionNode =>
        x.foundType = Option(result) match {
          case None => Errors.missingType(x)
          case o => o
        }
      case _ =>
    }
    if (ctx.alreadyLeft(node)) result
    else ctx.leaveWith(result)
  }

  def isInvalidFinalAssign(node : EirAssignment): Boolean = {
    node.lval match {
      case s : EirSymbol[_] => Find.uniqueResolution(s) match {
        case d : EirDeclaration => d.isFinal && {
          !d.parent.to[EirMember].map(_.base).exists(x => {
            Find.parentOf[EirMember](node)
              .exists(m => m.base == x && m.isConstructor)
          })
        }
        case _ : EirFunctionArgument => true
        case _ => false
      }
      case _ : EirArrayReference => false
      case _ : EirScopedSymbol[_] => ???
      case _ => true
    }
  }

  override def visitAssignment(ctx: TypeCheckContext, node: EirAssignment): EirType = {
    val lval = visit(ctx, node.lval)
    val rval = visit(ctx, node.rval)
    if (!rval.canAssignTo(lval)) {
      Errors.cannotCast(node, lval, rval)
    } else if (isInvalidFinalAssign(node)) {
      Errors.assignToVal(node)
    } else {
      globals.unitType
    }
  }

  override def visitTupleExpression(ctx: TypeCheckContext, node: EirTupleExpression): EirType = {
    EirTupleType(Some(node), visit(ctx, node.children).toList)
  }

  override def visitLambdaExpression(ctx: TypeCheckContext, node: EirLambdaExpression): EirType = {
    if (!ctx.lambdas.contains(node)) ctx.lambdas +:= node
    val retTy = visit(ctx, node.body)
    if (retTy == null) throw TypeCheckException(s"could not find return type of $node")
    EirLambdaType(Some(node), node.args.map(visit(ctx, _)), retTy)
  }

  override def visitReturn(ctx: TypeCheckContext, node: EirReturn): EirType = {
    visit(ctx, node.expression)
  }

  final case class TypeCheckException(message: String) extends Exception(message)

  override def visitSpecializedSymbol(ctx: TypeCheckContext, x: EirSpecializedSymbol): EirType = {
    val prevFc: Option[EirFunctionCall] =
      ctx.immediateAncestor[EirFunctionCall].filter(_.target.contains(x))
    // TODO this should probably return templated types
    val candidates = x.symbol.resolve()
    val found = screenCandidates(ctx, prevFc, candidates.view.zip(candidates.map(visit(ctx, _))))
    found match {
      case Some((m, ty)) =>
        x.disambiguation = Some(m)
        prevFc.foreach(x => validate(ctx, m, x))
        ty match {
          case t: EirLambdaType => t
          case t: EirSpecializable => ctx.getTemplatedType(t, x.types.map(visit(ctx, _)))
        }
      case None => Errors.missingType(x)
    }
  }

  override def visitIfElse(ctx: TypeCheckContext, x: EirIfElse): EirType = {
    val retTy = visit(ctx, x.test)
    val boolean = globals.typeFor(EirLiteralTypes.Boolean)
    if (!retTy.canAssignTo(boolean)) {
      Errors.cannotCast(x, retTy, boolean)
    }
    x.ifTrue.foreach(visit(ctx, _))
    x.ifFalse.foreach(visit(ctx, _))
    null
  }

  override def visitWhileLoop(ctx: TypeCheckContext, x: EirWhileLoop): EirType = {
    val exprTy = x.condition.map(visit(ctx, _))
    val boolean = globals.typeFor(EirLiteralTypes.Boolean)
    if (!exprTy.forall(_.canAssignTo(boolean))) {
      Errors.cannotCast(x, exprTy.get, boolean)
    }
    visit(ctx, x.body)
  }

  override def visitNew(ctx: TypeCheckContext, x: EirNew): EirType = {
    val base = visit(ctx, x.target)
    val spec = handleSpecialization(ctx, base)
    val candidates = Find.accessibleConstructor(base, x, mustBeConcrete = true)
    val found = screenCandidates(ctx, Some(x), candidates.zip(candidates.map(visit(ctx, _))))
    x.disambiguation = found.map(_._1)
    spec.foreach(ctx.leave)
    found match {
      case Some(_) => base
      case _ => error(ctx, x, "could not find a suitable constructor")
    }
  }

  override def visitProxy(ctx: TypeCheckContext, node: EirProxy): EirType = {
    val opt = ctx.shouldCheck(node)
    opt.foreach(subCtx => {
      ctx.start(subCtx)
      val element = ProxyManager.elementFor(node).getOrElse(node)
      element.members.map(visit(ctx, _))
      visit(ctx, node.base)
      ctx.stop(subCtx)
    })
    if (node.templateArgs.nonEmpty) {
      ctx.getTemplatedType(node, node.templateArgs.map(visit(ctx, _)))
    } else {
      node
    }
  }

  override def visitMatch(ctx: TypeCheckContext, x: EirMatch): EirType = {
    visit(ctx, x.expression)
    val cases = x.cases.map(visit(ctx, _))
    Find.unionType(cases).getOrElse(Errors.unableToUnify(x, cases))
  }

  override def visitMatchCase(ctx: TypeCheckContext, x: EirMatchCase): EirType = {
    ctx.goal.push(x.parent.to[EirMatch].flatMap(_.expression.foundType).getOrElse(Errors.missingType(x)))
    val boolean = globals.typeFor(EirLiteralTypes.Boolean)
    val condTy = x.condition.map(visit(ctx, _))
    if (!condTy.forall(_.canAssignTo(boolean))) {
      Errors.cannotCast(x.condition.get, condTy.get, boolean)
    }
    visit(ctx, x.patterns)
    x.body.map(visit(ctx, _)).getOrElse(globals.typeFor(EirLiteralTypes.Unit))
  }

  override def visitPatternList(ctx: TypeCheckContext, x: EirPatternList): EirType = {
    val goal = ctx.goal.pop()
    val theirs = goal match {
      case EirTupleType(_, progeny) => progeny.map(visit(ctx, _))
      case _ => List(goal)
    }
    ctx.goal.pushAll(theirs.reverse)
    val ours = x.patterns.map(visit(ctx, _))
    if ((ours.isEmpty && theirs.headOption.contains(globals.unitType)) || ours.length == theirs.length) null
    else Errors.invalidTupleIndices(EirTupleType(None, theirs), ours)
  }

  override def visitTupleType(ctx: TypeCheckContext, x: types.EirTupleType): EirType = {
    val children = x.children.flatMap{
      case x: EirPackExpansion =>
        val found = x.resolve().headOption.getOrElse(Errors.unableToResolve(x))
        visit(ctx, found) match {
          case EirTupleType(_, ts) => ts
          case t => List(t)
        }
      case x => List(x)
    }
    ctx.getTupleType(visit(ctx, children))
  }

  override def visitIdentifierPattern(ctx: TypeCheckContext, x: EirIdentifierPattern): EirType = {
    val goal = ctx.goal.pop()
    val found = x.ty match {
      case _: EirPlaceholder[_] => goal
      // TODO add type-checking to verify:
      //      both goal/theirs and ours are pointer types
      // -or- ours can assign to theirs
      case t => Find.uniqueResolution(t)
     }
    x.ty = found
    found
  }

  override def visitExpressionPattern(ctx: TypeCheckContext, x: EirExpressionPattern): EirType = {
    val goal = ctx.goal.pop()
    // TODO -- make this more reliable
    x.decl.declaredType = goal
    val ours = visit(ctx, x.expression)
    if (ours.canAssignTo(globals.boolType)) null
    else Errors.cannotCast(x, ours, globals.boolType)
  }

  @tailrec def hasField(x: EirType, field: String): Boolean = {
    x match {
      case t: EirTemplatedType => hasField(Find.uniqueResolution(t.base), field)
      case c: EirClassLike => c.members.exists(_.name == field)
    }
  }

  override def visitAwait(ctx: TypeCheckContext, x: EirAwait): EirType = {
    val f = makeMemberCall(x.target, "get")
    x.disambiguation = Some(f)
    val retTy = visit(ctx, f)
    if (f.target.disambiguation.flatMap(_.annotation("sync")).isEmpty) {
      Errors.expectedSync(x, f)
    }
    if (x.target.foundType.exists(hasField(_, "release"))) {
      val f = makeMemberCall(x.target, "release")
      x.release = Some(f)
      visit(ctx, f)
    }
    retTy
  }

  def makeMemberCall(target: EirExpressionNode, field: String, args: List[EirExpressionNode] = Nil): EirFunctionCall = {
    val f = EirFunctionCall(Some(target), null, args, Nil)
    val s = EirScopedSymbol(target, null)(Some(f))
    s.pending = EirSymbol(Some(s), List(field))
    f.target = s
    f
  }

  override def visitInterpolatedString(ctx: TypeCheckContext, x: EirInterpolatedString): EirType = {
    val strTy = globals.stringType
    x.children.foreach(f => {
      val ty = visit(ctx, f)
      if (!ty.canAssignTo(strTy)) {
        val fc = makeMemberCall(f, "toString")
        val retTy = visit(ctx, fc)
        if (retTy.canAssignTo(strTy)) {
          assert(x.replaceChild(f, fc))
        } else {
          Errors.unableToUnify(f, retTy, strTy)
        }
      }
    })
    globals.stringType
  }

  override def visitTypeAlias(ctx: TypeCheckContext, x: EirTypeAlias): EirType = {
    visit(ctx, x.resolve().headOption.getOrElse(Errors.unableToResolve(x.value)))
  }

  def valueWithin(ctx: TypeCheckContext, x: EirResolvable[_]): EirLiteral = {
    x.resolve().headOption.collect {
      case y: EirConstantFacade => y.value
      case x: EirTemplateArgument => valueWithin(ctx, visit(ctx, x))
    }.getOrElse(Errors.unableToResolve(x))
  }

  def evaluateConstExpr(ctx: TypeCheckContext, expr: EirExpressionNode): EirLiteral = {
    expr match {
      case x: EirSymbol[_] => valueWithin(ctx, x)
      case x: EirLiteral => x
      case _ => Errors.invalidConstExpr(expr)
    }
  }

  override def visitTupleMultiply(ctx: TypeCheckContext, multiply: types.EirTupleMultiply): EirType = {
    val lhs = visit(ctx, multiply.lhs)
    val rhs = evaluateConstExpr(ctx, multiply.rhs)
    val intTy = EirLiteralTypes.Integer
    if (rhs.`type` != intTy ) {
      Errors.unableToUnify(multiply, visit(ctx, rhs), globals.typeFor(intTy))
    } else {
      rhs.toInt match {
        case 0 => globals.unitType
        case 1 => lhs
        case n => ctx.getTupleType(List.fill(n)(lhs))
      }
    }
  }

  override def visitConstantFacade(context: TypeCheckContext, facade: EirConstantFacade): EirType = facade

  override def visitWhen(ctx: TypeCheckContext, x: EirSdagWhen): EirType = {
    Find.parentOf[EirMember](x).foreach(_.makeEntryOnly())
    for ((i, p) <- x.patterns) {
      // TODO ensure that target is mailbox
      ctx.goal.push({
        visit(ctx, i) match {
          case x: EirLambdaType => visit(ctx, x.from.toTupleType(allowUnit = true)(None))
          case _ => Errors.missingType(i)
      }})
      visit(ctx, p)
    }
    val boolTy = globals.typeFor(EirLiteralTypes.Boolean)
    val condTy = x.condition.map(visit(ctx, _))
    if (!condTy.forall(_.canAssignTo(boolTy))) {
      Errors.cannotCast(x.condition.get, condTy.get, boolTy)
    }
    visit(ctx, x.body)
  }

  override def visitSlice(ctx: TypeCheckContext, x: EirSlice): EirType = ???
}
