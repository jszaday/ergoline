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

  override def visitArrayReference(x: EirArrayReference)(implicit ctx: TypeCheckContext): EirType = {
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
        val lit = Option.when(x.args.length == 1)(x.args.head).map(evaluateConstExpr(_))
        val idx = lit.collect { case x if x.`type` == EirLiteralTypes.Integer => x.toInt }
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

  def handleSpecialization(x : EirType)(implicit ctx: TypeCheckContext): Either[EirSpecializable, EirSpecialization] = {
    val base = x match {
      case x: EirTemplatedType => visit(x)
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
  override def visitScopedSymbol[A <: EirNode](x: EirScopedSymbol[A])(implicit ctx: TypeCheckContext): EirType = {
    val base = visit(x.target)
    val spec = handleSpecialization(base)
    val prevFc: Option[EirFunctionCall] =
      ctx.immediateAncestor[EirFunctionCall].filter(_.target.contains(x))
    val candidates = Find.resolveAccessor(x, Some(base))
    val found = screenCandidates(prevFc, candidates)
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

  override def visitTernaryOperator(x: EirTernaryOperator)(implicit ctx: TypeCheckContext): EirType = {
    val testTy = visit(x.test)
    val boolean = globals.typeFor(EirLiteralTypes.Boolean)
    if (testTy.canAssignTo(boolean)) {
      val tty = visit(x.ifTrue)
      val fty = visit(x.ifFalse)
      Find.unionType(tty, fty) match {
        case Some(found) => found
        case None => Errors.unableToUnify(x, tty, fty)
      }
    } else {
      Errors.cannotCast(x, testTy, boolean)
    }
  }

  override def visitLambdaType(x: types.EirLambdaType)(implicit ctx: TypeCheckContext): EirType = {
    ctx.lambdaWith(x.from.map(visit(_)), visit(x.to), x.templateArgs)
  }

  override def visitTemplatedType(x: types.EirTemplatedType)(implicit ctx: TypeCheckContext): EirType = {
    val base: EirSpecializable = Find.uniqueResolution[EirSpecializable](x.base)
    val spec = ctx.specialize(base, x)
    visit(base)
    ctx.leave(spec)
    // visit our base
    base match {
      case c : EirClassLike => ctx.getTemplatedType(c, x.args.map(visit(_)))
      case _ => error(x, s"unsure how to specialize ${x.base}")
    }
  }

  override def visitProxyType(x: types.EirProxyType)(implicit ctx: TypeCheckContext): EirType = {
    visit(ProxyManager.proxyFor(x))
  }

  override def visitImport(eirImport: EirImport)(implicit ctx: TypeCheckContext): EirType = null

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

  override def visitFunctionCall(call: EirFunctionCall)(implicit ctx: TypeCheckContext): EirType = {
    
    val target = visit(call.target)
    val ours = call.args.map(visit(_))
    // everything else should be resolved already, or resolved "above" the specialization
    target match {
      case EirLambdaType(_, args, retTy, _) if argumentsMatch(ours, args.map(visit(_))) =>
        retTy match {
          case t: EirType => t
          case _ => visit(retTy)
        }
      case _ => error(call, s"cannot resolve ((${ours mkString ", "}) => ???) and $target")
    }
  }

  def argumentsMatch(x: Iterable[EirType], y: Iterable[EirType], exact: Boolean)(implicit ctx: TypeCheckContext): Boolean =
    argumentsMatch(x.toList, y.toList, exact)

  def argumentsMatch(x: List[EirType], y: List[EirType], exact: Boolean = false)(implicit ctx: TypeCheckContext): Boolean = {
    (x.length == y.length) &&
      x.zip(y).forall({
        case (x, y) if exact => x == y
        case (x, y) => x.canAssignTo(y)
      })
  }

  def resolveIterator(h: EirForAllHeader)(implicit ctx: TypeCheckContext): EirType = {
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

  override def visitForLoop(loop: EirForLoop)(implicit ctx: TypeCheckContext): EirType = {
    
    loop.header match {
      case EirCStyleHeader(decl, test, incr) =>
        visit(decl)
        val ttype = test.map(visit(_))
        val boolean = globals.typeFor(EirLiteralTypes.Boolean)
        if (!ttype.exists(_.canAssignTo(boolean))) {
          Errors.cannotCast(loop, ttype.get, boolean)
        }
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

  override def visitLiteral(value: EirLiteral)(implicit ctx: TypeCheckContext): EirType = {
    globals.typeFor(value)
  }

  def isSelf(value: EirSymbol[_]): Boolean =
    value.qualifiedName match {
      case ("self" | "self@" | "self[@]") :: Nil => true
      case _ => false
    }

  /** Merges two maps of type arguments to types, using unification.
   *  Propagates unification errors as null to indicate incompatibility
   */
  private def merge(a: Map[EirTemplateArgument, EirType], b: Map[EirTemplateArgument, EirType])(implicit ctx: TypeCheckContext): Map[EirTemplateArgument, EirType] = {
    (a.toList ++ b.toList).groupMap(_._1)(_._2).map {
      case (arg, tys) => arg -> {
        tys.reduce((a, b) =>
          Option.when(a != null && b != null)(Find.unionType(a, b).orNull).orNull
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
  private def learn(pair: (EirResolvable[EirType], EirType))(implicit ctx: TypeCheckContext): Map[EirTemplateArgument, EirType] = {
    pair match {
      case (EirPlaceholder(_, Some(a)), b) => learn((a, b))
      case (a: EirTemplatedType, b: EirTemplatedType) if a.args.length == b.args.length =>
        (learn((a.base, visit(b.base))) +: a.args.zip(b.args.map(visit(_))).map(learn)).reduce(merge)
      case (a: EirLambdaType, b: EirLambdaType) if a.from.length == b.from.length =>
        (learn((a.to, visit(b.to))) +: a.from.zip(b.from.map(visit(_))).map(learn)).reduce(merge)
      case (t: EirTemplateArgument, b) => Map(t -> b)
      case (_: EirSymbol[_] | _: EirSpecializedSymbol, b) => learn((Find.uniqueResolution[EirResolvable[EirType]](pair._1), b))
      case (_: EirProxyType, _: EirProxyType) => ???
      case _ => Map()
    }
  }

  def inferSpecialization(s: EirSpecializable, args: List[EirType])(implicit ctx: TypeCheckContext): Option[EirSpecialization] = {
    // TODO this does not consider static applications (e.g. option<?>(42))
    val unknowns = Option(s).to[EirLambdaType].map(_.from)
    val insights = unknowns
      .filter(_.length == args.length)
      .map(_.zip(args).map(learn(_)))
      .map(_.reduce(merge)).getOrElse(Map())

    val paired = s.templateArgs.map(t => {
      insights.get(t).orElse(t.defaultValue.map(visit(_)))
    })

    Option.when(paired.forall(x =>
      x.isDefined && !x.contains(null)
    ))(ctx.synthesize(paired.flatten))
  }

  def screenCandidates(argsrc: Option[EirExpressionNode],
                       candidates: Iterable[(EirNamedNode, EirType)])
                      (implicit ctx: TypeCheckContext): Option[(EirNamedNode, EirType)] = {
    val results = candidates.flatMap(pair => {
      val (candidate, member) = pair

      val (ispec, args) = handleSpecialization(member) match {
        case Left(s) =>
          val args = getArguments(argsrc)
          val sp = args
            .flatMap(inferSpecialization(s, _))
            .flatMap(ctx.trySpecialize(s, _))
            .getOrElse(return None)
          (sp, args)
        case Right(sp) => (sp, getArguments(argsrc))
      }

      val found = (member, args) match {
        case (EirTemplatedType(_, _ : EirClassLike, _) | _: EirClassLike, Some(_)) =>
          // TODO this should be applicable without the cast (only necessary until SpecializedSymbol::resolve impl'd)
          val sp = Option(member).to[EirTemplatedType].map(s =>
            handleSpecialization(s) match {
              case Left(s) => Errors.missingSpecialization(s)
              case Right(sp) => sp
            }).orNull
          val candidates = Find.accessibleMember(Find.asClassLike(member), argsrc.get, "apply")
          val found = screenCandidates(argsrc, candidates.view.zip(candidates.map(visit(_))))
          ctx.leave(sp)
          found
        case (_ : EirLambdaType, Some(ours)) =>
          val t = assertValid[EirLambdaType](visit(candidate))
          if (argumentsMatch(ours, t.from.map(assertValid[EirType]))) {
            // NOTE the double check is necessary here to visit candidate if it hasn't
            // already been visited... and since visitFunction doesn't resolve its types
            Some((candidate, t))
          } else {
            None
          }
        case (x, None) => Some(candidate, visit(x))
        case (_, Some(_)) => None
      }

      ctx.leave(ispec)
      found
    })

    // TODO implement some safety here, one must hide the others or it's ambiguous!
    results.headOption
  }

  def getArguments(opt: Option[EirExpressionNode])(implicit ctx: TypeCheckContext): Option[List[EirType]] = {
    val args = opt.collect{
      case f: EirFunctionCall => f.args
      case n: EirNew => n.args
    }
    args.map(_.map(visit(_)))
  }

  private def zipWithVisit(ns: Iterable[EirNamedNode])(implicit ctx: TypeCheckContext): Iterable[(EirNamedNode, EirType)] = {
    ns.view.zip(ns.view.map(visit(_)))
  }

  override def visitSymbol[A <: EirNamedNode](value: EirSymbol[A])(implicit ctx: TypeCheckContext): EirType = {
    val prevFc = value.parent.collect({ case f: EirFunctionCall if f.target.contains(value) => f })
    val self = Option.when(isSelf(value))(value.qualifiedName.last)
    val candidates = self match {
      case Some(name) =>
        ctx.ancestor[EirMember] match {
          case Some(m) => zipWithVisit(m.selfDeclarations.filter(_.name == name))
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
            Find.resolveAccessor(accessor, asCls)
          } else {
            zipWithVisit(value.resolve())
          }
        } else {
          zipWithVisit(value.resolve())
        }
    }
    val found = screenCandidates(prevFc, candidates)
    value.disambiguation = found.map(_._1)
    val retTy = found.map(x => visit(x._2))
    prevFc
      .zip(asMember(found.map(_._1)))
      .foreach(x => validate(ctx, x._2, x._1))
    retTy.getOrElse(self match {
      case Some(_) => throw MissingSelfException(value)
      case _ => Errors.unableToResolve(value)
    })
  }

  // TODO when the last statement in a block is an expression, put a "return" there
  override def visitBlock(node: EirBlock)(implicit ctx: TypeCheckContext): EirType = {
    
    node.children.foreach(visit(_))
    val retTys: List[EirType] = Find.descendant(node, {
      case _: EirLambdaExpression => None
      case _: EirFunction => None
      case _: EirReturn => Some(true)
      case _ => Some(false)
    }).map(visit(_)).toList
    if (retTys.isEmpty) globals.typeFor(EirLiteralTypes.Unit)
    else Find.unionType(retTys) match {
      case Some(x) => x
      case None => Errors.unableToUnify(node, retTys)
    }
  }

  override def visitNamespace(node: EirNamespace)(implicit ctx: TypeCheckContext): EirType = {
    visit(node.children)
    null
  }

  // TODO fix to be context sensitive! Otherwise this gon' blow up!
  override def visitDeclaration(node: EirDeclaration)(implicit ctx: TypeCheckContext): EirType = {
    ctx.avail(node).foreach(return _)
    val lval = {
      node.declaredType match {
        case p: EirPlaceholder[_] => p.expectation.map(visit(_))
        case t => Some(visit(t))
      }
    }
    val rval = node.initialValue.map(visit(_))
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

  override def visitTemplateArgument(node: EirTemplateArgument)(implicit ctx: TypeCheckContext): EirType = {
    ctx.hasSubstitution(node) match {
      case Some(x) => visit(x)
      case _ => Errors.missingSpecialization(node)
    }
  }

  // TODO need to check parent classes/traits too
  //      since they may not be reached otherwise
  def visitClassLike(node: EirClassLike)(implicit ctx: TypeCheckContext): EirType = {
    val (_, _, opt) = handleSpecializable(ctx, node)
    opt.foreach(subCtx => {
      ctx.start(subCtx)
      CheckClasses.visit(ctx, node)
      node.members.foreach(visit(_))
      node.inherited.foreach(visit(_))
      if (node.annotation("main").isDefined) {
        visitProxyType(EirProxyType(None, node, None, isElement = false))
      }
      ctx.stop(subCtx)
    })
    node
  }

  override def visitClass(node: EirClass)(implicit ctx: TypeCheckContext): EirType = visitClassLike(node)

  override def visitTrait(node: EirTrait)(implicit ctx: TypeCheckContext): EirType = visitClassLike(node)

  override def visitMember(node: EirMember)(implicit ctx: TypeCheckContext): EirType = {
    val proxy = ctx.ancestor[EirClassLike].collect{ case p: EirProxy => p }
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

  def isSystem(parent: Option[EirMember], node: EirNode): Boolean = {
    parent.exists(isSystem(None, _)) || node.annotation("system").isDefined
  }

  def handleSpecializable(ctx: TypeCheckContext, s: EirSpecializable): (Boolean, Option[EirMember], Option[TypeCheckContext.Context]) = {
    val member = ctx.immediateAncestor[EirMember]
    val noArgs = s.templateArgs.isEmpty
    val spec = if (noArgs) None else ctx.findSubstitution(s)
    val isDefined = noArgs || spec.isDefined
    (isDefined, member, {
      if (isSystem(member, s) || !isDefined) None
      else ctx.mkCheckContext(s, spec)
    })
  }

  override def visitFunction(node: EirFunction)(implicit ctx: TypeCheckContext): EirLambdaType = {
    val (isDefined, member, opt) = handleSpecializable(ctx, node)
    val proxy = member.flatMap(_.parent).to[EirProxy]

    opt
      .filterNot(_ => proxy.isDefined && node.body.isEmpty)
      .foreach(subCtx => {
      try {
        CheckFunctions.visit(ctx, node)
        ctx.start(subCtx)
        val bodyType = node.body.map(visit(_))
        val retTy = visit(node.returnType)
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

    if (isDefined) {
      ctx.lambdaWith(expand(node.functionArgs), visit(node.returnType))
    } else {
      // NOTE expansions are explicitly not known here...
      //      what should this actually return? a placeholder?
      ctx.lambdaWith(node.functionArgs.map(_.declaredType), node.returnType, node.templateArgs)
    }
  }

  def annotationsOf(node: EirFunction): List[EirAnnotation] = {
    if (node.parent.exists(_.isInstanceOf[EirMember])) {
      node.parent.get.annotations
    } else {
      node.annotations
    }
  }

  def error(x: EirNode, msg: String)(implicit ctx: TypeCheckContext): EirType = Errors.exit(Errors.format(x, msg))

  override def visitAnnotation(node: EirAnnotation)(implicit ctx: TypeCheckContext): EirType =
    error(node, "annotations are type-less")

  override def visitBinaryExpression(node: EirBinaryExpression)(implicit ctx: TypeCheckContext): EirType = {
    
    val op = if (node.op == "!=") "==" else node.op
    val boolTy = globals.typeFor(EirLiteralTypes.Boolean)
    if (op == "===" || op == "!==") {
      val (lhsTy, rhsTy) = (visit(node.lhs), visit(node.rhs))
      if (lhsTy == rhsTy) {
        return boolTy
      } else {
        Errors.unableToUnify(node, lhsTy, rhsTy)
      }
    } else if (op == "&&" || op == "||") {
      val (lhsTy, rhsTy) = (visit(node.lhs), visit(node.rhs))
      val failure =
        Option.when(!lhsTy.canAssignTo(boolTy))(lhsTy).orElse(
          Option.when(!rhsTy.canAssignTo(boolTy))(rhsTy))
      failure.foreach(Errors.unableToUnify(node, _, boolTy))
      return boolTy
    }
    val func = globals.operatorToFunction(op).getOrElse(Errors.unknownOperator(node, op))
    val f = makeMemberCall(node.lhs, func, List(node.rhs))
    node.disambiguation = Some(f)
    val retTy = visit(f)
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

  override def visitFunctionArgument(node: EirFunctionArgument)(implicit ctx: TypeCheckContext): EirType = {
    visit(node.declaredType)
  }

  override def visit(node: EirNode)(implicit ctx: TypeCheckContext): EirType = {
    ctx.enterNode(node)
    val result = super.visit(node)
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
      case s : EirSymbol[_] => Find.uniqueResolution[EirNode](s) match {
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

  override def visitAssignment(node: EirAssignment)(implicit ctx: TypeCheckContext): EirType = {
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

  override def visitTupleExpression(node: EirTupleExpression)(implicit ctx: TypeCheckContext): EirType = {
    EirTupleType(Some(node), visit(node.children).toList)
  }

  private def expand(args: List[EirFunctionArgument])(implicit ctx: TypeCheckContext): List[EirType] = {
    args.flatMap(arg => {
      visit(arg.declaredType) match {
        case t: EirTupleType if arg.isExpansion => t.children.map(visit(_))
        case t => List(t)
      }
    })
  }

  override def visitLambdaExpression(node: EirLambdaExpression)(implicit ctx: TypeCheckContext): EirType = {
    if (!ctx.lambdas.contains(node)) ctx.lambdas +:= node

    ctx.lambdaWith(expand(node.args), visit(node.body))
  }

  override def visitReturn(node: EirReturn)(implicit ctx: TypeCheckContext): EirType = {
    visit(node.expression)
  }

  final case class TypeCheckException(message: String) extends Exception(message)

  override def visitSpecializedSymbol(x: EirSpecializedSymbol)(implicit ctx: TypeCheckContext): EirType = {
    val prevFc: Option[EirFunctionCall] =
      ctx.immediateAncestor[EirFunctionCall].filter(_.target.contains(x))
    // TODO this should probably return templated types
    val candidates = Find.resolutions[EirNamedNode](x.symbol)
    val found = screenCandidates(prevFc, candidates.view.zip(candidates.map(visit(_))))
    found match {
      case Some((m, ty)) =>
        x.disambiguation = Some(m)
        prevFc.foreach(x => validate(ctx, m, x))
        ty match {
          case t: EirLambdaType => t
          case t: EirSpecializable => ctx.getTemplatedType(t, x.types.map(visit(_)))
        }
      case None => Errors.missingType(x)
    }
  }

  override def visitIfElse(x: EirIfElse)(implicit ctx: TypeCheckContext): EirType = {
    val retTy = visit(x.test)
    val boolean = globals.typeFor(EirLiteralTypes.Boolean)
    if (!retTy.canAssignTo(boolean)) {
      Errors.cannotCast(x, retTy, boolean)
    }
    x.ifTrue.foreach(visit(_))
    x.ifFalse.foreach(visit(_))
    null
  }

  override def visitWhileLoop(x: EirWhileLoop)(implicit ctx: TypeCheckContext): EirType = {
    val exprTy = x.condition.map(visit(_))
    val boolean = globals.typeFor(EirLiteralTypes.Boolean)
    if (!exprTy.forall(_.canAssignTo(boolean))) {
      Errors.cannotCast(x, exprTy.get, boolean)
    }
    visit(x.body)
  }

  override def visitNew(x: EirNew)(implicit ctx: TypeCheckContext): EirType = {
    val base = visit(x.target)
    val spec = handleSpecialization(base)
    val candidates = Find.accessibleConstructor(base, x, mustBeConcrete = true)
    val found = screenCandidates(Some(x), candidates.zip(candidates.map(visit(_))))
    x.disambiguation = found.map(_._1)
    spec.foreach(ctx.leave)
    found match {
      case Some(_) => base
      case _ => error(x, "could not find a suitable constructor")
    }
  }

  override def visitProxy(node: EirProxy)(implicit ctx: TypeCheckContext): EirType = {
    val (_, _, opt) = handleSpecializable(ctx, node)
    opt.foreach(subCtx => {
      ctx.start(subCtx)
      val element = ProxyManager.elementFor(node).getOrElse(node)
      element.members.map(visit(_))
      visit(node.base)
      ctx.stop(subCtx)
    })
    if (node.templateArgs.nonEmpty) {
      ctx.getTemplatedType(node, node.templateArgs.map(visit(_)))
    } else {
      node
    }
  }

  override def visitMatch(x: EirMatch)(implicit ctx: TypeCheckContext): EirType = {
    visit(x.expression)
    val cases = x.cases.map(visit(_))
    Find.unionType(cases).getOrElse(Errors.unableToUnify(x, cases))
  }

  override def visitMatchCase(x: EirMatchCase)(implicit ctx: TypeCheckContext): EirType = {
    ctx.goal.push(x.parent.to[EirMatch].flatMap(_.expression.foundType).getOrElse(Errors.missingType(x)))
    val boolean = globals.typeFor(EirLiteralTypes.Boolean)
    val condTy = x.condition.map(visit(_))
    if (!condTy.forall(_.canAssignTo(boolean))) {
      Errors.cannotCast(x.condition.get, condTy.get, boolean)
    }
    visit(x.patterns)
    x.body.map(visit(_)).getOrElse(globals.typeFor(EirLiteralTypes.Unit))
  }

  override def visitPatternList(x: EirPatternList)(implicit ctx: TypeCheckContext): EirType = {
    val goal = ctx.goal.pop()
    val theirs = goal match {
      case EirTupleType(_, progeny) => progeny.map(visit(_))
      case _ => List(goal)
    }
    ctx.goal.pushAll(theirs.reverse)
    val ours = x.patterns.map(visit(_))
    if ((ours.isEmpty && theirs.headOption.contains(globals.unitType)) || ours.length == theirs.length) null
    else Errors.invalidTupleIndices(EirTupleType(None, theirs), ours)
  }

  override def visitTupleType(x: types.EirTupleType)(implicit ctx: TypeCheckContext): EirType = {
    val children = x.children.flatMap{
      case x: EirPackExpansion =>
        val found = x.resolve().headOption.getOrElse(Errors.unableToResolve(x))
        visit(found) match {
          case EirTupleType(_, ts) => ts
          case t => List(t)
        }
      case x => List(x)
    }
    ctx.getTupleType(visit(children))
  }

  def sameTypes(a: EirType, b: EirType)(implicit ctx: TypeCheckContext): Boolean = {
    (a == b) || ((a, b) match {
      case (EirTemplatedType(_, b1, as1), EirTemplatedType(_, b2, as2)) if as1.length == as2.length =>
        sameTypes(visit(b1), visit(b2)) &&
          as1.map(visit(_)).zip(as2.map(visit(_))).forall(x => sameTypes(x._1, x._2))
      case _ => false
    })
  }

  override def visitIdentifierPattern(x: EirIdentifierPattern)(implicit ctx: TypeCheckContext): EirType = {
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

  override def visitExpressionPattern(x: EirExpressionPattern)(implicit ctx: TypeCheckContext): EirType = {
    val goal = ctx.goal.pop()
    // TODO -- make this more reliable
    x.decl.declaredType = goal
    val ours = visit(x.expression)
    if (ours.canAssignTo(globals.boolType)) null
    else Errors.cannotCast(x, ours, globals.boolType)
  }

  @tailrec def hasField(x: EirType, field: String)(implicit ctx: TypeCheckContext): Boolean = {
    x match {
      case t: EirTemplatedType => hasField(visit(t.base), field)
      case c: EirClassLike => c.members.exists(_.name == field)
    }
  }

  override def visitAwait(x: EirAwait)(implicit ctx: TypeCheckContext): EirType = {
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

  def makeMemberCall(target: EirExpressionNode, field: String, args: List[EirExpressionNode] = Nil): EirFunctionCall = {
    val f = EirFunctionCall(Some(target), null, args, Nil)
    val s = EirScopedSymbol(target, null)(Some(f))
    s.pending = EirSymbol(Some(s), List(field))
    f.target = s
    f
  }

  override def visitInterpolatedString(x: EirInterpolatedString)(implicit ctx: TypeCheckContext): EirType = {
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

  override def visitTypeAlias(x: EirTypeAlias)(implicit ctx: TypeCheckContext): EirType = {
    visit(x.resolve().headOption.getOrElse(Errors.unableToResolve(x.value)))
  }

  def valueWithin(x: EirResolvable[_])(implicit ctx: TypeCheckContext): EirLiteral = {
    x.resolve().headOption.collect {
      case y: EirConstantFacade => y.value
      case x: EirTemplateArgument => valueWithin(visit(x))
    }.getOrElse(Errors.unableToResolve(x))
  }

  def evaluateConstExpr(expr: EirExpressionNode)(implicit ctx: TypeCheckContext): EirLiteral = {
    expr match {
      case x: EirSymbol[_] => valueWithin(x)
      case x: EirLiteral => x
      case _ => Errors.invalidConstExpr(expr)
    }
  }

  override def visitTupleMultiply(multiply: types.EirTupleMultiply)(implicit ctx: TypeCheckContext): EirType = {
    val lhs = visit(multiply.lhs)
    val rhs = evaluateConstExpr(multiply.rhs)
    val intTy = EirLiteralTypes.Integer
    if (rhs.`type` != intTy ) {
      Errors.unableToUnify(multiply, visit(rhs), globals.typeFor(intTy))
    } else {
      rhs.toInt match {
        case 0 => globals.unitType
        case 1 => lhs
        case n => ctx.getTupleType(List.fill(n)(lhs))
      }
    }
  }

  override def visitConstantFacade(facade: EirConstantFacade)(implicit context: TypeCheckContext): EirType = facade

  override def visitWhen(x: EirSdagWhen)(implicit ctx: TypeCheckContext): EirType = {
    Find.parentOf[EirMember](x).foreach(_.makeEntryOnly())
    for ((i, p) <- x.patterns) {
      // TODO ensure that target is mailbox
      ctx.goal.push({
        visit(i) match {
          case x: EirLambdaType => visit(x.from.toTupleType(allowUnit = true)(None))
          case _ => Errors.missingType(i)
      }})
      visit(p)
    }
    val boolTy = globals.typeFor(EirLiteralTypes.Boolean)
    val condTy = x.condition.map(visit(_))
    if (!condTy.forall(_.canAssignTo(boolTy))) {
      Errors.cannotCast(x.condition.get, condTy.get, boolTy)
    }
    visit(x.body)
  }

  override def visitSlice(x: EirSlice)(implicit ctx: TypeCheckContext): EirType = ???

  override def visitAwaitMany(x: EirAwaitMany)(implicit ctx: TypeCheckContext): EirType = {
    x.children.foreach(visit(_))

    globals.unitType
  }
}
