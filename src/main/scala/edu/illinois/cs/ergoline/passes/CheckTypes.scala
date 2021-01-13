package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.EirAccessibility.{EirAccessibility, Private, Protected}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirLambdaType, EirProxyType, EirTemplatedType, EirTupleType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.{EirPlaceholder, EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.{Errors, assertValid, validAccessibility}
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirType

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
    } else if (targetType.isInstanceOf[EirTupleType]) {
      val argType = Option.when(x.args.length == 1)(visit(ctx, x.args.head))
      val integer = globals.typeFor(EirLiteralTypes.Integer)
      if (!argType.exists(_.canAssignTo(integer))) {
        Errors.invalidTupleIndices(x.args)
      } else {
        val targetTypes = targetType.asInstanceOf[EirTupleType].children.map(visit(ctx, _))
        x.args.head match {
          case l: EirLiteral => targetTypes(l.toInt)
          case _ => Find.unionType(targetTypes).getOrElse(Errors.unableToUnify(x, targetTypes))
        }
      }
    } else {
      val f = generateLval(x)
      x.disambiguation = Some(f)
      visit(ctx, f)
    }
  }

  def autoApply(ctx: TypeCheckContext, target: EirExpressionNode, args: List[EirResolvable[EirType]]): Boolean = {
    (args.length == 1) && visit(ctx, target).canAssignTo(visit(ctx, args.head))
  }

  def handleSpecialization(ctx: TypeCheckContext, x : EirType): Option[EirSpecialization] = {
    x match {
      case x : EirTemplatedType =>
        val base = visit(ctx, x.base)
        val specialization = ctx.specialize(base.asInstanceOf[EirSpecializable], x)
        visit(ctx, x.base)
        Some(specialization)
      case x : EirSpecializable if x.templateArgs.nonEmpty => Some(ctx.specialize(x))
      case _ => None
    }
  }

  // TODO fix behavior for static field members
  override def visitFieldAccessor(ctx: TypeCheckContext, x: EirFieldAccessor): EirType = {
    val base = visit(ctx, x.target)
    val spec = handleSpecialization(ctx, base)
    // TODO detection of cameViaFC and expectsSelf not at 100% yet, need to work on these
    //      and implement self-application as well (i.e. (42).toString vs 42.toString())
//    val expectsSelf = x.target match {
//      // we do not expect ourself for static applications and that's it :)
//      case s: EirSymbol[_] => !Find.uniqueResolution(s).isInstanceOf[EirClassLike]
//      // one may only make field accesses to a specialized class
//      // NOTE unless self-symbol application is added i.e. f<3> sugaring to f<3>()...
//      // NOTE i think that's fairly unlikely tho cause' it's kinda vague
//      case _: EirSpecializedSymbol => false
//      case _ => true
//    }
    val prevFc: Option[EirFunctionCall] = ctx.immediateAncestor[EirFunctionCall]
    val cameViaFuncCall = prevFc.isDefined && !prevFc.exists(_.args.contains(x))
    val ours =
      if (cameViaFuncCall) prevFc.get.args.map(visit(ctx, _)) else Nil
    // find the candidates ^_^
    val candidates = Find.resolveAccessor(ctx, base, x)
    val results = candidates.map(pair => {
      val (candidate, member) = pair
      val innerSpec = handleSpecialization(ctx, member)
      val found = member match {
        case t : EirLambdaType =>
          val theirs = t.from.map(visit(ctx, _))
          if (argumentsMatch(ours, theirs)) {
            (candidate, EirLambdaType(t.parent, theirs, visit(ctx, t.to)))
          } else {
            null
          }
        case x if ours.isEmpty => (candidate, x)
        case _ => null
      }
      innerSpec.foreach(ctx.leave)
      found
    })
    val found = results
      .filterNot(_ == null).headOption
    spec.foreach(ctx.leave)
    found match {
      case Some((member, result)) =>
        x.disambiguation = Some(member)
        prevFc.foreach(validate(ctx, member, _))
        result
      case None =>
        Errors.missingField(x, base, x.field)
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
    val res = ProxyManager.proxyFor(x)
    visit(ctx, res)
    res
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
    case s: EirSymbol[_] => isSelf(s) || s.disambiguation.collect({
      case b: EirMember => sharedBase(a, b)
      case n: EirNode => n.parent.to[EirMember].exists(sharedBase(a, _))
    }).getOrElse(false)
    case f: EirFieldAccessor => targetsSelf(a, f.target)
    case p: EirPostfixExpression => targetsSelf(a, p.target)
    case _ => false
  }

  def validate(ctx: TypeCheckContext, target: EirMember, call: EirFunctionCall): Unit = {
    val current = ctx.ancestor[EirMember]
    val bases = current.map(x => (x.base, target.base))
    if ((bases.isEmpty && !target.isPublic) ||
         bases.exists(!accessibleMember(_, target.accessibility))) {
      Errors.inaccessibleMember(target, call)
    }
    if (target.isEntryOnly && current.exists(targetsSelf(_, call))) {
      current.foreach(_.makeEntryOnly())
    }
  }

  override def visitFunctionCall(ctx: TypeCheckContext, call: EirFunctionCall): EirType = {
    val target = visit(ctx, call.target) match {
      case EirTemplatedType(_, _ : EirClassLike, _) | _: EirClassLike =>
        // TODO cycle this back into testing routine, use makeMemberFunction
        val accessor = EirFieldAccessor(Some(call), call.target, "apply")
        call.target.parent = Some(accessor)
        call.target = accessor
        // call.target.disambiguation = Some(accessor)
        return visit(ctx, call)
      case ty => ty
    }
    val ours = call.args.map(visit(ctx, _))
    // everything else should be resolved already, or resolved "above" the specialization
    target match {
      case EirLambdaType(_, args, retTy, _) if argumentsMatch(ours, args.map(visit(ctx, _))) => {
        retTy match {
          case t: EirType => t
          case _ => visit(ctx, retTy)
        }
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
      case _ => ???
    }
  }

  override def visitForLoop(ctx: TypeCheckContext, loop: EirForLoop): EirType = {
    loop.header match {
      case EirCStyleHeader(decl, test, incr) => {
        visit(ctx, decl)
        val ttype = test.map(visit(ctx, _))
        val boolean = globals.typeFor(EirLiteralTypes.Boolean)
        if (!ttype.exists(_.canAssignTo(boolean))) {
          Errors.cannotCast(loop, ttype.get, boolean)
        }
        visit(ctx, incr)
      }
      case h: EirForAllHeader => {
        val iterTy = resolveIterator(ctx, h)
        if (h.identifiers.length == 1) {
          h.declarations.head.declaredType = iterTy
        }
        else ???
      }
      case _ => ???
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

  override def visitSymbol[A <: EirNamedNode](ctx: TypeCheckContext, value: EirSymbol[A]): EirType = {
    val prevFc = value.parent.collect({ case f: EirFunctionCall if f.target.contains(value) => f })
    val ours = prevFc.map(_.args.map(visit(ctx, _))).getOrElse(Nil)
    val self = Option.when(isSelf(value))(value.qualifiedName.last)
    val candidates = {
      val resolved = value.resolve()
      if (resolved.isEmpty && self.isDefined) {
        ctx.ancestor[EirMember] match {
          case Some(m) =>
            m.selfDeclarations.filter(n => self.contains(n.name))
          case _ => Nil
        }
      } else {
        resolved
      }
    }
    val found = candidates.find({
      case f: EirFunction => argumentsMatch(ours, f.functionArgs.map(visit(ctx, _)))
      case EirMember(_, f: EirFunction, _) => argumentsMatch(ours, f.functionArgs.map(visit(ctx, _)))
      case s: EirSpecializable if s.templateArgs.nonEmpty && !ctx.hasSubstitution(s) && !value.parent.exists(_.isInstanceOf[EirTemplatedType]) =>
        Errors.missingSpecialization(s)
      case f => ours.isEmpty || (visit(ctx, f) match {
        case t: EirLambdaType => argumentsMatch(ours, t.from.map(visit(ctx, _)))
        case _ => false
      })
    })
    value.disambiguation = found
    val retTy = found.map(visit(ctx, _))
    prevFc
      .zip(found.collect({case m: EirMember => m}))
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
        case p: EirPlaceholder[_] => {
          p.expectation.map(visit(ctx, _))
        }
        case t => Some(visit(ctx, t))
      }
    }
    val rval = node.initialValue.map(visit(ctx, _))
    val ty = (lval, rval) match {
      case (None, None) => Errors.missingType(node)
      case (Some(a), None) => a
      case (None, Some(b)) => {
        node.declaredType = b
        b
      }
      case (Some(a), Some(b)) => {
        if (b.canAssignTo(a)) a
        else Errors.cannotCast(node, b, a)
      }
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
    // TODO check self-assigning arguments?
//    val args = node.functionArgs.map(_.declaredType).map(visit(ctx, _))
//    val retTy: EirType = visit(ctx, node.returnType)
//    val found: Option[EirType] = node.body.flatMap(body => Option(visit(ctx, body)))
//    // TODO or member of abstract class (to be added
//    val noReturnOk = retTy.isUnitType ||
//      (node.body.isEmpty && annotationsOf(node).map(_.name).contains("system"))
//    found match {
//      case None if !noReturnOk =>
//        throw TypeCheckException(s"expected a return value of type $retTy for ${node.name}")
//      case Some(other) if !other.canAssignTo(retTy) =>
//        throw TypeCheckException(s"${node.name} cannot return a value of type $other")
//      case _ =>
//    }
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
    val func = globals.operatorToFunction(op).getOrElse(Errors.unknownOperator(node, op))
    val f = makeMemberCall(node.lhs, func, List(node.rhs))
    node.disambiguation = Some(f)
    val retTy = visit(ctx, f)
    if (func == "compareTo") {
      val integer = globals.typeFor(EirLiteralTypes.Integer)
      if (retTy.canAssignTo(integer)) {
        globals.typeFor(EirLiteralTypes.Boolean)
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

  def isFinalDecl(lval : EirExpressionNode): Boolean = {
    lval match {
      case s : EirSymbol[_] => Find.uniqueResolution(s) match {
        case d : EirDeclaration => d.isFinal
        case a : EirFunctionArgument => a.isFinal
        case _ => false
      }
      case _: EirArrayReference => false
      case _ => ???
    }
  }

  override def visitAssignment(ctx: TypeCheckContext, node: EirAssignment): EirType = {
    val lval = visit(ctx, node.rval)
    val rval = visit(ctx, node.lval)
    if (!rval.canAssignTo(lval)) {
      Errors.cannotCast(node, lval, rval)
    } else if (isFinalDecl(node.lval)) {
      error(ctx, node, s"$rval cannot be assigned to final ${node.lval}")
    } else {
      null
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

  def visitSpecializedFunction(ctx: TypeCheckContext, f: EirFunction, x: EirSpecialization): EirType = {
    if (f.templateArgs.length != x.specialization.length) {
      error(ctx, x, "template length mismatch")
    }
    val enter = ctx.specialize(f, x)
    val result = visitFunction(ctx, f)
    result.from = result.from.map(visit(ctx, _))
    result.to = visit(ctx, result.to)
    result.templateArgs = Nil
    ctx.leave(enter)
    result
  }

  override def visitSpecializedSymbol(ctx: TypeCheckContext, x: EirSpecializedSymbol): EirType = {
    // TODO iterate through overloads
    val specializable = Find.uniqueResolution(x.symbol)
    specializable.asInstanceOf[Any] match {
      case c : EirClassLike if c.templateArgs.length == x.specialization.length => EirTemplatedType(Some(x), c, x.specialization)
      // TODO this only works because of a bug with Find.child wherein it ignores the requested type... womp womp
      case EirMember(_, f : EirFunction, _) => visitSpecializedFunction(ctx, f, x)
      case f: EirFunction => visitSpecializedFunction(ctx, f, x)
      case _ => error(ctx, x, "i'm still stupid")
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
    val ours = visit(ctx, x.args).toList
    x.disambiguation = candidates.find((m : EirMember) => {
      val f = m.member.asInstanceOf[EirFunction]
      val theirs =
        // proxy-related args are provided by the runtime
        f.functionArgs.map(visit(ctx, _))
      val matches = argumentsMatch(ours, theirs)
      matches
    })
    spec.foreach(ctx.leave)
    x.disambiguation match {
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
    node
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
    if (ours.length != theirs.length) Errors.invalidTupleIndices(theirs)
    else null
  }

  override def visitTupleType(ctx: TypeCheckContext, x: types.EirTupleType): EirType = {
    x.children = x.children.map(visit(ctx, _))
    x
  }

  override def visitIdentifierPattern(ctx: TypeCheckContext, x: EirIdentifierPattern): EirType = {
    val goal = ctx.goal.pop()
    val ours = x.ty
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
    val ours = visit(ctx, x.conditions.head)
    if (ours.canAssignTo(goal)) null
    else Errors.cannotCast(x, ours, goal)
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
    f.target = EirFieldAccessor(Some(f), target, field)
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
}
