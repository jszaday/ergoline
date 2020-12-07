package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirLambdaType, EirTemplatedType, EirTupleType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichEirNode, RichOption}
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirType

object CheckTypes extends EirVisitor[TypeCheckContext, EirType] {

  // TODO this should consider the current substitutions, and check each unique substitution!
  var classCache: List[EirClass] = Nil

  override def visitArrayReference(ctx: TypeCheckContext, x: EirArrayReference): EirType = {
    val assignment = x.parent.to[EirAssignment]
    val targetType = visit(ctx, x.target)
    if (assignment.exists(_.lval == x)) {
      ???
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
      val f = EirFunctionCall(Some(x), null, x.args, Nil)
      f.target = EirFieldAccessor(Some(f), x.target, "get")
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
        Some(ctx.specialize(base.asInstanceOf[EirSpecializable], x))
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
    val expectsSelf = x.target match {
      // we do not expect ourself for static applications and that's it :)
      case s: EirSymbol[_] => !Find.uniqueResolution(s).isInstanceOf[EirClassLike]
      // one may only make field accesses to a specialized class
      // NOTE unless self-symbol application is added i.e. f<3> sugaring to f<3>()...
      // NOTE i think that's fairly unlikely tho cause' it's kinda vague
      case _: EirSpecializedSymbol => false
      case _ => true
    }
    val prevFc: Option[EirFunctionCall] = ctx.cameVia[EirFunctionCall]
    val cameViaFuncCall = prevFc.isDefined && !prevFc.exists(_.args.contains(x))
    val ours =
      if (cameViaFuncCall) prevFc.get.args.map(visit(ctx, _)) else Nil
    // find the candidates ^_^
    val candidates = Find.accessibleMember(base, x)
    val results = candidates.map(candidate => {
      val member = visit(ctx, candidate)
      val innerSpec = handleSpecialization(ctx, member)
      val found = member match {
        case t : EirLambdaType =>
          val theirs = t.from.map(visit(ctx, _))
          if (argumentsMatch(ours, theirs)) {
            (candidate, EirLambdaType(t.parent, theirs, visit(ctx, t.to)))
          } else {
            null
          }
        case x if ours.isEmpty || (expectsSelf && ours.length == 1) => (candidate, x)
        case _ => null
      }
      innerSpec.foreach(ctx.leave)
      found
    }).filterNot(_ == null)
    spec.foreach(ctx.leave)
    if (results.length == 1) {
      x.disambiguation = Some(results.head._1)
      return results.head._2
    }
    Errors.missingField(x, base, x.field)
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
    // visit our base
    visit(ctx, x.base) match {
      case c : EirClassLike if c.templateArgs.length == x.args.length =>
        EirTemplatedType(x.parent, c, x.args.map(visit(ctx, _)))
      case _ => error(ctx, x, s"unsure how to specialize ${x.base}")
    }
  }

  override def visitProxyType(ctx: TypeCheckContext, x: types.EirProxyType): EirType = {
    val res = ProxyManager.proxyFor(x)
    visit(ctx, res.base)
    res
  }

  override def visitImport(ctx: TypeCheckContext, eirImport: EirImport): EirType = null

  override def visitFunctionCall(ctx: TypeCheckContext, call: EirFunctionCall): EirType = {
    val target = visit(ctx, call.target) match {
      case EirTemplatedType(_, _ : EirClassLike, _) | _: EirClassLike =>
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

  def argumentsMatch(x: List[EirResolvable[EirType]], y: List[EirResolvable[EirType]]): Boolean = {
    (x.length == y.length) &&
      x.zip(y).forall({ case (ours: EirType, theirs: EirType) => ours.canAssignTo(theirs) case _ => false })
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
      case _ =>
    }

    visit(ctx, loop.body)
  }

  override def visitLiteral(ctx: TypeCheckContext, value: EirLiteral): EirType = {
    globals.typeFor(value)
  }

  override def visitSymbol[A <: EirNamedNode](ctx: TypeCheckContext, value: EirSymbol[A]): EirType = {
    val ours = value.parent match {
      case Some(f : EirFunctionCall) if f.target == value => f.args.map(visit(ctx, _))
      case _ => Nil
    }
    val candidates = value.resolve()
    val found = candidates.find({
      case f: EirFunction => argumentsMatch(ours, f.functionArgs.map(visit(ctx, _)))
      case EirMember(_, f: EirFunction, _) => argumentsMatch(ours, f.functionArgs.map(visit(ctx, _)))
      case _ => ours.isEmpty
    })
    found.map(visit(ctx, _)).getOrElse(Errors.unableToResolve(value))
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

  override def visitDeclaration(ctx: TypeCheckContext, node: EirDeclaration): EirType = {
    val lval = visit(ctx, node.declaredType)
    val rval = node.initialValue.map(visit(ctx, _))
    if (!rval.forall(_.canAssignTo(lval))) {
      Errors.cannotCast(node, rval.get, lval)
    } else {
      lval
    }
  }

  override def visitTemplateArgument(ctx: TypeCheckContext, node: EirTemplateArgument): EirType = {
    ctx.hasSubstitution(node) match {
      case Some(x) => visit(ctx, x)
      case _ => error(ctx, node, s"missing substitution for $node")
    }
  }

  def error(ctx: TypeCheckContext, node: EirNode, message: String): EirType = {
    throw TypeCheckException(s"${node.location.map(_.toString).getOrElse(node.toString)}: $message")
  }

  // TODO need to check parent classes/traits too
  //      since they may not be reached otherwise
  def visitClassLike(ctx: TypeCheckContext, node: EirClassLike): EirType = {
    if (ctx.shouldCheck(node)) {
//      CheckClasses.visit(node)
      node.members.foreach(visitMember(ctx, _))
    }
    node
  }

  override def visitClass(ctx: TypeCheckContext, node: EirClass): EirType = visitClassLike(ctx, node)

  override def visitTrait(ctx: TypeCheckContext, node: EirTrait): EirType = visitClassLike(ctx, node)

  override def visitMember(ctx: TypeCheckContext, node: EirMember): EirType = visit(ctx, node.member)

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
    if (ctx.shouldCheck(node)) {
      val bodyType = node.body.map(visit(ctx, _))
      val retTy = visit(ctx, node.returnType)
      if (!bodyType.forall(_.canAssignTo(retTy))) {
        Errors.unableToUnify(node, bodyType.get, retTy)
      }
    }
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
    val func = globals.operatorToFunction(op).getOrElse(throw TypeCheckException(s"could not find member func for ${node.op}"))
    val f = EirFunctionCall(Some(node), null, List(node.rhs), Nil)
    f.target = EirFieldAccessor(Some(f), node.lhs, func)
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
        x.foundType = Option(result)
        if (x.foundType.isEmpty) error(ctx, node)
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
    val retTy = visit(ctx, node.body)
    if (retTy == null) throw TypeCheckException(s"could not find return type of $node")
    EirLambdaType(Some(node), node.args.map(visit(ctx, _)), retTy)
  }

  override def visitReturn(ctx: TypeCheckContext, node: EirReturn): EirType = {
    visit(ctx, node.expression)
  }

  final case class TypeCheckException(message: String) extends Exception(message)

  final case class MissingSpecializationException(message: String, node: EirSpecializable) extends Exception(message)

  override def visitSpecializedSymbol(ctx: TypeCheckContext, x: EirSpecializedSymbol): EirType = {
    val specializable = Find.uniqueResolution(x.symbol)
    specializable.asInstanceOf[Any] match {
      case c : EirClassLike if c.templateArgs.length == x.specialization.length => EirTemplatedType(Some(x), c, x.specialization)
      // TODO this only works because of a bug with Find.child wherein it ignores the requested type... womp womp
      case EirMember(_, f : EirFunction, _) if f.templateArgs.length == x.specialization.length =>
        val enter = ctx.specialize(f, x)
        val result = visitFunction(ctx, f)
        result.from = result.from.map(visit(ctx, _))
        result.to = visit(ctx, result.to)
        result.templateArgs = Nil
        ctx.leave(enter)
        result
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

  private def constructorDropCount(c : EirClassLike): Int = {
    c match {
      case p: EirProxy if p.collective.isEmpty && !p.isElement => 1
      case _ => 0
    }
  }

  override def visitNew(ctx: TypeCheckContext, x: EirNew): EirType = {
    val target = visit(ctx, x.target) match {
      case c : EirClassLike if !c.isAbstract => c
      case c => throw TypeCheckException(s"cannot instantiate $c")
    }
    val candidates =
      target.members.filter(m => m.isConstructor && x.canAccess(m))
    val ours = visit(ctx, x.args).toList
    x.disambiguation = candidates.find((m : EirMember) => {
      val f = m.member.asInstanceOf[EirFunction]
      val theirs =
        // proxy-related args are provided by the runtime
        f.functionArgs.drop(constructorDropCount(target)).map(visit(ctx, _))
      val matches = argumentsMatch(ours, theirs)
      matches
    })
    x.disambiguation match {
      case Some(_) => target
      case _ => error(ctx, x, "could not find a suitable constructor")
    }
  }

  override def visitProxy(ctx: TypeCheckContext, x: EirProxy): EirType = {
    visit(ctx, x.base)
    x
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
    x.ty match {
      case None => {
        x.ty = Some(goal)
      }
      case _ =>
    }
    null
  }

  override def visitExpressionPattern(ctx: TypeCheckContext, x: EirExpressionPattern): EirType = {
    val goal = ctx.goal.pop()
    val ours = visit(ctx, x.conditions.head)
    if (ours.canAssignTo(goal)) null
    else Errors.cannotCast(x, ours, goal)
  }
}
