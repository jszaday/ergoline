package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirLambdaType, EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirType

object CheckTypes extends EirVisitor[TypeCheckContext, EirType] {

  // TODO this should consider the current substitutions, and check each unique substitution!
  var classCache: List[EirClass] = Nil

  override def visitArrayReference(ctx: TypeCheckContext, x: EirArrayReference): EirType = ???

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
    val ours = {
      Option.when(expectsSelf)(base) ++
        (if (cameViaFuncCall) prevFc.get.args.map(visit(ctx, _)) else Nil)
    }.toList
    // find the candidates ^_^
    val candidates = Find.accessibleMember(base, x)
    val results = candidates.map(candidate => {
      val member = visit(ctx, candidate)
      val innerSpec = handleSpecialization(ctx, member)
      val found = member match {
        case t : EirLambdaType =>
          val theirs = t.from.map(visit(ctx, _))
          if (argumentsMatch(ours, theirs)) {
            val args = if (cameViaFuncCall && expectsSelf) theirs.tail else theirs
            (candidate, EirLambdaType(t.parent, args, visit(ctx, t.to)))
          } else {
            null
          }
        case x if ours.isEmpty => (candidate, x)
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
    error(ctx, x, s"attempting to access field ${x.field} of $base")
  }

  override def visitTernaryOperator(ctx: TypeCheckContext, x: EirTernaryOperator): EirType = {
    if (visit(ctx, x.test).canAssignTo(globals.typeFor(EirLiteralTypes.Boolean))) {
      Find.unionType(visit(ctx, x.ifTrue), visit(ctx, x.ifFalse)) match {
        case Some(found) => found
        case None => error(ctx, x, s"could not unify type of ${x.ifTrue} and ${x.ifFalse}")
      }
    } else {
      error(ctx, x, s"${x.test} is an unsuitable test")
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

  override def visitProxyType(ctx: TypeCheckContext, x: types.EirProxyType): EirType = x.resolve().head

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
        if (!ttype.exists(_.canAssignTo(globals.typeFor(EirLiteralTypes.Boolean)))) {
          error(ctx, loop, s"expected a boolean-like value, instead got $ttype")
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
    visit(ctx, Find.uniqueResolution(value))
  }

  override def visitBlock(ctx: TypeCheckContext, node: EirBlock): EirType = {
    node.children.foreach(visit(ctx, _))
    val retTys: List[EirType] = Find.descendant(node, {
      case _: EirLambdaExpression => None
      case _: EirFunction => None
      case _: EirReturn => Some(true)
      case _ => Some(false)
    }).map(visit(ctx, _)).toList
    if (retTys.isEmpty) null
    else Find.unionType(retTys) match {
      case Some(x) => x
      case None => throw TypeCheckException(s"could not find union of return types $retTys")
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
      throw TypeCheckException(s"$rval cannot be assigned to $lval")
    }
    lval
  }

  override def visitTemplateArgument(ctx: TypeCheckContext, node: EirTemplateArgument): EirType = {
    ctx.hasSubstitution(node) match {
      case Some(x) => visit(ctx, x)
      case _ => error(ctx, node, s"missing substitution for $node")
    }
  }

  def error(ctx: TypeCheckContext, node: EirNode, message: String): EirType = {
    throw TypeCheckException(s"error in $ctx on $node: $message")
  }


  def visitClassLike(ctx: TypeCheckContext, node: EirClassLike): EirType = {
    if (ctx.shouldCheck(node)) {
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
      node.body.foreach(visit(ctx, _))
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
    val func = globals.operatorToFunction(node.op).getOrElse(throw TypeCheckException(s"could not find member func for ${node.op}"))
    val f = EirFunctionCall(Some(node), null, List(node.rhs), Nil)
    f.target = EirFieldAccessor(Some(f), node.lhs, func)
    node.disambiguation = Some(f)
    val retTy = visit(ctx, f)
    if (func == "compareTo") {
      if (retTy.canAssignTo(globals.typeFor(EirLiteralTypes.Integer))) {
        globals.typeFor(EirLiteralTypes.Boolean)
      } else {
        error(ctx, node, s"expected $retTy to compatible with integer")
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
      case s : EirSymbol[_] => Find.singleReference(s) match {
        case Some(d : EirDeclaration) => d.isFinal
        case Some(a : EirFunctionArgument) => a.isFinal
        case _ => false
      }
      case _ => ???
    }
  }

  override def visitAssignment(ctx: TypeCheckContext, node: EirAssignment): EirType = {
    val lval = visit(ctx, node.rval)
    val rval = visit(ctx, node.lval)
    if (!rval.canAssignTo(lval)) {
      error(ctx, node, s"$rval cannot be assigned to $lval")
    } else if (isFinalDecl(node.lval)) {
      error(ctx, node, s"$rval cannot be assigned to final ${node.lval}")
    } else {
      null
    }
  }

  override def visitTupleExpression(ctx: TypeCheckContext, node: EirTupleExpression): EirType = ???

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
    if (!retTy.canAssignTo(globals.typeFor(EirLiteralTypes.Boolean))) {
      error(ctx, x.test, s"expected $x to be a boolean")
    }
    x.ifTrue.foreach(visit(ctx, _))
    x.ifFalse.foreach(visit(ctx, _))
    null
  }

  override def visitNew(ctx: TypeCheckContext, x: EirNew): EirType = {
    val target = visit(ctx, x.target) match {
      case c : EirClassLike if !c.isAbstract => c
      case c => throw TypeCheckException(s"cannot instantiate $c")
    }
    val candidates =
      target.members
        .filter(m => m.isConstructor && x.canAccess(m))
        .map(_.member.asInstanceOf[EirFunction].functionArgs.tail)
        // proxy-related args are provided by the runtime
        .map(m => if (target.isInstanceOf[EirProxy]) m.tail else m)
        .map(visit(ctx, _).toList)
    val ours = visit(ctx, x.args).toList
    if (candidates.exists(argumentsMatch(ours, _))) target
    else error(ctx, x, "could not find a suitable constructor")
  }

  override def visitProxy(ctx: TypeCheckContext, x: EirProxy): EirType = x
}
