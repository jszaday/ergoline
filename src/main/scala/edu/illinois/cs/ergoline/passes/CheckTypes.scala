package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirLambdaType, EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.CheckTypes.error
import edu.illinois.cs.ergoline.resolution.Find.{FindSyntax, singleReference, withName}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode

import scala.collection.mutable

class TypeCheckContext {
  private val stack : mutable.Stack[EirNode] = new mutable.Stack
  private val _substitutions : mutable.Stack[(EirSpecializable, EirSpecialization)] = new mutable.Stack
  def enterNode(n : EirNode): Unit = {
    stack.push(n)
    n match {
      case s : EirSpecializable =>
        val sp = specialization
        val (ourLength, theirLength) = (s.templateArgs.length, sp.map(_.specialization).getOrElse(Nil).length)
        if (theirLength != ourLength) {
          error(this, n, s"expected $ourLength template arguments, got $theirLength instead")
        }
        else if (ourLength > 0) emplaceSubstitution(s)
      case _ =>
    }
  }
  def emplaceSubstitution(s : EirSpecializable): Unit = {
    _substitutions.push((s, specialization.get))
  }
  def hasSubstitution(t : EirTemplateArgument): Option[EirResolvable[EirType]] = {
    _substitutions.flatMap({
      case (sable, stion) => sable.templateArgs.zip(stion.specialization)
    }).collectFirst({
      case (arg, ty) if arg == t => ty
    })
  }
  def leaveWith(t : EirType): EirType = {
    val leaving = stack.pop()
    if (_substitutions.headOption.exists({
      case (node, _) => leaving == node
    })) {
      _substitutions.pop()
    }
    t
  }

  def alreadyLeft(n : EirNode): Boolean = !stack.headOption.contains(n)

  def specialization: Option[EirSpecialization] = {
    stack.collectFirst {
      case x: EirSpecialization if !_substitutions.exists(y => {x.asInstanceOf[AnyRef] eq y._2}) && x.specialization.nonEmpty => x
    }
  }

  def cameVia[T: Manifest]: Option[T] = {
    Option.when(stack.length >= 2)(stack(1) match {
      case x : T => Some(x)
      case _ => None
    }).flatten
  }

//  def cameVia[T: Manifest]: Boolean = {
//    Option.when(stack.length >= 2)(stack(1)) match {
//      case Some(x : T) => true
//      case _ => false
//    }
//  }
}

object CheckTypes extends EirVisitor[TypeCheckContext, EirType] {

  def error(ctx: TypeCheckContext, node: EirNode, message : String): EirType = {
    throw TypeCheckException(s"error in $ctx on $node: $message")
  }

  def visit(node: EirNode): EirType = visit(new TypeCheckContext, node)

  object TypeCheckSyntax {
    implicit class RichEirType(t: EirType) {
      def canAssignTo(other : EirType): Boolean = {
        t == other
      }

      def isUnitType: Boolean = {
        t match {
          case n : EirNamedNode => n.name == "unit"
          case _ => false
        }
      }
    }
  }

  final case class TypeCheckException(message: String) extends Exception(message)
  final case class MissingSpecializationException(message: String, node : EirClassLike) extends Exception(message)

  import TypeCheckSyntax.RichEirType

  override def visit(ctx: TypeCheckContext, node: EirNode): EirType = {
    ctx.enterNode(node)
    val result = super.visit(ctx, node)
    node match {
      case x : EirExpressionNode =>
        x.foundType = Option(result)
        if (x.foundType.isEmpty) error(ctx, node)
      case _ =>
    }
    if (ctx.alreadyLeft(node)) result
    else ctx.leaveWith(result)
  }

  override def visitArrayReference(ctx: TypeCheckContext, x: EirArrayReference): EirType = ???

  def autoApply(ctx: TypeCheckContext, target : EirExpressionNode, args : List[EirResolvable[EirType]]): Boolean = {
    (args.length == 1) && visit(ctx, target).canAssignTo(visit(ctx, args.head))
  }

  import FindSyntax.RichPredicate

  def argumentsMatch(x : List[EirResolvable[EirType]], y : List[EirResolvable[EirType]]): Boolean = {
    (x.length == y.length) &&
      x.zip(y).forall({ case (ours : EirType, theirs : EirType) => ours.canAssignTo(theirs) case _ => false })
  }

  override def visitFieldAccessor(ctx: TypeCheckContext, x: EirFieldAccessor): EirType = {
    // visit the target to find its class
    val (sp, cls) = super.visit(ctx, x.target) match {
      case c : EirClassLike => (None, c)
      case t : EirTemplatedType => (Some(t), t.base.asInstanceOf[EirClassLike])
      case x => throw TypeCheckException(s"$x is not a class-like object with members")
    }
    // TODO detection of cameViaFC and expectsSelf not at 100% yet, need to work on these
    //      and implement self-application as well (i.e. (42).toString vs 42.toString())
    val expectsSelf = x.target match {
      // we do not expect ourself for class applications and that's it :)
      case s : EirSymbol[_] => !singleReference(s).get.isInstanceOf[EirClassLike]
      case _ => true
    }
    val prevFc : Option[EirFunctionCall] = ctx.cameVia[EirFunctionCall]
    val cameViaFuncCall = prevFc.isDefined && !prevFc.exists(_.args.contains(x))
    val ours = {
      Option.when(expectsSelf)(sp.getOrElse(cls)) ++
        (if (cameViaFuncCall) prevFc.get.args.map(visit) else Nil)
    }.toList
    // enter the found class
    sp.foreach(ctx.enterNode)
    ctx.enterNode(cls)
    // find the candidates ^_^
    // TODO check parent classes as well!
    val candidates = Find.child[EirMember](cls, withName(x.field).and(x.canAccess(_))).toList
    val results = candidates.map(candidate => {
      val resolved = visit(ctx, candidate)
      resolved match {
        case t@EirLambdaType(_, theirs, retTy) if argumentsMatch(ours, theirs.map(visit(ctx, _))) => {
          val args = if (cameViaFuncCall && expectsSelf) theirs.tail else theirs
          (candidate, EirLambdaType(t.parent, args, retTy))
        }
        case x if ours.isEmpty => (candidate, x)
        case _ => null
      }
    }).filterNot(_ == null)
    // and pop the substitution :3
    // find the first matching value
    if (results.length == 1) {
      val (chosen, retTy) = results.head
      x.disambiguation = Some(chosen)
      sp.foreach(_ => ctx.leaveWith(null))
      ctx.leaveWith(retTy)
    }
    else error(ctx, x, s"unable to uniquely resolve, found ${results.length} out of ${candidates.length} candidates")
  }

  override def visitTernaryOperator(ctx: TypeCheckContext, x: EirTernaryOperator): EirType = ???

  override def visitLambdaType(ctx: TypeCheckContext, x: types.EirLambdaType): EirType = {
    x.from = x.from.map(visit(ctx, _))
    x.to = visit(ctx, x.to)
    x
  }

  override def visitTemplatedType(ctx: TypeCheckContext, x: types.EirTemplatedType): EirType = {
    val base = visit(ctx, x.base)
    // the specialization only applies to the base, so now we leave
    ctx.leaveWith(null)
    // then everything else is resolved "above" us
    EirTemplatedType(x.parent, base, x.args.map(visit(ctx, _)))
  }

  override def visitProxyType(ctx: TypeCheckContext, x: types.EirProxyType): EirType = ???

  override def visitImport(ctx: TypeCheckContext, eirImport: EirImport): EirType = null


  override def visitFunctionCall(ctx: TypeCheckContext, call: EirFunctionCall): EirType = {
    val target = visit(ctx, call.target) match {
      case _ : EirClassLike =>
        val accessor = EirFieldAccessor(call.target.parent, call.target, "apply")
        // call.target.disambiguation = Some(accessor)
        visit(ctx, accessor)
      case ty => ty
    }
    // a function call's specialization only applies to its target, so we leave midway through
    ctx.leaveWith(null)
    val ours = call.args.map(visit(ctx, _))
    // everything else should be resolved already, or resolved "above" the specialization
    target match {
      case EirLambdaType(_, args, retTy) if argumentsMatch(args, ours) => {
        retTy match {
          case t : EirType => t
          case _ => visit(ctx, retTy)
        }
      }
      case _ => error(ctx, call, s"cannot resolve ((${ours mkString ", "}) => ???) and $target")
    }
  }

  override def visitForLoop(ctx: TypeCheckContext, loop: EirForLoop): EirType = ???

  override def visitLiteral(ctx: TypeCheckContext, value: EirLiteral): EirType = {
    globals.typeOfLiteral(value)
  }

  override def visitSymbol[A <: EirNamedNode](ctx: TypeCheckContext, value: EirSymbol[A]): EirType = {
    if (value.disambiguation.isEmpty) {
      value.disambiguation = Find.singleReference(value)
    }
    value.disambiguation match {
      case Some(node) => visit(ctx, node)
      case None => throw TypeCheckException(s"could not find type of $value")
    }
  }

  override def visitBlock(ctx: TypeCheckContext, node: EirBlock): EirType = {
    node.children.foreach(visit(ctx, _))
    val retTys : List[EirType] = Find.descendant(node, {
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

  // TODO this should consider the current substitutions, and check each unique substitution!
  var classCache: List[EirClass] = Nil
  override def visitClass(ctx: TypeCheckContext, node: EirClass): EirType = {
    if (!classCache.contains(node)) {
      classCache +:= node
      visit(ctx, node.members)
    }
    node
  }

  override def visitTrait(ctx: TypeCheckContext, node: EirTrait): EirType = ???

  override def visitMember(ctx: TypeCheckContext, node: EirMember): EirType = visit(ctx, node.member)

  def annotationsOf(node : EirFunction): List[EirAnnotation] = {
    if (node.parent.exists(_.isInstanceOf[EirMember])) {
      node.parent.get.annotations
    } else {
      node.annotations
    }
  }

  override def visitFunction(ctx: TypeCheckContext, node: EirFunction): EirType = {
    // TODO check self-assigning arguments?
    val args = node.functionArgs.map(_.declaredType).map(visit(ctx, _))
    val retTy : EirType = visit(ctx, node.returnType)
    val found : Option[EirType] = node.body.flatMap(body => Option(visit(ctx, body)))
    // TODO or member of abstract class (to be added
    val noReturnOk = retTy.isUnitType ||
      (node.body.isEmpty && annotationsOf(node).map(_.name).contains("system"))
    found match {
      case None if !noReturnOk =>
        throw TypeCheckException(s"expected a return value of type $retTy for ${node.name}")
      case Some(other) if !other.canAssignTo(retTy) =>
        throw TypeCheckException(s"${node.name} cannot return a value of type $other")
      case _ =>
    }
    node.body.foreach(visit(ctx, _))
    EirLambdaType(Some(node), args, retTy)
  }

  override def visitAnnotation(ctx: TypeCheckContext, node: EirAnnotation): EirType = ???

  override def visitBinaryExpression(ctx: TypeCheckContext, node: EirBinaryExpression): EirType = {
    val func = globals.operatorToFunction(node.op).getOrElse(throw TypeCheckException(s"could not find member func for ${node.op}"))
    val f = EirFunctionCall(Some(node), null, List(node.rhs), Nil)
    f.target = EirFieldAccessor(Some(f), node.lhs, func)
    node.disambiguation = Some(f)
    visit(ctx, f)
  }

  override def visitFunctionArgument(ctx: TypeCheckContext, node: EirFunctionArgument): EirType = {
    visit(ctx, node.declaredType)
  }

  override def visitAssignment(ctx: TypeCheckContext, node: EirAssignment): EirType = ???

  override def visitTupleExpression(ctx: TypeCheckContext, node: EirTupleExpression): EirType = ???

  override def visitLambdaExpression(ctx: TypeCheckContext, node: EirLambdaExpression): EirType = {
    val retTy = visit(ctx, node.body)
    if (retTy == null) throw TypeCheckException(s"could not find return type of $node")
    EirLambdaType(Some(node), node.args.map(visit(ctx, _)), retTy)
  }

  override def visitReturn(ctx: TypeCheckContext, node: EirReturn): EirType = {
    visit(ctx, node.expression)
  }
}
