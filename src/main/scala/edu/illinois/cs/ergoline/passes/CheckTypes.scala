package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.types.{EirLambdaType, EirType}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode
import edu.illinois.cs.ergoline.util.assertValid

object CheckTypes extends EirVisitor[EirType] {

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

  import TypeCheckSyntax.RichEirType

  override def visitArrayReference(x: EirArrayReference): EirType = ???

  def findCandidates(x: EirFieldAccessor): Iterable[EirMember] = {
    val lhs = visit(x.target)
    if (!lhs.isInstanceOf[EirClassLike]) {
      throw TypeCheckException(s"expected $lhs to be a class-like type in $x")
    }
    lhs.findChild[EirMember](withName(x.field))
  }

  def autoApply(target : EirExpressionNode, args : List[EirResolvable[EirType]]): Boolean = {
    (args.length == 1) && visit(target).canAssignTo(visit(args.head))
  }

  override def visitFieldAccessor(x: EirFieldAccessor): EirType = {
    val candidates = findCandidates(x).map(visit(_)).toList
    for (candidate <- candidates) {
      candidate match {
        case EirLambdaType(_, args, retTy) =>
          if (autoApply(x.target, args)) return visit(retTy)
        case _ => return visit(candidate)
      }
    }
    throw TypeCheckException(s"unable to find unique candidate for $x")
  }

  override def visitTernaryOperator(x: EirTernaryOperator): EirType = ???

  override def visitLambdaType(x: types.EirLambdaType): EirType = {
    x.from = x.from.map(visit)
    x.to = visit(x.to)
    x
  }

  override def visitTemplatedType(x: types.EirTemplatedType): EirType = ???

  override def visitProxyType(x: types.EirProxyType): EirType = ???

  override def visitImport(eirImport: EirImport): EirType = null

  override def visitFunctionCall(call: EirFunctionCall): EirType = {
    var args = call.args.map(visit(_))
    val candidates = call.target match {
      case x : EirFieldAccessor =>
        args = visit(x.target) +: args
        findCandidates(x)
      case x : EirSymbol[_] => x.candidates
      case x => Seq(x)
    }
    // TODO save any found candidates for code generation phase
    for (candidate <- candidates) {
      visit(candidate) match {
        case EirLambdaType(_, theirArgs, retTy) =>
          if (theirArgs.length == args.length) {
            if (theirArgs.zip(args).forall({
              case (theirs, ours) => ours.canAssignTo(visit(theirs))
            })) {
              call.found = Some(candidate)
              return visit(retTy)
            }
          }
        case x => throw TypeCheckException(s"unsure how to apply $x")
      }
    }
    throw TypeCheckException(s"could not find suitable candidate for call $call")
  }

  override def visitForLoop(loop: EirForLoop): EirType = ???

  override def visitLiteral(value: EirLiteral): EirType = {
    globals.typeOfLiteral(value)
  }

  override def visitSymbol(value: EirSymbol[_]): EirType = {
    visit(value.resolve().asInstanceOf[EirNode])
  }

  override def visitBlock(node: EirBlock): EirType = {
    node.children.foreach(visit)
    val retTys : List[EirType] = Find.descendant(node, {
      case _: EirLambdaExpression => None
      case _: EirFunction => None
      case _: EirReturn => Some(true)
      case _ => Some(false)
    }).map(visit).toList
    if (retTys.isEmpty) null
    else Find.unionType(retTys) match {
      case Some(x) => x
      case None => throw TypeCheckException(s"could not find union of return types $retTys")
    }
  }

  override def visitNamespace(node: EirNamespace): EirType = {
    visit(node.children)
    null
  }

  override def visitDeclaration(node: EirDeclaration): EirType = {
    val lval = visit(node.declaredType)
    val rval = node.initialValue.map(visit)
    if (!rval.forall(_.canAssignTo(lval))) {
      throw TypeCheckException(s"$rval cannot be assigned to $lval")
    }
    lval
  }

  override def visitTemplateArgument(node: EirTemplateArgument): EirType = ???

  var classCache: List[EirClass] = Nil

  override def visitClass(node: EirClass): EirType = {
    if (node.templateArgs.isEmpty) {
      if (!classCache.contains(node)) {
        classCache +:= node
        visit(node.members)
      }
      node
    } else {
      throw TypeCheckException("no can halp u")
    }
  }

  override def visitTrait(node: EirTrait): EirType = ???

  override def visitMember(node: EirMember): EirType = visit(node.member)

  def annotationsOf(node : EirFunction): List[EirAnnotation] = {
    if (node.parent.exists(_.isInstanceOf[EirMember])) {
      node.parent.get.annotations
    } else {
      node.annotations
    }
  }

  override def visitFunction(node: EirFunction): EirType = {
    // TODO check self-assigning arguments?
    val retTy : EirType = visit(node.returnType)
    val found : Option[EirType] = node.body.flatMap(body => Option(visit(body)))
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
    node.body.foreach(visit)
    EirLambdaType(Some(node), node.functionArgs.map(_.declaredType), retTy)
  }

  override def visitAnnotation(node: EirAnnotation): EirType = ???

  override def visitBinaryExpression(node: EirBinaryExpression): EirType = {
    val func = globals.operatorToFunction(node.op).getOrElse(throw TypeCheckException(s"could not find member func for ${node.op}"))
    val f = EirFunctionCall(Some(node), null, List(node.rhs))
    f.target = EirFieldAccessor(Some(f), node.lhs, func)
    visit(f)
  }

  override def visitFunctionArgument(node: EirFunctionArgument): EirType = {
    visit(node.declaredType)
  }

  override def visitAssignment(node: EirAssignment): EirType = ???

  override def visitTupleExpression(node: EirTupleExpression): EirType = ???

  override def visitLambdaExpression(node: EirLambdaExpression): EirType = {
    val retTy = visit(node.body)
    if (retTy == null) throw TypeCheckException(s"could not find return type of $node")
    node.found = Some(retTy)
    EirLambdaType(Some(node), node.args.map(visit), retTy)
  }

  override def visitReturn(node: EirReturn): EirType = {
    visit(node.expression)
  }
}
