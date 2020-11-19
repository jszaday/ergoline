package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.assertValid

object TypeCheck extends EirVisitor[EirType] {
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

  import TypeCheckSyntax.RichEirType;

  override def visitGlobalNamespace(): EirType = ???

  override def visitArrayReference(x: EirArrayReference): EirType = ???

  override def visitFieldAccessor(x: EirFieldAccessor): EirType = ???

  override def visitTernaryOperator(x: EirTernaryOperator): EirType = ???

  override def visitLambdaType(x: types.EirLambdaType): EirType = ???

  override def visitTemplatedType(x: types.EirTemplatedType): EirType = ???

  override def visitProxyType(x: types.EirProxyType): EirType = ???

  override def visitImport(eirImport: EirImport): EirType = null

  override def visitFunctionCall(call: EirFunctionCall): EirType = {
    // temporary
    // todo check arguments
    val target = visit(call.target)
    target
  }

  override def visitForLoop(loop: EirForLoop): EirType = ???

  override def visitLiteral(value: EirLiteral): EirType = {
    globals.typeOfLiteral(value)
  }

  override def visitSymbol(value: EirSymbol[_]): EirType = {
    visit(value.resolve().asInstanceOf[EirNode])
  }

  override def visitBlock(node: EirBlock): EirType = {
    visit(node.children)
    null
  }

  override def visitNamespace(node: EirNamespace): EirType = {
    visit(node.children)
    null
  }

  override def visitDeclaration(node: EirDeclaration): EirType = {
    val lval = visit(node.declaredType)
    val rval = node.initialValue.map(visit)
    if (!rval.forall(_.canAssignTo(lval))) {
      throw new RuntimeException(s"$rval cannot be assigned to $lval")
    }
    lval
  }

  override def visitTemplateArgument(node: EirTemplateArgument): EirType = ???

  override def visitClass(node: EirClass): EirType = {
    if (node.templateArgs.isEmpty) {
      visit(node.members)
      node
    } else {
      throw new RuntimeException("no can halp u")
    }
  }

  override def visitTrait(node: EirTrait): EirType = ???

  override def visitMember(node: EirMember): EirType = {
    // TODO fix things to avoid recurring into parent
    //      i.e. checking the members each time a class is reached
    //      (hint: a visited class cache may be helpful here...)
    // visit(node.member)
    null
  }

  override def visitFunction(node: EirFunction): EirType = {
    // TODO check self-assigning arguments?
    val retTy : EirType = visit(node.returnType)
    val found : Option[EirType] = node.body.flatMap(Find.returnType)
    found match {
      case None if !retTy.isUnitType => throw new RuntimeException(s"expected a return value of type $retTy for ${node.name}")
      case Some(other) if !other.canAssignTo(retTy) => throw new RuntimeException(s"${node.name} cannot return a value of type $other")
      case _ =>
    }
    node.body.foreach(visit)
    retTy
  }

  override def visitAnnotation(node: EirAnnotation): EirType = ???

  override def visitBinaryExpression(node: EirBinaryExpression): EirType = {
    val func = globals.operatorToFunction(node.op).getOrElse(throw new RuntimeException(s"could not find member func for ${node.op}"))
    val lhsType = visit(node.lhs)
    val rhsType = visit(node.rhs)
    val matchPool = assertValid[EirClassLike](lhsType)
      .members.filter(member => func == member.name)
//    println(matchPool)
    lhsType
  }

  override def visitFunctionArgument(node: EirFunctionArgument): EirType = ???

  override def visitAssignment(node: EirAssignment): EirType = ???

  override def visitTupleExpression(node: EirTupleExpression): EirType = ???

  override def visitLambdaExpression(node: EirLambdaExpression): EirType = ???

  override def visitReturn(node: EirReturn): EirType = ???
}
