package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import util.Properties.{lineSeparator => n}


object UnparseAst extends EirVisitor[String] {
  var numTabs = 0

  def t: String = ""

  def tabs: String = List.fill(3)(t).reduce(_ + _)

  def visitStatements(lst : Iterable[EirNode]): String =
    visit(lst).map(x => s"$tabs$x$n").mkString

  override def visitBlock(node: EirBlock): String = {
    numTabs += 1
    val body = visitStatements(node.children) match {
      case "" => " "
      case x => s"$n$x$n$tabs"
    }
    numTabs -= 1
    s"{$body}"
  }

  override def visitNamespace(node: EirNamespace): String = {
    numTabs += 1
    val body = visitStatements(node.children)
    numTabs -= 1
    s"namespace ${node.name} {$n$body$n}"
  }

  override def visitDeclaration(node: EirDeclaration): String = {
    val kwd = if (node.isFinal) "val" else "var"
    val declType = node.declaredType.represents.map(visit).getOrElse("")
    val expr = node.initialValue.map(x => s"= ${visit(x)}").getOrElse("")
    s"$kwd ${node.name}: $declType $expr"
  }

  override def visitTemplateArgument(node: EirTemplateArgument): String = ???

  override def visitClass(node: EirClass): String = ???

  override def visitTrait(node: EirTrait): String = ???

  override def visitMember(node: EirMember): String = ???

  override def visitFunction(node: EirFunction): String = {
    val args = node.functionArgs.map(visit) mkString ", "
    val templates = if (node.templateArgs.isEmpty) "" else ("<" + node.templateArgs.map(visit).mkString(", ") + ">")
    val retType = node.returnType.represents.map(visit).getOrElse("")
    s"func ${node.name}$templates($args): $retType " + node.body.map(visit).getOrElse("")
  }

  override def visitAnnotation(node: EirAnnotation): String = ???

  override def visitBinaryExpression(node: EirBinaryExpression): String = ???

  override def visitFunctionArgument(node: EirFunctionArgument): String = {
    val declTy = node.declaredType.represents.map(visit).getOrElse("")
    val equals = if (node.isSelfAssigning) "=" else ""
    s"${node.name}$equals: $declTy"
  }

  override def visitAssignment(node: EirAssignment): String = ???

  override def visitTupleExpression(node: EirTupleExpression): String = ???

  override def visitLambdaExpression(node: EirLambdaExpression): String = ???

  override def visitReturn(node: EirReturn): String = ???

  override def visitSymbol(value: EirSymbol[_]): String = value.qualifiedName mkString "::"
}
