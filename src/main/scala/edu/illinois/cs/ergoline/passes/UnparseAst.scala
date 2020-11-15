package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.EirResolvable

import util.Properties.{lineSeparator => n}


object UnparseAst extends EirVisitor[String] {
  var numTabs = 0

  private object UnparseSyntax {
    implicit class RichOption[T](option : Option[T]) {
      def mapOrEmpty(f : T => String): String = option.map(f).getOrElse("")
    }
  }

  import UnparseSyntax.RichOption

  def t: String = "  "

  def tabs: String = List.fill(numTabs)(t).reduce(_ + _)

  def visitResolvable[T](resolvable: EirResolvable[T]): String =
    resolvable.represents.mapOrEmpty(visit)

  def visitStatements(lst : Iterable[EirNode]): String = {
    val x = visit(lst).map(x => s"$n$tabs$x").mkString
    if (x == "") " " else s"$x$n"
  }

  override def visitBlock(node: EirBlock): String = {
    numTabs += 1
    val body = visitStatements(node.children)
    val tail = if (body.strip.isEmpty) "" else t
    numTabs -= 1
    s"{$body$tail}"
  }

  override def visitNamespace(node: EirNamespace): String = {
    numTabs += 1
    val body = visitStatements(node.children)
    numTabs -= 1
    s"namespace ${node.name} {$body}"
  }

  override def visitDeclaration(node: EirDeclaration): String = {
    val kwd = if (node.isFinal) "val" else "var"
    val declType = visitResolvable(node.declaredType)
    val expr = node.initialValue.mapOrEmpty(x => s"= ${visit(x)}")
    s"$kwd ${node.name}: $declType $expr;"
  }

  override def visitTemplateArgument(node: EirTemplateArgument): String = {
    node.name +
      node.upperBound.mapOrEmpty(x => " <: " + visitResolvable(x)) +
      node.lowerBound.mapOrEmpty(x => " :> " + visitResolvable(x))
  }

  override def visitClass(node: EirClass): String = ???

  override def visitTrait(node: EirTrait): String = ???

  override def visitMember(node: EirMember): String = ???

  override def visitFunction(node: EirFunction): String = {
    val args = node.functionArgs.map(visit) mkString ", "
    val templates = if (node.templateArgs.isEmpty) "" else ("<" + node.templateArgs.map(visit).mkString(", ") + ">")
    val retType = node.returnType.represents.map(visit).getOrElse("")
    s"func ${node.name}$templates($args): $retType " + node.body.mapOrEmpty(visit)
  }

  override def visitAnnotation(node: EirAnnotation): String = ???

  override def visitBinaryExpression(node: EirBinaryExpression): String =
    s"(${visit(node.lhs)} ${node.op} ${visit(node.rhs)})"

  override def visitFunctionArgument(node: EirFunctionArgument): String = {
    val declTy = visitResolvable(node.declaredType)
    val equals = if (node.isSelfAssigning) "=" else ""
    s"${node.name}$equals: $declTy"
  }

  override def visitAssignment(node: EirAssignment): String = ???

  override def visitTupleExpression(node: EirTupleExpression): String =
    s"(${node.expressions.map(visit) mkString ", "})"

  override def visitLambdaExpression(node: EirLambdaExpression): String = ???

  override def visitReturn(node: EirReturn): String = ???

  override def visitSymbol(value: EirSymbol[_]): String = value.qualifiedName mkString "::"

  override def visitLiteral(value: EirLiteral): String = value.value
}
