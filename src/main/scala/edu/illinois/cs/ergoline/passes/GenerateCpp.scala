package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._

import scala.util.Properties.{lineSeparator => n}
import UnparseAst.t
import edu.illinois.cs.ergoline.resolution.Find

object GenerateCpp extends EirVisitor[String] {
  var numTabs = 0
  var visited : List[EirNode] = Nil

  override def error(node : EirNode): String = {
    println(s"silently ignoring my failure to process $node")
    ""
  }

  def tabs: String = List.fill(numTabs)(t).mkString("")

  override def visitArrayReference(x: EirArrayReference): String = ???

  override def visitFieldAccessor(x: EirFieldAccessor): String = ???

  override def visitTernaryOperator(x: EirTernaryOperator): String = ???

  override def visitLambdaType(x: types.EirLambdaType): String = "auto"

  override def visitTemplatedType(x: types.EirTemplatedType): String = ???

  override def visitProxyType(x: types.EirProxyType): String = ???

  override def visitImport(x: EirImport): String = ""

  def handleOption(x : Option[EirNode]): Option[String] = {
    x.map({
      case n : EirNamedNode => generateName(n)
      case n => visit(n)
    })
  }

  override def visitFunctionCall(x: EirFunctionCall): String = {
    val target = handleOption(x.found).getOrElse(visit(x.target))
    s"($target(${x.args.map(visit) mkString ", "}))"
  }

  override def visitForLoop(x: EirForLoop): String = ???

  override def visitLiteral(x: EirLiteral): String = x.value

  override def visitSymbol[A <: EirNamedNode](x: EirSymbol[A]): String = {
    generateName(Find.singleReference(x).get)
  }

  override def visitBlock(x: EirBlock): String = {
    s"{$n" + visitChildren(x.children) + s"$n$tabs}"
  }

  def visitChildren(x : List[EirNode]): String = {
    numTabs += 1
    val str = visit(x).map(x => s"$tabs$x").mkString(n)
    numTabs -= 1
    str
  }

  override def visitNamespace(x: EirNamespace): String = {
    s"namespace ${generateName(x)} {$n" + visitChildren(x.children) + s"$n$tabs};$n"
  }

  override def visitDeclaration(x: EirDeclaration): String = {
    s"${visit(x.declaredType)} ${generateName(x)}" + {
      x.initialValue.map(x => s" = ${visit(x)}").getOrElse("")
    } + ";"
  }

  override def visitTemplateArgument(x: EirTemplateArgument): String = {
    s"typename ${generateName(x)}"
  }

  override def visitClass(x: EirClass): String = {
    visitTemplateArgs(x.templateArgs) + s"class ${generateName(x)} {$n" + visitChildren(x.children) + s"$n$tabs};$n"
  }

  override def visitTrait(x: EirTrait): String = ???

  override def visitMember(x: EirMember): String = {
    visit(x.member)
  }

  def visitTemplateArgs(args : List[EirTemplateArgument]): String = {
    if (args.isEmpty) ""
    else s"template<${args.map(visit) mkString ", "}>$n"
  }

  def dropSelf(x : EirFunction): List[EirFunctionArgument] = {
    x.parent match {
      case Some(_ : EirMember) if x.functionArgs.head.name == "self" =>  x.functionArgs.tail
      case _ => x.functionArgs
    }
  }

  override def visitFunction(x: EirFunction): String = {
    if (visited.contains(x)) return ""
    visited +:= x
    val body = x.body.map(visit).getOrElse(";")
    val args = dropSelf(x).map(visit)
    visitTemplateArgs(x.templateArgs) +
    s"${visit(x.returnType)} ${generateName(x)}(${args mkString ", "}) $body"
  }

  override def visitAnnotation(x: EirAnnotation): String = s"/* @${x.name} */ "

  override def visitBinaryExpression(x: EirBinaryExpression): String = {
    s"(${visit(x.lhs)} ${x.op} ${visit(x.rhs)})"
  }

  def generateName(x : EirNamedNode): String = {
    if (x.name == "self") "this" else x.name
  }

  override def visitFunctionArgument(x: EirFunctionArgument): String = {
    s"${visit(x.declaredType)} ${generateName(x)}"
  }

  override def visitAssignment(x: EirAssignment): String = {
    s"${x.lval} = ${x.rval};"
  }

  override def visitTupleExpression(x: EirTupleExpression): String = {
    val func = x.parent match {
      case Some(a : EirAssignment) if a.lval == x => "tie"
      case _ => "make_tuple"
    }
    s"std::$func(${visit(x) mkString ", "})"
  }

  override def visitLambdaExpression(x: EirLambdaExpression): String = {
    val found = handleOption(x.found).getOrElse("")
    s"[=] (${visit(x.args) mkString ", "}) -> $found ${visit(x.body)}"
  }

  override def visitReturn(x: EirReturn): String = {
    s"return ${visit(x.expression)};"
  }
}
