package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._

import scala.util.Properties.{lineSeparator => n}
import UnparseAst.t
import edu.illinois.cs.ergoline.resolution.Find


class CppContext { }

object GenerateCpp extends EirVisitor[CppContext, String] {
  var numTabs = 0
  var visited : List[EirNode] = Nil

  def visit(node: EirNode): String = visit(new CppContext, node)

  override def error(ctx: CppContext, node : EirNode): String = {
    println(s"silently ignoring my failure to process $node")
    ""
  }

  // TODO should use same character as other unparse passes
  def tabs: String = List.fill(numTabs)(t).mkString("")

  override def visitArrayReference(ctx: CppContext, x: EirArrayReference): String = ???

  override def visitFieldAccessor(ctx: CppContext, x: EirFieldAccessor): String = {
    // TODO handle self applications :3
    s"${visit(ctx, x.target)}.${x.field}"
  }

  override def visitTernaryOperator(ctx: CppContext, x: EirTernaryOperator): String = ???

  override def visitLambdaType(ctx: CppContext, x: types.EirLambdaType): String = "auto"

  override def visitTemplatedType(ctx: CppContext, x: types.EirTemplatedType): String = {
    s"${generateName(x.base)}<${x.args.map(generateName) mkString ", "}>"
  }

  override def visitProxyType(ctx: CppContext, x: types.EirProxyType): String = ???

  override def visitImport(ctx: CppContext, x: EirImport): String = ""

  def handleOption(ctx: CppContext, x : Option[EirNode]): Option[String] = {
    x.map({
      case n : EirNamedNode => generateName(n)
      case n => visit(ctx, n)
    })
  }

  override def visitFunctionCall(ctx: CppContext, x: EirFunctionCall): String = {
    val target = handleOption(ctx, x.target.disambiguation).getOrElse(visit(ctx, x.target))
    s"($target(${x.args.map(visit(ctx, _)) mkString ", "}))"
  }

  override def visitForLoop(ctx: CppContext, x: EirForLoop): String = ???

  override def visitLiteral(ctx: CppContext, x: EirLiteral): String = x.value

  override def visitSymbol[A <: EirNamedNode](ctx: CppContext, x: EirSymbol[A]): String = {
    generateName(Find.singleReference(x).get)
  }

  override def visitBlock(ctx: CppContext, x: EirBlock): String = {
    s"{$n" + visitChildren(ctx, x.children) + s"$n$tabs}"
  }

  def visitChildren(ctx: CppContext, x : List[EirNode]): String = {
    numTabs += 1
    val str = visit(ctx, x).map(x => s"$tabs$x").mkString(n)
    numTabs -= 1
    str
  }

  override def visitNamespace(ctx: CppContext, x: EirNamespace): String = {
    s"namespace ${generateName(x)} {$n" + visitChildren(ctx, x.children) + s"$n$tabs};$n"
  }

  override def visitDeclaration(ctx: CppContext, x: EirDeclaration): String = {
    s"${visit(ctx, x.declaredType)} ${generateName(x)}" + {
      x.initialValue.map(x => s" = ${visit(ctx, x)}").getOrElse("")
    } + ";"
  }

  override def visitTemplateArgument(ctx: CppContext, x: EirTemplateArgument): String = {
    s"typename ${generateName(x)}"
  }

  override def visitClass(ctx: CppContext, x: EirClass): String = {
    visitTemplateArgs(ctx, x.templateArgs) + s"class ${generateName(x)} {$n" + visitChildren(ctx, x.children) + s"$n$tabs};$n"
  }

  override def visitTrait(ctx: CppContext, x: EirTrait): String = ???

  override def visitMember(ctx: CppContext, x: EirMember): String = {
    visit(ctx, x.member)
  }

  def visitTemplateArgs(ctx: CppContext, args : List[EirTemplateArgument]): String = {
    if (args.isEmpty) ""
    else s"template<${args.map(visit(ctx, _)) mkString ", "}>$n"
  }

  def dropSelf(x : EirFunction): List[EirFunctionArgument] = {
    if (x.functionArgs.isEmpty) x.functionArgs
    else x.parent match {
      case Some(_ : EirMember) if x.functionArgs.head.name == "self" =>  x.functionArgs.tail
      case _ => x.functionArgs
    }
  }

  override def visitFunction(ctx: CppContext, x: EirFunction): String = {
    if (visited.contains(x)) return ""
    visited +:= x
    val body = x.body.map(visit(ctx, _)).getOrElse(";")
    val args = dropSelf(x).map(visit(ctx, _))
    visitTemplateArgs(ctx, x.templateArgs) +
    s"${visit(ctx, x.returnType)} ${generateName(x)}(${args mkString ", "}) $body"
  }

  override def visitAnnotation(ctx: CppContext, x: EirAnnotation): String = s"/* @${x.name} */ "

  override def visitBinaryExpression(ctx: CppContext, x: EirBinaryExpression): String = {
    s"(${visit(ctx, x.lhs)} ${x.op} ${visit(ctx, x.rhs)})"
  }

  def generateName(x : EirNode): String = {
    x match {
      case x : EirNamedNode => generateName(x)
      case x : EirSymbol[_] => x.qualifiedName.last
      case _ => throw new RuntimeException(s"name of $x is unknown")
    }
  }

  def generateName(x : EirNamedNode): String = {
    if (x.name == "self") "this" else x.name
  }

  override def visitFunctionArgument(ctx: CppContext, x: EirFunctionArgument): String = {
    s"${visit(ctx, x.declaredType)} ${generateName(x)}"
  }

  override def visitAssignment(ctx: CppContext, x: EirAssignment): String = {
    s"${x.lval} = ${x.rval};"
  }

  override def visitTupleExpression(ctx: CppContext, x: EirTupleExpression): String = {
    val func = x.parent match {
      case Some(a : EirAssignment) if a.lval == x => "tie"
      case _ => "make_tuple"
    }
    s"std::$func(${visit(ctx, x) mkString ", "})"
  }

  override def visitLambdaExpression(ctx: CppContext, x: EirLambdaExpression): String = {
    val retTy = handleOption(ctx, x.foundType).getOrElse("")
    s"[=] (${visit(ctx, x.args) mkString ", "}) -> $retTy ${visit(ctx, x.body)}"
  }

  override def visitReturn(ctx: CppContext, x: EirReturn): String = {
    s"return ${visit(ctx, x.expression)};"
  }

  override def visitSpecializedSymbol(ctx: CppContext, x: EirSpecializedSymbol): String = ???
}
