package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._

import scala.util.Properties.{lineSeparator => n}

object UnparseAst extends EirVisitor[String] {
  var numTabs = 0

  private object UnparseSyntax {

    implicit class RichOption[T](option: Option[T]) {
      def mapOrEmpty(f: T => String): String = option.map(f).getOrElse("")

      def mapOrSemi(f: T => String): String = option.map(f).getOrElse(";")
    }

  }

  import UnparseSyntax.RichOption

  def t: String = "  "

  def tabs: String = List.fill(numTabs)(t).mkString("")

  def tabsMinusOne: String = List.fill(Math.max(numTabs - 1, 0))(t).mkString("")

  override def visit(node: EirNode): String = {
    visit(node.annotations).mkString("") + super.visit(node)
  }

  def addSemi(x: String): String = if (x.endsWith(n) || x.endsWith("}") || x.endsWith(";")) x else s"$x;"

  def visitStatements(lst: Iterable[EirNode]): String = {
    val x = visit(lst).map(addSemi).map(x => s"$n$tabs$x").mkString
    if (x == "") " " else s"$x$n"
  }

  override def visitBlock(node: EirBlock): String = {
    numTabs += 1
    val body = visitStatements(node.children)
    numTabs -= 1
    val tail = if (body.trim.isEmpty) "" else tabs
    s"{$body$tail}"
  }

  override def visitNamespace(node: EirNamespace): String = {
    numTabs += 1
    val body = visitStatements(node.children)
    numTabs -= 1
    s"namespace ${node.name} {$body}$n"
  }

  override def visitDeclaration(node: EirDeclaration): String = {
    val kwd = if (node.isFinal) "val" else "var"
    val declType = visit(node.declaredType)
    val expr = node.initialValue.mapOrEmpty(x => s"= ${visit(x)}")
    s"$kwd ${node.name}: $declType $expr;"
  }

  override def visitTemplateArgument(node: EirTemplateArgument): String = {
    node.name +
      node.upperBound.mapOrEmpty(x => " <: " + visit(x)) +
      node.lowerBound.mapOrEmpty(x => " :> " + visit(x))
  }

  def visitClassLike(keyword: String, node: EirClassLike): String = {
    val decl = node.templateArgs match {
      case Nil => ""
      case lst => s"<${lst.map(visit) mkString ", "}>"
    }
    numTabs += 1
    val body = node.members match {
      case Nil => " "
      case lst => s"${visitStatements(lst)}$tabsMinusOne"
    }
    numTabs -= 1
    val inheritance: String = node.extendsThis.mapOrEmpty(x => s" extends ${visit(x)}") +
      node.implementsThese.zipWithIndex.map {
        case (x, 0) => s" implements ${visit(x)}"
        case (x, _) => s" and ${visit(x)}"
      }.mkString("")
    s"$keyword ${node.name}$decl$inheritance {$body}$n"
  }

  override def visitClass(node: EirClass): String = visitClassLike("class", node)

  override def visitTrait(node: EirTrait): String = visitClassLike("trait", node)

  override def visitMember(node: EirMember): String = {
    node.accessibility.toString.toLowerCase + " " + addSemi(visit(node.member))
  }

  override def visitFunction(node: EirFunction): String = {
    val args = node.functionArgs.map(visit) mkString ", "
    val templates = if (node.templateArgs.nonEmpty) "<" + node.templateArgs.map(visit).mkString(", ") + ">" else ""
    val retType = visit(node.returnType)
    s"func ${node.name}$templates($args): $retType " + node.body.mapOrSemi(visit)
  }

  override def visitAnnotation(node: EirAnnotation): String = s"@${node.name} "

  override def visitBinaryExpression(node: EirBinaryExpression): String =
    s"(${visit(node.lhs)} ${node.op} ${visit(node.rhs)})"

  override def visitFunctionArgument(node: EirFunctionArgument): String = {
    val declTy = visit(node.declaredType)
    val equals = if (node.isSelfAssigning) "=" else ""
    s"${node.name}$equals: $declTy"
  }

  override def visitAssignment(node: EirAssignment): String = {
    val semi = node.parent match {
      case Some(_: EirForLoop) => ""
      case _ => ";"
    }
    s"${visit(node.lval)} = ${visit(node.rval)}$semi"
  }

  override def visitTupleExpression(node: EirTupleExpression): String =
    s"(${node.expressions.map(visit) mkString ", "})"

  override def visitLambdaExpression(node: EirLambdaExpression): String =
    s"(${node.args.map(visit) mkString ", "}) => ${visit(node.body)}"

  override def visitReturn(node: EirReturn): String =
    s"return ${visit(node)};"

  override def visitSymbol(value: EirSymbol[_]): String = value.qualifiedName mkString "::"

  override def visitLiteral(value: EirLiteral): String = value.value

  override def visitForLoop(loop: EirForLoop): String = {
    val header: String = loop.header match {
      case EirCStyleHeader(declaration, test, increment) =>
        declaration.mapOrSemi(visit) + " " +
          test.mapOrEmpty(visit) + "; " + increment.mapOrEmpty(visit)
      case EirForAllHeader(_, identifiers, expressionNode) =>
        (identifiers mkString ", ") + " <- " + visit(expressionNode)
    }
    s"for ($header) ${visit(loop.body)}"
  }

  override def visitFunctionCall(call: EirFunctionCall): String = {
    visit(call.target) + "(" + (call.args.map(visit) mkString ", ") + ")"
  }

  override def visitImport(eirImport: EirImport): String = {
    s"import ${visit(eirImport.symbol)};"
  }

  override def visitProxyType(x: EirProxyType): String = {
    visit(x.base) + "@" + x.collective.getOrElse("")
  }

  override def visitTemplatedType(x: EirTemplatedType): String = {
    s"${visit(x.base)}<${x.args.map(visit) mkString ", "}>"
  }

  override def visitLambdaType(x: EirLambdaType): String = {
    s"((${x.from.map(visit) mkString ", "}) => ${visit(x.to)})"
  }

  override def visitTernaryOperator(x: EirTernaryOperator): String = {
    s"(${visit(x.test)} ? ${visit(x.ifTrue)} : ${x.ifFalse})"
  }

  override def visitFieldAccessor(x: EirFieldAccessor): String = {
    s"${visit(x.target)}.${x.field}"
  }

  override def visitArrayReference(x: EirArrayReference): String = {
    s"${visit(x.target)}[${x.args.map(visit) mkString ", "}]"
  }

  override def visitGlobalNamespace(): String = {
    visitStatements(EirGlobalNamespace.children).trim + n
  }
}
