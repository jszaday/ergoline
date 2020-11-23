package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._

import scala.util.Properties.{lineSeparator => n}

class UnparseContext {
  var numTabs = 0
  // def t: String = "  "

  def t(n : Int = numTabs): String = List.fill(n)(UnparseAst.t).mkString("")
  // def tabsMinusOne: String = List.fill(Math.max(numTabs - 1, 0))(t).mkString("")
}

object UnparseAst extends EirVisitor[UnparseContext, String] {
  def t = "  "

  private object UnparseSyntax {

    implicit class RichOption[T](option: Option[T]) {
      def mapOrEmpty(f: T => String): String = option.map(f).getOrElse("")

      def mapOrSemi(f: T => String): String = option.map(f).getOrElse(";")
    }

  }

  import UnparseSyntax.RichOption

  override def error(ctx: UnparseContext, node: EirNode): String = {
    throw new RuntimeException(s"could not unparse node of type ${node.getClass.getSimpleName}")
  }

  def visit(node: EirNode): String = visit(new UnparseContext, node)

  override def visit(ctx: UnparseContext, node: EirNode): String = {
    val annotations = Option(node)
      .map(_.annotations)
      .map(visit(ctx, _)).toIterable
      .flatten.mkString("")
    annotations + super.visit(ctx, node)
  }

  def addSemi(x: String): String = if (x.endsWith(n) || x.endsWith("}") || x.endsWith(";")) x else s"$x;"

  def visitStatements(ctx: UnparseContext, lst: Iterable[EirNode]): String = {
    val x = visit(ctx, lst).map(addSemi).map(x => s"$n${ctx.t()}$x").mkString
    if (x == "") " " else s"$x$n"
  }

  override def visitBlock(ctx: UnparseContext, node: EirBlock): String = {
    ctx.numTabs += 1
    val body = visitStatements(ctx, node.children)
    ctx.numTabs -= 1
    val tail = if (body.trim.isEmpty) "" else ctx.t()
    s"{$body$tail}"
  }

  override def visitNamespace(ctx: UnparseContext, node: EirNamespace): String = {
    ctx.numTabs += 1
    val body = visitStatements(ctx, node.children)
    ctx.numTabs -= 1
    s"namespace ${node.name} {$body}$n"
  }

  override def visitDeclaration(ctx: UnparseContext, node: EirDeclaration): String = {
    val kwd = if (node.isFinal) "val" else "var"
    val declType = visit(ctx, node.declaredType)
    val expr = node.initialValue.mapOrEmpty(x => s"= ${visit(ctx, x)}")
    s"$kwd ${node.name}: $declType $expr;"
  }

  override def visitTemplateArgument(ctx: UnparseContext, node: EirTemplateArgument): String = {
    node.name +
      node.upperBound.mapOrEmpty(x => " <: " + visit(ctx, x)) +
      node.lowerBound.mapOrEmpty(x => " :> " + visit(ctx, x))
  }

  def visitClassLike(ctx: UnparseContext, keyword: String, node: EirClassLike): String = {
    val decl = node.templateArgs match {
      case Nil => ""
      case lst => s"<${lst.map(visit(ctx, _)) mkString ", "}>"
    }
    ctx.numTabs += 1
    val body = node.members match {
      case Nil => " "
      case lst => s"${visitStatements(ctx, lst)}${ctx.t(ctx.numTabs - 1)}"
    }
    ctx.numTabs -= 1
    val inheritance: String = node.extendsThis.mapOrEmpty(x => s" extends ${visit(ctx, x)}") +
      node.implementsThese.zipWithIndex.map {
        case (x, 0) => s" implements ${visit(ctx, x)}"
        case (x, _) => s" and ${visit(ctx, x)}"
      }.mkString("")
    s"$keyword ${node.name}$decl$inheritance {$body}$n"
  }

  override def visitClass(ctx: UnparseContext, node: EirClass): String = visitClassLike(ctx, "class", node)

  override def visitTrait(ctx: UnparseContext, node: EirTrait): String = visitClassLike(ctx, "trait", node)

  override def visitMember(ctx: UnparseContext, node: EirMember): String = {
    node.accessibility.toString.toLowerCase + " " + addSemi(visit(ctx, node.member))
  }

  override def visitFunction(ctx: UnparseContext, node: EirFunction): String = {
    val args = node.functionArgs.map(visit(ctx, _)) mkString ", "
    val templates = if (node.templateArgs.nonEmpty) "<" + node.templateArgs.map(visit(ctx, _)).mkString(", ") + ">" else ""
    val retType = visit(ctx, node.returnType)
    s"func ${node.name}$templates($args): $retType " + node.body.mapOrSemi(visit(ctx, _))
  }

  override def visitAnnotation(ctx: UnparseContext, node: EirAnnotation): String = s"@${node.name} "

  override def visitBinaryExpression(ctx: UnparseContext, node: EirBinaryExpression): String =
    s"(${visit(ctx, node.lhs)} ${node.op} ${visit(ctx, node.rhs)})"

  override def visitFunctionArgument(ctx: UnparseContext, node: EirFunctionArgument): String = {
    val declTy = visit(ctx, node.declaredType)
    val equals = if (node.isSelfAssigning) "=" else ""
    s"${node.name}$equals: $declTy"
  }

  override def visitAssignment(ctx: UnparseContext, node: EirAssignment): String = {
    val semi = node.parent match {
      case Some(_: EirForLoop) => ""
      case _ => ";"
    }
    s"${visit(ctx, node.lval)} = ${visit(ctx, node.rval)}$semi"
  }

  override def visitTupleExpression(ctx: UnparseContext, node: EirTupleExpression): String =
    s"(${node.expressions.map(visit(ctx, _)) mkString ", "})"

  override def visitLambdaExpression(ctx: UnparseContext, node: EirLambdaExpression): String =
    s"(${node.args.map(visit(ctx, _)) mkString ", "}) => ${visit(ctx, node.body)}"

  override def visitReturn(ctx: UnparseContext, node: EirReturn): String =
    s"return ${visit(ctx, node.expression)};"

  override def visitSymbol[A <: EirNamedNode](ctx: UnparseContext, value: EirSymbol[A]): String = {
    value.qualifiedName mkString "::"
  }

  override def visitLiteral(ctx: UnparseContext, value: EirLiteral): String = value.value

  override def visitForLoop(ctx: UnparseContext, loop: EirForLoop): String = {
    val header: String = loop.header match {
      case EirCStyleHeader(declaration, test, increment) =>
        declaration.mapOrSemi(visit(ctx, _)) + " " +
          test.mapOrEmpty(visit(ctx, _)) + "; " + increment.mapOrEmpty(visit(ctx, _))
      case EirForAllHeader(_, identifiers, expressionNode) =>
        (identifiers mkString ", ") + " <- " + visit(ctx, expressionNode)
    }
    s"for ($header) ${visit(ctx, loop.body)}"
  }

  override def visitFunctionCall(ctx: UnparseContext, call: EirFunctionCall): String = {
    visit(ctx, call.target) + visitSpecialization(ctx, call) + "(" + (call.args.map(visit(ctx, _)) mkString ", ") + ")"
  }

  override def visitImport(ctx: UnparseContext, x: EirImport): String = {
    s"import ${x.qualified mkString "::"};"
  }

  override def visitProxyType(ctx: UnparseContext, x: EirProxyType): String = {
    visit(ctx, x.base) + "@" + x.collective.getOrElse("")
  }

  def superficial(ctx : UnparseContext, node : EirNode): String = {
    node match {
      case x : EirNamedNode => x.name
      case _ => visit(ctx, node)
    }
  }

  def visitSpecialization(ctx : UnparseContext, s : EirSpecialization): String = {
    if (s.specialization.isEmpty) {
      ""
    } else {
      s"<${s.specialization.map(superficial(ctx, _)) mkString ", "}>"
    }
  }

  override def visitTemplatedType(ctx: UnparseContext, x: EirTemplatedType): String = {
    s"${superficial(ctx, x.base)}<${x.args.map(superficial(ctx, _)) mkString ", "}>"
  }

  override def visitLambdaType(ctx: UnparseContext, x: EirLambdaType): String = {
    s"((${x.from.map(superficial(ctx, _)) mkString ", "}) => ${superficial(ctx, x.to)})"
  }

  override def visitTernaryOperator(ctx: UnparseContext, x: EirTernaryOperator): String = {
    s"(${visit(ctx, x.test)} ? ${visit(ctx, x.ifTrue)} : ${x.ifFalse})"
  }

  override def visitFieldAccessor(ctx: UnparseContext, x: EirFieldAccessor): String = {
    s"${visit(ctx, x.target)}.${x.field}"
  }

  override def visitArrayReference(ctx: UnparseContext, x: EirArrayReference): String = {
    s"${visit(ctx, x.target)}[${x.args.map(visit(ctx, _)) mkString ", "}]"
  }

  override def visitSpecializedSymbol(ctx: UnparseContext, x: EirSpecializedSymbol): String = {
    superficial(ctx, x.symbol) + visitSpecialization(ctx, x)
  }

  override def visitIfElse(ctx: UnparseContext, x: EirIfElse): String = {
    val ifFalse = x.ifFalse match {
      case Some(n : EirNode) => s"else ${visit(ctx, n)}"
      case None => ""
    }
    s"if (${visit(ctx, x.test)}) ${x.ifTrue.mapOrEmpty(visit(ctx, _))} $ifFalse"
  }
}
