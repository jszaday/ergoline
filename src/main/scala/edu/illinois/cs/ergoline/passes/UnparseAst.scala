package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.passes.UnparseAst.UnparseContext
import edu.illinois.cs.ergoline.proxies.EirProxy

import scala.util.Properties.{lineSeparator => n}

object UnparseAst {
  val tab = "  "

  class UnparseContext {
    var numTabs = 0
    /* (n : Int = numTabs) */
    def t: String = List.fill(numTabs)(tab).mkString("")
  }

  private val _instance = new UnparseAst

  def visit(node: EirNode): String = _instance.visit(new UnparseContext, node)
}

class UnparseAst extends EirVisitor[UnparseContext, String] {

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

  def visitAnnotations(ctx: UnparseContext, annotations: Iterable[EirAnnotation]): String = {
    annotations.map(visitAnnotation(ctx, _)).mkString(" ")
  }

  override def visit(ctx: UnparseContext, node: EirNode): String = {
    visitAnnotations(ctx, Option(node).map(_.annotations).getOrElse(Nil)) + super.visit(ctx, node)
  }

  def addSemi(x: String): String = {
    val trimmed = x.replaceAll("""(?m)\s+$""", "") // .stripTrailing()
    if (trimmed.endsWith("}") || trimmed.endsWith(";") || trimmed.isEmpty) trimmed else s"$trimmed;"
  }

  def visitStatements(ctx: UnparseContext, lst: Iterable[EirNode]): String = {
    val x = visit(ctx, lst).map(addSemi).filter(_.nonEmpty).map(x => s"$n${ctx.t}$x").mkString
    if (x.nonEmpty) s"$x$n" else " "
  }

  override def visitBlock(ctx: UnparseContext, node: EirBlock): String = {
    ctx.numTabs += 1
    val body = visitStatements(ctx, node.children)
    ctx.numTabs -= 1
    val tail = if (body.trim.isEmpty) "" else ctx.t
    s"{$body$tail}"
  }

  override def visitNamespace(ctx: UnparseContext, node: EirNamespace): String = {
    s"namespace ${node.name} {${visitStatements(ctx, node.children)}}$n"
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

  def visitChildren(ctx: UnparseContext, children: List[EirNode]): String = {
    ctx.numTabs += 1
    val body = visitStatements(ctx, children)
    ctx.numTabs -= 1
    "{" + body + s"${ctx.t}}"
  }

  def visitClassLike(ctx: UnparseContext, node: EirClassLike): String = {
    val keyword = node match {
      case _: EirClass => "class"
      case _: EirTrait => "trait"
    }
    val decl = node.templateArgs match {
      case Nil => ""
      case lst => s"<${lst.map(visit(ctx, _)) mkString ", "}>"
    }
    val body = visitChildren(ctx, node.members)
    val inheritance: String = node.extendsThis.mapOrEmpty(x => s" extends ${visit(ctx, x)}") +
      node.implementsThese.zipWithIndex.map {
        case (x, 0) => s" with ${visit(ctx, x)}"
        case (x, _) => s" and ${visit(ctx, x)}"
      }.mkString("")
    s"$keyword ${node.name}$decl$inheritance $body$n"
  }

  override def visitClass(ctx: UnparseContext, node: EirClass): String = visitClassLike(ctx, node)

  override def visitTrait(ctx: UnparseContext, node: EirTrait): String = visitClassLike(ctx, node)

  override def visitMember(ctx: UnparseContext, node: EirMember): String = {
    val overrides = if (node.isOverride) "override " else ""
    val accessibility: String = node.accessibility match {
      case EirAccessibility.Public => ""
      case x => x.toString.toLowerCase + " "
    }
    accessibility + overrides + addSemi(visit(ctx, node.member))
  }

  override def visitFunction(ctx: UnparseContext, node: EirFunction): String = {
    val args = node.functionArgs.map(visit(ctx, _)) mkString ", "
    val templates = if (node.templateArgs.nonEmpty) "<" + node.templateArgs.map(visit(ctx, _)).mkString(", ") + ">" else ""
    val retType = visit(ctx, node.returnType)
    s"def ${node.name}$templates($args): $retType " + node.body.mapOrSemi(visit(ctx, _))
  }

  override def visitAnnotation(ctx: UnparseContext, node: EirAnnotation): String = s"@${node.name} "

  override def visitBinaryExpression(ctx: UnparseContext, node: EirBinaryExpression): String =
    s"(${visit(ctx, node.lhs)} ${node.op} ${visit(ctx, node.rhs)})"

  override def visitFunctionArgument(ctx: UnparseContext, node: EirFunctionArgument): String = {
    val declTy = visit(ctx, node.declaredType)
    val equals = if (node.isSelfAssigning) "=" else ""
    s"$equals${node.name}: $declTy"
  }

  override def visitAssignment(ctx: UnparseContext, node: EirAssignment): String = {
    val semi = node.parent match {
      case Some(_: EirForLoop) => ""
      case _ => ";"
    }
    s"${visit(ctx, node.lval)} ${node.op} ${visit(ctx, node.rval)}$semi"
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

  def nameFor(ctx : UnparseContext, node : EirNode): String = {
    node match {
      case x : EirNamedNode => x.name
      case _ => visit(ctx, node)
    }
  }

  def visitSpecialization(ctx : UnparseContext, s : EirSpecialization): String = {
    if (s.specialization.isEmpty) {
      ""
    } else {
      s"<${s.specialization.map(nameFor(ctx, _)) mkString ", "}>"
    }
  }

  override def visitTemplatedType(ctx: UnparseContext, x: EirTemplatedType): String = {
    s"${nameFor(ctx, x.base)}<${x.args.map(nameFor(ctx, _)) mkString ", "}>"
  }

  override def visitLambdaType(ctx: UnparseContext, x: EirLambdaType): String = {
    s"((${x.from.map(nameFor(ctx, _)) mkString ", "}) => ${nameFor(ctx, x.to)})"
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
    nameFor(ctx, x.symbol) + visitSpecialization(ctx, x)
  }

  override def visitIfElse(ctx: UnparseContext, x: EirIfElse): String = {
    val ifFalse = x.ifFalse match {
      case Some(n : EirNode) => s"else ${visit(ctx, n)}"
      case None => ""
    }
    s"if (${visit(ctx, x.test)}) ${x.ifTrue.mapOrEmpty(visit(ctx, _))} $ifFalse"
  }

  override def visitNew(ctx: UnparseContext, x: EirNew): String = {
    s"new ${visit(ctx, x.target)}(${visit(ctx, x.args) mkString ", "})"
  }

  override def visitProxy(ctx: UnparseContext, x: EirProxy): String =
    visitProxyType(ctx, EirProxyType(x.parent, x.base, x.collective))
}
