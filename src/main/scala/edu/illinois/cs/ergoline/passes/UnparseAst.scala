package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.literals.{EirLiteral, EirStringLiteral}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.Matcher
import edu.illinois.cs.ergoline.passes.UnparseAst.UnparseContext
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.{EirPlaceholder, EirResolvable}
import edu.illinois.cs.ergoline.util.Errors

import scala.util.Properties.{lineSeparator => n}

object UnparseAst {
  val tab = "  "

  class UnparseContext(val lang: String) {
    var numTabs = 0
    /* (n : Int = numTabs) */
    def t: String = List.fill(numTabs)(tab).mkString("")

    override def toString: String = lang
  }

  private val _instance = new UnparseAst

  private def mkContext() = new UnparseContext("ergoline")

  def nameFor(node: EirNode): String = _instance.nameFor(node)(mkContext())

  def visit(node: EirNode): String = _instance.visit(node)(mkContext())

  def visitLoopHeader(node: EirNode): (String, String) =
    _instance.visitLoopHeader(node)(mkContext())

  def visitWhenHeader(node: EirSdagWhen): String =
    _instance.visitWhenHeader(node)(mkContext())

  def escape(s: String): String = {
    s.replace("\\", "\\\\")
      .replace("\t", "\\t")
      .replace("\b", "\\b")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\f", "\\f")
      .replace("\'", "\\'")
      .replace("\"", "\\\"");
  }

  def forceSemi(s: String): String = {
    // here for compatibility with JDK8 -- borrowed from rosetta code
    // JDK11+ should simply use "stripTrailing"
    def rtrim(s: String): String = {
      var i = s.length() - 1
      while (i > 0 && Character.isWhitespace(s.charAt(i))) {
        i -= 1
      }
      s.substring(0, i + 1)
    }
    val st = rtrim(s)
    if (st.endsWith(";") || st.endsWith("}")) st else s"$st;"
  }
}

class UnparseAst extends EirVisitor[UnparseContext, String] {

  private object UnparseSyntax {

    implicit class RichOption[T](option: Option[T]) {
      def mapOrEmpty(f: T => String): String = option.map(f).getOrElse("")

      def mapOrSemi(f: T => String): String = option.map(f).getOrElse(";")
    }

  }

  import UnparseSyntax.RichOption

  override def error(node: EirNode)(implicit ctx: UnparseContext): String =
    error(ctx, node, s"unknown error on ${node.getClass.getName}")

  override def fallback(implicit ctx: UnparseContext): Matcher = {
    case x: EirClosure          => visitBlock(x.block)
    case x: GenerateCpp.CppNode => s"system { ${x.s} }"
  }

  def error(ctx: UnparseContext, node: EirNode, msg: String = ""): String =
    Errors.exit(Errors.format(node, msg))

  def visitAnnotations(
      annotations: Iterable[EirAnnotation]
  )(implicit ctx: UnparseContext): String = {
    annotations.map(visitAnnotation(_)).mkString("")
  }

  override def visit(node: EirNode)(implicit ctx: UnparseContext): String = {
    visitAnnotations(Option(node).map(_.annotations).getOrElse(Nil)) + (
      node match {
        case _: EirPlaceholder[_] => "_"
        case _                    => super.visit(node)
      }
    )
  }

  def split(
      ctx: UnparseContext,
      lines: String,
      maxLength: Int = 120
  ): String = {
    def splitter(x: String): String = {
      var idx =
        if (x.length > maxLength) x.substring(0, maxLength).lastIndexOf(' ')
        else -1
      if (idx >= 0) {
        if (x.substring(0, idx).count(_ == '\"') % 2 == 1) {
          idx = x.substring(0, idx).lastIndexOf('"') - 1
          idx = if (x.charAt(idx) == '(') idx + 1 else idx
        }
        val c = if (x.charAt(idx).isWhitespace) "" else x.charAt(idx)
        s"${x.substring(0, idx)}$n${ctx.t}${ctx.t}$c${splitter(x.substring(idx + 1))}"
      } else x
    }
    lines.split(n).map(splitter).mkString(n)
  }

  def visitStatements(
      lst: Iterable[EirNode]
  )(implicit ctx: UnparseContext): String = {
    val x = lst
      .map(visitStatement)
      .map(split(ctx, _))
      .filter(_.nonEmpty)
      .map(x => s"$n${ctx.t}$x")
      .mkString
    if (x.nonEmpty) s"$x$n" else " "
  }

  override def visitBlock(
      node: EirBlock
  )(implicit ctx: UnparseContext): String = {
    ctx.numTabs += 1
    val body = visitStatements(node.children)
    ctx.numTabs -= 1
    val tail = if (body.trim.isEmpty) "" else ctx.t
    s"{$body$tail}"
  }

  override def visitNamespace(
      node: EirNamespace
  )(implicit ctx: UnparseContext): String = {
    s"namespace ${node.name} {${visitStatements(node.children)}}$n"
  }

  private def declKeyword(isFinal: Boolean) = if (isFinal) "val" else "var"

  override def visitDeclaration(
      node: EirDeclaration
  )(implicit ctx: UnparseContext): String = {
    val kwd = declKeyword(node.isFinal)
    val declType = nameFor(node.declaredType)
    val expr = node.initialValue.mapOrEmpty(x => s" = ${visit(x)}")
    s"$kwd ${node.name}: $declType$expr;"
  }

  override def visitTemplateArgument(
      node: EirTemplateArgument
  )(implicit ctx: UnparseContext): String = {
    (node.variance match {
      case EirInvariant     => ""
      case EirCovariant     => "+"
      case EirContravariant => "-"
    }) + node.name +
      node.upperBound.mapOrEmpty(x => " <: " + nameFor(x)) +
      node.lowerBound.mapOrEmpty(x => " >: " + nameFor(x)) +
      node.argumentType.mapOrEmpty(x => ": " + nameFor(x)) +
      node.defaultValue.mapOrEmpty(x => s" = ${visit(x)}")
  }

  def visitChildren(
      children: List[EirNode]
  )(implicit ctx: UnparseContext): String = {
    ctx.numTabs += 1
    val body = visitStatements(children)
    ctx.numTabs -= 1
    "{" + body + s"$n${ctx.t}}"
  }

  def visitClassLike(
      node: EirClassLike
  )(implicit ctx: UnparseContext): String = {
    val keyword = node match {
      case c: EirClass if c.valueType => "struct"
      case c: EirClass                => "class"
      case _: EirTrait                => "trait"
    }
    val decl = node.templateArgs match {
      case Nil => ""
      case lst => s"<${lst.map(visit(_)) mkString ", "}>"
    }
    val body = visitChildren(node.members)
    val inheritance: String = node.extendsThis.mapOrEmpty(x =>
      s" extends ${visit(x)}"
    ) +
      node.implementsThese.zipWithIndex
        .map {
          case (x, 0) => s" with ${visit(x)}"
          case (x, _) => s" and ${visit(x)}"
        }
        .mkString("")
    s"$keyword ${node.name}$decl$inheritance${visitWhere(node.predicate)} $body$n"
  }

  override def visitClass(node: EirClass)(implicit
      ctx: UnparseContext
  ): String = visitClassLike(node)

  override def visitTrait(node: EirTrait)(implicit
      ctx: UnparseContext
  ): String = visitClassLike(node)

  override def visitMember(
      node: EirMember
  )(implicit ctx: UnparseContext): String = {
    val overrides = if (node.isOverride) "override " else ""
    val accessibility: String = node.accessibility match {
      case EirAccessibility.Public => ""
      case x                       => x.toString.toLowerCase + " "
    }
    accessibility + overrides + visitStatement(node.member)
  }

  override def visitFunction(
      node: EirFunction
  )(implicit ctx: UnparseContext): String = {
    val args = node.functionArgs.map(visit(_)) mkString ", "
    val templates =
      if (node.templateArgs.nonEmpty)
        "<" + node.templateArgs.map(visit(_)).mkString(", ") + ">"
      else ""
    val retType = nameFor(node.returnType)
    s"def ${node.name}$templates($args): $retType${visitWhere(node.predicate)}${visitOptionalStatement(node.body)}"
  }

  override def visitAnnotation(node: EirAnnotation)(implicit
      ctx: UnparseContext
  ): String = {
    val opts = Option(node.opts)
      .filterNot(_.isEmpty)
      .mapOrEmpty(opts => {
        s"(${opts.map { case (id, value) => s"$id=${visit(value)}" } mkString (",")})"
      })
    s"@${node.name}$opts "
  }

  override def visitBinaryExpression(
      node: EirBinaryExpression
  )(implicit ctx: UnparseContext): String =
    s"(${visit(node.lhs)} ${node.op} ${visit(node.rhs)})"

  override def visitFunctionArgument(
      node: EirFunctionArgument
  )(implicit ctx: UnparseContext): String = {
    val declTy = nameFor(node.declaredType)
    val equals = if (node.isSelfAssigning) "=" else ""
    val asterisk = if (node.isExpansion) "*" else ""
    s"$equals${node.name}: $asterisk$declTy"
  }

  override def visitAssignment(
      node: EirAssignment
  )(implicit ctx: UnparseContext): String = {
    val semi = node.parent match {
      case Some(_: EirForLoop) => ""
      case _                   => ";"
    }
    s"${visit(node.lval)} ${node.op} ${visit(node.rval)}$semi"
  }

  override def visitTupleExpression(
      node: EirTupleExpression
  )(implicit ctx: UnparseContext): String =
    s"(${node.expressions.map(visit(_)) mkString ", "})"

  override def visitLambdaExpression(
      node: EirLambdaExpression
  )(implicit ctx: UnparseContext): String =
    s"(${node.args.map(visit(_)) mkString ", "}) => ${visit(node.body)}"

  override def visitReturn(
      node: EirReturn
  )(implicit ctx: UnparseContext): String = s"return ${visit(node.expression)};"

  override def visitSymbol[A <: EirNamedNode](
      value: EirSymbol[A]
  )(implicit ctx: UnparseContext): String = {
    (value.qualifiedName match {
      case "::" :: rest => "" +: rest
      case list         => list
    }).mkString("::")
  }

  override def visitLiteral(x: EirLiteral[_])(implicit
      ctx: UnparseContext
  ): String = x.value.toString

  def visitLoopHeader(
      node: EirNode
  )(implicit ctx: UnparseContext): (String, String) = {
    node match {
      case x: EirDoWhileLoop => ("do", s" while(${visit(x.condition)});")
      case x: EirWhileLoop   => (s"while(${visit(x.condition)})", "")
      case EirForLoop(_, hdr, _) => (
          "for (" + {
            hdr match {
              case x: EirCStyleHeader =>
                x.declaration.mapOrSemi(visit(_)) + visitOptionalStatement(
                  x.test
                ) + x.increment.mapOrEmpty(" " + visit(_))
              case x: EirForAllHeader =>
                (if (x.identifiers.length == 1) {
                   x.identifiers.head
                 } else {
                   s"(${x.identifiers.mkString(", ")})"
                 }) + " <- " + visit(x.expression)
            }
          } + ")",
          ""
        )
      case _ => ???
    }
  }

  def visitStatement(x: EirNode)(implicit ctx: UnparseContext): String = {
    x match {
      case _: EirExpressionNode => visit(x) + ";"
      case _                    => visit(x)
    }
  }

  def visitOptionalStatement(
      x: Option[EirNode],
      alt: String = ";"
  )(implicit ctx: UnparseContext): String = {
    x.map(visitStatement).map(" " + _).getOrElse(alt)
  }

  override def visitWhileLoop(
      loop: EirWhileLoop
  )(implicit ctx: UnparseContext): String = {
    visitLoopHeader(loop)._1 + visitOptionalStatement(loop.body)
  }

  override def visitDoWhileLoop(
      loop: EirDoWhileLoop
  )(implicit ctx: UnparseContext): String = {
    val (head, tail) = visitLoopHeader(loop)
    s"$head${visitOptionalStatement(loop.body)}$tail"
  }

  override def visitForLoop(
      loop: EirForLoop
  )(implicit ctx: UnparseContext): String = {
    visitLoopHeader(loop)._1 + visitOptionalStatement(loop.body)
  }

  override def visitFunctionCall(
      call: EirFunctionCall
  )(implicit ctx: UnparseContext): String = {
    visit(call.target) + visitSpecialization(call) + "(" + (call.args.map(
      visit(_)
    ) mkString ", ") + ")"
  }

  override def visitImport(
      x: EirImport
  )(implicit ctx: UnparseContext): String = {
    s"import ${x.qualified mkString "::"};"
  }

  def kindToSymbol(kind: Option[EirProxyKind]): String = {
    kind match {
      case Some(EirSectionProxy) => "{@}"
      case Some(EirElementProxy) => "[@]"
      case _                     => "@"
    }
  }

  def visitProxyLike(
      base: EirResolvable[EirType],
      kind: Option[EirProxyKind],
      collective: Option[String]
  )(implicit ctx: UnparseContext): String = {
    nameFor(base) + kindToSymbol(kind) + collective.getOrElse("")
  }

  override def visitProxyType(
      x: EirProxyType
  )(implicit ctx: UnparseContext): String =
    visitProxyLike(x.base, x.kind, x.collective)

  def nameFor(node: EirNode)(implicit ctx: UnparseContext): String = {
    node match {
      case _: EirProxy | _: EirProxyType => visit(node)
      case x: EirNamedNode               => x.name
      case _                             => visit(node)
    }
  }

  def visitSpecialization(s: EirSpecialization)(implicit
      ctx: UnparseContext
  ): String = visitSpecialization(s.types)

  def visitSpecialization(
      lst: List[EirResolvable[EirType]]
  )(implicit ctx: UnparseContext): String = {
    lst match {
      case Nil => ""
      case _   => s"<${lst.map(nameFor(_)) mkString ", "}>"
    }
  }

  override def visitTemplatedType(
      x: EirTemplatedType
  )(implicit ctx: UnparseContext): String = {
    val proxy = x.base match {
      case t: EirProxy =>
        (if (t.isElement) "[@]" else "@") + t.collective.getOrElse("")
      case t: EirProxyType =>
        (if (t.isElement) "[@]" else "@") + t.collective.getOrElse("")
      case _ => ""
    }
    s"${nameFor(x.base)}<${x.args.map(nameFor(_)) mkString ", "}>$proxy"
  }

  override def visitLambdaType(
      x: EirLambdaType
  )(implicit ctx: UnparseContext): String = {
    s"((${x.from.map(nameFor(_)) mkString ", "}) => ${nameFor(x.to)})"
  }

  override def visitTernaryOperator(
      x: EirTernaryOperator
  )(implicit ctx: UnparseContext): String = {
    s"(${visit(x.test)} ? ${visit(x.ifTrue)} : ${visit(x.ifFalse)})"
  }

  override def visitScopedSymbol[A <: EirNode](
      x: EirScopedSymbol[A]
  )(implicit ctx: UnparseContext): String = {
    s"${visit(x.target)}${if (x.isStatic) "::" else "."}${visit(x.pending)}"
  }

  override def visitArrayReference(
      x: EirArrayReference
  )(implicit ctx: UnparseContext): String = {
    s"${visit(x.target)}[${x.args.map(visit(_)) mkString ", "}]"
  }

  override def visitSpecializedSymbol[A <: EirNamedNode](
      x: EirSpecializedSymbol[A]
  )(implicit ctx: UnparseContext): String = {
    nameFor(x.symbol) + visitSpecialization(x)
  }

  override def visitIfElse(
      x: EirIfElse
  )(implicit ctx: UnparseContext): String = {
    val ifFalse =
      x.ifFalse.mapOrEmpty(_ => s" else${visitOptionalStatement(x.ifFalse)}")
    s"if (${visit(x.test)})${visitOptionalStatement(x.ifTrue)}$ifFalse "
  }

  override def visitNew(x: EirNew)(implicit ctx: UnparseContext): String = {
    s"new ${visit(x.target)}(${visit(x.args) mkString ", "})"
  }

  override def visitProxy(x: EirProxy)(implicit ctx: UnparseContext): String =
    visitProxyLike(x.base, x.kind, x.collective)

  override def visitMatch(x: EirMatch)(implicit ctx: UnparseContext): String = {
    s"${n}match (${visit(x.expression)}) {" + {
      ctx.numTabs += 1
      val res = visit(x.cases).mkString("")
      ctx.numTabs -= 1
      res
    } + s"$n}"
  }

  override def visitMatchCase(
      x: EirMatchCase
  )(implicit ctx: UnparseContext): String = {
    val ifCond = x.condition.map(y => s"if ${visit(y)} ").getOrElse("")
    val pattern = visit(x.patterns).init.tail
    s"$n${ctx.t}case $pattern $ifCond=>${visitOptionalStatement(x.body, alt = " ;")}"
  }

  override def visitPatternList(
      x: EirPatternList
  )(implicit ctx: UnparseContext): String = {
    s"(${x.patterns.map(visit(_)) mkString ", "})"
  }

  override def visitExpressionPattern(
      x: EirExpressionPattern
  )(implicit ctx: UnparseContext): String = visit(x.expression)

  override def visitIdentifierPattern(
      x: EirIdentifierPattern
  )(implicit ctx: UnparseContext): String = {
    x.name + ": " + nameFor(x.ty)
  }

  override def visitTupleType(
      x: EirTupleType
  )(implicit ctx: UnparseContext): String = {
    s"(${x.children.map(nameFor(_)) mkString ", "})"
  }

  override def visitAwait(x: EirAwait)(implicit ctx: UnparseContext): String = {
    s"await ${x.target}"
  }

  override def visitInterpolatedString(
      x: EirInterpolatedString
  )(implicit ctx: UnparseContext): String = {
    "`" + (x.children.map {
      case EirStringLiteral(value) => value
      case x                       => "${" + visit(x) + "}"
    } mkString "") + "`"
  }

  override def visitTypeAlias(
      x: EirTypeAlias
  )(implicit ctx: UnparseContext): String = {
    s"using ${x.name}${visitSpecialization(x.templateArgs)} = ${visit(x.value)};"
  }

  override def visitTupleMultiply(
      multiply: EirTupleMultiply
  )(implicit ctx: UnparseContext): String = {
    s"${nameFor(multiply.lhs)} .* ${visit(multiply.rhs)}"
  }

  override def visitConstantFacade(
      facade: EirConstantFacade
  )(implicit context: UnparseContext): String = visit(facade.value)

  def visitWhenHeader(x: EirSdagWhen)(implicit ctx: UnparseContext): String = {
    "when " + x.patterns
      .map(p => s"${visit(p._1)}${visit(p._2)}")
      .mkString(", ") + {
      x.condition.map(visitExpression(_)).map(" if " + _).getOrElse("")
    } + " =>"
  }

  override def visitWhen(
      x: EirSdagWhen
  )(implicit ctx: UnparseContext): String = {
    visitWhenHeader(x) + visitOptionalStatement(x.body, alt = " ;")
  }

  override def visitSlice(x: EirSlice)(implicit ctx: UnparseContext): String = {
    (x.start, x.step, x.end) match {
      case (None, None, None)          => ":"
      case (None, Some(y), None)       => s":${visit(y)}:"
      case (Some(x), None, Some(z))    => s"${visit(x)}:${visit(z)}"
      case (Some(x), None, None)       => s"${visit(x)}:"
      case (Some(x), Some(y), None)    => s"${visit(x)}:${visit(y)}:"
      case (Some(x), Some(y), Some(z)) => s"${visit(x)}:${visit(y)}:${visit(z)}"
      case (None, Some(y), Some(z))    => s":${visit(y)}:${visit(z)}"
      case (None, None, Some(z))       => s":${visit(z)}"
    }
  }

  override def visitAwaitMany(
      x: EirAwaitMany
  )(implicit ctx: UnparseContext): String = {
    s"await ${if (x.waitAll) "all" else "any"} {" + {
      ctx.numTabs += 1
      val res = visitStatements(x.children)
      ctx.numTabs -= 1
      res
    } + s"}"
  }

  override def visitUnaryExpression(
      x: EirUnaryExpression
  )(implicit ctx: UnparseContext): String = {
    s"(${x.op}(${x.rhs}))"
  }

  def visitWhere(
      opt: Option[EirExpressionNode]
  )(implicit ctx: UnparseContext): String = {
    opt match {
      case Some(s) => s" where ${visit(s)}"
      case None    => ""
    }
  }

  override def visitReferenceType(x: EirReferenceType)(implicit
      ctx: UnparseContext
  ): String = nameFor(x.base) + "&"

  override def visitMultiDeclaration(
      x: EirMultiDeclaration
  )(implicit ctx: UnparseContext): String = {
    val kwd = declKeyword(x.isFinal)
    // TODO ( incorporate types )
    kwd + " (" + x.children.map(_.name).mkString(",") + ")" + x.initialValue
      .map(visit)
      .map(" = " + _)
      .getOrElse("") + ";"
  }

  override def visitExtractorPattern(
      x: EirExtractorPattern
  )(implicit ctx: UnparseContext): String = {
    visit(x.identifier) + visit(x.list)
  }
}
