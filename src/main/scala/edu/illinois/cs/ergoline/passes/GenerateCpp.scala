package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.EirProxyType
import edu.illinois.cs.ergoline.passes.UnparseAst.UnparseContext

import scala.util.Properties.{lineSeparator => n}
import edu.illinois.cs.ergoline.resolution.Find

object GenerateCpp extends UnparseAst {
  var visited : List[EirNode] = Nil

  def visit(node: EirNode): String = visit(new UnparseContext, node)

  override def error(ctx: UnparseContext, node : EirNode): String = {
    println(s"silently ignoring my failure to process $node")
    ""
  }

  override def visitArrayReference(ctx: UnparseContext, x: EirArrayReference): String = ???

  override def visitFieldAccessor(ctx: UnparseContext, x: EirFieldAccessor): String = {
    // TODO handle self applications :3
    s"${visit(ctx, x.target)}.${x.field}"
  }

  override def visitTernaryOperator(ctx: UnparseContext, x: EirTernaryOperator): String = ???

  override def visitLambdaType(ctx: UnparseContext, x: types.EirLambdaType): String = "auto"

  override def visitProxyType(ctx: UnparseContext, x: types.EirProxyType): String = {
    "CProxy_" + visit(ctx, x.base)
  }

  override def visitImport(ctx: UnparseContext, x: EirImport): String = ""

  def handleOption(ctx: UnparseContext, x : Option[EirNode]): Option[String] = {
    x.map({
      case n : EirNamedNode => nameFor(ctx, n)
      case n => visit(ctx, n)
    })
  }

  override def visitFunctionCall(ctx: UnparseContext, x: EirFunctionCall): String = {
    val target = visit(x.target)
    // handleOption(ctx, x.target.disambiguation).getOrElse(visit(ctx, x.target))
    s"($target${visitSpecialization(ctx, x)}(${x.args.map(visit(ctx, _)) mkString ", "}))"
  }

  override def visitForLoop(ctx: UnparseContext, x: EirForLoop): String = {
    x.header match {
      case EirCStyleHeader(d, t, i) => {
        val decl = visit(ctx, d).headOption.getOrElse(";")
        val test = visit(ctx, t).headOption.getOrElse("")
        val incr = visit(ctx, i).headOption.getOrElse("")
        s"for ($decl $test; $incr) " + visit(ctx, x.body)
      }
      case _ => ???
    }
  }

  override def visitLiteral(ctx: UnparseContext, x: EirLiteral): String = x.value

  override def visitSymbol[A <: EirNamedNode](ctx: UnparseContext, x: EirSymbol[A]): String = {
    try {
      nameFor(ctx, Find.uniqueResolution(x))
    } catch {
      case _: Throwable => x.qualifiedName.mkString("::")
    }
  }

  override def visitDeclaration(ctx: UnparseContext, x: EirDeclaration): String = {
    s"${visit(ctx, x.declaredType)} ${nameFor(ctx, x)}" + {
      x.initialValue.map(x => s" = ${visit(ctx, x)}").getOrElse("")
    } + ";"
  }

  override def visitTemplateArgument(ctx: UnparseContext, x: EirTemplateArgument): String = {
    s"typename ${nameFor(ctx, x)}"
  }

  def visitInherits(ctx: UnparseContext, x: EirClassLike): String = {
    val parents = (x.extendsThis ++ x.implementsThese).map(visit(ctx, _))
    if (parents.nonEmpty) ": " + parents.map("public " + _).mkString(", ")
    else ""
  }

  override def visitClassLike(ctx: UnparseContext, x: EirClassLike): String = {
    visitTemplateArgs(ctx, x.templateArgs) + s"class ${nameFor(ctx, x)} " + visitInherits(ctx, x) + visitChildren(ctx, x.members) + s";$n"
  }

  override def visitMember(ctx: UnparseContext, x: EirMember): String = {
    visit(ctx, x.member)
  }

  def visitTemplateArgs(ctx: UnparseContext, args : List[EirTemplateArgument]): String = {
    if (args.isEmpty) ""
    else s"template<${args.map(visit(ctx, _)) mkString ", "}>$n${ctx.t}"
  }

  def dropSelf(x : EirFunction): List[EirFunctionArgument] = {
    if (x.functionArgs.isEmpty) x.functionArgs
    else x.parent match {
      case Some(_ : EirMember) if x.functionArgs.head.name == "self" =>  x.functionArgs.tail
      case _ => x.functionArgs
    }
  }

  override def visitFunction(ctx: UnparseContext, x: EirFunction): String = {
    if (visited.contains(x)) return ""
    visited +:= x
    val body = x.body.map(visit(ctx, _)).getOrElse(";")
    val args = dropSelf(x).map(visit(ctx, _))
    val cons = x.parent.exists({
      case m: EirMember => m.isConstructor
      case _ => false
    })
    val retTy = if (cons) "" else { visit(ctx, x.returnType) + " " }
    val static = x.parent.collect({
      case m : EirMember if m.isStatic => "static "
    }).getOrElse("")
    val const = x.parent.collect({
      case m : EirMember if m.isConst => " const"
    }).getOrElse("")
    visitTemplateArgs(ctx, x.templateArgs) +
    s"$static$retTy${nameFor(ctx, x)}(${args mkString ", "})$const $body"
  }

  override def visitAnnotation(ctx: UnparseContext, x: EirAnnotation): String = s"/* @${x.name} */ "

  override def visitBinaryExpression(ctx: UnparseContext, x: EirBinaryExpression): String = {
    s"(${visit(ctx, x.lhs)} ${x.op} ${visit(ctx, x.rhs)})"
  }

  override def nameFor(ctx: UnparseContext, x : EirNode): String = {
    x match {
      case n : EirNamedNode if n.name == "self" => "this"
      case _ => super.nameFor(ctx, x)
    }
  }

  override def visitFunctionArgument(ctx: UnparseContext, x: EirFunctionArgument): String = {
    s"${visit(ctx, x.declaredType)} ${nameFor(ctx, x)}"
  }

  override def visitTupleExpression(ctx: UnparseContext, x: EirTupleExpression): String = {
    val func = x.parent match {
      case Some(a : EirAssignment) if a.lval == x => "tie"
      case _ => "make_tuple"
    }
    s"std::$func(${visit(ctx, x) mkString ", "})"
  }

  override def visitLambdaExpression(ctx: UnparseContext, x: EirLambdaExpression): String = {
    val retTy = handleOption(ctx, x.foundType).getOrElse("")
    s"[=] (${visit(ctx, x.args) mkString ", "}) -> $retTy ${visit(ctx, x.body)}"
  }

  override def visitNew(ctx: UnparseContext, x: EirNew): String = {
    x.target match {
      case t: EirProxyType =>
        visit(ctx, t) + s"::ckNew(${visit(ctx, x.args) mkString ", "})"
      case _ => super.visitNew(ctx, x)
    }
  }
}
