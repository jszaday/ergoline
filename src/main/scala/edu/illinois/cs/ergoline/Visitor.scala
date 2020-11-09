package edu.illinois.cs.ergoline

import scala.jdk.CollectionConverters._

class Visitor extends ErgolineBaseVisitor[Any] {

  import ast.{EirGlobalNamespace => modules}
  import ast._

  val defaultModuleName = "__default__"

  /**
   * {@inheritDoc }
   *
   * <p>The default implementation returns the result of calling
   * {@link #visitChildren} on {@code ctx}.</p>
   */
  override def visitProgram(ctx: ErgolineParser.ProgramContext): Any = {
    val ps = Option(ctx.packageStatement())
    val idents = ps match {
      case Some(ps) => ps.fqn().Identifier().asScala.map(_.getText).toList
      case _ => List(defaultModuleName)
    }
    var module = {
      if (modules.contains(idents.head)) modules(idents.head).get.asInstanceOf[EirNamespace]
      else {
        val ns = EirNamespace(Some(modules), Nil, idents.head)
        modules.put(idents.head, ns)
        ns
      }
    }
    for (ident <- if (idents.length > 1) idents.tail else Nil) {
      val child = EirNamespace(Some(module), Nil, ident)
      module.children ++= List(child)
      module = child
    }
    ctx.importStatement().asScala.foreach(this.visitImportStatement)
    module.children = ctx.programStatement.asScala.map(this.visitProgramStatement).map(_.asInstanceOf[EirNode]).toList
    module
  }

  /**
   * {@inheritDoc }
   *
   * <p>The default implementation returns the result of calling
   * {@link #visitChildren} on {@code ctx}.</p>
   */
  override def visitImportStatement(ctx: ErgolineParser.ImportStatementContext): Any = {
    println("importing the module " + ctx.fqn().Identifier().asScala.map(_.getText))
  }

  override def visitProgramStatement(ctx: ErgolineParser.ProgramStatementContext): Any = {
    if (ctx.namespace() != null) visitNamespace(ctx.namespace())
    else if (ctx.classDeclaration() != null) visitClassDeclaration(ctx.classDeclaration())
    else if (ctx.function() != null) visitFunction(ctx.function())
  }

  override def visitFunction(ctx: ErgolineParser.FunctionContext): Any = {
    println("visiting function " + ctx.Identifier().getText)
  }
}
