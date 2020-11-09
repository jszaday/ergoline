package edu.illinois.cs.ergoline

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class Visitor extends ErgolineBaseVisitor[Any] {

  import ast.{EirGlobalNamespace => modules}
  import ast._
  import ErgolineParser._

  val parents: mutable.Stack[EirNode] = new mutable.Stack[EirNode]
  val defaultModuleName = "__default__"

  /**
   * {@inheritDoc }
   *
   * <p>The default implementation returns the result of calling
   * {@link #visitChildren} on {@code ctx}.</p>
   */
  override def visitProgram(ctx: ProgramContext): Any = {
    val ps = Option(ctx.packageStatement())
    val idents = ps match {
      case Some(ps) => ps.fqn().Identifier().asScala.map(_.getText).toList
      case _ => List(defaultModuleName)
    }
    parents.push(modules)
    for (ident <- idents) {
      val child = EirNamespace(parents.headOption, Nil, ident)
      parents.head match {
        case EirGlobalNamespace => modules.put(ident, child)
        case x: EirNamespace => x.children ++= List(child)
        case _ => throw new RuntimeException("unacceptable module target")
      }
      parents.push(child)
    }
    val module = parents.head.asInstanceOf[EirNamespace]
    ctx.importStatement().asScala.foreach(this.visitImportStatement)
    module.children ++= ctx.programStatement.asScala.map(this.visitProgramStatement).map(_.asInstanceOf[EirNode]).toList
  }

  /**
   * {@inheritDoc }
   *
   * <p>The default implementation returns the result of calling
   * {@link #visitChildren} on {@code ctx}.</p>
   */
  override def visitImportStatement(ctx: ImportStatementContext): Any = {
    println("importing the module " + ctx.fqn().Identifier().asScala.map(_.getText))
  }

  override def visitClassDeclaration(ctx: ClassDeclarationContext): Any = {
    val c: EirClass = EirClass(parents.headOption, Nil, ctx.Identifier().getText, Nil, None, Nil)
    parents.push(c)
    for (member <- ctx.annotatedMember().asScala) {
      val visited = visitMember(member.member()).asInstanceOf[EirNode]
      parents.push(visited)
      visited.annotations ++= member.annotation().asScala.map(this.visitAnnotation).map(_.asInstanceOf[EirAnnotation])
      c.members ++= List(parents.pop().asInstanceOf[EirMember])
    }
    println("visiting class " + c)
    parents.pop()
  }

  override def visitMember(ctx: MemberContext): Any = {
    val accessModifier = Option(ctx.accessModifier()).map(_.getText).getOrElse("private")
    EirMember(parents.headOption, super.visitMember(ctx).asInstanceOf[EirNamedNode],
      EirAccessibility.withName(accessModifier.capitalize))
  }

  override def visitFunction(ctx: FunctionContext): Any = {
    EirFunction(parents.headOption, Nil, ctx.Identifier().getText, Nil)
  }

  override def visitAnnotation(ctx: AnnotationContext): Any =
    EirAnnotation(parents.headOption, ctx.Identifier().getText)
}
