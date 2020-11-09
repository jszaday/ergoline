package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.EirAccessibility

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class Visitor extends ErgolineBaseVisitor[Any] {

  import ast.{EirGlobalNamespace => modules}
  import ast._
  import ErgolineParser._

  val parents: mutable.Stack[EirNode] = new mutable.Stack[EirNode]
  val defaultModuleName = "__default__"
  val defaultMemberAccessibility: EirAccessibility.Value = EirAccessibility.Private

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
    c.members ++= ctx.annotatedMember().asScala.map(ctx => {
      parents.push(visitMember(ctx.member()).asInstanceOf[EirNode])
      parents.head.annotations ++= ctx.annotation().asScala.map(this.visitAnnotation).map(_.asInstanceOf[EirAnnotation])
      parents.pop().asInstanceOf[EirMember]
    })
    parents.pop()
  }

  override def visitMember(ctx: MemberContext): Any = {
    EirMember(parents.headOption, super.visitMember(ctx).asInstanceOf[EirNamedNode],
      Option(ctx.accessModifier()).map(_.getText.capitalize).map(EirAccessibility.withName).getOrElse(defaultMemberAccessibility))
  }

  override def visitFunction(ctx: FunctionContext): Any = {
    EirFunction(parents.headOption, Nil, ctx.Identifier().getText, Nil)
  }

  override def visitAnnotation(ctx: AnnotationContext): Any =
    EirAnnotation(parents.headOption, ctx.Identifier().getText)
}
