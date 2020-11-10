package edu.illinois.cs.ergoline

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import org.antlr.v4.runtime.tree.TerminalNode

import ast._
import ast.{EirGlobalNamespace => modules, _}
import ErgolineParser._

class Visitor extends ErgolineBaseVisitor[Any] {

  val parents: mutable.Stack[EirNode] = new mutable.Stack[EirNode]
  val defaultModuleName = "__default__"
  val defaultMemberAccessibility: EirAccessibility.Value = EirAccessibility.Private

  def getModule(fqn: Iterable[String], scope: EirScope = EirGlobalNamespace): EirNamespace = {
    fqn.foldLeft[EirScope](scope)((parent, name) => {
      parent(name) match {
        case Some(x: EirNamespace) => x
        case _ =>
          val child = EirNamespace(Some(parent), Nil, name)
          parent match {
            case EirGlobalNamespace =>
              modules.put(name, child)
            case x: EirNamespace =>
              x.children ++= List(child)
            case _ => throw new RuntimeException("unacceptable module target")
          }
          child
      }
    }).asInstanceOf[EirNamespace]
  }

  /**
   * {@inheritDoc }
   *
   * <p>The default implementation returns the result of calling
   * {@link #visitChildren} on {@code ctx}.</p>
   */
  override def visitProgram(ctx: ProgramContext): Any = {
    val ps = Option(ctx.packageStatement())
    val module = getModule(ps match {
      case Some(ps) => visitFqn(ps.fqn())
      case _ => List(defaultModuleName)
    })
    parents.push(module)
    ctx.importStatement().asScala.foreach(this.visitImportStatement)
    module.children ++= visitTopLevelStatementList(ctx.topLevelStatement)
    parents.pop()
  }

  def visitTopLevelStatementList(statements: java.util.List[TopLevelStatementContext]): Iterable[EirNode] =
    statements.asScala.map(this.visitTopLevelStatement).map(_.asInstanceOf[EirNode])

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

  override def visitNamespace(ctx: NamespaceContext): Any = {
    val ns = getModule(visitFqn(ctx.fqn()))
    parents.push(ns)
    ns.children ++= visitTopLevelStatementList(ctx.topLevelStatement())
    parents.pop()
  }

  override def visitMember(ctx: MemberContext): Any = {
    EirMember(parents.headOption, super.visitMember(ctx).asInstanceOf[EirNamedNode],
      Option(ctx.accessModifier()).map(_.getText.capitalize).map(EirAccessibility.withName).getOrElse(defaultMemberAccessibility))
  }

  override def visitBlock(ctx: BlockContext): Any = Option(ctx) match {
    case Some(ctx) => ctx.statement().asScala.map(f => visit(f).asInstanceOf[EirNode]).toList
    case _ => Nil
  }

  override def visitFunction(ctx: FunctionContext): Any = {
    val f = EirFunction(parents.headOption, Nil, ctx.Identifier().getText, Nil)
    parents.push(f)
    f.children = visitBlock(ctx.block()).asInstanceOf[List[EirNode]]
    parents.pop()
  }

  override def visitAnnotation(ctx: AnnotationContext): Any =
    EirAnnotation(parents.headOption, ctx.Identifier().getText)

  def visitDeclaration(name: TerminalNode, declaredType: TypeContext, expressionContext: ExpressionContext, isFinal: Boolean): EirNode = {
    val d = EirDeclaration(parents.headOption, isFinal, name.getText, visitType(declaredType).asInstanceOf[EirResolvable[EirType]], None)
    parents.push(d)
    d.initialValue = Option(expressionContext).map(visitExpression(_).asInstanceOf[EirExpressionNode])
    parents.pop()
  }

  override def visitValueDeclaration(ctx: ValueDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = false)

  override def visitVariableDeclaration(ctx: VariableDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = true)

  override def visitExpression(ctx: ExpressionContext): Any = null

  override def visitBasicType(ctx: BasicTypeContext): Any = {
    EirResolvable[EirType](visitFqn(ctx.fqn()))
  }

  override def visitFqn(ctx: FqnContext): Iterable[String] =
    ctx.Identifier().asScala.map(_.getText)
}
