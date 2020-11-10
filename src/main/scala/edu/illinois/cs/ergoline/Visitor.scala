package edu.illinois.cs.ergoline

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import org.antlr.v4.runtime.tree.TerminalNode
import ast._
import ast.{EirGlobalNamespace => modules, _}
import ErgolineParser._
import edu.illinois.cs.ergoline.ast.EirAccessibility.EirAccessibility
import edu.illinois.cs.ergoline.types.EirType
import edu.illinois.cs.ergoline.util.EirResolvable

class Visitor extends ErgolineBaseVisitor[Any] {

  val parents: mutable.Stack[EirNode] = new mutable.Stack[EirNode]
  val defaultModuleName = "__default__"
  val defaultMemberAccessibility: EirAccessibility = EirAccessibility.Public

  override def visitProgram(ctx: ProgramContext): Any = {
    val ps = Option(ctx.packageStatement())
    val module = getModule(ps match {
      case Some(ps) => visitFqn(ps.fqn())
      case _ => List(defaultModuleName)
    })
    parents.push(module)
    module.children ++= visitStatementList(ctx.annotatedTopLevelStatement())
    parents.pop()
  }

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

  def visitStatementList(statements: java.util.List[AnnotatedTopLevelStatementContext]): Iterable[EirNode] =
    statements.asScala.map(this.visitAnnotatedTopLevelStatement)

  override def visitAnnotatedTopLevelStatement(ctx: AnnotatedTopLevelStatementContext): EirNode =
    Option(visit(ctx.topLevelStatement())) match {
      case Some(s: EirNode) =>
        parents.push(s)
        s.annotations ++= visitAnnotationList(ctx.annotation())
        parents.pop()
      case _ => null
    }

  def visitAnnotationList(annotations: java.util.List[AnnotationContext]): Iterable[EirAnnotation] =
    annotations.asScala.map(this.visitAnnotation)

  override def visitAnnotation(ctx: AnnotationContext): EirAnnotation =
    EirAnnotation(parents.headOption, ctx.Identifier().getText)

  override def visitFqn(ctx: FqnContext): Iterable[String] =
    ctx.Identifier().asScala.map(_.getText)

  override def visitImportStatement(ctx: ImportStatementContext): Any = {
    println("importing the module " + ctx.fqn().Identifier().asScala.map(_.getText))
  }

  override def visitClassDeclaration(ctx: ClassDeclarationContext): Any = {
    val c: EirClass = EirClass(parents.headOption, Nil, ctx.Identifier().getText, Nil, None, Nil)
    parents.push(c)
    c.members ++= ctx.annotatedMember().asScala.map(ctx => {
      parents.push(visitMember(ctx.member()))
      parents.head.annotations ++= visitAnnotationList(ctx.annotation())
      parents.pop().asInstanceOf[EirMember]
    })
    parents.pop()
  }

  override def visitMember(ctx: MemberContext): EirNode = {
    val m = EirMember(parents.headOption, null, visitAccessModifier(ctx.accessModifier))
    parents.push(m)
    Option(ctx.fieldDeclaration())
      .orElse(Option(ctx.topLevelStatement()))
      .map(this.visit).foreach(x => x.asInstanceOf[EirNamedNode])
    parents.pop()
  }

  override def visitAccessModifier(ctx: AccessModifierContext): EirAccessibility =
    Option(ctx).map(_.getText.capitalize).map(EirAccessibility.withName).getOrElse(defaultMemberAccessibility)

  override def visitNamespace(ctx: NamespaceContext): Any = {
    val ns = getModule(visitFqn(ctx.fqn()))
    parents.push(ns)
    ns.children ++= visitStatementList(ctx.annotatedTopLevelStatement())
    parents.pop()
  }

  override def visitFunction(ctx: FunctionContext): Any = {
    val f = EirFunction(parents.headOption, Nil, ctx.Identifier().getText, Nil)
    parents.push(f)
    f.children = visitBlock(ctx.block())
    parents.pop()
  }

  override def visitBlock(ctx: BlockContext): List[EirNode] = Option(ctx) match {
    case Some(ctx) => ctx.statement().asScala.map(f => visit(f).asInstanceOf[EirNode]).toList
    case _ => Nil
  }

  override def visitValueDeclaration(ctx: ValueDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = true)

  override def visitFieldValueDeclaration(ctx: FieldValueDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = true)

  override def visitVariableDeclaration(ctx: VariableDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = false)

  def visitDeclaration(name: TerminalNode, declaredType: TypeContext, expressionContext: ExpressionContext, isFinal: Boolean): EirNode = {
    val d = EirDeclaration(parents.headOption, isFinal, name.getText, visitType(declaredType).asInstanceOf[EirResolvable[EirType]], None)
    parents.push(d)
    d.initialValue = Option(expressionContext).map(visitExpression)
    parents.pop()
  }

  override def visitExpression(ctx: ExpressionContext): EirExpressionNode =
    super.visitExpression(ctx).asInstanceOf[EirExpressionNode]

  override def visitBasicType(ctx: BasicTypeContext): Any = {
    EirResolvable[EirType](visitFqn(ctx.fqn()))
  }
}
