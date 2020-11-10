package edu.illinois.cs.ergoline

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import org.antlr.v4.runtime.tree.TerminalNode
import ast._
import ast.{EirGlobalNamespace => modules, _}
import ErgolineParser._
import edu.illinois.cs.ergoline.ast.EirAccessibility.EirAccessibility
import edu.illinois.cs.ergoline.types._
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichAllowedIterable, RichType}
import org.antlr.v4.runtime.ParserRuleContext

class Visitor extends ErgolineBaseVisitor[Any] {

  val parents: mutable.Stack[EirNode] = new mutable.Stack[EirNode]
  val defaultModuleName = "__default__"
  val defaultMemberAccessibility: EirAccessibility = EirAccessibility.Public

  implicit def scope: EirScope = parents.headOption match {
    case Some(x: EirScopedNode) => x.scope.orNull
    case _ => null
  }

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
      .map(this.visit).foreach(x => m.member = x.asInstanceOf[EirNamedNode])
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

  override def visitFunction(ctx: FunctionContext): EirFunction = {
    val f = EirFunction(parents.headOption, Nil, ctx.Identifier().getText, Nil, visitFunctionArgumentList(ctx.functionArgumentList))
    parents.push(f)
    f.children = visitBlock(ctx.block())
    parents.pop().asInstanceOf[EirFunction]
  }

  override def visitBlock(ctx: BlockContext): List[EirNode] = Option(ctx) match {
    case Some(ctx) => ctx.statement().asScala.map(f => visit(f).asInstanceOf[EirNode]).toList
    case _ => Nil
  }

  override def visitValueDeclaration(ctx: ValueDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = true)

  override def visitFieldValueDeclaration(ctx: FieldValueDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = true)

  override def visitVariableDeclaration(ctx: VariableDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = false)

  def visitDeclaration(name: TerminalNode, declaredType: TypeContext, expressionContext: ExpressionContext, isFinal: Boolean): EirNode = {
    val d = EirDeclaration(parents.headOption, isFinal, name.getText, visitType(declaredType), None)
    parents.push(d)
    d.initialValue = Option(expressionContext).map(visitExpression)
    parents.pop()
  }

  override def visitExpression(ctx: ExpressionContext): EirExpressionNode =
    super.visitExpression(ctx).asInstanceOf[EirExpressionNode]

  override def visitType(ctx: TypeContext): Allowed = super.visitType(ctx).asInstanceOf[Allowed]

  override def visitTypeList(ctx: TypeListContext): Iterable[Allowed] = {
    Option(ctx).map(_.`type`.asScala).getOrElse(Nil).map(visitType)
  }

  override def visitTupleType(ctx: TupleTypeContext): Allowed =
    visitTypeList(ctx.typeList()).asType

  override def visitBasicType(ctx: BasicTypeContext): Allowed = {
    var base: EirType = EirResolvableType.fromName(visitFqn(ctx.fqn))
    val templates = visitTypeList(ctx.typeList()).toList
    if (templates.nonEmpty) {
      base = EirTemplatedType(base.asAllowed, templates)
    }
    if (ctx.Atpersand() != null) {
      base = EirProxyType(base.asAllowed, Option(ctx.CollectiveKeyword()).map(_.getText))
    }
    base.asAllowed
  }

  override def visitMultiplicativeExpression(ctx : MultiplicativeExpressionContext): Any = visitBinaryExpression(ctx)
  override def visitAdditiveExpression(ctx : AdditiveExpressionContext): Any = visitBinaryExpression(ctx)
  override def visitShiftExpression(ctx : ShiftExpressionContext): Any = visitBinaryExpression(ctx)
  override def visitRelationalExpression(ctx : RelationalExpressionContext): Any = visitBinaryExpression(ctx)
  override def visitEqualityExpression(ctx : EqualityExpressionContext): Any = visitBinaryExpression(ctx)
  override def visitAndExpression(ctx : AndExpressionContext): Any = visitBinaryExpression(ctx)
  override def visitExclusiveOrExpression(ctx : ExclusiveOrExpressionContext): Any = visitBinaryExpression(ctx)
  override def visitInclusiveOrExpression(ctx : InclusiveOrExpressionContext): Any = visitBinaryExpression(ctx)
  override def visitLogicalAndExpression(ctx : LogicalAndExpressionContext): Any = visitBinaryExpression(ctx)
  override def visitLogicalOrExpression(ctx : LogicalOrExpressionContext): Any = visitBinaryExpression(ctx)

  def visitBinaryExpression[T <: ParserRuleContext](ctx: T): EirExpressionNode = {
    val children = ctx.children.asScala.toList
    if (children.length == 1) visit(children.head).asInstanceOf[EirExpressionNode]
    else if (children.length == 3) {
      val e = EirBinaryExpression(parents.headOption, null, children(1).getText, null)
      parents.push(e)
      e.lhs = visit(children.head).asInstanceOf[EirExpressionNode]
      e.rhs = visit(children.last).asInstanceOf[EirExpressionNode]
      parents.pop().asInstanceOf[EirExpressionNode]
    } else throw new RuntimeException("how did I get here?")
  }

  override def visitFunctionArgumentList(ctx: FunctionArgumentListContext): List[EirFunctionArgument] =
    Option(ctx).map(_.functionArgument).map(_.asScala).getOrElse(Nil).map(visitFunctionArgument).toList

  override def visitFunctionArgument(ctx: FunctionArgumentContext): EirFunctionArgument = {
    EirFunctionArgument(parents.headOption, ctx.Identifier.getText, visitType(ctx.`type`()),
      isFinal = Option(ctx.VariableKeyword()).isEmpty,
      isSelfAssigning = Option(ctx.Equals()).isDefined)
  }
}
