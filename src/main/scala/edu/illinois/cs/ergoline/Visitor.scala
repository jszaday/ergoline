package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ErgolineParser._
import edu.illinois.cs.ergoline.ast.EirAccessibility.EirAccessibility
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.EirResolvable
import edu.illinois.cs.ergoline.types._
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichOption, RichParserRuleContext, RichResolvableTypeIterable}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.TerminalNode

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class Visitor extends ErgolineBaseVisitor[Any] {

  val parents: mutable.Stack[EirNode] = new mutable.Stack[EirNode]
  val defaultModuleName = "__default__"
  val defaultMemberAccessibility: EirAccessibility = EirAccessibility.Public

  parents.push(EirGlobalNamespace)

  def currentScope: Option[EirScope] = parents.headOption.flatMap(_.scope)

  override def visitProgram(ctx: ProgramContext): Any = {
    val ps = Option(ctx.packageStatement())
    val module = util.createOrGetNamespace(ps match {
      case Some(ps) => visitFqn(ps.fqn())
      case _ => List(defaultModuleName)
    }, currentScope)
    parents.push(module)
    module.children ++= ctx.mapOrEmpty(_.annotatedTopLevelStatement, visitAnnotatedTopLevelStatement)
    parents.pop()
  }

  override def visitTopLevelStatement(ctx: TopLevelStatementContext): EirNode =
    Option(ctx).map(super.visitTopLevelStatement).to[EirNode].orNull

  override def visitAnnotatedTopLevelStatement(ctx: AnnotatedTopLevelStatementContext): EirNode = {
    val s = visitTopLevelStatement(ctx.topLevelStatement())
    parents.push(s)
    s.annotations ++= visitAnnotationList(ctx.annotation())
    parents.pop()
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
    c.members ++= ctx.mapOrEmpty(_.annotatedMember, visitAnnotatedMember)
    parents.pop()
  }

  override def visitAnnotatedMember(ctx: AnnotatedMemberContext): EirMember = {
    parents.push(visitMember(ctx.member()))
    parents.head.annotations ++= visitAnnotationList(ctx.annotation())
    parents.pop().asInstanceOf[EirMember]
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
    val ns = util.createOrGetNamespace(visitFqn(ctx.fqn()), currentScope)
    parents.push(ns)
    ns.children ++= ctx.mapOrEmpty(_.annotatedTopLevelStatement, visitAnnotatedTopLevelStatement)
    parents.pop()
  }

  override def visitFunction(ctx: FunctionContext): EirFunction = {
    val f = EirFunction(parents.headOption, None, ctx.Identifier().getText, Nil, Nil)
    parents.push(f)
    f.templateArgs = visitTemplateDecl(ctx.templateDecl())
    f.functionArgs = visitFunctionArgumentList(ctx.functionArgumentList)
    f.body = visitBlock(ctx.block())
    parents.pop().asInstanceOf[EirFunction]
  }

  override def visitBlock(ctx: BlockContext): Option[EirBlock] = {
    if (ctx == null) return None
    val b = EirBlock(parents.headOption, Nil)
    parents.push(b)
    b.children = ctx.mapOrEmpty(_.statement, visitStatement)
    Some(parents.pop()).to[EirBlock]
  }

  override def visitStatement(ctx : StatementContext): EirNode = super.visitStatement(ctx).asInstanceOf[EirNode]

  override def visitTemplateDecl(ctx: TemplateDeclContext): List[EirTemplateArgument] =
    ctx.mapOrEmpty(_.templateDeclArg, visitTemplateDeclArg)

  override def visitTemplateDeclArg(ctx: TemplateDeclArgContext): EirTemplateArgument = {
    null
  }

  override def visitValueDeclaration(ctx: ValueDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = true)

  override def visitFieldValueDeclaration(ctx: FieldValueDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = true)

  override def visitVariableDeclaration(ctx: VariableDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = false)

  override def visitAssignmentStatement(ctx: AssignmentStatementContext): EirNode = {
    val a = EirAssignment(parents.headOption, null, null)
    parents.push(a)
    a.target = visitPostfixExpression(ctx.postfixExpression())
    a.value = visitExpression(ctx.expression())
    parents.pop()
  }

  override def visitPostfixExpression(ctx: PostfixExpressionContext): EirExpressionNode =
    Option(ctx).map(super.visitPostfixExpression).to[EirExpressionNode].orNull

  def visitDeclaration(name: TerminalNode, declaredType: TypeContext, expressionContext: ExpressionContext, isFinal: Boolean): EirNode = {
    val d = EirDeclaration(parents.headOption, isFinal, name.getText, visitType(declaredType), None)
    parents.push(d)
    d.initialValue = Option(expressionContext).map(visitExpression)
    parents.pop()
  }

  override def visitExpression(ctx: ExpressionContext): EirExpressionNode =
    super.visitExpression(ctx).asInstanceOf[EirExpressionNode]

  override def visitType(ctx: TypeContext): EirResolvable[EirType] = super.visitType(ctx).asInstanceOf[EirResolvable[EirType]]

  override def visitTypeList(ctx: TypeListContext): List[EirResolvable[EirType]] = ctx.mapOrEmpty(_.`type`, visitType)

  override def visitTupleType(ctx: TupleTypeContext): EirResolvable[EirType] =
    visitTypeList(ctx.typeList()).toTupleType

  override def visitBasicType(ctx: BasicTypeContext): EirResolvable[EirType] = {
    var base: EirResolvable[EirType] = EirResolvable.fromName(visitFqn(ctx.fqn))
    val templates = visitTypeList(ctx.typeList())
    if (templates.nonEmpty) {
      base = EirTemplatedType(base, templates)
    }
    if (ctx.Atpersand() != null) {
      base = EirProxyType(base, Option(ctx.CollectiveKeyword()).map(_.getText))
    }
    base
  }

  override def visitMultiplicativeExpression(ctx: MultiplicativeExpressionContext): Any = visitBinaryExpression(ctx)

  override def visitAdditiveExpression(ctx: AdditiveExpressionContext): Any = visitBinaryExpression(ctx)

  override def visitShiftExpression(ctx: ShiftExpressionContext): Any = visitBinaryExpression(ctx)

  override def visitRelationalExpression(ctx: RelationalExpressionContext): Any = visitBinaryExpression(ctx)

  override def visitEqualityExpression(ctx: EqualityExpressionContext): Any = visitBinaryExpression(ctx)

  override def visitAndExpression(ctx: AndExpressionContext): Any = visitBinaryExpression(ctx)

  override def visitExclusiveOrExpression(ctx: ExclusiveOrExpressionContext): Any = visitBinaryExpression(ctx)

  override def visitInclusiveOrExpression(ctx: InclusiveOrExpressionContext): Any = visitBinaryExpression(ctx)

  override def visitLogicalAndExpression(ctx: LogicalAndExpressionContext): Any = visitBinaryExpression(ctx)

  override def visitLogicalOrExpression(ctx: LogicalOrExpressionContext): Any = visitBinaryExpression(ctx)

  def visitBinaryExpression[T <: ParserRuleContext](ctx: T): EirExpressionNode = {
    val children = ctx.children.asScala.toList
    if (children.length == 1) {
      visit(children.head) match {
        case e: EirExpressionNode => e
        case _ => null
      }
    }
    else if (children.length == 3) {
      val e = EirBinaryExpression(parents.headOption, null, children(1).getText, null)
      parents.push(e)
      e.lhs = visit(children.head).asInstanceOf[EirExpressionNode]
      e.rhs = visit(children.last).asInstanceOf[EirExpressionNode]
      parents.pop().asInstanceOf[EirExpressionNode]
    } else throw new RuntimeException("how did I get here?")
  }

  override def visitExpressionList(ctx: ExpressionListContext): List[EirExpressionNode] =
    ctx.mapOrEmpty(_.expression, visitExpression)

  override def visitFunctionArgumentList(ctx: FunctionArgumentListContext): List[EirFunctionArgument] =
    ctx.mapOrEmpty(_.functionArgument, visitFunctionArgument)

  override def visitTupleExpression(ctx: TupleExpressionContext): EirExpressionNode =
    EirTupleExpression.fromExpressions(parents.headOption, visitExpressionList(ctx.expressionList()))

  override def visitFunctionArgument(ctx: FunctionArgumentContext): EirFunctionArgument = {
    val arg = EirFunctionArgument(parents.headOption, ctx.Identifier.getText, null,
      isFinal = Option(ctx.VariableKeyword()).isEmpty,
      isSelfAssigning = Option(ctx.Equals()).isDefined)
    arg.declaredType = visitType(ctx.`type`())
    arg
  }
}
