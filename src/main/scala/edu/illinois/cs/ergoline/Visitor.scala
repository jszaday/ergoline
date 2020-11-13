package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ErgolineParser._
import edu.illinois.cs.ergoline.ast.EirAccessibility.EirAccessibility
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.EirResolvable
import edu.illinois.cs.ergoline.types._
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichOption, RichParserRuleContext, RichResolvableTypeIterable}
import org.antlr.v4.runtime.{ParserRuleContext, Token}
import org.antlr.v4.runtime.tree.{ParseTree, TerminalNode}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class Visitor(global: EirNode = EirGlobalNamespace) extends ErgolineBaseVisitor[Any] {

  val parents: mutable.Stack[EirNode] = new mutable.Stack[EirNode]
  val defaultModuleName = "__default__"
  val defaultMemberAccessibility: EirAccessibility = EirAccessibility.Public

  parents.push(global)

  override def visitProgram(ctx: ProgramContext): Any = {
    val module = visitPackageStatement(ctx.packageStatement())
    parents.push(module)
    module.children ++= ctx.mapOrEmpty(_.annotatedTopLevelStatement, visitAnnotatedTopLevelStatement)
    parents.pop()
  }

  override def visitPackageStatement(ctx: PackageStatementContext): EirNamespace = {
    val opt : Option[List[String]] = Option(ctx).map(_.fqn()).map(visitFqn)
    util.createOrGetNamespace(opt.getOrElse(List(defaultModuleName)), currentScope)
  }

  override def visitImportStatement(ctx: ImportStatementContext): Any = {
    println("importing the module " + ctx.fqn().Identifier().asScala.map(_.getText))
  }

  override def visitClassDeclaration(ctx: ClassDeclarationContext): EirClass = {
    val c: EirClass = EirClass(parents.headOption, Nil, ctx.Identifier().getText, Nil, None, Nil)
    parents.push(c)
    c.members ++= ctx.mapOrEmpty(_.annotatedMember, visitAnnotatedMember)
    pop[EirClass]()
  }

  override def visitAnnotatedMember(ctx: AnnotatedMemberContext): EirMember = {
    parents.push(visitMember(ctx.member()))
    parents.head.annotations ++= visitAnnotationList(ctx.annotation())
    pop[EirMember]()
  }

  def visitAnnotationList(annotations: java.util.List[AnnotationContext]): Iterable[EirAnnotation] =
    annotations.asScala.map(this.visitAnnotation)

  override def visitAnnotation(ctx: AnnotationContext): EirAnnotation =
    EirAnnotation(parents.headOption, ctx.Identifier().getText)

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

  private def currentScope: Option[EirScope] = parents.headOption.flatMap(_.scope)

  override def visitAnnotatedTopLevelStatement(ctx: AnnotatedTopLevelStatementContext): EirNode = {
    val s = visitTopLevelStatement(ctx.topLevelStatement())
    parents.push(s)
    s.annotations ++= visitAnnotationList(ctx.annotation())
    parents.pop()
  }

  override def visitTopLevelStatement(ctx: TopLevelStatementContext): EirNode =
    Option(ctx).map(super.visitTopLevelStatement).to[EirNode].orNull

  override def visitFunction(ctx: FunctionContext): EirFunction = {
    val f = EirFunction(parents.headOption, None, ctx.Identifier().getText, Nil, Nil)
    parents.push(f)
    f.templateArgs = visitTemplateDecl(ctx.templateDecl())
    f.functionArgs = visitFunctionArgumentList(ctx.functionArgumentList)
    f.body = visitBlock(ctx.block())
    pop[EirFunction]()
  }

  override def visitBlock(ctx: BlockContext): Option[EirBlock] = {
    if (ctx == null) return None
    val b = EirBlock(parents.headOption, Nil)
    parents.push(b)
    b.children = ctx.mapOrEmpty(_.statement, visitStatement)
    Some(parents.pop()).to[EirBlock]
  }

  override def visitStatement(ctx: StatementContext): EirNode = super.visitStatement(ctx).asInstanceOf[EirNode]

  override def visitTemplateDecl(ctx: TemplateDeclContext): List[EirTemplateArgument] =
    ctx.mapOrEmpty(_.templateDeclArg, visitTemplateDeclArg)

  override def visitTemplateDeclArg(ctx: TemplateDeclArgContext): EirTemplateArgument = ???

  override def visitFunctionArgumentList(ctx: FunctionArgumentListContext): List[EirFunctionArgument] =
    ctx.mapOrEmpty(_.functionArgument, visitFunctionArgument)

  // TODO fix parent resolution
  override def visitFunctionArgument(ctx: FunctionArgumentContext): EirFunctionArgument = {
    val arg = EirFunctionArgument(parents.headOption, ctx.Identifier.getText, null,
      isFinal = Option(ctx.VariableKeyword()).isEmpty,
      isSelfAssigning = Option(ctx.Equals()).isDefined)
    arg.declaredType = visitType(ctx.`type`())
    arg
  }

  override def visitType(ctx: TypeContext): EirResolvable[EirType] = super.visitType(ctx).asInstanceOf[EirResolvable[EirType]]

  override def visitValueDeclaration(ctx: ValueDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = true)

  override def visitFieldValueDeclaration(ctx: FieldValueDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = true)

  override def visitVariableDeclaration(ctx: VariableDeclarationContext): Any = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = false)

  def visitDeclaration(name: TerminalNode, declaredType: TypeContext, expressionContext: ExpressionContext, isFinal: Boolean): EirNode = {
    val d = EirDeclaration(parents.headOption, isFinal, name.getText, visitType(declaredType), None)
    parents.push(d)
    d.initialValue = Option(expressionContext).map(visitExpression)
    parents.pop()
  }

  override def visitAssignmentStatement(ctx: AssignmentStatementContext): EirNode = {
    val a = EirAssignment(parents.headOption, null, null)
    parents.push(a)
    a.target = visitPostfixExpression(ctx.postfixExpression())
    a.value = visitExpression(ctx.expression())
    parents.pop()
  }

  override def visitPostfixExpression(ctx: PostfixExpressionContext): EirExpressionNode =
    Option(ctx).map(super.visitPostfixExpression).to[EirExpressionNode].orNull

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

  override def visitFqn(ctx: FqnContext): List[String] =
    ctx.Identifier().asScala.map(_.getText).toList

  def visitFqn[T : Manifest](ctx: FqnContext): EirResolvable[T] =
    EirResolvable.fromName(visitFqn(ctx))

  override def visitTypeList(ctx: TypeListContext): List[EirResolvable[EirType]] = ctx.mapOrEmpty(_.`type`, visitType)

  override def visitMultiplicativeExpression(ctx: MultiplicativeExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitAdditiveExpression(ctx: AdditiveExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitShiftExpression(ctx: ShiftExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitRelationalExpression(ctx: RelationalExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitEqualityExpression(ctx: EqualityExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

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
      pop[EirExpressionNode]()
    } else throw new RuntimeException("how did I get here?")
  }

  private def pop[T](): T = parents.pop().asInstanceOf[T]

  override def visitAndExpression(ctx: AndExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitExclusiveOrExpression(ctx: ExclusiveOrExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitInclusiveOrExpression(ctx: InclusiveOrExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitLogicalAndExpression(ctx: LogicalAndExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitLogicalOrExpression(ctx: LogicalOrExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitTupleExpression(ctx: TupleExpressionContext): EirExpressionNode =
    EirTupleExpression.fromExpressions(parents.headOption, visitExpressionList(ctx.expressionList()))

  override def visitExpressionList(ctx: ExpressionListContext): List[EirExpressionNode] =
    ctx.mapOrEmpty(_.expression, visitExpression)

  override def visitLambdaExpression(ctx: LambdaExpressionContext): EirExpressionNode = {
    val f = EirLambdaExpression(parents.headOption, null, null)
    parents.push(f)
    f.args = visitFunctionArgumentList(ctx.functionArgumentList())
    f.body = visitBlock(ctx.block()).getOrElse(util.encloseExpression(visitExpression(ctx.expression)))
    pop[EirExpressionNode]()
  }

  override def visitConditionalExpression(ctx: ConditionalExpressionContext): EirExpressionNode = {
    if (ctx.expression() == null) return visitLogicalOrExpression(ctx.logicalOrExpression())
    val e = EirTernaryOperator(parents.headOption, null, null, null)
    parents.push(e)
    e.test = visitLogicalOrExpression(ctx.logicalOrExpression())
    e.ifTrue = visitExpression(ctx.expression())
    e.ifFalse = visitConditionalExpression(ctx.conditionalExpression())
    pop[EirExpressionNode]()
  }

  override def visitUnaryExpression(ctx: UnaryExpressionContext): EirExpressionNode = {
    if (ctx.unaryOperator() == null) visitPostfixExpression(ctx.postfixExpression())
    else {
      val e = EirUnaryExpression(parents.headOption, ctx.unaryOperator().getText, null)
      parents.push(e)
      e.rhs = visitCastExpression(ctx.castExpression())
      pop[EirExpressionNode]()
    }
  }

  override def visitCastExpression(ctx: CastExpressionContext): EirExpressionNode =
    super.visitCastExpression(ctx).asInstanceOf[EirExpressionNode]

  override def visitReturnStatement(ctx: ReturnStatementContext): EirNode = {
    val ret = EirReturn(parents.headOption, null)
    parents.push(ret)
    ret.expression = visitExpression(ctx.expression())
    parents.pop()
  }

  override def visitInheritanceDecl(ctx: InheritanceDeclContext): Any = {
    val base : EirInheritable = parents.headOption.to[EirInheritable].get
    val children : Iterable[ParseTree] = ctx.children.asScala
    for (List(kwd, ty) <- children.sliding(2)) {
      kwd.getText match {
        case "extends" => base.extendsThis = Some(visitType(ty.asInstanceOf[TypeContext]))
        case "implements" | "and" => base.implementsThese ++= List(visitType(ty.asInstanceOf[TypeContext]))
      }
    }
  }

  override def visitForLoop(ctx: ForLoopContext): Any = ???

  override def visitLambdaType(ctx: LambdaTypeContext): EirType = {
    val children : List[ParseTree] = ctx.children.asScala.toList
    val from = children.head match {
      case x : TupleTypeContext => visitTupleType(x) match {
        case x : EirTupleType => x.elements
        case x => List(x)
      }
      case x : BasicTypeContext => List(visitBasicType(x))
    }
    val to = children.last match {
      case x : TupleTypeContext => visitTupleType(x)
      case x : BasicTypeContext => visitBasicType(x)
    }
    EirLambdaType(from, to)
  }

  override def visitExpression(ctx: ExpressionContext): EirExpressionNode =
    super.visitExpression(ctx).asInstanceOf[EirExpressionNode]

  override def visitPrimaryExpression(ctx: PrimaryExpressionContext): Any = {
    if (ctx.fqn() != null) EirIdentifier(parents.headOption, visitFqn(ctx.fqn()))
    else super.visitPrimaryExpression(ctx)
  }

  override def visitConstant(ctx: ConstantContext): EirExpressionNode = {
    if (ctx.IntegerConstant() != null) {
      EirLiteral(parents.headOption, "integer", ctx.IntegerConstant().getText)
    } else if (ctx.FloatingConstant() != null) {
      EirLiteral(parents.headOption, "float", ctx.FloatingConstant().getText)
    } else if (Option(ctx.StringLiteral()).exists(_.size() >= 1)) {
      EirLiteral(parents.headOption, "string", ctx.StringLiteral().asScala.map(_.getText).reduce(_ + _))
    } else {
      assert(ctx.CharacterConstant() != null)
      EirLiteral(parents.headOption, "char", ctx.CharacterConstant().getText)
    }
  }

//  override def visitChildren(ruleNode: RuleNode): Any = ???
//  override def visitErrorNode(errorNode: ErrorNode): Any = ???
}
