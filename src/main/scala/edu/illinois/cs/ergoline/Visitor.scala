package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ErgolineParser._
import edu.illinois.cs.ergoline.ast.EirAccessibility.EirAccessibility
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Modules}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichEirNode, RichOption, RichParserRuleContext, RichResolvableTypeIterable}
import edu.illinois.cs.ergoline.util.assertValid
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ParseTree, TerminalNode}

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import java.io.File
import java.nio.file.Files

class Visitor(global: EirScope = EirGlobalNamespace) extends ErgolineBaseVisitor[Any] {

  val parents: mutable.Stack[EirNode] = new mutable.Stack[EirNode]
  val defaultModuleName = "__default__"
  val defaultMemberAccessibility: EirAccessibility = EirAccessibility.Public

  object VisitorSyntax {
    implicit class RichTerminalNodeList(list: java.util.List[TerminalNode]) {
      implicit def toStringList: List[String] = list.asScala.map(_.getText).toList
    }
  }

  import VisitorSyntax.RichTerminalNodeList

  private def loadPackage(qualified : List[String], fileOption : Option[File]): EirScope = {
    fileOption match {
      case None => Modules.retrieve(qualified, global)
      case Some(file) =>
        val absPath = file.toPath.toAbsolutePath
        qualified.reverse.foldRight((absPath, global))((name, pathScope) => {
          val parent = pathScope._1.getParent
          if (parent.getFileName.endsWith(name)) {
            val loaded = Modules.provisional(parent.toFile, pathScope._2).get
            (parent, util.assertValid[EirScope](loaded))
          } else {
            throw new RuntimeException(s"could not locate $name within ${pathScope._1}")
          }
        })._2
    }
  }

  def dropSelf(scope : EirScope, fileOption : Option[File]): Unit = {
    fileOption match {
      case Some(file) =>
        util.dropNodes(scope, scope.findChild[EirFileSymbol](f => Files.isSameFile(f.file.toPath, file.toPath)))
      case _ =>
    }
  }

  def visitProgram(ctx : ProgramContext, file : Option[File]): Either[EirScope, EirNamedNode] = {
    val expectation = file.map(Modules.expectation)
    val topLevel : EirScope =
      Option(ctx.packageStatement())
        .map(_.fqn().Identifier.toStringList)
        .map(loadPackage(_, file))
        .getOrElse(global)
    // A provisional package sweep will include us, so we'll just drop that...
    dropSelf(topLevel, file)
    parents.push(topLevel)
    val nodes = ctx.mapOrEmpty(_.annotatedTopLevelStatement, visitAnnotatedTopLevelStatement)
    // namespaces are automatically placed into the top-level, and should not be replicated
    util.placeNodes(topLevel, nodes.filterNot(_.isInstanceOf[EirNamespace]))
    parents.pop()
    expectation match {
      case Some(name) =>
        (topLevel +: nodes).find(_.hasName(name))
          .map(x => Right(x.asInstanceOf[EirNamedNode]))
          .getOrElse(Left(topLevel))
      case None => Left(topLevel)
    }
  }

  override def visitProgram(ctx: ProgramContext): EirScope = {
    visitProgram(ctx, None).left.getOrElse(throw new RuntimeException("unreachable"))
  }

  private def parent: Option[EirNode] =
    Option.when(parents.isEmpty)(global).orElse(parents.headOption)

  private def scope: EirScope =
    parent match {
      case Some(x : EirScope) => x
      case Some(x) => x.scope.orNull
      case None => null
    }

  private def enter[T <: EirNode](node: T, f: T => Unit): T = {
    parents.push(node)
    f(node)
    pop()
  }

  override def visitImportStatement(ctx: ImportStatementContext): EirImport = {
    EirImport(parent, ctx.fqn().Identifier().toStringList)
  }

  override def visitClassDeclaration(ctx: ClassDeclarationContext): EirClass = {
    enter(EirClass(parent, Nil, ctx.Identifier().getText, Nil, None, Nil), (c: EirClass) => {
      c.members ++= ctx.mapOrEmpty(_.annotatedMember, visitAnnotatedMember)
    })
  }

  override def visitAnnotatedMember(ctx: AnnotatedMemberContext): EirMember = {
    enter(visitMember(ctx.member()), (member: EirMember) => {
      member.annotations ++= visitAnnotationList(ctx.annotation())
    })
  }

  override def visitMember(ctx: MemberContext): EirMember = {
    enter(EirMember(parent, null, visitAccessModifier(ctx.accessModifier)), (m: EirMember) => {
      Option(ctx.fieldDeclaration())
        .orElse(Option(ctx.topLevelStatement()))
        .map(x => assertValid[EirNamedNode](visit(x))).foreach(m.member = _)
    })
  }

  override def visitAccessModifier(ctx: AccessModifierContext): EirAccessibility =
    Option(ctx).map(_.getText.capitalize).map(EirAccessibility.withName).getOrElse(defaultMemberAccessibility)

  override def visitNamespace(ctx: NamespaceContext): EirNamespace = {
    val qualified : List[String] = ctx.fqn().Identifier.toStringList
    enter(Modules.retrieve(qualified, scope), (n: EirNamespace) => {
      n.children ++= ctx.mapOrEmpty(_.annotatedTopLevelStatement, visitAnnotatedTopLevelStatement)
    })
  }

  override def visitAnnotatedTopLevelStatement(ctx: AnnotatedTopLevelStatementContext): EirNode = {
    val target: ParseTree = Option(ctx.namespace()).getOrElse(ctx.topLevelStatement())
    enter(visit(target).asInstanceOf[EirNode], (n: EirNode) => {
      n.annotations ++= visitAnnotationList(ctx.annotation())
    })
  }

  override def visitTopLevelStatement(ctx: TopLevelStatementContext): EirNode =
    Option(ctx).map(super.visitTopLevelStatement).to[EirNode].orNull

  def visitAnnotationList(annotations: java.util.List[AnnotationContext]): Iterable[EirAnnotation] =
    annotations.asScala.map(this.visitAnnotation)

  override def visitAnnotation(ctx: AnnotationContext): EirAnnotation =
    EirAnnotation(parent, ctx.Identifier().getText)

  override def visitFunction(ctx: FunctionContext): EirFunction = {
    enter(EirFunction(parent, None, ctx.Identifier().getText, Nil, Nil, null), (f: EirFunction) => {
      f.templateArgs = visitTemplateDecl(ctx.templateDecl())
      f.functionArgs = visitFunctionArgumentList(ctx.functionArgumentList)
      f.returnType = visitType(ctx.`type`())
      f.body = visitBlock(ctx.block())
    })
  }

  override def visitBlock(ctx: BlockContext): Option[EirBlock] = {
    Option(ctx).map(ctx => enter(EirBlock(parent, Nil), (b: EirBlock) => {
      b.children = ctx.mapOrEmpty(_.statement, visitStatement)
    }))
  }

  override def visitStatement(ctx: StatementContext): EirNode = {
    if (ctx.assignment() != null) visitAssignment(ctx.assignment())
    else if (ctx.expression() != null) visitExpression(ctx.expression())
    else super.visitStatement(ctx).asInstanceOf[EirNode]
  }

  override def visitTemplateDecl(ctx: TemplateDeclContext): List[EirTemplateArgument] =
    ctx.mapOrEmpty(_.templateDeclArg, visitTemplateDeclArg)

  override def visitTemplateDeclArg(ctx: TemplateDeclArgContext): EirTemplateArgument = {
    enter(EirTemplateArgument(parent, ctx.Identifier().getText), (t: EirTemplateArgument) => {
      t.lowerBound = Option(ctx.lowerBound).map(visitType)
      t.upperBound = Option(ctx.upperBound).map(visitType)
    })
  }

  override def visitFunctionArgumentList(ctx: FunctionArgumentListContext): List[EirFunctionArgument] =
    ctx.mapOrEmpty(_.functionArgument, visitFunctionArgument)

  override def visitFunctionArgument(ctx: FunctionArgumentContext): EirFunctionArgument = {
    val arg = EirFunctionArgument(parent, ctx.Identifier.getText, null,
      isFinal = Option(ctx.VariableKeyword()).isEmpty,
      isSelfAssigning = Option(ctx.Equals()).isDefined)
    enter(arg, (_: EirFunctionArgument) => {
      arg.declaredType = visitType(ctx.`type`())
    })
  }

  override def visitValueDeclaration(ctx: ValueDeclarationContext): EirDeclaration = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = true)

  def visitDeclaration(name: TerminalNode, declaredType: TypeContext, expressionContext: ExpressionContext, isFinal: Boolean): EirDeclaration = {
    enter(EirDeclaration(parent, isFinal, name.getText, null, None), (d: EirDeclaration) => {
      d.declaredType = visitType(declaredType)
      d.initialValue = Option(expressionContext).map(visitExpression)
    })
  }

  override def visitType(ctx: TypeContext): EirResolvable[EirType] = super.visitType(ctx).asInstanceOf[EirResolvable[EirType]]

  override def visitExpression(ctx: ExpressionContext): EirExpressionNode =
    super.visitExpression(ctx).asInstanceOf[EirExpressionNode]

  override def visitFieldValueDeclaration(ctx: FieldValueDeclarationContext): EirDeclaration = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = true)

  override def visitVariableDeclaration(ctx: VariableDeclarationContext): EirDeclaration = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = false)

  override def visitAssignment(ctx: AssignmentContext): EirAssignment = {
    enter(EirAssignment(parent, null, null), (a: EirAssignment) => {
      a.lval = visitPostfixExpression(ctx.postfixExpression())
      a.rval = visitExpression(ctx.expression())
    })
  }

  override def visitPostfixExpression(ctx: PostfixExpressionContext): EirExpressionNode = {
    if (ctx.Identifier() != null) {
      enter(EirFieldAccessor(parent, null, ctx.Identifier().getText), (f: EirFieldAccessor) => {
        f.target = visitPostfixExpression(ctx.postfixExpression())
      })
    } else if (ctx.arrArgs != null) {
      enter(EirArrayReference(parent, null, null), (f: EirArrayReference) => {
        f.target = visitPostfixExpression(ctx.postfixExpression())
        f.args = visitExpressionList(ctx.arrArgs)
      })
    } else if (ctx.LParen() != null) {
      enter(EirFunctionCall(parent, null, null), (f: EirFunctionCall) => {
        f.target = visitPostfixExpression(ctx.postfixExpression())
        f.args = Option(ctx.fnArgs).map(visitExpressionList).getOrElse(Nil)
      })
    } else {
      assertValid[EirExpressionNode](visitPrimaryExpression(ctx.primaryExpression()))
    }
  }

  override def visitTupleType(ctx: TupleTypeContext): EirResolvable[EirType] =
    visitTypeList(ctx.typeList()).toTupleType(parent)

  override def visitBasicType(ctx: BasicTypeContext): EirResolvable[EirType] = {
    var base: EirResolvable[EirType] = symbolizeType(ctx.fqn.Identifier())
    val templates = visitTypeList(ctx.typeList())
    if (templates.nonEmpty) {
      val templatedType = EirTemplatedType(parent, base, templates)
      base.parent = Some(templatedType)
      base = templatedType
    }
    if (ctx.Atpersand() != null) {
      val proxyType = EirProxyType(parent, base, Option(ctx.CollectiveKeyword()).map(_.getText))
      base.parent = Some(proxyType)
      base = proxyType
    }
    base
  }

  def symbolize[T <: EirNamedNode : Manifest](identifiers: java.util.List[TerminalNode]): EirSymbol[T] = {
    val name = identifiers.toStringList
    EirBuiltInTypes.contains(name.last)
      .map(x => throw new RuntimeException(s"cannot override built-in symbol $x"))
      .getOrElse(EirSymbol[T](parent, name))
  }

  def symbolizeType(identifiers: java.util.List[TerminalNode]): EirResolvable[EirType] = {
    identifiers.toStringList match {
      case head :: Nil =>
        EirBuiltInTypes.contains(head).getOrElse(EirSymbol[EirNamedType](parent, List(head)))
      case name =>
        EirSymbol[EirNamedType](parent, name)
    }
  }

  override def visitTypeList(ctx: TypeListContext): List[EirResolvable[EirType]] = ctx.mapOrEmpty(_.`type`, visitType)

  override def visitMultiplicativeExpression(ctx: MultiplicativeExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitAdditiveExpression(ctx: AdditiveExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitShiftExpression(ctx: ShiftExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitRelationalExpression(ctx: RelationalExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitEqualityExpression(ctx: EqualityExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitAndExpression(ctx: AndExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitExclusiveOrExpression(ctx: ExclusiveOrExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitInclusiveOrExpression(ctx: InclusiveOrExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  def visitBinaryExpression[T <: ParserRuleContext](ctx: T): EirExpressionNode = {
    val children = ctx.children.asScala.toList
    if (children.length == 1) {
      assertValid[EirExpressionNode](visit(children.head))
    } else if (children.length == 3) {
      enter(EirBinaryExpression(parent, null, children(1).getText, null), (e: EirBinaryExpression) => {
        e.lhs = assertValid[EirExpressionNode](visit(children.head))
        e.rhs = assertValid[EirExpressionNode](visit(children.last))
      })
    } else throw new RuntimeException("how did I get here?")
  }

  private def pop[T](): T = parents.pop().asInstanceOf[T]

  override def visitLogicalAndExpression(ctx: LogicalAndExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitLogicalOrExpression(ctx: LogicalOrExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitTupleExpression(ctx: TupleExpressionContext): EirExpressionNode =
    EirTupleExpression.fromExpressions(parent, visitExpressionList(ctx.expressionList()))

  override def visitExpressionList(ctx: ExpressionListContext): List[EirExpressionNode] =
    ctx.mapOrEmpty(_.expression, visitExpression)

  override def visitLambdaExpression(ctx: LambdaExpressionContext): EirExpressionNode = {
    enter(EirLambdaExpression(parent, null, null), (f: EirLambdaExpression) => {
      f.args = visitFunctionArgumentList(ctx.functionArgumentList())
      f.body = visitBlock(ctx.block()).getOrElse(util.encloseNodes(visitExpression(ctx.expression)))
    })
  }

  override def visitConditionalExpression(ctx: ConditionalExpressionContext): EirExpressionNode = {
    if (ctx.expression() == null) return visitLogicalOrExpression(ctx.logicalOrExpression())
    enter(EirTernaryOperator(parent, null, null, null), (e: EirTernaryOperator) => {
      e.test = visitLogicalOrExpression(ctx.logicalOrExpression())
      e.ifTrue = visitExpression(ctx.expression())
      e.ifFalse = visitConditionalExpression(ctx.conditionalExpression())
    })
  }

  override def visitUnaryExpression(ctx: UnaryExpressionContext): EirExpressionNode = {
    Option(ctx.postfixExpression()).map(x => assertValid[EirExpressionNode](visitPostfixExpression(x))).getOrElse({
      enter(EirUnaryExpression(parent, ctx.unaryOperator().getText, null), (u: EirUnaryExpression) => {
        u.rhs = visitCastExpression(ctx.castExpression())
      })
    })
  }

  override def visitCastExpression(ctx: CastExpressionContext): EirExpressionNode = {
    Option(ctx.unaryExpression()).map(x => assertValid[EirExpressionNode](visitUnaryExpression(x))).getOrElse({
      enter(EirTypeCast(parent, null, null), (t: EirTypeCast) => {
        t.to = visitType(ctx.`type`())
        t.value = visitCastExpression(ctx.castExpression())
      })
    })
  }

  override def visitReturnStatement(ctx: ReturnStatementContext): EirNode = {
    enter(EirReturn(parent, null), (r: EirReturn) => {
      r.expression = visitExpression(ctx.expression())
    })
  }

  override def visitInheritanceDecl(ctx: InheritanceDeclContext): Any = {
    val base: EirClassLike = parent.to[EirClassLike].get
    val children: Iterable[ParseTree] = ctx.children.asScala
    for (List(kwd, ty) <- children.sliding(2)) {
      kwd.getText match {
        case "extends" => base.extendsThis = Some(visitType(ty.asInstanceOf[TypeContext]))
        case "implements" | "and" => base.implementsThese ++= List(visitType(ty.asInstanceOf[TypeContext]))
      }
    }
  }

  override def visitForLoop(ctx: ForLoopContext): EirForLoop = {
    enter(EirForLoop(parent, null, null), (f: EirForLoop) => {
      f.header = visitLoopHeader(ctx.loopHeader())
      f.body = Option(ctx.block()).flatMap(visitBlock).getOrElse(util.encloseNodes(visitStatement(ctx.statement())))
    })
  }

  override def visitLoopHeader(ctx: LoopHeaderContext): EirForLoopHeader = {
    if (ctx.variableDeclaration() != null) {
      EirCStyleHeader(Option(ctx.variableDeclaration()).map(visitVariableDeclaration),
        Option(ctx.test).map(visitExpression),
        Option(ctx.assignment()).map(visitAssignment))
    } else {
      EirForAllHeader(parent, ctx.identifierList().Identifier().toStringList, visitExpression(ctx.expression()))
    }
  }

  override def visitLambdaType(ctx: LambdaTypeContext): EirType = {
    val children: List[ParseTree] = ctx.children.asScala.toList
    val l = EirLambdaType(parent, null, null)
    parents.push(l)
    l.from = children.head match {
      case x: TupleTypeContext => visitTupleType(x) match {
        case x: EirTupleType => x.children
        case x => List(x)
      }
      case x: BasicTypeContext => List(visitBasicType(x))
    }
    l.to = children.last match {
      case x: TupleTypeContext => visitTupleType(x)
      case x: BasicTypeContext => visitBasicType(x)
    }
    pop()
  }

  override def visitPrimaryExpression(ctx: PrimaryExpressionContext): EirExpressionNode = {
    if (ctx == null) {
      throw new RuntimeException("encountered null")
    } else if (ctx.fqn() != null) {
      symbolize[EirNamedNode](ctx.fqn().Identifier())
    } else {
      assert(ctx.children.size() == 1)
      assertValid[EirExpressionNode](visit(ctx.children.get(0)))
    }
  }

  override def visitConstant(ctx: ConstantContext): EirExpressionNode = {
    if (ctx.IntegerConstant() != null) {
      EirLiteral(parent, EirLiteralTypes.Integer, ctx.IntegerConstant().getText)
    } else if (ctx.FloatingConstant() != null) {
      EirLiteral(parent, EirLiteralTypes.Float, ctx.FloatingConstant().getText)
    } else if (Option(ctx.StringLiteral()).exists(_.size() >= 1)) {
      EirLiteral(parent, EirLiteralTypes.String, ctx.StringLiteral().asScala.map(_.getText).reduce(_ + _))
    } else {
      assert(ctx.CharacterConstant() != null)
      EirLiteral(parent, EirLiteralTypes.Character, ctx.CharacterConstant().getText)
    }
  }

  //  override def visitChildren(ruleNode: RuleNode): Any = ???
  //  override def visitErrorNode(errorNode: ErrorNode): Any = ???
}
