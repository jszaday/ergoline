package edu.illinois.cs.ergoline

import java.io.File
import java.nio.file.Files

import edu.illinois.cs.ergoline.ErgolineParser._
import edu.illinois.cs.ergoline.ast.EirAccessibility.EirAccessibility
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.resolution.{EirPlaceholder, EirResolvable, Modules}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichEirNode, RichOption, RichResolvableTypeIterable}
import edu.illinois.cs.ergoline.util.{AstManipulation, Errors, assertValid}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ParseTree, TerminalNode}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._

class Visitor(global: EirScope = EirGlobalNamespace) extends ErgolineBaseVisitor[EirNode] {

  val parents: mutable.Stack[EirNode] = new mutable.Stack[EirNode]
  val defaultModuleName = "__default__"
  val defaultMemberAccessibility: EirAccessibility = EirAccessibility.Public

  object VisitorSyntax {
    implicit class RichTerminalNodeList(list: java.util.List[TerminalNode]) {
      implicit def toStringList: List[String] = list.asScala.map(_.getText).toList
    }

    implicit class RichParserRuleContext[T <: ParserRuleContext](t: T) {
      def mapOrEmpty[A, B](f: T => java.util.List[A], g: A => B): List[B] =
        Option(t).map(f).map(_.asScala).getOrElse(Nil).map(g).toList

      def sourceInfo: EirSourceInfo = {
        new EirSourceInfo(t.start.getTokenSource.getSourceName,
          t.start.getLine, t.start.getCharPositionInLine + 1, t.getText)
      }
    }

    implicit class RichTypeListContext(ctx : TypeListContext) {
      def toList: List[EirResolvable[EirType]] = {
        ctx.mapOrEmpty(_.`type`(), (x: TypeContext) => {
          visit(x).asInstanceOf[EirResolvable[EirType]]
        })
      }
    }
  }

  import VisitorSyntax.{RichParserRuleContext, RichTerminalNodeList, RichTypeListContext}

  override def visit(tree: ParseTree): EirNode = {
    val res = super.visit(tree)
    res.location = tree match {
      case c : ParserRuleContext => Some(c.sourceInfo)
      case _ => None
    }
    res
  }

  def visitAs[T <: EirNode : Manifest](tree: ParseTree): T = {
    Option(tree).map(visit) match {
      case Some(x) => assertValid[T](x)
      case None => Errors.cannotParse(tree)
    }
  }

  private def loadPackage(qualified : List[String], fileOption : Option[File]): EirScope = {
    fileOption match {
      case None => Modules.retrieve(qualified, global)
      case Some(file) =>
        val absPath = file.getCanonicalFile.toPath
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
        AstManipulation.dropNodes(scope, scope.findChild[EirFileSymbol](f => Files.isSameFile(f.file.toPath, file.toPath)))
      case _ =>
    }
  }

  def visitProgram(ctx : ProgramContext, file : Option[File]): (EirNode, List[EirNode]) = {
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
    AstManipulation.placeNodes(topLevel, nodes.filterNot(_.isInstanceOf[EirNamespace]))
    parents.pop()
    expectation.map(name => {
      if (file.exists(Modules.isPackageFile) && topLevel.hasName(name)) (topLevel, nodes)
      else nodes.partition(_.hasName(name)) match {
        case (head :: Nil, x) => (head, x)
        case _ => throw new RuntimeException(s"could not locate $name in ${file.get.getName}")
      }
    }).getOrElse((topLevel, Nil))
  }

  override def visitProgram(ctx: ProgramContext): EirScope = {
    assertValid[EirScope](visitProgram(ctx, None)._1)
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

  override def visitInterpolatedString(ctx: InterpolatedStringContext): EirNode = {
    val groupStartChar = "${"

    @tailrec
    def findGroupStart(s: String): Int = {
      val found = s.indexOf(groupStartChar)
      if (found > 0 && s.charAt(found - 1) == '\\') {
        findGroupStart(s.substring(found + groupStartChar.length))
      } else {
        found
      }
    }

    @tailrec
    def findGroupEnd(s: String): Int = {
      val open = s.indexOf("{")
      val close = s.indexOf("}")
      if (open >= 0 && open < close) {
        findGroupEnd(s.substring(close + 1))
      } else {
        close
      }
    }

    enter(EirInterpolatedString(Nil)(parent), (x: EirInterpolatedString) => {
      var text = {
        val tmp = ctx.IStringLiteral().getText
        tmp.substring(1, tmp.length - 1)
      }
      var start = findGroupStart(text)

      while (start >= 0) {
        val pre = text.substring(0, start)
        text = text.substring(start + groupStartChar.length)
        val end = findGroupEnd(text)

        if (pre.nonEmpty) x.append(pre)

        val group = text.substring(0, end).trim
        if (group.nonEmpty) {
          val subParser = Modules.parserFromString(group)
          val expr = visitAs[EirExpressionNode](subParser.expression())
          x.append(expr)
        }

        text = text.substring(end + 1)
        start = findGroupStart(text)
      }

      if (text.nonEmpty) x.append(text)
    })
  }

  override def visitClassDeclaration(ctx: ClassDeclarationContext): EirClassLike = {
    val name = ctx.Identifier().getText
    val node: EirClassLike =
      if (ctx.ClassKwd() != null) {
        EirClass(parent, null, name, null, None, Nil)
      } else {
        EirTrait(parent, null, name, null, None, Nil)
      }
    enter(node, (c: EirClassLike) => {
      c.isAbstract = c.isAbstract || ctx.AbstractKwd() != null
      Option(ctx.inheritanceDecl()).foreach(visitInheritanceDecl)
      c.templateArgs = visitTemplateDeclaration(ctx.templateDecl())
      c.members = ctx.mapOrEmpty(_.annotatedMember, visitAs[EirMember])
    })
  }

  def visitTemplateDeclaration(ctx: TemplateDeclContext): List[EirTemplateArgument] = {
    ctx.mapOrEmpty(_.templateDeclArg, visitAs[EirTemplateArgument])
  }

  override def visitAnnotatedMember(ctx: AnnotatedMemberContext): EirMember = {
    enter(visitMember(ctx.member()), (member: EirMember) => {
      member.annotations ++= visitAnnotationList(ctx.annotation())
    })
  }

  override def visitMember(ctx: MemberContext): EirMember = {
    val modifier: EirAccessibility =
      Option(ctx.accessModifier()).map(_.getText.capitalize)
        .map(EirAccessibility.withName).getOrElse(defaultMemberAccessibility)
    enter(EirMember(parent, null, modifier), (m: EirMember) => {
      m.isStatic = ctx.StaticKwd() != null
      m.isOverride = ctx.OverrideKwd() != null
      Option(ctx.fieldDeclaration())
        .orElse(Option(ctx.topLevelStatement()))
        .map(x => visitAs[EirNamedNode](x)).foreach(m.member = _)
    })
  }

  override def visitNamespace(ctx: NamespaceContext): EirNamespace = {
    val qualified : List[String] = ctx.fqn().Identifier.toStringList
    enter(Modules.retrieve(qualified, scope), (n: EirNamespace) => {
      n.children ++= ctx.mapOrEmpty(_.annotatedTopLevelStatement, visit)
    })
  }

  override def visitAnnotatedTopLevelStatement(ctx: AnnotatedTopLevelStatementContext): EirNode = {
    val target: ParseTree = Option(ctx.namespace()).getOrElse(ctx.topLevelStatement())
    enter(visit(target), (n: EirNode) => {
      n.annotations ++= visitAnnotationList(ctx.annotation())
    })
  }

//  override def visitTopLevelStatement(ctx: TopLevelStatementContext): EirNode =
//    Option(ctx).map(super.visitTopLevelStatement).to[EirNode].orNull

  def visitAnnotationList(annotations: java.util.List[AnnotationContext]): Iterable[EirAnnotation] =
    annotations.asScala.map(this.visitAnnotation)

  override def visitAnnotation(ctx: AnnotationContext): EirAnnotation = {
    val name = ctx.Identifier().getText
    val opts =
      ctx.annotationOptions().mapOrEmpty(_.annotationOption(), (ctx: AnnotationOptionContext) => {
        (Option(ctx.Identifier()).getOrElse(ctx.StaticKwd()).getText, visitAs[EirLiteral](ctx.constant()))
      }).toMap
    EirAnnotation(name, opts)
  }

  override def visitFunction(ctx: FunctionContext): EirFunction = {
    enter(EirFunction(parent, None, ctx.Identifier().getText, Nil, Nil, null), (f: EirFunction) => {
      f.templateArgs = visitTemplateDeclaration(ctx.templateDecl)
      f.functionArgs = ctx.functionArgumentList.mapOrEmpty(_.functionArgument, visitFunctionArgument)
      f.returnType = Option(ctx.`type`())
        .map(visitAs[EirResolvable[EirType]](_))
        .getOrElse(globals.unitType)
      f.body = Option(ctx.block()).map(visitAs[EirBlock])
    })
  }

  override def visitBlock(ctx: BlockContext): EirBlock = {
    enter(EirBlock(parent, Nil), (b: EirBlock) => {
      b.children = ctx.mapOrEmpty(_.statement, visit)
    })
  }

  override def visitStatement(ctx: StatementContext): EirNode = {
    if (ctx.assignment() != null) visit(ctx.assignment())
    else if (ctx.expression() != null) visit(ctx.expression())
    else super.visitStatement(ctx)
  }

  override def visitIfThenElse(ctx: IfThenElseContext): EirIfElse = {
    enter(EirIfElse(parent, null, null, null), (f : EirIfElse) => {
      f.test = visitAs[EirExpressionNode](ctx.condition)
      f.ifTrue = Option(ctx.ifTrue).map(visit)
      f.ifFalse = Option(ctx.ifFalse).map(visit)
    })
  }

  override def visitTemplateDefaultVal(ctx: TemplateDefaultValContext): EirNode = {
    if (ctx.constant() != null) {
      enter(EirConstantFacade(null)(parent), (x: EirConstantFacade) => {
        x.value = visitAs[EirLiteral](ctx.constant())
      })
    } else if (ctx.`type`() != null) {
      visitAs[EirResolvable[EirType]](ctx.`type`())
    } else {
      ???
    }
  }

  override def visitTemplateDeclArg(ctx: TemplateDeclArgContext): EirTemplateArgument = {
    enter(EirTemplateArgument(parent, ctx.name.getText), (t: EirTemplateArgument) => {
      t.lowerBound = Option(ctx.lowerBound).map(visitAs[EirResolvable[EirType]])
      t.upperBound = Option(ctx.upperBound).map(visitAs[EirResolvable[EirType]])
      t.argumentType = Option(ctx.argTy).map(visitAs[EirResolvable[EirType]])
      t.defaultValue = Option(ctx.templateDefaultVal()).map(visitAs[EirResolvable[EirType]])
      t.isPack = ctx.ellipses != null
    })
  }

  override def visitFunctionArgument(ctx: FunctionArgumentContext): EirFunctionArgument = {
    val arg = EirFunctionArgument(parent, ctx.Identifier.getText, null,
      isFinal = Option(ctx.VariableKeyword()).isEmpty,
      isSelfAssigning = Option(ctx.Equals()).isDefined)
    enter(arg, (_: EirFunctionArgument) => {
      arg.declaredType = visitAs[EirResolvable[EirType]](ctx.`type`())
    })
  }

  override def visitValueDeclaration(ctx: ValueDeclarationContext): EirDeclaration = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = true)

  def visitDeclaration(name: TerminalNode, declaredType: TypeContext, expressionContext: ExpressionContext, isFinal: Boolean): EirDeclaration = {
    enter(EirDeclaration(parent, isFinal, name.getText, null, None), (d: EirDeclaration) => {
      d.declaredType = Option(declaredType).map(visitAs[EirResolvable[EirType]](_)).getOrElse(EirPlaceholder(Some(d)))
      d.initialValue = Option(expressionContext).map(visitAs[EirExpressionNode])
    })
  }

  override def visitFieldDeclaration(ctx: FieldDeclarationContext): EirDeclaration =
    visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = ctx.ValueKeyword() != null)

  override def visitVariableDeclaration(ctx: VariableDeclarationContext): EirDeclaration = visitDeclaration(ctx.Identifier, ctx.`type`(), ctx.expression(), isFinal = false)

  override def visitAssignment(ctx: AssignmentContext): EirAssignment = {
    enter(EirAssignment(parent, null, null, null), (a: EirAssignment) => {
      a.lval = visitAs[EirExpressionNode](ctx.postfixExpression())
      a.op = ctx.assignmentOperator.getText
      a.rval = visitAs[EirExpressionNode](ctx.expression())
    })
  }

  override def visitPostfixExpression(ctx: PostfixExpressionContext): EirExpressionNode = {
    if (ctx.Identifier() != null) {
      enter(EirFieldAccessor(parent, null, ctx.Identifier().getText), (f: EirFieldAccessor) => {
        f.target = visitAs[EirExpressionNode](Option(ctx.postfixExpression()).getOrElse(ctx.selfExpression()))
      })
    } else if (ctx.arrArgs != null) {
      enter(EirArrayReference(parent, null, null), (f: EirArrayReference) => {
        f.target = visitAs[EirExpressionNode](ctx.postfixExpression())
        f.args = ctx.arrArgs.mapOrEmpty(_.expression, visitAs[EirExpressionNode])
      })
    } else if (ctx.LParen() != null) {
      enter(EirFunctionCall(parent, null, null, null), (f: EirFunctionCall) => {
        f.target = visitAs[EirExpressionNode](ctx.postfixExpression())
        f.args = ctx.fnArgs.mapOrEmpty(_.expression, visitAs[EirExpressionNode])
        f.specialization = specializationToList(ctx.specialization())
      })
    } else {
      visitAs[EirExpressionNode](ctx.primaryExpression())
    }
  }

  override def visitTupleType(ctx: TupleTypeContext): EirResolvable[EirType] = {
    if (ctx.multiply != null) {
      enter(EirTupleMultiply(null, null)(parent), (x: EirTupleMultiply) => {
        x.lhs = visitAs[EirResolvable[EirType]](ctx.tupleType())
        x.rhs = visitAs[EirExpressionNode](ctx.constExpression())
      })
    } else {
      ctx.typeList().toList.toTupleType(parent)
    }
  }

  override def visitBasicType(ctx: BasicTypeContext): EirResolvable[EirType] = {
    if (ctx.Ellipses() != null) {
      return EirPackExpansion(ctx.fqn.Identifier.toStringList)(parent)
    }
    var base: EirResolvable[EirType] = symbolizeType(ctx.fqn.Identifier())
    val templates = specializationToList(ctx.specialization())
    if (templates.nonEmpty) {
      val templatedType = EirTemplatedType(parent, base, templates)
      base.parent = Some(templatedType)
      base = templatedType
    }
    if (ctx.proxySuffix() != null) {
      val isElement = Option(ctx.proxySuffix().Element()).isDefined
      val proxyType = EirProxyType(parent, base, Option(ctx.proxySuffix().CollectiveKeyword()).map(_.getText), isElement)
      base.parent = Some(proxyType)
      base = proxyType
    }
    base
  }

  override def visitPattern(ctx: PatternContext): EirNode = {
    if (ctx.Identifier() != null) {
      enter(EirIdentifierPattern(parent, ctx.Identifier().getText, null), (i: EirIdentifierPattern) => {
        i.ty =
          Option(ctx.basicType()).orElse(Option(ctx.tupleType()))
            .map(visitAs[EirResolvable[EirType]])
            .getOrElse(EirPlaceholder(i.declarations.headOption))
      })
    } else if (ctx.constant() != null) {
      enter(EirExpressionPattern(parent, null), (e: EirExpressionPattern) => {
        e.expression = visitAs[EirExpressionNode](ctx.constant())
      })
    } else {
      super.visit(ctx)
    }
  }

  override def visitPatternList(ctx: PatternListContext): EirPatternList = {
    enter(EirPatternList(parent, null), (p : EirPatternList) => {
      p.patterns = ctx.pattern().asScala.map(visitAs[EirPattern]).toList
    })
  }

  override def visitCaseStatement(ctx: CaseStatementContext): EirMatchCase = {
    enter(EirMatchCase(parent, null, null, null), (m: EirMatchCase) => {
      m.patterns = visitAs[EirPatternList](ctx.patternList())
      m.condition = Option(ctx.condition).map(visitAs[EirExpressionNode])
      m.body = Option(ctx.bodyExpression).map(visitAs[EirExpressionNode])
    })
  }

  override def visitMatchStatement(ctx: MatchStatementContext): EirMatch = {
    enter(EirMatch(parent, null, null), (m: EirMatch) => {
      m.expression = visitAs[EirExpressionNode](ctx.expression())
      m.cases = ctx.caseStatement().asScala.map(visitAs[EirMatchCase]).toList
    })
  }

  def symbolize[T <: EirNamedNode : Manifest](identifiers: java.util.List[TerminalNode]): EirSymbol[T] = {
    EirSymbol[T](parent, identifiers.toStringList)
  }

  def symbolize[T <: EirNamedNode : Manifest](identifier: TerminalNode): EirSymbol[T] = {
    EirSymbol[T](parent, List(identifier.getText))
  }

  def symbolizeType(identifiers: java.util.List[TerminalNode]): EirResolvable[EirType] = {
    EirSymbol[EirNamedType](parent, identifiers.toStringList)
  }

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
      visitAs[EirExpressionNode](children.head)
    } else if (children.length == 3) {
      enter(EirBinaryExpression(parent, null, children(1).getText, null), (e: EirBinaryExpression) => {
        e.lhs = visitAs[EirExpressionNode](children.head)
        e.rhs = visitAs[EirExpressionNode](children.last)
      })
    } else throw new RuntimeException("how did I get here?")
  }

  private def pop[T](): T = parents.pop().asInstanceOf[T]

  override def visitLogicalAndExpression(ctx: LogicalAndExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitLogicalOrExpression(ctx: LogicalOrExpressionContext): EirExpressionNode = visitBinaryExpression(ctx)

  override def visitTupleExpression(ctx: TupleExpressionContext): EirExpressionNode =
    EirTupleExpression.fromExpressions(parent, ctx.expressionList().mapOrEmpty(_.expression, visitAs[EirExpressionNode]))

  override def visitLambdaExpression(ctx: LambdaExpressionContext): EirExpressionNode = {
    enter(EirLambdaExpression(parent, null, null), (f: EirLambdaExpression) => {
      f.args = ctx.functionArgumentList().mapOrEmpty(_.functionArgument, visitFunctionArgument)
      f.body = Option(ctx.block()).map(visitBlock)
        .getOrElse(AstManipulation.encloseNodes(visitAs[EirExpressionNode](ctx.expression))(addReturn = true))
    })
  }

  override def visitConditionalExpression(ctx: ConditionalExpressionContext): EirExpressionNode = {
    if (ctx.expression() == null) return visitLogicalOrExpression(ctx.logicalOrExpression())
    enter(EirTernaryOperator(parent, null, null, null), (e: EirTernaryOperator) => {
      e.test = visitAs[EirExpressionNode](ctx.logicalOrExpression())
      e.ifTrue = visitAs[EirExpressionNode](ctx.expression())
      e.ifFalse = visitAs[EirExpressionNode](ctx.conditionalExpression())
    })
  }

  override def visitNewExpression(ctx: NewExpressionContext): EirNew = {
    enter(EirNew(parent, null, null), (n : EirNew) => {
      n.target = visitAs[EirResolvable[EirType]](ctx.`type`())
      if (ctx.tupleExpression() != null) {
        n.args = ctx.tupleExpression().expressionList().mapOrEmpty(_.expression, visitAs[EirExpressionNode])
      }
    })
  }

  override def visitUnaryExpression(ctx: UnaryExpressionContext): EirExpressionNode = {
    if (ctx.unaryExpression() != null) {
      enter(EirUnaryExpression(parent, ctx.unaryOperator().getText, null),
        (u: EirUnaryExpression) => u.rhs = visitAs[EirExpressionNode](ctx.unaryExpression()))
    } else {
      visitAs[EirExpressionNode](ctx.children.get(0))
    }
  }

  override def visitAwaitExpression(ctx: AwaitExpressionContext): EirNode = {
    enter(EirAwait(parent, null), (a: EirAwait) => {
      a.target = visitAs[EirExpressionNode](ctx.postfixExpression())
    })
  }

  override def visitReturnStatement(ctx: ReturnStatementContext): EirNode = {
    enter(EirReturn(parent, null), (r: EirReturn) => {
      r.expression = visitAs[EirExpressionNode](ctx.expression())
    })
  }

  override def visitInheritanceDecl(ctx: InheritanceDeclContext): EirNode = {
    val base: EirClassLike = parent.to[EirClassLike].get
    val children: List[ParseTree] = Option(ctx.children).toIterable.flatMap(_.asScala).toList
    val grouped: List[List[ParseTree]] = children.grouped(2).toList
    for (List(kwd, ty) <- grouped) {
      kwd.getText match {
        case "extends" => base.extendsThis = Some(visitAs[EirResolvable[EirType]](ty.asInstanceOf[TypeContext]))
        case "with" | "and" => base.implementsThese ++= List(visitAs[EirResolvable[EirType]](ty.asInstanceOf[TypeContext]))
      }
    }
    null
  }

  override def visitWhileLoop(ctx: WhileLoopContext): EirWhileLoop = {
    enter(EirWhileLoop(parent, null, null), (l: EirWhileLoop) => {
      l.condition = Option(ctx.expression()).map(visitAs[EirExpressionNode])
      l.body = Option(ctx.block()).map(visitBlock).getOrElse(AstManipulation.encloseNodes(visitStatement(ctx.statement())))
    })
  }

  override def visitForLoop(ctx: ForLoopContext): EirForLoop = {
    enter(EirForLoop(parent, null, null), (f: EirForLoop) => {
      visitLoopHeader(ctx.loopHeader())
      f.body = Option(ctx.block()).map(visitBlock).getOrElse(AstManipulation.encloseNodes(visitStatement(ctx.statement())))
    })
  }

  override def visitLoopHeader(ctx: LoopHeaderContext): EirNode = {
    val forLoop = parent.to[EirForLoop].get
    forLoop.header = if (ctx.variableDeclaration() != null) {
      EirCStyleHeader(Option(ctx.variableDeclaration()).map(visitVariableDeclaration),
        Option(ctx.test).map(visitAs[EirExpressionNode]),
        Option(ctx.assignment()).map(visitAssignment))
    } else {
      EirForAllHeader(parent, ctx.identifierList().Identifier().toStringList, visitAs[EirExpressionNode](ctx.expression()))
    }
    null
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

  def specializationToList(ctx: SpecializationContext): List[EirResolvable[EirType]] = {
    if (ctx != null && ctx.LeftShift() != null) {
      ctx.mapOrEmpty(_.`type`(), visitAs[EirResolvable[EirType]]) :+ {
        enter(EirTemplatedType(parent, null, null), (t: EirTemplatedType) => {
          t.base = symbolizeType(ctx.fqn.Identifier())
          t.args = ctx.mapOrEmpty(_.typeList().`type`(), visitAs[EirResolvable[EirType]])
        })
      }
    } else {
      ctx.mapOrEmpty(_.typeList().`type`(), visitAs[EirResolvable[EirType]])
    }
  }

  override def visitIdentifierExpression(ctx: IdentifierExpressionContext): EirNode = {
    if (ctx.specialization() == null) {
      symbolize[EirNamedNode](ctx.fqn().Identifier())
    } else {
      enter(EirSpecializedSymbol(parent, null, null), (s : EirSpecializedSymbol) => {
        s.specialization = specializationToList(ctx.specialization())
        s.symbol = symbolize[EirNamedNode with EirSpecializable](ctx.fqn().Identifier())
      })
    }
  }

  override def visitUsingStatement(ctx: UsingStatementContext): EirNode = {
    enter(EirTypeAlias(ctx.Identifier().getText, null, null)(parent), (x: EirTypeAlias) => {
      x.templateArgs = visitTemplateDeclaration(ctx.templateDecl())
      x.value = visitAs[EirResolvable[EirType]](ctx.`type`())
    })
  }

  override def visitSelfExpression(ctx: SelfExpressionContext): EirNode = {
    symbolize[EirNamedNode](ctx.SelfKeyword)
  }

  override def visitPrimaryExpression(ctx: PrimaryExpressionContext): EirExpressionNode = {
    assert(ctx.children.size() == 1)
    visitAs[EirExpressionNode](ctx.children.get(0))
  }

  override def visitBoolLiteral(ctx: BoolLiteralContext): EirLiteral =
    EirLiteral(parent, EirLiteralTypes.Boolean,
      Option(ctx.FalseKwd()).getOrElse(ctx.TrueKwd()).getText)

    override def visitConstExpression(ctx: ConstExpressionContext): EirNode = {
      Option(ctx.fqn()).map(fqn => symbolizeType(fqn.Identifier()))
        .getOrElse(visitAs[EirExpressionNode](ctx.constant()))
    }

  override def visitConstant(ctx: ConstantContext): EirLiteral = {
    if (ctx.IntegerConstant() != null) {
      EirLiteral(parent, EirLiteralTypes.Integer, ctx.IntegerConstant().getText)
    } else if (ctx.FloatingConstant() != null) {
      EirLiteral(parent, EirLiteralTypes.Float, ctx.FloatingConstant().getText)
    } else if (Option(ctx.StringLiteral()).exists(_.size() >= 1)) {
      EirLiteral(parent, EirLiteralTypes.String, ctx.StringLiteral().asScala.map(_.getText).reduce(_ + _))
    } else if (ctx.boolLiteral() != null) {
      visitBoolLiteral(ctx.boolLiteral())
    } else {
      assert(ctx.CharacterConstant() != null)
      EirLiteral(parent, EirLiteralTypes.Character, ctx.CharacterConstant().getText)
    }
  }
}
