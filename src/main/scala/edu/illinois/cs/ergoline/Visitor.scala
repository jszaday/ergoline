package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ErgolineParser._
import edu.illinois.cs.ergoline.Visitor.{InfixPart, isAssignOperator}
import edu.illinois.cs.ergoline.ast.EirAccessibility.EirAccessibility
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.literals._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.resolution.{
  EirPlaceholder,
  EirResolvable,
  Find,
  Modules
}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{
  RichEirNode,
  RichOption,
  RichResolvableTypeIterable
}
import edu.illinois.cs.ergoline.util.{AstManipulation, Errors, assertValid}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.ParseTree

import java.io.File
import java.nio.file.Files
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.reflect.{ClassTag, classTag}

object Visitor {
  type InfixPart = Either[EirExpressionNode, String]

  val precedences: Seq[Seq[Char]] = Seq(
    Seq('*', '/', '%'),
    Seq('+', '-'),
    //    Seq(':'),
    Seq('=', '!'),
    Seq('<', '>'),
    Seq('&'),
    Seq('^'),
    Seq('|')
  )

  def kindFrom(
      prefix: Option[String],
      collective: Option[String]
  ): Option[EirProxyKind] = {
    prefix
      .filter(_ => collective.nonEmpty)
      .map({
        case "[@]" => EirElementProxy
        case "{@}" => EirSectionProxy
        case "@"   => EirCollectiveProxy
        case _     => ???
      })
  }

  def isAssignOperator(op: String): Boolean = {
    op.endsWith("=") && !(globals.isIdentityComparator(op) || globals
      .isComparisonOperator(op))
  }

  def precedenceOf(op: String): Int = {
    if (op.head.isLetter || isAssignOperator(op)) {
      precedences.size + 2
    } else {
      precedences.indexWhere(_.contains(op.head)) + 1
    }
  }

  def sortInfixes(
      parts: Seq[InfixPart],
      former: Seq[InfixPart] => EirExpressionNode
  ): EirExpressionNode = {
    var infixes = parts
    var ops = infixes.zipWithIndex
      .collect({ case (Right(op), i) => (i, precedenceOf(op)) })
      .sortBy(_._2)
      .map(_._1)

    while (ops.nonEmpty) {
      val start = ops.head - 1
      val slice = infixes.slice(start, start + 3)
      val expr = former(slice)

      infixes = infixes.patch(start, Seq(Left(expr)), 3)
      ops = ops.tail.map(x => if (x > start) x - 2 else x)
    }

    infixes match {
      case Left(x) :: Nil => x
      case _              => ???
    }
  }

  def dropSelf(scope: EirScope, fileOption: Option[File]): Unit = {
    fileOption match {
      case Some(file) => AstManipulation.dropNodes(
          scope,
          Find.child[EirFileSymbol](
            scope,
            f => Files.isSameFile(f.file.toPath, file.toPath)
          )
        )
      case _ =>
    }
  }

  def mkSpecialization[A <: EirNamedNode: ClassTag](
      parent: Option[EirNode],
      base: EirResolvable[A] = null,
      tys: List[EirResolvable[EirType]] = null
  ): EirResolvable[A] with EirSpecialization = {
    (if (classTag[A].runtimeClass.isAssignableFrom(classOf[EirType])) {
       EirTemplatedType(
         parent,
         Option(base).to[EirResolvable[EirType]].orNull,
         tys
       )
     } else {
       EirSpecializedSymbol(
         parent,
         Option(base).to[EirResolvable[A with EirSpecializable]].orNull,
         tys
       )
     }).asInstanceOf[EirResolvable[A] with EirSpecialization]
  }
}

class Visitor(global: EirScope = EirGlobalNamespace)
    extends ErgolineParserBaseVisitor[EirNode] {

  import Visitor._

  def flattenInfix[T <: ParseTree](ctx: T): Seq[InfixPart] = {
    if (ctx.getChildCount == 3) {
      val (lhs, rhs) = (ctx.getChild(0), ctx.getChild(2))
      flattenInfix(lhs) ++ {
        Option(ctx.getChild(1))
          .map(_.getText)
          .map(Right(_))
      } ++ flattenInfix(rhs)
    } else {
      Seq(Left(visitAs[EirExpressionNode](ctx.getChild(0))))
    }
  }

  def formBinaryExpression(seq: Seq[InfixPart]): EirExpressionNode = {
    seq match {
      case Left(lhs) :: Right(op) :: Left(rhs) :: Nil if isAssignOperator(op) =>
        enter(
          EirAssignment(parent, lhs, op, rhs),
          (expr: EirAssignment) => {
            lhs.parent = Some(expr)
            rhs.parent = Some(expr)
          }
        )

      case Left(lhs) :: Right(op) :: Left(rhs) :: Nil => enter(
          EirBinaryExpression(parent, lhs, op, rhs),
          (expr: EirBinaryExpression) => {
            lhs.parent = Some(expr)
            rhs.parent = Some(expr)
          }
        )
      case _ => ???
    }
  }

  val parents: mutable.Stack[EirNode] = new mutable.Stack[EirNode]
  val defaultModuleName = "__default__"
  val defaultMemberAccessibility: EirAccessibility = EirAccessibility.Public

  object VisitorSyntax {
    implicit class RichParseTreeList[T <: ParseTree](list: java.util.List[T]) {
      implicit def toStringList: List[String] =
        list.asScala.map(_.getText).toList
    }

    implicit class RichParserRuleContext[T <: ParserRuleContext](t: T) {
      def mapOrEmpty[A, B](f: T => java.util.List[A], g: A => B): List[B] =
        Option(t).map(f).map(_.asScala).getOrElse(Nil).map(g).toList

      def sourceInfo: EirSourceInfo = {
        new EirSourceInfo(
          t.start.getTokenSource.getSourceName,
          t.start.getLine,
          t.start.getCharPositionInLine + 1,
          text = Some(t.getText)
        )
      }
    }

    implicit class RichTypeListContext(ctx: TypeListContext) {
      def toList: List[EirResolvable[EirType]] = {
        ctx.mapOrEmpty(
          _.`type`(),
          (x: TypeContext) => {
            visit(x).asInstanceOf[EirResolvable[EirType]]
          }
        )
      }
    }
  }

  import VisitorSyntax._

  override def visit(tree: ParseTree): EirNode = {
    val res = super.visit(tree)
    Option(res).foreach(res => {
      res.location = tree match {
        case c: ParserRuleContext => Some(c.sourceInfo)
        case _                    => None
      }
    })
    res
  }

  def visitAs[T <: EirNode: ClassTag](tree: ParseTree): T = {
    Option(tree).map(visit) match {
      case Some(x) => assertValid[T](x)
      case None    => Errors.cannotParse(tree)
    }
  }

  def visitProgram(
      ctx: ProgramContext,
      file: Option[File]
  ): (EirNode, List[EirNode]) = {
    val topLevel: EirScope = Option(ctx.packageStatement())
      .map(_.fqn().identifier.toStringList)
      .map(Modules.loadPackage(_, file, global))
      .getOrElse(global)
    // A provisional package sweep will include us, so we'll just drop that...
    dropSelf(topLevel, file)
    parents.push(topLevel)
    val nodes = ctx.mapOrEmpty(
      _.annotatedTopLevelStatement,
      visitAnnotatedTopLevelStatement
    )
    parents.pop()
    Modules.exportNodes(file, topLevel, nodes)
  }

  override def visitType(ctx: TypeContext): EirNode = {
    val base = () => visitAs[EirResolvable[EirType]](ctx.`type`())
    if (ctx.Ampersand() != null) {
      enter(
        EirReferenceType(parent, null),
        (t: EirReferenceType) => { t.base = base() }
      )
    } else if (ctx.Ellipses() != null) {
      enter(
        EirPackExpansion(null)(parent),
        (t: EirPackExpansion) => { t.base = base() }
      )
    } else {
      super.visitType(ctx)
    }
  }

  override def visitProgram(ctx: ProgramContext): EirNode = ???

  private def parent: Option[EirNode] =
    Option.when(parents.isEmpty)(global).orElse(parents.headOption)

  private def scope: EirScope = parent match {
    case Some(x: EirScope) => x
    case Some(x)           => x.scope.orNull
    case None              => null
  }

  private def enter[T <: EirNode](node: T, f: T => Unit): T = {
    parents.push(node)
    f(node)
    pop()
  }

  override def visitPublicImport(ctx: PublicImportContext): EirImport = {
    val node = visitImportStatement(ctx.importStatement())
    node.publicOverride = true
    node
  }

  override def visitImportStatement(ctx: ImportStatementContext): EirImport = {
    EirImport(
      parent,
      ctx.fqn().identifier().toStringList,
      publicOverride = false
    )
  }

  override def visitInterpolatedString(
      ctx: InterpolatedStringContext
  ): EirNode = {
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

    enter(
      EirInterpolatedString(Nil)(parent),
      (x: EirInterpolatedString) => {
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
      }
    )
  }

  override def visitClassDeclaration(
      ctx: ClassDeclarationContext
  ): EirClassLike = {
    val kind = ctx.classKind()
    val name = ctx.identifier().getText
    val node: EirClassLike = {
      val isClass = kind.ClassKwd() != null
      val isStruct = kind.StructKwd() != null
      val isObject = kind.ObjectKwd() != null

      if (isClass || isObject || isStruct) {
        EirClass(
          parent,
          null,
          name,
          null,
          None,
          Nil,
          None, {
            if (isClass) EirReferenceKind
            else if (isObject) EirSingletonKind
            else EirValueKind
          }
        )
      } else {
        EirTrait(parent, null, name, null, None, Nil, None)
      }
    }
    enter(
      node,
      (c: EirClassLike) => {
        c.isAbstract = c.isAbstract || kind.AbstractKwd() != null
        Option(ctx.inheritanceDecl()).foreach(visitInheritanceDecl)
        c.templateArgs = visitTemplateDeclaration(ctx.templateDecl())
        c.members = ctx.mapOrEmpty(_.annotatedMember, visitAs[EirMember])
        c.predicate = Option(ctx.whereClause()).map(visitAs[EirExpressionNode])
      }
    )
  }

  def visitTemplateDeclaration(
      ctx: TemplateDeclContext
  ): List[EirTemplateArgument] = {
    ctx.mapOrEmpty(_.templateDeclArg, visitAs[EirTemplateArgument])
  }

  override def visitAnnotatedMember(ctx: AnnotatedMemberContext): EirMember = {
    enter(
      visitMember(ctx.member()),
      (member: EirMember) => {
        member.annotations ++= visitAnnotationList(ctx.annotation())
      }
    )
  }

  override def visitMember(ctx: MemberContext): EirMember = {
    val modifier = Option(ctx.accessModifier())
      .map(_.getText.capitalize)
      .map(EirAccessibility.withName)
    enter(
      EirMember(parent, null, null),
      (m: EirMember) => {
        m.isStatic = ctx.StaticKwd() != null
        m.isOverride = ctx.OverrideKwd() != null
        Option(ctx.fieldDeclaration())
          .orElse(Option(ctx.topLevelStatement()))
          .map(x => visitAs[EirNamedNode](x))
          .foreach(m.member = _)
        m.accessibility = modifier.getOrElse({
          m.member match {
            case _: EirImport => EirAccessibility.Private
            case _            => defaultMemberAccessibility
          }
        })
      }
    )
  }

  override def visitNamespace(ctx: NamespaceContext): EirNamespace = {
    val qualified: List[String] = ctx.fqn().identifier.toStringList
    enter(
      Modules.retrieve(qualified, scope),
      (n: EirNamespace) => {
        n.children ++= ctx.mapOrEmpty(_.annotatedTopLevelStatement, visit)
      }
    )
  }

  override def visitAnnotatedTopLevelStatement(
      ctx: AnnotatedTopLevelStatementContext
  ): EirNode = {
    val target: ParseTree = Option(ctx.namespace())
      .orElse(Option(ctx.publicImport()))
      .getOrElse(ctx.topLevelStatement())
    enter(
      visit(target),
      (n: EirNode) => {
        n.annotations ++= visitAnnotationList(ctx.annotation())
      }
    )
  }

//  override def visitTopLevelStatement(ctx: TopLevelStatementContext): EirNode =
//    Option(ctx).map(super.visitTopLevelStatement).to[EirNode].orNull

  def visitAnnotationList(
      annotations: java.util.List[AnnotationContext]
  ): Iterable[EirAnnotation] = annotations.asScala.map(this.visitAnnotation)

  override def visitTopLevelDeclaration(
      ctx: TopLevelDeclarationContext
  ): EirNode = {
    val decl = visit {
      Option(ctx.valueDeclaration()).getOrElse(ctx.variableDeclaration())
    }
    Option(decl)
      .to[EirDeclaration]
      .foreach(_.isImplicit = ctx.ImplicitKwd() != null)
    decl
  }

  override def visitAnnotation(ctx: AnnotationContext): EirAnnotation = {
    val name = ctx.identifier().getText
    val opts = ctx
      .annotationOptions()
      .mapOrEmpty(
        _.annotationOption(),
        (ctx: AnnotationOptionContext) => {
          (
            ctx.annotationIdentifier().getText,
            Option(ctx.constant())
              .map(visitAs[EirLiteral[_]])
              .getOrElse(globals.trueLiteral(parent))
          )
        }
      )
      .toMap
    EirAnnotation(name, opts)
  }

  override def visitFunction(ctx: FunctionContext): EirFunction = {
    enter(
      EirFunction(
        parent,
        None,
        ctx.functionIdentifier().getText,
        Nil,
        Nil,
        Nil,
        null,
        None
      ),
      (f: EirFunction) => {
        f.templateArgs = visitTemplateDeclaration(ctx.templateDecl)
        f.functionArgs = ctx.functionArgumentList
          .mapOrEmpty(_.functionArgument, visitAs[EirFunctionArgument])
        f.implicitArgs = ctx
          .implicitArguments()
          .mapOrEmpty(_.implicitArgument(), visitAs[EirFunctionArgument])
        f.implicitArgs.foreach(_.isImplicit = true)
        f.returnType = Option(ctx.`type`())
          .map(visitAs[EirResolvable[EirType]](_))
          .getOrElse({ globals.unitSymbol(parent) })
        f.predicate = Option(ctx.whereClause()).map(visitAs[EirExpressionNode])
        f.body = Option(ctx.block()).map(visitAs[EirBlock])
      }
    )
  }

  override def visitBlock(ctx: BlockContext): EirBlock = {
    enter(
      EirBlock(parent, Nil),
      (b: EirBlock) => {
        b.children = ctx.mapOrEmpty(_.statement, visit)
      }
    )
  }

  override def visitStatement(ctx: StatementContext): EirNode = {
    enter(
      visitInnerStatement(ctx.innerStatement()),
      (node: EirNode) => {
        node.annotations ++= visitAnnotationList(ctx.annotation())
      }
    )
  }

  override def visitInnerStatement(ctx: InnerStatementContext): EirNode = {
    if (ctx.expression() != null) visit(ctx.expression())
    else super.visitInnerStatement(ctx)
  }

  def forceEnclosed(opt: Option[EirNode]): Option[EirBlock] = {
    opt.collect {
      case x: EirBlock => x
      case x           => AstManipulation.encloseNodes(x)
    }
  }

  override def visitIfThenElse(ctx: IfThenElseContext): EirIfElse = {
    enter(
      EirIfElse(parent, null, null, null),
      (f: EirIfElse) => {
        f.test = visitAs[EirExpressionNode](ctx.condition)
        f.ifTrue = forceEnclosed(Option(ctx.ifTrue).map(visit))
        f.ifFalse = forceEnclosed(Option(ctx.ifFalse).map(visit))
      }
    )
  }

  override def visitSpecializationElement(
      ctx: SpecializationElementContext
  ): EirNode = {
    if (ctx.constant() != null) {
      enter(
        EirConstantFacade(null)(parent),
        (x: EirConstantFacade) => {
          x.value = visitAs[EirLiteral[_]](ctx.constant())
        }
      )
    } else if (ctx.`type`() != null) {
      visitAs[EirResolvable[EirType]](ctx.`type`())
    } else {
      Errors.unreachable()
    }
  }

  override def visitTemplateDeclArg(
      ctx: TemplateDeclArgContext
  ): EirTemplateArgument = {
    enter(
      EirTemplateArgument(parent, ctx.name.getText),
      (t: EirTemplateArgument) => {
        t.lowerBound =
          Option(ctx.lowerBound).map(visitAs[EirResolvable[EirType]])
        t.upperBound =
          Option(ctx.upperBound).map(visitAs[EirResolvable[EirType]])
        t.argumentType = Option(ctx.argTy).map(visitAs[EirResolvable[EirType]])
        t.defaultValue = Option(ctx.specializationElement())
          .map(visitAs[EirResolvable[EirType]])
        t.isPack = ctx.ellipses != null
      }
    )
  }

  override def visitBasicArgument(
      ctx: BasicArgumentContext
  ): EirFunctionArgument = {
    enter(
      EirFunctionArgument(
        parent,
        ctx.identifier().getText,
        null,
        isExpansion = ctx.ExpansionOp != null,
        isSelfAssigning = false
      ),
      (arg: EirFunctionArgument) => {
        arg.declaredType = visitAs[EirResolvable[EirType]](ctx.`type`())
      }
    )
  }

  override def visitImplicitArgument(
      ctx: ImplicitArgumentContext
  ): EirFunctionArgument = {
    visitAs[EirFunctionArgument](ctx.basicArgument())
  }

  override def visitFunctionArgument(
      ctx: FunctionArgumentContext
  ): EirFunctionArgument = {
    val arg = visitBasicArgument(ctx.basicArgument())
    arg.isSelfAssigning = Option(ctx.Equals()).isDefined
    arg.isReference = Option(ctx.Ampersand()).isDefined
    arg
  }

  private case class Declaration(
      ty: TypeContext,
      name: IdentifierContext,
      isRef: Boolean
  )

  private def fromJava(
      ctx: DecltypeContext
  ): List[Declaration] = {
    if (ctx.decltype() == null || ctx.decltype().isEmpty) {
      List(
        Declaration(ctx.`type`(), ctx.identifier(), ctx.Ampersand() != null)
      )
    } else {
      ctx
        .decltype()
        .asScala
        .toList
        .flatMap(ctx => {
          val children = fromJava(ctx)
          assert(
            children.length == 1,
            "nested structured assigns currently unsupported"
          )
          children
        })
    }
  }

  def visitDeclaration(
      decls: List[Declaration],
      expressionContext: Option[ExpressionContext],
      isFinal: Boolean
  ): EirNode = {
    def expr: Option[EirExpressionNode] =
      expressionContext.map(visitAs[EirExpressionNode])
    def helper(decl: Declaration, idx: Option[Int]): EirDeclaration = {
      enter(
        EirDeclaration(parent, isFinal, decl.name.getText, null, None),
        (d: EirDeclaration) => {
          d.declaredType = Option(decl.ty)
            .map(visitAs[EirResolvable[EirType]](_))
            .getOrElse(EirPlaceholder(Some(d)))
          d.initialValue = (idx, expr) match {
            case (Some(i), Some(x)) =>
              Some(EirArrayReference(None, x, List(EirIntegerLiteral(i)(None))))
            case (_, expr) => expr
          }
        }
      )
    }

    if (decls.length >= 2) {
      enter(
        EirMultiDeclaration(Nil)(parent),
        (b: EirMultiDeclaration) => {
          b.children = decls.zipWithIndex map { case (d, i) =>
            helper(d, Some(i))
          }
        }
      )
    } else {
      assert(decls.length == 1)
      helper(decls.head, None)
    }
  }

  override def visitFieldDeclaration(
      ctx: FieldDeclarationContext
  ): EirNode = visitDeclaration(
    List(Declaration(ctx.`type`(), ctx.identifier(), isRef = false)),
    Option(ctx.expression()),
    isFinal = ctx.ValueKeyword() != null
  )

  override def visitVariableDeclaration(
      ctx: VariableDeclarationContext
  ): EirNode = visitDeclaration(
    fromJava(ctx.decltype()),
    Option(ctx.expression()),
    isFinal = false
  )

  override def visitValueDeclaration(
      ctx: ValueDeclarationContext
  ): EirNode = visitDeclaration(
    fromJava(ctx.decltype()),
    Option(ctx.expression()),
    isFinal = true
  )

  override def visitSliceExpression(ctx: SliceExpressionContext): EirNode = {
    Option(ctx.single)
      .map(visitAs[EirExpressionNode])
      .getOrElse({
        enter(
          EirSlice(None, None, None)(parent),
          (s: EirSlice) => {
            s.start = Option(ctx.start).map(visitAs[EirExpressionNode])
            s.step = Option(ctx.step).map(visitAs[EirExpressionNode])
            s.end = Option(ctx.end).map(visitAs[EirExpressionNode])
          }
        )
      })
  }

  override def visitCallArgument(ctx: CallArgumentContext): EirNode = {
    enter(
      EirCallArgument(null, ctx.Ampersand() != null)(parent),
      (arg: EirCallArgument) => {
        arg.expr = visitAs[EirExpressionNode](ctx.expression())
      }
    )
  }

  override def visitPostfixExpression(
      ctx: PostfixExpressionContext
  ): EirExpressionNode = {
    if (ctx.identifier() != null) {
      enter(
        EirScopedSymbol[EirNode](null, null)(parent),
        (f: EirScopedSymbol[EirNode]) => {
          f.target = visitAs[EirExpressionNode](
            Option(ctx.postfixExpression()).getOrElse(ctx.proxySelfExpression())
          )
          f.pending = symbolize[EirNamedNode](ctx.identifier())
        }
      )
    } else if (ctx.arrArgs != null) {
      enter(
        EirArrayReference(parent, null, null),
        (f: EirArrayReference) => {
          f.target = visitAs[EirExpressionNode](ctx.postfixExpression())
          f.args = ctx.arrArgs
            .mapOrEmpty(_.sliceExpression, visitAs[EirExpressionNode])
        }
      )
    } else if (ctx.LeftParen() != null) {
      enter(
        EirFunctionCall(parent, null, null, null),
        (f: EirFunctionCall) => {
          f.target = visitAs[EirExpressionNode](ctx.postfixExpression())
          f.args =
            ctx.fnArgs.mapOrEmpty(_.callArgument, visitAs[EirCallArgument])
          f.types = specializationToList(ctx.specialization())
        }
      )
    } else {
      visitAs[EirExpressionNode](ctx.primaryExpression())
    }
  }

  override def visitTupleType(ctx: TupleTypeContext): EirResolvable[EirType] = {
    if (ctx.multiply != null) {
      enter(
        EirTupleMultiply(null, null)(parent),
        (x: EirTupleMultiply) => {
          x.lhs = visitAs[EirResolvable[EirType]](ctx.tupleType())
          x.rhs = visitAs[EirExpressionNode](ctx.staticPrimaryExpression())
        }
      )
    } else {
      ctx.typeList().toList.toTupleType()(parent)
    }
  }

  override def visitTemplateType(
      ctx: TemplateTypeContext
  ): EirResolvable[EirType] = {
    symbolizeType(ctx.identifierExpression())
  }

  override def visitProxyType(ctx: ProxyTypeContext): EirResolvable[EirType] = {
    val base = () => visitAs[EirResolvable[EirType]](ctx.templateType())
    Option(ctx.proxySuffix()) match {
      case Some(proxySuffix) =>
        val collective = Option(proxySuffix.CollectiveKwd()).map(_.getText)
        val prefix = Option(proxySuffix.prefix).map(_.getText)
        enter(
          EirProxyType(parent, null, collective, kindFrom(prefix, collective)),
          (ty: EirProxyType) => {
            ty.base = base()
          }
        )
      case None => base()
    }
  }

  override def visitPattern(ctx: PatternContext): EirNode = {
    if (ctx.id != null) {
      enter(
        EirIdentifierPattern(parent, ctx.id.getText, null),
        (i: EirIdentifierPattern) => {
          i.ty = Option(ctx.basicType())
            .map(visitAs[EirResolvable[EirType]])
            .getOrElse(EirPlaceholder(i.declarations.headOption))
        }
      )
    } else if (ctx.constant() != null) {
      enter(
        EirExpressionPattern(parent, null),
        (e: EirExpressionPattern) => {
          e.expression = enter(
            EirBinaryExpression(parent, null, "==", null),
            (b: EirBinaryExpression) => {
              b.lhs = EirSymbol[EirNamedNode](parent, List("_"))
              b.rhs = visitAs[EirExpressionNode](ctx.constant())
            }
          )
        }
      )
    } else if (ctx.expression() != null) {
      enter(
        EirExpressionPattern(parent, null),
        (e: EirExpressionPattern) => {
          e.expression = visitAs[EirExpressionNode](ctx.expression())
        }
      )
    } else if (ctx.fn != null) {
      enter(
        EirExtractorPattern(parent, null, null),
        (x: EirExtractorPattern) => {
          x.identifier = visitAs[EirExpressionNode](ctx.fn)
          x.list = visitAs[EirPatternList](ctx.patternList())
        }
      )
    } else {
      super.visit(ctx)
    }
  }

  override def visitPatternList(ctx: PatternListContext): EirPatternList = {
    val patterns = Option(ctx).map(_.pattern()).map(_.asScala.toList)
    enter(
      EirPatternList(parent, null),
      (p: EirPatternList) => {
        p.patterns = patterns.map(_.map(visitAs[EirPattern])).getOrElse(Nil)
      }
    )
  }

  override def visitCaseStatement(ctx: CaseStatementContext): EirMatchCase = {
    enter(
      EirMatchCase(parent, null, null, null),
      (m: EirMatchCase) => {
        m.patterns = visitAs[EirPatternList](ctx.patternList())
        m.condition = Option(ctx.condition).map(visitAs[EirExpressionNode])
        m.body = bodyToOptional(ctx.body)
      }
    )
  }

  override def visitMatchStatement(ctx: MatchStatementContext): EirMatch = {
    enter(
      EirMatch(parent, null, null),
      (m: EirMatch) => {
        m.expression = visitAs[EirExpressionNode](ctx.expression())
        m.cases = ctx.caseStatement().asScala.map(visitAs[EirMatchCase]).toList
      }
    )
  }

  def symbolize[T <: EirNamedNode: ClassTag, U <: ParseTree](
      identifiers: java.util.List[U]
  ): EirSymbol[T] = {
    EirSymbol[T](parent, identifiers.toStringList)
  }

  def symbolize[T <: EirNamedNode: ClassTag](
      identifier: ParseTree
  ): EirSymbol[T] = {
    EirSymbol[T](parent, List(identifier.getText))
  }

  def symbolizeType[T <: ParserRuleContext](
      ctx: IdentifierExpressionContext
  ): EirResolvable[EirType] = parseIdentifier[EirNamedType](ctx)

  private def pop[T](): T = parents.pop().asInstanceOf[T]

  override def visitTupleExpression(
      ctx: TupleExpressionContext
  ): EirExpressionNode = EirTupleExpression.fromExpressions(
    parent,
    ctx.expressionList().mapOrEmpty(_.expression, visitAs[EirExpressionNode])
  )

  override def visitLambdaExpression(
      ctx: LambdaExpressionContext
  ): EirExpressionNode = {
    enter(
      EirLambdaExpression(parent, null, null),
      (f: EirLambdaExpression) => {
        f.args = ctx
          .functionArgumentList()
          .mapOrEmpty(_.functionArgument, visitFunctionArgument)
        f.body = Option(ctx.block())
          .map(visitBlock)
          .getOrElse(
            AstManipulation.encloseNodes(
              visitAs[EirExpressionNode](ctx.expression)
            )(addReturn = true)
          )
      }
    )
  }

  def visitTernaryLikeExpression(
      test: ParseTree,
      ifTrue: ParseTree,
      ifFalse: ParseTree
  ): EirExpressionNode = {
    if (ifTrue != null && ifFalse != null) {
      enter(
        EirTernaryOperator(parent, null, null, null),
        (e: EirTernaryOperator) => {
          e.test = visitAs[EirExpressionNode](test)
          e.ifTrue = visitAs[EirExpressionNode](ifTrue)
          e.ifFalse = visitAs[EirExpressionNode](ifFalse)
        }
      )
    } else {
      visitAs[EirExpressionNode](test)
    }
  }

  override def visitConditionalExpression(
      ctx: ConditionalExpressionContext
  ): EirExpressionNode = {
    visitTernaryLikeExpression(
      ctx.unaryExpression(),
      ctx.expression(),
      ctx.conditionalExpression()
    )
  }

  override def visitNewExpression(ctx: NewExpressionContext): EirNew = {
    enter(
      EirNew(parent, null, null),
      (n: EirNew) => {
        n.target = visitAs[EirResolvable[EirType]](ctx.`type`())
        if (ctx.tupleExpression() != null) {
          n.args = ctx
            .tupleExpression()
            .expressionList()
            .mapOrEmpty(_.expression, visitAs[EirExpressionNode])
        }
      }
    )
  }

  override def visitUnaryExpression(
      ctx: UnaryExpressionContext
  ): EirExpressionNode = {
    visitPrefixLikeExpression(Option(ctx.PrefixOp()), ctx.simpleExpression())
  }

  override def visitAwaitExpression(ctx: AwaitExpressionContext): EirNode = {
    enter(
      EirAwait(parent, null),
      (a: EirAwait) => {
        a.target = visitAs[EirExpressionNode](ctx.postfixExpression())
      }
    )
  }

  override def visitReturnStatement(ctx: ReturnStatementContext): EirNode = {
    enter(
      EirReturn(parent, null),
      (r: EirReturn) => {
        r.expression = Option(ctx.expression())
          .map(visitAs[EirExpressionNode])
          .getOrElse(globals.unitLiteral(parent))
      }
    )
  }

  override def visitBody(ctx: BodyContext): EirNode = {
    Option(ctx.statement()).map(visit(_)).orNull
  }

  def bodyToOptional(ctx: BodyContext): Option[EirBlock] = {
    forceEnclosed(Option(visit(ctx)))
  }

  private def visitInfixLikeExpression[T <: ParseTree](
      ctx: T
  ): EirNode = {
    val exprs = flattenInfix(ctx)

    exprs match {
      case Left(x) :: Nil => x
      case _              => sortInfixes(exprs, this.formBinaryExpression)
    }
  }

  override def visitInfixExpression(ctx: InfixExpressionContext): EirNode =
    visitInfixLikeExpression(ctx)

  override def visitStaticExpression(ctx: StaticExpressionContext): EirNode =
    visitInfixLikeExpression(ctx)

  override def visitStaticPrimaryExpression(
      ctx: StaticPrimaryExpressionContext
  ): EirExpressionNode = {
    super.visitStaticPrimaryExpression(ctx) match {
      case x: EirExpressionNode => x
      // TODO -- this isn't the best solution, but unsure what would be better?
      case x: EirResolvable[_] => EirLiteralSymbol(x)(parent)
      case x                   => Errors.incorrectType(x, classOf[EirLiteral[_]])
    }
  }

  def visitPrefixLikeExpression(
      opt: Option[ParseTree],
      rhs: ParseTree
  ): EirExpressionNode = {
    opt match {
      case Some(op) => enter(
          EirUnaryExpression(parent, op.getText, null),
          (x: EirUnaryExpression) => {
            x.rhs = visitAs[EirExpressionNode](rhs)
          }
        )
      case None => visitAs[EirExpressionNode](rhs)
    }
  }

  override def visitStaticConditionalExpression(
      ctx: StaticConditionalExpressionContext
  ): EirExpressionNode = {
    visitTernaryLikeExpression(
      ctx.staticPrefixExpression(),
      ctx.staticExpression(),
      ctx.staticConditionalExpression()
    )
  }

  override def visitStaticPrefixExpression(
      ctx: StaticPrefixExpressionContext
  ): EirExpressionNode = {
    visitPrefixLikeExpression(
      Option(ctx.PrefixOp()),
      ctx.staticPostfixExpression()
    )
  }

  override def visitStaticPostfixExpression(
      ctx: StaticPostfixExpressionContext
  ): EirExpressionNode = {
    if (ctx.staticPrimaryExpression() != null) {
      visitAs[EirExpressionNode](ctx.staticPrimaryExpression())
    } else {
      enter(
        EirArrayReference(parent, null, Nil),
        (ref: EirArrayReference) => {
          ref.target = visitAs[EirExpressionNode](ctx.staticPostfixExpression())
          ref.args = ctx
            .staticExpressionList()
            .staticExpression()
            .asScala
            .map(visitAs[EirExpressionNode])
            .toList
        }
      )
    }
  }

  override def visitStaticTupleExpression(
      ctx: StaticTupleExpressionContext
  ): EirNode = {
    val expressions = ctx.staticExpressionList().staticExpression()
    if (expressions.isEmpty) EirUnitLiteral()(parent)
    else if (expressions.size() == 1)
      visitAs[EirExpressionNode](expressions.get(0))
    else {
      enter(
        EirTupleExpression(parent, null),
        (t: EirTupleExpression) => {
          t.expressions =
            expressions.asScala.map(visitAs[EirExpressionNode]).toList
        }
      )
    }
  }

  override def visitInheritanceDecl(ctx: InheritanceDeclContext): EirNode = {
    val base: EirClassLike = parent.to[EirClassLike].get
    val children: List[ParseTree] =
      Option(ctx.children).toIterable.flatMap(_.asScala).toList
    val grouped: List[List[ParseTree]] = children.grouped(2).toList
    for (List(kwd, ty) <- grouped) {
      kwd.getText match {
        case "extends" => base.extendsThis = Some(
            visitAs[EirResolvable[EirType]](ty.asInstanceOf[TypeContext])
          )
        case "with" | "and" => base.implementsThese ++= List(
            visitAs[EirResolvable[EirType]](ty.asInstanceOf[TypeContext])
          )
      }
    }
    null
  }

  override def visitWhileLoop(ctx: WhileLoopContext): EirWhileLoop = {
    enter(
      EirWhileLoop(parent, null, null),
      (l: EirWhileLoop) => {
        l.condition = visitAs[EirExpressionNode](ctx.expression())
        l.body = bodyToOptional(ctx.body())
      }
    )
  }

  override def visitForLoop(ctx: ForLoopContext): EirForLoop = {
    enter(
      EirForLoop(parent, null, null),
      (f: EirForLoop) => {
        visitLoopHeader(ctx.loopHeader())
        f.body = bodyToOptional(ctx.body())
      }
    )
  }

  override def visitLoopHeader(ctx: LoopHeaderContext): EirNode = {
    val forLoop = parent.to[EirForLoop].get
    forLoop.header =
      if (ctx.variableDeclaration() != null) {
        val decl =
          Option(ctx.variableDeclaration()).map(visitVariableDeclaration)
        EirCStyleHeader(
          decl,
          Option(ctx.test).map(visitAs[EirExpressionNode]),
          Option(ctx.incr).map(visitAs[EirExpressionNode])
        )
      } else {
        val decl =
          visitDeclaration(fromJava(ctx.decltype()), None, isFinal = true)
        EirForAllHeader(
          parent,
          Some(decl),
          visitAs[EirExpressionNode](ctx.iter)
        )
      }
    null
  }

  override def visitLambdaType(ctx: LambdaTypeContext): EirType = {
    enter(
      EirLambdaType(parent, null, null, Nil, None),
      (l: EirLambdaType) => {
        val head = visitAs[EirResolvable[EirType]](ctx.head)
        l.from = head match {
          case t: EirTupleType => t.children
          case t               => List(t)
        }
        l.to = visitAs[EirResolvable[EirType]](ctx.tail)
      }
    )
  }

  override def visitAwaitManyStatement(
      ctx: AwaitManyStatementContext
  ): EirAwaitMany = {
    val waitAll = ctx.AllKwd() != null
    enter(
      EirAwaitMany(waitAll, Nil)(parent),
      (x: EirAwaitMany) => {
        x.children = ctx.mapOrEmpty(_.whenStatement, visitAs[EirSdagWhen])
      }
    )
  }

  def specializationToList(ctx: ParseTree): List[EirResolvable[EirType]] = {
    ctx match {
      case null                     => Nil
      case s: StartSpecListContext  => specializationToList(s, None)
      case s: SpecializationContext => specializationToList(s.endSpecList())
      case s: EndSpecListContext    => specializationToList(s.init, Option(s.last))
      case s: QualEndSpecListContext =>
        specializationToList(s.init, Option(s.last))
      case _ => Errors.unreachable()
    }
  }

  def specializationToList(
      start: StartSpecListContext,
      end: Option[StartSpecListContext]
  ): List[EirResolvable[EirType]] = {
    val specs = start.specializationElement.asScala
    val init = specs.init.map(visitAs[EirResolvable[EirType]])
    val last = (specs.lastOption, end) match {
      case (Some(a), Some(b)) =>
        // TODO use symbol here?
        enter(
          EirTemplatedType(parent, null, null),
          (t: EirTemplatedType) => {
            t.base = visitAs[EirResolvable[EirType]](a)
            t.args = specializationToList(b)
          }
        )
      case (Some(a), None) => visitAs[EirResolvable[EirType]](a)
      case _               => Errors.cannotParse(start)
    }
    init.toList :+ last
  }

  def parseIdentifier[A <: EirNamedNode: ClassTag](
      ctx: IdentifierExpressionContext
  ): EirResolvable[A] = {
    type goalType = EirExpressionNode with EirResolvable[A]

    def conv2expr(s: EirResolvable[A] with EirSpecialization): goalType =
      s match {
        case x: goalType => x
        case x: A        => EirPlaceholder[A](None, Some(x))
      }

    val curr =
      Option(ctx.specialization).orElse(Option(ctx.qualEndSpecList())) match {
        case Some(spec) => conv2expr(
            enter(
              mkSpecialization[A](parent),
              (s: EirSpecialization) => {
                s.types = specializationToList(spec)
                s.setBase(
                  symbolize[
                    A with EirSpecializable,
                    IdentifierContext
                  ](
                    ctx.fqn().identifier()
                  )
                )
              }
            )
          )
        case None => symbolize[A, IdentifierContext](ctx.fqn().identifier())
      }

    Option(ctx.identifierExpression())
      .map(next => {
        enter(
          EirScopedSymbol[A](curr, null)(parent),
          (r: EirScopedSymbol[A]) => {
            r.isStatic = true
            r.pending = visitAs[EirResolvable[A]](next)
          }
        )
      })
      .getOrElse(curr)
  }

  override def visitIdentifierExpression(
      ctx: IdentifierExpressionContext
  ): EirResolvable[EirNode] = parseIdentifier[EirNamedNode](ctx)

  override def visitUsingStatement(ctx: UsingStatementContext): EirNode = {
    enter(
      EirTypeAlias(ctx.identifier().getText, null, null)(parent),
      (x: EirTypeAlias) => {
        x.templateArgs = visitTemplateDeclaration(ctx.templateDecl())
        x.value = visitAs[EirLiteral[_]](ctx.staticExpression())
      }
    )
  }

  override def visitProxySelfExpression(
      ctx: ProxySelfExpressionContext
  ): EirNode = {
    symbolize[EirNamedNode](ctx)
  }

  override def visitSelfExpression(ctx: SelfExpressionContext): EirNode = {
    Option(ctx.proxySelfExpression())
      .map(visit(_))
      .getOrElse({
        symbolize[EirNamedNode](ctx.SelfKwd())
      })
  }

  override def visitPrimaryExpression(
      ctx: PrimaryExpressionContext
  ): EirExpressionNode = {
    assert(ctx.children.size() == 1)
    visitAs[EirExpressionNode](ctx.children.get(0))
  }

  override def visitBoolLiteral(ctx: BoolLiteralContext): EirLiteral[_] = {
    val value =
      Option(ctx.FalseKwd()).getOrElse(ctx.TrueKwd()).getText.toBoolean
    EirBooleanLiteral(value)(parent)
  }

  override def visitConstant(ctx: ConstantContext): EirLiteral[_] = {
    if (ctx.IntegerConstant() != null) {
      val raw = ctx.IntegerConstant().getText
      EirIntegerLiteral(raw.toInt)(parent)
    } else if (ctx.FloatingConstant() != null) {
      val raw = ctx.FloatingConstant().getText
      EirFloatLiteral(raw.toFloat)(parent)
    } else if (Option(ctx.StringLiteral()).exists(_.size() >= 1)) {
      EirStringLiteral(
        ctx.StringLiteral().asScala.map(_.getText).reduce(_ + _)
      )(parent)
    } else if (ctx.boolLiteral() != null) {
      visitBoolLiteral(ctx.boolLiteral())
    } else {
      assert(ctx.CharacterConstant() != null)
      val raw = ctx.CharacterConstant().getText
      val escaped = StringContext.processEscapes(raw.slice(1, raw.length - 1))
      EirCharacterLiteral(escaped.head)(parent)
    }
  }

  override def visitWhenStatement(ctx: WhenStatementContext): EirNode = {
    enter(
      EirSdagWhen(null, null, null)(parent),
      (n: EirSdagWhen) => {
        val fns = ctx.whenFnList().whenFn().asScala
        n.patterns = fns.toList.map(x =>
          (
            visitAs[EirSymbol[EirNamedNode]](x.identifierExpression()),
            Option(x.patternList())
              .map(visitAs[EirPatternList])
              .getOrElse(EirPatternList(parent, Nil))
          )
        )
        n.condition = Option(ctx.condition).map(visitAs[EirExpressionNode])
        n.body = Option(ctx.body()).map(visitAs[EirNode])
      }
    )
  }
}
