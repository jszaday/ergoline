package edu.illinois.cs.ergoline.parsing

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.literals.{
  EirIntegerLiteral,
  EirLiteral,
  EirLiteralSymbol,
  EirUnitLiteral
}
import edu.illinois.cs.ergoline.ast.types.{EirNamedType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.parsing.syntax.Basics._
import edu.illinois.cs.ergoline.parsing.syntax.{Basics, Keywords}
import edu.illinois.cs.ergoline.parsing.syntax.Keywords._
import edu.illinois.cs.ergoline.resolution.{
  EirPlaceholder,
  EirResolvable,
  Modules
}
import fastparse.SingleLineWhitespace._
import fastparse._

import edu.illinois.cs.ergoline.Visitor.{isAssignOperator, sortInfixes}

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Parser {
  def scopeSymbols[A <: EirNode: ClassTag](
      s: Seq[EirSymbolLike[A]],
      isStatic: Boolean
  ): EirSymbolLike[A] = {
    s match {
      case Nil         => ???
      case head :: Nil => head
      case head :: tail => {
        val s = EirScopedSymbol(head, scopeSymbols(tail, isStatic))(None)
        s.isStatic = isStatic
        s
      }
    }
  }

  def extractArgs(node: Option[EirExpressionNode]): List[EirExpressionNode] = {
    node match {
      case None                        => Nil
      case Some(_: EirUnitLiteral)     => Nil
      case Some(x: EirTupleExpression) => x.expressions
      case Some(x)                     => List(x)
    }
  }

  def defaultAccessorFor(node: EirNode): EirAccessibility.Value = {
    node match {
      case _: EirImport => EirAccessibility.Private
      case _            => EirAccessibility.Public
    }
  }

  def mkDeclaration(decls: Seq[String]): Option[EirNode] = {
    decls match {
      case Nil => None
      case _ =>
        Some(mkDeclaration(decls.map((false, _, None)), None, isFinal = true))
    }
  }

  def forceEnclosed(node: EirNode, addReturn: Boolean): EirBlock = {
    node match {
      case x: EirBlock => x
      case x: EirExpressionNode =>
        EirBlock(None, List(if (addReturn) EirReturn(None, x) else x))
      case _ if !addReturn => EirBlock(None, List(node))
      case _               => ???
    }
  }

  def mkDeclaration(
      decls: Seq[DTriple],
      expr: Option[EirExpressionNode],
      isFinal: Boolean
  ): EirNode = {
    def helper(decl: DTriple, idx: Option[Int]): EirDeclaration = {
      EirDeclaration(
        None,
        isFinal,
        decl._2,
        decl._3.getOrElse(EirPlaceholder(None)), {
          (idx, expr) match {
            case (Some(i), Some(x)) =>
              Some(EirArrayReference(None, x, List(EirIntegerLiteral(i)(None))))
            case (_, expr) => expr
          }
        }
      )
    }

    if (decls.length >= 2) {
      EirMultiDeclaration(decls.zipWithIndex.toList.map { case (d, i) =>
        helper(d, Some(i))
      })(None)
    } else {
      assert(decls.length == 1)
      helper(decls.head, None)
    }
  }

  def ImportStatement[_: P]: P[EirImport] =
    P(`import` ~/ Id.rep(sep = "::", min = 1) ~ Semi).map { ids =>
      EirImport(None, ids.toList, publicOverride = false)
    }

  def UsingStatement[_: P]: P[EirTypeAlias] = P(
    `using` ~/ Id ~ TDeclaration.? ~ "=" ~ ConstExpr ~ Semi
  ).map { case (id, tArgs, expr) =>
    EirTypeAlias(id, tArgs.getOrElse(Nil), expr)(None)
  }

  def Qualified[p: P, A <: EirNamedNode: ClassTag]: P[EirSymbolLike[A]] = P(
    Specialized[p, A].rep(min = 1, sep = "::")
  ).map(scopeSymbols(_, isStatic = true))

  def Specialized[p: P, A <: EirNamedNode: ClassTag]: P[EirSymbolLike[A]] =
    P(Identifier[p, A] ~ Specialization.?).map {
      case (id, Some(sp)) => EirSpecializedSymbol[A](
          None,
          id.asInstanceOf[EirResolvable[A with EirSpecializable]],
          sp.toList
        )
      case (id, None) => id
    }

  def Expression[_: P]: P[EirExpressionNode] = P(InfixExpr)

  def WhileLoop[_: P]: P[EirWhileLoop] = P(
    `while` ~ `(` ~ Expression ~ `)` ~ OptionalStatement
  ).map { case (expr, body) => EirWhileLoop(None, expr, body) }

  def DoWhileLoop[_: P]: P[EirDoWhileLoop] = P(
    `do` ~ OptionalStatement ~ `while` ~ `(` ~ Expression ~ `)` ~ Semi
  ).map { case (body, expr) => EirDoWhileLoop(None, expr, body) }

  def ForAllHeader[_: P]: P[EirForLoopHeader] = P(
    Id.rep(min = 1, sep = ",") ~ `<-` ~ Expression
  ).map { case (symbols, expr) =>
    EirForAllHeader(None, mkDeclaration(symbols), expr)
  }

  def CStyleHeader[_: P]: P[EirForLoopHeader] = P(
    (!:(VarDeclaration) | `;`(None)) ~ Expression.? ~ Semi ~ Expression.?
  ).map { case (decl, test, incr) => EirCStyleHeader(decl, test, incr) }

  def InnerStatement[_: P]: P[EirNode] = P(
    Block | ForLoop | DoWhileLoop | WhileLoop | IfElseStatement | NamespaceMember | InnerDeclaration | ReturnStatement | (Expression ~ Semi)
  )

  def Statement[_: P]: P[EirNode] = P(Annotations.? ~ InnerStatement).map {
    case (as, node) => addAnnotations(node, as)
  }

  def ReturnStatement[_: P]: P[EirReturn] = P(`return` ~ OptionalExpression)
    .map(expr => EirReturn(None, expr.getOrElse(globals.unitLiteral(None))))

  def IfElseStatement[_: P]: P[EirIfElse] = P(
    `if` ~ `(` ~ Expression ~ `)` ~ OptionalStatement ~ (`else` ~ OptionalStatement).?
  ).map { case (expr, ifTrue, ifFalse) =>
    EirIfElse(None, expr, ifTrue, ifFalse.flatten)
  }

  def Block[_: P]: P[EirBlock] = P(`{` ~ OptionalStatement.rep(0) ~ `}`)
    .map(_.flatten.toList)
    .map(EirBlock(None, _))

  def OptionalExpression[_: P]: P[Option[EirExpressionNode]] =
    P((!:(Expression) ~ Semi) | `;`(None))

  def OptionalStatement[_: P]: P[Option[EirNode]] = P(!:(Statement) | `;`(None))

  def OptionalBlock[_: P]: P[Option[EirBlock]] = P(!:(Block) | `;`(None))

  def ForLoop[_: P]: P[EirForLoop] = P(
    `for` ~ `(` ~ (CStyleHeader | ForAllHeader) ~ `)` ~ OptionalStatement
  ).map { case (hdr, body) => EirForLoop(None, hdr, body) }

  def Specialization[p: P]: P[Seq[EirSymbolLike[EirType]]] =
    P("<" ~ Specialized[p, EirNamedType].rep(min = 0, sep = ",") ~ ">")

  def Class[_: P]: P[EirClass] = P(ClassKind ~/ Id ~ TDeclaration.? ~ ClassBody)
    .map { case (kind, id, args, body) =>
      EirClass(
        None,
        body.toList,
        id,
        args.getOrElse(Nil),
        None,
        Nil,
        None,
        kind
      )
    }

  def ClassKind[_: P]: P[EirClassKind] = P(`class` | `object` | `struct`).map {
    case "class"  => EirReferenceKind
    case "object" => EirSingletonKind
    case "struct" => EirValueKind
  }

  def ClassBody[_: P]: P[Seq[EirMember]] =
    P((`{` ~ ClassMember.rep(0) ~ `}`) | `;`(Nil))

  def Annotation[_: P]: P[EirAnnotation] = P(
    "@" ~ Id ~ Newline.rep(0)
  ).map(EirAnnotation(_, Map()))

  def Annotations[_: P]: P[Seq[EirAnnotation]] = P(Annotation.rep(1))

  def FieldDeclaration[_: P]: P[EirDeclaration] = P(
    (`var` | `val`) ~/ Id ~ ":" ~ Type ~ ("=" ~ Expression).? ~ Semi
  ).map { case (varOrVal, id, ty, expr) =>
    EirDeclaration(None, isFinal = varOrVal == "val", id, ty, expr)
  }

  def Namespace[_: P]: P[EirNamespace] = P(
    namespace ~ Id.rep(sep = "::", min = 1) ~ (`;`(Nil) | (`{` ~ ProgramMember
      .rep(0) ~ `}`))
  ).map { case (ids, nodes: Seq[EirNode]) =>
    val scope = Modules.retrieve(ids.toList, EirGlobalNamespace)
    scope.children ++= nodes
    scope
  }

  def Package[_: P]: P[Seq[String]] =
    P(Keywords.`package` ~ Id.rep(sep = "::", min = 1) ~ Semi)

  def Program[_: P]: P[(Option[Seq[String]], Seq[EirNode])] =
    Package.? ~ ProgramMember.rep(0) ~ End

  def addAnnotations[A <: EirNode](
      node: A,
      as: Option[Seq[EirAnnotation]]
  ): A = {
    as.foreach(node.annotations ++= _)
    node
  }

  def ProgramMember[_: P]: P[EirNode] =
    P(Annotations.? ~ (Namespace | NamespaceMember)).map { case (as, node) =>
      addAnnotations(node, as)
    }

  def NamespaceMember[_: P]: P[EirNode] =
    P(Class | FnDeclaration | ImportStatement | UsingStatement)

  def ClassMember[_: P]: P[EirMember] = P(
    Annotations.? ~ AccessModifier.? ~ (`override` | `static`).? ~ (FieldDeclaration | NamespaceMember)
  ).map { case (as, access, ovOrSt, body) =>
    ((m: EirMember) => {
      m.isStatic = ovOrSt.contains("static")
      m.isOverride = ovOrSt.contains("override")
      addAnnotations(m, as)
    })(EirMember(None, body, access.getOrElse(defaultAccessorFor(body))))
  }

  def TDeclaration[_: P]: P[List[EirTemplateArgument]] =
    P("<" ~ TDeclarationArg.rep(min = 0, sep = ",") ~ ">").map(_.toList)

  def TDeclarationArg[_: P]: P[EirTemplateArgument] = P(
    Id ~ `...`.!.? ~ Bounds.? ~ (":" ~ Type).? ~ ("=" ~ ConstExpr).?
  ).map { case (id, ellipses, bounds, declTy, defaultVal) =>
    EirTemplateArgument(id, ellipses.nonEmpty, bounds, declTy, defaultVal)
  }

  def Bounds[_: P]
      : P[(Option[EirResolvable[EirType]], Option[EirResolvable[EirType]])] =
    ((`>:` ~ Type) ~ (?:(`<:`) ~ Type).? |
      (`<:` ~ Type) ~ (?:(`>:`) ~ Type).?).map {
      case (op, lval, rval) if op == LowerBound => (Some(lval), rval)
      case (op, lval, rval) if op == UpperBound => (rval, Some(lval))
      case _                                    => ???
    }

  def Identifier[_: P, A <: EirNamedNode: ClassTag]: P[EirSymbol[A]] =
    Id.map(x => EirSymbol[A](None, List(x)))

  def Id[_: P]: P[String] = P(WL ~ Basics.Id)

  def Constant[_: P]: P[EirLiteral[_]] =
    P(CharIn("0-9").rep(1).!).map(_.toInt).map(EirIntegerLiteral(_)(None))

  def ConstantSymbol[_: P]: P[EirLiteralSymbol] =
    Type.map(EirLiteralSymbol(_)(None))

  def Type[p: P]: P[EirResolvable[EirType]] = Qualified[p, EirNamedType]

  def ConstExpr[_: P]: P[EirLiteral[_]] = P(Constant | ConstantSymbol)

  def ProxySuffix[_: P]: P[String] = P(`@` | `[@]` | `{@}`)

  def ProxySelfExpr[_: P]: P[EirSymbol[EirNamedNode]] =
    P(`self` ~/ ProxySuffix).map { case (self, suffix) =>
      EirSymbol[EirNamedNode](None, List(self + suffix))
    }

  def SelfExpr[_: P]: P[EirSymbol[EirNamedNode]] =
    ProxySelfExpr | P(`self`).map { self =>
      EirSymbol[EirNamedNode](None, List(self))
    }

  def PrimaryExpr[p: P]: P[EirExpressionNode] =
    P(SelfExpr | Qualified[p, EirNamedNode] | Constant | TupleExpr | LambdaExpr)

  def ProxyAccessor[_: P]: P[EirExpressionNode] = P(ProxySelfExpr ~ Id).map {
    case (self, id) => EirScopedSymbol(self, EirSymbol(None, List(id)))(None)
  }

  def FnCallArg[_: P]: P[EirCallArgument] = P("&".!.? ~ Expression).map {
    case (amp, expr) => EirCallArgument(expr, amp.nonEmpty)(None)
  }

  sealed trait ExprSuffixTuple

  case class CallSuffixTuple(
      sp: List[EirResolvable[EirType]],
      args: List[EirCallArgument]
  ) extends ExprSuffixTuple

  case class AccessSuffixTuple(id: String) extends ExprSuffixTuple

  case class AtSuffixTuple(exprs: Seq[EirExpressionNode])
      extends ExprSuffixTuple

  def CallSuffix[_: P]: P[ExprSuffixTuple] = P(
    Specialization.? ~ `(` ~/ FnCallArg.rep(min = 0, sep = ",") ~ `)`
  ).map { case (sp, args) =>
    CallSuffixTuple(sp.map(_.toList).getOrElse(Nil), args.toList)
  }

  def AccessSuffix[_: P]: P[ExprSuffixTuple] =
    P("." ~/ Id).map(AccessSuffixTuple)

  def Slice[_: P]: P[EirExpressionNode] = P(Expression)

  def AtSuffix[_: P]: P[ExprSuffixTuple] =
    P("[" ~/ Slice.rep(min = 0, sep = ",") ~ "]").map(AtSuffixTuple)

  def ExprSuffix[_: P]: P[ExprSuffixTuple] =
    P(CallSuffix | AccessSuffix | AtSuffix)

  def applySuffix(
      base: EirExpressionNode,
      suffix: ExprSuffixTuple
  ): EirExpressionNode = {
    suffix match {
      case CallSuffixTuple(sp, args) => EirFunctionCall(None, base, args, sp)
      case AccessSuffixTuple(id) =>
        EirScopedSymbol(base, EirSymbol(None, List(id)))(None)
      case AtSuffixTuple(args) => EirArrayReference(None, base, args.toList)
    }
  }

  @tailrec
  def applySuffixes(
      base: EirExpressionNode,
      suffixes: Seq[ExprSuffixTuple]
  ): EirExpressionNode = {
    suffixes match {
      case Nil          => base
      case head :: tail => applySuffixes(applySuffix(base, head), tail)
    }
  }

  def PostfixExpr[_: P]: P[EirExpressionNode] = P(
    (ProxyAccessor | PrimaryExpr) ~ ExprSuffix.rep(0)
  ).map { case (expr, suffixes) => applySuffixes(expr, suffixes) }

  def UnaryExpr[_: P]: P[EirExpressionNode] = P(PrefixOp.? ~ PostfixExpr).map {
    case (None, expr)     => expr
    case (Some(op), expr) => EirUnaryExpression(None, op, expr)
  }

  def BasicExpr[_: P]: P[EirExpressionNode] = P(NewExpr | AwaitExpr | UnaryExpr)

  def ConditionalExpr[_: P]: P[EirExpressionNode] =
    P(BasicExpr ~ (`?` ~ Expression ~ ":" ~ ConditionalExpr).?).map {
      case (expr, None) => expr
      case (expr, Some((ifTrue, ifFalse))) =>
        EirTernaryOperator(None, expr, ifTrue, ifFalse)
    }

  def buildInfix(
      expr: EirExpressionNode,
      pairs: Seq[(String, EirExpressionNode)]
  ): EirExpressionNode = {
    def mkNode(
        lhs: EirExpressionNode,
        op: String,
        rhs: EirExpressionNode
    ): EirExpressionNode = {
      if (isAssignOperator(op)) EirAssignment(None, lhs, op, rhs)
      else EirBinaryExpression(None, lhs, op, rhs)
    }

    pairs match {
      case Nil              => expr
      case (op, rhs) :: Nil => mkNode(expr, op, rhs)
      case _ => sortInfixes(
          Left(expr) +: pairs.flatMap { case (op, rhs) =>
            Seq(Right(op), Left(rhs))
          },
          { case Left(lhs) :: Right(op) :: Left(rhs) :: Nil =>
            mkNode(lhs, op, rhs)
          }
        )
    }
  }

  def InfixExpr[_: P]: P[EirExpressionNode] = P(
    ConditionalExpr ~ (Id ~/ ConditionalExpr).rep(0)
  ).map { case (expr, pairs) => buildInfix(expr, pairs) }

  def NewExpr[_: P]: P[EirNew] = P(`new` ~/ Type ~ TupleExpr.?).map {
    case (ty, tuple) => EirNew(None, ty, extractArgs(tuple))
  }

  def AwaitExpr[_: P]: P[EirAwait] =
    P(await ~/ PostfixExpr).map(EirAwait(None, _))

  def TupleExpr[_: P]: P[EirExpressionNode] = P(
    `(` ~ Expression.rep(min = 0, sep = ",") ~ `)`
  ).map(s => EirTupleExpression.fromExpressions(None, s.toList))

  def LambdaExpr[_: P]: P[EirLambdaExpression] = P(
    `(` ~ FnArg.rep(min = 0, sep = ",") ~ `)` ~ "=>" ~/ (Block | Expression)
  ).map { case (args, body) =>
    EirLambdaExpression(
      None,
      args.toList,
      forceEnclosed(body, addReturn = true)
    )
  }

  def MatchExpr[_: P]: P[EirMatch] = P(
    `match` ~/ `(` ~ Expression ~ `)` ~ `{` ~ CaseStatement.rep(1) ~ `}`
  ).map { case (expr, cases) => EirMatch(None, expr, cases.toList) }

  def CaseStatement[_: P]: P[EirMatchCase] = P(
    `case` ~/ PatternList ~ (`if` ~ Expression).? ~ "=>" ~/ OptionalStatement
  ).map { case (list, expr, body) =>
    EirMatchCase(None, list, expr, body.map(forceEnclosed(_, addReturn = true)))
  }

  def PatternList[_: P]: P[EirPatternList] =
    P(Pattern.rep(min = 1, sep = ",")).map(s => EirPatternList(None, s.toList))

  def Pattern[_: P]: P[EirPattern] =
    P(IdPattern | ExtractorPattern | ConstantPattern | ExprPattern)

  def IdPattern[_: P]: P[EirIdentifierPattern] =
    P(Id ~ (":" ~ BasicType).?).map { case (id, ty) =>
      EirIdentifierPattern(None, id, ty.getOrElse(EirPlaceholder(None)))
    }

  def ExtractorPattern[p: P]: P[EirExtractorPattern] = P(
    Identifier[p, EirNamedNode] ~ `(` ~ PatternList ~ `)`
  ).map { case (id, list) => EirExtractorPattern(None, id, list) }

  def ConstantPattern[_: P]: P[EirExpressionPattern] =
    P(Constant).map { constant =>
      EirExpressionPattern(
        None,
        EirBinaryExpression(
          None,
          EirSymbol[EirNamedNode](None, List("_")),
          "==",
          constant
        )
      )
    }

  def ExprPattern[_: P]: P[EirExpressionPattern] =
    P(Expression).map(EirExpressionPattern(None, _))

  def BasicType[_: P]: P[EirResolvable[EirType]] = P(Type)

  def BasicArg[_: P]: P[(String, Option[String], EirResolvable[EirType])] =
    P(Id ~ ":" ~ "*".!.? ~ Type)

  def FnArg[_: P]: P[EirFunctionArgument] =
    P(("&" | "=").!.? ~ BasicArg).map { case (refOrEq, (id, exp, ty)) =>
      EirFunctionArgument(
        None,
        id,
        ty,
        isExpansion = exp.nonEmpty,
        isReference = refOrEq.contains("&"),
        isSelfAssigning = refOrEq.contains("=")
      )
    }

  def ImplicitArg[_: P]: P[EirFunctionArgument] =
    P(?:(`implicit`) ~ BasicArg).map { case (id, exp, ty) =>
      val arg = EirFunctionArgument(None, id, ty, isExpansion = exp.nonEmpty)
      arg.isImplicit = true
      arg
    }

  def FnDeclaration[_: P]: P[EirFunction] = P(
    `def` ~/ (`self` | Id) ~ TDeclaration.? ~ `(` ~ FnArg.rep(
      min = 0,
      sep = ","
    ) ~ `)` ~ (`(` ~ ImplicitArg.rep(
      min = 0,
      sep = ","
    ) ~ `)`).? ~ (":" ~ Type).? ~ WhereClause.? ~ OptionalBlock
  ).map { case (id, tArgs, fArgs, iArgs, retTy, where, body) =>
    EirFunction(
      None,
      body,
      id,
      tArgs.getOrElse(Nil),
      fArgs.toList,
      iArgs.getOrElse(Nil).toList,
      retTy.getOrElse(globals.unitSymbol),
      where
    )
  }

  def WhereClause[_: P]: P[EirExpressionNode] = P(where ~ ConstExpr)

  def AccessModifier[_: P]: P[EirAccessibility.Value] =
    P(`public` | `private` | `protected`).map(EirAccessibility.withName)

  type DTriple = (Boolean, String, Option[EirResolvable[EirType]])

  def Decltype[_: P]: P[DTriple] = P("&".!.? ~ Id ~ (":" ~ Type).?).map {
    case (amp, id, ty) => (amp.nonEmpty, id, ty)
  }

  def Decltypes[_: P]: P[List[DTriple]] =
    P(`(` ~ Decltype.rep(1).map(_.toList) ~ `)`) | Decltype.map(List(_))

  def ValDeclaration[_: P]: P[EirNode] = P(
    ?:(`val`) ~/ Decltypes ~ "=" ~ !:(Expression) ~ Semi
  ).map { case (triples, expr) => mkDeclaration(triples, expr, isFinal = true) }

  def VarDeclaration[_: P]: P[EirNode] =
    P(?:(`var`) ~/ Decltypes ~ ("=" ~ Expression).? ~ Semi).map {
      case (triples, expr) => mkDeclaration(triples, expr, isFinal = false)
    }

  def InnerDeclaration[_: P]: P[EirNode] =
    P(`implicit`.? ~ (ValDeclaration | VarDeclaration)).map {
      case (Some(_), m: EirMultiDeclaration) =>
        m.children.foreach(_.isImplicit = true); m
      case (Some(_), d: EirDeclaration) => d.isImplicit = true; d
      case (Some(_), _)                 => ???
      case (None, node)                 => node
    }
}
