package edu.illinois.cs.ergoline.parsing

import edu.illinois.cs.ergoline.Visitor.{
  isAssignOperator,
  kindFrom,
  mkSpecialization,
  sortInfixes
}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.literals._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.parsing.syntax.Basics._
import edu.illinois.cs.ergoline.parsing.syntax.Keywords._
import edu.illinois.cs.ergoline.parsing.syntax.Literals._
import edu.illinois.cs.ergoline.parsing.syntax.{Basics, Keywords}
import edu.illinois.cs.ergoline.passes.Processes.RichProcessesSyntax.RichSeq
import edu.illinois.cs.ergoline.resolution.{
  EirPlaceholder,
  EirResolvable,
  Modules
}
import edu.illinois.cs.ergoline.util.Errors
import fastparse.JavaWhitespace._
import fastparse._

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}

object Parser {
  def scopeSymbols[Type <: EirNamedNode: ClassTag, Goal <: EirResolvable[
    Type
  ]: ClassTag](
      s: Seq[Goal],
      isStatic: Boolean
  ): Goal = {
    def helper(seq: Seq[Goal]): Goal = {
      seq match {
        case Nil         => ???
        case head :: Nil => head
        case head :: tail =>
          val s = EirScopedSymbol[Type](Symbolize(head), helper(tail))(None)
          s.isStatic = isStatic
          s.asInstanceOf[Goal]
      }
    }

    helper(
      s.orderedPartition {
        case _: EirSymbol[_] => isStatic
        case _               => false
      }.flatMap {
        case (true, seq) => Option.when(seq.length <= 1)(seq) getOrElse {
            Seq({
              EirSymbol[Type](
                None, {
                  seq.map(_.asInstanceOf[EirSymbol[_]]).flatMap(_.qualifiedName)
                }
              ).asInstanceOf[Goal]
            })
          }
        case (false, seq) => seq
      }
    )
  }

  def extractTypes(
      node: EirResolvable[EirType]
  ): List[EirResolvable[EirType]] = {
    node match {
      case x: EirTupleType => x.children
      case x               => List(x)
    }
  }

  def defaultAccessorFor(node: EirNode): EirAccessibility.Value = {
    node match {
      case _: EirImport => EirAccessibility.Private
      case _            => EirAccessibility.Public
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
    `using` ~/ Id ~ TDeclaration.? ~ "=" ~ ConstExpr(guarded = false) ~ Semi
  ).map { case (id, tArgs, expr) =>
    EirTypeAlias(id, tArgs.getOrElse(Nil), expr)(None)
  }

  def Qualified[A <: EirNamedNode: ClassTag](implicit
      ctx: P[Any]
  ): P[EirResolvable[A]] = P(
    Index ~ Specialized[A].rep(min = 1, sep = "::")
  ).map { case (idx, seq) =>
    SetLocation(scopeSymbols[A, EirResolvable[A]](seq, isStatic = true), idx)
  }

  def Specialized[A <: EirNamedNode: ClassTag](implicit
      ctx: P[Any]
  ): P[EirResolvable[A]] = P(Index ~ Identifier[A] ~ Specialization.?).map {
    case (idx, id, Some(sp)) => SetLocation(
        mkSpecialization[A](
          None,
          id,
          sp.toList
        ),
        idx
      )
    case (idx, id, None) => SetLocation(id, idx)
  }

  def Expression[_: P](implicit
      static: Boolean = false
  ): P[EirExpressionNode] = {
    if (static) P(InfixExpr(0)) else P(MatchExpr | InfixExpr(0))
  }

  def WhileLoop[_: P]: P[EirWhileLoop] = P(
    `while` ~ `(` ~ Expression ~ `)` ~ OptionalStatement
  ).map { case (expr, body) => EirWhileLoop(None, expr, body) }

  def DoWhileLoop[_: P]: P[EirDoWhileLoop] = P(
    `do` ~ OptionalStatement ~ `while` ~ `(` ~ Expression ~ `)` ~ Semi
  ).map { case (body, expr) => EirDoWhileLoop(None, expr, body) }

  def ForAllHeader[_: P]: P[EirForLoopHeader] = P(
    Decltypes ~ `<-` ~ Expression
  ).map { case (symbols, expr) =>
    EirForAllHeader(
      None,
      Some(mkDeclaration(symbols, None, isFinal = true)),
      expr
    )
  }

  def CStyleHeader[_: P]: P[EirForLoopHeader] = P(
    (!:(VarDeclaration) | `;`(None)) ~/ Expression.? ~ Semi ~/ Expression.?
  ).map { case (decl, test, incr) => EirCStyleHeader(decl, test, incr) }

  def InnerStatement[_: P]: P[EirNode] = P(
    ForLoop | DoWhileLoop | WhileLoop | IfElseStatement | NamespaceMember |
      InnerDeclaration | ReturnStatement | AwaitManyStatement | WhenStatement | ExprStatement
  )

  def ExprStatement[_: P]: P[EirNode] =
    P((Block ~/ Semi.?) | MatchExpr | (Expression ~ Semi))

  def Statement[_: P]: P[EirNode] = P(Annotations.? ~ InnerStatement).map {
    case (as, node) => addAnnotations(node, as)
  }

  def ReturnStatement[_: P]: P[EirReturn] = P(`return` ~ OptionalExpression)
    .map(expr => EirReturn(None, expr.getOrElse(globals.unitLiteral(None))))

  def IfElseStatement[_: P]: P[EirIfElse] = P(
    `if` ~/ `(` ~ Expression ~ `)` ~ OptionalStatement ~ (`else` ~/ OptionalStatement).?
  ).map { case (expr, ifTrue, ifFalse) =>
    EirIfElse(None, expr, ifTrue, ifFalse.flatten)
  }

  def WhenStatement[_: P]: P[EirSdagWhen] = P(
    `when` ~/ WhenFn.rep(
      min = 1,
      sep = ","
    ) ~ (`if` ~/ Expression).? ~ "=>" ~ OptionalStatement
  ).map { case (fns, cond, body) =>
    EirSdagWhen(
      fns.toList.map {
        case (id, Some(list)) => (id, list)
        case (id, None)       => (id, EirPatternList(None, Nil))
      },
      cond,
      body
    )(None)
  }

  def AwaitManyStatement[_: P]: P[EirAwaitMany] = P(
    `await` ~ ("any" | "all").! ~/ `{` ~ WhenStatement.rep(1) ~ `}`
  ).map { case (anyOrAll, body) =>
    EirAwaitMany(anyOrAll == "all", body.toList)(None)
  }

  def WhenFn[_: P]: P[(EirSymbolLike[EirNamedNode], Option[EirPatternList])] =
    P(
      Identifier[EirNamedNode] ~ `(` ~/ PatternList.? ~ `)`
    )

  def Block[_: P]: P[EirBlock] =
    P(Index ~ `{` ~/ OptionalStatement.rep(0) ~ `}`)
      .map { case (idx, slst) =>
        SetLocation(EirBlock(None, slst.flatten.toList), idx)
      }

  def OptionalExpression[_: P]: P[Option[EirExpressionNode]] =
    P((!:(Expression) ~ Semi) | `;`(None))

  def OptionalStatement[_: P]: P[Option[EirNode]] = P(!:(Statement) | `;`(None))

  def OptionalBlock[_: P]: P[Option[EirBlock]] = P(!:(Block) | `;`(None))

  def ForLoop[_: P]: P[EirForLoop] = P(
    `for` ~ `(` ~ (CStyleHeader | ForAllHeader) ~ `)` ~ OptionalStatement
  ).map { case (hdr, body) => EirForLoop(None, hdr, body) }

  def ConstantFacade[_: P]: P[EirResolvable[EirType]] =
    P(Constant).map { EirConstantFacade(_)(None) }

  def Specialization[_: P]: P[Seq[EirResolvable[EirType]]] =
    P("<" ~ (ConstantFacade | Type).rep(min = 0, sep = ",") ~ ">")

  def Extends[_: P]: P[EirResolvable[EirType]] = {
    `extends` ~/ Type
  }

  def With[_: P]: P[EirResolvable[EirType]] = {
    `with` ~/ Type
  }

  def Class[_: P]: P[EirClassLike] = P(
    ClassKind ~/ Id ~ TDeclaration.? ~ Extends.? ~ With.rep(
      0
    ) ~ WhereClause.? ~ ClassBody
  ).map { case (abs, kind, id, args, ext, ints, wh, body) =>
    ((cls: EirClassLike) => {
      cls.isAbstract = abs
      cls
    })(if (kind == EirTraitKind) {
      EirTrait(
        None,
        body.toList,
        id,
        args.getOrElse(Nil),
        ext,
        ints.toList,
        wh
      )
    } else {
      EirClass(
        None,
        body.toList,
        id,
        args.getOrElse(Nil),
        ext,
        ints.toList,
        wh,
        kind
      )
    })
  }

  def ClassKind[_: P]: P[(Boolean, EirClassKind)] = P(
    (`abstract`.? ~ `class`) | `object`.map((None, _)) | `struct`.map(
      (None, _)
    ) | `trait`.map((None, _))
  ).map {
    case (a, "class")  => (a.nonEmpty, EirReferenceKind)
    case (_, "object") => (false, EirSingletonKind)
    case (_, "struct") => (false, EirValueKind)
    case (_, "trait")  => (true, EirTraitKind)
    case (_, _)        => Errors.unreachable()
  }

  def ClassBody[_: P]: P[Seq[EirMember]] =
    P((`{` ~/ ClassMember.rep(0) ~ `}`) | `;`(Nil))

  def AnnotationParameter[_: P]: P[(String, EirLiteral[_])] =
    P((`static` | Id) ~ ("=" ~/ Constant).?).map { case (id, opt) =>
      (id, opt.getOrElse(EirBooleanLiteral(value = true)(None)))
    }

  def Annotation[_: P]: P[EirAnnotation] = P(
    "@" ~ Id ~ (`(` ~/ AnnotationParameter.rep(min = 0, sep = ",") ~ `)`).?
  ).map { case (id, opts) =>
    EirAnnotation(id, opts.map(_.toMap).getOrElse(Map()))
  }

  def Annotations[_: P]: P[Seq[EirAnnotation]] = P(Annotation.rep(1))

  def FieldDeclaration[_: P]: P[EirDeclaration] = P(
    (`var` | `val`) ~/ Id ~ ":" ~ Type ~ ("=" ~ Expression).? ~ Semi
  ).map { case (varOrVal, id, ty, expr) =>
    EirDeclaration(None, isFinal = varOrVal == "val", id, ty, expr)
  }

  def mkNamespace(ids: Seq[String], nodes: Seq[EirNode]): EirNamespace = {
    ids match {
      case head :: Nil => EirNamespace(None, nodes.toList, head)
      case head :: tail =>
        EirNamespace(None, List(mkNamespace(tail, nodes)), head)
    }
  }

  def Namespace[_: P]: P[EirNamespace] = P(
    namespace ~ Id.rep(sep = "::", min = 1) ~ (`;`(Nil) | (`{` ~/ ProgramMember
      .rep(0) ~ `}`))
  ).map { case (ids, nodes) => mkNamespace(ids, nodes) }

  def Package[_: P]: P[Seq[String]] =
    P(WL ~ Keywords.`package` ~/ Id.rep(sep = "::", min = 1) ~ Semi)

  def Program[_: P]: P[(Option[Seq[String]], Seq[EirNode])] =
    Package.? ~ ProgramMember.rep(0) ~ End

  def TestRule[A](rule: P[_] => P[A])(implicit ctx: P[Any]): P[A] = {
    P(rule(ctx) ~ End)
  }

  def addAnnotations[A <: EirNode](
      node: A,
      as: Option[Seq[EirAnnotation]]
  ): A = {
    as.foreach(node.annotations ++= _)
    node
  }

  def ProgramMember[_: P]: P[EirNode] = P(
    Annotations.? ~ (Namespace | PublicImport | NamespaceMember)
  ).map { case (as, node) => addAnnotations(node, as) }

  def PublicImport[_: P]: P[EirImport] = {
    P(?:(`public`) ~ ImportStatement)
  }.map { imp =>
    {
      imp.publicOverride = true
      imp
    }
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

  def Variance[_: P]: P[EirVariance] = P("+".! | "-".!).map {
    case "+" => EirCovariant
    case "-" => EirContravariant
    case _   => Errors.unreachable()
  }

  def TDeclarationArg[_: P]: P[EirTemplateArgument] = P(
    Variance.? ~ Id ~ `...`.!.? ~ Bounds.? ~ (":" ~/ Type).? ~ ("=" ~/ ConstExpr(
      guarded = true
    )).?
  ).map { case (variance, id, ellipses, bounds, declTy, defaultVal) =>
    EirTemplateArgument(
      id,
      ellipses.nonEmpty,
      bounds,
      declTy,
      defaultVal,
      variance
    )
  }

  def Bounds[_: P]
      : P[(Option[EirResolvable[EirType]], Option[EirResolvable[EirType]])] =
    ((`>:` ~ Type) ~ (?:(`<:`) ~ Type).? |
      (`<:` ~ Type) ~ (?:(`>:`) ~ Type).?).map {
      case (op, lval, rval) if op == LowerBound => (Some(lval), rval)
      case (op, lval, rval) if op == UpperBound => (rval, Some(lval))
      case _                                    => ???
    }

  def Identifier[A <: EirNamedNode: ClassTag](implicit
      ctx: P[Any]
  ): P[EirSymbol[A]] = (Index ~ Id.rep(sep = "::", min = 1)).map {
    case (idx, ids) => SetLocation(EirSymbol[A](None, ids.toList), idx)
  }

  def Id[_: P]: P[String] = P(WL ~ Basics.Id)

  def Constant[_: P]: P[EirLiteral[_]] =
    P(NumericLiteral | BooleanLiteral | StringLiteral)

  def NodeGroupKwd[_: P]: P[String] = P("node".!.? ~ "group".!).map {
    case (lhs, rhs) => lhs.map(_ + rhs).getOrElse(rhs)
  }

  def ArrayKwd[_: P]: P[String] =
    P("array".! ~ CharIn("1-9").! ~ "d".!).map { case (i, j, k) => i + j + k }

  def CollectiveKwd[_: P]: P[String] = P(ArrayKwd | NodeGroupKwd)

  def ProxyType[_: P]: P[EirResolvable[EirType]] =
    P(Qualified[EirNamedType] ~ (ProxySuffix ~/ CollectiveKwd.?).?).map {
      case (ty, None) => ty
      case (ty, Some((proxy, collective))) =>
        EirProxyType(None, ty, collective, kindFrom(Some(proxy), collective))
    }

  def TupleType[_: P]: P[EirResolvable[EirType]] =
    P(`(` ~ Type.rep(min = 1, sep = ",") ~ `)`).map {
      case ty :: Nil => ty
      case tys       => EirTupleType(None, tys.toList)
    }

  def TupleMultiply[_: P]: P[EirResolvable[EirType]] =
    P(TupleType ~ (".*" ~/ ConstExpr(guarded = true)).?).map {
      case (lhs, None)      => lhs
      case (lhs, Some(rhs)) => EirTupleMultiply(lhs, rhs)(None)
    }

  def BasicType[_: P]: P[EirResolvable[EirType]] = P(ProxyType | TupleMultiply)

  def LambdaType[_: P]: P[EirResolvable[EirType]] =
    P(BasicType ~ ("=>" ~/ BasicType).?).map {
      case (lhs, Some(rhs)) =>
        EirLambdaType(None, extractTypes(lhs), rhs, Nil, None)
      case (lhs, None) => lhs
    }

  def Type[_: P]: P[EirResolvable[EirType]] =
    P(LambdaType ~ ("&".! | "...".!).?).map {
      case (ty, Some("...")) => EirPackExpansion(ty)(None)
      case (ty, Some("&"))   => EirReferenceType(None, ty)
      case (ty, None)        => ty
      case (ty, Some(_))     => ???
    }

  def ConstExpr[_: P](guarded: Boolean): P[EirExpressionNode] = {
    implicit val static: Boolean = true

    if (guarded) {
      P(PrimaryExpr)
    } else {
      P(Expression)
    }
  }

  def ProxySuffix[_: P]: P[String] = P(`@` | `[@]` | `{@}`)

  def ProxySelfExpr[_: P]: P[EirSymbol[EirNamedNode]] =
    P(`self` ~ ProxySuffix).map { case (self, suffix) =>
      EirSymbol[EirNamedNode](None, List(self + suffix))
    }

  def SelfExpr[_: P]: P[EirSymbol[EirNamedNode]] =
    ProxySelfExpr | P(`self`).map { self =>
      EirSymbol[EirNamedNode](None, List(self))
    }

  def Symbolize[A <: EirNode](res: EirResolvable[A]): EirExpressionNode = {
    res match {
      case x: EirExpressionNode => x
      // TODO systematically evaluate whether this is the best option?
      case x => EirLiteralSymbol(res)(None)
    }
  }

  def ConstSymbol[_: P]: P[EirExpressionNode] = P(Type).map(Symbolize)

  def Closure[_: P]: P[EirExpressionNode] = {
    P(Block).map(EirClosure(None, _))
  }

  def PrimaryExpr[_: P](implicit static: Boolean): P[EirExpressionNode] = {
    if (static) {
      P(Constant | ConstSymbol | TupleExpr)
    } else {
      P(
        Constant | SelfExpr | Qualified[
          EirNamedNode
        ].map(
          _.asInstanceOf[EirSymbolLike[EirNamedNode]]
        ) | Closure | TupleExpr | LambdaExpr | InterpolatedString
      )
    }
  }

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
    Specialization.? ~ `(` ~ FnCallArg.rep(min = 0, sep = ",") ~ `)`
  ).map { case (sp, args) =>
    CallSuffixTuple(sp.map(_.toList).getOrElse(Nil), args.toList)
  }

  def AccessSuffix[_: P]: P[ExprSuffixTuple] =
    P("." ~/ Id).map(AccessSuffixTuple)

  def Slice[_: P](implicit static: Boolean): P[Option[EirExpressionNode]] = P(
    Expression.? ~ (":" ~/ (Expression ~ ":").? ~ Expression.?).?
  ).map {
    case (expr, None)               => expr
    case (start, Some((step, end))) => Some(EirSlice(start, step, end)(None))
  }

  def AtSuffix[_: P](implicit static: Boolean): P[ExprSuffixTuple] = P(
    "[" ~/ Slice.rep(min = 0, sep = ",") ~ "]"
  ).map(_.flatten).map(AtSuffixTuple)

  def ExprSuffix[_: P](implicit static: Boolean): P[ExprSuffixTuple] = {
    if (static) P(AtSuffix) else P(CallSuffix | AccessSuffix | AtSuffix)
  }

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

  def PostfixExpr[_: P](implicit static: Boolean): P[EirExpressionNode] = {
    if (static) P(Index ~ PrimaryExpr ~ ExprSuffix.rep(0))
    else P(Index ~ (ProxyAccessor | PrimaryExpr) ~ ExprSuffix.rep(0))
  }.map { case (idx, expr, suffixes) =>
    SetLocation(applySuffixes(expr, suffixes), idx)
  }

  def UnaryExpr[_: P](implicit static: Boolean): P[EirExpressionNode] =
    P(PrefixOp.? ~ PostfixExpr).map {
      case (None, expr)     => expr
      case (Some(op), expr) => EirUnaryExpression(None, op, expr)
    }

  def BasicExpr[_: P](implicit static: Boolean): P[EirExpressionNode] = {
    if (static) P(UnaryExpr) else P(UnaryExpr | NewExpr | AwaitExpr)
  }

  def ConditionalExpr[_: P](implicit static: Boolean): P[EirExpressionNode] =
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

  def BoundOp[_: P]: P[String] = P(`<:` | `>:`)

  def InfixOp[_: P](implicit static: Boolean): P[String] = {
    if (static) {
      P(BoundOp | Id)
    } else {
      P(Id)
    }
  }

  def InfixExpr[_: P](
      min: Int
  )(implicit static: Boolean): P[EirExpressionNode] = P(
    ConditionalExpr ~ (InfixOp ~ ConditionalExpr).rep(min)
  ).map { case (expr, pairs) => buildInfix(expr, pairs) }

  def SetLocation[A <: EirNode](node: A, idx: Int)(implicit ctx: P[Any]): A = {
    node.location = PrettyIndex(idx)
    node
  }

  def NewExpr[_: P](implicit static: Boolean): P[EirNew] =
    P(Index ~ `new` ~/ Type ~ ExprList.?).map { case (idx, ty, tuple) =>
      SetLocation(EirNew(None, ty, tuple.getOrElse(Nil)), idx)
    }

  def AwaitExpr[_: P](implicit static: Boolean): P[EirAwait] =
    P(await ~/ PostfixExpr).map(EirAwait(None, _))

  def ExprList[_: P](implicit static: Boolean): P[List[EirExpressionNode]] = P(
    `(` ~ Expression.rep(min = 0, sep = ",") ~ `)`
  ).map(_.toList)

  def TupleExpr[_: P](implicit static: Boolean): P[EirExpressionNode] =
    P(ExprList).map(
      EirTupleExpression.fromExpressions(None, _, enclose = false)
    )

  def PrettyIndex(idx: Int)(implicit ctx: P[Any]): Option[EirSourceInfo] = {
    val pos = ctx.input.prettyIndex(idx).split(":").map(_.toInt)
    val (line, col) = (pos(0), pos(1))
    Modules.CurrentFile.map(f =>
      new EirSourceInfo(f.getCanonicalPath, line, col)
    )
  }

  def LambdaExpr[_: P]: P[EirLambdaExpression] = P(
    Index ~ `(` ~ FnArg.rep(
      min = 0,
      sep = ","
    ) ~ `)` ~ "=>" ~/ (Block | Expression)
  ).map { case (idx, args, body) =>
    SetLocation(
      EirLambdaExpression(
        None,
        args.toList,
        forceEnclosed(body, addReturn = true)
      ),
      idx
    )
  }

  def MatchExpr[_: P]: P[EirMatch] = P(
    `match` ~/ `(` ~ Expression ~ `)` ~ `{` ~/ CaseStatement.rep(1) ~ `}`
  ).map { case (expr, cases) => EirMatch(None, expr, cases.toList) }

  def CaseStatement[_: P]: P[EirMatchCase] = P(
    `case` ~/ PatternList ~ (`if` ~ Expression).? ~ "=>" ~/ OptionalStatement
  ).map { case (list, expr, body) => EirMatchCase(None, list, expr, body) }

  def PatternList[_: P]: P[EirPatternList] =
    P(Pattern.rep(min = 1, sep = ",")).map(s => EirPatternList(None, s.toList))

  def Pattern[_: P]: P[EirPattern] =
    P(ExtractorPattern | ConstantPattern | ExprPattern | IdPattern)

  def IdPattern[_: P]: P[EirIdentifierPattern] =
    P(Id ~ (":" ~/ BasicType).?).map { case (id, ty) =>
      EirIdentifierPattern(None, id, ty.getOrElse(EirPlaceholder(None)))
    }

  def ExtractorPattern[_: P]: P[EirExtractorPattern] = P(
    Identifier[EirNamedNode] ~ `(` ~/ PatternList.? ~ `)`
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

  def ExprPattern[_: P]: P[EirExpressionPattern] = {
    implicit val static: Boolean = false
    P(InfixExpr(1)).map(EirExpressionPattern(None, _))
  }

  def BasicArg[_: P]: P[(String, Option[String], EirResolvable[EirType])] =
    P(Id ~ ":" ~ "*".!.? ~/ Type)

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
    P(?:(`implicit`) ~/ BasicArg).map { case (id, exp, ty) =>
      val arg = EirFunctionArgument(None, id, ty, isExpansion = exp.nonEmpty)
      arg.isImplicit = true
      arg
    }

  def FnId[_: P]: P[String] = P(`self` | `[]` | Id)

  def FnDeclaration[_: P]: P[EirFunction] = P(
    `def` ~/ FnId ~ TDeclaration.? ~ `(` ~/ FnArg.rep(
      min = 0,
      sep = ","
    ) ~ `)` ~ (`(` ~/ ImplicitArg.rep(
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
      retTy.getOrElse({ globals.unitSymbol(None) }),
      where
    )
  }

  def WhereClause[_: P]: P[EirExpressionNode] =
    P(where ~/ ConstExpr(guarded = false))

  def AccessModifier[_: P]: P[EirAccessibility.Value] = P(
    `public` | `private` | `protected`
  ).map(s => s.head.toUpper +: s.tail).map(EirAccessibility.withName)

  type DTriple = (Boolean, String, Option[EirResolvable[EirType]])

  def Decltype[_: P]: P[DTriple] = P("&".!.? ~ Id ~ (":" ~ Type).?).map {
    case (amp, id, ty) => (amp.nonEmpty, id, ty)
  }

  def Decltypes[_: P]: P[List[DTriple]] = P(
    `(` ~/ Decltype.rep(min = 1, sep = ",").map(_.toList) ~ `)`
  ) | Decltype.map(List(_))

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
