package edu.illinois.cs.ergoline.parsing

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.literals.{
  EirIntegerLiteral,
  EirLiteral,
  EirLiteralSymbol
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
      case _ => ???
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
    P(`import` ~ Id.rep(sep = "::", min = 1) ~ Semi).map { ids =>
      EirImport(None, ids.toList, publicOverride = false)
    }

  def UsingStatement[_: P]: P[EirTypeAlias] = P(
    `using` ~ Id ~ TDeclaration.? ~ "=" ~ ConstExpr ~ Semi
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

  def Expression[_: P]: P[EirExpressionNode] = PrimaryExpr

  def WhileLoop[_: P]: P[EirWhileLoop] = P(
    `while` ~ `(` ~ Expression ~ `)` ~ OptionalStatement
  ).map { case (expr, body) => EirWhileLoop(None, expr, body) }

  def DoWhileLoop[_: P]: P[EirDoWhileLoop] = P(
    `do` ~ OptionalStatement ~ `while` ~ `(` ~ Expression ~ `)`
  ).map { case (body, expr) => EirDoWhileLoop(None, expr, body) }

  def ForAllHeader[_: P]: P[EirForLoopHeader] = P(
    Id.rep(min = 1, sep = ",") ~ `<-` ~ Expression
  ).map { case (symbols, expr) =>
    EirForAllHeader(None, mkDeclaration(symbols), expr)
  }

  def Statement[_: P]: P[EirNode] = P(
    ForLoop | DoWhileLoop | WhileLoop | Block | InnerDeclaration | (Expression ~ Semi)
  )

  def Block[_: P]: P[EirBlock] =
    P(`{` ~ Statement.rep(0) ~ `}`).map(_.toList).map(EirBlock(None, _))

  def OptionalStatement[_: P]: P[Option[EirNode]] = P(!:(Statement) | `;`(None))

  def OptionalBlock[_: P]: P[Option[EirBlock]] = P(!:(Block) | `;`(None))

  def ForLoop[_: P]: P[EirForLoop] = P(
    `for` ~ `(` ~ ForAllHeader ~ `)` ~ OptionalStatement
  ).map { case (hdr, body) => EirForLoop(None, hdr, body) }

  def Specialization[p: P]: P[Seq[EirSymbolLike[EirType]]] =
    P("<" ~ Specialized[p, EirNamedType].rep(min = 0, sep = ",") ~ ">")

  def Class[_: P]: P[EirClass] = P(ClassKind ~ Id ~ TDeclaration.? ~ ClassBody)
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
    (`var` | `val`) ~ Id ~ ":" ~ Type ~ ("=" ~ Expression).? ~ Semi
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

  def addAnnotations[A <: EirNode](node: A, as: Option[Seq[EirAnnotation]]): A = {
    as.foreach(node.annotations ++= _)
    node
  }

  def ProgramMember[_: P]: P[EirNode] =
    P(Annotations.? ~ (Namespace | NamespaceMember)).map {
      case (as, node) => addAnnotations(node, as)
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

  def PrimaryExpr[p: P]: P[EirExpressionNode] =
    P(Qualified[p, EirNamedNode] | Constant | TupleExpr | LambdaExpr)

  def TupleExpr[_: P]: P[EirExpressionNode] = P(
    `(` ~ Expression.rep(min = 1, sep = ",") ~ `)`
  ).map(s => EirTupleExpression.fromExpressions(None, s.toList))

  def LambdaExpr[_: P]: P[EirLambdaExpression] = P(
    `(` ~ FnArg.rep(min = 0, sep = ",") ~ `)` ~ "=>" ~ (Block | Expression)
  ).map { case (args, body) =>
    EirLambdaExpression(
      None,
      args.toList,
      forceEnclosed(body, addReturn = true)
    )
  }

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
    `def` ~ Id ~ TDeclaration.? ~ `(` ~ FnArg.rep(
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
    ?:(`val`) ~ Decltypes ~ "=" ~ !:(Expression) ~ Semi
  ).map { case (triples, expr) => mkDeclaration(triples, expr, isFinal = true) }

  def VarDeclaration[_: P]: P[EirNode] =
    P(?:(`var`) ~ Decltypes ~ ("=" ~ Expression).? ~ Semi).map {
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
