package edu.illinois.cs.ergoline.parsing.syntax

import edu.illinois.cs.ergoline.ast.{EirExpressionNode, EirInterpolatedString}
import edu.illinois.cs.ergoline.ast.literals.{
  EirBooleanLiteral,
  EirFloatLiteral,
  EirIntegerLiteral,
  EirLiteral,
  EirStringLiteral
}
import edu.illinois.cs.ergoline.parsing.Parser.Expression
import edu.illinois.cs.ergoline.parsing.syntax.Keywords.{`false`, `true`}
import fastparse._
import NoWhitespace._

object Literals {
  private def IStringExpr[_: P]: P[EirExpressionNode] =
    P("${" ~/ Expression ~ "}")

  private def IStringPred[_: P]: P[String] =
    CharsWhile(c => c != '\r' && c != '\n' && c != '`' && c != '$').!

  private def StringPred[_: P]: P[String] =
    CharsWhile(c => c != '\r' && c != '\n' && c != '"' && c != '\\').!

  private def IStringChars[_: P]: P[EirStringLiteral] =
    P("$$".! | IStringPred).map(EirStringLiteral(_)(None))

  private def IStringFragment[_: P]: P[EirExpressionNode] =
    P(IStringChars | IStringExpr)

  private def EscapePred(c: Char): Boolean = c != '"' && !c.isWhitespace

  // TODO refine this!
  private def EscapeSeq[_: P]: P[String] =
    P("\\".! ~/ ("\"".! | CharsWhile(EscapePred, 1).!)).map { case (lhs, rhs) =>
      lhs + rhs
    }

  private def StringFragment[_: P]: P[String] = P(StringPred | EscapeSeq)

  def InterpolatedString[_: P]: P[EirInterpolatedString] =
    P("`" ~/ IStringFragment.rep(0) ~ "`").map { fragments =>
      EirInterpolatedString(fragments.toList)(None)
    }

  def StringLiteral[_: P]: P[EirStringLiteral] = {
    P("\"" ~/ StringFragment.rep(0) ~ "\"")
  }.map { seq => EirStringLiteral("\"" + seq.mkString("") + "\"")(None) }

  def Numerals[_: P]: P[String] = P(CharIn("0-9").rep(1).!)

  def NumericLiteral[_: P]: P[EirLiteral[_]] =
    P(Numerals ~ ("." ~ Numerals).?).map {
      case (lhs, None)      => EirIntegerLiteral(lhs.toInt)(None)
      case (lhs, Some(rhs)) => EirFloatLiteral(s"$lhs.$rhs".toFloat)(None)
    }

  def BooleanLiteral[_: P]: P[EirBooleanLiteral] =
    P(`true` | `false`).map(s => EirBooleanLiteral(s == "true")(None))
}
