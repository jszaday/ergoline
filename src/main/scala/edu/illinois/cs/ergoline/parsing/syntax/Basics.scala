package edu.illinois.cs.ergoline.parsing.syntax

import fastparse.CharPredicates._
import fastparse.NoWhitespace._
import fastparse._, ScalaWhitespace._
import Keywords._

object Basics {
  def UnicodeEscape[_: P] = P("u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit)

  //Numbers and digits
  def Digit[_: P] = P(CharIn("0-9"))

  def HexDigit[_: P] = P(CharIn("0-9a-fA-F"))
  def HexNum[_: P] = P("0x" ~ CharsWhileIn("0-9a-fA-F"))
  def DecNum[_: P] = P(CharsWhileIn("0-9"))
  def Exp[_: P] = P(CharIn("Ee") ~ CharIn("+\\-").? ~ DecNum)
  def FloatType[_: P] = P(CharIn("fFdD"))

  def WSChars[_: P] = P(NoTrace(CharsWhileIn("\u0020\u0009")))
  def Newline[_: P] = P(NoTrace(StringIn("\r\n", "\n")))
  def Semi[_: P] = P((";" | Newline) ~ Newline.rep(0))
  def OpChar[_: P] = P(CharPred(isOpChar))

  val isOpChar = NamedFunction {
    case '!' | '#' | '%' | '&' | '*' | '+' | '-' | '/' | ':' | '<' | '=' | '>' |
        '?' | '@' | '\\' | '^' | '|' | '~' => true
    case c => isOtherSymbol(c) || isMathSymbol(c)
  }

  val LetterDigitDollarUnderscore =
    NamedFunction(c => isLetter(c) | isDigit(c) | c == '$' | c == '_')
  val LowerChar = NamedFunction(c => isLower(c) || c == '$' | c == '_')
  val UpperChar = NamedFunction(isUpper)

  def Lower[_: P] = P(CharPred(LowerChar))
  def Upper[_: P] = P(CharPred(UpperChar))

  def WL0[_: P]: P[Unit] = P(ScalaWhitespace.whitespace(P.current))
  def WL[_: P]: P[Unit] = P(NoCut(WL0))

  def Id[_: P]: P[String] = P(("_" | Lower | Upper).rep(1).!)

  /** Most keywords don't just require the correct characters to match,
    * they have to ensure that subsequent characters *don't* match in
    * order for it to be a keyword. This enforces that rule for key-words
    * (W) and key-operators (O) which have different non-match criteria.
    */
  object Key {
    def W[_: P](s: String): P[String] = P(
      s.! ~ !CharPred(Basics.LetterDigitDollarUnderscore)
    )(s"`$s`", implicitly)
    // If the operator is followed by a comment, stop early so we can parse the comment
    def O[_: P](s: String): P[String] = P(
      s.! ~ (!Basics.OpChar | &(NoTrace(StringIn("/*", "//"))))
    )(s"`$s`", implicitly)
  }
}
