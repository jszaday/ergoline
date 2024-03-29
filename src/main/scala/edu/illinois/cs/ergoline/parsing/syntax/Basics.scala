package edu.illinois.cs.ergoline.parsing.syntax

import fastparse.CharPredicates._
import fastparse.NoWhitespace._
import fastparse._
import Keywords._

object Basics {
  def UnicodeEscape[_: P] = P("u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit)

  //Numbers and digits
  def Digit[_: P]: P[String] = P(CharIn("0-9").!)

  def HexDigit[_: P] = P(CharIn("0-9a-fA-F"))
  def HexNum[_: P] = P("0x" ~ CharsWhileIn("0-9a-fA-F"))
  def DecNum[_: P] = P(CharsWhileIn("0-9"))
  def Exp[_: P] = P(CharIn("Ee") ~ CharIn("+\\-").? ~ DecNum)
  def FloatType[_: P] = P(CharIn("fFdD"))

  def WSChars[_: P] = P(NoTrace(CharsWhileIn("\u0020\u0009")))
  def Newline[_: P] = P(NoTrace(StringIn("\r\n", "\n")))
  def Semi[_: P] = P(";")
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

  def SymbolicKeywords[_: P]: P[Unit] = P {
    StringIn(
      ":",
      ";",
      "=>",
//      "=",
      "<-",
      "<:",
      "<%",
      ">:",
      "#",
      "@",
      "\u21d2",
      "\u2190"
    ) ~ !OpChar
  }.opaque("SymbolicKeywords")

  def AlphabeticKeywords[_: P]: P[Unit] = P(
    StringIn(
      "match",
      "case",
      "package",
      "await",
      "do",
      "self",
      "public",
      "object",
      "import",
      "implicit",
      "struct",
      "where",
      "namespace",
      "static",
      "for",
      "protected",
      "using",
      "private",
      "return",
      "if",
      "override",
      "else",
      "new",
      "while",
      "class",
      "def",
      "var",
      "val",
      "true",
      "false",
      "with",
      "extends",
      "abstract",
      "trait"
    ) ~ !CharPred(LetterDigitDollarUnderscore)
  ).opaque("Alphabetic")

  def AlphanumericHead[_: P]: P[String] = P(("_" | Lower | Upper).!)

  def NotOpChar[_: P]: P[String] = P(!CharPred(isOpChar)).map(_ => "")

  def AlphanumericTail[_: P](min: Int): P[String] = P(
    CharsWhile(c => c != '_' && LetterDigitDollarUnderscore(c), min).! ~
      ("_".! ~ (AlphanumericTail(1) | NotOpChar)).?
  ).map { case (lhs, rhs) => lhs + rhs.map(t => t._1 + t._2).getOrElse("") }

  private def Closer[_: P]: P[String] = P(
    "_".! ~ PrefixOp.?
  ).map {
    case (lhs, Some(rhs)) => lhs + rhs
    case (lhs, None)      => lhs
  }

  def AlphanumericId[_: P]: P[String] = P(
    !AlphabeticKeywords ~ (AlphanumericHead ~ AlphanumericTail(
      0
    )) ~ Closer.?
  ).map { case (head, tail, suffix) => ((head +: tail) ++ suffix).mkString("") }

  val OpCharNotSlash: NamedFunction =
    NamedFunction(x => isOpChar(x) && x != '/')

  def OperatorId[_: P]: P[String] = P(
    !SymbolicKeywords ~ (!StringIn("/*", "//") ~ (CharsWhile(
      OpCharNotSlash
    ).! | "/".!)).rep(1)
  ).map { case (seq) => seq.mkString("") }

  def Id[_: P]: P[String] = P(AlphanumericId | OperatorId)

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
