package edu.illinois.cs.ergoline.parsing.syntax

import fastparse._
import Basics._

object Keywords {
  def Alphabetic: Seq[String] = {
    this.getClass.getDeclaredMethods
      .map(_.getName)
      .filter(_.forall(_.isLower))
      .toSet
      .toSeq
  }

  case class NamedFunction(f: Char => Boolean)(implicit name: sourcecode.Name)
      extends (Char => Boolean) {
    def apply(t: Char): Boolean = f(t)
    override def toString(): String = name.value
  }

  def `implicit`[_: P]: P[String] = Key.W("implicit")
  def `def`[_: P]: P[Unit] = ?:(Key.W("def"))

  def `do`[_: P]: P[Unit] = ?:(Key.W("do"))
  def `for`[_: P]: P[Unit] = ?:(Key.W("for"))
  def `while`[_: P]: P[Unit] = ?:(Key.W("while"))
  def `<-`[_: P]: P[Unit] = P("<-" | "\u2190")
  def `=>`[_: P]: P[Unit] = P("=>" | "\u21d2")
  def `class`[_: P]: P[String] = Key.W("class")
  def `object`[_: P]: P[String] = Key.W("object")
  def `struct`[_: P]: P[String] = Key.W("struct")

  def `using`[_: P]: P[Unit] = ?:(Key.W("using"))
  def `import`[_: P]: P[Unit] = ?:(Key.W("import"))

  def `if`[_: P]: P[Unit] = ?:(Key.W("if"))
  def `else`[_: P]: P[Unit] = ?:(Key.W("else"))

  def `return`[_: P]: P[Unit] = ?:(Key.W("return"))

  def `...` = "..."

  val LowerBound = ">:"
  val UpperBound = "<:"

  def `>:`[_: P]: P[String] = Key.O(LowerBound)
  def `<:`[_: P]: P[String] = Key.O(UpperBound)

  def `@`[_: P]: P[String] = Key.O("@")
  def `[@]`[_: P]: P[String] = Key.O("[@]")
  def `{@}`[_: P]: P[String] = Key.O("{@}")

  def Plus[_: P]: P[String] = Key.O("+")
  def Minus[_: P]: P[String] = Key.O("-")
  def BitNot[_: P]: P[String] = Key.O("~")
  def Not[_: P]: P[String] = Key.O("!")
  def `?`[_: P]: P[Unit] = ?:(Key.O("?"))

  def PrefixOp[_: P]: P[String] = P(Plus | Minus | Not | BitNot)

  def ?:[_: P, A](rule: P[A]): P[Unit] = P(rule).map(_ => ())
  def !:[_: P, A](rule: P[A]): P[Option[A]] = P(rule).map(Some(_))

  def `{`[_: P]: P[Unit] = P("{")
  def `}`[_: P]: P[Unit] = P("}")

  def `(`[_: P]: P[Unit] = P("(")
  def `)`[_: P]: P[Unit] = P(")")

  def `;`[_: P, A](a: A): P[A] = Semi.map(_ => a)

  def `when`[_: P]: P[Unit] = ?:(Key.W("when"))
  def `new`[_: P]: P[Unit] = ?:(Key.W("new"))
  def `match`[_: P]: P[Unit] = ?:(Key.W("match"))
  def `case`[_: P]: P[Unit] = ?:(Key.W("case"))
  def `var`[_: P]: P[String] = Key.W("var")
  def `val`[_: P]: P[String] = Key.W("val")
  def `public`[_: P]: P[String] = Key.W("public")
  def `private`[_: P]: P[String] = Key.W("private")
  def `protected`[_: P]: P[String] = Key.W("protected")
  def `static`[_: P]: P[String] = Key.W("static")
  def `override`[_: P]: P[String] = Key.W("override")
  def `package`[_: P]: P[Unit] = ?:(Key.W("package"))
  def `true`[_: P]: P[String] = Key.W("true")
  def `false`[_: P]: P[String] = Key.W("false")
  def `extends`[_: P]: P[Unit] = ?:(Key.W("extends"))
  def `with`[_: P]: P[Unit] = ?:(Key.W("with"))

  def self[_: P]: P[String] = Key.W("self")
  def await[_: P]: P[Unit] = ?:(Key.W("await"))
  def where[_: P]: P[Unit] = ?:(Key.W("where"))
  def namespace[_: P]: P[Unit] = ?:(Key.W("namespace"))
}
