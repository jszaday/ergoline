package edu.illinois.cs.ergoline.parsing.syntax

import fastparse._
import Basics._
import SingleLineWhitespace._

object Keywords {
  case class NamedFunction(f: Char => Boolean)(implicit name: sourcecode.Name)
      extends (Char => Boolean) {
    def apply(t: Char): Boolean = f(t)
    override def toString(): String = name.value
  }

  def NewlineAfter[_: P](s: String, min: Int = 0): P[Unit] =
    P(s ~ Newline.rep(min))

  def NewlineBefore[_: P](s: String, min: Int = 0): P[Unit] =
    P(Newline.rep(min) ~ s)

  def NewlineAgnostic[_: P](s: String, min: Int = 0): P[Unit] =
    P(Newline.rep(min) ~ s ~ Newline.rep(min))

  def `implicit`[_: P]: P[String] = Key.W("implicit")
  def `def`[_: P]: P[Unit] = ?:(Key.W("def"))

  def `do`[_: P]: P[Unit] = ?:(Key.W("do"))
  def `for`[_: P]: P[Unit] = ?:(Key.W("for"))
  def `while`[_: P]: P[Unit] = ?:(Key.W("while"))
  def `<-`[_: P]: P[Unit] = P("<-")
  def `class`[_: P]: P[String] = Key.W("class")
  def `object`[_: P]: P[String] = Key.W("object")
  def `struct`[_: P]: P[String] = Key.W("struct")

  def `using`[_: P]: P[Unit] = ?:(Key.W("using"))
  def `import`[_: P]: P[Unit] = ?:(Key.W("import"))

  def `...` = "..."

  val LowerBound = ">:"
  val UpperBound = "<:"

  def `>:`[_: P]: P[String] = Key.O(LowerBound)
  def `<:`[_: P]: P[String] = Key.O(UpperBound)

  def ?:[_: P, A](rule: P[A]): P[Unit] = P(rule).map(_ => ())
  def !:[_: P, A](rule: P[A]): P[Option[A]] = P(rule).map(Some(_))
  def `{`[_: P]: P[Unit] = P(NewlineAgnostic("{"))
  def `}`[_: P]: P[Unit] = P(NewlineAgnostic("}"))

  def `(`[_: P]: P[Unit] = P(NewlineAfter("("))
  def `)`[_: P]: P[Unit] = P(NewlineBefore(")"))

  def `;`[_: P, A](a: A): P[A] = Semi.map(_ => a)

  def `var`[_: P]: P[String] = Key.W("var")
  def `val`[_: P]: P[String] = Key.W("val")
  def `public`[_: P]: P[String] = Key.W("public")
  def `private`[_: P]: P[String] = Key.W("private")
  def `protected`[_: P]: P[String] = Key.W("protected")
  def `static`[_: P]: P[String] = Key.W("static")
  def `override`[_: P]: P[String] = Key.W("override")

  def where[_: P]: P[Unit] = ?:(Key.W("where"))
  def `package`[_: P]: P[Unit] = ?:(Key.W("package"))
  def namespace[_: P]: P[Unit] = ?:(Key.W("namespace"))
}
