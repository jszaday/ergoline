package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.Driver.{parserFromString, visitProgram}
import edu.illinois.cs.ergoline.ast.EirGlobalNamespace
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

class EirConstructorTests extends FunSuite {
  test("expected ok, multiple constructors and assignment") {
    EirGlobalNamespace.clear()
    visitProgram(parserFromString(
      """package foo;
        |class bar {
        |  func bar(self : bar, baz= : unit) : unit { }
        |  func bar(self : bar) : unit { baz = (); }
        |  val baz : unit;
        |}
        |""".stripMargin
    ))
    val numChecked : Int = CheckConstructors.checkAllConstructors()
    numChecked shouldEqual 2
  }

  test("expected failure, invalid self-assignment") {
    EirGlobalNamespace.clear()
    visitProgram(parserFromString(
      """package foo;
        |class bar {
        |  val baz : unit = ();
        |  func bar(self : bar, baz= : unit) : unit { }
        |}
        |""".stripMargin
    ))
    assertThrows[java.lang.AssertionError](CheckConstructors.checkAllConstructors())
  }

  test("expected failure, uninitialized field") {
    EirGlobalNamespace.clear()
    visitProgram(parserFromString(
      """package foo;
        |class bar {
        |  val baz : unit;
        |}
        |""".stripMargin
    ))
    assertThrows[java.lang.AssertionError](CheckConstructors.checkAllConstructors())
  }
}
