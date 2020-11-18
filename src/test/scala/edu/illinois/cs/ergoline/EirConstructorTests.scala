package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.EirGlobalNamespace
import edu.illinois.cs.ergoline.passes.CheckEnclose
import edu.illinois.cs.ergoline.resolution.Modules
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

class EirConstructorTests extends FunSuite {

  private val bigProgram = {
    EirGlobalNamespace.clear()
    Modules.load(
      """package foo;
        |class bar {
        |  func bar(self : bar, baz= : unit) : unit { }
        |  func bar(self : bar) : unit { baz = (); }
        |  val baz : unit;
        |}
        |""".stripMargin
    )
  }

  test("expected ok, multiple constructors and assignment") {
    CheckConstructors.checkConstructorsWithin(bigProgram) shouldEqual 2
  }

  test("expected ok, correct enclosure") {
    CheckEnclose.visit(bigProgram) shouldEqual None
  }

  test("expected failure, invalid self-assignment") {
    EirGlobalNamespace.clear()
    val module = Modules.load(
      """package foo;
        |class bar {
        |  val baz : unit = ();
        |  func bar(self : bar, baz= : unit) : unit { }
        |}
        |""".stripMargin
    )
    assertThrows[java.lang.AssertionError](
      CheckConstructors.checkConstructorsWithin(module))
  }

  test("expected failure, uninitialized field") {
    EirGlobalNamespace.clear()
    val module = Modules.load(
      """package foo;
        |class bar {
        |  val baz : unit;
        |}
        |""".stripMargin
    )
    assertThrows[java.lang.AssertionError](
      CheckConstructors.checkConstructorsWithin(module))
  }
}
