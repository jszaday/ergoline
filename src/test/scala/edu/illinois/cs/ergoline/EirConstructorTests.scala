package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.EirGlobalNamespace
import edu.illinois.cs.ergoline.passes.{CheckConstructors, CheckEnclose}
import edu.illinois.cs.ergoline.resolution.Modules
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

class EirConstructorTests extends FunSuite {

  private val bigProgram = {
    EirGlobalNamespace.clear()
    Modules.load(
      """package foo;
        |class bar {
        |  def bar(=baz : bar) : unit { }
        |  def bar() : unit { baz = (); }
        |  val baz : bar;
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
        |  val baz : bar = ();
        |  def bar(=baz : bar) : unit { }
        |}
        |""".stripMargin
    )
    assertThrows[java.lang.RuntimeException](
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
    assertThrows[java.lang.RuntimeException](
      CheckConstructors.checkConstructorsWithin(module))
  }
}
