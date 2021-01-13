package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.{EirGlobalNamespace, EirTrait}
import edu.illinois.cs.ergoline.passes.{CheckConstructors, CheckEnclose, CheckTypes, TypeCheckContext}
import edu.illinois.cs.ergoline.resolution.Modules
import edu.illinois.cs.ergoline.util.Errors.EirException
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
    assertThrows[EirException](
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
    assertThrows[EirException](
      CheckConstructors.checkConstructorsWithin(module))
  }

  test("preclude circular relationships") {
    EirImportTests.setupEnv()
    val module = Modules.load(
      """package foo;
        |class a extends b { }
        |class b extends c { }
        |class c extends a { }
        |""".stripMargin
    )
    assertThrows[EirException](
      CheckTypes.visit(new TypeCheckContext, module))
  }

  test("expect specialization in inheritance") {
    EirImportTests.setupEnv()
    val module = Modules.load(
      """package foo;
        |class a extends b { }
        |class b<A> { }
        |""".stripMargin
    )
    assertThrows[EirException](
      CheckTypes.visit(new TypeCheckContext, module))
  }

  test("shared parent class is OK") {
    EirImportTests.setupEnv()
    val module = Modules.load(
      """package foo;
        |trait a extends c { }
        |trait b extends c { }
        |trait c { }
        |class d with a { }
        |""".stripMargin
    )
    CheckTypes.visit(new TypeCheckContext, module)
    val c = module.children.collectFirst({
      case c: EirTrait if c.name == "c" => c
    })
    c.map(_.derived.toList.length) shouldEqual Some(3)
  }
}
