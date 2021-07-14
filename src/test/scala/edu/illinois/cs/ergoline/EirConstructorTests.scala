package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.{EirClassLike, EirNode, EirTrait}
import edu.illinois.cs.ergoline.passes.{CheckConstructors, CheckEnclose, CheckTypes, TypeCheckContext}
import edu.illinois.cs.ergoline.resolution.{Find, Modules}
import edu.illinois.cs.ergoline.util.Errors.EirException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class EirConstructorTests extends AnyFunSuite {

  private implicit val ctx: TypeCheckContext = new TypeCheckContext
  private val bigProgram = {
    EirImportTests.setupEnv()
    Modules.load(
      """package foo;
        |class bar {
        |  def bar(=baz : bar) {}
        |  def bar() { baz = self; }
        |  val baz : bar;
        |}
        |""".stripMargin
    )
  }


  private def checkConstructors(node: EirNode)(implicit ctx: TypeCheckContext): Int = {
    Find.child[EirClassLike](node, _ => true)
      .map(CheckConstructors.checkConstructors)
      .map(Math.max(_, 1))
      .sum
  }

  test("expected ok, multiple constructors and assignment") {
    checkConstructors(bigProgram) shouldEqual 2
  }

  test("expected ok, correct enclosure") {
    CheckEnclose(bigProgram) shouldEqual None
  }

  test("expected failure, invalid self-assignment") {
    EirImportTests.setupEnv()
    val module = Modules.load(
      """package foo;
        |class bar {
        |  val baz : bar = ();
        |  def bar(=baz : bar) : unit { }
        |}
        |""".stripMargin
    )
    assertThrows[EirException](checkConstructors(module))
  }

  test("expected failure, uninitialized field") {
    EirImportTests.setupEnv()
    val module = Modules.load(
      """package foo;
        |class bar {
        |  val baz : unit;
        |}
        |""".stripMargin
    )
    assertThrows[EirException](checkConstructors(module))
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
    assertThrows[EirException](CheckTypes.visit(module))
  }

  test("expect specialization in inheritance") {
    EirImportTests.setupEnv()
    val module = Modules.load(
      """package foo;
        |class a extends b { }
        |class b<A> { }
        |""".stripMargin
    )
    assertThrows[EirException](CheckTypes.visit(module))
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
    CheckTypes.visit(module)
    val c = module.children.collectFirst({
      case c: EirTrait if c.name == "c" => c
    })
    c.map(_.derived.toList.length) shouldEqual Some(3)
  }

  private def enablerCheckModule(pass: Boolean): EirNode = {
    val clause = s"${if (pass) "" else "!"}(A <: a)"
    EirImportTests.setupEnv()
    Modules.load(
      s"""package foo;
         |trait a { }
         |trait b extends a { }
         |class c<A> where $clause { }
         |class d {
         |  var e: c<b>;
         |}
         |""".stripMargin
    )
  }

  test("enabler check (should fail)") {
    val module = enablerCheckModule(false)
    assertThrows[EirException](CheckTypes.visit(module))
  }

  test("enabler check (should pass)") {
    CheckTypes.visit(enablerCheckModule(true))
  }
}
