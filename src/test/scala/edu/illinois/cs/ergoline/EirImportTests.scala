package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.EirGlobalNamespace
import edu.illinois.cs.ergoline.passes.{FullyResolve, CheckTypes}
import edu.illinois.cs.ergoline.resolution.Modules
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

class EirImportTests extends FunSuite {
  test("built-in types are resolvable") {
    EirGlobalNamespace.clear()
    val module = Modules.load("""
      |package foo;
      |import ergoline::_;
      |func bar(): unit {
      |  val x : int = 42;
      |  val y : int = x + 2;
      |  val z : int = x * y;
      |  println(z.toString);
      |}
      |""".stripMargin)
    FullyResolve.visit(module)
    FullyResolve.verify(module) shouldBe true
    CheckTypes.visit(module)
  }

  test("can resolve lambdas type-check") {
    EirGlobalNamespace.clear()
    val module = Modules.load("""
      |package foo;
      |import ergoline::_;
      |class bar {
      |  def baz(self: bar): (int => int) {
      |    val f: (int => int) = (x : int) => x * 2;
      |    return f;
      |  }
      |  def qux(self: bar): int {
      |    return self.baz()(4);
      |  }
      |}
      |""".stripMargin)
    FullyResolve.visit(module)
    CheckTypes.visit(module)
  }
}