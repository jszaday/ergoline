package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.{EirFunction, EirGlobalNamespace}
import edu.illinois.cs.ergoline.passes.FullyResolve
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.resolution.Modules
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode
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
      |  val y : int = x * 2;
      |  val z : int = x * y;
      |  println(z.toString);
      |}
      |""".stripMargin)
    FullyResolve.visit(module)
    FullyResolve.verify(module) shouldBe true
  }
}
