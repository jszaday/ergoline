package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.EirGlobalNamespace
import edu.illinois.cs.ergoline.passes.{CheckTypes, FullyResolve, GenerateCpp, Processes}
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
      |  println(z.toString());
      |}
      |""".stripMargin)
    Processes.onLoad(module)
    FullyResolve.verify(module) shouldBe true
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
    Processes.onLoad(module)
//    println(GenerateCpp.visit(module))
  }

  test("can resolve template types") {
    EirGlobalNamespace.clear()
    val module = Modules.load("""
      |package foo;
      |import ergoline::_;
      |func hello(): unit {
      |  val s: string = "42";
      |  val x: option<int> = option<int>(42);
      |  val y: option<int> = s.tryParse<int>();
      |  assert(x.nonEmpty());
      |  assert(y.nonEmpty());
      |  assert(x.get() == y.get());
      |  println("hello " + x.get().toString());
      |}
      |""".stripMargin)
    Processes.onLoad(module)
  }

  test("check sophisticated and chained templates") {
    EirGlobalNamespace.clear()
    val module = Modules.load("""
      |package foo;
      |import ergoline::_;
      |@main func hello(args : array<string>): unit {
      |    val arg : int =
      |        args.getOrNone(1) // -> option[string]
      |            .flatMap<int>(string::tryParse<int>) // -> option[int]
      |            .getOrElse(16); // -> int
      |    println("hello with: " + arg.toString());
      |}
      |""".stripMargin)
    Processes.onLoad(module)
  }
}
