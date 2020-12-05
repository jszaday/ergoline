package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.EirGlobalNamespace
import edu.illinois.cs.ergoline.passes.{CheckTypes, FullyResolve, GenerateCpp, Processes}
import edu.illinois.cs.ergoline.resolution.Modules
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

class EirImportTests extends FunSuite {
  globals.strict = true

  test("built-in types are resolvable") {
    EirGlobalNamespace.clear()
    val module = Modules.load("""
      |package foo;
      |import ergoline::_;
      |def bar(): unit {
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
      |  def baz(): (int => int) {
      |    val f: (int => int) = (x : int) => x * 2;
      |    return f;
      |  }
      |  def qux(): int {
      |    return baz()(4);
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
      |def hello(): unit {
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
    // TODO restore this to string::tryParse<...> and auto-generate lambda
    val module = Modules.load("""
      |package foo;
      |import ergoline::_;
      |@main def hello(args : array<string>): unit {
      |    val n : int =
      |        args.getOrNone(1) // -> option[string]
      |            .flatMap<int>((s: string) => s.tryParse<int>()) // -> option[int]
      |            .getOrElse(16); // -> int
      |    for (var i : int = 0; i < n; i = i + 1) {
      |        if (i % 2 == 0) {
      |            println("hello #" + i.toString());
      |        }
      |    }
      |}
      |""".stripMargin)
    Processes.onLoad(module)
  }
}
