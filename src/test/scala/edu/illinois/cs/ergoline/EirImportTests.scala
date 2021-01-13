package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.EirImportTests.setupEnv
import edu.illinois.cs.ergoline.ast.{EirClassLike, EirGlobalNamespace}
import edu.illinois.cs.ergoline.passes.Processes.RichProcessesSyntax.RichEirClassList
import edu.illinois.cs.ergoline.passes.{CheckTypes, FullyResolve, GenerateCpp, Processes}
import edu.illinois.cs.ergoline.resolution.{Find, Modules}
import edu.illinois.cs.ergoline.util.Errors
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

object EirImportTests{
  def setupEnv(): Unit = {
    globals.strict = true
    Errors.exitAction = () => throw new RuntimeException("")
    EirGlobalNamespace.clear()
    Processes.reset()
  }
}

class EirImportTests extends FunSuite {
  test("built-in types are resolvable") {
    setupEnv()
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

  test("tuple types work as expected") {
    setupEnv()
    val module = Modules.load("""
        |package foo;
        |import ergoline::_;
        |def bar(): unit {
        |  val t: (int, string, bool) = (42, "42", true);
        |
        |  val z: string = match (t) {
        |    case x, y, true => x.toString() + y;
        |    case _, y, false => y;
        |  };
        |}
        |""".stripMargin)
    Processes.onLoad(module)
    FullyResolve.verify(module) shouldBe true
  }

  test("can resolve lambdas type-check") {
    setupEnv()
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
    setupEnv()
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

  test("complex class relationships are correctly sorted and partitioned") {
    setupEnv()
    val module = Modules.load("""
        |package foobar;
        |
        |class foobar with foo::baz { }
        |
        |namespace foo {
        |  trait bar { }
        |  trait baz with qux::quux { }
        |}
        |
        |namespace qux {
        |  trait quux with foo::bar { }
        |}
        |""".stripMargin)
    Processes.onLoad(module)
    val unsorted = Find.within[EirClassLike](module, _ => true).toList
    unsorted.hasValidOrder shouldBe false
    val sorted = unsorted.dependenceSort()
    sorted.hasValidOrder shouldBe true
    sorted.map(_.name) shouldEqual List("bar", "quux", "baz", "foobar")
    val partitioned = sorted.namespacePartitioned
    partitioned.map(_._1.name) shouldEqual List("foo", "qux", "foo", "foobar")
  }

  test("check sophisticated and chained templates") {
    setupEnv()
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
