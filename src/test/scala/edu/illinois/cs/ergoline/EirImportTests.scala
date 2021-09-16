package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.EirImportTests.setupEnv
import edu.illinois.cs.ergoline.ast.types.EirTemplatedType
import edu.illinois.cs.ergoline.ast.{EirClass, EirClassLike, EirGlobalNamespace, EirNamedNode, EirTrait}
import edu.illinois.cs.ergoline.passes.Processes.RichProcessesSyntax.RichEirClassList
import edu.illinois.cs.ergoline.passes.{CheckTypes, FullyResolve, GenerateCpp, Processes, TypeCheckContext}
import edu.illinois.cs.ergoline.resolution.{Find, Modules}
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.Errors.EirException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.reflect.ClassTag

object EirImportTests{
  def setupEnv(): Unit = {
    globals.strict = true
    Errors.useDebugAction()
    EirGlobalNamespace.clear()
    Processes.reset()
  }
}

class EirImportTests extends AnyFunSuite {
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

  test("covariant relationships work") {
    setupEnv()
    val module = Modules.load("""
        |package tests;
        |import ergoline::_;
        |def test_either<L, R>(l: either<L, R>, r: either<L, R>) {
        |  assert(l.isLeft() && r.isRight());
        |}
        |
        |def test_covariant(): unit {
        |  val x = left(42);
        |  val y = right(false);
        |  test_either(x, y); // => will resolve to <int, bool>
        |}
        |""".stripMargin)
    Processes.onLoad(module)
  }

  test("contravariant relationships work") {
    setupEnv()
    val module = Modules.load("""
        |package tests;
        |import ergoline::_;
        |abstract class bird { }
        |class duck extends bird { }
        |class petter<-A> {
        |  def pet(a: A) { }
        |}
        |def pet_duck(p: petter<duck>, d: duck) { p.pet(d); }
        |def test_contravariant(): unit {
        |  val d = new duck;
        |  val p = new petter<bird>;
        |  pet_duck(p, d);
        |}
        |""".stripMargin)
    Processes.onLoad(module)
    Find.namedChild[EirClass](Some(module), "petter")
      .templateArgs
      .headOption
      .map(_.toString) shouldEqual Some("-A")
  }

  test("can access parent field") {
    setupEnv()
    val module = Modules.load("""
      |package foo;
      |import ergoline::int;
      |class bar { protected val x: int = 42; }
      |class foo extends bar { def getX(): int { return self.x; } }
      |""".stripMargin)
    Processes.onLoad(module)
  }

  test("for should encapsulate variable") {
    setupEnv()

    val module = Modules.load("""
      |package foo;
      |import ergoline::_;
      |def test (n: int) {
      |  for (var i = 0; i < (n - 1); i += 1) { test(i); }
      |  assert(i == (n - 1));
      |}
      |""".stripMargin)

    assertThrows[EirException]({
      Processes.onLoad(module)
    })
  }

  test("find implementation of") {
    setupEnv()

    val ctx = new TypeCheckContext()
    val range = globals.rangeType.asInstanceOf[EirClass]
    val iterable = globals.iterableType.asInstanceOf[EirTrait]
    val iterator = globals.iteratorType.asInstanceOf[EirTrait]

    val intRange = EirTemplatedType(None, range, List(globals.integerType))
    val intIterator = EirTemplatedType(None, iterator, List(globals.integerType))

    Find.implementationOf(intRange, iterable)(ctx).nonEmpty shouldBe true
    Find.implementationOf(intIterator, iterable)(ctx).isEmpty shouldBe true
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
    val partitioned = sorted.namespacePartitioned

    val expected1 = List("bar", "quux", "baz", "foobar")
    val expected2 = List("foo", "qux", "foo", "foobar")
    val knownGood = (expected1 ++ expected2).toSet
    def getNames[A <: EirNamedNode : ClassTag](classes: List[A]): List[String] = {
      classes collect {
        case n: A if knownGood.contains(n.name) => n.name
      }
    }

    getNames(sorted) shouldEqual expected1
    getNames(partitioned.map(_._1)) shouldEqual expected2
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

  test("verify that slicing works as expected") {
    setupEnv()
    // TODO restore this to string::tryParse<...> and auto-generate lambda
    val module = Modules.load("""
      |package foo;
      |import ergoline::_;
      |@main def hello(args : array<string>): unit {
      |    val n      = args.size();
      |    val slice1 = args[0:2:];
      |    val slice2 = args[0:2:n];
      |    val slice3 = args[:2:n];
      |}
      |""".stripMargin)
    Processes.onLoad(module)
  }

  test("test passing value as reference") {
    def program(failing: Boolean): String = {
      s"""
         |package foo;
         |import ergoline::_;
         |
         |def increment(i: int&) {
         |  i += 1;
         |}
         |
         |def test() {
         |  var i = 42;
         |  increment(${ if (failing) "i" else "&i" });
         |}
         |""".stripMargin
    }

    {
      setupEnv()
      val mod = Modules.load(program(failing = false))
      Processes.onLoad(mod)
    }

    {
      setupEnv()
      val mod = Modules.load(program(failing = true))
      assertThrows[EirException]({
        Processes.onLoad(mod)
      })
    }
  }

  test("check upper & lower type bounds") {
    setupEnv()
    val module = Modules.load("""
      |package foo;
      |
      |trait bird {}
      |class duck with bird {}
      |
      |class foo<A >: duck> {
      |  def foo() {}
      |}
      |
      |class bar<A <: bird> {
      |  def bar() {}
      |}
      |
      |def baz() {
      |    var a = new foo<bird>();
      |    var b = new bar<duck>();
      |}
      |""".stripMargin)
    Processes.onLoad(module)
  }

  test("use predicates to disambiguate functions") {
    setupEnv()
    val module = Modules.load("""
      |package foo;
      |
      |abstract class bird {}
      |
      |class owl extends bird {
      |   def owl() {}
      |   def hoot() {}
      |}
      |
      |class duck extends bird {
      |  def duck() {}
      |  def quack() {}
      |}
      |
      |def bar<A>(a: A) where (A == owl) {
      |  a.hoot();
      |}
      |
      |def bar<A>(a: A) where (A == duck) {
      |  a.quack();
      |}
      |
      |def baz() {
      |    bar(new owl());
      |    bar(new duck());
      |}
      |""".stripMargin)
    Processes.onLoad(module)
  }
}
