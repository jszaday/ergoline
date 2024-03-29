package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.EirTupleType
import edu.illinois.cs.ergoline.passes.CheckTypes.TypeCheckException
import edu.illinois.cs.ergoline.passes.{CheckTypes, TypeCheckContext, UnparseAst}
import edu.illinois.cs.ergoline.resolution.{Find, Modules}
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.resolution.Modules.parserFromString
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.Errors.EirException
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{convertToAnyShouldWrapper, matchPattern}

class EirParseTest extends AnyFunSuite {
  private object ParseTestSyntax {
    implicit class RichIterable[T](iterable: Iterable[T]) {
      def headOnly: T = {
        val lst = iterable.toList
        lst.length shouldEqual 1
        lst.head
      }
    }
  }

  Errors.useDebugAction()

  import ParseTestSyntax.RichIterable

  test("define class and resolve it") {
    EirGlobalNamespace.clear()
    val result = Modules.load("package foo; namespace bar { class baz { } }")
    val symbol = EirSymbol[EirClassLike](Some(result), List("foo", "bar", "baz"))
    Find.uniqueResolution[EirClassLike](symbol)
  }

  test("tuple with one element same type as base type") {
    val t = (new Visitor).visitTupleType(parserFromString("((unit), unit)").tupleType())
    t.isInstanceOf[EirTupleType] shouldBe true
    val types = t.asInstanceOf[EirTupleType].children
    types.length shouldBe 2
    types.head.toString shouldEqual types.last.toString
  }

  test("mini bin op precedence test") {
    EirGlobalNamespace.clear()
    val expression = (new Visitor).visitExpression(parserFromString("4 + 4 * 6").expression())
    expression should matchPattern {
      case EirBinaryExpression(_, _, "+", EirBinaryExpression(_, _, "*", _)) =>
    }
  }

  test("some slicing stuff") {
    EirGlobalNamespace.clear()
    val v = new Visitor
    val s = "a[:, :1, 1:, 1:2, :1:2, 1:2:, 1:2:3, :2:]"
    val x = v.visitExpression(parserFromString(s).expression())
    UnparseAst.visit(x) shouldEqual s
  }

  import EirUnparseTests._

  test("await many and when statements") {
    EirGlobalNamespace.clear()
    val v = new Visitor
    val s = localize (
     s"""await all {
         |${t}when foo(x: _), bar(y: _) if (x != y) => {
         |$t${t}return ((2 * x) + y);
         |$t}
         |}""".stripMargin
    )
    val x = v.visitAwaitManyStatement(parserFromString(s).awaitManyStatement())
    val res = localize(UnparseAst.visit(x))
    s shouldEqual res
  }

  test("tuple tests") {
    EirGlobalNamespace.clear()
    val v = new Visitor
    val e1 = v.visitExpression(parserFromString("(4 + 4)").expression())
    e1 should matchPattern {
      case EirBinaryExpression(_, _, "+", _) =>
    }
    val e2 = v.visitExpression(parserFromString("()").expression())
    e2.unparse shouldEqual "()"
    val e3 = v.visitExpression(parserFromString("(4, 4)").expression())
    e3 should matchPattern {
      case EirTupleExpression(_, elements) if elements.length == 2 =>
    }
  }
  test("mini function args test") {
    EirGlobalNamespace.clear()
    val f = (new Visitor).visitFunction(parserFromString("def foo (bar : unit, =baz : *unit): unit { }").function())
    f.functionArgs.length shouldEqual 2
    f.functionArgs.head should matchPattern {
      case EirFunctionArgument(_, "bar", _, false, false, false) =>
    }
    f.functionArgs.last should matchPattern {
      case EirFunctionArgument(_, "baz", _, true, true, false) =>
    }
  }
  test("annotated function retrieval test") {
    EirGlobalNamespace.clear()
    val ns = Modules.load("package foo; @entry def bar(): unit { }")
    val fs =
      Find.within[EirFunction](ns, _.annotation("entry").isDefined).headOption
    fs should matchPattern {
      case Some(f: EirFunction) if f.name == "bar" =>
    }
  }
  test("type-check ternary operator") {
    EirGlobalNamespace.clear()
    val v = new Visitor
    implicit val ctx: TypeCheckContext = new TypeCheckContext
    val a = v.visitExpression(parserFromString("(1 < 2) ? 1 : 2").expression())
    val b = v.visitExpression(parserFromString("1 ? 1 : 2").expression())
    val c = v.visitExpression(parserFromString("(1 < 2) ? \"potato\" : 2").expression())
    CheckTypes.visit(a) shouldEqual globals.integerType
    // cannot use non-boolean as test
    assertThrows[EirException](CheckTypes.visit(b))
    // must be able to unify expressions' types
    assertThrows[EirException](CheckTypes.visit(c))
  }
}
