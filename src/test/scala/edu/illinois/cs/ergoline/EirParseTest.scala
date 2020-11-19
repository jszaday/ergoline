package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.{Find, Modules}
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.resolution.Modules.parserFromString
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}

class EirParseTest extends FunSuite {
  private object ParseTestSyntax {
    implicit class RichIterable[T](iterable: Iterable[T]) {
      def headOnly: T = {
        val lst = iterable.toList
        lst.length shouldEqual 1
        lst.head
      }
    }
  }

  import ParseTestSyntax.RichIterable

  test("define class and resolve it") {
    EirGlobalNamespace.clear()
    val result = Modules.load("package foo; namespace bar { class baz { } }")
    val namespace = Find.qualifications(result, List("foo", "bar")).headOnly
    Find.child[EirClass](namespace, withName("baz")).headOnly
  }

  test("tuple with one element same type as base type") {
    val types = (new Visitor).visitTypeList(parserFromString("(unit), unit").typeList())
    types.length shouldBe 2
    types.head shouldEqual types.last
  }

  test("mini bin op precedence test") {
    EirGlobalNamespace.clear()
    val expression = (new Visitor).visitExpression(parserFromString("4 + 4 * 6").expression())
    expression should matchPattern {
      case EirBinaryExpression(_, _, "+", EirBinaryExpression(_, _, "*", _)) =>
    }
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
    val f = (new Visitor).visitFunction(parserFromString("func foo (var bar : unit, baz= : unit): unit { }").function())
    f.functionArgs.length shouldEqual 2
    f.functionArgs.head should matchPattern {
      case EirFunctionArgument(_, "bar", _, false, false) =>
    }
    f.functionArgs.last should matchPattern {
      case EirFunctionArgument(_, "baz", _, true, true) =>
    }
  }
  test("annotated function retrieval test") {
    EirGlobalNamespace.clear()
    val ns = Modules.load("package foo; @entry func bar(): unit { }")
    val fs = Find.annotatedWith[EirFunction](ns, "entry")
    fs should matchPattern {
      case EirFunction(_, _, "bar", _, _, _) :: Nil =>
    }
  }
}
