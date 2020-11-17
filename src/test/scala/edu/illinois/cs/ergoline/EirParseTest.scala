package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.Driver.{parserFromString, visitProgram}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.resolution.Find.withName
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
    val result = visitProgram(parserFromString("package foo; namespace bar { class baz { } }"))
    val namespace = Find.qualifications(result, List("foo", "bar")).headOnly
    Find.child[EirClass](namespace, withName("baz")).headOnly
  }

  test("singleton tuple yields same type") {
    EirGlobalNamespace.clear()
    val result = visitProgram(parserFromString("package foo; class bar { var baz : (unit) ; var qux : unit ; }"))
    val foo = Find.qualifications(result, List("foo")).headOnly
    val bar = Find.child[EirClass](foo, withName("bar")).headOnly
    val bazDeclaredType = Find.child[EirDeclaration](bar, withName("baz")).map(_.declaredType)
    val quxDeclaredType = Find.child[EirDeclaration](bar, withName("qux")).map(_.declaredType)
    bazDeclaredType shouldEqual quxDeclaredType
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
    visitProgram(parserFromString("package foo; @entry func bar(): unit { }"))
    val fs = Find.annotatedWith[EirFunction](EirGlobalNamespace, "entry")
    fs should matchPattern {
      case EirFunction(_, _, "bar", _, _, _) :: Nil =>
    }
  }
}
