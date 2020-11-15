package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.Driver.{parserFromString, visitProgram}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.Find
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}

class EirParseTest extends FunSuite {
  EirGlobalNamespace.clear()
  test("define class and resolve it") {
    val ns = visitProgram(parserFromString("package foo; namespace bar { class baz { } }"))
    assert(Find.byName[EirClass](List("foo", "bar", "baz"), ns).isDefined)
  }
  test("singleton tuple yields same type") {
    val ns = visitProgram(parserFromString("package foo; class bar { var baz : (unit) ; var qux : unit ; }"))
    val bazDeclaredType = Find.byName[EirDeclaration](List("foo", "bar", "baz"), ns).map(_.declaredType)
    val quxDeclaredType = Find.byName[EirDeclaration](List("foo", "bar", "qux"), ns).map(_.declaredType)
    bazDeclaredType shouldEqual quxDeclaredType
  }
  test("mini bin op precedence test") {
    val expression = (new Visitor).visitExpression(parserFromString("4 + 4 * 6").expression())
    expression should matchPattern {
      case EirBinaryExpression(_, _, "+", EirBinaryExpression(_, _, "*", _)) =>
    }
  }
  test("tuple tests") {
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
    visitProgram(parserFromString("package foo; @entry func bar(): unit { }"))
    val fs = Find.annotatedWith[EirFunction]("entry", EirGlobalNamespace)
    fs should matchPattern {
      case EirFunction(_, _, "bar", _, _, _) :: Nil =>
    }
  }
}
