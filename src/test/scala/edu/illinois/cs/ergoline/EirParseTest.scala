package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.Driver.{parserFromString, visitProgram}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.util.EirUtilitySyntax._
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}

class EirParseTest extends FunSuite {
  test("define class and resolve it") {
    visitProgram(parserFromString("package foo; namespace bar { class baz { } }"))
    assert(util.find[EirClass](List("foo", "bar", "baz")).isDefined)
  }
  test("singleton tuple yields same type") {
    visitProgram(parserFromString("package foo; class bar { var baz : (unit) ; var qux : unit ; }"))
    val bazDeclaredType = util.find[EirDeclaration](List("foo", "bar", "baz")).map(_.declaredType)
    val quxDeclaredType = util.find[EirDeclaration](List("foo", "bar", "qux")).map(_.declaredType)
    bazDeclaredType shouldEqual quxDeclaredType
  }
  test("mini bin op precedence test") {
    val expression = (new Visitor).visitExpression(parserFromString("4 + 4 * 6").expression())
    expression should matchPattern {
      case EirBinaryExpression(_, _, "+", EirBinaryExpression(_, _, "*", _)) =>
    }
  }
}
