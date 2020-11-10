package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.Driver.{parserFromString, visitProgram}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.types.EirUnitType
import edu.illinois.cs.ergoline.util.EirUtilitySyntax._
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}

class EirParseTest extends FunSuite {
  test("define class and resolve it") {
    visitProgram(parserFromString("package foo; namespace bar { class baz { } }"))
    assert(List("foo", "bar", "baz").asResolvable[EirClass].resolve(EirGlobalNamespace).isDefined)
  }
  test("singleton tuple yields same type") {
    visitProgram(parserFromString("package foo; class bar { var baz : (unit) ; var qux : unit ; }"))
    val bazDeclaredType = util.findDeclaration(List("foo", "bar", "baz")).map(_.declaredType)
    val quxDeclaredType = util.findDeclaration(List("foo", "bar", "qux")).map(_.declaredType)
    bazDeclaredType shouldEqual quxDeclaredType
  }
}
