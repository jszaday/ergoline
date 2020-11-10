package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.Driver.{parserFromString, visitProgram}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.types.EirUnitType
import edu.illinois.cs.ergoline.util.EirResolvable
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}

class EirParseTest extends FunSuite {
  test("define class and resolve it") {
    visitProgram(parserFromString("package foo; namespace bar { class baz { } }"))
    assert(EirResolvable[EirClass](List("foo", "bar", "baz")).resolve(EirGlobalNamespace).isDefined)
  }
  test("singleton tuple yields same type") {
    visitProgram(parserFromString("package foo; class bar { var baz : (unit) ; var qux : unit ; }"))
    val bazDeclaredType = EirResolvable[EirMember](List("foo", "bar", "baz")).resolve(EirGlobalNamespace) match {
      case Some(EirMember(_, x: EirDeclaration, _)) => Some(x.declaredType)
      case _ => None
    }
    val quxDeclaredType = EirResolvable[EirMember](List("foo", "bar", "qux")).resolve(EirGlobalNamespace) match {
      case Some(EirMember(_, x: EirDeclaration, _)) => Some(x.declaredType)
      case _ => None
    }
    bazDeclaredType shouldEqual quxDeclaredType
  }
}
