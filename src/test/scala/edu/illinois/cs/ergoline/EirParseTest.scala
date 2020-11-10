package edu.illinois.cs.ergoline

import org.scalatest.FunSuite
import Driver.{parserFromString, visitProgram}
import ast._
import edu.illinois.cs.ergoline.types.{EirTupleType, EirUnitType}
import edu.illinois.cs.ergoline.util.EirResolvable

class EirParseTest extends FunSuite {
  test("define class and resolve it") {
    visitProgram(parserFromString("package foo; namespace bar { class baz { } }"))
    assert(EirResolvable[EirClass](List("foo", "bar", "baz")).resolve(EirGlobalNamespace).isDefined)
  }
  test("empty tuple type yields unit") {
    val t = EirTupleType.fromElements(Nil)
    assert(t match {
      case Left(value) => value == EirUnitType
      case Right(_) => false
    })
  }
}
