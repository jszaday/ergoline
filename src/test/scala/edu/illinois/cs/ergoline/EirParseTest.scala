package edu.illinois.cs.ergoline

import org.scalatest.FunSuite

import Driver.{visitProgram,parserFromString}
import ast._

class EirParseTest extends FunSuite {
  test("define class and resolve it") {
    visitProgram(parserFromString("package foo; namespace bar { class baz { } }"))
    assert(EirResolvable[EirClass](List("foo", "bar", "baz")).resolve(EirGlobalNamespace).isDefined)
  }
}
