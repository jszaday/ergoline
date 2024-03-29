package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.passes.Processes
import edu.illinois.cs.ergoline.resolution.Modules
import edu.illinois.cs.ergoline.util.Errors.EirException
import org.scalatest.funsuite.AnyFunSuite

class EirFunctionTests extends AnyFunSuite {
  test("empty body fails") {
    EirImportTests.setupEnv()
    assertThrows[EirException]({
      val res = Modules.load("package foo; def bar();")
      Processes.onLoad(res)
    })
  }

  test("ambiguous definition fails (I)") {
    EirImportTests.setupEnv()
    assertThrows[EirException]({
      val res = Modules.load("package foo; def bar() {} def bar() {}")
      Processes.onLoad(res)
    })
  }

  test("ambiguous definition fails (II)") {
    EirImportTests.setupEnv()
    assertThrows[EirException]({
      val res = Modules.load("package foo; def bar(i: int) {} def bar(i: int) {}")
      Processes.onLoad(res)
    })
  }

  test("unambiguous definition ok") {
    EirImportTests.setupEnv()
    assertThrows[EirException]({
      val res = Modules.load("package foo; def bar(i: int) {} def bar(f: float) {}")
      Processes.onLoad(res)
    })
  }

  test("missing override fails") {
    EirImportTests.setupEnv()
    assertThrows[EirException]({
      val res = Modules.load("package foo; trait foo { def bar(); } class baz with foo { def bar() {} }")
      Processes.onLoad(res)
    })
  }

  test("incompatible override fails") {
    EirImportTests.setupEnv()
    assertThrows[EirException]({
      val res = Modules.load("package foo; trait foo { def bar(); } class baz with foo { override def bar(): foo {} }")
      Processes.onLoad(res)
    })
  }
}
