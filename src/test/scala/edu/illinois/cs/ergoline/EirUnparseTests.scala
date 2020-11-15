package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.Driver.{parserFromString, visitProgram}
import edu.illinois.cs.ergoline.ast.EirGlobalNamespace
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper
import edu.illinois.cs.ergoline.passes.UnparseAst

class EirUnparseTests extends FunSuite {
  EirGlobalNamespace.clear()

  import UnparseAst.t

  test("test function and empty block") {
    val program =
      """func foo<T>(bar: T, baz: T): T { }""".stripMargin
    val namespace =
      (new Visitor).visitFunction(parserFromString(program).function())
    program shouldEqual UnparseAst.visit(namespace)
  }

  test("namespace and declaration") {
    val program =
      s"""namespace foo {
        |${t}func bar(): unit {
        |$t${t}val x: int = 4;
        |$t}
        |}""".stripMargin
    val namespace =
      (new Visitor).visitNamespace(parserFromString(program).namespace())
    val unparsed = UnparseAst.visit(namespace)
    println(unparsed)
    program shouldEqual unparsed
  }
}
