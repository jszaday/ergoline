package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.Driver.{parserFromString, visitProgram}
import edu.illinois.cs.ergoline.ast.EirGlobalNamespace
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper
import edu.illinois.cs.ergoline.passes.UnparseAst

class EirUnparseTests extends FunSuite {
  EirGlobalNamespace.clear()

  test("painless reconstruct demo") {
    val program =
      """namespace foo {
        |func bar(self: bar, baz=: unit): unit { }
        |
        |}""".stripMargin
    val namespace =
      (new Visitor).visitNamespace(parserFromString(program).namespace())
    program shouldEqual UnparseAst.visit(namespace)
  }
}
