package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.Driver.parserFromString
import edu.illinois.cs.ergoline.ast.EirGlobalNamespace
import edu.illinois.cs.ergoline.passes.{CheckEnclose, UnparseAst}
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

class EirUnparseTests extends FunSuite {
  EirGlobalNamespace.clear()

  import UnparseAst.t

  test("test function and empty block") {
    val program =
      """func foo<T>(bar=: T, baz: T): T { }""".stripMargin
    val function =
      (new Visitor).visitFunction(parserFromString(program).function())
    CheckEnclose(function) shouldEqual None
    program shouldEqual UnparseAst.visit(function)
  }

  test("namespace and c-style for loop") {
    val program =
      s"""namespace foo {
        |${t}@entry func bar(x: int, n: int): unit {
        |$t${t}for (var y: int = ((x * 2) + 1); (y < n); y = (y + 1)) {
        |$t$t${t}println(y);
        |$t$t}
        |$t}
        |}""".stripMargin
    val namespace =
      (new Visitor).visitNamespace(parserFromString(program).namespace())
    CheckEnclose(namespace) shouldEqual None
    program shouldEqual UnparseAst.visit(namespace)
  }
}
