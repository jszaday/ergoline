package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.Driver.parserFromString
import edu.illinois.cs.ergoline.ast.EirGlobalNamespace
import edu.illinois.cs.ergoline.passes.{CheckEnclose, UnparseAst}
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

class EirUnparseTests extends FunSuite {
  import UnparseAst.t

  private val genericFunction = "func foo<T>(bar=: T, baz: T): T { }"
  private val parsedFunction = {
    EirGlobalNamespace.clear()
    (new Visitor).visitFunction(parserFromString(genericFunction).function())
  }

  test("check enclose of parsed function") {
    CheckEnclose(parsedFunction) shouldEqual None
  }

  test("check unparsed function") {
    genericFunction shouldEqual UnparseAst.visit(parsedFunction)
  }

  test("namespace and c-style for loop") {
    EirGlobalNamespace.clear()
    val program =
      s"""namespace foo {
        |${t}@entry func bar(x: int, n: int): unit {
        |$t${t}for (var y: int = ((x * 2) + 1); (y < n); y = (y + 1)) {
        |$t$t${t}println(y);
        |$t$t}
        |$t}
        |}
        |""".stripMargin
    val namespace =
      (new Visitor).visitNamespace(parserFromString(program).namespace())
    CheckEnclose(namespace) shouldEqual None
    program shouldEqual UnparseAst.visit(namespace)
  }
}