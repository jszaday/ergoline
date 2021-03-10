package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.EirGlobalNamespace
import edu.illinois.cs.ergoline.passes.{CheckEnclose, UnparseAst}
import edu.illinois.cs.ergoline.resolution.Modules.parserFromString
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

import scala.util.Properties

object EirUnparseTests {
  def t: String = UnparseAst.tab
  def localize(s: String): String = s.replaceAll("(\n|\r\n)+", Properties.lineSeparator)
}

class EirUnparseTests extends FunSuite {
  import EirUnparseTests._

  private val genericFunction = "def foo<T>(=bar: T, baz: T): T { }"
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
    val program = localize (
      s"""namespace foo {
        |@entry def bar(x: int, n: int): unit {
        |${t}for (var y: int = ((x * 2) + 1); (y < n); y += 1) {
        |$t${t}println(y);
        |$t}
        |}
        |}
        |""".stripMargin
    )
    val namespace =
      (new Visitor).visitNamespace(parserFromString(program).namespace())
    CheckEnclose(namespace) shouldEqual None
    program shouldEqual UnparseAst.visit(namespace)
  }
}
