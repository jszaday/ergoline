package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.literals.{EirLiteral, EirLiteralType}
import edu.illinois.cs.ergoline.ast.{EirExpressionNode, EirScope}
import edu.illinois.cs.ergoline.passes.{StaticEvaluator, TypeCheckContext}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.resolution.Modules
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

class EirConstexprTests extends FunSuite {

  def parseExpression(s: String): EirExpressionNode = {
    EirImportTests.setupEnv()

    val module = globals.ergolineModule.to[EirScope]
    new Visitor(module.get).visitAs[EirExpressionNode]({
      Modules.parserFromString(s).staticExpression()
    })
  }

  def evaluateExpression(x: EirExpressionNode, opt: Option[TypeCheckContext] = None): EirLiteral[_] = {
    StaticEvaluator.evaluate(x)(opt.getOrElse(new TypeCheckContext()))
  }

  test("a number is found") {
    val result = evaluateExpression(parseExpression("(21 + 21) * 42"))
    result.toInt shouldEqual (42 * 42)
  }

  test("relational ops I") {
    val result = evaluateExpression(parseExpression("(21 + 21) != 42"))
    result.toBoolean shouldEqual false
  }

  test("relational ops II") {
    val result = evaluateExpression(parseExpression("((21 + 21) == 42) && false"))
    result.toBoolean shouldEqual false
  }

  test("tuple check") {
    val result = evaluateExpression(parseExpression("(21, 42, 63)[1] == 42"))
    result.toBoolean shouldEqual true
  }

  test("type check") {
    val result = evaluateExpression(parseExpression("(int, bool, string)[1] == bool"))
    result.toBoolean shouldEqual true
  }

  test("upper bound check") {
    val result = evaluateExpression(parseExpression("range<int> <: iterable<int>"))
    result.toBoolean shouldEqual true
  }

  test("lower bound check") {
    val result = evaluateExpression(parseExpression("iterable<int> >: range<int>"))
    result.toBoolean shouldEqual true
  }

  test("prefix op check") {
    val result = evaluateExpression(parseExpression("!(-(0 - 5) <= 0)"))
    result.toBoolean shouldEqual true
  }
}
