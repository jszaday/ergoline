package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.literals.EirLiteral
import edu.illinois.cs.ergoline.ast.{EirExpressionNode, EirGlobalNamespace}
import edu.illinois.cs.ergoline.passes.{StaticEvaluator, TypeCheckContext}
import edu.illinois.cs.ergoline.resolution.Modules
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

class EirConstexprTests extends FunSuite {

  def parseExpression(s: String): EirExpressionNode = {
    EirImportTests.setupEnv()

    new Visitor(EirGlobalNamespace).visitAs[EirExpressionNode]({
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

}
