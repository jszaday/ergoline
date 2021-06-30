package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.{EirConstantFacade, EirExpressionNode, EirGlobalNamespace, EirLiteral}
import edu.illinois.cs.ergoline.passes.Processes
import edu.illinois.cs.ergoline.resolution.Modules
import edu.illinois.cs.ergoline.util.Errors.EirException
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}

class EirConstexprTests extends FunSuite {

  def parseExpression(s: String): EirExpressionNode = {
    EirImportTests.setupEnv()

    new Visitor(EirGlobalNamespace).visitAs[EirExpressionNode]({
      Modules.parserFromString(s).staticExpression()
    })
  }

  test("a number is found") {
    val expected = "42"
    val result = parseExpression(expected)

    result should matchPattern {
      case EirLiteral(_, _, actual) if expected == actual =>
    }
  }

}
