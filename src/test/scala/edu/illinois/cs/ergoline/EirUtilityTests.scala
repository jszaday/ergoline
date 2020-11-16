package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.Driver.parserFromString
import edu.illinois.cs.ergoline.ast.{EirGlobalNamespace, EirLiteral, EirNode}
import edu.illinois.cs.ergoline.resolution.Find
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

class EirUtilityTests extends FunSuite {
  EirGlobalNamespace.clear()

  test("block position and contains") {
    val b = new Visitor().visitBlock(
      parserFromString("{ val x : int = 42; val y : int = 16; }").block()).get
    // test a fairly elaborate query
    Find.all[EirLiteral](b)
      .find(_.value == "16")
      .flatMap(b.findPositionOf(_)) shouldEqual Some(1)
    b.children.zipWithIndex.foreach({
      case (n, i) => util.visitAll(n, x => {
        b.findPositionOf(x) shouldEqual Some(i)
      })
    })
    val dummy = EirLiteral(None, null, null)
    b.findPositionOf(dummy) shouldEqual None
  }
}
