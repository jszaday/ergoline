package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.Driver.{parserFromString, visitProgram}
import edu.illinois.cs.ergoline.ast.types.{EirNamedType, EirType}
import edu.illinois.cs.ergoline.ast.{EirClass, EirGlobalNamespace, EirLiteral, EirNamedNode, EirNode, EirSymbol}
import edu.illinois.cs.ergoline.resolution.Find
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}
import util.EirUtilitySyntax.RichEirNode;

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
      case (n, i) => n.visitAll(x => {
        b.findPositionOf(x) shouldEqual Some(i)
      })
    })
    val dummy = EirLiteral(None, null, null)
    b.findPositionOf(dummy) shouldEqual None
  }

  test("symbol resolution") {
    val foo = visitProgram(parserFromString("package foo ; class bar { val self : bar ; }"))
    val symbol = Find.all[EirSymbol[EirNamedType]](foo).headOption
    symbol.map(_.resolved) should matchPattern {
      case Some(EirClass(_, _, "bar", _, _, _)) =>
    }
  }
}
