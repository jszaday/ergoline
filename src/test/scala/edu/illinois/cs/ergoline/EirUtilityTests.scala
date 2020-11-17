package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.Driver.{parserFromString, visitProgram}
import edu.illinois.cs.ergoline.ast.types.{EirNamedType, EirType}
import edu.illinois.cs.ergoline.ast.{EirBlock, EirClass, EirDeclaration, EirGlobalNamespace, EirLiteral, EirNamedNode, EirNode, EirSymbol}
import edu.illinois.cs.ergoline.passes.FullyResolve
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}
import util.EirUtilitySyntax.RichEirNode;

class EirUtilityTests extends FunSuite {
  EirGlobalNamespace.clear()

  test("block position and contains") {
    EirGlobalNamespace.clear()
    val b = new Visitor().visitBlock(
      parserFromString("{ val x : int = 42; val y : int = 16; }").block()).get
    // test a fairly elaborate query
    b.findWithin[EirLiteral](_.value == "16")
      .flatMap(b.findPositionOf(_)) shouldEqual List(1)
    b.children.zipWithIndex.foreach({
      case (n, i) => n.visitAll(x => {
        Option.when(!x.isInstanceOf[EirResolvable[_]])(b.findPositionOf(x) shouldEqual Some(i))
      })
    })
    val dummy = EirLiteral(None, null, null)
    b.findPositionOf(dummy) shouldEqual None
  }

  test("fully resolve and verify") {
    EirGlobalNamespace.clear()
    val block = (new Visitor()).visitBlock(parserFromString("{ val x : int = 42; val y : int = x; val z : int = x * y; }").block())
    block.foreach(FullyResolve.visit)
    block.exists(FullyResolve.verify(_)) shouldBe true
  }

  test("symbol resolution") {
    val foo = visitProgram(parserFromString("package foo ; class bar { val self : bar ; }"))
    val symbol = Find.all[EirSymbol[EirNamedType]](foo).headOption
    symbol.map(_.resolve()) should matchPattern {
      case Some(EirClass(_, _, "bar", _, _, _)) =>
    }
  }

  test("should not find before definition") {
    EirGlobalNamespace.clear()
    val foo = visitProgram(parserFromString("package foo ; func bar(): unit { baz; val baz : unit = (); }"))
    val symbol = Find.all[EirSymbol[EirDeclaration]](foo).find(_.qualifiedName == List("baz"))
    val declaration = Find.all[EirDeclaration](foo).headOption
    symbol.isDefined shouldBe true
    declaration.isDefined shouldBe true
    val ancestor = Find.commonAncestor(symbol.get, declaration.get)
    ancestor should matchPattern {
      case Some(_ : EirBlock) =>
    }
    assertThrows[java.lang.RuntimeException]({
      symbol.get.resolve()
    })
  }
}
