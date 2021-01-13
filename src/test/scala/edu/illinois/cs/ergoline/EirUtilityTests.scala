package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.Modules.parserFromString
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find, Modules}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.Errors.EirException
import org.scalatest.FunSuite
import org.scalatest.Matchers.{convertToAnyShouldWrapper, matchPattern}

class EirUtilityTests extends FunSuite {
  EirGlobalNamespace.clear()
  Errors.useDebugAction()

  test("block position and contains") {
    EirGlobalNamespace.clear()
    val b = new Visitor().visitBlock(
      parserFromString("{ val x : int = 42; val y : int = 16; }").block())
    // test a fairly elaborate query
    val pos =
      Find.within[EirLiteral](b, _.value == "16")
        .flatMap(b.findPositionOf(_)).headOption
    pos shouldEqual Some(1)
    b.children.zipWithIndex.foreach({
      case (n, i) => n.visitAll(x => {
        Option.when(!x.isInstanceOf[EirResolvable[_]])(b.findPositionOf(x) shouldEqual Some(i))
      })
    })
    val dummy = EirLiteral(None, null, null)
    b.findPositionOf(dummy) shouldEqual None
  }

  test("symbol resolution") {
    val foo = Modules.load("package foo ; class bar { val other : bar ; }")
    val symbol = Find.within[EirSymbol[EirNamedNode]](foo, _ => true).headOption
    symbol.flatMap(_.resolve().headOption) should matchPattern {
      case Some(EirClass(_, _, "bar", _, _, _)) =>
    }
  }

  test("should not find before definition") {
    EirGlobalNamespace.clear()
    val foo = Modules.load("package foo ; def bar(): unit { baz; val baz : unit = (); }")
    val symbol = Find.within[EirSymbol[EirNamedNode]](foo, _.qualifiedName == List("baz")).headOption
    val declaration = Find.within[EirDeclaration](foo, _ => true).headOption
    symbol.isDefined shouldBe true
    declaration.isDefined shouldBe true
    val ancestor = Find.commonAncestor(symbol.get, declaration.get)
    ancestor should matchPattern {
      case Some(_ : EirBlock) =>
    }
    symbol.get.resolve() shouldEqual Nil
  }

  test("should not access inaccessible scope") {
    EirGlobalNamespace.clear()
    val foo = Modules.load("package foo ; def bar(): unit { { val baz : unit = (); } baz; }")
    val symbol = Find.within[EirSymbol[EirNamedNode]](foo, _.qualifiedName == List("baz"))
    assertThrows[EirException]({
      symbol.foreach(Find.uniqueResolution(_))
    })
  }
}
