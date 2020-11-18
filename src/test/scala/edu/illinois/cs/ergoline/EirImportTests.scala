package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.{EirFunction, EirGlobalNamespace}
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.resolution.Modules
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode
import org.scalatest.FunSuite
import org.scalatest.Matchers.convertToAnyShouldWrapper

class EirImportTests extends FunSuite {
  test("can find standard functions") {
    val module = Modules("ergoline", EirGlobalNamespace)
    module.isDefined shouldBe true
    val found = module.get.findWithin[EirFunction](withName("println")).headOption
    found.isDefined shouldBe true
  }
}
