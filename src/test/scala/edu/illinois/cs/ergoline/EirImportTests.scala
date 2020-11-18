package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.resolution.Modules
import org.scalatest.FunSuite

class EirImportTests extends FunSuite {
  test("dummy") {
    val module = Modules("ergoline").orNull
    println(module)
  }
}
