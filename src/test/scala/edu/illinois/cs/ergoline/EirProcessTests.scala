package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.resolution.Modules
import edu.illinois.cs.ergoline.resolution.Modules.charmc
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.Errors.EirException
import org.scalatest.FunSuite

import java.io.File
import java.nio.file.Paths

class EirProcessTests extends FunSuite {
  Errors.useDebugAction()

  def files(path: String = "examples"): List[File] =
    Modules.discoverSources(Paths.get(path).toFile)._2.filter(_.isFile).toList.sortBy(_.getName)

  private val wd = os.pwd
  private val tmp = os.temp.dir()

  def test(f: File): Unit = {
    val out1 = os.proc("java", "-jar", "ergc.jar", f.getCanonicalPath, "-o", tmp / "a.out").call().out.text()
    println(out1)
   println("starting process...")
    val out2 = charmc.map(_ => os.proc(tmp / "a.out", "16", "+p1", "++local").call(cwd = wd).out.text())
    out2.foreach(println(_))
    println("(moving .cc file...)\n")
    try {
      os.move.over(wd / "generate.cc", wd / f.getName.replace("erg", "cc"))
    } catch {
      case throwable: Throwable => println(throwable)
    }
  }

  test("compile examples") {
    println("building ergc.jar\n")
    os.proc("sbt", "assembly").call()

    var failures: List[(File, Throwable)] = Nil
    for (f <- files()) {
      try {
        println(s"testing ${f.getName}...")
        test(f)
      } catch {
        case t: Throwable => failures +:= (f, t)
      }
    }
    failures.foreach({
      case (f, t) => throw EirException(s"failure on ${f.getName} due to $t")
    })
  }
}
