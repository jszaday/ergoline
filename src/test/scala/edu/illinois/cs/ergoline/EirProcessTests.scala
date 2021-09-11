package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.resolution.Modules
import edu.illinois.cs.ergoline.resolution.Modules.charmc
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.Errors.EirException
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.nio.file.Paths

class EirProcessTests extends AnyFunSuite {
  Errors.useDebugAction()

  private val wd = os.pwd
  private val tmp = os.temp.dir()

  def files(path: String = "examples"): List[File] =
    Modules.discoverSources(Paths.get(path).toFile)._2.filter(_.isFile).toList.sortBy(_.getName)

  def test(f: File): Unit = {
    val charmHome = Modules.charmHome.map(os.Path(_))
    val charmRun = charmHome.map(_ / "bin" / "charmrun")
    val out1 = os.proc("java", "-jar", "ergc.jar", f.getCanonicalPath, "-o", tmp / "a.out").call().out.text()
    println(out1)
    println("starting process...")
    val out2 = charmc.map(_ => os.proc(charmRun.getOrElse(???), tmp / "a.out", "16", "+p2", "++local").call(cwd = wd).out.text())
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
        if (f.getName == "jacobi2d.erg") {
          println("skipping jacobi2d (for now)")
        } else {
          println(s"testing ${f.getName}...")
          test(f)
        }
      } catch {
        case t: Throwable => failures +:= (f, t)
      }
    }

    if (failures.nonEmpty) {
      throw EirException(
        s"failures in [ ${failures.map(_._1.getName) mkString ", "} ]!"
      )
    }
  }
}
