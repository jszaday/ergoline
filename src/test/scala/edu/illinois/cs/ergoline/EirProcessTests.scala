package edu.illinois.cs.ergoline

import java.io.File
import java.nio.file.{Files, Path, Paths}

import edu.illinois.cs.ergoline.resolution.Modules
import edu.illinois.cs.ergoline.util.Errors
import org.scalatest.FunSuite

import scala.util.Properties

class EirProcessTests extends FunSuite {
  Errors.exitAction = () => throw new RuntimeException("")

  def files(path: String = "examples"): Iterable[File] =
    Modules.discoverSources(Paths.get(path).toFile)._2.filter(_.isFile)

  def test(f: File): Unit = {
    val wd = os.pwd
    os.proc("sbt", "run " + "\"" + f.getCanonicalPath + "\"").call()
    charmc.foreach(x => os.proc(x.toAbsolutePath.toString, "generate.ci").call())
    charmc.foreach(x => os.proc(x.toAbsolutePath.toString, "-language", "charm++", "generate.cc", "-o", "a.out").call())
    os.move(wd / "generate.cc", wd / f.getName.replace("erg", "cc"))
    val output = charmc.map(_ => os.proc("charmrun", "a.out", "+setcpuaffinity", "++local", "++quiet").call().out.text())
    output.foreach(println(_))
  }

  def charmc: Option[Path] = charmHome
    .map(_.resolve("bin").resolve("charmc"))
    .find(Files.isExecutable)

  def charmHome: Option[Path] =
    Properties.envOrNone("CHARM_HOME")
      .map(Paths.get(_)).find(Files.isDirectory(_))

  test("compile examples") {
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
      case (f, t) => throw new RuntimeException(s"failure on ${f.getName} due to $t")
    })
  }
}
