package edu.illinois.cs.ergoline

import java.io.File
import java.nio.file.{Files, Path, Paths}

import edu.illinois.cs.ergoline.resolution.Modules
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.Errors.EirException
import org.scalatest.FunSuite

import scala.util.Properties

class EirProcessTests extends FunSuite {
  Errors.useDebugAction()

  def files(path: String = "examples"): Iterable[File] =
    Modules.discoverSources(Paths.get(path).toFile)._2.filter(_.isFile)

  private val wd = os.pwd
  private val tmp = os.temp.dir()

  def test(f: File): Unit = {
    os.proc("sbt", "run " + "\"" + f.getCanonicalPath + "\"").call()
    println("running charmc on generate.ci")
    charmc.foreach(x => os.proc(x.toAbsolutePath.toString, "generate.ci").call(cwd = wd))
    println("running charmc on generate.cc")
    charmc.foreach(x => os.proc(x.toAbsolutePath.toString, "-language", "charm++", "generate.cc", "-o", tmp / "a.out", "-Iinclude/").call(cwd = wd))
    println("moving .cc file")
    os.move.over(wd / "generate.cc", wd / f.getName.replace("erg", "cc"))
    println("current contents: " + (os.list(tmp) mkString ", "))
    println("starting process...")
    val output = charmc.map(_ => os.proc(tmp / "a.out", "16", "+p1", "++local").call(cwd = wd).out.text())
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
      case (f, t) => throw EirException(s"failure on ${f.getName} due to $t")
    })
  }
}
