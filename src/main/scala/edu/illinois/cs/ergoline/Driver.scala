package edu.illinois.cs.ergoline

import java.io.{File, PrintWriter}
import java.nio.file.Paths

import edu.illinois.cs.ergoline.ast.{EirGlobalNamespace, EirNode}
import edu.illinois.cs.ergoline.passes.Processes
import edu.illinois.cs.ergoline.resolution.Modules
import edu.illinois.cs.ergoline.resolution.Modules.{charmc, load}
import edu.illinois.cs.ergoline.util.Errors

import scala.util.Properties
import scala.util.Properties.{lineSeparator => n}


object Driver extends App {
  private def helpMessage(): Unit = {
    print("""| ergoline compiler, pre-alpha version
             | --debug    print additional (compilation) error information
             | --verbose  print additional information
             |""".stripMargin)
    System.exit(0)
  }
  // get the options from the command-line args
  var (options, files) = args.toList.partition(x => (x startsWith "-") || !(x.toLowerCase endsWith "erg"))

  if (options.contains("-h")|| files.isEmpty) helpMessage()
  else if (options.contains("--debug")) Errors.useDebugAction()

  val skipCompilation = options.contains("--no-compile")
  globals.strict = options.contains("-Wall")
  globals.verbose = options.contains("--verbose")

  val out = {
    val idx = options.indexOf("-o")
    val hasOutput = idx >= 0 && idx < (options.length - 1)
    val opt =
      Option.when(hasOutput)("-o " + options(idx + 1)).getOrElse("-o a.out")
    if (hasOutput) {
      options = options.patch(idx, Nil, 2)
    }
    opt
  }

  options = options.filterNot(_ startsWith "--") ++
    Properties.envOrElse("CFLAGS", "").split(raw"\s+")

  val inclDir =
    "\"" + Modules.ergolineHome.map(_.resolve("include"))
      .getOrElse(Errors.unableToResolve("ERGOLINE_HOME"))
      .toRealPath().toString + "\""
  val start = Modules.currTimeMs
  // open each specified file
  val modules: Iterable[EirNode] =
    files.map(Paths.get(_)).map(x => load(x.toFile, EirGlobalNamespace))

  val cpp = new PrintWriter(new File("generate.cc"))
  cpp.write(Processes.generateCpp().mkString(n))
  cpp.close()

  val ci = new PrintWriter(new File("generate.ci"))
  ci.write(Processes.generateCi())
  ci.close()

  val codegen = Modules.currTimeMs
  println(s"ergoline compilation:\t${codegen - start}ms")

  if (!skipCompilation) compile()

  private def compile(): Unit = {
    try {
      val cmd = s"${charmc.getOrElse("charmc")} ${options mkString " "} generate.ci"
      println(s"$$ $cmd")
      os.proc(cmd.split(raw"\s+")).call()
    } catch {
      case throwable: Throwable => Errors.exit(throwable + " (is CHARM_HOME set?)")
    }

    val charmxi = Modules.currTimeMs
    println(s"charmxi compilation:\t${charmxi - codegen}ms")

    try {
      val cmd = s"${charmc.getOrElse("charmc")} ${options mkString " "} $out -I$inclDir generate.cc"
      println(s"$$ $cmd")
      os.proc(cmd.split(raw"\s+")).call()
    } catch {
      case throwable: Throwable => Errors.exit(throwable + " (is CHARM_HOME set?)")
    }

    val cxx = Modules.currTimeMs
    println(s"c++ compilation:\t${cxx - charmxi}ms")
  }
}
