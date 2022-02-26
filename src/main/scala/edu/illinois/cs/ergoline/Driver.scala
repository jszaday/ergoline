package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.analysis.{ControlFlow, Segmentation}
import edu.illinois.cs.ergoline.ast.{EirGlobalNamespace, EirNamespace, EirNode}
import edu.illinois.cs.ergoline.passes.{Processes, Registry}
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.resolution.Modules.{charmc, load}
import edu.illinois.cs.ergoline.resolution.{Find, Modules}
import edu.illinois.cs.ergoline.util.{Errors, LibUtils}

import java.io.{File, PrintWriter}
import java.nio.file.Paths
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
  var (options, files) = args.toList.partition(x =>
    (x startsWith "-") || !(x.toLowerCase endsWith "erg")
  )

  if (options.contains("-h") || files.isEmpty) helpMessage()
  else if (options.contains("--debug")) Errors.useDebugAction()

  Modules.useFastParse = !options.contains("--antlr4")

  if (options.contains("--cfa")) {
    Registry.instance[ControlFlow.Pass]
  }

  if (options.contains("--sdag")) {
    Registry.instance[Segmentation.Pass]
  }

  val skipCompilation = options.contains("--no-compile")
  globals.strict = options.contains("-Wall")
  globals.verbose = options.contains("--verbose")
  val idx = options.indexOf("-fno-inplace")
  if (idx >= 0) {
    options = options.patch(idx, Nil, 1)
    globals.enableInPlace = false
  }

  val out = {
    val idx = options.indexOf("-o")
    val hasOutput = idx >= 0 && idx < (options.length - 1)
    val opt = {
      if (hasOutput) Seq("-o", options(idx + 1))
      else Seq("-o", "a.out")
    }
    if (hasOutput) {
      options = options.patch(idx, Nil, 2)
    }
    opt
  }

  options = options.filterNot(_ startsWith "--") ++
    Properties.envOrElse("CFLAGS", "").split(raw"\s+")

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

  val usingBlas = globals.ergolineModule
    .flatMap(Find.child(_, withName("blas")).headOption)
    .exists(_.isInstanceOf[EirNamespace])

  val usingTasking = Find
    .child(EirGlobalNamespace, withName("stencil"))
    .exists(_.isInstanceOf[EirNamespace])

  if (!skipCompilation) compile()

  private def compile(): Unit = {
    def splitInfo(o: Option[String]): Seq[String] = {
      o.toSeq.flatMap(_.split(" "))
    }

    val ergoInclPaths = Seq(
      Modules.ergolineHome.map(_.resolve("include"))
    )

    val info = Modules.hypercommHome.flatMap(util.readLibsAndIncludes)

    val hyperLibs = splitInfo(info.map(_._1)) ++ Option.when(usingTasking)({
      "-lhypercomm-tasking"
    })
    val hyperIncl = splitInfo(info.map(_._2))

    val inclPaths: Seq[String] = hyperIncl ++ {
      ergoInclPaths
        .flatMap(_.map(_.toRealPath().toString))
        .map("-I" + _)
    }

    try {
      val cmd =
        s"${charmc.getOrElse("charmc")} ${options mkString " "} generate.ci"
      println(s"$$ $cmd")
      os.proc(cmd.split(raw"\s+")).call()
    } catch {
      case throwable: Throwable =>
        Errors.exit(throwable.getMessage + " (is CHARM_HOME set?)")
    }

    val charmxi = Modules.currTimeMs
    println(s"charmxi compilation:\t${charmxi - codegen}ms")

    try {
      val linkOptions: Iterable[String] = options ++ {
        if (usingBlas) LibUtils.linkLib("gsl")
        else None
      } ++ hyperLibs
      val cmd: Iterable[String] =
        Seq(charmc.map(_.toRealPath().toString).getOrElse("charmc")) ++
          (linkOptions ++ out ++ inclPaths ++ Seq("generate.cc"))
      println(s"$$ ${cmd mkString " "}")
      os.proc(cmd.filter(_.nonEmpty)).call()
    } catch {
      case throwable: Throwable =>
        Errors.exit(throwable.getMessage + " (is CHARM_HOME set?)")
    }

    val cxx = Modules.currTimeMs
    println(s"c++ compilation:\t${cxx - charmxi}ms")
  }
}
