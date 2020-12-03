package edu.illinois.cs.ergoline

import java.nio.file.Paths

import edu.illinois.cs.ergoline.ast.{EirGlobalNamespace, EirNode}
import edu.illinois.cs.ergoline.passes.{GenerateCi, Processes}
import edu.illinois.cs.ergoline.resolution.Modules.load
import java.io.{File, PrintWriter}
import scala.util.Properties.{lineSeparator => n}


object Driver extends App {
  // get the options from the command-line args
  val (options, files) = args.partition(x => x startsWith "-")
  globals.strict = options.contains("-Wall")
  globals.verbose = options.contains("--verbose")
  // open each specified file
  val modules: Iterable[EirNode] =
    files.map(Paths.get(_)).map(x => load(x.toFile, EirGlobalNamespace))
  // resolve all the symbols :)
//  modules.foreach(x => Processes.onLoad(x.scope.get))
  // visit each file
//  modules.foreach(x => println(x.unparse))

  val cpp = new PrintWriter(new File("generate.cc" ))
  cpp.write(Processes.generateCpp().mkString(n))
  cpp.close()

  val ci = new PrintWriter(new File("generate.ci" ))
  ci.write(GenerateCi.visitAll())
  ci.close()
}
