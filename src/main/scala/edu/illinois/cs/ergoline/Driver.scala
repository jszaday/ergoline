package edu.illinois.cs.ergoline

import java.nio.file.Paths

import edu.illinois.cs.ergoline.ast.{EirGlobalNamespace, EirNode, EirScope}
import edu.illinois.cs.ergoline.resolution.Modules.{load, parserFromPath}

object Driver extends App {
  // get the options from the command-line args
  val (options, files) = args.partition(x => x startsWith "-")
  // open each specified file
  val modules: Iterable[EirNode] =
    files.map(Paths.get(_)).map(x => load(x.toFile, EirGlobalNamespace))
  // visit each file
  modules.foreach(x => println(x.unparse))
}
