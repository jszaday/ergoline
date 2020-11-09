package edu.illinois.cs.ergoline

import java.nio.file.{Files, Paths}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

object Driver extends App {
  // get the options from the command-line args
  val (options, files) = args.partition(x => x startsWith "-")
  // open each specified file
  val parsers: Array[ErgolineParser] = files.map(Paths.get(_)).map(f => {
    assert(Files.exists(f))
    val cs = CharStreams.fromPath(f)
    val lexer = new ErgolineLexer(cs)
    val tokens = new CommonTokenStream(lexer)
    new ErgolineParser(tokens)
  })
  // visit each file
  for (parser <- parsers) {
    parser.program.enterRule(new Listener)
  }
}
