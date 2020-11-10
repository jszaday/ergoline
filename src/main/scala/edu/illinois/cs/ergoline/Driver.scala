package edu.illinois.cs.ergoline

import java.nio.file.{Files, Path, Paths}

import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream}

object Driver extends App {
  def parserFromCharStream(cs : CharStream): ErgolineParser = {
    val lexer = new ErgolineLexer(cs)
    val tokens = new CommonTokenStream(lexer)
    new ErgolineParser(tokens)
  }
  def parserFromPath(path : Path): ErgolineParser =
    parserFromCharStream(CharStreams.fromPath(path))
  def parserFromString(s : String): ErgolineParser =
    parserFromCharStream(CharStreams.fromString(s))
  def visitProgram(parser: ErgolineParser) =
    (new Visitor).visitProgram(parser.program())
  // get the options from the command-line args
  val (options, files) = args.partition(x => x startsWith "-")
  // open each specified file
  val parsers: Array[ErgolineParser] = files.map(Paths.get(_)).map(parserFromPath)
  // visit each file
  parsers.foreach(visitProgram)
}
