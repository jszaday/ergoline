package edu.illinois.cs.ergoline.resolution

import java.io.File
import java.io.File.{pathSeparator, separator}
import java.nio.file.{Files, Path, Paths}

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode
import edu.illinois.cs.ergoline.{ErgolineLexer, ErgolineParser, Visitor, util}
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream}

import scala.collection.mutable
import scala.util.Properties

object Modules {
  val packageFile = "package.erg"
  val homePathEnv = "ERG_HOME"
  val searchPathEnv = "ERG_CLASSPATH"
  val coreModules: Path =
    Paths.get(Properties.envOrElse(homePathEnv, "."))
      .resolve("libs")
  val searchPathDefaults: Seq[Path] = Seq(coreModules, Paths.get("."))
  val searchPath: Seq[Path] = (searchPathDefaults ++
    Properties.envOrNone(searchPathEnv).toIterable
      .flatMap(_.split(pathSeparator)).map(Paths.get(_))).filter(Files.exists(_))

  val loadedFiles : mutable.Map[File, EirNamedNode] = mutable.Map()

  def load(body: String, scope: EirScope = EirGlobalNamespace): EirScope = {
    new Visitor(scope).visitProgram(parserFromString(body).program())
  }

  def parserFromString(s: String): ErgolineParser =
    parserFromCharStream(CharStreams.fromString(s))

  def parserFromCharStream(cs: CharStream): ErgolineParser = {
    val lexer = new ErgolineLexer(cs)
    val tokens = new CommonTokenStream(lexer)
    new ErgolineParser(tokens)
  }

  def retrieve(qualified: List[String], scope: EirScope): EirNamespace = {
    retrieve(qualified.last, qualified.reverse.tail.foldRight(scope)(retrieve))
  }

  def apply(qualified: List[String], scope: EirScope): Option[EirNamedNode] = {
    qualified match {
      case head :: Nil => this(head, scope)
      case head :: tail => this(head, scope).map(x =>
        retrieve(tail, util.assertValid[EirScope](x)))
      case Nil => None
    }
  }

  def apply(name: String, scope: EirScope): Option[EirNamedNode] = {
    // find a directory/file with the desired name, and provisionally import it
    searchPath.map(_.resolve(name).toFile).find(_.exists()).flatMap(provisional(_, scope))
  }

  def provisional(f: File, scope: EirScope): Option[EirNamedNode] = {
    val file = f.getAbsoluteFile
    if (loadedFiles.contains(file)) {
      Some(loadedFiles(file))
    } else if (file.isDirectory) {
      val name = file.getName
      val children = file.listFiles().map(_.getAbsoluteFile)
      val pkg: EirNamespace = retrieve(name, scope)
      loadedFiles(file) = pkg
      children.find(_.getName == packageFile).foreach(load(_, scope))
      for (child <- children.filterNot(_.getName == packageFile)) {
        val symbol = EirFileSymbol(Some(pkg), child)
        pkg.children +:= symbol
        loadedFiles(child) = symbol
      }
      Some(pkg)
    } else if (file.isFile) {
      Some(load(file, scope))
    } else {
      None
    }
  }

  def retrieve(name: String, scope: EirScope): EirNamespace = {
    scope.findChild[EirNamespace](withName(name)).headOption
      .getOrElse({
        val ns = EirNamespace(Some(scope), Nil, name)
        util.placeNodes(scope, List(ns))
        ns
      })
  }

  def load(f: File, scope: EirScope): EirNamedNode = {
    val file = f.getAbsoluteFile
    val parser = parserFromPath(file.toPath)
    val result = new Visitor(scope).visitProgram(parser.program(), Some(file))
    result match {
      case Right(value) =>
        loadedFiles(file) = value
        value
      case _ => throw new RuntimeException(s"could not find ${expectation(file)} within ${file.getAbsolutePath}")
    }
  }

  def parserFromPath(path: Path): ErgolineParser =
    parserFromCharStream(CharStreams.fromPath(path))

  def expectation(file: File): String = {
    val name = file.getName
    if (name == packageFile)
      file.getAbsoluteFile.getParentFile.getName
    else {
      val idx = name.indexOf('.')
      if (idx >= 0) name.substring(0, idx)
      else name
    }
  }
}
