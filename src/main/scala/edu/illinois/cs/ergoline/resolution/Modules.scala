package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.passes.Processes
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode
import edu.illinois.cs.ergoline.util.{AstManipulation, Errors}
import edu.illinois.cs.ergoline.{ErgolineLexer, ErgolineParser, Visitor, util}
import org.antlr.v4.runtime._

import java.io.File
import java.io.File.pathSeparator
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.util.Properties

object Modules {
  val packageFile   = "package.erg"
  val homePathEnv   = "ERG_HOME"
  val searchPathEnv = "ERG_CLASSPATH"

  val ergolineHome =
    Option(Properties.envOrElse(homePathEnv, ".")).map(Paths.get(_)).find(Files.isDirectory(_))

  val hypercommHome = ergolineHome
    .map(_.resolve("hypercomm"))
    .filter(Files.isDirectory(_))
    .orElse({
      Properties.envOrNone("HYPERCOMM_HOME").map(Paths.get(_))
    })

  val coreModules: Path = ergolineHome
    .map(_.resolve("libs"))
    .getOrElse(Errors.unableToResolve(ergolineHome.toString))
  val searchPathDefaults: Seq[Path] = Seq(coreModules, Paths.get("."))
  val searchPath: Seq[Path] = (searchPathDefaults ++
    Properties
      .envOrNone(searchPathEnv)
      .toIterable
      .flatMap(_.split(pathSeparator))
      .map(Paths.get(_))).filter(Files.exists(_))

  def charmHome: Option[Path] =
    Properties
      .envOrNone("CHARM_HOME")
      .map(Paths.get(_))
      .find(Files.isDirectory(_))

  def charmc: Option[Path] =
    charmHome
      .map(_.resolve("bin").resolve("charmc"))
      .find(Files.isExecutable)

  val loadedFiles: mutable.Map[File, EirNamedNode]      = mutable.Map()
  val fileSiblings: mutable.Map[EirNode, List[EirNode]] = mutable.Map()

  private object ErgolineErrorListener extends ConsoleErrorListener {
    override def syntaxError(
        recognizer: Recognizer[_, _],
        offendingSymbol: Any,
        line: Int,
        charPositionInLine: Int,
        msg: String,
        e: RecognitionException
    ): Unit = {
      Errors.exit("line " + line + ":" + charPositionInLine + " " + msg)
    }
  }

  def load(body: String, scope: EirScope = EirGlobalNamespace): EirScope = {
    new Visitor(scope).visitProgram(parserFromString(body).program())
  }

  def parserFromString(s: String): ErgolineParser =
    parserFromCharStream(CharStreams.fromString(s))

  def parserFromCharStream(cs: CharStream): ErgolineParser = {
    val lexer  = new ErgolineLexer(cs)
    val tokens = new CommonTokenStream(lexer)
    val parser = new ErgolineParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(ErgolineErrorListener)
    parser
  }

  def retrieve(qualified: List[String], scope: EirScope): EirNamespace = {
    retrieve(qualified.last, qualified.reverse.tail.foldRight(scope)(retrieve))
  }

  def apply(qualified: List[String], scope: EirScope): Option[EirNamedNode] = {
    qualified match {
      case head :: Nil  => this(head, scope)
      case head :: tail => this(head, scope).map(x => retrieve(tail, util.assertValid[EirScope](x)))
      case Nil          => None
    }
  }

  def apply(name: String, scope: EirScope): Option[EirNamedNode] = {
    // find a directory/file with the desired name, and provisionally import it
    searchPath.map(_.resolve(name).toFile).find(_.exists()).flatMap(provisional(_, scope))
  }

  def discoverSources(f: File): (Option[File], Iterable[File]) = {
    val children = f.listFiles().map(_.getCanonicalFile)
    (
      children.find(x => !x.isDirectory && x.getName == packageFile),
      children.filter(x => (x.isDirectory || x.getName.endsWith(".erg")) && (x.getName != packageFile))
    )
  }

  def provisional(f: File, scope: EirScope): Option[EirNamedNode] = {
    val file = f.getCanonicalFile
    if (loadedFiles.contains(file)) {
      Some(loadedFiles(file))
    } else if (file.isDirectory) {
      val name                = file.getName
      val (pkgFile, children) = discoverSources(file)
      val pkg: EirNamespace   = retrieve(name, scope)
      loadedFiles(file) = pkg
      for (child <- children) {
        val symbol = EirFileSymbol(Some(pkg), child)
        pkg.children +:= symbol
        loadedFiles(child) = symbol
      }
      pkgFile.foreach(load(_, scope))
      Some(pkg)
    } else if (file.isFile) {
      Some(load(file, scope))
    } else {
      None
    }
  }

  def retrieve(name: String, scope: EirScope): EirNamespace = {
    Find.child[EirNamespace](scope, withName(name)).headOption.getOrElse {
      val ns = EirNamespace(Some(scope), Nil, name)
      AstManipulation.placeNodes(scope, List(ns))
      ns
    }
  }

  private def memoryUsageMb: Long = {
    (Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory) / (1024 * 1024)
  }

  def currTimeMs: Long = System.currentTimeMillis()

  def load(f: File, scope: EirScope): EirNamedNode = {
    val file               = f.getCanonicalFile
    val (startMb, startMs) = (memoryUsageMb, currTimeMs)
    val parser             = parserFromPath(file.toPath)
    val result             = new Visitor(scope).visitProgram(parser.program(), Some(file))
    result match {
      case (value: EirNamedNode, sibilings) if value.hasName(Modules.expectation(f)) =>
        fileSiblings(value) = sibilings
        loadedFiles(file) = value
        Processes.onLoad(value)
        val (endMs, endMb) = (currTimeMs, memoryUsageMb)
        Errors.log(
          s"loaded ${file.getName} in ${endMs - startMs} ms (final mem usage ${Math.max(endMb - startMb, 0)} MB)"
        )
        value
      case _ => throw new RuntimeException(s"could not find ${expectation(file)} within ${file.getCanonicalPath}")
    }
  }

  def parserFromPath(path: Path): ErgolineParser =
    parserFromCharStream(CharStreams.fromPath(path))

  def isPackageFile(file: File): Boolean = { file.getName == packageFile }

  def expectation(file: File): String = {
    if (isPackageFile(file))
      file.getCanonicalFile.getParentFile.getName
    else {
      val name = file.getName
      val idx  = name.indexOf('.')
      if (idx >= 0) name.substring(0, idx)
      else name
    }
  }
}
