package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.parsing.Parser
import edu.illinois.cs.ergoline.passes.Processes
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichEirNode, RichOption}
import edu.illinois.cs.ergoline.util.{AstManipulation, Errors}
import edu.illinois.cs.ergoline.{ErgolineLexer, ErgolineParser, Visitor, util}
import fastparse._
import org.antlr.v4.runtime._

import java.io.File
import java.io.File.pathSeparator
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.util.Properties

object Modules {
  val packageFile = "package.erg"
  val homePathEnv = "ERG_HOME"
  val searchPathEnv = "ERG_CLASSPATH"
  var useFastParse = true

  val ergolineHome = Option(Properties.envOrElse(homePathEnv, "."))
    .map(Paths.get(_))
    .find(Files.isDirectory(_))

  val hypercommHome = ergolineHome
    .map(_.resolve("hypercomm"))
    .filter(dir => Files.isDirectory(dir) && !Files.list(dir).toList.isEmpty)
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

  def charmHome: Option[Path] = Properties
    .envOrNone("CHARM_HOME")
    .map(Paths.get(_))
    .find(Files.isDirectory(_))

  def charmc: Option[Path] = charmHome
    .map(_.resolve("bin").resolve("charmc"))
    .find(Files.isExecutable)

  val loadedFiles: mutable.Map[File, EirNamedNode] = mutable.Map()
  val fileSiblings: mutable.Map[EirNode, List[EirNode]] = mutable.Map()

  private val fileStack: mutable.Stack[File] = mutable.Stack()

  def CurrentFile: Option[File] = fileStack.headOption

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

  private def readString(file: File): String = {
    new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8)
  }

  def load(src: String, scope: EirScope = EirGlobalNamespace): EirScope = {
    load(Left(src), scope) match {
      case (res: EirScope, _) => res
      case (res, _)           => Errors.incorrectType(res, classOf[EirScope])
    }
  }

  def load(
      src: Either[String, File],
      scope: EirScope
  ): (EirNode, List[EirNode]) = {
    val file = src.toOption
    file.foreach(fileStack.push)
    val res =
      if (useFastParse) {
        val txt = src match {
          case Right(file) => readString(file)
          case Left(src)   => src
        }
        val (ids, nodes) = parse(txt, Parser.Program(_)) match {
          case Parsed.Success(res, _) => res
          case f: Parsed.Failure =>
            val pos = f.extra.input.prettyIndex(f.index)
            Errors.exit(
              s"${file.map(_.getCanonicalFile).getOrElse("???")}:$pos: " + f
                .trace()
                .longAggregateMsg
            )
        }
        val pkg = ids.map(_.toList).map(loadPackage(_, file, scope))
        pkg
          .map(pkg => {
            Visitor.dropSelf(pkg, file)
            exportNodes(file, pkg, nodes.toList, enclose = true)
          })
          .getOrElse({
            Errors.exit(s"could not load $src")
          })
      } else {
        new Visitor(scope).visitProgram(
          (src match {
            case Left(str) => parserFromString(str)
            case Right(f)  => parserFromPath(f.toPath)
          }).program(),
          file
        )
      }
    file.foreach(f => assert(f == fileStack.pop()))
    res
  }

  def loadPackage(
      ids: List[String],
      fileOption: Option[File],
      scope: EirScope
  ): EirScope = {
    fileOption match {
      case None => Modules.retrieve(ids, scope)
      case Some(file) =>
        val absPath = file.getCanonicalFile.toPath
        ids.reverse
          .foldRight((absPath, scope))((name, pathScope) => {
            val parent = pathScope._1.getParent
            if (parent.getFileName.endsWith(name)) {
              val loaded = Modules.provisional(parent.toFile, pathScope._2).get
              (parent, util.assertValid[EirScope](loaded))
            } else {
              throw new RuntimeException(
                s"could not locate $name within ${pathScope._1}"
              )
            }
          })
          ._2
    }
  }

  private def placeNamespace(
      scope: EirScope,
      ns: EirNamespace
  ): EirNamespace = {
    if (ns.parent.contains(scope)) {
      ns
    } else {
      val alt = retrieve(ns.name, scope)
      ns.children.foreach {
        case ns: EirNamespace => placeNamespace(alt, ns)
        case node             => AstManipulation.placeNode(alt, node, enclose = true)
      }
      alt
    }
  }

  def exportNodes(
      file: Option[File],
      scope: EirScope,
      nodes: List[EirNode],
      enclose: Boolean = false
  ): (EirNode, List[EirNode]) = {
    // put the nodes into the module
    nodes.foreach {
      // namespaces are singletons, so we need to avoid replication
      case ns: EirNamespace =>
        if (enclose) {
          placeNamespace(scope, ns)
        } else {
          assert(ns.parent.contains(scope))
        }
      // otherwise we simply want to place it!
      case node => AstManipulation.placeNode(scope, node, enclose)
    }
    // seek the file's exported object
    file
      .map(expectation)
      .map(name => {
        if (file.exists(Modules.isPackageFile) && scope.hasName(name))
          (scope, nodes)
        else nodes.partition(_.hasName(name)) match {
          case (head :: Nil, x) => (head, x)
          case _ => throw new RuntimeException(
              s"could not locate $name in ${file.get.getName}"
            )
        }
      })
      .getOrElse((scope, Nil))
  }

  def parserFromString(s: String): ErgolineParser =
    parserFromCharStream(CharStreams.fromString(s))

  def parserFromCharStream(cs: CharStream): ErgolineParser = {
    val lexer = new ErgolineLexer(cs)
    val tokens = new CommonTokenStream(lexer)
    val parser = new ErgolineParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(ErgolineErrorListener)
    parser
  }

  def apply(qualified: List[String], scope: EirScope): Option[EirScope] = {
    qualified match {
      case Nil          => Some(scope)
      case head :: Nil  => this(head, scope)
      case head :: tail => this(head, scope).flatMap(this(tail, _))
    }
  }

  def apply(name: String, scope: EirScope): Option[EirScope] = {
    // find a directory/file with the desired name, and provisionally import it
    Find
      .child[EirNamedNode](scope, withName(name))
      .headOption
      .map({
        case x: EirFileSymbol => Find.uniqueResolution[EirNamedNode](x)
        case x                => x
      })
      .orElse({
        searchPath
          .map(_.resolve(name).toFile)
          .find(_.exists())
          .flatMap(provisional(_, scope))
      })
      .to[EirScope]
  }

  def discoverSources(f: File): (Option[File], Iterable[File]) = {
    val children = f.listFiles().map(_.getCanonicalFile)
    (
      children.find(x => !x.isDirectory && x.getName == packageFile),
      children.filter(x =>
        (x.isDirectory || x.getName.endsWith(
          ".erg"
        )) && (x.getName != packageFile)
      )
    )
  }

  def provisional(f: File, scope: EirScope): Option[EirNamedNode] = {
    val file = f.getCanonicalFile
    if (loadedFiles.contains(file)) {
      Some(loadedFiles(file))
    } else if (file.isDirectory) {
      val name = file.getName
      val (pkgFile, children) = discoverSources(file)
      val pkg: EirNamespace = retrieve(name, scope)
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
      AstManipulation.placeNode(scope, ns)
      ns
    }
  }

  def retrieve(name: List[String], scope: EirScope): EirNamespace = {
    val init = name.lastOption.zip(this(name.init, scope))
    init.flatMap { case (last, init) => this(last, init) } match {
      case Some(x: EirNamespace) => x
      case Some(x)               => Errors.incorrectType(x, classOf[EirNamespace])
      case None => init
          .map { case (last, init) => retrieve(last, init) }
          .getOrElse(Errors.unableToResolve(name, scope))
    }
  }

  private def memoryUsageMb: Long = {
    (Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory) / (1024 * 1024)
  }

  def currTimeMs: Long = System.currentTimeMillis()

  def load(f: File, scope: EirScope): EirNamedNode = {
    val file = f.getCanonicalFile
    val expected = Modules.expectation(f)
    val (startMb, startMs) = (memoryUsageMb, currTimeMs)
    val result = load(Right(file), scope)
    result match {
      case (value: EirNamedNode, siblings) if value.hasName(expected) =>
        fileSiblings(value) = siblings
        loadedFiles(file) = value
        Processes.onLoad(value)
        val (endMs, endMb) = (currTimeMs, memoryUsageMb)
        Errors.log(
          s"loaded ${file.getName} in ${endMs - startMs} ms (final mem usage ${Math
            .max(endMb - startMb, 0)} MB)"
        )
        value
      case _ => throw new RuntimeException(
          s"could not find $expected within ${file.getCanonicalPath}"
        )
    }
  }

  def parserFromPath(path: Path): ErgolineParser =
    parserFromCharStream(CharStreams.fromPath(path))

  def isPackageFile(file: File): Boolean = { file.getName == packageFile }

  def expectation(file: File): String = {
    if (isPackageFile(file)) file.getCanonicalFile.getParentFile.getName
    else {
      val name = file.getName
      val idx = name.indexOf('.')
      if (idx >= 0) name.substring(0, idx)
      else name
    }
  }
}
