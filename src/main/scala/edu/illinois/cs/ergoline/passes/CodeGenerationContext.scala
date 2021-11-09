package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.literals.EirLiteral
import edu.illinois.cs.ergoline.ast.types.{
  EirReferenceType,
  EirTemplatedType,
  EirTupleType,
  EirType
}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.passes.GenerateCpp.{asMember, isOption}
import edu.illinois.cs.ergoline.passes.Processes.ctx
import edu.illinois.cs.ergoline.passes.UnparseAst.tab
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.Errors

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Properties.{lineSeparator => n}

object CodeGenerationContext {
  type Sentinel = (Boolean, String, Option[mutable.Stack[String]])
}

class CodeGenerationContext(val language: String, val tyCtx: TypeCheckContext) {

  import CodeGenerationContext.Sentinel

  var lines: List[String] = Nil
  val ignores: mutable.Stack[String] = new mutable.Stack[String]
  val current: StringBuilder = new StringBuilder
  private val _pointerOverrides: mutable.Set[EirNode] = mutable.Set()
  private val _proxies: mutable.Stack[EirProxy] = new mutable.Stack[EirProxy]
  private var _replacements = Map[String, String]()

  private val _selves = new mutable.Stack[String]
  private val _proxySelves = new mutable.Stack[String]
  private val _sentinels = new mutable.Stack[Sentinel]
  private val _inplace = mutable.Set[EirFunctionCall]()

  def appendLast(str: String): Unit = {
    lines.lastOption.foreach(last => {
      lines = lines.dropRight(1) :+ (last + str)
    })
  }

  def pushSelf(self: String): Unit = _selves.push(self)

  def popSelf(): Unit = _selves.pop()

  def currentSelf: String = _selves.headOption.getOrElse("this")

  def pushProxySelf(self: String): Unit = _proxySelves.push(self)
  def popProxySelf(): Unit = _proxySelves.pop()
  def currentProxySelf: String = _proxySelves.headOption.getOrElse("this")

  def checked: Map[EirSpecializable, List[EirSpecialization]] = tyCtx.checked

  def hasChecked(x: EirSpecializable): Boolean = checked.contains(x)

  def isMailbox(x: EirLambdaExpression): Boolean = asMember(x.disambiguation)
    .flatMap(_.parent)
    .flatMap(Find.tryClassLike)
    .exists(GenerateCpp.isMailbox)

  def lambdas: Map[EirNamespace, List[EirLambdaExpression]] = tyCtx.lambdas
    .filterNot(isMailbox)
    .groupBy(x =>
      Find.parentOf[EirNamespace](x).getOrElse(Errors.missingNamespace(x))
    )

  def repack(x: EirFunctionCall): Unit = _inplace.add(x)
  def shouldRepack(x: EirFunctionCall): Boolean = _inplace.contains(x)

  def makePointer(n: EirNode): Unit = _pointerOverrides.add(n)
  def unsetPointer(n: EirNode): Unit = _pointerOverrides.remove(n)
  def hasPointerOverride(n: EirNode): Boolean = _pointerOverrides.contains(n)

  def pushSentinel(s: Sentinel): Unit = _sentinels.push(s)
  def popSentinel(s: Sentinel): Unit = assert(s.eq(_sentinels.pop()))
  def peekSentinel(): Option[Sentinel] = _sentinels.headOption

  def updateProxy(proxy: EirProxy): Unit = {
    if (_proxies.headOption.contains(proxy)) {
      _proxies.pop()
    } else {
      _proxies.push(proxy)
    }
  }

  def tryResolve[T <: EirNode: ClassTag](
      resolvable: EirResolvable[T]
  ): Option[T] = Find.resolutions[T](resolvable).headOption

  def resolve[T <: EirNode: ClassTag](resolvable: EirResolvable[T]): T =
    tryResolve(resolvable).getOrElse(Errors.unableToResolve(resolvable))

  def proxy: Option[EirProxy] = _proxies.headOption

  def makeSubContext(): CodeGenerationContext = {
    val subCtx = new CodeGenerationContext(language, tyCtx)
    // TODO do something smarter here
    subCtx._proxies.pushAll(_proxies.reverse)
    subCtx._selves.pushAll(_selves.reverse)
    subCtx
  }

  def typeContext: TypeCheckContext = tyCtx

  def typeOf(n: EirNode): EirType = {
    n match {
      case x: EirExpressionNode => exprType(x)
      case x                    => CheckTypes.visit(x)(typeContext)
    }
  }

  def eval2const(x: EirNode): EirLiteral[_] =
    StaticEvaluator.evaluate(x)(typeContext)

  def specialize(
      s: EirSpecializable,
      sp: EirSpecialization
  ): EirSpecialization = tyCtx.specialize(s, sp)

  def leave(ours: EirSpecialization): Unit = tyCtx.leave(ours)

  def hasSubstitution(t: EirTemplateArgument): Option[EirType] = {
    tyCtx.hasSubstitution(t).map(CheckTypes.visit(_)(tyCtx))
  }

  def temporary: String = "_"

  def ignoreNext(s: String): Unit = ignores.push(s)

  def nameFor(node: EirNode, usage: Option[EirNode] = None): String = {
    val name = usage
      .map(GenerateCpp.qualifiedNameFor(this, _)(node))
      .getOrElse(GenerateCpp.nameFor(this, node))
    _replacements.getOrElse(name, name)
  }

  def putReplacement(from: String, to: String): Unit = {
    _replacements += (from -> to)
  }

  def typeFor(
      x: EirResolvable[EirType],
      ctx: Option[EirNode] = None
  ): String = {
    resolve[EirNode](x) match {
      case t: EirTemplateArgument => nameFor(t, ctx)
      case t: EirType =>
        typeFor(t, ctx.filterNot(_ => x.isInstanceOf[EirConstantFacade]))
      case n: EirNode => Errors.incorrectType(n, classOf[EirType])
    }
  }

  private def makeShared(wrap: Boolean, s: String): String = {
    if (wrap) s"std::shared_ptr<$s>" else s
  }

  def typeFor(x: EirType, ctx: Option[EirNode]): String = {
    x match {
      case t: EirTupleType =>
        s"std::tuple<${t.children.map(typeFor(_, ctx)) mkString ", "}>"
      case t: EirTemplatedType if isOption(resolve(t.base)) =>
        val arg = resolve(t.args.head) match {
          case EirReferenceType(_, ty) => resolve(ty)
          case ty                      => ty
        }
        // optionals are wrapped when they're not already pointers
        makeShared(!arg.isPointer(this), typeFor(arg, ctx))
      case _ => makeShared(
          x match {
            case _: EirTemplateArgument => false
            case _                      => x.isPointer(this)
          },
          nameFor(x, ctx)
        )
    }
  }

  def exprType(expr: EirExpressionNode): EirType =
    expr.foundType.getOrElse(Errors.missingType(expr))

  def <<(ctx: CodeGenerationContext): CodeGenerationContext = ctx

  def <<(node: EirNode)(implicit
      visitor: (CodeGenerationContext, EirNode) => Unit
  ): CodeGenerationContext = {
    visitor(this, node)
    this
  }

  def <|(
      t: (Option[EirNode], String)
  )(implicit
      visitor: (CodeGenerationContext, EirNode) => Unit
  ): CodeGenerationContext = {
    t._1 match {
      case Some(n) => this << n
      case None    => this << t._2
    }
  }

  def <<[T <: EirNode](
      t: (Iterable[T], String)
  )(implicit
      visitor: (CodeGenerationContext, EirNode) => Unit
  ): CodeGenerationContext = {
    val (nodes: Iterable[EirNode], separator: String) = t
    if (nodes.nonEmpty) {
      for (node <- nodes.init) {
        visitor(this, node)
        this << separator
      }
      visitor(this, nodes.last)
    }
    this
  }

  def <<(t: (Iterable[String], String)): CodeGenerationContext = {
    val (values: Iterable[String], separator: String) = t
    if (values.nonEmpty) {
      for (value <- values.init) {
        this << value << separator
      }
      this << values.last
    }
    this
  }

  def <<(
      nodes: Iterable[EirNode]
  )(implicit
      visitor: (CodeGenerationContext, EirNode) => Unit
  ): CodeGenerationContext = {
    nodes.foreach(visitor(this, _))
    this
  }

  def <<(option: Option[String]): CodeGenerationContext = {
    option.map(this << _).getOrElse(this)
  }

  def <<(unit: Unit): CodeGenerationContext = this

  private def endLine(isFor: Boolean, s: Option[Char]): Boolean = {
    s match {
      case Some(';')       => !isFor
      case Some('{' | '}') => true
      case _               => false
    }
  }

  private def seekUnbalanced(s: String): Int = {
    if (s.contains('\"')) {
      0
    } else {
      val left = s.count(_ == '{')
      val right = s.count(_ == '}')
      left - right
    }
  }

  def append(value: String): CodeGenerationContext = {
    current.append(value)
    this
  }

  private def needsSpace(c: Option[Char]): Boolean = {
    // val control = List('(', ')', '{', '}', '[', ']', '<', '>', ',', '.', ':', ';')
    c.exists(c => c.isLetterOrDigit || c == '_')
  }

  def <<(value: String): CodeGenerationContext = {
    if (ignores.headOption.contains(value)) {
      ignores.pop()
    } else if (value.nonEmpty) {
      val ns = needsSpace(current.lastOption) && needsSpace(value.headOption)
      current.append(if (ns) s" $value" else value)
      if (endLine(current.startsWith("for"), current.lastOption)) {
        val curr = current.toString()
        if (curr == "}" && lines.lastOption.exists(_.endsWith("{"))) {
          lines = lines.init :+ (lines.last + "}")
        } else if (curr.startsWith(")") && lines.lastOption.contains("}")) {
          lines = lines.init :+ ("}" + curr)
        } else {
          lines :+= curr
        }
        current.clear()
      }
    }
    this
  }

  def <<(values: Iterable[String]): CodeGenerationContext = {
    for (value <- values) this << value
    this
  }

  val maxLineWidth = 120

  private def skip(s: String): Boolean = {
    val ptn = raw"namespace\s+([_a-zA-Z0-9:]+)\{\}"
    s.matches(ptn)
  }

  private def shouldCollapseAt(idx: Int): Option[Int] = {
    var count = 0
    for (i <- (idx to lines.size)) {
      if (lines(i) == "}" && count == 0) {
        if ((i + 1) < lines.size) {
          if (lines(i) == "}") {
            return Some(i)
          }
        }
      } else if (count < 0) {
        return None
      }

      count = count + lines(i).count(_ == '{') - lines(i).count(_ == '}')
    }

    ???
  }

  private def flattened(): Iterable[String] = {
    val collapsible = lines.indices filter { idx =>
      (idx + 1) < lines.size && lines(idx).endsWith("{") && (lines(
        idx + 1
      ) == "{")
    } map { idx => (idx, shouldCollapseAt(idx + 2)) } collect {
      case (i, Some(j)) => (i + 1, j)
    } flatten { case (i, j) => IndexedSeq(i, j) }

    lines.zipWithIndex filterNot { case (_, idx) =>
      collapsible.contains(idx)
    } map (_._1)
  }

  override def toString: String = {
    if (current.nonEmpty) {
      lines :+= current.toString()
      current.clear()
    }
    val output = new StringBuilder
    var numTabs = 0
    def t: String = List.fill(numTabs)(tab).mkString("")
    for (line <- flattened()) {
      val count = seekUnbalanced(line).sign
      if (count < 0) numTabs = Math.max(numTabs + count, 0)
      if (!skip(line)) output.append(t).append(line).append(n)
      if (count > 0) numTabs += count
    }
    output.toString()
  }

  def last: Char = current
    .toString()
    .trim
    .lastOption
    .orElse(lines.lastOption.flatMap(_.trim.lastOption))
    .getOrElse('\u0000')
}
