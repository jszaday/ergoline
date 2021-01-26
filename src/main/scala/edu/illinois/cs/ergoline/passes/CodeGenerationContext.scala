package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.types.{EirTupleType, EirType}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.passes.UnparseAst.tab
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.Errors

import scala.collection.mutable
import scala.util.Properties.{lineSeparator => n}

class CodeGenerationContext(val language: String = "cpp") {

  var lines: List[String] = Nil
  val ignores: mutable.Stack[String] = new mutable.Stack[String]
  val current: StringBuilder = new StringBuilder
  private val _pointerOverrides: mutable.Set[EirNode] = mutable.Set()
  private val _proxies: mutable.Stack[EirProxy] = new mutable.Stack[EirProxy]
  private var _ctx : TypeCheckContext = new TypeCheckContext
  private var _replacements = Map[String, String]()

  def makePointer(n: EirNode): Unit = _pointerOverrides.add(n)
  def unsetPointer(n: EirNode): Unit = _pointerOverrides.remove(n)
  def hasPointerOverride(n: EirNode): Boolean = _pointerOverrides.contains(n)

  def updateProxy(proxy: EirProxy): Unit = {
    if (_proxies.headOption.contains(proxy)) {
      _proxies.pop()
    } else {
      _proxies.push(proxy)
    }
  }

  def resolve[T <: EirNode](resolvable: EirResolvable[T]): T =
    Find.uniqueResolution(resolvable)

  def proxy: Option[EirProxy] = _proxies.headOption

  def makeSubContext(): CodeGenerationContext = {
    val subCtx = new CodeGenerationContext(language)
    // TODO do something smarter here
    subCtx._proxies.pushAll(_proxies.reverse)
    subCtx._ctx = _ctx
    subCtx
  }

  def typeContext: TypeCheckContext = _ctx

  def typeOf(n: EirNode): EirType = {
    n match {
      case x: EirExpressionNode => exprType(x)
      case x => CheckTypes.visit(typeContext, x)
    }
  }

  def eval2const(n: EirNode): EirLiteral = n match {
    case e: EirExpressionNode => CheckTypes.evaluateConstExpr(typeContext, e)
    case c: EirConstantFacade => c.value
    case r: EirResolvable[_] => eval2const(resolve(r))
    case _ => Errors.invalidConstExpr(n)
  }

  def specialize(s : EirSpecializable, sp : EirSpecialization): EirSpecialization = _ctx.specialize(s, sp)

  def leave(ours: EirSpecialization): Unit = _ctx.leave(ours)

  def hasSubstitution(t: EirTemplateArgument): Option[EirType] = {
    _ctx.hasSubstitution(t).map(CheckTypes.visit(_ctx, _))
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

  def typeFor(x: EirResolvable[EirType], ctx: Option[EirNode] = None): String = {
    resolve[EirNode](x) match {
      case t: EirTemplateArgument => nameFor(t, ctx)
      case t: EirType => typeFor(t, ctx)
      case n: EirNode => Errors.incorrectType(n, classOf[EirType])
    }
  }

  def typeFor(x: EirType, ctx: Option[EirNode]): String = {
    x match {
      case t: EirTupleType => s"std::tuple<${t.children.map(typeFor(_, ctx)) mkString ", "}>"
      case _ => (if (x.isPointer) "std::shared_ptr<%s>" else "%s").format(nameFor(x, ctx))
    }
  }

  def exprType(expr: EirExpressionNode): EirType = expr.foundType.getOrElse(Errors.missingType(expr))

  def <<(ctx: CodeGenerationContext): CodeGenerationContext = ctx

  def <<(node: EirNode)(implicit visitor: (CodeGenerationContext, EirNode) => Unit): CodeGenerationContext = {
    visitor(this, node)
    this
  }

  def <|(t: (Option[EirNode], String))(implicit visitor: (CodeGenerationContext, EirNode) => Unit): CodeGenerationContext = {
    t._1 match {
      case Some(n) => this << n
      case None => this << t._2
    }
  }

  def <<[T <: EirNode](t: (Iterable[T], String))(implicit visitor: (CodeGenerationContext, EirNode) => Unit): CodeGenerationContext = {
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


  def << (t: (Iterable[String], String)): CodeGenerationContext = {
    val (values: Iterable[String], separator: String) = t
    if (values.nonEmpty) {
      for (value <- values.init) {
        this << value << separator
      }
      this << values.last
    }
    this
  }

  def <<(nodes: Iterable[EirNode])(implicit visitor: (CodeGenerationContext, EirNode) => Unit): CodeGenerationContext = {
    nodes.foreach(visitor(this, _))
    this
  }

  def <<(option: Option[String]): CodeGenerationContext = {
    option.map(this << _).getOrElse(this)
  }

  def <<(unit: Unit): CodeGenerationContext = this

  private def endLine(isFor: Boolean, s: Option[Char]): Boolean = {
    s match {
      case Some(';') => !isFor
      case Some('{' | '}') => true
      case _ => false
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

  def << (values: Iterable[String]): CodeGenerationContext = {
    for (value <- values) this << value
    this
  }

  val maxLineWidth = 120

  private def skip(s: String): Boolean = {
    val ptn = raw"namespace\s+([_a-zA-Z0-9:]+)\{\}"
    s.matches(ptn)
  }

  override def toString: String = {
    if (current.nonEmpty) {
      lines :+= current.toString()
      current.clear()
    }
    val output = new StringBuilder
    var numTabs = 0
    def t: String = List.fill(numTabs)(tab).mkString("")
    for (line <- lines) {
      val count = seekUnbalanced(line).sign
      if (count < 0) numTabs = Math.max(numTabs + count, 0)
      if (!skip(line)) output.append(t).append(line).append(n)
      if (count > 0) numTabs += count
    }
    output.toString()
  }

  def last: Char = current.toString().trim.lastOption.orElse(lines.lastOption.flatMap(_.trim.lastOption)).getOrElse('\u0000')
}
