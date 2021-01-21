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
  private var _substitutions: Map[EirSpecializable, EirSpecialization] = Map()
  private val _pointerOverrides: mutable.Set[EirNode] = mutable.Set()
  private val _proxies: mutable.Stack[EirProxy] = new mutable.Stack[EirProxy]

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
    subCtx
  }

  def typeContext: TypeCheckContext = new TypeCheckContext

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

  def specialize(s : EirSpecializable, sp : EirSpecialization): EirSpecialization = {
    _substitutions += (s -> sp)
    sp
  }

  def leave(ours: EirSpecialization): Unit = {
    _substitutions = _substitutions.filterNot({
      case (_, theirs) => ours == theirs
    })
  }

  def hasSubstitution(t: EirTemplateArgument): Option[EirType] = {
    _substitutions.flatMap({
      case (specializable, specialization) =>
        specializable.templateArgs.zip(specialization.types)
    }).collectFirst({
      case (arg, ty) if arg == t => resolve(ty)
    })
  }

  def temporary: String = "_"

  def ignoreNext(s: String): Unit = ignores.push(s)

  def nameFor(node: EirNode, usage: Option[EirNode] = None): String = {
    usage
      .map(GenerateCpp.qualifiedNameFor(this, _)(node))
      .getOrElse(GenerateCpp.nameFor(this, node))
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

  def appendSemi(): Unit = {
    if (current.nonEmpty && !current.endsWith(";")) this << ";"
  }

  def <<(ctx: CodeGenerationContext): CodeGenerationContext = ctx

  def <<(node: EirNode)(implicit visitor: (CodeGenerationContext, EirNode) => Unit): CodeGenerationContext = {
    visitor(this, node)
    this
  }

  def <||<(t: (Option[EirNode], String))(implicit visitor: (CodeGenerationContext, EirNode) => Unit): CodeGenerationContext = {
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

  private def isControl(s: Char): Boolean = {
    s == '(' || s == ')' || s == ':' || s == '.'
  }

  def append(value: String): CodeGenerationContext = {
    current.append(value)
    this
  }

  def <<(value: String): CodeGenerationContext = {
    if (ignores.headOption.contains(value)) {
      ignores.pop()
      return this
    }
    if (value.isEmpty) {
      return this
    } else if (value.startsWith("{") || value.endsWith("{")) {
      val curr = current.toString()
      lines +:= curr + (if (curr.isEmpty || curr.endsWith(" ")) "" else " ") + value
      current.clear()
    } else if (value.startsWith("}") || value.endsWith("}")) {
      if (current.nonEmpty) {
        lines +:= current.toString()
        current.clear()
      }
      if (value.nonEmpty) {
        if (isControl(value.last)) current.append(value)
        else lines +:= value
      }
    } else if ((value.endsWith(";") || value.endsWith(n)) && !current.startsWith("for")) {
      lines +:= current.toString() + value
      current.clear()
    } else if (value.nonEmpty) {
      if (current.nonEmpty && !current.endsWith(" ") && !(isControl(value.head) || isControl(current.last))) current.append(" ")
      current.append(value)
    }
    this
  }

  def << (values: Iterable[String]): CodeGenerationContext = {
    for (value <- values) this << value
    this
  }


  val maxLineWidth = 120

  override def toString: String = {
    if (current.nonEmpty) {
      lines +:= current.toString()
      current.clear()
    }
    val reversed = lines.reverse
    val output = new StringBuilder
    var numTabs = 0
    def t: String = List.fill(numTabs)(tab).mkString("")
    for (line <- reversed) {
//      if ((line + t).length >= maxLineWidth) { }
      if (!line.matches(raw".*?\{.*\}$$")) {
        if (line.endsWith("}") || line.endsWith("};") || line.startsWith("}")) numTabs = Math.max(numTabs - 1, 0)
      }
      output.append(t).append(line).append(n)
      if (line.endsWith("{")) numTabs += 1
    }
    output.toString()
  }
}
