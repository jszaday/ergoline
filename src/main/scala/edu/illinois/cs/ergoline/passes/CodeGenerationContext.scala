package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirNode, EirSpecializable, EirSpecialization, EirTemplateArgument}
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.passes.UnparseAst.tab
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}

import scala.collection.mutable
import scala.util.Properties.{lineSeparator => n}

class CodeGenerationContext {

  var lines: List[String] = Nil
  val ignores: mutable.Stack[String] = new mutable.Stack[String]
  val current: StringBuilder = new StringBuilder
  private var _substitutions: Map[EirSpecializable, EirSpecialization] = Map()

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
      case (sable, stion) => sable.templateArgs.zip(stion.specialization)
    }).collectFirst({
      case (arg, ty) if arg == t => Find.uniqueResolution(ty)
    })
  }

  def ignoreNext(s: String): Unit = ignores.push(s)

  def nameFor(node: EirNode): Unit = {
    this << GenerateCpp.nameFor(this, node)
  }

  def typeFor(x: EirResolvable[EirType])(implicit visitor: (CodeGenerationContext, EirNode) => Unit): Unit = {
    typeFor(Find.uniqueResolution(x))
  }

  def typeFor(x: EirType)(implicit visitor: (CodeGenerationContext, EirNode) => Unit): Unit = {
    if (x.isPointer) this << "std::shared_ptr<"
    try {
      nameFor(x)
    } catch {
      case _: MatchError => visitor(this, x)
    }
    if (x.isPointer) this << ">"
  }

  def appendSemi(): Unit = {
    if (current.nonEmpty && !current.endsWith(";")) this << ";"
  }

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

  def <<(value: String): CodeGenerationContext = {
    if (ignores.headOption.contains(value)) {
      ignores.pop()
      return this
    }
    if (value.startsWith("{") || value.endsWith("{")) {
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
