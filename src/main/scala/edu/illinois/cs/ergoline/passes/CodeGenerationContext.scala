package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.EirNode
import util.Properties.{lineSeparator => n}

class CodeGenerationContext {

  var lines: List[String] = Nil
  val current: StringBuilder = new StringBuilder

  def <<(node: EirNode)(implicit visitor: (CodeGenerationContext, EirNode) => Unit): CodeGenerationContext = {
    visitor(this, node)
    this
  }

  def <<(option: Option[EirNode], default: String)(implicit visitor: (CodeGenerationContext, EirNode) => Unit): CodeGenerationContext = {
    option match {
      case Some(node) => this << node
      case None => this << default
    }
  }

  def <<(nodes: Iterable[EirNode], separator: String)(implicit visitor: (CodeGenerationContext, EirNode) => Unit): CodeGenerationContext = {
    if (nodes.nonEmpty) {
      for (node <- nodes.init) {
        visitor(this, node)
        this << separator
      }
      visitor(this, nodes.last)
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
    s == '(' || s == ')'
  }

  def <<(value: String): CodeGenerationContext = {
    if (value.startsWith("{") || value.endsWith("{")) {
      val curr = current.toString()
      lines +:= curr + (if (curr.endsWith(" ")) "" else " ") + value
      current.clear()
    } else if (value.startsWith("}") || value.endsWith("}")) {
      if (current.nonEmpty) {
        lines +:= current.toString()
        current.clear()
      }
      lines +:= value
    } else if (value.endsWith(";")) {
      lines +:= current + value
      current.clear()
    } else {
      if (current.nonEmpty && !current.endsWith(" ") && !(isControl(value.head) || isControl(current.last))) current.append(" ")
      current.append(value)
    }
    this
  }

  def << (values: Iterable[String]): CodeGenerationContext = {
    for (value <- values) this << value
    this
  }

  def << (values: Iterable[String], separator: String): CodeGenerationContext = {
    this << (values mkString separator)
  }

  override def toString: String = lines.reverse.mkString(n)
}
