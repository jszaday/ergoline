package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.EirNode
import util.Properties.{lineSeparator => n}
import UnparseAst.tab
import scala.util.matching.Regex

class CodeGenerationContext {

  var lines: List[String] = Nil
  val current: StringBuilder = new StringBuilder

  def appendSemi(): Unit = {
    if (current.nonEmpty && !current.endsWith(";")) this << ";"
  }

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
    s == '(' || s == ')' || s == ':' || s == '.'
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
      if (value.nonEmpty) {
        if (isControl(value.last)) current.append(value)
        else lines +:= value
      }
    } else if (value.endsWith(";")) {
      lines +:= current + value
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

  def << (values: Iterable[String], separator: String): CodeGenerationContext = {
    this << (values mkString separator)
  }

  val maxLineWidth = 120

  override def toString: String = {
    val reversed = lines.reverse
    val output = new StringBuilder
    var numTabs = 0
    def t: String = List.fill(numTabs)(tab).mkString("")
    for (line <- reversed) {
//      if ((line + t).length >= maxLineWidth) { }
      if (!line.matches(raw".*?\{.*\}$$")) {
        if (line.endsWith("}") || line.endsWith("};") || line.startsWith("}")) numTabs -= 1
      }
      output.append(t).append(line).append(n)
      if (line.endsWith("{")) numTabs += 1
    }
    output.toString()
  }
}
