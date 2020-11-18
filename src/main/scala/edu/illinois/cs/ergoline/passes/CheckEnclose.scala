package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirEncloseExempt, EirNode}
import edu.illinois.cs.ergoline.passes.CheckEnclose.CheckEncloseSyntax.RichEirNode

object CheckEnclose {
  object CheckEncloseSyntax {
    implicit class RichEirNode(node : EirNode) {
      def validEnclose(other : EirNode): Boolean = {
        node.isInstanceOf[EirEncloseExempt] || node.parent.contains(other)
      }
    }
  }

  def visit(node: EirNode): Option[EirNode] = {
    node.children.find(!_.validEnclose(node)) match {
      case None => node.children.map(visit).find(_.isDefined).flatten
      case x => x
    }
  }

  def apply(node: EirNode): Option[EirNode] = visit(node)
}
