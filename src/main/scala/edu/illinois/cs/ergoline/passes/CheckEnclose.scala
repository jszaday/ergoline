package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.EirNode
import edu.illinois.cs.ergoline.passes.CheckEnclose.CheckEncloseSyntax.RichEirNode
import edu.illinois.cs.ergoline.resolution.EirResolvable

object CheckEnclose {
  object CheckEncloseSyntax {
    implicit class RichEirNode(node : EirNode) {
      def validEnclose(other : EirNode): Boolean = {
        node.isInstanceOf[EirResolvable[_]] || node.parent.contains(other)
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
