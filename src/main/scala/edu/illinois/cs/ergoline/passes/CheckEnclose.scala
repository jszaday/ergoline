package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.EirNode

object CheckEnclose {
  def visit(node : EirNode): Option[EirNode] = {
    node.children.find(!_.parent.contains(node)) match {
      case None => node.children.map(visit).find(_.isDefined).flatten
      case x => x
    }
  }

  def apply(node : EirNode): Option[EirNode] = visit(node)
}
