package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.EirNode
import edu.illinois.cs.ergoline.resolution.EirResolvable

object FullyResolve {
  def visit(node : EirNode): Unit = {
    node.children.foreach({
      case resolvable: EirResolvable[_] if !resolvable.resolved =>
        node.parent.exists(_.replaceChild(resolvable, resolvable.resolve()))
      case child => visit(child)
    })
  }
}
