package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.EirNode
import edu.illinois.cs.ergoline.resolution.EirResolvable

object FullyResolve {
  def visit(node : EirNode): Unit = {
    node.children.foreach({
      case resolvable: EirResolvable[_] if !resolvable.resolved =>
        resolvable.resolve()
      case child => visit(child)
    })
  }

  def verify(node : EirNode): Boolean = {
    node.children.map({
      case resolvable: EirResolvable[_] => resolvable.resolved
      case child => verify(child)
    }).forall(identity)
  }
}
