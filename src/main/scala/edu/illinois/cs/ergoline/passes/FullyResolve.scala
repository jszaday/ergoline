package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirImport, EirNode}
import edu.illinois.cs.ergoline.passes.FullyResolve.visit
import edu.illinois.cs.ergoline.resolution.EirResolvable

object FullyResolve {

  def seekImports(node : EirNode): Unit = {
    node.children.foreach({
      case x: EirImport if !x.resolved =>
        x.resolve()
      case child => seekImports(child)
    })
  }

  def seekOthers(node : EirNode): Unit = {
    node.children.foreach({
      case x: EirResolvable[_] if !x.resolved =>
        x.resolve()
      case child => seekOthers(child)
    })
  }

  def visit(node : EirNode): Unit = {
    // Resolve imports first
    seekImports(node)
    // Then process everything else :)
    seekOthers(node)
  }

  def verify(node : EirNode): Boolean = {
    node.children.map({
      case resolvable: EirResolvable[_] => resolvable.resolved
      case child => verify(child)
    }).forall(identity)
  }
}
