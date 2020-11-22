package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirClassLike, EirFunction, EirFunctionCall, EirImport, EirNode, EirSymbol}
import edu.illinois.cs.ergoline.passes.FullyResolve.visit
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}

object FullyResolve {

  def seekImports(node : EirNode): Unit = {
    node.children.foreach({
      case x: EirImport if !x.resolved => x.resolve()
      case child => seekImports(child)
    })
  }

  private def fullyResolve(x : EirResolvable[_ <: EirNode]): Unit = {
    var curr : Option[EirResolvable[_]] = Some(x);
    do {
      curr = curr.flatMap(_.resolve() match {
        case x: EirResolvable[_] if !x.resolved => Some(x)
        case _ => None
      })
    } while (curr.exists(!_.resolved))
  }

  def seekOthers(node : EirNode): Unit = {
    node.children.foreach({
      case x: EirResolvable[_] if !x.resolved => fullyResolve(x)
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
