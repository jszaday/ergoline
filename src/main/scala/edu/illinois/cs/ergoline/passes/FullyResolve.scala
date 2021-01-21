package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirFileSymbol, EirImport, EirNode}
import edu.illinois.cs.ergoline.resolution.{EirPlaceholder, EirResolvable}
import edu.illinois.cs.ergoline.util.Errors

object FullyResolve {

  def seekImports(node : EirNode): Unit = {
    node.children.foreach({
      case x: EirImport if !x.resolved => x.resolve()
      case x: EirImport if x.resolved =>
      case x if x.annotation("system").isDefined =>
        // TODO this PROBABLY should happen somewhere else.
        Processes.cppIncludes ++= x.annotation("system")
          .flatMap(_.opts.get("fromHeader").map(_.stripped))
        x
      case child => seekImports(child)
    })
  }

  private def fullyResolve(x : EirResolvable[_ <: EirNode]): Unit = {
    var curr : Option[EirResolvable[_]] = Some(x)
    var prev : Option[EirResolvable[_]] = None
    do {
      prev = curr
      curr = curr.flatMap(_.resolve() match {
        case x: EirResolvable[_] if !x.resolved => Some(x)
        case _ => None
      })
      // Prevents infinite loops when a node cannot be resolved
    } while (curr.exists(!_.resolved) && curr != prev)
  }

  def seekOthers(node : EirNode): Unit = {
    node.children.foreach({
      case _: EirFileSymbol | _: EirPlaceholder[_] | _: EirImport =>
      case x: EirResolvable[_] if !x.resolved => fullyResolve(x)
      case child => seekOthers(child)
    })
  }

  def visit(node : EirNode): Unit = {
    // Resolve imports first (including those within the scope)
    // TODO this should be changed to "seek all accessible imports"
    seekImports(node.scope.getOrElse(node))
    // Then process everything else :)
    seekOthers(node)
  }

  def verify(node : EirNode): Boolean = {
    node.children.map({
      case resolvable: EirResolvable[_] =>
        val resolved = resolvable.resolved
        if (!resolved) Errors.unableToResolve(resolvable)
        resolved
      case child => verify(child)
    }).forall(identity)
  }
}
