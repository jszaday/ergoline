package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirFileSymbol, EirImport, EirNode, EirScopedSymbol}
import edu.illinois.cs.ergoline.resolution.{EirPlaceholder, EirResolvable, Find}
import edu.illinois.cs.ergoline.util.Errors

object FullyResolve {

  def seekImports(node : EirNode, stack: List[EirNode] = Nil): Unit = {
    val kids = Option.unless(stack.contains(node))(node.children).getOrElse(Nil)
    kids.foreach({
      case x: EirImport => if (!x.resolved ) Find.resolutions(x)
      case x if x.annotation("system").isDefined =>
        // TODO this PROBABLY should happen somewhere else.
        Processes.cppIncludes ++= x.annotation("system")
          .flatMap(_.opts.get("fromHeader").map(_.stripped))
        x
      case child => seekImports(child, stack :+ node)
    })
  }

  private def fullyResolve(x : EirResolvable[_ <: EirNode]): Unit = {
    var curr : Option[EirResolvable[_]] = Some(x)
    var prev : Option[EirResolvable[_]] = None
    do {
      prev = curr
      curr = curr.flatMap(Find.resolutions[EirNode](_) match {
        case Seq(x: EirResolvable[_]) if !x.resolved => Some(x)
        case _ => None
      })
      // Prevents infinite loops when a node cannot be resolved
    } while (curr.exists(!_.resolved) && curr != prev)
  }

  def seekOthers(node : EirNode, stack: List[EirNode] = Nil): Unit = {
    if (!stack.contains(node)) {
      node.children.foreach({
        case _: EirFileSymbol | _: EirPlaceholder[_] | _: EirImport =>
        case x: EirResolvable[_] if !x.resolved => fullyResolve(x)
        case child => seekOthers(child, stack :+ node)
      })
    }
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
      case s: EirScopedSymbol[_] => true
      case resolvable: EirResolvable[_] =>
        val resolved = resolvable.resolved
        if (!resolved) Errors.unableToResolve(resolvable)
        resolved
      case child => verify(child)
    }).forall(identity)
  }
}
