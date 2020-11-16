package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption

object Find {
  private var lastSearch : Int = -1

  type EirNamedScope = EirScope with EirNamedNode

  def withName[T <: EirNamedNode](name : String): T => Boolean =
    (n : T) => n.name == name

  def qualifications(scope: EirScope, fqn: List[String]): Iterable[EirNamedScope] = {
    fqn match {
      case head :: Nil  => globally[EirNamedScope](scope, withName(head))
      case init :+ last => qualifications(scope, init)
        .flatMap(_.findChild[EirNamedScope](withName(last)))
      case _ => Nil
    }
  }

  // search up, down, and at a node
  def globally[T : Manifest](scope: EirScope, predicate: T => Boolean): Iterable[T] = {
    lastSearch += 1
    globally(scope, predicate, lastSearch)
  }

  private def globally[T : Manifest](scope: EirScope, predicate: T => Boolean, key : Int): Iterable[T] = {
    Option(scope).flatMap({
      case t : T if predicate(t) => Some(t)
    }) ++ within(scope, predicate)
    // TODO check siblings of the scope
  }

  // recursively check all children of a node
  def within[T: Manifest](node : EirNode, predicate: T => Boolean): Iterable[T] = {
    Find.child(node, predicate) ++ node.children.flatMap(within(_, predicate))
  }

  // check only the immediate children of the node (do not descend)
  def child[T: Manifest](node : EirNode, predicate: T => Boolean): Iterable[T] = {
    node.children.collect{
      case t : T if predicate(t) => t
    }
  }

  def all[T: Manifest](node: EirNode): Iterable[T] = {
    node.children.flatMap(all(_)) ++
      node.children.collect {
        case x: T => x
      }
  }

  def matching[T: Manifest](pattern: PartialFunction[EirNode, T], scope: EirScope): Iterable[T] = {
    scope.children.collect {
      case x: EirScope => matching(pattern, x)
    }.flatten ++ scope.children.collect(pattern)
  }

  def annotatedWith[T <: EirNode : Manifest](name: String, scope: EirScope): Iterable[T] =
    matching[T]({
      case x: T if x.annotations.exists(_.name == name) => x
    }, scope)

  def returnType(block: EirBlock): EirResolvable[EirType] = ???

  def unionType(types: EirResolvable[EirType]*): EirType = ???

  def parentOf[T <: EirNode : Manifest](node : EirNode): Option[T] =
    node.parent.to[T].orElse(node.parent.flatMap(parentOf[T]))

  import FindSyntax.RichPredicate

  def fromSymbol[T <: EirNamedNode : Manifest](symbol: EirSymbol[T]): Iterable[T] = {
    val scope = symbol.scope.getOrElse(throw new RuntimeException(s"no scope for symbol $symbol"))
    symbol.qualifiedName match {
      case name :: Nil =>
        // global (unrestricted) search, may appear anywhere as long as its accessible
        Find.globally[T](scope, withName(name).and(symbol.canAccess(_)))
      case init :+ last =>
        // namespace (restricted) search, may only be a child of the specified namespace
        val qualified = Find.qualifications(scope, init).filter(symbol.canAccess(_))
        qualified.flatMap(_.findChild[T](withName(last).and(symbol.canAccess(_))))
    }
  }

  def typeOf(node : EirNode): EirResolvable[EirType] = {
    node match {
      case x : EirType => x
      case x : EirExpressionNode => x.eirType
      case _ => throw new RuntimeException(s"$node does not have a type!")
    }
  }

  object FindSyntax {
    implicit class RichPredicate[T](predicate: T => Boolean) {
      def and(other : T => Boolean): T => Boolean = (t : T) => predicate(t) && other(t)
    }
  }
}
