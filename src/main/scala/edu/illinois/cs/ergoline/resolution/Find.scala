package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichBoolean, RichEirNode, RichOption}

import scala.collection.mutable

object Find {
  type EirNamedScope = EirScope with EirNamedNode

  def ancestors(x: EirNode): Seq[EirNode] = x.parent match {
    case Some(parent) => parent +: ancestors(parent)
    case None => Nil
  }

  // find where x's ancestors first overlap with y's
  def commonAncestor(x: EirNode, y: EirNode): Option[EirNode] = {
    val ancestors = Find.ancestors(x).toList
    Find.ancestors(y).find(ancestors.contains(_))
  }

  def withName[T <: EirNamedNode](name: String): T => Boolean =
    (n: T) => n.name == name

  def qualifications(scope: EirScope, fqn: List[String])(implicit ctx: Option[EirNode] = None): Iterable[EirNamedScope] = {
    fqn match {
      case head :: Nil => globally[EirNamedScope](scope, withName(head))
      case init :+ last => qualifications(scope, init)
        .flatMap(_.findChild[EirNamedScope](withName(last)))
      case _ => Nil
    }
  }

  // search up and down from a node
  def globally[T: Manifest](scope: EirScope, predicate: T => Boolean, searched: mutable.ArrayBuffer[EirScope] = mutable.ArrayBuffer())(implicit ctx: Option[EirNode] = None): Iterable[T] = {
    (ctx.forall(_.canAccess(scope)) && !searched.contains(scope)).ifTrue({
      searched += scope
      Find.within(scope, predicate) ++ scope.scope.toIterable.flatMap(globally(_, predicate, searched))
    })
  }

  // recursively check all children of a node
  def within[T: Manifest](node: EirNode, predicate: T => Boolean): Iterable[T] = {
    Find.child(node, predicate) ++ node.children.flatMap(within(_, predicate))
  }

  // check only the immediate children of the node (do not descend)
  def child[T: Manifest](node: EirNode, predicate: T => Boolean): Iterable[T] = {
    node.children.collect {
      case t: T if predicate(t) => t
    }
  }

  def all[T: Manifest](node: EirNode): Iterable[T] = {
    node.children.flatMap(all(_)) ++
      node.children.collect {
        case x: T => x
      }
  }

  def annotatedWith[T <: EirNode : Manifest](scope: EirScope, name: String): Iterable[T] = {
    Find.within[T](scope, _.annotations.exists(_.name == name))
  }

  // find values strictly "owned" by the owner, such that no other instance of its class owns it as well
  def owned[C <: EirNode : Manifest, T <: EirNode : Manifest](owner : C): Iterable[T] = {
    owner.findWithin((t : T) => parentOf[C](t).contains(owner))
  }

  def returnType(block: EirBlock): EirResolvable[EirType] =
    unionType(Find.owned[EirBlock, EirReturn](block).map(_.expression.eirType))

  def unionType(types: Iterable[EirResolvable[EirType]]): EirResolvable[EirType] = ???

  def unionType(types: EirResolvable[EirType]*): EirResolvable[EirType] = unionType(types)

  def parentOf[T <: EirNode : Manifest](node: EirNode): Option[T] =
    node.parent.to[T].orElse(node.parent.flatMap(parentOf[T]))

  import FindSyntax.RichPredicate

  def fromSymbol[T <: EirNamedNode : Manifest](symbol: EirSymbol[T]): Iterable[T] = {
    implicit val ctx: Option[EirNode] = Some(symbol)
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

  def typeOf(node: EirNode): EirResolvable[EirType] = {
    node match {
      case x: EirType => x
      case x: EirExpressionNode => x.eirType
      case _ => throw new RuntimeException(s"$node does not have a type!")
    }
  }

  object FindSyntax {

    implicit class RichPredicate[T](predicate: T => Boolean) {
      def and(other: T => Boolean): T => Boolean = (t: T) => predicate(t) && other(t)
    }

  }

}
