package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._

object Find {
  def byName[T <: EirNamedNode: Manifest](fqn: List[String], scope: Option[EirScope]): Option[T] = {
    scope match {
      case Some(s: EirScope) => byName(fqn, s)
      case _ => None
    }
  }

  def byName[T <: EirNamedNode: Manifest](fqn: List[String], scope: EirScope): Option[T] = {
    fqn match {
      case head :: Nil => scope.find(head)
      case init :+ last => byName[EirScope with EirNamedNode](init, scope).flatMap(_.find(last))
      case _ => None
    }
  }

  def all[T: Manifest](node: EirNode): Iterable[T] = {
    node.children.flatMap(all(_)) ++
      node.children.collect{
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

  def resolveSymbol[T](symbol: EirSymbol[T]): T = {
    val scope = symbol.scope.getOrElse(throw new RuntimeException(s"no scope for symbol $symbol"))


    symbol.resolved
  }
}
