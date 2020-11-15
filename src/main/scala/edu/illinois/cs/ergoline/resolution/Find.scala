package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.{EirBlock, EirGlobalNamespace, EirNamedNode, EirNode, EirScope}
import edu.illinois.cs.ergoline.types.EirType

object Find {

  import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption

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

  def all[T: Manifest](scope: EirScope): Iterable[T] = {
    scope.children.collect {
      case x: EirScope => all(x)
    }.flatten ++ scope.children.collect {
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
}
