package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.{EirGlobalNamespace, EirNode, EirScope}

object Find {
  import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption

  def byName[T: Manifest](fqn: List[String], scope: Option[EirScope]): Option[T] = {
    scope match {
      case Some(s : EirScope) => byName(fqn, s)
      case _ => None
    }
  }

  def byName[T: Manifest](fqn: List[String], scope: EirScope = EirGlobalNamespace): Option[T] = {
    fqn match {
      case head :: Nil => scope(head).to[T]
      case init :+ last => byName[EirScope](init, scope).flatMap(x => x(last)).to[T]
      case _ => None
    }
  }

  def all[T: Manifest](scope : EirScope): Iterable[T] = {
    scope.children.collect{
      case x : EirScope => all(x)
    }.flatten ++ scope.children.collect {
      case x : T => x
    }
  }

  def matching[T: Manifest](pattern : PartialFunction[EirNode, T], scope : EirScope): Iterable[T] = {
    scope.children.collect{
      case x : EirScope => matching(pattern, x)
    }.flatten ++ scope.children.collect(pattern)
  }

  def annotatedWith[T <: EirNode: Manifest](name : String, scope : EirScope): Iterable[T] =
    matching[T]({
      case x : T if x.annotations.exists(_.name == name) => x
    }, scope)
}
