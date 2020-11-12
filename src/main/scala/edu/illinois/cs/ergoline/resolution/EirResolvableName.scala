package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.EirScope

class EirResolvableName[T: Manifest](var fqn: List[String]) extends EirResolvable[T] {
  def resolve(scope: EirScope): T = Find.byName[T](fqn, scope) match {
    case Some(t: T) => t
    case None => throw new RuntimeException("could not resolve" + fqn)
  }

  override def toString: String = s"Resolvable!${fqn.mkString("::")}"
}
