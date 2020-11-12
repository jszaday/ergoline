package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.EirScope

class EirResolvableName[T: Manifest](var fqn: List[String]) extends EirResolvable[T] {
  def resolve(scope: EirScope): T = Find.byName[T](fqn, scope) match {
    case Some(t: T) => t
    case None => throw new RuntimeException("could not resolve" + fqn)
  }

  override def equals(obj: Any): Boolean = obj match {
    case x: EirResolvableName[_] => x.fqn == this.fqn
    case _ => false
  }

  override def toString: String = s"Pending(${fqn.mkString("::")})"
}
