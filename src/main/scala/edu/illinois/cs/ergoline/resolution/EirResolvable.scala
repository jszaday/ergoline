package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.EirScope

trait EirResolvable[T] {
  def resolve(scope: EirScope): T
}

object EirResolvable {
  def fromName[T : Manifest](fqn : Iterable[String]) = new EirResolvableName[T](fqn.toList)
}
