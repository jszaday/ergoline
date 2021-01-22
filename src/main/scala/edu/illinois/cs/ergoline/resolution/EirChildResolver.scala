package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.EirNode

case class EirChildResolver[A <: EirNode, B <: EirNode](var target: EirResolvable[A], var member: EirResolvable[B])(var parent: Option[EirNode]) extends EirResolvable[B] {
  override def resolve(): Seq[B] = ???

  override def resolved: Boolean = member.resolved

  override def children: Iterable[EirNode] = ???

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}
