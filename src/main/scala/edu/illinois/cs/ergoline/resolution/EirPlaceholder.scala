package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.EirNode


case class EirPlaceholder[T <: EirNode](var parent: Option[EirNode]) extends EirResolvable[T] {
  def fulfill(t: T): Boolean = {
    parent.map(_.replaceChild(this, t)).getOrElse(false)
  }

  override def resolve(): Seq[T] = Nil
  override def resolved: Boolean = false
  override def children: Iterable[EirNode] = Nil
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}
