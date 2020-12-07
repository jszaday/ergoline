package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.{EirEncloseExempt, EirNode}

object EirPlaceholder {
  def apply[T <: EirNode](): EirPlaceholder[T] = {
    new EirPlaceholder()
  }
}

case class EirPlaceholder[+T <: EirNode]() extends EirResolvable[T] with EirEncloseExempt {
  override def resolve(): Seq[T] = Nil
  override def resolved: Boolean = false
  override var parent: Option[EirNode] = None
  override def children: Iterable[EirNode] = Nil
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}
