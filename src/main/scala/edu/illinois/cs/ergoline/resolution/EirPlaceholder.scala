package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.EirNode

case class EirPlaceholder[T <: EirNode](var parent: Option[EirNode], var expectation: Option[T] = None) extends EirResolvable[T] {
  def fulfill(t: T): Boolean = {
    expectation match {
      case None => parent.map(_.replaceChild(this, t)).getOrElse(false)
      case _ => true
    }
  }

  override def resolve(): Seq[T] = expectation.toSeq
  override def resolved: Boolean = expectation.isDefined
  override def children: Iterable[EirNode] = Nil
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}
