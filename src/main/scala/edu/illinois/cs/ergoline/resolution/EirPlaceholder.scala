package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.{EirEncloseExempt, EirNode}
import edu.illinois.cs.ergoline.util.Errors

case class EirPlaceholder[T <: EirNode](
    var parent: Option[EirNode],
    var expectation: Option[T] = None
) extends EirResolvable[T]
    with EirEncloseExempt {
  override def resolve(): Seq[T] = expectation.toSeq
  override def resolved: Boolean = expectation.isDefined
  override def children: Iterable[EirNode] = Nil
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean =
    Errors.unreachable()
}
