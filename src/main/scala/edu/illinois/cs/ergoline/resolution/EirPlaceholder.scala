package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.{
  EirEncloseExempt,
  EirExpressionNode,
  EirNode
}
import edu.illinois.cs.ergoline.util.Errors

case class EirPlaceholder[T <: EirNode](
    var parent: Option[EirNode],
    var expectation: Option[T] = None
) extends EirExpressionNode
    with EirResolvable[T]
    with EirEncloseExempt {
  var isStatic = true

  override def disambiguation: Option[EirNode] = expectation
  override def disambiguation_=(x: Option[EirNode]): Unit = ???

  override def resolve(): Seq[T] = expectation.toSeq
  override def resolved: Boolean = expectation.isDefined

  override def children: Iterable[EirNode] = Nil

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean =
    Errors.unreachable()
}
