package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast.{EirNode, EirTemplateArgument}

case class EirTemplateFacade(t: EirTemplateArgument) extends EirType {
  override var parent: Option[EirNode] = None

  override def children: Iterable[EirNode] = Nil

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???

  override def toString: String = t.toString
}
