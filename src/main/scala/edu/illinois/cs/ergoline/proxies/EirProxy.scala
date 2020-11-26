package edu.illinois.cs.ergoline.proxies

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.EirResolvable

case class EirProxy(var parent: Option[EirNode], var base: EirClassLike, var collective: Option[String]) extends EirClassLike {

  // replace "self" with "selfProxy"
  // TODO use clone
  private def correctSelf(m : EirMember): EirMember = {
    val newMember = EirMember(Some(this), null, m.accessibility)
    val theirs = m.member.asInstanceOf[EirFunction]
    val ours = EirFunction(Some(newMember), None, theirs.name, theirs.templateArgs, null, theirs.returnType)
    newMember.annotations = m.annotations
    newMember.member = ours
    ours.functionArgs =
      EirFunctionArgument(Some(ours), "self", this, isFinal = true, isSelfAssigning = false) +: theirs.functionArgs.tail
    newMember
  }

  override def members: List[EirMember] = {
    base.members.collect({
      case m if m.isEntry => m
    }).map(correctSelf)
  }

  override def members_=(lst: List[EirMember]): Unit = ???

  override var extendsThis: Option[EirResolvable[types.EirType]] = None
  override var implementsThese: List[EirResolvable[types.EirType]] = Nil
  override var templateArgs: List[EirTemplateArgument] = base.templateArgs

  override def isDescendantOf(other: EirClassLike): Boolean = {
    other match {
      case proxy: EirProxy => base.isDescendantOf(proxy.base)
      case _ => false
    }
  }

  override def name: String = ???
}
