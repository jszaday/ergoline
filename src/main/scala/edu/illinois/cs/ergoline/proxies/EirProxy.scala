package edu.illinois.cs.ergoline.proxies

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}

case class EirProxy(var parent: Option[EirNode], var base: EirClassLike, var collective: Option[String], var isElement: Boolean) extends EirClassLike {

  isAbstract = base.isAbstract

  def isMain: Boolean =
    base.annotations.exists(_.name == "main") && collective.isEmpty

  override def derived: List[EirClassLike] =
    base.derived.flatMap(ProxyManager.proxiesFor)

  def namespaces: Seq[EirNamespace] = Find.ancestors(base).collect({
    case n : EirNamespace => n
  })

  // replace "self" with "selfProxy"
  // TODO use clone
  private def correctSelf(m : EirMember): EirMember = {
    val newMember = EirMember(Some(this), null, m.accessibility)
    val theirs = m.member.asInstanceOf[EirFunction]
    val ours = EirFunction(Some(newMember), None, theirs.name, theirs.templateArgs, null, theirs.returnType)
    val declType = ProxyManager.elementFor(this).getOrElse(this)
    newMember.annotations = m.annotations
    newMember.member = ours
    ours.functionArgs =
      EirFunctionArgument(Some(ours), "self", declType, isFinal = false, isSelfAssigning = false) +: theirs.functionArgs.tail
    newMember
  }

  private def validConstructor(m : EirMember): Boolean = {
    val args = m.member.asInstanceOf[EirFunction].functionArgs
    val checkArg = Option.when(args.length >= 2)(args(1))
    val checkDeclTy = checkArg.map(x => Find.uniqueResolution(x.declaredType))
    val element = ProxyManager.elementFor(this).getOrElse(this)
    checkDeclTy.contains(element)
  }

  override def members: List[EirMember] = {
    base.members
      .filter(x => x.isEntry && (!x.isConstructor || validConstructor(x)))
      .map(correctSelf)
  }

  override def members_=(lst: List[EirMember]): Unit = ???

  override var extendsThis: Option[EirResolvable[types.EirType]] = None
  override var implementsThese: List[EirResolvable[types.EirType]] = Nil
  override var templateArgs: List[EirTemplateArgument] = base.templateArgs

  override def isDescendantOf(other: EirClassLike): Boolean = {
    other match {
      case proxy: EirProxy =>
        (proxy.isElement == this.isElement) &&
          (proxy.collective == this.collective) &&
          base.isDescendantOf(proxy.base)
      case _ => false
    }
  }

  // TODO this may have to be changed eventually
  //     (if it's inaccessible it's not a problem)
  override def name: String = base.name

  def baseName: String = base.name + collective.map("_" + _ + "_").getOrElse("_")
}
