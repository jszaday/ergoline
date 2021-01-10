package edu.illinois.cs.ergoline.proxies

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.{globals, util}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichOption, RichResolvableTypeIterable}

case class EirProxy(var parent: Option[EirNode], var base: EirClassLike, var collective: Option[String], var isElement: Boolean) extends EirClassLike {

  isAbstract = base.isAbstract

  override def selfDeclarations: List[EirMember] = base.selfDeclarations ++ {
    Option.when(collective.nonEmpty && isElement)({
      EirClassLike.makeSelfDeclaration(Some(this), "self[@]", ProxyManager.elementType(this))
    }).toList
  } :+ {
    EirClassLike.makeSelfDeclaration(Some(this), "self@",
      collective.map(_ => ProxyManager.collectiveType(this)).getOrElse(ProxyManager.proxyType(this)))
  }

  def isMain: Boolean =
    base.annotations.exists(_.name == "main") && collective.isEmpty

  override def derived: Set[EirClassLike] =
    base.derived.flatMap(ProxyManager.proxiesFor).toSet

  def namespaces: Seq[EirNamespace] = Find.ancestors(base).collect({
    case n : EirNamespace => n
  })

  // replace "self" with "selfProxy"
  // TODO use clone
  private def updateMember(m : EirMember): EirMember = {
    val theirs = m.member.asInstanceOf[EirFunction]
    val isAsync = m.annotation("async").isDefined
    val newMember = EirMember(Some(this), null, m.accessibility)
    val ours = EirFunction(Some(newMember), None, theirs.name, theirs.templateArgs, null, null)
    ours.returnType = if (isAsync) {
      EirTemplatedType(Some(ours), globals.futureType, List(theirs.returnType))
    } else theirs.returnType
//    val declType = ProxyManager.elementFor(this).getOrElse(this)
    newMember.counterpart = Some(m)
    newMember.annotations = m.annotations
    newMember.member = ours
    ours.functionArgs = theirs.functionArgs.map(_.cloneWith(Some(ours)))
    newMember
  }

  private def validMember(m : EirMember): Boolean = {
    m.isEntry // && can be used with this type
  }

  private def indices: Option[List[EirType]] = {
    val n = collective.map(ProxyManager.dimensionality)
    val i = globals.typeFor(EirLiteralTypes.Integer)
    n.map(List.fill(_)(i))
  }

  private def indexType: Option[EirType] = {
    indices.map(_.toTupleType(Some(this))).to[EirType]
  }

  private def genElementMembers(): List[EirMember] = {
    val idx = indexType.get
    val parent = ProxyManager.collectiveFor(this).get
    List(util.makeMemberFunction(this, "parent", Nil, parent, isConst = true),
      util.makeMemberFunction(this, "index", Nil, idx, isConst = true))
  }

  private def genCollectiveMembers(): List[EirMember] = {
    val idx: List[EirType] = indices.get
    val u = globals.typeFor(EirLiteralTypes.Unit)
    val eleTy = ProxyManager.elementType(this)
    val needsIndex = collective.exists(_.startsWith("array"))
    base.members
      .filter(x => validMember(x) && x.isConstructor)
      .map(m => {
        // ckNew(n, ...); constructor with size + args
        val args = m.member.asInstanceOf[EirFunction].functionArgs.map(_.declaredType)
        util.makeMemberFunction(this, baseName, (if (needsIndex) idx else Nil) ++ args, u, isConst = false)
      }) ++ {
        // ckNew(); <-- empty constructor
        if (needsIndex) List(util.makeMemberFunction(this, baseName, Nil, u, isConst = false))
        else Nil
      } :+ util.makeMemberFunction(this, "get", idx, eleTy, isConst = true)
  }

  override def members: List[EirMember] = {
    val fromKind =
      if (isElement) genElementMembers()
      else if (collective.isDefined) genCollectiveMembers()
      else Nil
    fromKind ++ base.members
      .filter(x => validMember(x) && (singleton || !x.isConstructor))
      .map(updateMember)
  }

  def singleton: Boolean = isElement || collective.isEmpty

  def membersToGen: List[EirMember] = {
    members.filterNot(_.annotation("system").isDefined) ++ Option.when(!singleton)({
      base.members
        .filter(validMember)
        .filter(_.isConstructor)
        .map(updateMember)
    }).getOrElse(Nil)
  }

  override def members_=(lst: List[EirMember]): Unit = ???

  override var extendsThis: Option[EirResolvable[types.EirType]] = None
  override var implementsThese: List[EirResolvable[types.EirType]] = Nil
  override var templateArgs: List[EirTemplateArgument] = base.templateArgs

//  override def templateArgs: List[EirTemplateArgument] = Nil
//  override def templateArgs_=(x: List[EirTemplateArgument]): Unit = ()

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
