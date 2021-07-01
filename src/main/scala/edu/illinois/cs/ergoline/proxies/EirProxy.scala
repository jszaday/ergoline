package edu.illinois.cs.ergoline.proxies

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.literals.EirLiteral
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.resolution.{EirPlaceholder, EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{
  RichOption,
  RichResolvableTypeIterable
}
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.{globals, util}

case class EirProxy(
    var parent: Option[EirNode],
    var base: EirClassLike,
    var collective: Option[String],
    var kind: Option[EirProxyKind]
) extends EirClassLike {

  isAbstract = base.isAbstract

  def isElement: Boolean = kind.contains(EirElementProxy)
  def isSection: Boolean = kind.contains(EirSectionProxy)

  def predicate: Option[EirExpressionNode] = base.predicate
  override def predicate_=(expr: Option[EirExpressionNode]): Unit = ???

  override def selfDeclarations: List[EirMember] =
    base.selfDeclarations ++ {
      Option
        .when(collective.nonEmpty && isElement)({
          EirClassLike.makeSelfDeclaration(
            Some(this),
            "self[@]",
            ProxyManager.elementType(this)
          )
        })
        .toList
    } :+ {
      EirClassLike.makeSelfDeclaration(
        Some(this),
        "self@",
        collective
          .map(_ => ProxyManager.collectiveType(this))
          .getOrElse(ProxyManager.proxyType(this))
      )
    } :+ {
      val decl = EirClassLike.makeSelfDeclaration(
        Some(this),
        globals.implicitProxyName,
        globals.proxyType
      )
      decl.member.asInstanceOf[EirDeclaration].isImplicit = true
      decl
    }

  def isMain: Boolean =
    base.annotations.exists(_.name == "main") && collective.isEmpty

  override def derived: Set[EirClassLike] =
    base.derived.flatMap(ProxyManager.proxiesFor).toSet

  def namespaces: Seq[EirNamespace] =
    Find
      .ancestors(base)
      .collect({
        case n: EirNamespace => n
      })

  var internalMembers: List[EirMember] = Nil

  def mkValueContribute(): EirMember = {
    val m = EirMember(Some(this), null, EirAccessibility.Public)
    m.annotations +:= EirAnnotation("system", Map())
    val f =
      EirFunction(Some(m), None, "contribute", Nil, Nil, Nil, globals.unitType)
    val (from, to) =
      (EirTemplateArgument(Some(f), "From"), EirTemplateArgument(Some(f), "To"))
    val (fromExp, toExp) =
      (EirPlaceholder(None, Some(from)), EirPlaceholder(None, Some(to)))
    f.templateArgs = List(from, to)
    f.functionArgs = List(
      EirFunctionArgument(
        Some(f),
        "value",
        fromExp,
        isExpansion = false,
        isSelfAssigning = false
      ),
      EirFunctionArgument(
        Some(f),
        "reducer",
        EirLambdaType(None, List(toExp, fromExp), toExp),
        isExpansion = false,
        isSelfAssigning = false
      ),
      EirFunctionArgument(
        Some(f),
        "callback",
        EirLambdaType(None, List(toExp), globals.unitType),
        isExpansion = false,
        isSelfAssigning = false
      )
    )
    m.member = f
    m
  }

  def mkUnitContribute(): EirMember = {
    util.makeMemberFunctionWithArgs(
      this,
      "contribute",
      List(
        EirFunctionArgument(
          None,
          "callback",
          EirLambdaType(None, Nil, globals.unitType),
          isExpansion = false,
          isSelfAssigning = false
        )
      ),
      globals.unitType
    )
  }

  // replace "self" with "selfProxy"
  // TODO use clone
  private def updateMember(m: EirMember): EirMember = {
    val theirs = m.member.asInstanceOf[EirFunction]
    val isAsync = m.annotation("async").isDefined
    val newMember = EirMember(Some(this), null, m.accessibility)
    val ours = EirFunction(
      Some(newMember),
      None,
      theirs.name,
      theirs.templateArgs,
      null,
      null,
      null
    )
    ours.returnType = if (isAsync) {
      EirTemplatedType(Some(ours), globals.futureType, List(theirs.returnType))
    } else theirs.returnType
//    val declType = ProxyManager.elementFor(this).getOrElse(this)
    newMember.counterpart = Some(m)
    newMember.annotations =
      m.annotations ++ Option.when(isSection)(EirAnnotation("system", Map()))
    newMember.member = ours
    ours.functionArgs = theirs.functionArgs.map(_.cloneWith(Some(ours)))
    ours.implicitArgs = theirs.implicitArgs.map(_.cloneWith(Some(ours)))
    newMember
  }

  private def validMember(m: EirMember): Boolean = {
    m.isEntry // && can be used with this type
  }

  private def indices: Option[List[EirType]] = {
    val n = collective.map(ProxyManager.dimensionality)
    val i = globals.integerType
    n.map(List.fill(_)(i))
  }

  def indexType: Option[EirType] = {
    indices.map(_.toTupleType()(Some(this))).to[EirType]
  }

  private def genContributors(): List[EirMember] = {
    List(mkUnitContribute(), mkValueContribute())
  }

  private def genSectionMembers(): List[EirMember] = genContributors()

  private def genElementMembers(): List[EirMember] = {
    val parent = ProxyManager.collectiveFor(this).get
    val idx = indexType.get

    genContributors() ++ List(
      util.makeMemberFunction(this, "parent", Nil, parent),
      util.makeMemberFunction(this, "index", Nil, idx)
    )
  }

  private def genCollectiveMembers(): List[EirMember] = {
    val idx: List[EirType] = indices.get

    val idxTy = idx.toTupleType()(None)
    val eleTy = ProxyManager.elementType(this)
    val secTy = ProxyManager.sectionType(this)
    val unitTy = globals.unitType
    val arrayTy = // TODO iterable could be used here instead?
      EirTemplatedType(None, globals.arrayType, List(idxTy))
    val rangeTys =
      idx.map(x => EirTemplatedType(None, globals.rangeType, List(x)))

    val needsIndex = collective.exists(_.startsWith("array"))
    base.members
      .filter(x => validMember(x) && x.isConstructor)
      .map(m => {
        // ckNew(n, ...); constructor with size + args
        val args =
          m.member.asInstanceOf[EirFunction].functionArgs.map(_.declaredType)
        util.makeMemberFunction(
          this,
          baseName,
          (if (needsIndex) idx else Nil) ++ args,
          unitTy
        )
      }) ++ {
      // ckNew(); <-- empty constructor
      if (needsIndex) List(util.makeMemberFunction(this, baseName, Nil, unitTy))
      else Nil
    } :+ util.makeMemberFunction(this, "get", idx, eleTy) :+
      util.makeMemberFunction(this, "get", List(arrayTy), secTy) :+
      util.makeMemberFunction(this, "get", rangeTys, secTy)
  }

  override def members: List[EirMember] = {
    if (internalMembers.isEmpty) {
      val fromKind =
        (kind, collective) match {
          case (Some(EirElementProxy), _) => genElementMembers()
          case (Some(EirSectionProxy), _) => genSectionMembers()
          case (_, Some(_))               => genCollectiveMembers()
          case _                          => Nil
        }

      internalMembers = fromKind ++ base.members
        .filter(x => validMember(x) && (singleton || !x.isConstructor))
        .map(updateMember)
    }

    internalMembers
  }

  def ordinalFor(member: EirMember): Option[Int] =
    member.counterpart.flatMap(_.ordinal)

  def singleton: Boolean = isElement || collective.isEmpty

  def membersToGen: List[EirMember] = {
    members.filterNot(_.annotation("system").isDefined) ++ Option
      .when(!singleton)({
        base.members
          .filter(validMember)
          .filter(_.isConstructor)
          .map(updateMember)
      })
      .getOrElse(Nil)
  }

  override def members_=(lst: List[EirMember]): Unit =
    Errors.exit("cannot assign to members of proxy")

  override var extendsThis: Option[EirResolvable[types.EirType]] = None
  override var implementsThese: List[EirResolvable[types.EirType]] = Nil
  override var templateArgs: List[EirTemplateArgument] = base.templateArgs

//  override def templateArgs: List[EirTemplateArgument] = Nil
//  override def templateArgs_=(x: List[EirTemplateArgument]): Unit = ()

  // TODO this may have to be changed eventually
  //     (if it's inaccessible it's not a problem)
  override def name: String = base.name

  def baseName: String =
    base.name + collective.map("_" + _ + "_").getOrElse("_")
}
