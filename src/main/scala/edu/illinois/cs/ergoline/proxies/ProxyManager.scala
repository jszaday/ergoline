package edu.illinois.cs.ergoline.proxies

import edu.illinois.cs.ergoline.ast.EirClassLike
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.{CheckTypes, TypeCheckContext}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.Errors

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.matching.Regex

object ProxyManager {
  val arrayPtn: Regex = raw"array(\d+)d".r

  def dimensionality(s: String): Int = {
    s match {
      case arrayPtn(dim)         => dim.toInt
      case "nodegroup" | "group" => 1
      case _                     => Errors.unreachable()
    }
  }

  type KindMap = mutable.HashMap[Option[EirProxyKind], EirProxy]
  type ProxyType = (EirClassLike, Option[String])

  private var _proxies: Map[ProxyType, KindMap] = Map()
  private var _types: Map[(EirProxy, List[EirResolvable[EirType]]), EirType] =
    Map()

  def proxies: Iterable[EirProxy] =
    _proxies.values.flatMap(_.values)

  def registeredIndices(): Set[EirType] = {
    proxies.flatMap(_.indexType).toSet + globals.integerType
  }

  def asProxy(t: EirType): Option[EirProxy] = {
    t match {
      case EirTemplatedType(_, p: EirProxy, _) => Some(p)
      case p: EirProxy                         => Some(p)
      case _                                   => None
    }
  }

  def hasMain: Boolean = proxies.exists(_.isMain)

  def singletons: Iterable[EirProxy] = proxies.filter(_.collective.isEmpty)

  private def checkProxyable(
      base: EirClassLike,
      collective: Option[String],
      kind: Option[EirProxyKind]
  )(implicit ctx: TypeCheckContext): EirProxy = {
    CheckProxy(EirProxy(base.parent, base, collective, kind))
  }

  private def findCorrespondence(
      t: EirProxy,
      k: EirProxyKind,
      getter: () => EirProxy
  ): Option[EirProxy] = {
    Option.when(t.kind.contains(k))(t) orElse t.collective.map(_ => getter())
  }

  private def copyWithKind(t: EirProxy, k: EirProxyKind): EirProxy = {
    EirProxy(
      t.parent,
      t.base,
      t.collective,
      Some(k)
    )
  }

  def kindMapOf(t: EirProxy): KindMap = {
    _proxies.getOrElse((t.base, t.collective), ???)
  }

  def kindFor(t: EirProxy, kind: EirProxyKind): Option[EirProxy] = {
    findCorrespondence(
      t,
      kind,
      () => {
        val (map, key) = (kindMapOf(t), Some(kind))

        if (!map.contains(key)) {
          map.put(key, copyWithKind(t, kind))
        }

        map(key)
      }
    )
  }

  def shouldGenerate(x: EirProxy): Boolean = {
    x.collective.forall(_ => x.isCollective) && !x.base.isAbstract
  }

  def collectiveFor(t: EirProxy): Option[EirProxy] =
    kindFor(t, EirCollectiveProxy)

  def elementFor(t: EirProxy): Option[EirProxy] = kindFor(t, EirElementProxy)

  def sectionFor(t: EirProxy): Option[EirProxy] = kindFor(t, EirSectionProxy)

  private def typeFor(
      p: EirProxy,
      args: List[EirResolvable[EirType]]
  ): EirType = {
    if (args.isEmpty) p
    else if (_types.contains((p, args))) _types((p, args))
    else {
      val ty = EirTemplatedType(None, p, args)
      _types += ((p, args) -> ty)
      ty
    }
  }

  def proxyType(p: EirProxy): EirType = {
    assert(p.collective.isEmpty)
    typeFor(p, p.templateArgs)
  }

  def elementType(p: EirProxy): EirType = {
    elementFor(p) match {
      case Some(e) => typeFor(e, e.templateArgs)
      case None    => Errors.missingType(p)
    }
  }

  def collectiveType(p: EirProxy): EirType = {
    collectiveFor(p) match {
      case Some(e) => typeFor(e, e.templateArgs)
      case None    => Errors.missingType(p)
    }
  }

  def sectionType(p: EirProxy): EirType = {
    sectionFor(p) match {
      case Some(e) => typeFor(e, e.templateArgs)
      case None    => Errors.missingType(p)
    }
  }

  private def getBaseProxy(base: EirClassLike, collective: Option[String])(
      implicit ctx: TypeCheckContext
  ): EirProxy = {
    val kind = collective.map(_ => EirCollectiveProxy)
    _proxies.getOrElse(
      (base, collective), {
        val map = new KindMap()
        _proxies += ((base, collective) -> map)
        map.put(kind, checkProxyable(base, collective, kind))
        map
      }
    )(kind)
  }

  def proxyFor(t: EirProxyType)(implicit ctx: TypeCheckContext): EirType = {
    val baseTy = CheckTypes.visit(t.base)
    val baseCls = Find.asClassLike(baseTy)
    val templateArgs = baseTy match {
      // TODO the args map is probably not necessary here?
      case t: EirTemplatedType               => t.args.map(CheckTypes.visit)
      case _ if baseCls.templateArgs.isEmpty => Nil
      case _                                 => Errors.missingSpecialization(baseCls)
    }

    val baseProxy = getBaseProxy(baseCls, t.collective)
    val proxy =
      if (baseProxy.kind == t.kind) {
        Some(baseProxy)
      } else {
        t.kind.flatMap(kindFor(baseProxy, _))
      }

    typeFor(
      proxy.getOrElse(Errors.missingType(t)),
      templateArgs
    )
  }

  def proxiesFor(x: EirClassLike): Iterable[EirProxy] = {
    (_proxies collect {
      case ((y, _), p) if x.eq(y) => p.values.filterNot(_.isSection)
    }).flatten
  }
}
