package edu.illinois.cs.ergoline.proxies

import edu.illinois.cs.ergoline.ast.EirClassLike
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.{CheckTypes, TypeCheckContext}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.Errors

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

  private var _proxies: Map[(String, EirClassLike), EirProxy] = Map()
  private var _elements: Map[EirProxy, EirProxy] = Map()
  private var _sections: Map[EirProxy, EirProxy] = Map()
  private var _types: Map[(EirProxy, List[EirResolvable[EirType]]), EirType] =
    Map()

  def registeredIndices(): Set[EirType] = {
    (globals.integerType +: _proxies.values
      .flatMap(_.indexType)
      .toList).toSet
  }

  def asProxy(t: EirType): Option[EirProxy] = {
    t match {
      case EirTemplatedType(_, p: EirProxy, _) => Some(p)
      case p: EirProxy                         => Some(p)
      case _                                   => None
    }
  }

  def hasMain: Boolean = _proxies.exists(_._2.isMain)

  def proxies: Iterable[EirProxy] = _proxies.values

  def singletons: Iterable[EirProxy] = proxies.filter(_.collective.isEmpty)

  def checkProxyable(
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

  def elementFor(t: EirProxy): Option[EirProxy] = {
    findCorrespondence(
      t,
      EirElementProxy,
      () => {
        if (!_elements.contains(t)) {
          _elements += (t -> copyWithKind(t, EirElementProxy))
        }

        _elements(t)
      }
    )
  }

  def sectionFor(t: EirProxy): Option[EirProxy] = {
    findCorrespondence(
      t,
      EirSectionProxy,
      () => {
        if (!_sections.contains(t)) {
          _sections += (t -> copyWithKind(t, EirSectionProxy))
        }

        _sections(t)
      }
    )
  }

  def collectiveFor(t: EirProxy): Option[EirProxy] =
    _proxies
      .get((t.collective.get, t.base))
      .filter(_ => t.isElement || t.isSection)

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

  def proxyFor(t: EirProxyType)(implicit ctx: TypeCheckContext): EirType = {
    val baseTy = CheckTypes.visit(t.base)
    val baseCls = Find.asClassLike(baseTy)
    val templateArgs = baseTy match {
      // TODO the args map is probably not necessary here?
      case t: EirTemplatedType               => t.args.map(CheckTypes.visit)
      case _ if baseCls.templateArgs.isEmpty => Nil
      case _                                 => Errors.missingSpecialization(baseCls)
    }

    val collective = t.collective.getOrElse("")
    val baseProxy = _proxies.getOrElse(
      (collective, baseCls), {
        val proxy = checkProxyable(baseCls, t.collective, t.kind)
        _proxies += ((collective, baseCls) -> proxy)
        proxy
      }
    )

    val proxy = Option
      .unless(t.isElement)(baseProxy)
      .orElse(elementFor(baseProxy))

    typeFor(
      proxy.getOrElse(Errors.missingType(t)),
      templateArgs
    )
  }

  def proxiesFor(x: EirClassLike): Iterable[EirProxy] = {
    _proxies collect {
      case ((_, y), p) if x == y => p
    }
  }
}
