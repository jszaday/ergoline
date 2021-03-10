package edu.illinois.cs.ergoline.proxies

import edu.illinois.cs.ergoline.ast.EirClassLike
import edu.illinois.cs.ergoline.ast.types.{EirProxyType, EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.{Errors, assertValid}

import scala.util.matching.Regex

object ProxyManager {
  val arrayPtn: Regex = raw"array(\d+)d".r

  def dimensionality(s: String): Int = {
    s match {
      case arrayPtn(dim) => dim.toInt
      case "nodegroup" | "group" => 1
      case _ => Errors.unreachable()
    }
  }

  private var _proxies: Map[(String, EirClassLike), EirProxy] = Map()
  private var _elements: Map[EirProxy, EirProxy] = Map()
  private var _types: Map[(EirProxy, List[EirType]), EirType] = Map()

  def asProxy(t: EirType): Option[EirProxy] = {
    t match {
      case EirTemplatedType(_, p: EirProxy, _) => Some(p)
      case p: EirProxy => Some(p)
      case _ => None
    }
  }

  def proxies: Iterable[EirProxy] = _proxies.values

  def checkProxyable(base: EirClassLike, collective: Option[String], isElement: Boolean): EirProxy = {
    // validate base has an entry constructor of the appropriate nature
    EirProxy(Some(base), base, collective, isElement)
  }

  def elementFor(t: EirProxy): Option[EirProxy] = {
    Option.when(t.isElement)(t).orElse(t.collective.map(_ => {
      if (!_elements.contains(t)) {
        _elements += (t -> EirProxy(t.parent, t.base, t.collective, isElement = true))
      }
      _elements(t)
    }))
  }

  def collectiveFor(t: EirProxy): Option[EirProxy] =
    Option.when(t.isElement)(_proxies.get((t.collective.get, t.base))).flatten

  private def typeFor(p: EirProxy, args: List[EirType]): EirType = {
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
    typeFor(p, p.templateArgs.map(Find.uniqueResolution[EirType](_)))
  }

  def elementType(p: EirProxy): EirType = {
    elementFor(p) match {
      case Some(e) => typeFor(e, e.templateArgs.map(Find.uniqueResolution[EirType](_)))
      case None => Errors.missingType(p)
    }
  }

  def collectiveType(p: EirProxy): EirType = {
    collectiveFor(p) match {
      case Some(e) => typeFor(e, e.templateArgs.map(Find.uniqueResolution[EirType](_)))
      case None => Errors.missingType(p)
    }
  }

  def proxyFor(t: EirProxyType): EirType = {
    val collective = t.collective.getOrElse("")
    val resolved = Find.uniqueResolution[EirType](t.base)
    val base = resolved match {
      case t: EirTemplatedType =>
        Find.uniqueResolution[EirClassLike](t.base)
      case c: EirClassLike => c
      case _ => Errors.unableToResolve(t)
    }
    val templateArgs = resolved match {
      case t: EirTemplatedType => t.args.map(Find.uniqueResolution[EirType])
      case _ if base.templateArgs.isEmpty => Nil
      case _ => Errors.missingSpecialization(base)
    }
    val baseProxy = _proxies.get((collective, base)) match {
      case Some(p) => p
      case _ =>
        val proxy = checkProxyable(base, t.collective, isElement = false)
        _proxies += ((collective, base) -> proxy)
        proxy
    }
    typeFor(Option.when(t.isElement)(elementFor(baseProxy)).flatten.getOrElse(baseProxy), templateArgs)
  }

  def proxiesFor(c: EirClassLike): Iterable[EirProxy] = {
    _proxies.collect({
      case ((_, o), v) if o == c => v
    })
  }
}
