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
      case _ => throw new RuntimeException(s"unsure how to match $s")
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
    Option.when(t.collective.isDefined && !t.isElement)({
      if (!_elements.contains(t)) {
        _elements += (t -> EirProxy(t.parent, t.base, t.collective, isElement = true))
      }
      _elements(t)
    })
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

  def elementType(p: EirProxy): EirType = {
    elementFor(p) match {
      case Some(e) => typeFor(e, e.templateArgs.map(Find.uniqueResolution(_)))
      case None => ???
    }
  }

  def proxyFor(t: EirProxyType): EirType = {
    val ctve = t.collective.getOrElse("")
    val resolved = Find.uniqueResolution(t.base)
    val base = resolved match {
      case t: EirTemplatedType =>
        assertValid[EirClassLike](Find.uniqueResolution(t.base))
      case c: EirClassLike => c
      case _ => Errors.unableToResolve(t)
    }
    val templateArgs = resolved match {
      case t: EirTemplatedType => t.args.map(Find.uniqueResolution[EirType])
      case _ if base.templateArgs.isEmpty => Nil
      case _ => Errors.missingSpecialization(base)
    }
    val baseProxy = _proxies.get((ctve, base)) match {
      case Some(p) => p
      case _ =>
        val proxy = checkProxyable(base, t.collective, isElement = false)
        _proxies += ((ctve, base) -> proxy)
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
