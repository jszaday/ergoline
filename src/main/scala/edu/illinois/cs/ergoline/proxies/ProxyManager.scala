package edu.illinois.cs.ergoline.proxies

import edu.illinois.cs.ergoline.ast.EirClassLike
import edu.illinois.cs.ergoline.ast.types.EirProxyType
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.assertValid

object ProxyManager {
  private var _proxies: Map[(String, EirClassLike), EirProxy] = Map()
  private var _elements: Map[EirProxy, EirProxy] = Map()

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

  def proxyFor(t: EirProxyType): EirProxy = {
    val ctve = t.collective.getOrElse("")
    val base = assertValid[EirClassLike](Find.uniqueResolution(t.base))
    val baseProxy = _proxies.get((ctve, base)) match {
      case Some(p) => p
      case _ =>
        val proxy = checkProxyable(base, t.collective, isElement = false)
        _proxies += ((ctve, base) -> proxy)
        proxy
    }
    Option.when(t.isElement)(elementFor(baseProxy))
      .flatten.getOrElse(baseProxy)
  }

  def proxiesFor(c: EirClassLike): Iterable[EirProxy] = {
    _proxies.collect({
      case ((_, o), v) if o == c => v
    })
  }
}
