package edu.illinois.cs.ergoline.proxies

import edu.illinois.cs.ergoline.ast.EirClassLike
import edu.illinois.cs.ergoline.ast.types.EirProxyType
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.assertValid

object ProxyManager {
  private var _proxies: Map[EirClassLike, EirProxy] = Map()

  def proxies: Iterable[EirProxy] = _proxies.values

  def checkProxyable(base: EirClassLike, collective: Option[String]): EirProxy = {
    // validate base has an entry constructor of the appropriate nature
    EirProxy(Some(base), base, collective)
  }

  def proxyFor(t: EirProxyType): EirProxy = {
    val base = assertValid[EirClassLike](Find.uniqueResolution(t.base))
    _proxies.get(base) match {
      case Some(p) => p
      case _ =>
        val proxy = checkProxyable(base, t.collective)
        _proxies += (base -> proxy)
        proxy
    }
  }
}
