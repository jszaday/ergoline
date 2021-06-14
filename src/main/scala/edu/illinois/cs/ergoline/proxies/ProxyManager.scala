package edu.illinois.cs.ergoline.proxies

import edu.illinois.cs.ergoline.ast.EirClassLike
import edu.illinois.cs.ergoline.ast.types.{EirProxyType, EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.passes.{CheckTypes, TypeCheckContext}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
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
  private var _types: Map[(EirProxy, List[EirResolvable[EirType]]), EirType] = Map()

  def asProxy(t: EirType): Option[EirProxy] = {
    t match {
      case EirTemplatedType(_, p: EirProxy, _) => Some(p)
      case p: EirProxy => Some(p)
      case _ => None
    }
  }

  def hasMain: Boolean = _proxies.exists(_._2.isMain)

  def proxies: Iterable[EirProxy] = _proxies.values

  def singletons: Iterable[EirProxy] = proxies.filter(_.collective.isEmpty)

  def checkProxyable(base: EirClassLike, collective: Option[String], isElement: Boolean): EirProxy = {
    // validate base has an entry constructor of the appropriate nature
    EirProxy(base.parent, base, collective, isElement)
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
    _proxies.get((t.collective.get, t.base)).filter(_ => t.isElement)

  private def typeFor(p: EirProxy, args: List[EirResolvable[EirType]]): EirType = {
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
      case None => Errors.missingType(p)
    }
  }

  def collectiveType(p: EirProxy): EirType = {
    collectiveFor(p) match {
      case Some(e) => typeFor(e, e.templateArgs)
      case None => Errors.missingType(p)
    }
  }

  def proxyFor(t: EirProxyType)(implicit ctx: TypeCheckContext): EirType = {
    val baseTy = CheckTypes.visit(t.base)
    val baseCls = Find.asClassLike(baseTy)
    val templateArgs = baseTy match {
      // TODO the args map is probably not necessary here?
      case t: EirTemplatedType => t.args.map(CheckTypes.visit)
      case _ if baseCls.templateArgs.isEmpty => Nil
      case _ => Errors.missingSpecialization(baseCls)
    }

    val collective = t.collective.getOrElse("")
    val baseProxy = _proxies.getOrElse((collective, baseCls), {
      val proxy = checkProxyable(baseCls, t.collective, isElement = false)
      _proxies += ((collective, baseCls) -> proxy)
      proxy
    })

    val proxy = Option
      .unless(t.isElement)(baseProxy)
      .orElse(elementFor(baseProxy))

    typeFor (
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
