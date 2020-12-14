package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.passes.UnparseAst.UnparseContext
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.proxies.ProxyManager.arrayPtn
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.assertValid

import scala.util.Properties.{lineSeparator => n}

object GenerateCi {

  class CiUnparseContext(val checked: Map[EirSpecializable, List[EirSpecialization]]) extends CodeGenerationContext("ci") {
    val puppables: Iterable[EirSpecializable] = checked.keys.filter({
      case x: EirClassLike => !(x.annotation("system").isDefined || x.isAbstract)
      case _ => false
    })
  }

  def visitAll(checked: Map[EirSpecializable, List[EirSpecialization]]): String = {
    val ctx = new CiUnparseContext(checked)
    ctx << "mainmodule generate" << "{"
    ProxyManager.proxies
      .filterNot(x => x.base.isAbstract || x.isElement)
      .map(x => (x.namespaces.toList, x))
      .groupBy(_._1)
      .foreach({
        case (k, v) => visitNamespaces(ctx, k, v.map(_._2).toList)
      })
    ctx << s"}"
    ctx.toString
  }

  def visitNamespaces(ctx: CiUnparseContext, namespaces: List[EirNamespace], proxies: List[EirProxy]): Unit = {
    namespaces.foreach(ns => {
      ctx << s"namespace ${ns.name}" << "{"
      ctx << ctx.puppables.filter(x => ns.children.contains(x)).flatMap(x => {
        if (x.templateArgs.isEmpty) List(s"PUPable ${GenerateCpp.nameFor(ctx, x)};")
        else ctx.checked(x).map(y => s"PUPable ${
          ctx.specialize(x, y)
          val ret: String = GenerateCpp.nameFor(ctx, x)
          ctx.leave(y)
          ret
        };")
      })
    })
    proxies.sortBy(!_.isMain).foreach(visit(ctx, _))
    namespaces.foreach(_ => ctx << "}")
  }

  def visit(ctx: CiUnparseContext, proxy: EirProxy): Unit = {
    // TODO bring template args into consideration
    // ctx << GenerateCpp.visitTemplateArgs(proxy.templateArgs)
    ctx << visitChareType(proxy.isMain, proxy.collective) << proxy.baseName << "{" << {
      proxy.membersToGen.foreach(visit(ctx, proxy, _))
    } << "};"
  }

  def attributesFor(m: EirMember): String = {
    m.annotation("threaded").map(x => "[" + x.name + "]").getOrElse("")
  }

  def visit(ctx: CiUnparseContext, proxy: EirProxy, f: EirMember): Unit = {
    if (proxy.isMain && f.isConstructor) {
      ctx << s"entry [nokeep]" << proxy.baseName << "(CkArgMsg* msg);"
    } else {
      ctx << "entry" << attributesFor(f)
      GenerateCpp.visitFunction(ctx, assertValid[EirFunction](f.member), isMember = true)
    }
  }

  def visitChareType(isMain: Boolean, o: Option[String]): String = {
    o match {
      case Some(arrayPtn(dim)) => s"array [${dim}D]"
      case Some(s) => s
      case None if isMain => "mainchare"
      case None => "chare"
    }
  }
}
