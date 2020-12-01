package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.passes.UnparseAst.UnparseContext
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.proxies.ProxyManager.arrayPtn

import scala.util.Properties.{lineSeparator => n}

object GenerateCi {

  def visitAll(): String = {
    val ctx = new UnparseContext
    ctx.numTabs += 1
    val body = ProxyManager.proxies
      .filterNot(x => x.base.isAbstract || x.isElement)
      .map(x => (x.namespaces.toList, x))
      .groupBy(_._1)
      .map({
        case (k, v) => visitNamespaces(ctx, k, v.map(_._2).toList)
      }).mkString(n)
    ctx.numTabs -= 1
    s"mainmodule generate {$n$body}$n"
  }

  def visitNamespaces(ctx: UnparseContext, namespaces: List[EirNamespace], proxies: List[EirProxy]): String = {
    namespaces.map(ns => s"${n}namespace ${ns.name} {").mkString("") + n + {
      proxies.map(visit(ctx, _)).mkString("")
    } + "}" + n
  }

  def visit(ctx: UnparseContext, proxy: EirProxy): String = {
    val template: String = GenerateCpp.visitTemplateArgs(ctx, proxy.templateArgs)
    val name = proxy.baseName
    val header =
      template + ctx.t + visitChareType(proxy.isMain, proxy.collective) + s" $name {$n"
    ctx.numTabs += 1
    val body = proxy.membersToGen.map(visit(ctx, proxy, _)).mkString(n)
    ctx.numTabs -= 1
    header + body + s"${ctx.t}};$n"
  }

  def visit(ctx: UnparseContext, proxy: EirProxy, f: EirMember): String = {
    if (proxy.isMain && f.isConstructor) {
      s"${ctx.t} entry [nokeep] ${proxy.baseName}(CkArgMsg* msg);$n"
    } else {
      val body =
        GenerateCpp.visit(ctx, f.member)
          .replaceFirst(proxy.name, proxy.baseName)
      s"${ctx.t} entry $body$n"
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