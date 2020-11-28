package edu.illinois.cs.ergoline.passes

import scala.util.Properties.{lineSeparator => n}
import UnparseAst.UnparseContext
import edu.illinois.cs.ergoline.ast.{EirFunction, EirMember}
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}

import scala.util.matching.Regex

object GenerateCi {
  val arrayPtn: Regex = raw"array(\d+)".r

  def visitAll(): String = {
    val ctx = new UnparseContext
    ProxyManager.proxies
      .filterNot(_.base.isAbstract)
      .map(visit(ctx, _)).mkString(n)
  }

  def visit(ctx: UnparseContext, proxy: EirProxy): String = {
    val template: String = GenerateCpp.visitTemplateArgs(ctx, proxy.templateArgs)
    val isMain = proxy.base.annotations.exists(_.name == "main")
    val header =
      template + ctx.t + visitChareType(isMain, proxy.collective) + " " + proxy.name + s" {$n"
    ctx.numTabs += 1
    val body = proxy.members.map(visit(ctx, _)).mkString(n)
    ctx.numTabs -= 1
    header + body + s"${ctx.t}}$n"
  }

  def visit(ctx: UnparseContext, f: EirMember): String = {
    s"${ctx.t} entry " + GenerateCpp.visit(ctx, f.member) + n
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
