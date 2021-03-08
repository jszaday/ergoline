package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirResolvable
import edu.illinois.cs.ergoline.proxies.ProxyManager.arrayPtn
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.assertValid

object GenerateCi {

  class CiUnparseContext(override val tyCtx: TypeCheckContext) extends CodeGenerationContext("ci", tyCtx) {}

  def visitAll(tyCtx: TypeCheckContext): String = {
    val ctx = new CiUnparseContext(tyCtx)
    ctx << "mainmodule generate" << "{"
    ctx << "initnode void enroll_polymorphs(void);"
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

  def makeChareSpecializations(ctx: CiUnparseContext, proxy: EirProxy): Unit = {
    val kind = visitChareType(proxy.isMain, proxy.collective)
    val specializations = ctx.checked.getOrElse(proxy, Nil).map(
      _.types.map(Find.uniqueResolution[EirType])
    ) // NOTE we might want to consider putting .distinct here?
      //      (but it shouldn't be necessary)
    specializations.foreach(sp => {
      ctx << kind << proxy.baseName << "<" << {
        (sp.map(ctx.typeFor(_, Some(proxy))), ", ")
      } << ">" << ";"
    })
  }

  def visitNamespaces(ctx: CiUnparseContext, namespaces: List[EirNamespace], proxies: List[EirProxy]): Unit = {
    namespaces.foreach(ns => {
      ctx << s"namespace ${ns.name}" << "{"
    })
    proxies.sortBy(!_.isMain).foreach(visit(ctx, _))
    proxies.filter(_.templateArgs.nonEmpty).foreach(makeChareSpecializations(ctx, _))
    namespaces.foreach(_ => ctx << "}")
  }

  def visit(ctx: CiUnparseContext, proxy: EirProxy): Unit = {
    // TODO bring template args into consideration
    GenerateCpp.visitTemplateArgs(ctx, proxy.templateArgs)
    ctx << visitChareType(proxy.isMain, proxy.collective) << proxy.baseName << "{" << {
      proxy.membersToGen.foreach(visit(ctx, proxy, _))
    } << "};"
  }

  def attributesFor(p: EirProxy, m: EirMember): String = {
    val attributes =
      Option.when(p.collective.contains("nodegroup") && !m.isConstructor)("exclusive") ++
      m.annotation("threaded").map(_.name)
    if (attributes.nonEmpty) s" [${attributes mkString ","}] " else ""
  }

  def visit(ctx: CiUnparseContext, p: EirProxy, f: EirMember): Unit = {
    if (p.isMain && f.isConstructor) {
      ctx << s"entry [nokeep] " << p.baseName << "(CkArgMsg* msg);"
    } else {
      ctx << "entry" << attributesFor(p, f)
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
