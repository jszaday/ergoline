package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.passes.GenerateCpp.{
  collectiveTypeFor,
  readOnlyFor,
  zipWithSpecializations
}
import edu.illinois.cs.ergoline.proxies.ProxyManager.arrayPtn
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.assertValid

object GenerateCi {

  class CiUnparseContext(override val tyCtx: TypeCheckContext)
      extends CodeGenerationContext("ci", tyCtx) {}

  def generatedMain: Option[String] =
    Option.when(ProxyManager.hasMain)("generated_main_")

  def visitAll(tyCtx: TypeCheckContext): String = {
    val ctx = new CiUnparseContext(tyCtx)
    ctx << "mainmodule generate" << "{"

    generatedMain match {
      case Some(name) =>
        ctx << "mainchare" << name << "{"
        ctx << "entry [nokeep] generated_main_(CkArgMsg*);"
        ctx << "};"
      case None =>
    }

    ctx << "initproc void setup_environment(void);"

    ProxyManager.proxies
      .filter(ProxyManager.shouldGenerate)
      .map(x => (x.namespaces.toList, x))
      .groupBy(_._1)
      .foreach({ case (k, v) => visitNamespaces(ctx, k, v.map(_._2).toList) })

    ctx << s"}"
    ctx.toString
  }

  def makeChareSpecializations(ctx: CiUnparseContext, proxy: EirProxy): Unit = {
    val kind = visitChareType(proxy.isMain, proxy.collective)
    val specializations = ctx.checked
      .getOrElse(proxy, Nil)
      .map(
        _.types.map(Find.uniqueResolution[EirType])
      ) // NOTE we might want to consider putting .distinct here?
    //      (but it shouldn't be necessary)
    specializations.foreach(sp => {
      ctx << kind << proxy.baseName << "<" << {
        (sp.map(ctx.typeFor(_, Some(proxy))), ", ")
      } << ">" << ";"
    })
  }

  def makeEntrySpecialization(
      proxy: EirProxy,
      m: EirMember,
      tys: List[EirResolvable[EirType]]
  )(ctx: CiUnparseContext): Unit = {
    m.member match {
      case f: EirFunction if f.templateArgs.nonEmpty =>
        ctx << "extern" << "entry" << attributesFor(proxy, m) << ctx.typeFor(
          f.returnType,
          Some(m)
        ) << proxy.baseName << ctx.nameFor(f) << "<" << {
          (tys.map(ctx.typeFor(_, Some(m))), ",")
        } << ">" << "(" << {
          if (f.functionArgs.nonEmpty || m.annotations.contains("async")) {
            "CkMessage*"
          } else {
            "void"
          }
        } << ");"
      case _ => ???
    }
  }

  def makeEntrySpecializations(proxy: EirProxy)(ctx: CiUnparseContext): Unit = {
    proxy.members collect {
      case m @ EirMember(_, sp: EirSpecializable, _)
          if m.isEntry && sp.templateArgs.nonEmpty => (m, sp)
    } foreach { case (m, sp) =>
      val ss = ctx.checked(sp)
      ss foreach { s => makeEntrySpecialization(proxy, m, s.types)(ctx) }
    }
  }

  val registerMailboxes = "__register_mailboxes__"

  def visitNamespaces(
      ctx: CiUnparseContext,
      namespaces: List[EirNamespace],
      proxies: List[EirProxy]
  ): Unit = {
    namespaces.foreach(ns => {
      ctx << s"namespace ${ns.name}" << "{"
    })

    proxies
      .sortBy(!_.isMain)
      // NOTE only element proxies are specialized!
      .map(p => ProxyManager.elementFor(p).getOrElse(p))
      .foreach(p => {
        visit(ctx, p)

        if (p.mailboxes.nonEmpty) {
          ctx << s"initnode void ${p.baseName}::$registerMailboxes(void);"
        }

        if (p.templateArgs.nonEmpty) {
          makeChareSpecializations(ctx, p)
        }

        makeEntrySpecializations(p)(ctx)

        if (p.collective.isEmpty) {
          zipWithSpecializations(Seq(p))(ctx) foreach { case (p, types) =>
            ctx << "readonly" << collectiveTypeFor(p, types)(
              ctx
            ) << " " << readOnlyFor(p, types, None)(ctx) << ";"
          }
        }
      })

    namespaces.foreach(_ => ctx << "}")
  }

  def visit(ctx: CiUnparseContext, proxy: EirProxy): Unit = {
    // TODO bring template args and inheritance into consideration
    GenerateCpp.visitTemplateArgs(proxy.templateArgs)(ctx)
    ctx << visitChareType(proxy.isMain, proxy.collective) << proxy.baseName
    ctx << ":" << "hypercomm::locality_base_" << "{" << {
      proxy.membersToGen.foreach(visit(ctx, proxy, _))
    } << "};"
  }

  val passThruAttributes: Seq[String] = Seq(
    "local",
    "threaded",
    "createhere",
    "createhome"
  )

  def attributesFor(p: EirProxy, m: EirMember): String = {
    val attributes = passThruAttributes.flatMap(m.annotation).map(_.name)
//      Option.when(p.collective.contains("nodegroup") && !m.isConstructor)("exclusive") ++
    if (attributes.nonEmpty) s" [${attributes mkString ","}] " else ""
  }

  def visit(ctx: CiUnparseContext, p: EirProxy, f: EirMember): Unit = {
//    if (p.isMain && f.isConstructor) {
//      ctx << s"entry [nokeep] " << p.baseName << "(CkArgMsg* msg);"
//    } else {
    if (!f.isMailbox) {
      val entry = "entry" + attributesFor(p, f)
      GenerateCpp.visitFunction(
        assertValid[EirFunction](f.member),
        isMember = true,
        Some(entry)
      )(ctx)
    }
//    }
  }

  def visitChareType(isMain: Boolean, o: Option[String]): String = {
//    o match {
//      case Some(arrayPtn(dim)) => s"array [${dim}D]"
//      case Some(s) => s
//      case None if isMain => "mainchare"
//      case None => "chare"
//    }
    "array [Max] "
  }
}
