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
import edu.illinois.cs.ergoline.resolution.Find
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

    ctx << "extern module locality;"
    ctx << "initproc void setup_environment(void);"

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
      .foreach(p => {
        visit(ctx, p)

        if (p.templateArgs.nonEmpty) {
          makeChareSpecializations(ctx, p)
        }

        if (p.collective.isEmpty) {
          zipWithSpecializations(Seq(p))(ctx) foreach {
            case (p, types) =>
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
    ctx << ":" << "locality_base_" << "{" << {
      proxy.membersToGen.foreach(visit(ctx, proxy, _))
    } << "};"
  }

  val passThruAttributes: Seq[String] = Seq(
    "threaded",
    "createhome",
    "createhere"
  )

  def attributesFor(p: EirProxy, m: EirMember): String = {
    val attributes =
      passThruAttributes.flatMap(m.annotation).map(_.name)
//      Option.when(p.collective.contains("nodegroup") && !m.isConstructor)("exclusive") ++
    if (attributes.nonEmpty) s" [${attributes mkString ","}] " else ""
  }

  def visit(ctx: CiUnparseContext, p: EirProxy, f: EirMember): Unit = {
//    if (p.isMain && f.isConstructor) {
//      ctx << s"entry [nokeep] " << p.baseName << "(CkArgMsg* msg);"
//    } else {
    ctx << "entry" << attributesFor(p, f)
    GenerateCpp.visitFunction(
      assertValid[EirFunction](f.member),
      isMember = true
    )(ctx)
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
