package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirResolvable
import edu.illinois.cs.ergoline.proxies.ProxyManager.arrayPtn
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.assertValid

object GenerateCi {

  class CiUnparseContext(val checked: Map[EirSpecializable, List[EirSpecialization]],
                         val lambdas: Map[EirNamespace, List[EirLambdaExpression]]) extends CodeGenerationContext("ci") {
    val puppables: Iterable[EirSpecializable] = checked.keys.filter({
      case _: EirProxy => false
      case x: EirClassLike => !(x.annotation("system").isDefined || x.isAbstract || x.isTransient)
      case _ => false
    })
  }

  def visitPuppables(ctx: CiUnparseContext): Unit = {
    // TODO this is not nesting aware!
    val grouped = ctx.puppables.groupBy(Find.parentOf[EirNamespace](_).getOrElse(???))
    grouped.foreach({
      case (ns, pupables) => {
        ctx << s"namespace ${ns.fullyQualifiedName.mkString("::")}" << "{"
        ctx << ctx.lambdas.getOrElse(ns, Nil).map(GenerateCpp.nameFor).map(name => s"PUPable $name;")
        ctx << pupables.flatMap(x => {
          if (x.templateArgs.isEmpty) List(s"PUPable ${GenerateCpp.nameFor(ctx, x)};")
          else ctx.checked(x).map(y => s"PUPable ${
            ctx.specialize(x, y)
            val ret: String = GenerateCpp.nameFor(ctx, x)
            ctx.leave(y)
            ret
          };")
        })
        ctx << "}"
      }
    })
  }

  def visitAll(checked: Map[EirSpecializable, List[EirSpecialization]], lambdas: Map[EirNamespace, List[EirLambdaExpression]]): String = {
    val ctx = new CiUnparseContext(checked, lambdas)
    ctx << "mainmodule generate" << "{"
    visitPuppables(ctx)
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
      _.specialization.map(Find.uniqueResolution[EirType])
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
