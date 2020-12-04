package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirClassLike, EirFileSymbol, EirGlobalNamespace, EirNamespace, EirNode, EirSpecializable, EirSpecialization, EirTrait}
import edu.illinois.cs.ergoline.passes.UnparseAst.UnparseContext
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.Errors

import scala.util.Properties.{lineSeparator => n}

object Processes {
  private val ctx = new TypeCheckContext

  def checked: Map[EirSpecializable, List[EirSpecialization]] = ctx.checked

  def onLoad(x : EirNode): Unit = {
    FullyResolve.visit(x)
    Find.classes(x.scope.getOrElse(x)).foreach(CheckClasses.visit)

    x match {
      case n : EirNamespace => CheckTypes.visit(ctx, n.children.filterNot(_.isInstanceOf[EirFileSymbol]))
      case _ => CheckTypes.visit(ctx, x)
    }
  }

  def generateCi(): String = {
    GenerateCi.visitAll(ctx.checked)
  }

  def generateCpp(): Iterable[String] = {
    val (a, c) = ProxyManager.proxies.toList.partition(_.isAbstract)
    val ctx = new UnparseContext("cpp")
    val body = a.map(GenerateCpp.visit(ctx, _)) ++
      EirGlobalNamespace.children.filterNot(_.name == "ergoline").map(GenerateCpp.visit(ctx, _)) ++
      c.map(GenerateCpp.visit(ctx, _))
    val grouped = body.map(_.trim).groupBy(x => x.substring(0, x.indexOf('{') + 1))
    val gathered = grouped.map(x => {
      x._1 + n + x._2.map(y => y.substring(x._1.length + 1, y.length - 1)).mkString("") + n + "}"
    })
    val toDecl = checked.keys.collect({
      case c: EirClassLike if !c.isInstanceOf[EirProxy] && c.annotation("system").isEmpty => c
    }).toList.groupBy(x => {
      Find.parentOf[EirNamespace](x).getOrElse(Errors.missingNamespace(x))
    })
    val fwdDecls = toDecl.map({
      case (namespace, classes) =>
        s"namespace ${namespace.fullyQualifiedName.mkString("::")} {$n" +
          classes.map(GenerateCpp.forwardDecl(ctx, _)).mkString(n) + s"$n}$n"
    })
    val wrapup = toDecl.map({
      case (namespace, classes) =>
        s"namespace ${namespace.fullyQualifiedName.mkString("::")} {$n" +
          classes.collect({ case t: EirTrait => t }).map(GenerateCpp.makeFromPuppable(ctx, _)).mkString(n) + s"$n}$n"
    })
    fwdDecls ++ a.map(GenerateCpp.forwardDecl(ctx, _)) ++
      Seq("#include <algorithm>",
          "#include <memory>",
          "#include <string>",
          "#include <vector>",
          "#include \"generate.decl.h\"") ++
      gathered ++ wrapup ++ Seq(n + "#include \"generate.def.h\"")
  }
}
