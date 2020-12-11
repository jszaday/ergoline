package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.{Find, Modules}
import edu.illinois.cs.ergoline.util.Errors
import scala.util.Properties.{lineSeparator => n}

object Processes {
  private val ctx = new TypeCheckContext

  var cppIncludes: Set[String] = Set(
    "algorithm",
    "memory",
    "string",
    "tuple",
    "utility",
    "vector",
    "#include \"generate.decl.h\" // ;"
  )

  def checked: Map[EirSpecializable, List[EirSpecialization]] = ctx.checked

  def onLoad(node : EirNode): Unit = {
    val all = node +: Modules.fileSiblings.getOrElse(node, Nil)

    for (x <- all) {
      FullyResolve.visit(x)
      Find.classes(x.scope.getOrElse(x)).foreach(CheckClasses.visit)

      x match {
        case n : EirNamespace => CheckTypes.visit(ctx, n.children.filterNot(_.isInstanceOf[EirFileSymbol]))
        case _ => CheckTypes.visit(ctx, x)
      }
    }
  }

  def generateCi(): String = {
    GenerateCi.visitAll(ctx.checked)
  }

  def generateCpp(): Iterable[String] = {
    val ctx: CodeGenerationContext = new CodeGenerationContext
    val (a, c) = ProxyManager.proxies.toList.partition(_.isAbstract)
    val kids = EirGlobalNamespace.children.filterNot(_.name == "ergoline")
    val toDecl = checked.keys.collect({
      case c: EirClassLike if !c.isInstanceOf[EirProxy] && c.annotation("system").isEmpty => c
    }).toList.groupBy(x => {
      Find.parentOf[EirNamespace](x).getOrElse(Errors.missingNamespace(x))
    })
    ctx << Seq("#include \"pup.h\" // ;") ++ (a.map(GenerateCpp.forwardDecl(ctx, _)) ++  Seq(n + GenerateCpp.systemClasses() + n))
    toDecl.foreach({
      case (namespace, classes) =>
        ctx << s"namespace ${namespace.fullyQualifiedName.mkString("::")}" << "{" << {
          classes.foreach(GenerateCpp.forwardDecl(ctx, _))
        } << "}"
    })
    ctx << cppIncludes.map(x => if (x.contains("#include")) x else s"#include <$x> // ;")
    a.foreach(GenerateProxies.visitProxy(ctx, _))
    kids.foreach(GenerateDecls.visit(ctx, _))
    kids.foreach(GenerateCpp.visit(ctx, _))
    c.foreach(GenerateProxies.visitProxy(ctx, _))
    println()
    Nil
//
//    val ctx = new UnparseContext("cpp")
//    val body = a.map(GenerateCpp.visit(ctx, _)) ++
//      EirGlobalNamespace.children.filterNot(_.name == "ergoline").map(GenerateCpp.visit(ctx, _)) ++
//      c.map(GenerateCpp.visit(ctx, _))
//    val grouped = body.map(_.trim).groupBy(x => x.substring(0, x.indexOf('{') + 1))
//    val gathered = grouped.map(x => {
//      x._1 + n + x._2.map(y => y.substring(x._1.length + 1, y.length - 1)).mkString("") + n + "}"
//    })
//    val toDecl = checked.keys.collect({
//      case c: EirClassLike if !c.isInstanceOf[EirProxy] && c.annotation("system").isEmpty => c
//    }).toList.groupBy(x => {
//      Find.parentOf[EirNamespace](x).getOrElse(Errors.missingNamespace(x))
//    })
//    val fwdDecls = toDecl.map({
//      case (namespace, classes) =>
//        s"namespace ${namespace.fullyQualifiedName.mkString("::")} {$n" +
//          classes.map(GenerateCpp.forwardDecl(ctx, _)).mkString(n) + s"$n}$n"
//    })
//    val wrapup = toDecl.map({
//      case (namespace, classes) =>
//        s"namespace ${namespace.fullyQualifiedName.mkString("::")} {$n" +
//          classes.collect({ case t: EirTrait => t }).map(GenerateCpp.makeFromPuppable(ctx, _)).mkString(n) + s"$n}$n"
//    })
    ctx << List(
      "#define CK_TEMPLATES_ONLY",
      "#include \"generate.def.h\"",
      "#undef CK_TEMPLATES_ONLY",
      "#include \"generate.def.h\""
    ).map(_ + "// ;")
    List(ctx.toString)
//    fwdDecls ++  ++
//       ++
//      gathered ++ wrapup ++
  }
}
