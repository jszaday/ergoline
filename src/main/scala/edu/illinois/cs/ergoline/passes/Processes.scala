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

      x match {
        case n : EirNamespace => CheckTypes.visit(ctx, n.children.filterNot(_.isInstanceOf[EirFileSymbol]))
        case _ => CheckTypes.visit(ctx, x)
      }
    }
  }

  def generateCi(): String = {
    GenerateCi.visitAll(checked)
  }

  def generateCpp(): Iterable[String] = {
    val ctx: CodeGenerationContext = new CodeGenerationContext
    val (a, c) = ProxyManager.proxies.toList.partition(_.isAbstract)
    val kids = EirGlobalNamespace.children // .filterNot(_.name == "ergoline")
    val toDecl = checked.keys.collect({
      case c: EirClassLike if !c.isInstanceOf[EirProxy] && c.annotation("system").isEmpty => c
    }).toList.groupBy(x => {
      Find.parentOf[EirNamespace](x).getOrElse(Errors.missingNamespace(x))
    })
    ctx << Seq("#include <ergoline/object.hpp> // ;", "#include <ergoline/hash.hpp> // ;")
    ctx << a.map(GenerateCpp.forwardDecl(ctx, _))
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
    toDecl.foreach({
    case (namespace, classes) =>
      ctx << s"namespace ${namespace.fullyQualifiedName.mkString("::")} {" << {
        classes.collect({ case t: EirTrait => t }).foreach(GenerateCpp.makeFromPuppable(ctx, _))
      } << s"}"
    })
    ctx << List(
      "#define CK_TEMPLATES_ONLY",
      "#include \"generate.def.h\"",
      "#undef CK_TEMPLATES_ONLY",
      "#include \"generate.def.h\""
    ).map(_ + "// ;")
    List(ctx.toString)
  }
}
