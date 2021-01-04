package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirClassLike, _}
import edu.illinois.cs.ergoline.ast.types.{EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.Processes.RichProcessesSyntax.RichEirClassList
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find, Modules}
import edu.illinois.cs.ergoline.util.Errors

import scala.annotation.tailrec
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

  object RichProcessesSyntax {
    implicit class RichEirClassList(self: List[EirClassLike]) {

      // TODO use a topological instead of greedy sorting algorithm
       def dependenceSort(): List[EirClassLike] = {
        var unplaced = self.sortBy(_.inherited.size)
        var placed: List[EirClassLike] = Nil
        while (unplaced.nonEmpty) {
          val idx = unplaced.indexWhere(
            !_.inherited.map(Find.asClassLike).exists(unplaced.contains(_)))
          placed :+= unplaced(idx)
          unplaced = unplaced.patch(idx, Nil, 1)
        }
        placed
      }

      // TODO find a more idiomatic way to do this
      def orderedPartition[A](f: EirClassLike => A): List[(A, List[EirClassLike])] = {
        var current: Option[A] = None
        var group: List[EirClassLike] = Nil
        var result: List[(A, List[EirClassLike])] = Nil
        for (a <- self) {
          val b = f(a)
          if (!current.contains(b)) {
            current match {
              case Some(c) =>
                result :+= (c -> group)
              case _ =>
            }
            current = Some(b)
            group = Nil
          }
          group :+= a
        }
        if (group.nonEmpty) {
          assert(current.isDefined)
          result :+= (current.get -> group)
        }
        result
      }

      def hasValidOrder: Boolean = {
        self.zipWithIndex.forall({
          case (c, i) =>
            self.find(c.isDescendantOf).forall(self.indexOf(_) < i)
        })
      }
    }
  }

  def generateCpp(): Iterable[String] = {
    val ctx: CodeGenerationContext = new CodeGenerationContext
    val (a, c) = ProxyManager.proxies.toList.partition(_.isAbstract)
    val kids = EirGlobalNamespace.children // .filterNot(_.name == "ergoline")
    val sorted = checked.keys.collect({
      case c: EirClassLike if !c.isInstanceOf[EirProxy] && c.annotation("system").isEmpty => c
    }).toList.dependenceSort()
    assert(!globals.strict || sorted.hasValidOrder)
    val toDecl = sorted.orderedPartition(x => {
      Find.parentOf[EirNamespace](x)
        .getOrElse(Errors.missingNamespace(x))
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
    ctx << List(
      "#define CK_TEMPLATES_ONLY",
      "#include \"generate.def.h\"",
      "#undef CK_TEMPLATES_ONLY",
      "#include \"generate.def.h\""
    ).map(_ + "// ;")
    List(ctx.toString)
  }
}
