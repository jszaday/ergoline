package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirClassLike, _}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.Processes.RichProcessesSyntax.RichEirClassList
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.{Find, Modules}
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirClassLike

object Processes {
  var cppIncludes: Set[String] = Set(
    "algorithm",
    "memory",
    "string",
    "tuple",
    "utility",
    "vector",
//    "ergoline/array.hpp",
//    "ergoline/requests.hpp",
//    "ergoline/reducer.hpp",
    "#include \"generate.decl.h\" // ;"
  )

  var sensitiveDeclIncludes: Map[String, String] = Map(
    ("range", "ergoline/section.decl.hpp")
  )

  var sensitiveDefIncludes: Map[String, String] = Map(
    ("range", "ergoline/section.def.hpp")
  )

  private var ctx = new TypeCheckContext

  def reset(): Unit = {
    ctx = new TypeCheckContext
  }

  def onLoad(node: EirNode): Unit = {
    val all = node +: Modules.fileSiblings.getOrElse(node, Nil)

    for (x <- all) {
      FullyResolve.visit(x)

      x match {
        case n: EirNamespace =>
          CheckTypes.visit(n.children.filterNot(_.isInstanceOf[EirFileSymbol]))(
            ctx
          )
        case _ => CheckTypes.visit(x)(ctx)
      }
    }
  }

  // NOTE This will go away once passes are implemented
  def generateCi(): String = {
    GenerateCi.visitAll(ctx)
  }

  val priorityIncludes: Seq[String] = Seq(
    "#include <ergoline/components.hpp> // ;",
    "#include <hypercomm/core/typed_value.hpp> // ;",
    "#include <hypercomm/components/sentinel.hpp> // ;",
    "#include <hypercomm/core/resuming_callback.hpp> // ;",
    "#include <hypercomm/core/inter_callback.hpp> // ;",
    "#include <ergoline/callback.hpp> // ;",
    "#include <ergoline/function.hpp> // ;",
    "#include <ergoline/mailbox.hpp> // ;",
    "#include <ergoline/reducer.hpp> // ;",
    "#include <ergoline/object.hpp> // ;",
    "#include <ergoline/array.hpp> // ;"
  )

  def willGenerate(name: String)(implicit ctx: CodeGenerationContext): Boolean = {
    ctx.checked.keys exists {
      case n: EirNamedNode => n.name == name
      case _ => false
    }
  }

  def sensitiveHelper(map: Map[String, String])(implicit ctx: CodeGenerationContext) = {
    map foreach {
      case (name, header) =>
        if (willGenerate(name)) {
          ctx << s"#include <$header> // ;"
        }
    }
  }

  // TODO this logic should be moved into GenerateCpp
  // NOTE This will go away once passes are implemented
  def generateCpp(): Iterable[String] = {
    val ctx: CodeGenerationContext = new CodeGenerationContext("cpp", this.ctx)
    val (a, c) = ProxyManager.proxies.toList.partition(_.isAbstract)
    val kids = EirGlobalNamespace.children // .filterNot(_.name == "ergoline")

    val sorted = ctx.checked.keys
      .collect({
        case c: EirClassLike
            if !c.isInstanceOf[EirProxy] && c.annotation("system").isEmpty =>
          c
      })
      .toList
      .dependenceSort()
    assert(!globals.strict || sorted.hasValidOrder)
    val toDecl = sorted.namespacePartitioned
    ctx << priorityIncludes
    // NOTE do we ever need to topo sort these?
    a.foreach(GenerateCpp.forwardDecl(ctx, _))

    toDecl foreach {
      case (namespace, classes) =>
        ctx << s"namespace ${namespace.fullyQualifiedName.mkString("::")}" << "{" << {
          classes.foreach(GenerateCpp.forwardDecl(_)(ctx))
        } << "}"
    }

    ctx << cppIncludes.map(x =>
      if (x.contains("#include")) x else s"#include <$x> // ;"
    )

    sensitiveHelper(sensitiveDeclIncludes)(ctx)

    GenerateCpp.declareGlobals(ctx)

    // NOTE do we ever need to topo sort proxies?
    a.foreach(GenerateProxies.visitProxy(ctx, _))

    toDecl foreach {
      case (namespace, classes) =>
        ctx << s"namespace ${namespace.fullyQualifiedName.mkString("::")}" << "{" << {
          classes.foreach(GenerateDecls.visit(ctx, _))
        } << "}"
    }
    ctx.lambdas.foreach({
      case (namespace, lambdas) =>
        ctx << s"namespace ${namespace.fullyQualifiedName.mkString("::")}" << "{" << {
          lambdas.foreach(GenerateCpp.makeLambdaWrapper(ctx, _))
        } << "}"
    })
    kids.foreach(GenerateCpp.visit(_)(ctx))
    c.foreach(GenerateProxies.visitProxy(ctx, _))

    GenerateCpp.generateMain(ctx)
    GenerateCpp.registerPolymorphs(ctx)

    sensitiveHelper(sensitiveDefIncludes)(ctx)

    ctx << List(
      "#define CK_TEMPLATES_ONLY",
      "#include \"generate.def.h\"",
      "#undef CK_TEMPLATES_ONLY",
      "#include \"generate.def.h\""
    ).map(_ + "// ;")

    List(ctx.toString)
  }

  // TODO this logic should be moved into its own file or generate cpp
  object RichProcessesSyntax {
    implicit class RichEirClassList(self: List[EirClassLike]) {

      // TODO use a topological instead of greedy sorting algorithm
      def dependenceSort(): List[EirClassLike] = {
        var unplaced = self.sortBy(_.inherited.size)
        var placed: List[EirClassLike] = Nil
        while (unplaced.nonEmpty) {
          val idx = unplaced.indexWhere(
            !_.inherited.map(Find.asClassLike).exists(unplaced.contains(_))
          )
          placed :+= unplaced(idx)
          unplaced = unplaced.patch(idx, Nil, 1)
        }
        placed
      }

      def namespacePartitioned: List[(EirNamespace, List[EirClassLike])] =
        self.orderedPartition(x => {
          Find.parentOf[EirNamespace](x).getOrElse(Errors.missingNamespace(x))
        })

      // TODO find a more idiomatic way to do this
      def orderedPartition[A](
          f: EirClassLike => A
      ): List[(A, List[EirClassLike])] = {
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
}
