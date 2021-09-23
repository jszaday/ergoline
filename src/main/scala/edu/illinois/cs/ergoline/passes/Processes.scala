package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.types.EirTemplatedType
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.Processes.RichProcessesSyntax.RichEirClassList
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.{Find, Modules}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirClassLike
import edu.illinois.cs.ergoline.util.{Errors, TopologicalSort, isSystem}

object Processes {
  def registerPasses(): Unit = {
    Registry.instance[FullyResolve]
    Registry.instance[CheckTypes]
    Registry.instance[Orchestrate]
  }

  registerPasses()

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
    ("iterable", "ergoline/section.decl.hpp")
  )

  var sensitiveDefIncludes: Map[String, String] = Map(
    ("iterable", "ergoline/section.def.hpp")
  )

  private var ctx: TypeCheckContext = new TypeCheckContext

  def typeContext(): TypeCheckContext = ctx

  def reset(): Unit = {
    ctx = new TypeCheckContext
  }

  def isMain(node: EirNode): Boolean = {
    node.annotation("main").nonEmpty
  }

  def onOptimize(node: EirNode): Unit = {
    Registry.onOptimize.foreach(pass => {
      Find
        .topLevel(
          node,
          Option.unless(pass.annotations.isEmpty)((a: EirAnnotation) => {
            pass.annotations.contains(a.name)
          })
        )
        .foreach(pass(_))
    })
  }

  def onLoad(node: EirNode): Unit = {
    val all =
      (node +: Modules.fileSiblings.getOrElse(node, Nil)).sortBy(!isMain(_))

    for (node <- all) {
      Registry.onLoad.foreach(pass => {
        if (pass.canEnter[EirFileSymbol]) {
          pass(node)
        } else {
          node match {
            case n: EirNamespace => n.children
                .filterNot(_.isInstanceOf[EirFileSymbol])
                .foreach(pass(_))
            case _ => pass(node)
          }
        }
      })
    }

    for (node <- all) {
      onOptimize(node)
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
    "#include <hypercomm/core/temporary_port.hpp> // ;",
    "#include <ergoline/callback.hpp> // ;",
    "#include <ergoline/function.hpp> // ;",
    "#include <ergoline/mailbox.hpp> // ;",
    "#include <ergoline/collectives.hpp> // ;",
    "#include <ergoline/object.hpp> // ;",
    "#include <ergoline/array.hpp> // ;"
  )

  def willGenerate(
      name: String
  )(implicit ctx: CodeGenerationContext): Boolean = {
    ctx.checked.keys exists {
      case n: EirNamedNode => n.name == name
      case _               => false
    }
  }

  def sensitiveHelper(
      map: Map[String, String]
  )(implicit ctx: CodeGenerationContext): Unit = {
    map foreach { case (name, header) =>
      if (willGenerate(name)) {
        ctx << s"#include <$header> // ;"
      }
    }
  }

  private[this] def checkContext(): TypeCheckContext = {
    assert(this.ctx.transactions.isEmpty)

    val redFlag = ctx.checked.filterNot(x => isSystem(x._1)).map { x =>
      val sets = x._2.map(_.types.map(x => {
        Find.tryClassLike(x).map(_.classKind).getOrElse(EirValueKind)
      }))
      val min = if (sets.nonEmpty) sets.map(_.size).min else 0
      sets.map(_.slice(0, min)).toSet
    } exists (_.size > 1)

    if (redFlag) {
      Errors.warn(
        s"due to ${Errors.Limitation.CppCodeGen}, specializations of mixed-kinded types (e.g., value and reference) might fail."
      )
    }

    this.ctx
  }

  // TODO this logic should be moved into GenerateCpp
  // NOTE This will go away once passes are implemented
  def generateCpp(): Iterable[String] = {
    val ctx: CodeGenerationContext =
      new CodeGenerationContext("cpp", this.checkContext())
    val (a, c) = ProxyManager.proxies.toList.partition(_.isAbstract)
    val kids = EirGlobalNamespace.children // .filterNot(_.name == "ergoline")

    val sorted = ctx.checked.keys
      .collect({
        case c: EirClassLike if !c.isInstanceOf[EirProxy] => c
      })
      .toList
      .dependenceSort()

    assert(!globals.strict || sorted.hasValidOrder)

    val toDecl = sorted.namespacePartitioned
    ctx << priorityIncludes
    // NOTE do we ever need to topo sort these?
    a.foreach(GenerateCpp.forwardDecl(ctx, _))
    c.filter(ProxyManager.shouldGenerate)
     .foreach(GenerateProxies.makeIndices(ctx, _))

    toDecl foreach { case (namespace, classes) =>
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

    toDecl foreach { case (namespace, classes) =>
      ctx << "namespace" << (namespace.fullyQualifiedName, "::") << "{" << {
        classes
          .filterNot(_.isNested)
          .foreach(GenerateDecls.visit(ctx, _))
      } << "}"
    }

    ctx.lambdas.foreach({ case (namespace, lambdas) =>
      ctx << s"namespace ${namespace.fullyQualifiedName.mkString("::")}" << "{" << {
        lambdas.foreach(GenerateCpp.makeLambdaWrapper(ctx, _))
      } << "}"
    })

    val iterableTy = globals.iterableType
    val iterables = sorted
      .collect({
        case s: EirClass if s.isDescendantOf(iterableTy) => s
      })
      .flatMap(s => {
        val ts = GenerateCpp.templatize(s)

        Find
          .implementationOf(ts, iterableTy)(ctx.tyCtx)
          .to[EirTemplatedType]
          .map(t => (ts, t))
      })
    if (iterables.nonEmpty) {
      ctx << "namespace" << "ergoline" << "{"
      iterables.foreach(it => GenerateDecls.mkIteratorBridge(it._1, it._2)(ctx))
      ctx << "}"
    }

    kids.foreach(GenerateCpp.visit(_)(ctx))
    c.filter(ProxyManager.shouldGenerate)
      .foreach(GenerateProxies.visitProxy(ctx, _))

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
    implicit class RichSeq[A](self: Seq[A]) {

      // TODO find a more idiomatic way to do this
      def orderedPartition[B](
          f: A => B
      ): List[(B, List[A])] = {
        var current: Option[B] = None
        var group: List[A] = Nil
        var result: List[(B, List[A])] = Nil
        for (a <- self) {
          val b = f(a)
          if (!current.contains(b)) {
            current match {
              case Some(c) => result :+= (c -> group)
              case _       =>
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
    }

    implicit class RichEirClassList(self: List[EirClassLike]) {

      def dependenceSort(): List[EirClassLike] = TopologicalSort.sort(self)

      def namespacePartitioned: List[(EirNamespace, List[EirClassLike])] =
        self.orderedPartition(x => {
          Find.parentOf[EirNamespace](x).getOrElse(Errors.missingNamespace(x))
        })

      def hasValidOrder: Boolean = {
        self.zipWithIndex.forall({ case (c, i) =>
          self.find(c.isDescendantOf).forall(self.indexOf(_) < i)
        })
      }
    }
  }
}
