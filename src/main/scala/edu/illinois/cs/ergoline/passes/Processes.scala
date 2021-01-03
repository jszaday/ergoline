package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirClassLike, _}
import edu.illinois.cs.ergoline.ast.types.{EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.globals
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

  @tailrec
  def toClassLike(resolvable: EirResolvable[EirType]): EirClassLike = {
    Find.uniqueResolution(resolvable) match {
      case EirTemplatedType(_, base, _) => toClassLike(base)
      case c: EirClassLike => c
      case t => Errors.incorrectType(t, classOf[EirClassLike])
    }
  }

  def sortClasses(input: List[EirClassLike]): List[EirClassLike] = {
    var unplaced = input.sortBy(_.inherited.size)
    var placed: List[EirClassLike] = Nil
    while (unplaced.nonEmpty) {
      val idx = unplaced.indexWhere(
        !_.inherited.map(toClassLike).exists(unplaced.contains(_)))
      placed :+= unplaced(idx)
      unplaced = unplaced.patch(idx, Nil, 1)
    }
    placed
  }

  def partitionWithOrder[A, B](list: List[A], f: A => B): List[(B, List[A])] = {
    var current: Option[B] = None
    var group: List[A] = Nil
    var result: List[(B, List[A])] = Nil
    for (a <- list) {
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

  def hasValidOrder(classes: List[EirClassLike]): Boolean = {
    classes.zipWithIndex.forall({
      case (c, i) =>
        classes.find(c.isDescendantOf).forall(classes.indexOf(_) < i)
    })
  }

  def generateCpp(): Iterable[String] = {
    val ctx: CodeGenerationContext = new CodeGenerationContext
    val (a, c) = ProxyManager.proxies.toList.partition(_.isAbstract)
    val kids = EirGlobalNamespace.children // .filterNot(_.name == "ergoline")
    val sorted = sortClasses(checked.keys.collect({
      case c: EirClassLike if !c.isInstanceOf[EirProxy] && c.annotation("system").isEmpty => c
    }).toList)
    assert(!globals.strict || hasValidOrder(sorted))
    val toDecl = partitionWithOrder(sorted, (x: EirClassLike) => {
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
    ctx << List(
      "#define CK_TEMPLATES_ONLY",
      "#include \"generate.def.h\"",
      "#undef CK_TEMPLATES_ONLY",
      "#include \"generate.def.h\""
    ).map(_ + "// ;")
    List(ctx.toString)
  }
}
