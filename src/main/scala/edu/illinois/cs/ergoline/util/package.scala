package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.EirAccessibility.EirAccessibility
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.passes.{CheckTypes, TypeCheckContext}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirClassLike

import scala.annotation.tailrec
import scala.collection.View
import scala.reflect.{ClassTag, classTag}

package object util {

  import EirUtilitySyntax.RichEirNode

  def addExplicitSelf(
      ctx: TypeCheckContext,
      target: EirType,
      self: EirType
  ): EirType = {
    target match {
      case EirLambdaType(_, from, to, args, pred) =>
        ctx.lambdaWith(self +: from, to, args, pred)
      case _ => Errors.incorrectType(target, classTag[EirLambdaType])
    }
  }

  def makeMemberFunctionWithArgs(
      parent: EirClassLike,
      name: String,
      args: List[EirFunctionArgument],
      retTy: EirResolvable[EirType]
  ): EirMember = {
    val m = EirMember(Some(parent), null, EirAccessibility.Public)
    m.annotations +:= EirAnnotation("system", Map())
    val f = EirFunction(Some(m), None, name, Nil, Nil, Nil, retTy, None)
    m.member = f
    args.foreach(_.parent = Some(f))
    f.functionArgs = args
    m
  }

  @tailrec
  def onLeftSide(assignment: EirAssignment, seek: EirNode): Boolean = {
    seek.parent match {
      case Some(parent) =>
        if (parent == assignment) seek == assignment.lval
        else onLeftSide(assignment, parent)
      case None => false
    }
  }

  def makeMemberFunction(
      parent: EirClassLike,
      name: String,
      args: List[EirResolvable[EirType]],
      retTy: EirResolvable[EirType]
  ): EirMember = {
    makeMemberFunctionWithArgs(
      parent,
      name,
      args.zipWithIndex.map({ case (value, i) =>
        EirFunctionArgument(
          None,
          s"x$i",
          value,
          isExpansion = false,
          isSelfAssigning = false
        )
      }),
      retTy
    )
  }

  def resolveToPair(
      resolvable: EirResolvable[EirType]
  )(implicit
      ctx: TypeCheckContext
  ): (EirClassLike, Option[EirSpecialization]) = {
    CheckTypes.visit(resolvable) match {
      case t @ EirTemplatedType(_, base, _) =>
        (Find.uniqueResolution[EirClassLike](base), Some(t))
      case c: EirClassLike => (c, None)
      case _               => Errors.unableToResolve(resolvable)
    }
  }

  // lazily sweep (immediately) inherited classes, applying specialization(s)
  def sweepInheritedFirst[T](
      ctx: TypeCheckContext,
      base: EirClassLike,
      f: (TypeCheckContext, EirClassLike) => Option[T]
  ): Option[T] = {
    sweepInherited(
      ctx,
      base,
      (ictx: TypeCheckContext, a: EirClassLike) => f(ictx, a).view
    ).headOption
  }

  private def pickHelper[A <: EirType: ClassTag](
      s: EirClassLike,
      sp: Option[EirSpecialization]
  ): A = {
    (s, sp) match {
      case (_, Some(a: A)) => a
      case (a: A, _)       => a
      case _               => ???
    }
  }

  def sweepInherited[A <: EirType: ClassTag, B](
      ctx: TypeCheckContext,
      base: A,
      f: (TypeCheckContext, A) => View[B]
  ): View[B] = {
    Find.asClassLike(base).inherited.view.map(resolveToPair(_)(ctx)).flatMap {
      case (a, None) => f(ctx, pickHelper[A](a, None))
      case (a, Some(sp)) =>
        val inner = new TypeCheckContext(Some(ctx))
        val spec = inner.specialize(a, sp)
        val found = f(inner, pickHelper[A](a, Some(spec)))
        inner.leave(spec)
        found
    }
  }

  def extractFunction(node: EirNode): Option[EirFunction] = {
    node match {
      case EirMember(_, f: EirFunction, _) => Some(f)
      case f: EirFunction                  => Some(f)
      case _                               => None
    }
  }

  def assertValid[T: ClassTag](value: Option[EirNode]): T = {
    value match {
      case Some(x: T) => x
      case Some(x)    => Errors.incorrectType(x, classTag[T])
      case None       => Errors.unreachable()
    }
  }

  def assertValid[T: ClassTag](value: EirNode): T = assertValid[T](Some(value))

  def visitAll[T](node: EirNode, f: EirNode => T): Seq[T] = {
    f(node) +: node.children
      .flatMap(n => {
        f(n) +: visitAll(n, f)
      })
      .toSeq
  }

  def applyOrFalse[T <: EirNode: ClassTag](
      function: T => Unit,
      value: EirNode
  ): Boolean = {
    value.isValid[T].map(function).isDefined
  }

  def validAccessibility(a: EirClassLike, b: EirClassLike): EirAccessibility = {
    if (a == b) EirAccessibility.Private
    else if (a.isDescendantOf(b)) EirAccessibility.Protected
    else EirAccessibility.Public
  }

  private def xCanAccessY(x: EirNode, y: EirNode): Boolean = {
    y match {
      case _: EirTemplateArgument                                     => Find.commonAncestor(x, y) == y.parent
      case m: EirMember if m.accessibility == EirAccessibility.Public => true
      case m: EirMember => (Find.parentOf[EirClassLike](x), y.parent) match {
          case (Some(xParent: EirClassLike), Some(yParent: EirClassLike)) =>
            val relationship = validAccessibility(xParent, yParent)
            EirAccessibility.compatible(relationship, m.accessibility)
          case _ => false
        }
      case _ => true
    }
  }

  // TODO implement this
  def deepCloneTree[T <: EirNode](node: T): T = node

  def isSystem(node: EirNode): Boolean = {
    node.parent
      .collect({ case m: EirMember => m })
      .getOrElse(node)
      .annotation("system")
      .isDefined
  }

  object EirUtilitySyntax {

    implicit class RichEirNode(node: EirNode) {
      def isSystem: Boolean = util.isSystem(node)

      def canAccess(other: EirNode): Boolean = xCanAccessY(node, other)

      def visitAll[T](f: EirNode => T): Seq[T] = util.visitAll(node, f)

      def isValid[T: ClassTag]: Option[T] = node match {
        case t: T => Some(t)
        case _    => None
      }

      def hasName(name: String): Boolean = {
        node match {
          case x: EirNamedNode => x.name == name
          case _               => false
        }
      }
    }

    implicit class RichOption(option: Option[_]) {
      def to[T: ClassTag]: Option[T] = option match {
        case Some(t: T) => Some(t)
        case _          => None
      }
    }

    implicit class RichIntOption(option: Option[Int]) {
      def >(other: Option[Int]): Boolean = option ++ other match {
        case List(x, y) => x > y
        case _          => false
      }
    }

    implicit class RichResolvableTypeIterable[A <: EirResolvable[
      EirType
    ]: ClassTag](
        types: Iterable[A]
    ) {
      def toTupleType(
          allowUnit: Boolean = false
      )(implicit parent: Option[EirNode]): EirResolvable[EirType] =
        types.toList match {
          case Nil =>
            if (allowUnit) globals.unitType
            else throw new RuntimeException("please use unit type")
          case element :: Nil => element
          case x              => TupleFactory(x)
        }
    }

    implicit class RichBoolean(boolean: Boolean) {
      def ifTrue[T](t: Iterable[T]): Iterable[T] = if (boolean) t else None

      def ifFalse[T](t: Iterable[T]): Iterable[T] = if (boolean) None else t

      def enclose[T](t: T): Option[T] = if (boolean) Option(t) else None
    }

  }

}
