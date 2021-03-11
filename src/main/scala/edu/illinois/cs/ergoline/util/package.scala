package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.EirAccessibility.EirAccessibility
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.passes.TypeCheckContext
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirClassLike

import scala.collection.View
import scala.reflect.{ClassTag, classTag}

package object util {

  import EirUtilitySyntax.RichEirNode

  def addExplicitSelf(ctx: TypeCheckContext, base: EirType, ty: EirType): EirType = {
    ty match {
      case EirLambdaType(_, from, to, args) =>
        ctx.lambdaWith(base +: from, to, args)
      case _ => Errors.incorrectType(ty, classTag[EirLambdaType])
    }
  }

  def makeMemberFunction(parent: EirClassLike, name: String,
                         args: List[EirResolvable[EirType]],
                         retTy: EirResolvable[EirType], isConst: Boolean): EirMember = {
    val m = EirMember(Some(parent), null, EirAccessibility.Public)
    m.annotations +:= EirAnnotation("system", Map())
    m.member = EirFunction(Some(m), None, name, Nil, Nil, retTy)
    m.member.asInstanceOf[EirFunction].functionArgs = args.zipWithIndex.map({
      case (value, i) => EirFunctionArgument(Some(m.member), s"x$i", value, isExpansion = false, isSelfAssigning = false)
    })
    m
  }

  def resolveToPair(resolvable: EirResolvable[EirType]): (EirClassLike, Option[EirSpecialization]) = {
    Find.uniqueResolution[EirType](resolvable) match {
      case t@EirTemplatedType(_, base, _) => (Find.uniqueResolution[EirClassLike](base), Some(t))
      case c: EirClassLike => (c, None)
      case _ => Errors.unableToResolve(resolvable)
    }
  }

  // lazily sweep (immediately) inherited classes, applying specialization(s)
  def sweepInheritedFirst[T](ctx: TypeCheckContext, base: EirClassLike,
                        f: EirClassLike => Option[T]): Option[T] = {
    sweepInherited(ctx, base, (a: EirClassLike) => f(a).view).headOption
  }

  def sweepInherited[T](ctx: TypeCheckContext, base: EirClassLike,
                        f: EirClassLike => View[T]): View[T] = {
    base.inherited.view.map(resolveToPair).flatMap {
      case (a, None) => f(a)
      case (a, Some(sp)) =>
        val spec = ctx.specialize(a, sp)
        val found = f(a)
        ctx.leave(spec)
        found
    }
  }

  def extractFunction(node: EirNode): Option[EirFunction] = {
    node match {
      case EirMember(_, f: EirFunction, _) => Some(f)
      case f: EirFunction => Some(f)
      case _ => None
    }
  }

  def assertValid[T : ClassTag](value: EirNode): T = {
    Option(value) match {
      case Some(x: T) => x
      case _ => Errors.incorrectType(value, classTag[T])
    }
  }

  def visitAll[T](node: EirNode, f: EirNode => T): Seq[T] = {
    f(node) +: node.children.flatMap(n => {
      f(n) +: visitAll(n, f)
    }).toSeq
  }

  def applyOrFalse[T <: EirNode : ClassTag](function: T => Unit, value: EirNode): Boolean = {
    value.isValid[T].map(function).isDefined
  }

  def validAccessibility(a: EirClassLike, b: EirClassLike): EirAccessibility = {
    if (a == b) EirAccessibility.Private
    else if (a.isDescendantOf(b)) EirAccessibility.Protected
    else EirAccessibility.Public
  }

  private def xCanAccessY(x: EirNode, y: EirNode): Boolean = {
    y match {
      case _ : EirTemplateArgument => Find.commonAncestor(x, y) == y.parent
      case m : EirMember if m.accessibility == EirAccessibility.Public => true
      case m : EirMember =>
        (Find.parentOf[EirClassLike](x), y.parent) match {
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

  object EirUtilitySyntax {

    implicit class RichEirNode(node: EirNode) {
      def canAccess(other: EirNode): Boolean = xCanAccessY(node, other)

      def visitAll[T](f: EirNode => T): Seq[T] = util.visitAll(node, f)

      def isValid[T : ClassTag]: Option[T] = node match {
        case t : T => Some(t)
        case _ => None
      }

      def hasName(name : String): Boolean = {
        node match {
          case x : EirNamedNode => x.name == name
          case _ => false
        }
      }
    }

    implicit class RichOption(option: Option[_]) {
      def to[T: ClassTag]: Option[T] = option match {
        case Some(t: T) => Some(t)
        case _ => None
      }
    }

    implicit class RichIntOption(option: Option[Int]) {
      def >(other: Option[Int]): Boolean = option ++ other match {
        case List(x, y) => x > y
        case _ => false
      }
    }

    implicit class RichResolvableTypeIterable(types: Iterable[EirResolvable[EirType]]) {
      def toTupleType(allowUnit: Boolean = false)(implicit parent: Option[EirNode]): EirResolvable[EirType] =
        types.toList match {
          case Nil => if (allowUnit) globals.unitType else throw new RuntimeException("please use unit type")
          case element :: Nil => element
          case x => EirTupleType(parent, x)
        }
    }

    implicit class RichBoolean(boolean: Boolean) {
      def ifTrue[T](t: Iterable[T]): Iterable[T] = if (boolean) t else None

      def ifFalse[T](t: Iterable[T]): Iterable[T] = if (boolean) None else t

      def enclose[T](t: T): Option[T] = if (boolean) Option(t) else None
    }

  }

}
