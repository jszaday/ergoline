package edu.illinois.cs.ergoline.util

import edu.illinois.cs.ergoline.ast.types.{
  EirLambdaType,
  EirTemplatedType,
  EirTupleType,
  EirType
}
import edu.illinois.cs.ergoline.ast.{
  EirClass,
  EirClassLike,
  EirConstantFacade,
  EirFunction,
  EirMember,
  EirTrait
}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.TypeCheckContext.ExpressionScope
import edu.illinois.cs.ergoline.passes.{CheckTypes, TypeCheckContext}
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}

import scala.reflect.ClassTag

object TypeCompatibility {

  private def checkSubclass(a: EirClassLike, b: EirClassLike): Boolean = {
    a.inherited
      .map(Find.uniqueResolution[EirType])
      .map({
        case t: EirTemplatedType =>
          assertValid[EirClassLike](Find.uniqueResolution[EirType](t.base))
        case c: EirClassLike => c
      })
      .collect({ case c: EirClassLike => c })
      .exists(x => { x == b || checkSubclass(x, b) })
  }

  implicit class RichEirClassLike(self: EirClassLike) {
    def isDescendantOf(other: EirClassLike): Boolean = {
      (self, other) match {
        case (a: EirProxy, b: EirProxy) => (a.isElement == b.isElement) &&
            (a.collective == b.collective) &&
            a.base.isDescendantOf(b.base)
        case _ => checkSubclass(self, other)
      }
    }

    def isValueType: Boolean = {
      self match {
        case x: EirClass => x.valueType
        case _: EirProxy => true
        case _           => false
      }
    }
  }

  private def canAssignHelper(ours: EirTemplatedType, theirs: EirTemplatedType)(
      implicit ctx: TypeCheckContext
  ): Boolean = {
    val (ourBase, theirBase) =
      (Find.uniqueResolution[EirType](ours.base), theirs.base)
    if (ourBase == theirBase) {
      ours.args.zip(theirs.args) forall { case (a, b) =>
        a == b // TODO add support for contravariance
      }
    } else {
      (ourBase, theirBase) match {
        case (_: EirClassLike, b: EirTrait) =>
          val impl = Find.implementationOf(ours, b)(ctx)
          impl.exists({
            case impl: EirTemplatedType => canAssignHelper(impl, theirs)
            case _                      => ???
          })
        case _ => ???
      }
    }
  }

  private def canAssignHelper(ours: EirType, theirs: EirLambdaType)(implicit
      ctx: TypeCheckContext
  ): Boolean = {
    CheckTypes.findApply[EirMember](
      ExpressionScope(None, None),
      ours,
      Some(ours)
    ) exists {
      case (_, t: EirLambdaType) => t.canAssignTo(theirs)
      case _                     => false
    }
  }

  implicit class RichEirType(ours: EirType) {

    def canAssignTo(theirs: EirType)(implicit ctx: TypeCheckContext): Boolean =
      (ours == theirs) || ((ours, theirs) match {
        case (a: EirLambdaType, b: EirLambdaType) =>
          // TODO make this more robust!
          (a.templateArgs == b.templateArgs) && a.to.canAssignTo(b.to) && {
            val (x, y) = (
              a.from.map(assertValid[EirType]),
              b.from.map(assertValid[EirType])
            )
            if ((x.isEmpty && y.length == 1) || (y.isEmpty && x.length == 1)) {
              x.headOption.orElse(y.headOption).contains(globals.unitType)
            } else {
              CheckTypes.argumentsMatch(x, y)
            }
          }
        case (x: EirTemplatedType, y: EirTemplatedType) => canAssignHelper(x, y)
        case (_: EirClassLike | _: EirTemplatedType, y: EirLambdaType) =>
          canAssignHelper(ours, y)
        case (x: EirClassLike, y: EirClassLike) => x.isDescendantOf(y)
        case (x: EirTupleType, y: EirTupleType) =>
          x.children.length == y.children.length &&
            x.children
              .zip(y.children)
              .forall({ case (a, b) => a.canAssignTo(b) })
        case (x: EirClassLike, y: EirTemplatedType) if x.templateArgs.isEmpty =>
          x.inherited.map(CheckTypes.visit(_)).exists(_.canAssignTo(y))
        case (x: EirTemplatedType, y: EirClassLike) if y.templateArgs.isEmpty =>
          x.base.canAssignTo(y)
        case (x: EirConstantFacade, y: EirConstantFacade) =>
          x.value.equals(y.value)
        case _ => false
      })
  }

  implicit class RichEirResolvable[T <: EirType: ClassTag](
      ours: EirResolvable[T]
  ) {
    // A resolver must be able to resolve an unspecialized specializable for it to be used here!!
    // For example, EirSymbol(..., "foo") must resolve to foo even if when it's declared as ``trait foo<A> ...``
    def canAssignTo(
        theirs: EirResolvable[T]
    )(implicit ctx: TypeCheckContext): Boolean = {
      CheckTypes.visit(ours).canAssignTo(CheckTypes.visit(theirs))
    }

    def canAssignTo(b: EirType)(implicit ctx: TypeCheckContext): Boolean = {
      CheckTypes.visit(ours).canAssignTo(b)
    }
  }
}
