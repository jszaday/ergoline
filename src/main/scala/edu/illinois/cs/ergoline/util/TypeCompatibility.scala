package edu.illinois.cs.ergoline.util

import edu.illinois.cs.ergoline.ast.types.{EirTemplatedType, EirTupleType, EirType}
import edu.illinois.cs.ergoline.ast.{EirClassLike, EirConstantFacade}
import edu.illinois.cs.ergoline.passes.{CheckTypes, TypeCheckContext}
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.EirResolvable

object TypeCompatibility {

  private def checkSubclass(a: EirClassLike, b: EirClassLike)(implicit ctx: TypeCheckContext): Boolean = {
    a.inherited.map(CheckTypes.visit(_)).map({
      case t: EirTemplatedType => assertValid[EirClassLike](t.base)
      case c: EirClassLike => c
    }).collect({
      case c: EirClassLike => c
    }).exists(x => { x == b || checkSubclass(x, b) })
  }

  implicit class RichEirClassLike(self: EirClassLike) {
    def isDescendantOf(other: EirClassLike)(implicit ctx: TypeCheckContext): Boolean = {
      (self, other) match {
        case (a: EirProxy, b: EirProxy) =>
          (a.isElement == b.isElement) &&
            (a.collective == b.collective) &&
            a.base.isDescendantOf(b.base)
        case _ => checkSubclass(self, other)
      }
    }
  }

  implicit class RichEirType(ours: EirType) {

    def canAssignTo(theirs: EirType)(implicit ctx: TypeCheckContext): Boolean = (ours == theirs) || ((ours, theirs) match {
      case (x: EirTemplatedType, y: EirTemplatedType) =>
        // TODO add checking for default arguments
        x.base.canAssignTo(y.base) && (x.args.length == y.args.length) && x.args.zip(y.args).forall{
          case (xx, yy) => xx.canAssignTo(yy)
        }
      case (x: EirClassLike, y: EirClassLike) => x.isDescendantOf(y)
      case (x: EirTupleType, y: EirTupleType) =>
        x.children.length == y.children.length &&
          x.children.zip(y.children).forall({
            case (a, b) => a.canAssignTo(b)
          })
      case (x: EirClassLike, y: EirTemplatedType) if x.templateArgs.isEmpty =>
        x.inherited.exists(_.canAssignTo(y))
      case (x: EirTemplatedType, y: EirClassLike) if y.templateArgs.isEmpty =>
        x.base.canAssignTo(y)
      case (x: EirConstantFacade, y: EirConstantFacade) => x.value.equivalentTo(y.value)
      case _ => false
    })
  }

  implicit class RichEirResolvable[T <: EirType](ours: EirResolvable[T]) {
    def canAssignTo(theirs: EirResolvable[T])(implicit ctx: TypeCheckContext): Boolean = {
      val a :: b :: Nil = List(ours, theirs).map {
        case t: EirType => t
        case t => CheckTypes.visit(t)
      }
      a.canAssignTo(b)
    }

    def canAssignTo(b: EirType)(implicit ctx: TypeCheckContext): Boolean = {
      ours match {
        case a: EirType => a.canAssignTo(b)
        case a => CheckTypes.visit(a).canAssignTo(b)
      }
    }
  }
}