package edu.illinois.cs.ergoline.util

import edu.illinois.cs.ergoline.ast.EirClassLike
import edu.illinois.cs.ergoline.ast.types.{EirTemplatedType, EirTupleType, EirType}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}

object TypeCompatibility {
  implicit class RichEirType(ours: EirType) {
    def canAssignTo(theirs: EirType): Boolean = (ours == theirs) || ((ours, theirs) match {
      case (x: EirTemplatedType, y: EirTemplatedType) => {
        x.base.canAssignTo(y.base) && (x.args.length == y.args.length) && x.args.zip(y.args).forall{
          case (xx, yy) => xx.canAssignTo(yy)
        }
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
        Find.uniqueResolution(x.base).canAssignTo(y)
      case _ => false
    })
  }

  implicit class RichEirResolvable[T <: EirType](ours: EirResolvable[T]) {
    def canAssignTo(theirs: EirResolvable[T]): Boolean = {
      Find.uniqueResolution(ours).canAssignTo(Find.uniqueResolution(theirs))
    }

    def canAssignTo(eirType: EirType): Boolean = {
      Find.uniqueResolution(ours).canAssignTo(eirType)
    }
  }
}
