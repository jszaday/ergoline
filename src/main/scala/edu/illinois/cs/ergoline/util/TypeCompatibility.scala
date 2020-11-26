package edu.illinois.cs.ergoline.util

import edu.illinois.cs.ergoline.ast.EirClassLike
import edu.illinois.cs.ergoline.ast.types.EirType

object TypeCompatibility {
  implicit class RichEirType(ours: EirType) {
    def canAssignTo(theirs: EirType): Boolean = {
      (ours == theirs) || ((ours, theirs) match {
        case (x: EirClassLike, y: EirClassLike) => x.isDescendantOf(y)
        case _ => false
      })
    }
  }
}
