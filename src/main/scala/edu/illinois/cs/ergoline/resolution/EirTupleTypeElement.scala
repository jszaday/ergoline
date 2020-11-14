package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.EirScope
import edu.illinois.cs.ergoline.types.{EirTupleType, EirType}

class EirTupleTypeElement(resolvable: EirResolvable[EirType], element : Int) extends EirResolvable[EirType] {
  override def resolve(scope: EirScope): EirType = resolvable.resolve(scope) match {
    case x : EirTupleType => x.elements(element) match {
      case x : EirType if !x.needsResolution => x
      case _ => x.resolve(scope)
    }
  }
}
