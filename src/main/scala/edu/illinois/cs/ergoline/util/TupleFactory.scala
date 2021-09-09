package edu.illinois.cs.ergoline.util

import edu.illinois.cs.ergoline.ast.types.{EirTupleType, EirType}
import edu.illinois.cs.ergoline.resolution.EirResolvable

import scala.reflect.{ClassTag, classTag}

object TupleFactory {

  private var _tuples: Map[List[EirType], EirTupleType] = Map()

  def apply(tys: Iterable[EirType]): EirTupleType = {
    val ts = tys.toList
    _tuples.getOrElse(
      ts, {
        val u = EirTupleType(None, ts)
        _tuples += (ts -> u)
        u
      }
    )
  }

  def apply[A <: EirResolvable[EirType]: ClassTag](
      tys: Iterable[A]
  ): EirTupleType = {
    if (isType[A]) {
      apply(tys.map(_.asInstanceOf[EirType]))
    } else {
      EirTupleType(None, tys.toList)
    }
  }
}
