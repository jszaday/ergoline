package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.types.Allowed
import edu.illinois.cs.ergoline.util.EirResolvable

package object types {

  type Allowed = Either[EirType, EirResolvable[EirType]]

  trait EirType {
    def name: String
    def canCastTo(other: EirType): Boolean
    override def toString: String = name
  }

  case class EirTupleType(var elements : List[Allowed]) extends EirType {
    def name: String = s"(${elements mkString ","})"

    override def canCastTo(other: EirType): Boolean = ???
  }

  case class EirLambdaType(var from: Allowed, var to: Allowed) extends EirType {
    def name: String = s"$from => $to"

    override def canCastTo(other: EirType): Boolean = ???
  }

  case class EirTemplatedType(var base : Allowed, var args : List[Allowed]) extends EirType {
    override def name: String = s"$base<${args mkString ","}>"

    override def canCastTo(other: EirType): Boolean = ???
  }

  case class EirProxyType(var base : Allowed, var collective : Option[String]) extends EirType {
    override def name: String = s"$base@${collective.getOrElse("")}"

    override def canCastTo(other: EirType): Boolean = ???
  }

  object EirUnitType extends EirType {
    override def name: String = "unit"

    override def canCastTo(other: EirType): Boolean = other == EirUnitType
  }

  object EirTupleType {
    def fromElements(elements: Iterable[Allowed]): Allowed = elements match {
      case Nil => Left(EirUnitType)
      case element :: Nil => element
      case _ => Left(EirTupleType(elements.toList))
    }
  }
}
