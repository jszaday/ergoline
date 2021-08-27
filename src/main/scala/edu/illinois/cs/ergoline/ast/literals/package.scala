package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.resolution.EirResolvable
import edu.illinois.cs.ergoline.util.Errors

import scala.reflect.ClassTag
import scala.util.matching.Regex

package object literals {
  abstract class EirLiteral[A: ClassTag] extends EirExpressionNode {
    def value_=(a: A): Unit

    def value: A

    def `type`: String

    override def equals(other: Any): Boolean = {
      other match {
        case x: EirLiteral[_] => this.equals(x)
        case _                => false
      }
    }

    def equals(other: EirLiteral[_]): Boolean = {
      other match {
        case x: EirLiteral[A] => this.value == x.value
        case _                => false
      }
    }

    def toInt: Int = Errors.cannotCast(this, `type`, globals.integerType)

    def toBoolean: Boolean = Errors.cannotCast(this, `type`, globals.boolType)

    override def toString: String = value.toString

    def strip(): String = Errors.cannotStrip(this)

    override def children: Iterable[EirNode] = Nil

    override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean =
      false
  }

  case class EirIntegerLiteral(var value: Int)(
      override var parent: Option[EirNode]
  ) extends EirLiteral[Int] {
    override def `type`: String = "int"

    override def toInt: Int = value
  }

  case class EirFloatLiteral(var value: Float)(
      override var parent: Option[EirNode]
  ) extends EirLiteral[Float] {
    override def `type`: String = "double"
  }

  case class EirStringLiteral(var value: String)(
      override var parent: Option[EirNode]
  ) extends EirLiteral[String] {
    override def `type`: String = "string"

    override def strip(): String = {
      val (head, last) = (value.headOption, value.lastOption)
      (head, last) match {
        case (Some('"'), Some('"')) => value.substring(1, value.length - 1)
        case _                      => super.strip()
      }
    }
  }

  case class EirBooleanLiteral(var value: Boolean)(
      override var parent: Option[EirNode]
  ) extends EirLiteral[Boolean] {
    override def `type`: String = "bool"

    override def toBoolean: Boolean = value
  }

  case class EirCharacterLiteral(var value: Char)(
      override var parent: Option[EirNode]
  ) extends EirLiteral[Char] {
    override def `type`: String = "char"
  }

  case class EirUnitLiteral(var value: Unit = ())(
      override var parent: Option[EirNode]
  ) extends EirLiteral[Unit] {
    override def `type`: String = globals.unitName
  }

  case class EirLiteralTuple(var value: List[EirLiteral[_]])(
      override var parent: Option[EirNode]
  ) extends EirLiteral[List[EirLiteral[_]]] {
    override def children: Iterable[EirNode] = value

    override def `type`: String = s"(${value.map(_.`type`) mkString ","})"
  }

  case class EirLiteralSymbol(var value: EirResolvable[EirNode])(
      override var parent: Option[EirNode]
  ) extends EirLiteral[EirResolvable[EirNode]] {
    override def children: Iterable[EirNode] = Seq(value)

    override def `type`: String = value.toString
  }

  case class EirLiteralType(var value: EirType)(
      override var parent: Option[EirNode]
  ) extends EirLiteral[EirType] {
    override def children: Iterable[EirNode] = Seq(value)

    override def `type`: String = value.toString
  }
}
