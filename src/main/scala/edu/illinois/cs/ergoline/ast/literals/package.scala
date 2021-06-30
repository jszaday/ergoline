package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.util.Errors

import scala.reflect.ClassTag
import scala.util.matching.Regex

package object literals {
  abstract class EirLiteral[A: ClassTag] extends EirExpressionNode {
    def value_=(a: A): Unit

    def value: A

    def `type`: String

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

    val valuePattern: Regex = "[-:a-zA-Z0-9_\\.\\/]+".r

    override def strip(): String = {
      val quoteless = value.substring(1, value.length - 1)
      if ((quoteless == "[]") || valuePattern.matches(quoteless)) quoteless
      else super.strip()
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
    override def `type`: String = "unit"
  }

  //  val String: Value = Value("string")
  //  val Integer: Value = Value("int")
  //  val Float: Value = Value("float")
  //  val Character: Value = Value("char")
  //  val Unit: Value = Value("unit")
  //  val Boolean: Value = Value("bool")
}
