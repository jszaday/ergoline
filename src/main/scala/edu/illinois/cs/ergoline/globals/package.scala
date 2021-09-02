package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.literals.{
  EirBooleanLiteral,
  EirLiteral,
  EirUnitLiteral
}
import edu.illinois.cs.ergoline.ast.types.{EirNamedType, EirType}
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find, Modules}

package object globals {
  def unitLiteral(parent: Option[EirNode]): EirLiteral[_] =
    EirUnitLiteral()(parent)

  def trueLiteral(parent: Option[EirNode]): EirLiteral[_] =
    EirBooleanLiteral(value = true)(parent)

  var strict: Boolean = false
  var verbose: Boolean = false
  var enableInPlace: Boolean = true

  def clearGlobals(): Unit = {
    EirGlobalNamespace.clear()
    Modules.loadedFiles.clear()
  }

  val operatorNames: Map[Char, String] = Map(
    '!' -> "not",
    '+' -> "plus",
    '-' -> "minus",
    '*' -> "times",
    '=' -> "equals",
    '<' -> "less",
    '>' -> "greater",
    '%' -> "mod",
    '/' -> "div"
  )

  def encodeOperator(op: String): String = {
    if (op == "[]") {
      "at"
    } else if (op == "==") {
      operatorNames('=')
    } else {
      (operatorNames(op.head) +: op.tail.toList.map(c =>
        operatorNames(c).capitalize
      )).mkString("")
    }
  }

  val unaryPrefix: String = "unary_"

  def prefixOperatorMethod(op: String): String = {
    unaryPrefix + op
  }

  val spaceshipOperator: String = "<=>"

  val equalityOperators: Seq[String] = Seq(
    "!=",
    "=="
  )

  val comparisonOperators: Seq[String] = equalityOperators ++ Seq(
    "<",
    "<=",
    ">=",
    ">"
  )

  def hasEqualityComparator(c: EirClassLike): Option[String] = {
    equalityOperators.find(c.hasMember)
  }

  def isEqualityComparator(op: String): Boolean = equalityOperators.contains(op)

  def isComparisonOperator(op: String): Boolean =
    comparisonOperators.contains(op)

  def isIdentityComparator(op: String): Boolean = op == "===" || op == "!=="

  val implicitProxyName: String = "__proxy__"

  def iterableType: EirTrait = {
    Find.namedChild[EirTrait](ergolineModule, "iterable")
  }

  def iteratorType: EirTrait = {
    Find.namedChild[EirTrait](ergolineModule, "iterator")
  }

  def objectType: EirType = {
    Find.namedChild[EirClassLike](ergolineModule, "object")
  }

  def rangeType: EirType = {
    Find.namedChild[EirClassLike](ergolineModule, "range")
  }

  def arrayType: EirType = {
    Find.namedChild[EirClassLike](ergolineModule, "array")
  }

  def futureType: EirType = {
    Find.namedChild[EirClassLike](ckModule, "future")
  }

  def proxyType: EirType = {
    Find.namedChild[EirClassLike](ckModule, "proxy")
  }

  def unitName: String = "unit"

  def unitSymbol: EirResolvable[EirType] =
    EirSymbol[EirNamedType](ergolineModule, List(unitName))

  def unitType: EirType = typeFor(unitName)

  def stringType: EirType = typeFor("string")

  def boolType: EirType = typeFor("bool")

  def shortType: EirType = typeFor("short")
  def integerType: EirType = typeFor("int")

  def ckModule: Option[EirScope] = Modules("ck", EirGlobalNamespace)
  def ergolineModule: Option[EirScope] = Modules("ergoline", EirGlobalNamespace)

  private def typeFor(name: String): EirType = {
    this.ergolineModule
      .flatMap(Find.child[EirNamedNode](_, withName(name)).headOption)
      .collect({
        case f: EirFileSymbol => Find.uniqueResolution[EirClassLike](f)
        case c: EirClassLike  => c
      })
      .getOrElse(throw new RuntimeException(s"could not find type $name"))
  }

  def typeFor(literal: EirLiteral[_]): EirType =
    typeFor(literal.`type`) // typeFor(literal.`type`)
}
