package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.resolution.{Find, Modules}

package object globals {
  def unitLiteral(parent: Option[EirNode]): EirExpressionNode =
    EirLiteral(parent, EirLiteralTypes.Unit, "()")

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
    if (op == "==") {
      operatorNames('=')
    } else {
      (operatorNames(op.head) +: op.tail.toList.map(c =>
        operatorNames(c).capitalize
      )).mkString("")
    }
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

  def isEqualityComparator(op: String): Boolean =
    equalityOperators.contains(op)

  def hasEqualityComparator(c: EirClassLike): Option[String] = {
    equalityOperators.find(c.hasMember)
  }

  def isIdentityComparator(op: String): Boolean =
    op == "===" || op == "!=="

  def isComparisonOperator(op: String): Boolean =
    isEqualityComparator(op) || comparisonOperators.contains(op)

  val implicitProxyName: String = "__proxy__"

  def iterableType: EirType = {
    Find.namedChild[EirClassLike](ergolineModule, "iterable")
  }

  def iteratorType: EirType = {
    Find.namedChild[EirClassLike](ergolineModule, "iterator")
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

  def unitType: EirType = typeFor(EirLiteralTypes.Unit)

  def stringType: EirType = typeFor(EirLiteralTypes.String)

  def boolType: EirType = typeFor(EirLiteralTypes.Boolean)

  def ckModule: Option[EirNamedNode] = Modules("ck", EirGlobalNamespace)
  def ergolineModule: Option[EirNamedNode] =
    Modules("ergoline", EirGlobalNamespace)

  def typeFor(litTy: EirLiteralTypes.Value): EirType = {
    val name: String =
      if (litTy == EirLiteralTypes.Float) "double"
      else litTy.toString.toLowerCase
    this.ergolineModule
      .flatMap(Find.child[EirNamedNode](_, withName(name)).headOption)
      .collect({
        case f: EirFileSymbol => Find.uniqueResolution[EirClassLike](f)
        case c: EirClassLike  => c
      })
      .getOrElse(throw new RuntimeException(s"could not find type of $litTy"))
  }

  def typeFor(literal: EirLiteral): EirType = typeFor(literal.`type`)
}
