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
import edu.illinois.cs.ergoline.util.Errors

package object globals {
  def unitLiteral(parent: Option[EirNode]): EirLiteral[_] =
    EirUnitLiteral()(parent)

  def trueLiteral(parent: Option[EirNode]): EirLiteral[_] =
    EirBooleanLiteral(value = true)(parent)

  var strict: Boolean = false
  var verbose: Boolean = false
  var enableInPlace: Boolean = true

  def isResumeThread(node: EirNode): Boolean = {
    (node.parent == ckModule) && (
      node match {
        case f: EirFunction => f.name == "resumeThread"
        case _              => false
      }
    )
  }

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

  def optionType: EirClass = {
    Find.namedChild[EirClass](ergolineModule, "option")
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

  // uhhh... what???
  val ergolineName = "ergoline"

  def unitSymbol(parent: Option[EirNode]): EirResolvable[EirType] = {
    // TODO make this more robust!
    EirSymbol[EirNamedType](parent, List("::", ergolineName, unitName))
  }

  def unitType: EirType = typeFor(unitName)

  def stringType: EirType = typeFor("string")

  def boolType: EirType = typeFor("bool")

  def shortType: EirType = typeFor("short")
  def integerType: EirType = typeFor("int")

  def ckModule: Option[EirScope] = Modules("ck", EirGlobalNamespace)
  def ergolineModule: Option[EirScope] =
    Modules(ergolineName, EirGlobalNamespace)

  private def typeFor(name: String): EirClassLike = {
    val candidates = ergolineModule
      .map(Find.child[EirNamedNode](_, withName(name)))
      .getOrElse(Nil)
      .toList
    candidates.collectFirst({
      case c: EirClassLike  => c
      case f: EirFileSymbol => Find.uniqueResolution[EirClassLike](f)
      case node             => Errors.incorrectType(node, classOf[EirClassLike])
    }) match {
      case Some(res) => res
      case None => Errors.unableToResolve(
          List(name),
          ergolineModule.getOrElse(Errors.unableToResolve(ergolineName))
        )
    }
  }

  def typeFor(literal: EirLiteral[_]): EirType =
    typeFor(literal.`type`) // typeFor(literal.`type`)
}
