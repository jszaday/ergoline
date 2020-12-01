package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast.{EirClass, EirExpressionNode, EirFileSymbol, EirGlobalNamespace, EirLiteral, EirLiteralTypes, EirNamedNode, EirNode}
import edu.illinois.cs.ergoline.resolution.{Find, Modules}
import edu.illinois.cs.ergoline.resolution.Find.withName

package object globals {
  def unitLiteral(parent: Option[EirNode]): EirExpressionNode = EirLiteral(parent, EirLiteralTypes.Unit, "()")

  var strict: Boolean = false

  def clearGlobals(): Unit = {
    EirGlobalNamespace.clear()
    Modules.loadedFiles.clear()
  }

  val operators: Map[String, String] = Map(
    "+" -> "plus", "-" -> "minus", "*" -> "times", "==" -> "equals", ">=" -> "compareTo", ">" -> "compareTo", "<" -> "compareTo", "%" -> "rem"
  )

  def ergolineModule: Option[EirNamedNode] = Modules("ergoline", EirGlobalNamespace)

  def operatorToFunction(op : String): Option[String] = {
    Option.when(operators.contains(op))(operators(op))
  }

  def typeFor(litTy: EirLiteralTypes.Value): EirType = {
    val name : String = litTy.toString.toLowerCase
    this.ergolineModule.flatMap(Find.child[EirNamedNode](_, withName(name)).headOption)
      .collect({
        case f: EirFileSymbol => f.resolve().head.asInstanceOf[EirClass]
        case c: EirClass => c
      })
      .getOrElse(throw new RuntimeException(s"could not find type of $litTy"))
  }

  def typeFor(literal: EirLiteral): EirType = typeFor(literal.`type`)
}
