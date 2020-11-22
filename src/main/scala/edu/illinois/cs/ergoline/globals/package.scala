package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast.{EirClass, EirExpressionNode, EirGlobalNamespace, EirLiteral, EirLiteralTypes, EirNamedNode, EirNode}
import edu.illinois.cs.ergoline.resolution.{Find, Modules}
import edu.illinois.cs.ergoline.resolution.Find.withName

package object globals {
  def unitLiteral(parent: Option[EirNode]): EirExpressionNode = EirLiteral(parent, EirLiteralTypes.Unit, "()")

  def clearGlobals(): Unit = {
    EirGlobalNamespace.clear()
    Modules.loadedFiles.clear()
  }

  val operators: Map[String, String] = Map(
    "+" -> "plus", "-" -> "minus", "*" -> "times", "==" -> "equals"
  )

  def ergolineModule: Option[EirNamedNode] = Modules("ergoline", EirGlobalNamespace)

  def operatorToFunction(op : String): Option[String] = {
    Option.when(operators.contains(op))(operators(op))
  }

  def typeOfLiteral(literal: EirLiteral): EirType = {
    val name : String = literal.`type`.toString.toLowerCase
    this.ergolineModule.flatMap(Find.child[EirClass](_, withName(name)).headOption)
      .getOrElse(throw new RuntimeException(s"could not find type of $literal"))
  }
}
