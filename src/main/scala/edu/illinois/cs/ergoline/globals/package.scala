package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.{EirExpressionNode, EirLiteral, EirLiteralTypes, EirNode}

package object globals {
  def unitLiteral(parent : Option[EirNode]): EirExpressionNode = EirLiteral(parent, EirLiteralTypes.Unit, "()")
}
