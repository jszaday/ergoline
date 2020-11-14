package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.{EirExpressionNode, EirScope}
import edu.illinois.cs.ergoline.types.EirType

class EirForwardedExpression(expression : EirExpressionNode) extends EirResolvable[EirType] {
  override def resolve(scope: EirScope): EirType = expression.eirType.resolve(scope)
}
