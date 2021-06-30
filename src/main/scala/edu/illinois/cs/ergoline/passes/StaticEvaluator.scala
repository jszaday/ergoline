package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.Errors

import scala.annotation.tailrec

object StaticEvaluator {

  private def valueWithin(
      x: EirResolvable[_]
  )(implicit ctx: TypeCheckContext): EirLiteral = {
    Find.uniqueResolution[EirNode](x) match {
      case x: EirTemplateArgument => evaluate(CheckTypes.visit(x))
      case x                      => evaluate(x)
    }
  }

  def evaluate(x: EirNode)(implicit ctx: TypeCheckContext): EirLiteral = {
    x match {
      case x: EirConstantFacade => x.value
      case x: EirSymbol[_]      => valueWithin(x)
      case x: EirLiteral        => x
      case _                    => Errors.invalidConstExpr(x)
    }
  }
}
