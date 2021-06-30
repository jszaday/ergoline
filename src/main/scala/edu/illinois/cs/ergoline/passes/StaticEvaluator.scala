package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.literals.{
  EirBooleanLiteral,
  EirIntegerLiteral,
  EirLiteral
}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.Errors

import scala.annotation.tailrec

object StaticEvaluator {

  private def valueWithin(
      x: EirResolvable[_]
  )(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    Find.uniqueResolution[EirNode](x) match {
      case x: EirTemplateArgument => evaluate(CheckTypes.visit(x))
      case x                      => evaluate(x)
    }
  }

  def evaluate(x: EirNode)(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    x match {
      case x: EirConstantFacade   => x.value
      case x: EirSymbol[_]        => valueWithin(x)
      case x: EirLiteral[_]       => x
      case x: EirBinaryExpression => evaluate(x)
      case _                      => Errors.invalidConstExpr(x)
    }
  }

  def evaluate(
      x: EirBinaryExpression
  )(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    val (lval, rval) = (evaluate(x.lhs), evaluate(x.rhs))
    val (lty, rty) = (CheckTypes.visit(lval), CheckTypes.visit(rval))
    val (boolTy, intTy) = (globals.boolType, globals.integerType)

    Find.unionType(lty, rty) match {
      case Some(ty) if ty == intTy  => evaluateIntBinop(lval, x, rval)
      case Some(ty) if ty == boolTy => evaluateBoolBinop(lval, x, rval)
      case _                        => Errors.unableToUnify(x, lty, rty)
    }
  }

  private def mkBoolLiteral(x: Boolean): EirLiteral[_] = {
    EirBooleanLiteral(x)(None)
  }

  private def mkIntLiteral(x: Int): EirLiteral[_] = {
    EirIntegerLiteral(x)(None)
  }

  private def evaluateBoolBinop(
      lval: EirLiteral[_],
      x: EirBinaryExpression,
      rval: EirLiteral[_]
  ): EirLiteral[_] = {
    x.op match {
      case "&&" => mkBoolLiteral(lval.toBoolean && rval.toBoolean)
      case "||" => mkBoolLiteral(lval.toBoolean && rval.toBoolean)
      case "==" => mkBoolLiteral(lval.toBoolean == rval.toBoolean)
      case "!=" => mkBoolLiteral(lval.toBoolean != rval.toBoolean)
      case _    => Errors.unknownOperator(x, x.op)
    }
  }

  private def evaluateIntBinop(
      lval: EirLiteral[_],
      x: EirBinaryExpression,
      rval: EirLiteral[_]
  ): EirLiteral[_] = {
    x.op match {
      case "+"  => mkIntLiteral(lval.toInt + rval.toInt)
      case "-"  => mkIntLiteral(lval.toInt - rval.toInt)
      case "*"  => mkIntLiteral(lval.toInt * rval.toInt)
      case "/"  => mkIntLiteral(lval.toInt / rval.toInt)
      case "%"  => mkIntLiteral(lval.toInt % rval.toInt)
      case "==" => mkBoolLiteral(lval.toInt == rval.toInt)
      case "!=" => mkBoolLiteral(lval.toInt != rval.toInt)
      case "<=" => mkBoolLiteral(lval.toInt <= rval.toInt)
      case "<"  => mkBoolLiteral(lval.toInt < rval.toInt)
      case ">"  => mkBoolLiteral(lval.toInt > rval.toInt)
      case ">=" => mkBoolLiteral(lval.toInt >= rval.toInt)
      case _    => Errors.unknownOperator(x, x.op)
    }
  }

}
