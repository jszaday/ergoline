package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.literals.{
  EirBooleanLiteral,
  EirIntegerLiteral,
  EirLiteral,
  EirLiteralSymbol,
  EirLiteralTuple,
  EirLiteralType
}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirTupleType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirClassLike

import scala.annotation.tailrec

object StaticEvaluator {

  private def valueWithin(
      x: EirResolvable[_]
  )(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    evaluate(CheckTypes.visit(x) match {
      case x: EirConstantFacade => x.value
      case x: EirType           => EirLiteralType(x)(None)
    })
  }

  def evaluate(x: EirNode)(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    x match {
      case x: EirConstantFacade   => x.value
      case x: EirSymbol[_]        => valueWithin(x)
      case x: EirLiteralSymbol    => valueWithin(x.value)
      case x: EirLiteral[_]       => x
      case x: EirTupleExpression  => evaluate(x)
      case x: EirBinaryExpression => evaluate(x)
      case x: EirUnaryExpression  => evaluate(x)
      case x: EirArrayReference   => evaluate(x)
      case _                      => Errors.invalidConstExpr(x)
    }
  }

  def evaluate(
      x: EirArrayReference
  )(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    val (lval, rvals) = (evaluate(x.target), x.args.map(evaluate(_)))
    (lval, rvals) match {
      case (EirLiteralTuple(value), EirIntegerLiteral(idx) :: Nil)
          if 0 <= idx && idx < value.length =>
        value(idx)
      case (EirLiteralType(t: EirTupleType), EirIntegerLiteral(idx) :: Nil)
          if 0 <= idx && idx < t.children.length =>
        EirLiteralType(CheckTypes.visit(t.children(idx)))(None)
      case (value, EirIntegerLiteral(idx) :: Nil) if idx == 0 =>
        value
      case _ => Errors.invalidTupleIndices(lval, rvals)
    }
  }

  def evaluate(
      x: EirTupleExpression
  )(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    assert(x.children.size > 1)

    EirLiteralTuple(x.children.map(evaluate(_)).toList)(None)
  }

  def evaluate(
      expr: EirUnaryExpression
  )(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    (expr.op, evaluate(expr.rhs)) match {
      case ("!", EirBooleanLiteral(x)) => mkBoolLiteral(!x)
      case ("+", EirIntegerLiteral(x)) => mkIntLiteral(+x)
      case ("-", EirIntegerLiteral(x)) => mkIntLiteral(-x)
      case _                           => Errors.unknownOperator(expr, expr.op)
    }
  }

  def evaluate(
      expr: EirBinaryExpression
  )(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    (evaluate(expr.lhs), evaluate(expr.rhs)) match {
      case (EirIntegerLiteral(x), EirIntegerLiteral(y)) =>
        evaluateIntBinop(x, expr, y)
      case (EirBooleanLiteral(x), EirBooleanLiteral(y)) =>
        evaluateBoolBinop(x, expr, y)
      case (EirLiteralType(x), EirLiteralType(y)) =>
        evaluateTypeBinop(x, expr, y)
      case _ => Errors.unknownOperator(expr, expr.op)
    }
  }

  private def mkBoolLiteral(x: Boolean): EirLiteral[_] = {
    EirBooleanLiteral(x)(None)
  }

  private def mkIntLiteral(x: Int): EirLiteral[_] = {
    EirIntegerLiteral(x)(None)
  }

  def evaluateTypeBinop(
      x: EirType,
      expr: EirBinaryExpression,
      y: EirType
  ): EirLiteral[_] = {
    expr.op match {
      case "==" => mkBoolLiteral(x == y)
      case "!=" => mkBoolLiteral(x != y)
      case ">:" => mkBoolLiteral(TypeCheckContext.lowerBound(y, x))
      case "<:" => mkBoolLiteral(TypeCheckContext.upperBound(y, x))
      case _    => Errors.unknownOperator(expr, expr.op)
    }
  }

  private def evaluateBoolBinop(
      lval: Boolean,
      x: EirBinaryExpression,
      rval: Boolean
  ): EirLiteral[_] = {
    x.op match {
      case "&&" => mkBoolLiteral(lval && rval)
      case "||" => mkBoolLiteral(lval && rval)
      case "==" => mkBoolLiteral(lval == rval)
      case "!=" => mkBoolLiteral(lval != rval)
      case _    => Errors.unknownOperator(x, x.op)
    }
  }

  private def evaluateIntBinop(
      lval: Int,
      x: EirBinaryExpression,
      rval: Int
  ): EirLiteral[_] = {
    x.op match {
      case "+"  => mkIntLiteral(lval + rval)
      case "-"  => mkIntLiteral(lval - rval)
      case "*"  => mkIntLiteral(lval * rval)
      case "/"  => mkIntLiteral(lval / rval)
      case "%"  => mkIntLiteral(lval % rval)
      case "==" => mkBoolLiteral(lval == rval)
      case "!=" => mkBoolLiteral(lval != rval)
      case "<=" => mkBoolLiteral(lval <= rval)
      case "<"  => mkBoolLiteral(lval < rval)
      case ">"  => mkBoolLiteral(lval > rval)
      case ">=" => mkBoolLiteral(lval >= rval)
      case _    => Errors.unknownOperator(x, x.op)
    }
  }

}
