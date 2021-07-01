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
import edu.illinois.cs.ergoline.util.{Errors, assertValid}
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirClassLike

import scala.annotation.tailrec
import scala.reflect.ClassTag

object StaticEvaluator {

  private def valueWithin(
      x: EirResolvable[_]
  )(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    val result = CheckTypes.visit(x)

    result match {
      case x: EirConstantFacade => x.value
      case x: EirType           => EirLiteralType(x)(None)
    }
  }

  def evaluateToType[T <: EirNode](
      x: T
  )(implicit ctx: TypeCheckContext): EirType = {
    val result = evaluate(x)

    result match {
      case EirLiteralType(ty) => ty
      case lit                => EirConstantFacade(lit)(None)
    }
  }

  def evaluate(x: EirNode)(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    x match {
      case x: EirConstantFacade   => evaluate(x.value)
      case x: EirSymbol[_]        => valueWithin(x)
      case x: EirLiteral[_]       => evaluate(x)
      case x: EirTupleExpression  => evaluate(x)
      case x: EirUnaryExpression  => evaluate(x)
      case x: EirBinaryExpression => evaluate(x)
      case x: EirTernaryOperator  => evaluate(x)
      case x: EirArrayReference   => evaluate(x)
      case _                      => Errors.invalidConstExpr(x)
    }
  }

  def evaluate(
      x: EirLiteral[_]
  )(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    x match {
      case x: EirLiteralSymbol => valueWithin(x.value)
      case _                   => x
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
      expr: EirTernaryOperator
  )(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    if (valueAs[Boolean](() => evaluate(expr.test))) {
      evaluate(expr.ifTrue)
    } else {
      evaluate(expr.ifFalse)
    }
  }

  def evaluate(
      expr: EirBinaryExpression
  )(implicit ctx: TypeCheckContext): EirLiteral[_] = {
    val lhs = evaluate(expr.lhs)
    val rhs = () => evaluate(expr.rhs)

    lhs match {
      case EirIntegerLiteral(x) =>
        evaluateIntBinop(x, expr, valueAs[Int](rhs))
      case EirBooleanLiteral(x) =>
        evaluateBoolBinop(x, expr, rhs)
      case EirLiteralType(x) =>
        evaluateTypeBinop(x, expr, valueAs[EirType](rhs))
      case _ => Errors.unknownOperator(expr, expr.op)
    }
  }

  private def mkBoolLiteral(x: Boolean): EirLiteral[_] = {
    EirBooleanLiteral(x)(None)
  }

  private def mkIntLiteral(x: Int): EirLiteral[_] = {
    EirIntegerLiteral(x)(None)
  }

  def valueAs[A: ClassTag](rhs: () => EirLiteral[_]): A = {
    assertValid[EirLiteral[A]](rhs()).value
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
      rhs: () => EirLiteral[_]
  ): EirLiteral[_] = {
    x.op match {
      case "&&" =>
        mkBoolLiteral(if (lval) {
          valueAs[Boolean](rhs)
        } else false)
      case "||" =>
        mkBoolLiteral(
          if (lval) true
          else {
            valueAs[Boolean](rhs)
          }
        )
      case "==" =>
        mkBoolLiteral(lval == valueAs[Boolean](rhs))
      case "!=" =>
        mkBoolLiteral(lval != valueAs[Boolean](rhs))
      case _ => Errors.unknownOperator(x, x.op)
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
