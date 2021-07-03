package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast.{
  EirBinaryExpression,
  EirExpressionNode,
  EirNamedNode,
  EirNode,
  EirSymbol,
  EirTemplateArgument,
  EirUnaryExpression
}

import scala.reflect.ClassTag

object StaticGenerator {
  implicit val visitor: (CodeGenerationContext, EirNode) => Unit =
    (ctx, x) => this.visit(x)(ctx)

  def visit(
      x: EirNode
  )(implicit ctx: CodeGenerationContext): Unit = {
    x match {
      case x: EirExpressionNode => visit(x)
    }
  }

  def visit(
      x: EirExpressionNode
  )(implicit ctx: CodeGenerationContext): Unit = {
    x match {
      case x: EirUnaryExpression  => visit(x)
      case x: EirBinaryExpression => visit(x)
      case x: EirSymbol[_]        => visit(x.asInstanceOf[EirSymbol[EirNamedNode]])
    }
  }

  def visit(
      x: EirSymbol[EirNamedNode]
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx.resolve(x) match {
      case _: EirTemplateArgument => GenerateCpp.visit(x)
      case t: EirType             => ctx << ctx.typeFor(t, Some(x))
    }
  }

  def visit(
      x: EirUnaryExpression
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << x.op << x.rhs
  }

  def visit(
      x: EirBinaryExpression
  )(implicit ctx: CodeGenerationContext): Unit = {
    x.op match {
      case "==" | "!=" =>
        ctx << Option.when(x.op == "!=")(
          "!"
        ) << "(std::is_same<" << x.lhs << "," << x.rhs << ">::value)"
      case "<:" | ">:" =>
        val flip = x.op == ">:"
        ctx << "std::is_base_of<ergoline::extricate_t<" << {
          if (flip) x.lhs else x.rhs
        } << ">,ergoline::extricate_t<" << {
          if (flip) x.rhs else x.lhs
        } << ">>::value"
    }
  }
}
