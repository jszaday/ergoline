package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast.{
  EirBinaryExpression,
  EirExpressionNode,
  EirNamedNode,
  EirNode,
  EirSymbol,
  EirTemplateArgument
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
      x: EirBinaryExpression
  )(implicit ctx: CodeGenerationContext): Unit = {
    x.op match {
      case "==" | "!=" =>
        ctx << Option.when(x.op == "!=")(
          "!"
        ) << "(std::is_same<" << x.lhs << "," << x.rhs << ">::value)"
    }
  }
}
