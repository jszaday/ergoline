package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.literals.EirLiteral
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.util.Errors.EirSubstitutionException

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
      case x: EirLiteral[_]       => GenerateCpp.visit(x)
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

  def staticType(
      x: EirExpressionNode
  )(implicit ctx: TypeCheckContext): Option[EirType] = {
    val before = ctx.staticTyping
    ctx.staticTyping = true
    val ty =
      try {
        Some(CheckTypes.visit(x))
      } catch {
        case _: EirSubstitutionException => None
      }
    ctx.staticTyping = before
    ty
  }

  def visit(
      x: EirBinaryExpression
  )(implicit ctx: CodeGenerationContext): Unit = {
    val lhsTy = staticType(x.lhs)(ctx.tyCtx)

    x.op match {
      case "==" | "!=" if !lhsTy.contains(globals.integerType) =>
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
      case op =>
        ctx << "(" << x.lhs << op << x.rhs << ")"
    }
  }
}
