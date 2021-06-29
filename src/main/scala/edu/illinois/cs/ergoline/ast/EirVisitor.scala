package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}

trait EirVisitor[Context, Value] {
  def error(x: EirNode)(implicit ctx: Context): Value = {
    throw new RuntimeException(s"unable to visit $x in context $ctx")
  }

  def visit[A <: EirNode](it: Iterable[A])(implicit
      ctx: Context
  ): Iterable[Value] = it.map(visit)

  def visit(node: EirNode)(implicit ctx: Context): Value = {
    node match {
      case x: EirType             => visitType(x)
      case x: EirPattern          => visitPattern(x)
      case x: EirSymbolLike[_]    => visitSymbolLike(x)
      case x: EirExpressionNode   => visitExpression(x)
      case x: EirBlock            => visitBlock(x)
      case x: EirNamespace        => visitNamespace(x)
      case x: EirDeclaration      => visitDeclaration(x)
      case x: EirMember           => visitMember(x)
      case x: EirFunction         => visitFunction(x)
      case x: EirAnnotation       => visitAnnotation(x)
      case x: EirFunctionArgument => visitFunctionArgument(x)
      case x: EirReturn           => visitReturn(x)
      case x: EirForLoop          => visitForLoop(x)
      case x: EirWhileLoop        => visitWhileLoop(x)
      case x: EirImport           => visitImport(x)
      case x: EirIfElse           => visitIfElse(x)
      case x: EirMatchCase        => visitMatchCase(x)
      case x: EirSdagWhen         => visitWhen(x)
      case x: EirAwaitMany        => visitAwaitMany(x)
      case x: EirResolvable[_] if x.resolved =>
        val found = Find.uniqueResolution[EirNode](x)
        if (found == x) error(found)
        else visit(found)
      case x: EirUserNode => x.accept(ctx, this)
      case null           => error(null)
      case x              => error(x)
    }
  }

  def visitType(x: EirType)(implicit ctx: Context): Value = {
    x match {
      case x: EirClass            => visitClass(x)
      case x: EirTrait            => visitTrait(x)
      case x: EirProxy            => visitProxy(x)
      case x: EirTypeAlias        => visitTypeAlias(x)
      case x: EirTupleType        => visitTupleType(x)
      case x: EirProxyType        => visitProxyType(x)
      case x: EirLambdaType       => visitLambdaType(x)
      case x: EirTupleMultiply    => visitTupleMultiply(x)
      case x: EirTemplatedType    => visitTemplatedType(x)
      case x: EirConstantFacade   => visitConstantFacade(x)
      case x: EirTemplateArgument => visitTemplateArgument(x)
      case _                      => error(x)
    }
  }

  def visitPattern(x: EirPattern)(implicit ctx: Context): Value = {
    x match {
      case x: EirPatternList       => visitPatternList(x)
      case x: EirExpressionPattern => visitExpressionPattern(x)
      case x: EirIdentifierPattern => visitIdentifierPattern(x)
      case _                       => error(x)
    }
  }

  def visitSymbolLike(
      x: EirSymbolLike[_ <: EirNode]
  )(implicit ctx: Context): Value = {
    x match {
      case x: EirSymbol[_]         => visitSymbol(x)
      case x: EirScopedSymbol[_]   => visitScopedSymbol(x)
      case x: EirSpecializedSymbol => visitSpecializedSymbol(x)
      case _                       => error(x)
    }
  }

  def visitExpression(node: EirExpressionNode)(implicit ctx: Context): Value = {
    node match {
      case x: EirSymbolLike[_] =>
        visitSymbolLike(
          x
        ) // <-- MAY BE TREATED AS TYPE ELSEWHERE. matched separately above
      case x: EirNew                => visitNew(x)
      case x: EirMatch              => visitMatch(x)
      case x: EirSlice              => visitSlice(x)
      case x: EirAwait              => visitAwait(x)
      case x: EirLiteral            => visitLiteral(x)
      case x: EirFunctionCall       => visitFunctionCall(x)
      case x: EirArrayReference     => visitArrayReference(x)
      case x: EirTupleExpression    => visitTupleExpression(x)
      case x: EirTernaryOperator    => visitTernaryOperator(x)
      case x: EirLambdaExpression   => visitLambdaExpression(x)
      case x: EirBinaryExpression   => visitBinaryExpression(x)
      case x: EirInterpolatedString => visitInterpolatedString(x)
      case x: EirCallArgument       => visitExpression(x.expr)
      case x: EirAssignment         => visitAssignment(x)
      case x                        => error(x)
    }
  }

  def visitSlice(x: EirSlice)(implicit ctx: Context): Value

  def visitWhen(x: EirSdagWhen)(implicit ctx: Context): Value
  def visitAwaitMany(x: EirAwaitMany)(implicit ctx: Context): Value

  def visitConstantFacade(x: EirConstantFacade)(implicit ctx: Context): Value
  def visitTupleMultiply(x: types.EirTupleMultiply)(implicit
      ctx: Context
  ): Value

  def visitTypeAlias(x: EirTypeAlias)(implicit ctx: Context): Value
  def visitInterpolatedString(x: EirInterpolatedString)(implicit
      ctx: Context
  ): Value
  def visitAwait(x: EirAwait)(implicit ctx: Context): Value
  def visitTupleType(x: EirTupleType)(implicit ctx: Context): Value

  def visitMatch(x: EirMatch)(implicit ctx: Context): Value

  def visitMatchCase(x: EirMatchCase)(implicit ctx: Context): Value

  def visitPatternList(x: EirPatternList)(implicit ctx: Context): Value
  def visitIdentifierPattern(x: EirIdentifierPattern)(implicit
      ctx: Context
  ): Value
  def visitExpressionPattern(x: EirExpressionPattern)(implicit
      ctx: Context
  ): Value

  def visitNew(x: EirNew)(implicit ctx: Context): Value

  def visitIfElse(x: EirIfElse)(implicit ctx: Context): Value

  def visitProxy(x: EirProxy)(implicit ctx: Context): Value

//  def visitDefault(x: EirNode)(implicit ctx: Context): Value

//  def visitGlobalNamespace(ctx: Context, x: Value
  def visitSpecializedSymbol(x: EirSpecializedSymbol)(implicit
      ctx: Context
  ): Value

  def visitArrayReference(x: EirArrayReference)(implicit ctx: Context): Value

  def visitScopedSymbol[A <: EirNode](x: EirScopedSymbol[A])(implicit
      ctx: Context
  ): Value

  def visitTernaryOperator(x: EirTernaryOperator)(implicit ctx: Context): Value

  def visitLambdaType(x: EirLambdaType)(implicit ctx: Context): Value

  def visitTemplatedType(x: EirTemplatedType)(implicit ctx: Context): Value

  def visitProxyType(x: EirProxyType)(implicit ctx: Context): Value

  def visitImport(x: EirImport)(implicit ctx: Context): Value

  def visitFunctionCall(x: EirFunctionCall)(implicit ctx: Context): Value

  def visitWhileLoop(x: EirWhileLoop)(implicit ctx: Context): Value
  def visitForLoop(x: EirForLoop)(implicit ctx: Context): Value

  def visitLiteral(x: EirLiteral)(implicit ctx: Context): Value

  def visitSymbol[A <: EirNamedNode](x: EirSymbol[A])(implicit
      ctx: Context
  ): Value

  def visitBlock(x: EirBlock)(implicit ctx: Context): Value

  def visitNamespace(x: EirNamespace)(implicit ctx: Context): Value

  def visitDeclaration(x: EirDeclaration)(implicit ctx: Context): Value

  def visitTemplateArgument(x: EirTemplateArgument)(implicit
      ctx: Context
  ): Value

  def visitClass(x: EirClass)(implicit ctx: Context): Value

  def visitTrait(x: EirTrait)(implicit ctx: Context): Value

  def visitMember(x: EirMember)(implicit ctx: Context): Value

  def visitFunction(x: EirFunction)(implicit ctx: Context): Value

  def visitAnnotation(x: EirAnnotation)(implicit ctx: Context): Value

  def visitBinaryExpression(x: EirBinaryExpression)(implicit
      ctx: Context
  ): Value

  def visitFunctionArgument(x: EirFunctionArgument)(implicit
      ctx: Context
  ): Value

  def visitAssignment(x: EirAssignment)(implicit ctx: Context): Value

  def visitTupleExpression(x: EirTupleExpression)(implicit ctx: Context): Value

  def visitLambdaExpression(x: EirLambdaExpression)(implicit
      ctx: Context
  ): Value

  def visitReturn(x: EirReturn)(implicit ctx: Context): Value
}
