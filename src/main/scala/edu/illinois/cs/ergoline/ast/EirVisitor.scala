package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}

trait EirVisitor[Context, Value] {
  def error(ctx: Context, node : EirNode): Value = {
    throw new RuntimeException(s"unable to visit $node in context $ctx")
  }

  def visit(ctx: Context, it: Iterable[EirNode]): Iterable[Value] = it.map(visit(ctx, _))

  def visit(ctx: Context, node: EirNode): Value = {
    node match {
      case x: EirSpecializedSymbol => visitSpecializedSymbol(ctx, x)
      case x: EirBlock => visitBlock(ctx, x)
      case x: EirNamespace => visitNamespace(ctx, x)
      case x: EirDeclaration => visitDeclaration(ctx, x)
      case x: EirTemplateArgument => visitTemplateArgument(ctx, x)
      case x: EirClass => visitClass(ctx, x)
      case x: EirTrait => visitTrait(ctx, x)
      case x: EirMember => visitMember(ctx, x)
      case x: EirFunction => visitFunction(ctx, x)
      case x: EirAnnotation => visitAnnotation(ctx, x)
      case x: EirBinaryExpression => visitBinaryExpression(ctx, x)
      case x: EirFunctionArgument => visitFunctionArgument(ctx, x)
      case x: EirAssignment => visitAssignment(ctx, x)
      case x: EirTupleExpression => visitTupleExpression(ctx, x)
      case x: EirLambdaExpression => visitLambdaExpression(ctx, x)
      case x: EirReturn => visitReturn(ctx, x)
      case x: EirSymbol[_] => visitSymbol(ctx, x)
      case x: EirLiteral => visitLiteral(ctx, x)
      case x: EirForLoop => visitForLoop(ctx, x)
      case x: EirWhileLoop => visitWhileLoop(ctx, x)
      case x: EirFunctionCall => visitFunctionCall(ctx, x)
      case x: EirImport => visitImport(ctx, x)
      case x: EirTupleType => visitTupleType(ctx, x)
      case x: EirProxyType => visitProxyType(ctx, x)
      case x: EirTemplatedType => visitTemplatedType(ctx, x)
      case x: EirLambdaType => visitLambdaType(ctx, x)
      case x: EirTernaryOperator => visitTernaryOperator(ctx, x)
      case x: EirFieldAccessor => visitFieldAccessor(ctx, x)
      case x: EirArrayReference => visitArrayReference(ctx, x)
      case x: EirIfElse => visitIfElse(ctx, x)
      case x: EirNew => visitNew(ctx, x)
      case x: EirProxy => visitProxy(ctx, x)
      case x: EirMatch => visitMatch(ctx, x)
      case x: EirMatchCase => visitMatchCase(ctx, x)
      case x: EirPatternList => visitPatternList(ctx, x)
      case x: EirIdentifierPattern => visitIdentifierPattern(ctx, x)
      case x: EirExpressionPattern => visitExpressionPattern(ctx, x)
      case x: EirInterpolatedString => visitInterpolatedString(ctx, x)
      case x: EirTupleMultiply => visitTupleMultiply(ctx, x)
      case x: EirConstantFacade => visitConstantFacade(ctx, x)
      case x: EirResolvable[_] if x.resolved => {
        val found = Find.uniqueResolution(x)
        if (found == x) error(ctx, found)
        else visit(ctx, found)
      }
      case x: EirAwait => visitAwait(ctx, x)
      case x: EirUserNode => x.accept(ctx, this)
      case null => error(ctx, null)
      case x: EirTypeAlias => visitTypeAlias(ctx, x)
      case x => error(ctx, x)
    }
  }

  def visitConstantFacade(context: Context, facade: EirConstantFacade): Value
  def visitTupleMultiply(context: Context, multiply: types.EirTupleMultiply): Value

  def visitTypeAlias(ctx: Context, x: EirTypeAlias): Value
  def visitInterpolatedString(ctx: Context, x: EirInterpolatedString): Value
  def visitAwait(ctx: Context, x: EirAwait): Value
  def visitTupleType(ctx: Context, x: EirTupleType): Value

  def visitMatch(ctx: Context, x: EirMatch): Value

  def visitMatchCase(ctx: Context, x: EirMatchCase): Value

  def visitPatternList(ctx: Context, x: EirPatternList): Value
  def visitIdentifierPattern(ctx: Context, x: EirIdentifierPattern): Value
  def visitExpressionPattern(ctx: Context, x: EirExpressionPattern): Value

  def visitNew(ctx: Context, x: EirNew): Value

  def visitIfElse(ctx: Context, x: EirIfElse): Value

  def visitProxy(ctx: Context, x: EirProxy): Value

//  def visitDefault(ctx: Context, x: EirNode): Value

//  def visitGlobalNamespace(ctx: Context, x: Value
  def visitSpecializedSymbol(ctx: Context, x: EirSpecializedSymbol): Value

  def visitArrayReference(ctx: Context, x: EirArrayReference): Value

  def visitFieldAccessor(ctx: Context, x: EirFieldAccessor): Value

  def visitTernaryOperator(ctx: Context, x: EirTernaryOperator): Value

  def visitLambdaType(ctx: Context, x: EirLambdaType): Value

  def visitTemplatedType(ctx: Context, x: EirTemplatedType): Value

  def visitProxyType(ctx: Context, x: EirProxyType): Value

  def visitImport(ctx: Context, x: EirImport): Value

  def visitFunctionCall(ctx: Context, x: EirFunctionCall): Value

  def visitWhileLoop(ctx: Context, x: EirWhileLoop): Value
  def visitForLoop(ctx: Context, x: EirForLoop): Value

  def visitLiteral(ctx: Context, x: EirLiteral): Value

  def visitSymbol[A <: EirNamedNode](ctx: Context, x: EirSymbol[A]): Value

  def visitBlock(ctx: Context, x: EirBlock): Value

  def visitNamespace(ctx: Context, x: EirNamespace): Value

  def visitDeclaration(ctx: Context, x: EirDeclaration): Value

  def visitTemplateArgument(ctx: Context, x: EirTemplateArgument): Value

  def visitClass(ctx: Context, x: EirClass): Value

  def visitTrait(ctx: Context, x: EirTrait): Value

  def visitMember(ctx: Context, x: EirMember): Value

  def visitFunction(ctx: Context, x: EirFunction): Value

  def visitAnnotation(ctx: Context, x: EirAnnotation): Value

  def visitBinaryExpression(ctx: Context, x: EirBinaryExpression): Value

  def visitFunctionArgument(ctx: Context, x: EirFunctionArgument): Value

  def visitAssignment(ctx: Context, x: EirAssignment): Value

  def visitTupleExpression(ctx: Context, x: EirTupleExpression): Value

  def visitLambdaExpression(ctx: Context, x: EirLambdaExpression): Value

  def visitReturn(ctx: Context, x: EirReturn): Value
}
