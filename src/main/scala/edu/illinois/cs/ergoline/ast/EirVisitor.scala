package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.resolution.EirResolvable

trait EirVisitor[T] {
  def error(node : EirNode): T = {
    throw new RuntimeException(s"could not visit item of unknown type: ${if (node == null) node else node.getClass.getSimpleName}")
  }

  def visit(it: Iterable[EirNode]): Iterable[T] = it.map(visit)

  def visit(node: EirNode): T = {
    node match {
      case x: EirBlock => visitBlock(x)
      case x: EirNamespace => visitNamespace(x)
      case x: EirDeclaration => visitDeclaration(x)
      case x: EirTemplateArgument => visitTemplateArgument(x)
      case x: EirClass => visitClass(x)
      case x: EirTrait => visitTrait(x)
      case x: EirMember => visitMember(x)
      case x: EirFunction => visitFunction(x)
      case x: EirAnnotation => visitAnnotation(x)
      case x: EirBinaryExpression => visitBinaryExpression(x)
      case x: EirFunctionArgument => visitFunctionArgument(x)
      case x: EirAssignment => visitAssignment(x)
      case x: EirTupleExpression => visitTupleExpression(x)
      case x: EirLambdaExpression => visitLambdaExpression(x)
      case x: EirReturn => visitReturn(x)
      case x: EirSymbol[_] => visitSymbol(x)
      case x: EirLiteral => visitLiteral(x)
      case x: EirForLoop => visitForLoop(x)
      case x: EirFunctionCall => visitFunctionCall(x)
      case x: EirImport => visitImport(x)
      case x: EirProxyType => visitProxyType(x)
      case x: EirTemplatedType => visitTemplatedType(x)
      case x: EirLambdaType => visitLambdaType(x)
      case x: EirTernaryOperator => visitTernaryOperator(x)
      case x: EirFieldAccessor => visitFieldAccessor(x)
      case x: EirArrayReference => visitArrayReference(x)
      case x: EirResolvable[_] if x.resolved => visit(x.resolve())
      case x: EirUserNode => x.accept(this)
      case null => error(null)
      case x => error(x)
    }
  }

//  def visitDefault(x: EirNode): T

//  def visitGlobalNamespace(): T

  def visitArrayReference(x: EirArrayReference): T

  def visitFieldAccessor(x: EirFieldAccessor): T

  def visitTernaryOperator(x: EirTernaryOperator): T

  def visitLambdaType(x: EirLambdaType): T

  def visitTemplatedType(x: EirTemplatedType): T

  def visitProxyType(x: EirProxyType): T

  def visitImport(eirImport: EirImport): T

  def visitFunctionCall(call: EirFunctionCall): T

  def visitForLoop(loop: EirForLoop): T

  def visitLiteral(value: EirLiteral): T

  def visitSymbol(value: EirSymbol[_]): T

  def visitBlock(node: EirBlock): T

  def visitNamespace(node: EirNamespace): T

  def visitDeclaration(node: EirDeclaration): T

  def visitTemplateArgument(node: EirTemplateArgument): T

  def visitClass(node: EirClass): T

  def visitTrait(node: EirTrait): T

  def visitMember(node: EirMember): T

  def visitFunction(node: EirFunction): T

  def visitAnnotation(node: EirAnnotation): T

  def visitBinaryExpression(node: EirBinaryExpression): T

  def visitFunctionArgument(node: EirFunctionArgument): T

  def visitAssignment(node: EirAssignment): T

  def visitTupleExpression(node: EirTupleExpression): T

  def visitLambdaExpression(node: EirLambdaExpression): T

  def visitReturn(node: EirReturn): T
}
