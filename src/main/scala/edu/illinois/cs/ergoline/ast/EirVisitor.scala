package edu.illinois.cs.ergoline.ast

trait EirVisitor[T] {
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
    }
  }

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