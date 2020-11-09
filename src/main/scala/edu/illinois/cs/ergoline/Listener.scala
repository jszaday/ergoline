package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ErgolineListener
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ErrorNode, TerminalNode}

class Listener extends ErgolineListener {
  /**
   * Enter a parse tree produced by {@link ErgolineParser# program}.
   *
   * @param ctx the parse tree
   */
  override def enterProgram(ctx: ErgolineParser.ProgramContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# program}.
   *
   * @param ctx the parse tree
   */
  override def exitProgram(ctx: ErgolineParser.ProgramContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# packageStatement}.
   *
   * @param ctx the parse tree
   */
  override def enterPackageStatement(ctx: ErgolineParser.PackageStatementContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# packageStatement}.
   *
   * @param ctx the parse tree
   */
  override def exitPackageStatement(ctx: ErgolineParser.PackageStatementContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# importStatement}.
   *
   * @param ctx the parse tree
   */
  override def enterImportStatement(ctx: ErgolineParser.ImportStatementContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# importStatement}.
   *
   * @param ctx the parse tree
   */
  override def exitImportStatement(ctx: ErgolineParser.ImportStatementContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# statement}.
   *
   * @param ctx the parse tree
   */
  override def enterStatement(ctx: ErgolineParser.StatementContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# statement}.
   *
   * @param ctx the parse tree
   */
  override def exitStatement(ctx: ErgolineParser.StatementContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# forLoop}.
   *
   * @param ctx the parse tree
   */
  override def enterForLoop(ctx: ErgolineParser.ForLoopContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# forLoop}.
   *
   * @param ctx the parse tree
   */
  override def exitForLoop(ctx: ErgolineParser.ForLoopContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# returnStatement}.
   *
   * @param ctx the parse tree
   */
  override def enterReturnStatement(ctx: ErgolineParser.ReturnStatementContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# returnStatement}.
   *
   * @param ctx the parse tree
   */
  override def exitReturnStatement(ctx: ErgolineParser.ReturnStatementContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# block}.
   *
   * @param ctx the parse tree
   */
  override def enterBlock(ctx: ErgolineParser.BlockContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# block}.
   *
   * @param ctx the parse tree
   */
  override def exitBlock(ctx: ErgolineParser.BlockContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# templateDeclArg}.
   *
   * @param ctx the parse tree
   */
  override def enterTemplateDeclArg(ctx: ErgolineParser.TemplateDeclArgContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# templateDeclArg}.
   *
   * @param ctx the parse tree
   */
  override def exitTemplateDeclArg(ctx: ErgolineParser.TemplateDeclArgContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# templateDecl}.
   *
   * @param ctx the parse tree
   */
  override def enterTemplateDecl(ctx: ErgolineParser.TemplateDeclContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# templateDecl}.
   *
   * @param ctx the parse tree
   */
  override def exitTemplateDecl(ctx: ErgolineParser.TemplateDeclContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# accessModifier}.
   *
   * @param ctx the parse tree
   */
  override def enterAccessModifier(ctx: ErgolineParser.AccessModifierContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# accessModifier}.
   *
   * @param ctx the parse tree
   */
  override def exitAccessModifier(ctx: ErgolineParser.AccessModifierContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# inheritanceDecl}.
   *
   * @param ctx the parse tree
   */
  override def enterInheritanceDecl(ctx: ErgolineParser.InheritanceDeclContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# inheritanceDecl}.
   *
   * @param ctx the parse tree
   */
  override def exitInheritanceDecl(ctx: ErgolineParser.InheritanceDeclContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# classDeclaration}.
   *
   * @param ctx the parse tree
   */
  override def enterClassDeclaration(ctx: ErgolineParser.ClassDeclarationContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# classDeclaration}.
   *
   * @param ctx the parse tree
   */
  override def exitClassDeclaration(ctx: ErgolineParser.ClassDeclarationContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# annotatedMember}.
   *
   * @param ctx the parse tree
   */
  override def enterAnnotatedMember(ctx: ErgolineParser.AnnotatedMemberContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# annotatedMember}.
   *
   * @param ctx the parse tree
   */
  override def exitAnnotatedMember(ctx: ErgolineParser.AnnotatedMemberContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# member}.
   *
   * @param ctx the parse tree
   */
  override def enterMember(ctx: ErgolineParser.MemberContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# member}.
   *
   * @param ctx the parse tree
   */
  override def exitMember(ctx: ErgolineParser.MemberContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# namespace}.
   *
   * @param ctx the parse tree
   */
  override def enterNamespace(ctx: ErgolineParser.NamespaceContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# namespace}.
   *
   * @param ctx the parse tree
   */
  override def exitNamespace(ctx: ErgolineParser.NamespaceContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# fqn}.
   *
   * @param ctx the parse tree
   */
  override def enterFqn(ctx: ErgolineParser.FqnContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# fqn}.
   *
   * @param ctx the parse tree
   */
  override def exitFqn(ctx: ErgolineParser.FqnContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# valueDeclaration}.
   *
   * @param ctx the parse tree
   */
  override def enterValueDeclaration(ctx: ErgolineParser.ValueDeclarationContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# valueDeclaration}.
   *
   * @param ctx the parse tree
   */
  override def exitValueDeclaration(ctx: ErgolineParser.ValueDeclarationContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# variableDeclaration}.
   *
   * @param ctx the parse tree
   */
  override def enterVariableDeclaration(ctx: ErgolineParser.VariableDeclarationContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# variableDeclaration}.
   *
   * @param ctx the parse tree
   */
  override def exitVariableDeclaration(ctx: ErgolineParser.VariableDeclarationContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# fieldDeclaration}.
   *
   * @param ctx the parse tree
   */
  override def enterFieldDeclaration(ctx: ErgolineParser.FieldDeclarationContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# fieldDeclaration}.
   *
   * @param ctx the parse tree
   */
  override def exitFieldDeclaration(ctx: ErgolineParser.FieldDeclarationContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# function}.
   *
   * @param ctx the parse tree
   */
  override def enterFunction(ctx: ErgolineParser.FunctionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# function}.
   *
   * @param ctx the parse tree
   */
  override def exitFunction(ctx: ErgolineParser.FunctionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# functionArgument}.
   *
   * @param ctx the parse tree
   */
  override def enterFunctionArgument(ctx: ErgolineParser.FunctionArgumentContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# functionArgument}.
   *
   * @param ctx the parse tree
   */
  override def exitFunctionArgument(ctx: ErgolineParser.FunctionArgumentContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# functionArgumentList}.
   *
   * @param ctx the parse tree
   */
  override def enterFunctionArgumentList(ctx: ErgolineParser.FunctionArgumentListContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# functionArgumentList}.
   *
   * @param ctx the parse tree
   */
  override def exitFunctionArgumentList(ctx: ErgolineParser.FunctionArgumentListContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# primaryExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterPrimaryExpression(ctx: ErgolineParser.PrimaryExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# primaryExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitPrimaryExpression(ctx: ErgolineParser.PrimaryExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# lambdaExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterLambdaExpression(ctx: ErgolineParser.LambdaExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# lambdaExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitLambdaExpression(ctx: ErgolineParser.LambdaExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# postfixExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterPostfixExpression(ctx: ErgolineParser.PostfixExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# postfixExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitPostfixExpression(ctx: ErgolineParser.PostfixExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# argumentExpressionList}.
   *
   * @param ctx the parse tree
   */
  override def enterArgumentExpressionList(ctx: ErgolineParser.ArgumentExpressionListContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# argumentExpressionList}.
   *
   * @param ctx the parse tree
   */
  override def exitArgumentExpressionList(ctx: ErgolineParser.ArgumentExpressionListContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# unaryExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterUnaryExpression(ctx: ErgolineParser.UnaryExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# unaryExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitUnaryExpression(ctx: ErgolineParser.UnaryExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# unaryOperator}.
   *
   * @param ctx the parse tree
   */
  override def enterUnaryOperator(ctx: ErgolineParser.UnaryOperatorContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# unaryOperator}.
   *
   * @param ctx the parse tree
   */
  override def exitUnaryOperator(ctx: ErgolineParser.UnaryOperatorContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# castExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterCastExpression(ctx: ErgolineParser.CastExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# castExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitCastExpression(ctx: ErgolineParser.CastExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# multiplicativeExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterMultiplicativeExpression(ctx: ErgolineParser.MultiplicativeExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# multiplicativeExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitMultiplicativeExpression(ctx: ErgolineParser.MultiplicativeExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# additiveExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterAdditiveExpression(ctx: ErgolineParser.AdditiveExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# additiveExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitAdditiveExpression(ctx: ErgolineParser.AdditiveExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# shiftExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterShiftExpression(ctx: ErgolineParser.ShiftExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# shiftExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitShiftExpression(ctx: ErgolineParser.ShiftExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# relationalExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterRelationalExpression(ctx: ErgolineParser.RelationalExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# relationalExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitRelationalExpression(ctx: ErgolineParser.RelationalExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# equalityExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterEqualityExpression(ctx: ErgolineParser.EqualityExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# equalityExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitEqualityExpression(ctx: ErgolineParser.EqualityExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# andExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterAndExpression(ctx: ErgolineParser.AndExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# andExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitAndExpression(ctx: ErgolineParser.AndExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# exclusiveOrExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterExclusiveOrExpression(ctx: ErgolineParser.ExclusiveOrExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# exclusiveOrExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitExclusiveOrExpression(ctx: ErgolineParser.ExclusiveOrExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# inclusiveOrExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterInclusiveOrExpression(ctx: ErgolineParser.InclusiveOrExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# inclusiveOrExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitInclusiveOrExpression(ctx: ErgolineParser.InclusiveOrExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# logicalAndExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterLogicalAndExpression(ctx: ErgolineParser.LogicalAndExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# logicalAndExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitLogicalAndExpression(ctx: ErgolineParser.LogicalAndExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# logicalOrExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterLogicalOrExpression(ctx: ErgolineParser.LogicalOrExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# logicalOrExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitLogicalOrExpression(ctx: ErgolineParser.LogicalOrExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# conditionalExpression}.
   *
   * @param ctx the parse tree
   */
  override def enterConditionalExpression(ctx: ErgolineParser.ConditionalExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# conditionalExpression}.
   *
   * @param ctx the parse tree
   */
  override def exitConditionalExpression(ctx: ErgolineParser.ConditionalExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# expression}.
   *
   * @param ctx the parse tree
   */
  override def enterExpression(ctx: ErgolineParser.ExpressionContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# expression}.
   *
   * @param ctx the parse tree
   */
  override def exitExpression(ctx: ErgolineParser.ExpressionContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# typeList}.
   *
   * @param ctx the parse tree
   */
  override def enterTypeList(ctx: ErgolineParser.TypeListContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# typeList}.
   *
   * @param ctx the parse tree
   */
  override def exitTypeList(ctx: ErgolineParser.TypeListContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# tupleType}.
   *
   * @param ctx the parse tree
   */
  override def enterTupleType(ctx: ErgolineParser.TupleTypeContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# tupleType}.
   *
   * @param ctx the parse tree
   */
  override def exitTupleType(ctx: ErgolineParser.TupleTypeContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# basicType}.
   *
   * @param ctx the parse tree
   */
  override def enterBasicType(ctx: ErgolineParser.BasicTypeContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# basicType}.
   *
   * @param ctx the parse tree
   */
  override def exitBasicType(ctx: ErgolineParser.BasicTypeContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# lambdaType}.
   *
   * @param ctx the parse tree
   */
  override def enterLambdaType(ctx: ErgolineParser.LambdaTypeContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# lambdaType}.
   *
   * @param ctx the parse tree
   */
  override def exitLambdaType(ctx: ErgolineParser.LambdaTypeContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# type}.
   *
   * @param ctx the parse tree
   */
  override def enterType(ctx: ErgolineParser.TypeContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# type}.
   *
   * @param ctx the parse tree
   */
  override def exitType(ctx: ErgolineParser.TypeContext): Unit = ???

  /**
   * Enter a parse tree produced by {@link ErgolineParser# annotation}.
   *
   * @param ctx the parse tree
   */
  override def enterAnnotation(ctx: ErgolineParser.AnnotationContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link ErgolineParser# annotation}.
   *
   * @param ctx the parse tree
   */
  override def exitAnnotation(ctx: ErgolineParser.AnnotationContext): Unit = ???

  override def visitTerminal(node: TerminalNode): Unit = ???

  override def visitErrorNode(node: ErrorNode): Unit = ???

  override def enterEveryRule(ctx: ParserRuleContext): Unit = ???

  override def exitEveryRule(ctx: ParserRuleContext): Unit = ???
}
