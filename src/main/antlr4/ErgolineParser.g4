parser grammar ErgolineParser;
options { tokenVocab=ErgolineLexer; }

program
    : packageStatement? annotatedTopLevelStatement+ EOF
    ;

annotatedTopLevelStatement
    :   annotation* (publicImport | topLevelStatement | namespace)
    ;

publicImport
    :   PublicKwd importStatement
    ;

topLevelStatement
    :   function
    |   usingStatement
    |   importStatement
    |   classDeclaration
    ;

packageStatement
    :   PackageKwd fqn Semicolon
    ;

importStatement
    :   ImportKwd fqn Semicolon
    ;

usingStatement
    :   UsingKwd identifier templateDecl? Equals staticExpression Semicolon
    ;

statement
    :   annotation* innerStatement
    ;

innerStatement
    :   block
    |   forLoop
    |   whileLoop
    |   ifThenElse
    |   matchStatement
    |   expression Semicolon
    |   returnStatement
    |   whenStatement
    |   awaitManyStatement
    |   topLevelStatement
    |   topLevelDeclaration
    ;

body
    :   statement | Semicolon
    ;

whenStatement
    :   WhenKwd whenFnList (IfKwd condition=expression)? LambdaArrow body
    ;

awaitManyStatement
    :   AwaitKwd (AnyKwd || AllKwd) LeftCurly (whenStatement+) RightCurly
    ;

whenFnList
    :   (whenFn Comma)* whenFn
    ;

whenFn
    :   identifierExpression LeftParen patternList? RightParen
    ;

patternList
    :   (pattern Comma)* pattern
    ;

pattern
    :   id=identifier (Colon basicType)?
    |   fn=identifierExpression? LeftParen patternList RightParen
    |   constant
    |   expression
    ;

caseStatement
    :   CaseKwd patternList (IfKwd condition=expression)? LambdaArrow body
    ;

matchStatement
    :   MatchKwd LeftParen expression RightParen LeftCurly caseStatement+ RightCurly
    ;

ifThenElse
    :   IfKwd LeftParen condition=expression RightParen ifTrue=body (ElseKwd ifFalse=body)?
    ;

identifier
    :   Less
    |   Equals
    |   Greater
    |   PrefixOp
    |   Identifier
    |   ExpansionOp
    |   LeftShift
    |   RightShift
    |   Ampersand
    |   WhereKwd
    ;

loopHeader
    :   decltype LeftArrow iter=expression
    |   (variableDeclaration? | Semicolon) test=expression? Semicolon incr=expression?
    ;

whileLoop
    :   WhileKwd LeftParen expression RightParen body
    ;

forLoop
    :   ForKwd LeftParen loopHeader RightParen body
    ;

returnStatement
    :   ReturnKwd expression? Semicolon
    ;

block
    :   LeftCurly statement* RightCurly
    ;

boundOperator
    :  LowerBound | UpperBound
    ;

templateDeclArg
    :   name=identifier ellipses=Ellipses? ((UpperBound upperBound=type)? (LowerBound lowerBound=type)? | Colon argTy=type?) (Equals specializationElement)?
    ;

templateDecl
    :   Less (templateDeclArg Comma)* templateDeclArg Greater
    ;

accessModifier
    :   PublicKwd | ProtectedKwd | PrivateKwd
    ;

inheritanceDecl
    :   (ExtendsKwd type)? (WithKwd type (WithKwd type)*)?
    ;

whereClause
    :   WhereKwd staticExpression
    ;

classKind
    :  (AbstractKwd? ClassKwd)
    |   ObjectKwd
    |   StructKwd
    |   TraitKwd
    ;

classDeclaration
    :   classKind identifier templateDecl? inheritanceDecl whereClause? LeftCurly annotatedMember* RightCurly
    ;

annotatedMember
    :   annotation* member
    ;

member
    :   accessModifier? (OverrideKwd | StaticKwd)? ( fieldDeclaration | topLevelStatement )
    ;

namespace
    :   NamespaceKwd fqn LeftCurly annotatedTopLevelStatement* RightCurly
    ;

fqn
    :   (identifier DoubleColon)* identifier
    ;

decltype
    :   Ampersand? identifier (Colon type)?
    |   LeftParen (decltype Comma)+ decltype RightParen
    ;

valueDeclaration
    :   ValueKeyword decltype Equals expression Semicolon
    ;

variableDeclaration
    :   VariableKeyword decltype (Equals expression)? Semicolon
    ;

fieldDeclaration
    :   (ValueKeyword | VariableKeyword) identifier Colon type (Equals expression)? Semicolon
    ;

topLevelDeclaration
    :   ImplicitKwd? (valueDeclaration | variableDeclaration)
    ;

basicArgument
    :   identifier Colon ExpansionOp? type
    ;

implicitArgument
    :   ImplicitKwd basicArgument
    ;

implicitArguments
    :   LeftParen (implicitArgument Comma)* implicitArgument RightParen
    ;

functionIdentifier
    :   SelfKwd | EmptySquare | identifier
    ;

function
    :   FunctionKwd functionIdentifier templateDecl? LeftParen functionArgumentList? RightParen implicitArguments? (Colon type)? whereClause? (Semicolon | block)
    ;

functionArgument
    :   (Ampersand | Equals)? basicArgument
    ;

functionArgumentList
    :   (functionArgument Comma)* functionArgument
    ;

expressionList
    :   (expression Comma)* expression
    ;

identifierExpression
    :   fqn Less qualEndSpecList identifierExpression
    |   fqn specialization?
    ;

proxySelfExpression
    :   SelfKwd suffix=(Atpersand | ProxySuffix)
    ;

selfExpression
    :   proxySelfExpression
    |   SelfKwd
    ;

primaryExpression
    :   identifierExpression
    |   interpolatedString
    |   selfExpression
    |   constant
    |   tupleExpression
    |   lambdaExpression
    ;

tupleExpression
    :   LeftParen expressionList? RightParen
    ;

lambdaExpression
    :   LeftParen functionArgumentList? RightParen LambdaArrow (block | expression)
    ;

sliceExpression
    :   start=expression? Colon (step=expression Colon)? end=expression?
    |   single=expression
    ;

sliceExpressionList
    :   (sliceExpression Comma)* sliceExpression
    ;

callArgument
    :   Ampersand? expression
    ;

callArgumentList
    :   (callArgument Comma)* callArgument
    ;

postfixExpression
    :   proxySelfExpression identifier  // proxies have the highest precedence
    |   primaryExpression
    |   postfixExpression LeftSquare arrArgs=sliceExpressionList RightSquare
    |   postfixExpression specialization? LeftParen fnArgs=callArgumentList? RightParen
    |   postfixExpression (Dot | RightArrow) identifier
    ;

awaitExpression
    :    AwaitKwd postfixExpression
    ;

newExpression
    :   NewKwd type tupleExpression?
    ;

simpleExpression
    :   postfixExpression
    |   newExpression
    |   awaitExpression
    ;

unaryExpression
    :   PrefixOp? simpleExpression
    ;

conditionalExpression
    :   unaryExpression (Question expression Colon conditionalExpression)?
    ;

infixExpression
    :   conditionalExpression
    |   infixExpression identifier infixExpression
    ;

expression
    :   matchStatement
    |   infixExpression
    ;

typeList
    :   (type Comma)* type
    ;

staticPrimaryExpression
    :   type
    |   constant
    |   staticTupleExpression
    ;

staticExpressionList
    :   (staticExpression Comma)* staticExpression
    ;

staticTupleExpression
    :   LeftParen staticExpressionList RightParen
    ;

staticPostfixExpression
    :   staticPrimaryExpression
    |   staticPostfixExpression LeftSquare staticExpressionList RightSquare
    ;

staticPrefixExpression
    :   PrefixOp? staticPostfixExpression
    ;

staticConditionalExpression
    :   staticPrefixExpression (Question staticExpression Colon staticConditionalExpression)?
    ;

staticExpression
    :   staticConditionalExpression
    |   staticExpression (boundOperator | identifier) staticExpression
    ;

tupleMultiply
    :   Dot ExpansionOp
    ;

tupleType
    :   LeftParen typeList RightParen
    |   tupleType multiply=tupleMultiply staticPrimaryExpression
    ;

specializationElement
    :   type
    |   constant
    ;

startSpecList
    :   (specializationElement Comma)* specializationElement
    ;

endSpecList
    :   init=startSpecList Greater
    |   init=startSpecList Less last=startSpecList RightShift
    ;

qualEndSpecList
    :   init=startSpecList QualSpecGreaterEnd
    |   init=startSpecList Less last=startSpecList QualSpecShiftEnd
    ;

specialization
    :   Less endSpecList
    ;

proxySuffix
    :   Atpersand
    |   prefix=(Atpersand | ProxySuffix) CollectiveKwd
    ;

templateType
    :   identifierExpression
    ;

proxyType
    :   templateType proxySuffix?
    ;

basicType
    :   proxyType | tupleType
    ;

lambdaType
    :   head=basicType LambdaArrow tail=basicType
    ;

type
    :   proxyType
    |   tupleType
    |   lambdaType
    |   type (Ampersand | Ellipses)
    ;

annotation
    :   Atpersand identifier annotationOptions?
    ;

annotationIdentifier
    :   identifier
    |   StaticKwd
    |   CollectiveKwd
    ;

annotationOption
    :   annotationIdentifier (Equals constant)? // TODO make staticExpression
    ;

annotationOptions
    :   LeftParen (annotationOption Comma)* annotationOption RightParen
    ;

interpolatedString
    :   IStringLiteral
    ;

boolLiteral
    :    TrueKwd | FalseKwd
    ;

constant
    :   IntegerConstant
    |   FloatingConstant
    |   CharacterConstant
    |   StringLiteral+
    |   boolLiteral
    ;
