grammar Ergoline;

program
    : packageStatement? annotatedTopLevelStatement+ EOF
    ;

annotatedTopLevelStatement
    :   annotation* (topLevelStatement | namespace)
    ;

topLevelStatement
    :   function
    |   usingStatement
    |   importStatement
    |   classDeclaration
    ;

packageStatement
    :   'package' fqn ';'
    ;

importStatement
    :   'import' fqn ';'
    ;

usingStatement
    :   'using' identifier templateDecl? Equals staticExpression ';'
    ;

statement
    :   annotation* innerStatement
    ;

innerStatement
    :   block
    |   forLoop
    |   whileLoop
    |   function
    |   ifThenElse
    |   matchStatement
    |   expression ';'
    |   returnStatement
    |   classDeclaration
    |   topLevelDeclaration
    |   whenStatement
    |   awaitManyStatement
    ;

whenStatement
    :   'when' whenFnList ('if' condition=expression)? LambdaArrow (block | bodyExpr=expression ';')
    ;

awaitManyStatement
    :   AwaitKwd (AnyKwd || AllKwd) '{' (whenStatement+) '}'
    ;

whenFnList
    :   (whenFn ',')* whenFn
    ;

whenFn
    :   identifierExpression '(' patternList? ')'
    ;

patternList
    :   (pattern ',')* pattern
    ;

pattern
    :   identifier (':' (tupleType | basicType)?)?
    |   constant
    |   expression
    |   '(' patternList ')'
    ;

caseStatement
    :   'case' patternList ('if' condition=expression)? LambdaArrow bodyExpression=expression? ';'
    ;

matchStatement
    :   'match' '(' expression ')' '{' caseStatement+ '}'
    ;

ifThenElse
    :   'if' '(' condition=expression ')' (';' | ifTrue=statement) ('else' ifFalse=statement)?
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
    |   WhereKwd
    ;

identifierList
    :   (identifier ',')* identifier
    ;

loopHeader
    :   (identifierList | '(' identifierList ')') '<-' iter=expression
    |   (variableDeclaration? | ';') test=expression? ';' incr=expression?
    ;

whileLoop
    :   'while' '(' expression ')' (block | statement)
    ;

forLoop
    :   'for' '(' loopHeader ')' (block | statement)
    ;

returnStatement
    :   'return' expression? ';'
    ;

block
    :   '{' statement* '}'
    ;

LowerBound : '>:' ;
UpperBound : '<:' ;

boundOperator
    :   LowerBound | UpperBound
    ;

templateDeclArg
    :   name=identifier ellipses=Ellipses? ((UpperBound upperBound=type)? (LowerBound lowerBound=type)? | ':' argTy=type?) (Equals specializationElement)?
    ;

templateDecl
    :   Less (templateDeclArg ',')* templateDeclArg Greater
    ;

accessModifier
    :   'public' | 'protected' | 'private'
    ;

inheritanceDecl
    :   ('extends' type)? ('with' type ('and' type)*)?
    ;

whereClause
    :   WhereKwd staticExpression
    ;

classDeclaration
    :   ((AbstractKwd? ClassKwd) | StructKwd | TraitKwd) identifier templateDecl? inheritanceDecl whereClause? '{' annotatedMember* '}'
    ;

annotatedMember
    :   annotation* member
    ;

member
    :   accessModifier? (OverrideKwd | StaticKwd)? ( fieldDeclaration | topLevelStatement )
    ;

namespace
    :   'namespace' fqn '{' annotatedTopLevelStatement* '}'
    ;

fqn
    :   (identifier '::')* identifier
    ;

valueDeclaration
    :   ValueKeyword identifier (':' type)? Equals expression ';'
    ;

variableDeclaration
    :   VariableKeyword identifier (':' type)? (Equals expression)? ';'
    ;

fieldDeclaration
    :   (ValueKeyword | VariableKeyword) identifier ':' type (Equals expression)? ';'
    ;

topLevelDeclaration
    :   ImplicitKwd? (valueDeclaration | variableDeclaration)
    ;

basicArgument
    :   identifier ':' ExpansionOp? type
    ;

implicitArgument
    :   ImplicitKwd basicArgument
    ;

implicitArguments
    :   '(' (implicitArgument ',')* implicitArgument ')'
    ;

function
    :   FunctionKwd identifier templateDecl? '(' functionArgumentList? ')' implicitArguments? (':' type)? whereClause? (';' | block)
    ;

functionArgument
    :   (Ampersand | Equals)? basicArgument
    ;

functionArgumentList
    :   (functionArgument ',')* functionArgument
    ;

expressionList
    :   (expression ',')* expression
    ;

identifierExpression
    :   fqn Less qualEndSpecList identifierExpression
    |   fqn specialization?
    ;

selfExpression
    :   SelfKeyword
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
    :   '(' expressionList? ')'
    ;

LambdaArrow
    :   '=>'
    ;

lambdaExpression
    :   '(' functionArgumentList? ')' LambdaArrow (block | expression)
    ;

sliceExpression
    :   start=expression? ':' (step=expression ':')? end=expression?
    |   single=expression
    ;

sliceExpressionList
    :   (sliceExpression ',')* sliceExpression
    ;

callArgument
    :   '&'? expression
    ;

callArgumentList
    :   (callArgument ',')* callArgument
    ;

postfixExpression
    :   selfExpression identifier
    |   primaryExpression
    |   postfixExpression '[' arrArgs=sliceExpressionList ']'
    |   postfixExpression specialization? LParen fnArgs=callArgumentList? RParen
    |   postfixExpression '.' identifier
    ;

awaitExpression
    :    AwaitKwd postfixExpression
    ;

newExpression
    :   'new' type tupleExpression?
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
    :   unaryExpression ('?' expression ':' conditionalExpression)?
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
    :   (type ',')* type
    ;

staticPrimaryExpression
    :   type
    |   constant
    |   staticTupleExpression
    ;

staticExpressionList
    :   (staticExpression ',')* staticExpression
    ;

staticTupleExpression
    :   '(' staticExpressionList ')'
    ;

staticPostfixExpression
    :   staticPrimaryExpression
    |   staticPostfixExpression '[' staticExpressionList ']'
    ;

staticPrefixExpression
    :   PrefixOp? staticPostfixExpression
    ;

staticConditionalExpression
    :   staticPrefixExpression ('?' staticExpression ':' staticConditionalExpression)?
    ;

staticExpression
    :   staticConditionalExpression
    |   staticExpression (boundOperator | identifier) staticExpression
    ;

tupleType
    :   '(' typeList ')'
    |   tupleType multiply='.*' staticPrimaryExpression
    ;

specializationElement
    :   type
    |   constant
    ;

startSpecList
    :   (specializationElement ',')* specializationElement
    ;

endSpecList
    :   init=startSpecList Greater
    |   init=startSpecList Less last=startSpecList RightShift
    ;

qualEndSpecList
    :   init=startSpecList '>::'
    |   init=startSpecList Less last=startSpecList '>>::'
    ;

specialization
    :   Less endSpecList
    ;

proxySuffix
    :    Atpersand
    |   (Atpersand | ProxySuffix) CollectiveKeyword
    ;

basicType
    :   fqn specialization? proxySuffix?
    |   fqn Ellipses
    ;

lambdaType
    :   (basicType | tupleType) '=>' (basicType | tupleType)
    ;

type
    :   basicType
    |   tupleType
    |   lambdaType
    ;

annotation
    :   Atpersand identifier annotationOptions?
    ;

annotationOption
    :   (StaticKwd | identifier) Equals constant
    ;

annotationOptions
    :   '(' (annotationOption ',')* annotationOption ')'
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

Ampersand
    :   '&'
    ;

Atpersand
    :   '@'
    ;

fragment
Element
    :   '[@]'
    ;

fragment
Section
    :   '{@}'
    ;

ProxySuffix
    :   Element | Section
    ;

SelfKeyword
    :   'self' ('@' | ProxySuffix)?
    ;

CollectiveKeyword
    :   'array' [1-6] 'd'
    |   'nodegroup'
    |   'group'
    ;

AwaitKwd : 'await';
AnyKwd : 'any';
AllKwd : 'all';

VariableKeyword : 'var' ;
ValueKeyword : 'val' ;
AbstractKwd : 'abstract';
OverrideKwd : 'override';
ClassKwd : 'class';
StructKwd : 'struct';
TraitKwd : 'trait';
FunctionKwd : 'def';
TrueKwd : 'true' ;
FalseKwd : 'false' ;
StaticKwd : 'static';
ImplicitKwd : 'implicit' ;
WhereKwd : 'where' ;

fragment Sign
    :   '+' | '-'
    ;

Greater     : '>' ;
Less        : '<' ;
Equals      : '=' ;
LeftShift   : '<<' ;
RightShift  : '>>' ;

fragment Digit
    :   [0-9]
    ;

fragment NonDigit
    :   [a-zA-Z_]
    ;

fragment PrefixChar
    :   Sign | '~' | '!'
    ;

fragment ExpansionChar
    :   '*'
    ;

fragment Inequalities
    :   Less
    |   Greater
    ;

fragment OpChar
   :    '#' | '%' | '&' | '?' | '\\' | '^' | '|' // | ':' | '@'
   |    Equals
   |    PrefixChar
   |    ExpansionChar
   ;

fragment NonConsecutiveOp
    :   Less | Greater | '/'
    ;

fragment Op
    :   '/'
    |   NonConsecutiveOp? (OpChar+ NonConsecutiveOp?)+
    ;

fragment Idstart
    :   NonDigit | '$' | '_'
    ;

fragment Idrest
   :    ( Idstart | Digit )* ('_' Op)?
   ;

PrefixOp
    :   PrefixChar
    ;

ExpansionOp
    :   ExpansionChar
    ;

Identifier
    :   ( Idstart Idrest ) | Op
    ;

Ellipses: '...';

LParen : '(' ;
RParen : ')' ;

IntegerConstant
    :   DecimalConstant IntegerSuffix?
    |   OctalConstant IntegerSuffix?
    |   HexadecimalConstant IntegerSuffix?
    |	BinaryConstant
    ;

fragment
BinaryConstant
	:	'0' [bB] [0-1]+
	;

fragment
DecimalConstant
    :   NonzeroDigit Digit*
    ;

fragment
OctalConstant
    :   '0' OctalDigit*
    ;

fragment
HexadecimalConstant
    :   HexadecimalPrefix HexadecimalDigit+
    ;

fragment
HexadecimalPrefix
    :   '0' [xX]
    ;

fragment
NonzeroDigit
    :   [1-9]
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
HexadecimalDigit
    :   [0-9a-fA-F]
    ;

fragment
IntegerSuffix
    :   UnsignedSuffix LongSuffix?
    |   UnsignedSuffix LongLongSuffix
    |   LongSuffix UnsignedSuffix?
    |   LongLongSuffix UnsignedSuffix?
    ;

fragment
UnsignedSuffix
    :   [uU]
    ;

fragment
LongSuffix
    :   [lL]
    ;

fragment
LongLongSuffix
    :   'll' | 'LL'
    ;

FloatingConstant
    :   DecimalFloatingConstant
    |   HexadecimalFloatingConstant
    ;

fragment
DecimalFloatingConstant
    :   FractionalConstant ExponentPart? FloatingSuffix?
    |   DigitSequence ExponentPart FloatingSuffix?
    ;

fragment
HexadecimalFloatingConstant
    :   HexadecimalPrefix HexadecimalFractionalConstant BinaryExponentPart FloatingSuffix?
    |   HexadecimalPrefix HexadecimalDigitSequence BinaryExponentPart FloatingSuffix?
    ;

fragment
FractionalConstant
    :   DigitSequence? '.' DigitSequence
    |   DigitSequence '.'
    ;

fragment
ExponentPart
    :   'e' Sign? DigitSequence
    |   'E' Sign? DigitSequence
    ;

DigitSequence
    :   Digit+
    ;

fragment
HexadecimalFractionalConstant
    :   HexadecimalDigitSequence? '.' HexadecimalDigitSequence
    |   HexadecimalDigitSequence '.'
    ;

fragment
BinaryExponentPart
    :   'p' Sign? DigitSequence
    |   'P' Sign? DigitSequence
    ;

fragment
HexadecimalDigitSequence
    :   HexadecimalDigit+
    ;

fragment
FloatingSuffix
    :   'f' | 'l' | 'F' | 'L'
    ;

CharacterConstant
    :   '\'' CCharSequence '\''
    |   'L\'' CCharSequence '\''
    |   'u\'' CCharSequence '\''
    |   'U\'' CCharSequence '\''
    ;

fragment
CCharSequence
    :   CChar+
    ;

fragment
CChar
    :   ~['\\\r\n]
    |   EscapeSequence
    ;
fragment
EscapeSequence
    :   SimpleEscapeSequence
    |   OctalEscapeSequence
    |   HexadecimalEscapeSequence
    ;

fragment
SimpleEscapeSequence
    :   '\\' ['"?abfnrtv\\]
    ;
fragment
OctalEscapeSequence
    :   '\\' OctalDigit
    |   '\\' OctalDigit OctalDigit
    |   '\\' OctalDigit OctalDigit OctalDigit
    ;
fragment
HexadecimalEscapeSequence
    :   '\\x' HexadecimalDigit+
    ;
StringLiteral
    :   EncodingPrefix? '"' SCharSequence? '"'
    ;

IStringLiteral
    :   Backtick (InterpGroup | InterpCharSeq)* Backtick
    ;

fragment
Backtick
    :   '`'
    ;

fragment
IGroupInner
    :   IGroupCharSeq
    |   '{' IGroupInner? '}'
    ;

fragment
InterpGroup
    :   '${' IGroupInner* '}'
    ;

fragment
IGroupCharSeq
    :   IGroupChar+
    ;

fragment
IGroupChar
    :   ~('{' | '}')
    ;

fragment
InterpCharSeq
    :   InterpChar+
    ;

fragment
InterpChar
    :   ~[`$\r\n]
    |   '\\$'
    |   '\\`'
    ;

fragment
EncodingPrefix
    :   'u8'
    |   'u'
    |   'U'
    |   'L'
    ;
fragment
SCharSequence
    :   SChar+
    ;
fragment
SChar
    :   ~["\\\r\n]
    |   EscapeSequence
    |   '\\\n'   // Added line
    |   '\\\r\n' // Added line
    ;

Whitespace
    :   [ \t]+
        -> skip
    ;

Newline
    :   (   '\r' '\n'?
        |   '\n'
        )
        -> skip
    ;

BlockComment
    :   '/*' .*? '*/'
        -> skip
    ;

LineComment
    :   '//' ~[\r\n]*
        -> skip
    ;
