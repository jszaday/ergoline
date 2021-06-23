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
    :   'using' Identifier templateDecl? '=' type ';'
    ;

statement
    :   annotation* innerStatement
    ;

innerStatement
    :   assignment ';'
    |   block
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
    :   Identifier (':' (tupleType | basicType)?)?
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

identifierList
    :   (Identifier ',')* Identifier
    ;

loopHeader
    :   (identifierList | '(' identifierList ')') '<-' expression
    |   (variableDeclaration? | ';') test=expression? ';' assignment?
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

templateDeclArg
    :   name=Identifier ellipses=Ellipses? (('<:' upperBound=type)? ('>:' lowerBound=type)? | ':' argTy=type?) ('=' specializationElement)?
    ;

templateDecl
    :   '<' (templateDeclArg ',')* templateDeclArg '>'
    ;

accessModifier
    :   'public' | 'protected' | 'private'
    ;

inheritanceDecl
    :   ('extends' type)? ('with' type ('and' type)*)?
    ;

classDeclaration
    :   ((AbstractKwd? ClassKwd) | StructKwd | TraitKwd) Identifier templateDecl? inheritanceDecl '{' annotatedMember* '}'
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
    :   (Identifier '::')* Identifier
    ;

valueDeclaration
    :   ValueKeyword Identifier (':' type)? Equals expression ';'
    ;

variableDeclaration
    :   VariableKeyword Identifier (':' type)? (Equals expression)? ';'
    ;

fieldDeclaration
    :   (ValueKeyword | VariableKeyword) Identifier ':' type (Equals expression)? ';'
    ;

topLevelDeclaration
    :   ImplicitKwd? (valueDeclaration | variableDeclaration)
    ;

basicArgument
    :   Identifier ':' expansion='*'? type
    ;

implicitArgument
    :   ImplicitKwd basicArgument
    ;

implicitArguments
    :   '(' (implicitArgument ',')* implicitArgument ')'
    ;

function
    :   FunctionKwd Identifier templateDecl? '(' functionArgumentList? ')' implicitArguments? (':' type)? (';' | block)
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

assignment
    :   postfixExpression assignmentOperator expression
    ;

assignmentOperator
    : Equals
	| StarAssign
	| DivAssign
	| ModAssign
	| PlusAssign
	| MinusAssign
	| RightShiftAssign
	| LeftShiftAssign
	| AndAssign
	| XorAssign
	| OrAssign
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

postfixExpression
    :   primaryExpression
    |   postfixExpression '[' arrArgs=sliceExpressionList ']'
    |   postfixExpression specialization? LParen fnArgs=expressionList? RParen
    |   selfExpression Identifier
    |   postfixExpression '.' Identifier
    ;

unaryExpression
    :   postfixExpression
    |   newExpression
    |   awaitExpression
    |   unaryOperator unaryExpression
    ;

unaryOperator
    :   '+' | '-' | '~' | '!'
    ;

awaitExpression
    :    AwaitKwd postfixExpression
    ;

newExpression
    :   'new' type tupleExpression?
    ;

multiplicativeExpression
    :   unaryExpression
    |   multiplicativeExpression '*' unaryExpression
    |   multiplicativeExpression '/' unaryExpression
    |   multiplicativeExpression '%' unaryExpression
    ;

additiveExpression
    :   multiplicativeExpression
    |   additiveExpression '+' multiplicativeExpression
    |   additiveExpression '-' multiplicativeExpression
    ;

shiftExpression
    :   additiveExpression
    |   shiftExpression LeftShift additiveExpression
    |   shiftExpression RightShift additiveExpression
    ;

relationalExpression
    :   shiftExpression
    |   relationalExpression Less shiftExpression
    |   relationalExpression Greater shiftExpression
    |   relationalExpression '<=' shiftExpression
    |   relationalExpression '>=' shiftExpression
    ;

equalityExpression
    :   relationalExpression
    |   relationalExpression ('!=' || '==' || '===' || '!==') equalityExpression
    ;

andExpression
    :   equalityExpression
    |   andExpression Ampersand equalityExpression
    ;

exclusiveOrExpression
    :   andExpression
    |   exclusiveOrExpression '^' andExpression
    ;

inclusiveOrExpression
    :   exclusiveOrExpression
    |   inclusiveOrExpression '|' exclusiveOrExpression
    ;

logicalAndExpression
    :   inclusiveOrExpression
    |   logicalAndExpression '&&' inclusiveOrExpression
    ;

logicalOrExpression
    :   logicalAndExpression
    |   logicalOrExpression '||' logicalAndExpression
    ;

conditionalExpression
    :   logicalOrExpression ('?' expression ':' conditionalExpression)?
    ;

expression
    :   matchStatement
    |   conditionalExpression
    ;

typeList
    :   (type ',')* type
    ;

constExpression
    :   constant
    |   fqn
    ;

tupleType
    :   '(' typeList ')'
    |   tupleType multiply='.*' constExpression
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
    :   Atpersand
    |   (Atpersand | Element) CollectiveKeyword
    ;

basicType
    :   fqn Ellipses
    |   fqn specialization? proxySuffix?
    ;

lambdaType
    :   (basicType | tupleType) '=>' (basicType | tupleType)
    ;

type
    :   tupleType
    |   basicType
    |   lambdaType
    ;

annotation
    :   Atpersand Identifier annotationOptions?
    ;

annotationOption
    :   (StaticKwd | Identifier) Equals constant
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

Element
    :   '[@]'
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

SelfKeyword
    :   'self' (Atpersand | Element)?
    ;

Equals : '=' ;
PlusAssign: '+=';
MinusAssign: '-=';
StarAssign: '*=';
DivAssign: '/=';
ModAssign: '%=';
XorAssign: '^=';
AndAssign: '&=';
OrAssign: '|=';
LeftShiftAssign: LeftShift Equals;
RightShiftAssign: RightShift Equals;
Ellipses: '...';

Greater: '>' ;
Less: '<' ;

LeftShift: Less Less ;

RightShift: Greater Greater ;


LParen : '(' ;
RParen : ')' ;

Identifier
    :   NonDigit
        (   NonDigit
        |   Digit
        )*
    ;

fragment NonDigit
    :   [a-zA-Z_]
    ;

fragment Digit
    :   [0-9]
    ;

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

fragment
Sign
    :   '+' | '-'
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
