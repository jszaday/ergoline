grammar Ergoline;

program
    : packageStatement? annotatedTopLevelStatement+ EOF
    ;

annotatedTopLevelStatement
    :   annotation* (topLevelStatement | namespace)
    ;

topLevelStatement
    :   function
    |   importStatement
    |   classDeclaration
    ;

packageStatement
    :   'package' fqn ';'
    ;

importStatement
    :   'import' fqn ';'
    ;

statement
    :   assignment ';'
    |   block
    |   forLoop
    |   function
    |   ifThenElse
    |   matchStatement
    |   expression ';'
    |   returnStatement
    |   classDeclaration
    |   topLevelDeclaration
    ;

pattern
    :   Identifier (':' type)?
    ;

caseStatement
    :   'case' pattern ('if' condition=expression)? LambdaArrow bodyExpression=expression? ';'
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
    :   Identifier ('<:' upperBound=type)? ('>:' lowerBound=type)?
    ;

templateDecl
    :   '<' (templateDeclArg ',')* templateDeclArg '...'? '>'
    ;

accessModifier
    :   'public' | 'protected' | 'private'
    ;

inheritanceDecl
    :   ('extends' type)? ('with' type ('and' type)*)?
    ;

classDeclaration
    :   AbstractKwd? (ClassKwd | TraitKwd) Identifier templateDecl? inheritanceDecl '{' annotatedMember* '}'
    ;

annotatedMember
    :   annotation* member
    ;

member
    :   accessModifier? OverrideKwd? ( fieldDeclaration | topLevelStatement )
    ;

namespace
    :   'namespace' fqn '{' annotatedTopLevelStatement* '}'
    ;

fqn
    :   (Identifier '::')* Identifier
    ;

valueDeclaration
    :   ValueKeyword Identifier ':' type Equals expression ';'
    ;

variableDeclaration
    :   VariableKeyword Identifier ':' type (Equals expression)? ';'
    ;

fieldValueDeclaration
    :   ValueKeyword Identifier ':' type (Equals expression)? ';'
    ;

topLevelDeclaration
    :   valueDeclaration
    |   variableDeclaration
    ;

fieldDeclaration
    :   fieldValueDeclaration
    |   variableDeclaration
    ;

function
    :   FunctionKwd Identifier templateDecl? '(' functionArgumentList? ')' ':' type (';' | block)
    ;

functionArgument
    :   VariableKeyword? Equals? Identifier ':' type
    ;

functionArgumentList
    :   (functionArgument ',')* functionArgument
    ;

expressionList
    :   (expression ',')* expression
    ;

primaryExpression
    :   fqn specialization?
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
    :   '(' functionArgumentList ')' LambdaArrow (block | expression)
    ;

postfixExpression
    :   primaryExpression
    |   postfixExpression '[' arrArgs=expressionList? ']'
    |   postfixExpression specialization? LParen fnArgs=expressionList? RParen
    |   postfixExpression '.' Identifier
    ;

unaryExpression
    :   postfixExpression
    |   unaryOperator castExpression
    |   newExpression
    // |   'sizeof' unaryExpression
    // |   'sizeof' '(' type ')'
    ;

unaryOperator
    :   '+' | '-' | '~' | '!'
    ;

newExpression
    :   'new' type tupleExpression?
    ;

castExpression
    :   '(' type ')' castExpression
    |   unaryExpression
    ;

multiplicativeExpression
    :   castExpression
    |   multiplicativeExpression '*' castExpression
    |   multiplicativeExpression '/' castExpression
    |   multiplicativeExpression '%' castExpression
    ;

additiveExpression
    :   multiplicativeExpression
    |   additiveExpression '+' multiplicativeExpression
    |   additiveExpression '-' multiplicativeExpression
    ;

shiftExpression
    :   additiveExpression
    |   shiftExpression '<<' additiveExpression
    |   shiftExpression '>>' additiveExpression
    ;

relationalExpression
    :   shiftExpression
    |   relationalExpression '<' shiftExpression
    |   relationalExpression '>' shiftExpression
    |   relationalExpression '<=' shiftExpression
    |   relationalExpression '>=' shiftExpression
    ;

equalityExpression
    :   relationalExpression
    |   equalityExpression ('==' || '===') relationalExpression
    |   equalityExpression '!=' relationalExpression
    ;

andExpression
    :   equalityExpression
    |   andExpression '&' equalityExpression
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

tupleType
    :   '(' typeList ')'
    ;

specialization
    :   '<' typeList '>'
    ;

proxySuffix
    :   Atpersand
    |   (Atpersand | Element) CollectiveKeyword
    ;

basicType
    :   fqn specialization? proxySuffix?
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
    :   Atpersand Identifier annotationOptions?
    ;

annotationOption
    :   Identifier Equals constant
    ;

annotationOptions
    :   '(' (annotationOption ',')* annotationOption ')'
    ;

Atpersand
    :   '@'
    ;

Element
    :   '[@]'
    ;

CollectiveKeyword
    :   'array1d'
    |   'nodegroup'
    |   'group'
    ;

VariableKeyword : 'var' ;
ValueKeyword : 'val' ;
AbstractKwd : 'abstract';
OverrideKwd : 'override';
ClassKwd : 'class';
TraitKwd : 'trait';
FunctionKwd : 'def';
TrueKwd : 'true' ;
FalseKwd : 'false' ;

Equals : '=' ;
PlusAssign: '+=';
MinusAssign: '-=';
StarAssign: '*=';
DivAssign: '/=';
ModAssign: '%=';
XorAssign: '^=';
AndAssign: '&=';
OrAssign: '|=';
LeftShiftAssign: '<<=';
RightShiftAssign: '>>=';

LParen : '(' ;
RParen : ')' ;

Identifier
    :   NonDigit
        (   NonDigit
        |   Digit
        )*
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
