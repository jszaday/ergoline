grammar Ergoline;

program
    : packageStatement? importStatement* topLevelStatement*
    ;

topLevelStatement
    :   namespace | classDeclaration | function
    ;

packageStatement
    :   'package' fqn ';'
    ;

importStatement
    :   'import' fqn ';'
    ;

statement
    :   forLoop
    |   returnStatement
    |   classDeclaration
    |   function
    |   namespace
    |   block
    |   fieldDeclaration
    |   expression ';'
    ;

forLoop
    :   'for' '(' Identifier '<-' expression ')' (block | statement)
    // |   'for' '(' variableDeclaration expression ';' expression ')' (block | statement)
    ;

returnStatement
    :   'return' expression? ';'
    ;

block
    :   '{' statement* '}'
    ;

templateDeclArg
    :   Identifier
    ;

templateDecl
    :   '<' (templateDeclArg ',')* templateDeclArg '...'? '>'
    ;

accessModifier
    :   'public' | 'protected' | 'private'
    ;

inheritanceDecl
    :   ('extends' type)? ('implements' type ('and' type)*)?
    ;

classDeclaration
    :   'class' Identifier templateDecl? inheritanceDecl '{' annotatedMember* '}'
    ;

annotatedMember
    :   annotation* member
    ;

member
    :   accessModifier? 'override'? topLevelStatement
    ;

namespace
    :   'namespace' fqn '{' topLevelStatement* '}'
    ;

fqn
    :   (Identifier '::')* Identifier
    ;

valueDeclaration
    :   'val' Identifier ':' type '=' expression ';'
    ;

variableDeclaration
    :   'var' Identifier ':' type ('=' expression)? ';'
    ;

fieldDeclaration
    :   valueDeclaration | variableDeclaration
    ;

function
    :   'func' Identifier templateDecl? '(' functionArgumentList? ')' ':' type (';' | block)
    ;

functionArgument
    :   'var'? Identifier ':' type
    ;

functionArgumentList
    :   functionArgument
    |   functionArgumentList ',' functionArgument
    ;

primaryExpression
    :   fqn
    |   Constant
    |   StringLiteral+
    |   lambdaExpression
    |   '(' expression ')'
    ;

lambdaExpression
    :   '(' functionArgumentList ')' '=>' (block | expression)
    ;

postfixExpression
    :   primaryExpression
    |   postfixExpression '[' expression ']'
    |   postfixExpression '(' argumentExpressionList? ')'
    |   postfixExpression '.' Identifier
    ;

argumentExpressionList
    :   conditionalExpression
    |   argumentExpressionList ',' conditionalExpression
    ;

unaryExpression
    :   postfixExpression
    |   unaryOperator castExpression
    // |   'sizeof' unaryExpression
    // |   'sizeof' '(' type ')'
    ;

unaryOperator
    :   '+' | '-' | '~' | '!'
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
    |   equalityExpression '==' relationalExpression
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
    :   conditionalExpression
    ;

typeList
    :   (type ',')* type
    ;

tupleType
    :   '(' typeList ')'
    ;

basicType
    :   fqn ('<' typeList '>')?
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
    :   '@' Identifier
    ;

Identifier
    :   NonDigit
        (   NonDigit
        |   Digit
        )*
    ;

Constant
    :   IntegerConstant
    |   FloatingConstant
    |   CharacterConstant
    ;

fragment NonDigit
    :   [a-zA-Z_]
    ;

fragment Digit
    :   [0-9]
    ;

fragment
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

fragment
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

fragment
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
