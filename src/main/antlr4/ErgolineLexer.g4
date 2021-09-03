lexer grammar ErgolineLexer;

Ampersand : '&' ;
Atpersand : '@' ;

LeftArrow : '<-' ;
RightArrow : '->' ;

LambdaArrow : '=>' ;

LeftCurly : '{' ;
RightCurly : '}' ;

LeftParen : '(' ;
RightParen : ')' ;

LeftSquare : '[' ;
RightSquare : ']' ;
EmptySquare : '[]' ;

Dot : '.' ;
Colon : ':' ;
Comma : ',' ;
Question : '?' ;
Semicolon : ';' ;

Less : '<' ;
Equals : '=' ;
Greater : '>' ;
LeftShift : '<<' ;
RightShift : '>>' ;
DoubleColon : '::' ;

LowerBound : '>:' ;
UpperBound : '<:' ;

QualSpecGreaterEnd
    :   Greater DoubleColon
    ;

QualSpecShiftEnd
    :   RightShift DoubleColon
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

SelfKwd
    :   'self'
    ;

CollectiveKwd
    :   'array' [1-6] 'd'
    |   'nodegroup'
    |   'group'
    ;

AwaitKwd : 'await';
AnyKwd : 'any';
AllKwd : 'all';

NewKwd : 'new' ;
ForKwd : 'for' ;
CaseKwd : 'case' ;
ElseKwd : 'else' ;
MatchKwd : 'match' ;
WhileKwd : 'while' ;
ReturnKwd : 'return' ;
ValueKeyword : 'val' ;
VariableKeyword : 'var' ;
AbstractKwd : 'abstract';
OverrideKwd : 'override';
ClassKwd : 'class';
ObjectKwd : 'object';
StructKwd : 'struct';
TraitKwd : 'trait';
FunctionKwd : 'def';
TrueKwd : 'true' ;
FalseKwd : 'false' ;
StaticKwd : 'static';
ImplicitKwd : 'implicit' ;
WhereKwd : 'where' ;
EnumKwd : 'enum' ;
PublicKwd : 'public' ;
PrivateKwd : 'private' ;
ProtectedKwd : 'protected' ;
PackageKwd : 'package' ;
UsingKwd : 'using' ;
ImportKwd : 'import' ;
WhenKwd : 'when' ;
IfKwd : 'if' ;
WithKwd : 'with' ;
ExtendsKwd : 'extends' ;
NamespaceKwd : 'namespace' ;

fragment Sign
    :   '+' | '-'
    ;

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
