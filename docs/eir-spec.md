# EIR — Compiler Architecture and Language Specification

> Status: design draft
> Companion to: `eir.md`

---

## Part I — Compiler Architecture

### Pipeline Overview

```
 Source files (.erg)
        │
        ▼
 ┌─────────────┐
 │    Lexer    │  source text → token stream
 └──────┬──────┘
        │
        ▼
 ┌─────────────┐
 │   Parser    │  tokens → untyped AST (UntypedAst)
 └──────┬──────┘
        │
        ▼
 ┌──────────────────┐
 │ Module Resolution│  discovers + lazily parses imported modules
 └──────┬───────────┘
        │
        ▼
 ┌─────────────────┐
 │ Name Resolution │  UntypedAst → ResolvedAst (all symbols have DefIds)
 └──────┬──────────┘
        │
        ▼
 ┌────────────────────────┐
 │ Type Inference/Checking│  ResolvedAst → TypedAst (every node has a type)
 └──────┬─────────────────┘
        │
        ▼
 ┌──────────────────┐
 │ SDAG Segmentation│  TypedAst → SegmentedAst (structured EPs → coroutines)
 └──────┬───────────┘
        │
        ▼
 ┌──────────────────┐
 │ Monomorphization │  SegmentedAst → MonoAst (type params eliminated)
 └──────┬───────────┘
        │
        ▼
 ┌─────────────┐
 │  C++ Codegen│  MonoAst → .cc file + eir_register()
 └─────────────┘
```

Each phase consumes a distinct typed representation. This is intentional — the Rust type
system enforces that an unresolved symbol cannot appear after name resolution, a
type-unannotated expression cannot appear after type checking, and so on. This eliminates
the class of bugs in EIRv1 where `???` stubs in `replaceChild` and `replaceChild` methods
were left unimplemented because the phase boundaries were implicit.

---

### Phase 1: Lexer

**Input:** UTF-8 source text  
**Output:** `Vec<Token>` with source spans  
**Crate:** `logos` (zero-copy lexer generator)

Tokens carry their source span (`Span { file: FileId, start: u32, end: u32 }`) for error
reporting. Whitespace and comments are discarded at this stage. The lexer is context-free
with one exception: f-string interpolation requires a small mode stack (text mode vs.
expression mode inside `${...}`).

**f-string lexing:**

```
f"hello $name, you are ${age + 1} years old"
```

Emitted as:
```
FStringStart
FStringText("hello ")
FStringInterp(bare: "name")
FStringText(", you are ")
FStringExprStart        ← entering expression mode
  ID("age"), OP("+"), INT(1)
FStringExprEnd          ← back to text mode
FStringText(" years old")
FStringEnd
```

The lexer maintains a brace-depth counter inside `${...}` to handle nested expressions:
`` f"val = ${f(a, {b: 1})}" `` works correctly.

---

### Phase 2: Parser

**Input:** Token stream  
**Output:** `UntypedAst` — a tree of `Spanned<Node>` values  
**Approach:** Hand-written recursive descent (no parser generator)

The parser never aborts on the first error. It uses a `Diagnostic` accumulator and
attempts recovery at statement boundaries — similar to rustc's error recovery. A file
with parse errors produces a partial `UntypedAst` annotated with error nodes; downstream
phases skip subtrees containing error nodes.

**UntypedAst node definitions (Rust sketch):**

```rust
// Every node carries its source span.
type Spanned<T> = (T, Span);

enum Expr {
    Lit(Literal),
    Id(Vec<String>),                            // qualified name, unresolved
    FString(Vec<FStringPart>),
    Tuple(Vec<Spanned<Expr>>),
    Lambda(Vec<FnArg>, Box<Spanned<Expr>>),
    Call(Box<Spanned<Expr>>, Vec<CallArg>, Vec<TypeArg>),
    Index(Box<Spanned<Expr>>, Vec<Slice>),
    Field(Box<Spanned<Expr>>, String),
    Unary(UnaryOp, Box<Spanned<Expr>>),
    Binary(Box<Spanned<Expr>>, String, Box<Spanned<Expr>>),
    Cast(Box<Spanned<Expr>>, Spanned<Type>),     // 'as' cast
    Propagate(Box<Spanned<Expr>>, PropOp),        // ? or !
    Ternary(Box<Spanned<Expr>>, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Match(Box<Spanned<Expr>>, Vec<CaseArm>),
    New(Spanned<Type>, Vec<Spanned<Expr>>),
    Await(Box<Spanned<Expr>>),
    Async(Box<Block>),
    Forall(ForallHeader, Box<Block>),
    SelfExpr(Option<ProxySuffix>),
    Block(Box<Block>),
    Error(Span),
}

enum PropOp { Result, Option }   // ? and ! respectively

struct Block {
    stmts: Vec<Spanned<Stmt>>,
    implicit_return: bool,
}
```

The `UntypedAst` contains no resolved `DefId`s — all names are plain `Vec<String>`
representing qualified identifiers. This is the faithful syntactic representation.

**Operator precedence** is not resolved during parsing. The parser produces a flat
`Binary(lhs, op, rhs)` tree and a post-parse precedence-climbing pass (the `sortInfixes`
approach from EIRv1, but implemented cleanly as a separate step) restructures it. This
avoids embedding precedence rules deep in the recursive descent grammar.

---

### Phase 3: Module Resolution

**Input:** `UntypedAst` for the root file, filesystem  
**Output:** `ModuleGraph` — a DAG of parsed modules  
**Key data:** `FileId → UntypedAst`, `FqName → FileId`

Modules are discovered lazily: when an `import foo::bar` is encountered, the resolver
finds `foo/bar.erg` on the search path and parses it (triggering further imports
transitively). The search path is `$ERG_PATH` + the standard library directory.

Package statements are validated: a file at `foo/bar.erg` must begin with `package foo;`.

Circular imports are detected and reported as a `Diagnostic`. The `ModuleGraph` is a DAG
(cycles are errors, not silently handled).

---

### Phase 4: Name Resolution

**Input:** `ModuleGraph`  
**Output:** `ResolvedAst` — same structure as `UntypedAst` but all names replaced with `DefId`s  
**Key data:** `DefId → DefInfo`, `ScopeId → Scope`

```rust
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct DefId(u32);

struct DefInfo {
    name:   String,
    kind:   DefKind,     // Fn, Class, Trait, Field, Local, ...
    span:   Span,
    scope:  ScopeId,
}
```

Resolution rules:
- Declarations within a class/namespace can be in any order (forward references are valid)
- Local variables are ordered — a variable is not visible before its `val`/`var` declaration
- Implicit variables are resolved by name+type match, propagated through call chains

Template type parameters get their own `DefId` kind (`DefKind::TypeParam`) and are
scoped to their enclosing function or class.

---

### Phase 5: Type Inference and Checking

**Input:** `ResolvedAst`  
**Output:** `TypedAst` — every `Expr` node annotated with a `TypeId`  
**Key data:** `TypeId → TypeInfo`, substitution context (for generics)

```rust
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct TypeId(u32);

enum TypeInfo {
    Primitive(PrimType),
    Class(DefId, Vec<TypeId>),       // class + type args
    Trait(DefId, Vec<TypeId>),
    Tuple(Vec<TypeId>),
    Lambda(Vec<TypeId>, TypeId),     // (args) => ret
    Proxy(TypeId, Option<ProxyKind>),
    Ref(TypeId),
    Pack(TypeId),
    Infer(u32),                      // unification variable (before inference)
}
```

**Template specialization** is handled via a substitution stack. When type-checking a
call `foo<int>(x)`, the checker pushes `{A → int}` onto the stack and type-checks the
body of `foo` under that substitution. `where` clause predicates are evaluated against
the substitution using `StaticEvaluator`.

**`must_use` enforcement:** `result<T, E>` and `option<T>` are registered as must-use
types. Any expression statement of a must-use type that is not the last expression of a
block produces a `Diagnostic::Error`. Calling `.ignore()` suppresses this.

**Packed struct layout** is computed here: each `packed struct` gets a `PackedLayout`
annotation recording the bit offset and bit width of each field. `static_assert`
statements are evaluated against the computed layout.

**The `specialization_worklist`** — the type checker maintains a set of
`(DefId, Vec<TypeId>)` pairs representing all specializations encountered during checking.
This is the seed for monomorphization in Phase 7.

---

### Phase 6: SDAG Segmentation

**Input:** `TypedAst`  
**Output:** `SegmentedAst` — structured entry methods replaced with coroutine state machines  
**Scope:** Only touches `@entry` functions containing `when`/`await`/`forall`

An entry method that contains no `when`/`await` constructs is a plain function — it passes
through unchanged.

An entry method containing suspension points is transformed into:

```
struct FooStateMachine {
    state: u32,
    // all locals that are live across a suspension point
    local_x: TypeOfX,
    local_y: TypeOfY,
    // ...
}

impl FooStateMachine {
    fn resume(&mut self, actor: &mut FooActor, msg: Option<AnyMessage>) {
        match self.state {
            0 => { /* pre-first-suspension code */ self.state = 1; }
            1 => { /* code after first when, uses msg */ self.state = 2; }
            // ...
            N => { /* terminal state */ }
        }
    }
}
```

**CFA for segmentation:** The pass performs a control flow analysis to identify all
suspension points and the variables live across each. A suspension point is any of:
- `when mailbox(pattern) => body`
- `await all/any { when ... }`
- `await future_expr`
- `await forall (...) { ... }`

The liveness analysis determines which locals must be stored in the state machine struct
(those that are used after a suspension point but defined before it). Locals that do not
cross suspension points remain as normal stack variables in the generated code.

**`forall` lowering:**

```ergoline
await forall (i <- 0 to n) {
    when receive(_ == i, data) => { process(i, data); };
}
```

Lowers to: spawn `n` coroutine instances (one per iteration), each with its own mailbox
slot binding, collect their `future<unit>` into an array, then `await` the array.
The implicit join is an `await all(futures)`.

---

### Phase 7: Monomorphization

**Input:** `SegmentedAst`, `specialization_worklist` from Phase 5  
**Output:** `MonoAst` — flat list of concrete classes and functions, no type parameters  

Starting from the worklist seed (all specializations encountered during type checking),
the monomorphizer performs a fixed-point expansion:

1. For each `(DefId, Vec<TypeId>)` in the worklist, substitute type parameters and
   produce a concrete `MonoItem`
2. Scan the resulting `MonoItem` for further specializations; add any new ones to the
   worklist
3. Repeat until the worklist is empty

Each `MonoItem` is assigned a mangled C++ name: `ClassName__TypeArg1__TypeArg2`.

```rust
struct MonoItem {
    mangled_name:  String,
    original:      DefId,
    type_args:     Vec<TypeId>,
    kind:          MonoItemKind,   // Class, Fn, EntryMethod, ...
}
```

The output has no `TypeParam` nodes — every type is fully concrete. Variance
relationships have been verified during type checking and need not be represented in the
monomorphized output.

---

### Phase 8: C++ Codegen

**Input:** `MonoAst`  
**Output:** `generate.cc` containing:
- Forward declarations
- Packed struct definitions with bit-field accessor methods
- Concrete class/struct definitions (no C++ templates)
- Coroutine state machine structs and `resume()` implementations
- Entry method dispatch functions
- `eir_register()` — registers all actor types and entry methods with the runtime

**No `.ci` files.** Everything the Charmxi interface file previously described is now
emitted directly by ergc.

**`eir_register()` structure:**

```cpp
void eir_register() {
    // Register actor types
    eir::register_actor<jacobi2d__concrete>(
        "examples::jacobi2d",
        sizeof(jacobi2d__concrete)
    );
    // Register entry methods
    eir::register_entry<jacobi2d__concrete>(
        "run",
        [](void* actor, void* msg) {
            static_cast<jacobi2d__concrete*>(actor)->run__entry(msg);
        }
    );
    // Register mailboxes
    eir::register_mailbox<jacobi2d__concrete>(
        "receive_ghost",
        offsetof(jacobi2d__concrete, receive_ghost__mailbox)
    );
    // ...
}
```

**Packed struct codegen:**

```ergoline
@bigEndian
packed struct ipv4Header {
    version: unsigned<4>;
    ihl:     unsigned<4>;
    length:  u16;
}
```

Emits:

```cpp
struct __attribute__((packed)) ipv4Header {
private:
    uint8_t  _bytes[4];  // raw storage
public:
    uint8_t  version() const { return (_bytes[0] >> 4) & 0x0F; }
    void     version(uint8_t v) {
                 EIRV2_RANGE_CHECK(v, 4);
                 _bytes[0] = (_bytes[0] & 0x0F) | ((v & 0x0F) << 4);
             }
    uint8_t  ihl() const { return _bytes[0] & 0x0F; }
    void     ihl(uint8_t v) {
                 EIRV2_RANGE_CHECK(v, 4);
                 _bytes[0] = (_bytes[0] & 0xF0) | (v & 0x0F);
             }
    uint16_t length() const {
                 return (uint16_t(_bytes[1]) << 8) | _bytes[2];
             }
    void     length(uint16_t v) {
                 _bytes[1] = v >> 8; _bytes[2] = v & 0xFF;
             }
};
static_assert(sizeof(ipv4Header) == 4);
```

`EIRV2_RANGE_CHECK` expands to a panic in debug builds and is a no-op in release.

---

### Crate Structure

```
ergc/                          ← workspace root
├── Cargo.toml
├── crates/
│   ├── ergc-lexer/            ← logos-based lexer, token types, Span
│   ├── ergc-ast/              ← UntypedAst, ResolvedAst, TypedAst, SegmentedAst, MonoAst
│   ├── ergc-parse/            ← recursive descent parser, error recovery
│   ├── ergc-resolve/          ← module graph, name resolution, DefId/ScopeId
│   ├── ergc-check/            ← type inference, checking, packed layout, static_assert
│   ├── ergc-sdag/             ← CFA, segmentation pass, coroutine lowering
│   ├── ergc-mono/             ← monomorphization, mangling
│   ├── ergc-codegen/          ← C++ emission, packed struct accessors, eir_register()
│   ├── ergc-diagnostics/      ← Diagnostic type, source rendering (miette-based)
│   └── ergc-driver/           ← CLI, pipeline orchestration
├── stdlib/                     ← .erg standard library sources
│   ├── ergoline/
│   └── ck/
└── tests/
    ├── parse/
    ├── check/
    └── codegen/
```

The `ergc-ast` crate defines all five AST types. Each downstream crate depends on the
input and output types of its phase; it does not depend on earlier phases directly. This
enforces clean phase boundaries at the Rust crate level.

---

### Diagnostics

All errors and warnings use `miette` for rendering with source annotations:

```
error[E0042]: value 16 does not fit in unsigned<4>
  --> src/main.erg:12:5
   |
12 |     h.version = 16u8;
   |                 ^^^^ constant 16 exceeds range 0..15 for unsigned<4>
   |
   = note: field `version` declared as unsigned<4> in ipv4Header
```

The `Diagnostic` type is accumulated across all phases. The compiler reports all
diagnostics at the end (not abort-on-first-error), unless a later phase cannot proceed
without a clean earlier phase (e.g., codegen requires a clean monomorphized AST).

---

## Part II — Language Specification (EBNF)

Notation:
- `::=` — production rule
- `|` — alternation
- `A?` — optional (zero or one)
- `A*` — zero or more
- `A+` — one or more
- `( A )` — grouping
- `'kw'` — literal terminal
- `TOKEN` — lexer token (upper case)
- `rule` — non-terminal (lower case)
- `(* note *)` — annotation

---

### Lexical Structure

#### Whitespace and comments

```
whitespace  ::= [ \t\r\n]+                     (* discarded *)
line_comment ::= '//' [^\n]* '\n'              (* discarded *)
block_comment ::= '/*' .* '*/'                 (* discarded, non-nested *)
```

#### Identifiers and keywords

```
ID          ::= id_start id_rest* id_close?
              | operator_seq

id_start    ::= LETTER | '_' | '$'
id_rest     ::= LETTER | DIGIT | '_' | '$'
id_close    ::= '_' operator_seq?
operator_seq ::= op_char+

op_char     ::= '!' | '#' | '%' | '&' | '*' | '+' | '-' | '/'
              | ':' | '<' | '=' | '>' | '?' | '@' | '\' | '^'
              | '|' | '~'
              (* excluding reserved sequences: '::' '=>' '<-' '<:' '>:' '@' '[@]' '{@}' )
```

**Reserved keywords** (may not be used as identifiers):
```
abstract  all      any      as       async    await
bool      byte     char     class    def      do
else      extends  f16      f32      f64      bf16
false     for      forall   i8       i16      i32
i64       if       implicit import   match    namespace
new       object   override package  packed   private
protected public   return   self     signed   static
struct    trait    true     u8       u16      u32
u64       unsigned using    val      var      when
where     with
```

#### Integer literals

```
INT_LIT     ::= decimal_int int_suffix?
              | '0x' hex_int    int_suffix?
              | '0o' octal_int  int_suffix?
              | '0b' binary_int int_suffix?

decimal_int ::= NONZERO_DIGIT ( '_'? DIGIT )* | '0'
hex_int     ::= HEX_DIGIT ( '_'? HEX_DIGIT )*
octal_int   ::= OCTAL_DIGIT ( '_'? OCTAL_DIGIT )*
binary_int  ::= BIT ( '_'? BIT )*

int_suffix  ::= 'u8' | 'i8' | 'u16' | 'i16' | 'u32' | 'i32' | 'u64' | 'i64'
```

Decimal literals with a leading `0` followed by more digits are a compile error — there
is no implicit octal. Use `0o` explicitly.

Integer literals overflow their suffix type at compile time → compile error.

#### Floating-point literals

```
FLOAT_LIT   ::= decimal_float float_suffix?
              | hex_float     float_suffix?

decimal_float ::= decimal_int '.' decimal_int exponent?
                | decimal_int exponent

hex_float   ::= '0x' hex_int ( '.' hex_int )? 'p' ( '+' | '-' )? decimal_int
                (* IEEE 754 hexadecimal floating point, e.g. 0x1.8p+1 = 3.0 *)

exponent    ::= ( 'e' | 'E' ) ( '+' | '-' )? decimal_int
float_suffix ::= 'f32' | 'f64' | 'f16' | 'bf16'
```

Default float type (no suffix): `f64`.

Numeric separators (`_`) are permitted in both the integer and fractional parts of float
literals: `1_234.567_89f64`.

Special float values are type-associated constants, not literal syntax:
`f32::nan`, `f64::inf`, `f32::negInf`, `f64::negZero`.
Negative zero via literal: `-0.0f32` (unary minus applied to `0.0f32`).

#### String and character literals

```
STRING_LIT  ::= '"' s_char* '"'
CHAR_LIT    ::= '\'' c_char '\''

s_char      ::= [^"\\\r\n] | escape_seq | '\\\n'
c_char      ::= [^'\\\r\n] | escape_seq

escape_seq  ::= '\\' ( 'n' | 't' | 'r' | '0' | '\\' | '\'' | '"'
              |         'x' HEX_DIGIT HEX_DIGIT )
```

#### F-strings (interpolated strings)

```
FSTRING     ::= 'f' '"' fstring_part* '"'

fstring_part ::= fstring_text
               | '$' ID                    (* bare identifier: $name *)
               | '${' expr '}'             (* arbitrary expression: ${age + 1} *)

fstring_text ::= ( [^"\\$] | escape_seq | '\\$' )+
```

`$name` is sugar for `${name}`. A literal `$` not followed by an identifier or `{` is a
compile error (use `\$` for a literal dollar sign inside an f-string).

Plain string literals `"..."` are never interpolated.

---

### Top-Level Grammar

```
program         ::= package_decl? program_member* EOF

package_decl    ::= 'package' fqn ';'

program_member  ::= annotation* ( namespace_decl | import_decl | top_decl )

namespace_decl  ::= 'namespace' fqn ( ';' | '{' program_member* '}' )

import_decl     ::= 'public'? 'import' fqn ( '::' '_' )? ';'
                  (* wildcard '_' imports all names from the module *)
                  (* single-name: import foo::bar; *)
                  (* no brace-list form *)

top_decl        ::= class_decl | fn_decl | using_decl
```

---

### Type Aliases

```
using_decl      ::= 'using' ID template_decl? '=' const_expr ';'
```

---

### Classes, Structs, Traits, Objects

```
class_decl      ::= class_kind ID template_decl? inheritance? where_clause? class_body

class_kind      ::= 'abstract'? 'class'
                  | 'struct'
                  | 'object'
                  | 'trait'
                  | 'packed' 'struct'

inheritance     ::= ( 'extends' type )? ( 'with' type )*

class_body      ::= '{' class_member* '}' | ';'

class_member    ::= annotation* access_mod? ( 'override' | 'static' )? member_body

access_mod      ::= 'public' | 'protected' | 'private'

member_body     ::= field_decl | fn_decl | class_decl | using_decl | import_decl

field_decl      ::= ( 'val' | 'var' ) ID ':' field_type ( '=' expr )? ';'

field_type      ::= pack_field_type   (* only valid inside 'packed struct' *)
                  | type

pack_field_type ::= 'unsigned' '<' INT_LIT '>'
                  | 'signed'   '<' INT_LIT '>'
                  | type
                  (* 'unsigned<N>' and 'signed<N>' reserved here;
                     using them outside a packed struct is a compile error *)
```

---

### Functions

```
fn_decl         ::= 'def' fn_id template_decl?
                    '(' fn_arg_list? ')'
                    ( '(' implicit_arg_list? ')' )?
                    ( ':' type )?
                    where_clause?
                    ( block | ';' )

fn_id           ::= 'self' | '[]' | ID

fn_arg          ::= ( '&' | '=' )? ID ':' '*'? type
fn_arg_list     ::= fn_arg ( ',' fn_arg )*

implicit_arg    ::= 'implicit' ID ':' '*'? type
implicit_arg_list ::= implicit_arg ( ',' implicit_arg )*

template_decl   ::= '<' template_arg ( ',' template_arg )* '>'
template_arg    ::= variance? ID '...'?
                    bounds?
                    ( ':' type )?
                    ( '=' const_expr )?
variance        ::= '+' | '-'
bounds          ::= ( '>:' type ) ( '<:' type )?
                  | ( '<:' type ) ( '>:' type )?

where_clause    ::= 'where' const_expr
```

Return type defaults to `unit` when omitted. Functions with no body (`;` instead of
block) are abstract; only permitted inside `trait` or annotated `@system`.

---

### Statements

```
statement       ::= annotation* inner_stmt

inner_stmt      ::= block
                  | for_loop
                  | forall_loop
                  | do_while
                  | while_loop
                  | if_else
                  | return_stmt
                  | when_stmt
                  | await_many_stmt
                  | static_assert_stmt
                  | top_decl
                  | local_decl
                  | expr ';'

opt_stmt        ::= statement | ';'

block           ::= '{' statement* '}'

return_stmt     ::= 'return' expr? ';'
                  (* bare 'return;' produces unit *)

static_assert_stmt ::= 'static_assert' '(' const_expr ( ',' STRING_LIT )? ')' ';'
                     (* evaluated at compile time; failure is a compile error *)

local_decl      ::= 'implicit'? ( 'val' | 'var' ) decltypes ( '=' expr )? ';'

decltypes       ::= decltype | '(' decltype ( ',' decltype )+ ')'
decltype        ::= '&'? ID ( ':' type )?
```

---

### Loops

```
for_loop        ::= 'for' '(' loop_header ')' opt_stmt

forall_loop     ::= 'forall' '(' forall_header ')' block
                  (* each iteration is an implicit 'async' task;
                     implicit 'await all' at closing brace *)

loop_header     ::= decltypes '<-' expr                   (* range-based *)
                  | local_decl? expr? ';' expr?           (* C-style *)

forall_header   ::= decltypes '<-' expr                   (* range-based only *)

while_loop      ::= 'while' '(' expr ')' opt_stmt
do_while        ::= 'do' opt_stmt 'while' '(' expr ')' ';'
if_else         ::= 'if' '(' expr ')' opt_stmt ( 'else' opt_stmt )?
```

`forall` accepts only a range-based header. The body is always a block. The implicit
`await` at the end means execution does not proceed past the `forall` until all
iterations have completed. Semantically equivalent to:

```ergoline
val __tasks = (0 to n).map(i => async { body(i) });
await __tasks.reduce((a, b) => a.and(b));
```

---

### SDAG Constructs

```
when_stmt       ::= 'when' when_fn ( ',' when_fn )* ( 'if' expr )? '=>' opt_stmt

await_many_stmt ::= 'await' ( 'all' | 'any' ) '{' when_stmt+ '}'

when_fn         ::= identifier '(' pattern_list? ')'
```

Valid only inside a structured entry method (`@entry def` containing suspension points).
The compiler identifies structured entry methods during SDAG segmentation and rejects
`when`/`await` outside them.

---

### Pattern Matching

```
match_expr      ::= 'match' '(' expr ')' '{' case_stmt+ '}'

case_stmt       ::= 'case' pattern_list ( 'if' expr )? '=>' opt_stmt

pattern_list    ::= pattern ( ',' pattern )*

pattern         ::= extractor_pattern
                  | id_pattern
                  | const_pattern
                  | expr_pattern

extractor_pattern ::= identifier '(' pattern_list? ')'
                    (* identifier must resolve to a type/object with static unapply *)

id_pattern      ::= ID ( ':' basic_type )?
                  (* '_' is the wildcard; matches any value and binds nothing *)

const_pattern   ::= INT_LIT | FLOAT_LIT | STRING_LIT | CHAR_LIT | BOOL_LIT
                  (* sugar for expr_pattern: '_ ==' literal *)

expr_pattern    ::= infix_expr
                  (* must include at least one infix operator, distinguishing
                     it from id_pattern; typically '_ == expr' or '_ >= expr' etc. *)
```

A `match` without an exhaustive case is a compile error in strict mode; a runtime abort
in non-strict mode when no case matches.

---

### Expressions

```
expr            ::= match_expr | assign_expr

assign_expr     ::= infix_expr ( assign_op infix_expr )?
assign_op       ::= '=' | '+=' | '-=' | '*=' | '/=' | '%='
                  | '&=' | '|=' | '^=' | '<<=' | '>>='
                  | ID '='       (* custom assignment operator overload *)

infix_expr      ::= cast_expr ( ID cast_expr )*
                  (* ID may be any operator identifier *)
                  (* precedence determined by first character: *)
                  (*   first level: '*' '/' '%'               *)
                  (*   second:      '+' '-'                    *)
                  (*   third:       '=' '!'                    *)
                  (*   fourth:      '<' '>'                    *)
                  (*   fifth:       '&'                        *)
                  (*   sixth:       '^'                        *)
                  (*   seventh:     '|'                        *)
                  (*   alphabetic / assign-ops: lowest         *)

cast_expr       ::= conditional_expr ( 'as' type )?
                  (* 'as' binds tighter than infix ops:        *)
                  (* 'a + b as u32' parses as 'a + (b as u32)' *)

conditional_expr ::= basic_expr ( '?' expr ':' conditional_expr )?

basic_expr      ::= unary_expr | new_expr | await_expr | async_expr

unary_expr      ::= prefix_op? postfix_expr

prefix_op       ::= '+' | '-' | '!' | '~'

postfix_expr    ::= primary_expr postfix_suffix*

postfix_suffix  ::= '.' ID                                   (* member access *)
                  | '.' 'self' proxy_suffix                  (* proxy member shorthand *)
                  | '[' slice_list ']'                       (* index or slice *)
                  | specialization? '(' call_arg_list? ')'  (* function call *)
                  | '?'                  (* result<T,E> propagation — only in result-returning fn *)
                  | '!'                  (* option<T>   propagation — only in option-returning fn *)
```

`?` and `!` are postfix — they bind tighter than any infix operator. Chaining works
naturally: `foo()?.bar` calls `foo`, propagates error if `err`, then accesses `.bar` on
the unwrapped value.

```
primary_expr    ::= literal
                  | fstring
                  | self_expr
                  | qualified_id
                  | tuple_expr
                  | lambda_expr
                  | block             (* closure: evaluates to last expression *)

new_expr        ::= 'new' type tuple_expr?

await_expr      ::= 'await' postfix_expr
                  | 'await' 'all' '{' when_stmt+ '}'
                  | 'await' 'any' '{' when_stmt+ '}'
                  (* postfix_expr may be:
                     - a future<T>          → unwraps T when resolved
                     - an array<future<T>>  → awaits all, returns array<T>
                     - a forall_expr        → awaits all iterations          *)

async_expr      ::= 'async' block
                  (* type: future<T> where T is the type of the last expr in block *)

forall_expr     ::= 'forall' '(' forall_header ')' block
                  (* usable as expression: type is future<array<T>> *)
                  (* or as statement: implicit 'await' applied *)

self_expr       ::= 'self' proxy_suffix?
proxy_suffix    ::= '@' | '[@]' | '{@}'

qualified_id    ::= ( '::' )? ID ( '::' ID )* specialization?

specialization  ::= '<' type_arg ( ',' type_arg )* '>'
type_arg        ::= type | const_expr

tuple_expr      ::= '(' ( expr ( ',' expr )* )? ')'
                  (* '()' is the unit literal *)

lambda_expr     ::= '(' fn_arg_list? ')' '=>' ( block | expr )

call_arg        ::= '&'? expr
call_arg_list   ::= call_arg ( ',' call_arg )*

slice           ::= expr? ( ':' ( expr ':' )? expr? )?
                  | expr
slice_list      ::= slice ( ',' slice )*
```

---

### Types

```
type            ::= lambda_type ( '&' | '...' )?
                  (* '&' → reference type; '...' → pack expansion *)

lambda_type     ::= tuple_multiply ( '=>' tuple_multiply )?

tuple_multiply  ::= basic_type ( '.*' const_primary_expr )?

basic_type      ::= proxy_type | tuple_type

proxy_type      ::= type_path proxy_type_suffix?

type_path       ::= ( '::' )? type_segment ( '::' type_segment )*
type_segment    ::= ID ( '<' type_arg ( ',' type_arg )* '>' )?

proxy_type_suffix ::= ( '@' | '[@]' | '{@}' ) collective_kwd?
collective_kwd  ::= 'array' [1-9] 'd' | 'nodegroup' | 'group'

tuple_type      ::= '(' type ( ',' type )+ ')'
                  | '(' ')'          (* unit type *)
```

---

### Static (Compile-Time) Expressions

Used in `where` clauses, template default values, `static_assert`, and `using` aliases.

```
const_expr      ::= static_infix

static_infix    ::= static_unary ( ( '<:' | '>:' | ID ) static_unary )*

static_unary    ::= prefix_op? static_postfix

static_postfix  ::= static_primary ( '[' const_expr_list ']' )*

static_primary  ::= type | literal | '(' const_expr_list ')'

const_expr_list ::= const_expr ( ',' const_expr )*

const_primary_expr ::= INT_LIT | '(' const_expr ')'
                     (* used in tuple multiply: (int) .* 3 *)
```

---

### Annotations

```
annotation      ::= '@' ID ( '(' annotation_arg ( ',' annotation_arg )* ')' )?

annotation_arg  ::= ( ID | 'static' ) ( '=' literal )?
```

Built-in annotations:

| Annotation | Target | Meaning |
|---|---|---|
| `@entry` | method | Remotely invocable entry method |
| `@entry(kind, ...)` | method | Entry method restricted to certain proxy kinds |
| `@mailbox` | body-less method | Declares a typed mailbox slot |
| `@local` | entry method | Executes synchronously on caller's PE only |
| `@main` | class | Main chare (program entry point) |
| `@system(...)` | fn, class | C++ FFI binding |
| `@extern "C"` | fn | C ABI binding (ctypes-compatible) |
| `@override` | method | (syntactic; also keyword form) |
| `@static` | member | (syntactic; also keyword form) |
| `@bigEndian` | packed struct | Big-endian byte order |
| `@littleEndian` | packed struct | Little-endian byte order (default) |
| `@allowPadding(N)` | packed struct | Allow padding to N bits (N must be power of two) |
| `@hashExclude` | field | Exclude from generated hash/pup |

---

### Literals (summary)

```
literal         ::= INT_LIT | FLOAT_LIT | STRING_LIT | CHAR_LIT | BOOL_LIT

INT_LIT         ::= ( decimal_int | hex_int | octal_int | binary_int ) int_suffix?
FLOAT_LIT       ::= ( decimal_float | hex_float ) float_suffix?
STRING_LIT      ::= '"' s_char* '"'
CHAR_LIT        ::= '\'' c_char '\''
BOOL_LIT        ::= 'true' | 'false'

int_suffix      ::= 'u8'|'i8'|'u16'|'i16'|'u32'|'i32'|'u64'|'i64'
float_suffix    ::= 'f32'|'f64'|'f16'|'bf16'
```

**Default types:** integer literal without suffix → `i32` (or `u64` in index position);
float literal without suffix → `f64`.

**Literal type inference:** Integer and float literals are contextually typed. `a[42]`
infers `42: u64` because array indexing demands `u64`. `val x = 42` without context
defaults to `i32`.

---

### Operator Precedence (low to high)

| Level | Operators | Notes |
|---|---|---|
| 0 (lowest) | assignment: `=`, `+=`, `-=`, `*=`, … | Right-associative |
| 1 | alphabetic identifiers as infix (`and`, `or`, …) | Left-associative |
| 2 | `\|` | |
| 3 | `^` | |
| 4 | `&` | |
| 5 | `<`, `>`, `<:`, `>:`, `==`, `!=`, … | |
| 6 | `+`, `-` | |
| 7 | `*`, `/`, `%` | |
| 8 | `as` (cast) | Right of `as` is a type, not an expr |
| 9 (highest) | unary prefix: `+`, `-`, `!`, `~` | |
| — | postfix: `.`, `[]`, `()`, `?`, `!` | Tightest binding |

Precedence is determined by the **first character** of an operator identifier. Custom
operators inherit the precedence of their first character.

---

### Special Type-Associated Constants

These are defined in the STL and are not grammar productions:

```ergoline
f32::nan      f64::nan
f32::inf      f64::inf
f32::negInf   f64::negInf
f32::negZero  f64::negZero
f32::max      f64::max
f32::min      f64::min      (* smallest positive normal *)
f32::epsilon  f64::epsilon
i8::min  i8::max   u8::min  u8::max
i16::min i16::max  u16::min u16::max
i32::min i32::max  u32::min u32::max
i64::min i64::max  u64::min u64::max
```

