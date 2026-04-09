# Toward EIRv2 — A Clean Rework

> Status: design notes / pre-proposal
> Context: builds on `code-review.md` and the 2022 dissertation

---

## Framing

EIRv2 is a clean-sheet redesign that keeps the core intellectual contributions of Ergoline/EIR —
migratable objects, SDAG, a rich type system — while cutting the Charm++/Hypercomm dependency,
fixing known design debt, and targeting intra-node execution first with a clear path to
multi-node (UCX) later.

The scope for an initial version is deliberately narrow:
- **Keep:** migratable objects (lite), SDAG/segmentation, coroutine execution, the core STL
- **Cut initially:** all other eDSLs (Stencil, Charisma, MSA, DivCon)
- **Cut entirely:** Charm++ `.ci` files, Hypercomm, `ck::updateGlobal`, chare arrays as a
  distributed primitive, `@createhome`/`@createhere`, nodegroups

---

## What Was Good About Ergoline

Before cutting, it's worth naming what worked well and should survive in some form:

### Language design wins
- **Migratable objects as first-class citizens.** Proxies, entry methods, and mailboxes are
  natural abstractions for actor-style concurrency. The `self@`, `self[@]`, `self{@}` proxy
  syntax was clever, even if the three-way split is Charm++-specific.
- **SDAG.** `when`/`await all`/`await any` with pattern-matching predicates is a genuinely
  expressive way to describe message-driven control flow. The segmentation pass that turns
  these into state machines is the compiler's strongest contribution.
- **Implicit variables.** Propagating the implicit proxy through call chains is ergonomic and
  avoids boilerplate.
- **Operator overloading + infix identifiers.** Scala-style "any identifier can be an operator"
  is excellent for DSL embedding and expression.
- **The STL is lean and expresssive.** `channel<A>` is 37 lines. `dht<K,V>` is 65 lines.
  These are good abstractions at an appropriate level of abstraction — no Charm++
  knowledge leaks into user code.
- **Template predicates (`where` clauses).** Ergoline's predicated specialization is more
  expressive than C++ `requires` and enables compile-time dispatch that would otherwise
  require SFINAE.
- **Tuple types and destructuring.** First-class tuples with destructuring assignment are
  ergonomic and well-integrated.
- **`@system` FFI.** Mapping Ergoline types onto C++ aliases, operators, and casts via
  annotations was practical and worked for the standard library.
- **The module system.** Python-style package inference from directory structure is clean.

### Compiler/IR design wins
- **The pass architecture.** `Pass` → `Registry` → `Processes.onLoad` is a clean pipeline.
  Topological ordering of passes is correct in principle.
- **`EirResolvable<T>` as a first-class concept.** Lazy resolution through placeholder nodes
  is a sound approach to handling forward references and circular types.
- **`TypeCheckContext` with specialization transactions.** The substitution/transaction model
  for template instantiation is the right approach.
- **FastParse.** The FastParse parser is clean and maintainable. The decision to move away
  from ANTLR4 was correct.

---

## Primitive Types

### No unsized types

Ergoline v1 inherited C's loosely-sized primitives: `int` (aliased to `std::int32_t` but
named as if platform-dependent), `long`, `short`, `double`, `char`. In an HPC context —
where data layout, SIMD width, serialization format, and wire protocol all depend on
knowing exact sizes — this is a footgun. EIRv2 eliminates all unsized primitive names.

Every numeric type has an explicit width. There are no surprises about layout.

### The primitive type table

| Type | Width | Description |
|------|-------|-------------|
| `bool` | 1 byte | Boolean — `true` or `false` |
| `byte` | 1 byte | Alias for `u8`; for byte buffers and I/O |
| `u8` | 8 bits | Unsigned integer |
| `i8` | 8 bits | Signed integer |
| `u16` | 16 bits | Unsigned integer |
| `i16` | 16 bits | Signed integer |
| `u32` | 32 bits | Unsigned integer |
| `i32` | 32 bits | Signed integer — **default integer literal type** |
| `u64` | 64 bits | Unsigned integer |
| `i64` | 64 bits | Signed integer |
| `f32` | 32 bits | IEEE 754 single precision |
| `f64` | 64 bits | IEEE 754 double precision — **default float literal type** |
| `char` | 8 bits | Alias for `i8`; kept as a convenience for ASCII/byte strings |
| `f16` | 16 bits | IEEE 754 half precision (optional — see below) |
| `bf16` | 16 bits | bfloat16 (optional — see below) |

**No `usize` / `isize`.** Pointer-width integers are a Rust-ism that leaks platform
details into user code. Array indices use `i64` by convention (large enough for any
realistic count, signed to allow sentinel values). If a specific binding needs a
pointer-width value, `@extern "C"` can declare it explicitly.

### `char` is `i8`

`char` is a convenience alias for `i8` — a signed byte. It has no special Unicode
semantics. Strings in EIRv2 are UTF-8 byte sequences; iterating over a `string` yields
`byte` (`u8`) values, not decoded code points. This is the HPC-pragmatic choice: string
processing in HPC is almost always byte-level (parsing, protocol headers, file I/O), and
Unicode decoding is a library concern when needed.

The name `char` is kept because `for (c <- "hello")` reading naturally. It is not a
distinct type — `i8 == char` is always true; they are interchangeable.

### `byte` is `u8`

Similarly, `byte` is a convenience alias for `u8`. Byte-buffer types read more clearly
as `array<byte>` than `array<u8>`. They are identical in every other respect.

### `f16` and `bf16` — provisional

`f16` (IEEE 754 half precision) and `bf16` (bfloat16, 8-bit exponent / 7-bit mantissa)
are included provisionally. Both are increasingly relevant for HPC and ML workloads
(GPU memory bandwidth reduction, tensor cores). The caveats:

- Neither has native arithmetic support on all CPUs. x86 gains `f16` arithmetic in
  AVX-512FP16; `bf16` arithmetic requires AVX-512BF16 or AMX. Without hardware support,
  operations must promote to `f32`.
- They are fully valid as **layout types**: storing, transmitting, and converting `f16`
  / `bf16` values is always supported. Arithmetic may require an explicit `.toF32()`
  round-trip on unsupported hardware.
- The compiler should emit a diagnostic (not an error) when `f16`/`bf16` arithmetic is
  used on a target without native support.

### Literal suffixes

Integer and float literals support explicit type suffixes. Without a suffix, `42` is
`i32` and `3.14` is `f64` — matching the defaults.

```ergoline
val a: i32  = 42;        // inferred from type annotation
val b        = 42i32;    // explicit suffix
val c        = 42u64;    // 64-bit unsigned
val d        = 3.14f32;  // single precision
val e        = 3.14f64;  // double precision (same as 3.14)
val f        = 255u8;    // max u8
val g        = -1i8;     // min negative i8
```

Literals that overflow their declared type are a compile error, not silent truncation.

### What this replaces from v1

| Ergoline v1 | EIRv2 | Notes |
|-------------|-------|-------|
| `int` | `i32` | Same underlying type; explicit name |
| `long` | `i64` | Same underlying type; explicit name |
| `short` | `i16` | Same underlying type; explicit name |
| `double` | `f64` | Same underlying type; explicit name |
| `float` (EirFloatLiteral, labeled "double") | `f32` | The v1 naming bug is gone |
| `char` | `char` / `i8` | Now explicit alias; no semantic change |
| `bool` | `bool` | Unchanged |

No source-level migration path is provided for the name changes — this is a clean break.
The v1 names (`int`, `long`, `double`, etc.) are not reserved; they could be user-defined
type aliases if someone wants them, but the STL does not define them.

---

## What to Cut / Redesign

### Cut entirely
- **Charm++ `.ci` files and Charmxi.** EIRv2's compiler emits all registration code
  directly — it has full knowledge of all chare types and entry methods at compile time.
  A `register_all()` function (or equivalent) replaces the `.ci` → Charmxi → `.decl.h`
  pipeline.
- **`EirProxy` three-way split** (`@`, `[@]`, `{@}`). The element/section/collective
  distinction is Charm++ array semantics. In EIRv2, proxy kinds simplify: a proxy is a
  handle to an actor (singleton or collection member). Sections become a library concern.
- **`ck::updateGlobal`, `ck::exitAfterQuiescence`, `ck::awaitQuiescence`.** These are
  Charm++ runtime hooks. EIRv2's runtime exposes its own lifecycle API.
- **`@createhome` / `@createhere`.** Demand-creation semantics can be re-expressed as
  regular `new` with routing hints if needed later.
- **`@async` as a distinct annotation.** In EIRv2, `async` is a first-class expression
  modifier, not an annotation. Every non-`@entry` call to a remote actor is inherently
  async; the return type is inferred as `future<T>`.
- **Nodegroups.** Intra-node only for now; revisit for multinode.
- **`tspace<K,V>`.** Too Charm++-specific; DHT subsumes it.
- **`blas`.** External library wrapper; can be an `@system` binding if needed.
- **`Stencil`, `Charisma`/`@charisma`, `MSA`, `DivCon` eDSLs.**

### Redesign
- **`EirTemplateArgument` and `EirTypeAlias` are not types.** The single biggest type
  system correctness issue in EIR. Both incorrectly extend `EirType`, causing confusion
  throughout resolution and codegen. EIRv2 separates them cleanly.
- **`EirReturn.expression` is not optional.** A void return synthesizes `unitLiteral` at
  parse time. Fix by making it `Option[EirExpressionNode]` in the AST.
- **`replaceChild` stubs.** The tree mutation API is half-implemented. EIRv2 should either
  implement it completely or replace it with a structural rewrite pass that doesn't mutate
  in-place.
- **`deepCloneTree` no-op.** The buffer-reuse optimization requires real tree cloning.
  Either implement it or adopt a purely functional/persistent AST where "cloning" is free.
- **`TopologicalSort`.** Replace the O(n²) greedy algorithm with Kahn's algorithm.
- **`object` singletons.** In an intra-node context, `object` is simply a thread-safe lazy
  singleton — no broadcast required. It can stay as syntactic sugar over a standard
  singleton pattern, but the `ck::updateGlobal` coupling is removed.
- **`@overlap for`.** See SDAG section below.

---

## Execution Model: Coroutines over ULTs

The dissertation notes that SDAG segmentation already decomposes `@threaded @entry` methods
into continuation state machines — exactly what coroutine transformation does. ULTs (user-level
threads with their own stacks) were needed in Charm++ because the SDAG library operated at
runtime, not compile time. Since EIRv2's segmentation pass operates at compile time, there is
no reason to pay the ULT overhead.

**EIRv2 execution model:**

```
Actor (chare instance)
  ├── mailbox slots (per @mailbox declaration)
  ├── coroutine state machine (generated by segmentation pass from @entry def)
  └── data members
```

Each actor runs on a thread from a work-stealing pool. Entry method invocations are
asynchronous messages. Structured entry methods (`when`/`await`) are lowered to coroutine
suspension points — the segmentation pass produces a state machine, and the runtime drives
it forward when the required messages arrive.

**Key properties:**
- No stack per actor (coroutines are stackless by default)
- Suspension only at explicit `when`/`await` points — non-preemptive within an entry method
- Work-stealing pool underneath; number of OS threads = hardware concurrency (configurable)
- No per-actor thread affinity initially; migration is a scheduler decision

**`@threaded` in EIRv2:** Becomes the default for any entry method containing `when`/`await`.
The annotation is no longer needed — the compiler detects suspension points and generates a
coroutine automatically. For entry methods with no suspension, plain function calls suffice.

### `@overlap for` → structured concurrency

The `@overlap for` construct means "all iterations are independent, run them concurrently,
implicit barrier at end." This is structured concurrency. Propose replacing it with an
explicit `concurrent for` keyword with the same semantics, or a `parallel for` that makes
the barrier explicit:

```ergoline
// EIRv2 proposal: structured concurrent loop
for concurrent (i <- 0 to n) {   // all iterations launch concurrently
    when receive(_ == i, data) => process(i, data);
}
// implicit join here — all iterations must complete before continuing
```

Alternatively, adopt a `spawn`/`sync` model from Cilk:
```ergoline
for (i <- 0 to n) {
    spawn {
        when receive(_ == i, data) => process(i, data);
    }
}
sync; // explicit barrier
```

The `spawn`/`sync` model is more composable and maps cleanly onto structured concurrency
primitives. **Recommendation: adopt `spawn`/`sync` or an equivalent `async { } / await`
block form, and make the barrier explicit.**

---

## SDAG in EIRv2

SDAG is the one eDSL to keep. Two questions: (1) how should it be lowered, and (2) should
`when`/`await` be language built-ins or STL abstractions?

### Lowering strategy: compile-time segmentation (keep)

The segmentation pass approach — transforming structured entry methods into state machines
at compile time — is the right approach. It is superior to the Charm++ runtime-library
approach because:
- Local variables within structured entry methods are first-class (not banned as in Charmxi)
- Type checking of `when` predicates is possible
- The generated code has no runtime SDAG overhead, only the coroutine state machine

In EIRv2, the segmentation pass outputs coroutine frames (or equivalent continuation-passing
code) rather than SDAG continuation objects. The runtime only needs to know how to deliver
messages and resume suspended coroutines — it does not need SDAG-specific logic.

### Language built-ins vs. STL abstractions

**Built-ins (current approach):**
- `when`, `await all`, `await any` are syntactic keywords
- `@mailbox` is an annotation on method declarations
- The compiler must understand them to perform segmentation

**STL abstractions:**
- `when` could be expressed as a method on a mailbox type: `mailbox.when(pred).then(body)`
- `await all` / `await any` become combinators: `Mailbox.all(m1, m2).then(...)`

**Tradeoffs:**

| | Built-ins | STL abstractions |
|---|---|---|
| Segmentation pass complexity | Lower (direct AST analysis) | Higher (must recognize patterns in arbitrary expressions) |
| Expressiveness | Fixed syntax, limited composability | Composable, mixable with other expressions |
| Type checking | Direct | Must be designed carefully |
| Python embedding | Requires Python syntax wrappers | Can expose as Python class methods naturally |
| User extensibility | None | Can subclass/extend |

**Recommendation:** Keep `when`/`await all`/`await any` as language built-ins for the
compiled path — the segmentation pass requires syntactic recognition and there is no
compelling reason to fight that. For the Python embedding (nanobind) path, the runtime
exposes a `Mailbox` class with `.recv()`, `.when()`, and `Mailbox.select()` (equivalent to
`await any`) as regular Python awaitable methods. The two paths do not need to share syntax.

### `@mailbox` in EIRv2

`@mailbox` declares a typed message queue slot on an actor. This maps cleanly onto a
`std::deque<T>` (or similar) field with a condition variable or coroutine wakeup. The key
semantics:

```ergoline
// EIRv2: mailbox declaration (unchanged syntax)
@mailbox def receive(it: int, data: array<double>);

// usage in structured entry method
when receive(_ == it, data) => { process(data); }
```

The `@mailbox` annotation can remain. What changes is the lowering: instead of generating
SDAG continuation objects, the compiler generates a coroutine that suspends at `when` and
is resumed by the runtime's message delivery.

---

## Multi-node Design: Not Boxing into Message Passing

The intra-node runtime is a work-stealing coroutine scheduler. When expanding to multi-node
via UCX, the critical design decisions are:

**What to expose at the language level:**
- An actor's "location" should be opaque to user code — proxies should work identically
  whether the target is local or remote
- Message serialization should be compiler-generated (ergc knows all types)
- One-sided RDMA operations should be expressible as language-level operations, not just
  as C-level calls

**Proposed design principle:** The runtime exposes three communication primitives:

1. **Send** — fire-and-forget message to an actor (maps to UCX `ucp_am_send` or similar)
2. **Get/Put** — one-sided RDMA read/write into a named buffer registered with an actor
   (maps to `ucp_get`/`ucp_put`)
3. **Future** — a value that will arrive from a remote computation

These three primitives cover the full space. Message-passing (`send`) is a special case of
one-sided (sending triggers a handler). The language should not commit to only message-passing.

**Implication for proxy design:** A proxy in EIRv2 should carry enough information to
support both message-dispatch and direct memory operations. For intra-node, a proxy is a
pointer + actor ID. For multi-node, a proxy is a global address (node ID + actor ID) plus,
optionally, a registered memory key for RDMA.

This means `future<T>` and RDMA gets share the same abstraction level — both are "wait for
a value to arrive" from the language's perspective.

---

## Surface Language: New Syntax vs. Python Embedding

### Option A: New compiled language (Ergoline successor)

Keep the Scala-inspired surface, fix the grammar gaps, ship a clean language.

**Pros:**
- Full compile-time analysis (SDAG segmentation, type checking, specialization)
- No GIL
- Can express the full type system correctly
- Coroutine lowering is natural
- The existing FastParse grammar is a solid starting point

**Cons:**
- Need to maintain a compiler
- Smaller audience; harder to attract users

### Option B: Python embedding via nanobind

Expose the EIRv2 runtime as a Python extension module. Actors are Python classes decorated
with `@actor`. Entry methods are `async def`. SDAG `when`-clauses become `await mailbox.recv()`.

```python
from eirv2 import actor, mailbox, future

@actor
class Jacobi:
    receive_ghost = mailbox(int, int, np.ndarray)  # (it, dir, data)

    async def run(self):
        for it in range(self.max_iters):
            self.send_ghosts(it)
            for _ in range(self.num_neighbors):
                it_, dir_, data = await self.receive_ghost.when(lambda it_, *_: it_ == it)
                self.process_ghost(dir_, data)
```

**Pros:**
- Python ecosystem (NumPy, SciPy, etc.) immediately available
- No new syntax to learn
- nanobind gives C-speed Python↔C++ transitions
- Excellent for research / rapid prototyping
- GIL-less Python (3.13+) removes the main objection

**Cons:**
- Python's type system is structural/duck-typed — the predicated generics and variance of
  Ergoline's type system are inexpressible without a separate type annotation layer
- SDAG's compile-time segmentation pass is fundamentally a compiler analysis. Without a
  compiler, `when`-clauses must be implemented as runtime polling/callbacks — losing the
  zero-overhead state machine lowering that is EIR's key optimization
- Serialization reverts to Python pickle or custom `__reduce__` — loses the
  zero-copy/PUP optimizations
- Cannot enforce the "no data sharing between actors" invariant at compile time
- `await mailbox.recv()` does not compose as cleanly as `when m1(...), m2(...) =>` for
  compound waits across multiple mailboxes

### Recommendation: compiled language primary, Python bindings for the runtime

The segmentation pass and the type system are the core value propositions. Neither can be
replicated in pure Python. The right model is:

1. **EIRv2 compiler** (compiled language) — full type checking, SDAG segmentation, codegen
2. **EIRv2 Python runtime** (nanobind extension) — expose actors, futures, and mailboxes
   as Python objects; no SDAG segmentation, but the runtime primitives are available for
   Python actors that don't need structured entry methods

This mirrors the Charm++/Charm4Py split: the compiled path is high-performance, the Python
path is productive. The runtime is shared.

For the Python bindings specifically:
- `@actor` decorator registers a Python class as an actor type
- `mailbox(T)` is a descriptor that creates a typed mailbox slot
- `async def entry(self, ...)` marks entry methods; Python's `asyncio` or a custom event
  loop drives them
- `await actor_proxy.method(args)` sends a message and returns a future

---

## Type System: Fixing It Properly

The main correctness problems in EIR's type system:

### 1. Separate type hierarchy from template metadata

```
EirNode
  ├── EirType (resolved, concrete types)
  │     ├── EirClassLike (EirClass, EirTrait, EirProxy)
  │     ├── EirTupleType
  │     ├── EirLambdaType
  │     └── EirTemplatedType
  └── EirTemplateParameter  (← NOT a type)
        ├── name: String
        ├── variance: Variance
        ├── bounds: (Option[Type], Option[Type])
        └── argumentType: Option[Type]
```

`EirTemplateArgument` should never appear where a `EirType` is expected. It is metadata
*about* a type position, not a type itself. When a template argument is used as a type in
an expression, it should be wrapped in `EirTemplateFacade` (which already exists for this
purpose).

Similarly, `EirTypeAlias` should not be a type — it is a declaration that resolves to a
type. The resolution pass should always fully expand aliases before type-checking.

### 2. Variance: implement or cut

Variance (`+A` covariant, `-A` contravariant) is correctly parsed and type-checked in
Ergoline but the C++ codegen doesn't handle it. The root cause is that C++ has no notion
of variance — you can't cast `vector<Cat>` to `vector<Animal>`.

In EIRv2, if the codegen target emits monomorphized code (see below), variance becomes
straightforward: each `Foo<Cat>` and `Foo<Animal>` are distinct generated types, and
variance relationships are checked at the ergc level only. The generated code doesn't need
to know about variance at all.

**Recommendation: keep variance, implement it correctly via monomorphization.**

### 3. `EirReturn` optionality

`EirReturn.expression: EirExpressionNode` → `EirReturn.expression: Option[EirExpressionNode]`

The parser currently synthesizes `unitLiteral` for bare `return;`. Fix this in the AST and
propagate through type checking. A `return;` in a `unit`-returning function is valid;
in a non-`unit` function it is an error.

### 4. Trait `self` declarations

Traits currently don't get a `self` declaration. This means trait methods can't refer to
`self` for member access without special-casing. Fix in EIRv2 — all traits get a `self`
with the trait's own type.

---

## Codegen: Monomorphized C++ (No Templates in Output)

The fundamental problem with EIR's current C++ codegen: it emits C++ templates, which
means the C++ compiler re-does a version of the type-checking and instantiation that
ergc already performed. This causes the covariance/contravariance issue and makes errors
hard to attribute.

**EIRv2 proposal: monomorphized codegen**

ergc collects all specializations used in a program (it already tracks these in
`TypeCheckContext.checked`) and emits one concrete C++ class per specialization. No C++
templates appear in the output.

```
// Ergoline source
class box<A> { val value: A; }
val x: box<int> = new box<int>(42);
val y: box<string> = new box<string>("hi");

// EIRv2 generated output (monomorphized)
struct box__int { int value; ... };
struct box__string { std::string value; ... };
```

**Advantages:**
- Covariance/contravariance is a pure ergc concern — the C++ output is oblivious to it
- C++ compilation is faster (no template instantiation)
- Error messages are simpler
- Enables variance-correct casting without C++ hacks
- `EirTemplateArgument extends EirType` fix is natural — template args never appear in output

**Registration without `.ci`:**

ergc emits a `eirv2_register()` function in the generated `.cc` file:

```cpp
void eirv2_register() {
    runtime::register_actor<Jacobi>("examples::jacobi2d::jacobi2d");
    runtime::register_entry<Jacobi, &Jacobi::run>("run");
    runtime::register_entry<Jacobi, &Jacobi::receive_ghost>("receive_ghost");
    // ...
}
```

The runtime calls this at startup. No separate `.ci` compilation step, no Charmxi.

**FFI in a monomorphized C++ world:**

`@system` bindings still work well — ergc simply emits the alias in the monomorphized
output. The FFI story actually improves because there are no C++ template complications
in the generated bindings.

If the Python embedding path is also in scope, the alternative is a **ctypes-style FFI**
for anything the compiler doesn't know about statically. The tradeoff:

| | `@system` (C++ FFI) | ctypes-style |
|---|---|---|
| Performance | Zero overhead | Small overhead per call |
| Expressiveness | Full C++ types (templates, overloads) | C ABI only |
| Build complexity | Requires C++ compiler for binding | Link to any `.so` |
| Python path | Must wrap C++ in nanobind separately | Directly callable from Python ctypes |

**Recommendation:** Keep `@system` for the compiled path (it works well). Add a separate
`@extern "C"` annotation for ctypes-compatible declarations that work from both the compiled
and Python paths. This gives the best of both worlds without the build complexity of
maintaining full C++ template binding machinery.

---

## Compiler Implementation Language

The case for ditching Scala:
- Scala 2.13 → 3 migration is a significant maintenance burden
- SBT is slow and the toolchain is heavyweight
- The JVM startup overhead is annoying for a compiler CLI
- FastParse, while good, is not as fast as native parsers
- Scala's type system is being used to implement another language's type system —
  this creates an impedance mismatch that has caused several of the bugs found in this review

### Options

**Rust**
- Strong type system enforces correct AST representation
- Excellent performance; native binary
- Good ecosystem: `logos` (lexing), `chumsky` or `pest` (parsing), `inkwell` (LLVM bindings)
- Memory safety without GC — important for a long-running compiler server
- `salsa` framework for incremental computation (query-based compiler architecture)
- Cons: steep learning curve; borrow checker fights recursive AST structures (use `Arena`
  allocation or `Rc<RefCell<>>`)

**Python (compiler prototype)**
- Fast iteration; good for experimenting with type system designs
- `lark` for parsing, `pydantic` for AST nodes
- Cons: slow for large programs; type system of the *implementation* language (Python) is
  weaker than what EIRv2 is trying to implement

**TypeScript**
- Surprisingly viable: the TypeScript compiler itself is written in TypeScript
- Fast iteration, good tooling, easy to hire for
- `chevrotain` or `nearley` for parsing
- Cons: performance ceiling; less suitable for the runtime half

**OCaml**
- Traditional compiler language; algebraic data types match AST structure perfectly
- `menhir` for parsing
- Cons: smaller ecosystem; niche

**Recommendation: Rust for production, Python for the initial prototype**

The type system work (fixing variance, separating template parameters from types, implementing
monomorphization) is well-suited to Rust's algebraic types and pattern matching. A Rust
compiler also naturally produces a fast native binary that can be distributed without a JVM.

For the initial research prototype, a Python implementation is faster to iterate on. Once the
design stabilizes, rewrite in Rust. The two implementations can coexist during the transition
(the Python prototype can be used to validate test cases for the Rust implementation).

---

## Summary: Keep / Redesign / Cut

### Keep (with minor changes)
- Migratable objects model: actors, proxies, entry methods, mailboxes
- SDAG: `when`, `await all/any`, `@mailbox`, segmentation pass (lowered to coroutines)
- Template predicates (`where` clauses)
- Tuple types and destructuring
- `val`/`var`, `match`/`case`, pattern matching
- `import`/`package` module system
- Infix identifiers as operators
- Implicit variables/parameters
- Primitives: `bool`, `byte`/`u8`, `i8`, `u16`, `i16`, `u32`, `i32`, `u64`, `i64`,
  `f32`, `f64`, `char` (= `i8`), provisional `f16`/`bf16`
- Core STL: `array`, `slice`, `queue`, `map`, `string`, `option`, `result`, `either`,
  `range`, `iterator`/`iterable`, `future`, `channel`, `dht`, `uid`, `math`, `status` trait
- `@system` FFI (compiled path)
- FastParse grammar as starting point (with primitive names updated)

### Redesign
- **Primitive types:** `int`→`i32`, `long`→`i64`, `short`→`i16`, `double`→`f64`,
  `float` (mislabeled in v1) → `f32`; `char` kept as alias for `i8`; `byte` alias for `u8`;
  no unsized types, no silent platform-dependent sizing
- Error propagation: `result<T, E>` + `?` + `!` operators replace exceptions and absl macros
- Type hierarchy: separate `EirTemplateArgument`/`EirTypeAlias` from `EirType`
- Variance: implement correctly via monomorphization
- `@threaded` → implicit from suspension points; annotation removed
- `@async` → implicit from return type inference; annotation removed
- Proxy kinds: simplify to handle + optional collection index; remove Charm++ three-way split
- `@overlap for` → `concurrent for` or `spawn`/`sync`
- Registration: ergc emits `eirv2_register()`, no `.ci` files
- Codegen: monomorphized C++, no templates in output
- Compiler implementation: Rust (production) / Python (prototype)
- `object` singletons: keep as syntactic sugar, remove `ck::updateGlobal` coupling

### Cut
- All eDSLs except SDAG (Stencil, Charisma, MSA, DivCon)
- Charm++ `.ci` / Charmxi pipeline
- Hypercomm dependency
- Nodegroup/group proxy kinds (revisit for multinode)
- `ck::updateGlobal`, `ck::exitAfterQuiescence`, `ck::awaitQuiescence`
- `@createhome` / `@createhere`
- `tspace`, `blas`, `mailbox` (as a library class — becomes a language primitive)
- ANTLR4 grammar and `Visitor.scala`
- `EirClosure` (unused/stub)
- `EirTypeOf` (commented out, abandoned)
- `Processes.modules` dead variable
- The `???`-stub `replaceChild` implementations (replace with functional rewrite)

---

## Error Propagation: `result<T, E>` and `option<T>`

Ergoline deliberately omitted exceptions (standard HPC practice — `-fno-exceptions` is
common). EIRv2 formalizes this: no exceptions, no `abort`-on-error in library code. Error
propagation is explicit, lightweight, and statically enforced by the compiler. The design
replaces the need for anything like `absl::StatusOr` or `std::expected` — those are
library workarounds for a language that lacks the feature natively.

### `result<T, E>` — the error type

```ergoline
// STL: result.erg
abstract class result<T, E> {
    def isOk(): bool;
    def isErr(): bool;
    def get(): T;               // panics if err — use only when certain
    def getErr(): E;            // panics if ok
    def map<U>(f: T => U): result<U, E>;
    def flatMap<U>(f: T => result<U, E>): result<U, E>;
    def getOrElse(default: T): T;
    def ignore(): unit;         // explicit opt-out of must_use
}

class ok<T, E>(val value: T) extends result<T, E>;
class err<T, E>(val error: E) extends result<T, E>;
```

`result<T, E>` is `must_use` — the compiler issues an error if a `result` return value
is silently dropped. The only way to suppress this is calling `.ignore()`, which is
visible and grep-able in code review.

```ergoline
readFile("foo.erg");          // ERROR: result<string, ioError> is must_use
readFile("foo.erg").ignore(); // OK: explicit discard
```

`E` is unconstrained — any type is a valid error. `result<int, string>` is valid for
quick ad-hoc errors. The STL provides a `status` trait for callers that want richer
error information:

```ergoline
trait status {
    def message(): string;
    def code(): int;
    def withContext(msg: string): self;  // wrap with additional context
}
```

Using `status` is optional. Nothing in the language requires it.

### `?` — propagate `err` upward

The `?` postfix operator unwraps a `result<T, E>`, returning the inner `T` on success or
immediately returning `err(e)` from the enclosing function on failure. It is only valid
inside a function whose return type is `result<U, F>` where `E` is assignable to `F`.

```ergoline
def readFile(path: string): result<string, ioError> { ... }
def parseAst(src: string):  result<ast, ioError>    { ... }

def compile(path: string): result<module, ioError> {
    val src  = readFile(path)?;  // propagates ioError on failure, unwraps on success
    val tree = parseAst(src)?;   // same
    return ok(lower(tree));
}
```

This replaces the entire `RETURN_IF_ERROR` / `ASSIGN_OR_RETURN` macro family from
`absl`. There are no macros — `?` is a real grammar production that the type checker
understands. The equivalence:

| absl / C++ pattern | EIRv2 |
|---|---|
| `RETURN_IF_ERROR(expr)` | `expr?;` |
| `ASSIGN_OR_RETURN(var, expr)` | `val var = expr?;` |
| `StatusOr<T>.value()` | `.get()` or `match` |
| Silent ignore | Compile error |
| `Status::OkStatus()` | `ok(value)` |

### `!` — propagate `none` upward

`option<T>` uses a separate operator `!` for propagation, deliberately distinct from `?`.
`!` unwraps a `some(v)` to `v` or immediately returns `none` from the enclosing function.
It is only valid inside a function whose return type is `option<U>`.

```ergoline
def lookup(m: map<string, int>, k: string): option<int> {
    val x = m.get("a")!;  // returns none if missing, unwraps if present
    val y = m.get("b")!;
    return some(x + y);
}
```

The separation from `?` is intentional. Mixing `option` and `result` requires an explicit
conversion — the compiler forces you to name what `none` means in a `result`-returning
context:

```ergoline
def load(path: string): result<int, ioError> {
    val src = readFile(path)?;

    // This is a compile error — ! inside a result-returning function:
    val n = parseInt(src)!;

    // Correct: explicitly convert none to an err
    val n = parseInt(src).okOr(ioError("not a number"))?;

    return ok(n);
}
```

This catches a real class of bugs: `none` and `err` are semantically different. A missing
value is not automatically an I/O error, a parse error, or a logic error — the programmer
must say which. The compiler enforces this at the `option`→`result` boundary.

### Composition

`option<T>` and `result<T, E>` compose via STL methods:

```ergoline
// option -> result
val r: result<int, string> = opt.okOr("value was missing");
val r: result<int, E>      = opt.okOrElse(() => computeError());

// result -> option (discard the error)
val o: option<int> = res.ok();    // some(v) on success, none on error
val o: option<E>   = res.err();   // some(e) on error, none on success
```

Pattern matching works uniformly across both:

```ergoline
match parseFile("foo.erg") {
    case ok(tree) => compile(tree);
    case err(e)   => println(`error: ${e.message()}`);
}

match lookup(map, "key") {
    case some(v) => use(v);
    case none()  => println("not found");
}
```

### Summary of operators

| Operator | Valid inside | On success | On failure |
|---|---|---|---|
| `expr?` | `result<U, F>`-returning fn | unwraps to `T` | returns `err(e)` |
| `expr!` | `option<U>`-returning fn | unwraps to `T` | returns `none` |

Both are postfix, both are single-character, both are compile-time checked. No macros,
no exceptions, no runtime overhead beyond the branch already implied by the type.

---

## Type Parameter Syntax: Keep `<>`

EIRv2 keeps `<>` for type parameters and type arguments, matching C++, Java, Rust, Go,
and Swift. The alternative (`[]`, Scala-style) has a concrete conflict with array indexing:
EIRv2 uses `a[i]` for indexing (C-style), so `a[int]` in expression position is ambiguous
between type application and array access. Scala avoids this by using `a(i)` for indexing,
which we do not want. The disambiguation `<>` requires in the FastParse grammar is already
handled cleanly and ANTLR4 (where it was painful) is being dropped.

---

## Self-hosting

EIRv2 is designed to eventually compile itself. This is a concrete milestone that:
- Validates that the language is expressive enough for non-HPC general programming
- Forces the core sequential subset (classes, generics, pattern matching, `result`,
  string handling, file I/O) to be solid before any actor/SDAG features are needed
- Provides a natural test suite: the bootstrap compiler and the self-hosted compiler
  must agree on all inputs

The self-hosting compiler is a purely sequential program — no actors, no SDAG, no entry
methods. It exercises exactly the subset that needs to work correctly first.

**What EIRv2 already has for self-hosting (from Ergoline):**
- Recursive class hierarchies + traits for AST nodes ✓
- Generics + predicates for typed containers ✓
- Pattern matching / `match`-`case` for AST dispatch ✓
- `option<T>`, `map<K,V>`, `array<T>`, `queue<T>` ✓
- String + interpolation ✓
- Infix operators for builder-style APIs ✓

**What needs to be added or strengthened:**
- `result<T, E>` with `?`/`!` (designed above — not yet in Ergoline)
- **File I/O** — entirely absent from Ergoline's STL; needs a thin `@extern "C"`
  wrapper over `fopen`/`fread`/`fwrite`/`fclose` or a `stdio` module
- **Richer string library** — `split`, `indexOf`, `startsWith`, `trim`, `padLeft`;
  the current `string.erg` is thin
- **`int64` / `long` arithmetic** — needed for file offsets and hash values; `long.erg`
  exists but needs more coverage
- **Exit codes** — `exit(n: int)` needs to be callable from non-actor code

**Bootstrap path:**

```
Phase 1: Rust (or Python) bootstrap compiler
         → compiles EIRv2 source → C++ output

Phase 2: EIRv2 compiler written in EIRv2
         → compiled by Phase 1 bootstrap
         → output compared against bootstrap for equivalence

Phase 3: Self-hosted compiler replaces bootstrap
         → CI runs both compilers on the full test suite
         → they must agree
```

The language is rich enough that Phase 2 is not a large lift. The gap is almost entirely
in the I/O and string surface area, not in the type system or control flow.

---

## Open Questions

1. **Lambda serialization:** For intra-node only, lambdas can be passed by value — no
   static registration table needed. For multinode, the same problem as Ergoline recurs
   (consistent function addresses across processes). Defer to the multinode design phase.

2. **`@entry` on constructors:** The `def self(...)` constructor convention is
   Charm++-specific. For EIRv2, `new Actor(args)` with a conventionally-named constructor
   method is cleaner; ergc generates the registration entry automatically. Whether `self`
   is kept or replaced with the class name is a stylistic decision to make before the
   grammar is finalized.

3. **GC vs. ARC vs. ownership:** Ergoline used ARC with optional `owned`/`borrowed`
   qualifiers. For an intra-node actor model with the "no sharing between actors" invariant,
   strong ownership (Rust-style) is the natural fit and could be made a compile-time
   guarantee, simplifying the runtime significantly.
