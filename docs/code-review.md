# Ergoline Codebase Review — Findings Report

> Reviewed: 2026-04-08
> Dissertation: *Building Blocks for High-Performance Languages*, Justin Szaday, UIUC 2022

## Project Overview

Ergoline is the implementation artifact of a PhD dissertation (advisor: Prof. Laxmikant
"Sanjay" Kale, UIUC Parallel Programming Lab). It is explicitly described as a
**pre-alpha proof-of-concept**, not a production system — context that explains many of
the stub implementations found in this review.

The project is a three-layer stack:

```
Ergoline (language)          ← user-facing .erg source language
         ↓
EIR (compiler framework)     ← this repo: AST, type system, passes, codegen
         ↓
Charm++ + Hypercomm (RTS)    ← external dep (hypercomm/ git submodule)
```

**Ergoline** is a general-purpose, object-oriented language that raises Charm++'s
*migratable objects* to first-class citizens. It draws syntactic inspiration from Scala and
Swift, and is designed as a platform for embedding HPC DSLs.

**EIR** (Ergoline IR) is the compiler framework in this repo. It is explicitly designed to
be retargeted by other languages beyond Ergoline itself (the dissertation calls this the
projected "EIRv2" roadmap).

**Hypercomm** is a C++ runtime library (the git submodule) that closes semantic gaps
between EIR's abstractions (per-chare futures, lambda serialization, channels, spanning
trees) and raw Charm++.

The compiler pipeline is:
FastParse/ANTLR4 → AST → resolution → type-checking → code generation → `.cc` + `.ci`

---

## Example Programs — DSL/Feature Labels

34 example programs in `examples/`. Labels:
- **SDAG** — uses `@mailbox`, `when`-clauses, `await all/any`, `@overlap for` (Ch. 3.1)
- **Stencil** — uses `@stencil`, `stencil::_`, `foreach`, `boundary` (Ch. 3.2)
- **Charisma** — uses `@charisma`, `or::placeholder`, `or::ispace` (Ch. 3.3)
- **Core** — exercises base Ergoline / EIR features, no embedded DSL

| File | Label | What it exercises |
|------|-------|-------------------|
| `activator.erg` | Core | Traits, multi-level pattern matching, generic functions with type predicates (`where A == lion`), `typeid`, extractor objects |
| `awaiter.erg` | Core | `@async @entry`, `await` on futures, `@threaded @entry` |
| `cannon.erg` | **SDAG** | Compound `when` across two mailboxes with refnum matching, `@overlap for`, `blas::dgemm` via FFI, 2D chare arrays — dissertation §2.5.2 |
| `fib.erg` | Core | Basic chares, abstract proxy via trait (`acceptor@`), singleton objects, divide-and-conquer — dissertation §2.5.1 |
| `functorcheck.erg` | Core | Lambdas, higher-order functions, functor pattern (`apply`) |
| `globcheck.erg` | Core | Singleton objects, `ck::updateGlobal`, broadcast-then-callback pattern |
| `greeter.erg` | Core | Generic entry methods (`greet<A>`), 1D chare arrays, chain messaging |
| `hello.erg` | Core | Manual future chaining, `future.then()`, `@threaded @entry`, `await` |
| `hello1d.erg` | Core | `ck::tspace` (tuple space STL), array1d chares, tuple destructuring |
| `hello2d.erg` | Core | 2D chare arrays, match/case on tuples, sequential element-to-element messaging |
| `hello6d.erg` | Core | 6D chare arrays, `@createhome` demand creation, dynamic array insertion |
| `implicitest.erg` | Core | Implicit variables/parameters, propagation through nested calls |
| `itercheck.erg` | Core | Iterators, `zip`, `map` (lambda), 2D array slicing (`a[2, :]`), slice assignment |
| `jacobi2d.erg` | **SDAG** | `when` with refnum matching, `@overlap for` inside `@threaded @entry`, per-element reductions, array slice halo exchange — dissertation §2.5.3 |
| `matmul.erg` | Core | Sequential 2D matrix multiply, `array<double, 2>`, local computation only |
| `maxHolder.erg` | Core | Generic classes, FFI (`@system` for `rand`/`srand`/`time`), partial type inference, traits |
| `nodes.erg` | Core | Nodegroup + group proxy kinds, `ck::nodeFirst`/`ck::nodeSize`, cross-proxy communication |
| `opcheck.erg` | Core | Operator precedence correctness, bitwise ops, string concatenation |
| `opttest.erg` | Core | `option<A>`, `either<L,R>`, generic match/case, `flatMap`, extractor patterns (`some`/`none`/`left`/`right`) |
| `orcheck.erg` | **Charisma** | `@charisma @entry`, `or::placeholder<int,1>`, `or::ispace`, `@local @entry` methods |
| `pi.erg` | Core | 1D chare array, `contribute` with reducer (`double::+`), parallel reduction to main |
| `pingpong.erg` | **SDAG** | `await all { when ... }` bundled construct, `@overlap for`, typed pattern matching in when-clauses (`msg: ping`) |
| `printer.erg` | Core | Generic entry methods, `ck::exitAfterQuiescence` |
| `puptest.erg` | Core | De/serialization (PUP) of complex types, lambda capture over entry method boundary, identity comparator (`===`), tuple composition |
| `receiver.erg` | Core | `ck::channel<A>` (STL channel), `await ch` syntax for blocking receive, point-to-point messaging |
| `refcheck.erg` | Core | Reference types (`int&`), pass-by-reference (`&j`), tuple destructuring assignment, `range.zip` |
| `relaxation.erg` | **Stencil** | `@stencil def`, `stencil::_`, `foreach({...})` kernel, `boundary(grid)`, implicit all-reduce on convergence check |
| `sdagtest.erg` | **SDAG** | `await any { when ... }`, nested `@overlap for` (doubly-overlapped), refnum matching on both iteration and neighbor, most SDAG-complete example |
| `sharing.erg` | Core | `ck::dht<K,V>` (distributed hash table STL), `@threaded @entry`, `await` on DHT get |
| `slicecheck.erg` | Core | Chare-array sections (`arrProxy[0:2:n]`), section contributions, index-array sections (`self@[odds]`) — Hypercomm section feature |
| `strtest.erg` | Core | `array<string>`, `queue<string>`, string interpolation, character iteration over string |
| `tester.erg` | **SDAG** | Benchmark comparing mailbox vs. plain entry method throughput; compound `when` in `@overlap for`, `ck::awaitQuiescence` |
| `traitcheck.erg` | Core | `abstract class`, `struct` (value type), trait, pass-by-value vs. pass-by-reference semantics, `assert`/`assertNot` |
| `uidtest.erg` | Core | `uid` STL type, uniqueness invariants, PE-local UID generation |

### Summary by label

| Label | Count | Files |
|-------|-------|-------|
| **SDAG** | 6 | `cannon`, `jacobi2d`, `pingpong`, `sdagtest`, `tester`, plus SDAG constructs also in `jacobi2d` |
| **Stencil** | 1 | `relaxation` |
| **Charisma** | 1 | `orcheck` |
| **Core only** | 26 | all others |

No examples exercise **MSA** or **DivCon** — the dissertation describes both as
incomplete/future work (§3.4–3.5), and neither has `.erg` examples here.

### Notes on coverage gaps

- **`await any`** is only exercised in `sdagtest.erg` — the most thorough SDAG test.
- **Charisma** has exactly one example (`orcheck.erg`), matching the dissertation's
  description of it as a re-imagination/proof-of-concept.
- **Stencil** has one example (`relaxation.erg`). The `@stencil` annotation, `foreach`,
  and `boundary` builtins are unique to this file across the whole examples directory.
- **`@createhome` / demand creation** is only exercised in `hello6d.erg`.
- **Lambda serialization** (PUP of captured closures) is only exercised in `puptest.erg`.
- **Array sections / Hypercomm sections** are only exercised in `slicecheck.erg`.
- **6D arrays** are only exercised in `hello6d.erg`.

---

## Dissertation–Codebase Mapping

| Dissertation Chapter | Corresponding Code |
|---|---|
| Ch. 2: Ergoline language (syntax, types, STL) | `parsing/`, `ast/`, `libs/`, `examples/` |
| Ch. 3: Embedded DSLs (SDAG, Stencil, Charisma, MSA, DivCon) | `analysis/Segmentation.scala`, `analysis/Stencil.scala`, `passes/GenerateSdag.scala`, `passes/GenerateStencil.scala`, `passes/Orchestrate.scala` |
| Ch. 4: EIR compiler architecture + optimizations | `passes/` broadly, `proxies/`, `passes/GenerateCpp.scala`, `passes/GenerateCi.scala` |
| Ch. 5: Hypercomm runtime abstractions | `hypercomm/` submodule, `include/ergoline/`, `libs/ck/` |

### Dissertation Context for Key Design Decisions

Several things that appear unusual in isolation are explained by the dissertation:

- **`@entry`, `@async`, `@threaded`, `@mailbox`, `@local` annotations** — these map
  directly to Charm++ entry method concepts. `@async` auto-creates a future on the
  caller side and replaces `return` with `future.set()` in generated code. `@threaded`
  entry methods run in user-level threads (ULTs), enabling suspension/resumption.

- **`EirProxy` / `ProxyManager` / three `EirProxyKind` variants** — correspond exactly
  to Charm++'s three proxy scopes: collective (`@`), element (`[@]`), section (`{@}`).
  The dissertation's Table 2.1 enumerates all proxy suffix combinations.

- **`EirSdagWhen` / `EirAwaitMany` / `GenerateSdag.scala`** — implement Ch. 3.1's
  CFA-based SDAG segmentation pass. This re-implements Charmxi's SDAG translator
  entirely within EIR, the dissertation's stated advancement over Charj, enabling local
  variables inside structured entry methods and proper type-checking — both known
  Charmxi limitations.

- **`passes/Orchestrate.scala` / `annotationName = "charisma"`** — Ch. 3.3 describes
  re-imagining the Charisma DSL (data-independent data flows) as an embedded DSL
  via `@charisma` annotations. `Orchestrate.scala` is this pass.

- **`libs/or/` package (`or::placeholder`)** — orchestration variables used by the
  Charisma EDSL pass.

- **The many `replaceChild = ???` stubs** — the dissertation explicitly frames Ergoline
  as a pre-alpha proof-of-concept. The existing code generation paths don't traverse
  most of these nodes, so the stubs haven't caused failures in demonstrated examples.

- **`deepCloneTree` no-op** — a real implementation is needed for the buffer-reuse
  optimization described in Ch. 4.2.2 (reusing message buffers across similar entry
  method calls). That optimization is not yet implemented.

- **`EirTemplateArgument` / `EirTypeAlias` noted as wrong types** — the dissertation
  acknowledges that covariant/contravariant type support is incomplete and that a
  custom object model (EIRv2, targeting something other than C++) is needed to fix
  it properly. The TODOs in the code reflect this known design debt.

- **`GenerateStencil.scala` / `analysis/Stencil.scala`** — Ch. 3.2 describes
  [Ergo]Stencil, a Basilisk-inspired stencil EDSL. The 11 `???` stubs in
  `GenerateStencil.scala` reflect its partially-implemented status.

- **`libs/ck/dht.erg`, `libs/ck/channel.erg`, `libs/ck/future.erg`** — the STL
  implementations shown as complete working examples in Ch. 2.4. The dissertation
  notes the channel is implemented in under 40 lines of Ergoline.

- **`hypercomm/` submodule** — Ch. 5 covers Hypercomm in depth. It provides:
  per-chare futures (vs. Charm++'s PE-scoped futures), lambda serialization (via a
  static registration table), element-spanning trees for sections, and migratable threads.
  The `include/ergoline/` headers are the EIR-side interface to these abstractions.

---

## Module Summaries

| Module | Purpose | Status |
|--------|---------|--------|
| `ast/ast.scala` | All AST node definitions | Large, many stubs |
| `ast/types/` | Type system (lambda, tuple, proxy, template types) | Mostly complete |
| `ast/literals/` | Literal value nodes | Clean |
| `ast/EirVisitor.scala` | Visitor trait with dispatch logic | Clean |
| `resolution/Find.scala` | Symbol lookup, scope walking, type unification | Some TODOs, mostly solid |
| `resolution/Modules.scala` | File loading, package resolution | Clean |
| `resolution/EirResolvable.scala` | Resolution interface | Minimal/clean |
| `resolution/EirPlaceholder.scala` | Unresolved node stand-in | Clean |
| `resolution/EirTemplateFacade.scala` | Template arg facade | Stub (`replaceChild = ???`) |
| `resolution/Transactions.scala` | Substitution tracking | Clean |
| `globals/package.scala` | Global state, type lookups | Minor issues |
| `util/package.scala` | AST utilities, syntax enrichments | `deepCloneTree` is stub |
| `util/AstManipulation.scala` | AST tree mutation helpers | Clean |
| `util/Errors.scala` | Error handling/exit | Clean |
| `util/TypeCompatibility.scala` | Type assignability rules | Some `???` arms |
| `util/TopologicalSort.scala` | Greedy topo sort | Works but O(n²), noted as TODO |
| `util/TupleFactory.scala` | Tuple type interning | Clean |
| `util/LibUtils.scala` | `ldconfig`-based library detection (Linux-only) | Platform issue |
| `passes/Pass.scala` / `Registry.scala` | Pass infrastructure | Clean |
| `passes/FullyResolve.scala` | Resolves all resolvable nodes | Clean |
| `passes/CheckTypes.scala` | Type inference/checking | Many `???` arms |
| `passes/CheckEnclose.scala` | Parent-pointer consistency | Clean |
| `passes/Orchestrate.scala` | SDAG/orchestration transform | Some stubs |
| `passes/StaticEvaluator.scala` | Compile-time constant evaluation | Mostly solid |
| `passes/GenerateCpp.scala` | C++ code generation | 18 `???` stubs |
| `passes/GenerateSdag.scala` | SDAG generation | 10 `???` stubs |
| `passes/GenerateStencil.scala` | Stencil generation | 11 `???` stubs |
| `passes/GenerateDecls.scala` | Declaration generation | 1 `???` |
| `passes/GenerateCi.scala` | Charm Interface generation | 1 `???` |
| `passes/CodeGenerationContext.scala` | Code gen state | 1 `???` |
| `passes/Processes.scala` | Top-level pipeline orchestration | TODOs re: pass migration |
| `passes/TypeCheckContext.scala` | Specialization/substitution context | Clean, 1 TODO |
| `proxies/EirProxy.scala` | Proxy AST nodes | Some stubs |
| `proxies/ProxyManager.scala` | Proxy registry/factory | Clean |
| `analysis/ControlFlow.scala` | CFG construction | Clean, 1 `???` |
| `analysis/Segmentation.scala` | SDAG segmentation | 1 `???` |
| `Driver.scala` | CLI entry point | Clean but minor issues |
| `Visitor.scala` | ANTLR4 tree → AST | 4 `???` stubs |

---

## Issues by Category

### 🔴 Correctness Issues

**1. `EirFunction.replaceChild` doesn't handle `implicitArgs`** (`ast/ast.scala:649-665`)

Implicit arguments can never be replaced via the child-replacement mechanism. Depending on
whether this path is exercised, this silently fails.

**2. `EirMultiDeclaration.replaceChild` is `???`** (`ast/ast.scala:246`)

This node appears in `EirVisitor.visit` dispatch but child replacement is unimplemented —
any pass that calls `replaceChild` on it will throw.

**3. `deepCloneTree` is a no-op stub** (`util/package.scala:208`)

```scala
def deepCloneTree[T <: EirNode](node: T): T = node
```

Returns the original node unchanged. Any caller expecting an actual clone gets a shared
reference, which can cause mutation aliasing bugs. The function is currently unused, but
is dangerous to leave as-is.

**4. `TopologicalSort.sort` can infinite-loop on cyclic input** (`util/TopologicalSort.scala:15-20`)

`indexWhere` returns `-1` if no valid node is found (cycle), causing `unplaced(idx)` to
pick the last element and potentially loop. No cycle detection exists. A TODO comment
acknowledges the greedy approach.

**5. `Find.uniqueResolution` silently picks `headOption`** (`resolution/Find.scala:104-120`)

The large commented-out block (lines 108–119) shows ambiguity detection was removed.
Currently the first resolution is silently taken even if multiple exist, masking
overload/scope bugs.

**6. `EirForLoop.replaceChild` is `???` with commented-out implementation** (`ast/ast.scala:1066-1069`)

The actual implementation is commented out. For-loop body/header changes cannot be
propagated via `replaceChild`.

**7. `EirWhileLoop.replaceChild` and `EirDoWhileLoop.replaceChild` are `???`** (lines 1045, 1055)

**8. `EirMatch.replaceChild` and `EirMatchCase.replaceChild` are `???`** (lines 1139, 1151)

**9. `TypeCompatibility.canAssignHelper` hits `???`** (`util/TypeCompatibility.scala:119, 121`)

In the `(_: EirClassLike, b: EirTrait)` arm for non-templated inheritance when
`ourBase != theirBase` and neither matches the known cases.

**10. `EirMember.isImplOnly` hits `???` for unexpected member types** (`ast/ast.scala:544`)

If new member types are added without updating this match, it will crash at runtime.

**11. `globals.encodeOperator` can throw `NoSuchElementException`** (`globals/package.scala:57`)

Will crash if an operator contains a character not in `operatorNames` (e.g., `@` used in
proxy syntax). No bounds check or fallback.

**12. `EirSpecialization.setBase` and `types_=` default to `???`** (`ast/ast.scala:289, 292`)

Any subclass that doesn't override these will crash if invoked.

---

### 🟡 Dead Code / Commented-Out Code

**13. `EirMember.isConst` commented out** (`ast/ast.scala:586-590`)

A `TODO` note says "these checks should be more robust." The method body is fully
commented out.

**14. `EirTypeOf` class fully commented out** (`ast/ast.scala:1341-1345`)

Appears to be an abandoned feature with no replacement.

**15. Large commented block in `Find.uniqueResolution`** (`Find.scala:108-119`)

Strict ambiguity detection was intentionally disabled but left in place as dead code.

**16. Colon precedence level commented out** (`Visitor.scala:37`)

```scala
//    Seq(':'),
```

No explanation for removal.

**17. `Processes.modules` is declared but never read** (`passes/Processes.scala:24`)

```scala
var modules: Set[String] = Set()
```

Written nowhere and never queried — dead state.

**18. Three commented-out C++ includes in `cppIncludes`** (`Processes.scala:33-35`)

```scala
//    "ergoline/array.hpp",
//    "ergoline/requests.hpp",
//    "ergoline/reducer.hpp",
```

No explanation for why these were commented out.

**19. Commented-out `filterNot` in `Processes.generateCpp`** (`Processes.scala:176`)

```scala
val kids = EirGlobalNamespace.children // .filterNot(_.name == "ergoline")
```

---

### 🟡 Design / Structural TODOs

**20. `EirTemplateArgument` should NOT be `EirType`** (`ast/ast.scala:419`)

Acknowledged as a TODO. Currently participates in type resolution which can cause confusion.
The dissertation notes this is deferred to EIRv2, which plans a custom object model rather
than reusing C++'s type system.

**21. `EirTypeAlias` should NOT be `EirType`** (`ast/ast.scala:1272`)

Same issue — acknowledged as a TODO. Also deferred to EIRv2. The dissertation notes that
Scala's covariant/contravariant relationships are correctly type-checked but may cause the
underlying C++ compiler to fail on generated code.

**22. `EirReturn.expression` should be `Option`** (`ast/ast.scala:856`)

A `TODO` comment notes this. Currently a bare `EirExpressionNode`, meaning void returns
cannot be represented without a dummy expression.

**23. `Find.accessibleMember` doesn't walk parent class hierarchies** (`resolution/Find.scala:412`)

A `TODO` comment says "check parent classes as well!" Accessing an inherited member
through a direct field access will silently fail.

**24. `Find.implementationOf` needs to use `sweepInherited`** (`resolution/Find.scala:423`)

Currently incomplete per the TODO comment.

**25. Traits don't get a `self` declaration** (`ast/ast.scala:356`)

Noted as a TODO: "eventually traits will need a self as well."

**26. `TypeCheckContext.RichEirTemplateArgument.accepts` is partially unimplemented**

A `TODO` comment at `TypeCheckContext.scala:48` notes that upper/lower/type bounds are not
yet checked.

---

### 🟡 Platform / Portability Issues

**27. `LibUtils.isPresent` uses `ldconfig -p`**

Linux-specific. Will silently return `false` on macOS/Windows, meaning BLAS linking is
skipped without warning.

**28. `util.readLibsAndIncludes` invokes `make`**

Uses `sys.process._` and assumes GNU Make with a specific `Makefile.common`. Brittle on
non-Linux platforms.

---

### 🟡 Naming / Minor Inconsistencies

**29. `EirFloatLiteral` reports type `"double"`** (`literals/package.scala:58`)

The class is named `EirFloatLiteral` but its `type` field returns `"double"`. This is
used for type lookup in globals. There is no `float` type — the mismatch is confusing.

**30. `EirSyntheticSpecialization.replaceChild` is `???`** (`ast/ast.scala:300`)

Synthetic nodes are likely transient, but the unimplemented `replaceChild` could cause
unexpected crashes if one ends up in a tree that is mutated.

---

## Total `???` Count: 104 occurrences across 26 files

| File | Count |
|------|-------|
| `ast/ast.scala` | 23 |
| `passes/GenerateCpp.scala` | 18 |
| `passes/GenerateStencil.scala` | 11 |
| `passes/GenerateSdag.scala` | 10 |
| `passes/CheckTypes.scala` | 9 |
| `passes/UnparseAst.scala` | 1 |
| `passes/GenerateDecls.scala` | 1 |
| `passes/GenerateCi.scala` | 1 |
| `passes/CodeGenerationContext.scala` | 1 |
| `passes/GenerateProxies.scala` | 1 |
| `Visitor.scala` | 4 |
| `resolution/Find.scala` | 2 |
| `resolution/Modules.scala` | 1 |
| `resolution/EirPlaceholder.scala` | 1 |
| `resolution/EirTemplateFacade.scala` | 1 |
| `util/TypeCompatibility.scala` | 2 |
| `util/Errors.scala` | 2 |
| `util/package.scala` | 1 |
| `globals/package.scala` | 1 |
| `proxies/EirProxy.scala` | 2 |
| `analysis/ControlFlow.scala` | 1 |
| `analysis/Segmentation.scala` | 1 |
| `ast/EirVisitor.scala` | 2 |
| `ast/types/package.scala` | 1 |
| `parsing/Parser.scala` | 5 |

---

## Maturity Assessment (Dissertation Context)

The dissertation is explicit that this is a pre-alpha proof-of-concept. The most complete
and exercised paths correspond directly to the dissertation's working examples:

| Feature | Dissertation Example | Maturity |
|---|---|---|
| Basic chare codegen, proxies, entry methods | Fibonacci (§2.5.1) | Solid |
| SDAG `when`/`await all`, mailboxes | Jacobi2D (§2.5.3), Cannon (§2.5.2) | Solid |
| Generic chare types | Channel, DHT (§2.4.3–4) | Solid |
| `@async` futures | Fibonacci (§2.5.1) | Solid |
| Stencil EDSL | `examples/jacobi2d.erg` | Partial (11 stubs) |
| Charisma (`@charisma`) | Described in §3.3 | Partial |
| Buffer reuse optimization | Described in §4.2.2 | Not implemented (`deepCloneTree` stub) |
| Covariant/contravariant types | Mentioned as future work (EIRv2) | Not implemented |
| `break`/`continue` in loops | Noted as missing in §2.2.4 | Not implemented |
| `future.and()` / `future.or()` | Planned in §2.4.2 | Not implemented |
| Exception handling | Noted as future work in §2.2.6 | Not implemented |

The stubs in `GenerateStencil.scala`, `GenerateSdag.scala`, and many `replaceChild`
implementations represent planned but not-yet-implemented features or code generation
paths that the demonstrated examples don't exercise.

---

## Priority Recommendations

### High priority — likely to cause crashes

- Implement `replaceChild` for `EirForLoop`, `EirWhileLoop`, `EirDoWhileLoop`, `EirMatch`,
  `EirMatchCase`, `EirMultiDeclaration`, `EirSdagWhen`, `EirSlice`, `EirNew`
- Fix `EirFunction.replaceChild` to handle `implicitArgs`
- Fix or remove `deepCloneTree` (guard callers if removing; needed for buffer-reuse §4.2.2)
- Add cycle detection to `TopologicalSort.sort`
- Guard `globals.encodeOperator` against unknown characters

### Medium priority — correctness gaps

- Re-enable ambiguity detection in `Find.uniqueResolution` (at minimum in strict mode)
- Fix `Find.accessibleMember` to sweep inherited members (affects trait member lookup)
- Change `EirReturn.expression` to `Option[EirExpressionNode]` (needed for void returns)
- Guard `LibUtils.isPresent` with a platform check or document Linux-only behavior

### Low priority — cleanup

- Remove `EirTypeOf` commented-out class
- Remove unused `Processes.modules` variable
- Remove commented-out `EirMember.isConst`
- Rename `EirFloatLiteral` or correct its `type` field
- File tracking tickets for the `EirTemplateArgument`/`EirTypeAlias` type system refactors

---

## CI and Test Suite Review

### CI Pipeline (`.github/workflows/scala.yml`)

The single workflow, `Scala CI (Non-SMP)`, runs on `ubuntu-latest` on every push/PR to `main`. Its steps are:

1. `actions/checkout@v2`
2. Clone and build Charm++ (`netlrts-linux-x86_64 -g -j2 --with-production`)
3. Init submodule, `cmake` + `make -j2` Hypercomm
4. `actions/setup-java@v1` with JDK 1.8
5. `sbt test` (with `ERG_HOME` and `CHARM_HOME` set)
6. `actions/upload-artifact@v2` — uploads `*.cc` and `ergc.jar`

**Issues:**

- **All three GitHub Actions are deprecated versions.** `actions/checkout@v2`, `actions/setup-java@v1`, and `actions/upload-artifact@v2` have all been superseded. `v4` is current for checkout/upload-artifact; `v4` for setup-java. These will generate deprecation warnings and may break when GitHub removes runner support.

- **JDK 1.8 is EOL** and the oldest supported by the build. SBT 1.5.5 supports JDK 8, but Scala 2.13.6 and modern tooling work better on JDK 11 or 17. Running on the oldest possible JVM reduces the chance of catching compatibility issues with the versions most users will actually use.

- **No caching.** Every CI run clones Charm++ from scratch, builds it (slow), and downloads all SBT/Maven dependencies from the internet. There is no `actions/cache` step for `~/.sbt`, `~/.ivy2`, or the Charm++ build. This makes CI runs unnecessarily slow and fragile (network-dependent).

- **Charm++ is cloned from `HEAD` of the default branch** with no pinned ref or tag. A breaking upstream change in Charm++ will silently break CI with no indication of what changed.

- **No matrix testing.** Only one OS (ubuntu), one JDK (1.8), one Charm++ build target (netlrts). There is no macOS or multi-JDK matrix, and no SMP variant (the workflow is even named `Non-SMP`).

- **No separate compile vs. test stages.** A single `sbt test` runs everything, including `EirProcessTests` which internally calls `sbt assembly` — spawning a nested SBT process from within an SBT process. This is fragile and can cause subtle failures.

---

### Test Suite

8 test files, all using ScalaTest `AnyFunSuite`. Tests run serially (`Tags.limit(Tags.Test, 1)` in `build.sbt`). Total test count: **~55 cases**.

#### Test file summary

| File | Cases | What it covers |
|------|-------|----------------|
| `EirUtilityTests` | 3 | Block position lookup, symbol resolution, scope visibility |
| `EirUnparseTests` | 3 | Parse → unparse round-trip for a function and a namespace/for-loop; `CheckEnclose` |
| `EirParseTest` | 9 | Parsing correctness: class resolution, tuple types, binary op precedence, slice syntax, `await many`/`when`, tuple expressions, function args, annotations, ternary type-check |
| `EirConstexprTests` | 9 | `StaticEvaluator`: arithmetic, relational ops, tuple indexing, type indexing, `<:`/`>:` bounds, prefix ops, conditional type expressions |
| `EirImportTests` | 16 | Resolution and type-checking: built-ins, tuples, lambdas, generics, covariance, contravariance, varargs, parent field access, `for` scoping, `implementationOf`, topological sort/namespace partition, chained templates, custom assignment operators, slice types, references, type bounds, predicate dispatch |
| `EirConstructorTests` | 8 | Constructor validation: multiple constructors, enclosure, invalid self-assignment, uninitialized fields, circular inheritance, missing specialization, shared parent class, where-clause enablers |
| `EirFunctionTests` | 6 | Function validation: bodyless function, duplicate definitions (×2), overload disambiguation, missing/incompatible `override` |
| `EirProcessTests` | 1 | End-to-end: `sbt assembly` → compile all 34 examples with `ergc.jar` → (optionally) run with `charmrun` |

---

### Test Suite Issues

**1. `EirFunctionTests`: "unambiguous definition ok" has a wrong assertion** (`EirFunctionTests.scala:33-39`)

```scala
test("unambiguous definition ok") {
  assertThrows[EirException]({
    val res = Modules.load("package foo; def bar(i: int) {} def bar(f: float) {}")
    Processes.onLoad(res)
  })
}
```

The test name says "unambiguous definition ok" but the body asserts an exception IS thrown. The two `bar` functions take `int` vs. `float` — since there is no `float` type in Ergoline (only `double`), this throws due to an unresolvable type, not an ambiguity check. The test is passing for the wrong reason and masking the fact that overload disambiguation between `int` and `double` is not actually tested. The name should either be corrected or the test rewritten against a valid pair of distinct types.

**2. `EirProcessTests`: nested `sbt assembly` inside `sbt test`** (`EirProcessTests.scala:39`)

```scala
os.proc("sbt", "assembly").call()
```

Called at the start of the single `"compile examples"` test, from within the `sbt test` process. This is fragile: it relies on `sbt` being on `PATH` inside CI, on the working directory being correct, and introduces a recursive SBT invocation. A pre-built `ergc.jar` or a separate CI step for assembly would be cleaner.

**3. `EirProcessTests`: all 34 examples in a single test case**

All examples are collected and compiled in one `"compile examples"` test. Failures accumulate and are reported together at the end, but the whole batch counts as one ScalaTest case — meaning CI shows a single red/green for all 34, with no per-example granularity in the test report.

**4. `EirProcessTests`: `charmrun` execution is silently skipped when `CHARM_HOME` is unset**

```scala
val out2 = charmc.map(_ => os.proc(charmRun.getOrElse(???), ...).call(...).out.text())
```

If `CHARM_HOME` is not set, `charmc` is `None`, `out2` is `None`, and the charmrun step silently does nothing — the test passes having only verified that `ergc.jar` didn't crash, not that the generated C++ actually compiles or runs correctly. In CI, `CHARM_HOME` is set, so this path is exercised there, but locally it's easy to miss.

**5. `EirProcessTests`: `charmRun.getOrElse(???)` will crash if `charmHome` is set but `charmrun` binary is missing**

If `CHARM_HOME` points to a Charm++ tree that doesn't have `charmrun` at `bin/charmrun` (e.g. a partial install), the `???` throws `NotImplementedError`. This should be guarded the same way `charmc` is.

**6. No tests for any embedded DSL path**

The unit tests cover parsing, type-checking, resolution, and constructor validation — all for pure Core Ergoline. There are no unit-level tests for:
- SDAG segmentation (`analysis/Segmentation.scala`)
- Stencil analysis/generation (`analysis/Stencil.scala`, `passes/GenerateStencil.scala`)
- Charisma/Orchestrate pass (`passes/Orchestrate.scala`)
- Code generation output correctness (`passes/GenerateCpp.scala`, `passes/GenerateCi.scala`)

The only coverage of these paths is the end-to-end `EirProcessTests` compile-examples test, which checks that compilation doesn't crash but does not assert anything about the generated output.

**7. No tests for error-recovery or negative cases in the parser**

`EirParseTest` only tests that valid inputs parse correctly. There are no tests that invalid syntax produces a useful error (rather than crashing or hanging).

**8. `EirImportTests.setupEnv()` is called inconsistently**

Several tests call `setupEnv()` at the top (which clears global state and resets passes), but `EirConstexprTests` calls it via `EirImportTests.setupEnv()` only in `parseExpression`, not as a suite-level fixture. `EirUtilityTests` and `EirUnparseTests` do `EirGlobalNamespace.clear()` but not `Processes.reset()`. Since tests run serially, leftover global state from one test can affect later ones. The `Global / concurrentRestrictions += Tags.limit(Tags.Test, 1)` in `build.sbt` prevents concurrent contamination, but ordering-dependent state leaks are still possible.

---

### Coverage Gaps Summary

| Area | Unit test coverage | End-to-end coverage |
|------|-------------------|---------------------|
| Parsing (core) | Good (9 cases) | Via process tests |
| Type checking (core) | Good (16+ cases) | Via process tests |
| Constructor validation | Good (8 cases) | Via process tests |
| Static evaluation | Good (9 cases) | — |
| Unparse / round-trip | Minimal (3 cases) | — |
| Symbol resolution / scoping | Minimal (3 cases) | — |
| SDAG segmentation | **None** | compile-only |
| Stencil codegen | **None** | compile-only |
| Charisma/Orchestrate | **None** | compile-only |
| C++ output correctness | **None** | run-only (no assertions) |
| Error messages / diagnostics | **None** | — |

---

## Grammar Review: Dissertation EBNF vs. FastParse Parser vs. ANTLR4

### Status of the two parsers

The repo contains two parsers:

| Parser | Location | Default? | Status |
|--------|----------|----------|--------|
| **FastParse** | `parsing/Parser.scala` + `parsing/syntax/` | **Yes** (`Modules.useFastParse = true`) | Working spec |
| **ANTLR4** | `src/main/antlr4/ErgolineParser.g4` + `ErgolineLexer.g4` + `Visitor.scala` | No (`--antlr4` flag only) | **Legacy — do not treat as working spec** |

The ANTLR4 grammar is demonstrably out of sync with the language: it is missing constructs
that exist in the FastParse parser, contains at least one grammar-level syntax error, and is
not exercised by any test. **The FastParse parser is the only authoritative grammar.**

---

### ANTLR4-specific defects

These are problems in the ANTLR4 grammar itself, independent of any comparison with the
dissertation:

**1. `awaitManyStatement` uses `||` instead of `|`** (`ErgolineParser.g4:62`)

```antlr
awaitManyStatement
    :   AwaitKwd (AnyKwd || AllKwd) LeftCurly (whenStatement+) RightCurly
    ;
```

`||` is not valid ANTLR4 alternation syntax. This rule is syntactically broken — the ANTLR4
grammar does not compile correctly as written.

**2. `do`/`do-while` loop is entirely absent from ANTLR4**

The FastParse parser has `DoWhileLoop` and `Visitor.scala` has a `visitDoWhileLoop` method,
but the ANTLR4 lexer defines no `DoKwd` token and the parser grammar has no `doWhileLoop`
rule. Any `.erg` file using `do { } while (...)` cannot be parsed by the ANTLR4 path.

**3. `enum` is reserved but unimplemented in ANTLR4** (`ErgolineLexer.g4:94`)

```antlr
EnumKwd : 'enum' ;
```

`enum` is a lexer token with no parser rule referencing it, no `Visitor` handler, and no
FastParse counterpart. It is a dead reserved word.

**4. Variance annotations (`+A`, `-A`) missing from ANTLR4 `templateDeclArg`**

The FastParse `TDeclarationArg` rule begins with `Variance.?` to handle `+`/`-` prefixes
on template arguments (covariant/contravariant). The ANTLR4 `templateDeclArg` rule has no
such prefix. The `identifier` rule does accept `+`/`-` as operator identifiers, so a
variance-annotated argument would be silently misparsed as an oddly-named argument.

**5. `->` member access is in ANTLR4 but not in FastParse** (`ErgolineParser.g4:287`)

```antlr
postfixExpression
    :   ...
    |   postfixExpression (Dot | RightArrow) identifier
    ;
```

ANTLR4 accepts `foo->bar` as member access. FastParse only accepts `foo.bar`. The `->` form
exists nowhere in the dissertation, the examples, or the standard library.

**6. Namespace semicolon form missing from ANTLR4** (`ErgolineParser.g4:174`)

```antlr
namespace
    :   NamespaceKwd fqn LeftCurly annotatedTopLevelStatement* RightCurly
    ;
```

FastParse accepts `namespace foo;` (empty-body shorthand). ANTLR4 requires braces.

---

### Dissertation EBNF vs. FastParse Parser

The following table maps each dissertation EBNF form to its FastParse implementation and
flags deviations.

#### Declarations and top-level forms

| Construct | Dissertation EBNF | FastParse | Notes |
|-----------|-------------------|-----------|-------|
| Package | `package ⟨fqn⟩ ;` | `Package` | ✓ Matches |
| Import | `public? import ⟨fqn⟩ (::⟨identifier-list⟩)? ;` | `ImportStatement`, `PublicImport` | **Partial** — the `{a,b,c}` identifier-list form is not implemented; only single-name and `_` wildcard work (e.g. `import foo::{int, bool}` is unsupported) |
| Using | `using ⟨id⟩ ⟨template-decl⟩? = ⟨expression⟩ ;` | `UsingStatement` | **Narrowed** — RHS is `ConstExpr` (static expressions only), not a full `Expression` |
| Namespace | `namespace ⟨fqn⟩ { … }` | `Namespace` | **Extended** — parser also accepts `namespace foo;` (semicolon/empty-body shorthand, not in dissertation) |
| Function | `def ⟨id⟩ ⟨tdecl⟩? (⟨args⟩)* (: ⟨type⟩)? ⟨where⟩? (⟨block⟩ \| ;)` | `FnDeclaration` | **Extended** — parser supports two arg lists for implicits: `def foo(args)(implicit iargs)`; return type defaults to `unit` when omitted |
| Class/struct/object/trait | (described in prose, not full EBNF) | `Class`, `ClassKind` | ✓ Matches described kinds; `abstract class` supported |

#### Statements and control flow

| Construct | Dissertation EBNF | FastParse | Notes |
|-----------|-------------------|-----------|-------|
| If-else | `if (⟨expr⟩) (⟨stmt⟩ \| ;) (else (⟨stmt⟩ \| ;))?` | `IfElseStatement` | ✓ Matches |
| Return | `return ⟨expr⟩? ;` | `ReturnStatement` | **Diverges** — when no expression is given, parser synthesizes `unitLiteral`, not a bare `return;`. This conceals the missing `Option[EirExpressionNode]` issue noted elsewhere |
| While | `while (⟨expr⟩) (⟨stmt⟩ \| ;)` | `WhileLoop` | ✓ Matches |
| Do-while | `do (⟨stmt⟩ \| ;) while (⟨expr⟩) ;` | `DoWhileLoop` | ✓ Matches (FastParse); **absent from ANTLR4** |
| For (range) | `⟨decltypes⟩ <- ⟨expression⟩` | `ForAllHeader` | ✓ Matches; parser also accepts `←` (U+2190) |
| For (C-style) | `⟨decl⟩? ; ⟨expr⟩? ; ⟨expr⟩?` | `CStyleHeader` | ✓ Matches; dissertation also notes `for(;;)` is valid |
| Match | `match (⟨expr⟩) { ⟨case⟩+ }` | `MatchExpr` | ✓ Matches |
| Case | `case ⟨pattern-list⟩ (if ⟨expr⟩)? => (⟨stmt⟩ \| ;)` | `CaseStatement` | ✓ Matches |
| `break` / `continue` | Noted as **not supported** in §2.2.4 | Not in parser | ✓ Consistent with dissertation |

#### Patterns

| Construct | Dissertation EBNF | FastParse | Notes |
|-----------|-------------------|-----------|-------|
| Pattern list | `(⟨pattern⟩ ,)* ⟨pattern⟩` | `PatternList` | ✓ Matches |
| Identifier pattern | `⟨name⟩ (: ⟨type⟩)?` | `IdPattern` | ✓ Matches; `_` is a special wildcard |
| Extractor pattern | `⟨id⟩ (⟨pattern-list⟩)` | `ExtractorPattern` | ✓ Matches |
| Constant pattern | (implied by "constant-pattern") | `ConstantPattern` | **Desugared** — parser rewrites `42` into `EirExpressionPattern(_ == 42)`, not a distinct AST node |
| Expression pattern | (implied by "expression-pattern") | `ExprPattern` | ✓ Present; requires at least one infix operator (`InfixExpr(1)`) to distinguish from identifier patterns |

#### Expressions

| Construct | Dissertation EBNF | FastParse | Notes |
|-----------|-------------------|-----------|-------|
| Lambda | `(⟨id⟩ \| (⟨arg-list⟩)) => ⟨expr⟩` | `LambdaExpr` | **Narrowed** — bare `x => body` form is not implemented; only `(x: T) => body` works. Both FastParse and ANTLR4 omit the single-identifier form |
| Ternary | `⟨expr⟩ ? ⟨expr⟩ : ⟨expr⟩` | `ConditionalExpr` | ✓ Matches |
| Operator precedence | First character of identifier determines level | `sortInfixes` / `precedenceOf` in `Visitor.scala` | ✓ Matches dissertation §2.2.5 description; assign-operators (`+=`, etc.) have lowest precedence |
| Await | `await ⟨expr⟩` | `AwaitExpr` | ✓ Matches; target must be a `PostfixExpr` (not arbitrary expression) |
| New | `new ⟨type⟩ (⟨args⟩)?` | `NewExpr` | ✓ Matches |
| Slice | `A[start:step:end]` | `Slice`, `AtSuffix` | ✓ Matches; all three parts optional |
| Interpolated string | `` `text ${expr}` `` | `InterpolatedString` (in `syntax/Literals.scala`) | ✓ Backtick-delimited; `${…}` groups may be nested |
| String literal | `"…"` | `StringLiteral` | ✓ |
| SDAG `when` | `when ⟨fn⟩[, ⟨fn⟩]* (if ⟨expr⟩)? => ⟨stmt⟩` | `WhenStatement` | ✓ Matches |
| SDAG `await all/any` | `await (all\|any) { ⟨when⟩+ }` | `AwaitManyStatement` | ✓ Matches |

#### Types

| Construct | Dissertation | FastParse | Notes |
|-----------|--------------|-----------|-------|
| Proxy type | `⟨type⟩@`, `⟨type⟩[@]⟨coll⟩`, `⟨type⟩{@}⟨coll⟩` | `ProxyType` | ✓ Matches |
| Tuple type | `(T₁, T₂, …)` | `TupleType` | ✓ |
| Tuple multiply | `(T) .* N` | `TupleMultiply` | ✓ (FastParse parses `.*` as a unit) |
| Lambda type | `(T₁, …) => T` | `LambdaType` | ✓ |
| Reference type | `T&` | `Type` suffix `"&"` | ✓ |
| Pack expansion | `T...` | `Type` suffix `"..."` | ✓ |
| Template bounds | `A <: T`, `A >: T` | `Bounds`, `TDeclarationArg` | ✓ |
| Variance | `+A` (covariant), `-A` (contravariant) | `Variance`, `TDeclarationArg` | ✓ (FastParse only; absent from ANTLR4) |

#### Features present in FastParse but absent from the dissertation EBNF

These are implemented in the parser but not described in the EBNF sections of the
dissertation (some are mentioned in prose):

- **`namespace foo;`** — semicolon shorthand for empty namespace body
- **Implicit argument lists** — `def foo(args)(implicit iargs)` second argument list
- **`@overlap` annotation on loops** — parsed as a regular annotation, not a distinct syntactic form
- **`self[@]`, `self@`, `self{@}`** — proxy-self expressions; partially described in prose (§2.3.2) but no EBNF given
- **Unicode arrows** — `←` (U+2190) as alias for `<-`; `⇒` (U+21D2) as alias for `=>`
- **`<%`** reserved in `SymbolicKeywords` but has no parser rule and is never produced

#### Features in the dissertation EBNF not fully implemented in FastParse

- **`import foo::{a, b, c}` — import identifier list with braces** (§2.2.3): The dissertation EBNF shows `(::⟨identifier-list⟩)?` but the parser only accepts a flat `Id.rep(sep = "::", min = 1)`. The brace-delimited multi-import form is not implemented.
- **Bare-identifier lambda `x => expr`** (§2.2.5): The dissertation EBNF explicitly includes `⟨identifier⟩ => ⟨expression⟩` as a valid lambda form. Only `(args) => body` is accepted.

---

### Summary

| | FastParse | ANTLR4 |
|---|---|---|
| Is the working spec | **Yes** | No |
| Agrees with dissertation | Mostly — two missing forms | Partially — several divergences plus syntax errors |
| `do-while` | ✓ | ✗ missing |
| Variance annotations | ✓ | ✗ missing |
| `awaitManyStatement` | ✓ | ✗ broken (`||` bug) |
| `->` member access | ✗ | ✓ (but undocumented) |
| `enum` keyword | ✗ | Reserved but dead |
| Tests exercising it | All unit/process tests | None |
