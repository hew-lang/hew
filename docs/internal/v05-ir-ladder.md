# Hew v0.5 IR Ladder — Internal Reference

This document is the canonical internal reference for the Hew v0.5 compiler
IR ladder and value model.  It describes the compilation pipeline from source
to machine code, the contract each layer owns, and the user-visible value
semantics the ladder is built to support.

The verifier rules and deletion milestones are detailed in the separate
implementation plan for this work.

---

## 1. The IR ladder

Hew v0.5 compiles through an explicit sequence of intermediate representations.
Each layer has a distinct owner, a verifier or diagnostic class, and a
deterministic text dump (`hew dump-<layer>`).

```
source.hew
  └→ AST              (hew-parser)                syntactic only; no resolution
  └→ Resolved HIR     (hew-hir, name-resolved)    names, scopes, imports, capabilities
  └→ THIR             (hew-hir, fully typed)       monomorphised types; ValueClass per type
  └→ Raw MIR          (hew-mir, CFG)               SSA/places; value-model ops chosen; not yet proven
  └→ Checked MIR      (hew-mir)                    ownership proven; diagnostics emitted; FAIL-CLOSED gate
  └→ Elaborated MIR   (hew-mir)                    explicit Drop edges; DecisionMap; cleanup CFG
  └→ LLVM IR          (hew-codegen-rs/Inkwell)     direct emission from proven MIR facts
  └→ Native/WASM obj  (LLVM TargetMachine/wasm-ld) target-specific; no Hew decisions remain
```

### Why this ladder, not a single IR

- **Ownership belongs in CFG MIR.** Every successful precedent (Rust MIR,
  Swift SIL/OSSA, Flang FIR) decides ownership in a CFG IR with explicit
  `Place`s.  Doing it on a typed AST means reinventing CFG analysis on a tree.
- **THIR exists to retire `Ty::Var`.** The fail-closed gate ("no `Ty::Var`
  survives into codegen") becomes a structural verifier between THIR and
  Raw MIR, not a post-hoc sweep.
- **Elaborated MIR exists so codegen consumes proven, not hypothetical, facts.**
  Drop elaboration changes the CFG; doing it in the LLVM emitter would mean
  either deferring ownership to codegen (rejected) or emitting unsafe IR and
  patching it post-hoc (the old bandaid pattern).
- **No semantic re-derivation in codegen-rs.** The Rust/Inkwell backend lowers
  MIR facts directly into LLVM IR. It verifies LLVM modules, but it does not
  re-prove ownership, aliasing, or value-model decisions.
- **No dialect bridge.** The old C++ backend ladder was retired in v0.5; v0.5
  work now widens the Rust HIR/MIR/codegen-rs path instead of adding dialect
  conversion passes.

---

## 2. Layer contracts

### 2.1 AST (`hew-parser`)

**Owns:** tokens, syntactic shape, source spans, comment trivia.

**Must not own:** type info, name resolution, ownership.

**Verifier / diagnostics:** parser diagnostics only (syntax).

**Dump:** `hew dump-ast` (S-expression / pretty-print).

---

### 2.2 Resolved HIR (`hew-hir::resolved`)

**Owns:** name resolution, scope/imports, module graph, capability declarations,
sugar desugaring (canonical form), `Place` introduction for bindings, hygiene,
and inferred intent per site (read / modify / consume / capture / yield) from
value expressions and receiver shape.

No `&` / `&mut` / lifetime syntax in Hew's surface; the HIR infers intent
without it.

**Must not own:** types beyond name resolution, ownership proof, surface borrow
syntax.

**Verifier / diagnostics:** unresolved names, capability not in scope,
shadowing; `var` declared but never modified (note); `mutating`/`consume`
callee contract violated.

**Dump:** `hew dump-hir`.

---

### 2.3 THIR (`hew-hir::typed`)

**Owns:** fully resolved types on every expression; monomorphised generics;
trait dispatch resolved; method dispatch chosen; `StructInit.type_args` carried
structurally; `ResolvedTy` final here; **ValueClass per type** (see §3).

**Must not own:** ownership proof, drop elaboration, CFG, site-level cost
decisions (those land in Raw / Checked MIR).

**Verifier / diagnostics:** type errors, trait selection failures, coherence,
generic constraint failures; `Ty::Var` must be eliminated here.

**Dump:** `hew dump-thir` (indented with type + ValueClass annotations).

---

### 2.4 Raw MIR (`hew-mir::raw`)

**Owns:** CFG of basic blocks, `Place`s and `Operand`s,
`Statement` / `Terminator`, drop scopes (lexical), explicit value-model
operations at every use (`borrow_read`, `move`, `cow_share`, `ensure_unique`,
`materialize`, `consume_call`, `drop`, `freeze`), coroutine `suspend` / `resume`
terminators.

The operations are chosen by the value-model classifier from THIR's ValueClass
facts.  They have not yet been *proven* correct by analysis.

**Must not own:** proof of uniqueness/aliasing, LLVM emission concerns.

**Verifier:** structural only — every block ends in a terminator, every place
is dominated by its definition, every site has a chosen value-model operation
(no "unclassified").

**Dump:** `hew dump-mir=raw`.

---

### 2.5 Checked MIR (`hew-mir::checked`)

**Owns:** proven uniqueness, aliasing analysis (read-shared XOR mutate-unique
at every program point), init / use-after-move, generator-borrow-across-yield
analysis, actor-send escape analysis, Copy/COW/Materialize dispatch per
operand.

**The fail-closed boundary for value semantics.**  Diagnostics fire here in
value-cost language (see §4.3).

**Must not own:** drop elaboration, cleanup blocks, code emission.

**Verifier / diagnostics:** value-cost diagnostics — "value `s` is consumed
at <span> but read at <span>", "two mutations alias the same value", "affine
resource `c` would be shared across an actor send — consume or materialize".

**Dump:** `hew dump-mir=checked` (annotation overlay: `// read-share`,
`// move (last use)`, `// ensure-unique → mutate`, `// materialize`, etc.).

---

### 2.6 Elaborated MIR (`hew-mir::elab`)

**Owns:** explicit `Drop(place)` statements on every exit path, explicit
cleanup basic blocks, panic-edge CFG, coroutine state struct layout (proven
from `CoroutineSchema`), actor-shutdown cleanup blocks, `DropPlan` per scope,
storage classes materialised on every place, and the **DecisionMap**.

The `DecisionMap` is a deterministic table of
`DecisionFact { site_id, kind, chosen_strategy, why, cost_class }` keyed by
stable `SiteId`.  It is attached as a top-level region attribute on each
elaborated function and is emitted into IR dumps.  `SiteId` is derived from
the THIR/MIR structure (function id + canonical CFG path to the operation),
not from a source span — the table is stable across whitespace and reformat.

**Must not own:** re-running ownership analysis, LLVM values, span-keyed side
tables.

**Verifier:** every owning place has exactly one `Drop` on every exit path;
no `Drop` of a moved-out place; cleanup-block dominance; coroutine frame-slot
type matches yield value-class; DecisionMap is total and SiteIds are stable.

**Dump:** `hew dump-mir=elab` (includes explicit drop / cleanup-block section
and DecisionMap).

---

### 2.7 LLVM IR emission (`hew-codegen-rs`)

**Owns:** direct LLVM IR construction from `Elaborated MIR` using Inkwell,
including function declarations, stack slots for MIR places, value loads and
stores, arithmetic/control-flow lowering, runtime-symbol references, and
module verification.

DecisionFacts stay attached to MIR inputs. Codegen may carry them into debug
metadata or comments in dumps, but it must not reinterpret them or synthesize a
replacement side table.

**Must not own:** re-deriving any value-model fact; inventing ABI shape from
LLVM value layout; ownership/drop decisions; source-level diagnostics.

**Verifier:** `Module::verify()` after emission; fail-closed errors for
unsupported MIR constructs, missing locals, unresolved runtime symbols, and
LLVM verifier failures.

**Dump:** textual `.ll` plus any requested MIR dump (`hew compile
--dump-mir raw|checked|elab`).

---

### 2.8 Native and WASM object emission

**Owns:** target-specific object emission from verified LLVM IR. Native builds
write a relocatable object and then link it with `libhew.a`; WASM builds write
a wasm object and link it into a standalone module with `wasm-ld` or
`rust-lld`.

**Must not own:** Hew-level semantics, ownership/drop decisions, or checker
diagnostics. Unsupported target substrates must report a named fail-closed
diagnostic instead of falling back silently.

**Verifier:** LLVM target emission errors and linker exit status.

**Dump:** `.o`, executable, `.wasm.o`, `.wasm`.

---

## 3. Value model

### 3.1 Value classes

Every type in Hew v0.5 belongs to exactly one **ValueClass**.  Classification
is structural — propagated through fields — unless the type declares a marker.

| ValueClass       | User-facing name | Marker          | Description |
|------------------|------------------|-----------------|-------------|
| `BitCopy`        | Copy             | (structural)    | Strict bit-copy; no destructor, no COW, no refcount.  Integers, bools, floats, chars, unit, tuples and fixed-arrays of Copy types.  **Not** "anything cheap to copy" — COW values are a separate class. |
| `CowValue`       | Value            | `@value` (opt)  | Value semantics with COW implementation.  String, Vec<T>, Map<K,V>, Set<T>, and user structs whose fields are all Copy or Value (default for user structs).  Refcounted backing; mutation triggers `ensure_unique`. |
| `PersistentShare`| Shareable        | (stdlib types)  | Explicitly shared persistent data structures (HAMT maps/sets, RRB vectors).  Structural sharing across versions; reads are cheap; writes produce a new version.  Always safe to share; no COW needed. |
| `AffineResource` | Resource         | `@linear` / `@resource` | At-most-one-owner resources: file handles, sockets, channels, capability handles.  No refcount, no COW.  Consumed on last use; sharing is a checker error. |
| `View`           | View             | (compiler-only) | Borrowed read-only window into another value.  Users do not name Views in v0.5 surface syntax; they appear as method receivers and iterator yields.  Compiler proves View does not outlive its producer. |

**User struct default:** a user struct is `CowValue` if all its fields are
Copy or Value, `BitCopy` if all its fields are Copy, and requires `@linear` /
`@resource` to be `AffineResource`.  The user may spell `@value` explicitly
to pin the classification (rarely needed).

### 3.2 Surface syntax

The v0.5 surface is Swift/Kotlin-shaped, not Rust-shaped:

- **Bindings:** `let` (immutable) and `var` (mutable / rebindable).
  No `&`, `&mut`, no lifetime syntax at the surface.
- **Receivers:** `fn (self) …` = immutable read (default);
  `fn (mutating self) …` = mutable receiver (requires `var` binding);
  `fn (consuming self) …` = consuming receiver (last-use; caller loses access).
- **Escape hatches:** `copy(x)` forces a deep copy (reports as
  `UserRequestedCopy`, silences hidden-copy note); `consume(x)` explicitly
  consumes `x` (subsequent use rejected).
- **Record update:** `{ ..record, field: v }` — produces a new value;
  source record unaffected.  For Value records: cow_share + ensure_unique.
  For Copy records: bit-copy with field replaced (free).
- **Containers:** `[1, 2, 3]` and `{"k": v}` literals produce `CowValue`
  collections.  Mutation through a `var` binding does ensure_unique-then-mutate.
- **Strings:** `String` is `CowValue`.  String slices are `View`.
  No user-facing distinction between "owned" and "borrowed" string.
- **Resources:** declared at the type with `@linear` (single-owner, no drop
  side effect) or `@resource` (single-owner, has a drop side effect — file
  handle, socket, capability).

### 3.3 Internal MIR operations (compiler vocabulary)

The value-model classifier emits exactly one of these at each value-use site.
Users never type these; the diagnostics and Ownership Plan Report use them
as implementation terms, and inlay hints / hovers surface them to compiler
engineers.

| Operation         | When chosen | Cost class |
|-------------------|-------------|------------|
| `borrow_read`     | Immutable read; no refcount touch.  View / COW-share-read. | Free |
| `move`            | Last use; transfer ownership without copy. | Free |
| `cow_share`       | Shared use of a CowValue; bumps refcount. | RefcountTouch |
| `ensure_unique`   | Prepare a CowValue for mutation; clones if refcount > 1. | OAlloc (conditional) |
| `materialize`     | Deep copy / clone-now.  Only operation with unbounded cost. | OCopyN |
| `consume_call`    | Pass to a callee that takes ownership. | OResourceTransfer |
| `drop`            | Deterministic destructor; explicit in Elaborated MIR. | (implicit) |
| `freeze`          | Immutable snapshot of a `var` for crossing a yield/send boundary. | Free |

### 3.4 Actor / concurrency rules

Actor isolation is the central concurrency boundary.  Send-path classification
(computed in Checked MIR, attributed on `hew.actor_send` in the dialect):

- **Transfer mode:** last-use of an owned value; sender loses access, receiver
  gains it.  Cheapest path.  For AffineResource, the only valid send mode.
- **Share mode:** CowValue or PersistentShare; sender retains access, receiver
  gets a refcount bump.  Safe: COW guarantees no observable mutation across
  aliases.
- **Materialize mode:** deep copy on send.  Used when the value is reachable
  from the sender after the send and is not COW/persistent, or when an explicit
  `copy(x)` is used.
- **AffineResource sends:** consume-or-error.  Either the resource is last-used
  at the send site (transfer) or the send is rejected.

**`actor_scope { … }`** (v0.5 opt-in primitive): spawns child actors whose
lifetimes are bounded by the scope.  On scope exit, the runtime guarantees all
child actors have drained their mailboxes and their resources have been dropped.
Lowers to a `hew.scope` op with attached actor-cleanup edges in Elaborated MIR.
The unstructured `spawn` path remains available.

**Refcount strategy:** actor-local non-atomic RC with cross-actor promotion
(v0.5 decision).  COW values that cross an actor boundary (share mode) are
promoted to a shared atomic refcount; actor-local values use a cheaper
non-atomic counter.

### 3.5 Generator / yield rules

A generator may hold an immutable read across `yield` if and only if Checked
MIR proves the underlying value cannot be mutated through any other path during
the suspension:
- No `var` aliasing of the captured value reachable through any other path.
- No actor message can reach the value during suspension.

Mutation or consume across `yield` is rejected with a value-cost diagnostic.

Closures capture `let` bindings as reads, `var` bindings as the minimum
operation needed by the body (read / mutate / consume); the inferred capture
mode appears in the Ownership Plan Report.

### 3.6 Diagnostic vocabulary (user-facing)

Internal vocabulary (`move`, `borrow`, `lifetime`, `'a`) does **not** appear
in user-facing diagnostics.  User diagnostics use:

- "value `s` is read here"
- "value `s` is mutated here"
- "value `s` is consumed here"
- "value `s` is shared here"
- "value `s` is copied here (cost: O(n))"
- "the resource `c` cannot be shared; consume it or restructure"
- "`mutating` method called on immutable binding `s` — declare `s` with `var`
  or use a non-mutating alternative"
- "value `s` is consumed at <span> but read at <span>; choose one"

Internal MIR dumps and the dialect retain the precise vocabulary
(`borrow_read`, `ensure_unique`, etc.) for compiler engineers.

---

## 4. Ownership Plan Report

The report surfaces every value-model classification to users and compiler
engineers.

### 4.1 CLI

```
hew explain ownership <file>
```

Prints a deterministic per-site classification table for the file's functions,
grouped by function, ordered by SiteId.  Each row:
`site → kind → value-class → strategy → cost → why`.

A summary footer reports the function's hidden-copy budget (sum of `OCopyN`
and `OAlloc` sites) and flags any site over the per-function budget threshold.

```
hew build --emit-decisions=json
```

Emits the same DecisionMap as newline-delimited JSON keyed by SiteId.  Schema
versioned; round-trip tested.

```
hew explain ownership --diff <before> <after>
```

Diff mode: shows which sites changed strategy or cost class between two builds.
Computed on SiteId — reformatting noise does not appear.

```
hew explain ownership --filter=cow|materialize|affine|share
```

Narrows the report.

### 4.2 DecisionFact schema

```
DecisionFact {
  site_id:         SiteId,       // stable across whitespace/format
  kind:            SiteKind,     // Binding | Call | FieldAccess
                                 // | ActorSend | YieldUse
                                 // | RecordUpdate | CaptureIntoClosure
  value_class:     ValueClass,   // BitCopy | CowValue
                                 // | PersistentShare
                                 // | AffineResource | View
  chosen_strategy: Strategy,     // borrow_read | move | cow_share
                                 // | ensure_unique | materialize
                                 // | consume_call | freeze
  cost_class:      CostClass,    // Free | RefcountTouch | OAlloc
                                 // | OCopyN | OResourceTransfer
  why:             Reason,       // LastUse | SharedRead
                                 // | MutationRequiresUniqueness
                                 // | NotCowSafeAcrossSend
                                 // | UserRequestedConsume
                                 // | UserRequestedCopy | …
  source_loc:      FusedLoc,     // for diagnostic display only;
                                 // SiteId is the stable key
}
```

`SiteId` is derived from the THIR/MIR structure (function id + canonical path
through the elaborated CFG), not from a source span.  `DecisionFact`s travel
inside the IR (as region attributes on Elaborated MIR functions and as
`hew.site_id` / `hew.value_decision` op attributes in the dialect).

### 4.3 Hidden-copy budget

Each function carries an implicit budget for `materialize` and
`ensure_unique`-with-clone sites.  Default: **note-level** at the first hidden
materialize per function in non-test code (user-confirmed default; configurable
per-crate).  Explicit `copy(x)` and `consume(x)` never count against the budget.

### 4.4 LSP surfaces

- **Inlay hints** at each classifiable site: `read` / `move` / `cow-share` /
  `ensure-unique → mutate` / `materialize` / `consume`.
- **Code lenses** above functions: hidden-copy budget summary.
- **Hovers** over any value-bearing expression: chosen strategy + why + cost
  class + one-line link to the value model rule.
- **Diagnostics:** same value-cost vocabulary; quick-fixes offer `consume(…)` /
  `copy(…)` / "rebind as `var`" / "switch to immutable read receiver".

---

## 5. Corpus and worked examples

`tests/corpus/v05-value-model/` contains hand-written fixture files and their
companion `.ownership-plan.txt` expected reports.  These are **implementation
targets** for the v0.5 value-model checker and Elaborated MIR implementation:
the checker must produce output matching the companion files byte-for-byte
(modulo source locations).

See `tests/corpus/v05-value-model/README.md` for the full index and naming
conventions.

---

## 6. Checker and lowering work

The checker and lowering work should be introduced as cohesive compiler changes
against the v0.5 value model, progressing through each layer of the IR ladder
(Resolved HIR → THIR → Raw MIR → Checked MIR → Elaborated MIR → LLVM IR →
native/WASM emission).  The corpus fixtures in
`tests/corpus/v05-value-model/` serve as the byte-level acceptance targets for
the Checked MIR and Elaborated MIR stages.

---

## 7. Building the v0.5 spine

`hew compile` runs the v0.5 Rust HIR/MIR/codegen-rs path. The backend is a
normal Cargo dependency of `hew`; no retired C++ backend build step is involved:

```
make hew          # debug: builds hew
make release      # release: builds hew + adze + stdlib
```

---

*This document is an internal engineering reference.  The public-facing
language specification is `docs/specs/HEW-SPEC.md`.*
