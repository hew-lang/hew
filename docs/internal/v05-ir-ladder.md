# Hew v0.5 IR Ladder — Internal Reference

This document is the canonical internal reference for the Hew v0.5 compiler
IR ladder and value model. It describes the compilation pipeline from source
to machine code, the contract each layer owns, and the user-visible value
semantics the ladder is built to support. There is no additional
general-purpose IR between the layers defined here.

The verifier rules and deletion milestones are detailed in the separate
implementation plan for this work.

### Migration status

The intended steady-state ladder inserts SIR between normalized HIR and Raw
MIR. The initial value/CFG proof uses a bounded legacy-MIR differential bridge,
while the strict direct-call slice already produces fresh Raw/Checked MIR from
SIR. That bridge is deliberately disposable: it is neither a second release
compiler configuration nor a compatibility contract. The cutover contract in
§1.2 defines what must be removed before SIR becomes the normal path.

---

## 1. The IR ladder

Hew v0.5 compiles through an explicit sequence of representations. Each layer
has a distinct owner, verifier or diagnostic class, and an eventual
deterministic text dump. Today the CLI exposes `hew tool compile --dump-sir` and
`hew tool compile --dump-mir raw|checked|elab`; AST/HIR dumps are planned cutover
tooling, not current commands.

**Status convention:** this document defines the required final architecture.
`[current]` names behavior implemented today, `[transitional]` names bounded
migration scaffolding that must be deleted, and `[planned]` names a required
target-state facility that must not be mistaken for present implementation.

```
source / package graph
  └→ AST                         (hew-parser)      source syntax and provenance
  └→ Resolved + Typed HIR         (hew-hir)         authoritative Hew meaning
  └→ Normalized HIR invariant     (hew-hir)         canonical structured Hew form
  └→ SIR — Hew Semantic SSA       (hew-sir)         semantic values, effects, CFG, optimization
  └→ Raw MIR                      (hew-mir)         representation and ownership realization begins
  └→ Checked MIR                  (hew-mir)         ownership/concurrency legality proven
  └→ Elaborated MIR               (hew-mir)         drops, cleanup, and lifetime realization
  └→ LLVM IR                      (hew-codegen-rs)  low-level computation
  └→ LLVM backend / Machine IR    (LLVM)            target legalization and machine realization
  └→ native / Wasm / embedded artifact, or ORC JIT execution
```

The design question at every boundary is deliberately narrow:

| Layer          | Question it answers                                                  |
| -------------- | -------------------------------------------------------------------- |
| AST            | What did the programmer write?                                       |
| HIR            | What does it mean in Hew?                                            |
| Normalized HIR | What is its canonical structured Hew form?                           |
| SIR            | What semantic values, effects, and control flow exist?               |
| MIR            | How are those values represented, owned, transported, and destroyed? |
| LLVM IR        | What low-level computation should execute?                           |
| LLVM backend   | How does this target machine execute it?                             |

`Normalized HIR` is primarily an invariant/state of HIR, not a requirement for
a second, giant Rust type hierarchy. It remains recognizable as Hew; ordinary
`let` bindings and other useful structured concepts do not disappear merely
because they are structured.

### Why this ladder, not a single IR

- **Ownership realization belongs in CFG MIR.** Every successful precedent (Rust MIR,
  Swift SIL/OSSA, Flang FIR) decides ownership in a CFG IR with explicit
  `Place`s. Doing it on a typed AST means reinventing CFG analysis on a tree.
- **Typed HIR retires `Ty::Var`.** The fail-closed gate ("no `Ty::Var`
  survives into codegen") is a structural verifier before SIR, not a post-hoc
  sweep.
- **SIR retains semantic values until ownership/layout realization.** It uses
  SSA values and block arguments from inception, so normal CFG and value
  simplification happen before allocation, storage, drop, ABI, and runtime
  choices discard the language structure that makes those transformations
  meaningful. SIR control effects are terminators; ordinary operations have
  derived effect classes rather than mutable effect metadata.
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

### 1.1 One convergent compiler driver

All artifact-producing commands share the same final body-lowering path:

```
compile / run / build / debug / test / watch --run / eval
  └→ parse → resolved + typed HIR → normalize → SIR → MIR → LLVM IR → backend
```

Target choice begins no earlier than the MIR/LLVM boundary. Native, Wasm, and
embedded builds are target outputs; ORC JIT is an execution mode over the same
verified LLVM IR, not a second middle end. Inspection modes may intentionally
stop after a layer. [current] `hew tool compile --dump-sir` and `--dump-mir
raw|checked|elab` do so today; AST/HIR inspection exits are planned. No
execution mode may skip, duplicate, or substitute semantic lowering.

### 1.2 SIR hard-cutover contract

The current SIR bridge is bounded migration evidence, not a permanent dual
pipeline. `--sir-lower` is a temporary selector for the migrated SIR domain,
not a release-mode compatibility promise. Unsupported SIR surface is an
implementation gap, not a reason for a permanent hidden legacy fallback.

The first strict domain is already template-free: a closed reachable graph of
ordinary, non-generic direct calls with scalar read-only parameters and scalar
or `Unit` returns. SIR owns each callable's stable identity, semantic signature,
source origin, resolved effect summary, and parameter-use facts; SIR →
Raw/Checked MIR independently legalizes call continuations and creates fresh
boundary/scheduling facts. The migration callable table temporarily also carries
an emitted symbol and default convention so the first strict component can
reach the unchanged backend. Final linkage names and concrete
ABI/calling-convention realization move to the MIR declaration/header boundary.
A reachable feature outside that domain fails explicitly. An unrelated
unsupported body neither blocks the selected component nor becomes a hidden
legacy callee.

#### Callable identity on the MIR function types

Each of `RawMirFunction`, `CheckedMirFunction` and `ElaboratedMirFunction`
carries `key: MirCallableKey` — the declaration identity hew-hir currently
reconstructs for the body it realizes, plus the instance discriminator that
separates one realization from another (monomorphic, one generic
specialization with its declared-order `type_args`, the abstract polymorphic
origin, or a synthesized child naming its parent key and the producer that
minted it). `HirFn::declaration` and `HirMachineDecl::declaration` are built
with `DefId::legacy_reconstruct_from_full_path` over the owner's qualified
path, not yet a resolver-native mint; resolver-native minting is the tracked
upstream fix (`.tmp/TODO.md` on `refactor/mir-callable-identity`). The legacy
HIR lowerer projects the key from `HirFn::declaration`; the SIR bridge
projects the same identity from `SemCallable::declaration` +
`CallableInstance`, so a monomorphic function lowered either way carries an
equal key. `name` remains the
presentation/linkage alias beside it, and a module whose raw MIR realizes one
key twice is rejected. Re-keying the cross-stage joins onto this field — and
deleting the name comparisons in `sir.rs` and `llvm.rs` — is the next slice.

#### Transitional callable/linkage bridge

Today, the strict bridge carries HIR's selected body spelling in
`SemCallable::symbol` so it can fill the current `RawMirFunction.name` header.
That symbol/default-convention carrier is transitional bridge metadata, not
SIR's permanent ABI or linkage authority: SIR calls are resolved by `CallableId`
and semantic declaration identity, never by rejoining bodies on a string
spelling. At cutover, a SIR-derived Raw MIR module/header — metadata within the
existing MIR layer, not a new IR — records target linkage names and concrete
ABI/calling-convention choices from the semantic callable table and target
layout. LLVM lowering consumes that header; SIR retains only the semantic
callable relation.

The shadow adapter is gone. It was a candidate-buildability probe: it built a
SIR candidate through a legacy Raw-MIR template, discarded it, and shipped the
legacy pipeline anyway, so its success was never evidence for direct-call
correctness. Its flag, its per-function legacy fallback, and its corpus harness
are deleted rather than kept as a second lane. `--sir-lower` is now the only
SIR selector and it fails closed: a body outside the strict domain is a typed
error, never a silent legacy body. Until the old body-lowering branch is
deleted, each strict domain must carry an execution-parity test against the
established lane — a strict compile that merely succeeds proves nothing about
the surface it does not own.

The following scaffolding is still a deletion target: the transitional
symbol/default-convention carrier above (the strict raw header's name is still
`SemCallable::symbol`, checked by `verify_strict_sir_raw_checked` in
`hew-mir/src/sir.rs`), the `--sir-lower` selector itself, and the legacy
HIR → Raw-MIR body-lowering branch. The normal CLI flips only after all of
these gates hold:

1. SIR owns verified callable identity, semantic signature, source provenance,
   effect summary, and semantic parameter-use facts. The transitional
   symbol/default-convention carrier is replaced by a SIR-derived MIR module
   header; Raw MIR/LLVM independently realize linkage and concrete
   ABI/calling-convention details.
2. SIR → MIR independently creates and verifies Raw, Checked, and Elaborated
   MIR facts and scheduling decisions—including an explicit zero-drop
   elaboration when appropriate. Each Raw body has exactly one Checked and one
   Elaborated finalization; it copies no legacy function body, ABI decision, or
   drop plan.
3. Each migrated language surface is a closed reachable lowering domain. Its
   old HIR → Raw-MIR branch is deleted when SIR owns it rather than retained as
   a mixed-artifact fallback.
4. The accepted language surface reaches SIR → MIR across the examples and
   ecosystem corpus, including native and WASM targets where supported.
5. The default path is changed to SIR and all bridge flags, template code, and
   legacy body-lowering branches are removed.

The direct-call slice has replaced the immediate template dependency. The next
work first proves SIR infrastructure and simple optimization, then adds semantic
domains in closed reachable increments: abstract aggregates/projections,
closures, ownership-aware uses, machines, actors, and async/suspension. Each
increment deletes its replaced HIR → Raw-MIR branch rather than accumulating a
mixed-artifact fallback.

### 1.3 Incremental acceptance gates

| Milestone                   | Acceptance criterion                                                                                                                                                                                                          |
| --------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| SIR foundation              | A scalar function with a conditional lowers to typed SSA blocks and block arguments, verifies definition/dominance, edge arity and types, lowers mechanically to Raw MIR, and preserves behavior across the existing suite.   |
| SIR infrastructure          | A pass can inspect and replace uses, erase an operation, rewrite an edge, and add/remove a block argument; every pass supports verify/dump before and after, timing, and pass bisection.                                      |
| Basic optimization          | CFG simplification, SCCP, DCE, copy propagation, and local GVN operate on SIR rather than asking LLVM to recover the semantic CFG.                                                                                            |
| Generic instances           | A single normalized-HIR instance service deduplicates concrete function/type instances and selected implementation evidence, including recursive instances; the resulting concrete SIR bodies contain no layout or ABI facts. |
| Value semantics             | Tuples, records, and variants remain abstract values: an unused construction can disappear before MIR and no SIR operation observes a byte offset, physical tag, or payload layout.                                           |
| Closures                    | `closure.make` plus `closure.call` can eliminate a non-escaping closure allocation before it reaches MIR.                                                                                                                     |
| Machines, actors, and async | Known machine transitions can specialize; actor messages remain typed semantic values; every suspension is an explicit SIR CFG terminator before runtime/coroutine lowering.                                                  |
| Full cutover                | Normal compilation has exactly `HIR → normalized HIR → SIR → Raw MIR → Checked MIR → Elaborated MIR → LLVM`; bridge flags, templates, shadow harnesses, and legacy body-lowering branches are deleted.                        |

### Design axioms

Three cross-cutting invariants the ladder, the gates, and the runtime are
built to preserve.

- **Totality at decision points.** Internal semantic decision enums, and the
  dispatch tables over them, are total: no semantic branch carries a silent
  wildcard or default arm. Adding a variant fails compilation at every
  consumer site until that site decides what the new case means.
  `SendAliasMode` (`hew-mir/src/model.rs`) is the exemplar: MIR authors a
  complete per-argument send decision, and checked MIR may not contain an
  undecided value. Genuinely undecidable cases take the sanctioned form of
  an explicit fail-closed variant — `Strategy::UnknownBlocked`, routed to
  `MirCheck::DropPlanUndetermined` — because "undecidable" is itself a
  decision, and it must reject, never fall through. A wildcard arm on a
  semantic branch converts a future variant into silent misbehaviour;
  totality converts it into a build break that names every site owing a
  decision.

- **Leak-never-double-free total order.** Release bugs have a strict
  severity order, and the gates enforce it as a total one: over-release is
  unconditionally rejected (a refcount that would go below zero aborts, in
  every build mode), under-release is tracked and ratcheted shrink-only, and
  abort/trap paths may leak-at-abort but must never double-free. The full
  statement lives in the ownership model doc
  ([`docs/v05/ownership.md`](../v05/ownership.md)).

- **Observable honesty.** Any silent adaptive behaviour in the runtime or
  toolchain — execution-lane fallback, COW share-vs-materialize selection,
  retained-version selection, supervision restart/rehoming — is observable
  in the artifact's output, response, or telemetry. A fallback that cannot
  be observed is a fail-open: the system substitutes behaviour without
  leaving evidence, and neither users nor gates can tell the substituted
  path from the intended one. The Ownership Plan Report (§4) is this axiom
  applied to the value model — every share/materialize decision surfaces as
  a `DecisionFact` — and the `std::observe` counters
  ([`docs/observe.md`](../observe.md)) carry the runtime side.

---

## 2. Layer contracts

### 2.1 AST (`hew-parser`)

**Owns:** tokens, syntactic shape, source spans, comment trivia.

**Must not own:** type info, name resolution, ownership.

**Verifier / diagnostics:** parser diagnostics only (syntax).

**Dump:** [planned] deterministic AST S-expression / pretty-print.

---

### 2.2 Resolved + Typed HIR (`hew-hir`)

HIR is the authoritative representation of Hew language meaning, not a second
copy of source syntax. It owns stable `ItemId`/`BindingId`/`SiteId` identities,
resolved names and overloads, lexical scopes, imports, capability declarations,
`ResolvedTy` on every semantic expression, generic parameters and resolved
generic-instance facts, trait and method selection, pattern semantics,
closure-capture facts, and actor,
machine, state, event, and method identities. It also retains source-semantic
ownership intent and diagnostic provenance. **ValueClass** is a resolved type
fact (see §3), not a layout decision.

No `&` / `&mut` / lifetime syntax appears in Hew's surface; HIR resolves the
language's semantic intent without inventing surface borrow syntax.

**Must not own:** CFG blocks, SSA values, `Place`, storage/lifetime realization,
target offsets, runtime carriers, LLVM concepts, or ownership proof.

**Verifier / diagnostics:** unresolved names, capability not in scope,
shadowing, type errors, trait/coherence and generic-constraint failures, and
callee-contract violations. `Ty::Var` must be eliminated before SIR lowering.

**Dump:** [planned] resolved HIR with type and ValueClass annotations.

---

### 2.3 Normalized HIR (an invariant of `hew-hir`)

Normalization reduces the number of semantically equivalent structured forms
that SIR lowering must understand. It establishes canonical bindings and
pattern/control semantics; expands surface/operator/call/try sugar; makes
implicit returns explicit; and canonicalizes closure captures and
actor/machine references. It deliberately preserves useful Hew structure such
as ordinary `let` bindings rather than treating every structured construct as
junk to erase.

**Must not own:** CFG, SSA, layout, allocation, ABI, or target facts.

**Done means:** HIR → SIR lowering is mechanical and does not need a separate
case for every spelling of the same semantic operation.

#### [planned] Generic-instance service at the Normalized-HIR → SIR boundary

Genericity is semantic; concrete layout is representational. A normalized HIR
generic body remains a canonical template, for example `identity<T>(T) -> T`.
When SIR lowering encounters a resolved use such as `identity<i64>`, it asks a
single module-level instance service for the concrete semantic instance rather
than letting each lowering subsystem invent a monomorphization path:

```text
InstanceKey {
    item: ItemId,
    type_args: GenericArgs,
    selected_impls: ResolvedImplArgs,
}
```

The exact selected-implementation component is omitted when it is a pure,
deterministic consequence of the fully substituted types; it is included when
Hew permits two semantically distinct selected implementations for the same
type arguments. The service owns canonicalization, deterministic discovery
order/naming, caching, recursion/SCC handling, and the diagnostic chain that
led to an instance.

The same service owns one semantic instance graph: concrete function instances,
semantic type instances, and selected implementation evidence. A
`TypeInstanceKey { template, type_args }` deduplicates substituted nominal
shapes such as `Cache<String, i64>` and `HashMap<String, i64>` without naming a
layout. Raw MIR later consumes a semantic type instance together with target
layout to select representation.

It lowers a `GenericBodyTemplate + SubstitutionEnvironment` to a concrete SIR
function without permanently cloning a second HIR tree. Its instance graph has
separate semantic function identity from the generic HIR declaration: one HIR
`ItemId` can produce many concrete SIR function/callable instances. SIR calls
therefore name a concrete callable instance, never an unresolved generic item
plus ad-hoc substitutions.

Instantiation recursively resolves semantic types and selected implementations
(`Cache<String, i64>`, `HashMap<String, i64>`, `Hash<String>`, and `Eq<String>`
in one example), but it does **not** compute size, alignment, field offsets,
storage class, parameter ABI, runtime carrier, or destruction strategy. SIR
can consequently inline or simplify a concrete `HashMap<String, i64>` semantic
operation before any map object, pointer, bucket layout, or drop glue exists.
Those questions begin only at Raw MIR's target-parameterized representation
boundary.

**Done means:** generic function/type specialization is a canonical compiler
service used by every SIR-producing path; no MIR body lowerer re-specializes a
generic HIR body, and no concrete layout fact is embedded in `ResolvedTy` or
SIR instance identity.

---

### 2.4 SIR (`hew-sir`)

**Owns:** typed SSA `ValueId`s, `BlockId`s, basic blocks, block arguments,
semantic CFG edges, def-use relationships, dominance, semantic operations,
and optional/multi-origin source provenance. It is both the semantic CFG and
the canonical SSA optimization IR; there is no general non-SSA CFG before it.

Operands retain source-semantic ownership use modes:
`Read`, `BorrowShared`, `BorrowMut`, `Move`, and `Consume`. These facts make
rewriting legal or illegal, but SIR does not decide whether a consuming use
becomes a copy, clone, retain/release, move-out, or drop. That physical
realization belongs to MIR.

Effects are normally derived from an operation's kind and resolved callee
metadata, rather than stored as a mutable second source of truth. The current
initial subset exposes `pure`, `may trap`, and conservative unknown-call
barrier effects. Memory access, allocation, message, suspend, I/O, and
ownership-sensitive classes are introduced only with their first semantic
operations; effect-token SSA remains deferred until a concrete ordering
optimization requires it.

Language concepts stay semantic here until a feature-specific lowering needs
their representation: abstract aggregates; `variant.make`, `variant.test`, and
`variant.project` (never representation-implying `tag`/`payload` operations);
typed closures; typed actor handles and messages; and machine dispatch versus a
known transition. `actor.spawn` and `actor.send` may be ordinary effecting
operations; an ask can produce a future, while waiting belongs to suspension.

Potential suspension is always a terminator, not an ordinary operation:

```
Suspend {
    kind: SuspendKind,
    inputs: Vec<Operand>,
    resumes: Vec<ResumeEdge>,
    cancel: Option<Edge>,
}
```

An await has one normal resume edge; select, receive, timeout, cancellation,
and generator operations may have several. This is the semantic model; it does
not expose an LLVM coroutine representation.

`Unreachable` is likewise a semantic terminator, never a temporary marker for
an unfinished SIR builder block. Builders use a separate private completion
state. Before CFG simplification or DCE may create an unreachable block, Raw
MIR must have a verified unreachable legalization so every verifier-accepted
SIR terminator has a mechanical representation-lowering path.

**Must not own:** addressable `Place`s, stack/heap allocation, concrete
aggregate layout, byte offsets, discriminant encoding, ABI carriers, drop
scheduling, ownership proof, LLVM intrinsics, or target instruction selection.

**Verifier:** module identity; operation/result type and arity; use-mode and
semantic-op constraints; SSA definition/dominance; block-argument edge
arity/types; entry parameter initialization; and operation-specific semantic
invariants. The module SIR → MIR pipeline verifies at module scope before it
can construct representation or storage.

Optimization-safety verifier ledger:

| Rewrite                     | Required proof                                                                                                                 | Counterfactual evidence                                                                                                                                                                                   |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Constant-CFG region discard | Every newly unreachable block has no value or ownership use carrying a drop obligation and no operation with a `MayTrap` edge. | `optimize_cfg` proves ordinary structural verification accepts a folded candidate with a discarded trapping arm, while the discard-safety verifier rejects it and leaves the original function unchanged. |

**Dump:** `hew tool compile --dump-sir`.

### 2.5 Raw MIR (`hew-mir::raw`)

**Owns:** a backend-independent but target-parameterized CFG realization of
SIR values. It introduces virtual MIR values/temporaries and `Place`s where
addressable storage is actually needed, representation/layout choices using an
explicit target-layout contract, and physical value-model operations
(`borrow_read`, `move`, `cow_share`, `ensure_unique`, `materialize`,
`consume_call`, and `freeze`). It also owns runtime transport/carrier and
coroutine-frame decisions once their semantic concepts have been lowered.

**Current transitional adapter:** today's `RawLowerer` materializes every SIR
`ValueId` as `Place::Local` because the current Raw MIR body format expects
places. That bridge is useful compatibility evidence, but it is not the final
SIR → MIR contract.

#### [planned] Virtual-value / `Place` seam (required before general SIR → MIR)

Raw MIR gains an explicit virtual-value versus addressable-`Place` distinction
before this bridge becomes the general lowering path:

```text
RawValueId  = typed virtual operation result or Raw block argument
LocalId     = storage declaration identity
Place       = LocalId plus storage projections; never a virtual value
RawEdge     = target block plus typed RawValueId arguments
```

`Place::Value(RawValueId)` is forbidden. A virtual value becomes storage only
through an explicit `Materialize { value, local, reason }`, where `reason` is
one of `AddressTaken`, `MutablePlace`, `ByRefAbi`, `CaptureOrEscape`,
`Transport`, `CoroutineFrame`, `OwnershipDrop`, or `ExplicitStorage`.
Aggregate layout alone is never a materialization reason: representation-level
`Pack`/`Extract` keep an inline aggregate virtual until an addressable or
by-reference observation actually requires storage. This makes every loss of
value form auditable and revisable rather than an accidental property of a
lowerer.

Raw blocks use typed `RawValueId` arguments and every `Goto`/`Branch`/resume
edge carries a matching ordered value list. Ordinary mutable locals therefore
flow through SIR/Raw SSA and block arguments until a listed materialization
reason applies. A virtual value is not a stack slot, address, or `alloca`; SIR
scalar SSA must not be needlessly destroyed into memory for LLVM to reconstruct.
There is no generic `Cell<T>` escape hatch: add a cell/place only when an
address-taken or otherwise genuinely location-semantic feature requires it.

The first virtual domain is deliberately no-drop: scalar `BitCopy` values,
structural inline aggregates, and their CFG/call flow only. Values with
destruction, borrow, mutation/addressability, capture, transport, or suspension
obligations remain outside that domain until their materialization and
elaboration rules are implemented.

`RawMirModuleHeader` owns the target-parameterized `TargetLayout` and logical
linkage declarations used by Raw bodies. It is MIR metadata, not another IR and
not LLVM metadata: no LLVM type, calling-convention ID, intrinsic, or address
space appears there.

#### Mandatory stage finalization

Every Raw MIR function produced from SIR must finalize 1:1 into exactly one
Checked MIR function and exactly one Elaborated MIR function with the same
callable/function identity. Codegen never consumes Raw or header-only Checked
MIR. The no-drop virtual domain still produces a verified explicit zero-drop
Elaborated MIR body; it is not allowed to skip a stage.

Raw MIR consumes resolved HIR type/value-class facts and SIR use modes. Its
physical operations have not yet been _proven_ correct by ownership/concurrency
analysis.

**Must not own:** `LLVMTypeRef`, LLVM calling-convention or attribute IDs,
LLVM intrinsics, LLVM coroutine objects, LLVM address-space assumptions, or
proof of uniqueness/aliasing. It may be target-parameterized by pointer width,
endianness, alignment rules, and ABI integer widths, but remains independent of
an LLVM-specific backend.

**Verifier:** structural only — every block ends in a terminator, every place
is dominated by its definition, every site has a chosen value-model operation
(no "unclassified").

**Dump:** [current] `hew tool compile --dump-mir raw`.

#### Current raw coroutine substrate

The existing `RawMirFunction::Terminator::Suspend { resume, cleanup, is_final }`
is a backend-facing switched-resume coroutine carrier. It is not the source
semantic model for async: SIR owns the general `Suspend` terminator above, then
later lowers it to an appropriate Raw MIR/runtime representation. In particular,
LLVM coroutine intrinsics and the `HewCont` runtime ABI must not surface in
HIR or SIR. No source construct currently produces this raw terminator; its
synthetic test coverage is infrastructure evidence, not a reason to relax the
source async gates before a real readiness/waker path exists.

---

### 2.6 Checked MIR (`hew-mir::checked`)

**Owns:** proofs and legality: uniqueness and aliasing (read-shared XOR
mutate-unique at every program point), initialization/use-after-move,
borrow/suspension legality, actor-send escape/concurrency safety, and
cooperation requirements. It validates the physical strategy selected in Raw
MIR; it does not own generic ABI or representation policy.

**The fail-closed boundary for value semantics.** Diagnostics fire here in
value-cost language (see §4.3).

**Must not own:** drop elaboration, cleanup blocks, code emission.

**Verifier / diagnostics:** value-cost diagnostics — "value `s` is consumed
at <span> but read at <span>", "two mutations alias the same value", "affine
resource `c` would be shared across an actor send — consume or materialize".

**Dump:** [current] `hew tool compile --dump-mir checked` (annotation overlay: `// read-share`,
`// move (last use)`, `// ensure-unique → mutate`, `// materialize`, etc.).

---

### 2.7 Elaborated MIR (`hew-mir::elab`)

**Owns:** explicit `Drop(place)` statements on every exit path, explicit
cleanup basic blocks, panic-edge CFG, cancellation and actor-shutdown cleanup,
`DropPlan` per scope, and the **DecisionMap**. Raw MIR chooses a coroutine
frame, layout, and materialized place where representation requires one;
Elaborated MIR adds lifetime and cleanup obligations over those already chosen
places. It does not select storage classes or coroutine representation.

Every exit `DropPlan` is _derived_ from the Checked-MIR `OwnershipEvent`
stream (`derive_drop_plans_from_replay`): `required(exit)` is the exact set of
owner generations still live at that exit whose inline release does not
dominate it, and each becomes one `ElabDrop` through the owner's
definition-site `DropRecipe` and `Guard`. There is no function-wide LIFO
template, no allow-set prover, and no per-exit re-admission pass: a value the
lowering cannot safely drop must not `Mint` an owner (it is neutralized or
aliased explicitly at the mint site), and a value that mints is dropped on
every exit where it is still owned.

Two edge kinds are fixed by that rule rather than by a plan. A `Goto` never
discharges: replay carries the source block's whole exit state into the
target, and the Checked-MIR verifier requires a source-side `EdgeCarry` for
every generation live on that edge (and rejects a carry naming a generation
that is not live), so a body-local generation cannot cross a join without an
explicit `Release` in the event stream. Function-entry cancellation is the one
exit that runs before MIR's leading parameter `Mint`s execute; its cleanup is
the set of those parameter owners whose `ParamBoundary` decision fact (part of
Checked MIR, not a lowering ledger) says the ABI argument arrives owned.

A generation that is moved on one path into a join and still owned on
another has no admissible plan entry at any later exit (a drop would
double-free the moved path; omitting it leaks the owning one). Raw MIR
resolves this before sealing, from the same replay
(`materialize_conditional_consume_releases`): the release is placed on the
predecessor edge where the generation is still exactly live, provided the
owning storage is physically dead on entry to the join (no read reaches it
before a whole-local redefinition). Guarded generations keep their runtime
flag and lineage `Join` inputs keep the join rules. What the pass cannot
place — a producer that hands the value onward at the join without
publishing its `Transfer` — remains an unreported leak on the owning path;
closing that requires the producer fix, not a plan entry.

The `DecisionMap` is a deterministic table of
`DecisionFact { site_id, kind, chosen_strategy, why, cost_class }` keyed by
stable `SiteId`. It is attached as top-level function metadata on each
elaborated function and is emitted into IR dumps. `SiteId` is derived from the
typed-HIR/SIR/MIR structure (function id + canonical CFG path to the operation),
not from a source span — the table is stable across whitespace and reformat.

**Must not own:** re-running ownership analysis, LLVM values, span-keyed side
tables.

**Verifier:** every owning place has exactly one `Drop` on every exit path;
no `Drop` of a moved-out place; cleanup-block dominance; coroutine frame-slot
type matches yield value-class; DecisionMap is total and SiteIds are stable.

**Dump:** [current] `hew tool compile --dump-mir elab` (includes explicit drop / cleanup-block section
and DecisionMap).

---

### 2.8 LLVM IR lowering (`hew-codegen-rs`)

**Owns:** direct LLVM IR construction from Elaborated MIR using Inkwell,
including function declarations, storage only for MIR places that require it,
value loads/stores, arithmetic/control-flow lowering, runtime-symbol
references, and LLVM module verification. This is the boundary at which Hew
semantics become low-level LLVM computation; LLVM IR optimization may then run
over that computation.

DecisionFacts stay attached to MIR inputs. Codegen may carry them into debug
metadata or comments in dumps, but it must not reinterpret them or synthesize a
replacement side table.

**Must not own:** re-deriving any value-model fact; inventing ABI shape from
LLVM value layout; ownership/drop decisions; source-level diagnostics; target
legalization; instruction selection; or machine policy.

**Verifier:** `Module::verify()` after emission; fail-closed errors for
unsupported MIR constructs, missing locals, unresolved runtime symbols, and
LLVM verifier failures.

**Dump:** textual `.ll` plus any requested MIR dump (`hew tool compile
--dump-mir raw|checked|elab`).

---

### 2.9 LLVM backend and execution modes

**Owns:** LLVM's target-specific work below LLVM IR: target legalization,
instruction selection, Machine IR, register allocation, scheduling, and object
or module emission. Native builds write a relocatable object and then link it
with `libhew.a`; Wasm builds write a Wasm object and link it into a standalone
module with `wasm-ld` or `rust-lld`; embedded targets produce their appropriate
object or image.

ORC JIT is an execution mode over the same verified LLVM IR. It is not a Hew
target and must not introduce a JIT-only frontend, SIR, or MIR path.

**Must not own:** Hew-level semantics, ownership/drop decisions, or checker
diagnostics. Unsupported target substrates must report a named fail-closed
diagnostic instead of falling back silently.

**Verifier:** LLVM target emission errors and linker exit status.

**Dump:** `.o`, executable, `.wasm.o`, `.wasm`.

---

## 3. Value model

### 3.1 Value classes

Every type in Hew v0.5 belongs to exactly one **ValueClass**. Classification
is structural — propagated through fields — unless the type declares a marker.

| ValueClass        | User-facing name | Marker                  | Description                                                                                                                                                                                                       |
| ----------------- | ---------------- | ----------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `BitCopy`         | Copy             | (structural)            | Strict bit-copy; no destructor, no COW, no refcount. Integers, bools, floats, chars, unit, tuples and fixed-arrays of Copy types. **Not** "anything cheap to copy" — COW values are a separate class.             |
| `CowValue`        | Value            | `@value` (opt)          | Value semantics with COW implementation. String, Vec<T>, Map<K,V>, Set<T>, and user structs whose fields are all Copy or Value (default for user structs). Refcounted backing; mutation triggers `ensure_unique`. |
| `PersistentShare` | Shareable        | (stdlib types)          | Explicitly shared persistent data structures (HAMT maps/sets, RRB vectors). Structural sharing across versions; reads are cheap; writes produce a new version. Always safe to share; no COW needed.               |
| `AffineResource`  | Resource         | `@linear` / `@resource` | At-most-one-owner resources: file handles, sockets, channels, capability handles. No refcount, no COW. Consumed on last use; sharing is a checker error.                                                          |
| `View`            | View             | (compiler-only)         | Borrowed read-only window into another value. Users do not name Views in v0.5 surface syntax; they appear as method receivers and iterator yields. Compiler proves View does not outlive its producer.            |

**User struct default:** a user struct is `CowValue` if all its fields are
Copy or Value, `BitCopy` if all its fields are Copy, and requires `@linear` /
`@resource` to be `AffineResource`. The user may spell `@value` explicitly
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
  source record unaffected. For Value records: cow_share + ensure_unique.
  For Copy records: bit-copy with field replaced (free).
- **Containers:** `[1, 2, 3]` and `{"k": v}` literals produce `CowValue`
  collections. Mutation through a `var` binding does ensure_unique-then-mutate.
- **Strings:** `String` is `CowValue`. String slices are `View`.
  No user-facing distinction between "owned" and "borrowed" string.
- **Resources:** declared at the type with `@linear` (single-owner, no drop
  side effect) or `@resource` (single-owner, has a drop side effect — file
  handle, socket, capability).

### 3.3 Semantic use modes and physical MIR operations

HIR establishes source intent and SIR carries it on each operand as one of
`Read`, `BorrowShared`, `BorrowMut`, `Move`, or `Consume`. This is semantic
information: an optimizer must not duplicate or reorder a move/consume as
though it were an ordinary read. It does not prescribe a refcount, copy,
allocation, or storage mechanism.

Raw MIR realizes that semantic use through one physical operation or strategy.
Checked MIR proves the selected realization legal. Users never type these
operation names; diagnostics and the Ownership Plan Report may expose their
cost in user-facing terms.

| Operation       | When chosen                                                       | Cost class           |
| --------------- | ----------------------------------------------------------------- | -------------------- |
| `borrow_read`   | Immutable read; no refcount touch. View / COW-share-read.         | Free                 |
| `move`          | Last use; transfer ownership without copy.                        | Free                 |
| `cow_share`     | Shared use of a CowValue; bumps refcount.                         | RefcountTouch        |
| `ensure_unique` | Prepare a CowValue for mutation; clones if refcount > 1.          | OAlloc (conditional) |
| `materialize`   | Deep copy / clone-now. Only operation with unbounded cost.        | OCopyN               |
| `consume_call`  | Pass to a callee that takes ownership.                            | OResourceTransfer    |
| `drop`          | Deterministic destructor; explicit in Elaborated MIR.             | (implicit)           |
| `freeze`        | Immutable snapshot of a `var` for crossing a yield/send boundary. | Free                 |

### 3.4 Actor / concurrency rules

Actor isolation is the central concurrency boundary. Send-path classification
is validated in Checked MIR after SIR has retained the typed actor handle,
resolved `ActorMethodId`, and typed semantic message:

- **Transfer mode:** last-use of an owned value; sender loses access, receiver
  gains it. Cheapest path. For AffineResource, the only valid send mode.
- **Share mode:** CowValue or PersistentShare; sender retains access, receiver
  gets a refcount bump. Safe: COW guarantees no observable mutation across
  aliases.
- **Materialize mode:** deep copy on send. Used when the value is reachable
  from the sender after the send and is not COW/persistent, or when an explicit
  `copy(x)` is used.
- **AffineResource sends:** consume-or-error. Either the resource is last-used
  at the send site (transfer) or the send is rejected.

**`actor_scope { … }`** (v0.5 opt-in primitive): spawns child actors whose
lifetimes are bounded by the scope. On scope exit, the runtime guarantees all
child actors have drained their mailboxes and their resources have been dropped.
Lowers to a `hew.scope` op with attached actor-cleanup edges in Elaborated MIR.
The unstructured `spawn` path remains available.

**Refcount strategy:** actor-local non-atomic RC with cross-actor promotion
(v0.5 decision). COW values that cross an actor boundary (share mode) are
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
in user-facing diagnostics. User diagnostics use:

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
grouped by function, ordered by SiteId. Each row:
`site → kind → value-class → strategy → cost → why`.

A summary footer reports the function's hidden-copy budget (sum of `OCopyN`
and `OAlloc` sites) and flags any site over the per-function budget threshold.

```
hew build --emit-decisions=json
```

Emits the same DecisionMap as newline-delimited JSON keyed by SiteId. Schema
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

`SiteId` is derived from typed-HIR/SIR/MIR structure (function id + canonical
path through the elaborated CFG), not from a source span. `DecisionFact`s travel
with Elaborated MIR and its deterministic dump/JSON metadata; they do not
require a dialect attribute system.

### 4.3 Hidden-copy budget

Each function carries an implicit budget for `materialize` and
`ensure_unique`-with-clone sites. Default: **note-level** at the first hidden
materialize per function in non-test code (user-confirmed default; configurable
per-crate). Explicit `copy(x)` and `consume(x)` never count against the budget.

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
companion `.ownership-plan.txt` expected reports. These are **implementation
targets** for the v0.5 value-model checker and Elaborated MIR implementation:
the checker must produce output matching the companion files byte-for-byte
(modulo source locations).

See `tests/corpus/v05-value-model/README.md` for the full index and naming
conventions.

---

## 6. Checker and lowering work

The checker and lowering work should be introduced as cohesive compiler changes
against the v0.5 value model, progressing through each layer of the IR ladder
(resolved + typed HIR → normalized HIR → SIR → Raw MIR → Checked MIR →
Elaborated MIR → LLVM IR → LLVM backend). The corpus fixtures in
`tests/corpus/v05-value-model/` serve as the byte-level acceptance targets for
the Checked MIR and Elaborated MIR stages.

---

## 7. Building the v0.5 spine

At hard cutover, `hew tool compile` runs the v0.5 Rust
HIR/SIR/MIR/codegen-rs path. The backend is a normal Cargo dependency of `hew`;
no retired C++ backend build step is involved:

```
make hew          # debug: builds hew
make release      # release: builds hew + stdlib
```

---

_This document is an internal engineering reference. The public-facing
language specification is `docs/specs/HEW-SPEC-2026.md`._
