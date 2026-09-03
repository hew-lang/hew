# Hew IR Ladder — Normative Internal Reference (v2, revision 7)

This document is the contract for the final Hew IR ladder. It replaces
`docs/internal/v05-ir-ladder.md` (deleted in P0; §9 lists every reference that
moves). It is written for the one person implementing the program in
`hew-orchestration/plans/final-ladder-program.md`; every section names the
files on `main` (54e8dde2c) it is grounded on and the phase (P0–P6) in which
each obligation lands. Nothing here is `[planned]` without a phase letter.
Where this design changes what `main` does for a user program, §11 names the
sentence in the spec or the ownership contract that loses and the sentence
that replaces it.

Two conventions:

- **must** is testable: every "must" is paired with the command, gate, or test
  kind that proves it.
- **[current]** names what `main` does today; **[Pn]** names what this design
  requires by the end of phase n. Marked shortcuts carry WHY / WHEN-OBSOLETE /
  WHAT.

Revision 6 resolves twelve findings against `main`; each carries its evidence
inline and, where it changes a user-visible decision, a §11 row. Six were
soundness or totality holes in revision 5 rather than in `main`: the four
deadline-await `HirExprKind` variants had no §1.5 row (§1.5); the borrowed
message payload was a use-after-free for any handler that suspends, so there is
now one dispatch disposition (§5.6, §1.4, §11 row 13); `state_clone_fn` has
three dispatch readers, not none, and deleting it moves an admission gate
(§5.7); the state-field taken bit must be set **before** its release, which is
what lets §4.3's escrow bracket narrow to the registry target (§1.3.6, §4.3,
§4.7); `fork` was `CowValue`-only and left `Vec<Conn>.push` with no admitted
op (§1.3); and a `dyn … + Send` over an `Rc`-holding concrete was a second
authority for the send fact, now closed by a coercion-site wall (§1.1, §11
row 37). Four were miscited or unimplementable claims — `header_validate` as
an aliasing check (§5.6), `hew_vec_remove_at_layout` as the owned move-out
(§5.3), the `--no-typecheck` walls gate (§1.6), and the arc-versus-box carrier
seam read as per-callable (§7). Two were representable-wrong-answer holes: the
taken bit's two runtime readers (§1.3.6, §11 row 35), and `TypeFacts.send` is
no longer a `bool` a consumer can misread for a `Closure` key (§6.3).

Revision 7 reconciles this document with `sir-domain-matrix.md`,
`runtime-ownership-table.md` and the plan, under the precedence rule below.
It carries no new grounding of its own; it removes the places where the three
documents contradicted each other: the `Spawn` handle left in §1.3.1's `Owned`
producers, the `hew_vec_free_owned` leaf of §5.2 item 6, the split symbol-table
authority of §5.1 and §9, the P3 tag on the missing-FFI-row build error (§6.4),
the coverage-ratchet and parity wording that ran ahead of the shipped tool
(§7), the P1 verifier gates written as if they existed (§1.6, §2.1, §4.5, §10),
the restored **Design axioms** section below, the probe corpus moved from
`.tmp/` into `repros/ladder/`, and §11's precedence claim over the plan.

## When these documents disagree

Quoting `hew-orchestration/plans/final-ladder-program.md` §5.1 in full, which
is the authority for this paragraph and for the identical paragraph in
`sir-domain-matrix.md` and `runtime-ownership-table.md`:

> `docs/internal/ir-ladder.md` decides SIR ops, ownership kinds, MIR forms, and
> runtime symbol names; `docs/internal/sir-domain-matrix.md` decides which
> phase owns a construct; `docs/internal/runtime-ownership-table.md` decides a
> runtime symbol's parameter and result ownership. This plan decides sequencing
> and gates. A disagreement inside a document's own domain is a defect in the
> other document, fixed in the same PR that finds it; none of the four is a
> fallback for another.

No other precedence rule is in force. §11 below is the authority over the
**spec and `docs/v05/ownership.md`** sentences it names, and over nothing else.

Probes are committed under `repros/ladder/` with `repros/ladder/README.md`
naming the section and row that cites each one. Every transcript quoted below
was produced by `hew run` with `TMPDIR` outside the checkout, on the PATH
binary `hew 0.6.0-rc3-dev.141+fa2986bb2` (one commit behind 54e8dde2c; the
delta touches only callable-key validation), except `state_reinit.hew` and
`dyn_rc.hew`, which were run on `hew 0.6.0-rc3-dev.142+54e8dde2c` and
re-reproduced on that binary when they were committed.

```
AST → typed+resolved HIR → (normalize) → SIR (OSSA) → MIR (one form) → LLVM
   hew-parser    hew-types/hew-hir        hew-sir      hew-mir       hew-codegen-rs
```

Three IRs after HIR. Each has one job:

| Layer | Question it answers | Verifier |
| --- | --- | --- |
| HIR | What does this mean in Hew? Which declaration is this? | checker + HIR verify |
| SIR | Who owns each value, on every path? | OSSA verifier (§2) |
| MIR | How is each value represented, stored, transported, released? | MIR balance verifier (§4.5) |
| LLVM | What computation executes? | `Module::verify()` |

## Design axioms

Three cross-cutting invariants the ladder, the gates, and the runtime are built
to preserve. They are carried forward from `docs/internal/v05-ir-ladder.md`
(the document this one replaces); the statements are unchanged and the
exemplars are repointed at the final ladder, because every exemplar the old
text named is on §9's deletion list.

- **Totality at decision points.** Internal semantic decision enums, and the
  dispatch tables over them, are total: no semantic branch carries a silent
  wildcard or default arm. Adding a variant fails compilation at every consumer
  site until that site decides what the new case means. The exemplar is
  verifier rule 5 (§2.1): every call argument, send argument, return and
  capture carries a decided mode, and **an undecided mode does not exist as a
  value** — there is no `Unknown` variant to fall through to, so a new operand
  position is a build break at every match over the mode set. Genuinely
  undecidable cases take the sanctioned form of an explicit fail-closed
  refusal — `Admission::Legacy(reason)` before P5 (§7), `E_SIR_ICE` after —
  because "undecidable" is itself a decision, and it must reject, never fall
  through. A wildcard arm on a semantic branch converts a future variant into
  silent misbehaviour; totality converts it into a build break that names every
  site owing a decision.

- **Leak-never-double-free total order.** Release bugs have a strict severity
  order, and the gates enforce it as a total one: over-release is
  unconditionally rejected (a refcount that would go below zero aborts, in
  every build mode), under-release is tracked and ratcheted shrink-only, and a
  process-terminating abort may leak-at-abort. Under this design the *unwind*
  edge is no longer in the leak-at-abort bucket: it is a path, rule 1 counts
  it, and every `Owned` value live across it is destroyed on it (§1.3.3, §2.1
  rule 1, §4.7; `docs/v05/ownership.md`'s "Abort and trap paths may leak-at-abort"
  loses, §11 row 18). What remains leak-at-abort is the terminating abort
  itself — the `A` and `P` trap classes of `runtime-ownership-table.md` §7,
  which have no cleanup edge because the process does not continue.

- **Observable honesty.** Any silent adaptive behaviour in the runtime or
  toolchain — execution-lane fallback, COW share-versus-materialize selection,
  retained-version selection, supervision restart/rehoming — is observable in
  the artifact's output, response, or telemetry. A fallback that cannot be
  observed is a fail-open: the system substitutes behaviour without leaving
  evidence, and neither users nor gates can tell the substituted path from the
  intended one. The transitional routing of §7 is this axiom applied to the
  ladder itself — `hew tool sir-coverage` prints `legacy:<reason>` per
  callable and `make sir-parity` runs both routes, so the one remaining
  fallback in the compiler is counted and diffed rather than silent — and the
  `std::observe` counters ([`docs/observe.md`](../observe.md)) carry the
  runtime side.

---

## 1. SIR is Ownership SSA

### 1.1 Value classes: one authority, two columns

**[current]** Four predicates answer "is this type bit-copyable" and they
disagree:

| Predicate | File | Verdict on `(i64, i64)` | Verdict on `(string, i64)` |
| --- | --- | --- | --- |
| `Ty::is_copy` | `hew-types/src/ty.rs:1452` | Copy | not Copy |
| `ValueClass::of_ty` | `hew-hir/src/value_class.rs:401-404` | `CowValue` | `CowValue` |
| `ty_drop_obligation_inner` | `hew-mir/src/model.rs:2000` (heap walk) | no obligation | obligation |
| `container_ingress_is_copy_in(&str)` | `hew-mir/src/model.rs:3261` | by symbol string | by symbol string |

(`hew-types/src/vec_authority.rs:270 is_copy_layout` is a fifth, layout-shaped
copy.)

**[P1] The authority is `TypeFacts { class, clone, send, hash, eq }` per
substituted type (§6.3)**, computed once by the checker and published in
`TypeCheckOutput.type_facts`. `class: ValueClass` decides the ownership kind
(§1.2) and the destructor shape (§5.2); `clone: CloneKind` decides whether a
`copy_value` exists and what it costs. **Class alone does not decide copy
legality** — `Rc<T>` and `Generator` are both `AffineResource` (one owner, an
implicit destructor) yet `Rc` has a retain path (`hew_rc_clone`,
`hew-runtime/src/rc.rs:156`) and a generator has none. Rule 6b (§2.1) reads
`clone`, not `class`. `ValueClass::of_ty` moves verbatim from
`hew-hir/src/value_class.rs` into `hew-types` and is extended to the table
below; the other four predicates are deleted (§9).

The structural rule, total over `ResolvedTy`. For `Named { builtin: Some(b) }`
the first authority is `BuiltinType::marker()` (`hew-types/src/builtin_type.rs:355`,
today's `lookup_type_marker_for_ty` input); the rows below name every variant
so the per-arm test can be written against a closed list.

| `ResolvedTy` | class | `clone` | notes |
| --- | --- | --- | --- |
| integers, floats, `Bool`, `Char`, `Unit`, `Never`, `Duration` | `BitCopy` | `Bits` | unchanged from `of_ty` |
| `Named{builtin}` with marker `BitCopy`: `SupervisorPool`, `ChildRef`, `NodeId`, `Location`, `RemotePid`, `MonitorId`, `DownTarget`, `DownReason`, `DownNotification` | `BitCopy` | `Bits` | `NodeId`/`Location` are empty std records (`std/builtins.hew:26-31`); `DownNotification` is three BitCopy fields (`std/link_monitor.hew:90-94`) |
| `Named{builtin: Instant \| Unit \| Duration \| Range \| Trap \| TimeoutError \| CrashAction \| CrashKind \| SendError \| AskError \| LookupError \| RecvError \| LinkError \| MonitorError \| CloseError}` | `BitCopy` | `Bits` | marker `None` today; scalars and std enums whose variants are unit or carry `BitCopy` payloads (`std/builtins.hew:49-188`, `std/failure.hew:52-111`, `std/link_monitor.hew:25`; `CloseError::Io(IoError)` with `IoError` payloads `i64`, `std/io/closable.hew:67`, `std/fs.hew:42-46`). **decision**: these get marker `BitCopy` at P1 so `marker()` and this table agree by construction; the per-variant test asserts the std declaration is all-`BitCopy`, and `builtin_marker_and_the_class_table_agree` refuses a `None` marker whose class is `BitCopy` so the next such row cannot land silently |
| `Named{builtin: LocalPid \| HewActor}` | `BitCopy` | `Bits` | **decision, overrides `marker() = Resource`**: no `close_method()` row for `LocalPid` (`builtin_type.rs:394`), and codegen already treats them as non-owning (`ty_is_nonowning_pid_leaf`, `llvm.rs:25479`, `(Resource, drop_fn: None)` is a no-op). The marker becomes `BitCopy` at P5 with the legacy carrier that reads it (§9): flipping it at P1 routes a `Vec<LocalPid<_>>` element off its pointer ABI and moves an elaborated-MIR baseline, and `HewActor`/`BoxedActor` carry a close method a `BitCopy` builtin may not register. A pid never owns the actor |
| `Named{builtin: BoxedActor}` | `AffineResource` | `None` | compiler-internal opaque carrier, marker `Resource`, `close_method() = Some("close")`, handle family `ActorRuntime` (`hew-hir/src/builtin_type_classes.rs:647-654`) but no runtime lifecycle descriptor (`state_clone.rs:1872-1880` fails closed). It never reaches a user program; P4 either names its release symbol in `runtime_symbols.rs` or deletes the variant |
| `Named{builtin: Iterator}` | never a value type | — | `Iterator` is the std trait name (`std/builtins.hew:362`, arity 0); a `dyn Iterator` value is `ResolvedTy::TraitObject`. The table test asserts the arm is unreachable for values |
| `Named{builtin: Option \| Result}` | aggregate rule | aggregate rule | enums; `Option<Rc<T>>` is `AffineResource` |
| `Named{builtin: Vec \| HashMap \| HashSet}` | **aggregate rule over the element (key, value) classes**: `Vec<string>`/`Vec<Record{string}>` → `CowValue`; `Vec<#[resource] T>`/`Vec<Rc<T>>` → `AffineResource`; `Vec<#[linear] T>` → `Linear`; `Vec<i64>` → `CowValue` (the buffer is heap; the collection is never `BitCopy`) | `DeepCopy` when every element is `BitCopy`; `FieldWise` (structure copied, elements through `hew_copy$Elem`) when the element has `clone ∈ {Retain, DeepCopy, FieldWise}`; `None` when the element has `clone == None` | **decision** (F-collections): the old row classed every collection `CowValue`/`DeepCopy` regardless of element. `main` accepts `Vec<Conn>` and closes each element at scope exit (`repros/ladder/vec_resource_drop.hew` → `2`, `close 1`, `close 2`), moves it on `let w = v` (`repros/ladder/vec_resource.hew` → `E_MIR_CHECK … UseAfterConsume`), and keeps a `Weak` inside `Vec<Rc<i64>>` upgradable until scope exit (`repros/ladder/vec_rc_weak.hew` → `1`, `5`). Only the element-joined class reproduces all three: `let w = v` is `move` for an `AffineResource` collection (§11 row 3), `clone v` is 6b, the destroy is never sunk (§3), and the descriptor carries `clone_fn = None` (§5.3) |
| `Named{builtin: VecIter \| HashMapIter}` | aggregate rule (the `Vec`/`HashMap` field's class) | as the field | synthetic records of the `for x in v` / `for (k, v) in m` desugar (`hew-hir/src/lower.rs:26504 lower_for_iter_desugar`; the `VecIter` item is the sentinel at lower.rs:557, `vec_iter_field_shape`); a collection field plus BitCopy cursor fields |
| `Named{builtin: CrashInfo \| CrashNotification}` | aggregate rule | aggregate rule | `CrashInfo { code, message: string }` → `CowValue` (`std/failure.hew:33-36`); `CrashNotification { actor_id, kind }` → `BitCopy` (`std/failure.hew:83-92`) |
| `String`, `Bytes` | `CowValue` | `Retain` (refcount +1; a string is immutable, a bytes mutator forks inside the runtime, §4.3) | |
| `Tuple`, `Array` | aggregate rule | aggregate rule | **decision**: resolves the `is_copy` vs `of_ty` split |
| user record / enum with no marker | aggregate rule | aggregate rule | **decision**: replaces `ValueClass::Unknown` (15 consumer sites, all `Strategy::UnknownBlocked` refusals); a user type is never unclassified. An `indirect` enum (`HirTypeDecl.is_indirect`, node.rs:1016-1040) keeps its payload class and its `clone` is `FieldWise`: `hew_copy$E` allocates a fresh box and copies the payload into it — the box carries no refcount, so a retain of the box does not exist (§5.2 item 2) |
| `Named` with `#[resource]` marker; `Named{builtin}` with marker `Resource` and a `close_method()`: `Duplex`, `Sink`, `Stream`, `Sender`, `Receiver`, `HewDuplex`, `HewSendHalf`, `HewRecvHalf`, `SendHalf`, `RecvHalf`, `LambdaActorHandle`, `MonitorRef`, `CancellationToken` | `AffineResource` | `None` (`LambdaPid`: `Retain`, §5.4) | implicit destructor is the registered close/release symbol |
| `Named{builtin: StreamPair}`, the regex `Pattern` handle | `AffineResource` | `None` | marker `None` in `builtin_type.rs`, but the std declarations carry `#[resource]` (`std/stream.hew:247-249` `#[resource] #[opaque] pub type StreamPair` with `close(consuming self)` → `hew_stream_pair_free`; `std/text/regex/regex.hew:28-29`), which `lookup_type_marker_for_ty` already reads |
| `Named{builtin: Generator \| AsyncGenerator \| Rc \| Weak}` | `AffineResource` | generators `None`; `Rc`/`Weak` `Retain` | |
| `Named{builtin: LambdaPid}` | `AffineResource` | `Retain` (`hew_lambda_actor_clone` mints a new handle) | a send of a `LambdaPid` is `Transfer` only (rule 5, §11 row 5): `repros/ladder/lambda_send_twice.hew` → `use of moved value \`w\`` on the second send |
| `Function`, `TraitObject` (incl. `dyn Iterator`) | `PersistentShare` | `Retain` (`hew_arc_clone`) | §5.4: refcounted box, never forked; **design change** against `main` (§11 row 7). A bare named-fn value (`ResolvedRef::Item`, ids.rs:25) is a `{fn, env = null}` pair; `hew_arc_clone`/`hew_arc_drop` return/return-early on a null pointer (`arc.rs:157, 184`), so its `copy_value`/`destroy_value` are no-ops at run time and need no special case. A `dyn Trait` is flat: the concrete payload's class is known only at `CoerceToDynTrait`, never on the `TraitObject` type, so it is **not** joined into the class — see the §3 destroy-sinking restriction, which is `CowValue`-only for exactly this reason. Flatness is safe for the *class* and unsafe for the *send fact*, so the send fact is bought with a wall at the coercion: **[P1 decision] `CoerceToDynTrait` into a `dyn … + Send` requires the concrete to be `Send`**, `E_OWN_SEND_UNSUPPORTED` otherwise. Without it, `traits.rs:1072-1086` decides `Send` from the bound name alone (test `dyn_trait_plus_send_is_send`, traits.rs:1799-1825) while `coerce.rs:432-444` records a `DynCoercion` on object safety alone with no marker check (`grep -n 'MarkerTrait::Send\|implements_marker' hew-types/src/check/coerce.rs` is empty), so a `dyn Handler + Send` over a concrete holding an `Rc<T>` would be a `PersistentShare` with send fact true, get `Snapshot::Share` under rule 5, and let two actors race the non-atomic `hew_rc_*` count — the very argument §5.4 makes for closures and `LambdaPid`. The wall makes the type-level fact sound for every value of the type, because the coercion is the only producer of a `dyn` value. §11 row 37 |
| `Closure` | **`PersistentShare` joined with the aggregate rule over the capture classes** | `Retain` (`hew_arc_clone` on the env) | **decision**: the env is a record and its captures are its fields (§1.3.5: every env owns its captures), so a closure capturing a `#[resource] Conn` or an `Rc<T>` is `AffineResource`, not `PersistentShare`. Consequences that a flat row got wrong: rule 5 gives it `Transfer` only, so a `move \|\| { conn.close() }` closure cannot be `Share`d into a second actor and raced (the same argument that rejected `Share` of a `LambdaPid`); §3 never sinks its release, so an `Rc` capture's `Weak.upgrade()` still flips at scope exit; a `Linear` capture is refused at the capture site regardless (§1.3.5). `clone` stays `Retain` in every case — retaining the env duplicates the handle, not the capture, exactly as `Rc<T>` does. A closure with a `BorrowMut` capture has send fact **false** (§1.3.5, rule 6c). §11 row 33 |
| `Named` with `#[linear]` marker, `Task(_)` | `Linear` | `None` | no implicit destructor; must be consumed (6d, with the `Task` cancel-edge exemption of §2.1). Only a **bound** task handle (`fork t = f()`) is a SIR value; an unbound spawn produces no value (§1.5) |
| `Named{builtin: ActorState \| MachineState}` | never a value type | — | **correction**: these are the two reserved *names* `ActorState`/`MachineState`, arity 1, roles `ActorStatePayload`/`MachineStatePayload` (`builtin_type.rs:1093-1108`; a user machine literally named `MachineState` normalizes to them, `check/tests/collections.rs:3017-3020`). They are compiler-internal payload carriers, never the type of a user value: a machine value is `Named{builtin: None}` (`var light: TrafficLight`), an actor's state record is a set of `Place`s (§1.3.6), not a value. The `Linear` marker on them (`builtin_type.rs:383`) stays as the admissibility gate it is today; the table test asserts the arm is unreachable for values, like `Iterator` |
| `Slice`, `Pointer`, `Borrow` | `View` | `Bits` | extern-boundary, non-owning; `Pointer` keeps `Ty::is_copy`'s Copy verdict because `View` is `None`-kind (§6.3) |
| `TypeParam` | never reaches SIR | — | the instance service (§6.2) substitutes first; the verifier rejects a `TypeParam` in any SIR type as `E_SIR_ICE` |

**Aggregate rule** (records, enums, tuples, arrays, `Option`/`Result`,
collections over their element types, `VecIter`, `StreamPair`, `CrashInfo`,
**and a `Closure`'s captures**; a `TraitObject` is not an aggregate — its
payload is not part of its type):
the class is the join of the field (element) classes in this order — any
field `Linear` → `Linear`; else any field `AffineResource` →
`AffineResource`; else any field `CowValue` or `PersistentShare` →
`CowValue`; else `BitCopy` (a collection is at least `CowValue`, its buffer is
heap). `clone` is `Bits` for `BitCopy`; `None` when any field has `clone ==
None`; else the field-wise glue `hew_copy$T` (§5.1), which is `FieldWise`
(for a collection: structure copied, elements through `hew_copy$Elem` — a
`Vec<Rc<T>>` copy retains each `Rc`). A `Closure` is the one aggregate whose
`clone` does **not** follow the join: retaining a closure is
`hew_arc_clone` of the env, which duplicates no capture, so its `clone` is
`Retain` whatever its captures are. This is what stops a record holding a
`Sender` from acquiring a copy path by accident, and what gives §3's
destroy-sinking restriction ("no `AffineResource`/`Linear` leaf reachable")
as a class test rather than a second type walk.

What the old table got wrong against code (the old doc's §3.1): `PersistentShare`
is closures/functions/trait objects, not HAMT structures (none exist);
`View` is `Slice`/`Pointer`/`Borrow`, not string slices; `Linear` exists; and
`#[resource]`/`#[linear]` **are shipped** (`docs/specs/HEW-SPEC-2026.md`
§3.7.8, `std/fs.hew:509`, `std/net/net.hew:473`, 15 + 4 vertical-slice accept
fixtures use them). What is reserved is the `resource` *kind keyword*
(`docs/v05/ownership.md` "`resource` — reserved"): a `resource type` with
move-on-send. This design keeps the attribute markers and does not ship the
kind. `ResourceMarker` is single-valued (`hew-parser/src/ast.rs:1036-1041`:
`None | Resource | Linear`), so a type is never both `#[linear]` and
`#[resource]` — spec §3.7.8.4 Path 3's "admitted only when the field's type
*also* satisfies `#[resource]` semantics" is vacuous today (§1.3.6).

Test: `hew-types` unit table test over every `ResolvedTy` arm **and every
`BuiltinType` variant** (a `match` with no wildcard arm, so a new variant is a
compile error, not a silent `Unknown`) asserting `(class, clone)`, including
`Vec<Conn>` → `(AffineResource, None)`, `Vec<Rc<i64>>` →
`(AffineResource, FieldWise)`, a closure capturing an `i64` →
`(PersistentShare, Retain)` and one capturing a `Conn` →
`(AffineResource, Retain)`; ast-grep rule in `rules/rust` forbidding any
new `fn *is_copy*` / `*owns_heap*` outside `hew-types` (added P1, CI `make
lint`).

### 1.2 Ownership kind on `ValueId`

Every SIR `ValueId` carries `own: OwnKind`, a pure function of its type's
class:

| `ValueClass` | `OwnKind` | obligation |
| --- | --- | --- |
| `BitCopy`, `View` | `None` | none; may be used any number of times |
| `CowValue`, `PersistentShare`, `AffineResource`, `Linear` | `Owned` | exactly one consuming use per path |
| any `Owned` value inside a borrow scope | `Guaranteed` | none; must not outlive the scope |

`Guaranteed` is not a class; it is the kind of the result of `begin_borrow`.
`Linear` differs from `AffineResource` only in which consumers are legal
(§2.1 rule 6d: `destroy_value` is not a legal consumer of a `Linear` value on
a normal or cancel exit).

### 1.3 Ownership operations

All ownership is explicit in the op stream. **The `UseMode` set is deleted**
[P1]: `UseMode { Read, BorrowShared, BorrowMut, Move, Consume }`
(`hew-sir/src/model.rs:73`) and the `mode` field of `Operand { value, mode }`
go away together, replaced by the op set below. An operand's mode **is the op
it feeds** — `begin_borrow`, `copy_value`, `move`, `fork`, `load.*`,
`store.*` — and never a side tag on a read. This is the one statement of that
rule; nothing in this document, `sir-domain-matrix.md` or
`runtime-ownership-table.md` annotates an operand `Read`, `BorrowShared` or
`BorrowMut`. Two live spellings survive the deletion and are **not** operand
modes: `ClosureCaptureMode::BorrowMut`, a capture kind (§1.1 `Closure` row,
§1.3.5), and the closed decided-mode set of rule 5, `{Borrow, Copy, Move,
Snapshot{Share, DeepCopy, Transfer}}`, which is a per-argument *decision* the
verifier checks, not a tag the reader consults instead of the op.
A mutating call is therefore spelled one of exactly two ways: on an SSA value,
`fork %v → %v'` then `borrow{%v'}` around the call; on a place, `load.take %p`
→ `fork` → `borrow` → call → `store.init %p`. The "Emitted for" column names HIR
constructs (`hew-hir/src/node.rs`) only; MIR instruction names
(`ActorStateFieldLoad`, `ClosureEnvFieldStore`, …) are realizations in §4.3,
never emitter inputs. §1.3.8 is the closed list every HIR variant must appear
in.

| Op | Operands | Result | Verifier obligation | Emitted for (HIR, `hew-hir/src/node.rs`) |
| --- | --- | --- | --- | --- |
| `copy_value %v` | `%v : Owned` or `Guaranteed` (not consumed; a `Guaranteed` operand is a projection read out through a live borrow, and the §3 copy-propagation rewrite deletes the pair when the copy has no consumer of its own); type has `clone ≠ None` | `Owned` | new obligation; `%v` must be live; rule 6b | `BindingRef` of an `Owned` binding in value position that is not its last use; `let y = x` when `x` is `CowValue` or `PersistentShare` (§11 row 3; a closure with an affine capture is `AffineResource` by §1.1 and follows the `move` row instead); `FieldAccess`/`TupleIndex`/`Index` read-out of a live composite (inside a borrow); `StructInit { base: Some(b) }` un-overridden fields when `b` is not a last use (per field, inside the borrow of `b`); every `Closure`/`SpawnLambdaActor` capture with `ClosureCaptureMode::{Copy, Borrow, BorrowMut}` / `HirCaptureKind::Strong` (§1.3.5: the environment owns its own copy); every `HirGenCapture` (`Local` and `ActorStateField`, §1.3.5); `ActorSend`/`ActorAsk`/`Spawn`/`StreamSend`/select `ActorAsk` arm/`Join` branch argument and `receive gen fn` `Yield` value with snapshot mode `Share` (rule 5); `RecordCloneCall` (`clone x`); `RcIntrinsic{Clone}` (`hew_rc_clone`), `RcIntrinsic{WeakClone}` (`hew_weak_clone_rc`) |
| `destroy_value %v` | `%v : Owned`, consumed | — | consumes the obligation; illegal on `Linear` except on an unwind edge (rule 6d) | scope exit of every live `Owned` binding **and every unnamed `Owned` temporary of the block** (§1.3.4: `Block`, `Scope`, early `Return`, `Break`, `Continue`, unwind, cancel); the `Owned` result of a statement-position `HirStmtKind::Expr`, `If`, `Match`, `Block` (§1.3.4); unused `destructure` parts; the `Owned` operand of `Break { value }` when the produced `Place` is discarded (node.rs:2584-2595); the old value of a mem2reg `Assign` to a non-escaping `var`; a loser reply of a `Select` (§1.5, performed by the runtime through registered glue, never a SIR op) |
| `begin_borrow %v` / `end_borrow %b` | `%v : Owned` (not consumed) / `%b : Guaranteed` | `Guaranteed` / — | lexical scope; owner not consumed inside; an `end_borrow` on every exit of the region including `Return`/`Break`/`Continue` inside a borrowed `Match` arm | every `Call`/`ResolvedImplCall`/`CallDynMethod`/`CallTraitMethodStatic`/`VarSelfMethodCall`/`NumericMethod` argument and receiver whose header mode is `Borrow` (§4.2); `FieldAccess` base; `Match`/`IfLet`/`WhileLet`/`LetElse` scrutinee for the tag check, every `HirPayloadPredicate`, every `HirPayloadVariantPredicate` and the arm `guard` (§1.3.2); `Index`/`Slice` base; `Binary`/`Unary`/`IdentityCompare` operands of heap type; `RcIntrinsic{GetCopy, StrongCount, WeakCount, IsUnique, Downgrade, WeakUpgrade}` receiver; `CancellationTokenIsCancelled.receiver` (node.rs:2223, `hew_cancel_token_is_requested` borrows); `GeneratorNext` receiver; `WireCodec{Encode}` operand; `RemoteActorAsk.msg` (wire-encoded, read only); the `Borrow`-mode inputs of a `Suspend` (§1.5); the `StructInit.base` of a functional update whose base stays live; **`begin_borrow %p` on a `Place`** (state field, env field, extern-addressed `var`) for a non-mutating receiver or argument read — the region forbids every `store.*`/`load.take`/`end_lifetime` of `%p`, so the place stays `Init` across a trapping or suspending read call and needs no re-init |
| `move %v` | `%v : Owned`, consumed | `Owned` (forwarding) | one obligation in, one out | `Return`; `Call` argument to a `Consume` header slot (§4.2); `let y = x` when `x` is `AffineResource` or `Linear` (main behaviour, §11 row 3); `push`/`insert` of an element into a collection (§5.3: move-in, never clone-in); `StructInit`/`TupleLiteral`/`MachineVariantCtor`/`EnumVariantCtor` field from a last use; `ActorSend`/`ActorAsk`/`Spawn`/`StreamSend`/select-ask/`Join` argument and `receive gen fn` `Yield` value with snapshot mode `Transfer` (rule 5); `SpawnedCall { bound: true }` argument (`fork t = f(s)`, node.rs:1724 — the checker already moves it, tests/handles.rs:117-127); `ForkBlock.captures` (all `Move`, node.rs:1747-1749) and `Closure`/`SpawnLambdaActor` captures with `ClosureCaptureMode::Move`; `Yield` value in a `gen fn`/`gen {}` body (intra-frame); a `receive gen fn`'s payload parameters into the generator env (§1.3.5: the pump takes the payload, so it owns them); `AwaitTask` and a select `TaskAwait` arm (`move %t` of the `Linear` task handle into the `Suspend`, §1.5); `RcIntrinsic{New}` payload and `RcIntrinsic{Set}` replacement (the runtime destroys the old payload, `hew_rc_set`, rc.rs:345); `CoerceToDynTrait.value` into the box (§5.4); `MachineEmit` payload into the emit queue (§5.8) |
| `fork %v` | `%v : Owned`, consumed; class ∈ {`CowValue`, `AffineResource`, `Linear`} with a heap carrier | `Owned` (unique) | one in, one out; a `fork` on a `BitCopy`, `View` or `PersistentShare` value is `E_SIR_ICE` (nothing to make unique: the first two carry no obligation, the third is a share by definition and mutating through it is not on the surface, §5.4). **The class decides the realization, not the legality** [decision]: for a `CowValue` carrier the fork is `ensure_unique` (a no-op today under §5.5's shortcut, an arc refcount check when it retires); for an `AffineResource`/`Linear` carrier it is a register move — the value is unique *by class* (`let w = v` on a `Vec<Conn>` is a `move`, §11 row 3, so no second live handle exists) and there is nothing to copy. Revision 5's "exists only for `CowValue` … any other class is `E_SIR_ICE`" is **withdrawn**: it left `var v: Vec<Conn>; v.push(c)` — the shape §1.1's F-collections decision was rewritten to preserve, `repros/ladder/vec_resource_drop.hew` → `2`, `close 1`, `close 2` — with no admitted op sequence, since `push` is not a `VarSelfMethodCall` (`hew_vec_push_owned_move(v: *mut HewVec, data)` borrows the collection by pointer and has no dual return, vec.rs:2681) and the `VarSelfMethodCall` move-in/move-back escape below does not reach it | the **written-through operand** of an `Assign` through a projection of a `var` (`v[i] = …` forks the collection `v`; `p.name = …` forks nothing — an inline record is unique by construction and the assignment is `destroy_value` of the old field plus the store, §1.3.5); the receiver of a mutating collection call (`push`, `insert`, `set`, `pop`, `remove`, `clear`, …) and of a `bytes` mutator; the receiver of a `VarSelfMethodCall` (node.rs:2113-2136, `requires_mutable_receiver`, dual-return `(result, Self)`) **when it is `CowValue`** — for an `AffineResource`/`Linear` `VarSelfMethodCall` receiver (`var c: Conn; c.reset()`) the dual return makes the shape explicit and the receiver is `move`d in with the returned `Self` `move`d back into the binding, so no `fork` is emitted there either; `RecordCloneCall` result (`clone x` is `fork` of a `copy_value`, so the copy is unique now). A `string` is never forked: `std/string.hew` declares no `var self`/`consuming self` method (`grep -n 'var self\|mut self\|consuming self' std/string.hew` is empty) and every string operation returns a fresh value |
| `destructure %agg` | `%agg : Owned`, consumed | one `Owned`/`None` per field | every `Owned` part consumed on every path | `Match` on an enum/record/tuple by value (payload binders), `LetElse`, `IfLet`, `WhileLet` when the scrutinee is a last use; a `let` destructuring pattern (`let Two { a, b } = t;`, `let (x, y) = pair;`) whose initializer is a last use — the only admitted way to consume one field of a local aggregate, since Hew has no partial move (`ir-ladder-worked-examples.md` W3, D345) — emitted **after** every predicate of the arm has passed (§1.3.2); a nested `HirPayloadVariantPredicate.bindings` set is a nested `destructure` of the payload part; `StructInit { base: Some(b) }` when `b` is a last use (un-overridden parts `move` into the new record, overridden parts `destroy_value`d); `MachineStep`'s event after the D287 desugar (§1.3.7) |
| `alloc_place T` | — | `Place` | definite-initialization tracked (rule 4) | a `var` whose address is taken by an extern `&`/`&mut` parameter — **an ordinary function-owned place, the same rule as any other escaping `var`; there is no third memory class** [decision, plan §6] (the only way a local escapes SSA — a `BorrowMut` capture does **not** make the outer `var` a place, §1.3.5; and no such producer exists on `main` today, so P1 delivers the op and rule 4's function-owned clause with no producer to exercise them — `sir-domain-matrix.md` D-NOPLACE, which owns the phase); actor state fields (one place per field, owned by the runtime object, §1.3.6); environment fields of a closure, lambda actor, spawn task or generator (owned by the env allocation); coroutine frame slots; the payload record of a dispatched message (§5.6) |
| `load.copy %p` | `Place` (initialized) | `Owned` | retain out; place stays initialized; type has `clone ≠ None` | `BindingRef` of an actor state field (a binding the checker resolved to a state field: `HirActorDecl.state_fields`) in value position when the field is not the receiver of a mutating call; `BindingRef` of a captured binding inside a closure/lambda/generator body (env field read); `HirGenCaptureSource::ActorStateField` snapshot at generator construction; read of an extern-addressed `var` |
| `load.take %p` | `Place` (initialized) | `Owned` | place becomes `Uninit`; a function-owned place is `Uninit` at every exit (taken or `end_lifetime`d); a runtime-owned `CowValue` place must be `Init` again at every exit (unwind and cancel included); a runtime-owned `AffineResource`/`Linear` place may stay `Uninit` — its taken bit records that (§1.3.6, rule 4) | consuming use of an extern-addressed `var`; **a mutating receiver call on a runtime-owned place** (`push` on a state-field `Vec`, a `bytes` mutator on a state field, `VarSelfMethodCall` on a state field): `load.take` → `fork` → call → `store.init` of the result back. This is one sequence for **every** class whose carrier the callee borrows by pointer: `hew_vec_push_owned_move(v: *mut HewVec, …)` (vec.rs:2681) takes the collection by pointer whether the element is a `string` or a `Conn`, so a `Vec<Conn>` state field's `push` takes exactly this path with the `fork` realized as a register move (§1.3 `fork` row). On the unwind and cancel edges of that call the forked value is still live and the edge op is `store.init %p, %forked` — never a fabricated default. Only where the callee **consumes** the receiver — an `AffineResource`/`Linear` `VarSelfMethodCall`, which moves it in and returns `Self` — is there no fork, and the edge then leaves the place `Uninit` with its taken bit set; `hew_drop$State` skips it (§1.3.6). An explicit `close` of a `#[resource]` state field (spec §3.7.8.4 Path 2) is the same `load.take` with no store-back; `GeneratorNext` result take |
| `store.init %p, %v` | `Place` (uninitialized), `%v : Owned` consumed | — | place becomes initialized | first assignment / declaration; `Assign` to a `let` or `var` state field inside `init {}`; the store-back after a place receiver call; the `ActorInit` producer storing a spawn argument or `HirField.default` value into a state field (§1.3.1) |
| `store.assign %p, %v` | `Place` (`Init`, or `Uninit`/`Maybe` when it carries a taken bit), `%v : Owned` consumed | — | old value destroyed, then stored — bit-guarded on a place with a taken bit, so re-initializing a closed resource field is this op and not `store.init` (§1.3.6); rule 6a on a `let`-rooted place | `Assign` to a `var` state field outside `init {}` (§1.3.6); `Assign` to a `BorrowMut`-captured binding inside a closure body (the env field, §1.3.5); `Assign` to an extern-addressed `var`; `self.field = v` in a machine transition after the D287 desugar (§1.3.7) |
| `end_lifetime %p` | `Place` (initialized) | — | destroys the contents; place uninitialized | scope exit of an extern-addressed `var`; actor stop (`hew_drop$State`); env release (`hew_drop$Env` from the arc `drop_fn`, or the generator env thunk) |

`BitCopy` values have no ownership ops; `View` values are `None`-kind and are
only legal inside the borrow scope of their producer's argument (rule 3
applies to the producing call's borrow, nothing more — §2.2).

Non-escaping `var`s never get a place: HIR→SIR construction does mem2reg,
so `var v = a; if c { v = b }` is block arguments (`Assign` → `destroy_value`
old + edge argument). The `Materialize` reasons in §4.2 are the only ways a
value leaves SSA. A `HirStmtKind::Let(binding, None)` (`var x: T;`) has no
SIR op: the checker's definite-initialization rule (`E_OWN_UNINIT`, §1.6)
has already proven every read of `x` is dominated by an assignment on every
path, so mem2reg finds a defining value for every use; if it does not (a
checker hole), construction fails closed with `E_SIR_ICE definite-init`
naming the binding — SIR has no `undef` value and a block argument can never
carry one (rule 4).

#### 1.3.1 Producers of `Owned` values (by composition; listed so the column is grep-checkable)

`HirLiteral::String` / `HirLiteral::Bytes` (node.rs:3338-3352; a static
literal is a non-owning pointer the leaf release ignores,
`hew_string_drop` → `is_managed_cstring`, string.rs:1264), `HirItem::Const`
string references (`hew-mir/src/lower/consts.rs:1332` today), `Binary` string
concatenation results (`"a" + "b"` is a `Fresh` string — rule 1's own
positive example), `Slice` results (node.rs:2008: "a freshly-allocated copy",
a `Fresh` collection of the container's element class), `StructInit` /
`TupleLiteral` / `MachineVariantCtor` / `EnumVariantCtor` results (a
`StructInit { base: Some(_) }` functional update produces one fresh record;
its inputs follow the `copy_value`/`destructure` rows), every `Call`
whose header `RetMode` is `Fresh` (§4.2), `WireCodec{Encode}` (bytes) and
`WireCodec{Decode}` (value) results (node.rs:2254-2258), `RcIntrinsic{New,
Downgrade, WeakUpgrade}` results (`hew_rc_new`, `hew_rc_downgrade`,
`hew_weak_upgrade_rc` mint; `WeakUpgrade` mints `Option<Rc<T>>`;
`RcIntrinsic{New}` passes `hew_drop$T` of the payload as `hew_rc_new`'s
`drop_fn`, rc.rs:103-107 — today `rc_payload_drop_thunk`, llvm.rs:16052 —
without which `hew_rc_drop`/`hew_rc_set` (rc.rs:345) release nothing inside
the payload), `CoerceToDynTrait` result, `MakeGenerator`/`GenBlock` result,
`SpawnLambdaActor` handle, `SpawnedCall { bound: true }` handle (a
`Linear Task<T>`; `bound: false` and `ForkBlock` produce **no value**, §1.5),
`ActorGenStream` (a `Stream`, `AffineResource`), `RegexLiteralRef` (its type is
the std `#[resource]` `Pattern`, `std/text/regex/regex.hew:28-29` — the
`ResolvedTy::Regex` named in node.rs:1518 does not exist, `grep -n Regex
hew-types/src/resolved_ty.rs` is empty) and `Regex` predicate capture strings
(`Fresh` call results minted before the arm body), `ContextReader` results per
their type's class (`ExecutionContextReader::{ActorId, Supervisor,
TraceSpan}`, hew-types/src/check/types.rs:55-58, are all `BitCopy` today:
`None`-kind), `RemoteActorAsk` decoded reply, `Suspend` resume-edge results
(§1.5), `HirField.default` initializers of actor state fields (node.rs:1170;
evaluated in the `ActorInit` producer and `store.init`ed into the field's
place, §6.5), `MachineStateName` after the D287 desugar (a static string
literal, §1.3.7). `RcIntrinsic{GetCopy}` requires `T: Copy` (spec §3.7.5) and
produces a `None`-kind value. `ResolvedRef::Item` (a bare named-fn value,
ids.rs:25) is a `PersistentShare` `{fn, null}` pair: an `Owned` value whose
runtime retain/release are null no-ops (§1.1). `SubsumedValue { source,
producer }` (node.rs:2284; wraps `CopyCloneNoop` rewrites and `Expr::Timeout`
over an `await`, lower.rs:20404-20418 `HirProducedValueProducer::Timeout`) is
the identity of `source`: it has the kind and ops of its source and emits
nothing of its own. `HirExprKind::Unsupported` is an admission reason
(`Admission::Legacy("unsupported-expr")`, §7), never lowered.

**`Spawn` produces no `Owned` value** [decision, plan §6]. A pid is `BitCopy`:
`LocalPid`/`HewActor` are `None`-kind (§1.1, §11 row 15) because a pid's drop
frees nothing — `builtin_type.rs:305-312` states it ("A pid's drop frees
nothing — it is a by-value reference snapshot") and `ty_is_nonowning_pid_leaf`
(`llvm.rs:25479-25488`) is the codegen half. The owned thing at a spawn is the
**state record**, `move`d into the runtime object (§1.3.6), and at a
`SpawnLambdaActor` the **captured environment**, `move`d into the actor. Revision
6 listed the `Spawn` handle among the producers above and that entry is
withdrawn.

`SpawnLambdaActor`'s `LambdaPid` result keeps its own class and stays in the
list. Reading "a pid is `BitCopy`" to cover it would double-release the handle:
`LambdaPid` is a `transfers_ownership_across_actor_boundary` type whose own
comment records why (`builtin_type.rs:314-320`, "`hew_lambda_actor_clone`
allocates a distinct owning wrapper precisely because a plain address copy is
unsafe; two owners of one wrapper release it twice (observed: SIGSEGV)"), it has
a release symbol (`hew_lambda_actor_release`, §5.2 item 6), and `main` consumes
the binding on a send (`repros/ladder/lambda_send_twice.hew` → `use of moved
value \`w\``). Ownership kinds are this document's domain, so the narrow reading
is the one in force; the plan row's wording is the defect to fix there.

#### 1.3.2 Match predicates, nested payload predicates and guards

`HirMatchArmPredicate` (node.rs:2964-3033): `Wildcard`/`EnumVariant`/`Literal`
read the scrutinee inside its borrow; `Binding` binds the whole scrutinee — a
`move` when the scrutinee is a last use, else `copy_value` (composition of the
`BindingRef` rule); `RecordProject`/`TupleProject` are `destructure` on a
last use, else borrowed projections (`copy_value` per bound part); `Regex`
mints capture strings as `Fresh` results before the body.

**Every check precedes every consume.** An arm (and a `WhileLet`, `IfLet`,
`LetElse` head) may carry, in this order, the tag check, literal payload
checks (`HirPayloadPredicate`, node.rs:3034), nested constructor checks
(`HirPayloadVariantPredicate { bindings, nested }`, node.rs:3045-3070:
"branches to the arm's fallthrough target on mismatch. On match, `bindings`
are materialised"), and a `guard`. All of them evaluate inside one borrow of
the scrutinee; a nested predicate reads its payload slot as a `Guaranteed`
projection (today's "unregistered transient alias — never entered into
`owned_locals`"). Only after the last check passes does the arm either
`destructure` the scrutinee (last use: outer bindings are parts, and each
nested `bindings` set is a nested `destructure` of the corresponding payload
part, recursively) or `copy_value` each bound projection (scrutinee stays
live). Fall-through to the next arm or to `else_body` therefore never
re-consumes a part, and a `LetElse`'s escaping bindings (node.rs:1270-1300)
are ordinary `Owned` values of the enclosing block from that point on.

Guards are **shipped**: `HirMatchArm.guard` is lowered from the source guard
(`hew-hir/src/lower.rs:30667-30670` "a guard expression is lowered and
attached to the `HirMatchArm` via the `guard` field. MIR lowering evaluates
the guard after pattern matching succeeds and falls through to the next arm
when the guard is false"); the 18 `guard: None` literals in lower.rs are the
desugar sites (`grep -c 'HirMatchArm {' hew-hir/src/lower.rs` = 19). The one
refusal on `main` is a guarded arm over a nested aggregate payload
destructure (lower.rs:31089, `unsupported(… "guarded match arm with nested
aggregate payload destructure")`); under the rule above it needs no refusal
and the fixture moves reject → accept at P2 (§7 records the move). The old
sentence "guards are `None` at all eight construction sites; [P2] when guards
land" is withdrawn.

#### 1.3.3 `defer`

`HirStmtKind::Defer { body, scope_id }` (node.rs:1315-1331). Deferred bodies
are inlined in LIFO order **before** the scope's `destroy_value`/`end_lifetime`
ops (they read the scope's bindings) on every exit of `scope_id`: normal exit,
`Return`, `Break`, `Continue`, **and the cancel edge** of every `Suspend` inside
the scope. **[current]** `hew-mir/src/lower/scope.rs` emits defers on normal
exit (128), return (1011), break/continue (1043) only; the cancel path is
missing. Spec §4.5 line 3257 ("All `defer` blocks and `Drop` implementations
run during unwinding" on cancellation) wins; P4 lands cancel-edge defers as a
behaviour fix with a vertical-slice fixture whose `defer println` must appear
after a cancelled `await`. Unwind (trap) edges do not run defers [P4 decision, matches `main`]: the
spec promises defers on cancellation (line 3257) and says fallible cleanup
"composes through `defer`" (line 1792) but nowhere promises defer bodies on a
trap; `#[resource]` `close` does run on the trap edge (spec §3.7.8.1 "dispatches
`close` on every scope-exit path including `Trap` and `Cancel`"), so the
unwind edge carries `destroy_value`s and no defer bodies (§11 row 18 names
the ownership.md sentence this overrides). The P5 doc sync
states this in spec §4.5. A use
inside a defer body is a use for rules 1–3 and for §3: a value read by a defer
is live to the exit, so destroy sinking and move-on-last-use never move it.

#### 1.3.4 Temporaries and discarded results

An `Owned` value with no binding — the result of a statement-position
`HirStmtKind::Expr(e)`, an intermediate in a larger expression (`f(g())`,
`open(p).read()`, `"a" + "b"` before its `let`), or the value of an
`If`/`Match`/`Block` used as a statement whose arms produce `Owned` values —
belongs to the innermost enclosing block scope exactly like a `let` binding
of that block: HIR→SIR emits its `destroy_value` at that block's exit (every
exit: normal, `Return`, `Break`, `Continue`, unwind, cancel) unless a
consumer (a `Consume` slot, `move`, `Return`) took it first. A borrowing use
(`peek(mk())`) is a `begin_borrow`/`end_borrow` around the call; the
temporary stays live until the block exit. For an `If`/`Match` statement the
destroy sits in each arm before the join when the arm value is unused, so
nothing crosses the join. This is `main`'s observable order:
`repros/ladder/temp_close.hew` (`println(peek(mk()));
println("after")` with `#[resource] Conn`) prints `9`, `after`, `close 9` —
the resource temporary closes at block exit, not at the end of the full
expression — and spec §3.7.3 "Cleanup runs at a predictable point (scope
exit)" names the same point. For `CowValue`/`PersistentShare` temporaries
the release is unobservable and §3 destroy sinking moves it to the end of
the full expression [P2]; for `AffineResource`/`Linear` temporaries it stays
at block exit (§3). This is the successor of `temp_drop.rs` (§9): one rule,
no allow-set. Test: core-matrix cell pinning `close` after `after` for a
resource temporary; ASan fixture for `open(p).read()` with a trapping `read`.

#### 1.3.5 Environments: closures, lambda actors, spawn tasks, generators

Every capture is a value **copied or moved into an environment the callee
owns**; no environment ever holds a pointer into the constructing frame.
This is `main`'s behaviour and it is what makes `ClosureEscapeKind`
(hew-types/src/check/types.rs:852-865, "`Escapes` unless the classifier can
positively prove `Local` or `Forked`") an optimization input, not an
ownership input: an escaping closure cannot dangle.

| Capture | HIR | SIR op into the env | body access | env release |
| --- | --- | --- | --- | --- |
| `ClosureCaptureMode::Copy` | `HirClosureCapture` (node.rs:3183) | bits | read | none |
| `ClosureCaptureMode::Borrow` | same | `copy_value` (rule 6b: `clone == None` → `E_OWN_CLONE_UNSUPPORTED`) | `load.copy`/`begin_borrow` of the env field | `hew_drop$Env` |
| `ClosureCaptureMode::BorrowMut` | same | `copy_value` (the env owns its **own copy**) | reads as above; a write is `store.assign` on the env field (a runtime-owned place) | `hew_drop$Env` |
| `ClosureCaptureMode::Move`, `HirCaptureKind::Strong` with `move`, `ForkBlock.captures` | same / `HirLambdaCapture` (node.rs:1908) | `move` | as `Borrow` (closure, lambda, generator) or taken by the body (spawn task, below) | `hew_drop$Env` / taken |
| `HirCaptureKind::Weak` | node.rs:3237 | bits (non-owning alias of the actor's own `LambdaPid`) | read | none (today's `LambdaEnvFieldDrop::WeakSelfHandle`, closure_gen.rs:1436) |
| `HirGenCapture` `Local` / `ActorStateField` | node.rs:3204-3230 | `copy_value` (`load.copy` of the state place for `ActorStateField`) | `load.copy`/`begin_borrow` of the env field | generator env thunk (`hew_gen_coro_destroy`, cont.rs:2058-2098, runs the env-drop thunk after the coroutine's cleanup path) |

**`BorrowMut` captures [decision].** `main` captures by value: the write-back
lands in the closure env field (`hew-mir/src/lower/assign.rs:631-640`
"mutations accumulate across calls through the persistent env pointer, and
the caller's original binding is independent"), and
`repros/ladder/borrowmut_capture.hew` (`var n = 0; let bump = || { n = n + 1
}; bump(); bump(); println(f"{n}")`) prints `0`. This design keeps that: the
outer `var` is **not** an escaping place, the env field is. What changes:
`main` restricts the write-back to `BitCopy` scalar fields and fails closed
on an owned captured field (assign.rs: "would leak its prior value on
overwrite"); `store.assign` on the env field releases the old value, so an
owned `BorrowMut` capture (`var s = "a"; let f = || { s = s + "b" }`) becomes
legal (§11 row 17, relaxation). Because closures are `PersistentShare`
(§5.4), `let g = f; f(); g()` mutate the same env field — sequential, inside
one actor, sound. Across actors it would be a race (spec §3.4.8 "No data
races between actors"), so **the send fact of a closure is false when any
capture has mode `BorrowMut`** — a per-closure-expression fact keyed by the
`ClosureInvokeShim` `DefId`, not a per-type one, because capture modes are not
part of `ResolvedTy::Closure` (§6.3) — and rule 6c refuses the `Snapshot`;
today the closure `Send`
rule is mode-agnostic (`traits.rs:1039-1043` "all captured types are
Send") and the only guard is `NonSyncMutCaptureCrossesSuspend`
(expressions.rs:7615-7620, a suspend inside the body); `main` still fails
closed on the sharing shape, but in MIR (`repros/ladder/closure_mut_share.hew` →
`E_MIR … CannotMaterializeClosureCapture`). The checker rule lands at P1 with
the class table; the fixture moves to `reject` with `E_OWN_SEND_UNSUPPORTED`.
`NonSyncMutCaptureCrossesSuspend` stays as the one suspend-crossing rule.

**Spawn task environments are taken, not borrowed.** Spec §3.7.8.5 item 2
(line 3096): "A `@linear` value moved into a child task transfers the
must-consume obligation to the child body … the child must reach a declared
consuming method on every path that does not trap"; a child may also `close`
a captured `#[resource]`. `hew_task_free` releases `env_ptr` unconditionally
when non-null (task_scope.rs:683-686), so a body that consumed a field would
be double-released. [P4]: the `TaskEntryAdapter`/`ForkEntryShim` header slot
for the env is `Consume`; at entry the adapter calls `hew_task_take_env(task)
-> env` (new: returns the env and nulls the task's pointer, so
`hew_task_free` releases the env only for a task that never started),
`load.take`s every field into body-owned values, and frees the shell with
`hew_arc_release_storage(env)` (new: drops the strong count and frees storage
without running `drop_fn`). Its **precondition is `strong == 1`**, and it
aborts otherwise with the same message shape `hew_arc_drop` uses for an
already-zero count (`assert!(old > 0, "Arc double-free: strong refcount was
already 0")`, arc.rs:187-190): `hew_arc_drop` frees only when the strong
decrement returns 1 (arc.rs:199-215), so freeing at `strong > 1` would drop
an allocation another holder still references. A task env has exactly one
holder — the task — by construction, so a count above one is an ICE, not a
race, and the abort is the §5.6 `hew_msg_envelope_take_payload` pattern.
The body then owns each capture like a local:
`Linear` captures are consumed under rule 6d, resources close at the body's
scope exit, and the never-started task is released by the runtime through the
env glue as the arc `drop_fn` — `hew_drop$Env`, or `hew_abandon$Env` when a
capture is `Linear`, since no body code ran to consume it (§5.2 item 7) —
exactly as §1.5 says. A `fork`/`SpawnedCall` env is therefore the one
environment that may hold a `Linear` capture: unlike a closure, lambda actor
or generator env it is entered at most once and taken whole
(`hew_task_take_env`), so the "shared environments have no consume path"
paragraph below does not reach it, and spec §3.7.8.5 item 2's `@linear`
capture into a child task is expressible. Today's env thunk drops
only `OwnsMoved` fields (thunks.rs:693-703, model.rs:7723-7728); the
`BorrowsOnly` kind disappears because every capture is owned by the env.

**A `Borrow` capture of a `clone == None` type is 6b.** `main` lets a closure
alias a resource it does not own (`repros/ladder/closure_borrow_conn.hew`:
`let c = Conn { fd: 4 }; let f = || { c.fd }; println(f())` prints `4`,
`after`, `close 4` — one close, the env field is a `BorrowsOnly` alias). An
aliasing env field dangles as soon as the closure outlives `c`, and
`ClosureEscapeKind` is not an ownership input, so under this design the
capture is `copy_value` → `E_OWN_CLONE_UNSUPPORTED` with the fix-it "capture
with `move`" (`move || { c.fd }` moves the `Conn` into the env, which closes it
when the last handle drops). §11 row 31 (tightening). Inside a `move`
closure the env slot is `Borrow` (§6.5), so `move || { c.close() }` is rule
3's `E_OWN_CONSUME_BORROWED` by composition — a shared env has no consume
path (next paragraph).

**Shared environments have no consume path.** A closure, lambda actor or
generator env is released by glue (`hew_drop$Env` or the generator env
thunk) and its body may run many times or be abandoned, so the body may
only `load.copy`/`begin_borrow` a capture, never `load.take` it; a `Linear`
value cannot be captured by a `Closure`, `SpawnLambdaActor` or
`GenBlock`/`gen fn` — `E_OWN_MUST_CONSUME` at the capture site (rule 6d: the
env would have to `destroy_value` it). A `#[resource]` capture is owned by
the env and closed by its glue when the last handle drops (closure), the
actor stops (lambda), or the generator is destroyed.

**Generators.** `main` clones ordinary `gen fn` parameters and `gen {}`
captures into the env and moves a `receive gen fn`'s mailbox-delivered
parameters (`closure_gen.rs:2054-2200`: "Ordinary `gen fn` parameters and
anonymous-generator captures stay borrowed sources and retain the clone
plan"; `transfers_mailbox_owner`); captures with no total clone are refused
("Opaque/resource/IO handles, trait objects, and closure pairs with a
non-null environment remain rejected because no total clone exists"). Under
this design every `HirGenCapture` is a `copy_value` at
`MakeGenerator`/`GenBlock` (rule 6b refuses `clone == None` exactly where
`main` refuses; closures gain a copy path through `hew_arc_clone`, §11 row
7); the `gen fn`'s own header slots are `Borrow` (§4.2 — the constructor call
returns immediately and the body reads its env copies across suspends), and
the `GeneratorBody` producer's env slot is `Borrow` (§6.5). A `receive gen
fn`'s parameters follow the handler rule (§5.6): the pump takes the payload,
so it owns every parameter and `move`s each into the generator env — `main`'s
`transfers_mailbox_owner` (closure_gen.rs:2185-2200), now the only path rather
than one of two, and the only way a `clone == None` parameter reaches a
generator env. The pump is also the shape that forced the single disposition:
it suspends on every `Yield`, so a borrowed parameter would outlive its
envelope (§5.6).

#### 1.3.6 Runtime-owned places: actor state fields

An actor state field is a `Place` owned by the runtime object
(`Materialize { ExplicitStorage }` per field in the init body). The rules:

- **Read-only use** (`BindingRef` in value position, receiver of a
  non-mutating call, argument to a `Borrow` slot): `begin_borrow %p` or
  `load.copy %p`; the place stays `Init` across a trapping or suspending
  call. `main`'s `ActorStateLoadMode::Borrowed` classifier
  (model.rs:6648-6656) is replaced by the syntactic position (§11 row 12).
- **Mutating call** on a field whose carrier the callee borrows by pointer —
  `items.push(1)` on a `Vec<i64>` (`CowValue`) and `conns.push(c)` on a
  `Vec<Conn>` (`AffineResource`) alike: `load.take` → `fork` → call →
  `store.init`, with `store.init %p, %forked` on the unwind and cancel edges
  (the callee borrowed the collection). The class changes only the `fork`'s
  realization — `ensure_unique` for a `CowValue` carrier, a register move for
  an affine one (§1.3 `fork` row) — never the sequence. This is legal on a
  `let` field: `repros/ladder/let_state_push.hew` (`let items: Vec<i64> =
  Vec.new(); receive fn demo() { items.push(1); … }`) prints `1`, and spec
  §3.4.3 forbids only *assignment* (line 636: "Any assignment to a `let`
  field from a `receive fn`, a plain actor method, or a lifecycle hook is
  rejected at check time"). Rule 6a names state-field places explicitly:
  `store.assign` to a `let` field outside `init {}` → `E_OWN_MUTATE_LET`;
  `store.init` inside `init {}` and the `load.take → fork → store.init`
  sequence are not violations.
- **Consuming use** of an `AffineResource`/`Linear` field — `VarSelfMethodCall`
  through a trait `var self` receiver (main refuses:
  `repros/ladder/state_resource_trait.hew` → `E_MIR … var-self receiver binding
  has no MIR place`; `var self` is trait-only, `repros/ladder/state_resource_mut.hew`
  → "`var self` on an inherent impl method has no effect"), or an explicit
  `conn.close()` in a handler (spec §3.7.8.4 Path 2: "The handler may also
  close them explicitly via `f.close()?` … an already-closed `#[resource]` is
  a use-after-consume diagnostic"): `load.take %p` with no store-back (or a
  `store.init` of the returned `Self` for the dual-return call). There is no
  fabricated default and no "zero variant" — neither is total, and closing a
  fabricated `Conn { fd: 0 }` would be a side effect the spec never promised.
  **[decision] Every runtime-owned `AffineResource`/`Linear` place carries a
  taken bit** in the owner's `TargetLayout` (one byte beside the field, laid
  out by MIR, §4.2): `load.take` sets it (MIR `MarkUninit { place }` writes
  it), a store clears it, and `hew_drop$State` (§5.2) tests it before
  running the field's close. Rule 4's lattice for these places is `{Init,
  Uninit, Maybe}`: `Maybe` at a join (`if c { conn.close() }`) is legal
  because the bit decides at run time; a `load.*`/`begin_borrow`
  of a `Maybe` or `Uninit` place is `E_OWN_USE_AFTER_CONSUME` ("may have been
  closed"); an exit with the place `Uninit`/`Maybe` is legal. `CowValue`
  places keep the exact `{Init, Uninit}` lattice with `Init` required at
  every exit (the value is never consumed by a callee). This is the one
  place drop-flag state survives, typed and read by exactly two runtime
  consumers — `hew_drop$State` (§5.2 item 1) and the bit-guarded `store.assign`
  sequence below, which also writes it. Every other use of the lattice is a
  compile-time refusal in rule 4, not a run-time read. §11 row 35 records the
  user-visible half. Plan §1.1's "a definite-initialization dataflow over these
  places replaces drop flags entirely" holds for every function-owned place and
  every `CowValue` place; a runtime-owned affine place is the one exception, and
  naming it here is this document deciding an ownership representation in its own
  domain (plan §5.1), not a table row overriding the plan. SHORTCUT-free: the bit is the representation of
  the spec's "already-closed field", not an approximation.
- **Re-initialization is `store.assign`, total over the lattice [decision].**
  `store.assign %p, %v` on a place that carries a taken bit is legal in **all
  three** states and lowers to one bit-guarded sequence — `if !taken { taken
  := 1; Release }; Store; taken := 0` — the same conditional `hew_drop$State`
  already runs (§5.2 item 1), so this introduces no new mechanism and rule 4
  keeps a three-state lattice with no fourth. **The bit is set before the
  release, not after the store** [decision]: a `#[resource]` field's release is
  a user `close` that can trap, and on a `StructuredLlvm` target the trap edge
  is a landingpad that runs the SIR unwind block and then leaves
  `hew_drop$State` to walk the record at actor teardown (actor.rs:2599-2606).
  With the bit set first, a trap mid-`close` leaves the field `taken` and the
  teardown drop skips it — exactly one attempted close. With the bit set last
  the field would hold a released pointer at `taken == 0` and the teardown drop
  would close it again; nothing else catches that, because the Linux panic path
  passes `drop_state = false` (`scheduler.rs:3625`) and `consume_state =
  drop_state || state_mutation_began` (cont.rs:1355-1357) is then false, so the
  escrow is discarded and the live wrapper is dropped normally. The rule is
  total: a `CowValue` place carries no bit and needs none — its release runs
  only runtime leaf symbols and glue reached through `hew_arc_drop`, all
  declared plain `extern "C"` (arc.rs:102-106, 183) against `hew_panic`'s
  `extern "C-unwind"` (actor.rs:7142), so a trap inside one aborts the process
  rather than unwinding into a landingpad, and the `Release`-to-`Store` window
  cannot be observed. Without the rule `var conn:
  Conn` has **no admitted op** for `conn.close(); conn = Conn.open(2)`:
  `store.assign` would require `Init`, `store.init` is reserved for `init {}`
  / a first assignment / the store-back after a place-receiver call, and rule
  4 refuses a store on `Uninit`/`Maybe`. `main` accepts that program and
  **double-closes**: probe `repros/ladder/state_reinit.hew` (`actor Holder { var
  conn: Conn = Conn.open(1); receive fn cycle() { conn.close(); conn =
  Conn.open(2); return conn.fd } }`) prints `close 1`, `close 1`, `2`,
  `after`, `close 2` — the explicit close runs and the assignment's implicit
  release runs again over the same value. The bit-guarded store prints `close
  1` once. §11 row 35 (behaviour fix). 6a is unaffected: a `let` field
  outside `init {}` is still `E_OWN_MUTATE_LET`.
- **Enum-shaped resource fields carry the bit too.** The sentence
  "`Option<Conn>` and enum fields: `store.assign` — the old value is destroyed
  by glue, no take, no bit" is **deleted**: it contradicted the decision above
  it. `Option<Conn>` is `AffineResource` by the §1.1 aggregate rule and `match
  conn { .Some(c) => c.close() }` is a `load.take` of that place, so exempting
  it would leave the take unrecorded and `hew_drop$State` would close a
  moved-out payload. One rule for every `AffineResource`/`Linear` place,
  enum-shaped or not: a take sets the bit, a store is the bit-guarded
  release-and-store above (a `None` payload releases nothing, because the
  enum's glue switches on the discriminant, §5.2 item 2).
- **`#[linear]` fields are rejected at actor declaration** [P4,
  `E_OWN_LINEAR_STATE_FIELD`]: spec §3.7.8.4 Path 3 (line 1720-1728) admits a
  `#[linear]` field only when the type also satisfies `#[resource]`
  semantics, and `ResourceMarker` cannot express both (§1.1), so every bare
  `#[linear]` field is "a compile error at actor-declaration time". `main`
  accepts it silently (`repros/ladder/linear_actor_field.hew` prints `1`); §11
  row 21 (tightening). Consequently `hew_drop$State` never meets a `Linear`
  field and `emit_drop_glue`'s `Linear` arm is `E_MIR_ICE` (§5.2), and Path
  2's "every reachable exit path of the terminating handler consume each
  `#[linear]` field" has nothing to check until a future edition admits such
  fields (HEW-FUTURE §1.7).

#### 1.3.7 Machines

Every machine construct is `Admission::Legacy("machine")` until the D287
desugar lands [P4]: `HirItem::Machine` (states with `entry`/`exit` blocks,
`HirMachineTransition { guard, … }`, node.rs:767-830), `MachineStep` (2312),
`MachineStateName` (2323), `MachineTakeEmits` (2338), `MachineVariantCtor`
(2380), `MachineFieldAccess` (2401), `MachineEventFieldAccess` (2409),
`MachineEmit` (2300). The P0 totality gate accepts this row because the
reason is a closed enum value and the coverage tool prints it per function.
After the desugar a machine is an enum plus a synthesized step function in
HIR and the constructs map by composition: `MachineStep` → `load.take`/mem2reg
read of the `var` receiver, `destructure` of the event (moved in), a `match`
whose arms are the transitions (guards under §1.3.2, entry/exit blocks
inlined), and a store-back into the receiver; `MachineStateName` → a static
string literal (§1.3.1); `MachineFieldAccess`/`MachineEventFieldAccess` →
`FieldAccess` (`copy_value` inside the borrow of `self`/the event);
`self.field = v` in a transition → `store.assign` through the machine value
(a `var`); `MachineEmit`/`MachineTakeEmits` → §5.8; `MachineVariantCtor` →
`EnumVariantCtor`. The sentence "`MachineState` stays `Linear` so a step that
forgets to store back is 6d" is **withdrawn**: `BuiltinType::MachineState` is
the reserved name, not a user machine's type (§1.1 correction), so a machine
value is an ordinary enum under the aggregate rule and 6d never applies to it.
A forgotten store-back is not an ownership error at all — under mem2reg the
step's returned `Self` is an `Owned` value with no consumer, which rule 1
catches as `E_SIR_ICE linearity` in the desugar, not as a user diagnostic.
Shipped machine locals stay accepted unchanged
(`examples/machine/run_lifecycle.hew:5`,
`examples/playground/machines/traffic_light.hew:24`, both `.expected`-verified
and never consumed); §11 row 34 records the withdrawal.

#### 1.3.8 Closed list: constructs with no ownership op

So the closed-list table test (§10 P0) catches a future non-`BitCopy` result,
these variants are named as producing `None`-kind values or no value:
`HirExprKind::{NumericCast, SaturatingWidthCast, TryWidthCast}` (node.rs:1551-1580;
numeric in, numeric out), `ActorSelf` (1674; a pid, `BitCopy`), `While`,
`ForRange`, `Loop` (2422/2438/2617; loop headers are joins per §1.4, the
expression value is `Unit`), `IdentityCompare` (`Bool`, operands borrowed),
`ContextReader` (all three readers `BitCopy`), `CancellationTokenIsCancelled`
(`Bool`, receiver borrowed), `ClosureCaptureMode::Copy` (bits),
`HirCaptureKind::Weak` (bits), `HirLifecycleHookKind::{Start, Stop, Upgrade}`
(node.rs:633-660: no payload; `Crash`/`Exit`/`Down` payloads in §5.6), the
`HirActorStateGuard::Exclusive` marker (dispatch policy, not ownership),
`HirStmtKind::Let(_, None)` (no op, see the mem2reg note above), `Break`/
`Continue`/`Return(None)` (edges), `HirLiteral::{Int, Float, Bool, Char,
Unit, Duration}` and the `Never`-typed `Unreachable`. `Yield { value: None }`
is deliberately **not** here: it is a `Suspend` with no inputs, §1.5. Test:
`hew-sir` table test matching on every `HirExprKind`/`HirStmtKind`/`HirItem`
variant with no wildcard — and, for the variants carrying an `Option` payload
(`Yield.value`, `Return.value`, `Break.value`,
`MachineVariantCtor.payload`), on both inhabitants — each arm naming its §1.3
row, its §1.5 row, or this list.

### 1.4 Block arguments at joins and loop headers

- Every `Owned` value that is live at a join arrives as a block argument of
  the join block; nothing else survives a join. A value consumed in one arm
  and live in the other is `destroy_value`d in the live arm before the edge.
- Loop headers are joins: the initial edge and every back edge pass the same
  `Owned` arguments; a value consumed in the body must be re-produced (a fresh
  `Owned`) before the back edge or destroyed before `Break`.
- Edge argument kinds must match block-argument kinds exactly (`Owned` to
  `Owned`, `None` to `None`). `Guaranteed` never crosses an edge as a block
  argument. It may nonetheless be **live across a `Suspend`** — a `Borrow`
  header slot is a whole-body borrow (rule 3) and a suspending body does not
  end it — but only when the *owner* is held by a frame that stays parked for
  the whole suspension: a caller-owned argument (the caller is parked on the
  call and holds the owner in its own frame), or a runtime-owned place the
  runtime keeps alive across the park (an actor state field, an env field). A
  `Guaranteed` value whose owner is a runtime buffer released at dispatch
  return is `E_SIR_ICE borrow-scope`. The language has exactly one such owner —
  the message envelope, released by `hew_msg_node_free`
  (`hew-runtime/src/scheduler.rs:3766`) before the park at 3780-3788 — which is
  why every dispatched payload is taken and never borrowed (§5.6). A borrow
  region is one of: an instruction range inside one block;
  a structured region (`Match`/`IfLet`/`WhileLet` arms over a borrowed
  scrutinee) with an `end_borrow` on every exit of the region, early exits
  included (so rule 3's post-domination holds); or the input region of a
  single `Suspend` terminator (§1.5).

There is no `Join`, `EdgeCarry`, `Rearm`, `Reset` event because SSA already is
that (compare `OwnershipEvent` at `hew-mir/src/model.rs:4903`, 16 variants;
§9).

### 1.5 `Suspend`

```
Suspend {
    kind:    SuspendKind,          // semantic: closed set, table below
    inputs:  Vec<Operand>,         // mode ∈ { Borrow, Move }
    resumes: Vec<Edge>,            // one per outcome (await: 1; select: n; timeout: 2; join: 1)
    cancel:  Edge,                 // always present; its first op is the kind's abandon op
}
```

**Inputs.** A `Move` input is consumed into the runtime (a moved send
argument, the `Linear` task handle of `AwaitTask` or of a select `TaskAwait`
arm, a `StreamSend` value with mode `Transfer`). A `Borrow` input (`conn` of
`Read`, `listener` of `Accept`, `rx` of `ChannelRecv`, `stream` of
`StreamNext`, `sink` of `StreamSend`, `actor` of `Ask`/`ActorSend`/`RemoteAsk`,
`sup` of `RestartWait`, `callee` of `CallClosure`) is borrowed **for the
duration of the runtime operation only**: the borrow region is the terminator
itself and ends at it. The owner is a block argument on every resume edge and
on the cancel edge, exactly like any other `Owned` value live across the
suspension (MIR puts it in the coroutine frame, `Materialize { CoroutineFrame
}`). This is sound because nothing in this frame runs between the park and
the resume, so the owner cannot be consumed while the runtime holds the
handle. The obligation this places on **the cancel block** is ordering: on
`main` the withdrawal is emitted code, not a runtime step —
`hew_cont_destroy` "resumes the coroutine at its `coro.suspend` cleanup edge,
running the coroutine's OWN cleanup funclet … in order: (a) the suspend
kind's per-park bookkeeping (slot cancel/free, observer deregister, deadline
cancel), then (b) the drop of every frame-owned Hew heap value live across
the park" (cont.rs:1948-1955), and `hew_task_detach_await` is passive
(task_scope.rs:1009-1030, `let _ = (scope, task)`; called only from the
emitted abandon block, suspend.rs:3337-3362). So the SIR contract is: **the
cancel block's first op is the row's abandon op** (table below), before any
`destroy_value` of a borrowed owner; the verifier checks this structurally
[P4] (`E_SIR_ICE cancel-order`). A cancel block carrying only
`destroy_value`s would free `conn`/`rx` while the reactor slot still
references it. Rule 3 is unchanged in shape; §1.4 names the `Suspend` region
as the third legal borrow region.

**Outputs.** Each resume edge mints its result as a fresh `Owned` (or `None`)
block argument of the target: the awaited `T`, the decoded reply, the received
element, the accepted connection. A `Select` with `n` arms has `n` resume
edges. A loser's reply is never a SIR value: the runtime releases it through
the drop glue registered on that arm's reply channel (`hew_drop$<Reply>` —
today `thunks.rs:433 ask_reply_drop_thunk_ptr`) — on the winning edge if it
has already arrived, otherwise later, when the late replier observes the
cancelled channel and runs the registered destructor itself
(`release_sender_ref_if_cancelled`, reply_channel.rs:357-368;
`hew_reply_channel_cancel` doc, reply_channel.rs:948 "Late repliers observe
the cancelled flag and free the channel themselves"). A deadline-cancelled
`ActorAsk { deadline_ns }` reply is released the same way. `Join` has one
resume edge carrying the tuple of replies.

**Withdrawal is per-arm, not per-cancel [decision].** The abandon op of an
arm withdraws that arm's runtime registration; the cancel edge is only one of
the paths on which an arm loses. For a multi-arm `Suspend` (`Select`,
`Timeout`, `Join`) **every resume edge begins with the abandon op of every
*other* arm, in arm order, before the winning arm's result is read**; the
cancel edge begins with the abandon op of *every* arm, in arm order. A
single-arm `Suspend` has no losers, so only its cancel edge carries one. This
is `main`'s emitted shape, not an addition:
`emit_select_winner_dispatch` (`hew-codegen-rs/src/suspend.rs:9699-9820`)
allocates one winner block per arm — "Each winner block handles its own loser
cleanup, reply read, and branch to the MIR-allocated body block"
(suspend.rs:9806) — and interns `hew_reply_channel_cancel`,
`hew_reply_channel_free`, `hew_stream_cancel_pending_read`,
`hew_channel_cancel_pending_read` and `hew_task_completion_unobserve`
(suspend.rs:9723-9759) for exactly that block. The registrations point into
the caller's `select_channels` / `select_pending_read_ids` allocas
(suspend.rs:9520-9532), so a resume edge that emitted no withdrawal would
leak every loser reply channel and leave a stream/channel pending-read
registration armed against a frame the winner is about to leave — a
use-after-free, not a leak. The verifier checks the ordering structurally
[P4] (`E_SIR_ICE resume-order`, the twin of `cancel-order`); §4.5 Structural
carries the MIR-side rule and `hew-mir/tests/balance_verifier/` (P1, §1.6) gains
a negative fixture per shape (resume edge missing a loser abandon; abandon out
of arm order).

Every `Owned` value live across the suspension is an argument on **every**
resume edge and on the cancel edge; the cancel target must consume them like
any other path (rule 1).

**Who runs the cancel edge.** The cancel edge is code in this function; the
executor depends on how the body is hosted [P4]:

| Body host | Cancel executor | [current] |
| --- | --- | --- |
| coroutine-hosted (actor handler awaiting, `gen` body, `receive gen fn`) | `hew_cont_destroy` under the FG1/FG2 single-owner CAS (`hew-runtime/src/coro_exec.rs:13-25`) runs `coro.destroy`, whose cleanup path is the SIR cancel block; generators via `hew_gen_coro_destroy` (cont.rs:2058) | `Terminator::Suspend { cleanup }` (task.rs:1448-1453) plus `ExitPath::Suspend` plans consumed at `suspend.rs:312-321` |
| thread-hosted spawn body (`fork t = f(args)` / `fork {}` today run on an OS thread, `thunks.rs:686-760`) that has started | the body itself takes its cancel edge at its next safepoint after `hew_task_scope_cancel` (task_scope.rs:2067-2085) marks the scope; a thread body has no `Suspend`, so its "cancel edge" is the safepoint's cancelled branch | same |
| spawn body that never started (`Ready` at cancel) | no body code runs; the runtime releases the environment through the env glue as the arc `drop_fn` (`hew_drop$Env`, or `hew_abandon$Env` when a capture is `Linear`, §5.2 item 7, §5.4) at `hew_task_free`; a started body has already taken the env (§1.3.5, `hew_task_take_env`) | `hew_task_free → hew_rc_drop(env)` (task_scope.rs:683-686) |

`hew_task_set_cancel_cleanup_fn` (task_scope.rs:729) has no registrant in
`hew-codegen-rs` (`grep -rn hew_task_set_cancel_cleanup_fn hew-codegen-rs/src`
is empty); the ladder does not use it and it is deleted with its tests [P5].

**`Task<T>` is not a value the function releases.** The scope owns every task
and frees it at scope destroy (`free_scope_tasks`, task_scope.rs:619-627,
from `hew_task_scope_destroy`). Only a **bound** spawn mints a SIR value:
`SpawnedCall { bound: true }` (`fork t = f()`) produces a `Linear Task<T>`
handle whose one legal consumer is `AwaitTask` or a select `TaskAwait` arm
(`move %t` into the `Suspend`). `SpawnedCall { bound: false }` (a bare
`work()` statement inside `scope {}`, node.rs:1732-1735 "the task handle is
immediately discarded"; lowered today through `direct_no_arg_unit_callee`,
task.rs:558-575) and every `ForkBlock` (node.rs:1744, anonymous) are the
statement-shaped op `spawn_detached` with **no result**: the scope is the
owner from the start and rule 6d has nothing to demand — this is `main`'s
behaviour, whose `MustConsume` only ever iterates `linear_bindings`
(dataflow.rs:1493-1495) and never sees an unbound task place, and it is
fixture-covered (`grep -rlE '^\s*fork \{' tests/vertical-slice/accept` → 5,
e.g. `w2006_scope_spawn.hew`, `fork_block_args_spawn.hew`). The unit-return
gate on an unbound non-unit callee stays a checker wall. The result of an
awaited task is **copied out** of the task's result buffer by `result_size`
bytes on the resume edge and the task marked consumed
(`hew_task_take_result`, task_scope.rs:776-786, only flips
`result_consumed`; `hew_task_free`, 666-690, frees `t.result`
unconditionally), so adopting the pointer would be a use-after-free. A
written-but-unconsumed result (the scope cancelled between completion and
the await, or a losing select arm's task) is released by `hew_task_free`
through the registered `result_drop_fn` (task_scope.rs:671-676), so the
`TaskEntryAdapter` **must** register `hew_task_set_result_drop_fn(task,
hew_drop$T)` for every owning `T` (task_scope.rs:1322; codegen does so today
at thunks.rs:96-104) — the ASan fixture "cancel after completion, before
await" pins it [P4]. `hew_task_free` is not a leaf release symbol (§5.2).

Mapping to the current Raw MIR suspension carriers (`hew-mir/src/model.rs:2772-2890`,
thirteen `SuspendKind` variants, plus the select/join/deadline/yield
terminators). The abandon column is the op the cancel block must begin with;
rows marked "P4" take their symbol from the existing emitter in
`hew-codegen-rs/src/suspend.rs` when the row lands:

| SIR `SuspendKind` | inputs (mode) | result | abandon op (first op of the cancel block; and of every resume edge on which this arm lost) | [current] Raw carrier | phase |
| --- | --- | --- | --- | --- | --- |
| `Await` (`AwaitTask`) | task (`Move`), scope (`Borrow`) | `Owned T` copied out of the result buffer | `hew_task_detach_await(scope, task, slot)` (task_scope.rs:1009) | `SuspendKind::TaskAwait`, `Terminator::Suspend` | P4 |
| `RestartWait` (`AwaitRestart`) | supervisor (`Borrow`) | `ChildRef` (`BitCopy`) | observer deregister (P4) | `SuspendKind::RestartWait` | P4 |
| `ActorSend { block }` | actor (`Borrow`), args (`Snapshot`, rule 5) | `Result<(), SendError>` (`BitCopy`) | mailbox park withdraw (P4) | `SuspendKind::ActorSend` | P4 |
| `Ask` | actor (`Borrow`), args (`Snapshot`) | `Owned Reply` / `AskError` | `hew_reply_channel_cancel` (reply_channel.rs:948) | `SuspendKind::Ask` | P4 |
| `RemoteAsk` (`RemoteActorAsk`, node.rs:1658) | actor (`Borrow`), msg (`Borrow`, wire-encoded by the runtime), timeout (`None`) | `Owned Reply` decoded (`Fresh`) / `AskError` | reply-table cancel (P4) | `SuspendKind::RemoteAsk` | P4 |
| `Read` / `Accept` (HIR `ConnAwaitRead`, node.rs:1800; `ListenerAwaitAccept`, node.rs:1819) | conn / listener (`Borrow`), deadline (`None`) | `Owned bytes \| string` / `Owned Conn`; with a deadline, `Result<_, NetError>` and **two** resume edges | read-slot cancel (P4) on the deadline edge; timer cancel (P4) on the value edge | `Read` / `Accept`, both carrying `deadline_result_dest: Option<Place>` (model.rs:2800-2813) | P4 |
| `ChannelRecv` / `StreamNext` (HIR `ChannelRecvAwait`, node.rs:1838; `StreamRecvAwait`, node.rs:1856) | rx / stream (`Borrow`), deadline (`None`) | `Option<T>` `Owned`; with a deadline, `Result<Option<T>, TimeoutError>` and **two** resume edges | waiter deregister (P4) on the deadline edge; timer cancel (P4) on the value edge | `ChannelRecv` / `StreamNext`, both carrying `deadline_result_dest` (model.rs:2828-2840) | P4 |
| `StreamSend` | sink (`Borrow`), value (`Snapshot`: streams cross actor boundaries, same rule as a send) | `Result<(), CloseError>` | waiter deregister (P4) | `StreamSend` | P4 |
| `CallClosure` (`await closure(args)`) | callee (`Borrow`), args per the closure's header modes | per `ret_ty` | none (the callee's own cancel edge runs) | `CallClosure` | P4 |
| `Select` / `Timeout` (`HirSelect`, arms node.rs:3283-3316) | per arm: `StreamNext`/`ChannelRecv` source (`Borrow`), `ActorAsk` actor (`Borrow`) + args (`Snapshot`), `TaskAwait` task (**`Move`**), `AfterTimer` duration (`None`) | one resume edge per arm; loser replies released by the runtime | per arm: the arm kind's abandon op, in arm order — on the cancel edge for every arm, and on each resume edge for every arm but the winner (`emit_select_winner_dispatch`, suspend.rs:9699-9820) | `SuspendingSelect`, `AfterTimer` | P4 |
| `Join` (`HirJoin`, node.rs:3330-3336) | per branch: actor (`Borrow`), args (`Snapshot`) | tuple of `Owned` replies | `hew_reply_channel_cancel` per branch | `Terminator::Join` | P4 |
| `ScopeDeadline` | duration (`None`) | — | deadline cancel (P4) | `SuspendingScopeDeadline` | P4 |
| `Yield` (`gen fn`, `gen {}`) | value (`Move`) **when present**; none for a bare `yield;` | resume value | none (the companion owns the pending value, cont.rs:2094-2098) | `Terminator::Yield`, `MakeGenerator` | P3 |
| `Yield` (`receive gen fn`) | value (`Snapshot`, rule 5 — the `StreamProducerPump` sends it across the actor boundary on the stream sink, node.rs:1649-1657, expr.rs:6806-6842) **when present**; none for a bare `yield;` | — | waiter deregister (P4) | `Terminator::Yield` under the pump | P4 |
| `Sleep` / `SleepUntil` | duration / instant (`None`) | — | timer cancel (P4) | `SuspendingSleep` / `SleepUntil` | P4 |

**A bare `yield;` has no operand.** `HirExprKind::Yield { value:
Option<Box<HirExpr>>, yield_ty }` (`node.rs:1960-1963`) is reached with
`value: None` — `hew-hir/src/lower.rs` matches it at 7167 and 33176 and lists
it among the "Leaf / no-sub-expression variants … Nothing to recurse into" at
35603-35605 — for a generator whose Yield type is `Unit`. Both rows above
apply with an empty `inputs` vector: `Unit` is `BitCopy`, so there is nothing
to `Move` or `Snapshot` and rule 5 has no operand to decide. The `Suspend`
itself, its resume edge and its cancel edge are unchanged, which is why the
construct is **not** in §1.3.8's no-op list — a bare `yield` is still a
suspension. The §1.3.8 table test names this row for the `Yield` arm so the
`None` case cannot fall through to a wildcard.

**The deadline form is a second arm, not a second kind [decision].** Four
`HirExprKind` variants spell `await <op> | after d`, and all four are distinct
tags in the closed `HirProducedValueProducer` enum (node.rs:1402-1405, match
arms 2662-2665, source-anchor construction 2850-2880), so §1.3.8's table test
sees four arms and each must name its row above. `ConnAwaitRead`
(node.rs:1800) and `ListenerAwaitAccept` (node.rs:1819) carry `deadline_ns:
Option<i64>` and are produced with and without one; `ChannelRecvAwait`
(node.rs:1838) and `StreamRecvAwait` (node.rs:1856) are produced only by
`lower_await_deadline`, so their `deadline_ns` is always `Some` and their
`HirExpr::ty` is always `Result<Option<T>, TimeoutError>`. None of the four
gets its own `SuspendKind`: `SuspendKind::{Read, Accept, ChannelRecv,
StreamNext}` already carry `deadline_result_dest: Option<Place>`
(model.rs:2800-2840), so a deadline is a second **arm** of the same
suspension — `inputs` gains the duration (`None`-kind), `resumes` gains an
edge, and the per-arm withdrawal rule above applies unchanged: the value
resume edge begins with the timer cancel, the deadline resume edge with the
source's own abandon op (read-slot cancel / waiter deregister), and the cancel
edge with both, in arm order. `RestartWait.deadline_result_dest` is documented
RESERVED for the future bounded `await_restart … within:` form and is `None`
on every construction today (model.rs:2860-2866), so that row keeps one resume
edge until it ships.

**Select `TaskAwait` arm [decision].** Spec line 3548 places the task-await
arm outside edition 2026's sealed select set ("`Task<T>` is unnameable and
`fork` is parser-only … return with their substrate"); `main` lowers it with a
plain read of the task (task.rs:1687-1707, no consume; `mark_binding_moved`
at 1914 is the direct-escape arm body) and no accept fixture uses it (`grep
-rl 'from await' tests/vertical-slice/accept` is empty). The mode is `Move`,
the same as `AwaitTask`: the handle is consumed on every edge, a later `await
t` is `E_OWN_USE_AFTER_CONSUME` (no double `hew_task_take_result`), and a
losing arm's task keeps running under its scope, its result released by
`hew_task_free` through `result_drop_fn`. §11 row 22.

The generic `Terminator::Suspend { resume, cleanup, is_final }` (model.rs:4213)
is today produced only by the task-await carrier (task.rs:1448); it becomes the
one MIR suspension terminator all rows lower to [P4], with the `SuspendKind`
payload moved onto it (the side table is deleted, §9).

### 1.6 Walls versus internal errors

**[current]** The user-facing rejection surface on `main` is wider than the
three walls of `docs/v05/ownership.md`. Probes (binary `hew
0.6.0-rc3-dev.141+fa2986bb2`, one commit behind 54e8dde2c; files in
`repros/ladder/`):

| Program shape | today's rejection | authority |
| --- | --- | --- |
| `p.x = 10`, `v[0] = 5`, `s = "b"` on a `let` | checker `cannot assign to immutable variable` | checker (`MutabilityError`) |
| `let h = g; g.next()` on a `Generator`; `let d = c; c.fd` on `#[resource]`; `let g = f; f(1)` on a closure; `let w = v; v.len()` on a `Vec` or a `Vec<Conn>` | `E_MIR_CHECK … used after it was consumed` (`UseAfterConsume`) | MIR (`hew-cli/src/diagnostic.rs:118-129`) |
| `var s: string; if c { s = "a" }; println(s)` | `E_MIR_CHECK … may be read before it is initialized` (`InitialisedBeforeUse`) | MIR |
| `fork t = work(); }` with `t` never awaited | `E_MIR_CHECK: linear binding … must be consumed` (`MustConsume`) | MIR |
| `fork t = work(s); println(s)`; `c.close(); c.fd`; `h.take(c); c.fd` on `#[resource]`; `r.go(w); r.go(w)` on a `LambdaPid`; `println(v.len())` after `actor move \|x\| { v.len() }` | checker `use of moved value` (`UseAfterMove`) | checker (`hew-types/src/check/expressions.rs:1238`, `methods.rs:7879/10455`, `tests/handles.rs:117-127`; `repros/ladder/{lambda_send_twice,cap_move}.hew`) |
| `clone b` on `bytes` | checker `no method clone on bytes` | checker |
| `worker.take(rc)` with `rc: Rc<i64>`; non-Send capture into a lambda actor | checker `not Send` (`expressions.rs:1435, 2650`) | checker |
| `actor \|x\| { f() }` with `f` a `BorrowMut`-capturing closure | `E_MIR … CannotMaterializeClosureCapture` | MIR (`repros/ladder/closure_mut_share.hew`) |
| `conn.bump()` with `bump(var self)` on a `#[resource]` state field | `E_MIR … var-self receiver binding has no MIR place` | MIR (`repros/ladder/state_resource_trait.hew`) |
| `fn shutdown(c: Conn) { c.close(); }` (no `consume`) | accepted; the parameter's disposition is inferred CONSUME from the body | MIR `facts.rs:994-1075` (`repros/ladder/res_param_consume.hew` → `close 3`, `after`) |
| `actor Holder { var tx: Tx = … }` with `#[linear] Tx` never consumed | accepted, exit 0 | none (`repros/ladder/linear_actor_field.hew`) |

[P1 unless marked] The user-facing surface is stated as three families, all
owned by the checker first and re-proved by the SIR verifier:

| Family | Wall | SIR rule | code |
| --- | --- | --- | --- |
| COW value walls (ownership.md) | assign to a `let` (reassignment, `v.field =`, `v[i] =`, a `let` state field outside `init {}`) | 6a | `E_OWN_MUTATE_LET` |
| | `clone` a type with `clone == None` (incl. a capture or generator snapshot of such a type) | 6b | `E_OWN_CLONE_UNSUPPORTED` |
| | send / capture-into-spawn / `receive gen fn` yield of a type whose send fact is false (incl. a `BorrowMut`-capturing closure), **and a `CoerceToDynTrait` into a `dyn … + Send` whose concrete is not `Send`** (§1.1, §11 row 37) | 6c | `E_OWN_SEND_UNSUPPORTED` |
| move-checker family (spec §3.7.8, §3.7.8.1 item 4, §3.7.8.2) | use of any `Owned` binding after its consuming use — an `AffineResource`/`Linear`/`PersistentShare` rebind (`let h = g`), a `consume` argument, `consuming self`, `close`, a spawned-call argument, a `#[resource]`/`LambdaPid` send, a select `TaskAwait` arm, **or an explicit `move` of a `CowValue`** (`actor move \|x\| { v.len() }`, `repros/ladder/cap_move.hew`); a read of an `AffineResource` state field that may have been closed (§1.3.6) | 2, 4 | `E_OWN_USE_AFTER_CONSUME` |
| | a `Linear` value live at a normal exit, or at a cancel exit with no `defer` consumer (`Task<T>` exempt on cancel, §2.1 6d); a `Linear` value captured into a shared env (§1.3.5) | 6d | `E_OWN_MUST_CONSUME` |
| | a consuming use (`Consume` slot, `close`, `consuming self`) of a `Borrow` parameter — `fn shutdown(c: Conn) { c.close() }` without `consume` (§4.2; §11 row 19, tightening) | 3 | `E_OWN_CONSUME_BORROWED` |
| | a `#[linear]` actor state field (§1.3.6; spec §3.7.8.4 Path 3) [P4] | declaration | `E_OWN_LINEAR_STATE_FIELD` |
| definite initialization | read of a declared-uninitialized `var` on some path | 4 | `E_OWN_UNINIT` |

`E_OWN_USE_AFTER_CONSUME` unifies today's checker `UseAfterMove`/`UseAfterConsume`
kinds (`hew-types/src/error.rs:744-746`) and the MIR `UseAfterConsume`;
`E_OWN_UNINIT` is a new checker rule [P1] (`grep -rn 'uninitiali[sz]ed\|definite'
hew-types/src/check/` is empty today); `E_OWN_MUST_CONSUME` moves the MIR
`MustConsume` check into the checker for `Task` and `#[linear]`;
`E_OWN_CONSUME_BORROWED` replaces the body-inferred parameter disposition
(§4.2) and lands with a `consume` sweep of `std/` (§4.2, P3 gate). The
verifier's rules **2, 3 (parameter case), 4 and 6d are user-facing** whenever
the offending op's provenance (§8, SIR carries binding name and span) is a
source binding: they render with the code above, never as an ICE. Rules **1,
3 (lowering temps) and 5** are internal (`E_SIR_ICE <rule> <function>
<value> <path>`): HIR→SIR lowering or an optimization emitted a malformed
program. The old claim that rules 1–5 "never" name a user error is withdrawn: a rule-2
violation whose op has a source binding renders as
`E_OWN_USE_AFTER_CONSUME`, not as an ICE, whether the checker reached it first
or not.

**How that is proved is not `--no-typecheck`** [correction]. Revision 5 built
the gate on running the reject corpus "with the checker's diagnostic
suppressed through `--no-typecheck`", which this compiler cannot do and this
design's own §6.3 forbids. `hew_compile`'s
`typecheck_program_with_diagnostics` returns `TypeCheckResult { tco: None, .. }`
under the flag (`hew-compile/src/lib.rs:877`), and every lowering entry walls
on it — `hew-cli/src/main.rs:434-440` ("Hew lowering requires a type-checked
program; this path should be unreachable") immediately before
`hew_hir::lower_program`, and again at main.rs:961 for `eval`. There is no
HIR, so there is no SIR, no `type_facts`, no `closure_facts` and no
`identity`: the same shape §6.1 already lists as a P1 blocker for
`hew-cli/src/machine.rs` lowering with `TypeCheckOutput::default()`. The
checker is not an optional stage of this ladder.

Gate: two halves, neither using `--no-typecheck` [P1; the P4 code added at
P4]. **Every gate named here is a P1 lane deliverable, not an existing
target**: `hew tool sir-verify`, `make test-sir-verify`,
`hew-sir/tests/verify_negative/`, `hew-cli/tests/…/walls_e2e.rs` and
`hew-mir/tests/balance_verifier/` do not exist on `main` (`ls hew-mir/tests |
grep balance`, `find . -name 'walls_e2e*'`, `grep -rn sir-verify
hew-cli/src/args.rs Makefile` are all empty), and neither does either ast-grep
rule of §1.1 and §6.1 (`rules/rust/` holds `authority/`, `concurrency-drop/`,
`fail-closed/`, `hygiene/`, `panics-nyi/` and no `is_copy`/`owns_heap` or
emission-order rule). They are listed in the P1 briefs and in §10's P1 row.
(i) `hew-cli/tests/…/walls_e2e.rs` [P1]: every program in
`tests/vertical-slice/reject` whose expected code is one of the eight above is
rejected with that code and that primary span through the ordinary
(type-checked) pipeline. (ii) "the checker is not the only wall" is proved
where it can be — at the verifier's own entry: `hew-sir/tests/verify_negative/`
carries one hand-written textual SIR fixture per user-facing rule (2, 3's
parameter case, 4, 6a, 6b, 6c, 6d), fed to `hew tool sir-verify` (§2.1) and
expected to fail with that rule and to render the `E_OWN_*` code, not
`E_SIR_ICE`. The SIR text is the input a checker hole would produce, so the
fixture is the counterfactual the flag was meant to supply, and it is the
mechanism §2.1 already names. The MIR verifier's
errors are `E_MIR_ICE` (the existing prefix, `diagnostic.rs:137`).
`E_MIR_CHECK` (diagnostic.rs:129) stops existing with the legacy lowerer
[P5]: no user program reaches an ownership check after the SIR verifier.
ownership.md's "There is no fourth wall" is true of `CowValue` types and
loses for the affine families (§11 row 1).

---

## 2. The checked proof

The OSSA verifier runs on every SIR function before lowering to MIR, including
every synthesized function (§6.5). **[current]** `validate_ownership_events`
runs only from `lower_function` (`hew-mir/src/lower/mod.rs:13226`); the six
`seal_checked` sites in `closure_gen.rs` and `task.rs` seal unverified bodies.
That hole is closed by construction: `hew_sir::verify_module` has one entry
and `hew_mir::lower_module` accepts only `Verified<SemModule>`.

### 2.1 Rules 1–6 as predicates

Let `G` be the SSA graph, `paths(d, x)` the set of CFG paths from the
definition of `%d` to exit `x` (return, unwind, cancel), and `consumers(%d)`
the uses in consuming position (`destroy_value`, `move`, `fork`,
`destructure`, `store.*` operand, edge argument, `Suspend` `Move` input,
`Return` operand).

1. **Linearity.** For every `%d : Owned` and every `p ∈ paths(d, x)`:
   `|consumers(%d) ∩ p| == 1`. Zero on a path is a leak, two is a double
   free. Both name the value and the path.
   - positive: `fn f() { let s = "a" + "b"; println(s) }` (the `Binary`
     result is `move`d into `s`; one `destroy_value` at scope exit)
   - negative (internal): a lowering that emits `let s = …; if c { return }`
     without `destroy_value s` on the early-return arm → `E_SIR_ICE
     linearity`. Hew source cannot express it; the negative is a SIR text
     fixture in `hew-sir/tests/verify_negative/`.
2. **Liveness.** No use of `%d` (any position) is reachable from a consumer
   of `%d` on the same path: `∀ u ∈ uses(%d), c ∈ consumers(%d): ¬(c ⇝ u)`.
   - positive: `let v = Vec.new(); sink(consume v)` (nothing after)
   - negative (user, `E_OWN_USE_AFTER_CONSUME`): `let h = g; g.next()` with
     `g: Generator<i64, ()>` (`repros/ladder/gen_rebind.hew`); `c.close();
     println(f"{c.fd}")` (`repros/ladder/resource_early_close.hew`); `let w =
     v; v.len()` with `v: Vec<Conn>` (`repros/ladder/vec_resource.hew`).
   - negative (internal): `move %v` followed by `copy_value %v` where `%v`
     has no source binding (a lowering temp) → `E_SIR_ICE liveness`.
3. **Borrow scopes.** Every `Guaranteed` use is dominated by its
   `begin_borrow` and post-dominated by the matching `end_borrow`; no consumer
   of the owner lies inside `[begin_borrow, end_borrow]`; a `Guaranteed`
   value is never an operand of a consuming position; the region is one of
   the three shapes of §1.4. A parameter whose header slot is `Borrow` is a
   `Guaranteed` value for the whole body — across a `Suspend` too, under §1.4's
   owner condition — so its consuming use is this rule's
   **user-facing** case: `E_OWN_CONSUME_BORROWED` "parameter `c` is borrowed;
   declare it `consume c: Conn`" (§4.2, §11 row 19).
   - positive: `let n = v.len(); v.push(n)` (borrow ends before the fork);
     `fn peek(c: Conn) -> i64 { c.fd }` (`repros/ladder/res_param_borrow.hew`)
   - negative (user): `fn shutdown(c: Conn) { c.close(); }` without
     `consume` (`repros/ladder/res_param_consume.hew`, accepted today by body
     inference)
   - negative (internal): `begin_borrow %v; call f(%b); move %v; end_borrow %b`
     → `E_SIR_ICE borrow-scope`.
4. **Definite initialization.** Over places: forward dataflow with states
   `{Uninit, Init}`. `load.*`/`begin_borrow` require `Init`; `store.init`
   requires `Uninit`; `store.assign` requires `Init`. Places come in three
   classes. A **function-owned** place (extern-addressed `var`, coroutine
   frame slot) is `Uninit` at every exit (`load.take` or `end_lifetime`
   consumed it); a `Maybe` at a join is `E_SIR_ICE`. A **runtime-owned
   `CowValue` place** (a collection/string/record state field, an env field)
   is `Init` at function entry and must be `Init` again at every exit of the
   function that borrows or takes it — normal, unwind and cancel (§1.3
   `load.take` row: the edge op is `store.init %p, %forked`) — and becomes
   `Uninit` only through `end_lifetime` in the owner's stop/drop path
   (`hew_drop$State`, `hew_drop$Env`); a `Maybe` at a join is `E_SIR_ICE`.
   A **runtime-owned `AffineResource`/`Linear` place** (a resource state
   field, §1.3.6) has the lattice `{Init, Uninit, Maybe}` backed by its taken
   bit: `Uninit`/`Maybe` is legal at a join and at every exit; a
   `load.*`/`begin_borrow` on `Uninit`/`Maybe` is the
   user-facing `E_OWN_USE_AFTER_CONSUME` ("may have been closed"), while
   `store.assign` is legal in all three states and lowers to the bit-guarded
   release-and-store of §1.3.6 (`if !taken { taken := 1; Release }; Store;
   taken := 0`) — that is what makes re-initializing a closed field
   expressible, and setting the bit first is what makes a trap inside the
   release single-close rather than double-close. This
   replaces drop flags for every function-owned value and every `CowValue`
   place; the taken bit is the one typed remnant, read only by the owner's
   drop glue. Non-escaping `var`s have no place (§1.3 mem2reg note): their
   definite initialization is the checker's `E_OWN_UNINIT` over HIR bindings
   and construction fails closed if a use has no defining value.
   - positive: `var s = "x"; if c { s = "y" }; println(s)` (mem2reg: block
     argument at the join); `if c { conn.close() }` in a handler with `conn`
     a resource state field (`Maybe` at the join; no later read)
   - negative (user, `E_OWN_UNINIT`): `var x: string; if c { x = "a" };
     println(x)` (`repros/ladder/cond_init.hew`; today `E_MIR_CHECK
     InitialisedBeforeUse` plus two `E_MIR UnresolvedPlace` diagnostics on
     the same binding — mem2reg gives a maybe-uninitialized `var` no MIR
     place, so both its assignment and its use are separately unresolvable
     once the checker has already refused it). The checker rule [P1] is the
     dataflow over HIR bindings; the verifier re-proves places and refuses a
     use with no defining value.
   - negative (user, `E_OWN_USE_AFTER_CONSUME`): `if c { conn.close() };
     println(conn.fd)` on a resource state field.
   - negative (internal): a lowering that `load.copy`s a `CowValue` place it
     never initialized on a path with no source binding → `E_SIR_ICE
     definite-init`.
5. **Totality of physical decisions.** Every `Call` argument, `Suspend`
   input, `Return` operand, capture (closure, lambda, fork, generator),
   `Spawn` argument, send argument (`ActorSend`/`ActorAsk`/`StreamSend`/
   select-ask/`Join`) and `receive gen fn` `Yield` value carries a mode from
   the closed set `{Borrow, Copy, Move, Snapshot::{Share, DeepCopy,
   Transfer}}` decided at construction; the SIR data model has no
   `Undecided` variant, so this rule is a type-level brace plus one runtime
   check that every `Snapshot` operand agrees with the class table:

   | class of the argument | `Snapshot` mode | MIR `SendAliasMode` (model.rs:37-50) |
   | --- | --- | --- |
   | `BitCopy` | bits (no `Snapshot` op; `SnapshotBitCopy` in MIR) | `SnapshotBitCopy` |
   | `CowValue` string / bytes | `Share` | `SnapshotRetain` |
   | `CowValue` collection, record, tuple, enum with heap | `DeepCopy` (§5.5; records via `hew_copy$T`) | `SnapshotMaterialize` |
   | `PersistentShare` with send fact true (a `dyn`/named fn, or a closure whose captures are all `Send`, none `BorrowMut` and none affine — an affine capture makes the closure `AffineResource` and puts it in the row below, §1.1, §1.3.5) | `Share` (`hew_arc_clone`) | `SnapshotRetain` |
   | `AffineResource` / `Linear` with send fact true — `#[resource]` records, `Vec<Conn>`, and **`LambdaPid` although its `clone` is `Retain`** (a `Task` never reaches a send site — it is scope-local and consumed by `AwaitTask`; its `Send` marker, true iff `T: Send` (traits.rs:1090-1099), exists only so a fork body may cross its thread boundary) | `Transfer` only: the sender's binding is consumed (`move`), later use is `E_OWN_USE_AFTER_CONSUME` — main behaviour (`repros/ladder/resource_send2.hew`; `repros/ladder/lambda_send_twice.hew` → `use of moved value \`w\``), spec §3.7.8.1 item 4 (§11 row 5). **decision**: `Share` of a `LambdaPid` was considered and rejected — a second live handle would let two actors race the lambda's release (`hew_lambda_actor_release` joins the dispatch thread on the last handle, lambda_actor.rs:1461-1466), and `main` transfers today | `TransferLastUse` |
   | any class with send fact false (`Rc`, `Weak`, `Pointer`, non-Send or `BorrowMut`-capturing closures, generators, duplex halves) | none — rule 6c | — |

   `Spawn { args }` (node.rs:1603) arguments are `Snapshot` operands like a
   send — the new actor's heap is another owner — and they arrive at the
   synthesized `ActorInit` producer (§6.5) as `Consume` header slots: with an
   `init {}` block they are its parameters (today
   `lower_spawn_actor_init_args`, actor.rs:2685-2715); without one the
   `ActorInit` body is one `store.init` per state field from the argument of
   the same name (today `lower_spawn_actor_state_arg`, actor.rs:2790-2810,
   "requires every state field by declaration name"). `Transfer` on a
   `CowValue`/`PersistentShare` argument is legal only when rule 2 proves the
   producing value has no later use (§3 move-on-send).
   - positive: `worker.handle(conn_info, conn)` in a loop (both `Share`)
   - negative (internal): a `Transfer` on a `CowValue` with a later use →
     `E_SIR_ICE totality` (rule 2 also fires).
6. **Type-class capability** (the walls):
   - 6a: `store.assign` (or the mem2reg edge-argument reassignment) whose
     root binding is a non-`mutable` `HirBinding` (node.rs:1237), **or whose
     root is a `let` actor state field (`HirField.is_mutable == false`,
     node.rs:1178) outside `init {}`** →
     `E_OWN_MUTATE_LET`. `fork` and `load.take → fork → store.init` carry no
     binding-mutability obligation:
     `let v = Vec.new(); v.push(1)` is a COW mutation of the value, not of
     the binding, and is accepted today (`repros/ladder/mutate_let.hew`,
     `repros/ladder/let_map_insert.hew`, `repros/ladder/let_state_push.hew`,
     `tests/vertical-slice/accept/wire_json_vec_option_roundtrip.hew:11-12`,
     `std/text/template/template.hew:445/476`; spec §3.4.3 line 636, §3.4.6).
     positive: `var p = Point{..}; p.x = 1`; `let items: Vec<i64>` state
     field with `items.push(1)` in a handler; negative: `let p = Point{..};
     p.x = 1` (`repros/ladder/let_field_mut.hew`), `let v = Vec.new(); v[0] = 5`
     (`repros/ladder/let_index_assign.hew`), `items = Vec.new()` in a
     handler on a `let` field. A user method that mutates
     `self` is `VarSelfMethodCall` only through a `var` receiver
     (node.rs:2113-2136, `requires_mutable_receiver`), so the checker
     already owns that case.
   - 6b: `copy_value` (incl. the `RecordCloneCall` pair, a capture snapshot
     and a generator capture) on a type whose `clone == None` →
     `E_OWN_CLONE_UNSUPPORTED`. `Rc<T>`/`Weak<T>` have `clone == Retain`, so
     `let b = clone a` on an `Rc` stays legal (`repros/ladder/rc_clone.hew`
     prints `2`; spec §3.7.5).
     positive: `let b = clone a` with `a: Vec<i64>`, `a: Rc<i64>` or `a:
     Vec<Rc<i64>>`; negative: `let g2 = clone g` with `g: Generator<i64,
     ()>`; `clone v` with `v: Vec<Conn>`; `clone b` with `b: bytes` is
     rejected today for lack of a wired path and becomes legal at P1
     (§5.4, §11 row 9).
   - 6c: `Snapshot` on a type whose send fact is false → `E_OWN_SEND_UNSUPPORTED`.
     Every capture into a `SpawnLambdaActor`, `ForkBlock`, or `SpawnedCall`
     environment, every `Spawn` argument and every `receive gen fn` `Yield`
     value is a `Snapshot` operand (`Share` for `Borrow`/`Strong`
     captures of shareable types, `Transfer` for `Move` captures), so this
     rule covers spec §3.4.5/§3.4.7 (non-Send capture) and not only
     `ActorSend`. The send fact is the checker's per-substituted-type `Send`
     decision carried on the class table [P1], with the closure rule of
     §1.3.5 (false when any capture is `BorrowMut`); today it is decided at
     eight `implements_marker(MarkerTrait::Send)` sites (`expressions.rs:1435,
     2650, 2749, 2767, 7576`; `calls.rs:2571, 2591`; `methods.rs:1516`) and
     carried for captures as `ClosureCaptureFact.is_send` →
     `HirClosureCapture.is_send` (node.rs:3183); all eight read
     `TypeFacts.send` at P1. `TraitRegistry::is_send` (`traits.rs:1107`) has
     no production caller and is deleted (§9). A `Borrow` capture of a
     non-`Copy` shareable value into a lambda actor is a legal `Share`
     (`repros/ladder/cap_nomove.hew` prints `hello`, `hello`, `1`); spec §3.4.5
     "Non-`Copy` values cause a compile error" loses (§11 row 23).
     positive: `printer.print(greeting)` with `greeting: string`; negative:
     `worker.take(rc)` with `rc: Rc<i64>` (ownership.md: `Rc`/`Weak` are
     non-`Send`); `actor |x| { use(local_handle) }` with a non-Send capture;
     `actor |x| { f() }` with `f = || { n = n + 1 }`
     (`repros/ladder/closure_mut_share.hew`).
   - 6d (`Linear`): `destroy_value` on a `Linear` value on a normal or
     cancel exit → `E_OWN_MUST_CONSUME` (spec §3.7.8.2 `MustConsumeAtScopeExit`;
     spec §3.7.8.4 Path 1). On an **unwind** exit the `destroy_value` of a
     `Linear` value is legal and lowers to storage reclaim only — spec
     §3.7.8.4 Path 4: "The move-checker does not require `#[linear]` consume
     on trap-only edges". A `Linear` capture into a shared env (§1.3.5) is
     the same error at the capture site. An unbound spawn mints no `Linear`
     value (§1.5), so `scope { work(); }` and `fork { … }` are not subject to
     6d.
     **`Task<T>` is exempt on cancel exits [decision].** A task's only legal
     consumers are `AwaitTask` and a select `TaskAwait` arm; both are
     themselves `Suspend`s and a `Suspend` cannot appear on a cancel block, so
     a `Task` live across any earlier suspension would have no admissible
     cancel-edge consumer. `destroy_value %t : Task<T>` on a **cancel** exit is
     therefore legal and lowers to nothing: the scope already owns every task
     and frees it and its unconsumed result at `free_scope_tasks`
     (task_scope.rs:619-627) through `result_drop_fn` (§1.5, §11 row 14).
     Without this the canonical structured-concurrency join —
     `repros/20_generic_task_spawn.hew:23-26` and
     `tests/hew/task_entry_adapter_symbol_collision_test.hew:38-41,49-52`
     (`fork t1 = …; fork t2 = …; let v1 = await t1; let v2 = await t2`), all
     shipped and transcript-verified — would be rejected, because `t2` is live
     across the `Suspend` of `await t1`. For a user `#[linear]` value 6d on a
     cancel exit stands and `defer` is the consumer the language gives:
     §1.3.3 inlines deferred bodies on the cancel edge before the scope's
     `destroy_value`s, so `defer { tx.rollback() }` discharges the obligation.
     That is a tightening — `main`'s `MustConsume` walks only
     `Terminator::Return`-terminated blocks (`dataflow.rs:1467-1472`), so no
     cancel exit is checked today — recorded as §11 row 32.
     positive: `fork t = work(); let r = await t`; `fork a = f(); fork b =
     g(); let ra = await a; let rb = await b` (`b` live across the first
     `Suspend`, destroyed on its cancel edge); `scope { work(); }`;
     negative: `fork t = work(); }` (`repros/ladder/fork_unawaited.hew`, today
     `E_MIR_CHECK MustConsume`, a normal exit, plus an unrelated
     `E_NOT_YET_IMPLEMENTED`: `fn main` has no execution-context parameter, so
     `fork` from top-level `main` cannot lower at all yet, independent of
     whether the forked task is later consumed); `let f = || { tx.commit() }`
     with `tx: #[linear] Tx`; a `#[linear] Tx` live across an `await` inside a
     `scope {}` with no `defer` consumer.

Command: `make test-sir-verify` [P1] runs `hew-sir/tests/verify_positive/*.hew`
(compile through `hew check`) and `verify_negative/*.sir` (textual SIR fed to
`hew tool sir-verify`, each expected to fail with the named rule). The target,
the `hew tool sir-verify` subcommand and both fixture directories are P1 lane
deliverables; none exists on `main` (§1.6).

### 2.2 What the verifier does not prove

- **Lifetimes of `View` values.** A `View` (extern-returned `&T`, `Slice`,
  `Pointer`) is `None`-kind and only checked to sit inside the borrow scope of
  the call that produced it (rule 3 on the call's own borrow). Whether the
  callee's result really points into its argument is the FFI table's claim
  (§6.4), audited at the `extern` boundary, not proven here. Hew has no
  surface references and every environment owns its captures (§1.3.5), so no
  user program can build a dangling view.
- **Aliasing across calls.** Calls borrow (ownership.md), so a callee sees an
  immutable COW view; the verifier does not track which values a callee may
  retain because a retained share is a `copy_value` inside the callee's own
  body, balanced there.
- **Uniqueness before mutation.** `fork` is unconditional in SIR; whether the
  runtime actually copies is a refcount check at run time (a bytes mutator's
  internal `ensure_unique`, or nothing at all for today's deep-copied
  collections, §4.3/§5.5). The verifier proves the fork happens before the
  write, not that it is needed.
- **Cycles.** Refcounting leaks cycles (ownership.md Q321). Not a verifier
  matter. `HirActorDecl.cycle_capable` (node.rs:517-520) selects the
  runtime's cycle-breaking spawn strategy (`suspend.rs:7102-7121` sets
  `HewActorOpts.cycle_capable`); it is a spawn-policy bit carried on the
  actor header in `MirModule` (§4.2), not an ownership decision, and is kept.
- **Actor state races.** Actor state places are accessed only from handlers,
  methods and hooks of the owning actor, which borrow or `load.take`/`store.init`
  the state place; cross-actor access is impossible by construction (state
  places are not values that can be sent). A closure env with a `BorrowMut`
  field is a mutable place too, and rule 6c keeps it inside one actor.
  Nothing to prove.

This is enough for Hew's surface because there is nothing else to be unsound
about: no references, no lifetimes, no interior mutability outside actor
state and `BorrowMut` closure envs (both actor-local), and every heap value
is either retained (`copy_value`) or forked before mutation.

---

## 3. Canonical OSSA optimizations before MIR

Allowed transforms on verified SIR, in this order, each bracketed by
re-verification (`verify → transform → verify`; the second failure is
`E_SIR_ICE opt:<pass>` naming the pass — the `optimize.rs:53/83` bracket
pattern on `main` today):

| Pass | Rewrite | Rules that must survive | Restriction |
| --- | --- | --- | --- |
| Constant-CFG fold (exists: `hew-sir/src/optimize.rs`) | fold `Branch` on `ConstBool`, discard unreachable blocks | a discarded block must hold no `Owned` consumer of a value live on the surviving path (today's discard-safety check, `verify.rs:246-327`) | — |
| Copy propagation | `%c = copy_value %v … destroy_value %c` with no consumer of `%v` between → delete both, rewrite uses of `%c` to a `Guaranteed` borrow of `%v` | 1, 2, 3 | — |
| Destroy sinking | move `destroy_value %v` from scope exit (or block exit for a temporary, §1.3.4) to immediately after the last use of `%v` on each path | 1, 2 (never past a `Suspend` — the frame set is decided after this pass; never past a `defer` site, §1.3.3) | **only values whose class is `CowValue`** [decision]. Under the §1.1 aggregate rule that is exactly "no `AffineResource`/`Linear` leaf reachable through the type", so `Vec<Conn>`, `Vec<Rc<T>>` and a closure capturing an `Rc` are excluded by class, no second type walk. `PersistentShare` is **not** sinkable: a `dyn Trait`'s class is flat (§1.1) because the concrete payload is not part of the type, so a `dyn` over an `Rc`-holding record would have its `hew_arc_drop` → `hew_drop$<Concrete>` → `hew_rc_drop` sunk before scope exit and `Weak.upgrade()` would flip to `None` early — `main` prints `5`, `alive` for that program (probe `dyn Show for Holder { r: Rc<i64> }`, `repros/ladder/dyn_rc.hew`). Excluding the whole class costs one missed sink on named-fn values and captureless closures and needs no per-payload walk. `AffineResource`/`Linear` releases run a destructor (`close`, `hew_rc_drop` making `Weak.upgrade()` flip to `None`) and stay at scope exit: spec §3.7.3 "Cleanup runs at a predictable point (scope exit)", §3.7.5, §3.7.6 (`repros/ladder/weak_scope.hew` prints `5` then `alive`; `repros/ladder/vec_rc_weak.hew` prints `1`, `5`; `repros/ladder/vec_resource_drop.hew` closes after `2`; core-matrix `#[resource]` cells pin the close position) |
| Move-on-last-use for sends | `Snapshot::Share`/`DeepCopy` of `%v` whose only later consumer is the scope-exit `destroy_value` → `Snapshot::Transfer` and delete the destroy | 1, 5; requires the argument not be reachable from any other live value (aliases/projections always snapshot, ownership.md "uncertain branches, loop back-edges, aliases, and projections always snapshot") | the same class restriction as destroy sinking (a transferred `AffineResource` is already a `Transfer` by rule 5) |

No pass may introduce an op MIR cannot lower (`Unreachable` lowering exists,
`model.rs:3889`). Passes land [P1] (fold, copy propagation), [P2] (destroy
sinking), [P4] (move-on-send). Each pass ships with a counterfactual test:
the pass applied to a hand-built SIR fixture where the rule would break must
be refused by the bracketing verifier, not applied; destroy sinking ships a
negative fixture that hands it an `AffineResource` destroy (a `Conn` and a
`Vec<Conn>`) and asserts both are left in place.

---

## 4. MIR: one physical form with a typestate witness

### 4.1 The form

```rust
pub struct MirFunction {
    pub key: MirCallableKey,           // §6.1; the only identity
    pub header: CallableHeader,        // symbol, conv, param modes, ret, frame
    pub locals: Vec<LocalDecl>,        // typed slots for materialized places
    pub blocks: Vec<BasicBlock>,       // Instr stream + Terminator
    pub frame: Option<CoroutineFrame>, // §4.2
}
pub struct Mir<S: Stage> { function: MirFunction, proof: S::Proof }
pub struct Raw;      // Proof = ()
pub struct Checked;  // Proof = BalanceWitness (per-value, per-place counts, §4.5)
```

`RawMirFunction`, `CheckedMirFunction`, `ElaboratedMirFunction`
(`hew-mir/src/model.rs:2978, 6722, 7046`) collapse into this [P1]; `--dump-mir
raw|checked` are two views of one struct, `elab` is deleted [P5]. Codegen's
entry is `fn lower_module(m: &MirModule<Checked>)`; it does not compile
against `Mir<Raw>` (type-level; `hew-codegen-rs` has no `Raw` import, checked
by the ast-grep rule `codegen-consumes-checked-only` [P1]).

`Terminator::Call.callee` is `MirCallableKey` (today `String`,
`model.rs:3927`); `ExitPath::{Call, Unwind}` carry `MirCallableKey` (today
`callee: String`, model.rs:7214-7226). `fn_symbols`,
`param_boundary_modes_by_function`, `representation_loan_params_by_function`
keyed by `func.name` (`llvm.rs:37995-38008`) do not exist in the fresh emitter;
the symbol comes from `header.symbol` looked up by key.

### 4.2 What MIR owns

- **`TargetLayout`**: size/align/field offsets/discriminant encoding for every
  type instance (`TypeInstanceKey`, §6.2), parameterized by pointer width and
  target ABI, no LLVM type. For an actor state record it also lays out the
  **taken bit** of every `AffineResource`/`Linear` field (§1.3.6): one byte
  per such field after the fields, read by `hew_drop$State` and written by
  `MarkUninit`/`Store` on that place.
- **`Place`** = `LocalId` + projections (`Field(i)`, `Index`, `Deref`,
  `EnumPayload(variant)`, `MachineTag`, `ActorState(field)`, `EnvField(i)`).
  `Place::Value` is forbidden. The current 12 special-cased handle places
  (`DuplexHandle`, `SendHalf`, `RecvHalf`, `LambdaActorHandle`, `ActorHandle`,
  `MachineTag`, …, `model.rs:4399`) become ordinary locals whose type carries
  the drop glue (§5).
- **`Materialize { value, local, reason }`** with `reason ∈ { AddressTaken,
  ByRefAbi, CoroutineFrame, Capture, Transport, ExplicitStorage, TrapFrame
  }`. Layout is never a reason. Every SIR `alloc_place` is `ExplicitStorage`;
  resume-edge arguments are `CoroutineFrame`; envelope payloads are
  `Transport` and are always a **malloc-compatible allocation**
  (`hew_msg_envelope_new` doc, mailbox.rs:536-540: "`payload` must be a
  malloc-compatible allocation"; the envelope frees it with `libc::free`,
  cow_envelope.rs:114-118) — never stack or arena storage; `TrapFrame` is the
  registry-target realization of values live across a trapping call (§4.7).
- **Calling convention** per callable: `CallableHeader { key, symbol, conv:
  Default | TaskEntry | ActorHandler | ActorInit | ActorMethod | LifecycleHook
  | ClosureInvoke | GeneratorBody | Extern, params: Vec<ParamMode>, ret:
  RetMode }` where `ActorMethod` is a header LABEL over the ordinary `Default`
  shape rather than a second convention — an actor-body plain `fn` already
  lowers as a `Default` callable whose declared params are `(ctx, state,
  <user params>)` (#3285) — and `ParamMode ∈ { Borrow, Consume, Retain }` (the same three
  words as `ExternParamOwnership`, `hew-types/src/ffi_contracts.rs:10`) and
  `RetMode ∈ { None, Fresh, Borrowed }`. The header is derived from the
  checker's registries, never from body lowering. A slot is `Consume`
  exactly when: the parameter is `consume`-declared (`HirBinding.is_consume`,
  node.rs:1246); the receiver is `consuming self` (`std/net/net.hew:473`);
  the callable is the `close` declaration of a `#[resource]` type
  (`LifecycleRegistry ResourceRecordLifecycle.close_declaration`), whose
  receiver or first parameter consumes in either spelling — `fn close(self)`
  (spec §3.7.8.1) or `fn close(c: Conn)` (spec §3.7.8.5;
  `repros/ladder/resource_early_close.hew` shows main already treats it as
  consuming); the callable is a declared consuming method of a `#[linear]`
  type; or the slot is a synthesized producer's owned input (`ActorInit`
  parameters, the `TaskEntryAdapter`/`ForkEntryShim` env, §6.5). Every other
  slot is `Borrow`. **This derivation covers `conv ∈ {Default, TaskEntry,
  ActorHandler, ActorInit, ActorMethod, LifecycleHook, ClosureInvoke,
  GeneratorBody}` only. For `conv: Extern` the authority is the
  `ExternOwnershipContract` row (§6.4), not the declaration** [decision]: the
  TOML is "the single source of truth" for the C-ABI side
  (`hew-types/src/ffi_contracts.rs:1-6`) and only it can express `Retain`,
  which no `.hew` spelling has. The `.hew` signature's `consume` modifiers do
  not decide the header; they must **agree** with the row, and disagreement is
  a build error, never a silent winner. Today that cross-check exists but is
  reached only from the opaque-resource release-contract validation
  (`check/registration.rs:430-465`, `"source consume disposition differs from
  contract at parameter {index}"`); [P3] it runs for **every** registered
  extern, and `make test-ffi-table` (§6.4) fails on a disagreeing row as well
  as an `Absent` one. Without one named winner a row saying `Consume` under a
  signature without `consume` would give the caller a `Borrow` header (which
  destroys at scope exit) and the callee a consumed owner — a double release;
  the mirror case leaks. **[current]** `main` infers a by-value `#[resource]`
  parameter's disposition from the body: `facts.rs:994-1075` seeds every
  resource parameter at its `consume` annotation, force-consumes
  "non-receiver resource params of `impl`/trait methods", then runs a
  "monotone least-fixpoint: a pass only ever flips a BORROW param to
  CONSUME"; so `fn shutdown(c: Conn) { c.close(); }` compiles and
  `shutdown(conn)` consumes the caller's binding
  (`repros/ladder/res_param_consume.hew` → `close 3`, `after`;
  `res_param_consume_use.hew` → `E_MIR_CHECK … UseAfterConsume`), while `fn
  peek(c: Conn) -> i64 { c.fd }` is inferred borrow (`res_param_borrow.hew`
  → `3`, `3`, `close 3`). **[decision, P1]** the header is declaration-only:
  the body of `shutdown` is a consuming use of a `Guaranteed` parameter and
  is rejected as `E_OWN_CONSUME_BORROWED` (rule 3) with the fix-it "declare
  it `consume c: Conn`"; the fixture moves accept → reject (§11 row 19). The
  "inferred borrow/consume disposition" that node.rs:1237-1246 describes
  (`facts.rs compute_param_ownership`) is deleted (§9); `ParamBoundaryMode`'s
  eight variants (`model.rs:8362`) are deleted [P5]. **P3 gate**: `std/`
  compiles under the wall — every std function that consumes a resource
  parameter declares `consume` (49 declarations exist today, `grep -rn
  '\bconsume [a-z_]*:' std --include=*.hew | wc -l`); the sweep is
  diagnostic-driven and mechanical. A handler's payload slots are always
  `Consume` (§5.6: one disposition, the handler takes the payload) — no header
  fact is derived from whether the body suspends; `LifecycleHook` params are
  `Borrow` (their payload is runtime-owned across the call, §5.6).
- **Carriers**: the runtime representation per class — `string` = header-aware
  `*mut c_char`; `bytes` = `BytesTriple` (a bytes mutator rewrites the whole
  triple: `ensure_unique` returns a new buffer with `offset` reset to 0 and
  consumes one owner of the old, bytes.rs:181-205, 435-440); collections =
  `*mut HewVec` etc.; closures = `{fn, env}` pair with an `hew_arc_*` env
  (§5.4); `dyn Trait` = `{data, vtable}` fat pointer over an `hew_arc_*` box;
  actor handles = pid; records/enums = inline aggregates by `TargetLayout`.
- **Coroutine frames**: `CoroutineFrame { slots: Vec<(LocalId, Ty)> }` from
  the union of `CoroutineFrame` materializations; codegen emits the switched
  resume from this description (`hew-codegen-rs/src/suspend.rs` reads only
  this [P4]).
- **Envelopes**: `Instr::PrepareEnvelope { args: Vec<(Place, SendAliasMode)>,
  drop_glue: GlueRef, reply: Option<Place> }` with `SendAliasMode`
  (`model.rs:37`, kept: `SnapshotBitCopy`, `SnapshotRetain`,
  `SnapshotMaterialize`, `TransferLastUse`) mapped 1:1 from the SIR snapshot
  mode, and `drop_glue = hew_drop$<MsgRecord>` (§5.6) — the envelope owns the
  payload after the send until dispatch takes or borrows it.
- **Actor header** per actor: state layout (with taken bits), `cycle_capable`,
  mailbox capacity/overflow/coalesce policy and `coalesce_key_fn`,
  `hew_drop$State` glue ref (§5.7). There is **no** per-handler payload
  disposition field: every handler takes its payload (§5.6).

### 4.3 Mechanical mapping SIR → MIR → runtime

| SIR op | MIR instruction(s) | runtime symbol (by carrier) |
| --- | --- | --- |
| `copy_value %v` | `Retain { src, dst }` (writes `dst`; `dst == src` only where the leaf returns the same pointer) | string `hew_string_clone`; bytes `hew_bytes_clone_ref`; Vec/HashMap/HashSet `hew_vec_clone_owned` / `hew_hashmap_clone_layout` / `hew_hashset_clone_layout` (§5.5 shortcut; the descriptor's `clone_fn` clones each element — `hew_vec_clone_layout` is the BitCopy-element entry and aborts on `LayoutManaged`, vec.rs:1799-1815); closure/`dyn` env `hew_arc_clone` (null env → no-op); `Rc`/`Weak` `hew_rc_clone` / `hew_weak_clone_rc`; `LambdaPid` `hew_lambda_actor_clone` (returns a new boxed handle, lambda_actor.rs:1140-1182); record/enum `hew_copy$<ty>` glue (§5) |
| `destroy_value %v` | `Release { place }` | `hew_drop$<ty>` (§5) or the leaf symbol (§5.2 item 6) |
| `move %v` | `Move { src, dst }` | none (register move; `src` is dead after) |
| `fork %v` | `Fork { place }` | **no runtime call for any current carrier**: `string` — never emitted (§1.3); `bytes` — no-op, every bytes mutator calls `ensure_unique` itself and rewrites the triple (bytes.rs:181, 435-440); `CowValue` collections — no-op (§5.5, a `copy_value` already produced a unique deep copy); `AffineResource`/`Linear` collections (`Vec<Conn>`, `Vec<Rc<T>>`) — a register move, unique by class, and it stays one when §5.5 retires (an affine collection is never shared, so it must never pay an `ensure_unique` refcount check: the realization is selected by the class row in the runtime symbol table); record/enum/tuple — no-op (an inline aggregate is unique; the field write is `Release` of the old field + `Store`). `hew_string_make_unique`/`hew_bytes_make_unique` are **not** introduced: `cstring_ensure_unique` reads the 16-byte header at `data-16` and "Unmanaged pointers must be filtered out by the caller" (cabi.rs:495-511, 418-433), so a fork over a record whose string field is a rodata literal (`var p = P{name: "x", n: 0}; p.n = 1`) would read `rodata-16`. `Fork` stays an instruction so §5.5's obsolescence is a one-row change |
| `destructure %agg` | `Move` per field from projections + `Release` of the shell if boxed | none |
| `alloc_place T` | `LocalDecl` + `Materialize { ExplicitStorage }` | none |
| `load.copy %p` | `Load { place, dst }` + `Retain` | as `copy_value` |
| `load.take %p` | `Load { place, dst }` + `MarkUninit { place }` | none for a `CowValue`/function-owned place; a store of `1` to the taken bit for an `AffineResource`/`Linear` state field (§1.3.6) |
| `store.init %p, %v` | `Store { place, src }` | none (clears the taken bit where one exists) |
| `store.assign %p, %v` | `Release { place }` + `Store { place, src }`; on a place with a taken bit the bit is **set before** the `Release`, the `Release` is guarded by `!taken`, and the `Store` clears it (`if !taken { taken := 1; Release }; Store; taken := 0`, §1.3.6); on a **state place inside a trapping region of a `CrashOwnerRegistry` target** the pair is additionally bracketed by `hew_dispatch_state_cleanup_begin_replace` before the `Release` and `hew_dispatch_state_cleanup_prepare` before the `Store` (§4.7) — one SIR op, one extra target-decided bracket, no second ownership decision. **[current] `main` emits that bracket on every target**, selected by handler kind alone: `llvm.rs:18408-18416` emits `begin_replace` whenever `fn_ctx.actor_state_store_transaction == Required`, and `actor_state_store_transaction_for_kind` (llvm.rs:22252-22266) returns `Required` for `ActorHandlerKind::{Receive, Exit, Down}` and for `None`, reading neither `CleanupUnwindStrategy` nor the triple. Narrowing it to the registry target is sound **only** because the bit is set before the release (§1.3.6): on a `StructuredLlvm` target the trap edge is the landingpad plus `hew_drop$State`, which honours the bit, so the escrow's replacement authority is not needed and is not armed there at all (§5.7) | as `destroy_value`; the bracket symbols are runtime-table rows |
| `end_lifetime %p` | `Release { place }` | as `destroy_value` |
| `begin_borrow` / `end_borrow` | nothing | none — a borrow is an SSA fact with no physical cost |
| `Suspend` | `Materialize { CoroutineFrame }` × frame set + `Terminator::Suspend { kind, resume, cancel }`; the cancel block begins with the row's abandon call and each resume edge of a multi-arm suspend with every loser's (§1.5) | per `kind`: `hew_task_await_suspend` + `hew_task_take_result` (copy out) + `hew_task_detach_await` (abandon), mailbox park, `hew_reply_channel_*`, timer wheel, … (`hew-mir/src/runtime_symbols.rs` rows) |
| `Call` | `Terminator::Call { callee: key, args, modes }` | `header.symbol` |
| `push` / `insert` (a `move` into a collection) | `Terminator::Call` to the move-in entry | `hew_vec_push_owned_move` (vec.rs:2663); `hew_hashmap_insert_layout_move` / `hew_hashset_insert_layout_move` [P2, §5.3] |
| `spawn_detached` (§1.5) | `Terminator::Call` to the spawn entry, no `dst` | `hew_task_new` + `hew_task_scope_spawn` + `SpawnTaskDirect` (task.rs:566-573 today) |

`Retain`/`Release`/`Move`/`Fork`/`Load`/`Store` are the only ownership-bearing
instructions. `Instr::StringRetain { condition }`, `BytesRetain`,
`ValueSnapshotDrop`, `Drop { DropFnSpec }`, `OwnershipEvent`,
`NeutralizePayloadSlot`, `AggregateProjectionNeutralize`,
`AggregateOverwriteRelease`, `RecordFieldDrop`, `FieldDropInPlace`
(`model.rs:5106-6503`) are deleted [P5].

### 4.4 Ownership arrives decided

MIR lowering from SIR is a fold over the op stream; it consults `TargetLayout`
and the header table and nothing else. There is no elaboration pass, no drop
plan per exit, no replay: every `destroy_value` is already an instruction on
the path OSSA proved it belongs to. `hew-mir/src/lower/drop_plan.rs`
(`derive_drop_plans_from_replay`, `seal_checked`, `elaborate`) has no
successor.

### 4.5 The MIR verifier (second oracle)

Runs on `Mir<Raw>`, produces `Mir<Checked>` with a `BalanceWitness`. It shares
no code and no model with the SIR verifier: it never sees `OwnKind`, it counts
physical operations.

- **Balance.** For every local `%l` of an owning carrier and every path
  from function entry to each exit: count `+1` at every producer (`Retain
  dst`, `Load+Retain dst`, `Fork dst`, `Move dst`, fresh-result `Call dst`
  per header `RetMode::Fresh`, literal allocation, resume-edge result) and
  `-1` at every consumer (`Release`, `Move src`, `Store src`, `Call` argument
  with `Consume`, `PrepareEnvelope` `TransferLastUse`, `Suspend` `Move`
  input, `Return`). The running count must never go negative and must be
  zero at every exit. A place's count must be `≤ 1` at every point (one owner
  per slot); a state place with a taken bit may be `0` at exit.
- **Structural.** Every block ends in a terminator; every `Place` local is
  declared; every `Load` is dominated by a `Store`/`Materialize` of the same
  place with no intervening `MarkUninit`/`Release`; every `Call` callee has a
  header row; every type in `locals`/`header` has a `TargetLayout`; no
  `TypeParam`; every cancel block begins with its kind's abandon call — one
  per arm, in arm order, for a multi-arm `Suspend` — and every resume edge of
  a multi-arm `Suspend` begins with the abandon call of every arm but the
  winner, in arm order, before the winner's result read (§1.5).
- **Frame.** Every local live across `Terminator::Suspend` is a frame slot;
  on a `TrapFrame` target every owning local live across a trapping call is
  a `TrapFrame` slot armed for that call's region and deactivated on its
  normal edge, with no `Store`/`MarkUninit`/`Release` of a **covered
  `TrapFrame` local** between the arm and the deactivate. A state place is
  never a `TrapFrame` slot, so a `store.assign` of a state field inside a
  region is permitted and carries the escrow bracket instead (§4.7).

If the SIR verifier accepted and the MIR verifier refuses, the compiler fails
closed with both facts printed (`E_MIR_ICE balance <fn> <local> <path>` plus
the SIR value it came from via the lowering's `ValueId → LocalId` map). That
is one authority checked twice, not two authorities reconciled.

Test [P1, a new test directory — see §1.6]: `hew-mir/tests/balance_verifier/`
hand-built `MirFunction` fixtures —
one positive per instruction kind, one negative per rule (missing `Release`,
double `Release`, `Load` after `MarkUninit`, callee without header, live local
not in frame, cancel block not starting with the abandon call, a two-arm
`Suspend` whose resume edge omits the losing arm's abandon call, and one whose
abandon calls are out of arm order) [P1].

### 4.6 What codegen accepts

`hew-codegen-rs` is rewritten [P1, grown per phase] as a fold over
`MirModule<Checked>`: instruction lowering, ABI realization from
`CallableHeader`, frames from `CoroutineFrame`, runtime symbol references from
one table (`hew-mir/src/runtime_symbols.rs` stays the table). It contains no
predicate over `ResolvedTy` shape that chooses a retain, release, or symbol.
Concretely gone (§9): `retain_string_field_load` (llvm.rs:16698),
`resolved_ty_cow_heap_release` (26256), `emit_heap_slot_drop` (26630),
`resolved_ty_element_owns_heap_for_owned_vec` (26059), the borrow-taint
fixpoints (22270-22740; their successor is the envelope protocol of §5.6, not
a MIR fact), the crash-cleanup owner **derivation** (23264-25218:
`collect_helper_crash_cleanup_descriptors`, `helper_crash_cleanup_owner_lineages`
re-deriving lineage from `OwnershipEvent`s — its runtime API survives as the
§4.7 registry target's executor), `lower_drop_runtime`'s symbol-string ABI
switch (21990/22040), `is_known_cow_heap_drop_symbol` (26336),
composite-return admission (3342-3420, 36911), type-directed glue synthesis
(6083-11141), `emit_actor_message_drop_fn` (thunks.rs:3302),
`emit_insert_overwrite_key_release` (layout.rs:4822),
`wire.rs::emit_de_drop_owned` (137), `layout.rs` key/value `drop_fn`
derivation (2747/2861/4839), `thunks.rs:433`, `suspend.rs:7724`.
Gate: `grep -c 'ResolvedTy::' hew-codegen-rs/src/*.rs` outside `layout.rs`
type→LLVM-type mapping is zero at P5 (recorded in the P5 PR body).

### 4.7 Target realization of the unwind edge

SIR is target-independent: the unwind edge of a trapping call is a CFG edge
whose block carries `destroy_value`s and the `store.init`s rule 4 demands
(§1.3 `load.take` row, §1.3.3, §5.6). How that block **executes** is a MIR
target fact, decided by `cleanup_capabilities_for_target`
(`hew-codegen-rs/src/llvm.rs:556-575`) today and kept as the one target
switch:

| Target | `CleanupUnwindStrategy` [current] | how the SIR unwind block runs |
| --- | --- | --- |
| linux/macOS/FreeBSD | `StructuredLlvm` (`invoke`/landingpad; the one `build_invoke` site, llvm.rs:23095; `unwind_enabled` at 36747) | the block is the landingpad's cleanup code; `hew_panic` is `extern "C-unwind"` (actor.rs:7142) and the actor boundary catches at actor.rs:2859 |
| `windows-msvc` | `CrashOwnerRegistry, includes_unwind_plans: true` ("Inkwell does not currently expose the Windows funclet builders required to emit valid MSVC EH IR"); every call is a plain `build_call` (llvm.rs:23052) and the Rust panic unwinds through Hew frames without running frame code | **[P4 decision]** the block is compiled as an out-of-line cleanup function `__hew_unwind$<fn>$<block>(frame: ptr)` over a `TrapFrame` record (`Materialize { TrapFrame }` for every owning local and every runtime-owned place live across a trapping call in that function), registered with the existing typed registry (`hew_cont_crash_cleanup_arm(token, slot, size, align, thunk, storage, relocation) -> u64`, cont.rs:997-1004; `deactivate` 1124 when the region's normal edge runs; `retire` 1162 at frame exit) and run by the runtime's catch site before `hew_drop$State` (actor.rs:2600-2606). **Region granularity is one trapping call [decision].** A `TrapFrame` slot is armed immediately before the call it protects and deactivated on that call's normal edge; a MIR verifier Frame rule refuses any `Store`/`MarkUninit`/`Release` of a covered slot between the arm and the deactivate, so within a region the slot's value never changes and nothing needs re-arming. A slot live across `n` trapping calls is armed `n` times — `hew_cont_crash_cleanup_arm` with the slot's returned non-zero token reactivates the same entry "after reassignment without changing its first-activation order" (cont.rs:983-985), which is exactly this obligation, and `retire` runs once at frame exit. A coarser region would be unsound rather than merely imprecise: a stack `TrapFrame` in a synchronous helper and every actor-state place must use `Snapshot` storage (`DirectFrame` is rejected unless `active_frame_contains_range(owner_frame, slot, size)`, cont.rs:894-896, and `DirectFrame` means "the slot itself lies in the tracked coroutine allocation", cont.rs:160-165), and `copy_crash_cleanup_snapshot` byte-copies the slot only from an arm/reactivate call (cont.rs:938-976) — so a region spanning a since-consumed value would release stale bytes and a region spanning a since-produced one would release nothing. **Actor-state places are not `TrapFrame` slots**: they use the registry's separate dispatch-state escrow, whose per-field transaction hooks `hew_dispatch_state_cleanup_{begin_replace, clear, prepare, prepare_transfer}` (cont.rs:1437-1563) already re-establish escrow authority per field store, so a `store.assign` of a state field inside a trapping region is expressed as begin_replace → release → prepare → store, not as a re-arm (§4.3's `store.assign` row carries the bracket). **That bracket is emitted on this target only** [P4 decision]: `main` emits it on every target from the handler kind alone (llvm.rs:18408-18416, selector at 22252-22266), and `begin_dispatch_crash_cleanup` is armed on every target too, because "the LLVM landing pads have already destroyed ordinary Hew locals. This compatibility escrow handles actor-state writes until state itself is represented as an OSSA owner" (scheduler.rs:3618-3620) — this design is the sentence that condition names. On a `StructuredLlvm` target state fields are OSSA places whose taken bit is set before their release (§1.3.6), the landingpad runs the SIR unwind block, and `hew_drop$State` honours the bit at teardown, so neither the escrow nor its per-field bracket is armed there and the per-dispatch `state_size` byte copy disappears (§5.7). Consequently the cleanup thunk never writes back into a live state field: `begin_dispatch_crash_cleanup` copies the **whole** state allocation — `state_size` bytes, so `TargetLayout`'s trailing taken bits are inside the escrow — into a distinct block (cont.rs:1233-1262), and the catch site runs `state_drop` over that snapshot and then deallocates it (`free_dispatch_state_snapshot`, cont.rs:722-750), raw-freeing the live wrapper without invoking `state_drop` again (cont.rs:1330). So on the trap edge `hew_drop$State` reads the escrowed taken bit, which the per-field hooks kept current; there is no write-back and nothing to re-arm. The registry is scoped to the active tracked frame (`current_crash_cleanup_registry()`, cont.rs:1197-1205, `active_top_tracked_frame`; a new arm "Returns zero … when no tracked coroutine is active", cont.rs:987), so the realization covers every actor-hosted body and the synchronous helpers it calls (`Snapshot` entries); a trap on the main thread outside any actor terminates the process on every target and runs no cleanup — the same as `main`. The cleanup body is SIR code compiled by the same fold — codegen decides nothing; the registry executes what SIR proved. So a handler that `load.take`d a state field and trapped re-`Init`s it (or leaves its taken bit set) before `hew_drop$State` walks the record, on every target. SHORTCUT: WHY — inkwell has no `cleanuppad`/`catchswitch` builders; WHEN-OBSOLETE — when inkwell (or a direct LLVM-C shim) exposes MSVC funclets; WHAT — `StructuredLlvm` on MSVC, `TrapFrame` and the registry rows deleted. Gate: the resource-trap fixture (`open` a `#[resource]`, `load.take` a state field, trap) runs under `win11-dev` (ssh-mcp) with the ASan/leak oracle and asserts one `close` [P4] |
| `wasm32` | `CrashOwnerRegistry, includes_unwind_plans: false` ("traps end the instance") | no unwind block runs; a trap terminates the instance and no cleanup is observable. One sentence, no mechanism; the sandbox stays outside the ladder for v0.6.0 (program plan §6) |

`TrapFrame` never appears on a `StructuredLlvm` target (the MIR verifier
refuses it there); the target switch is the only place `hew-mir` reads the
triple.

---

## 5. Drop glue per monomorphic type

### 5.1 Naming, ABI and emission

For every type instance `T` with `clone ≠ Bits` (every non-`BitCopy` class),
MIR publishes one glue pair in the module header:

```
hew_drop$<mangle_resolved_ty(T)>   (slot: ptr) -> void          // release the value in place; never frees the slot
hew_copy$<mangle_resolved_ty(T)>   (src: ptr, dst: ptr) -> i32  // retain/deep-copy fields into dst; 0 = ok, non-zero = rolled back
hew_abandon$<mangle_resolved_ty(T)>(slot: ptr) -> void          // only for a T with a Linear leaf; §5.2 item 7 Abandon role
```

The ABI is the runtime's existing thunk contract, not a new one:
`HewVecElemCloneThunk = fn(src, dst) -> i32` (`hew-cabi/src/vec.rs:156`;
`hew_vec_push_owned` aborts on a non-zero status, vec.rs:2650-2657;
`hew_hashmap_clone_layout` likewise, hashmap.rs:985-987) and the in-place
drop thunk `fn(slot)`. A `void` copy glue would leave the status register
undefined; the glue always returns `i32`. **Leaves have glue too, as thin
wrappers**: `hew_copy$string` calls `hew_string_clone(src) → dst`,
`hew_drop$string` calls `hew_string_drop(*slot)`; same for `bytes`, `Rc`,
`Weak`, `LambdaPid`, closures, `dyn`. The wrapper is what collection
descriptors and envelopes receive (today codegen wraps ad hoc:
`hew_layout_string_drop`, `clone_layout_string_blob`, hashmap.rs:926,
1120-1122). A `Release`/`Retain` on a leaf **value** calls the leaf symbol
directly (`hew_string_drop`, not the wrapper); the wrapper exists only to give
every element type one thunk shape.

`mangle_resolved_ty` is `hew_hir::monomorph::mangle_resolved_ty`
(`monomorph.rs:360`), moved to `hew-types` beside `ValueClass` [P1] so hew-mir
does not depend on hew-hir for a name. Glue is keyed by `TypeInstanceKey`
(§6.2) in `MirModule.glue: BTreeMap<TypeInstanceKey, GlueDecl>`, emitted once
per module by codegen from `TargetLayout` and `GlueDecl`, `linkonce_odr` so
separately compiled modules dedupe.

**`GlueDecl` is where the leaf-symbol authority lives [decision].** §4.6 bans
any codegen predicate over `ResolvedTy` that chooses a retain, release or
symbol, and `TargetLayout` carries only size/align/field offsets/discriminant
encoding/taken bits — so the type→symbol mapping of §5.2 item 6 has to be
carried across the boundary, not re-derived. It is:

```
GlueDecl { drop_symbol, copy_symbol: Option<_>, abandon_symbol: Option<_>,
           body: GlueBody }
GlueBody = Record  { fields: Vec<(offset, GlueRef)> }        // reverse decl order
         | Enum    { variants: Vec<(tag, Vec<(offset, GlueRef)>)>, indirect: bool }
         | Elements{ elem: GlueRef, count: usize }           // tuple / array
         | Leaf    { release: RuntimeSymbol, retain: Option<RuntimeSymbol> }
         | UserClose { symbol, taken_bit: Option<offset> }   // #[resource]
GlueRef  = TypeInstanceKey                                   // resolves in MirModule.glue
```

MIR fills every `RuntimeSymbol` from `hew-mir/src/runtime_symbols.rs` while
resolving §5.2's item list; codegen walks `GlueBody` and emits calls, reading
no type. That is the consistent reading of §5.1, §5.2 item 6 and the §4.6 P5
gate `grep -c 'ResolvedTy::' hew-codegen-rs/src/*.rs` = 0, stated here so the
three are not read as contradictory.

**`hew-mir/src/runtime_symbols.rs` is the only symbol table** [decision, plan
§6], and it carries both halves of a symbol's row: the spelling and its
ownership. The FFI ownership TOML (`scripts/jit-symbol-classification.toml`) is
the source text an author edits and is **generated into** `runtime_symbols.rs`
[P1], the way `hew-types/build.rs:52` `generate_ffi_ownership_table` already
generates `ffi_contracts::FFI_OWNERSHIP_CONTRACTS` from it (`:60-118`) — never
a second table beside it and never a second lookup for a consumer to choose
between. What that deletes is the *string-keyed* second authority in the same
file: `callee_ownership_contract` (`runtime_symbols.rs:411`) and its
`CalleeOwnershipContract { receiver, string_args, result }` verdicts join by
callee spelling and go with the legacy lowerer (§9; the A4 row of
`runtime-ownership-table.md` §3). A `GlueBody::Leaf`'s release is the generated
row's release symbol called with that row's binding, not a verdict looked up by
name. The current `__hew_record_drop_inplace_<R>` /
`__hew_enum_drop_inplace_<E>` / `__hew_record_clone_inplace_<R>` synthesized
in codegen (llvm.rs:8050, 9061) are replaced by these [P2]; `DropFnSpec {
Runtime, Release, InPlace, UserClose }` (`model.rs:7321`) and `DropKind`
(`model.rs:7418`) are deleted [P5] — the glue symbol is the only release
authority MIR carries, and `Release { place }` selects it by the place's type.

### 5.2 Recursion over `TargetLayout`

`hew_drop$T` body, generated by one function `emit_drop_glue(layout)`:

1. Records: for each field in **reverse declaration order** (spec §3.7.3
   LIFO), if the field's class is owning, call its leaf release or
   `hew_drop$F`. For an **actor state record** an `AffineResource` field's
   release is guarded by its taken bit (§1.3.6): `if !taken { close }`. This
   is the only conditional in any glue body. It is one of **two** runtime
   readers of the bit; the other is the bit-guarded `store.assign` sequence of
   §1.3.6, which also *writes* it (set before the release, cleared by the
   store). Nothing else reads it: a `load.*`/`begin_borrow` on `Uninit`/`Maybe`
   is refused at compile time by rule 4, not at run time.
2. Enums: switch on the discriminant; per variant, release payload fields
   LIFO. Indirect (boxed) payloads: release the payload, then free the box.
3. Tuples/arrays: element-wise, last element first.
4. `#[resource]` records: call the user `close` (`LifecycleRegistry`
   `ResourceRecordLifecycle.close_declaration` → symbol, `Consume` header),
   then nothing — `close` consumed the value and released its fields.
5. Closures and `dyn Trait`: `hew_arc_drop` on the env/box; the arc's
   `drop_fn` is `hew_drop$<EnvRecord>` / `hew_drop$<Concrete>` (§5.4).
6. Leaves (called directly for values, through the §5.1 wrapper for
   elements): `string` → `hew_string_drop`, `bytes` → `hew_bytes_drop`,
   `Vec<T>` → **`hew_vec_free`, for every element class** [decision, plan §6]:
   the two spellings are one function — `hew_vec_free` (vec.rs:1593) and
   `hew_vec_free_owned` (vec.rs:2953) both forward to `free_vec_descriptor(v)`
   and nothing else — so the element-class split at the call site was never a
   behavioural choice, and the descriptor's `drop_fn` already decides what each
   slot's release is (§5.3). `hew_vec_free_owned` is deleted with the legacy
   emitter (§9); nothing this design emits names it. `HashMap<K,V>` →
   `hew_hashmap_free_layout`, `HashSet<T>`
   → `hew_hashset_free_layout`, `Rc<T>`/`Weak<T>` → `hew_rc_drop` /
   `hew_weak_drop_rc`, generators → `hew_gen_coro_destroy` (cont.rs:2058; a
   generator is a companion block `{coro handle, env, env-drop thunk,
   out-drop thunk, started, pending}` freed with `hew_cont_frame_free`, not an
   rc env), cancellation token → `hew_cancel_token_release`, opaque
   `#[resource]` → the registered `release_symbol` (`OpaqueResourceLifecycle`,
   `value_class.rs:53`), lambda actor handle → `hew_lambda_actor_release`
   (the last handle joins the dispatch thread, lambda_actor.rs:1461-1466),
   duplex → `hew_duplex_close` / `hew_duplex_close_half(dir)` with the
   direction as a typed argument of the descriptor, not a symbol comparison,
   reply channel → `hew_reply_channel_free`. `hew_task_free` is **not** a leaf:
   the scope frees tasks (§1.5).
7. A `Linear` field: **depends on the glue's role [decision]**. `emit_drop_glue`
   takes a role, `Release` or `Abandon`, and §5.1 publishes the second symbol
   `hew_abandon$T` only for a `T` with a `Linear` leaf.
   - In the **`Release`** role — every `destroy_value`, `store.assign`,
     `end_lifetime`, `hew_drop$State`, every collection element `drop_fn` — a
     `Linear` field is unreachable: a `Linear` value has no implicit
     destructor, a `Linear` capture into a *shared* env is refused (§1.3.5),
     and a `#[linear]` state field is refused at declaration (§1.3.6).
     `emit_drop_glue(Release)` meeting one is `E_MIR_ICE glue-linear`.
   - In the **`Abandon`** role a `Linear` field is legal and lowers to storage
     reclaim with no consuming call — spec §3.7.8.4 Path 4's "the value's
     storage is reclaimed by the runtime without invoking any consuming
     method". `hew_abandon$T` releases every non-`Linear` field per items 1–6
     and reclaims the `Linear` ones. It has exactly two registrants, both
     cases where the obligation was transferred to a receiver that never ran:
     the `drop_glue` of an envelope carrying a `Linear` message field (§5.6 —
     a coalesced, overflow-dropped or stop-drained message), and the arc
     `drop_fn` of a `fork`/`SpawnedCall` environment holding a `Linear`
     capture when the task is freed `Ready` (§1.5, §1.3.5).
   Without the role split the design would either ICE on a shipped program or
   leak the field: `tests/hew/actor_message_ownership_transfer_test.hew:29-46`
   declares `#[linear] type Ticket` and sends it to `receive fn take_ticket(t:
   Ticket)`, and spec §3.7.8.5 item 2 (line 3095) admits a `@linear` capture
   into a child task, so "a `Linear` value never reaches glue" is false and
   `Linear` types keep their send fact. §11 row 36.

`hew_copy$T` is the mirror: BitCopy fields are already in `dst` (the caller
memcpy'd), owning fields are retained/deep-copied in declaration order, and a
failed field copy rolls back the earlier ones and returns non-zero. Types with
`clone == None` get no `hew_copy$` symbol and their collection descriptors
carry `clone_fn = None` (§5.3); no SIR op can ask for the missing symbol
because 6b refused every `copy_value` of such a type, so a glue table lookup
that misses is `E_MIR_ICE` at module assembly.

### 5.3 Runtime element-glue protocol

The seam is `hew_arc_new(data, size, align, drop_fn: Option<unsafe extern "C"
fn(*mut u8)>)` (`hew-runtime/src/arc.rs:102`) and the element descriptor
`HewVecElemLayout { size, align, ownership_kind, clone_fn, drop_fn }`
(`hew-cabi/src/vec.rs:207`; `HewTypeOwnershipKind::LayoutManaged` is documented
"not implemented yet", vec.rs:21). [P2]:

- Every collection constructor takes an element descriptor whose `drop_fn =
  hew_drop$T` (the §5.1 wrapper for leaves) and `clone_fn = hew_copy$T`
  **when `clone ≠ None`, else `None`** (a `Vec<Conn>` descriptor has a drop
  thunk and no clone thunk; a clone through such a descriptor aborts
  fail-closed today — `hew_vec_clone_owned` checks `ownership_kind != Plain
  && clone_fn.is_none()` → `abort_owned_thunk_missing("clone")`, vec.rs:1724,
  and every clone-in path resolves through `owned_clone_fn`, vec.rs:2571 —
  and no program reaches it because 6b refused the `copy_value`),
  `ownership_kind = LayoutManaged` for every non-`BitCopy` element. The runtime's existing fail-closed checks stay and become the
  contract: `vec.rs:268` ("non-Plain ownership requires drop_fn"),
  `hashmap.rs:472-512` ("ownership_kind=String|LayoutManaged requires
  drop_fn"). `Plain` with `drop_fn = None` is legal only for `BitCopy`
  elements.
- Element release on `set`/`truncate`/`clear`/`free`/`remove` (the discarding
  `hew_hashmap_remove_layout`) / insert-overwrite is the runtime calling
  `drop_fn` (already the case for `String` kind: `drop_element_range`
  vec.rs:1530-1550, `hew_vec_set_ptr` vec.rs:2174-2178, hashmap.rs:1500-1516).
  **`pop`, `Vec.remove(i)` and `HashMap.remove(k)` are move-outs, not
  releases** [correction]: they byte-copy the element into the caller's `out`
  and run **no** `drop_fn` — vec.rs:2516 "`pop` — memcpy the last slot to
  `out` (move out), NO drop"; `hew_vec_remove_at_owned`
  (vec.rs:2130-2157, resolving `owned_descriptor(v)`) "Move the removed element
  OUT into `out` before the shift. No drop — possession transfers to the
  caller" at vec.rs:2145-2146 — **not** `hew_vec_remove_at_layout`
  (vec.rs:1963), which opens with `validate_bitcopy_layout_operation(v, layout)`
  (vec.rs:1971) and therefore `abort_layout_aware_operation()`s on exactly the
  `LayoutManaged` vec this section is about (vec.rs:285-301 requires
  `elem_kind == ElemKind::Plain` and `ownership_kind == Plain`); `hew_hashmap_remove_take_layout`
  hashmap.rs:1527-1545 moves the value out and drops only the key. In SIR each
  is a `Call` with `RetMode::Fresh` producing a new `Owned` value (§1.3.1), and
  the collection's obligation for that element ends with the move. Treating
  them as `drop_fn` sites, as an earlier draft of this section did, would close
  the element and hand the caller the same bytes — a double free for
  `Vec<Conn>.pop()`, a use-after-free for `Vec<Rc<T>>`.
  Element retain on `get_clone`/`clone` is `clone_fn`. **Ingress is a move,
  never a clone**: `push` lowers to `hew_vec_push_owned_move` (vec.rs:2663,
  byte-move, source dead; the collection itself is passed by pointer and
  borrowed), and SIR emits a `copy_value` before the push when the source
  stays live, so the pairing is `move ↔ push_owned_move` always;
  `hew_vec_push_owned` (clone-in, vec.rs:2634) is deleted [P5].
- **HashMap/HashSet insert consumes both operands unconditionally.**
  [current] `hew_hashmap_insert_layout` consumes the caller's key only on the
  vacant path and leaves the duplicate key to a codegen conditional release
  on the `i1` result (hashmap.rs:1245-1262 → `emit_insert_overwrite_key_release`,
  layout.rs:4822; `hew_hashset_insert_layout` forwards, hashset.rs:314-324).
  Rule 1 forbids a consume that depends on a runtime branch, so [P2] adds
  `hew_hashmap_insert_layout_move` / `hew_hashset_insert_layout_move`, which
  release the duplicate key through `key_layout.drop_fn` on the existed path
  and consume it on the vacant path; SIR `insert` is `move` of key and value
  to that entry. SHORTCUT: two entry points coexist during the parity window.
  WHY: the legacy route keeps emitting the conditional release. WHEN: P5,
  when the legacy emitter is deleted. WHAT: the `_move` variant is renamed to
  the plain symbol and the `i1`-branch protocol is deleted.
- `hew_vec_new_str`, `hew_vec_new_ptr`, `hew_vec_new_generic(elem_kind)`,
  `hew_vec_closure_pair_drop_inplace` and every kind-specialized constructor
  are deleted [P5]; one `hew_vec_new_with_elem_layout` remains.
- `Unsupported(NoReleaseProtocol)` and `Unsupported(UnknownValueClass)`
  (`hew-mir/src/ownership.rs`) stop existing as states [P2]: a type with a
  class has glue, by construction.

Test: `hew-runtime` unit test per collection op asserting, for a
`LayoutManaged` descriptor, that the `drop_fn` call count equals the
released-element count for the releasing ops (`set`, `truncate`, `clear`,
`free`, discarding `remove`, insert-overwrite) and is **zero** for the
move-out ops (`pop`, `Vec.remove`, `hew_hashmap_remove_take_layout` — whose
counterfactual is a `Vec<Conn>.pop()` fixture asserting exactly one `close`,
run by the caller at its scope exit); plus an
insert-overwrite test asserting exactly one key release; ASan leak oracle over
`tests/ownership-balance` clean rows extended with `Vec<Record{string}>`,
`HashMap<string, Vec<string>>`, `Vec<Conn>` (close count = element count),
`Vec<Rc<i64>>` with a `Weak` probe, and a `HashMap<string, i64>`
double-insert fixture [P2].

### 5.4 Mapping per surface type

| Hew type | class | `clone` | `copy_value` | `destroy_value` | `fork` |
| --- | --- | --- | --- | --- | --- |
| `string` | CowValue | Retain | `hew_string_clone` (refcount +1) | `hew_string_drop` | never emitted (immutable; no `var self` method in `std/string.hew`) |
| `bytes` | CowValue | Retain | `hew_bytes_clone_ref` — `clone b` becomes legal through this path (§11 row 9) | `hew_bytes_drop` | no-op: every bytes mutator forks internally and rewrites the triple (bytes.rs:181-205, 435-440) |
| `Vec<T>`, `HashMap<K,V>`, `HashSet<T>` with `BitCopy`/`CowValue`/`PersistentShare` elements | CowValue | DeepCopy / FieldWise | `hew_vec_clone_owned` / `hew_hashmap_clone_layout` / `hew_hashset_clone_layout` (deep, §5.5; elements through the descriptor `clone_fn`) | `hew_vec_free` / `hew_*_free_layout` (call element `drop_fn`; §5.2 item 6) | no-op (§5.5) |
| `Vec<T>` etc. with an `AffineResource` element (`Vec<Conn>`, `Vec<Rc<i64>>`, `Vec<Sender<T>>`) | AffineResource | `None` for `Conn`/`Sender`; `FieldWise` for `Rc`/`Weak`/`LambdaPid` elements | rejected (6b) / `hew_vec_clone_owned` with `clone_fn = hew_copy$Rc` (element retain) | `hew_vec_free` (element `drop_fn` closes each element; never sunk, §3) | emitted for a mutating receiver (`v.push(c)`) and realized as a register move — unique by class, no `ensure_unique` now or after §5.5 retires |
| `VecIter<T>`, `HashMapIter<K,V>` | as the collection field | as the field | `hew_copy$T` (field-wise) | `hew_drop$T` | no-op |
| record / enum / tuple with heap, `Option`/`Result` with heap payload, `CrashInfo`, `indirect` enums | aggregate rule | field-wise (`indirect`: fresh box) | `hew_copy$T` | `hew_drop$T` | no-op (inline aggregate is unique; a field write releases the old field) |
| record / enum / tuple all-BitCopy, `CrashNotification`, `DownNotification`, error enums | BitCopy | Bits | bits | none | none |
| closure (`Function`; `Closure` with all-`BitCopy`/`CowValue`/`PersistentShare` captures) | PersistentShare | Retain | `hew_arc_clone` on the env (null env → no-op) | `hew_arc_drop` (arc `drop_fn = hew_drop$Env`) | never (the env is a place: a `BorrowMut` write is `store.assign` on its field, §1.3.5; the closure's send fact is false in that case) |
| `Closure` with an `AffineResource` capture (`move \|\| { conn.fd }`, an `Rc` capture) | AffineResource (§1.1 aggregate rule over captures) | Retain | `hew_arc_clone` on the env — the capture is shared, not duplicated, exactly as `Rc<T>` | `hew_arc_drop`; the last handle runs `hew_drop$Env`, which closes the captured resource | never |
| `dyn Trait` (`TraitObject`, `Iterator`) | PersistentShare | Retain | `hew_arc_clone` on the box | `hew_arc_drop` (arc `drop_fn = hew_drop$<Concrete>`); the vtable's `drop_in_place` slot 0 is the same glue | never; a mutating method through `CallDynMethod` is not on the surface (`VarSelfMethodCall` requires a concrete `var` receiver) — if P3 finds the checker admits one, it is a wall `E_OWN_MUTATE_SHARED` decided then, not an ICE |
| `LocalPid<A>`, `RemotePid`, `ChildRef`, `HewActor` | BitCopy | Bits | bits (non-owning pid; `marker()` flips to `BitCopy` at P1) | none | none |
| `BoxedActor` (compiler-internal) | AffineResource | None | rejected (6b) | release symbol named at P4 or the variant deleted (§1.1) | none |
| `LambdaPid`, `LambdaActorHandle` | AffineResource | Retain | `hew_lambda_actor_clone` (new handle into `dst`); a send is `Transfer` regardless (rule 5) | `hew_lambda_actor_release` | none |
| `Generator`, `AsyncGenerator` | AffineResource | None | rejected (6b) | `hew_gen_coro_destroy` | none |
| `Rc<T>` / `Weak<T>` | AffineResource | Retain | `hew_rc_clone` / `hew_weak_clone_rc`; `hew_rc_new` receives `hew_drop$T` as its payload `drop_fn` (rc.rs:103-107) | `hew_rc_drop` / `hew_weak_drop_rc` | none |
| `#[resource] T` (record) | AffineResource | None | rejected (6b) | user `close` (consumes) | none |
| opaque `#[resource]` (std handles), `MonitorRef`, `CancellationToken` | AffineResource | None | rejected (6b) | `release_symbol` from `LifecycleRegistry` / `hew_cancel_token_release` | none |
| `Duplex`, halves, `Sender`/`Receiver`, `Stream`/`Sink`, `StreamPair`, regex `Pattern` | AffineResource | None | rejected (6b) | `hew_duplex_close(_half)`, channel/stream close symbols, `hew_reply_channel_free`; `StreamPair`/`Pattern` via their declared `close(consuming self)` (`hew_stream_pair_free`, …) | none |
| `#[linear] T` | Linear | None | rejected (6b) | rejected on a normal exit, and on a cancel exit with no `defer` consumer (6d); consumed by a declared consuming method; storage-only on unwind, and in the `abandon` glue role of an undispatched message or a never-started task env (§5.2 item 7) | none |
| `ActorState`, `MachineState` (the reserved names) | never a value type | — | — | — | — |
| `Task<T>` (bound handle only, §1.5) | Linear | None | rejected (6b) | rejected on a normal exit (6d); **legal and code-free on a cancel exit** (6d's `Task` exemption: the scope owns the task); consumed by `AwaitTask` or a select `TaskAwait` arm (`move` into the `Suspend`); the scope frees the task and its unconsumed result through `result_drop_fn` | none |
| actor state field (`let` or `var`) | place (§1.3.6) | — | `load.copy` | `end_lifetime` at actor stop (`hew_actor_set_state_drop` receives `hew_drop$State`, which honours the taken bit of a resource field) | `load.take` → `fork` → `store.init` for a mutating collection/bytes call on a `let` **or** `var` field; `store.assign` only on a `var` field (6a) |

**`PersistentShare` is atomic.** Closures and `dyn` boxes are allocated with
`hew_arc_new` and refcounted with `hew_arc_clone`/`hew_arc_drop`
(`arc.rs:156, 183`), never the non-atomic `hew_rc_*` (rc.rs:1-4 "NOT `Send` —
cannot cross actor boundaries"). A closure whose captures are all `Send`,
none `BorrowMut`, and none `AffineResource`/`Linear` is `Send` and `Share`able
(§1.3.5, §1.1: an `AffineResource`-classed closure is `Transfer` only by rule
5, so it never has two live handles in two actors) and may be retained into a
lambda-actor environment or a spawn
environment that runs on another OS thread (`lambda_actor.rs:1461-1466`
dispatch thread; `thunks.rs:686-760`
`hew_task_spawn_thread_with_inherited_context`), so a non-atomic count would be
touched from two threads. `hew_rc_*` is reserved for `Rc<T>`/`Weak<T>`, whose
send fact is false by construction — and, because the `Rc` can hide inside a
`dyn`'s concrete payload, by the `CoerceToDynTrait` send wall of §1.1 as well:
a `dyn … + Send` whose concrete is not `Send` is refused at the coercion, so no
`hew_arc_*` share ever reaches a second actor holding an `hew_rc_*` count. **[current] closures are not refcounted at
all**: a closure env is a unique-owner `hew_dyn_box_alloc` box
(`ClosureEnvMode::HeapBox`, model.rs:7668-7700) freed exactly once by the last
owner of the pair, and `let g = f` moves (`repros/ladder/closure_rebind.hew`
→ `E_MIR_CHECK … used after it was consumed`); spawn envs are `hew_rc_new`'d
(thunks.rs:690) and released by `hew_task_free → hew_rc_drop`
(task_scope.rs:683-686); `dyn` boxes are `hew_dyn_box_alloc` with `clone`
refused (llvm.rs:8541-8544, trait_object.rs:426). This design is a **change**
(§11 row 7): `PersistentShare` values become shareable (`let g = f` then use
both), with the runtime changes [P3 closures/dyn, P4 task envs]: env/box
allocation through `hew_arc_new` with the glue as `drop_fn`, `hew_task_set_env`
documented as arc, `hew_task_free` releasing a never-started task's env with
`hew_arc_drop`, and `hew_task_take_env`/`hew_arc_release_storage` for the
started body (§1.3.5). The `hew_dyn_box_alloc` closure/dyn boxes and
`ClosureEnvMode::HeapBox` are deleted at P5 (§9).

### 5.5 Marked shortcut: collections are not refcounted at runtime

WHY: `HewVec`/`HewLayoutHashMap` have no arc header; `hew_vec_clone_owned` is
a deep copy (`hew-runtime/src/vec.rs:2966`), and `refcount` in that file refers
only to `String` elements. `docs/v05/ownership.md:139-141` records the same:
mutable collections are "deep-copied … today, converging to
retain-share-plus-copy-on-write". So `copy_value : Vec<T>` lowers to a deep
clone and `fork` is a no-op; semantics are identical to COW (the copy is
already unique), cost is not.
WHEN-OBSOLETE: when `HewVec`/`HewLayoutHashMap`/`HewLayoutHashSet` are
allocated through `hew_arc_new` with `hew_drop$` element glue (a runtime
change, v0.7 per the lifecycle table).
WHAT: `Retain` on a collection carrier becomes an arc retain and `Fork` becomes
`ensure_unique` (refcount > 1 → clone); no SIR, verifier, or MIR change — only
the row in the runtime symbol table.

### 5.6 Message payload protocol: envelope-only

**[current]** The runtime has two delivery protocols: copy mode, where the node
buffer is `libc::free`d with no payload drop and the handler owns the fields
(`hew_msg_node_free`, mailbox.rs:1093-1113, `envelope.is_null()` arm), and
envelope mode, where `hew_msg_envelope_release` runs the envelope's
`drop_glue` on the payload after dispatch and the handler borrows
(`cow_envelope.rs:102-120`; dispatch trampoline `thunks.rs:4018-4035`,
`borrow_mode` 0 = copy mode, handler owns). Undelivered, coalesced,
overflow-dropped and drained copy-mode messages are released through a
per-actor `HewMessageDropFn(msg_type, data, size)` switch (mailbox.rs:416;
`hew_actor_set_message_drop`, actor.rs:5489-5510) that codegen synthesizes per
actor (`emit_actor_message_drop_fn`, thunks.rs:3302-3400). Which handler
locals are envelope-borrowed views is decided by codegen's borrow-taint
fixpoint (llvm.rs:22270-22740). Copy mode **double-closes a kept resource**:
`repros/ladder/resource_keep.hew` (`receive fn take(c: Conn) { conn =
Some(c) }` then `await h.show()`) prints `kept`, `close 7`, `fd 7`, `close 7`.
Asks are copy-mode only: `hew_actor_ask_with_channel` (actor.rs:6351-6365)
calls `actor_send_result_internal_reply(actor, msg_type, data, size, ch)`;
the aliased send (`hew_mailbox_send_aliased(mb, msg_type, envelope)`,
mailbox.rs:2652-2656) takes no reply channel and allocates its node with
`reply_channel = null` (mailbox.rs:2408). An envelope send never
key-coalesces: `HewOverflowPolicy::Coalesce` on the aliased path applies the
*fallback* policy (mailbox.rs:2506-2510 "Envelope payloads are opaque
refcounted buffers and cannot be byte-coalesced in place"), and the copy-mode
`replace_node_payload` (1815-1852) converts an envelope node into a byte
buffer with `envelope = null`; `coalesce_message_key` (1782) is
envelope-aware but called only from the copy-mode path (2081/2096) and 4515.

[P4] **One protocol.** Every message is an envelope whose `drop_glue` is
`hew_drop$<MsgRecord>` (the per-handler message record instance, §5.1),
created by `PrepareEnvelope` (§4.2) into a malloc-compatible payload. The
envelope owns the payload from the send on: a `Share` argument was retained
into it, a `DeepCopy` argument was copied into it, a `Transfer` argument was
moved into it. A message that is never dispatched (coalesce replacement,
overflow drop, drain at stop) is released by `hew_msg_envelope_release`, so
no per-actor drop switch exists. **Dispatch has one disposition: the handler
takes the payload** [decision] — there is no borrowed-payload shim and no
per-handler disposition field. The trampoline calls
`hew_msg_envelope_take_payload(env) -> ptr` (new: returns the payload pointer,
nulls `payload` and `drop_glue`, and **aborts if `refcount != 1`**); the
`ActorHandler` shim's payload header slots are `Consume`; the shim
`destructure`s the payload record into body-owned values and frees the buffer
with `hew_msg_payload_free(ptr)` (`libc::free`); the body owns each field like
a local (`conn = Some(c)` is a `move` into `store.assign`; a field the body
never uses is `destroy_value`d at the shim's exit; a field it keeps needs no
`copy_value` at all). `release` runs `drop_fn` only when `payload` is non-null
(cow_envelope.rs:114-118), so `hew_msg_node_free`'s later
`hew_msg_envelope_release` frees nothing but the envelope shell. This fixes
the copy-mode double close above (§11 row 24, behaviour fix).

**Why one disposition and not two.** A borrowed disposition is unsound for any
handler that suspends, and the compiler cannot restrict it to handlers that do
not without deriving a header fact from the body (§4.2 forbids that). The
runtime order is decisive: on the dispatch-return edge the scheduler runs
`hew_msg_node_free(msg)` **unconditionally** (`hew-runtime/src/scheduler.rs:3766`)
and only afterwards parks the suspended continuation
(`if !suspend_handle.is_null() { … park_suspended_activation(actor, suspend_handle) }`,
scheduler.rs:3780-3788); `hew_msg_node_free`'s envelope arm calls
`hew_msg_envelope_release` (mailbox.rs:1106-1110), whose final observer runs
`drop_fn((*env).payload)` and then `libc::free((*env).payload)`
(cow_envelope.rs:113-121). A handler that parks and then reads a payload field
after resume would therefore read freed memory. That is not a hypothetical
shape: `tests/vertical-slice/accept/coalesce_owned_payload_leak.hew:13-17` is a
shipped ASan/leak-oracle fixture whose `receive fn update(id: i64, payload:
string, pause: duration)` calls `sleep(pause)` and then reads `payload.len()`.
Today the shape is safe only because copy mode hands the handler owned copies
and envelope-mode dispatch is hard-refused (`scheduler.rs:3354-3396`, the
`hew_panic` guard "refusing to double-drop"); §5.6 removes that guard and the
copy path together, so the borrowed disposition would have removed the only
thing keeping it safe. The taken disposition costs nothing: a `Share`
argument's retain was taken at `PrepareEnvelope` and is *moved* to the handler
rather than duplicated, so the release count is identical.

**The `refcount == 1` precondition holds by construction.** The only minter of
a second observer is `cow_envelope::clone_alias` (cow_envelope.rs:87-99),
reachable only through the C-ABI entries `hew_msg_envelope_clone_alias`
(mailbox.rs:557, mailbox_wasm.rs:284) — both deleted by §9 — with no other
caller in `hew-runtime/src` outside that module's own tests
(`grep -rn 'clone_alias' hew-runtime/src`). A queued node holds exactly one
reference, so a delivered envelope has one observer; a count above one is an
ICE, never a race, and the abort is the §1.3.5 `hew_arc_release_storage`
pattern.

`Linear` message fields are 6d in the handler body like any local; the
envelope of a message record with a `Linear` leaf carries
`hew_abandon$<MsgRecord>` as its `drop_glue` (§5.2 item 7) so a coalesced,
overflow-dropped or stop-drained delivery reclaims the field's storage instead
of ICE-ing or leaking. `PrepareEnvelope` sets
`HEW_MSG_ENVELOPE_CAPABILITY_TRANSFER` (`mailbox_header.rs:17-18`, "payload is
a capability transfer; aliasing is forbidden") on a payload carrying a
`Transfer` argument. **That bit is descriptive, not enforcing** [correction]:
`header_validate` (mailbox_header.rs:154-162) asserts only
`bits & HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK == 0` and never inspects
`CAPABILITY_TRANSFER` or `ALIAS_ACTIVE`, and it has exactly one production
caller — the `if prev == 1` final-observer branch of `release`
(cow_envelope.rs:116). `CAPABILITY_TRANSFER` itself has no production reader at
all on `main` (`grep -rn CAPABILITY_TRANSFER hew-runtime/src hew-codegen-rs/src`
finds only the header constant, the wasm/native mirrors and test assertions).
There is therefore **one** enforcement mechanism, not two: the `refcount != 1`
abort in `hew_msg_envelope_take_payload`. The bit stays as the wire-visible
record of the contract, which is what makes an aliasing attempt legible in a
dump; it is not a check and the doc must not be read as promising one.

`mailbox coalesce` keeps replacement-by-key: [P4] `send_aliased_with_overflow`
runs `coalesce_message_key` over the envelope payload (the function already
reads `envelope` when `data` is null) and replaces a queued node by
`hew_msg_envelope_release(old); node.envelope = new` — no byte buffer, no
`replace_node_payload` copy path (deleted with copy mode). Fixture: a bounded
`coalesce` mailbox receiving two string-carrying messages with one key
asserts one delivery and one release of the replaced payload. Asks: [P4]
`hew_mailbox_send_aliased_with_reply(mb, msg_type, envelope, reply_channel)`
(new: `msg_node_alloc_aliased` already takes the channel as its third
argument, mailbox.rs:748/2408) replaces the copy-mode
`actor_send_result_internal_reply` under `hew_actor_ask_with_channel`;
`hew_msg_node_free` already retires a queued ask's sender ref for every node
(`retire_msg_node_ask_sender_ref`, mailbox.rs:1102). The handler's reply is an
`Owned` result moved into the reply channel; the channel carries
`hew_drop$<Reply>` (today `ask_reply_drop_thunk_ptr`, thunks.rs:433) so a
loser's or a timed-out reply is released by the runtime (§1.5). Copy mode,
the `borrow_mode` trampoline parameter, `HewMessageDropFn`,
`hew_actor_set_message_drop`, `emit_actor_message_drop_fn`, the
borrow-taint fixpoints, and `replace_node_payload`'s byte path are deleted
(§9). New symbols (`hew_msg_envelope_take_payload`, `hew_msg_payload_free`,
`hew_mailbox_send_aliased_with_reply`) are rows in `runtime_symbols.rs` [P4].

Lifecycle hooks (`HirLifecycleHook`, node.rs:620-660): `Crash` receives
`CrashInfo` (`CowValue`: `message: string`), `Exit` receives
`CrashNotification` (`BitCopy`), `Down` receives `DownNotification`
(`BitCopy`). The `LifecycleHook` header slot is `Borrow`. For `Crash` the
runtime passes `(code, message)` and keeps ownership of the message
(`invoke_on_crash_handler`, supervisor.rs:3325-3375: the supervisor allocates
it header-aware and frees it after the call); the synthesized hook shim
(§6.5) mints an `Owned CrashInfo` by `copy_value` of the message
(`hew_string_clone`), the user body borrows it, and the shim `destroy_value`s
it (`hew_drop$CrashInfo`) before returning — the protocol the runtime
documents today, expressed as SIR ops instead of a codegen prologue.
BitCopy payloads need nothing.

Test: an ASan fixture per path — a dispatched message whose `string` field is
kept in state (exactly one release, at actor stop), a dispatched message
keeping a `Conn` in state (exactly one `close`, at actor stop), a message with
an unused `Conn` field (one `close` at dispatch), a **suspending** handler that
reads a payload field after resume — `tests/vertical-slice/accept/coalesce_owned_payload_leak.hew`
is that fixture and is the oracle for the single disposition — coalesced-away
message (by key), overflow-dropped message, drained message at stop, ask
through the aliased entry, select with a losing ask arm — each asserting zero
leaks and exactly one release [P4].

### 5.7 Supervisor child templates and restart

**[current]** Two restart models coexist. A **config-init child** (any child
whose init args read `config.field`) carries a per-child init thunk:
`HewChildInitFn` (supervisor.rs:672, `fn(config) -> HewChildInitResult`),
registered by codegen through `hew_supervisor_set_child_init_fn` when
`has_config_field` (suspend.rs:8686-8702), and "THE source of the child's
actor state on the initial spawn AND every restart, REPLACING the byte-copy
state template" (supervisor.rs:530-548; `restart_child_from_spec` calls
`init_fn(config)` at 2731-2735) — every init-arg expression is re-evaluated
per restart. A child with no config dependence takes the **template path**:
`HirSupervisorChild.init_args` (node.rs:927-960) builds a state template
once; restart deep-copies it through `state_clone_fn` (supervisor.rs:2760-
2770 "call the codegen-emitted clone fn"; `hew_actor_set_state_clone`,
actor.rs:5531; `HewStateCloneFn = fn(*const) -> *mut` returning a fresh
malloc'd wrapper, actor.rs:1198) or, with none registered, the "Legacy
byte-copy path" (2810). The old §5.7 described only the template path and
mis-stated main.

[P4, decision] **One model: the init thunk, for every child.** The
supervisor bootstrap (`SupervisorBootstrap`, §6.5) synthesizes per child a
`ChildInit` producer — an ordinary SIR function `(config: Borrow) -> Fresh
State` whose body is the child's `init_args` expressions followed by the
actor's `ActorInit` (§6.5) — and registers it as the child's
`HewChildInitFn`; the supervisor owns its config buffer (a runtime-owned
place the thunk borrows) and frees it once at supervisor stop
(supervisor.rs:555). Every spawn and restart runs the thunk; the state it
returns is `Fresh` and independently owned, so no template exists, no clone
of one is needed, and `hew_copy$State` has no supervisor consumer.
Consequences: `hew_actor_set_state_clone`, `HewStateCloneFn`,
`state_clone_fn`, the template deep-copy in `add_child_spec`
and the byte-copy path (2810), `StateFieldCloneKind` and `state_clone.rs`
are deleted (§9) — the F28 ABI mismatch (`fn(*const) -> *mut` versus the
glue's `(src, dst) -> i32`) disappears with the ABI. `hew_drop$State` is
registered through `hew_actor_set_state_drop`.

**`state_clone_fn` has three production readers besides restart, and deleting
it changes an admission gate [decision].** Revision 5's "no other reader"
claim is **withdrawn as false**: `scheduler.rs:3315-3324` (dispatch entry),
`scheduler.rs:2442-2450` (resumed dispatch) and `scheduler_wasm.rs:2034-2038`
all read the pair

```
match (a.state_clone_fn, a.state_drop_fn) {
    (Some(_), Some(drop)) => Some(drop),
    (None, None)          => None,
    _ => { /* "half-registered clone/drop classifier proof" */ abort() }
}
```

and feed the result to `crate::cont::begin_dispatch_crash_cleanup(a.state,
a.state_size, crash_state_drop)`, which snapshots the state only when
`state_drop.is_some()` (cont.rs:1240-1244). The comment at scheduler.rs:3317-3321
names the pair "the relocation proof for byte-escrowing this state", and
codegen registers the two symbols strictly together — `resolve_state_clone_drop_symbols`
fails closed on a half-populated pair (llvm.rs:5161-5173) and
`emit_actor_state_clone_drop_registration` emits `hew_actor_set_state_drop`
then `hew_actor_set_state_clone` (llvm.rs:5202-5214). Dropping only
`hew_actor_set_state_clone` would leave every owned-state actor at
`(None, Some(drop))` and `process::abort()` on **every dispatch**. So the
deletion carries two replacements:

- **The relocation proof becomes structural.** `TargetLayout` (§4.2) has no
  representation for an interior pointer — a state record is size, align,
  field offsets, discriminant encoding and trailing taken bits, and every
  owning field is a pointer to a *separate* allocation — so every actor state
  is byte-relocatable by construction. The pair test collapses to
  `a.state_drop_fn` alone: `begin_dispatch_crash_cleanup` is admitted exactly
  when a typed state drop is registered.
- **`state_drop_borrowed` goes with it.** The bit exists only to express
  "drop registered without clone" for the shallow-template restart path
  (`mark_state_drop_borrowed`, actor.rs:1641-1650, whose only production
  caller is the supervisor byte-copy leg; `mark_state_drop_owned`,
  actor.rs:1665-1676, flips it back after the template deep-clone). With the
  `ChildInit` thunk every incarnation's state is `Fresh` and independently
  owned, so borrowed provenance is unrepresentable and the reads at
  actor.rs:2599 / 2739 and the three scheduler sites lose their guard. The
  runtime comment at supervisor.rs:2826-2830 already names this change as the
  WHEN-obsolete for the whole mechanism ("when the v0.6 init-closure restart
  model lands").

**And on a `StructuredLlvm` target the escrow is not armed at all [P4
decision].** `begin_dispatch_crash_cleanup` exists because "the LLVM landing
pads have already destroyed ordinary Hew locals. This compatibility escrow
handles actor-state writes until state itself is represented as an OSSA
owner" (scheduler.rs:3618-3620). Under §1.3.6 state fields *are* OSSA places
with taken bits and the landingpad runs the SIR unwind block (§4.7), so on
linux/macOS/FreeBSD the escrow's `state_size` byte copy per dispatch buys
nothing and is removed (`#[cfg]` on the runtime build, which is per-triple).
It stays armed on the `CrashOwnerRegistry` target (windows-msvc), where the
snapshot **is** the release authority and §4.3's per-field bracket keeps it
current. `wasm32` traps end the instance, so its arm is unreachable cleanup
(§4.7) and follows the registry target's code path unchanged. Behaviour: a non-config child
whose init arg has a side effect (`Conn.open()`) is evaluated once on `main`
and per restart under this design — §11 row 25 (behaviour); config-init
children are unchanged. Test: ASan supervisor restart fixture with a string
init arg and a side-effecting init arg counting evaluations [P4].

### 5.8 Machine emit queue

`HirExprKind::MachineEmit { fields }` / `MachineTakeEmits` (node.rs:2300-2338).
**[current]** only unit events lower: `Instr::MachineEmitPlaceholder` passes a
null payload to `hew_machine_emit_push` and non-unit payloads fail closed
(llvm.rs:15545-15578 SHIM; model.rs:6405-6416), so no heap payload escapes
into the thread-local queue today (machine_emit.rs:359-361 describes a
borrowed payload the runtime never dereferences). [P4, with D287 desugar]: a
payload is `move`d into the queue; the queue owns it and releases it through
`hew_drop$<EventEnum>` when `take_emits` removes it or the queue is dropped, so
the borrow-that-must-outlive-the-frame described in machine_emit.rs never
exists. Unit events stay payload-free.

---

## 6. Identity and facts crossing the ladder

### 6.1 Callable identity

`MirCallableKey { declaration: DefId, instance: MirCallableInstance }`
(`hew-mir/src/identity.rs:58`) is the only cross-stage key (PR #3206 did the
stage-key cutover; it survives as the key of the single form). **[current]**
`declaration` is reconstructed from a dotted path by
`DefId::legacy_reconstruct_from_full_path` (hew-hir 6 + dispatch.rs 18 +
value_class.rs 2 + verify.rs 1; hew-mir ~52; llvm.rs 4;
`resolved_ty.rs:56,324` free mints). `DefId` is a `String` newtype
(`hew-types/src/lib.rs:141`), so mint-vs-reconstruct is API discipline only.

[P1] The frozen `ir-identity-declaration-authority` worktree's design is
salvaged as the minting authority: `IdentityTable::declare(occurrence,
canonical_path) -> DefId` (idempotent on identical claim, fail-closed on a
contradicted axis), `freeze() -> IdentityView` published in
`TypeCheckOutput.identity`, `DeclarationKind` closed set, and `declaration:
DefId` on every HIR declaration node — including `HirActorDecl`,
`HirSupervisorDecl`, `HirActorReceiveFn`, `HirActorMethod`, lifecycle hooks,
machine states/events/transitions (today only `ExternFn`, `MachineDecl`,
`TypeDecl`, `Fn` carry one, node.rs:353/715/1021/1197). Known landing
blockers from the ground map, all P1: `hew-analysis/src/signature_help.rs:189,210`
(`root_fn_identity` deleted), `hew-cli/src/machine.rs:73-84` (must run the
checker instead of `TypeCheckOutput::default()`), hew-mir/hew-hir test struct
literals, the double-mint question for flattened file imports (settled by a
test asserting one `DefId` for an item present in both surfaces). The 18
`dispatch.rs` primitive reconstructions become `IdentityView` lookups of
`lang_items`; `resolved_ty.rs:324 nominal_instance` takes the `NominalId` from
the checker's type instance table. `legacy_reconstruct_from_full_path` is
deleted from `hew-types` at P5; P1 leaves it callable only from
`hew-mir/src/lower/**` (`#![allow(deprecated)]` scoped there and nowhere else).

Determinism: `ModuleGraph.modules` is a std `HashMap` and topo order seeds
from `keys()` (`hew-parser/src/module.rs:381`), the refuted source of
run-to-run diagnostic order drift. [P1] every map that feeds emission order,
dumps, or the coverage ratchet is a `BTreeMap`/sorted `Vec`; ast-grep rule
`no-hashmap-in-emission-order` over `hew-parser/src/module.rs`, `hew-sir`,
`hew-mir/src/model.rs`.

### 6.2 Generic instance service

One service at the HIR→SIR boundary [P2], grown from `hew-sir/src/lower.rs`
`InstanceService` (L336) and `SirInstanceKey` (model.rs):

```
InstanceKey     { item: DefId, type_args: Vec<ResolvedTy>, selected_impls: Vec<(DefId, NominalInstance)> }
TypeInstanceKey ( ResolvedTy )   // the canonical substituted type; structural equality
```

**`TypeInstanceKey` is structural, not nominal [decision].** The nominal
spelling `{ template: NominalId, type_args }` is **withdrawn**: it is not
total over the types §1.1 classes. `ResolvedTy::nominal_instance()`
(`resolved_ty.rs:308-329`) returns `Some` only for `Named { builtin: None }` —
"Builtins and abstract parameters have their own closed discriminators and
therefore do not produce a user nominal" — so `Tuple`, `Array`, `Slice`,
`Function`, `Closure`, `Pointer`, `Borrow`, `TraitObject` and `Task` have no
`NominalId` at all, while §1.1 is total over `ResolvedTy` and assigns every
one of them a class and a `clone`. With `ValueClass::of_ty` and `Ty::is_copy`
on the P1 deletion ledger there is no fallback, so a nominal key would force
every consumer of `type_facts` (§6.3), `TargetLayout` (§4.2) and
`MirModule.glue` (§5.1) to re-derive the fact for a `(string, i64)` tuple —
the second authority §1.1 exists to delete. The key is therefore the
canonical substituted `ResolvedTy` itself, compared structurally. It is not a
name: `mangle_resolved_ty` (§5.1) renders a *symbol* from the key and never
joins on one, so the one-authority tenet's ban on name-keyed joins holds.
Obligations [P1]: `ResolvedTy` derives `Ord`/`PartialOrd` (it has `Eq`/`Hash`
today, `resolved_ty.rs:75`) so every keyed table is a `BTreeMap` under §6.1's
determinism rule; canonicalization (import spellings resolved, no `TypeParam`,
no `Var`) happens once in the instance service and the SIR verifier rejects a
non-canonical key as `E_SIR_ICE`. Test: `type_facts` lookups for `(string,
i64)`, `[Conn; 3]`, `dyn Show`, a closure instance and `Task<i64>` all hit
[P1].

`selected_impls` is present only where two distinct implementations are legal
for the same type arguments. The service owns canonicalization, discovery
order (declaration order, then encounter order), caching, recursion (SCC:
an instance under construction is a resolved key, not a re-entry), and the
diagnostic chain. `hew-hir/src/monomorph.rs` `MonoKey` / `function_monomorph_symbol`
(monomorph.rs:57, 138) are what the *legacy* lowerer specializes with; they
are deleted with it [P5]. `SIR_GENERIC_INSTANCE_CAP = 1024` (lower.rs:317)
stays as a marked shortcut (WHY: no SCC-aware termination proof for
polymorphic recursion; WHEN: the service gains a per-SCC depth witness; WHAT:
the cap becomes a diagnostic naming the recursive chain). No MIR body lowerer
re-specializes: `MirCallableInstance::Polymorphic` is deleted [P5].

### 6.3 Capability facts on the class table

The checker decides once per substituted type and publishes
`TypeCheckOutput.type_facts: BTreeMap<TypeInstanceKey, TypeFacts { class:
ValueClass, clone: CloneKind { Bits, Retain, DeepCopy, FieldWise, None }, send:
SendFact, hash: bool, eq: bool }>` [P1], keyed by `TypeInstanceKey` (§6.2),
with `SendFact { Known(bool), DeferredToClosureFacts }`. For a
`Closure` the `send` fact is `all captures Send ∧ no capture has mode
BorrowMut ∧ the class is not AffineResource/Linear` (§1.3.5, §1.1) and it is
**not** a per-type fact. For a `TraitObject` it **is** a per-type fact — "the
bound list contains `Send`" — and stays sound only because §1.1's
`CoerceToDynTrait` wall refuses a coercion into a `+ Send` object whose
concrete is not `Send`; the wall is what turns the flat type into a proof. The claim that "the capture modes are part of the
closure's type instance" is **withdrawn as false**: `Ty::Closure { params,
ret, captures: Vec<Ty> }` (`hew-types/src/ty.rs:216-223`) and
`ResolvedTy::Closure` (`resolved_ty.rs:160-167`) carry capture **types**, the
marker walk iterates them as types (`traits.rs:1039-1046`), the mode lives on
`HirClosureCapture` (`node.rs:3183`), and captures are excluded from call-type
identity altogether (`resolved_ty.rs:1014-1019`). Two closures with identical
capture types and different modes are one type instance and need opposite
facts. **[decision]** the closure `send` fact is keyed by the closure's
**synthesized `ClosureInvokeShim` `DefId`** — §6.5 mints one per closure
expression, so the key already exists and is per-expression — and published
as `TypeCheckOutput.closure_facts: BTreeMap<DefId, ClosureFacts { send: bool
}>` beside `type_facts`. That is one authority for the fact, not a second
authority for the class: `class`, `clone`, `hash` and `eq` stay on
`type_facts`; only `send` for a `Closure` reads `closure_facts`, and rule 6c
names which table it read in its diagnostic. **The wrong reading is made
unrepresentable [decision]**: `send` is `SendFact`, not `bool`, and every
`Closure`-keyed row carries `SendFact::DeferredToClosureFacts`, so a consumer
that asks `type_facts[closure_key].send` for a yes/no gets a value it cannot
use without going to `closure_facts` — the compiler refuses the shortcut
instead of handing back the mode-agnostic answer §1.3.5 says is wrong for a
`BorrowMut`-capturing closure. This is the §1.1 treatment of
`ValueClass::Unknown` ("a user type is never unclassified") applied to the
second table: one authority by construction, not by convention. A
`TraitObject` keeps `SendFact::Known` — the bound list, made sound by the
`CoerceToDynTrait` wall of §1.1. Test: two closures over the same
capture types, one `Borrow` and one `BorrowMut`, get opposite `send` facts and
the `BorrowMut` one is `E_OWN_SEND_UNSUPPORTED` at a lambda-actor capture
[P1]. Consumers: SIR rules 5 and 6
(`send`, `clone`, class), `TargetLayout` (class), glue emission (`clone`),
collection descriptors (`hash`/`eq` thunks). Nothing downstream re-asks:
`hew-codegen-rs/src/layout.rs:2737` key `ownership_kind` walk,
`hew-mir/src/state_clone.rs:339 is_clone_total`, `traits.rs:921`
`register_drop_type`, `module_registry.rs:813-835 drop_func_for` are deleted
(§9). `Copy` is not a separate fact: the checker's `implements_marker(Copy)`
/ `Ty::is_copy` sites (13) read `class ∈ {BitCopy, View}` — `View` included so
extern `Pointer` types keep today's Copy verdict (`ty.rs:1452-1459` lists
`Ty::Pointer` as Copy; `of_ty` classes it `View`).

### 6.4 FFI / runtime ownership table (produced by another lane)

Every C-ABI symbol has a row in `scripts/jit-symbol-classification.toml`,
generated into `hew-mir/src/runtime_symbols.rs` — the one symbol table (§5.1) —
and projected to `hew_types::ffi_contracts::ExternOwnershipContract { params:
[Borrow | Consume | Retain], result: Fresh | Retained | Borrowed | None,
release_symbol, discharge_depth, result_retention }` (ffi_contracts.rs:10-88).
Every `extern fn` in `std/` declares its ownership on the signature: `consume`
parameter modifier (`std/fs.hew:536-537`, `std/string.hew:850`) and default
borrow. HIR→SIR reads the contract row to emit `move` (Consume) / borrow
(Borrow) / `copy_value` (Retain) per argument and to mint the result as
`Owned` (Fresh/Retained) or `View` (Borrowed). **A missing row is a build
error from P1** [decision, plan §1.5], scoped by what the phase lowers:
`ExternOwnershipFact::Absent` is rejected at HIR→SIR lowering **for every
symbol P1 lowers** — its own runtime-protocol callees, the leaf releases and
retains of §5.2 item 6 and §4.3 — and the `std/` `extern` declarations join the
same rule at **P3**, when the `consume` sweep and `make test-ffi-table` land.
This resolves the split `runtime-ownership-table.md` §8's P1-lowering row
raised: the emitter never ships with its own edges unguarded, and the phase that
owns a `std/` declaration is the phase that owes its row. Today the fact is
enforced nowhere — "an absent row is deliberately not a borrow" holds only for
resource params (ffi_contracts.rs:140-246) and `Absent` otherwise falls through
to defaults (`lower/facts.rs:1295-1302`, `runtime_call.rs:2025-2040`). The row
is also the **only** authority for
an `Extern` `CallableHeader`'s param modes (§4.2): the `.hew` `consume`
spelling is checked against it, never merged with it. Gate: `make
test-ffi-table` [P3] compiles every `std/**.hew` and fails on the first
`Absent` **and on the first disagreement** between a declaration's
`consuming_params` and its row's `ExternParamOwnership::Consume` positions —
the `registration.rs:465` check generalized from the opaque-release path
(registration.rs:430-446) to every registered extern; the same run is the
`E_OWN_CONSUME_BORROWED` sweep of §4.2 (every std function consuming a
resource parameter declares `consume`). Counterfactual in the gate's own
tests: a fixture whose row says `Consume` under a signature without `consume`
must fail the target.

### 6.5 Synthesized functions move to HIR normalization

**[current]** `hew-mir/src/lower/machine_synth.rs` builds synthetic `HirFn`s
for actor receive/init/lifecycle handlers (L242, 437, 536, 597, 880, 1000,
1125) and supervisor bootstrap (L2652, bare `sup.name`, collides across
modules); `closure_gen.rs` mints closure invoke shims (997), named-fn shims
(1178), lambda-actor bodies (1713), generator bodies (2559) and the
receive-generator stream pump (`build_stream_producer_pump`, 2832); `task.rs`
mints task-entry adapters (1045) and fork shims — all with
`legacy_reconstruct_from_full_path`, none verified (§2). `HirActorMethod`
(node.rs:606-618, `HirActorDecl.methods`) bodies are lowered as ordinary
functions that read state fields by name (facts.rs:960-962 walks them beside
handlers).

[P3 closures/generators/task adapters, P4 actors/supervisors/machines] HIR
normalization synthesizes them as ordinary callables with
`declaration` minted by `IdentityTable::declare(DeclarationOccurrence {
kind: Synthesized { parent, producer, ordinal } })` and
`MirCallableInstance::Synthesized { parent, child }`
(`SynthesizedCallable`, identity.rs) as the instance. They lower through the
same HIR→SIR path and the same verifier. **The producer set is closed**:

| Producer | body | header (§4.2) | phase |
| --- | --- | --- | --- |
| `ClosureInvokeShim`, `NamedFnInvokeShim` | the closure body | env `Borrow`; params per the closure signature | P3 |
| `GeneratorBody` | `gen fn`/`gen {}` body | env `Borrow` (the companion's env thunk releases the captures at `hew_gen_coro_destroy`, cont.rs:2058-2098; the body never consumes one, §1.3.5) | P3 |
| `TaskEntryAdapter`, `ForkEntryShim` | `fork t = f(args)` / `fork {}` body | env **`Consume`** (`hew_task_take_env` + `load.take` per field + `hew_arc_release_storage`, §1.3.5); registers `hew_task_set_result_drop_fn(task, hew_drop$T)` for an owning result (§1.5) | P3 (thread-hosted), P4 |
| `LambdaActorBody` | `actor \|x\| { … }` body | env `Borrow` (a `BorrowMut` field is written through `store.assign`; the env is actor-local by 6c); the message taken per §5.6 | P4 |
| `ActorHandler` | `receive fn` body | payload **`Consume`** (§5.6, one disposition); hidden state-place slot | P4 |
| `ActorMethod` | plain `fn` in an actor body (`HirActorMethod`) | hidden state-place slot (`Borrow`, exactly as a handler's); declared params per §4.2; callable only from the same actor's handlers, hooks and methods | P4 |
| `ActorInit` | `init {}` body, or one `store.init` per state field from the spawn argument of that name, plus `HirField.default` initializers | params **`Consume`** (the `Spawn` snapshots, rule 5) | P4 |
| `LifecycleHook` | `#[on(...)]` body | params `Borrow` (§5.6) | P4 |
| `StreamProducerPump` | the `receive gen fn` pump (peer-closed check, `GeneratorNext`, `Yield` as `Snapshot` onto the sink, node.rs:1649-1657) | message per §5.6; sink `Borrow` | P4 |
| `SupervisorBootstrap`, `ChildInit` | supervisor setup; per-child `(config) -> Fresh State` init thunk (§5.7) | config `Borrow`; returns `Fresh` | P4 |

`MachineStep` is deleted when machines desugar (D287): a machine becomes an
enum + `match` in HIR [P4] (§1.3.7), and the `<Machine>Event` companion name
is rendered once in the checker keyed by the machine's `DefId` (today ~19
bare-name sites in `check/registration.rs` and a qualified render in
`machine_synth.rs:1393` disagree for any machine outside the root module).

Lambda actors: `HirExprKind::SpawnLambdaActor.captures` (node.rs:1908-1913,
`HirLambdaCapture { binding, name, kind }`) with `HirCaptureKind::Strong` are
`Snapshot` operands (rule 5, 6c) stored into the actor environment;
`HirCaptureKind::Weak` (node.rs:3237-3245, the forward-bound recursive
self-capture, fixture `tests/vertical-slice/accept/lambda_self_recursion.hew`)
is **not an ownership op**: the environment field is declared with class
`BitCopy` (a non-owning alias of the actor's own `LambdaPid`, valid because the
actor outlives its environment) and the capture is a bit copy of the handle —
today's `LambdaEnvFieldDrop::WeakSelfHandle` (`closure_gen.rs:1436`), which
already drops nothing.

Actors: state is a materialized place owned by the runtime object
(`Materialize { ExplicitStorage }` per field in the init body; handlers,
methods and hooks borrow it for reads and `load.take`/`store.init` it for
mutation; `end_lifetime` on stop through `hew_actor_set_state_drop`
receiving `hew_drop$State`, §1.3.6). `send` operands get `Snapshot::{Share,
DeepCopy, Transfer}` from class + rule 2 last-use (§3 move-on-send). `is`
value identity (#3134) is a checker fact (`IdentityCompare`), not ownership.

---

## 7. The dev-only admission gate

Until P5 the compiler routes **each callable** through SIR when SIR admits it
and through the legacy lowerer otherwise. This is the one transitional
mechanism and it is observable.

- **Routing** [P1]: `hew_compile::Session::lower_hir_module` calls
  `hew_sir::admit(callable) -> Admission::{Sir, Legacy(reason)}` per
  callable HIR normalization produces — `HirItem::Function`, `HirActorMethod`,
  handler/hook/init bodies and every §6.5 producer once it exists in HIR —
  lowers the `Sir` set through `lower → verify → optimize → verify →
  hew_mir::lower_from_sir → verify_mir` and the `Legacy` set through
  `hew_mir::lower_hir_module_with_facts`, and assembles one `IrPipeline`.
  `SirMode { Disabled, Lower }` and `--sir-lower` (`hew-cli/src/compile.rs:34`,
  `args.rs:48-64`) are deleted in the same PR; `HEW_SIR_ROUTE=force-legacy`
  (env, dev-only, refused in `--release`) is introduced **[P1]** for the parity
  harness — `grep -rn HEW_SIR_ROUTE` over the tree is empty today, so nothing
  reads it yet.
  `reason` is a closed enum rendered as a stable string **[P1]**: `unsupported-expr`,
  `machine` (§1.3.7), `actor` (until P4), `suspend` (until P4), `generator`
  (until P3), `closure` (until P3), `callee-header-drift` (below), plus one
  per surface family added at its phase and removed when the family lands;
  the enum has no free-text variant.
- **Route seam**: both routes publish a `CallableHeader` (§4.2) per callable.
  The SIR route derives it from the checker signature; the legacy route
  publishes what its body lowering actually implements (`facts.rs`
  `ParamBoundaryMode` per parameter, `FunctionCallConv`). Module assembly
  compares the two and, on disagreement, routes the callable **and every
  SIR-admitted caller of it** to `Legacy("callee-header-drift")` — never
  fails the build, never lets a SIR caller (`Borrow`, destroys at scope exit)
  call a legacy body that inferred CONSUME (closes): that pair would double
  close. The drift is real during the window: `facts.rs:994-1075`
  force-consumes "non-receiver resource params of `impl`/trait methods"
  regardless of body, and until the `E_OWN_CONSUME_BORROWED` wall (P1) and
  the std `consume` sweep (P3) land, a body-inferred CONSUME can differ from
  a declaration-derived `Borrow`. Handler params are seeded
  `ParamBoundaryMode::OwnedMessage` on the legacy route (facts.rs:1203-1207),
  which agrees with §5.6's taken payload rather than contradicting it; no SIR function calls a handler
  directly (the runtime dispatches), and every handler is `Legacy("actor")`
  until P4, so that pair never meets. `hew tool sir-coverage` prints the
  drift set so the ratchet cannot hide it. The old sentence "agree on …
  parameter modes by construction" is withdrawn; agreement holds by
  construction only after §9's P4 row deletes `compute_param_ownership`.
  Cross-route calls resolve at LLVM linkage by `header.symbol`; the legacy
  emitter keeps its own maps until P5. **Value representation is module-global; only protocols with a header record
  are per-callable** [decision]. A per-callable choice is safe exactly when the
  seam is a *call* the header describes: §5.3's insert entry points
  (`hew_vec_push_owned` versus `hew_vec_push_owned_move` operate on the same
  `HewVec`, so a collection minted by one route and mutated or freed by the
  other is fine), param modes, `conv`, and glue refs. It is **not** safe when
  the seam is how a *value* is laid out, because a closure's `{fn, env}` pair
  and a `dyn`'s `{data, vtable}` fat pointer carry no representation tag and
  the `callee-header-drift` check compares header modes, not representations.
  §5.4's arc-versus-unique-box seam is of the second kind and would be a real
  bug: `hew_arc_new` writes a `HewArcInner` header before the data region
  (arc.rs:102-125) while `hew_dyn_box_alloc`'s free demands "exactly the triple
  a prior `hew_dyn_box_alloc` returned — mismatched layouts are undefined
  behaviour" (trait_object.rs:365-382), so a closure returned `RetMode::Fresh`
  from a `Legacy("suspend")` callable to a SIR-routed caller would meet
  `hew_arc_drop` over a box pointer, and the mirror case frees an arc
  allocation with `hew_dyn_box_free`. So: **the P3 carrier change (closure env
  and `dyn` box → `hew_arc_new`/`hew_arc_clone`/`hew_arc_drop` with the glue as
  `drop_fn`) and the P4 task-env change land in *both* emitters in the same PR**,
  before any callable holding such a value routes through SIR. That is a
  three-symbol swap in the legacy emitter (alloc, free, the in-place drop
  becomes the arc `drop_fn`), not a conversion of any authority: plan §6's ban
  covers retain authority and the post-CFG finalizers, which stay deleted
  rather than converted, and an arc carrier is strictly more permissive than a
  unique box, so the legacy route's move semantics for `let g = f` keep
  working unchanged until §11 row 7 relaxes them at P5. §5.6's envelope-only
  delivery is module-global for the same reason and flips once, at P4: a
  SIR-routed sender building an envelope for a legacy copy-mode handler hits
  the scheduler's hard refuse (`hew_panic`, scheduler.rs:3354-3396), so
  `Legacy("actor")` covers **any** callable containing an actor send, ask,
  spawn, handler, hook or init body until P4, not only handler bodies.
- **`hew tool sir-coverage <path…>`** [P0, shipped]: one line per function body
  — `<file> <item> sir` or `<file> <item> legacy: <reason>` — then
  `sir-coverage: <admitted>/<total> functions (<pct>)`, with `--json` emitting
  the same inventory as a document. Only function bodies (free functions, impl
  methods including root-local trait defaults, actor/machine handler bodies)
  are counted; a bodiless declaration or an impl-block header prints as an
  uncounted `legacy: item-kind:<kind>` line so the corpus is still fully listed.
  `--ratchet FILE` compares the count against the committed one.
  Corpus: `tests/vertical-slice/accept`, `tests/hew`, `examples`, `std`; `make
  sir-coverage` and `make sir-parity` run it in `ci-shard-2`.
  **The ratchet is the admitted-function count, monotone** [decision, plan §6]:
  `scripts/sir-coverage-ratchet.txt` holds that one integer, a drop fails, and
  a rise is reported so the file can be raised (it fails only under
  `RATCHET_STRICT_RECOVERIES=1`). A percentage is deliberately not the ratchet:
  its denominator moves whenever the corpus gains or loses a fixture, so corpus
  growth alone can lower it with no compiler regression at all. Two halves of
  the decision are **not** shipped and are P1 deliverables of this lane, listed
  here so no brief reads them as existing: **`all` is reported but its shrink
  is not yet refused** [P1] — the ratchet file grows a second field and a
  `<total>` drop without a matching ratchet edit in the same PR fails, which is
  what stops a de-admission from hiding behind a shrinking corpus; and the
  **refusal reasons are still free-form strings** [P1] (`item-kind:<kind>`,
  `no-sir-route:<body>`, `sir-verifier:{kind:?}`, `generic-template:…`,
  `not-reached`, plus whatever `SirLoweringStatus::Unsupported` carries) where
  the design requires a closed enum rendered as `E_` codes through
  `hew-cli/src/diagnostic.rs`, so the report is diffable and admission failures
  reach the one diagnostic channel plan §6 requires.
- **Parity harness** [P1] `make sir-parity`, landing **before the first
  admitted owned domain** (plan §6) so no owned value routes through SIR
  unverified against the legacy route. Today's `make sir-parity`
  (`scripts/sir-parity.sh`, P0) is its stand-in and does less: it compiles each
  main-declaring program through the whole-program `--sir-lower` route and
  through the legacy route and diffs exit status and stdout, with a
  compared-program ratchet. P1 replaces all three of those: **per-function
  routing** for the second leg (`HEW_SIR_ROUTE=force-legacy`, not
  `--sir-lower`, so the leg being compared is the routing the compiler actually
  ships), **both legs under ASan with leak detection**, and a **reject corpus**
  comparing diagnostic codes. Concretely: for every runnable fixture in
  `tests/vertical-slice/accept`, `examples/v05/checked-mir`,
  `tests/core-matrix/cells`, and `tests/ownership-balance`, compile twice
  (default routing; `HEW_SIR_ROUTE=force-legacy`), both with
  `HEW_SANITIZE_ADDRESS=1` and `nm`-verified as `tests/ownership-balance/run.py:120-160`
  does; run both under `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1`; diff
  exit status and stdout byte-for-byte (`scripts/checked-mir-corpus.sh run`
  transcript shape). **Reject corpus**: every fixture in
  `tests/vertical-slice/reject` is checked on both routes and the diagnostic
  code and primary span must agree after mapping the legacy codes to the
  §1.6 codes (`E_MIR_CHECK UseAfterConsume` and checker `UseAfterMove` ↔
  `E_OWN_USE_AFTER_CONSUME`; `E_MIR_CHECK InitialisedBeforeUse` ↔
  `E_OWN_UNINIT`; `E_MIR_CHECK MustConsume` ↔ `E_OWN_MUST_CONSUME`; checker
  `MutabilityError` ↔ `E_OWN_MUTATE_LET`; `CloneNotYetSupported`/`no method
  clone` ↔ `E_OWN_CLONE_UNSUPPORTED`; `InvalidSend`/`E_DUPLEX_NON_SEND` ↔
  `E_OWN_SEND_UNSUPPORTED`; the table lives in the harness, not the compiler).
  `CannotMaterializeClosureCapture` has no mapping: `main` refuses **every**
  closure captured into a lambda actor ("only BitCopy scalars, `string`, actor
  pids, `LambdaPid`, and the weak self-handle have an ownership protocol across
  the actor boundary", `repros/ladder/closure_mut_share.hew`), and this design
  admits a `Send` closure as a `Share` (§11 row 7) while refusing only the
  `BorrowMut` shape (6c) — both are fixture moves, not a code mapping.
  A relaxation or tightening this design makes on purpose (§11 rows 3, 7, 9,
  17, 19, 21, 24, 25, 31, 32, 35 and the guarded-destructure arm of §1.3.2) is recorded
  as a fixture move between `reject` and `accept` in the same PR, never as a
  harness exemption. Any other difference or sanitizer finding fails the
  target. Determinism twin: `scripts/compile-determinism-corpus.sh` runs over
  the admitted set as well.
- **rc3 condition** (P5→P6): ratchet at `all/all` (100 %), `hew-mir/src/lower/`
  absent (`test -e` in `make ci-shard-2` fails if present), `HEW_SIR_ROUTE`
  removed, `hew tool sir-coverage` remains as a listing tool whose only legal
  output is `sir` for every callable (a `legacy:` line is a test failure).
  No fallback survives into a release binary: `hew --version` output for a
  release build must not contain `sir-route`, checked by `make test-release-binary`.

---

## 8. Layer contracts (short form)

| Layer | Owns | Must not own | Verifier | Dump |
| --- | --- | --- | --- | --- |
| AST (`hew-parser`) | syntax, spans, trivia | types, names, ownership | parser diagnostics | — |
| HIR (`hew-hir`) | resolved names, `ResolvedTy`, `DefId` on every declaration, `ItemId`/`BindingId`/`SiteId`, capture facts, class table consumption, synthesized functions (§6.5), the checker's ownership walls (§1.6) | CFG, SSA, `Place`, layout, proof | checker + `hew-hir/src/verify.rs`; no `Ty::Var`, no `TypeParam` past the instance service | `--dump-hir` [P5] |
| SIR (`hew-sir`) | typed SSA, block args, `OwnKind`, ownership ops, `Suspend`, provenance (binding name, scope, span per value and op), optimizations | `Place` (only `alloc_place` handles), layout, ABI, symbols, LLVM, the target | OSSA rules 1–6 | `hew compile --dump-sir` [current] |
| MIR (`hew-mir`) | `TargetLayout` (incl. taken bits), `Place`+projections, `Materialize`, headers, carriers, frames, envelopes, glue table, actor headers, the unwind-edge target switch (§4.7) | ownership decisions, re-derivation of class | balance + structural (§4.5) | `--dump-mir raw\|checked` |
| LLVM (`hew-codegen-rs`) | instruction lowering, ABI realization, frame emission, glue bodies, cleanup-thunk emission on registry targets, runtime symbol references | any `ResolvedTy`-shaped policy | `Module::verify()` | `.ll` |

---

## 9. Deletion ledger

Everything below exists on `main` (54e8dde2c) and is deleted by this design.
No **route or fallback** survives rc3: after P5 there is one lowering path,
one MIR form and one emitter. What does survive rc3, each marked
WHY/WHEN-OBSOLETE/WHAT where it is defined, is the coverage tool of §7 and
three deliberate shortcuts: `SIR_GENERIC_INSTANCE_CAP = 1024` (§6.2 — not a
new user-visible limit, it matches `MONOMORPHISATION_REGISTRY_CAP = 1024` on
the legacy route, `hew-hir/src/monomorph.rs:48`), collections not being
refcounted at runtime (§5.5, WHEN-OBSOLETE v0.7), and the MSVC `TrapFrame` +
crash-cleanup-registry realization of the unwind edge (§4.7, WHEN-OBSOLETE
when inkwell exposes funclets). "P5" items go in one PR after the ratchet
reaches 100 %.

| Item | Location | Phase |
| --- | --- | --- |
| `docs/internal/v05-ir-ladder.md`; links in `docs/observe.md:10`, `docs/diagrams.md:17`; skills `hew-ecosystem-map`, `hew-ir-lowering-map`, `hew-supervision/reference.md:62` repointed | docs / `~/.claude/skills` mirror | P0 |
| `tests/corpus/v05-value-model/*.ownership-plan.txt` (19, no consumer) and the old doc's §4 "Ownership Plan Report" | tests | P0 |
| `SirMode`, `--sir-lower`, `SirModeArgs`, `WatchSirModeArgs`, `report_sir_lane`, `report_strict_sir_missing_body` (name-keyed zip, `main.rs:336-344`) | `hew-cli/src/{compile.rs:34, args.rs:48-65,759-773, main.rs:282-387,1596}` | P1 |
| SIR→MIR bridge: `lower_entry_component`, `RawLowerer`, `VirtualRawLowerer`, `zero_drop_elaboration`, `verify_strict_sir_*`, symbol rejoin at sir.rs:804-817 | `hew-mir/src/sir.rs` (3674 lines) | P1 (replaced by `lower_from_sir`) |
| three private scalar-domain predicates | `hew-sir/src/lower.rs:797-815`, `verify.rs:905-923`, `hew-mir/src/sir.rs:1252-1258` | P1 |
| `UseMode`, write-only `EffectSummary`, dead `effects_in` | `hew-sir/src/model.rs:73, 311, 749` | P1 |
| `Ty::is_copy` and its tests (replaced by the §1.1 class table) | `hew-types/src/ty.rs:1452` | P1 |
| `None` markers on the BitCopy scalars and std enums of §1.1 (markers corrected) | `hew-types/src/builtin_type.rs:355` | P1 |
| `vec_authority::is_copy_layout`; `ValueClass::Unknown` and its 15 sites; `hew-hir/src/value_class.rs` (copied, not moved); `BuiltinTypeMarker::Resource` on `LocalPid`/`HewActor`/`BoxedActor`. Each is a reader or an input of the legacy lowering path, which is the parity oracle until P5, so none can move while that leg must agree with itself: flipping `LocalPid` routes a `Vec<LocalPid<_>>` element off its pointer ABI and moves an elaborated-MIR baseline, and `HewActor`/`BoxedActor` carry a close method the builtin class-table seed refuses on a `BitCopy` builtin. Those three disagreements are recorded rather than resolved, by `builtin_marker_and_the_class_table_agree`'s named exception list. | `vec_authority.rs:270`, `hew-hir`, `hew-types/src/builtin_type.rs:355` | P5 |
| `TraitRegistry::is_send` (no callers), the eight `implements_marker(MarkerTrait::Send)` decisions (replaced by `TypeFacts.send` reads, closure rule incl. capture mode), `register_drop_type`/`is_drop_type_any` marker inference, `drop_func_for`/`all_drop_funcs` (test-only) | `hew-types/src/traits.rs:525-537,1039-1046,1107`, `check/{expressions,calls,methods}.rs`, `module_registry.rs:813-835,1246-1321` | P1 |
| MIR-hosted user diagnostics `UseAfterConsume`, `InitialisedBeforeUse`, `MustConsume` (moved into the checker as `E_OWN_*`, §1.6); `CannotMaterializeClosureCapture` for the `BorrowMut` sharing shape (now 6c) | `hew-mir/src/model.rs:6799-6850`, `dataflow.rs`, `closure_gen.rs` | P1 introduce checker rules, P5 delete MIR arms |
| `legacy_reconstruct_from_full_path` sites: hew-hir lower.rs 6, dispatch.rs 18, value_class.rs 2, verify.rs 1; `resolved_ty.rs:56,324` mints; llvm.rs 6172/8980/18265/26757 | hew-hir, hew-types, hew-codegen-rs | P1 |
| `IdentityTable::root_fn_identity`, `ExternTable::register_declaration_only` | `hew-types/src/identity.rs:261`, `extern_table.rs` | P1 |
| dead MIR items: `drop_obligation.rs` (`ValuePosition`/`ReleaseSupport`), `ObligationMintProvenance::{ExplicitRetain, Mixed}`, `ElaboratedMirFunction.coroutine`, `lambda_captures`, `IrPipeline.polymorphic_mir`, `ElaboratedMirFunction.statements`, `ElabBlock::drops`, `CheckedMirFunction.ownership_elaboration` (duplicate carrier) | `hew-mir/src/{drop_obligation.rs, model.rs:6764,7020,7046}` | P1 |
| `__hew_record_drop_inplace_*` / `__hew_enum_drop_inplace_*` / `__hew_record_clone_inplace_*` synthesis; `get_or_declare_clone_helper` | `hew-codegen-rs/src/llvm.rs:5569-9100` | P2 (glue) |
| kind-specialized collection constructors (`hew_vec_new_str/ptr/generic`, `hew_vec_closure_pair_drop_inplace`), `hew_vec_push_owned` (clone-in), `hew_hashmap_insert_layout`/`hew_hashset_insert_layout` `i1`-branch protocol + `emit_insert_overwrite_key_release`, `HewTypeOwnershipKind::{String, Bytes}` special cases, `layout.rs` 2747/2861/4839 `drop_fn` derivation | `hew-runtime/src/vec.rs`, `hashmap.rs`, `hashset.rs`, `hew-cabi/src/vec.rs`, `hew-codegen-rs/src/layout.rs` | P2 introduce, P5 delete |
| `hew-hir/src/monomorph.rs` `MonoKey`/`function_monomorph_symbol`; `MirCallableInstance::Polymorphic` | hew-hir, `hew-mir/src/identity.rs` | P5 (P2 introduces the service) |
| `ClosureEnvMode::HeapBox` / `hew_dyn_box_alloc` closure boxes and `DropKind::ClosurePair`; `hew_dyn_box_alloc`/`hew_dyn_box_free`/`hew_dyn_trait_drop_boxed_in_place` for `dyn` values (replaced by `hew_arc_*` + glue); `TraitObjectStorage`; the `BitCopy`-only `BorrowMut` write-back restriction (assign.rs:631-650) | `hew-mir/src/model.rs:7660-7700`, `hew-runtime/src/trait_object.rs:318-450`, `hew-codegen-rs/src/llvm.rs:15398`, `hew-mir/src/lower/assign.rs` | P3 introduce, P5 delete |
| `machine_synth.rs` (3183), `closure_gen.rs` synth minting (997, 1178, 1713, 2559, 2832), `task.rs:1045`; `<Machine>Event` bare-name renders; `facts.rs compute_param_ownership`/`finalize_param_boundary_modes` (inferred param disposition, replaced by `E_OWN_CONSUME_BORROWED` + declaration-derived headers) | `hew-mir/src/lower/`, `hew-types/src/check/registration.rs` | P3/P4 |
| `SuspendKind` side table, `Terminator::{TaskAwait, ChannelRecv, StreamNext, ConnAwaitRead, ListenerAwaitAccept, SuspendingSelect, SuspendingScopeDeadline, AfterTimer, …}` (folded into `Suspend`) | `hew-mir/src/model.rs:2772,3076,3889-4400` | P4 |
| copy-mode message delivery: `hew_msg_node_free` copy arm, the dispatch `borrow_mode` parameter, `HewMessageDropFn`, `hew_actor_set_message_drop`, `hew_msg_node_free_with_message_drop`, `emit_actor_message_drop_fn`, the borrow-taint fixpoints, `replace_node_payload`'s byte-buffer path, the copy-mode ask body under `hew_actor_ask_with_channel` (`actor_send_result_internal_reply`) | `hew-runtime/src/{mailbox.rs:416,1093-1113,1815-1852,1834-2219,3675, actor.rs:5489-5510,6351-6365}`, `hew-codegen-rs/src/{thunks.rs:3302-3400,4018-4035, llvm.rs:22270-22740}` | P4 introduce envelope-only, P5 delete |
| supervisor state templates: `hew_actor_set_state_clone`, `HewStateCloneFn`, `HewActor.state_clone_fn` **and its three dispatch readers** (`scheduler.rs:3315-3324`, `2442-2450`, `scheduler_wasm.rs:2034-2038` — the pair match collapses to `state_drop_fn` alone, §5.7), `catch_unwind_state_clone` (`scheduler.rs:9188`) and the `wasm_parity_tests.rs:435` assertion on it, `HewActor.state_drop_borrowed` with `mark_state_drop_borrowed`/`mark_state_drop_owned` (actor.rs:1641-1676) and their reads at actor.rs:2599/2739, the `add_child_spec` template deep-copy, `restart_child_from_spec`'s clone and byte-copy legs (2760-2815), `StateFieldCloneKind`, `state_clone.rs`, `suspend.rs:7724` (replaced by the uniform `ChildInit` thunk, §5.7) | `hew-runtime/src/{actor.rs:1198,1291,1562,1641-1676,2599,2739,5531-5540, scheduler.rs:2442,3315,9188, scheduler_wasm.rs:2034, supervisor.rs:2760-2830}`, `hew-mir/src/state_clone.rs`, `hew-codegen-rs/src/{llvm.rs:5155-5215, suspend.rs}` | P4 |
| spawn env `hew_rc_new` (thunks.rs:690) and `hew_task_free → hew_rc_drop(env)`; `SpawnEnvFieldOwnership::BorrowsOnly` (every capture is env-owned); `hew_task_set_cancel_cleanup_fn` and `run_cancel_cleanup` (no codegen registrant) | `hew-codegen-rs/src/thunks.rs:686-760`, `hew-mir/src/model.rs:7723`, `hew-runtime/src/task_scope.rs:544-566,683-686,729` | P4 |
| crash-cleanup **derivation** in codegen (`collect_helper_crash_cleanup_descriptors`, `helper_crash_cleanup_owner_lineages`, write-set producers) — replaced by SIR unwind blocks compiled to cleanup thunks over `TrapFrame` (§4.7); the runtime API `hew_cont_crash_cleanup_arm/deactivate/retire` is kept | `hew-codegen-rs/src/llvm.rs:23264-25218` | P4 introduce, P5 delete |
| unconditional dispatch-state escrow on `StructuredLlvm` targets: the `hew_dispatch_state_cleanup_begin_replace`/`_prepare` emission selected by handler kind alone (`actor_state_store_transaction_for_kind`) and the `begin_dispatch_crash_cleanup` arm in the native scheduler — both narrowed to `CrashOwnerRegistry` targets, the runtime side by `#[cfg]` on the per-triple build (§4.7, §5.7); the runtime entry points themselves are kept | `hew-codegen-rs/src/llvm.rs:18408-18416,22252-22266`, `hew-runtime/src/scheduler.rs:2442,3315` | P4 |
| `Instr::MachineEmitPlaceholder` unit-only SHIM (replaced by the owned-payload queue of §5.8 under the D287 desugar) | `hew-codegen-rs/src/llvm.rs:15545-15578`, `hew-mir/src/model.rs:6405-6435` | P4 |
| `wire.rs::emit_de_drop_owned` and the decode-temp `hew_string_drop` at 2873-2881 (replaced by `hew_drop$` calls from MIR envelope facts) | `hew-codegen-rs/src/wire.rs:137,2873` | P4 |
| the direct HIR body lowerer, all 57 files (`mod.rs` 14850, `temp_drop.rs` 10549 — successor §1.3.4, `expr.rs`, `drop_plan.rs` + `drop_plan/*`, `ownership.rs`, `pattern.rs`, `facts.rs`, `composite_own.rs` + 8 subfiles, `split_consume.rs`, `edge_owner_replay.rs`, `suspend_places.rs`, `scope.rs`, `move_value.rs`, `control_flow.rs`, `actor.rs`, `assign.rs`, `owner_mint.rs`, `owned_cursor_call.rs`, `borrowed_argument_owner.rs`, `field_load_poison.rs`, `vec_index.rs`, `consts.rs`, `cfg_util.rs`, `rc_intrinsic.rs`, `actor_state_handle.rs`, test files) | `hew-mir/src/lower/**` | P5 |
| `OwnershipEvent` (16 variants), `OwnerId {binding, generation}`, `OwnerDropRecipe`, `DropRecipe`, `DropPlan`/`ElabDrop`/`ElabDropGuard`, `DropFnSpec`, `DropKind`, `ExitPath`, `Instr::{OwnershipEvent, StringRetain, BytesRetain, ValueSnapshotDrop, ValueSnapshotClone, Drop, NeutralizePayloadSlot, AggregateProjectionNeutralize, AggregateOverwriteRelease, RecordFieldDrop, FieldDropInPlace, InteriorMutationCommit}`, `StringRetainCondition`, `ActorStateLoadMode`, `ParamBoundaryMode`/`ParamBoundaryFact`, `Strategy`/`DecisionFact`, `MirCheck`, `CheckedMirFunction`, `ElaboratedMirFunction`, `RawMirFunction` (replaced by `MirFunction`), `Terminator::Call.callee: String`, `Place` handle variants | `hew-mir/src/model.rs` | P5 |
| `dataflow.rs` (owner replay / move checker over `MirStatement`), `return_provenance.rs` (7440) + `return_provenance_ref.rs`, `ownership.rs` (2183, `ValueOwnership`/`CowHeapRelease`), `thunk_requirements.rs` resource lookups by name, `model.rs` `ty_drop_obligation_inner`/`ty_carries_drop_obligation*`/`ty_owns_heap_mir`/`ty_owns_heap`/`container_ingress_is_copy_in`/`CloseObligationRegistry` string join (1834), `ownership.rs` `ValueOwnership::owns_heap`, `MirStatement` stream. The five `no-new-copy-predicate` exempt symbols are these five: `ty_owns_heap`/`ty_owns_heap_mir`/`container_ingress_is_copy_in` (`model.rs`), `owns_heap` (`ownership.rs`), `resolved_ty_element_owns_heap_for_owned_vec` (`hew-codegen-rs/src/llvm.rs`, deleted with the legacy emitter row above). | `hew-mir/src` | P5 |
| `hew_mir::lower_hir_module_with_facts`, `Session::lower_hir_module` legacy arm, `HEW_SIR_ROUTE`, `Admission::Legacy` and its reason enum, the `callee-header-drift` seam | `hew-compile/src/lib.rs`, `hew-cli` | P5 |
| legacy emitter: `llvm.rs` policy sites 16698, 26256, 26630, 26059, 22270-22740, 21990/22040, 26336, 3342-3420/36911, 6083-11141, 12908; name-keyed maps 37995-38008, 31683, 21610, 24776, 31277, 31718-31720; `validate_and_index_mir_stages` (37455); `suspend.rs` drop-plan consumers 312-321/3428/3478; `thunks.rs:433`; `codegen_record_layouts` (26380); mirrored table tests `is_owned_vec_element_matches_codegen_owned_vec_table` / `resolved_ty_element_owns_heap_for_owned_vec_matches_mir_table` | `hew-codegen-rs/src/{llvm.rs, suspend.rs, thunks.rs, llvm_tests.rs}` | P5 |
| `E_MIR_CHECK` renderer arm, `DropPlanUndetermined`, `OwnedHandleAggregateExtractionUnsupported`, 281 MIR `NotYetImplemented` constructions | `hew-cli/src/diagnostic.rs:118-145`, `hew-mir` | P5 |
| dump text pins of the legacy form: `examples/v05/checked-mir/golden/*.mir` + `MANIFEST.sha256`, `tests/mir-baselines/*.elab.mir`, `hew-cli/tests/funcupdate_mir_baselines.rs`; `--dump-mir elab` | tests, hew-cli | P5 (regenerated as raw/checked goldens of the one form) |
| `scripts/nextest-expected-failures.tsv` rows citing #3079/#3118/#2523/#3106/#3124, `hew-corpus-expected-failures.txt` ownership rows, `hew-orchestration/plans/ownership-seam-corpus/residue/` messages pinned to legacy wording | ledgers | P5 (each row deleted with evidence of the passing command) |
| tests pinning deleted mechanisms: `hew-mir/src/lower/stale_owner_canonicalization_tests.rs`, `drop_plan/{replay_plan_tests, replay_plan_proptests, obligation_balance_validator}.rs`, hew-mir `tests/lowering_expr`, `tests/actor` fixtures built on `RawMirFunction`; `hew-codegen-rs/tests/support/mir_fixture.rs` | hew-mir, hew-codegen-rs | P5 (rewritten against `MirFunction` where the behaviour survives) |

**Not introduced** (named in revision 3, withdrawn here): `hew_string_make_unique`
(`cstring_ensure_unique` exposure) and `hew_bytes_make_unique` — `fork` has
no runtime realization for any current carrier (§4.3).

**Also deleted, from the otherwise-kept envelope authority**:
`hew_msg_envelope_fork_for_write` / `cow_envelope::fork_for_write` and
`hew_msg_envelope_clone_alias`, on native (`mailbox.rs:557,596`) and WASM
(`mailbox_wasm.rs:284,330`), with their `wasm_parity_tests.rs:604-720` cases
[P4]. They are incompatible with the envelope-only protocol of §5.6:
`fork_for_write` `memcpy`s the payload and constructs the fork with the **same
`drop_glue`** (cow_envelope.rs:140-186), so two envelopes would claim
ownership of the same heap fields, and nothing in `mailbox_header.rs`
`header_validate` ties `HEW_MSG_ENVELOPE_FORKED` to a null glue. Neither has a
compiler caller today (`grep -rn "envelope_fork_for_write\|envelope_clone_alias"
hew-codegen-rs/src hew-mir/src` is empty); under §5.6 a `Share` argument is
retained into the payload at `PrepareEnvelope` and a `Transfer` argument
carries `HEW_MSG_ENVELOPE_CAPABILITY_TRANSFER`, so no fork-on-write path
exists to keep. `HEW_MSG_ENVELOPE_ALIAS_ACTIVE`/`FORKED` become unused header
bits; the mask stays for wire compatibility.

**Introduced** (new runtime rows in `hew-mir/src/runtime_symbols.rs`, each
with its ASan fixture): `hew_hashmap_insert_layout_move` /
`hew_hashset_insert_layout_move` [P2]; `hew_task_take_env`,
`hew_arc_release_storage` (precondition `strong == 1`, §1.3.5) [P3/P4];
`hew_msg_envelope_take_payload`, `hew_msg_payload_free`,
`hew_mailbox_send_aliased_with_reply` [P4, §5.6]. Plus one new **emitted**
glue role, not a runtime row: `hew_abandon$<T>` for every `T` with a `Linear`
leaf (§5.1, §5.2 item 7) [P4].

Not deleted (kept as authorities): `hew-mir/src/runtime_symbols.rs` — **the one
symbol table**, spelling and ownership row together (§5.1), with
`scripts/jit-symbol-classification.toml` kept as the source text generated into
it [P1] and `hew-types/src/ffi_contracts.rs` kept as the generator's other
output; `callee_ownership_contract` and `CalleeOwnershipContract` inside that
same file are **deleted** with the legacy lowerer [P5], because a spelling-keyed
ownership verdict is the second authority §5.1 forbids;
`hew-hir::LifecycleRegistry`/`declared_release.rs`;
`hew-mir/src/identity.rs` (`MirCallableKey`); `SendAliasMode`;
`hew-runtime/src/arc.rs`; `hew_actor_set_state_drop`;
`hew_supervisor_set_child_init_fn`/`HewChildInitFn` and the supervisor config
buffer (§5.7); `hew_task_set_result_drop_fn` (§1.5); `hew_task_detach_await`
and every per-kind abandon symbol (§1.5); `hew_cont_crash_cleanup_arm/
deactivate/retire` and `hew_dispatch_state_cleanup_*` (§4.7 registry target);
`hew_msg_envelope_*` and `cow_envelope.rs` **except**
`fork_for_write`/`clone_alias` (next paragraph); `hew_reply_channel_*`;
`coalesce_message_key`;
`HirActorDecl.cycle_capable`; `tests/ownership-balance`, vertical-slice,
core-matrix, checked-mir `.expected` transcripts (behavioural oracles, not
dump pins); `hew-sir/src/{analysis.rs, optimize.rs, dump.rs}`.

---

## 10. Acceptance per phase (what "done" means for this document)

| Phase | Gate command(s) |
| --- | --- |
| P0 | three adversarial reviewers find no `HirExprKind`/`HirStmtKind`/`HirItem` variant absent from §1.3's emitter column, §1.3.1–§1.3.8 or the `Admission::Legacy` reasons of §7 (the §1.3.8 closed-list table test is the mechanical form, on **both inhabitants** of every `Option` payload — `Yield { value: None }` is the case that motivated it), no `SuspendKind` variant without a §1.5 row and no HIR suspension variant without one either — `AwaitTask`, `AwaitRestart`, `ConnAwaitRead`, `ListenerAwaitAccept`, `ChannelRecvAwait`, `StreamRecvAwait`, `RemoteActorAsk`, `Select`, `Join`, `ScopeDeadline`, `Yield`, `ActorSend`/`ActorAsk`/`StreamSend` in suspending position (each row carrying its abandon op and where it is emitted: cancel edge and losing resume edges), no `BuiltinType` variant without a §1.1 row, no synthesized callable outside §6.5's producer table, and no send/call/capture/spawn/yield/suspend operand without a §2.1 rule-5 mode; `hew tool sir-coverage` builds and prints a total; this doc merged |
| P1 | the six P1 verifier gates **all built by this phase** (§1.6): `hew tool sir-verify` over `hew-sir/tests/verify_negative/`, `make test-sir-verify`, `hew-mir/tests/balance_verifier` (incl. the two resume-edge abandon negatives of §4.5), `walls_e2e.rs`, and the two `rules/rust` ast-grep rules (no new `is_copy`/`owns_heap` outside `hew-types`, §1.1; `no-hashmap-in-emission-order`, §6.1) in `make lint`; `make sir-parity` rebuilt on per-function routing (`HEW_SIR_ROUTE=force-legacy`), both legs ASan with leak detection, over the admitted set (scalars, string, bytes) including the reject corpus, landing before the first admitted owned domain (§7); `walls_e2e.rs` green for the seven P1 codes of §1.6 (`E_OWN_LINEAR_STATE_FIELD` at P4) **plus** the `verify_negative/` SIR-text fixture per user-facing rule that replaces the withdrawn `--no-typecheck` half of that gate (§1.6); `scripts/sir-coverage-ratchet.txt` extended to a second field so an `all` shrink is refused, and the refusal reasons made a closed enum rendered as `E_` codes (§7 — the P0 tool ships neither); identity blockers listed in §6.1 closed (`cargo check --workspace --tests` green); §1.1 marker corrections landed, with the three `LocalPid`/`HewActor`/`BoxedActor` rows recorded as named exceptions by `builtin_marker_and_the_class_table_agree` and deleted at P5 (§9), and with the per-variant table test (incl. `Vec<Conn>`, `Vec<Rc<i64>>`, a closure over an `i64` and one over a `Conn`); closure send fact keyed by the `ClosureInvokeShim` `DefId` with the two-modes-one-capture-type test (§6.3), and `send` unrepresentable as a plain `bool` for a `Closure` key (§6.3); the `CoerceToDynTrait` send wall with a reject fixture coercing an `Rc`-holding concrete into a `dyn … + Send` (§1.1, §11 row 37); `TypeInstanceKey` structural with `ResolvedTy: Ord` and the non-nominal lookup test (§6.2) |
| P2 | ratchet rises; F19/F14/#3186/#3187 repro corpus (`repros/`, `hew-orchestration/plans/ownership-seam-corpus/residue/`) compiles through `sir` per `sir-coverage`; glue ASan fixtures incl. insert-overwrite, `Vec<Conn>`, `Vec<Rc<i64>>`; the move-out counterfactual of §5.3 (`Vec<Conn>.pop()` closes exactly once, `drop_fn` count zero at the pop); destroy-sinking negative fixture (an `AffineResource`, a `Vec<Conn>` and a `dyn` over an `Rc`-holding record, all left in place); temporary-release core-matrix cell (§1.3.4); guarded nested-destructure arm accept (§1.3.2) |
| P3 | `make test-ffi-table` (the std `consume` sweep of §4.2, `Absent` rows, **and** the declaration-versus-row disagreement check generalized from `registration.rs:465`, with a disagreeing fixture as its counterfactual); #3119 generator cases through `sir`; closures/traits/`consuming`; arc-backed closure and `dyn` envs under ASan, with the carrier swap landed in **both** emitters in the same PR and a cross-route fixture (a `Legacy` callable returning a `Fresh` closure to a SIR-routed caller) under ASan (§7); owned `BorrowMut` capture fixture (§1.3.5); `hew_arc_release_storage`'s `strong == 1` abort covered by a unit test; the `dyn` mutating-receiver probe recorded (wall or absent) |
| P4 | native/WASM parity (`make sandbox-parity`, `wasm_parity_tests` — including the `state_clone_fn` assertion at `wasm_parity_tests.rs:435` deleted with the field, §5.7), #3195/#3193 behaviours, `Suspend` for every row of §1.5 with the cancel-order **and resume-order** structural checks plus an ASan select fixture asserting one loser reply-channel free per resume edge, envelope-only delivery fixtures incl. a taken payload with `CAPABILITY_TRANSFER` set, `coalesce_owned_payload_leak.hew` (a suspending handler reading a payload field after resume — the single-disposition oracle, §5.6), coalesce-by-key, aliased ask and a `#[linear]`-carrying message dropped at overflow through `hew_abandon$` (§5.6, §5.2 item 7), cancel-edge `defer` fixture (§1.3.3) with a `#[linear]` value as its subject (§11 row 32), the two-task sequential join of §2.1 6d compiling unchanged, `result_drop_fn` cancel-after-completion fixture (§1.5), supervisor restart under ASan with the evaluation counter (§5.7), `E_OWN_LINEAR_STATE_FIELD`, the resource-state-field taken-bit fixtures and the re-initialization single-`close` fixture (§1.3.6, §11 row 35), the MSVC trap fixture on `win11-dev` with a value live across two trapping calls (§4.7 region rule), machine desugar rows (§1.3.7) |
| P5 | ratchet 100 %; §9 P5 rows deleted with `grep` evidence in the PR body; full `make test`, sanitizer matrix, O0/O2 differential, multi-module project builds and runs; §11 doc sync landed |
| P6 | ID-diff protocol versus main; no red |

---

## 11. Sentences this design overrides

Every row is a user-visible decision. The P5 doc sync edits the losing
sentence; until then this table is the authority over the **spec
(`docs/specs/HEW-SPEC-2026.md`), `docs/v05/ownership.md`, and this document's
own withdrawn drafts** — the three sources its "losing sentence" column names.
It is not a precedence rule over the other ladder documents: those are governed
by plan §5.1, quoted at the top of this file. Rows marked **relaxation** accept programs `main` rejects; rows
marked **tightening** reject programs `main` accepts; rows marked
**behaviour** change what an accepted program does; rows marked **wording**
change no program.

| # | Decision | Losing sentence | Winning sentence / evidence | Kind |
| --- | --- | --- | --- | --- |
| 1 | The user-facing surface is three COW walls plus the move-checker family plus definite initialization (§1.6) | ownership.md:189 "There is no fourth wall." | spec §3.7.8.1 item 4 "the move-checker tracks the single live binding"; §3.7.8.2 `MustConsumeAtScopeExit`; probes `repros/ladder/{gen_rebind,cond_init,resource_early_close}.hew`, `repros/ladder/fork_unawaited.hew` | wording (codes move from `E_MIR_CHECK` to `E_OWN_*`) |
| 2 | Wall 6a is assignment to a `let` (reassignment or through a projection; a `let` state field outside `init {}`), not a mutating method call | ownership.md:186 "mutation of a `let` — you mutate a value bound with `let`" (as read to cover `v.push`) | spec §3.4.3 "It controls whether the _binding_ can be reassigned, not whether the underlying data is mutable" and line 636 (assignment to a `let` field is the rejected form); §3.4.6 `ref1.push(1)` under "What IS Allowed"; `repros/ladder/mutate_let.hew` prints `1`; `repros/ladder/let_state_push.hew` prints `1` | wording |
| 3 | `let y = x` is `copy_value` for `CowValue`/`PersistentShare`, `move` for `AffineResource`/`Linear` — the class being the element-joined class, so `let w = v` with `v: Vec<Conn>` stays a move | spec §3.7.2 "`let owned = data; // move, not copy` / `data is no longer valid`" for a `Vec` | ownership.md value model (calls borrow, values are COW); `main` already copies strings (`repros/ladder/bind_copy.hew` lines 3-5 pass) and moves generators/resources and `Vec<Conn>` (`repros/ladder/vec_resource.hew`) | relaxation for `Vec<CowValue>`/closure rebinds (reject → accept) |
| 4 | Copy legality is `TypeFacts.clone`, not class; `Rc`/`Weak` clone; a collection's `clone` follows its element | old draft §5.4 "`Generator` … rejected (6b)" read as a class rule; old §1.1 `Vec` row "`CowValue` / `DeepCopy`" regardless of element | spec §3.7.5 "`.clone()` creates another strong owner"; `repros/ladder/rc_clone.hew` prints `2`; `repros/ladder/vec_resource_drop.hew`, `vec_rc_weak.hew` | wording |
| 5 | An `AffineResource`/`Linear` value — a `#[resource]` record, `Vec<Conn>`, **and `LambdaPid` although it has a `Retain` clone path** — is sent by `Transfer`; the sender's binding is consumed; `Share` of a `LambdaPid` was rejected (§2.1 rule 5) | ownership.md:147 "Sending a **non-sendable** value — a resource-shaped type … is a fail-closed compile error" | spec §3.7.8.1 item 4 "Sends … consume the value"; `repros/ladder/resource_send.hew` prints `sent`, `got 1`, `close 1`; `resource_send2.hew` → `use of moved value`; `repros/ladder/lambda_send_twice.hew` → `use of moved value \`w\`` | wording (matches `main`) |
| 6 | Rule 6d exempts unwind edges | old draft §1.3 destroy row "(… unwind, cancel)" with 6d unconditional | spec §3.7.8.4 Path 4 "The move-checker does not require `#[linear]` consume on trap-only edges" | wording |
| 7 | Closures and `dyn Trait` are refcounted shares (`hew_arc_*`); a closure with a `BorrowMut` capture is not `Send`, and one with an `AffineResource` capture is `Transfer` only (row 33) | `main`'s unique-owner `hew_dyn_box_alloc` boxes (model.rs:7668-7700), `let g = f` then `f(1)` → `E_MIR_CHECK`; `traits.rs:1039` mode-agnostic closure `Send` | §5.4 (atomic because `Send` closures cross threads); §1.3.5; `repros/ladder/closure_mut_share.hew` (main already refuses, in MIR) | relaxation (closure rebind and closure-into-lambda-actor/spawn capture of a `Send` closure, reject → accept) + runtime carrier change; the `BorrowMut` shape keeps its refusal, now 6c in the checker |
| 8 | `defer` bodies run on the cancel edge | `main` (scope.rs emits defers on normal/return/break/continue only) | spec §4.5 line 3257 "All `defer` blocks … run during unwinding" | behaviour (P4 fix) |
| 9 | `clone b` on `bytes` is legal | ownership.md:98 "a heap type whose runtime copy path genuinely isn't wired yet, e.g. `bytes`"; `repros/ladder/bytes_clone.hew` → `no method clone on bytes` | §5.4: `copy_value` on `bytes` is `hew_bytes_clone_ref` (`Retain`) [P1]; no `make_unique` symbol is involved | relaxation |
| 10 | `is` is reference identity on heap handles | ownership.md:204 "There is no pointer-equality operator." | spec line 5095 "`is` = reference identity on heap handles"; `IdentityCompare` node.rs:1415, 2026 | wording (stale source doc) |
| 11 | Destroy sinking moves **only** a `CowValue` release: `AffineResource`/`Linear` are excluded by the element-joined class (`Vec<Conn>`, `Vec<Rc<T>>`, an `Rc`-capturing closure), and `PersistentShare` is excluded outright because a `dyn Trait`'s concrete payload is not part of its type | old draft §3 row with no restriction; revision 3's top-level-class restriction; revision 4's "`CowValue` or `PersistentShare`" | spec §3.7.3 "Cleanup runs at a predictable point (scope exit)", §3.7.5 `upgrade()` exactness, §3.7.6 side-effect restriction; `repros/ladder/weak_scope.hew` prints `alive`; `repros/ladder/vec_rc_weak.hew` prints `1`, `5`; `repros/ladder/dyn_rc.hew` (`dyn Show` over a record holding an `Rc`) prints `5`, `alive` | wording (preserves behaviour) |
| 12 | A read-only place receiver is `begin_borrow %p` (the place stays `Init`); a mutating `CowValue` place receiver is `load.take` → `fork` → call → `store.init`, with `store.init %p, %forked` on unwind/cancel edges (the callee borrowed it) | `main`'s `ActorStateLoadMode::Borrowed` bare alias decided by a classifier (model.rs:6648-6656); revision 3's "the edge stores a fresh default/empty value" | §1.3 borrow/`load.take` rows; `hew_vec_push_owned_move(v: *mut HewVec, …)` borrows `v` (vec.rs:2663); rule 4's place classes | wording (same observable behaviour) |
| 13 | Message delivery is envelope-only and every dispatched payload is **taken** by the handler — one disposition, no class split and no per-handler header field (§5.6) | `main`'s copy-mode nodes + `HewMessageDropFn`; revision 3's "handlers borrow" for every message; revision 5's own two-disposition split (borrowed for a `BitCopy`/`CowValue`/`PersistentShare` record) | §5.6; `cow_envelope.rs:106-118` (`drop_fn` only on a non-null payload); the dispatch order that kills the borrowed disposition — `hew_msg_node_free` at `scheduler.rs:3766` precedes `park_suspended_activation` at 3780-3788, so a suspending handler's borrowed field would be read after `libc::free` (cow_envelope.rs:113-121); shipped counterexample `tests/vertical-slice/accept/coalesce_owned_payload_leak.hew:13-17` | runtime protocol change; see row 24 for the one user-visible effect |
| 14 | `Task<T>` is freed by the scope; `AwaitTask` copies the result out; an unconsumed result is released through `result_drop_fn`; unbound spawns (`work();` in a scope, `fork {}`) mint no `Task` value | old draft §5.2 "tasks → `hew_task_free`"; revision 3's `Task<T>` row read as covering unbound spawns (which 6d would then reject) | task_scope.rs:619-627, 666-690, 776-786, 1322; dataflow.rs:1493 (`MustConsume` iterates `linear_bindings` only); `tests/vertical-slice/accept/{w2006_scope_spawn,fork_block_args_spawn}.hew` | wording (matches `main`) |
| 15 | `LocalPid`/`HewActor` are `BitCopy`; the BitCopy scalars/enums of §1.1 get marker `BitCopy` | `builtin_type.rs:355` marker rows (`Resource` / `None`) | `ty_is_nonowning_pid_leaf` llvm.rs:25479; no `close_method()` | wording (matches `main` behaviour) |
| 16 | `fork` is never emitted for a `string` and has no runtime realization for any current carrier (§4.3); `p.n = 1` on a record with a literal string field forks nothing | revision 3 §4.3/§5.4 "`hew_string_make_unique` [P1, exposes `cstring_ensure_unique`]", "record with heap fields: `Fork` per field" | cabi.rs:495-511 ("Unmanaged pointers must be filtered out by the caller"), string.rs:1264-1296 (`is_managed_cstring` guards); `std/string.hew` has no `var self` method | wording (a UB path removed before it existed) |
| 17 | A `BorrowMut` capture is an env-owned copy written through `store.assign` on the env field; the outer `var` never observes the closure's writes; owned `BorrowMut` captures are legal | spec §3.4.5 read as "captured variables" being the outer bindings; `main`'s `BitCopy`-only write-back restriction (assign.rs:631-650) | assign.rs:631-640 ("the caller's original binding is independent"); `repros/ladder/borrowmut_capture.hew` prints `0`; §1.3.5 | wording for scalars (matches `main`); relaxation for owned captures (reject → accept) |
| 18 | Every unwind edge carries `destroy_value`s (a trap closes resources and frees heap); traps do not leak | ownership.md:48-52 "Abort and trap paths may leak-at-abort, never double-free. A runtime trap abandons outstanding obligations rather than force-discharging them" | spec §3.7.8.1 / line 1790 "dispatches `close` on every scope-exit path including `Trap` and `Cancel`"; §1.3.3, §2.1 rule 1, §4.7 (every target runs the block) | wording (ownership.md loses) |
| 19 | A by-value `#[resource]` parameter without `consume` is `Borrow`; consuming it in the body is `E_OWN_CONSUME_BORROWED` | `main`'s body-inferred disposition (facts.rs:994-1075 monotone fixpoint); node.rs:1237-1246 "inferred borrow/consume disposition" | §4.2 header derivation from declarations; spec §3.7.8.5 `consume` modifier (`std/fs.hew:536`); `repros/ladder/res_param_consume.hew` (accepted today → rejected with a fix-it) | tightening (accept → reject; std swept at P3) |
| 20 | *(withdrawn)* — the row overrode a sentence of `hew-orchestration/plans/final-ladder-program.md`, which plan §5.1 does not let this table do. The taken-bit design it carried is §1.3.6 and §5.2 item 1; its user-visible half is folded into row 35. The number is kept so the citations in §1.6 and §7 do not shift | — | — | — |
| 21 | A `#[linear]` actor state field is a compile error at actor declaration (`E_OWN_LINEAR_STATE_FIELD`) | `main` accepts and never consumes it (`repros/ladder/linear_actor_field.hew` prints `1`) | spec §3.7.8.4 Path 3 "A bare `#[linear]` field whose consume path can be bypassed by a supervised restart is a compile error at actor-declaration time"; `ResourceMarker` is single-valued so the dual-marker admission is empty | tightening (accept → reject) |
| 22 | A select `TaskAwait` arm `Move`s the task handle; a later `await t` is `E_OWN_USE_AFTER_CONSUME` | `main`'s plain read of the task in the arm (task.rs:1694) | spec line 3548 (the arm is outside edition 2026's sealed select set); `AwaitTask`'s `Move`; `hew_task_take_result` single take | wording (no accept fixture exists; the arm is not in the edition) |
| 23 | A non-`move` capture of a non-`Copy` shareable value into a lambda actor is a legal `Share` snapshot | spec §3.4.5 "Without `move` keyword: … Non-`Copy` values cause a compile error" | `repros/ladder/cap_nomove.hew` prints `hello`, `hello`, `1`; expressions.rs:7598-7606 ("inferred `Borrow` / `BorrowMut` captures are ACCEPTED"); rule 6c | wording (matches `main`) |
| 24 | A handler that keeps a sent `#[resource]` field owns it; the resource closes exactly once (at actor stop or the field's overwrite) | `main`'s copy mode hands the handler the fields and the envelope/node path double-closes (`repros/ladder/resource_keep.hew` → `kept`, `close 7`, `fd 7`, `close 7`) | §5.6's taken payload (`hew_msg_envelope_take_payload`) | behaviour (bug fix: one `close`) |
| 25 | Every supervised child's init args are re-evaluated on every restart through a `ChildInit` thunk | `main`'s template + `state_clone_fn` deep copy for children without a config dependence (supervisor.rs:2760-2815; suspend.rs:8686 registers the thunk only `if has_config_field`) | supervisor.rs:530-548 (the thunk is already "THE source of the child's actor state … on every restart" for config-init children); §5.7 | behaviour for a non-config child with a side-effecting init arg (evaluated per restart, not once); wording otherwise |
| 26 | `let ref1 = data; let ref2 = data; ref1.push(1); ref2.push(2)` and `var y = x; x.push(3); y.push(4)` are independent COW values, not aliases | spec §3.4.6 comments "Multiple mutable references - ALLOWED", "Aliasing mutable data - ALLOWED" | `repros/ladder/state_alias.hew` prints `0`, `1`, `1` (the state field is untouched); §1.3 `copy_value` row; row 3 | wording (the comments lose; the code is accepted unchanged) |
| 27 | An unnamed `AffineResource` temporary (`peek(mk())`) closes at the enclosing block's exit; `CowValue` temporaries may be sunk to the full expression | revision 3 (no rule; `temp_drop.rs` allow-sets) | `repros/ladder/temp_close.hew` → `9`, `after`, `close 9`; spec §3.7.3 "Cleanup runs at a predictable point (scope exit)"; §1.3.4 | wording (matches `main`) |
| 28 | `mailbox coalesce` replaces by key under envelope-only delivery | `main`'s aliased send path applies the coalesce *fallback* for envelopes (mailbox.rs:2506-2510) | §5.6 (`coalesce_message_key` over the envelope payload; node replacement by envelope swap) | wording (semantics preserved when every message becomes an envelope; a P4 fixture pins it) |
| 29 | A `receive gen fn` `Yield` is a `Snapshot` with the send fact checked (6c) | `main` has no send-fact check on a yielded value (`grep -rn -i yield hew-types/src/check/*.rs \| grep -i send` is empty) | §1.5 `Yield (receive gen fn)` row; spec §3.4.8 (no data races between actors) | tightening if a fixture yielding a non-`Send` element exists at P4 (recorded as a fixture move then); wording otherwise |
| 30 | A guarded match arm over a nested aggregate payload destructure is accepted; every predicate runs before the arm's `destructure` | `main`'s refusal at lower.rs:31089 ("guarded match arm with nested aggregate payload destructure"); revision 3's "guards are `None` at all eight construction sites; [P2] when guards land" | lower.rs:30667-30670 (guards are lowered and evaluated after pattern matching); §1.3.2 | relaxation (reject → accept at P2); wording for the withdrawn sentence |
| 31 | A non-`move` capture of a `clone == None` value (`#[resource]`, generator, `Sender`) into a closure is `E_OWN_CLONE_UNSUPPORTED`; `move` captures it | `main` aliases the value through a `BorrowsOnly` env field (`repros/ladder/closure_borrow_conn.hew` → `4`, `after`, `close 4`) | §1.3.5 (every env owns its captures; no env holds a pointer into the constructing frame); §2.2 | tightening (accept → reject with a `move` fix-it) |
| 32 | 6d applies on cancel exits for a user `#[linear]` value (`defer` is the consumer), and **not** to a `Task<T>`, whose cancel-edge `destroy_value` is code-free because the scope owns it | `main` checks must-consume only at normal exits: `dataflow.rs:1467-1472` walks `Terminator::Return` blocks alone | spec §3.7.8.4 Path 4 exempts trap-only edges and calls cancellation distinct from a trap (line 1745-1747); §1.3.3 runs `defer` on the cancel edge; §1.5 (`free_scope_tasks`, task_scope.rs:619-627). Without the `Task` half, `repros/20_generic_task_spawn.hew:23-26` and `tests/hew/task_entry_adapter_symbol_collision_test.hew:38-41,49-52` — the two-task sequential join — would be rejected | tightening for `#[linear]` across a suspend with no `defer` (accept → reject), with **no fixture to move**: no corpus program holds a `#[linear]` value live across a suspension — the only `#[linear]`-plus-`await` fixture, `tests/hew/actor_message_ownership_transfer_test.hew:114-115`, consumes its `Ticket` into the send before the `await`. A P4 reject fixture is added for the wall; no change for `Task` |
| 33 | A closure's class is `PersistentShare` joined with the aggregate rule over its captures, so a closure capturing a `#[resource]` or an `Rc` is `AffineResource` (`Transfer` only, release never sunk); its `clone` stays `Retain` | revision 4's flat `Function`/`Closure`/`TraitObject` → `PersistentShare` row, which made a `move \|\| { conn… }` closure `Share`able into a second actor and its release sinkable | §1.1 aggregate rule; §2.1 rule 5's `AffineResource` row and the `LambdaPid` argument ("a second live handle would let two actors race the lambda's release"); §3 | wording (narrows row 7's relaxation to closures with no affine capture; `main` refuses every closure-into-lambda-actor capture today, so nothing moves accept → reject) |
| 34 | A machine value is an ordinary enum under the aggregate rule; `BuiltinType::{ActorState, MachineState}` are the two reserved *names*, never the type of a user value | §1.3.7's "`MachineState` stays `Linear` so a step that forgets to store back is 6d"; §1.1/§5.4 rows reading `ActorState`/`MachineState` as user-facing `Linear` values | `builtin_type.rs:1093-1108` (arity 1, roles `ActorStatePayload`/`MachineStatePayload`); `check/tests/collections.rs:3017-3020` (a user machine *named* `MachineState` normalizes to the builtin); `examples/machine/run_lifecycle.hew:5` and `examples/playground/machines/traffic_light.hew:24` hold machine values in locals and never consume them, both `.expected`-verified | wording (matches `main`; the withdrawn sentence would have rejected shipped programs) |
| 35 | A runtime-owned `AffineResource`/`Linear` place carries a taken bit with two runtime readers — `hew_drop$State` and the bit-guarded `store.assign` (§1.3.6) — and the bit is set **before** its release. So `if c { conn.close() }` in a handler is legal, a later read is `E_OWN_USE_AFTER_CONSUME`, and re-initializing a closed `#[resource]` state field is `store.assign` with a bit-guarded release; the resource closes once | `main` refuses the mutating shape entirely (`repros/ladder/state_resource_trait.hew` → `var-self receiver binding has no MIR place`) and, where it does run, runs both the explicit `close` and the assignment's implicit release: `repros/ladder/state_reinit.hew` (`conn.close(); conn = Conn.open(2)`) prints `close 1`, `close 1`, `2`, `after`, `close 2`. Also the deleted §1.3.6 bullet "`Option<Conn>` and enum fields … no take, no bit" | spec §3.7.8.4 Path 2 "The handler may also close them explicitly via `f.close()?` … an already-closed `#[resource]` is a use-after-consume diagnostic"; §1.3.6 re-initialization decision; §2.1 rule 4's three-state lattice; §5.2 item 1's bit test | relaxation (reject → accept for trait `var self` on a resource state field) + behaviour (bug fix: one `close 1`) |
| 36 | A message record or task env with a `Linear` leaf carries `hew_abandon$<T>` as its envelope/arc glue; `Linear` types keep their send fact | §5.2 item 7 read as "a `Linear` field is unreachable in *any* glue", which would ICE on a shipped program or leak the field | spec §3.7.8.4 Path 4 (storage reclaim without a consuming method) and §3.7.8.5 item 2 (a `@linear` capture into a child task, line 3095, unimplemented today — `grep -rn LinearCaptureCancellable` finds one runtime comment); `tests/hew/actor_message_ownership_transfer_test.hew:29-46` sends a `#[linear] Ticket` to an actor and is `.expected`-verified | wording (matches `main`; the ICE would have been a regression) |
| 37 | `CoerceToDynTrait` into a `dyn … + Send` requires the concrete payload to be `Send`; the `TraitObject`'s send fact is then the bound list, soundly | `main` admits the coercion on object safety alone (`coerce.rs:432-444`; `grep -n 'MarkerTrait::Send\|implements_marker' hew-types/src/check/coerce.rs` is empty) while `traits.rs:1072-1086` reads `Send` off the bound name (`dyn_trait_plus_send_is_send`, traits.rs:1799-1825) | §1.1 `TraitObject` row; §2.1 rule 5's `PersistentShare` row + §11 row 7 (dyn boxes become `hew_arc_*` shares) would otherwise let two actors race the `hew_rc_*` count inside a `dyn Handler + Send` over an `Rc`-holding concrete — the argument §11 row 33 already applied to closures; §5.4 "`hew_rc_*` is reserved for `Rc<T>`/`Weak<T>`" | tightening (accept → reject) with **no fixture to move**: `grep -rn '+ Send' std tests/vertical-slice examples` finds no `dyn … + Send` in the corpus, so the wall lands with a new P1 reject fixture and no accept fixture changes |

_This document is an internal engineering reference. The public language
specification is `docs/specs/HEW-SPEC-2026.md`; the user-facing ownership
contract is `docs/v05/ownership.md`. Where the spec's prose and this design
disagree, §11 names the sentence and the P5 doc sync fixes the spec._
