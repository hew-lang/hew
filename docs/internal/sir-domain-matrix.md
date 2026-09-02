# SIR domain matrix

Total mapping from the Hew surface (as HIR on main `54e8dde2c`) to the final
ladder in `hew-orchestration/plans/final-ladder-program.md` (§1). One row per
construct; a construct with no row is a defect in this document.

## What this revision decides (2026-09-01)

Three adversarial passes ran against this document on 2026-09-01. The first
found fourteen defects in the first draft; the second found nine more in the
first pass's output; the third found thirteen more, of which four force a
decision. All thirty-six reproduced against code; none was refuted. Every seam
any pass surfaced is decided here, because P0's gate is "no undecided ownership
mode" and this document is where the deciding happens — a P-lane implementer
must never have to arbitrate between this matrix and `docs/internal/ir-ladder.md`.

`ir-ladder.md` moved under this document between the second and third passes
and again in the reconciliation pass (it is now **revision 7**). The third
pass's D-FORK-R and D-NOMODE are this document following the ladder on ops and
classes; the reconciliation pass closed the remaining disagreements in both
directions. Line citations into the ladder were last re-grepped against
revision 6 and have drifted with revision 7's edits, so read the section and
row name and re-grep the line (Method, below).

## When these documents disagree

Quoting `hew-orchestration/plans/final-ladder-program.md` §5.1 in full, which
is the authority for this paragraph and for the identical paragraph in
`ir-ladder.md` and `runtime-ownership-table.md`:

> `docs/internal/ir-ladder.md` decides SIR ops, ownership kinds, MIR forms, and
> runtime symbol names; `docs/internal/sir-domain-matrix.md` decides which
> phase owns a construct; `docs/internal/runtime-ownership-table.md` decides a
> runtime symbol's parameter and result ownership. This plan decides sequencing
> and gates. A disagreement inside a document's own domain is a defect in the
> other document, fixed in the same PR that finds it; none of the four is a
> fallback for another.

No other precedence rule is in force. This document's domain is the **phase**
column; every op, class, kind and symbol name in a row below is the ladder's,
and a row that disagrees with the ladder about one of those is a defect in this
document.

### First pass (fourteen findings)

Nine were repairs to rows, five were seams the document had inherited rather
than closed.

| id | decision | where |
| --- | --- | --- |
| D-CATALOG | Catalog builtins (`println`, `print`, `assert*`, `to_string_*`, `len_str`, …) have **no** `RuntimeCallFamily` on main and reach codegen only through callee-name interception, which P5 deletes. P1 mints one family per catalog endpoint from its `BuiltinLinkage` symbol and lowers as `rt.call{family}`. This is P1 work, not P5 cleanup: without it P1's own corpus-parity gate cannot run, because P1's programs print | §3.3 rows + decision; §6 first family row; §9 |
| D-OPTION | A family whose result is `Option<T>` is P2, without exception — the split is by result type, not by family name. `String{CharAt,CharAtUtf8,Find,Get}` and `Bytes{Get}` split out to P2; `Bytes{Pop}` stays P1 because it returns `u8` and aborts on empty. Matches the rule the draft already applied to `TryWidthCast` and `checked_*` | §6 Bytes/String rows + decision |
| D-FORK | The op is `fork`, not `unchecked_fork`. `string` is never forked; `bytes` is (a runtime no-op that stays an explicit op). **The class precondition this row carried ("`CowValue` only, `E_SIR_ICE` otherwise") and the `BorrowMut` receiver mode it prescribed are both superseded — see D-FORK-R and D-NOMODE in the third pass below.** The spelling half stands and is now uniform: `ir-ladder.md`, `runtime-ownership-table.md` and `final-ladder-program.md` all spell it `fork` (`grep -n unchecked_fork` over the plan is empty; the plan's op bullet and its MIR mapping both read `fork`) | Legend; §2 assign row; §3.3 `RuntimeCollection`/`VarSelfMethodCall`; §6 Vec/Bytes; §7 string/bytes/Vec |
| D-PLACE | `end_lifetime %p` joins the op set. It is what discharges rule 4 for places, and it was missing from the draft entirely. (**Narrowed by D-NOPLACE below**: two of the ladder's three emission sites have a producer — the escaping-`var` site is the rule with nothing on `main` to produce it) | Legend; §1 Actor; §3.6 closure/generator envs |
| D-HANDLE | Three handle types were mistyped and are corrected against code, not against the spec: `RegexLiteralRef` produces an **`O`** cloned `Pattern` (not `N`); `SpawnLambdaActor` produces a **`LambdaPid`** (not a `Duplex`), which has `clone = Retain`; `CancelToken{Retain}` is runtime-internal and is **not** a `copy_value`. `Spawn`'s `LocalPid` stays `N`, and `ir-ladder.md` §1.3.1 (revision 7) has dropped `Spawn` from its `Owned`-producer list and states the rule: a pid is `BitCopy`, spawn produces no `Owned` value, and the owned thing is the state record (or, for a lambda actor, the captured environment) moved in at spawn | §3.4 Regex; §3.6 lambda actor; §3.8 Spawn; §6 CancelToken; §7 |

### Second pass (nine findings)

Three are phase or contract errors big enough to force a decision this document
must take rather than hand to a lane; six are repairs to rows, notation or
citations.

| id | decision | where |
| --- | --- | --- |
| D-NOPLACE | **No function-owned `Place` exists on main.** `&mut T` is a hard parse error and `&expr` is not an expression at all, so the sole escape route this document and `ir-ladder.md` both named — "a `var` whose address is taken by an extern `&`/`&mut` parameter" — is unwritable. Every remaining `alloc_place` producer is an env field (P3) or a runtime-owned place (P4). P1 therefore delivers the place op set and verifier rule 4 with **no P1 producer**: P1's "mem2reg + places" reduces to mem2reg, and rule 4's function-owned clause plus `end_lifetime` first bind at P3. **The rule itself stands and this document does not touch it** (plan §6, `ir-ladder.md` §1.3 `alloc_place` row, revision 7): an extern-addressed `var` **is** an ordinary function-owned `alloc_place`, the same rule as any other escaping `var`, and there is no third memory class. What this row decides is the phase — nothing on `main` produces one, so nothing exercises it before P3 | Legend `end_lifetime` note + phase legend item 4; §2 `Let(_, None)` and `Assign` rows; §3.2 escaping-`var` row; §3.6 `Closure{Local}` cite; Counts |
| D-BYTESLEN | **`bytes.len()` is re-pointed at `hew_bytes_len` in P1, not intercepted in codegen.** On main `b.len()` mints `RuntimeCallFamily::VecLen`, and only a codegen receiver-type intercept reaches the bytes entry — the name-keyed call-site join plan §6 forbids. P1 changes the `std/io.hew` declaration so the symbol→family bijection yields `BytesLen` directly; the intercept and `lower_bytes_len` die with the legacy route. The §6 `Bytes{Len}` row stays P1 | §6 Bytes row + decision; §8 `runtime_builtins.rs` row |
| D-METRIC | **The `Metric` family group is not P1.** Its registration half is P2 (the `metrics.counter/gauge/histogram` free functions return a record and panic through an interpolated string) and its accessor half is P3 (every accessor lives in a trait impl body over a record receiver). `Observe` stays P1 and moves to its own row | §6 Metric/Observe rows; §9 first row |

Six repairs with no new id, listed so a reader diffing against the first pass
knows what moved: the D-CATALOG P1 witness (`string_split_nonempty.hew` is
Vec-typed and ungated by a transcript — replaced), `len_vec` (a P2 endpoint
listed under P1), `IdentityCompare`'s operand kinds (`N`, not `G`, for the P4
pid case), `Unary{RawDeref}`'s op (`load.copy` takes a `Place`, not a `View`
pointer), the §6 `HashMap`/`HashSet` brace notation (which named two variants
that do not exist), and every line citation into `ir-ladder.md` (all of them
wrong — see the Method note below).

### Third pass (thirteen findings)

Six force a decision this document must take; seven are repairs to rows,
phases or citations.

| id | decision | where |
| --- | --- | --- |
| D-FORK-R | **`fork`'s class precondition follows `ir-ladder.md` revision 6, not the first pass.** The op admits `class ∈ {CowValue, AffineResource, Linear}` with a heap carrier; `E_SIR_ICE` is `BitCopy`, `View` and `PersistentShare` only. **The class decides the realization, not the legality**: `CowValue` → `ensure_unique`, `AffineResource`/`Linear` → a register move (unique by class). The first pass's "`CowValue` only" left `var v: Vec<Conn>; v.push(c)` — a shape `main` accepts — with no admitted op sequence, because `push` is not a `VarSelfMethodCall` and the move-in/move-back escape does not reach it | Legend `fork` note; §2 `Assign` field/index row; §3.3 `RuntimeCollection`; §6 Vec/Bytes; §7 bytes/Vec |
| D-NOMODE | **`BorrowMut` is not an operand mode and never appears in a row.** It is a deleted `UseMode` (`ir-ladder.md` §1.3 preamble L187) and a `ClosureCaptureMode` variant, nothing else: the plan's closed mode set is `{Borrow, Copy, Move, Snapshot{Share,DeepCopy,Transfer}}` (plan §1.1 rule 5, `ir-ladder.md` §2.1 rule 5 L1044-1049) and the header set is `ParamMode ∈ {Borrow, Consume, Retain}` (§4.2 L1287). A mutating call on an SSA value is `fork %v` then `borrow{%v'}` around the call; on a place it is `load.take %p` → `fork` → `borrow` → call → `store.init %p` (§1.3 `load.take` row L205, §11 row 12 L2532) | Legend; §3.3 `RuntimeCollection`/`VarSelfMethodCall`; §3.7 `MachineStep`; §6 Bytes/Vec; §7 Vec |
| D-CATALOG-2 | **The endpoint→symbol map is many-to-one, so the family is keyed by endpoint and the symbol is not a key.** 16 print endpoints share `hew_print_value`; `to_string_u16`/`to_string_u32` share `hew_uint_to_string`; `to_string_str`/`clone_str` share `hew_string_clone`. **Decision** (superseding the third pass's "one variant per distinct symbol", and §6's bijection with it): P1 mints **one `RuntimeCallFamily` per catalog endpoint, keyed by the endpoint id** — the `BuiltinEntry` name, which is unique by construction — and takes the **symbol from that endpoint's `BuiltinLinkage` row**, where the symbol is **not required to be unique** across families. §6's constraint is therefore endpoint↔family, never symbol↔family: `from_c_symbol` stops being a join key for catalog endpoints (it cannot be one — `from_c_symbol("hew_print_value")` has 16 answers), and the lowering joins on the endpoint the checker already resolved. Where a symbol already carries a family, the endpoint reuses it rather than minting a twin (`string_concat` → `StringConcat`, `len_vec` → `VecLen`, `len_str` → a new `StringLength`) | §3.3 D-CATALOG; §6 brace-group note; §6 catalog row |
| D-STRCMP | **`==`/`!=` and `<`/`<=`/`>`/`>=` on `string`, and `==`/`!=` on `bytes`, get families at P1.** Both `string` forms are codegen `ResolvedTy`-shape intercepts today — exactly what plan §1.4 forbids and §6's last decision row deletes — and the first draft named a symbol (`hew_string_eq`) that does not exist; the `bytes` form is a codegen fail-closed, so it is new P1 work rather than a repoint. P1 mints `StringEquals` (`hew_string_equals`), `StringCompare` (`hew_string_compare`) and `BytesEquals` (`hew_bytes_eq`, exported and today called by nothing outside its own unit tests). Ordered comparison on `string` had no row at all and gets one | §3.1 equality row + new ordering row; §6 String group |
| D-EXTERN-P1 | **P1's `string`/`bytes` domain is enumerated, not gestured at.** Nineteen of the twenty-four `#[extern_symbol]` methods in `std/string.hew`'s impl block have no `RuntimeCallFamily`, and `bytes::to_string` is a `CanonicalStdlibExternSignature` with `family: None`. All are shipped. They stay open-set `CallTarget::Extern` calls and P1 delivers **their FFI ownership rows**, not twenty new families | §3.3 `Call{Extern}` row; §1 `ExternFn` row; §6 note |
| D-BYTESIS | **`a is b` on `bytes` becomes a checker wall at P1.** A `bytes` value is a `{ptr, offset, len}` triple, so two distinct values share a `ptr` and pointer identity has no operand; `is_identity_capable` already excludes `string` (the same `CowValue` class) for the same reason it excludes records. P1 drops `Ty::Bytes => true` (`hew-types/src/check/expressions.rs:9757`); the MIR fail-closed disappears with the construct, and no `cmp.identity` row gains a `bytes` case. **This is a tightening** — a program that compiles today and aborts in MIR becomes a checker error — so it needs a `.expected` move, not just a code change | §3.1 `IdentityCompare` row |

Seven repairs with no new id: the `BindingRef{Const}` legacy owner (`expr.rs:2464`
is the `Binding` arm; the `Const` arm is 2840-2849), the `Index` on `bytes`/`string`
legacy owner (the `Option`-returning `get` lowerings, not the index ones), six
`Option`-result NYI sites still filed under P1 in §9, the `Bytes{Pop}` return type
(`u8` on the surface, `i64` at the C symbol), the §7 `HashMap`/`HashSet` class
(the ladder's F-collections rule covers all three collections, not just `Vec`),
the actor handler's payload disposition (the handler **takes** it — there is no
envelope-owned form), and the bare `ownership.rs` citations (two files share that
basename).

### D-MATH: `abs`/`min`/`max` route through `MathIntrinsic`, not the catalog row

The first pass left this open and the third pass named where to settle it —
"by reading where `intrinsic_math_generic_op_for_signature` (`calls.rs:1181`)
is consumed in `hew-hir`". Read: **they route through §6 `MathIntrinsic`; the
catalog row (D-CATALOG) never sees them**, so D-CATALOG's endpoint minting owes
them nothing.

The `GenericMathIntrinsic` rewrite picks the overload before SIR, inside HIR
lowering. `hew-hir/src/lower.rs:28389-28466` resolves the rewrite through
`stdlib_catalog::generic_math_intrinsic_callee` (`stdlib_catalog.rs:28-51`),
which returns `"abs"`/`"min"`/`"max"` for an `I64` operand and
`"abs_f"`/`"min_f"`/`"max_f"` for `F64`, then rebuilds the node as
`HirExprKind::Call { target: self.registered_symbol_target(symbol), .. }`.
`registered_symbol_target` (`lower.rs:12754-12758`) returns
`CallTarget::Runtime(family)` whenever the registry entry carries a
`builtin_family`, and the builtin seeding sets it from
`RuntimeCallFamily::from_c_symbol(builtin.name)` (`lower.rs:10395-10409`) —
which for all six names is `MathIntrinsic(_)` (`runtime_call.rs:1543-1548`).
So the HIR the matrix rows describe already carries
`CallTarget::Runtime(MathIntrinsic(AbsI64 | MinI64 | MaxI64 | AbsF64 | MinF64 |
MaxF64))`, and SIR emits `rt.call{MathIntrinsic}` like any other row of that
group. The catalog fallthrough that D-CATALOG covers is only reached by an
endpoint with **no** family, which these are not.

The realization column is unchanged from §6's `MathIntrinsic` row (LLVM
intrinsic or libm, not an FFI symbol): the catalog linkage for all six is
`BuiltinLinkage::CalleeNameDispatchOnly` (`stdlib_catalog.rs:583-625`), so
there is no C symbol and no FFI ownership row to owe. Phase stays P1.

Method (every inventory below is a grep or a parse, not memory):

**Citing `ir-ladder.md`.** That document is under concurrent revision and it
moved again between the second and third passes: it is now **revision 6**
(`head -1 ir-ladder.md`, stamped 2026-09-01 20:07, after this matrix's second
pass at 19:56), and the second pass's re-grepped lines had drifted by 20 to 250
lines — far past the "off by a few" this note used to tolerate. Every cite below
names the section and the table row first and the line second (`ir-ladder.md`
§1.3 `fork` row, L201); **the row name is the citation and the line is a
convenience**. Lines below were last re-grepped against revision 6; revision 7
moved them again (it inserts a **Design axioms** section and a precedence
paragraph before §1 and edits §1.3, §1.3.1, §5.1, §5.2, §6.4, §7, §9, §10 and
§11). Read the row name and re-grep. A P-lane implementer who finds a line
wrong must trust the row name, and must not reconcile a matrix row against a
ladder row by line number alone.

Variant counts come from `scratchpad/count_variants.py` (a brace/paren/string/
comment-aware top-level-comma scan of each `pub enum X { … }` body, printing
the variant names so the count is auditable, not asserted). Re-run it after any
rebase; a count that moves means a variant landed and this document owes it a
row. The 2026-09-01 revision corrected four counts that were wrong in the first
draft (`HirItem` 8→9, `HirExprKind` 72→70, `RuntimeCallFamily` 233→231, HIR NYI
grep 11→14 hits/11 construction sites); the tables themselves were total in
every case, but a wrong headline count is exactly the drift the "grep, not
memory" rule exists to stop, so the script replaces the eyeball count.

- HIR variants: `hew-hir/src/node.rs` — `HirItem` (L215, **9** variants:
  Function, TypeDecl, Machine, Record, Actor, Supervisor, Impl, ExternFn,
  Const), `HirStmtKind` (L1267, 6), `HirExprKind` (L1493, **70**),
  `HirMatchArmPredicate` (L2964, 7), `HirLiteral` (L3338, 8),
  `HirSelectArmKind` (L3283, 5), `HirCaptureKind` (L3237, 2),
  `HirLifecycleHookKind` (L633, 6).
- Suspend kinds: `hew-mir/src/model.rs:2772` `SuspendKind` (13 variants).
- Runtime families: `hew-types/src/runtime_call.rs:366` `RuntimeCallFamily`
  (**231** top-level variants; `MathIntrinsic`, `VecScalar`, `SinkWrite`,
  `SinkTryWrite` are the parameterized ones), `hew-mir/src/model.rs:3780`
  `CompilerCallKind` (7), `hew-types/src/builtin_names.rs:27`
  `BuiltinMethodRuntime` (4). No `RuntimeCallFamily` variant covers
  `println`/`print`/`assert*`/`to_string_*`/`len_str` — see the catalog-builtin
  rows in §3.3 and §6.
- Legacy owner functions: `grep -nE 'fn [a-z_0-9]+' hew-mir/src/lower/{actor,task,closure_gen,machine_synth,control_flow,pattern,assign,scope}.rs hew-mir/src/lower/expr/runtime_builtins.rs`;
  dispatch arms: `grep -nE 'HirExprKind::' hew-mir/src/lower/expr.rs`
  (`lower_value_inner` L2446-7100), `HirStmtKind::` (expr.rs `stmt` L1508-2260),
  `HirItem::` (`hew-mir/src/lower/mod.rs:2163-3773`).
- NYI: `grep -rn 'MirDiagnosticKind::NotYetImplemented {' hew-mir/src` = 284
  hits; 281 are production sites (excluded: `dump.rs:1970` renderer,
  `expr/binding_ty_is_plain_vec_tuple.rs:804` and
  **`hew-mir/src/lower/ownership.rs:7395`** inside `#[cfg(test)]`, whose module
  opens at `:7138`). **Two files share the basename `ownership.rs` and the bare
  name is never safe here**: `hew-mir/src/lower/ownership.rs` (8152 lines, the
  legacy lowerer — every bare `ownership.rs` cite in §8/§9 below means this one)
  and `hew-mir/src/ownership.rs` (2183 lines, the `DropClass`/`HeapLeaf`/
  `CowHeapRelease`/`VecElementRelease` file cited by full path in the Ownership
  protocols bullet). `grep -c 'MirDiagnosticKind::NotYetImplemented {'
  hew-mir/src/ownership.rs` is **0**, so a re-grep of the wrong file makes the
  citation look fabricated. HIR: `grep -rn 'HirDiagnosticKind::NotYetImplemented {' hew-hir/src`
  = **14** hits, of which **11 are construction sites** (`verify.rs` 2477,
  2510; `lower.rs` 15865, 18777, 18883, 21550, 28492, 28817, 30114, 30182,
  30624). The other three are readers, not producers: `verify.rs:2473`
  (`matches!` guard), `lower.rs:32564` and `lower.rs:39756` (match arms over an
  existing diagnostic). §9's closing paragraph counts the 11 constructions.
- Ownership protocols: `hew-hir/src/value_class.rs:341` `ValueClass`
  (`of_ty` L372-461), `hew-mir/src/ownership.rs` `DropClass` (L185),
  `HeapLeaf` (L237), `CowHeapRelease` (L1157), `VecElementRelease` (L349),
  `hew-mir/src/model.rs:7418` `DropKind`, `hew-types/src/runtime_call.rs:2717`
  `RuntimeDropDescriptor`, `std/` `consuming self` and `#[resource]` greps.

## Legend

Ownership kinds on SIR values (plan §1.1): `O` Owned (one drop obligation),
`G` Guaranteed (borrow inside a scope), `N` None (BitCopy/unit/never).

SIR ops. Existing on main (`hew-sir/src/model.rs:584-627`): `const.i`,
`const.bool`, `tuple.make`, `tuple.get`, `unary`, `binary`, `cast`, `call`;
terminators `return`, `goto`, `branch`, `unreachable` (L770-786). Ownership
ops from the plan: `copy_value`, `destroy_value`, `begin_borrow`/`end_borrow`
(written `borrow{...}` below), `move`, `fork`, `destructure`, `alloc_place`,
`load.copy`, `load.take`, `store.init`, `store.assign`, `end_lifetime`, block
arguments (`bbarg`), `suspend{Kind}` terminator with resume/cancel edges.

Two op-name notes, because the plan and the ladder spec disagree and rows below
must not inherit the ambiguity:

- **`fork %v`** is the spelling every document uses, matching `ir-ladder.md`
  §1.3 `fork` row. `unchecked_fork` was the older name and is gone from all
  four documents. The op carries a class precondition the plan's prose omits, and **D-FORK-R
  restates it from revision 6**: `fork` admits `class ∈ {CowValue,
  AffineResource, Linear}` with a heap carrier, and only `BitCopy`, `View` and
  `PersistentShare` are `E_SIR_ICE` (same row). **The class decides the
  realization, not the legality**: a `CowValue` fork is `ensure_unique` (a
  runtime no-op today, §4.3 `fork` row L1370), an `AffineResource`/`Linear`
  fork is a register move — the value is unique by class, since `let w = v` on
  a `Vec<Conn>` is a `move`. The first pass's "`CowValue` only" is withdrawn by
  the ladder itself, with the counterexample named: `var v: Vec<Conn>;
  v.push(c)` is a shape `main` accepts and has no other admitted sequence,
  because `push` is not a `VarSelfMethodCall` and `hew_vec_push_owned_move(v:
  *mut HewVec, data)` borrows the collection by pointer.
- **There is no `BorrowMut` operand mode (D-NOMODE).** `BorrowMut` survives in
  exactly two places, neither of them an operand annotation: the deleted
  `UseMode` enum that P1 replaces with the op set (`ir-ladder.md` §1.3 preamble
  L187, `hew-sir/src/model.rs:73`), and `ClosureCaptureMode::BorrowMut`, a
  capture kind (§1.1 `Closure` row L120, §3.6 below). The closed operand set is
  `{Borrow, Copy, Move, Snapshot{Share, DeepCopy, Transfer}}` (plan §1.1 rule
  5; `ir-ladder.md` §2.1 rule 5 L1044-1049) and the closed header set is
  `ParamMode ∈ {Borrow, Consume, Retain}` (§4.2 L1287). **A mutating call is
  therefore spelled one of two ways and never a third**: on an SSA value,
  `fork %v → %v'` then `borrow{%v'}` around the call (header slot `Borrow`);
  on a place, `load.take %p` → `fork` → `borrow` → call → `store.init %p`
  (§1.3 `load.take` row L205, §11 row 12 L2532). Rows below use those two
  sequences literally.
- **`end_lifetime %p`** (`ir-ladder.md` §1.3 `end_lifetime` row, L208) is the
  release op for *places*, the counterpart of `destroy_value` for values. The
  first draft omitted it and rule 4 ("every initialized slot is `destroy`ed or
  `take`n before frame exit") therefore had no op to discharge it. The ladder names three
  emission sites; two of them have rows below — actor stop via `hew_drop$State`
  (§1 Actor, P4) and environment release for a closure / lambda-actor /
  spawn-task / generator env (§3.6, P3/P4) — and the third, scope exit of an
  extern-addressed `var`, is the rule with no producer on `main` (D-NOPLACE,
  §3.2). Both sites that have a producer are runtime-owned or env-owned, so
  **no `end_lifetime` is emitted before P3**, and rule 4's
  function-owned-place clause has nothing to check at P1.

Value ops this matrix adds (each is a P-lane deliverable, named once here):

| op | meaning |
| --- | --- |
| `const.{f,char,unit,duration,str,bytes}` | literal producers; `str`/`bytes` produce `O` (heap literal retained from the constant pool) |
| `agg.make` | record/enum/array constructor from `O`/`N` parts (parts are consumed) |
| `project` | field/tuple/array/payload read of a `G` aggregate; result is `G` (borrow view) — a `copy_value` follows when the value escapes the borrow scope |
| `switch_enum` | terminator on a `G` enum's tag; payload binders are `project`s in the arm |
| `switch_int` | terminator for literal/`switch_int` matches (int/char/bool/duration) |
| `str.eq`, `bytes.eq`, `struct.eq` | structural equality over `G` operands |
| `rt.call{Family}` | call to a `RuntimeCallFamily` symbol; per-operand modes come from the FFI ownership row (plan §1.5) |
| `closure.make` | environment construction; captures consumed by mode |
| `call.indirect` | call through a closure/fn value (`G` callee) |
| `deref` | read through a `View` pointer (`*p` in `unsafe`). A *value* op, not a place op: `load.copy`'s operand is a `Place` (`ir-ladder.md` §1.3 `load.copy` row, L204) and a raw pointer is not one (the `alloc_place` producer list at L203 is closed and has no pointer). P3 deliverable; HIR rejects `*p` today (§3.1) |
| `dyn.coerce`, `dyn.call` | trait-object construction/vtable call |
| `spawn`, `send{Snapshot}`, `ask` | actor surface; `send` operand mode is `Share`, `DeepCopy` or `Transfer` |
| `trap{Kind}` | terminator (overflow, bounds, panic) |
| `cmp.identity` | pointer identity `is` |
| `machine.*` | none — machines desugar in HIR (D287); rows below say what they desugar to |

Phases (plan §4): P1 OSSA core (scalars, string, bytes, mem2reg/places, direct
calls, loops/blocks, one-form MIR, drop glue); P2 values (aggregates, enums,
patterns, Option/Result, collections, iterators, interpolation, clone,
instances); P3 callables (closures, traits, impl methods, consuming/var self,
FFI ownership rows, generators); P4 concurrency (actors, supervisors,
machines-as-desugar, async/select/timeout/fork/scope, defer, coroutine frames,
channels/streams/duplex, node/remote). Six placement decisions the plan text
does not make, taken here:

1. Plain loops and `break`/`continue`/`return` are P1: mem2reg and block
   arguments (plan §1.1) are meaningless without loop headers.
2. Catalog builtins — printing and asserting — are P1 (D-CATALOG). P1's gate is
   corpus parity on program output, so P1 must be able to produce output.
3. A family whose result is `Option<T>` is P2 even when its receiver is a P1
   type (D-OPTION), because `agg.make`/`switch_enum`/`destructure` are P2.
4. P1's "mem2reg + places" (plan §4) is **mem2reg only** at P1 (D-NOPLACE). The
   place ops (`alloc_place`, `load.copy`, `load.take`, `store.init`,
   `store.assign`, `end_lifetime`) and verifier rule 4 are P1 deliverables, but
   P1 has no program that produces a place: the closed producer list is env
   fields (P3), actor state fields, coroutine frame slots and dispatched
   message payloads (P4). P1's own negative programs for rule 4 must therefore
   be hand-written SIR, not Hew source, and its gate cannot claim place
   coverage.
5. Comparison on `string`/`bytes` is P1 and needs three new families
   (D-STRCMP). All three realizations on main are codegen `ResolvedTy`-shape
   intercepts, which plan §1.4 forbids and §6's last decision row deletes, so
   they cannot be left to P5 cleanup any more than printing could.
6. P1's `string`/`bytes` domain includes twenty shipped endpoints that have no
   family and never will (D-EXTERN-P1). They are open-set `std` externs, so
   what P1 owes them is FFI ownership rows, not families — but they are P1
   work, and neither §6's coverage proof nor the family table sees them.

Walls (plan §0): mutate a `let`; `clone` a non-cloneable; `send` a
non-sendable (incl. a resource-shaped send). Rows note which wall applies.
One construct becomes a wall in this revision that is not one today: `a is b`
on `bytes` (D-BYTESIS, §3.1).

## 1. Items (`HirItem`, node.rs:215 — 9 variants, all rowed)

| construct | source | SIR | MIR realization | runtime | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `Function` (free fn, impl method, extern-visible body) | `fn add(a: i64, b: i64) -> i64 { a + b }` | one `SemCallable` per `DefId`+instance; params carry decided mode (`Borrow` default → `G`; `consume`/`consuming self` → `O`); body values per rows below; every `O` param not moved is `destroy_value`d at each exit | one `MirFunction` per `MirCallableKey`; header row carries param modes | direct symbol from `build_direct_call_symbol_index` | P1 (scalars) / P3 (`consuming`, `var self`) | `mod.rs:3534` item arm; `mod.rs:12832` `lower_function`, `13699` `lower_params`, `14581` `function_body` | — |
| `Function` with `type_params` (generic template) | `fn id<T>(x: T) -> T` | template not lowered; instances minted by the instance service `InstanceKey{item,type_args,selected_impls}` (plan §1.5) | one `MirFunction` per instance key | mangled via `function_monomorph_symbol` | P2 (instance service) | `mod.rs:3464` (generic names collected), `mod.rs:3604` `polymorphic_mir` producer (no consumer) | — |
| `Function` with `is_generator` (`gen fn`) | `gen fn count(n: i64) -> Generator<i64, ()>` | body is a coroutine callable: `yield` → `suspend{Yield}`; frame-live `O` values destroyed on the cancel edge | coroutine frame from MIR frame description; `MakeGenerator` | `hew_gen_*` (`HeapLeaf::Generator`, release `hew_gen_free`) | P3 | `closure_gen.rs:2048` `lower_gen_block` (shared with `GenBlock`), `pattern.rs:699` yield-binding drops | — |
| `Function` with `intrinsic_id` | std `#[intrinsic]` bodies | no body; call sites use the intrinsic's `rt.call{Family}` row | none | per family | P1 | `hew-sir/src/lower.rs:724-728` `generic_template_admission` refuses (`is_generator \|\| intrinsic_id.is_some()`); legacy carry `mod.rs:13065` (`intrinsic_id: func.intrinsic_id.clone()` onto the `MirFunction` header) | — |
| `TypeDecl` (enum, opaque, indirect, `marker`, `consuming_methods`) | `type Shape { Circle(f64), Sq(f64) }` | no ops; supplies `TargetLayout` + value class (`ResourceMarker::{None,BitCopy,Resource,Linear}`) and the drop-glue function `hew_drop$<ty>` | enum layout, tag width, payload slots; `IndirectEnum` boxed | drop glue per plan §1.3 | P2 (P1 for `BitCopy` marker types) | `mod.rs:2198` layout registration, `2476` opaque, `4049-4057` opaque/indirect names | — |
| `Record` | `record Point { x: i64, name: string }` | as `TypeDecl`: layout + glue; `#[resource]` records get `AffineResource` class with a `UserClose(close)` obligation | `RecordLayout`; `RecordInPlace` glue | `hew_drop$Point`; `#[resource]` → user `close(consuming self)` | P2 (P3 for `#[resource]` close obligation) | `mod.rs:2163`; `mod.rs:1260` record field table | — |
| `Actor` | `actor Counter { state n: i64; receive fn inc(...) }` | state is one `alloc_place` per field owned by the runtime object; each `receive fn`/`init`/lifecycle hook is a callable with `self` = `G` place; handler params are `O` message payloads the handler **takes** — `Consume` header slots, and the `ActorHandler` shim `destructure`s the payload record and frees the buffer with `hew_msg_payload_free` (`ir-ladder.md` §5.6 L1814-1821: "Dispatch has one disposition: the handler takes the payload"; there is no borrowed-payload shim and no envelope-owned form); at actor stop the runtime calls `hew_drop$State`, which is `end_lifetime` on every still-initialized state place (a `#[resource]` field closed by hand is `Uninit` with its taken bit set and is skipped) | `FunctionCallConv::ActorHandler`; `ActorStateFieldLoad/Store` become `load.copy`/`store.assign` on the state place | `hew_actor_*`, mailbox policy `OverflowPolicy::{DropNew,DropOld,Block,Fail,Coalesce}` | P4 | `mod.rs:3653` item arm; `machine_synth.rs:169` receive handlers, `395` init, `490-1215` lifecycle (`Start` 515, `Stop` 582, `Crash` 643, `Exit` 926, `Down` 1046, `Upgrade` 1171 → `OnUpgradeNotYetWired`) | `Upgrade` hook: P4 rejects until a hot-upgrade spec exists (today `push_lifecycle_not_wired_diagnostic`) |
| `Supervisor` | `supervisor Tree { strategy one_for_one; children [ Worker ] }` | bootstrap callable: `spawn` per child in `supervisor_children_in_spawn_order`; child config values are `N`/`O` literals or `config.<field>` `project`s | `SynthesizedCallable` bootstrap; `SupervisorPool` is `BitCopy` | `hew_supervisor_*`, strategies `OneForOne/OneForAll/RestForOne/SimpleOneForOne` | P4 | `mod.rs:3693`; `machine_synth.rs:2297-2853` (`supervisor_children_in_spawn_order` 2107); config-value NYIs `mod.rs:3043-3349` | non-literal / non-`config.<field>` child init values stay rejected (checker fact, not ownership) |
| `Machine` | `machine Light { state Red; state Green; on Go: Red -> Green }` | none in SIR: HIR desugars to `enum <M>` + `enum <M>Event` + a `step` fn = `match (state, event)` (D287); rows for `MachineStep`/`MachineEmit`/… below say the target | ordinary enum + fn rows | none | P4 (desugar lane) | `mod.rs:3773`; `machine_synth.rs:1360-2099` (step 1360, `emit_machine_step_transition_return` 1811, `emit_machine_transition_out_drops` 2002, `lower_machine_lifecycle_block` 2055) | — |
| `Impl` (metadata; methods are sibling `Function`s) | `impl Shape { fn area(self) -> f64 }` | none; trait/impl selection is a checker fact consumed by `ResolvedImplCall`/`CallDynMethod` | none | none | P3 | `mod.rs:3749` (metadata-only), `mod.rs:1331` impl table | — |
| `ExternFn` | `extern "C" { fn hew_fs_read(path: string) -> bytes; }` | declaration row in the FFI ownership table: every param `borrow` unless declared `consume`; return `O` unless `ExternProvenance` says C-string adopt/borrow; a missing row is a build error (plan §1.5) | `ExternDecl` with `malloc_string_return` | symbol as declared | P3 (table) / P1 for the enumerated `std` string/bytes rows (D-EXTERN-P1, §3.3: the nineteen `#[extern_symbol]` methods of `std/string.hew:709-821` that have no family, plus `bytes::to_string`) | `mod.rs:3428`, `3938`; `return_provenance.rs:1508` `ExternContractTable` | — |
| `Const` | `const LIMIT: i64 = 10;` | `const.i`/`const.f`/`const.str` at each `BindingRef{Const}` use (`str` → `O` retained from pool) | `ConstGlobalLoad` | none | P1 | `consts.rs:1284-1330` descriptor table; `expr.rs:2840-2849` the `ResolvedRef::Const(item_id)` `BindingRef` arm that emits `Instr::ConstGlobalLoad` (**not** `expr.rs:2464`, which the first two revisions cited: that is the `ResolvedRef::Binding(id)` arm, the row two below) | — |

## 2. Statements (`HirStmtKind`, node.rs:1267)

| construct | source | SIR | MIR realization | runtime | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `Let(binding, Some(init))` | `let s = make();` | value of `init` bound to the SSA name; `O` result owns; scope exit → `destroy_value` if not moved | register / stack slot | — | P1 | `expr.rs:1521-2094`; owner mint `ownership.rs:582` `register_owned_local`, warrant `owner_mint.rs:155-379` | — |
| `Let(binding, None)` (declared, initialized later) | `var x: i64; x = 1;` | no SIR op at all: mem2reg finds the defining value and the binding is a `bbarg`-carried SSA value. There is no escaping branch — a local cannot have its address taken (D-NOPLACE, §3.2) — so this row never mints an `alloc_place`. Definite-init rule 4 still applies to the SSA value and rejects a read before init | register / `bbarg` | — | P1 | `expr.rs:2095` | use-before-init is a verifier error (rule 4); a mem2reg miss is `E_SIR_ICE definite-init`, never an `undef` value |
| `Let` of a tuple/record destructuring pattern | `let (a, b) = pair;` | HIR already lowers to `TupleIndex`/`FieldAccess` lets; each is `project` + `copy_value` (or `destructure` when the source is an `O` temporary) | field loads | — | P2 | `expr.rs:4901` TupleIndex; `hew-hir/src/lower.rs:15865` refuses nested patterns | — |
| `Let` binding a closure / lambda actor / fn value | `let f = \|x\| x + 1;` | see `Closure`, `SpawnLambdaActor`; the binding owns the env (`O`) | — | — | P3/P4 | `mod.rs:1222`, `1547`, `1682` (three RHS shapes special-cased) | — |
| `LetElse` | `let Ok(v) = r else { return; };` | `switch_enum` on `G` scrutinee; success arm `project`s payload (+`copy_value` when the scrutinee outlives, `destructure` when it is an `O` temp); else block must be divergent | branch + payload loads | — | P2 | `control_flow.rs:75-341`; NYI `control_flow.rs:98` | — |
| `Assign{target,value}` to a `var` local | `v = v + 1;` | new SSA def + `bbarg`; the old `O` value is `destroy_value`d. Every `var` local is non-escaping on main (D-NOPLACE), so `store.assign` never appears for this row — it appears only for env fields (§3.6, P3) and actor state fields (§3.4/§1 Actor, P4) | register / `bbarg` | — | P1 | `assign.rs:17-859` (`assign`); overwrite release `pattern.rs:2083` `emit_local_overwrite_release`, `2275` enum; `drop_plan.rs:1044` replay | assigning to a `let` is wall 1 (checker) |
| `Assign` to a field / index / actor-state target | `p.x = 1; xs[i] = v; self.n = 2;` | record field (`p.x = 1`): no fork — an inline record is unique by construction; `destroy_value` of the old field value then `store.assign`. Collection index-assign (`xs[i] = v`): **`fork` the container in every class**, then `borrow{}` it around `rt.call{VecSet*/HashMapInsertLayout}` with the `O` element operand `move`d. The write-through operand of an `Assign` through a projection of a `var` is a named `fork` producer (`ir-ladder.md` §1.3 `fork` row, L201, "Emitted for" column); only the realization varies with the class — `ensure_unique` for `Vec<i64>`/`Vec<string>`, a register move for `Vec<Conn>`/`Vec<Rc<T>>`. **Corrected in the third pass** (D-FORK-R): the second revision said "fork iff `CowValue`" and gave an `AffineResource` container no fork at all | `RecordFieldStore`, `ActorStateFieldStore`, runtime set | `hew_vec_set_*`, `hew_hashmap_insert_layout` | P2 (P4 for actor state) | `assign.rs:860` `field_store_target_path`; `assign.rs:29/66` VecIter borrow NYIs | — |
| `Expr(expr)` | `foo();` | value lowered; an `O` result in statement position is `destroy_value`d immediately | — | — | P1 | `expr.rs:2096-2108`; discarded-result NYIs `ownership.rs:3336-3382` | — |
| `Return(Some/None)` | `return x;` | `move` of the `O` return value; every other live `O` in the frame `destroy_value`d; pending `defer`s run first | `Terminator::Return` | — | P1 (P4 with defer) | `expr.rs:2119-2203`; `scope.rs:1011` `emit_defers_for_return` | — |
| `Defer{body,scope_id}` | `defer { close(f); }` | registered per scope; every exit edge (return/break/continue/fall-through/cancel) runs deferred bodies LIFO before the scope's `destroy_value`s; moved bindings referenced in the body are a checker error | inlined on each exit path | — | P4 | `expr.rs:2204-2212`; `scope.rs:128` `emit_pending_defers`, `1011`, `1043` | — |

## 3. Expressions (`HirExprKind`, node.rs:1493 — 70 variants, all rowed across §3.1-§3.9)

### 3.1 Literals, scalars, numerics

| construct | source | SIR | MIR realization | runtime | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `Literal(Integer)` | `42` | `const.i` → `N` | `ConstI` | — | P1 (exists) | `expr.rs:2455` → `lower_literal` (~7150-7250) | — |
| `Literal(Bool)` | `true` | `const.bool` → `N` | `ConstI` i1 | — | P1 (exists) | same | — |
| `Literal(Float)` | `1.5` | `const.f` → `N` | `FloatLit` | — | P1 | same; NYI `expr.rs:7192` | — |
| `Literal(Char)` | `'a'` | `const.char` → `N` | `CharLit` | — | P1 | same | — |
| `Literal(Duration)` | `5s` | `const.duration` → `N` (i64 ns) | `DurationLit` | — | P1 | same | — |
| `Literal(Unit)` | `()` | `const.unit` → `N` | `UnitLit` | — | P1 | same | — |
| `Literal(String)` | `"hi"` | `const.str` → `O` (pool literal, refcount +1) | `StringLit` | `hew_string_*` COW; release `hew_string_drop` | P1 | same; `temp_drop.rs:5507` `finalize_string_ownership` | — |
| `Literal(Bytes)` | `b"AB"`, `bytes[0x41]` | `const.bytes` → `O` | `BytesLit` | `hew_bytes_*`; release `hew_bytes_drop` | P1 | same; `temp_drop.rs:8687` `finalize_bytes_ownership` | — |
| `Binary{Add..Modulo, Wrapping*}` on ints | `a + b` | `binary` (`N`,`N`)→`N`; checked ops split to `trap{IntegerOverflow}` | `IntArithChecked` + `Trap` / `IntAdd`… | — | P1 (exists; Divide/Modulo/Shl/Shr bridge gap `hew-mir/src/sir.rs:1994-2003`) | `expr.rs:3040-3058`, helpers `7290-8100` (NYIs 7307, 7330, 7621, 7687, 7899, 7946, 8064, 8081) | — |
| `Binary` on floats | `x * 2.0` | `binary` → `N` | `FloatAdd/Sub/Mul/Div/Rem/Cmp` | — | P1 | same | — |
| `Binary{Equal..GreaterEqual}` on scalars | `a < b` | `binary` → `N` bool | `IntCmp`/`FloatCmp` | — | P1 (exists) | same | — |
| `Binary{Equal,NotEqual}` on `string` | `s == "x"` | `borrow{lhs,rhs}` `str.eq` → `N` | `CallRuntimeAbi{StringEquals}` — **P1 mints the family (D-STRCMP)**; today the call site is `Instr::IntCmp` and codegen recovers "this is a string" from the operands' `ResolvedTy` | `hew_string_equals` (`hew-runtime/src/string.rs:815`; the first draft named `hew_string_eq`, which does not exist anywhere in the tree) | P1 | `expr.rs:7395` `lower_binary`, whose comparison fall-through pushes `Instr::IntCmp { dest, pred, lhs, rhs }` at `expr.rs:7551` with no string case at all (`temp_drop.rs:4921-4924` documents the split); the type is recovered downstream by the codegen intercept `hew-codegen-rs/src/llvm.rs:14216-14260`, pinned by `llvm_tests.rs:2929 string_intcmp_eq_uses_string_equals_for_string_operands` and `:2981 non_string_pointer_intcmp_eq_fails_closed_without_string_equals` | — |
| `Binary{Less,LessEqual,Greater,GreaterEqual}` on `string` | `if a < b` | `borrow{lhs,rhs}` `rt.call{StringCompare}` → `N` i32, then `binary` of that against `0` with the surface predicate. **No new SIR op**: the strcmp sign convention is arithmetic, not a comparison primitive | `CallRuntimeAbi{StringCompare}` + `IntCmp` — **P1 mints the family (D-STRCMP)** | `hew_string_compare` (`hew-runtime/src/string.rs:790`, "returns an i32 with the same sign" as C `strcmp`) | P1 | `expr.rs:7395` `lower_binary` → `expr.rs:7551` (the same untyped `IntCmp` push as equality). **This row had no entry before the third pass and the construct ships**: `hew run` on `let a: string = "apple"; let b: string = "banana"; if a < b { println("lt") }` prints `lt`. The whole realization is the codegen intercept `hew-codegen-rs/src/llvm.rs:14262-14320`, pinned by `llvm_tests.rs:3018 string_intcmp_ordering_uses_hew_string_compare`; no `RuntimeCallFamily` exists (`grep -nE '^    String[A-Za-z]*,' hew-types/src/runtime_call.rs` lists CharAt, CharAtUtf8, CharCount, Concat, Find, Get, Index, SliceCodepoints and no compare) | — |
| `Binary{Equal,NotEqual}` on `bytes` | `a == b` | `borrow{lhs,rhs}` `bytes.eq` → `N` | `CallRuntimeAbi{BytesEquals}` — **P1 mints the family (D-STRCMP)** | `hew_bytes_eq(a_ptr, a_off, a_len, b_ptr, b_off, b_len) -> bool` (`hew-runtime/src/bytes.rs:751`) — exported and called by nothing outside its own unit tests, the same repoint shape as D-BYTESLEN | P1 (**fails closed today** — this is new P1 work, not an existing path) | `hew run` on `let a: bytes = b"AB"; let b: bytes = b"AB"; if a == b {…}` → `E_NOT_YET_IMPLEMENTED: fail-closed: IntCmp aggregate operands must be named structural-equality types; got lhs=Bytes, rhs=Bytes`. The refusal is **codegen's**, not MIR's: `hew-codegen-rs/src/llvm.rs:14079-14089` admits only `(Named, Named)` and `(Tuple, Tuple)` aggregate operands | — |
| `Binary{Equal}` on records/enums/tuples (structural `==`) | `p == q` | `borrow` both, `struct.eq` (per-field recursion generated with the drop glue) → `N` | generated eq helper | none | P2 | `expr.rs:7482`, `7516` NYIs | — |
| `Binary{And,Or}` | `a && b` | short-circuit CFG: `branch` + `bbarg` bool | blocks | — | P1 | `control_flow.rs:2280` `lower_logical_and`, `2341` `lower_logical_or` | — |
| `Binary{BitAnd,BitOr,BitXor,Shl,Shr}` | `a & b` | `binary` → `N` | `IntBit*`, `IntShl/Shr` | — | P1 (exists for bit ops) | `expr.rs:7899-8081` | — |
| `Binary{Range,RangeInclusive}` | `0..n` (only inside `ForRange`/`Slice` after HIR desugar) | no value: consumed by `ForRange`/`Slice` rows | — | — | P1 | `hew-hir/src/lower.rs:20497` rejects a bare range | bare range value is a HIR error |
| `Unary{Not,Negate,BitNot}` | `!b`, `-x`, `~m` | `unary` → `N`; `Negate` checked → `trap` | `BoolNot`, `IntNegChecked`, `FloatNeg`, `IntBitNot` | — | P1 (exists) | `expr.rs:3059-3063`, `7759`, `7800` | — |
| `Unary{RawDeref}` | `*p` in `unsafe` | `deref` (the value op in the Legend table) → `N`/`O` by pointee class. **Not `load.copy`**, which the first revision prescribed: `load.copy`'s operand is a `Place` (`ir-ladder.md` §1.3 `load.copy` row, L204) and `Pointer`/`Borrow` are `View`-class *values* (§1.1 `Slice`/`Pointer`/`Borrow` row, L123; kind `None` at §1.2 kinds table, L175), never Places — the `alloc_place` producer list (§1.3 `alloc_place` row, L203) is closed and has no raw pointer. If P3 implements this row it must name a new op | load | — | P3 (FFI) | `hew-hir/src/lower.rs:21935-21942` "raw pointer unary dereference" unsupported | HIR rejects today; P3 decides implement (FFI-only) or keep rejecting |
| `NumericCast` | `x as i32` | `cast` → `N` | `NumericCast` | — | P1 (exists) | `expr.rs:3064-3094` | — |
| `SaturatingWidthCast` | `x.saturating_as_i8()` | `cast{saturating}` → `N` | `SaturatingWidthCast` | — | P1 | `expr.rs:3095-3125` | — |
| `TryWidthCast` | `x.try_to_u8()` | `cast{try}` → `N` (`Option<W>` of a BitCopy payload is `N`) | `TryWidthCast` → Option layout | — | P2 (needs Option) | `expr.rs:3126-3158` | — |
| `NumericMethod{Wrapping,Checked,Saturating}×{Add,Sub,Mul}` | `a.checked_add(b)` | `binary{family}` → `N` (checked returns `Option<int>`, `N`) | `IntArithCheckedOption`, `IntArithSaturating`, wrapping | — | P1 (checked_* P2 for Option) | `expr.rs:3185-3233` | — |
| `IdentityCompare` | `a is b` | `cmp.identity` → `N`. Operand kinds split by class: **`(G,G)`** for the heap-backed cases (`Vec`/`HashMap`/`HashSet`, user `enum`), **`(N,N)`** for `LocalPid` and machine/actor handles — those are `BitCopy` (`ir-ladder.md` §1.1 `LocalPid`/`HewActor` row, L105), so their `OwnKind` is `None` (§1.2 kinds table, L175) and "`BitCopy` values have no ownership ops" (L210): a `begin_borrow` of a pid is not admissible. The heap cases borrow because §1.3 `begin_borrow` row (L199) covers `IdentityCompare` operands **of heap type** only. **`bytes` is not in either set — D-BYTESIS makes it a wall** | `IdentityCompare` | — | P2 (collections, enums) / P4 (pids, actors, machines); `bytes` is **P1 (reject)** | `expr.rs:5000-5014` | checker fact (#3134); `a is b` on `bytes` becomes a checker rejection at P1 (D-BYTESIS) |

**Decision — `is` on `bytes` is a checker wall (D-BYTESIS).** The second pass
left this row hedged ("if P1 lands it, it is `(G,G)` at P1"), which means no
lane owns a P1-domain construct. Closing it: a `bytes` value is a
`{ptr, offset, len}` triple, not a handle — every bytes endpoint takes the three
fields apart (`hew_bytes_index(ptr, offset, len, i)`, `hew_bytes_eq(a_ptr,
a_off, a_len, …)`, `hew-runtime/src/bytes.rs:751, 1018`) — so two distinct
`bytes` values share a `ptr` whenever one is a slice of the other, and
`cmp.identity` has no operand that means what `is` promises. The checker's own
`is_identity_capable` (`hew-types/src/check/expressions.rs:9754-9800`) already
excludes `string` and user records on exactly this reasoning ("a copy-on-write
value with structural `==` and no pointer identity"), and `bytes` is the same
`CowValue` class as `string` (§7). `Ty::Bytes => true` at `:9757` is the
outlier. **P1 deletes that arm**; the MIR/codegen fail-closed (`hew run` on
`let a: bytes = bytes.new(); if a is a {…}` →
`E_NOT_YET_IMPLEMENTED: fail-closed: IdentityCompare lhs must be a pointer or
integer value`) disappears with the construct rather than being implemented,
and no `cmp.identity` row gains a `bytes` case. This is a **tightening**: a
program that compiles today and aborts in MIR becomes a checker error.

### 3.2 Bindings, places, context

| construct | source | SIR | MIR realization | runtime | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `BindingRef{Binding}` of a `let`/`var` | `x` | SSA use; `N` values copy; `O` values: a read-only use is inside a `borrow{x}`; a consuming use is `move x` (proven last use) or `copy_value x` | register/slot load | — | P1 | `expr.rs:2464-3039` (the `BindingRef` arm chain; **this row is the `ResolvedRef::Binding(id)` arm at 2464** — 2840 in the same chain is the `Const` arm, rowed below, and 2863/3014/3039 are the `Item`/`Builtin` arms), `5374`/`5704` `__hew_array_` temps | rule 2 liveness: use after `move` is a verifier error |
| **no construct** — an escaping `var` (a local whose address is taken) is unwritable on main (**D-NOPLACE**) | `unsafe { fill(&mut n) }` — rejected | none. Both halves of the source form are parse errors: `&mut T` as a type (`hew-parser/src/parser/types.rs:132-157`, the whole `Some(Token::Ampersand)` arm: it errors on `Mut \| Var` — "mutable borrow types are not supported; use `*mut T` for FFI or `T` in ordinary Hew code" — and errors again on a bare `&T` outside `TypeParseContext::Extern`) and `&x` as an expression (`hew-parser/src/parser/expressions.rs:237`: "`&` is not a prefix operator; Hew has no reference or borrow expression"). `&T` parses only inside `TypeParseContext::Extern`, and its one corpus user (`tests/ll-oracle/corpus/ffi_borrow_boundary.hew`) passes a `&i64` **returned** by FFI, never the address of a local; `grep -rn '&mut' --include=*.hew .` over the repo is empty. Probes: `hew run` on a `fn fill(p: &mut i64)` extern + `fill(&mut n)` call, and on `fn peek(p: &i64)` + `peek(&n)` — both fail in the parser | — | — | **P1 (nothing to build)** — every `var` is covered by the `BindingRef{Binding}` row above; this row exists so no lane goes looking for the escaping case | no legacy owner: every `var` is a MIR local today and drop flags decide release. `assign.rs:657` NYI ("reassigning owned captured") is the *closure-capture* case, which is an env-field place owned by the closure row (§3.6, `ir-ladder.md` §1.3 `alloc_place` row L203), not this row | — |
| `BindingRef{Item}` (named fn as value) | `let f = add;` | `closure.make{empty env, fn ptr}` → `O` (PersistentShare pair) | `MakeClosure` / fn pointer pair | closure-pair drop | P3 | `expr.rs:2876`, `2908`, `2935` NYIs; `closure_gen.rs:1072` `lower_named_fn_invoke_shim` | generic fn as value stays rejected (needs instance) |
| `BindingRef{Const}` | `LIMIT` | see `Const` item row | `ConstGlobalLoad` | — | P1 | `expr.rs:2840-2849` (`grep -n 'ResolvedRef::Const' hew-mir/src/lower/expr.rs` returns exactly `2842`) | — |
| `BindingRef{Builtin(family)}` used as a value | `let f = hew_vec_len;` | rejected: builtins are call-position only | — | — | P1 (reject) | `expr.rs:3019` NYI | wall: rejected in checker/SIR |
| `BindingRef{Unresolved}` | (checker boundary) | never lowered | — | — | P1 (reject) | `hew-hir` `CheckerBoundaryViolation` | HIR boundary error |
| `ContextReader{ActorId,Supervisor,TraceSpan}` | `context.actor_id` | `rt.call{ActorSelf}`-style context read → `N` | `ContextField` | runtime context | P4 | `expr.rs:2456-2463` | — |
| `ActorSelf` | `this` | `rt.call{ActorSelf}` → `N` (non-owning `LocalPid` leaf) | `CallRuntimeAbi(hew_actor_self)` | `hew_actor_self` | P4 | `expr.rs:4838-4846` → `runtime_builtins.rs:783` `emit_actor_self_handle` | — |
| `SubsumedValue{source,producer}` | (HIR-internal: a produced-value carrier re-anchor) | transparent: lowers `source` | — | — | P1 | `expr.rs:6851-6853`, `2369` | — |
| `Unsupported(reason)` | (HIR boundary node) | refused at HIR→SIR with the reason | — | — | P1 (reject) | `expr.rs:7083` | HIR boundary error |

### 3.3 Calls

| construct | source | SIR | MIR realization | runtime | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `Call{target: User}` (direct user fn) | `f(a, s)` | `borrow{args}` `call` (`G` args, `O` result if heap) `end_borrow`; `consume` params get `move`/`copy_value` | `Terminator::Call{Direct}` keyed by `MirCallableKey` (deletes the callee-string join `hew-mir/src/sir.rs:1837`) | direct symbol | P1 (exists for scalars) | `expr.rs:3300-3833` Call arm (`User` 3380), `9228` `lower_direct_call`, `9392` `_with_authority`, `move_value.rs:1004` args; NYIs `expr.rs:3390`, `3405`, `3683` | — |
| `Call{target: ImplMethod}` | `p.area()` | as User with `self` as first operand (`G` by default; `O` for `consuming self`) | `Terminator::Call` | — | P3 | `expr.rs:3583`, NYIs `3597`, `3612` | — |
| `Call{target: Extern}` | `unsafe { hew_fs_read(p) }` | `rt.call`-shaped `call` with operand modes from the extern's FFI ownership row; `O` result when the row says owned/adopt | `Terminator::Call` to extern symbol | declared symbol | P3 (table) / **P1 for the enumerated `std` string/bytes extern set — see D-EXTERN-P1 under the table; the second pass's carve-out named only the two `extern "C"` block symbols at `std/string.hew:848-851` and left the nineteen impl-block methods unbudgeted** | `expr.rs:3633`; `9538` NYI (ownership-opaque extern) | missing row = build error |
| `Call{target: Runtime(family)}` | `hew_bytes_push(b, x)` | `rt.call{family}` (see §6 rows) | `CallRuntimeAbi` | per family | P1 (per §6 family row) | `expr.rs:3426` → `runtime_builtins.rs:11` `lower_runtime_call`; NYI `expr.rs:3464` "no direct MIR route" | — |
| `Call{target: Builtin{endpoint}}` — **catalog free function**: `println_*`, `print_*`, `assert`, `assert_eq`, `assert_ne`, `to_string_*`, `len_str`, `string_concat`, `panic`, `exit`, `random.gauss`/`randint`/`random`/`seed` (see the scoping note under the table — the closed identity list carries other names that belong to other rows). **`len_vec` is not in this P1 set**: `len_name_for_ty` mints it only for a `Vec` receiver (`hew-hir/src/stdlib_catalog.rs:2976`, sibling `2975` → `len_str` for `String`), and `Vec` is P2 everywhere in this document, so no P1 program can reach the endpoint — it lands with the §6 `Vec` row. The sibling case was checked and needs no change: `to_string_name_for_ty` (`stdlib_catalog.rs:2919-2933`) and `assert_eq_name_for_ty`/`assert_ne_name_for_ty` (`2935-2971`) map only scalars and `String`, so `to_string_*` and `assert_eq_*` are correctly P1 | `println(x)`, `assert_eq(a, b)`, `s.len()` | `rt.call{family}` with `G` operands for string/bytes and `N` for scalars; result `N` for the print/assert endpoints, `O string` for `to_string_*`/`string_concat`. **See D-CATALOG below the table — this row has no `RuntimeCallFamily` on main and P1 must mint one.** | `Terminator::Call` to the catalog symbol (`module_fn_names` must contain it) | the `runtime_symbol`/`symbol` field of the endpoint's `BuiltinLinkage` row | **P1** (scalars, `string`, `bytes` — plan §4 puts these squarely in P1's domain and P1's gate is corpus output parity) | `expr.rs:3484-3555` `CallTarget::Builtin` arm, symbol joined by catalog `ItemId` through `return_provenance::stdlib_shim_emitted_symbol`; NYI `3509` ("has no direct MIR route") | — |
| `Call{target: Builtin{endpoint}}` — **method on a builtin named type** | `stream.close()`, `tok.is_cancelled()` | `rt.call{family}` selected by `BuiltinMethodRuntime::{Fixed,IntegerOverload,ElementOverload}` (`hew-types/src/builtin_names.rs:27-38`, consumed once at `hew-types/src/check/mod.rs:243-262` to register the method's symbol) | `CallRuntimeAbi` | per family | P2 (per §6 family row); P4 for the handle families | `expr.rs:3484`; NYI `3509` | — |
| `Call{target: RuntimeCollection}` | `m.insert(k, v)` | `rt.call{HashMap*/HashSet*/Vec*Layout}`; `O` key/value operands consumed (`move`); the receiver is **`fork`ed and then borrowed** — `fork %m → %m'` then `borrow{%m'}` around the call, header slot `Borrow` — because the runtime takes the collection by pointer and returns nothing. The `fork` is emitted for **every** heap-carrier class and only its realization varies (`CowValue` → `ensure_unique`; `AffineResource`/`Linear` → a register move): D-FORK-R, `ir-ladder.md` §1.3 `fork` row L201. There is no `BorrowMut` mode (D-NOMODE). When the receiver is a place (an actor state field, an env field) the sequence is `load.take %p` → `fork` → `borrow` → call → `store.init %p` (§1.3 `load.take` row L205) | `CallRuntimeAbi` with layout descriptor | `hew_hashmap_*_layout`, `hew_hashset_*_layout`, `hew_vec_*_layout` | P2 | `expr.rs:3660` (routed to `ResolvedImplCall`) | — |
| `Call{target: DynamicVtable/StaticTraitMethod}` | (routed) | see `CallDynMethod`/`CallTraitMethodStatic` | — | — | P3 | `expr.rs:3660-3662` | — |
| `Call{target: IndirectFunctionValue}` | `f(x)` where `f: fn(i64)->i64` | `borrow{f}` `call.indirect` (`G` callee, `G`/`O` args by callee header) → `O`/`N` | `CallClosure` | closure-pair ABI | P3 | `expr.rs:3673`; NYI `3823` | — |
| `Call{target: Unsupported{reason}}` | (checker boundary) | refused | — | — | P1 (reject) | `expr.rs:3651` | checker error |
| `Call` whose callee is a `VecIter`-typed value or arg | `next(it)` | see `VecIter` rows §3.5/§7 | — | — | P2 | NYIs `expr.rs:3735`, `9284`, `9488`, `mod.rs:13801` | — |
| interpolated string (HIR desugar of `Expr::InterpolatedString`) | `"x = {x}"` | chain of `rt.call{StringConcat}` and `rt.call{StructuralFormat}`; each intermediate `O` string `destroy_value`d after the next concat consumes it | `CallRuntimeAbi` | `hew_string_concat`, structural format | P2 | `hew-hir/src/lower.rs:12110` `lower_interpolated_string`, `11982`; `expr.rs:3438` NYI, `runtime_builtins.rs:704/717` | — |
| `?` operator (HIR desugar of `Expr::PostfixTry` to `Match` + early `Return`) | `let v = f()?;` | `switch_enum` on the `O` temp: Ok arm `destructure` → `O` payload; Err arm `agg.make{Err}` + return path destroys other live `O` | branch + return | — | P2 | `hew-hir/src/lower.rs:29585` (`question-operator` unsupported shapes) | — |
| `ResolvedImplCall{target_family: Vec/HashMap/HashSet}` (generic builtin methods) | `xs.push(v)`, `m.get(k)` | `rt.call{family}`; ownership per `RuntimeResultAuthority`: `IndependentOwned` → `O` result, `InteriorAliasOfReceiver` → `G` (+`copy_value` on escape), `IndependentBitCopy` → `N`, `FailClosed` → reject | `CallRuntimeAbi` | per family | P2 | `expr.rs:5174-5894` (NYIs 5339, 5469, 5485, 5523, 5539, 5580, 5623, 5656, 5677, 5861) | `FailClosed` authority is a wall (no protocol) |
| `ResolvedImplCall` to user impl method | `p.area()` | as `Call{ImplMethod}` | — | — | P3 | same arm | — |
| `CallTraitMethodStatic` (deprecated) | `x.describe()` on `T: Describe` | instance service resolves `(concrete_ty, trait, method)` → direct `call` | `Terminator::Call` | — | P3 (deleted with HIR migration to `ResolvedImplCall`) | `expr.rs:5895-6095` (NYI 5965), `8831` | — |
| `VarSelfMethodCall{Direct/StaticTrait}` (`var self` receiver) | `xs.sort()` with `fn sort(var self)` | `CowValue` receiver: `fork %recv → %recv'` then `borrow{%recv'}` around the call (header slot `Borrow`; **not** a `BorrowMut` mode, which does not exist — D-NOMODE). `AffineResource`/`Linear` receiver (`var c: Conn; c.reset()`): **no fork here** — this is the one shape D-FORK-R still exempts, because the dual return makes it explicit: the receiver is `move`d in and the returned `Self` is `move`d back into the binding, unique by class (`ir-ladder.md` §1.3 `fork` row, L201, "Emitted for" column: the receiver of a `VarSelfMethodCall` **when it is `CowValue`") | `Terminator::Call` with `ParamBoundaryMode::BorrowRepresentationLoan` collapsed into the header row | — | P3 | `expr.rs:6096-6112`, `8940-9140` (NYIs 8955, 8984, 9122, 9136); `move_value.rs:331` | — |
| `CallDynMethod` | `shape.area()` on `dyn Shape` | `borrow{recv}` `dyn.call{slot}` → result by signature | `CallTraitMethod` | vtable statics | P3 | `expr.rs:5084-5173`; NYI `5109` | — |
| `CoerceToDynTrait` | `let s: dyn Shape = circle;` | `dyn.coerce` consumes `O` value → `O` trait object (`TraitObjectStorage::{FrameOwned,HeapBoxed}`) | `CoerceToDynTrait`; `DropKind::TraitObject` → glue call through vtable drop slot | `hew_dyn_box_alloc/free` | P3 | `expr.rs:5015-5083` | — |
| `RecordCloneCall` (`clone x` on records) | `clone p` | `copy_value`-equivalent structural clone: `borrow{p}` + generated clone glue → `O` | `RecordCloneInplace` / `EnumCloneInplace` | generated clone helper | P2 | `expr.rs:6854-7082` (NYIs 6935, 6958, 6973, 7020) | wall 2: clone of a non-cloneable (drop-only member) rejected |
| `clone` on collections/strings (HIR `Expr::Clone` → `ResolvedImplCall`/`CopyCloneNoop`) | `clone xs` | `copy_value` (retain) is the semantic clone for COW values; eager deep copy only for `VecCloneOwned`/`HashMapCloneLayout` when the checker rewrite demands | `CallRuntimeAbi` | `hew_vec_clone*`, `hew_hashmap_clone_layout`, `hew_string_clone` | P2 | `hew-hir/src/lower.rs:19666`, `28689-28727`; `expr.rs:5469-5677` | — |
| `WireCodec{Encode,Decode,ToJson,FromJson,ToYaml,FromYaml}` | `wire.encode(v)` | `borrow{v}` codec call → `O` bytes/string or `O` decoded value | `WireCodec` | wire runtime (codegen `wire.rs`) | P2 | `expr.rs:3282-3299` | decode-failure drop path (`wire.rs:137` `emit_de_drop_owned`) is replaced by drop glue |
| `RcIntrinsic{New,Clone,GetCopy,Set,Downgrade,StrongCount,WeakCount,IsUnique,WeakClone,WeakUpgrade}` | `Rc.new(v)` | `rt.call{Rc*/Weak*}`; `New` consumes `O` payload; `Clone`/`WeakClone` produce a new `O` handle; `Downgrade`/`WeakUpgrade` produce `O`; counts `N` | `RcIntrinsic` | `hew_rc_*`, `hew_weak_*` | P2 | `expr.rs:3241-3253`, `rc_intrinsic.rs:1-29` | — |
| `CancellationTokenIsCancelled` | `tok.is_cancelled()` | `borrow{tok}` `rt.call{CancelTokenIsRequested}` → `N` | `CancellationTokenIsCancelled` | `hew_cancel_token_*` | P4 | `expr.rs:3234-3240` | — |
| `GeneratorNext` | `g.next()` | `borrow{g}` `rt.call{gen next}` → `O Option<Y>` (yielded value transfers to caller) | `GeneratorNext` | `hew_gen_*` | P3 | `expr.rs:3254-3281` | — |

**Scoping the catalog row — which names it owns, and which it does not.**
There are two ways a call reaches `CallTarget::Builtin { endpoint }`: the
checker's closed identity list `MONOMORPHIC_CALLABLE_IDENTITIES`
(`hew-types/src/stdlib_catalog_identity.rs:12-46`), and HIR overload resolution
for `println`/`print`/`to_string`/`assert_eq`/`assert_ne`/`len`
(`hew-hir/src/stdlib_catalog.rs:2799` `is_overloaded_builtin`, `2973`
`len_name_for_ty`, resolved to a concrete catalog row by `build_catalog_call`,
`hew-hir/src/lower.rs:11598`) — which is why the overload-only spellings
(`println_i32`, `println_u8`, `assert_eq`, `len_str`, `len_vec`) are catalog
rows without being in the closed list. `len_vec` is a catalog row and still not
a P1 endpoint: the row above rules it out by receiver type. Four groups of
names in that closed list are **not** this row, and listing them here would
double-row constructs the document already covers:

- `sqrt`, `floor`, `ceil`, `pow`, `round` — pre-empted before the catalog
  fallthrough by `intrinsic_runtime_target_for_signature`
  (`hew-types/src/check/calls.rs:1290-1291`, table at `1138-1176`), which
  returns `CallTarget::Runtime(MathIntrinsic(_))`. Rowed at §6 `MathIntrinsic`.
- `abs`, `min`, `max` — deliberately excluded from that pre-emption
  (`calls.rs:1164-1170`: "never freeze one of the overloads here") and carrying
  a type-directed `GenericMathIntrinsic` rewrite instead. They reach the catalog
  arm only in the checker; **HIR lowering resolves the rewrite to a concrete
  overload symbol and stamps `CallTarget::Runtime(MathIntrinsic(_))` on the
  call** (`lower.rs:28389-28466` → `registered_symbol_target` `:12754` →
  `builtin_family` from `from_c_symbol` `:10395`), so they are rowed at §6
  `MathIntrinsic` and **not** at this row. Decided as D-MATH above.
- `sleep`, `sleep_until` — rowed at §3.9 as `suspend{Sleep}`/`suspend{SleepUntil}`
  (P4). Their catalog identity is how the callee is named, not how it lowers.
- `Node::allow_peer`/`connect`/`id`/`identity_key`/`load_keys`/`set_transport`/
  `shutdown`/`start` — rowed at §6 `Node` (P4); `bytes::new` — rowed at §6
  `Bytes{New}` (P1).

`random.gauss`/`randint`/`random`/`seed` do belong to this row: like print and
assert they are catalog identities with no `RuntimeCallFamily` on main, and
D-CATALOG mints theirs at P1 along with the rest.

**Decision — catalog builtins get a `RuntimeCallFamily` at P1 (D-CATALOG).**
The first draft of this document gave `println`/`print`/`assert*`/`to_string_*`/
`len_str` a single row that said "`rt.call{family}` selected by
`BuiltinMethodRuntime`". Both halves were wrong and the consequence was
blocking, so the correction is recorded here rather than left to a lane.

What main actually does: the checker returns `CallTarget::Builtin { endpoint }`
for every monomorphic catalog identity (`hew-types/src/check/calls.rs:1300-1315`),
`println_i64`/`println_str` are catalog identities
(`hew-types/src/stdlib_catalog_identity.rs:36-37`), HIR builds the call through
`build_catalog_call("println_str", …)` (`hew-hir/src/lower.rs:12101-12103`) over
the `print_entry!` rows at `hew-hir/src/stdlib_catalog.rs:688-703`, and
`assert_eq`/`assert_ne` are overloaded catalog builtins
(`stdlib_catalog.rs:2802`). MIR states the gap outright at
`hew-mir/src/lower/expr.rs:3484-3494`: "Catalog builtin endpoints … some use a
codegen-only linkage such as `PrintIntercept` and therefore have no
`RuntimeCallFamily`." The 231-variant enumeration confirms it — no print, assert
or `len_str` member exists. `BuiltinMethodRuntime` is unrelated: it is method
metadata on builtin *named types*, read once in `check/mod.rs:243-262`, and it
never sees a free function.

So on main the only route that prints is the legacy lowerer's `Terminator::Call`
plus codegen callee-name interception — precisely what plan §2 deletes at P5 and
what plan §6's last decision row ("name-keyed call-site joins in codegen")
forbids. `BuiltinLinkage::PrintIntercept` is itself marked transitional in
source (`hew-hir/src/stdlib_catalog.rs:58-64`).

Decision: **P1 mints one `RuntimeCallFamily` per distinct runtime symbol behind
a catalog endpoint** — keyed by symbol, not by endpoint (**D-CATALOG-2**, third
pass; the second revision said "per endpoint" and that rule cannot be applied,
see the sub-decision below) — taking the symbol from the linkage row
(`PrintIntercept.runtime_symbol`, `RuntimeFfiShim.symbol`, `ToStringShim.symbol`,
`StringCloneShim.symbol`), and the call lowers as an ordinary `rt.call{family}`
joined by `MirCallableKey`. `CompilerIntrinsic`, `CalleeNameDispatchOnly`,
`NodeRegisterByPid` and `LayoutDescriptorSymbol` rows keep their existing
non-family handling and are P3/P4. This is a P1 deliverable, not a P5 cleanup:
without it P1 cannot run its own gate. P1's declared domain is "all scalars +
string + bytes" and its gate is corpus parity on program output, and the P1
witnesses are unrunnable otherwise. Three gated fixtures, each inside P1's
domain and each pinning a catalog endpoint's *observable output*:

- `tests/vertical-slice/accept/string_slice_full.hew` — `let s: string = "hié";
  let slice = s[..]; println(slice); slice.len()`. Nothing but `const.str`,
  `Slice` on `string`, `println_str` and `len_str`. Gated by
  `run_accept_expect_status_and_stdout "string_slice_full" 4` (run.sh:3995),
  `.expected` = `hié`.
- `tests/vertical-slice/accept/assert_eq_fail.hew` — `let actual: i64 = 2 + 2;
  assert_eq(actual, 5);`. Scalars only. Gated by
  `run_accept_expect_status "assert_eq_fail" 134` plus a `grep -q 'assertion
  failed: assert_eq(4, 5)'` over stderr (run.sh:1059-1060), which is the
  strongest catalog witness in the corpus: it pins the endpoint's abort *and*
  its message text.
- `tests/vertical-slice/accept/assert_eq.hew` — the positive pair, status 0
  (run.sh:948).

The first revision named `tests/vertical-slice/accept/string_split_nonempty.hew`
as the second witness. That was wrong twice over and is withdrawn:
`string.split` returns `Vec<string>` (`std/string.hew:634`), so the fixture is
Vec-typed end to end — `parts.len()` is the P2 `len_vec` endpoint and `parts[0]`
is `Index` on `Vec<T>`, both P2 rows in this document — and `ls
tests/vertical-slice/accept/string_split_nonempty.*` returns only the `.hew`,
so it is a status-only fixture (run.sh:5856) with no transcript to be parity
against.

**Sub-decision — minting is keyed by symbol, with parameterized variants where
endpoints share one (D-CATALOG-2).** The second revision's rule ("one family per
catalog endpoint … taking the symbol from the linkage row") is not applicable as
written, because the endpoint→symbol map is many-to-one while §6 requires the
symbol side to be single-valued. Three collisions, each verified:

- `const PRINT_RUNTIME: &str = "hew_print_value";` (`hew-hir/src/stdlib_catalog.rs:365`)
  is the `runtime_symbol` of the `print_entry!` macro (`:464-478`), instantiated
  for all sixteen `println_{i32,i64,u8,u32,u64,f64,bool,str}` / `print_*`
  endpoints (`:688-703`). They differ only in the macro's `kind` and `newline`
  fields, which are **call operands**, not linkage: `hew_print_value(kind: u8,
  bits: u64, newline: bool)` (`hew-runtime/src/print.rs:185`).
- `to_string_u16` and `to_string_u32` both carry `hew_uint_to_string`
  (`stdlib_catalog.rs:708-709`).
- `to_string_str` (`:714-720`) and `clone_str` (`:1037-1043`) both carry
  `hew_string_clone` through `StringCloneShim`.

Decision: **one top-level `RuntimeCallFamily` variant per distinct symbol**, and
where several endpoints share a symbol the variant is **parameterized** — the
`MathIntrinsic`/`VecScalar`/`SinkWrite` precedent already in the enum, which
contributes one variant each to the 231. Concretely P1 mints `CatalogPrint{kind,
newline}` → `hew_print_value`, `CatalogToString{width}` over the six
`to_string_*` symbols (the u16/u32 share is inside one variant and so is not a
collision), `StringLength` → `hew_string_length`, and one plain variant per
distinct `assert*`/`panic`/`exit`/`random.*` symbol (each of those is unique).

Two endpoints mint **nothing** because their symbol already has a family, and
reuse is the rule that keeps the map single-valued: `string_concat` →
`RuntimeCallFamily::StringConcat` (`hew-types/src/runtime_call.rs:1635`) and
`len_vec` → `VecLen` (`:1706`) — which is a second, independent reason `len_vec`
is not P1 work, on top of the receiver-type argument in the row above.

The §6 constraint is restated to match (see the brace-group note there): the map
is a bijection between **exported symbols and top-level variants**, not between
symbols and endpoints. That is exactly what `RuntimeCallFamily::from_c_symbol`
(`runtime_call.rs:1424`) needs to be a function, and it is the property
D-BYTESLEN turns on.

The second revision's open flag on this row — "unverified that every one is an
exported C symbol" — **closes positively**. Spot-checked against
`grep -rn 'extern "C\(-unwind\)\?" fn <sym>(' hew-runtime/src`:
`hew_print_value` (`print.rs:185`), `hew_exit` (`lib.rs:182`), `hew_assert`
(`assert.rs:71`), `hew_assert_eq_i64` (`:85`), `hew_assert_eq_str` (`:114`),
`hew_i64_to_string` (`string.rs:503`), `hew_uint_to_string` (`:485`),
`hew_string_clone` (`:1287`), `hew_string_concat` (`:270`), `hew_string_length`
(`:777`), `hew_char_to_string` (`:752`), `hew_random_gauss` (`random.rs:273`)
all exist, and `hew_panic_msg` exists as `extern "C-unwind"`
(`hew-runtime/src/actor.rs:7232`). P1 still runs the sweep over the whole
linkage table before writing the family list; nothing found so far is missing.

**Decision — P1's `string`/`bytes` domain is an enumerated extern set
(D-EXTERN-P1).** P1's declared domain is "all scalars + `string` + `bytes`", and
the second revision budgeted for it with §6's family groups plus a two-symbol
carve-out on the `Call{Extern}` row. The shipped method surface is much larger
than that, and the extra methods have no family at all:

- `grep -n '#\[extern_symbol(' std/string.hew` returns **24** methods in the
  `impl string` block (`:709-821`). Five have a `RuntimeCallFamily` (`find`,
  `char_at`, `char_at_utf8`, `get`, `char_count`). The other **nineteen** —
  `hew_string_{length,is_empty,contains,starts_with,ends_with,to_uppercase,
  to_lowercase,trim,is_digit,is_alpha,is_alphanumeric,clone,replace,split,lines,
  slice,repeat,chars,to_bytes}` — have no `from_c_symbol` arm, so they resolve to
  `CallTarget::Extern { declaration, endpoint, trusted_compiled_stdlib: true }`
  (`hew-types/src/check/dispatch.rs:209-221`).
- All are shipped: `hew run` on a program calling `s.trim()`, `s.to_upper()`,
  `s.contains("ell")`, `s.is_empty()`, `s.replace("l","L")`, `t.repeat(3)`,
  `t.slice(0,1)`, `t.starts_with("a")`, `t.to_bytes()` prints
  `Hello` / `  HELLO  ` / `yes` / `nonempty` / `  HeLLo  ` / `ababab` / `a` /
  `sw` / `2`.
- `bytes::to_string` is the extreme case and sits outside §6's coverage proof
  entirely: it runs (`b.push(65); b.push(66); println(b.to_string())` → `AB`)
  and `hew-types/src/runtime_call.rs:972-978` records
  `CanonicalStdlibExternSignature { signature_key: "bytes::to_string", symbol:
  "hew_bytes_to_string", family: None }` — documented at `:873-876` as "a real
  canonical source method whose dedicated codegen path remains an open-set
  extern call". `expand_families.py` cannot see it, because §6's Bytes groups are
  the twelve `Bytes*` **variants**, and this row has none.

Decision: **these twenty endpoints stay open-set `CallTarget::Extern` calls and
mint no families.** What P1 owes them is their **FFI ownership rows** (plan §1.5,
`ir-ladder.md` §6.4) — every param `Borrow` unless declared `consume`, return
`O` unless `ExternProvenance` says adopt/borrow — plus a P1 slice of the
`make test-ffi-table` gate covering exactly this set. The `Call{Extern}` row's
phase cell now points here instead of naming two symbols.

### 3.4 Aggregates and projections

| construct | source | SIR | MIR realization | runtime | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `TupleLiteral` | `(1, s)` | `tuple.make` consuming `O`/`N` parts → `O` when any part is heap, else `N` | `TupleConstruct` | glue `TupleInPlace` | P1 (scalar; exists) / P2 (heap parts) | `expr.rs:3159-3184` | — |
| `TupleIndex` | `t.0` | `borrow{t}` `project` → `G`; `copy_value` if the element escapes the borrow | `TupleFieldLoad` (codegen `retain_string_field_load` at llvm.rs:21005 deleted) | — | P1 (scalar; exists) / P2 | `expr.rs:4901-4944` | — |
| `StructInit{base: None}` | `Point { x: 1, name: s }` | `agg.make` consuming field values (`move` last-use or `copy_value`) → `O` | `RecordInit` + `RecordFieldStore` | `hew_drop$Point` glue | P2 | `expr.rs:3902-4584` (NYIs 3946, 4556) | — |
| `StructInit{base: Some}` (functional update) | `Point { x: 2, ..p }` | `destructure p` when `p` is an `O` temp/last use, else `borrow{p}` + `project`+`copy_value` per carried field; overridden fields' old values `destroy_value`d; `agg.make` | field loads/stores | glue | P2 | `expr.rs:4004`, `4061`, `4246`, `4307`, `4337`, `4380` NYIs; `facts.rs:97` `compute_funcupdate_base_provenance`; `tests/mir-baselines/funcupdate-reassign` | — |
| `FieldAccess` on record | `p.name` | `borrow{p}` `project` → `G`; escaping read → `copy_value` (plan: "field read-out is a retain") | `RecordFieldLoad` (+ explicit `Retain`, deleting llvm.rs:16698 `retain_string_field_load`) | — | P2 | `expr.rs:4585-4795` (NYIs 4714, 4730, 4756) | — |
| `FieldAccess` on actor state (`self.n`) | `self.count` | `load.copy` from the state place (`G` view inside the handler borrow) | `ActorStateFieldLoad` | — | P4 | `actor.rs:45` `actor_state_field_for_target`; `temp_drop.rs` actor-state load modes | — |
| `FieldAccess` on supervisor child / pool accessor | `sup.worker`, `sup.pool[i]` | `rt.call{SupervisorChildGet/PoolChildGet/NestedGet}` → `N` (`ChildRef` is a non-owning leaf) | `CallRuntimeAbi` | `hew_supervisor_*` | P4 | `actor.rs:612` `lower_supervisor_child_get`, `772` nested, `834` `lower_pool_accessor`, `956` `lower_pool_index`, `1015-1090` child refs | — |
| `Index` on `Vec<T>` | `xs[i]` | `borrow{xs}` bounds check → `trap{IndexOutOfBounds}`; `rt.call{VecGet}` → `G` element (`copy_value` on escape) | bounds CFG + `CallRuntimeAbi(hew_vec_get_*)` | `hew_vec_len`, `hew_vec_get_*` | P2 | `expr.rs:4945-4970`, `vec_index.rs:1-386` (NYIs 34, 161) | — |
| `Index` on `HashMap<K,V>` (`m[k]`) | `m["a"]` | `rt.call{HashMapGetLayout}` → `G`/`copy_value`; missing key → `trap` | compiler call `HashMapGetCloneLayoutIndex` | `hew_hashmap_get_layout` | P2 | `expr.rs:8296` NYI (indexed value clone) | — |
| `Index` on `bytes`/`string` (`b[i]`) | `b[0]` | `rt.call{BytesIndex/StringIndex}` → `N` | `CallRuntimeAbi` | `hew_bytes_index`, `hew_string_index` | P1 | `expr.rs:8692` `lower_bytes_index` and `expr.rs:8617` `lower_string_index` (endpoint shapes documented at `expr.rs:8607-8609`). **Corrected in the third pass**: the first two revisions cited `runtime_builtins.rs` 454/522, which are `lower_bytes_get_option`/`lower_string_get_option` — the `Option`-returning `.get()` lowerings D-OPTION moved to P2, a different construct from `b[i]` | — |
| array literal (HIR: `__hew_array_N` temp + `Vec` pushes) | `[1, 2, 3]`, `[0; n]` | `rt.call{VecNew}` → `O`, then `rt.call{VecPush*}` per element | as Vec | `hew_vec_*` | P2 | `hew-hir/src/lower.rs:22547`, `26592-26638`; `expr.rs:5374`, `5704` | — |
| map literal (HIR desugar to HashMap inserts) | `{"a": 1}` | `rt.call{HashMapNewWithLayout}` + `rt.call{HashMapInsertLayout}` | as HashMap | `hew_hashmap_*` | P2 | `hew-hir/src/lower.rs:20369` `lower_map_literal` | — |
| `Slice` on `Vec<T>` | `xs[a..b]` | bounds `trap`s; `rt.call{VecSliceRange}` → fresh `O Vec` (elements retained by the runtime through glue) | `CallRuntimeAbi(hew_vec_slice_range_*)` | `hew_vec_slice_range` | P2 | `expr.rs:4971-4999`, `8380-8460` (NYIs 8403, 8454) | — |
| `Slice` on `bytes` | `b[1..]` | `rt.call{BytesSlice}` → `O` | `CallRuntimeAbi` | `hew_bytes_slice` | P1 | same arm | — |
| `Slice` on `string` | `s[..]`, `s[1..3]`, `s[1..=2]` | `borrow{s}`; an absent `end` becomes `rt.call{StringCharCount}` + `cast` i32→i64, an inclusive `end` a `binary` +1; then `rt.call{StringSliceCodepoints}` → **`O` fresh string** (a slice is a new allocation, never a view — `View` is `Slice`/`Pointer`/`Borrow` at the FFI boundary only). No SIR `trap{}` edge: bounds are asserted inside the runtime helper and abort the process (`string_slice_oob_panics` expects status 134), unlike the `Vec` row's bounds CFG. **P1 decides** whether to keep the runtime abort or lift it to `trap{IndexOutOfBounds}` for uniformity with `Index`/`Slice` on `Vec`; the transcript fixtures pin today's behaviour either way | `CallRuntimeAbi(hew_string_char_count)` + `NumericCast` + `CallRuntimeAbi(hew_string_slice_codepoints)` | `hew_string_char_count`, `hew_string_slice_codepoints` | P1 | `expr.rs:4971-5004` `HirExprKind::Slice` dispatches on `container.ty`: `String` → `lower_string_slice` (`expr.rs:8644-8690`), `Bytes` → `lower_bytes_slice` (8717), else `lower_vec_slice` (8384) — the first draft had rows for only two of the three arms | — |
| `RegexLiteralRef` | `let r = re"a+";` | `const.i{literal_id}` then `rt.call{RegexHandle}` → **`O`** `regex.Pattern` (`AffineResource`, `clone = None`). **Corrected from `N` in the first draft**: `hew_regex_handle` GEP-loads the shared module-static handle and `hew_regex_clone`s it into the local, so the value owns *the clone*, not the table entry; `destroy_value` is the type's `close` and releases exactly that clone. `expr.rs:6743-6759` says so in its own comment ("`regex.Pattern` is `#[resource]`, so `dest` is a resource-typed local: normal scope-exit drop elaboration emits `close()` on it … releasing the clone (not the shared literal-table entry) exactly once"), and `std/text/regex/regex.hew:28-31` declares `#[resource] pub type Pattern`. This agrees with `ir-ladder.md` §1.3.1 (Owned producers, L225-266 — the regex clause is at L247-251) and §1.1 `StreamPair`/regex `Pattern` row (L116: `AffineResource`, `clone = None`); no spec change is needed | `CallRuntimeAbi(hew_regex_handle)` into a resource-typed local | `hew_regex_handle`, `hew_regex_clone`, `hew_regex_free` | P2 | `expr.rs:6743-6787` (NYI 6777) | wall 2: `clone = None`, so an explicit `clone r` is `E_OWN_CLONE_UNSUPPORTED` |

### 3.5 Control flow

| construct | source | SIR | MIR realization | runtime | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `Block(HirBlock)` | `{ let a = 1; a }` | statements then tail value; scope exit `destroy_value` for every `O` still live | blocks | — | P1 (exists) | `expr.rs:3834-3896`; scope ledger `scope.rs:152` `record_binding_scope` | — |
| `If{else: Some}` | `if c { a } else { b }` | `branch` + `bbarg` result; an `O` consumed in one arm is `destroy_value`d in the other before the join | blocks + phi | — | P1 (exists for scalars) | `expr.rs:3897-3901` | — |
| `If{else: None}` | `if c { f() }` | `branch`, unit result `N` | blocks | — | P1 (refused today `hew-sir/src/lower.rs:1269`) | same | — |
| `While` | `while c { … }` | loop header with `bbarg`s for every SSA `var` and every `O` carried across iterations | blocks | — | P1 | `expr.rs:6543-6547` → `control_flow.rs:342-445` | — |
| `Loop` | `loop { … break; }` | header `bbarg`s; `break value` feeds the exit `bbarg` | blocks | — | P1 | `expr.rs:6604` → `control_flow.rs:2207-2279` | — |
| `ForRange` (numeric, incl. `step`, `descending`, `inclusive`) | `for i in 0..n { }` | trip count captured once; header `bbarg` induction var (`N`) | blocks | — | P1 | `expr.rs:6548-6566` → `control_flow.rs:1778-2206`; `control_flow.rs:18` `normalize_range_integer_operand` | — |
| `for x in vec` (HIR desugar `lower_for_iter_desugar` → `WhileLet` over `VecIter.next`) | `for s in names { }` | `rt.call{VecIter new}` cursor (`O`, borrows the Vec: `borrow{names}` spans the loop); `switch_enum` on `next()`; element `G` (or `O` when the runtime moves it out) | cursor ops + loop | `hew_vec_iter_*`; cursor release `DropKind::VecIterCursor` | P2 | `hew-hir/src/lower.rs:26504`; `control_flow.rs:446-1394` `lower_while_let` (+ `853` `classify_while_let_binding_iteration_owner`); `scope.rs:348` `register_vec_iter_scope_owner`, `489`, `551`, `582-746` | reassigning/moving the Vec while the cursor borrows it is a rule-3 borrow-scope error (today NYIs `assign.rs:29/66`, `move_value.rs:784`) |
| `for await x in stream/receiver` (HIR desugar to `WhileLet` over `StreamRecvAwait`/`ChannelRecvAwait`) | `for await m in rx { }` | loop with `suspend{StreamNext/ChannelRecv}` per iteration; element `O` | coroutine loop | `hew_stream_next_layout`, `hew_channel_recv_layout` | P4 | `hew-hir/src/lower.rs` "for await over Stream/Receiver" unsupported shapes; `task.rs:2167`, `2269` | — |
| `WhileLet` | `while let Some(x) = it.next() { }` | header `bbarg`s; per-iteration `switch_enum` on the `O` scrutinee temp; `destructure` payload | blocks | — | P2 | `control_flow.rs:446-1394` (NYIs 496, 530, 902, 915, 928, 940) | — |
| `IfLet` | `if let Some(v) = opt { } else { }` | `switch_enum` (`G` scrutinee); `project`+`copy_value` or `destructure` for an `O` temp | blocks | — | P2 | `expr.rs:6586-6603` → `control_flow.rs:1395-1777` (NYI 1419) | — |
| `Match` | `match e { A(x) => …, _ => … }` | see §4 pattern rows; `bbarg` join of arm values | blocks | — | P2 | `expr.rs:6567-6569` → `pattern.rs:174` `lower_match` (dispatch: `4649` enum tag, `3166` project, `3495` literal, `3784` regex, `2643` binding chain, `4561` guard) | — |
| `Break{label,value}` | `break 'outer 3;` | `goto` exit with `bbarg`; every `O` live in the exited scopes `destroy_value`d; defers run | blocks | — | P1 (P4 defers) | `expr.rs:6605-6643`; `scope.rs:1043` `emit_defers_for_break_continue`, `1072` generator drops, `1198` `resolve_loop_frame` | — |
| `Continue{label}` | `continue;` | `goto` header with current `bbarg`s; scope-local `O` destroyed | blocks | — | P1 | `expr.rs:6700-6742` | — |
| `Return{value}` (expression form) | `return x` | as stmt row | — | — | P1 | `expr.rs:6644-6699` | — |
| `Scope{body}` (structured concurrency) | `scope { fork a = f(); }` | `rt.call{TaskScopeNew}` → `O` scope handle; body; implicit `rt.call{TaskScopeJoinAll}` + `TaskScopeDestroy` on every exit incl. cancel | coroutine-aware blocks | `hew_task_scope_*` | P4 | `expr.rs:4796` → `task.rs:109-146` `lower_task_scope` | — |

### 3.6 Closures and generators

| construct | source | SIR | MIR realization | runtime | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `Closure{escape_kind: Local}` | `let f = \|x\| x + n;` | `closure.make` with captures by `CaptureModeOrigin::{ImplicitCopy,InferredBorrow,InferredBorrowMut,ExplicitMove}` → `O` closure pair; each env field is an `alloc_place` (`store.init` at construction, `load.copy`/`store.assign` from inside the body, `end_lifetime` on every field when the env is released — this is `hew_drop$Env` reached from the arc `drop_fn`); a `BorrowMut` capture — the one surviving use of that word, a `ClosureCaptureMode` variant and not an operand mode (D-NOMODE) — makes the **env field** a place, not the outer `var` (`ir-ladder.md` §1.3 `alloc_place` row, L203; §1.1 `Closure` row, L120) — and since no outer `var` can ever be a place (D-NOPLACE), env fields are the *first* places any Hew program produces | `MakeClosure` + `ClosureEnvInit`; `ClosureEnvFieldLoad/Store` become place ops | env drop thunk (`thunks.rs:289`) replaced by glue | P3 | `expr.rs:4894-4900` → `closure_gen.rs:171` `lower_closure_literal`, `420` `closure_env_capture_ownership_by_strategy`, `489` `materialize_closure_env`, `828` `lower_closure_shim`; `750` `child_builder_tables` | — |
| `Closure{escape_kind: Escapes}` | returned/stored closure | as above but env is heap (`HeapBoxed`); captures must be `move`/`copy_value` (no borrow may escape) | boxed env | glue | P3 | same; NYIs `closure_gen.rs:721` (suspension inside closure), `865` | rule 3: an escaping borrow capture is a verifier error |
| `Closure{escape_kind: Forked}` | closure spawned by `fork` | env owned by the task scope; captures `move`d; non-Send capture rejected | task env | `hew_task_*` | P4 | `task.rs:1094` `lower_spawned_closure_task`, `45` `spawn_env_ownership_from_closure_manifest` (NYIs 1122, 1134) | wall 3: non-`Send` capture |
| `SpawnLambdaActor` (`actor \|m\| { }`) | `let a = actor \|n: i64\| { n + 1 };` | `closure.make` for the body env (captures Strong = `copy_value`, Weak = weak handle, `end_lifetime` on the env at release) + `rt.call{LambdaActorNew}` → **`O LambdaPid<Msg, Reply>`** (`AffineResource`, `clone = Retain` via `hew_lambda_actor_clone`; a `send` of it is `Transfer` only). **Corrected from "`O Duplex`" in the first draft** — the two types carry different ownership facts (`Duplex` is `AffineResource` with `clone = None`, `ir-ladder.md` §1.1 `#[resource]` row, L115; `LambdaPid` is `AffineResource` with `clone = Retain`, `ir-ladder.md` §1.1 `LambdaPid` row, L118), so a P4 lane briefed off the old text would have given the handle no copy path. The checker returns `LambdaPid<Msg, ()>` for a send-shaped lambda and `LambdaPid<Msg, Reply>` for an ask-shaped one (`hew-types/src/check/expressions.rs:2713-2717`) | `MakeLambdaActor` | `hew_lambda_actor_*`; release `DropKind::LambdaActorRelease` | P4 | `expr.rs:4821-4834` → `closure_gen.rs:1260` `lower_spawn_lambda_actor`, `1829` `lower_lambda_actor_call` (NYIs 1876, 1940) | — |
| `GenBlock` | `gen { yield 1; }` | coroutine callable; `captures` (`HirGenCapture`, `Local`/`ActorStateField` source) `copy_value`d into `alloc_place` env slots and `end_lifetime`d by the generator env thunk when the handle is destroyed; `yield` → `suspend{Yield}` | `MakeGenerator`; frame from MIR frame description | `hew_gen_*` | P3 | `expr.rs:6788-6847` → `closure_gen.rs:2048-2959` (NYIs 2217, 2291; capture admission `59`, `85`, `111-156`) | — |
| `Yield` | `yield v;` | `move v` into the suspend payload; `suspend{Yield}` with resume/cancel edges; cancel edge destroys frame-live `O`s | `Terminator::Yield` | — | P3 | `expr.rs:6848-6850` → `closure_gen.rs:2960` `lower_yield_expr` (NYI 2970 outside gen body) | — |
| `receive gen fn` stream producer pump | `receive gen fn ticks(self) -> Stream<i64>` | producer callable: loop of `suspend{Yield}` → `rt.call{SinkWrite}`; peer-closed check exits | pump body | `hew_sink_*`, `hew_stream_*` | P4 | `closure_gen.rs:2832` `build_stream_producer_pump`, `2713`, `2763` `mint_pump_yield_value_owner`, `2806` | — |

### 3.7 Machines (HIR desugar target; no SIR ops of their own)

| construct | source | SIR (after D287 desugar) | MIR realization | runtime | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `MachineStep` | `m.step(ev)` | direct `call` of the desugared `step` fn: the state enum is a `var self` receiver, so it follows the `VarSelfMethodCall` row — `fork` then `borrow{}` for a `CowValue` state enum, `move`-in/`move`-back for an `AffineResource`/`Linear` one (D-NOMODE: there is no `BorrowMut` mode); `ev: O` event consumed (`destructure` per `ir-ladder.md` §1.3 `destructure` row, L202) | `Terminator::Call` | — | P4 | `expr.rs:6341-6465`; `machine_synth.rs:1360-1385`, `1811` | — |
| `MachineEmit` | `emit Done { n }` inside a transition | `agg.make{Event}` `O` pushed to the machine's emit buffer (`rt.call{VecPushOwned}`) | `MachineEmitPlaceholder` deleted | — | P4 | `expr.rs:6113-6158` | — |
| `MachineTakeEmits` | `m.take_emits()` | `load.take` of the emit Vec (`O`), `store.init` fresh empty | `MachineEmitTake` deleted | — | P4 | `expr.rs:6505-6542` | — |
| `MachineStateName` | `m.state_name()` | `switch_enum` over state tags → `const.str` | `MachineStateName` deleted | — | P4 | `expr.rs:6466-6504` | — |
| `MachineVariantCtor` | `Light.Red`, `Light.Amber { t: 3 }` | `agg.make{state variant}` | enum init | — | P4 | `expr.rs:6159-6206` | functional update on state ctors stays a HIR error (`hew-hir/src/lower.rs:18883`) |
| `MachineFieldAccess` / `MachineEventFieldAccess` | `self.t` in a transition, `ev.n` | `project` on the state/event enum payload (`G`) | `EnumTagLoad` + payload load | — | P4 | `expr.rs:6207-6280`, `6281-6340`; `machine_synth.rs:1926` passthrough, `2002` transition-out drops | `self.<field>` in a wildcard transition stays a HIR error (`lower.rs:30114`) |
| machine `reenter`/`default`/guards, `hew machine diagram` | (decl surface) | desugar-time facts; guards become `branch`es in the `step` match | — | — | P4 | `machine_synth.rs:2055` `lower_machine_lifecycle_block` | — |

### 3.8 Actors, supervisors, links

| construct | source | SIR | MIR realization | runtime | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `Spawn{actor_name,args}` | `spawn Counter(n: 0)` | `agg.make` state record from init args (`O` args consumed, snapshot mode per rule 5); `spawn` → **`N`** `LocalPid` (non-owning leaf). **Decision, and the ladder spec must follow**: `ir-ladder.md` §1.1 `LocalPid`/`HewActor` row (L105) classes them `BitCopy` with `clone = Bits` ("a pid never owns the actor"), which is code truth — `ty_is_nonowning_pid_leaf` (`hew-codegen-rs/src/llvm.rs:25479-25488`) is true exactly for a builtin whose `handle_family()` is `ActorPid` and whose `close_method()` is `None`, and nothing releases a pid. `ir-ladder.md` §1.3.1 still lists the `Spawn`/`SpawnLambdaActor` handle among the producers of `Owned` values (revision 6, L245); that sentence must drop `Spawn` (the `SpawnLambdaActor` half is correct — it produces a `LambdaPid`, §3.6). Until it does, P0's "no undecided ownership mode" gate is not met | `SpawnActor` | `hew_actor_spawn`; state owned by the runtime object (drop glue on actor exit) | P4 | `expr.rs:4835-4837` → `actor.rs:2518-2872` (NYIs 2540, 2577, 2618, 2672, 2700, 2859) | — |
| `ActorSend{checked,blocking}` | `pid.inc(1)`, `pid.inc(1)?` | `send{Snapshot}` per arg: `Share` (immutable-shareable, `copy_value`), `DeepCopy` (mutable collection), `Transfer` (proven last use, `move`); result `N` or `O Result<(), SendError>` | `Terminator::Send`; `blocking` → `suspend{ActorSend}` | `hew_actor_send_by_id`; `SendAliasMode::{SnapshotBitCopy,SnapshotRetain,SnapshotMaterialize,TransferLastUse}` | P4 | `expr.rs:4847-4853` → `actor.rs:1839` `lower_actor_send`, `1916`/`1940` finish, `1692-1763` payload packing, `1814` message owner mint; post-CFG mode `mod.rs:5130-5336` (NYIs 5189, 5224, 5243, 5285, 5299, 5318) | wall 3: non-`Send` payload; resource-shaped send |
| `ActorAsk{deadline_ns}` | `await pid.get()`, `await pid.get() \| after 1s` | as send for args; `suspend{Ask}` with `result_dest`/`reply_dest`/`error_dest`; reply `O`; deadline adds a timer race on the same suspend | `Terminator::Ask` / `SuspendKind::Ask` | `hew_actor_ask*`, reply channel `ReplyChannel*` | P4 | `expr.rs:4854-4861` → `actor.rs:2164-2318` (NYIs 2177, 2191, 2288) | ask from a blocking caller (`main`/free fn) stays rejected (2288) |
| `ActorGenStream` | `let s = pid.ticks();` | `rt.call{ChannelSendLayout pair}` → `O Stream`, sink half `send{Transfer}` in a start message | channel + tell | `hew_channel_*`, `ActorGenSinkRegister/Complete` | P4 | `expr.rs:4862-4866` → `actor.rs:1975` `lower_actor_gen_stream`, `2081` `build_receive_gen_channel` (NYIs 1990, 2002) | — |
| `RemoteActorAsk` | `await rpid.ask(msg, 500)` | `send{DeepCopy}`-shaped CBOR encode of `O` msg; `suspend{RemoteAsk}` → `O Result<Reply, AskError>` | `Terminator::RemoteAsk` | `hew_remote_pid_send`, wire codec | P4 | `expr.rs:4888-4893` → `actor.rs:2319-2517` (NYIs 2329, 2380) | — |
| `AwaitRestart{child}` | `await_restart sup.worker` | `suspend{RestartWait}` → `N` `ChildRef` | `SuspendKind::RestartWait` | `hew_supervisor_restart_await_blocking` | P4 | `expr.rs:4816-4818` → `actor.rs:627-708` (NYIs 635, 647) | — |
| `link`/`monitor`/`unlink`/`demonitor` builtins | `link(pid)`, `let m = monitor(pid)` | `rt.call{ActorLink/ActorMonitor/ActorUnlink/ActorDemonitor}`; `monitor` → `O MonitorRef` (`#[resource]`, `RuntimeDropDescriptor::MonitorRefClose`) | `CallRuntimeAbi` | `hew_actor_link/monitor/unlink/demonitor` | P4 | `actor.rs:88` `lower_actor_link_or_monitor`, `358` `lower_actor_unlink`, `412` void call (NYIs 105, 157, 366, 380) | — |
| `monitor(RemotePid)`, `link_remote(rpid, policy)` | `link_remote(r, PartitionPolicy.Kill)` | `rt.call{NodeMonitor/LinkRemote}` → `O Result` | `CallRuntimeAbi` | `hew_node_monitor_location`, `hew_node_link_remote_location` | P4 | `actor.rs:199`, `262` (NYIs 223, 271, 290, 324) | — |
| supervisor `stop` | `sup.stop()` | `rt.call{SupervisorStop}` | `CallRuntimeAbi` | `hew_supervisor_stop` | P4 | `actor.rs:576` (NYI 583) | — |
| supervisor token capture inside handlers | (HIR `ContextReader::Supervisor`) | `N` context read | — | — | P4 | `actor.rs:1090` `capture_supervisor_token` | — |
| receive handler body (`receive fn`) | `receive fn add(self, n: i64) { self.n += n }` | callable with `self: G` state place; message params `O` **taken** by the handler (`Consume` header slots; the `ActorHandler` shim `destructure`s the payload record and frees the buffer with `hew_msg_payload_free`, `ir-ladder.md` §5.6 L1814-1821), destroyed at handler exit unless moved; reply value `move`d into the reply channel | `ActorHandler` conv | `hew_actor_*`; envelope drop thunks replaced by glue | P4 | `machine_synth.rs:169-394` (`mangle_actor_receive_handler` 1216); codegen borrow taint (llvm.rs:22270) deleted | — |
| actor `init` / lifecycle hook bodies | `init(n: i64) { self.n = n }`, `on_stop { }` | as handler; `init` params `O` consumed into `store.init` of state fields | same | same | P4 | `machine_synth.rs:395-489`, `490-1215` | — |
| duplex / half handles (`hew_duplex_*`, `hew_send_half_*`, `hew_recv_half_*`) | `let (a, b) = duplex_pair(); a.send(v)` | `rt.call{DuplexPair}` → two `O` handles; `send` consumes `O` payload; `recv` → `O Result<R, RecvError>`; `close` consumes the handle | `CallRuntimeAbi` | `hew_duplex_*`; `DropKind::DuplexClose`, `DuplexHalfClose(Direction)` | P4 | `actor.rs:476` pair, `1129` send, `1274` close, `1369` half extract, `1433` half send, `1514` recv, `1590` half close (NYIs 490, 1138, 1199, 1283, 1333, 1378, 1443, 1465, 1524, 1538, 1548, 1599, 1615, 1632); `split_consume.rs` | — |

### 3.9 Tasks, async, select

| construct | source | SIR | MIR realization | runtime | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `SpawnedCall{bound}` | `fork t = work(x)` / `work(x)` in a scope | args `move`/`copy_value` into a task env (`O`); `rt.call{TaskScopeSpawn}` → `O Task<T>` (`Linear`: must be awaited or joined by scope end) | `SpawnTaskDirect` + entry adapter | `hew_task_*` | P4 | `expr.rs:4797-4803` → `task.rs:479` `lower_spawned_call_task`, `587`, `703`, `309-478` entry adapters (NYIs 491, 503, 518, 599-652, 715-830) | unbound spawn of a non-unit callee stays rejected |
| `ForkBlock` | `fork { … }` | captures `move`d (`Send` required); `closure.make` + `rt.call{TaskScopeSpawn}` | `SpawnTaskClosure` + fork shim | `hew_task_*` | P4 | `expr.rs:4804-4806` → `task.rs:1172` (NYIs 1208, 1218, 1231, 1249); `939` `synthesize_fork_entry_shim` | wall 3: non-`Send`/borrowed capture |
| `ScopeDeadline` | `after(2s) { … }` | `rt.call{TaskScopeCancelAfterNs}`; body on the cancel edge of the scope's `suspend` | `SuspendingScopeDeadline` | timers | P4 | `expr.rs:4807-4809` → `task.rs:1293` (NYIs 1301, 1345) | — |
| `AwaitTask` | `let r = await t;` | `move t` (consumes the `Linear` task) + `suspend{TaskAwait}` → `O`/`N` result | `SuspendKind::TaskAwait` | `hew_task_await_blocking`, `TaskGetResult/Error/Free` | P4 | `expr.rs:4810-4815` → `task.rs:1394` (NYI 1477) | — |
| `Select(HirSelect)` arms `StreamNext/ActorAsk/TaskAwait/ChannelRecv/AfterTimer` | `select { m = await rx.recv() => …, after 1s => … }` | one `suspend{Select}` with per-arm resume edges; the winning arm's value is `O` bound in its scope; losing in-flight asks cancelled on the cancel edge | `Terminator::Select`/`SuspendingSelect` | `hew_select_first`, `ReplyChannelCancel` | P4 | `expr.rs:4819` → `task.rs:1552-1946` (NYIs 1590, 1664, 1691, 1722, 1856); `16-44` item-type helpers | — |
| `Join(HirJoin)` | `join { a.get(), b.get() }` | N asks issued (`send` modes per arg) then one `suspend{Join}` → `O` tuple of replies | `Terminator::Join` | reply channels | P4 | `expr.rs:4820` → `task.rs:1947-2054` (NYI 1965) | — |
| `ConnAwaitRead{to_string,deadline_ns}` | `await conn.read()` | `suspend{Read}` → `O bytes` (or `O string`; `Result<_, NetError>` with deadline) | `SuspendKind::Read` | `hew_tcp_read`, `TcpAttachLocal` | P4 | `expr.rs:4867-4872` → `task.rs:2055` (NYI 2114) | non-suspendable caller keeps the blocking call |
| `ListenerAwaitAccept` | `await ln.accept()` | `suspend{Accept}` → `O Connection` (`#[resource]`) | `SuspendKind::Accept` | `hew_tcp_accept` | P4 | `expr.rs:4873-4877` → `task.rs:2372` (NYI 2436) | — |
| `ChannelRecvAwait` | `await rx.recv() \| after 1s` | `suspend{ChannelRecv}` → `O Result<Option<T>, TimeoutError>` | `SuspendKind::ChannelRecv` | `hew_channel_recv_layout` | P4 | `expr.rs:4878-4882` → `task.rs:2167` (NYI 2230) | — |
| `StreamRecvAwait` | `await s.recv() \| after 1s` | `suspend{StreamNext}` → `O Result<Option<T>, TimeoutError>` | `SuspendKind::StreamNext` | `hew_stream_next_layout` | P4 | `expr.rs:4883-4887` → `task.rs:2269` (NYI 2332) | — |
| `await closure(args)` (suspendable callee) | `await handler(conn)` | `suspend{CallClosure}` | `SuspendKind::CallClosure` | — | P4 | `expr.rs:3735` NYI; codegen `suspend.rs` | — |
| `sleep(d)` / `sleep_until(t)` builtins | `await sleep(1s)` | `suspend{Sleep}` / `suspend{SleepUntil}` | `SuspendKind::Sleep/SleepUntil` | timers | P4 | `task.rs` (not a distinct fn; via runtime family) | — |
| `\| after d` on non-await / non-literal duration | `x \| after 1s` | rejected in HIR | — | — | P4 (reject) | `hew-hir/src/lower.rs:21833-21936` unsupported | HIR error |

## 4. Patterns (`HirMatchArmPredicate` node.rs:2964, payload predicates, bindings)

| construct | source | SIR | MIR realization | runtime | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `Wildcard` | `_ => …` | default edge of `switch_enum`/`switch_int` | — | — | P2 | `pattern.rs:174` (NYI 2775, 2843) | — |
| `Binding{binding_id}` (catch-all bind) | `x => …` | `copy_value` of a `G` scrutinee (or `move` of an `O` temp) into the arm binding | — | — | P2 | `pattern.rs:2643` `lower_match_binding_chain` (NYI 2661); `6090` `emit_match_arm_binding`, `4634` scope | — |
| `EnumVariant{variant_idx}` with payload bindings (`HirMatchArmBinding`) | `Some(v) => …` | `switch_enum`; payload `project`+`copy_value` (scrutinee outlives) or `destructure` (scrutinee is an `O` temp: whole consumed, unused fields destroyed — F19 class cannot be written) | tag load + payload loads | — | P2 | `pattern.rs:4649-6005` `lower_match_enum_tag`, `4310` `classify_producer_scrutinee_origin`, `4532`, `6006` `emit_payload_variant_predicate_checks` (NYIs 188, 4801, 6019) | — |
| `EnumVariant` on indirect (boxed) enum | `match tree { Node(l, r) => … }` | as above with the box as the `O` root; `destructure` frees the box after parts move | `IndirectEnum` glue | — | P2 | `pattern.rs` via `ty_is_indirect_enum` | — |
| `HirPayloadVariantPredicate` (nested ctor check) | `Ok(Some(x)) => …` | nested `switch_enum` inside the outer arm | — | — | P2 | `pattern.rs:6006`, `3259` `lower_match_project_predicate_chain` | — |
| `Literal{lit}` (int/char/bool/duration) | `1 => …`, `'a' => …` | `switch_int` | jump table / compare chain | — | P2 | `pattern.rs:3495` `lower_match_literal`, `3640` constant (NYIs 3533, 3588, 3715) | — |
| `Literal{String}` | `"go" => …` | `str.eq` chain inside `borrow{scrutinee}` | compare chain | `hew_string_equals` (through the `StringEquals` family D-STRCMP mints at P1; the first two revisions wrote `hew_string_eq`, which does not exist) | P2 | same (NYI 3696 embedded NUL) | — |
| `RecordProject{ty}` / `TupleProject{arity}` (destructure) | `Point { x, y } => …`, `(a, b) => …` | `borrow`+`project`+`copy_value` per binder, or `destructure` of an `O` temp | field loads | — | P2 | `pattern.rs:3166` `lower_match_project`, `2739` preflight, `2869` emit, `3120` local, `2573` `match_project_scrutinee_reject` (NYIs 2757, 3155, 3187, 3206, 3267, 3286, 3296, 3319) | guarded destructure of an owned aggregate stays rejected until `destructure` lands (P2) |
| `Regex{literal_id,captures}` | `re"(?P<n>\d+)" => …` | `rt.call{RegexMatch}` → `N`; captures `rt.call{RegexCapture}` → `O string` each, `RegexFreeCapture` on the arm's exits | runtime calls | `hew_regex_match/capture/free_capture` | P2 | `pattern.rs:3784-4255` (NYIs 3819, 3845, 3914, 4031, 4110, 4210) | — |
| arm guard (`HirMatchArm.guard`) | `Some(x) if x > 0 => …` | `branch` after binders are bound; guard failure falls through with binders destroyed | blocks | — | P2 | `pattern.rs:4561` `lower_match_arm_guard` (NYIs 2425, 2505 enum overwrite inside guard) | — |
| `let`/`let-else`/`if-let`/`while-let` payload binders | (shared shape) | same as `EnumVariant` bindings | — | — | P2 | rows §2/§3.5 | — |
| nested/or/`NominalPath`/`ContextVariant`/`RecordShorthand` surface patterns (parser `Pattern` variants) | `A \| B => …` | HIR desugars or rejects (`hew-hir/src/lower.rs:28858` "pattern" slice-2, `15865` nested let pattern) | — | — | P2 (HIR desugar lane) | HIR | unsupported nesting stays a HIR error until P2 desugar |

## 5. Suspend kinds (`SuspendKind`, hew-mir/src/model.rs:2772)

| construct | source | SIR | MIR realization | runtime | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `ActorSend{stable_role,arg_modes,cleanup_plan}` | blocking bounded send | `suspend{ActorSend}`; args already `send{mode}`-decided; cancel edge destroys the prepared `O` message | `Terminator::Suspend` | `hew_actor_send_by_id` (block) | P4 | `actor.rs:1916` `finish_blocking_actor_send`; `state_clone.rs:309` `ValueSnapshotPlan` deleted (snapshot is a SIR decision) | — |
| `Ask{result_dest,reply_dest,error_dest}` | `await pid.m()` | `suspend{Ask}` | same | `ActorAsk/AskWithChannel`, reply channel | P4 | `actor.rs:2164` | — |
| `Read` | `await conn.read()` | `suspend{Read}` | same | tcp | P4 | `task.rs:2055` | — |
| `Accept` | `await ln.accept()` | `suspend{Accept}` | same | tcp | P4 | `task.rs:2372` | — |
| `CallClosure` | `await f(x)` | `suspend{CallClosure}` | same | — | P4 | `expr.rs:3735` | — |
| `StreamNext` | `await s.recv()` | `suspend{StreamNext}` | same | `StreamNextLayout` | P4 | `task.rs:2269` | — |
| `StreamSend{sink,value}` | `await sink.send(v)` | `move v` then `suspend{StreamSend}` | same | `StreamSendLayout`/`SinkWrite` | P4 | `suspend_places.rs` (place enumeration); producer in closure_gen pump | — |
| `ChannelRecv` | `await rx.recv()` | `suspend{ChannelRecv}` | same | `ChannelRecvLayout` | P4 | `task.rs:2167` | — |
| `RemoteAsk` | `await rpid.ask(..)` | `suspend{RemoteAsk}` | same | `RemotePidSend` | P4 | `actor.rs:2399` | — |
| `TaskAwait` | `await t` | `suspend{TaskAwait}` | same | `TaskAwaitBlocking` | P4 | `task.rs:1394` | — |
| `RestartWait` | `await_restart` | `suspend{RestartWait}` | same | `SupervisorRestartAwaitBlocking` | P4 | `actor.rs:627` | — |
| `Sleep{duration_ns}` | `await sleep(1s)` | `suspend{Sleep}` | same | timer | P4 | via runtime family | — |
| `SleepUntil{instant_ns}` | `await sleep_until(t)` | `suspend{SleepUntil}` | same | timer | P4 | via runtime family | — |

Every suspend row shares one invariant: values live across the suspension are
frame-materialized by MIR (`materialization reason CoroutineFrame`), and the
cancel edge is a path on which every `O` must be `destroy_value`d (plan
§1.1). Today's `suspend_places.rs:1-1975` source/escape enumeration and
`drop_plan.rs:882` `entry_cancel_parameter_owners` are the legacy owners of
that fact and are deleted in P5.

## 6. Runtime call families (`RuntimeCallFamily`, hew-types/src/runtime_call.rs:366 — 231 variants) and compiler calls

Legacy routing today has three shapes, all replaced by one `rt.call{family}`
whose operand modes come from the FFI ownership row: (a) symbol-string arms in
`runtime_builtins.rs:11-172` `lower_runtime_call`; (b) the builtin-method route
through `ResolvedImplCall`/`Call{Builtin}` with `RuntimeResultAuthority`
(`expr.rs:3484`, `5174-5894`); (c) no producer arm → NYI
(`runtime_builtins.rs:154`, `expr.rs:3464`, `3509`).

**Reading the brace groups.** `Prefix{A,B}Suffix` expands to the literal variant
names `PrefixASuffix`, `PrefixBSuffix`, and the parenthesised number is the
group's variant count. Three conventions the notation carries:

- A member ending `*` (the `Vec` row's `Contains*`, `Pop*`, `Push*`,
  `RemoveAt*`, `Set*`) stands for every variant with that prefix. The row also
  names `PushOwnedMove` and `SetOwnedMove` explicitly, which the wildcards
  already cover; that is the notation, not a double-listing.
- `MathIntrinsic`, `VecScalar`, `SinkWrite` and `SinkTryWrite` are single
  **parameterized** variants. Their inner enums are what the row's brace list
  and count describe (`MathIntrinsic{Sqrt,…}` = 15 inner cases, one top-level
  variant), so they contribute one variant each to the 231.
- Two variants do **not** take their group's suffix and are written out:
  `HashMapNew`/`HashMapNewWithLayout` and `HashSetNew`/`HashSetNewWithLayout`
  (`runtime_call.rs:487-495`, `508-510` — the constructor surface form is a
  second callee identity, catalogued because the map between **exported symbols
  and top-level variants** must be a bijection: `from_c_symbol`
  (`runtime_call.rs:1424`) has to be a function, and D-BYTESLEN turns on it.
  **The bijection is symbol↔variant, never symbol↔endpoint** — several catalog
  endpoints share one symbol, which is why D-CATALOG-2 mints parameterized
  variants rather than one per endpoint). The first revision folded them into the `…Layout` brace group and
  so named `HashMapNewLayout` and `HashSetNewWithLayoutLayout`, neither of which
  exists.

Checked mechanically: `python3 scratchpad/expand_families.py` transcribes every
group in this table, expands it against the live variant list from
`count_variants.py`, and set-diffs. It prints
`231/231 covered; 0 double-listed, 0 missed, 0 named but nonexistent` and exits
0. Re-run it after any change to a group here or to the enum. The table's last
row (`HashMapIter`/`VecIter` cursor families) is deliberately outside that
check: those are `BuiltinType`s, not `RuntimeCallFamily` variants — there is no
`VecIterNext` family, which is why the row carries no count.

**What the 231/231 proof does not cover, and why that matters to P1's budget.**
The check is total over `RuntimeCallFamily`, so it is silent about every shipped
endpoint that has no family. Two groups sit outside it, both inside P1's
declared `string`/`bytes` domain: the catalog builtins (0 families on main —
D-CATALOG mints them, and the row for them below is prose for exactly this
reason) and the twenty open-set `std` externs of D-EXTERN-P1 (nineteen
`#[extern_symbol]` methods in `std/string.hew`'s impl block plus
`bytes::to_string`, whose `CanonicalStdlibExternSignature` records
`family: None`, `hew-types/src/runtime_call.rs:972-978`). A lane sizing P1 off
"231/231 covered" alone would miss both.

| family group (variants) | source | SIR operand/result kinds | MIR | runtime symbols | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `Bytes{Append,Clear,Contains,Index,IsEmpty,Pop,Push,Set,Slice,New}` (10) | `b.push(1)`, `b.pop()` | receiver `O bytes` (`CowValue`) mutated in place: `fork %b → %b'` then `borrow{%b'}` around the `rt.call` (header slot `Borrow` — D-NOMODE, there is no `BorrowMut` mode); `Slice`/`New` → `O`; the rest → `N`. `Pop` stays P1 because its result is not an `Option`: `std/io.hew:59-61` declares `#[extern_symbol(hew_bytes_pop)] fn pop(buf: bytes) -> u8`, and the C symbol `hew_bytes_pop(triple: &mut BytesTriple) -> i64` (`hew-runtime/src/bytes.rs:427-431`) aborts on empty rather than returning a sentinel. **The `i64` is the symbol's return, not the surface type** — the first two revisions said `bytes.pop()` returns `i64`, which the Hew declaration contradicts; the D-OPTION conclusion is unchanged | `CallRuntimeAbi` | `hew_bytes_*` | P1 | `runtime_builtins.rs:183-520` (push 183, pop 216, set 258, is_empty 287, contains 315, clear 349, append 381; NYIs 191-479) | — |
| `Bytes{Len}` (1) | `b.len()` | `borrow{b}` `rt.call{BytesLen}` → `N`. **No producer mints this family on main — see D-BYTESLEN below the table.** `b.len()` mints `VecLen`; only a codegen receiver-type intercept reaches the bytes entry | `CallRuntimeAbi` | `hew_bytes_len` | P1 (after D-BYTESLEN) | `runtime_builtins.rs:67` (`"hew_vec_len" =>`) → `414` `lower_bytes_len`, which emits `hew_vec_len`; codegen intercept `hew-codegen-rs/src/runtime_abi.rs:2205-2226` | — |
| `Bytes{Get}` (1) | `b.get(0)` | `borrow{b}` (the receiver is borrowed, not consumed) then `rt.call`; result is `Option<u8>` built from the runtime's sentinel: `agg.make` at the call site, `switch_enum` + payload binder at every use. Ownership kind is `N` (BitCopy payload), but the **ops** are P2's | `CallRuntimeAbi` | `hew_bytes_get` | **P2** (needs `Option`, same rule as `TryWidthCast` and `checked_*`) | `runtime_builtins.rs:454` `lower_bytes_get_option` | — |
| `String{CharCount,Concat,Index,SliceCodepoints}`, `StructuralFormat` (5) | `s + t`, `s[0]`, `s[1..3]` | `G` receiver (a `string` is never `fork`ed — see the §7 row); `Concat`/`SliceCodepoints`/`StructuralFormat` → `O string`; `CharCount`/`Index` → `N` | `CallRuntimeAbi` | `hew_string_*` | P1 (interpolation, which chains `StringConcat`, is P2). **P1 also mints three families this group does not yet contain — `StringEquals`, `StringCompare`, `BytesEquals` (D-STRCMP, §3.1)**; they are written in prose, not in the brace group, because they do not exist on main and `expand_families.py` set-diffs this table against the live enum | `runtime_builtins.rs:522-740` (char_count 652; NYIs 531-717); `expr.rs:3438`; comparison has no MIR owner at all — `lower_binary` emits `Instr::IntCmp` and codegen recovers the type (`llvm.rs:14216-14320`) | — |
| `String{CharAt,CharAtUtf8,Find,Get}` (4) — the D46 sentinel inspectors | `s.find("x")`, `s.char_at(0)` | `G` receiver; result is `Option<i64>`/`Option<char>` built from the runtime's `-1` sentinel: `agg.make` at the call site, `switch_enum` + payload binder at every use | `CallRuntimeAbi` with an Option dest the intercept fills | `hew_string_find`, `hew_string_char_at`, `hew_string_char_at_utf8` | **P2** (needs `Option`) | `runtime_builtins.rs:590` `lower_string_sentinel_option`, `522` get | — |
| `Vec{New,Len,IsEmpty,Clear,Append,Clone,CloneLayout,CloneOwned,Contains*,TakeAll,Get(elem),Pop*,Push*,PushOwnedMove,RemoveAt*,Set*,SetOwnedMove,Scalar{op,elem},SliceRange,JoinStr}` (~30) | `xs.push(s)`, `xs.pop()` | the callee borrows the collection by pointer and returns nothing — `hew_vec_push_owned_move(v: *mut HewVec, data: *const c_void)` (`hew-runtime/src/vec.rs:2682`) — so the receiver is **`fork`ed and then borrowed**: `fork %v → %v'` then `borrow{%v'}` around the call, header slot `Borrow` (D-NOMODE — there is no `BorrowMut` mode). **The `fork` is emitted for every heap-carrier class, not only `CowValue`** (D-FORK-R, `ir-ladder.md` §1.3 `fork` row L201): its *realization* is `ensure_unique` for `Vec<i64>`/`Vec<string>` and a register move for `Vec<Conn>`/`Vec<Rc<T>>`, which are unique by class. The second revision's "`fork` iff `CowValue`" left `var v: Vec<Conn>; v.push(c)` with no admitted sequence — the ladder withdrew it for that reason. On a place receiver (state field, env field) the sequence is `load.take %p` → `fork` → `borrow` → call → `store.init %p`, identical for both classes (§1.3 `load.take` row L205); pushed element `move`d in (never cloned in); `Pop*Owned` → `O Option<T>`; `Get` → `G`; `Clone*` → `O`; element release is the runtime calling the glue pointer (plan §1.3) so `Plain/OwnedElement/ClosurePair` classes collapse | `CallRuntimeAbi` + layout descriptor with `drop_fn` | `hew_vec_*` | P2 | `expr.rs:5174-5894`, `expr/vec_element_release.rs:26` `classify_vec_element_release` (NYI 302), `expr.rs:1083`, `vec_index.rs`; codegen `resolved_ty_element_owns_heap_for_owned_vec` (llvm.rs:26059) deleted | `VecElementRelease::Unsupported{NoReleaseProtocol,UnknownValueClass}` stops existing |
| `HashMap{ContainsKey,Clear,Clone,Entries,Free,Get,Insert,Keys,Len,Remove,Values}Layout` + `HashMapNew` + `HashMapNewWithLayout` (13) | `m.insert(k, v)`, `m.get(k)` | key/value `O` operands `move`d on insert; `Get` → `G`/`copy_value` (`CompilerCallKind::HashMapGetCloneLayoutOption`); `Remove` → `O Option<V>` (`HashMapRemoveTakeLayout`); overwrite-key release done by the runtime via glue | `CallRuntimeAbi` + `LayoutProbe` | `hew_hashmap_*_layout` | P2 | `assign.rs` (InsertLayout), `expr.rs:5580-5623`, `8296`; codegen `layout.rs:2747/2861/4839` key/value drop_fn deleted | — |
| `HashSet{Contains,Clear,Clone,Free,Insert,IsEmpty,Len,Remove,ToVec}Layout` + `HashSetNew` + `HashSetNewWithLayout` (11) | `set.insert(s)` | as HashMap | same | `hew_hashset_*_layout` | P2 | `expr.rs:5656-5677` | — |
| `CompilerCallKind::{HashMapGetCloneLayoutOption, HashMapGetCloneLayoutIndex, HashMapRemoveTakeLayout, SupervisorPoolGetOption, LayoutProbe(kind), IdentityAggregate(kind), ClosurePairVec(kind)}` (7) | (compiler-synthesized helpers) | `LayoutProbe`/`IdentityAggregate` become `TargetLayout` facts, not calls; the Get/Remove/Pool kinds are the `rt.call` rows above; `ClosurePairVec` collapses into the Vec glue row | `CompilerCall` deleted | — | P2 (P4 for pool) | `hew-mir/src/model.rs:3780`; producers in `expr.rs`/`actor.rs:834` | — |
| **catalog builtins (0 families today — P1 mints them, D-CATALOG §3.3)**: `println_{i32,i64,u8,u32,u64,f64,bool,str}`, `print_*`, `assert`, `assert_eq`, `assert_ne`, `to_string_*`, `len_str`, `string_concat`, `panic`, `exit`, `random.*` (`len_vec` is a catalog endpoint too but needs a `Vec` receiver, so it lands with the §6 `Vec` row at P2 — see §3.3) | `println(x)`, `assert_eq(a, b)`, `s.len()` | `rt.call{family}`: `G` string/bytes operands inside a `borrow`, `N` scalars; print/assert results `N`; `to_string_*`/`string_concat` → `O string` | `Terminator::Call` to the catalog symbol today; `CallRuntimeAbi` once the families exist | the `runtime_symbol`/`symbol` field of each endpoint's `BuiltinLinkage` row (`hew-hir/src/stdlib_catalog.rs:53-130`: `PrintIntercept`, `RuntimeFfiShim`, `ToStringShim`, `StringCloneShim`). **The second revision's "unverified that every one is an exported C symbol" flag closes positively** — the spot-check is under D-CATALOG-2 in §3.3. **Minting is keyed by symbol, not by endpoint** (D-CATALOG-2): sixteen print endpoints share `hew_print_value`, `to_string_u16`/`to_string_u32` share `hew_uint_to_string`, `to_string_str`/`clone_str` share `hew_string_clone`, so P1 mints one parameterized variant per shared symbol and reuses `StringConcat`/`VecLen` where the symbol already has a family | **P1** | `expr.rs:3484-3555` `CallTarget::Builtin` arm + codegen callee-name interception; both deleted in P5 | — |
| `MathIntrinsic{Sqrt,Exp,Log,Sin,Cos,AbsI,MinI,MaxI,AbsF,MinF,MaxF,Pow,Floor,Ceil,Round}` (15) | `sqrt(x)` | `rt.call` (`N`)→`N` | `CallRuntimeAbi`/LLVM intrinsic | libm | P1 | checker `calls.rs`/`registration.rs` → `Call{Runtime}` route (b) | — |
| `Duration{Abs,Hours,IsZero,Micros,Millis,Mins,Nanos,Secs}`, `Instant{Now,Elapsed,DurationSince}` (11) | `d.secs()`, `Instant.now()` | (`N`)→`N` | `CallRuntimeAbi` | `hew_duration_*`, `hew_instant_*` | P1 | `runtime_builtins.rs:741` duration, `813` instant (NYIs 750, 828) | — |
| `Metric{CounterRegister,GaugeRegister,HistogramRegisterSimple}` (3) | `metrics.counter("app.requests")` | `G` string name → `N` i64 slot id | `CallRuntimeAbi` | `hew_metric_*_register*` | **P2** (D-METRIC) — the only callers are the `metrics.counter`/`gauge`/`histogram` free functions, whose bodies build a record (`Counter { id: id }`, `std/metrics/metrics.hew:182-248` — `StructInit`, P2) and whose rejection path is `panic(f"…")` (interpolation, P2); the `try_*` twins return `Result<Counter, MetricsError>` (`metrics.hew:194-202`, needs `agg.make`/`switch_enum`) | `runtime_builtins.rs:123-134` → `metrics_runtime_calls.rs:68` `lower_metric_runtime_call` (NYI 88) | — |
| `Metric{CounterInc,CounterAdd,GaugeSet,GaugeInc,GaugeDec,GaugeAdd,HistogramRecord}` (7) | `counter.inc()` | receiver is a **record field read**, not a handle: the impl body is `hew_metric_counter_inc(c.id)` — `borrow{c}` `project{id}` → `N`, then `rt.call` (`N`)→`N` | `CallRuntimeAbi` | `hew_metric_*` | **P3** (D-METRIC) — every accessor lives in a trait impl body (`trait CounterMethods` / `impl CounterMethods for Counter`, `metrics.hew:85-108`; likewise Gauge and Histogram) with no `#[extern_symbol]` on the method, so the call site is `Call{ImplMethod}` (§3.3, P3) over a record receiver whose field read is P2 | same | — |
| `Metric{HistogramRegister,VecRegister,VecWith}` (3) | — | — | — | `hew_metric_histogram_register`, `hew_metric_vec_register`, `hew_metric_vec_with` | **unreachable from the Hew surface today** (`grep -rn` over `std/`, `examples/`, `tests/` for these three symbols is empty; `std/metrics/metrics.hew:286` declares only the `_simple` histogram entry). Catalogued for the symbol→family bijection. P3 with the rest of the group if a labelled-metrics surface lands | none | — |
| `Observe{ReadU64,Scrape,Series,Barrier}` (4) | `observe.scrape()` | `G` string name → `N` i64 / `O string` | `CallRuntimeAbi` | `hew_observe_*` | **P1** — free functions over scalars and `string` with no record anywhere (`std/observe.hew:6,13,20,30`: `read(name: string) -> i64`, `scrape() -> string`, `series() -> string`, `barrier() -> i64`) | `runtime_builtins.rs:117-122` → `metrics_runtime_calls.rs:4` `lower_observe_runtime_call` (NYI 19) | — |
| `Rc{New,Clone,Downgrade,Drop,Get,IsUnique,Set,StrongCount,WeakCount}`, `Weak{CloneRc,DropRc,UpgradeRc}` (12) | `Rc.new(v)` | see `RcIntrinsic` row | `RcIntrinsic` | `hew_rc_*` | P2 | `rc_intrinsic.rs` | — |
| `Regex{Capture,Compile,FreeCapture,Handle,Match}` (5) | regex patterns | see `Regex` pattern row | runtime calls | `hew_regex_*` | P2 | `pattern.rs:3784-4255`, `expr.rs:6743` | — |
| `Actor{Ask,AskWithChannel,Cooperate,Demonitor,Link,Monitor,Self,SendById,Spawn,Unlink,GenSinkComplete,GenSinkRegister}`, `LinkRemote` (13) | actor surface | see §3.8 rows; `ActorCooperate` is the scheduler cooperate site (`compute_structural_cooperate_sites`) | `Send/Ask/SpawnActor`, `CallRuntimeAbi` | `hew_actor_*` | P4 | `actor.rs` | — |
| `Reply{ChannelCancel,ChannelFree,ChannelNew,PayloadFree,Wait}` (5) | (ask reply plumbing) | internal to `ask`/`select`/`join` rows; reply payload `O` handed to the awaiting frame | `Ask` terminator | `hew_reply_*` | P4 | `actor.rs:2164`, `task.rs:1552`, `1947`; codegen `thunks.rs:433` reply drop thunk deleted | — |
| `Supervisor{DirectId,ChildGet,NestedGet,PoolChildGet,PoolLen,Stop,RestartAwaitBlocking}`, `LocalPidSupervisor{ChildGet,PoolChildRefGet}` (9) | `sup.worker`, `sup.pool.len()` | (`N`)→`N` `ChildRef`/ints | `CallRuntimeAbi` | `hew_supervisor_*` | P4 | `actor.rs:576-1090` | — |
| `Duplex{Clone,Close,CloseHalf,Pair,PayloadFree,Recv,RecvHalf,Send,SendHalf,TryRecv,TrySend}`, `SendHalf{Send,TrySend}`, `RecvHalf{Recv,TryRecv}` (15) | duplex surface | see duplex row §3.8 | `CallRuntimeAbi` | `hew_duplex_*` | P4 | `actor.rs:476-1691` | — |
| `LambdaActor{Ask,Clone,Downgrade,New,Release,Send,WeakClone,WeakDrop,WeakSend}`, `LambdaBodyAllocReplyBuf`, `LambdaDrainAll` (11) | `actor \|m\| {}` calls | see `SpawnLambdaActor` | `MakeLambdaActor` | `hew_lambda_actor_*` | P4 | `closure_gen.rs:1260-2047` | — |
| `Channel{SendLayout,RecvLayout,TryRecvLayout,SenderClose,ReceiverClose}` (5) | `tx.send(v)`, `rx.recv()` | `send`: `move v`; `recv` → `O Option<T>`; `Sender`/`Receiver` are `O` handles with `RuntimeDropDescriptor::{SenderClose,ReceiverClose}` | `CallRuntimeAbi` / `suspend{ChannelRecv}` | `hew_channel_*_layout` | P4 | `task.rs:2167`; `std/channel/channel.hew:206` `close(consuming self)` | — |
| `Stream{Close,NextLayout,SendLayout,TryNextLayout}`, `Sink{Close,PeerClosed,Write(kind),TryWrite(kind)}` (8) | `s.recv()`, `sink.send(b)` | `Stream`/`Sink` `O` handles (`StreamClose`/`SinkClose` descriptors); element `move` on write, `O` on next | `CallRuntimeAbi` / suspends | `hew_stream_*`, `hew_sink_*` | P4 | `task.rs:2269`, `closure_gen.rs:2832`, `scope.rs:934` `emit_scope_stream_drops`, `972` | — |
| `Task{AwaitBlocking,CompleteThreaded,CompletionObserve,CompletionUnobserve,Free,GetEnv,GetError,GetResult,New,SetEnv,SetResult,SpawnThread}`, `TaskScope{CancelAfterNs,Destroy,JoinAll,New,SetCurrent,Spawn}` (18) | scope/fork/await | see §3.9 rows | task ABI | `hew_task_*` | P4 | `task.rs` | — |
| `CancelToken{IsRequested,Release,Retain}` (3) | `tok.is_cancelled()` | `CancellationToken` is `AffineResource` with `clone = None`; `IsRequested` is `borrow{tok}` → `N`; `Release` is the type's `destroy_value` descriptor. **`Retain` is runtime-internal and is never a SIR `copy_value`** — corrected from the first draft, which called it `copy_value` and would have prescribed an op rule 6b must reject (`E_OWN_CLONE_UNSUPPORTED` on a `clone == None` type). Evidence: no MIR lowering emits the family (`grep -rn CancelTokenRetain hew-mir/src` is empty); the only callers of `hew_cancel_token_retain` are inside the runtime (`hew-runtime/src/scheduler.rs:385`, `await_cancel.rs:259`, `task_scope.rs:205`), so codegen's two arms for it (`runtime_abi.rs:3912`, `llvm.rs:1446`) are unreachable from any producer. The class table needs no `Retain` row for `CancellationToken` | `CallRuntimeAbi` | `hew_cancel_token_*` | P4 | `expr.rs:3234` | wall 2: `clone tok` is `E_OWN_CLONE_UNSUPPORTED` |
| `SelectFirst` (1) | `select { }` | see `Select` | `Select` terminator | `hew_select_first` | P4 | `task.rs:1552` | — |
| `Node{AllowPeer,Connect,Id,IdentityKey,LoadKeys,Lookup,Monitor,Register,SetTransport,Shutdown,Start}`, `RemotePidSend` (12) | `node.connect(addr)` | `G` string operands → `N`/`O Result`; `RemotePid` is `N` leaf | `CallRuntimeAbi` | `hew_node_*` | P4 | `actor.rs:199`, `262`; rest via route (b) | — |
| `{Tcp,Tls,WebSocket}AttachLocal` (3) | `conn.attach(handler)` | `move` the `O` connection into the runtime (consuming) | `CallRuntimeAbi` | `hew_tcp_attach_local` … | P4 | `drop_plan.rs` (TcpAttachLocal consume authority); `std/net/net.hew:692` `attach(consuming self)` | — |
| `AutoMutex{Alloc,Free,Lock,Unlock}` (4) | (actor state guard `Exclusive`) | handler prologue/epilogue `rt.call`; lock token `N` | `AutoLockAcquire/Release` | `hew_auto_mutex_*` | P4 | `actor_state_handle.rs:1-250`; `HirActorStateGuard::Exclusive` | — |
| `DynBox{Alloc,Free}` (2) | `dyn Trait` boxing | part of `dyn.coerce`/glue | — | `hew_dyn_box_*` | P3 | `expr.rs:5015` | — |
| `VtableDispatchPanicOnOob` (1) | (dyn slot guard) | `trap` | `Trap` | — | P3 | codegen | — |
| `HashMapIter`, `VecIter` cursor families (via `BuiltinType::{VecIter,HashMapIter}`, `Iterator`) | `for x in xs` | see `for x in vec` row; cursor `O` borrows the collection | cursor ops | `hew_vec_iter_*` | P2 | `scope.rs:21-946`, `control_flow.rs:853` | — |

`BuiltinMethodRuntime::{None, Fixed(sym), IntegerOverload{default,integer}, ElementOverload{string,bytes}}` (builtin_names.rs:27-38) is the checker's symbol selector for methods **on builtin named types** — it is read once, at `hew-types/src/check/mod.rs:243-262`, to register those methods' symbols. It is not a free-function selector and has nothing to do with the catalog row above. It becomes the family selector feeding `rt.call` for route (b) and needs no SIR row of its own.

**The `Option`-result rule, applied consistently (D-OPTION).** `Option<T>` and
`Result<T,E>`, and the ops that build and consume them (`agg.make`,
`switch_enum`, `destructure`), are P2 everywhere in this document: §7's
`Option`/`Result` row, the phase legend, `Match`, `EnumVariant`, `TryWidthCast`
("P2 (needs Option)") and `NumericMethod{Checked}`. The first draft then put the
`string` sentinel inspectors and `bytes.get` at P1 while typing their results
`Option`. That is not a P1 program: `std/string.hew:772,787,794` declare
`find -> Option<i64>`, `char_at -> Option<char>`, `get -> Option<char>`, and any
use needs a `match` with a payload binder. The rows above now split those
families out at P2. The witness is
`examples/v05/checked-mir/string_sentinel_ops.hew` — a string-only program that
needs `switch_enum` + payload binders (P2) *and* `println` (D-CATALOG), so it
belongs to P2's gate, not P1's. Every remaining `string`/`bytes` family whose
result is a scalar, a bool or a fresh `string`/`bytes` stays P1 — including
`bytes.pop()`, which returns `u8` on the surface (`std/io.hew:59-61`) and aborts
on empty rather than returning an `Option`; its C symbol returns `i64`
(`hew-runtime/src/bytes.rs:427-431`), which is a marshalling fact, not the
result type the D-OPTION split reads. The split is by result type, not
by family name.

**Decision — `bytes.len()` is re-pointed at `hew_bytes_len` in P1 (D-BYTESLEN).**
The `Bytes{Len}` row above says P1, and on main nothing produces the `BytesLen`
family at all. `std/io.hew:58-62` declares the method as
`#[extern_symbol(hew_vec_len)] fn len(buf: bytes) -> i64`, with the source
comment "`len` names hew_vec_len: codegen routes a bytes receiver to the
canonical hew_bytes_len entry." That symbol maps to `RuntimeCallFamily::VecLen`
(`hew-types/src/runtime_call.rs:1706`; there is no bytes case), MIR's
`lower_bytes_len` (`runtime_builtins.rs:414`, reached from the `"hew_vec_len"`
arm at `:67`) emits `hew_vec_len`, and the bytes entry is reached only by a
codegen intercept that branches on the operand's `ResolvedTy`
(`hew-codegen-rs/src/runtime_abi.rs:2211-2226`: "Checker-authority: branch on
`args[0]`'s `ResolvedTy::Bytes`"). The only MIR producer of the symbol
`hew_bytes_len` is bytes-slice open-end lowering
(`hew-mir/src/lower/expr.rs:8745`, comment at `8615`), not `b.len()`.

That is the same name-keyed call-site join D-CATALOG identified for print and
assert, and plan §6's last decision row deletes it with the legacy emitter — so
this document must say which side of the split the row lands on, not leave it.

Decision: **P1 changes the `std/io.hew` declaration to
`#[extern_symbol(hew_bytes_len)]`** so the symbol→family bijection yields
`BytesLen` directly and the receiver type stops being a codegen fact. The family
already carries everything the change needs: a borrows-receiver ownership
contract (`hew-mir/src/runtime_symbols.rs:453-467`, `BorrowsReceiver { scans:
ReceiverScanSet::BYTES }`, shared with every other bytes receiver op) and an ABI
shape row (`runtime_call.rs:1876`). Both the codegen intercept and
`lower_bytes_len` are deleted by it. This is P1 work and not a std-only edit:
today the callee name `hew_bytes_len` falls through to the `_ =>` NYI arm at
`runtime_builtins.rs:154`, so the declaration change alone would turn a working
program into a refusal. Witness: `tests/vertical-slice/accept/bytes_clear.hew`
(`if b.len() != 0`, run.sh:2301) is otherwise a pure P1 program — bytes,
scalars, `if`, `return`.

## 7. Std types with an ownership protocol

| type | value class today (`ValueClass::of_ty`) | SIR kind / ops | release protocol (glue target) | phase | legacy owner | wall |
| --- | --- | --- | --- | --- | --- | --- |
| ints, floats, bool, char, unit, never, `duration`, `instant`, `SupervisorPool` | `BitCopy` | `N`; no ops | none | P1 | `value_class.rs:372-461` (scalar arms) | — |
| `string` | `CowValue` | `O`; `copy_value`=`hew_string_clone` (refcount +1), `destroy_value`=`hew_string_drop`. **A `string` is never `fork`ed** — corrected from the first draft, which said "`unchecked_fork` before mutation" and would have had a P1 implementer build a path the verifier and `ir-ladder.md` (§1.3 `fork` row L201, §4.3 `fork` row L1370) both forbid. There is no string mutator to fork for: `grep -n 'var self\|consuming self' std/string.hew` returns nothing (exit 1), and every string operation returns a fresh value. `hew_string_make_unique` is deliberately not introduced (`ir-ladder.md` §4.3 `fork` row, L1370: `cstring_ensure_unique` reads a 16-byte header at `data-16`, so forking a rodata literal would read `rodata-16`) | `CowHeapRelease::String` | P1 | `temp_drop.rs:5507` `finalize_string_ownership`, `StringRetain{Always,FreshShare,AggregateBorrowedIngress,ActorStateRecordBorrowedIngress}` markers, codegen `retain_string_field_load` (llvm.rs:16698) — all deleted | — |
| `bytes` | `CowValue` | `O`; `copy_value`=`hew_bytes_clone_ref` (refcount +1), `destroy_value`=`hew_bytes_drop`. Unlike `string`, `bytes` **is** `fork`ed: it has mutators (`push`, `set`, `clear`, `append`) and the §6 Bytes row emits `fork` before each. The op is a no-op at runtime today — every bytes mutator calls `ensure_unique` itself and rewrites the `{ptr, offset, len}` triple (`hew-runtime/src/bytes.rs:181, 435-440`) — but it stays an explicit SIR op so the uniqueness fact is in the proof, not in the runtime's head (`ir-ladder.md` §4.3 `fork` row, L1370) | `CowHeapRelease::Bytes` | P1 | `temp_drop.rs:8687`, `BytesRetain`, `composite_own.rs:1317` `derive_local_bytes_drop_allowed` | — |
| `Vec<T>` | `CowValue` today; **`ir-ladder.md` §1.1 collections row (L109, F-collections) joins the class over the element**: `Vec<i64>`/`Vec<string>` → `CowValue`, `Vec<#[resource] T>`/`Vec<Rc<T>>` → `AffineResource`, `Vec<#[linear] T>` → `Linear` | `O`; element glue pointer at construction; `Plain`/`OwnedElement`/`ClosurePairs` classes collapse. A mutating call `fork`s the receiver in **every** class and then borrows it (D-FORK-R, D-NOMODE); only the fork's realization varies — `ensure_unique` for `Vec<string>`, a register move for `Vec<Conn>` (see the §6 Vec row). `let w = v` is `copy_value` for `Vec<string>` and `move` for `Vec<Conn>` (`ir-ladder.md` §11 row 3, L2523), and `clone v` on the latter is rule 6b | `CowHeapRelease::Vec*` → one `hew_vec_drop(glue)` | P2 | `expr/vec_element_release.rs`, `expr/binding_ty_is_plain_vec_tuple.rs`, `ownership.rs:4460-4909` owned-element key harvest | — |
| `HashMap<K,V>`, `HashSet<T>` | `CowValue` today; **the same F-collections rule as the `Vec` row above joins the class over the key and value classes** — `ir-ladder.md` §1.1 collections row L109 covers `Named{builtin: Vec \| HashMap \| HashSet}` in one row, so `HashMap<string, Conn>` is `AffineResource` and `HashMap<string, i64>` is `CowValue`. **Corrected in the third pass**: the second revision updated only the sibling `Vec` row, which left a lane reading this one a copy path the ladder refuses | `O`; key/value glue pointers; a mutating call is `fork` + `borrow{}` exactly as the `Vec` row, with the fork's realization selected by the joined class | `CowHeapRelease::HashMap/HashSet` | P2 | codegen `layout.rs:2737-4845` | wall 2 when the joined `clone` is `None` (`HashMap<string, Conn>`), same as `Vec<Conn>` |
| `Option<T>`, `Result<T,E>` (builtin enums) | by payload (`EnumInPlace`/`BitCopy`) | `agg.make`/`switch_enum`/`destructure`; `N` when payload is `N` | `DropKind::EnumInPlace` glue | P2 | `pattern.rs:37` `is_builtin_option_carrier`; `hew-hir` `?` desugar | — |
| user records | `Unknown` unless marker; heap-field records own | `O` when any field owns; `agg.make`/`project`/`destructure` | `RecordInPlace` glue `hew_drop$R` | P2 | `composite_own.rs:125-2098`, `state_clone.rs` `StateFieldCloneKind`, codegen glue synthesis (llvm.rs:6083-11141) | — |
| user enums (incl. `indirect`) | as records | `switch_enum`; boxed variants freed by glue | `EnumInPlace`/`IndirectEnum` glue | P2 | `pattern.rs:2275` `emit_enum_overwrite_release`; `ty_is_indirect_enum` | — |
| tuples | `CowValue` (HIR) vs `Ty::is_copy` (checker: all-Copy tuples are Copy) — divergent authorities | `tuple.make`/`tuple.get`; `N` iff every element is `N` (one authority: checker value class, plan §1.5) | `TupleInPlace` glue | P1/P2 | `value_class.rs:372-461` (Tuple → CowValue) vs `hew-types/src/ty.rs:1452`; `ownership.rs:2004-2007` documents the divergence | P1 must pick the checker predicate and delete the HIR/MIR copies |
| arrays (`[T; N]`, HIR-desugared to Vec temps) | `CowValue` | as Vec | as Vec | P2 | `hew-hir/src/lower.rs:22547` | — |
| closures / fn values | `PersistentShare` | `O` pair (fn ptr + env); `copy_value` retains env | `ClosurePair` glue; env thunk (`thunks.rs:289`) deleted | P3 | `closure_gen.rs:420-749`; `ownership.rs:7008-7099` closure-pair ingress | — |
| `dyn Trait` | `PersistentShare` | `O` fat pointer; `FrameOwned`/`HeapBoxed` storage | `TraitObject` glue via vtable drop slot | P3 | `expr.rs:5015` | — |
| `Generator<Y,R>`, `AsyncGenerator<Y>` | `AffineResource` | `O` handle; `destroy_value`=`hew_gen_free`; frame-live `O`s destroyed by the cancel edge | `CowHeapRelease::Generator` | P3 | `scope.rs:220` `emit_scope_generator_drops`, `1072`, `1146`; `pattern.rs:699-1400` yield-binding drop proofs (#3119) | — |
| `Stream<T>`, `Sink<T>`, `StreamPair` | runtime handle | `O`; `StreamClose`/`SinkClose` descriptors; `std/stream.hew:253` `close(consuming self)` | `RuntimeDropDescriptor` | P4 | `scope.rs:934-1010` | — |
| `Sender<T>`, `Receiver<T>` | runtime handle | `O`; `SenderClose`/`ReceiverClose`; `std/channel/channel.hew:206` | `RuntimeDropDescriptor` | P4 | codegen `layout.rs:26100` (Sender/Receiver element rows) | — |
| `Duplex`, `SendHalf`, `RecvHalf` | `AffineResource`, `clone = None` (`ir-ladder.md` §1.1 `#[resource]` row, L115) | `O`; `DuplexClose`, `SendHalfClose`, `RecvHalfClose`; split-consume of the pair is a `destructure` | `DropKind::DuplexClose/DuplexHalfClose` | P4 | `split_consume.rs:1-3138`, `actor.rs:1274-1691` | wall 2: `clone = None` |
| `LambdaActorHandle`/`LambdaPid<Msg,Reply>` | `AffineResource`, `clone = Retain` (`ir-ladder.md` §1.1 `LambdaPid` row, L118) — **split out of the `Duplex` row**, which gave it `clone = None` by association | `O`; `copy_value`=`hew_lambda_actor_clone` (mints a new boxed handle); `destroy_value`=`hew_lambda_actor_release`. A `send` of a `LambdaPid` is `Transfer` only despite the retain path (rule 5: `ir-ladder.md` §2.1 rule 5, L1044, and §11 row 5, L2525): a second live handle would let two actors race the release, which joins the dispatch thread on the last handle | `DropKind::LambdaActorRelease` | P4 | `closure_gen.rs:1260-2047`, `actor.rs:1274-1691` | wall 3: `Share` of a `LambdaPid` is rejected |
| `LocalPid<A>`, `RemotePid<A>`, `ChildRef<A>`, `NodeId`, `Location` | non-owning leaf | `N` (`ty_is_nonowning_pid_leaf`) | none | P4 | codegen llvm.rs:25479 | — |
| `MonitorRef` | `#[resource]` (`std/link_monitor.hew:97`) | `O`; `destroy_value`=`MonitorRefClose` (`hew_actor_demonitor`) | `RuntimeDropDescriptor::MonitorRefClose` | P4 | `actor.rs:88`; codegen `lower_drop_runtime` string compare (llvm.rs:21990) deleted | — |
| `CancellationToken` | `AffineResource` | `O`; `CancellationTokenRelease` | descriptor | P4 | `value_class.rs:372-461` (CancellationToken → AffineResource) | — |
| `Task<T>` | `Linear` | `O` consume-once: `AwaitTask` or scope join; leaving one live at exit is rule-1 (zero consumers) | `TaskFree` | P4 | `value_class.rs:372-461` (Task → Linear); `dataflow.rs` `MustConsume` | — |
| `Rc<T>`, `Weak<T>` | `AffineResource` | `O` handles; `RcDrop`/`WeakDropRc` | glue | P2 | `rc_intrinsic.rs` | — |
| `regex.Pattern` | `#[resource]` → `AffineResource`, `clone = None` (`std/text/regex/regex.hew:28-31`) | **`O`** at every use — corrected from "module-table handle `N`". `RegexLiteralRef` clones the module-static handle into the local (`expr.rs:6743-6759`), so the value carries one drop obligation discharged by the type's `close`; the shared table entry is never released by a user frame. Capture strings from a `Regex` pattern arm are separate `O string`s released by `RegexFreeCapture` on the arm's exits | `RuntimeDropDescriptor` / user `close` | P2 | `pattern.rs:3784`, `expr.rs:6743` | wall 2: `clone = None` |
| `#[resource]` records with `close(consuming self)` (`std/fs.hew:503`, `process.hew:33/57`, `net/net.hew:473/680`, `tls`, `quic`, `http`, `smtp`, `websocket`, `arena`, `deque`, `path`, `semaphore`, `cron`, `protobuf`, `xml`, `yaml`, `link_monitor`) | `AffineResource` | `O`; the one shipped consuming use: `close`/`free` is `move self` into the consuming callee; scope exit without close → rule 1 leak error unless the type's glue is the close | `DropFnSpec::UserClose` → glue calls the user `close` | P3 | `hew-hir/src/declared_release.rs`, `return_provenance.rs`, `drop_plan.rs:7190` `drop_kind_for` | resource-shaped `send` is wall 3 |
| `Value` builders with `consuming self` fluent methods (`std/encoding/{json,yaml,toml}` `with_*`, `push_*`, `free`) | `AffineResource` | `move self` into the callee; result `O Self` re-bound (the r_fluent_resource ICE class: SSA rebind, no place reuse) | direct call | P3 | `expr.rs:9228` direct-call args; `ownership.rs:2305` `transfer_identity_owner` deleted | — |
| `ExternProvenance::Root` C-string returns | adopt | `O` string adopted at the boundary (`malloc_string_return`) | `ExternDecl` | P3 | `hew-hir/src/node.rs` `ExternProvenance`; `return_provenance.rs:1508` | — |
| `View` types (`Slice`, `Pointer`, `Borrow`) | `View` | `G`-only; never owned | none | P3 (FFI) | `value_class.rs:372-461` (Slice/Pointer/Borrow → View) | — |
| `TypeParam` in a polymorphic body | `Unknown` | never lowered: instances only | — | P2 | `value_class.rs:372-461` (TypeParam → Unknown) | — |

## 8. Legacy owner functions the phases delete (per file, grep of `fn` names)

Every function below is a deletion target in P5 once its row's phase has
landed. Line numbers are `fn` starts on main.

| file | functions (line) | phase that replaces them |
| --- | --- | --- |
| `hew-mir/src/lower/expr/runtime_builtins.rs` | `lower_runtime_call` 11, `lower_bytes_unit_result` 174, `lower_bytes_{push 183,pop 216,set 258,is_empty 287,contains 315,clear 349,append 381,len 414,get_option 454}`, `lower_string_{get_option 522,sentinel_option 590,char_count 652}`, `lower_duration_runtime_call` 741, `emit_actor_self_handle` 783, `lower_instant_runtime_call` 813 | P1 (bytes/string/duration/instant; `lower_bytes_len` 414 goes with D-BYTESLEN, and the codegen intercept it depends on, `hew-codegen-rs/src/runtime_abi.rs:2211-2226`, is deleted in the same change), **P2 for the three `Option`-result lowerings — `lower_bytes_get_option` 454, `lower_string_get_option` 522, `lower_string_sentinel_option` 590 — which D-OPTION moved out of P1 with their families**, P4 (`ActorSelf`) |
| `hew-mir/src/lower/actor.rs` | `record_pending_outbound_args` 18, `record_pending_actor_request_args` 26, `actor_state_field_for_target` 45, `lower_actor_link_or_monitor` 88, `lower_node_monitor` 199, `lower_node_link_remote` 262, `lower_actor_unlink` 358, `lower_simple_{void 412,int 435}_runtime_call`, `lower_duplex_pair` 476, `lower_supervisor_stop` 576, `lower_supervisor_child_get` 612, `lower_await_restart` 627, `partitioned_static_slot_index` 709, `lower_supervisor_nested_get` 772, `lower_pool_accessor` 834, `emit_pool_child_ref_get` 931, `lower_pool_index` 956, `pool_accessor_shape_error` 1003, `make_static_child_ref` 1015, `make_child_ref` 1029, `child_ref_role_of` 1052, `capture_supervisor_token` 1090, `lower_duplex_send` 1129, `lower_duplex_close` 1274, `lower_duplex_half_extract` 1369, `lower_half_send` 1433, `lower_duplex_recv` 1514, `lower_half_close` 1590, `lower_packed_args_payload` 1692, `pack_actor_payload_from_places` 1715, `actor_method_info` 1763, `actor_handler_mints_an_owner_for_message` 1814, `reject_proven_foreign_actor_message_args` 1827, `lower_actor_send` 1839, `finish_{blocking 1916,nonblocking 1940}_actor_send`, `lower_actor_gen_stream` 1975, `build_receive_gen_channel` 2081, `lower_actor_ask` 2164, `remote_actor_method_info` 2319, `lower_remote_actor_ask` 2399, `invalid_spawn_arg_note` 2465, `lower_spawn_actor` 2518, `lower_spawn_actor_{state_or_diag 2658,init_args 2685,state 2716,state_arg 2790}`, `default_actor_state_field_value` 2812 | P4 |
| `hew-mir/src/lower/task.rs` | `select_{stream_item 16,receiver_item 27,task_output 38}_ty`, `spawn_env_ownership_from_closure_manifest` 45, `task_scope_ty` 100, `lower_task_scope` 109, `direct_no_arg_unit_callee` 147, `mir_sanitize_symbol` 273, `task_entry_adapter_symbol` 280, `ensure_task_entry_adapter` 309, `task_entry_adapter_blocks` 359, `synthesize_task_entry_adapter` 397, `lower_spawned_call_task` 479, `lower_no_arg_value_callee_task` 587, `lower_spawned_args_call_task` 703, `synthesize_fork_entry_shim` 939, `lower_spawned_closure_task` 1094, `lower_fork_block_task` 1172, `lower_scope_deadline` 1293, `lower_await_task` 1394, `lower_select` 1552, `lower_join` 1947, `lower_conn_await_read` 2055, `lower_channel_recv_await` 2167, `lower_stream_recv_await` 2269, `lower_listener_await_accept` 2372 | P4 |
| `hew-mir/src/lower/closure_gen.rs` | `is_lambda_pid_ty` 19, `gen_env_capture_admissible` 59, `gen_env_capture_field_plan` 85, `capture_env_whole_escape_requires_clone` 111, `reject_capture_env_whole_escape{,_expr}` 118/139, `sanitize_symbol_component` 157, `closure_env_pointer_ty` 164, `lower_closure_literal` 171, `closure_env_capture_ownership_by_strategy` 420, `closure_env_allocation_manifest` 474, `materialize_closure_env` 489, `child_builder_tables` 750, `lower_closure_shim` 828, `lower_named_fn_invoke_shim` 1072, `lower_spawn_lambda_actor` 1260, `lower_lambda_actor_call` 1829, `lower_gen_block` 2048, `emit_pump_peer_closed_check` 2713, `mint_pump_yield_value_owner` 2763, `emit_pump_yield_value_release` 2806, `build_stream_producer_pump` 2832, `lower_yield_expr` 2960 | P3 (closures, generators), P4 (lambda actors, pump) |
| `hew-mir/src/lower/machine_synth.rs` | `with_actor_handler_identity` 108, `stream_producer_shell_params_and_return_ty` 143, `lower_actor_receive_handlers` 169, `lower_actor_init_handler` 395, `lower_actor_lifecycle_handlers` 490, `push_lifecycle_not_wired_diagnostic` 1183, `mangle_actor_receive_handler` 1216, `emit_machine_step_transition_return` 1811, `is_machine_state_passthrough` 1926, `emit_machine_transition_out_drops` 2002, `lower_machine_lifecycle_block` 2055, `hir_expr_reads_config_field` 2100, `supervisor_children_in_spawn_order` 2107, `local_pid_of` 2284, `unknown_self_fields_in_block` 2854, `collect_unknown_self_fields_in_{block 2860,expr 2905}` | P4 (machine parts move to the HIR desugar) |
| `hew-mir/src/lower/control_flow.rs` | `normalize_range_integer_operand` 18, `lower_let_else_stmt` 75, `lower_while` 342, `lower_while_let` 446, `classify_while_let_binding_iteration_owner` 853, `while_let_{reassignment_provably_fresh 952,body_has_exit_after_reassignment 968,block_has_unsafe_exit 1012,expr_has_unsafe_exit 1055,body_contains_targeting_continue 1137,expr_contains_targeting_continue 1188,skipped_owned_payload_field 1324}`, `lower_if_let` 1395, `lower_for_range` 1778, `lower_loop` 2207, `lower_logical_{and 2280,or 2341}` | P1 (while/loop/for-range/and/or), P2 (let-else, while-let, if-let) |
| `hew-mir/src/lower/pattern.rs` | `refcounted_overwrite_release_symbol` 23, `is_builtin_option_carrier` 37, `exact_whole_binding_owner_handoff` 87, `lower_match` 174, `retain_typed_join_branch` 316, `is_{project_match_scrutinee_ty 394,vec_iter_next_scrutinee 406,generator_next_scrutinee 419,recv_next_scrutinee 448}`, `generator_yield_drop_symbol` 514, `owned_composite_release_kind` 623, `vec_release_symbol_verdict` 667, `emit_generator_yield_binding_drop` 699, `record_body_end_release_event` 764, `generator_yield_linear_handoff_owner` 801, `generator_yield_binding_drop_safe{,_until_scope} 906/978`, `call_carrier_{body_end_drop_safe 928,scope_boundary 1078,release_precedes_scope 1108}`, `generator_yield_block_paths_drop_safe` 1181, `project_match_scrutinee_is_bitcopy` 1408, `local_storage_is_interior_alias` 1441, `project_field_inline_drop_symbol` 1509, `field_drop_{in_place_admissible 1595,aggregate_admissible 1622,slot_dischargeable 1680}`, `project_record_{leaf_field_drop 1601,owned_field_list 1765}`, `reassign_rhs_may_alias_binding` 1832, `ty_has_unretained_owned_leaf` 1976, `emit_refcounted_overwrite_release` 2028, `publish_overwrite_owner_release` 2059, `emit_local_overwrite_release` 2083, `emit_enum_overwrite_release` 2275, `project_tuple_owned_field_list` 2529, `match_project_scrutinee_reject` 2573, `lower_match_binding_chain` 2643, `preflight_selected_project_arm` 2739, `emit_selected_project_arm` 2869, `lower_project_match_scrutinee_local` 3120, `lower_match_project` 3166, `lower_match_project_predicate_chain` 3259, `lower_match_literal{,_constant} 3495/3640`, `lower_match_regex` 3784, `hir_scrutinee_is_unconditional_ephemeral_producer` 4256, `classify_{producer 4310,call_arm 4418,builtin_getter 4441}_scrutinee_origin`, `method_scrutinee_emitted_symbol` 4462, `scrutinee_precise_bits` 4503, `classify_scrutinee_origin` 4532, `lower_match_arm_guard` 4561, `record_projected_payload_provenance` 4576, `record_match_arm_binding_scope` 4634, `lower_match_enum_tag` 4649, `emit_payload_variant_predicate_checks` 6006, `emit_match_arm_binding` 6090 | P1 (overwrite release, 2028-2275), P2 (match), P3 (generator yield drops 514-1400) |
| `hew-mir/src/lower/assign.rs` | `assign` 17, `field_store_target_path` 860 | P1 (locals), P2 (fields/index), P4 (actor state) |
| `hew-mir/src/lower/scope.rs` | `ty_is_exact_vec_iter` 21, `reject_unsupported_vec_iter_boundary` 37, `reject_vec_iter_collection_storage_boundary` 65, `vec_iter_value_is_owned` 95, `emit_pending_defers` 128, `record_binding_scope{,_in}` 152/170, `note_scope_span` 181, `emit_scope_generator_drops` 220, `vec_iter_cursor_release_{symbol 265,protocol 300}`, `vec_iter_param_boundary_mode` 282, `register_vec_iter_scope_owner` 348, `emit_scope_vec_iter_drops` 489, `emit_vec_iter_drops_for_exit_edge{,_except} 551/558`, `emit_flag_gated_vec_iter_{cursor 582,value 607}_release`, `emit_vec_iter_value_release` 677, `vec_field_src_consumes_bare_actor_state_field` 748, `vec_iter_source_{projects_actor_state_field 773,live_binding_record_field_root 833,live_binding_record_field_path 847,indexes_owned_element_vec 906}`, `emit_scope_stream_drops` 934, `emit_stream_drops_for_exit_edge` 972, `emit_defers_for_return` 1011, `emit_defers_for_break_continue` 1043, `emit_generator_drops_for_break_continue` 1072, `emit_generator_yield_value_drops_for_exit_edge` 1146, `resolve_loop_frame` 1198, `active_string_yield_builder` 1238 | P1 (scopes), P2 (VecIter), P3 (generator), P4 (defer, streams) |

**`hew-mir/src/lower/expr.rs` is deliberately not inventoried above.** It is the
dispatch file, not a per-construct owner: its `fn` list runs to hundreds of
entries and most of them are arms, not lowerings. Its P5 deletion targets are
named in the rows that own them instead, and the third pass added four the
second revision had mis-filed under `runtime_builtins.rs`: `lower_string_index`
8617, `lower_string_slice` 8644, `lower_bytes_index` 8692, `lower_bytes_slice`
8717 (all P1, the §3.4 `Index`/`Slice` rows). A P5 implementer working from this
section alone would find no owner for `b[i]`; work from the rows.

## 9. NotYetImplemented sites grouped by construct (281 production sites)

Each group names the phase whose rows above make the site unreachable
(implemented) or turn it into a checker/SIR verifier rejection (reject).
"internal" marks refusals of the legacy ownership authority's own
bookkeeping: they have no surface construct and are deleted outright in P5
because the OSSA verifier (rules 1-6) is the replacement proof.

| group | sites | phase / outcome |
| --- | --- | --- |
| runtime-call arity/result-type shape (bytes, string, duration, instant, observe, link/monitor/unlink, node) | `runtime_builtins.rs` 154, 191, 224, 266, 295, 323, 357, 389, 422, 660, 704, 717, 750, 828; `metrics_runtime_calls.rs` 19; `actor.rs` 105, 157, 223, 271, 290, 324, 366, 380 | P1 (typed `rt.call` signatures from the family table make shape checks checker-side); P4 for actor/node symbols. `runtime_builtins.rs:154` is also where a `hew_bytes_len` callee lands today, which is why D-BYTESLEN is P1 work and not a `std/io.hew` edit |
| `Option`-result sentinel inspectors: arity/result-type shape inside the three `Option`-returning lowerings (bytes get, string get, string sentinel) | `runtime_builtins.rs` 463, 479, 531, 547, 601, 617 | **P2** — D-OPTION moved `Bytes{Get}` and `String{CharAt,CharAtUtf8,Find,Get}` to P2 with their result type, and these six sites are the arity checks inside exactly those lowerings. **Split out in the third pass**: the second revision left them in the P1 group above and flagged the inconsistency without fixing it, which put the Counts section's P1/P2 NYI totals out by six in each direction. Verified by function bounds: `grep -nE '^    fn ' hew-mir/src/lower/expr/runtime_builtins.rs` gives 454, 522, 590, 652, so every site listed here falls inside a P2 lowering |
| metric register/accessor arity shape | `metrics_runtime_calls.rs` 88 | P2 for the register half, P3 for the accessors (D-METRIC): the site is unreachable until a record-returning free function (P2) or a trait impl body (P3) lowers |
| literals and numeric operators | `expr.rs` 7192, 7307, 7330, 7621, 7687, 7759, 7800, 7899, 7946, 8064, 8081 | P1 implemented (implicit integer coercion 7307/7330 stays a checker rejection) |
| structural/enum equality | `expr.rs` 7482, 7516 | P2 implemented (`struct.eq`) |
| ambiguous byte-copy alias rebind | `expr.rs` 1868 | P1 (SSA rebind; no alias tracking) |
| direct call resolution and symbol maps (incl. var-self callee 8955, 8984) | `expr.rs` 3390, 3405, 3597, 3612, 3683, 3823, 8955, 8984; `mod.rs` 14138 | P1 (callable identity by `MirCallableKey`; P3 for impl/var-self) |
| runtime family / catalog builtin without a route | `expr.rs` 3438, 3464, 3509, 5861 | P1/P2. Every `RuntimeCallFamily` variant has a row in §6, **and** §6's first row covers the catalog endpoints that have no family on main (`expr.rs:3509` is exactly that hole: "catalog builtin endpoint … has no direct MIR route"). D-CATALOG closes it at P1 by minting the families |
| named function / builtin used as a value | `expr.rs` 2876, 2908, 2935, 3019 | P3 (`closure.make` for named fns; builtin-as-value stays rejected) |
| closures: suspension inside, string return contract, captured var reassign, whole-value capture move | `closure_gen.rs` 125, 721, 865; `assign.rs` 657 | P3 (P4 for suspension inside a closure) |
| generators: yield binding drops, captures into generator, yield outside body | `pattern.rs` 719, 5525; `closure_gen.rs` 2217, 2291, 2970; `ownership.rs` 6919 | P3 (#3119 cases) |
| records: field access, struct init, functional update | `expr.rs` 3946, 4004, 4061, 4246, 4307, 4337, 4380, 4556, 4714, 4730, 4756 | P2 (`destructure` for the consumed base) |
| clone (records, Vec, HashMap, HashSet, VecIter, generic param, indexed value) | `expr.rs` 5339, 5469, 5485, 5523, 5539, 5580, 5623, 5656, 5677, 6935, 6958, 6973, 7020, 8296 | P2 (wall 2 for drop-only members) |
| Vec index / slice / element release / drop-only element ops | `vec_index.rs` 34, 161; `expr.rs` 1083, 8403, 8454; `vec_element_release.rs` 302 | P2 (element glue pointer removes `NoReleaseProtocol`) |
| VecIter cursor borrow discipline | `move_value.rs` 331, 784; `expr.rs` 317, 3735, 5109, 9122, 9136, 9284, 9488; `scope.rs` 47, 78; `assign.rs` 29, 66; `mod.rs` 13801 | P2 (cursor is an `O` value inside `borrow{collection}`; violations become rule-3 errors) |
| let-else / if-let / while-let scrutinee shape and reassignment | `control_flow.rs` 98, 496, 530, 902, 915, 928, 940, 1419 | P2 |
| match: scrutinee shape, project/destructure, literal, guard, regex | `pattern.rs` 188, 2425, 2505, 2661, 2757, 2775, 2843, 3155, 3187, 3206, 3267, 3286, 3296, 3319, 3533, 3588, 3696, 3715, 3819, 3845, 3914, 4031, 4110, 4210, 4801, 6019; `expr.rs` 6777 | P2 (3696 embedded NUL stays a checker rejection) |
| static trait dispatch on receiver shape | `expr.rs` 5965, 8831 | P3 (instance service) |
| extern returning a string-returning callable | `expr.rs` 9538 | P3 (FFI ownership row) |
| actor send/ask/spawn/gen-stream/remote | `actor.rs` 1773, 1787, 1852, 1990, 2002, 2177, 2191, 2288, 2329, 2380, 2540, 2577, 2618, 2672, 2700, 2859; `mod.rs` 5189, 5224, 5243, 5285, 5299, 5318 | P4 (2288 blocking-caller ask stays rejected; 5189 non-Send payload is wall 3) |
| duplex / half handles | `actor.rs` 490, 1138, 1199, 1283, 1333, 1378, 1443, 1465, 1524, 1538, 1548, 1599, 1615, 1632 | P4 |
| supervisor stop / await_restart / pool accessor / child wiring / config values | `actor.rs` 583, 635, 647, 1005; `machine_synth.rs` 2391, 2544; `mod.rs` 3043, 3125, 3170, 3240, 3259, 3317, 3333, 3349 | P4 (config-value shapes stay checker rejections) |
| lambda actors | `closure_gen.rs` 1876, 1940 | P4 |
| tasks: scope, spawned call, value task, fork block, deadline, await, select, join, deadline awaits | `task.rs` 164, 179, 197, 218, 243, 261, 491, 503, 518, 599, 619, 640, 652, 715, 735, 756, 768, 780, 801, 830, 1122, 1134, 1208, 1218, 1231, 1249, 1301, 1345, 1477, 1590, 1664, 1691, 1722, 1856, 1965, 2114, 2230, 2332, 2436 | P4 (1134/1231 non-Send capture and 1249 borrowed capture are wall 3 / rule 3) |
| internal: typed produced-value handoffs, provisional owners, identity transfer, discarded results, foreign provenance | `ownership.rs` 193, 907, 1554, 1840, 1853, 2313, 2323, 2352, 3087, 3336, 3355, 3382, 6290, 6519 | P5 delete (no construct) |
| internal: OwnedCursor call handoff | `owned_cursor_call.rs` 33, 48, 63, 86, 109, 120, 232, 243, 269 | P5 delete |
| internal: borrowed-argument provisional owners | `borrowed_argument_owner.rs` 85, 106 | P5 delete |
| internal: owned call-carrier plans and affine consume splices | `mod.rs` 4520, 4547, 4582, 4650, 4806, 4817, 4865, 5060, 5094, 7906; `move_value.rs` 488 | P5 delete |

Sum of the site lists above = 281 (checked by `scratchpad/count.py` over this file; no site appears in two groups).

HIR-side `NotYetImplemented`: the grep returns 14 hits, of which **11 are
constructions** — `hew-hir/src/lower.rs` 15865, 18777, 18883, 21550, 28492,
28817, 30114, 30182, 30624 (the `unsupported` helper behind ~75 call sites: `?`
shapes, for-await, `| after` shapes, nested patterns, qualified initializers,
raw deref), `verify.rs` 2477, 2510. The remaining three (`verify.rs:2473`,
`lower.rs:32564`, `lower.rs:39756`) read an existing diagnostic and produce
none. The 11 constructions
are surface rejections that stay in HIR; P2/P4 desugar lanes decide each one
(rows in §3.3, §3.5, §4 note them).

## Counts

Computed over the tables in §1-§7 of this file by `scratchpad/count.py` (a row
counts under the first phase named in its phase cell; §8 and §9 are inventories,
not construct rows). The script prints `BAD`/`NOPHASE` for any row it cannot
parse; the run behind the numbers below printed neither.

Rows per phase, 228 → 234 → 238 → 240 over the three 2026-09-01 passes.

The first pass added six rows and moved none: P1 +3 (the catalog free-function
row split off the builtin-method row, `Slice` on `string`, the §6 catalog family
group), P2 +2 (the `Bytes{Get}` and `String{CharAt,CharAtUtf8,Find,Get}`
`Option`-result rows split off their P1 parents under D-OPTION — the parents stay
P1 with fewer variants), P4 +1 (`LambdaPid` split out of the `Duplex` row).

The second pass added four rows and moved one: `Bytes{Len}` split off the P1
Bytes row (D-BYTESLEN, P1 +1), and the single `Metric`/`Observe` row became four
— `Observe` P1, `Metric{*Register}` P2, `Metric{accessors}` P3,
`Metric{HistogramRegister,Vec*}` P3-unreachable — so the P1 row it replaced moves
out (P1 net 0 from that split, P2 +1, P3 +2). The §3.2 escaping-`var` row was
rewritten, not removed: under D-NOPLACE it stays a P1 row that says the construct
does not exist, so no lane goes looking for it. 234 → 238.

The third pass added two rows and moved none, both P1 and both in §3.1: the
`Binary{Equal,NotEqual}` row on string/bytes split into three (a `string`
equality row, a `bytes` equality row and — the row that did not exist at all —
ordered comparison on `string`), all under D-STRCMP. 238 → 240:

| phase | rows |
| --- | --- |
| P1 | 65 |
| P2 | 63 |
| P3 | 31 |
| P4 | 81 |
| total | 240 |

Rows per SIR op (rows whose SIR column names the op literally; one row can name
several, and a row that names an op to *deny* it — the `string` row's "never
`fork`ed" — counts too, so read this as coverage of the vocabulary, not as a
count of emission sites):

| op | rows |
| --- | --- |
| `rt.call{}` | 47 |
| `suspend{}` | 30 |
| `borrow{}` (begin/end_borrow) | 30 |
| `copy_value` | 29 |
| `move` | 26 |
| `destroy_value` | 19 |
| `destructure` | 15 |
| `switch_enum` / `switch_int` | 14 |
| `binary` / `unary` / `cast` / `tuple.*` | 13 |
| `project` | 12 |
| `alloc_place` / `load.*` / `store.*` | 12 |
| `bbarg` (block arguments) | 11 |
| `const.*` | 11 |
| `agg.make` | 10 |
| `fork` | 10 |
| `spawn` / `send{}` / `ask` | 7 |
| `call` / `call.indirect` (named literally; most call rows say "as User") | 5 |
| `closure.make` | 4 |
| `cmp.identity` / `str.eq` / `struct.eq` | 4 |
| `end_lifetime` | 4 |
| `dyn.*` | 3 |
| `trap{}` | 3 |

Two op counts fell in the second pass and the drop is the point, not noise:
`end_lifetime` 6 → 4 and `alloc_place`/`load.*`/`store.*` 11 → 10, because
D-NOPLACE deleted the escaping-`var` row's place ops and the `Let(_, None)` and
`Assign` escaping branches. Every surviving row that names a place op is P3
(env fields) or P4 (actor state, coroutine frames, message payloads). `borrow{}`
19 → 22, `rt.call{}` 44 → 46 and `project` 11 → 12 were the Metric and
`Bytes{Len}` splits.

The third pass moved seven counts up and none down, all from D-FORK-R and
D-NOMODE writing the mutating-call sequence out in full where the rows used to
say `BorrowMut`: `borrow{}` 22 → 30 and `fork` 9 → 10 are that sequence;
`alloc_place`/`load.*`/`store.*` 10 → 12 and `destructure` 12 → 15 are the
place-receiver variant (`load.take` → `fork` → `borrow` → call → `store.init`)
and the payload-`destructure` corrections in the actor and machine rows; `move`
25 → 26, `rt.call{}` 46 → 47 and the scalar group 12 → 13 are the three D-STRCMP
rows. **No count fell, and that is the check**: the third pass replaced a
non-existent mode with real ops rather than deleting coverage. `deref` (Legend)
is still not in this table: `count.py`'s key list is fixed and predates it — add
the key before the next re-run.

NYI sites per phase outcome (§9): P1 48, P2 89, P3 17, P4 91, P5-delete 36;
total 281. The P1/P2 split moved by six from the second pass's `P1 54, P2 83`,
which was wrong in exactly the way the second pass flagged and did not fix: the
six arity checks inside `lower_bytes_get_option`, `lower_string_get_option` and
`lower_string_sentinel_option` were counted under a P1-tagged group while
D-OPTION had already moved their families to P2. A P-lane implementer sizing P1
off the old numbers was over by six sites, and P2 under by six.
