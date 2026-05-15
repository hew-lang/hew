use hew_hir::{BindingId, IntentKind, SiteId, ValueClass};
use hew_types::ResolvedTy;

/// Distinguishes shared (read-only, may alias) from mutable (unique,
/// no-alias) borrows for the aliasing check. The check itself is
/// declared in `MirCheck::Aliasing` but the spine has no construction
/// surface for borrows yet — the variant exists so the borrow
/// lowering that lands later doesn't have to retrofit the kind enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BorrowKind {
    Shared,
    Mutable,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrPipeline {
    pub thir: Vec<ThirFunction>,
    pub raw_mir: Vec<RawMirFunction>,
    pub checked_mir: Vec<CheckedMirFunction>,
    pub elaborated_mir: Vec<ElaboratedMirFunction>,
    pub diagnostics: Vec<MirDiagnostic>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    pub statements: Vec<MirStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RawMirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    /// Type-indexed local registers consumed by the backend-authority `Instr`
    /// stream. `locals[i]` is the `ResolvedTy` of `Place::Local(i as u32)`.
    /// The lowering pass allocates one local per value-producing HIR
    /// expression and per `Let`-introduced binding.
    pub locals: Vec<ResolvedTy>,
    pub blocks: Vec<BasicBlock>,
    pub decisions: Vec<DecisionFact>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    pub id: u32,
    /// Checker-authority stream consumed by `check_raw_mir` and the
    /// use-after-consume / D10 passes. Carries every Hew-level statement and
    /// expression site with its `SiteId`, `BindingId`, and `ResolvedTy`.
    pub statements: Vec<MirStatement>,
    /// Backend-authority stream consumed by `hew-codegen-rs::llvm`. One
    /// `Instr` per machine-level value movement. Both streams are populated
    /// by the same `lower::Builder` pass so the checker and the emitter
    /// agree on what each `SiteId` resolves to.
    pub instructions: Vec<Instr>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    /// Return whatever has been written into `Place::ReturnSlot`. The
    /// emitter loads the slot and emits `ret`.
    Return,
    /// Unconditional branch to another block in the same function.
    Goto { target: u32 },
    /// Two-way branch on an i1/i8/i32/i64 local treated as a boolean.
    Branch {
        cond: Place,
        then_target: u32,
        else_target: u32,
    },
    /// Call into a sibling function by name; store its return value into
    /// `dest`, then branch to `next`. Cluster 1 doesn't construct this; it
    /// exists so the emitter match is exhaustive.
    Call {
        callee: String,
        args: Vec<Place>,
        dest: Place,
        next: u32,
    },
    /// Hard abort: emit a trap or `unreachable`. Used by future panic
    /// lowering; Cluster 1 doesn't construct this.
    Panic,
    /// Generator suspension: yield `value` to the resumer and continue
    /// at `next` on resume. The presence of this terminator in a
    /// function's CFG is what makes `MirCheck::GeneratorBorrowAcrossYield`
    /// interesting; the v0.5 integer spine never constructs it.
    /// Declared here so the borrow-liveness check has a place to look.
    Yield { value: Place, next: u32 },
    /// Actor message send. The sent value at `value` crosses the
    /// actor boundary; `MirCheck::ActorSendEscape` checks the value's
    /// transitive references satisfy the `Send` constraint. Declared
    /// here so the escape check has a construction site to look for;
    /// the v0.5 integer spine never constructs it.
    Send {
        actor: Place,
        value: Place,
        next: u32,
    },
    /// Actor ask: send `value` to `actor` on a caller-owned reply
    /// channel and resume at `next` once the reply has been received.
    /// The terminator carries two distinct Places by design:
    ///
    /// - `channel` — the `HewReplyChannel*` slot allocated by codegen.
    ///   Used for the runtime ABI sequence
    ///   `hew_reply_channel_new` → `hew_actor_ask_with_channel` →
    ///   `hew_reply_wait` on the winning path, and
    ///   `hew_reply_channel_cancel` → `hew_reply_channel_free` on
    ///   loser-cleanup. Codegen-internal; not user-visible.
    /// - `reply_dest` — the user-visible binding that receives the
    ///   reply value. Populated from `hew_reply_wait`'s return on win.
    ///
    /// Declared variant. The v0.5 integer spine has no construction
    /// surface today — HIR-to-MIR lowers `select{}` arms into
    /// `Terminator::Select` with `SelectArmKind::ActorAsk`; per-arm
    /// body-block construction (the seam that would terminate an arm
    /// body with `Terminator::Ask`) is the `select-wait-dispatch`
    /// cluster's responsibility. Non-select `actor.method()` lowering
    /// is the `actor-method-call-lowering` cluster's responsibility.
    /// The variant is declared here so the MIR shape is forward-
    /// compatible with both clusters and so `MirCheck::ActorAskEscape`
    /// has a construction site to look for when actor-call lowering
    /// lands.
    Ask {
        actor: Place,
        value: Place,
        channel: Place,
        reply_dest: Place,
        next: u32,
    },
    /// Sealed `select{}` construct. The terminator carries the per-arm
    /// discriminator and per-arm body block ids; the runtime substrate
    /// that decides the winner and runs loser-cleanup is supplied by
    /// codegen + runtime entries that are not yet wired. Declared here
    /// so the construct's MIR shape is forward-compatible with the
    /// runtime substrate; codegen rejects this terminator with a
    /// `FailClosed` error today.
    ///
    /// The arm vector is non-empty (HIR enforces) and contains at most
    /// one `AfterTimer` arm (HIR enforces). The `next` slot is the
    /// block reached after the winning arm body completes — the join
    /// edge that converges the per-arm bodies.
    Select { arms: Vec<SelectArm>, next: u32 },
}

/// One arm of a sealed `select{}` terminator. Declared-only — the v0.5
/// pipeline never constructs a `Terminator::Select` with attached body
/// blocks; codegen fails closed before reaching the per-arm body
/// dispatch. The per-arm `body_block` is reserved for the cleanup-CFG
/// wire-up when the runtime substrate lands.
#[derive(Debug, Clone, PartialEq)]
pub struct SelectArm {
    pub kind: SelectArmKind,
    /// Block id reached when this arm wins. Unused while codegen fails
    /// closed; reserved for the cleanup-CFG wire-up.
    pub body_block: u32,
    /// `Some(place)` for arms that bind a value (stream/ask/await);
    /// `None` for the `AfterTimer` arm.
    pub binding: Option<Place>,
}

/// The four sealed arm forms mirrored from HIR. The MIR layer carries
/// only the discriminator + the place(s) holding the source operand;
/// the per-form runtime contract is documented at the codegen
/// fail-closed match arms.
#[derive(Debug, Clone, PartialEq)]
pub enum SelectArmKind {
    /// `next(<stream>)` — pending read on a stream.
    StreamNext { stream: Place },
    /// `<actor>.<method>(<args>)` — actor ask.
    ActorAsk {
        actor: Place,
        method: String,
        args: Vec<Place>,
    },
    /// `await <task>` — task completion.
    TaskAwait { task: Place },
    /// `after <duration>` — timer.
    AfterTimer { duration: Place },
}

/// An addressable target for a load or store in the backend-authority
/// instruction stream. Cluster 1 needs only `Local(N)` and `ReturnSlot`;
/// later clusters add `YieldSlot`, enum-payload projection, field
/// projection, deref, etc.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Place {
    Local(u32),
    ReturnSlot,
}

/// Integer comparison predicate. Maps 1:1 to LLVM `IntPredicate`. The
/// signed-ness selector is intentional: Hew's spine treats `int` as a
/// signed 64-bit integer, so the default cmp lowerings are signed
/// comparisons. Once unsigned types reach value-bearing positions in
/// the spine, the lowering picks the unsigned variant from the same
/// enum; the IR shape doesn't change.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpPred {
    Eq,
    NotEq,
    SignedLess,
    SignedLessEq,
    SignedGreater,
    SignedGreaterEq,
}

/// Minimal machine-level instruction set for the spine subset (integer
/// literals, integer add, value moves). Each variant maps to a single
/// inkwell builder call in `hew-codegen-rs::llvm`.
///
/// Variants the emitter cannot lower (Drop on a live heap value, anything
/// coroutine-shaped) emit a hard error rather than silently no-op; the
/// per-variant rejection happens at lowering time, not here.
#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    /// `dest = const <value>` as i64.
    ConstI64 { dest: Place, value: i64 },
    /// `dest = lhs + rhs` on i64.
    IntAdd { dest: Place, lhs: Place, rhs: Place },
    /// `dest = lhs - rhs` on i64.
    IntSub { dest: Place, lhs: Place, rhs: Place },
    /// `dest = lhs * rhs` on i64.
    IntMul { dest: Place, lhs: Place, rhs: Place },
    /// `dest = (lhs <pred> rhs)` on integers. The result is written into
    /// `dest` as an integer truth value: `1` for true, `0` for false. The
    /// dest's local type controls the result width (today every cmp dest
    /// is allocated as the front-half's `bool` resolved to i64 by the
    /// type checker; the lowering zero-extends the LLVM i1 result so the
    /// stored value matches the dest's width). When a real bool type
    /// lands in v0.6, the dest narrows to i8/i1 without IR-level rework.
    IntCmp {
        dest: Place,
        pred: CmpPred,
        lhs: Place,
        rhs: Place,
    },
    /// `dest = <src>` — load `src`, store into `dest`.
    Move { dest: Place, src: Place },
    /// Run the drop ritual for `place`. Cluster 3 makes this first-class:
    /// `drop_fn = Some(name)` calls the `@resource` type's declared
    /// `close(consuming self)` method; `drop_fn = None` is a trivial drop
    /// (no side effect — `@linear` types whose move-checker proof is
    /// elsewhere, or value classes with no implicit close). The inkwell
    /// backend treats trivial drops as no-ops on the integer spine; real
    /// emission of the `close` call lands when `@resource` types reach
    /// the spine subset.
    Drop {
        place: Place,
        ty: ResolvedTy,
        drop_fn: Option<String>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct CheckedMirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    /// CFG basic blocks, mirroring `RawMirFunction.blocks`. Slice 1 of
    /// the CFG-construction lane carries a single entry block (id 0)
    /// terminated by `Terminator::Return`; Slice 2 widens the surface
    /// to multi-block CFGs once `If` lowering builds Branch + Goto +
    /// join terminators. Every consumer that previously read a single
    /// `block` field iterates `blocks[*]` now — the entry block remains
    /// `blocks[0]`.
    pub blocks: Vec<BasicBlock>,
    pub decisions: Vec<DecisionFact>,
    pub checks: Vec<MirCheck>,
}

/// Per-function legality findings produced by Checked MIR. A
/// `CheckedMirFunction` with any non-`DecisionMapTotal` `MirCheck`
/// is rejected by `hew compile-v05`; no backend artefact is emitted.
///
/// The variants are exhaustive over the v0.5 move/borrow/init/aliasing
/// surface. Variants whose construction surface doesn't yet exist in
/// the spine (`Aliasing`, `GeneratorBorrowAcrossYield`,
/// `ActorSendEscape`) are declared here so subsequent work that adds
/// borrow ops, `Terminator::Yield` construction, and actor-send
/// lowering doesn't have to retrofit the enum. They cannot be
/// constructed by passes today because the IR has no surface for
/// them; the passes that target them are no-ops on the current spine.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirCheck {
    /// A binding is used before any initialising `Bind` for it appears
    /// in the function's statement stream.
    InitialisedBeforeUse {
        binding: BindingId,
        name: String,
        use_site: SiteId,
    },
    /// A non-`BitCopy` binding was consumed and then read again. The
    /// payload carries the consume site and the offending use site so
    /// the diagnostic surface can point at both ends of the bug.
    UseAfterConsume {
        binding: BindingId,
        name: String,
        consumed_at: SiteId,
        used_at: SiteId,
    },
    /// A shared and mutable borrow of the same place are simultaneously
    /// live, violating read-shared XOR mutate-unique. Declared variant;
    /// no construction surface in the v0.5 integer spine — borrow ops
    /// will populate it once they land.
    Aliasing {
        conflicting_borrows: Vec<(SiteId, BorrowKind)>,
    },
    /// A borrow is live across a generator yield point. Declared
    /// variant; `Terminator::Yield` exists as the suspension site
    /// shape, but the generator-body construction surface that builds
    /// it isn't in the v0.5 integer spine.
    GeneratorBorrowAcrossYield { place: Place, yield_point: SiteId },
    /// A non-`Send` value escapes across an actor message boundary.
    /// Declared variant; `Terminator::Send` exists as the boundary
    /// shape, but actor lowering that builds it isn't in the v0.5
    /// integer spine.
    ActorSendEscape { place: Place, send_site: SiteId },
    /// A non-`Send` value escapes across an actor ask boundary as the
    /// request payload. Symmetric to `ActorSendEscape`: that variant
    /// checks the fire-and-forget send boundary, this one checks the
    /// request side of an ask boundary. Declared variant;
    /// `Terminator::Ask` exists as the boundary shape, but actor-call
    /// lowering that constructs it isn't in the v0.5 integer spine.
    ActorAskEscape { place: Place, ask_site: SiteId },
    /// Structural invariant on the lowering: every value-producing
    /// `SiteId` must have a `DecisionFact` with a concrete `Strategy`
    /// (not `UnknownBlocked`). Violation indicates a lowering bug, not
    /// a user error — surface as a hard rejection.
    DecisionMapTotal { offending_sites: Vec<SiteId> },
    /// A `@linear` (`ValueClass::Linear`) binding is live at an exit
    /// without being consumed via a declared consuming method. Symmetric
    /// to `UseAfterConsume`: that variant rejects consume-then-use, this
    /// one rejects bind-but-never-consume. The payload carries the
    /// exit site for diagnostic anchoring (which path forgot to commit).
    MustConsume {
        binding: BindingId,
        name: String,
        exit_site: SiteId,
        ty: ResolvedTy,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElaboratedMirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    /// Checker-authority statement stream, retained for compatibility with the
    /// existing `--dump-mir elab` consumers and for snapshot continuity.
    /// Drop-elaboration's authoritative output is `blocks` + `drop_plans` —
    /// once the inkwell emitter consumes those directly, `statements` becomes
    /// pure documentation (council R-C3.1 — staged retirement).
    pub statements: Vec<MirStatement>,
    pub decisions: Vec<DecisionFact>,
    /// Basic-block CFG: drop-elaboration's structural output. One entry per
    /// `BasicBlock` (id-indexed via the block's own `id` field, matching
    /// `RawMirFunction::blocks`). Spine-only functions (no `@resource` /
    /// `@linear` locals) carry a single `Normal` block.
    pub blocks: Vec<ElabBlock>,
    /// Per-`ExitPath` drop plan. One entry per terminator-bearing exit edge
    /// across the function. Spine-only functions carry a single
    /// `ExitPath::Return` with an empty `DropPlan`.
    pub drop_plans: Vec<(ExitPath, DropPlan)>,
    /// Generator state schema. `None` on every non-generator function;
    /// the field is a reserved slot for generator state-machine lowering.
    // DROP-TODO: populate when generator construction surface lands
    pub coroutine: Option<CoroutineSchema>,
}

/// A basic-block kind. `Normal` blocks carry user-level statements;
/// `Cleanup` blocks run drop plans on panic / cancel / outer-trap edges
/// per HEW-SPEC §3.7.8.4. Cleanup blocks are reachable only via
/// `ExitPath::Panic` / `ExitPath::Cancel` predecessors and always
/// flow to a strictly outer cleanup block or to function-trap; this is
/// enforced structurally — cleanup blocks form a tree, never a cycle.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockKind {
    Normal,
    Cleanup,
}

/// Elaborated basic block. Carries the same `id` as the corresponding
/// `RawMirFunction::blocks[id]` for normal blocks; cleanup blocks use
/// fresh ids past the highest normal-block id.
#[derive(Debug, Clone, PartialEq)]
pub struct ElabBlock {
    pub id: u32,
    pub kind: BlockKind,
    /// Drop instructions to fire on entry to this block. Empty for normal
    /// blocks; populated in LIFO declaration order for cleanup blocks.
    pub drops: Vec<ElabDrop>,
    /// Successor block to jump to after firing this block's drops. `None`
    /// indicates the function terminates here (trap / return-from-cleanup).
    pub successor: Option<u32>,
}

/// Exit-path discriminator. One value per outgoing edge a function's
/// CFG can take. Mirrors `Terminator::*` plus the new `Cancel` variant
/// (scope-structural cancellation propagation in `fork{}` blocks per
/// HEW-SPEC §3.7.8.4 "lexical task cancellation"). Generator suspension
/// (`Yield`) and actor send (`Send`) are declared so the elaboration pass
/// is exhaustive; the integer spine never constructs them.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExitPath {
    Return {
        block: u32,
    },
    Goto {
        block: u32,
        target: u32,
    },
    Branch {
        block: u32,
        then_target: u32,
        else_target: u32,
    },
    Call {
        block: u32,
        callee: String,
        next: u32,
    },
    Panic {
        block: u32,
    },
    Cancel {
        block: u32,
    },
    Yield {
        block: u32,
        next: u32,
    },
    Send {
        block: u32,
        actor: String,
        next: u32,
    },
    /// Actor-ask exit. Mirrors `Terminator::Ask`. The `channel` Place
    /// is what the loser-cleanup sequence needs
    /// (`hew_reply_channel_cancel` + `hew_reply_channel_free`); the
    /// `reply_dest` Place is irrelevant in the exit path because the
    /// reply value is only consumed inside the winner body. Declared
    /// so the elaboration pass is exhaustive; the spine never
    /// constructs `Terminator::Ask` today, so this exit is unreachable
    /// in practice until the construction surface lands.
    Ask {
        block: u32,
        actor: Place,
        channel: Place,
        next: u32,
    },
    /// Sealed `select{}` exit. Mirrors `Terminator::Select`; declared
    /// so the elaboration pass is exhaustive. The spine never
    /// constructs this — codegen rejects `Terminator::Select` before
    /// the elaboration pass would observe a `Select` exit at runtime.
    Select {
        block: u32,
        next: u32,
    },
}

/// Ordered drop sequence for a single exit. Drops fire in
/// reverse-declaration (LIFO) order: the latest-bound `@resource`
/// drops first.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct DropPlan {
    pub drops: Vec<ElabDrop>,
}

/// A single elaborated drop op. Either a `@resource` drop calling the
/// type's `close` method (`drop_fn = Some(name)`), or a trivial drop
/// for a value class with no side effect (`drop_fn = None`).
#[derive(Debug, Clone, PartialEq)]
pub struct ElabDrop {
    pub place: Place,
    pub ty: ResolvedTy,
    pub drop_fn: Option<String>,
}

/// Generator state-machine schema. Declared scaffold; constructed in
/// Cluster 4.
#[derive(Debug, Clone, PartialEq)]
pub struct CoroutineSchema {
    /// The single resume-state type carrying the generator's locals
    /// across yield points.
    // PROBE-AMBIGUITY: Cluster 4 fills
    pub state: ResolvedTy,
    /// Every yield site in the generator body, in source order.
    // PROBE-AMBIGUITY: Cluster 4 fills
    pub yield_points: Vec<SiteId>,
    /// Places captured into the state record.
    // PROBE-AMBIGUITY: Cluster 4 fills
    pub captured: Vec<Place>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MirStatement {
    Bind {
        binding: BindingId,
        name: String,
        site: SiteId,
        ty: ResolvedTy,
    },
    Evaluate {
        site: SiteId,
        ty: ResolvedTy,
    },
    Use {
        binding: BindingId,
        name: String,
        site: SiteId,
        ty: ResolvedTy,
        intent: IntentKind,
    },
    Return {
        site: Option<SiteId>,
        ty: ResolvedTy,
    },
    Drop {
        binding: BindingId,
        name: String,
        ty: ResolvedTy,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MirDiagnostic {
    pub kind: MirDiagnosticKind,
    pub note: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirDiagnosticKind {
    UseAfterConsume {
        binding: BindingId,
        name: String,
        consumed_at: SiteId,
        used_at: SiteId,
    },
    /// A binding is read before any initialising `let` for it appears.
    /// Surfaced from `MirCheck::InitialisedBeforeUse` in commit 2.
    InitialisedBeforeUse {
        binding: BindingId,
        name: String,
        use_site: SiteId,
    },
    /// Structural invariant on lowering: a `DecisionFact` carries
    /// `Strategy::UnknownBlocked`. Surfaced from
    /// `MirCheck::DecisionMapTotal`.
    DecisionMapTotal { offending_sites: Vec<SiteId> },
    /// A `@linear` binding reached an exit without being consumed via
    /// a declared consuming method. Symmetric to `UseAfterConsume`.
    /// Surfaced from `MirCheck::MustConsume`.
    MustConsume {
        binding: BindingId,
        name: String,
        exit_site: SiteId,
        ty: ResolvedTy,
    },
    /// D10: a named user type had no known `ValueClass` at the MIR boundary.
    /// Only builtin types are supported in slice 1.
    UnknownType { name: String },
    /// Defense-in-depth: an `HirExprKind::Unsupported` node reached MIR
    /// lowering.  The HIR diagnostic should have stopped the pipeline earlier.
    UnsupportedNode { reason: String },
    /// Cluster 1 spine subset rejection: an expression form (e.g. a call, a
    /// non-integer literal, a control-flow construct) is recognised but not
    /// yet lowered to the backend `Instr` stream. Fail-closed so the emitter
    /// never sees a function body with an uninitialised return slot.
    CutoverUnsupported { construct: String, site: SiteId },
    /// A `BindingRef` could not be resolved to a backend `Place` (typically
    /// a function parameter — Cluster 1's spine does not yet bind incoming
    /// arguments to local slots). Without a Place, the value cannot be
    /// moved into the return slot, so the function would silently emit a
    /// binary with an uninitialised return.
    UnresolvedPlace {
        binding: BindingId,
        name: String,
        site: SiteId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecisionFact {
    pub site: SiteId,
    /// The resolved type of the expression at this decision site.
    pub ty: ResolvedTy,
    pub value_class: ValueClass,
    pub intent: IntentKind,
    pub strategy: Strategy,
    pub why: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Strategy {
    BorrowRead,
    Move,
    CowShare,
    EnsureUnique,
    Materialize,
    ConsumeCall,
    Freeze,
    UnknownBlocked,
}
