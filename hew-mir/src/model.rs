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
    /// `dest = <src>` — load `src`, store into `dest`.
    Move { dest: Place, src: Place },
    /// Run the drop ritual for `place`. Cluster 1 emits this for every
    /// `AffineResource` local at function exit; the inkwell backend treats
    /// it as a no-op for now (real Drop emission is Cluster 3). The shape
    /// exists so the emitter's `match` is exhaustive without a wildcard.
    Drop { place: Place, ty: ResolvedTy },
}

#[derive(Debug, Clone, PartialEq)]
pub struct CheckedMirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    pub block: BasicBlock,
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
    /// Structural invariant on the lowering: every value-producing
    /// `SiteId` must have a `DecisionFact` with a concrete `Strategy`
    /// (not `UnknownBlocked`). Violation indicates a lowering bug, not
    /// a user error — surface as a hard rejection.
    DecisionMapTotal { offending_sites: Vec<SiteId> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElaboratedMirFunction {
    pub name: String,
    pub return_ty: ResolvedTy,
    pub statements: Vec<MirStatement>,
    pub decisions: Vec<DecisionFact>,
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
