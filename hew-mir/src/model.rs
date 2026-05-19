use std::collections::{HashMap, HashSet};

use hew_hir::{BindingId, IntentKind, SiteId, ValueClass};
use hew_types::ResolvedTy;

pub use crate::runtime_symbols::UnknownRuntimeSymbol;

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
    /// Layout descriptors for every named-form `record` declaration in the
    /// module. Populated by `lower_hir_module` from the same `HirItem::Record`
    /// walk that builds the field-order table. Codegen (`hew-codegen-rs`)
    /// consumes this to register LLVM named struct types and resolve the
    /// `ResolvedTy::Named { name, .. }` of record-typed locals to the
    /// corresponding struct layout for alloca / GEP emission.
    ///
    /// Tuple-form records (`record Pair(i64, i64)`) are NOT included here:
    /// their `HirRecordDecl.fields` is empty (the parser keeps positional
    /// fields on the `RecordKind::Tuple` discriminator, which the HIR lowerer
    /// does not promote into `HirField`s). Tuple records construct via
    /// `Expr::Call`, not `StructInit`, so they never produce `RecordInit`
    /// or `RecordFieldLoad` instructions and need no codegen layout entry
    /// in this slice.
    pub record_layouts: Vec<RecordLayout>,
    /// Layout descriptors for every actor declaration in the module. Populated
    /// by `lower_hir_module` from `HirItem::Actor` declarations in source
    /// order. Codegen/runtime dispatch slices consume this to materialise actor
    /// state storage and init-call signatures without re-reading HIR.
    pub actor_layouts: Vec<ActorLayout>,
    /// Layout descriptors for every supervisor declaration in the module.
    /// Populated by `lower_hir_module` from `HirItem::Supervisor` declarations
    /// in source order. Each entry pairs the user-declared supervisor metadata
    /// (strategy, restart budget, window, children) with the synthesized
    /// bootstrap-function symbol whose body spawns and wires the children.
    /// Codegen (S-D) consumes this to emit the per-supervisor registration
    /// table that the runtime supervisor substrate dispatches against.
    pub supervisor_layouts: Vec<SupervisorLayout>,
}

/// Layout descriptor for a named-form `record` declaration. The codegen
/// emitter materialises this as an LLVM named struct type whose body is the
/// field-type list in declaration order. Field-name resolution to the
/// `FieldOffset` ordinal has already been performed by the MIR producer at
/// `RecordInit` / `RecordFieldLoad` construction time, so codegen consumes
/// only the positional type list here.
#[derive(Debug, Clone, PartialEq)]
pub struct RecordLayout {
    /// Record type name. Matches the `name` field on
    /// `ResolvedTy::Named { name, .. }` for a record-typed local.
    pub name: String,
    /// Field types in declaration order. Index `i` corresponds to
    /// `FieldOffset(i)`.
    pub field_tys: Vec<ResolvedTy>,
}

/// Layout descriptor for an `actor` declaration. The state field list follows
/// declaration order; the init parameter list follows source parameter order.
#[derive(Debug, Clone, PartialEq)]
pub struct ActorLayout {
    /// Actor type name.
    pub name: String,
    /// Actor state field names in declaration order.
    pub state_field_names: Vec<String>,
    /// Actor state field types in declaration order.
    pub state_field_tys: Vec<ResolvedTy>,
    /// Actor init parameter names in declaration order. Empty when the actor
    /// has no explicit init block.
    pub init_param_names: Vec<String>,
    /// Actor init parameter types in declaration order. Empty when the actor
    /// has no explicit init block.
    pub init_param_tys: Vec<ResolvedTy>,
    /// Actor init handler symbol. `None` when the actor has no explicit init block.
    pub init_symbol: Option<String>,
    /// `#[on(start)]` handler symbol. `None` when the actor has no start hook.
    pub on_start_symbol: Option<String>,
    /// `#[on(stop)]` handler symbols in lexical declaration order.
    /// Empty when the actor has no stop hooks. Multiple hooks are all run
    /// at terminate time in this order via a synthesised fan-out trampoline.
    pub on_stop_symbols: Vec<String>,
    /// `#[on(crash)]` handler symbol. `None` when the actor has no crash hook.
    pub on_crash_symbol: Option<String>,
    /// Receive handlers in message-type order.
    pub handlers: Vec<ActorHandlerLayout>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActorHandlerLayout {
    pub name: String,
    pub symbol: String,
    pub msg_type: i32,
    pub param_tys: Vec<ResolvedTy>,
    pub return_ty: ResolvedTy,
}

/// Layout descriptor for a `supervisor` declaration.
///
/// Supervisors are spawn-only actor-likes: they carry an execution context
/// (their bootstrap body lowers under `FunctionCallConv::ActorHandler`) and
/// occupy a position in the parent/child tree, but they do not accept open
/// messages. They therefore deliberately have NO `ActorProtocolDescriptor`
/// (Q87) and no per-handler `msg_type` mapping — there are no receive
/// handlers to address. If a future iteration introduces user-facing
/// supervisor messages (for example, programmatic restart APIs that route
/// through a supervisor mailbox), the descriptor must be added at that
/// point; until then, the absence is load-bearing — codegen knows a
/// `SupervisorLayout` carries spawn structure and nothing else.
///
/// The `children` vector is ordered by topological spawn order
/// (`wired_to:` dependencies spawn first), assigned during MIR lowering
/// from the S-A/S-B-validated DAG. Each `SupervisorChildLayout.spawn_order`
/// records the position within this same vector so codegen and the runtime
/// reconstruction can re-derive ordering without re-running Kahn's
/// algorithm.
#[derive(Debug, Clone, PartialEq)]
pub struct SupervisorLayout {
    /// Supervisor type name (e.g. `App`). Matches the `name` field on the
    /// `HirSupervisorDecl` lifted from the parser.
    pub name: String,
    /// Restart strategy (`one_for_one`, `one_for_all`, `rest_for_one`,
    /// `simple_one_for_one`). `None` when the supervisor declaration
    /// omitted an explicit `strategy:` clause — runtime defaults apply at
    /// codegen.
    pub strategy: Option<hew_hir::HirSupervisorStrategy>,
    /// Maximum number of restarts allowed inside `window`. `None` when the
    /// supervisor declaration omitted `max_restarts:` — runtime defaults
    /// apply at codegen.
    pub max_restarts: Option<i64>,
    /// Restart-budget window, retained as the raw parser literal (e.g.
    /// `"60s"`). Codegen parses this to a concrete `Duration` so the
    /// duration-literal lexer remains the single source of truth for
    /// unit interpretation.
    pub window: Option<String>,
    /// Mangled symbol of the bootstrap function whose body spawns and
    /// wires the declared children in topological order. The function
    /// itself is emitted into `IrPipeline.{thir,raw_mir,checked_mir,
    /// elaborated_mir}` like any other `FunctionCallConv::ActorHandler`
    /// function. See `mangle_supervisor_bootstrap`.
    pub bootstrap_symbol: String,
    /// Children in topological spawn order. Dependencies spawn before
    /// dependents; siblings with no dependency relationship preserve
    /// declaration order (Kahn's algorithm queue is FIFO).
    pub children: Vec<SupervisorChildLayout>,
}

/// One child or pool entry on a `SupervisorLayout`. Lifted from
/// `HirSupervisorChild` with the wired-to map preserved verbatim; codegen
/// reads the `actor_name` to resolve the per-child `ActorLayout` for
/// init-arg shape validation and runtime registration.
#[derive(Debug, Clone, PartialEq)]
pub struct SupervisorChildLayout {
    /// Child slot name (e.g. `cache`). Matches the field name used at
    /// `sup.<name>` access sites.
    pub name: String,
    /// Actor type spawned at this slot. `HirSupervisorChild.ty` is a raw
    /// String (no `ResolvedTy` round-trip) so this mirrors the field
    /// verbatim. The bootstrap function's spawn instructions name this
    /// type for the `Instr::SpawnActor.actor_name` field.
    pub actor_name: String,
    /// `with restart: <policy>` clause. `None` when the child declaration
    /// omitted the clause — runtime defaults apply at codegen.
    pub restart_policy: Option<hew_hir::HirRestartPolicy>,
    /// `true` for `pool name: Type`; `false` for `child name: Type`.
    pub is_pool: bool,
    /// Compile-time-assigned slot index within the child's own slot
    /// space. Static children index into the supervisor's static slot
    /// table; pool children index into the dynamic pool slot table.
    /// Both spaces start at 0 and are disjoint.
    pub slot_index: u32,
    /// `wired_to:` declarations preserved verbatim. Each entry maps an
    /// init-param name on this child's actor type to the sibling-child
    /// name whose handle is passed at spawn time. S-A/S-B have validated
    /// key existence, sibling existence, and type compatibility before
    /// this layout is built.
    pub wired_to: std::collections::HashMap<String, String>,
    /// Zero-based topological-spawn-order position within
    /// `SupervisorLayout.children`. Equals the index of this entry in
    /// that vector — duplicated here for codegen sites that pattern-
    /// match on a single child without re-correlating against the
    /// parent layout.
    pub spawn_order: u32,
    /// Mangled symbol of the `#[on(crash)]` handler on this child's actor
    /// type. `None` when the child's actor declares no crash hook. Codegen
    /// (Slice 3) reads this to populate the `on_crash_fn` pointer in the
    /// emitted `HewChildSpec` literal; if `None`, the field is left null.
    pub on_crash_symbol: Option<String>,
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
    pub call_conv: FunctionCallConv,
    /// Declared parameter types in declaration order. `params[i]` is the
    /// `ResolvedTy` of the i-th function parameter. Codegen uses this to
    /// declare the LLVM function signature and to emit the parameter-prologue
    /// (store each `llvm_fn.get_nth_param(i)` into `locals[i]`).
    ///
    /// Invariant: `params.len()` equals the number of initial `locals` entries
    /// that correspond to parameter slots. The lowering pass allocates one
    /// `Place::Local` per parameter at the top of `function_body` — these
    /// occupy `locals[0..params.len()]` — and subsequent body-local
    /// allocations begin at `locals[params.len()]`.
    pub params: Vec<ResolvedTy>,
    /// Type-indexed local registers consumed by the backend-authority `Instr`
    /// stream. `locals[i]` is the `ResolvedTy` of `Place::Local(i as u32)`.
    /// The lowering pass allocates one local per value-producing HIR
    /// expression and per `Let`-introduced binding. Parameters occupy
    /// `locals[0..params.len()]` (see `params` invariant above).
    pub locals: Vec<ResolvedTy>,
    pub blocks: Vec<BasicBlock>,
    pub decisions: Vec<DecisionFact>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FunctionCallConv {
    #[default]
    Default,
    ActorHandler,
    ClosureInvoke,
}

impl FunctionCallConv {
    #[must_use]
    pub fn carries_execution_context(self) -> bool {
        matches!(self, Self::ActorHandler | Self::ClosureInvoke)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ContextState {
    Outside,
    Inside,
    Exited,
    Invalid,
}

impl ContextState {
    fn meet(self, other: Self) -> Self {
        if self == other {
            self
        } else {
            Self::Invalid
        }
    }
}

/// Validate execution-context carrier marker invariants on hand-built or
/// lowered MIR.
///
/// Context-bearing functions must enter exactly at the entry block, exit before
/// every terminal path, and use context-observing instructions only while the
/// marker lattice is inside the context. Contextless functions reject all
/// carrier instructions so the context substrate cannot be smuggled into
/// ordinary code.
#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "CFG marker validation keeps entry/exit/use checks together so diagnostics share one dedupe ledger"
)]
pub fn validate_context_markers(func: &RawMirFunction) -> Vec<MirCheck> {
    let mut findings = Vec::new();
    let mut seen: HashSet<(String, u32, &'static str)> = HashSet::new();
    let push = |findings: &mut Vec<MirCheck>,
                seen: &mut HashSet<(String, u32, &'static str)>,
                block: u32,
                kind: &'static str,
                reason: String| {
        if seen.insert((func.name.clone(), block, kind)) {
            findings.push(MirCheck::ContextBoundaryViolation {
                function: func.name.clone(),
                block,
                kind,
                reason,
            });
        }
    };

    if !func.call_conv.carries_execution_context() {
        for block in &func.blocks {
            for instr in &block.instructions {
                if matches!(
                    instr,
                    Instr::EnterContext
                        | Instr::ExitContext
                        | Instr::CheckCancellation
                        | Instr::ContextField { .. }
                ) {
                    push(
                        &mut findings,
                        &mut seen,
                        block.id,
                        "context-marker-outside-handler",
                            "execution-context carrier instructions are only legal in context-bearing functions"
                            .to_string(),
                    );
                }
            }
        }
        return findings;
    }

    let Some(entry) = func.blocks.first() else {
        push(
            &mut findings,
            &mut seen,
            0,
            "missing-enter-context",
            "context-bearing function has no entry block, so EnterContext cannot dominate the body"
                .to_string(),
        );
        return findings;
    };
    if !matches!(entry.instructions.first(), Some(Instr::EnterContext)) {
        push(
            &mut findings,
            &mut seen,
            entry.id,
            "missing-enter-context",
            "context-bearing function entry block must start with EnterContext".to_string(),
        );
    }

    for block in &func.blocks {
        if matches!(
            block.terminator,
            Terminator::Return | Terminator::Trap { .. }
        ) && !matches!(block.instructions.last(), Some(Instr::ExitContext))
        {
            push(
                &mut findings,
                &mut seen,
                block.id,
                "missing-exit-context",
                "context-bearing function terminal block must end with ExitContext before its terminator"
                    .to_string(),
            );
        }
    }

    let by_id: HashMap<u32, &BasicBlock> = func.blocks.iter().map(|b| (b.id, b)).collect();
    let mut entry_states: HashMap<u32, ContextState> = HashMap::new();
    let mut worklist = vec![entry.id];
    entry_states.insert(entry.id, ContextState::Outside);

    while let Some(block_id) = worklist.pop() {
        let Some(block) = by_id.get(&block_id).copied() else {
            continue;
        };
        let mut state = entry_states
            .get(&block_id)
            .copied()
            .unwrap_or(ContextState::Invalid);

        for instr in &block.instructions {
            match instr {
                Instr::EnterContext => {
                    if state == ContextState::Outside {
                        state = ContextState::Inside;
                    } else {
                        push(
                            &mut findings,
                            &mut seen,
                            block.id,
                            "invalid-enter-context",
                            "EnterContext is only legal before the handler context has been entered"
                                .to_string(),
                        );
                        state = ContextState::Invalid;
                    }
                }
                Instr::ExitContext => {
                    if state == ContextState::Inside {
                        state = ContextState::Exited;
                    } else {
                        push(
                            &mut findings,
                            &mut seen,
                            block.id,
                            "invalid-exit-context",
                            "ExitContext is only legal while the handler context is active"
                                .to_string(),
                        );
                        state = ContextState::Invalid;
                    }
                }
                Instr::CheckCancellation
                | Instr::ContextField { .. }
                | Instr::ActorStateFieldLoad { .. }
                | Instr::ActorStateFieldStore { .. }
                    if state != ContextState::Inside =>
                {
                    push(
                        &mut findings,
                        &mut seen,
                        block.id,
                        "context-use-outside-boundary",
                        "context-observing instructions must execute between EnterContext and ExitContext"
                            .to_string(),
                    );
                    state = ContextState::Invalid;
                }
                _ => {}
            }
        }

        if matches!(
            block.terminator,
            Terminator::Return | Terminator::Trap { .. }
        ) && state != ContextState::Exited
        {
            push(
                &mut findings,
                &mut seen,
                block.id,
                "missing-exit-context",
                "context-bearing function terminal path reaches its terminator outside an exited context"
                    .to_string(),
            );
        }

        for succ in block.successors() {
            let next = entry_states
                .get(&succ)
                .copied()
                .map_or(state, |prev| prev.meet(state));
            let changed = entry_states.get(&succ).copied() != Some(next);
            if changed {
                entry_states.insert(succ, next);
                worklist.push(succ);
            }
        }
    }

    findings
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

impl BasicBlock {
    #[must_use]
    pub fn successors(&self) -> Vec<u32> {
        match &self.terminator {
            Terminator::Return | Terminator::Trap { .. } => Vec::new(),
            Terminator::Goto { target } => vec![*target],
            Terminator::Branch {
                then_target,
                else_target,
                ..
            } => vec![*then_target, *else_target],
            Terminator::Call { next, .. }
            | Terminator::Yield { next, .. }
            | Terminator::Send { next, .. }
            | Terminator::Ask { next, .. }
            | Terminator::Select { next, .. } => vec![*next],
        }
    }
}

/// Failure class carried by `Terminator::Trap`. The discriminant lets
/// diagnostics, tests, and runtime-trap handlers distinguish the five
/// trap causes without re-walking the IR or re-inferring from context.
///
/// All five variants are declared here; producer bridges land in later
/// slices:
/// - `IntegerOverflow`     — wired by B-2 (overflow-trap lowering)
/// - `IndexOutOfBounds`    — wired by C-2 (Vec/array OOB formalisation)
/// - `DivideByZero`        — wired by B-5 (divide-by-zero trap)
/// - `SignedMinDivNegOne`  — wired by B-5 (signed-MIN/-1 trap)
/// - `ShiftOutOfRange`     — wired by B-5 (shift-range trap)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TrapKind {
    /// Integer arithmetic overflow on `+`, `-`, or `*`. Fires on signed
    /// and unsigned overflow when the default (non-wrapping) operators
    /// are used. Producer: B-2.
    IntegerOverflow,
    /// Array or `Vec<T>` index out of bounds. Fires when `xs[i]` has
    /// `i >= xs.len()` or `i < 0`. Producer: C-2.
    IndexOutOfBounds,
    /// Integer division by zero. Fires when the divisor of `/` or `%`
    /// is zero. Producer: B-5.
    DivideByZero,
    /// Signed integer division of the minimum value by -1 (`i64::MIN /
    /// -1`), which would overflow the result width. Producer: B-5.
    SignedMinDivNegOne,
    /// Shift count outside `[0, width)`. Fires when `<<` or `>>` has a
    /// shift amount that is negative or ≥ the operand's bit-width.
    /// Producer: B-5.
    ShiftOutOfRange,
    /// Supervisor child slot is not live (tag 1 = Transient or tag 2 = Dead)
    /// at the time of the field-access lookup. Per LESSONS `fail-closed-not-pretend`
    /// (P0), MIR traps rather than fabricating a null PID. The LLVM exit code
    /// must stay in lock-step with `HEW_TRAP_SUPERVISOR_CHILD_UNAVAILABLE` in
    /// `hew-runtime/src/supervisor.rs`. Producer: S2 (`FieldAccess` intercept).
    SupervisorChildUnavailable,
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
    /// Call into a sibling function by name, optionally store its return value,
    /// then branch to `next`.
    Call {
        callee: String,
        args: Vec<Place>,
        dest: Option<Place>,
        next: u32,
    },
    /// Hard abort: emit `llvm.trap` followed by `unreachable`. The
    /// `kind` discriminant identifies the failure class so diagnostics,
    /// tests, and future runtime-trap handlers can distinguish overflow
    /// from OOB from divide-by-zero without re-walking the IR.
    ///
    /// Construction discipline: producers that wire arithmetic overflow
    /// (sub-area B), OOB indexing (sub-area C), divide-by-zero, and
    /// shift-range traps each emit this terminator with the appropriate
    /// `TrapKind`. No producer exists yet for any variant — this slice
    /// introduces the consumer-side primitive; the per-variant producer
    /// bridges land in slices B-2, B-5, C-2, and C-3 respectively.
    Trap { kind: TrapKind },
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
        msg_type: i32,
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
        msg_type: i32,
        value: Place,
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
    ///
    /// `msg_type` and `value` mirror `Terminator::Ask` so codegen
    /// consumes the same packed-payload shape per arm (slice 3
    /// resolves the method name to its handler `msg_type` and packs
    /// the args via `lower_actor_payload` at producer time, the same
    /// path single-shot ask lowering uses). `method` is kept for
    /// diagnostics and producer-side tests; codegen reads `msg_type`
    /// and `value` only.
    ActorAsk {
        actor: Place,
        method: String,
        args: Vec<Place>,
        msg_type: i32,
        value: Place,
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
///
/// ## M2 substrate variants (declared scaffold)
///
/// `DuplexHandle`, `LambdaActorHandle`, `SendHalf`, and `RecvHalf`
/// are the M2 unified-concurrency substrate's MIR addressing surface.
/// Each carries only a discriminator-pointer to a `Local(N)` so the
/// enum stays `Copy`. The S/R type information lives on the parent
/// local's `ResolvedTy` (`Named { name: "Duplex", args: [S, R] }`).
///
/// The half-handle aliases address direction-isolated ends of a
/// `Duplex<S, R>`'s dual queue: `SendHalf(parent)` is the write-only
/// end of the parent's S-direction; `RecvHalf(parent)` is the
/// read-only end of the parent's R-direction. Dropping a half closes
/// only that direction; the Duplex itself ceases when both halves
/// (or the last unified handle) are gone.
///
/// The HIR currently has no construction surface for `LambdaActor` /
/// `Duplex` (the parser flip lives in slice 1, the HIR-lower for it
/// lands later). These variants exist so the drop-elaboration plan
/// and codegen seam don't have to retrofit `Place` when the
/// construction surface lands. The pattern matches the four other
/// declared-but-never-constructed scaffold variants already in this
/// model (`Terminator::Yield`/`Send`, `MirCheck::Aliasing` /
/// `GeneratorBorrowAcrossYield` / `ActorSendEscape`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Place {
    Local(u32),
    ReturnSlot,
    /// A `Duplex<S, R>` handle. The carried `u32` is the `Local(N)`
    /// id whose `locals[N]` is `ResolvedTy::Named { name: "Duplex",
    /// args: [S, R] }`. Drop semantics: dropping the last surviving
    /// handle closes both directions (design §7.3).
    DuplexHandle(u32),
    /// A lambda-actor handle. The carried `u32` is the `Local(N)` id
    /// whose `locals[N]` is the lambda-actor's `Duplex<Msg, Reply>`
    /// (the surface call-syntax dispatches through this Duplex).
    /// Drop semantics: stop-on-last-handle-drop with weak-ref body
    /// capture (§5.9 ratification 2).
    LambdaActorHandle(u32),
    /// A named actor handle returned by `spawn Actor(...)`. The carried `u32`
    /// is the backing local whose resolved type is `LocalPid<Actor>`.
    ActorHandle(u32),
    /// Write-only end of a `Duplex<S, R>`'s S-direction queue. The
    /// carried `u32` is the parent Duplex's `Local(N)` id (the same
    /// local that a `DuplexHandle` would address). Drop closes the
    /// S-direction only; the R-direction stays open until the
    /// matching `RecvHalf` (or last surviving `DuplexHandle`) drops.
    SendHalf(u32),
    /// Read-only end of a `Duplex<S, R>`'s R-direction queue. The
    /// carried `u32` is the parent Duplex's `Local(N)` id (the same
    /// local that a `DuplexHandle` would address). Drop closes the
    /// R-direction only; the S-direction stays open until the
    /// matching `SendHalf` (or last surviving `DuplexHandle`) drops.
    RecvHalf(u32),
}

/// Integer comparison predicate. Maps 1:1 to LLVM `IntPredicate`. The
/// signed-ness selector is intentional: Hew's spine treats `i64` as a
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
    /// Unsigned ≥: reinterprets both operands as unsigned. Used by
    /// shift-range checking to catch both negative shift counts (which
    /// become large unsigned values) and counts ≥ bit-width in a single
    /// compare. B-5 wires this; prior slices had no unsigned predicate.
    UnsignedGreaterEq,
}

/// A validated runtime-ABI call payload carried by `Instr::CallRuntimeAbi`.
///
/// Construction is only possible via `RuntimeCall::new`, which enforces that
/// `symbol` is in the `runtime_symbols::M2_RUNTIME_SYMBOLS` allowlist.
/// Direct struct construction is impossible because the fields are private,
/// so the allowlist check cannot be bypassed at any call site — including
/// release builds (LESSONS P0 `boundary-fail-closed`).
///
/// Consumers (codegen, `instr_places`, MIR dump) access fields through the
/// provided getter methods.
#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeCall {
    /// Validated `hew_*` C-ABI symbol name.
    symbol: String,
    /// Argument places in C-ABI order.
    args: Vec<Place>,
    /// Destination place for the return value, or `None` if discarded.
    dest: Option<Place>,
}

impl RuntimeCall {
    /// Construct a validated runtime-ABI call.
    ///
    /// Returns `Err(UnknownRuntimeSymbol)` if `symbol` is not in the
    /// M2 runtime-ABI allowlist — enforcing the allowlist boundary at
    /// construction in all build profiles (LESSONS P0 `boundary-fail-closed`).
    ///
    /// # Errors
    ///
    /// Returns [`UnknownRuntimeSymbol`] when `symbol` is not recognised by
    /// `runtime_symbols::is_known_runtime_symbol`.
    pub fn new(
        symbol: impl Into<String>,
        args: Vec<Place>,
        dest: Option<Place>,
    ) -> Result<Self, UnknownRuntimeSymbol> {
        let symbol = symbol.into();
        if crate::runtime_symbols::is_known_runtime_symbol(&symbol) {
            Ok(RuntimeCall { symbol, args, dest })
        } else {
            Err(UnknownRuntimeSymbol(symbol))
        }
    }

    /// The validated C-ABI symbol name.
    #[must_use]
    pub fn symbol(&self) -> &str {
        &self.symbol
    }

    /// Argument places in C-ABI order.
    #[must_use]
    pub fn args(&self) -> &[Place] {
        &self.args
    }

    /// Destination place for the return value, or `None` if discarded.
    #[must_use]
    pub fn dest(&self) -> Option<Place> {
        self.dest
    }
}

/// Minimal machine-level instruction set for the spine subset (integer
/// literals, integer add, value moves). Each variant maps to a single
/// inkwell builder call in `hew-codegen-rs::llvm`.
///
/// Variants the emitter cannot lower (Drop on a live heap value, anything
/// coroutine-shaped) emit a hard error rather than silently no-op; the
/// per-variant rejection happens at lowering time, not here.
/// Discriminator for the three integer arithmetic operators that B-2
/// wires through the checked-overflow lowering. Carried by
/// `Instr::IntArithChecked` so codegen can select the matching
/// `llvm.{s,u}{add,sub,mul}.with.overflow.iN` intrinsic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntArithOp {
    Add,
    Sub,
    Mul,
}

/// Signedness discriminator for `Instr::IntArithChecked`. Selects the
/// signed-vs-unsigned LLVM with-overflow intrinsic family at codegen
/// time. Producers read this off the operand's `ResolvedTy` (B-1
/// canonicalised operands and the destination to the same width and
/// signedness so a single field is sufficient).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntSignedness {
    Signed,
    Unsigned,
}

/// Width discriminator for float instructions. Carried on every
/// `Instr::FloatLit` and `Instr::Float*` variant so codegen can select
/// the correct LLVM float type (`float` vs `double`) without re-deriving
/// the width from operand locals. Mirrors `IntSignedness` for integers.
///
/// No implicit widening: `f32 + f64` is rejected by the type checker
/// before MIR construction. Same-width operands only reach these
/// variants, so a single `width` field is sufficient — matches the
/// design invariant in `IntArithChecked.signed`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatWidth {
    /// IEEE 754 single-precision (32-bit).
    F32,
    /// IEEE 754 double-precision (64-bit).
    F64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    /// Semantic marker at actor-handler entry. Codegen emits no user-visible
    /// instruction, but validates that the hidden execution-context argument is
    /// bound before any context-dependent carrier op can execute.
    EnterContext,
    /// Semantic marker at actor-handler exit. Bounds context-derived values:
    /// after this marker they may not be read, returned, captured, or otherwise
    /// propagated across the handler boundary.
    ExitContext,
    /// Explicit cancellation observation point. Codegen lowers this through
    /// the same `hew_actor_cooperate` runtime consult used by cooperate-site
    /// injection.
    CheckCancellation,
    /// Load one field from the hidden `*mut HewExecutionContext` actor-handler
    /// argument by stable byte offset. `dest` supplies the expected field type;
    /// codegen validates it against the known execution-context ABI table before
    /// emitting the byte-offset GEP + typed load.
    ContextField { dest: Place, offset: usize },
    /// `dest = const <value>` as i64.
    ConstI64 { dest: Place, value: i64 },
    /// Two's-complement wrapping `dest = lhs + rhs`. No overflow check.
    /// Producers: `&+` operator sugar (B-4); `.wrapping_add()` method
    /// call (B-3, method body lowering). The default `+` operator uses
    /// `Instr::IntArithChecked` (B-2) for trap-on-overflow semantics.
    IntAdd { dest: Place, lhs: Place, rhs: Place },
    /// Two's-complement wrapping `dest = lhs - rhs`. No overflow check.
    /// Producers: `&-` operator sugar (B-4); `.wrapping_sub()` (B-3).
    IntSub { dest: Place, lhs: Place, rhs: Place },
    /// Two's-complement wrapping `dest = lhs * rhs`. No overflow check.
    /// Producers: `&*` operator sugar (B-4); `.wrapping_mul()` (B-3).
    IntMul { dest: Place, lhs: Place, rhs: Place },
    /// Integer division `dest = lhs / rhs` with no implicit trap guard.
    /// Producers that need trap-on-zero and trap-on-signed-MIN/-1 MUST
    /// emit the divisor checks and branch to a `Terminator::Trap` block
    /// BEFORE emitting this instruction (B-5 does this). Direct emission
    /// of `IntDiv` without that guard is a construct-discipline violation
    /// mirroring `IntAdd`/`IntMul`; no runtime check is added here.
    /// `signed` selects `sdiv` vs `udiv`. Unsigned division can never
    /// produce signed-MIN/-1 overflow, but the divisor-zero check is
    /// still required for both signednesses.
    IntDiv {
        signed: IntSignedness,
        dest: Place,
        lhs: Place,
        rhs: Place,
    },
    /// Integer remainder `dest = lhs % rhs` with no implicit trap guard.
    /// Same guard discipline as `IntDiv`: divisor-zero and
    /// signed-MIN/-1 checks must precede this instruction.
    /// `signed` selects `srem` vs `urem`.
    IntRem {
        signed: IntSignedness,
        dest: Place,
        lhs: Place,
        rhs: Place,
    },
    /// Bitwise AND `dest = lhs & rhs`. Well-defined for all bit widths and
    /// signednesses; no traps, no overflow. Operands and dest must share the
    /// same integer type (enforced upstream by the type checker).
    IntBitAnd { dest: Place, lhs: Place, rhs: Place },
    /// Bitwise OR `dest = lhs | rhs`. Same well-defined semantics as
    /// `IntBitAnd`; no traps or overflow conditions.
    IntBitOr { dest: Place, lhs: Place, rhs: Place },
    /// Bitwise XOR `dest = lhs ^ rhs`. Same well-defined semantics as
    /// `IntBitAnd`; no traps or overflow conditions.
    IntBitXor { dest: Place, lhs: Place, rhs: Place },
    /// Left shift `dest = lhs << rhs`. No signedness on the shift
    /// itself (LLVM `shl`). Producers must check `(rhs as unsigned) >=
    /// bit_width(dest)` before emitting this instruction and branch to
    /// a `Terminator::Trap { kind: TrapKind::ShiftOutOfRange }` block
    /// on the out-of-range path (B-5). No implicit guard here.
    IntShl { dest: Place, lhs: Place, rhs: Place },
    /// Right shift `dest = lhs >> rhs`. `signed` selects arithmetic
    /// shift right (`ashr`) vs logical shift right (`lshr`). Same
    /// out-of-range guard discipline as `IntShl`: check-and-trap MUST
    /// precede this instruction.
    IntShr {
        signed: IntSignedness,
        dest: Place,
        lhs: Place,
        rhs: Place,
    },
    /// Checked integer arithmetic with trap-on-overflow. Lowers to
    /// `call {iN, i1} @llvm.{s,u}{add,sub,mul}.with.overflow.iN(lhs, rhs)`
    /// plus two `extractvalue`s: the iN result into `dest` and the i1
    /// overflow flag into `overflow_flag`. The producing block is
    /// terminated with `Terminator::Branch { cond: overflow_flag,
    /// then_target: trap_bb, else_target: cont_bb }` where `trap_bb`
    /// terminates with `Terminator::Trap { kind:
    /// TrapKind::IntegerOverflow }` and `cont_bb` is the continuation.
    ///
    /// Construction discipline: `dest` and both operands MUST share
    /// the same `ResolvedTy` integer width (B-1 mixed-width rejection)
    /// and `signed` MUST agree with the operand type's signedness.
    /// `overflow_flag` is a fresh local typed as `ResolvedTy::Bool`
    /// (i8 in LLVM lowering) — codegen zero-extends the i1 flag into
    /// the i8 slot.
    ///
    /// Why a single variant covering all three ops: codegen disambiguates
    /// by `op` and `signed` to select one of six intrinsics
    /// (`s{add,sub,mul}` × `u{add,sub,mul}`); a per-op variant would
    /// duplicate the surrounding extract-and-branch shape three times.
    /// LESSONS: `boundary-fail-closed` (P0 — default arithmetic is
    /// the boundary; trap-on-overflow is fail-closed for accidental
    /// overflow); `exhaustive-coverage` (every integer width × every op
    /// × every signedness has an explicit lowering arm).
    IntArithChecked {
        op: IntArithOp,
        signed: IntSignedness,
        dest: Place,
        lhs: Place,
        rhs: Place,
        overflow_flag: Place,
    },
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
    /// `dest = (lhs is rhs)` — pointer/handle identity comparison.
    ///
    /// Produced exclusively from `HirExprKind::IdentityCompare`, which the
    /// HIR lowering emits for `Expr::Is` once the checker (D-2) has
    /// validated that both operands are identity-bearing types (actor refs,
    /// `Vec`, `HashMap`, `HashSet`, `bytes`, machine instances, user named
    /// `type` declarations). The result is a boolean (`1` = same identity,
    /// `0` = distinct identities), stored in `dest`.
    ///
    /// Codegen (D-3): for pointer-shaped LLVM values (`ptr` alloca, i.e.
    /// `ResolvedTy::Named { name: "Duplex", .. }` and future heap-backed
    /// types), `ptrtoint` both operands to `i64`, compare with `icmp eq`,
    /// then `zext` the `i1` result to the dest's stored width. For
    /// machine-id integers (encoded as stable `i64` identifiers by the
    /// machine runtime), the `ptrtoint` step is skipped and `icmp eq` is
    /// applied directly to the loaded integer values.
    ///
    /// LESSONS: `checker-authority` (P0) — codegen reads the operand's
    /// `ResolvedTy` to select between the pointer-path and the integer-path.
    /// The identity allowance set is the checker's sole responsibility; MIR
    /// and codegen never re-check which types are allowed.
    IdentityCompare { dest: Place, lhs: Place, rhs: Place },
    /// `dest = <src>` — load `src`, store into `dest`.
    Move { dest: Place, src: Place },
    /// Call into a `hew_*` runtime-ABI entry by name. The carried
    /// `symbol` names a `#[no_mangle] extern "C" fn` exported by
    /// `hew-runtime/` (the M2 substrate set is listed in
    /// `crate::runtime_symbols::M2_RUNTIME_SYMBOLS`). One variant
    /// covers every Duplex / lambda-actor / half-handle runtime
    /// call — codegen disambiguates by the `symbol` string at lower
    /// time. Aligns with the runtime ABI shape: each symbol IS the
    /// authoritative discriminator and the variant carries no
    /// additional structural information beyond the argument
    /// places and the optional destination.
    ///
    /// Construction discipline (LESSONS P0 `boundary-fail-closed`):
    /// producers MUST validate `symbol` against
    /// `crate::runtime_symbols::is_known_runtime_symbol` BEFORE
    /// pushing this instruction. A typo or unrecognised symbol
    /// surfaces as a `MirDiagnostic::CutoverUnsupported` at MIR
    /// construction, never as a silent link-time failure.
    ///
    /// `dest = None` denotes a runtime call whose return type the
    /// substrate models as `Result<(), _>` and which the producer
    /// has decided not to bind into a Place (a discarded `.send()`
    /// result, or a half-handle `.close()` that consumes the
    /// receiver). `dest = Some(place)` writes the runtime call's
    /// return value into `place`; codegen (slice 5) materialises
    /// the `inkwell` call result into the local backing `place`.
    ///
    /// WHY (M2 slice 4.5c): the typecheck→HIR/MIR bridge that maps
    /// `Duplex<S, R>::send(msg)` (a `MethodCallRewrite` side-table
    /// entry produced by `hew-types` slice 4.5b) to this variant
    /// does not yet reach the Rust MIR pipeline (`hew compile`
    /// never invokes the typechecker). The variant lands first so
    /// slice 5 codegen can wire a real `inkwell::BuildCall` arm and
    /// the producer-side bridge work in a follow-up slice does not
    /// have to retrofit `Instr`. WHEN-OBSOLETE: producers in
    /// `hew-mir/src/lower.rs` start emitting this variant once the
    /// bridge lands. WHAT: a single producer arm in `lower_value`
    /// that walks a Call whose callee resolves to a builtin /
    /// rewritten symbol and pushes `Instr::CallRuntimeAbi`.
    /// Call into a `hew_*` runtime-ABI entry by name. The payload is a
    /// [`RuntimeCall`] whose constructor enforces the symbol allowlist at
    /// construction in all build profiles — direct struct construction is
    /// impossible because `RuntimeCall`'s fields are private
    /// (LESSONS P0 `boundary-fail-closed`).
    CallRuntimeAbi(RuntimeCall),
    /// Construct a first-class callable value from a closure invoke shim and
    /// the environment record materialised at the literal site.
    MakeClosure {
        /// Synthetic function symbol whose ABI is `(ctx, env_ptr, user_args...)`.
        fn_symbol: String,
        /// Environment record place. Codegen stores this place's address in
        /// the closure pair; the env layout is registered in `record_layouts`.
        env: Place,
        /// Destination closure-pair value (`{ fn_ptr, env_ptr }`).
        dest: Place,
    },
    /// Load one captured field from a closure invoke shim's environment pointer.
    ClosureEnvFieldLoad {
        /// Local holding the opaque env pointer parameter.
        env: Place,
        /// Named env-record type whose layout defines `field_offset`.
        env_ty: ResolvedTy,
        /// Capture field index in first-use order.
        field_offset: FieldOffset,
        /// Destination place receiving the field value.
        dest: Place,
    },
    ActorStateFieldLoad {
        field_offset: FieldOffset,
        dest: Place,
    },
    ActorStateFieldStore {
        field_offset: FieldOffset,
        src: Place,
    },
    SpawnActor {
        actor_name: String,
        state: Option<Place>,
        init_args: Vec<Place>,
        dest: Place,
    },
    /// Call a first-class callable pair. Codegen loads the function pointer and
    /// environment pointer from `callee`, then emits an indirect call with the
    /// current execution context and environment pointer prepended to `args`.
    CallClosure {
        callee: Place,
        args: Vec<Place>,
        ret_ty: ResolvedTy,
        dest: Option<Place>,
    },
    /// Spawn a no-argument, unit-returning user function as a scope-owned task.
    ///
    /// Codegen synthesises the C-ABI task wrapper (`void (*)(HewTask*)`) and
    /// passes it to `hew_task_spawn_thread`. MIR only constructs this after
    /// validating the fork body is directly observable by cancellation.
    SpawnTaskDirect { task: Place, callee_symbol: String },
    /// Spawn a no-argument, unit-returning closure as a scope-owned task.
    ///
    /// The producer materialises the closure environment record in the parent
    /// frame, then codegen copies it into the task-owned Rc environment before
    /// spawning the worker. The worker wrapper fetches the task env and calls
    /// `fn_symbol(ctx, env_ptr)`, inheriting the parent execution context's
    /// cancellation, supervisor-lineage, and trace lanes.
    SpawnTaskClosure {
        task: Place,
        fn_symbol: String,
        env: Place,
        env_ty: ResolvedTy,
    },
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
    /// `dest = <global_str_ptr>` — emit an LLVM-level global constant for
    /// `bytes` (null-terminated, internal linkage, read-only) and store the
    /// pointer into `dest`. The `dest` local's type is `ResolvedTy::String`,
    /// which codegen maps to an opaque `ptr` (matching the runtime's
    /// `*const c_char` ABI). No runtime call is made: the pointer refers to
    /// data in the compiled binary's read-only data segment, so
    /// `hew_string_drop` safely skips freeing it via its `is_static_string`
    /// guard. This mirrors the C++ codegen's `hew.global_string` →
    /// `llvm.mlir.global` + `llvm.mlir.addressof` pattern (codegen.cpp
    /// `ConstantOpLowering` / `GlobalStringOpLowering`).
    ///
    /// Escape decoding: `bytes` carries the already-decoded UTF-8 byte
    /// sequence from `HirLiteral::String` — the parser's `unescape_string`
    /// function runs at parse time, so MIR sees decoded bytes. No re-decoding
    /// is needed here.
    ///
    /// Embedded NUL: Hew strings are NUL-terminated C strings at the runtime
    /// boundary. A literal with an embedded NUL byte would be truncated
    /// silently at the first NUL by all C-string runtime operations. The
    /// parser does not produce such literals today; this variant makes no
    /// additional guarantee beyond what the runtime's C-string contract implies.
    StringLit {
        /// Decoded UTF-8 bytes of the literal. LLVM global is emitted as
        /// `bytes` + one NUL terminator byte.
        bytes: Vec<u8>,
        dest: Place,
    },
    /// Construct a record value by storing each field into a freshly
    /// allocated destination place. `fields` carries `(offset, src)`
    /// pairs in declaration order; `dest` receives the completed record.
    ///
    /// The `FieldOffset` is the 0-based index of the field within the
    /// record's declared field order — the same ordinal that
    /// `RecordFieldLoad` uses for reads. Codegen (A-7) uses the offset
    /// to select the struct GEP index for each field store.
    ///
    /// Functional-update (`R { x: 1, ..base }`) is desugared by the MIR
    /// producer: for every field absent from the explicit list it emits a
    /// `RecordFieldLoad` from the base place, then includes the loaded
    /// place here as if it were an explicit field. No `base` field is
    /// needed on this Instr — codegen sees only flat store-each-field.
    ///
    /// WHY flat: keeps codegen dumb, makes the checker stream see every
    /// field read from the base (important for use-after-consume), and
    /// leaves the memcpy optimisation to A-7 pattern recognition.
    /// WHEN-OBSOLETE: if A-7 determines a memcpy path is always better
    ///   for large records, it can introduce a `RecordCopy { base, dest }`
    ///   variant and route functional-update through it instead.
    RecordInit {
        /// Resolved type of the constructed record (used by codegen to
        /// look up the LLVM struct type for the alloca).
        ty: ResolvedTy,
        /// `(field_offset, source_place)` pairs in declaration order.
        fields: Vec<(FieldOffset, Place)>,
        /// Destination place that receives the constructed record value.
        dest: Place,
    },
    /// Load a single field from a record value by its declaration-order
    /// offset. `dest` receives the field value.
    ///
    /// Produced from `HirExprKind::FieldAccess { object, field }` after
    /// the MIR producer resolves the field name to its 0-based
    /// `FieldOffset` via the record-field-order table.
    ///
    /// Codegen (A-7) lowers this to a GEP + load on the record's alloca.
    RecordFieldLoad {
        /// The record value to read from.
        record: Place,
        /// 0-based index of the field within the record's declared field order.
        field_offset: FieldOffset,
        /// Destination place that receives the loaded field value.
        dest: Place,
    },
    /// Load a single element from a tuple value by its 0-based positional index.
    ///
    /// Produced from `HirExprKind::TupleIndex { tuple, index }` when the tuple
    /// sub-expression resolves to a regular tuple-typed local (not a
    /// `tuple_decomp` runtime-call proxy). The tuple local is laid out as a
    /// packed LLVM struct (declaration-order positional fields, `packed = false`
    /// for natural alignment); codegen emits `build_struct_gep(field_index) +
    /// build_load`.
    ///
    /// The `tuple_decomp` proxy path (runtime multi-output calls) bypasses this
    /// instruction entirely and recovers the individual `Place`s from the side-
    /// table without emitting any additional instructions. This variant handles
    /// every other `TupleIndex` site.
    ///
    /// Producer: `lower_value`'s `HirExprKind::TupleIndex` arm (general case).
    /// Codegen: GEP at `field_index` into the tuple's struct alloca + load.
    TupleFieldLoad {
        /// The tuple value to read from.
        tuple: Place,
        /// 0-based element index (positional declaration order).
        field_index: u32,
        /// Destination place that receives the loaded element value.
        dest: Place,
    },
    /// `dest = const <float>` stored as a bit pattern.
    ///
    /// `value_bits` is the IEEE 754 bit-pattern of the float constant:
    /// - For `F32`: `(value as f32).to_bits() as u64` (upper 32 bits zero).
    /// - For `F64`: `value.to_bits()`.
    ///
    /// Storing the bit pattern avoids f32/f64 coercion in the MIR model
    /// and lets codegen reconstruct the exact constant via
    /// `f32_type().const_float_from_apfloat` / `f64_type().const_float`.
    ///
    /// Producer: `lower_literal` for `HirLiteral::Float`, width from `expr.ty`.
    FloatLit {
        dest: Place,
        value_bits: u64,
        width: FloatWidth,
    },
    /// `dest = const <char>` stored as its Unicode scalar value.
    ///
    /// Hew `char` is a Unicode scalar value (U+0000 to U+D7FF and U+E000 to
    /// U+10FFFF). The scalar value is stored as a `u32` bit pattern; codegen
    /// maps it to an `i32` constant (matching C's `int32_t` convention for
    /// Unicode code points). The `u32` encoding is total — Rust's `char as u32`
    /// never produces a surrogate or out-of-range value.
    ///
    /// Producer: `lower_literal` for `HirLiteral::Char`, cast via `c as u32`.
    CharLit {
        /// Unicode scalar value of the character constant.
        value: u32,
        dest: Place,
    },
    /// `dest = ()` — a unit value with no data content.
    ///
    /// Unit is zero-sized. The MIR producer emits this variant to give the
    /// dest place a definition point in the instruction stream; codegen may
    /// emit nothing, a zero-size alloca, or an undef constant depending on
    /// whether `dest` is ever read after this point. In practice, unit-typed
    /// bindings are dropped before they reach codegen in well-typed programs,
    /// so the variant is primarily a completeness placeholder.
    ///
    /// NOTE: `HirLiteral::Unit` is currently never produced by the HIR
    /// lowerer (no parser `Literal::Unit` exists; unit expressions reach MIR
    /// via other HIR node kinds). This variant is present for exhaustiveness
    /// so that a future producer arm has a corresponding MIR representation.
    ///
    /// Producer: `lower_literal` for `HirLiteral::Unit`.
    UnitLit { dest: Place },
    /// `dest = const <duration>` stored as nanoseconds in an `i64`.
    ///
    /// Hew duration literals (`100ms`, `5s`, `1h`, `10ns`, etc.) are
    /// resolved to nanoseconds at parse time by `parse_duration_literal`.
    /// The `i64` nanosecond encoding can represent durations from ~−292 years
    /// to ~+292 years, which covers all practical use cases.
    ///
    /// The runtime representation (nanoseconds as `i64`) is consistent with
    /// `HirLiteral::Duration(i64)` and the parser's `Literal::Duration(i64)`,
    /// which both carry nanoseconds. No representation decision is made here;
    /// the upstream has already committed to `i64` nanoseconds.
    ///
    /// Producer: `lower_literal` for `HirLiteral::Duration`, forwarding the
    /// pre-computed nanosecond value directly.
    DurationLit {
        /// Duration value in nanoseconds.
        nanos: i64,
        dest: Place,
    },
    /// IEEE 754 float addition `dest = lhs + fadd rhs`. No overflow trap —
    /// out-of-range results produce `+inf`/`-inf` per IEEE 754 §6.1.
    FloatAdd {
        dest: Place,
        lhs: Place,
        rhs: Place,
        width: FloatWidth,
    },
    /// IEEE 754 float subtraction `dest = lhs - rhs` (`fsub`).
    FloatSub {
        dest: Place,
        lhs: Place,
        rhs: Place,
        width: FloatWidth,
    },
    /// IEEE 754 float multiplication `dest = lhs * rhs` (`fmul`).
    FloatMul {
        dest: Place,
        lhs: Place,
        rhs: Place,
        width: FloatWidth,
    },
    /// IEEE 754 float division `dest = lhs / rhs` (`fdiv`).
    ///
    /// Division by zero yields `+inf`, `-inf`, or `NaN` per IEEE 754 §7.3 —
    /// there is no runtime trap. Producers MUST NOT add a divisor-zero check
    /// (contrast with `IntDiv`, which requires one). No trap blocks are emitted.
    FloatDiv {
        dest: Place,
        lhs: Place,
        rhs: Place,
        width: FloatWidth,
    },
    /// IEEE 754 float remainder `dest = lhs % rhs` (`frem`, equivalent to
    /// C99 `fmod`). IEEE 754 semantics: `frem(x, 0)` → `NaN`; no trap.
    FloatRem {
        dest: Place,
        lhs: Place,
        rhs: Place,
        width: FloatWidth,
    },
    /// Construct a `dyn Trait` fat pointer from a concrete value.
    ///
    /// Produced from `HirExprKind::CoerceToDynTrait` at every accepted
    /// `T → dyn Trait` coercion site (the checker's
    /// `TypeCheckOutput::dyn_trait_coercions` side table — see
    /// `hew-types/src/check/coerce.rs::try_record_dyn_trait_coercion`).
    /// Codegen (TO-4) lowers this to a `(data_ptr, vtable_ptr)` pair
    /// where `vtable_ptr` references the LLVM private constant for the
    /// `(trait_name, concrete_type)` pair: the value is stack-allocated /
    /// pointer-taken into `data_ptr` and the vtable symbol is materialised
    /// from the codegen-side `(Trait, ImplType)` registry.
    ///
    /// The carried `method_table` is the checker-authoritative
    /// `(trait_method_name, impl_fn_key)` resolution: codegen emits one
    /// function-pointer slot per entry, in declaration order, starting at
    /// vtable slot 3 (after the runtime-fixed prefix triple
    /// `drop_in_place`/`size_of`/`align_of`). For multi-bound coercions the
    /// method names are prefixed by their originating trait (`Trait::method`).
    ///
    /// LESSONS: `checker-authority` (P0) — the trait/concrete/method
    /// resolution lives entirely in the checker side table; MIR and codegen
    /// never re-derive impl functions from the type system.
    CoerceToDynTrait {
        /// Source place holding the concrete `Self` value to wrap.
        value: Place,
        /// Destination place that receives the constructed `dyn Trait`
        /// fat pointer.
        dest: Place,
        /// Trait name (or `Trait1+Trait2` for multi-bound coercions) the
        /// fat pointer represents.
        trait_name: String,
        /// Resolved concrete `Self` type at the coercion site. Codegen
        /// keys the vtable static on `(trait_name, concrete_type)`.
        concrete_type: ResolvedTy,
        /// Ordered `(method_name, impl_fn_key)` pairs naming the impl-side
        /// resolution for each trait method. Order matches the trait
        /// declaration order; multi-bound coercions prefix each method
        /// name with `Trait::`.
        method_table: Vec<(String, String)>,
        /// Ordered vtable entries with checker-substituted method signatures.
        vtable_entries: Vec<hew_types::DynVtableEntry>,
    },
    /// Dispatch a method call through a `dyn Trait` fat pointer's vtable.
    ///
    /// Produced from `HirExprKind::CallDynMethod` at every accepted
    /// method-call on a `Ty::TraitObject` receiver (the checker's
    /// `TypeCheckOutput::dyn_trait_method_calls` side table — see
    /// `hew-types/src/check/methods.rs` `TraitObject` arm). Codegen
    /// (TO-4) lowers this to:
    ///
    /// 1. Load `vtable_ptr` from the fat pointer's second word.
    /// 2. GEP to `slot` (the precomputed vtable index).
    /// 3. Load the function pointer at that slot.
    /// 4. Call the function pointer with `fat_pointer.data` as the
    ///    receiver, followed by `args`.
    ///
    /// `slot` is precomputed by the checker (`3 + method_decl_order` for
    /// the originating trait — see `DynMethodCall::slot`) so MIR and
    /// codegen carry no trait-method-order knowledge of their own.
    ///
    /// `dest = None` denotes a discarded return value; `dest = Some(place)`
    /// writes the return into `place`.
    ///
    /// LESSONS: `checker-authority` (P0) — slot index is the checker's
    /// authority, not MIR's; `boundary-fail-closed` (P0) — HIR rejects
    /// a missing side-table entry before MIR sees the call.
    CallTraitMethod {
        /// Fat-pointer place holding the `dyn Trait` receiver
        /// (constructed by `CoerceToDynTrait` or bound from a parameter
        /// of declared type `dyn Trait`).
        fat_pointer: Place,
        /// Destination for the return value, or `None` if discarded.
        dest: Option<Place>,
        /// Trait name from which the method was resolved.
        trait_name: String,
        /// Trait method name being dispatched.
        method_name: String,
        /// Pre-computed vtable slot index (`3 + method_decl_order`).
        slot: u32,
        /// Argument Places in source order (the implicit receiver is
        /// `fat_pointer.data` and is NOT included here).
        args: Vec<Place>,
    },
}

/// 0-based declaration-order index of a field within a `record` type.
///
/// For named records (`record Point { x: i64, y: i64 }`) the offset of
/// `x` is `0` and the offset of `y` is `1`, matching the order in which
/// fields were declared. This ordinal is the number codegen passes to the
/// LLVM GEP `struct_gep` call to address the field's alloca slot.
///
/// For tuple records the field order matches the positional declaration
/// (`record Pair(i64, i64)` → field 0 and field 1), but tuple records
/// use the function-call constructor and are NOT reachable via
/// `HirExprKind::StructInit` — they never produce `RecordInit` or
/// `RecordFieldLoad` instructions. The offset type is shared for
/// symmetry and for future use if tuple destructuring is added.
///
/// WHY u32: matches the existing convention for `Place::Local(u32)` and
/// `BasicBlock::id: u32`. A record with > 4 billion fields is impossible
/// in practice; the cast from `usize` at the construction site is checked
/// via `try_from` so an impossibly large offset would panic at MIR time
/// rather than silently truncate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldOffset(pub u32);

/// Discriminates the two kinds of yield-check site the cooperate-site
/// analysis identifies. Consumed by codegen to select the injection point
/// within the LLVM function.
///
/// - `FunctionEntry`: cooperate call emitted in the function prologue,
///   after alloca slots are set up and before the first user instruction.
///   Present for every non-leaf function. Ensures that calling into a
///   non-trivial function always decrements the reductions counter.
///
/// - `LoopBackEdge`: cooperate call emitted in the back-edge block —
///   the block whose terminator is `Goto { target }` where `target` is
///   an ancestor block in the CFG. Armed but dormant in v0.5 because the
///   current MIR lowering only produces acyclic CFGs (loop lowering is
///   deferred). The field is populated by the analysis when synthetic
///   back-edge CFGs are constructed in tests and fires automatically once
///   loop lowering lands.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CooperateKind {
    /// Inject cooperate at function entry (prologue).
    FunctionEntry,
    /// Inject cooperate at the loop back-edge block (before the `Goto`
    /// terminator that returns control to the loop header).
    LoopBackEdge,
}

/// One cooperate-check site identified by the cooperate-site analysis.
/// Carried by `CheckedMirFunction::cooperate_sites` for codegen to consume.
///
/// `bb_id` identifies the block where the cooperate call must be injected;
/// `kind` tells codegen whether this is a function-entry or loop-back-edge
/// site.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CooperateSite {
    /// Basic-block id in the owning function's `blocks` vec. For
    /// `FunctionEntry`, this is always block 0 (the entry block). For
    /// `LoopBackEdge`, this is the id of the block whose `Goto`
    /// terminator targets an earlier block.
    pub bb_id: u32,
    /// Injection kind — function-entry prologue or loop back-edge.
    pub kind: CooperateKind,
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
    /// Yield-check injection sites identified by the cooperate-site analysis.
    ///
    /// Codegen iterates this vec to decide where to emit
    /// `call @hew_actor_cooperate()`. Empty means the function is a leaf
    /// (< 10 MIR statements, no calls, no loops) or has a yield-equivalent
    /// entry terminator (receive handler) — no cooperate call is emitted.
    ///
    /// WHY here and not on `ElaboratedMirFunction`: cooperate-site injection
    /// is a pure analysis result with no effect on drop elaboration or
    /// type-checker decisions. Placing it on `CheckedMirFunction` lets
    /// codegen read it without threading through the elaboration pass.
    ///
    /// WHEN-OBSOLETE: if a future change decides cooperate injection interacts
    /// with drop elaboration (e.g. a cooperate inside a cleanup block), the
    /// field can be migrated to `ElaboratedMirFunction` at that time.
    pub cooperate_sites: Vec<CooperateSite>,
}

/// Per-function legality findings produced by Checked MIR. A
/// `CheckedMirFunction` with any non-`DecisionMapTotal` `MirCheck`
/// is rejected by `hew compile`; no backend artefact is emitted.
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
    /// Drop-elaboration could not determine the live-set at a `Return`
    /// block — either the meet-lattice produced an ambiguous state
    /// for an M2 substrate handle (`Duplex` / lambda-actor /
    /// half-handle) or the structural invariant ("every drop in the
    /// per-exit plan resolves to a `DuplexHandle` / `LambdaActorHandle`
    /// / `SendHalf` / `RecvHalf` Place that has a recorded
    /// `DropKind`") fails. The elaborator aborts with this finding
    /// rather than emitting a partial drop plan (LESSONS:
    /// boundary-fail-closed, cleanup-all-exits). The payload carries
    /// the offending block id and a short reason so the diagnostic
    /// surface can anchor the rejection.
    DropPlanUndetermined { block: u32, reason: String },
    /// Execution-context carrier invariant failed: actor handlers must bracket
    /// their bodies with EnterContext/ExitContext and ordinary functions must
    /// not contain carrier instructions.
    ContextBoundaryViolation {
        function: String,
        block: u32,
        kind: &'static str,
        reason: String,
    },
    /// A value derived from `Instr::ContextField` crossed an `ExitContext`
    /// boundary by being read or returned after the context had been exited.
    ContextBindingEscapes { place: Place, block: u32 },
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
    /// Closure-capture metadata for every lambda-actor body in this
    /// function. Empty on non-actor functions; one entry per
    /// captured binding per lambda-actor literal. The codegen layer
    /// (slice 5) consumes this to emit the right capture-strength
    /// (strong refcount bump vs weak-handle allocation) so the
    /// runtime's self-binding weak-ref discipline (§5.9
    /// ratification 2) holds. Declared scaffold; HIR construction
    /// surface for lambda-actor capture-set discovery lands later.
    pub lambda_captures: Vec<LambdaCapture>,
}

/// One captured binding inside a lambda-actor body. The
/// `capture_kind` discriminator is the structural fact codegen needs
/// for the runtime to honour the self-binding weak-ref discipline:
/// a `Weak` capture must NOT bump the actor's external strong
/// refcount, so when external handles drop, the actor stops even
/// though the body still references its own binding name.
///
/// See `CaptureKind::Weak` for the §5.9 ratification 2 narrative.
#[derive(Debug, Clone, PartialEq)]
pub struct LambdaCapture {
    /// The lambda-actor handle this capture belongs to. The Place
    /// is the actor's `LambdaActorHandle(N)` — the spawn-site
    /// binding. Codegen uses this to associate captures with the
    /// right actor's body frame.
    pub actor_handle: Place,
    /// The captured binding's id from the enclosing scope.
    pub captured: BindingId,
    /// The captured binding's name (for diagnostics). The name is
    /// load-bearing for the self-ref case: a `Weak` capture whose
    /// name matches the lambda-actor's own let-binding-name is the
    /// recursive-self case (§5.9 ratification 2).
    pub name: String,
    /// Capture-strength discriminator (Strong vs Weak).
    pub capture_kind: CaptureKind,
}

/// Capture-strength selector for a lambda-actor body capture.
///
/// `Strong` is the default for every non-self capture: the captured
/// value's refcount (for `@resource` types) is bumped so the actor
/// body keeps the captured handle alive. `Weak` is the self-binding
/// recursive case (§5.9 ratification 2): the actor's body
/// references its own binding name, but the reference is held
/// weakly so the body does NOT keep the actor alive past external
/// refcount zero. When the last external handle drops, the actor
/// stops; the body's weak self-ref upgrades fail and recursive
/// self-sends surface as `SendError::ActorStopped` (the runtime
/// contract lands in slice 4).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CaptureKind {
    /// Strong capture: bumps the captured value's refcount; the
    /// body keeps the captured handle alive for as long as the
    /// body's own frame lives.
    Strong,
    /// Weak capture: does NOT bump the captured value's refcount.
    /// The body holds a weak handle that upgrades only while the
    /// captured value's strong refcount is non-zero. Used for the
    /// lambda-actor's own self-binding-name to break the
    /// body-keeps-self-alive cycle.
    Weak,
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
///
/// `kind` discriminates the M2 substrate's structural drop semantics:
/// generic `@resource` close, Duplex-handle close-both-directions,
/// half-handle close-one-direction, and lambda-actor stop-on-last.
#[derive(Debug, Clone, PartialEq)]
pub struct ElabDrop {
    pub place: Place,
    pub ty: ResolvedTy,
    pub drop_fn: Option<String>,
    /// Drop-kind discriminator. Distinguishes the structural close
    /// semantics that codegen (slice 5) and runtime (slice 4) need
    /// to honour. Generic `@resource` drops use `DropKind::Resource`
    /// (the existing path); M2-substrate drops use the specialised
    /// variants. Defaults to `Resource` so existing call sites that
    /// only populate the pre-M2 fields stay correct.
    pub kind: DropKind,
}

/// Drop-kind discriminator for `ElabDrop`. Each variant pins a
/// distinct structural close-protocol contract that the runtime
/// (slice 4) and codegen (slice 5) layers must implement.
///
/// The pre-M2 path emits `DropKind::Resource` for every owned
/// `@resource` binding — the existing close-method dispatch through
/// `drop_fn = Some("Type::close")`. The three M2 variants encode the
/// dual-queue Duplex protocol's three drop shapes (design §7.3-§7.4
/// + §5.9 ratification 2):
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DropKind {
    /// Generic `@resource` drop: call the type's `close` method (or
    /// no-op if `drop_fn` is `None`). The pre-M2 default — every
    /// owned `AffineResource` local lowers to this kind.
    Resource,
    /// `Duplex<S, R>` handle drop with close-both-directions. When
    /// the last surviving handle for this Duplex drops, both the
    /// S-direction and R-direction queues close. Codegen emits a
    /// `hew_duplex_close(handle)` runtime call (slice 5); the
    /// runtime's refcount + dual-queue protocol decides whether
    /// this drop is the last-handle case (slice 4).
    DuplexClose,
    /// Half-handle drop that closes one direction of a Duplex's
    /// dual queue. The carried `Direction` selects which queue
    /// closes; the other direction stays open until the matching
    /// half (or the last unified handle) drops. Codegen emits a
    /// `hew_duplex_close_half(handle, direction)` runtime call
    /// (slice 5).
    DuplexHalfClose(Direction),
    /// Lambda-actor stop-on-last-handle-drop. When the external
    /// strong refcount reaches zero, the actor stops; the body's
    /// recursive self-ref is held weakly so it does NOT keep the
    /// actor alive past external refcount zero (§5.9 ratification
    /// 2). Codegen emits a `hew_lambda_actor_release(handle)`
    /// runtime call (slice 5); the runtime decides whether this
    /// drop is the last-handle case (slice 4).
    LambdaActorRelease,
    /// `dyn Trait` fat-pointer drop: dispatch through vtable slot 0
    /// (`drop_in_place`) on the pointer's `data` word, then release the
    /// fat-pointer storage. Codegen (TO-4) emits the GEP-to-slot-0 +
    /// load + call sequence — no runtime helper, matching the inline
    /// dispatch shape of `Instr::CallTraitMethod` (plan §D-6). The
    /// vtable static itself has program lifetime and is never freed.
    TraitObject,
}

/// Direction selector for `DropKind::DuplexHalfClose`. Mirrors the
/// `SendHalf` / `RecvHalf` Place variants: `Send` closes the
/// S-direction queue, `Recv` closes the R-direction queue.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction {
    /// S-direction (send) queue: the write-end the `SendHalf`
    /// addresses.
    Send,
    /// R-direction (recv) queue: the read-end the `RecvHalf`
    /// addresses.
    Recv,
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
    /// A `select{}` arm of a kind the current lane does not lower
    /// (today: `StreamNext` and `TaskAwait`). Distinct from
    /// `UnsupportedNode` so the diagnostic names both the arm kind and
    /// the future lane that will close it; the producer-bridge contract
    /// for MIR's `Terminator::Select` is "only `ActorAsk` + `AfterTimer`
    /// arms emit; everything else fails closed with a named pointer to
    /// the lane that lifts the restriction."
    SelectArmNotImplemented {
        arm_kind: String,
        lane_pointer: String,
        site: SiteId,
    },
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
    /// A HIR-declared closure/lambda-actor capture could not be mapped to a MIR
    /// backend place. Capture analysis is checker/HIR authority; MIR must not
    /// silently drop a capture and emit a smaller environment.
    CannotMaterializeClosureCapture {
        binding: BindingId,
        name: String,
        site: SiteId,
    },
    /// Drop-elaboration aborted because the M2 substrate's per-exit
    /// drop plan could not be determined for a `Return` block. Surfaced
    /// from `MirCheck::DropPlanUndetermined`; the elaborator never
    /// emits a partial drop (fail-closed per LESSONS
    /// `cleanup-all-exits` / `boundary-fail-closed`).
    DropPlanUndetermined { block: u32, reason: String },
    /// Execution-context carrier marker validation failed.
    ContextBoundaryViolation {
        function: String,
        block: u32,
        kind: &'static str,
        reason: String,
    },
    /// A context-derived place escaped past `ExitContext`.
    ContextBindingEscapes { place: Place, block: u32 },
    /// A hand-built or malformed HIR actor body referenced `self.<field>` for a
    /// field that is not declared in the actor's state layout.
    UnknownActorStateField { actor: String, field: String },
    /// Two actor receive handlers, or a handler and an existing function symbol,
    /// resolved to the same emitted MIR symbol.
    ActorHandlerSymbolCollision {
        symbol: String,
        existing: String,
        duplicate: String,
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
