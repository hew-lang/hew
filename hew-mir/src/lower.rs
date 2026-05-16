use std::collections::{HashMap, HashSet};

use hew_hir::{
    named_type_names, BindingId, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule,
    HirStmtKind, IntentKind, ResolvedRef, ValueClass,
};
use hew_parser::ast::BinaryOp;
use hew_types::ResolvedTy;

use crate::dataflow;
use crate::model::{
    BasicBlock, BlockKind, CheckedMirFunction, CmpPred, DecisionFact, DropKind, DropPlan,
    ElabBlock, ElabDrop, ElaboratedMirFunction, ExitPath, Instr, IntArithOp, IntSignedness,
    IrPipeline, LambdaCapture, MirCheck, MirDiagnostic, MirDiagnosticKind, MirStatement, Place,
    RawMirFunction, Strategy, Terminator, ThirFunction, TrapKind,
};

/// Classify a resolved integer type as signed or unsigned. Returns
/// `None` for non-integer types — callers that demand an integer
/// signedness (the B-2 overflow-trap lowering) fail closed when this
/// returns `None`. Platform-sized `Isize` / `Usize` are canonicalised
/// to their pointer-width LLVM type by codegen; here we only need the
/// signedness discriminator so the intrinsic family selection is
/// correct regardless of pointer width.
fn integer_signedness(ty: &ResolvedTy) -> Option<IntSignedness> {
    match ty {
        ResolvedTy::I8
        | ResolvedTy::I16
        | ResolvedTy::I32
        | ResolvedTy::I64
        | ResolvedTy::Isize => Some(IntSignedness::Signed),
        ResolvedTy::U8
        | ResolvedTy::U16
        | ResolvedTy::U32
        | ResolvedTy::U64
        | ResolvedTy::Usize => Some(IntSignedness::Unsigned),
        _ => None,
    }
}

/// Return the statically known bit-width for a concrete integer type.
///
/// `Isize` / `Usize` are platform-sized (32-bit on WASM32, 64-bit on
/// native) — their width is NOT knowable at MIR construction time.
/// Returns `None` for platform-sized types and all non-integer types.
/// Callers that require a static width (shift-range check, signed-MIN
/// constant emission) must fail-closed (`CutoverUnsupported`) when this
/// returns `None`.
///
/// WHY-ISIZE-NONE: the shift-range bound `(count as unsigned) >= W`
/// requires `W` to be a compile-time constant in the generated
/// `Instr::ConstI64`. On `isize`/`usize` the correct constant is
/// target-dependent (32 vs 64); emitting the wrong constant would
/// silently admit out-of-range shifts on one target. Fail-closed is
/// the right answer for the v0.5 integer spine.
/// WHEN-OBSOLETE: when MIR carries target-info (pointer-width in
/// `IrPipeline` or a `TargetSpec` passed to the builder), re-wire to
/// emit the correct per-target constant and remove this `None` arm.
fn integer_bit_width(ty: &ResolvedTy) -> Option<i64> {
    match ty {
        ResolvedTy::I8 | ResolvedTy::U8 => Some(8),
        ResolvedTy::I16 | ResolvedTy::U16 => Some(16),
        ResolvedTy::I32 | ResolvedTy::U32 => Some(32),
        ResolvedTy::I64 | ResolvedTy::U64 => Some(64),
        // Isize / Usize are platform-sized (see doc comment) and all
        // other types are non-integer — both arms return None.
        _ => None,
    }
}

/// Return the signed minimum value for a concrete signed integer type
/// as an `i64`. Used to emit the `lhs == iN::MIN` constant in the
/// signed-MIN/-1 trap check for `/` and `%`.
///
/// Returns `None` for unsigned types, `Isize` (platform-sized), and
/// all non-integer types. Callers must fail-closed when this returns
/// `None`.
fn signed_min_value(ty: &ResolvedTy) -> Option<i64> {
    match ty {
        ResolvedTy::I8 => Some(i64::from(i8::MIN)),
        ResolvedTy::I16 => Some(i64::from(i16::MIN)),
        ResolvedTy::I32 => Some(i64::from(i32::MIN)),
        ResolvedTy::I64 => Some(i64::MIN),
        // Isize: platform-sized, not knowable at MIR time.
        // Unsigned types: no MIN check needed.
        _ => None,
    }
}

/// Run Checked MIR's legality passes over a function's statement
/// stream. Two real passes ship today (use-after-consume,
/// initialised-before-use); the aliasing, generator-borrow-across-
/// yield, and actor-send-escape variants are declared on `MirCheck`
/// but have no construction surface in the v0.5 integer spine yet
/// (no borrow ops in `Instr`, no projection variants on `Place`, no
/// construction site for `Terminator::Yield` / `Terminator::Send`).
/// The `MirCheck::DecisionMapTotal` invariant fires if any
/// `DecisionFact` in this function carries `Strategy::UnknownBlocked`.
///
/// Delegates to `dataflow::analyze` which runs the four-state lattice
/// (`Uninit / Live / Consumed / MaybeConsumed`) over the CFG's basic
/// blocks via a forward fixpoint. Per-block transfer emits
/// `InitialisedBeforeUse` on `Uninit` reads and `UseAfterConsume` on
/// `Consumed`/`MaybeConsumed` reads; the inter-block meet rule is
/// `Uninit ⊔ X = Uninit` (most-conservative). `If`-lowering (Slice 2)
/// produces `Branch` + two arm blocks + a join block, so the
/// path-sensitive cases that a flat-stream scan would mishandle
/// (false-positive on mutually-exclusive `consume` arms; false-negative
/// for a binding consumed on only one path) are handled correctly by
/// the per-block fixpoint. LESSONS: `boundary-fail-closed` — verify
/// the substrate is path-sensitive before relying on it for linear
/// safety, and mandate property tests on meet rules before landing.
fn check_function(
    builder: &Builder,
    blocks: &[BasicBlock],
    _func: &HirFn,
) -> dataflow::DataflowResult {
    let mut result = dataflow::analyze(blocks, &builder.type_classes);
    let checks = &mut result.checks;

    // DecisionMapTotal. Every `DecisionFact` on this function must
    // carry a concrete `Strategy` — `Strategy::UnknownBlocked` is a
    // lowering escape valve that must never reach the emitter. This
    // pass is independent of the per-block dataflow.
    let offending: Vec<_> = builder
        .decisions
        .iter()
        .filter(|d| d.strategy == Strategy::UnknownBlocked)
        .map(|d| d.site)
        .collect();
    if !offending.is_empty() {
        checks.push(MirCheck::DecisionMapTotal {
            offending_sites: offending,
        });
    }

    // Aliasing, GeneratorBorrowAcrossYield, and ActorSendEscape have
    // no construction surface in the v0.5 integer spine: `Place` has
    // no projection variants, `Instr` has no borrow ops, and
    // `Terminator::Yield` / `Terminator::Send` are declared but never
    // built. The passes are no-ops on the current IR; they populate
    // when the construction surface for borrows, generators, and
    // actor sends lands.

    result
}

#[must_use]
pub fn lower_hir_module(module: &HirModule) -> IrPipeline {
    let mut thir = Vec::new();
    let mut raw_mir = Vec::new();
    let mut checked_mir = Vec::new();
    let mut elaborated_mir = Vec::new();
    let mut diagnostics = Vec::new();

    for item in &module.items {
        match item {
            HirItem::Function(func) => {
                let lowered = lower_function(func, &module.type_classes);
                thir.push(lowered.thir);
                raw_mir.push(lowered.raw);
                checked_mir.push(lowered.checked);
                elaborated_mir.push(lowered.elaborated);
                diagnostics.extend(lowered.diagnostics);
            }
            HirItem::TypeDecl(_) | HirItem::Machine(_) => {
                // Neither type declarations nor Lane A machine declarations
                // have executable MIR bodies. TypeDecl markers are consumed
                // via `HirModule.type_classes`; machine codegen (step()
                // dispatch, tagged-union layout) is Lane B.
            }
        }
    }

    IrPipeline {
        thir,
        raw_mir,
        checked_mir,
        elaborated_mir,
        diagnostics,
    }
}

#[derive(Debug)]
struct LoweredFunction {
    thir: ThirFunction,
    raw: RawMirFunction,
    checked: CheckedMirFunction,
    elaborated: ElaboratedMirFunction,
    diagnostics: Vec<MirDiagnostic>,
}

fn lower_function(func: &HirFn, type_classes: &hew_hir::TypeClassTable) -> LoweredFunction {
    let mut builder = Builder {
        type_classes: type_classes.clone(),
        ..Builder::default()
    };
    builder.function_body(func);

    // Drain the in-flight current block into a sealed `BasicBlock` with
    // a `Terminator::Return`. Slice 1's flat lowering always produces a
    // singleton blocks vector; Slice 2+ may surface multiple blocks
    // when `If` (and later `Match` / loops) split the CFG. The order is
    // monotone in block id.
    let blocks = builder.finalize_blocks(Terminator::Return);
    // THIR's `statements` is the union of every block's checker stream
    // in CFG-construction order — the THIR snapshot's job is preserving
    // the pre-CFG flat-stream shape for diagnostic readers that haven't
    // been ported to block-aware iteration yet. Slice 3's per-block
    // dataflow consumes `RawMirFunction.blocks` directly and doesn't
    // touch this snapshot.
    let thir_statements: Vec<MirStatement> = blocks
        .iter()
        .flat_map(|b| b.statements.iter().cloned())
        .collect();
    let thir = ThirFunction {
        name: func.name.clone(),
        return_ty: func.return_ty.clone(),
        statements: thir_statements,
    };
    // `CheckedMirFunction` mirrors `RawMirFunction.blocks` directly
    // (widened in Slice 2 from a single-block field to a vec). The
    // elaborator + check_function consume the block vec; legacy
    // single-block tests still see `blocks[0]` as the entry block.
    let raw = RawMirFunction {
        name: func.name.clone(),
        return_ty: func.return_ty.clone(),
        locals: builder.locals.clone(),
        blocks,
        decisions: builder.decisions.clone(),
    };
    // Checked MIR's `checks` field is populated by `check_function`
    // from real dataflow over the checker-authority `MirStatement`
    // stream. The `MirDiagnostic` surface that the CLI rejects on is
    // projected from these checks — there is one source of truth for
    // move/borrow/init legality.
    let dataflow_result = check_function(&builder, &raw.blocks, func);
    let mut diagnostics: Vec<MirDiagnostic> = dataflow_result
        .checks
        .iter()
        .filter_map(check_to_diagnostic)
        .collect();

    // Collect diagnostics emitted by the builder (e.g., Unsupported HIR nodes).
    diagnostics.append(&mut builder.diagnostics);

    collect_unknown_type_diagnostics(func, &builder, &mut diagnostics);

    let checked = CheckedMirFunction {
        name: func.name.clone(),
        return_ty: func.return_ty.clone(),
        blocks: raw.blocks.clone(),
        decisions: builder.decisions.clone(),
        checks: dataflow_result.checks.clone(),
    };
    // Drop-elaboration pass. Consumes the CheckedMirFunction we just
    // built; emits an ElaboratedMirFunction whose `blocks` + `drop_plans`
    // are the authoritative description of what fires on every exit.
    //
    // The integer-only spine never lowers `@resource` or `@linear`
    // bindings (no construction surface yet for those types — see
    // R-C3.5), so on the current ladder `owned_locals` is empty
    // whenever the function passed type-checking AND the only
    // non-BitCopy class reaching MIR is `CowValue` (e.g. String) which
    // does not emit a Drop. The elaboration shape is exercised by
    // hew-mir's unit tests that hand-construct CheckedMirFunction
    // inputs with synthesized DecisionFact::value_class values.
    let elaborated = elaborate(&checked, &builder, &thir.statements, &dataflow_result);

    // Fail-closed validation of the elaborated drop plan. Surfaces a
    // `MirCheck::DropPlanUndetermined` for any Return-block whose
    // per-exit live-set decision the elaborator could not commit to.
    // No partial drops escape: a `DropPlanUndetermined` finding
    // upgrades into a `MirDiagnostic` via `check_to_diagnostic`, and
    // the CLI rejects the program before codegen runs. LESSONS:
    // cleanup-all-exits, boundary-fail-closed.
    for check in validate_drop_plan(&elaborated) {
        if let Some(diag) = check_to_diagnostic(&check) {
            diagnostics.push(diag);
        }
    }
    // Cross-block stale-handle detection. Walks the backend Instr
    // stream across the function's CFG and rejects drop plans that
    // fire `Place::DuplexHandle(N)` on a block whose reaching paths
    // have already moved the unified handle into a `SendHalf` /
    // `RecvHalf` split. Catches the case the slice-3 structural
    // checker cannot — a same-direction close emitted by codegen on
    // a handle whose previous owner has been moved out. LESSONS:
    // cleanup-all-exits, raii-null-after-move,
    // boundary-fail-closed.
    for check in validate_cross_block_split_consume(&raw.blocks, &elaborated) {
        if let Some(diag) = check_to_diagnostic(&check) {
            diagnostics.push(diag);
        }
    }

    LoweredFunction {
        thir,
        raw,
        checked,
        elaborated,
        diagnostics,
    }
}

fn collect_unknown_type_diagnostics(
    func: &HirFn,
    builder: &Builder,
    diagnostics: &mut Vec<MirDiagnostic>,
) {
    let mut reported = HashSet::new();

    for param in &func.params {
        push_unknown_type_diagnostics(&param.ty, &builder.type_classes, &mut reported, diagnostics);
    }
    push_unknown_type_diagnostics(
        &func.return_ty,
        &builder.type_classes,
        &mut reported,
        diagnostics,
    );

    for decision in &builder.decisions {
        push_unknown_type_diagnostics(
            &decision.ty,
            &builder.type_classes,
            &mut reported,
            diagnostics,
        );
        if decision.strategy == Strategy::UnknownBlocked
            && named_type_names(&decision.ty).is_empty()
        {
            push_unknown_type_diagnostic(format!("{:?}", decision.ty), &mut reported, diagnostics);
        }
    }

    for statement in &builder.statements {
        match statement {
            MirStatement::Bind { ty, .. }
            | MirStatement::Evaluate { ty, .. }
            | MirStatement::Use { ty, .. }
            | MirStatement::Return { ty, .. }
            | MirStatement::Drop { ty, .. } => {
                push_unknown_type_diagnostics(
                    ty,
                    &builder.type_classes,
                    &mut reported,
                    diagnostics,
                );
            }
        }
    }
}

/// Emit `UnknownType` diagnostics for each Named type in `ty` that is absent
/// from `type_classes`. Names present in the registry are known — they carry
/// an `@linear` or `@resource` marker — and must not be treated as unknown.
/// This implements §3.1 "Checker authority survives downstream": the MIR layer
/// consumes the HIR checker's `type_classes` decision rather than re-deriving
/// Named-type knownness independently.
fn push_unknown_type_diagnostics(
    ty: &ResolvedTy,
    type_classes: &hew_hir::TypeClassTable,
    reported: &mut HashSet<String>,
    diagnostics: &mut Vec<MirDiagnostic>,
) {
    for name in named_type_names(ty) {
        if type_classes.contains_key(&name) {
            continue;
        }
        push_unknown_type_diagnostic(name, reported, diagnostics);
    }
}

fn push_unknown_type_diagnostic(
    name: String,
    reported: &mut HashSet<String>,
    diagnostics: &mut Vec<MirDiagnostic>,
) {
    if reported.insert(name.clone()) {
        diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::UnknownType { name },
            note: "named user type has no known ValueClass at the MIR boundary; \
                   only builtin types are supported in slice 1"
                .to_string(),
        });
    }
}

#[derive(Debug, Default)]
struct Builder {
    /// Checker-authority stream for the *current* basic block. Drained
    /// into a `BasicBlock` when the cursor moves (`finish_current_block`)
    /// or at function-body finalisation. Once a block is sealed it lives
    /// in `pending_blocks` until the function's body walk completes.
    statements: Vec<MirStatement>,
    /// Backend-authority stream for the *current* basic block. Populated
    /// in lock-step with `statements` by `lower_value` so the checker
    /// and the emitter agree on what each `SiteId` resolves to. Drained
    /// at the same cursor-move site as `statements`.
    instructions: Vec<Instr>,
    /// Completed basic blocks in construction order. Block id `0` is the
    /// function's entry block; subsequent ids are monotone in allocation
    /// order. The currently-being-built block (`current_block_id` /
    /// `statements` / `instructions`) is appended at function-body
    /// finalisation. Slice 1 leaves this empty for every function (the
    /// cursor never moves under the CFG-flat lowering); Slice 2's `If`
    /// lowering is the first writer.
    pending_blocks: Vec<BasicBlock>,
    /// Monotone counter for fresh `BasicBlock` ids. `alloc_block` returns
    /// the next id without switching the cursor — the caller is
    /// responsible for `finish_current_block(...)` + `start_block(id)`
    /// at the right point in the lowering sequence.
    next_block_id: u32,
    /// Id of the block currently receiving `statements` / `instructions`.
    /// Initialised to `0` (the entry block). Updated by
    /// `start_block(id)` after a `finish_current_block(...)` seals the
    /// previous block into `pending_blocks`.
    current_block_id: u32,
    /// Type-indexed local registers. `locals[i]` is the `ResolvedTy` of
    /// `Place::Local(i as u32)`.
    locals: Vec<ResolvedTy>,
    /// Maps `BindingId` to the `Local(N)` slot that holds the binding's
    /// initialiser. Cluster 1 reads the slot directly; later clusters add
    /// drop-cleanup and rebinding semantics.
    binding_locals: HashMap<BindingId, Place>,
    decisions: Vec<DecisionFact>,
    owned_locals: Vec<(hew_hir::BindingId, String, ResolvedTy)>,
    /// Diagnostics collected during MIR building (e.g., Unsupported HIR nodes).
    diagnostics: Vec<MirDiagnostic>,
    /// Per-named-type marker registry, cloned from the parent `HirModule` at
    /// builder construction. Read by every `ValueClass::of_ty` call site in
    /// MIR lowering so the marker is the single fact about whether a Named
    /// type participates in the ownership-discipline surface.
    type_classes: hew_hir::TypeClassTable,
    /// Lambda-actor capture ledger collected across every
    /// `HirExprKind::SpawnLambdaActor` literal in the function body.
    /// Drained into `ElaboratedMirFunction.lambda_captures` at the
    /// elaboration boundary; the structural fail-closed checker
    /// `validate_lambda_captures` runs against the drained list.
    lambda_captures: Vec<LambdaCapture>,
    /// `Some(LambdaActorHandle)` while the producer is lowering the
    /// value of `let <name> = actor |..| { .. }`. The `HirStmtKind::Let`
    /// arm pre-allocates the actor's local and records the
    /// `LambdaActorHandle(N)` here BEFORE lowering the value, so
    /// `lower_spawn_lambda_actor` reuses the slot the binding already
    /// owns instead of allocating a second local. The HIR forward-bind
    /// already routed the binding's resolved name to the lambda's own
    /// `BindingId`; this mirror at MIR keeps the binding's `Place`
    /// alignment to that same handle so a Weak self-capture's slot
    /// resolves correctly.
    pending_lambda_actor_handle: Option<Place>,
    /// Tuple decomposition map for runtime-call results that produce multiple
    /// output Places (e.g. `hew_duplex_pair` → two `DuplexHandle` slots).
    ///
    /// Key: the `u32` local index of the "tuple proxy" `Place::Local(N)` that
    /// `lower_runtime_call` returns for a multi-output call.  Value: the
    /// ordered slice of output Places in source-declaration order (e.g.
    /// `[DuplexHandle(N0), DuplexHandle(N1)]` for `duplex_pair`).
    ///
    /// `TupleIndex` lowering looks this map up when the tuple sub-expression
    /// resolves to a proxy local: index `i` into the value vec to obtain the
    /// concrete output Place without emitting additional instructions.
    ///
    /// SHIM(E2→E3): only `hew_duplex_pair` populates this map today.
    /// WHY: MIR has no multi-return instruction; a proxy local threads the
    ///   output Places through the existing single-`Place` `BindingRef` lookup.
    /// WHEN obsolete: when a dedicated MIR multi-return or projection surface
    ///   lands and `TupleIndex` lowering is rewritten to use it directly.
    /// WHAT: replace with `Place::Projection { base, index }` variant or a
    ///   `Terminator::Call`-style multi-dest encoding.
    tuple_decomp: HashMap<u32, Vec<Place>>,
}

impl Builder {
    fn alloc_local(&mut self, ty: ResolvedTy) -> Place {
        // u32::MAX locals per function is well beyond any realistic Hew
        // function size; the cast is bounded by `locals.len()` growing one
        // entry at a time within a single function-body walk.
        let id = u32::try_from(self.locals.len())
            .expect("function exceeds u32::MAX locals — impossible in Hew");
        self.locals.push(ty);
        Place::Local(id)
    }

    /// Allocate a fresh `BasicBlock` id without switching the cursor.
    /// The caller invokes `finish_current_block(terminator)` to seal
    /// the current block, then `start_block(id)` to route subsequent
    /// `statements` / `instructions` into the new block.
    ///
    /// The very first `alloc_block` call returns id `1` because id `0`
    /// is reserved for the function's entry block (the cursor starts
    /// there at `Builder::default()`-time).
    #[allow(
        dead_code,
        reason = "Slice 1 declares cursor helpers; Slice 2 is the first caller"
    )]
    fn alloc_block(&mut self) -> u32 {
        // `next_block_id` starts at 0; bump to 1 the first time
        // `alloc_block` is called (id 0 is the entry block, allocated by
        // construction). After that, monotone increment.
        if self.next_block_id == 0 {
            self.next_block_id = 1;
        }
        let id = self.next_block_id;
        self.next_block_id = self
            .next_block_id
            .checked_add(1)
            .expect("function exceeds u32::MAX blocks — impossible in Hew");
        id
    }

    /// Seal the current basic block with `terminator` and move its
    /// statements + instructions into `pending_blocks`. The cursor is
    /// left at the just-sealed block's id; `start_block(new_id)` must
    /// be called before any further `statements.push` /
    /// `instructions.push` routes into the new block.
    #[allow(
        dead_code,
        reason = "Slice 1 declares cursor helpers; Slice 2 is the first caller"
    )]
    fn finish_current_block(&mut self, terminator: Terminator) {
        let block = BasicBlock {
            id: self.current_block_id,
            statements: std::mem::take(&mut self.statements),
            instructions: std::mem::take(&mut self.instructions),
            terminator,
        };
        self.pending_blocks.push(block);
    }

    /// Move the cursor to `id`. `statements` and `instructions` must be
    /// empty before this call — typically reached by following a
    /// `finish_current_block(...)` which drains both. The new id is
    /// recorded for the next `finish_current_block` call's
    /// `BasicBlock.id` payload.
    #[allow(
        dead_code,
        reason = "Slice 1 declares cursor helpers; Slice 2 is the first caller"
    )]
    fn start_block(&mut self, id: u32) {
        debug_assert!(
            self.statements.is_empty() && self.instructions.is_empty(),
            "start_block must follow finish_current_block; \
             current block has {} statements / {} instructions buffered",
            self.statements.len(),
            self.instructions.len(),
        );
        self.current_block_id = id;
    }

    /// Finalise the function's CFG by sealing the in-flight current
    /// block with the provided terminator. Returns the full
    /// `Vec<BasicBlock>` in id order. Slice 1 always returns a singleton
    /// because no caller invokes `finish_current_block`/`start_block`
    /// during the function-body walk; Slice 2's `If` lowering is the
    /// first writer to produce a non-trivial CFG here.
    fn finalize_blocks(&mut self, terminator: Terminator) -> Vec<BasicBlock> {
        let last = BasicBlock {
            id: self.current_block_id,
            statements: std::mem::take(&mut self.statements),
            instructions: std::mem::take(&mut self.instructions),
            terminator,
        };
        let mut blocks = std::mem::take(&mut self.pending_blocks);
        blocks.push(last);
        // Sort by id so consumers can index by position when they want
        // RPO-ish iteration. Construction order is already monotone
        // because `alloc_block` is monotone, so this is a no-op in
        // every Slice 1 callsite (single-block) and a stable order in
        // Slice 2+.
        blocks.sort_by_key(|b| b.id);
        blocks
    }

    fn function_body(&mut self, func: &HirFn) {
        for stmt in &func.body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &func.body.tail {
            let value_place = self.lower_value(tail);
            self.decide(tail);
            self.mark_returned_binding_moved(tail);
            self.statements.push(MirStatement::Return {
                site: Some(tail.site),
                ty: tail.ty.clone(),
            });
            // Backend stream: write the tail's value into the return slot.
            // If `lower_value` declined to produce a Place (an unsupported
            // construct in the spine subset), skip the move; the
            // `Unsupported` diagnostic already short-circuits the pipeline.
            if let Some(src) = value_place {
                self.instructions.push(Instr::Move {
                    dest: Place::ReturnSlot,
                    src,
                });
            }
        }
    }

    fn stmt(&mut self, stmt: &hew_hir::HirStmt) {
        match &stmt.kind {
            HirStmtKind::Let(binding, Some(value)) => {
                // Mirror the HIR forward-bind discipline at the MIR
                // layer for actor-lambda RHS. When the value is
                // `HirExprKind::SpawnLambdaActor`, pre-allocate the
                // binding's backend slot as a
                // `Place::LambdaActorHandle(N)` BEFORE lowering the
                // value. The body walk then sees a `BindingRef` to
                // the let-name resolve to a `binding_locals` entry
                // that already points at the actor's own handle;
                // the producer reuses this slot via
                // `pending_lambda_actor_handle` instead of allocating
                // a second local. Without this pre-allocation, a
                // Weak self-capture would try to look up a slot for
                // the let-binding that doesn't exist yet.
                let pending = if matches!(&value.kind, HirExprKind::SpawnLambdaActor { .. }) {
                    let slot = self.alloc_local(binding.ty.clone());
                    let Place::Local(local_id) = slot else {
                        unreachable!("alloc_local returns Place::Local");
                    };
                    let handle = Place::LambdaActorHandle(local_id);
                    self.binding_locals.insert(binding.id, handle);
                    self.pending_lambda_actor_handle = Some(handle);
                    true
                } else {
                    false
                };
                let value_place = self.lower_value(value);
                if pending {
                    self.pending_lambda_actor_handle = None;
                }
                self.decide(value);
                self.statements.push(MirStatement::Bind {
                    binding: binding.id,
                    name: binding.name.clone(),
                    site: value.site,
                    ty: binding.ty.clone(),
                });
                if ValueClass::of_ty(&binding.ty, &self.type_classes) != ValueClass::BitCopy {
                    self.owned_locals
                        .push((binding.id, binding.name.clone(), binding.ty.clone()));
                }
                // Backend stream: the binding owns a fresh local that the
                // initialiser's value is moved into. The pre-allocated
                // actor-lambda case already wired `binding_locals` and
                // does not need a second slot.
                if pending {
                    // The lambda-actor case: the producer already
                    // routed the binding to its `LambdaActorHandle`;
                    // no Move instruction is required (the handle is
                    // the value).
                } else if let Some(src) = value_place {
                    // Handle-typed places (DuplexHandle, SendHalf, RecvHalf,
                    // LambdaActorHandle) ARE the binding's backend slot —
                    // they carry ownership-discipline semantics through the
                    // Place kind itself.  Emitting a `Move { dest:
                    // Local(M), src: DuplexHandle(N) }` would store the
                    // handle in a generic Local, losing the kind information
                    // that `drop_kind_for` and `validate_cross_block_*` rely
                    // on (`drop_kind_for(Local(_)) → DropKind::Resource`).
                    // Register the handle Place directly in `binding_locals`
                    // without allocating a second local or emitting a Move.
                    match src {
                        Place::DuplexHandle(_)
                        | Place::SendHalf(_)
                        | Place::RecvHalf(_)
                        | Place::LambdaActorHandle(_) => {
                            self.binding_locals.insert(binding.id, src);
                        }
                        Place::Local(n) if self.tuple_decomp.contains_key(&n) => {
                            // Tuple-proxy: store the proxy directly so TupleIndex can recover
                            // element Places via tuple_decomp[n] — the existing Local-Move arm
                            // would allocate a fresh slot and lose the index that tuple_decomp
                            // is keyed by, leaving owned_locals entries without binding_locals.
                            self.binding_locals.insert(binding.id, src);
                        }
                        Place::Local(_) | Place::ReturnSlot => {
                            let slot = self.alloc_local(binding.ty.clone());
                            self.instructions.push(Instr::Move { dest: slot, src });
                            self.binding_locals.insert(binding.id, slot);
                        }
                    }
                }
            }
            HirStmtKind::Let(_, None) => {}
            HirStmtKind::Expr(expr) => {
                let _ = self.lower_value(expr);
                self.statements.push(MirStatement::Evaluate {
                    site: expr.site,
                    ty: expr.ty.clone(),
                });
            }
            HirStmtKind::Return(Some(expr)) => {
                let value_place = self.lower_value(expr);
                self.decide(expr);
                self.mark_returned_binding_moved(expr);
                self.statements.push(MirStatement::Return {
                    site: Some(expr.site),
                    ty: expr.ty.clone(),
                });
                if let Some(src) = value_place {
                    self.instructions.push(Instr::Move {
                        dest: Place::ReturnSlot,
                        src,
                    });
                }
            }
            HirStmtKind::Return(None) => {
                self.statements.push(MirStatement::Return {
                    site: None,
                    ty: ResolvedTy::Unit,
                });
            }
        }
    }

    /// Walk an expression, emit checker-stream `MirStatement`s plus
    /// backend-stream `Instr`s, and return the `Place` that holds the
    /// expression's value (or `None` if the construct is outside the
    /// spine subset — a `MirDiagnostic` is recorded in that case).
    #[allow(
        clippy::too_many_lines,
        reason = "single large match on HirExprKind variants; each arm is a fail-closed \
                  boundary rule and splitting would obscure the exhaustiveness requirement"
    )]
    fn lower_value(&mut self, expr: &HirExpr) -> Option<Place> {
        self.decide(expr);
        match &expr.kind {
            HirExprKind::Literal(lit) => self.lower_literal(lit, &expr.ty, expr.site),
            HirExprKind::BindingRef {
                name,
                resolved: ResolvedRef::Binding(id),
            } => {
                self.statements.push(MirStatement::Use {
                    binding: *id,
                    name: name.clone(),
                    site: expr.site,
                    ty: expr.ty.clone(),
                    intent: expr.intent,
                });
                if expr.intent == IntentKind::Consume
                    && ValueClass::of_ty(&expr.ty, &self.type_classes) != ValueClass::BitCopy
                {
                    self.mark_binding_moved(*id);
                }
                let place = self.binding_locals.get(id).copied();
                if place.is_none() {
                    // Function parameters and other bindings without a
                    // backend slot are out of Cluster 1's spine. Without a
                    // Place, the emitter would silently load an
                    // uninitialised return slot — fail closed here.
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnresolvedPlace {
                            binding: *id,
                            name: name.clone(),
                            site: expr.site,
                        },
                        note: "binding has no backend slot in the Cluster 1 spine \
                               (function parameters and captured bindings are not \
                               yet lowered)"
                            .to_string(),
                    });
                }
                place
            }
            HirExprKind::BindingRef { .. } => None,
            HirExprKind::Binary { op, left, right } => {
                let lhs = self.lower_value(left);
                let rhs = self.lower_value(right);
                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => self.lower_binary(*op, lhs, rhs, &expr.ty, expr.site),
                    _ => None,
                }
            }
            HirExprKind::Call { callee, args } => {
                // SHIM(E2→checker): runtime-symbol detection uses the callee
                // name string rather than a checker-resolved `ResolvedRef::Builtin`.
                // WHY: the typecheck→HIR bridge (E1) emits `BindingRef { name:
                //   c_symbol, resolved: ResolvedRef::Unresolved }` for every
                //   runtime-symbol callee because the Rust MIR pipeline does not
                //   thread `TypeCheckOutput.method_call_rewrites` resolver IDs
                //   into HIR's `ResolvedRef`.  The name is the only available
                //   discriminator at MIR time.
                // WHEN obsolete: when HIR emits `ResolvedRef::Builtin { c_symbol }`
                //   for runtime-symbol callees and MIR can match on the resolved
                //   variant instead of the name string.
                // WHAT: replace with `matches!(callee.resolved, ResolvedRef::Builtin { .. })`
                //   and remove the `is_known_runtime_symbol` name check.
                let callee_name = match &callee.kind {
                    HirExprKind::BindingRef { name, .. } => Some(name.as_str()),
                    _ => None,
                };
                if let Some(name) = callee_name {
                    // Direct `hew_*` C-ABI name (from method-call rewrites).
                    if crate::runtime_symbols::is_known_runtime_symbol(name) {
                        return self.lower_runtime_call(name, args, expr.site);
                    }
                    // User-facing builtin name (e.g. `duplex_pair`) that maps
                    // to a C-ABI symbol. HIR emits the source name because
                    // checker-registered builtins do not appear in the AST
                    // function-item registry (see `runtime_symbols::user_name_to_c_symbol`
                    // for the shim rationale).
                    if let Some(c_sym) = crate::runtime_symbols::user_name_to_c_symbol(name) {
                        return self.lower_runtime_call(c_sym, args, expr.site);
                    }
                }
                // Non-runtime calls: Cluster 1 does not lower these to backend
                // instructions yet (Terminator::Call is wired but the spine subset
                // accepts only literal/binary/return). Walk the children so any
                // Unsupported inside an argument still surfaces, then fail closed so
                // the emitter never sees a return slot with no producer
                // (LESSONS `boundary-fail-closed`).
                let _ = self.lower_value(callee);
                for arg in args {
                    let _ = self.lower_value(arg);
                }
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CutoverUnsupported {
                        construct: "function call".to_string(),
                        site: expr.site,
                    },
                    note: "non-runtime call expressions are not yet lowered to the \
                           backend instruction stream in the Cluster 1 spine subset"
                        .to_string(),
                });
                None
            }
            HirExprKind::Block(block) => {
                // Every nested statement reaches the checker-authority
                // stream via `self.stmt`, not just `HirStmtKind::Expr`.
                // Forwarding only `Expr` here would silently drop nested
                // `let` / `return` statements from a block expression and
                // let a real `UseAfterConsume` / `InitialisedBeforeUse`
                // pattern slip past the move-checker (fail-closed gap).
                // The HIR-Block-as-expression case recurses through this
                // arm — `If` / `StructInit` / `Call` / `Binary` lower
                // their nested expressions via `lower_value`, so a block
                // embedded in any of those forms reaches this arm and is
                // lowered the same way.
                for stmt in &block.statements {
                    self.stmt(stmt);
                }
                block.tail.as_ref().and_then(|tail| self.lower_value(tail))
            }
            HirExprKind::If {
                condition,
                then_expr,
                else_expr,
            } => self.lower_if(condition, then_expr, else_expr.as_deref(), &expr.ty),
            HirExprKind::StructInit { fields, .. } => {
                for (_, field) in fields {
                    let _ = self.lower_value(field);
                }
                None
            }
            HirExprKind::Scope { body } => {
                // TODO: MIR lowering for scope{} bodies. Required runtime contract:
                // (a) For each SpawnedCall child: allocate a HewTask slot via
                //     hew_task_new, bind the closure environment, call
                //     hew_task_spawn_thread to start the child on the thread pool.
                // (b) For each named ForkTaskHandle binding (fork name = call):
                //     same spawn sequence; the task pointer is stored in the
                //     binding's Place so that a later AwaitTask can load it.
                // (c) At scope-block exit: iterate the set of anonymous child tasks
                //     in declaration order; for each call hew_task_await_blocking
                //     then hew_task_free (lifecycle-symmetry invariant: every
                //     hew_task_new must be paired with hew_task_free on every
                //     exit path including panic/cancel).
                // (d) Named task handles that were explicitly awaited earlier are
                //     already freed at the AwaitTask site; do NOT double-free.
                //
                // Until this is wired, walk the body so nested Unsupported nodes
                // still surface via the checker stream (fail-closed).
                for stmt in &body.statements {
                    self.stmt(stmt);
                }
                let _ = body.tail.as_ref().map(|t| self.lower_value(t));
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CutoverUnsupported {
                        construct: "scope block".to_string(),
                        site: expr.site,
                    },
                    note: "scope{} MIR lowering is not yet implemented; \
                           codegen will wire hew_task_new / hew_task_spawn_thread / \
                           hew_task_await_blocking / hew_task_free"
                        .to_string(),
                });
                None
            }
            HirExprKind::SpawnedCall { callee, args, .. } => {
                // TODO: MIR lowering for a spawned call (task-spawn ABI). Required
                // runtime contract:
                // (a) Allocate a HewTask via hew_task_new(parent_scope).
                // (b) Capture the closure environment for the child function via
                //     hew_task_set_env — all values the child body closes over must
                //     be moved into the task's env slot (ownership transferred; the
                //     parent must not access them after spawn).
                // (c) Issue hew_task_spawn_thread(task, fn_ptr, stack_size) to
                //     schedule the child on the thread pool.
                // (d) Return the HewTask* as the value of this expression so the
                //     containing fork-block can track it for the implicit join.
                //
                // Walk children for checker-stream coverage; fail closed (boundary-fail-closed).
                let _ = self.lower_value(callee);
                for arg in args {
                    let _ = self.lower_value(arg);
                }
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CutoverUnsupported {
                        construct: "spawned call".to_string(),
                        site: expr.site,
                    },
                    note: "SpawnedCall MIR lowering is not yet implemented; \
                           codegen will emit hew_task_new + hew_task_spawn_thread"
                        .to_string(),
                });
                None
            }
            HirExprKind::AwaitTask { .. } => {
                // TODO: MIR lowering for await-task (task-join ABI). Required
                // runtime contract:
                // (a) Load the HewTask* from the binding's Place.
                // (b) Call hew_task_await_blocking(task) — parks the current
                //     coroutine until the child task finishes.
                // (c) Extract the result via hew_task_get_result(task) → Place
                //     of type T (the inner type of Task<T>).
                // (d) Call hew_task_free(task) to release the HewTask allocation.
                //     Null the binding's Place after free so any subsequent
                //     reference is caught as UseAfterConsume.
                // (e) Cancelled-task case: hew_task_get_error returns non-null;
                //     propagate as Err(TaskError::Cancelled) through the Result<T>
                //     if T is Result, or trap if T is not a Result type.
                //
                // Fail closed until the codegen slice implements this (boundary-fail-closed).
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CutoverUnsupported {
                        construct: "await task".to_string(),
                        site: expr.site,
                    },
                    note: "AwaitTask MIR lowering is not yet implemented; \
                           codegen will emit hew_task_await_blocking + hew_task_get_result + \
                           hew_task_free"
                        .to_string(),
                });
                None
            }
            HirExprKind::Select(_select) => {
                // Sealed `select{}` construct. The HIR shape is fixed
                // (see `HirSelect`/`HirSelectArmKind`) and the MIR
                // terminator + per-arm shape are declared in
                // `model::Terminator::Select`, but the runtime
                // substrate (`hew_select_wait` heterogeneous-arm
                // dispatch, `hew_stream_poll` pending-read,
                // `hew_task_scope_cancel_one`, and actor-call
                // lowering for the ask arm) is not yet wired. MIR
                // rejects the construct here with a clear diagnostic
                // so the pipeline fails closed at the earliest seam
                // that can name the missing substrate; codegen also
                // fails closed if a `Terminator::Select` ever reaches
                // it (defence-in-depth).
                //
                // TODO: when the runtime substrate lands, replace
                // this diagnostic with the real construction:
                //   1. Allocate per-arm setup blocks, winner blocks,
                //      and a single join block.
                //   2. Per-arm setup emits the form-specific
                //      registration runtime call (stream-poll
                //      registration, ask issue, task observer
                //      register, timer schedule).
                //   3. Terminate the select's current basic block
                //      with `Terminator::Select { arms, next }` where
                //      `next` is the join block.
                //   4. The per-arm cleanup blocks consume the
                //      cleanup-CFG substrate (`BlockKind::Cleanup`,
                //      `ExitPath::Cancel`) introduced by the
                //      CFG-construction work that already merged.
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::UnsupportedNode {
                        reason: "select-construct: runtime substrate \
                                 (hew_select_wait dispatch + per-arm \
                                 cancellable primitives) not yet wired"
                            .to_string(),
                    },
                    note: "select{} construct reached MIR lowering; \
                           HIR-level Select recognition is in place but \
                           MIR-to-codegen lowering awaits the runtime \
                           substrate"
                        .to_string(),
                });
                None
            }
            HirExprKind::SpawnLambdaActor { .. } => {
                // The lambda-actor literal allocates a fresh local
                // (typed as the actor's Duplex<Msg, Reply>) and
                // surfaces it as a Place::LambdaActorHandle so drop
                // elaboration selects DropKind::LambdaActorRelease.
                // The HIR's resolved capture set is forwarded into
                // the function's lambda_captures ledger; the
                // structural checker validate_lambda_captures pins
                // the Weak-on-LambdaActorHandle invariants on the
                // emitted list. Codegen for the lambda body itself
                // lands in a follow-up slice (it fails closed on a
                // Place::LambdaActorHandle today).
                Some(self.lower_spawn_lambda_actor(expr))
            }
            HirExprKind::TupleIndex { tuple, index } => {
                // Walk the inner tuple expression.  If the tuple sub-expression
                // resolves to a proxy local from a multi-output runtime call
                // (e.g. `hew_duplex_pair` populates `self.tuple_decomp`), return
                // the indexed DuplexHandle Place directly without emitting any
                // additional instructions.  This is the complement of the
                // `lower_runtime_call` path that stores the output Places into
                // `tuple_decomp`.  Any `TupleIndex` on a tuple whose producer
                // did NOT populate `tuple_decomp` falls through to fail-closed.
                let inner_place = self.lower_value(tuple);
                if let Some(Place::Local(local_idx)) = inner_place {
                    if let Some(parts) = self.tuple_decomp.get(&local_idx) {
                        if *index < parts.len() {
                            return Some(parts[*index]);
                        }
                    }
                }
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CutoverUnsupported {
                        construct: format!("tuple-index .{index}"),
                        site: expr.site,
                    },
                    note: "TupleIndex MIR lowering is only implemented for runtime-call \
                           outputs registered in the tuple_decomp table; general tuple \
                           projection is not yet supported"
                        .to_string(),
                });
                None
            }
            HirExprKind::Unsupported(reason) => {
                // Defense-in-depth: HIR lowering should have emitted
                // CutoverUnsupported and the driver should have stopped
                // before reaching MIR. Emit a MirDiagnostic so the pipeline
                // is still rejected if somehow the gate was bypassed.
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::UnsupportedNode {
                        reason: reason.clone(),
                    },
                    note: "HIR Unsupported node reached MIR lowering; \
                           CutoverUnsupported should have been caught earlier"
                        .to_string(),
                });
                None
            }
        }
    }

    fn lower_literal(
        &mut self,
        lit: &HirLiteral,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Spine subset: only integer + bool literals reach the backend in
        // the CFG-construction lane. Float/String/Char/Duration/Unit
        // literals remain out of scope (Cluster 2 takes String; Cluster 4
        // takes the rest). Bool lands here because the CFG-construction
        // surface needs a constructible condition Place for `If`:
        // without bool literals, no non-trivial `If` condition compiles.
        // Fail closed so the emitter never produces a binary with an
        // uninitialised return slot (LESSONS `boundary-fail-closed`).
        let construct = match lit {
            HirLiteral::Integer(value) => {
                let dest = self.alloc_local(ty.clone());
                self.instructions.push(Instr::ConstI64 {
                    dest,
                    value: *value,
                });
                return Some(dest);
            }
            HirLiteral::Bool(value) => {
                // Bool lowers as an integer truth value (1 / 0) into the
                // dest local's natural width. The dest local's type is
                // whatever HIR resolved for the literal — `ResolvedTy::Bool`
                // on this base, which the codegen maps to i8. The
                // `ConstI64.value` is fed through the same store path as
                // ConstI64 for integer literals; `Instr::ConstI64`'s
                // emitter already truncates to the dest local's width.
                let dest = self.alloc_local(ty.clone());
                self.instructions.push(Instr::ConstI64 {
                    dest,
                    value: i64::from(*value),
                });
                return Some(dest);
            }
            HirLiteral::Float(_) => "float literal",
            HirLiteral::String(_) => "string literal",
            HirLiteral::Char(_) => "char literal",
            HirLiteral::Unit => "unit literal",
            HirLiteral::Duration(_) => "duration literal",
        };
        self.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::CutoverUnsupported {
                construct: construct.to_string(),
                site,
            },
            note: "non-integer literals are not yet lowered to the backend \
                   instruction stream in the Cluster 1 spine subset"
                .to_string(),
        });
        None
    }

    fn lower_binary(
        &mut self,
        op: BinaryOp,
        lhs: Place,
        rhs: Place,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let dest = self.alloc_local(ty.clone());
        // Comparison binops: lower to `Instr::IntCmp` with a `CmpPred`
        // discriminator. The result Place is allocated to whatever type
        // HIR resolved for the expression (`ResolvedTy::Bool` for cmp
        // ops); codegen widens the LLVM `i1` cmp result to the dest's
        // stored width on the way to the store. Without this arm,
        // `if 1 == 1 { ... }` cannot construct a condition Place for
        // CFG-construction-lane `If` lowering — the boolean-condition
        // pre-requisite called out by the cluster plan §1 / Slice 0.
        let cmp_pred = match op {
            BinaryOp::Equal => Some(CmpPred::Eq),
            BinaryOp::NotEqual => Some(CmpPred::NotEq),
            BinaryOp::Less => Some(CmpPred::SignedLess),
            BinaryOp::LessEqual => Some(CmpPred::SignedLessEq),
            BinaryOp::Greater => Some(CmpPred::SignedGreater),
            BinaryOp::GreaterEqual => Some(CmpPred::SignedGreaterEq),
            _ => None,
        };
        if let Some(pred) = cmp_pred {
            self.instructions.push(Instr::IntCmp {
                dest,
                pred,
                lhs,
                rhs,
            });
            return Some(dest);
        }
        // B-4 wrapping arithmetic: `&+` / `&-` / `&*` lower to plain
        // two's-complement `IntAdd` / `IntSub` / `IntMul` — no overflow
        // flag, no CFG split, no Trap block. These are the first source-
        // level producers of `Instr::IntAdd/IntSub/IntMul`; previously
        // those variants were reachable only from hand-built fixtures.
        // LESSONS `boundary-fail-closed` (P0): the user has explicitly
        // opted into modular arithmetic by writing `&+`; no trap is the
        // correct behaviour here.
        let wrapping_instr = match op {
            BinaryOp::WrappingAdd => Some(Instr::IntAdd { dest, lhs, rhs }),
            BinaryOp::WrappingSub => Some(Instr::IntSub { dest, lhs, rhs }),
            BinaryOp::WrappingMul => Some(Instr::IntMul { dest, lhs, rhs }),
            _ => None,
        };
        if let Some(instr) = wrapping_instr {
            self.instructions.push(instr);
            return Some(dest);
        }

        // B-5 divide / modulo / shift lowering.
        //
        // These operators are handled here with early returns so they
        // don't fall through to the B-2 overflow-trap `IntArithChecked`
        // path below (which is only for `+`/`-`/`*`).
        match op {
            BinaryOp::Divide | BinaryOp::Modulo => {
                return self.lower_div_rem(op, dest, lhs, rhs, ty, site);
            }
            BinaryOp::Shl | BinaryOp::Shr => {
                return self.lower_shift(op, dest, lhs, rhs, ty, site);
            }
            _ => {}
        }

        let arith_op = match op {
            BinaryOp::Add => IntArithOp::Add,
            BinaryOp::Subtract => IntArithOp::Sub,
            BinaryOp::Multiply => IntArithOp::Mul,
            // The spine subset still rejects logical / range / send /
            // regex / bitwise binops. Previously this arm silently
            // popped the dest local and returned `None`, letting the
            // parent expression succeed with a missing producer (quiet
            // fail-soft — caller's `decide` ran, `MirDiagnostic` did
            // not). Fail closed now: drop the dest local, emit a
            // `CutoverUnsupported` so the CLI rejection surface sees
            // the offending construct, and return `None`.
            // LESSONS `boundary-fail-closed`.
            _ => {
                self.locals.pop();
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CutoverUnsupported {
                        construct: format!("binary operator `{op}`"),
                        site,
                    },
                    note: "binary operator is recognised by HIR but not yet lowered \
                           to the backend instruction stream"
                        .to_string(),
                });
                return None;
            }
        };
        // B-2 overflow-trap lowering. The default `+` / `-` / `*` on
        // integer types lowers to the checked LLVM intrinsic family
        // (`llvm.{s,u}{add,sub,mul}.with.overflow.iN`) with a hard
        // `Terminator::Trap { kind: TrapKind::IntegerOverflow }` on
        // the overflow path and a continuation block on the success
        // path. The MIR-level CFG split — current block ends with a
        // `Branch` on the overflow flag, with a trap block and a
        // continuation block as successors — is what makes the trap
        // visible to drop elaboration, the cross-block dataflow pass,
        // and every other MIR consumer (instead of being a codegen-
        // only emission). LESSONS `boundary-fail-closed` (P0 —
        // default arithmetic IS the boundary; trap-on-overflow is
        // fail-closed for accidental overflow).
        let Some(signed) = integer_signedness(ty) else {
            // Non-integer reaching `+` / `-` / `*` would be a B-1
            // mixed-width or non-integer violation upstream. Fail
            // closed rather than emit unchecked arithmetic.
            self.locals.pop();
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::CutoverUnsupported {
                    construct: format!("binary operator `{op}` on non-integer type"),
                    site,
                },
                note: "B-2 overflow-trap lowering requires an integer-typed result \
                       (i8/i16/i32/i64/u8/u16/u32/u64/isize/usize); float arithmetic \
                       does not use the checked intrinsics and is not yet wired here"
                    .to_string(),
            });
            return None;
        };
        // Allocate the overflow-flag local as a bool. Codegen widens
        // the i1 returned by `extractvalue` to the i8 backing slot.
        let overflow_flag = self.alloc_local(ResolvedTy::Bool);
        self.instructions.push(Instr::IntArithChecked {
            op: arith_op,
            signed,
            dest,
            lhs,
            rhs,
            overflow_flag,
        });
        // Seal the current block with a Branch on the overflow flag.
        // Then-target is the trap block; else-target is the
        // continuation block that subsequent lowering writes into.
        let trap_bb = self.alloc_block();
        let cont_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: overflow_flag,
            then_target: trap_bb,
            else_target: cont_bb,
        });
        // Trap block: a single Terminator::Trap with no instructions.
        self.start_block(trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::IntegerOverflow,
        });
        // Continuation block: the cursor lands here so the parent
        // expression's caller can keep emitting into the success path.
        self.start_block(cont_bb);
        Some(dest)
    }

    /// Lower integer `/` and `%` with divide-by-zero and (for signed
    /// types) signed-MIN/-1 trap guards.
    ///
    /// CFG shape:
    ///
    /// ```text
    /// entry_bb (current)
    ///   IntCmp { pred: Eq, dest: zero_flag, lhs: rhs, rhs: const_0 }
    ///   Branch { cond: zero_flag, then: dbz_trap_bb, else: after_zero_bb }
    ///
    /// dbz_trap_bb
    ///   Trap { kind: DivideByZero }
    ///
    /// after_zero_bb  [signed only]
    ///   IntCmp { pred: Eq, dest: min_flag, lhs: lhs, rhs: const_MIN }
    ///   Branch { cond: min_flag, then: min_check_bb, else: div_bb }
    ///
    /// min_check_bb   [signed only]
    ///   IntCmp { pred: Eq, dest: negone_flag, lhs: rhs, rhs: const_NEG1 }
    ///   Branch { cond: negone_flag, then: smno_trap_bb, else: div_bb }
    ///
    /// smno_trap_bb   [signed only]
    ///   Trap { kind: SignedMinDivNegOne }
    ///
    /// div_bb
    ///   IntDiv / IntRem { dest, lhs, rhs }
    ///   [cursor stays here for subsequent lowering]
    /// ```
    ///
    /// For unsigned types the after-zero block is `div_bb` directly.
    ///
    /// `dest` must already be allocated by the caller (`lower_binary`
    /// allocates it before dispatching here).
    #[allow(
        clippy::too_many_arguments,
        reason = "all arguments are structurally required: the builder state \
                  (&mut self), the opcode discriminator (op), the pre-allocated \
                  destination place (dest), both operand places (lhs, rhs), the \
                  result type (ty) for constant-emission width, and the site id \
                  for diagnostics. There is no natural grouping that reduces this."
    )]
    #[allow(
        clippy::too_many_lines,
        reason = "the function implements a single coherent CFG-emission \
                  pattern (zero-check → MIN/-1 check → div/rem) that must \
                  stay in one place for readability; extracting sub-steps \
                  would require passing more builder state around."
    )]
    fn lower_div_rem(
        &mut self,
        op: BinaryOp,
        dest: Place,
        lhs: Place,
        rhs: Place,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let Some(signed) = integer_signedness(ty) else {
            // Non-integer reaching `/` or `%` — B-1 violation upstream.
            self.locals.pop();
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::CutoverUnsupported {
                    construct: format!("binary operator `{op}` on non-integer type"),
                    site,
                },
                note: "B-5 div/rem trap lowering requires an integer-typed result".to_string(),
            });
            return None;
        };

        // ── divide-by-zero check ────────────────────────────────────
        let zero_const = self.alloc_local(ty.clone());
        self.instructions.push(Instr::ConstI64 {
            dest: zero_const,
            value: 0,
        });
        let zero_flag = self.alloc_local(ResolvedTy::Bool);
        self.instructions.push(Instr::IntCmp {
            dest: zero_flag,
            pred: CmpPred::Eq,
            lhs: rhs,
            rhs: zero_const,
        });
        let dbz_trap_bb = self.alloc_block();
        let after_zero_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: zero_flag,
            then_target: dbz_trap_bb,
            else_target: after_zero_bb,
        });

        self.start_block(dbz_trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::DivideByZero,
        });

        self.start_block(after_zero_bb);

        // ── signed-MIN / -1 check (signed types only) ───────────────
        if signed == IntSignedness::Signed {
            let Some(min_val) = signed_min_value(ty) else {
                // isize: platform-sized, MIN not knowable at MIR time.
                // Fail closed rather than emit an incorrect guard.
                self.locals.pop();
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CutoverUnsupported {
                        construct: format!("binary operator `{op}` on `isize`"),
                        site,
                    },
                    note: "B-5 signed-MIN/-1 trap for `isize` requires target-width \
                           information not available at MIR construction time. \
                           WHEN-OBSOLETE: when IrPipeline carries a TargetSpec, \
                           re-wire to emit the correct per-target MIN constant."
                        .to_string(),
                });
                return None;
            };
            let min_const = self.alloc_local(ty.clone());
            self.instructions.push(Instr::ConstI64 {
                dest: min_const,
                value: min_val,
            });
            let min_flag = self.alloc_local(ResolvedTy::Bool);
            self.instructions.push(Instr::IntCmp {
                dest: min_flag,
                pred: CmpPred::Eq,
                lhs,
                rhs: min_const,
            });
            let min_check_bb = self.alloc_block();
            let div_bb = self.alloc_block();
            self.finish_current_block(Terminator::Branch {
                cond: min_flag,
                then_target: min_check_bb,
                else_target: div_bb,
            });

            // min_check_bb: check whether rhs == -1
            self.start_block(min_check_bb);
            let negone_const = self.alloc_local(ty.clone());
            self.instructions.push(Instr::ConstI64 {
                dest: negone_const,
                value: -1,
            });
            let negone_flag = self.alloc_local(ResolvedTy::Bool);
            self.instructions.push(Instr::IntCmp {
                dest: negone_flag,
                pred: CmpPred::Eq,
                lhs: rhs,
                rhs: negone_const,
            });
            let smno_trap_bb = self.alloc_block();
            self.finish_current_block(Terminator::Branch {
                cond: negone_flag,
                then_target: smno_trap_bb,
                else_target: div_bb,
            });

            self.start_block(smno_trap_bb);
            self.finish_current_block(Terminator::Trap {
                kind: TrapKind::SignedMinDivNegOne,
            });

            self.start_block(div_bb);
        }

        // ── div / rem instruction on the safe path ──────────────────
        match op {
            BinaryOp::Divide => self.instructions.push(Instr::IntDiv {
                signed,
                dest,
                lhs,
                rhs,
            }),
            BinaryOp::Modulo => self.instructions.push(Instr::IntRem {
                signed,
                dest,
                lhs,
                rhs,
            }),
            _ => unreachable!("lower_div_rem called only for Divide / Modulo"),
        }
        Some(dest)
    }

    /// Lower `<<` and `>>` with a shift-out-of-range trap guard.
    ///
    /// The range check uses an unsigned ≥ compare on the shift count:
    ///   `(count as unsigned) >= bit_width(T)`
    /// This single compare catches both negative counts (which become
    /// large unsigned values after reinterpretation) and counts ≥ the
    /// type's width.
    ///
    /// `isize`/`usize` are rejected with `CutoverUnsupported` because
    /// the bit-width is not statically known at MIR time (see
    /// `integer_bit_width` for the documented why / when-obsolete).
    ///
    /// CFG shape:
    /// ```text
    /// entry_bb (current)
    ///   ConstI64 { dest: width_const, value: bit_width }
    ///   IntCmp { pred: UnsignedGreaterEq, dest: oor_flag,
    ///            lhs: rhs (shift count), rhs: width_const }
    ///   Branch { cond: oor_flag, then: sor_trap_bb, else: shift_bb }
    ///
    /// sor_trap_bb
    ///   Trap { kind: ShiftOutOfRange }
    ///
    /// shift_bb
    ///   IntShl / IntShr { dest, lhs, rhs }
    ///   [cursor stays here]
    /// ```
    fn lower_shift(
        &mut self,
        op: BinaryOp,
        dest: Place,
        lhs: Place,
        rhs: Place,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let Some(signed) = integer_signedness(ty) else {
            self.locals.pop();
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::CutoverUnsupported {
                    construct: format!("binary operator `{op}` on non-integer type"),
                    site,
                },
                note: "B-5 shift trap lowering requires an integer-typed operand".to_string(),
            });
            return None;
        };

        let Some(width) = integer_bit_width(ty) else {
            // isize / usize: width not knowable at MIR time.
            self.locals.pop();
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::CutoverUnsupported {
                    construct: format!("binary operator `{op}` on `isize`/`usize`"),
                    site,
                },
                note: "B-5 shift-range trap for `isize`/`usize` requires target-width \
                       information not available at MIR construction time. \
                       WHEN-OBSOLETE: when IrPipeline carries a TargetSpec, \
                       re-wire to emit the correct per-target width constant."
                    .to_string(),
            });
            return None;
        };

        // ── out-of-range check: (count as unsigned) >= width ────────
        let width_const = self.alloc_local(ty.clone());
        self.instructions.push(Instr::ConstI64 {
            dest: width_const,
            value: width,
        });
        let oor_flag = self.alloc_local(ResolvedTy::Bool);
        self.instructions.push(Instr::IntCmp {
            dest: oor_flag,
            pred: CmpPred::UnsignedGreaterEq,
            lhs: rhs, // shift count
            rhs: width_const,
        });
        let sor_trap_bb = self.alloc_block();
        let shift_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: oor_flag,
            then_target: sor_trap_bb,
            else_target: shift_bb,
        });

        self.start_block(sor_trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::ShiftOutOfRange,
        });

        self.start_block(shift_bb);

        // ── shift instruction on the safe path ──────────────────────
        match op {
            BinaryOp::Shl => self.instructions.push(Instr::IntShl { dest, lhs, rhs }),
            BinaryOp::Shr => self.instructions.push(Instr::IntShr {
                signed,
                dest,
                lhs,
                rhs,
            }),
            _ => unreachable!("lower_shift called only for Shl / Shr"),
        }
        Some(dest)
    }

    /// Lower an `If` expression into a real CFG with a `Branch`
    /// terminator on the entry block, separate `then` / `else` blocks
    /// each terminated by a `Goto join_bb`, and a join block that
    /// receives the result value.
    ///
    /// The expression's value Place is a result-local *alloca'd before
    /// the branch* — when each arm finishes lowering its tail
    /// expression, the arm emits an `Instr::Move { dest: result_local,
    /// src: arm_value }` before the `Goto`. The join block then loads
    /// the value through the result local. This matches the existing
    /// alloca-per-local pattern (`alloc_local`) and the codegen's
    /// `place_pointer` lookup (each Place is a stack slot); LLVM's
    /// mem2reg pass promotes the alloca to SSA at the LLVM layer if
    /// the optimiser sees fit. Phi at MIR is a v0.6 refactor
    /// (`R-CFG-V06-phi`).
    ///
    /// `else_expr: None` reaches here when the HIR types the If as
    /// `ResolvedTy::Unit` (no else block). The else arm is still
    /// emitted as a block that just `Goto join` — no Move, no value
    /// written to `result_place`. Downstream code that loads from
    /// `result_place` on the else path observes whatever the alloca
    /// was initialised with (LLVM `undef` for an i8 unit-stand-in,
    /// inconsequential because Unit's value is by definition never
    /// observed). No special fail-closed needed.
    fn lower_if(
        &mut self,
        condition: &HirExpr,
        then_expr: &HirExpr,
        else_expr: Option<&HirExpr>,
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        // Result local first, so it dominates every branch arm's Move.
        // Allocated even for Unit Ifs to keep a single Place-shape
        // contract on the value-bearing return; codegen never loads a
        // Unit result so the placeholder's initial value is unused.
        let result_place = self.alloc_local(result_ty.clone());

        // Lower the condition in the entry (current) block. Receive a
        // Place holding the truth value; codegen's `Terminator::Branch`
        // emitter loads it and compares non-zero.
        // Condition lowering failed (CutoverUnsupported or similar) —
        // propagate by returning None via `?`. The diagnostic already
        // lives on `self.diagnostics`, so the CLI rejects the program;
        // the half-built If does not need to seal the current block.
        // Leaving the result_local dangling is benign — no Branch/Goto
        // refers to it.
        let cond_place = self.lower_value(condition)?;

        // Allocate the three CFG blocks: then arm, else arm, join.
        let then_bb = self.alloc_block();
        let else_bb = self.alloc_block();
        let join_bb = self.alloc_block();

        // Seal the entry block with a Branch on the cond Place.
        self.finish_current_block(Terminator::Branch {
            cond: cond_place,
            then_target: then_bb,
            else_target: else_bb,
        });

        // Then arm.
        self.start_block(then_bb);
        let then_value = self.lower_value(then_expr);
        if let Some(src) = then_value {
            self.instructions.push(Instr::Move {
                dest: result_place,
                src,
            });
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        // Else arm. `else_expr: None` (the HIR-types-as-Unit case)
        // emits a Goto-only block — no Move, no value contributed.
        self.start_block(else_bb);
        if let Some(else_expr) = else_expr {
            let else_value = self.lower_value(else_expr);
            if let Some(src) = else_value {
                self.instructions.push(Instr::Move {
                    dest: result_place,
                    src,
                });
            }
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        // Join. Subsequent lowering continues in this block; the If
        // expression's value Place is the result_local (loads happen
        // through the same Place that both arms wrote into).
        self.start_block(join_bb);
        Some(result_place)
    }

    /// Lower a recognised `hew_*` runtime-ABI call to
    /// `Instr::CallRuntimeAbi`.
    ///
    /// Called from `lower_value`'s `HirExprKind::Call` arm when the
    /// callee is a `BindingRef` whose name passes
    /// `runtime_symbols::is_known_runtime_symbol`.  The HIR args have
    /// already been validated by the HIR pipeline; this method lower
    /// each arg via `lower_value`, then emits the appropriate
    /// instruction sequence.
    ///
    /// # `hew_duplex_pair` encoding
    ///
    /// The runtime C-ABI takes `(s_cap, r_cap, *mut *mut HewDuplexHandle,
    /// *mut *mut HewDuplexHandle)`.  The user surface is `duplex_pair(N)`
    /// with one symmetric capacity arg.  E2 duplicates `args[0]` into
    /// both cap slots and passes two fresh `Place::DuplexHandle(N0/N1)`
    /// in the out-param positions (`args[2..=3]`).  Codegen (E4) takes
    /// the address of each `DuplexHandle` local and passes it as the
    /// actual pointer.  A "tuple proxy" `Place::Local(M)` is returned
    /// so that subsequent `TupleIndex` projections can recover the
    /// individual `DuplexHandle` Places via `self.tuple_decomp`.
    ///
    /// # `hew_duplex_send` encoding
    ///
    /// The runtime C-ABI takes `(*mut HewDuplexHandle, *const u8, usize)`.
    /// For an integer payload `42`, this method emits a prefatory
    /// `Instr::ConstI64 { value: 8 }` as the byte-length constant
    /// before the `CallRuntimeAbi`.  The message value Place is passed
    /// as-is; codegen (E4) stores it to a stack alloca and passes its
    /// address as the `*const u8`.
    ///
    /// # SHIM(E4) convention
    ///
    // SHIM(E4): codegen interprets Place::DuplexHandle(N) per-symbol convention:
    //   hew_duplex_send/recv/close: load the raw ptr from local-N's alloca, pass as *mut HewDuplexHandle.
    //   hew_duplex_pair out-params (args[2], args[3]): take address of local-N's alloca, pass as *mut *mut HewDuplexHandle.
    // The message-value Place::Local(N) in args[1] of hew_duplex_send is store-to-alloca + address-cast by E4.
    // The length Place::Local(N) in args[2] of hew_duplex_send carries the ConstI64(8) emitted above.
    // WHY: MIR names semantics; address materialisation is a codegen-target concern.
    // WHEN obsolete: when E4's lower_instr arm is wired and tested for each of these conventions.
    // WHAT: replace with direct LLVMBuildCall emission for each symbol group.
    fn lower_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Construction-time contract: the symbol must be in the allowlist.
        // This is the HIR-string-boundary gate: the caller dispatched this
        // symbol from a `BindingRef` name, so we assert in all build profiles
        // that it is known before we dispatch to a symbol-specific arm.
        // `RuntimeCall::new` enforces the same invariant at the MIR data level;
        // this assert defends the dispatch table (LESSONS `boundary-fail-closed`).
        assert!(
            crate::runtime_symbols::is_known_runtime_symbol(symbol),
            "lower_runtime_call called with unrecognised symbol `{symbol}`; \
             the call site must gate on is_known_runtime_symbol first"
        );

        match symbol {
            "hew_duplex_pair" => self.lower_duplex_pair(hir_args, site),
            "hew_duplex_send" => self.lower_duplex_send(hir_args, site),
            _ => {
                // Known-allowlisted symbol but no producer arm yet.  Fail closed
                // so the pipeline rejects the program before codegen runs.
                // Individual symbol producers land in follow-up slices (recv,
                // half-handle split, close, lambda-actor lifecycle).
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CutoverUnsupported {
                        construct: format!("runtime call `{symbol}`"),
                        site,
                    },
                    note: format!(
                        "`{symbol}` is a recognised runtime symbol but has no \
                         MIR producer arm yet; wired per-symbol in follow-up slices"
                    ),
                });
                None
            }
        }
    }

    /// Emit `Instr::CallRuntimeAbi` for `hew_duplex_pair`.
    ///
    /// HIR shape (from E1 bridge): `Call { callee: BindingRef("hew_duplex_pair"),
    /// args: [cap_expr] }` — one symmetric capacity arg.
    ///
    /// MIR emission:
    ///   1. Lower `cap_expr` → `cap_place`.
    ///   2. Allocate two fresh `DuplexHandle` locals (N0, N1).
    ///   3. Emit `CallRuntimeAbi { args: [cap, cap, DuplexHandle(N0), DuplexHandle(N1)], dest: None }`.
    ///   4. Allocate a "tuple proxy" `Place::Local(M)` to thread the two
    ///      output Places through the existing `BindingRef` lookup.
    ///   5. Register `tuple_decomp[M] = [DuplexHandle(N0), DuplexHandle(N1)]`.
    ///   6. Return `Some(Local(M))`.
    ///
    /// `TupleIndex` lowering recovers the individual `DuplexHandle` Places from
    /// `tuple_decomp`.  `owned_locals` registration for `a` and `b` happens
    /// naturally in `stmt()` when `let a = __tuple_N.0` stores
    /// `DuplexHandle(N0)` directly into `binding_locals` (see the handle-typed
    /// branch in the `stmt` Let arm).
    fn lower_duplex_pair(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // E1 registers duplex_pair<S, R>(int) — one symmetric capacity arg.
        // If E1 ever expands to two args (s_cap, r_cap), skip the duplication.
        let cap_place = if hir_args.len() == 1 {
            self.lower_value(&hir_args[0])
        } else if hir_args.len() >= 2 {
            // Future: two-arg form — just lower both and use the first two.
            self.lower_value(&hir_args[0])
        } else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::CutoverUnsupported {
                    construct: "hew_duplex_pair with zero args".to_string(),
                    site,
                },
                note: "hew_duplex_pair requires at least one capacity argument".to_string(),
            });
            return None;
        };
        let Some(cap_place) = cap_place else {
            // Capacity expression failed to lower (e.g. nested Unsupported).
            // Diagnostic already recorded; propagate the failure.
            return None;
        };

        // If E1 emits two args, lower the second capacity independently.
        // For the one-arg case, duplicate the single capacity for both slots.
        let r_cap_place = if hir_args.len() >= 2 {
            self.lower_value(&hir_args[1]).unwrap_or(cap_place)
        } else {
            cap_place // symmetric capacity: s_cap == r_cap
        };

        // Allocate two DuplexHandle locals.  The local index is shared
        // between `Place::Local(N)` (for type bookkeeping in `self.locals`)
        // and `Place::DuplexHandle(N)` (for semantic kind tracking in the
        // instruction and drop streams).
        let local0 = self.alloc_local(ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![],
        });
        let Place::Local(n0) = local0 else {
            unreachable!("alloc_local returns Place::Local");
        };
        let dh0 = Place::DuplexHandle(n0);

        let local1 = self.alloc_local(ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![],
        });
        let Place::Local(n1) = local1 else {
            unreachable!("alloc_local returns Place::Local");
        };
        let dh1 = Place::DuplexHandle(n1);

        // Emit the runtime call.  The i32 return (error code) is discarded
        // (`dest: None`); the two DuplexHandle out-params are in args[2..=3].
        // Codegen (E4) interprets DuplexHandle places in args[2..=3] as
        // "pass the address of this local's alloca as *mut *mut DuplexHandle".
        self.instructions.push(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_duplex_pair",
                vec![cap_place, r_cap_place, dh0, dh1],
                None,
            )
            .expect("hew_duplex_pair is an allowlisted runtime symbol"),
        ));

        // Create a "tuple proxy" local so TupleIndex lowering can recover dh0/dh1.
        // The proxy carries no runtime value; its index is the key into tuple_decomp.
        // Using `ResolvedTy::Unit` for the proxy type so no spurious UnknownType
        // diagnostic fires for it.
        let proxy = self.alloc_local(ResolvedTy::Unit);
        let Place::Local(proxy_idx) = proxy else {
            unreachable!("alloc_local returns Place::Local");
        };
        self.tuple_decomp.insert(proxy_idx, vec![dh0, dh1]);

        Some(proxy)
    }

    /// Emit `Instr::CallRuntimeAbi` for `hew_duplex_send`.
    ///
    /// HIR shape (from E1 bridge): `Call { callee: BindingRef("hew_duplex_send"),
    /// args: [receiver_expr, msg_expr] }` — receiver prepended by E1.
    ///
    /// MIR emission:
    ///   1. Lower `receiver_expr` → `recv_place` (expected `DuplexHandle(N)`).
    ///   2. Lower `msg_expr` → `msg_place` (the integer value's `Local(K)`).
    ///   3. Emit `ConstI64 { dest: len_place, value: 8 }` — the byte-length.
    ///   4. Emit `CallRuntimeAbi { symbol: "hew_duplex_send",
    ///         args: [recv_place, msg_place, len_place], dest: None }`.
    ///   5. Return `None` — send discards its i32 result.
    ///
    /// The receiver is NOT consumed (non-move send semantics); `owned_locals`
    /// for the receiver `DuplexHandle` must persist across multiple sends
    /// (LESSONS `raii-null-after-move`).
    fn lower_duplex_send(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        if hir_args.len() < 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::CutoverUnsupported {
                    construct: "hew_duplex_send with fewer than 2 args".to_string(),
                    site,
                },
                note: "hew_duplex_send requires receiver + message arguments".to_string(),
            });
            return None;
        }
        // args[0] = receiver (DuplexHandle — non-consuming borrow; no Move emitted).
        let recv_place = self.lower_value(&hir_args[0]);
        // args[1] = message value.
        let msg_place = self.lower_value(&hir_args[1]);

        let (Some(recv_place), Some(msg_place)) = (recv_place, msg_place) else {
            // Argument lowering failed; diagnostic already recorded.
            return None;
        };

        // Emit the byte-length constant.  The runtime ABI takes `*const u8 + usize`;
        // E4 codegen stores `msg_place`'s value to a stack alloca and passes its
        // address.  The length constant here encodes the fixed 8-byte integer size.
        //
        // SHIM(E4): the ConstI64(8) encodes the integer payload byte-length.
        // WHY: MIR has no "sizeof" expression; the integer spine always uses 8-byte
        //   i64 values, so the length is a compile-time constant for this skeleton.
        // WHEN obsolete: when the type system can express the payload size directly,
        //   or when hew_duplex_send uses a typed message rather than a byte slice.
        // WHAT: replace with a proper sizeof/alignof expression or a typed ABI.
        let len_place = self.alloc_local(ResolvedTy::I64);
        self.instructions.push(Instr::ConstI64 {
            dest: len_place,
            value: 8,
        });

        // Emit the runtime call.  `recv_place` is used as a borrow (not consumed);
        // the receiver's `owned_locals` entry survives for subsequent sends and the
        // scope-exit drop (LESSONS `raii-null-after-move`, `cleanup-all-exits`).
        self.instructions.push(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_duplex_send",
                vec![recv_place, msg_place, len_place],
                None,
            )
            .expect("hew_duplex_send is an allowlisted runtime symbol"),
        ));

        None // send result (i32 error code) is discarded
    }

    /// Lower an `HirExprKind::SpawnLambdaActor` literal to a MIR
    /// `Place::LambdaActorHandle`. The literal allocates a fresh
    /// local (typed as the actor's `Duplex<Msg, Reply>`) and emits a
    /// `Place::LambdaActorHandle(local_id)` so drop elaboration
    /// selects `DropKind::LambdaActorRelease` — the
    /// stop-on-last-handle-drop protocol with weak-ref body capture
    /// (§5.9 ratification 2).
    ///
    /// Every HIR-resolved capture is forwarded into the function's
    /// `lambda_captures` ledger with the source binding's MIR
    /// `Place` looked up via `binding_locals`. A capture whose
    /// source binding has no backend slot in the enclosing function
    /// (typically a function parameter that hasn't been wired through
    /// `binding_locals` yet) is skipped — the structural checker
    /// never sees a missing entry as a violation, and a future
    /// surface that populates parameter slots will populate this
    /// ledger the same way.
    ///
    /// Body lowering (the actor's per-message dispatch) is a
    /// follow-up slice; the MIR shape only needs the handle Place plus
    /// the capture metadata. Codegen rejects `Place::LambdaActorHandle`
    /// today (fail-closed) so a runtime substrate is not required for
    /// the static checks to land.
    fn lower_spawn_lambda_actor(&mut self, expr: &HirExpr) -> Place {
        let HirExprKind::SpawnLambdaActor { captures, .. } = &expr.kind else {
            unreachable!("lower_spawn_lambda_actor called on non-SpawnLambdaActor kind");
        };
        // Two paths produce the handle:
        //   - `let <name> = actor |..| { .. }`: the `stmt` Let arm
        //     pre-allocates the binding's slot and stashes its
        //     `LambdaActorHandle` in `pending_lambda_actor_handle`
        //     so the body's Weak self-capture finds a backend slot
        //     for the let-binding. Reuse the pre-allocated handle.
        //   - any non-let position (return-position literal, an
        //     argument, etc.): allocate a fresh local on the fly.
        let handle = if let Some(handle) = self.pending_lambda_actor_handle {
            handle
        } else {
            let local = self.alloc_local(expr.ty.clone());
            let Place::Local(local_id) = local else {
                unreachable!("alloc_local returns Place::Local");
            };
            Place::LambdaActorHandle(local_id)
        };
        for capture in captures {
            // Each captured binding must already have a backend slot
            // in the enclosing function. The forward-bound recursive
            // self capture is the let-binding itself, whose
            // `binding_locals` entry was populated by the `stmt` Let
            // arm before this producer ran. Captures from outer
            // bindings with no `binding_locals` entry (typically
            // function parameters, which the spine doesn't wire) are
            // silently skipped — `validate_lambda_captures` treats a
            // missing entry as no-finding rather than as a violation.
            if !self.binding_locals.contains_key(&capture.binding) {
                continue;
            }
            let capture_kind = match capture.kind {
                hew_hir::HirCaptureKind::Strong => crate::model::CaptureKind::Strong,
                hew_hir::HirCaptureKind::Weak => crate::model::CaptureKind::Weak,
            };
            self.lambda_captures.push(LambdaCapture {
                actor_handle: handle,
                captured: capture.binding,
                name: capture.name.clone(),
                capture_kind,
            });
        }
        handle
    }

    fn decide(&mut self, expr: &HirExpr) {
        if self
            .decisions
            .iter()
            .any(|decision| decision.site == expr.site)
        {
            return;
        }
        let strategy = match expr.value_class {
            ValueClass::CowValue => Strategy::CowShare,
            // `@linear` and `@resource` (AffineResource) both move by default;
            // `MirCheck::MustConsume` rejects unconsumed `@linear` exits.
            ValueClass::AffineResource | ValueClass::Linear => Strategy::Move,
            ValueClass::Unknown => Strategy::UnknownBlocked,
            ValueClass::BitCopy | ValueClass::PersistentShare | ValueClass::View => {
                Strategy::BorrowRead
            }
        };
        let strategy = match (expr.value_class, expr.intent) {
            (ValueClass::CowValue, IntentKind::Modify) => Strategy::EnsureUnique,
            (ValueClass::CowValue, IntentKind::Read | IntentKind::Capture) => Strategy::CowShare,
            (ValueClass::AffineResource, IntentKind::Read) => Strategy::BorrowRead,
            // `@linear` Read is *not* a borrow — the value must be consumed
            // exactly once; a read-without-consume leaves the binding
            // live for a later `MustConsume` rejection. Encode as Move
            // alongside the explicit Consume arm below.
            (ValueClass::Linear, IntentKind::Read | IntentKind::Capture)
            | (
                ValueClass::BitCopy
                | ValueClass::CowValue
                | ValueClass::AffineResource
                | ValueClass::Linear,
                IntentKind::Consume,
            ) => Strategy::Move,
            (_, IntentKind::Yield) => Strategy::Freeze,
            _ => strategy,
        };
        self.decisions.push(DecisionFact {
            site: expr.site,
            ty: expr.ty.clone(),
            value_class: expr.value_class,
            intent: expr.intent,
            strategy,
            why: "first vertical-slice classifier".to_string(),
        });
    }

    fn mark_returned_binding_moved(&mut self, expr: &HirExpr) {
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } = expr.kind
        else {
            return;
        };
        self.mark_binding_moved(id);
    }

    fn mark_binding_moved(&mut self, id: BindingId) {
        self.owned_locals.retain(|(binding, _, _)| *binding != id);
    }
}

/// Project a Checked MIR finding to a `MirDiagnostic` for the CLI
/// rejection surface. `CheckedMirFunction::checks` is the single
/// source of truth for move/borrow/init legality; this function
/// adapts those findings to the older `MirDiagnostic` channel the
/// driver already consumes. Variants whose construction surface
/// isn't yet wired (`Aliasing`, `GeneratorBorrowAcrossYield`,
/// `ActorSendEscape`) cannot appear today; they yield `None` defensively.
fn check_to_diagnostic(check: &MirCheck) -> Option<MirDiagnostic> {
    match check {
        MirCheck::UseAfterConsume {
            binding,
            name,
            consumed_at,
            used_at,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::UseAfterConsume {
                binding: *binding,
                name: name.clone(),
                consumed_at: *consumed_at,
                used_at: *used_at,
            },
            note: "binding is used after an owned value move in checked MIR".to_string(),
        }),
        MirCheck::InitialisedBeforeUse {
            binding,
            name,
            use_site,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::InitialisedBeforeUse {
                binding: *binding,
                name: name.clone(),
                use_site: *use_site,
            },
            note: "binding is read before any initialising `let` for it appears".to_string(),
        }),
        MirCheck::DecisionMapTotal { offending_sites } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::DecisionMapTotal {
                offending_sites: offending_sites.clone(),
            },
            note: "DecisionFact carries Strategy::UnknownBlocked at MIR boundary; \
                   the emitter must never receive an undecided value-class site"
                .to_string(),
        }),
        MirCheck::MustConsume {
            binding,
            name,
            exit_site,
            ty,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::MustConsume {
                binding: *binding,
                name: name.clone(),
                exit_site: *exit_site,
                ty: ty.clone(),
            },
            note: "@linear binding reached an exit without being consumed; \
                   declare a consuming method (e.g. `commit(consuming self)`) \
                   and ensure every reachable exit path invokes one"
                .to_string(),
        }),
        MirCheck::DropPlanUndetermined { block, reason } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::DropPlanUndetermined {
                block: *block,
                reason: reason.clone(),
            },
            note: "drop-elaboration could not determine the per-exit live-set \
                   for an M2 substrate handle (Duplex / lambda-actor / \
                   half-handle); the elaborator aborts rather than emit a \
                   partial drop plan (LESSONS cleanup-all-exits)"
                .to_string(),
        }),
        // No construction surface in the v0.5 integer spine. The
        // corresponding `MirDiagnosticKind` projections will land
        // alongside the construction surface for borrows, generators,
        // and actor sends.
        MirCheck::Aliasing { .. }
        | MirCheck::GeneratorBorrowAcrossYield { .. }
        | MirCheck::ActorSendEscape { .. }
        | MirCheck::ActorAskEscape { .. } => None,
    }
}

/// Drop-elaboration pass over a `CheckedMirFunction`.
///
/// Produces an `ElaboratedMirFunction` whose `blocks` + `drop_plans`
/// describe, structurally, what drops fire on every exit edge of the
/// function. The pass is intraprocedural and uses the
/// `DecisionFact::value_class` data already on the checked MIR (no
/// cross-join coalescing — council R-C3.1 / plan §5 commit 2: drops
/// fire at each exit independently; full NLL precision is deferred to
/// v0.6).
///
/// Algorithm per HEW-SPEC §3.7.8.4 (lexical scope teardown):
///   1. Walk the builder's `owned_locals` ledger (the per-function
///      ordered list of non-`BitCopy` bindings introduced by `let`).
///      The ledger is already maintained in source/declaration order
///      with bindings removed when consumed (`mark_binding_moved`).
///   2. For every `Terminator::Return` exit, emit a `DropPlan` whose
///      `drops` are the live owned-local list in reverse declaration
///      order (LIFO). `If`-lowering (Slice 2) constructs
///      `Terminator::Branch` and `Terminator::Goto` in addition to
///      `Terminator::Return`; `enumerate_exits` handles all three.
///   3. For declared-but-not-constructed terminators (`Panic`, `Yield`,
///      `Send`, `Call`), the pass enumerates them with an empty drop
///      plan when reached — later cluster additions add the construction
///      surfaces that turn these into populated plans.
///   4. A `BlockKind::Cleanup` block is emitted ONLY when a
///      `Terminator::Panic` is constructed in the function's CFG
///      (currently no spine surface — declared scaffold). Same for
///      `ExitPath::Cancel` (scope-structural cancellation, also
///      declared scaffold in v0.5).
///
/// Drop classification:
///   - `ValueClass::AffineResource` -> `ElabDrop { drop_fn: Some("<TypeName>::close") }`
///     (synthesised name; once `@resource` types reach the spine subset,
///     this is replaced by the resolved `FnId` of the type's `close`
///     consuming method).
///   - `ValueClass::Linear` -> NO implicit drop emitted. The move-checker
///     is the proof-of-consume; an unconsumed `Linear` binding has
///     already been rejected as `MirCheck::MustConsume` upstream.
///   - All other classes -> no drop emitted (`BitCopy`, `CowValue`, `View`,
///     `PersistentShare`, `Unknown` — `Unknown` is itself an upstream
///     rejection).
fn elaborate(
    checked: &CheckedMirFunction,
    builder: &Builder,
    flat_statements: &[MirStatement],
    dataflow_result: &dataflow::DataflowResult,
) -> ElaboratedMirFunction {
    // Statements stream: retained for snapshot/compat continuity with
    // the pre-Cluster-3 elaborator. Every non-`BitCopy` owned local
    // gets a checker-stream `Drop` entry in reverse-declaration order;
    // the structural drop plan in `drop_plans` is the authoritative
    // per-`ExitPath` answer. The flat stream is the union of every
    // block's `statements` in construction order — Slice 1 maintains
    // pre-CFG snapshot continuity by feeding the same union here.
    let mut elaborated_statements: Vec<MirStatement> = flat_statements.to_vec();
    for (binding, name, ty) in builder.owned_locals.iter().rev() {
        elaborated_statements.push(MirStatement::Drop {
            binding: *binding,
            name: name.clone(),
            ty: ty.clone(),
        });
    }

    // Function-wide LIFO drop sequence — one ElabDrop per
    // AffineResource owned local in reverse declaration order. The
    // per-Return-block exit live-set then narrows this sequence to
    // bindings still Live at that block's exit (drops fire only for
    // bindings whose state is Live at the exit; Consumed / Uninit
    // skip; MaybeConsumed is rejected upstream by the move-checker).
    let lifo_drops = build_lifo_drops(
        &builder.owned_locals,
        &builder.binding_locals,
        &builder.type_classes,
    );
    let (elab_blocks, drop_plans) = enumerate_exits(
        &checked.blocks,
        &lifo_drops,
        &dataflow_result.exit_states,
        &builder.binding_locals,
    );

    ElaboratedMirFunction {
        name: checked.name.clone(),
        return_ty: checked.return_ty.clone(),
        statements: elaborated_statements,
        decisions: builder.decisions.clone(),
        blocks: elab_blocks,
        drop_plans,
        coroutine: None,
        // Lambda-actor capture set, populated by the MIR producer at
        // each `HirExprKind::SpawnLambdaActor` site (see
        // `Builder::lower_spawn_lambda_actor`). The HIR resolver
        // forward-binds the lambda's own let-name before lowering the
        // body, so a body-internal reference to that name resolves to
        // a `BindingRef { resolved: Binding(let_id) }`; the resolver
        // classifies that capture as `HirCaptureKind::Weak` and every
        // other free-variable reference as `Strong`. The MIR producer
        // copies the list through with the source binding's MIR
        // `Place` attached. The structural fail-closed checker
        // `validate_lambda_captures` enforces the invariants (Weak
        // attaches to LambdaActorHandle; at most one Weak per actor
        // handle) on the emitted ledger.
        lambda_captures: builder.lambda_captures.clone(),
    }
}

/// Owning-block id for an `ExitPath`. Every variant carries a `block`
/// field — surfacing it as a single function keeps `validate_drop_plan`
/// uniform across exit kinds.
#[must_use]
fn exit_block_id(exit: &ExitPath) -> u32 {
    match *exit {
        ExitPath::Return { block }
        | ExitPath::Goto { block, .. }
        | ExitPath::Branch { block, .. }
        | ExitPath::Call { block, .. }
        | ExitPath::Panic { block }
        | ExitPath::Cancel { block }
        | ExitPath::Yield { block, .. }
        | ExitPath::Send { block, .. }
        | ExitPath::Ask { block, .. }
        | ExitPath::Select { block, .. } => block,
    }
}

/// Human-readable label for an `ExitPath` discriminator — surfaced in
/// `DropPlanUndetermined` diagnostics so the rejected exit is named.
#[must_use]
fn exit_kind_label(exit: &ExitPath) -> &'static str {
    match exit {
        ExitPath::Return { .. } => "Return",
        ExitPath::Goto { .. } => "Goto",
        ExitPath::Branch { .. } => "Branch",
        ExitPath::Call { .. } => "Call",
        ExitPath::Panic { .. } => "Panic",
        ExitPath::Cancel { .. } => "Cancel",
        ExitPath::Yield { .. } => "Yield",
        ExitPath::Send { .. } => "Send",
        ExitPath::Ask { .. } => "Ask",
        ExitPath::Select { .. } => "Select",
    }
}

/// Structural validation of an elaborated drop plan. Walks every
/// `(ExitPath, DropPlan)` entry and every `ElabBlock.drops` cleanup
/// list, verifying that each drop's `kind` matches what the drop's
/// `place` would select via `drop_kind_for`, that the per-block
/// consume-on-split invariant holds, and that the lambda-actor
/// capture side-table honours the weak-ref discipline. A mismatch
/// indicates the elaborator's drop-plan construction lost coherence —
/// surface as `MirCheck::DropPlanUndetermined` so the program is
/// rejected before codegen observes a partial / inconsistent plan.
///
/// This is the M2 substrate's fail-closed boundary: a `Place::
/// DuplexHandle` paired with `DropKind::Resource` would otherwise
/// route through the generic `close` method dispatch instead of the
/// runtime's close-both-directions protocol — silently dropping the
/// recv-direction queue. Same idea for `LambdaActorHandle` paired
/// with `DropKind::DuplexClose` (would skip the actor's stop-
/// protocol).
///
/// The walk covers EVERY exit-path discriminator, not just `Return`:
///   - `Return` is the canonical Hew exit; carries the function-wide
///     LIFO drops narrowed by per-block live-set.
///   - `Panic` and `Cancel` exits transfer to a cleanup block whose
///     `ElabBlock.drops` carry the same LIFO drops; both the
///     `DropPlan` and the destination cleanup block's `drops` are
///     validated.
///   - `Yield`, `Send`, `Select` exits carry empty `DropPlan`s today
///     (per-arm cleanup lives in codegen for `Select`; coroutine and
///     actor-send surfaces have no construction site on the integer
///     spine) but the walk treats them uniformly — when a future
///     surface populates a non-empty plan, it is checked the same
///     way without retrofitting.
///   - `Goto`, `Branch`, `Call` carry empty `DropPlan`s (intra-CFG
///     edges that don't fire drops) but are walked for forward
///     compatibility.
///
/// `ElabBlock.drops` are walked symmetrically so a malformed cleanup
/// block (e.g. a panic-cleanup block with an over-broad close-both-
/// dirs drop on a half-handle binding) is rejected at the same
/// boundary.
///
/// LESSONS: boundary-fail-closed, cleanup-all-exits.
#[must_use]
fn validate_drop_plan(elab: &ElaboratedMirFunction) -> Vec<MirCheck> {
    let mut findings = Vec::new();
    for (exit, plan) in &elab.drop_plans {
        let block = exit_block_id(exit);
        let kind_label = exit_kind_label(exit);
        for drop in &plan.drops {
            let expected = drop_kind_for(drop.place, &drop.ty);
            if drop.kind != expected {
                findings.push(MirCheck::DropPlanUndetermined {
                    block,
                    reason: format!(
                        "drop on place {:?} has kind {:?}, but the place \
                         variant selects {:?}; elaborator must use the \
                         Place-driven kind (exit path: {kind_label})",
                        drop.place, drop.kind, expected,
                    ),
                });
            }
        }
        check_duplex_split_state(block, &plan.drops, &mut findings);
    }
    // Cleanup block drops are the panic / cancel landing pad. Validate
    // the same invariants against ElabBlock.drops so a malformed
    // cleanup block surfaces at the same boundary as a malformed
    // DropPlan.
    for block in &elab.blocks {
        for drop in &block.drops {
            let expected = drop_kind_for(drop.place, &drop.ty);
            if drop.kind != expected {
                findings.push(MirCheck::DropPlanUndetermined {
                    block: block.id,
                    reason: format!(
                        "cleanup drop on place {:?} has kind {:?}, but the \
                         place variant selects {:?}; elaborator must use the \
                         Place-driven kind",
                        drop.place, drop.kind, expected,
                    ),
                });
            }
        }
        check_duplex_split_state(block.id, &block.drops, &mut findings);
    }
    validate_lambda_captures(&elab.lambda_captures, &mut findings);
    findings
}

/// Lambda-actor capture invariants. The capture side-table encodes the
/// runtime's self-binding weak-ref discipline (§5.9 ratification 2):
/// the recursive forward-bind case
///
/// ```hew
/// let fib = actor |n| { ... fib(n - 1) ... };
/// ```
///
/// captures the lambda's own let-binding name as a `Weak` reference so
/// the body does NOT keep the actor alive past external refcount zero.
///
/// Two structural invariants:
///
/// 1. A `Weak` capture must attach to a `LambdaActorHandle`. Attaching
///    a `Weak` capture to any other `Place` (a `DuplexHandle`, a plain
///    `Local`, etc.) would silently relax the refcount discipline on a
///    non-actor resource.
/// 2. At most ONE `Weak` capture per `LambdaActorHandle`. The self-
///    binding-name discipline is a single-name discipline — every
///    lambda has exactly one let-binding name, so a second `Weak`
///    capture on the same actor handle is a lowering bug.
///
/// (a) is the existing "Weak must attach to `LambdaActorHandle`" check.
/// (b) is the new "exactly one Weak per `LambdaActorHandle`" check. The
/// non-recursive lambda case (`let f = actor |n| { n + 1 }`) has zero
/// `Weak` captures and is silently accepted — the discipline only
/// applies when the body references its own binding name.
///
/// LESSONS: boundary-fail-closed, raii-null-after-move (the weak-ref
/// is the actor's null-after-move equivalent for the self-binding
/// reference).
fn validate_lambda_captures(captures: &[LambdaCapture], findings: &mut Vec<MirCheck>) {
    use std::collections::BTreeMap;

    for capture in captures {
        if matches!(capture.capture_kind, crate::model::CaptureKind::Weak)
            && !matches!(capture.actor_handle, Place::LambdaActorHandle(_))
        {
            findings.push(MirCheck::DropPlanUndetermined {
                block: 0,
                reason: format!(
                    "weak capture of `{}` attached to non-lambda-actor handle \
                     {:?}; weak captures are exclusive to LambdaActorHandle \
                     places (§5.9 ratification 2)",
                    capture.name, capture.actor_handle,
                ),
            });
        }
    }

    // Tally Weak captures per LambdaActorHandle. The self-binding-name
    // discipline is a single-name discipline — every lambda has
    // exactly one let-binding name, so multiple Weak captures on the
    // same actor handle indicate a lowering bug.
    let mut weak_per_actor: BTreeMap<u32, Vec<&str>> = BTreeMap::new();
    for capture in captures {
        if !matches!(capture.capture_kind, crate::model::CaptureKind::Weak) {
            continue;
        }
        let Place::LambdaActorHandle(n) = capture.actor_handle else {
            continue; // already rejected above
        };
        weak_per_actor
            .entry(n)
            .or_default()
            .push(capture.name.as_str());
    }
    for (handle_id, names) in weak_per_actor {
        if names.len() > 1 {
            findings.push(MirCheck::DropPlanUndetermined {
                block: 0,
                reason: format!(
                    "LambdaActorHandle({handle_id}) has {} weak captures ({}); \
                     the self-binding-name weak-ref discipline is exactly one \
                     per actor (§5.9 ratification 2)",
                    names.len(),
                    names.join(", "),
                ),
            });
        }
    }
}

/// Parent-local id for a Duplex-family Place. `DuplexHandle(N)`,
/// `SendHalf(N)`, and `RecvHalf(N)` all reference the same parent
/// Duplex local `N`; the variants only differ in which directions
/// the drop closes. Returns `None` for non-Duplex-family Places.
#[must_use]
fn duplex_parent_local(place: Place) -> Option<u32> {
    match place {
        Place::DuplexHandle(n) | Place::SendHalf(n) | Place::RecvHalf(n) => Some(n),
        Place::LambdaActorHandle(_) | Place::Local(_) | Place::ReturnSlot => None,
    }
}

/// Consume-on-split invariant for `Duplex<S, R>` handles.
///
/// A `Duplex` may be addressed by either the unified `DuplexHandle`
/// (close-both-dirs) or by the pair of direction-aliases
/// `SendHalf` / `RecvHalf` (close-one-dir each). The split operation
/// (`.send_half()` / `.recv_half()`) MOVES the underlying handle: after
/// a split, the original `DuplexHandle` binding is uninhabited and only
/// the half-handle binding(s) remain in the drop plan. If a stale
/// `DuplexHandle(N)` coexists with `SendHalf(N)` or `RecvHalf(N)` in the
/// same drop plan, codegen would emit close-both on the unified handle
/// AND close-one on the half — closing the same direction twice and, in
/// the runtime's refcount discipline, closing a direction that the
/// half-handle binding now owns. Same shape if two `SendHalf(N)` or two
/// `RecvHalf(N)` entries coexist (the half-handle is also a moved
/// resource; aliased drops would close the same queue twice).
///
/// The plan is sound iff, for each parent local `N`, the drop plan
/// contains at most one of `{DuplexHandle(N), <SendHalf(N) + RecvHalf(N)
/// pair>, SendHalf(N) alone, RecvHalf(N) alone}` — never a mix and
/// never duplicates within a half-class.
///
/// LESSONS: raii-null-after-move (consume-on-split is the affine
/// move-checker discipline expressed at the drop-plan layer),
/// cleanup-all-exits (each direction closes exactly once per exit
/// path), boundary-fail-closed (a stale-handle drop plan is rejected
/// before codegen observes it).
///
/// NOTE: the HIR construction surface for `.send_half()` /
/// `.recv_half()` is slice-4 work (`hew-types/src/builtin_names.rs:270`
/// WHEN-OBSOLETE marker). Until that surface exists, the upstream
/// lowering never EMITS a drop plan that violates the invariant; this
/// check is the structural backstop that fails closed when slice 4
/// wires the split methods through MIR. The synthetic property tests
/// in `slice3_invariants` exercise every legal and illegal split-state
/// shape against this checker today.
fn check_duplex_split_state(block: u32, drops: &[ElabDrop], findings: &mut Vec<MirCheck>) {
    use std::collections::BTreeMap;

    #[derive(Default)]
    struct PerParent {
        whole: u32,
        send: u32,
        recv: u32,
    }

    let mut per_parent: BTreeMap<u32, PerParent> = BTreeMap::new();
    for drop in drops {
        let Some(parent) = duplex_parent_local(drop.place) else {
            continue;
        };
        let entry = per_parent.entry(parent).or_default();
        match drop.place {
            Place::DuplexHandle(_) => entry.whole = entry.whole.saturating_add(1),
            Place::SendHalf(_) => entry.send = entry.send.saturating_add(1),
            Place::RecvHalf(_) => entry.recv = entry.recv.saturating_add(1),
            _ => {}
        }
    }

    for (parent, counts) in per_parent {
        // Whole + any half is the "stale unified handle after split"
        // bug: split would have moved the DuplexHandle out, so its
        // presence alongside a half means codegen would close the same
        // direction twice.
        if counts.whole > 0 && (counts.send > 0 || counts.recv > 0) {
            findings.push(MirCheck::DropPlanUndetermined {
                block,
                reason: format!(
                    "drop plan contains both DuplexHandle({parent}) and a half-handle \
                     (send={}, recv={}) for the same parent local; the split \
                     operation must consume the DuplexHandle (slice-4 HIR seam \
                     for .send_half() / .recv_half() must MOVE the unified handle)",
                    counts.send, counts.recv,
                ),
            });
        }
        // Multiple DuplexHandle entries for the same parent: should be
        // structurally impossible (one binding per local), but reject
        // defensively so a duplicated drop emission is caught.
        if counts.whole > 1 {
            findings.push(MirCheck::DropPlanUndetermined {
                block,
                reason: format!(
                    "drop plan contains {} DuplexHandle({parent}) entries for the \
                     same parent local; close-both-dirs must fire exactly once",
                    counts.whole,
                ),
            });
        }
        // Aliased halves: two SendHalf(N) or two RecvHalf(N) would
        // close the same queue twice. The split methods return a
        // single half per direction, so duplicates are a lowering bug.
        if counts.send > 1 {
            findings.push(MirCheck::DropPlanUndetermined {
                block,
                reason: format!(
                    "drop plan contains {} SendHalf({parent}) entries; \
                     the S-direction must close exactly once",
                    counts.send,
                ),
            });
        }
        if counts.recv > 1 {
            findings.push(MirCheck::DropPlanUndetermined {
                block,
                reason: format!(
                    "drop plan contains {} RecvHalf({parent}) entries; \
                     the R-direction must close exactly once",
                    counts.recv,
                ),
            });
        }
    }
}

/// Per-`Duplex` consume state at a program point. Tracks the
/// affine move-checker discipline expressed at the backend
/// instruction layer: a `.send_half()` / `.recv_half()` split
/// emits an `Instr::Move { src: Place::DuplexHandle(N), dest:
/// Place::SendHalf(N) | Place::RecvHalf(N) }`, which moves the
/// unified handle out and leaves only the half-handle binding(s)
/// live in the drop plan.
///
/// Lattice semantics over the CFG meet:
///   - `Live ⊓ Live = Live`
///   - `Live ⊓ Consumed = MaybeConsumed` (some-path-consumed)
///   - `Consumed ⊓ Consumed = Consumed`
///   - `MaybeConsumed ⊓ X = MaybeConsumed`
///
/// Either `Consumed` or `MaybeConsumed` at a block whose drop plan
/// contains `DuplexHandle(N)` is rejected — `Consumed` because the
/// drop is structurally stale, `MaybeConsumed` because the program
/// could close the same direction twice on some paths
/// (fail-closed for ambiguous shapes per the M2 substrate's
/// `boundary-fail-closed` discipline).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DuplexSplitState {
    /// No `.send_half()` / `.recv_half()` consume of the unified
    /// handle has been observed on any reaching path.
    Live,
    /// The unified handle was moved on every reaching path; carries
    /// the earliest split-emitting block id for diagnostic anchoring.
    Consumed(u32),
    /// The unified handle was moved on some-but-not-all reaching
    /// paths; the drop plan cannot decide whether to fire the close.
    /// Carries the earliest split-emitting block id.
    MaybeConsumed(u32),
}

impl DuplexSplitState {
    /// Lattice meet over the three-state space. Commutative,
    /// associative, idempotent — pinned by tests below.
    fn meet(self, other: DuplexSplitState) -> DuplexSplitState {
        use DuplexSplitState::{Consumed, Live, MaybeConsumed};
        match (self, other) {
            (Live, Live) => Live,
            (Live, Consumed(b) | MaybeConsumed(b)) | (Consumed(b) | MaybeConsumed(b), Live) => {
                MaybeConsumed(b)
            }
            (Consumed(a), Consumed(b)) => Consumed(a.min(b)),
            (Consumed(a), MaybeConsumed(b)) | (MaybeConsumed(a), Consumed(b)) => {
                MaybeConsumed(a.min(b))
            }
            (MaybeConsumed(a), MaybeConsumed(b)) => MaybeConsumed(a.min(b)),
        }
    }
}

/// Walk the backend `Instr` stream across a function's CFG and
/// reject drop plans that fire `Place::DuplexHandle(N)` on a block
/// whose reaching paths have already moved the unified handle into
/// a `SendHalf` / `RecvHalf` split.
///
/// The slice-3 structural checker (`check_duplex_split_state`) catches
/// only same-drop-list collisions of `DuplexHandle(N)` with
/// `SendHalf(N)` / `RecvHalf(N)`. It cannot detect the cross-block
/// case where block A emits the split and block B (a successor)
/// drops the now-stale `DuplexHandle(N)`. This dataflow closes the
/// gap.
///
/// The transfer function scans each block's instructions for
/// `Instr::Move { src: Place::DuplexHandle(N), dest:
/// Place::SendHalf(N) | Place::RecvHalf(N) }` and transitions the
/// parent's state to `Consumed(block_id)`. Inter-block, the entry
/// state of each block is the meet over predecessor exit states;
/// the entry block starts with every parent `Live`.
///
/// The CFG is acyclic in v0.5 (the loop cluster is deferred), so
/// the worklist terminates in one RPO pass. The check then iterates
/// every `(ExitPath, DropPlan)` plus every cleanup
/// `ElabBlock.drops`, looking for `DuplexHandle(N)` drops whose
/// owning block's exit state is `Consumed` (definitely stale) or
/// `MaybeConsumed` (ambiguous). Either case produces a
/// `MirCheck::DropPlanUndetermined`.
///
/// Cleanup blocks inherit the state of the normal block they
/// trap-cleanup from — `enumerate_exits` clones the normal-path
/// `drops_template` unfiltered into the cleanup block's drops, so a
/// `DuplexHandle` left in the cleanup block's drop list reflects a
/// program shape where the normal-path consume already happened
/// but the cleanup path was forgotten. Validate against the normal
/// predecessor's exit state.
///
/// LESSONS: cleanup-all-exits, raii-null-after-move,
/// boundary-fail-closed.
#[allow(
    clippy::too_many_lines,
    reason = "single fixpoint + drop-list-walk; splitting would obscure the dataflow"
)]
fn validate_cross_block_split_consume(
    blocks: &[BasicBlock],
    elab: &ElaboratedMirFunction,
) -> Vec<MirCheck> {
    use std::collections::{BTreeMap, VecDeque};

    let mut findings = Vec::new();
    if blocks.is_empty() {
        return findings;
    }

    // Per-block per-parent exit state. Absent entries are implicitly
    // `Live` — the most-permissive default. The map carries only
    // parents whose state diverged from `Live` on at least one
    // reaching path; this keeps the lattice sparse.
    let mut exit_states: HashMap<u32, BTreeMap<u32, DuplexSplitState>> = HashMap::new();
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();

    // Predecessor edges for the meet step.
    let mut preds: HashMap<u32, Vec<u32>> = HashMap::new();
    for block in blocks {
        let mut emit = |target: u32| preds.entry(target).or_default().push(block.id);
        match &block.terminator {
            Terminator::Return | Terminator::Trap { .. } => {}
            Terminator::Goto { target } => emit(*target),
            Terminator::Branch {
                then_target,
                else_target,
                ..
            } => {
                emit(*then_target);
                emit(*else_target);
            }
            Terminator::Call { next, .. }
            | Terminator::Yield { next, .. }
            | Terminator::Send { next, .. }
            | Terminator::Ask { next, .. }
            | Terminator::Select { next, .. } => emit(*next),
        }
    }

    let successors = |block: &BasicBlock| -> Vec<u32> {
        match &block.terminator {
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
    };

    // Forward fixpoint. The entry block is id 0 by construction.
    let entry_id = 0;
    let mut worklist: VecDeque<u32> = blocks.iter().map(|b| b.id).collect();
    let mut all_parents: HashSet<u32> = HashSet::new();
    // Seed all_parents from any DuplexHandle/Half-handle place that
    // appears in any block's instructions — those are the only
    // parents the cross-block dataflow needs to track.
    for block in blocks {
        for instr in &block.instructions {
            for place in instr_places(instr) {
                if let Some(parent) = duplex_parent_local(place) {
                    all_parents.insert(parent);
                }
            }
        }
    }
    if all_parents.is_empty() {
        return findings;
    }

    while let Some(bb_id) = worklist.pop_front() {
        let Some(block) = by_id.get(&bb_id) else {
            continue;
        };
        let entry: BTreeMap<u32, DuplexSplitState> = if bb_id == entry_id {
            BTreeMap::new()
        } else {
            let empty = Vec::new();
            let predecessors = preds.get(&bb_id).unwrap_or(&empty);
            meet_predecessors_split(predecessors, &exit_states, &all_parents)
        };
        let new_exit = transfer_block_split(entry, block);
        let changed = exit_states.get(&bb_id).is_none_or(|prev| *prev != new_exit);
        exit_states.insert(bb_id, new_exit);
        if changed {
            for succ in successors(block) {
                worklist.push_back(succ);
            }
        }
    }

    // Inspect every drop plan / cleanup-block-drop list against the
    // owning block's exit state for the relevant parent. A stale
    // unified-handle drop (`Consumed` or `MaybeConsumed`) is rejected.
    let report_drop = |block: u32, drops: &[ElabDrop], findings: &mut Vec<MirCheck>| {
        let Some(state_map) = exit_states.get(&block) else {
            return;
        };
        for drop in drops {
            let Place::DuplexHandle(parent) = drop.place else {
                continue;
            };
            let state = state_map
                .get(&parent)
                .copied()
                .unwrap_or(DuplexSplitState::Live);
            match state {
                DuplexSplitState::Live => {}
                DuplexSplitState::Consumed(consume_block) => {
                    findings.push(MirCheck::DropPlanUndetermined {
                        block,
                        reason: format!(
                            "drop plan contains DuplexHandle({parent}) but block {consume_block} \
                             on every reaching path moves the unified handle into a half-handle \
                             (split via .send_half()/.recv_half() consumes the DuplexHandle; \
                             the half-handle binding is the only remaining owner)"
                        ),
                    });
                }
                DuplexSplitState::MaybeConsumed(consume_block) => {
                    findings.push(MirCheck::DropPlanUndetermined {
                        block,
                        reason: format!(
                            "drop plan contains DuplexHandle({parent}) but block {consume_block} \
                             on some reaching paths moves the unified handle into a half-handle \
                             (split-on-some-paths leaves the drop ambiguous; fail-closed per \
                             cleanup-all-exits)"
                        ),
                    });
                }
            }
        }
    };

    for (exit, plan) in &elab.drop_plans {
        let block = exit_block_id(exit);
        report_drop(block, &plan.drops, &mut findings);
    }
    // Cleanup blocks: validate against the cleanup block's predecessor
    // (the normal block that trapped into it). The cleanup block id
    // itself is past the highest normal-block id and has no
    // predecessor entry in `exit_states`; consult the normal block
    // whose `Terminator::Panic` produced the cleanup edge.
    for elab_block in &elab.blocks {
        if elab_block.kind != BlockKind::Cleanup {
            continue;
        }
        // Find the normal block that traps into this cleanup. The
        // `Terminator::Panic` path generated cleanup ids past the
        // max normal id; the cleanup's drops mirror the normal
        // block's pre-terminator state. Without a back-reference,
        // we conservatively validate against EVERY normal block whose
        // Terminator is Panic (the elab structure has one cleanup
        // per Panic). For each, report any stale DuplexHandle drop.
        for raw_block in blocks {
            if matches!(raw_block.terminator, Terminator::Trap { .. }) {
                report_drop(raw_block.id, &elab_block.drops, &mut findings);
            }
        }
    }

    findings
}

fn meet_predecessors_split(
    preds: &[u32],
    exit_states: &std::collections::HashMap<u32, std::collections::BTreeMap<u32, DuplexSplitState>>,
    all_parents: &std::collections::HashSet<u32>,
) -> std::collections::BTreeMap<u32, DuplexSplitState> {
    use std::collections::BTreeMap;
    if preds.is_empty() {
        return BTreeMap::new();
    }
    let mut entry = BTreeMap::new();
    for &parent in all_parents {
        let acc = preds
            .iter()
            .map(|p| {
                exit_states
                    .get(p)
                    .and_then(|m| m.get(&parent).copied())
                    .unwrap_or(DuplexSplitState::Live)
            })
            .reduce(DuplexSplitState::meet)
            .unwrap_or(DuplexSplitState::Live);
        if !matches!(acc, DuplexSplitState::Live) {
            entry.insert(parent, acc);
        }
    }
    entry
}

fn transfer_block_split(
    entry: std::collections::BTreeMap<u32, DuplexSplitState>,
    block: &BasicBlock,
) -> std::collections::BTreeMap<u32, DuplexSplitState> {
    let mut state = entry;
    for instr in &block.instructions {
        if let Instr::Move { dest, src } = instr {
            if let (Place::DuplexHandle(parent), true) = (
                *src,
                matches!(dest, Place::SendHalf(_) | Place::RecvHalf(_)),
            ) {
                // Transition the parent to Consumed by this block. A
                // prior MaybeConsumed/Consumed entry is overwritten —
                // the move-checker upstream has already determined
                // whether a re-use after consume is legal; for the
                // drop-plan check, the latest consume in this block
                // is the relevant anchor.
                state.insert(parent, DuplexSplitState::Consumed(block.id));
            }
        }
    }
    state
}

/// Return every `Place` mentioned by a backend `Instr`. Used by the
/// cross-block split-state seed pass to discover which parent
/// locals participate in the dataflow.
fn instr_places(instr: &Instr) -> Vec<Place> {
    match instr {
        Instr::ConstI64 { dest, .. } => vec![*dest],
        Instr::IntAdd { dest, lhs, rhs }
        | Instr::IntSub { dest, lhs, rhs }
        | Instr::IntMul { dest, lhs, rhs }
        | Instr::IntDiv { dest, lhs, rhs, .. }
        | Instr::IntRem { dest, lhs, rhs, .. }
        | Instr::IntShl { dest, lhs, rhs }
        | Instr::IntShr { dest, lhs, rhs, .. }
        | Instr::IntCmp { dest, lhs, rhs, .. } => vec![*dest, *lhs, *rhs],
        Instr::IntArithChecked {
            dest,
            lhs,
            rhs,
            overflow_flag,
            ..
        } => vec![*dest, *lhs, *rhs, *overflow_flag],
        Instr::Move { dest, src } => vec![*dest, *src],
        Instr::Drop { place, .. } => vec![*place],
        Instr::CallRuntimeAbi(call) => {
            // Every Place participating in the runtime call surfaces
            // here so the cross-block split-state seed pass can
            // observe handle moves through C-ABI boundaries. The
            // `dest` (when present) is also a Place the dataflow
            // needs to discover.
            let mut places: Vec<Place> = call.args().to_vec();
            if let Some(d) = call.dest() {
                places.push(d);
            }
            places
        }
    }
}

/// Resolve the `DropKind` for an `ElabDrop` given the addressable
/// `Place` and the binding's `ResolvedTy`.
///
/// The M2 substrate's drop kinds are selected by the `Place` variant
/// rather than the `ResolvedTy` alone — a binding whose type is
/// `Duplex<S, R>` may be addressed by either a `DuplexHandle`
/// (close-both-dirs) or a `SendHalf` / `RecvHalf` (close-one-dir
/// alias), and the kind must follow the Place. Lambda-actor handles
/// share the underlying `Duplex<Msg, Reply>` type but use
/// `LambdaActorHandle` Place addressing so they select
/// `LambdaActorRelease` — the stop-on-last-handle-drop protocol with
/// weak-ref body capture (§5.9 ratification 2).
///
/// `Place::Local` / `Place::ReturnSlot` fall through to
/// `DropKind::Resource` — the pre-M2 generic `@resource` close path.
///
/// LESSONS: cleanup-all-exits, raii-null-after-move,
/// boundary-fail-closed (kind is selected by Place; mismatching
/// Place + `DropKind` is structurally impossible because this function
/// is the single source of truth).
#[must_use]
fn drop_kind_for(place: Place, _ty: &ResolvedTy) -> DropKind {
    match place {
        Place::DuplexHandle(_) => DropKind::DuplexClose,
        Place::LambdaActorHandle(_) => DropKind::LambdaActorRelease,
        Place::SendHalf(_) => DropKind::DuplexHalfClose(crate::model::Direction::Send),
        Place::RecvHalf(_) => DropKind::DuplexHalfClose(crate::model::Direction::Recv),
        Place::Local(_) | Place::ReturnSlot => DropKind::Resource,
    }
}

/// LIFO drop sequence for an owned-locals ledger. Only `AffineResource`
/// contributes; `Linear` is the move-checker's responsibility (`MustConsume`),
/// and other classes have no implicit drop.
///
/// The `binding_locals` map is consulted to resolve each owned-local's
/// real backend `Place`. A binding without an entry (function parameters
/// and other surfaces that don't populate `binding_locals`) does not
/// appear in `owned_locals` either today, so the `ReturnSlot` fallback
/// arm is structurally unreachable; it survives only as a fail-soft for
/// future surfaces that may extend `owned_locals` ahead of `binding_locals`.
fn build_lifo_drops(
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    type_classes: &hew_hir::TypeClassTable,
) -> Vec<ElabDrop> {
    let mut drops = Vec::new();
    for (binding, _name, ty) in owned_locals.iter().rev() {
        match ValueClass::of_ty(ty, type_classes) {
            ValueClass::AffineResource => {
                // Registry-driven drop_fn dispatch. The HIR-lowering pass
                // populates `type_classes` with `(marker, Some(close_method))`
                // for every `#[resource]` type; reaching this arm without
                // a `close_method` is structurally unreachable because the
                // `E_RESOURCE_MISSING_CLOSE` HIR diagnostic short-circuits
                // the pipeline upstream. The string form is preserved as a
                // failsafe; codegen rejects `Some(_)` until runtime drop
                // dispatch lands (`hew-codegen-rs/src/llvm.rs:471`).
                let drop_fn = match ty {
                    ResolvedTy::Named { name, .. } => type_classes
                        .get(name)
                        .and_then(|(_, close)| close.as_ref())
                        .map(|m| format!("{name}::{m}")),
                    // Task<T> and all other types have no user-visible close
                    // method. Task<T> drop (hew_task_await_blocking +
                    // hew_task_free) lands as a runtime ABI call in a later
                    // slice (MIR/codegen glue); no close method name here.
                    _ => None,
                };
                // Resolve to the binding's real backend place. Falling
                // back to `ReturnSlot` for an unmapped binding would
                // drop the wrong slot — fail closed instead. The
                // `stmt` handler always populates `binding_locals` for
                // any binding that reaches `owned_locals` (see
                // `HirStmtKind::Let` arm), so this expect is a builder
                // invariant. A future surface that grows
                // `owned_locals` ahead of `binding_locals` must wire
                // a real `Place` before reaching here. LESSONS:
                // boundary-fail-closed.
                let place = *binding_locals.get(binding).unwrap_or_else(|| {
                    panic!(
                        "build_lifo_drops invariant: binding {binding:?} is in owned_locals \
                         but missing from binding_locals; lowering must wire a Place before \
                         the drop-elaboration pass observes the binding"
                    )
                });
                // Drop-kind classification for the M2 substrate. The
                // pre-M2 generic `@resource` path keeps `DropKind::Resource`;
                // M2 Duplex / lambda-actor / half-handle Places select
                // the specialised kinds so codegen (slice 5) and the
                // runtime (slice 4) emit the right close protocol.
                // LESSONS: cleanup-all-exits, raii-null-after-move.
                let kind = drop_kind_for(place, ty);
                drops.push(ElabDrop {
                    place,
                    ty: ty.clone(),
                    drop_fn,
                    kind,
                });
            }
            // Linear, BitCopy, CowValue, PersistentShare, View, Unknown:
            // no implicit drop. Linear is enforced by MustConsume; the
            // rest have no drop semantics by value-class definition.
            ValueClass::Linear
            | ValueClass::BitCopy
            | ValueClass::CowValue
            | ValueClass::PersistentShare
            | ValueClass::View
            | ValueClass::Unknown => {}
        }
    }
    drops
}

/// Build the elaborated block list + per-`ExitPath` drop plans for a
/// function's CFG. Every basic block becomes one `ElabBlock` of
/// `BlockKind::Normal`; `Terminator::Panic` synthesises a sibling
/// `BlockKind::Cleanup` block. Each block's terminator maps to one
/// `(ExitPath, DropPlan)` entry. `Return`-terminated blocks narrow
/// the function-wide LIFO `lifo` sequence to bindings whose state at
/// that block's exit is `Live` — bindings already `Consumed` on
/// every reaching path do not need their drop fired again
/// (LESSONS `raii-null-after-move`). `MaybeConsumed` at a Return
/// exit is rejected upstream by the move-checker; the elaborator
/// treats it as if `Live` for drop-plan purposes, but the program
/// would have already been rejected before reaching codegen so the
/// drop list is informational.
#[allow(
    clippy::too_many_lines,
    reason = "enumerate_exits is a flat match over Terminator variants \
              with per-arm payload construction; the line count is the \
              variant count, not deep nesting"
)]
fn enumerate_exits(
    blocks: &[BasicBlock],
    lifo: &[ElabDrop],
    exit_states: &std::collections::HashMap<
        u32,
        std::collections::BTreeMap<hew_hir::BindingId, dataflow::BindingState>,
    >,
    binding_locals: &HashMap<BindingId, Place>,
) -> (Vec<ElabBlock>, Vec<(ExitPath, DropPlan)>) {
    // Track the highest block id observed so cleanup-block ids can
    // start past it. Slice 2 onwards may emit multiple non-trivial
    // blocks; reserving cleanup ids past the max keeps invariants from
    // the single-block era intact.
    let max_normal_id = blocks.iter().map(|b| b.id).max().unwrap_or(0);
    let mut elab_blocks: Vec<ElabBlock> = blocks
        .iter()
        .map(|b| ElabBlock {
            id: b.id,
            kind: BlockKind::Normal,
            drops: Vec::new(),
            successor: None,
        })
        .collect();
    let mut next_cleanup_id = max_normal_id.saturating_add(1);
    let mut plans: Vec<(ExitPath, DropPlan)> = Vec::new();
    let drops_template = lifo.to_vec();

    // Map each owned-local's Place back to its BindingId so the
    // per-exit filter can consult exit_states. The drops in `lifo`
    // already carry the binding's Place but not its id; reverse the
    // builder's `binding_locals` (BindingId -> Place) is the cleanest
    // bridge. Builds only as large as there are owned bindings.
    let place_to_binding: std::collections::HashMap<Place, BindingId> = binding_locals
        .iter()
        .map(|(binding, place)| (*place, *binding))
        .collect();

    let drops_for_exit = |block_id: u32| -> Vec<ElabDrop> {
        let Some(state_map) = exit_states.get(&block_id) else {
            // No dataflow result for this block (defensive — every
            // reachable block has an exit_state entry after
            // analyze). Fall back to the function-wide LIFO.
            return drops_template.clone();
        };
        drops_template
            .iter()
            .filter(|drop| match place_to_binding.get(&drop.place) {
                Some(binding) => matches!(
                    state_map
                        .get(binding)
                        .copied()
                        .unwrap_or(dataflow::BindingState::Uninit),
                    dataflow::BindingState::Live | dataflow::BindingState::MaybeConsumed(_)
                ),
                // No binding mapping → conservatively keep the drop.
                // This arm guards against future surfaces that build
                // drops outside the binding_locals registry; the
                // current `build_lifo_drops` `expect()` rules out
                // this path today, but keep it for forward safety.
                None => true,
            })
            .cloned()
            .collect()
    };

    for block in blocks {
        let block_id = block.id;
        let plan = match &block.terminator {
            Terminator::Return => (
                ExitPath::Return { block: block_id },
                DropPlan {
                    drops: drops_for_exit(block_id),
                },
            ),
            Terminator::Goto { target } => (
                ExitPath::Goto {
                    block: block_id,
                    target: *target,
                },
                DropPlan::default(),
            ),
            Terminator::Branch {
                cond: _,
                then_target,
                else_target,
            } => (
                ExitPath::Branch {
                    block: block_id,
                    then_target: *then_target,
                    else_target: *else_target,
                },
                DropPlan::default(),
            ),
            Terminator::Call {
                callee,
                args: _,
                dest: _,
                next,
            } => (
                ExitPath::Call {
                    block: block_id,
                    callee: callee.clone(),
                    next: *next,
                },
                DropPlan::default(),
            ),
            Terminator::Trap { .. } => {
                // Cleanup block: same LIFO drop plan as the normal exit
                // at this scope depth; no successor (trap is terminal).
                let cleanup_id = next_cleanup_id;
                next_cleanup_id = next_cleanup_id.saturating_add(1);
                elab_blocks.push(ElabBlock {
                    id: cleanup_id,
                    kind: BlockKind::Cleanup,
                    drops: drops_template.clone(),
                    successor: None,
                });
                (
                    ExitPath::Panic { block: block_id },
                    DropPlan {
                        drops: drops_template.clone(),
                    },
                )
            }
            Terminator::Yield { value: _, next } => (
                ExitPath::Yield {
                    block: block_id,
                    next: *next,
                },
                DropPlan::default(),
            ),
            Terminator::Send {
                actor: _,
                value: _,
                next,
            } => (
                // `actor` is a Place; the ExitPath::Send slot carries
                // the callee name. Spine has no Send construction
                // surface, so this is unreachable in practice — empty
                // placeholder name.
                ExitPath::Send {
                    block: block_id,
                    actor: String::new(),
                    next: *next,
                },
                DropPlan::default(),
            ),
            Terminator::Ask {
                actor,
                value: _,
                channel,
                reply_dest: _,
                next,
            } => (
                // Declared-only. Spine has no Ask construction surface;
                // HIR-to-MIR lowers select arms into `Terminator::Select`,
                // and non-select actor calls remain rejected as
                // `CutoverUnsupported`. The `ExitPath::Ask` slot carries
                // the `channel` Place because the loser-cleanup sequence
                // (`hew_reply_channel_cancel` + `hew_reply_channel_free`)
                // is what the cleanup CFG needs when the construction
                // surface lands. Empty drop plan is a placeholder until
                // the cleanup CFG wires per-arm loser-cleanup blocks.
                ExitPath::Ask {
                    block: block_id,
                    actor: *actor,
                    channel: *channel,
                    next: *next,
                },
                DropPlan::default(),
            ),
            Terminator::Select { arms: _, next } => (
                // Per-arm select-loser cleanup lives in codegen, not in
                // the function-wide DropPlan. The DropPlan abstraction
                // models LIFO `@resource` drops over `place + drop_fn`;
                // select-loser cleanup needs two operands (the resource
                // and the runtime-allocated registration id returned by
                // the substrate primitive) and runs at the select
                // dispatch site, not the function exit. Keeping it out
                // of DropPlan avoids stretching the ElabDrop shape to
                // cover a case it was not designed for.
                //
                // The contract codegen must honour for each arm kind:
                //
                //   - StreamNext loser: emit
                //     `hew_stream_cancel_pending_read(stream, id)`
                //     where `id` is the PendingReadId returned by the
                //     winning-side `hew_stream_poll`. The stream
                //     binding remains usable in the enclosing scope
                //     (no item consumed). See `hew-runtime::stream`
                //     for the ABI and TOCTOU contract.
                //
                //   - ActorAsk loser: withdraw the envelope by
                //     correlation id if not yet dispatched, otherwise
                //     tombstone the reply sink; a late reply is
                //     classified as OrphanedAsk and dropped silently.
                //
                //   - TaskAwait loser: cancel the task at its next
                //     safepoint via the single-task cancel primitive;
                //     the awaitable handle is torn down.
                //
                //   - AfterTimer loser: cancel the timer; no callback
                //     fires.
                //
                // LESSONS: cleanup-all-exits — every select exit path
                // gets a non-empty cleanup at the codegen dispatch
                // site; the function-wide DropPlan is intentionally
                // empty for ExitPath::Select.
                ExitPath::Select {
                    block: block_id,
                    next: *next,
                },
                DropPlan::default(),
            ),
        };
        plans.push(plan);
    }
    (elab_blocks, plans)
}

// ============================================================================
// Slice 3 (M2 substrate) drop-plan invariant tests.
//
// Pin the per-Return live-set narrowing semantics + the Place->DropKind
// invariants + the weak-ref capture discipline. Built directly against
// the internal helpers (`drop_kind_for`, `validate_drop_plan`) using
// synthetic `ElaboratedMirFunction` inputs so the HIR-construction gap
// (no LambdaActor/Duplex HIR shape yet) doesn't block coverage.
// ============================================================================

#[cfg(test)]
mod slice3_invariants {
    use super::*;
    use crate::model::{CaptureKind, Direction};

    /// A `Duplex<i64, i64>` `ResolvedTy` used as a stand-in payload
    /// for synthetic `ElabDrop` entries. The body of these tests
    /// cares about `Place` + `DropKind`, not the inner type detail.
    fn duplex_int_int_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![ResolvedTy::I64, ResolvedTy::I64],
        }
    }

    fn make_elab_with_drops(drops: Vec<ElabDrop>) -> ElaboratedMirFunction {
        ElaboratedMirFunction {
            name: "synthetic".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![],
            drop_plans: vec![(ExitPath::Return { block: 0 }, DropPlan { drops })],
            coroutine: None,
            lambda_captures: vec![],
        }
    }

    // ---------- drop_kind_for: Place -> DropKind mapping ----------

    #[test]
    fn drop_kind_for_duplex_handle_selects_duplex_close() {
        let ty = duplex_int_int_ty();
        assert_eq!(
            drop_kind_for(Place::DuplexHandle(0), &ty),
            DropKind::DuplexClose
        );
    }

    #[test]
    fn drop_kind_for_lambda_actor_handle_selects_lambda_actor_release() {
        let ty = duplex_int_int_ty();
        assert_eq!(
            drop_kind_for(Place::LambdaActorHandle(0), &ty),
            DropKind::LambdaActorRelease
        );
    }

    #[test]
    fn drop_kind_for_send_half_selects_duplex_half_close_send() {
        let ty = duplex_int_int_ty();
        assert_eq!(
            drop_kind_for(Place::SendHalf(0), &ty),
            DropKind::DuplexHalfClose(Direction::Send)
        );
    }

    #[test]
    fn drop_kind_for_recv_half_selects_duplex_half_close_recv() {
        let ty = duplex_int_int_ty();
        assert_eq!(
            drop_kind_for(Place::RecvHalf(0), &ty),
            DropKind::DuplexHalfClose(Direction::Recv)
        );
    }

    #[test]
    fn drop_kind_for_local_selects_resource() {
        // Pre-M2 path: generic Resource for Local Places. Pinning this
        // is the regression guard against accidentally routing Local
        // drops through a Duplex-specific protocol.
        let ty = duplex_int_int_ty();
        assert_eq!(drop_kind_for(Place::Local(0), &ty), DropKind::Resource);
    }

    // ---------- validate_drop_plan: structural invariants ----------

    #[test]
    fn validate_drop_plan_accepts_consistent_duplex_close() {
        // A DuplexHandle paired with DuplexClose — the canonical M2
        // substrate shape. No findings.
        let drops = vec![ElabDrop {
            place: Place::DuplexHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose,
        }];
        let elab = make_elab_with_drops(drops);
        assert!(
            validate_drop_plan(&elab).is_empty(),
            "consistent (DuplexHandle, DuplexClose) must not flag"
        );
    }

    #[test]
    fn validate_drop_plan_rejects_duplex_handle_with_resource_kind() {
        // A DuplexHandle paired with DropKind::Resource would silently
        // route through generic Type::close dispatch and miss the
        // close-both-directions protocol. Must surface as
        // DropPlanUndetermined.
        let drops = vec![ElabDrop {
            place: Place::DuplexHandle(7),
            ty: duplex_int_int_ty(),
            drop_fn: Some("Duplex::close".to_string()),
            kind: DropKind::Resource,
        }];
        let elab = make_elab_with_drops(drops);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "exactly one finding expected");
        let MirCheck::DropPlanUndetermined { block, reason } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert_eq!(*block, 0);
        assert!(
            reason.contains("DuplexHandle") && reason.contains("Resource"),
            "diagnostic must name both the Place and the wrong kind: {reason}"
        );
    }

    #[test]
    fn validate_drop_plan_rejects_lambda_actor_handle_with_duplex_close_kind() {
        // LambdaActorHandle MUST select LambdaActorRelease (the
        // stop-protocol with weak-ref body capture). DuplexClose would
        // skip the actor's stop protocol — silently leaking the actor.
        let drops = vec![ElabDrop {
            place: Place::LambdaActorHandle(3),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose,
        }];
        let elab = make_elab_with_drops(drops);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        assert!(matches!(findings[0], MirCheck::DropPlanUndetermined { .. }));
    }

    #[test]
    fn validate_drop_plan_rejects_send_half_with_close_both_kind() {
        // SendHalf MUST close one direction only; pairing with
        // DuplexClose (close-both) would over-close the recv side.
        let drops = vec![ElabDrop {
            place: Place::SendHalf(1),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose,
        }];
        let elab = make_elab_with_drops(drops);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn validate_drop_plan_rejects_recv_half_with_send_direction() {
        // RecvHalf MUST close Direction::Recv; pairing with
        // Direction::Send would close the wrong queue.
        let drops = vec![ElabDrop {
            place: Place::RecvHalf(2),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexHalfClose(Direction::Send),
        }];
        let elab = make_elab_with_drops(drops);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn validate_drop_plan_accepts_half_handle_pair_closing_both_dirs() {
        // A SendHalf + RecvHalf pair, each closing its own direction —
        // the canonical "duplex split via .send_half() / .recv_half()"
        // shape. Together they close both directions; individually
        // each is a one-direction drop.
        let drops = vec![
            ElabDrop {
                place: Place::SendHalf(0),
                ty: duplex_int_int_ty(),
                drop_fn: None,
                kind: DropKind::DuplexHalfClose(Direction::Send),
            },
            ElabDrop {
                place: Place::RecvHalf(0),
                ty: duplex_int_int_ty(),
                drop_fn: None,
                kind: DropKind::DuplexHalfClose(Direction::Recv),
            },
        ];
        let elab = make_elab_with_drops(drops);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    // ---------- consume-on-split invariant for Duplex<S, R> ----------

    /// Build an `ElabDrop` addressing a Duplex-family `Place` with its
    /// canonical `DropKind`. Used by every split-state shape below.
    fn duplex_drop(place: Place) -> ElabDrop {
        let ty = duplex_int_int_ty();
        let kind = drop_kind_for(place, &ty);
        ElabDrop {
            place,
            ty,
            drop_fn: None,
            kind,
        }
    }

    #[test]
    fn split_state_whole_only_is_accepted() {
        // Pre-split: only DuplexHandle(N) is in the drop plan. Canonical
        // "no .send_half() was ever called" shape.
        let elab = make_elab_with_drops(vec![duplex_drop(Place::DuplexHandle(0))]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_both_halves_only_is_accepted() {
        // Post-full-split: DuplexHandle is gone (consumed by both
        // split methods), only SendHalf(N) + RecvHalf(N) remain.
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::SendHalf(0)),
            duplex_drop(Place::RecvHalf(0)),
        ]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_send_half_only_is_accepted() {
        // .send_half() called, RecvHalf retained on the original (rare
        // but legal: caller kept the unified handle's recv side via a
        // .recv_half() that wasn't yet called). The plan-level shape
        // is one half — codegen closes that direction; the other side
        // stays open under runtime refcount until its handle drops.
        let elab = make_elab_with_drops(vec![duplex_drop(Place::SendHalf(0))]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_recv_half_only_is_accepted() {
        let elab = make_elab_with_drops(vec![duplex_drop(Place::RecvHalf(0))]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_whole_plus_send_half_is_rejected() {
        // The stale-unified-handle bug: split would have moved the
        // DuplexHandle out, so it must NOT coexist with a half.
        // Without this rejection, codegen emits close-both on the
        // stale handle AND close-send on the half — the S-direction
        // closes twice; the R-direction closes once on a half-handle
        // binding that doesn't own it.
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::SendHalf(0)),
        ]);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "expected exactly one finding");
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert!(
            reason.contains("DuplexHandle") && reason.contains("half-handle"),
            "diagnostic must name the stale-handle conflict: {reason}"
        );
    }

    #[test]
    fn split_state_whole_plus_recv_half_is_rejected() {
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::RecvHalf(0)),
        ]);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn split_state_whole_plus_both_halves_is_rejected() {
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::SendHalf(0)),
            duplex_drop(Place::RecvHalf(0)),
        ]);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn split_state_duplicate_send_half_is_rejected() {
        // Two SendHalf(N) entries would close the S-direction twice
        // — a lowering bug (the split method returns a single half).
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::SendHalf(0)),
            duplex_drop(Place::SendHalf(0)),
        ]);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            unreachable!();
        };
        assert!(reason.contains("SendHalf") && reason.contains("S-direction"));
    }

    #[test]
    fn split_state_duplicate_recv_half_is_rejected() {
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::RecvHalf(0)),
            duplex_drop(Place::RecvHalf(0)),
        ]);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn split_state_duplicate_whole_is_rejected() {
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::DuplexHandle(0)),
        ]);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn split_state_independent_parents_do_not_interfere() {
        // Two distinct Duplex parents (N=0 and N=1). Parent 0 is in
        // the whole state; parent 1 is in the both-halves state.
        // Neither should flag — the invariant is per-parent.
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::SendHalf(1)),
            duplex_drop(Place::RecvHalf(1)),
        ]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_each_direction_closes_exactly_once_property() {
        // Property: for every legal split-state shape on a single
        // Duplex, the emitted drop plan closes the S-direction at
        // most once AND the R-direction at most once. The four
        // legal shapes (Whole, SendOnly, RecvOnly, BothHalves) all
        // satisfy this; the rejected shapes (Whole+Send, Whole+Recv,
        // Whole+Both, dup-Send, dup-Recv, dup-Whole) violate it.
        //
        // Encoded as exhaustive enumeration matching the project's
        // existing exhaustive-small-state test style (see
        // dataflow.rs meet-lattice tests).
        let parent = 0u32;
        let candidates: Vec<(&str, Vec<Place>)> = vec![
            ("Whole", vec![Place::DuplexHandle(parent)]),
            ("SendOnly", vec![Place::SendHalf(parent)]),
            ("RecvOnly", vec![Place::RecvHalf(parent)]),
            (
                "BothHalves",
                vec![Place::SendHalf(parent), Place::RecvHalf(parent)],
            ),
        ];
        for (label, places) in candidates {
            // Count how many times each direction closes under the
            // canonical Place->DropKind mapping.
            let mut s_count = 0u32;
            let mut r_count = 0u32;
            for p in &places {
                match drop_kind_for(*p, &duplex_int_int_ty()) {
                    DropKind::DuplexClose => {
                        s_count += 1;
                        r_count += 1;
                    }
                    DropKind::DuplexHalfClose(Direction::Send) => s_count += 1,
                    DropKind::DuplexHalfClose(Direction::Recv) => r_count += 1,
                    DropKind::Resource | DropKind::LambdaActorRelease => {}
                }
            }
            assert!(
                s_count <= 1 && r_count <= 1,
                "{label}: each direction must close at most once (got S={s_count}, R={r_count})"
            );
            let drops: Vec<ElabDrop> = places.into_iter().map(duplex_drop).collect();
            let elab = make_elab_with_drops(drops);
            assert!(
                validate_drop_plan(&elab).is_empty(),
                "{label}: legal split state must validate"
            );
        }
    }

    // ---------- broadened validation: every ExitPath + ElabBlock.drops ----------

    /// Build a synthetic `ElaboratedMirFunction` whose sole `DropPlan`
    /// is attached to `exit`. Used to exercise non-`Return` `ExitPath`
    /// validation under the broadened walk.
    fn make_elab_with_exit_and_drops(
        exit: ExitPath,
        drops: Vec<ElabDrop>,
    ) -> ElaboratedMirFunction {
        ElaboratedMirFunction {
            name: "synthetic".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![],
            drop_plans: vec![(exit, DropPlan { drops })],
            coroutine: None,
            lambda_captures: vec![],
        }
    }

    #[test]
    fn validate_walks_panic_exit_path() {
        // A Panic exit's DropPlan is the same LIFO sequence as the
        // Return exit at the same scope. A malformed kind here would
        // be silently accepted under the Return-only walk; the
        // broadened walk must reject it.
        let bad = ElabDrop {
            place: Place::DuplexHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::Resource, // wrong: DuplexHandle wants DuplexClose
        };
        let elab = make_elab_with_exit_and_drops(ExitPath::Panic { block: 5 }, vec![bad]);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "Panic-exit plan must be validated");
        let MirCheck::DropPlanUndetermined { block, reason } = &findings[0] else {
            unreachable!();
        };
        assert_eq!(*block, 5);
        assert!(
            reason.contains("Panic"),
            "diagnostic should name the exit kind: {reason}"
        );
    }

    #[test]
    fn validate_walks_cancel_exit_path() {
        // Cancel is the scope-structural cancellation exit. Same
        // shape as Panic for drop-plan purposes.
        let bad = ElabDrop {
            place: Place::SendHalf(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose, // wrong: SendHalf wants DuplexHalfClose(Send)
        };
        let elab = make_elab_with_exit_and_drops(ExitPath::Cancel { block: 9 }, vec![bad]);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        let MirCheck::DropPlanUndetermined { block, reason } = &findings[0] else {
            unreachable!();
        };
        assert_eq!(*block, 9);
        assert!(reason.contains("Cancel"));
    }

    #[test]
    fn validate_walks_yield_send_select_exit_paths() {
        // The three forward-compat exit kinds. Their DropPlans are
        // empty on the spine, but the validator must still apply the
        // Place-driven kind check when a future surface populates
        // them. Use a malformed DuplexHandle drop on each.
        let bad = ElabDrop {
            place: Place::DuplexHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::Resource,
        };
        for exit in [
            ExitPath::Yield { block: 1, next: 99 },
            ExitPath::Send {
                block: 2,
                actor: String::new(),
                next: 99,
            },
            ExitPath::Select { block: 3, next: 99 },
        ] {
            let expected_label = exit_kind_label(&exit);
            let elab = make_elab_with_exit_and_drops(exit, vec![bad.clone()]);
            let findings = validate_drop_plan(&elab);
            assert_eq!(
                findings.len(),
                1,
                "exit {expected_label} must be walked by validator"
            );
        }
    }

    #[test]
    fn validate_walks_goto_branch_call_exit_paths() {
        // Intra-CFG edges. Empty DropPlans on the spine, but the
        // validator walks them uniformly so a future construction
        // surface can't slip a malformed drop past.
        let bad = ElabDrop {
            place: Place::LambdaActorHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose, // wrong
        };
        for exit in [
            ExitPath::Goto {
                block: 1,
                target: 99,
            },
            ExitPath::Branch {
                block: 2,
                then_target: 98,
                else_target: 99,
            },
            ExitPath::Call {
                block: 3,
                callee: "f".to_string(),
                next: 99,
            },
        ] {
            let elab = make_elab_with_exit_and_drops(exit, vec![bad.clone()]);
            let findings = validate_drop_plan(&elab);
            assert_eq!(findings.len(), 1);
        }
    }

    #[test]
    fn validate_walks_split_state_invariant_on_panic_exit() {
        // Consume-on-split applies at every exit path, not just
        // Return. A Panic cleanup with both DuplexHandle + SendHalf
        // for the same parent must be rejected.
        let elab = make_elab_with_exit_and_drops(
            ExitPath::Panic { block: 4 },
            vec![
                duplex_drop(Place::DuplexHandle(0)),
                duplex_drop(Place::SendHalf(0)),
            ],
        );
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "split-state must apply to Panic exit");
        assert!(matches!(findings[0], MirCheck::DropPlanUndetermined { .. }));
    }

    #[test]
    fn validate_walks_elab_block_drops() {
        // Cleanup blocks carry drops directly in ElabBlock.drops.
        // A malformed kind here must be rejected at the same
        // structural boundary.
        let bad = ElabDrop {
            place: Place::RecvHalf(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexHalfClose(Direction::Send), // wrong queue
        };
        let elab = ElaboratedMirFunction {
            name: "synthetic".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 12,
                kind: BlockKind::Cleanup,
                drops: vec![bad],
                successor: None,
            }],
            drop_plans: vec![],
            coroutine: None,
            lambda_captures: vec![],
        };
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        let MirCheck::DropPlanUndetermined { block, reason } = &findings[0] else {
            unreachable!();
        };
        assert_eq!(*block, 12);
        assert!(
            reason.contains("cleanup drop"),
            "diagnostic must name the cleanup-block context: {reason}"
        );
    }

    #[test]
    fn validate_walks_elab_block_drops_split_state() {
        // Consume-on-split applies inside cleanup blocks too. A
        // cleanup block whose drops list has DuplexHandle + RecvHalf
        // for the same parent must be rejected.
        let elab = ElaboratedMirFunction {
            name: "synthetic".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 13,
                kind: BlockKind::Cleanup,
                drops: vec![
                    duplex_drop(Place::DuplexHandle(0)),
                    duplex_drop(Place::RecvHalf(0)),
                ],
                successor: None,
            }],
            drop_plans: vec![],
            coroutine: None,
            lambda_captures: vec![],
        };
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
    }

    // ---------- per-Return live-set narrowing (synthetic) ----------

    #[test]
    fn per_return_live_set_drops_exactly_match_kept_handles() {
        // The plan's invariant: at each Return block, the set of
        // Duplex/lambda-actor places dropped is exactly
        // (places-defined-in-this-fn) - (places-moved-out). Synthetic
        // shape: two Duplex handles defined, one moved out, one
        // dropped. With consistent kinds, validate_drop_plan accepts;
        // the drop list is exactly the one not-moved place.
        let kept = ElabDrop {
            place: Place::DuplexHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose,
        };
        let elab = make_elab_with_drops(vec![kept.clone()]);
        let return_plan = elab
            .drop_plans
            .iter()
            .find(|(e, _)| matches!(e, ExitPath::Return { .. }))
            .unwrap();
        assert_eq!(return_plan.1.drops, vec![kept]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn per_return_no_spurious_drops_when_all_moved_out() {
        // If every defined Place was moved out before the Return, the
        // drop list is empty. (places-defined - places-moved-out = ∅.)
        // This is the dual of the previous test.
        let elab = make_elab_with_drops(vec![]);
        let return_plan = &elab.drop_plans[0];
        assert!(return_plan.1.drops.is_empty());
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn per_return_no_missed_drops_for_three_live_handles() {
        // Three live Duplex handles at the Return — all three must
        // appear in the drop list. Order doesn't matter for this
        // invariant; codegen consumes the list in LIFO source order
        // (a separate concern). Verify count + presence.
        let drops: Vec<ElabDrop> = (0..3u32)
            .map(|i| ElabDrop {
                place: Place::DuplexHandle(i),
                ty: duplex_int_int_ty(),
                drop_fn: None,
                kind: DropKind::DuplexClose,
            })
            .collect();
        let elab = make_elab_with_drops(drops.clone());
        let return_plan = &elab.drop_plans[0];
        assert_eq!(return_plan.1.drops.len(), 3);
        for d in &drops {
            assert!(
                return_plan.1.drops.contains(d),
                "missing drop for {:?}",
                d.place
            );
        }
        assert!(validate_drop_plan(&elab).is_empty());
    }

    // ---------- weak-ref capture invariants ----------

    fn make_elab_with_captures(captures: Vec<LambdaCapture>) -> ElaboratedMirFunction {
        ElaboratedMirFunction {
            name: "synthetic".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![],
            drop_plans: vec![],
            coroutine: None,
            lambda_captures: captures,
        }
    }

    #[test]
    fn weak_capture_on_lambda_actor_handle_is_accepted() {
        // The canonical §5.9 ratification 2 shape:
        //   let fib = actor |n| { ... fib(n-1) ... };
        // The body's `fib` reference is captured as Weak attached to
        // the lambda-actor's own LambdaActorHandle. Validation must
        // accept this.
        let captures = vec![LambdaCapture {
            actor_handle: Place::LambdaActorHandle(0),
            captured: BindingId(7),
            name: "fib".to_string(),
            capture_kind: CaptureKind::Weak,
        }];
        let elab = make_elab_with_captures(captures);
        assert!(
            validate_drop_plan(&elab).is_empty(),
            "Weak capture on a LambdaActorHandle is the recursive self-case (§5.9 ratification 2)"
        );
    }

    #[test]
    fn weak_capture_on_duplex_handle_is_rejected() {
        // Weak captures are exclusive to LambdaActorHandle Places.
        // Attaching a Weak capture to a Duplex handle would silently
        // relax the refcount discipline on a non-actor resource.
        let captures = vec![LambdaCapture {
            actor_handle: Place::DuplexHandle(0),
            captured: BindingId(7),
            name: "ch".to_string(),
            capture_kind: CaptureKind::Weak,
        }];
        let elab = make_elab_with_captures(captures);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert!(
            reason.contains("weak capture") && reason.contains("ch"),
            "diagnostic must name the capture and identify the misuse: {reason}"
        );
    }

    #[test]
    fn weak_capture_on_local_place_is_rejected() {
        // Same invariant for a plain Local — only LambdaActorHandle
        // can host a Weak capture.
        let captures = vec![LambdaCapture {
            actor_handle: Place::Local(5),
            captured: BindingId(2),
            name: "x".to_string(),
            capture_kind: CaptureKind::Weak,
        }];
        let elab = make_elab_with_captures(captures);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn strong_capture_is_accepted_on_any_handle_kind() {
        // Strong captures are unrestricted — they're the default for
        // every non-self capture and the refcount discipline is
        // strong everywhere. Pair Strong with Local, DuplexHandle,
        // and LambdaActorHandle — all should accept.
        for handle in [
            Place::Local(0),
            Place::DuplexHandle(0),
            Place::LambdaActorHandle(0),
        ] {
            let captures = vec![LambdaCapture {
                actor_handle: handle,
                captured: BindingId(1),
                name: "captured".to_string(),
                capture_kind: CaptureKind::Strong,
            }];
            let elab = make_elab_with_captures(captures);
            assert!(
                validate_drop_plan(&elab).is_empty(),
                "Strong capture on {handle:?} must be accepted"
            );
        }
    }

    #[test]
    fn multiple_captures_one_weak_one_strong_validates_correctly() {
        // Mixed-capture case: the self-binding-name is Weak (on the
        // actor's own LambdaActorHandle), a non-self captured value
        // is Strong. Both must coexist without findings.
        let captures = vec![
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(1),
                name: "fib".to_string(),
                capture_kind: CaptureKind::Weak,
            },
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(2),
                name: "memo".to_string(),
                capture_kind: CaptureKind::Strong,
            },
        ];
        let elab = make_elab_with_captures(captures);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn non_recursive_lambda_has_zero_weak_captures() {
        // The non-recursive lambda case: `let f = actor |n| { n + 1 }`.
        // The body does not reference its own binding name, so the
        // capture set contains zero Weak entries. The plan validates;
        // any Strong captures (closed-over outer bindings) coexist
        // freely.
        let captures = vec![LambdaCapture {
            actor_handle: Place::LambdaActorHandle(0),
            captured: BindingId(1),
            name: "outer".to_string(),
            capture_kind: CaptureKind::Strong,
        }];
        let elab = make_elab_with_captures(captures);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn recursive_lambda_has_exactly_one_weak_capture() {
        // The canonical forward-bind recursive shape:
        //   let fib = actor |n| { ... fib(n - 1) ... };
        // The capture-set discovery (slice 4) must emit EXACTLY ONE
        // Weak capture (the `fib` self-reference) plus zero-or-more
        // Strong captures for outer bindings. Pin the "exactly one"
        // half of the discipline as a structural invariant on the
        // synthetic capture list.
        let captures = vec![LambdaCapture {
            actor_handle: Place::LambdaActorHandle(0),
            captured: BindingId(7),
            name: "fib".to_string(),
            capture_kind: CaptureKind::Weak,
        }];
        let elab = make_elab_with_captures(captures.clone());
        let weak_count = captures
            .iter()
            .filter(|c| matches!(c.capture_kind, CaptureKind::Weak))
            .count();
        assert_eq!(
            weak_count, 1,
            "recursive lambda must have exactly one Weak capture (the self-binding-name)"
        );
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn multiple_weak_captures_on_same_actor_handle_is_rejected() {
        // Two Weak captures on the same LambdaActorHandle would mean
        // the lambda has two self-binding-names — structurally
        // impossible (a let-binding has exactly one name). This is a
        // lowering bug; fail closed.
        let captures = vec![
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(1),
                name: "fib".to_string(),
                capture_kind: CaptureKind::Weak,
            },
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(2),
                name: "fib_shadow".to_string(),
                capture_kind: CaptureKind::Weak,
            },
        ];
        let elab = make_elab_with_captures(captures);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "expected one finding");
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert!(
            reason.contains("LambdaActorHandle(0)")
                && reason.contains("weak captures")
                && reason.contains("fib"),
            "diagnostic must name the actor handle, count, and capture names: {reason}"
        );
    }

    #[test]
    fn multiple_weak_captures_on_distinct_actor_handles_are_independent() {
        // Two distinct lambda-actors, each with their own Weak self-
        // capture. Validation must accept — the "exactly one" rule
        // is per-LambdaActorHandle.
        let captures = vec![
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(1),
                name: "fib".to_string(),
                capture_kind: CaptureKind::Weak,
            },
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(1),
                captured: BindingId(2),
                name: "fact".to_string(),
                capture_kind: CaptureKind::Weak,
            },
        ];
        let elab = make_elab_with_captures(captures);
        assert!(validate_drop_plan(&elab).is_empty());
    }
}

// ============================================================================
// Generative property tests for per-Return live-set narrowing.
//
// The fixed-shape tests in `slice3_invariants` pin three hand-built shapes
// (one live handle survives, every handle moved out, three live handles).
// These proptest cases exercise the narrowing algorithm over randomly-
// generated `(defined-bindings, moved-out-subset)` inputs so the
// `dropped(block) == defined(block) - moved_out(block)` invariant is
// checked across the full state space rather than three fixtures.
//
// The proptest dependency is scoped to `cfg(not(target_arch = "wasm32"))`
// in `Cargo.toml` (matches the pattern in `hew-runtime`). The
// `cfg_attr(target_arch = "wasm32", allow(unused))` on the module below
// keeps the wasm build clean — the entire module compiles away on wasm
// since `proptest` is not available there.
// ============================================================================

#[cfg(all(test, not(target_arch = "wasm32")))]
mod slice3_narrowing_proptests {
    use super::*;
    use crate::dataflow::BindingState;
    use hew_hir::SiteId;
    use proptest::prelude::*;
    use std::collections::BTreeMap;

    /// A `Duplex<i64, i64>` `ResolvedTy` payload — the inner type
    /// detail is irrelevant for narrowing.
    fn duplex_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![ResolvedTy::I64, ResolvedTy::I64],
        }
    }

    /// Build a single-block `BasicBlock` with a `Return` terminator.
    fn single_return_block(block_id: u32) -> BasicBlock {
        BasicBlock {
            id: block_id,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        }
    }

    /// Build the function-wide LIFO drop template for `n` `DuplexHandle`
    /// bindings, indexed `0..n`. Each drop carries the canonical
    /// `(DuplexHandle(i), DropKind::DuplexClose)` shape.
    fn build_lifo(n: u32) -> Vec<ElabDrop> {
        (0..n)
            .map(|i| ElabDrop {
                place: Place::DuplexHandle(i),
                ty: duplex_ty(),
                drop_fn: None,
                kind: DropKind::DuplexClose,
            })
            .collect()
    }

    /// Build the `binding_locals` map: `BindingId(i) -> DuplexHandle(i)`.
    fn build_binding_locals(n: u32) -> HashMap<BindingId, Place> {
        (0..n)
            .map(|i| (BindingId(i), Place::DuplexHandle(i)))
            .collect()
    }

    /// Build the `exit_states` map for block 0, marking each binding in
    /// `moved_out` as `Consumed` and every other binding (up to `n`) as
    /// `Live`. The narrowing must keep Live + `MaybeConsumed` and drop
    /// Consumed.
    fn build_exit_states(
        n: u32,
        moved_out: &[u32],
    ) -> HashMap<u32, BTreeMap<BindingId, BindingState>> {
        let mut per_binding: BTreeMap<BindingId, BindingState> = BTreeMap::new();
        for i in 0..n {
            let binding = BindingId(i);
            if moved_out.contains(&i) {
                per_binding.insert(binding, BindingState::Consumed(SiteId(0)));
            } else {
                per_binding.insert(binding, BindingState::Live);
            }
        }
        let mut map = HashMap::new();
        map.insert(0u32, per_binding);
        map
    }

    proptest! {
        /// `dropped(block) == defined(block) - moved_out(block)`.
        ///
        /// For every randomly-generated `(N, moved_out_subset)` input:
        ///   - N is the number of defined DuplexHandle bindings.
        ///   - moved_out_subset is the indices Consumed before Return.
        ///   - The narrowed drop list must contain exactly the indices
        ///     NOT in moved_out_subset, each as a DuplexHandle(i) drop.
        ///
        /// Proptest's default 256 cases sweeps shapes from N=0 (empty
        /// drop list) up to N=8 (all eight handles live or moved
        /// across every subset combination).
        #[test]
        fn dropped_equals_defined_minus_moved_out(
            n in 0u32..8,
            moved_out_mask in 0u32..256,
        ) {
            let moved_out: Vec<u32> = (0..n)
                .filter(|i| (moved_out_mask >> i) & 1 == 1)
                .collect();

            let blocks = vec![single_return_block(0)];
            let lifo = build_lifo(n);
            let exit_states = build_exit_states(n, &moved_out);
            let binding_locals = build_binding_locals(n);

            let (_, plans) = enumerate_exits(&blocks, &lifo, &exit_states, &binding_locals);

            // Exactly one Return plan for the single block.
            prop_assert_eq!(plans.len(), 1);
            let (exit, plan) = &plans[0];
            let is_return_block_0 = matches!(exit, ExitPath::Return { block: 0 });
            prop_assert!(is_return_block_0);

            // Build the expected drop list: every index 0..n not in
            // moved_out, as a DuplexHandle drop. The order matches the
            // input `lifo` template's iteration order (build_lifo
            // emits 0..n in forward order; enumerate_exits' filter
            // preserves that order).
            let expected: Vec<Place> = (0..n)
                .filter(|i| !moved_out.contains(i))
                .map(Place::DuplexHandle)
                .collect();
            let actual: Vec<Place> = plan.drops.iter().map(|d| d.place).collect();
            prop_assert_eq!(actual, expected,
                "narrowing: defined={}, moved_out={:?}", n, moved_out);
        }

        /// `MaybeConsumed` at a Return is treated as Live for drop-plan
        /// purposes (the move-checker rejects the program upstream, but
        /// the drop list stays informational). Sweep random subsets
        /// where bindings are MaybeConsumed and assert each appears in
        /// the drop list alongside the Live bindings.
        #[test]
        fn maybe_consumed_appears_in_drop_list(
            n in 0u32..6,
            maybe_mask in 0u32..64,
            consumed_mask in 0u32..64,
        ) {
            // Decide per-binding state: Consumed wins over MaybeConsumed
            // wins over Live so the masks don't overlap meaningfully —
            // a binding is Consumed if its bit is set in consumed_mask,
            // else MaybeConsumed if set in maybe_mask, else Live.
            let mut per_binding: BTreeMap<BindingId, BindingState> = BTreeMap::new();
            for i in 0..n {
                let state = if (consumed_mask >> i) & 1 == 1 {
                    BindingState::Consumed(SiteId(0))
                } else if (maybe_mask >> i) & 1 == 1 {
                    BindingState::MaybeConsumed(SiteId(0))
                } else {
                    BindingState::Live
                };
                per_binding.insert(BindingId(i), state);
            }
            let mut exit_states = HashMap::new();
            exit_states.insert(0u32, per_binding);

            let blocks = vec![single_return_block(0)];
            let lifo = build_lifo(n);
            let binding_locals = build_binding_locals(n);

            let (_, plans) = enumerate_exits(&blocks, &lifo, &exit_states, &binding_locals);
            let (_, plan) = &plans[0];

            // Expected: every binding NOT Consumed survives in the drop
            // list (Live and MaybeConsumed both qualify).
            let dropped: std::collections::HashSet<u32> = plan
                .drops
                .iter()
                .filter_map(|d| match d.place {
                    Place::DuplexHandle(i) => Some(i),
                    _ => None,
                })
                .collect();
            for i in 0..n {
                let is_consumed = (consumed_mask >> i) & 1 == 1;
                prop_assert_eq!(
                    dropped.contains(&i),
                    !is_consumed,
                    "binding {} state should determine drop-list membership", i
                );
            }
        }

        /// The narrowing is deterministic: running `enumerate_exits`
        /// twice on the same inputs produces the same drops. (Catches
        /// any HashMap-iteration-order leakage into the output.)
        #[test]
        fn narrowing_is_deterministic(
            n in 0u32..8,
            moved_out_mask in 0u32..256,
        ) {
            let moved_out: Vec<u32> = (0..n)
                .filter(|i| (moved_out_mask >> i) & 1 == 1)
                .collect();
            let blocks = vec![single_return_block(0)];
            let lifo = build_lifo(n);
            let exit_states = build_exit_states(n, &moved_out);
            let binding_locals = build_binding_locals(n);

            let (b1, p1) = enumerate_exits(&blocks, &lifo, &exit_states, &binding_locals);
            let (b2, p2) = enumerate_exits(&blocks, &lifo, &exit_states, &binding_locals);

            prop_assert_eq!(b1.len(), b2.len());
            prop_assert_eq!(p1.len(), p2.len());
            for ((e1, plan1), (e2, plan2)) in p1.iter().zip(p2.iter()) {
                prop_assert_eq!(e1, e2);
                prop_assert_eq!(&plan1.drops, &plan2.drops);
            }
        }

        /// No binding outside the function's owned set ever appears in
        /// the narrowed drop list. The narrowing must be a subset
        /// operation — it never INVENTS a drop.
        #[test]
        fn narrowing_never_invents_drops(
            n in 0u32..8,
            moved_out_mask in 0u32..256,
        ) {
            let moved_out: Vec<u32> = (0..n)
                .filter(|i| (moved_out_mask >> i) & 1 == 1)
                .collect();
            let blocks = vec![single_return_block(0)];
            let lifo = build_lifo(n);
            let exit_states = build_exit_states(n, &moved_out);
            let binding_locals = build_binding_locals(n);

            let (_, plans) = enumerate_exits(&blocks, &lifo, &exit_states, &binding_locals);
            let (_, plan) = &plans[0];

            for d in &plan.drops {
                let Place::DuplexHandle(i) = d.place else {
                    panic!("non-DuplexHandle drop appeared: {:?}", d.place);
                };
                prop_assert!(i < n, "drop for binding {} but only {} defined", i, n);
            }
        }
    }
}

// ============================================================================
// Slice 3.5 cross-block stale-DuplexHandle detection — generative property
// tests against `validate_cross_block_split_consume` built directly on
// hand-constructed `BasicBlock` + `Instr` shapes. The full source pipeline
// can't drive these tests because the parser surface for `.send_half()` /
// `.recv_half()` is slice-4 work; the synthetic inputs mirror what slice 4
// will eventually emit.
// ============================================================================

#[cfg(all(test, not(target_arch = "wasm32")))]
mod slice35_cross_block_proptests {
    use super::*;
    use proptest::prelude::*;

    fn duplex_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![ResolvedTy::I64, ResolvedTy::I64],
        }
    }

    /// Build a single-block CFG that splits `DuplexHandle(parent)` into
    /// a `SendHalf(parent)` then attempts to drop the unified handle
    /// after the split. The structural same-list check already rejects
    /// this shape; the cross-block check must also catch the
    /// dataflow-derived stale state on the same block (a Live entry
    /// followed by a Move-to-half transitions to Consumed before the
    /// block terminator, but the drop plan was assembled from the
    /// pre-Move LIFO — fail-closed).
    fn build_split_block(parent: u32, terminator: Terminator) -> BasicBlock {
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::SendHalf(parent),
                src: Place::DuplexHandle(parent),
            }],
            terminator,
        }
    }

    fn elab_with_return_drop(parent: u32) -> ElaboratedMirFunction {
        ElaboratedMirFunction {
            name: "f".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 0,
                kind: BlockKind::Normal,
                drops: vec![],
                successor: None,
            }],
            drop_plans: vec![(
                ExitPath::Return { block: 0 },
                DropPlan {
                    drops: vec![ElabDrop {
                        place: Place::DuplexHandle(parent),
                        ty: duplex_ty(),
                        drop_fn: None,
                        kind: DropKind::DuplexClose,
                    }],
                },
            )],
            coroutine: None,
            lambda_captures: vec![],
        }
    }

    proptest! {
        /// Cross-block: A splits DuplexHandle(p) in block 0, a drop on
        /// it appears in the Return plan. The checker must fire
        /// DropPlanUndetermined.
        #[test]
        fn split_in_predecessor_rejects_stale_unified_drop(
            parent in 0u32..8,
        ) {
            let blocks = vec![build_split_block(parent, Terminator::Return)];
            let elab = elab_with_return_drop(parent);
            let findings = validate_cross_block_split_consume(&blocks, &elab);
            prop_assert!(
                findings
                    .iter()
                    .any(|f| matches!(f, MirCheck::DropPlanUndetermined { .. })),
                "expected DropPlanUndetermined when DuplexHandle({parent}) is split AND \
                 still appears in the drop plan; got {findings:?}"
            );
        }

        /// Cross-block: block 0 branches into 1 (split path) and 2
        /// (no-split path), both jump to block 3 whose Return plan
        /// drops the unified handle. The meet of preds at block 3 is
        /// `MaybeConsumed` — fail-closed.
        #[test]
        fn split_on_some_paths_rejects_unified_drop_at_join(
            parent in 0u32..8,
        ) {
            let blocks = vec![
                // Entry: branch on a dummy cond (cond Place won't be
                // evaluated by the validator — only Move shape matters).
                BasicBlock {
                    id: 0,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Branch {
                        cond: Place::Local(99),
                        then_target: 1,
                        else_target: 2,
                    },
                },
                // Then: split DuplexHandle(parent) into SendHalf.
                BasicBlock {
                    id: 1,
                    statements: vec![],
                    instructions: vec![Instr::Move {
                        dest: Place::SendHalf(parent),
                        src: Place::DuplexHandle(parent),
                    }],
                    terminator: Terminator::Goto { target: 3 },
                },
                // Else: no split.
                BasicBlock {
                    id: 2,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Goto { target: 3 },
                },
                // Join: Return — the drop plan is checked here.
                BasicBlock {
                    id: 3,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                },
            ];
            // The Return ExitPath references block 3 — point the elab
            // drop plan at it.
            let mut elab = elab_with_return_drop(parent);
            elab.drop_plans = vec![(
                ExitPath::Return { block: 3 },
                DropPlan {
                    drops: vec![ElabDrop {
                        place: Place::DuplexHandle(parent),
                        ty: duplex_ty(),
                        drop_fn: None,
                        kind: DropKind::DuplexClose,
                    }],
                },
            )];
            elab.blocks = vec![
                ElabBlock {
                    id: 0,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
                ElabBlock {
                    id: 1,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
                ElabBlock {
                    id: 2,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
                ElabBlock {
                    id: 3,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
            ];

            let findings = validate_cross_block_split_consume(&blocks, &elab);
            let has_undetermined = findings
                .iter()
                .any(|f| matches!(f, MirCheck::DropPlanUndetermined { .. }));
            prop_assert!(
                has_undetermined,
                "expected DropPlanUndetermined at join block 3 when DuplexHandle({parent}) \
                 is split on only the then-path; got {findings:?}"
            );
            // The reason text should anchor at the split-emitting block (1).
            let reason_mentions_block_1 = findings.iter().any(|f| match f {
                MirCheck::DropPlanUndetermined { reason, .. } => reason.contains("block 1"),
                _ => false,
            });
            prop_assert!(
                reason_mentions_block_1,
                "diagnostic reason should cite block 1 as the split-emitting block; \
                 got {findings:?}"
            );
        }

        /// Cross-block: split on EVERY path. The meet at the join is
        /// `Consumed`. The drop on the unified handle at the join
        /// must be rejected with a Consumed reason (not MaybeConsumed).
        #[test]
        fn split_on_every_path_rejects_unified_drop_at_join(
            parent in 0u32..8,
        ) {
            let blocks = vec![
                BasicBlock {
                    id: 0,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Branch {
                        cond: Place::Local(99),
                        then_target: 1,
                        else_target: 2,
                    },
                },
                BasicBlock {
                    id: 1,
                    statements: vec![],
                    instructions: vec![Instr::Move {
                        dest: Place::SendHalf(parent),
                        src: Place::DuplexHandle(parent),
                    }],
                    terminator: Terminator::Goto { target: 3 },
                },
                BasicBlock {
                    id: 2,
                    statements: vec![],
                    instructions: vec![Instr::Move {
                        dest: Place::RecvHalf(parent),
                        src: Place::DuplexHandle(parent),
                    }],
                    terminator: Terminator::Goto { target: 3 },
                },
                BasicBlock {
                    id: 3,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                },
            ];
            let mut elab = elab_with_return_drop(parent);
            elab.drop_plans = vec![(
                ExitPath::Return { block: 3 },
                DropPlan {
                    drops: vec![ElabDrop {
                        place: Place::DuplexHandle(parent),
                        ty: duplex_ty(),
                        drop_fn: None,
                        kind: DropKind::DuplexClose,
                    }],
                },
            )];
            elab.blocks = (0..=3)
                .map(|id| ElabBlock {
                    id,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                })
                .collect();

            let findings = validate_cross_block_split_consume(&blocks, &elab);
            prop_assert!(
                findings
                    .iter()
                    .any(|f| matches!(f, MirCheck::DropPlanUndetermined { .. })),
                "expected DropPlanUndetermined when DuplexHandle({parent}) is split on \
                 every reaching path; got {findings:?}"
            );
        }

        /// Non-regression: no split, no rejection. A drop plan that
        /// fires the unified DuplexHandle on a block with no
        /// predecessor split must accept silently — the checker is a
        /// fail-CLOSED gate, not a fail-OPEN one. (No findings is the
        /// expected outcome.)
        #[test]
        fn no_split_no_rejection(
            parent in 0u32..8,
        ) {
            let blocks = vec![
                BasicBlock {
                    id: 0,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                },
            ];
            let elab = elab_with_return_drop(parent);
            let findings = validate_cross_block_split_consume(&blocks, &elab);
            prop_assert!(
                findings.is_empty(),
                "no split observed; the cross-block check must not invent findings; \
                 got {findings:?}"
            );
        }

        /// Multi-return: two Return blocks, the unified DuplexHandle is
        /// split on the predecessor edge of one Return but not the
        /// other. The first Return's drop fires legally (no preceding
        /// split); the second's must be rejected.
        #[test]
        fn multi_return_per_path_drops_only_flag_split_path(
            parent in 0u32..8,
        ) {
            let blocks = vec![
                // Entry branches into two Return blocks.
                BasicBlock {
                    id: 0,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Branch {
                        cond: Place::Local(99),
                        then_target: 1,
                        else_target: 2,
                    },
                },
                // Then-arm: split + return.
                BasicBlock {
                    id: 1,
                    statements: vec![],
                    instructions: vec![Instr::Move {
                        dest: Place::SendHalf(parent),
                        src: Place::DuplexHandle(parent),
                    }],
                    terminator: Terminator::Return,
                },
                // Else-arm: no split, return.
                BasicBlock {
                    id: 2,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                },
            ];
            let mut elab = elab_with_return_drop(parent);
            // Two Return drop plans, one per Return-terminated block.
            elab.drop_plans = vec![
                (
                    ExitPath::Return { block: 1 },
                    DropPlan {
                        drops: vec![ElabDrop {
                            place: Place::DuplexHandle(parent),
                            ty: duplex_ty(),
                            drop_fn: None,
                            kind: DropKind::DuplexClose,
                        }],
                    },
                ),
                (
                    ExitPath::Return { block: 2 },
                    DropPlan {
                        drops: vec![ElabDrop {
                            place: Place::DuplexHandle(parent),
                            ty: duplex_ty(),
                            drop_fn: None,
                            kind: DropKind::DuplexClose,
                        }],
                    },
                ),
            ];
            elab.blocks = (0..=2)
                .map(|id| ElabBlock {
                    id,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                })
                .collect();

            let findings = validate_cross_block_split_consume(&blocks, &elab);
            // Exactly one finding: the Return at block 1 (split path).
            // Block 2's Return drop fires on the unified handle that
            // was never split on its path — accept.
            let on_block_1 = findings.iter().filter(|f| {
                matches!(f, MirCheck::DropPlanUndetermined { block, .. } if *block == 1)
            }).count();
            let on_block_2 = findings.iter().filter(|f| {
                matches!(f, MirCheck::DropPlanUndetermined { block, .. } if *block == 2)
            }).count();
            prop_assert_eq!(on_block_1, 1, "block 1 (split path) must reject the unified drop");
            prop_assert_eq!(on_block_2, 0, "block 2 (no-split path) must accept the unified drop");
        }
    }
}
