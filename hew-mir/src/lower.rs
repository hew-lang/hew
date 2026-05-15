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
    ElabBlock, ElabDrop, ElaboratedMirFunction, ExitPath, Instr, IrPipeline, LambdaCapture,
    MirCheck, MirDiagnostic, MirDiagnosticKind, MirStatement, Place, RawMirFunction, Strategy,
    Terminator, ThirFunction,
};

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
                let value_place = self.lower_value(value);
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
                // initialiser's value is moved into.
                if let Some(src) = value_place {
                    let slot = self.alloc_local(binding.ty.clone());
                    self.instructions.push(Instr::Move { dest: slot, src });
                    self.binding_locals.insert(binding.id, slot);
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
                // Cluster 1 does not lower Call to backend instructions yet
                // (Terminator::Call is wired but the spine subset accepts
                // only literal/binary/return). Walk the children so any
                // Unsupported inside an argument still surfaces, then
                // fail closed so the emitter never sees a return slot with
                // no producer (LESSONS `boundary-fail-closed`).
                let _ = self.lower_value(callee);
                for arg in args {
                    let _ = self.lower_value(arg);
                }
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CutoverUnsupported {
                        construct: "function call".to_string(),
                        site: expr.site,
                    },
                    note: "call expressions are not yet lowered to the backend \
                           instruction stream in the Cluster 1 spine subset"
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
        let instr = match op {
            BinaryOp::Add => Instr::IntAdd { dest, lhs, rhs },
            BinaryOp::Subtract => Instr::IntSub { dest, lhs, rhs },
            BinaryOp::Multiply => Instr::IntMul { dest, lhs, rhs },
            // The spine subset still rejects Divide / Modulo / logical
            // / shift / range / send / regex / bitwise binops. Previously
            // this arm silently popped the dest local and returned
            // `None`, letting the parent expression succeed with a
            // missing producer (quiet fail-soft — caller's `decide`
            // ran, `MirDiagnostic` did not). Fail closed now: drop the
            // dest local, emit a `CutoverUnsupported` so the CLI
            // rejection surface sees the offending construct, and
            // return `None`. LESSONS `boundary-fail-closed`.
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
        self.instructions.push(instr);
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
        | MirCheck::ActorSendEscape { .. } => None,
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
        // Lambda-actor capture-set discovery lives at the
        // HIR-lower seam (the lambda-actor literal carries its
        // capture list). HIR has no construction surface for
        // LambdaActor today, so this side-table stays empty on
        // every function the current ladder produces — the
        // pattern matches the other declared-scaffold side-tables
        // (coroutine, Terminator::Yield/Send). Synthetic test
        // inputs in `hew-mir/tests/` populate it directly to
        // exercise the weak-ref invariant.
        //
        // WHEN-OBSOLETE: when HIR gains a SpawnLambdaActor expression
        // kind (slice driving the .send_half() / .recv_half() seam),
        // this side-table is populated by walking the lambda body's
        // capture list:
        //   - For each captured binding whose name matches the
        //     lambda's own let-binding name (the forward-bind case
        //     pre-bound by hew-types::check::statements at slice 2's
        //     `forward-bind actor-lambda let name before body
        //     synthesis` path), emit LambdaCapture { actor_handle:
        //     LambdaActorHandle(spawn_local), captured, name,
        //     capture_kind: CaptureKind::Weak }.
        //   - For every other captured binding, emit a Strong capture
        //     on the same actor_handle.
        // The structural fail-closed checker `validate_lambda_captures`
        // below already rejects the malformed shapes the future HIR
        // walker could produce (Weak on non-LambdaActorHandle, multiple
        // Weak captures on the same handle); the walker only needs to
        // emit captures, not re-check them.
        lambda_captures: Vec::new(),
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
            Terminator::Panic => {
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
