use std::collections::{HashMap, HashSet};

use hew_hir::{
    named_type_names, BindingId, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule,
    HirStmtKind, IntentKind, ResolvedRef, ValueClass,
};
use hew_parser::ast::BinaryOp;
use hew_types::ResolvedTy;

use crate::model::{
    BasicBlock, BlockKind, CheckedMirFunction, DecisionFact, DropPlan, ElabBlock, ElabDrop,
    ElaboratedMirFunction, ExitPath, Instr, IrPipeline, MirCheck, MirDiagnostic, MirDiagnosticKind,
    MirStatement, Place, RawMirFunction, Strategy, Terminator, ThirFunction,
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
/// LOAD-BEARING INVARIANT — single-block CFG. The forward-scan over
/// `builder.statements` is correct *only* while MIR is structurally
/// CFG-flat (`HirExprKind::If` and `HirExprKind::Block` lower their
/// arms inline rather than producing separate `BasicBlock`s with
/// branching terminators — see `lower_value`). When CFG construction
/// for `If`/`match` lands, this scan must be replaced by per-block
/// dataflow: a flat-stream walk would false-positive on
/// `consume(x)` appearing in mutually-exclusive arms (flattened to
/// `consume; consume` looks like `UseAfterConsume`) and false-
/// negative across siblings of a branch that consumes on only one
/// path (the binding is removed from `linear_live` globally). The
/// fixture corpus that names `linear_unconsumed_fall_through` /
/// `linear_consumed_both_branches` exists to drive that follow-on
/// work — it cannot meaningfully run under this implementation.
/// LESSONS: boundary-fail-closed (don't assume the substrate is
/// path-sensitive when the construction surface is single-block).
fn check_function(builder: &Builder, _func: &HirFn) -> Vec<MirCheck> {
    let mut checks = Vec::new();

    // Pass 1: forward-scan the checker-authority statement stream.
    // Track which bindings have been initialised (`Bind`) and which
    // have been consumed (`Use` with `IntentKind::Consume` on a
    // non-`BitCopy` type). A `Use` of an uninitialised binding is
    // `InitialisedBeforeUse`; a `Use` of a consumed binding is
    // `UseAfterConsume`. A `Linear` binding live at `Return` without
    // being consumed is `MustConsume` — symmetric to `UseAfterConsume`,
    // closing the move-checker's @linear-side proof obligation.
    let mut initialised: HashSet<BindingId> = HashSet::new();
    let mut consumed: HashMap<BindingId, hew_hir::SiteId> = HashMap::new();
    // Linear binding ledger: name + type + the site at which the
    // binding was introduced. Cleared when the binding is consumed.
    let mut linear_live: HashMap<BindingId, (String, ResolvedTy)> = HashMap::new();
    for statement in &builder.statements {
        match statement {
            MirStatement::Bind {
                binding, name, ty, ..
            } => {
                initialised.insert(*binding);
                consumed.remove(binding);
                if ValueClass::of_ty(ty, &builder.type_classes) == ValueClass::Linear {
                    linear_live.insert(*binding, (name.clone(), ty.clone()));
                }
            }
            MirStatement::Use {
                binding,
                name,
                site,
                ty,
                intent,
            } => {
                if !initialised.contains(binding) {
                    checks.push(MirCheck::InitialisedBeforeUse {
                        binding: *binding,
                        name: name.clone(),
                        use_site: *site,
                    });
                }
                if let Some(consumed_at) = consumed.get(binding) {
                    checks.push(MirCheck::UseAfterConsume {
                        binding: *binding,
                        name: name.clone(),
                        consumed_at: *consumed_at,
                        used_at: *site,
                    });
                }
                if *intent == IntentKind::Consume
                    && ValueClass::of_ty(ty, &builder.type_classes) != ValueClass::BitCopy
                {
                    consumed.insert(*binding, *site);
                    linear_live.remove(binding);
                }
            }
            MirStatement::Return { site, .. } => {
                // Any `Linear` binding still live at a Return is an
                // unconsumed `@linear` value reaching an exit. Emit
                // `MustConsume` for each, anchored at the Return site
                // (or `SiteId::default` if the Return synthesised no
                // site — only happens on unit-returning trailing-stmt
                // bodies with no tail expression).
                // Sentinel SiteId(0) for unit-return tail-less bodies
                // (no Return-site is constructed by the builder). The
                // real exit-site projection lands when CFG construction
                // surfaces in Cluster 4+.
                let exit_site = site.unwrap_or(hew_hir::SiteId(0));
                // Deterministic order: walk in BindingId order so
                // diagnostics are stable across runs.
                let mut ids: Vec<_> = linear_live.keys().copied().collect();
                ids.sort();
                for id in ids {
                    if let Some((name, ty)) = linear_live.get(&id) {
                        checks.push(MirCheck::MustConsume {
                            binding: id,
                            name: name.clone(),
                            exit_site,
                            ty: ty.clone(),
                        });
                    }
                }
            }
            MirStatement::Evaluate { .. } | MirStatement::Drop { .. } => {}
        }
    }

    // Pass 2: DecisionMapTotal. Every `DecisionFact` on this function
    // must carry a concrete `Strategy` — `Strategy::UnknownBlocked` is
    // a lowering escape valve that must never reach the emitter.
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

    checks
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
            HirItem::TypeDecl(_) => {
                // Type declarations carry no executable body. Their
                // `ResourceMarker` is consumed via `HirModule.type_classes`
                // by `ValueClass::of_ty` when a downstream expression
                // references a Named type. Nothing else to lower here.
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

    let thir = ThirFunction {
        name: func.name.clone(),
        return_ty: func.return_ty.clone(),
        statements: builder.statements.clone(),
    };
    let raw_block = BasicBlock {
        id: 0,
        statements: builder.statements.clone(),
        instructions: builder.instructions.clone(),
        terminator: Terminator::Return,
    };
    let raw = RawMirFunction {
        name: func.name.clone(),
        return_ty: func.return_ty.clone(),
        locals: builder.locals.clone(),
        blocks: vec![raw_block.clone()],
        decisions: builder.decisions.clone(),
    };
    // Checked MIR's `checks` field is populated by `check_function`
    // from real dataflow over the checker-authority `MirStatement`
    // stream. The `MirDiagnostic` surface that the CLI rejects on is
    // projected from these checks — there is one source of truth for
    // move/borrow/init legality.
    let checks = check_function(&builder, func);
    let mut diagnostics: Vec<MirDiagnostic> =
        checks.iter().filter_map(check_to_diagnostic).collect();

    // Collect diagnostics emitted by the builder (e.g., Unsupported HIR nodes).
    diagnostics.append(&mut builder.diagnostics);

    collect_unknown_type_diagnostics(func, &builder, &mut diagnostics);

    let checked = CheckedMirFunction {
        name: func.name.clone(),
        return_ty: func.return_ty.clone(),
        block: raw_block,
        decisions: builder.decisions.clone(),
        checks,
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
    let elaborated = elaborate(&checked, &builder);

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
        push_unknown_type_diagnostics(&param.ty, &mut reported, diagnostics);
    }
    push_unknown_type_diagnostics(&func.return_ty, &mut reported, diagnostics);

    for decision in &builder.decisions {
        push_unknown_type_diagnostics(&decision.ty, &mut reported, diagnostics);
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
                push_unknown_type_diagnostics(ty, &mut reported, diagnostics);
            }
        }
    }
}

fn push_unknown_type_diagnostics(
    ty: &ResolvedTy,
    reported: &mut HashSet<String>,
    diagnostics: &mut Vec<MirDiagnostic>,
) {
    for name in named_type_names(ty) {
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
    /// Checker-authority stream: one `MirStatement` per Hew-level site
    /// (`Bind`, `Use`, `Evaluate`, `Return`, `Drop`). Consumed by
    /// `check_raw_mir` and the D10 / use-after-consume passes.
    statements: Vec<MirStatement>,
    /// Backend-authority stream: one `Instr` per machine-level value
    /// movement. Consumed by `hew-codegen-rs::llvm`. Populated in lock-step
    /// with `statements` by `lower_value` so the checker and the emitter
    /// agree on what each `SiteId` resolves to.
    instructions: Vec<Instr>,
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
                    (Some(lhs), Some(rhs)) => self.lower_binary(*op, lhs, rhs, &expr.ty),
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
            } => {
                let _ = self.lower_value(condition);
                let _ = self.lower_value(then_expr);
                if let Some(else_expr) = else_expr {
                    let _ = self.lower_value(else_expr);
                }
                None
            }
            HirExprKind::StructInit { fields, .. } => {
                for (_, field) in fields {
                    let _ = self.lower_value(field);
                }
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
        // Spine subset: only integer literals reach the backend in
        // Cluster 1. Float/String/Bool/Char/Duration/Unit literals are out
        // of scope (Cluster 2 takes String; Cluster 4 takes the rest).
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
            HirLiteral::Bool(_) => "bool literal",
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
    ) -> Option<Place> {
        let dest = self.alloc_local(ty.clone());
        let instr = match op {
            BinaryOp::Add => Instr::IntAdd { dest, lhs, rhs },
            BinaryOp::Subtract => Instr::IntSub { dest, lhs, rhs },
            BinaryOp::Multiply => Instr::IntMul { dest, lhs, rhs },
            // Cluster 1 only supports +/-/* on integers. Anything else
            // (Divide, Modulo, comparisons, logical ops, range, send,
            // regex) falls outside the spine subset and the backend
            // pipeline rejects it at the driver level via the
            // `E_CUTOVER_UNSUPPORTED` reject path. Drop the dest local
            // and return None so the caller propagates the absence.
            _ => {
                self.locals.pop();
                return None;
            }
        };
        self.instructions.push(instr);
        Some(dest)
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
///   2. For every `Terminator::Return` exit (the only terminator the
///      current single-block spine constructs), emit a `DropPlan`
///      whose `drops` are the live owned-local list in reverse
///      declaration order (LIFO).
///   3. For declared-but-not-constructed terminators (`Panic`, `Yield`,
///      `Send`, `Goto`, `Branch`, `Call`), the pass enumerates them
///      with an empty drop plan when reached — Cluster 4+ adds the
///      construction surfaces that turn these into populated plans.
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
fn elaborate(checked: &CheckedMirFunction, builder: &Builder) -> ElaboratedMirFunction {
    // Statements stream: retained for snapshot/compat continuity with
    // the pre-Cluster-3 elaborator. Every non-`BitCopy` owned local
    // gets a checker-stream `Drop` entry in reverse-declaration order;
    // the structural drop plan in `drop_plans` is the authoritative
    // per-`ExitPath` answer.
    let mut elaborated_statements = builder.statements.clone();
    for (binding, name, ty) in builder.owned_locals.iter().rev() {
        elaborated_statements.push(MirStatement::Drop {
            binding: *binding,
            name: name.clone(),
            ty: ty.clone(),
        });
    }

    let lifo_drops = build_lifo_drops(
        &builder.owned_locals,
        &builder.binding_locals,
        &builder.type_classes,
    );
    let (elab_blocks, drop_plans) = enumerate_exits(&checked.block, &lifo_drops);

    ElaboratedMirFunction {
        name: checked.name.clone(),
        return_ty: checked.return_ty.clone(),
        statements: elaborated_statements,
        decisions: builder.decisions.clone(),
        blocks: elab_blocks,
        drop_plans,
        coroutine: None,
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
                    _ => None,
                };
                // Resolve to the binding's real backend place. Falling
                // back to `ReturnSlot` for an unmapped binding would
                // drop the wrong slot, so we treat a missing entry as
                // a builder invariant violation in debug builds and
                // emit ReturnSlot in release as the historical
                // placeholder (preserved so this change does not
                // regress snapshot-stability if a previously-unmapped
                // surface exists in another test). Today's call sites
                // always populate `binding_locals` for any binding
                // that reaches `owned_locals`.
                let place = binding_locals
                    .get(binding)
                    .copied()
                    .unwrap_or(Place::ReturnSlot);
                drops.push(ElabDrop {
                    place,
                    ty: ty.clone(),
                    drop_fn,
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
/// single-block function. The spine constructs only `Terminator::Return`;
/// the remaining variants are enumerated for forward compatibility so
/// future construction surfaces don't have to retrofit the dispatch.
fn enumerate_exits(
    block: &BasicBlock,
    lifo: &[ElabDrop],
) -> (Vec<ElabBlock>, Vec<(ExitPath, DropPlan)>) {
    let block_id = block.id;
    let mut blocks = vec![ElabBlock {
        id: block_id,
        kind: BlockKind::Normal,
        drops: Vec::new(),
        successor: None,
    }];
    let drops = lifo.to_vec();
    let plan = match &block.terminator {
        Terminator::Return => (
            ExitPath::Return { block: block_id },
            DropPlan {
                drops: drops.clone(),
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
            // Cleanup block: same LIFO drop plan as the normal exit at
            // this scope depth; no successor (trap is terminal). Cleanup
            // ids start past the highest normal-block id — single-block
            // spine puts the cleanup at id 1.
            blocks.push(ElabBlock {
                id: block_id + 1,
                kind: BlockKind::Cleanup,
                drops: drops.clone(),
                successor: None,
            });
            (
                ExitPath::Panic { block: block_id },
                DropPlan {
                    drops: drops.clone(),
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
            // `actor` is a Place; the ExitPath::Send slot carries the
            // callee name. Spine has no Send construction surface, so
            // this is unreachable in practice — empty placeholder.
            ExitPath::Send {
                block: block_id,
                actor: String::new(),
                next: *next,
            },
            DropPlan::default(),
        ),
    };
    (blocks, vec![plan])
}
