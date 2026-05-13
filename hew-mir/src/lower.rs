use std::collections::{HashMap, HashSet};

use hew_hir::{
    named_type_names, BindingId, HirExpr, HirExprKind, HirFn, HirItem, HirLiteral, HirModule,
    HirStmtKind, IntentKind, ResolvedRef, ValueClass,
};
use hew_parser::ast::BinaryOp;
use hew_types::ResolvedTy;

use crate::model::{
    BasicBlock, CheckedMirFunction, DecisionFact, ElaboratedMirFunction, Instr, IrPipeline,
    MirCheck, MirDiagnostic, MirDiagnosticKind, MirStatement, Place, RawMirFunction, Strategy,
    Terminator, ThirFunction,
};

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
                let lowered = lower_function(func);
                thir.push(lowered.thir);
                raw_mir.push(lowered.raw);
                checked_mir.push(lowered.checked);
                elaborated_mir.push(lowered.elaborated);
                diagnostics.extend(lowered.diagnostics);
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

fn lower_function(func: &HirFn) -> LoweredFunction {
    let mut builder = Builder::default();
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
    let mut diagnostics = check_raw_mir(&raw_block);

    // Collect diagnostics emitted by the builder (e.g., Unsupported HIR nodes).
    diagnostics.append(&mut builder.diagnostics);

    collect_unknown_type_diagnostics(func, &builder, &mut diagnostics);

    let checked = CheckedMirFunction {
        name: func.name.clone(),
        return_ty: func.return_ty.clone(),
        block: raw_block,
        decisions: builder.decisions.clone(),
        checks: vec![
            MirCheck::InitialisedBeforeUse,
            MirCheck::DecisionMapTotal,
            MirCheck::UseAfterConsume,
        ],
    };
    let mut elaborated_statements = builder.statements.clone();
    for (binding, name, ty) in builder.owned_locals.iter().rev() {
        elaborated_statements.push(MirStatement::Drop {
            binding: *binding,
            name: name.clone(),
            ty: ty.clone(),
        });
    }
    let elaborated = ElaboratedMirFunction {
        name: func.name.clone(),
        return_ty: func.return_ty.clone(),
        statements: elaborated_statements,
        decisions: builder.decisions,
    };

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
                if ValueClass::of_ty(&binding.ty) != ValueClass::BitCopy {
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
            HirExprKind::Literal(lit) => self.lower_literal(lit, &expr.ty),
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
                    && ValueClass::of_ty(&expr.ty) != ValueClass::BitCopy
                {
                    self.mark_binding_moved(*id);
                }
                self.binding_locals.get(id).copied()
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
                // Unsupported inside an argument still surfaces.
                let _ = self.lower_value(callee);
                for arg in args {
                    let _ = self.lower_value(arg);
                }
                None
            }
            HirExprKind::Block(block) => {
                for stmt in &block.statements {
                    if let HirStmtKind::Expr(inner) = &stmt.kind {
                        let _ = self.lower_value(inner);
                    }
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

    fn lower_literal(&mut self, lit: &HirLiteral, ty: &ResolvedTy) -> Option<Place> {
        match lit {
            HirLiteral::Integer(value) => {
                let dest = self.alloc_local(ty.clone());
                self.instructions.push(Instr::ConstI64 {
                    dest,
                    value: *value,
                });
                Some(dest)
            }
            // Spine subset: only integer literals reach the backend in
            // Cluster 1. Float/String/Bool/Char/Duration/Unit literals are
            // out of scope (Cluster 2 takes String; Cluster 4 takes the
            // rest). They still produce a Bind/Evaluate checker statement
            // via the caller; the backend stream simply has no value.
            _ => None,
        }
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
            ValueClass::AffineResource => Strategy::Move,
            ValueClass::Unknown => Strategy::UnknownBlocked,
            ValueClass::BitCopy | ValueClass::PersistentShare | ValueClass::View => {
                Strategy::BorrowRead
            }
        };
        let strategy = match (expr.value_class, expr.intent) {
            (ValueClass::CowValue, IntentKind::Modify) => Strategy::EnsureUnique,
            (ValueClass::CowValue, IntentKind::Read | IntentKind::Capture) => Strategy::CowShare,
            (ValueClass::AffineResource, IntentKind::Read) => Strategy::BorrowRead,
            (
                ValueClass::BitCopy | ValueClass::CowValue | ValueClass::AffineResource,
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

fn check_raw_mir(block: &BasicBlock) -> Vec<MirDiagnostic> {
    let mut consumed = HashSet::new();
    let mut diagnostics = Vec::new();

    for statement in &block.statements {
        match statement {
            MirStatement::Bind { binding, .. } => {
                consumed.remove(binding);
            }
            MirStatement::Use {
                binding,
                name,
                ty,
                intent,
                ..
            } => {
                if consumed.contains(binding) {
                    diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UseAfterConsume {
                            binding: *binding,
                            name: name.clone(),
                        },
                        note: "binding is used after an owned value move in checked MIR"
                            .to_string(),
                    });
                }
                if *intent == IntentKind::Consume && ValueClass::of_ty(ty) != ValueClass::BitCopy {
                    consumed.insert(*binding);
                }
            }
            MirStatement::Evaluate { .. }
            | MirStatement::Return { .. }
            | MirStatement::Drop { .. } => {}
        }
    }

    diagnostics
}
