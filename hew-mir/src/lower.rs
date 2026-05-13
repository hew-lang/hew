use hew_hir::{
    HirExpr, HirExprKind, HirFn, HirItem, HirModule, HirStmtKind, IntentKind, ResolvedRef, SiteId,
    ValueClass,
};
use hew_types::ResolvedTy;

use crate::model::{
    BasicBlock, CheckedMirFunction, DecisionFact, ElaboratedMirFunction, HewMlirFunction,
    HewMlirModule, HewMlirOp, IrPipeline, MirCheck, MirStatement, RawMirFunction, Strategy,
    Terminator, ThirFunction,
};

#[must_use]
pub fn lower_hir_module(module: &HirModule) -> IrPipeline {
    let mut thir = Vec::new();
    let mut raw_mir = Vec::new();
    let mut checked_mir = Vec::new();
    let mut elaborated_mir = Vec::new();
    let mut mlir_functions = Vec::new();

    for item in &module.items {
        match item {
            HirItem::Function(func) => {
                let lowered = lower_function(func);
                thir.push(lowered.thir);
                raw_mir.push(lowered.raw);
                checked_mir.push(lowered.checked);
                mlir_functions.push(lowered.mlir);
                elaborated_mir.push(lowered.elaborated);
            }
        }
    }

    IrPipeline {
        thir,
        raw_mir,
        checked_mir,
        elaborated_mir,
        hew_mlir: HewMlirModule {
            functions: mlir_functions,
        },
    }
}

#[derive(Debug)]
struct LoweredFunction {
    thir: ThirFunction,
    raw: RawMirFunction,
    checked: CheckedMirFunction,
    elaborated: ElaboratedMirFunction,
    mlir: HewMlirFunction,
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
        terminator: Terminator::Return,
    };
    let raw = RawMirFunction {
        name: func.name.clone(),
        return_ty: func.return_ty.clone(),
        blocks: vec![raw_block.clone()],
        decisions: builder.decisions.clone(),
    };
    let checked = CheckedMirFunction {
        name: func.name.clone(),
        return_ty: func.return_ty.clone(),
        block: raw_block,
        decisions: builder.decisions.clone(),
        checks: vec![MirCheck::InitialisedBeforeUse, MirCheck::DecisionMapTotal],
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
    let mlir = lower_elaborated_to_mlir(&elaborated);

    LoweredFunction {
        thir,
        raw,
        checked,
        elaborated,
        mlir,
    }
}

#[derive(Debug, Default)]
struct Builder {
    statements: Vec<MirStatement>,
    decisions: Vec<DecisionFact>,
    owned_locals: Vec<(hew_hir::BindingId, String, ResolvedTy)>,
}

impl Builder {
    fn function_body(&mut self, func: &HirFn) {
        for stmt in &func.body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &func.body.tail {
            self.expr(tail);
            self.decide(tail);
            self.mark_returned_binding_moved(tail);
            self.statements.push(MirStatement::Return {
                site: Some(tail.site),
                ty: tail.ty.clone(),
            });
        }
    }

    fn stmt(&mut self, stmt: &hew_hir::HirStmt) {
        match &stmt.kind {
            HirStmtKind::Let(binding, Some(value)) => {
                self.expr(value);
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
            }
            HirStmtKind::Let(_, None) => {}
            HirStmtKind::Expr(expr) => {
                self.expr(expr);
                self.statements.push(MirStatement::Evaluate {
                    site: expr.site,
                    ty: expr.ty.clone(),
                });
            }
            HirStmtKind::Return(Some(expr)) => {
                self.expr(expr);
                self.decide(expr);
                self.mark_returned_binding_moved(expr);
                self.statements.push(MirStatement::Return {
                    site: Some(expr.site),
                    ty: expr.ty.clone(),
                });
            }
            HirStmtKind::Return(None) => {
                self.statements.push(MirStatement::Return {
                    site: None,
                    ty: ResolvedTy::Unit,
                });
            }
        }
    }

    fn expr(&mut self, expr: &HirExpr) {
        self.decide(expr);
        match &expr.kind {
            HirExprKind::Binary { left, right, .. } => {
                self.expr(left);
                self.expr(right);
            }
            HirExprKind::Call { callee, args } => {
                self.expr(callee);
                for arg in args {
                    self.expr(arg);
                }
            }
            HirExprKind::Block(block) => {
                for stmt in &block.statements {
                    if let HirStmtKind::Expr(expr) = &stmt.kind {
                        self.expr(expr);
                    }
                }
                if let Some(tail) = &block.tail {
                    self.expr(tail);
                }
            }
            HirExprKind::If {
                condition,
                then_expr,
                else_expr,
            } => {
                self.expr(condition);
                self.expr(then_expr);
                if let Some(else_expr) = else_expr {
                    self.expr(else_expr);
                }
            }
            HirExprKind::StructInit { fields, .. } => {
                for (_, field) in fields {
                    self.expr(field);
                }
            }
            HirExprKind::Literal(_)
            | HirExprKind::BindingRef { .. }
            | HirExprKind::Unsupported(_) => {}
        }
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
        self.owned_locals.retain(|(binding, _, _)| *binding != id);
    }
}

fn lower_elaborated_to_mlir(func: &ElaboratedMirFunction) -> HewMlirFunction {
    let mut ops = Vec::new();
    for statement in &func.statements {
        match statement {
            MirStatement::Bind { name, site, ty, .. } => ops.push(HewMlirOp::Bind {
                name: name.clone(),
                ty: ty.clone(),
                site: *site,
                decision: decision_for(func, *site),
            }),
            MirStatement::Evaluate { site, ty } => ops.push(HewMlirOp::Read {
                ty: ty.clone(),
                site: *site,
                decision: decision_for(func, *site),
            }),
            MirStatement::Return {
                site: Some(site),
                ty,
            } => ops.push(HewMlirOp::Return {
                ty: ty.clone(),
                site: Some(*site),
                decision: Some(decision_for(func, *site)),
            }),
            MirStatement::Return { site: None, ty } => {
                ops.push(HewMlirOp::Return {
                    ty: ty.clone(),
                    site: None,
                    decision: None,
                });
            }
            MirStatement::Drop { name, ty, .. } => {
                ops.push(HewMlirOp::Drop {
                    name: name.clone(),
                    ty: ty.clone(),
                });
            }
        }
    }
    HewMlirFunction {
        name: func.name.clone(),
        ops,
    }
}

fn decision_for(func: &ElaboratedMirFunction, site: SiteId) -> Strategy {
    func.decisions
        .iter()
        .find(|decision| decision.site == site)
        .map_or_else(
            || unreachable!("missing DecisionFact for MLIR site {site}"),
            |decision| decision.strategy,
        )
}
