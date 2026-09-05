//! HIR vertical tests for `is` lowering.
//!
//! The checker records a static-tautology entry in `is_type_patterns`
//! whenever an `is TypeName` LHS's static type matches the RHS type
//! pattern; the HIR `Expr::Is` branch in `lower.rs` reads that side table
//! and would emit `HirExprKind::Literal(HirLiteral::Bool(true))` instead of
//! the runtime `HirExprKind::IdentityCompare` form used by value-pattern
//! `is`. D340 narrows the `is` admission set to actor handles only
//! (`is_identity_capable`, HEW-SPEC-2026 §3.4.3's pid handle row), and a
//! `resolve_is_type_pattern` RHS is always the bare `TypeDef` name with no
//! generic arguments, while every admitted actor value is a `LocalPid<T>`
//! handle — so the LHS and the RHS pattern can never structurally match
//! (`LocalPid<Worker>` vs. `Worker`) and the tautology branch has no
//! reachable caller in current Hew source (#3134). The lowering code stays
//! for when handle-category admission broadens (HEW-SPEC-2026 §3.4.3's
//! counted/opaque/resource rows); there is no positive-control program left
//! to pin it with, so only the always-reachable value-pattern lowering is
//! tested here.

use hew_hir::{lower_program, HirBlock, HirExpr, HirExprKind, HirItem, HirStmtKind, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

fn collect_is_exprs<'a>(block: &'a HirBlock, out: &mut Vec<&'a HirExpr>) {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(_, Some(value)) | HirStmtKind::Expr(value) => walk_expr(value, out),
            HirStmtKind::Return(Some(e)) => walk_expr(e, out),
            HirStmtKind::Assign { value, .. } => walk_expr(value, out),
            _ => {}
        }
    }
    if let Some(tail) = &block.tail {
        walk_expr(tail, out);
    }
}

fn walk_expr<'a>(expr: &'a HirExpr, out: &mut Vec<&'a HirExpr>) {
    match &expr.kind {
        HirExprKind::Literal(_) => out.push(expr),
        HirExprKind::IdentityCompare { left, right } => {
            out.push(expr);
            walk_expr(left, out);
            walk_expr(right, out);
        }
        HirExprKind::Unary { operand, .. } => walk_expr(operand, out),
        HirExprKind::Block(b) => collect_is_exprs(b, out),
        _ => {}
    }
}

#[test]
fn is_value_pattern_lowers_to_identity_compare() {
    // Value-pattern `is` (both sides actor handles of the same actor type)
    // lowers to the runtime `IdentityCompare` form, never to a literal — the
    // one shape of `is` this file still has a reachable positive control for
    // (see the module doc comment).
    let output = lower(
        r"
        actor Worker {
            let _id: i64,
            receive fn ping() {}
        }

        fn main() {
            let a = spawn Worker(_id: 1);
            let b = spawn Worker(_id: 2);
            let _eq: bool = a is b;
        }
        ",
    );

    let main_fn = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(f) if f.name == "main" => Some(f),
            _ => None,
        })
        .expect("main fn must be lowered");

    let mut collected = Vec::new();
    collect_is_exprs(&main_fn.body, &mut collected);

    let any_identity = collected
        .iter()
        .any(|e| matches!(&e.kind, HirExprKind::IdentityCompare { .. }));
    assert!(
        any_identity,
        "value-pattern `a is b` must lower to HirExprKind::IdentityCompare; body: {:#?}",
        main_fn.body,
    );
}
