//! HIR vertical tests for the `is TypeName` (type-pattern) lowering.
//!
//! The checker records a static-tautology entry in `is_type_patterns`
//! whenever the LHS's static type matches the RHS type pattern. The HIR
//! `Expr::Is` branch in `lower.rs` reads that side table and emits a
//! `HirExprKind::Literal(HirLiteral::Bool(true))` instead of the runtime
//! `HirExprKind::IdentityCompare` form used by value-pattern `is`.
//!
//! These tests pin that vertical: type-pattern → `Bool(true)`, value-pattern
//! → `IdentityCompare`. The pair guards against either branch silently
//! flipping shape.

use hew_hir::{
    lower_program, HirBlock, HirExpr, HirExprKind, HirItem, HirLiteral, HirStmtKind, ResolutionCtx,
};
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
fn is_type_pattern_lowers_to_bool_true_literal() {
    // `holder is Holder` where `holder: Holder` is a user `type` decl —
    // the checker has recorded the static type-pattern match, so HIR
    // lowers the comparison to a `Bool(true)` literal. Any `else` branch
    // gated on the negation is therefore dead at the HIR level.
    let output = lower(
        r"
        pub type Holder {
            v: i64;
        }

        fn main() {
            let holder = Holder { v: 1 };
            let _eq: bool = holder is Holder;
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

    // The `is` site must lower to `Literal(Bool(true))`, never to an
    // `IdentityCompare` runtime form.
    let bool_true = collected
        .iter()
        .any(|e| matches!(&e.kind, HirExprKind::Literal(HirLiteral::Bool(true))));
    assert!(
        bool_true,
        "type-pattern `is Holder` must lower to HirLiteral::Bool(true); body: {:#?}",
        main_fn.body,
    );
    let any_identity = collected
        .iter()
        .any(|e| matches!(&e.kind, HirExprKind::IdentityCompare { .. }));
    assert!(
        !any_identity,
        "type-pattern `is Holder` must NOT lower to HirExprKind::IdentityCompare; body: {:#?}",
        main_fn.body,
    );
}

#[test]
fn is_value_pattern_lowers_to_identity_compare() {
    // Control: value-pattern `is` (both sides bindings) lowers to the
    // runtime `IdentityCompare` form, NOT to a literal.
    let output = lower(
        r"
        pub type Holder {
            v: i64;
        }

        fn main() {
            let a = Holder { v: 1 };
            let b = Holder { v: 2 };
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
