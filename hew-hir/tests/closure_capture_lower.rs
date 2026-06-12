//! HIR-level closure capture probe tests.
//!
//! These tests verify that the HIR lowering pass correctly materialises
//! binding-accurate capture facts from the checker's `closure_capture_facts`
//! side-table into `HirExprKind::Closure::captures`. The checker-level
//! equivalents live in `hew-types/src/check/tests.rs`; this file pins the
//! producer-to-HIR boundary so a regression in `lower_closure` or
//! `materialize_closure_captures` surfaces here before reaching MIR.
//!
//! LESSONS trigger: `feedback_verify_ast_carries_discriminator_before_codegen_fix`
//! — verify the HIR closure node carries non-empty captures for a closure that
//! references an outer binding before relying on capture facts downstream.

use hew_hir::{lower_program, HirExprKind, HirItem, HirStmtKind, ResolutionCtx};
use hew_types::Checker;
use hew_types::{module_registry::ModuleRegistry, ClosureCaptureMode, ResolvedTy};

/// Run the full source → typecheck → HIR pipeline.  Panics on parse or type
/// errors so individual tests can assert on the resulting HIR structure.
fn typecheck_and_lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:?}", tco.errors);
    lower_program(
        &parsed.program,
        &tco,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

/// Find the `HirExprKind::Closure` node inside the body of a top-level
/// function named `outer_fn`.  Returns `None` if no closure is found.
fn find_closure_in_fn<'a>(
    output: &'a hew_hir::LowerOutput,
    fn_name: &str,
) -> Option<&'a hew_hir::HirExpr> {
    let func = output.module.items.iter().find_map(|item| {
        if let HirItem::Function(f) = item {
            if f.name == fn_name {
                return Some(f);
            }
        }
        None
    })?;
    find_closure_in_stmts(&func.body.statements, func.body.tail.as_deref())
}

fn find_closure_in_stmts<'a>(
    stmts: &'a [hew_hir::HirStmt],
    tail: Option<&'a hew_hir::HirExpr>,
) -> Option<&'a hew_hir::HirExpr> {
    for stmt in stmts {
        if let HirStmtKind::Let(_, Some(expr)) = &stmt.kind {
            if let found @ Some(_) = find_closure_in_expr(expr) {
                return found;
            }
        }
        if let HirStmtKind::Expr(expr) = &stmt.kind {
            if let found @ Some(_) = find_closure_in_expr(expr) {
                return found;
            }
        }
    }
    if let Some(tail) = tail {
        find_closure_in_expr(tail)
    } else {
        None
    }
}

fn find_closure_in_expr(expr: &hew_hir::HirExpr) -> Option<&hew_hir::HirExpr> {
    if matches!(expr.kind, HirExprKind::Closure { .. }) {
        return Some(expr);
    }
    match &expr.kind {
        HirExprKind::Block(block) => {
            find_closure_in_stmts(&block.statements, block.tail.as_deref())
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => find_closure_in_expr(condition)
            .or_else(|| find_closure_in_expr(then_expr))
            .or_else(|| else_expr.as_deref().and_then(find_closure_in_expr)),
        HirExprKind::Binary { left, right, .. } => {
            find_closure_in_expr(left).or_else(|| find_closure_in_expr(right))
        }
        _ => None,
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[test]
fn copy_capture_produces_one_hir_capture_entry() {
    // A closure that references one outer `i64` binding via Copy capture
    // must produce exactly one `HirClosureCapture` with the correct name,
    // type, and mode.  This exercises the `checker_facts → HIR captures`
    // materialisation path in `lower_closure` / `materialize_closure_captures`.
    let output = typecheck_and_lower(
        r"
        fn main() {
            let k: i64 = 42;
            let f = |n: i64| n + k;
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let closure_expr =
        find_closure_in_fn(&output, "main").expect("HirExprKind::Closure must be present in main");

    let HirExprKind::Closure {
        captures,
        params,
        ret_ty,
        ..
    } = &closure_expr.kind
    else {
        panic!("expected Closure kind; got {:?}", closure_expr.kind);
    };

    assert_eq!(
        captures.len(),
        1,
        "one outer binding `k` must produce exactly one capture; got {captures:?}"
    );
    let cap = &captures[0];
    assert_eq!(cap.name, "k", "captured binding name must be `k`");
    assert_eq!(
        cap.ty,
        ResolvedTy::I64,
        "captured binding type must be i64; got {:?}",
        cap.ty
    );
    assert_eq!(
        cap.mode,
        ClosureCaptureMode::Copy,
        "i64 is Copy; capture mode must be Copy; got {:?}",
        cap.mode
    );

    // Params and return type are sanity-checked as a secondary invariant.
    assert_eq!(params.len(), 1, "closure has one explicit param `n`");
    assert_eq!(*ret_ty, ResolvedTy::I64, "return type is i64");
}

#[test]
fn repeated_use_of_same_binding_is_deduplicated_to_one_capture() {
    // A closure body that references the same outer binding twice (e.g. `k + k`)
    // must produce exactly ONE `HirClosureCapture` entry for that binding — the
    // checker deduplicates by `binding_id` before populating `closure_capture_facts`,
    // and the HIR materialisation walk must honour that.
    //
    // This pins the deduplication path documented in the checker test at
    // `hew-types/src/check/tests.rs::closure_capture_facts_are_binding_accurate_and_deduplicated`.
    let output = typecheck_and_lower(
        r"
        fn main() {
            let k: i64 = 2;
            let f = |n: i64| n + k + k;
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let closure_expr =
        find_closure_in_fn(&output, "main").expect("HirExprKind::Closure must be present in main");
    let HirExprKind::Closure { captures, .. } = &closure_expr.kind else {
        panic!("expected Closure kind");
    };

    let k_captures: Vec<_> = captures.iter().filter(|c| c.name == "k").collect();
    assert_eq!(
        k_captures.len(),
        1,
        "repeated use of `k` must deduplicate to one capture entry; got {k_captures:?}"
    );
    assert_eq!(
        k_captures[0].ty,
        ResolvedTy::I64,
        "deduplicated capture type must be i64"
    );
}

#[test]
fn non_capturing_closure_has_empty_capture_list() {
    // A closure that only references its own parameters and no outer bindings
    // must produce an empty captures list. This verifies the walker does NOT
    // include parameter bindings as captures (they're in scope within the
    // closure body's own scope, not from the outer closure scope).
    let output = typecheck_and_lower(
        r"
        fn main() {
            let f = |n: i64| n + 1;
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let closure_expr =
        find_closure_in_fn(&output, "main").expect("HirExprKind::Closure must be present in main");
    let HirExprKind::Closure { captures, .. } = &closure_expr.kind else {
        panic!("expected Closure kind");
    };

    assert!(
        captures.is_empty(),
        "a closure that captures nothing must have an empty captures list; got {captures:?}"
    );
}

#[test]
fn closure_capture_binding_id_is_stable_across_lowering() {
    // The `HirClosureCapture::binding` id must match the `BindingId` assigned
    // to the outer `let k` binding.  This verifies that `lower_closure` threads
    // the binding identity from the enclosing scope through the capture, not a
    // freshly allocated id.
    let output = typecheck_and_lower(
        r"
        fn main() {
            let k: i64 = 42;
            let f = |n: i64| n + k;
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let func = output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Function(f) = item {
                if f.name == "main" {
                    return Some(f);
                }
            }
            None
        })
        .expect("`main` must exist");

    // Pull the BindingId for `let k` from the first Let statement.
    let k_binding_id = func
        .body
        .statements
        .iter()
        .find_map(|stmt| {
            if let HirStmtKind::Let(binding, _) = &stmt.kind {
                if binding.name == "k" {
                    return Some(binding.id);
                }
            }
            None
        })
        .expect("`let k` must be the first let statement in main");

    // Find the closure and retrieve its captures.
    let closure_expr = find_closure_in_fn(&output, "main").expect("closure must exist");
    let HirExprKind::Closure { captures, .. } = &closure_expr.kind else {
        panic!("expected Closure kind");
    };

    let cap = captures
        .iter()
        .find(|c| c.name == "k")
        .expect("capture for `k` must be present");

    assert_eq!(
        cap.binding, k_binding_id,
        "capture binding id must match the outer `let k` binding id"
    );
}
