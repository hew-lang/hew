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
use hew_types::{module_registry::ModuleRegistry, ClosureCaptureAcquisition, ResolvedTy};

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
        HirExprKind::MachineVariantCtor {
            payload: Some(fields),
            ..
        } => fields
            .iter()
            .find_map(|(_, value)| find_closure_in_expr(value)),
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
        cap.acquisition,
        ClosureCaptureAcquisition::Snapshot,
        "i64 is Copy; capture mode must be Copy; got {:?}",
        cap.acquisition
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

#[test]
fn callable_joins_preserve_checker_guarantees_in_either_order() {
    for choice in [
        "if flag { a } else { b }",
        "if flag { b } else { a }",
        "match flag { true => a, false => b }",
        "match flag { true => b, false => a }",
    ] {
        let source = format!("fn choose(a: fn[clone]() -> i64, b: fn[once, clone]() -> i64, flag: bool) {{ let f = {choice}; }}");
        let output = typecheck_and_lower(&source);
        assert!(
            output.diagnostics.is_empty(),
            "{choice}: {:?}",
            output.diagnostics
        );
        let function = output
            .module
            .items
            .iter()
            .find_map(|item| match item {
                HirItem::Function(f) if f.name == "choose" => Some(f),
                _ => None,
            })
            .unwrap();
        let value = function
            .body
            .statements
            .iter()
            .find_map(|stmt| match &stmt.kind {
                HirStmtKind::Let(_, Some(value)) => Some(value),
                _ => None,
            })
            .unwrap();
        assert!(
            matches!(value.ty, ResolvedTy::Function { capabilities, .. } if capabilities.call == hew_types::CallableCallMode::Once && capabilities.clone),
            "{choice}: {:?}",
            value.ty
        );
    }
}

#[test]
fn callable_return_erasure_preserves_concrete_closure_type() {
    let output =
        typecheck_and_lower("fn make() -> fn[once]() -> i64 { let n: i64 = 7; return || n; }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let function = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(f) if f.name == "make" => Some(f),
            _ => None,
        })
        .unwrap();
    let returned = function
        .body
        .statements
        .iter()
        .find_map(|stmt| match &stmt.kind {
            HirStmtKind::Return(Some(value)) => Some(value),
            _ => None,
        })
        .unwrap();
    assert!(
        matches!(returned.ty, ResolvedTy::Closure { capabilities, .. } if capabilities.call == hew_types::CallableCallMode::Read && capabilities.clone),
        "{:?}",
        returned.ty
    );
}

#[test]
fn contextual_option_result_preserves_nested_callable_storage() {
    let output = typecheck_and_lower("fn choose(flag: bool) -> Result<Option<fn[var, clone]() -> i64>, string> { let count: i64 = 0; if flag { .Ok(.Some(capture(var count) || { count = count + 1; count })) } else { .Ok(.None) } }");
    assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
    let closure = find_closure_in_fn(&output, "choose")
        .expect("closure retained in nested constructor payloads");
    assert!(
        matches!(closure.ty, ResolvedTy::Closure { capabilities, .. } if capabilities.call == hew_types::CallableCallMode::Var && capabilities.clone)
    );
    let HirExprKind::Closure { captures, .. } = &closure.kind else {
        panic!("closure")
    };
    assert_eq!(captures[0].access, hew_types::ClosureCaptureAccess::Var);
}
