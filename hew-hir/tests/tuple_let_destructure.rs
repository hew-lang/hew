//! Tuple-let lowering: `let (a, b) = expr;` produces per-element Let stmts.
//!
//! Verifies:
//!   1. A two-element tuple-let produces one synthetic temp binding + two
//!      per-element `HirStmtKind::Let` bindings whose init exprs are
//!      `HirExprKind::TupleIndex` projections.
//!   2. A wildcard element in the tuple pattern is bound as `_<idx>`.
//!
//! Uses `duplex_pair<i64, i64>(16)` as the tuple-producing expression because
//! `duplex_pair` is a checker-registered builtin that returns a genuine
//! `ResolvedTy::Tuple` — the most realistic producer for the exemplar vertebra.
//! Both tests run the full typecheck pipeline so the rewrite table is populated.

use hew_hir::{lower_program, HirExprKind, HirStmtKind, ResolutionCtx};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn lower_with_typecheck(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    lower_program(&parsed.program, &tc_output, &ResolutionCtx)
}

fn main_fn(output: &hew_hir::LowerOutput) -> &hew_hir::HirFn {
    output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let hew_hir::HirItem::Function(f) = item {
                if f.name == "main" {
                    return Some(f);
                }
            }
            None
        })
        .expect("main function must be present")
}

/// `let (a, b) = duplex_pair<i64, i64>(16);` produces:
///   - `let __tuple_N = duplex_pair<i64, i64>(16);`  (synthetic temp)
///   - `let a = __tuple_N.0;`                         (`TupleIndex` 0)
///   - `let b = __tuple_N.1;`                         (`TupleIndex` 1)
#[test]
fn two_element_tuple_let_produces_three_stmts() {
    let source = r"
        fn main() -> i64 {
            let (a, b) = duplex_pair<i64, i64>(16);
            return 0;
        }
    ";
    let output = lower_with_typecheck(source);

    // Filter to only the MethodCallNoRewrite / structural errors — the
    // UnresolvedSymbol for `duplex_pair` callee and UnresolvedInferenceVar are
    // expected at the HIR level (duplex_pair is a builtin, not an AST fn item).
    // The critical assertion is that NO tuple-pattern errors fired.
    let tuple_errors: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                hew_hir::HirDiagnosticKind::CutoverUnsupported { .. }
            ) && {
                if let hew_hir::HirDiagnosticKind::CutoverUnsupported { ref construct, .. } = d.kind
                {
                    construct.contains("tuple")
                } else {
                    false
                }
            }
        })
        .collect();
    assert!(
        tuple_errors.is_empty(),
        "no tuple-pattern CutoverUnsupported errors must fire for a well-formed two-element let; \
         got: {tuple_errors:#?}"
    );

    let fn_item = main_fn(&output);

    // Expect at least 3 Let stmts from the tuple-let expansion.
    let let_stmts: Vec<_> = fn_item
        .body
        .statements
        .iter()
        .filter(|s| matches!(s.kind, HirStmtKind::Let(_, _)))
        .collect();
    assert!(
        let_stmts.len() >= 3,
        "tuple-let (a, b) = ... must produce ≥3 Let stmts (temp + a + b); got {} stmts: {:#?}",
        let_stmts.len(),
        let_stmts
    );

    // First Let binds the synthetic temp (name starts with `__tuple_`).
    if let HirStmtKind::Let(binding, _) = &let_stmts[0].kind {
        assert!(
            binding.name.starts_with("__tuple_"),
            "first Let must bind the synthetic temp; got name `{}`",
            binding.name
        );
    }

    // Second Let must bind `a` with TupleIndex(0) init.
    if let HirStmtKind::Let(binding, Some(init)) = &let_stmts[1].kind {
        assert_eq!(binding.name, "a", "second Let must bind `a`");
        assert!(
            matches!(&init.kind, HirExprKind::TupleIndex { index: 0, .. }),
            "a's init must be TupleIndex(0); got: {:#?}",
            init.kind
        );
    } else {
        panic!(
            "second Let must have an initialiser; got: {:#?}",
            let_stmts[1].kind
        );
    }

    // Third Let must bind `b` with TupleIndex(1) init.
    if let HirStmtKind::Let(binding, Some(init)) = &let_stmts[2].kind {
        assert_eq!(binding.name, "b", "third Let must bind `b`");
        assert!(
            matches!(&init.kind, HirExprKind::TupleIndex { index: 1, .. }),
            "b's init must be TupleIndex(1); got: {:#?}",
            init.kind
        );
    } else {
        panic!(
            "third Let must have an initialiser; got: {:#?}",
            let_stmts[2].kind
        );
    }
}

/// Wildcard element `let (a, _) = duplex_pair<i64, i64>(16);` binds the
/// wildcard as `_1` (synthetic name `_<idx>`).
#[test]
fn tuple_let_wildcard_element_binds_synthetic_name() {
    let source = r"
        fn main() -> i64 {
            let (a, _) = duplex_pair<i64, i64>(16);
            return 0;
        }
    ";
    let output = lower_with_typecheck(source);

    let fn_item = main_fn(&output);

    let let_stmts: Vec<_> = fn_item
        .body
        .statements
        .iter()
        .filter(|s| matches!(s.kind, HirStmtKind::Let(_, _)))
        .collect();

    // temp + a + _1 = 3 Let stmts minimum.
    assert!(
        let_stmts.len() >= 3,
        "wildcard tuple-let must produce ≥3 Let stmts; got {}: {:#?}",
        let_stmts.len(),
        let_stmts
    );

    // Third stmt should bind `_1` (wildcard element 1 → `_<idx>`).
    if let HirStmtKind::Let(binding, _) = &let_stmts[2].kind {
        assert_eq!(
            binding.name, "_1",
            "wildcard element 1 must bind as `_1`; got `{}`",
            binding.name
        );
    }
}
