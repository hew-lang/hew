//! HIR-shape tests for C-3: `Vec<T>` range-slice (`xs[a..b]` and four
//! open-end forms) lowering.
//!
//! `Expr::Index { object, index: Expr::Range { .. } }` lowers to
//! `HirExprKind::Slice { container, start, end, inclusive }`, distinct
//! from C-2's `HirExprKind::Index { container, index }` for single-
//! element access. Open endpoints survive into HIR as `None`; MIR fills
//! them at lowering (open `start := 0`, open `end := hew_vec_len(v)`).
//!
//! Result type is `Vec<T>` (a freshly-allocated copy), read from the
//! checker's `expr_types` side-table. LESSONS: `checker-authority` (P0).

use hew_hir::{lower_program, verify_hir, HirExprKind, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

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

fn tail_kind<'a>(out: &'a hew_hir::LowerOutput, fn_name: &str) -> &'a HirExprKind {
    let func = out
        .module
        .items
        .iter()
        .find_map(|item| {
            if let hew_hir::HirItem::Function(f) = item {
                if f.name == fn_name {
                    return Some(f);
                }
            }
            None
        })
        .expect("function must be in HIR module");
    &func.body.tail.as_ref().expect("body has tail").kind
}

#[test]
fn closed_slice_a_b_lowers_to_hir_slice() {
    // `xs[a..b]` lowers to HirExprKind::Slice (NOT Index).
    let out = lower("fn f(xs: Vec<i64>, a: i64, b: i64) -> Vec<i64> { xs[a..b] }");
    let kind = tail_kind(&out, "f");
    let HirExprKind::Slice {
        start,
        end,
        inclusive,
        ..
    } = kind
    else {
        panic!("xs[a..b] must lower to HirExprKind::Slice; got {kind:?}");
    };
    assert!(start.is_some(), "closed start must lower to Some");
    assert!(end.is_some(), "closed end must lower to Some");
    assert!(!inclusive, "..= flag must be false for ..");
}

#[test]
fn inclusive_slice_a_eq_b_carries_inclusive_flag() {
    let out = lower("fn f(xs: Vec<i64>, a: i64, b: i64) -> Vec<i64> { xs[a..=b] }");
    let kind = tail_kind(&out, "f");
    let HirExprKind::Slice { inclusive, .. } = kind else {
        panic!("xs[a..=b] must lower to HirExprKind::Slice; got {kind:?}");
    };
    assert!(*inclusive, "inclusive flag must propagate from parser");
}

#[test]
fn open_start_slice_lowers_with_none_start() {
    let out = lower("fn f(xs: Vec<i64>, b: i64) -> Vec<i64> { xs[..b] }");
    let kind = tail_kind(&out, "f");
    let HirExprKind::Slice { start, end, .. } = kind else {
        panic!("xs[..b] must lower to HirExprKind::Slice; got {kind:?}");
    };
    assert!(start.is_none(), "open start must lower to None in HIR");
    assert!(end.is_some(), "closed end must lower to Some");
}

#[test]
fn open_end_slice_lowers_with_none_end() {
    let out = lower("fn f(xs: Vec<i64>, a: i64) -> Vec<i64> { xs[a..] }");
    let kind = tail_kind(&out, "f");
    let HirExprKind::Slice { start, end, .. } = kind else {
        panic!("xs[a..] must lower to HirExprKind::Slice; got {kind:?}");
    };
    assert!(start.is_some(), "closed start must lower to Some");
    assert!(end.is_none(), "open end must lower to None in HIR");
}

#[test]
fn fully_open_slice_lowers_with_both_none() {
    let out = lower("fn f(xs: Vec<i64>) -> Vec<i64> { xs[..] }");
    let kind = tail_kind(&out, "f");
    let HirExprKind::Slice { start, end, .. } = kind else {
        panic!("xs[..] must lower to HirExprKind::Slice; got {kind:?}");
    };
    assert!(start.is_none(), "open start must be None");
    assert!(end.is_none(), "open end must be None");
}

#[test]
fn slice_result_type_is_vec_t_not_t() {
    // The expression type of a range-slice is Vec<T> (a fresh copy),
    // NOT the element type T. LESSONS: `checker-authority` (P0).
    let out = lower("fn f(xs: Vec<i64>) -> Vec<i64> { xs[..] }");
    let func = out
        .module
        .items
        .iter()
        .find_map(|item| {
            if let hew_hir::HirItem::Function(f) = item {
                if f.name == "f" {
                    return Some(f);
                }
            }
            None
        })
        .expect("function must be in HIR module");
    let tail = func.body.tail.as_ref().expect("tail must exist");
    match &tail.ty {
        ResolvedTy::Named { name, args, .. } => {
            assert_eq!(name, "Vec");
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], ResolvedTy::I64);
        }
        other => panic!("slice result type must be Vec<i64>; got {other:?}"),
    }
}

#[test]
fn single_element_index_still_lowers_to_index() {
    // Regression guard: `xs[i]` must continue to produce HirExprKind::Index
    // (NOT Slice). The C-3 routing only activates when the bracket
    // contents are a range expression.
    let out = lower("fn f(xs: Vec<i64>, i: i64) -> i64 { xs[i] }");
    let kind = tail_kind(&out, "f");
    assert!(
        matches!(kind, HirExprKind::Index { .. }),
        "xs[i] must lower to HirExprKind::Index; got {kind:?}"
    );
}

#[test]
fn slice_verify_passes() {
    let out = lower("fn f(xs: Vec<i64>) -> Vec<i64> { xs[..] }");
    let diags = verify_hir(&out.module);
    assert!(diags.is_empty(), "HIR verify must pass; got {diags:?}");
}
