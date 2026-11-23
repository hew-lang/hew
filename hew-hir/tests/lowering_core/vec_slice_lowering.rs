//! HIR-shape tests for C-3: `Vec<T>` range-slice (`xs[a..b]` and four
//! open-end forms) lowering.
//!
//! `Expr::Index { object, index: Expr::Range { .. } }` lowers to
//! `HirExprKind::Slice { container, start, end }`, distinct from C-2's
//! `HirExprKind::Index { container, index }` for single-element access.
//! Open endpoints survive into HIR as `None`; semantic lowering fills them
//! (open `start := 0`, open `end := the container's open-ended operation`).
//! The end bound is always exclusive: `xs[a..=b]` lowers to `xs[a..b + 1]`.
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
fn slice_forms_lower_to_hir_slice_with_vec_type_and_verify() {
    for (source, expect_start, expect_end) in [
        (
            "fn f(xs: Vec<i64>, a: i64, b: i64) -> Vec<i64> { xs[a..b] }",
            true,
            true,
        ),
        (
            "fn f(xs: Vec<i64>, b: i64) -> Vec<i64> { xs[..b] }",
            false,
            true,
        ),
        (
            "fn f(xs: Vec<i64>, a: i64) -> Vec<i64> { xs[a..] }",
            true,
            false,
        ),
        ("fn f(xs: Vec<i64>) -> Vec<i64> { xs[..] }", false, false),
    ] {
        let out = lower(source);
        let func = out
            .module
            .items
            .iter()
            .find_map(|item| match item {
                hew_hir::HirItem::Function(f) if f.name == "f" => Some(f),
                _ => None,
            })
            .expect("function must be in HIR module");
        let tail = func.body.tail.as_ref().expect("body has tail");
        let HirExprKind::Slice { start, end, .. } = &tail.kind else {
            panic!(
                "{source}: must lower to HirExprKind::Slice; got {:?}",
                tail.kind
            );
        };
        assert_eq!(start.is_some(), expect_start, "{source}: start");
        assert_eq!(end.is_some(), expect_end, "{source}: end");
        if expect_start && expect_end {
            assert!(
                !matches!(end.as_ref().unwrap().kind, HirExprKind::Binary { .. }),
                "`..` must carry the source bound unchanged; got {:?}",
                end.as_ref().unwrap().kind
            );
        }
        match &tail.ty {
            ResolvedTy::Named {
                head: name_head,
                args,
                ..
            } => {
                assert_eq!(name_head.spelling(), "Vec");
                assert_eq!(args.as_slice(), &[ResolvedTy::I64]);
            }
            other => panic!("slice result type must be Vec<i64>; got {other:?}"),
        }
        let diags = verify_hir(&out.module);
        assert!(
            diags.is_empty(),
            "{source}: HIR verify must pass; got {diags:?}"
        );
    }
}

#[test]
fn inclusive_slice_a_eq_b_lowers_to_an_exclusive_bound_plus_one() {
    // `xs[a..=b]` carries no inclusive flag past HIR: the bound becomes
    // `b + 1`, whose checked addition traps on overflow, and every later
    // stage sees the single exclusive range shape.
    let out = lower("fn f(xs: Vec<i64>, a: i64, b: i64) -> Vec<i64> { xs[a..=b] }");
    let kind = tail_kind(&out, "f");
    let HirExprKind::Slice { end, .. } = kind else {
        panic!("xs[a..=b] must lower to HirExprKind::Slice; got {kind:?}");
    };
    let end = end.as_ref().expect("inclusive end must lower to Some");
    let Some((_, op, right)) = end.kind.as_binary_pair() else {
        panic!("inclusive end must lower to `b + 1`; got {:?}", end.kind);
    };
    assert_eq!(op, hew_parser::ast::BinaryOp::Add);
    assert!(
        matches!(
            right.kind,
            HirExprKind::Literal(hew_hir::HirLiteral::Integer(1))
        ),
        "inclusive end must add the literal one; got {:?}",
        right.kind
    );
    assert_eq!(end.ty, ResolvedTy::I64, "the added bound keeps its width");
}
