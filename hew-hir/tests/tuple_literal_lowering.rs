use hew_hir::{lower_program, HirDiagnosticKind, HirExprKind, ResolutionCtx, TargetArch};
use hew_parser::{
    ast::{Expr, Item},
    parse,
};
use hew_types::{
    module_registry::ModuleRegistry, Checker, ResolvedTy, SpanKey, Ty, TypeCheckOutput,
};

#[test]
fn tuple_literal_basic_construction() {
    let source = r"
fn main() -> (i64, bool) {
    (42, true)
}
";
    let parsed = parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let output = lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host());

    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );

    let verify = hew_hir::verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify errors: {verify:#?}");

    // Check that TupleLiteral node was produced
    let main_fn = output
        .module
        .items
        .iter()
        .find(|item| {
            if let hew_hir::node::HirItem::Function(f) = item {
                f.name == "main"
            } else {
                false
            }
        })
        .expect("main function not found");

    if let hew_hir::node::HirItem::Function(f) = main_fn {
        let has_tuple_literal = matches!(
            &f.body.tail.as_ref().unwrap().kind,
            hew_hir::node::HirExprKind::TupleLiteral { .. }
        );
        assert!(has_tuple_literal, "Expected TupleLiteral node in main body");
    }
}

#[test]
fn tuple_literal_in_let_binding() {
    let source = r"
fn main() -> i64 {
    let (x, y) = (1, 2);
    x + y
}
";
    let parsed = parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let output = lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host());

    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );

    let verify = hew_hir::verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify errors: {verify:#?}");
}

#[test]
fn tuple_return_from_function() {
    let source = r"
fn swap(a: i64, b: i64) -> (i64, i64) {
    (b, a)
}

fn main() -> i64 {
    let (x, y) = swap(1, 2);
    x + y
}
";
    let parsed = parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let output = lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host());

    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );

    let verify = hew_hir::verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify errors: {verify:#?}");
}

#[test]
fn tuple_numeric_field_access_lowers_to_tuple_index() {
    let source = r"
fn first(t: (i64, bool)) -> i64 {
    t.0
}

fn second(t: (i64, bool)) -> bool {
    t.1
}
";
    let parsed = parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);

    let output = lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host());
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );
    let verify = hew_hir::verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify errors: {verify:#?}");

    let first = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::node::HirItem::Function(f) if f.name == "first" => Some(f),
            _ => None,
        })
        .expect("first function must be lowered");
    let first_tail = first.body.tail.as_ref().expect("first has a tail expr");
    assert_eq!(first_tail.ty, ResolvedTy::I64);
    assert!(
        matches!(&first_tail.kind, HirExprKind::TupleIndex { index: 0, .. }),
        "first must lower t.0 to TupleIndex(0), got: {:#?}",
        first_tail.kind
    );

    let second = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            hew_hir::node::HirItem::Function(f) if f.name == "second" => Some(f),
            _ => None,
        })
        .expect("second function must be lowered");
    let second_tail = second.body.tail.as_ref().expect("second has a tail expr");
    assert_eq!(second_tail.ty, ResolvedTy::Bool);
    assert!(
        matches!(&second_tail.kind, HirExprKind::TupleIndex { index: 1, .. }),
        "second must lower t.1 to TupleIndex(1), got: {:#?}",
        second_tail.kind
    );
}

#[test]
fn tuple_numeric_field_access_out_of_bounds_fails_closed_in_hir() {
    let source = "fn bad(t: (i64, bool)) -> i64 { t.2 }";
    let parsed = parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(
        !tco.errors.is_empty(),
        "checker must reject out-of-bounds tuple field access"
    );

    let output = lower_program(&parsed.program, &tco, &ResolutionCtx, TargetArch::host());
    assert!(
        output.diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            HirDiagnosticKind::CheckerBoundaryViolation { name, reason }
                if name == "tuple index .2" && reason.contains("out of range")
        )),
        "HIR must fail closed on poisoned/out-of-bounds tuple field access; got: {:#?}",
        output.diagnostics
    );
}

/// Verifier regression test: a `TupleLiteral` with 2 elements but a 3-tuple
/// type in `expr_types` must be caught by the verifier's arity check.
/// This pins the fail-closed boundary contract: the verifier never silently
/// accepts a tuple literal whose element count disagrees with its declared
/// type width.
#[test]
fn verifier_catches_arity_mismatch() {
    // Source: tuple literal with 2 elements.
    let source = "fn main() -> (i64, i64) { (1, 2) }";
    let parsed = parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );

    // Walk: items[0] → FnDecl → body.trailing_expr → tuple literal expression.
    let (Item::Function(fn_decl), _) = &parsed.program.items[0] else {
        panic!("expected Item::Function as first item");
    };
    let Some(boxed_expr) = &fn_decl.body.trailing_expr else {
        panic!("expected trailing expression; got None");
    };
    let (Expr::Tuple(elements), tuple_span) = boxed_expr.as_ref() else {
        panic!("expected Expr::Tuple; got: {boxed_expr:?}");
    };
    assert_eq!(elements.len(), 2, "tuple literal must have 2 elements");

    // Poison the expr_types entry: declare tuple literal as 3-tuple type,
    // but the literal has only 2 elements.
    let span_key = SpanKey {
        start: tuple_span.start,
        end: tuple_span.end,
    };
    let mut tc = TypeCheckOutput::default();
    tc.insert_expr_type(
        span_key,
        Ty::Tuple(vec![Ty::I64, Ty::I64, Ty::I64]), // 3-tuple, mismatched
    );

    let lower_output = lower_program(&parsed.program, &tc, &ResolutionCtx, TargetArch::host());

    // Lowering should not fail (it reads the type from the side-table),
    // but the verifier must catch the mismatch.
    assert!(
        lower_output.diagnostics.is_empty(),
        "lowering should not produce diagnostics; got: {:#?}",
        lower_output.diagnostics
    );

    let verify_diagnostics = hew_hir::verify_hir(&lower_output.module);

    // The verifier must produce a CheckerBoundaryViolation for the arity mismatch.
    let violations: Vec<_> = verify_diagnostics
        .iter()
        .filter(|d| matches!(&d.kind, HirDiagnosticKind::CheckerBoundaryViolation { .. }))
        .collect();
    assert!(
        !violations.is_empty(),
        "verifier must catch arity mismatch; got diagnostics: {verify_diagnostics:#?}",
    );

    // Verify the diagnostic names the tuple literal and mentions arity.
    let HirDiagnosticKind::CheckerBoundaryViolation { name, reason } = &violations[0].kind else {
        unreachable!()
    };
    assert_eq!(
        name, "tuple literal",
        "violation name must be 'tuple literal'"
    );
    assert!(
        reason.to_lowercase().contains("arity") || reason.to_lowercase().contains("element"),
        "reason must mention arity or element count; got: {reason:?}"
    );
}
