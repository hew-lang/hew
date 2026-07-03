use hew_hir::{lower_program, verify_hir, HirExprKind, HirItem, ResolutionCtx};
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
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "HIR verify: {verify:#?}");
    output
}

fn assert_main_tail_call_resolves(source: &str, expected: &str) {
    let output = lower(source);
    let main_fn = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "main" => Some(function),
            _ => None,
        })
        .expect("main function must be present");
    let tail = main_fn.body.tail.as_deref().expect("main must have tail");
    let HirExprKind::Call { callee, .. } = &tail.kind else {
        panic!("main tail must be a call, got {tail:#?}");
    };
    let HirExprKind::BindingRef { name, resolved } = &callee.kind else {
        panic!("callee must be a binding ref, got {callee:#?}");
    };
    assert_eq!(name, expected);
    assert!(
        matches!(resolved, hew_hir::ResolvedRef::Item(_)),
        "{expected} must resolve to a synthetic stdlib item, got {resolved:?}"
    );
}

#[test]
fn free_math_builtins_resolve_to_hir_items() {
    for (name, source) in [
        ("sqrt", "fn main() -> f64 { sqrt(9.0) }"),
        ("abs", "fn main() -> i64 { abs(-9) }"),
        ("min", "fn main() -> i64 { min(7, 3) }"),
        ("max", "fn main() -> i64 { max(7, 3) }"),
        ("pow", "fn main() -> f64 { pow(2, 10) }"),
        ("floor", "fn main() -> f64 { floor(2.75) }"),
        ("ceil", "fn main() -> f64 { ceil(2.25) }"),
        ("round", "fn main() -> f64 { round(2.5) }"),
    ] {
        assert_main_tail_call_resolves(source, name);
    }
}

#[test]
fn generic_math_module_calls_dispatch_to_concrete_intrinsics() {
    for (expected, source) in [
        ("abs", "fn main() -> i64 { math.abs(-9) }"),
        ("abs_f", "fn main() -> f64 { math.abs(-9.0) }"),
        ("min", "fn main() -> i64 { math.min(7, 3) }"),
        ("min_f", "fn main() -> f64 { math.min(7.0, 3.0) }"),
        ("max", "fn main() -> i64 { math.max(7, 3) }"),
        ("max_f", "fn main() -> f64 { math.max(7.0, 3.0) }"),
    ] {
        assert_main_tail_call_resolves(source, expected);
    }
}
