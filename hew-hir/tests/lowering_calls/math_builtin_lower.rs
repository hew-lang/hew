use hew_hir::{lower_program, verify_hir, HirExprKind, HirItem, ResolutionCtx};
use hew_types::{
    module_registry::ModuleRegistry, runtime_call::MathIntrinsic, Checker, RuntimeCallFamily,
};

fn lower(source: &str) -> hew_hir::LowerOutput {
    let source = format!("import std::math;\n{source}");
    let parsed = hew_parser::parse(&source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-hir crate must live below repo root")
        .to_path_buf();
    let mut checker = Checker::new(ModuleRegistry::new(vec![repo_root]));
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

fn assert_main_tail_call_resolves(source: &str, expected: &str, expected_intrinsic: MathIntrinsic) {
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
    assert_eq!(
        *resolved,
        hew_hir::ResolvedRef::Builtin(RuntimeCallFamily::MathIntrinsic(expected_intrinsic)),
        "{expected} must resolve to its typed math intrinsic"
    );
}

#[test]
fn math_builtin_identifiers_resolve_to_typed_intrinsics() {
    for (name, intrinsic, source) in [
        (
            "sqrt",
            MathIntrinsic::Sqrt,
            "fn main() -> f64 { math.sqrt(9.0) }",
        ),
        (
            "exp",
            MathIntrinsic::Exp,
            "fn main() -> f64 { math.exp(1.0) }",
        ),
        (
            "log",
            MathIntrinsic::Log,
            "fn main() -> f64 { math.log(1.0) }",
        ),
        (
            "sin",
            MathIntrinsic::Sin,
            "fn main() -> f64 { math.sin(1.0) }",
        ),
        (
            "cos",
            MathIntrinsic::Cos,
            "fn main() -> f64 { math.cos(1.0) }",
        ),
        (
            "abs",
            MathIntrinsic::AbsI64,
            "fn main() -> i64 { math.abs(-9) }",
        ),
        (
            "min",
            MathIntrinsic::MinI64,
            "fn main() -> i64 { math.min(7, 3) }",
        ),
        (
            "max",
            MathIntrinsic::MaxI64,
            "fn main() -> i64 { math.max(7, 3) }",
        ),
        (
            "abs_f",
            MathIntrinsic::AbsF64,
            "fn main() -> f64 { math.abs(-9.0) }",
        ),
        (
            "min_f",
            MathIntrinsic::MinF64,
            "fn main() -> f64 { math.min(7.0, 3.0) }",
        ),
        (
            "max_f",
            MathIntrinsic::MaxF64,
            "fn main() -> f64 { math.max(7.0, 3.0) }",
        ),
        (
            "pow",
            MathIntrinsic::Pow,
            "fn main() -> f64 { math.pow(2.0, 10.0) }",
        ),
        (
            "floor",
            MathIntrinsic::Floor,
            "fn main() -> f64 { math.floor(2.75) }",
        ),
        (
            "ceil",
            MathIntrinsic::Ceil,
            "fn main() -> f64 { math.ceil(2.25) }",
        ),
        (
            "round",
            MathIntrinsic::Round,
            "fn main() -> f64 { math.round(2.5) }",
        ),
    ] {
        assert_main_tail_call_resolves(source, name, intrinsic);
    }
}
