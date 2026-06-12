use hew_hir::{
    dump_hir, lower_program, verify_hir, HirDiagnosticKind, HirExprKind, HirItem, ResolutionCtx,
};
use hew_types::ResolvedTy;
use hew_types::{module_registry::ModuleRegistry, Checker};

fn checked_lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:#?}", tco.errors);
    lower_program(
        &parsed.program,
        &tco,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

#[test]
fn lowers_checker_admitted_numeric_cast_to_hir_node() {
    let output = checked_lower(
        r"
        fn main() -> i64 {
            let x: i64 = 300;
            let y: i32 = x as i32;
            y as i64
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "HIR diagnostics: {:?}",
        output.diagnostics
    );
    assert!(verify_hir(&output.module).is_empty());
    assert!(
        dump_hir(&output.module).contains("numeric-cast i32 -> i64"),
        "dump should expose the HIR cast node"
    );

    let HirItem::Function(function) = &output.module.items[0] else {
        panic!("expected function item");
    };
    let tail = function.body.tail.as_deref().expect("main has tail expr");
    let HirExprKind::NumericCast {
        value: _,
        from_ty,
        to_ty,
    } = &tail.kind
    else {
        panic!("expected tail NumericCast, got {tail:#?}");
    };
    assert_eq!(from_ty, &ResolvedTy::I32);
    assert_eq!(to_ty, &ResolvedTy::I64);
}

#[test]
fn non_numeric_cast_fails_closed_in_hir_lowering() {
    let parsed = hew_parser::parse(
        r#"
        fn main() -> i64 {
            let s: string = "not numeric";
            s as i64
        }
        "#,
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tco,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );

    assert!(
        output.diagnostics.iter().any(|diag| matches!(
            &diag.kind,
            HirDiagnosticKind::CheckerBoundaryViolation { name, reason }
                if name == "numeric cast" && reason.contains("outside the checker-admitted numeric matrix")
        )),
        "expected numeric-cast fail-closed diagnostic, got: {:?}",
        output.diagnostics
    );
    assert!(output.into_result().is_err());
}
