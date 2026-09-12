use hew_hir::{
    dump_hir, lower_program, verify_hir, HirDiagnosticKind, HirExprKind, HirItem, ResolutionCtx,
};
use hew_types::{module_registry::ModuleRegistry, Checker};
use hew_types::{BuiltinType, ResolvedTy, TryConversionKind};

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
fn lowers_try_to_numeric_method_to_hir_node() {
    let output = checked_lower(
        r"
        fn sample() -> Option<i32> {
            let x: i64 = 300;
            x.try_to_i32()
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
        dump_hir(&output.module).contains("try-width-cast IntToInt i64 -> i32"),
        "dump should expose the HIR try-width-cast node"
    );

    let HirItem::Function(function) = &output.module.items[0] else {
        panic!("expected function item");
    };
    let tail = function.body.tail.as_deref().expect("sample has tail expr");
    let HirExprKind::TryWidthCast {
        value: _,
        from_ty,
        to_ty,
        kind,
        ..
    } = &tail.kind
    else {
        panic!("expected tail TryWidthCast, got {tail:#?}");
    };
    assert_eq!(from_ty, &ResolvedTy::I64);
    assert_eq!(to_ty, &ResolvedTy::I32);
    assert_eq!(kind, &TryConversionKind::IntToInt);
    assert_eq!(
        tail.ty,
        ResolvedTy::Named {
            name: "Option".to_string(),
            args: vec![ResolvedTy::I32],
            builtin: Some(BuiltinType::Option),
            is_opaque: false,
        }
    );
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

#[test]
fn numeric_conversion_bounds_follow_requested_target() {
    let parsed = hew_parser::parse(
        "fn checked(n: i64) -> Option<usize> { n.try_to_usize() }\n\
         fn clamped(n: u64) -> isize { n.saturating_as_isize() }",
    );
    assert!(parsed.errors.is_empty());
    for (target, signed_max, unsigned_max) in [
        (
            hew_hir::TargetArch::Wasm32,
            i128::from(i32::MAX),
            i128::from(u32::MAX),
        ),
        (
            hew_hir::TargetArch::X86_64,
            i128::from(i64::MAX),
            i128::from(u64::MAX),
        ),
    ] {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        if target == hew_hir::TargetArch::Wasm32 {
            checker.enable_wasm_target();
        }
        let facts = checker.check_program(&parsed.program);
        assert!(facts.errors.is_empty(), "{:?}", facts.errors);
        let output = lower_program(&parsed.program, &facts, &ResolutionCtx, target);
        assert!(output.diagnostics.is_empty(), "{:?}", output.diagnostics);
        let HirItem::Function(function) = &output.module.items[0] else {
            panic!("checked function")
        };
        let HirExprKind::TryWidthCast { to_range, .. } = &function.body.tail.as_ref().unwrap().kind
        else {
            panic!("checked cast")
        };
        assert_eq!(*to_range, Some((0, unsigned_max)));
        let HirItem::Function(function) = &output.module.items[1] else {
            panic!("clamped function")
        };
        let HirExprKind::SaturatingWidthCast { to_range, .. } =
            &function.body.tail.as_ref().unwrap().kind
        else {
            panic!("clamped cast")
        };
        assert_eq!(*to_range, Some((-signed_max - 1, signed_max)));
    }
}

#[test]
fn numeric_conversion_methods_reject_arguments_at_source_boundary() {
    for method in ["try_to_u8", "wrapping_as_u8", "saturating_as_u8"] {
        let source = format!("fn main() {{ let n: i64 = 300; let bad = n.{method}(1); }}");
        let parsed = hew_parser::parse(&source);
        assert!(parsed.errors.is_empty());
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let facts = checker.check_program(&parsed.program);
        assert!(
            facts
                .errors
                .iter()
                .any(|error| error.kind == hew_types::error::TypeErrorKind::ArityMismatch),
            "{method}: {:?}",
            facts.errors
        );
    }
}
