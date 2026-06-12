use hew_hir::{lower_program, HirExprKind, HirItem, HirStmtKind, ResolutionCtx, ValueClass};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{BuiltinType, Checker, ResolvedTy};

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

fn main_fn(out: &hew_hir::LowerOutput) -> &hew_hir::HirFn {
    out.module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(func) if func.name == "observe" => Some(func),
            _ => None,
        })
        .expect("observe function must lower")
}

#[test]
fn lowers_cancellation_token_local_and_is_cancelled_intrinsic() {
    let out = lower(
        r"
        fn observe(token: CancellationToken) -> bool {
            let t: CancellationToken = token;
            return t.is_cancelled();
        }
        ",
    );

    assert!(
        out.diagnostics.is_empty(),
        "CancellationToken HIR lowering should be diagnostic-free: {:#?}",
        out.diagnostics
    );
    assert_eq!(
        out.module.type_classes.get("CancellationToken"),
        Some(&(
            hew_hir::ResourceMarker::Resource,
            Some("release".to_string())
        )),
        "CancellationToken must be registered as a ref-counted resource whose drop releases a ref"
    );

    let func = main_fn(&out);
    let let_ty = func
        .body
        .statements
        .iter()
        .find_map(|stmt| match &stmt.kind {
            HirStmtKind::Let(binding, Some(_)) if binding.name == "t" => Some(&binding.ty),
            _ => None,
        })
        .expect("token local should lower");
    assert_eq!(let_ty, &ResolvedTy::CancellationToken);

    let ret = func
        .body
        .statements
        .iter()
        .find_map(|stmt| match &stmt.kind {
            HirStmtKind::Return(Some(expr)) => Some(expr),
            _ => None,
        })
        .expect("return expression should lower");
    let HirExprKind::CancellationTokenIsCancelled { receiver } = &ret.kind else {
        panic!("is_cancelled() must lower to the token intrinsic, got {ret:#?}");
    };
    assert_eq!(ret.ty, ResolvedTy::Bool);
    assert_eq!(receiver.ty, ResolvedTy::CancellationToken);
    assert_eq!(receiver.value_class, ValueClass::AffineResource);
}

#[test]
fn cancellation_token_resolved_type_carries_builtin_identity() {
    let named = hew_types::Ty::normalize_named("CancellationToken".to_string(), Vec::new());
    assert_eq!(named, hew_types::Ty::CancellationToken);
    assert_eq!(
        hew_types::lookup_builtin_type("CancellationToken"),
        Some(BuiltinType::CancellationToken)
    );
}
