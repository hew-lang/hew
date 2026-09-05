use hew_hir::{
    lower_program_host_target, verify_hir, HirExprKind, HirItem, HirStmtKind, ResolutionCtx,
    ResolvedRef,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let lowered = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(lowered.diagnostics.is_empty(), "{:?}", lowered.diagnostics);
    let diagnostics = verify_hir(&lowered.module);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    lowered
}

fn function<'a>(lowered: &'a hew_hir::LowerOutput, name: &str) -> &'a hew_hir::HirFn {
    lowered
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == name => Some(function),
            _ => None,
        })
        .expect("source function")
}

#[test]
fn fallible_returns_use_one_result_representation_with_exact_payloads() {
    for (source, expected_variant, payload_ty) in [
        (
            "fn f() -> (i64, string) fails string { return (10, \"hello\"); }",
            0,
            ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::String]),
        ),
        (
            "fn f() -> i64 fails string { return error \"missing\"; }",
            1,
            ResolvedTy::String,
        ),
        ("fn f() -> () fails string { return; }", 0, ResolvedTy::Unit),
    ] {
        let lowered = lower(source);
        let function = function(&lowered, "f");
        let value = match &function.body.statements[0].kind {
            HirStmtKind::Return(Some(value)) => value,
            HirStmtKind::Expr(value) => {
                let HirExprKind::Return { value: Some(value) } = &value.kind else {
                    panic!("typed error return");
                };
                value
            }
            other => panic!("return statement: {other:?}"),
        };
        assert_eq!(value.ty, function.return_ty);
        let HirExprKind::MachineVariantCtor {
            machine_name,
            state_idx,
            payload: Some(payload),
        } = &value.kind
        else {
            panic!("Result constructor");
        };
        assert_eq!(machine_name, "Result");
        assert_eq!(*state_idx, expected_variant);
        assert_eq!(payload.len(), 1);
        assert_eq!(payload[0].1.ty, payload_ty);
    }
}

#[test]
fn fallible_tails_wrap_success_including_result_valued_success() {
    for source in [
        "fn f() -> i64 fails string { 7 }",
        "fn f() -> () fails string {}",
        "fn f(value: Result<i64, string>) -> Result<i64, string> fails bool { value }",
    ] {
        let lowered = lower(source);
        let function = function(&lowered, "f");
        let tail = function.body.tail.as_ref().expect("success return");
        assert_eq!(tail.ty, function.return_ty);
        assert!(
            matches!(&tail.kind, HirExprKind::MachineVariantCtor { machine_name, state_idx: 0, payload: Some(payload) } if machine_name == "Result" && payload.len() == 1)
        );
    }
}

#[test]
fn lazy_default_places_the_fallback_call_only_in_the_absence_arm() {
    let lowered = lower("fn source(value: Option<i64>) -> Option<i64> { value } fn fallback() -> i64 { 7 } fn f(value: Option<i64>) -> i64 { source(value) ?? fallback() }");
    let tail = function(&lowered, "f")
        .body
        .tail
        .as_ref()
        .expect("recovery tail");
    let HirExprKind::Match { scrutinee, arms } = &tail.kind else {
        panic!("typed recovery branch");
    };
    assert!(
        matches!(&scrutinee.kind, HirExprKind::Call { target: hew_types::CallTarget::User(id), .. } if id == &function(&lowered, "source").declaration)
    );
    assert_eq!(arms.len(), 2);
    for (arm, name) in arms.iter().zip(["Some", "None"]) {
        assert!(
            matches!(&arm.predicate, hew_hir::HirMatchArmPredicate::EnumVariant { variant_match, .. }
            if variant_match.type_name == "Option" && variant_match.variant_name == name)
        );
    }
    assert!(
        matches!(&arms[0].body.kind, HirExprKind::BindingRef { resolved: ResolvedRef::Binding(id), .. } if *id == arms[0].bindings[0].binding)
    );
    assert!(arms[1].bindings.is_empty());
    assert!(
        matches!(&arms[1].body.kind, HirExprKind::Call { target: hew_types::CallTarget::User(id), .. } if id == &function(&lowered, "fallback").declaration)
    );
}

#[test]
fn local_handler_passes_the_resolved_error_payload_to_an_ordinary_call() {
    let lowered = lower("fn recover(problem: string) -> i64 { 7 } fn f(value: Result<i64, string>) -> i64 { value handle problem { recover(problem) } }");
    let tail = function(&lowered, "f")
        .body
        .tail
        .as_ref()
        .expect("recovery tail");
    let HirExprKind::Match { arms, .. } = &tail.kind else {
        panic!("typed Result recovery branch");
    };
    let error = &arms[1].bindings[0];
    for (arm, name) in arms.iter().zip(["Ok", "Err"]) {
        assert!(
            matches!(&arm.predicate, hew_hir::HirMatchArmPredicate::EnumVariant { variant_match, .. }
            if variant_match.type_name == "Result" && variant_match.variant_name == name)
        );
    }
    assert_eq!(error.name, "problem");
    assert_eq!(error.ty, ResolvedTy::String);
    assert_eq!(error.field_idx, 0);
    let HirExprKind::Block(block) = &arms[1].body.kind else {
        panic!("lexical handler block");
    };
    let HirExprKind::Call { args, .. } = &block.tail.as_ref().expect("recovery call").kind else {
        panic!("ordinary recovery call");
    };
    assert!(
        matches!(&args[0].kind, HirExprKind::BindingRef { resolved: ResolvedRef::Binding(id), .. } if *id == error.binding)
    );
}

#[test]
fn local_handler_preserves_divergent_lexical_return() {
    let lowered =
        lower("fn f(value: Result<i64, string>) -> i64 { value handle problem { return 0; } }");
    let HirExprKind::Match { arms, .. } = &function(&lowered, "f")
        .body
        .tail
        .as_ref()
        .expect("tail")
        .kind
    else {
        panic!("recovery match");
    };
    let HirExprKind::Block(block) = &arms[1].body.kind else {
        panic!("handler block");
    };
    assert!(matches!(block.statements[0].kind, HirStmtKind::Return(_)));
    assert_eq!(arms[1].body.ty, ResolvedTy::Never);
}

#[test]
fn local_handler_closure_captures_the_error_binding_by_identity() {
    let lowered = lower("fn f(value: Result<i64, string>) -> i64 { value handle problem { let describe = || problem; describe(); 7 } }");
    let HirExprKind::Match { arms, .. } = &function(&lowered, "f")
        .body
        .tail
        .as_ref()
        .expect("tail")
        .kind
    else {
        panic!("recovery match");
    };
    let HirExprKind::Block(block) = &arms[1].body.kind else {
        panic!("handler block");
    };
    let HirStmtKind::Let(_, Some(closure)) = &block.statements[0].kind else {
        panic!("closure binding");
    };
    let HirExprKind::Closure { captures, .. } = &closure.kind else {
        panic!("ordinary closure");
    };
    assert_eq!(captures.len(), 1);
    assert_eq!(captures[0].binding, arms[1].bindings[0].binding);
    assert_eq!(captures[0].ty, ResolvedTy::String);
}

#[test]
fn propagation_inside_handler_operand_retains_its_lexical_return_edge() {
    let lowered = lower("fn f(value: Result<Result<i64, string>, string>) -> Result<i64, string> { let number = value? handle problem { 0 }; Ok(number) }");
    let HirStmtKind::Let(_, Some(recovery)) = &function(&lowered, "f").body.statements[0].kind
    else {
        panic!("local result binding");
    };
    let HirExprKind::Match { scrutinee, .. } = &recovery.kind else {
        panic!("outer handler");
    };
    let HirExprKind::Match { arms, .. } = &scrutinee.kind else {
        panic!("inner propagation");
    };
    let HirExprKind::Block(failure) = &arms[1].body.kind else {
        panic!("propagation failure block");
    };
    assert!(matches!(failure.statements[0].kind, HirStmtKind::Return(_)));
}

#[test]
fn required_optional_binding_preserves_payload_identity_and_failure_branch() {
    let source =
        "fn f(value: Option<i64>) -> i64 { let number = value else { return 0; }; number }";
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let lowered = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(lowered.diagnostics.is_empty(), "{:?}", lowered.diagnostics);
    assert!(verify_hir(&lowered.module).is_empty());
    let function = lowered
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "f" => Some(function),
            _ => None,
        })
        .expect("source function");
    let HirStmtKind::LetElse {
        scrutinee,
        bindings,
        else_body,
        ..
    } = &function.body.statements[0].kind
    else {
        panic!("required binding must branch on the optional value");
    };
    assert!(
        matches!(&scrutinee.kind, HirExprKind::BindingRef { resolved: ResolvedRef::Binding(id), .. } if *id == function.params[0].id)
    );
    assert_eq!(bindings.len(), 1);
    assert_eq!(bindings[0].ty, ResolvedTy::I64);
    assert_eq!(bindings[0].field_idx, 0);
    assert_eq!(bindings[0].name, "number");
    assert!(
        matches!(&function.body.tail.as_ref().expect("payload tail").kind, HirExprKind::BindingRef { resolved: ResolvedRef::Binding(id), .. } if *id == bindings[0].binding)
    );
    assert!(matches!(
        else_body.statements[0].kind,
        HirStmtKind::Return(_)
    ));
}
