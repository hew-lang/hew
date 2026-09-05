use hew_hir::{
    lower_program_host_target, verify_hir, HirExprKind, HirItem, HirStmtKind, ResolutionCtx,
    ResolvedRef,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

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
