use hew_hir::{
    lower_program, verify_hir, HirExpr, HirExprKind, HirItem, HirStmtKind, ResolutionCtx,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn typecheck_and_lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    assert!(tco.errors.is_empty(), "type errors: {:?}", tco.errors);

    lower_program(
        &parsed.program,
        &tco,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

fn main_body(output: &hew_hir::LowerOutput) -> &hew_hir::HirBlock {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == "main" => Some(&function.body),
            _ => None,
        })
        .expect("expected lowered main function")
}

fn binding_initializer<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a HirExpr {
    let body = main_body(output);
    body.statements
        .iter()
        .find_map(|stmt| match &stmt.kind {
            HirStmtKind::Let(binding, Some(init)) if binding.name == name => Some(init),
            _ => None,
        })
        .unwrap_or_else(|| panic!("expected let binding initializer named {name}"))
}

fn assert_generator_type(ty: &ResolvedTy, yield_ty: &ResolvedTy, return_ty: &ResolvedTy) {
    let ResolvedTy::Named { name, args } = ty else {
        panic!("expected Generator<Yield, Return>, got {ty:?}");
    };
    assert_eq!(name, "Generator");
    assert_eq!(args, &vec![yield_ty.clone(), return_ty.clone()]);
}

fn yield_values(block: &hew_hir::HirBlock) -> Vec<(&Option<Box<HirExpr>>, &ResolvedTy)> {
    block
        .statements
        .iter()
        .filter_map(|stmt| match &stmt.kind {
            HirStmtKind::Expr(expr) => match &expr.kind {
                HirExprKind::Yield { value, yield_ty } => Some((value, yield_ty)),
                other => panic!("expected yield expression in gen body, got {other:?}"),
            },
            _ => None,
        })
        .collect()
}

#[test]
fn gen_block_with_two_yields_lowers_to_typed_hir() {
    let output = typecheck_and_lower(
        r"
        fn main() {
            let g = gen { yield 1; yield 2; };
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "verify diagnostics: {verify:?}");

    let init = binding_initializer(&output, "g");
    assert_generator_type(&init.ty, &ResolvedTy::I64, &ResolvedTy::Unit);
    let HirExprKind::GenBlock {
        body,
        yield_ty,
        return_ty,
    } = &init.kind
    else {
        panic!("expected GenBlock initializer, got {:?}", init.kind);
    };
    assert_eq!(yield_ty, &ResolvedTy::I64);
    assert_eq!(return_ty, &ResolvedTy::Unit);

    let yields = yield_values(body);
    assert_eq!(yields.len(), 2);
    for (value, yield_ty) in yields {
        assert_eq!(yield_ty, &ResolvedTy::I64);
        let value = value.as_ref().expect("yield must carry value");
        assert_eq!(value.ty, ResolvedTy::I64);
    }
}

#[test]
fn gen_block_with_string_yield_and_i64_return_lowers_types() {
    let output = typecheck_and_lower(
        r#"
        fn main() {
            let g = gen { yield "x"; return 42; };
        }
        "#,
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "verify diagnostics: {verify:?}");

    let init = binding_initializer(&output, "g");
    assert_generator_type(&init.ty, &ResolvedTy::String, &ResolvedTy::I64);
    let HirExprKind::GenBlock {
        body,
        yield_ty,
        return_ty,
    } = &init.kind
    else {
        panic!("expected GenBlock initializer, got {:?}", init.kind);
    };
    assert_eq!(yield_ty, &ResolvedTy::String);
    assert_eq!(return_ty, &ResolvedTy::I64);

    let yields = yield_values(body);
    assert_eq!(yields.len(), 1);
    let (value, yield_ty) = yields[0];
    assert_eq!(yield_ty, &ResolvedTy::String);
    let value = value.as_ref().expect("yield must carry value");
    assert_eq!(value.ty, ResolvedTy::String);
    assert!(
        matches!(body.statements[1].kind, HirStmtKind::Return(Some(_))),
        "expected explicit return after yield"
    );
}

#[test]
fn gen_block_as_value_is_binding_initializer() {
    let output = typecheck_and_lower(
        r"
        fn main() {
            let g = gen { yield 1; };
        }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );

    let body = main_body(&output);
    let HirStmtKind::Let(binding, Some(init)) = &body.statements[0].kind else {
        panic!("expected first main statement to bind gen block");
    };
    assert_eq!(binding.name, "g");
    assert!(matches!(init.kind, HirExprKind::GenBlock { .. }));
    assert_generator_type(&init.ty, &ResolvedTy::I64, &ResolvedTy::Unit);
}
