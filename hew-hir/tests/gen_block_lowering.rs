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
    let ResolvedTy::Named { name, args, .. } = ty else {
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

fn function_named<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a hew_hir::HirFn {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Function(function) if function.name == name => Some(function),
            _ => None,
        })
        .unwrap_or_else(|| panic!("expected lowered function named {name}"))
}

#[test]
fn gen_fn_lowers_to_generator_returning_fn_with_genblock_body() {
    // Slice 0: a `gen fn` threads its `-> T` declaration as the Yield element
    // type, marks the HirFn `is_generator`, and routes the body through the
    // same GenBlock path the `gen { ... }` block expression uses — so MIR and
    // codegen construction is shared between both surfaces.
    let output = typecheck_and_lower(
        r"
        gen fn count() -> i64 { yield 1; yield 2; yield 3 }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "verify diagnostics: {verify:?}");

    let function = function_named(&output, "count");
    assert!(
        function.is_generator,
        "gen fn must carry is_generator on its HirFn"
    );
    // The function value is Generator<Yield = i64, Return = Unit>, matching the
    // checker's registered signature (Ty::generator(declared, Unit)).
    assert_generator_type(&function.return_ty, &ResolvedTy::I64, &ResolvedTy::Unit);

    let tail = function
        .body
        .tail
        .as_ref()
        .expect("gen fn body must have a GenBlock tail expression");
    let HirExprKind::GenBlock {
        body,
        yield_ty,
        return_ty,
    } = &tail.kind
    else {
        panic!("expected GenBlock tail, got {:?}", tail.kind);
    };
    assert_eq!(yield_ty, &ResolvedTy::I64);
    assert_eq!(return_ty, &ResolvedTy::Unit);
    assert_generator_type(&tail.ty, &ResolvedTy::I64, &ResolvedTy::Unit);

    // The two semicolon-terminated yields are statements; the final
    // `yield 3` (no trailing semicolon) is the block tail expression.
    let stmt_yields = yield_values(body);
    assert_eq!(stmt_yields.len(), 2, "expected two statement yields");
    for (value, yield_ty) in stmt_yields {
        assert_eq!(yield_ty, &ResolvedTy::I64);
        let value = value.as_ref().expect("yield must carry value");
        assert_eq!(value.ty, ResolvedTy::I64);
    }
    let tail_yield = body
        .tail
        .as_ref()
        .expect("final yield with no trailing semicolon is the body tail");
    let HirExprKind::Yield {
        value,
        yield_ty: tail_yield_ty,
    } = &tail_yield.kind
    else {
        panic!("expected tail yield, got {:?}", tail_yield.kind);
    };
    assert_eq!(tail_yield_ty, &ResolvedTy::I64);
    assert_eq!(
        value.as_ref().expect("tail yield must carry value").ty,
        ResolvedTy::I64
    );
}

fn find_actor<'a>(output: &'a hew_hir::LowerOutput, name: &str) -> &'a hew_hir::HirActorDecl {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Actor(actor) if actor.name == name => Some(actor),
            _ => None,
        })
        .unwrap_or_else(|| panic!("expected lowered actor named {name}"))
}

#[test]
fn actor_receive_gen_fn_yield_lowers_through_genblock() {
    // Regression: `yield` inside an actor `receive gen fn` used to find an
    // empty `generator_yield_tys` stack during HIR lowering ("yield expression
    // has no enclosing generator yield type") because `lower_actor_receive_fn`
    // never pushed the handler's Yield type the way `lower_generator_fn_body`
    // does for a standalone `gen fn`. The handler now lowers its body through
    // the shared `GenBlock` path: the outer block is a thin wrapper whose tail
    // is a `GenBlock` typed `Generator<Yield = i64, Return = Unit>`, with the
    // yields inside the GenBlock body. This is the producer-side analogue of
    // `gen_fn_lowers_to_generator_returning_fn_with_genblock_body`.
    let output = typecheck_and_lower(
        r"
        actor Seq {
            init() {}
            receive gen fn count_up() -> i64 { yield 1; yield 2; }
        }
        fn main() { let _s = spawn Seq(); }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(verify.is_empty(), "verify diagnostics: {verify:?}");

    let actor = find_actor(&output, "Seq");
    let handler = actor
        .receive_handlers
        .iter()
        .find(|receive| receive.name == "count_up")
        .expect("count_up generator receive handler should lower");
    assert!(
        handler.is_generator,
        "receive gen fn must carry is_generator"
    );
    // `return_ty` stays the declared element type (the protocol/ABI layer
    // consults it as the stream element type); the body tail carries the
    // generator handle type.
    assert_eq!(handler.return_ty, ResolvedTy::I64);
    assert!(
        handler.body.statements.is_empty(),
        "generator handler outer block should be a thin GenBlock wrapper"
    );
    let tail = handler
        .body
        .tail
        .as_ref()
        .expect("generator handler body must have a GenBlock tail");
    let HirExprKind::GenBlock {
        body,
        yield_ty,
        return_ty,
    } = &tail.kind
    else {
        panic!("expected GenBlock tail, got {:?}", tail.kind);
    };
    assert_eq!(yield_ty, &ResolvedTy::I64);
    assert_eq!(return_ty, &ResolvedTy::Unit);
    assert_generator_type(&tail.ty, &ResolvedTy::I64, &ResolvedTy::Unit);

    // The first `yield 1;` is a statement; the trailing `yield 2;` (with a
    // semicolon) is also a statement, so the GenBlock body has two statement
    // yields and no tail yield.
    let yields = yield_values(body);
    assert_eq!(yields.len(), 2, "expected two statement yields in gen body");
    for (value, yield_ty) in yields {
        assert_eq!(yield_ty, &ResolvedTy::I64);
        let value = value.as_ref().expect("yield must carry value");
        assert_eq!(value.ty, ResolvedTy::I64);
    }
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
