use hew_hir::{
    lower_program, verify_hir, BindingId, HirDiagnosticKind, HirExpr, HirExprKind,
    HirGenCaptureSource, HirItem, HirStmtKind, ResolutionCtx,
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

/// Type-check `source` and return the checker's error messages without lowering.
/// Used by reject-path regressions where the body is expected to be ill-typed.
fn typecheck_errors(source: &str) -> Vec<String> {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    tco.errors.iter().map(|e| e.message.clone()).collect()
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
        captures: _,
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
        captures: _,
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
        captures: _,
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
        captures: _,
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
fn actor_receive_gen_fn_rejects_non_unit_return() {
    // Regression (fail-open fix): an actor `receive gen fn`'s declared `-> T` is
    // the Yield element type; the body falls off the end with Unit, so an
    // explicit `return <T>;` must be rejected exactly like a standalone
    // `gen fn` — the value would otherwise be silently dropped. The checker
    // shapes `current_return_type` as `Generator<Yield = T, Return = Unit>`, so
    // `return 7;` is checked against the Unit Return component and fails.
    let errors = typecheck_errors(
        r"
        actor Seq {
            init() {}
            receive gen fn one_then_done() -> i64 { yield 42; return 7; }
        }
        fn main() { let _s = spawn Seq(); }
        ",
    );
    assert!(
        errors
            .iter()
            .any(|m| m.contains("expected `()`") && m.contains("found `i64`")),
        "expected a Unit/i64 return-type mismatch on the explicit `return 7;`, got: {errors:?}"
    );
}

#[test]
fn actor_receive_gen_fn_accepts_bare_return() {
    // Companion to the reject regression: a bare `return;` in an actor
    // `receive gen fn` is valid (it returns the Unit Return component) and must
    // not be flagged as a Unit-vs-`i64` mismatch. Before the fix the body was
    // checked against the bare yield type, so a bare return was wrongly rejected.
    let output = typecheck_and_lower(
        r"
        actor Seq {
            init() {}
            receive gen fn one_then_done() -> i64 { yield 42; return; }
        }
        fn main() { let _s = spawn Seq(); }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
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

#[test]
fn receive_gen_fn_state_field_capture_passes_verify() {
    // A `receive gen fn` reading an actor state field lowers that field as an
    // `ActorStateField`-tagged gen capture. Its binding id is synthetic —
    // `lower_actor_generator_body` mints it while binding the actor's state
    // fields into scope, and no `HirBinding` declaration node carries it — so
    // it is intentionally absent from the verifier's declared-binding set and
    // MUST be exempt from the generator DanglingRef gate. Before the exemption,
    // `verify_hir` wrongly rejected a stateful receive-gen as a dangling
    // capture, blocking the whole `hew run` pipeline before MIR.
    let output = typecheck_and_lower(
        r"
        actor Ticker {
            var base: i64;
            init(b: i64) { base = b; }
            receive gen fn stream(n: i64) -> i64 {
                var i = 0;
                while i < n {
                    yield base + i;
                    i = i + 1;
                }
            }
        }
        fn main() { let _t = spawn Ticker(b: 100); }
        ",
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );
    let verify = verify_hir(&output.module);
    assert!(
        verify.is_empty(),
        "state-field gen capture must pass verify (exemption): {verify:?}"
    );

    // Precisely: the `base` state field reached the capture list tagged
    // `ActorStateField` (the tag the exemption keys on).
    let actor = find_actor(&output, "Ticker");
    let handler = actor
        .receive_handlers
        .iter()
        .find(|receive| receive.name == "stream")
        .expect("stream generator receive handler should lower");
    let tail = handler
        .body
        .tail
        .as_ref()
        .expect("generator handler body must have a GenBlock tail");
    let HirExprKind::GenBlock { captures, .. } = &tail.kind else {
        panic!("expected GenBlock tail, got {:?}", tail.kind);
    };
    assert!(
        captures
            .iter()
            .any(|c| c.name == "base" && c.source == HirGenCaptureSource::ActorStateField),
        "`base` must be an ActorStateField-tagged capture: {captures:?}"
    );
}

#[test]
fn generator_local_capture_with_undeclared_binding_still_trips_dangling_ref() {
    // Guardrail: the DanglingRef exemption is NARROW. It applies ONLY to
    // synthetic `ActorStateField` captures. A `Local` capture (a `gen fn`'s
    // own param, a `gen {}` block's outer local, or a receive-gen handler
    // param) that references a binding not declared in the resolved HIR is a
    // resolver bug and MUST still be caught. Lower a real `gen fn` whose param
    // is a Local capture, corrupt that capture's binding id to an undeclared
    // sentinel, and confirm `verify_hir` still fires DanglingRef.
    let mut output = typecheck_and_lower(
        r"
        gen fn count(x: i64) -> i64 { yield x; }
        fn main() { let _g = count(1); }
        ",
    );
    // The un-corrupted module verifies clean — proving the DanglingRef below is
    // caused by the corruption, not a pre-existing defect.
    assert!(
        verify_hir(&output.module).is_empty(),
        "baseline gen fn must verify clean before corruption"
    );

    let func = output
        .module
        .items
        .iter_mut()
        .find_map(|item| match item {
            HirItem::Function(f) if f.name == "count" => Some(f),
            _ => None,
        })
        .expect("count gen fn should lower");
    let tail = func
        .body
        .tail
        .as_mut()
        .expect("gen fn body must have a GenBlock tail");
    let HirExprKind::GenBlock { captures, .. } = &mut tail.kind else {
        panic!("expected GenBlock tail");
    };
    assert_eq!(
        captures[0].source,
        HirGenCaptureSource::Local,
        "the gen fn param must be a Local capture"
    );
    // Point the Local capture at a binding id that no declaration mints.
    captures[0].binding = BindingId(u32::MAX);

    let verify = verify_hir(&output.module);
    assert!(
        verify
            .iter()
            .any(|d| matches!(d.kind, HirDiagnosticKind::DanglingRef { .. })),
        "a dangling Local gen capture must still trip DanglingRef: {verify:?}"
    );
}
