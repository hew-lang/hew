#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn retired_string_free_functions_are_not_source_builtins() {
    for expression in [
        "string_concat(\"a\", \"b\")",
        "string_length(\"a\")",
        "string_contains(\"a\", \"a\")",
        "substring(\"abc\", 0, 1)",
    ] {
        let parsed = hew_parser::parse(&format!("fn main() {{ let value = {expression}; }}"));
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
        assert!(
            output.errors.iter().any(|error| matches!(
                error.kind,
                TypeErrorKind::UndefinedFunction | TypeErrorKind::UndefinedVariable
            )),
            "retired call {expression} must be unresolved: {:?}",
            output.errors
        );
    }
}

#[test]
fn retired_bare_math_spellings_name_their_module_qualified_form() {
    for (expression, canonical) in [
        ("sqrt(9.0)", "math.sqrt"),
        ("abs(-5)", "math.abs"),
        ("min(1, 2)", "math.min"),
        ("max(1, 2)", "math.max"),
        ("pow(2.0, 3.0)", "math.pow"),
        ("floor(1.5)", "math.floor"),
        ("ceil(1.5)", "math.ceil"),
        ("round(1.5)", "math.round"),
    ] {
        let parsed = hew_parser::parse(&format!("fn main() {{ let value = {expression}; }}"));
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
        let undefined = output
            .errors
            .iter()
            .find(|error| error.kind == TypeErrorKind::UndefinedFunction)
            .unwrap_or_else(|| {
                panic!("bare `{expression}` must be undefined: {:?}", output.errors)
            });
        assert!(
            undefined
                .suggestions
                .iter()
                .any(|suggestion| suggestion.contains(canonical)
                    && suggestion.contains("import std.math")),
            "bare `{expression}` must point at `{canonical}`: {:?}",
            undefined.suggestions
        );
    }
}

#[test]
fn test_literal_types() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    // Integer literals synthesize as a first-class literal kind.
    let int_expr = make_int_literal(42, 0..2);
    let int_ty = checker.synthesize(&int_expr.0, &int_expr.1);
    assert_eq!(int_ty, Ty::IntLiteral);

    // Test boolean literal
    let bool_expr = make_bool_literal(true, 0..4);
    let bool_ty = checker.synthesize(&bool_expr.0, &bool_expr.1);
    assert_eq!(bool_ty, Ty::Bool);
}

#[test]
fn test_builtin_registration() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();

    // Check that println_int is registered
    assert!(checker.fn_sigs.contains_key("println_int"));
    let sig = &checker.fn_sigs["println_int"];
    assert_eq!(sig.params.len(), 1);
    assert_eq!(sig.params[0], Ty::I64);
    assert_eq!(sig.return_type, Ty::Unit);
}

#[test]
fn test_yield_outside_generator() {
    use hew_parser::ast::{Block, Expr, FnDecl, Item, Program, Stmt};

    let yield_expr: Spanned<Expr> = (Expr::Yield(None), 10..15);
    let body = Block {
        stmts: vec![(Stmt::Expression(yield_expr), 10..15)],
        trailing_expr: None,
    };
    let fd = FnDecl {
        origin: hew_parser::ast::DeclarationOrigin::Authored,
        attributes: vec![],
        is_generator: false,
        visibility: Visibility::Private,
        name: "not_a_gen".to_string(),
        type_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body,
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Function(fd), 0..30)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert!(output
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::YieldOutsideGenerator));
}

#[test]
fn test_receive_gen_fn_returns_stream() {
    use hew_parser::ast::{ActorDecl, Expr, Item, Literal, ReceiveFnDecl, Stmt, TypeExpr};

    let receive_fn = ReceiveFnDecl {
        is_generator: true,
        name: "numbers".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "i64".to_string(),
                type_args: None,
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![(
                Stmt::Expression((
                    Expr::Yield(Some(Box::new((
                        Expr::Literal(Literal::Integer {
                            value: 1,
                            radix: IntRadix::Decimal,
                        }),
                        0..0,
                    )))),
                    0..0,
                )),
                0..0,
            )],
            trailing_expr: None,
        },
        span: 0..0,
        attributes: vec![],
        doc_comment: None,
    };

    let actor = ActorDecl {
        visibility: Visibility::Pub,
        name: "NumberStream".to_string(),
        type_params: vec![],
        super_traits: None,
        init: None,
        fields: vec![],
        receive_fns: vec![receive_fn],
        methods: vec![],
        mailbox_capacity: None,
        overflow_policy: None,
        is_isolated: false,
        doc_comment: None,
        max_heap_bytes: None,
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Actor(actor), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert!(output.errors.is_empty());
    assert_eq!(
        output.fn_sigs["NumberStream::numbers"].return_type,
        Ty::stream(Ty::I64)
    );
}

#[test]
fn typecheck_generic_call_with_explicit_type_args() {
    // This test exercises generic type-arg resolution, not Rc safety.
    // The BorrowedParamReturn diagnostic on `identity` is expected and filtered.
    let source = concat!(
        "fn identity<T>(x: T) -> T { x }\n",
        "fn main() {\n",
        "    let a = identity<i64>(42);\n",
        "    let b = identity<string>(\"hello\");\n",
        "    println(a);\n",
        "    println(b);\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let unexpected: Vec<_> = output
        .errors
        .iter()
        .filter(|e| !matches!(e.kind, TypeErrorKind::BorrowedParamReturn))
        .collect();
    assert!(unexpected.is_empty(), "unexpected errors: {unexpected:?}");
}

#[test]
fn typecheck_generic_call_with_inferred_type_args() {
    // This test exercises generic type-arg resolution, not Rc safety.
    // The BorrowedParamReturn diagnostic on `identity` is expected and filtered.
    let source = concat!(
        "fn identity<T>(x: T) -> T { x }\n",
        "fn main() {\n",
        "    let a = identity(42);\n",
        "    let b = identity(\"hello\");\n",
        "    println(a);\n",
        "    println(b);\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let unexpected: Vec<_> = output
        .errors
        .iter()
        .filter(|e| !matches!(e.kind, TypeErrorKind::BorrowedParamReturn))
        .collect();
    assert!(unexpected.is_empty(), "unexpected errors: {unexpected:?}");
    assert!(
        output
            .call_type_args
            .values()
            .any(|args| args == &vec![Ty::I64]),
        "expected inferred i64 literal type args to materialize at output boundary, got {:?}",
        output.call_type_args
    );
}

#[test]
fn typecheck_generator_yield_uses_element_type() {
    let source = concat!(
        "gen fn count_up() -> i64 {\n",
        "    yield 1;\n",
        "    yield 2;\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_second_generator_yield_uses_element_type() {
    let source = concat!(
        "gen fn count_up() -> i64 {\n",
        "    yield 1;\n",
        "    yield 2;\n",
        "}\n"
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_generator_yield_mismatch_reports_element_type() {
    let source = "gen fn bad() -> i64 { yield \"oops\"; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.iter().any(|e| {
            matches!(
                &e.kind,
                TypeErrorKind::Mismatch { expected, actual }
                if expected == "i64" && actual == "string"
            )
        }),
        "expected element-type mismatch, got: {:?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| !e.message.contains("Generator<")),
        "yield diagnostic should mention element type, got: {:?}",
        output.errors
    );
}

#[test]
fn stream_lazy_adapters_fail_closed_with_one_honest_diagnostic() {
    // `Stream<T>.take/map/filter` type-check (they carry builtin signatures)
    // but have no MIR lowering: they previously routed to the legacy
    // `DeferToLowering` codegen path the Rust MIR pipeline does not consume, so
    // they dead-ended in HIR lowering with TWO misleading, internal-shaped
    // `E_NOT_YET_IMPLEMENTED` notes ("method-call rewrite variant" +
    // "Unsupported HIR node reached verification"). They must now fail closed at
    // the checker with exactly ONE honest, user-facing capability-boundary
    // diagnostic (issue #2530).
    for method in ["take", "map", "filter"] {
        let arg = if method == "take" { "3" } else { "|x| x" };
        let source =
            format!("fn use_stream(s: Stream<string>) {{\n    let _t = s.{method}({arg});\n}}\n");
        let (errors, _warnings) = parse_and_check(&source);
        let adapter_errors: Vec<_> = errors
            .iter()
            .filter(|e| e.kind.as_kind_str() == "StreamAdapterNotSupported")
            .collect();
        assert_eq!(
            adapter_errors.len(),
            1,
            "`Stream<string>.{method}` must emit exactly one StreamAdapterNotSupported \
             diagnostic; got errors: {errors:?}"
        );
        // No leftover internal-shaped NYI noise from the old DeferToLowering path.
        assert!(
            !errors
                .iter()
                .any(|e| e.kind.as_kind_str() == "NotYetImplemented"),
            "`Stream<string>.{method}` must not leak the internal NotYetImplemented \
             note; got errors: {errors:?}"
        );
        // The single diagnostic points at the supported alternative.
        assert!(
            adapter_errors[0].message.contains("for"),
            "the diagnostic must point at the supported `for` consumption \
             pattern; got: {}",
            adapter_errors[0].message
        );
    }
}

#[test]
fn stream_recv_still_type_checks_after_adapter_fail_closed() {
    // Guard: fail-closing the lazy adapters must not regress the supported
    // fundamental recv surface, which continues to type-check cleanly.
    let source = "fn use_stream(s: Stream<string>) {\n    let _m = s.recv();\n}\n";
    let (errors, _warnings) = parse_and_check(source);
    assert!(
        errors.is_empty(),
        "`Stream<string>.recv()` must still type-check cleanly; got: {errors:?}"
    );
}

#[test]
fn test_stream_annotation_resolves_to_stream_type() {
    use hew_parser::ast::{FnDecl, Item, TypeExpr};

    // Stream<i32> (the canonical name) must resolve to Ty::stream(Ty::I32).
    let fn_decl = FnDecl {
        origin: hew_parser::ast::DeclarationOrigin::Authored,
        attributes: vec![],
        is_generator: false,
        visibility: Visibility::Private,
        name: "foo".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "Stream".to_string(),
                type_args: Some(vec![(
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                )]),
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };

    let program = Program {
        module_graph: None,
        items: vec![(Item::Function(fn_decl), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    // The body is empty (returns unit) so there will be a return-type mismatch error,
    // but fn_sigs is populated in pass 1 (before body checking), so the signature
    // should already reflect the resolved return type.
    assert_eq!(output.fn_sigs["foo"].return_type, Ty::stream(Ty::I32));
}

#[test]
fn test_actor_stream_name_no_longer_aliases_stream() {
    use hew_parser::ast::{FnDecl, Item, TypeExpr};

    // ActorStream<i32> must NOT resolve to Ty::stream(Ty::I32) — the alias is removed.
    // It should resolve to Ty::Named { name: "ActorStream", .. } (an unknown named type).
    let fn_decl = FnDecl {
        origin: hew_parser::ast::DeclarationOrigin::Authored,
        attributes: vec![],
        is_generator: false,
        visibility: Visibility::Private,
        name: "bar".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "ActorStream".to_string(),
                type_args: Some(vec![(
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                )]),
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };

    let program = Program {
        module_graph: None,
        items: vec![(Item::Function(fn_decl), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    // The alias is removed and `ActorStream` is defined nowhere, so it resolves
    // to neither the built-in `stream` type nor any registered user type: the
    // undefined-named-type check reports it at the return-type annotation. A
    // regression that re-introduced the alias would make `ActorStream<i32>`
    // resolve to `Ty::stream(Ty::I32)` with no diagnostic, failing this
    // assertion — so this still pins the alias removal, now via the error path.
    assert!(
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::UndefinedType && error.message.contains("ActorStream")
        }),
        "ActorStream<i32> must be reported as an unknown type, not aliased to the Stream builtin: {:?}",
        output.errors
    );
}

#[test]
fn test_stream_canonical_name_still_resolves_after_actor_stream_removal() {
    use hew_parser::ast::{FnDecl, Item, TypeExpr};

    // Positive companion to test_actor_stream_name_no_longer_aliases_stream:
    // removing the ActorStream alias must not break resolution of the canonical
    // Stream<Y> name.
    let fn_decl = FnDecl {
        origin: hew_parser::ast::DeclarationOrigin::Authored,
        attributes: vec![],
        is_generator: false,
        visibility: Visibility::Private,
        name: "baz".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "Stream".to_string(),
                type_args: Some(vec![(
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                )]),
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
        fn_span: 0..0,
        intrinsic: None,
        consumes_self: false,
    };

    let program = Program {
        module_graph: None,
        items: vec![(Item::Function(fn_decl), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    // Stream<i32> must still resolve to the built-in stream type.
    assert_eq!(
        output.fn_sigs["baz"].return_type,
        Ty::stream(Ty::I32),
        "Stream<i32> (canonical name) must resolve to Ty::stream(Ty::I32)"
    );
}

#[test]
fn test_qualified_builtin_type_names_retain_presentation_and_builtin_identity() {
    let source = concat!(
        "import std.stream;\n",
        "import std.channel as channel_api;\n",
        "\n",
        "fn stream_id(s: stream.Stream<i64>) -> stream.Stream<i64> { s }\n",
        "fn close_sender(tx: channel_api.Sender) {\n",
        "    tx.close();\n",
        "}\n",
        "fn close_receiver(rx: channel_api.Receiver) {\n",
        "    rx.close();\n",
        "}\n",
    );
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(output.errors.is_empty(), "type errors: {:?}", output.errors);
    for ty in [
        &output.fn_sigs["stream_id"].params[0],
        &output.fn_sigs["stream_id"].return_type,
    ] {
        assert!(matches!(
            ty,
            Ty::Named {
                name,
                args,
                builtin: Some(crate::BuiltinType::Stream),
            } if name == "std.stream.Stream" && args == &[Ty::I64]
        ));
    }
    assert!(matches!(
        &output.fn_sigs["close_sender"].params[0],
        Ty::Named {
            name,
            builtin: Some(crate::BuiltinType::Sender),
            args,
        } if name == "std.channel.Sender" && args.len() == 1
    ));
    assert!(matches!(
        &output.fn_sigs["close_receiver"].params[0],
        Ty::Named {
            name,
            builtin: Some(crate::BuiltinType::Receiver),
            args,
        } if name == "std.channel.Receiver" && args.len() == 1
    ));
}

#[test]
fn direct_main_observations_require_actor_context() {
    for operation in ["link", "monitor"] {
        let parsed = hew_parser::parse(&format!(
            "actor Worker {{ receive fn ping() {{}} }} fn main() {{ let worker = spawn Worker; let _ = {operation}(worker); }}"
        ));
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let output = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.message.contains("E_ACTOR_CONTEXT_REQUIRED")),
            "{:?}",
            output.errors
        );
    }
}

/// A source declaration owns its name. `close` and `closed` are actor-handle
/// spellings, so a program that declares one of them keeps its own signature
/// while a handle call in the same program still reaches the builtin.
#[test]
fn a_declared_closed_keeps_its_signature_beside_the_handle_builtin() {
    let source = "actor Worker { receive fn ping() {} }\n\
         pub fn closed(value: i64, names: string) -> Result<(), string> {\n\
             if value > 0 { Ok(()) } else { Err(names) }\n\
         }\n\
         fn check() -> Result<(), string> { closed(7, \"ok\")?; Ok(()) }\n\
         fn main() {\n\
             let worker = spawn Worker();\n\
             let _sent = worker.ping();\n\
             close(worker);\n\
             let _checked = check();\n\
         }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

/// Negative control for the guard: with no declaration in scope both handle
/// builtins keep their own signature on a real pid.
#[test]
fn an_undeclared_close_and_closed_still_take_a_handle() {
    let source = "actor Worker { receive fn ping() {} }\n\
         fn main() {\n\
             let worker = spawn Worker();\n\
             let _sent = worker.ping();\n\
             close(worker);\n\
             closed(worker);\n\
         }";
    let output = check_source(source);
    assert!(output.errors.is_empty(), "{:?}", output.errors);
}

/// Negative control: without a declaration the builtin still owns the name and
/// still refuses a two-argument call.
#[test]
fn an_undeclared_closed_keeps_the_handle_builtin_arity() {
    let output = check_source("fn main() { closed(7, \"ok\"); }");
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("takes 1 argument")),
        "{:?}",
        output.errors
    );
}
