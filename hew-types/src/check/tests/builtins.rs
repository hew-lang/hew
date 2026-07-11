#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

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
        attributes: vec![],
        is_async: false,
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
fn typecheck_async_generator_yield_uses_element_type() {
    let source = concat!(
        "async gen fn count_up() -> i64 {\n",
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
            adapter_errors[0].message.contains("for await"),
            "the diagnostic must point at the supported `for await` consumption \
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
        attributes: vec![],
        is_async: false,
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
        attributes: vec![],
        is_async: false,
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
        attributes: vec![],
        is_async: false,
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
fn test_qualified_builtin_type_names_canonicalize_in_signatures() {
    let source = concat!(
        "import std::stream;\n",
        "import std::channel::channel;\n",
        "\n",
        "fn stream_id(s: stream.Stream<i64>) -> stream.Stream<i64> { s }\n",
        "fn close_sender(tx: channel.Sender) {\n",
        "    tx.close();\n",
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
    assert_eq!(output.fn_sigs["stream_id"].params[0], Ty::stream(Ty::I64));
    assert_eq!(output.fn_sigs["stream_id"].return_type, Ty::stream(Ty::I64));
    assert!(matches!(
        &output.fn_sigs["close_sender"].params[0],
        Ty::Named {
            builtin: Some(crate::BuiltinType::Sender),
            args,
            ..
        } if args.len() == 1
    ));
}
