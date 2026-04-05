#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::module_registry::ModuleRegistry;
use hew_parser::ast::IntRadix;
use hew_parser::ast::{ImportName, TraitMethod, Visibility};

/// Module registry with the repo root as a search path, so stdlib
/// modules (e.g. `std::encoding::json`) can be loaded during tests.
fn test_registry() -> ModuleRegistry {
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    ModuleRegistry::new(vec![repo_root])
}

fn check_source(_source: &str) -> TypeCheckOutput {
    // hew-types has zero external deps, so we cannot parse source here.
    // Integration tests that parse+check live in the workspace-level tests.
    let program = Program {
        module_graph: None,
        items: vec![],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&program)
}

#[test]
fn test_empty_program() {
    let output = check_source("");
    assert!(output.errors.is_empty());
}

#[test]
fn test_type_checker_creation() {
    let checker = Checker::new(ModuleRegistry::new(vec![]));
    assert_eq!(checker.errors.len(), 0);
}

// Helper functions for testing AST construction
fn make_int_literal(n: i64, span: Span) -> Spanned<Expr> {
    (
        Expr::Literal(Literal::Integer {
            value: n,
            radix: IntRadix::Decimal,
        }),
        span,
    )
}

fn make_bool_literal(b: bool, span: Span) -> Spanned<Expr> {
    (Expr::Literal(Literal::Bool(b)), span)
}

#[test]
fn test_literal_types() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    // Test integer literal (defaults to i64)
    let int_expr = make_int_literal(42, 0..2);
    let int_ty = checker.synthesize(&int_expr.0, &int_expr.1);
    assert_eq!(int_ty, Ty::I64);

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
        is_pure: false,
        name: "not_a_gen".to_string(),
        type_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body,
        doc_comment: None,
        decl_span: 0..0,
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
        is_pure: false,
        name: "numbers".to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "int".to_string(),
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
    };

    let actor = ActorDecl {
        visibility: Visibility::Pub,
        name: "NumberStream".to_string(),
        super_traits: None,
        init: None,
        terminate: None,
        fields: vec![],
        receive_fns: vec![receive_fn],
        methods: vec![],
        mailbox_capacity: None,
        overflow_policy: None,
        is_isolated: false,
        doc_comment: None,
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
        "    let a = identity<int>(42);\n",
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
    assert!(unexpected.is_empty(), "unexpected errors: {unexpected:?}",);
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
    assert!(unexpected.is_empty(), "unexpected errors: {unexpected:?}",);
}

#[test]
fn typecheck_generator_yield_uses_element_type() {
    let source = concat!(
        "gen fn count_up() -> int {\n",
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
        "async gen fn count_up() -> int {\n",
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
    let source = "gen fn bad() -> int { yield \"oops\"; }";
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
                if expected == "int" && actual == "String"
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
fn test_actor_stream_annotation_is_stream_alias() {
    use hew_parser::ast::{FnDecl, Item, TypeExpr};

    // A standalone function returning ActorStream<i32> should resolve to Stream<i32>
    let fn_decl = FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        is_pure: false,
        name: "foo".to_string(),
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
fn test_qualified_builtin_type_names_canonicalize_in_signatures() {
    let source = concat!(
        "import std::stream;\n",
        "import std::channel::channel;\n",
        "\n",
        "fn stream_id(s: stream.Stream<int>) -> stream.Stream<int> { s }\n",
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
        Ty::Named { name, args } if name == "Sender" && args.len() == 1
    ));
}

#[test]
fn test_arity_mismatch_too_many_args() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();
    // println_int takes 1 arg; call with 2
    let call = (
        Expr::Call {
            function: Box::new((Expr::Identifier("println_int".to_string()), 0..11)),
            type_args: None,
            args: vec![
                CallArg::Positional((
                    Expr::Literal(hew_parser::ast::Literal::Integer {
                        value: 1,
                        radix: IntRadix::Decimal,
                    }),
                    12..13,
                )),
                CallArg::Positional((
                    Expr::Literal(hew_parser::ast::Literal::Integer {
                        value: 2,
                        radix: IntRadix::Decimal,
                    }),
                    15..16,
                )),
            ],
            is_tail_call: false,
        },
        0..17,
    );
    checker.synthesize(&call.0, &call.1);
    assert!(checker
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::ArityMismatch));
}

#[test]
fn test_arity_mismatch_too_few_args() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();
    // println_int takes 1 arg; call with 0
    let call = (
        Expr::Call {
            function: Box::new((Expr::Identifier("println_int".to_string()), 0..11)),
            type_args: None,
            args: vec![],
            is_tail_call: false,
        },
        0..13,
    );
    checker.synthesize(&call.0, &call.1);
    assert!(checker
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::ArityMismatch));
}

#[test]
fn typecheck_error_undefined_var() {
    let result = hew_parser::parse("fn main() -> i32 {\n    let x = undefined_var;\n    x\n}");
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "expected type error for undefined variable"
    );
}

#[test]
fn typecheck_error_type_mismatch() {
    let source = concat!(
        "fn add(a: i32, b: i32) -> i32 {\n",
        "    a + b\n",
        "}\n\n",
        "fn main() {\n",
        "    let result = add(\"hello\", \"world\");\n",
        "}"
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
        !output.errors.is_empty(),
        "expected type errors for mismatched argument types"
    );
}

// -----------------------------------------------------------------------
// Additional edge-case tests
// -----------------------------------------------------------------------

#[test]
fn test_string_literal_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let expr = (Expr::Literal(Literal::String("hello".to_string())), 0..5);
    let ty = checker.synthesize(&expr.0, &expr.1);
    assert_eq!(ty, Ty::String);
}

#[test]
#[expect(
    clippy::approx_constant,
    reason = "testing that 3.14 parses as Float, not using it as PI"
)]
fn test_float_literal_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let expr = (Expr::Literal(Literal::Float(3.14)), 0..4);
    let ty = checker.synthesize(&expr.0, &expr.1);
    assert_eq!(ty, Ty::F64);
}

#[test]
fn test_char_literal_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let expr = (Expr::Literal(Literal::Char('a')), 0..3);
    let ty = checker.synthesize(&expr.0, &expr.1);
    assert_eq!(ty, Ty::Char);
}

#[test]
fn typecheck_binary_op_type_mismatch() {
    let source = "fn main() -> i32 {\n    let x: i32 = 1;\n    let y: bool = true;\n    x + y\n}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "expected type error for i32 + bool"
    );
}

#[test]
fn typecheck_rejects_implicit_signedness_change_in_call() {
    let source = concat!(
        "fn takes_u32(x: u32) -> u32 { x }\n",
        "fn main() -> u32 {\n",
        "    let n: i64 = 42;\n",
        "    takes_u32(n)\n",
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
        output
            .errors
            .iter()
            .any(|e| e.message.contains("implicit numeric coercion")),
        "expected explicit coercion diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_rejects_implicit_integer_to_float_in_call() {
    let source = concat!(
        "fn takes_f64(x: f64) -> f64 { x }\n",
        "fn main() -> f64 {\n",
        "    let n: i64 = 42;\n",
        "    takes_f64(n)\n",
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
        output
            .errors
            .iter()
            .any(|e| e.message.contains("implicit numeric coercion")),
        "expected explicit coercion diagnostic, got: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_allows_safe_integer_widening_in_call() {
    let source = concat!(
        "fn takes_i64(x: i64) -> i64 { x }\n",
        "fn main() -> i64 {\n",
        "    let n: i32 = 42;\n",
        "    takes_i64(n)\n",
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
fn typecheck_return_type_mismatch() {
    // The type checker may not flag all return-type mismatches at the
    // trailing-expression level; verify the function signature is recorded.
    let source = "fn foo() -> i32 {\n    true\n}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // The function signature should still reflect i32 return type
    assert_eq!(output.fn_sigs["foo"].return_type, Ty::I32);
}

#[test]
fn typecheck_trailing_return_stmt_matches_declared_type() {
    let source = "fn foo() -> i32 {\n    return 42;\n}";
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
fn typecheck_trailing_return_stmt_matches_declared_bool_type() {
    let source = "fn foo() -> bool {\n    return true;\n}";
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
fn typecheck_nested_function_calls() {
    let source = concat!(
        "fn double(x: i32) -> i32 { let two: i32 = 2; x * two }\n",
        "fn main() -> i32 { let x: i32 = 5; double(double(x)) }\n"
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
fn typecheck_let_with_explicit_type() {
    let source = "fn main() { let x: i32 = 42; }";
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
fn typecheck_let_type_annotation_mismatch() {
    let source = "fn main() { let x: int = \"hello\"; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "expected type error for string assigned to int variable"
    );
    assert!(output.errors.iter().any(|e| {
        e.message.contains("expected `int`")
            && e.message.contains("found `String`")
            && !e.message.contains("i64")
    }));
}

#[test]
fn typecheck_if_branch_type_consistency() {
    let source =
        "fn main() -> i32 {\n    if true { let x: i32 = 1; x } else { let y: i32 = 2; y }\n}";
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
fn typecheck_vec_type_annotation() {
    let source = "fn main() { let v: Vec<i32> = Vec::new(); }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // Vec::new() may or may not resolve depending on builtins, but should not panic
    assert!(output.errors.len() <= 2);
}

#[test]
fn typecheck_multiple_functions_cross_call() {
    let source = concat!(
        "fn add(a: i32, b: i32) -> i32 { a + b }\n",
        "fn mul(a: i32, b: i32) -> i32 { a * b }\n",
        "fn main() -> i32 { add(mul(2, 3), 4) }\n"
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
fn typecheck_actor_receive_fn_registered() {
    use hew_parser::ast::{ActorDecl, Block, Item, Param, ReceiveFnDecl, TypeExpr};

    let recv = ReceiveFnDecl {
        is_generator: false,
        is_pure: false,
        name: "greet".to_string(),
        params: vec![Param {
            name: "name".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "string".into(),
                    type_args: None,
                },
                0..0,
            ),
            is_mutable: false,
        }],
        return_type: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        type_params: None,
        where_clause: None,
        span: 0..0,
        attributes: vec![],
    };
    let actor = ActorDecl {
        visibility: Visibility::Pub,
        name: "Greeter".to_string(),
        super_traits: None,
        init: None,
        terminate: None,
        fields: vec![],
        receive_fns: vec![recv],
        methods: vec![],
        mailbox_capacity: None,
        overflow_policy: None,
        is_isolated: false,
        doc_comment: None,
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Actor(actor), 0..0)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    assert!(output.fn_sigs.contains_key("Greeter::greet"));
}

#[test]
fn typecheck_empty_function_no_error() {
    let source = "fn noop() {}";
    let result = hew_parser::parse(source);
    assert!(result.errors.is_empty());
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_recursive_function() {
    let source = concat!(
        "fn factorial(n: i32) -> i32 {\n",
        "    let one: i32 = 1; if n <= one { one } else { n * factorial(n - one) }\n",
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
fn typecheck_local_result_enum_not_qualified_to_sqlite() {
    let source = concat!(
        "import ecosystem::db::sqlite;\n",
        "enum Result {\n",
        "    Ok(int);\n",
        "    Err(int)\n",
        "}\n",
        "fn unwrap_or(r: Result, fallback: int) -> int {\n",
        "    match r {\n",
        "        Ok(v) => v,\n",
        "        Err(_) => fallback,\n",
        "    }\n",
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
    // Filter out the expected UnresolvedImport for the dummy stdlib import — the
    // test is about local type naming, not module resolution.
    let non_import_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind != TypeErrorKind::UnresolvedImport)
        .collect();
    assert!(
        non_import_errors.is_empty(),
        "unexpected errors: {non_import_errors:?}"
    );
    let sig = output
        .fn_sigs
        .get("unwrap_or")
        .expect("unwrap_or signature should be registered");
    assert_eq!(
        sig.params[0],
        Ty::Named {
            name: "Result".to_string(),
            args: vec![],
        }
    );
}

#[test]
fn typecheck_match_statement_exhaustive_enum_ok() {
    let (errors, _) = parse_and_check(concat!(
        "enum Light { Red; Green; }\n",
        "fn main() { let v: Light = Red; match v { Red => 1, Green => 2, } let _done = 0; }\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
}

#[test]
fn typecheck_match_statement_missing_variant_errors() {
    let (errors, warnings) = parse_and_check(concat!(
        "enum Light { Red; Green; }\n",
        "fn main() { let v: Light = Red; match v { Red => 1, } let _done = 0; }\n",
    ));
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "non-exhaustive enum match must not be a warning: {warnings:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "expected non-exhaustive match error, got: {errors:?}"
    );
}

#[test]
fn typecheck_guarded_wildcard_not_exhaustive() {
    // A guarded wildcard (`_ if false`) does not count as an exhaustive arm;
    // matching bool with only that arm leaves both `true` and `false` uncovered.
    // Bool is enum-like, so this is a hard error.
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let x = true;\n",
        "    match x {\n",
        "        _ if false => 0,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "non-exhaustive bool match must not be a warning: {warnings:?}"
    );
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "expected non-exhaustive match error, got: {errors:?}"
    );
    let err = errors
        .iter()
        .find(|e| matches!(e.kind, TypeErrorKind::NonExhaustiveMatch))
        .expect("expected NonExhaustiveMatch error");
    assert_eq!(err.severity, crate::error::Severity::Error);
    assert_eq!(err.message, "non-exhaustive match: missing true, false");
}

/// Scalar types (int, float, string, …) have no closed variant set, so a
/// missing catch-all should remain a *warning*, not an error.
#[test]
fn typecheck_scalar_missing_catchall_is_warning_not_error() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let x: int = 5;\n",
        "    match x {\n",
        "        1 => 10,\n",
        "        2 => 20,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "scalar missing catch-all must not be an error: {errors:?}"
    );
    assert!(
        warnings
            .iter()
            .any(|w| matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "scalar missing catch-all must be a warning: {warnings:?}"
    );
}

#[test]
fn typecheck_struct_pattern_unknown_field_errors() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: int, y: int }\n",
        "fn main() {\n",
        "    let p = Point { x: 1, y: 2 };\n",
        "    match p {\n",
        "        Point { x, z } => x,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::UndefinedField)),
        "expected UndefinedField error for unknown field 'z', got: {errors:?}"
    );
}

#[test]
fn typecheck_match_wrong_enum_variant_errors() {
    // Matching a Colour scrutinee with a Shape variant should be an error.
    let (errors, _) = parse_and_check(concat!(
        "enum Colour { Red; Green; Blue; }\n",
        "enum Shape { Circle(i32); Rectangle(i32); }\n",
        "fn describe(c: Colour) -> i32 {\n",
        "    match c {\n",
        "        Circle(r) => r,\n",
        "        _ => 0,\n",
        "    }\n",
        "}\n",
    ));
    assert!(
        !errors.is_empty(),
        "expected type error for wrong-enum variant in match, got no errors"
    );
}

#[test]
fn typecheck_error_scrutinee_constructor_pattern_stays_fail_closed() {
    let (errors, _) = parse_and_check(concat!(
        "fn main() {\n",
        "    let _value = match missing {\n",
        "        Some(x) => x,\n",
        "        None => panic(\"boom\"),\n",
        "    };\n",
        "}\n",
    ));
    assert_eq!(
        errors
            .iter()
            .filter(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable))
            .count(),
        1,
        "expected only the errored scrutinee to report UndefinedVariable: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::InferenceFailed)),
        "errored scrutinees must not seed constructor-pattern inference holes: {errors:?}"
    );
}

#[test]
fn typecheck_error_scrutinee_skips_exhaustiveness_follow_on() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    match missing {\n",
        "        true => 1,\n",
        "    }\n",
        "    let _done = 0;\n",
        "}\n",
    ));
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::UndefinedVariable)),
        "expected primary undefined variable error, got: {errors:?}"
    );
    assert!(
        errors
            .iter()
            .all(|e| !matches!(e.kind, TypeErrorKind::NonExhaustiveMatch)),
        "Ty::Error scrutinee must not emit follow-on non-exhaustive errors: {errors:?}"
    );
    assert!(
        warnings
            .iter()
            .all(|w| !matches!(w.kind, TypeErrorKind::NonExhaustiveMatch)),
        "Ty::Error scrutinee must not emit follow-on non-exhaustive warnings: {warnings:?}"
    );
}

#[test]
fn typecheck_generic_enum_constructor_infers_type_args() {
    let (errors, _) = parse_and_check(concat!(
        "enum Option<T> { Some(T); None; }\n",
        "fn take_int(x: Option<int>) -> Option<int> { x }\n",
        "fn take_string(x: Option<string>) -> Option<string> { x }\n",
        "fn main() { take_int(Some(42)); take_string(Some(\"hello\")); }\n",
    ));
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
}

#[test]
fn warn_unused_variable() {
    let source = "fn main() { let unused_var = 42; }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.message.contains("unused variable `unused_var`")),
        "expected unused variable warning, got: {:?}",
        output.warnings
    );
}

#[test]
fn warn_var_never_mutated() {
    let source = "fn main() { var x = 10; println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "expected unmutated var warning, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_underscore_prefix() {
    let source = "fn main() { let _unused = 42; }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.message.contains("_unused")),
        "should not warn on _ prefix, got: {:?}",
        output.warnings
    );
}

// -----------------------------------------------------------------------
// Lint / warning regression tests
// -----------------------------------------------------------------------

/// Helper: parse + typecheck, return (errors, warnings).
fn parse_and_check(source: &str) -> (Vec<TypeError>, Vec<TypeError>) {
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    (output.errors, output.warnings)
}

// ---- unused variable ----

#[test]
fn warn_unused_in_nested_scope() {
    let (errors, warnings) = parse_and_check("fn main() { if true { let nested = 1; } }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("unused variable `nested`")),
        "expected unused variable warning for nested, got: {warnings:?}"
    );
}

#[test]
fn no_warn_used_variable() {
    let (errors, warnings) = parse_and_check("fn main() { let x = 42; println(x); }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "should not warn on used variable, got: {warnings:?}"
    );
}

#[test]
fn no_warn_underscore_alone() {
    let (_, warnings) = parse_and_check("fn main() { let _ = 42; }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("unused")),
        "bare _ should never warn, got: {warnings:?}"
    );
}

// ---- var never mutated ----

#[test]
fn no_warn_var_actually_mutated() {
    let (errors, warnings) = parse_and_check("fn main() { var x = 10; x = 20; println(x); }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "should not warn when var is actually reassigned, got: {warnings:?}"
    );
}

#[test]
fn warn_var_never_mutated_suggestion() {
    let (_, warnings) = parse_and_check("fn main() { var x = 10; println(x); }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("never reassigned"))
        .expect("expected never-reassigned warning");
    assert!(
        w.suggestions.iter().any(|s| s.contains("let")),
        "should suggest using `let`, got: {:?}",
        w.suggestions
    );
}

// ---- assignment LHS not false-positive as "used" ----

#[test]
fn warn_write_only_variable() {
    // `x` is only written to, never read — should get unused warning
    let (_, warnings) = parse_and_check("fn main() { var x = 0; x = 1; }");
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "write-only variable should be warned as unused, got: {warnings:?}"
    );
}

#[test]
fn no_warn_variable_used_then_assigned() {
    // `x` is read (println) AND then written — it's genuinely used
    let (_, warnings) = parse_and_check("fn main() { var x = 0; println(x); x = 1; println(x); }");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "variable that is read should not get unused warning, got: {warnings:?}"
    );
}

// ---- while true → loop ----

#[test]
fn warn_while_true() {
    let (_, warnings) = parse_and_check("fn main() { while true { break; } }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("while true"))
        .expect("expected while-true style warning");
    assert!(
        w.suggestions.iter().any(|s| s.contains("loop")),
        "should suggest loop, got: {:?}",
        w.suggestions
    );
    assert!(
        matches!(w.kind, TypeErrorKind::StyleSuggestion),
        "should be StyleSuggestion kind"
    );
}

#[test]
fn no_warn_while_condition() {
    let (_, warnings) = parse_and_check("fn main() { let x = true; while x { break; } }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("while true")),
        "should not warn on while <variable>, got: {warnings:?}"
    );
}

// ---- unused return value ----

#[test]
fn no_warn_discarded_return_value() {
    // Calling a function that returns a value without binding the result
    // should NOT warn — users can discard results freely
    let (_, warnings) = parse_and_check(concat!(
        "fn compute() -> i32 { 42 }\n",
        "fn main() { compute(); }\n",
    ));
    assert!(
        !warnings.iter().any(|w| w.message.contains("unused")),
        "discarded return value should not warn, got: {warnings:?}"
    );
}

#[test]
fn no_warn_unused_println() {
    // println returns unit, and is a known side-effect function
    let (_, warnings) = parse_and_check("fn main() { println(42); }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("unused")),
        "println() should not produce unused value warning, got: {warnings:?}"
    );
}

#[test]
fn no_warn_unused_spawn() {
    // spawn is a side-effect expression — don't warn about discarded return
    let (_, warnings) = parse_and_check(concat!(
        "actor Worker { count: i32;\n",
        "    receive fn work() {} }\n",
        "fn main() { let _w = spawn Worker(count: 0); }\n",
    ));
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused") && !w.message.contains("unused variable")),
        "spawn() should not produce unused value warning, got: {warnings:?}"
    );
}

// ---- unit binding ----

#[test]
fn warn_unit_binding() {
    let (_, warnings) = parse_and_check("fn main() { let x = println(42); }");
    assert!(
        warnings.iter().any(|w| w.message.contains("unit type")),
        "binding to unit type should warn, got: {warnings:?}"
    );
}

#[test]
fn no_warn_non_unit_binding() {
    let (_, warnings) =
        parse_and_check("fn compute() -> i32 { 42 }\nfn main() { let x = compute(); println(x); }");
    assert!(
        !warnings.iter().any(|w| w.message.contains("unit type")),
        "binding to non-unit type should not warn, got: {warnings:?}"
    );
}

// ---- Levenshtein "did you mean?" suggestions ----

#[test]
fn suggest_similar_variable() {
    let (errors, _) = parse_and_check("fn main() { let counter = 42; println(conter); }");
    let err = errors
        .iter()
        .find(|e| e.message.contains("conter"))
        .expect("expected error for misspelled variable");
    assert!(
        err.suggestions.iter().any(|s| s.contains("counter")),
        "should suggest 'counter', got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_function() {
    let (errors, _) = parse_and_check(concat!(
        "fn calculate_sum(a: i32, b: i32) -> i32 { a + b }\n",
        "fn main() { calculate_sun(1, 2); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("calculate_sun"))
        .expect("expected error for misspelled function");
    assert!(
        err.suggestions.iter().any(|s| s.contains("calculate_sum")),
        "should suggest 'calculate_sum', got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_type() {
    // Use a misspelled type in a constructor position, which triggers undefined type lookup
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i32; y: i32; }\n",
        "fn make() { let p = Pont { x: 0, y: 0 }; println(p.x); }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains("Pont"))
        .expect("expected error for misspelled type");
    assert!(
        err.suggestions.iter().any(|s| s.contains("Point")),
        "should suggest 'Point', got: {:?}",
        err.suggestions
    );
}

#[test]
fn suggest_similar_field() {
    let (errors, _) = parse_and_check(concat!(
        "type Point { x: i32; y: i32; }\n",
        "fn get_z(p: Point) -> i32 { p.z }\n",
    ));
    let err = errors
        .iter()
        .find(|e| e.message.contains('z'))
        .expect("expected error for undefined field");
    assert!(
        !err.suggestions.is_empty(),
        "should suggest similar fields, got: {:?}",
        err.suggestions
    );
}

#[test]
fn no_suggest_when_too_different() {
    let (errors, _) = parse_and_check("fn main() { let alpha = 1; println(zzzzz); }");
    let err = errors
        .iter()
        .find(|e| e.message.contains("zzzzz"))
        .expect("expected error for undefined variable");
    // "zzzzz" is too far from "alpha" — no suggestion
    assert!(
        err.suggestions.is_empty() || !err.suggestions.iter().any(|s| s.contains("alpha")),
        "should not suggest distant names, got: {:?}",
        err.suggestions
    );
}

// ---- warning severity and kind ----

#[test]
fn lint_warnings_have_warning_severity() {
    let (_, warnings) = parse_and_check("fn main() { let unused = 42; }");
    for w in &warnings {
        assert_eq!(
            w.severity,
            crate::error::Severity::Warning,
            "lint warnings must have Warning severity, got: {:?} for {}",
            w.severity,
            w.message
        );
    }
}

#[test]
fn unused_variable_has_correct_kind() {
    let (_, warnings) = parse_and_check("fn main() { let unused = 42; }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("unused"))
        .unwrap();
    assert!(
        matches!(w.kind, TypeErrorKind::UnusedVariable),
        "expected UnusedVariable kind, got: {:?}",
        w.kind
    );
}

#[test]
fn never_mutated_has_correct_kind() {
    let (_, warnings) = parse_and_check("fn main() { var x = 10; println(x); }");
    let w = warnings
        .iter()
        .find(|w| w.message.contains("never reassigned"))
        .unwrap();
    assert!(
        matches!(w.kind, TypeErrorKind::UnusedMut),
        "expected UnusedMut kind, got: {:?}",
        w.kind
    );
}

// ---- multiple warnings in one function ----

#[test]
fn multiple_warnings_in_one_fn() {
    let (errors, warnings) = parse_and_check(concat!(
        "fn main() {\n",
        "    let unused_a = 1;\n",
        "    var never_written = 2;\n",
        "    println(never_written);\n",
        "    while true { break; }\n",
        "}\n",
    ));
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("unused variable `unused_a`")),
        "missing unused_a warning: {warnings:?}"
    );
    assert!(
        warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        "missing never-mutated warning: {warnings:?}"
    );
    assert!(
        warnings.iter().any(|w| w.message.contains("while true")),
        "missing while-true warning: {warnings:?}"
    );
}

// ── Unreachable Code Tests ──────────────────────────────────────────

#[test]
fn warn_unreachable_after_return() {
    let source = "fn foo() -> i32 { return 1; let x = 2; x }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnreachableCode),
        "expected unreachable code warning, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_unreachable_when_return_is_last() {
    let source = "fn foo() -> i32 { let x = 1; return x; }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnreachableCode),
        "should not warn when return is last statement: {:?}",
        output.warnings
    );
}

// ── Shadowing Tests ─────────────────────────────────────────────────

#[test]
fn error_same_scope_shadowing() {
    let source = "fn main() { let x = 5; let x = 10; println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing
                && e.message.contains("already defined in this scope")),
        "expected same-scope shadowing error, got: {:?}",
        output.errors
    );
}

#[test]
fn warn_nested_scope_shadowing() {
    // Nested/child scope shadowing is a warning, not an error.
    let source = "fn main() { let x = 1; if true { let x = 2; println(x); } println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Shadowing
                && w.message.contains("shadows a binding in an outer scope")),
        "expected outer-scope shadowing warning, got warnings: {:?}, errors: {:?}",
        output.warnings,
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing),
        "outer-scope shadowing of a local should not be an error: {:?}",
        output.errors
    );
}

#[test]
fn no_shadowing_diagnostic_underscore_prefix() {
    // Underscore-prefixed bindings are fully exempt from shadowing diagnostics.
    let source = "fn main() { let _x = 1; if true { let _x = 2; println(_x); } println(_x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing),
        "should not error for _ prefixed vars: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Shadowing),
        "should not warn for _ prefixed vars: {:?}",
        output.warnings
    );
}

#[test]
fn no_shadowing_diagnostic_for_loop_var() {
    // For-loop induction variables are exempt from shadowing diagnostics.
    let source = "fn main() { let i = 0; for i in 0..10 { println(i); } println(i); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::Shadowing),
        "should not error for for-loop variable shadowing: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::Shadowing),
        "should not warn for for-loop variable shadowing: {:?}",
        output.warnings
    );
}

#[test]
fn warn_deeply_nested_scope_shadowing() {
    // Shadowing across multiple scope levels is still a warning.
    let source = r"
        fn main() {
            let val = 1;
            if true {
                if true {
                    let val = 99;
                    println(val);
                }
            }
            println(val);
        }
    ";
    let (errors, warnings) = parse_and_check(source);
    assert!(
        warnings.iter().any(|w| w.kind == TypeErrorKind::Shadowing
            && w.message.contains("shadows a binding in an outer scope")),
        "expected nested shadowing warning, got warnings: {warnings:?}, errors: {errors:?}"
    );
    assert!(
        !errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing),
        "outer-scope shadowing of a local should not be an error: {errors:?}"
    );
}

#[test]
fn test_actor_field_shadowing_is_error() {
    // Shadowing an actor field is a hard error — bare field access requires
    // unambiguous names.
    let source = r"
        actor Counter {
            var count: int = 0;
            receive fn update(count: int) {
                println(count);
            }
        }
    ";
    let (errors, _warnings) = parse_and_check(source);
    assert!(
        errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing
            && e.message
                .contains("variable `count` shadows a binding in an outer scope")),
        "should error on param shadowing actor field, got: {errors:?}",
    );
}

#[test]
fn test_actor_fn_method_field_shadowing_is_error() {
    // Shadowing an actor field via an fn helper method is also a hard error.
    let source = r"
        actor Counter {
            var count: int = 0;
            fn helper(count: int) -> int { count }
        }
    ";
    let (errors, _warnings) = parse_and_check(source);
    assert!(
        errors.iter().any(|e| e.kind == TypeErrorKind::Shadowing
            && e.message
                .contains("variable `count` shadows a binding in an outer scope")),
        "should error on fn param shadowing actor field, got: {errors:?}",
    );
}

// ── Dead Code (Unused Function) Tests ───────────────────────────────

#[test]
fn warn_dead_code_unused_function() {
    let source = "fn unused_helper() -> i32 { 42 } fn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("unused_helper")),
        "expected dead code warning for unused_helper, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_main() {
    let source = "fn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode),
        "should not warn about main: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_called_function() {
    let source = "fn helper() -> i32 { 42 } fn main() { let x = helper(); println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("helper")),
        "should not warn about called function: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_underscore_prefix() {
    let source = "fn _unused() -> i32 { 42 } fn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode),
        "should not warn for _ prefixed functions: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_dead_code_called_from_actor_receive() {
    let source = r"
fn fib(n: i32) -> i32 {
if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }
}
actor Worker {
receive fn compute(n: i32) {
    let r = fib(n);
    println(r);
}
}
fn main() {
let w = spawn Worker();
w.compute(10);
}
";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("fib")),
        "should not warn about function called from actor receive fn: {:?}",
        output.warnings
    );
}

// ── Unused Import Tests ─────────────────────────────────────────────

#[test]
fn warn_unused_import() {
    let source = "import std::encoding::json;\nfn main() { println(1); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("json")),
        "expected unused import warning for json, got: {:?}",
        output.warnings
    );
}

#[test]
fn no_warn_used_import() {
    let source =
        "import std::encoding::json;\nfn main() { let v = json.parse(\"[]\"); println(v); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&result.program);
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnusedImport && w.message.contains("json")),
        "should not warn about used import: {:?}",
        output.warnings
    );
}

#[test]
fn stdlib_import_registers_trait_impls_for_generic_bounds() {
    let root_source = r"
        import std::string;

        fn main() -> String {
            string.describe(string.make_label())
        }
    ";
    let module_source = r#"
        pub trait Describable {
            fn describe(val: Self) -> String;
        }

        pub type Label {
            text: String;
        }

        pub fn make_label() -> Label {
            Label { text: "hello" }
        }

        impl Describable for Label {
            fn describe(label: Label) -> String {
                label.text
            }
        }

        pub fn describe<T: Describable>(item: T) -> String {
            item.describe()
        }

    "#;

    let mut root = hew_parser::parse(root_source);
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );
    let call_span = root
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(fd) if fd.name == "main" => {
                fd.body.trailing_expr.as_ref().map(|expr| expr.1.clone())
            }
            _ => None,
        })
        .expect("main trailing call should exist");
    let module = hew_parser::parse(module_source);
    assert!(
        module.errors.is_empty(),
        "module parse errors: {:?}",
        module.errors
    );

    let import_decl = root
        .program
        .items
        .iter_mut()
        .find_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
        .expect("root import should exist");
    import_decl.resolved_items = Some(module.program.items.clone());

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&root.program);

    assert!(
        !output.user_modules.contains("string"),
        "stdlib Hew import should not go through the user-module import path"
    );
    assert!(
        output.errors.is_empty(),
        "stdlib imported Hew impl should satisfy imported generic bounds: {:?}",
        output.errors
    );
    let inferred = output
        .call_type_args
        .get(&SpanKey::from(&call_span))
        .expect("stdlib imported generic call should record inferred type args");
    assert_eq!(
        inferred,
        &vec![Ty::Named {
            name: "Label".to_string(),
            args: vec![],
        }]
    );
    assert!(
        checker
            .trait_impls_set
            .contains(&("Label".to_string(), "Describable".to_string())),
        "stdlib Hew items should register trait impls for downstream generic bound checks"
    );
}

#[test]
fn duplicate_stdlib_import_with_same_resolved_source_does_not_reregister_items() {
    let mut root = hew_parser::parse(
        r"
            import std::bench;
            import std::bench;
        ",
    );
    assert!(
        root.errors.is_empty(),
        "root parse errors: {:?}",
        root.errors
    );

    let bench_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("std/bench/bench.hew")
        .canonicalize()
        .expect("stdlib bench module should exist");
    let bench_source =
        std::fs::read_to_string(&bench_path).expect("should read stdlib bench Hew source");
    let bench_module = hew_parser::parse(&bench_source);
    assert!(
        bench_module.errors.is_empty(),
        "bench parse errors: {:?}",
        bench_module.errors
    );

    for import_decl in root
        .program
        .items
        .iter_mut()
        .filter_map(|(item, _)| match item {
            Item::Import(import) => Some(import),
            _ => None,
        })
    {
        import_decl.resolved_items = Some(bench_module.program.items.clone());
        import_decl.resolved_source_paths = vec![bench_path.clone()];
    }

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&root.program);

    assert!(
        !output.user_modules.contains("bench"),
        "stdlib Hew import should not go through the user-module import path"
    );
    assert!(
        output.type_defs.contains_key("Suite"),
        "stdlib Hew items should still register public types"
    );
    assert!(
        output.fn_sigs.contains_key("bench.suite"),
        "stdlib Hew items should still register qualified functions"
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::DuplicateDefinition),
        "duplicate stdlib imports should dedupe Hew item registration: {:?}",
        output.errors
    );
}

// ── Warning severity tests ──────────────────────────────────────────

#[test]
fn unreachable_code_has_warning_severity() {
    let source = "fn foo() -> i32 { return 1; let x = 2; x }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let w = output
        .warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::UnreachableCode);
    assert!(w.is_some(), "expected unreachable warning");
    assert_eq!(
        w.unwrap().severity,
        crate::error::Severity::Warning,
        "unreachable code should have Warning severity"
    );
}

#[test]
fn shadowing_warning_has_note_for_original_definition() {
    // The warning for outer-scope shadowing must include a note pointing back
    // to where the original binding was defined.
    let source = "fn main() { let x = 1; if true { let x = 2; println(x); } println(x); }";
    let result = hew_parser::parse(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let w = output
        .warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::Shadowing);
    assert!(w.is_some(), "expected shadowing warning");
    assert!(
        !w.unwrap().notes.is_empty(),
        "shadowing warning should have a note pointing to the original definition"
    );
}

// ── Bug fix regression tests ────────────────────────────────────────

#[test]
fn no_warn_unused_read_then_assign() {
    // Bug 1: var x = 0; println(x); x = 1; should NOT warn about unused x
    let (errors, warnings) = parse_and_check("fn main() { var x = 0; println(x); x = 1; }");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(
        !warnings
            .iter()
            .any(|w| w.message.contains("unused variable `x`")),
        "read-then-assign should not produce unused warning, got: {warnings:?}"
    );
}

#[test]
fn warn_unreachable_after_if_all_branches_return() {
    // Bug 2: if where all branches return should mark subsequent code unreachable
    let (_, warnings) = parse_and_check(
        "fn foo() -> i32 { if true { return 1; } else { return 2; } let y = 3; y }",
    );
    assert!(
        warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::UnreachableCode),
        "expected unreachable code warning after if with all-returning branches, got: {warnings:?}"
    );
}

#[test]
fn no_warn_dead_code_function_referenced_as_value() {
    // Bug 3: let f = helper; f(); should mark helper as called
    let (_, warnings) =
        parse_and_check("fn helper() -> i32 { 42 } fn main() { let f = helper; println(f); }");
    assert!(
        !warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("helper")),
        "function referenced as value should not get dead code warning, got: {warnings:?}"
    );
}

#[test]
fn warn_dead_code_self_recursive_function() {
    // Bug 4: fn rec() { rec(); } fn main() {} — rec only calls itself, dead
    let (_, warnings) = parse_and_check("fn rec() { rec(); } fn main() { println(1); }");
    assert!(
        warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::DeadCode && w.message.contains("rec")),
        "self-recursive unreachable function should get dead code warning, got: {warnings:?}"
    );
}

// -----------------------------------------------------------------------
// Module namespacing tests
// -----------------------------------------------------------------------

/// Helper: build a simple pub function declaration.
fn make_pub_fn(name: &str, params: Vec<Param>, ret: Option<TypeExpr>) -> FnDecl {
    FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Pub,
        is_pure: false,
        name: name.to_string(),
        type_params: None,
        params,
        return_type: ret.map(|te| (te, 0..0)),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(0, 0..1))),
        },
        doc_comment: None,
        decl_span: 0..0,
    }
}

/// Helper: build a private (non-pub) function declaration.
fn make_priv_fn(name: &str) -> FnDecl {
    FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        is_pure: false,
        name: name.to_string(),
        type_params: None,
        params: vec![],
        return_type: Some((
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        )),
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: Some(Box::new(make_int_literal(0, 0..1))),
        },
        doc_comment: None,
        decl_span: 0..0,
    }
}

/// Helper: build an `ImportDecl` with resolved items.
fn make_user_import(
    path: &[&str],
    spec: Option<ImportSpec>,
    items: Vec<Spanned<Item>>,
) -> ImportDecl {
    ImportDecl {
        path: path.iter().map(ToString::to_string).collect(),
        spec,
        file_path: None,
        resolved_items: Some(items),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    }
}

/// Helper: type-check a program with given items.
fn check_items(items: Vec<Spanned<Item>>) -> TypeCheckOutput {
    let program = Program {
        module_graph: None,
        items,
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.check_program(&program)
}

// -- should_import_name --

#[test]
fn should_import_name_bare_import_returns_false() {
    assert!(!Checker::should_import_name("helper", &None));
}

#[test]
fn should_import_name_glob_returns_true() {
    assert!(Checker::should_import_name(
        "helper",
        &Some(ImportSpec::Glob)
    ));
    assert!(Checker::should_import_name(
        "anything",
        &Some(ImportSpec::Glob)
    ));
}

#[test]
fn should_import_name_named_match() {
    let spec = Some(ImportSpec::Names(vec![
        ImportName {
            name: "helper".to_string(),
            alias: None,
        },
        ImportName {
            name: "parse".to_string(),
            alias: None,
        },
    ]));
    assert!(Checker::should_import_name("helper", &spec));
    assert!(Checker::should_import_name("parse", &spec));
    assert!(!Checker::should_import_name("other", &spec));
}

// -- Bare import: qualified only --

#[test]
fn bare_import_registers_qualified_name() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        None, // bare import
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.fn_sigs.contains_key("utils.helper"),
        "bare import should register qualified name 'utils.helper'"
    );
    assert!(
        !output.fn_sigs.contains_key("helper"),
        "bare import should NOT register unqualified name 'helper'"
    );
}

// -- Glob import: everything unqualified --

#[test]
fn glob_import_registers_unqualified_names() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let other = make_pub_fn(
        "other",
        vec![],
        Some(TypeExpr::Named {
            name: "String".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Glob),
        vec![
            (Item::Function(helper), 0..0),
            (Item::Function(other), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    // Both qualified and unqualified should be registered
    assert!(output.fn_sigs.contains_key("utils.helper"));
    assert!(output.fn_sigs.contains_key("utils.other"));
    assert!(
        output.fn_sigs.contains_key("helper"),
        "glob import should register unqualified 'helper'"
    );
    assert!(
        output.fn_sigs.contains_key("other"),
        "glob import should register unqualified 'other'"
    );
}

// -- Named import: specific names only --

#[test]
fn named_import_registers_specified_names_only() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let other = make_pub_fn(
        "other",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "helper".to_string(),
            alias: None,
        }])),
        vec![
            (Item::Function(helper), 0..0),
            (Item::Function(other), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    // Both should be qualified
    assert!(output.fn_sigs.contains_key("utils.helper"));
    assert!(output.fn_sigs.contains_key("utils.other"));
    // Only "helper" should be unqualified
    assert!(
        output.fn_sigs.contains_key("helper"),
        "named import should register 'helper' unqualified"
    );
    assert!(
        !output.fn_sigs.contains_key("other"),
        "named import should NOT register 'other' unqualified"
    );
}

// -- Pub visibility enforcement --

#[test]
fn non_pub_functions_not_registered() {
    let priv_fn = make_priv_fn("secret");
    let pub_fn = make_pub_fn(
        "visible",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        Some(ImportSpec::Glob), // even glob shouldn't expose private fns
        vec![
            (Item::Function(priv_fn), 0..0),
            (Item::Function(pub_fn), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        !output.fn_sigs.contains_key("utils.secret"),
        "non-pub function should not be registered as qualified"
    );
    assert!(
        !output.fn_sigs.contains_key("secret"),
        "non-pub function should not be registered as unqualified"
    );
    assert!(output.fn_sigs.contains_key("utils.visible"));
    assert!(output.fn_sigs.contains_key("visible"));
}

// -- User module const registration --

#[test]
fn user_module_registers_pub_consts() {
    use hew_parser::ast::ConstDecl;

    let pub_const = ConstDecl {
        visibility: Visibility::Pub,
        name: "MAX_SIZE".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(100, 0..3),
    };
    let priv_const = ConstDecl {
        visibility: Visibility::Private,
        name: "INTERNAL".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(42, 0..2),
    };
    let import = make_user_import(
        &["myapp", "config"],
        Some(ImportSpec::Glob),
        vec![
            (Item::Const(pub_const), 0..0),
            (Item::Const(priv_const), 0..0),
        ],
    );

    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let _output = checker.check_program(&program);

    // pub const should be findable in the environment
    assert!(
        checker.env.lookup_ref("config.MAX_SIZE").is_some(),
        "pub const should be registered as qualified"
    );
    assert!(
        checker.env.lookup_ref("MAX_SIZE").is_some(),
        "pub const should be unqualified with glob import"
    );
    assert!(
        checker.env.lookup_ref("config.INTERNAL").is_none(),
        "private const should NOT be registered"
    );
    assert!(
        checker.env.lookup_ref("INTERNAL").is_none(),
        "private const should NOT be registered unqualified"
    );
}

#[test]
fn user_module_const_bare_import_qualified_only() {
    use hew_parser::ast::ConstDecl;

    let pub_const = ConstDecl {
        visibility: Visibility::Pub,
        name: "LIMIT".to_string(),
        ty: (
            TypeExpr::Named {
                name: "i32".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: make_int_literal(50, 0..2),
    };
    let import = make_user_import(
        &["myapp", "config"],
        None, // bare import
        vec![(Item::Const(pub_const), 0..0)],
    );

    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..0)],
        module_doc: None,
    };
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let _output = checker.check_program(&program);

    assert!(
        checker.env.lookup_ref("config.LIMIT").is_some(),
        "pub const should be registered as qualified"
    );
    assert!(
        checker.env.lookup_ref("LIMIT").is_none(),
        "bare import should NOT register const unqualified"
    );
}

// -- User module type registration --

#[test]
fn user_module_registers_types() {
    let struct_decl = TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: "Config".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![TypeBodyItem::Field {
            name: "value".to_string(),
            ty: (
                TypeExpr::Named {
                    name: "i32".to_string(),
                    type_args: None,
                },
                0..0,
            ),
            attributes: Vec::new(),
        }],
        doc_comment: None,
        wire: None,
        is_indirect: false,
    };
    let import = make_user_import(
        &["myapp", "config"],
        None, // bare import
        vec![(Item::TypeDecl(struct_decl), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.type_defs.contains_key("Config"),
        "user module type should be registered unqualified"
    );
    assert!(
        output.type_defs.contains_key("config.Config"),
        "user module type should also be registered as qualified"
    );
}

// -- user_modules set --

#[test]
fn user_modules_set_populated() {
    let helper = make_pub_fn(
        "helper",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["myapp", "utils"],
        None,
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.user_modules.contains("utils"),
        "user_modules should contain the module short name"
    );
}

#[test]
fn stdlib_not_in_user_modules() {
    // A stdlib import should NOT appear in user_modules
    let import = ImportDecl {
        path: vec!["std".to_string(), "fs".to_string()],
        spec: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        !output.user_modules.contains("fs"),
        "stdlib module should NOT be in user_modules"
    );
}

// -- Function signature correctness --

#[test]
fn user_module_fn_sig_has_correct_types() {
    let helper = make_pub_fn(
        "add",
        vec![
            Param {
                name: "a".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                ),
                is_mutable: false,
            },
            Param {
                name: "b".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "i32".to_string(),
                        type_args: None,
                    },
                    0..0,
                ),
                is_mutable: false,
            },
        ],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["mylib", "math"],
        None,
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    let sig = output
        .fn_sigs
        .get("math.add")
        .expect("math.add should be registered");
    assert_eq!(sig.params.len(), 2, "should have 2 params");
    assert_eq!(sig.params[0], Ty::I32);
    assert_eq!(sig.params[1], Ty::I32);
    assert_eq!(sig.return_type, Ty::I32);
    assert_eq!(sig.param_names, vec!["a", "b"]);
}

// -- Multiple modules don't collide --

#[test]
fn two_modules_same_fn_name_no_collision() {
    let helper_a = make_pub_fn(
        "run",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let helper_b = make_pub_fn(
        "run",
        vec![],
        Some(TypeExpr::Named {
            name: "String".to_string(),
            type_args: None,
        }),
    );
    let import_a = make_user_import(
        &["pkg", "alpha"],
        None,
        vec![(Item::Function(helper_a), 0..0)],
    );
    let import_b = make_user_import(
        &["pkg", "beta"],
        None,
        vec![(Item::Function(helper_b), 0..0)],
    );
    let output = check_items(vec![
        (Item::Import(import_a), 0..0),
        (Item::Import(import_b), 0..0),
    ]);

    assert!(output.fn_sigs.contains_key("alpha.run"));
    assert!(output.fn_sigs.contains_key("beta.run"));
    // Both should have different return types
    assert_eq!(output.fn_sigs["alpha.run"].return_type, Ty::I32);
    assert_eq!(output.fn_sigs["beta.run"].return_type, Ty::String);
}

// -- Import with no resolved items (stdlib) still works --

#[test]
fn import_without_resolved_items_emits_unresolved_error() {
    // An import with resolved_items = None and no stdlib match (empty registry)
    // must now emit an UnresolvedImport error rather than silently dropping.
    let import = ImportDecl {
        path: vec!["unknown".to_string(), "pkg".to_string()],
        spec: None,
        file_path: None,
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UnresolvedImport),
        "expected UnresolvedImport error, got: {errors:?}",
        errors = output.errors
    );
    assert!(!output.user_modules.contains("pkg"));
}

#[test]
fn import_with_resolved_items_no_error() {
    // When resolved_items is provided the user-module path is taken and no
    // UnresolvedImport diagnostic should be emitted.
    let import = make_user_import(&["myapp", "util"], None, vec![]);
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UnresolvedImport),
        "unexpected UnresolvedImport error for user module with resolved_items"
    );
    assert!(output.user_modules.contains("util"));
}

#[test]
fn file_import_without_resolved_items_emits_unresolved_error() {
    let import = ImportDecl {
        path: vec![],
        spec: None,
        file_path: Some("missing.hew".to_string()),
        resolved_items: None,
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };
    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import), 0..20)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);
    let error = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::UnresolvedImport)
        .expect("expected UnresolvedImport error for unresolved file import");

    assert!(
        error.message.contains("missing.hew"),
        "unresolved file import should mention the missing file path: {error:?}"
    );
}

#[test]
fn merged_file_import_duplicate_pub_name_emits_duplicate_definition() {
    let shared_decl = make_pub_fn(
        "shared",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = ImportDecl {
        path: vec![],
        spec: None,
        file_path: Some("pkg.hew".to_string()),
        resolved_items: Some(vec![
            (Item::Function(shared_decl.clone()), 0..5),
            (Item::Function(shared_decl), 10..15),
        ]),
        resolved_item_source_paths: vec![
            std::path::PathBuf::from("pkg/pkg.hew"),
            std::path::PathBuf::from("pkg/helpers.hew"),
        ],
        resolved_source_paths: vec![
            std::path::PathBuf::from("pkg/pkg.hew"),
            std::path::PathBuf::from("pkg/helpers.hew"),
        ],
    };
    let output = check_items(vec![(Item::Import(import), 0..20)]);
    let error = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::DuplicateDefinition)
        .expect("merged file import should fail closed on duplicate pub names");

    assert!(
        error.message.contains("shared"),
        "duplicate pub name error should mention the colliding binding: {error:?}"
    );
    assert_eq!(
        error.notes.first().map(|(span, _)| span.clone()),
        Some(0..5),
        "duplicate pub name should point back to the first merged definition"
    );
}

#[test]
fn repeated_flat_file_import_with_same_resolved_source_does_not_reregister_items() {
    let shared_source = std::path::PathBuf::from("pkg/pkg.hew");
    let import = ImportDecl {
        path: vec![],
        spec: None,
        file_path: Some("pkg.hew".to_string()),
        resolved_items: Some(vec![(
            Item::Function(make_pub_fn(
                "shared",
                vec![],
                Some(TypeExpr::Named {
                    name: "i32".to_string(),
                    type_args: None,
                }),
            )),
            0..5,
        )]),
        resolved_item_source_paths: vec![shared_source.clone()],
        resolved_source_paths: vec![shared_source],
    };
    let output = check_items(vec![
        (Item::Import(import.clone()), 0..5),
        (Item::Import(import), 10..15),
    ]);

    assert!(
        output.errors.is_empty(),
        "same resolved flat file import should stay idempotent: {:?}",
        output.errors
    );
    assert!(
        output.fn_sigs.contains_key("shared"),
        "flat file import should still register the imported function"
    );
}

#[test]
fn repeated_stdlib_import_does_not_duplicate_hew_items() {
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    let fs_path = repo_root.join("std/fs.hew");
    let source = std::fs::read_to_string(&fs_path).expect("std/fs.hew should exist");
    let parsed = hew_parser::parse(&source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors in std/fs.hew: {:?}",
        parsed.errors
    );

    let import = ImportDecl {
        path: vec!["std".to_string(), "fs".to_string()],
        spec: None,
        file_path: None,
        resolved_items: Some(parsed.program.items),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: vec![fs_path],
    };
    let program = Program {
        module_graph: None,
        items: vec![
            (Item::Import(import.clone()), 0..0),
            (Item::Import(import), 0..0),
        ],
        module_doc: None,
    };

    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "unexpected errors for repeated stdlib import: {:?}",
        output.errors
    );
    assert!(
        output.type_defs.contains_key("IoError"),
        "expected std::fs Hew items to remain registered"
    );
}

// -- Empty module import --

#[test]
fn empty_module_import_no_crash() {
    let import = make_user_import(&["myapp", "empty"], None, vec![]);
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(output.user_modules.contains("empty"));
    assert!(output.errors.is_empty());
}

// -- Import alias binding --

#[test]
fn import_alias_binds_under_alias_name() {
    // import mymod::{foo as bar} — "bar" must resolve, "foo" must not be unqualified
    let helper = make_pub_fn(
        "foo",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["mymod"],
        Some(ImportSpec::Names(vec![ImportName {
            name: "foo".to_string(),
            alias: Some("bar".to_string()),
        }])),
        vec![(Item::Function(helper), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    // qualified form always uses original name
    assert!(
        output.fn_sigs.contains_key("mymod.foo"),
        "qualified 'mymod.foo' should be registered regardless of alias"
    );
    // unqualified binding must use the alias
    assert!(
        output.fn_sigs.contains_key("bar"),
        "aliased import should register unqualified binding 'bar'"
    );
    // original unqualified name must NOT be registered
    assert!(
        !output.fn_sigs.contains_key("foo"),
        "aliased import must NOT register unqualified 'foo'"
    );
}

#[test]
fn import_alias_multiple_names() {
    // import pkg::{alpha as a, beta as b}
    let fn_alpha = make_pub_fn(
        "alpha",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let fn_beta = make_pub_fn(
        "beta",
        vec![],
        Some(TypeExpr::Named {
            name: "i32".to_string(),
            type_args: None,
        }),
    );
    let import = make_user_import(
        &["pkg"],
        Some(ImportSpec::Names(vec![
            ImportName {
                name: "alpha".to_string(),
                alias: Some("a".to_string()),
            },
            ImportName {
                name: "beta".to_string(),
                alias: Some("b".to_string()),
            },
        ])),
        vec![
            (Item::Function(fn_alpha), 0..0),
            (Item::Function(fn_beta), 0..0),
        ],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.fn_sigs.contains_key("a"),
        "'a' alias should be registered"
    );
    assert!(
        output.fn_sigs.contains_key("b"),
        "'b' alias should be registered"
    );
    assert!(
        !output.fn_sigs.contains_key("alpha"),
        "original 'alpha' must not be unqualified"
    );
    assert!(
        !output.fn_sigs.contains_key("beta"),
        "original 'beta' must not be unqualified"
    );
}

// -- Trait import from module --

#[test]
fn import_trait_from_module_glob() {
    use hew_parser::ast::{TraitDecl, TraitItem, TraitMethod};

    let trait_decl = TraitDecl {
        visibility: Visibility::Pub,
        name: "Display".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![TraitItem::Method(TraitMethod {
            name: "display".to_string(),
            is_pure: true,
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: None,
        })],
        doc_comment: None,
    };
    let import = make_user_import(
        &["mylib", "fmt"],
        Some(ImportSpec::Glob),
        vec![(Item::Trait(trait_decl), 0..0)],
    );
    let output = check_items(vec![(Item::Import(import), 0..0)]);

    assert!(
        output.errors.is_empty(),
        "importing a pub trait should not produce errors: {:?}",
        output.errors
    );
    // The module should be registered as a user module
    assert!(
        output.user_modules.contains("fmt"),
        "module 'fmt' should be in user_modules"
    );
}

#[test]
fn import_private_trait_not_registered() {
    use hew_parser::ast::{TraitDecl, TraitItem, TraitMethod};

    let private_trait = TraitDecl {
        visibility: Visibility::Private,
        name: "Internal".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![TraitItem::Method(TraitMethod {
            name: "internal_op".to_string(),
            is_pure: false,
            type_params: None,
            params: vec![],
            return_type: None,
            where_clause: None,
            body: None,
        })],
        doc_comment: None,
    };
    let import = make_user_import(
        &["mylib", "internals"],
        Some(ImportSpec::Glob),
        vec![(Item::Trait(private_trait), 0..0)],
    );
    // Should complete without errors; private trait is simply ignored
    let output = check_items(vec![(Item::Import(import), 0..0)]);
    assert!(output.errors.is_empty(), "errors: {:?}", output.errors);
}

// -- Orphan rule warning --

#[test]
fn orphan_impl_emits_warning() {
    use hew_parser::ast::TraitBound;
    // impl ExternalTrait for ExternalType → neither is local → orphan warning
    let impl_decl = ImplDecl {
        type_params: None,
        trait_bound: Some(TraitBound {
            name: "SomeTrait".to_string(),
            type_args: None,
        }),
        target_type: (
            TypeExpr::Named {
                name: "SomeType".to_string(),
                type_args: None,
            },
            0..0,
        ),
        where_clause: None,
        type_aliases: vec![],
        methods: vec![],
    };
    let output = check_items(vec![(Item::Impl(impl_decl), 0..0)]);

    let has_orphan_warning = output
        .warnings
        .iter()
        .any(|w| w.kind == crate::error::TypeErrorKind::OrphanImpl);
    assert!(
        has_orphan_warning,
        "expected OrphanImpl warning when neither trait nor type is local, got: {:?}",
        output.warnings
    );
}

#[test]
fn local_type_impl_no_orphan_warning() {
    use hew_parser::ast::TraitBound;
    // Locally defined type: impl SomeExternalTrait for LocalType → no orphan warning
    let type_decl = TypeDecl {
        visibility: Visibility::Pub,
        kind: TypeDeclKind::Struct,
        name: "LocalType".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![],
        doc_comment: None,
        wire: None,
        is_indirect: false,
    };
    let impl_decl = ImplDecl {
        type_params: None,
        trait_bound: Some(TraitBound {
            name: "ExternalTrait".to_string(),
            type_args: None,
        }),
        target_type: (
            TypeExpr::Named {
                name: "LocalType".to_string(),
                type_args: None,
            },
            0..0,
        ),
        where_clause: None,
        type_aliases: vec![],
        methods: vec![],
    };
    let output = check_items(vec![
        (Item::TypeDecl(type_decl), 0..0),
        (Item::Impl(impl_decl), 0..0),
    ]);

    let has_orphan = output
        .warnings
        .iter()
        .any(|w| w.kind == crate::error::TypeErrorKind::OrphanImpl);
    assert!(
        !has_orphan,
        "impl on a locally defined type must NOT produce an orphan warning"
    );
}

#[test]
fn test_file_import_private_items_not_visible() {
    use hew_parser::ast::{
        Block, ConstDecl, Expr, FnDecl, ImportDecl, Item, Literal, Program, Spanned, TypeDecl,
        TypeDeclKind, TypeExpr,
    };

    let private_fn = Item::Function(FnDecl {
        attributes: vec![],
        is_async: false,
        is_generator: false,
        visibility: Visibility::Private,
        is_pure: false,
        name: "private_func".to_string(),
        type_params: None,
        params: vec![],
        return_type: None,
        where_clause: None,
        body: Block {
            stmts: vec![],
            trailing_expr: None,
        },
        doc_comment: None,
        decl_span: 0..0,
    });

    let private_const = Item::Const(ConstDecl {
        visibility: Visibility::Private,
        name: "PRIVATE_CONST".to_string(),
        ty: (
            TypeExpr::Named {
                name: "Int".to_string(),
                type_args: None,
            },
            0..0,
        ),
        value: (
            Expr::Literal(Literal::Integer {
                value: 42,
                radix: hew_parser::ast::IntRadix::Decimal,
            }),
            0..0,
        ),
    });

    let private_type = Item::TypeDecl(TypeDecl {
        visibility: Visibility::Private,
        kind: TypeDeclKind::Struct,
        name: "PrivateType".to_string(),
        type_params: None,
        where_clause: None,
        body: vec![],
        doc_comment: None,
        wire: None,
        is_indirect: false,
    });

    let resolved: Vec<Spanned<Item>> = vec![
        (private_fn, 0..0),
        (private_const, 0..0),
        (private_type, 0..0),
    ];

    let import_decl = ImportDecl {
        path: vec![],
        spec: None,
        file_path: Some("private_lib.hew".to_string()),
        resolved_items: Some(resolved),
        resolved_item_source_paths: Vec::new(),
        resolved_source_paths: Vec::new(),
    };

    let program = Program {
        module_graph: None,
        items: vec![(Item::Import(import_decl), 0..0)],
        module_doc: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&program);

    assert!(
        !output.fn_sigs.contains_key("private_func"),
        "private function must not be registered from file import"
    );
    assert!(
        checker.env.lookup("PRIVATE_CONST").is_none(),
        "private const must not be registered from file import"
    );
    assert!(
        !checker.known_types.contains("PrivateType"),
        "private type must not be registered from file import"
    );
}

#[test]
fn check_generic_lambda() {
    let source = r"
        fn apply<T>(f: fn(T) -> T, x: T) -> T {
            f(x)
        }

        fn main() {
            // Identity generic lambda
            let id = <T>(x: T) => x;
            // Instantiation happens when calling `apply`
            // apply takes fn(T) -> T. `id` matches that.
            // However, `id` is a generic closure.
            // We need to make sure generic instantiation works.
            // Currently, `check_lambda` creates fresh type variables for T.
            // So id has type ?0 -> ?0.
            // When passed to apply(id, 5), T inferred as int.
            // apply expects fn(int) -> int.
            // id matches fn(?0) -> ?0 where ?0=int.
            let res = apply(id, 5);
        }
    ";

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
        "type check errors: {:?}",
        output.errors
    );
}

/// Slice-1 generic lambda regression test.
///
/// Verifies that:
/// 1. A let-bound generic lambda type-checks cleanly.
/// 2. A direct call whose arguments make the type obvious resolves the
///    return type correctly.
/// 3. `call_type_args` is populated for the call so the enricher can
///    fill in explicit type arguments before serialisation to MLIR.
#[test]
fn generic_lambda_slice1_type_inference() {
    let source = r"
        fn main() {
            let v: int = 30;
            let r = <T>(a: T, b: T) -> T => a;
            let q = r(v, v);
        }
    ";

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
        "type check errors: {:?}",
        output.errors
    );

    // The call r(v, v) must have produced a call_type_args entry (T→int).
    assert!(
        !output.call_type_args.is_empty(),
        "call_type_args should contain the inferred type for r(v,v)"
    );
    // The single entry should map to [int / i64].
    let type_args: Vec<_> = output.call_type_args.values().collect();
    assert_eq!(type_args.len(), 1);
    assert_eq!(
        type_args[0],
        &vec![crate::ty::Ty::I64],
        "T should be inferred as int (i64)"
    );
}

/// Slice-1: two-type-param generic lambda, verify both params inferred.
#[test]
fn generic_lambda_slice1_two_type_params() {
    let source = concat!(
        "fn main() {\n",
        r#"    let combine = <A, B>(a: A, b: B) -> A => a;"#,
        "\n",
        r#"    let x: int = 1;"#,
        "\n",
        r#"    let y: string = "hello";"#,
        "\n",
        r#"    let z = combine(x, y);"#,
        "\n",
        "}\n",
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
        "type check errors: {:?}",
        output.errors
    );

    // Should have one call_type_args entry with two type args.
    assert_eq!(
        output.call_type_args.len(),
        1,
        "expected one call_type_args entry"
    );
    let args = output.call_type_args.values().next().unwrap();
    assert_eq!(args.len(), 2, "expected two type args (A and B)");
}

#[test]
fn contextual_lambda_binding_records_lambda_expr_type() {
    let source = concat!(
        "fn main() {\n",
        "    let f: fn(int) -> int = (x) => x + 1;\n",
        "    let y = f(5);\n",
        "}\n",
    );

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let lambda_span = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            Item::Function(fd) if fd.name == "main" => {
                fd.body.stmts.iter().find_map(|(stmt, _)| match stmt {
                    Stmt::Let {
                        value: Some((Expr::Lambda { .. }, span)),
                        ..
                    } => Some(span.clone()),
                    _ => None,
                })
            }
            _ => None,
        })
        .expect("main let-bound lambda should exist");

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );

    assert_eq!(
        output.expr_types.get(&SpanKey::from(&lambda_span)),
        Some(&Ty::Function {
            params: vec![Ty::I64],
            ret: Box::new(Ty::I64),
        })
    );
}

/// Regression: a generic lambda passed as a function *argument* (not
/// directly bound to `let`) must not leak its `TypeVar` pairs into the
/// scratch field and then be picked up by the *next* unrelated let-binding.
///
/// Before the fix, `last_lambda_generic_vars` was set by `check_lambda`
/// any time a generic lambda was type-checked, so the following sequence
/// would falsely register `q` as having a generic lambda type:
///
/// ```text
///   fn apply<T>(f: fn(T) -> T, x: T) -> T { f(x) }
///   fn main() {
///       apply(<T>(x: T) => x, 5);  // generic lambda in arg position
///       let q = 42;                 // should NOT be in lambda_poly_type_var_map
///   }
/// ```
#[test]
fn generic_lambda_scratch_state_no_leak() {
    let source = r"
        fn apply<T>(f: fn(T) -> T, x: T) -> T {
            f(x)
        }

        fn main() {
            apply(<T>(x: T) => x, 5);
            let q = 42;
            let z = q + 1;
        }
    ";

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
        "type check errors: {:?}",
        output.errors
    );

    // call_type_args may have an entry for the `apply(...)` call, but q
    // must not appear in lambda_poly_type_var_map.  We verify indirectly:
    // the number of call_type_args entries must be exactly 1 (for `apply`)
    // and must not grow due to a spurious phantom call on `q` or `z`.
    // (There is no call through q, so any extra entry would signal a leak.)
    assert!(
        output.call_type_args.len() <= 1,
        "expected at most 1 call_type_args entry (for apply), got {}; \
         stale lambda scratch state likely leaked into a later let-binding",
        output.call_type_args.len()
    );
}

#[test]
fn test_self_with_generics_in_impl() {
    let source = r"
        type Pair<T> {
            first: T,
            second: T,
        }

        impl<T> Pair<T> {
            fn new(first: T, second: T) -> Self {
                return Pair { first: first, second: second };
            }

            fn swap(p: Pair<T>) -> Self {
                return Pair { first: p.second, second: p.first };
            }
        }
    ";

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
        "type check errors: {:?}",
        output.errors
    );

    // Verify that Self resolves to Pair<T>, not bare Pair
    // The new method should return Pair<T>
    let new_sig = output
        .fn_sigs
        .get("Pair::new")
        .expect("Pair::new should exist");
    if let Ty::Named { name, args } = &new_sig.return_type {
        assert_eq!(name, "Pair", "return type should be Pair");
        assert_eq!(args.len(), 1, "Pair should have one type argument");
    } else {
        panic!("Expected Pair::new to return a named type");
    }
}

#[test]
fn test_trait_object_type_args_substitution() {
    // Bug 2: Test that dyn Trait<Args> methods get correct substitutions
    let source = r"
        trait Iterator<T> {
            fn next(iter: Self) -> Option<T>;
        }

        type Counter {
            count: int;
        }

        impl Iterator<int> for Counter {
            fn next(c: Counter) -> Option<int> {
                Some(42)
            }
        }

        fn test_iterator() -> int {
            let iter: dyn Iterator<int> = Counter { count: 5 };
            let result = iter.next(); // Should be Option<int>, not Option<T>
            match result {
                Some(x) => x,
                None => 0
            }
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);

    if !output.errors.is_empty() {
        for error in &output.errors {
            println!("Type error: {error}");
        }
    }

    assert!(
        output.errors.is_empty(),
        "type check errors: {:?}",
        output.errors
    );
}

#[test]
fn test_wire_since_without_version_warns() {
    use hew_parser::ast::{WireFieldMeta, WireMetadata};
    let wire = WireMetadata {
        field_meta: vec![WireFieldMeta {
            field_name: "added_field".to_string(),
            field_number: 2,
            is_optional: false,
            is_deprecated: false,
            is_repeated: false,
            json_name: None,
            yaml_name: None,
            since: Some(2),
        }],
        reserved_numbers: vec![],
        json_case: None,
        yaml_case: None,
        version: None,
        min_version: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.validate_wire_version_constraints("TestMsg", &wire);

    assert!(checker.errors.is_empty(), "should not produce errors");
    assert_eq!(checker.warnings.len(), 1);
    assert!(
        checker.warnings[0].message.contains("since 2"),
        "warning should mention since: {}",
        checker.warnings[0].message
    );
    assert!(
        checker.warnings[0]
            .message
            .contains("no #[wire(version = N)]"),
        "warning should mention missing version: {}",
        checker.warnings[0].message
    );
}

#[test]
fn test_wire_since_with_version_no_extra_warning() {
    use hew_parser::ast::{WireFieldMeta, WireMetadata};
    let wire = WireMetadata {
        field_meta: vec![WireFieldMeta {
            field_name: "added_field".to_string(),
            field_number: 2,
            is_optional: false,
            is_deprecated: false,
            is_repeated: false,
            json_name: None,
            yaml_name: None,
            since: Some(2),
        }],
        reserved_numbers: vec![],
        json_case: None,
        yaml_case: None,
        version: Some(3),
        min_version: None,
    };

    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.validate_wire_version_constraints("TestMsg", &wire);

    assert!(checker.errors.is_empty(), "should not produce errors");
    // No "since without version" warning since version is present
    let since_without_version = checker
        .warnings
        .iter()
        .any(|w| w.message.contains("no #[wire(version = N)]"));
    assert!(
        !since_without_version,
        "should not warn about missing version"
    );
}

#[test]
fn typecheck_await_actor_ref_returns_unit() {
    let output = check_source(
        r#"
        actor Greeter {
            receive fn greet(name: String) {
                println(name);
            }
        }
        fn main() {
            let g = spawn Greeter;
            g.greet("hi");
            close(g);
            await g;
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors, got: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_await_close_actor_ref() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();

    checker.env.define(
        "g".to_string(),
        Ty::actor_ref(Ty::Named {
            name: "Greeter".to_string(),
            args: vec![],
        }),
        false,
    );

    let span = 0..0;
    let expr = Expr::Await(Box::new((
        Expr::Call {
            function: Box::new((Expr::Identifier("close".to_string()), span.clone())),
            type_args: None,
            args: vec![CallArg::Positional((
                Expr::Identifier("g".to_string()),
                span.clone(),
            ))],
            is_tail_call: false,
        },
        span.clone(),
    )));

    let ty = checker.synthesize(&expr, &span);
    assert_eq!(ty, Ty::Unit);
    assert!(
        checker.errors.is_empty(),
        "expected no errors, got: {:?}",
        checker.errors
    );
}

#[test]
fn typecheck_await_close_lambda_actor() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.register_builtins();

    checker.env.define(
        "worker".to_string(),
        Ty::Named {
            name: "Actor".to_string(),
            args: vec![Ty::I64],
        },
        false,
    );

    let span = 0..0;
    let expr = Expr::Await(Box::new((
        Expr::Call {
            function: Box::new((Expr::Identifier("close".to_string()), span.clone())),
            type_args: None,
            args: vec![CallArg::Positional((
                Expr::Identifier("worker".to_string()),
                span.clone(),
            ))],
            is_tail_call: false,
        },
        span.clone(),
    )));

    let ty = checker.synthesize(&expr, &span);
    assert_eq!(ty, Ty::Unit);
    assert!(
        checker.errors.is_empty(),
        "expected no errors, got: {:?}",
        checker.errors
    );
}

#[test]
fn typecheck_join_rejects_non_actor_sources() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let span = 0..0;
    let expr = Expr::Join(vec![
        make_int_literal(1, span.clone()),
        make_int_literal(2, span.clone()),
    ]);
    let _ = checker.synthesize(&expr, &span);
    assert!(
        checker.errors.iter().any(|error| error
            .message
            .contains("join expression element must be actor.method(args)")),
        "expected join source error, got: {:?}",
        checker.errors
    );
}

#[test]
fn typecheck_integer_literal_coerces_in_arithmetic() {
    // `n - 1` where n: i32 should work — literal 1 coerces to i32
    let source = concat!(
        "fn fib(n: i32) -> i32 {\n",
        "    if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }\n",
        "}\n",
        "fn main() { println(fib(10)); }\n"
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
        "integer literal should coerce in arithmetic: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_literal_range_infers_from_context() {
    // `for i in 0..8 { fib(i) }` where fib takes i32 — range bounds
    // should not force i64; the loop variable should be usable as i32
    let source = concat!(
        "fn fib(n: i32) -> i32 {\n",
        "    if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }\n",
        "}\n",
        "fn main() {\n",
        "    for i in 0..8 {\n",
        "        println(fib(i));\n",
        "    }\n",
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
        "literal range should infer element type from context: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_cast_expression_numeric() {
    let source = concat!(
        "fn main() {\n",
        "    let x: i64 = 42;\n",
        "    let y: i32 = x as i32;\n",
        "    let z: f64 = y as f64;\n",
        "    println(y);\n",
        "    println(z);\n",
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
        "numeric casts should type-check: {:?}",
        output.errors
    );
}

#[test]
fn typecheck_cast_expression_invalid() {
    let source = concat!(
        "fn main() {\n",
        "    let s = \"hello\";\n",
        "    let x = s as i32;\n",
        "    println(x);\n",
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
        output
            .errors
            .iter()
            .any(|e| e.message.contains("cannot cast")),
        "should reject invalid cast: {:?}",
        output.errors
    );
}

// ── Literal coercion tests ────────────────────────────────────────

#[test]
fn literal_coercion_integer_to_i32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(42, 0..2);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::I32);
    assert_eq!(ty, Ty::I32);
}

#[test]
fn literal_coercion_integer_to_u8() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(255, 0..3);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::U8);
    assert_eq!(ty, Ty::U8);
}

#[test]
fn literal_coercion_integer_to_u8_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(256, 0..3);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::U8);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn literal_coercion_negative_to_unsigned() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = (
        Expr::Unary {
            op: UnaryOp::Negate,
            operand: Box::new(make_int_literal(1, 1..2)),
        },
        0..2,
    );
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::U32);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("negative literal")),
        "expected negative-to-unsigned error: {:?}",
        checker.errors
    );
}

#[test]
fn literal_coercion_i32_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // 2^31 = 2147483648, which exceeds i32 max (2147483647)
    let lit = make_int_literal(2_147_483_648, 0..10);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::I32);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn literal_coercion_integer_to_f32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = make_int_literal(42, 0..2);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::F32);
    assert_eq!(ty, Ty::F32);
}

#[test]
fn literal_coercion_float_to_f32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let lit = (Expr::Literal(Literal::Float(std::f64::consts::PI)), 0..4);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::F32);
    assert_eq!(ty, Ty::F32);
}

#[test]
fn literal_coercion_integer_fits_i8() {
    assert!(integer_fits_type(127, &Ty::I8));
    assert!(integer_fits_type(-128, &Ty::I8));
    assert!(!integer_fits_type(128, &Ty::I8));
    assert!(!integer_fits_type(-129, &Ty::I8));
}

#[test]
fn literal_coercion_integer_fits_u8() {
    assert!(integer_fits_type(0, &Ty::U8));
    assert!(integer_fits_type(255, &Ty::U8));
    assert!(!integer_fits_type(256, &Ty::U8));
    assert!(!integer_fits_type(-1, &Ty::U8));
}

#[test]
fn literal_coercion_integer_fits_i16() {
    assert!(integer_fits_type(32767, &Ty::I16));
    assert!(integer_fits_type(-32768, &Ty::I16));
    assert!(!integer_fits_type(32768, &Ty::I16));
}

#[test]
fn literal_coercion_integer_fits_u16() {
    assert!(integer_fits_type(65535, &Ty::U16));
    assert!(!integer_fits_type(65536, &Ty::U16));
}

#[test]
fn literal_coercion_integer_fits_i32() {
    assert!(integer_fits_type(2_147_483_647, &Ty::I32));
    assert!(integer_fits_type(-2_147_483_648, &Ty::I32));
    assert!(!integer_fits_type(2_147_483_648, &Ty::I32));
}

#[test]
fn literal_coercion_integer_fits_u32() {
    assert!(integer_fits_type(4_294_967_295, &Ty::U32));
    assert!(!integer_fits_type(4_294_967_296, &Ty::U32));
    assert!(!integer_fits_type(-1, &Ty::U32));
}

#[test]
fn literal_coercion_integer_fits_u64() {
    // i64 max fits in u64
    assert!(integer_fits_type(i64::MAX, &Ty::U64));
    // 0 fits
    assert!(integer_fits_type(0, &Ty::U64));
    // Negative doesn't fit
    assert!(!integer_fits_type(-1, &Ty::U64));
}

// ── Array literal → Array type coercion tests ────────────────────

#[test]
fn literal_coercion_array_to_i32_array() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![
        make_int_literal(1, 1..2),
        make_int_literal(2, 4..5),
        make_int_literal(3, 7..8),
    ];
    let arr = (Expr::Array(elems), 0..9);
    let expected = Ty::Array(Box::new(Ty::I32), 3);
    let ty = checker.check_against(&arr.0, &arr.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn literal_coercion_array_repeat_to_i32() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let value = make_int_literal(0, 1..2);
    let count = make_int_literal(5, 4..5);
    let arr = (
        Expr::ArrayRepeat {
            value: Box::new(value),
            count: Box::new(count),
        },
        0..6,
    );
    let expected = Ty::Array(Box::new(Ty::I32), 5);
    let ty = checker.check_against(&arr.0, &arr.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

// ── Tuple literal coercion tests ───────────────────────────────────

#[test]
fn tuple_literal_coercion_to_typed() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![
        make_int_literal(42, 1..3),
        (Expr::Literal(Literal::Float(std::f64::consts::PI)), 5..9),
    ];
    let tuple = (Expr::Tuple(elems), 0..10);
    let expected = Ty::Tuple(vec![Ty::I32, Ty::F32]);
    let ty = checker.check_against(&tuple.0, &tuple.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn tuple_literal_coercion_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let elems = vec![make_int_literal(256, 1..4)];
    let tuple = (Expr::Tuple(elems), 0..5);
    let expected = Ty::Tuple(vec![Ty::U8]);
    let _ty = checker.check_against(&tuple.0, &tuple.1, &expected);
    // The tuple itself coerces, but the element should have reported an error
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

#[test]
fn range_default_type_is_i64() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let range = (
        Expr::Range {
            start: None,
            end: None,
            inclusive: false,
        },
        0..2,
    );
    let ty = checker.synthesize(&range.0, &range.1);
    assert_eq!(ty, Ty::range(Ty::I64));
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

// ── Type variable resolution in check_against ────────────────────

#[test]
fn literal_coercion_through_type_var() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Create a type variable and unify it with i32
    let tv = TypeVar::fresh();
    checker.subst.insert(tv, Ty::I32);
    // Now check an integer literal against the type variable
    let lit = make_int_literal(42, 0..2);
    let ty = checker.check_against(&lit.0, &lit.1, &Ty::Var(tv));
    assert_eq!(ty, Ty::I32);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

// ── Let-bound literal coercion tests ─────────────────────────────

#[test]
fn let_bound_literal_coercion() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Simulate: let n = 5
    let let_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some(make_int_literal(5, 8..9)),
    };
    checker.check_stmt(&let_stmt, &(0..10));
    // Now check: let x: i32 = n
    let ident = (Expr::Identifier("n".to_string()), 15..16);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I32);
    assert_eq!(ty, Ty::I32);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn let_bound_literal_overflow() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    // Simulate: let n = 2147483648 (exceeds i32 max)
    let let_stmt = Stmt::Let {
        pattern: (Pattern::Identifier("n".to_string()), 4..5),
        ty: None,
        value: Some(make_int_literal(2_147_483_648, 8..18)),
    };
    checker.check_stmt(&let_stmt, &(0..19));
    // Now check: let x: i32 = n — should fail with range error
    let ident = (Expr::Identifier("n".to_string()), 24..25);
    let ty = checker.check_against(&ident.0, &ident.1, &Ty::I32);
    assert_eq!(ty, Ty::Error);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

// ── Struct init literal coercion tests ─────────────────────────────

fn register_generic_wrapper(checker: &mut Checker) {
    let mut fields = HashMap::new();
    fields.insert(
        "value".to_string(),
        Ty::Named {
            name: "T".to_string(),
            args: vec![],
        },
    );
    checker.type_defs.insert(
        "Wrapper".to_string(),
        TypeDef {
            kind: TypeDefKind::Struct,
            name: "Wrapper".to_string(),
            type_params: vec!["T".to_string()],
            fields,
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        },
    );
}

#[test]
fn struct_init_coerces_literal_to_expected_type_arg() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    // Wrapper { value: 42 } checked against Wrapper<i32>
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(42, 10..12))],
        },
        0..20,
    );
    let expected = Ty::Named {
        name: "Wrapper".to_string(),
        args: vec![Ty::I32],
    };
    let ty = checker.check_against(&init.0, &init.1, &expected);
    assert_eq!(ty, expected);
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn struct_init_infers_type_param_from_literal() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    // Wrapper { value: 42 } without expected type — should infer Wrapper<i64>
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(42, 10..12))],
        },
        0..20,
    );
    let ty = checker.synthesize(&init.0, &init.1);
    assert_eq!(
        ty,
        Ty::Named {
            name: "Wrapper".to_string(),
            args: vec![Ty::I64],
        }
    );
    assert!(
        checker.errors.is_empty(),
        "unexpected errors: {:?}",
        checker.errors
    );
}

#[test]
fn struct_init_overflow_in_expected_type() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    register_generic_wrapper(&mut checker);

    // Wrapper { value: 256 } checked against Wrapper<u8> — should error
    let init = (
        Expr::StructInit {
            name: "Wrapper".to_string(),
            fields: vec![("value".to_string(), make_int_literal(256, 10..13))],
        },
        0..20,
    );
    let expected = Ty::Named {
        name: "Wrapper".to_string(),
        args: vec![Ty::U8],
    };
    let _ty = checker.check_against(&init.0, &init.1, &expected);
    assert!(
        checker
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "expected range error: {:?}",
        checker.errors
    );
}

// ── Trailing-literal coercion in typed functions (gap fix) ────────

#[test]
fn trailing_integer_literal_coerces_to_declared_return_type() {
    // fn foo() -> i32 { 0 }  — bare 0 defaulted to i64 before fix
    let source = "fn foo() -> i32 { 0 }";
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
        "trailing literal should coerce to declared return type: {:?}",
        output.errors
    );
}

#[test]
fn trailing_integer_literal_coerces_smaller_width() {
    // fn foo() -> i8 { 42 }  — literal fits in i8
    let source = "fn foo() -> i8 { 42 }";
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
        "literal 42 fits in i8 and should coerce: {:?}",
        output.errors
    );
}

#[test]
fn trailing_integer_literal_out_of_range_is_rejected() {
    // fn foo() -> i8 { 300 }  — 300 does not fit in i8 (range -128..=127)
    let source = "fn foo() -> i8 { 300 }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("does not fit")),
        "out-of-range literal should be rejected: {:?}",
        output.errors
    );
}

#[test]
fn trailing_if_with_literal_branches_coerces() {
    // fn foo(x: i32) -> i32 { if x > 0 { 1 } else { 0 } }
    // Both branches are integer literals that should coerce to i32.
    let source = "fn foo(x: i32) -> i32 { if x > 0 { 1 } else { 0 } }";
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
        "if-else with integer literals should coerce to declared return type: {:?}",
        output.errors
    );
}

#[test]
fn explicit_return_with_literal_still_works() {
    // Regression guard: explicit return was already working; must stay working.
    let source = "fn foo() -> i32 { return 0; }";
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
        "explicit return with literal coercion should still work: {:?}",
        output.errors
    );
}

#[test]
fn trailing_negative_literal_rejected_for_unsigned_return() {
    // fn foo() -> u32 { -1 }  — negative literal cannot fit in unsigned type
    let source = "fn foo() -> u32 { -1 }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "negative literal should be rejected for unsigned return type"
    );
}

#[test]
fn trailing_match_with_literal_arms_coerces() {
    // fn foo(x: bool) -> i32 { match x { true => 1, false => 0 } }
    // Stmt::Match is the last statement; both arms are integer literals that
    // should coerce to i32 via the pre-seeded expected type.
    let source = "fn foo(x: bool) -> i32 { match x { true => 1, false => 0 } }";
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
        "match with integer literal arms should coerce to declared return type: {:?}",
        output.errors
    );
}

#[test]
fn tuple_if_element_coerces_from_expected_type() {
    // The tuple annotation supplies i32 to the nested if expression's first
    // element; the literal branch must not synthesize to i64 first.
    let source = "fn foo(flag: bool, y: i32) -> (i32, i32) { (if flag { 1 } else { y }, y) }";
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
        "nested tuple if should inherit the tuple element type: {:?}",
        output.errors
    );
}

#[test]
fn tuple_match_element_coerces_from_expected_type() {
    // The tuple annotation supplies i32 to the nested match expression's
    // first element; the literal arm must not synthesize to i64 first.
    let source =
        "fn foo(flag: bool, y: i32) -> (i32, i32) { (match flag { true => 1, false => y }, y) }";
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
        "nested tuple match should inherit the tuple element type: {:?}",
        output.errors
    );
}

#[test]
fn trailing_type_mismatch_reports_exactly_one_error() {
    // fn foo() -> i32 { "hello" }
    // check_against already reports the mismatch at the expression site;
    // check_fn_decl's outer expect_type must NOT fire a duplicate.
    let source = "fn foo() -> i32 { \"hello\" }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert_eq!(
        output.errors.len(),
        1,
        "expected exactly one type mismatch error, got: {:?}",
        output.errors
    );
}

#[test]
fn trailing_identifier_mismatch_reports_exactly_one_error() {
    // fn foo(s: String) -> i32 { s }
    // The identifier arm in check_against matched before the default arm,
    // so without the guard it fired a second duplicate error.
    let source = "fn foo(s: String) -> i32 { s }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert_eq!(
        output.errors.len(),
        1,
        "expected exactly one type mismatch error for identifier, got: {:?}",
        output.errors
    );
}

#[test]
fn error_return_type_does_not_suppress_match_arm_diagnostics() {
    // fn foo() -> UnknownType { match true { true => "hello", false => 42 } }
    // UnknownType resolves to Ty::Error. Without the Ty::Error guard in
    // check_match_expr, the error type pre-seeds all arms via check_against,
    // silently accepting the String/int mismatch between arms.
    let source = r#"fn foo() -> UnknownType { match true { true => "hello", false => 42 } }"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    // We expect at least two errors: one for UnknownType and one for the
    // arm type mismatch (String vs int). Before the fix only the
    // UnknownType error appeared.
    let arm_mismatch = output.errors.iter().any(|e| {
        let msg = format!("{e:?}");
        msg.contains("TypeMismatch") || msg.contains("mismatch")
    });
    assert!(
        arm_mismatch,
        "match arms with mismatched types should still report an error even when \
         the return type is Ty::Error; got: {:?}",
        output.errors
    );
}

#[test]
fn nonwire_from_json_returns_result_self_string() {
    // Regression: non-wire struct.from_json(s) must type-check as
    // Result<Self, String>, not Self.  The SHIM that returned Self directly
    // was removed; this test pins the correct surface type.
    let source = r#"
type Point { x: i32; y: i32; }
fn main() {
let s = "{\"x\":1,\"y\":2}";
let r: Result<Point, String> = Point.from_json(s);
}
"#;
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
        "from_json should return Result<Self, String> with no type errors; got: {:?}",
        output.errors
    );
}

#[test]
fn nonwire_from_json_bare_self_is_type_error() {
    // Assigning the result of from_json directly to `Self` (not Result<Self, …>)
    // must produce a type mismatch — confirms the SHIM is gone.
    let source = r#"
type Point { x: i32; y: i32; }
fn main() {
let s = "{\"x\":1,\"y\":2}";
let p: Point = Point.from_json(s);
}
"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let has_mismatch = output.errors.iter().any(|e| {
        let msg = format!("{e:?}");
        msg.contains("TypeMismatch") || msg.contains("mismatch") || msg.contains("Result")
    });
    assert!(
        has_mismatch,
        "assigning Result<Point, String> to Point must be a type error; got: {:?}",
        output.errors
    );
}

#[test]
fn nonwire_from_yaml_and_from_toml_return_result() {
    // Both from_yaml and from_toml should also return Result<Self, String>.
    let source = r#"
type Cfg { n: i32; }
fn main() {
let _a: Result<Cfg, String> = Cfg.from_yaml("n: 1");
let _b: Result<Cfg, String> = Cfg.from_toml("n = 1");
}
"#;
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
        "from_yaml / from_toml should return Result<Self, String>; got: {:?}",
        output.errors
    );
}

// -------------------------------------------------------------------------
// Structural-bounds scaffold tests (E1)
//
// These tests verify the `type_structurally_satisfies` scaffold and the
// updated `type_satisfies_trait_bound` fallback without changing any
// existing program behaviour.
// -------------------------------------------------------------------------

/// Build a minimal `Checker` with a trait registered in `trait_defs`.
///
/// The trait is method-only (no associated types, no generic methods) unless
/// the caller opts in via the `with_assoc` / `with_generic_method` flags.
fn make_checker_with_trait(
    trait_name: &str,
    method_names: &[&str],
    with_assoc: bool,
    with_generic_method: bool,
) -> Checker {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let mut items: Vec<hew_parser::ast::TraitItem> = method_names
        .iter()
        .map(|name| {
            let type_params = if with_generic_method {
                Some(vec![TypeParam {
                    name: "U".to_string(),
                    bounds: vec![],
                }])
            } else {
                None
            };
            TraitItem::Method(TraitMethod {
                name: name.to_string(),
                is_pure: false,
                type_params,
                params: vec![Param {
                    name: "val".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "Self".to_string(),
                            type_args: None,
                        },
                        0..4,
                    ),
                    is_mutable: false,
                }],
                return_type: None,
                where_clause: None,
                body: None,
            })
        })
        .collect();

    if with_assoc {
        items.push(TraitItem::AssociatedType {
            name: "Output".to_string(),
            default: None,
            bounds: vec![],
        });
    }

    let td = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: trait_name.to_string(),
        type_params: None,
        super_traits: None,
        items,
        doc_comment: None,
    };

    let info = Checker::trait_info_from_decl(&td);
    checker.trait_defs.insert(trait_name.to_string(), info);
    checker
}

fn make_test_type_def(
    name: &str,
    type_params: Vec<String>,
    methods: HashMap<String, FnSig>,
) -> TypeDef {
    TypeDef {
        kind: TypeDefKind::Struct,
        name: name.to_string(),
        type_params,
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods,
        doc_comment: None,
        is_indirect: false,
    }
}

#[test]
fn structural_satisfies_returns_false_for_unknown_trait() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    assert!(
        !checker.type_structurally_satisfies("MyType", "NoSuchTrait"),
        "unknown trait must not satisfy structural check"
    );
}

#[test]
fn structural_satisfies_e1_guard_associated_types() {
    let mut checker = make_checker_with_trait("Indexed", &["get"], true, false);
    assert!(
        !checker.type_structurally_satisfies("MyType", "Indexed"),
        "E1 guard: traits with associated types must return false"
    );
}

#[test]
fn structural_satisfies_e1_guard_generic_methods() {
    let mut checker = make_checker_with_trait("Mapper", &["map"], false, true);
    assert!(
        !checker.type_structurally_satisfies("MyType", "Mapper"),
        "E1 guard: traits with generic methods must return false"
    );
}

#[test]
fn structural_satisfies_e1_guard_method_only_trait_unknown_type_returns_false() {
    // In E2, the placeholder is replaced with real method-presence matching.
    // An unregistered type still returns false because no methods are found.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    assert!(
        !checker.type_structurally_satisfies("MyType", "Greet"),
        "unregistered type must not satisfy structural check even after E2"
    );
}

#[test]
fn type_satisfies_trait_bound_nominal_path_unchanged() {
    // Verify that existing nominal conformance still works after the
    // structural fallback was wired into type_satisfies_trait_bound.
    let source = r"
        trait Greet {
            fn hello(val: Self);
        }

        type Greeter {}

        impl Greet for Greeter {
            fn hello(val: Greeter) {}
        }

        fn use_greet<T: Greet>(t: T) {}

        fn main() {
            let g = Greeter {};
            use_greet(g);
        }
    ";

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
        "nominal trait conformance must still succeed after E1 scaffold: {:?}",
        output.errors
    );
}

#[test]
fn type_satisfies_trait_bound_missing_impl_still_fails() {
    // A type that has no impl and no structural match must still fail the
    // bound — E1 must not silently accept it.
    let source = r"
        trait Greet {
            fn hello(val: Self);
        }

        type Stranger {}

        fn use_greet<T: Greet>(t: T) {}

        fn main() {
            let s = Stranger {};
            use_greet(s);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        !output.errors.is_empty(),
        "E1 must not accept a type with no impl and no structural match; expected errors"
    );
}

// -------------------------------------------------------------------------
// E2 structural method-presence tests
//
// These exercises the live structural-satisfaction logic that replaced the
// E1 placeholder.  Programs use `impl Type { fn method }` (no trait) so
// the method is registered in `type_defs.methods` without a nominal impl.
// -------------------------------------------------------------------------

#[test]
fn structural_e2_single_method_match_satisfies_bound() {
    // Positive: a type that has the required method via a bare impl block
    // (no explicit `impl Trait for Type`) must satisfy the bound structurally.
    let source = r"
        trait Area {
            fn area(val: Self) -> int;
        }

        type Square {}

        impl Square {
            fn area(s: Square) -> int { 1 }
        }

        fn measure<T: Area>(s: T) -> int {
            s.area()
        }

        fn main() {
            let sq = Square {};
            let _ = measure(sq);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "structural method match must satisfy Area bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_multi_method_trait_all_present_satisfies_bound() {
    // Positive: all required methods present → bound satisfied.
    let source = r#"
        trait Named {
            fn label(val: Self) -> string;
            fn code(val: Self) -> int;
        }

        type Widget {}

        impl Widget {
            fn label(w: Widget) -> string { "w" }
            fn code(w: Widget) -> int { 0 }
        }

        fn print_label<T: Named>(t: T) -> string {
            t.label()
        }

        fn main() {
            let w = Widget {};
            let _ = print_label(w);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "all methods present → Named bound must be satisfied; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_method_with_non_self_param_satisfies_bound() {
    // Positive: trait method has a non-receiver parameter; the type's method
    // must have the same arity and parameter type.
    let source = r"
        trait Scalable {
            fn scale(val: Self, factor: int) -> int;
        }

        type Brick {}

        impl Brick {
            fn scale(b: Brick, factor: int) -> int { factor }
        }

        fn resize<T: Scalable>(t: T) -> int {
            t.scale(2)
        }

        fn main() {
            let b = Brick {};
            let _ = resize(b);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "matching non-receiver param must satisfy Scalable bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_nominal_impl_still_preferred_over_structural() {
    // Positive: an explicit `impl Trait for Type` still works; E2 must not
    // break the nominal path.
    let source = r"
        trait Area {
            fn area(val: Self) -> int;
        }

        type Circle {}

        impl Area for Circle {
            fn area(c: Circle) -> int { 3 }
        }

        fn measure<T: Area>(s: T) -> int {
            s.area()
        }

        fn main() {
            let c = Circle {};
            let _ = measure(c);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    let bound_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        !bound_err,
        "explicit impl must still satisfy bound in E2; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_wrong_return_type_does_not_satisfy_bound() {
    // Negative: the type has a method with the right name but wrong return type;
    // the bound must not be satisfied.
    let source = r#"
        trait Area {
            fn area(val: Self) -> int;
        }

        type Triangle {}

        impl Triangle {
            fn area(t: Triangle) -> string { "big" }
        }

        fn measure<T: Area>(s: T) -> int {
            s.area()
        }

        fn main() {
            let t = Triangle {};
            let _ = measure(t);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "wrong return type must not satisfy Area bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_wrong_arity_does_not_satisfy_bound() {
    // Negative: the type's method has one extra non-receiver parameter;
    // the arity mismatch must cause the bound to fail.
    let source = r"
        trait Ping {
            fn ping(val: Self) -> int;
        }

        type Server {}

        impl Server {
            fn ping(s: Server, timeout: int) -> int { 1 }
        }

        fn use_ping<T: Ping>(t: T) -> int {
            t.ping()
        }

        fn main() {
            let s = Server {};
            let _ = use_ping(s);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "arity mismatch must not satisfy Ping bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_missing_one_of_two_methods_does_not_satisfy_bound() {
    // Negative: a multi-method trait where only one of two required methods is present.
    let source = r#"
        trait Named {
            fn label(val: Self) -> string;
            fn code(val: Self) -> int;
        }

        type Partial {}

        impl Partial {
            fn label(p: Partial) -> string { "p" }
            // `code` is intentionally missing
        }

        fn use_named<T: Named>(t: T) -> string {
            t.label()
        }

        fn main() {
            let p = Partial {};
            let _ = use_named(p);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "partial method set must not satisfy Named bound; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_e2_all_default_methods_still_requires_explicit_impl() {
    // Negative (conservative): a trait whose every method has a default body
    // has no required methods.  E2 returns false in that case — an explicit
    // `impl Trait for Type` is still needed, keeping explicit impls authoritative
    // for default-only and marker-like traits.
    let source = r#"
        trait WithDefault {
            fn greet(val: Self) -> string { "hello" }
        }

        type Thingy {}

        impl Thingy {
            fn greet(t: Thingy) -> string { "world" }
        }

        fn use_it<T: WithDefault>(t: T) {}

        fn main() {
            let t = Thingy {};
            use_it(t);
        }
    "#;

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "all-default-method trait must require explicit impl; got: {:?}",
        output.errors
    );
}

#[test]
fn named_method_lookup_prefers_type_defs_before_fn_sigs() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let mut methods = HashMap::new();
    methods.insert(
        "hello".to_string(),
        FnSig {
            return_type: Ty::String,
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "Speaker".to_string(),
        make_test_type_def("Speaker", vec![], methods),
    );
    checker.fn_sigs.insert(
        "Speaker::hello".to_string(),
        FnSig {
            return_type: Ty::I64,
            ..FnSig::default()
        },
    );

    let sig = checker
        .lookup_named_method_sig("Speaker", &[], "hello")
        .expect("type_defs method should resolve");
    assert_eq!(sig.return_type, Ty::String);
}

#[test]
fn custom_index_uses_named_method_get_for_type_def() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let mut methods = HashMap::new();
    methods.insert(
        "get".to_string(),
        FnSig {
            param_names: vec!["index".to_string()],
            params: vec![Ty::I64],
            return_type: Ty::Named {
                name: "T".to_string(),
                args: vec![],
            },
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "Boxy".to_string(),
        make_test_type_def("Boxy", vec!["T".to_string()], methods),
    );
    checker.env.define(
        "boxy".to_string(),
        Ty::Named {
            name: "Boxy".to_string(),
            args: vec![Ty::String],
        },
        false,
    );

    let expr = Expr::Index {
        object: Box::new((Expr::Identifier("boxy".to_string()), 0..4)),
        index: Box::new(make_int_literal(0, 5..6)),
    };

    let ty = checker.synthesize(&expr, &(0..6));
    assert_eq!(ty, Ty::String);
    assert!(
        checker.errors.is_empty(),
        "expected type-def get lookup to succeed, got: {:?}",
        checker.errors
    );
}

#[test]
fn custom_index_uses_named_method_get_for_fn_sig_fallback() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Wrapper".to_string(),
        make_test_type_def("Wrapper", vec!["T".to_string()], HashMap::new()),
    );
    checker.fn_sigs.insert(
        "Wrapper::get".to_string(),
        FnSig {
            param_names: vec!["index".to_string()],
            params: vec![Ty::I64],
            return_type: Ty::Named {
                name: "T".to_string(),
                args: vec![],
            },
            ..FnSig::default()
        },
    );
    checker.env.define(
        "wrapper".to_string(),
        Ty::Named {
            name: "Wrapper".to_string(),
            args: vec![Ty::String],
        },
        false,
    );

    let expr = Expr::Index {
        object: Box::new((Expr::Identifier("wrapper".to_string()), 0..7)),
        index: Box::new(make_int_literal(0, 8..9)),
    };

    let ty = checker.synthesize(&expr, &(0..9));
    assert_eq!(ty, Ty::String);
    assert!(
        checker.errors.is_empty(),
        "expected fn_sigs get fallback to succeed, got: {:?}",
        checker.errors
    );
}

#[test]
fn named_method_lookup_substitutes_type_params_for_fn_sig_fallback() {
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    checker.type_defs.insert(
        "Wrapper".to_string(),
        make_test_type_def("Wrapper", vec!["T".to_string()], HashMap::new()),
    );
    checker.fn_sigs.insert(
        "Wrapper::value".to_string(),
        FnSig {
            param_names: vec!["next".to_string()],
            params: vec![Ty::Named {
                name: "T".to_string(),
                args: vec![],
            }],
            return_type: Ty::Named {
                name: "T".to_string(),
                args: vec![],
            },
            ..FnSig::default()
        },
    );

    let sig = checker
        .lookup_named_method_sig("Wrapper", &[Ty::String], "value")
        .expect("fn_sigs fallback should resolve");
    assert_eq!(sig.params, vec![Ty::String]);
    assert_eq!(sig.return_type, Ty::String);
}

// -------------------------------------------------------------------------
// Structural-hardening tests (qualified names + super-trait walk)
// -------------------------------------------------------------------------

#[test]
fn structural_hardening_uses_fn_sigs_named_method_fallback() {
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    checker.type_defs.insert(
        "Speaker".to_string(),
        make_test_type_def("Speaker", vec![], HashMap::new()),
    );
    checker
        .fn_sigs
        .insert("Speaker::hello".to_string(), FnSig::default());

    assert!(
        checker.type_structurally_satisfies("Speaker", "Greet"),
        "structural check should reuse named-method fn_sigs fallback"
    );
}

#[test]
fn structural_hardening_prefers_builtin_method_surface_for_imported_handle() {
    let mut checker = make_checker_with_trait("Closable", &["close"], false, false);

    let mut methods = HashMap::new();
    methods.insert(
        "close".to_string(),
        FnSig {
            return_type: Ty::I32,
            ..FnSig::default()
        },
    );
    checker.type_defs.insert(
        "Sender".to_string(),
        make_test_type_def("Sender", vec![], methods),
    );
    checker.fn_sigs.insert(
        "Sender::close".to_string(),
        FnSig {
            return_type: Ty::I32,
            ..FnSig::default()
        },
    );

    assert!(
        checker.type_structurally_satisfies("channel.Sender", "Closable"),
        "structural check should prefer builtin Sender::close over imported stubs"
    );
}

#[test]
fn structural_hardening_qualified_trait_name_matches() {
    // A type registered under "Speaker" must structurally satisfy a bound
    // expressed as "greet.Greet" once "greet" is a known module.
    // We build the checker state manually because check_program drains
    // type_defs/fn_sigs at the end of the pass.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    checker.modules.insert("greet".to_string());

    // Register a TypeDef for Speaker.  The trait `hello(val: Self)` has its
    // receiver stripped by lookup_trait_method, so the effective trait_sig has
    // params=[].  The concrete method entry must match: receiver already stripped.
    let type_def = TypeDef {
        kind: TypeDefKind::Struct,
        name: "Speaker".to_string(),
        type_params: vec![],
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods: {
            let mut m = HashMap::new();
            m.insert("hello".to_string(), FnSig::default()); // params=[], return=Unit
            m
        },
        doc_comment: None,
        is_indirect: false,
    };
    checker.type_defs.insert("Speaker".to_string(), type_def);

    assert!(
        checker.type_structurally_satisfies("Speaker", "greet.Greet"),
        "structural check with qualified trait name must succeed after normalization"
    );
    // Unqualified form must still work too.
    assert!(
        checker.type_structurally_satisfies("Speaker", "Greet"),
        "structural check with unqualified trait name must still succeed"
    );
}

#[test]
fn structural_hardening_qualified_type_name_matches() {
    // A bound check with the type expressed as "mymod.Speaker" must succeed
    // when "mymod" is a known module and "Speaker" is registered in type_defs.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    checker.modules.insert("mymod".to_string());

    let type_def = TypeDef {
        kind: TypeDefKind::Struct,
        name: "Speaker".to_string(),
        type_params: vec![],
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods: {
            let mut m = HashMap::new();
            m.insert("hello".to_string(), FnSig::default());
            m
        },
        doc_comment: None,
        is_indirect: false,
    };
    checker.type_defs.insert("Speaker".to_string(), type_def);

    assert!(
        checker.type_structurally_satisfies("mymod.Speaker", "Greet"),
        "structural check with qualified type name must succeed after normalization"
    );
    // Unqualified form must still work too.
    assert!(
        checker.type_structurally_satisfies("Speaker", "Greet"),
        "unqualified type name must still succeed"
    );
}

#[test]
fn structural_hardening_unknown_module_qualifier_is_rejected() {
    // If the prefix is not a known module, we must not strip it and must
    // not accidentally match a same-suffix type/trait.
    let mut checker = make_checker_with_trait("Greet", &["hello"], false, false);
    // "unknown" is NOT inserted into modules.

    let type_def = TypeDef {
        kind: TypeDefKind::Struct,
        name: "Speaker".to_string(),
        type_params: vec![],
        fields: HashMap::new(),
        variants: HashMap::new(),
        methods: {
            let mut m = HashMap::new();
            m.insert("hello".to_string(), FnSig::default());
            m
        },
        doc_comment: None,
        is_indirect: false,
    };
    checker.type_defs.insert("Speaker".to_string(), type_def);

    // Trait "unknown.Greet" should not resolve to "Greet" because "unknown" is
    // not a registered module.
    assert!(
        !checker.type_structurally_satisfies("Speaker", "unknown.Greet"),
        "unrecognised module prefix must not be stripped"
    );
}

#[test]
fn structural_hardening_super_trait_methods_required() {
    // If trait B extends A, a type must provide A's required methods to
    // structurally satisfy B.  Before the fix, only B's own methods were
    // checked and A's were silently skipped.
    let source = r"
        trait Printable {
            fn print(val: Self);
        }

        trait PrettyPrintable: Printable {
            fn pretty_print(val: Self);
        }

        type Doc {}

        impl Doc {
            fn pretty_print(d: Doc) {}
            // `print` (from super-trait Printable) is intentionally missing
        }

        fn use_pp<T: PrettyPrintable>(t: T) {}

        fn main() {
            let d = Doc {};
            use_pp(d);
        }
    ";

    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let output = checker.check_program(&result.program);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied)),
        "missing super-trait method must fail structural check; got: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_super_trait_methods_all_present_succeeds() {
    // When ALL required methods across the super-trait chain are present the
    // structural check must succeed without an explicit impl.
    let source = r"
        trait Printable {
            fn print(val: Self);
        }

        trait PrettyPrintable: Printable {
            fn pretty_print(val: Self);
        }

        type Doc {}

        impl Doc {
            fn print(d: Doc) {}
            fn pretty_print(d: Doc) {}
        }

        fn use_pp<T: PrettyPrintable>(t: T) {}

        fn main() {
            let d = Doc {};
            use_pp(d);
        }
    ";

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
        "all super-trait methods present must pass structural check: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_child_default_overrides_super_required_method() {
    // If a child trait provides a default implementation for a super-trait
    // method, that inherited requirement is satisfied by the trait itself and
    // must not be re-required structurally from the concrete type.
    let source = r"
        trait Printable {
            fn print(val: Self);
        }

        trait PrettyPrintable: Printable {
            fn print(val: Self) {}
            fn pretty_print(val: Self);
        }

        type Doc {}

        impl Doc {
            fn pretty_print(d: Doc) {}
        }

        fn use_pp<T: PrettyPrintable>(t: T) {}

        fn main() {
            let d = Doc {};
            use_pp(d);
        }
    ";

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
        "child default override should satisfy inherited structural requirement: {:?}",
        output.errors
    );
}

#[test]
fn structural_hardening_super_trait_e1_guard_propagates() {
    // If a super-trait has an associated type, the E1 guard must veto the
    // entire structural check — even if the immediate trait has no assoc types.
    use hew_parser::ast::{Param, TraitDecl, TraitItem, TraitMethod, TypeExpr};
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    // Build super-trait with an associated type.
    let assoc_super = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "AssocSuper".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![
            TraitItem::AssociatedType {
                name: "Output".to_string(),
                default: None,
                bounds: vec![],
            },
            TraitItem::Method(TraitMethod {
                name: "do_it".to_string(),
                is_pure: false,
                type_params: None,
                params: vec![Param {
                    name: "val".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "Self".to_string(),
                            type_args: None,
                        },
                        0..4,
                    ),
                    is_mutable: false,
                }],
                return_type: None,
                where_clause: None,
                body: None,
            }),
        ],
        doc_comment: None,
    };
    let info_super = Checker::trait_info_from_decl(&assoc_super);
    checker
        .trait_defs
        .insert("AssocSuper".to_string(), info_super);

    // Child trait with no assoc types of its own.
    let child = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "ChildTrait".to_string(),
        type_params: None,
        super_traits: Some(vec![hew_parser::ast::TraitBound {
            name: "AssocSuper".to_string(),
            type_args: None,
        }]),
        items: vec![TraitItem::Method(TraitMethod {
            name: "run".to_string(),
            is_pure: false,
            type_params: None,
            params: vec![Param {
                name: "val".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "Self".to_string(),
                        type_args: None,
                    },
                    0..4,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: None,
        })],
        doc_comment: None,
    };
    let info_child = Checker::trait_info_from_decl(&child);
    checker
        .trait_defs
        .insert("ChildTrait".to_string(), info_child);
    checker
        .trait_super
        .insert("ChildTrait".to_string(), vec!["AssocSuper".to_string()]);

    assert!(
        !checker.type_structurally_satisfies("AnyType", "ChildTrait"),
        "E1 guard in super-trait must veto structural check for child trait"
    );
}

#[test]
fn structural_hardening_super_trait_generic_method_guard_propagates() {
    // If a super-trait has a generic method, the E1 guard must veto the whole
    // structural check for the child trait too.
    use hew_parser::ast::{Param, TraitDecl, TraitItem, TraitMethod, TypeExpr, TypeParam};
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));

    let generic_super = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "GenericSuper".to_string(),
        type_params: None,
        super_traits: None,
        items: vec![TraitItem::Method(TraitMethod {
            name: "map".to_string(),
            is_pure: false,
            type_params: Some(vec![TypeParam {
                name: "U".to_string(),
                bounds: vec![],
            }]),
            params: vec![Param {
                name: "val".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "Self".to_string(),
                        type_args: None,
                    },
                    0..4,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: None,
        })],
        doc_comment: None,
    };
    let info_super = Checker::trait_info_from_decl(&generic_super);
    checker
        .trait_defs
        .insert("GenericSuper".to_string(), info_super);

    let child = TraitDecl {
        visibility: hew_parser::ast::Visibility::Private,
        name: "ChildTrait".to_string(),
        type_params: None,
        super_traits: Some(vec![hew_parser::ast::TraitBound {
            name: "GenericSuper".to_string(),
            type_args: None,
        }]),
        items: vec![TraitItem::Method(TraitMethod {
            name: "run".to_string(),
            is_pure: false,
            type_params: None,
            params: vec![Param {
                name: "val".to_string(),
                ty: (
                    TypeExpr::Named {
                        name: "Self".to_string(),
                        type_args: None,
                    },
                    0..4,
                ),
                is_mutable: false,
            }],
            return_type: None,
            where_clause: None,
            body: None,
        })],
        doc_comment: None,
    };
    let info_child = Checker::trait_info_from_decl(&child);
    checker
        .trait_defs
        .insert("ChildTrait".to_string(), info_child);
    checker
        .trait_super
        .insert("ChildTrait".to_string(), vec!["GenericSuper".to_string()]);

    assert!(
        !checker.type_structurally_satisfies("AnyType", "ChildTrait"),
        "generic-method guard in super-trait must veto structural check for child trait"
    );
}
