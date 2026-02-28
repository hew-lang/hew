use hew_parser::ast::{CallArg, Expr, Item, Stmt};

#[test]
fn missing_param_type_reports_error() {
    let source = r#"
        fn demo(a) {}
    "#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|err| err
            .message
            .contains("expected ':' and type annotation for parameter")),
        "expected missing type error, got {:?}",
        result.errors
    );
}

#[test]
fn invalid_pub_scope_reports_error() {
    let source = r#"
        pub(invalid) fn demo() {}
    "#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|err| err
            .message
            .contains("expected 'package' or 'super' after 'pub('")),
        "expected pub scope error, got {:?}",
        result.errors
    );
}

#[test]
fn invalid_char_escape_reports_error() {
    let source = r#"
        fn demo() { let c = '\q'; }
    "#;
    let result = hew_parser::parse(source);
    assert!(
        result
            .errors
            .iter()
            .any(|err| err.message.contains("invalid escape sequence")),
        "expected invalid escape error, got {:?}",
        result.errors
    );
}

#[test]
fn invalid_enum_decl_reports_error() {
    let source = r#"
        enum {}
    "#;
    let result = hew_parser::parse(source);
    assert!(
        result
            .errors
            .iter()
            .any(|err| err.message.contains("expected identifier")),
        "expected identifier error, got {:?}",
        result.errors
    );
}

#[test]
fn positional_after_named_arg_is_skipped() {
    let source = r#"
        fn demo() { foo(a: 1, 2); }
    "#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.iter().any(|err| err
            .message
            .contains("positional arguments must come before named arguments")),
        "expected positional-after-named error, got {:?}",
        result.errors
    );
    let item = &result.program.items[0].0;
    let args = match item {
        Item::Function(f) => match &f.body.stmts[0].0 {
            Stmt::Expression(expr) => match &expr.0 {
                Expr::Call { args, .. } => args,
                _ => panic!("expected call expression"),
            },
            _ => panic!("expected expression statement"),
        },
        _ => panic!("expected function item"),
    };
    assert_eq!(args.len(), 1, "expected only named args, got {args:?}");
    match &args[0] {
        CallArg::Named { name, .. } => assert_eq!(name, "a"),
        CallArg::Positional(_) => panic!("expected named argument"),
    }
}
