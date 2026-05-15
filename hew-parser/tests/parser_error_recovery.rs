use hew_parser::ast::{CallArg, Expr, Item, Stmt};

/// `=~` was removed in v0.5: regex matching now goes through `Pattern.is_match`.
/// The lexer no longer recognizes `=~` as a token, so the parser sees `=`
/// followed by `~` and errors out. We don't assert on a specific error
/// message — only that the input fails to parse cleanly.
#[test]
fn removed_regex_match_op_is_rejected() {
    let source = r#"fn main() { let x = "hi" =~ re"a"; }"#;
    let result = hew_parser::parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected `=~` to be rejected, got clean parse"
    );
}

/// Companion to `removed_regex_match_op_is_rejected`: `!~` is also gone.
#[test]
fn removed_regex_not_match_op_is_rejected() {
    let source = r#"fn main() { let x = "hi" !~ re"a"; }"#;
    let result = hew_parser::parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected `!~` to be rejected, got clean parse"
    );
}

/// Accept side of §3.3: the supported replacement (`Pattern.is_match`) parses
/// cleanly. Pairs with the reject tests above so the removal is exercised
/// from both directions.
#[test]
fn pattern_is_match_method_call_parses() {
    let source = r#"fn main() { let p = re"a"; let x = p.is_match("hi"); p.free(); }"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "expected clean parse of `.is_match()`, got errors: {:?}",
        result.errors
    );
}

/// `Pattern.matches` (the non-boolean iterator form) also parses cleanly.
/// Ensures both replacement methods are accepted by the parser.
#[test]
fn pattern_matches_method_call_parses() {
    let source = r#"fn main() { let p = re"a+"; let ms = p.matches("aab"); p.free(); }"#;
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "expected clean parse of `.matches()`, got errors: {:?}",
        result.errors
    );
}

#[test]
fn missing_param_type_reports_error() {
    let source = r"
        fn demo(a) {}
    ";
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
    let source = r"
        pub(invalid) fn demo() {}
    ";
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
    let source = r"
        fn demo() { let c = '\q'; }
    ";
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
    let source = r"
        enum {}
    ";
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
    let source = r"
        fn demo() { foo(a: 1, 2); }
    ";
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
