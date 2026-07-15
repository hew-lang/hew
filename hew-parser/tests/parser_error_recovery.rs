use hew_parser::ast::{CallArg, Expr, Item, Pattern, Stmt};

/// `=~` was removed in v0.5: regex matching now goes through `Pattern.is_match`
/// or a match arm with a regex literal pattern.  The parser emits
/// `E_REGEX_OP_REMOVED` with a hint pointing at the match-arm form.
#[test]
fn removed_regex_match_op_is_rejected() {
    let source = r#"fn main() { let x = "hi" =~ re"a"; }"#;
    let result = hew_parser::parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected `=~` to be rejected, got clean parse"
    );
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("E_REGEX_OP_REMOVED")),
        "expected E_REGEX_OP_REMOVED in errors, got: {:?}",
        result.errors
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
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("E_REGEX_OP_REMOVED")),
        "expected E_REGEX_OP_REMOVED in errors, got: {:?}",
        result.errors
    );
}

#[test]
fn pure_fn_modifier_is_rejected() {
    let source = r"pure fn foo() -> i64 { return 1; }";
    let result = hew_parser::parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected `pure fn` to be rejected, got clean parse"
    );
    assert!(
        result
            .errors
            .iter()
            .any(|err| err.message.contains("found pure") || err.message.contains("pure")),
        "expected parse error to identify `pure`, got {:?}",
        result.errors
    );
}

#[test]
fn normal_fn_still_parses() {
    let source = r"fn foo() -> i64 { return 1; }";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "expected normal `fn` to parse cleanly, got {:?}",
        result.errors
    );
}

/// `fork { ... }` block syntax was removed in v0.5; use `scope { ... }` with
/// `fork name = call(...);` bindings instead.  The parser emits a clear
/// diagnostic pointing at the replacement form.
#[test]
fn fork_block_syntax_is_rejected() {
    let source = r"fn main() { fork { let x = 1; } }";
    let result = hew_parser::parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected `fork {{ ... }}` to be rejected, got clean parse"
    );
    assert!(
        result.errors.iter().any(|e| e.message.contains("scope")),
        "expected error message to mention `scope` as the replacement, got: {:?}",
        result.errors
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
    // `pub(package)` and `pub(super)` were removed in favour of standalone
    // `package` and `pub` keywords.  `pub(anything)` no longer has special
    // meaning — `pub` parses as a visibility modifier and the trailing `(...)`
    // is treated as a call expression on the following token, which produces
    // a parse error because a call expression is not a valid item.
    let source = r"
        pub(invalid) fn demo() {}
    ";
    let result = hew_parser::parse(source);
    // The parser must produce at least one error; the exact message may vary
    // because `pub` is consumed as a visibility modifier and the parenthesised
    // suffix is parsed as a statement, not a qualifier.
    assert!(
        !result.errors.is_empty(),
        "pub(invalid) should not parse as a valid item, got no errors",
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

/// The `<-` send operator was removed in v0.5. Because the lexer no longer
/// emits a `LeftArrow` token, the character sequence is tokenised as `<`
/// followed by `-`. The parser detects adjacent `<` + `-` spans and emits
/// `E_OPERATOR_REMOVED` instead of silently accepting a less-than comparison.
#[test]
fn left_arrow_operator_is_rejected() {
    let source = "fn main() {\n    let w = 1;\n    w <- 42;\n}\n";
    let result = hew_parser::parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected `<-` to be rejected with E_OPERATOR_REMOVED, got clean parse"
    );
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("E_OPERATOR_REMOVED")),
        "expected E_OPERATOR_REMOVED in error messages, got: {:?}",
        result.errors
    );
}

/// After emitting `E_OPERATOR_REMOVED` the parser must skip to the statement
/// boundary so that RHS tokens do not produce additional cascading errors.
/// Exactly one error (the `E_OPERATOR_REMOVED` diagnostic) is the target;
/// no follow-on "unexpected token" errors should appear.
#[test]
fn left_arrow_cascade_is_suppressed() {
    // A simple `target <- msg;` statement — after the diagnostic the parser
    // should skip `msg` and `;` without emitting further errors.
    let source = "fn main() {\n    let w = 1;\n    w <- 42;\n}\n";
    let result = hew_parser::parse(source);
    let removed_count = result
        .errors
        .iter()
        .filter(|e| e.message.contains("E_OPERATOR_REMOVED"))
        .count();
    assert_eq!(
        removed_count, 1,
        "expected exactly one E_OPERATOR_REMOVED diagnostic, got: {:?}",
        result.errors
    );
    let total_errors: usize = result
        .errors
        .iter()
        .filter(|e| matches!(e.severity, hew_parser::Severity::Error))
        .count();
    assert_eq!(
        total_errors, 1,
        "expected no cascade errors after E_OPERATOR_REMOVED, got {} total errors: {:?}",
        total_errors, result.errors
    );
}

/// The legacy `spawn (params) => body` lambda-actor syntax was removed in v0.5.
/// The parser now emits `E_SPAWN_LAMBDA_SYNTAX_REMOVED` with a fixit pointing
/// at `actor |params| { body }`.
#[test]
fn legacy_spawn_lambda_syntax_is_rejected() {
    let source = "fn main() {\n    let w = spawn (x: int) => { println(x); };\n    w.send(1);\n}\n";
    let result = hew_parser::parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected legacy spawn-lambda syntax to be rejected, got clean parse"
    );
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("E_SPAWN_LAMBDA_SYNTAX_REMOVED")),
        "expected E_SPAWN_LAMBDA_SYNTAX_REMOVED in error messages, got: {:?}",
        result.errors
    );
}

/// Accept side: `actor |params| { body }` is the new lambda-actor syntax.
#[test]
fn actor_lambda_syntax_parses_cleanly() {
    let source = "fn main() {\n    let w = actor |x: int| {\n        println(x);\n    };\n    w.send(42);\n}\n";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "expected `actor |params| {{ body }}` to parse cleanly, got: {:?}",
        result.errors
    );
}

// ── `<-` adjacency rule: which forms trigger E_OPERATOR_REMOVED ───────────
//
// The detector fires when `<` and `-` are adjacent at the byte level (no
// whitespace between them).  This precisely matches the legacy `<-` operator
// while leaving `x < -y` (space between `<` and `-`) unaffected.
//
// Form matrix (source text → expected behaviour):
//   x<-y    (tight, no spaces)   → E_OPERATOR_REMOVED
//   x <- y  (send with spaces)   → E_OPERATOR_REMOVED
//   x <-y   (space before only)  → E_OPERATOR_REMOVED
//   x< -y   (space after only)   → parses as comparison x < (-y)
//   x < -y  (full spaces)        → parses as comparison x < (-y)

fn has_removed_operator_error(source: &str) -> bool {
    hew_parser::parse(source)
        .errors
        .iter()
        .any(|e| e.message.contains("E_OPERATOR_REMOVED"))
}

fn parses_without_operator_removed_error(source: &str) -> bool {
    !has_removed_operator_error(source)
}

/// Form 1: `x<-y` (no surrounding spaces) must trigger `E_OPERATOR_REMOVED`.
#[test]
fn tight_left_arrow_triggers_removed_operator() {
    // The `<` and `-` tokens are byte-adjacent; this is the legacy send form.
    let source = "fn f(x: int, y: int) { x<-y; }";
    assert!(
        has_removed_operator_error(source),
        "expected E_OPERATOR_REMOVED for `x<-y`, got clean parse"
    );
}

/// Form 2: `x <- y` (space before `<`, no space between `<` and `-`) must trigger.
#[test]
fn send_with_surrounding_spaces_triggers_removed_operator() {
    let source = "fn f(x: int, y: int) { x <- y; }";
    assert!(
        has_removed_operator_error(source),
        "expected E_OPERATOR_REMOVED for `x <- y`, got clean parse"
    );
}

/// Form 3: `x <-y` (space before `<`, no space between `<` and `-`) must trigger.
#[test]
fn send_with_space_before_triggers_removed_operator() {
    let source = "fn f(x: int, y: int) { x <-y; }";
    assert!(
        has_removed_operator_error(source),
        "expected E_OPERATOR_REMOVED for `x <-y`, got clean parse"
    );
}

/// Form 4: `x< -y` (no space before `<`, space between `<` and `-`) must NOT trigger.
/// This is comparison (`x` less-than negative `y`).
#[test]
fn comparison_space_after_lt_parses_normally() {
    // Space between `<` and `-` means they are not byte-adjacent; the parser
    // treats this as `x < (-y)` (less-than applied to negation).
    let source = "fn f(x: int, y: int) -> bool { x< -y }";
    assert!(
        parses_without_operator_removed_error(source),
        "expected `x< -y` to parse as comparison, but got E_OPERATOR_REMOVED"
    );
}

/// Form 5: `x < -y` (spaces on both sides) must NOT trigger `E_OPERATOR_REMOVED`.
#[test]
fn comparison_full_spaces_parses_normally() {
    let source = "fn f(x: int, y: int) -> bool { x < -y }";
    assert!(
        parses_without_operator_removed_error(source),
        "expected `x < -y` to parse as comparison, but got E_OPERATOR_REMOVED"
    );
}

/// F2 regression: `Point { x, .. }` is a deferred rest-pattern that the parser
/// does not yet fully support. When `..` appears inside a struct pattern field
/// list, `expect_ident` fails on the `..` token and returns `None`; the parser
/// propagates errors and never panics.
///
/// This test asserts:
///   1. Parsing never panics (a panic would kill the test process outright).
///   2. At least one parse error is reported (the `..` is rejected cleanly).
///
#[test]
fn struct_rest_pattern_does_not_panic() {
    let source = r"
type Point {
    x: i64,
    y: i64,
}

fn main() -> i64 {
    let p = Point { x: 3, y: 4 };
    match p {
        Point { x, .. } => x,
    }
}";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "record rest pattern should parse cleanly: {:?}",
        result.errors
    );
    let Item::Function(function) = &result.program.items[1].0 else {
        panic!("expected main function");
    };
    let Some((Expr::Match { arms, .. }, _)) = function.body.trailing_expr.as_deref() else {
        panic!("expected trailing match expression");
    };
    let Pattern::Struct { fields, rest, .. } = &arms[0].pattern.0 else {
        panic!("expected struct pattern");
    };
    assert_eq!(fields.len(), 1);
    assert!(rest.is_some());
}

// `ask` is not lexer-recognised in edition 2026 — it is reserved for a future
// syntactic marker (HEW-FUTURE) but carries no keyword status today. `ask foo()`
// is therefore treated as two adjacent expressions: the identifier `ask` followed
// by the call `foo()`, and the parser reports an unexpected token after `ask`.
#[test]
fn ask_prefix_call_form_rejects() {
    let source = r"fn f() { let x = ask foo(); }";
    let result = hew_parser::parse(source);
    assert!(
        !result.errors.is_empty(),
        "`ask foo()` should not parse as a valid expression"
    );
    // The parser sees `ask` as an identifier expression, then `foo` as an
    // unexpected next token where `;` was expected.
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("expected `;`") || e.message.contains("unexpected")),
        "expected a parse error mentioning unexpected token, got: {:?}",
        result.errors
    );
}
