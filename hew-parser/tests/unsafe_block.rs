use hew_parser::{ast::Expr, parse};

/// Helper: parse a source string that wraps an expression inside a function
/// and return the first item's function body's trailing expression.
fn parse_expr_in_fn(src: &str) -> Expr {
    let source = format!("fn f() {{ {src} }}");
    let result = parse(&source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors for `{src}`: {:#?}",
        result.errors
    );
    let (item, _span) = result
        .program
        .items
        .into_iter()
        .next()
        .expect("expected at least one item");
    match item {
        hew_parser::ast::Item::Function(f) => {
            f.body.trailing_expr.expect("expected trailing expr").0
        }
        _ => panic!("expected Item::Function"),
    }
}

/// Helper: parse source and assert at least one error is emitted.
fn parse_expects_error(src: &str) {
    let result = parse(src);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `{src}`, but got none"
    );
}

// ── Accept: valid unsafe block forms ────────────────────────────────────────

#[test]
fn unsafe_stmt_block_parses() {
    let expr = parse_expr_in_fn("unsafe { do_thing(); }");
    assert!(
        matches!(expr, Expr::UnsafeBlock(_)),
        "expected Expr::UnsafeBlock, got {expr:?}"
    );
}

#[test]
fn unsafe_block_as_let_value_parses() {
    // `let x = unsafe { compute() };` — unsafe block in value position
    let source = "fn f() -> int { let x = unsafe { compute() }; x }";
    let result = parse(source);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:#?}",
        result.errors
    );
}

#[test]
fn unsafe_block_multiple_stmts_parses() {
    let expr = parse_expr_in_fn("unsafe { x = 1; y = 2; }");
    assert!(
        matches!(expr, Expr::UnsafeBlock(_)),
        "expected Expr::UnsafeBlock, got {expr:?}"
    );
}

#[test]
fn unsafe_block_with_trailing_value_parses() {
    // unsafe { expr } returns the trailing expression's value.
    let expr = parse_expr_in_fn("unsafe { compute() }");
    assert!(
        matches!(expr, Expr::UnsafeBlock(_)),
        "expected Expr::UnsafeBlock, got {expr:?}"
    );
}

#[test]
fn nested_unsafe_blocks_parse_silently() {
    // Per Q-M5-D4: nested unsafe blocks are silent in v0.5 — no warning,
    // no rejection.
    let expr = parse_expr_in_fn("unsafe { unsafe { inner() } }");
    assert!(
        matches!(expr, Expr::UnsafeBlock(_)),
        "expected outer Expr::UnsafeBlock for nested form, got {expr:?}"
    );
}

// ── Reject: malformed unsafe forms ──────────────────────────────────────────

#[test]
fn bare_unsafe_semicolon_is_parse_error() {
    // `unsafe;` without a brace is malformed — `unsafe` must be followed by `{`.
    parse_expects_error("fn f() { unsafe; }");
}

#[test]
fn unsafe_without_brace_call_is_parse_error() {
    // `unsafe foo()` — missing `{` before function call.
    parse_expects_error("fn f() { unsafe foo(); }");
}
