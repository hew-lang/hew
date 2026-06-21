//! `let-else` parsing (v0.6 error-prop ergonomics).
//!
//! `let Pat = expr else { <diverging block> };` carries the else block
//! structurally on `Stmt::Let { else_block }` so the type checker can enforce
//! that it diverges. The else clause parses after the value and before the
//! terminating `;`. A refutable pattern is admitted only when an `else` clause
//! is present; the divergence obligation is the checker's, not the parser's.

use hew_parser::ast::{Item, Stmt};
use hew_parser::parse;

/// Parse a source string, assert no parse errors, and return the statements of
/// the single function's body.
fn parse_fn_stmts(src: &str) -> Vec<Stmt> {
    let result = parse(src);
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
        Item::Function(f) => f.body.stmts.into_iter().map(|(s, _)| s).collect(),
        _ => panic!("expected Item::Function"),
    }
}

/// Parse a full source string and assert it produces no parse errors.
fn parse_ok(src: &str) {
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors for `{src}`: {:#?}",
        result.errors
    );
}

/// Parse a full source string and assert it produces at least one parse error
/// whose message contains `needle`.
fn parse_err_contains(src: &str, needle: &str) {
    let result = parse(src);
    assert!(
        result.errors.iter().any(|e| e.message.contains(needle)),
        "expected a parse error containing `{needle}` for `{src}`, got: {:#?}",
        result.errors
    );
}

// -- Accept: `let-else` carries the else block -------------------------------

#[test]
fn let_else_carries_else_block() {
    // The dogfood bind-or-bail repro: a refutable `Ok(_)` pattern with an else
    // clause that diverges. The parser admits it and carries the block.
    let stmts = parse_fn_stmts(
        "fn f() -> Result<i64, string> { let Ok(n) = validate(s) else { return Err(\"bad\") }; Ok(n) }",
    );
    let Stmt::Let {
        pattern,
        else_block,
        ..
    } = &stmts[0]
    else {
        panic!("expected Stmt::Let, got {:?}", stmts[0]);
    };
    assert!(
        else_block.is_some(),
        "expected an else block on the let-else, got None"
    );
    // The Ok-path pattern is preserved verbatim (it is the binder source).
    assert!(
        matches!(&pattern.0, hew_parser::ast::Pattern::Constructor { .. }),
        "expected a constructor pattern (Ok(n)), got {:?}",
        pattern.0
    );
}

#[test]
fn ordinary_let_has_no_else_block() {
    // A plain `let` (no `else`) leaves `else_block` as None.
    let stmts = parse_fn_stmts("fn f() { let x = 1; }");
    let Stmt::Let { else_block, .. } = &stmts[0] else {
        panic!("expected Stmt::Let, got {:?}", stmts[0]);
    };
    assert!(
        else_block.is_none(),
        "expected None else_block on an ordinary let, got {else_block:?}"
    );
}

#[test]
fn let_else_with_multi_statement_block_parses() {
    // The else block is a full block: multiple statements before the divergent
    // tail are admitted by the parser (the checker proves divergence later).
    parse_ok(
        "fn f() -> Result<i64, string> { \
         let Ok(n) = validate(s) else { log(\"fail\"); return Err(\"bad\") }; Ok(n) }",
    );
}

#[test]
fn let_else_inline_block_parses() {
    // The single-line form used in the combined dogfood example.
    parse_ok(
        "fn f() -> Result<i64, string> { let Ok(p) = parse(s) else { return Err(\"e\") }; Ok(p) }",
    );
}

// -- Reject: contradictory or incomplete let-else forms ----------------------

#[test]
fn let_propagate_with_else_is_rejected() {
    // `let r? = e else {...}` -- the `?` suffix already supplies a fallback, so
    // an `else` clause is contradictory.
    parse_err_contains(
        "fn f() -> Result<i64, string> { let r? = e() else { return Err(\"x\") }; Ok(r) }",
        "`?` propagation suffix and an `else` clause cannot both",
    );
}

#[test]
fn let_else_without_initialiser_is_rejected() {
    // `let x else {...}` has nothing to bind -- there is no value to match.
    parse_err_contains(
        "fn f() { let x else { return }; }",
        "requires an initialiser before the",
    );
}
