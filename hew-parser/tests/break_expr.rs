//! `break` and `continue` as first-class expressions (v0.6 loop ergonomics).
//!
//! Before this slice, `break`/`continue` in expression position (a match-arm
//! body, an `else` block tail, ...) errored with "expected expression, found
//! `break`" even though the statement form `{ break; }` in the identical
//! position compiled. The parser now desugars the expression-position
//! spelling to the same one-statement-block AST the working `{ break; }`
//! form already produces, mirroring how `return` was made expression-capable.

use hew_parser::ast::{Block, Expr, Item, MatchArm, Stmt};
use hew_parser::parse;

/// Parse a source string wrapping a statement inside a function and return
/// the function's body block (`while`/`loop` are statements, not
/// trailing-expr-promoted, so this returns `Block` rather than `Expr`).
fn parse_fn_body(src: &str) -> Block {
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
        Item::Function(f) => f.body,
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

/// Parse a full source string and assert it produces at least one error.
fn parse_err(src: &str) {
    let result = parse(src);
    assert!(
        !result.errors.is_empty(),
        "expected parse errors for `{src}`, got none"
    );
}

// ── Accept: `break` in expression position ──────────────────────────────────

#[test]
fn bare_break_in_match_arm_parses() {
    // The dogfood repro: a bare match arm whose body is `break`.
    let body = parse_fn_body("while true { let x = match 1 { 1 => break, _ => 0 }; }");
    let Stmt::While { body, .. } = &body.stmts[0].0 else {
        panic!("expected Stmt::While, got {:?}", body.stmts[0].0);
    };
    let Stmt::Let { value, .. } = &body.stmts[0].0 else {
        panic!("expected Stmt::Let, got {:?}", body.stmts[0].0);
    };
    let value = value.as_ref().expect("expected let initialiser");
    let Expr::Match { arms, .. } = &value.0 else {
        panic!("expected Expr::Match, got {:?}", value.0);
    };
    let MatchArm { body: arm_body, .. } = &arms[0];
    assert!(
        matches!(
            &arm_body.0,
            Expr::Block(Block { stmts, trailing_expr: None })
                if stmts.len() == 1 && matches!(stmts[0].0, Stmt::Break { label: None, value: None })
        ),
        "expected desugared Expr::Block wrapping Stmt::Break, got {:?}",
        arm_body.0
    );
}

#[test]
fn break_with_value_in_match_arm_parses() {
    let body = parse_fn_body("loop { let x = match 1 { 1 => break 42, _ => 0 }; }");
    let Stmt::Loop { body, .. } = &body.stmts[0].0 else {
        panic!("expected Stmt::Loop, got {:?}", body.stmts[0].0);
    };
    let Stmt::Let { value, .. } = &body.stmts[0].0 else {
        panic!("expected Stmt::Let, got {:?}", body.stmts[0].0);
    };
    let value = value.as_ref().expect("expected let initialiser");
    let Expr::Match { arms, .. } = &value.0 else {
        panic!("expected Expr::Match, got {:?}", value.0);
    };
    let Expr::Block(Block { stmts, .. }) = &arms[0].body.0 else {
        panic!("expected Expr::Block, got {:?}", arms[0].body.0);
    };
    let Stmt::Break { label, value } = &stmts[0].0 else {
        panic!("expected Stmt::Break, got {:?}", stmts[0].0);
    };
    assert!(label.is_none());
    let Some(value) = value else {
        panic!("expected Stmt::Break value, got None");
    };
    assert!(
        matches!(value.0, Expr::Literal(_)),
        "expected an integer literal operand, got {:?}",
        value.0
    );
}

#[test]
fn labelled_break_in_match_arm_parses() {
    let body = parse_fn_body("@outer: loop { let x = match 1 { 1 => break @outer, _ => 0 }; }");
    let Stmt::Loop { body, .. } = &body.stmts[0].0 else {
        panic!("expected Stmt::Loop, got {:?}", body.stmts[0].0);
    };
    let Stmt::Let { value, .. } = &body.stmts[0].0 else {
        panic!("expected Stmt::Let, got {:?}", body.stmts[0].0);
    };
    let value = value.as_ref().expect("expected let initialiser");
    let Expr::Match { arms, .. } = &value.0 else {
        panic!("expected Expr::Match, got {:?}", value.0);
    };
    let Expr::Block(Block { stmts, .. }) = &arms[0].body.0 else {
        panic!("expected Expr::Block, got {:?}", arms[0].body.0);
    };
    assert!(matches!(
        &stmts[0].0,
        Stmt::Break { label: Some(l), value: None } if l == "outer"
    ));
}

#[test]
fn continue_in_match_arm_parses() {
    let body = parse_fn_body("while true { let x = match 1 { 1 => continue, _ => 0 }; }");
    let Stmt::While { body, .. } = &body.stmts[0].0 else {
        panic!("expected Stmt::While, got {:?}", body.stmts[0].0);
    };
    let Stmt::Let { value, .. } = &body.stmts[0].0 else {
        panic!("expected Stmt::Let, got {:?}", body.stmts[0].0);
    };
    let value = value.as_ref().expect("expected let initialiser");
    let Expr::Match { arms, .. } = &value.0 else {
        panic!("expected Expr::Match, got {:?}", value.0);
    };
    let Expr::Block(Block { stmts, .. }) = &arms[0].body.0 else {
        panic!("expected Expr::Block, got {:?}", arms[0].body.0);
    };
    assert!(matches!(&stmts[0].0, Stmt::Continue { label: None }));
}

#[test]
fn break_in_else_block_tail_parses() {
    // The historical repro (verified evidence table): before this slice,
    // this cascaded into 4 parse errors because statement-position `break`
    // unconditionally demanded a trailing `;`. Mirrors `Token::Return`'s
    // existing tail-of-block semicolon skip.
    parse_ok("fn f() { while true { if true { } else { break } } }");
}

#[test]
fn break_with_semicolon_in_else_block_tail_still_parses() {
    // The semicolon form already worked before this slice; keep it covered.
    parse_ok("fn f() { while true { if true { } else { break; } } }");
}

#[test]
fn statement_position_match_arm_break_still_parses() {
    // `None => break,` as a full statement (not a `let` initialiser) — the
    // acceptance bar's "statement-position match" case.
    parse_ok("fn f() { while true { match 1 { 1 => break, _ => () } } }");
}

// ── Structural equivalence: the desugar produces the pre-existing tree ──────

#[test]
fn break_expr_desugar_matches_explicit_block_form() {
    let sugared = parse("fn f() { while true { let x = match 1 { 1 => break, _ => 0 }; } }");
    let explicit = parse("fn f() { while true { let x = match 1 { 1 => { break; }, _ => 0 }; } }");
    assert!(sugared.errors.is_empty(), "{:#?}", sugared.errors);
    assert!(explicit.errors.is_empty(), "{:#?}", explicit.errors);
    assert!(
        hew_parser::ast_eq::program_eq_ignoring_spans(&sugared.program, &explicit.program),
        "expected `break` in expression position to desugar to the identical \
         tree `{{ break; }}` already produces"
    );
}

#[test]
fn continue_expr_desugar_matches_explicit_block_form() {
    let sugared = parse("fn f() { while true { let x = match 1 { 1 => continue, _ => 0 }; } }");
    let explicit =
        parse("fn f() { while true { let x = match 1 { 1 => { continue; }, _ => 0 }; } }");
    assert!(sugared.errors.is_empty(), "{:#?}", sugared.errors);
    assert!(explicit.errors.is_empty(), "{:#?}", explicit.errors);
    assert!(
        hew_parser::ast_eq::program_eq_ignoring_spans(&sugared.program, &explicit.program),
        "expected `continue` in expression position to desugar to the identical \
         tree `{{ continue; }}` already produces"
    );
}

// ── Reject: the trailing-comma hole (stage 2) ───────────────────────────────

#[test]
fn comma_less_break_arm_followed_by_another_arm_still_errors() {
    // A desugared `break` IS an `Expr::Block`; naively reusing `is_block_expr`
    // on the resulting AST would make the trailing comma optional here too,
    // silently swallowing the next arm's pattern as part of `break`'s value.
    parse_err("fn f() { while true { match 1 { 1 => break 2 => 3, _ => 0 } } }");
}
