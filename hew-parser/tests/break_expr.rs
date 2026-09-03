//! `break` and `continue` as first-class expressions (v0.6 loop ergonomics).
//!
//! Before this slice, `break`/`continue` in expression position (a match-arm
//! body, an `else` block tail, ...) errored with "expected expression, found
//! `break`" even though the statement form `{ break; }` in the identical
//! position compiled. The parser now desugars the expression-position
//! spelling to the same one-statement-block AST the working `{ break; }`
//! form already produces, mirroring how `return` was made expression-capable.

use hew_parser::ast::{Block, Expr, Item, MatchArm, Stmt};
use hew_parser::{parse, ParseDiagnosticKind};

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
    // `.None => break,` as a full statement (not a `let` initialiser) — the
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

/// Parse a full source string and assert it reports a missing `,`, so a test
/// that means "the comma is mandatory here" cannot pass on some unrelated
/// syntax error.
fn parse_err_missing_comma(src: &str) {
    let result = parse(src);
    assert!(
        result.errors.iter().any(|e| matches!(
            &e.kind,
            ParseDiagnosticKind::UnexpectedToken { expected, .. } if expected == "`,`"
        )),
        "expected a missing-comma error for `{src}`, got: {:#?}",
        result.errors
    );
}

#[test]
fn comma_less_break_arm_followed_by_another_arm_still_errors() {
    // A desugared `break` IS an `Expr::Block`; naively reusing `is_block_expr`
    // on the resulting AST would make the trailing comma optional here too,
    // silently swallowing the next arm's pattern as part of `break`'s value.
    parse_err("fn f() { while true { match 1 { 1 => break 2 => 3, _ => 0 } } }");
}

#[test]
fn comma_less_desugared_arms_all_still_error() {
    // Every `break`/`continue` spelling desugars to a one-statement
    // `Expr::Block`. None of their opening tokens is in the block-opening set,
    // so all of them keep requiring the separator.
    parse_err_missing_comma("fn f() { while true { match 1 { 1 => break _ => 0 } } }");
    parse_err_missing_comma("fn f() { while true { match 1 { 1 => continue _ => 0 } } }");
    parse_err_missing_comma("fn f() { loop { match 1 { 1 => break 7 _ => 0 } } }");
    parse_err_missing_comma("fn f() { 'a: while true { match 1 { 1 => break 'a _ => 0 } } }");
    parse_err_missing_comma("fn f() { 'a: while true { match 1 { 1 => continue 'a _ => 0 } } }");
}

// ── Accept: every block-opening arm body keeps its optional comma ───────────

#[test]
fn comma_less_block_opening_arms_parse() {
    // One case per variant `Parser::is_block_expr` accepts. Written out by hand
    // rather than taken from the corpus: `fmt` emits `,` after every arm, so a
    // corpus-derived case cannot exercise the comma-less spelling at all.

    // Expr::Block
    parse_ok("fn f() { match 1 { 1 => { 1 } _ => 0 } }");
    // Expr::If
    parse_ok("fn f() { match 1 { 1 => if c { 1 } else { 2 } _ => 0 } }");
    // Expr::IfLet
    parse_ok("fn f() { match 1 { 1 => if let Some(v) = o { v } else { 2 } _ => 0 } }");
    // Expr::Match
    parse_ok("fn f() { match 1 { 1 => match x { _ => 1 } _ => 0 } }");
    // Expr::UnsafeBlock
    parse_ok("fn f() { match 1 { 1 => unsafe { 1 } _ => 0 } }");
    // Expr::Select
    parse_ok("fn f() { match 1 { 1 => select { m from ch => m, } _ => 0 } }");
    // Expr::Scope
    parse_ok("fn f() { match 1 { 1 => scope { 1 } _ => 0 } }");
    // Expr::ForkBlock — a brace must follow `fork`, and `fork`/`after` blocks
    // are only legal inside a `scope`.
    parse_ok("fn f() { scope { let v = match 1 { 1 => fork { 1 } _ => 0 }; } }");
    // Expr::ScopeDeadline
    parse_ok("fn f() { scope { let v = match 1 { 1 => after(1s) { 1 } _ => 0 }; } }");
}

#[test]
fn comma_less_non_block_arms_still_error() {
    // The set is the tokens that OPEN a block-bodied expression, not every
    // token that happens to be a `{` or a keyword: a map literal, a `gen`
    // block and a `fork` child binding are not `is_block_expr`, so they keep
    // requiring the comma exactly as before.
    parse_err_missing_comma("fn f() { match 1 { 1 => {\"a\": 1} _ => 0 } }");
    parse_err_missing_comma("fn f() { match 1 { 1 => gen { yield 1; } _ => 0 } }");
    parse_err_missing_comma("fn f() { scope { let v = match 1 { 1 => fork g() _ => 0 }; } }");
}
