//! `return` as a first-class expression (v0.6 error-prop ergonomics).
//!
//! Before this slice, `return` in expression position errored with "return
//! statement in expression context". It now parses to `Expr::Return`, usable
//! anywhere an expression is expected (match arms, `let` initialisers, `&&`/`||`
//! operands). The operand has no trailing `;` in expression position; it ends
//! where the surrounding expression ends.

use hew_parser::ast::{Expr, MatchArm};
use hew_parser::parse;

/// Parse a source string wrapping an expression inside a function and return
/// the function body's trailing expression.
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

/// Parse a full source string and assert it produces no parse errors.
fn parse_ok(src: &str) {
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors for `{src}`: {:#?}",
        result.errors
    );
}

// ── Accept: `return` in expression position ─────────────────────────────────

#[test]
fn return_with_value_in_expr_position_parses() {
    // The dogfood repro: a bare match arm whose body is `return <expr>`.
    let expr = parse_expr_in_fn("match r { Ok(x) => x, Err(e) => return Err(e) }");
    let Expr::Match { arms, .. } = expr else {
        panic!("expected Expr::Match, got {expr:?}");
    };
    let MatchArm { body, .. } = &arms[1];
    assert!(
        matches!(body.0, Expr::Return(Some(_))),
        "expected match-arm body Expr::Return(Some(_)), got {:?}",
        body.0
    );
}

#[test]
fn bare_return_no_value_in_match_arm_parses() {
    // `return` with no operand, terminated by the arm `,` — operand is `None`.
    let expr = parse_expr_in_fn("match r { Some(_) => return, None => 0 }");
    let Expr::Match { arms, .. } = expr else {
        panic!("expected Expr::Match, got {expr:?}");
    };
    assert!(
        matches!(arms[0].body.0, Expr::Return(None)),
        "expected Expr::Return(None), got {:?}",
        arms[0].body.0
    );
}

#[test]
fn return_in_let_initialiser_parses() {
    // `let x = cond || return ...;` — return as the RHS of a `||` operand.
    parse_ok("fn f() -> int { let x = compute() || return 0; x }");
}

#[test]
fn return_with_call_operand_parses() {
    // The operand is a full expression (a call), not just an atom. Exercised in
    // genuine expression position (a match arm); the operand ends at the arm
    // boundary, not a `;`.
    let expr = parse_expr_in_fn("match r { _ => return wrap(err, \"io\") }");
    let Expr::Match { arms, .. } = expr else {
        panic!("expected Expr::Match, got {expr:?}");
    };
    let Expr::Return(Some(operand)) = &arms[0].body.0 else {
        panic!("expected Expr::Return(Some(_)), got {:?}", arms[0].body.0);
    };
    assert!(
        matches!(operand.0, Expr::Call { .. }),
        "expected the operand to be a call, got {:?}",
        operand.0
    );
}
