//! Tests for `\u{...}` Unicode brace escape sequences in string literals.

use hew_parser::ast::{Expr, Literal};
use hew_parser::parse;

fn parse_str_expr(src: &str) -> String {
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    let item = &result.program.items[0].0;
    match item {
        hew_parser::ast::Item::Function(f) => {
            let stmt = &f.body.stmts[0].0;
            match stmt {
                hew_parser::ast::Stmt::Let {
                    value: Some(val), ..
                } => match &val.0 {
                    Expr::Literal(Literal::String(s)) => s.clone(),
                    other => panic!("expected string literal, got {other:?}"),
                },
                other => panic!("expected let statement, got {other:?}"),
            }
        }
        other => panic!("expected function item, got {other:?}"),
    }
}

fn parse_errors(src: &str) -> Vec<String> {
    let result = parse(src);
    result.errors.iter().map(|e| e.message.clone()).collect()
}

// --- Decoding tests ---

#[test]
fn unicode_brace_basic_latin_small_e_acute() {
    // \u{00E9} → é (U+00E9)
    let s = parse_str_expr(r#"fn main() { let s = "\u{00E9}"; }"#);
    assert_eq!(s, "é");
}

#[test]
fn unicode_brace_astral_grinning_face() {
    // \u{1F600} → 😀 (U+1F600)
    let s = parse_str_expr(r#"fn main() { let s = "\u{1F600}"; }"#);
    assert_eq!(s, "😀");
}

#[test]
fn unicode_brace_boundary_max_codepoint() {
    // \u{10FFFF} → last valid Unicode scalar
    let s = parse_str_expr(r#"fn main() { let s = "\u{10FFFF}"; }"#);
    assert_eq!(s, "\u{10FFFF}");
}

#[test]
fn unicode_brace_case_insensitive() {
    // \u{1f600} (lowercase hex) must decode the same as \u{1F600}
    let s = parse_str_expr(r#"fn main() { let s = "\u{1f600}"; }"#);
    assert_eq!(s, "😀");
}

#[test]
fn unicode_brace_single_digit() {
    // Minimum: single hex digit
    let s = parse_str_expr(r#"fn main() { let s = "\u{41}"; }"#);
    assert_eq!(s, "A");
}

// --- Fail-closed tests ---

#[test]
fn unicode_brace_empty_is_error() {
    let errs = parse_errors(r#"fn main() { let s = "\u{}"; }"#);
    assert!(
        !errs.is_empty(),
        "expected a diagnostic for empty \\u{{}} but got none"
    );
}

#[test]
fn unicode_brace_non_hex_is_error() {
    let errs = parse_errors(r#"fn main() { let s = "\u{XYZ}"; }"#);
    assert!(
        !errs.is_empty(),
        "expected a diagnostic for non-hex digits in \\u{{...}} but got none"
    );
}

#[test]
fn unicode_brace_surrogate_is_error() {
    let errs = parse_errors(r#"fn main() { let s = "\u{D800}"; }"#);
    assert!(
        !errs.is_empty(),
        "expected a diagnostic for surrogate codepoint \\u{{D800}} but got none"
    );
}

#[test]
fn unicode_brace_out_of_range_is_error() {
    let errs = parse_errors(r#"fn main() { let s = "\u{110000}"; }"#);
    assert!(
        !errs.is_empty(),
        "expected a diagnostic for out-of-range \\u{{110000}} but got none"
    );
}

#[test]
fn unicode_brace_missing_close_is_error() {
    // \u{41 with no closing }
    let errs = parse_errors("fn main() { let s = \"\\u{41\"; }");
    assert!(
        !errs.is_empty(),
        "expected a diagnostic for missing `}}` in \\u{{...}} but got none"
    );
}
