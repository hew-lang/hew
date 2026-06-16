//! Tests for f-string interpolation span correctness and `{{` diagnostics.
//!
//! **Item A** — the sub-parser that parses an interpolation expression is built
//! with a byte offset equal to the expression's position in the original source,
//! so every span in the returned AST nodes (including deeply nested sub-nodes)
//! and every error emitted during that parse refers to the original source,
//! not to the local 0-based text of the extracted expression.
//!
//! **Item B** — when an interpolation is opened with `{{` (Python-style literal
//! brace escaping), a clear diagnostic is emitted pointing at the `{{` and
//! guiding the user toward the correct `\{` syntax.  No cascade errors follow.

use hew_parser::ast::{BinaryOp, Expr, Item, Stmt, StringPart};
use hew_parser::parse;

// ── helpers ──────────────────────────────────────────────────────────────────

/// Return the `Vec<StringPart>` from the first `let` binding in `fn main`.
fn extract_interp_parts(src: &str) -> Vec<StringPart> {
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors: {:?}",
        result.errors
    );
    match &result.program.items[0].0 {
        Item::Function(f) => match &f.body.stmts[0].0 {
            Stmt::Let {
                value: Some(val), ..
            } => match &val.0 {
                Expr::InterpolatedString(parts) => parts.clone(),
                other => panic!("expected InterpolatedString, got {other:?}"),
            },
            other => panic!("expected Let stmt, got {other:?}"),
        },
        other => panic!("expected Function item, got {other:?}"),
    }
}

// ── Item A: sub-expression span correctness ──────────────────────────────────

/// `f"{hello}"` — the identifier span must point at `hello` in the original
/// source, not at offset 0 of the extracted sub-text.
///
/// Source:
/// ```text
/// fn main() { let s = f"{hello}"; }
/// 0         1         2         3
/// 0123456789012345678901234567890123
/// ```
/// f-string prefix `f"` starts at byte 20, opening `{` at byte 22, `h` at 23.
#[test]
fn fstring_span_simple_identifier() {
    let src = r#"fn main() { let s = f"{hello}"; }"#;
    // Verify assumed layout.
    assert_eq!(&src[20..22], "f\"", "layout check: f-string starts at 20");
    assert_eq!(&src[23..28], "hello", "layout check: 'hello' at 23..28");

    let parts = extract_interp_parts(src);
    assert_eq!(parts.len(), 1);
    match &parts[0] {
        StringPart::Expr((_, span)) => {
            assert_eq!(
                span.start, 23,
                "identifier span should start at 'h' (byte 23), got {}",
                span.start
            );
            assert_eq!(
                span.end, 28,
                "identifier span should end after 'hello' (byte 28), got {}",
                span.end
            );
        }
        other @ StringPart::Literal(_) => panic!("expected Expr part, got {other:?}"),
    }
}

/// `f"{a + b}"` — for a binary expression the outer span must be correct AND
/// the inner left/right node spans must be independently correct.
///
/// Source:
/// ```text
/// fn main() { let s = f"{a + b}"; }
/// 0         1         2         3
/// 0123456789012345678901234567890123
/// ```
/// `a` at byte 23, `+` at byte 25, `b` at byte 27.
#[test]
fn fstring_span_binary_expression_inner_nodes() {
    let src = r#"fn main() { let s = f"{a + b}"; }"#;
    assert_eq!(&src[23..24], "a", "layout check: 'a' at 23");
    assert_eq!(&src[25..26], "+", "layout check: '+' at 25");
    assert_eq!(&src[27..28], "b", "layout check: 'b' at 27");

    let parts = extract_interp_parts(src);
    assert_eq!(parts.len(), 1);
    match &parts[0] {
        StringPart::Expr((expr, outer_span)) => {
            // Outer span covers the whole `a + b` expression.
            assert_eq!(
                outer_span.start, 23,
                "outer span start should be at 'a' (23), got {}",
                outer_span.start
            );
            assert_eq!(
                outer_span.end, 28,
                "outer span end should be after 'b' (28), got {}",
                outer_span.end
            );

            // Inner node spans must also point into the original source.
            match expr {
                Expr::Binary { op, left, right } => {
                    assert_eq!(*op, BinaryOp::Add);
                    assert_eq!(
                        left.1.start, 23,
                        "'a' left-node span start should be 23, got {}",
                        left.1.start
                    );
                    // end = start of next token (+), which is 25 (parser convention:
                    // span.end == next-token.start, not the character's own end byte)
                    assert_eq!(
                        left.1.end, 25,
                        "'a' left-node span end should be 25 (start of '+'), got {}",
                        left.1.end
                    );
                    assert_eq!(
                        right.1.start, 27,
                        "'b' right-node span start should be 27, got {}",
                        right.1.start
                    );
                    assert_eq!(
                        right.1.end, 28,
                        "'b' right-node span end should be 28, got {}",
                        right.1.end
                    );
                }
                other => panic!("expected BinaryOp, got {other:?}"),
            }
        }
        other @ StringPart::Literal(_) => panic!("expected Expr part, got {other:?}"),
    }
}

/// Multiple interpolations in one f-string — each expression's span must be
/// independently and correctly positioned.
///
/// Source:
/// ```text
/// fn main() { let s = f"{x} and {y}"; }
/// 0         1         2         3
/// 01234567890123456789012345678901234567
/// ```
/// `x` at byte 23, `y` at byte 33.
#[test]
fn fstring_span_multiple_interpolations() {
    let src = r#"fn main() { let s = f"{x} and {y}"; }"#;
    assert_eq!(&src[23..24], "x", "layout check: 'x' at 23");
    assert_eq!(&src[31..32], "y", "layout check: 'y' at 31");

    let parts = extract_interp_parts(src);
    // Parts: Expr(x), Literal(" and "), Expr(y)
    assert_eq!(
        parts.len(),
        3,
        "expected [Expr, Literal, Expr], got {parts:?}"
    );

    match &parts[0] {
        StringPart::Expr((_, span)) => {
            assert_eq!(span.start, 23, "first expr start (x)");
            assert_eq!(span.end, 24, "first expr end (x)");
        }
        other @ StringPart::Literal(_) => panic!("expected first Expr, got {other:?}"),
    }
    match &parts[1] {
        StringPart::Literal(s) => assert_eq!(s, " and "),
        other @ StringPart::Expr(_) => panic!("expected Literal, got {other:?}"),
    }
    match &parts[2] {
        StringPart::Expr((_, span)) => {
            assert_eq!(span.start, 31, "second expr start (y)");
            assert_eq!(span.end, 32, "second expr end (y)");
        }
        other @ StringPart::Literal(_) => panic!("expected second Expr, got {other:?}"),
    }
}

// ── Item B: `{{` diagnostic ──────────────────────────────────────────────────

/// `f"{{lit}}"` — must produce exactly one diagnostic mentioning `\{` or
/// literal braces.  No cascade errors are expected.
///
/// Source:
/// ```text
/// fn main() { let s = f"{{lit}}"; }
/// 0         1         2         3
/// 012345678901234567890123456789012
/// ```
/// f-string starts at 20, `{{` is at bytes 22..24.
#[test]
fn fstring_double_brace_emits_diagnostic() {
    let src = r#"fn main() { let s = f"{{lit}}"; }"#;
    assert_eq!(&src[22..24], "{{", "layout check: {{ at 22..24");

    let result = parse(src);
    assert!(
        !result.errors.is_empty(),
        "expected a diagnostic for `{{` but got none"
    );

    let diag = &result.errors[0];
    // Message must mention the correct escape syntax.
    assert!(
        diag.message.contains(r"\{") || diag.message.contains("literal brace"),
        "diagnostic message should mention `\\{{` or 'literal brace', got: {:?}",
        diag.message
    );
    // Span must point at the `{{` in the source.
    assert_eq!(
        diag.span.start, 22,
        "diagnostic span start should be at first `{{` (byte 22), got {}",
        diag.span.start
    );
    assert_eq!(
        diag.span.end, 24,
        "diagnostic span end should be after second `{{` (byte 24), got {}",
        diag.span.end
    );
}

/// The hint on a `{{` diagnostic must mention `\{`.
#[test]
fn fstring_double_brace_hint_mentions_backslash_brace() {
    let src = r#"fn main() { let s = f"{{x}}"; }"#;
    let result = parse(src);
    assert!(!result.errors.is_empty(), "expected a diagnostic for `{{`");

    let diag = &result.errors[0];
    let hint = diag
        .hint
        .as_deref()
        .expect("diagnostic should carry a hint");
    assert!(
        hint.contains(r"\{"),
        "hint should mention `\\{{`, got: {hint:?}"
    );
}

/// `{{` at the start of an f-string produces exactly one diagnostic — no
/// cascade errors from parsing the content as a block expression.
#[test]
fn fstring_double_brace_no_cascade_errors() {
    let src = r#"fn main() { let s = f"{{lit}}"; }"#;
    let result = parse(src);
    assert_eq!(
        result.errors.len(),
        1,
        "expected exactly one diagnostic for `{{`, got: {:?}",
        result.errors
    );
}

/// `{{` in the middle of an f-string — surrounding literal text is preserved
/// and no cascade errors are emitted.
///
/// Source: `f"prefix {{x}} suffix"`
/// `{{` is at bytes 27..29 in the full source.
#[test]
fn fstring_double_brace_midstring_diagnostic_span() {
    let src = r#"fn main() { let s = f"prefix {{x}} suffix"; }"#;
    // Locate `{{` in source.
    let double_open = src.find("{{").expect("source contains {{");

    let result = parse(src);
    assert_eq!(
        result.errors.len(),
        1,
        "expected one diagnostic, got: {:?}",
        result.errors
    );
    let diag = &result.errors[0];
    assert_eq!(
        diag.span.start, double_open,
        "span start should be at first `{{` (byte {double_open}), got {}",
        diag.span.start
    );
    assert_eq!(
        diag.span.end,
        double_open + 2,
        "span end should be after `{{` (byte {}), got {}",
        double_open + 2,
        diag.span.end
    );
}

/// `\{` is the correct Hew literal brace — must NOT produce any diagnostic.
#[test]
fn fstring_backslash_brace_is_valid() {
    // f"\{lit\}" — backslash-brace is the correct literal brace syntax in Hew.
    let src = "fn main() { let s = f\"\\{lit\\}\"; }";
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "\\{{...\\}} should be valid, got errors: {:?}",
        result.errors
    );
}

// ── regression: sub-parser error spans are absolute (#1931 headline) ─────────

/// A malformed interpolation expression (`f"{&x}"`) must produce a
/// sub-parser diagnostic whose span is **absolute** — pointing at the `&` in
/// the original source, not at offset 0 of the extracted expression text.
///
/// Source:
/// ```text
/// fn main() { let s = f"{&x}"; }
/// 0         1         2
/// 012345678901234567890123456789
/// ```
///
/// Layout:
/// - f-string token starts at byte 20 (`f"`), so `inner_offset` = 22.
/// - `{` opens at inner byte 0 (absolute 22); `expr_start_byte` = 1.
/// - `&` is at inner byte 1 → absolute byte **23**.
/// - `x` is at inner byte 2 → absolute byte **24**.
/// - `base` = `inner_offset` + `expr_start_byte` = 22 + 1 = **23**.
///
/// The sub-parser is built with `Parser::new_with_offset("&x", 23)`, so the
/// `&` token carries span `23..24` (not the local `0..1`). The
/// `` `&` is not a prefix operator `` diagnostic is emitted at `peek_span()`
/// before the `&` is consumed, so its span is `23..24` — absolute.
#[test]
fn fstring_subparser_error_span_is_absolute() {
    let src = r#"fn main() { let s = f"{&x}"; }"#;

    // Verify assumed layout.
    assert_eq!(&src[20..22], "f\"", "layout: f-string starts at 20");
    assert_eq!(&src[22..23], "{", "layout: opening brace at 22");
    assert_eq!(&src[23..24], "&", "layout: & at 23");
    assert_eq!(&src[24..25], "x", "layout: x at 24");

    let result = parse(src);

    // The parse emits at least one error (the `&` prefix diagnostic).
    assert!(
        !result.errors.is_empty(),
        "expected a diagnostic for `&x` interpolation, got none"
    );

    // Find the diagnostic about `&` not being a prefix operator.
    let amp_diag = result
        .errors
        .iter()
        .find(|e| e.message.contains('&'))
        .expect("expected a diagnostic mentioning `&`");

    // The span must be absolute: `&` is at byte 23 in the original source.
    assert_eq!(
        amp_diag.span.start, 23,
        "sub-parser error span.start should be 23 (absolute `&` position), \
         was {} — rebasing failed",
        amp_diag.span.start
    );
    assert_eq!(
        amp_diag.span.end, 24,
        "sub-parser error span.end should be 24, was {}",
        amp_diag.span.end
    );
}

// ── regression: eat_closing_angle updates last_token_end ─────────────────────

/// After `eat_closing_angle()` consumes a `>` in a turbofish
/// (`Foo::<T>`) inside an f-string interpolation, the EOF-based
/// `peek_span()` must return the position **after** the `>`, not before it.
///
/// Source:
/// ```text
/// fn main() { let s = f"{Foo::<T>}"; }
/// 0         1         2         3
/// 012345678901234567890123456789012345
/// ```
///
/// Layout:
/// - f-string token starts at byte 20; `inner_offset` = 22.
/// - `{` at inner byte 0 (absolute 22); `F` at inner byte 1; `expr_start_byte` = 1.
/// - `base` = 22 + 1 = 23.
/// - Sub-parser for `"Foo::<T>"` with offset 23:
///   - Foo: local 0..3 → absolute 23..26
///   - :: local 3..5 → absolute 26..28
///   - < local 5..6 → absolute 28..29
///   - T: local 6..7 → absolute 29..30
///   - > local 7..8 → absolute **30..31**
/// - `eat_closing_angle()` consumes `>` at span 30..31; with the fix it
///   sets `last_token_end = 31`.  After that there are no tokens, so
///   `peek_span()` returns `31..31`.
/// - `self.error("turbofish … must be followed by …")` is called with
///   that span → diagnostic at byte **31** (not 30).
#[test]
fn fstring_eat_closing_angle_updates_last_token_end() {
    let src = r#"fn main() { let s = f"{Foo::<T>}"; }"#;

    // Verify assumed layout.
    assert_eq!(&src[20..22], "f\"", "layout: f-string at 20");
    assert_eq!(&src[29..30], "T", "layout: T at 29");
    assert_eq!(&src[30..31], ">", "layout: > at 30");
    assert_eq!(&src[31..32], "}", "layout: closing interp-brace at 31");

    let result = parse(src);

    // The parse must emit a turbofish diagnostic.
    let turbofish_diag = result
        .errors
        .iter()
        .find(|e| e.message.contains("turbofish"))
        .expect("expected a turbofish diagnostic after `Foo::<T>` without `(...)`");

    // peek_span() at EOF after eat_closing_angle consumed `>` (span 30..31)
    // must return 31..31, so the error is anchored just past the `>`.
    assert_eq!(
        turbofish_diag.span.start, 31,
        "turbofish error span.start should be 31 (byte after `>`), \
         was {} — eat_closing_angle did not update last_token_end",
        turbofish_diag.span.start
    );
    assert_eq!(
        turbofish_diag.span.end, 31,
        "turbofish error span.end should be 31 (zero-width at EOF), was {}",
        turbofish_diag.span.end
    );
}
