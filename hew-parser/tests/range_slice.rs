//! Parser tests for C-3 `Vec<T>` range-slice forms in index position.
//!
//! The parser must admit all five forms inside `[...]` brackets and emit
//! `Expr::Range` (with the inclusive flag) so the checker/HIR can route
//! to range-slice lowering rather than single-element indexing.
//!
//! Forms:
//!   xs[a..b]    — closed start, exclusive end
//!   xs[a..=b]   — closed start, inclusive end
//!   xs[..b]     — open start, exclusive end
//!   xs[a..]     — closed start, open end
//!   xs[..]      — both endpoints open
//!
//! Outside index position, ranges still parse as `Expr::Binary { op:
//! Range/RangeInclusive }` per the existing Pratt loop — no behaviour
//! change to that surface.

use hew_parser::ast::Expr;

/// Parse `source` and return the first function's trailing-expression.
fn tail_of(source: &str) -> Expr {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let item = parsed
        .program
        .items
        .iter()
        .find_map(|spanned_item| match &spanned_item.0 {
            hew_parser::ast::Item::Function(f) => Some(f),
            _ => None,
        })
        .expect("expected a function item");
    let trailing = item
        .body
        .trailing_expr
        .as_ref()
        .expect("function body must have a trailing expression");
    trailing.0.clone()
}

#[track_caller]
fn assert_index_with_range(
    expr: &Expr,
    expect_start: bool,
    expect_end: bool,
    expect_inclusive: bool,
) {
    let Expr::Index { index, .. } = expr else {
        panic!("expected Expr::Index, got {expr:?}");
    };
    let Expr::Range {
        start,
        end,
        inclusive,
    } = &index.0
    else {
        panic!(
            "expected Expr::Index.index = Expr::Range, got {:?}",
            index.0
        );
    };
    assert_eq!(
        start.is_some(),
        expect_start,
        "start.is_some() expected {expect_start}; got {start:?}"
    );
    assert_eq!(
        end.is_some(),
        expect_end,
        "end.is_some() expected {expect_end}; got {end:?}"
    );
    assert_eq!(
        *inclusive, expect_inclusive,
        "inclusive flag expected {expect_inclusive}; got {inclusive}",
    );
}

#[test]
fn closed_range_xs_a_b_parses_as_range_in_brackets() {
    // `xs[a..b]` must emit `Expr::Index { index: Expr::Range { Some, Some,
    // inclusive: false } }` — not `Expr::Index { index: Expr::Binary { op:
    // Range, .. } }`. The parser rewrites the Pratt-loop's `Binary(Range)`
    // into `Range` inside brackets so downstream stages distinguish single-
    // element indexing (`xs[i]`) from range-slice (`xs[a..b]`).
    let expr = tail_of("fn f(xs: Vec<i64>, a: i64, b: i64) -> Vec<i64> { xs[a..b] }");
    assert_index_with_range(&expr, true, true, false);
}

#[test]
fn closed_inclusive_range_xs_a_eq_b_parses_with_inclusive_flag() {
    // `xs[a..=b]` carries `inclusive: true` so MIR can emit the +1
    // overflow-trap bump on the upper endpoint.
    let expr = tail_of("fn f(xs: Vec<i64>, a: i64, b: i64) -> Vec<i64> { xs[a..=b] }");
    assert_index_with_range(&expr, true, true, true);
}

#[test]
fn open_start_xs_dot_dot_b_parses_with_none_start() {
    // `xs[..b]` — open start, closed exclusive end.
    let expr = tail_of("fn f(xs: Vec<i64>, b: i64) -> Vec<i64> { xs[..b] }");
    assert_index_with_range(&expr, false, true, false);
}

#[test]
fn open_start_inclusive_xs_dot_dot_eq_b_parses_with_inclusive_flag() {
    // `xs[..=b]` — open start, inclusive end. Less common but admissible.
    let expr = tail_of("fn f(xs: Vec<i64>, b: i64) -> Vec<i64> { xs[..=b] }");
    assert_index_with_range(&expr, false, true, true);
}

#[test]
fn open_end_xs_a_dot_dot_parses_with_none_end() {
    // `xs[a..]` — closed start, open end. The parser detects the
    // `..]` trailer pattern after parsing the start expression.
    let expr = tail_of("fn f(xs: Vec<i64>, a: i64) -> Vec<i64> { xs[a..] }");
    assert_index_with_range(&expr, true, false, false);
}

#[test]
fn fully_open_xs_dot_dot_parses_with_both_none() {
    // `xs[..]` — both endpoints open. MIR fills `start := 0`, `end :=
    // len(xs)`, producing a full-copy slice.
    let expr = tail_of("fn f(xs: Vec<i64>) -> Vec<i64> { xs[..] }");
    assert_index_with_range(&expr, false, false, false);
}

#[test]
fn single_element_index_unchanged() {
    // Regression guard: `xs[i]` must still parse as `Expr::Index { index:
    // BindingRef { name: "i" } }` (no Range wrapping). The C-3 parser
    // rewrite only activates when the bracket contents are a range.
    let expr = tail_of("fn f(xs: Vec<i64>, i: i64) -> i64 { xs[i] }");
    let Expr::Index { index, .. } = &expr else {
        panic!("expected Expr::Index, got {expr:?}");
    };
    assert!(
        !matches!(index.0, Expr::Range { .. }),
        "xs[i] must NOT wrap the index in Expr::Range; got {:?}",
        index.0
    );
}
