//! Type-checker tests for C-3: `Vec<T>` range-slice `xs[a..b]` (plus four
//! open-end forms).
//!
//! The checker rule (in `synthesize_index`):
//! * `xs[a..b]` on `Vec<T>` → result type is `Vec<T>`.
//! * Each present endpoint must check against `i64`.
//! * Open endpoints contribute no constraint (lowering fills them).
//! * Non-Vec receivers are rejected with an `InvalidOperation` diagnostic
//!   pointing at range-slice syntax.

mod common;

use common::typecheck_isolated;
use hew_types::error::TypeErrorKind;

fn assert_clean(src: &str) {
    let output = typecheck_isolated(src);
    assert!(
        output.errors.is_empty(),
        "expected clean type-check, got: {:#?}",
        output.errors
    );
}

fn first_error_kind(src: &str) -> TypeErrorKind {
    let output = typecheck_isolated(src);
    output.errors.first().map_or_else(
        || panic!("expected a type error; got none. source:\n{src}"),
        |e| e.kind.clone(),
    )
}

#[test]
fn closed_slice_on_vec_typechecks_clean() {
    assert_clean("fn f(xs: Vec<i64>, a: i64, b: i64) -> Vec<i64> { xs[a..b] }");
}

#[test]
fn inclusive_slice_on_vec_typechecks_clean() {
    assert_clean("fn f(xs: Vec<i64>, a: i64, b: i64) -> Vec<i64> { xs[a..=b] }");
}

#[test]
fn open_start_slice_on_vec_typechecks_clean() {
    assert_clean("fn f(xs: Vec<i64>, b: i64) -> Vec<i64> { xs[..b] }");
}

#[test]
fn open_end_slice_on_vec_typechecks_clean() {
    assert_clean("fn f(xs: Vec<i64>, a: i64) -> Vec<i64> { xs[a..] }");
}

#[test]
fn fully_open_slice_on_vec_typechecks_clean() {
    assert_clean("fn f(xs: Vec<i64>) -> Vec<i64> { xs[..] }");
}

#[test]
fn slice_on_vec_string_typechecks_clean() {
    // Vec<String> range-slice is supported; the runtime strdups each
    // element into the fresh vec. Element-type dispatch is by the MIR
    // emitter.
    assert_clean("fn f(xs: Vec<String>, a: i64, b: i64) -> Vec<String> { xs[a..b] }");
}

#[test]
fn slice_on_int_rejected_as_invalid_operation() {
    // `42[a..b]` — receiver is a scalar, not a Vec. The checker rejects
    // with `InvalidOperation` and the diagnostic must name range-slice
    // syntax, not single-element indexing.
    let kind = first_error_kind("fn f() -> i64 { let x: i64 = 42; x[0..1] }");
    assert!(
        matches!(kind, TypeErrorKind::InvalidOperation),
        "non-Vec range-slice must surface InvalidOperation; got {kind:?}"
    );
}

#[test]
fn slice_on_string_rejected_as_invalid_operation() {
    // `String[a..b]` is not supported by this slice — the checker rejects.
    let kind = first_error_kind("fn f(s: String) -> String { s[0..1] }");
    assert!(
        matches!(kind, TypeErrorKind::InvalidOperation),
        "String range-slice must surface InvalidOperation; got {kind:?}"
    );
}

#[test]
fn non_i64_endpoint_typecheck_error() {
    // The start endpoint is a string literal; the checker must check it
    // against i64 and surface a type error.
    let output = typecheck_isolated("fn f(xs: Vec<i64>, b: i64) -> Vec<i64> { xs[\"oops\"..b] }");
    assert!(
        !output.errors.is_empty(),
        "string range endpoint must produce a type error"
    );
}
