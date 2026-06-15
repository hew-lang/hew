//! Explicit-return Ok-coercion: a `Result<Ok, Err>`-returning function whose
//! tail expression yields the `Ok` payload type is auto-wrapped in `Ok(..)`.
//!
//! The rule is type-directed and strictly tail-only:
//!
//! - tail unifies with the full `Result<Ok, Err>` → NO coercion (return it),
//! - else tail unifies with the `Ok` payload → coerce to `Ok(tail)`,
//! - else → the existing type error.
//!
//! These cases are mutually exclusive for finite types, so the coercion is
//! unambiguous and never double-wraps.

mod common;

use common::typecheck;
use hew_types::error::TypeErrorKind;

fn mismatch_count(output: &hew_types::TypeCheckOutput) -> usize {
    output
        .errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. }))
        .count()
}

#[test]
fn tail_question_typed_as_ok_payload_is_ok_wrapped() {
    // `helper(x)?` is typed `i64` (the `?` already unwrapped the Ok), and the
    // declared return is `Result<i64, E>` → the tail is Ok-wrapped.
    let src = r"
        enum E { Bad; }
        fn helper(x: i64) -> Result<i64, E> { Ok(x) }
        fn load(x: i64) -> Result<i64, E> { helper(x)? }
    ";
    let output = typecheck(src);
    assert!(
        output.errors.is_empty(),
        "tail `?` typed as Ok payload should type-check, got: {:#?}",
        output.errors
    );
    assert_eq!(
        output.tail_ok_coercions.len(),
        1,
        "exactly one tail Ok-coercion should be recorded"
    );
}

#[test]
fn bare_tail_returning_full_result_is_not_double_wrapped() {
    // The tail `helper(x)` already produces `Result<i64, E>` — the FULL return
    // type. It must be returned directly: no coercion, no double-wrap into
    // `Result<Result<i64, E>, E>`.
    let src = r"
        enum E { Bad; }
        fn helper(x: i64) -> Result<i64, E> { Ok(x) }
        fn passthrough(x: i64) -> Result<i64, E> { helper(x) }
    ";
    let output = typecheck(src);
    assert!(
        output.errors.is_empty(),
        "bare passthrough should type-check, got: {:#?}",
        output.errors
    );
    assert!(
        output.tail_ok_coercions.is_empty(),
        "a tail already producing the full Result must NOT be coerced"
    );
}

#[test]
fn explicit_ok_wrap_still_works_without_coercion() {
    // The explicit `Ok(helper(x)?)` form already produces the full Result, so
    // the coercion declines (no double-wrap) and there is no regression.
    let src = r"
        enum E { Bad; }
        fn helper(x: i64) -> Result<i64, E> { Ok(x) }
        fn explicit(x: i64) -> Result<i64, E> { Ok(helper(x)?) }
    ";
    let output = typecheck(src);
    assert!(
        output.errors.is_empty(),
        "explicit Ok(...) form should type-check, got: {:#?}",
        output.errors
    );
    assert!(
        output.tail_ok_coercions.is_empty(),
        "explicit Ok(...) tail already yields Result; no coercion expected"
    );
}

#[test]
fn if_tail_ok_wraps_both_arms() {
    // Both arms of a tail `if` flow to the function return → both Ok-wrap.
    let src = r"
        enum E { Bad; }
        fn helper(x: i64) -> Result<i64, E> { Ok(x) }
        fn branchy(x: i64) -> Result<i64, E> {
            if x > 0 { helper(x)? } else { helper(0 - x)? }
        }
    ";
    let output = typecheck(src);
    assert!(
        output.errors.is_empty(),
        "if-tail should type-check, got: {:#?}",
        output.errors
    );
    assert_eq!(
        output.tail_ok_coercions.len(),
        2,
        "both arms of a tail `if` should be Ok-coerced"
    );
}

#[test]
fn match_tail_ok_wraps_both_arms() {
    // Both arms of a tail `match` flow to the function return → both Ok-wrap.
    let src = r"
        enum E { Bad; }
        fn helper(x: i64) -> Result<i64, E> { Ok(x) }
        fn matchy(x: i64) -> Result<i64, E> {
            match x {
                0 => helper(10)?,
                _ => helper(x)?,
            }
        }
    ";
    let output = typecheck(src);
    assert!(
        output.errors.is_empty(),
        "match-tail should type-check, got: {:#?}",
        output.errors
    );
    assert_eq!(
        output.tail_ok_coercions.len(),
        2,
        "both arms of a tail `match` should be Ok-coerced"
    );
}

#[test]
fn plain_value_tail_typed_as_ok_payload_is_ok_wrapped() {
    // A plain-value tail (no `?`) whose type is the Ok payload also coerces.
    let src = r"
        enum E { Bad; }
        fn make(x: i64) -> Result<i64, E> { x + 1 }
    ";
    let output = typecheck(src);
    assert!(
        output.errors.is_empty(),
        "plain-value Ok-payload tail should type-check, got: {:#?}",
        output.errors
    );
    assert_eq!(output.tail_ok_coercions.len(), 1);
}

#[test]
fn nested_result_tail_wraps_against_ok_payload_only() {
    // `g()?` is typed `Result<i64, B>` — the Ok payload of the declared return
    // `Result<Result<i64, B>, E>`. It must wrap exactly once to
    // `Ok(Result<i64, B>)`, NOT recurse against the full return.
    let src = r"
        enum B { Bv; }
        enum E { Ev; }
        fn g() -> Result<Result<i64, B>, E> { Ok(Ok(1)) }
        fn nested() -> Result<Result<i64, B>, E> { g()? }
    ";
    let output = typecheck(src);
    assert!(
        output.errors.is_empty(),
        "nested-Result tail should type-check, got: {:#?}",
        output.errors
    );
    assert_eq!(
        output.tail_ok_coercions.len(),
        1,
        "nested-Result tail wraps exactly once against the Ok payload"
    );
}

#[test]
fn wrong_typed_tail_in_result_fn_still_errors() {
    // A tail whose type unifies with neither the full Result nor the Ok payload
    // must still produce a type-mismatch error — the coercion declines.
    let src = r#"
        enum E { Bad; }
        fn wrong() -> Result<i64, E> { "not an int" }
    "#;
    let output = typecheck(src);
    assert_eq!(
        mismatch_count(&output),
        1,
        "a genuinely wrong tail type must still error, got: {:#?}",
        output.errors
    );
    assert!(
        output.tail_ok_coercions.is_empty(),
        "no coercion should be recorded for a wrong-typed tail"
    );
}

#[test]
fn non_result_fn_tail_is_unaffected() {
    // A non-Result return never arms the coercion; a wrong tail still errors.
    let src = r#"
        fn plain() -> i64 { "nope" }
    "#;
    let output = typecheck(src);
    assert_eq!(
        mismatch_count(&output),
        1,
        "a non-Result fn with a wrong tail must still error, got: {:#?}",
        output.errors
    );
    assert!(output.tail_ok_coercions.is_empty());
}

#[test]
fn bare_identifier_tail_typed_as_ok_payload_is_ok_wrapped() {
    // The most common tail shape: a bare identifier whose type is the Ok
    // payload. `fn f(x: i64) -> Result<i64, E> { x }` must Ok-wrap `x`. This
    // tail is an `Expr::Identifier`, handled by the identifier arm of
    // `check_against` (not the default arm), so it exercises the arm-level
    // coercion fix.
    let src = r"
        enum E { Bad; }
        fn f(x: i64) -> Result<i64, E> { x }
    ";
    let output = typecheck(src);
    assert!(
        output.errors.is_empty(),
        "bare identifier Ok-payload tail should type-check, got: {:#?}",
        output.errors
    );
    assert_eq!(
        output.tail_ok_coercions.len(),
        1,
        "a bare identifier tail typed as the Ok payload should Ok-wrap"
    );
}

#[test]
fn generic_identifier_tail_is_ok_wrapped() {
    // The generic form `fn g<T>(x: T) -> Result<T, E> { x }`: the tail
    // identifier `x` has type `T`, the Ok payload of the declared return, so it
    // Ok-wraps.
    let src = r"
        enum E { Bad; }
        fn g<T>(x: T) -> Result<T, E> { x }
    ";
    let output = typecheck(src);
    assert!(
        output.errors.is_empty(),
        "generic identifier tail should type-check, got: {:#?}",
        output.errors
    );
    assert_eq!(
        output.tail_ok_coercions.len(),
        1,
        "a generic identifier tail typed as the Ok payload should Ok-wrap"
    );
}

#[test]
fn bare_call_tail_typed_as_ok_payload_is_ok_wrapped() {
    // A bare call whose return type is the Ok payload:
    // `fn f() -> Result<i64, E> { value() }` where `value(): i64`. This tail is
    // an `Expr::Call`, handled by the call arm of `check_against` (not the
    // default arm), so it exercises the call-arm coercion fix.
    let src = r"
        enum E { Bad; }
        fn value() -> i64 { 23 }
        fn f() -> Result<i64, E> { value() }
    ";
    let output = typecheck(src);
    assert!(
        output.errors.is_empty(),
        "bare call Ok-payload tail should type-check, got: {:#?}",
        output.errors
    );
    assert_eq!(
        output.tail_ok_coercions.len(),
        1,
        "a bare call tail typed as the Ok payload should Ok-wrap"
    );
}

#[test]
fn bare_call_tail_returning_full_result_is_not_double_wrapped() {
    // A bare call already producing the FULL Result must return directly — the
    // call arm's probe takes the no-coercion path, no double-wrap.
    let src = r"
        enum E { Bad; }
        fn g() -> Result<i64, E> { Ok(5) }
        fn passthrough() -> Result<i64, E> { g() }
    ";
    let output = typecheck(src);
    assert!(
        output.errors.is_empty(),
        "bare call passthrough should type-check, got: {:#?}",
        output.errors
    );
    assert!(
        output.tail_ok_coercions.is_empty(),
        "a bare call tail already producing the full Result must NOT coerce"
    );
}

#[test]
fn non_tail_call_argument_does_not_coerce() {
    // Over-coercion guard for the call arm: a call of the Ok-payload type in a
    // NON-tail argument position (where a Result is expected) must NOT Ok-wrap —
    // it must still error. The coercion is strictly tail-only.
    let src = r"
        enum E { Bad; }
        fn value() -> i64 { 23 }
        fn takes_result(r: Result<i64, E>) -> i64 {
            match r { Ok(v) => v, Err(_) => 0 }
        }
        fn caller() -> i64 { takes_result(value()) }
    ";
    let output = typecheck(src);
    assert_eq!(
        mismatch_count(&output),
        1,
        "a non-tail call argument must NOT be Ok-coerced, got: {:#?}",
        output.errors
    );
    assert!(
        output.tail_ok_coercions.is_empty(),
        "no coercion should fire for a call in a non-tail argument position"
    );
}

#[test]
fn non_tail_identifier_argument_does_not_coerce() {
    // Over-coercion guard for the identifier arm: an identifier of the
    // Ok-payload type in a NON-tail argument position (where a Result is
    // expected) must NOT Ok-wrap — it must still error.
    let src = r"
        enum E { Bad; }
        fn takes_result(r: Result<i64, E>) -> i64 {
            match r { Ok(v) => v, Err(_) => 0 }
        }
        fn caller(x: i64) -> i64 { takes_result(x) }
    ";
    let output = typecheck(src);
    assert_eq!(
        mismatch_count(&output),
        1,
        "a non-tail identifier argument must NOT be Ok-coerced, got: {:#?}",
        output.errors
    );
    assert!(
        output.tail_ok_coercions.is_empty(),
        "no coercion should fire for an identifier in a non-tail argument position"
    );
}

#[test]
fn non_tail_let_binding_does_not_coerce() {
    // The coercion is tail-ONLY: a `let r: Result<i64, E> = <i64>` binding must
    // NOT auto-wrap — it must still error. This is the over-coercion guard.
    let src = r"
        enum E { Bad; }
        fn f() -> i64 {
            let r: Result<i64, E> = 42;
            match r { Ok(v) => v, Err(_) => 0 }
        }
    ";
    let output = typecheck(src);
    assert_eq!(
        mismatch_count(&output),
        1,
        "a non-tail Result binding must NOT be Ok-coerced, got: {:#?}",
        output.errors
    );
    assert!(
        output.tail_ok_coercions.is_empty(),
        "no coercion should fire in a non-tail let-binding position"
    );
}
