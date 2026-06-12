//! Q004 / LESSONS row `diagnostic-trust` (P1):
//!
//! Pin the local-authority property that an `impl Trait for X` block's method
//! signatures must structurally match the trait's declared method signatures
//! after substituting `Self` for `X`, projecting `Self::Assoc` through the
//! impl's `type Assoc = ...` alias, and renaming method-level type params
//! positionally.
//!
//! Without this check, a wrong impl signature surfaces one stage downstream
//! as a confusing "type does not satisfy trait" at a call site (typically a
//! `for x in ...` desugar) instead of locally where the user wrote the
//! divergent signature. These tests lock the reverse: the diagnostic fires at
//! the impl method's span, citing the divergence kind (arity / parameter /
//! return-type / receiver shape).

#![allow(
    clippy::missing_panics_doc,
    reason = "integration-test panics on assertion failure are intentional"
)]

mod common;

use common::typecheck_isolated;
use hew_types::error::TypeErrorKind;

/// Trait prelude shared by the negative tests. Mirrors `std/builtins.hew`'s
/// `Iterator` shape so the lockedness is anchored to the real surface.
const ITER_TRAIT_PRELUDE: &str = r"
pub trait Iterator {
    type Item;

    fn next(self) -> Option<Self::Item>;
}
";

fn has_trait_impl_sig_mismatch(output: &hew_types::TypeCheckOutput, detail: &str) -> bool {
    output
        .errors
        .iter()
        .any(|e| matches!(&e.kind, TypeErrorKind::TraitImplSignatureMismatch { detail: d, .. } if *d == detail))
}

#[test]
fn impl_with_correct_signature_typechecks_clean() {
    let src = format!(
        "{ITER_TRAIT_PRELUDE}

pub type Counter {{
    n: i64;
    limit: i64;
}}

impl Iterator for Counter {{
    type Item = i64;
    fn next(it: Counter) -> Option<i64> {{
        if it.n >= it.limit {{ None }} else {{ Some(it.n) }}
    }}
}}
"
    );
    let output = typecheck_isolated(&src);
    assert!(
        output.errors.is_empty(),
        "correct signature must not emit TraitImplSignatureMismatch; got: {:#?}",
        output.errors
    );
}

#[test]
fn impl_with_wrong_return_type_rejected_at_impl_site() {
    let src = format!(
        "{ITER_TRAIT_PRELUDE}

pub type Counter {{
    n: i64;
    limit: i64;
}}

impl Iterator for Counter {{
    type Item = i64;
    fn next(it: Counter) -> i64 {{
        it.n
    }}
}}
"
    );
    let output = typecheck_isolated(&src);
    assert!(
        has_trait_impl_sig_mismatch(&output, "return type"),
        "wrong return type must surface TraitImplSignatureMismatch with `return type` detail; \
         got: {:#?}",
        output.errors
    );
    let cited = output.errors.iter().any(|e| {
        e.message.contains("Counter::next")
            && e.message.contains("Iterator")
            && e.message.contains("i64")
    });
    assert!(
        cited,
        "diagnostic must name the offending impl method and trait; got: {:#?}",
        output.errors
    );
}

#[test]
fn impl_with_wrong_receiver_rejected_at_impl_site() {
    // Receiver type is wrong — `(it: i64)` instead of an impl-target-shaped
    // receiver. The first param fails `is_receiver_param` so the impl post-skip
    // arity is 1, vs trait post-skip arity 0, surfacing as an arity mismatch.
    let src = format!(
        "{ITER_TRAIT_PRELUDE}

pub type Counter {{
    n: i64;
    limit: i64;
}}

impl Iterator for Counter {{
    type Item = i64;
    fn next(it: i64) -> Option<i64> {{
        None
    }}
}}
"
    );
    let output = typecheck_isolated(&src);
    assert!(
        has_trait_impl_sig_mismatch(&output, "arity"),
        "wrong receiver shape must surface TraitImplSignatureMismatch with `arity` detail; \
         got: {:#?}",
        output.errors
    );
}

#[test]
fn impl_with_extra_parameter_rejected_at_impl_site() {
    let src = format!(
        "{ITER_TRAIT_PRELUDE}

pub type Counter {{
    n: i64;
    limit: i64;
}}

impl Iterator for Counter {{
    type Item = i64;
    fn next(it: Counter, extra: i64) -> Option<i64> {{
        None
    }}
}}
"
    );
    let output = typecheck_isolated(&src);
    assert!(
        has_trait_impl_sig_mismatch(&output, "arity"),
        "extra parameter must surface TraitImplSignatureMismatch with `arity` detail; \
         got: {:#?}",
        output.errors
    );
}

#[test]
fn impl_with_wrong_parameter_type_rejected_at_impl_site() {
    // A non-self-receiver trait method whose impl supplies a wrong parameter
    // type at a non-receiver position. Uses the Index trait as it has an
    // additional argument beyond the receiver.
    let src = r"
pub trait Index {
    type Output;
    fn at(self, key: i32) -> Self::Output;
}

pub type Bag {
    val: i64;
}

impl Index for Bag {
    type Output = i64;
    fn at(self_bag: Bag, key: string) -> i64 {
        0
    }
}
";
    let output = typecheck_isolated(src);
    assert!(
        has_trait_impl_sig_mismatch(&output, "parameter"),
        "wrong parameter type must surface TraitImplSignatureMismatch with `parameter` detail; \
         got: {:#?}",
        output.errors
    );
}

#[test]
fn impl_with_renamed_method_type_param_is_accepted() {
    // Trait declares a method with type param `T`. Impl declares the same
    // method with a different type-param spelling `U`. After positional
    // renaming the signatures match — must not be rejected.
    //
    // NB: v0.5 does not yet expose user-defined generic trait methods in a
    // shape that exercises this on its own (Iterator's only method is
    // monomorphic). This test pins the legitimacy of method-level type-param
    // renaming on a synthetic trait so the renaming policy is locked
    // independent of trait-method-generic surface evolution.
    let src = r"
pub trait Lift {
    fn lift<T>(self, x: T) -> T;
}

pub type Box {
    inner: i64;
}

impl Lift for Box {
    fn lift<U>(b: Box, x: U) -> U {
        x
    }
}
";
    let output = typecheck_isolated(src);
    let sig_mismatch_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::TraitImplSignatureMismatch { .. }))
        .collect();
    assert!(
        sig_mismatch_errors.is_empty(),
        "renamed method-level type param must be accepted; got mismatch errors: {sig_mismatch_errors:#?}",
    );
}
