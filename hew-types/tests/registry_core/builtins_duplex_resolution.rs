use crate::common;
use hew_types::error::TypeErrorKind;

use common::typecheck;

// ---------------------------------------------------------------------------
// Accept fixtures
// ---------------------------------------------------------------------------

/// `duplex_pair<i64, i64>(16)` resolves to `(Duplex<i64, i64>, Duplex<i64, i64>)` and the
/// tuple destructure binds both variables without errors.
#[test]
fn duplex_pair_symmetric_resolves() {
    let source = r"
        fn main() {
            let (a, b) = duplex_pair<i64, i64>(16);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "duplex_pair<i64, i64>(16) should typecheck; got: {:#?}",
        output.errors
    );
}

/// Use asymmetric type args to verify cross-wiring: `a: Duplex<i64, bool>`,
/// `b: Duplex<bool, i64>`. A bug that returns `(Duplex<S,R>, Duplex<S,R>)`
/// instead of `(Duplex<S,R>, Duplex<R,S>)` would give `b: Duplex<i64, bool>`
/// and fail the annotation on b below.
#[test]
fn duplex_pair_asymmetric_cross_wired() {
    let source = r"
        fn main() {
            let (a, b) = duplex_pair<i64, bool>(8);
            let _: Duplex<i64, bool> = a;
            let _: Duplex<bool, i64> = b;
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "duplex_pair cross-wiring should give (Duplex<i64,bool>, Duplex<bool,i64>); got: {:#?}",
        output.errors
    );
}

/// `duplex<i64, i64>(16)` returns a detached `Duplex<i64, i64>`.
#[test]
fn duplex_detached_resolves() {
    let source = r"
        fn main() {
            let d: Duplex<i64, i64> = duplex<i64, i64>(16);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "duplex<i64, i64>(16) should typecheck; got: {:#?}",
        output.errors
    );
}

// ---------------------------------------------------------------------------
// Reject fixtures
// ---------------------------------------------------------------------------

/// `duplex_pair<Rc<i64>, i64>(16)` — `Rc<T>` is not Send; must fail with
/// `BoundsNotSatisfied` for the `S` type parameter.
///
/// The error kind is `BoundsNotSatisfied` (from `enforce_type_param_bounds`),
/// not `InvalidSend` (which is only emitted on the lambda-actor call path).
#[test]
fn duplex_pair_non_send_s_rejected() {
    let source = r"
        fn main() {
            let (a, b) = duplex_pair<Rc<i64>, i64>(16);
        }
    ";
    let output = typecheck(source);
    let has_bounds_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        has_bounds_err,
        "duplex_pair<Rc<i64>, i64> should fail with BoundsNotSatisfied; got: {:#?}",
        output.errors
    );
}

/// `duplex_pair<i64, Rc<i64>>(16)` — `Rc<T>` is not Send on the `R` side.
#[test]
fn duplex_pair_non_send_r_rejected() {
    let source = r"
        fn main() {
            let (a, b) = duplex_pair<i64, Rc<i64>>(16);
        }
    ";
    let output = typecheck(source);
    let has_bounds_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        has_bounds_err,
        "duplex_pair<i64, Rc<i64>> should fail with BoundsNotSatisfied; got: {:#?}",
        output.errors
    );
}

/// `duplex<Rc<i64>, i64>(16)` — Send constraint must apply to the standalone
/// constructor on the S side, matching `duplex_pair`.
#[test]
fn duplex_non_send_s_rejected() {
    let source = r"
        fn main() {
            let d: Duplex<Rc<i64>, i64> = duplex<Rc<i64>, i64>(16);
        }
    ";
    let output = typecheck(source);
    let has_bounds_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        has_bounds_err,
        "duplex<Rc<i64>, i64> should fail with BoundsNotSatisfied; got: {:#?}",
        output.errors
    );
}

/// `duplex<i64, Rc<i64>>(16)` — Send constraint on the R side.
#[test]
fn duplex_non_send_r_rejected() {
    let source = r"
        fn main() {
            let d: Duplex<i64, Rc<i64>> = duplex<i64, Rc<i64>>(16);
        }
    ";
    let output = typecheck(source);
    let has_bounds_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        has_bounds_err,
        "duplex<i64, Rc<i64>> should fail with BoundsNotSatisfied; got: {:#?}",
        output.errors
    );
}

/// Calling `duplex_pair(16)` with no generic args at all must fail with
/// the type-inference / annotation diagnostic — generic args cannot be
/// inferred from the integer capacity alone.
#[test]
fn duplex_pair_missing_generic_args_rejected() {
    let source = r"
        fn main() {
            let (a, b) = duplex_pair(16);
        }
    ";
    let output = typecheck(source);
    assert!(
        !output.errors.is_empty(),
        "duplex_pair(16) without generic args must produce an error; got: {:#?}",
        output.errors
    );
}

/// Calling `duplex(16)` with no generic args must fail similarly.
#[test]
fn duplex_missing_generic_args_rejected() {
    let source = r"
        fn main() {
            let d = duplex(16);
        }
    ";
    let output = typecheck(source);
    assert!(
        !output.errors.is_empty(),
        "duplex(16) without generic args must produce an error; got: {:#?}",
        output.errors
    );
}

/// `channel(16)` is no longer a builtin; the name is unresolved and must fail.
/// The canonical constructor is `std::channel::channel.new(capacity)`.
#[test]
fn channel_builtin_is_removed_unresolved() {
    let source = r"
        fn main() {
            let (s, r) = channel(16);
        }
    ";
    let output = typecheck(source);
    assert!(
        !output.errors.is_empty(),
        "channel(16) must fail after builtin removal; got: {:#?}",
        output.errors
    );
}
