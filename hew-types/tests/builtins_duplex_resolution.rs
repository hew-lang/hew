use hew_types::error::TypeErrorKind;
mod common;

use common::typecheck;

// ---------------------------------------------------------------------------
// Accept fixtures
// ---------------------------------------------------------------------------

/// `duplex_pair<int, int>(16)` resolves to `(Duplex<int, int>, Duplex<int, int>)` and the
/// tuple destructure binds both variables without errors.
#[test]
fn duplex_pair_symmetric_resolves() {
    let source = r"
        fn main() {
            let (a, b) = duplex_pair<int, int>(16);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "duplex_pair<int, int>(16) should typecheck; got: {:#?}",
        output.errors
    );
}

/// Use asymmetric type args to verify cross-wiring: `a: Duplex<int, bool>`,
/// `b: Duplex<bool, int>`. A bug that returns `(Duplex<S,R>, Duplex<S,R>)`
/// instead of `(Duplex<S,R>, Duplex<R,S>)` would give `b: Duplex<int, bool>`
/// and fail the annotation on b below.
#[test]
fn duplex_pair_asymmetric_cross_wired() {
    let source = r"
        fn main() {
            let (a, b) = duplex_pair<int, bool>(8);
            let _: Duplex<int, bool> = a;
            let _: Duplex<bool, int> = b;
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "duplex_pair cross-wiring should give (Duplex<int,bool>, Duplex<bool,int>); got: {:#?}",
        output.errors
    );
}

/// `duplex<int, int>(16)` returns a detached `Duplex<int, int>`.
#[test]
fn duplex_detached_resolves() {
    let source = r"
        fn main() {
            let d: Duplex<int, int> = duplex<int, int>(16);
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "duplex<int, int>(16) should typecheck; got: {:#?}",
        output.errors
    );
}

/// `channel<int>(16)` returns `(Sink<int>, Stream<int>)`.
#[test]
fn channel_resolves_to_sink_stream_pair() {
    let source = r"
        fn main() {
            let (sink, stream) = channel<int>(16);
            let _: Sink<int> = sink;
            let _: Stream<int> = stream;
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "channel<int>(16) should give (Sink<int>, Stream<int>); got: {:#?}",
        output.errors
    );
}

// ---------------------------------------------------------------------------
// Reject fixtures
// ---------------------------------------------------------------------------

/// `duplex_pair<Rc<int>, int>(16)` — `Rc<T>` is not Send; must fail with
/// `BoundsNotSatisfied` for the `S` type parameter.
///
/// The error kind is `BoundsNotSatisfied` (from `enforce_type_param_bounds`),
/// not `InvalidSend` (which is only emitted on the lambda-actor call path).
#[test]
fn duplex_pair_non_send_s_rejected() {
    let source = r"
        fn main() {
            let (a, b) = duplex_pair<Rc<int>, int>(16);
        }
    ";
    let output = typecheck(source);
    let has_bounds_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        has_bounds_err,
        "duplex_pair<Rc<int>, int> should fail with BoundsNotSatisfied; got: {:#?}",
        output.errors
    );
}

/// `duplex_pair<int, Rc<int>>(16)` — `Rc<T>` is not Send on the `R` side.
#[test]
fn duplex_pair_non_send_r_rejected() {
    let source = r"
        fn main() {
            let (a, b) = duplex_pair<int, Rc<int>>(16);
        }
    ";
    let output = typecheck(source);
    let has_bounds_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        has_bounds_err,
        "duplex_pair<int, Rc<int>> should fail with BoundsNotSatisfied; got: {:#?}",
        output.errors
    );
}

/// `channel<Rc<int>>(16)` — `Rc<T>` is not Send; must fail.
#[test]
fn channel_non_send_rejected() {
    let source = r"
        fn main() {
            let (s, r) = channel<Rc<int>>(16);
        }
    ";
    let output = typecheck(source);
    let has_bounds_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        has_bounds_err,
        "channel<Rc<int>> should fail with BoundsNotSatisfied; got: {:#?}",
        output.errors
    );
}

/// `duplex<Rc<int>, int>(16)` — Send constraint must apply to the standalone
/// constructor on the S side, matching `duplex_pair`.
#[test]
fn duplex_non_send_s_rejected() {
    let source = r"
        fn main() {
            let d: Duplex<Rc<int>, int> = duplex<Rc<int>, int>(16);
        }
    ";
    let output = typecheck(source);
    let has_bounds_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        has_bounds_err,
        "duplex<Rc<int>, int> should fail with BoundsNotSatisfied; got: {:#?}",
        output.errors
    );
}

/// `duplex<int, Rc<int>>(16)` — Send constraint on the R side.
#[test]
fn duplex_non_send_r_rejected() {
    let source = r"
        fn main() {
            let d: Duplex<int, Rc<int>> = duplex<int, Rc<int>>(16);
        }
    ";
    let output = typecheck(source);
    let has_bounds_err = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, TypeErrorKind::BoundsNotSatisfied));
    assert!(
        has_bounds_err,
        "duplex<int, Rc<int>> should fail with BoundsNotSatisfied; got: {:#?}",
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

/// Calling `channel(16)` with no generic args must fail similarly.
#[test]
fn channel_missing_generic_args_rejected() {
    let source = r"
        fn main() {
            let (s, r) = channel(16);
        }
    ";
    let output = typecheck(source);
    assert!(
        !output.errors.is_empty(),
        "channel(16) without generic args must produce an error; got: {:#?}",
        output.errors
    );
}
