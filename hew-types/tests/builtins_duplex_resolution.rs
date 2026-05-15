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
