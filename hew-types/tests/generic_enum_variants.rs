mod common;

use common::typecheck_isolated as typecheck;

// ── Struct-variant generic inference ─────────────────────────────────────────

// Initialising a generic enum struct-variant should infer the type args from
// the field values and produce the fully-applied enum type.
#[test]
fn struct_variant_init_infers_type_args() {
    let output = typecheck(
        r"
        enum Event<T> {
            Move { x: T, y: T };
            Click;
        }

        fn main() {
            let e: Event<int> = Event::Move { x: 10, y: 20 };
            let _ = e;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

// A struct-variant init should reject a value whose type conflicts with an
// already-inferred type arg for the same parameter.
#[test]
fn struct_variant_init_mismatched_field_type_is_an_error() {
    let output = typecheck(
        r"
        enum Event<T> {
            Move { x: T, y: T };
            Click;
        }

        fn main() {
            // Annotate T=int via first field; passing a bool for the second
            // field is a mismatch because T must be consistent.
            let e: Event<int> = Event::Move { x: 1, y: true };
            let _ = e;
        }
        ",
    );
    assert!(
        !output.errors.is_empty(),
        "expected a type error for mismatched struct-variant field types"
    );
}

// Pattern-matching a generic enum struct-variant should bind field names with
// the concrete scrutinee type args substituted in.
#[test]
fn struct_variant_pattern_binds_concrete_field_types() {
    let output = typecheck(
        r"
        enum Wrapper<T> {
            Pair { first: T, second: T };
            Empty;
        }

        fn add(w: Wrapper<int>) -> int {
            match w {
                Wrapper::Pair { first, second } => first + second,
                Wrapper::Empty => 0,
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

// Using a pattern-bound field of the wrong concrete type should be an error.
#[test]
fn struct_variant_pattern_wrong_field_use_is_an_error() {
    let output = typecheck(
        r"
        enum Wrapper<T> {
            Pair { first: T, second: T };
            Empty;
        }

        fn broken(w: Wrapper<int>) -> bool {
            match w {
                // first is int, but we return it as bool — type error
                Wrapper::Pair { first, second } => first,
                Wrapper::Empty => true,
            }
        }
        ",
    );
    assert!(
        !output.errors.is_empty(),
        "expected a type error for struct-variant field used at wrong type"
    );
}

// Unqualified short-name struct-variant init should also infer type args.
#[test]
fn struct_variant_init_unqualified_infers_type_args() {
    let output = typecheck(
        r"
        enum Shape<T> {
            Rect { width: T, height: T };
            Circle;
        }

        fn main() {
            let s: Shape<int> = Rect { width: 5, height: 3 };
            let _ = s;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

// ── Nested-generic expected-type preseed (review blocker fix) ─────────────────

// When the declared type (e.g. `let w: Wrap<int>`) is already known, nested
// generic fields such as `inner: Box<T>` must be checked as `Box<int>`, not
// the raw `Box<T>`.  Without preseed the checker would see `Box<int>` (from
// the inner struct init) vs `Box<T>` (from the variant definition) and emit a
// spurious mismatch.
#[test]
fn struct_variant_init_nested_generic_field_with_expected_type() {
    let output = typecheck(
        r"
        type Box<T> { value: T }

        enum Wrap<T> {
            Boxed { inner: Box<T> };
        }

        fn main() {
            let w: Wrap<int> = Wrap::Boxed { inner: Box { value: 1 } };
            let _ = w;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors (nested generic field in enum struct-variant): {:?}",
        output.errors
    );
}

// ── Tuple-variant coverage (regression guard) ─────────────────────────────────

// Use an Option-like enum with different variant names to avoid clashing
// with the built-in `Some`/`None` helpers.
#[test]
fn variant_constructors_preserve_type_args() {
    let output = typecheck(
        r"
        enum Maybe<T> {
            Just(T);
            Nothing;
        }

        impl Maybe<int> {
            fn unwrap(m: Maybe<int>) -> int {
                0
            }
        }

        fn main() {
            let explicit: Maybe<int> = Just(42);
            let x = Just(42);
            let y: int = x.unwrap();
            let _: Maybe<int> = explicit;
            let _: int = y;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}
