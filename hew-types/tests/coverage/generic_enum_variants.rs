use crate::common;

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
            let e: Event<i64> = Event::Move { x: 10, y: 20 };
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
            // Annotate T=i64 via first field; passing a bool for the second
            // field is a mismatch because T must be consistent.
            let e: Event<i64> = Event::Move { x: 1, y: true };
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

        fn add(w: Wrapper<i64>) -> i64 {
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

        fn broken(w: Wrapper<i64>) -> bool {
            match w {
                // first is i64, but we return it as bool — type error
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
            let s: Shape<i64> = Rect { width: 5, height: 3 };
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

// When the declared type (e.g. `let w: Wrap<i64>`) is already known, nested
// generic fields such as `inner: Box<T>` must be checked as `Box<i64>`, not
// the raw `Box<T>`.  Without preseed the checker would see `Box<i64>` (from
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
            let w: Wrap<i64> = Wrap::Boxed { inner: Box { value: 1 } };
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

        impl Maybe<i64> {
            fn unwrap(m: Maybe<i64>) -> i64 {
                0
            }
        }

        fn main() {
            let explicit: Maybe<i64> = Just(42);
            let x = Just(42);
            let y: i64 = x.unwrap();
            let _: Maybe<i64> = explicit;
            let _: i64 = y;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

// Pattern-matching a generic enum tuple-variant should bind the payload with
// the concrete scrutinee type args substituted in, so a downstream call that
// requires a trait bound (e.g. `Display`) on the payload resolves against the
// concrete type rather than the enum's free type parameter. Regression guard
// for the generic-enum tuple-variant substitution fix in patterns.rs.
#[test]
fn tuple_variant_pattern_binds_concrete_payload_for_bound_resolution() {
    let output = typecheck(
        r"
        pub enum Either<A, B> {
            Left(A);
            Right(B);
        }

        fn main() -> i64 {
            let e: Either<i64, string> = Either::Left(42);
            match e {
                Either::Left(n) => println(n),
                Either::Right(s) => println(s),
            }
            0
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

// A generic function with a `T: Display` where-clause bound should be able to
// pass a value of type `T` to a builtin (`println`) that itself requires
// `Display`, even when `T` is destructured from a generic enum payload. The
// abstract type parameter satisfies the bound by carrying it on the function
// signature.
#[test]
fn generic_fn_type_param_bound_satisfies_display_via_enum_payload() {
    let output = typecheck(
        r#"
        pub enum Foo<T: Display> {
            Item(T);
            Nothing;
        }

        fn show<T: Display>(f: Foo<T>) {
            match f {
                Foo::Item(x) => println(x),
                Foo::Nothing => println("nothing"),
            }
        }

        fn main() -> i64 {
            show(Foo::Item(42));
            show(Foo::Item("hello"));
            0
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

// An impl-scoped inline bound (`impl<T: Display> Holder<T>`) should allow a
// method body to pass the field of type T to println, which requires Display.
#[test]
fn impl_inline_bound_satisfies_display_in_method_body() {
    let output = typecheck(
        r"
        type Holder<T> { value: T }

        impl<T: Display> Holder<T> {
            fn show(h: Holder<T>) {
                println(h.value)
            }
        }

        fn main() -> i64 {
            let h = Holder { value: 42 };
            h.show();
            0
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

// An impl-scoped where-clause bound (`impl<T> Holder<T> where T: Display`)
// should behave identically to the inline form.
#[test]
fn impl_where_clause_bound_satisfies_display_in_method_body() {
    let output = typecheck(
        r"
        type Holder<T> { value: T }

        impl<T> Holder<T> where T: Display {
            fn show(h: Holder<T>) {
                println(h.value)
            }
        }

        fn main() -> i64 {
            let h = Holder { value: 42 };
            h.show();
            0
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

// Without any bound the method body should still fail: `T` alone does not
// satisfy the `Display` requirement of println.
#[test]
fn impl_without_bound_rejects_display_call_in_method_body() {
    let output = typecheck(
        r"
        type Holder<T> { value: T }

        impl<T> Holder<T> {
            fn show(h: Holder<T>) {
                println(h.value)
            }
        }
        ",
    );
    assert!(
        !output.errors.is_empty(),
        "expected a type error for missing Display bound, but got none"
    );
}
