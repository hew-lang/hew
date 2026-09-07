use crate::common;
use hew_types::error::TypeErrorKind;

use common::typecheck_isolated as typecheck;

/// Regression: `fn max<T: Ord>(a: T, b: T) -> T` must accept primitive `i32` arguments.
/// Previously `type_satisfies_trait_bound` fell through to `_ => false` for all non-Named,
/// non-TraitObject Ty variants, so every call with a primitive was rejected.
#[test]
fn primitive_satisfies_ord_bound() {
    let source = r"
        fn max<T: Ord>(a: T, b: T) -> T {
            if a > b { a } else { b }
        }

        fn main() {
            let result = max(3, 7);
        }
    ";

    let output = typecheck(source);
    assert!(
        !output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::BoundsNotSatisfied),
        "i32 should satisfy Ord bound; got errors: {:?}",
        output.errors
    );
}

/// Regression: `string` satisfies `Ord`, so a generic function bounded by `Ord` must accept
/// `string` arguments.  This exercises the non-primitive built-in path (`Ty::String`).
#[test]
fn string_satisfies_ord_bound() {
    let source = r#"
        fn min_str<T: Ord>(a: T, b: T) -> T {
            if a < b { a } else { b }
        }

        fn main() {
            let result = min_str("apple", "banana");
        }
    "#;

    let output = typecheck(source);
    assert!(
        !output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::BoundsNotSatisfied),
        "string should satisfy Ord bound; got errors: {:?}",
        output.errors
    );
}

#[test]
fn trait_bound_violation_reports_error() {
    let source = r"
        trait Describable {
            fn describe(val: Self) -> string;
        }

        type Dog {
            name: string,
        }

        impl Describable for Dog {
            fn describe(d: Dog) -> string {
                d.name
            }
        }

        fn show<T: Describable>(item: T) {
            println(item.describe());
        }

        fn main() {
            show(42);
        }
    ";

    let output = typecheck(source);
    assert!(
        output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::BoundsNotSatisfied),
        "expected a BoundsNotSatisfied error, got {:?}",
        output.errors
    );
    assert!(
        output.errors.iter().any(|err| {
            err.message
                .contains("type `i64` does not implement trait `Describable` required by `T`")
        }),
        "expected user-facing i64 in bound error, got {:?}",
        output.errors
    );
}

/// A trait's supertraits are part of what an impl promises: `impl Sub for T`
/// asserts `T` satisfies every super of `Sub`. Report the gap where the promise
/// is made rather than at some later call that needs the inherited method.
#[test]
fn impl_of_subtrait_requires_its_supertrait_impl() {
    let source = r"
        trait Base {
            fn base(value: Self) -> i64;
        }

        trait Derived: Base {}

        type Widget { size: i64 }

        impl Derived for Widget {}
    ";

    let output = typecheck(source);
    assert!(
        output.errors.iter().any(|err| {
            err.kind == TypeErrorKind::BoundsNotSatisfied
                && err.message.contains("supertrait `Base`")
                && err.message.contains("Widget")
        }),
        "expected the supertrait obligation on `impl Derived for Widget`, got {:?}",
        output.errors
    );
}

/// The same program with the supertrait implemented checks cleanly, so the
/// obligation above is the missing impl and not the subtrait impl itself.
#[test]
fn impl_of_subtrait_with_supertrait_impl_is_accepted() {
    let source = r"
        trait Base {
            fn base(value: Self) -> i64;
        }

        trait Derived: Base {}

        type Widget { size: i64 }

        impl Base for Widget {
            fn base(value: Widget) -> i64 { value.size }
        }

        impl Derived for Widget {}
    ";

    let output = typecheck(source);
    assert!(
        !output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::BoundsNotSatisfied),
        "expected no bound violation, got {:?}",
        output.errors
    );
}
