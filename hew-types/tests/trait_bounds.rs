use hew_types::error::TypeErrorKind;
use hew_types::Checker;

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

    let parse = hew_parser::parse(source);
    assert!(parse.errors.is_empty(), "parser errors: {:?}", parse.errors);

    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let output = checker.check_program(&parse.program);
    assert!(
        !output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::BoundsNotSatisfied),
        "i32 should satisfy Ord bound; got errors: {:?}",
        output.errors
    );
}

/// Regression: `String` satisfies `Ord`, so a generic function bounded by `Ord` must accept
/// `String` arguments.  This exercises the non-primitive built-in path (`Ty::String`).
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

    let parse = hew_parser::parse(source);
    assert!(parse.errors.is_empty(), "parser errors: {:?}", parse.errors);

    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let output = checker.check_program(&parse.program);
    assert!(
        !output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::BoundsNotSatisfied),
        "String should satisfy Ord bound; got errors: {:?}",
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
            name: string;
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

    let parse = hew_parser::parse(source);
    assert!(parse.errors.is_empty(), "parser errors: {:?}", parse.errors);

    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let output = checker.check_program(&parse.program);
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
                .contains("type `int` does not implement trait `Describable` required by `T`")
                && !err.message.contains("type `i64`")
        }),
        "expected user-facing int in bound error, got {:?}",
        output.errors
    );
}
