use hew_types::error::TypeErrorKind;
use hew_types::Checker;

fn typecheck(source: &str) -> hew_types::TypeCheckOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parser errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    checker.check_program(&parsed.program)
}

#[test]
fn impl_requires_associated_type_definition() {
    let output = typecheck(
        r"
        trait Iterator {
            type Item;
            fn next(val: Self) -> Self::Item;
        }

        type Counter {
            value: int;
        }

        impl Iterator for Counter {
            fn next(c: Counter) -> Self::Item {
                c.value
            }
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|err| err.kind == TypeErrorKind::UndefinedType),
        "expected undefined type error, got {:?}",
        output.errors
    );
}

#[test]
fn impl_type_aliases_resolve_in_methods() {
    let output = typecheck(
        r"
        trait Iterator {
            type Item;
            fn next(val: Self) -> Self::Item;
        }

        type Counter {
            value: int;
        }

        impl Iterator for Counter {
            type Item = int;
            fn next(c: Counter) -> Self::Item {
                c.value
            }
        }

        fn takes_int(value: int) {}

        fn main() {
            let counter = Counter { value: 1 };
            takes_int(counter.next());
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}

#[test]
fn trait_default_associated_type_used_in_impl() {
    let output = typecheck(
        r"
        trait Identity {
            type Output = int;
            fn value(val: Self) -> Self::Output;
        }

        type Answer {
            x: int;
        }

        impl Identity for Answer {
            fn value(a: Answer) -> Self::Output {
                42
            }
        }

        fn accepts_int(value: int) {}

        fn main() {
            let a = Answer { x: 1 };
            accepts_int(a.value());
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}
