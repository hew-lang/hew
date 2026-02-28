use hew_types::Checker;

fn typecheck(source: &str) -> hew_types::TypeCheckOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parser errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new();
    checker.check_program(&parsed.program)
}

// Use an Option-like enum with different variant names to avoid clashing
// with the built-in `Some`/`None` helpers.
#[test]
fn variant_constructors_preserve_type_args() {
    let output = typecheck(
        r#"
        enum Maybe<T> {
            Just(T);
            Nothing;
        }

        impl Maybe<int> {
            fn unwrap(self) -> int {
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
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "unexpected errors: {:?}",
        output.errors
    );
}
