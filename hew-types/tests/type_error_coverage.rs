use hew_types::error::TypeErrorKind;
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

#[test]
fn test_non_exhaustive_match() {
    let output = typecheck(
        r#"
        enum Color { Red; Green; Blue; }
        fn check(c: Color) -> int {
            match c {
                Red => 1,
                Green => 2,
            }
        }
        fn main() {
            check(Red);
        }
    "#,
    );
    assert!(output
        .warnings
        .iter()
        .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch));
}

#[test]
fn test_non_exhaustive_match_stmt() {
    let output = typecheck(
        r#"
        enum Color { Red; Green; Blue; }
        fn main() {
            let color: Color = Red;
            match color {
                Red => {},
                Green => {},
            }
        }
    "#,
    );
    assert!(output
        .warnings
        .iter()
        .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch));
}

#[test]
fn test_mutability_error() {
    let output = typecheck(
        r#"
        fn main() {
            let x = 42;
            x = 100; // Error: cannot assign to immutable let binding
        }
    "#,
    );
    assert!(output
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::MutabilityError));
}

#[test]
fn test_arity_mismatch() {
    let output = typecheck(
        r#"
        fn add(a: int, b: int) -> int {
            a + b
        }
        fn main() {
            add(5); // Error: wrong number of arguments
        }
    "#,
    );
    assert!(output
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::ArityMismatch));
}
