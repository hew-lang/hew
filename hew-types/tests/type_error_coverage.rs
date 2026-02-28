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
fn test_exhaustive_or_option_match() {
    let output = typecheck(
        r#"
        fn check(opt: Option<int>) -> int {
            match opt {
                Some(x) | None => 1,
            }
        }
        fn main() {
            check(Some(1));
        }
    "#,
    );
    assert!(!output
        .warnings
        .iter()
        .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch));
}

#[test]
fn test_non_exhaustive_option_match() {
    let output = typecheck(
        r#"
        fn check(opt: Option<int>) -> int {
            match opt {
                Some(x) => x,
            }
        }
        fn main() {
            check(Some(1));
        }
    "#,
    );
    assert!(output
        .warnings
        .iter()
        .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch));
}

#[test]
fn test_exhaustive_or_result_match() {
    let output = typecheck(
        r#"
        fn check(res: Result<int, int>) -> int {
            match res {
                Ok(x) | Err(e) => 1,
            }
        }
        fn main() {
            check(Ok(1));
        }
    "#,
    );
    assert!(!output
        .warnings
        .iter()
        .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch));
}

#[test]
fn test_exhaustive_or_enum_match() {
    let output = typecheck(
        r#"
        enum Color { Red; Green; Blue; }
        fn check(c: Color) -> int {
            match c {
                Red | Green | Blue => 1,
            }
        }
        fn main() {
            check(Red);
        }
    "#,
    );
    assert!(!output
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

#[test]
fn test_numeric_same_sign_coercion_allowed() {
    let output = typecheck(
        r#"
        fn main() {
            let x: i64 = 1000;
            let y: i8 = x; // OK: same-sign integer coercion allowed in Hew
        }
    "#,
    );
    assert!(
        output.errors.is_empty(),
        "Same-sign integer coercion should be allowed, got: {:?}",
        output.errors
    );
}

#[test]
fn test_numeric_widening_allowed() {
    let output = typecheck(
        r#"
        fn main() {
            let x: i8 = 42;
            let y: i32 = x; // OK: widening i8 -> i32
        }
    "#,
    );
    assert!(
        output.errors.is_empty(),
        "Expected no errors for i8 -> i32 widening, got: {:?}",
        output.errors
    );
}

#[test]
fn test_lambda_arity_mismatch() {
    let output = typecheck(
        r#"
        fn main() {
            let f: fn(int, int) -> int = (x: int) => x + 1; // Error: lambda has 1 param, expected 2
        }
    "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ArityMismatch),
        "Expected ArityMismatch error for lambda with wrong arity"
    );
}
