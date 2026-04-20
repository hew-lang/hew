mod common;

use common::typecheck_isolated as typecheck;
use hew_types::error::{Severity, TypeErrorKind};

#[test]
fn test_non_exhaustive_match() {
    let output = typecheck(
        r"
        enum Colour { Red; Green; Blue; }
        fn check(c: Colour) -> int {
            match c {
                Red => 1,
                Green => 2,
            }
        }
        fn main() {
            check(Red);
        }
    ",
    );
    // Enum-like non-exhaustive match → hard error, not a warning.
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::NonExhaustiveMatch),
        "expected NonExhaustiveMatch error for enum, got errors: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch),
        "NonExhaustiveMatch for enum must not appear as a warning"
    );
}

#[test]
fn test_non_exhaustive_match_stmt() {
    let output = typecheck(
        r"
        enum Colour { Red; Green; Blue; }
        fn main() {
            let colour: Colour = Red;
            match colour {
                Red => {},
                Green => {},
            }
        }
    ",
    );
    // Enum-like non-exhaustive match → hard error, not a warning.
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::NonExhaustiveMatch),
        "expected NonExhaustiveMatch error for enum stmt, got errors: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch),
        "NonExhaustiveMatch for enum must not appear as a warning"
    );
}

#[test]
fn test_exhaustive_or_option_match() {
    let output = typecheck(
        r"
        fn check(opt: Option<int>) -> int {
            match opt {
                Option::Some(x) => x,
                Option::None => 1,
            }
        }
        fn main() {
            check(Some(1));
        }
    ",
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::NonExhaustiveMatch),
        "qualified Option match must not produce an exhaustiveness error: {:?}",
        output.errors
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UndefinedVariable),
        "qualified Option payload must bind correctly: {:?}",
        output.errors
    );
    assert!(!output
        .warnings
        .iter()
        .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch));
}

#[test]
fn test_non_exhaustive_option_match() {
    let output = typecheck(
        r"
        fn check(opt: Option<int>) -> int {
            match opt {
                Some(x) => x,
            }
        }
        fn main() {
            check(Some(1));
        }
    ",
    );
    // Option is enum-like → hard error.
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::NonExhaustiveMatch),
        "expected NonExhaustiveMatch error for Option, got errors: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch),
        "NonExhaustiveMatch for Option must not appear as a warning"
    );
    let err = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::NonExhaustiveMatch)
        .expect("expected NonExhaustiveMatch error for Option");
    assert_eq!(err.severity, Severity::Error);
    assert_eq!(err.message, "non-exhaustive match: missing None");
    assert_eq!(err.suggestions, vec!["None"]);
}

#[test]
fn test_non_exhaustive_match_suggestions_include_arm_patterns() {
    let output = typecheck(
        r"
        enum Packet {
            Empty;
            Value(int);
            Named { count: int };
        }

        fn label(packet: Packet) -> int {
            match packet {
                Empty => 0,
            }
        }
    ",
    );
    let err = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::NonExhaustiveMatch)
        .expect("expected NonExhaustiveMatch error for Packet");
    assert_eq!(
        err.suggestions,
        vec!["Named { .. }".to_string(), "Value(_)".to_string()]
    );
}

#[test]
fn test_exhaustive_or_result_match() {
    let output = typecheck(
        r"
        fn check(res: Result<int, int>) -> int {
            match res {
                Result::Ok(x) => x,
                Result::Err(e) => e,
            }
        }
        fn main() {
            check(Ok(1));
        }
    ",
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::NonExhaustiveMatch),
        "exhaustive Result match must not produce an error"
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UndefinedVariable),
        "qualified Result payloads must bind correctly: {:?}",
        output.errors
    );
    assert!(!output
        .warnings
        .iter()
        .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch));
}

#[test]
fn test_exhaustive_or_enum_match() {
    let output = typecheck(
        r"
        enum Colour { Red; Green; Blue; }
        fn check(c: Colour) -> int {
            match c {
                Red | Green | Blue => 1,
            }
        }
        fn main() {
            check(Red);
        }
    ",
    );
    assert!(
        !output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::NonExhaustiveMatch),
        "exhaustive enum match must not produce an error"
    );
    assert!(!output
        .warnings
        .iter()
        .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch));
}

/// Scalar (non-enum) types have no closed variant set; missing a catch-all
/// should be a *warning*, not a hard error.
#[test]
fn test_scalar_missing_catchall_is_warning() {
    let output = typecheck(
        r"
        fn check(n: int) -> int {
            match n {
                1 => 10,
                2 => 20,
            }
        }
        fn main() {
            check(1);
        }
    ",
    );
    assert!(
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::NonExhaustiveMatch),
        "scalar missing catch-all should be a warning, got warnings: {:?}",
        output.warnings
    );
    assert!(
        output
            .errors
            .iter()
            .all(|e| e.kind != TypeErrorKind::NonExhaustiveMatch),
        "scalar missing catch-all must not be an error, got errors: {:?}",
        output.errors
    );
    let warning = output
        .warnings
        .iter()
        .find(|w| w.kind == TypeErrorKind::NonExhaustiveMatch)
        .expect("expected NonExhaustiveMatch warning for scalar catch-all");
    assert_eq!(warning.severity, Severity::Warning);
    assert_eq!(
        warning.message,
        "non-exhaustive match: consider adding a wildcard `_` arm"
    );
}

#[test]
fn test_mutability_error() {
    let output = typecheck(
        r"
        fn main() {
            let x = 42;
            x = 100; // Error: cannot assign to immutable let binding
        }
    ",
    );
    let err = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::MutabilityError)
        .expect("Expected MutabilityError");
    assert!(
        err.suggestions.iter().any(|s| s.contains("var x")),
        "Expected `var x` suggestion, got: {:?}",
        err.suggestions
    );
}

#[test]
fn test_arity_mismatch() {
    let output = typecheck(
        r"
        fn add(a: int, b: int) -> int {
            a + b
        }
        fn main() {
            add(5); // Error: wrong number of arguments
        }
    ",
    );
    assert!(output
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::ArityMismatch));
}

#[test]
fn test_numeric_same_sign_coercion_allowed() {
    // With width check, narrowing i64 -> i8 should be rejected
    let output = typecheck(
        r"
        fn main() {
            let x: i8 = 42;
            let y: i64 = x; // OK: widening i8 -> i64
        }
    ",
    );
    assert!(
        output.errors.is_empty(),
        "Widening integer coercion should be allowed, got: {:?}",
        output.errors
    );
}

#[test]
fn test_numeric_widening_allowed() {
    let output = typecheck(
        r"
        fn main() {
            let x: i8 = 42;
            let y: i32 = x; // OK: widening i8 -> i32
        }
    ",
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
        r"
        fn main() {
            let f: fn(int, int) -> int = (x: int) => x + 1; // Error: lambda has 1 param, expected 2
        }
    ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::ArityMismatch),
        "Expected ArityMismatch error for lambda with wrong arity"
    );
}

/// Receiver detection must compare generic arguments, not just the type name.
/// `impl Box<int>` should reject `b: Box<string>` as a receiver parameter.
#[test]
fn test_receiver_param_rejects_mismatched_generics() {
    let output = typecheck(
        r"
        type Box<T> { value: T; }
        impl Box<int> {
            fn bad(b: Box<string>) -> int { 0 }
        }
        fn main() {
            let b = Box { value: 42 };
            b.bad();
        }
        ",
    );
    assert!(
        !output.errors.is_empty(),
        "Expected a type error when receiver generic arguments don't match the impl target, \
         but type-checking succeeded. `Box<string>` should not be treated as a receiver \
         for `impl Box<int>`."
    );
}

/// A non-first parameter whose type matches the impl target must not be
/// flagged as a mutable receiver. Only the first parameter can be the receiver.
#[test]
fn test_non_receiver_param_same_type_not_flagged() {
    let output = typecheck(
        r"
        type Box { value: int; }
        impl Box {
            fn combine(b: Box, var other: Box) -> int { b.value + other.value }
        }
        fn main() {
            let b1 = Box { value: 1 };
            let b2 = Box { value: 2 };
            println(b1.combine(b2));
        }
    ",
    );
    assert!(
        output.errors.is_empty(),
        "non-receiver param of same type should not trigger receiver warning: {:?}",
        output.errors
    );
}
