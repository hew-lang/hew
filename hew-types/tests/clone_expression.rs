//! Type-checking coverage for the `clone <expr>` duplication prefix.
//!
//! `clone x` resolves through the same machinery as `x.clone()`, so these
//! tests pin the user-visible contract: cloneable values type-check, the
//! operand is read non-consumingly, unsupported types fail closed with the
//! method-resolution diagnostic, and a moved binding is steered toward
//! `clone` in its use-after-move suggestion.

mod common;

use common::typecheck;
use hew_types::error::TypeErrorKind;

#[test]
fn clone_string_typechecks_and_leaves_operand_usable() {
    let source = r#"
        fn main() {
            let s = "hello";
            let dup = clone s;
            println(dup);
            println(s);
        }
    "#;
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "cloning a string should type-check cleanly, got: {:?}",
        output.errors
    );
}

#[test]
fn clone_vec_typechecks() {
    let source = r"
        fn main() {
            let xs: Vec<i64> = Vec::new();
            xs.push(1);
            let ys = clone xs;
            println(ys.len());
            println(xs.len());
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "cloning a Vec should type-check cleanly, got: {:?}",
        output.errors
    );
}

#[test]
fn existing_method_clone_still_typechecks() {
    // Regression guard: the prefix must not disturb `x.clone()`.
    let source = r"
        fn main() {
            let xs: Vec<i64> = Vec::new();
            xs.push(1);
            let ys = xs.clone();
            println(ys.len());
            println(xs.len());
        }
    ";
    let output = typecheck(source);
    assert!(
        output.errors.is_empty(),
        "`x.clone()` should still type-check cleanly, got: {:?}",
        output.errors
    );
}

#[test]
fn clone_on_unsupported_scalar_fails_closed() {
    // `i64` has no `clone` method; `clone n` must fail closed exactly as
    // `n.clone()` does, via the shared method-resolution diagnostic.
    let source = r"
        fn main() {
            let n = 5;
            let m = clone n;
            println(m);
        }
    ";
    let output = typecheck(source);
    let undefined = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::UndefinedMethod)
        .unwrap_or_else(|| {
            panic!(
                "expected an UndefinedMethod error for `clone` on i64, got: {:?}",
                output.errors
            )
        });
    assert!(
        undefined.message.contains("clone") && undefined.message.contains("i64"),
        "diagnostic should name the missing `clone` method on `i64`, got: {}",
        undefined.message
    );
}

#[test]
fn use_after_move_suggests_clone() {
    // Sending a non-Copy Vec into an actor consumes it; using it afterwards is
    // a use-after-move whose suggestion should steer the author to `clone`.
    let source = r"
        actor Sink {
            let id: i64;
            receive fn take(v: Vec<i64>) -> i64 { v.len() }
        }
        fn main() {
            let xs: Vec<i64> = Vec::new();
            xs.push(1);
            let sink = spawn Sink(id: 0);
            sink.take(xs);
            println(xs.len());
        }
    ";
    let output = typecheck(source);
    let moved = output
        .errors
        .iter()
        .find(|e| e.kind == TypeErrorKind::UseAfterMove)
        .unwrap_or_else(|| {
            panic!(
                "expected a UseAfterMove error for the consumed Vec, got: {:?}",
                output.errors
            )
        });
    assert!(
        moved.suggestions.iter().any(|s| s.contains("clone xs")),
        "use-after-move suggestion should point at `clone xs`, got: {:?}",
        moved.suggestions
    );
}
