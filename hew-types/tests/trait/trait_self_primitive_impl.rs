//! Tests that `Self` in non-receiver parameter and return position of a trait
//! impl for a primitive type resolves correctly.
//!
//! Prior to this fix, `substitute_trait_sig_for_impl` substituted `Self` with
//! `Ty::Named { name: "i64", builtin: None }`, while the impl's annotation
//! resolved to the flat `Ty::I64` variant.  `canonicalize_type_identity` does
//! not collapse `Ty::Named { name: "i64" }` → `Ty::I64`, so the comparison
//! always failed — yielding a self-contradictory "has type i64 but requires i64".

use crate::common;

use common::typecheck_isolated as typecheck;

// ---------------------------------------------------------------------------
// Non-receiver Self in parameter position
// ---------------------------------------------------------------------------

/// `impl Compare for i64` — both `a` and `b` params are `Self` in the trait.
/// Previously this raised a false "has type i64 but requires i64" error on `b`.
#[test]
fn self_non_receiver_param_i64_typechecks() {
    let output = typecheck(
        r"
        trait Compare {
            fn eq_to(a: Self, b: Self) -> bool;
        }

        impl Compare for i64 {
            fn eq_to(a: i64, b: i64) -> bool {
                a == b
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for i64 impl; got: {:#?}",
        output.errors
    );
}

/// Same trait impl for `bool`.
#[test]
fn self_non_receiver_param_bool_typechecks() {
    let output = typecheck(
        r"
        trait Compare {
            fn eq_to(a: Self, b: Self) -> bool;
        }

        impl Compare for bool {
            fn eq_to(a: bool, b: bool) -> bool {
                a == b
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for bool impl; got: {:#?}",
        output.errors
    );
}

/// Same trait impl for `f64`.
#[test]
fn self_non_receiver_param_f64_typechecks() {
    let output = typecheck(
        r"
        trait Compare {
            fn eq_to(a: Self, b: Self) -> bool;
        }

        impl Compare for f64 {
            fn eq_to(a: f64, b: f64) -> bool {
                a == b
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for f64 impl; got: {:#?}",
        output.errors
    );
}

/// Same trait impl for `i32`.
#[test]
fn self_non_receiver_param_i32_typechecks() {
    let output = typecheck(
        r"
        trait Compare {
            fn eq_to(a: Self, b: Self) -> bool;
        }

        impl Compare for i32 {
            fn eq_to(a: i32, b: i32) -> bool {
                a == b
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for i32 impl; got: {:#?}",
        output.errors
    );
}

// ---------------------------------------------------------------------------
// Self in return position
// ---------------------------------------------------------------------------

/// `Self` used as a return type must also match the primitive variant.
#[test]
fn self_in_return_position_i64_typechecks() {
    let output = typecheck(
        r"
        trait Clone {
            fn clone(self) -> Self;
        }

        impl Clone for i64 {
            fn clone(v: i64) -> i64 {
                v
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for i64 Clone impl; got: {:#?}",
        output.errors
    );
}

/// `Self` in both param and return position (e.g. an `Add` style trait).
#[test]
fn self_in_param_and_return_position_i64_typechecks() {
    let output = typecheck(
        r"
        trait Add {
            fn add(a: Self, b: Self) -> Self;
        }

        impl Add for i64 {
            fn add(a: i64, b: i64) -> i64 {
                a + b
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for i64 Add impl; got: {:#?}",
        output.errors
    );
}

// ---------------------------------------------------------------------------
// Receiver-position Self (must still work — no regression)
// ---------------------------------------------------------------------------

/// A receiver-only Self method on a primitive must still typecheck.
#[test]
fn self_receiver_position_i64_no_regression() {
    let output = typecheck(
        r#"
        trait Stringify {
            fn to_str(self) -> string;
        }

        impl Stringify for i64 {
            fn to_str(v: i64) -> string {
                "num"
            }
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for receiver-position i64 impl; got: {:#?}",
        output.errors
    );
}

// ---------------------------------------------------------------------------
// Non-primitive impl type (must still work — no regression)
// ---------------------------------------------------------------------------

/// A non-primitive struct impl must still typecheck after the fix.
#[test]
fn self_non_receiver_param_struct_no_regression() {
    let output = typecheck(
        r"
        trait Compare {
            fn eq_to(a: Self, b: Self) -> bool;
        }

        type Point { x: i64, y: i64 }

        impl Compare for Point {
            fn eq_to(a: Point, b: Point) -> bool {
                a.x == b.x
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for struct impl; got: {:#?}",
        output.errors
    );
}

// ---------------------------------------------------------------------------
// Type mismatch must still be caught (regression guard)
// ---------------------------------------------------------------------------

/// A genuinely wrong impl (wrong param type) must still be rejected.
#[test]
fn wrong_impl_param_type_is_rejected() {
    use hew_types::error::TypeErrorKind;

    let output = typecheck(
        r"
        trait Compare {
            fn eq_to(a: Self, b: Self) -> bool;
        }

        impl Compare for i64 {
            fn eq_to(a: i64, b: bool) -> bool {
                a == 0
            }
        }
        ",
    );
    assert!(
        output.errors.iter().any(|e| e.kind
            == TypeErrorKind::TraitImplSignatureMismatch {
                trait_name: "Compare".to_string(),
                method_name: "eq_to".to_string(),
                detail: "parameter",
            }),
        "expected TraitImplSignatureMismatch for wrong param type; got: {:#?}",
        output.errors
    );
}

// ---------------------------------------------------------------------------
// Nested Self on a primitive impl (recursion through composite types)
// ---------------------------------------------------------------------------
//
// `canonicalize_type_identity` recurses into composite type variants
// (Vec/Option/tuple/…), so a `Self` nested inside `Vec<Self>`, `Option<Self>`,
// or a tuple must also collapse `Ty::Named { name: "i64" }` → `Ty::I64` on
// both sides. These lock the recursive path against future regression.

/// `Self` nested inside `Vec<Self>` in return position.
#[test]
fn nested_self_in_vec_return_i64_typechecks() {
    let output = typecheck(
        r"
        trait Dup {
            fn dup(a: Self) -> Vec<Self>;
        }

        impl Dup for i64 {
            fn dup(a: i64) -> Vec<i64> {
                Vec::new()
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for Vec<Self> return; got: {:#?}",
        output.errors
    );
}

/// `Self` nested inside `Option<Self>` in return position.
#[test]
fn nested_self_in_option_return_i64_typechecks() {
    let output = typecheck(
        r"
        trait Wrap {
            fn wrap(a: Self) -> Option<Self>;
        }

        impl Wrap for i64 {
            fn wrap(a: i64) -> Option<i64> {
                Some(a)
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for Option<Self> return; got: {:#?}",
        output.errors
    );
}

/// `Self` nested inside a tuple in both param and return position.
#[test]
fn nested_self_in_tuple_return_i64_typechecks() {
    let output = typecheck(
        r"
        trait Pair {
            fn mk(a: Self, b: Self) -> (Self, Self);
        }

        impl Pair for i64 {
            fn mk(a: i64, b: i64) -> (i64, i64) {
                (a, b)
            }
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "expected no errors for (Self, Self) return; got: {:#?}",
        output.errors
    );
}
