//! Borrow type spelling: `&T` is the canonical immutable borrow-marker syntax.
//!
//! M-COW P0 adds `&T` as surface sugar for an immutable non-owning borrow.
//! The mutable borrow form `&mut T` is intentionally reserved — it must be
//! rejected with a clear diagnostic so users don't accidentally write Rust
//! idioms that Hew does not support.
//!
//! Tests:
//!   1. `&T` in a parameter position parses to `TypeExpr::Borrow`.
//!   2. `&T` in a return-type position parses correctly.
//!   3. `&mut T` is rejected with a diagnostic mentioning `&mut`.
//!   4. `&var T` is rejected (legacy spelling guard).

use hew_parser::{
    ast::{Item, TypeExpr},
    parse,
};

fn first_param_type(src: &str) -> TypeExpr {
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors for `{src}`: {:#?}",
        result.errors
    );
    let item = result
        .program
        .items
        .into_iter()
        .next()
        .expect("expected one item")
        .0;
    let Item::Function(fn_decl) = item else {
        panic!("expected Item::Function, got {item:?}");
    };
    fn_decl
        .params
        .into_iter()
        .next()
        .expect("expected one param")
        .ty
        .0
}

fn return_type(src: &str) -> TypeExpr {
    let result = parse(src);
    assert!(
        result.errors.is_empty(),
        "unexpected parse errors for `{src}`: {:#?}",
        result.errors
    );
    let item = result
        .program
        .items
        .into_iter()
        .next()
        .expect("expected one item")
        .0;
    let Item::Function(fn_decl) = item else {
        panic!("expected Item::Function, got {item:?}");
    };
    fn_decl
        .return_type
        .expect("expected a return type annotation")
        .0
}

#[test]
fn borrow_type_param_parses_to_borrow_variant() {
    // `&T` in parameter position must produce `TypeExpr::Borrow`, not
    // `TypeExpr::Pointer { is_mutable: false }`.
    let ty = first_param_type("fn foo(x: &string) -> i32 { return 0; }");
    match ty {
        TypeExpr::Borrow(inner) => match inner.0 {
            TypeExpr::Named { ref name, .. } if name == "string" => {}
            other => panic!("expected Named(string) inside Borrow, got {other:?}"),
        },
        other => panic!("expected TypeExpr::Borrow, got {other:?}"),
    }
}

#[test]
fn borrow_type_return_pos_parses_to_borrow_variant() {
    // `&T` in return-type position — same shape.
    let ty = return_type("fn foo() -> &string { return \"\"; }");
    match ty {
        TypeExpr::Borrow(inner) => match inner.0 {
            TypeExpr::Named { ref name, .. } if name == "string" => {}
            other => panic!("expected Named(string) inside Borrow, got {other:?}"),
        },
        other => panic!("expected TypeExpr::Borrow, got {other:?}"),
    }
}

#[test]
fn borrow_type_distinct_from_const_pointer() {
    // `&T` and `*const T` must parse to different variants — they are
    // distinct surface forms with different round-trip identities.
    let borrow = first_param_type("fn f(x: &i32) -> i32 { return 0; }");
    let ptr = first_param_type("fn f(x: *const i32) -> i32 { return 0; }");
    assert!(
        matches!(borrow, TypeExpr::Borrow(_)),
        "expected Borrow for `&i32`, got {borrow:?}"
    );
    assert!(
        matches!(
            ptr,
            TypeExpr::Pointer {
                is_mutable: false,
                ..
            }
        ),
        "expected Pointer{{is_mutable:false}} for `*const i32`, got {ptr:?}"
    );
}

#[test]
fn borrow_mut_is_rejected() {
    // `&mut T` must be rejected — mutable borrows are not part of Hew v0.5.
    let result = parse("fn f(x: &mut i32) -> i32 { return 0; }");
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `&mut T`, got none"
    );
    let msg = &result.errors[0].message;
    assert!(
        msg.contains("&mut") || msg.contains("mutable"),
        "expected diagnostic to mention `&mut` or `mutable`, got: {msg}"
    );
}

#[test]
fn borrow_var_is_rejected() {
    // `&var T` must be rejected — `var` is not a valid borrow qualifier.
    let result = parse("fn f(x: &var i32) -> i32 { return 0; }");
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `&var T`, got none"
    );
}
