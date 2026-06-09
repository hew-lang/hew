//! Pointer type spelling: v0.5 requires canonical `*const T` / `*mut T`.
//!
//! Bare `*T` and legacy `*var T` must be rejected with explicit
//! diagnostics so users cannot accidentally end up with the wrong
//! mutability.

use hew_parser::{
    ast::{ExternFnDecl, Item, TypeExpr},
    parse,
};

fn first_extern_arg_type(src: &str) -> TypeExpr {
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
    let Item::ExternBlock(mut extern_block) = item else {
        panic!("expected Item::ExternBlock, got {item:?}");
    };
    let extern_fn: ExternFnDecl = extern_block.functions.remove(0);
    extern_fn
        .params
        .into_iter()
        .next()
        .expect("expected one param")
        .ty
        .0
}

#[test]
fn pointer_type_spelling_accepts_const_and_mut() {
    let immutable = first_extern_arg_type("extern \"C\" { fn read(p: *const u8) -> i32; }");
    match immutable {
        TypeExpr::Pointer {
            is_mutable,
            pointee,
        } => {
            assert!(!is_mutable, "*const T should be immutable");
            match pointee.0 {
                TypeExpr::Named { ref name, .. } if name == "u8" => {}
                other => panic!("expected u8 pointee, got {other:?}"),
            }
        }
        other => panic!("expected pointer type, got {other:?}"),
    }

    let mutable = first_extern_arg_type("extern \"C\" { fn write(p: *mut u8) -> i32; }");
    match mutable {
        TypeExpr::Pointer { is_mutable, .. } => {
            assert!(is_mutable, "*mut T should be mutable");
        }
        other => panic!("expected pointer type, got {other:?}"),
    }
}

#[test]
fn pointer_type_spelling_rejects_legacy_var() {
    let result = parse("extern \"C\" { fn legacy(p: *var u8) -> i32; }");
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `*var T`, got none"
    );
    let msg = result.errors[0].message.clone();
    assert!(
        msg.contains("`*mut T`") && msg.contains("`*var T`"),
        "expected diagnostic to mention `*mut T` and `*var T`, got: {msg}"
    );
}

#[test]
fn pointer_type_spelling_rejects_bare_star() {
    let result = parse("extern \"C\" { fn bare(p: *u8) -> i32; }");
    assert!(
        !result.errors.is_empty(),
        "expected parse error for bare `*T`, got none"
    );
    let msg = result.errors[0].message.clone();
    assert!(
        msg.contains("`*const T`") && msg.contains("`*mut T`"),
        "expected diagnostic to mention `*const T` / `*mut T`, got: {msg}"
    );
}

#[test]
fn pointer_type_spelling_rejects_const_without_pointee() {
    let result = parse("extern \"C\" { fn nope(p: *const) -> i32; }");
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `*const` without pointee"
    );
}

#[test]
fn pointer_type_spelling_rejects_mut_without_pointee() {
    let result = parse("extern \"C\" { fn nope(p: *mut) -> i32; }");
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `*mut` without pointee"
    );
}
