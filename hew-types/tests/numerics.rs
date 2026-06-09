//! Tests for the integer-width canonicalisation (B-1).
//!
//! Covers:
//! - All 8 explicit integer widths + 2 floats admitted as types.
//! - Aliases `i64 = i64`, `u64 = u64`, `byte = u8`, `float = f64`.
//! - `isize`/`usize` resolve to distinct `Ty::Isize`/`Ty::Usize` (not aliases
//!   for I64/U64) — platform-sized per Q42 ratification.
//! - Mixed-width arithmetic is rejected by the type-checker (unifier).

use hew_types::ty::{Substitution, Ty};
use hew_types::unify::unify;

// ===========================================================================
// Width set admission
// ===========================================================================

#[test]
fn width_aliases_explicit_integers_admitted() {
    // All 8 explicit signed/unsigned widths resolve correctly.
    assert_eq!(Ty::from_name("i8"), Some(Ty::I8));
    assert_eq!(Ty::from_name("i16"), Some(Ty::I16));
    assert_eq!(Ty::from_name("i32"), Some(Ty::I32));
    assert_eq!(Ty::from_name("i64"), Some(Ty::I64));
    assert_eq!(Ty::from_name("u8"), Some(Ty::U8));
    assert_eq!(Ty::from_name("u16"), Some(Ty::U16));
    assert_eq!(Ty::from_name("u32"), Some(Ty::U32));
    assert_eq!(Ty::from_name("u64"), Some(Ty::U64));
}

#[test]
fn width_aliases_floats_admitted() {
    assert_eq!(Ty::from_name("f32"), Some(Ty::F32));
    assert_eq!(Ty::from_name("f64"), Some(Ty::F64));
}

#[test]
fn width_aliases_canonical_aliases_resolve_correctly() {
    // i64 = i64
    assert_eq!(Ty::from_name("i64"), Some(Ty::I64));
    // `int`/`Int` are removed aliases; they must NOT resolve.
    assert_eq!(Ty::from_name("int"), None);
    assert_eq!(Ty::from_name("Int"), None);
    // u64 = u64
    assert_eq!(Ty::from_name("u64"), Some(Ty::U64));
    // `uint` is a removed alias; it must NOT resolve.
    assert_eq!(Ty::from_name("uint"), None);
    // byte = u8
    assert_eq!(Ty::from_name("byte"), Some(Ty::U8));
    // float = f64
    assert_eq!(Ty::from_name("float"), Some(Ty::F64));
    assert_eq!(Ty::from_name("Float"), Some(Ty::F64));
}

#[test]
fn width_aliases_isize_usize_are_distinct_platform_sized_types() {
    // isize and usize are distinct from fixed-width i64/u64 (Q42 ratification).
    let isize_ty = Ty::from_name("isize").expect("isize must resolve");
    let usize_ty = Ty::from_name("usize").expect("usize must resolve");

    assert_eq!(
        isize_ty,
        Ty::Isize,
        "isize must map to Ty::Isize, not Ty::I64"
    );
    assert_eq!(
        usize_ty,
        Ty::Usize,
        "usize must map to Ty::Usize, not Ty::U64"
    );

    // Distinct from the fixed-width aliases.
    assert_ne!(isize_ty, Ty::I64, "isize must not be aliased to i64");
    assert_ne!(usize_ty, Ty::U64, "usize must not be aliased to u64");
    assert_ne!(isize_ty, usize_ty);
}

#[test]
fn width_aliases_isize_usize_are_integers() {
    assert!(
        Ty::Isize.is_integer(),
        "Isize must be classified as integer"
    );
    assert!(
        Ty::Usize.is_integer(),
        "Usize must be classified as integer"
    );
}

#[test]
fn width_aliases_usize_is_unsigned() {
    assert!(
        Ty::Usize.is_unsigned(),
        "Usize must be classified as unsigned"
    );
    assert!(
        !Ty::Isize.is_unsigned(),
        "Isize must not be classified as unsigned"
    );
}

// ===========================================================================
// Mixed-width arithmetic rejection
// ===========================================================================

/// Mixed-width integer arithmetic is rejected at the unifier level.
///
/// `let x: i32 = 1; let y: i64 = x + 1` style code would require
/// `Ty::I32` to unify with `Ty::I64` — the checker must reject that.
/// This mirrors the spec rule (B-D6): explicit cast required.
#[test]
fn mixed_arithmetic_rejected_i32_vs_i64() {
    let mut subst = Substitution::new();
    let result = unify(&mut subst, &Ty::I32, &Ty::I64);
    assert!(
        result.is_err(),
        "mixed i32 + i64 arithmetic must be rejected by the unifier"
    );
}

#[test]
fn mixed_arithmetic_rejected_u32_vs_u64() {
    let mut subst = Substitution::new();
    let result = unify(&mut subst, &Ty::U32, &Ty::U64);
    assert!(result.is_err(), "u32 vs u64 must not unify");
}

#[test]
fn mixed_arithmetic_rejected_i64_vs_u64() {
    let mut subst = Substitution::new();
    let result = unify(&mut subst, &Ty::I64, &Ty::U64);
    assert!(result.is_err(), "i64 vs u64 must not unify (sign mismatch)");
}

#[test]
fn mixed_arithmetic_rejected_isize_vs_i64() {
    // isize is distinct from i64: the checker must reject their mixing.
    let mut subst = Substitution::new();
    let result = unify(&mut subst, &Ty::Isize, &Ty::I64);
    assert!(
        result.is_err(),
        "isize vs i64 must not unify: isize is platform-sized, i64 is fixed-64"
    );
}

#[test]
fn mixed_arithmetic_rejected_usize_vs_u64() {
    let mut subst = Substitution::new();
    let result = unify(&mut subst, &Ty::Usize, &Ty::U64);
    assert!(
        result.is_err(),
        "usize vs u64 must not unify: usize is platform-sized, u64 is fixed-64"
    );
}

#[test]
fn mixed_arithmetic_rejected_isize_vs_usize() {
    let mut subst = Substitution::new();
    let result = unify(&mut subst, &Ty::Isize, &Ty::Usize);
    assert!(
        result.is_err(),
        "isize vs usize must not unify (sign mismatch)"
    );
}

// ===========================================================================
// Same-type unification succeeds
// ===========================================================================

#[test]
fn same_width_unification_succeeds() {
    for ty in [
        Ty::I8,
        Ty::I16,
        Ty::I32,
        Ty::I64,
        Ty::U8,
        Ty::U16,
        Ty::U32,
        Ty::U64,
        Ty::Isize,
        Ty::Usize,
        Ty::F32,
        Ty::F64,
    ] {
        let mut subst = Substitution::new();
        let result = unify(&mut subst, &ty, &ty);
        assert!(result.is_ok(), "{ty:?} should unify with itself");
    }
}
