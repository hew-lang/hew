//! Integration tests for the B-1c integer width-conversion method family.
//!
//! Covers:
//! - `.to_<W>()`:           infallible widening (same sign, strictly wider fixed-width).
//! - `.try_to_<W>()`:       fallible narrowing / cross-sign → `Result<W, NarrowError>`.
//! - `.wrapping_as_<W>()`:  bit-reinterpret (any integer pair) → `W`.
//! - `.saturating_as_<W>()`: clamp-on-overflow (any integer pair) → `W`.
//!
//! Reject cases verify that the type-checker emits `UndefinedMethod` errors for
//! inadmissible `.to_<W>()` calls (same-width, narrowing, cross-sign, platform-sized).

mod common;

use common::typecheck_isolated as typecheck;
use hew_types::error::TypeErrorKind;

// ---------------------------------------------------------------------------
// Accept: infallible widening `.to_<W>()`
// ---------------------------------------------------------------------------

#[test]
fn to_wider_i32_to_i64_accepted() {
    let out = typecheck(
        r"
        fn f() -> i64 {
            let x: i32 = 5;
            x.to_i64()
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "i32.to_i64() should be admitted as infallible widening, got: {:?}",
        out.errors
    );
}

#[test]
fn to_wider_i8_to_i16_accepted() {
    let out = typecheck(
        r"
        fn f() -> i16 {
            let x: i8 = 5;
            x.to_i16()
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "i8.to_i16() should be admitted, got: {:?}",
        out.errors
    );
}

#[test]
fn to_wider_u8_to_u32_accepted() {
    let out = typecheck(
        r"
        fn f() -> u32 {
            let x: u8 = 5;
            x.to_u32()
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "u8.to_u32() should be admitted as infallible widening, got: {:?}",
        out.errors
    );
}

#[test]
fn to_wider_u16_to_u64_accepted() {
    let out = typecheck(
        r"
        fn f() -> u64 {
            let x: u16 = 5;
            x.to_u64()
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "u16.to_u64() should be admitted, got: {:?}",
        out.errors
    );
}

// ---------------------------------------------------------------------------
// Accept: fallible `.try_to_<W>() -> Result<W, NarrowError>`
// ---------------------------------------------------------------------------

#[test]
fn try_to_narrowing_i64_to_i32_accepted() {
    let out = typecheck(
        r"
        fn f() {
            let x: i64 = 5;
            let _: Result<i32, NarrowError> = x.try_to_i32();
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "i64.try_to_i32() should be admitted, got: {:?}",
        out.errors
    );
}

#[test]
fn try_to_return_type_is_result_of_target() {
    // Verify the synthesised type is Result<i32, NarrowError>, not just i32.
    let out = typecheck(
        r"
        fn f() {
            let x: i64 = 5;
            let _: Result<i32, NarrowError> = x.try_to_i32();
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "try_to_i32 return type must match Result<i32, NarrowError>: {:?}",
        out.errors
    );
}

#[test]
fn try_to_cross_sign_u32_to_i32_accepted() {
    let out = typecheck(
        r"
        fn f() {
            let x: u32 = 5;
            let _: Result<i32, NarrowError> = x.try_to_i32();
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "u32.try_to_i32() (cross-sign) should be admitted via try_to, got: {:?}",
        out.errors
    );
}

#[test]
fn try_to_isize_source_accepted() {
    // Use a function parameter rather than a literal to avoid the literal-range
    // check that rejects integer literals coerced to isize in the isolated checker.
    let out = typecheck(
        r"
        fn f(x: isize) {
            let _: Result<i64, NarrowError> = x.try_to_i64();
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "isize.try_to_i64() should be admitted via try_to, got: {:?}",
        out.errors
    );
}

// ---------------------------------------------------------------------------
// Accept: bit-reinterpret `.wrapping_as_<W>() -> W`
// ---------------------------------------------------------------------------

#[test]
fn wrapping_as_u32_to_i32_accepted() {
    let out = typecheck(
        r"
        fn f() -> i32 {
            let x: u32 = 5;
            x.wrapping_as_i32()
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "u32.wrapping_as_i32() should be admitted, got: {:?}",
        out.errors
    );
}

#[test]
fn wrapping_as_i64_to_u8_accepted() {
    let out = typecheck(
        r"
        fn f() -> u8 {
            let x: i64 = 5;
            x.wrapping_as_u8()
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "i64.wrapping_as_u8() should be admitted, got: {:?}",
        out.errors
    );
}

// ---------------------------------------------------------------------------
// Accept: saturating clamp `.saturating_as_<W>() -> W`
// ---------------------------------------------------------------------------

#[test]
fn saturating_as_i64_to_i32_accepted() {
    let out = typecheck(
        r"
        fn f() -> i32 {
            let x: i64 = 5;
            x.saturating_as_i32()
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "i64.saturating_as_i32() should be admitted, got: {:?}",
        out.errors
    );
}

#[test]
fn saturating_as_u64_to_i8_accepted() {
    let out = typecheck(
        r"
        fn f() -> i8 {
            let x: u64 = 5;
            x.saturating_as_i8()
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "u64.saturating_as_i8() should be admitted, got: {:?}",
        out.errors
    );
}

// ---------------------------------------------------------------------------
// Reject: same-width `.to_<W>()` must use no conversion
// ---------------------------------------------------------------------------

#[test]
fn to_same_width_i32_to_i32_rejected() {
    let out = typecheck(
        r"
        fn f() {
            let x: i32 = 5;
            let _ = x.to_i32();
        }
    ",
    );
    let has_error = out
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::UndefinedMethod);
    assert!(
        has_error,
        "i32.to_i32() (same width) must be rejected with UndefinedMethod, got: {:?}",
        out.errors
    );
}

// ---------------------------------------------------------------------------
// Reject: narrowing `.to_<W>()` must use `.try_to_<W>()`
// ---------------------------------------------------------------------------

#[test]
fn to_narrowing_i64_to_i32_rejected() {
    let out = typecheck(
        r"
        fn f() {
            let x: i64 = 5;
            let _ = x.to_i32();
        }
    ",
    );
    let has_error = out
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::UndefinedMethod);
    assert!(
        has_error,
        "i64.to_i32() (narrowing) must be rejected with UndefinedMethod, got: {:?}",
        out.errors
    );
}

// ---------------------------------------------------------------------------
// Reject: cross-sign `.to_<W>()` must use `.try_to_<W>()`
// ---------------------------------------------------------------------------

#[test]
fn to_cross_sign_i32_to_u32_rejected() {
    let out = typecheck(
        r"
        fn f() {
            let x: i32 = 5;
            let _ = x.to_u32();
        }
    ",
    );
    let has_error = out
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::UndefinedMethod);
    assert!(
        has_error,
        "i32.to_u32() (cross-sign) must be rejected with UndefinedMethod, got: {:?}",
        out.errors
    );
}

// ---------------------------------------------------------------------------
// Reject: platform-sized source with `.to_<W>()` must use `.try_to_<W>()`
// ---------------------------------------------------------------------------

#[test]
fn to_isize_source_to_i64_rejected() {
    // Use a function parameter rather than a literal to avoid the literal-range
    // check that rejects integer literals coerced to isize in the isolated checker.
    let out = typecheck(
        r"
        fn f(x: isize) {
            let _ = x.to_i64();
        }
    ",
    );
    let has_error = out
        .errors
        .iter()
        .any(|e| e.kind == TypeErrorKind::UndefinedMethod);
    assert!(
        has_error,
        "isize.to_i64() must be rejected (platform-sized source), got: {:?}",
        out.errors
    );
}

// ---------------------------------------------------------------------------
// Verify: wrapping_as_ and saturating_as_ do NOT interfere with arithmetic
//         wrapping_add / saturating_add
// ---------------------------------------------------------------------------

#[test]
fn wrapping_add_still_works_after_wrapping_as_arm() {
    let out = typecheck(
        r"
        fn f() -> i32 {
            let x: i32 = 1;
            x.wrapping_add(2)
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "wrapping_add must still work after adding wrapping_as_ arm: {:?}",
        out.errors
    );
}

#[test]
fn saturating_add_still_works_after_saturating_as_arm() {
    let out = typecheck(
        r"
        fn f() -> i32 {
            let x: i32 = 1;
            x.saturating_add(2)
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "saturating_add must still work after adding saturating_as_ arm: {:?}",
        out.errors
    );
}

// ---------------------------------------------------------------------------
// Verify: NarrowError is the correct error type in try_to_ results
// ---------------------------------------------------------------------------

#[test]
fn try_to_result_error_type_is_narrow_error() {
    // The return type of try_to_i32() must be exactly Result<i32, NarrowError>.
    // We verify by binding to that type annotation — any mismatch causes a
    // type error.
    let out = typecheck(
        r"
        fn f() {
            let x: i64 = 5;
            let r: Result<i32, NarrowError> = x.try_to_i32();
            let _: Result<i32, NarrowError> = r;
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "try_to_i32 must return Result<i32, NarrowError>: {:?}",
        out.errors
    );
}

// ---------------------------------------------------------------------------
// Verify: float conversions still work (legacy behaviour preserved)
// ---------------------------------------------------------------------------

#[test]
fn to_f64_from_integer_still_admitted() {
    let out = typecheck(
        r"
        fn f() -> f64 {
            let x: i32 = 5;
            x.to_f64()
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "i32.to_f64() must still be admitted (legacy float conversion): {:?}",
        out.errors
    );
}

#[test]
fn to_f32_from_i64_still_admitted() {
    let out = typecheck(
        r"
        fn f() -> f32 {
            let x: i64 = 5;
            x.to_f32()
        }
    ",
    );
    assert!(
        out.errors.is_empty(),
        "i64.to_f32() must still be admitted (legacy float conversion): {:?}",
        out.errors
    );
}
