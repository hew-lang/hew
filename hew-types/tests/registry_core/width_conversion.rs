//! Integration tests for numeric width-conversion methods.
//!
//! Covers:
//! - `.try_to_<W>()`: exact fallible numeric conversion -> `Option<W>`.
//! - `.wrapping_as_<W>()`: bit-reinterpret integer conversion -> `W`.
//! - `.saturating_as_<W>()`: clamp-on-overflow integer conversion -> `W`.
//!
//! The `.to_<W>()` method family is not part of the admitted surface.

use crate::common;

use common::typecheck_isolated as typecheck;
use hew_types::error::TypeErrorKind;
use hew_types::TryConversionKind;

fn assert_accepts(source: &str, context: &str) {
    let out = typecheck(source);
    assert!(
        out.errors.is_empty(),
        "{context} should type-check, got: {:?}",
        out.errors
    );
}

fn assert_undefined_method(source: &str, context: &str) -> Vec<String> {
    let out = typecheck(source);
    let messages: Vec<String> = out
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::UndefinedMethod)
        .map(|e| e.message.clone())
        .collect();
    assert!(
        !messages.is_empty(),
        "{context} should produce UndefinedMethod, got: {:?}",
        out.errors
    );
    messages
}

#[test]
fn try_to_i32_admitted_on_every_numeric_source() {
    for src in [
        "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64", "usize", "f32", "f64",
    ] {
        assert_accepts(
            &format!(
                r"
                fn f(x: {src}) {{
                    let _: Option<i32> = x.try_to_i32();
                }}
            "
            ),
            &format!("{src}.try_to_i32() -> Option<i32>"),
        );
    }
}

#[test]
fn try_to_float_targets_admitted_on_integer_and_float_sources() {
    for (src, target, method) in [
        ("i32", "f32", "try_to_f32"),
        ("u64", "f64", "try_to_f64"),
        ("f64", "f32", "try_to_f32"),
        ("f32", "f64", "try_to_f64"),
    ] {
        assert_accepts(
            &format!(
                r"
                fn f(x: {src}) {{
                    let _: Option<{target}> = x.{method}();
                }}
            "
            ),
            &format!("{src}.{method}() -> Option<{target}>"),
        );
    }
}

#[test]
fn try_to_return_type_is_option_of_target() {
    assert_accepts(
        r"
        fn f(x: i64) {
            let r: Option<i32> = x.try_to_i32();
            let _: Option<i32> = r;
        }
    ",
        "try_to_i32 return type",
    );
}

#[test]
fn try_to_records_checker_owned_conversion_kind() {
    let out = typecheck(
        r"
        fn f(x: f64, y: i32) {
            let _: Option<i32> = x.try_to_i32();
            let _: Option<f32> = y.try_to_f32();
        }
    ",
    );
    assert!(out.errors.is_empty(), "type errors: {:?}", out.errors);
    let mut kinds: Vec<TryConversionKind> = out
        .try_width_cast_lowerings
        .values()
        .map(|lowering| lowering.kind)
        .collect();
    kinds.sort_by_key(|kind| match kind {
        TryConversionKind::IntToInt => 0,
        TryConversionKind::FloatToInt => 1,
        TryConversionKind::IntToFloat => 2,
        TryConversionKind::FloatToFloat => 3,
    });
    assert_eq!(
        kinds,
        vec![TryConversionKind::FloatToInt, TryConversionKind::IntToFloat]
    );
}

#[test]
fn to_numeric_methods_are_not_admitted() {
    for (src, method) in [
        ("i32", "to_i32"),
        ("i32", "to_i64"),
        ("i64", "to_i32"),
        ("u32", "to_i32"),
        ("isize", "to_i64"),
        ("f64", "to_i32"),
        ("i32", "to_f64"),
    ] {
        let messages = assert_undefined_method(
            &format!(
                r"
                fn f(x: {src}) {{
                    let _ = x.{method}();
                }}
            "
            ),
            &format!("{src}.{method}()"),
        );
        assert!(
            messages
                .iter()
                .any(|message| message.contains("use `as`") && message.contains("try_to")),
            "numeric .to_<W>() rejection should name `as` and try_to, got: {messages:?}",
        );
    }
}

#[test]
fn wrapping_as_integer_conversions_are_admitted() {
    assert_accepts(
        r"
        fn f() -> i32 {
            let x: u32 = 5;
            x.wrapping_as_i32()
        }
    ",
        "u32.wrapping_as_i32()",
    );
    assert_accepts(
        r"
        fn f() -> u8 {
            let x: i64 = 5;
            x.wrapping_as_u8()
        }
    ",
        "i64.wrapping_as_u8()",
    );
}

#[test]
fn saturating_as_integer_conversions_are_admitted() {
    assert_accepts(
        r"
        fn f() -> i32 {
            let x: i64 = 5;
            x.saturating_as_i32()
        }
    ",
        "i64.saturating_as_i32()",
    );
    assert_accepts(
        r"
        fn f() -> i8 {
            let x: u64 = 5;
            x.saturating_as_i8()
        }
    ",
        "u64.saturating_as_i8()",
    );
}

#[test]
fn width_cast_methods_do_not_interfere_with_arithmetic_methods() {
    assert_accepts(
        r"
        fn f() -> i32 {
            let x: i32 = 1;
            x.wrapping_add(2)
        }
    ",
        "wrapping_add",
    );
    assert_accepts(
        r"
        fn f() -> i32 {
            let x: i32 = 1;
            x.saturating_add(2)
        }
    ",
        "saturating_add",
    );
}
