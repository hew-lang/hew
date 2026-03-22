//! Hew runtime: `option` module.
//!
//! Tagged union for `Option<T>` — either `None` (tag=0) or `Some(value)` (tag=1).
//! Layout-compatible with the C runtime representation used by MLIR codegen.

use std::ffi::{c_char, c_void};

use crate::tagged_union::{
    decode_const_ptr, decode_f64, decode_i32, decode_i64, decode_mut_ptr, encode_f64, encode_i32,
    encode_i64, is_variant_0, is_variant_1, TAG_VARIANT_0, TAG_VARIANT_1,
};

/// ABI-stable `Option<T>` representation.
///
/// `tag == 0` → None, `tag == 1` → Some. The `value` field holds the payload
/// as a 64-bit union: i32/i64/f64 stored directly, pointers stored as `*mut c_void`.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewOption {
    /// 0 = None, 1 = Some
    pub tag: i32,
    _pad: i32,
    /// Payload (interpreted based on element type).
    pub value: u64,
}

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

/// Create a `None` option.
#[no_mangle]
pub extern "C" fn hew_option_none() -> HewOption {
    HewOption {
        tag: TAG_VARIANT_0,
        _pad: 0,
        value: 0,
    }
}

/// Create a `Some(i32)` option.
#[no_mangle]
pub extern "C" fn hew_option_some_i32(val: i32) -> HewOption {
    HewOption {
        tag: TAG_VARIANT_1,
        _pad: 0,
        value: encode_i32(val),
    }
}

/// Create a `Some(i64)` option.
#[no_mangle]
pub extern "C" fn hew_option_some_i64(val: i64) -> HewOption {
    HewOption {
        tag: TAG_VARIANT_1,
        _pad: 0,
        value: encode_i64(val),
    }
}

/// Create a `Some(f64)` option.
#[no_mangle]
pub extern "C" fn hew_option_some_f64(val: f64) -> HewOption {
    HewOption {
        tag: TAG_VARIANT_1,
        _pad: 0,
        value: encode_f64(val),
    }
}

// ---------------------------------------------------------------------------
// Predicates
// ---------------------------------------------------------------------------

/// Returns 1 if the option is `Some`, 0 if `None`.
#[no_mangle]
pub extern "C" fn hew_option_is_some(opt: HewOption) -> i32 {
    i32::from(is_variant_1(opt.tag))
}

/// Returns 1 if the option is `None`, 0 if `Some`.
#[no_mangle]
pub extern "C" fn hew_option_is_none(opt: HewOption) -> i32 {
    i32::from(is_variant_0(opt.tag))
}

// ---------------------------------------------------------------------------
// Unwrap
// ---------------------------------------------------------------------------

/// Unwrap the i32 value. Aborts if `None`.
#[no_mangle]
pub extern "C" fn hew_option_unwrap_i32(opt: HewOption) -> i32 {
    if is_variant_0(opt.tag) {
        eprintln!("hew: unwrap called on None");
        std::process::abort();
    }
    decode_i32(opt.value)
}

/// Unwrap the i64 value. Aborts if `None`.
#[no_mangle]
pub extern "C" fn hew_option_unwrap_i64(opt: HewOption) -> i64 {
    if is_variant_0(opt.tag) {
        eprintln!("hew: unwrap called on None");
        std::process::abort();
    }
    decode_i64(opt.value)
}

/// Unwrap the f64 value. Aborts if `None`.
#[no_mangle]
pub extern "C" fn hew_option_unwrap_f64(opt: HewOption) -> f64 {
    if is_variant_0(opt.tag) {
        eprintln!("hew: unwrap called on None");
        std::process::abort();
    }
    decode_f64(opt.value)
}

// ---------------------------------------------------------------------------
// unwrap_or (default fallback)
// ---------------------------------------------------------------------------

/// Unwrap i32 or return `default` if `None`.
#[no_mangle]
pub extern "C" fn hew_option_unwrap_or_i32(opt: HewOption, default: i32) -> i32 {
    if is_variant_0(opt.tag) {
        default
    } else {
        decode_i32(opt.value)
    }
}

/// Unwrap i64 or return `default` if `None`.
#[no_mangle]
pub extern "C" fn hew_option_unwrap_or_i64(opt: HewOption, default: i64) -> i64 {
    if is_variant_0(opt.tag) {
        default
    } else {
        decode_i64(opt.value)
    }
}

/// Unwrap f64 or return `default` if `None`.
#[no_mangle]
pub extern "C" fn hew_option_unwrap_or_f64(opt: HewOption, default: f64) -> f64 {
    if is_variant_0(opt.tag) {
        default
    } else {
        decode_f64(opt.value)
    }
}

/// Unwrap pointer or return `default` if `None`.
///
/// # Safety
///
/// Caller must ensure the stored pointer (if `Some`) and the default are valid.
#[no_mangle]
pub unsafe extern "C" fn hew_option_unwrap_or_ptr(
    opt: HewOption,
    default: *mut c_void,
) -> *mut c_void {
    if is_variant_0(opt.tag) {
        default
    } else {
        // SAFETY: caller guarantees stored pointer validity.
        decode_mut_ptr(opt.value)
    }
}

// ---------------------------------------------------------------------------
// map
// ---------------------------------------------------------------------------

/// Map an `Option<i32>` by applying `f`. Returns `None` if input is `None`.
///
/// # Safety
///
/// `f` must be a valid function pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_option_map_i32(
    opt: HewOption,
    f: unsafe extern "C" fn(i32) -> i32,
) -> HewOption {
    if is_variant_0(opt.tag) {
        opt
    } else {
        // SAFETY: caller guarantees `f` is valid.
        let result = unsafe { f(decode_i32(opt.value)) };
        HewOption {
            tag: TAG_VARIANT_1,
            _pad: 0,
            value: encode_i32(result),
        }
    }
}

/// Returns 1 if `Some` and the value equals `needle`, 0 otherwise.
#[no_mangle]
pub extern "C" fn hew_option_contains_i32(opt: HewOption, needle: i32) -> i32 {
    if is_variant_0(opt.tag) {
        0
    } else {
        i32::from(decode_i32(opt.value) == needle)
    }
}

/// Returns 1 if `Some` and the string equals `needle`, 0 otherwise.
///
/// # Safety
///
/// Both the stored pointer and `needle` must be valid C strings (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_option_contains_str(opt: HewOption, needle: *const c_char) -> i32 {
    if is_variant_0(opt.tag) {
        return 0;
    }
    let stored = decode_const_ptr::<c_char>(opt.value);
    if stored.is_null() || needle.is_null() {
        return i32::from(stored.is_null() && needle.is_null());
    }
    // SAFETY: caller guarantees both strings are valid and null-terminated.
    let cmp = unsafe { libc::strcmp(stored, needle) };
    i32::from(cmp == 0)
}

/// Replace the value inside a `Some`, returning the old option. If `None`, returns `None`
/// and does nothing.
///
/// # Safety
///
/// `opt` must be a valid pointer to a `HewOption`.
#[no_mangle]
#[expect(
    clippy::not_unsafe_ptr_arg_deref,
    reason = "C ABI function — caller guarantees pointer validity"
)]
pub extern "C" fn hew_option_replace_i32(opt: *mut HewOption, new_val: i32) -> HewOption {
    // SAFETY: caller guarantees `opt` points to a valid HewOption.
    let o = unsafe { &mut *opt };
    let old = *o;
    if is_variant_1(o.tag) {
        o.value = encode_i32(new_val);
    }
    old
}

/// Take the value out of the option, leaving `None` in its place.
///
/// # Safety
///
/// `opt` must be a valid pointer to a `HewOption`.
#[no_mangle]
#[expect(
    clippy::not_unsafe_ptr_arg_deref,
    reason = "C ABI function — caller guarantees pointer validity"
)]
pub extern "C" fn hew_option_take(opt: *mut HewOption) -> HewOption {
    // SAFETY: caller guarantees `opt` points to a valid HewOption.
    let o = unsafe { &mut *opt };
    let old = *o;
    o.tag = TAG_VARIANT_0;
    o.value = 0;
    old
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    // -- None constructor --

    #[test]
    fn none_has_tag_zero_and_predicates_correct() {
        // Catches: wrong tag constant for None, swapped predicates
        let opt = hew_option_none();
        assert_eq!(opt.tag, TAG_VARIANT_0);
        assert_eq!(hew_option_is_none(opt), 1);
        assert_eq!(hew_option_is_some(opt), 0);
    }

    // -- Some constructors + accessor round-trips --

    #[test]
    fn some_i32_round_trips_value() {
        // Catches: encode/decode mismatch, value stored in wrong field
        let opt = hew_option_some_i32(42);
        assert_eq!(opt.tag, TAG_VARIANT_1);
        assert_eq!(decode_i32(opt.value), 42);
    }

    #[test]
    fn some_i32_negative_round_trips() {
        // Catches: sign-extension bug in i32 encoding
        let opt = hew_option_some_i32(-1);
        assert_eq!(decode_i32(opt.value), -1);
    }

    #[test]
    fn some_i32_boundary_values() {
        // Catches: truncation at 32-bit boundaries
        for &val in &[i32::MIN, i32::MAX, 0] {
            let opt = hew_option_some_i32(val);
            assert_eq!(decode_i32(opt.value), val, "failed for {val}");
        }
    }

    #[test]
    fn some_i64_round_trips_value() {
        // Catches: i64 stored in u64 incorrectly
        let opt = hew_option_some_i64(999_999_999_999);
        assert_eq!(decode_i64(opt.value), 999_999_999_999);
    }

    #[test]
    fn some_i64_boundary_values() {
        // Catches: sign-extension or truncation for extreme i64 values
        for &val in &[i64::MIN, i64::MAX, 0i64] {
            let opt = hew_option_some_i64(val);
            assert_eq!(decode_i64(opt.value), val, "failed for {val}");
        }
    }

    #[test]
    fn some_f64_round_trips_value() {
        // Catches: f64 bit representation mangled during encode/decode
        let opt = hew_option_some_f64(1.0 / 3.0);
        assert_eq!(decode_f64(opt.value).to_bits(), (1.0_f64 / 3.0).to_bits());
    }

    #[test]
    fn some_f64_special_values() {
        // Catches: NaN/Infinity bits lost in to_bits/from_bits round-trip
        let nan = hew_option_some_f64(f64::NAN);
        assert!(decode_f64(nan.value).is_nan());

        let inf = hew_option_some_f64(f64::INFINITY);
        assert!(decode_f64(inf.value).is_infinite());
        assert!(decode_f64(inf.value).is_sign_positive());

        let neg_inf = hew_option_some_f64(f64::NEG_INFINITY);
        assert!(decode_f64(neg_inf.value).is_infinite());
        assert!(decode_f64(neg_inf.value).is_sign_negative());

        // Negative zero must preserve sign bit
        let neg_zero = hew_option_some_f64(-0.0_f64);
        let decoded = decode_f64(neg_zero.value);
        assert!(decoded.is_sign_negative());
        assert_eq!(decoded.to_bits(), (-0.0_f64).to_bits());
    }

    // -- Predicates for Some --

    #[test]
    fn is_some_true_for_some_variant() {
        // Catches: predicate checking wrong tag value
        let opt = hew_option_some_i32(0);
        assert_eq!(hew_option_is_some(opt), 1);
        assert_eq!(hew_option_is_none(opt), 0);
    }

    // -- Unwrap on Some --

    #[test]
    fn unwrap_i32_returns_value_for_some() {
        // Catches: unwrap returning wrong value or aborting on Some
        let opt = hew_option_some_i32(-42);
        assert_eq!(hew_option_unwrap_i32(opt), -42);
    }

    #[test]
    fn unwrap_i64_returns_value_for_some() {
        let opt = hew_option_some_i64(i64::MAX);
        assert_eq!(hew_option_unwrap_i64(opt), i64::MAX);
    }

    #[test]
    fn unwrap_f64_returns_value_for_some() {
        let opt = hew_option_some_f64(1.0 / 3.0);
        let got = hew_option_unwrap_f64(opt);
        assert_eq!(got.to_bits(), (1.0_f64 / 3.0).to_bits());
    }

    // -- unwrap_or --

    #[test]
    fn unwrap_or_i32_returns_value_when_some() {
        // Catches: always returning default regardless of tag
        let opt = hew_option_some_i32(7);
        assert_eq!(hew_option_unwrap_or_i32(opt, 999), 7);
    }

    #[test]
    fn unwrap_or_i32_returns_default_when_none() {
        // Catches: returning garbage from value field instead of default
        let opt = hew_option_none();
        assert_eq!(hew_option_unwrap_or_i32(opt, -1), -1);
    }

    #[test]
    fn unwrap_or_i64_returns_value_when_some() {
        let opt = hew_option_some_i64(123_456);
        assert_eq!(hew_option_unwrap_or_i64(opt, 0), 123_456);
    }

    #[test]
    fn unwrap_or_i64_returns_default_when_none() {
        let opt = hew_option_none();
        assert_eq!(hew_option_unwrap_or_i64(opt, -999), -999);
    }

    #[test]
    fn unwrap_or_f64_returns_value_when_some() {
        let opt = hew_option_some_f64(1.5);
        assert_eq!(
            hew_option_unwrap_or_f64(opt, 0.0).to_bits(),
            1.5_f64.to_bits()
        );
    }

    #[test]
    fn unwrap_or_f64_returns_default_when_none() {
        let opt = hew_option_none();
        assert_eq!(
            hew_option_unwrap_or_f64(opt, 99.5).to_bits(),
            99.5_f64.to_bits()
        );
    }

    #[test]
    fn unwrap_or_ptr_returns_value_when_some() {
        // Catches: returning default even when Some contains a valid pointer
        let mut data: i32 = 42;
        let ptr: *mut c_void = (&raw mut data).cast();
        let opt = HewOption {
            tag: TAG_VARIANT_1,
            _pad: 0,
            value: ptr as u64,
        };
        let default = std::ptr::null_mut();
        // SAFETY: opt contains a valid pointer, default is null.
        let result = unsafe { hew_option_unwrap_or_ptr(opt, default) };
        assert_eq!(result, ptr);
    }

    #[test]
    fn unwrap_or_ptr_returns_default_when_none() {
        // Catches: returning garbage pointer from None's value field
        let mut fallback: i32 = 99;
        let default: *mut c_void = (&raw mut fallback).cast();
        let opt = hew_option_none();
        // SAFETY: default is a valid pointer.
        let result = unsafe { hew_option_unwrap_or_ptr(opt, default) };
        assert_eq!(result, default);
    }

    // -- map --

    unsafe extern "C" fn negate_i32(x: i32) -> i32 {
        -x
    }

    #[test]
    fn map_i32_applies_function_to_some() {
        // Catches: map not calling function, or storing unmapped value
        let opt = hew_option_some_i32(21);
        // SAFETY: negate_i32 is a valid function pointer.
        let mapped = unsafe { hew_option_map_i32(opt, negate_i32) };
        assert_eq!(mapped.tag, TAG_VARIANT_1);
        assert_eq!(decode_i32(mapped.value), -21);
    }

    #[test]
    fn map_i32_returns_none_for_none() {
        // Catches: map calling function on None, or changing tag
        let opt = hew_option_none();
        // SAFETY: negate_i32 is valid.
        let mapped = unsafe { hew_option_map_i32(opt, negate_i32) };
        assert_eq!(mapped.tag, TAG_VARIANT_0);
        assert_eq!(hew_option_is_none(mapped), 1);
    }

    // -- contains_i32 --

    #[test]
    fn contains_i32_matching_value() {
        // Catches: contains always returning 0
        let opt = hew_option_some_i32(10);
        assert_eq!(hew_option_contains_i32(opt, 10), 1);
    }

    #[test]
    fn contains_i32_non_matching_value() {
        // Catches: contains always returning 1
        let opt = hew_option_some_i32(10);
        assert_eq!(hew_option_contains_i32(opt, 11), 0);
    }

    #[test]
    fn contains_i32_none_returns_zero() {
        // Catches: contains not checking tag first
        let opt = hew_option_none();
        assert_eq!(hew_option_contains_i32(opt, 0), 0);
    }

    // -- contains_str --

    #[test]
    fn contains_str_matching_string() {
        // Catches: strcmp comparison inverted or pointer comparison instead of content
        let stored = CString::new("hello").unwrap();
        let opt = HewOption {
            tag: TAG_VARIANT_1,
            _pad: 0,
            value: stored.as_ptr() as u64,
        };
        let needle = CString::new("hello").unwrap();
        // SAFETY: both strings are valid CStrings.
        assert_eq!(unsafe { hew_option_contains_str(opt, needle.as_ptr()) }, 1);
    }

    #[test]
    fn contains_str_non_matching_string() {
        // Catches: always returning 1
        let stored = CString::new("hello").unwrap();
        let opt = HewOption {
            tag: TAG_VARIANT_1,
            _pad: 0,
            value: stored.as_ptr() as u64,
        };
        let needle = CString::new("world").unwrap();
        // SAFETY: both strings are valid CStrings.
        assert_eq!(unsafe { hew_option_contains_str(opt, needle.as_ptr()) }, 0);
    }

    #[test]
    fn contains_str_empty_string_matches_empty() {
        // Catches: empty string handled differently from non-empty
        let stored = CString::new("").unwrap();
        let opt = HewOption {
            tag: TAG_VARIANT_1,
            _pad: 0,
            value: stored.as_ptr() as u64,
        };
        let needle = CString::new("").unwrap();
        // SAFETY: valid CStrings.
        assert_eq!(unsafe { hew_option_contains_str(opt, needle.as_ptr()) }, 1);
    }

    #[test]
    fn contains_str_none_returns_zero() {
        // Catches: not checking tag before string comparison
        let opt = hew_option_none();
        let needle = CString::new("x").unwrap();
        // SAFETY: needle is valid, opt is None.
        assert_eq!(unsafe { hew_option_contains_str(opt, needle.as_ptr()) }, 0);
    }

    #[test]
    fn contains_str_both_null_pointers_match() {
        // Catches: null+null case not handled as equal
        let opt = HewOption {
            tag: TAG_VARIANT_1,
            _pad: 0,
            value: 0, // null pointer
        };
        // SAFETY: both pointers are null — the function handles this case.
        assert_eq!(unsafe { hew_option_contains_str(opt, std::ptr::null()) }, 1);
    }

    #[test]
    fn contains_str_one_null_one_not_returns_zero() {
        // Catches: null asymmetry — one null and one non-null must not match
        let stored = CString::new("hello").unwrap();
        let opt = HewOption {
            tag: TAG_VARIANT_1,
            _pad: 0,
            value: stored.as_ptr() as u64,
        };
        // SAFETY: stored is valid, needle is null.
        assert_eq!(unsafe { hew_option_contains_str(opt, std::ptr::null()) }, 0);
    }

    #[test]
    fn contains_str_stored_null_needle_not_null_returns_zero() {
        // Catches: passing null stored pointer into strcmp when needle is non-null
        let opt = HewOption {
            tag: TAG_VARIANT_1,
            _pad: 0,
            value: 0, // null stored pointer
        };
        let needle = CString::new("hello").unwrap();
        // SAFETY: stored is null, needle is valid — the function must handle this.
        assert_eq!(unsafe { hew_option_contains_str(opt, needle.as_ptr()) }, 0);
    }

    // -- unwrap abort behaviour --

    #[test]
    fn unwrap_i32_aborts_on_none() {
        // Catches: unwrap returning garbage instead of aborting on None
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "option::tests::_helper_unwrap_i32_none",
                "--include-ignored",
            ])
            .env("RUST_TEST_THREADS", "1")
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "unwrap on None must terminate abnormally"
        );
    }

    #[test]
    #[ignore = "subprocess helper for unwrap_i32_aborts_on_none death test"]
    fn _helper_unwrap_i32_none() {
        let opt = hew_option_none();
        let _ = hew_option_unwrap_i32(opt);
    }

    // -- replace_i32 --

    #[test]
    fn replace_i32_swaps_value_in_some() {
        // Catches: not writing new value, or returning new instead of old
        let mut opt = hew_option_some_i32(10);
        let old = hew_option_replace_i32(&raw mut opt, 20);
        assert_eq!(decode_i32(old.value), 10, "old value must be returned");
        assert_eq!(old.tag, TAG_VARIANT_1);
        assert_eq!(decode_i32(opt.value), 20, "option must contain new value");
        assert_eq!(opt.tag, TAG_VARIANT_1);
    }

    #[test]
    fn replace_i32_on_none_returns_none_unchanged() {
        // Catches: replace writing value into None variant
        let mut opt = hew_option_none();
        let old = hew_option_replace_i32(&raw mut opt, 42);
        assert_eq!(old.tag, TAG_VARIANT_0, "old must be None");
        assert_eq!(opt.tag, TAG_VARIANT_0, "option must remain None");
    }

    // -- take --

    #[test]
    fn take_from_some_returns_value_and_leaves_none() {
        // Catches: not clearing the source, or returning None instead of old value
        let mut opt = hew_option_some_i32(77);
        let taken = hew_option_take(&raw mut opt);
        assert_eq!(taken.tag, TAG_VARIANT_1);
        assert_eq!(decode_i32(taken.value), 77);
        assert_eq!(opt.tag, TAG_VARIANT_0, "source must be None after take");
        assert_eq!(opt.value, 0);
    }

    #[test]
    fn take_from_none_returns_none() {
        // Catches: take panicking or changing None to Some
        let mut opt = hew_option_none();
        let taken = hew_option_take(&raw mut opt);
        assert_eq!(taken.tag, TAG_VARIANT_0);
        assert_eq!(opt.tag, TAG_VARIANT_0);
    }
}
