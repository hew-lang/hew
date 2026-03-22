//! Hew runtime: `result` module.
//!
//! Tagged union for `Result<T, E>` — either `Ok(value)` (tag=0) or `Err(msg)` (tag=1).
//! Layout-compatible with the C runtime representation used by MLIR codegen.
//! Error payloads carry both a numeric code and a heap-allocated message string.

use std::ffi::{c_char, c_void};
use std::ptr;

use crate::tagged_union::{
    decode_f64, decode_i32, decode_i64, encode_f64, encode_i32, encode_i64, is_variant_0,
    is_variant_1, TAG_VARIANT_0, TAG_VARIANT_1,
};

/// ABI-stable `Result<T, E>` representation.
///
/// `tag == 0` → Ok, `tag == 1` → Err.
/// For Ok: `value` holds the payload (i32/i64/f64/ptr as u64).
/// For Err: `error_code` holds a numeric code, `error_msg` is a heap-allocated
/// null-terminated C string (owned, freed on drop).
#[repr(C)]
#[derive(Debug)]
pub struct HewResult {
    /// 0 = Ok, 1 = Err
    pub tag: i32,
    /// Error code (only meaningful when `tag == 1`).
    pub error_code: i32,
    /// Ok payload (interpreted based on element type).
    pub value: u64,
    /// Error message (heap-allocated C string, only when `tag == 1`).
    pub error_msg: *mut c_char,
}

// ---------------------------------------------------------------------------
// Constructors — Ok
// ---------------------------------------------------------------------------

/// Create an `Ok(i32)` result.
#[no_mangle]
pub extern "C" fn hew_result_ok_i32(val: i32) -> HewResult {
    HewResult {
        tag: TAG_VARIANT_0,
        error_code: 0,
        value: encode_i32(val),
        error_msg: ptr::null_mut(),
    }
}

/// Create an `Ok(i64)` result.
#[no_mangle]
pub extern "C" fn hew_result_ok_i64(val: i64) -> HewResult {
    HewResult {
        tag: TAG_VARIANT_0,
        error_code: 0,
        value: encode_i64(val),
        error_msg: ptr::null_mut(),
    }
}

/// Create an `Ok(f64)` result.
#[no_mangle]
pub extern "C" fn hew_result_ok_f64(val: f64) -> HewResult {
    HewResult {
        tag: TAG_VARIANT_0,
        error_code: 0,
        value: encode_f64(val),
        error_msg: ptr::null_mut(),
    }
}

// ---------------------------------------------------------------------------
// Constructors — Err
// ---------------------------------------------------------------------------

/// Create an `Err` result with a numeric code and message string.
///
/// The message is duplicated via `strdup`; the caller retains ownership of `msg`.
///
/// # Safety
///
/// `msg` must be a valid null-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_result_err(code: i32, msg: *const c_char) -> HewResult {
    let owned_msg = if msg.is_null() {
        ptr::null_mut()
    } else {
        // SAFETY: caller guarantees `msg` is a valid C string.
        unsafe { libc::strdup(msg) }
    };
    HewResult {
        tag: TAG_VARIANT_1,
        error_code: code,
        value: 0,
        error_msg: owned_msg,
    }
}

/// Create an `Err` result with just a numeric code (no message).
#[no_mangle]
pub extern "C" fn hew_result_err_code(code: i32) -> HewResult {
    HewResult {
        tag: TAG_VARIANT_1,
        error_code: code,
        value: 0,
        error_msg: ptr::null_mut(),
    }
}

// ---------------------------------------------------------------------------
// Predicates
// ---------------------------------------------------------------------------

/// Returns 1 if the result is `Ok`, 0 if `Err`.
///
/// # Safety
///
/// `res` must be a valid pointer to a `HewResult`.
#[no_mangle]
#[expect(
    clippy::not_unsafe_ptr_arg_deref,
    reason = "C ABI function — caller guarantees pointer validity"
)]
pub extern "C" fn hew_result_is_ok(res: *const HewResult) -> i32 {
    // SAFETY: caller guarantees `res` is valid.
    let r = unsafe { &*res };
    i32::from(is_variant_0(r.tag))
}

/// Returns 1 if the result is `Err`, 0 if `Ok`.
///
/// # Safety
///
/// `res` must be a valid pointer to a `HewResult`.
#[no_mangle]
#[expect(
    clippy::not_unsafe_ptr_arg_deref,
    reason = "C ABI function — caller guarantees pointer validity"
)]
pub extern "C" fn hew_result_is_err(res: *const HewResult) -> i32 {
    // SAFETY: caller guarantees `res` is valid.
    let r = unsafe { &*res };
    i32::from(is_variant_1(r.tag))
}

// ---------------------------------------------------------------------------
// Unwrap — Ok values
// ---------------------------------------------------------------------------

/// Unwrap the i32 value. Aborts with error message if `Err`.
///
/// # Safety
///
/// `res` must be a valid pointer to a `HewResult`.
#[no_mangle]
#[expect(
    clippy::not_unsafe_ptr_arg_deref,
    reason = "C ABI function — caller guarantees pointer validity"
)]
pub extern "C" fn hew_result_unwrap_i32(res: *const HewResult) -> i32 {
    // SAFETY: caller guarantees `res` is valid.
    let r = unsafe { &*res };
    if is_variant_1(r.tag) {
        abort_with_error(r);
    }
    decode_i32(r.value)
}

/// Unwrap the i64 value. Aborts with error message if `Err`.
///
/// # Safety
///
/// `res` must be a valid pointer to a `HewResult`.
#[no_mangle]
#[expect(
    clippy::not_unsafe_ptr_arg_deref,
    reason = "C ABI function — caller guarantees pointer validity"
)]
pub extern "C" fn hew_result_unwrap_i64(res: *const HewResult) -> i64 {
    // SAFETY: caller guarantees `res` is valid.
    let r = unsafe { &*res };
    if is_variant_1(r.tag) {
        abort_with_error(r);
    }
    decode_i64(r.value)
}

/// Unwrap the f64 value. Aborts with error message if `Err`.
///
/// # Safety
///
/// `res` must be a valid pointer to a `HewResult`.
#[no_mangle]
#[expect(
    clippy::not_unsafe_ptr_arg_deref,
    reason = "C ABI function — caller guarantees pointer validity"
)]
pub extern "C" fn hew_result_unwrap_f64(res: *const HewResult) -> f64 {
    // SAFETY: caller guarantees `res` is valid.
    let r = unsafe { &*res };
    if is_variant_1(r.tag) {
        abort_with_error(r);
    }
    decode_f64(r.value)
}

// ---------------------------------------------------------------------------
// Error accessors
// ---------------------------------------------------------------------------

/// Get the error code from an `Err` result. Returns 0 if `Ok`.
///
/// # Safety
///
/// `res` must be a valid pointer to a `HewResult`.
#[no_mangle]
#[expect(
    clippy::not_unsafe_ptr_arg_deref,
    reason = "C ABI function — caller guarantees pointer validity"
)]
pub extern "C" fn hew_result_error_code(res: *const HewResult) -> i32 {
    // SAFETY: caller guarantees `res` is valid.
    let r = unsafe { &*res };
    if is_variant_0(r.tag) {
        0
    } else {
        r.error_code
    }
}

/// Get the error message from an `Err` result. Returns null if `Ok` or no message.
///
/// The returned pointer is borrowed — do not free it.
///
/// # Safety
///
/// `res` must be a valid pointer to a `HewResult`.
#[no_mangle]
#[expect(
    clippy::not_unsafe_ptr_arg_deref,
    reason = "C ABI function — caller guarantees pointer validity"
)]
pub extern "C" fn hew_result_error_msg(res: *const HewResult) -> *const c_char {
    // SAFETY: caller guarantees `res` is valid.
    let r = unsafe { &*res };
    if is_variant_0(r.tag) {
        ptr::null()
    } else {
        r.error_msg.cast_const()
    }
}

// ---------------------------------------------------------------------------
// unwrap_or (default fallback)
// ---------------------------------------------------------------------------

/// Unwrap i32 or return `default` if `Err`.
///
/// # Safety
///
/// `res` must be a valid pointer to a `HewResult`.
#[no_mangle]
#[expect(
    clippy::not_unsafe_ptr_arg_deref,
    reason = "C ABI function — caller guarantees pointer validity"
)]
pub extern "C" fn hew_result_unwrap_or_i32(res: *const HewResult, default: i32) -> i32 {
    // SAFETY: caller guarantees `res` is valid.
    let r = unsafe { &*res };
    if is_variant_0(r.tag) {
        decode_i32(r.value)
    } else {
        default
    }
}

/// Unwrap i64 or return `default` if `Err`.
///
/// # Safety
///
/// `res` must be a valid pointer to a `HewResult`.
#[no_mangle]
#[expect(
    clippy::not_unsafe_ptr_arg_deref,
    reason = "C ABI function — caller guarantees pointer validity"
)]
pub extern "C" fn hew_result_unwrap_or_i64(res: *const HewResult, default: i64) -> i64 {
    // SAFETY: caller guarantees `res` is valid.
    let r = unsafe { &*res };
    if is_variant_0(r.tag) {
        decode_i64(r.value)
    } else {
        default
    }
}

// ---------------------------------------------------------------------------
// Cleanup
// ---------------------------------------------------------------------------

/// Free the error message. Call when a `HewResult` is no longer needed.
///
/// # Safety
///
/// Must only be called once per result. The result must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_result_free(res: *mut HewResult) {
    if res.is_null() {
        return;
    }
    // SAFETY: caller guarantees `res` is valid and not double-freed.
    let r = unsafe { &mut *res };
    if !r.error_msg.is_null() {
        // SAFETY: error_msg was allocated by libc::strdup.
        unsafe { libc::free(r.error_msg.cast::<c_void>()) };
        r.error_msg = ptr::null_mut();
    }
}

// ---------------------------------------------------------------------------
// map
// ---------------------------------------------------------------------------

/// Map an `Ok(i32)` result. Returns `Err` unchanged.
///
/// # Safety
///
/// `f` must be a valid function pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_result_map_i32(
    res: *const HewResult,
    f: unsafe extern "C" fn(i32) -> i32,
) -> HewResult {
    // SAFETY: caller guarantees `res` is valid.
    let r = unsafe { &*res };
    if is_variant_1(r.tag) {
        // Clone the Err — duplicate the message string.
        let cloned_msg = if r.error_msg.is_null() {
            ptr::null_mut()
        } else {
            // SAFETY: error_msg is a valid C string from strdup.
            unsafe { libc::strdup(r.error_msg) }
        };
        HewResult {
            tag: TAG_VARIANT_1,
            error_code: r.error_code,
            value: 0,
            error_msg: cloned_msg,
        }
    } else {
        // SAFETY: caller guarantees `f` is valid.
        let mapped = unsafe { f(decode_i32(r.value)) };
        HewResult {
            tag: TAG_VARIANT_0,
            error_code: 0,
            value: encode_i32(mapped),
            error_msg: ptr::null_mut(),
        }
    }
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

fn abort_with_error(r: &HewResult) -> ! {
    if r.error_msg.is_null() {
        eprintln!("hew: unwrap called on Err(code={})", r.error_code);
    } else {
        // SAFETY: error_msg was allocated by strdup, guaranteed null-terminated.
        let msg = unsafe { std::ffi::CStr::from_ptr(r.error_msg) };
        eprintln!(
            "hew: unwrap called on Err(code={}, msg={:?})",
            r.error_code, msg
        );
    }
    std::process::abort();
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    // -- Ok constructors + accessor round-trips --

    #[test]
    fn ok_i32_round_trips_value() {
        // Catches: encode/decode mismatch, truncation, wrong field stored in
        let res = hew_result_ok_i32(42);
        assert_eq!(decode_i32(res.value), 42);
    }

    #[test]
    fn ok_i32_negative_round_trips() {
        // Catches: sign-extension bug in encode_i32/decode_i32
        let res = hew_result_ok_i32(-1);
        assert_eq!(decode_i32(res.value), -1);
    }

    #[test]
    fn ok_i32_boundary_values() {
        // Catches: truncation at 32-bit boundaries
        for &val in &[i32::MIN, i32::MAX, 0] {
            let res = hew_result_ok_i32(val);
            assert_eq!(decode_i32(res.value), val, "failed for {val}");
        }
    }

    #[test]
    fn ok_i64_round_trips_value() {
        // Catches: i64 stored in u64 field incorrectly
        let res = hew_result_ok_i64(999_999_999_999);
        assert_eq!(decode_i64(res.value), 999_999_999_999);
    }

    #[test]
    fn ok_i64_boundary_values() {
        // Catches: sign-extension or truncation for extreme i64 values
        for &val in &[i64::MIN, i64::MAX, 0i64] {
            let res = hew_result_ok_i64(val);
            assert_eq!(decode_i64(res.value), val, "failed for {val}");
        }
    }

    #[test]
    fn ok_f64_round_trips_value() {
        // Catches: f64 bit representation mangled during encode/decode
        let res = hew_result_ok_f64(1.0 / 3.0);
        assert_eq!(decode_f64(res.value).to_bits(), (1.0_f64 / 3.0).to_bits());
    }

    #[test]
    fn ok_f64_special_values() {
        // Catches: NaN/Infinity bits lost in to_bits/from_bits round-trip
        let res_nan = hew_result_ok_f64(f64::NAN);
        assert!(decode_f64(res_nan.value).is_nan());

        let res_inf = hew_result_ok_f64(f64::INFINITY);
        assert!(decode_f64(res_inf.value).is_infinite());
        assert!(decode_f64(res_inf.value).is_sign_positive());

        let res_neg_inf = hew_result_ok_f64(f64::NEG_INFINITY);
        assert!(decode_f64(res_neg_inf.value).is_infinite());
        assert!(decode_f64(res_neg_inf.value).is_sign_negative());

        // Negative zero must preserve sign bit
        let res_neg_zero = hew_result_ok_f64(-0.0_f64);
        let decoded = decode_f64(res_neg_zero.value);
        assert!(decoded.is_sign_negative());
        assert_eq!(decoded.to_bits(), (-0.0_f64).to_bits());
    }

    // -- Ok tag is set correctly --

    #[test]
    fn ok_constructor_sets_tag_zero() {
        // Catches: wrong tag constant used in constructor
        let res = hew_result_ok_i32(1);
        assert_eq!(res.tag, TAG_VARIANT_0);
        assert!(res.error_msg.is_null());
        assert_eq!(res.error_code, 0);
    }

    // -- Err constructors --

    #[test]
    fn err_with_code_and_message() {
        // Catches: error_code or error_msg not stored, tag not set to Err
        let msg = CString::new("something went wrong").unwrap();
        // SAFETY: msg is a valid CString.
        let mut res = unsafe { hew_result_err(42, msg.as_ptr()) };
        assert_eq!(res.tag, TAG_VARIANT_1);
        assert_eq!(res.error_code, 42);
        assert!(!res.error_msg.is_null());
        // SAFETY: error_msg was allocated by strdup in the constructor.
        let stored = unsafe { std::ffi::CStr::from_ptr(res.error_msg) };
        assert_eq!(stored.to_str().unwrap(), "something went wrong");
        // SAFETY: res owns the error_msg, free it.
        unsafe { hew_result_free(&raw mut res) };
    }

    #[test]
    fn err_copies_message_independently() {
        // Catches: storing the original pointer instead of strdup'ing
        let msg = CString::new("original").unwrap();
        // SAFETY: msg is a valid CString.
        let mut res = unsafe { hew_result_err(1, msg.as_ptr()) };
        // The result's error_msg pointer must differ from the source.
        assert_ne!(res.error_msg.cast_const(), msg.as_ptr());
        // Content must still match.
        // SAFETY: error_msg was allocated by strdup.
        let stored = unsafe { std::ffi::CStr::from_ptr(res.error_msg) };
        assert_eq!(stored.to_str().unwrap(), "original");
        // SAFETY: free the owned message.
        unsafe { hew_result_free(&raw mut res) };
    }

    #[test]
    fn err_with_null_message() {
        // Catches: null dereference when msg is null
        // SAFETY: null is explicitly allowed by the API contract.
        let res = unsafe { hew_result_err(7, ptr::null()) };
        assert_eq!(res.tag, TAG_VARIANT_1);
        assert_eq!(res.error_code, 7);
        assert!(res.error_msg.is_null());
    }

    #[test]
    fn err_code_only_no_message() {
        // Catches: hew_result_err_code setting wrong tag or leaking memory
        let res = hew_result_err_code(99);
        assert_eq!(res.tag, TAG_VARIANT_1);
        assert_eq!(res.error_code, 99);
        assert!(res.error_msg.is_null());
        assert_eq!(res.value, 0);
    }

    // -- Predicates --

    #[test]
    fn is_ok_true_for_ok_variant() {
        // Catches: predicate checking wrong tag value
        let res = hew_result_ok_i32(0);
        assert_eq!(hew_result_is_ok(&raw const res), 1);
        assert_eq!(hew_result_is_err(&raw const res), 0);
    }

    #[test]
    fn is_err_true_for_err_variant() {
        // Catches: predicates swapped (is_ok returns 1 for Err)
        let res = hew_result_err_code(1);
        assert_eq!(hew_result_is_ok(&raw const res), 0);
        assert_eq!(hew_result_is_err(&raw const res), 1);
    }

    // -- Error accessors --

    #[test]
    fn error_code_returns_zero_for_ok() {
        // Catches: returning garbage error_code from Ok variant
        let res = hew_result_ok_i32(100);
        assert_eq!(hew_result_error_code(&raw const res), 0);
    }

    #[test]
    fn error_code_returns_code_for_err() {
        // Catches: returning 0 instead of the actual error code
        let res = hew_result_err_code(55);
        assert_eq!(hew_result_error_code(&raw const res), 55);
    }

    #[test]
    fn error_msg_returns_null_for_ok() {
        // Catches: returning dangling pointer from Ok variant
        let res = hew_result_ok_i64(1);
        assert!(hew_result_error_msg(&raw const res).is_null());
    }

    #[test]
    fn error_msg_returns_message_for_err() {
        // Catches: returning null when a message exists
        let msg = CString::new("oops").unwrap();
        // SAFETY: msg is valid.
        let mut res = unsafe { hew_result_err(1, msg.as_ptr()) };
        let ptr = hew_result_error_msg(&raw const res);
        assert!(!ptr.is_null());
        // SAFETY: ptr borrows from res.error_msg which is valid.
        let s = unsafe { std::ffi::CStr::from_ptr(ptr) };
        assert_eq!(s.to_str().unwrap(), "oops");
        // SAFETY: free the owned message.
        unsafe { hew_result_free(&raw mut res) };
    }

    // -- Unwrap on Ok --

    #[test]
    fn unwrap_i32_returns_value_for_ok() {
        // Catches: unwrap returning wrong value or aborting on Ok
        let res = hew_result_ok_i32(-42);
        assert_eq!(hew_result_unwrap_i32(&raw const res), -42);
    }

    #[test]
    fn unwrap_i64_returns_value_for_ok() {
        let res = hew_result_ok_i64(i64::MAX);
        assert_eq!(hew_result_unwrap_i64(&raw const res), i64::MAX);
    }

    #[test]
    fn unwrap_f64_returns_value_for_ok() {
        let res = hew_result_ok_f64(1.0 / 3.0);
        let got = hew_result_unwrap_f64(&raw const res);
        assert_eq!(got.to_bits(), (1.0_f64 / 3.0).to_bits());
    }

    // -- unwrap_or --

    #[test]
    fn unwrap_or_i32_returns_value_when_ok() {
        // Catches: always returning default regardless of tag
        let res = hew_result_ok_i32(7);
        assert_eq!(hew_result_unwrap_or_i32(&raw const res, 999), 7);
    }

    #[test]
    fn unwrap_or_i32_returns_default_when_err() {
        // Catches: returning garbage from value field instead of default
        let res = hew_result_err_code(1);
        assert_eq!(hew_result_unwrap_or_i32(&raw const res, -1), -1);
    }

    #[test]
    fn unwrap_or_i64_returns_value_when_ok() {
        let res = hew_result_ok_i64(123_456);
        assert_eq!(hew_result_unwrap_or_i64(&raw const res, 0), 123_456);
    }

    #[test]
    fn unwrap_or_i64_returns_default_when_err() {
        let res = hew_result_err_code(2);
        assert_eq!(hew_result_unwrap_or_i64(&raw const res, -999), -999);
    }

    // -- unwrap abort behaviour --

    #[test]
    fn unwrap_i32_aborts_on_err() {
        // Catches: unwrap returning garbage instead of aborting on Err
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "result::tests::_helper_unwrap_i32_err",
                "--include-ignored",
            ])
            .env("RUST_TEST_THREADS", "1")
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "unwrap on Err must terminate abnormally"
        );
    }

    #[test]
    #[ignore = "subprocess helper for unwrap_i32_aborts_on_err death test"]
    fn _helper_unwrap_i32_err() {
        let res = hew_result_err_code(1);
        let _ = hew_result_unwrap_i32(&raw const res);
    }

    // -- hew_result_free --

    #[test]
    fn free_null_is_safe() {
        // Catches: null dereference in free path
        // SAFETY: null is explicitly handled by hew_result_free.
        unsafe { hew_result_free(ptr::null_mut()) };
    }

    #[test]
    fn free_ok_result_is_safe() {
        // Catches: free attempting to free null error_msg pointer
        let mut res = hew_result_ok_i32(1);
        // SAFETY: res is a valid stack-allocated HewResult with null error_msg.
        unsafe { hew_result_free(&raw mut res) };
    }

    #[test]
    fn free_err_clears_message_pointer() {
        // Catches: double-free if called twice (pointer not nulled)
        let msg = CString::new("test").unwrap();
        // SAFETY: msg is valid.
        let mut res = unsafe { hew_result_err(1, msg.as_ptr()) };
        assert!(!res.error_msg.is_null());
        // SAFETY: res owns the message.
        unsafe { hew_result_free(&raw mut res) };
        assert!(res.error_msg.is_null(), "free must null out the pointer");
    }

    // -- map --

    unsafe extern "C" fn double_i32(x: i32) -> i32 {
        x * 2
    }

    #[test]
    fn map_i32_applies_function_to_ok() {
        // Catches: map not calling function, or storing unmapped value
        let res = hew_result_ok_i32(21);
        // SAFETY: double_i32 is a valid function pointer, res is valid.
        let mapped = unsafe { hew_result_map_i32(&raw const res, double_i32) };
        assert_eq!(mapped.tag, TAG_VARIANT_0);
        assert_eq!(decode_i32(mapped.value), 42);
    }

    #[test]
    fn map_i32_preserves_err() {
        // Catches: map calling function on Err, or losing error info
        let msg = CString::new("fail").unwrap();
        // SAFETY: msg is valid.
        let mut res = unsafe { hew_result_err(5, msg.as_ptr()) };
        // SAFETY: double_i32 is valid, res is valid.
        let mut mapped = unsafe { hew_result_map_i32(&raw const res, double_i32) };
        assert_eq!(mapped.tag, TAG_VARIANT_1);
        assert_eq!(mapped.error_code, 5);
        // The mapped error must have its own copy of the message.
        assert!(!mapped.error_msg.is_null());
        assert_ne!(mapped.error_msg, res.error_msg);
        // SAFETY: both error_msgs are valid strdup'd strings.
        let mapped_msg = unsafe { std::ffi::CStr::from_ptr(mapped.error_msg) };
        assert_eq!(mapped_msg.to_str().unwrap(), "fail");
        // SAFETY: Clean up both owned messages.
        unsafe {
            hew_result_free(&raw mut res);
            hew_result_free(&raw mut mapped);
        }
    }

    #[test]
    fn map_i32_err_with_null_message() {
        // Catches: strdup on null in map's Err branch
        let res = hew_result_err_code(3);
        // SAFETY: double_i32 is valid, res is valid.
        let mapped = unsafe { hew_result_map_i32(&raw const res, double_i32) };
        assert_eq!(mapped.tag, TAG_VARIANT_1);
        assert_eq!(mapped.error_code, 3);
        assert!(mapped.error_msg.is_null());
    }
}
