//! Hew runtime: `result` module.
//!
//! Tagged union for `Result<T, E>` — either `Ok(value)` (tag=0) or `Err(msg)` (tag=1).
//! Layout-compatible with the C runtime representation used by MLIR codegen.
//! Error payloads carry both a numeric code and a heap-allocated message string.

use std::ffi::{c_char, c_void};
use std::ptr;

use crate::tagged_union::{
    decode_f64, decode_i32, decode_i64, decode_mut_ptr, encode_f64, encode_i32, encode_i64,
    encode_ptr, is_variant_0, is_variant_1, TAG_VARIANT_0, TAG_VARIANT_1,
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

/// Create an `Ok(ptr)` result.
///
/// # Safety
///
/// `val` must be a valid pointer or null.
#[no_mangle]
pub unsafe extern "C" fn hew_result_ok_ptr(val: *mut c_void) -> HewResult {
    HewResult {
        tag: TAG_VARIANT_0,
        error_code: 0,
        value: encode_ptr(val),
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

/// Unwrap the pointer value. Aborts with error message if `Err`.
///
/// # Safety
///
/// Caller must ensure the original pointer is still valid.
#[no_mangle]
pub unsafe extern "C" fn hew_result_unwrap_ptr(res: *const HewResult) -> *mut c_void {
    // SAFETY: caller guarantees `res` is valid.
    let r = unsafe { &*res };
    if is_variant_1(r.tag) {
        abort_with_error(r);
    }
    decode_mut_ptr(r.value)
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
