//! Hew runtime: `option` module.
//!
//! Tagged union for `Option<T>` — either `None` (tag=0) or `Some(value)` (tag=1).
//! Layout-compatible with the C runtime representation used by MLIR codegen.

use std::ffi::{c_char, c_void};

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
        tag: 0,
        _pad: 0,
        value: 0,
    }
}

/// Create a `Some(i32)` option.
#[no_mangle]
#[expect(
    clippy::cast_sign_loss,
    reason = "bit-preserving storage of i32 in u64 union field"
)]
pub extern "C" fn hew_option_some_i32(val: i32) -> HewOption {
    HewOption {
        tag: 1,
        _pad: 0,
        value: val as u64,
    }
}

/// Create a `Some(i64)` option.
#[no_mangle]
#[expect(
    clippy::cast_sign_loss,
    reason = "bit-preserving storage of i64 in u64 union field"
)]
pub extern "C" fn hew_option_some_i64(val: i64) -> HewOption {
    HewOption {
        tag: 1,
        _pad: 0,
        value: val as u64,
    }
}

/// Create a `Some(f64)` option.
#[no_mangle]
pub extern "C" fn hew_option_some_f64(val: f64) -> HewOption {
    HewOption {
        tag: 1,
        _pad: 0,
        value: val.to_bits(),
    }
}

/// Create a `Some(ptr)` option.
///
/// # Safety
///
/// `val` must be a valid pointer or null. The caller is responsible for
/// the lifetime of the pointed-to data.
#[no_mangle]
pub unsafe extern "C" fn hew_option_some_ptr(val: *mut c_void) -> HewOption {
    HewOption {
        tag: 1,
        _pad: 0,
        value: val as u64,
    }
}

// ---------------------------------------------------------------------------
// Predicates
// ---------------------------------------------------------------------------

/// Returns 1 if the option is `Some`, 0 if `None`.
#[no_mangle]
pub extern "C" fn hew_option_is_some(opt: HewOption) -> i32 {
    i32::from(opt.tag == 1)
}

/// Returns 1 if the option is `None`, 0 if `Some`.
#[no_mangle]
pub extern "C" fn hew_option_is_none(opt: HewOption) -> i32 {
    i32::from(opt.tag == 0)
}

// ---------------------------------------------------------------------------
// Unwrap
// ---------------------------------------------------------------------------

/// Unwrap the i32 value. Aborts if `None`.
#[no_mangle]
#[expect(
    clippy::cast_possible_truncation,
    reason = "retrieving i32 from u64 union field — was stored as i32"
)]
pub extern "C" fn hew_option_unwrap_i32(opt: HewOption) -> i32 {
    if opt.tag == 0 {
        eprintln!("hew: unwrap called on None");
        std::process::abort();
    }
    opt.value as i32
}

/// Unwrap the i64 value. Aborts if `None`.
#[no_mangle]
#[expect(
    clippy::cast_possible_wrap,
    reason = "retrieving i64 from u64 union field — was stored as i64"
)]
pub extern "C" fn hew_option_unwrap_i64(opt: HewOption) -> i64 {
    if opt.tag == 0 {
        eprintln!("hew: unwrap called on None");
        std::process::abort();
    }
    opt.value as i64
}

/// Unwrap the f64 value. Aborts if `None`.
#[no_mangle]
pub extern "C" fn hew_option_unwrap_f64(opt: HewOption) -> f64 {
    if opt.tag == 0 {
        eprintln!("hew: unwrap called on None");
        std::process::abort();
    }
    f64::from_bits(opt.value)
}

/// Unwrap the pointer value. Aborts if `None`.
///
/// # Safety
///
/// Caller must ensure the original pointer is still valid.
#[no_mangle]
pub unsafe extern "C" fn hew_option_unwrap_ptr(opt: HewOption) -> *mut c_void {
    if opt.tag == 0 {
        eprintln!("hew: unwrap called on None");
        std::process::abort();
    }
    // SAFETY: caller guarantees the stored pointer is valid.
    opt.value as *mut c_void
}

// ---------------------------------------------------------------------------
// unwrap_or (default fallback)
// ---------------------------------------------------------------------------

/// Unwrap i32 or return `default` if `None`.
#[no_mangle]
#[expect(
    clippy::cast_possible_truncation,
    reason = "retrieving i32 from u64 union field — was stored as i32"
)]
pub extern "C" fn hew_option_unwrap_or_i32(opt: HewOption, default: i32) -> i32 {
    if opt.tag == 0 {
        default
    } else {
        opt.value as i32
    }
}

/// Unwrap i64 or return `default` if `None`.
#[no_mangle]
#[expect(
    clippy::cast_possible_wrap,
    reason = "retrieving i64 from u64 union field — was stored as i64"
)]
pub extern "C" fn hew_option_unwrap_or_i64(opt: HewOption, default: i64) -> i64 {
    if opt.tag == 0 {
        default
    } else {
        opt.value as i64
    }
}

/// Unwrap f64 or return `default` if `None`.
#[no_mangle]
pub extern "C" fn hew_option_unwrap_or_f64(opt: HewOption, default: f64) -> f64 {
    if opt.tag == 0 {
        default
    } else {
        f64::from_bits(opt.value)
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
    if opt.tag == 0 {
        default
    } else {
        // SAFETY: caller guarantees stored pointer validity.
        opt.value as *mut c_void
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
#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    reason = "i32 ↔ u64 round-trip through tagged union field"
)]
pub unsafe extern "C" fn hew_option_map_i32(
    opt: HewOption,
    f: unsafe extern "C" fn(i32) -> i32,
) -> HewOption {
    if opt.tag == 0 {
        opt
    } else {
        // SAFETY: caller guarantees `f` is valid.
        let result = unsafe { f(opt.value as i32) };
        HewOption {
            tag: 1,
            _pad: 0,
            value: result as u64,
        }
    }
}

/// Returns 1 if `Some` and the value equals `needle`, 0 otherwise.
#[no_mangle]
#[expect(
    clippy::cast_possible_truncation,
    reason = "retrieving i32 from u64 union field — was stored as i32"
)]
pub extern "C" fn hew_option_contains_i32(opt: HewOption, needle: i32) -> i32 {
    if opt.tag == 0 {
        0
    } else {
        i32::from(opt.value as i32 == needle)
    }
}

/// Returns 1 if `Some` and the string equals `needle`, 0 otherwise.
///
/// # Safety
///
/// Both the stored pointer and `needle` must be valid C strings (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_option_contains_str(opt: HewOption, needle: *const c_char) -> i32 {
    if opt.tag == 0 {
        return 0;
    }
    let stored = opt.value as *const c_char;
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
    clippy::cast_sign_loss,
    reason = "C ABI function — caller guarantees pointer validity; bit-preserving i32→u64"
)]
pub extern "C" fn hew_option_replace_i32(opt: *mut HewOption, new_val: i32) -> HewOption {
    // SAFETY: caller guarantees `opt` points to a valid HewOption.
    let o = unsafe { &mut *opt };
    let old = *o;
    if o.tag == 1 {
        o.value = new_val as u64;
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
    o.tag = 0;
    o.value = 0;
    old
}
