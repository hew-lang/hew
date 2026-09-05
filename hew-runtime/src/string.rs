//! Hew runtime: `string` module.
//!
//! String operations exposed with C ABI for use by compiled Hew programs.
//! Every ordinary Hew string is an immutable, length-carrying, valid UTF-8
//! [`HewString`] allocation. Null is the canonical empty value. Owned handles
//! are released through [`hew_string_drop`], and [`hew_string_clone`] retains
//! the same immutable allocation. Foreign C strings cross only explicitly
//! named adapter boundaries; they are never accepted as managed handles.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::internal::types::HEW_TRAP_INDEX_OUT_OF_BOUNDS;
use crate::trap_code::{fmt_decimal_i64, fmt_decimal_usize, runtime_bounds_trap};
use hew_cabi::string::{
    string_alloc_utf8_unchecked, string_as_bytes, string_as_str, string_from_str, string_release,
    string_retain, HewString,
};
use std::ffi::{c_void, CStr};
use std::fmt::Write as _;
use std::os::raw::c_char;

pub type HewStructuralFormatFn = unsafe extern "C" fn(*mut c_void, *const c_void);

#[derive(Debug, Default)]
pub struct HewStringBuilder {
    bytes: Vec<u8>,
}

/// Initialize a compiler literal's output with one managed string owner.
/// Allocation failure aborts; this private entry point does not unwind.
///
/// # Safety
///
/// `data` must contain `len` readable UTF-8 bytes, including any embedded NUL,
/// or be null when
/// `len` is zero. `out` must be aligned, uniquely writable storage for an
/// uninitialized string pointer. Release the resulting owner with
/// [`hew_string_drop`].
#[no_mangle]
pub unsafe extern "C" fn hew_string_literal_new(
    data: *const u8,
    len: u32,
    out: *mut *mut HewString,
) {
    // SAFETY: compiler literals are valid UTF-8 and readable for `len` bytes.
    let value = unsafe { string_alloc_utf8_unchecked(data, len as usize) };
    // SAFETY: the caller supplies unique output storage, not an existing owner.
    unsafe { out.write(value) };
}

/// Compiler-intercept sentinel. Generated code lowers this symbol to typed
/// formatter thunks before link time; a direct call is always a compiler bug.
///
/// # Safety
///
/// This function must never be called; it aborts unconditionally.
#[no_mangle]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "the sentinel's only operation is unconditional process abort"
)]
pub unsafe extern "C" fn hew_structural_format(_value: *const c_void) -> *mut c_char {
    unsafe { libc::abort() }
}

#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "callers preserve the builder allocation for the complete formatting traversal"
)]
pub(crate) unsafe fn structural_builder_append(builder: *mut c_void, bytes: &[u8]) {
    if builder.is_null() {
        unsafe { libc::abort() };
    }
    let builder = unsafe { &mut *builder.cast::<HewStringBuilder>() };
    builder.bytes.extend_from_slice(bytes);
}

#[no_mangle]
pub extern "C" fn hew_string_builder_new() -> *mut c_void {
    Box::into_raw(Box::new(HewStringBuilder::default())).cast()
}

/// Append a borrowed managed Hew string.
///
/// # Safety
///
/// `builder` must be live and `value` must be a managed string handle.
#[no_mangle]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "the function validates null and borrows both inputs for the call"
)]
pub unsafe extern "C" fn hew_string_builder_append_string(
    builder: *mut c_void,
    value: *const HewString,
) {
    // SAFETY: the caller supplies a live managed value; null is canonical empty.
    let bytes = unsafe { string_as_bytes(value) };
    unsafe { structural_builder_append(builder, bytes) };
}

/// Append a signed integer.
///
/// # Safety
///
/// `builder` must be a live structural string builder.
#[no_mangle]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "the builder pointer is valid by the FFI contract"
)]
pub unsafe extern "C" fn hew_string_builder_append_i64(builder: *mut c_void, value: i64) {
    let mut text = String::new();
    let _ = write!(text, "{value}");
    unsafe { structural_builder_append(builder, text.as_bytes()) };
}

/// Append an unsigned integer.
///
/// # Safety
///
/// `builder` must be a live structural string builder.
#[no_mangle]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "the builder pointer is valid by the FFI contract"
)]
pub unsafe extern "C" fn hew_string_builder_append_u64(builder: *mut c_void, value: u64) {
    let mut text = String::new();
    let _ = write!(text, "{value}");
    unsafe { structural_builder_append(builder, text.as_bytes()) };
}

/// Append a floating-point value.
///
/// # Safety
///
/// `builder` must be a live structural string builder.
#[no_mangle]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "the builder pointer is valid by the FFI contract"
)]
pub unsafe extern "C" fn hew_string_builder_append_f64(builder: *mut c_void, value: f64) {
    let mut text = String::new();
    let _ = write!(text, "{value}");
    unsafe { structural_builder_append(builder, text.as_bytes()) };
}

/// Append a Hew boolean byte.
///
/// # Safety
///
/// `builder` must be a live structural string builder.
#[no_mangle]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "the builder pointer is valid by the FFI contract"
)]
pub unsafe extern "C" fn hew_string_builder_append_bool(builder: *mut c_void, value: u8) {
    unsafe {
        structural_builder_append(builder, if value == 0 { b"false" } else { b"true" });
    }
}

/// Append a Unicode scalar value.
///
/// # Safety
///
/// `builder` must be a live structural string builder.
#[no_mangle]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "the builder pointer is valid by the FFI contract"
)]
pub unsafe extern "C" fn hew_string_builder_append_char(builder: *mut c_void, value: u32) {
    let ch = char::from_u32(value).unwrap_or(char::REPLACEMENT_CHARACTER);
    let mut bytes = [0u8; 4];
    unsafe { structural_builder_append(builder, ch.encode_utf8(&mut bytes).as_bytes()) };
}

/// Append an opaque identity without exposing its representation bits.
///
/// # Safety
///
/// `builder` must be live and `type_name` must be null or NUL-terminated.
#[no_mangle]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "the function borrows the validated builder and optional C string"
)]
pub unsafe extern "C" fn hew_string_builder_append_identity(
    builder: *mut c_void,
    type_name: *const c_char,
    identity: *const c_void,
) {
    let name = if type_name.is_null() {
        "<opaque>"
    } else {
        unsafe { CStr::from_ptr(type_name) }
            .to_str()
            .unwrap_or("<opaque>")
    };
    let mut text = String::new();
    let _ = write!(text, "<{name}@{identity:p}>");
    unsafe { structural_builder_append(builder, text.as_bytes()) };
}

/// Finish the builder and transfer one owned Hew string to the caller.
///
/// # Safety
///
/// `builder` must be a live pointer returned by [`hew_string_builder_new`] and
/// must not be used again after this call.
#[no_mangle]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "this boundary consumes the unique builder allocation and initializes the returned managed string"
)]
pub unsafe extern "C" fn hew_string_builder_finish(builder: *mut c_void) -> *mut HewString {
    if builder.is_null() {
        unsafe { libc::abort() };
    }
    let builder = unsafe { Box::from_raw(builder.cast::<HewStringBuilder>()) };
    let Ok(text) = std::str::from_utf8(&builder.bytes) else {
        unsafe { libc::abort() };
    };
    string_from_str(text)
}

/// Write a message to stderr.
///
/// # Safety
///
/// `msg` must be valid for reads for its full length.
unsafe fn write_stderr(msg: &[u8]) {
    // SAFETY: msg.as_ptr() is valid for msg.len() bytes, and fd 2 is stderr.
    unsafe {
        #[cfg(not(target_os = "windows"))]
        libc::write(2, msg.as_ptr().cast(), msg.len());
        #[cfg(target_os = "windows")]
        libc::write(2, msg.as_ptr().cast(), msg.len() as core::ffi::c_uint);
    }
}

/// Emit a string bounds diagnostic and route through the trap seam.
///
/// # Safety
///
/// Call only from a fail-closed string bounds path.
unsafe fn string_bounds_trap(message: &str) -> ! {
    // SAFETY: writing the diagnostic and trapping is the terminal failure path.
    unsafe {
        write_stderr(message.as_bytes());
        runtime_bounds_trap(HEW_TRAP_INDEX_OUT_OF_BOUNDS);
    }
}

/// Concatenate two managed strings. Caller owns the result.
///
/// # Safety
///
/// Both handles must be null or live managed strings.
#[no_mangle]
pub unsafe extern "C" fn hew_string_concat(
    a: *const HewString,
    b: *const HewString,
) -> *mut HewString {
    // SAFETY: both arguments satisfy the managed-handle contract.
    let (a, b) = unsafe { (string_as_str(a), string_as_str(b)) };
    let Some(capacity) = a.len().checked_add(b.len()) else {
        std::process::abort();
    };
    let mut joined = String::with_capacity(capacity);
    joined.push_str(a);
    joined.push_str(b);
    string_from_str(&joined)
}

/// Extract a substring by codepoint range `[start, end)`. The caller owns the result.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_slice(
    s: *const HewString,
    start: i64,
    end: i64,
) -> *mut HewString {
    // SAFETY: `s` is a live managed string handle; null is empty.
    let text = unsafe { string_as_str(s) };
    let len = i64::try_from(text.chars().count()).unwrap_or_else(|_| std::process::abort());
    let start = start.max(0);
    let end = end.min(len);
    if start >= end {
        return core::ptr::null_mut();
    }
    let (Ok(start), Ok(count)) = (usize::try_from(start), usize::try_from(end - start)) else {
        return core::ptr::null_mut();
    };
    let result: String = text.chars().skip(start).take(count).collect();
    string_from_str(&result)
}

/// Find the first occurrence of `substr` in `s`. Returns its codepoint offset or -1.
///
/// # Safety
///
/// Both arguments must be null or live managed string handles.
#[no_mangle]
pub unsafe extern "C" fn hew_string_find(s: *const HewString, substr: *const HewString) -> i64 {
    // SAFETY: both arguments are live managed string handles; null is empty.
    let (text, needle) = unsafe { (string_as_str(s), string_as_str(substr)) };
    let Some(byte_offset) = text.find(needle) else {
        return -1;
    };
    i64::try_from(text[..byte_offset].chars().count()).unwrap_or_else(|_| std::process::abort())
}

/// Check if `s` starts with `prefix`.
///
/// # Safety
///
/// Both arguments must be null or live managed string handles.
#[no_mangle]
pub unsafe extern "C" fn hew_string_starts_with(
    s: *const HewString,
    prefix: *const HewString,
) -> bool {
    // SAFETY: both arguments are live managed handles.
    unsafe { string_as_str(s) }.starts_with(unsafe { string_as_str(prefix) })
}

/// Check if `s` ends with `suffix`.
///
/// # Safety
///
/// Both arguments must be null or live managed string handles.
#[no_mangle]
pub unsafe extern "C" fn hew_string_ends_with(
    s: *const HewString,
    suffix: *const HewString,
) -> bool {
    // SAFETY: both arguments are live managed handles.
    unsafe { string_as_str(s) }.ends_with(unsafe { string_as_str(suffix) })
}

/// Check if `s` contains `substr`.
///
/// # Safety
///
/// Both arguments must be null or live managed string handles.
#[no_mangle]
pub unsafe extern "C" fn hew_string_contains(
    s: *const HewString,
    substr: *const HewString,
) -> bool {
    // SAFETY: both arguments are live managed handles.
    unsafe { string_as_str(s) }.contains(unsafe { string_as_str(substr) })
}

/// Check if all bytes in `s` are ASCII digits. Returns `false` for empty strings.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_is_digit(s: *const HewString) -> bool {
    // SAFETY: `s` is a live managed handle.
    let text = unsafe { string_as_str(s) };
    !text.is_empty() && text.chars().all(char::is_numeric)
}

/// Check if all bytes in `s` are ASCII alphabetic. Returns `false` for empty strings.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_is_alpha(s: *const HewString) -> bool {
    // SAFETY: `s` is a live managed handle.
    let text = unsafe { string_as_str(s) };
    !text.is_empty() && text.chars().all(char::is_alphabetic)
}

/// Check if all bytes in `s` are ASCII alphanumeric. Returns `false` for empty strings.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_is_alphanumeric(s: *const HewString) -> bool {
    // SAFETY: `s` is a live managed handle.
    let text = unsafe { string_as_str(s) };
    !text.is_empty() && text.chars().all(char::is_alphanumeric)
}

/// Check if a string is empty (zero length).
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_is_empty(s: *const HewString) -> bool {
    // SAFETY: `s` is a live managed handle; null is empty.
    unsafe { string_as_bytes(s) }.is_empty()
}

/// Convert an `i32` to its decimal string representation. Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_int_to_string(n: i32) -> *mut HewString {
    let mut buf = [0u8; 32];
    let len = {
        use std::io::Write;
        let mut w: &mut [u8] = &mut buf;
        let _ = write!(w, "{n}");
        32 - w.len()
    };
    // SAFETY: buf contains len valid UTF-8 bytes from write!.
    unsafe { string_alloc_utf8_unchecked(buf.as_ptr(), len) }
}

/// Convert a `u8` to its decimal string representation. Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_u8_to_string(n: u8) -> *mut HewString {
    let mut buf = [0u8; 4]; // max 3 digits for 0..=255
    let len = {
        use std::io::Write;
        let mut w: &mut [u8] = &mut buf;
        let _ = write!(w, "{n}");
        4 - w.len()
    };
    // SAFETY: buf contains len valid UTF-8 bytes from write!.
    unsafe { string_alloc_utf8_unchecked(buf.as_ptr(), len) }
}

/// Convert a `u32` to its decimal string representation. Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_uint_to_string(n: u32) -> *mut HewString {
    let mut buf = [0u8; 32];
    let len = {
        use std::io::Write;
        let mut w: &mut [u8] = &mut buf;
        let _ = write!(w, "{n}");
        32 - w.len()
    };
    // SAFETY: buf contains len valid UTF-8 bytes from write!.
    unsafe { string_alloc_utf8_unchecked(buf.as_ptr(), len) }
}

/// Convert an `i64` to its decimal string representation. Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_i64_to_string(n: i64) -> *mut HewString {
    let mut buf = [0u8; 32];
    let len = {
        use std::io::Write;
        let mut w: &mut [u8] = &mut buf;
        let _ = write!(w, "{n}");
        32 - w.len()
    };
    // SAFETY: buf contains len valid UTF-8 bytes from write!.
    unsafe { string_alloc_utf8_unchecked(buf.as_ptr(), len) }
}

/// Convert a `u64` to its decimal string representation. Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_u64_to_string(n: u64) -> *mut HewString {
    let mut buf = [0u8; 32];
    let len = {
        use std::io::Write;
        let mut w: &mut [u8] = &mut buf;
        let _ = write!(w, "{n}");
        32 - w.len()
    };
    // SAFETY: buf contains len valid UTF-8 bytes from write!.
    unsafe { string_alloc_utf8_unchecked(buf.as_ptr(), len) }
}

fn parse_strict_i64(bytes: &[u8]) -> i64 {
    std::str::from_utf8(bytes)
        .ok()
        .and_then(|s| s.parse::<i64>().ok())
        .unwrap_or(0)
}

/// Parse a managed string as an `int`/`i64` using `std::string.to_int` semantics.
///
/// # Safety
///
/// `s` must be null or a live managed string handle. Empty returns 0.
#[no_mangle]
pub unsafe extern "C" fn hew_string_to_int(s: *const HewString) -> i64 {
    // SAFETY: `s` is a live managed handle; null is empty.
    parse_strict_i64(unsafe { string_as_bytes(s) })
}

/// Convert an `f64` to its string representation. Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_float_to_string(f: f64) -> *mut HewString {
    // Match C's %g format: compact representation with scientific notation
    // for very large/small values, trailing zeros trimmed.
    unsafe extern "C" {
        fn snprintf(buf: *mut c_char, size: usize, fmt: *const c_char, ...) -> i32;
    }
    let f = crate::print::canonical_f64_for_render(f);
    let mut buf = [0u8; 64];
    // SAFETY: buf is large enough for any %g output. snprintf is available
    // on all platforms (MSVC CRT, glibc, musl).
    let len = unsafe {
        snprintf(
            buf.as_mut_ptr().cast::<c_char>(),
            buf.len(),
            c"%g".as_ptr(),
            f,
        )
    };
    if len < 0 {
        return std::ptr::null_mut();
    }
    #[expect(clippy::cast_sign_loss, reason = "len >= 0 checked above")]
    let len = (len as usize).min(buf.len());
    // SAFETY: buf contains len valid bytes from snprintf.
    unsafe { string_alloc_utf8_unchecked(buf.as_ptr(), len) }
}

/// Convert a `bool` to `"true"` or `"false"`. Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_bool_to_string(b: bool) -> *mut HewString {
    let s = if b { "true" } else { "false" };
    // SAFETY: s points to valid static string bytes with known length.
    string_from_str(s)
}

/// Trim leading and trailing ASCII whitespace. Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_trim(s: *const HewString) -> *mut HewString {
    // SAFETY: `s` is a live managed handle; null is empty.
    string_from_str(unsafe { string_as_str(s) }.trim())
}

/// Replace all occurrences of `old_str` with `new_str`. Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// All three arguments must be null or live managed string handles.
#[no_mangle]
pub unsafe extern "C" fn hew_string_replace(
    s: *const HewString,
    old_str: *const HewString,
    new_str: *const HewString,
) -> *mut HewString {
    // SAFETY: all arguments are live managed handles; null is empty.
    let (text, old, new) = unsafe {
        (
            string_as_str(s),
            string_as_str(old_str),
            string_as_str(new_str),
        )
    };
    if old.is_empty() {
        return string_from_str(text);
    }
    string_from_str(&text.replace(old, new))
}

/// Convert a Unicode codepoint to its UTF-8 string representation.
/// Returns a fresh managed string. The caller owns the result.
///
/// `c` is a Unicode scalar value passed as `i32` (codepoints fit in `[0, 0x10_FFFF]`).
/// Invalid codepoints (values that `char::from_u32` rejects) produce the replacement
/// character U+FFFD so the caller always receives a valid UTF-8 string.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions beyond a valid `i32`.
#[expect(
    clippy::cast_sign_loss,
    reason = "C ABI passes codepoints as i32; reinterpreting as u32 for char::from_u32 is correct"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_char_to_string(c: i32) -> *mut HewString {
    // Decode the Unicode scalar; fall back to U+FFFD for invalid codepoints.
    let ch = char::from_u32(c as u32).unwrap_or('\u{FFFD}');
    let mut buf = [0u8; 4];
    let encoded = ch.encode_utf8(&mut buf);
    let nbytes = encoded.len();
    // SAFETY: buf contains nbytes valid UTF-8 bytes for this codepoint; buf is alive
    // for the duration of the call.
    unsafe { string_alloc_utf8_unchecked(buf.as_ptr(), nbytes) }
}

/// Return the number of Unicode codepoints in a managed string.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_length(s: *const HewString) -> i64 {
    // SAFETY: the caller supplies a live managed string handle.
    let length = unsafe { string_as_str(s) }.chars().count();
    i64::try_from(length).unwrap_or_else(|_| std::process::abort())
}

/// Lexicographic comparison of two managed strings by their UTF-8 bytes.
/// Returns −1 if `a < b`, 0 if `a == b`, 1 if `a > b`.
/// Null is the canonical empty string.
///
/// # Safety
///
/// Both arguments must be null or live managed string handles.
#[no_mangle]
pub unsafe extern "C" fn hew_string_compare(a: *const HewString, b: *const HewString) -> i32 {
    // SAFETY: both arguments satisfy the managed-handle contract.
    match unsafe { string_as_bytes(a) }.cmp(unsafe { string_as_bytes(b) }) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}

/// Compare two managed strings for equality. Returns 1 if equal, 0 otherwise.
///
/// # Safety
///
/// Both arguments must be null or live managed string handles.
#[no_mangle]
pub unsafe extern "C" fn hew_string_equals(a: *const HewString, b: *const HewString) -> i32 {
    // SAFETY: both arguments satisfy the managed-handle contract.
    i32::from(unsafe { string_as_bytes(a) } == unsafe { string_as_bytes(b) })
}

/// FNV-1a-64 hash of a managed string's complete UTF-8 byte range.
///
/// This is the hash twin of [`hew_string_equals`], including bytes after an
/// embedded NUL. A null handle hashes like the empty byte range.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_hash_fnv1a(s: *const HewString) -> u64 {
    const FNV_OFFSET_64: u64 = 0xcbf2_9ce4_8422_2325;
    const FNV_PRIME_64: u64 = 0x0000_0100_0000_01b3;
    let mut h = FNV_OFFSET_64;
    // SAFETY: the caller supplies a live managed string handle.
    for &b in unsafe { string_as_bytes(s) } {
        h ^= u64::from(b);
        h = h.wrapping_mul(FNV_PRIME_64);
    }
    h
}

/// Split a string by `delim` into a `HewVec` of strings. Caller must free
/// the returned vec with [`crate::vec::hew_vec_free`].
///
/// # Safety
///
/// Both arguments must be null or live managed string handles.
#[no_mangle]
pub unsafe extern "C" fn hew_string_split(
    s: *const HewString,
    delim: *const HewString,
) -> *mut crate::vec::HewVec {
    // SAFETY: hew_vec_new_str has no preconditions.
    let v = unsafe { crate::vec::hew_vec_new_str() };
    // SAFETY: both arguments are live managed handles; null is empty.
    let (text, separator) = unsafe { (string_as_str(s), string_as_str(delim)) };
    let push = |part: &str| {
        let part = string_from_str(part);
        // SAFETY: the vec retains one owner and `part` is then released.
        unsafe {
            crate::vec::hew_vec_push_str(v, part);
            string_release(part);
        }
    };
    if separator.is_empty() {
        for ch in text.chars() {
            let mut bytes = [0_u8; 4];
            push(ch.encode_utf8(&mut bytes));
        }
    } else {
        for part in text.split(separator) {
            push(part);
        }
    }
    v
}

/// Split a string into lines (on `\n`), stripping `\r`. Returns a `HewVec` of
/// strings. Caller must free the returned vec with [`crate::vec::hew_vec_free`].
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_lines(s: *const HewString) -> *mut crate::vec::HewVec {
    // SAFETY: hew_vec_new_str has no preconditions.
    let v = unsafe { crate::vec::hew_vec_new_str() };
    // SAFETY: `s` is a live managed handle; null is empty.
    for line in unsafe { string_as_str(s) }.split('\n') {
        let line = line.strip_suffix('\r').unwrap_or(line);
        let part = string_from_str(line);
        // SAFETY: the vec retains one owner and `part` is then released.
        unsafe {
            crate::vec::hew_vec_push_str(v, part);
            string_release(part);
        }
    }
    v
}

/// Returns a `Vec<i32>` containing the Unicode scalar value of each character in `s`.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_chars(s: *const HewString) -> *mut crate::vec::HewVec {
    // SAFETY: hew_vec_new has no preconditions.
    let v = unsafe { crate::vec::hew_vec_new() };
    // SAFETY: `s` is a live managed handle; null is empty.
    let rust_str = unsafe { string_as_str(s) };
    for ch in rust_str.chars() {
        // SAFETY: v is a valid HewVec allocated above.
        // ch as i32: Unicode scalar values are ≤ 0x10_FFFF, which fits in i32.
        unsafe { crate::vec::hew_vec_push_i32(v, ch as i32) };
    }
    v
}

/// Join a `Vec<String>` into a single string with `sep` between elements.
/// Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// `v` must be a valid `HewVec` of managed string handles. `sep` must be null
/// or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_join_str(
    v: *mut crate::vec::HewVec,
    sep: *const HewString,
) -> *mut HewString {
    cabi_guard!(v.is_null(), core::ptr::null_mut());
    // SAFETY: v is a valid HewVec per caller contract.
    let len = unsafe { crate::vec::hew_vec_len(v) };
    if len == 0 {
        return core::ptr::null_mut();
    }
    // SAFETY: `sep` is a live managed handle; null is empty.
    let separator = unsafe { string_as_str(sep) };
    let mut result = String::new();
    for i in 0..len {
        // SAFETY: `v` is a live vector of string handles and `i` is in bounds.
        let s = unsafe { crate::vec::hew_vec_get_str(v, i) };
        if i != 0 {
            result.push_str(separator);
        }
        // SAFETY: the retained owner remains live while borrowed, then is released.
        unsafe {
            result.push_str(string_as_str(s));
            hew_string_drop(s.cast_mut());
        }
    }
    string_from_str(&result)
}

/// Convert a managed string to Unicode lowercase. The caller owns the result.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_to_lowercase(s: *const HewString) -> *mut HewString {
    // SAFETY: the caller supplies a live managed string handle.
    string_from_str(&unsafe { string_as_str(s) }.to_lowercase())
}

/// Convert a managed string to Unicode uppercase. The caller owns the result.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_to_uppercase(s: *const HewString) -> *mut HewString {
    // SAFETY: the caller supplies a live managed string handle.
    string_from_str(&unsafe { string_as_str(s) }.to_uppercase())
}

/// Return the Unicode scalar at codepoint offset `idx`, or -1 if out of bounds.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_char_at(s: *const HewString, idx: i64) -> i32 {
    if idx < 0 {
        return -1;
    }
    let Ok(idx) = usize::try_from(idx) else {
        return -1;
    };
    // SAFETY: `s` is a live managed handle; null is empty.
    unsafe { string_as_str(s) }
        .chars()
        .nth(idx)
        .map_or(-1, |ch| ch as i32)
}

/// Legacy exported abort for byte-indexed string access.
///
/// The compiler does not emit calls to this symbol for user-facing string
/// index/slice operations; those route through `hew_string_abort_index_oob`.
///
/// # Safety
///
/// Always aborts — safe to call from any context.
#[no_mangle]
pub unsafe extern "C" fn hew_string_abort_oob(index: i64, len: i64) -> ! {
    // SAFETY: writing to stderr and aborting is always safe.
    unsafe {
        let msg = b"PANIC: String index out of bounds\n";
        #[cfg(not(target_os = "windows"))]
        libc::write(2, msg.as_ptr().cast(), msg.len());
        #[cfg(target_os = "windows")]
        libc::write(2, msg.as_ptr().cast(), msg.len() as core::ffi::c_uint);
        let _ = (index, len);
        libc::abort();
    }
}

/// Create a UTF-8 string from a Unicode codepoint. Caller must free the result with `hew_string_drop`.
///
/// `c` is a Unicode scalar value passed as `i32`. Invalid codepoints produce U+FFFD.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions beyond a valid `i32`.
#[no_mangle]
pub unsafe extern "C" fn hew_string_from_char(c: i32) -> *mut HewString {
    // Delegate to the canonical codepoint-to-string function.
    // SAFETY: hew_char_to_string has the same ABI contract and handles all codepoints.
    unsafe { hew_char_to_string(c) }
}

/// Repeat a string `count` times. Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_repeat(s: *const HewString, count: i64) -> *mut HewString {
    if count <= 0 {
        return core::ptr::null_mut();
    }
    // SAFETY: `s` is a live managed handle; null is empty.
    let count = usize::try_from(count).unwrap_or_else(|_| std::process::abort());
    // SAFETY: `s` is a live managed string handle.
    let text = unsafe { string_as_str(s) };
    string_from_str(&text.repeat(count))
}

/// Release one owner of a string. `String` is refcounted (P2a): this decrements
/// the refcount and frees the allocation only when the last owner drops it.
/// Null is the canonical empty value and is a no-op.
///
/// # Safety
///
/// `s` must be null or one owned handle returned by the managed string
/// allocator. Foreign or interior pointers are never accepted.
#[no_mangle]
pub unsafe extern "C" fn hew_string_drop(s: *mut HewString) {
    // SAFETY: the caller transfers one managed owner; null is canonical empty.
    unsafe { string_release(s) };
}

/// Retain (share) a string. `String` is immutable-shareable, so cloning is a
/// refcount bump that returns the same opaque handle — both owners alias one
/// buffer (the copy-on-write win). The result must still be released with
/// [`hew_string_drop`], which decrements the refcount and frees only when the
/// last owner drops it.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_clone(s: *const HewString) -> *mut HewString {
    // SAFETY: the caller borrows a live managed value; null is canonical empty.
    unsafe { string_retain(s) }
}

// ---------------------------------------------------------------------------
// UTF-8 aware string operations
// ---------------------------------------------------------------------------

/// Count Unicode codepoints (not bytes) in a managed string.
///
/// For ASCII strings this equals the byte length. Null is canonical empty.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_char_count(s: *const HewString) -> i64 {
    // SAFETY: `s` is a live managed handle; null is empty.
    let count = unsafe { string_as_str(s) }.chars().count();
    i64::try_from(count).unwrap_or_else(|_| std::process::abort())
}

/// Return the UTF-8 byte length of a managed string.
///
/// This differs from [`hew_string_length`] for multibyte codepoints.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_byte_length(s: *const HewString) -> i64 {
    // SAFETY: the caller supplies a live managed string handle.
    let length = unsafe { string_as_bytes(s) }.len();
    i64::try_from(length).unwrap_or_else(|_| std::process::abort())
}

/// Returns 1 if all bytes are ASCII, 0 otherwise. Returns 1 for null (vacuous
/// truth).
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_is_ascii(s: *const HewString) -> i32 {
    // SAFETY: `s` is a live managed handle; null is empty.
    i32::from(unsafe { string_as_bytes(s) }.is_ascii())
}

/// Get the Unicode codepoint at the given codepoint index. Returns -1 if
/// `index` is out of bounds. Null is canonical empty.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_char_at_utf8(s: *const HewString, index: i64) -> i32 {
    if index < 0 {
        return -1;
    }
    let Ok(index) = usize::try_from(index) else {
        return -1;
    };
    // SAFETY: `s` is a live managed handle; null is empty.
    let rust_str = unsafe { string_as_str(s) };
    match rust_str.chars().nth(index) {
        Some(ch) => ch as i32,
        None => -1,
    }
}

/// Slice a string by codepoint indices `[start, end)`. Returns null for invalid
/// indices; null input is canonical empty.
///
/// Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_substring_utf8(
    s: *const HewString,
    start: i64,
    end: i64,
) -> *mut HewString {
    if start < 0 || end < 0 || start > end {
        return core::ptr::null_mut();
    }
    let (Ok(start), Ok(count)) = (usize::try_from(start), usize::try_from(end - start)) else {
        return core::ptr::null_mut();
    };
    // SAFETY: `s` is a live managed handle; null is empty.
    let rust_str = unsafe { string_as_str(s) };
    let result: String = rust_str.chars().skip(start).take(count).collect();
    string_from_str(&result)
}

/// Reverse a managed string by codepoints (not bytes).
///
/// Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_reverse_utf8(s: *const HewString) -> *mut HewString {
    // SAFETY: `s` is a live managed handle; null is empty.
    let rust_str = unsafe { string_as_str(s) };
    let reversed: String = rust_str.chars().rev().collect();
    string_from_str(&reversed)
}

/// Convert a string to a `bytes` value (the canonical by-value
/// [`crate::bytes::BytesTriple`] codegen materialises for a `bytes` return).
///
/// The string's complete byte range, including any embedded NUL, is copied into a fresh,
/// refcount-1 buffer the caller owns; the Hew drop spine releases it via
/// `hew_bytes_drop`. Returns an empty triple (`null` ptr, len 0) for null or
/// empty input. This is the `string -> bytes` analogue of
/// [`crate::bytes::hew_bytes_from_str`] and shares its construction/ownership.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_to_bytes(s: *const HewString) -> crate::bytes::BytesTriple {
    // SAFETY: the caller supplies a live managed string handle.
    let bytes = unsafe { string_as_bytes(s) };
    let len = u32::try_from(bytes.len()).unwrap_or_else(|_| std::process::abort());
    // SAFETY: `bytes` is readable for its exact checked length.
    unsafe { crate::bytes::hew_bytes_from_static(bytes.as_ptr(), len) }
}

/// Initialize an owned bytes result through the private physical calling ABI.
///
/// The input remains borrowed. The output owns a fresh copy of its UTF-8 bytes,
/// including any embedded NUL, and must be released with `hew_bytes_drop`.
/// Using output storage avoids target-specific C aggregate return conventions.
///
/// # Safety
///
/// `value` must be null or a live managed string handle. `out` must point to aligned,
/// writable, uninitialized storage distinct from the input allocation.
#[no_mangle]
pub unsafe extern "C" fn hew_string_to_bytes_owned(
    value: *const HewString,
    out: *mut crate::bytes::BytesTriple,
) {
    // SAFETY: the caller supplies a valid borrowed string and unique output
    // storage. The conversion transfers its sole result owner into that storage.
    unsafe { out.write(hew_string_to_bytes(value)) };
}

/// Join a `Vec<String>` into a single string with `sep` between elements.
///
/// Convenience alias for [`hew_vec_join_str`] used by the `string_join` builtin.
/// Caller must free the result with `hew_string_drop`.
///
/// # Safety
///
/// `v` must be a valid `HewVec` of managed string handles. `sep` must be null
/// or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_string_join(
    v: *mut crate::vec::HewVec,
    sep: *const HewString,
) -> *mut HewString {
    // SAFETY: forwarding identical contract to hew_vec_join_str.
    unsafe { hew_vec_join_str(v, sep) }
}

// ---------------------------------------------------------------------------
// W3 collections-sugar S2 — fail-closed codepoint indexing / slicing
// ---------------------------------------------------------------------------
//
// These four runtime entries back the compiler-emitted `s[i]` and `s[a..b]`
// sugar (Q-CS1 locked semantics): codepoint-offset, panic on invalid bounds,
// fresh owned slice. They intentionally do NOT reuse:
//
// - `hew_string_char_at`        — codepoint-indexed + returns -1.
// - `hew_string_char_at_utf8`   — codepoint-indexed but returns -1 sentinel.
// - `hew_string_slice`          — codepoint-clamping + returns empty on OOB.
// - `hew_string_substring_utf8` — codepoint-based but returns null on error.
//
// LESSONS: boundary-fail-closed (P0) — sentinel/clamp returns silently
// corrupted user code; the new intrinsics abort the process on any invalid
// input rather than producing a poisoned value.

/// Trap with a string-indexing panic message (codepoint OOB / invalid bounds).
///
/// # Safety
///
/// Always aborts — safe to call from any context.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_string_abort_index_oob() -> ! {
    // SAFETY: this exported fallback has no operand context.
    unsafe { string_bounds_trap("PANIC: string index/slice out of bounds\n") }
}

unsafe fn string_index_oob_trap(index: i64, len: Option<usize>) -> ! {
    // SAFETY: this is a terminal index bounds path.
    unsafe {
        let mut index_buf = [0u8; 20];
        write_stderr(b"PANIC: string[i] ");
        if let Some(len) = len {
            let mut len_buf = [0u8; 20];
            write_stderr(b"index ");
            write_stderr(fmt_decimal_i64(index, &mut index_buf));
            write_stderr(b" out of bounds (len ");
            write_stderr(fmt_decimal_usize(len, &mut len_buf));
            write_stderr(b")\n");
        } else {
            write_stderr(b"invalid index ");
            write_stderr(fmt_decimal_i64(index, &mut index_buf));
            write_stderr(b"\n");
        }
        runtime_bounds_trap(HEW_TRAP_INDEX_OUT_OF_BOUNDS);
    }
}

unsafe fn string_slice_oob_trap(start: i64, end: i64, len: Option<usize>) -> ! {
    // SAFETY: this is a terminal slice bounds path.
    unsafe {
        let mut start_buf = [0u8; 20];
        let mut end_buf = [0u8; 20];
        write_stderr(b"PANIC: string slice ");
        if let Some(len) = len {
            let mut len_buf = [0u8; 20];
            write_stderr(b"range ");
            write_stderr(fmt_decimal_i64(start, &mut start_buf));
            write_stderr(b"..");
            write_stderr(fmt_decimal_i64(end, &mut end_buf));
            write_stderr(b" out of bounds (len ");
            write_stderr(fmt_decimal_usize(len, &mut len_buf));
            write_stderr(b")\n");
        } else {
            write_stderr(b"invalid range ");
            write_stderr(fmt_decimal_i64(start, &mut start_buf));
            write_stderr(b"..");
            write_stderr(fmt_decimal_i64(end, &mut end_buf));
            write_stderr(b"\n");
        }
        runtime_bounds_trap(HEW_TRAP_INDEX_OUT_OF_BOUNDS);
    }
}

/// Return the Unicode codepoint at codepoint offset `index` in `s`.
///
/// Semantics (Q-CS1):
/// - O(n) walk of the UTF-8 stream.
/// - Aborts if `index < 0` or
///   `index >= char_count(s)`. No `-1` sentinel.
/// - Returns a Unicode scalar value as `i32` (1:1 with Hew's `char`).
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[expect(
    clippy::cast_sign_loss,
    reason = "index is bounds-checked >= 0 above before cast to usize"
)]
#[expect(
    clippy::cast_possible_truncation,
    reason = "i64 -> usize: usize >= 32 bits on all supported targets; \
              real string codepoint counts never exceed usize::MAX"
)]
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_string_index(s: *const HewString, index: i64) -> i32 {
    if index < 0 {
        // SAFETY: this is the terminal index bounds path.
        unsafe { string_index_oob_trap(index, None) };
    }
    // SAFETY: `s` is a live managed handle; null is empty and UTF-8 is invariant.
    let rust_str = unsafe { string_as_str(s) };
    // i64 -> usize: index is non-negative; truncation only matters when
    // index > usize::MAX which is unrepresentable in a real string.
    let idx = index as usize;
    let Some(ch) = rust_str.chars().nth(idx) else {
        let len = rust_str.chars().count();
        // SAFETY: this is the terminal index bounds path.
        unsafe { string_index_oob_trap(index, Some(len)) };
    };
    ch as i32
}

/// Slice `s` by codepoint range `[start, end)`, returning a fresh managed
/// string owner.
///
/// Semantics (Q-CS1):
/// - O(n) walk; fresh owned allocation (LESSONS:
///   stdlib-borrowed-param-return-guard P0 — the returned pointer never
///   aliases the input).
/// - Aborts if `start < 0`, `end < 0`,
///   `start > end`, or `end > char_count(s)`. No null / empty fallback.
///
/// # Safety
///
/// `s` must be null or a live managed string handle.
#[expect(
    clippy::cast_sign_loss,
    reason = "start and end are bounds-checked >= 0 above before cast to usize"
)]
#[expect(
    clippy::cast_possible_truncation,
    reason = "i64 -> usize: usize >= 32 bits on all supported targets; \
              real string codepoint counts never exceed usize::MAX"
)]
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_string_slice_codepoints(
    s: *const HewString,
    start: i64,
    end: i64,
) -> *mut HewString {
    if start < 0 || end < 0 || start > end {
        // SAFETY: this is the terminal slice bounds path.
        unsafe { string_slice_oob_trap(start, end, None) };
    }
    // SAFETY: `s` is a live managed handle; null is empty and UTF-8 is invariant.
    let rust_str = unsafe { string_as_str(s) };
    let start_idx = start as usize;
    let end_idx = end as usize;
    let char_len = rust_str.chars().count();
    if start_idx > char_len || end_idx > char_len {
        // SAFETY: this is the terminal slice bounds path.
        unsafe { string_slice_oob_trap(start, end, Some(char_len)) };
    }
    let take_n = end_idx - start_idx;
    let mut buf = String::new();
    for ch in rust_str.chars().skip(start_idx).take(take_n) {
        buf.push(ch);
    }
    string_from_str(&buf)
}

// ---------------------------------------------------------------------------
// Unicode codepoint classification — std::text::unicode
// ---------------------------------------------------------------------------
//
// These functions operate on a Unicode scalar value (Unicode codepoint) passed
// as `i32`, matching the Hew `i64` ABI after Hew's widening cast. They back
// `unicode.is_upper`, `unicode.is_lower`, `unicode.is_space`, `unicode.is_digit`,
// `unicode.is_letter`, `unicode.is_alnum`, `unicode.to_upper`, `unicode.to_lower`.
//
// Rust's `char` type directly implements the Unicode-derived predicates we need
// (via the `unicode_core` tables baked into stdlib), so no additional crates are
// required.
//
// The `-1` input is used as an "invalid codepoint" sentinel by the Hew caller
// (e.g. from `char_at_utf8` returning -1 on OOB). All functions return
// `false`/original-codepoint for invalid inputs rather than panicking.

/// Test whether the Unicode codepoint `cp` is an uppercase letter.
///
/// Returns `false` for invalid codepoints.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[expect(
    clippy::cast_sign_loss,
    reason = "negative cp reinterprets as a high u32 value that char::from_u32 rejects — \
              the sign-loss is the intentional invalid-codepoint guard"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_unicode_is_upper(cp: i32) -> bool {
    char::from_u32(cp as u32).is_some_and(char::is_uppercase)
}

/// Test whether the Unicode codepoint `cp` is a lowercase letter.
///
/// Returns `false` for invalid codepoints.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[expect(
    clippy::cast_sign_loss,
    reason = "negative cp reinterprets as a high u32 value that char::from_u32 rejects — \
              the sign-loss is the intentional invalid-codepoint guard"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_unicode_is_lower(cp: i32) -> bool {
    char::from_u32(cp as u32).is_some_and(char::is_lowercase)
}

/// Test whether the Unicode codepoint `cp` is a whitespace character.
///
/// Returns `false` for invalid codepoints.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[expect(
    clippy::cast_sign_loss,
    reason = "negative cp reinterprets as a high u32 value that char::from_u32 rejects — \
              the sign-loss is the intentional invalid-codepoint guard"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_unicode_is_space(cp: i32) -> bool {
    char::from_u32(cp as u32).is_some_and(char::is_whitespace)
}

/// Test whether the Unicode codepoint `cp` is a decimal digit (0–9).
///
/// Returns `false` for invalid codepoints.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[expect(
    clippy::cast_sign_loss,
    reason = "negative cp reinterprets as a high u32 value that char::from_u32 rejects — \
              the sign-loss is the intentional invalid-codepoint guard"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_unicode_is_digit(cp: i32) -> bool {
    char::from_u32(cp as u32).is_some_and(|c| c.is_ascii_digit())
}

/// Test whether the Unicode codepoint `cp` is a Unicode letter (alphabetic).
///
/// Returns `false` for invalid codepoints.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[expect(
    clippy::cast_sign_loss,
    reason = "negative cp reinterprets as a high u32 value that char::from_u32 rejects — \
              the sign-loss is the intentional invalid-codepoint guard"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_unicode_is_letter(cp: i32) -> bool {
    char::from_u32(cp as u32).is_some_and(char::is_alphabetic)
}

/// Test whether the Unicode codepoint `cp` is a Unicode letter or decimal digit.
///
/// Returns `false` for invalid codepoints.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[expect(
    clippy::cast_sign_loss,
    reason = "negative cp reinterprets as a high u32 value that char::from_u32 rejects — \
              the sign-loss is the intentional invalid-codepoint guard"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_unicode_is_alnum(cp: i32) -> bool {
    char::from_u32(cp as u32).is_some_and(char::is_alphanumeric)
}

/// Test whether the Unicode codepoint `cp` is Unicode punctuation.
///
/// "Punctuation" is the Unicode general category group `P*` — `Pc`, `Pd`,
/// `Ps`, `Pe`, `Pi`, `Pf`, and `Po` — as decided by the pinned Unicode tables
/// rather than by a hand-maintained range list. Returns `false` for invalid
/// codepoints (negative, surrogate, or above `U+10FFFF`).
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[expect(
    clippy::cast_sign_loss,
    reason = "negative cp reinterprets as a high u32 value that char::from_u32 rejects — \
              the sign-loss is the intentional invalid-codepoint guard"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_unicode_is_punct(cp: i32) -> bool {
    use finl_unicode::categories::CharacterCategories;
    char::from_u32(cp as u32).is_some_and(CharacterCategories::is_punctuation)
}

/// Convert the Unicode codepoint `cp` to its uppercase equivalent.
///
/// Returns the first codepoint in the Unicode to-uppercase mapping, which
/// covers the common case. Ligature decompositions (e.g. ß → SS) are not
/// represented; callers needing full title-case must use string-level
/// `to_uppercase()`.
///
/// Returns `cp` unchanged for codepoints with no uppercase mapping or for
/// invalid codepoints.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[expect(
    clippy::cast_sign_loss,
    reason = "negative cp reinterprets as a high u32 value that char::from_u32 rejects — \
              the sign-loss is the intentional invalid-codepoint guard"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_unicode_to_upper(cp: i32) -> i32 {
    let Some(ch) = char::from_u32(cp as u32) else {
        return cp;
    };
    ch.to_uppercase().next().map_or(cp, |u| u as i32)
}

/// Convert the Unicode codepoint `cp` to its lowercase equivalent.
///
/// Returns the first codepoint in the Unicode to-lowercase mapping.
/// Returns `cp` unchanged for codepoints with no lowercase mapping or for
/// invalid codepoints.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[expect(
    clippy::cast_sign_loss,
    reason = "negative cp reinterprets as a high u32 value that char::from_u32 rejects — \
              the sign-loss is the intentional invalid-codepoint guard"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_unicode_to_lower(cp: i32) -> i32 {
    let Some(ch) = char::from_u32(cp as u32) else {
        return cp;
    };
    ch.to_lowercase().next().map_or(cp, |u| u as i32)
}

#[cfg(test)]
mod tests {
    use super::*;

    struct CString(*mut HewString);

    impl CString {
        #[expect(
            clippy::unnecessary_wraps,
            reason = "compatibility adapter preserves the old test fixture call shape"
        )]
        fn new(value: impl AsRef<str>) -> Result<Self, core::convert::Infallible> {
            Ok(Self(string_from_str(value.as_ref())))
        }

        fn as_ptr(&self) -> *const HewString {
            self.0
        }
    }

    impl Drop for CString {
        fn drop(&mut self) {
            // SAFETY: the fixture owns one managed string share.
            unsafe { string_release(self.0) };
        }
    }

    struct CStr<'a>(&'a str);

    impl<'a> CStr<'a> {
        unsafe fn from_ptr(value: *const HewString) -> Self {
            // SAFETY: test callers retain a live owner for the wrapper lifetime.
            Self(unsafe { string_as_str(value) })
        }

        fn to_bytes(&self) -> &'a [u8] {
            self.0.as_bytes()
        }

        #[expect(
            clippy::unnecessary_wraps,
            reason = "compatibility adapter preserves the old test fixture call shape"
        )]
        fn to_str(&self) -> Result<&'a str, core::str::Utf8Error> {
            Ok(self.0)
        }
    }

    #[test]
    fn to_bytes_owned_output_survives_source_release() {
        let mut source = std::ptr::null_mut();
        let mut output = std::mem::MaybeUninit::<crate::bytes::BytesTriple>::uninit();
        // SAFETY: literal input is readable and both outputs are writable.
        unsafe {
            hew_string_literal_new(b"A\xc3\xa9".as_ptr(), 3, &raw mut source);
            hew_string_to_bytes_owned(source, output.as_mut_ptr());
            assert_eq!(string_as_bytes(source), b"A\xc3\xa9");
            hew_string_drop(source);
            let bytes = output.assume_init();
            assert_eq!(bytes.len, 3);
            assert_eq!(
                std::slice::from_raw_parts(bytes.ptr.add(bytes.offset as usize), 3),
                b"A\xc3\xa9"
            );
            crate::bytes::hew_bytes_drop(bytes.ptr);
        }
    }

    #[test]
    fn to_bytes_owned_empty_input_initializes_empty_output() {
        let mut output = std::mem::MaybeUninit::<crate::bytes::BytesTriple>::uninit();
        // SAFETY: null is canonical empty and output is writable.
        unsafe {
            hew_string_to_bytes_owned(core::ptr::null(), output.as_mut_ptr());
            let bytes = output.assume_init();
            assert_eq!(bytes.len, 0);
            assert_eq!(bytes.offset, 0);
            crate::bytes::hew_bytes_drop(bytes.ptr);
        }
    }

    #[test]
    fn literal_new_copies_exact_bytes_into_managed_owner() {
        let mut source = *b"hello!";
        let mut output = std::ptr::null_mut();
        // SAFETY: source has five readable bytes and output is writable.
        unsafe { hew_string_literal_new(source.as_ptr(), 5, &raw mut output) };
        assert!(!output.is_null());
        source[0] = b'j';
        // SAFETY: output is a live managed string; cloning retains an owner.
        unsafe {
            let copy = hew_string_clone(output);
            hew_string_drop(output);
            assert_eq!(string_as_bytes(copy), b"hello");
            hew_string_drop(copy);
        }
        assert_eq!(source, *b"jello!");
    }

    #[test]
    fn literal_new_empty_input_initializes_managed_empty_string() {
        let mut output = std::ptr::null_mut();
        // SAFETY: zero length permits null input; output is writable.
        unsafe { hew_string_literal_new(std::ptr::null(), 0, &raw mut output) };
        assert!(output.is_null());
    }

    unsafe fn read_and_free(ptr: *mut HewString) -> String {
        // SAFETY: ptr is a live managed string owner.
        let s = unsafe { string_as_str(ptr) }.to_owned();
        // SAFETY: release the sole result owner.
        unsafe { string_release(ptr) };
        s
    }

    #[test]
    fn test_string_concat_basic() {
        let a = CString::new("hello ").unwrap();
        let b = CString::new("world").unwrap();
        // SAFETY: Both args are valid NUL-terminated C strings.
        let result = unsafe { hew_string_concat(a.as_ptr(), b.as_ptr()) };
        // SAFETY: result is a valid malloc'd C string returned by hew_string_concat.
        assert_eq!(unsafe { read_and_free(result) }, "hello world");
    }

    #[test]
    fn test_string_concat_empty_both() {
        let a = CString::new("").unwrap();
        let b = CString::new("").unwrap();
        // SAFETY: Both args are valid NUL-terminated C strings.
        let result = unsafe { hew_string_concat(a.as_ptr(), b.as_ptr()) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "");
    }

    #[test]
    fn test_string_concat_null_left() {
        let b = CString::new("world").unwrap();
        // SAFETY: Null left arg is explicitly handled; b is a valid C string.
        let result = unsafe { hew_string_concat(core::ptr::null(), b.as_ptr()) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "world");
    }

    #[test]
    fn test_string_concat_null_right() {
        let a = CString::new("hello").unwrap();
        // SAFETY: a is a valid C string; null right arg is explicitly handled.
        let result = unsafe { hew_string_concat(a.as_ptr(), core::ptr::null()) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "hello");
    }

    #[test]
    fn test_string_slice_basic() {
        let s = CString::new("hello world").unwrap();
        // SAFETY: s is a valid C string; indices are within bounds.
        let result = unsafe { hew_string_slice(s.as_ptr(), 0, 5) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "hello");
    }

    #[test]
    fn test_string_slice_null() {
        // SAFETY: Null input is explicitly handled by hew_string_slice.
        let result = unsafe { hew_string_slice(core::ptr::null(), 0, 5) };
        // SAFETY: result is a valid malloc'd C string (empty).
        assert_eq!(unsafe { read_and_free(result) }, "");
    }

    #[test]
    fn test_string_slice_start_past_end() {
        let s = CString::new("hello").unwrap();
        // SAFETY: s is a valid C string; out-of-bounds indices are handled.
        let result = unsafe { hew_string_slice(s.as_ptr(), 10, 20) };
        // SAFETY: result is a valid malloc'd C string (empty).
        assert_eq!(unsafe { read_and_free(result) }, "");
    }

    #[test]
    fn test_string_find_basic() {
        let s = CString::new("hello world").unwrap();
        let sub = CString::new("world").unwrap();
        // SAFETY: Both args are valid NUL-terminated C strings.
        assert_eq!(unsafe { hew_string_find(s.as_ptr(), sub.as_ptr()) }, 6);
    }

    #[test]
    fn test_string_find_not_found() {
        let s = CString::new("hello").unwrap();
        let sub = CString::new("xyz").unwrap();
        // SAFETY: Both args are valid NUL-terminated C strings.
        assert_eq!(unsafe { hew_string_find(s.as_ptr(), sub.as_ptr()) }, -1);
    }

    #[test]
    fn test_string_find_null() {
        let sub = CString::new("test").unwrap();
        assert_eq!(
            // SAFETY: Null haystack is explicitly handled; sub is a valid C string.
            unsafe { hew_string_find(core::ptr::null(), sub.as_ptr()) },
            -1
        );
    }

    #[test]
    fn test_string_length() {
        let s = CString::new("hello").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_string_length(s.as_ptr()) }, 5);
    }

    #[test]
    fn test_string_length_empty() {
        let s = CString::new("").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_string_length(s.as_ptr()) }, 0);
    }

    #[test]
    fn test_string_equals() {
        let a = CString::new("hello").unwrap();
        let b = CString::new("hello").unwrap();
        let c = CString::new("world").unwrap();
        // SAFETY: All args are valid NUL-terminated C strings.
        assert_eq!(unsafe { hew_string_equals(a.as_ptr(), b.as_ptr()) }, 1);
        // SAFETY: All args are valid NUL-terminated C strings.
        assert_eq!(unsafe { hew_string_equals(a.as_ptr(), c.as_ptr()) }, 0);
    }

    #[test]
    fn test_string_compare() {
        let apple = CString::new("apple").unwrap();
        let banana = CString::new("banana").unwrap();
        let cherry = CString::new("cherry").unwrap();
        assert_eq!(
            // SAFETY: All args are valid NUL-terminated C strings.
            unsafe { hew_string_compare(apple.as_ptr(), banana.as_ptr()) },
            -1
        );
        assert_eq!(
            // SAFETY: All args are valid NUL-terminated C strings.
            unsafe { hew_string_compare(cherry.as_ptr(), banana.as_ptr()) },
            1
        );
        assert_eq!(
            // SAFETY: All args are valid NUL-terminated C strings.
            unsafe { hew_string_compare(banana.as_ptr(), banana.as_ptr()) },
            0
        );
        assert_eq!(
            // SAFETY: Null is explicitly handled by hew_string_compare.
            unsafe { hew_string_compare(std::ptr::null(), banana.as_ptr()) },
            -1
        );
        assert_eq!(
            // SAFETY: Null is explicitly handled by hew_string_compare.
            unsafe { hew_string_compare(banana.as_ptr(), std::ptr::null()) },
            1
        );
        assert_eq!(
            // SAFETY: Null is explicitly handled by hew_string_compare.
            unsafe { hew_string_compare(std::ptr::null(), std::ptr::null()) },
            0
        );
    }

    #[test]
    fn test_string_starts_with() {
        let s = CString::new("hello world").unwrap();
        let prefix = CString::new("hello").unwrap();
        let bad = CString::new("world").unwrap();
        // SAFETY: All args are valid NUL-terminated C strings.
        assert!(unsafe { hew_string_starts_with(s.as_ptr(), prefix.as_ptr()) });
        // SAFETY: All args are valid NUL-terminated C strings.
        assert!(!unsafe { hew_string_starts_with(s.as_ptr(), bad.as_ptr()) });
    }

    #[test]
    fn test_string_ends_with() {
        let s = CString::new("hello world").unwrap();
        let suffix = CString::new("world").unwrap();
        let bad = CString::new("hello").unwrap();
        // SAFETY: All args are valid NUL-terminated C strings.
        assert!(unsafe { hew_string_ends_with(s.as_ptr(), suffix.as_ptr()) });
        // SAFETY: All args are valid NUL-terminated C strings.
        assert!(!unsafe { hew_string_ends_with(s.as_ptr(), bad.as_ptr()) });
    }

    #[test]
    fn test_string_contains() {
        let s = CString::new("hello world").unwrap();
        let sub = CString::new("lo wo").unwrap();
        let bad = CString::new("xyz").unwrap();
        // SAFETY: All args are valid NUL-terminated C strings.
        assert!(unsafe { hew_string_contains(s.as_ptr(), sub.as_ptr()) });
        // SAFETY: All args are valid NUL-terminated C strings.
        assert!(!unsafe { hew_string_contains(s.as_ptr(), bad.as_ptr()) });
    }

    #[test]
    fn test_int_to_string() {
        // SAFETY: No pointer arguments.
        let result = unsafe { hew_int_to_string(42) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "42");
    }

    #[test]
    fn test_int_to_string_negative() {
        // SAFETY: No pointer arguments.
        let result = unsafe { hew_int_to_string(-7) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "-7");
    }

    #[test]
    fn test_int_to_string_zero() {
        // SAFETY: No pointer arguments.
        let result = unsafe { hew_int_to_string(0) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "0");
    }

    #[test]
    fn test_bool_to_string() {
        // SAFETY: No pointer arguments.
        let t = unsafe { hew_bool_to_string(true) };
        // SAFETY: No pointer arguments.
        let f = unsafe { hew_bool_to_string(false) };
        // SAFETY: t is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(t) }, "true");
        // SAFETY: f is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(f) }, "false");
    }

    #[test]
    fn test_string_trim() {
        let s = CString::new("  hello  ").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        let result = unsafe { hew_string_trim(s.as_ptr()) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "hello");
    }

    #[test]
    fn test_string_trim_all_whitespace() {
        let s = CString::new("   ").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        let result = unsafe { hew_string_trim(s.as_ptr()) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "");
    }

    #[test]
    fn test_string_replace() {
        let s = CString::new("hello world").unwrap();
        let from = CString::new("world").unwrap();
        let to = CString::new("rust").unwrap();
        // SAFETY: All args are valid NUL-terminated C strings.
        let result = unsafe { hew_string_replace(s.as_ptr(), from.as_ptr(), to.as_ptr()) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "hello rust");
    }

    #[test]
    fn test_string_to_lowercase() {
        let s = CString::new("HELLO World").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        let result = unsafe { hew_string_to_lowercase(s.as_ptr()) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "hello world");
    }

    #[test]
    fn test_string_to_uppercase() {
        let s = CString::new("hello World").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        let result = unsafe { hew_string_to_uppercase(s.as_ptr()) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "HELLO WORLD");
    }

    #[test]
    fn test_string_repeat() {
        let s = CString::new("ab").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        let result = unsafe { hew_string_repeat(s.as_ptr(), 3) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "ababab");
    }

    #[test]
    fn test_string_repeat_zero() {
        let s = CString::new("ab").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        let result = unsafe { hew_string_repeat(s.as_ptr(), 0) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "");
    }

    #[test]
    fn test_string_char_count() {
        let s = CString::new("hello").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        assert_eq!(unsafe { hew_string_char_count(s.as_ptr()) }, 5);
    }

    #[test]
    fn test_string_is_ascii() {
        let ascii = CString::new("hello").unwrap();
        let non_ascii = CString::new("héllo").unwrap();
        // SAFETY: Both args are valid NUL-terminated C strings.
        assert_eq!(unsafe { hew_string_is_ascii(ascii.as_ptr()) }, 1);
        // SAFETY: Both args are valid NUL-terminated C strings.
        assert_eq!(unsafe { hew_string_is_ascii(non_ascii.as_ptr()) }, 0);
    }

    #[test]
    fn test_string_reverse_utf8() {
        let s = CString::new("hello").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        let result = unsafe { hew_string_reverse_utf8(s.as_ptr()) };
        // SAFETY: result is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(result) }, "olleh");
    }

    #[test]
    fn test_string_reverse_utf8_null() {
        // SAFETY: Null input is explicitly handled by hew_string_reverse_utf8.
        let result = unsafe { hew_string_reverse_utf8(core::ptr::null()) };
        assert!(result.is_null());
    }

    #[test]
    fn test_string_split_basic() {
        let s = CString::new("a,b,c").unwrap();
        let sep = CString::new(",").unwrap();
        // SAFETY: Both args are valid NUL-terminated C strings.
        let v = unsafe { hew_string_split(s.as_ptr(), sep.as_ptr()) };
        assert!(!v.is_null());
        // SAFETY: v is a valid HewVec from hew_string_split.
        assert_eq!(unsafe { crate::vec::hew_vec_len(v) }, 3);
        // SAFETY: index 1 is within bounds.
        // hew_vec_get_str retains the element for the caller — must be dropped.
        let part = unsafe { crate::vec::hew_vec_get_str(v, 1) };
        assert!(!part.is_null());
        // SAFETY: part is a valid C string.
        let elem_str = unsafe { CStr::from_ptr(part) }.to_str().unwrap();
        assert_eq!(elem_str, "b");
        // SAFETY: release the caller's retained ref from hew_vec_get_str.
        unsafe { hew_string_drop(part.cast_mut()) };
        // SAFETY: v is a valid HewVec.
        unsafe { crate::vec::hew_vec_free(v) };
    }

    // split("abc", "") produces ["a", "b", "c"] — one element per codepoint.
    #[test]
    fn split_empty_sep_ascii_produces_chars() {
        let s = CString::new("abc").unwrap();
        let sep = CString::new("").unwrap();
        // SAFETY: Both args are valid NUL-terminated C strings.
        let v = unsafe { hew_string_split(s.as_ptr(), sep.as_ptr()) };
        assert!(!v.is_null());
        // SAFETY: v is a valid HewVec.
        assert_eq!(unsafe { crate::vec::hew_vec_len(v) }, 3);
        for (i, expected) in ["a", "b", "c"].iter().enumerate() {
            #[expect(clippy::cast_possible_wrap, reason = "test: index ≤ 2, never wraps")]
            // SAFETY: index i is within bounds.
            let part = unsafe { crate::vec::hew_vec_get_str(v, i as i64) };
            assert!(!part.is_null());
            // SAFETY: part is a valid C string.
            assert_eq!(unsafe { CStr::from_ptr(part) }.to_str().unwrap(), *expected);
            // SAFETY: release the caller's retained ref.
            unsafe { hew_string_drop(part.cast_mut()) };
        }
        // SAFETY: v is a valid HewVec.
        unsafe { crate::vec::hew_vec_free(v) };
    }

    // split("café", "") produces ["c","a","f","é"] — é is one element, not two bytes.
    #[test]
    fn split_empty_sep_multibyte_produces_codepoints() {
        let s = CString::new("café").unwrap();
        let sep = CString::new("").unwrap();
        // SAFETY: Both args are valid NUL-terminated C strings.
        let v = unsafe { hew_string_split(s.as_ptr(), sep.as_ptr()) };
        assert!(!v.is_null());
        // SAFETY: v is a valid HewVec.
        assert_eq!(unsafe { crate::vec::hew_vec_len(v) }, 4);
        let expected = ["c", "a", "f", "é"];
        for (i, exp) in expected.iter().enumerate() {
            #[expect(clippy::cast_possible_wrap, reason = "test: index ≤ 3, never wraps")]
            // SAFETY: index i is within bounds.
            let part = unsafe { crate::vec::hew_vec_get_str(v, i as i64) };
            assert!(!part.is_null());
            // SAFETY: part is a valid C string.
            let got = unsafe { CStr::from_ptr(part) }.to_str().unwrap();
            assert_eq!(got, *exp, "element {i}");
            // Confirm that each element is valid UTF-8 (the old implementation
            // would have emitted a lone 0xE9 byte here, which is not valid UTF-8).
            // SAFETY: part is a valid C string — already checked above.
            assert!(std::str::from_utf8(unsafe { CStr::from_ptr(part) }.to_bytes()).is_ok());
            // SAFETY: release the caller's retained ref.
            unsafe { hew_string_drop(part.cast_mut()) };
        }
        // SAFETY: v is a valid HewVec.
        unsafe { crate::vec::hew_vec_free(v) };
    }

    /// split("", "") → [] (empty vec for empty input).
    #[test]
    fn split_empty_sep_empty_string_is_empty_vec() {
        let s = CString::new("").unwrap();
        let sep = CString::new("").unwrap();
        // SAFETY: Both args are valid NUL-terminated C strings.
        let v = unsafe { hew_string_split(s.as_ptr(), sep.as_ptr()) };
        assert!(!v.is_null());
        // SAFETY: v is a valid HewVec.
        assert_eq!(unsafe { crate::vec::hew_vec_len(v) }, 0);
        // SAFETY: v is a valid HewVec.
        unsafe { crate::vec::hew_vec_free(v) };
    }

    #[test]
    fn test_string_lines_basic() {
        let s = CString::new("line1\nline2\nline3").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        let v = unsafe { hew_string_lines(s.as_ptr()) };
        assert!(!v.is_null());
        // SAFETY: v is a valid HewVec from hew_string_lines.
        assert_eq!(unsafe { crate::vec::hew_vec_len(v) }, 3);
        // SAFETY: index 0 is within bounds.
        // hew_vec_get_str retains the element for the caller — must be dropped.
        let part = unsafe { crate::vec::hew_vec_get_str(v, 0) };
        // SAFETY: part is a valid C string.
        assert_eq!(unsafe { CStr::from_ptr(part) }.to_str().unwrap(), "line1");
        // SAFETY: release the caller's retained ref from hew_vec_get_str.
        unsafe { hew_string_drop(part.cast_mut()) };
        // SAFETY: v is a valid HewVec.
        unsafe { crate::vec::hew_vec_free(v) };
    }

    #[test]
    fn test_string_chars_ascii() {
        let s = CString::new("abc").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        let v = unsafe { hew_string_chars(s.as_ptr()) };
        assert!(!v.is_null());
        // SAFETY: v is a valid HewVec from hew_string_chars.
        assert_eq!(unsafe { crate::vec::hew_vec_len(v) }, 3);
        // SAFETY: index 0 is within bounds.
        let c0 = unsafe { crate::vec::hew_vec_get_i32(v, 0) };
        // SAFETY: index 1 is within bounds.
        let c1 = unsafe { crate::vec::hew_vec_get_i32(v, 1) };
        // SAFETY: index 2 is within bounds.
        let c2 = unsafe { crate::vec::hew_vec_get_i32(v, 2) };
        assert_eq!(c0, i32::from(b'a'));
        assert_eq!(c1, i32::from(b'b'));
        assert_eq!(c2, i32::from(b'c'));
        // SAFETY: v is a valid HewVec.
        unsafe { crate::vec::hew_vec_free(v) };
    }

    #[test]
    fn test_string_chars_multibyte() {
        // "é" is U+00E9 (two UTF-8 bytes), "中" is U+4E2D (three bytes).
        let s = CString::new("é中").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        let v = unsafe { hew_string_chars(s.as_ptr()) };
        assert!(!v.is_null());
        // SAFETY: v is a valid HewVec from hew_string_chars.
        assert_eq!(unsafe { crate::vec::hew_vec_len(v) }, 2);
        // SAFETY: index 0 is within bounds.
        let c0 = unsafe { crate::vec::hew_vec_get_i32(v, 0) };
        // SAFETY: index 1 is within bounds.
        let c1 = unsafe { crate::vec::hew_vec_get_i32(v, 1) };
        assert_eq!(c0, 0x00E9); // é
        assert_eq!(c1, 0x4E2D); // 中
                                // SAFETY: v is a valid HewVec.
        unsafe { crate::vec::hew_vec_free(v) };
    }

    #[test]
    fn test_string_chars_empty() {
        let s = CString::new("").unwrap();
        // SAFETY: s is a valid NUL-terminated C string.
        let v = unsafe { hew_string_chars(s.as_ptr()) };
        assert!(!v.is_null());
        // SAFETY: v is a valid HewVec from hew_string_chars.
        assert_eq!(unsafe { crate::vec::hew_vec_len(v) }, 0);
        // SAFETY: v is a valid HewVec.
        unsafe { crate::vec::hew_vec_free(v) };
    }

    #[test]
    fn test_string_chars_null() {
        // SAFETY: null is the documented "skip" sentinel.
        let v = unsafe { hew_string_chars(core::ptr::null()) };
        assert!(!v.is_null());
        // SAFETY: v is a valid HewVec from hew_string_chars.
        assert_eq!(unsafe { crate::vec::hew_vec_len(v) }, 0);
        // SAFETY: v is a valid HewVec.
        unsafe { crate::vec::hew_vec_free(v) };
    }

    #[test]
    fn test_string_join_basic() {
        let s = CString::new("a,b,c").unwrap();
        let sep = CString::new(",").unwrap();
        // SAFETY: Both args are valid NUL-terminated C strings.
        let v = unsafe { hew_string_split(s.as_ptr(), sep.as_ptr()) };
        // SAFETY: v is a valid HewVec from hew_string_split; sep is a valid C string.
        let joined = unsafe { hew_string_join(v, sep.as_ptr()) };
        assert!(!joined.is_null());
        // SAFETY: joined is a valid malloc'd C string.
        let result = unsafe { CStr::from_ptr(joined) }
            .to_str()
            .unwrap()
            .to_owned();
        // SAFETY: joined was allocated header-aware by hew_string_join.
        unsafe { hew_string_drop(joined) };
        // SAFETY: v is a valid HewVec.
        unsafe { crate::vec::hew_vec_free(v) };
        assert_eq!(result, "a,b,c");
    }

    // ------------------------------------------------------------------
    // W3 collections-sugar S2 — hew_string_index /
    // hew_string_slice_codepoints tests
    // ------------------------------------------------------------------

    #[test]
    fn string_index_ascii() {
        let s = CString::new("hello").unwrap();
        // SAFETY: s is a valid NUL-terminated C string; index in bounds.
        assert_eq!(unsafe { hew_string_index(s.as_ptr(), 0) }, i32::from(b'h'));
        // SAFETY: s is a valid NUL-terminated C string; index in bounds.
        assert_eq!(unsafe { hew_string_index(s.as_ptr(), 4) }, i32::from(b'o'));
    }

    #[test]
    fn string_index_multibyte() {
        // "héllo" has codepoint sequence: h, é (U+00E9), l, l, o.
        let s = CString::new("héllo").unwrap();
        // SAFETY: s is a valid NUL-terminated C string; index in bounds.
        assert_eq!(unsafe { hew_string_index(s.as_ptr(), 0) }, i32::from(b'h'));
        // SAFETY: s is a valid NUL-terminated C string; index in bounds.
        assert_eq!(unsafe { hew_string_index(s.as_ptr(), 1) }, 0x00E9);
        // SAFETY: s is a valid NUL-terminated C string; index in bounds.
        assert_eq!(unsafe { hew_string_index(s.as_ptr(), 4) }, i32::from(b'o'));
    }

    #[test]
    fn string_slice_ascii_fresh_alloc() {
        // Drop-safety: the returned pointer must be a fresh allocation,
        // disjoint from the input. We free the input first, then read
        // the slice — if the slice borrowed from the input this would
        // be use-after-free.
        let input = string_from_str("hello");
        // SAFETY: input is a live managed string.
        let slice = unsafe { hew_string_slice_codepoints(input, 1, 4) };
        assert!(!slice.is_null());
        assert_ne!(slice as usize, input as usize);
        // SAFETY: release the input before proving the owned slice survives.
        unsafe { string_release(input) };
        // SAFETY: `slice` is the independent owner returned above.
        assert_eq!(unsafe { read_and_free(slice) }, "ell");
    }

    #[test]
    fn string_slice_multibyte_codepoints() {
        let s = CString::new("héllo").unwrap();
        // SAFETY: s is a valid NUL-terminated C string; bounds in range.
        let slice = unsafe { hew_string_slice_codepoints(s.as_ptr(), 1, 4) };
        // SAFETY: read_and_free takes ownership of the malloc'd slice.
        assert_eq!(unsafe { read_and_free(slice) }, "éll");
    }

    #[test]
    fn string_slice_full_range() {
        let s = CString::new("héllo").unwrap();
        // SAFETY: s is a valid NUL-terminated C string; bounds in range.
        let slice = unsafe { hew_string_slice_codepoints(s.as_ptr(), 0, 5) };
        // SAFETY: read_and_free takes ownership of the malloc'd slice.
        assert_eq!(unsafe { read_and_free(slice) }, "héllo");
    }

    #[test]
    fn string_slice_empty() {
        let s = CString::new("héllo").unwrap();
        // SAFETY: s is a valid NUL-terminated C string; bounds in range.
        let slice = unsafe { hew_string_slice_codepoints(s.as_ptr(), 2, 2) };
        // SAFETY: read_and_free takes ownership of the malloc'd slice.
        assert_eq!(unsafe { read_and_free(slice) }, "");
    }

    // Subprocess-spawning abort tests (mirrors the pattern in bytes.rs).
    // Each parent test spawns the in-process helper `string_abort_helper`
    // with the case name in HEW_STRING_ABORT_CASE; the helper triggers
    // the matching `libc::abort()`-on-OOB path. Parent asserts child
    // exited non-zero.

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn string_index_oob_aborts() {
        run_aborting_subprocess("string_index_oob_aborts");
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn string_index_negative_aborts() {
        run_aborting_subprocess("string_index_negative_aborts");
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn string_index_null_aborts() {
        run_aborting_subprocess("string_index_null_aborts");
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn string_slice_oob_aborts() {
        run_aborting_subprocess("string_slice_oob_aborts");
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn string_slice_inverted_aborts() {
        run_aborting_subprocess("string_slice_inverted_aborts");
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn string_slice_negative_aborts() {
        run_aborting_subprocess("string_slice_negative_aborts");
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn run_aborting_subprocess(case: &str) {
        let exe = std::env::current_exe().expect("current_exe");
        let status = std::process::Command::new(exe)
            .args([
                "--quiet",
                "--exact",
                "--nocapture",
                "string::tests::string_abort_helper",
            ])
            .env("HEW_STRING_ABORT_CASE", case)
            .stderr(std::process::Stdio::null())
            .stdout(std::process::Stdio::null())
            .status()
            .expect("spawn");
        assert!(!status.success(), "child did not abort");
    }

    #[test]
    fn string_abort_helper() {
        let Ok(case) = std::env::var("HEW_STRING_ABORT_CASE") else {
            return;
        };
        let s = CString::new("héllo").unwrap();
        match case.as_str() {
            "string_index_oob_aborts" => {
                // SAFETY: s is a valid C string; intentional OOB triggers abort.
                let _ = unsafe { hew_string_index(s.as_ptr(), 5) };
            }
            "string_index_negative_aborts" => {
                // SAFETY: s is a valid C string; negative index triggers abort.
                let _ = unsafe { hew_string_index(s.as_ptr(), -1) };
            }
            "string_index_null_aborts" => {
                // SAFETY: null pointer triggers abort (by design).
                let _ = unsafe { hew_string_index(core::ptr::null(), 0) };
            }
            "string_slice_oob_aborts" => {
                // SAFETY: s is a valid C string; OOB end triggers abort.
                let _ = unsafe { hew_string_slice_codepoints(s.as_ptr(), 0, 6) };
            }
            "string_slice_inverted_aborts" => {
                // SAFETY: s is a valid C string; start>end triggers abort.
                let _ = unsafe { hew_string_slice_codepoints(s.as_ptr(), 3, 1) };
            }
            "string_slice_negative_aborts" => {
                // SAFETY: s is a valid C string; negative start triggers abort.
                let _ = unsafe { hew_string_slice_codepoints(s.as_ptr(), -1, 2) };
            }
            other => panic!("unknown abort case: {other}"),
        }
        unreachable!("abort case {case} should have terminated the process");
    }

    // ── Unicode codepoint classification ──────────────────────────────────────

    #[test]
    fn unicode_is_upper_ascii_uppercase() {
        // 'A' = 0x41
        // SAFETY: hew_unicode_is_upper takes only an i32; no pointers.
        assert!(unsafe { hew_unicode_is_upper(0x41) });
    }

    #[test]
    fn unicode_is_upper_ascii_lowercase_is_false() {
        // 'a' = 0x61
        // SAFETY: hew_unicode_is_upper takes only an i32; no pointers.
        assert!(!unsafe { hew_unicode_is_upper(0x61) });
    }

    #[test]
    fn unicode_is_upper_latin_extended() {
        // 'É' (U+00C9) is uppercase
        // SAFETY: hew_unicode_is_upper takes only an i32; no pointers.
        assert!(unsafe { hew_unicode_is_upper(0xC9) });
    }

    #[test]
    fn unicode_is_upper_invalid_codepoint_is_false() {
        // -1 cast to u32 = 0xFFFFFFFF, not a valid Unicode scalar
        // SAFETY: hew_unicode_is_upper takes only an i32; no pointers.
        assert!(!unsafe { hew_unicode_is_upper(-1) });
    }

    #[test]
    fn unicode_is_lower_ascii_lowercase() {
        // 'a' = 0x61
        // SAFETY: hew_unicode_is_lower takes only an i32; no pointers.
        assert!(unsafe { hew_unicode_is_lower(0x61) });
    }

    #[test]
    fn unicode_is_lower_ascii_uppercase_is_false() {
        // SAFETY: hew_unicode_is_lower takes only an i32; no pointers.
        assert!(!unsafe { hew_unicode_is_lower(0x41) });
    }

    #[test]
    fn unicode_is_lower_latin_extended() {
        // 'é' (U+00E9) is lowercase
        // SAFETY: hew_unicode_is_lower takes only an i32; no pointers.
        assert!(unsafe { hew_unicode_is_lower(0xE9) });
    }

    #[test]
    fn unicode_is_space_ascii_space() {
        // SAFETY: hew_unicode_is_space takes only an i32; no pointers.
        assert!(unsafe { hew_unicode_is_space(0x20) });
    }

    #[test]
    fn unicode_is_space_tab_and_newline() {
        // SAFETY: hew_unicode_is_space takes only an i32; no pointers.
        assert!(unsafe { hew_unicode_is_space(0x09) }); // \t
                                                        // SAFETY: hew_unicode_is_space takes only an i32; no pointers.
        assert!(unsafe { hew_unicode_is_space(0x0A) }); // \n
    }

    #[test]
    fn unicode_is_space_letter_is_false() {
        // SAFETY: hew_unicode_is_space takes only an i32; no pointers.
        assert!(!unsafe { hew_unicode_is_space(0x41) }); // 'A'
    }

    #[test]
    fn unicode_is_digit_zero_through_nine() {
        for cp in 0x30..=0x39 {
            // SAFETY: hew_unicode_is_digit takes only an i32; no pointers.
            assert!(unsafe { hew_unicode_is_digit(cp) });
        }
    }

    #[test]
    fn unicode_is_digit_letter_is_false() {
        // SAFETY: hew_unicode_is_digit takes only an i32; no pointers.
        assert!(!unsafe { hew_unicode_is_digit(0x41) }); // 'A'
    }

    #[test]
    fn unicode_is_letter_ascii_alpha() {
        // SAFETY: hew_unicode_is_letter takes only an i32; no pointers.
        assert!(unsafe { hew_unicode_is_letter(0x41) }); // 'A'
                                                         // SAFETY: hew_unicode_is_letter takes only an i32; no pointers.
        assert!(unsafe { hew_unicode_is_letter(0x61) }); // 'a'
    }

    #[test]
    fn unicode_is_letter_cjk() {
        // CJK Unified Ideographs block — 日 = U+65E5
        // SAFETY: hew_unicode_is_letter takes only an i32; no pointers.
        assert!(unsafe { hew_unicode_is_letter(0x65E5) });
    }

    #[test]
    fn unicode_is_letter_digit_is_false() {
        // SAFETY: hew_unicode_is_letter takes only an i32; no pointers.
        assert!(!unsafe { hew_unicode_is_letter(0x31) }); // '1'
    }

    #[test]
    fn unicode_is_alnum_letter_and_digit() {
        // SAFETY: hew_unicode_is_alnum takes only an i32; no pointers.
        assert!(unsafe { hew_unicode_is_alnum(0x41) }); // 'A'
                                                        // SAFETY: hew_unicode_is_alnum takes only an i32; no pointers.
        assert!(unsafe { hew_unicode_is_alnum(0x31) }); // '1'
                                                        // SAFETY: hew_unicode_is_alnum takes only an i32; no pointers.
        assert!(!unsafe { hew_unicode_is_alnum(0x20) }); // space
    }

    /// `hew_unicode_is_punct` must answer for the Unicode `P*` category group
    /// exactly, over every scalar value, not for a hand-maintained subset of
    /// ranges. The pinned Unicode 17 tables are the authority.
    #[test]
    fn unicode_is_punct_matches_every_p_category_scalar() {
        use finl_unicode::categories::{CharacterCategories, MinorCategory};

        let mut punct_count = 0_u32;
        let mut categories_seen: Vec<MinorCategory> = Vec::new();
        for cp in 0..=0x0010_ffff_u32 {
            let Some(c) = char::from_u32(cp) else {
                continue;
            };
            let minor = c.get_minor_category();
            let expected = matches!(
                minor,
                MinorCategory::Pc
                    | MinorCategory::Pd
                    | MinorCategory::Ps
                    | MinorCategory::Pe
                    | MinorCategory::Pi
                    | MinorCategory::Pf
                    | MinorCategory::Po
            );
            // SAFETY: hew_unicode_is_punct takes only an i32; no pointers.
            let actual = unsafe { hew_unicode_is_punct(cp.cast_signed()) };
            assert_eq!(
                actual, expected,
                "U+{cp:04X} punctuation classification disagrees with the Unicode tables"
            );
            if expected {
                punct_count += 1;
                if !categories_seen.contains(&minor) {
                    categories_seen.push(minor);
                }
            }
        }
        assert_eq!(
            categories_seen.len(),
            7,
            "every P* minor category (Pc, Pd, Ps, Pe, Pi, Pf, Po) must be represented \
             in the sweep, saw {categories_seen:?}"
        );
        assert_eq!(
            punct_count, 856,
            "Unicode 17 defines 856 P* scalars; a different count means the pinned tables moved"
        );
    }

    #[test]
    fn unicode_is_punct_refuses_scalars_that_are_not_codepoints() {
        for cp in [-1_i32, i32::MIN, 0xD800, 0xDFFF, 0x0011_0000, i32::MAX] {
            // SAFETY: hew_unicode_is_punct takes only an i32; no pointers.
            let is_punct = unsafe { hew_unicode_is_punct(cp) };
            assert!(!is_punct, "{cp} is not a Unicode scalar value");
        }
    }

    #[test]
    fn unicode_to_upper_ascii() {
        // 'a' -> 'A'
        // SAFETY: hew_unicode_to_upper takes only an i32; no pointers.
        assert_eq!(unsafe { hew_unicode_to_upper(0x61) }, 0x41);
    }

    #[test]
    fn unicode_to_upper_already_upper() {
        // 'A' stays 'A'
        // SAFETY: hew_unicode_to_upper takes only an i32; no pointers.
        assert_eq!(unsafe { hew_unicode_to_upper(0x41) }, 0x41);
    }

    #[test]
    fn unicode_to_upper_latin_extended() {
        // 'é' (U+00E9) -> 'É' (U+00C9)
        // SAFETY: hew_unicode_to_upper takes only an i32; no pointers.
        assert_eq!(unsafe { hew_unicode_to_upper(0xE9) }, 0xC9);
    }

    #[test]
    fn unicode_to_upper_invalid_returns_unchanged() {
        // SAFETY: hew_unicode_to_upper takes only an i32; no pointers.
        assert_eq!(unsafe { hew_unicode_to_upper(-1) }, -1);
    }

    #[test]
    fn unicode_to_lower_ascii() {
        // 'A' -> 'a'
        // SAFETY: hew_unicode_to_lower takes only an i32; no pointers.
        assert_eq!(unsafe { hew_unicode_to_lower(0x41) }, 0x61);
    }

    #[test]
    fn unicode_to_lower_already_lower() {
        // SAFETY: hew_unicode_to_lower takes only an i32; no pointers.
        assert_eq!(unsafe { hew_unicode_to_lower(0x61) }, 0x61);
    }

    #[test]
    fn unicode_to_lower_latin_extended() {
        // 'É' (U+00C9) -> 'é' (U+00E9)
        // SAFETY: hew_unicode_to_lower takes only an i32; no pointers.
        assert_eq!(unsafe { hew_unicode_to_lower(0xC9) }, 0xE9);
    }

    #[test]
    fn unicode_to_lower_invalid_returns_unchanged() {
        // SAFETY: hew_unicode_to_lower takes only an i32; no pointers.
        assert_eq!(unsafe { hew_unicode_to_lower(-1) }, -1);
    }

    // ── hew_char_to_string UTF-8 correctness ─────────────────────────────────

    #[test]
    fn char_to_string_ascii() {
        // 'a' = 0x61 — single-byte UTF-8; result must be exactly "a".
        // SAFETY: hew_char_to_string takes only an i32; no pointers.
        let s = unsafe { hew_char_to_string(0x61) };
        assert!(!s.is_null());
        // SAFETY: s is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(s) }, "a");
    }

    #[test]
    fn char_to_string_latin_extended() {
        // U+00E9 'é' — two UTF-8 bytes (0xC3, 0xA9).
        // The old implementation emitted only 0xE9 (a single invalid UTF-8 byte).
        // SAFETY: hew_char_to_string takes only an i32; no pointers.
        let s = unsafe { hew_char_to_string(0x00E9) };
        assert!(!s.is_null());
        // SAFETY: s is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(s) }, "é");
    }

    #[test]
    fn char_to_string_cjk() {
        // U+4E2D '中' — three UTF-8 bytes.
        // SAFETY: hew_char_to_string takes only an i32; no pointers.
        let s = unsafe { hew_char_to_string(0x4E2D) };
        assert!(!s.is_null());
        // SAFETY: s is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(s) }, "中");
    }

    #[test]
    fn char_to_string_emoji() {
        // U+1F600 '😀' — four UTF-8 bytes.
        // SAFETY: hew_char_to_string takes only an i32; no pointers.
        let s = unsafe { hew_char_to_string(0x1F600) };
        assert!(!s.is_null());
        // SAFETY: s is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(s) }, "😀");
    }

    #[test]
    fn char_to_string_invalid_codepoint_yields_replacement() {
        // 0xD800 is a surrogate half — not a valid Unicode scalar.
        // Must produce U+FFFD rather than garbage bytes.
        // SAFETY: hew_char_to_string takes only an i32; no pointers.
        let s = unsafe { hew_char_to_string(0xD800) };
        assert!(!s.is_null());
        // SAFETY: s is a valid malloc'd C string.
        assert_eq!(unsafe { read_and_free(s) }, "\u{FFFD}");
    }
}
