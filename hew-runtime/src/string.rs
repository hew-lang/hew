//! Hew runtime: `string` module.
//!
//! String operations exposed with C ABI for use by compiled Hew programs.
//! All returned strings are allocated with `libc::malloc` so callers can free
//! them with `libc::free`.

use crate::cabi::{cstr_to_str, malloc_cstring};
use std::ffi::CStr;
use std::os::raw::c_char;

/// Helper: get byte length of a C string, returning 0 for null pointers.
///
/// # Safety
///
/// `s` must be null or a valid NUL-terminated C string.
unsafe fn cstr_len(s: *const c_char) -> usize {
    if s.is_null() {
        return 0;
    }
    // SAFETY: Caller guarantees s is a valid NUL-terminated C string.
    unsafe { libc::strlen(s) }
}

/// Concatenate two C strings. Caller must `free` the result.
///
/// # Safety
///
/// Both `a` and `b` must be valid NUL-terminated C strings (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_string_concat(a: *const c_char, b: *const c_char) -> *mut c_char {
    // SAFETY: cstr_len handles null check internally; a and b are valid per contract.
    let la = unsafe { cstr_len(a) };
    // SAFETY: cstr_len handles null check internally; b is valid per contract.
    let lb = unsafe { cstr_len(b) };
    let total = la + lb;
    // SAFETY: Requesting total+1 bytes from malloc.
    let result = unsafe { libc::malloc(total + 1) }.cast::<u8>();
    if result.is_null() {
        return result.cast::<c_char>();
    }
    if la > 0 {
        // SAFETY: a is valid for la bytes; result has total+1 bytes allocated.
        unsafe { std::ptr::copy_nonoverlapping(a.cast::<u8>(), result, la) };
    }
    if lb > 0 {
        // SAFETY: b is valid for lb bytes; result+la is within the allocation.
        unsafe { std::ptr::copy_nonoverlapping(b.cast::<u8>(), result.add(la), lb) };
    }
    // SAFETY: result + total is within the allocated region of total+1 bytes.
    unsafe { *result.add(total) = 0 };
    result.cast::<c_char>()
}

/// Extract a substring `[start, end)`. Caller must `free` the result.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    reason = "Matching C ABI: string lengths are bounded by i32 in the Hew runtime"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_string_slice(s: *const c_char, start: i32, end: i32) -> *mut c_char {
    if s.is_null() {
        // SAFETY: malloc_copy with len=0 is safe regardless of src.
        return unsafe { malloc_cstring(core::ptr::null(), 0) };
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    let len = unsafe { cstr_len(s) } as i32;
    let start = start.max(0);
    let end = end.min(len);
    if start >= end {
        // SAFETY: Allocating an empty string.
        return unsafe { malloc_cstring(core::ptr::null(), 0) };
    }
    let slice_len = (end - start) as usize;
    // SAFETY: start is non-negative and < len, so s + start is within the string;
    // slice_len bytes are readable because end <= len.
    unsafe { malloc_cstring(s.cast::<u8>().add(start as usize), slice_len) }
}

/// Find the first occurrence of `substr` in `s`. Returns byte offset or -1.
///
/// # Safety
///
/// Both `s` and `substr` must be valid NUL-terminated C strings (or null).
#[expect(
    clippy::cast_possible_truncation,
    reason = "String offsets are bounded by i32 in the Hew runtime ABI"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_string_find(s: *const c_char, substr: *const c_char) -> i32 {
    if s.is_null() || substr.is_null() {
        return -1;
    }
    // SAFETY: Both pointers are valid NUL-terminated C strings per caller contract.
    let p = unsafe { libc::strstr(s, substr) };
    if p.is_null() {
        return -1;
    }
    // SAFETY: p points within s, so the offset is non-negative and fits in isize.
    unsafe { p.offset_from(s) as i32 }
}

/// Check if `s` starts with `prefix`.
///
/// # Safety
///
/// Both `s` and `prefix` must be valid NUL-terminated C strings (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_string_starts_with(s: *const c_char, prefix: *const c_char) -> bool {
    if s.is_null() || prefix.is_null() {
        return false;
    }
    // SAFETY: Both pointers are valid NUL-terminated C strings per caller contract.
    let plen = unsafe { cstr_len(prefix) };
    // SAFETY: s and prefix are valid C strings; plen is derived from prefix.
    unsafe { libc::strncmp(s, prefix, plen) == 0 }
}

/// Check if `s` ends with `suffix`.
///
/// # Safety
///
/// Both `s` and `suffix` must be valid NUL-terminated C strings (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_string_ends_with(s: *const c_char, suffix: *const c_char) -> bool {
    if s.is_null() || suffix.is_null() {
        return false;
    }
    // SAFETY: Both pointers are valid NUL-terminated C strings per caller contract.
    let slen = unsafe { cstr_len(s) };
    // SAFETY: suffix is a valid NUL-terminated C string per caller contract.
    let xlen = unsafe { cstr_len(suffix) };
    if xlen > slen {
        return false;
    }
    // SAFETY: s + (slen - xlen) is within the string; suffix is a valid C string.
    unsafe { libc::strcmp(s.add(slen - xlen), suffix) == 0 }
}

/// Check if `s` contains `substr`.
///
/// # Safety
///
/// Both `s` and `substr` must be valid NUL-terminated C strings (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_string_contains(s: *const c_char, substr: *const c_char) -> bool {
    if s.is_null() || substr.is_null() {
        return false;
    }
    // SAFETY: Both pointers are valid NUL-terminated C strings per caller contract.
    !unsafe { libc::strstr(s, substr) }.is_null()
}

/// Convert an `i32` to its decimal string representation. Caller must `free`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_int_to_string(n: i32) -> *mut c_char {
    // A 32-bit integer is at most 11 characters (including sign) plus NUL.
    let mut buf = [0u8; 32];
    // SAFETY: buf is a valid 32-byte buffer; format string is a valid C literal.
    unsafe {
        libc::snprintf(
            buf.as_mut_ptr().cast::<c_char>(),
            buf.len(),
            c"%d".as_ptr(),
            n,
        )
    };
    // SAFETY: snprintf NUL-terminates the buffer; we measure the resulting length.
    let len = unsafe { libc::strlen(buf.as_ptr().cast::<c_char>()) };
    // SAFETY: buf contains len valid bytes from snprintf output.
    unsafe { malloc_cstring(buf.as_ptr(), len) }
}

/// Convert an `i64` to its decimal string representation. Caller must `free`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_i64_to_string(n: i64) -> *mut c_char {
    // A 64-bit integer is at most 20 characters (including sign) plus NUL.
    let mut buf = [0u8; 32];
    // SAFETY: buf is a valid 32-byte buffer; format string is a valid C literal.
    unsafe {
        libc::snprintf(
            buf.as_mut_ptr().cast::<c_char>(),
            buf.len(),
            c"%lld".as_ptr(),
            n,
        )
    };
    // SAFETY: snprintf NUL-terminates the buffer; we measure the resulting length.
    let len = unsafe { libc::strlen(buf.as_ptr().cast::<c_char>()) };
    // SAFETY: buf contains len valid bytes from snprintf output.
    unsafe { malloc_cstring(buf.as_ptr(), len) }
}

/// Parse a C string as an `i32` (like C `atoi`).
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null, which returns 0).
#[no_mangle]
pub unsafe extern "C" fn hew_string_to_int(s: *const c_char) -> i32 {
    if s.is_null() {
        return 0;
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    unsafe { libc::atoi(s) }
}

/// Convert an `f64` to its string representation. Caller must `free`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_float_to_string(f: f64) -> *mut c_char {
    // An f64 in %g format is at most ~25 characters plus NUL.
    let mut buf = [0u8; 64];
    // SAFETY: buf is a valid 64-byte buffer; format string is a valid C literal.
    unsafe {
        libc::snprintf(
            buf.as_mut_ptr().cast::<c_char>(),
            buf.len(),
            c"%g".as_ptr(),
            f,
        )
    };
    // SAFETY: snprintf NUL-terminates the buffer; we measure the resulting length.
    let len = unsafe { libc::strlen(buf.as_ptr().cast::<c_char>()) };
    // SAFETY: buf contains len valid bytes from snprintf output.
    unsafe { malloc_cstring(buf.as_ptr(), len) }
}

/// Convert a `bool` to `"true"` or `"false"`. Caller must `free`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_bool_to_string(b: bool) -> *mut c_char {
    let s = if b { "true" } else { "false" };
    // SAFETY: s points to valid static string bytes with known length.
    unsafe { malloc_cstring(s.as_ptr(), s.len()) }
}

/// Trim leading and trailing ASCII whitespace. Caller must `free`.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_string_trim(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        // SAFETY: Allocating an empty string with len=0.
        return unsafe { malloc_cstring(core::ptr::null(), 0) };
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    let bytes = unsafe { CStr::from_ptr(s) }.to_bytes();
    let start = bytes
        .iter()
        .position(|&b| !b.is_ascii_whitespace())
        .unwrap_or(bytes.len());
    let end = bytes
        .iter()
        .rposition(|&b| !b.is_ascii_whitespace())
        .map_or(start, |i| i + 1);
    let trimmed = &bytes[start..end];
    // SAFETY: trimmed points into the valid CStr buffer with trimmed.len() bytes.
    unsafe { malloc_cstring(trimmed.as_ptr(), trimmed.len()) }
}

/// Replace all occurrences of `old_str` with `new_str`. Caller must `free`.
///
/// # Safety
///
/// All three pointers must be valid NUL-terminated C strings (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_string_replace(
    s: *const c_char,
    old_str: *const c_char,
    new_str: *const c_char,
) -> *mut c_char {
    if s.is_null() {
        // SAFETY: Allocating an empty string with len=0.
        return unsafe { malloc_cstring(core::ptr::null(), 0) };
    }
    // SAFETY: s is valid per caller contract.
    let slen = unsafe { cstr_len(s) };
    let olen = if old_str.is_null() {
        0
    } else {
        // SAFETY: old_str is non-null and valid per caller contract.
        unsafe { cstr_len(old_str) }
    };
    let nlen = if new_str.is_null() {
        0
    } else {
        // SAFETY: new_str is non-null and valid per caller contract.
        unsafe { cstr_len(new_str) }
    };

    // If old_str is empty or null, just duplicate s.
    if olen == 0 {
        // SAFETY: s is valid for slen bytes per caller contract.
        return unsafe { malloc_cstring(s.cast::<u8>(), slen) };
    }

    // Count occurrences.
    let mut count: usize = 0;
    let mut p = s;
    loop {
        // SAFETY: p is within the original string; old_str is valid and non-empty.
        let q = unsafe { libc::strstr(p, old_str) };
        if q.is_null() {
            break;
        }
        count += 1;
        // SAFETY: q points into s; advancing by olen stays within bounds.
        p = unsafe { q.add(olen) };
    }

    let result_len = slen + count * nlen - count * olen;
    // SAFETY: Allocating result_len + 1 bytes via malloc.
    let result = unsafe { libc::malloc(result_len + 1) }.cast::<u8>();
    if result.is_null() {
        return result.cast::<c_char>();
    }

    let mut dst = result;
    p = s;
    loop {
        // SAFETY: p is within the original string; old_str is valid and non-empty.
        let q = unsafe { libc::strstr(p, old_str) };
        if q.is_null() {
            break;
        }
        // SAFETY: q >= p since q was found at or after p; cast_unsigned is correct.
        let chunk = unsafe { q.offset_from(p) }.cast_unsigned();
        if chunk > 0 {
            // SAFETY: p is valid for chunk bytes; dst has enough space in the allocation.
            unsafe { std::ptr::copy_nonoverlapping(p.cast::<u8>(), dst, chunk) };
            // SAFETY: Advancing dst within the allocated buffer.
            dst = unsafe { dst.add(chunk) };
        }
        if nlen > 0 {
            // SAFETY: new_str is valid for nlen bytes; dst has enough space.
            unsafe { std::ptr::copy_nonoverlapping(new_str.cast::<u8>(), dst, nlen) };
            // SAFETY: Advancing dst within the allocated buffer.
            dst = unsafe { dst.add(nlen) };
        }
        // SAFETY: q + olen is within the original string.
        p = unsafe { q.add(olen) };
    }
    // Copy the remaining tail (including NUL terminator).
    // SAFETY: p points to the remaining portion of s.
    let tail_len = unsafe { cstr_len(p) };
    if tail_len > 0 {
        // SAFETY: p is valid for tail_len bytes; dst has enough space.
        unsafe { std::ptr::copy_nonoverlapping(p.cast::<u8>(), dst, tail_len) };
        // SAFETY: Advancing dst within the allocated buffer.
        dst = unsafe { dst.add(tail_len) };
    }
    // SAFETY: dst is at the end of written content, within the allocation.
    unsafe { *dst = 0 };
    result.cast::<c_char>()
}

/// Convert a single character code point to a one-byte C string. Caller must `free`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions beyond a valid `i32`.
#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    reason = "C ABI passes characters as i32; truncation to u8 is intentional (matching C cast)"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_char_to_string(c: i32) -> *mut c_char {
    // SAFETY: Allocating 2 bytes via malloc.
    let ptr = unsafe { libc::malloc(2) }.cast::<u8>();
    if ptr.is_null() {
        return ptr.cast::<c_char>();
    }
    // SAFETY: ptr is a valid 2-byte allocation.
    unsafe {
        *ptr = c as u8;
        *ptr.add(1) = 0;
    }
    ptr.cast::<c_char>()
}

/// Return the byte length of a C string.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null, which returns 0).
#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    reason = "Matching C ABI: strlen result is returned as i32 per Hew convention"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_string_length(s: *const c_char) -> i32 {
    // SAFETY: cstr_len handles null check internally.
    unsafe { cstr_len(s) as i32 }
}

/// Compare two C strings for equality. Returns 1 if equal, 0 otherwise.
///
/// # Safety
///
/// Both `a` and `b` must be valid NUL-terminated C strings (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_string_equals(a: *const c_char, b: *const c_char) -> i32 {
    if a.is_null() && b.is_null() {
        return 1;
    }
    if a.is_null() || b.is_null() {
        return 0;
    }
    // SAFETY: Both pointers are valid NUL-terminated C strings per caller contract.
    i32::from(unsafe { libc::strcmp(a, b) } == 0)
}

/// Split a string by `delim` into a `HewVec` of strings. Caller must free
/// the returned vec with [`crate::vec::hew_vec_free`].
///
/// # Safety
///
/// Both `s` and `delim` must be valid NUL-terminated C strings (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_string_split(
    s: *const c_char,
    delim: *const c_char,
) -> *mut crate::vec::HewVec {
    // SAFETY: hew_vec_new_str has no preconditions.
    let v = unsafe { crate::vec::hew_vec_new_str() };
    if s.is_null() {
        return v;
    }
    if delim.is_null() {
        // SAFETY: s is valid per caller contract.
        unsafe { crate::vec::hew_vec_push_str(v, s) };
        return v;
    }
    // SAFETY: Both pointers are valid NUL-terminated C strings per caller contract.
    let s_bytes = unsafe { CStr::from_ptr(s) }.to_bytes();
    // SAFETY: delim is a valid NUL-terminated C string per caller contract.
    let d_bytes = unsafe { CStr::from_ptr(delim) }.to_bytes();
    let dlen = d_bytes.len();

    if dlen == 0 {
        // SAFETY: s is valid per caller contract.
        unsafe { crate::vec::hew_vec_push_str(v, s) };
        return v;
    }

    let mut start = 0;
    while start <= s_bytes.len() {
        let rest = &s_bytes[start..];
        let found = rest.windows(dlen).position(|w| w == d_bytes);
        if let Some(pos) = found {
            // SAFETY: Allocating a substring via malloc_copy and pushing.
            let part = unsafe { malloc_cstring(s_bytes[start..start + pos].as_ptr(), pos) };
            // SAFETY: v is a valid HewVec and part is a valid C string.
            unsafe { crate::vec::hew_vec_push_str(v, part) };
            // SAFETY: part was allocated by malloc_copy.
            unsafe { libc::free(part.cast()) };
            start += pos + dlen;
        } else {
            let tail_len = s_bytes.len() - start;
            // SAFETY: Tail slice is within s_bytes bounds.
            let part = unsafe { malloc_cstring(s_bytes[start..].as_ptr(), tail_len) };
            // SAFETY: v is a valid HewVec and part is a valid C string.
            unsafe { crate::vec::hew_vec_push_str(v, part) };
            // SAFETY: part was allocated by malloc_copy.
            unsafe { libc::free(part.cast()) };
            break;
        }
    }
    v
}

/// Convert a C string to ASCII lowercase. Caller must `free` the result.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_string_to_lowercase(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        // SAFETY: Allocating an empty string.
        return unsafe { malloc_cstring(core::ptr::null(), 0) };
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    let bytes = unsafe { CStr::from_ptr(s) }.to_bytes();
    let len = bytes.len();
    // SAFETY: Allocating len+1 bytes.
    let result = unsafe { libc::malloc(len + 1) }.cast::<u8>();
    if result.is_null() {
        return result.cast::<c_char>();
    }
    for (i, &b) in bytes.iter().enumerate() {
        // SAFETY: i < len, result is valid for len bytes.
        unsafe { *result.add(i) = b.to_ascii_lowercase() };
    }
    // SAFETY: NUL-terminating the result.
    unsafe { *result.add(len) = 0 };
    result.cast::<c_char>()
}

/// Convert a C string to ASCII uppercase. Caller must `free` the result.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_string_to_uppercase(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        // SAFETY: Allocating an empty string.
        return unsafe { malloc_cstring(core::ptr::null(), 0) };
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    let bytes = unsafe { CStr::from_ptr(s) }.to_bytes();
    let len = bytes.len();
    // SAFETY: Allocating len+1 bytes.
    let result = unsafe { libc::malloc(len + 1) }.cast::<u8>();
    if result.is_null() {
        return result.cast::<c_char>();
    }
    for (i, &b) in bytes.iter().enumerate() {
        // SAFETY: i < len, result is valid for len bytes.
        unsafe { *result.add(i) = b.to_ascii_uppercase() };
    }
    // SAFETY: NUL-terminating the result.
    unsafe { *result.add(len) = 0 };
    result.cast::<c_char>()
}

/// Return the byte at `idx` as an `i32`, or -1 if out of bounds.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[expect(
    clippy::cast_sign_loss,
    reason = "Matching C ABI: index and return value are i32 per Hew convention"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_string_char_at(s: *const c_char, idx: i32) -> i32 {
    if s.is_null() || idx < 0 {
        return -1;
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    let len = unsafe { cstr_len(s) };
    let i = idx as usize;
    if i >= len {
        return -1;
    }
    // SAFETY: i < len, so s + i is within the string.
    i32::from(unsafe { *s.cast::<u8>().add(i) })
}

/// Create a one-character string from a character code. Caller must `free`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions beyond a valid `i32`.
#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    reason = "C ABI passes characters as i32; truncation to u8 is intentional"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_string_from_char(c: i32) -> *mut c_char {
    // SAFETY: Allocating 2 bytes via malloc.
    let ptr = unsafe { libc::malloc(2) }.cast::<u8>();
    if ptr.is_null() {
        return ptr.cast::<c_char>();
    }
    // SAFETY: ptr is a valid 2-byte allocation.
    unsafe {
        *ptr = c as u8;
        *ptr.add(1) = 0;
    }
    ptr.cast::<c_char>()
}

/// Repeat a string `count` times. Caller must `free` the result.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[expect(
    clippy::cast_sign_loss,
    reason = "count is validated to be non-negative before cast"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_string_repeat(s: *const c_char, count: i32) -> *mut c_char {
    if s.is_null() || count <= 0 {
        // SAFETY: Allocating an empty string.
        return unsafe { malloc_cstring(core::ptr::null(), 0) };
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    let len = unsafe { cstr_len(s) };
    let n = count as usize;
    let total = len * n;
    // SAFETY: Allocating total+1 bytes.
    let result = unsafe { libc::malloc(total + 1) }.cast::<u8>();
    if result.is_null() {
        return result.cast::<c_char>();
    }
    for i in 0..n {
        // SAFETY: Each copy writes len bytes at offset i*len, within total bytes.
        unsafe { std::ptr::copy_nonoverlapping(s.cast::<u8>(), result.add(i * len), len) };
    }
    // SAFETY: NUL-terminating.
    unsafe { *result.add(total) = 0 };
    result.cast::<c_char>()
}

/// Find the first occurrence of `substr` in `s` starting at position `from`.
/// Returns byte offset or -1 if not found.
///
/// # Safety
///
/// Both `s` and `substr` must be valid NUL-terminated C strings (or null).
#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    reason = "Matching C ABI: string offsets are i32 per Hew convention"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_string_index_of(
    s: *const c_char,
    substr: *const c_char,
    from: i32,
) -> i32 {
    if s.is_null() || substr.is_null() {
        return -1;
    }
    // SAFETY: Both pointers are valid NUL-terminated C strings per caller contract.
    let slen = unsafe { cstr_len(s) };
    let start = if from < 0 { 0usize } else { from as usize };
    if start >= slen {
        // SAFETY: substr is a valid NUL-terminated C string per caller contract.
        let sublen = unsafe { cstr_len(substr) };
        if sublen == 0 && start == slen {
            return start as i32;
        }
        return -1;
    }
    // SAFETY: start < slen, so s + start is within the string.
    let p = unsafe { libc::strstr(s.add(start), substr) };
    if p.is_null() {
        return -1;
    }
    // SAFETY: p points within s, so the offset is non-negative.
    unsafe { p.offset_from(s) as i32 }
}

// Linker-provided symbols marking the extent of the loaded binary.
// String literals in `.rodata` fall within these bounds — we must not
// free them.

#[cfg(not(target_arch = "wasm32"))]
unsafe extern "C" {
    #[link_name = "__executable_start"]
    static EXEC_START: u8;
    #[link_name = "_end"]
    static EXEC_END: u8;
}

/// Returns `true` if `ptr` points into the binary's loaded segments
/// (text, rodata, data, bss).  Such pointers must never be passed to
/// `free`.
#[cfg(not(target_arch = "wasm32"))]
fn is_static_string(ptr: *const u8) -> bool {
    let addr = ptr as usize;
    // SAFETY: These are linker-defined symbols provided by the ELF linker;
    // taking their address is safe and gives the loaded extent of the binary.
    let start = (&raw const EXEC_START) as usize;
    let end = (&raw const EXEC_END) as usize;
    addr >= start && addr < end
}

/// WASM version: static data lives below `__heap_base` in linear memory.
/// Anything at or above `__heap_base` was allocated by `malloc`.
#[cfg(target_arch = "wasm32")]
fn is_static_string(ptr: *const u8) -> bool {
    unsafe extern "C" {
        static __heap_base: u8;
    }
    let addr = ptr as usize;
    let heap_start = (&raw const __heap_base) as usize;
    addr < heap_start
}

/// Free a string if it was heap-allocated.  Safe to call with null or
/// with pointers to string literals embedded in the binary — those are
/// detected via linker symbols and silently skipped.
///
/// # Safety
///
/// `s` must be null, a pointer into the binary's read-only data, or a
/// pointer previously returned by `malloc` / `hew_string_*` allocating
/// functions.
#[no_mangle]
pub unsafe extern "C" fn hew_string_drop(s: *mut c_char) {
    if s.is_null() || is_static_string(s.cast()) {
        return;
    }
    // SAFETY: Not null and not a static string — must be heap-allocated.
    unsafe { libc::free(s.cast()) };
}

// ---------------------------------------------------------------------------
// UTF-8 aware string operations
// ---------------------------------------------------------------------------

/// Count Unicode codepoints (not bytes) in a C string.
///
/// For ASCII strings this equals the byte length. Returns 0 for null or
/// invalid UTF-8.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    reason = "Codepoint count is bounded by byte length which fits in i32 for Hew strings"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_string_char_count(s: *const c_char) -> i32 {
    // SAFETY: Caller guarantees s is valid or null.
    let Some(rust_str) = (unsafe { cstr_to_str(s) }) else {
        return 0;
    };
    rust_str.chars().count() as i32
}

/// Return the byte length of a C string (explicit alias for
/// [`hew_string_length`]).
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null, which returns 0).
#[no_mangle]
pub unsafe extern "C" fn hew_string_byte_length(s: *const c_char) -> i32 {
    // SAFETY: Forwarding to hew_string_length with same contract.
    unsafe { hew_string_length(s) }
}

/// Returns 1 if all bytes are ASCII, 0 otherwise. Returns 1 for null (vacuous
/// truth).
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_string_is_ascii(s: *const c_char) -> i32 {
    if s.is_null() {
        return 1;
    }
    // SAFETY: Caller guarantees s is a valid NUL-terminated C string.
    let bytes = unsafe { CStr::from_ptr(s) }.to_bytes();
    i32::from(bytes.is_ascii())
}

/// Get the Unicode codepoint at the given codepoint index. Returns -1 if
/// `s` is null, not valid UTF-8, or `index` is out of bounds.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[expect(
    clippy::cast_sign_loss,
    reason = "index is validated to be non-negative before cast to usize"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_string_char_at_utf8(s: *const c_char, index: i32) -> i32 {
    if index < 0 {
        return -1;
    }
    // SAFETY: Caller guarantees s is valid or null.
    let Some(rust_str) = (unsafe { cstr_to_str(s) }) else {
        return -1;
    };
    match rust_str.chars().nth(index as usize) {
        Some(ch) => ch as i32,
        None => -1,
    }
}

/// Slice a UTF-8 string by codepoint indices `[start, end)`. Returns a
/// heap-allocated C string. Returns null on error (null input, invalid UTF-8,
/// or invalid indices).
///
/// Caller must `free` the result.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[expect(
    clippy::cast_sign_loss,
    reason = "start and end are validated to be non-negative before cast to usize"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_string_substring_utf8(
    s: *const c_char,
    start: i32,
    end: i32,
) -> *mut c_char {
    if start < 0 || end < 0 || start > end {
        return core::ptr::null_mut();
    }
    // SAFETY: Caller guarantees s is valid or null.
    let Some(rust_str) = (unsafe { cstr_to_str(s) }) else {
        return core::ptr::null_mut();
    };
    let result: String = rust_str
        .chars()
        .skip(start as usize)
        .take((end - start) as usize)
        .collect();
    let bytes = result.as_bytes();
    // SAFETY: bytes points to valid UTF-8 with known length.
    unsafe { malloc_cstring(bytes.as_ptr(), bytes.len()) }
}

/// Reverse a UTF-8 string by codepoints (not bytes). Returns a heap-allocated
/// C string. Returns null for null input or invalid UTF-8.
///
/// Caller must `free` the result.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_string_reverse_utf8(s: *const c_char) -> *mut c_char {
    // SAFETY: Caller guarantees s is valid or null.
    let Some(rust_str) = (unsafe { cstr_to_str(s) }) else {
        return core::ptr::null_mut();
    };
    let reversed: String = rust_str.chars().rev().collect();
    let bytes = reversed.as_bytes();
    // SAFETY: bytes points to valid UTF-8 with known length.
    unsafe { malloc_cstring(bytes.as_ptr(), bytes.len()) }
}

/// Convert a string to a `HewVec` of raw bytes (`u8`). Caller must free the
/// returned vec with [`crate::vec::hew_vec_free`].
///
/// Returns an empty vec for null input.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_string_to_bytes(s: *const c_char) -> *mut crate::vec::HewVec {
    // SAFETY: hew_vec_new creates a Vec<i32>-style HewVec, matching what
    // hew_tcp_write / hew_bytes_to_string expect (i32-element vecs).
    let v = unsafe { crate::vec::hew_vec_new() };
    if s.is_null() {
        return v;
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    let bytes = unsafe { CStr::from_ptr(s) }.to_bytes();
    for &b in bytes {
        // SAFETY: v is a valid HewVec; push each byte as i32 to match
        // the convention used by hew_tcp_read and hew_bytes_to_string.
        unsafe {
            crate::vec::hew_vec_push_i32(v, i32::from(b));
        };
    }
    v
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    unsafe fn read_and_free(ptr: *mut c_char) -> String {
        if ptr.is_null() {
            return String::new();
        }
        // SAFETY: ptr is a valid malloc'd NUL-terminated C string.
        let s = unsafe { CStr::from_ptr(ptr) }
            .to_string_lossy()
            .into_owned();
        unsafe { libc::free(ptr.cast()) };
        s
    }

    #[test]
    fn test_string_concat_basic() {
        let a = CString::new("hello ").unwrap();
        let b = CString::new("world").unwrap();
        let result = unsafe { hew_string_concat(a.as_ptr(), b.as_ptr()) };
        assert_eq!(unsafe { read_and_free(result) }, "hello world");
    }

    #[test]
    fn test_string_concat_empty_both() {
        let a = CString::new("").unwrap();
        let b = CString::new("").unwrap();
        let result = unsafe { hew_string_concat(a.as_ptr(), b.as_ptr()) };
        assert_eq!(unsafe { read_and_free(result) }, "");
    }

    #[test]
    fn test_string_concat_null_left() {
        let b = CString::new("world").unwrap();
        let result = unsafe { hew_string_concat(core::ptr::null(), b.as_ptr()) };
        assert_eq!(unsafe { read_and_free(result) }, "world");
    }

    #[test]
    fn test_string_concat_null_right() {
        let a = CString::new("hello").unwrap();
        let result = unsafe { hew_string_concat(a.as_ptr(), core::ptr::null()) };
        assert_eq!(unsafe { read_and_free(result) }, "hello");
    }

    #[test]
    fn test_string_slice_basic() {
        let s = CString::new("hello world").unwrap();
        let result = unsafe { hew_string_slice(s.as_ptr(), 0, 5) };
        assert_eq!(unsafe { read_and_free(result) }, "hello");
    }

    #[test]
    fn test_string_slice_null() {
        let result = unsafe { hew_string_slice(core::ptr::null(), 0, 5) };
        assert_eq!(unsafe { read_and_free(result) }, "");
    }

    #[test]
    fn test_string_slice_start_past_end() {
        let s = CString::new("hello").unwrap();
        let result = unsafe { hew_string_slice(s.as_ptr(), 10, 20) };
        assert_eq!(unsafe { read_and_free(result) }, "");
    }

    #[test]
    fn test_string_find_basic() {
        let s = CString::new("hello world").unwrap();
        let sub = CString::new("world").unwrap();
        assert_eq!(unsafe { hew_string_find(s.as_ptr(), sub.as_ptr()) }, 6);
    }

    #[test]
    fn test_string_find_not_found() {
        let s = CString::new("hello").unwrap();
        let sub = CString::new("xyz").unwrap();
        assert_eq!(unsafe { hew_string_find(s.as_ptr(), sub.as_ptr()) }, -1);
    }

    #[test]
    fn test_string_find_null() {
        let sub = CString::new("test").unwrap();
        assert_eq!(
            unsafe { hew_string_find(core::ptr::null(), sub.as_ptr()) },
            -1
        );
    }

    #[test]
    fn test_string_length() {
        let s = CString::new("hello").unwrap();
        assert_eq!(unsafe { hew_string_length(s.as_ptr()) }, 5);
    }

    #[test]
    fn test_string_length_empty() {
        let s = CString::new("").unwrap();
        assert_eq!(unsafe { hew_string_length(s.as_ptr()) }, 0);
    }

    #[test]
    fn test_string_equals() {
        let a = CString::new("hello").unwrap();
        let b = CString::new("hello").unwrap();
        let c = CString::new("world").unwrap();
        assert_eq!(unsafe { hew_string_equals(a.as_ptr(), b.as_ptr()) }, 1);
        assert_eq!(unsafe { hew_string_equals(a.as_ptr(), c.as_ptr()) }, 0);
    }

    #[test]
    fn test_string_starts_with() {
        let s = CString::new("hello world").unwrap();
        let prefix = CString::new("hello").unwrap();
        let bad = CString::new("world").unwrap();
        assert!(unsafe { hew_string_starts_with(s.as_ptr(), prefix.as_ptr()) });
        assert!(!unsafe { hew_string_starts_with(s.as_ptr(), bad.as_ptr()) });
    }

    #[test]
    fn test_string_ends_with() {
        let s = CString::new("hello world").unwrap();
        let suffix = CString::new("world").unwrap();
        let bad = CString::new("hello").unwrap();
        assert!(unsafe { hew_string_ends_with(s.as_ptr(), suffix.as_ptr()) });
        assert!(!unsafe { hew_string_ends_with(s.as_ptr(), bad.as_ptr()) });
    }

    #[test]
    fn test_string_contains() {
        let s = CString::new("hello world").unwrap();
        let sub = CString::new("lo wo").unwrap();
        let bad = CString::new("xyz").unwrap();
        assert!(unsafe { hew_string_contains(s.as_ptr(), sub.as_ptr()) });
        assert!(!unsafe { hew_string_contains(s.as_ptr(), bad.as_ptr()) });
    }

    #[test]
    fn test_int_to_string() {
        let result = unsafe { hew_int_to_string(42) };
        assert_eq!(unsafe { read_and_free(result) }, "42");
    }

    #[test]
    fn test_int_to_string_negative() {
        let result = unsafe { hew_int_to_string(-7) };
        assert_eq!(unsafe { read_and_free(result) }, "-7");
    }

    #[test]
    fn test_int_to_string_zero() {
        let result = unsafe { hew_int_to_string(0) };
        assert_eq!(unsafe { read_and_free(result) }, "0");
    }

    #[test]
    fn test_bool_to_string() {
        let t = unsafe { hew_bool_to_string(true) };
        let f = unsafe { hew_bool_to_string(false) };
        assert_eq!(unsafe { read_and_free(t) }, "true");
        assert_eq!(unsafe { read_and_free(f) }, "false");
    }

    #[test]
    fn test_string_trim() {
        let s = CString::new("  hello  ").unwrap();
        let result = unsafe { hew_string_trim(s.as_ptr()) };
        assert_eq!(unsafe { read_and_free(result) }, "hello");
    }

    #[test]
    fn test_string_trim_all_whitespace() {
        let s = CString::new("   ").unwrap();
        let result = unsafe { hew_string_trim(s.as_ptr()) };
        assert_eq!(unsafe { read_and_free(result) }, "");
    }

    #[test]
    fn test_string_replace() {
        let s = CString::new("hello world").unwrap();
        let from = CString::new("world").unwrap();
        let to = CString::new("rust").unwrap();
        let result = unsafe { hew_string_replace(s.as_ptr(), from.as_ptr(), to.as_ptr()) };
        assert_eq!(unsafe { read_and_free(result) }, "hello rust");
    }

    #[test]
    fn test_string_to_lowercase() {
        let s = CString::new("HELLO World").unwrap();
        let result = unsafe { hew_string_to_lowercase(s.as_ptr()) };
        assert_eq!(unsafe { read_and_free(result) }, "hello world");
    }

    #[test]
    fn test_string_to_uppercase() {
        let s = CString::new("hello World").unwrap();
        let result = unsafe { hew_string_to_uppercase(s.as_ptr()) };
        assert_eq!(unsafe { read_and_free(result) }, "HELLO WORLD");
    }

    #[test]
    fn test_string_repeat() {
        let s = CString::new("ab").unwrap();
        let result = unsafe { hew_string_repeat(s.as_ptr(), 3) };
        assert_eq!(unsafe { read_and_free(result) }, "ababab");
    }

    #[test]
    fn test_string_repeat_zero() {
        let s = CString::new("ab").unwrap();
        let result = unsafe { hew_string_repeat(s.as_ptr(), 0) };
        assert_eq!(unsafe { read_and_free(result) }, "");
    }

    #[test]
    fn test_string_index_of() {
        let s = CString::new("abcabc").unwrap();
        let sub = CString::new("bc").unwrap();
        assert_eq!(
            unsafe { hew_string_index_of(s.as_ptr(), sub.as_ptr(), 0) },
            1
        );
    }

    #[test]
    fn test_string_char_count() {
        let s = CString::new("hello").unwrap();
        assert_eq!(unsafe { hew_string_char_count(s.as_ptr()) }, 5);
    }

    #[test]
    fn test_string_is_ascii() {
        let ascii = CString::new("hello").unwrap();
        let non_ascii = CString::new("héllo").unwrap();
        assert_eq!(unsafe { hew_string_is_ascii(ascii.as_ptr()) }, 1);
        assert_eq!(unsafe { hew_string_is_ascii(non_ascii.as_ptr()) }, 0);
    }

    #[test]
    fn test_string_reverse_utf8() {
        let s = CString::new("hello").unwrap();
        let result = unsafe { hew_string_reverse_utf8(s.as_ptr()) };
        assert_eq!(unsafe { read_and_free(result) }, "olleh");
    }

    #[test]
    fn test_string_reverse_utf8_null() {
        let result = unsafe { hew_string_reverse_utf8(core::ptr::null()) };
        assert!(result.is_null());
    }
}
