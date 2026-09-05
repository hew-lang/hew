//! Managed UTF-8 string carrier shared by compiled Hew code and its runtime.
//!
//! A Hew string value is one opaque pointer to an immutable allocation. The
//! pointer names the allocation header, never a C character buffer. The exact
//! byte length in the header makes every Unicode operation length-bounded and
//! permits embedded NUL bytes. Foreign C strings enter and leave only through
//! explicit copying adapters.

use core::slice;
use core::str::{self, Utf8Error};
use std::ffi::CStr;
use std::os::raw::c_char;
use std::sync::atomic::{fence, AtomicU32, Ordering};

/// Opaque pointee for the managed Hew string handle.
///
/// Its allocation layout is private. Runtime and package code must use the
/// helpers in this module instead of dereferencing or casting the handle.
#[repr(C)]
#[derive(Debug)]
pub struct HewString {
    _private: [u8; 0],
}

#[repr(C)]
struct HewStringHeader {
    byte_len: usize,
    rc: AtomicU32,
}

const MAX_REFCOUNT: u32 = u32::MAX / 2;

/// Why a foreign C string could not be copied into managed string storage.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringFromCStrError {
    /// A null C pointer does not spell an input string.
    NullInput,
    /// The bytes preceding the C terminator are not valid UTF-8.
    InvalidUtf8,
}

/// Why a managed string could not be copied to a C string.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringToCStrError {
    /// A C string cannot represent an embedded NUL byte.
    InteriorNul,
}

#[inline]
unsafe fn header(value: *const HewString) -> *const HewStringHeader {
    value.cast()
}

#[inline]
unsafe fn data(value: *const HewString) -> *const u8 {
    // SAFETY: callers establish that `value` names a live managed allocation.
    unsafe {
        value
            .cast::<u8>()
            .add(core::mem::size_of::<HewStringHeader>())
    }
}

/// Allocate one managed string from bytes already known to be valid UTF-8.
///
/// The empty string has the canonical null representation. Allocation failure
/// aborts, matching the compiler-private runtime's infallible value producers.
///
/// # Safety
///
/// When `len > 0`, `source` must be readable for `len` bytes and those bytes
/// must be valid UTF-8.
#[must_use]
pub unsafe fn string_alloc_utf8_unchecked(source: *const u8, len: usize) -> *mut HewString {
    if len == 0 {
        return core::ptr::null_mut();
    }
    if source.is_null() {
        std::process::abort();
    }
    let Some(total) = core::mem::size_of::<HewStringHeader>().checked_add(len) else {
        std::process::abort();
    };
    if isize::try_from(total).is_err() {
        std::process::abort();
    }

    // SAFETY: `total` is non-zero and fits pointer-offset arithmetic.
    let allocation = unsafe { libc::malloc(total) }.cast::<HewStringHeader>();
    if allocation.is_null() {
        std::process::abort();
    }
    // SAFETY: malloc storage is suitably aligned and writable for the header.
    unsafe {
        allocation.write(HewStringHeader {
            byte_len: len,
            rc: AtomicU32::new(1),
        });
        core::ptr::copy_nonoverlapping(
            source,
            allocation
                .cast::<u8>()
                .add(core::mem::size_of::<HewStringHeader>()),
            len,
        );
    }
    allocation.cast()
}

/// Allocate one managed string by copying a Rust string slice.
#[must_use]
pub fn string_from_str(value: &str) -> *mut HewString {
    // SAFETY: a Rust `str` is valid UTF-8 and readable for its byte length.
    unsafe { string_alloc_utf8_unchecked(value.as_ptr(), value.len()) }
}

/// Validate and copy an arbitrary byte slice into one managed string.
///
/// # Errors
///
/// Returns the UTF-8 validation error without allocating a managed value.
pub fn string_from_utf8(value: &[u8]) -> Result<*mut HewString, Utf8Error> {
    let value = str::from_utf8(value)?;
    Ok(string_from_str(value))
}

/// Validate and copy one explicitly foreign, NUL-terminated C string.
///
/// The returned handle is always managed; the foreign pointer is never stored
/// or reinterpreted as a Hew string.
///
/// # Errors
///
/// Returns [`StringFromCStrError::NullInput`] for null or
/// [`StringFromCStrError::InvalidUtf8`] when the foreign bytes are not UTF-8.
///
/// # Safety
///
/// `value` must be null or point to a readable NUL-terminated C string.
pub unsafe fn string_from_cstr_copy(
    value: *const c_char,
) -> Result<*mut HewString, StringFromCStrError> {
    if value.is_null() {
        return Err(StringFromCStrError::NullInput);
    }
    // SAFETY: the caller supplies a readable NUL-terminated C string.
    let value = unsafe { CStr::from_ptr(value) }
        .to_str()
        .map_err(|_| StringFromCStrError::InvalidUtf8)?;
    Ok(string_from_str(value))
}

/// Copy a managed value into an owned foreign `CString`.
///
/// Rust owns this adapter's allocation; drop the `CString` normally. Use
/// [`cstring_from_string_copy`] and [`cstring_copy_release`] for raw C ownership.
///
/// # Errors
///
/// Returns [`StringToCStrError::InteriorNul`] instead of truncating input.
///
/// # Safety
///
/// `value` must be null (empty) or a live managed string handle.
pub unsafe fn string_to_cstring(
    value: *const HewString,
) -> Result<std::ffi::CString, StringToCStrError> {
    // SAFETY: the caller holds a live managed owner during the copy.
    std::ffi::CString::new(unsafe { string_as_bytes(value) })
        .map_err(|_| StringToCStrError::InteriorNul)
}

/// Copy a managed string into a freshly allocated NUL-terminated C string.
///
/// The allocation belongs exclusively to [`cstring_copy_release`]. It is not
/// a managed Hew string and must never be passed to [`string_release`].
///
/// # Errors
///
/// Returns [`StringToCStrError::InteriorNul`] when the managed value cannot be
/// represented by a NUL-terminated C string without truncation.
///
/// # Safety
///
/// `value` must be null (the canonical empty string) or a live managed handle.
pub unsafe fn cstring_from_string_copy(
    value: *const HewString,
) -> Result<*mut c_char, StringToCStrError> {
    // SAFETY: the caller provides a live managed handle.
    let bytes = unsafe { string_as_bytes(value) };
    if bytes.contains(&0) {
        return Err(StringToCStrError::InteriorNul);
    }
    let Some(size) = bytes.len().checked_add(1) else {
        std::process::abort();
    };
    // SAFETY: `size` is positive and was checked for arithmetic overflow.
    let out = unsafe { libc::malloc(size) }.cast::<c_char>();
    if out.is_null() {
        std::process::abort();
    }
    // SAFETY: `out` owns `size` writable bytes and the source is disjoint.
    unsafe {
        core::ptr::copy_nonoverlapping(bytes.as_ptr(), out.cast::<u8>(), bytes.len());
        out.add(bytes.len()).write(0);
    }
    Ok(out)
}

/// Release a C-string copy returned by [`cstring_from_string_copy`].
///
/// # Safety
///
/// `value` must be null or an outstanding result from
/// [`cstring_from_string_copy`].
pub unsafe fn cstring_copy_release(value: *mut c_char) {
    // SAFETY: this is the documented allocator pair for the copied C string.
    unsafe { libc::free(value.cast()) };
}

/// Borrow the complete UTF-8 byte contents of a managed string.
///
/// Null is the canonical empty value and returns an empty slice.
///
/// # Safety
///
/// `value` must be null or a live managed string handle, and the returned
/// borrow must not outlive an owning reference to that handle.
#[must_use]
pub unsafe fn string_as_bytes<'a>(value: *const HewString) -> &'a [u8] {
    if value.is_null() {
        return &[];
    }
    // SAFETY: the handle contract gives a live initialized header.
    let len = unsafe { (*header(value)).byte_len };
    // SAFETY: the allocation carries exactly `len` initialized payload bytes.
    unsafe { slice::from_raw_parts(data(value), len) }
}

/// Borrow the complete text contents of a managed string.
///
/// # Safety
///
/// The requirements of [`string_as_bytes`] apply. Every managed allocation is
/// constructed from valid UTF-8, so no validation is repeated here.
#[must_use]
pub unsafe fn string_as_str<'a>(value: *const HewString) -> &'a str {
    // SAFETY: managed-string construction establishes the UTF-8 invariant.
    unsafe { str::from_utf8_unchecked(string_as_bytes(value)) }
}

/// Retain one additional owner of a managed string and return the same handle.
///
/// Null is the canonical empty value and remains null.
///
/// # Safety
///
/// `value` must be null or a live managed string handle.
#[must_use]
pub unsafe fn string_retain(value: *const HewString) -> *mut HewString {
    if value.is_null() {
        return core::ptr::null_mut();
    }
    // SAFETY: the caller supplies a live managed allocation.
    let rc = unsafe { &(*header(value)).rc };
    let old = rc.fetch_add(1, Ordering::Relaxed);
    if old > MAX_REFCOUNT {
        std::process::abort();
    }
    value.cast_mut()
}

/// Release one owner of a managed string.
///
/// Null is the canonical empty value and is a no-op.
///
/// # Safety
///
/// `value` must be null or a live managed string owner that has not already
/// been released or transferred.
pub unsafe fn string_release(value: *mut HewString) {
    if value.is_null() {
        return;
    }
    // SAFETY: the caller supplies a live managed allocation.
    let rc = unsafe { &(*header(value)).rc };
    match rc.fetch_sub(1, Ordering::Release) {
        1 => {
            fence(Ordering::Acquire);
            // SAFETY: this was the final owner and the handle is the allocation base.
            unsafe { libc::free(value.cast()) };
        }
        0 => std::process::abort(),
        _ => {}
    }
}
