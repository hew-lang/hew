//! Hew runtime: `uuid_gen` module.
//!
//! Provides UUID v4 and v7 generation, validation, and memory management for
//! compiled Hew programs. All returned strings are allocated with `libc::malloc`
//! so callers can free them with [`hew_uuid_free`].

#[cfg(feature = "export-meta")]
pub mod export_meta;

use hew_cabi::cabi::malloc_cstring;
use std::ffi::CStr;
use std::os::raw::c_char;

use uuid::Uuid;

/// Generate a UUID v4 (random) string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string containing the
/// hyphenated UUID (36 chars + NUL). The caller must free it with
/// [`hew_uuid_free`]. Returns null on allocation failure.
///
/// # Safety
///
/// No special requirements. The returned pointer must be freed with
/// [`hew_uuid_free`].
#[cfg_attr(
    feature = "export-meta",
    hew_export_macro::hew_export(
        module = "std::misc::uuid",
        doc = "Generate a random (v4) UUID string"
    )
)]
#[no_mangle]
pub unsafe extern "C" fn hew_uuid_v4() -> *mut c_char {
    let id = Uuid::new_v4();
    let s = id.to_string();
    // SAFETY: s.as_ptr() is valid for s.len() bytes.
    unsafe { malloc_cstring(s.as_ptr(), s.len()) }
}

/// Generate a UUID v7 (time-ordered, random) string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string containing the
/// hyphenated UUID (36 chars + NUL). The caller must free it with
/// [`hew_uuid_free`]. Returns null on allocation failure.
///
/// # Safety
///
/// No special requirements. The returned pointer must be freed with
/// [`hew_uuid_free`].
#[cfg_attr(
    feature = "export-meta",
    hew_export_macro::hew_export(
        module = "std::misc::uuid",
        doc = "Generate a time-ordered (v7) UUID string"
    )
)]
#[no_mangle]
pub unsafe extern "C" fn hew_uuid_v7() -> *mut c_char {
    let id = Uuid::now_v7();
    let s = id.to_string();
    // SAFETY: s.as_ptr() is valid for s.len() bytes.
    unsafe { malloc_cstring(s.as_ptr(), s.len()) }
}

/// Validate a UUID string.
///
/// Returns `1` if the string is a valid UUID, `0` otherwise (including if the
/// pointer is null or contains invalid UTF-8).
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string, or null.
#[cfg_attr(
    feature = "export-meta",
    hew_export_macro::hew_export(
        module = "std::misc::uuid",
        name = "is_valid",
        doc = "Return 1 if the string is a valid UUID, 0 otherwise"
    )
)]
#[no_mangle]
pub unsafe extern "C" fn hew_uuid_parse(s: *const c_char) -> i32 {
    if s.is_null() {
        return 0;
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    let Ok(rust_str) = unsafe { CStr::from_ptr(s) }.to_str() else {
        return 0;
    };
    i32::from(Uuid::parse_str(rust_str).is_ok())
}

/// Free a UUID string previously returned by [`hew_uuid_v4`] or [`hew_uuid_v7`].
///
/// # Safety
///
/// `s` must be a pointer previously returned by `hew_uuid_v4` or `hew_uuid_v7`,
/// and must not have been freed already. Null is accepted (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_uuid_free(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    // SAFETY: s was allocated with libc::malloc in malloc_cstring.
    unsafe { libc::free(s.cast()) };
}

#[cfg(test)]
extern crate hew_runtime; // Link hew_vec_* symbol implementations
#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_uuid_v4_format() {
        // SAFETY: hew_uuid_v4 has no preconditions.
        let ptr = unsafe { hew_uuid_v4() };
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated string from hew_uuid_v4.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap();
        assert_eq!(s.len(), 36);
        assert!(Uuid::parse_str(s).is_ok());
        // SAFETY: ptr was allocated by hew_uuid_v4.
        unsafe { hew_uuid_free(ptr) };
    }

    #[test]
    fn test_uuid_v7_format() {
        // SAFETY: hew_uuid_v7 has no preconditions.
        let ptr = unsafe { hew_uuid_v7() };
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated string from hew_uuid_v7.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap();
        assert_eq!(s.len(), 36);
        let parsed = Uuid::parse_str(s).unwrap();
        assert_eq!(parsed.get_version(), Some(uuid::Version::SortRand));
        // SAFETY: ptr was allocated by hew_uuid_v7.
        unsafe { hew_uuid_free(ptr) };
    }

    #[test]
    fn test_uuid_parse_valid_and_invalid() {
        let valid = CString::new("550e8400-e29b-41d4-a716-446655440000").unwrap();
        // SAFETY: valid.as_ptr() is a NUL-terminated C string.
        assert_eq!(unsafe { hew_uuid_parse(valid.as_ptr()) }, 1);

        let invalid = CString::new("not-a-uuid").unwrap();
        // SAFETY: invalid.as_ptr() is a NUL-terminated C string.
        assert_eq!(unsafe { hew_uuid_parse(invalid.as_ptr()) }, 0);

        // SAFETY: null pointer is explicitly handled by hew_uuid_parse.
        assert_eq!(unsafe { hew_uuid_parse(std::ptr::null()) }, 0);
    }

    #[test]
    fn test_uuid_free_null() {
        // SAFETY: null is explicitly accepted as a no-op.
        unsafe { hew_uuid_free(std::ptr::null_mut()) };
    }
}
