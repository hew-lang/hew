//! Hew runtime: semantic versioning parsing and comparison.
//!
//! Provides semver parsing, component access, comparison, and requirement
//! matching for compiled Hew programs. All returned strings are allocated with
//! `libc::malloc` and NUL-terminated. All returned [`HewSemver`] pointers are
//! heap-allocated via `Box` and must be freed with [`hew_semver_free`].

use hew_cabi::cabi::str_to_malloc;
use std::ffi::CStr;
use std::os::raw::c_char;

/// Opaque wrapper around a [`semver::Version`].
///
/// Returned by [`hew_semver_parse`].
/// Must be freed with [`hew_semver_free`].
#[derive(Debug)]
pub struct HewSemver {
    inner: semver::Version,
}

/// Opaque wrapper around a [`semver::VersionReq`].
///
/// Not yet exposed via C ABI; reserved for future requirement-object APIs.
#[derive(Debug)]
#[expect(
    dead_code,
    reason = "public API type reserved for future C ABI functions"
)]
pub struct HewSemverReq {
    inner: semver::VersionReq,
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Parse a semver string (e.g. `"1.2.3-beta.1"`) into a [`HewSemver`].
///
/// Returns null on parse error or invalid input.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_semver_parse(s: *const c_char) -> *mut HewSemver {
    if s.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    let Ok(rust_str) = unsafe { CStr::from_ptr(s) }.to_str() else {
        return std::ptr::null_mut();
    };
    match semver::Version::parse(rust_str) {
        Ok(ver) => Box::into_raw(Box::new(HewSemver { inner: ver })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Get the major version number.
///
/// Returns 0 if `v` is null.
///
/// # Safety
///
/// `v` must be a valid pointer to a [`HewSemver`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_semver_major(v: *const HewSemver) -> u64 {
    if v.is_null() {
        return 0;
    }
    // SAFETY: v is a valid HewSemver pointer per caller contract.
    unsafe { &*v }.inner.major
}

/// Get the minor version number.
///
/// Returns 0 if `v` is null.
///
/// # Safety
///
/// `v` must be a valid pointer to a [`HewSemver`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_semver_minor(v: *const HewSemver) -> u64 {
    if v.is_null() {
        return 0;
    }
    // SAFETY: v is a valid HewSemver pointer per caller contract.
    unsafe { &*v }.inner.minor
}

/// Get the patch version number.
///
/// Returns 0 if `v` is null.
///
/// # Safety
///
/// `v` must be a valid pointer to a [`HewSemver`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_semver_patch(v: *const HewSemver) -> u64 {
    if v.is_null() {
        return 0;
    }
    // SAFETY: v is a valid HewSemver pointer per caller contract.
    unsafe { &*v }.inner.patch
}

/// Get the pre-release string of a [`HewSemver`].
///
/// Returns a `malloc`-allocated, NUL-terminated C string. Returns an empty
/// string if there is no pre-release component. Returns null if `v` is null.
/// The caller must free the result with `libc::free`.
///
/// # Safety
///
/// `v` must be a valid pointer to a [`HewSemver`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_semver_pre(v: *const HewSemver) -> *mut c_char {
    if v.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: v is a valid HewSemver pointer per caller contract.
    let ver = unsafe { &*v };
    let pre_str = ver.inner.pre.as_str();
    str_to_malloc(pre_str)
}

/// Compare two [`HewSemver`] values.
///
/// Returns `-1` if `a < b`, `0` if `a == b`, `1` if `a > b`.
/// Returns `0` if either pointer is null.
///
/// # Safety
///
/// Both `a` and `b` must be valid pointers to [`HewSemver`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_semver_compare(a: *const HewSemver, b: *const HewSemver) -> i32 {
    if a.is_null() || b.is_null() {
        return 0;
    }
    // SAFETY: both pointers are valid HewSemver per caller contract.
    let va = unsafe { &*a };
    // SAFETY: b is a valid HewSemver pointer per caller contract.
    let vb = unsafe { &*b };
    match va.inner.cmp(&vb.inner) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}

/// Check whether a version string matches a requirement string.
///
/// For example, `version_str = "1.5.0"` and `req_str = ">=1.0.0, <2.0.0"`.
///
/// Returns `1` if the version matches, `0` if it does not, `-1` on parse error.
///
/// # Safety
///
/// Both `version_str` and `req_str` must be valid NUL-terminated C strings, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_semver_matches(
    version_str: *const c_char,
    req_str: *const c_char,
) -> i32 {
    if version_str.is_null() || req_str.is_null() {
        return -1;
    }
    // SAFETY: version_str is a valid NUL-terminated C string per caller contract.
    let Ok(ver_s) = unsafe { CStr::from_ptr(version_str) }.to_str() else {
        return -1;
    };
    // SAFETY: req_str is a valid NUL-terminated C string per caller contract.
    let Ok(req_s) = unsafe { CStr::from_ptr(req_str) }.to_str() else {
        return -1;
    };
    let Ok(version) = semver::Version::parse(ver_s) else {
        return -1;
    };
    let Ok(req) = semver::VersionReq::parse(req_s) else {
        return -1;
    };
    i32::from(req.matches(&version))
}

/// Serialize a [`HewSemver`] to its string representation.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. Returns null if `v` is null.
///
/// # Safety
///
/// `v` must be a valid pointer to a [`HewSemver`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_semver_to_string(v: *const HewSemver) -> *mut c_char {
    if v.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: v is a valid HewSemver pointer per caller contract.
    let ver = unsafe { &*v };
    str_to_malloc(&ver.inner.to_string())
}

/// Free a [`HewSemver`] previously returned by [`hew_semver_parse`].
///
/// # Safety
///
/// `v` must be a pointer previously returned by [`hew_semver_parse`],
/// and must not have been freed already. Null is accepted (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_semver_free(v: *mut HewSemver) {
    if v.is_null() {
        return;
    }
    // SAFETY: v was allocated with Box::into_raw and has not been freed.
    drop(unsafe { Box::from_raw(v) });
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    /// Helper: parse a semver string and return the owned pointer.
    fn parse(s: &str) -> *mut HewSemver {
        let c = CString::new(s).unwrap();
        // SAFETY: c is a valid NUL-terminated C string.
        unsafe { hew_semver_parse(c.as_ptr()) }
    }

    /// Helper: read a C string pointer and free it.
    ///
    /// # Safety
    ///
    /// `ptr` must be a non-null, NUL-terminated, malloc-allocated C string.
    unsafe fn read_and_free_cstr(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from malloc.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with malloc.
        unsafe { libc::free(ptr.cast()) };
        s
    }

    #[test]
    fn parse_basic_version() {
        let v = parse("1.2.3");
        assert!(!v.is_null());

        // SAFETY: v is a valid HewSemver from parse.
        unsafe {
            assert_eq!(hew_semver_major(v), 1);
            assert_eq!(hew_semver_minor(v), 2);
            assert_eq!(hew_semver_patch(v), 3);
            assert_eq!(read_and_free_cstr(hew_semver_pre(v)), "");
            assert_eq!(read_and_free_cstr(hew_semver_to_string(v)), "1.2.3");
            hew_semver_free(v);
        }
    }

    #[test]
    fn parse_prerelease_version() {
        let v = parse("2.0.0-beta.1");
        assert!(!v.is_null());

        // SAFETY: v is a valid HewSemver from parse.
        unsafe {
            assert_eq!(hew_semver_major(v), 2);
            assert_eq!(hew_semver_minor(v), 0);
            assert_eq!(hew_semver_patch(v), 0);
            assert_eq!(read_and_free_cstr(hew_semver_pre(v)), "beta.1");
            hew_semver_free(v);
        }
    }

    #[test]
    fn compare_versions() {
        let a = parse("1.0.0");
        let b = parse("2.0.0");
        let c = parse("1.0.0");
        assert!(!a.is_null());
        assert!(!b.is_null());
        assert!(!c.is_null());

        // SAFETY: all pointers are valid HewSemver values.
        unsafe {
            assert_eq!(hew_semver_compare(a, b), -1);
            assert_eq!(hew_semver_compare(b, a), 1);
            assert_eq!(hew_semver_compare(a, c), 0);
            hew_semver_free(a);
            hew_semver_free(b);
            hew_semver_free(c);
        }
    }

    #[test]
    fn matches_requirement() {
        let ver = CString::new("1.5.0").unwrap();
        let req_match = CString::new(">=1.0.0, <2.0.0").unwrap();
        let req_no_match = CString::new(">=2.0.0").unwrap();

        // SAFETY: all pointers are valid NUL-terminated C strings.
        unsafe {
            assert_eq!(hew_semver_matches(ver.as_ptr(), req_match.as_ptr()), 1);
            assert_eq!(hew_semver_matches(ver.as_ptr(), req_no_match.as_ptr()), 0);
        }
    }

    #[test]
    fn matches_parse_error() {
        let bad_ver = CString::new("not-a-version").unwrap();
        let req = CString::new(">=1.0.0").unwrap();

        // SAFETY: all pointers are valid NUL-terminated C strings.
        unsafe {
            assert_eq!(hew_semver_matches(bad_ver.as_ptr(), req.as_ptr()), -1);
            assert_eq!(hew_semver_matches(std::ptr::null(), req.as_ptr()), -1);
        }
    }

    #[test]
    fn null_handling() {
        // SAFETY: null pointers are explicitly handled by all functions.
        unsafe {
            assert!(hew_semver_parse(std::ptr::null()).is_null());
            assert_eq!(hew_semver_major(std::ptr::null()), 0);
            assert_eq!(hew_semver_minor(std::ptr::null()), 0);
            assert_eq!(hew_semver_patch(std::ptr::null()), 0);
            assert!(hew_semver_pre(std::ptr::null()).is_null());
            assert_eq!(hew_semver_compare(std::ptr::null(), std::ptr::null()), 0);
            assert!(hew_semver_to_string(std::ptr::null()).is_null());
            hew_semver_free(std::ptr::null_mut());
        }
    }

    #[test]
    fn invalid_string_returns_null() {
        let bad = parse("not.valid");
        assert!(bad.is_null());

        let also_bad = parse("");
        assert!(also_bad.is_null());
    }
}
