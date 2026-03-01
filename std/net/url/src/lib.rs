//! Hew runtime: URL parsing and construction.
//!
//! Provides URL parsing, component access, and URL joining for compiled Hew
//! programs. All returned strings are allocated with `libc::malloc` and
//! NUL-terminated. All returned [`HewUrl`] pointers are heap-allocated via
//! `Box` and must be freed with [`hew_url_free`].

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use std::ffi::c_char;

/// Opaque wrapper around a [`url::Url`].
///
/// Returned by [`hew_url_parse`] and [`hew_url_join`].
/// Must be freed with [`hew_url_free`].
#[derive(Debug)]
pub struct HewUrl {
    inner: url::Url,
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Parse a URL string into a [`HewUrl`].
///
/// Returns null on parse error or invalid input.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_parse(s: *const c_char) -> *mut HewUrl {
    // SAFETY: If non-null, s is a valid NUL-terminated C string per caller contract.
    let Some(rust_str) = (unsafe { cstr_to_str(s) }) else {
        return std::ptr::null_mut();
    };
    match url::Url::parse(rust_str) {
        Ok(parsed) => Box::into_raw(Box::new(HewUrl { inner: parsed })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Get the scheme component (e.g. `"https"`) of a [`HewUrl`].
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. Returns null if `url` is null.
///
/// # Safety
///
/// `url` must be a valid pointer to a [`HewUrl`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_scheme(url: *const HewUrl) -> *mut c_char {
    if url.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: url is a valid HewUrl pointer per caller contract.
    let u = unsafe { &*url };
    str_to_malloc(u.inner.scheme())
}

/// Get the host component of a [`HewUrl`].
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. Returns null if the URL has no host or `url` is null.
///
/// # Safety
///
/// `url` must be a valid pointer to a [`HewUrl`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_host(url: *const HewUrl) -> *mut c_char {
    if url.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: url is a valid HewUrl pointer per caller contract.
    let u = unsafe { &*url };
    match u.inner.host_str() {
        Some(h) => str_to_malloc(h),
        None => std::ptr::null_mut(),
    }
}

/// Get the port number of a [`HewUrl`].
///
/// Returns the port as an `i32`, or `-1` if no port is specified.
///
/// # Safety
///
/// `url` must be a valid pointer to a [`HewUrl`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_port(url: *const HewUrl) -> i32 {
    if url.is_null() {
        return -1;
    }
    // SAFETY: url is a valid HewUrl pointer per caller contract.
    let u = unsafe { &*url };
    match u.inner.port() {
        Some(p) => i32::from(p),
        None => -1,
    }
}

/// Get the path component of a [`HewUrl`].
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. Returns null if `url` is null.
///
/// # Safety
///
/// `url` must be a valid pointer to a [`HewUrl`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_path(url: *const HewUrl) -> *mut c_char {
    if url.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: url is a valid HewUrl pointer per caller contract.
    let u = unsafe { &*url };
    str_to_malloc(u.inner.path())
}

/// Get the query string of a [`HewUrl`].
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. Returns null if the URL has no query or `url` is null.
///
/// # Safety
///
/// `url` must be a valid pointer to a [`HewUrl`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_query(url: *const HewUrl) -> *mut c_char {
    if url.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: url is a valid HewUrl pointer per caller contract.
    let u = unsafe { &*url };
    match u.inner.query() {
        Some(q) => str_to_malloc(q),
        None => std::ptr::null_mut(),
    }
}

/// Get the fragment component of a [`HewUrl`].
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. Returns null if the URL has no fragment or `url` is
/// null.
///
/// # Safety
///
/// `url` must be a valid pointer to a [`HewUrl`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_fragment(url: *const HewUrl) -> *mut c_char {
    if url.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: url is a valid HewUrl pointer per caller contract.
    let u = unsafe { &*url };
    match u.inner.fragment() {
        Some(f) => str_to_malloc(f),
        None => std::ptr::null_mut(),
    }
}

/// Serialize a [`HewUrl`] back to its full string representation.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. Returns null if `url` is null.
///
/// # Safety
///
/// `url` must be a valid pointer to a [`HewUrl`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_to_string(url: *const HewUrl) -> *mut c_char {
    if url.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: url is a valid HewUrl pointer per caller contract.
    let u = unsafe { &*url };
    str_to_malloc(u.inner.as_str())
}

/// Join a relative URL against a base [`HewUrl`].
///
/// Returns a new heap-allocated [`HewUrl`], or null on error.
///
/// # Safety
///
/// `base` must be a valid pointer to a [`HewUrl`], or null.
/// `relative` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_join(base: *const HewUrl, relative: *const c_char) -> *mut HewUrl {
    if base.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: base is a valid HewUrl pointer per caller contract.
    let b = unsafe { &*base };
    // SAFETY: If non-null, relative is a valid NUL-terminated C string per caller contract.
    let Some(rel_str) = (unsafe { cstr_to_str(relative) }) else {
        return std::ptr::null_mut();
    };
    match b.inner.join(rel_str) {
        Ok(joined) => Box::into_raw(Box::new(HewUrl { inner: joined })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Free a [`HewUrl`] previously returned by [`hew_url_parse`] or
/// [`hew_url_join`].
///
/// # Safety
///
/// `url` must be a pointer previously returned by a `hew_url_*` function,
/// and must not have been freed already. Null is accepted (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_url_free(url: *mut HewUrl) {
    if url.is_null() {
        return;
    }
    // SAFETY: url was allocated with Box::into_raw and has not been freed.
    drop(unsafe { Box::from_raw(url) });
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    /// Helper: parse a URL string and return the owned pointer.
    fn parse(s: &str) -> *mut HewUrl {
        let c = CString::new(s).unwrap();
        // SAFETY: c is a valid NUL-terminated C string.
        unsafe { hew_url_parse(c.as_ptr()) }
    }

    /// Helper: read a C string pointer and free it.
    unsafe fn read_and_free_cstr(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from malloc.
        let s = unsafe { cstr_to_str(ptr) }.unwrap().to_owned();
        // SAFETY: ptr was allocated with malloc.
        unsafe { libc::free(ptr.cast()) };
        s
    }

    #[test]
    fn parse_full_url_components() {
        let url = parse("https://example.com:8080/path/to?key=val#frag");
        assert!(!url.is_null());

        // SAFETY: url is a valid HewUrl from parse.
        unsafe {
            assert_eq!(read_and_free_cstr(hew_url_scheme(url)), "https");
            assert_eq!(read_and_free_cstr(hew_url_host(url)), "example.com");
            assert_eq!(hew_url_port(url), 8080);
            assert_eq!(read_and_free_cstr(hew_url_path(url)), "/path/to");
            assert_eq!(read_and_free_cstr(hew_url_query(url)), "key=val");
            assert_eq!(read_and_free_cstr(hew_url_fragment(url)), "frag");
            assert_eq!(
                read_and_free_cstr(hew_url_to_string(url)),
                "https://example.com:8080/path/to?key=val#frag"
            );
            hew_url_free(url);
        }
    }

    #[test]
    fn parse_minimal_url() {
        let url = parse("http://localhost");
        assert!(!url.is_null());

        // SAFETY: url is a valid HewUrl from parse.
        unsafe {
            assert_eq!(read_and_free_cstr(hew_url_scheme(url)), "http");
            assert_eq!(read_and_free_cstr(hew_url_host(url)), "localhost");
            assert_eq!(hew_url_port(url), -1);
            assert!(hew_url_query(url).is_null());
            assert!(hew_url_fragment(url).is_null());
            hew_url_free(url);
        }
    }

    #[test]
    fn join_relative_url() {
        let base = parse("https://example.com/base/");
        assert!(!base.is_null());

        let relative = CString::new("sub/page?q=1").unwrap();
        // SAFETY: base is a valid HewUrl; relative is a valid C string.
        let joined = unsafe { hew_url_join(base, relative.as_ptr()) };
        assert!(!joined.is_null());

        // SAFETY: joined is a valid HewUrl.
        unsafe {
            assert_eq!(
                read_and_free_cstr(hew_url_to_string(joined)),
                "https://example.com/base/sub/page?q=1"
            );
            hew_url_free(joined);
            hew_url_free(base);
        }
    }

    #[test]
    fn null_and_invalid_handling() {
        // Null input returns null.
        // SAFETY: Null pointer is safe for hew_url_parse.
        unsafe {
            assert!(hew_url_parse(std::ptr::null()).is_null());
        }

        // Invalid URL returns null.
        let bad = parse("not a url");
        assert!(bad.is_null());

        // Null URL pointer returns null/defaults.
        // SAFETY: Null pointer is safe for all accessor functions.
        unsafe {
            assert!(hew_url_scheme(std::ptr::null()).is_null());
            assert!(hew_url_host(std::ptr::null()).is_null());
            assert_eq!(hew_url_port(std::ptr::null()), -1);
            assert!(hew_url_path(std::ptr::null()).is_null());
            assert!(hew_url_query(std::ptr::null()).is_null());
            assert!(hew_url_fragment(std::ptr::null()).is_null());
            assert!(hew_url_to_string(std::ptr::null()).is_null());
            assert!(hew_url_join(std::ptr::null(), std::ptr::null()).is_null());
            // Free null is a no-op.
            hew_url_free(std::ptr::null_mut());
        }
    }

    #[test]
    fn data_scheme_url() {
        let url = parse("data:text/plain;base64,SGVsbG8=");
        assert!(!url.is_null());

        // SAFETY: url is a valid HewUrl from parse.
        unsafe {
            assert_eq!(read_and_free_cstr(hew_url_scheme(url)), "data");
            // data URIs have no host.
            assert!(hew_url_host(url).is_null());
            hew_url_free(url);
        }
    }
}
