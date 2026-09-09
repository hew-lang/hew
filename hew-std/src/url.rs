//! Hew runtime: URL parsing and construction.
//!
//! Provides URL parsing, component access, and URL joining for compiled Hew
//! programs. Returned strings are managed strings; release them with
//! `hew_string_drop`. Null is the canonical empty string. All returned
//! [`HewUrl`] pointers are heap-allocated via `Box` and must be freed with
//! [`hew_url_free`].
use hew_cabi::string::{string_as_str, string_from_str, HewString};

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
/// `s` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_url_parse(s: *const HewString) -> *mut HewUrl {
    // SAFETY: s borrows a live managed string or canonical empty, per caller contract.
    let rust_str = unsafe { string_as_str(s) };
    match url::Url::parse(rust_str) {
        Ok(parsed) => Box::into_raw(Box::new(HewUrl { inner: parsed })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Return true if `url` is a non-null URL handle.
#[no_mangle]
pub extern "C" fn hew_url_is_valid(url: *const HewUrl) -> bool {
    !url.is_null()
}

/// Get the scheme component (e.g. `"https"`) of a [`HewUrl`].
///
/// Returns one owned managed string. Release it with `hew_string_drop`.
/// Returns null if `url` is null.
///
/// # Safety
///
/// `url` must be a valid pointer to a [`HewUrl`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_scheme(url: *const HewUrl) -> *mut HewString {
    if url.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: url is a valid HewUrl pointer per caller contract.
    let u = unsafe { &*url };
    string_from_str(u.inner.scheme())
}

/// Get the host component of a [`HewUrl`].
///
/// Returns one owned managed string. Release it with `hew_string_drop`.
/// Returns null if the URL has no host or `url` is null.
///
/// # Safety
///
/// `url` must be a valid pointer to a [`HewUrl`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_host(url: *const HewUrl) -> *mut HewString {
    if url.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: url is a valid HewUrl pointer per caller contract.
    let u = unsafe { &*url };
    match u.inner.host_str() {
        Some(h) => string_from_str(h),
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
/// Returns one owned managed string. Release it with `hew_string_drop`.
/// Returns null if `url` is null.
///
/// # Safety
///
/// `url` must be a valid pointer to a [`HewUrl`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_path(url: *const HewUrl) -> *mut HewString {
    if url.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: url is a valid HewUrl pointer per caller contract.
    let u = unsafe { &*url };
    string_from_str(u.inner.path())
}

/// Get the query string of a [`HewUrl`].
///
/// Returns one owned managed string. Release it with `hew_string_drop`.
/// Returns null if the URL has no query or `url` is null. The `.hew` wrapper
/// (`UrlMethods::query`) returns this value directly as a `string`, so an
/// absent query and a present-but-empty query (`?` with nothing after it)
/// were already indistinguishable before this migration; the managed
/// canonical-empty convention preserves that.
///
/// # Safety
///
/// `url` must be a valid pointer to a [`HewUrl`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_query(url: *const HewUrl) -> *mut HewString {
    if url.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: url is a valid HewUrl pointer per caller contract.
    let u = unsafe { &*url };
    match u.inner.query() {
        Some(q) => string_from_str(q),
        None => std::ptr::null_mut(),
    }
}

/// Get the fragment component of a [`HewUrl`].
///
/// Returns one owned managed string. Release it with `hew_string_drop`.
/// Returns null if the URL has no fragment or `url` is null. As with
/// [`hew_url_query`], the `.hew` wrapper returns this value directly as a
/// `string`, so an absent fragment and a present-but-empty fragment (`#`
/// with nothing after it) were already indistinguishable before this
/// migration.
///
/// # Safety
///
/// `url` must be a valid pointer to a [`HewUrl`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_fragment(url: *const HewUrl) -> *mut HewString {
    if url.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: url is a valid HewUrl pointer per caller contract.
    let u = unsafe { &*url };
    match u.inner.fragment() {
        Some(f) => string_from_str(f),
        None => std::ptr::null_mut(),
    }
}

/// Serialize a [`HewUrl`] back to its full string representation.
///
/// Returns one owned managed string. Release it with `hew_string_drop`.
/// Returns null if `url` is null.
///
/// # Safety
///
/// `url` must be a valid pointer to a [`HewUrl`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_url_to_string(url: *const HewUrl) -> *mut HewString {
    if url.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: url is a valid HewUrl pointer per caller contract.
    let u = unsafe { &*url };
    string_from_str(u.inner.as_str())
}

/// Join a relative URL against a base [`HewUrl`].
///
/// Returns a new heap-allocated [`HewUrl`], or null on error.
///
/// # Safety
///
/// `base` must be a valid pointer to a [`HewUrl`], or null.
/// `relative` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_url_join(
    base: *const HewUrl,
    relative: *const HewString,
) -> *mut HewUrl {
    if base.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: base is a valid HewUrl pointer per caller contract.
    let b = unsafe { &*base };
    // SAFETY: relative borrows a live managed string or canonical empty.
    let rel_str = unsafe { string_as_str(relative) };
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
    use crate::test_string::ManagedString;
    use hew_cabi::string::string_release;

    /// Helper: parse a URL string and return the owned pointer.
    fn parse(s: &str) -> *mut HewUrl {
        let managed = ManagedString::new(s);
        // SAFETY: managed owns a live managed string for this call.
        unsafe { hew_url_parse(managed.as_ptr()) }
    }

    /// Helper: read an owned managed string result and release it.
    unsafe fn read_and_release(ptr: *mut HewString) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is the live owner returned by the producer.
        let s = unsafe { string_as_str(ptr) }.to_owned();
        // SAFETY: this test holds the only owner of `ptr`.
        unsafe { string_release(ptr) };
        s
    }

    #[test]
    fn parse_full_url_components() {
        let url = parse("https://example.com:8080/path/to?key=val#frag");
        assert!(!url.is_null());

        // SAFETY: url is a valid HewUrl from parse.
        unsafe {
            assert_eq!(read_and_release(hew_url_scheme(url)), "https");
            assert_eq!(read_and_release(hew_url_host(url)), "example.com");
            assert_eq!(hew_url_port(url), 8080);
            assert_eq!(read_and_release(hew_url_path(url)), "/path/to");
            assert_eq!(read_and_release(hew_url_query(url)), "key=val");
            assert_eq!(read_and_release(hew_url_fragment(url)), "frag");
            assert_eq!(
                read_and_release(hew_url_to_string(url)),
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
            assert_eq!(read_and_release(hew_url_scheme(url)), "http");
            assert_eq!(read_and_release(hew_url_host(url)), "localhost");
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

        let relative = ManagedString::new("sub/page?q=1");
        // SAFETY: base is a valid HewUrl; relative is a live managed string.
        let joined = unsafe { hew_url_join(base, relative.as_ptr()) };
        assert!(!joined.is_null());

        // SAFETY: joined is a valid HewUrl.
        unsafe {
            assert_eq!(
                read_and_release(hew_url_to_string(joined)),
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
        assert!(!hew_url_is_valid(std::ptr::null()));

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
            assert_eq!(read_and_release(hew_url_scheme(url)), "data");
            // data URIs have no host.
            assert!(hew_url_host(url).is_null());
            hew_url_free(url);
        }
    }
}
