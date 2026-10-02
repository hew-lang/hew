//! Hew `std::net::dns` — DNS hostname resolution.
//!
//! Resolves hostnames to IP address strings using the system resolver
//! (`std::net::ToSocketAddrs`). Hostnames borrow managed UTF-8 strings; results
//! transfer managed owners. Null is the canonical empty string.
//!
//! These calls block in `getaddrinfo`. Their `std.net.dns` declarations are
//! `#[offload]`, so a Hew caller parks while the pool runs them, and a
//! deadline is a `scope within` around the call.
use hew_cabi::string::{string_as_str, string_from_str, string_release, HewString};
use hew_cabi::vec::HewVec;
use std::net::{IpAddr, ToSocketAddrs};

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Run `getaddrinfo` for `host:0`. An empty host, an embedded NUL or a
/// resolver error yields no addresses.
///
/// # Safety
///
/// `hostname` must be a live managed string handle (null means empty).
unsafe fn resolve(hostname: *const HewString) -> Vec<IpAddr> {
    // SAFETY: hostname borrows a live managed string or canonical empty.
    let host = unsafe { string_as_str(hostname) };
    if host.is_empty() || host.contains('\0') {
        return Vec::new();
    }
    format!("{host}:0")
        .to_socket_addrs()
        .map(|addrs| addrs.map(|address| address.ip()).collect())
        .unwrap_or_default()
}

/// Resolve a hostname to all associated IP addresses.
///
/// Returns an owned `Vec<string>`, one managed string per resolved address.
/// Returns an empty vector on failure, empty input or embedded NUL.
///
/// # Safety
///
/// `hostname` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_dns_resolve(hostname: *const HewString) -> *mut HewVec {
    // SAFETY: hew_vec_new_str allocates a valid string-typed HewVec.
    let vec = unsafe { hew_cabi::vec::hew_vec_new_str() };
    // SAFETY: forwarded borrowed hostname.
    for addr in unsafe { resolve(hostname) } {
        let ip = string_from_str(&addr.to_string());
        // SAFETY: vec is a live Vec<string>; push retains the borrowed managed
        // address. Release the producer owner after the vector acquires its own.
        unsafe {
            hew_cabi::vec::hew_vec_push_str(vec, ip);
            string_release(ip);
        }
    }
    vec
}

/// Resolve a hostname to its first IP address.
///
/// Returns an owned managed string with the first resolved address; release it
/// with `string_release`. Failure, empty input or embedded NUL returns null.
///
/// # Safety
///
/// `hostname` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_dns_lookup_host(hostname: *const HewString) -> *mut HewString {
    // SAFETY: forwarded borrowed hostname.
    unsafe { resolve(hostname) }
        .first()
        .map_or(std::ptr::null_mut(), |addr| {
            string_from_str(&addr.to_string())
        })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_string::ManagedString;

    /// Helper: copy and release an owned managed string.
    unsafe fn read_and_free(ptr: *mut HewString) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is non-null (asserted above) and points to a live managed string handle.
        let s = unsafe { string_as_str(ptr) }.to_owned();
        // SAFETY: ptr was returned as an owned managed string by the FFI layer.
        unsafe { string_release(ptr) };
        s
    }

    #[test]
    fn resolve_localhost() {
        let host = ManagedString::new("localhost");
        // SAFETY: host is a live managed string handle.
        let vec = unsafe { hew_dns_resolve(host.as_ptr()) };
        assert!(!vec.is_null());

        // SAFETY: vec is a valid HewVec returned by hew_dns_resolve.
        let len = unsafe { hew_cabi::vec::hew_vec_len(vec) };
        // localhost should resolve to at least one address (127.0.0.1 or ::1).
        assert!(len > 0, "expected at least one address for localhost");

        // SAFETY: vec is valid and index 0 is within bounds (len > 0).
        let first = unsafe { hew_cabi::vec::hew_vec_get_str(vec, 0) };
        assert!(!first.is_null());
        // SAFETY: get retained the managed result, which survives the vector's release.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
        // SAFETY: first still owns the reference returned by the getter.
        let first_str = unsafe { read_and_free(first.cast_mut()) };
        assert!(
            first_str == "127.0.0.1" || first_str == "::1",
            "expected 127.0.0.1 or ::1, got {first_str}"
        );
    }

    #[test]
    fn managed_nul_hostname_does_not_resolve_a_valid_prefix() {
        for text in ["127.0.0.1\0suffix", "localhost\0suffix", "\0"] {
            let host = ManagedString::new(text);
            // SAFETY: each input is valid managed UTF-8; embedded NUL is an invalid hostname.
            unsafe {
                let result = hew_dns_resolve(host.as_ptr());
                assert!(!result.is_null());
                assert_eq!(hew_cabi::vec::hew_vec_len(result), 0);
                hew_cabi::vec::hew_vec_free(result);
                assert!(hew_dns_lookup_host(host.as_ptr()).is_null());
                assert_eq!(string_as_str(host.as_ptr()), text);
            }
        }
    }

    #[test]
    fn lookup_host_localhost() {
        let host = ManagedString::new("localhost");
        // SAFETY: host is a live managed string handle.
        let result = unsafe { hew_dns_lookup_host(host.as_ptr()) };
        assert!(!result.is_null());

        // SAFETY: result is non-null and was returned by hew_dns_lookup_host.
        let ip = unsafe { read_and_free(result) };
        assert!(
            ip == "127.0.0.1" || ip == "::1",
            "expected 127.0.0.1 or ::1, got {ip}"
        );
    }

    #[test]
    fn resolve_null_returns_empty_vec() {
        // SAFETY: Null pointer is explicitly handled by hew_dns_resolve.
        let vec = unsafe { hew_dns_resolve(std::ptr::null()) };
        assert!(!vec.is_null());
        // SAFETY: vec is a valid HewVec returned by hew_dns_resolve.
        assert_eq!(unsafe { hew_cabi::vec::hew_vec_len(vec) }, 0);
        // SAFETY: vec was allocated by hew_dns_resolve and has not been freed.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
    }

    #[test]
    fn lookup_host_null_returns_null() {
        // SAFETY: Null pointer is explicitly handled by hew_dns_lookup_host.
        let result = unsafe { hew_dns_lookup_host(std::ptr::null()) };
        assert!(result.is_null());
    }

    #[test]
    fn resolve_invalid_hostname_returns_empty() {
        let host = ManagedString::new("this-host-does-not-exist.invalid.test");
        // SAFETY: host is a live managed string handle.
        let vec = unsafe { hew_dns_resolve(host.as_ptr()) };
        assert!(!vec.is_null());
        // SAFETY: vec is a valid HewVec returned by hew_dns_resolve.
        assert_eq!(unsafe { hew_cabi::vec::hew_vec_len(vec) }, 0);
        // SAFETY: vec was allocated by hew_dns_resolve and has not been freed.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
    }

    #[test]
    fn lookup_host_invalid_returns_null() {
        let host = ManagedString::new("this-host-does-not-exist.invalid.test");
        // SAFETY: host is a live managed string handle.
        let result = unsafe { hew_dns_lookup_host(host.as_ptr()) };
        assert!(result.is_null());
    }

    #[test]
    fn resolve_empty_string_returns_empty() {
        let host = ManagedString::new("");
        // SAFETY: host is a live managed string handle (empty).
        let vec = unsafe { hew_dns_resolve(host.as_ptr()) };
        assert!(!vec.is_null());
        // SAFETY: vec is a valid HewVec returned by hew_dns_resolve.
        assert_eq!(unsafe { hew_cabi::vec::hew_vec_len(vec) }, 0);
        // SAFETY: vec was allocated by hew_dns_resolve and has not been freed.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
    }

    #[test]
    fn lookup_host_empty_string_returns_null() {
        let host = ManagedString::new("");
        // SAFETY: host is a live managed string handle (empty).
        let result = unsafe { hew_dns_lookup_host(host.as_ptr()) };
        assert!(result.is_null());
    }
}
