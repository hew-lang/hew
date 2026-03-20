//! Hew `std::net::dns` — DNS hostname resolution.
//!
//! Resolves hostnames to IP address strings using the system resolver
//! (`std::net::ToSocketAddrs`). All returned strings are allocated with
//! `libc::malloc` and NUL-terminated.

#[cfg(test)]
extern crate hew_runtime;

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use hew_cabi::vec::HewVec;
use std::net::ToSocketAddrs;
use std::os::raw::c_char;

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Resolve a hostname to all associated IP addresses.
///
/// Returns a `HewVec` of malloc-allocated C strings, one per resolved address.
/// Returns an empty vec on failure or null input.
///
/// # Safety
///
/// `hostname` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_dns_resolve(hostname: *const c_char) -> *mut HewVec {
    // SAFETY: hew_vec_new_str allocates a valid string-typed HewVec.
    let vec = unsafe { hew_cabi::vec::hew_vec_new_str() };

    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(host) = (unsafe { cstr_to_str(hostname) }) else {
        return vec;
    };

    if host.is_empty() {
        return vec;
    }

    // Append ":0" so ToSocketAddrs can parse it as host:port.
    let addr_str = format!("{host}:0");
    let Ok(addrs) = addr_str.to_socket_addrs() else {
        return vec;
    };

    for addr in addrs {
        let ip = str_to_malloc(&addr.ip().to_string());
        // SAFETY: vec is a valid HewVec; ip is a malloc'd C string.
        // push_str internally strdup's, so free the original.
        unsafe {
            hew_cabi::vec::hew_vec_push_str(vec, ip);
            libc::free(ip.cast());
        }
    }

    vec
}

/// Resolve a hostname to its first IP address.
///
/// Returns a malloc-allocated, NUL-terminated C string with the first
/// resolved address, or null if resolution fails.
///
/// # Safety
///
/// `hostname` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_dns_lookup_host(hostname: *const c_char) -> *mut c_char {
    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(host) = (unsafe { cstr_to_str(hostname) }) else {
        return std::ptr::null_mut();
    };

    if host.is_empty() {
        return std::ptr::null_mut();
    }

    let addr_str = format!("{host}:0");
    let Ok(mut addrs) = addr_str.to_socket_addrs() else {
        return std::ptr::null_mut();
    };

    match addrs.next() {
        Some(addr) => str_to_malloc(&addr.ip().to_string()),
        None => std::ptr::null_mut(),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{CStr, CString};

    /// Helper: read a C string pointer and free it.
    unsafe fn read_and_free(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is non-null (asserted above) and points to a valid NUL-terminated C string.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with libc::malloc by the FFI layer.
        unsafe { libc::free(ptr.cast()) };
        s
    }

    #[test]
    fn resolve_localhost() {
        let host = CString::new("localhost").unwrap();
        // SAFETY: host is a valid NUL-terminated C string.
        let vec = unsafe { hew_dns_resolve(host.as_ptr()) };
        assert!(!vec.is_null());

        // SAFETY: vec is a valid HewVec returned by hew_dns_resolve.
        let len = unsafe { hew_cabi::vec::hew_vec_len(vec) };
        // localhost should resolve to at least one address (127.0.0.1 or ::1).
        assert!(len > 0, "expected at least one address for localhost");

        // SAFETY: vec is valid and index 0 is within bounds (len > 0).
        let first = unsafe { hew_cabi::vec::hew_vec_get_str(vec, 0) };
        assert!(!first.is_null());
        // SAFETY: first is a non-null, valid NUL-terminated C string from the vec.
        let first_str = unsafe { CStr::from_ptr(first) }.to_str().unwrap();
        assert!(
            first_str == "127.0.0.1" || first_str == "::1",
            "expected 127.0.0.1 or ::1, got {first_str}"
        );

        // SAFETY: vec was allocated by hew_dns_resolve and has not been freed.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
    }

    #[test]
    fn lookup_host_localhost() {
        let host = CString::new("localhost").unwrap();
        // SAFETY: host is a valid NUL-terminated C string.
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
        let host = CString::new("this-host-does-not-exist.invalid.test").unwrap();
        // SAFETY: host is a valid NUL-terminated C string.
        let vec = unsafe { hew_dns_resolve(host.as_ptr()) };
        assert!(!vec.is_null());
        // SAFETY: vec is a valid HewVec returned by hew_dns_resolve.
        assert_eq!(unsafe { hew_cabi::vec::hew_vec_len(vec) }, 0);
        // SAFETY: vec was allocated by hew_dns_resolve and has not been freed.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
    }

    #[test]
    fn lookup_host_invalid_returns_null() {
        let host = CString::new("this-host-does-not-exist.invalid.test").unwrap();
        // SAFETY: host is a valid NUL-terminated C string.
        let result = unsafe { hew_dns_lookup_host(host.as_ptr()) };
        assert!(result.is_null());
    }

    #[test]
    fn resolve_empty_string_returns_empty() {
        let host = CString::new("").unwrap();
        // SAFETY: host is a valid NUL-terminated C string (empty).
        let vec = unsafe { hew_dns_resolve(host.as_ptr()) };
        assert!(!vec.is_null());
        // SAFETY: vec is a valid HewVec returned by hew_dns_resolve.
        assert_eq!(unsafe { hew_cabi::vec::hew_vec_len(vec) }, 0);
        // SAFETY: vec was allocated by hew_dns_resolve and has not been freed.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
    }

    #[test]
    fn lookup_host_empty_string_returns_null() {
        let host = CString::new("").unwrap();
        // SAFETY: host is a valid NUL-terminated C string (empty).
        let result = unsafe { hew_dns_lookup_host(host.as_ptr()) };
        assert!(result.is_null());
    }
}
