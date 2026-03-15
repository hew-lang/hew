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

    // Append ":0" so ToSocketAddrs can parse it as host:port.
    let addr_str = format!("{host}:0");
    let Ok(addrs) = addr_str.to_socket_addrs() else {
        return vec;
    };

    for addr in addrs {
        let ip = str_to_malloc(&addr.ip().to_string());
        // SAFETY: vec is a valid HewVec; ip is a malloc'd C string.
        unsafe { hew_cabi::vec::hew_vec_push_str(vec, ip) };
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
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        unsafe { libc::free(ptr.cast()) };
        s
    }

    #[test]
    fn resolve_localhost() {
        let host = CString::new("localhost").unwrap();
        let vec = unsafe { hew_dns_resolve(host.as_ptr()) };
        assert!(!vec.is_null());

        let len = unsafe { hew_cabi::vec::hew_vec_len(vec) };
        // localhost should resolve to at least one address (127.0.0.1 or ::1).
        assert!(len > 0, "expected at least one address for localhost");

        let first = unsafe { hew_cabi::vec::hew_vec_get_str(vec, 0) };
        assert!(!first.is_null());
        let first_str = unsafe { CStr::from_ptr(first) }.to_str().unwrap();
        assert!(
            first_str == "127.0.0.1" || first_str == "::1",
            "expected 127.0.0.1 or ::1, got {first_str}"
        );

        unsafe { hew_cabi::vec::hew_vec_free(vec) };
    }

    #[test]
    fn lookup_host_localhost() {
        let host = CString::new("localhost").unwrap();
        let result = unsafe { hew_dns_lookup_host(host.as_ptr()) };
        assert!(!result.is_null());

        let ip = unsafe { read_and_free(result) };
        assert!(
            ip == "127.0.0.1" || ip == "::1",
            "expected 127.0.0.1 or ::1, got {ip}"
        );
    }

    #[test]
    fn resolve_null_returns_empty_vec() {
        let vec = unsafe { hew_dns_resolve(std::ptr::null()) };
        assert!(!vec.is_null());
        assert_eq!(unsafe { hew_cabi::vec::hew_vec_len(vec) }, 0);
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
    }

    #[test]
    fn lookup_host_null_returns_null() {
        let result = unsafe { hew_dns_lookup_host(std::ptr::null()) };
        assert!(result.is_null());
    }

    #[test]
    fn resolve_invalid_hostname_returns_empty() {
        let host = CString::new("this-host-does-not-exist.invalid.test").unwrap();
        let vec = unsafe { hew_dns_resolve(host.as_ptr()) };
        assert!(!vec.is_null());
        assert_eq!(unsafe { hew_cabi::vec::hew_vec_len(vec) }, 0);
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
    }

    #[test]
    fn lookup_host_invalid_returns_null() {
        let host = CString::new("this-host-does-not-exist.invalid.test").unwrap();
        let result = unsafe { hew_dns_lookup_host(host.as_ptr()) };
        assert!(result.is_null());
    }
}
