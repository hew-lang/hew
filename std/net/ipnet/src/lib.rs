//! Hew runtime: IP address and CIDR network utilities.
//!
//! Provides IP address validation, classification, and CIDR network
//! calculations for compiled Hew programs. All returned strings are allocated
//! with `libc::malloc` and NUL-terminated.

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code.
#[cfg(test)]
extern crate hew_runtime;

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use std::net::IpAddr;
use std::os::raw::c_char;

use ipnet::IpNet;

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Validate whether `s` is a valid IP address (v4 or v6).
///
/// Returns 1 if valid, 0 otherwise.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ip_parse(s: *const c_char) -> i32 {
    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(ip_str) = (unsafe { cstr_to_str(s) }) else {
        return 0;
    };
    i32::from(ip_str.parse::<IpAddr>().is_ok())
}

/// Check whether `s` is a valid IPv4 address.
///
/// Returns 1 if IPv4, 0 otherwise.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ip_is_v4(s: *const c_char) -> i32 {
    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(ip_str) = (unsafe { cstr_to_str(s) }) else {
        return 0;
    };
    match ip_str.parse::<IpAddr>() {
        Ok(IpAddr::V4(_)) => 1,
        _ => 0,
    }
}

/// Check whether `s` is a valid IPv6 address.
///
/// Returns 1 if IPv6, 0 otherwise.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ip_is_v6(s: *const c_char) -> i32 {
    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(ip_str) = (unsafe { cstr_to_str(s) }) else {
        return 0;
    };
    match ip_str.parse::<IpAddr>() {
        Ok(IpAddr::V6(_)) => 1,
        _ => 0,
    }
}

/// Check whether `s` is a loopback address (`127.0.0.0/8` for v4, `::1` for
/// v6).
///
/// Returns 1 if loopback, 0 otherwise.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ip_is_loopback(s: *const c_char) -> i32 {
    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(ip_str) = (unsafe { cstr_to_str(s) }) else {
        return 0;
    };
    match ip_str.parse::<IpAddr>() {
        Ok(ip) => i32::from(ip.is_loopback()),
        Err(_) => 0,
    }
}

/// Check whether `s` is an RFC 1918 private IPv4 address (`10.0.0.0/8`,
/// `172.16.0.0/12`, `192.168.0.0/16`).
///
/// Returns 1 if private, 0 otherwise (including for IPv6 addresses).
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ip_is_private(s: *const c_char) -> i32 {
    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(ip_str) = (unsafe { cstr_to_str(s) }) else {
        return 0;
    };
    match ip_str.parse::<IpAddr>() {
        Ok(IpAddr::V4(v4)) => {
            let octets = v4.octets();
            let is_private = octets[0] == 10
                || (octets[0] == 172 && (octets[1] & 0xf0) == 16)
                || (octets[0] == 192 && octets[1] == 168);
            i32::from(is_private)
        }
        _ => 0,
    }
}

/// Check whether a CIDR network contains the given IP address.
///
/// Returns 1 if the CIDR contains the IP, 0 if it does not, or -1 on parse
/// error.
///
/// # Safety
///
/// Both `cidr` and `ip` must be valid NUL-terminated C strings, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_cidr_contains(cidr: *const c_char, ip: *const c_char) -> i32 {
    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(cidr_str) = (unsafe { cstr_to_str(cidr) }) else {
        return -1;
    };
    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(ip_str) = (unsafe { cstr_to_str(ip) }) else {
        return -1;
    };
    let Ok(net) = cidr_str.parse::<IpNet>() else {
        return -1;
    };
    let Ok(addr) = ip_str.parse::<IpAddr>() else {
        return -1;
    };
    i32::from(net.contains(&addr))
}

/// Get the network address of a CIDR block.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with [`hew_cidr_free`]. Returns null on parse error or null input.
///
/// # Safety
///
/// `cidr` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_cidr_network(cidr: *const c_char) -> *mut c_char {
    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(cidr_str) = (unsafe { cstr_to_str(cidr) }) else {
        return std::ptr::null_mut();
    };
    let Ok(net) = cidr_str.parse::<IpNet>() else {
        return std::ptr::null_mut();
    };
    str_to_malloc(&net.network().to_string())
}

/// Get the broadcast address of an IPv4 CIDR block.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with [`hew_cidr_free`]. Returns null for IPv6 networks, on parse error,
/// or null input.
///
/// # Safety
///
/// `cidr` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_cidr_broadcast(cidr: *const c_char) -> *mut c_char {
    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(cidr_str) = (unsafe { cstr_to_str(cidr) }) else {
        return std::ptr::null_mut();
    };
    let Ok(net) = cidr_str.parse::<IpNet>() else {
        return std::ptr::null_mut();
    };
    match net {
        IpNet::V4(v4net) => str_to_malloc(&v4net.broadcast().to_string()),
        IpNet::V6(_) => std::ptr::null_mut(),
    }
}

/// Get the number of host addresses in a CIDR block.
///
/// Returns 0 on parse error or null input.
///
/// # Safety
///
/// `cidr` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_cidr_hosts(cidr: *const c_char) -> u64 {
    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(cidr_str) = (unsafe { cstr_to_str(cidr) }) else {
        return 0;
    };
    let Ok(net) = cidr_str.parse::<IpNet>() else {
        return 0;
    };
    let total: u128 = net.hosts().count() as u128;
    // Saturate to u64::MAX for very large IPv6 ranges.
    u64::try_from(total).unwrap_or(u64::MAX)
}

/// Free a string previously returned by [`hew_cidr_network`] or
/// [`hew_cidr_broadcast`].
///
/// # Safety
///
/// `s` must be a pointer previously returned by a `hew_cidr_*` string function,
/// and must not have been freed already. Null is accepted (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_cidr_free(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    // SAFETY: s was allocated with libc::malloc in str_to_malloc.
    unsafe { libc::free(s.cast()) };
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
        // SAFETY: ptr is a valid NUL-terminated C string from malloc.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with malloc.
        unsafe { libc::free(ptr.cast()) };
        s
    }

    #[test]
    fn parse_v4_and_v6() {
        let v4 = CString::new("192.168.1.1").unwrap();
        let v6 = CString::new("::1").unwrap();
        let bad = CString::new("not-an-ip").unwrap();

        // SAFETY: all CString pointers are valid NUL-terminated strings.
        unsafe {
            assert_eq!(hew_ip_parse(v4.as_ptr()), 1);
            assert_eq!(hew_ip_parse(v6.as_ptr()), 1);
            assert_eq!(hew_ip_parse(bad.as_ptr()), 0);
            assert_eq!(hew_ip_parse(std::ptr::null()), 0);

            assert_eq!(hew_ip_is_v4(v4.as_ptr()), 1);
            assert_eq!(hew_ip_is_v4(v6.as_ptr()), 0);
            assert_eq!(hew_ip_is_v6(v6.as_ptr()), 1);
            assert_eq!(hew_ip_is_v6(v4.as_ptr()), 0);
        }
    }

    #[test]
    fn loopback_and_private() {
        let lo4 = CString::new("127.0.0.1").unwrap();
        let lo6 = CString::new("::1").unwrap();
        let priv_a = CString::new("10.0.0.1").unwrap();
        let priv_b = CString::new("172.16.5.1").unwrap();
        let priv_c = CString::new("192.168.0.1").unwrap();
        let public = CString::new("8.8.8.8").unwrap();

        // SAFETY: all CString pointers are valid NUL-terminated strings.
        unsafe {
            assert_eq!(hew_ip_is_loopback(lo4.as_ptr()), 1);
            assert_eq!(hew_ip_is_loopback(lo6.as_ptr()), 1);
            assert_eq!(hew_ip_is_loopback(public.as_ptr()), 0);

            assert_eq!(hew_ip_is_private(priv_a.as_ptr()), 1);
            assert_eq!(hew_ip_is_private(priv_b.as_ptr()), 1);
            assert_eq!(hew_ip_is_private(priv_c.as_ptr()), 1);
            assert_eq!(hew_ip_is_private(public.as_ptr()), 0);
            // IPv6 is not considered RFC 1918 private.
            assert_eq!(hew_ip_is_private(lo6.as_ptr()), 0);
        }
    }

    #[test]
    fn cidr_contains_and_network() {
        let cidr = CString::new("192.168.1.0/24").unwrap();
        let inside = CString::new("192.168.1.100").unwrap();
        let outside = CString::new("192.168.2.1").unwrap();
        let bad_cidr = CString::new("garbage").unwrap();

        // SAFETY: all CString pointers are valid NUL-terminated strings.
        unsafe {
            assert_eq!(hew_cidr_contains(cidr.as_ptr(), inside.as_ptr()), 1);
            assert_eq!(hew_cidr_contains(cidr.as_ptr(), outside.as_ptr()), 0);
            assert_eq!(hew_cidr_contains(bad_cidr.as_ptr(), inside.as_ptr()), -1);
            assert_eq!(hew_cidr_contains(std::ptr::null(), inside.as_ptr()), -1);

            assert_eq!(
                read_and_free(hew_cidr_network(cidr.as_ptr())),
                "192.168.1.0"
            );
            assert_eq!(
                read_and_free(hew_cidr_broadcast(cidr.as_ptr())),
                "192.168.1.255"
            );
        }
    }

    #[test]
    fn cidr_hosts_and_v6_broadcast() {
        let cidr24 = CString::new("10.0.0.0/24").unwrap();
        let cidr32 = CString::new("10.0.0.1/32").unwrap();
        let v6_cidr = CString::new("::1/128").unwrap();

        // SAFETY: all CString pointers are valid NUL-terminated strings.
        unsafe {
            // /24 has 254 usable hosts (256 - network - broadcast).
            assert_eq!(hew_cidr_hosts(cidr24.as_ptr()), 254);
            // /32 has 1 host.
            assert_eq!(hew_cidr_hosts(cidr32.as_ptr()), 1);
            // IPv6 /128 has 1 host.
            assert_eq!(hew_cidr_hosts(v6_cidr.as_ptr()), 1);
            // IPv6 broadcast returns null.
            assert!(hew_cidr_broadcast(v6_cidr.as_ptr()).is_null());
        }
    }

    #[test]
    fn free_null_is_noop() {
        // SAFETY: null is accepted by hew_cidr_free.
        unsafe { hew_cidr_free(std::ptr::null_mut()) };
    }
}
