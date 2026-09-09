//! Hew runtime: IP address and CIDR network utilities.
//!
//! Provides IP address validation, classification, and CIDR network
//! calculations for compiled Hew programs. Addresses arrive as borrowed
//! managed strings; returned addresses are owned managed strings released with
//! `hew_string_drop`.
use hew_cabi::string::{string_as_str, string_from_str, HewString};
use std::net::IpAddr;

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
/// `s` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_ip_parse(s: *const HewString) -> i32 {
    // SAFETY: the caller keeps the managed owner alive for this borrow.
    let ip_str = unsafe { string_as_str(s) };
    i32::from(ip_str.parse::<IpAddr>().is_ok())
}

/// Check whether `s` is a valid IPv4 address.
///
/// Returns 1 if IPv4, 0 otherwise.
///
/// # Safety
///
/// `s` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_ip_is_v4(s: *const HewString) -> i32 {
    // SAFETY: the caller keeps the managed owner alive for this borrow.
    let ip_str = unsafe { string_as_str(s) };
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
/// `s` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_ip_is_v6(s: *const HewString) -> i32 {
    // SAFETY: the caller keeps the managed owner alive for this borrow.
    let ip_str = unsafe { string_as_str(s) };
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
/// `s` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_ip_is_loopback(s: *const HewString) -> i32 {
    // SAFETY: the caller keeps the managed owner alive for this borrow.
    let ip_str = unsafe { string_as_str(s) };
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
/// `s` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_ip_is_private(s: *const HewString) -> i32 {
    // SAFETY: the caller keeps the managed owner alive for this borrow.
    let ip_str = unsafe { string_as_str(s) };
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
/// `cidr` and `ip` must be null (canonical empty) or live managed string
/// handles.
#[no_mangle]
pub unsafe extern "C" fn hew_cidr_contains(cidr: *const HewString, ip: *const HewString) -> i32 {
    // SAFETY: the caller keeps both managed owners alive for these borrows.
    let (cidr_str, ip_str) = unsafe { (string_as_str(cidr), string_as_str(ip)) };
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
/// Returns one owned managed string; release it with `hew_string_drop`.
/// Returns null (the empty string) on parse error.
///
/// # Safety
///
/// `cidr` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_cidr_network(cidr: *const HewString) -> *mut HewString {
    // SAFETY: the caller keeps the managed owner alive for this borrow.
    let cidr_str = unsafe { string_as_str(cidr) };
    let Ok(net) = cidr_str.parse::<IpNet>() else {
        return std::ptr::null_mut();
    };
    string_from_str(&net.network().to_string())
}

/// Get the broadcast address of an IPv4 CIDR block.
///
/// Returns one owned managed string; release it with `hew_string_drop`.
/// Returns null (the empty string) for IPv6 networks or on parse error.
///
/// # Safety
///
/// `cidr` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_cidr_broadcast(cidr: *const HewString) -> *mut HewString {
    // SAFETY: the caller keeps the managed owner alive for this borrow.
    let cidr_str = unsafe { string_as_str(cidr) };
    let Ok(net) = cidr_str.parse::<IpNet>() else {
        return std::ptr::null_mut();
    };
    match net {
        IpNet::V4(v4net) => string_from_str(&v4net.broadcast().to_string()),
        IpNet::V6(_) => std::ptr::null_mut(),
    }
}

/// Get the number of host addresses in a CIDR block.
///
/// Returns -1 on parse error or a host count that does not fit i64.
///
/// # Safety
///
/// `cidr` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_cidr_hosts(cidr: *const HewString) -> i64 {
    // SAFETY: the caller keeps the managed owner alive for this borrow.
    let cidr_str = unsafe { string_as_str(cidr) };
    let Ok(net) = cidr_str.parse::<IpNet>() else {
        return -1;
    };
    let total: u128 = net.hosts().count() as u128;
    i64::try_from(total).unwrap_or(-1)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_string::ManagedString;
    use hew_cabi::string::string_release;

    /// Read an owned managed result and release it.
    unsafe fn read_and_release(value: *mut HewString) -> String {
        assert!(!value.is_null());
        // SAFETY: `value` is the live owner returned by the producer.
        let text = unsafe { string_as_str(value) }.to_string();
        // SAFETY: this test holds the only owner of `value`.
        unsafe { string_release(value) };
        text
    }

    #[test]
    fn parse_v4_and_v6() {
        let v4 = ManagedString::new("192.168.1.1");
        let v6 = ManagedString::new("::1");
        let bad = ManagedString::new("not-an-ip");

        // SAFETY: every fixture owns a live managed string for these calls.
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
        let lo4 = ManagedString::new("127.0.0.1");
        let lo6 = ManagedString::new("::1");
        let priv_a = ManagedString::new("10.0.0.1");
        let priv_b = ManagedString::new("172.16.5.1");
        let priv_c = ManagedString::new("192.168.0.1");
        let public = ManagedString::new("8.8.8.8");

        // SAFETY: every fixture owns a live managed string for these calls.
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
        let cidr = ManagedString::new("192.168.1.0/24");
        let inside = ManagedString::new("192.168.1.100");
        let outside = ManagedString::new("192.168.2.1");
        let bad_cidr = ManagedString::new("garbage");

        // SAFETY: every fixture owns a live managed string for these calls.
        unsafe {
            assert_eq!(hew_cidr_contains(cidr.as_ptr(), inside.as_ptr()), 1);
            assert_eq!(hew_cidr_contains(cidr.as_ptr(), outside.as_ptr()), 0);
            assert_eq!(hew_cidr_contains(bad_cidr.as_ptr(), inside.as_ptr()), -1);
            assert_eq!(hew_cidr_contains(std::ptr::null(), inside.as_ptr()), -1);

            assert_eq!(
                read_and_release(hew_cidr_network(cidr.as_ptr())),
                "192.168.1.0"
            );
            assert_eq!(
                read_and_release(hew_cidr_broadcast(cidr.as_ptr())),
                "192.168.1.255"
            );
        }
    }

    #[test]
    fn cidr_hosts_and_v6_broadcast() {
        let cidr24 = ManagedString::new("10.0.0.0/24");
        let cidr32 = ManagedString::new("10.0.0.1/32");
        let v6_cidr = ManagedString::new("::1/128");

        // SAFETY: every fixture owns a live managed string for these calls.
        unsafe {
            // /24 has 254 usable hosts (256 - network - broadcast).
            assert_eq!(hew_cidr_hosts(cidr24.as_ptr()), 254);
            // /32 has 1 host.
            assert_eq!(hew_cidr_hosts(cidr32.as_ptr()), 1);
            // IPv6 /128 has 1 host.
            assert_eq!(hew_cidr_hosts(v6_cidr.as_ptr()), 1);
            // IPv6 broadcast is the canonical empty string.
            assert!(hew_cidr_broadcast(v6_cidr.as_ptr()).is_null());
        }
    }

    #[test]
    fn cidr_hosts_rejects_counts_outside_the_hew_i64_abi() {
        let too_large = ManagedString::new("::/65");
        // SAFETY: `too_large` owns a live managed string for this call.
        assert_eq!(unsafe { hew_cidr_hosts(too_large.as_ptr()) }, -1);
    }
}
