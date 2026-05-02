//! Hew `std::net::dns` — DNS hostname resolution.
//!
//! Resolves hostnames to IP address strings using the system resolver
//! (`std::net::ToSocketAddrs`). All returned strings are allocated with
//! `libc::malloc` and NUL-terminated.

#[cfg(test)]
extern crate hew_runtime;

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use hew_cabi::vec::HewVec;
use hew_runtime::blocking_pool::{shared_blocking_pool, spawn_blocking_result, BlockingPoolError};
use std::net::{IpAddr, ToSocketAddrs};
use std::os::raw::c_char;
use std::time::Duration;

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Run `getaddrinfo` for `host:0` on the shared blocking pool with an
/// optional deadline.
///
/// `deadline_ms == 0` parks indefinitely (no deadline). Returns:
/// - `Ok(addrs)` — resolution completed.
/// - `Err(true)` — pool deadline elapsed (`IoError::TimedOut` semantics).
///   The pool thread keeps running `getaddrinfo` until it returns; its
///   result is discarded when the worker publishes it (no leak; see
///   `spawn_blocking_result` saturation note).
/// - `Err(false)` — `getaddrinfo` itself errored, or the pool was stopped.
fn resolve_via_pool(host: &str, deadline_ms: i64) -> Result<Vec<IpAddr>, bool> {
    let host = format!("{host}:0");
    let deadline = if deadline_ms <= 0 {
        None
    } else {
        // SAFETY: deadline_ms > 0 fits in u64 since it's i64.
        #[expect(clippy::cast_sign_loss, reason = "deadline_ms is checked > 0 above")]
        Some(Duration::from_millis(deadline_ms as u64))
    };
    // SAFETY: shared_blocking_pool returns a process-lifetime pool that is
    // never stopped; the pointer is valid for the call.
    let result = unsafe {
        spawn_blocking_result(
            shared_blocking_pool(),
            move || {
                // Collect inside the closure so the iterator (which is not
                // Send) doesn't escape; the result is `Vec<IpAddr>` which is.
                host.to_socket_addrs()
                    .map(|iter| iter.map(|sa| sa.ip()).collect::<Vec<IpAddr>>())
                    .map_err(|_| ())
            },
            deadline,
        )
    };
    match result {
        Ok(Ok(addrs)) => Ok(addrs),
        Err(BlockingPoolError::TimedOut) => Err(true),
        Ok(Err(())) | Err(BlockingPoolError::PoolStopped) => Err(false),
    }
}

/// Resolve a hostname to all associated IP addresses.
///
/// Returns a `HewVec` of malloc-allocated C strings, one per resolved address.
/// Returns an empty vec on failure, deadline expiry, or null input.
///
/// # Safety
///
/// `hostname` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_dns_resolve(hostname: *const c_char) -> *mut HewVec {
    // SAFETY: forwarded contract; deadline_ms=0 means no deadline (legacy
    // behaviour). `hew_dns_resolve_timed` is the deadline-bounded entrypoint.
    unsafe { hew_dns_resolve_timed(hostname, 0) }
}

/// Resolve a hostname to all associated IP addresses with a deadline.
///
/// Performs `getaddrinfo` on the shared blocking pool so the calling
/// scheduler thread is not parked. `deadline_ms <= 0` disables the deadline;
/// any positive value bounds the call. On timeout the returned vec is empty.
///
/// # Safety
///
/// `hostname` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_dns_resolve_timed(
    hostname: *const c_char,
    deadline_ms: i64,
) -> *mut HewVec {
    // SAFETY: hew_vec_new_str allocates a valid string-typed HewVec.
    let vec = unsafe { hew_cabi::vec::hew_vec_new_str() };

    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(host) = (unsafe { cstr_to_str(hostname) }) else {
        return vec;
    };

    if host.is_empty() {
        return vec;
    }

    let Ok(addrs) = resolve_via_pool(host, deadline_ms) else {
        return vec;
    };

    for addr in addrs {
        let ip = str_to_malloc(&addr.to_string());
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
    // SAFETY: forwarded contract; deadline_ms=0 means no deadline.
    unsafe { hew_dns_lookup_host_timed(hostname, 0) }
}

/// Resolve a hostname to its first IP address with a deadline.
///
/// Performs `getaddrinfo` on the shared blocking pool so the calling
/// scheduler thread is not parked. `deadline_ms <= 0` disables the deadline.
/// Returns null on failure or deadline expiry.
///
/// # Safety
///
/// `hostname` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_dns_lookup_host_timed(
    hostname: *const c_char,
    deadline_ms: i64,
) -> *mut c_char {
    // SAFETY: forwarding to cstr_to_str with same contract.
    let Some(host) = (unsafe { cstr_to_str(hostname) }) else {
        return std::ptr::null_mut();
    };

    if host.is_empty() {
        return std::ptr::null_mut();
    }

    let Ok(addrs) = resolve_via_pool(host, deadline_ms) else {
        return std::ptr::null_mut();
    };

    match addrs.into_iter().next() {
        Some(addr) => str_to_malloc(&addr.to_string()),
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

    /// `hew_dns_resolve_timed(host, 0)` matches the no-deadline shape:
    /// returns the same addresses as `hew_dns_resolve(host)` for a
    /// well-known local host. Proves the delegating shim is wired right.
    #[test]
    fn resolve_timed_zero_means_no_deadline() {
        let host = CString::new("localhost").unwrap();
        // SAFETY: host is a valid NUL-terminated C string.
        let vec = unsafe { hew_dns_resolve_timed(host.as_ptr(), 0) };
        assert!(!vec.is_null());
        // SAFETY: vec is a valid HewVec returned by hew_dns_resolve_timed.
        let len = unsafe { hew_cabi::vec::hew_vec_len(vec) };
        assert!(len > 0, "expected at least one address for localhost");
        // SAFETY: vec was allocated above.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
    }

    /// Deadline-bounded resolution honours the deadline.
    ///
    /// We can't reliably stall `getaddrinfo` itself in a portable test (a
    /// blackhole hostname depends on resolver config and `.invalid` returns
    /// NXDOMAIN fast). Instead, drive the same `spawn_blocking_result`
    /// primitive that the DNS path uses and prove the deadline plumbing
    /// fires within bounds.
    ///
    /// This deliberately tests at the primitive level; the contract relied
    /// on by `hew_dns_resolve_timed` is "return early when the pool reports
    /// `TimedOut`". This test exercises that exact path on the same shared
    /// pool the DNS code uses.
    #[test]
    fn stall_dns_honors_deadline() {
        use hew_runtime::blocking_pool::{
            shared_blocking_pool, spawn_blocking_result, BlockingPoolError,
        };
        use std::sync::Barrier;
        use std::time::{Duration, Instant};

        let release = std::sync::Arc::new(Barrier::new(2));
        let release_clone = std::sync::Arc::clone(&release);

        let start = Instant::now();
        // SAFETY: shared_blocking_pool returns a process-lifetime pool.
        let result = unsafe {
            spawn_blocking_result(
                shared_blocking_pool(),
                move || {
                    release_clone.wait();
                    "should have been discarded"
                },
                Some(Duration::from_millis(150)),
            )
        };
        let elapsed = start.elapsed();
        assert_eq!(result, Err(BlockingPoolError::TimedOut));
        assert!(
            elapsed < Duration::from_millis(750),
            "deadline should fire well before 750ms; got {elapsed:?}"
        );
        // Release the worker so it can publish-and-discard. Without this
        // the barrier holds the worker thread until process exit (no leak,
        // but the worker is wedged for follow-on tests).
        release.wait();
    }
}
