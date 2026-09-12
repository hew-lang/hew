//! Hew `std::net::dns` — DNS hostname resolution.
//!
//! Resolves hostnames to IP address strings using the system resolver
//! (`std::net::ToSocketAddrs`). Hostnames borrow managed UTF-8 strings; results
//! transfer managed owners. Null is the canonical empty string.
use hew_cabi::string::{string_as_str, string_from_str, string_release, HewString};
use hew_cabi::vec::HewVec;
use hew_runtime::blocking_pool::{
    shared_blocking_pool_opt, spawn_blocking_result, BlockingPoolError,
};
use std::net::{IpAddr, ToSocketAddrs};
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
/// - `Err(false)` — `getaddrinfo` itself errored, the pool was stopped, its
///   worker panicked, or no runtime is installed (fail closed: the entrypoint
///   returns its empty/null sentinel rather than aborting across the ABI).
fn resolve_via_pool(host: &str, deadline_ms: i64) -> Result<Vec<IpAddr>, bool> {
    let host = format!("{host}:0");
    let deadline = if deadline_ms <= 0 {
        None
    } else {
        // SAFETY: deadline_ms > 0 fits in u64 since it's i64.
        #[expect(clippy::cast_sign_loss, reason = "deadline_ms is checked > 0 above")]
        Some(Duration::from_millis(deadline_ms as u64))
    };
    // Fail closed when no runtime is installed: reaching this offload without a
    // runtime is a programming error, but it must return the empty/null sentinel
    // to the C caller rather than abort in `rt_current()` across the ABI.
    let Some(pool) = shared_blocking_pool_opt() else {
        return Err(false);
    };
    // SAFETY: shared_blocking_pool_opt returns the current runtime's pool, valid
    // for that runtime's lifetime; the resolve runs on a scheduler thread that
    // cleanup joins before the runtime (and its pool) drops, so the pointer is
    // valid for the call.
    let result = unsafe {
        spawn_blocking_result(
            pool,
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
        Ok(Err(()))
        | Err(
            BlockingPoolError::PoolStopped
            | BlockingPoolError::NoRuntime
            | BlockingPoolError::WorkerPanicked,
        ) => Err(false),
    }
}

/// Resolve a hostname to all associated IP addresses.
///
/// Returns an owned `Vec<string>`, one managed string per resolved address.
/// Returns an empty vector on failure, deadline expiry, empty input or embedded NUL.
///
/// # Safety
///
/// `hostname` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_dns_resolve(hostname: *const HewString) -> *mut HewVec {
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
/// `hostname` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_dns_resolve_timed(
    hostname: *const HewString,
    deadline_ms: i64,
) -> *mut HewVec {
    // SAFETY: hew_vec_new_str allocates a valid string-typed HewVec.
    let vec = unsafe { hew_cabi::vec::hew_vec_new_str() };

    // SAFETY: hostname borrows a live managed string or canonical empty.
    let host = unsafe { string_as_str(hostname) };

    if host.is_empty() || host.contains('\0') {
        return vec;
    }

    let Ok(addrs) = resolve_via_pool(host, deadline_ms) else {
        return vec;
    };

    for addr in addrs {
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
/// `hostname` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_dns_lookup_host_timed(
    hostname: *const HewString,
    deadline_ms: i64,
) -> *mut HewString {
    // SAFETY: hostname borrows a live managed string or canonical empty.
    let host = unsafe { string_as_str(hostname) };

    if host.is_empty() || host.contains('\0') {
        return std::ptr::null_mut();
    }

    let Ok(addrs) = resolve_via_pool(host, deadline_ms) else {
        return std::ptr::null_mut();
    };

    match addrs.into_iter().next() {
        Some(addr) => string_from_str(&addr.to_string()),
        None => std::ptr::null_mut(),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_string::ManagedString;

    // Resolving through the blocking pool now requires an installed runtime
    // (`shared_blocking_pool()` reads `rt_current()`); this guard installs a
    // real scheduler under a process-wide lock and stops the pool on drop.
    use crate::net_error_slot_test_support::NetErrorSlotRuntimeGuard;

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
        let _rt = NetErrorSlotRuntimeGuard::new();
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
        let _runtime = NetErrorSlotRuntimeGuard::new();
        for text in ["127.0.0.1\0suffix", "localhost\0suffix", "\0"] {
            let host = ManagedString::new(text);
            // SAFETY: each input is valid managed UTF-8; embedded NUL is an invalid hostname.
            unsafe {
                let plain = hew_dns_resolve(host.as_ptr());
                let timed = hew_dns_resolve_timed(host.as_ptr(), 1_000);
                for result in [plain, timed] {
                    assert!(!result.is_null());
                    assert_eq!(hew_cabi::vec::hew_vec_len(result), 0);
                    hew_cabi::vec::hew_vec_free(result);
                }
                assert!(hew_dns_lookup_host(host.as_ptr()).is_null());
                assert!(hew_dns_lookup_host_timed(host.as_ptr(), 1_000).is_null());
                assert_eq!(string_as_str(host.as_ptr()), text);
            }
        }
    }

    #[test]
    fn lookup_host_localhost() {
        let _rt = NetErrorSlotRuntimeGuard::new();
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

    /// A DNS entrypoint reached with a real hostname but NO runtime installed
    /// must fail closed with the empty/null sentinel, never abort in
    /// `rt_current()` across the C ABI. This is the negative test the original
    /// blocking-pool inventory lacked: the production entrypoints reached the
    /// pool unguarded and would SIGABRT when called before `hew_sched_init`.
    #[test]
    fn resolve_without_runtime_fails_closed() {
        // Hold the shared scheduler lock so no concurrent guard installs a
        // runtime; do NOT install one ourselves.
        let _lock = crate::net_error_slot_test_support::lock_without_runtime();
        assert!(
            shared_blocking_pool_opt().is_none(),
            "test requires no runtime installed"
        );

        let host = ManagedString::new("example.com");
        // SAFETY: host is a live managed string handle; reaching the offload
        // with no runtime must return an empty vec, not abort.
        let vec = unsafe { hew_dns_resolve(host.as_ptr()) };
        assert!(!vec.is_null(), "must return an empty vec, not null/abort");
        // SAFETY: vec is a valid HewVec returned by hew_dns_resolve.
        let len = unsafe { hew_cabi::vec::hew_vec_len(vec) };
        assert_eq!(
            len, 0,
            "no runtime installed => empty resolution, fail closed"
        );
        // SAFETY: vec was allocated by hew_dns_resolve and has not been freed.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };

        // The lookup variant fails closed to null on the same path.
        // SAFETY: host is a live managed string handle.
        let result = unsafe { hew_dns_lookup_host(host.as_ptr()) };
        assert!(
            result.is_null(),
            "lookup with no runtime must return null, not abort"
        );
    }

    #[test]
    fn lookup_host_null_returns_null() {
        // SAFETY: Null pointer is explicitly handled by hew_dns_lookup_host.
        let result = unsafe { hew_dns_lookup_host(std::ptr::null()) };
        assert!(result.is_null());
    }

    #[test]
    fn resolve_invalid_hostname_returns_empty() {
        let _rt = NetErrorSlotRuntimeGuard::new();
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
        let _rt = NetErrorSlotRuntimeGuard::new();
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

    /// `hew_dns_resolve_timed(host, 0)` matches the no-deadline shape:
    /// returns the same addresses as `hew_dns_resolve(host)` for a
    /// well-known local host. Proves the delegating shim is wired right.
    #[test]
    fn resolve_timed_zero_means_no_deadline() {
        let _rt = NetErrorSlotRuntimeGuard::new();
        let host = ManagedString::new("localhost");
        // SAFETY: host is a live managed string handle.
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

        // `shared_blocking_pool()` now resolves the current runtime's pool, so a
        // runtime must be installed for the duration of the offload.
        let _runtime = NetErrorSlotRuntimeGuard::new();

        let release = std::sync::Arc::new(Barrier::new(2));
        let release_clone = std::sync::Arc::clone(&release);

        let start = Instant::now();
        // SAFETY: shared_blocking_pool returns the current runtime's pool, valid
        // while `_runtime` is held.
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
