//! Measured retention for the native `dns` and compression string exports.
//!
//! This is intentionally separate from the `*_last_error` oracle: these are
//! ordinary OS/I/O producers with different input and state shapes.  Every
//! legacy compression probe establishes R1 (two live allocations differ), R2
//! (the result arrives at rc==1), and R3 (the caller's balancing release leaves a later producer
//! read intact), which is the only evidence that may add
//! `result-retention = "transferred"` to their ownership rows. DNS instead
//! exercises managed releases and results surviving the input/runtime lifetime.

use std::ffi::{c_char, CStr};

use hew_cabi::cabi::{cstring_ensure_unique, free_cstring};

fn assert_result_is_transferred(symbol: &str, mut call: impl FnMut() -> *mut c_char) {
    let first = call();
    let second = call();
    assert!(
        !first.is_null() && !second.is_null(),
        "{symbol}: expected non-null results"
    );
    assert_ne!(
        first, second,
        "{symbol}: two live calls returned one address rather than distinct allocations"
    );
    for (label, ptr) in [("first", first), ("second", second)] {
        // SAFETY: each result is a live header-aware Hew string from the
        // measured export.
        let unique = unsafe { cstring_ensure_unique(ptr) };
        assert_eq!(
            unique, ptr,
            "{symbol}: {label} had another owner at handoff"
        );
    }
    // SAFETY: `first` is a live NUL-terminated result.
    let text = unsafe { CStr::from_ptr(first) }.to_bytes().to_vec();
    // SAFETY: R2 established that both are the caller's sole owners.
    unsafe {
        free_cstring(first);
        free_cstring(second);
    }
    let third = call();
    assert!(
        !third.is_null(),
        "{symbol}: producer/input state did not survive caller release"
    );
    // SAFETY: `third` is a live NUL-terminated result.
    // SAFETY: `third` is a live NUL-terminated result.
    let third_text = unsafe { CStr::from_ptr(third) }.to_bytes();
    assert_eq!(
        third_text,
        text.as_slice(),
        "{symbol}: caller release corrupted a later source read"
    );
    // SAFETY: `third` is a live sole owner by the same R2 producer contract.
    unsafe { free_cstring(third) };
}

#[test]
fn dns_lookup_host_and_forwarded_timed_result_are_transferred() {
    use crate::dns::{hew_dns_lookup_host, hew_dns_lookup_host_timed};
    use crate::test_string::ManagedString;
    use hew_cabi::string::{string_as_str, string_release};

    // Numeric loopback exercises the pool without an external DNS dependency.
    let runtime = crate::net_error_slot_test_support::NetErrorSlotRuntimeGuard::new();
    let host = ManagedString::new("127.0.0.1");
    // SAFETY: host is borrowed by every call; all returned strings are independent owners.
    let results = unsafe {
        let first = hew_dns_lookup_host(host.as_ptr());
        let second = hew_dns_lookup_host(host.as_ptr());
        assert!(!first.is_null());
        assert_ne!(first, second);
        assert_eq!(string_as_str(first), "127.0.0.1");
        string_release(first);
        let third = hew_dns_lookup_host(host.as_ptr());
        let timed = hew_dns_lookup_host_timed(host.as_ptr(), 1_000);
        let timed_later = hew_dns_lookup_host_timed(host.as_ptr(), 1_000);
        assert!(!timed.is_null());
        assert_ne!(timed, timed_later);
        string_release(timed_later);
        [second, third, timed]
    };
    drop(host);
    drop(runtime);
    for result in results {
        // SAFETY: each result owns managed storage independently of the input/runtime.
        unsafe {
            assert_eq!(string_as_str(result), "127.0.0.1");
            string_release(result);
        }
    }
}

#[test]
fn compress_last_error_result_is_transferred() {
    let malformed = b"not a gzip stream";
    assert_result_is_transferred("hew_compress_last_error", || {
        let mut out_len = 0;
        // SAFETY: the byte slice and output-length slot are valid.  The input
        // is deliberately malformed, so this records a fresh deterministic
        // error and returns null before any output ownership can arise.
        let output = unsafe {
            crate::compress::hew_gzip_decompress(
                malformed.as_ptr(),
                malformed.len(),
                &raw mut out_len,
                1_024,
            )
        };
        assert!(output.is_null(), "malformed gzip must fail");
        crate::compress::hew_compress_last_error()
    });
}
