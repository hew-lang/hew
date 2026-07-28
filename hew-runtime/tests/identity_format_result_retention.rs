//! Runtime retention measurements for the identity formatters.
//!
//! The public Hew `NodeId::display`, `Location::display`, and
//! `RemotePid::display` calls are compiler synthetics that codegen rewrites to
//! these two exports. MIR may mint the synthetic result's caller-side
//! `hew_string_drop` only after the real runtime allocation has established:
//!
//! * R1 — two simultaneously-live results have distinct addresses;
//! * R2 — each result is solely owned (`rc == 1`) at handoff;
//! * R3 — releasing both results leaves the next formatting call intact.
//!
//! Together those measurements prove the formatter keeps no pointer into its
//! fresh header-aware allocation and the caller owns exactly one balancing
//! release.

use std::ffi::{c_char, CStr};

use hew_cabi::cabi::cstring_ensure_unique;
use hew_runtime::hew_node::{hew_location_format, hew_node_id_format};
use hew_runtime::node_identity::{HewNodeId, HewRemotePid};
use hew_runtime::string::hew_string_drop;

fn assert_result_is_transferred<T: Copy>(
    symbol: &str,
    value: T,
    expected: &str,
    call: unsafe extern "C" fn(*const T) -> *mut c_char,
) {
    // SAFETY: `value` remains live and readable for every call.
    let first = unsafe { call(&raw const value) };
    // SAFETY: as above; `first` remains live for the R1 comparison.
    let second = unsafe { call(&raw const value) };
    assert!(
        !first.is_null() && !second.is_null(),
        "{symbol}: valid identity input must produce two strings"
    );

    // R1 — the formatter allocates independently for each live result.
    assert_ne!(
        first, second,
        "{symbol}: two simultaneously-live results share an address"
    );

    // R2 — no runtime-retained owner exists at handoff.
    for (label, ptr) in [("first", first), ("second", second)] {
        // SAFETY: `ptr` is a live header-aware Hew string from the formatter.
        let unique = unsafe { cstring_ensure_unique(ptr) };
        assert_eq!(
            unique, ptr,
            "{symbol}: {label} result is not solely owned at handoff"
        );
    }

    // SAFETY: both pointers are live NUL-terminated strings.
    assert_eq!(unsafe { CStr::from_ptr(first) }.to_str(), Ok(expected));
    // SAFETY: R2 established that each pointer carries exactly the caller's
    // one share, so the named release balances each allocation.
    unsafe {
        hew_string_drop(first);
        hew_string_drop(second);
    }

    // R3 — a later call still returns the same value after the prior results
    // were released, proving the formatter kept no pointer into either.
    // SAFETY: `value` is still live and readable.
    let third = unsafe { call(&raw const value) };
    assert!(
        !third.is_null(),
        "{symbol}: formatting failed after releasing prior results"
    );
    // SAFETY: `third` is a live NUL-terminated Hew string.
    assert_eq!(unsafe { CStr::from_ptr(third) }.to_str(), Ok(expected));
    // SAFETY: the same R2 allocator path created `third` as a fresh rc=1
    // allocation; this is its one balancing release.
    unsafe { hew_string_drop(third) };
}

#[test]
fn node_id_format_result_is_transferred() {
    let node = HewNodeId {
        hi: 0x0123_4567_89ab_cdef,
        lo: 0xfedc_ba98_7654_3210,
    };
    assert_result_is_transferred(
        "hew_node_id_format",
        node,
        "0123456789abcdeffedcba9876543210",
        hew_node_id_format,
    );
}

#[test]
fn location_format_result_is_transferred() {
    let location = HewRemotePid {
        node: HewNodeId {
            hi: 0x0123_4567_89ab_cdef,
            lo: 0xfedc_ba98_7654_3210,
        },
        slot: 42,
        incarnation: 7,
        reserved: 0,
    };
    assert_result_is_transferred(
        "hew_location_format",
        location,
        "0123456789abcdeffedcba9876543210/42@7",
        hew_location_format,
    );
}
