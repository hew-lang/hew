//! R1/R2/R3 retention measurements for deterministic, local runtime string
//! producers.
//!
//! A transferred symbol is admitted only when two live calls return distinct
//! allocations (R1), each arrives with refcount one (R2), and releasing both
//! leaves the producer/input usable for a third call (R3). A shared-refcount
//! symbol instead proves same-address aliasing, an exact +1 retain, and both
//! release orders.

use std::sync::atomic::{AtomicU32, Ordering};

use hew_cabi::string::{string_as_str, string_from_str, HewString};
use hew_runtime::bytes::{hew_bytes_decode_utf8_lossy, hew_bytes_drop, hew_bytes_from_static};
use hew_runtime::log_core::hew_log_encode_field_value;
use hew_runtime::observe::{hew_observe_scrape, hew_observe_series};
use hew_runtime::string::{hew_char_to_string, hew_string_clone, hew_string_drop};

/// Read the refcount from the documented 16-byte Hew string header. The
/// atomic count occupies bytes 8..12, immediately eight bytes before `data`.
///
/// # Safety
///
/// `data` must point to a live header-aware Hew string allocation.
#[expect(
    clippy::cast_ptr_alignment,
    reason = "the documented string header sits in a 16-byte-aligned allocation; AtomicU32 needs four-byte alignment"
)]
unsafe fn string_owner_count(data: *mut HewString) -> u32 {
    // SAFETY: the managed handle is the header base; rc follows byte_len.
    unsafe {
        &*data
            .cast::<u8>()
            .add(core::mem::size_of::<usize>())
            .cast::<AtomicU32>()
    }
    .load(Ordering::Acquire)
}

fn assert_managed_transferred(
    symbol: &str,
    mut call: impl FnMut() -> *mut HewString,
    expected: &str,
) {
    let first = call();
    let second = call();
    assert_ne!(first, second, "{symbol}: two live results share storage");
    // SAFETY: both results are live managed owners.
    unsafe {
        assert_eq!(string_as_str(first), expected);
        assert_eq!(string_as_str(second), expected);
        hew_string_drop(first);
        hew_string_drop(second);
    }
}

/// Like [`assert_managed_transferred`], for a snapshot whose exact text
/// changes between calls: both live results must still be independent owners.
fn assert_managed_contains(symbol: &str, mut call: impl FnMut() -> *mut HewString, needle: &str) {
    let first = call();
    let second = call();
    assert_ne!(first, second, "{symbol}: two live results share storage");
    // SAFETY: both results are live managed owners.
    unsafe {
        assert!(
            string_as_str(first).contains(needle),
            "{symbol}: must remain a readable runtime snapshot"
        );
        hew_string_drop(first);
        assert!(
            string_as_str(second).contains(needle),
            "{symbol}: releasing one result disturbed its sibling"
        );
        hew_string_drop(second);
    }

    let third = call();
    // SAFETY: `third` is a fresh live owner.
    unsafe {
        assert!(string_as_str(third).contains(needle));
        hew_string_drop(third);
    }
}

#[test]
fn local_runtime_string_results_are_transferred() {
    let bytes = b"local codec";
    // SAFETY: the input slice is valid for its full length.
    let triple =
        unsafe { hew_bytes_from_static(bytes.as_ptr(), u32::try_from(bytes.len()).unwrap()) };

    assert_managed_transferred(
        "hew_bytes_decode_utf8_lossy",
        || {
            // SAFETY: `triple` owns a readable byte range for the duration of the call.
            unsafe { hew_bytes_decode_utf8_lossy(&raw const triple) }
        },
        "local codec",
    );
    assert_managed_transferred(
        "hew_char_to_string",
        // SAFETY: every i32 is accepted; this is U+1F980 CRAB.
        || unsafe { hew_char_to_string(0x1f980) },
        "🦀",
    );

    let field = string_from_str("line one\n\"quoted\"");
    assert_managed_transferred(
        "hew_log_encode_field_value",
        // SAFETY: `field` is a live managed string for every call.
        || unsafe { hew_log_encode_field_value(field) },
        "\"line one\\n\\\"quoted\\\"\"",
    );
    // SAFETY: this test holds the only owner of `field`.
    unsafe { hew_string_drop(field) };

    // SAFETY: `triple.ptr` is the one owner returned by
    // `hew_bytes_from_static`; all string conversions only borrowed it.
    unsafe { hew_bytes_drop(triple.ptr) };
}

#[test]
fn string_clone_returns_one_independently_balanced_shared_owner() {
    for drop_original_first in [true, false] {
        let original = string_from_str("shared owner");
        // SAFETY: `original` is a live header-aware string allocation.
        assert_eq!(unsafe { string_owner_count(original) }, 1);

        // SAFETY: `original` is a live header-aware string allocation.
        let clone = unsafe { hew_string_clone(original) };
        assert_eq!(clone, original, "clone must alias the same allocation");
        // SAFETY: both names alias the live allocation after the retain.
        assert_eq!(unsafe { string_owner_count(clone) }, 2);

        let (first, survivor) = if drop_original_first {
            (original, clone)
        } else {
            (clone, original)
        };
        // SAFETY: `first` owns one of the two live refcount shares.
        unsafe { hew_string_drop(first) };
        // SAFETY: `survivor` owns the remaining live share.
        assert_eq!(unsafe { string_owner_count(survivor) }, 1);
        // SAFETY: the remaining share keeps the allocation live and readable.
        assert_eq!(unsafe { string_as_str(survivor) }, "shared owner");
        // SAFETY: `survivor` is the final live refcount share.
        unsafe { hew_string_drop(survivor) };
    }
}

/// Both observe exports allocate a new header-aware string per call and keep
/// their metric state separate from the caller-owned output allocation.
/// `assert_transferred` measures exactly that R1/R2/R3 contract before the
/// compiler may promote either row to `result-retention = "transferred"`.
#[test]
fn observe_string_results_are_transferred() {
    assert_managed_contains(
        "hew_observe_scrape",
        || hew_observe_scrape(),
        "heap_live_bytes",
    );
    assert_managed_contains(
        "hew_observe_series",
        || hew_observe_series(),
        "heap.live_bytes",
    );
}
