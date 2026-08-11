//! R1/R2/R3 retention measurements for deterministic, local runtime string
//! producers.
//!
//! A transferred symbol is admitted only when two live calls return distinct
//! allocations (R1), each arrives with refcount one (R2), and releasing both
//! leaves the producer/input usable for a third call (R3). A shared-refcount
//! symbol instead proves same-address aliasing, an exact +1 retain, and both
//! release orders.

use std::ffi::{c_char, CStr, CString};
use std::sync::atomic::{AtomicU32, Ordering};

use hew_runtime::bytes::{hew_bytes_drop, hew_bytes_from_static, hew_bytes_to_string};
use hew_runtime::cabi::{alloc_cstring_from_str, cstring_ensure_unique, free_cstring};
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
    reason = "the documented string header is malloc-aligned; AtomicU32 needs four-byte alignment"
)]
unsafe fn string_owner_count(data: *mut c_char) -> u32 {
    // SAFETY: guaranteed by the caller; malloc alignment exceeds AtomicU32's.
    unsafe { &*data.cast::<u8>().sub(8).cast::<AtomicU32>() }.load(Ordering::Acquire)
}

fn assert_transferred(
    symbol: &str,
    mut call: impl FnMut() -> *mut c_char,
    validate: impl Fn(&CStr),
) {
    let first = call();
    let second = call();
    assert!(
        !first.is_null() && !second.is_null(),
        "{symbol}: expected two live results"
    );
    assert_ne!(
        first, second,
        "{symbol}: R1 failed: two live results share an address"
    );

    for (label, ptr) in [("first", first), ("second", second)] {
        // SAFETY: `ptr` is a live header-aware result from the named producer.
        let unique = unsafe { cstring_ensure_unique(ptr) };
        assert_eq!(
            unique, ptr,
            "{symbol}: R2 failed: the {label} result was shared at handoff"
        );
        // SAFETY: the uniqueness probe returned the live pointer unchanged.
        validate(unsafe { CStr::from_ptr(ptr) });
    }

    // SAFETY: R1/R2 establish two distinct, solely-owned live results.
    unsafe {
        free_cstring(first);
        free_cstring(second);
    }

    let third = call();
    assert!(
        !third.is_null(),
        "{symbol}: R3 failed after caller releases"
    );
    // SAFETY: `third` is a fresh live result.
    validate(unsafe { CStr::from_ptr(third) });
    // SAFETY: the third result is solely owned by this caller.
    unsafe { free_cstring(third) };
}

#[test]
fn local_runtime_string_results_are_transferred() {
    let bytes = b"local codec";
    // SAFETY: the input slice is valid for its full length.
    let triple =
        unsafe { hew_bytes_from_static(bytes.as_ptr(), u32::try_from(bytes.len()).unwrap()) };

    assert_transferred(
        "hew_bytes_to_string",
        // SAFETY: `triple` stays live until the end of the test.
        || unsafe { hew_bytes_to_string(&raw const triple) },
        |text| assert_eq!(text.to_bytes(), bytes),
    );
    assert_transferred(
        "hew_char_to_string",
        // SAFETY: every i32 is accepted; this is U+1F980 CRAB.
        || unsafe { hew_char_to_string(0x1f980) },
        |text| assert_eq!(text.to_str().unwrap(), "🦀"),
    );

    let field = CString::new("line one\n\"quoted\"").unwrap();
    assert_transferred(
        "hew_log_encode_field_value",
        // SAFETY: `field` is a valid NUL-terminated string.
        || unsafe { hew_log_encode_field_value(field.as_ptr()) },
        |text| assert_eq!(text.to_str().unwrap(), "\"line one\\n\\\"quoted\\\"\""),
    );

    // SAFETY: `triple.ptr` is the one owner returned by
    // `hew_bytes_from_static`; all string conversions only borrowed it.
    unsafe { hew_bytes_drop(triple.ptr) };
}

#[test]
fn string_clone_returns_one_independently_balanced_shared_owner() {
    for drop_original_first in [true, false] {
        let original = alloc_cstring_from_str("shared owner");
        assert!(!original.is_null());
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
        let survivor_text = unsafe { CStr::from_ptr(survivor) };
        assert_eq!(survivor_text.to_bytes(), b"shared owner");
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
    assert_transferred(
        "hew_observe_scrape",
        || hew_observe_scrape(),
        |text| {
            assert!(
                text.to_str()
                    .expect("observe scrape is UTF-8")
                    .contains("heap_live_bytes"),
                "scrape must remain a readable runtime snapshot"
            );
        },
    );
    assert_transferred(
        "hew_observe_series",
        || hew_observe_series(),
        |text| {
            assert!(
                text.to_str()
                    .expect("observe series is UTF-8")
                    .contains("heap.live_bytes"),
                "series must remain a readable runtime snapshot"
            );
        },
    );
}
