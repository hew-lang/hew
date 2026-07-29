//! R1/R2/R3 retention measurements for deterministic, local runtime string
//! producers.
//!
//! A symbol is admitted as a caller-owned extern result only when this test
//! proves that two live calls return distinct allocations (R1), each arrives
//! with refcount one (R2), and releasing both leaves the producer/input usable
//! for a third call (R3).

use std::ffi::{c_char, CStr, CString};

use hew_runtime::bytes::{hew_bytes_drop, hew_bytes_from_static, hew_bytes_to_string};
use hew_runtime::cabi::{cstring_ensure_unique, free_cstring};
use hew_runtime::log_core::hew_log_encode_field_value;
use hew_runtime::string::hew_char_to_string;

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
