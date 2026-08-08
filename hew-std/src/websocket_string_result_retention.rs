//! Measured result-retention proofs for WebSocket message/error strings.
//!
//! `hew_ws_message_text` is measured with a real C-layout message object and
//! `hew_ws_last_error` with its real actor-aware slot. Each positive path
//! proves distinct simultaneous allocations (R1), refcount one at handoff
//! (R2), and producer state surviving caller release for a third result (R3).
//! Null/empty message and empty-error paths are audited separately.

use std::ffi::{c_char, CStr};

use hew_cabi::cabi::{cstring_ensure_unique, free_cstring, malloc_bytes};
use hew_runtime::parse_error_slot::{self, ErrorSlotKind};

use crate::websocket::{
    hew_ws_last_errno, hew_ws_last_error, hew_ws_message_free, hew_ws_message_text,
    hew_ws_message_type, HewWsMessage,
};

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
fn message_text_result_is_transferred_and_message_survives_release() {
    let payload = b"websocket-message-owner";
    let message = Box::into_raw(Box::new(HewWsMessage {
        msg_type: 0,
        data: malloc_bytes(payload),
        data_len: payload.len(),
    }));

    assert_transferred(
        "hew_ws_message_text",
        // SAFETY: `message` stays live for all three accessor calls.
        || unsafe { hew_ws_message_text(message) },
        |text| assert_eq!(text.to_bytes(), payload),
    );
    assert_eq!(
        // SAFETY: text accessors/releases borrowed, but never consumed, message.
        unsafe { hew_ws_message_type(message) },
        0,
        "message state must remain usable after caller result releases"
    );

    // SAFETY: `message` is the sole live Box and owns its malloc'd payload.
    unsafe { hew_ws_message_free(message) };
}

#[test]
fn message_text_null_and_empty_paths_return_null() {
    // SAFETY: null is explicitly accepted.
    assert!(unsafe { hew_ws_message_text(std::ptr::null()) }.is_null());

    let empty = Box::into_raw(Box::new(HewWsMessage {
        msg_type: 0,
        data: std::ptr::null_mut(),
        data_len: 0,
    }));
    // SAFETY: `empty` is a live local message whose no-data path returns null.
    assert!(unsafe { hew_ws_message_text(empty) }.is_null());
    assert_eq!(
        // SAFETY: the empty message remains valid after the null result.
        unsafe { hew_ws_message_type(empty) },
        0
    );
    // SAFETY: `empty` is the sole live Box; its null data requires no free.
    unsafe { hew_ws_message_free(empty) };
}

#[test]
fn last_error_result_is_transferred_and_error_slot_survives_release() {
    const MESSAGE: &str = "websocket retention owner";
    const ERRNO: i64 = 8123;
    parse_error_slot::set_error_with_errno(ErrorSlotKind::Websocket, ERRNO, MESSAGE);

    assert_transferred(
        "hew_ws_last_error",
        || hew_ws_last_error(),
        |text| assert_eq!(text.to_str().unwrap(), MESSAGE),
    );
    assert_eq!(
        hew_ws_last_errno(),
        ERRNO,
        "error slot state must survive caller result releases"
    );
    parse_error_slot::clear_error(ErrorSlotKind::Websocket);
}

#[test]
fn last_error_empty_path_returns_fresh_allocations_not_a_static_sentinel() {
    parse_error_slot::clear_error(ErrorSlotKind::Websocket);
    assert_transferred(
        "hew_ws_last_error(empty)",
        || hew_ws_last_error(),
        |text| assert!(text.to_bytes().is_empty()),
    );
    assert_eq!(
        hew_ws_last_errno(),
        0,
        "empty detail and errno must remain a coherent empty slot"
    );
}
