//! Managed string results survive sibling releases and leave their producer usable.
//! Raw protocol objects retain their own independent storage and release path.

use hew_cabi::cabi::malloc_bytes;
use hew_cabi::string::{string_as_str, string_release, string_retain, HewString};
use hew_runtime::parse_error_slot::{self, ErrorSlotKind};

use crate::websocket::{
    hew_ws_last_errno, hew_ws_last_error, hew_ws_message_free, hew_ws_message_text,
    hew_ws_message_type, HewWsMessage,
};

fn assert_owned_results(
    symbol: &str,
    mut call: impl FnMut() -> *mut HewString,
    validate: impl Fn(&str),
) {
    let first = call();
    let second = call();
    if !first.is_null() {
        assert_ne!(first, second, "{symbol}: live copies must be independent");
    }
    // SAFETY: both results are owned managed strings, including canonical empty.
    unsafe {
        validate(string_as_str(first));
        validate(string_as_str(second));
        let retained = string_retain(first);
        string_release(first);
        string_release(second);
        validate(string_as_str(retained));
        string_release(retained);
    }
    let third = call();
    // SAFETY: the producer remains usable after releasing earlier results.
    unsafe {
        validate(string_as_str(third));
        string_release(third);
    }
}

#[test]
fn message_text_result_is_transferred_and_message_survives_release() {
    let payload = b"websocket\0message\0owner";
    let message = Box::into_raw(Box::new(HewWsMessage {
        msg_type: 0,
        data: malloc_bytes(payload),
        data_len: payload.len(),
    }));

    assert_owned_results(
        "hew_ws_message_text",
        // SAFETY: `message` stays live for all three accessor calls.
        || unsafe { hew_ws_message_text(message) },
        |text| assert_eq!(text.as_bytes(), payload),
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
fn invalid_utf8_message_text_reports_error_without_consuming_message() {
    let payload = [0xff, 0xfe];
    let message = Box::into_raw(Box::new(HewWsMessage {
        msg_type: 1,
        data: malloc_bytes(&payload),
        data_len: payload.len(),
    }));
    // SAFETY: the message owns its live raw payload throughout the accessor call.
    let text = unsafe { hew_ws_message_text(message) };
    assert!(text.is_null());
    let detail = hew_ws_last_error();
    // SAFETY: detail is an owned managed result and message remains an owned raw frame.
    unsafe {
        assert!(string_as_str(detail).contains("not valid UTF-8"));
        assert_eq!(hew_ws_message_type(message), 1);
        string_release(detail);
        hew_ws_message_free(message);
    }
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

    assert_owned_results(
        "hew_ws_last_error",
        || hew_ws_last_error(),
        |text| assert_eq!(text, MESSAGE),
    );
    assert_eq!(
        hew_ws_last_errno(),
        ERRNO,
        "error slot state must survive caller result releases"
    );
    parse_error_slot::clear_error(ErrorSlotKind::Websocket);
}

#[test]
fn last_error_empty_path_uses_canonical_empty_string() {
    parse_error_slot::clear_error(ErrorSlotKind::Websocket);
    assert_owned_results(
        "hew_ws_last_error(empty)",
        || hew_ws_last_error(),
        |text| assert!(text.is_empty()),
    );
    assert_eq!(
        hew_ws_last_errno(),
        0,
        "empty detail and errno must remain a coherent empty slot"
    );
}
