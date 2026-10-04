//! Managed string results survive sibling releases and leave their producer usable.

use hew_cabi::string::{string_as_str, string_release, string_retain, HewString};
use hew_runtime::parse_error_slot::{self, ErrorSlotKind};

use crate::websocket::{hew_ws_last_errno, hew_ws_last_error};

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
