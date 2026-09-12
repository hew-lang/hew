//! Who owns the buffer a hew-std `*_last_error` export hands back
//! (hew-lang/hew#2828).
//!
//! The hew-runtime half of the family — `hew_process_last_error`,
//! `hew_stream_last_error`, and the `hew_last_error` counterfactual that proves
//! the probes discriminate a borrow from a transfer — is measured by
//! `hew-runtime/tests/last_error_result_retention.rs`. That file carries the
//! full argument for the legacy instrument. JSON/YAML use managed handles
//! and additionally preserve embedded NUL text after the slot changes or clears.
//!
//! R1 (two live results are distinct allocations), R2 (releasing one result
//! leaves its sibling readable, so each is an independent owner) and R3 (the
//! slot survives the caller's release) together say the returned string is the
//! caller's to release and nothing here retains a pointer into it.
//!
//! Each symbol names its own inducer. The slots backed by
//! [`hew_runtime::parse_error_slot`] are driven directly through that public
//! storage; the three with a module-private thread-local (`cron`, `xml`,
//! `msgpack`) are driven through a public entry point instead. All of them are
//! deterministic and free of I/O.
//!
//! The three handle-scoped QUIC variants (`hew_quic_endpoint_last_error`,
//! `hew_quic_conn_last_error`, `hew_quic_stream_last_error`) require live
//! transport state, so their real loopback R1/R2/R3 proofs live in
//! `quic/string_result_retention.rs` rather than this I/O-free family module.

use crate::test_string::ManagedString;
use hew_cabi::string::{string_as_str, string_release, HewString};
use hew_runtime::parse_error_slot::{set_error, ErrorSlotKind};

/// The diagnostics the two module-private slots hold once their inducer runs.
const CRON_PARSE_ERROR: &str = "cron parse error: not a cron expression\n^\nThe 'Seconds' field does not support using names. 'not' specified.";
const XML_PARSE_ERROR: &str = "xml: parse error";

/// Run R1/R2/R3 against one `-> string` export and assert the returned buffer
/// is transferred to the caller.
///
/// `induce` runs before every read, so a slot that clears on read and a slot
/// that clones on read are measured on the same terms. `expected` is the
/// message the export must report once `induce` has run.
fn assert_result_is_transferred(
    symbol: &str,
    induce: &dyn Fn(),
    expected: &str,
    call: unsafe extern "C" fn() -> *mut HewString,
) {
    induce();
    // SAFETY: the export takes no arguments and returns one managed owner.
    let first = unsafe { call() };
    induce();
    // SAFETY: as above.
    let second = unsafe { call() };
    assert!(
        !first.is_null() && !second.is_null(),
        "{symbol}: expected a message"
    );

    // R1 — both results are live at once, so a shared address would mean the
    // export hands out a borrow into storage it keeps.
    assert_ne!(
        first, second,
        "{symbol}: two live results share an address, so the export does not \
         allocate a fresh buffer per call"
    );

    // SAFETY: `first` is a live managed owner held by this test.
    let text = unsafe { string_as_str(first) }.to_owned();
    assert_eq!(
        text, expected,
        "{symbol}: the export must report the message its slot holds"
    );

    // R2 — each result is an independent owner, so releasing one leaves its
    // sibling readable.
    // SAFETY: both results are live owners held by this test.
    unsafe {
        string_release(first);
        assert_eq!(
            string_as_str(second),
            expected,
            "{symbol}: releasing one result disturbed its sibling, so the two \
             results share one owner"
        );
    }

    // R3 — release the second owner through the release symbol the contract
    // names (`hew_string_drop`), then read again: the slot must be untouched.
    // SAFETY: `second` is live and solely owned.
    unsafe { string_release(second) };
    induce();
    // SAFETY: as above.
    let third = unsafe { call() };
    assert!(
        !third.is_null(),
        "{symbol}: the slot did not survive the release"
    );
    // SAFETY: `third` is a live managed owner.
    unsafe {
        assert_eq!(
            string_as_str(third),
            text,
            "{symbol}: the message changed after the caller released an earlier \
             result, so the export retained a pointer into the freed buffer"
        );
        string_release(third);
    }
}

/// Probe an export whose message lives in the shared
/// [`hew_runtime::parse_error_slot`] storage.
fn assert_slot_backed_result_is_transferred(
    symbol: &str,
    slot: ErrorSlotKind,
    call: unsafe extern "C" fn() -> *mut HewString,
) {
    let message = format!("hew-2828-oracle: {symbol}");
    let induce = || set_error(slot, message.clone());
    assert_result_is_transferred(symbol, &induce, &message, call);
}

#[test]
fn tls_last_error_result_is_transferred() {
    assert_slot_backed_result_is_transferred(
        "hew_tls_last_error",
        ErrorSlotKind::Tls,
        crate::tls::hew_tls_last_error,
    );
}

#[test]
fn smtp_last_error_result_is_transferred() {
    assert_slot_backed_result_is_transferred(
        "hew_smtp_last_error",
        ErrorSlotKind::Smtp,
        crate::smtp::hew_smtp_last_error,
    );
}

#[test]
fn datetime_last_error_result_is_transferred() {
    assert_slot_backed_result_is_transferred(
        "hew_datetime_last_error",
        ErrorSlotKind::Datetime,
        crate::time::datetime::hew_datetime_last_error,
    );
}

/// Managed diagnostics keep complete text after the slot changes or clears.
fn assert_managed_error_is_transferred(
    slot: ErrorSlotKind,
    call: extern "C" fn() -> *mut hew_cabi::string::HewString,
    release: unsafe extern "C" fn(*mut hew_cabi::string::HewString),
) {
    use hew_runtime::parse_error_slot::clear_error;

    let message = "parse diagnostic: clé\0雪\0tail";
    set_error(slot, message.to_owned());
    let first = call();
    let second = call();
    assert!(!first.is_null());
    assert_ne!(first, second);
    // SAFETY: the exports return independent managed owners.
    unsafe {
        assert_eq!(string_as_str(first), message);
        release(first);
        assert_eq!(string_as_str(second), message);
        let third = call();
        set_error(slot, "replacement diagnostic".to_owned());
        let replacement = call();
        clear_error(slot);
        let empty = call();
        assert!(empty.is_null());
        assert_eq!(string_as_str(second), message);
        assert_eq!(string_as_str(third), message);
        assert_eq!(string_as_str(replacement), "replacement diagnostic");
        release(second);
        release(third);
        release(replacement);
        release(empty);
    }
}

#[test]
fn json_last_error_result_is_transferred() {
    assert_managed_error_is_transferred(
        ErrorSlotKind::Json,
        crate::json::hew_json_last_error,
        crate::json::hew_json_string_free,
    );
}

#[test]
fn toml_last_error_result_is_transferred() {
    assert_slot_backed_result_is_transferred(
        "hew_toml_last_error",
        ErrorSlotKind::Toml,
        crate::toml::hew_toml_last_error,
    );
}

#[test]
fn yaml_last_error_result_is_transferred() {
    assert_managed_error_is_transferred(
        ErrorSlotKind::Yaml,
        crate::yaml::hew_yaml_last_error,
        crate::yaml::hew_yaml_string_free,
    );
}

#[test]
fn quic_last_error_result_is_transferred() {
    assert_slot_backed_result_is_transferred(
        "hew_quic_last_error",
        ErrorSlotKind::Quic,
        crate::quic::hew_quic_last_error,
    );
}

/// `cron` keeps its message in a module-private thread-local, so the inducer is
/// the public parse entry point rejecting an expression.
#[test]
fn cron_last_error_result_is_transferred() {
    let expression = ManagedString::new("not a cron expression");
    let induce = || {
        // SAFETY: `expression` owns a live managed string for the call.
        unsafe { crate::time::cron::hew_cron_parse(expression.as_ptr()) };
    };
    assert_result_is_transferred(
        "hew_cron_last_error",
        &induce,
        CRON_PARSE_ERROR,
        crate::time::cron::hew_cron_last_error,
    );
}

/// `xml` keeps its message in a module-private thread-local; same shape.
#[test]
fn xml_last_error_result_is_transferred() {
    let document = ManagedString::new("<unclosed>");
    let induce = || {
        // SAFETY: `document` owns a live managed string for the call.
        unsafe { crate::xml::hew_xml_parse(document.as_ptr()) };
    };
    assert_result_is_transferred(
        "hew_xml_last_error",
        &induce,
        XML_PARSE_ERROR,
        crate::xml::hew_xml_last_error,
    );
}

/// `msgpack` keeps its message in a module-private thread-local; same shape.
#[test]
fn msgpack_last_error_result_is_transferred() {
    let induce = || {
        // SAFETY: a null triple is an accepted input; the export records the
        // error and returns an empty string without dereferencing it.
        let empty = unsafe { crate::msgpack::hew_msgpack_to_json_hew(std::ptr::null()) };
        assert!(empty.is_null(), "a rejected buffer reports no JSON text");
    };
    assert_result_is_transferred(
        "hew_msgpack_last_error",
        &induce,
        "msgpack: invalid input buffer",
        crate::msgpack::hew_msgpack_last_error,
    );
}

#[test]
fn http_last_error_result_is_transferred() {
    assert_managed_error_is_transferred(
        ErrorSlotKind::Http,
        crate::http::client::hew_http_last_error,
        hew_runtime::string::hew_string_drop,
    );
}
