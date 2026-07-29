//! Who owns the buffer a hew-std `*_last_error` export hands back
//! (hew-lang/hew#2828).
//!
//! The hew-runtime half of the family — `hew_process_last_error`,
//! `hew_stream_last_error`, and the `hew_last_error` counterfactual that proves
//! the probes discriminate a borrow from a transfer — is measured by
//! `hew-runtime/tests/last_error_result_retention.rs`. That file carries the
//! full argument for the instrument; this one applies the identical three
//! probes to the exports that live in this crate.
//!
//! R1 (two live results are distinct addresses), R2 (`rc == 1` at handoff, read
//! through the non-destructive `cstring_ensure_unique`) and R3 (the slot
//! survives the caller's release) together say the returned buffer is the
//! caller's to release and nothing here retains a pointer into it.
//!
//! Each symbol names its own inducer. The slots backed by
//! [`hew_runtime::parse_error_slot`] are driven directly through that public
//! storage; the three with a module-private thread-local (`cron`, `xml`,
//! `msgpack`) are driven through a public entry point instead. All of them are
//! deterministic and free of I/O.
//!
//! `hew_http_last_error` has no reachable non-empty path from outside its own
//! module — its slot is written only on an allocation failure the crate injects
//! under `#[cfg(test)]` from inside that module — so it is probed on the
//! empty-message path. That is not a weaker measurement of the question asked
//! here: the export's single statement is
//! `str_to_malloc(&get_http_last_error())`, so the empty path allocates through
//! the identical call and R1/R2/R3 read the identical buffer.
//!
//! The three handle-scoped QUIC variants (`hew_quic_endpoint_last_error`,
//! `hew_quic_conn_last_error`, `hew_quic_stream_last_error`) require live
//! transport state, so their real loopback R1/R2/R3 proofs live in
//! `quic/string_result_retention.rs` rather than this I/O-free family module.

use std::ffi::{c_char, CStr};

use hew_cabi::cabi::{cstring_ensure_unique, free_cstring};
use hew_runtime::parse_error_slot::{set_error, ErrorSlotKind};

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
    call: unsafe extern "C" fn() -> *mut c_char,
) {
    induce();
    // SAFETY: the export takes no arguments and returns a header-aware Hew
    // string allocated by `str_to_malloc`.
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

    // R2 — the caller holds the sole owner of each result.
    for (label, ptr) in [("first", first), ("second", second)] {
        // SAFETY: the pointer is a live header-aware Hew string from the export.
        let unique = unsafe { cstring_ensure_unique(ptr) };
        assert_eq!(
            unique, ptr,
            "{symbol}: the {label} result was not solely owned at handoff \
             (rc > 1), so the caller's release would not balance it"
        );
    }

    // SAFETY: `first` is a live header-aware Hew string.
    let text = unsafe { CStr::from_ptr(first) }.to_owned();
    assert_eq!(
        text.to_str().expect("the oracle message is UTF-8"),
        expected,
        "{symbol}: the export must report the message its slot holds"
    );

    // R3 — release both through the release symbol the contract names
    // (`hew_string_drop` is `free_cstring` with the static-literal skip in
    // front), then read again: the slot must be untouched by the free.
    // SAFETY: each pointer is live and solely owned (R2), so this is its
    // balancing release.
    unsafe {
        free_cstring(first);
        free_cstring(second);
    }
    induce();
    // SAFETY: as above.
    let third = unsafe { call() };
    assert!(
        !third.is_null(),
        "{symbol}: the slot did not survive the release"
    );
    // SAFETY: `third` is a live header-aware Hew string.
    let after = unsafe { CStr::from_ptr(third) }.to_owned();
    assert_eq!(
        text, after,
        "{symbol}: the message changed after the caller released an earlier \
         result, so the export retained a pointer into the freed buffer"
    );
    // SAFETY: `third` is live and solely owned.
    unsafe { free_cstring(third) };
}

/// Probe an export whose message lives in the shared
/// [`hew_runtime::parse_error_slot`] storage.
fn assert_slot_backed_result_is_transferred(
    symbol: &str,
    slot: ErrorSlotKind,
    call: unsafe extern "C" fn() -> *mut c_char,
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

#[test]
fn json_last_error_result_is_transferred() {
    assert_slot_backed_result_is_transferred(
        "hew_json_last_error",
        ErrorSlotKind::Json,
        crate::json::hew_json_last_error,
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
    assert_slot_backed_result_is_transferred(
        "hew_yaml_last_error",
        ErrorSlotKind::Yaml,
        crate::yaml::hew_yaml_last_error,
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
/// the public parse entry point rejecting a null expression.
#[test]
fn cron_last_error_result_is_transferred() {
    let induce = || {
        // SAFETY: a null expression is an accepted input; the export records
        // the error and returns null without dereferencing it.
        unsafe { crate::time::cron::hew_cron_parse(std::ptr::null()) };
    };
    assert_result_is_transferred(
        "hew_cron_last_error",
        &induce,
        "invalid cron expression: null pointer or invalid UTF-8",
        crate::time::cron::hew_cron_last_error,
    );
}

/// `xml` keeps its message in a module-private thread-local; same shape.
#[test]
fn xml_last_error_result_is_transferred() {
    let induce = || {
        // SAFETY: a null document is an accepted input; the export records the
        // error and returns null without dereferencing it.
        unsafe { crate::xml::hew_xml_parse(std::ptr::null()) };
    };
    assert_result_is_transferred(
        "hew_xml_last_error",
        &induce,
        "xml: invalid input: null pointer",
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
        // SAFETY: `empty` is a live, solely-owned header-aware Hew string.
        unsafe { free_cstring(empty) };
    };
    assert_result_is_transferred(
        "hew_msgpack_last_error",
        &induce,
        "msgpack: invalid input buffer",
        crate::msgpack::hew_msgpack_last_error,
    );
}

/// `http` has no reachable non-empty path from outside its own module (see the
/// module docs), so it is probed on the empty-message path — the same
/// `str_to_malloc` call, the same buffer, the same three probes.
#[test]
fn http_last_error_result_is_transferred() {
    assert_result_is_transferred(
        "hew_http_last_error",
        &|| {},
        "",
        crate::http::client::hew_http_last_error,
    );
}
