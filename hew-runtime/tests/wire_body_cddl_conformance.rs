//! CDDL conformance gate for the internode `#[wire]` CBOR body.
//!
//! `schemas/wire-body.cddl` claims to describe the bytes a compiled codec walk
//! emits for each `#[wire]` type. This test makes that claim *checked*: it
//! drives the runtime's event ABI (`hew_ser_*` over a static tagged table) in
//! the sequence the codegen walk calls it, then feeds the finished bytes
//! through a real RFC 8610 CDDL validator (`cddl`) against the rules in
//! `wire-body.cddl`. The cross-process round-trip in
//! `distributed_two_process_e2e.rs` proves the full compiled-binary path.
//!
//! Fail-closed: the CDDL check is additive. A non-conforming body still fails
//! closed at the runtime decoder; the negative cases below assert both that
//! the CDDL rejects a malformed body and that the decoder refuses it.

#![cfg(not(target_arch = "wasm32"))]

use std::ffi::c_void;
use std::path::PathBuf;

use hew_codec::{Member, Table};
use hew_runtime::codec::{
    hew_de_failed, hew_de_free, hew_de_new_raw, hew_de_variant, hew_ser_field, hew_ser_finish_raw,
    hew_ser_i64, hew_ser_new, hew_ser_null, hew_ser_record_begin, hew_ser_record_end, hew_ser_str,
    hew_ser_variant, hew_ser_variant_end,
};
use hew_runtime::xnode_serial::hew_ser_free_bytes;

const CBOR: i32 = 0;

static POINT_MEMBERS: [Member<'static>; 2] = [Member::new("x", 1, 0), Member::new("y", 2, 0)];
/// `#[wire] type WirePoint { x: i64 @1; y: i64 @2; }`
static POINT: Table<'static> = Table::new(&POINT_MEMBERS, true);

static PRESENCE_MEMBERS: [Member<'static>; 3] = [
    Member::new("value", 1, 0),
    Member::new("required_option", 2, 0),
    Member::new(
        "optional_option",
        3,
        Member::ACCEPT_ABSENT | Member::OMIT_NULL,
    ),
];
static PRESENCE: Table<'static> = Table::new(&PRESENCE_MEMBERS, true);

static CMD_MEMBERS: [Member<'static>; 2] = [
    Member::new("Ping", 0, 0),
    Member::new("Move", 1, Member::PAYLOAD),
];
/// `#[wire] enum WireCmd { Ping @0; Move(WirePoint) @1; }`
static CMD: Table<'static> = Table::new(&CMD_MEMBERS, true);

static GREETING_MEMBERS: [Member<'static>; 1] = [Member::new("name", 1, 0)];
static GREETING: Table<'static> = Table::new(&GREETING_MEMBERS, true);

fn wire_body_cddl() -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("schemas")
        .join("wire-body.cddl");
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()))
}

/// Run `walk` against a fresh CBOR sink and return the finished body.
fn encode(walk: impl FnOnce(*mut c_void)) -> Vec<u8> {
    let sink = hew_ser_new(CBOR);
    walk(sink);
    let mut len = 0usize;
    // SAFETY: the walk left one complete value; `len` is writable.
    let ptr = unsafe { hew_ser_finish_raw(sink, &raw mut len) };
    // SAFETY: the runtime returned `len` initialized bytes.
    let bytes = unsafe { std::slice::from_raw_parts(ptr, len) }.to_vec();
    // SAFETY: freed exactly once.
    unsafe { hew_ser_free_bytes(ptr) };
    bytes
}

/// # Safety
/// `sink` is live.
unsafe fn point(sink: *mut c_void, x: i64, y: i64) {
    // SAFETY: per contract; the table is static.
    unsafe {
        hew_ser_record_begin(sink, &raw const POINT);
        hew_ser_field(sink, 0);
        hew_ser_i64(sink, x);
        hew_ser_field(sink, 1);
        hew_ser_i64(sink, y);
        hew_ser_record_end(sink);
    }
}

fn validate_against_rule(rule: &str, bytes: &[u8]) -> Result<(), String> {
    let schema = format!("start = {rule}\n\n{}", wire_body_cddl());
    cddl::validate_cbor_from_slice(&schema, bytes, None).map_err(|e| format!("{e:?}"))
}

#[test]
fn wire_struct_body_conforms_to_cddl() {
    // SAFETY: the sink is live for the walk.
    let bytes = encode(|sink| unsafe { point(sink, 3, 4) });
    for rule in ["wire-point-body", "wire-struct-body", "wire-body"] {
        validate_against_rule(rule, &bytes).unwrap_or_else(|e| panic!("{rule}: {e}"));
    }
}

/// Required `Option` None is a present null; optional None omits its key.
#[test]
fn wire_struct_presence_matrix_conforms_to_cddl() {
    for optional_some in [false, true] {
        // SAFETY: the sink is live for the walk; the table is static.
        let bytes = encode(|sink| unsafe {
            hew_ser_record_begin(sink, &raw const PRESENCE);
            hew_ser_field(sink, 0);
            hew_ser_i64(sink, 7);
            hew_ser_field(sink, 1);
            hew_ser_null(sink);
            hew_ser_field(sink, 2);
            if optional_some {
                hew_ser_i64(sink, 42);
            } else {
                hew_ser_null(sink);
            }
            hew_ser_record_end(sink);
        });
        validate_against_rule("wire-presence-body", &bytes)
            .expect("required/null/optional presence body must conform");
        assert_eq!(bytes.len() > 5, optional_some, "{bytes:?}");
    }
}

#[test]
fn wire_enum_unit_body_conforms_to_cddl() {
    // SAFETY: the sink is live for the walk; the table is static.
    let bytes = encode(|sink| unsafe {
        hew_ser_variant(sink, &raw const CMD, 0);
        hew_ser_variant_end(sink);
    });
    assert_eq!(bytes, [0x00]);
    for rule in [
        "wire-enum-unit",
        "wire-enum-body",
        "wire-cmd-body",
        "wire-body",
    ] {
        validate_against_rule(rule, &bytes).unwrap_or_else(|e| panic!("{rule}: {e}"));
    }
}

/// `WireCmd.Move(WirePoint { x: 3, y: 4 })` → `{ 1 => {1: 3, 2: 4} }`.
#[test]
fn wire_enum_payload_body_conforms_to_cddl() {
    // SAFETY: the sink is live for the walk; the table is static.
    let bytes = encode(|sink| unsafe {
        hew_ser_variant(sink, &raw const CMD, 1);
        point(sink, 3, 4);
        hew_ser_variant_end(sink);
    });
    for rule in [
        "wire-enum-payload",
        "wire-enum-body",
        "wire-cmd-body",
        "wire-body",
    ] {
        validate_against_rule(rule, &bytes).unwrap_or_else(|e| panic!("{rule}: {e}"));
    }
}

#[test]
fn wire_struct_string_field_conforms_to_cddl() {
    let name = hew_cabi::string::string_from_str("hew");
    // SAFETY: the sink is live for the walk; `name` is a live managed string.
    let bytes = encode(|sink| unsafe {
        hew_ser_record_begin(sink, &raw const GREETING);
        hew_ser_field(sink, 0);
        hew_ser_str(sink, name);
        hew_ser_record_end(sink);
    });
    // SAFETY: the test owns `name`.
    unsafe { hew_runtime::string::hew_string_drop(name) };
    validate_against_rule("wire-struct-body", &bytes)
        .expect("string-field struct body must validate against wire-struct-body");
}

// ── Fail-closed (negative) axis ──────────────────────────────────────────────

#[test]
fn non_conforming_struct_body_is_rejected_by_cddl() {
    // `"not a struct"` as CBOR text.
    let mut bytes = vec![0x6c];
    bytes.extend_from_slice(b"not a struct");
    assert!(validate_against_rule("wire-struct-body", &bytes).is_err());
}

/// `{1: [], 2: []}` is not a map-of-one: the CDDL rejects it and so does the
/// runtime decoder, which never fabricates a variant.
#[test]
fn multi_entry_enum_body_is_refused_by_cddl_and_decoder() {
    let multi = [0xa2, 0x01, 0x80, 0x02, 0x80];
    assert!(validate_against_rule("wire-enum-payload", &multi).is_err());
    // A tag the schema does not name is refused too.
    for body in [&multi[..], &[0x09][..]] {
        // SAFETY: `body` is a live slice; the table is static.
        unsafe {
            let reader = hew_de_new_raw(CBOR, body.as_ptr(), body.len());
            assert_eq!(hew_de_variant(reader, &raw const CMD), -1, "{body:?}");
            assert_eq!(hew_de_failed(reader), 1, "{body:?}");
            hew_de_free(reader);
        }
    }
}
