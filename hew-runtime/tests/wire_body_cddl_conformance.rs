//! CDDL conformance gate for the internode `#[wire]` CBOR body.
//!
//! `schemas/wire-body.cddl` claims to describe the bytes the codegen serializer
//! emits for each `#[wire]` type. This test makes that claim *checked*: it drives
//! the runtime's CBOR serializer primitives in the exact sequence the codegen
//! emitter calls them (`hew_cbor_ser_begin_map`, then per-field
//! `hew_cbor_ser_key_u64` and the value primitives — see `emit_ser_value_cbor` /
//! `emit_ser_enum_cbor` in `hew-codegen-rs/src/llvm.rs`), then feeds the finished
//! bytes through a real RFC 8610 CDDL validator (`cddl`) against the rules in
//! `wire-body.cddl`.
//!
//! Driving the same FFI primitives the emitter calls produces byte-identical
//! output to a compiled `#[wire]` send — the emitter's whole job is to sequence
//! these primitives — so this is a genuine conformance check on the wire body,
//! not a hand-mirrored structural guess that could silently drift from the CDDL.
//! The cross-process round-trip in `distributed_two_process_e2e.rs::
//! wire_cbor_cross_process_round_trip` proves the full compiled-binary path.
//!
//! Fail-closed (CLAUDE.md §2, `serializer-fail-closed`): the CDDL check here is
//! ADDITIVE. A non-conforming body still fails closed at the runtime decoder
//! regardless of the validator — the negative cases below assert both that the
//! CDDL rejects a malformed body AND that a malformed body decodes to a
//! fail-closed result (never a fabricated value).

#![cfg(not(target_arch = "wasm32"))]

use std::ffi::CString;
use std::path::PathBuf;

use hew_runtime::cbor_serial::{
    hew_cbor_de_enum_begin, hew_cbor_de_failed, hew_cbor_de_free, hew_cbor_de_new,
    hew_cbor_ser_begin_array, hew_cbor_ser_begin_map, hew_cbor_ser_end_array, hew_cbor_ser_end_map,
    hew_cbor_ser_finish, hew_cbor_ser_i64, hew_cbor_ser_key_u64, hew_cbor_ser_new,
    hew_cbor_ser_string, hew_cbor_ser_u64,
};
use hew_runtime::xnode_serial::hew_ser_free_bytes;

/// The committed CDDL schema text. Loaded from the file so the test validates
/// against the SAME schema that ships, not an inline copy that could drift.
fn wire_body_cddl() -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("schemas")
        .join("wire-body.cddl");
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()))
}

/// Finish a serializer buffer into an owned `Vec<u8>`, freeing the FFI buffer.
/// Panics on a fail-closed (null) finish — a well-formed encode must produce
/// bytes.
///
/// # Safety
/// `buf` must be a live handle from `hew_cbor_ser_new` whose container frames are
/// balanced, and must not have been finished or aborted already.
unsafe fn finish_to_vec(buf: *mut std::ffi::c_void) -> Vec<u8> {
    let mut len: usize = 0;
    // SAFETY: `buf` is a live, balanced handle per this fn's contract; `len` is a
    // valid writable `*mut usize`.
    let ptr = unsafe { hew_cbor_ser_finish(buf, &raw mut len) };
    assert!(
        !ptr.is_null(),
        "serializer finished null (fail-closed) on a well-formed encode"
    );
    assert!(len > 0, "serializer produced zero-length output");
    // SAFETY: `hew_cbor_ser_finish` returned a non-null buffer of exactly `len`
    // bytes (asserted above).
    let bytes = unsafe { std::slice::from_raw_parts(ptr, len) }.to_vec();
    // SAFETY: `ptr` came from `hew_cbor_ser_finish` and is freed exactly once here.
    unsafe { hew_ser_free_bytes(ptr) };
    bytes
}

/// Validate `bytes` against a single named CDDL rule, returning the validator's
/// result. The rule is selected by prepending a one-line root alias so the
/// validator entry rule is unambiguous.
fn validate_against_rule(rule: &str, bytes: &[u8]) -> Result<(), String> {
    // The `cddl` validator uses the first rule as the root unless told otherwise;
    // prepend a root that aliases the rule under test so we validate exactly it.
    let schema = format!("start = {rule}\n\n{}", wire_body_cddl());
    cddl::validate_cbor_from_slice(&schema, bytes, None).map_err(|e| format!("{e:?}"))
}

/// `#[wire] struct WirePoint { x: i64 @1, y: i64 @2 }` body, encoded exactly as
/// `emit_ser_value_cbor` would: `begin_map`, key 1 → i64, key 2 → i64, `end_map`.
#[test]
fn wire_struct_body_conforms_to_cddl() {
    let buf = hew_cbor_ser_new();
    // SAFETY: `buf` is a live handle from `hew_cbor_ser_new`, balanced
    // begin_map/end_map, and is consumed exactly once by `finish_to_vec`.
    unsafe {
        hew_cbor_ser_begin_map(buf);
        hew_cbor_ser_key_u64(buf, 1);
        hew_cbor_ser_i64(buf, 3);
        hew_cbor_ser_key_u64(buf, 2);
        hew_cbor_ser_i64(buf, 4);
        hew_cbor_ser_end_map(buf);
    }
    // SAFETY: `buf` is the same live handle, not yet finished elsewhere.
    let bytes = unsafe { finish_to_vec(buf) };

    validate_against_rule("wire-point-body", &bytes)
        .expect("WirePoint body must validate against wire-point-body");
    // The generic open-map rule must also accept it (forward-compat shape).
    validate_against_rule("wire-struct-body", &bytes)
        .expect("WirePoint body must validate against wire-struct-body");
    // And the top-level body rule.
    validate_against_rule("wire-body", &bytes)
        .expect("WirePoint body must validate against wire-body");
}

/// `#[wire] enum WireCmd { Ping; Move(WirePoint); }` unit variant `Ping`
/// (ordinal 0) → bare uint `0`, exactly as `emit_ser_enum_cbor`'s unit arm
/// (`hew_cbor_ser_u64(tag)`).
#[test]
fn wire_enum_unit_body_conforms_to_cddl() {
    let buf = hew_cbor_ser_new();
    // SAFETY: `buf` is a live handle from `hew_cbor_ser_new`; a unit enum body is
    // a single bare uint, consumed once by `finish_to_vec`.
    unsafe {
        hew_cbor_ser_u64(buf, 0);
    }
    // SAFETY: `buf` is the same live handle, not yet finished elsewhere.
    let bytes = unsafe { finish_to_vec(buf) };

    validate_against_rule("wire-enum-unit", &bytes)
        .expect("Ping unit body must validate against wire-enum-unit");
    validate_against_rule("wire-enum-body", &bytes)
        .expect("Ping unit body must validate against wire-enum-body");
    validate_against_rule("wire-body", &bytes)
        .expect("Ping unit body must validate against wire-body");
}

/// `WireCmd::Move(WirePoint { x: 3, y: 4 })` payload variant (ordinal 1) →
/// `{ 1 => [ {1: 3, 2: 4} ] }`, exactly as `emit_ser_enum_cbor`'s payload arm
/// (`begin_map`, `key_u64(tag)`, `begin_array`, <field encodes>, `end_array`,
/// `end_map`).
#[test]
fn wire_enum_payload_body_conforms_to_cddl() {
    let buf = hew_cbor_ser_new();
    // SAFETY: `buf` is a live handle from `hew_cbor_ser_new`; every begin_* is
    // matched by an end_* (outer map / payload array / nested struct map) and the
    // handle is consumed exactly once by `finish_to_vec`.
    unsafe {
        hew_cbor_ser_begin_map(buf);
        hew_cbor_ser_key_u64(buf, 1); // variant tag 1 (Move)
        hew_cbor_ser_begin_array(buf); // positional payload array
                                       // payload field 0: the nested WirePoint struct body
        hew_cbor_ser_begin_map(buf);
        hew_cbor_ser_key_u64(buf, 1);
        hew_cbor_ser_i64(buf, 3);
        hew_cbor_ser_key_u64(buf, 2);
        hew_cbor_ser_i64(buf, 4);
        hew_cbor_ser_end_map(buf);
        hew_cbor_ser_end_array(buf);
        hew_cbor_ser_end_map(buf);
    }
    // SAFETY: `buf` is the same live handle, not yet finished elsewhere.
    let bytes = unsafe { finish_to_vec(buf) };

    validate_against_rule("wire-enum-payload", &bytes)
        .expect("Move payload body must validate against wire-enum-payload");
    validate_against_rule("wire-enum-body", &bytes)
        .expect("Move payload body must validate against wire-enum-body");
    validate_against_rule("wire-body", &bytes)
        .expect("Move payload body must validate against wire-body");
}

/// A `#[wire] struct WireGreeting { name: string @1 }` body exercises the
/// text-leaf rule (`hew_cbor_ser_string`).
#[test]
fn wire_struct_string_field_conforms_to_cddl() {
    let buf = hew_cbor_ser_new();
    let name = CString::new("hew").unwrap();
    // SAFETY: `buf` is a live handle; `name` is a valid NUL-terminated C string
    // that outlives the call; begin_map/end_map are balanced.
    unsafe {
        hew_cbor_ser_begin_map(buf);
        hew_cbor_ser_key_u64(buf, 1);
        hew_cbor_ser_string(buf, name.as_ptr());
        hew_cbor_ser_end_map(buf);
    }
    // SAFETY: `buf` is the same live handle, not yet finished elsewhere.
    let bytes = unsafe { finish_to_vec(buf) };

    validate_against_rule("wire-struct-body", &bytes)
        .expect("string-field struct body must validate against wire-struct-body");
}

// ── Fail-closed (negative) axis ──────────────────────────────────────────────

/// A body that does not match the struct map shape (a bare text string) is
/// rejected by the CDDL struct rule. The CDDL is a real contract: it says no to
/// a shape the emitter never produces for a struct body.
#[test]
fn non_conforming_struct_body_is_rejected_by_cddl() {
    // A bare CBOR text string — never a valid struct body (which is a map).
    let mut bytes = Vec::new();
    ciborium::ser::into_writer(
        &ciborium::value::Value::Text("not a struct".into()),
        &mut bytes,
    )
    .unwrap();

    let result = validate_against_rule("wire-struct-body", &bytes);
    assert!(
        result.is_err(),
        "a bare text string must NOT validate as a struct body; got Ok"
    );
}

/// An enum body shaped as a MULTI-entry map (two keys) is not a well-formed
/// map-of-one; the CDDL enum-payload rule rejects it. This is the same shape the
/// runtime reader `hew_cbor_de_enum_begin` fails closed on — the test below
/// asserts that runtime fail-closed behaviour directly.
#[test]
fn multi_entry_enum_body_is_rejected_by_cddl() {
    let mut bytes = Vec::new();
    let bad = ciborium::value::Value::Map(vec![
        (
            ciborium::value::Value::Integer(1.into()),
            ciborium::value::Value::Array(vec![]),
        ),
        (
            ciborium::value::Value::Integer(2.into()),
            ciborium::value::Value::Array(vec![]),
        ),
    ]);
    ciborium::ser::into_writer(&bad, &mut bytes).unwrap();

    let result = validate_against_rule("wire-enum-payload", &bytes);
    assert!(
        result.is_err(),
        "a two-entry map must NOT validate as a map-of-one enum body; got Ok"
    );
}

/// The runtime decoder fails CLOSED on the same malformed enum bodies the CDDL
/// rejects: a multi-entry map and a single entry whose value is not an array
/// both make `hew_cbor_de_enum_begin` set the reader's `failed` flag and return
/// 0 — it never fabricates a variant. This is the load-bearing guarantee: the
/// CDDL check is additive, the runtime reader is the trust boundary, and they
/// agree on what is malformed.
///
/// The `failed` flag (`hew_cbor_de_failed == 1`) is the structural sentinel that
/// distinguishes "decoder failed closed" from "decoded a real tag-0 unit variant
/// (e.g. `Ping`)": a clean tag-0 decode leaves `failed == 0`; these malformed
/// inputs latch `failed == 1`. Asserting the flag (not just `tag == 0`) ensures
/// this test has real teeth — it would catch a decoder that silently decoded a
/// well-formed variant-0 body instead of failing closed.
#[test]
fn malformed_enum_body_fails_closed_at_runtime_decoder() {
    // Multi-entry map: `{1: [], 2: []}` — not a map-of-one.
    let mut multi = Vec::new();
    ciborium::ser::into_writer(
        &ciborium::value::Value::Map(vec![
            (
                ciborium::value::Value::Integer(1.into()),
                ciborium::value::Value::Array(vec![]),
            ),
            (
                ciborium::value::Value::Integer(2.into()),
                ciborium::value::Value::Array(vec![]),
            ),
        ]),
        &mut multi,
    )
    .unwrap();
    // SAFETY: `multi` is a live slice valid for its length; `hew_cbor_de_new`
    // borrows it only for the duration of parsing.
    let reader = unsafe { hew_cbor_de_new(multi.as_ptr(), multi.len()) };
    assert!(!reader.is_null(), "decoder handle for multi-entry body");
    // SAFETY: `reader` is the live handle just created; `hew_cbor_de_enum_begin`
    // and `hew_cbor_de_failed` each take a live handle for the reader's lifetime.
    let tag = unsafe { hew_cbor_de_enum_begin(reader) };
    assert_eq!(tag, 0, "multi-entry enum body must fail closed to tag 0");
    // The `failed` flag distinguishes "failed closed" from "decoded a real tag-0
    // variant": a malformed body MUST latch failed == 1.
    // SAFETY: `reader` is a live handle for the same decoder object just used
    // for `hew_cbor_de_enum_begin` above; it has not been freed yet.
    let failed = unsafe { hew_cbor_de_failed(reader) };
    assert_eq!(
        failed, 1,
        "multi-entry enum body must set the failed flag (not just return tag 0)"
    );
    // SAFETY: `reader` is the same handle, freed exactly once.
    unsafe { hew_cbor_de_free(reader) };

    // Single entry whose value is NOT an array: `{1: 7}`.
    let mut not_array = Vec::new();
    ciborium::ser::into_writer(
        &ciborium::value::Value::Map(vec![(
            ciborium::value::Value::Integer(1.into()),
            ciborium::value::Value::Integer(7.into()),
        )]),
        &mut not_array,
    )
    .unwrap();
    // SAFETY: `not_array` is a live slice valid for its length.
    let reader2 = unsafe { hew_cbor_de_new(not_array.as_ptr(), not_array.len()) };
    assert!(!reader2.is_null(), "decoder handle for non-array body");
    // SAFETY: `reader2` is the live handle just created.
    let tag2 = unsafe { hew_cbor_de_enum_begin(reader2) };
    assert_eq!(
        tag2, 0,
        "single-entry-non-array enum body must fail closed to tag 0"
    );
    // Again, the structural sentinel is the `failed` flag, not the sentinel tag value.
    // SAFETY: `reader2` is a live handle for the same decoder object just used
    // for `hew_cbor_de_enum_begin` above; it has not been freed yet.
    let failed2 = unsafe { hew_cbor_de_failed(reader2) };
    assert_eq!(
        failed2, 1,
        "single-entry-non-array enum body must set the failed flag"
    );
    // SAFETY: `reader2` is the same handle, freed exactly once.
    unsafe { hew_cbor_de_free(reader2) };
}
