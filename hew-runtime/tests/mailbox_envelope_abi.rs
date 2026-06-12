//! ABI integration tests for the mailbox envelope payload-class and
//! cross-node send gates.
//!
//! Covers:
//!   - `MailboxPayloadClass` discriminant round-trips (tests 1–4)
//!   - `HewMsgNode` field-offset pins (tests 5–6)
//!   - `CancellationTokenHandle` zero/non-zero semantics (tests 7–8)
//!   - Cross-node send gate enforcement (tests 9–14)
//!   - Allocator zero-init invariant (test 15)

#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI test harness — safety invariants documented per-test"
)]
#![expect(
    clippy::doc_markdown,
    reason = "layout arithmetic in doc comments uses field names as text, not Rust code"
)]

use std::ffi::{c_char, CStr};
use std::mem::offset_of;

use hew_runtime::mailbox::{
    hew_mailbox_free, hew_mailbox_new, hew_mailbox_send, hew_mailbox_try_recv, hew_msg_node_free,
    HewMsgNode,
};
use hew_runtime::mailbox_envelope::{
    validate_cross_node_send_params, CancellationTokenHandle, MailboxPayloadClass,
    CANCEL_TOKEN_NONE, SOURCE_PID_UNKNOWN,
};
use hew_runtime::{hew_clear_error, hew_last_error};

// ── helpers ─────────────────────────────────────────────────────────────────

/// Read the thread-local last-error string into an owned `String`, or `None`
/// if no error has been set.
fn last_error_str() -> Option<String> {
    unsafe {
        let ptr: *const c_char = hew_last_error();
        if ptr.is_null() {
            None
        } else {
            Some(CStr::from_ptr(ptr).to_string_lossy().into_owned())
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// 1–4: MailboxPayloadClass discriminant round-trips
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn payload_class_unit_roundtrip() {
    assert_eq!(MailboxPayloadClass::Unit as u8, 0u8);
    assert_eq!(MailboxPayloadClass::Unit, MailboxPayloadClass::Unit);
}

#[test]
fn payload_class_inline_value_roundtrip() {
    assert_eq!(MailboxPayloadClass::InlineValue as u8, 1u8);
}

#[test]
fn payload_class_heap_boxed_fat_pointer_roundtrip() {
    assert_eq!(MailboxPayloadClass::HeapBoxedFatPointer as u8, 2u8);
}

#[test]
fn payload_class_serialized_cross_node_roundtrip() {
    assert_eq!(MailboxPayloadClass::SerializedCrossNode as u8, 3u8);
}

// ═══════════════════════════════════════════════════════════════════════════
// 5–6: HewMsgNode field-offset pins
// ═══════════════════════════════════════════════════════════════════════════

/// Pin `payload_class` and `source_pid` offsets.
///
/// Layout (64-bit, repr(C)):
///   next(8) + msg_type(4) + pad(4) + data(8) + data_size(8)
///   + reply_channel(8) + envelope(8) = 48
///   + trace_context(40 = 4×u64 + u8 + pad7) = 88
///   + payload_class(u8) = 88
///   + pad(7) + source_pid(u64) = 96
#[test]
fn msg_node_layout_payload_class_and_source_pid_offsets() {
    assert_eq!(offset_of!(HewMsgNode, payload_class), 88);
    assert_eq!(offset_of!(HewMsgNode, source_pid), 96);
}

/// Pin `cancel_token_handle` offset and total struct size.
///
/// cancel_token_handle follows source_pid at offset 96 + 8 = 104.
/// Total struct size: 104 + 8 = 112.
#[test]
fn msg_node_layout_cancel_token_handle_offset() {
    assert_eq!(offset_of!(HewMsgNode, cancel_token_handle), 104);
    assert_eq!(std::mem::size_of::<HewMsgNode>(), 112);
}

// ═══════════════════════════════════════════════════════════════════════════
// 7–8: CancellationTokenHandle zero/non-zero semantics
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn cancel_token_handle_zero_is_none() {
    assert_eq!(CANCEL_TOKEN_NONE, 0u64);
    assert_eq!(SOURCE_PID_UNKNOWN, 0u64);
    // Zero-init node has CANCEL_TOKEN_NONE and SOURCE_PID_UNKNOWN.
    let node: HewMsgNode = unsafe { std::mem::zeroed() };
    assert_eq!(node.cancel_token_handle, CANCEL_TOKEN_NONE);
    assert_eq!(node.source_pid, SOURCE_PID_UNKNOWN);
    assert_eq!(node.payload_class, MailboxPayloadClass::Unit as u8);
}

#[test]
fn cancel_token_handle_nonzero_survives_round_trip() {
    const SENTINEL: CancellationTokenHandle = 0xDEAD_BEEF_CAFE_1234;
    let mut node: HewMsgNode = unsafe { std::mem::zeroed() };
    node.cancel_token_handle = SENTINEL;
    node.source_pid = 42;
    node.payload_class = MailboxPayloadClass::SerializedCrossNode as u8;
    assert_eq!(node.cancel_token_handle, SENTINEL);
    assert_eq!(node.source_pid, 42);
    assert_eq!(
        node.payload_class,
        MailboxPayloadClass::SerializedCrossNode as u8
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// 9–14: Cross-node gate enforcement
// ═══════════════════════════════════════════════════════════════════════════

/// Test 9 — legal baseline: SerializedCrossNode + no cancel → accepted.
#[test]
fn cross_node_gate_accepts_serialized_with_no_cancel() {
    hew_clear_error();
    let result = validate_cross_node_send_params(
        MailboxPayloadClass::SerializedCrossNode as u8,
        CANCEL_TOKEN_NONE,
    );
    assert!(result.is_some(), "expected Some(()), got None");
    assert!(last_error_str().is_none(), "unexpected last error");
}

/// Test 10 — Gate 2 alone: SerializedCrossNode + non-zero cancel → rejected.
#[test]
fn cross_node_gate_serialized_with_nonzero_cancel_fails_token_gate() {
    hew_clear_error();
    let result = validate_cross_node_send_params(MailboxPayloadClass::SerializedCrossNode as u8, 1);
    assert!(result.is_none(), "expected None (gate 2)");
    let err = last_error_str().expect("expected last error to be set");
    assert!(
        err.contains("cancel_token_handle"),
        "gate 2 error must mention cancel_token_handle; got: {err}"
    );
}

/// Test 11 — Gate 1 alone: HeapBoxedFatPointer + no cancel → rejected.
#[test]
fn cross_node_gate_heap_boxed_fat_pointer_fails_payload_gate() {
    hew_clear_error();
    let result = validate_cross_node_send_params(
        MailboxPayloadClass::HeapBoxedFatPointer as u8,
        CANCEL_TOKEN_NONE,
    );
    assert!(result.is_none(), "expected None (gate 1)");
    let err = last_error_str().expect("expected last error to be set");
    assert!(
        err.contains("payload_class"),
        "gate 1 error must mention payload_class; got: {err}"
    );
}

/// Test 12 — Unit payload + non-zero cancel: Gate 1 fires first (Unit=0
/// ≠ SerializedCrossNode=3) before Gate 2 gets a chance. Demonstrates that
/// the payload gate wins even when the cancel-token condition would also fail.
#[test]
fn cross_node_gate_unit_payload_with_nonzero_cancel_fails_payload_gate_first() {
    hew_clear_error();
    let result = validate_cross_node_send_params(MailboxPayloadClass::Unit as u8, 99);
    assert!(result.is_none(), "expected None");
    // Gate 1 fires first because Unit ≠ SerializedCrossNode (3).
    let err = last_error_str().expect("expected last error to be set");
    assert!(
        err.contains("payload_class"),
        "gate 1 error must mention payload_class; got: {err}"
    );
}

/// Test 13 — ordering proof: HeapBoxedFatPointer + non-zero cancel → Gate 1
/// message surfaces, not Gate 2.
#[test]
fn cross_node_gate_payload_gate_wins_when_both_fail() {
    hew_clear_error();
    let result = validate_cross_node_send_params(MailboxPayloadClass::HeapBoxedFatPointer as u8, 7);
    assert!(result.is_none(), "expected None");
    let err = last_error_str().expect("expected last error to be set");
    // Gate 1 error (payload_class) must surface, not Gate 2 (cancel_token_handle).
    assert!(
        err.contains("payload_class"),
        "Gate 1 error must surface when both fail; got: {err}"
    );
    assert!(
        !err.contains("cancel_token_handle"),
        "Gate 2 must NOT surface when Gate 1 fails first; got: {err}"
    );
}

/// Test 14 — release-build enforcement: the guards use `set_last_error` +
/// `return None`, not `debug_assert!`, so they fire in both debug and release
/// builds. Verified by calling the function in this test (which runs under
/// whatever build profile the test suite uses) and confirming the function
/// returns `None` rather than silently succeeding.
///
/// If the guard were `debug_assert!`-only, a `--release` run would return
/// `Some(())` for an invalid payload_class (because `debug_assert!` compiles
/// to a no-op). The assertion below would then fail, catching the regression.
#[test]
fn cross_node_gate_release_build_enforcement_is_not_debug_only() {
    hew_clear_error();
    // Use InlineValue (1) — invalid for cross-node; gate 1 must fire.
    let result_debug_or_release =
        validate_cross_node_send_params(MailboxPayloadClass::InlineValue as u8, CANCEL_TOKEN_NONE);
    // This assertion would fail in a release build if the guard were
    // debug_assert!-only (it would return Some(()) in release mode).
    assert!(
        result_debug_or_release.is_none(),
        "cross-node gate must fire in all build profiles (not debug_assert!-only)"
    );
    let err = last_error_str().expect("last error must be set in all profiles");
    assert!(
        err.contains("payload_class"),
        "gate 1 error must be set in all profiles; got: {err}"
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// 15: Allocator zero-init invariant
// ═══════════════════════════════════════════════════════════════════════════

/// Test 15 — `msg_node_alloc` (exercised via `hew_mailbox_send` +
/// `hew_mailbox_try_recv`) must zero-init the three new ABI fields so
/// that an uninitialized `payload_class` byte cannot silently equal
/// `SerializedCrossNode` (3) and bypass the cross-node gate.
///
/// `alloc_sentinel` is also patched but is never returned to consumers
/// (it lives inside the MPSC queue structure); it is covered by code review
/// and the explicit zero-init in the source, not by a separate observable test.
/// `msg_node_alloc_aliased` is Phase-α fail-closed (dead production path) and
/// is exercised via the in-module `aliased_node_alloc_consumes_envelope_refcount`
/// unit test in mailbox.rs.
#[test]
fn msg_node_alloc_zero_inits_new_abi_fields() {
    // SAFETY: this test owns the mailbox and node exclusively; the mailbox is
    // freed after the node is consumed, which is the correct teardown order.
    unsafe {
        let mb = hew_mailbox_new();
        assert!(!mb.is_null(), "hew_mailbox_new returned null");

        // Send a zero-size message — triggers msg_node_alloc internally.
        let rc = hew_mailbox_send(mb, 42, std::ptr::null_mut(), 0);
        assert_eq!(rc, 0, "hew_mailbox_send failed: {rc}");

        let node: *mut HewMsgNode = hew_mailbox_try_recv(mb);
        assert!(!node.is_null(), "hew_mailbox_try_recv returned null");

        // The three new fields must be zero regardless of what libc::malloc
        // left in those bytes — the allocator now explicitly writes 0.
        assert_eq!(
            (*node).payload_class,
            0,
            "payload_class must be zero (MailboxPayloadClass::Unit) after msg_node_alloc"
        );
        assert_eq!(
            (*node).source_pid,
            0,
            "source_pid must be zero (SOURCE_PID_UNKNOWN) after msg_node_alloc"
        );
        assert_eq!(
            (*node).cancel_token_handle,
            0,
            "cancel_token_handle must be zero (CANCEL_TOKEN_NONE) after msg_node_alloc"
        );

        hew_msg_node_free(node);
        hew_mailbox_free(mb);
    }
}
