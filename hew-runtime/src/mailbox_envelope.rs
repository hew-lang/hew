//! Mailbox envelope payload classification and cross-node send guards.
//!
//! Provides the [`MailboxPayloadClass`] discriminant that tags [`crate::mailbox::HewMsgNode`]
//! message payloads for routing, serialization gating, and cross-node enforcement.
//!
//! # W3.037 ownership invariant
//!
//! Each `HewMsgNode` carries a `payload_class: u8` field. The runtime owns the
//! classification for the message's lifetime: the class is set at enqueue time
//! and must be treated as immutable thereafter. A receiving actor must not
//! mutate `payload_class`, `source_pid`, or `cancel_token_handle` after
//! dequeue.
//!
//! # FUTURE-EXTENSION-PT: `hew_mailbox_envelope_alloc`
//!
//! Stage 7.5 defers a dedicated `hew_mailbox_envelope_alloc` C-ABI entry that
//! will set `payload_class`, `source_pid`, and `cancel_token_handle` atomically
//! on an allocated node, removing the need for callers to field-initialise
//! individually. Until that lane lands, callers zero-init the node (which maps
//! to `Unit` / `SOURCE_PID_UNKNOWN` / `CANCEL_TOKEN_NONE`) and overwrite
//! individual fields as needed.

/// Discriminant that classifies the payload carried by a
/// [`crate::mailbox::HewMsgNode`].
///
/// Stored as `u8` in `HewMsgNode::payload_class` for ABI stability.
/// Zero-init maps to [`MailboxPayloadClass::Unit`], which is backward
/// compatible with every existing node allocator that predates this field.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MailboxPayloadClass {
    /// No payload bytes; the message is the type tag alone. Zero-init default.
    Unit = 0,
    /// Small scalar or POD value packed inline in the envelope.
    /// The `data` pointer addresses an inline buffer of at most `data_size` bytes.
    InlineValue = 1,
    /// Heap-boxed fat pointer (e.g. `dyn Trait` continuation or arena slice).
    /// The receiving actor owns the box after dequeue. Must not cross node
    /// boundaries without serialisation — Gate 1 in [`validate_cross_node_send_params`]
    /// rejects this class on cross-node sends.
    HeapBoxedFatPointer = 2,
    /// CBOR-serialised payload; safe to transport across node boundaries.
    /// Only this class is accepted by the cross-node send path.
    SerializedCrossNode = 3,
}

/// Opaque integer handle referencing a `HewCancellationToken` by ID.
/// `0` (`CANCEL_TOKEN_NONE`) means no token is associated.
pub type CancellationTokenHandle = u64;

/// Sentinel: no cancellation token is associated with this message.
pub const CANCEL_TOKEN_NONE: CancellationTokenHandle = 0;

/// Sentinel: the source actor PID is not known (e.g. anonymous cross-node send).
pub const SOURCE_PID_UNKNOWN: u64 = 0;

/// Validate `(payload_class, cancel_token_handle)` for a cross-node envelope send.
///
/// Returns `Some(())` when the combination is safe to encode for cross-node
/// transport. Returns `None` and sets the thread-local last error on failure.
///
/// ## Gate ordering (Gate 1 before Gate 2)
///
/// - **Gate 1** — `payload_class` must equal
///   [`MailboxPayloadClass::SerializedCrossNode`] (`3`).  Local-heap types
///   (`HeapBoxedFatPointer`) and bare scalars (`InlineValue`) cannot survive
///   cross-node transport without prior serialisation.
/// - **Gate 2** — `cancel_token_handle` must be [`CANCEL_TOKEN_NONE`] (`0`).
///   Cross-node cancel-token propagation is deferred to a future lane.
///
/// When both conditions fail simultaneously, Gate 1's error surfaces (ordering
/// proof — see `cross_node_gate_payload_gate_wins_when_both_fail` in the ABI
/// integration tests).
///
/// These guards are **not** gated on `debug_assertions`; they fire in both
/// debug and release builds via `set_last_error` + `return None` rather than
/// `debug_assert!`.
#[must_use]
pub fn validate_cross_node_send_params(
    payload_class: u8,
    cancel_token_handle: CancellationTokenHandle,
) -> Option<()> {
    // Gate 1: only serialised payloads may cross node boundaries.
    if payload_class != MailboxPayloadClass::SerializedCrossNode as u8 {
        crate::set_last_error(format!(
            "cross-node send rejected: payload_class={payload_class} is not \
             SerializedCrossNode ({}); serialise the payload before sending cross-node",
            MailboxPayloadClass::SerializedCrossNode as u8,
        ));
        return None;
    }
    // Gate 2: cross-node cancel-token serialisation is not yet implemented.
    if cancel_token_handle != CANCEL_TOKEN_NONE {
        crate::set_last_error(format!(
            "cross-node send rejected: cancel_token_handle={cancel_token_handle} is \
             non-zero; cross-node cancel-token propagation is deferred to a future lane",
        ));
        return None;
    }
    Some(())
}
