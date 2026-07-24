//! Target-independent mailbox message-header bit logic.
//!
//! These bits describe in-process COW message payloads and must remain identical
//! across native and WASM mailboxes. They are unrelated to the cross-node CBOR
//! wire envelope.

use crate::internal::types::HewOverflowPolicy;

/// Header bit: at least two observers hold this message payload.
pub const HEW_MSG_ENVELOPE_ALIAS_ACTIVE: u32 = 1 << 0;
/// Header bit: payload is `Frozen`; never forks.
pub const HEW_MSG_ENVELOPE_SHARED_FROZEN: u32 = 1 << 1;
/// Header bit: payload was bumped from a per-dispatch arena.
pub const HEW_MSG_ENVELOPE_ARENA_BACKED: u32 = 1 << 2;
/// Header bit: a fork-on-write has fired.
pub const HEW_MSG_ENVELOPE_FORKED: u32 = 1 << 3;
/// Header bit: payload is a capability transfer; aliasing is forbidden.
pub const HEW_MSG_ENVELOPE_CAPABILITY_TRANSFER: u32 = 1 << 4;
/// Reserved for captured payloads shared by multiple task-scope children.
pub const HEW_MSG_ENVELOPE_RESERVED_GAMMA_A: u32 = 1 << 5;
/// Reserved for an additional shared-task payload contract.
pub const HEW_MSG_ENVELOPE_RESERVED_GAMMA_B: u32 = 1 << 6;
/// Reserved for an arena-backed payload contract.
pub const HEW_MSG_ENVELOPE_RESERVED_DELTA_A: u32 = 1 << 7;
/// Reserved for an additional arena-backed payload contract.
pub const HEW_MSG_ENVELOPE_RESERVED_DELTA_B: u32 = 1 << 8;
/// All bits at or above bit 9 must read zero on every header load.
pub const HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK: u32 = !((1u32 << 9) - 1);

/// System `msg_type` reserved for the shutdown sentinel that a mailbox enqueues
/// onto a Running actor's system queue (native `mailbox::mailbox_send_stop_sys_once`
/// and `mailbox_wasm::mailbox_send_stop_sys_once`).
///
/// It is a **lifecycle signal**, not an application message: each scheduler must
/// OBSERVE it as a self-stop request (gated on system-queue provenance) and free
/// it, never hand it to the actor's user dispatch trampoline. The generated
/// dispatch `match` has no arm for it, so dispatching it lands on the trapping
/// default arm (`ud2`). Lives here — the target-independent header module
/// compiled on BOTH native and wasm32 — so the native (`mailbox.rs`) and WASM
/// (`mailbox_wasm.rs`) paths share ONE authority and cannot drift (D159); the
/// native `mailbox` module is `#[cfg(not(wasm32))]` and unavailable on wasm32.
pub(crate) const HEW_MAILBOX_SHUTDOWN_SENTINEL: i32 = -1;

/// Validate that reserved header bits are zero.
///
/// A newer runtime assigning one of these bits would otherwise let an older
/// runtime silently drop an in-process payload contract.
#[inline]
pub(crate) fn header_validate(bits: u32) -> u32 {
    assert!(
        bits & HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK == 0,
        "hew_msg_envelope: reserved header bits set (bits = {bits:#x}); \
         this runtime does not understand the envelope's contract — \
         refusing to proceed (fail-closed)"
    );
    bits
}

/// Prevent a coalescing mailbox from recursively selecting coalescing as its
/// fallback policy.
pub(crate) fn normalize_coalesce_fallback(policy: HewOverflowPolicy) -> HewOverflowPolicy {
    match policy {
        HewOverflowPolicy::Coalesce => HewOverflowPolicy::DropOld,
        other => other,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bit_layout_fingerprint() {
        assert_eq!(HEW_MSG_ENVELOPE_ALIAS_ACTIVE, 0x0000_0001);
        assert_eq!(HEW_MSG_ENVELOPE_SHARED_FROZEN, 0x0000_0002);
        assert_eq!(HEW_MSG_ENVELOPE_ARENA_BACKED, 0x0000_0004);
        assert_eq!(HEW_MSG_ENVELOPE_FORKED, 0x0000_0008);
        assert_eq!(HEW_MSG_ENVELOPE_CAPABILITY_TRANSFER, 0x0000_0010);
        assert_eq!(HEW_MSG_ENVELOPE_RESERVED_GAMMA_A, 0x0000_0020);
        assert_eq!(HEW_MSG_ENVELOPE_RESERVED_GAMMA_B, 0x0000_0040);
        assert_eq!(HEW_MSG_ENVELOPE_RESERVED_DELTA_A, 0x0000_0080);
        assert_eq!(HEW_MSG_ENVELOPE_RESERVED_DELTA_B, 0x0000_0100);
        assert_eq!(HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK, 0xffff_fe00);
    }
}
