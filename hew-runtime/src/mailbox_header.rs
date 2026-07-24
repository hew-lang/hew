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

/// The closed set of runtime lifecycle signals.
///
/// These are **not** application messages and share no namespace with them.
/// A `HewSysMsg` travels only on a mailbox's system queue and is delivered only
/// through [`crate::internal::types::HewSysDispatchFn`], a dispatch entry point
/// distinct from the user trampoline — so an application `msg_type`, which is
/// unrestricted over the full `i32` range in the public C ABI
/// ([`crate::actor::hew_actor_send`]) and is a `SipHash` tag in generated code,
/// cannot express a lifecycle signal at all. The provenance is the TYPE, not a
/// reserved value and not a boolean conjunct a future edit could drop.
///
/// The discriminants are this enum's own private namespace. They are carried
/// across the C ABI as the `i32` argument of `HewSysDispatchFn`, always
/// produced by [`HewSysMsg::as_i32`] and always validated back through
/// [`HewSysMsg::from_raw`] at the mailbox boundary; there is no fallthrough
/// decode, so an unrecognised raw value is refused rather than dispatched.
///
/// Lives here — the target-independent header module compiled on BOTH native
/// and wasm32 — so the native (`mailbox.rs`) and WASM (`mailbox_wasm.rs`) paths
/// share ONE authority and cannot drift (D159); the native `mailbox` module is
/// `#[cfg(not(wasm32))]` and unavailable on wasm32.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HewSysMsg {
    /// Self-stop request enqueued by `mailbox_send_stop_sys_once` when
    /// `hew_actor_stop` targets a Running actor. Observed structurally by the
    /// scheduler; never reaches any dispatch entry point.
    Shutdown = 0,
    /// A supervised child reached a clean terminal state.
    ChildStopped = 1,
    /// A supervised child crashed. Payload: `ChildEvent`.
    ChildCrashed = 2,
    /// Supervisor shutdown command.
    SupervisorStop = 3,
    /// Link propagation: a linked actor died. Payload: `ExitMessage`.
    Exit = 4,
    /// Monitor notification: a monitored actor died. Payload: `HewDownMessage`.
    Down = 5,
    /// Delayed restart handoff, timer thread → supervisor mailbox.
    DelayedRestart = 6,
    /// A CHILD SUPERVISOR exhausted its restart budget and escalated.
    ///
    /// Its own variant rather than a retagged [`Self::ChildCrashed`]: the
    /// escalation carries a supervisor index, not an actor index, and the
    /// former encoding signalled that by setting `ChildEvent.child_index` to
    /// `-1` — which silently changed the meaning of the sibling `child_id`
    /// field. Payload: `ChildSupervisorEscalation`.
    ChildSupervisorEscalated = 7,
}

impl HewSysMsg {
    /// Decode a raw discriminant. `None` for anything outside the closed set —
    /// fail-closed, no catch-all arm.
    #[must_use]
    pub const fn from_raw(raw: i32) -> Option<Self> {
        match raw {
            0 => Some(Self::Shutdown),
            1 => Some(Self::ChildStopped),
            2 => Some(Self::ChildCrashed),
            3 => Some(Self::SupervisorStop),
            4 => Some(Self::Exit),
            5 => Some(Self::Down),
            6 => Some(Self::DelayedRestart),
            7 => Some(Self::ChildSupervisorEscalated),
            _ => None,
        }
    }

    /// The raw discriminant carried across the `HewSysDispatchFn` boundary.
    #[must_use]
    pub const fn as_i32(self) -> i32 {
        self as i32
    }
}

/// Which queue a dequeued mailbox node arrived on, carrying the system
/// message's kind when it is one.
///
/// The scheduler decides by an exhaustive `match` on this value, so omitting
/// the provenance term is a compile error rather than a silently-dropped
/// conjunct. There is no value to compare against: a `Sys(Shutdown)` node is
/// the shutdown signal because of what it IS, not because its `msg_type`
/// happens to equal a reserved integer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Origin {
    /// An application message from the user queue. Its `msg_type` is
    /// unrestricted over the full `i32` range.
    User,
    /// A runtime lifecycle signal from the system queue.
    Sys(HewSysMsg),
}

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

    #[test]
    fn sys_msg_decode_round_trips_and_refuses_unknown() {
        for kind in [
            HewSysMsg::Shutdown,
            HewSysMsg::ChildStopped,
            HewSysMsg::ChildCrashed,
            HewSysMsg::SupervisorStop,
            HewSysMsg::Exit,
            HewSysMsg::Down,
            HewSysMsg::DelayedRestart,
            HewSysMsg::ChildSupervisorEscalated,
        ] {
            assert_eq!(HewSysMsg::from_raw(kind.as_i32()), Some(kind));
        }
        // Fail-closed decode: no fallthrough arm. The former reserved block
        // (100..=105), the former shutdown sentinel (-1), and the extremes of
        // the application tag range all decode to None.
        for raw in [-1, 8, 99, 100, 101, 102, 103, 104, 105, i32::MIN, i32::MAX] {
            assert_eq!(HewSysMsg::from_raw(raw), None, "raw {raw} must not decode");
        }
    }
}
