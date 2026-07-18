//! Target-independent mailbox message-header bit logic.
//!
//! These bits describe in-process COW message payloads and must remain identical
//! across native and WASM mailboxes. They are unrelated to the cross-node CBOR
//! wire envelope.

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
