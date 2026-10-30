//! Control frames delivered to a connected node's handler.
//!
//! The input is a sequence of `(peer, frame)` deliveries to an in-process node
//! with two authenticated peers and one delivery-only peer. After every
//! delivery the node must hold a bounded member table, no incarnation may move
//! backwards within a session, and the frame may provoke only a bounded
//! number of sends.
//! An ACK forwarded for a probed member, with no gossip of its own, vouches
//! for liveness only: it never changes that member's session or incarnation.
#![no_main]

mod support;

use hew_runtime::cluster::SWIM_MSG_ACK;
use hew_runtime::envelope::{decode_swim_payload, decode_wire_frame, WireFrame, CTRL_SWIM};
use hew_runtime::fuzzing::{split_deliveries, ControlHarness, Member, HARNESS_PEERS};
use libfuzzer_sys::fuzz_target;

#[global_allocator]
static ALLOC: support::Counting = support::Counting;

/// Replies one frame may provoke: an ACK or a PING_REQ fan-out to every
/// peer, plus a registry reannouncement to every peer after a refutation.
const MAX_SENDS_PER_FRAME: usize = 2 * HARNESS_PEERS + 2;

fuzz_target!(|data: &[u8]| {
    let mut node = ControlHarness::new();
    let members_at_start = node.member_count();
    let mut before = node.members();
    let mut local_before = node.local_incarnation();
    for (peer, frame) in split_deliveries(data) {
        let sends = support::bounded("control handler", frame.len(), || node.deliver(peer, frame));
        assert!(
            sends <= MAX_SENDS_PER_FRAME,
            "one frame provoked {sends} sends"
        );
        assert!(
            node.member_count() <= members_at_start,
            "member table grew from {members_at_start} to {}",
            node.member_count()
        );
        let after = node.members();
        for (old, new) in before.iter().zip(&after) {
            check_incarnation(old, new);
        }
        if let Some(slot) = forwarded_ack_target(frame) {
            let old = before.iter().find(|member| member.slot == slot);
            let new = after.iter().find(|member| member.slot == slot);
            assert_eq!(
                old.map(|m| (m.session, m.incarnation)),
                new.map(|m| (m.session, m.incarnation)),
                "a forwarded ACK changed slot {slot}"
            );
        }
        let local_after = node.local_incarnation();
        assert!(
            local_after >= local_before,
            "local incarnation fell from {local_before} to {local_after}"
        );
        before = after;
        local_before = local_after;
    }
});

/// A member's `(session, incarnation)` never moves backwards: a newer
/// session starts a fresh incarnation domain, so only the pair is ordered.
fn check_incarnation(old: &Member, new: &Member) {
    let was = (old.session, old.incarnation);
    let now = (new.session, new.incarnation);
    if old.session.is_some() && old.incarnation.is_some() {
        assert!(
            now >= was,
            "slot {} (session, incarnation) fell from {was:?} to {now:?} ({old:?} -> {new:?})",
            old.slot
        );
    }
}

/// The probed member a gossip-free forwarded ACK vouches for.
fn forwarded_ack_target(frame: &[u8]) -> Option<u16> {
    let Ok(WireFrame::Control(control)) = decode_wire_frame(frame) else {
        return None;
    };
    if control.ctrl_kind != CTRL_SWIM {
        return None;
    }
    let payload = decode_swim_payload(&control.payload).ok()?;
    let target = payload.target?;
    if payload.msg_type != SWIM_MSG_ACK || !payload.gossip.is_empty() || target == payload.from {
        return None;
    }
    ControlHarness::slot_of(target.node_id)
}
