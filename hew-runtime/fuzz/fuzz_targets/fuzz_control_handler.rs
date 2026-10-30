//! Control frames delivered to a connected node's handler.
//!
//! The input is a sequence of `(peer, frame)` deliveries to an in-process node
//! with two authenticated peers and one delivery-only peer. After every
//! delivery the node must hold a bounded member table, no incarnation may move
//! backwards, and the frame may provoke only a bounded number of sends.
#![no_main]

mod support;

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
        let local_after = node.local_incarnation();
        assert!(
            local_after >= local_before,
            "local incarnation fell from {local_before} to {local_after}"
        );
        before = after;
        local_before = local_after;
    }
});

fn check_incarnation(old: &Member, new: &Member) {
    if let (Some(was), Some(now)) = (old.incarnation, new.incarnation) {
        assert!(
            now >= was,
            "slot {} incarnation fell from {was} to {now} ({old:?} -> {new:?})",
            old.slot
        );
    }
}
