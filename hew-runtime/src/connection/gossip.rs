//! Registry-gossip flush, retry and broadcast.

use std::collections::HashMap;
use std::ffi::c_int;
use std::sync::atomic::Ordering;

use crate::node_identity::Location;
use crate::set_last_error;

use super::control::encode_registry_gossip_control;
use super::handshake::supports_gossip;
use super::peer::authenticated_peer_node_id_for_conn;
use super::send::send_preencoded_on_manager;
use super::{
    HewConnMgr, PendingRegistryFlush, CONN_STATE_ACTIVE, MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS,
    MAX_REGISTRY_GOSSIP_FLUSH_EVENTS,
};

pub(super) fn active_gossip_connection_ids(mgr: &HewConnMgr) -> Vec<c_int> {
    // Candidate connections: ACTIVE + gossip-capable. Snapshot conn_ids first so
    // the authenticated-owner check below (which locks `claims`) never runs
    // nested inside the `connections` read guard.
    let candidates: Vec<c_int> = mgr.connections.access(|conns| {
        conns
            .iter()
            .filter(|c| {
                c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                    && supports_gossip(c.peer_feature_flags)
            })
            .map(|c| c.conn_id)
            .collect()
    });
    // issue #2652 (D9, outbound): an `Unverified` (delivery-only) or superseded
    // connection carries no control-plane authority in EITHER direction — it must
    // not RECEIVE registry gossip any more than it may inject it. Keep only
    // connections that are the exact authenticated owner of a published claim.
    candidates
        .into_iter()
        .filter(|&conn_id| authenticated_peer_node_id_for_conn(mgr, conn_id) != 0)
        .collect()
}

pub(super) fn flush_registry_gossip_to_connection(
    mgr: &HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    peer_feature_flags: u32,
) {
    if !supports_gossip(peer_feature_flags) || mgr.cluster.is_null() {
        return;
    }
    // issue #2652 (D9, outbound): never flush registry gossip onto an
    // `Unverified` (delivery-only) or superseded connection — it carries no
    // control-plane authority in either direction. Only the exact authenticated
    // owner of a published claim receives cluster state. This gate runs BEFORE
    // the drain, so a denial retains the events for other connections.
    if authenticated_peer_node_id_for_conn(mgr, conn_id) == 0 {
        return;
    }

    // SAFETY: cluster pointer belongs to this live connection manager.
    let events = unsafe { (&*mgr.cluster).take_registry_gossip(MAX_REGISTRY_GOSSIP_FLUSH_EVENTS) };
    let frames: Vec<Vec<u8>> = events
        .into_iter()
        .filter_map(|event| {
            encode_registry_gossip_control(&event.name, event.location, event.is_add)
        })
        .collect();
    send_registry_flush_frames(mgr, conn_id, publication_token, frames, 0);
}

/// Send encoded registry-gossip flush frames in order. On a failed send, park
/// the unsent remainder (including the failed frame) for retry on the
/// connection's next inbound frame — the flush is one-shot at the cluster
/// level (drained events age out of the queue), so dropping here would leave
/// the peer permanently without those names.
fn send_registry_flush_frames(
    mgr: &HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    mut frames: Vec<Vec<u8>>,
    attempts: u32,
) {
    let mut sent = 0;
    while let Some(bytes) = frames.get(sent) {
        // SAFETY: mgr is live and `bytes` is a complete encoded control frame.
        if unsafe { send_preencoded_on_manager(mgr, conn_id, None, bytes.as_ptr(), bytes.len()) }
            != 0
        {
            set_last_error(format!(
                "registry gossip flush send failed for conn {conn_id}; \
                 parking {} frame(s) for retry",
                frames.len() - sent
            ));
            // Re-park AT FRONT: a concurrent broadcast may have appended newer
            // frames to the entry while this drain ran; the unsent remainder
            // predates them, so it must go back ahead of them (FIFO).
            park_pending_registry_flush(
                mgr,
                conn_id,
                publication_token,
                frames.split_off(sent),
                attempts,
                true,
            );
            return;
        }
        sent += 1;
    }
}

/// Park unsent flush frames for retry, bound to the admission's publication
/// token. Bounded at [`MAX_REGISTRY_GOSSIP_FLUSH_EVENTS`] frames per
/// connection (oldest evicted with a diagnostic).
fn park_pending_registry_flush(
    mgr: &HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    frames: Vec<Vec<u8>>,
    attempts: u32,
    at_front: bool,
) {
    if frames.is_empty() {
        return;
    }
    mgr.pending_registry_flush.access(|map| {
        let entry = map.entry(conn_id).or_insert_with(|| PendingRegistryFlush {
            token: publication_token,
            frames: Vec::new(),
            attempts: 0,
        });
        if entry.token != publication_token {
            // A previous admission's leftovers never survive into a successor
            // on a reused conn_id.
            entry.token = publication_token;
            entry.frames.clear();
            entry.attempts = 0;
        }
        entry.attempts = entry.attempts.max(attempts);
        if at_front {
            entry.frames.splice(0..0, frames);
        } else {
            entry.frames.extend(frames);
        }
        if entry.frames.len() > MAX_REGISTRY_GOSSIP_FLUSH_EVENTS {
            let excess = entry.frames.len() - MAX_REGISTRY_GOSSIP_FLUSH_EVENTS;
            entry.frames.drain(..excess);
            set_last_error(format!(
                "registry gossip retry buffer overflow for conn {conn_id}: \
                 evicted {excess} oldest frame(s)"
            ));
        }
        mgr.pending_registry_flush_count
            .store(map.len(), Ordering::Release);
    });
}

/// Retry a parked registry-gossip flush on inbound traffic from `conn_id`.
/// Consumes the parked entry only for the SAME admission (publication token
/// match); re-checks the outbound authority gate first, dropping the frames
/// fail-closed if the connection lost its claim (superseded / removed — the
/// successor's own establish flush carries current state).
pub(super) fn retry_pending_registry_flush(mgr: &HewConnMgr, conn_id: c_int, claim_token: u64) {
    if mgr.pending_registry_flush_count.load(Ordering::Acquire) == 0 {
        return;
    }
    let Some((frames, attempts)) = mgr.pending_registry_flush.access(|map| {
        let matches = map
            .get(&conn_id)
            .is_some_and(|pending| pending.token == claim_token);
        if !matches {
            return None;
        }
        let pending = map.remove(&conn_id);
        mgr.pending_registry_flush_count
            .store(map.len(), Ordering::Release);
        pending.map(|p| (p.frames, p.attempts + 1))
    }) else {
        return;
    };
    if attempts > MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS {
        // Fail closed, loudly: this connection's sends have failed
        // MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS drains in a row — drop the parked
        // frames instead of retrying forever on a connection that is
        // evidently broken.
        set_last_error(format!(
            "registry gossip retry budget exhausted for conn {conn_id}: dropping \
             {} parked frame(s) after {MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS} failed attempts",
            frames.len()
        ));
        return;
    }
    if authenticated_peer_node_id_for_conn(mgr, conn_id) == 0 {
        set_last_error(format!(
            "registry gossip retry dropped for conn {conn_id}: connection no longer \
             holds its published claim"
        ));
        return;
    }
    send_registry_flush_frames(mgr, conn_id, claim_token, frames, attempts);
}

/// Broadcast a registry-gossip control frame to active gossip-capable peers.
///
/// Returns the number of successful sends.
///
/// # Safety
///
/// `mgr` must be a valid connection manager pointer for the duration of the
/// call when non-null.
pub(crate) unsafe fn hew_connmgr_broadcast_registry_gossip(
    mgr: *mut HewConnMgr,
    name: &str,
    location: Location,
    is_add: bool,
) -> c_int {
    if mgr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees manager pointer validity.
    let mgr_ref = unsafe { &*mgr };
    let Some(bytes) = encode_registry_gossip_control(name, location, is_add) else {
        return 0;
    };

    let conn_ids = active_gossip_connection_ids(mgr_ref);
    // Publication tokens for FIFO parking: a broadcast frame for a connection
    // with an undelivered parked flush must queue BEHIND it — never overtake
    // it — so per-connection registry-event ordering (ADD before a newer
    // REMOVE of the same name, and vice versa) is preserved even across send
    // failures. A failed direct send parks the frame the same way.
    let tokens: HashMap<c_int, u64> = mgr_ref.connections.access(|conns| {
        conns
            .iter()
            .filter(|c| conn_ids.contains(&c.conn_id))
            .map(|c| (c.conn_id, c.publication_token))
            .collect()
    });
    let mut success_count: c_int = 0;
    for conn_id in conn_ids {
        let Some(&token) = tokens.get(&conn_id) else {
            continue;
        };
        let has_parked = mgr_ref.pending_registry_flush_count.load(Ordering::Acquire) != 0
            && mgr_ref.pending_registry_flush.access(|map| {
                map.get(&conn_id)
                    .is_some_and(|pending| pending.token == token)
            });
        if has_parked {
            park_pending_registry_flush(mgr_ref, conn_id, token, vec![bytes.clone()], 0, false);
            set_last_error(format!(
                "registry gossip broadcast parked behind an undelivered flush for conn {conn_id}"
            ));
            continue;
        }
        // SAFETY: manager is live and bytes is a complete encoded control frame.
        if unsafe {
            send_preencoded_on_manager(mgr_ref, conn_id, None, bytes.as_ptr(), bytes.len())
        } == 0
        {
            success_count += 1;
        } else {
            set_last_error(format!(
                "registry gossip broadcast send failed for conn {conn_id}; parked for retry"
            ));
            park_pending_registry_flush(mgr_ref, conn_id, token, vec![bytes.clone()], 0, false);
        }
    }
    success_count
}
