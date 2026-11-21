//! Registry-gossip flush, retry and broadcast.

use std::ffi::c_int;
use std::sync::atomic::Ordering;

use crate::envelope::{
    validate_registry_gossip_payload, NodeSessionIdentity, RegistryGossipPayload,
    REGISTRY_GOSSIP_OP_ADD, REGISTRY_GOSSIP_OP_REMOVE,
};
use crate::node_identity::Location;
use crate::set_last_error;

use super::control::encode_registry_gossip_control;
use super::handshake::supports_gossip;
use super::peer::{active_gossip_peers, authenticated_peer_node_id_for_conn};
use super::send::claim_send;
use super::{
    HewConnMgr, PendingRegistryFlush, MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS,
    MAX_REGISTRY_GOSSIP_FLUSH_EVENTS,
};

fn registry_gossip_intent(
    name: String,
    location: Location,
    is_add: bool,
) -> Option<RegistryGossipPayload> {
    let intent = RegistryGossipPayload {
        op: if is_add {
            REGISTRY_GOSSIP_OP_ADD
        } else {
            REGISTRY_GOSSIP_OP_REMOVE
        },
        name,
        location,
    };
    if let Err(err) = validate_registry_gossip_payload(&intent) {
        set_last_error(format!(
            "registry gossip control payload encode failure: {err}"
        ));
        return None;
    }
    Some(intent)
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
    // owner of a published claim receives cluster state. Standalone native
    // managers retain their queued events when this gate denies the flush.
    if authenticated_peer_node_id_for_conn(mgr, conn_id) == 0 {
        return;
    }

    // A publication snapshots the canonical local map, including names whose
    // transition gossip has already aged out. It never forwards another origin.
    // SAFETY: the node pins its registry through every manager owner.
    let intents = if let (Some(registry), Some(identity), Some(session)) = (
        unsafe { mgr.registry.as_ref() },
        mgr.local_identity,
        mgr.local_session_incarnation,
    ) {
        registry
            .local_names(NodeSessionIdentity {
                node_id: identity,
                session_incarnation: session,
            })
            .into_iter()
            .filter_map(|(name, location)| registry_gossip_intent(name, location, true))
            .collect()
    } else {
        // Standalone native managers have only their explicit cluster event queue.
        // SAFETY: the manager pins its cluster.
        unsafe { (&*mgr.cluster).take_registry_gossip(MAX_REGISTRY_GOSSIP_FLUSH_EVENTS) }
            .into_iter()
            .filter_map(|event| registry_gossip_intent(event.name, event.location, event.is_add))
            .collect()
    };
    send_registry_flush(mgr, conn_id, publication_token, intents, 0);
}

/// Send registry-gossip intents in order. Park the failed intent and unsent
/// remainder for retry on the connection's next inbound frame. The send gate
/// reads current local state so a delayed snapshot cannot resurrect an old name.
fn send_registry_flush(
    mgr: &HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    mut intents: Vec<RegistryGossipPayload>,
    attempts: u32,
) {
    let mut sent = 0;
    while let Some(intent) = intents.get(sent) {
        if send_registry_intent(mgr, conn_id, publication_token, intent) != 0 {
            set_last_error(format!(
                "registry gossip flush send failed for conn {conn_id}; \
                 parking {} frame(s) for retry",
                intents.len() - sent
            ));
            // Re-park AT FRONT: a concurrent broadcast may have appended newer
            // frames to the entry while this drain ran; the unsent remainder
            // predates them, so it must go back ahead of them (FIFO).
            park_pending_registry_flush(
                mgr,
                conn_id,
                publication_token,
                intents.split_off(sent),
                attempts,
                true,
            );
            return;
        }
        sent += 1;
    }
}

/// Park unsent intents for retry, bound to the admission's publication
/// token. Bounded at [`MAX_REGISTRY_GOSSIP_FLUSH_EVENTS`] intents per
/// connection (oldest evicted with a diagnostic).
fn park_pending_registry_flush(
    mgr: &HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    intents: Vec<RegistryGossipPayload>,
    attempts: u32,
    at_front: bool,
) {
    if intents.is_empty() {
        return;
    }
    // Keep the publication live until the insert. Removal drops the parked
    // intents after unlinking the connection, so an older snapshot cannot
    // park on a reused slot or clear the newer publication's queue.
    mgr.connections.access(|connections| {
        if !connections.iter().any(|conn| {
            conn.conn_id == conn_id
                && conn.publication_token == publication_token
                && !conn.publication_removed.load(Ordering::Acquire)
        }) {
            return;
        }
        mgr.pending_registry_flush.access(|map| {
            let entry = map.entry(conn_id).or_insert_with(|| PendingRegistryFlush {
                token: publication_token,
                intents: Vec::new(),
                attempts: 0,
            });
            if entry.token != publication_token {
                // A previous admission's leftovers never survive into a successor
                // on a reused conn_id.
                entry.token = publication_token;
                entry.intents.clear();
                entry.attempts = 0;
            }
            entry.attempts = entry.attempts.max(attempts);
            if at_front {
                entry.intents.splice(0..0, intents);
            } else {
                entry.intents.extend(intents);
            }
            if entry.intents.len() > MAX_REGISTRY_GOSSIP_FLUSH_EVENTS {
                let excess = entry.intents.len() - MAX_REGISTRY_GOSSIP_FLUSH_EVENTS;
                entry.intents.drain(..excess);
                set_last_error(format!(
                    "registry gossip retry buffer overflow for conn {conn_id}: \
                     evicted {excess} oldest frame(s)"
                ));
            }
            mgr.pending_registry_flush_count
                .store(map.len(), Ordering::Release);
        });
    });
}

fn send_registry_intent(
    manager: &HewConnMgr,
    conn_id: c_int,
    publication: u64,
    intent: &RegistryGossipPayload,
) -> c_int {
    let Some(claim) = claim_send(manager, conn_id, Some(publication)) else {
        return -1;
    };
    claim.send_gossip(|| {
        // SAFETY: the claimed connection pins the manager and its borrowed node map.
        let Some(registry) = (unsafe { manager.registry.as_ref() }) else {
            return encode_registry_gossip_control(
                &intent.name,
                intent.location,
                intent.op == REGISTRY_GOSSIP_OP_ADD,
            );
        };
        let session = NodeSessionIdentity {
            node_id: manager.local_identity?,
            session_incarnation: manager.local_session_incarnation?,
        };
        if intent.location.node() != session.node_id
            || intent.location.incarnation() != session.session_incarnation
        {
            return None;
        }
        let latest = registry.current_local_name(session, &intent.name);
        encode_registry_gossip_control(
            &intent.name,
            latest.unwrap_or(intent.location),
            latest.is_some(),
        )
    })
}

/// Retry a parked registry-gossip flush on inbound traffic from `conn_id`.
/// Consumes the parked entry only for the SAME admission (publication token
/// match); re-checks the outbound authority gate first, dropping the intents
/// fail-closed if the connection lost its claim (superseded / removed — the
/// successor's own establish flush carries current state).
pub(super) fn retry_pending_registry_flush(mgr: &HewConnMgr, conn_id: c_int, claim_token: u64) {
    if mgr.pending_registry_flush_count.load(Ordering::Acquire) == 0 {
        return;
    }
    let Some((intents, attempts)) = mgr.pending_registry_flush.access(|map| {
        let matches = map
            .get(&conn_id)
            .is_some_and(|pending| pending.token == claim_token);
        if !matches {
            return None;
        }
        let pending = map.remove(&conn_id);
        mgr.pending_registry_flush_count
            .store(map.len(), Ordering::Release);
        pending.map(|p| (p.intents, p.attempts + 1))
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
            intents.len()
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
    send_registry_flush(mgr, conn_id, claim_token, intents, attempts);
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
    let Some(intent) = registry_gossip_intent(name.to_owned(), location, is_add) else {
        return 0;
    };

    let mut success_count: c_int = 0;
    for peer in active_gossip_peers(mgr_ref) {
        let conn_id = peer.conn_id;
        let token = peer.publication_token;
        // Park behind any undelivered flush for this publication to preserve
        // the order of name updates across send failures.
        let has_parked = mgr_ref.pending_registry_flush_count.load(Ordering::Acquire) != 0
            && mgr_ref.pending_registry_flush.access(|map| {
                map.get(&conn_id)
                    .is_some_and(|pending| pending.token == token)
            });
        if has_parked {
            park_pending_registry_flush(mgr_ref, conn_id, token, vec![intent.clone()], 0, false);
            set_last_error(format!(
                "registry gossip broadcast parked behind an undelivered flush for conn {conn_id}"
            ));
            continue;
        }
        if send_registry_intent(mgr_ref, conn_id, token, &intent) == 0 {
            success_count += 1;
        } else {
            set_last_error(format!(
                "registry gossip broadcast send failed for conn {conn_id}; parked for retry"
            ));
            park_pending_registry_flush(mgr_ref, conn_id, token, vec![intent.clone()], 0, false);
        }
    }
    success_count
}

/// Re-announce every local name over a connection whose peer buried this
/// node. The peer dropped them, and readmitting this session repeats no
/// admission flush. The names queue behind any parked flush.
pub(super) fn reannounce_local_names(mgr: &HewConnMgr, conn_id: c_int, publication_token: u64) {
    // SAFETY: the node pins its registry through every manager owner.
    let (Some(registry), Some(node_id), Some(session_incarnation)) = (
        unsafe { mgr.registry.as_ref() },
        mgr.local_identity,
        mgr.local_session_incarnation,
    ) else {
        return;
    };
    let intents = registry
        .local_names(NodeSessionIdentity {
            node_id,
            session_incarnation,
        })
        .into_iter()
        .filter_map(|(name, location)| registry_gossip_intent(name, location, true))
        .collect();
    let parked = mgr.pending_registry_flush_count.load(Ordering::Acquire) != 0
        && mgr.pending_registry_flush.access(|map| {
            map.get(&conn_id)
                .is_some_and(|pending| pending.token == publication_token)
        });
    if parked {
        park_pending_registry_flush(mgr, conn_id, publication_token, intents, 0, false);
    } else {
        send_registry_flush(mgr, conn_id, publication_token, intents, 0);
    }
}
