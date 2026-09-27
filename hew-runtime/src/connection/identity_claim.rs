//! `NodeId` claim reservation, publication and retirement for admitted peers.

use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use crate::node_identity::NodeId;
use crate::peer_binding::{ClaimState, LiveClaim, PeerCredential};
use crate::routing::{
    hew_routing_add_route, hew_routing_can_add_route, hew_routing_remove_route_if_conn,
};
use crate::set_last_error;
use crate::util::{CondvarExt, MutexExt};

use super::admission::remove_connection;
use super::gossip::flush_registry_gossip_to_connection;
use super::handshake::close_transport_conn;
#[cfg(test)]
use super::reconnect::next_publication_token;
use super::HewConnMgr;

/// Bounded wait for an in-flight `Reserved` claim to resolve, in ms. Mirrors the
/// handshake timeout ceiling: a stuck reservation must not wedge admission.
pub(super) const CLAIM_RESERVE_WAIT_MS: u64 = 5_000;

/// Outcome of reserving a `NodeId` claim during admission (issue #2652, D3).
pub(super) enum ClaimReservation {
    /// The reservation succeeded. Carries the superseded same-credential
    /// `Published` claim (if any) to demote after our publish.
    Reserved { superseded: Option<LiveClaim> },
    /// The reservation was rejected fail-closed (a live different-credential
    /// owner, or an in-flight reservation that did not resolve in time). Carries
    /// the diagnostic detail.
    Rejected(String),
}

/// Reserve a `NodeId` claim for a connection mid-admission (issue #2652, D3
/// step 1). Runs under the claims mutex; the condvar coordinates the
/// reserve/publish handoff.
///
/// On an existing entry for `node_id`:
/// - `Published` with a **different** credential ⇒ reject fail-closed (a live
///   authenticated owner is never taken over by a different credential).
/// - `Published` with the **same** credential ⇒ same-peer reconnect: supersede.
/// - `Reserved` (another admission in flight) ⇒ wait on the condvar until it
///   publishes/aborts, then re-evaluate; on timeout ⇒ reject.
/// - absent ⇒ insert `Reserved`; no supersede.
pub(super) fn reserve_identity_claim(
    mgr: &HewConnMgr,
    node_id: NodeId,
    route_slot: u16,
    session_incarnation: u32,
    credential: Option<&PeerCredential>,
    conn_id: c_int,
    publication_token: u64,
) -> ClaimReservation {
    if route_slot == 0 || session_incarnation == 0 {
        return ClaimReservation::Rejected(format!(
            "invalid route slot/session for NodeId {node_id} (route_slot={route_slot}, session={session_incarnation})"
        ));
    }
    let (lock, condvar) = &mgr.claims;
    let mut map = lock.lock_or_recover();
    loop {
        match map.get(&node_id) {
            Some(existing) if existing.state != ClaimState::Reserved => {
                if existing.credential.as_ref() == credential && existing.route_slot == route_slot {
                    if session_incarnation < existing.session_incarnation {
                        return ClaimReservation::Rejected(format!(
                            "lower-session replay for NodeId {node_id}: received {session_incarnation}, current {}",
                            existing.session_incarnation
                        ));
                    }
                    if session_incarnation == existing.session_incarnation
                        && !mgr.cluster.is_null()
                        && matches!(
                            // SAFETY: cluster is owned by the live manager.
                            unsafe { (&*mgr.cluster).member_state(route_slot) },
                            crate::cluster::MEMBER_DEAD | crate::cluster::MEMBER_LEFT
                        )
                    {
                        return ClaimReservation::Rejected(format!(
                            "equal session {session_incarnation} cannot revive buried NodeId {node_id}"
                        ));
                    }
                    // Same-credential reconnect or a higher durable session.
                    let superseded = Some(existing.clone());
                    map.insert(
                        node_id,
                        LiveClaim {
                            credential: credential.cloned(),
                            route_slot,
                            session_incarnation,
                            conn_id,
                            publication_token,
                            state: ClaimState::Reserved,
                        },
                    );
                    return ClaimReservation::Reserved { superseded };
                }
                // A NodeId is key-derived, so another credential or route slot is
                // either a collision or a wrong local pin.
                return ClaimReservation::Rejected(format!(
                    "NodeId {node_id} is already bound to another credential or route slot (conn {conn_id})"
                ));
            }
            Some(_reserved) => {
                // Another admission is mid-flight for this NodeId. Wait for it to
                // publish or abort, then re-evaluate. Bounded to avoid wedging.
                let (guard, timeout) = condvar.wait_timeout_or_recover(
                    map,
                    std::time::Duration::from_millis(CLAIM_RESERVE_WAIT_MS),
                );
                map = guard;
                if timeout.timed_out() {
                    return ClaimReservation::Rejected(format!(
                        "timed out waiting on in-flight reservation for node id {node_id} (conn {conn_id})"
                    ));
                }
                // Loop and re-evaluate the (possibly changed) entry.
            }
            None => {
                map.insert(
                    node_id,
                    LiveClaim {
                        credential: credential.cloned(),
                        route_slot,
                        session_incarnation,
                        conn_id,
                        publication_token,
                        state: ClaimState::Reserved,
                    },
                );
                return ClaimReservation::Reserved { superseded: None };
            }
        }
    }
}

/// Abort a reservation on install failure (issue #2652, D3 step 2). If
/// `claims[node_id]` is still our exact `Reserved` claim, restore the superseded
/// claim (if any) or remove it, then wake any waiter.
pub(super) fn abort_identity_claim(
    mgr: &HewConnMgr,
    node_id: NodeId,
    conn_id: c_int,
    publication_token: u64,
    superseded: Option<LiveClaim>,
) {
    let (lock, condvar) = &mgr.claims;
    let mut map = lock.lock_or_recover();
    if let Some(current) = map.get(&node_id) {
        if current.state == ClaimState::Reserved
            && current.conn_id == conn_id
            && current.publication_token == publication_token
        {
            match superseded {
                Some(prev) => {
                    map.insert(node_id, prev);
                }
                None => {
                    map.remove(&node_id);
                }
            }
        }
    }
    drop(map);
    condvar.notify_all();
}

/// Transition our reservation `Reserved → Published` (issue #2652, D3 step 3).
/// Returns `true` iff `claims[node_id]` is still our exact reservation (so the
/// caller may write route + cluster token under the same lock). A superseding
/// claim arriving meanwhile ⇒ `false` (abort publish, write nothing).
pub(super) fn publish_identity_claim(
    mgr: &HewConnMgr,
    node_id: NodeId,
    conn_id: c_int,
    publication_token: u64,
) -> bool {
    let (lock, condvar) = &mgr.claims;
    let mut map = lock.lock_or_recover();
    let still_ours = map.get(&node_id).is_some_and(|c| {
        c.state == ClaimState::Reserved
            && c.conn_id == conn_id
            && c.publication_token == publication_token
    });
    if still_ours {
        if let Some(claim) = map.get_mut(&node_id) {
            claim.state = ClaimState::Published;
        }
    }
    drop(map);
    condvar.notify_all();
    still_ours
}

/// Retire our published claim (issue #2652, D3 retire). Removes `claims[node_id]`
/// iff it exactly matches this connection's `(conn_id, publication_token)`,
/// then wakes any waiter. Returns `true` iff this connection was the exact owner
/// (drives the real route removal / `MonitorLost` fan-out). A non-matching /
/// superseded connection removes nothing and reports "not owner".
pub(super) fn retire_identity_claim(
    mgr: &HewConnMgr,
    node_id: NodeId,
    conn_id: c_int,
    publication_token: u64,
) -> bool {
    let (lock, condvar) = &mgr.claims;
    let mut map = lock.lock_or_recover();
    let is_owner = map
        .get(&node_id)
        .is_some_and(|c| c.conn_id == conn_id && c.publication_token == publication_token);
    if is_owner {
        if let Some(claim) = map.get_mut(&node_id) {
            claim.state = ClaimState::Retired;
        }
    }
    drop(map);
    condvar.notify_all();
    is_owner
}

/// Test-only: reserve an `Unverified` (credential-free) claim, mirroring what
/// admission does before `publish_connection_established`. Returns the
/// superseded claim (if any) to thread into publish. Panics on rejection — the
/// unit tests below only reserve fresh or same-`NodeId` reconnect claims.
#[cfg(test)]
pub(super) fn reserve_unverified_identity_claim(
    mgr: &HewConnMgr,
    node_id: NodeId,
    route_slot: u16,
    session_incarnation: u32,
    conn_id: c_int,
    token: u64,
) -> Option<LiveClaim> {
    match reserve_identity_claim(
        mgr,
        node_id,
        route_slot,
        session_incarnation,
        None,
        conn_id,
        token,
    ) {
        ClaimReservation::Reserved { superseded } => superseded,
        ClaimReservation::Rejected(detail) => {
            panic!("unexpected claim rejection in test (node {node_id}, conn {conn_id}): {detail}")
        }
    }
}

#[cfg(test)]
pub(super) fn test_node_identity(route_slot: u16) -> NodeId {
    let mut bytes = [0_u8; 16];
    bytes[14..].copy_from_slice(&route_slot.to_be_bytes());
    NodeId::from_bytes(bytes)
}

#[cfg(test)]
pub(super) fn reserve_claim(
    mgr: &HewConnMgr,
    route_slot: u16,
    credential: Option<&PeerCredential>,
    conn_id: c_int,
    publication_token: u64,
) -> ClaimReservation {
    reserve_identity_claim(
        mgr,
        test_node_identity(route_slot),
        route_slot,
        1,
        credential,
        conn_id,
        publication_token,
    )
}

#[cfg(test)]
pub(super) fn abort_claim(
    mgr: &HewConnMgr,
    route_slot: u16,
    conn_id: c_int,
    publication_token: u64,
    superseded: Option<LiveClaim>,
) {
    abort_identity_claim(
        mgr,
        test_node_identity(route_slot),
        conn_id,
        publication_token,
        superseded,
    );
}

#[cfg(test)]
pub(super) fn publish_claim(
    mgr: &HewConnMgr,
    route_slot: u16,
    conn_id: c_int,
    publication_token: u64,
) -> bool {
    let identity = test_node_identity(route_slot);
    mgr.connections.access(|connections| {
        if let Some(connection) = connections
            .iter_mut()
            .find(|connection| connection.conn_id == conn_id)
        {
            connection.peer_identity = Some(identity);
            connection.peer_session_incarnation = 1;
            connection.publication_token = publication_token;
        }
    });
    publish_identity_claim(mgr, identity, conn_id, publication_token)
}

#[cfg(test)]
pub(super) fn retire_claim(
    mgr: &HewConnMgr,
    route_slot: u16,
    conn_id: c_int,
    publication_token: u64,
) -> bool {
    retire_identity_claim(
        mgr,
        test_node_identity(route_slot),
        conn_id,
        publication_token,
    )
}

#[cfg(test)]
pub(super) fn test_reserve_unverified(
    mgr: &HewConnMgr,
    route_slot: u16,
    conn_id: c_int,
    publication_token: u64,
) -> Option<LiveClaim> {
    reserve_unverified_identity_claim(
        mgr,
        test_node_identity(route_slot),
        route_slot,
        1,
        conn_id,
        publication_token,
    )
}

/// Test helper: reserve **and** publish a claim binding `node_id` to `conn_id`,
/// modelling a fully-admitted authenticated peer. Unit tests that construct a
/// [`ConnectionActor`](super::ConnectionActor) by hand (bypassing `hew_connmgr_add`) must seed this so
/// the issue #2652 exact-owner gate ([`authenticated_peer_node_id_for_conn`](super::peer::authenticated_peer_node_id_for_conn))
/// recognises the connection as the current published owner of its `NodeId`.
#[cfg(test)]
pub(super) fn test_publish_claim(mgr: &HewConnMgr, route_slot: u16, conn_id: c_int) -> u64 {
    let node_id = test_node_identity(route_slot);
    let token = next_publication_token(mgr).expect("the publication token space is not exhausted");
    reserve_unverified_identity_claim(mgr, node_id, route_slot, 1, conn_id, token);
    let (lock, condvar) = &mgr.claims;
    let mut guard = lock
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if let Some(claim) = guard.get_mut(&node_id) {
        claim.state = ClaimState::Published;
    }
    drop(guard);
    condvar.notify_all();
    // Stamp the connection entry with the same publication token so the
    // generation-bound waiting gate recognises the hand-built actor as this
    // admission (production sets `actor.publication_token` before install).
    mgr.connections.access(|conns| {
        if let Some(conn) = conns.iter_mut().find(|c| c.conn_id == conn_id) {
            conn.peer_identity = Some(node_id);
            conn.peer_session_incarnation = 1;
            conn.publication_token = token;
        }
    });
    token
}

/// Fail closed on a publication that cannot complete after the connection has
/// already been installed and exposed to the cluster.
///
/// Order matters, and both steps are synchronous:
///
/// 1. Retire the cluster token/member FIRST, before any close is exposed.
///    Dropping the current token demotes the member and cancels the remaining
///    steps of any claimed guarded delivery. A step that already passed its
///    own gate linearizes before this retirement and is not a callback-return
///    fence; immutable route refusals therefore use the pre-publication path
///    below instead. The call is token-guarded, so a superseding admission's
///    publication is untouched and the repeat inside teardown is a no-op.
/// 2. Hand the connection to the ordinary guarded teardown. That retires the
///    claim, removes the route, sets `reader_stop` and ACQUIRES the transport
///    close before closing, unlinks the actor and joins the reader — so the
///    transport is closed EXACTLY ONCE even when a manager free or a second
///    removal races this refusal, since only the caller that wins the claim
///    closes and the losers wait for that close instead of repeating it. The
///    woken reader takes the expected-stop path instead of treating the refusal
///    as an unexpected drop, which is what would otherwise schedule a reconnect
///    and retry the rejected peer indefinitely.
pub(super) fn refuse_established_publication(
    mgr: &HewConnMgr,
    peer_route_slot: u16,
    conn_id: c_int,
    publication_token: u64,
) {
    if !mgr.cluster.is_null() {
        // SAFETY: cluster is owned by the live manager; pointer validity is
        // re-checked by the callee.
        let _ = unsafe {
            crate::cluster::hew_cluster_notify_connection_lost_if_current(
                mgr.cluster,
                peer_route_slot,
                publication_token,
            )
        };
    }
    let _ = remove_connection(mgr, conn_id, Some(publication_token));
}

/// Fail closed before cluster publication when the immutable route shape can
/// never be represented by this manager's routing table.
///
/// No cluster token was installed for this admission, so there is no cluster
/// publication to retire. In particular, a bare route-slot demotion would be
/// unsafe: the cluster entry at that receiver-local alias may belong to an
/// existing/current member unrelated to this refused identity. The ordinary
/// removal below uses token-bound retirement, which is a no-op for this
/// never-published admission while still retiring its claim and connection.
fn refuse_unpublishable_route(mgr: &HewConnMgr, conn_id: c_int, publication_token: u64) {
    let _ = remove_connection(mgr, conn_id, Some(publication_token));
}

#[expect(
    clippy::too_many_arguments,
    reason = "publication threads route/cluster/gossip metadata plus the issue #2652 superseded claim; bundling them would obscure the call site"
)]
pub(super) fn publish_identity_connection_established(
    mgr: &HewConnMgr,
    peer_identity: NodeId,
    peer_route_slot: u16,
    peer_session_incarnation: u32,
    conn_id: c_int,
    peer_feature_flags: u32,
    publication_token: u64,
    publication_sync: &Arc<Mutex<()>>,
    publication_removed: &Arc<AtomicBool>,
    superseded: Option<LiveClaim>,
) {
    if publication_removed.load(Ordering::Acquire) {
        return;
    }

    // Transition our reservation Reserved → Published (issue #2652, D3 step 3).
    // A superseding admission arriving in the reserve window ⇒ we are no longer
    // the map owner ⇒ abort publish (write no route / cluster token / gossip).
    if !publish_identity_claim(mgr, peer_identity, conn_id, publication_token) {
        return;
    }

    // Reserved slots and zero sessions are immutable properties of this
    // admission and routing table. Reject them BEFORE exposing an ALIVE
    // cluster transition. Once a guarded delivery has passed one of its
    // per-step gates, token retirement is intentionally not a callback-return
    // fence; publishing first would therefore permit an ALIVE callback after
    // this refusal returned even though no route could ever be installed.
    if !mgr.routing_table.is_null()
        && !unsafe {
            // SAFETY: the routing table is owned by the live manager.
            hew_routing_can_add_route(mgr.routing_table, peer_route_slot, peer_session_incarnation)
        }
    {
        refuse_unpublishable_route(mgr, conn_id, publication_token);
        // Reported last so the teardown's own diagnostics cannot mask the
        // reason the admission was refused.
        set_last_error(format!(
            "connection publication refused for peer {peer_identity} on conn {conn_id}: \
             route slot {peer_route_slot} is reserved (slot 0 or this node's own local \
             route slot), so no route could be registered"
        ));
        return;
    }

    if let Some(previous) = superseded.as_ref().filter(|previous| {
        previous.state == ClaimState::Published
            && previous.session_incarnation != peer_session_incarnation
    }) {
        crate::hew_node::fan_out_monitor_lost_for_session(
            peer_identity,
            previous.session_incarnation,
        );
    }

    let published = if mgr.cluster.is_null() {
        true
    } else {
        // SAFETY: cluster is owned by the live manager.
        // SAFETY: pointer validity is checked by the callee.
        let result = unsafe {
            crate::cluster::hew_cluster_notify_connection_established_for_token_if_not_removed(
                mgr.cluster,
                peer_route_slot,
                peer_session_incarnation,
                publication_token,
                publication_sync,
                publication_removed,
            )
        };
        if result < 0 {
            // Fail closed, the same way the reserved-slot refusal below does.
            // The cluster rejected this admission outright — a session
            // regression on the peer's route slot, or a member already buried
            // as DEAD/LEFT — so it will never be published. Retiring only the
            // claim would leave the connection installed and its transport
            // open: an unroutable, unowned peer that still reads frames and
            // that manager free would close later. Tear it down now.
            refuse_established_publication(mgr, peer_route_slot, conn_id, publication_token);
            set_last_error(format!(
                "connection publication refused for peer {peer_identity} on conn {conn_id}: \
                 the cluster rejected session {peer_session_incarnation} on route slot \
                 {peer_route_slot}"
            ));
            return;
        }
        result == 1
    };
    if !published {
        return;
    }

    #[cfg(test)]
    if !mgr.routing_table.is_null() {
        // SAFETY: the routing table is owned by the live manager.
        unsafe { &*mgr.routing_table }.route_add_rendezvous();
    }

    if !mgr.routing_table.is_null() {
        let route_registered = {
            let _publication = publication_sync.lock_or_recover();
            if publication_removed.load(Ordering::Acquire) {
                return;
            }
            // SAFETY: pointer validity is checked by the callee.
            unsafe {
                hew_routing_add_route(
                    mgr.routing_table,
                    peer_identity,
                    peer_route_slot,
                    peer_session_incarnation,
                    conn_id,
                    publication_token,
                )
            }
        };
        // Defensive fail-closed check. The immutable route coordinates were
        // accepted before cluster publication by the same predicate used in
        // `hew_routing_add_route`, so a normal reserved-slot admission cannot
        // reach this branch. If that invariant is ever broken, retire the
        // established token and connection and emit no registry gossip for a
        // peer we failed to route.
        if !route_registered {
            refuse_established_publication(mgr, peer_route_slot, conn_id, publication_token);
            // Reported last so the teardown's own diagnostics cannot mask the
            // reason the admission was refused.
            set_last_error(format!(
                "connection publication refused for peer {peer_identity} on conn {conn_id}: \
                 route slot {peer_route_slot} is reserved (slot 0 or this node's own local \
                 route slot), so no route could be registered"
            ));
            return;
        }
    }

    flush_registry_gossip_to_connection(mgr, conn_id, publication_token, peer_feature_flags);

    // Publication is COMPLETE: clear the actor's stashed superseded claim
    // before closing the superseded connection below, so a later remove of
    // THIS connection can never restore a claim for a connection whose
    // transport is closed here. The stash is restorable state only while the
    // publication is still pending or suppressed.
    clear_superseded_claim_if_current(mgr, conn_id, publication_token);

    // Same-credential reconnect (issue #2652, D3 step 3): our claim overwrote a
    // live Published claim from the same peer credential. The superseded
    // connection already lost its authority at the map overwrite (D9 owner
    // check); close its transport so it can no longer emit SWIM/gossip/control
    // frames. Its actor tears down via the normal reader-exit path.
    if let Some(superseded) = superseded {
        if superseded.state == ClaimState::Published && superseded.conn_id != conn_id {
            close_superseded_connection_once(mgr, superseded.conn_id, superseded.publication_token);
        }
    }
}

/// Clear the restorable predecessor claim only from the exact admission whose
/// publication completed.
///
/// A transport may recycle `conn_id` after a concurrent removal closes and
/// unlinks this actor. The old publisher can still reach this cleanup point, so
/// a bare `conn_id` lookup could erase a successor admission's predecessor
/// claim. The publication token makes the lookup an exact admission identity.
pub(super) fn clear_superseded_claim_if_current(
    mgr: &HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
) {
    mgr.connections.access(|conns| {
        if let Some(conn) = conns.iter().find(|connection| {
            connection.conn_id == conn_id && connection.publication_token == publication_token
        }) {
            conn.superseded_claim.lock_or_recover().take();
        }
    });
}

/// Close a superseded connection's transport at most once, and only while that
/// exact admission is still installed.
///
/// Two things make this safe, and both are refusals to act on an assumption:
///
/// - The close is ACQUIRED, never announced. A teardown already inside this
///   connection's close owns the one-shot handle; this call loses the claim and
///   leaves it alone.
/// - An ABSENT actor is treated as ALREADY CLOSED. It is not evidence that the
///   transport was never closed — it is the opposite. This helper runs only for
///   a previously `Published` connection, and every path that removes an
///   installed actor claims and performs its close FIRST: `remove_connection`
///   closes before its `swap_remove`, and manager free closes before it drains
///   the list. So a missing actor means the close has been claimed by whoever
///   removed it, and the numeric `conn_id` may already have been reissued to a
///   fresh admission. Closing here would free a freed handle — or a live
///   stranger's. Fail closed: if we cannot establish that the transport is
///   still open, we do not close it.
///
/// Matching the publication token as well as the id is what makes "still
/// installed" mean "*this* admission is still installed", so a successor that
/// reused the id is never closed by its predecessor's supersede.
///
/// Deliberately leaves `reader_stop` alone: this is the supersede path, where
/// the point is that the superseded connection's reader wakes on an
/// *unexpected* drop and tears itself down through the ordinary reader-exit
/// route. Only the close is claimed, so the removal that reader triggers — and
/// the actor's own `Drop` — cannot free the one-shot handle a second time.
fn close_superseded_connection_once(mgr: &HewConnMgr, conn_id: c_int, publication_token: u64) {
    let claim = mgr.connections.access(|connections| {
        connections
            .iter()
            .find(|connection| {
                connection.conn_id == conn_id && connection.publication_token == publication_token
            })
            .map(|connection| {
                (
                    connection.transport_close.claim(),
                    Arc::clone(&connection.transport_close),
                )
            })
    });
    // `Some((false, _))`: another teardown path owns this connection's close.
    // `None`: the actor is gone, so its close already happened (or is happening
    // under someone else's claim) and the id may belong to a successor.
    // Neither may close here.
    if let Some((true, transport_close)) = claim {
        // SAFETY: mgr.transport is valid while the manager is alive.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        transport_close.finish();
    }
}

#[cfg(test)]
#[expect(
    clippy::too_many_arguments,
    reason = "legacy test adapter mirrors the pre-v2 publication surface"
)]
pub(super) fn publish_connection_established(
    mgr: &HewConnMgr,
    peer_route_slot: u16,
    conn_id: c_int,
    peer_feature_flags: u32,
    publication_token: u64,
    publication_sync: &Arc<Mutex<()>>,
    publication_removed: &Arc<AtomicBool>,
    superseded: Option<LiveClaim>,
) {
    let identity = test_node_identity(peer_route_slot);
    mgr.connections.access(|connections| {
        if let Some(connection) = connections
            .iter_mut()
            .find(|connection| connection.conn_id == conn_id)
        {
            connection.peer_identity = Some(identity);
            connection.peer_session_incarnation = 1;
            connection.publication_token = publication_token;
        }
    });
    publish_identity_connection_established(
        mgr,
        identity,
        peer_route_slot,
        1,
        conn_id,
        peer_feature_flags,
        publication_token,
        publication_sync,
        publication_removed,
        superseded,
    );
}

pub(super) fn retire_identity_connection_publication(
    mgr: &HewConnMgr,
    peer_identity: Option<NodeId>,
    peer_route_slot: u16,
    conn_id: c_int,
    publication_token: u64,
    publication_sync: &Arc<Mutex<()>>,
) {
    let Some(peer_identity) = peer_identity else {
        return;
    };

    // Retire our claim iff we are still the exact owner (issue #2652, D3 retire).
    // A superseded / non-matching connection removes nothing and must NOT drive
    // route removal, a cluster-lost notification, or a MonitorLost fan-out — the
    // peer is still live under a newer claim.
    let is_owner = retire_identity_claim(mgr, peer_identity, conn_id, publication_token);
    if !is_owner {
        return;
    }

    {
        let _publication = publication_sync.lock_or_recover();
        if !mgr.routing_table.is_null() {
            // SAFETY: pointer validity is checked by the callee.
            let _ = unsafe {
                hew_routing_remove_route_if_conn(
                    mgr.routing_table,
                    peer_identity,
                    conn_id,
                    publication_token,
                )
            };
        }
    }
    if !mgr.cluster.is_null() {
        // SAFETY: pointer validity is checked by the callee.
        let _ = unsafe {
            crate::cluster::hew_cluster_notify_connection_lost_if_current(
                mgr.cluster,
                peer_route_slot,
                publication_token,
            )
        };
    }

    // Cross-node monitor connection-drop fan-out: the connection to
    // `peer_node_id` is gone, so every local watcher of an actor on that node
    // gets a MonitorLost DOWN — unless it already received a definitive
    // clean-exit / crash DOWN (only Pending slots are armed; exactly-once).
    crate::hew_node::fan_out_monitor_lost_for_identity(peer_identity);
}

#[cfg(test)]
pub(super) fn retire_connection_publication(
    mgr: &HewConnMgr,
    peer_route_slot: u16,
    conn_id: c_int,
    publication_token: u64,
    publication_sync: &Arc<Mutex<()>>,
) {
    retire_identity_connection_publication(
        mgr,
        Some(test_node_identity(peer_route_slot)),
        peer_route_slot,
        conn_id,
        publication_token,
        publication_sync,
    );
}
