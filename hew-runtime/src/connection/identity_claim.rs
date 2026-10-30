//! `NodeId` claim reservation, publication and retirement for admitted peers.

use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use crate::node_identity::NodeId;
use crate::peer_binding::{ClaimState, ConnectionOrigin, LiveClaim, PeerCredential};
use crate::routing::{
    hew_routing_add_route, hew_routing_can_add_route, hew_routing_remove_route_if_conn,
};
use crate::set_last_error;
use crate::util::{CondvarExt, MutexExt};

use super::admission::remove_connection;
use super::gossip::flush_registry_gossip_to_connection;
use super::handshake::close_transport_conn;
use super::reconnect::next_publication_token;
use super::{HewConnMgr, ReconnectOwner};

/// Bounded wait for an in-flight `Reserved` claim to resolve, in ms. Mirrors the
/// handshake timeout ceiling: a stuck reservation must not wedge admission.
pub(super) const CLAIM_RESERVE_WAIT_MS: u64 = 5_000;

/// Outcome of reserving a `NodeId` claim during admission (issue #2652, D3).
pub(super) enum ClaimReservation {
    /// The reservation succeeded. Carries the superseded same-credential
    /// `Published` claim (if any) to demote after our publish, and whether
    /// the peer returned at the session this node buried, which publishes the
    /// claim as [`ClaimState::Refuting`] instead.
    Reserved {
        superseded: Option<LiveClaim>,
        buried: bool,
    },
    /// This admission crossed the peer's own dial and lost the tie-break: the
    /// peer stays connected through the published owner named here, and the
    /// caller closes its own unpublished connection.
    Yielded(ReconnectOwner),
    /// The reservation was rejected fail-closed (a live different-credential
    /// owner, or an in-flight reservation that did not resolve in time). Carries
    /// the diagnostic detail.
    Rejected(String),
}

/// Where an admission came from and when it began, which together decide a
/// crossing dial.
#[derive(Clone, Copy, Debug)]
pub(super) struct AdmissionStart {
    pub(super) origin: ConnectionOrigin,
    /// The manager's admission order when this admission began. A claim that
    /// published at this order or later published while it was in flight.
    pub(super) began_at: u64,
}

impl AdmissionStart {
    pub(super) fn now(mgr: &HewConnMgr, origin: ConnectionOrigin) -> Self {
        Self {
            origin,
            began_at: mgr.next_publication_token.load(Ordering::Relaxed),
        }
    }
}

/// The node with the lower `NodeId` opens the connection both ends keep.
/// A relayed circuit uses the same order to pick its initiator.
pub(super) fn local_dials_first(local: NodeId, peer: NodeId) -> bool {
    local.to_bytes() < peer.to_bytes()
}

/// This end's part in settling which same-session connection a pair keeps.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Commit {
    /// This end is the responder: it settles the pair, and a reservation
    /// draws the order it commits the connection in.
    Responder,
    /// This end is the initiator: it keeps whichever connection the responder
    /// committed last, given this connection's commit order.
    Initiator(u64),
}

impl Commit {
    /// The role this end takes once the records are exchanged. The node with
    /// the lower `NodeId` initiates, as it does the Noise handshake.
    pub(super) fn negotiate(local: NodeId, peer: NodeId) -> Self {
        if local_dials_first(local, peer) {
            // The order arrives with the responder's commit.
            Self::Initiator(0)
        } else {
            Self::Responder
        }
    }
}

/// How a same-session admission settles against the peer's published
/// connection when the two were dialled from opposite ends.
enum CrossingDial {
    /// The newcomer was dialled by the lower `NodeId`: it replaces the other.
    Replace,
    /// The existing connection was dialled by the lower `NodeId`: keep it.
    Yield,
}

/// Decide a crossing dial under the responder-commit step. The responder
/// decides: two direct, same-session connections opened from opposite ends
/// whose admissions overlapped on the responder keep the one dialled by the
/// lower `NodeId`. The initiator keeps whichever the responder committed last.
///
/// The overlap is structural: the existing claim published after this
/// admission began, both drawn from the manager's one admission order. A
/// published connection therefore pins nothing beyond the admissions already
/// in flight when it published, each bounded by the connection-attempt
/// budget, and a half-open connection published before a redial began is
/// still replaced by it.
fn crossing_dial(
    mgr: &HewConnMgr,
    peer: NodeId,
    existing: &LiveClaim,
    admission: AdmissionStart,
    commit: Commit,
) -> Option<CrossingDial> {
    // The initiator follows the responder's commits: the later one is the
    // connection the responder kept, whatever order they arrive in here.
    if let Commit::Initiator(order) = commit {
        if existing.commit_order != 0 {
            if order > existing.commit_order {
                return Some(CrossingDial::Replace);
            }
            if existing.state.holds_connection() {
                return Some(CrossingDial::Yield);
            }
        }
    }
    // Only the responder decides by overlap: it is the one clock both ends
    // follow. Two ends deciding by their own clocks can disagree about
    // whether the admissions overlapped and each close the other's keeper.
    if commit != Commit::Responder {
        return None;
    }
    let local = mgr.local_identity?;
    if !existing.state.holds_connection() || existing.published_at < admission.began_at {
        return None;
    }
    let preferred = if local_dials_first(local, peer) {
        ConnectionOrigin::Dialled
    } else {
        ConnectionOrigin::Accepted
    };
    match (existing.origin, admission.origin) {
        (ConnectionOrigin::Relayed, _) | (_, ConnectionOrigin::Relayed) => None,
        (existing, newcomer) if existing == newcomer => None,
        (existing, _) if existing == preferred => Some(CrossingDial::Yield),
        _ => Some(CrossingDial::Replace),
    }
}

/// Select under the claims lock; retained identities keep their existing alias.
pub(super) fn select_route_alias(
    claims: &std::collections::HashMap<NodeId, LiveClaim>,
    local: u16,
    identity: NodeId,
    requested: u16,
) -> Option<u16> {
    if requested != 0 {
        return Some(requested);
    }
    if let Some(existing) = claims.get(&identity) {
        return Some(existing.route_slot);
    }
    let occupied: std::collections::HashSet<u16> = claims
        .values()
        .map(|claim| claim.route_slot)
        .chain(std::iter::once(local))
        .collect();
    (1..=u16::MAX).find(|candidate| !occupied.contains(candidate))
}

/// Reserve a `NodeId` claim for a connection mid-admission (issue #2652, D3
/// step 1). Runs under the claims mutex; the condvar coordinates the
/// reserve/publish handoff.
///
/// On an existing entry for `node_id`:
/// - `Reserved` (another admission in flight) ⇒ wait on the condvar until it
///   publishes/aborts, then re-evaluate; on timeout ⇒ reject.
/// - `Published` with a **different** credential ⇒ reject fail-closed (a live
///   authenticated owner is never taken over by a different credential).
/// - `Published` by a crossing dial under the responder-commit step (see
///   [`crossing_dial`]) ⇒ the responder keeps the lower `NodeId`'s dial and the
///   initiator the responder's last commit, whatever the reconnect predecessor.
/// - `Published` with the **same** credential ⇒ same-peer reconnect: supersede.
/// - absent ⇒ insert `Reserved`; no supersede.
#[expect(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "one reservation transaction owns alias allocation and authenticated session handoff"
)]
pub(super) fn reserve_identity_claim(
    mgr: &HewConnMgr,
    node_id: NodeId,
    route_slot: u16,
    session_incarnation: u32,
    credential: Option<&PeerCredential>,
    conn_id: c_int,
    publication_token: u64,
    predecessor: Option<&ReconnectOwner>,
    admission: AdmissionStart,
    commit: Commit,
) -> ClaimReservation {
    if (route_slot == 0 && mgr.auth.admission().is_none()) || session_incarnation == 0 {
        return ClaimReservation::Rejected(format!(
            "invalid route slot/session for NodeId {node_id} (route_slot={route_slot}, session={session_incarnation})"
        ));
    }
    let (lock, condvar) = &mgr.claims;
    let mut map = lock.lock_or_recover();
    let Some(route_slot) = select_route_alias(&map, mgr.local_node_id, node_id, route_slot) else {
        return ClaimReservation::Rejected("peer route alias space is exhausted".into());
    };
    loop {
        if mgr.reconnect_shutdown.load(Ordering::Acquire)
            || crate::transport::connection_attempt_remaining()
                .is_some_and(|remaining| remaining.is_zero())
        {
            return ClaimReservation::Rejected("connection attempt cancelled or expired".into());
        }
        if map
            .get(&node_id)
            .is_some_and(|claim| claim.state == ClaimState::Reserved)
        {
            // Another admission is mid-flight for this NodeId. Wait for it to
            // publish or abort, then re-evaluate. Bounded to avoid wedging. A
            // provisional admission may also abort and restore the exact
            // retired predecessor a reconnect is waiting to replace.
            let (guard, timeout) = condvar.wait_timeout_or_recover(
                map,
                crate::transport::connection_attempt_remaining().map_or(
                    std::time::Duration::from_millis(CLAIM_RESERVE_WAIT_MS),
                    |remaining| {
                        remaining.min(std::time::Duration::from_millis(CLAIM_RESERVE_WAIT_MS))
                    },
                ),
            );
            map = guard;
            if timeout.timed_out() {
                return ClaimReservation::Rejected(format!(
                    "timed out waiting on in-flight reservation for node id {node_id} (conn {conn_id})"
                ));
            }
            continue;
        }
        let claim = LiveClaim {
            credential: credential.cloned(),
            route_slot,
            session_incarnation,
            conn_id,
            publication_token,
            state: ClaimState::Reserved,
            origin: admission.origin,
            published_at: 0,
            commit_order: match commit {
                // Drawn under the claims lock, so it orders the reservations.
                Commit::Responder => next_publication_token(mgr).unwrap_or(u64::MAX),
                Commit::Initiator(order) => order,
            },
        };
        // A peer back at the session this node buried is admitted to refute
        // the burial over SWIM, and is published only once it has.
        let buried = !mgr.cluster.is_null() && {
            // SAFETY: cluster is owned by the live manager.
            let cluster = unsafe { &*mgr.cluster };
            cluster.member_session(route_slot) == Some(session_incarnation)
                && cluster.member_state(route_slot) == crate::cluster::MEMBER_DEAD
        };
        let Some(existing) = map.get(&node_id) else {
            if predecessor.is_some() {
                return ClaimReservation::Rejected(format!(
                    "stale reconnect predecessor for NodeId {node_id}"
                ));
            }
            map.insert(node_id, claim);
            return ClaimReservation::Reserved {
                superseded: None,
                buried,
            };
        };
        // A NodeId is key-derived, so another credential or route slot is
        // either a collision or a wrong local pin.
        if existing.credential.as_ref() != credential || existing.route_slot != route_slot {
            return ClaimReservation::Rejected(format!(
                "NodeId {node_id} is already bound to another credential or route slot (conn {conn_id})"
            ));
        }
        if session_incarnation < existing.session_incarnation {
            return ClaimReservation::Rejected(format!(
                "lower-session replay for NodeId {node_id}: received {session_incarnation}, current {}",
                existing.session_incarnation
            ));
        }
        if session_incarnation == existing.session_incarnation
            && !mgr.cluster.is_null()
            // SAFETY: cluster is owned by the live manager.
            && unsafe { (&*mgr.cluster).member_state(route_slot) } == crate::cluster::MEMBER_LEFT
        {
            return ClaimReservation::Rejected(format!(
                "equal session {session_incarnation} cannot revive departed NodeId {node_id}"
            ));
        }
        let crossing = (session_incarnation == existing.session_incarnation)
            .then(|| crossing_dial(mgr, node_id, existing, admission, commit))
            .flatten();
        match crossing {
            Some(CrossingDial::Yield) => {
                return ClaimReservation::Yielded(ReconnectOwner {
                    node_id,
                    session_incarnation: existing.session_incarnation,
                    publication_token: existing.publication_token,
                });
            }
            Some(CrossingDial::Replace) => {}
            None => {
                if predecessor
                    .is_some_and(|owner| owner.node_id != node_id || !owner.matches(existing))
                {
                    return ClaimReservation::Rejected(format!(
                        "stale reconnect predecessor for NodeId {node_id}"
                    ));
                }
            }
        }
        // Same-credential reconnect, a higher durable session, or the winning
        // side of a crossing dial.
        let superseded = map.insert(node_id, claim);
        return ClaimReservation::Reserved { superseded, buried };
    }
}

/// The later same-session admission that took `node_id`'s claim from the one
/// published as `own_token`, once it has published. `None` when
/// no such connection holds the claim: the peer is not connected through it.
pub(super) fn replacing_owner(
    mgr: &HewConnMgr,
    node_id: NodeId,
    session_incarnation: u32,
    own_token: u64,
) -> Option<ReconnectOwner> {
    let (lock, condvar) = &mgr.claims;
    let mut map = lock.lock_or_recover();
    loop {
        let claim = map.get(&node_id)?;
        // A later admission holds a later token; an earlier one is a restored
        // predecessor, not a replacement.
        if claim.publication_token <= own_token || claim.session_incarnation != session_incarnation
        {
            return None;
        }
        match claim.state {
            ClaimState::Published | ClaimState::Refuting { .. } => {
                return Some(ReconnectOwner {
                    node_id,
                    session_incarnation,
                    publication_token: claim.publication_token,
                });
            }
            ClaimState::Retired => return None,
            ClaimState::Reserved => {
                let wait = crate::transport::connection_attempt_remaining().map_or(
                    std::time::Duration::from_millis(CLAIM_RESERVE_WAIT_MS),
                    |remaining| {
                        remaining.min(std::time::Duration::from_millis(CLAIM_RESERVE_WAIT_MS))
                    },
                );
                let (guard, timeout) = condvar.wait_timeout_or_recover(map, wait);
                map = guard;
                if timeout.timed_out() {
                    return None;
                }
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
#[cfg(test)]
pub(super) fn publish_identity_claim(
    mgr: &HewConnMgr,
    node_id: NodeId,
    conn_id: c_int,
    publication_token: u64,
) -> bool {
    settle_identity_claim(
        mgr,
        node_id,
        conn_id,
        publication_token,
        |state| state == ClaimState::Reserved,
        ClaimState::Published,
    )
}

/// Move this admission's claim to `to`, iff it still holds it in a state
/// `from` accepts. Leaving `Reserved` stamps the admission order the claim
/// settled at; entering `Published` clears the names a predecessor taught
/// this node first.
fn settle_identity_claim(
    mgr: &HewConnMgr,
    node_id: NodeId,
    conn_id: c_int,
    publication_token: u64,
    from: impl Fn(ClaimState) -> bool,
    to: ClaimState,
) -> bool {
    let (lock, condvar) = &mgr.claims;
    let mut map = lock.lock_or_recover();
    let held = map
        .get(&node_id)
        .filter(|c| c.conn_id == conn_id && c.publication_token == publication_token)
        .map(|c| c.state)
        .filter(|state| from(*state));
    let still_ours = held.is_some();
    // Clear predecessor discovery before exposing Published to the new reader.
    // Holding the claim lock fences an old publication's final name mutation.
    // SAFETY: the node retains its canonical registry through all manager users.
    let registry = unsafe { mgr.registry.as_ref() };
    let names_changed = still_ours
        && to == ClaimState::Published
        && registry.is_some_and(|registry| registry.forget_origin_names(node_id));
    if still_ours {
        let published_at = (held == Some(ClaimState::Reserved))
            .then(|| next_publication_token(mgr).unwrap_or(u64::MAX));
        if let Some(claim) = map.get_mut(&node_id) {
            claim.state = to;
            if let Some(published_at) = published_at {
                claim.published_at = published_at;
            }
        }
    }
    drop(map);
    condvar.notify_all();
    if names_changed {
        registry.expect("changed registry").notify_names();
    }
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
        None,
        TEST_ADMISSION,
        Commit::Initiator(0),
    ) {
        ClaimReservation::Reserved { superseded, .. } => superseded,
        ClaimReservation::Yielded(_) => panic!("unexpected crossing-dial yield in test"),
        ClaimReservation::Rejected(detail) => {
            panic!("unexpected claim rejection in test (node {node_id}, conn {conn_id}): {detail}")
        }
    }
}

/// A dialled admission that began after every claim reserved, so it never
/// meets a crossing dial.
#[cfg(test)]
pub(super) const TEST_ADMISSION: AdmissionStart = AdmissionStart {
    origin: ConnectionOrigin::Dialled,
    began_at: u64::MAX,
};

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
        None,
        TEST_ADMISSION,
        Commit::Initiator(0),
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
            conn.transport = super::ConnectionTransport::borrowed(mgr.transport);
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

#[allow(
    clippy::too_many_lines,
    reason = "keep claim, route and cluster publication together with their ordered refusal paths"
)]
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
    buried: bool,
) -> bool {
    if publication_removed.load(Ordering::Acquire) {
        set_last_error("connection publication cancelled before establishment");
        return false;
    }

    // Transition our reservation Reserved → Published (issue #2652, D3 step 3),
    // or to Refuting for a buried peer, which publishes nothing until it
    // refutes. A superseding admission arriving in the reserve window ⇒ we are
    // no longer the map owner ⇒ abort publish (write no route / cluster token /
    // gossip).
    let settled = if buried {
        ClaimState::Refuting {
            expires_ms: refutation_deadline(mgr),
        }
    } else {
        ClaimState::Published
    };
    if !settle_identity_claim(
        mgr,
        peer_identity,
        conn_id,
        publication_token,
        |state| state == ClaimState::Reserved,
        settled,
    ) {
        set_last_error("connection publication no longer owns the peer claim");
        return false;
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
        return false;
    }

    if let Some(previous) = superseded.as_ref().filter(|previous| {
        previous.state == ClaimState::Published && previous.publication_token != publication_token
    }) {
        crate::hew_node::fan_out_monitor_lost_for_publication(
            peer_identity,
            previous.session_incarnation,
            previous.publication_token,
        );
    }

    if !buried
        && !expose_publication(
            mgr,
            peer_identity,
            peer_route_slot,
            peer_session_incarnation,
            conn_id,
            peer_feature_flags,
            publication_token,
            publication_sync,
            publication_removed,
        )
    {
        return false;
    }

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
        if superseded.state.holds_connection() && superseded.conn_id != conn_id {
            close_superseded_connection_once(mgr, superseded.conn_id, superseded.publication_token);
        }
    }
    if buried {
        // A peer that also buried this node never probes it, so open the
        // exchange: the PING leads with the peer's obituary.
        // SAFETY: the manager is live for this call; SWIM sends only read it.
        let _ = unsafe {
            super::swim::hew_connmgr_send_swim(
                std::ptr::from_ref(mgr).cast_mut(),
                peer_route_slot,
                crate::cluster::SWIM_MSG_PING,
                0,
            )
        };
    }
    true
}

/// Expose an admission whose claim just became `Published`: its cluster
/// membership token, its route, its service session and this node's names.
/// Fails closed, tearing the connection down, when the cluster refuses the
/// session or no route can be registered.
#[expect(
    clippy::too_many_arguments,
    reason = "exposure threads the publication's route, session and cancellation guards"
)]
fn expose_publication(
    mgr: &HewConnMgr,
    peer_identity: NodeId,
    peer_route_slot: u16,
    peer_session_incarnation: u32,
    conn_id: c_int,
    peer_feature_flags: u32,
    publication_token: u64,
    publication_sync: &Arc<Mutex<()>>,
    publication_removed: &Arc<AtomicBool>,
) -> bool {
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
            return false;
        }
        result == 1
    };
    if !published {
        set_last_error("connection publication cancelled during cluster admission");
        return false;
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
                set_last_error("connection publication cancelled before route admission");
                return false;
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
            return false;
        }
    }

    mgr.service_exports
        .observe_session(crate::envelope::NodeSessionIdentity {
            node_id: peer_identity,
            session_incarnation: peer_session_incarnation,
        });

    flush_registry_gossip_to_connection(mgr, conn_id, publication_token, peer_feature_flags);
    true
}

/// When a buried peer's refuting connection lapses: one suspicion window, the
/// time any suspected member has to refute before it is buried.
fn refutation_deadline(mgr: &HewConnMgr) -> u64 {
    // SAFETY: hew_now_ms has no preconditions.
    let now = unsafe { crate::clock::hew_now_ms() };
    if mgr.cluster.is_null() {
        return now;
    }
    // SAFETY: cluster is owned by the live manager.
    now.saturating_add(unsafe { &*mgr.cluster }.suspect_timeout_ms())
}

/// Publish the refuting claim of `conn_id`'s admission once the membership
/// table has readmitted its peer: the peer refuted its burial at a higher
/// incarnation, so the connection now carries everything a published one
/// does.
pub(super) fn publish_refuted_claim(mgr: &HewConnMgr, conn_id: c_int, publication_token: u64) {
    if mgr.cluster.is_null() {
        return;
    }
    let admission = mgr.connections.access(|connections| {
        connections
            .iter()
            .find(|connection| {
                connection.conn_id == conn_id && connection.publication_token == publication_token
            })
            .and_then(|connection| {
                Some((
                    connection.peer_identity?,
                    connection.peer_node_id,
                    connection.peer_session_incarnation,
                    connection.peer_feature_flags,
                    Arc::clone(&connection.publication_sync),
                    Arc::clone(&connection.publication_removed),
                ))
            })
    });
    let Some((identity, route_slot, session, feature_flags, sync, removed)) = admission else {
        return;
    };
    // SAFETY: cluster is owned by the live manager.
    if unsafe { &*mgr.cluster }.member_state(route_slot) != crate::cluster::MEMBER_ALIVE
        || removed.load(Ordering::Acquire)
        || !settle_identity_claim(
            mgr,
            identity,
            conn_id,
            publication_token,
            |state| matches!(state, ClaimState::Refuting { .. }),
            ClaimState::Published,
        )
    {
        return;
    }
    expose_publication(
        mgr,
        identity,
        route_slot,
        session,
        conn_id,
        feature_flags,
        publication_token,
        &sync,
        &removed,
    );
}

/// Close each refuting connection whose peer has not refuted by its deadline,
/// or whose buried member the membership table has since retired: the peer
/// is then admitted afresh when it next connects.
pub(crate) fn expire_refuting_claims(mgr: &HewConnMgr, now_ms: u64) {
    if mgr.cluster.is_null() {
        return;
    }
    // SAFETY: cluster is owned by the live manager.
    let cluster = unsafe { &*mgr.cluster };
    let lapsed: Vec<(NodeId, c_int, u64)> = mgr
        .claims
        .0
        .lock_or_recover()
        .iter()
        .filter(|(_, claim)| match claim.state {
            ClaimState::Refuting { expires_ms } => {
                expires_ms <= now_ms || cluster.member_state(claim.route_slot) == -1
            }
            _ => false,
        })
        .map(|(identity, claim)| (*identity, claim.conn_id, claim.publication_token))
        .collect();
    for (identity, conn_id, publication_token) in lapsed {
        // Retire the claim only while it is still refuting: a refutation read
        // since the scan has published the connection, which stays open.
        if settle_identity_claim(
            mgr,
            identity,
            conn_id,
            publication_token,
            |state| matches!(state, ClaimState::Refuting { .. }),
            ClaimState::Retired,
        ) {
            let _ = remove_connection(mgr, conn_id, Some(publication_token));
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
                    connection.transport.clone(),
                )
            })
    });
    // `Some((false, _))`: another teardown path owns this connection's close.
    // `None`: the actor is gone, so its close already happened (or is happening
    // under someone else's claim) and the id may belong to a successor.
    // Neither may close here.
    if let Some((true, transport_close, transport)) = claim {
        // SAFETY: mgr.transport is valid while the manager is alive.
        unsafe { close_transport_conn(transport.as_ptr(), conn_id) };
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
        false,
    );
}

pub(super) fn retire_identity_connection_publication(
    mgr: &HewConnMgr,
    peer_identity: Option<NodeId>,
    peer_route_slot: u16,
    peer_session_incarnation: u32,
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

    // Settle monitor/link observations registered by this exact publication.
    // A replacement publication may already own observations for the same
    // identity and session, so cleanup must not fan out by identity alone.
    crate::hew_node::fan_out_monitor_lost_for_publication(
        peer_identity,
        peer_session_incarnation,
        publication_token,
    );
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
        1,
        conn_id,
        publication_token,
        publication_sync,
    );
}

/// The commit order of the connection that holds `peer`'s claim.
#[cfg(test)]
pub(crate) fn commit_order_for_test(mgr: &HewConnMgr, peer: NodeId) -> Option<u64> {
    mgr.claims
        .0
        .lock_or_recover()
        .get(&peer)
        .map(|claim| claim.commit_order)
}

/// A test's view of one peer on this manager.
#[cfg(test)]
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct PeerConnectionView {
    /// Connections to the peer still installed.
    pub(crate) installed: usize,
    /// The peer claim's state and the origin of the connection that holds it.
    pub(crate) owner: Option<(ClaimState, ConnectionOrigin)>,
}

#[cfg(test)]
pub(crate) fn peer_connection_view(mgr: &HewConnMgr, peer: NodeId) -> PeerConnectionView {
    let owner = mgr
        .claims
        .0
        .lock_or_recover()
        .get(&peer)
        .map(|claim| (claim.state, claim.origin));
    PeerConnectionView {
        installed: mgr.connections.access(|connections| {
            connections
                .iter()
                .filter(|connection| connection.peer_identity == Some(peer))
                .count()
        }),
        owner,
    }
}
