//! Authenticated peer identity queries and inbound reply deposit.

use std::ffi::c_int;
use std::sync::atomic::Ordering;
use std::time::Duration;

use crate::envelope::EnvelopeFrame;
use crate::node_identity::Location;
use crate::peer_binding::{ClaimState, Posture};
use crate::util::{CondvarExt, MutexExt};

use super::handshake::is_ask_rejection_reply;
use super::identity_claim::CLAIM_RESERVE_WAIT_MS;
use super::{HewConnMgr, CONN_STATE_ACTIVE};

/// The posture-agnostic receiver-local route slot assigned to an ACTIVE
/// connection, or `0` if none. It carries no control-plane authority: an
/// unverified or superseded connection can still retain its delivery slot.
/// Security-relevant callers must use
/// [`authenticated_peer_node_id_for_conn`] instead. Retained as a test-only
/// accessor for route-publication tests; production inbound delivery routes via
/// the routing table populated at admission, not this lookup.
#[cfg(test)]
pub(crate) fn peer_node_id_for_conn(mgr: &HewConnMgr, conn_id: c_int) -> u16 {
    mgr.connections.access(|conns| {
        conns
            .iter()
            .find(|c| c.conn_id == conn_id && c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE)
            .map_or(0, |c| c.peer_node_id)
    })
}

/// The *authenticated* peer `NodeId` for a control frame arriving on `conn_id`
/// (issue #2652, D9/D12). Returns `Some(node_id)` only when the connection is
/// ACTIVE, carries `Strict` (credential-authenticated) posture, and advertised
/// a nonzero `NodeId` — i.e. it is the exact current owner of a published,
/// credential-bound claim. Returns `None` for an `Unverified` (loopback-dev /
/// opt-out) connection: such a peer is delivery-only and carries no
/// control-plane authority, so it can never inject registry/SWIM/cluster
/// membership gossip, monitor/link control, complete another peer's ask, NOR
/// receive outbound gossip/SWIM traffic. Inbound user-message delivery (D9's one
/// intentional `Unverified` capability) routes through the admission-populated
/// routing table keyed by the self-declared delivery id, so it does not consult
/// this posture gate.
pub(crate) fn authenticated_peer_node_id_for_conn(mgr: &HewConnMgr, conn_id: c_int) -> u16 {
    // (a) the connection must be ACTIVE, carry `Strict` posture, and advertise a
    // nonzero NodeId.
    let claimed = mgr.connections.access(|conns| {
        conns
            .iter()
            .find(|c| {
                c.conn_id == conn_id
                    && c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                    && c.posture == Posture::Strict
                    && c.peer_node_id != 0
            })
            .map(|c| (c.peer_identity, c.peer_node_id))
    });
    let Some((Some(identity), route_slot)) = claimed else {
        return 0;
    };
    // (b)+(c) the connection must be the *exact current owner* of the published
    // claim for its NodeId (issue #2652, D9 / BLOCK-4 point 2): the claim must be
    // `Published` AND owned by THIS `conn_id`. A superseded connection (D3 point
    // 2) still carries `Strict` posture but its `conn_id` no longer matches the
    // claim owner, so it loses control authority the instant the map is
    // overwritten — before its actor is even closed.
    let (lock, _condvar) = &mgr.claims;
    let guard = lock
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    match guard.get(&identity) {
        Some(claim) if claim.state == ClaimState::Published && claim.conn_id == conn_id => {
            route_slot
        }
        _ => 0,
    }
}

pub(crate) fn local_location_for_actor(mgr: &HewConnMgr, actor_id: u64) -> Option<Location> {
    let slot = crate::pid::hew_pid_serial(actor_id);
    Location::new(mgr.local_identity?, slot, mgr.local_session_incarnation?).ok()
}

/// As [`authenticated_peer_node_id_for_conn`], but tolerant of this
/// connection's own admission window. `hew_connmgr_add` spawns the reader
/// thread BEFORE `install_connection_actor` pushes the `ConnectionActor` into
/// `mgr.connections` and BEFORE `publish_connection_established` transitions
/// the claim `Reserved → Published`, so an inbound control frame can arrive
/// while the connection is missing from the connections list, or listed but
/// with its claim still `Reserved`. Some of those frames are one-shot — the
/// peer's registry-gossip flush at ITS publish drains the peer's event queue
/// and is never retransmitted — so an instant deny anywhere in that window
/// permanently loses cluster state (the lookup-unresolved race).
///
/// The gate therefore keys on the CLAIMS map first, not the connections list:
/// `reserve_claim` runs strictly before the reader spawn, so a `Reserved`
/// claim owned by THIS admission — identified by `(conn_id, claim_token)`,
/// the publication token minted for this admission before the reserve — is
/// present for the entire admission window and is the complete "admission in
/// flight" signal. While that claim is `Reserved`, block on the claims
/// condvar until the local admission decision resolves; every claim
/// transition (`publish_claim` / `abort_claim` / `retire_claim` /
/// `reserve_claim`) notifies the condvar, so this is a readiness signal on a
/// local bounded step, never a poll. Once the claim is `Published` — which
/// happens strictly after the connections-list install — the connections
/// entry decides (posture, ACTIVE state, self-declared node), matched by the
/// same publication token. An absent claim or a claim from a different
/// admission (superseded / aborted / a successor on a REUSED transport
/// `conn_id`) denies immediately and fail-closed, and the wait itself fails
/// closed on the `CLAIM_RESERVE_WAIT_MS` backstop.
///
/// The token binding matters because transport `conn_id`s are recycled: the
/// TCP transport hands out the first free slot index (`store_conn`), so a
/// reader still processing an already-read frame after its connection was
/// removed could otherwise observe a successor connection's claim under the
/// same `conn_id` and adopt that successor's authority. The publication token
/// is unique per admission (`next_publication_token`), so a stale reader's
/// gate resolves to deny the moment its own claim is gone.
pub(crate) fn wait_authenticated_peer_node_id_for_conn(
    mgr: &HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
) -> u16 {
    let deadline = std::time::Instant::now() + Duration::from_millis(CLAIM_RESERVE_WAIT_MS);
    let (lock, condvar) = &mgr.claims;
    let mut guard = lock.lock_or_recover();
    loop {
        let own_claim = guard
            .iter()
            .find(|(_, claim)| claim.conn_id == conn_id && claim.publication_token == claim_token)
            .map(|(node_id, claim)| (*node_id, claim.route_slot, claim.state));
        match own_claim {
            Some((claimed, route_slot, ClaimState::Published)) => {
                // Our admission published. Publication happens strictly after
                // the connections-list install, so the entry is present; bind
                // it by the same publication token (never bare `conn_id`) and
                // apply the posture / ACTIVE / self-declared-node checks the
                // non-waiting gate applies.
                drop(guard);
                return mgr.connections.access(|conns| {
                    conns
                        .iter()
                        .find(|c| {
                            c.conn_id == conn_id
                                && c.publication_token == claim_token
                                && c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                                && c.posture == Posture::Strict
                                && c.peer_identity == Some(claimed)
                                && c.peer_node_id == route_slot
                        })
                        .map_or(0, |c| c.peer_node_id)
                });
            }
            Some((_, _, ClaimState::Reserved)) => {
                // Our own admission is mid-flight (pre-install or
                // pre-publication); wait for it to publish or abort rather
                // than dropping the frame.
                let remaining = deadline.saturating_duration_since(std::time::Instant::now());
                if remaining.is_zero() {
                    return 0;
                }
                let (next_guard, _timeout) = condvar.wait_timeout_or_recover(guard, remaining);
                guard = next_guard;
            }
            // No claim owned by this admission: superseded, aborted, or a
            // claimless (node_id 0) connection — no authority, deny now.
            Some((_, _, ClaimState::Retired)) | None => return 0,
        }
    }
}

/// D12 data-plane gate: decide whether an inbound *ask* on `conn_id` must be
/// dropped because the source connection carries no reply/route authority.
///
/// Returns `true` (deny) when the envelope is an ask (`request_id > 0`) and the
/// connection is not the exact current owner of a published, credential-bound
/// claim — i.e. an `Unverified` (loopback-dev / opt-out) or superseded peer. A
/// fire-and-forget delivery (`request_id == 0`) is never denied here: it flows
/// on the D9 self-declared delivery route, the sole allowed `Unverified`
/// capability.
#[cfg(test)]
pub(super) fn inbound_ask_denied_unverified(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    request_id: u64,
) -> bool {
    if request_id == 0 {
        return false;
    }
    if mgr.is_null() {
        return true;
    }
    // SAFETY: reader_loop holds a live manager pointer for this connection while
    // the reader thread runs; the null case is handled above.
    let mgr_ref = unsafe { &*mgr };
    authenticated_peer_node_id_for_conn(mgr_ref, conn_id) == 0
}

/// Deposit an inbound reply envelope (`request_id > 0`, `source_node_id == 0`)
/// into the reply routing table, bypassing the normal inbound router. The
/// completion validates the originating `(conn_mgr, conn_id)` (issue #2652 D12)
/// so a peer cannot complete or reject another peer's ask.
pub(super) fn deposit_reply_envelope(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    peer_feature_flags: u32,
    envelope: &EnvelopeFrame,
) {
    if is_ask_rejection_reply(envelope.msg_type, peer_feature_flags) {
        // Rejection reply: the remote node hit its inbound ask path and sent an
        // encoded AskError reason. Mark the pending ask as failed so the
        // originating caller gets the precise remote rejection reason. The
        // `supports_ask_rejection` guard ensures old nodes (which never send
        // this sentinel) cannot trigger this path even if they happen to send a
        // message with msg_type = 65535.
        crate::hew_node::fail_remote_reply(
            mgr.cast_const(),
            conn_id,
            envelope.request_id,
            envelope.payload.as_slice(),
        );
    } else {
        crate::hew_node::complete_remote_reply(
            mgr.cast_const(),
            conn_id,
            envelope.request_id,
            envelope.payload.as_slice(),
        );
    }
}
