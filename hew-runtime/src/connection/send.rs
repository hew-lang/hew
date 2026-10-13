//! Outbound sends, broadcast and connection queries.

use std::ffi::c_int;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use crate::mailbox_envelope::MailboxPayloadClass;
use crate::node_identity::{HewLocation, Location, NodeId};
use crate::peer_binding::{ClaimState, Posture};
use crate::set_last_error;
use crate::transport::HEW_CONN_INVALID;
use crate::util::MutexExt;

use super::channel::ChannelProtection;
use super::control::encode_envelope;
use super::{HewConnMgr, ReaderLifecycleGuard, CONN_STATE_ACTIVE, CONN_STATE_CLOSED};

/// One manager-issued authority for an authenticated peer and transport slot.
/// The lease prevents removal or reuse of the slot until the send returns.
pub(crate) struct SendClaim<'a> {
    manager: &'a HewConnMgr,
    conn_id: c_int,
    identity: NodeId,
    route_slot: u16,
    session_incarnation: u32,
    _guard: ReaderLifecycleGuard,
    publication_removed: Arc<std::sync::atomic::AtomicBool>,
    channel: Arc<ChannelProtection>,
}

impl SendClaim<'_> {
    pub(crate) fn send(&self, frame: &[u8]) -> c_int {
        if self.publication_removed.load(Ordering::Acquire) {
            return -1;
        }
        // SAFETY: the manager owns the transport. The claim's lifecycle lease
        // prevents this exact connection slot from being removed or reused.
        match unsafe {
            self.channel
                .send(self.manager.transport, self.conn_id, frame)
        } {
            Ok(()) => 0,
            Err(error) => {
                set_last_error(format!("connection send on conn {}: {error}", self.conn_id));
                -1
            }
        }
    }
}

fn claim_send(
    manager: &HewConnMgr,
    conn_id: c_int,
    expected_publication: Option<u64>,
) -> Option<SendClaim<'_>> {
    let (claim, publication_token, credential) = manager.connections.access(|connections| {
        let connection = connections.iter().find(|connection| {
            connection.conn_id == conn_id
                && connection.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                && connection.posture == Posture::Strict
                && connection.peer_node_id != 0
                && connection.peer_session_incarnation != 0
                && expected_publication.is_none_or(|token| token == connection.publication_token)
        })?;
        Some((
            SendClaim {
                manager,
                conn_id,
                identity: connection.peer_identity?,
                route_slot: connection.peer_node_id,
                session_incarnation: connection.peer_session_incarnation,
                _guard: connection.claimed_send_lifecycle.register(),
                publication_removed: Arc::clone(&connection.publication_removed),
                channel: Arc::clone(&connection.channel),
            },
            connection.publication_token,
            connection.credential.clone(),
        ))
    })?;
    let published = manager
        .claims
        .0
        .lock_or_recover()
        .get(&claim.identity)
        .is_some_and(|owner| {
            owner.state == ClaimState::Published
                && owner.conn_id == conn_id
                && owner.publication_token == publication_token
                && owner.route_slot == claim.route_slot
                && owner.session_incarnation == claim.session_incarnation
                && owner.credential == credential
        });
    (published && !claim.publication_removed.load(Ordering::Acquire)).then_some(claim)
}

pub(crate) fn claim_send_to_location(
    manager: &HewConnMgr,
    conn_id: c_int,
    target: Location,
) -> Option<SendClaim<'_>> {
    let claim = claim_send(manager, conn_id, None)?;
    (claim.identity == target.node() && claim.session_incarnation == target.incarnation())
        .then_some(claim)
}

pub(crate) fn claim_send_to_route(
    manager: &HewConnMgr,
    conn_id: c_int,
    route_slot: u16,
) -> Option<SendClaim<'_>> {
    let claim = claim_send(manager, conn_id, None)?;
    (route_slot != 0 && claim.route_slot == route_slot).then_some(claim)
}

/// Legacy API: outbound queue tuning is no longer supported.
///
/// Returns 0 on success, -1 on failure.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_set_outbound_capacity(
    mgr: *mut HewConnMgr,
    _conn_id: c_int,
    _capacity: usize,
) -> c_int {
    if mgr.is_null() {
        set_last_error("hew_connmgr_set_outbound_capacity: manager is null");
        return -1;
    }
    set_last_error(
        "hew_connmgr_set_outbound_capacity: outbound queue support was removed; sends are synchronous",
    );
    -1
}

/// Send a wire envelope over a specific connection.
///
/// Returns 0 on success, -1 on failure.
///
/// # Safety
///
/// - `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
/// - `target` must point to a valid exact actor location.
/// - `data` must point to at least `size` readable bytes, or be null
///   when `size` is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_send(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    target: *const HewLocation,
    identity: *const std::ffi::c_char,
    data: *mut u8,
    size: usize,
) -> c_int {
    cabi_guard!(mgr.is_null() || target.is_null() || identity.is_null(), -1);
    // SAFETY: the caller supplies a NUL-terminated declaration identity.
    let Ok(identity) = (unsafe { std::ffi::CStr::from_ptr(identity) }).to_str() else {
        return -1;
    };
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };
    // SAFETY: caller guarantees `target` is readable.
    let Ok(target) = Location::try_from(unsafe { *target }) else {
        set_last_error("hew_connmgr_send: target location is invalid");
        return -1;
    };

    let Some(claim) = claim_send_to_location(mgr_ref, conn_id, target) else {
        set_last_error("hew_connmgr_send: no authenticated claim for the target");
        return -1;
    };

    // SAFETY: data is valid for size bytes per caller contract of hew_connmgr_send.
    let Some(encoded) = (unsafe {
        encode_envelope(
            target,
            identity,
            data,
            size,
            MailboxPayloadClass::SerializedCrossNode as u8,
            0,
        )
    }) else {
        return -1;
    };

    claim.send(&encoded)
}

pub(super) unsafe fn send_preencoded_on_manager(
    mgr_ref: &HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    data: *const u8,
    len: usize,
) -> c_int {
    let Some(claim) = claim_send(mgr_ref, conn_id, Some(publication_token)) else {
        return -1;
    };
    if data.is_null() && len != 0 {
        return -1;
    }
    let frame = if len == 0 {
        &[]
    } else {
        // SAFETY: the caller lends `len` readable bytes for this send.
        unsafe { std::slice::from_raw_parts(data, len) }
    };
    claim.send(frame)
}

/// Send a pre-encoded frame only if `conn_id` still names the exact published
/// connection incarnation identified by `publication_token`.
///
/// # Safety
///
/// `mgr` must be valid and `data` readable for `len` bytes.
pub(crate) unsafe fn hew_connmgr_send_preencoded_claimed(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    data: *const u8,
    len: usize,
) -> c_int {
    if mgr.is_null() || conn_id == HEW_CONN_INVALID || (data.is_null() && len > 0) {
        return -1;
    }
    // SAFETY: caller guarantees `mgr` and `data` validity.
    unsafe { send_preencoded_on_manager(&*mgr, conn_id, publication_token, data, len) }
}

/// Return the connection ID of the first active connection whose handshake
/// identified the peer as `node_id`. Returns -1 if not found.
///
/// Used by `send_reply_envelope` on the accepting side where `conn_by_node`
/// is not populated (only the initiating side calls `hew_node_connect`).
///
/// # Safety
///
/// `mgr` must be a valid pointer for the duration of the call.
pub(crate) unsafe fn hew_connmgr_conn_id_for_node(mgr: *const HewConnMgr, node_id: u16) -> c_int {
    if mgr.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };
    mgr_ref.connections.access(|conns| {
        for c in conns.iter() {
            if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE && c.peer_node_id == node_id {
                return c.conn_id;
            }
        }
        -1
    })
}

/// Return the negotiated feature flags for the active connection to `node_id`,
/// or `0` if no active connection exists.
///
/// Used by membership gossip to check the negotiated control capability.
///
/// # Safety
///
/// `mgr` must be a valid pointer for the duration of the call.
pub(crate) unsafe fn hew_connmgr_feature_flags_for_node(
    mgr: *const HewConnMgr,
    node_id: u16,
) -> u32 {
    if mgr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };
    mgr_ref.connections.access(|conns| {
        for c in conns.iter() {
            if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE && c.peer_node_id == node_id {
                return c.peer_feature_flags;
            }
        }
        0
    })
}

/// Return the number of active connections.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_count(mgr: *mut HewConnMgr) -> c_int {
    if mgr.is_null() {
        set_last_error("hew_connmgr_count: manager is null");
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    let count = mgr.connections.access(|conns| conns.len());
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "connection count will not exceed c_int range in practice"
    )]
    {
        count as c_int
    }
}

/// Send a message to all active connections.
///
/// Returns the number of successful sends.
///
/// # Safety
///
/// - `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
/// - `data` must point to at least `size` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_broadcast(
    mgr: *mut HewConnMgr,
    target: *const HewLocation,
    identity: *const std::ffi::c_char,
    data: *mut u8,
    size: usize,
) -> c_int {
    cabi_guard!(mgr.is_null() || target.is_null(), 0);
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };

    // Collect active connection IDs under the lock.
    let conn_ids: Vec<c_int> = mgr_ref.connections.access(|conns| {
        conns
            .iter()
            .filter(|c| c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE)
            .map(|c| c.conn_id)
            .collect()
    });

    let mut success_count: c_int = 0;
    for cid in conn_ids {
        // SAFETY: mgr is valid, data/size from caller.
        let rc = unsafe { hew_connmgr_send(mgr, cid, target, identity, data, size) };
        if rc == 0 {
            success_count += 1;
        }
    }

    success_count
}

/// Get the last activity timestamp (ms) for a connection.
///
/// Returns 0 if the connection is not found.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_last_activity(mgr: *mut HewConnMgr, conn_id: c_int) -> u64 {
    cabi_guard!(mgr.is_null(), 0);
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    mgr.connections.access(|conns| {
        conns
            .iter()
            .find(|c| c.conn_id == conn_id)
            .map_or(0, |c| c.last_activity_ms.load(Ordering::Acquire))
    })
}

/// Get the state of a connection.
///
/// Returns [`CONN_STATE_CLOSED`] if the connection is not found.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_conn_state(mgr: *mut HewConnMgr, conn_id: c_int) -> c_int {
    cabi_guard!(mgr.is_null(), CONN_STATE_CLOSED);
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    mgr.connections.access(|conns| {
        conns
            .iter()
            .find(|c| c.conn_id == conn_id)
            .map_or(CONN_STATE_CLOSED, |c| c.state.load(Ordering::Acquire))
    })
}
