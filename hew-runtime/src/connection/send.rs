//! Authenticated outbound claims and connection queries.

use std::ffi::c_int;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use crate::node_identity::{Location, NodeId};
use crate::peer_binding::{ClaimState, Posture};
use crate::set_last_error;
use crate::transport::HEW_CONN_INVALID;
use crate::util::MutexExt;

use super::channel::{ChannelError, ChannelProtection};
use super::{HewConnMgr, ReaderLifecycleGuard, CONN_STATE_ACTIVE, CONN_STATE_CLOSED};

/// One manager-issued authority for an authenticated peer and transport slot.
/// The lease prevents removal or reuse of the slot until the send returns.
pub(crate) struct SendClaim {
    transport: *mut crate::transport::HewTransport,
    conn_id: c_int,
    identity: NodeId,
    route_slot: u16,
    session_incarnation: u32,
    _guard: ReaderLifecycleGuard,
    pub(crate) publication_token: u64,
    publication_removed: Arc<std::sync::atomic::AtomicBool>,
    channel: Arc<ChannelProtection>,
    gossip_out: Arc<std::sync::atomic::AtomicU64>,
}

// SAFETY: the claim's lifecycle lease pins its transport connection until drop.
// Manager removal closes the socket before waiting for all such leases, and the
// channel serializes encryption and transport access across senders.
unsafe impl Send for SendClaim {}

impl SendClaim {
    pub(crate) fn connection_id(&self) -> c_int {
        self.conn_id
    }

    pub(crate) fn peer_session(&self) -> crate::envelope::NodeSessionIdentity {
        crate::envelope::NodeSessionIdentity {
            node_id: self.identity,
            session_incarnation: self.session_incarnation,
        }
    }

    pub(crate) fn publication_removed(&self) -> Arc<std::sync::atomic::AtomicBool> {
        Arc::clone(&self.publication_removed)
    }

    /// Transfer this lease to the existing pool. The caller never waits on the
    /// channel's send lock or on a peer accepting the frame.
    pub(crate) fn submit(
        self,
        prepare: impl FnOnce() -> Option<Vec<u8>> + Send + 'static,
        complete: impl FnOnce(Result<(), ChannelError>) + Send + 'static,
    ) -> bool {
        fn enqueue<F: FnOnce() + Send + 'static>(work: F) -> bool {
            unsafe extern "C" fn run<F: FnOnce()>(context: *mut std::ffi::c_void) {
                // SAFETY: successful submission transfers this box to one callback.
                let work = unsafe { Box::from_raw(context.cast::<F>()) };
                work();
            }
            let Some(pool) = crate::blocking_pool::shared_blocking_pool_opt() else {
                return false;
            };
            let job = Box::into_raw(Box::new(work));
            // SAFETY: the installed pool receives an owned closure. Its send
            // lease keeps the connection alive until this callback returns.
            if unsafe { crate::blocking_pool::submit_on_worker(pool, run::<F>, job.cast()) } == 0 {
                true
            } else {
                // SAFETY: failed admission did not transfer the box.
                drop(unsafe { Box::from_raw(job) });
                false
            }
        }
        enqueue(move || complete(self.send_prepared_result(prepare)))
    }

    pub(crate) fn send(&self, frame: &[u8]) -> c_int {
        self.send_prepared(|| Some(frame))
    }

    pub(crate) fn send_prepared<B: AsRef<[u8]>>(
        &self,
        prepare: impl FnOnce() -> Option<B>,
    ) -> c_int {
        if self.send_prepared_result(prepare).is_ok() {
            0
        } else {
            -1
        }
    }

    fn send_prepared_result<B: AsRef<[u8]>>(
        &self,
        prepare: impl FnOnce() -> Option<B>,
    ) -> Result<(), ChannelError> {
        if self.publication_removed.load(Ordering::Acquire) {
            return Err(ChannelError::Faulted);
        }
        let mut gossip = false;
        // SAFETY: the manager owns the transport. The claim's lifecycle lease
        // prevents this exact connection slot from being removed or reused.
        let result = unsafe {
            self.channel
                .send_prepared(self.transport, self.conn_id, || {
                    let frame = prepare()?;
                    if crate::observe::observe_hot_tier_enabled() {
                        gossip = matches!(crate::envelope::decode_wire_frame(frame.as_ref()),
                            Ok(crate::envelope::WireFrame::Control(control))
                            if matches!(control.ctrl_kind, crate::envelope::CTRL_SWIM | crate::envelope::CTRL_REGISTRY_GOSSIP));
                    }
                    Some(frame)
                })
        };
        if result.is_ok() && gossip {
            self.gossip_out.fetch_add(1, Ordering::Relaxed);
        }
        if let Err(error) = result {
            set_last_error(format!("connection send on conn {}: {error}", self.conn_id));
        }
        result
    }
}

pub(crate) fn claim_send(
    manager: &HewConnMgr,
    conn_id: c_int,
    expected_publication: Option<u64>,
) -> Option<SendClaim> {
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
                transport: manager.transport,
                conn_id,
                identity: connection.peer_identity?,
                route_slot: connection.peer_node_id,
                session_incarnation: connection.peer_session_incarnation,
                _guard: connection.claimed_send_lifecycle.register(),
                publication_token: connection.publication_token,
                publication_removed: Arc::clone(&connection.publication_removed),
                channel: Arc::clone(&connection.channel),
                gossip_out: Arc::clone(&manager.gossip_out),
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

pub(crate) fn claim_send_by_identity(
    manager: &HewConnMgr,
    peer: NodeId,
    publication: Option<u64>,
) -> Option<SendClaim> {
    let connection = manager.claims.0.lock_or_recover().get(&peer)?.conn_id;
    let claim = claim_send(manager, connection, publication)?;
    (claim.identity == peer).then_some(claim)
}

pub(crate) fn claim_send_to_location(
    manager: &HewConnMgr,
    conn_id: c_int,
    target: Location,
) -> Option<SendClaim> {
    let claim = claim_send(manager, conn_id, None)?;
    (claim.identity == target.node() && claim.session_incarnation == target.incarnation())
        .then_some(claim)
}

pub(crate) fn claim_send_to_route(
    manager: &HewConnMgr,
    conn_id: c_int,
    route_slot: u16,
    publication: Option<u64>,
) -> Option<SendClaim> {
    let claim = claim_send(manager, conn_id, publication)?;
    (route_slot != 0 && claim.route_slot == route_slot).then_some(claim)
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
