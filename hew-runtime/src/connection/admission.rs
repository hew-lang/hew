//! Connection admission and removal.

use std::ffi::c_int;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::thread;

use crate::node_identity::NodeId;
use crate::peer_binding::{ClaimState, ConnectionOrigin, LiveClaim, Posture};
use crate::routing::hew_routing_remove_route_if_conn;
use crate::set_last_error;
use crate::transport::with_connection_attempt_deadline;
use crate::util::MutexExt;

use super::channel::{
    channel_kind, establish as establish_channel, local_channel_key, ChannelRefusal,
};
use super::handshake::{
    close_transport_conn, handshake_exchange, local_handshake, negotiates_responder_commit,
    offers_responder_commit, peer_identity_compatible, recv_commit, send_commit,
};
use super::identity_claim::{
    abort_identity_claim, publish_identity_connection_established, replacing_owner,
    reserve_identity_claim, retire_identity_connection_publication, settle_held_loser,
    AdmissionStart, ClaimReservation, Commit, HeldLoser,
};
use super::reader::reader_loop;
use super::reconnect::{next_publication_token, track_worker};
use super::{
    ConnectionActor, ConnectionInstallError, ConnectionInstallPublication, HewConnMgr,
    ReconnectOwner, SendConnMgr, CONN_STATE_ACTIVE, CONN_STATE_CLOSED,
};

pub(super) fn install_connection_actor(
    mgr: &HewConnMgr,
    actor: ConnectionActor,
) -> Result<ConnectionInstallPublication, ConnectionInstallError> {
    let conn_id = actor.conn_id;
    let mut actor = Some(actor);
    let install = mgr.connections.access(|conns| {
        if mgr.reconnect_shutdown.load(Ordering::Acquire) {
            Err(ConnectionInstallError::Shutdown)
        } else if conns.iter().any(|c| c.conn_id == conn_id) {
            Err(ConnectionInstallError::Duplicate)
        } else {
            let actor_ref = actor
                .as_ref()
                .expect("actor should remain available until install succeeds");
            let publication = ConnectionInstallPublication {
                token: actor_ref.publication_token,
                sync: Arc::clone(&actor_ref.publication_sync),
                removed: Arc::clone(&actor_ref.publication_removed),
            };
            conns.push(
                actor
                    .take()
                    .expect("actor should be consumed exactly once during install"),
            );
            Ok(publication)
        }
    });
    if install.is_err() {
        // The actor never entered the list, so this thread owns it outright —
        // but take the close through the same claim so its Drop cannot close
        // the handle a second time.
        match actor {
            Some(ref uninstalled) => {
                if uninstalled.transport_close.claim() {
                    // SAFETY: mgr.transport is valid per caller contract of hew_connmgr_add.
                    unsafe { close_transport_conn(uninstalled.transport.as_ptr(), conn_id) };
                    uninstalled.transport_close.finish();
                }
            }
            None => {
                unreachable!("an actor is consumed only on the successful install path");
            }
        }
    }
    install
}

/// Add a connection to the manager. Spawns a reader thread for inbound
/// messages.
///
/// Returns 0 on success, -1 on failure.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
/// `conn_id` must be a valid connection ID from the transport.
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_add(mgr: *mut HewConnMgr, conn_id: c_int) -> c_int {
    // SAFETY: the caller provides a live manager, or null.
    let Some(manager) = (unsafe { mgr.as_ref() }) else {
        set_last_error("hew_connmgr_add: manager is null");
        return -1;
    };
    let admission = AdmissionStart::now(manager, ConnectionOrigin::Dialled);
    // SAFETY: the caller provides a live manager and an owned transport connection.
    unsafe { add_connection(mgr, conn_id, None, admission) }.map_or(-1, |_| 0)
}

/// A successful admission: the peer is connected through `owner`.
#[derive(Debug)]
pub(super) enum Admitted {
    /// This connection was installed and published.
    Installed(ReconnectOwner),
    /// This connection crossed the peer's own dial and lost the tie-break, so
    /// it was closed; the peer stays connected through the published winner.
    Yielded(ReconnectOwner),
}

impl Admitted {
    pub(super) fn owner(self) -> ReconnectOwner {
        match self {
            Self::Installed(owner) | Self::Yielded(owner) => owner,
        }
    }
}

/// Inbound admissions that may authenticate at once. Each ends within the
/// connection-attempt budget, so a full set holds the accept loop at most that.
const MAX_INBOUND_ADMISSIONS: usize = 16;

/// Admit an accepted connection on a tracked worker under the
/// connection-attempt budget. A peer whose path dies mid-handshake can then
/// neither hold the accept loop nor delay other peers' admissions.
///
/// # Safety
///
/// `mgr` must be a live manager that outlives the call; `conn_id` is owned by
/// admission from entry.
pub(crate) unsafe fn spawn_inbound_admission(mgr: *mut HewConnMgr, conn_id: c_int) {
    // SAFETY: the caller pins a live manager; `hew_connmgr_free` joins the worker.
    let manager = unsafe { &*mgr };
    // The admission begins at accept, before any wait for a worker, so a busy
    // node still sees a dial that crossed its own as overlapping it.
    let start = AdmissionStart::now(manager, ConnectionOrigin::Accepted);
    manager
        .inbound_admissions
        .wait_below(MAX_INBOUND_ADMISSIONS);
    let admission = manager.inbound_admissions.register();
    let send = SendConnMgr(mgr);
    let spawned = track_worker(manager, "hew-inbound-admission", move || {
        let _admission = admission;
        let send = send;
        with_connection_attempt_deadline(|| {
            // SAFETY: the tracked worker pins the manager; admission owns `conn_id`.
            unsafe { add_connection(send.0, conn_id, None, start) };
        });
    });
    if !spawned {
        // SAFETY: admission owns this unpublished transport handle.
        unsafe { close_transport_conn(manager.transport, conn_id) };
    }
}

/// Admit a direct transport connection. Returns the exact admitted
/// publication even if its reader retires before admission returns: looking
/// up a recycled transport id afterwards is unsound. `None` means the
/// connection was refused and closed.
pub(super) unsafe fn add_connection(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    predecessor: Option<&ReconnectOwner>,
    admission: AdmissionStart,
) -> Option<Admitted> {
    // SAFETY: the caller pins a live manager, or supplies null.
    let Some(manager) = (unsafe { mgr.as_ref() }) else {
        set_last_error("hew_connmgr_add: manager is null");
        return None;
    };
    // SAFETY: the caller owns the connection and pins the node transport.
    unsafe {
        add_connection_on_transport(
            mgr,
            conn_id,
            super::ConnectionTransport::borrowed(manager.transport),
            None,
            predecessor,
            admission,
        )
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "authenticated admission is one transaction"
)]
pub(super) unsafe fn add_connection_on_transport(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    transport: super::ConnectionTransport,
    expected: Option<crate::envelope::NodeSessionIdentity>,
    predecessor: Option<&ReconnectOwner>,
    admission: AdmissionStart,
) -> Option<Admitted> {
    let transport_ptr = transport.as_ptr();
    if mgr.is_null() {
        // Cannot close conn_id: no transport pointer available.  This is API
        // misuse; the caller retains ownership of conn_id in this case only.
        set_last_error("hew_connmgr_add: manager is null");
        return None;
    }
    // Ownership: hew_connmgr_add takes ownership of conn_id on entry.
    // On SUCCESS: ownership transfers to the manager (closed by remove/free).
    // On FAILURE (any path below): conn_id is closed here; callers must not
    // close it themselves after observing a -1 return.
    // Preserve the raw pointer before reborrowing — `SendConnMgr` needs
    // `*mut` for the reader thread, and round-tripping `&T → *mut T`
    // violates Rust aliasing rules.
    let mgr_ptr = mgr;
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    let expected_peer_node_id = mgr
        .expected_peer_ids
        .access(|expected| expected.remove(&conn_id));

    if mgr.reconnect_shutdown.load(Ordering::Acquire) {
        // SAFETY: conn_id came from the transport and is not yet tracked by the manager.
        unsafe { close_transport_conn(transport_ptr, conn_id) };
        set_last_error("hew_connmgr_add: manager is shutting down");
        return None;
    }

    {
        let already_exists = mgr
            .connections
            .access(|conns| conns.iter().any(|c| c.conn_id == conn_id));
        if already_exists {
            // SAFETY: conn_id is a new transport handle that cannot be installed;
            // close it so the caller does not need to clean up on failure.
            unsafe { close_transport_conn(transport_ptr, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: connection {conn_id} already exists"
            ));
            return None;
        }
    }

    // The transport's ops identity alone decides the channel (#3332): TCP is
    // Noise-protected, quic-mesh is TLS-protected, and any other transport
    // carries no node traffic. Nothing the peer sends can select plaintext.
    let refuse_channel = |refusal: ChannelRefusal| {
        // SAFETY: transport_ptr and conn_id are valid per caller contract of hew_connmgr_add.
        unsafe { close_transport_conn(transport_ptr, conn_id) };
        set_last_error(format!("hew_connmgr_add: {refusal} (conn {conn_id})"));
        None
    };
    // SAFETY: transport_ptr is valid while the connection manager is alive.
    let Some(channel) = (unsafe { channel_kind(transport_ptr) }) else {
        return refuse_channel(ChannelRefusal::UnprotectedTransport);
    };
    // A stable per-node key (issue #2652, D1) so peers can pin it.
    let (local_noise_pubkey, local_noise_private) = match local_channel_key(channel, &mgr.auth) {
        Ok(key) => key,
        Err(refusal) => return refuse_channel(refusal),
    };

    let (Some(local_identity), Some(local_session_incarnation)) =
        (mgr.local_identity, mgr.local_session_incarnation)
    else {
        // SAFETY: transport_ptr and conn_id are valid per caller contract.
        unsafe { close_transport_conn(transport_ptr, conn_id) };
        set_last_error(
            "hew_connmgr_add: v2 distributed handshake requires a loaded authenticated local identity and durable session",
        );
        return None;
    };
    let mut local_hs = local_handshake(
        local_identity,
        local_session_incarnation,
        local_noise_pubkey,
    );
    if let Some(admission) = mgr.auth.admission() {
        local_hs.enrollment = admission.bundle().to_vec();
    }
    #[cfg(test)]
    if super::handshake::withheld_commit::applies(mgr) {
        local_hs.feature_flags &= !super::HEW_FEATURE_RESPONDER_COMMIT;
    }
    // SAFETY: transport_ptr and conn_id are valid per caller contract; local_hs is stack-local.
    let Some(peer_hs) = (unsafe { handshake_exchange(transport_ptr, conn_id, &local_hs) }) else {
        // SAFETY: transport_ptr and conn_id are valid per caller contract of hew_connmgr_add.
        unsafe { close_transport_conn(transport_ptr, conn_id) };
        return None;
    };
    if expected.is_some_and(|expected| {
        expected.node_id != peer_hs.node_id
            || expected.session_incarnation != peer_hs.session_incarnation
    }) {
        // SAFETY: admission owns this unpublished connection.
        unsafe { close_transport_conn(transport_ptr, conn_id) };
        set_last_error(
            "hew_connmgr_add: origin identity or session differs from the selected peer",
        );
        return None;
    }
    if !peer_identity_compatible(local_identity, peer_hs.node_id) {
        // SAFETY: transport_ptr and conn_id are valid per caller contract.
        unsafe { close_transport_conn(transport_ptr, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: peer NodeId {} collides with local NodeId for conn {conn_id}",
            peer_hs.node_id
        ));
        return None;
    }

    // ── Per-connection posture (issue #2652, D1/D2/BLOCK-7) ───────────────
    // Classify the endpoint and derive posture from this manager's per-node
    // snapshot (never a process-global authority). A `Strict` connection must
    // resolve an authenticated credential bound to the claimed NodeId; an
    // `Unverified` connection (demonstrated-loopback dev, or the explicit
    // opt-out) is delivery-only. Credential-free posture rejects fire here,
    // before the credential is resolved:
    // an unconfigured node (no peer bindings or stable credential) has no way
    // to authenticate the peer, so the strict connection is rejected rather
    // than silently admitted. A transport with no channel was refused above.
    // SAFETY: transport_ptr is valid while the manager is alive; conn_id is the
    // live handle being admitted.
    let remote_ip_class =
        unsafe { crate::transport::hew_transport_conn_remote_ip_class(transport_ptr, conn_id) };
    let posture = mgr.auth.posture_for(remote_ip_class);
    if posture != Posture::Strict {
        // v2 identity-bearing traffic is never admitted through the diagnostic
        // unverified posture.
        // SAFETY: transport_ptr and conn_id are valid per caller contract.
        unsafe { close_transport_conn(transport_ptr, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: v2 identity traffic requires authenticated strict posture (conn {conn_id})"
        ));
        return None;
    }
    if posture == Posture::Strict {
        let unconfigured =
            mgr.auth.admission().is_none() && !(cfg!(test) && mgr.auth.has_bindings());
        if unconfigured {
            // SAFETY: transport_ptr and conn_id are valid per caller contract.
            unsafe { close_transport_conn(transport_ptr, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: strict connection requires an installed cluster enrolment (conn {conn_id})"
            ));
            return None;
        }
    }

    // Establish the channel and recover the authenticated peer credential
    // (issue #2652, D2/D6): the Noise static key on TCP, the pinned leaf SPKI on
    // quic-mesh. Claim reservation below binds it to the claimed NodeId.
    // SAFETY: transport_ptr and conn_id are valid per caller contract; the
    // records and the private key are stack-local.
    let (protection, peer_credential) = match unsafe {
        establish_channel(
            channel,
            transport_ptr,
            conn_id,
            &mgr.auth,
            &local_hs,
            &peer_hs,
            &local_noise_private,
        )
    } {
        Ok(established) => established,
        Err(refusal) => return refuse_channel(refusal),
    };
    if let Some(admission) = mgr.auth.admission() {
        if let Err(error) = admission.verify_peer(&peer_hs.enrollment, &peer_credential) {
            // SAFETY: admission still owns this unpublished transport handle.
            unsafe { close_transport_conn(transport_ptr, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: cluster admission refused: {error}"
            ));
            return None;
        }
    }
    let derived_peer_identity = peer_credential.node_id();
    if derived_peer_identity != peer_hs.node_id {
        // SAFETY: transport_ptr and conn_id are valid per caller contract.
        unsafe { close_transport_conn(transport_ptr, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: authenticated credential derives NodeId {derived_peer_identity}, not advertised {} (conn {conn_id})",
            peer_hs.node_id
        ));
        return None;
    }
    #[cfg(test)]
    admission_script::reach(
        mgr,
        admission_script::Point::Authenticated,
        admission.origin,
    );
    // Under the responder-commit step the responder decides which connection
    // the pair keeps. It commits this connection and waits for the
    // initiator's echo before it touches its own claim, so the connection the
    // pair shares stays published and sendable throughout; the initiator
    // echoes once its claim has moved here and keeps the connection it
    // superseded until the responder closes one of the two. Neither end waits
    // on the network holding a reservation.
    let commit = match Commit::negotiate(
        offers_responder_commit(&local_hs),
        negotiates_responder_commit(&local_hs, &peer_hs),
        local_identity,
        peer_hs.node_id,
    ) {
        Commit::Initiator(_) => {
            #[cfg(test)]
            admission_script::reach(
                mgr,
                admission_script::Point::AwaitingCommit,
                admission.origin,
            );
            // SAFETY: admission owns the connection and no reader has started.
            let Some(order) = (unsafe { recv_commit(&protection, transport_ptr, conn_id) }) else {
                // SAFETY: admission still owns this unpublished transport handle.
                unsafe { close_transport_conn(transport_ptr, conn_id) };
                set_last_error(format!(
                    "hew_connmgr_add: peer {} did not commit conn {conn_id}",
                    peer_hs.node_id
                ));
                return None;
            };
            Commit::Initiator(order)
        }
        Commit::Responder(_) => {
            let order = next_publication_token(mgr).unwrap_or(u64::MAX);
            // SAFETY: admission owns the connection and no reader has started.
            let echoed = unsafe {
                send_commit(&protection, transport_ptr, conn_id, order)
                    && recv_commit(&protection, transport_ptr, conn_id) == Some(order)
            };
            if !echoed {
                // SAFETY: admission still owns this unpublished transport handle.
                unsafe { close_transport_conn(transport_ptr, conn_id) };
                set_last_error(format!(
                    "hew_connmgr_add: peer {} did not settle conn {conn_id}",
                    peer_hs.node_id
                ));
                return None;
            }
            #[cfg(test)]
            admission_script::reach(mgr, admission_script::Point::Echoed, admission.origin);
            Commit::Responder(order)
        }
        commit => commit,
    };
    let requested_route_slot = if cfg!(test) {
        mgr.auth
            .route_slot_for_credential(&peer_credential)
            .unwrap_or(0)
    } else {
        0
    };
    // Reserve the authenticated NodeId/session claim before any route, cluster,
    // registry, SWIM, monitor, link, or reply-table publication.
    let Some(claim_token) = next_publication_token(mgr) else {
        // SAFETY: transport_ptr and conn_id are valid per caller contract.
        unsafe { close_transport_conn(transport_ptr, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: publication token space is exhausted (conn {conn_id})"
        ));
        return None;
    };
    let (superseded_claim, buried): (Option<LiveClaim>, bool) = match reserve_identity_claim(
        mgr,
        peer_hs.node_id,
        requested_route_slot,
        peer_hs.session_incarnation,
        Some(&peer_credential),
        conn_id,
        claim_token,
        predecessor,
        admission,
        commit,
    ) {
        ClaimReservation::Reserved { superseded, buried } => (superseded, buried),
        ClaimReservation::Yielded(winner) => {
            // SAFETY: admission still owns this unpublished transport handle.
            unsafe { close_transport_conn(transport_ptr, conn_id) };
            return Some(Admitted::Yielded(winner));
        }
        ClaimReservation::Rejected(detail) => {
            // SAFETY: transport_ptr and conn_id are valid per caller contract.
            unsafe { close_transport_conn(transport_ptr, conn_id) };
            set_last_error(format!("hew_connmgr_add: {detail}"));
            return None;
        }
    };

    let reserved = mgr
        .claims
        .0
        .lock_or_recover()
        .get(&peer_hs.node_id)
        .filter(|claim| claim.conn_id == conn_id && claim.publication_token == claim_token)
        .map(|claim| claim.route_slot);
    let Some(peer_route_slot) = reserved else {
        abort_identity_claim(mgr, peer_hs.node_id, conn_id, claim_token, superseded_claim);
        // SAFETY: this failed admission still owns its unpublished handle.
        unsafe { close_transport_conn(transport_ptr, conn_id) };
        set_last_error("hew_connmgr_add: reserved identity lost its route assignment");
        return None;
    };
    if expected_peer_node_id.is_some_and(|expected| expected != peer_route_slot) {
        abort_identity_claim(mgr, peer_hs.node_id, conn_id, claim_token, superseded_claim);
        // SAFETY: admission owns the unpublished transport handle.
        unsafe { close_transport_conn(transport_ptr, conn_id) };
        set_last_error("hew_connmgr_add: peer route slot does not match selected target");
        return None;
    }
    // The initiator echoes the responder's commit once its claim has moved to
    // this connection.
    if let Commit::Initiator(order) = commit {
        #[cfg(test)]
        admission_script::reach(mgr, admission_script::Point::Echoing, admission.origin);
        // SAFETY: admission owns the connection and no reader has started.
        if !unsafe { send_commit(&protection, transport_ptr, conn_id, order) } {
            abort_identity_claim(mgr, peer_hs.node_id, conn_id, claim_token, superseded_claim);
            // SAFETY: admission owns the unpublished transport handle.
            unsafe { close_transport_conn(transport_ptr, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: could not echo peer {}'s commit on conn {conn_id}",
                peer_hs.node_id
            ));
            return None;
        }
    }

    let mut actor = ConnectionActor::new(conn_id, Arc::new(protection));
    actor.transport = transport.clone();
    actor.publication_token = claim_token;
    // Stash the superseded claim on the actor so hew_connmgr_remove can
    // restore it if removal wins the race against publication (the local
    // `superseded_claim` below still feeds the publish path's close of the
    // superseded transport).
    actor
        .superseded_claim
        .lock_or_recover()
        .clone_from(&superseded_claim);
    actor.peer_node_id = peer_route_slot;
    actor.peer_identity = Some(peer_hs.node_id);
    actor.peer_session_incarnation = peer_hs.session_incarnation;
    actor.peer_feature_flags = peer_hs.feature_flags;
    actor.posture = posture;
    actor.credential = Some(peer_credential);
    actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);

    // SAFETY: hew_now_ms has no preconditions.
    let now = unsafe { crate::clock::hew_now_ms() };
    actor.last_activity_ms.store(now, Ordering::Release);

    // I/O span: record accept span and stash context for reader_loop.
    // Fast path: atomic load only when tracing is disabled.
    crate::tracing::io_accept_span_begin(conn_id);

    // Spawn reader thread.
    let stop = Arc::clone(&actor.reader_stop);
    let transport_send = transport;
    let router = mgr.inbound_router;
    let activity_send = Arc::clone(&actor.last_activity_ms);
    let mgr_send = SendConnMgr(mgr_ptr);
    let peer_feature_flags = actor.peer_feature_flags;
    let reader_lifecycle_guard = mgr.reader_lifecycle.register();
    let channel = Arc::clone(&actor.channel);
    actor.held_loser = HeldLoser::new(commit).map(|held| Arc::new(std::sync::Mutex::new(held)));
    let held_loser = actor.held_loser.clone();
    let reader_held_loser = held_loser.clone();

    let handle = thread::Builder::new()
        .name(format!("hew-conn-{conn_id}"))
        .spawn(move || {
            let _reader_lifecycle_guard = reader_lifecycle_guard;
            reader_loop(
                mgr_send,
                transport_send,
                conn_id,
                claim_token,
                stop,
                activity_send,
                router,
                peer_feature_flags,
                channel,
                reader_held_loser,
                buried,
            );
        });

    if let Ok(h) = handle {
        actor.reader_handle = Some(h);
    } else {
        // SAFETY: actor.transport is valid per caller contract of hew_connmgr_add.
        unsafe { actor.close_transport() };
        // Abort the reservation (issue #2652, D3 step 2): restore the superseded
        // claim (if any) or remove our Reserved entry, and wake any waiter.
        abort_identity_claim(mgr, peer_hs.node_id, conn_id, claim_token, superseded_claim);
        set_last_error(format!(
            "hew_connmgr_add: failed to spawn reader thread for conn {conn_id}"
        ));
        return None;
    }

    let ConnectionInstallPublication {
        token: publication_token,
        sync: publication_sync,
        removed: publication_removed,
    } = match install_connection_actor(mgr, actor) {
        Ok(publication) => publication,
        Err(ConnectionInstallError::Shutdown) => {
            abort_identity_claim(mgr, peer_hs.node_id, conn_id, claim_token, superseded_claim);
            set_last_error(format!(
                "hew_connmgr_add: manager shutdown won install race for conn {conn_id}"
            ));
            return None;
        }
        Err(ConnectionInstallError::Duplicate) => {
            abort_identity_claim(mgr, peer_hs.node_id, conn_id, claim_token, superseded_claim);
            set_last_error(format!(
                "hew_connmgr_add: connection {conn_id} became duplicate during install"
            ));
            return None;
        }
    };

    if !publish_identity_connection_established(
        mgr,
        peer_hs.node_id,
        peer_route_slot,
        peer_hs.session_incarnation,
        conn_id,
        peer_hs.feature_flags,
        publication_token,
        &publication_sync,
        &publication_removed,
        superseded_claim,
        held_loser.as_deref(),
        buried,
    ) {
        // A same-session connection that took the claim while this one was
        // publishing keeps the peer connected: report it, as a crossing dial
        // that lost at reservation would.
        return replacing_owner(
            mgr,
            peer_hs.node_id,
            peer_hs.session_incarnation,
            publication_token,
        )
        .map(Admitted::Yielded);
    }

    Some(Admitted::Installed(ReconnectOwner {
        node_id: peer_hs.node_id,
        session_incarnation: peer_hs.session_incarnation,
        publication_token,
    }))
}

/// Pre-join admission-claim resolution for `hew_connmgr_remove` (see the
/// call site's comment): aborts a still-`Reserved` claim owned by this exact
/// admission (restoring `stashed_superseded`) and wakes any parked reader; a
/// `Published`-but-about-to-be-suppressed claim instead hands the stash back
/// for restoration AFTER retirement. Returns the claim to restore post-retire
/// (if any).
fn resolve_admission_claim_before_join(
    mgr: &HewConnMgr,
    peer_identity: Option<NodeId>,
    conn_id: c_int,
    publication_token: u64,
    stashed_superseded: Option<LiveClaim>,
) -> Option<LiveClaim> {
    let peer_identity = peer_identity?;
    let (lock, condvar) = &mgr.claims;
    let mut guard = lock.lock_or_recover();
    match guard.get(&peer_identity) {
        Some(claim) if claim.conn_id == conn_id && claim.publication_token == publication_token => {
            match claim.state {
                ClaimState::Reserved => {
                    match stashed_superseded {
                        Some(prev) => {
                            guard.insert(peer_identity, prev);
                        }
                        None => {
                            guard.remove(&peer_identity);
                        }
                    }
                    drop(guard);
                    condvar.notify_all();
                    None
                }
                ClaimState::Published | ClaimState::Refuting { .. } => stashed_superseded,
                ClaimState::Retired => None,
            }
        }
        _ => None,
    }
}

/// Remove a connection from the manager and close it.
///
/// Returns 0 on success, -1 if not found.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`](super::manager::hew_connmgr_new).
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_remove(mgr: *mut HewConnMgr, conn_id: c_int) -> c_int {
    if mgr.is_null() {
        set_last_error("hew_connmgr_remove: manager is null");
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    remove_connection(unsafe { &*mgr }, conn_id, None)
}

/// The guarded removal/teardown path, as a safe function over a borrowed
/// manager: [`hew_connmgr_remove`] is the FFI shim over it, and in-crate
/// callers (notably [`refuse_established_publication`](super::identity_claim::refuse_established_publication)) route through it
/// instead of hand-rolling teardown steps.
///
/// It maintains the teardown invariants in one place: cancel the publication,
/// remove the route, set `reader_stop` and then ACQUIRE the transport close
/// before closing — so the transport is closed exactly once no matter how many
/// callers arrive concurrently, and the woken reader takes the expected-stop
/// path (no re-entrant removal, no reconnect) — drain claimed sends, unlink the
/// actor, join the reader, then retire the claim, the route and the cluster
/// membership.
///
/// `expected_publication_token` makes an internal removal exact: refusal
/// teardown supplies its admission token so a stale publisher cannot remove a
/// successor that recycled the same numeric `conn_id`. The public remove API
/// passes `None` because it intentionally removes the connection currently
/// installed under the caller-supplied id.
///
/// Returns 0 on success, -1 if the requested admission is not installed.
#[allow(
    clippy::too_many_lines,
    reason = "function handles the full connection teardown and claim retirement protocol"
)]
pub(super) fn remove_connection(
    mgr: &HewConnMgr,
    conn_id: c_int,
    expected_publication_token: Option<u64>,
) -> c_int {
    // Phase 1: locate the connection and clone the publication handles — without
    // removing the conn from the list yet. Holding only Arc clones here so we
    // release the connections lock before calling into routing/cluster.
    let publication_data = mgr.connections.access(|conns| {
        let idx = conns.iter().position(|connection| {
            connection.conn_id == conn_id
                && expected_publication_token
                    .is_none_or(|token| connection.publication_token == token)
        });
        let Some(idx) = idx else {
            if let Some(token) = expected_publication_token {
                set_last_error(format!(
                    "hew_connmgr_remove: connection {conn_id} publication {token} not found"
                ));
            } else {
                set_last_error(format!(
                    "hew_connmgr_remove: connection {conn_id} not found"
                ));
            }
            return None;
        };
        let conn = &conns[idx];
        Some((
            conn.peer_node_id,
            conn.peer_identity,
            conn.peer_session_incarnation,
            conn.publication_token,
            Arc::clone(&conn.publication_sync),
            Arc::clone(&conn.publication_removed),
            Arc::clone(&conn.claimed_send_lifecycle),
        ))
    });
    let Some((
        peer_node_id,
        peer_identity,
        peer_session_incarnation,
        publication_token,
        publication_sync,
        publication_removed,
        claimed_send_lifecycle,
    )) = publication_data
    else {
        return -1;
    };

    // Step 2a: flag the publication as removed and remove the route from the
    // routing table — BEFORE removing the connection from mgr.connections.
    //
    // This eliminates the TOCTOU window where hew_routing_lookup returns
    // route-ok while send claims fail (connection already gone
    // from the list). After the route is removed, any new routing lookup for
    // this peer returns no route, so no new send claims are
    // dispatched to this conn_id. Route removal is idempotent; the full
    // retire_connection_publication call at the end of this function repeats
    // the routing step (no-op second time) and then fires the cluster
    // notification — intentionally deferred so that a racing replacement
    // connection can publish first and suppress the notification via the
    // publication-token check.
    {
        let _publication = publication_sync.lock_or_recover();
        publication_removed.store(true, Ordering::Release);
    }
    // Wake this connection's reader if it waits on another admission's claim
    // as a held loser: it has no authority left to wait for.
    mgr.claims.1.notify_all();
    // Publication may have filled in the identity after the initial lookup
    // snapshot. Refresh it only after cancellation is serialized: an earlier
    // publisher has now exposed its identity and route, while a later one must
    // observe `publication_removed` and cannot add a route.
    let peer_identity = peer_identity.or_else(|| {
        mgr.connections.access(|connections| {
            connections
                .iter()
                .find(|connection| {
                    connection.conn_id == conn_id
                        && connection.publication_token == publication_token
                })
                .and_then(|connection| connection.peer_identity)
        })
    });
    if !mgr.routing_table.is_null() {
        if let Some(peer_identity) = peer_identity {
            // SAFETY: routing_table is valid per manager contract.
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

    super::relay::carrier_lost(mgr, conn_id, publication_token);

    // Step 2b: close the transport while the actor remains in the list. That
    // keeps a new admission from reusing this numeric slot until every claimed
    // deferred send has observed the close and released its lifecycle guard.
    //
    // The close is ACQUIRED, not announced: a concurrent remover, refusal or
    // manager free reaching the same connection loses the claim and must not
    // touch the one-shot handle. It still has to see the close land before it
    // may unlink and drop the actor below, since the drop joins a reader that
    // only the real close can wake — so a loser waits instead of closing.
    let claim = mgr.connections.access(|connections| {
        let connection = connections.iter().find(|connection| {
            connection.conn_id == conn_id && connection.publication_token == publication_token
        })?;
        connection.reader_stop.store(1, Ordering::Release);
        Some((
            connection.transport_close.claim(),
            Arc::clone(&connection.transport_close),
            connection.transport.clone(),
        ))
    });
    let Some((owns_close, transport_close, transport)) = claim else {
        return 0;
    };
    if owns_close {
        // SAFETY: transport is valid per manager contract.
        unsafe { close_transport_conn(transport.as_ptr(), conn_id) };
        transport_close.finish();
    } else {
        transport_close.wait_closed();
    }
    claimed_send_lifecycle.wait_for_idle();

    // Remove only the exact closed publication. A second concurrent removal
    // finds it gone and returns harmlessly.
    let conn = mgr.connections.access(|conns| {
        let idx = conns.iter().position(|connection| {
            connection.conn_id == conn_id && connection.publication_token == publication_token
        })?;
        let conn = conns.swap_remove(idx);
        conn.state.store(CONN_STATE_CLOSED, Ordering::Release);
        Some(conn)
    });
    let Some(conn) = conn else {
        // Already removed by a concurrent hew_connmgr_remove call (reader re-entry).
        return 0;
    };

    // Resolve a still-pending admission BEFORE joining the reader. When removal
    // wins the race against `publish_connection_established`, the publication
    // early-returns on `publication_removed` without publishing or aborting,
    // and this connection's reader may be parked in the admission wait
    // (`wait_authenticated_peer_node_id_for_conn`) on the still-`Reserved`
    // claim — which only THIS abort (or the `CLAIM_RESERVE_WAIT_MS` backstop)
    // can resolve, because claim retirement runs after the join below. Abort
    // the reservation — restoring any claim this admission superseded — and
    // wake the waiter so the join cannot wedge on the backstop. Exact-owner
    // guarded (`conn_id` + token), so a successor's claim or an already-
    // aborted admission is untouched.
    //
    // If the claim is already PUBLISHED but our removal SUPPRESSES the
    // publication (`publication_removed` was set before the cluster notify),
    // the stashed predecessor claim is still live restorable state — the
    // publish path clears the stash only when publication completes. Carry it
    // past the retirement below and restore it there, so a same-credential
    // reconnect removed mid-publication hands authority back to the prior
    // connection instead of orphaning it claimless.
    let held_loser = conn.held_loser.clone();
    let restore_after_retire = resolve_admission_claim_before_join(
        mgr,
        peer_identity,
        conn_id,
        publication_token,
        conn.superseded_claim.lock_or_recover().take(),
    );
    // Drop this admission's parked registry-flush retry frames (token-matched
    // so a successor's parked frames on a reused conn_id are never touched).
    mgr.pending_registry_flush.access(|map| {
        if map
            .get(&conn_id)
            .is_some_and(|pending| pending.token == publication_token)
        {
            map.remove(&conn_id);
        }
        mgr.pending_registry_flush_count
            .store(map.len(), Ordering::Release);
    });
    // Drop joins the reader thread after transport close.
    drop(conn);
    // A connection that held the one it superseded for the peer's choice:
    // the loser takes the claim if the peer closed this one, before this
    // one's retirement could report the peer lost; otherwise it closes.
    let restored = held_loser.is_some_and(|held| {
        settle_held_loser(mgr, peer_identity, conn_id, publication_token, &held)
    });
    // Full publication retirement: re-removes route (idempotent) and fires the
    // cluster connection-lost notification. Deferred until after drop(conn) so
    // that a racing replacement connection can establish and publish first —
    // its higher publication token then suppresses this notification.
    retire_identity_connection_publication(
        mgr,
        peer_identity,
        peer_node_id,
        peer_session_incarnation,
        conn_id,
        publication_token,
        &publication_sync,
    );
    // The loser took the claim, so retirement settled nothing for this
    // publication: its own monitors and links are still lost.
    if let (true, Some(peer_identity)) = (restored, peer_identity) {
        crate::hew_node::fan_out_monitor_lost_for_publication(
            peer_identity,
            peer_session_incarnation,
            publication_token,
        );
    }

    // Suppressed-publication restore (see the pre-join comment above): with our
    // claim retired, hand the node's authority back to the superseded prior
    // connection — guarded on an EMPTY slot so a racing fresh admission's
    // reservation is never overwritten.
    if let Some(prev) = restore_after_retire {
        let Some(peer_identity) = peer_identity else {
            return 0;
        };
        let (lock, condvar) = &mgr.claims;
        let mut guard = lock.lock_or_recover();
        match guard.entry(peer_identity) {
            std::collections::hash_map::Entry::Occupied(mut occupied)
                if occupied.get().state == ClaimState::Retired
                    && occupied.get().conn_id == conn_id
                    && occupied.get().publication_token == publication_token =>
            {
                occupied.insert(prev);
            }
            std::collections::hash_map::Entry::Vacant(vacant) => {
                vacant.insert(prev);
            }
            std::collections::hash_map::Entry::Occupied(_) => {}
        }
        drop(guard);
        condvar.notify_all();
    }

    0
}

/// Test hooks that run at fixed points of a manager's admissions, so a test
/// can order two nodes' crossing admissions however it needs.
#[cfg(test)]
pub(crate) mod admission_script {
    use std::collections::HashMap;
    use std::sync::{Arc, LazyLock, Mutex};

    use crate::peer_binding::ConnectionOrigin;
    use crate::util::MutexExt;

    use super::HewConnMgr;

    /// Where an admission has reached.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub(crate) enum Point {
        /// The channel is established and the peer authenticated; nothing is
        /// reserved or committed yet.
        Authenticated,
        /// The initiator is about to wait for the responder's commit.
        AwaitingCommit,
        /// The initiator has reserved the committed connection and is about to
        /// echo the commit.
        Echoing,
        /// The responder has the initiator's echo and is about to reserve.
        Echoed,
        /// A held loser on this transport id has its claim reserved again and
        /// is about to publish.
        RestoringLoser(std::ffi::c_int),
        /// A node-API dial on this transport id was admitted, before its
        /// reconnect is armed.
        Admitted(std::ffi::c_int),
    }

    pub(crate) type Hook = Arc<dyn Fn(Point, ConnectionOrigin) + Send + Sync>;

    static HOOKS: LazyLock<Mutex<HashMap<usize, Hook>>> = LazyLock::new(Mutex::default);

    /// Hooks installed until this is dropped, even by a panic, so a later
    /// manager at a reused address runs none.
    pub(crate) struct Script(Vec<usize>);

    pub(crate) fn install(hooks: &[(*const HewConnMgr, Hook)]) -> Script {
        let mut installed = HOOKS.lock_or_recover();
        Script(
            hooks
                .iter()
                .map(|(mgr, hook)| {
                    installed.insert(*mgr as usize, Arc::clone(hook));
                    *mgr as usize
                })
                .collect(),
        )
    }

    impl Drop for Script {
        fn drop(&mut self) {
            let mut installed = HOOKS.lock_or_recover();
            for key in &self.0 {
                installed.remove(key);
            }
        }
    }

    pub(in crate::connection) fn reach(mgr: &HewConnMgr, point: Point, origin: ConnectionOrigin) {
        let hook = HOOKS
            .lock_or_recover()
            .get(&(std::ptr::from_ref(mgr) as usize))
            .cloned();
        if let Some(hook) = hook {
            hook(point, origin);
        }
    }
}
