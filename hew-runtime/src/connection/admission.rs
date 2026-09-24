//! Connection admission and removal.

use std::ffi::c_int;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::thread;

#[cfg(feature = "encryption")]
use zeroize::Zeroizing;

use crate::node_identity::NodeId;
use crate::peer_binding::{ClaimState, LiveClaim, PeerCredential, Posture};
use crate::routing::hew_routing_remove_route_if_conn;
use crate::set_last_error;
use crate::util::MutexExt;

use super::handshake::{
    close_transport_conn, handshake_exchange, local_handshake, peer_identity_compatible,
};
#[cfg(feature = "encryption")]
use super::handshake::{supports_encryption, upgrade_noise};
use super::identity_claim::{
    abort_identity_claim, publish_identity_connection_established, reserve_identity_claim,
    retire_identity_connection_publication, ClaimReservation,
};
use super::reader::reader_loop;
use super::reconnect::next_publication_token;
#[cfg(feature = "encryption")]
use super::NOISE_PATTERN;
use super::{
    ConnectionActor, ConnectionInstallError, ConnectionInstallPublication, HewConnMgr, SendConnMgr,
    SendTransport, CONN_STATE_ACTIVE, CONN_STATE_CLOSED, NOISE_STATIC_PUBKEY_LEN,
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
                    unsafe { close_transport_conn(mgr.transport, conn_id) };
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
#[expect(
    clippy::too_many_lines,
    reason = "connection event loop handles all states"
)]
pub unsafe extern "C" fn hew_connmgr_add(mgr: *mut HewConnMgr, conn_id: c_int) -> c_int {
    if mgr.is_null() {
        // Cannot close conn_id: no transport pointer available.  This is API
        // misuse; the caller retains ownership of conn_id in this case only.
        set_last_error("hew_connmgr_add: manager is null");
        return -1;
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
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error("hew_connmgr_add: manager is shutting down");
        return -1;
    }

    {
        let already_exists = mgr
            .connections
            .access(|conns| conns.iter().any(|c| c.conn_id == conn_id));
        if already_exists {
            // SAFETY: conn_id is a new transport handle that cannot be installed;
            // close it so the caller does not need to clean up on failure.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: connection {conn_id} already exists"
            ));
            return -1;
        }
    }

    #[cfg_attr(
        not(feature = "encryption"),
        allow(
            unused_mut,
            reason = "filled by copy_from_slice only in the encryption-gated keypair block"
        )
    )]
    let mut local_noise_pubkey = [0u8; NOISE_STATIC_PUBKEY_LEN];
    #[cfg(feature = "encryption")]
    let local_noise_private = {
        #[cfg(feature = "quic")]
        let skip_noise = {
            // SAFETY: mgr.transport is valid while the connection manager is alive.
            unsafe {
                crate::quic_transport::hew_transport_is_quic(mgr.transport)
                    || crate::quic_mesh::hew_transport_is_quic_mesh(mgr.transport)
            }
        };
        #[cfg(not(feature = "quic"))]
        let skip_noise = false;
        if skip_noise {
            Zeroizing::new(Vec::new())
        } else {
            let Ok(pattern) = NOISE_PATTERN.parse() else {
                // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
                unsafe { close_transport_conn(mgr.transport, conn_id) };
                set_last_error("hew_connmgr_add: invalid noise pattern");
                return -1;
            };
            if let Some(identity) = mgr.auth.noise_identity() {
                // Stable per-node Noise identity (issue #2652, D1): present the same
                // static key on every connection and across restarts so peers can
                // pin it via `Node::allow_peer`. Retires per-connection keypair
                // churn, which made a node's Noise public key unbindable.
                local_noise_pubkey.copy_from_slice(&identity.public());
                Zeroizing::new(identity.private().to_vec())
            } else {
                // No stable identity loaded (unconfigured / loopback-dev node): fall
                // back to an ephemeral keypair. Such a node is `Unverified`
                // (delivery-only); no peer pins its key, so churn is harmless.
                let builder = snow::Builder::new(pattern);
                let Ok(keypair) = builder.generate_keypair() else {
                    // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
                    unsafe { close_transport_conn(mgr.transport, conn_id) };
                    set_last_error("hew_connmgr_add: failed to generate noise keypair");
                    return -1;
                };
                local_noise_pubkey.copy_from_slice(&keypair.public);
                Zeroizing::new(keypair.private)
            }
        }
    };

    let (Some(local_identity), Some(local_session_incarnation)) =
        (mgr.local_identity, mgr.local_session_incarnation)
    else {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(
            "hew_connmgr_add: v2 distributed handshake requires a loaded authenticated local identity and durable session",
        );
        return -1;
    };
    let local_hs = local_handshake(
        local_identity,
        local_session_incarnation,
        local_noise_pubkey,
    );
    // SAFETY: mgr.transport and conn_id are valid per caller contract; local_hs is stack-local.
    let Some(peer_hs) = (unsafe { handshake_exchange(mgr.transport, conn_id, local_hs) }) else {
        // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        return -1;
    };
    if !peer_identity_compatible(local_identity, peer_hs.node_id) {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: peer NodeId {} collides with local NodeId for conn {conn_id}",
            peer_hs.node_id
        ));
        return -1;
    }

    // ── Per-connection posture (issue #2652, D1/D2/BLOCK-7) ───────────────
    // Classify the endpoint and derive posture from this manager's per-node
    // snapshot (never a process-global authority). A `Strict` connection must
    // resolve an authenticated credential bound to the claimed NodeId; an
    // `Unverified` connection (demonstrated-loopback dev, or the explicit
    // opt-out) is delivery-only. Credential-free posture rejects fire here,
    // before the credential is resolved:
    //   * an unconfigured node (no peer bindings or stable credential) on a
    //     non-loopback/Unknown endpoint has no way to authenticate the peer, so
    //     the strict connection is rejected rather than silently admitted;
    //   * a strict connection over a transport with no peer-credential channel
    //     (plain quic / stub / Unknown) is rejected fail-closed.
    // SAFETY: mgr.transport is valid while the manager is alive; conn_id is the
    // live handle being admitted.
    let remote_ip_class =
        unsafe { crate::transport::hew_transport_conn_remote_ip_class(mgr.transport, conn_id) };
    let posture = mgr.auth.posture_for(remote_ip_class);
    if posture != Posture::Strict {
        // v2 identity-bearing traffic is never admitted through the diagnostic
        // unverified posture.
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: v2 identity traffic requires authenticated strict posture (conn {conn_id})"
        ));
        return -1;
    }
    if posture == Posture::Strict {
        let unconfigured = !mgr.auth.has_bindings();
        if unconfigured {
            // SAFETY: mgr.transport and conn_id are valid per caller contract.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: strict connection requires configured peer credentials (conn {conn_id})"
            ));
            return -1;
        }
        if remote_ip_class == crate::peer_binding::RemoteIpClass::Unknown {
            // Unknown transport class (plain quic / stub) has no peer-credential
            // mechanism — a strict admission cannot be authenticated.
            // SAFETY: mgr.transport and conn_id are valid per caller contract.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: strict binding unsupported on plain quic transport (use quic-mesh or tcp-noise) (conn {conn_id})"
            ));
            return -1;
        }
    }

    #[cfg(feature = "encryption")]
    let skip_noise = {
        #[cfg(feature = "quic")]
        {
            // QUIC provides TLS 1.3 encryption — skip Noise when using QUIC transport.
            // SAFETY: mgr.transport is valid while the connection manager is alive.
            unsafe {
                crate::quic_transport::hew_transport_is_quic(mgr.transport)
                    || crate::quic_mesh::hew_transport_is_quic_mesh(mgr.transport)
            }
        }
        #[cfg(not(feature = "quic"))]
        {
            false
        }
    };

    #[cfg(feature = "encryption")]
    let upgraded_noise = if !skip_noise
        && supports_encryption(local_hs.feature_flags)
        && supports_encryption(peer_hs.feature_flags)
    {
        // SAFETY: mgr.transport and conn_id are valid per caller contract;
        // local_hs, peer_hs, and local_noise_private are valid stack-local references.
        unsafe {
            upgrade_noise(
                mgr.transport,
                conn_id,
                &local_hs,
                &peer_hs,
                &local_noise_private,
            )
        }
    } else {
        None
    };

    // The authenticated peer credential this connection presents (issue #2652),
    // used to bind the claimed `NodeId` in the claim machine below. Populated
    // from the Noise static key for tcp-noise; mesh SPKI extraction is wired in
    // a later slice. `None` under `Unverified` posture (loopback / opt-out).
    #[cfg(feature = "encryption")]
    let mut peer_credential: Option<PeerCredential> = None;

    #[cfg(feature = "encryption")]
    let upgraded_noise = if !skip_noise
        && supports_encryption(local_hs.feature_flags)
        && supports_encryption(peer_hs.feature_flags)
    {
        let Some((noise, peer_static_pubkey)) = upgraded_noise else {
            // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: noise upgrade failed for conn {conn_id}"
            ));
            return -1;
        };
        if posture == Posture::Strict && !mgr.auth.noise_pubkey_allowlisted(&peer_static_pubkey) {
            // Per-node Noise pre-gate (issue #2652, D14): under strict posture
            // the peer's stable Noise static key must be bound in THIS node's
            // snapshot (via `allow_peer`) — not a process-global allowlist.
            // `authorize` below then binds the key to the *claimed* NodeId; this
            // pre-gate rejects an entirely unknown key early, before a claim is
            // reserved. Under `Unverified` posture (loopback dev / opt-out)
            // delivery is allowed without a binding.
            // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: peer key not allowlisted for conn {conn_id}"
            ));
            return -1;
        }
        if peer_hs.static_noise_pubkey != peer_static_pubkey {
            // SAFETY: mgr.transport and conn_id are valid per caller contract.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: authenticated Noise key does not match the v2 handshake key for conn {conn_id}"
            ));
            return -1;
        }
        peer_credential = Some(PeerCredential::NoiseKey(peer_static_pubkey));
        Some(noise)
    } else {
        None
    };

    // Resolve the credential for the claim machine (issue #2652, D2/D6):
    //  - tcp-noise: the Noise static key recovered above (encryption build);
    //  - quic-mesh: the peer's leaf-certificate SPKI (D6) — the mTLS handshake
    //    already pinned the SPKI at the transport layer, so binding it here ties
    //    the *claimed* NodeId to that authenticated key (an allowlisted key must
    //    not claim a NodeId bound to a different key);
    //  - otherwise (plain tcp without encryption / plain quic / Unknown):
    //    credential-free (delivery-only / loopback dev).
    #[cfg(feature = "encryption")]
    let noise_credential: Option<PeerCredential> = peer_credential;
    #[cfg(not(feature = "encryption"))]
    let noise_credential: Option<PeerCredential> = None;

    #[cfg(feature = "quic")]
    // SAFETY: mgr.transport is valid while the manager is alive; conn_id is the
    // live handle being admitted. A non-mesh/unknown conn yields None.
    let peer_credential: Option<PeerCredential> = if unsafe {
        crate::quic_mesh::hew_transport_is_quic_mesh(mgr.transport)
    } {
        if peer_hs.static_noise_pubkey != [0; NOISE_STATIC_PUBKEY_LEN] {
            // SAFETY: mgr.transport and conn_id are valid per caller contract.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                    "hew_connmgr_add: quic-mesh v2 handshake carried a non-zero Noise key for conn {conn_id}"
                ));
            return -1;
        }
        // SAFETY: same caller contract as above.
        unsafe { crate::quic_mesh::hew_transport_quic_mesh_peer_spki(mgr.transport, conn_id) }
            .map(PeerCredential::Spki)
    } else {
        noise_credential
    };
    #[cfg(not(feature = "quic"))]
    let peer_credential: Option<PeerCredential> = noise_credential;

    let Some(peer_credential) = peer_credential else {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: authenticated transport credential unavailable for conn {conn_id}"
        ));
        return -1;
    };
    let derived_peer_identity = peer_credential.node_id();
    if derived_peer_identity != peer_hs.node_id {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: authenticated credential derives NodeId {derived_peer_identity}, not advertised {} (conn {conn_id})",
            peer_hs.node_id
        ));
        return -1;
    }
    let Some(peer_route_slot) = mgr.auth.route_slot_for_credential(&peer_credential) else {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: authenticated credential has no configured route slot (conn {conn_id})"
        ));
        return -1;
    };
    if expected_peer_node_id.is_some_and(|expected| expected != peer_route_slot) {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: peer route slot {peer_route_slot} does not match expected node id {} for conn {conn_id}",
            expected_peer_node_id.unwrap_or_default()
        ));
        return -1;
    }

    // Reserve the authenticated NodeId/session claim before any route, cluster,
    // registry, SWIM, monitor, link, or reply-table publication.
    let Some(claim_token) = next_publication_token(mgr) else {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: publication token space is exhausted (conn {conn_id})"
        ));
        return -1;
    };
    let superseded_claim: Option<LiveClaim> = match reserve_identity_claim(
        mgr,
        peer_hs.node_id,
        peer_route_slot,
        peer_hs.session_incarnation,
        Some(&peer_credential),
        conn_id,
        claim_token,
    ) {
        ClaimReservation::Reserved { superseded } => superseded,
        ClaimReservation::Rejected(detail) => {
            // SAFETY: mgr.transport and conn_id are valid per caller contract.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!("hew_connmgr_add: {detail}"));
            return -1;
        }
    };

    let mut actor = ConnectionActor::new(conn_id);
    actor.transport = mgr.transport;
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
    #[cfg(feature = "encryption")]
    if let Some(noise) = upgraded_noise {
        let Ok(mut guard) = actor.noise_transport.lock() else {
            // Policy: per-connection state (C-ABI) — poisoned noise transport
            // means this connection's encryption state is corrupted.
            set_last_error("hew_connmgr_add: noise_transport mutex poisoned (a thread panicked)");
            return -1;
        };
        *guard = Some(noise);
    }
    actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);

    // SAFETY: hew_now_ms has no preconditions.
    let now = unsafe { crate::clock::hew_now_ms() };
    actor.last_activity_ms.store(now, Ordering::Release);

    // I/O span: record accept span and stash context for reader_loop.
    // Fast path: atomic load only when tracing is disabled.
    crate::tracing::io_accept_span_begin(conn_id);

    // Spawn reader thread.
    let stop = Arc::clone(&actor.reader_stop);
    let transport_send = SendTransport(mgr.transport);
    let router = mgr.inbound_router;
    let activity_send = Arc::clone(&actor.last_activity_ms);
    let mgr_send = SendConnMgr(mgr_ptr);
    let peer_feature_flags = actor.peer_feature_flags;
    let reader_lifecycle_guard = mgr.reader_lifecycle.register();
    #[cfg(feature = "encryption")]
    let noise_transport = Arc::clone(&actor.noise_transport);

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
                #[cfg(feature = "encryption")]
                noise_transport,
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
        return -1;
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
            return -1;
        }
        Err(ConnectionInstallError::Duplicate) => {
            abort_identity_claim(mgr, peer_hs.node_id, conn_id, claim_token, superseded_claim);
            set_last_error(format!(
                "hew_connmgr_add: connection {conn_id} became duplicate during install"
            ));
            return -1;
        }
    };

    publish_identity_connection_established(
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
    );

    0
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
                ClaimState::Published => stashed_superseded,
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
            conn.publication_token,
            Arc::clone(&conn.publication_sync),
            Arc::clone(&conn.publication_removed),
            Arc::clone(&conn.claimed_send_lifecycle),
        ))
    });
    let Some((
        peer_node_id,
        peer_identity,
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
    // route-ok while hew_connmgr_send returns -1 (connection already gone
    // from the list). After the route is removed, any new routing lookup for
    // this peer returns no route, so no new hew_connmgr_send calls are
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
        ))
    });
    let Some((owns_close, transport_close)) = claim else {
        return 0;
    };
    if owns_close {
        // SAFETY: transport is valid per manager contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
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
    // Full publication retirement: re-removes route (idempotent) and fires the
    // cluster connection-lost notification. Deferred until after drop(conn) so
    // that a racing replacement connection can establish and publish first —
    // its higher publication token then suppresses this notification.
    retire_identity_connection_publication(
        mgr,
        peer_identity,
        peer_node_id,
        conn_id,
        publication_token,
        &publication_sync,
    );

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
