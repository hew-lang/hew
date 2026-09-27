//! Admission waits on reserved `NodeId` claims.

use super::*;

/// Shared scaffold for the admission-window (`Reserved → Published`) gate
/// tests: a manager with one Strict ACTIVE connection whose claim is still
/// `Reserved` by that connection, modelling the reader thread starting
/// before `publish_connection_established` resolves the claim.
fn with_reserved_strict_conn(
    node_id: u16,
    conn_id: c_int,
    token: u64,
    body: impl FnOnce(*mut HewConnMgr),
) {
    with_reserved_claim(node_id, conn_id, token, true, body);
}

/// Admission-window race (the lookup-unresolved CI failure): a control frame
/// arriving while this connection's OWN claim is still `Reserved` must WAIT
/// for the local publication and then be granted — not be dropped. The
/// non-waiting gate denies inside the window (that instant deny permanently
/// lost the peer's one-shot registry-gossip flush); the waiting gate blocks
/// on the claims condvar until `publish_claim` fires and then grants.
#[test]
fn reserved_claim_same_conn_wait_grants_after_publish() {
    with_reserved_strict_conn(5, 30, 77, |mgr| {
        // SAFETY: mgr is live for the whole closure.
        unsafe {
            // Pre-fix behaviour: the non-waiting gate denies mid-window.
            assert_eq!(
                authenticated_peer_node_id_for_conn(&*mgr, 30),
                0,
                "a Reserved claim carries no authority in the non-waiting gate"
            );
            let waiter = SendConnMgr(mgr);
            let handle = std::thread::spawn(move || {
                let waiter = waiter;
                // SAFETY: manager outlives the join below.
                wait_authenticated_peer_node_id_for_conn(&*waiter.0, 30, 77)
            });
            // Give the waiter time to reach the condvar wait, then publish —
            // modelling `publish_connection_established` completing.
            std::thread::sleep(Duration::from_millis(50));
            assert!(publish_claim(&*mgr, 5, 30, 77), "publication must be ours");
            let granted = handle.join().expect("waiter thread");
            assert_eq!(
                granted, 5,
                "the waiting gate must grant once the local claim publishes"
            );
        }
    });
}

/// Fail-closed half of the admission-window wait: when the mid-window
/// admission ABORTS (install failure), the waiter must wake and deny — the
/// wait never fabricates authority for a connection that was never admitted.
#[test]
fn reserved_claim_same_conn_wait_denies_after_abort() {
    with_reserved_strict_conn(6, 40, 88, |mgr| {
        // SAFETY: mgr is live for the whole closure.
        unsafe {
            let waiter = SendConnMgr(mgr);
            let handle = std::thread::spawn(move || {
                let waiter = waiter;
                // SAFETY: manager outlives the join below.
                wait_authenticated_peer_node_id_for_conn(&*waiter.0, 40, 88)
            });
            std::thread::sleep(Duration::from_millis(50));
            abort_claim(&*mgr, 6, 40, 88, None);
            let granted = handle.join().expect("waiter thread");
            assert_eq!(
                granted, 0,
                "an aborted admission must deny the waiting gate (fail-closed)"
            );
        }
    });
}

/// The wait applies ONLY to this connection's own in-flight admission: a
/// claim `Reserved` by a DIFFERENT connection (a superseding admission) is
/// denied immediately — no blocking, no authority (D3 point 2 preserved).
#[test]
fn reserved_claim_other_conn_denied_without_wait() {
    with_reserved_strict_conn(7, 50, 99, |mgr| {
        // SAFETY: mgr is live for the whole closure.
        unsafe {
            // A second Strict ACTIVE conn 51 for the same node id, with NO
            // claim of its own — node 7's claim is Reserved by conn 50.
            let mut other = ConnectionActor::new(51);
            other.peer_node_id = 7;
            other.posture = crate::peer_binding::Posture::Strict;
            other.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(other));

            let started = std::time::Instant::now();
            assert_eq!(
                wait_authenticated_peer_node_id_for_conn(&*mgr, 51, 100),
                0,
                "a claim reserved by another connection must deny immediately"
            );
            assert!(
                started.elapsed() < Duration::from_millis(CLAIM_RESERVE_WAIT_MS / 2),
                "the other-owner denial must not consume the wait budget"
            );
        }
    });
}

/// Pre-install admission window (the residual lookup-unresolved race):
/// `hew_connmgr_add` spawns the reader thread BEFORE
/// `install_connection_actor`, so a frame can be gated while the claim is
/// `Reserved` by this connection and the connections list does not yet
/// contain the connection at all. The gate must WAIT (keyed on the claim,
/// which reserve placed before the spawn), then grant once the install +
/// publication complete — a connections-list-first gate denies here and
/// permanently drops the peer's one-shot registry-gossip flush.
#[test]
fn reserved_claim_before_install_wait_grants_after_install_and_publish() {
    with_reserved_claim(8, 60, 111, false, |mgr| {
        // SAFETY: mgr is live for the whole closure.
        unsafe {
            let waiter = SendConnMgr(mgr);
            let handle = std::thread::spawn(move || {
                let waiter = waiter;
                // SAFETY: manager outlives the join below.
                wait_authenticated_peer_node_id_for_conn(&*waiter.0, 60, 111)
            });
            // Give the waiter time to reach the condvar wait, then install
            // and publish — modelling `install_connection_actor` followed
            // by `publish_connection_established`.
            std::thread::sleep(Duration::from_millis(50));
            install_strict_conn(&*mgr, 8, 60, 111);
            assert!(publish_claim(&*mgr, 8, 60, 111), "publication must be ours");
            let granted = handle.join().expect("waiter thread");
            assert_eq!(
                granted, 8,
                "the waiting gate must grant once install + publication complete"
            );
        }
    });
}

/// Fail-closed half of the pre-install window: when the admission aborts
/// before the connection is ever installed, the waiter must wake and deny —
/// the wait never fabricates authority for a connection that was never
/// admitted, installed, or published.
#[test]
fn reserved_claim_before_install_wait_denies_after_abort() {
    with_reserved_claim(9, 70, 122, false, |mgr| {
        // SAFETY: mgr is live for the whole closure.
        unsafe {
            let waiter = SendConnMgr(mgr);
            let handle = std::thread::spawn(move || {
                let waiter = waiter;
                // SAFETY: manager outlives the join below.
                wait_authenticated_peer_node_id_for_conn(&*waiter.0, 70, 122)
            });
            std::thread::sleep(Duration::from_millis(50));
            abort_claim(&*mgr, 9, 70, 122, None);
            let granted = handle.join().expect("waiter thread");
            assert_eq!(
                granted, 0,
                "an admission aborted pre-install must deny the waiting gate"
            );
        }
    });
}

/// Generation separation on a REUSED transport `conn_id`: the TCP transport
/// recycles slot indices (`store_conn` hands out the first free slot), so a
/// stale reader — one still processing an already-read frame after its
/// connection was removed — can observe a SUCCESSOR admission's claim under
/// the same `conn_id`. The stale reader's gate carries its own (older)
/// publication token and must deny immediately: it never waits on, and
/// never adopts, the successor's authority, whether the successor's claim
/// is still Reserved or already Published.
#[test]
fn stale_token_on_reused_conn_id_denied_without_wait() {
    with_reserved_strict_conn(11, 80, 200, |mgr| {
        // SAFETY: mgr is live for the whole closure.
        unsafe {
            // Successor mid-admission (claim Reserved, token 200): the stale
            // reader (token 150) must deny without consuming the wait budget.
            let started = std::time::Instant::now();
            assert_eq!(
                wait_authenticated_peer_node_id_for_conn(&*mgr, 80, 150),
                0,
                "a stale reader must not wait on a successor's Reserved claim"
            );
            assert!(
                started.elapsed() < Duration::from_millis(CLAIM_RESERVE_WAIT_MS / 2),
                "the stale-token denial must not consume the wait budget"
            );

            // Successor fully admitted (claim Published, token 200): the
            // stale reader still denies; the successor itself grants.
            assert!(
                publish_claim(&*mgr, 11, 80, 200),
                "publication must be ours"
            );
            assert_eq!(
                wait_authenticated_peer_node_id_for_conn(&*mgr, 80, 150),
                0,
                "a stale reader must not adopt a successor's published authority"
            );
            assert_eq!(
                wait_authenticated_peer_node_id_for_conn(&*mgr, 80, 200),
                11,
                "the successor's own gate must grant after its publication"
            );
        }
    });
}

/// Removal racing publication must resolve the admission wait PROMPTLY —
/// pinned with a REAL joined reader: the installed actor's `reader_handle`
/// IS a thread parked in `wait_authenticated_peer_node_id_for_conn`, and
/// `hew_connmgr_remove` JOINS it (via the actor drop). Without the
/// pre-join claim abort the join blocks for the full
/// `CLAIM_RESERVE_WAIT_MS` backstop and the wall-clock assertion fails;
/// with it the abort wakes the parked reader to a fail-closed deny and
/// remove returns promptly. Also pins the cleanup: the aborted
/// reservation leaves no dangling claim to wedge a reconnect's
/// `reserve_claim`.
#[test]
fn remove_during_admission_wait_resolves_promptly_and_cleans_claim() {
    with_reserved_claim(12, 90, 300, false, |mgr| {
        // SAFETY: mgr is live for the whole closure.
        unsafe {
            let (started_tx, started_rx) = std::sync::mpsc::channel();
            let (result_tx, result_rx) = std::sync::mpsc::channel();
            let waiter = SendConnMgr(mgr);
            let reader = std::thread::spawn(move || {
                let waiter = waiter;
                started_tx.send(()).expect("reader start signal");
                // SAFETY: the manager outlives the join hew_connmgr_remove
                // performs on this thread.
                let granted = wait_authenticated_peer_node_id_for_conn(&*waiter.0, 90, 300);
                result_tx.send(granted).expect("reader result");
            });

            // Install the actor with the parked thread as its REAL reader
            // handle, exactly what hew_connmgr_remove must join.
            let mut actor = ConnectionActor::new(90);
            actor.peer_node_id = 12;
            actor.peer_identity = Some(test_node_identity(12));
            actor.peer_session_incarnation = 1;
            actor.publication_token = 300;
            actor.posture = crate::peer_binding::Posture::Strict;
            actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            actor.reader_handle = Some(reader);
            (*mgr).connections.access(|conns| conns.push(actor));

            started_rx.recv().expect("reader thread started");
            // Scheduling grace so the reader reaches the condvar wait; the
            // verdict is interleaving-independent (an un-parked reader
            // sees the aborted claim and denies just the same).
            std::thread::sleep(Duration::from_millis(50));

            let remove_started = std::time::Instant::now();
            assert_eq!(hew_connmgr_remove(mgr, 90), 0, "remove must succeed");
            let elapsed = remove_started.elapsed();
            assert!(
                elapsed < Duration::from_millis(CLAIM_RESERVE_WAIT_MS / 2),
                "remove (which joins the parked reader) must resolve at the \
                 abort, not the {CLAIM_RESERVE_WAIT_MS} ms backstop; took {elapsed:?}"
            );
            let granted = result_rx.recv().expect("reader result delivered");
            assert_eq!(
                granted, 0,
                "removal aborting the reservation must deny the waiting gate"
            );
            let (lock, _condvar) = &(*mgr).claims;
            assert!(
                lock.lock_or_recover()
                    .get(&test_node_identity(12))
                    .is_none(),
                "the aborted reservation must leave no dangling claim"
            );
        }
    });
}

/// The remove-side abort restores the same-credential claim this admission
/// superseded (D3): when a reconnect's admission is removed before its
/// publication, the PREVIOUS owner's `Published` claim returns to the map
/// — authority falls back to the still-live prior connection instead of
/// evaporating.
#[test]
fn remove_before_publication_restores_superseded_claim() {
    with_reserved_claim(13, 95, 400, true, |mgr| {
        // SAFETY: mgr is live for the whole closure.
        unsafe {
            // The original connection 95 is fully admitted (Published).
            assert!(publish_claim(&*mgr, 13, 95, 400), "original must publish");

            // A same-credential reconnect (conn 96) reserves, superseding
            // the published claim, and installs — but is removed before
            // `publish_connection_established` runs.
            let superseded = match reserve_claim(&*mgr, 13, None, 96, 500) {
                ClaimReservation::Reserved { superseded } => {
                    superseded.expect("reconnect must supersede the published claim")
                }
                ClaimReservation::Rejected(detail) => {
                    panic!("same-credential reconnect must reserve: {detail}")
                }
            };
            install_strict_conn(&*mgr, 13, 96, 500);
            (*mgr).connections.access(|conns| {
                let conn = conns
                    .iter_mut()
                    .find(|c| c.conn_id == 96)
                    .expect("reconnect actor installed");
                *conn.superseded_claim.lock_or_recover() = Some(superseded);
            });

            assert_eq!(hew_connmgr_remove(mgr, 96), 0, "remove must succeed");

            let (lock, _condvar) = &(*mgr).claims;
            let guard = lock.lock_or_recover();
            let restored = guard
                .get(&test_node_identity(13))
                .expect("superseded claim must be restored");
            assert_eq!(
                restored.conn_id, 95,
                "restored claim owner is the original conn"
            );
            assert_eq!(restored.publication_token, 400);
            assert_eq!(restored.state, ClaimState::Published);
        }
    });
}

/// Issue #2655: a connection's monitor/link retirement fan-out must honour
/// the publication token. `retire_connection_publication` fires
/// `fan_out_monitor_lost_for_node` (arming `MonitorLost` for every pending
/// cross-node watcher of the peer) ONLY when the retiring connection is the
/// current publication owner. A superseded connection — one whose claim was
/// overwritten by a same-credential reconnect while the peer stays live under
/// the newer claim — retires nothing and must NOT drive the loss fan-out: the
/// peer is still reachable through its healthy replacement, so a `MonitorLost`
/// there is a false loss.
///
/// This drives `retire_connection_publication` directly against a registered
/// watcher — the observation point is connection-level (a superseded vs owning
/// retire of ONE peer's claim), which the §14 `sim_transport_property` harness
/// cannot express because it is transport-seam-only and brings up no `HewNode`.
/// A non-owner retire leaves the watcher pending; the subsequent owner retire
/// arms it, proving the gate discriminates on ownership, not merely that the
/// fan-out can fire.
#[test]
fn retire_fans_out_monitor_lost_only_for_the_owning_connection() {
    let _rt_guard = crate::runtime_test_guard();
    let rt = crate::runtime::rt_current_opt().expect("test guard installs a runtime");
    let peer: u16 = 42;
    let target = test_location(peer, 7);
    let _ref_id = rt
        .monitors
        .register_remote_monitor(target, 999)
        .expect("test monitor id");

    with_reserved_claim(peer, 95, 400, true, |mgr| {
        // SAFETY: mgr is live for the whole closure.
        unsafe {
            assert!(
                publish_claim(&*mgr, peer, 95, 400),
                "the owning connection's claim must publish"
            );
            let publication_sync = Arc::new(Mutex::new(()));

            // A superseded (non-owner) retire: `retire_claim` denies, so the
            // fan-out never runs and the watcher stays pending.
            retire_connection_publication(&*mgr, peer, 96, 500, &publication_sync);
            assert_eq!(
                rt.monitors.pending_observation_count(),
                1,
                "a superseded (non-owner) connection's retire must NOT fan out \
                 MonitorLost while the peer is live under a newer claim"
            );

            // The owning connection's retire: `retire_claim` grants, so the
            // fan-out arms the pending watcher with MonitorLost.
            retire_connection_publication(&*mgr, peer, 95, 400, &publication_sync);
            assert_eq!(
                rt.monitors.pending_observation_count(),
                0,
                "the owning connection's retire must fan out MonitorLost"
            );
        }
    });
}
