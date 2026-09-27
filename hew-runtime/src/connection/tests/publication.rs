//! Publication stash restoration after removal and stale publishers.

use super::*;

/// Remove racing a JUST-PUBLISHED claim whose publication our removal
/// suppressed: the stashed predecessor claim must be restored after our
/// claim's retirement — the prior same-credential connection regains
/// authority instead of being orphaned claimless. (The publish path clears
/// the stash when publication completes, so this restore can never
/// resurrect a closed connection's claim.)
#[test]
fn remove_after_suppressed_publication_restores_superseded_claim() {
    with_reserved_claim(15, 95, 700, true, |mgr| {
        // SAFETY: mgr is live for the whole closure.
        unsafe {
            assert!(publish_claim(&*mgr, 15, 95, 700), "original must publish");

            // Same-credential reconnect: conn 96 reserves (superseding),
            // installs, and its claim reaches Published — but the cluster
            // publication is then SUPPRESSED by removal, so the stash is
            // never cleared.
            let superseded = match reserve_claim(&*mgr, 15, None, 96, 800) {
                ClaimReservation::Reserved { superseded } => {
                    superseded.expect("reconnect must supersede the published claim")
                }
                ClaimReservation::Rejected(detail) => {
                    panic!("same-credential reconnect must reserve: {detail}")
                }
            };
            install_strict_conn(&*mgr, 15, 96, 800);
            (*mgr).connections.access(|conns| {
                let conn = conns
                    .iter_mut()
                    .find(|c| c.conn_id == 96)
                    .expect("reconnect actor installed");
                *conn.superseded_claim.lock_or_recover() = Some(superseded);
            });
            assert!(
                publish_claim(&*mgr, 15, 96, 800),
                "reconnect claim publishes"
            );

            assert_eq!(hew_connmgr_remove(mgr, 96), 0, "remove must succeed");

            let (lock, _condvar) = &(*mgr).claims;
            let guard = lock.lock_or_recover();
            let restored = guard
                .get(&test_node_identity(15))
                .expect("predecessor claim must be restored");
            assert_eq!(restored.conn_id, 95);
            assert_eq!(restored.publication_token, 700);
            assert_eq!(restored.state, ClaimState::Published);
        }
    });
}

/// The counter-half: when the publication COMPLETES, the publish path
/// clears the stash (and closes the superseded connection), so a later
/// remove retains only the successor's retired replay fence — no resurrected
/// authority for a closed predecessor.
#[test]
fn remove_after_completed_publication_restores_nothing() {
    with_reserved_claim(16, 95, 900, true, |mgr| {
        // SAFETY: mgr is live for the whole closure.
        unsafe {
            assert!(publish_claim(&*mgr, 16, 95, 900), "original must publish");
            let superseded = match reserve_claim(&*mgr, 16, None, 96, 1000) {
                ClaimReservation::Reserved { superseded } => {
                    superseded.expect("reconnect must supersede the published claim")
                }
                ClaimReservation::Rejected(detail) => {
                    panic!("same-credential reconnect must reserve: {detail}")
                }
            };
            let mut actor = ConnectionActor::new(96);
            actor.peer_node_id = 16;
            actor.publication_token = 1000;
            actor.posture = crate::peer_binding::Posture::Strict;
            actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            *actor.superseded_claim.lock_or_recover() = Some(superseded.clone());
            let Ok(publication) = install_connection_actor(&*mgr, actor) else {
                panic!("install must succeed");
            };
            // Full publication (not suppressed): clears the stash.
            publish_connection_established(
                &*mgr,
                16,
                96,
                0,
                publication.token,
                &publication.sync,
                &publication.removed,
                Some(superseded),
            );

            assert_eq!(hew_connmgr_remove(mgr, 96), 0, "remove must succeed");

            let (lock, _condvar) = &(*mgr).claims;
            let guard = lock.lock_or_recover();
            let retired = guard
                .get(&test_node_identity(16))
                .expect("completed publication must retain a replay fence");
            assert_eq!(retired.conn_id, 96);
            assert_eq!(retired.state, ClaimState::Retired);
        }
    });
}

/// A publication completing late after its actor was removed must not
/// clear the predecessor stash of a successor that recycled the same
/// transport `conn_id`.
///
/// Counterfactual: a bare `conn_id` lookup finds the successor and takes
/// its stash on the first call below, so the retention assertion fails.
#[test]
fn stale_publisher_cannot_clear_reused_conn_successor_stash() {
    const CONN: c_int = 96;
    const STALE_TOKEN: u64 = 1_000;
    const SUCCESSOR_TOKEN: u64 = 1_100;

    with_reserved_claim(17, CONN, SUCCESSOR_TOKEN, true, |mgr| {
        // SAFETY: the manager is live for the whole closure.
        let mgr = unsafe { &*mgr };
        let predecessor = LiveClaim {
            credential: None,
            route_slot: 17,
            session_incarnation: 1,
            conn_id: 95,
            publication_token: 900,
            state: ClaimState::Published,
        };
        mgr.connections.access(|connections| {
            let successor = connections
                .iter()
                .find(|connection| {
                    connection.conn_id == CONN && connection.publication_token == SUCCESSOR_TOKEN
                })
                .expect("successor admission is installed");
            *successor.superseded_claim.lock_or_recover() = Some(predecessor);
        });

        clear_superseded_claim_if_current(mgr, CONN, STALE_TOKEN);
        mgr.connections.access(|connections| {
            let successor = connections
                .iter()
                .find(|connection| connection.conn_id == CONN)
                .expect("successor admission remains installed");
            let stash = successor.superseded_claim.lock_or_recover();
            assert_eq!(
                stash.as_ref().map(|claim| claim.conn_id),
                Some(95),
                "a stale publication token must not clear the successor's stash"
            );
        });

        clear_superseded_claim_if_current(mgr, CONN, SUCCESSOR_TOKEN);
        mgr.connections.access(|connections| {
            let successor = connections
                .iter()
                .find(|connection| connection.conn_id == CONN)
                .expect("successor admission remains installed");
            assert!(
                successor.superseded_claim.lock_or_recover().is_none(),
                "the exact successor publication must clear its own stash"
            );
        });
    });
}

/// A refusal belongs to one exact `(conn_id, publication_token)`
/// admission. If its actor has already gone and a successor recycled the
/// numeric id, the stale refusal must leave that successor installed and
/// leave its authority claim untouched.
///
/// Counterfactual: routing the refusal through ordinary bare-`conn_id`
/// removal unlinks the successor, marks its publication removed, and
/// retires its claim; every assertion below fails.
#[test]
fn stale_refusal_cannot_remove_reused_conn_successor() {
    const ROUTE_SLOT: u16 = 18;
    const CONN: c_int = 97;
    const STALE_TOKEN: u64 = 1_000;
    const SUCCESSOR_TOKEN: u64 = 1_100;

    with_reserved_claim(ROUTE_SLOT, CONN, SUCCESSOR_TOKEN, true, |mgr| {
        // SAFETY: the manager is live for the whole closure.
        let mgr_ref = unsafe { &*mgr };
        refuse_established_publication(mgr_ref, ROUTE_SLOT, CONN, STALE_TOKEN);

        assert_eq!(
            // SAFETY: the manager is still live.
            unsafe { hew_connmgr_count(mgr) },
            1,
            "a stale refusal must not unlink the recycled-id successor"
        );
        mgr_ref.connections.access(|connections| {
            let successor = connections
                .iter()
                .find(|connection| {
                    connection.conn_id == CONN && connection.publication_token == SUCCESSOR_TOKEN
                })
                .expect("the exact successor admission must remain installed");
            assert_eq!(
                successor.state.load(Ordering::Acquire),
                CONN_STATE_ACTIVE,
                "the stale refusal must not close the successor"
            );
            assert!(
                !successor.publication_removed.load(Ordering::Acquire),
                "the stale refusal must not cancel the successor publication"
            );
        });

        let (claims, _changed) = &mgr_ref.claims;
        let claims = claims.lock_or_recover();
        let successor_claim = claims
            .get(&test_node_identity(ROUTE_SLOT))
            .expect("the successor claim must remain live");
        assert_eq!(successor_claim.conn_id, CONN);
        assert_eq!(successor_claim.publication_token, SUCCESSOR_TOKEN);
        assert_eq!(successor_claim.state, ClaimState::Reserved);
    });
}
