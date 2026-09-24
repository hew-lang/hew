//! `NodeId` claim reserve, publish, abort and retire lifecycle.

use super::*;

/// The publication token is the discriminator that makes
/// `(conn_id, publication_token)` an exact identity for one admission, and
/// transport `conn_id`s are recycled. A wrapping counter would eventually
/// reissue a token a paused teardown or supersede is still holding, and
/// that stale operation would then match an unrelated successor on the same
/// recycled id. So the generator must never reissue: it saturates and
/// refuses instead of wrapping, and the admission that cannot get a token
/// fails closed rather than proceeding with an ambiguous one.
///
/// Counterfactual: with the bare `fetch_add` back, the call at saturation
/// returns `Some(u64::MAX)` and the one after it hands out `0` again —
/// both assertions below fail.
#[test]
fn the_publication_token_never_wraps_back_onto_a_live_token() {
    let (mgr_ptr, transport) = claim_test_mgr();
    // SAFETY: the manager is live until `free_claim_test_mgr` below.
    let mgr = unsafe { &*mgr_ptr };

    let first = next_publication_token(mgr).expect("a fresh manager must issue a token");
    let second = next_publication_token(mgr).expect("a fresh manager must issue a token");
    assert!(
        second > first && first > 0,
        "tokens must be strictly increasing and never zero, got {first} then {second}"
    );

    // Park the generator at the end of the space.
    mgr.next_publication_token
        .store(u64::MAX, Ordering::Relaxed);
    assert_eq!(
        next_publication_token(mgr),
        None,
        "an exhausted generator must refuse rather than wrap"
    );
    assert_eq!(
        mgr.next_publication_token.load(Ordering::Relaxed),
        u64::MAX,
        "a refused issue must not advance the counter past the end"
    );
    assert_eq!(
        next_publication_token(mgr),
        None,
        "an exhausted generator must stay exhausted"
    );

    free_claim_test_mgr(mgr_ptr, transport);
}

// ---- issue #2652 · Slice 4 · NodeId claim state machine ----------------

/// Build a minimal manager for claim-machine unit tests: a stub transport
/// with all-`None` ops (its `close_conn` is a no-op) and no routing/cluster.
/// The claim helpers only touch `mgr.claims`, so this isolates the state
/// machine from routing/cluster side effects.
fn claim_test_mgr() -> (*mut HewConnMgr, *mut HewTransport) {
    let transport = Box::into_raw(Box::new(HewTransport {
        ops: std::ptr::null(),
        r#impl: std::ptr::null_mut(),
    }));
    // SAFETY: transport is a freshly-boxed valid pointer; routing/cluster null
    // is accepted by hew_connmgr_new.
    let mgr = unsafe {
        hew_connmgr_new(
            transport,
            None,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            1,
        )
    };
    assert!(!mgr.is_null());
    (mgr, transport)
}

fn free_claim_test_mgr(mgr: *mut HewConnMgr, transport: *mut HewTransport) {
    // SAFETY: both pointers were allocated in claim_test_mgr and are not
    // referenced after this call.
    unsafe {
        hew_connmgr_free(mgr);
        drop(Box::from_raw(transport));
    }
}

fn claim_snapshot(mgr: &HewConnMgr, node_id: u16) -> Option<LiveClaim> {
    mgr.claims
        .0
        .lock_or_recover()
        .get(&test_node_identity(node_id))
        .cloned()
}

#[test]
fn claim_reserve_publish_retire_exact_owner_lifecycle() {
    let (mgr_ptr, transport) = claim_test_mgr();
    // SAFETY: mgr_ptr is live until free below.
    let mgr = unsafe { &*mgr_ptr };
    let cred = Some(PeerCredential::NoiseKey([7u8; 32]));

    match reserve_claim(mgr, 42, cred.as_ref(), 100, 1) {
        ClaimReservation::Reserved { superseded } => assert!(superseded.is_none()),
        ClaimReservation::Rejected(d) => panic!("fresh reserve rejected: {d}"),
    }
    let reserved = claim_snapshot(mgr, 42).expect("claim should exist after reserve");
    assert_eq!(reserved.state, ClaimState::Reserved);
    assert_eq!(reserved.conn_id, 100);

    assert!(publish_claim(mgr, 42, 100, 1), "exact owner should publish");
    assert_eq!(
        claim_snapshot(mgr, 42).expect("claim persists").state,
        ClaimState::Published
    );

    assert!(retire_claim(mgr, 42, 100, 1), "exact owner should retire");
    assert_eq!(
        claim_snapshot(mgr, 42)
            .expect("retired claim remains as a replay fence")
            .state,
        ClaimState::Retired
    );
    free_claim_test_mgr(mgr_ptr, transport);
}

#[test]
fn retired_identity_claim_rejects_lower_session_and_allows_reconnect() {
    let (mgr_ptr, transport) = claim_test_mgr();
    // SAFETY: mgr_ptr is live until free below.
    let mgr = unsafe { &*mgr_ptr };
    let identity = test_node_identity(42);

    assert!(matches!(
        reserve_identity_claim(mgr, identity, 42, 7, None, 100, 1),
        ClaimReservation::Reserved { superseded: None }
    ));
    assert!(publish_identity_claim(mgr, identity, 100, 1));
    assert!(retire_identity_claim(mgr, identity, 100, 1));

    assert!(matches!(
        reserve_identity_claim(mgr, identity, 42, 6, None, 200, 2),
        ClaimReservation::Rejected(_)
    ));

    let equal_superseded = match reserve_identity_claim(mgr, identity, 42, 7, None, 200, 2) {
        ClaimReservation::Reserved {
            superseded: Some(retired),
        } => retired,
        ClaimReservation::Reserved { superseded: None } => {
            panic!("equal-session reconnect must retain the retired fence")
        }
        ClaimReservation::Rejected(detail) => {
            panic!("equal-session reconnect must be admitted: {detail}")
        }
    };
    assert_eq!(equal_superseded.state, ClaimState::Retired);
    abort_identity_claim(mgr, identity, 200, 2, Some(equal_superseded));

    assert!(matches!(
        reserve_identity_claim(mgr, identity, 42, 8, None, 300, 3),
        ClaimReservation::Reserved {
            superseded: Some(LiveClaim {
                state: ClaimState::Retired,
                ..
            })
        }
    ));

    free_claim_test_mgr(mgr_ptr, transport);
}

#[test]
fn claim_reserve_rejects_different_credential_over_published() {
    let (mgr_ptr, transport) = claim_test_mgr();
    // SAFETY: mgr_ptr live until free.
    let mgr = unsafe { &*mgr_ptr };
    let cred_a = Some(PeerCredential::NoiseKey([0xAA; 32]));
    let cred_b = Some(PeerCredential::NoiseKey([0xBB; 32]));

    // A owns 42, Published.
    assert!(matches!(
        reserve_claim(mgr, 42, cred_a.as_ref(), 100, 1),
        ClaimReservation::Reserved { .. }
    ));
    assert!(publish_claim(mgr, 42, 100, 1));

    // B presents a different credential for the same NodeId ⇒ reject.
    match reserve_claim(mgr, 42, cred_b.as_ref(), 200, 2) {
        ClaimReservation::Rejected(detail) => {
            assert!(detail.contains("another credential"), "detail: {detail}");
        }
        ClaimReservation::Reserved { .. } => {
            panic!("different-credential reserve must be rejected fail-closed")
        }
    }
    // A's claim is untouched.
    let still = claim_snapshot(mgr, 42).expect("A's claim persists");
    assert_eq!(still.conn_id, 100);
    assert_eq!(still.publication_token, 1);
    assert_eq!(still.state, ClaimState::Published);
    assert_eq!(still.credential, cred_a);
    free_claim_test_mgr(mgr_ptr, transport);
}

#[test]
fn claim_reserve_supersedes_same_credential_and_abort_restores_it() {
    let (mgr_ptr, transport) = claim_test_mgr();
    // SAFETY: mgr_ptr live until free.
    let mgr = unsafe { &*mgr_ptr };
    let cred = Some(PeerCredential::NoiseKey([0xCC; 32]));

    assert!(matches!(
        reserve_claim(mgr, 42, cred.as_ref(), 100, 1),
        ClaimReservation::Reserved { .. }
    ));
    assert!(publish_claim(mgr, 42, 100, 1));

    // Same-credential reconnect supersedes the live Published claim.
    let superseded = match reserve_claim(mgr, 42, cred.as_ref(), 200, 2) {
        ClaimReservation::Reserved { superseded } => {
            superseded.expect("same-credential reconnect supersedes the old claim")
        }
        ClaimReservation::Rejected(d) => panic!("same-credential reconnect rejected: {d}"),
    };
    assert_eq!(superseded.conn_id, 100);
    assert_eq!(superseded.state, ClaimState::Published);
    // Map now holds the new Reserved claim.
    let mid = claim_snapshot(mgr, 42).expect("new reservation present");
    assert_eq!(mid.conn_id, 200);
    assert_eq!(mid.state, ClaimState::Reserved);

    // Aborting the replacement restores the superseded Published claim —
    // never orphans it.
    abort_claim(mgr, 42, 200, 2, Some(superseded));
    let restored = claim_snapshot(mgr, 42).expect("superseded claim restored");
    assert_eq!(restored.conn_id, 100);
    assert_eq!(restored.state, ClaimState::Published);
    free_claim_test_mgr(mgr_ptr, transport);
}

#[test]
fn claim_abort_removes_fresh_reservation() {
    let (mgr_ptr, transport) = claim_test_mgr();
    // SAFETY: mgr_ptr live until free.
    let mgr = unsafe { &*mgr_ptr };
    assert!(matches!(
        reserve_claim(mgr, 7, None, 300, 5),
        ClaimReservation::Reserved { .. }
    ));
    abort_claim(mgr, 7, 300, 5, None);
    assert!(
        claim_snapshot(mgr, 7).is_none(),
        "aborting a fresh reservation leaves the map empty"
    );
    free_claim_test_mgr(mgr_ptr, transport);
}

#[test]
fn claim_retire_non_owner_removes_nothing() {
    let (mgr_ptr, transport) = claim_test_mgr();
    // SAFETY: mgr_ptr live until free.
    let mgr = unsafe { &*mgr_ptr };
    assert!(matches!(
        reserve_claim(mgr, 9, None, 100, 1),
        ClaimReservation::Reserved { .. }
    ));
    assert!(publish_claim(mgr, 9, 100, 1));
    // A different conn/token retires nothing and reports "not owner".
    assert!(!retire_claim(mgr, 9, 999, 42), "non-owner must not retire");
    assert!(
        claim_snapshot(mgr, 9).is_some(),
        "non-owner retire leaves the claim intact"
    );
    // The real owner retires.
    assert!(retire_claim(mgr, 9, 100, 1));
    assert_eq!(
        claim_snapshot(mgr, 9)
            .expect("retired claim remains as a replay fence")
            .state,
        ClaimState::Retired
    );
    free_claim_test_mgr(mgr_ptr, transport);
}

#[test]
fn claim_publish_aborts_when_superseded_in_reserve_window() {
    let (mgr_ptr, transport) = claim_test_mgr();
    // SAFETY: mgr_ptr live until free.
    let mgr = unsafe { &*mgr_ptr };
    let cred = Some(PeerCredential::NoiseKey([0xDD; 32]));

    // A reserves + publishes.
    assert!(matches!(
        reserve_claim(mgr, 42, cred.as_ref(), 100, 1),
        ClaimReservation::Reserved { .. }
    ));
    assert!(publish_claim(mgr, 42, 100, 1));

    // A2 supersedes (same credential) → map owner is now A2's reservation.
    assert!(matches!(
        reserve_claim(mgr, 42, cred.as_ref(), 200, 2),
        ClaimReservation::Reserved { .. }
    ));

    // A's (late) publish must abort — it is no longer the map owner.
    assert!(
        !publish_claim(mgr, 42, 100, 1),
        "a superseded connection's publish must abort"
    );
    // A2 still owns the reservation, still Reserved (its own publish pending).
    let owner = claim_snapshot(mgr, 42).expect("A2 reservation present");
    assert_eq!(owner.conn_id, 200);
    assert_eq!(owner.state, ClaimState::Reserved);
    free_claim_test_mgr(mgr_ptr, transport);
}

#[test]
fn claim_reserve_waits_on_reserved_then_proceeds_after_abort() {
    use std::sync::atomic::{AtomicBool, Ordering};
    let (mgr_ptr, transport) = claim_test_mgr();
    // A reserves node 42 (Reserved), does not yet publish or abort.
    {
        // SAFETY: mgr_ptr live until free.
        let mgr = unsafe { &*mgr_ptr };
        assert!(matches!(
            reserve_claim(mgr, 42, None, 100, 1),
            ClaimReservation::Reserved { .. }
        ));
    }

    let mgr_addr = mgr_ptr as usize;
    let b_reserved = Arc::new(AtomicBool::new(false));
    let b_reserved_thread = Arc::clone(&b_reserved);
    // B arrives for 42, observes Reserved, and must WAIT (not reject).
    let b = std::thread::spawn(move || {
        // SAFETY: mgr stays live until the test joins this thread.
        let mgr = unsafe { &*(mgr_addr as *mut HewConnMgr) };
        let outcome = reserve_claim(mgr, 42, None, 200, 2);
        b_reserved_thread.store(true, Ordering::Release);
        matches!(outcome, ClaimReservation::Reserved { .. })
    });

    // Give B time to reach the wait; it must not have returned yet.
    std::thread::sleep(std::time::Duration::from_millis(100));
    assert!(
        !b_reserved.load(Ordering::Acquire),
        "B must block while A's reservation is in flight"
    );

    // A aborts → signals the condvar → B wakes, sees absent, reserves.
    {
        // SAFETY: mgr_ptr live.
        let mgr = unsafe { &*mgr_ptr };
        abort_claim(mgr, 42, 100, 1, None);
    }
    let b_ok = b.join().expect("B thread should not panic");
    assert!(b_ok, "B should reserve after A aborts");
    // SAFETY: mgr_ptr live.
    let owner = claim_snapshot(unsafe { &*mgr_ptr }, 42).expect("B now owns 42");
    assert_eq!(owner.conn_id, 200);
    free_claim_test_mgr(mgr_ptr, transport);
}

/// Concurrent restoration of BLOCK-3: many admissions race for the same
/// `NodeId`; exactly one may hold a Published claim at a time.
#[test]
fn claim_concurrent_reserve_publish_yields_exactly_one_owner() {
    let (mgr_ptr, transport) = claim_test_mgr();
    let mgr_addr = mgr_ptr as usize;
    let winners = Arc::new(std::sync::Mutex::new(Vec::<c_int>::new()));
    let mut handles = Vec::new();
    for i in 0u8..8 {
        let winners = Arc::clone(&winners);
        handles.push(std::thread::spawn(move || {
            // SAFETY: mgr stays live until join below.
            let mgr = unsafe { &*(mgr_addr as *mut HewConnMgr) };
            let conn_id = 1000 + c_int::from(i);
            let token = 1000 + u64::from(i);
            let cred = Some(PeerCredential::NoiseKey([i; 32]));
            match reserve_claim(mgr, 42, cred.as_ref(), conn_id, token) {
                ClaimReservation::Reserved { .. } => {
                    if publish_claim(mgr, 42, conn_id, token) {
                        winners.lock_or_recover().push(conn_id);
                    }
                }
                ClaimReservation::Rejected(_) => {}
            }
        }));
    }
    for h in handles {
        h.join().expect("claim race thread should not panic");
    }
    // At most one Published owner at any time; whoever published is the sole
    // map owner. Distinct credentials mean losers were rejected, not queued.
    let winners = winners.lock_or_recover();
    assert!(
        winners.len() <= 1,
        "at most one connection may publish a NodeId claim, got {winners:?}"
    );
    // SAFETY: mgr_ptr live.
    let final_owner = claim_snapshot(unsafe { &*mgr_ptr }, 42);
    if let Some(owner) = final_owner {
        assert_eq!(owner.state, ClaimState::Published);
        if let Some(&w) = winners.first() {
            assert_eq!(owner.conn_id, w);
        }
    }
    drop(winners);
    free_claim_test_mgr(mgr_ptr, transport);
}
