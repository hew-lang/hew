use super::*;

struct BlockingMembershipState {
    events: Mutex<Vec<(u16, u8)>>,
    suspect_seen: std::sync::mpsc::Sender<()>,
    release: Mutex<std::sync::mpsc::Receiver<()>>,
    blocked_first_suspect: AtomicBool,
}

extern "C" fn block_on_first_suspect(node_id: u16, event: u8, user_data: *mut c_void) {
    // SAFETY: the test keeps the boxed callback state alive until the delivery
    // thread has joined.
    let state = unsafe { &*user_data.cast::<BlockingMembershipState>() };
    state
        .events
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .push((node_id, event));
    if event == HEW_MEMBERSHIP_EVENT_NODE_SUSPECT
        && !state.blocked_first_suspect.swap(true, Ordering::AcqRel)
    {
        state
            .suspect_seen
            .send(())
            .expect("suspect callback should notify the test");
        state
            .release
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .recv_timeout(std::time::Duration::from_secs(5))
            .expect("the test should release the blocked callback");
    }
}

struct SendCluster(*const HewCluster);

// SAFETY: the test keeps the cluster alive until the worker thread joins.
unsafe impl Send for SendCluster {}

impl SendCluster {
    fn notify_connection_lost_if_current(&self, node_id: u16, token: u64) {
        // SAFETY: the test retains the cluster until this worker is joined.
        unsafe { (&*self.0).notify_connection_lost_if_current(node_id, token) };
    }
}

#[expect(
    clippy::type_complexity,
    reason = "compare the existing callback and gossip tuple shapes in one snapshot"
)]
fn terminal_transition_result(
    new_session: bool,
    same_session_reconnect: bool,
) -> (Vec<(u16, u8)>, Vec<(u16, Option<u32>, i32, u64)>) {
    let cluster = HewCluster::new(make_config(1));
    let (suspect_tx, suspect_rx) = std::sync::mpsc::channel();
    let (release_tx, release_rx) = std::sync::mpsc::channel();
    let callback_state = Box::into_raw(Box::new(BlockingMembershipState {
        events: Mutex::new(Vec::new()),
        suspect_seen: suspect_tx,
        release: Mutex::new(release_rx),
        blocked_first_suspect: AtomicBool::new(false),
    }));
    *cluster.membership_callback_binding.lock_or_recover() =
        MembershipCallbackBinding::new(Some(block_on_first_suspect), callback_state.cast());

    cluster.sessions.lock_or_recover().insert(2, 1);
    cluster
        .connection_tokens
        .lock_or_recover()
        .current
        .insert(2, 1);
    cluster.upsert_member(2, MEMBER_ALIVE, 1, b"peer");

    let (lost_done_tx, lost_done_rx) = std::sync::mpsc::channel();
    let lost_cluster = SendCluster(std::ptr::from_ref(&cluster));
    let lost = std::thread::spawn(move || {
        lost_cluster.notify_connection_lost_if_current(2, 1);
        lost_done_tx.send(()).expect("loss thread should finish");
    });
    suspect_rx
        .recv_timeout(std::time::Duration::from_secs(1))
        .expect("the drainer should block in the first SUSPECT callback");

    // This Plain DEAD is queued behind the blocked callback while the table is
    // already DEAD. A higher authenticated session then reuses the route slot
    // and starts its SWIM incarnation domain at one.
    cluster.upsert_member(2, MEMBER_DEAD, 1, &[]);
    if new_session {
        let publication_sync = Arc::new(Mutex::new(()));
        let publication_removed = Arc::new(AtomicBool::new(false));
        // SAFETY: the cluster remains live and the publication handles are
        // retained for the duration of this synchronous admission call.
        let result = unsafe {
            hew_cluster_notify_connection_established_for_token_if_not_removed(
                std::ptr::from_ref(&cluster).cast_mut(),
                2,
                2,
                2,
                &publication_sync,
                &publication_removed,
            )
        };
        assert_eq!(result, 1, "the higher session should be admitted");
    } else if same_session_reconnect {
        // A same-session, higher-incarnation ALIVE readmits this member while
        // the old DEAD is still queued. Then publish and lose a new connection
        // so the publication value returns to None (the None-ABA boundary).
        cluster.apply_swim_gossip(&[(2, 1, MEMBER_ALIVE, 2)]);
        let publication_sync = Arc::new(Mutex::new(()));
        let publication_removed = Arc::new(AtomicBool::new(false));
        // SAFETY: the cluster and publication handles remain live for both calls.
        let result = unsafe {
            hew_cluster_notify_connection_established_for_token_if_not_removed(
                std::ptr::from_ref(&cluster).cast_mut(),
                2,
                1,
                2,
                &publication_sync,
                &publication_removed,
            )
        };
        assert_eq!(result, 1, "the same-session connection should publish");
        cluster.notify_connection_lost_if_current(2, 2);
    }

    release_tx
        .send(())
        .expect("the blocked callback should be released");
    lost_done_rx
        .recv_timeout(std::time::Duration::from_secs(1))
        .expect("the blocked drainer should finish after release");
    lost.join().expect("loss thread should not panic");

    // SAFETY: callback state remains boxed until after the snapshot.
    let events = unsafe { &*callback_state }
        .events
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .clone();
    // SAFETY: no transition delivery can still reference the callback state.
    unsafe { drop(Box::from_raw(callback_state)) };

    if new_session || same_session_reconnect {
        assert!(
            !events.iter().any(|(_, event)| *event == HEW_MEMBERSHIP_EVENT_NODE_DEAD),
            "an old-session DEAD queued behind a blocked callback must not be delivered after rejoin"
        );
    } else {
        assert!(
            events
                .iter()
                .any(|(_, event)| *event == HEW_MEMBERSHIP_EVENT_NODE_DEAD),
            "same-session SUSPECT→DEAD history must still be delivered in FIFO order"
        );
    }

    let gossip = cluster.take_swim_gossip(8);
    (events, gossip)
}

#[test]
fn queued_plain_dead_is_superseded_by_higher_session() {
    let (events, gossip) = terminal_transition_result(true, false);
    assert!(events
        .iter()
        .any(|(_, event)| *event == HEW_MEMBERSHIP_EVENT_NODE_SUSPECT));
    assert!(
        gossip.iter().all(|(_, _, state, _)| *state != MEMBER_DEAD),
        "an obsolete DEAD must not enter current-session gossip"
    );
}

#[test]
fn queued_plain_dead_preserves_same_session_fifo_history() {
    let (events, gossip) = terminal_transition_result(false, false);
    let suspect = events
        .iter()
        .position(|(_, event)| *event == HEW_MEMBERSHIP_EVENT_NODE_SUSPECT)
        .expect("same-session SUSPECT should be delivered");
    let dead = events
        .iter()
        .position(|(_, event)| *event == HEW_MEMBERSHIP_EVENT_NODE_DEAD)
        .expect("same-session DEAD should be delivered");
    assert!(suspect < dead, "same-session notifications remain FIFO");
    assert!(
        gossip
            .iter()
            .any(|(_, session, state, _)| *session == Some(1) && *state == MEMBER_DEAD),
        "same-session DEAD history remains gossipable"
    );
}

#[test]
fn queued_plain_dead_is_superseded_after_same_session_none_aba() {
    let (events, gossip) = terminal_transition_result(false, true);
    assert!(
        !events
            .iter()
            .any(|(_, event)| *event == HEW_MEMBERSHIP_EVENT_NODE_DEAD),
        "incarnation 1 DEAD must not overtake same-session ALIVE at incarnation 2"
    );
    assert!(
        gossip
            .iter()
            .all(|(_, _, state, incarnation)| *state != MEMBER_DEAD || *incarnation != 1),
        "obsolete incarnation 1 DEAD must not enter current gossip after publication returns to None"
    );
}

#[test]
fn higher_session_gossip_retires_predecessor_publication() {
    let cluster = HewCluster::new(make_config(1));
    cluster.sessions.lock_or_recover().insert(2, 1);
    {
        let mut tokens = cluster.connection_tokens.lock_or_recover();
        tokens.current.insert(2, 10);
        tokens.visible.insert(2, 10);
        tokens.pending_emissions.insert(2, 10);
    }
    cluster.upsert_member(2, MEMBER_ALIVE, 1, b"peer");

    cluster.apply_swim_gossip(&[(2, 2, MEMBER_ALIVE, 1)]);
    assert_eq!(cluster.sessions.lock_or_recover().get(&2).copied(), Some(2));
    {
        let tokens = cluster.connection_tokens.lock_or_recover();
        assert!(!tokens.current.contains_key(&2));
        assert!(!tokens.visible.contains_key(&2));
        assert!(!tokens.pending_emissions.contains_key(&2));
    }

    cluster.apply_swim_gossip(&[(2, 2, MEMBER_SUSPECT, 2)]);
    assert!(
        !cluster
            .connection_tokens
            .lock_or_recover()
            .current
            .contains_key(&2),
        "later gossip in the new session must still have no authenticated publication"
    );
}
