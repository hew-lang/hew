//! Retention of buried members: windows, the cap, readmission and rejoin.

use super::*;

const DEAD_WINDOW_MS: u64 = 3_000 * DEAD_RETENTION_SUSPECT_TIMEOUTS;
const LEFT_WINDOW_MS: u64 = 3_000 * LEFT_RETENTION_SUSPECT_TIMEOUTS;

/// Admit `slot` in `session`, then bury it in `state` at `incarnation`.
fn bury(cluster: &HewCluster, slot: u16, session: u32, state: i32, incarnation: u64) {
    cluster.sessions.lock_or_recover().insert(slot, session);
    cluster.upsert_member(slot, MEMBER_ALIVE, 1, &[]);
    cluster.update_last_seen(slot, 1);
    cluster.upsert_member(slot, state, incarnation, &[]);
    assert_eq!(cluster.member_state(slot), state);
}

fn buried_count(cluster: &HewCluster) -> usize {
    cluster
        .members
        .lock_or_recover()
        .iter()
        .filter(|member| member.is_buried())
        .count()
}

fn publish(cluster: &HewCluster, slot: u16, session: u32, token: u64) -> c_int {
    let sync = Arc::new(Mutex::new(()));
    let removed = Arc::new(AtomicBool::new(false));
    // SAFETY: the test owns the live cluster for the call.
    unsafe {
        hew_cluster_notify_connection_established_for_token_if_not_removed(
            std::ptr::from_ref(cluster).cast_mut(),
            slot,
            session,
            token,
            &sync,
            &removed,
        )
    }
}

/// A DEAD member is forgotten once its window has passed, with every fact
/// kept for its slot. Counterfactual: without retirement the entry, its
/// session and its gossip stay for the life of the node.
#[test]
fn dead_member_is_retired_after_its_window() {
    let cluster = HewCluster::new(make_config(1));
    bury(&cluster, 2, 7, MEMBER_DEAD, 3);
    bury(&cluster, 3, 4, MEMBER_LEFT, 2);
    let start = 1_000;

    assert!(cluster.retire_buried(start).is_empty());
    assert_eq!(
        cluster.retire_buried(start + LEFT_WINDOW_MS),
        vec![RetiredMember {
            route_slot: 3,
            session: Some(4),
        }],
        "a member that left goes after its short grace"
    );
    assert!(cluster.retire_buried(start + DEAD_WINDOW_MS - 1).is_empty());
    assert_eq!(cluster.member_state(2), MEMBER_DEAD);
    // Open indirect probes that name slot 2 as target or as peer, and one
    // between live members.
    let probe_time = start + DEAD_WINDOW_MS;
    cluster.await_indirect_ack(2, 5, probe_time);
    cluster.await_indirect_ack(5, 2, probe_time);
    cluster.note_ping_req_sent_at(2, 6, probe_time);
    cluster.note_ping_req_sent_at(6, 2, probe_time);
    cluster.note_ping_req_sent_at(6, 5, probe_time);

    assert_eq!(
        cluster.retire_buried(start + DEAD_WINDOW_MS),
        vec![RetiredMember {
            route_slot: 2,
            session: Some(7),
        }]
    );
    assert_eq!(cluster.member_state(2), -1);
    assert_eq!(cluster.member_session(2), None);
    assert!(cluster.detectors.lock_or_recover().is_empty());
    assert!(cluster.events.lock_or_recover().is_empty());
    assert!(cluster.relayed_probes.lock_or_recover().is_empty());
    let requested = cluster.requested_probes.lock_or_recover();
    assert_eq!(requested.len(), 1);
    assert_eq!((requested[0].target, requested[0].peer), (5, 6));
    drop(requested);
    let tokens = cluster.connection_tokens.lock_or_recover();
    assert!(tokens.announced.is_empty() && tokens.visible.is_empty());
}

/// Churn through 1000 node ids that never return keeps at most the cap of
/// buried entries, oldest retired first. Counterfactual: every id stays.
#[test]
fn buried_cap_holds_under_churn_of_1000_ids() {
    let cluster = HewCluster::new(make_config(1));
    for (offset, slot) in (2..1002_u16).enumerate() {
        bury(&cluster, slot, 1, MEMBER_DEAD, 2);
        cluster.retire_buried(offset as u64);
        assert!(buried_count(&cluster) <= MAX_BURIED_MEMBERS);
    }
    assert_eq!(buried_count(&cluster), MAX_BURIED_MEMBERS);
    assert_eq!(cluster.member_state(2), -1, "the oldest burial goes first");
    assert_eq!(cluster.member_state(1001), MEMBER_DEAD);
    assert!(cluster.sessions.lock_or_recover().len() <= MAX_BURIED_MEMBERS);
    assert!(cluster.detectors.lock_or_recover().len() <= MAX_BURIED_MEMBERS);
    assert!(cluster.events.lock_or_recover().len() <= MAX_GOSSIP_EVENTS);
    let tokens = cluster.connection_tokens.lock_or_recover();
    assert!(tokens.announced.len() <= MAX_BURIED_MEMBERS);
}

/// A buried member still holding a route publication is never retired: its
/// connection carries the obituary and can readmit it.
#[test]
fn connected_buried_member_is_retained() {
    let cluster = HewCluster::new(make_config(1));
    bury(&cluster, 2, 7, MEMBER_DEAD, 3);
    cluster
        .connection_tokens
        .lock_or_recover()
        .current
        .insert(2, 11);
    cluster.retire_buried(0);
    assert!(cluster.retire_buried(DEAD_WINDOW_MS * 4).is_empty());
    assert_eq!(
        cluster.take_swim_gossip(2, 8)[0],
        (2, Some(7), MEMBER_DEAD, 3)
    );
}

/// Within its window a buried member returns by refuting at a higher
/// incarnation, and a later burial starts a fresh window.
#[test]
fn buried_member_is_readmitted_within_its_window() {
    let cluster = HewCluster::new(make_config(1));
    bury(&cluster, 2, 7, MEMBER_DEAD, 3);
    cluster.retire_buried(0);

    cluster.upsert_member(2, MEMBER_ALIVE, 3, &[]);
    assert_eq!(
        cluster.member_state(2),
        MEMBER_DEAD,
        "no revival at or below"
    );
    cluster.upsert_member(2, MEMBER_ALIVE, 4, &[]);
    assert_eq!(cluster.member_state(2), MEMBER_ALIVE);
    assert!(cluster.retire_buried(DEAD_WINDOW_MS).is_empty());

    cluster.upsert_member(2, MEMBER_DEAD, 5, &[]);
    assert!(cluster.retire_buried(DEAD_WINDOW_MS).is_empty());
    assert!(cluster.retire_buried(2 * DEAD_WINDOW_MS - 1).is_empty());
    assert_eq!(cluster.retire_buried(2 * DEAD_WINDOW_MS).len(), 1);
}

/// After retirement the identity is unknown, so even its old session is
/// admitted as a new member. Within the window that reconnect is refused.
#[test]
fn retired_member_rejoins_as_new_member() {
    let cluster = HewCluster::new(make_config(1));
    bury(&cluster, 2, 7, MEMBER_DEAD, 3);
    cluster.retire_buried(0);
    assert_eq!(
        publish(&cluster, 2, 7, 21),
        -1,
        "an equal-session reconnect cannot revive a buried member"
    );

    assert_eq!(cluster.retire_buried(DEAD_WINDOW_MS).len(), 1);
    assert_eq!(publish(&cluster, 2, 7, 22), 1);
    assert_eq!(cluster.member_state(2), MEMBER_ALIVE);
    assert_eq!(cluster.member_incarnation(2), Some(1));
    assert_eq!(cluster.member_session(2), Some(7));
    assert!(cluster
        .observation_members()
        .contains(&(2, 7, MEMBER_ALIVE)));
}

/// A peer that left is refused at its session for one suspect timeout, so its
/// departure can be gossiped, then forgotten: a redial at the same session
/// is admitted as a new member. This covers a node whose leave was announced
/// while it stayed reachable; a stopped node refuses the redial itself.
#[test]
fn left_peer_redial_is_refused_in_grace_then_admitted_as_new() {
    let cluster = HewCluster::new(make_config(1));
    bury(&cluster, 2, 7, MEMBER_LEFT, 3);
    cluster.retire_buried(0);
    assert_eq!(publish(&cluster, 2, 7, 21), -1);
    assert!(cluster.retire_buried(LEFT_WINDOW_MS - 1).is_empty());
    assert_eq!(cluster.retire_buried(LEFT_WINDOW_MS).len(), 1);
    assert_eq!(publish(&cluster, 2, 7, 22), 1);
    assert_eq!(cluster.member_state(2), MEMBER_ALIVE);
    assert_eq!(cluster.member_incarnation(2), Some(1));
}

/// Retention clocks are per node, so A may retire B and readmit it at
/// incarnation 1 while C still holds B DEAD at a higher incarnation. When C's
/// gossip reaches A, A buries B again, its next frame tells B, B refutes past
/// the burial, and B's own frame readmits it at A; C accepts A's gossip of
/// the refutation. The membership converges on B ALIVE above every burial.
#[test]
fn early_retirement_converges_with_a_peer_still_holding_the_burial() {
    const B: u16 = 2;
    const SESSION: u32 = 7;
    let a = HewCluster::new(make_config(1));
    let b = HewCluster::new(make_config(B));
    let c = HewCluster::new(make_config(3));
    bury(&a, B, SESSION, MEMBER_DEAD, 3);
    bury(&c, B, SESSION, MEMBER_DEAD, 5);
    a.retire_buried(0);
    assert_eq!(a.retire_buried(DEAD_WINDOW_MS).len(), 1);

    // B reconnects to A only: a new member at A, still DEAD at C.
    assert_eq!(publish(&a, B, SESSION, 40), 1);
    assert_eq!(a.member_incarnation(B), Some(1));
    // C's obituary for B reaches A as gossip and buries B there again.
    let from_c = c.take_swim_gossip(1, 8);
    assert!(from_c.contains(&(B, Some(SESSION), MEMBER_DEAD, 5)));
    a.apply_swim_gossip(&[(B, SESSION, MEMBER_DEAD, 5)]);
    assert_eq!(a.member_state(B), MEMBER_DEAD);

    // A's next frame to B leads with the obituary; B refutes past it.
    let to_b = a.take_swim_gossip(B, 8);
    assert_eq!(to_b.first(), Some(&(B, Some(SESSION), MEMBER_DEAD, 5)));
    let refuted = b
        .refute_if_suspected(&[(B, SESSION, MEMBER_DEAD, 5)])
        .expect("B refutes its burial");
    assert!(refuted > 5);

    // B's next frame readmits it at A, and A's gossip readmits it at C.
    a.receive_swim_at(SWIM_MSG_PING, B, refuted, 0, 1);
    assert_eq!(a.member_state(B), MEMBER_ALIVE);
    assert_eq!(a.member_incarnation(B), Some(refuted));
    assert!(a
        .take_swim_gossip(3, 8)
        .contains(&(B, Some(SESSION), MEMBER_ALIVE, refuted)));
    c.apply_swim_gossip(&[(B, SESSION, MEMBER_ALIVE, refuted)]);
    assert_eq!(c.member_state(B), MEMBER_ALIVE);
    assert_eq!(c.member_incarnation(B), Some(refuted));
}

extern "C" fn record(node_id: u16, event: u8, user_data: *mut c_void) {
    // SAFETY: the test keeps the recorder alive until the binding is cleared.
    let events = unsafe { &*user_data.cast::<Mutex<Vec<(u16, u8)>>>() };
    events.lock_or_recover().push((node_id, event));
}

/// Retirement wakes observers exactly once, re-reports no departure and
/// gossips nothing; a second sweep is silent.
#[test]
fn retirement_wakes_observers_once_without_reporting_again() {
    let cluster = HewCluster::new(make_config(1));
    let observer = Arc::new(crate::hew_node::observation::NodeObservation::default());
    cluster.set_observer(Arc::downgrade(&observer));
    let events = Box::into_raw(Box::new(Mutex::new(Vec::<(u16, u8)>::new())));
    *cluster.membership_callback_binding.lock_or_recover() =
        MembershipCallbackBinding::new(Some(record), events.cast());

    bury(&cluster, 2, 7, MEMBER_DEAD, 3);
    cluster.retire_buried(0);
    // Exhaust the burial's gossip so any re-report would be visible.
    while !cluster.events.lock_or_recover().is_empty() {
        cluster.take_swim_gossip(3, 8);
    }
    assert!(cluster.observation_members().contains(&(2, 7, MEMBER_DEAD)));
    let reported = {
        // SAFETY: the recorder outlives this read.
        unsafe { &*events }.lock_or_recover().clone()
    };
    let woken = observer.notified_for_test();

    assert_eq!(cluster.retire_buried(DEAD_WINDOW_MS).len(), 1);
    assert_eq!(observer.notified_for_test(), woken + 1);
    assert!(cluster.observation_members().is_empty());
    assert!(cluster.retire_buried(DEAD_WINDOW_MS * 2).is_empty());
    assert_eq!(observer.notified_for_test(), woken + 1);
    // SAFETY: the recorder outlives this read.
    assert_eq!(unsafe { &*events }.lock_or_recover().clone(), reported);
    assert!(cluster.events.lock_or_recover().is_empty());

    *cluster.membership_callback_binding.lock_or_recover() = MembershipCallbackBinding::default();
    // SAFETY: the binding no longer refers to the recorder.
    drop(unsafe { Box::from_raw(events) });
}

/// Live-heap growth allowed across the measured half of the soak. Retaining
/// every departed id instead grew the live heap by several MiB over the same
/// span; the margin covers the buried entries still held at the cap.
const SOAK_GROWTH_BOUND_BYTES: u64 = 1024 * 1024;

/// Counts the bytes currently allocated through the global allocator, so the
/// soak measures what the cluster retains rather than where the system
/// allocator happens to place and release pages.
struct LiveBytesAllocator;

static LIVE_BYTES: AtomicUsize = AtomicUsize::new(0);

// SAFETY: every call is forwarded unchanged to the system allocator; the
// counter only observes sizes.
unsafe impl std::alloc::GlobalAlloc for LiveBytesAllocator {
    unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
        // SAFETY: caller upholds the `GlobalAlloc` contract.
        let ptr = unsafe { std::alloc::System.alloc(layout) };
        if !ptr.is_null() {
            LIVE_BYTES.fetch_add(layout.size(), Ordering::Relaxed);
        }
        ptr
    }

    unsafe fn alloc_zeroed(&self, layout: std::alloc::Layout) -> *mut u8 {
        // SAFETY: caller upholds the `GlobalAlloc` contract.
        let ptr = unsafe { std::alloc::System.alloc_zeroed(layout) };
        if !ptr.is_null() {
            LIVE_BYTES.fetch_add(layout.size(), Ordering::Relaxed);
        }
        ptr
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: std::alloc::Layout) {
        LIVE_BYTES.fetch_sub(layout.size(), Ordering::Relaxed);
        // SAFETY: caller upholds the `GlobalAlloc` contract.
        unsafe { std::alloc::System.dealloc(ptr, layout) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: std::alloc::Layout, new_size: usize) -> *mut u8 {
        // SAFETY: caller upholds the `GlobalAlloc` contract.
        let new = unsafe { std::alloc::System.realloc(ptr, layout, new_size) };
        if !new.is_null() {
            LIVE_BYTES.fetch_sub(layout.size(), Ordering::Relaxed);
            LIVE_BYTES.fetch_add(new_size, Ordering::Relaxed);
        }
        new
    }
}

#[global_allocator]
static TEST_ALLOCATOR: LiveBytesAllocator = LiveBytesAllocator;

/// Bytes currently allocated by this process.
fn live_bytes() -> u64 {
    LIVE_BYTES.load(Ordering::Relaxed) as u64
}

/// Three clusters churning 2000 node ids through join, burial and departure
/// keep every per-slot map within the cap, and the live heap stays within
/// [`SOAK_GROWTH_BOUND_BYTES`] of its level after a 1000-id warm-up.
///
/// Runs in its own process: the heap counter is process-wide, so other
/// libtest cases would otherwise move it.
#[test]
fn soak_three_clusters_with_2000_departing_ids_stays_flat() {
    const CHILD: &str = "HEW_CLUSTER_RETENTION_SOAK_CHILD";
    if std::env::var_os(CHILD).is_none() {
        let output = std::process::Command::new(
            std::env::current_exe().expect("runtime test binary"),
        )
        .args([
            "cluster::tests::retention::soak_three_clusters_with_2000_departing_ids_stays_flat",
            "--exact",
            "--nocapture",
            "--test-threads=1",
        ])
        .env(CHILD, "1")
        .output()
        .expect("isolated retention soak");
        assert!(
            output.status.success(),
            "retention soak failed:\n{}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        return;
    }

    let live = live_bytes;
    let clusters = (1..=3)
        .map(|local| HewCluster::new(make_config(local)))
        .collect::<Vec<_>>();
    let mut warm = 0;
    let mut highest = 0;
    for round in 0..2000_u64 {
        let slot = 10 + u16::try_from(round).expect("slot fits");
        let state = if round % 3 == 0 {
            MEMBER_LEFT
        } else {
            MEMBER_DEAD
        };
        for cluster in &clusters {
            bury(cluster, slot, 1 + (round % 5) as u32, state, 2);
            cluster.retire_buried(round * 10);
            cluster.take_swim_gossip(2, 8);
        }
        if round == 999 {
            warm = live();
        } else if round > 999 {
            highest = highest.max(live());
        }
    }
    let growth = highest.saturating_sub(warm);
    println!("retention soak: warm {warm} B, highest {highest} B, growth {growth} B");
    assert!(
        growth <= SOAK_GROWTH_BOUND_BYTES,
        "live heap grew {growth} bytes after warm-up"
    );
    for cluster in &clusters {
        let members = cluster.members.lock_or_recover().len();
        assert!(members <= MAX_BURIED_MEMBERS, "members {members}");
        assert!(cluster.sessions.lock_or_recover().len() <= MAX_BURIED_MEMBERS);
        assert!(cluster.detectors.lock_or_recover().len() <= MAX_BURIED_MEMBERS);
        assert!(cluster.events.lock_or_recover().len() <= MAX_GOSSIP_EVENTS);
        assert!(cluster.relayed_probes.lock_or_recover().is_empty());
        assert!(cluster.requested_probes.lock_or_recover().is_empty());
        let tokens = cluster.connection_tokens.lock_or_recover();
        assert!(tokens.announced.len() <= MAX_BURIED_MEMBERS);
        assert!(tokens.visible.len() <= MAX_BURIED_MEMBERS);
    }
}
