//! Driven SWIM failure-detection tests.

use super::*;

/// A dead node is detected by a survivor via the now-driven SWIM detector:
/// SUSPECT→DEAD escalation fires and `on_member_dead` fans out a partition
/// signal — within a bounded number of protocol periods.
///
/// This is the C1 proof. SUSPECT→DEAD escalation and the `on_member_dead`
/// fan-out happen ONLY inside `hew_cluster_tick`, which had zero production
/// call sites before the driver. Without the driver, node B would stay
/// SUSPECT forever (the connection-event path only reaches SUSPECT) and the
/// partition recv below would block until the test's deadline.
///
/// Runs under simulated time (enabled by `SwimTimingEnv::fast()` before
/// any node starts): the SWIM driver uses the sim clock, so the test is
/// load-immune — wall time is irrelevant.
///
/// The detector is driven through its two transitions in order — SUSPECT
/// first (synchronised on, not raced), then SUSPECT→DEAD — because a single
/// sim-time advance only fires one tick and each tick applies at most one
/// transition.  See the inline notes at the advance points.
#[cfg(feature = "encryption")]
#[test]
fn dead_node_is_detected_by_survivor_via_driven_swim() {
    use crate::cluster::{hew_cluster_member_state, hew_cluster_set_partition_registry};
    use crate::duplex::{HewDuplex, RecvError};
    use std::sync::Arc;
    const NODE_A: u16 = 340;
    const NODE_B: u16 = 341;
    let _guard = crate::runtime_test_guard();
    // Enable simtime BEFORE any node starts so the SWIM drivers pick up
    // the sim clock at thread spawn time.
    let _swim_env = SwimTimingEnv::fast();
    crate::registry::hew_registry_clear();

    let (node_a, _node_a_port, node_b, node_b_port) = start_authorized_tcp_pair(NODE_A, NODE_B);

    // Install a partition registry on A so on_member_dead is observable,
    // and bind a duplex recv to NODE_B.
    let registry = Arc::new(crate::cluster::PartitionRegistry::new());
    // SAFETY: A's cluster is live after a successful start.
    unsafe {
        assert!(!(*node_a.as_ptr()).cluster.is_null());
        hew_cluster_set_partition_registry((*node_a.as_ptr()).cluster, Arc::clone(&registry));
    }
    let (dx, peer) = HewDuplex::new_pair(8, 8);
    dx.register_recv_with_partition_registry(&registry, NODE_B);
    let recv_handle = {
        let handle = dx.clone_handle();
        thread::spawn(move || handle.recv())
    };

    // Connect A → B and wait for the handshake so A knows B is ALIVE.
    let connect_addr = CString::new(format!("{NODE_B}@127.0.0.1:{node_b_port}")).unwrap();
    // SAFETY: node_a and the connect addr are valid for this call.
    unsafe { connect_with_retry(node_a.as_ptr(), &connect_addr) };
    // SAFETY: both nodes are valid here.
    unsafe { wait_for_handshake(node_a.as_ptr(), node_b.as_ptr()) };

    // Kill node B: stopping it drops the mesh connection, so A's view of B
    // is no longer refreshed. The driver must escalate it to DEAD.
    // SAFETY: node_b is valid.
    unsafe { assert_eq!(hew_node_stop(node_b.as_ptr()), 0) };

    // Drive the failure detector deterministically through its TWO state
    // transitions.  `compute_tick_transitions` applies at most one
    // transition per member per tick (ALIVE→SUSPECT, then SUSPECT→DEAD),
    // and one simulated-time advance lets the driver fire exactly one tick
    // before the virtual clock freezes again — so ALIVE→DEAD cannot happen
    // on a single advance.  B must already be SUSPECT before the advance
    // that crosses the suspect timeout.
    //
    // Phase 1 — reach SUSPECT.  Stopping B drops the mesh connection, which
    // moves A's view of B to SUSPECT via the connection-event path; that
    // detection is asynchronous, so we WAIT for it rather than racing the
    // driver's tick.  Advancing two protocol periods first also lets a
    // driver tick perform ALIVE→SUSPECT (elapsed > ping_timeout) should the
    // connection event not have landed yet, so SUSPECT is reached regardless
    // of ordering — but B cannot yet reach DEAD (elapsed < suspect_timeout).
    //
    // Racing this — advancing straight past the suspect timeout while B was
    // still ALIVE — is what hung this test on Linux under load: the single
    // tick spent itself on ALIVE→SUSPECT, then the frozen sim clock starved
    // the SUSPECT→DEAD tick (the driver waits in `sim_sleep_ms` without
    // advancing time), so `on_member_dead` never fired and `recv` blocked
    // until the harness deadline.
    crate::deterministic::hew_simtime_advance_ms((2 * SWIM_TEST_PERIOD_MS).cast_signed());
    // SAFETY: A's cluster is live (node still running).
    unsafe {
        wait_for_member_state_at_least(node_a.as_ptr(), NODE_B, crate::cluster::MEMBER_SUSPECT);
    }

    // Phase 2 — escalate SUSPECT→DEAD.  Advance past the suspect timeout so
    // the next tick observes elapsed (= now − last_seen) > suspect_timeout,
    // declares B DEAD, and `on_member_dead` fans out PartitionDetected —
    // unblocking the recv below.  No real-time deadline is needed: the
    // driver fires as soon as it gets a scheduling turn after the advance.
    crate::deterministic::hew_simtime_advance_ms(
        (SWIM_TEST_SUSPECT_TIMEOUT_MS + 2 * SWIM_TEST_PERIOD_MS).cast_signed(),
    );
    let result = recv_handle.join().expect("recv thread panicked");
    assert!(
        matches!(result, Err(RecvError::PartitionDetected)),
        "survivor must detect the dead node via on_member_dead, got: {result:?}"
    );

    // And A's membership view must record B as DEAD.
    // SAFETY: A's cluster is still live.
    let state = unsafe { hew_cluster_member_state((*node_a.as_ptr()).cluster, NODE_B) };
    assert_eq!(
        state,
        crate::cluster::MEMBER_DEAD,
        "A's membership view must record node B as DEAD"
    );

    drop(peer);
    // SAFETY: nodes were allocated in this test and remain valid.
    unsafe { assert_eq!(hew_node_stop(node_a.as_ptr()), 0) };
    crate::registry::hew_registry_clear();
}

/// No false positive: two connected, both-alive nodes run the driven SWIM
/// detector for many simulated protocol periods and neither is wrongly
/// declared DEAD.
///
/// This proves the detector does not kill a slow-but-reachable peer: as
/// long as the mesh connection stays up (refreshing last-seen via incoming
/// SWIM frames) the tick never escalates either node.
///
/// Runs under simulated time.  The test explicitly advances `SIMTIME_MS`
/// by one protocol period at a time, sleeping real `SWIM_ALIVE_REAL_SLEEP_MS`
/// between advances so the loopback TCP ping-ack round-trip completes and
/// the connection-reader thread can update `last_seen_ms = hew_now_ms()`.
/// This is load-immune: the SWIM thresholds are in sim time, so high CPU
/// overhead (e.g. from `TSan` or coverage instrumentation) never causes a
/// false-DEAD verdict.
#[cfg_attr(windows, ignore)]
// WINDOWS-TODO: loopback TCP on Windows has higher round-trip latency
// (15 ms OS timer granularity) than this test's real-sleep window.  Fix
// requires the IOCP reactor (Phase 2) timer infrastructure.
#[cfg(feature = "quic")]
#[test]
fn alive_node_is_not_falsely_killed_by_driven_swim() {
    use crate::cluster::hew_cluster_member_state;
    const NODE_A: u16 = 342;
    const NODE_B: u16 = 343;
    // After advancing each simulated period we wait for the real loopback
    // PING-ACK round-trip to refresh last_seen_ms before advancing the next
    // period (see the observation loop): a short poll cadence and a generous
    // deadline that only trips if the round-trip never lands. This replaces a
    // fixed sleep bet against loopback/VM latency, so a loaded CI host can no
    // longer stale last_seen into a false DEAD.
    const SWIM_ALIVE_POLL_MS: u64 = 2;
    const SWIM_ALIVE_DEADLINE_MS: u64 = 5_000;
    // Number of simulated protocol periods to observe.  3 periods span
    // suspect_timeout (120 ms sim) so this exercises the "don't kill a live
    // peer" invariant across the full timeout window.
    const OBSERVATION_PERIODS: u64 = 5;

    let _guard = crate::runtime_test_guard();
    // Enable simtime BEFORE any node starts so the SWIM drivers pick up
    // the sim clock at thread spawn time.
    let _swim_env = SwimTimingEnv::fast();
    crate::registry::hew_registry_clear();

    // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
    // Driven SWIM requires an authenticated peer (D9 gates the handler).
    let (node_a, _node_a_port, node_b, node_b_port) =
        start_authorized_quic_mesh_pair(NODE_A, NODE_B);

    let connect_addr = CString::new(format!("{NODE_B}@127.0.0.1:{node_b_port}")).unwrap();
    // SAFETY: node_a and the connect addr are valid for this call.
    unsafe { connect_with_retry(node_a.as_ptr(), &connect_addr) };
    // SAFETY: both nodes are valid here.
    unsafe { wait_for_handshake(node_a.as_ptr(), node_b.as_ptr()) };

    // Baseline last_seen_ms readings from immediately after the handshake,
    // before any simulated period has been advanced. Each period below
    // must observe last_seen_ms strictly *newer* than the value recorded
    // at the end of the previous period. Polling the coarse MEMBER_ALIVE
    // state alone is not sufficient here: update_last_seen only flips
    // SUSPECT -> ALIVE and otherwise leaves an already-ALIVE state
    // unchanged, so once both peers reach ALIVE on period 0, a state-only
    // poll is trivially already satisfied at the very first check of every
    // subsequent period -- it would break immediately without ever
    // observing that period's own PING-ACK round-trip actually landed.
    // last_seen_ms is refreshed on every round-trip regardless of state,
    // so requiring a fresh value each period genuinely confirms each tick
    // re-synchronized rather than re-observing stale state from an earlier
    // period.
    // SAFETY: node_a's cluster is live (node still running).
    let mut prev_a_view_of_b = unsafe { &*(*node_a.as_ptr()).cluster }
        .member_last_seen_ms(NODE_B)
        .unwrap_or(0);
    // SAFETY: node_b's cluster is live (node still running).
    let mut prev_b_view_of_a = unsafe { &*(*node_b.as_ptr()).cluster }
        .member_last_seen_ms(NODE_A)
        .unwrap_or(0);

    // Step sim time forward one protocol period at a time. After each
    // advance the driver fires a tick (its next_period_ms has been crossed)
    // and sends a PING; the peer's ACK lets the connection-reader refresh
    // last_seen_ms = hew_now_ms(). We then wait for that round-trip to land
    // before advancing the next period, so each tick sees elapsed = one
    // period (< suspect_timeout) and neither node is declared DEAD.
    #[expect(
        clippy::cast_possible_wrap,
        reason = "SWIM_TEST_PERIOD_MS is 40; always fits in i64"
    )]
    for _ in 0..OBSERVATION_PERIODS {
        crate::deterministic::hew_simtime_advance_ms(SWIM_TEST_PERIOD_MS as i64);
        // Wait for this period's PING-ACK round-trip to land and refresh
        // last_seen_ms before advancing the next sim period. Sim time is
        // frozen during this real wait, so no new staleness accrues; a
        // landing ACK revives a transient SUSPECT straight back to ALIVE
        // (cluster::update_last_seen), and the SUSPECT -> DEAD step cannot
        // fire without another sim advance. We proceed only once both
        // peers report a last_seen_ms strictly newer than the value
        // recorded at the end of the previous period — confirming this
        // period's own round-trip actually landed, not just that state is
        // still ALIVE from an earlier one. A transient DEAD can be
        // self-healing when the driver races the connection-reader refresh,
        // so the invariant is enforced when the deadline expires.
        let alive_deadline =
            std::time::Instant::now() + Duration::from_millis(SWIM_ALIVE_DEADLINE_MS);
        let (a_view_of_b, b_view_of_a) = loop {
            // SAFETY: A's cluster is live (node still running).
            let a_state = unsafe { hew_cluster_member_state((*node_a.as_ptr()).cluster, NODE_B) };
            // SAFETY: B's cluster is live (node still running).
            let b_state = unsafe { hew_cluster_member_state((*node_b.as_ptr()).cluster, NODE_A) };
            if std::time::Instant::now() >= alive_deadline {
                assert_ne!(
                    a_state,
                    crate::cluster::MEMBER_DEAD,
                    "A persistently declared a still-alive B as DEAD after \
                     the period deadline (state={a_state})"
                );
                assert_ne!(
                    b_state,
                    crate::cluster::MEMBER_DEAD,
                    "B persistently declared a still-alive A as DEAD after \
                     the period deadline (state={b_state})"
                );
            }
            // SAFETY: A's cluster is live (node still running).
            let a_last_seen = unsafe { &*(*node_a.as_ptr()).cluster }
                .member_last_seen_ms(NODE_B)
                .unwrap_or(0);
            // SAFETY: B's cluster is live (node still running).
            let b_last_seen = unsafe { &*(*node_b.as_ptr()).cluster }
                .member_last_seen_ms(NODE_A)
                .unwrap_or(0);
            if a_state == crate::cluster::MEMBER_ALIVE
                && b_state == crate::cluster::MEMBER_ALIVE
                && a_last_seen > prev_a_view_of_b
                && b_last_seen > prev_b_view_of_a
            {
                break (a_last_seen, b_last_seen);
            }
            assert!(
                std::time::Instant::now() < alive_deadline,
                "PING-ACK round-trip did not refresh last_seen within the \
                 deadline (a_state={a_state}, b_state={b_state}, \
                 a_last_seen={a_last_seen}, prev_a_view_of_b={prev_a_view_of_b}, \
                 b_last_seen={b_last_seen}, prev_b_view_of_a={prev_b_view_of_a})"
            );
            thread::sleep(Duration::from_millis(SWIM_ALIVE_POLL_MS));
        };
        prev_a_view_of_b = a_view_of_b;
        prev_b_view_of_a = b_view_of_a;
    }

    // SAFETY: nodes were allocated in this test and remain valid.
    unsafe {
        assert_eq!(hew_node_stop(node_a.as_ptr()), 0);
        assert_eq!(hew_node_stop(node_b.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}
