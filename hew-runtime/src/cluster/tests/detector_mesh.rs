//! Failure-detector accuracy across a simulated three-node SWIM mesh.
//!
//! Three in-process clusters exchange SWIM frames on a simulated clock. Each
//! protocol period mirrors `swim_driver::run_one_period` (tick, direct PING,
//! `PING_REQ` through every other connected peer) and each delivery mirrors
//! `connection::swim::handle_swim_control_frame` (gossip import, refutation,
//! then `receive_swim` and the replies it returns).

use super::*;

const PERIOD_MS: u64 = 1_000;
/// Drivers started together tick within milliseconds of each other, so each
/// peer's frames arrive in one burst per period.
const ALIGNED_PHASES_MS: [u64; 3] = [0, 3, 7];
/// Drivers started apart spread their frames across the period.
const SPREAD_PHASES_MS: [u64; 3] = [0, 337, 671];
/// Route slots, distinct from other tests' slots.
const SLOTS: [u16; 3] = [7101, 7102, 7103];

struct Frame {
    deliver_at: u64,
    from: u16,
    to: u16,
    msg_type: i32,
    incarnation: u64,
    target: u16,
    gossip: Vec<(u16, u32, i32, u64)>,
}

/// Membership events one node published, stamped with the simulated time.
struct EventLog {
    clock: Arc<AtomicU64>,
    events: Mutex<Vec<(u64, u16, u8)>>,
}

extern "C" fn record_event(node_id: u16, event: u8, user_data: *mut c_void) {
    // SAFETY: each mesh node's callback points at its own log; the mesh never
    // grows `logs` and drops it only after the clusters.
    let log = unsafe { &*user_data.cast::<EventLog>() };
    let now = log.clock.load(Ordering::Relaxed);
    log.events.lock_or_recover().push((now, node_id, event));
}

struct Mesh {
    nodes: Vec<HewCluster>,
    logs: Vec<EventLog>,
    clock: Arc<AtomicU64>,
    phases_ms: [u64; 3],
    in_flight: Vec<Frame>,
    sent: u64,
    /// A stalled node keeps its connections but neither runs nor reads; frames
    /// to and from it wait in the transport until the stall ends.
    stalled: Option<u16>,
    /// Black-holed links: frames between the pair are lost while both stay
    /// connected.
    cut: Vec<[u16; 2]>,
    /// Whether each period also sends `PING_REQ` through the other peers.
    indirect: bool,
}

impl Mesh {
    fn new(phases_ms: [u64; 3]) -> Self {
        let clock = Arc::new(AtomicU64::new(0));
        let nodes: Vec<HewCluster> = SLOTS
            .iter()
            .map(|&slot| HewCluster::new(make_config(slot)))
            .collect();
        let logs: Vec<EventLog> = SLOTS
            .iter()
            .map(|_| EventLog {
                clock: Arc::clone(&clock),
                events: Mutex::new(Vec::new()),
            })
            .collect();
        for (node, log) in nodes.iter().zip(&logs) {
            // Node startup joins its own slot, then admits each peer's session.
            node.upsert_member(node.config.local_node_id, MEMBER_ALIVE, 1, &[]);
            for &peer in SLOTS
                .iter()
                .filter(|&&slot| slot != node.config.local_node_id)
            {
                node.sessions.lock_or_recover().insert(peer, 1);
                node.upsert_member(peer, MEMBER_ALIVE, 1, &[]);
                node.update_last_seen(peer, 0);
            }
            node.events.lock_or_recover().clear();
            let user_data = std::ptr::from_ref::<EventLog>(log).cast_mut().cast();
            *node.membership_callback_binding.lock_or_recover() =
                MembershipCallbackBinding::new(Some(record_event), user_data);
        }
        Self {
            nodes,
            logs,
            clock,
            phases_ms,
            in_flight: Vec::new(),
            sent: 0,
            stalled: None,
            cut: Vec::new(),
            indirect: true,
        }
    }

    fn index(slot: u16) -> usize {
        SLOTS.iter().position(|&s| s == slot).expect("mesh slot")
    }

    fn node(&self, slot: u16) -> &HewCluster {
        &self.nodes[Self::index(slot)]
    }

    fn is_cut(&self, a: u16, b: u16) -> bool {
        self.cut
            .iter()
            .any(|link| *link == [a, b] || *link == [b, a])
    }

    /// Black-hole the `a`–`b` link, losing the frames already on it.
    fn cut_link(&mut self, a: u16, b: u16) {
        self.cut.push([a, b]);
        self.in_flight.retain(|frame| {
            !(frame.from == a && frame.to == b || frame.from == b && frame.to == a)
        });
    }

    /// Queue a frame with a deterministic 1–40 ms one-way latency, draining
    /// the sender's piggybacked gossip exactly as an outbound frame does. A
    /// frame on a cut link is lost after its gossip is spent.
    fn send(&mut self, now: u64, from: u16, to: u16, msg_type: i32, target: u16) {
        let sender = self.node(from);
        let gossip = sender
            .take_swim_gossip(to, sender.max_gossip_per_msg())
            .into_iter()
            .map(|(slot, session, state, incarnation)| {
                (slot, session.unwrap_or(1), state, incarnation)
            })
            .collect();
        let incarnation = sender.local_incarnation();
        if self.is_cut(from, to) {
            return;
        }
        self.sent += 1;
        self.in_flight.push(Frame {
            deliver_at: now + 1 + (self.sent * 7_919) % 40,
            from,
            to,
            msg_type,
            incarnation,
            target,
            gossip,
        });
    }

    fn deliver(&mut self, now: u64, frame: &Frame) {
        let receiver = self.node(frame.to);
        receiver.apply_swim_gossip(&frame.gossip);
        receiver.refute_if_suspected(&frame.gossip);
        let replies = receiver.receive_swim_at(
            frame.msg_type,
            frame.from,
            frame.incarnation,
            frame.target,
            now,
        );
        for reply in replies {
            self.send(now, frame.to, reply.to, reply.msg_type, reply.target);
        }
    }

    fn run_period(&mut self, now: u64, slot: u16) {
        if self.stalled == Some(slot) {
            return;
        }
        let Some(target) = self.node(slot).run_protocol_period(now) else {
            return;
        };
        self.send(now, slot, target, SWIM_MSG_PING, 0);
        if !self.indirect {
            return;
        }
        for relay in SLOTS {
            if relay != slot && relay != target {
                self.send(now, slot, relay, SWIM_MSG_PING_REQ, target);
                self.node(slot).note_ping_req_sent_at(relay, target, now);
            }
        }
    }

    /// Advance the simulated clock one millisecond at a time up to `until`.
    fn run_until(&mut self, until: u64) {
        let from = self.clock.load(Ordering::Relaxed);
        for now in from..until {
            self.clock.store(now, Ordering::Relaxed);
            let stalled = self.stalled;
            while let Some(index) = self.in_flight.iter().position(|frame| {
                frame.deliver_at <= now && stalled.is_none_or(|slot| frame.to != slot)
            }) {
                let frame = self.in_flight.swap_remove(index);
                self.deliver(now, &frame);
            }
            for (slot, phase) in SLOTS.into_iter().zip(self.phases_ms) {
                if now % PERIOD_MS == phase {
                    self.run_period(now, slot);
                }
            }
        }
        self.clock.store(until, Ordering::Relaxed);
    }

    /// `(time, event)` pairs `observer` published about `subject`.
    fn events_about(&self, observer: u16, subject: u16) -> Vec<(u64, u8)> {
        self.logs[Self::index(observer)]
            .events
            .lock_or_recover()
            .iter()
            .filter(|(_, node, _)| *node == subject)
            .map(|&(at, _, event)| (at, event))
            .collect()
    }

    /// Every SUSPECT or DEAD event published since `since`, as
    /// `(time, observer, subject, event)`.
    fn faults_since(&self, since: u64) -> Vec<(u64, u16, u16, u8)> {
        let mut faults = Vec::new();
        for observer in SLOTS {
            for &(at, subject, event) in self.logs[Self::index(observer)]
                .events
                .lock_or_recover()
                .iter()
            {
                if at >= since
                    && (event == HEW_MEMBERSHIP_EVENT_NODE_SUSPECT
                        || event == HEW_MEMBERSHIP_EVENT_NODE_DEAD)
                {
                    faults.push((at, observer, subject, event));
                }
            }
        }
        faults
    }

    fn incarnations(&self) -> Vec<u64> {
        self.nodes
            .iter()
            .map(HewCluster::local_incarnation)
            .collect()
    }

    fn assert_all_alive(&self) {
        for observer in SLOTS {
            for subject in SLOTS {
                assert_eq!(
                    self.node(observer).member_state(subject),
                    MEMBER_ALIVE,
                    "slot {observer} sees slot {subject} alive"
                );
            }
        }
    }
}

/// A healthy mesh never suspects a member: silence between two round-robin
/// probes of the same peer is the protocol's cadence, not a fault.
#[test]
fn healthy_mesh_raises_no_suspicion() {
    let _guard = crate::runtime_test_guard();
    for phases in [ALIGNED_PHASES_MS, SPREAD_PHASES_MS] {
        let mut mesh = Mesh::new(phases);
        let start = mesh.incarnations();
        mesh.run_until(60_000);
        let faults = mesh.faults_since(0);
        assert!(
            faults.is_empty(),
            "tick phases {phases:?} raised {} faults, first {:?}",
            faults.len(),
            faults.iter().take(4).collect::<Vec<_>>()
        );
        assert_eq!(mesh.incarnations(), start, "no member may need to refute");
        mesh.assert_all_alive();
    }
}

/// A stalled member is suspected once a direct probe goes unanswered and is
/// declared dead by its suspect timeout; the healthy pair never suspects
/// each other.
#[test]
fn stalled_member_is_suspected_then_declared_dead() {
    const STALL_AT: u64 = 20_000;
    let _guard = crate::runtime_test_guard();
    let stalled = SLOTS[2];
    let config = make_config(0);
    let probe_cycle = (SLOTS.len() as u64 - 1) * PERIOD_MS;
    // The next round-robin probe of the stalled peer is at most one probe
    // cycle away, and the first tick strictly past its timeout finds it
    // overdue.
    let suspect_by =
        STALL_AT + probe_cycle + (u64::from(config.ping_timeout_ms) / PERIOD_MS + 1) * PERIOD_MS;
    // SUSPECT -> DEAD measures the suspect timeout from the last contact.
    let dead_by = STALL_AT + u64::from(config.suspect_timeout_ms) + PERIOD_MS;

    let mut mesh = Mesh::new(ALIGNED_PHASES_MS);
    mesh.run_until(STALL_AT);
    mesh.stalled = Some(stalled);
    mesh.run_until(dead_by + 1);

    for observer in [SLOTS[0], SLOTS[1]] {
        let events = mesh.events_about(observer, stalled);
        let [(suspect_at, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT), (dead_at, HEW_MEMBERSHIP_EVENT_NODE_DEAD)] =
            events[..]
        else {
            panic!("slot {observer} must suspect then bury the stalled member: {events:?}");
        };
        assert!(
            suspect_at <= suspect_by && suspect_at < dead_at && dead_at <= dead_by,
            "slot {observer}: suspect at {suspect_at} (bound {suspect_by}), \
             dead at {dead_at} (bound {dead_by})"
        );
    }
    let healthy_pair = mesh
        .faults_since(0)
        .into_iter()
        .filter(|&(_, observer, subject, _)| observer != stalled && subject != stalled)
        .collect::<Vec<_>>();
    assert_eq!(healthy_pair, Vec::new(), "the healthy pair stays trusted");
}

/// After a stall heals, the buried member refutes and the mesh settles:
/// readmission never cascades into fresh suspicion.
#[test]
fn healed_stall_settles_without_flapping() {
    const STALL_AT: u64 = 20_000;
    const HEAL_AT: u64 = 30_000;
    const SETTLE_BY: u64 = HEAL_AT + 10_000;
    let _guard = crate::runtime_test_guard();
    let mut mesh = Mesh::new(ALIGNED_PHASES_MS);
    mesh.run_until(STALL_AT);
    mesh.stalled = Some(SLOTS[2]);
    mesh.run_until(HEAL_AT);
    mesh.stalled = None;
    mesh.run_until(SETTLE_BY);
    let settled = mesh.incarnations();
    mesh.run_until(SETTLE_BY + 60_000);

    mesh.assert_all_alive();
    assert_eq!(
        mesh.faults_since(SETTLE_BY),
        Vec::new(),
        "no fault after settling"
    );
    assert_eq!(mesh.incarnations(), settled, "no refutation after settling");
}

/// A pause shorter than the suspect timeout may draw a suspicion, but the
/// member's next frame recovers it before anyone buries it.
#[test]
fn brief_pause_is_never_buried() {
    const PAUSE_AT: u64 = 20_000;
    let _guard = crate::runtime_test_guard();
    let mut mesh = Mesh::new(ALIGNED_PHASES_MS);
    mesh.run_until(PAUSE_AT);
    mesh.stalled = Some(SLOTS[2]);
    mesh.run_until(PAUSE_AT + 2_000);
    mesh.stalled = None;
    mesh.run_until(PAUSE_AT + 30_000);

    let buried = mesh
        .faults_since(0)
        .into_iter()
        .filter(|&(_, _, _, event)| event == HEW_MEMBERSHIP_EVENT_NODE_DEAD)
        .collect::<Vec<_>>();
    assert_eq!(buried, Vec::new(), "a brief pause is forgiven");
    mesh.assert_all_alive();
}

/// A black hole between two live members is bridged by the third: the relay
/// forwards the probed member's ACK, so neither side of the cut is suspected
/// and nobody has to refute.
#[test]
fn pairwise_partition_is_bridged_by_indirect_acks() {
    const CUT_AT: u64 = 20_000;
    let _guard = crate::runtime_test_guard();
    for phases in [ALIGNED_PHASES_MS, SPREAD_PHASES_MS] {
        let mut mesh = Mesh::new(phases);
        mesh.run_until(CUT_AT);
        let start = mesh.incarnations();
        mesh.cut_link(SLOTS[0], SLOTS[1]);
        mesh.run_until(CUT_AT + 60_000);

        let faults = mesh.faults_since(0);
        assert!(
            faults.is_empty(),
            "tick phases {phases:?} raised {} faults across the cut, first {:?}",
            faults.len(),
            faults.iter().take(4).collect::<Vec<_>>()
        );
        assert_eq!(mesh.incarnations(), start, "no member may need to refute");
        mesh.assert_all_alive();
    }
}

/// A member buried while its frames were lost, not held, still learns of its
/// burial once traffic flows again, long after every gossiped obituary was
/// spent, and returns by refuting it.
#[test]
fn dropped_burial_is_refuted_after_heal() {
    const STALL_AT: u64 = 20_000;
    const HEAL_AT: u64 = 40_000;
    const REJOIN_BY: u64 = HEAL_AT + 10_000;
    let _guard = crate::runtime_test_guard();
    let buried = SLOTS[2];
    let mut mesh = Mesh::new(ALIGNED_PHASES_MS);
    mesh.run_until(STALL_AT);
    mesh.stalled = Some(buried);
    mesh.cut_link(buried, SLOTS[0]);
    mesh.cut_link(buried, SLOTS[1]);
    mesh.run_until(HEAL_AT);

    for observer in [SLOTS[0], SLOTS[1]] {
        let node = mesh.node(observer);
        assert_eq!(
            node.member_state(buried),
            MEMBER_DEAD,
            "slot {observer} buried it"
        );
        assert!(
            !node
                .events
                .lock_or_recover()
                .iter()
                .any(|event| event.node_id == buried),
            "slot {observer} spent its gossiped obituary on lost frames"
        );
    }
    let buried_incarnation = mesh.node(buried).local_incarnation();
    mesh.stalled = None;
    mesh.cut.clear();
    mesh.run_until(REJOIN_BY);

    mesh.assert_all_alive();
    assert!(
        mesh.node(buried).local_incarnation() > buried_incarnation,
        "the buried member refuted its death"
    );
    let settled = mesh.incarnations();
    mesh.run_until(REJOIN_BY + 60_000);
    assert_eq!(
        mesh.faults_since(REJOIN_BY),
        Vec::new(),
        "no fault after rejoining"
    );
    assert_eq!(
        mesh.incarnations(),
        settled,
        "no refutation after rejoining"
    );
}
