//! SWIM-based cluster membership protocol for the Hew runtime.
//!
//! Implements a simplified [SWIM][swim] (Scalable Weakly-consistent
//! Infection-style process group Membership) protocol for discovering
//! and monitoring nodes in a Hew actor cluster.
//!
//! [swim]: https://www.cs.cornell.edu/projects/Quicksilver/public_pdfs/SWIM.pdf
//!
//! # Protocol Summary
//!
//! Each node periodically pings a random peer. If no ACK arrives within
//! a timeout, the node sends indirect pings through K other peers.
//! If those also fail, the peer is marked **suspect**. After a configurable
//! suspect timeout, the peer is declared **dead** and removed.
//!
//! Membership changes (join, leave, dead) are disseminated via
//! **piggyback gossip**: each ping/ack message carries a bounded list
//! of recent membership events.
//!
//! # Architecture
//!
//! ```text
//! HewCluster
//!   ├── members: Vec<ClusterMember>  (current membership list)
//!   ├── events: VecDeque<MemberEvent> (recent gossip, bounded)
//!   └── config: ClusterConfig (timeouts, fanout, seed list)
//! ```
//!
//! # C ABI
//!
//! - [`hew_cluster_new`] — Create a cluster instance.
//! - [`hew_cluster_free`] — Destroy a cluster instance.
//! - [`hew_cluster_join`] — Add a seed node and begin protocol.
//! - [`hew_cluster_leave`] — Gracefully leave the cluster.
//! - [`hew_cluster_members`] — Get current member list.
//! - [`hew_cluster_member_count`] — Number of known members.
//! - [`hew_cluster_process_message`] — Handle an incoming SWIM message.
//! - [`hew_cluster_tick`] — Advance the protocol (call periodically).
//! - [`hew_cluster_set_callback`] — Register membership change callback.
//! - [`hew_cluster_set_membership_callback`] — Register event callback with user data.
//! - [`hew_cluster_notify_connection_lost`] — Notify SWIM when a connection drops.
//! - [`hew_cluster_notify_connection_established`] — Notify SWIM when a connection is restored.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::duplex::Queue;
use crate::phi_accrual::PhiAccrualDetector;
use crate::util::MutexExt;
use std::collections::{HashMap, VecDeque};
use std::ffi::{c_char, c_int, c_void, CStr};
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, Weak};

// ── Member states ──────────────────────────────────────────────────────

/// Member is alive and responding to pings.
pub const MEMBER_ALIVE: i32 = 0;
/// Member did not respond to ping; awaiting indirect confirmation.
pub const MEMBER_SUSPECT: i32 = 1;
/// Member confirmed unreachable; will be removed.
pub const MEMBER_DEAD: i32 = 2;
/// Member has gracefully left the cluster.
pub const MEMBER_LEFT: i32 = 3;

/// Membership callback event: node joined or became alive.
pub const HEW_MEMBERSHIP_EVENT_NODE_JOINED: u8 = 1;
/// Membership callback event: node became suspect.
pub const HEW_MEMBERSHIP_EVENT_NODE_SUSPECT: u8 = 2;
/// Membership callback event: node declared dead.
pub const HEW_MEMBERSHIP_EVENT_NODE_DEAD: u8 = 3;
/// Membership callback event: node left gracefully.
pub const HEW_MEMBERSHIP_EVENT_NODE_LEFT: u8 = 4;

/// Gossip event: a registry name was added (actor registered).
pub const GOSSIP_REGISTRY_ADD: u8 = 5;
/// Gossip event: a registry name was removed (actor unregistered).
pub const GOSSIP_REGISTRY_REMOVE: u8 = 6;

// ── SWIM message types ─────────────────────────────────────────────────

/// Ping request.
pub const SWIM_MSG_PING: i32 = 1;
/// Ping acknowledgement.
pub const SWIM_MSG_ACK: i32 = 2;
/// Indirect ping request (via intermediary).
pub const SWIM_MSG_PING_REQ: i32 = 3;
/// Membership event dissemination.
pub const SWIM_MSG_GOSSIP: i32 = 4;

// ── Data structures ────────────────────────────────────────────────────

/// Information about a cluster member.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct ClusterMember {
    /// Node ID (matches [`crate::pid`] encoding).
    pub node_id: u16,
    /// Member state (alive, suspect, dead, left).
    pub state: i32,
    /// Incarnation number — monotonically increasing to resolve
    /// conflicts (higher incarnation wins).
    pub incarnation: u64,
    /// Address string (e.g., "192.168.1.10:9000"). Null-terminated.
    /// Owned by the cluster; valid until the member is removed.
    pub addr: [u8; 128],
    /// Monotonic timestamp (ms) of last successful ping response.
    pub last_seen_ms: u64,
}

/// A membership event for gossip dissemination.
#[derive(Debug, Clone)]
struct MemberEvent {
    /// Node ID of the affected member.
    node_id: u16,
    /// New state (alive, suspect, dead, left).
    new_state: i32,
    /// Incarnation number.
    incarnation: u64,
    /// How many times this event has been piggybacked.
    dissemination_count: u32,
}

/// A registry event for gossip dissemination.
///
/// Propagates actor name registrations and removals across the cluster
/// so that [`crate::hew_node::hew_node_lookup`] can resolve remote actors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegistryEvent {
    /// The registered actor name.
    pub name: String,
    /// Actor ID (PID) for this registration; `0` for removal events.
    pub actor_id: u64,
    /// Whether this is an add (`true`) or remove (`false`) event.
    pub is_add: bool,
    /// How many times this event has been piggybacked.
    dissemination_count: u32,
}

#[derive(Debug, Default)]
struct ConnectionTokens {
    current: HashMap<u16, u64>,
    visible: HashMap<u16, u64>,
}

#[derive(Debug)]
enum PublicationTransition {
    Plain,
    TokenEstablished(u64),
    GuardedTokenEstablished {
        publication_token: u64,
        publication_sync: Arc<Mutex<()>>,
        publication_removed: Arc<AtomicBool>,
    },
    TokenLost(u64),
}

#[derive(Debug)]
struct MemberTransition {
    node_id: u16,
    state: i32,
    incarnation: u64,
    is_new_member: bool,
    old_state: Option<i32>,
    publication: PublicationTransition,
}

impl MemberTransition {
    fn membership_event(&self) -> Option<u8> {
        match self.state {
            MEMBER_ALIVE => {
                if self.is_new_member
                    || matches!(self.old_state, Some(prev) if prev != MEMBER_ALIVE)
                {
                    Some(HEW_MEMBERSHIP_EVENT_NODE_JOINED)
                } else {
                    None
                }
            }
            MEMBER_SUSPECT => Some(HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
            MEMBER_DEAD => Some(HEW_MEMBERSHIP_EVENT_NODE_DEAD),
            MEMBER_LEFT => Some(HEW_MEMBERSHIP_EVENT_NODE_LEFT),
            _ => None,
        }
    }

    fn with_publication(mut self, publication: PublicationTransition) -> Self {
        self.publication = publication;
        self
    }
}

#[derive(Debug, Default)]
struct PendingMemberTransitions {
    queue: VecDeque<MemberTransition>,
    draining: bool,
}

/// Callback for registry gossip notifications.
///
/// Signature: `fn(name: *const c_char, actor_id: u64, is_add: bool, user_data: *mut c_void)`.
pub type HewRegistryGossipCallback = extern "C" fn(*const c_char, u64, bool, *mut c_void);

/// Cluster configuration.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct ClusterConfig {
    /// This node's ID.
    pub local_node_id: u16,
    /// Protocol period in milliseconds (how often to ping).
    pub protocol_period_ms: u32,
    /// Ping timeout in milliseconds.
    pub ping_timeout_ms: u32,
    /// Suspect timeout in milliseconds (before declaring dead).
    pub suspect_timeout_ms: u32,
    /// Number of indirect ping targets (K in SWIM).
    pub indirect_ping_count: u32,
    /// Maximum gossip events to piggyback per message.
    pub max_gossip_per_msg: u32,
}

impl Default for ClusterConfig {
    fn default() -> Self {
        Self {
            local_node_id: 0,
            protocol_period_ms: 1000,
            ping_timeout_ms: 500,
            suspect_timeout_ms: 3000,
            indirect_ping_count: 3,
            max_gossip_per_msg: 8,
        }
    }
}

/// Callback for membership change notifications.
///
/// Signature: `fn(node_id: u16, new_state: i32, incarnation: u64)`
type MemberChangeCallback = unsafe extern "C" fn(u16, i32, u64);

/// Callback for connection-lifecycle-integrated membership notifications.
///
/// Signature: `fn(node_id: u16, event: u8, user_data: *mut c_void)`.
pub type HewMembershipCallback = extern "C" fn(u16, u8, *mut c_void);

/// Snapshot of the currently installed membership callback slot.
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct MembershipCallbackBinding {
    pub(crate) callback: Option<HewMembershipCallback>,
    user_data_bits: usize,
}

impl MembershipCallbackBinding {
    pub(crate) fn new(callback: Option<HewMembershipCallback>, user_data: *mut c_void) -> Self {
        Self {
            callback,
            user_data_bits: user_data as usize,
        }
    }

    pub(crate) fn user_data(self) -> *mut c_void {
        self.user_data_bits as *mut c_void
    }
}

// WHY: MembershipCallbackGeneration and its accessors were consumed only by
// remote_sup.rs (now deleted), so this epoch machinery is currently dead —
// `hew_cluster_replace_membership_callback` only writes the binding and does
// not exercise generation()/in_flight()/invoke(). Retained under
// #[allow(dead_code)] as the membership-callback epoch discipline a future
// supervision protocol would build on.
// WHEN obsolete: when that protocol lands and consumes it, or it is removed.
#[allow(
    dead_code,
    reason = "sole consumer was remote_sup.rs (deleted); epoch discipline retained for replace_membership_callback"
)]
#[derive(Clone, Debug, Default)]
pub(crate) struct MembershipCallbackGeneration {
    binding: MembershipCallbackBinding,
    epoch: Arc<MembershipCallbackEpoch>,
}

impl MembershipCallbackGeneration {
    #[allow(dead_code, reason = "sole consumer was remote_sup.rs (deleted)")]
    fn new(binding: MembershipCallbackBinding, epoch: Arc<MembershipCallbackEpoch>) -> Self {
        Self { binding, epoch }
    }

    #[allow(dead_code, reason = "sole consumer was remote_sup.rs (deleted)")]
    pub(crate) fn binding(&self) -> MembershipCallbackBinding {
        self.binding
    }

    #[allow(dead_code, reason = "sole consumer was remote_sup.rs (deleted)")]
    pub(crate) fn in_flight(&self) -> usize {
        self.epoch.in_flight()
    }

    #[allow(dead_code, reason = "sole consumer was remote_sup.rs (deleted)")]
    pub(crate) fn invoke(&self, node_id: u16, event: u8) {
        if let Some(callback) = self.binding.callback {
            callback(node_id, event, self.binding.user_data());
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct MembershipCallbackEpoch {
    in_flight: AtomicUsize,
}

impl MembershipCallbackEpoch {
    fn begin_dispatch(self: &Arc<Self>) -> MembershipCallbackDispatchGuard {
        self.in_flight.fetch_add(1, Ordering::AcqRel);
        MembershipCallbackDispatchGuard {
            epoch: Arc::clone(self),
        }
    }

    #[allow(dead_code, reason = "sole consumer was remote_sup.rs (deleted)")]
    pub(crate) fn in_flight(&self) -> usize {
        self.in_flight.load(Ordering::Acquire)
    }
}

#[derive(Debug)]
struct MembershipCallbackDispatchGuard {
    epoch: Arc<MembershipCallbackEpoch>,
}

impl Drop for MembershipCallbackDispatchGuard {
    fn drop(&mut self) {
        self.epoch.in_flight.fetch_sub(1, Ordering::AcqRel);
    }
}

/// The cluster membership manager.
#[derive(Debug)]
pub struct HewCluster {
    /// Cluster configuration.
    config: ClusterConfig,
    /// Current membership list (protected by mutex for thread safety).
    members: Mutex<Vec<ClusterMember>>,
    /// Durable authenticated session incarnation per receiver-local route slot.
    sessions: Mutex<HashMap<u16, u32>>,
    /// Current connection publication token per peer.
    connection_tokens: Mutex<ConnectionTokens>,
    /// Deferred membership transitions waiting to notify callbacks.
    pending_member_transitions: Mutex<PendingMemberTransitions>,
    /// Recent membership events for gossip dissemination.
    events: Mutex<VecDeque<MemberEvent>>,
    /// Recent registry events for gossip dissemination.
    registry_events: Mutex<VecDeque<RegistryEvent>>,
    /// Our own incarnation number.
    ///
    /// Atomic because incarnation self-refutation (C5) bumps it from the
    /// connection reader thread (on receiving SUSPECT-about-self) while the
    /// SWIM driver thread reads it to stamp outbound PING/ACK frames.
    local_incarnation: AtomicU64,
    /// Membership change callback.
    callback: Option<MemberChangeCallback>,
    /// Membership event callback binding.
    membership_callback_binding: Mutex<MembershipCallbackBinding>,
    /// Tracks in-flight membership callback dispatches.
    membership_callback_epoch: Arc<MembershipCallbackEpoch>,
    /// Registry gossip callback.
    registry_callback: Option<HewRegistryGossipCallback>,
    /// User data for [`HewRegistryGossipCallback`].
    registry_callback_user_data: *mut c_void,
    /// Monotonic timestamp of last tick.
    ///
    /// Atomic so `tick` takes `&self` (the SWIM driver thread drives the
    /// cluster through a shared reference).
    last_tick_ms: AtomicU64,
    /// Protocol strategy (SWIM message handlers + tick-transition logic).
    ///
    /// Holds `Box<dyn ClusterProtocol>` so the v0.6 Lifeguard
    /// implementation can replace `SimpleSwim` without changing any FFI
    /// surface or membership-callback semantics.
    protocol: Box<dyn ClusterProtocol>,
    /// Per-peer phi-accrual failure detectors.
    ///
    /// Locked after `members` (members → detectors) to avoid deadlock.
    /// Heartbeat observations and tick-time phi queries are the only
    /// sites that touch this mutex.  The detectors are orthogonal to
    /// `protocol`: they produce the `phi_snapshot` the protocol consumes
    /// in `compute_tick_transitions`, but the detector itself is owned
    /// here, not by the protocol.
    detectors: Mutex<HashMap<u16, PhiAccrualDetector>>,
    /// Partition registry for the partition-injection seam.
    ///
    /// When a peer transitions to `MEMBER_DEAD`, the cluster calls
    /// `partition_registry.on_member_dead(node_id)` to fan out
    /// `RecvError::PartitionDetected` to all registered queues.
    ///
    /// `None` (the default) means no queues are registered and the
    /// fan-out is a no-op — backward-compatible with all existing callers.
    partition_registry: Option<Arc<PartitionRegistry>>,
}

/// Maximum number of gossip events to retain.
const MAX_GOSSIP_EVENTS: usize = 64;

// ── ClusterProtocol trait ──────────────────────────────────────────────

/// The decision a protocol handler asks the cluster to apply after
/// processing one inbound SWIM message.
///
/// Using a decision struct (rather than calling cluster methods directly
/// from inside the trait) keeps `ClusterProtocol` impls free of any
/// borrow on the outer `HewCluster`, which makes `Box<dyn
/// ClusterProtocol>` on `HewCluster` work without self-borrow cycles.
#[derive(Debug, Default)]
pub struct ProtocolDecision {
    /// Record this peer's last-seen timestamp and advance its phi
    /// detector anchor.
    pub update_last_seen: bool,
    /// Additionally mark the peer `MEMBER_ALIVE` at the supplied
    /// incarnation (only meaningful when `update_last_seen` is also
    /// true).
    pub upsert_alive: bool,
}

/// A single state-transition decision produced by
/// [`ClusterProtocol::compute_tick_transitions`].
#[derive(Debug, Clone)]
pub struct StateChange {
    /// The peer whose state changes.
    pub node_id: u16,
    /// The new membership state (`MEMBER_SUSPECT` or `MEMBER_DEAD`).
    pub new_state: i32,
    /// The peer's incarnation number at the time of the decision.
    pub incarnation: u64,
}

/// Protocol strategy for the SWIM cluster substrate.
///
/// `ClusterProtocol` separates the *protocol decision logic* from the
/// *membership-state bookkeeping* held by [`HewCluster`].  The cluster
/// calls these methods, receives decisions, and applies them — so
/// alternative protocol implementations (e.g. SWIM-Lifeguard with
/// phi-accrual in v0.6) can be swapped behind this trait without
/// touching the FFI surface or the `MembershipCallback` semantics.
///
/// # Design
///
/// Implementations receive borrowed read-only state that is already
/// materialized by the caller (phi snapshot, member slice) so no
/// re-entrant lock is needed.  All mutations are requested through the
/// return type; the caller executes them.
///
/// # v0.5 / v0.6 boundary
///
/// v0.5 ships [`SimpleSwim`] behind this trait.  The v0.6 Lifeguard
/// implementation will carry its own adaptive-timeout logic and can
/// replace `SimpleSwim` without changing the FFI surface or any caller
/// of `HewCluster`.
pub trait ClusterProtocol: Send + Sync + std::fmt::Debug {
    /// Handle an inbound `SWIM_MSG_PING` from `from_node`.
    fn handle_ping(&self, from_node: u16) -> ProtocolDecision;

    /// Handle an inbound `SWIM_MSG_ACK` from `from_node`.
    fn handle_ack(&self, from_node: u16, incarnation: u64) -> ProtocolDecision;

    /// Handle an inbound `SWIM_MSG_PING_REQ` from `from_node`.
    fn handle_ping_req(&self, from_node: u16) -> ProtocolDecision;

    /// Handle an inbound `SWIM_MSG_GOSSIP` from `from_node`.
    ///
    /// Default: no-op — v0.5 gossip is piggybacked on other messages;
    /// a dedicated GOSSIP frame handler arrives in v0.6 / C3.
    fn handle_gossip(&self, _from_node: u16, _incarnation: u64) -> ProtocolDecision {
        ProtocolDecision::default()
    }

    /// Compute which member state transitions are due at `now_ms`.
    ///
    /// The caller passes:
    /// - `ping_timeout_ms` / `suspect_timeout_ms` — from [`ClusterConfig`].
    /// - `phi_snapshot` — per-peer `(phi_value, is_warm)` pre-computed
    ///   from the cluster's phi-accrual detectors (orthogonal to this
    ///   trait; detectors stay on [`HewCluster`]).
    /// - `members` — read-only snapshot of the current membership list.
    ///
    /// Returns only transitions that should be applied; dead / left
    /// members are already filtered out by the caller.
    fn compute_tick_transitions(
        &self,
        now_ms: u64,
        ping_timeout_ms: u64,
        suspect_timeout_ms: u64,
        phi_snapshot: &HashMap<u16, (f64, bool)>,
        members: &[ClusterMember],
    ) -> Vec<StateChange>;

    /// Choose the next ping target from `alive_members` (round-robin or
    /// implementation-defined selection).
    ///
    /// Returns `None` when the live set is empty.
    ///
    /// Takes `&self` (not `&mut self`): any selection cursor must live behind
    /// interior mutability so the SWIM driver thread and the connection reader
    /// thread can both drive the cluster through shared references without
    /// `&mut`-aliasing the same `HewCluster`.
    fn next_ping_target(&self, alive_members: &[u16]) -> Option<u16>;

    /// Called when a peer node transitions to DEAD.
    ///
    /// Implementations use this to drive the partition-injection seam:
    /// any local resources (e.g. duplexes registered in a `PartitionRegistry`)
    /// bound to `node_id` should be signalled with
    /// [`RecvError::PartitionDetected`] so blocked receivers wake with a
    /// typed failure.
    ///
    /// Default: no-op. Implementors that need partition fan-out override this
    /// method or register a `PartitionRegistry` on `HewCluster` directly.
    ///
    /// # Contract
    ///
    /// - Called exactly once per DEAD transition per node.
    /// - Called outside any cluster mutex.
    /// - Must not re-enter the cluster.
    fn on_member_dead(&self, _node_id: u16) {}
}

// ── PartitionRegistry ──────────────────────────────────────────────────

/// Maps remote `node_id`s to queues that receive on behalf of that node.
///
/// When the cluster declares a node DEAD, `on_member_dead` walks the
/// registry for that node, upgrades each `Weak<Queue>` (dropping dead ones),
/// and calls `Queue::force_partition()` on the live ones. Blocked receivers
/// wake with `RecvError::PartitionDetected`.
///
/// # Fail-closed contract
///
/// - Dead `Weak` refs are pruned on every `register_remote_queue` and
///   `on_member_dead` pass — no unbounded memory growth.
/// - `force_partition` on a live queue MUST resolve any pending `recv`
///   to `Err(RecvError::PartitionDetected)` (enforced by `Queue`).
/// - A `force_partition` on an already-dropped queue is silently discarded
///   (the `Weak` upgrade fails; that is the correct no-op path).
/// - If the registry has no entry for a dead node (no queues were bound),
///   `on_member_dead` is a no-op — not an error.
///
/// # CP-3 forward flag
///
/// C3 (SWIM driver, Phase 5) calls `on_member_dead` through
/// `ClusterProtocol::on_member_dead`; A5 (Phase 3) plugs in the datagram
/// transport that drives SWIM events. C2 owns only the seam shape.
#[derive(Debug, Default)]
pub struct PartitionRegistry {
    /// Node-ID → weak refs to all queues receiving from that node.
    queues: Mutex<HashMap<u16, Vec<Weak<Queue>>>>,
}

impl PartitionRegistry {
    /// Create a new, empty registry.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a queue as receiving data from `node_id`.
    ///
    /// When `node_id` is declared DEAD, `force_partition` will be called
    /// on this queue. The registry holds only a `Weak` ref — it does not
    /// extend the queue's lifetime.
    ///
    /// Dead weak refs for this `node_id` are pruned on entry.
    pub fn register_remote_queue(&self, node_id: u16, queue: Weak<Queue>) {
        let mut map = self.queues.lock_or_recover();
        let slot = map.entry(node_id).or_default();
        // Prune dead refs before appending; keeps the slot compact.
        slot.retain(|w| w.strong_count() > 0);
        slot.push(queue);
    }

    /// Fan out a partition signal to every live queue registered for `node_id`.
    ///
    /// Dead `Weak` refs are pruned from the slot as a side effect.
    /// If no queues are registered for `node_id`, this is a no-op (not an error).
    ///
    /// This is the core partition-injection seam: call it from
    /// `ClusterProtocol::on_member_dead` or from the `MEMBER_DEAD` callback
    /// in [`HewCluster`].
    pub fn on_member_dead(&self, node_id: u16) {
        let upgraded = {
            let mut map = self.queues.lock_or_recover();
            let Some(slot) = map.get_mut(&node_id) else {
                return;
            };
            // Upgrade live refs and prune dead ones atomically under the lock.
            let live: Vec<Arc<Queue>> = slot.iter().filter_map(Weak::upgrade).collect();
            slot.retain(|w| w.strong_count() > 0);
            live
        };
        // Call force_partition outside the lock so the queue's own mutex
        // is not nested under the registry mutex.
        if upgraded.is_empty() {
            eprintln!(
                "[partition] MEMBER_DEAD node_id={node_id}: no live queues registered (no-op)"
            );
        }
        for queue in &upgraded {
            queue.force_partition();
        }
    }
}

// ── SimpleSwim: the v0.5 ClusterProtocol implementation ───────────────

/// Simple SWIM protocol implementation — the v0.5 cluster protocol
/// strategy.
///
/// Implements the SWIM message handlers and a round-robin ping-target
/// selector.  Tick-time ALIVE→SUSPECT decisions defer to the phi-accrual
/// snapshot supplied by [`HewCluster::tick`] so the detector itself stays
/// orthogonal to the protocol (per A152 ratification).
///
/// # SHIM note
///
/// v0.5 indirect-ping forwarding (`SWIM_MSG_PING_REQ`) records the
/// sender's last-seen but does not yet issue a real forwarded ping to the
/// target — the actual fanout requires the C3 SWIM driver (protocol
/// runner + transport).  This is the correct "trampoline + caller handles
/// forwarding" contract from the original `process_message` comment.
/// When C3 lands it will either extend this impl or replace it via the
/// trait.
#[derive(Debug, Default)]
pub struct SimpleSwim {
    /// Cursor for round-robin ping target selection.
    ///
    /// Owned here (not on `HewCluster`) because it is purely
    /// SWIM-protocol-driver state; a Lifeguard impl may use a different
    /// target-selection strategy. Atomic so `next_ping_target` takes `&self`
    /// (the driver and reader threads share the protocol via `&self`).
    ping_index: AtomicUsize,
}

impl SimpleSwim {
    /// Create a new `SimpleSwim` instance.
    #[must_use]
    pub fn new() -> Self {
        Self {
            ping_index: AtomicUsize::new(0),
        }
    }
}

impl ClusterProtocol for SimpleSwim {
    fn handle_ping(&self, _from_node: u16) -> ProtocolDecision {
        ProtocolDecision {
            update_last_seen: true,
            upsert_alive: false,
        }
    }

    fn handle_ack(&self, _from_node: u16, _incarnation: u64) -> ProtocolDecision {
        ProtocolDecision {
            update_last_seen: true,
            upsert_alive: true,
        }
    }

    fn handle_ping_req(&self, _from_node: u16) -> ProtocolDecision {
        // SHIM: record last-seen for the intermediary; actual forwarding
        // to the target is the caller's (C3 driver's) responsibility.
        // WHY: C3 SWIM driver not yet wired; this preserves the pre-C1
        //   "caller handles forwarding" trampoline contract.
        // WHEN obsolete: when C3 supplies a real indirect-ping sender.
        // REAL solution: ClusterProtocol gains a `send_indirect_ping`
        //   callback or the C3 runner wraps the trait for transport access.
        ProtocolDecision {
            update_last_seen: true,
            upsert_alive: false,
        }
    }

    fn compute_tick_transitions(
        &self,
        now_ms: u64,
        ping_timeout_ms: u64,
        suspect_timeout_ms: u64,
        phi_snapshot: &HashMap<u16, (f64, bool)>,
        members: &[ClusterMember],
    ) -> Vec<StateChange> {
        let mut changes = Vec::new();
        for member in members {
            if member.state == MEMBER_DEAD || member.state == MEMBER_LEFT {
                continue;
            }
            let elapsed = now_ms.saturating_sub(member.last_seen_ms);
            if member.state == MEMBER_SUSPECT && elapsed > suspect_timeout_ms {
                changes.push(StateChange {
                    node_id: member.node_id,
                    new_state: MEMBER_DEAD,
                    incarnation: member.incarnation,
                });
            } else if member.state == MEMBER_ALIVE {
                let (phi, warm) = phi_snapshot
                    .get(&member.node_id)
                    .copied()
                    .unwrap_or((0.0, false));
                let suspect = if warm {
                    phi > PHI_THRESHOLD
                } else {
                    elapsed > ping_timeout_ms
                };
                if suspect {
                    changes.push(StateChange {
                        node_id: member.node_id,
                        new_state: MEMBER_SUSPECT,
                        incarnation: member.incarnation,
                    });
                }
            }
        }
        changes
    }

    fn next_ping_target(&self, alive_members: &[u16]) -> Option<u16> {
        if alive_members.is_empty() {
            return None;
        }
        // Atomically pick-and-advance the cursor modulo the live set so two
        // concurrent callers never select the same slot twice in a row.
        let len = alive_members.len();
        let mut current = self.ping_index.load(Ordering::Relaxed);
        loop {
            let slot = current % len;
            let next = (slot + 1) % len;
            match self.ping_index.compare_exchange_weak(
                current,
                next,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => return Some(alive_members[slot]),
                Err(observed) => current = observed,
            }
        }
    }
}

// ── Phi-accrual failure-detector tuning ────────────────────────────────
//
// These constants tune the phi-accrual replacement of the legacy
// fixed-threshold ALIVE → SUSPECT trigger. They are module-level (not
// fields on the FFI-stable [`ClusterConfig`]) so that future re-tuning
// is a one-line change without breaking the C ABI.
//
// Defaults match the production-SOTA choices used by Akka, Cassandra,
// and Hashicorp memberlist:
//   - threshold 8.0 ⇒ "very likely dead" (≤10⁻⁸ probability under the
//     learned distribution)
//   - 200-sample sliding window — enough to track minutes of cadence
//     at a 1Hz heartbeat without becoming sluggish to react.
//   - 10-sample warm-up — below this we fall back to `ping_timeout_ms`
//     so brand-new peers and quiet peers are not blind-spots.

/// Suspect a peer when its phi value exceeds this threshold.
const PHI_THRESHOLD: f64 = 8.0;
/// Maximum number of inter-arrival samples retained per peer.
const PHI_WINDOW_SIZE: usize = 200;
/// Minimum interval samples before phi is consulted; below this
/// the legacy `ping_timeout_ms` fixed threshold is used.
const PHI_MIN_SAMPLES: usize = 10;

// ── Core protocol logic ────────────────────────────────────────────────

impl HewCluster {
    /// Create a new cluster instance.
    fn new(config: ClusterConfig) -> Self {
        Self {
            config,
            members: Mutex::new(Vec::with_capacity(16)),
            sessions: Mutex::new(HashMap::new()),
            connection_tokens: Mutex::new(ConnectionTokens::default()),
            pending_member_transitions: Mutex::new(PendingMemberTransitions::default()),
            events: Mutex::new(VecDeque::with_capacity(MAX_GOSSIP_EVENTS)),
            registry_events: Mutex::new(VecDeque::with_capacity(MAX_GOSSIP_EVENTS)),
            local_incarnation: AtomicU64::new(1),
            callback: None,
            membership_callback_binding: Mutex::new(MembershipCallbackBinding::default()),
            membership_callback_epoch: Arc::new(MembershipCallbackEpoch::default()),
            registry_callback: None,
            registry_callback_user_data: std::ptr::null_mut(),
            last_tick_ms: AtomicU64::new(0),
            protocol: Box::new(SimpleSwim::new()),
            detectors: Mutex::new(HashMap::new()),
            partition_registry: None,
        }
    }

    /// Install a `PartitionRegistry` on this cluster.
    ///
    /// Once installed, every `MEMBER_DEAD` transition fans out
    /// `RecvError::PartitionDetected` to all queues registered in the
    /// registry for the dead node. Installing a second registry replaces
    /// the first.
    pub fn set_partition_registry(&mut self, registry: Arc<PartitionRegistry>) {
        self.partition_registry = Some(registry);
    }

    /// Add or update a member in the membership list.
    ///
    /// `gossip_announced` is `true` when `incarnation` comes from a peer's own
    /// SWIM frame (ping/ack/gossip) — i.e. the peer announced this incarnation.
    /// It is `false` for a locally-synthesized incarnation bump (a TCP reconnect
    /// raising a SUSPECT peer back to ALIVE). Only a genuinely peer-announced,
    /// strictly higher incarnation may readmit a DEAD/LEFT node: a local reconnect
    /// is not proof the buried peer is back, so it can never resurrect it.
    fn stage_member_transition_locked(
        members: &mut Vec<ClusterMember>,
        node_id: u16,
        state: i32,
        incarnation: u64,
        addr: &[u8],
        gossip_announced: bool,
    ) -> Option<MemberTransition> {
        if let Some(existing) = members.iter_mut().find(|m| m.node_id == node_id) {
            let existing_terminal = existing.state == MEMBER_DEAD || existing.state == MEMBER_LEFT;
            if existing_terminal && state == MEMBER_ALIVE {
                // Readmission is monotonic and fail-closed: a dead or gracefully
                // left node returns ONLY when a peer announces a strictly higher
                // incarnation. A <=-incarnation ALIVE is stale gossip about the
                // buried identity and is refused — it can never resurrect the node.
                // A locally-synthesized bump (a reconnect) is likewise refused; it
                // is not proof the peer restarted with a fresh identity.
                if !gossip_announced || incarnation <= existing.incarnation {
                    return None;
                }
                // strictly higher, peer-announced -> fall through to the LWW admit
                // below. The transition it returns carries old_state = terminal,
                // which the dispatch keys the quarantine eviction on.
                eprintln!(
                    "[cluster] readmitting node {node_id} at incarnation {incarnation} \
                     (was {} at {})",
                    existing.state, existing.incarnation
                );
            }
            if incarnation > existing.incarnation
                || (incarnation == existing.incarnation && state > existing.state)
            {
                let old_state = existing.state;
                existing.state = state;
                existing.incarnation = incarnation;
                if !addr.is_empty() {
                    let len = addr.len().min(127);
                    existing.addr[..len].copy_from_slice(&addr[..len]);
                    existing.addr[len] = 0;
                }
                return Some(MemberTransition {
                    node_id,
                    state,
                    incarnation,
                    is_new_member: false,
                    old_state: Some(old_state),
                    publication: PublicationTransition::Plain,
                });
            }
            return None;
        }

        let mut member = ClusterMember {
            node_id,
            state,
            incarnation,
            addr: [0u8; 128],
            // Initialise to the current monotonic time so the first tick does
            // not immediately mark a brand-new member as suspect.
            // SAFETY: hew_now_ms has no preconditions.
            last_seen_ms: unsafe { crate::io_time::hew_now_ms() },
        };
        let len = addr.len().min(127);
        member.addr[..len].copy_from_slice(&addr[..len]);
        members.push(member);
        Some(MemberTransition {
            node_id,
            state,
            incarnation,
            is_new_member: true,
            old_state: None,
            publication: PublicationTransition::Plain,
        })
    }

    fn queue_member_transition(&self, transition: MemberTransition) -> bool {
        let mut pending = self.pending_member_transitions.lock_or_recover();
        pending.queue.push_back(transition);
        if pending.draining {
            return false;
        }
        pending.draining = true;
        true
    }

    fn drain_member_transitions(&self) {
        loop {
            let transition = {
                let mut pending = self.pending_member_transitions.lock_or_recover();
                if let Some(transition) = pending.queue.pop_front() {
                    transition
                } else {
                    pending.draining = false;
                    return;
                }
            };
            self.deliver_member_transition(&transition);
        }
    }

    fn deliver_member_transition(&self, transition: &MemberTransition) {
        let membership_event = transition.membership_event();
        let deliver = match &transition.publication {
            PublicationTransition::Plain => {
                if matches!(membership_event, Some(HEW_MEMBERSHIP_EVENT_NODE_JOINED)) {
                    let mut tokens = self.connection_tokens.lock_or_recover();
                    if let Some(publication_token) =
                        tokens.current.get(&transition.node_id).copied()
                    {
                        tokens.visible.insert(transition.node_id, publication_token);
                    }
                }
                true
            }
            PublicationTransition::TokenEstablished(publication_token) => {
                let mut tokens = self.connection_tokens.lock_or_recover();
                match tokens.current.get(&transition.node_id) {
                    Some(current) if *current == *publication_token => {
                        tokens
                            .visible
                            .insert(transition.node_id, *publication_token);
                        true
                    }
                    _ => false,
                }
            }
            PublicationTransition::GuardedTokenEstablished {
                publication_token,
                publication_sync,
                publication_removed,
            } => {
                let _publication = publication_sync.lock_or_recover();
                let mut tokens = self.connection_tokens.lock_or_recover();
                let should_deliver = if publication_removed.load(Ordering::Acquire) {
                    false
                } else {
                    match tokens.current.get(&transition.node_id) {
                        Some(current) if *current == *publication_token => {
                            let members = self.members.lock_or_recover();
                            matches!(
                                members.iter().find(|m| m.node_id == transition.node_id),
                                Some(member)
                                    if member.state == transition.state
                                        && member.incarnation == transition.incarnation
                            )
                        }
                        _ => false,
                    }
                };
                if should_deliver {
                    tokens
                        .visible
                        .insert(transition.node_id, *publication_token);
                    true
                } else {
                    if matches!(
                        tokens.current.get(&transition.node_id),
                        Some(current) if *current == *publication_token
                    ) {
                        tokens.current.remove(&transition.node_id);
                    }
                    if matches!(
                        tokens.visible.get(&transition.node_id),
                        Some(current) if *current == *publication_token
                    ) {
                        tokens.visible.remove(&transition.node_id);
                    }
                    false
                }
            }
            PublicationTransition::TokenLost(publication_token) => {
                let mut tokens = self.connection_tokens.lock_or_recover();
                match tokens.visible.get(&transition.node_id) {
                    Some(current) if *current == *publication_token => {
                        tokens.visible.remove(&transition.node_id);
                        true
                    }
                    _ => false,
                }
            }
        };
        if !deliver {
            return;
        }

        self.emit_event(transition.node_id, transition.state, transition.incarnation);
        self.notify_callback(transition.node_id, transition.state, transition.incarnation);
        if let Some(event) = membership_event {
            let _ = self.with_membership_callback_dispatch(|callback, user_data| {
                callback(transition.node_id, event, user_data);
            });
        }
        self.apply_member_transition_side_effects(transition);
    }

    /// Run the per-peer-table reactions a delivered transition triggers: the
    /// `MEMBER_DEAD` fan-outs (partition recv-queue, protocol, cross-node
    /// monitors, pending asks + quarantine) and the readmission eviction. Kept
    /// separate from the publication-token bookkeeping in
    /// `deliver_member_transition` so each stays a single, readable concern.
    fn apply_member_transition_side_effects(&self, transition: &MemberTransition) {
        // Partition-injection seam: fan out PartitionDetected to all queues
        // registered for this node when it transitions to DEAD.
        if transition.state == MEMBER_DEAD {
            if let Some(registry) = &self.partition_registry {
                registry.on_member_dead(transition.node_id);
            }
            self.protocol.on_member_dead(transition.node_id);
            // Cross-node monitor SWIM-DEAD fan-out: the peer is gone, so arm
            // MonitorLost for every still-pending local watcher of an actor on
            // it (exactly-once; a prior definitive DOWN wins).
            crate::hew_node::fan_out_monitor_lost_for_node(transition.node_id);
            // Pending-ask SWIM-DEAD fan-out + quarantine: the recv-queue seam
            // above wakes blocked recvs to PartitionDetected; this resolves every
            // pending remote ask routed to the dead peer with AskError::Partition
            // immediately (instead of hanging to the caller's deadline) AND
            // quarantines the peer at the incarnation it died at so a
            // Quarantine-policy send/ask fails closed until it rejoins higher.
            crate::hew_node::fail_remote_and_quarantine(transition.node_id, transition.incarnation);
        }
        // Readmission seam: a buried peer (DEAD or LEFT) returning ALIVE is the
        // strictly-higher-incarnation rejoin the admission gate already authorized
        // (a <=-incarnation ALIVE never produces this transition). Evict its
        // quarantine entry exactly once so it is sendable again. An ALIVE->ALIVE
        // incarnation bump has old_state = ALIVE and is skipped — nothing to evict.
        if transition.state == MEMBER_ALIVE
            && matches!(transition.old_state, Some(MEMBER_DEAD | MEMBER_LEFT))
        {
            crate::hew_node::readmit_node_clear_quarantine(transition.node_id);
        }
    }

    fn upsert_member(&self, node_id: u16, state: i32, incarnation: u64, addr: &[u8]) {
        let mut should_drain = false;
        {
            let mut members = self.members.lock_or_recover();
            // Gossip path: `incarnation` is what a peer announced, so a strictly
            // higher one may readmit a buried node.
            if let Some(transition) = Self::stage_member_transition_locked(
                &mut members,
                node_id,
                state,
                incarnation,
                addr,
                true,
            ) {
                should_drain = self.queue_member_transition(transition);
            }
        }
        if should_drain {
            self.drain_member_transitions();
        }
    }

    /// Admit an authenticated durable node session for a receiver-local route
    /// slot. Equal-session reconnects may revive only non-terminal membership;
    /// a strictly higher session starts a fresh SWIM incarnation domain.
    #[cfg(test)]
    #[allow(
        dead_code,
        reason = "called by session admission tests introduced in the following commit"
    )]
    pub(crate) fn admit_authenticated_session(&self, node_id: u16, session: u32) -> bool {
        if node_id == 0 || node_id == self.config.local_node_id || session == 0 {
            return false;
        }

        let mut should_drain = false;
        let accepted = {
            let mut sessions = self.sessions.lock_or_recover();
            let mut members = self.members.lock_or_recover();
            let current_session = sessions.get(&node_id).copied();
            let member_index = members.iter().position(|member| member.node_id == node_id);

            match current_session {
                Some(current) if session < current => false,
                Some(current) if session == current => {
                    let Some(index) = member_index else {
                        return false;
                    };
                    let member = &mut members[index];
                    if matches!(member.state, MEMBER_DEAD | MEMBER_LEFT) {
                        false
                    } else if member.state == MEMBER_ALIVE {
                        // SAFETY: hew_now_ms has no preconditions.
                        member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                        true
                    } else {
                        let next_incarnation = member.incarnation.saturating_add(1);
                        if let Some(transition) = Self::stage_member_transition_locked(
                            &mut members,
                            node_id,
                            MEMBER_ALIVE,
                            next_incarnation,
                            &[],
                            false,
                        ) {
                            should_drain = self.queue_member_transition(transition);
                        }
                        true
                    }
                }
                _ => {
                    sessions.insert(node_id, session);
                    if let Some(index) = member_index {
                        let member = &mut members[index];
                        let old_state = member.state;
                        member.state = MEMBER_ALIVE;
                        member.incarnation = 1;
                        // SAFETY: hew_now_ms has no preconditions.
                        member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                        should_drain = self.queue_member_transition(MemberTransition {
                            node_id,
                            state: MEMBER_ALIVE,
                            incarnation: 1,
                            is_new_member: false,
                            old_state: Some(old_state),
                            publication: PublicationTransition::Plain,
                        });
                    } else {
                        let mut member = ClusterMember {
                            node_id,
                            state: MEMBER_ALIVE,
                            incarnation: 1,
                            addr: [0; 128],
                            // SAFETY: hew_now_ms has no preconditions.
                            last_seen_ms: unsafe { crate::io_time::hew_now_ms() },
                        };
                        member.addr[0] = 0;
                        members.push(member);
                        should_drain = self.queue_member_transition(MemberTransition {
                            node_id,
                            state: MEMBER_ALIVE,
                            incarnation: 1,
                            is_new_member: true,
                            old_state: None,
                            publication: PublicationTransition::Plain,
                        });
                    }
                    true
                }
            }
        };
        if should_drain {
            self.drain_member_transitions();
        }
        accepted
    }

    /// Stage-1 compatibility helper for tests and low-level callers.
    #[cfg(test)]
    pub(crate) fn admit_handshake_peer(&self, node_id: u16) {
        if self.member_incarnation(node_id).is_none() {
            self.upsert_member(node_id, MEMBER_ALIVE, 1, &[]);
        }
    }

    /// Current durable session for a receiver-local route slot.
    #[allow(dead_code, reason = "used by connection.rs in the next commit")]
    pub(crate) fn member_session(&self, node_id: u16) -> Option<u32> {
        self.sessions.lock_or_recover().get(&node_id).copied()
    }

    /// Seed a member directly into the table without running the transition
    /// dispatch fan-outs. Used by cross-module tests that need the membership
    /// table to resolve a live incarnation for a node without also triggering the
    /// DEAD fan-out / quarantine side-effects.
    #[cfg(test)]
    pub(crate) fn seed_member_for_test(&self, node_id: u16, state: i32, incarnation: u64) {
        let mut members = self.members.lock_or_recover();
        if let Some(existing) = members.iter_mut().find(|m| m.node_id == node_id) {
            existing.state = state;
            existing.incarnation = incarnation;
        } else {
            members.push(ClusterMember {
                node_id,
                state,
                incarnation,
                addr: [0u8; 128],
                // SAFETY: hew_now_ms has no preconditions.
                last_seen_ms: unsafe { crate::io_time::hew_now_ms() },
            });
        }
    }

    /// Emit a gossip event.
    fn emit_event(&self, node_id: u16, new_state: i32, incarnation: u64) {
        let mut events = self.events.lock_or_recover();
        // Deduplicate: remove older event for this node.
        events.retain(|e| e.node_id != node_id);
        // Add new event.
        if events.len() >= MAX_GOSSIP_EVENTS {
            events.pop_front();
        }
        events.push_back(MemberEvent {
            node_id,
            new_state,
            incarnation,
            dissemination_count: 0,
        });
    }

    /// Notify the callback if registered.
    fn notify_callback(&self, node_id: u16, state: i32, incarnation: u64) {
        if let Some(cb) = self.callback {
            // SAFETY: callback is a valid function pointer per caller contract.
            unsafe { cb(node_id, state, incarnation) };
        }
    }

    /// Notify event callback if registered.
    fn notify_membership_callback(
        &self,
        node_id: u16,
        state: i32,
        is_new_member: bool,
        old_state: Option<i32>,
    ) {
        let event = match state {
            MEMBER_ALIVE => {
                if is_new_member || matches!(old_state, Some(prev) if prev != MEMBER_ALIVE) {
                    Some(HEW_MEMBERSHIP_EVENT_NODE_JOINED)
                } else {
                    None
                }
            }
            MEMBER_SUSPECT => Some(HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
            MEMBER_DEAD => Some(HEW_MEMBERSHIP_EVENT_NODE_DEAD),
            MEMBER_LEFT => Some(HEW_MEMBERSHIP_EVENT_NODE_LEFT),
            _ => None,
        };
        if let Some(evt) = event {
            let _ = self.with_membership_callback_dispatch(|callback, user_data| {
                callback(node_id, evt, user_data);
            });
        }
    }

    #[allow(dead_code, reason = "sole consumer was remote_sup.rs (deleted)")]
    fn membership_callback_binding(&self) -> MembershipCallbackBinding {
        *self.membership_callback_binding.lock_or_recover()
    }

    #[allow(dead_code, reason = "sole consumer was remote_sup.rs (deleted)")]
    fn membership_callback_generation(&self) -> MembershipCallbackGeneration {
        MembershipCallbackGeneration::new(
            self.membership_callback_binding(),
            Arc::clone(&self.membership_callback_epoch),
        )
    }

    /// Snapshot the current membership callback binding and enter the
    /// dispatch epoch before releasing the binding lock.
    fn with_membership_callback_dispatch<R>(
        &self,
        invoke: impl FnOnce(HewMembershipCallback, *mut c_void) -> R,
    ) -> Option<R> {
        let binding_guard = self.membership_callback_binding.lock_or_recover();
        let binding = *binding_guard;
        let callback = binding.callback?;
        let user_data = binding.user_data();
        let _dispatch_guard = self.membership_callback_epoch.begin_dispatch();
        drop(binding_guard);
        Some(invoke(callback, user_data))
    }

    /// Get pending gossip events (up to `max_count`), incrementing
    /// dissemination counters and pruning expired events.
    #[cfg_attr(
        not(test),
        expect(
            dead_code,
            reason = "used when wiring gossip into SWIM message piggybacking"
        )
    )]
    fn take_gossip(&self, max_count: usize) -> Vec<MemberEvent> {
        let mut events = self.events.lock_or_recover();
        let mut result = Vec::with_capacity(max_count);
        for event in events.iter_mut() {
            if result.len() >= max_count {
                break;
            }
            result.push(event.clone());
            event.dissemination_count += 1;
        }
        // Remove events that have been disseminated enough times.
        events.retain(|e| e.dissemination_count < 8);
        result
    }

    /// Process a received SWIM message, delegating protocol decisions to
    /// the installed [`ClusterProtocol`] strategy.
    ///
    /// `pub(crate)` so the connection reader thread can drive the SWIM state
    /// machine directly when a `CTRL_SWIM` control frame arrives, without
    /// round-tripping through the FFI wrapper.
    pub(crate) fn process_message(
        &self,
        msg_type: i32,
        from_node: u16,
        incarnation: u64,
        source_conn_node_id: u16,
    ) {
        if from_node != source_conn_node_id {
            eprintln!(
                "[cluster] rejecting message: from_node {from_node} doesn't match connection node {source_conn_node_id}"
            );
            return;
        }
        let decision = match msg_type {
            SWIM_MSG_PING => self.protocol.handle_ping(from_node),
            SWIM_MSG_ACK => self.protocol.handle_ack(from_node, incarnation),
            SWIM_MSG_PING_REQ => self.protocol.handle_ping_req(from_node),
            SWIM_MSG_GOSSIP => self.protocol.handle_gossip(from_node, incarnation),
            _ => return,
        };
        if decision.update_last_seen {
            self.update_last_seen(from_node);
        }
        if decision.upsert_alive {
            self.upsert_member(from_node, MEMBER_ALIVE, incarnation, &[]);
        }
    }

    /// Update `last_seen_ms` for a member, and record the heartbeat
    /// into that peer's phi-accrual detector.
    ///
    /// If the peer is recovering from `SUSPECT`, the recovery interval
    /// is intentionally *not* fed into the distribution — folding a
    /// multi-second silence into the window would teach the detector
    /// that long gaps are normal and dull all future detections. We
    /// still advance the detector's anchor so the next genuine interval
    /// is measured correctly.
    fn update_last_seen(&self, node_id: u16) {
        // SAFETY: hew_now_ms has no preconditions.
        let now = unsafe { crate::io_time::hew_now_ms() };
        let was_suspect = {
            let mut members = self.members.lock_or_recover();
            if let Some(m) = members.iter_mut().find(|m| m.node_id == node_id) {
                m.last_seen_ms = now;
                let was_suspect = m.state == MEMBER_SUSPECT;
                if was_suspect {
                    m.state = MEMBER_ALIVE;
                }
                Some(was_suspect)
            } else {
                None
            }
        };
        if let Some(was_suspect) = was_suspect {
            // members → detectors lock order.
            let mut detectors = self.detectors.lock_or_recover();
            let det = detectors
                .entry(node_id)
                .or_insert_with(|| PhiAccrualDetector::new(PHI_WINDOW_SIZE, PHI_MIN_SAMPLES));
            if was_suspect {
                det.heartbeat_anchor_only(now);
            } else {
                det.heartbeat(now);
            }
        }
    }

    /// Advance the protocol: check for suspects and dead members.
    ///
    /// The ALIVE → SUSPECT transition uses the phi-accrual failure
    /// detector once the per-peer window holds at least
    /// [`PHI_MIN_SAMPLES`] inter-arrival samples (the "warm" state).
    /// While cold, the legacy `ping_timeout_ms` threshold is used as a
    /// conservative fallback so brand-new or quiet peers are not blind
    /// spots. The SUSPECT → DEAD escalation is unchanged.
    ///
    /// Note: this changes *when* a [`HEW_MEMBERSHIP_EVENT_NODE_SUSPECT`]
    /// fires, never *what* the consumer observes — the membership event
    /// ABI surface (`Partition`-equivalent) is preserved.
    ///
    /// The transition decision logic is delegated to the installed
    /// [`ClusterProtocol`] via [`ClusterProtocol::compute_tick_transitions`].
    /// The phi-accrual detector snapshot is computed here (detectors are
    /// orthogonal to the protocol: they stay on `HewCluster`) and
    /// supplied as a read-only argument to the protocol.
    fn tick(&self, now_ms: u64) {
        self.last_tick_ms.store(now_ms, Ordering::Relaxed);

        let suspect_timeout_ms = u64::from(self.config.suspect_timeout_ms);
        let ping_timeout_ms = u64::from(self.config.ping_timeout_ms);

        // Take a snapshot of (node_id, phi, is_warm) outside the
        // `members` lock so we respect the members → detectors lock
        // order. We hold detectors only briefly.
        let phi_snapshot: HashMap<u16, (f64, bool)> = {
            let detectors = self.detectors.lock_or_recover();
            detectors
                .iter()
                .map(|(id, det)| (*id, (det.phi(now_ms), det.is_warm())))
                .collect()
        };

        // Take a member snapshot to pass to the protocol.  We release
        // the lock before applying state changes so the apply loop can
        // re-lock for mutation without holding two locks.
        let member_snapshot: Vec<ClusterMember> = self.members.lock_or_recover().clone();

        // Delegate the transition decision to the protocol strategy.
        let state_changes = self.protocol.compute_tick_transitions(
            now_ms,
            ping_timeout_ms,
            suspect_timeout_ms,
            &phi_snapshot,
            &member_snapshot,
        );

        // Apply state transitions: update the authoritative members list,
        // emit gossip events, fire callbacks.
        for change in &state_changes {
            {
                let mut members = self.members.lock_or_recover();
                if let Some(m) = members.iter_mut().find(|m| m.node_id == change.node_id) {
                    // Only apply if the member hasn't moved on since the snapshot.
                    if m.state != MEMBER_DEAD && m.state != MEMBER_LEFT {
                        m.state = change.new_state;
                    }
                }
            }
            self.emit_event(change.node_id, change.new_state, change.incarnation);
            self.notify_callback(change.node_id, change.new_state, change.incarnation);
            self.notify_membership_callback(change.node_id, change.new_state, false, None);
            // Prune the detector once the peer has left the live set.
            if change.new_state == MEMBER_DEAD {
                self.detectors.lock_or_recover().remove(&change.node_id);
                // Partition-injection seam: fan out PartitionDetected to all
                // queues registered for the dead node.
                if let Some(registry) = &self.partition_registry {
                    registry.on_member_dead(change.node_id);
                }
                self.protocol.on_member_dead(change.node_id);
                // Cross-node monitor SWIM-DEAD fan-out: arm MonitorLost for
                // every still-pending local watcher of an actor on the dead
                // node (exactly-once; a prior definitive DOWN wins).
                crate::hew_node::fan_out_monitor_lost_for_node(change.node_id);
                // Pending-ask SWIM-DEAD fan-out + quarantine: resolve every
                // pending remote ask routed to the dead peer with
                // AskError::Partition immediately (mirroring the recv-queue
                // partition seam above) AND quarantine the peer at the incarnation
                // it died at.
                crate::hew_node::fail_remote_and_quarantine(change.node_id, change.incarnation);
            }
        }
    }

    /// Notify SWIM state machine that a connection dropped.
    fn notify_connection_lost(&self, node_id: u16) {
        let mut should_drain = false;
        let known_member = {
            let mut members = self.members.lock_or_recover();
            let member = members
                .iter()
                .find(|m| m.node_id == node_id)
                .map(|m| (m.state, m.incarnation));
            if let Some((state, incarnation)) = member {
                if state == MEMBER_ALIVE {
                    // Demote an ALIVE peer to SUSPECT on a dropped connection. Not
                    // a readmission (never terminal->ALIVE), so the gossip flag is
                    // immaterial; pass false.
                    if let Some(transition) = Self::stage_member_transition_locked(
                        &mut members,
                        node_id,
                        MEMBER_SUSPECT,
                        incarnation,
                        &[],
                        false,
                    ) {
                        should_drain = self.queue_member_transition(transition);
                    }
                }
                true
            } else {
                false
            }
        };

        if !known_member {
            eprintln!(
                "hew-runtime cluster warning: ignoring connection_lost for unknown node_id={node_id}"
            );
            return;
        }
        if should_drain {
            self.drain_member_transitions();
        }
    }

    fn notify_connection_lost_if_current(&self, node_id: u16, publication_token: u64) {
        let mut should_drain = false;
        let known_member = {
            let mut tokens = self.connection_tokens.lock_or_recover();
            match tokens.current.get(&node_id) {
                Some(current) if *current == publication_token => {
                    tokens.current.remove(&node_id);
                }
                _ => return,
            }

            let mut clear_visible_now = false;
            let known_member = {
                let mut members = self.members.lock_or_recover();
                let member = members
                    .iter()
                    .find(|m| m.node_id == node_id)
                    .map(|m| (m.state, m.incarnation));
                if let Some((state, incarnation)) = member {
                    if state == MEMBER_ALIVE {
                        // SUSPECT demotion on a token-current drop; not a
                        // readmission, so gossip_announced is immaterial.
                        if let Some(transition) = Self::stage_member_transition_locked(
                            &mut members,
                            node_id,
                            MEMBER_SUSPECT,
                            incarnation,
                            &[],
                            false,
                        ) {
                            should_drain =
                                self.queue_member_transition(transition.with_publication(
                                    PublicationTransition::TokenLost(publication_token),
                                ));
                        }
                    } else {
                        clear_visible_now = true;
                    }
                    true
                } else {
                    clear_visible_now = true;
                    false
                }
            };

            if clear_visible_now
                && matches!(
                    tokens.visible.get(&node_id),
                    Some(current) if *current == publication_token
                )
            {
                tokens.visible.remove(&node_id);
            }
            known_member
        };
        if !known_member {
            eprintln!(
                "hew-runtime cluster warning: ignoring connection_lost for unknown node_id={node_id}"
            );
            return;
        }
        if should_drain {
            self.drain_member_transitions();
        }
    }

    /// Notify SWIM state machine that a connection was established.
    fn notify_connection_established(&self, node_id: u16) {
        let mut should_drain = false;
        let known_member = {
            let mut members = self.members.lock_or_recover();
            let member = members
                .iter()
                .find(|m| m.node_id == node_id)
                .map(|m| (m.state, m.incarnation));
            if let Some((state, incarnation)) = member {
                if state == MEMBER_ALIVE {
                    if let Some(member) = members.iter_mut().find(|m| m.node_id == node_id) {
                        // SAFETY: hew_now_ms has no preconditions.
                        member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                    }
                } else {
                    // Locally-synthesized bump on reconnect: raises a SUSPECT peer
                    // back to ALIVE. gossip_announced=false so it can NEVER readmit
                    // a DEAD/LEFT node — a TCP reconnect is not proof the buried
                    // peer restarted with a fresh identity.
                    let incarnation = incarnation.saturating_add(1);
                    if let Some(transition) = Self::stage_member_transition_locked(
                        &mut members,
                        node_id,
                        MEMBER_ALIVE,
                        incarnation,
                        &[],
                        false,
                    ) {
                        if let Some(member) = members.iter_mut().find(|m| m.node_id == node_id) {
                            // SAFETY: hew_now_ms has no preconditions.
                            member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                        }
                        should_drain = self.queue_member_transition(transition);
                    }
                }
                true
            } else {
                false
            }
        };

        if !known_member {
            eprintln!("[cluster] unknown node {node_id} connected, waiting for join");
            return;
        }
        if should_drain {
            self.drain_member_transitions();
        }
    }

    fn notify_connection_established_for_token(&self, node_id: u16, publication_token: u64) {
        let mut should_drain = false;
        let known_member = {
            let mut tokens = self.connection_tokens.lock_or_recover();
            tokens.current.insert(node_id, publication_token);

            let mut members = self.members.lock_or_recover();
            let member = members
                .iter()
                .find(|m| m.node_id == node_id)
                .map(|m| (m.state, m.incarnation));
            if let Some((state, incarnation)) = member {
                if state == MEMBER_ALIVE {
                    if let Some(member) = members.iter_mut().find(|m| m.node_id == node_id) {
                        // SAFETY: hew_now_ms has no preconditions.
                        member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                    }
                    tokens.visible.insert(node_id, publication_token);
                } else {
                    // Locally-synthesized reconnect bump (token variant); cannot
                    // readmit a DEAD/LEFT node — gossip_announced=false.
                    let incarnation = incarnation.saturating_add(1);
                    if let Some(transition) = Self::stage_member_transition_locked(
                        &mut members,
                        node_id,
                        MEMBER_ALIVE,
                        incarnation,
                        &[],
                        false,
                    ) {
                        if let Some(member) = members.iter_mut().find(|m| m.node_id == node_id) {
                            // SAFETY: hew_now_ms has no preconditions.
                            member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                        }
                        should_drain = self.queue_member_transition(transition.with_publication(
                            PublicationTransition::TokenEstablished(publication_token),
                        ));
                    }
                }
                true
            } else {
                false
            }
        };

        if !known_member {
            eprintln!("[cluster] unknown node {node_id} connected, waiting for join");
            return;
        }
        if should_drain {
            self.drain_member_transitions();
        }
    }

    /// Get the next ping target, delegating selection to the installed
    /// [`ClusterProtocol`] strategy.
    fn next_ping_target(&self) -> Option<u16> {
        let alive_members: Vec<u16> = {
            let members = self.members.lock_or_recover();
            members
                .iter()
                .filter(|m| m.state == MEMBER_ALIVE || m.state == MEMBER_SUSPECT)
                .map(|m| m.node_id)
                .collect()
        };
        self.protocol.next_ping_target(&alive_members)
    }

    // ── Registry gossip ────────────────────────────────────────────────

    /// Queue a registry add event for gossip dissemination.
    pub fn emit_registry_add(&self, name: &str, actor_id: u64) {
        let mut events = self.registry_events.lock_or_recover();
        // Deduplicate: remove prior event for the same name.
        events.retain(|e| e.name != name);
        if events.len() >= MAX_GOSSIP_EVENTS {
            events.pop_front();
        }
        events.push_back(RegistryEvent {
            name: name.to_owned(),
            actor_id,
            is_add: true,
            dissemination_count: 0,
        });
    }

    /// Queue a registry remove event for gossip dissemination.
    pub fn emit_registry_remove(&self, name: &str) {
        let mut events = self.registry_events.lock_or_recover();
        events.retain(|e| e.name != name);
        if events.len() >= MAX_GOSSIP_EVENTS {
            events.pop_front();
        }
        events.push_back(RegistryEvent {
            name: name.to_owned(),
            actor_id: 0,
            is_add: false,
            dissemination_count: 0,
        });
    }

    /// Get pending registry gossip events (up to `max_count`),
    /// incrementing dissemination counters and pruning expired events.
    pub fn take_registry_gossip(&self, max_count: usize) -> Vec<RegistryEvent> {
        let mut events = self.registry_events.lock_or_recover();
        let mut result = Vec::with_capacity(max_count);
        for event in events.iter_mut() {
            if result.len() >= max_count {
                break;
            }
            result.push(event.clone());
            event.dissemination_count += 1;
        }
        events.retain(|e| e.dissemination_count < 8);
        result
    }

    /// Get the number of pending registry gossip events.
    pub fn registry_gossip_count(&self) -> usize {
        let events = self.registry_events.lock_or_recover();
        events.len()
    }

    /// Process an inbound registry gossip event received from a peer.
    pub fn apply_registry_event(&self, name: &str, actor_id: u64, is_add: bool) {
        let Some(cb) = self.registry_callback else {
            return;
        };
        let Ok(c_name) = std::ffi::CString::new(name) else {
            return;
        };
        cb(
            c_name.as_ptr(),
            actor_id,
            is_add,
            self.registry_callback_user_data,
        );
    }

    // ── SWIM membership gossip (piggyback dissemination) ────────────────

    /// The current local incarnation number.
    ///
    /// Stamped on outbound PING/ACK frames so peers can resolve conflicting
    /// membership beliefs about this node (higher incarnation wins).
    pub fn local_incarnation(&self) -> u64 {
        self.local_incarnation.load(Ordering::SeqCst)
    }

    /// Configured maximum number of membership-gossip entries to piggyback
    /// per SWIM frame.
    #[must_use]
    pub fn max_gossip_per_msg(&self) -> usize {
        self.config.max_gossip_per_msg as usize
    }

    /// Configured SWIM protocol period in milliseconds (the driver's tick
    /// cadence).
    #[must_use]
    pub fn protocol_period_ms(&self) -> u64 {
        u64::from(self.config.protocol_period_ms.max(1))
    }

    /// Configured number of indirect-ping relays (K in SWIM).
    #[must_use]
    pub fn indirect_ping_count(&self) -> usize {
        self.config.indirect_ping_count as usize
    }

    /// Drain up to `max_count` pending membership-gossip entries for
    /// piggybacking on an outbound SWIM frame (C6 infection-style export).
    ///
    /// Returns `(node_id, state, incarnation)` triples. The caller maps these
    /// onto the wire `SwimGossipEntry` type; the cluster stays decoupled from
    /// the wire codec. Dissemination counters are advanced and exhausted
    /// events pruned, exactly as [`Self::take_registry_gossip`] does for
    /// registry events.
    pub fn take_swim_gossip(&self, max_count: usize) -> Vec<(u16, i32, u64)> {
        let mut events = self.events.lock_or_recover();
        let mut result = Vec::with_capacity(max_count.min(events.len()));
        for event in events.iter_mut() {
            if result.len() >= max_count {
                break;
            }
            result.push((event.node_id, event.new_state, event.incarnation));
            event.dissemination_count += 1;
        }
        events.retain(|e| e.dissemination_count < 8);
        result
    }

    /// Apply a batch of piggybacked membership-gossip entries received on a
    /// SWIM frame (C6 infection-style import).
    ///
    /// Each entry is folded into the membership list via the normal
    /// incarnation-LWW `upsert_member` path, so a node learns of a peer's
    /// DEAD/SUSPECT/ALIVE state even when it has no direct connection to the
    /// affected peer. Entries about the local node are ignored here — local
    /// state is authoritative and self-refutation is handled separately by
    /// [`Self::refute_if_suspected`].
    pub fn apply_swim_gossip(&self, entries: &[(u16, i32, u64)]) {
        let local = self.config.local_node_id;
        for &(node_id, state, incarnation) in entries {
            if node_id == local {
                continue;
            }
            if !matches!(
                state,
                MEMBER_ALIVE | MEMBER_SUSPECT | MEMBER_DEAD | MEMBER_LEFT
            ) {
                continue;
            }
            self.upsert_member(node_id, state, incarnation, &[]);
        }
    }

    /// Incarnation self-refutation (C5).
    ///
    /// If any entry in `entries` claims the local node is `MEMBER_SUSPECT`
    /// (or DEAD) at an incarnation greater-or-equal to ours, bump the local
    /// incarnation past it and enqueue a fresh `MEMBER_ALIVE` gossip event so
    /// the next outbound SWIM frame disseminates the refutation. Returns the
    /// new incarnation if a refutation was issued, else `None`.
    ///
    /// This is the standard SWIM defence against a false suspicion: a healthy
    /// node that sees itself suspected re-asserts liveness at a higher
    /// incarnation, which wins the LWW conflict resolution everywhere.
    pub fn refute_if_suspected(&self, entries: &[(u16, i32, u64)]) -> Option<u64> {
        let local = self.config.local_node_id;
        let mut max_suspect_incarnation: Option<u64> = None;
        for &(node_id, state, incarnation) in entries {
            if node_id == local && (state == MEMBER_SUSPECT || state == MEMBER_DEAD) {
                max_suspect_incarnation =
                    Some(max_suspect_incarnation.map_or(incarnation, |m| m.max(incarnation)));
            }
        }
        let suspect_incarnation = max_suspect_incarnation?;

        // Bump our incarnation strictly past the suspicion. Loop on CAS so a
        // concurrent refutation/leave on another thread cannot regress us.
        let mut current = self.local_incarnation.load(Ordering::SeqCst);
        loop {
            let next = current.max(suspect_incarnation).saturating_add(1);
            match self.local_incarnation.compare_exchange(
                current,
                next,
                Ordering::SeqCst,
                Ordering::SeqCst,
            ) {
                Ok(_) => {
                    // Enqueue a fresh ALIVE-about-self gossip event so the
                    // refutation is disseminated on the next outbound frame.
                    self.emit_event(local, MEMBER_ALIVE, next);
                    return Some(next);
                }
                Err(observed) => current = observed,
            }
        }
    }
}

// ── C ABI ──────────────────────────────────────────────────────────────

/// Create a new cluster membership manager.
///
/// Returns a pointer to the cluster instance (heap-allocated).
///
/// # Safety
///
/// `config` must be a valid pointer to a [`ClusterConfig`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_new(config: *const ClusterConfig) -> *mut HewCluster {
    if config.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `config` is valid.
    let cfg = unsafe { (*config).clone() };
    let cluster = Box::new(HewCluster::new(cfg));
    Box::into_raw(cluster)
}

/// Destroy a cluster instance.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_free(cluster: *mut HewCluster) {
    if !cluster.is_null() {
        // SAFETY: caller guarantees `cluster` is valid.
        let _ = unsafe { Box::from_raw(cluster) };
    }
}

/// Add a seed node to the cluster.
///
/// The node is added as `MEMBER_ALIVE` with incarnation 1.
/// `addr` must be a null-terminated C string (e.g., "192.168.1.10:9000").
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// - `cluster` must be a valid pointer returned by [`hew_cluster_new`].
/// - `addr` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_join(
    cluster: *mut HewCluster,
    node_id: u16,
    addr: *const c_char,
) -> c_int {
    if cluster.is_null() || addr.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees both pointers are valid.
    let cluster = unsafe { &*cluster };
    // SAFETY: caller guarantees `addr` is a valid null-terminated C string.
    let addr_str = unsafe { CStr::from_ptr(addr) };
    let addr_bytes = addr_str.to_bytes();

    cluster.upsert_member(node_id, MEMBER_ALIVE, 1, addr_bytes);
    0
}

/// Gracefully leave the cluster by marking self as LEFT.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_leave(cluster: *mut HewCluster) {
    if cluster.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    let local_id = cluster.config.local_node_id;
    let leave_incarnation = cluster.local_incarnation.fetch_add(1, Ordering::SeqCst) + 1;
    cluster.upsert_member(local_id, MEMBER_LEFT, leave_incarnation, &[]);
}

/// Return the number of known members (all states).
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_member_count(cluster: *mut HewCluster) -> c_int {
    if cluster.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    let members = cluster.members.lock_or_recover();
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "member count will not exceed c_int range in practice"
    )]
    {
        members.len() as c_int
    }
}

/// Process an incoming SWIM protocol message.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_process_message(
    cluster: *mut HewCluster,
    msg_type: i32,
    from_node: u16,
    incarnation: u64,
    source_conn_node_id: u16,
) -> c_int {
    if cluster.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `cluster` is valid. `process_message` takes
    // `&self`, so a shared reference is sound even with the SWIM driver thread
    // concurrently driving `tick` through another shared reference.
    let cluster = unsafe { &*cluster };
    cluster.process_message(msg_type, from_node, incarnation, source_conn_node_id);
    0
}

/// Advance the protocol: check timeouts, detect suspects/dead members.
///
/// Should be called periodically (e.g., every `protocol_period_ms`).
///
/// Returns the node ID of the next ping target (0 = none).
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_tick(cluster: *mut HewCluster) -> u16 {
    if cluster.is_null() {
        return 0;
    }
    // SAFETY: hew_now_ms has no preconditions.
    let now = unsafe { crate::io_time::hew_now_ms() };
    // SAFETY: caller guarantees `cluster` is valid. `tick` / `next_ping_target`
    // take `&self`, so a shared reference is sound even when the connection
    // reader thread concurrently drives `process_message` (also `&self`).
    let cluster = unsafe { &*cluster };
    cluster.tick(now);
    cluster.next_ping_target().unwrap_or(0)
}

/// Register a callback for membership change notifications.
///
/// The callback receives `(node_id, new_state, incarnation)`.
///
/// # Safety
///
/// - `cluster` must be a valid pointer returned by [`hew_cluster_new`].
/// - `callback` must be a valid function pointer that remains valid
///   for the cluster's lifetime (or null to clear).
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_set_callback(
    cluster: *mut HewCluster,
    callback: Option<MemberChangeCallback>,
) {
    if cluster.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &mut *cluster };
    cluster.callback = callback;
}

/// Register a callback for membership events with user data.
///
/// The callback receives `(node_id, event, user_data)` where `event` is one
/// of `HEW_MEMBERSHIP_EVENT_NODE_*`.
///
/// # Safety
///
/// - `cluster` must be a valid pointer returned by [`hew_cluster_new`].
/// - `callback` must remain valid for the cluster lifetime.
/// - `user_data` must remain valid while callback is registered.
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_set_membership_callback(
    cluster: *mut HewCluster,
    callback: HewMembershipCallback,
    user_data: *mut c_void,
) {
    if cluster.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    unsafe {
        hew_cluster_replace_membership_callback(
            cluster,
            MembershipCallbackBinding::new(Some(callback), user_data),
        );
    };
}

/// Read the current membership callback binding.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[cfg(test)]
#[allow(dead_code, reason = "sole consumer was remote_sup.rs (deleted)")]
pub(crate) unsafe fn hew_cluster_membership_callback_binding(
    cluster: *mut HewCluster,
) -> MembershipCallbackBinding {
    if cluster.is_null() {
        return MembershipCallbackBinding::default();
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    cluster.membership_callback_binding()
}

/// Snapshot the current membership callback generation for `cluster`.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[allow(dead_code, reason = "sole consumer was remote_sup.rs (deleted)")]
pub(crate) unsafe fn hew_cluster_membership_callback_generation(
    cluster: *mut HewCluster,
) -> MembershipCallbackGeneration {
    if cluster.is_null() {
        return MembershipCallbackGeneration::default();
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    cluster.membership_callback_generation()
}

/// Replace the current membership callback binding.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
pub(crate) unsafe fn hew_cluster_replace_membership_callback(
    cluster: *mut HewCluster,
    binding: MembershipCallbackBinding,
) {
    if cluster.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    *cluster.membership_callback_binding.lock_or_recover() = binding;
}

#[cfg(test)]
#[allow(dead_code, reason = "sole consumer was remote_sup.rs (deleted)")]
pub(crate) unsafe fn hew_cluster_test_fire_membership_callback(
    cluster: *mut HewCluster,
    node_id: u16,
    event: u8,
) {
    if cluster.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    let _ = cluster.with_membership_callback_dispatch(|callback, user_data| {
        callback(node_id, event, user_data);
    });
}

/// Install a [`PartitionRegistry`] on the cluster.
///
/// After installation, every `MEMBER_DEAD` transition fans out
/// `RecvError::PartitionDetected` to all queues registered in `registry`
/// for the dead node. Passing the same registry to multiple clusters is
/// allowed; each cluster holds an `Arc` clone.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
pub unsafe fn hew_cluster_set_partition_registry(
    cluster: *mut HewCluster,
    registry: Arc<PartitionRegistry>,
) {
    if cluster.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &mut *cluster };
    cluster.set_partition_registry(registry);
}

/// Register a callback for registry gossip events.
///
/// The callback receives `(name, actor_id, is_add, user_data)`.
///
/// # Safety
///
/// - `cluster` must be a valid pointer returned by [`hew_cluster_new`].
/// - `callback` must remain valid for the cluster lifetime.
/// - `user_data` must remain valid while callback is registered.
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_set_registry_callback(
    cluster: *mut HewCluster,
    callback: HewRegistryGossipCallback,
    user_data: *mut c_void,
) {
    if cluster.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &mut *cluster };
    cluster.registry_callback = Some(callback);
    cluster.registry_callback_user_data = user_data;
}

/// Queue a registry-add gossip event for dissemination.
///
/// # Safety
///
/// - `cluster` must be a valid pointer returned by [`hew_cluster_new`].
/// - `name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_registry_add(
    cluster: *mut HewCluster,
    name: *const c_char,
    actor_id: u64,
) {
    if cluster.is_null() || name.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    // SAFETY: caller guarantees `name` is a valid null-terminated C string.
    let name_str = unsafe { CStr::from_ptr(name) }.to_string_lossy();
    cluster.emit_registry_add(&name_str, actor_id);
}

/// Queue a registry-remove gossip event for dissemination.
///
/// # Safety
///
/// - `cluster` must be a valid pointer returned by [`hew_cluster_new`].
/// - `name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_registry_remove(
    cluster: *mut HewCluster,
    name: *const c_char,
) {
    if cluster.is_null() || name.is_null() {
        return;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    // SAFETY: caller guarantees `name` is a valid null-terminated C string.
    let name_str = unsafe { CStr::from_ptr(name) }.to_string_lossy();
    cluster.emit_registry_remove(&name_str);
}

/// Get the number of pending registry gossip events.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_registry_gossip_count(cluster: *mut HewCluster) -> c_int {
    if cluster.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "registry gossip count will not exceed c_int range in practice"
    )]
    {
        cluster.registry_gossip_count() as c_int
    }
}

/// Notify SWIM membership that a connection to `node_id` has been lost.
///
/// Returns 0 on success, -1 on invalid cluster pointer.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_notify_connection_lost(
    cluster: *mut HewCluster,
    node_id: u16,
) -> c_int {
    if cluster.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    cluster.notify_connection_lost(node_id);
    0
}

pub(crate) unsafe fn hew_cluster_notify_connection_lost_if_current(
    cluster: *mut HewCluster,
    node_id: u16,
    publication_token: u64,
) -> c_int {
    if cluster.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    cluster.notify_connection_lost_if_current(node_id, publication_token);
    0
}

/// Notify SWIM membership that a connection to `node_id` has been established.
///
/// Returns 0 on success, -1 on invalid cluster pointer.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_notify_connection_established(
    cluster: *mut HewCluster,
    node_id: u16,
) -> c_int {
    if cluster.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    cluster.notify_connection_established(node_id);
    0
}

#[cfg_attr(
    not(test),
    expect(dead_code, reason = "token-only helper is exercised by focused tests")
)]
pub(crate) unsafe fn hew_cluster_notify_connection_established_for_token(
    cluster: *mut HewCluster,
    node_id: u16,
    publication_token: u64,
) -> c_int {
    if cluster.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    cluster.notify_connection_established_for_token(node_id, publication_token);
    0
}

#[allow(
    clippy::too_many_lines,
    reason = "function coordinates admission, session verification, and durable publication in sequence"
)]
pub(crate) unsafe fn hew_cluster_notify_connection_established_for_token_if_not_removed(
    cluster: *mut HewCluster,
    node_id: u16,
    session: u32,
    publication_token: u64,
    publication_sync: &Arc<Mutex<()>>,
    publication_removed: &Arc<AtomicBool>,
) -> c_int {
    if cluster.is_null() || node_id == 0 || session == 0 {
        return -1;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    let mut should_drain = false;
    let known_member = {
        let _publication = publication_sync.lock_or_recover();
        if publication_removed.load(Ordering::Acquire) {
            return 0;
        }
        let mut sessions = cluster.sessions.lock_or_recover();
        let previous_session = sessions.get(&node_id).copied();
        match previous_session {
            Some(current) if session < current => return -1,
            Some(current) if session == current => {}
            _ => {
                sessions.insert(node_id, session);
            }
        }
        let session_is_new = previous_session != Some(session);
        let mut tokens = cluster.connection_tokens.lock_or_recover();
        tokens.current.insert(node_id, publication_token);

        let mut members = cluster.members.lock_or_recover();
        let member_index = members.iter().position(|member| member.node_id == node_id);
        if let Some(index) = member_index {
            let state = members[index].state;
            let old_incarnation = members[index].incarnation;
            if session_is_new {
                members[index].state = MEMBER_ALIVE;
                members[index].incarnation = 1;
                // SAFETY: hew_now_ms has no preconditions.
                members[index].last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                should_drain = cluster.queue_member_transition(
                    MemberTransition {
                        node_id,
                        state: MEMBER_ALIVE,
                        incarnation: 1,
                        is_new_member: false,
                        old_state: Some(state),
                        publication: PublicationTransition::Plain,
                    }
                    .with_publication(
                        PublicationTransition::GuardedTokenEstablished {
                            publication_token,
                            publication_sync: Arc::clone(publication_sync),
                            publication_removed: Arc::clone(publication_removed),
                        },
                    ),
                );
            } else if state == MEMBER_ALIVE {
                if let Some(member) = members.iter_mut().find(|m| m.node_id == node_id) {
                    // SAFETY: hew_now_ms has no preconditions.
                    member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                }
                tokens.visible.insert(node_id, publication_token);
            } else {
                if matches!(state, MEMBER_DEAD | MEMBER_LEFT) {
                    tokens.current.remove(&node_id);
                    return -1;
                }
                let incarnation = old_incarnation.saturating_add(1);
                if let Some(transition) = HewCluster::stage_member_transition_locked(
                    &mut members,
                    node_id,
                    MEMBER_ALIVE,
                    incarnation,
                    &[],
                    false,
                ) {
                    if let Some(member) = members.iter_mut().find(|m| m.node_id == node_id) {
                        // SAFETY: hew_now_ms has no preconditions.
                        member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
                    }
                    should_drain = cluster.queue_member_transition(transition.with_publication(
                        PublicationTransition::GuardedTokenEstablished {
                            publication_token,
                            publication_sync: Arc::clone(publication_sync),
                            publication_removed: Arc::clone(publication_removed),
                        },
                    ));
                } else if matches!(
                    tokens.current.get(&node_id),
                    Some(current) if *current == publication_token
                ) {
                    tokens.current.remove(&node_id);
                }
            }
            true
        } else {
            members.push(ClusterMember {
                node_id,
                state: MEMBER_ALIVE,
                incarnation: 1,
                addr: [0; 128],
                // SAFETY: hew_now_ms has no preconditions.
                last_seen_ms: unsafe { crate::io_time::hew_now_ms() },
            });
            should_drain = cluster.queue_member_transition(
                MemberTransition {
                    node_id,
                    state: MEMBER_ALIVE,
                    incarnation: 1,
                    is_new_member: true,
                    old_state: None,
                    publication: PublicationTransition::Plain,
                }
                .with_publication(PublicationTransition::GuardedTokenEstablished {
                    publication_token,
                    publication_sync: Arc::clone(publication_sync),
                    publication_removed: Arc::clone(publication_removed),
                }),
            );
            true
        }
    };
    if !known_member {
        eprintln!("[cluster] unknown node {node_id} connected, waiting for join");
    } else if should_drain {
        cluster.drain_member_transitions();
    }
    1
}

/// Get the number of alive members.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_alive_count(cluster: *mut HewCluster) -> c_int {
    if cluster.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    let members = cluster.members.lock_or_recover();
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "member count will not exceed c_int range in practice"
    )]
    {
        members.iter().filter(|m| m.state == MEMBER_ALIVE).count() as c_int
    }
}

/// Return the membership state of `node_id`, or `-1` if the node is unknown.
///
/// States are the `MEMBER_*` constants (`MEMBER_ALIVE` / `_SUSPECT` / `_DEAD`
/// / `_LEFT`). Used to observe SWIM transitions (e.g. confirming a peer was
/// declared DEAD by the driven failure detector).
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_member_state(cluster: *mut HewCluster, node_id: u16) -> i32 {
    if cluster.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    let members = cluster.members.lock_or_recover();
    members
        .iter()
        .find(|m| m.node_id == node_id)
        .map_or(-1, |m| m.state)
}

impl HewCluster {
    /// The live incarnation the membership table currently records for `node_id`,
    /// or `None` if the node is unknown. Used by the `Quarantine` send/ask consult
    /// to compare a peer's live incarnation against the quarantined one.
    pub(crate) fn member_incarnation(&self, node_id: u16) -> Option<u64> {
        let members = self.members.lock_or_recover();
        members
            .iter()
            .find(|m| m.node_id == node_id)
            .map(|m| m.incarnation)
    }

    /// The raw `last_seen_ms` timestamp the membership table currently records
    /// for `node_id`, or `None` if the node is unknown. Unlike the coarse
    /// `MEMBER_*` state (which only flips SUSPECT->ALIVE and is otherwise
    /// unchanged while already ALIVE), this value is refreshed on every
    /// PING-ACK round-trip, so polling for a *new* value distinct from a
    /// previously-observed one detects a fresh round-trip even when state
    /// stays steady-state ALIVE across multiple observation periods. Used by
    /// the driven-SWIM false-DEAD regression test to confirm each simulated
    /// period actually waits for its own round-trip rather than re-observing
    /// stale ALIVE state left over from an earlier period.
    #[cfg(test)]
    pub(crate) fn member_last_seen_ms(&self, node_id: u16) -> Option<u64> {
        let members = self.members.lock_or_recover();
        members
            .iter()
            .find(|m| m.node_id == node_id)
            .map(|m| m.last_seen_ms)
    }

    /// The membership state recorded for `node_id` (a `MEMBER_*` constant), or
    /// `-1` if the node is unknown. Used by the rejoin test-introspection probe.
    pub(crate) fn member_state(&self, node_id: u16) -> i32 {
        let members = self.members.lock_or_recover();
        members
            .iter()
            .find(|m| m.node_id == node_id)
            .map_or(-1, |m| m.state)
    }
}

/// Get the number of pending gossip events.
///
/// # Safety
///
/// `cluster` must be a valid pointer returned by [`hew_cluster_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_cluster_gossip_count(cluster: *mut HewCluster) -> c_int {
    if cluster.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `cluster` is valid.
    let cluster = unsafe { &*cluster };
    let events = cluster.events.lock_or_recover();
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "gossip count will not exceed c_int range in practice"
    )]
    {
        events.len() as c_int
    }
}

// ── Profiler snapshot ───────────────────────────────────────────────────

/// Build a JSON array of cluster members for the profiler HTTP API.
///
/// Each element: `{"node_id":N,"state":"S","incarnation":N,"addr":"S","last_seen_ms":N}`
#[cfg(feature = "profiler")]
pub fn snapshot_members_json(cluster: &HewCluster) -> String {
    use std::fmt::Write as _;

    // SAFETY: hew_now_ms has no preconditions.
    let now_ms = unsafe { crate::io_time::hew_now_ms() };

    let members = cluster.members.lock_or_recover();

    crate::util::json_array(members.iter(), |json, m| {
        let state_str = match m.state {
            MEMBER_ALIVE => "alive",
            MEMBER_SUSPECT => "suspect",
            MEMBER_DEAD => "dead",
            MEMBER_LEFT => "left",
            _ => "unknown",
        };
        // Extract address as UTF-8 trimmed of null bytes.
        let addr_end = m.addr.iter().position(|&b| b == 0).unwrap_or(m.addr.len());
        let addr = std::str::from_utf8(&m.addr[..addr_end]).unwrap_or("");
        // Emit last_seen_ms as a relative "ms ago" value for the observer client.
        let last_seen_ago_ms = now_ms.saturating_sub(m.last_seen_ms);
        let _ = write!(
            json,
            r#"{{"node_id":{},"state":"{}","incarnation":{},"addr":"#,
            m.node_id, state_str, m.incarnation,
        );
        crate::util::push_json_string(json, addr);
        let _ = write!(json, r#","last_seen_ms":{last_seen_ago_ms}}}"#);
    })
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn make_config(node_id: u16) -> ClusterConfig {
        ClusterConfig {
            local_node_id: node_id,
            ..ClusterConfig::default()
        }
    }

    #[test]
    fn handshake_admission_adds_unknown_peer_without_resurrecting_known_dead_peer() {
        let cluster = HewCluster::new(make_config(1));
        cluster.admit_handshake_peer(2);
        assert_eq!(cluster.member_state(2), MEMBER_ALIVE);
        assert_eq!(cluster.member_incarnation(2), Some(1));

        cluster.upsert_member(3, MEMBER_DEAD, 7, &[]);
        cluster.admit_handshake_peer(3);
        assert_eq!(cluster.member_state(3), MEMBER_DEAD);
        assert_eq!(cluster.member_incarnation(3), Some(7));
    }

    #[cfg(feature = "profiler")]
    #[test]
    fn snapshot_members_json_escapes_addr_field() {
        let cluster = HewCluster::new(make_config(1));
        // SAFETY: hew_now_ms has no preconditions.
        let before_snapshot_ms = unsafe { crate::io_time::hew_now_ms() };
        let fixture_last_seen_ms = before_snapshot_ms.saturating_sub(25);
        let mut member = ClusterMember {
            node_id: 7,
            state: MEMBER_ALIVE,
            incarnation: 42,
            addr: [0; 128],
            last_seen_ms: fixture_last_seen_ms,
        };
        let addr = b"node\"\\\\name\n:9000";
        member.addr[..addr.len()].copy_from_slice(addr);
        cluster.members.lock_or_recover().push(member);

        let json = snapshot_members_json(&cluster);
        // SAFETY: hew_now_ms has no preconditions.
        let after_snapshot_ms = unsafe { crate::io_time::hew_now_ms() };
        let prefix = r#"[{"node_id":7,"state":"alive","incarnation":42,"addr":"node\"\\\\name\n:9000","last_seen_ms":"#;
        assert!(
            json.starts_with(prefix),
            "snapshot should preserve field order and escape the address: {json}"
        );
        assert!(
            json.ends_with("}]"),
            "snapshot should end with a single object: {json}"
        );

        let last_seen_ms = json
            .trim_start_matches(prefix)
            .trim_end_matches("}]")
            .parse::<u64>()
            .expect("last_seen_ms should be numeric");
        let min_elapsed_ms = before_snapshot_ms.saturating_sub(fixture_last_seen_ms);
        let max_elapsed_ms = after_snapshot_ms.saturating_sub(fixture_last_seen_ms);
        assert!(
            (min_elapsed_ms..=max_elapsed_ms).contains(&last_seen_ms),
            "snapshot should emit relative ms-ago output in [{min_elapsed_ms}, {max_elapsed_ms}], got {last_seen_ms}"
        );
    }

    #[test]
    fn create_and_destroy() {
        let config = make_config(1);
        // SAFETY: config is valid.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert!(!cluster.is_null());
            assert_eq!(hew_cluster_member_count(cluster), 0);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn join_adds_member() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            let addr = c"192.168.1.10:9000";
            assert_eq!(hew_cluster_join(cluster, 2, addr.as_ptr()), 0);
            assert_eq!(hew_cluster_member_count(cluster), 1);
            assert_eq!(hew_cluster_alive_count(cluster), 1);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn ack_updates_member() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            let addr = c"10.0.0.1:9000";
            hew_cluster_join(cluster, 2, addr.as_ptr());

            // ACK from node 2 should keep it alive.
            hew_cluster_process_message(cluster, SWIM_MSG_ACK, 2, 1, 2);
            assert_eq!(hew_cluster_alive_count(cluster), 1);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn process_message_rejects_source_mismatch() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            let addr = c"10.0.0.1:9000";
            assert_eq!(hew_cluster_join(cluster, 2, addr.as_ptr()), 0);
            assert_eq!(hew_cluster_notify_connection_lost(cluster, 2), 0);

            // ACK claims to be from node 2, but arrived on node 3 connection.
            hew_cluster_process_message(cluster, SWIM_MSG_ACK, 2, 2, 3);
            {
                let cluster_ref = &*cluster;
                let members = cluster_ref.members.lock().unwrap();
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(member.state, MEMBER_SUSPECT);
            }
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn gossip_events_generated() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            let addr = c"10.0.0.1:9000";
            hew_cluster_join(cluster, 2, addr.as_ptr());
            // Joining should generate a gossip event.
            assert_eq!(hew_cluster_gossip_count(cluster), 1);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn null_safety() {
        // All operations on null should return gracefully.
        // SAFETY: testing null safety.
        unsafe {
            let null: *mut HewCluster = std::ptr::null_mut();
            assert_eq!(hew_cluster_member_count(null), 0);
            assert_eq!(hew_cluster_alive_count(null), 0);
            assert_eq!(hew_cluster_tick(null), 0);
            hew_cluster_free(null);
        }
    }

    #[test]
    fn incarnation_supersedes() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"10.0.0.1:9000");

        // Same incarnation, higher state (suspect) should update.
        cluster.upsert_member(2, MEMBER_SUSPECT, 1, &[]);
        {
            let members = cluster.members.lock().unwrap();
            assert_eq!(members[0].state, MEMBER_SUSPECT);
        }

        // Higher incarnation, alive again should update.
        cluster.upsert_member(2, MEMBER_ALIVE, 2, &[]);
        {
            let members = cluster.members.lock().unwrap();
            assert_eq!(members[0].state, MEMBER_ALIVE);
            assert_eq!(members[0].incarnation, 2);
        }

        // Lower incarnation should NOT update.
        cluster.upsert_member(2, MEMBER_DEAD, 1, &[]);
        {
            let members = cluster.members.lock().unwrap();
            assert_eq!(members[0].state, MEMBER_ALIVE); // unchanged
        }
    }

    #[test]
    fn tick_suspects_and_kills() {
        let cluster = HewCluster::new(ClusterConfig {
            local_node_id: 1,
            ping_timeout_ms: 100,
            suspect_timeout_ms: 300,
            ..ClusterConfig::default()
        });
        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"10.0.0.1:9000");
        {
            let mut members = cluster.members.lock().unwrap();
            members[0].last_seen_ms = 0;
        }

        // Advance time past ping timeout → should become suspect.
        cluster.tick(150);
        {
            let members = cluster.members.lock().unwrap();
            assert_eq!(members[0].state, MEMBER_SUSPECT);
        }

        // Advance time past suspect timeout → should become dead.
        cluster.tick(500);
        {
            let members = cluster.members.lock().unwrap();
            assert_eq!(members[0].state, MEMBER_DEAD);
        }
    }

    /// The rejoin-admission truth table: a buried node (DEAD or LEFT) is
    /// readmitted ONLY by a peer-announced strictly-higher incarnation. Equal and
    /// lower incarnations are stale gossip and must never resurrect it.
    #[test]
    fn dead_member_revived_only_by_strictly_higher_incarnation() {
        // Equal-incarnation ALIVE does NOT resurrect (fail-closed): a dead node at
        // N stays dead for any ALIVE-at-N gossip.
        {
            let cluster = HewCluster::new(make_config(1));
            cluster.upsert_member(2, MEMBER_DEAD, 5, b"10.0.0.1:9000");
            cluster.upsert_member(2, MEMBER_ALIVE, 5, &[]);
            let members = cluster.members.lock().unwrap();
            assert_eq!(
                members[0].state, MEMBER_DEAD,
                "equal-incarnation must not revive"
            );
            assert_eq!(members[0].incarnation, 5);
        }
        // Lower-incarnation ALIVE does NOT resurrect (fail-closed): replayed stale
        // gossip about the old identity is inert.
        {
            let cluster = HewCluster::new(make_config(1));
            cluster.upsert_member(2, MEMBER_DEAD, 5, b"10.0.0.1:9000");
            cluster.upsert_member(2, MEMBER_ALIVE, 4, &[]);
            let members = cluster.members.lock().unwrap();
            assert_eq!(
                members[0].state, MEMBER_DEAD,
                "lower-incarnation must not revive"
            );
            assert_eq!(members[0].incarnation, 5);
        }
        // Strictly-higher-incarnation ALIVE IS admitted (the positive readmission).
        {
            let cluster = HewCluster::new(make_config(1));
            cluster.upsert_member(2, MEMBER_DEAD, 5, b"10.0.0.1:9000");
            cluster.upsert_member(2, MEMBER_ALIVE, 6, &[]);
            let members = cluster.members.lock().unwrap();
            assert_eq!(
                members[0].state, MEMBER_ALIVE,
                "strictly-higher must readmit"
            );
            assert_eq!(members[0].incarnation, 6);
        }
    }

    /// The same truth table for a gracefully-LEFT node: the readmission rule is
    /// uniform across both terminal states (DEAD and LEFT).
    #[test]
    fn left_member_revived_only_by_strictly_higher_incarnation() {
        // Equal-incarnation ALIVE does NOT readmit a LEFT node.
        {
            let cluster = HewCluster::new(make_config(1));
            cluster.upsert_member(2, MEMBER_LEFT, 5, b"10.0.0.1:9000");
            cluster.upsert_member(2, MEMBER_ALIVE, 5, &[]);
            let members = cluster.members.lock().unwrap();
            assert_eq!(
                members[0].state, MEMBER_LEFT,
                "equal-incarnation must not readmit"
            );
            assert_eq!(members[0].incarnation, 5);
        }
        // Lower-incarnation ALIVE does NOT readmit a LEFT node.
        {
            let cluster = HewCluster::new(make_config(1));
            cluster.upsert_member(2, MEMBER_LEFT, 5, b"10.0.0.1:9000");
            cluster.upsert_member(2, MEMBER_ALIVE, 4, &[]);
            let members = cluster.members.lock().unwrap();
            assert_eq!(
                members[0].state, MEMBER_LEFT,
                "lower-incarnation must not readmit"
            );
            assert_eq!(members[0].incarnation, 5);
        }
        // Strictly-higher-incarnation ALIVE readmits a LEFT node.
        {
            let cluster = HewCluster::new(make_config(1));
            cluster.upsert_member(2, MEMBER_LEFT, 5, b"10.0.0.1:9000");
            cluster.upsert_member(2, MEMBER_ALIVE, 6, &[]);
            let members = cluster.members.lock().unwrap();
            assert_eq!(
                members[0].state, MEMBER_ALIVE,
                "strictly-higher must readmit"
            );
            assert_eq!(members[0].incarnation, 6);
        }
    }

    /// A locally-synthesized reconnect bump (`gossip_announced=false`) must never
    /// readmit a buried node, even though it raises the incarnation — a `TCP`
    /// reconnect is not proof the peer restarted with a fresh identity. This is the
    /// distinction `connection_established_dead_member_stays_dead` depends on.
    #[test]
    fn reconnect_bump_does_not_readmit_dead_node() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_DEAD, 5, b"10.0.0.1:9000");
        // notify_connection_established synthesizes incarnation 6 (5 + 1) locally.
        cluster.notify_connection_established(2);
        let members = cluster.members.lock().unwrap();
        assert_eq!(
            members[0].state, MEMBER_DEAD,
            "a reconnect must not resurrect a dead node"
        );
        assert_eq!(members[0].incarnation, 5);
    }

    #[test]
    fn leave_marks_self_left() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            // First add ourselves.
            let addr = c"127.0.0.1:9000";
            hew_cluster_join(cluster, 1, addr.as_ptr());
            assert_eq!(hew_cluster_alive_count(cluster), 1);

            hew_cluster_leave(cluster);
            assert_eq!(hew_cluster_alive_count(cluster), 0);
            hew_cluster_free(cluster);
        }
    }

    extern "C" fn collect_membership_events(node_id: u16, event: u8, user_data: *mut c_void) {
        // SAFETY: test passes a valid pointer to `Vec<(u16, u8)>`.
        let events = unsafe { &mut *user_data.cast::<Vec<(u16, u8)>>() };
        events.push((node_id, event));
    }

    #[test]
    fn connection_notifications_update_membership() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            let addr = c"10.0.0.1:9000";
            assert_eq!(hew_cluster_join(cluster, 2, addr.as_ptr()), 0);

            assert_eq!(hew_cluster_notify_connection_lost(cluster, 2), 0);
            let cluster_ref = &*cluster;
            {
                let members = cluster_ref.members.lock().unwrap();
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(member.state, MEMBER_SUSPECT);
            }

            assert_eq!(hew_cluster_notify_connection_established(cluster, 2), 0);
            {
                let members = cluster_ref.members.lock().unwrap();
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(member.state, MEMBER_ALIVE);
            }
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn membership_callback_receives_connection_events() {
        let config = make_config(1);
        let mut events: Vec<(u16, u8)> = Vec::new();
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            hew_cluster_set_membership_callback(
                cluster,
                collect_membership_events,
                (&raw mut events).cast(),
            );
            let addr = c"10.0.0.1:9000";
            assert_eq!(hew_cluster_join(cluster, 2, addr.as_ptr()), 0);
            assert_eq!(hew_cluster_notify_connection_lost(cluster, 2), 0);
            assert_eq!(hew_cluster_notify_connection_established(cluster, 2), 0);
            hew_cluster_free(cluster);
        }

        assert_eq!(
            events,
            vec![
                (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
            ]
        );
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "test coordinates the stale lost-vs-established race in one place"
    )]
    fn tokenized_connection_lost_serializes_replacement_publish() {
        struct BlockingMembershipState {
            events: std::sync::Mutex<Vec<(u16, u8)>>,
            suspect_seen: std::sync::mpsc::Sender<()>,
            release: std::sync::Arc<std::sync::Barrier>,
        }

        extern "C" fn block_on_suspect(node_id: u16, event: u8, user_data: *mut c_void) {
            // SAFETY: user_data points at the BlockingMembershipState allocated in this test.
            let state = unsafe { &*user_data.cast::<BlockingMembershipState>() };
            state
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push((node_id, event));
            if event == HEW_MEMBERSHIP_EVENT_NODE_SUSPECT {
                state
                    .suspect_seen
                    .send(())
                    .expect("suspect callback should notify the test");
                state.release.wait();
            }
        }

        struct SendCluster(*mut HewCluster);
        // SAFETY: the test keeps the cluster alive until both worker threads join.
        unsafe impl Send for SendCluster {}

        let config = make_config(1);
        let (suspect_tx, suspect_rx) = std::sync::mpsc::channel::<()>();
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        let callback_state = Box::into_raw(Box::new(BlockingMembershipState {
            events: std::sync::Mutex::new(Vec::new()),
            suspect_seen: suspect_tx,
            release: std::sync::Arc::clone(&release),
        }));

        // SAFETY: test-owned cluster and callback state remain valid until the
        // explicit free/drop calls after both worker threads complete.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert!(!cluster.is_null());
            hew_cluster_set_membership_callback(cluster, block_on_suspect, callback_state.cast());
            assert_eq!(hew_cluster_join(cluster, 2, c"10.0.0.1:9000".as_ptr()), 0);
            assert_eq!(
                hew_cluster_notify_connection_established_for_token(cluster, 2, 1),
                0
            );
            (&*cluster).sessions.lock_or_recover().insert(2, 1);

            let (lost_done_tx, lost_done_rx) = std::sync::mpsc::channel::<()>();
            let lost_cluster = SendCluster(cluster);
            let lost_handle = std::thread::spawn(move || {
                let cluster = lost_cluster;
                // SAFETY: cluster stays alive until this thread joins.
                let rc = hew_cluster_notify_connection_lost_if_current(cluster.0, 2, 1);
                assert_eq!(rc, 0);
                lost_done_tx
                    .send(())
                    .expect("lost thread should report completion");
            });

            suspect_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost path should reach the membership callback");

            let (established_done_tx, established_done_rx) = std::sync::mpsc::channel::<()>();
            let established_cluster = SendCluster(cluster);
            let established_handle = std::thread::spawn(move || {
                let cluster = established_cluster;
                // SAFETY: cluster stays alive until this thread joins.
                let rc = hew_cluster_notify_connection_established_for_token(cluster.0, 2, 2);
                assert_eq!(rc, 0);
                established_done_tx
                    .send(())
                    .expect("established thread should report completion");
            });

            assert_eq!(
                (&*callback_state)
                    .events
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .clone(),
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                ],
                "replacement publish must not emit JOINED before the lost callback is released"
            );

            release.wait();

            lost_done_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost thread should finish after the callback is released");
            established_done_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("replacement publish should complete after lost finishes");

            lost_handle.join().expect("lost thread should not panic");
            established_handle
                .join()
                .expect("established thread should not panic");

            let cluster_ref = &*cluster;
            {
                let members = cluster_ref
                    .members
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(member.state, MEMBER_ALIVE);
            }
            let events = (&*callback_state)
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone();
            assert_eq!(
                events,
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                ],
                "replacement publish must not slip in between token retirement and the lost update"
            );

            hew_cluster_free(cluster);
            drop(Box::from_raw(callback_state));
        }
    }

    #[test]
    fn tokenized_connection_established_observes_sync_cancellation() {
        struct SendCluster(*mut HewCluster);
        // SAFETY: the test keeps the cluster alive until the worker joins.
        unsafe impl Send for SendCluster {}

        let config = make_config(1);
        let mut events: Vec<(u16, u8)> = Vec::new();
        let publication_sync = std::sync::Arc::new(std::sync::Mutex::new(()));
        let publication_removed = std::sync::Arc::new(AtomicBool::new(false));

        // SAFETY: test context with explicit cleanup before return.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert!(!cluster.is_null());
            hew_cluster_set_membership_callback(
                cluster,
                collect_membership_events,
                (&raw mut events).cast(),
            );
            assert_eq!(hew_cluster_join(cluster, 2, c"10.0.0.1:9000".as_ptr()), 0);
            assert_eq!(
                hew_cluster_notify_connection_established_for_token(cluster, 2, 1),
                0
            );
            assert_eq!(
                hew_cluster_notify_connection_lost_if_current(cluster, 2, 1),
                0
            );

            let publication_guard = publication_sync
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let members_guard = (&*cluster)
                .members
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let (publish_done_tx, publish_done_rx) = std::sync::mpsc::channel::<c_int>();
            let publish_cluster = SendCluster(cluster);
            let publication_sync_for_thread = std::sync::Arc::clone(&publication_sync);
            let publication_removed_for_thread = std::sync::Arc::clone(&publication_removed);
            let publish_handle = std::thread::spawn(move || {
                let cluster = publish_cluster;
                // SAFETY: test keeps the cluster alive until the worker joins.
                let rc = hew_cluster_notify_connection_established_for_token_if_not_removed(
                    cluster.0,
                    2,
                    1,
                    2,
                    &publication_sync_for_thread,
                    &publication_removed_for_thread,
                );
                publish_done_tx
                    .send(rc)
                    .expect("publish thread should report completion");
            });

            assert!(
                matches!(
                    publish_done_rx.recv_timeout(std::time::Duration::from_millis(100)),
                    Err(std::sync::mpsc::RecvTimeoutError::Timeout)
                ),
                "publish should remain blocked while the publication lock is held"
            );

            publication_removed.store(true, Ordering::Release);
            drop(publication_guard);
            drop(members_guard);

            assert_eq!(
                publish_done_rx
                    .recv_timeout(std::time::Duration::from_secs(1))
                    .expect("publish thread should complete once cancellation is visible"),
                0
            );
            publish_handle
                .join()
                .expect("publish thread should not panic");

            assert_eq!(
                events,
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                ],
                "a cancelled delayed publish must not emit a stale JOINED event"
            );
            assert!(
                !(&*cluster)
                    .connection_tokens
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .current
                    .contains_key(&2),
                "a cancelled publish must not install a live token"
            );

            hew_cluster_free(cluster);
        }
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "test coordinates delayed guarded publish, tick, and callback release end-to-end"
    )]
    fn tokenized_connection_established_stays_alive_while_guarded_delivery_waits() {
        struct BlockingMembershipState {
            events: std::sync::Mutex<Vec<(u16, u8)>>,
            suspect_seen: std::sync::mpsc::Sender<()>,
            release: std::sync::Arc<std::sync::Barrier>,
            blocked_first_suspect: AtomicBool,
        }

        extern "C" fn block_on_first_suspect(node_id: u16, event: u8, user_data: *mut c_void) {
            // SAFETY: user_data points at the BlockingMembershipState allocated in this test.
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
                state.release.wait();
            }
        }

        struct SendCluster(*mut HewCluster);
        // SAFETY: the test keeps the cluster alive until all worker threads join.
        unsafe impl Send for SendCluster {}

        let config = ClusterConfig {
            local_node_id: 1,
            ping_timeout_ms: 100,
            suspect_timeout_ms: 300,
            ..ClusterConfig::default()
        };
        let publication_sync = std::sync::Arc::new(std::sync::Mutex::new(()));
        let publication_removed = std::sync::Arc::new(AtomicBool::new(false));
        let (suspect_tx, suspect_rx) = std::sync::mpsc::channel::<()>();
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        let callback_state = Box::into_raw(Box::new(BlockingMembershipState {
            events: std::sync::Mutex::new(Vec::new()),
            suspect_seen: suspect_tx,
            release: std::sync::Arc::clone(&release),
            blocked_first_suspect: AtomicBool::new(false),
        }));

        // SAFETY: test-owned cluster and callback state remain valid until the
        // explicit free/drop calls after all worker threads complete.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert!(!cluster.is_null());
            hew_cluster_set_membership_callback(
                cluster,
                block_on_first_suspect,
                callback_state.cast(),
            );
            assert_eq!(hew_cluster_join(cluster, 2, c"10.0.0.1:9000".as_ptr()), 0);
            assert_eq!(
                hew_cluster_notify_connection_established_for_token(cluster, 2, 1),
                0
            );
            (&*cluster).sessions.lock_or_recover().insert(2, 1);

            let (lost_done_tx, lost_done_rx) = std::sync::mpsc::channel::<()>();
            let lost_cluster = SendCluster(cluster);
            let lost_handle = std::thread::spawn(move || {
                let cluster = lost_cluster;
                // SAFETY: cluster stays alive until this thread joins.
                let rc = hew_cluster_notify_connection_lost_if_current(cluster.0, 2, 1);
                assert_eq!(rc, 0);
                lost_done_tx
                    .send(())
                    .expect("lost thread should report completion");
            });

            suspect_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost path should reach the membership callback");

            while crate::io_time::hew_now_ms() < 2 {
                std::thread::sleep(std::time::Duration::from_millis(1));
            }

            {
                let cluster_ref = &*cluster;
                let mut members = cluster_ref
                    .members
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                let member = members.iter_mut().find(|m| m.node_id == 2).unwrap();
                assert_eq!(member.state, MEMBER_SUSPECT);
                member.last_seen_ms = 1;
            }

            assert_eq!(
                hew_cluster_notify_connection_established_for_token_if_not_removed(
                    cluster,
                    2,
                    1,
                    2,
                    &publication_sync,
                    &publication_removed,
                ),
                1
            );

            {
                let cluster_ref = &*cluster;
                let members = cluster_ref
                    .members
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(
                    member.state, MEMBER_ALIVE,
                    "queued guarded publish must eagerly restore ALIVE state so tick cannot mark DEAD"
                );
                assert_eq!(member.incarnation, 2);
                assert!(
                    member.last_seen_ms > 1,
                    "queued guarded publish must refresh last_seen before callback delivery"
                );
                let guarded_tick = member
                    .last_seen_ms
                    .saturating_add(u64::from(config.ping_timeout_ms));
                drop(members);
                (&*cluster).tick(guarded_tick);
            }

            let queued_events = (&*callback_state)
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone();
            assert_eq!(
                queued_events,
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                ],
                "queued guarded publish must suppress tick-driven DEAD before delayed delivery runs"
            );

            release.wait();

            lost_done_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost thread should finish after the callback is released");
            lost_handle.join().expect("lost thread should not panic");

            let events = (&*callback_state)
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone();
            assert_eq!(
                events,
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                ],
                "guarded delivery should publish JOINED exactly once after the blocked callback drains"
            );

            let cluster_ref = &*cluster;
            {
                let members = cluster_ref
                    .members
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(member.state, MEMBER_ALIVE);
                assert_eq!(member.incarnation, 2);
            }
            {
                let tokens = cluster_ref
                    .connection_tokens
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert_eq!(tokens.current.get(&2), Some(&2));
                assert_eq!(tokens.visible.get(&2), Some(&2));
            }

            hew_cluster_free(cluster);
            drop(Box::from_raw(callback_state));
        }
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "test coordinates delayed guarded publish cancellation end-to-end"
    )]
    fn tokenized_connection_established_skips_removed_delivery_after_queueing() {
        struct BlockingMembershipState {
            events: std::sync::Mutex<Vec<(u16, u8)>>,
            suspect_seen: std::sync::mpsc::Sender<()>,
            release: std::sync::Arc<std::sync::Barrier>,
            blocked_first_suspect: AtomicBool,
        }

        extern "C" fn block_on_first_suspect(node_id: u16, event: u8, user_data: *mut c_void) {
            // SAFETY: user_data points at the BlockingMembershipState allocated in this test.
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
                state.release.wait();
            }
        }

        struct SendCluster(*mut HewCluster);
        // SAFETY: the test keeps the cluster alive until all worker threads join.
        unsafe impl Send for SendCluster {}

        let config = make_config(1);
        let publication_sync = std::sync::Arc::new(std::sync::Mutex::new(()));
        let publication_removed = std::sync::Arc::new(AtomicBool::new(false));
        let (suspect_tx, suspect_rx) = std::sync::mpsc::channel::<()>();
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        let callback_state = Box::into_raw(Box::new(BlockingMembershipState {
            events: std::sync::Mutex::new(Vec::new()),
            suspect_seen: suspect_tx,
            release: std::sync::Arc::clone(&release),
            blocked_first_suspect: AtomicBool::new(false),
        }));

        // SAFETY: test-owned cluster and callback state remain valid until the
        // explicit free/drop calls after all worker threads complete.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert!(!cluster.is_null());
            hew_cluster_set_membership_callback(
                cluster,
                block_on_first_suspect,
                callback_state.cast(),
            );
            assert_eq!(hew_cluster_join(cluster, 2, c"10.0.0.1:9000".as_ptr()), 0);
            assert_eq!(
                hew_cluster_notify_connection_established_for_token(cluster, 2, 1),
                0
            );

            let (lost_done_tx, lost_done_rx) = std::sync::mpsc::channel::<()>();
            let lost_cluster = SendCluster(cluster);
            let lost_handle = std::thread::spawn(move || {
                let cluster = lost_cluster;
                // SAFETY: cluster stays alive until this thread joins.
                let rc = hew_cluster_notify_connection_lost_if_current(cluster.0, 2, 1);
                assert_eq!(rc, 0);
                lost_done_tx
                    .send(())
                    .expect("lost thread should report completion");
            });

            suspect_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost path should reach the membership callback");

            let (publish_done_tx, publish_done_rx) = std::sync::mpsc::channel::<c_int>();
            let publish_cluster = SendCluster(cluster);
            let publication_sync_for_thread = std::sync::Arc::clone(&publication_sync);
            let publication_removed_for_thread = std::sync::Arc::clone(&publication_removed);
            let publish_handle = std::thread::spawn(move || {
                let cluster = publish_cluster;
                // SAFETY: cluster stays alive until this thread joins.
                let rc = hew_cluster_notify_connection_established_for_token_if_not_removed(
                    cluster.0,
                    2,
                    1,
                    2,
                    &publication_sync_for_thread,
                    &publication_removed_for_thread,
                );
                publish_done_tx
                    .send(rc)
                    .expect("publish thread should report completion");
            });

            assert_eq!(
                publish_done_rx
                    .recv_timeout(std::time::Duration::from_secs(1))
                    .expect("guarded publish should queue while the lost callback is blocked"),
                1
            );

            {
                let _publication = publication_sync
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                publication_removed.store(true, Ordering::Release);
            }

            let (retire_done_tx, retire_done_rx) = std::sync::mpsc::channel::<()>();
            let retire_cluster = SendCluster(cluster);
            let retire_handle = std::thread::spawn(move || {
                let cluster = retire_cluster;
                // SAFETY: cluster stays alive until this thread joins.
                let rc = hew_cluster_notify_connection_lost_if_current(cluster.0, 2, 2);
                assert_eq!(rc, 0);
                retire_done_tx
                    .send(())
                    .expect("retire thread should report completion");
            });

            retire_done_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("removal should retire the hidden token before lost drains");
            release.wait();

            lost_done_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost thread should finish after the callback is released");

            lost_handle.join().expect("lost thread should not panic");
            publish_handle
                .join()
                .expect("publish thread should not panic");
            retire_handle
                .join()
                .expect("retire thread should not panic");

            let events = (&*callback_state)
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone();
            assert_eq!(
                events,
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                ],
                "removing a hidden delayed publish must not emit stale JOINED or duplicate SUSPECT events"
            );
            let cluster_ref = &*cluster;
            {
                let tokens = cluster_ref
                    .connection_tokens
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert!(
                    !tokens.current.contains_key(&2),
                    "removing a hidden delayed publish must clear the staged token"
                );
                assert!(
                    !tokens.visible.contains_key(&2),
                    "removing a hidden delayed publish must not leak visibility"
                );
            }
            {
                let members = cluster_ref
                    .members
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                let member = members.iter().find(|m| m.node_id == 2).unwrap();
                assert_eq!(
                    member.state, MEMBER_SUSPECT,
                    "removing a hidden delayed publish must leave the node suspect"
                );
            }

            hew_cluster_free(cluster);
            drop(Box::from_raw(callback_state));
        }
    }

    #[test]
    fn tokenized_connection_established_clears_hidden_current_when_guarded_delivery_is_stale() {
        let cluster = HewCluster::new(make_config(1));
        let publication_sync = Arc::new(Mutex::new(()));
        let publication_removed = Arc::new(AtomicBool::new(false));

        cluster.upsert_member(2, MEMBER_SUSPECT, 1, b"10.0.0.1:9000");
        let transition = {
            let mut members = cluster.members.lock_or_recover();
            let transition = HewCluster::stage_member_transition_locked(
                &mut members,
                2,
                MEMBER_ALIVE,
                2,
                &[],
                false,
            )
            .expect("suspect member should stage a guarded ALIVE transition");
            if let Some(member) = members.iter_mut().find(|m| m.node_id == 2) {
                // SAFETY: hew_now_ms has no preconditions.
                member.last_seen_ms = unsafe { crate::io_time::hew_now_ms() };
            }
            transition.with_publication(PublicationTransition::GuardedTokenEstablished {
                publication_token: 2,
                publication_sync: Arc::clone(&publication_sync),
                publication_removed: Arc::clone(&publication_removed),
            })
        };

        {
            let mut tokens = cluster.connection_tokens.lock_or_recover();
            tokens.current.insert(2, 2);
        }

        cluster.upsert_member(2, MEMBER_DEAD, 3, &[]);
        cluster.deliver_member_transition(&transition);

        {
            let tokens = cluster.connection_tokens.lock_or_recover();
            assert!(
                !tokens.current.contains_key(&2),
                "suppressed guarded delivery must clear the hidden current token"
            );
            assert!(
                !tokens.visible.contains_key(&2),
                "suppressed guarded delivery must not leak visibility"
            );
        }
        {
            let members = cluster.members.lock_or_recover();
            let member = members.iter().find(|m| m.node_id == 2).unwrap();
            assert_eq!(member.state, MEMBER_DEAD);
            assert_eq!(member.incarnation, 3);
        }
    }

    #[test]
    fn tokenized_connection_establish_before_join_promotes_visible_lost() {
        let config = make_config(1);
        let mut events: Vec<(u16, u8)> = Vec::new();

        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert!(!cluster.is_null());
            hew_cluster_set_membership_callback(
                cluster,
                collect_membership_events,
                (&raw mut events).cast(),
            );

            assert_eq!(
                hew_cluster_notify_connection_established_for_token(cluster, 2, 1),
                0
            );
            assert_eq!(hew_cluster_join(cluster, 2, c"10.0.0.1:9000".as_ptr()), 0);
            assert_eq!(
                (&*cluster)
                    .connection_tokens
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .visible
                    .get(&2),
                Some(&1),
                "join should publish the pre-join connection token as externally visible"
            );

            assert_eq!(
                hew_cluster_notify_connection_lost_if_current(cluster, 2, 1),
                0
            );
            assert_eq!(
                events,
                vec![
                    (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                ],
                "a pre-join tokenized establish should still allow the later tokenized lost"
            );
            {
                let tokens = (&*cluster)
                    .connection_tokens
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert!(
                    !tokens.current.contains_key(&2) && !tokens.visible.contains_key(&2),
                    "lost should retire both current and visible token state"
                );
            }

            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn connection_lost_unknown_node_is_ignored() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert_eq!(hew_cluster_notify_connection_lost(cluster, 99), 0);
            assert_eq!(hew_cluster_member_count(cluster), 0);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn connection_established_unknown_node_is_ignored() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert_eq!(hew_cluster_notify_connection_established(cluster, 99), 0);
            assert_eq!(hew_cluster_member_count(cluster), 0);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn cluster_membership_callback_on_connection_lost() {
        let config = make_config(1);
        let mut events: Vec<(u16, u8)> = Vec::new();
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            let addr = c"10.0.0.2:9000";
            assert_eq!(hew_cluster_join(cluster, 2, addr.as_ptr()), 0);
            hew_cluster_set_membership_callback(
                cluster,
                collect_membership_events,
                (&raw mut events).cast(),
            );
            assert_eq!(hew_cluster_notify_connection_lost(cluster, 2), 0);
            hew_cluster_free(cluster);
        }
        assert_eq!(events, vec![(2, HEW_MEMBERSHIP_EVENT_NODE_SUSPECT)]);
    }

    // ── Registry gossip tests ──────────────────────────────────────────

    #[test]
    fn registry_add_event_queued() {
        let cluster = HewCluster::new(make_config(1));
        assert_eq!(cluster.registry_gossip_count(), 0);

        cluster.emit_registry_add("counter", 0x1234);
        assert_eq!(cluster.registry_gossip_count(), 1);

        let events = cluster.take_registry_gossip(10);
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].name, "counter");
        assert_eq!(events[0].actor_id, 0x1234);
        assert!(events[0].is_add);
    }

    #[test]
    fn registry_remove_event_queued() {
        let cluster = HewCluster::new(make_config(1));
        cluster.emit_registry_remove("counter");
        assert_eq!(cluster.registry_gossip_count(), 1);

        let events = cluster.take_registry_gossip(10);
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].name, "counter");
        assert_eq!(events[0].actor_id, 0);
        assert!(!events[0].is_add);
    }

    #[test]
    fn registry_events_deduplicate_by_name() {
        let cluster = HewCluster::new(make_config(1));
        cluster.emit_registry_add("counter", 0x1111);
        cluster.emit_registry_add("counter", 0x2222);
        assert_eq!(cluster.registry_gossip_count(), 1);

        let events = cluster.take_registry_gossip(10);
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].actor_id, 0x2222);
    }

    #[test]
    fn registry_events_pruned_after_dissemination() {
        let cluster = HewCluster::new(make_config(1));
        cluster.emit_registry_add("alpha", 1);
        // Disseminate 8 times to reach the prune threshold.
        for _ in 0..8 {
            let _ = cluster.take_registry_gossip(10);
        }
        assert_eq!(cluster.registry_gossip_count(), 0);
    }

    #[test]
    fn registry_callback_invoked_on_apply() {
        use std::ffi::CStr;

        extern "C" fn collect_registry(
            name: *const c_char,
            pid: u64,
            is_add: bool,
            user_data: *mut c_void,
        ) {
            // SAFETY: test passes a valid Vec pointer.
            let vec = unsafe { &mut *user_data.cast::<Vec<(String, u64, bool)>>() };
            // SAFETY: name is a valid NUL-terminated C string from the cluster callback.
            let s = unsafe { CStr::from_ptr(name) }
                .to_string_lossy()
                .into_owned();
            vec.push((s, pid, is_add));
        }

        let mut cluster = HewCluster::new(make_config(1));
        let mut collected: Vec<(String, u64, bool)> = Vec::new();
        cluster.registry_callback = Some(collect_registry);
        cluster.registry_callback_user_data = (&raw mut collected).cast();

        cluster.apply_registry_event("counter", 0x42, true);
        cluster.apply_registry_event("timer", 0, false);

        assert_eq!(collected.len(), 2);
        assert_eq!(collected[0], ("counter".to_owned(), 0x42, true));
        assert_eq!(collected[1], ("timer".to_owned(), 0, false));
    }

    #[test]
    fn registry_gossip_cabi_round_trip() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert_eq!(hew_cluster_registry_gossip_count(cluster), 0);

            let name = c"my_actor";
            hew_cluster_registry_add(cluster, name.as_ptr(), 0xABCD);
            assert_eq!(hew_cluster_registry_gossip_count(cluster), 1);

            hew_cluster_registry_remove(cluster, name.as_ptr());
            // Dedup replaces the add with a remove.
            assert_eq!(hew_cluster_registry_gossip_count(cluster), 1);

            hew_cluster_free(cluster);
        }
    }

    // ── take_gossip tests ──────────────────────────────────────────────

    #[test]
    fn take_gossip_respects_max_count() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"10.0.0.1:9000");
        cluster.upsert_member(3, MEMBER_ALIVE, 1, b"10.0.0.2:9000");
        cluster.upsert_member(4, MEMBER_ALIVE, 1, b"10.0.0.3:9000");

        let batch = cluster.take_gossip(2);
        assert_eq!(batch.len(), 2);
        assert_eq!(batch[0].node_id, 2);
        assert_eq!(batch[1].node_id, 3);
    }

    #[test]
    fn take_gossip_prunes_after_eight_disseminations() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"10.0.0.1:9000");

        // After 7 disseminations, the event should still be present.
        for _ in 0..7 {
            let batch = cluster.take_gossip(10);
            assert_eq!(
                batch.len(),
                1,
                "event should survive before reaching threshold"
            );
        }
        // The 8th take_gossip increments count to 8 and then prunes.
        let batch = cluster.take_gossip(10);
        assert_eq!(batch.len(), 1, "returned on the call that prunes");
        // Now the event is gone.
        let batch = cluster.take_gossip(10);
        assert_eq!(batch.len(), 0, "pruned after 8 disseminations");
    }

    // ── next_ping_target tests ─────────────────────────────────────────

    #[test]
    fn next_ping_target_empty_returns_none() {
        let cluster = HewCluster::new(make_config(1));
        assert_eq!(cluster.next_ping_target(), None);
    }

    #[test]
    fn next_ping_target_round_robins_through_members() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(10, MEMBER_ALIVE, 1, b"a:1");
        cluster.upsert_member(20, MEMBER_ALIVE, 1, b"b:1");

        let first = cluster.next_ping_target().unwrap();
        let second = cluster.next_ping_target().unwrap();
        let third = cluster.next_ping_target().unwrap();
        assert_eq!(first, 10);
        assert_eq!(second, 20);
        // Wraps around.
        assert_eq!(third, 10);
    }

    #[test]
    fn next_ping_target_skips_dead_and_left() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"a:1");
        cluster.upsert_member(3, MEMBER_DEAD, 5, b"b:1");
        cluster.upsert_member(4, MEMBER_LEFT, 1, b"c:1");
        cluster.upsert_member(5, MEMBER_SUSPECT, 1, b"d:1");

        // Only nodes 2 (alive) and 5 (suspect) should be selected.
        let mut targets = Vec::new();
        for _ in 0..4 {
            targets.push(cluster.next_ping_target().unwrap());
        }
        assert!(!targets.contains(&3), "dead member must not be pinged");
        assert!(!targets.contains(&4), "left member must not be pinged");
        assert!(targets.contains(&2));
        assert!(targets.contains(&5));
    }

    // ── emit_event tests ───────────────────────────────────────────────

    #[test]
    fn emit_event_deduplicates_by_node_id() {
        let cluster = HewCluster::new(make_config(1));
        cluster.emit_event(2, MEMBER_ALIVE, 1);
        cluster.emit_event(3, MEMBER_ALIVE, 1);
        cluster.emit_event(2, MEMBER_SUSPECT, 2);

        let events = cluster.events.lock().unwrap();
        // Node 2's first event should be replaced.
        assert_eq!(events.len(), 2);
        assert_eq!(events[0].node_id, 3);
        assert_eq!(events[1].node_id, 2);
        assert_eq!(events[1].new_state, MEMBER_SUSPECT);
    }

    #[test]
    fn emit_event_evicts_oldest_at_capacity() {
        let cluster = HewCluster::new(make_config(1));
        // Fill to MAX_GOSSIP_EVENTS with distinct node IDs.
        for i in 0..MAX_GOSSIP_EVENTS {
            #[expect(clippy::cast_possible_truncation, reason = "test values fit in u16")]
            let node_id = (i + 100) as u16;
            cluster.emit_event(node_id, MEMBER_ALIVE, 1);
        }
        {
            let events = cluster.events.lock().unwrap();
            assert_eq!(events.len(), MAX_GOSSIP_EVENTS);
        }

        // One more should evict the oldest (node 100).
        cluster.emit_event(999, MEMBER_ALIVE, 1);
        let events = cluster.events.lock().unwrap();
        assert_eq!(events.len(), MAX_GOSSIP_EVENTS);
        assert!(
            !events.iter().any(|e| e.node_id == 100),
            "oldest event (node 100) should have been evicted"
        );
        assert!(events.iter().any(|e| e.node_id == 999));
    }

    // ── process_message tests ──────────────────────────────────────────

    #[test]
    fn process_message_ping_recovers_suspect_to_alive() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_SUSPECT, 1, b"10.0.0.1:9000");

        // A PING from node 2 should update last_seen and recover to alive.
        cluster.process_message(SWIM_MSG_PING, 2, 1, 2);
        let members = cluster.members.lock().unwrap();
        assert_eq!(members[0].state, MEMBER_ALIVE);
    }

    #[test]
    fn process_message_unknown_type_is_noop() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"10.0.0.1:9000");

        // Unknown message type should not change anything.
        cluster.process_message(999, 2, 1, 2);
        let members = cluster.members.lock().unwrap();
        assert_eq!(members[0].state, MEMBER_ALIVE);
    }

    // ── upsert_member address tests ────────────────────────────────────

    #[test]
    fn upsert_member_truncates_long_address() {
        let cluster = HewCluster::new(make_config(1));
        let long_addr = [b'A'; 200];
        cluster.upsert_member(2, MEMBER_ALIVE, 1, &long_addr);

        let members = cluster.members.lock().unwrap();
        // Address should be truncated to 127 bytes + null terminator.
        assert_eq!(&members[0].addr[..127], &[b'A'; 127]);
        assert_eq!(members[0].addr[127], 0);
    }

    // ── tick edge cases ────────────────────────────────────────────────

    #[test]
    fn tick_skips_left_members() {
        let cluster = HewCluster::new(ClusterConfig {
            local_node_id: 1,
            ping_timeout_ms: 100,
            suspect_timeout_ms: 300,
            ..ClusterConfig::default()
        });
        cluster.upsert_member(2, MEMBER_LEFT, 1, b"10.0.0.1:9000");
        {
            let mut members = cluster.members.lock().unwrap();
            members[0].last_seen_ms = 0;
        }

        // Even after a long time, LEFT should remain LEFT (not transition to suspect/dead).
        cluster.tick(10_000);
        let members = cluster.members.lock().unwrap();
        assert_eq!(members[0].state, MEMBER_LEFT);
    }

    // ── connection_established on dead member ──────────────────────────

    #[test]
    fn connection_established_dead_member_stays_dead() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_DEAD, 5, b"10.0.0.1:9000");

        // Re-establishing a connection should not revive a dead member.
        cluster.notify_connection_established(2);
        let members = cluster.members.lock().unwrap();
        assert_eq!(
            members[0].state, MEMBER_DEAD,
            "dead member must not be revived via connection"
        );
    }

    // ── apply_registry_event edge cases ────────────────────────────────

    #[test]
    fn apply_registry_event_without_callback_is_noop() {
        let cluster = HewCluster::new(make_config(1));
        // No callback registered — should not panic.
        cluster.apply_registry_event("counter", 42, true);
    }

    #[test]
    fn apply_registry_event_name_with_interior_nul_is_noop() {
        extern "C" fn should_not_be_called(_: *const c_char, _: u64, _: bool, _: *mut c_void) {
            panic!("callback should not be invoked for invalid name");
        }
        let mut cluster = HewCluster::new(make_config(1));
        cluster.registry_callback = Some(should_not_be_called);
        cluster.registry_callback_user_data = std::ptr::null_mut();

        // Name with interior null byte — CString::new fails, early return.
        cluster.apply_registry_event("bad\0name", 42, true);
    }

    // ── membership callback edge cases ─────────────────────────────────

    #[test]
    fn alive_to_alive_upsert_skips_joined_callback() {
        let mut events: Vec<(u16, u8)> = Vec::new();
        let mut cluster = HewCluster::new(make_config(1));
        // SAFETY: pointers are valid for the duration of this test.
        unsafe {
            hew_cluster_set_membership_callback(
                &raw mut cluster,
                collect_membership_events,
                (&raw mut events).cast(),
            );
        }

        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"10.0.0.1:9000");
        // Same state, same incarnation — should NOT fire again.
        cluster.upsert_member(2, MEMBER_ALIVE, 1, &[]);

        // Only one JOINED event for the initial insert.
        assert_eq!(events.len(), 1);
        assert_eq!(events[0], (2, HEW_MEMBERSHIP_EVENT_NODE_JOINED));
    }

    #[test]
    fn membership_callback_dispatch_snapshot_keeps_callback_and_user_data_paired() {
        static OLD_USER_DATA: AtomicUsize = AtomicUsize::new(0);
        static NEW_USER_DATA: AtomicUsize = AtomicUsize::new(0);

        extern "C" fn record_old_user_data(_: u16, _: u8, user_data: *mut c_void) {
            OLD_USER_DATA.store(user_data as usize, Ordering::Relaxed);
        }

        extern "C" fn record_new_user_data(_: u16, _: u8, user_data: *mut c_void) {
            NEW_USER_DATA.store(user_data as usize, Ordering::Relaxed);
        }

        OLD_USER_DATA.store(0, Ordering::Relaxed);
        NEW_USER_DATA.store(0, Ordering::Relaxed);

        let mut old_tag = 0u8;
        let mut new_tag = 0u8;
        let mut cluster = HewCluster::new(make_config(1));
        let cluster_ptr = &raw mut cluster;
        let old_user_data = (&raw mut old_tag).cast();
        let new_user_data = (&raw mut new_tag).cast();

        // SAFETY: test owns the cluster and callback user data for this scope.
        unsafe {
            hew_cluster_replace_membership_callback(
                cluster_ptr,
                MembershipCallbackBinding::new(Some(record_old_user_data), old_user_data),
            );
        }

        let dispatched = cluster.with_membership_callback_dispatch(|callback, user_data| {
            // SAFETY: test owns the cluster and callback user data for this scope.
            unsafe {
                hew_cluster_replace_membership_callback(
                    cluster_ptr,
                    MembershipCallbackBinding::new(Some(record_new_user_data), new_user_data),
                );
            }
            callback(7, HEW_MEMBERSHIP_EVENT_NODE_DEAD, user_data);
        });
        assert!(dispatched.is_some());
        assert_eq!(
            OLD_USER_DATA.load(Ordering::Relaxed),
            old_user_data as usize
        );
        assert_eq!(NEW_USER_DATA.load(Ordering::Relaxed), 0);

        let _ = cluster.with_membership_callback_dispatch(|callback, user_data| {
            callback(7, HEW_MEMBERSHIP_EVENT_NODE_LEFT, user_data);
        });
        assert_eq!(
            NEW_USER_DATA.load(Ordering::Relaxed),
            new_user_data as usize
        );
    }

    // ── registry gossip overflow ───────────────────────────────────────

    #[test]
    fn registry_gossip_overflow_evicts_oldest() {
        let cluster = HewCluster::new(make_config(1));
        for i in 0..MAX_GOSSIP_EVENTS {
            cluster.emit_registry_add(&format!("actor_{i}"), i as u64);
        }
        assert_eq!(cluster.registry_gossip_count(), MAX_GOSSIP_EVENTS);

        // One more should evict the oldest.
        cluster.emit_registry_add("overflow", 999);
        assert_eq!(cluster.registry_gossip_count(), MAX_GOSSIP_EVENTS);

        let events = cluster.take_registry_gossip(MAX_GOSSIP_EVENTS + 1);
        assert!(
            !events.iter().any(|e| e.name == "actor_0"),
            "oldest should be evicted"
        );
        assert!(events.iter().any(|e| e.name == "overflow"));
    }

    // ── extended CABI null safety ──────────────────────────────────────

    #[test]
    fn null_safety_extended() {
        extern "C" fn noop_registry_cb(_: *const c_char, _: u64, _: bool, _: *mut c_void) {}

        // SAFETY: testing null safety of remaining CABI functions.
        unsafe {
            let null: *mut HewCluster = std::ptr::null_mut();

            // Functions that return -1 on null.
            assert_eq!(hew_cluster_join(null, 1, c"addr".as_ptr()), -1);
            assert_eq!(
                hew_cluster_process_message(null, SWIM_MSG_PING, 1, 1, 1),
                -1
            );
            assert_eq!(hew_cluster_notify_connection_lost(null, 1), -1);
            assert_eq!(hew_cluster_notify_connection_established(null, 1), -1);

            // Functions that return 0 on null.
            assert_eq!(hew_cluster_gossip_count(null), 0);
            assert_eq!(hew_cluster_registry_gossip_count(null), 0);

            // Functions that return gracefully on null.
            hew_cluster_leave(null);
            hew_cluster_set_callback(null, None);
            hew_cluster_set_membership_callback(
                null,
                collect_membership_events,
                std::ptr::null_mut(),
            );
            hew_cluster_set_registry_callback(null, noop_registry_cb, std::ptr::null_mut());

            // Null name pointers.
            hew_cluster_registry_add(null, std::ptr::null(), 0);
            hew_cluster_registry_remove(null, std::ptr::null());
        }
    }

    #[test]
    fn join_with_null_addr_returns_error() {
        let config = make_config(1);
        // SAFETY: test context.
        unsafe {
            let cluster = hew_cluster_new(&raw const config);
            assert_eq!(hew_cluster_join(cluster, 2, std::ptr::null()), -1);
            assert_eq!(hew_cluster_member_count(cluster), 0);
            hew_cluster_free(cluster);
        }
    }

    #[test]
    fn hew_cluster_new_null_config_returns_null() {
        // SAFETY: testing null safety.
        unsafe {
            let cluster = hew_cluster_new(std::ptr::null());
            assert!(cluster.is_null());
        }
    }

    // ── ClusterProtocol trait-surface tests ────────────────────────────
    //
    // Gate requirement: at least 2 unit tests for the trait surface.
    //   1. `cluster_protocol_dyn_dispatch_produces_correct_decisions`
    //      — calls through `&dyn ClusterProtocol`, verifying that trait
    //      object dispatch routes to the correct decisions for each
    //      SWIM message type.
    //   2. `simple_swim_identical_to_pre_extraction_on_ping_ack_sequence`
    //      — drives `HewCluster` through the same PING/ACK sequence that
    //      the pre-extraction `process_message` implementation handled,
    //      and asserts that membership state is identical to the expected
    //      pre-extraction outcome.

    #[test]
    fn cluster_protocol_dyn_dispatch_produces_correct_decisions() {
        let protocol: Box<dyn ClusterProtocol> = Box::new(SimpleSwim::new());

        // PING: update last_seen, do NOT upsert alive.
        let d = protocol.handle_ping(2);
        assert!(d.update_last_seen, "PING must update last_seen");
        assert!(!d.upsert_alive, "PING must not upsert alive");

        // ACK: update last_seen AND upsert alive.
        let d = protocol.handle_ack(2, 1);
        assert!(d.update_last_seen, "ACK must update last_seen");
        assert!(d.upsert_alive, "ACK must upsert alive");

        // PING_REQ: update last_seen, do NOT upsert alive (forwarding is
        // the C3 driver's responsibility).
        let d = protocol.handle_ping_req(3);
        assert!(d.update_last_seen, "PING_REQ must update last_seen");
        assert!(!d.upsert_alive, "PING_REQ must not upsert alive");

        // GOSSIP: no-op default.
        let d = protocol.handle_gossip(4, 1);
        assert!(!d.update_last_seen, "GOSSIP default must be a no-op");
        assert!(!d.upsert_alive, "GOSSIP default must be a no-op");
    }

    #[test]
    fn simple_swim_identical_to_pre_extraction_on_ping_ack_sequence() {
        // Pre-extraction behaviour (now implemented via trait dispatch):
        //
        //   1. Node 2 is MEMBER_SUSPECT.
        //   2. Receiving SWIM_MSG_PING from node 2 calls update_last_seen,
        //      which recovers node 2 to MEMBER_ALIVE.
        //   3. Receiving SWIM_MSG_ACK from node 2 marks it MEMBER_ALIVE
        //      via upsert_member.
        //
        // The post-extraction cluster must produce the same final
        // membership state as the pre-extraction cluster would have.

        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_SUSPECT, 1, b"10.0.0.2:9000");

        // Step 1: PING recovers SUSPECT → ALIVE via update_last_seen.
        cluster.process_message(SWIM_MSG_PING, 2, 1, 2);
        {
            let members = cluster.members.lock().unwrap();
            assert_eq!(
                members[0].state, MEMBER_ALIVE,
                "PING from a SUSPECT peer must recover it to ALIVE"
            );
        }

        // Step 2: drive node 2 back to SUSPECT manually to test ACK path.
        cluster.upsert_member(2, MEMBER_SUSPECT, 1, b"");

        // Step 3: ACK marks MEMBER_ALIVE via upsert_member (incarnation bump).
        cluster.process_message(SWIM_MSG_ACK, 2, 2, 2);
        {
            let members = cluster.members.lock().unwrap();
            assert_eq!(
                members[0].state, MEMBER_ALIVE,
                "ACK must mark the sender MEMBER_ALIVE"
            );
            assert_eq!(
                members[0].incarnation, 2,
                "ACK must update incarnation to the supplied value"
            );
        }
    }

    // ── SWIM gossip export / import / self-refutation (C5/C6) ───────────

    #[test]
    fn take_swim_gossip_exports_pending_member_events() {
        let cluster = HewCluster::new(make_config(1));
        // A membership transition queues a gossip event.
        cluster.upsert_member(2, MEMBER_ALIVE, 3, b"10.0.0.2:9000");
        cluster.emit_event(2, MEMBER_DEAD, 5);

        let gossip = cluster.take_swim_gossip(8);
        assert!(
            gossip
                .iter()
                .any(|&(n, s, i)| n == 2 && s == MEMBER_DEAD && i == 5),
            "exported gossip must carry the DEAD transition: {gossip:?}"
        );
    }

    #[test]
    fn apply_swim_gossip_folds_remote_death_into_membership() {
        let cluster = HewCluster::new(make_config(1));
        cluster.upsert_member(2, MEMBER_ALIVE, 1, b"10.0.0.2:9000");

        // A peer gossips that node 2 is DEAD at a higher incarnation.
        cluster.apply_swim_gossip(&[(2, MEMBER_DEAD, 2)]);

        let members = cluster.members.lock().unwrap();
        let m2 = members
            .iter()
            .find(|m| m.node_id == 2)
            .expect("node 2 present");
        assert_eq!(
            m2.state, MEMBER_DEAD,
            "remote DEAD gossip must transition the member to DEAD"
        );
    }

    #[test]
    fn apply_swim_gossip_ignores_entries_about_self() {
        let cluster = HewCluster::new(make_config(1));
        // Gossip claiming the local node (id 1) is DEAD must be ignored here;
        // self-state is authoritative (refutation handled separately).
        cluster.apply_swim_gossip(&[(1, MEMBER_DEAD, 99)]);
        let members = cluster.members.lock().unwrap();
        assert!(
            !members
                .iter()
                .any(|m| m.node_id == 1 && m.state == MEMBER_DEAD),
            "self DEAD gossip must not mark the local node dead"
        );
    }

    #[test]
    fn refute_if_suspected_bumps_incarnation_past_suspicion() {
        let cluster = HewCluster::new(make_config(1));
        let before = cluster.local_incarnation();

        // A peer suspects the local node (id 1) at incarnation 7.
        let new_inc = cluster
            .refute_if_suspected(&[(1, MEMBER_SUSPECT, 7)])
            .expect("self-suspicion must trigger refutation");

        assert!(
            new_inc > 7,
            "refuted incarnation {new_inc} must exceed the suspicion (7)"
        );
        assert!(
            new_inc > before,
            "refutation must advance the local incarnation"
        );
        assert_eq!(cluster.local_incarnation(), new_inc);

        // The refutation is queued as an ALIVE-about-self gossip event.
        let gossip = cluster.take_swim_gossip(8);
        assert!(
            gossip
                .iter()
                .any(|&(n, s, i)| n == 1 && s == MEMBER_ALIVE && i == new_inc),
            "refutation must enqueue an ALIVE-about-self gossip event: {gossip:?}"
        );
    }

    #[test]
    fn refute_if_suspected_noop_when_not_suspected() {
        let cluster = HewCluster::new(make_config(1));
        let before = cluster.local_incarnation();
        // Gossip about other nodes, and ALIVE-about-self, must not refute.
        assert_eq!(
            cluster.refute_if_suspected(&[(2, MEMBER_SUSPECT, 9), (1, MEMBER_ALIVE, 3)]),
            None
        );
        assert_eq!(cluster.local_incarnation(), before);
    }
}
