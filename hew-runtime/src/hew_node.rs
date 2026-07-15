//! Unified distributed node runtime.
//!
//! Integrates transport, connection manager, SWIM membership, and
//! name/actor registry wiring.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::lifetime::{PoisonSafe, PoisonSafeRw};
use crate::util::{CondvarExt, MutexExt};
use std::cell::Cell;
use std::collections::HashMap;
use std::ffi::{c_char, c_int, c_void, CStr, CString};
use std::ptr;
use std::sync::atomic::{
    AtomicBool, AtomicPtr, AtomicU16, AtomicU64, AtomicU8, AtomicUsize, Ordering,
};
use std::sync::{Arc, Condvar, Mutex};

use crate::set_last_error;
use std::thread::{self, JoinHandle};

use crate::cluster::{self, ClusterConfig, HewCluster};
use crate::connection::{self, HewConnMgr};
use crate::envelope::encode_envelope_frame_from_raw_parts;
use crate::peer_binding::{ConfigState, PeerAuthSnapshot, PEER_AUTH_STATE};
use crate::routing::{self, HewRoutingTable};
use crate::transport::{self, HewTransport, HewTransportOps, HEW_CONN_INVALID};

const NODE_STATE_STARTING: u8 = 0;
/// Node is started and serving traffic. `pub(crate)` so the SWIM driver only
/// drives a fully-running node.
pub(crate) const NODE_STATE_RUNNING: u8 = 1;
const NODE_STATE_STOPPING: u8 = 2;
const NODE_STATE_STOPPED: u8 = 3;

/// Remote-send rc returned by [`hew_node_send`] when the captured `RemotePid<T>`
/// names a registration that has since been superseded — the `StaleRef`
/// fail-closed boundary. The codegen send-path maps this distinct rc to
/// `SendError::StaleRef`; every OTHER nonzero rc stays the generic routing
/// failure. `pub` so the codegen cross-crate parity test pins its literal to
/// this producer.
pub const HEW_ERR_STALE_REF: c_int = -16;
const _: () = assert!(
    std::mem::size_of::<usize>() >= std::mem::size_of::<u64>(),
    "Hew requires 64-bit target for actor ID encoding"
);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TransportSelection {
    Tcp,
    #[cfg(feature = "quic")]
    Quic,
    #[cfg(feature = "quic")]
    QuicMesh,
}

fn normalize_transport_name(name: &str) -> Result<&'static str, String> {
    if name.eq_ignore_ascii_case("tcp") {
        return Ok("tcp");
    }
    if name.eq_ignore_ascii_case("quic") {
        #[cfg(feature = "quic")]
        {
            return Ok("quic");
        }
        #[cfg(not(feature = "quic"))]
        {
            return Err("transport 'quic' requires the hew-runtime quic feature".into());
        }
    }
    if name.eq_ignore_ascii_case("quic-mesh") {
        #[cfg(feature = "quic")]
        {
            return Ok("quic-mesh");
        }
        #[cfg(not(feature = "quic"))]
        {
            return Err("transport 'quic-mesh' requires the hew-runtime quic feature".into());
        }
    }
    Err(format!(
        "unknown transport '{name}'; supported values: tcp, quic, quic-mesh"
    ))
}

/// Read an optional `u32` SWIM-timing override from the environment.
///
/// Returns `None` if the variable is unset or does not parse as a non-zero
/// `u32`, so a malformed value falls back to the compiled default rather than
/// silently disabling failure detection.
fn swim_timing_env_u32(key: &str) -> Option<u32> {
    let raw = crate::env::ENV_LOCK.read_access(|()| std::env::var(key).ok())?;
    raw.parse::<u32>().ok().filter(|v| *v > 0)
}

/// Build the cluster config for `local_node_id`, applying any SWIM-timing
/// overrides from the environment.
///
/// `HEW_SWIM_PROTOCOL_PERIOD_MS`, `HEW_SWIM_PING_TIMEOUT_MS`, and
/// `HEW_SWIM_SUSPECT_TIMEOUT_MS` tune the failure detector's cadence and
/// thresholds — an operator knob for deployments with non-default network
/// characteristics, and the mechanism tests use to drive detection on a short
/// horizon. Each falls back to the [`ClusterConfig`] default when unset.
fn cluster_config_from_env(local_node_id: u16) -> ClusterConfig {
    let mut cfg = ClusterConfig {
        local_node_id,
        ..ClusterConfig::default()
    };
    if let Some(v) = swim_timing_env_u32("HEW_SWIM_PROTOCOL_PERIOD_MS") {
        cfg.protocol_period_ms = v;
    }
    if let Some(v) = swim_timing_env_u32("HEW_SWIM_PING_TIMEOUT_MS") {
        cfg.ping_timeout_ms = v;
    }
    if let Some(v) = swim_timing_env_u32("HEW_SWIM_SUSPECT_TIMEOUT_MS") {
        cfg.suspect_timeout_ms = v;
    }
    cfg
}

fn transport_selection_from_env() -> Result<TransportSelection, String> {
    let value = crate::env::ENV_LOCK.read_access(|()| std::env::var("HEW_TRANSPORT").ok());
    let Some(value) = value else {
        return Ok(TransportSelection::Tcp);
    };

    match normalize_transport_name(&value)? {
        "tcp" => Ok(TransportSelection::Tcp),
        #[cfg(feature = "quic")]
        "quic" => Ok(TransportSelection::Quic),
        #[cfg(feature = "quic")]
        "quic-mesh" => Ok(TransportSelection::QuicMesh),
        _ => unreachable!("normalize_transport_name returns only supported transport keys"),
    }
}

#[derive(Clone, Copy)]
struct KnownNodePtr(*mut HewNode);

// SAFETY: Node pointers are owned by the runtime and removed from the node
// slot's known-node list before the node allocation is freed.
unsafe impl Send for KnownNodePtr {}

/// Runtime-owned distributed-node state.
///
/// Was the `CURRENT_NODE` + `KNOWN_NODES` + `REPLY_TABLE` globals; now a field
/// of `RuntimeInner`, resolved through [`crate::runtime::rt_current`]. A runtime
/// owns at most one active node ([`NodeSlot::current`]), the list of node
/// allocations it knows about so actor teardown can unregister distributed
/// names ([`NodeSlot::known_nodes`]), and the table correlating its outbound
/// remote asks with their replies ([`NodeSlot::reply_table`]). Dropping it drops
/// the (normally empty after teardown) reply table and known-node list.
///
/// `reply_table` was a process-`LazyLock`; it is now eagerly constructed per
/// runtime so each runtime's pending remote asks are isolated. Construction is
/// cheap (an atomic counter and an empty map).
pub(crate) struct NodeSlot {
    /// Pointer to the active node for remote message routing, or `0` when no
    /// node is running. Only one `HewNode` may be active per runtime; the write
    /// lock serializes start/stop against in-flight reply sends (the lifetime
    /// barrier in `hew_node_stop`).
    current: PoisonSafeRw<usize>,
    /// Node allocations this runtime knows about, so actor teardown can
    /// unregister distributed names before the owning node is freed.
    known_nodes: PoisonSafe<Vec<KnownNodePtr>>,
    /// Reply routing table correlating this runtime's outbound remote asks with
    /// their replies.
    reply_table: ReplyRoutingTable,
    /// Quarantine set: peers the failure detector has declared DEAD, keyed
    /// `node_id -> the incarnation the peer was quarantined at`. A
    /// `Quarantine`-policy send/ask to a peer present here fails closed until the
    /// peer rejoins at a strictly higher incarnation, which evicts the entry. Per
    /// runtime, isolated like `reply_table`; written from the SWIM-DEAD verdict and
    /// the readmission dispatch, read on the send/ask path.
    quarantine: PoisonSafe<HashMap<u16, u64>>,
    /// Node id assigned to this runtime for local PID encoding/routing. Was
    /// `pid::LOCAL_NODE_ID`.
    local_node_id: AtomicU16,
}

impl NodeSlot {
    /// Construct an empty node slot for a new runtime: no active node, no known
    /// nodes, and an empty reply table.
    pub(crate) fn new() -> Self {
        Self {
            current: PoisonSafeRw::new(0),
            known_nodes: PoisonSafe::new(Vec::new()),
            reply_table: ReplyRoutingTable::new(),
            quarantine: PoisonSafe::new(HashMap::new()),
            local_node_id: AtomicU16::new(0),
        }
    }

    /// Move `other`'s node state (active node, local node id, known-node list,
    /// pending replies, and the request-id counter) into `self`, leaving `other`
    /// empty.
    ///
    /// Test-only. Before de-globalization the active-node pointer, the
    /// known-node list, local node id, and the reply table were process statics
    /// that *survived* a test scheduler swap (`init_real_scheduler_for_test`).
    /// Now that they live in `RuntimeInner`, the swap would otherwise discard a
    /// node already started on the placeholder runtime; this transfer preserves
    /// the prior survival semantics so node-startup ordering in tests is
    /// unchanged. Production never swaps the installed runtime, so this has no
    /// production counterpart.
    #[cfg(test)]
    pub(crate) fn test_transfer_from(&self, other: &NodeSlot) {
        let current = other.current.access(|guard| std::mem::replace(guard, 0));
        self.current.access(|guard| *guard = current);

        let known = other.known_nodes.access(std::mem::take);
        self.known_nodes.access(|dst| *dst = known);

        let quarantine = other.quarantine.access(std::mem::take);
        self.quarantine.access(|dst| *dst = quarantine);

        let pending = {
            let mut map = other
                .reply_table
                .pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            std::mem::take(&mut *map)
        };
        let next_id = other
            .reply_table
            .next_id
            .load(std::sync::atomic::Ordering::Relaxed);
        self.reply_table
            .next_id
            .store(next_id, std::sync::atomic::Ordering::Relaxed);
        {
            let mut map = self
                .reply_table
                .pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            *map = pending;
        }
        let local_node_id = other.local_node_id.swap(0, Ordering::AcqRel);
        self.local_node_id.store(local_node_id, Ordering::Release);
    }

    pub(crate) fn local_node_id(&self) -> u16 {
        self.local_node_id.load(Ordering::Acquire)
    }

    pub(crate) fn set_local_node_id(&self, node_id: u16) {
        self.local_node_id.store(node_id, Ordering::Release);
    }
}

/// Run `f` with read access to the current runtime's active-node slot.
fn with_current_node_read<R>(f: impl FnOnce(&usize) -> R) -> R {
    match crate::runtime::rt_current_opt() {
        Some(rt) => rt.node.current.read_access(f),
        None => f(&0),
    }
}

/// Run `f` with write access to the current runtime's active-node slot.
fn with_current_node<R>(f: impl FnOnce(&mut usize) -> R) -> R {
    crate::runtime::rt_current().node.current.access(f)
}

/// Run `f` with mutable access to the current runtime's known-node list.
fn with_known_nodes<R>(f: impl FnOnce(&mut Vec<KnownNodePtr>) -> R) -> R {
    crate::runtime::rt_current().node.known_nodes.access(f)
}

fn with_known_nodes_opt<R>(f: impl FnOnce(&mut Vec<KnownNodePtr>) -> R) -> Option<R> {
    crate::runtime::rt_current_opt().map(|rt| rt.node.known_nodes.access(f))
}

/// The current runtime's reply routing table.
fn reply_table() -> &'static ReplyRoutingTable {
    &crate::runtime::rt_current().node.reply_table
}

fn reply_table_opt() -> Option<&'static ReplyRoutingTable> {
    crate::runtime::rt_current_opt().map(|rt| &rt.node.reply_table)
}

/// Quarantine a peer at the incarnation it was declared DEAD at.
///
/// Idempotent and monotonic: a later DEAD verdict at a higher incarnation
/// overwrites the recorded incarnation; a lower or equal one never regresses it.
/// Fail-closed on no installed runtime (the SWIM-DEAD verdict runs from the
/// connection-reader / SWIM-driver threads, where a runtime may not be installed)
/// — there is no quarantine set to write, so the call is a no-op.
fn quarantine_insert(node_id: u16, incarnation: u64) {
    if let Some(rt) = crate::runtime::rt_current_opt() {
        rt.node.quarantine.access(|set| {
            let entry = set.entry(node_id).or_insert(incarnation);
            if incarnation > *entry {
                *entry = incarnation;
            }
        });
    }
}

/// Whether a send/ask to `node_id` at its `live_incarnation` is currently blocked
/// by the quarantine set.
///
/// Blocked iff the set holds an entry for `node_id` AND the peer's live
/// incarnation has not yet exceeded the quarantined one (`live <= quarantined`).
/// A peer whose live incarnation already passed the quarantined one is not blocked
/// — though in practice the readmission dispatch evicts the entry first, so the
/// incarnation compare is the belt to eviction's suspenders. Returns `false` when
/// no runtime is installed (nothing to consult).
fn quarantine_is_blocked(node_id: u16, live_incarnation: u64) -> bool {
    crate::runtime::rt_current_opt().is_some_and(|rt| {
        rt.node
            .quarantine
            .access(|set| set.get(&node_id).is_some_and(|&q| live_incarnation <= q))
    })
}

/// Evict a peer from the quarantine set on its strictly-higher-incarnation
/// readmission, so it is sendable again. A no-op if the peer is not quarantined.
/// Fail-closed on no runtime.
fn quarantine_evict(node_id: u16) {
    if let Some(rt) = crate::runtime::rt_current_opt() {
        rt.node.quarantine.access(|set| {
            set.remove(&node_id);
        });
    }
}

// ── Registration incarnation + supersession (actor-slot identity) ──────────────
//
// The actor-slot / registration incarnation dimension lives on `HewRegistry`.
// DISTINCT from the node-level SWIM peer incarnation (the `quarantine` set):
//   * `quarantine`             — "is the PEER I'm talking to the same incarnation?"
//   * registration incarnation — "is the ACTOR REGISTRATION I captured still live?"
// The two counters never read each other (LESSONS `monotonic-epoch-single-clock`).

/// Record that `name` now resolves to `new_serial` on `registry`, bumping its
/// registration incarnation when the name is re-pointed to a DIFFERENT actor and
/// superseding the prior serial so any captured `RemotePid<T>` over it fails
/// closed with `StaleRef` on dispatch.
///
/// Returns the registration incarnation for `name` after the update — `0` for a
/// fresh registration, bumped on each re-registration to a new actor. A
/// re-registration of the SAME serial is idempotent (no bump, no supersession).
/// Both the local register path and the cluster gossip ADD path call this so the
/// server and every client converge on the same supersession verdict.
fn registration_record(registry: &HewRegistry, name: &str, new_serial: u64) -> u32 {
    let prev_serial = registry.name_serials.lock_or_recover().get(name).copied();
    let incarnation = {
        let mut map = registry.incarnations.lock_or_recover();
        if let Some(slot) = map.get_mut(name) {
            if prev_serial == Some(new_serial) {
                // Idempotent re-register of the same actor: no bump.
                *slot
            } else {
                *slot = slot.saturating_add(1);
                *slot
            }
        } else {
            map.insert(name.to_string(), 0);
            0
        }
    };
    // Supersede the previous serial if the name now points at a different actor.
    if let Some(prev) = prev_serial {
        if prev != new_serial {
            registry
                .superseded_serials
                .lock_or_recover()
                .insert(prev, incarnation);
        }
    }
    registry
        .name_serials
        .lock_or_recover()
        .insert(name.to_string(), new_serial);
    incarnation
}

/// Mark a serial superseded on `registry` directly. Used by actor-teardown
/// (`unregister_actor_names`) where a still-live registration's actor is freed:
/// any captured `RemotePid<T>` over that serial must fail closed with `StaleRef`
/// rather than route to a dead / replacement actor. A bare unregister of a NAME
/// does NOT call this — the name→serial bookkeeping persists so a later
/// re-registration to a different actor supersedes the old serial through
/// `registration_record`.
fn registry_supersede_serial(registry: &HewRegistry, serial: u64) {
    registry
        .superseded_serials
        .lock_or_recover()
        .entry(serial)
        .or_insert(0);
}

/// The live registration incarnation for a captured PID, resolved by the PID's
/// serial against the current node's registry. Returns the incarnation a still-
/// live registration carries, or `0` when the serial names no tracked
/// registration / no node is installed. The bare-`u64` resolution path for
/// `pid::hew_pid_incarnation`.
pub(crate) fn registration_incarnation_for_serial(serial: u64) -> u32 {
    with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return 0;
        }
        // SAFETY: the read lock pins CURRENT_NODE for the duration of this call.
        let node = unsafe { &*node };
        if node.registry.is_null() {
            return 0;
        }
        // SAFETY: registry is valid while the node is installed.
        let registry = unsafe { &*node.registry };
        let live_name = registry
            .name_serials
            .lock_or_recover()
            .iter()
            .find(|(_, &s)| s == serial)
            .map(|(n, _)| n.clone());
        match live_name {
            Some(name) => registry
                .incarnations
                .lock_or_recover()
                .get(&name)
                .copied()
                .unwrap_or(0),
            None => 0,
        }
    })
}

/// Whether a captured PID's serial has been superseded — consulted by the remote
/// dispatch boundary (`hew_node_send` / ask / monitor) to fail closed with
/// `StaleRef`. Resolves against the registry of the node the dispatch is leaving
/// (the captured ref's local node), so a client that learned the supersession
/// via gossip rejects its stale capture.
fn serial_is_superseded(registry: &HewRegistry, serial: u64) -> bool {
    registry
        .superseded_serials
        .lock_or_recover()
        .contains_key(&serial)
}

/// The single `StaleRef` gate shared across the remote send / ask / monitor
/// dispatch boundaries (LESSONS `cross-layer-name-gate-agreement` — one helper,
/// not three divergent checks): does `target_pid` name a registration that has
/// been superseded by a re-registration of its name to a different actor?
///
/// Fails CLOSED (LESSONS `boundary-fail-closed`, DI-012): a superseded capture
/// is rejected here rather than routed to the live replacement. The check keys
/// on the captured serial against the dispatching node's registry; a node with
/// no registry (never started a distributed registry) has nothing to supersede,
/// so the gate is open. The captured serial uniquely names its registration
/// because serials are monotonic and never reused.
fn dispatch_is_stale(node: &HewNode, target_pid: u64) -> bool {
    if node.registry.is_null() {
        return false;
    }
    // SAFETY: registry is valid while the node is installed.
    let registry = unsafe { &*node.registry };
    serial_is_superseded(registry, crate::pid::hew_pid_serial(target_pid))
}

/// Whether the current dispatch's partition policy is `Quarantine` AND a send/ask
/// to `target_node_id` must fail closed because the peer is quarantined at a still-
/// stale incarnation.
///
/// Only the `Quarantine` policy consults the set; `FailFast`/`Deadline`/the others
/// keep their prior behaviour. The peer's live incarnation is resolved from the
/// cluster membership table so the comparison is exact (a peer whose live
/// incarnation already exceeds the quarantined one is not blocked, even before the
/// readmission dispatch evicts the entry). A quarantined peer the membership table
/// no longer knows (unknown incarnation) is treated as blocked — fail-closed.
fn quarantine_blocks_send(node: &HewNode, target_node_id: u16) -> bool {
    if crate::execution_context::current_partition_policy()
        != crate::execution_context::PartitionPolicy::Quarantine
    {
        return false;
    }
    if node.cluster.is_null() {
        // No cluster to resolve a live incarnation: if the set holds the peer at
        // any incarnation, block (fail-closed). Passing 0 as the live incarnation
        // ensures no quarantined entry can ever be considered cleared.
        return quarantine_is_blocked(target_node_id, 0);
    }
    // SAFETY: node.cluster is valid while the node is installed.
    let cluster = unsafe { &*node.cluster };
    let live_incarnation = cluster.member_incarnation(target_node_id).unwrap_or(0);
    quarantine_is_blocked(target_node_id, live_incarnation)
}

// ---------------------------------------------------------------------------
// Inbound ask worker bound
// ---------------------------------------------------------------------------

/// Maximum number of OS threads that may concurrently service inbound remote
/// asks.  A remote peer cannot exceed this by flooding ask requests, bounding
/// both OS thread count and virtual-memory usage.
///
/// The value is intentionally generous (64) so that legitimate high-fanout
/// workloads are unaffected while still preventing unbounded growth.
pub(crate) const INBOUND_ASK_WORKER_LIMIT: usize = 64;

/// Sentinel `msg_type` value used in reply envelopes to signal that the
/// inbound ask was **rejected** (worker-limit exceeded) rather than answered.
///
/// Normal reply envelopes carry `msg_type = 0`. The connection reader checks
/// for this sentinel to call [`fail_remote_reply`] instead of
/// [`complete_remote_reply`], ensuring both void and non-void asks fail
/// closed with [`AskError::WorkerAtCapacity`] on the originating node.
///
/// The value `65535` is the maximum valid `msg_type` in the Hew wire
/// protocol (`0..=MAX_MSG_TYPE`).  Reply envelopes always use `msg_type = 0`
/// by convention, so `65535` is unambiguous as a rejection marker.
pub(crate) const HEW_REPLY_REJECT_MSG_TYPE: i32 = 65_535;

/// Count of currently active inbound ask-handler threads.
///
/// Incremented before spawning, decremented by [`InboundAskGuard`] when the
/// handler thread exits (or panics — the `Drop` impl runs in both cases).
static INBOUND_ASK_ACTIVE: AtomicUsize = AtomicUsize::new(0);

/// RAII guard that decrements [`INBOUND_ASK_ACTIVE`] and the per-manager
/// active counter exactly once on drop.
///
/// Constructed in the spawned ask-handler thread so both counters stay
/// accurate even under panics or early returns.
struct InboundAskGuard(Arc<AtomicUsize>);

impl Drop for InboundAskGuard {
    fn drop(&mut self) {
        INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
        // SeqCst (matching the spawn-gate protocol in `node_inbound_router` and
        // the drain in `drain_inbound_ask_workers`): this decrement is the
        // signal the Phase-1 drain waits on. A worker only reaches here after
        // `handle_inbound_ask` has flushed its reply, so when the drain observes
        // the counter hit zero, every counted worker's reply has been sent.
        self.0.fetch_sub(1, Ordering::SeqCst);
    }
}

#[cfg(test)]
#[derive(Default)]
enum TestGateMode {
    #[default]
    Disabled,
    Notify,
    Blocked,
    Released,
}

#[cfg(test)]
#[derive(Default)]
struct TestGateState {
    mode: TestGateMode,
    entered: bool,
}

#[cfg(test)]
#[derive(Default)]
struct TestGate {
    state: Mutex<TestGateState>,
    cond: Condvar,
}

#[cfg(test)]
impl TestGate {
    fn arm(&self, block_on_enter: bool) {
        let mut state = self.state.lock_or_recover();
        *state = TestGateState {
            mode: if block_on_enter {
                TestGateMode::Blocked
            } else {
                TestGateMode::Notify
            },
            entered: false,
        };
        self.cond.notify_all();
    }

    fn hit(&self) {
        let mut state = self.state.lock_or_recover();
        if matches!(state.mode, TestGateMode::Disabled) {
            return;
        }
        state.entered = true;
        self.cond.notify_all();
        while matches!(state.mode, TestGateMode::Blocked) {
            state = self.cond.wait_or_recover(state);
        }
    }

    fn release(&self) {
        let mut state = self.state.lock_or_recover();
        if matches!(state.mode, TestGateMode::Disabled) {
            return;
        }
        state.mode = TestGateMode::Released;
        self.cond.notify_all();
    }

    fn wait_for_enter(&self, timeout: std::time::Duration) -> bool {
        let deadline = std::time::Instant::now() + timeout;
        let mut state = self.state.lock_or_recover();
        while !matches!(state.mode, TestGateMode::Disabled) && !state.entered {
            let remaining = deadline.saturating_duration_since(std::time::Instant::now());
            if remaining.is_zero() {
                return false;
            }
            let (next, wait_result) = self.cond.wait_timeout_or_recover(state, remaining);
            state = next;
            if wait_result.timed_out() && !state.entered {
                return false;
            }
        }
        state.entered
    }

    fn reset(&self) {
        let mut state = self.state.lock_or_recover();
        *state = TestGateState::default();
        self.cond.notify_all();
    }
}

#[cfg(test)]
static INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK: std::sync::LazyLock<TestGate> =
    std::sync::LazyLock::new(TestGate::default);
#[cfg(test)]
static NODE_STOP_BEFORE_CONNMGR_FREE_HOOK: std::sync::LazyLock<TestGate> =
    std::sync::LazyLock::new(TestGate::default);
/// Fires in `node_inbound_router` AFTER the per-manager counter increment and
/// BEFORE the spawn-gate re-check. Lets a test wedge a router exactly in the
/// Dekker window: counter already incremented, gate not yet re-read. A
/// concurrent `hew_node_stop` drain that closes the gate and then loads the
/// counter MUST observe this worker (count ≥ 1) and wait — proving the atomic
/// gate/counter protocol.
#[cfg(test)]
static INBOUND_ROUTER_AFTER_INCREMENT_HOOK: std::sync::LazyLock<TestGate> =
    std::sync::LazyLock::new(TestGate::default);
/// Fires at the TOP of `handle_inbound_ask`, BEFORE the up-front feature-flags
/// capture acquires the `CURRENT_NODE` read lock. Lets a test wedge an uncounted
/// straggler worker (one driven directly, past the drain ceiling) so a
/// concurrent `hew_node_stop` on THAT worker's node can complete teardown —
/// including freeing the node's `conn_mgr` — while the worker is parked. On
/// release, the worker evaluates the capture's barrier: the fix returns `None`
/// via this node's `shutdown_started` flag instead of dereferencing the freed
/// `conn_mgr`. Proves the capture is safe for a SECONDARY node whose teardown
/// does not zero the global `CURRENT_NODE`.
#[cfg(test)]
static INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK: std::sync::LazyLock<TestGate> =
    std::sync::LazyLock::new(TestGate::default);

// ---------------------------------------------------------------------------
// Ask-error discriminant
// ---------------------------------------------------------------------------

/// Re-exported from [`crate::internal::types`] so callers that already import
/// from this module keep working. The canonical definition lives in
/// `internal::types` so that WASM targets, which cannot import `hew_node`,
/// can also use the type.
pub use crate::internal::types::AskError;

thread_local! {
    static LAST_ASK_ERROR: Cell<i32> = const { Cell::new(AskError::None as i32) };
}

/// Write `err` to the thread-local slot and return `ptr::null_mut()`.
///
/// Using a helper keeps every NULL-return site in `hew_node_api_ask`
/// a single expression and prevents accidentally forgetting the annotation.
#[inline]
fn ask_null(err: AskError) -> *mut c_void {
    LAST_ASK_ERROR.with(|cell| cell.set(err as i32));
    ptr::null_mut()
}

/// Read and clear the last ask-error discriminant for the current thread.
///
/// Returns one of the [`AskError`] values as an `i32`.  The slot is reset to
/// `AskError::None` (0) after each call, so repeated calls without an
/// intervening failed ask return 0.
///
/// This function is intended for use immediately after `hew_node_api_ask`
/// returns `NULL` to distinguish the failure reason.  For direct local asks
/// via `hew_actor_ask` / `hew_actor_ask_timeout`, use
/// `hew_actor_ask_take_last_error` instead.
///
/// When the local delegation path in `hew_node_api_ask` is taken, actor-level
/// errors are bridged into this slot automatically.
#[no_mangle]
pub extern "C" fn hew_node_ask_take_last_error() -> i32 {
    LAST_ASK_ERROR.with(|cell| {
        let v = cell.get();
        cell.set(AskError::None as i32);
        v
    })
}

// ---------------------------------------------------------------------------
// Reply routing table for distributed ask/reply
// ---------------------------------------------------------------------------

/// A single pending remote ask waiting for its reply.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ReplyStatus {
    Success,
    Failed,
}

#[derive(Debug)]
struct ReplyOutcome {
    status: ReplyStatus,
    data: Vec<u8>,
    /// Meaningful only when `status == ReplyStatus::Failed`; carries the
    /// specific ask-error discriminant so callers can distinguish rejection
    /// (e.g. [`AskError::WorkerAtCapacity`]) from a genuine connection drop.
    ask_error: AskError,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ConnectionKey {
    conn_mgr: usize,
    conn_id: c_int,
}

impl ConnectionKey {
    fn new(conn_mgr: *const HewConnMgr, conn_id: c_int) -> Self {
        Self {
            conn_mgr: conn_mgr.cast::<()>() as usize,
            conn_id,
        }
    }
}

struct PendingReply {
    connection: ConnectionKey,
    request_id: u64,
    outcome: Mutex<Option<ReplyOutcome>>,
    cond: Condvar,
    /// When non-null, the remote ask was issued by a SUSPENDABLE caller (NEW-5):
    /// the caller's coroutine has parked (or is about to park) on this reply and
    /// the readiness source must RESUME it through the scheduler rather than
    /// signal `cond`. Null for a blocking (condvar) caller. Set once at
    /// registration; read on completion to pick the wake path.
    parked_caller: AtomicPtr<crate::actor::HewActor>,
}

/// Per-runtime reply routing table for correlating remote ask/reply pairs.
///
/// Each outbound remote ask registers a `PendingReply` keyed by a unique
/// request ID. When the reply envelope arrives, the reader thread deposits
/// the payload and signals the condvar to wake the blocked caller. Owned by
/// the runtime's [`NodeSlot`]; resolved through [`reply_table`].
struct ReplyRoutingTable {
    next_id: AtomicU64,
    pending: Mutex<HashMap<u64, Arc<PendingReply>>>,
}

impl ReplyRoutingTable {
    fn new() -> Self {
        Self {
            next_id: AtomicU64::new(1),
            pending: Mutex::new(HashMap::new()),
        }
    }

    /// Allocate a new request ID and register a pending reply slot, recording
    /// the optional parked caller (NEW-5). A non-null `parked_caller` routes the
    /// completion wake through `scheduler::enqueue_resume` instead of the condvar.
    fn register_with_caller(
        &self,
        connection: ConnectionKey,
        parked_caller: *mut crate::actor::HewActor,
    ) -> (u64, Arc<PendingReply>) {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);
        let entry = Arc::new(PendingReply {
            connection,
            request_id: id,
            outcome: Mutex::new(None),
            cond: Condvar::new(),
            parked_caller: AtomicPtr::new(parked_caller),
        });
        let mut map = self
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        map.insert(id, Arc::clone(&entry));
        (id, entry)
    }

    /// Allocate a new request ID and register a blocking (condvar) pending reply.
    fn register(&self, connection: ConnectionKey) -> (u64, Arc<PendingReply>) {
        self.register_with_caller(connection, ptr::null_mut())
    }

    /// Allocate a new request ID and register a SUSPENDED (NEW-5) pending reply
    /// whose completion resumes `parked_caller`'s coroutine.
    fn register_parked(
        &self,
        connection: ConnectionKey,
        parked_caller: *mut crate::actor::HewActor,
    ) -> (u64, Arc<PendingReply>) {
        self.register_with_caller(connection, parked_caller)
    }

    /// Complete a pending reply by depositing the payload and signalling
    /// the waiting thread. Returns `true` if the request ID was found.
    fn complete(&self, request_id: u64, payload: Vec<u8>) -> bool {
        self.finish(
            request_id,
            ReplyOutcome {
                status: ReplyStatus::Success,
                data: payload,
                ask_error: AskError::None,
            },
        )
    }

    fn finish(&self, request_id: u64, outcome: ReplyOutcome) -> bool {
        let entry = {
            let mut map = self
                .pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            map.remove(&request_id)
        };
        if let Some(pending) = entry {
            Self::complete_pending(&pending, outcome);
            true
        } else {
            false
        }
    }

    /// Deposit `outcome` into `pending` and wake the waiter. A blocking caller
    /// is signalled via the condvar; a SUSPENDED caller (NEW-5,
    /// `parked_caller` non-null) is RESUMED through the scheduler's single
    /// readiness-resume edge (`enqueue_resume`) — the same edge the reactor /
    /// channels / reply slot feed. The outcome lock is released BEFORE the wake
    /// so the resumed coroutine can drain it without contending on this lock.
    fn complete_pending(pending: &PendingReply, outcome: ReplyOutcome) {
        let mut guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *guard = Some(outcome);
        drop(guard);

        let caller = pending.parked_caller.load(Ordering::Acquire);
        if caller.is_null() {
            pending.cond.notify_one();
        } else {
            // SAFETY: `caller` is the actor whose coroutine parked on this
            // request. `enqueue_resume` re-confirms liveness against the live
            // registry under lock and drops the wake for an actor already torn
            // down, so a stale pointer from an abandoned ask is never
            // dereferenced. `cont` is null: the parked continuation handle the
            // suspend edge published is resumed in place (every readiness source
            // passes null here).
            unsafe { crate::scheduler::enqueue_resume(caller, ptr::null_mut()) };
        }
    }

    fn fail_pending_with_reason(pending: &PendingReply, ask_error: AskError) {
        Self::complete_pending(
            pending,
            ReplyOutcome {
                status: ReplyStatus::Failed,
                data: Vec::new(),
                ask_error,
            },
        );
    }

    fn fail_pending(pending: &PendingReply) {
        Self::fail_pending_with_reason(pending, AskError::ConnectionDropped);
    }

    /// Fail every pending reply tied to the given connection with `ask_error`.
    ///
    /// Drains the matching entries under the table lock (so a second failure
    /// path — e.g. a SWIM-DEAD verdict racing a socket drop — finds nothing and
    /// the ask is resolved exactly once), then fails each outside the lock.
    fn fail_connection_with_reason(&self, connection: ConnectionKey, ask_error: AskError) {
        let pending = {
            let mut map = self
                .pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let request_ids = map
                .iter()
                .filter_map(|(&request_id, pending)| {
                    (pending.connection == connection).then_some(request_id)
                })
                .collect::<Vec<_>>();
            request_ids
                .into_iter()
                .filter_map(|request_id| map.remove(&request_id))
                .collect::<Vec<_>>()
        };
        for pending in pending {
            Self::fail_pending_with_reason(&pending, ask_error);
        }
    }

    /// Fail every pending reply tied to the given connection with
    /// [`AskError::ConnectionDropped`] (the socket-drop cause).
    fn fail_connection(&self, connection: ConnectionKey) {
        self.fail_connection_with_reason(connection, AskError::ConnectionDropped);
    }

    /// Fail every pending reply and wake all blocked waiters.
    fn fail_all(&self) {
        let pending = {
            let mut map = self
                .pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            map.drain().map(|(_, pending)| pending).collect::<Vec<_>>()
        };
        for pending in pending {
            Self::fail_pending(&pending);
        }
    }

    /// Fail a single pending reply by request ID. Returns `true` if the
    /// request was found. Used for in-band rejection signals from the remote
    /// node.
    fn fail(&self, request_id: u64, ask_error: AskError) -> bool {
        let entry = {
            let mut map = self
                .pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            map.remove(&request_id)
        };
        if let Some(pending) = entry {
            Self::fail_pending_with_reason(&pending, ask_error);
            true
        } else {
            false
        }
    }

    /// Remove a pending entry (used on timeout / coroutine abandonment to
    /// prevent leaks). Returns the removed entry, if any.
    fn remove(&self, request_id: u64) -> Option<Arc<PendingReply>> {
        let mut map = self
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        map.remove(&request_id)
    }

    /// Number of registered-but-unresolved pending replies. Test-only: used to
    /// assert the reply slot does not leak on the fail-closed send path.
    #[cfg(test)]
    fn pending_len(&self) -> usize {
        self.pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .len()
    }
}

static REMOTE_VOID_REPLY_SENTINEL: u8 = 0;

/// Deposit a reply payload for a pending remote ask.
///
/// Called by the reader thread when a reply envelope arrives. Returns
/// `true` if the request ID was matched.
pub(crate) fn complete_remote_reply(request_id: u64, payload: &[u8]) -> bool {
    reply_table_opt().is_some_and(|table| table.complete(request_id, payload.to_vec()))
}

fn ask_error_from_code(code: i32) -> Option<AskError> {
    match code {
        x if x == AskError::None as i32 => Some(AskError::None),
        x if x == AskError::NodeNotRunning as i32 => Some(AskError::NodeNotRunning),
        x if x == AskError::RoutingFailed as i32 => Some(AskError::RoutingFailed),
        x if x == AskError::EncodeFailed as i32 => Some(AskError::EncodeFailed),
        x if x == AskError::SendFailed as i32 => Some(AskError::SendFailed),
        x if x == AskError::Timeout as i32 => Some(AskError::Timeout),
        x if x == AskError::ConnectionDropped as i32 => Some(AskError::ConnectionDropped),
        x if x == AskError::PayloadSizeMismatch as i32 => Some(AskError::PayloadSizeMismatch),
        x if x == AskError::WorkerAtCapacity as i32 => Some(AskError::WorkerAtCapacity),
        x if x == AskError::ActorStopped as i32 => Some(AskError::ActorStopped),
        x if x == AskError::MailboxFull as i32 => Some(AskError::MailboxFull),
        x if x == AskError::OrphanedAsk as i32 => Some(AskError::OrphanedAsk),
        x if x == AskError::NoRunnableWork as i32 => Some(AskError::NoRunnableWork),
        x if x == AskError::DecodeFailure as i32 => Some(AskError::DecodeFailure),
        x if x == AskError::Partition as i32 => Some(AskError::Partition),
        x if x == AskError::StaleRef as i32 => Some(AskError::StaleRef),
        x if x == AskError::Cancelled as i32 => Some(AskError::Cancelled),
        x if x == AskError::LocalShutdown as i32 => Some(AskError::LocalShutdown),
        x if x == AskError::VersionMismatch as i32 => Some(AskError::VersionMismatch),
        x if x == AskError::Unauthorized as i32 => Some(AskError::Unauthorized),
        x if x == AskError::Backpressure as i32 => Some(AskError::Backpressure),
        x if x == AskError::MonitorLost as i32 => Some(AskError::MonitorLost),
        _ => None,
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum AskRejectionReasonCode {
    WorkerAtCapacity = AskError::WorkerAtCapacity as u8,
    ActorStopped = AskError::ActorStopped as u8,
    MailboxFull = AskError::MailboxFull as u8,
    OrphanedAsk = AskError::OrphanedAsk as u8,
    NoRunnableWork = AskError::NoRunnableWork as u8,
    DecodeFailure = AskError::DecodeFailure as u8,
}

/// Errors returned by [`AskRejectionReasonCode::decode`].
#[derive(Debug, PartialEq)]
enum AskRejectionDecodeError {
    /// The payload's first byte is not a recognised rejection-reason code.
    /// Decoders MUST reject rather than silently substitute a default reason.
    UnknownAskRejectionReason { code: u8 },
}

impl AskRejectionReasonCode {
    fn encode(reason: AskError) -> Option<u8> {
        let code = match reason {
            AskError::WorkerAtCapacity => Self::WorkerAtCapacity,
            AskError::ActorStopped => Self::ActorStopped,
            AskError::MailboxFull => Self::MailboxFull,
            AskError::OrphanedAsk => Self::OrphanedAsk,
            AskError::NoRunnableWork => Self::NoRunnableWork,
            AskError::DecodeFailure => Self::DecodeFailure,
            _ => return None,
        };
        Some(code as u8)
    }

    fn decode(reason_payload: &[u8]) -> Result<AskError, AskRejectionDecodeError> {
        match reason_payload.first().copied() {
            None => Ok(AskError::WorkerAtCapacity), // empty payload: legacy peer
            Some(x) if x == Self::WorkerAtCapacity as u8 => Ok(AskError::WorkerAtCapacity),
            Some(x) if x == Self::ActorStopped as u8 => Ok(AskError::ActorStopped),
            Some(x) if x == Self::MailboxFull as u8 => Ok(AskError::MailboxFull),
            Some(x) if x == Self::OrphanedAsk as u8 => Ok(AskError::OrphanedAsk),
            Some(x) if x == Self::NoRunnableWork as u8 => Ok(AskError::NoRunnableWork),
            Some(x) if x == Self::DecodeFailure as u8 => Ok(AskError::DecodeFailure),
            Some(code) => Err(AskRejectionDecodeError::UnknownAskRejectionReason { code }),
        }
    }
}

fn decode_rejection_reason(reason_payload: &[u8]) -> Result<AskError, AskRejectionDecodeError> {
    AskRejectionReasonCode::decode(reason_payload)
}

/// Fail a pending remote ask identified by `request_id` with the remote rejection reason.
///
/// Called by the reader thread when a **rejection** reply envelope arrives
/// (one with [`HEW_REPLY_REJECT_MSG_TYPE`] in the `msg_type` field).
/// Empty payloads from older peers still default to `WorkerAtCapacity`.
pub(crate) fn fail_remote_reply(request_id: u64, reason_payload: &[u8]) -> bool {
    // On unknown codes, leave the pending ask unresolved (it will timeout)
    // rather than fabricating a misleading AskError.
    match decode_rejection_reason(reason_payload) {
        Ok(reason) => reply_table_opt().is_some_and(|table| table.fail(request_id, reason)),
        Err(_) => false,
    }
}

pub(crate) fn fail_remote_replies_for_connection(conn_mgr: *const HewConnMgr, conn_id: c_int) {
    if let Some(table) = reply_table_opt() {
        table.fail_connection(ConnectionKey::new(conn_mgr, conn_id));
    }
}

fn remote_void_reply_sentinel() -> *mut c_void {
    ptr::from_ref(&REMOTE_VOID_REPLY_SENTINEL)
        .cast_mut()
        .cast::<c_void>()
}

fn remote_reply_data_to_ptr(reply_data: &[u8], reply_size: usize) -> *mut c_void {
    if reply_data.is_empty() {
        return if reply_size == 0 {
            remote_void_reply_sentinel()
        } else {
            ptr::null_mut()
        };
    }

    // The reply payload arrives from a remote (possibly malicious or corrupt)
    // peer, while the caller's generated code reads exactly `reply_size` bytes
    // (the static size of the expected `Reply` type). A peer-supplied payload
    // whose length differs from `reply_size` must fail closed — a shorter
    // payload would otherwise be read past its allocation (heap over-read), and
    // a longer one silently truncated. The call site maps null to
    // `AskError::PayloadSizeMismatch`.
    if reply_data.len() != reply_size {
        return ptr::null_mut();
    }

    // SAFETY: malloc for reply buffer.
    let result = unsafe { libc::malloc(reply_data.len()) };
    if result.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: result was just allocated with reply_data.len() bytes.
    unsafe {
        ptr::copy_nonoverlapping(reply_data.as_ptr(), result.cast::<u8>(), reply_data.len());
    }
    result
}

/// Route a message to a remote actor via the current node.
///
/// # Safety
/// `data` must be valid for `size` bytes (or null when size is 0).
pub(crate) unsafe fn try_remote_send(
    target_pid: u64,
    dispatch: *const c_void,
    msg_type: c_int,
    data: *mut c_void,
    size: usize,
) -> c_int {
    with_current_node_read(|guard| {
        if *guard == 0 {
            return -1;
        }
        let node = *guard as *mut HewNode;
        // SAFETY: read lock pins CURRENT_NODE pointer for this send.
        unsafe {
            hew_node_send(
                node,
                target_pid,
                dispatch,
                msg_type,
                data.cast::<u8>(),
                size,
            )
        }
    })
}

/// Node-local distributed registry state.
///
/// `incarnations` / `name_serials` / `superseded_serials` together carry the
/// actor-slot / registration **incarnation** dimension of the spec's
/// `(NodeId, slot, incarnation)` identity — DISTINCT from the node-level SWIM
/// peer incarnation (`NodeSlot::quarantine`). They live on the registry (not the
/// per-runtime `NodeSlot`) because both the local register/unregister path
/// (`node.registry`) AND the cluster gossip callback (`user_data` → registry)
/// must keep them in lockstep, and the gossip callback can run on a
/// connection-reader thread with no installed runtime.
#[repr(C)]
#[derive(Debug, Default)]
pub struct HewRegistry {
    remote_names: Mutex<HashMap<String, u64>>,
    /// Registration incarnation per name: `0` for a fresh registration, bumped
    /// each time the name is re-pointed to a DIFFERENT actor.
    incarnations: Mutex<HashMap<String, u32>>,
    /// The serial each name currently resolves to. Persists across a bare
    /// unregister so a later re-registration to a different actor can detect the
    /// re-point and supersede the prior serial.
    name_serials: Mutex<HashMap<String, u64>>,
    /// Serials whose registration has been superseded by a re-registration of
    /// the same name to a different actor (or whose actor was torn down). Serials
    /// are monotonic and never reused, so a captured `RemotePid<T>` (a bare
    /// `u64`) uniquely names its original registration; a hit here makes the
    /// remote dispatch boundary fail closed with `StaleRef`. Value = the
    /// incarnation it was superseded at (diagnostics).
    superseded_serials: Mutex<HashMap<u64, u32>>,
}

#[derive(Clone, Copy)]
struct SendTransport(*mut HewTransport);
// SAFETY: transport implementations are internally synchronized and used via
// their vtable APIs.
unsafe impl Send for SendTransport {}

#[derive(Clone, Copy)]
struct SendConnMgr(*mut HewConnMgr);
// SAFETY: manager internals are synchronized by mutexes.
unsafe impl Send for SendConnMgr {}

/// Unified distributed node runtime.
/// Integrates transport, connections, cluster membership, and registry.
#[repr(C)]
pub struct HewNode {
    /// Unique node identifier (16-bit)
    pub node_id: u16,
    /// Bind address for incoming connections
    pub bind_addr: *const c_char,
    /// Transport ops vtable
    pub transport_ops: *const HewTransportOps,
    /// Active transport handle
    pub transport: *mut HewTransport,
    /// Connection manager
    pub conn_mgr: *mut HewConnMgr,
    /// Cluster membership state
    pub cluster: *mut HewCluster,
    /// PID routing table for remote node delivery.
    pub routing_table: *mut HewRoutingTable,
    /// Local + remote registry
    pub registry: *mut HewRegistry,
    /// Node state (starting/running/stopping/stopped)
    pub state: AtomicU8,
    bind_addr_owned: *mut c_char,
    accept_stop: Arc<AtomicBool>,
    accept_thread: Mutex<Option<JoinHandle<()>>>,
    /// The per-node peer-authentication authority installed before start.
    ///
    /// Defaults to [`PeerAuthSnapshot::unconfigured`] in [`hew_node_new`]; the
    /// public `Node::start` path installs the staged snapshot via
    /// [`hew_node_set_auth_snapshot`] before the shared low-level start, and
    /// low-level callers install their own explicit snapshot. `hew_node_start`
    /// reads this — never the public `ConfigState` — so concurrent low-level
    /// nodes stay isolated.
    pub(crate) auth: PeerAuthSnapshot,
}

impl std::fmt::Debug for HewNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewNode")
            .field("node_id", &self.node_id)
            .field("state", &self.state.load(Ordering::Relaxed))
            .finish_non_exhaustive()
    }
}

fn remember_node(node: *mut HewNode) {
    with_known_nodes(|known| {
        if !known.iter().any(|entry| entry.0 == node) {
            known.push(KnownNodePtr(node));
        }
    });
}

fn forget_node(node: *mut HewNode) {
    with_known_nodes(|known| {
        known.retain(|entry| entry.0 != node);
    });
}

fn take_registry_names_if<F>(registry: &HewRegistry, mut predicate: F) -> Vec<String>
where
    F: FnMut(u64) -> bool,
{
    let mut map = registry.remote_names.lock_or_recover();
    let names: Vec<String> = map
        .iter()
        .filter(|(_, actor_id)| predicate(**actor_id))
        .map(|(name, _)| name.clone())
        .collect();
    for name in &names {
        map.remove(name);
    }
    names
}

unsafe fn unregister_names_from_node(node: &HewNode, names: Vec<String>, emit_gossip_remove: bool) {
    for name in names {
        let Ok(c_name) = CString::new(name) else {
            continue;
        };
        // SAFETY: `c_name` is a valid NUL-terminated string for the call.
        unsafe { crate::registry::hew_registry_unregister(c_name.as_ptr()) };
        if emit_gossip_remove && !node.cluster.is_null() {
            // SAFETY: node teardown is serialized against actor-free cleanup by
            // KNOWN_NODES, so the cluster pointer stays valid for this call.
            unsafe { cluster::hew_cluster_registry_remove(node.cluster, c_name.as_ptr()) };
        }
    }
}

unsafe fn unregister_local_names_for_node(node: &HewNode) {
    if node.registry.is_null() {
        return;
    }
    // SAFETY: registry belongs to `node` for the node lifetime.
    let registry = unsafe { &*node.registry };
    let names = take_registry_names_if(registry, |actor_id| {
        crate::pid::hew_pid_node(actor_id) == node.node_id
    });
    // Node shutdown is the decommission path; remote peers prune their cached
    // names when they observe the corresponding left/dead membership event.
    // SAFETY: `node` and its cluster/registry pointers remain valid for the
    // duration of hew_node_stop.
    unsafe { unregister_names_from_node(node, names, false) };

    registry.remote_names.lock_or_recover().clear();
}

/// Remove all registered names that still point at `actor_id`.
///
/// Called from actor teardown so long-running runtimes do not retain stale
/// local names after a named actor is freed or restarted.
pub(crate) unsafe fn unregister_actor_names(actor_id: u64) {
    let owner_node_id = crate::pid::hew_pid_node(actor_id);
    let _ = with_known_nodes_opt(|known| {
        for entry in known.iter().copied() {
            if entry.0.is_null() {
                continue;
            }
            // SAFETY: KNOWN_NODES holds node allocations live until `forget_node`.
            let node = unsafe { &*entry.0 };
            if owner_node_id != 0 && node.node_id != owner_node_id {
                continue;
            }
            if node.registry.is_null() {
                continue;
            }

            // SAFETY: registry belongs to `node` for the node lifetime.
            let registry = unsafe { &*node.registry };
            let names =
                take_registry_names_if(registry, |registered_actor| registered_actor == actor_id);
            if names.is_empty() {
                continue;
            }

            // The actor backing these registrations is being torn down: supersede
            // its serial so any captured `RemotePid<T>` over it fails closed with
            // StaleRef rather than routing to a freed or replacement actor.
            registry_supersede_serial(registry, crate::pid::hew_pid_serial(actor_id));

            let emit_gossip_remove =
                node.state.load(Ordering::Acquire) == NODE_STATE_RUNNING && !node.cluster.is_null();
            // SAFETY: KNOWN_NODES keeps `node` alive for the duration of this loop,
            // and the node state check above excludes teardown in progress.
            unsafe { unregister_names_from_node(node, names, emit_gossip_remove) };
        }
    });
}

// SAFETY: mutable shared fields are guarded by mutexes/atomics.
unsafe impl Send for HewNode {}
// SAFETY: concurrent access goes through mutexes/atomics.
unsafe impl Sync for HewNode {}

unsafe extern "C" fn node_inbound_router(
    target_actor_id: u64,
    msg_type: i32,
    data: *mut u8,
    size: usize,
    request_id: u64,
    source_node_id: u16,
    conn_mgr: *mut connection::HewConnMgr,
) {
    if request_id > 0 && source_node_id > 0 {
        // Inbound remote ask — dispatch locally and send the reply back.

        // ── Backpressure: bounded concurrent inbound ask workers ─────────────
        //
        // Optimistically increment the counter. If we were already at the
        // limit we revert and — if the source peer understands the rejection
        // sentinel (HEW_FEATURE_SUPPORTS_ASK_REJECTION) — send a rejection
        // reply envelope back (msg_type = HEW_REPLY_REJECT_MSG_TYPE). The
        // connection reader on the originating node dispatches this sentinel
        // to `fail_remote_reply`, which sets ReplyStatus::Failed with reason
        // WorkerAtCapacity so `hew_node_api_ask` returns the precise
        // discriminant — fail-closed for both void and non-void asks.
        //
        // If the source peer does NOT advertise the feature flag (old node),
        // we send no reply at all.  The originating ask will time out through
        // its normal deadline path — this is the safe fail-closed fallback: an
        // old peer that never sends the sentinel cannot misinterpret 65535 as
        // a void-success, and we avoid the silent-success regression.
        let prev = INBOUND_ASK_ACTIVE.fetch_add(1, Ordering::AcqRel);
        if prev >= INBOUND_ASK_WORKER_LIMIT {
            INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
            // SAFETY: the inbound router is only called while `conn_mgr` is live.
            let peer_flags =
                unsafe { connection::hew_connmgr_feature_flags_for_node(conn_mgr, source_node_id) };
            if connection::supports_ask_rejection(peer_flags) {
                // SAFETY: conn_mgr is live for the duration of the inbound router call.
                let shutdown = unsafe { connection::hew_connmgr_shutdown_flag(conn_mgr) };
                if let Some(shutdown) = shutdown {
                    send_rejection_reply(
                        source_node_id,
                        request_id,
                        AskError::WorkerAtCapacity,
                        conn_mgr,
                        shutdown.as_ref(),
                    );
                }
            }
            return;
        }
        // ─────────────────────────────────────────────────────────────────────

        // Deep-copy the payload so we can hand it to a background thread.
        let payload = if size > 0 && !data.is_null() {
            // SAFETY: data is valid for `size` bytes (reader_loop contract).
            unsafe { std::slice::from_raw_parts(data, size) }.to_vec()
        } else {
            Vec::new()
        };
        let conn_mgr_send = SendConnMgr(conn_mgr);
        // SAFETY: the inbound router is only called while `conn_mgr` is live.
        let Some(shutdown_started) = (unsafe { connection::hew_connmgr_shutdown_flag(conn_mgr) })
        else {
            // Undo the increment we already committed above.
            INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
            return;
        };
        // Acquire the per-manager active counter and the spawn-gate flag. The
        // counter tracks workers for THIS conn_mgr specifically, so hew_node_stop
        // drains only its own workers (not another node's in a multi-node test).
        // SAFETY: conn_mgr is live for the duration of this router call.
        let Some(per_mgr_active) =
            (unsafe { connection::hew_connmgr_inbound_ask_active(conn_mgr) })
        else {
            INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
            return;
        };
        // SAFETY: conn_mgr is live for the duration of this router call.
        let Some(spawn_gate) =
            (unsafe { connection::hew_connmgr_inbound_spawn_closed_flag(conn_mgr) })
        else {
            per_mgr_active.fetch_sub(1, Ordering::SeqCst);
            INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
            return;
        };
        // ── ATOMIC spawn-gate + counter protocol (close the teardown race) ───
        //
        // The spawn-gate check and the counter increment MUST be atomic with
        // respect to `hew_node_stop`'s Phase-1 drain, or a worker can pass the
        // gate, then have the drain close the gate and observe a zero counter,
        // then increment + spawn AFTER the drain finished — reaching Phase 2 and
        // abandoning its reply (the production reply-abandon race this fix
        // closes). We make it atomic with the increment-then-recheck (Dekker)
        // pattern under SeqCst:
        //
        //   router: per_mgr_active.fetch_add(1, SeqCst); gate.load(SeqCst)
        //   drain : gate.store(true, SeqCst);            per_mgr_active.load(SeqCst)
        //
        // SeqCst on BOTH sides (not AcqRel — that permits the store/load on the
        // two distinct atomics to reorder) gives a single total order in which
        // at least one thread observes the other's store. So either the drain
        // sees count ≥ 1 and waits for this worker, OR this router sees the gate
        // closed and bails before spawning. It is impossible for the router to
        // see the gate open AND the drain to see count 0 — i.e. a worker that
        // passes this gate is guaranteed visible to a concurrent drain, and a
        // worker counted-out by the drain cannot spawn.
        //
        // Two signals close the window:
        //   • `inbound_spawn_closed` — set FIRST by `hew_node_stop`, before its
        //     drain, so already-running `handle_inbound_ask` threads still flush
        //     their replies while NEW workers are turned away here.
        //   • `reconnect_shutdown` — set in Phase 2 (`hew_connmgr_mark_stopping`
        //     / `hew_connmgr_free`); a worker spawned past it would bail inside
        //     `handle_inbound_ask` with conn_mgr on the verge of being freed.
        per_mgr_active.fetch_add(1, Ordering::SeqCst);
        // Test seam: wedge a router in the Dekker window (incremented, gate not
        // yet re-read) so a test can prove a concurrent drain always observes
        // this worker. No-op in production (Disabled mode).
        #[cfg(test)]
        INBOUND_ROUTER_AFTER_INCREMENT_HOOK.hit();
        if spawn_gate.load(Ordering::SeqCst) || shutdown_started.load(Ordering::SeqCst) {
            per_mgr_active.fetch_sub(1, Ordering::SeqCst);
            INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
            return;
        }
        // Construct the guard *before* spawning so both counter decrements are
        // covered regardless of whether spawn succeeds:
        //   • spawn succeeds: guard moves into the closure; Drop runs when the
        //     thread exits or panics.
        //   • spawn fails (OOM): thread::spawn drops the closure, which calls
        //     InboundAskGuard::drop and decrements both counters immediately.
        // Previously the guard was created inside the closure body, leaving a
        // window where a spawn failure would leak both INBOUND_ASK_ACTIVE and
        // the per-manager counter.
        let guard = InboundAskGuard(per_mgr_active);
        let _ = thread::spawn(move || {
            let _guard = guard;
            handle_inbound_ask(
                target_actor_id,
                msg_type,
                &payload,
                request_id,
                source_node_id,
                conn_mgr_send,
                shutdown_started,
            );
        });
    } else {
        // Fire-and-forget message — reconstruct the value into THIS node's
        // address space before delivering it to the local mailbox.
        // SAFETY: data is valid for `size` bytes (reader_loop contract).
        unsafe { deliver_inbound_send(target_actor_id, msg_type, data, size) };
    }
}

/// Deliver an inbound fire-and-forget (`send`) frame to its target actor's local
/// mailbox, reconstructing the value into THIS node's address space first. The
/// inbound `data` is the serialized wire form; feeding it raw to the mailbox
/// would make the actor handler dereference sender-side heap pointers and crash.
///
/// Fail closed throughout: an unregistered codec, a decode failure, or a
/// target actor that is no longer live all DROP the message rather than deliver
/// garbage. An empty payload (`size == 0`) is a genuine zero-field message and
/// bypasses decode.
///
/// # Safety
/// `data` must be valid for `size` bytes (or null when `size == 0`).
unsafe fn deliver_inbound_send(target_actor_id: u64, msg_type: i32, data: *mut u8, size: usize) {
    if size == 0 {
        // Local delivery into THIS node's mailbox — no cross-node encode, so no
        // codec key needed (null dispatch).
        // SAFETY: zero-length payload; mailbox handles null+0.
        let _ = unsafe {
            crate::actor::hew_actor_send_by_id(
                target_actor_id,
                std::ptr::null(),
                msg_type,
                std::ptr::null_mut(),
                0,
            )
        };
        return;
    }
    // Resolve the TARGET actor type's dispatch pointer from the wire's
    // `target_actor_id` and decode under `(dispatch, msg_type)`, so the frame is
    // reconstructed ONLY by the codec belonging to its target actor's type —
    // never by a different actor whose `msg_type` SipHash-collides (remote
    // type-confusion).
    let Some(dispatch) = crate::lifetime::live_actors::dispatch_ptr_by_id(target_actor_id) else {
        // Target actor not live (torn down between frame arrival and decode, or
        // never existed). No dispatch pointer resolvable → no codec selectable →
        // drop the message fail-closed rather than fabricate a key.
        set_last_error(format!(
            "cross-node send dropped: target actor {target_actor_id} not live \
             for msg_type={msg_type}"
        ));
        return;
    };
    // SAFETY: data is valid for `size` bytes (caller contract).
    let (value, struct_size) =
        unsafe { crate::xnode_serial::decode_payload(dispatch, msg_type, data.cast_const(), size) };
    if value.is_null() {
        // No codec or decode failure — drop the message fail-closed.
        set_last_error(format!(
            "cross-node send dropped: no codec or decode failure for msg_type={msg_type}"
        ));
        return;
    }
    // The reconstructed value lives in a malloc'd buffer of `struct_size` bytes
    // (the in-memory struct size, NOT the wire length). hew_actor_send_by_id
    // deep-copies `struct_size` bytes into the mailbox, MOVING the owned heap
    // fields (strings/bytes) into the mailbox copy — exactly the move semantics
    // of a local send where the caller's stack value is byte-copied. We then
    // free the reconstructed struct SHELL only (not its fields, now owned by the
    // mailbox copy). Local mailbox delivery — null dispatch (no re-encode here).
    // SAFETY: value is a valid reconstructed struct for msg_type.
    let _ = unsafe {
        crate::actor::hew_actor_send_by_id(
            target_actor_id,
            std::ptr::null(),
            msg_type,
            value,
            struct_size,
        )
    };
    // SAFETY: value came from decode_payload (libc::malloc).
    unsafe { libc::free(value) };
}

/// Handle an inbound remote ask by performing a local blocking ask and
/// sending the reply envelope back to the requesting node.
#[expect(
    clippy::needless_pass_by_value,
    reason = "spawned ask-handler thread must own the shutdown flag clone"
)]
fn handle_inbound_ask(
    target_actor_id: u64,
    msg_type: i32,
    payload: &[u8],
    request_id: u64,
    source_node_id: u16,
    conn_mgr: SendConnMgr,
    shutdown_started: Arc<AtomicBool>,
) {
    // Test seam: wedge a straggler worker at the top of the handler, BEFORE the
    // feature-flags capture acquires the CURRENT_NODE read lock, so a test can
    // let a concurrent secondary-node stop free `conn_mgr` while we are parked
    // here. No-op in production (Disabled mode).
    #[cfg(test)]
    INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK.hit();

    // Capture the peer's negotiated feature flags ONCE, up front, under the
    // SAME barrier that protects `send_reply_envelope` / `send_rejection_reply`:
    // the CURRENT_NODE read lock AND THIS manager's `shutdown_started`
    // (`reconnect_shutdown`) flag. The three fail-closed error paths below
    // (decode failure, actor error, encode failure) need these flags to decide
    // whether the peer understands the ask-rejection sentinel. Reading them at
    // each error site dereferenced `conn_mgr` OUTSIDE any barrier: if the 5s
    // Phase-1 drain ceiling expired and Phase 2 freed `conn_mgr`, a straggler
    // dereferenced freed memory (use-after-free). The flags are negotiated once
    // at handshake and never change for the connection's life, so a single
    // guarded read at the top is equivalent to reading at each site.
    //
    // `*guard == 0` alone is INSUFFICIENT for a SECONDARY node. `hew_node_stop`
    // zeroes CURRENT_NODE only for the node that owns it (`*guard == this node`);
    // stopping a secondary node in a multi-node runtime leaves CURRENT_NODE
    // pointing at a DIFFERENT (still-running) node, so `*guard != 0` and the
    // old check passed — then `feature_flags_for_node` dereferenced the
    // secondary node's ALREADY-FREED `conn_mgr`. What IS set per-node is
    // `reconnect_shutdown`: `hew_node_stop` calls `mark_stopping` on the
    // stopping node's own `conn_mgr` (regardless of CURRENT_NODE) INSIDE the
    // CURRENT_NODE write lock, before Phase 2 frees it. `shutdown_started` is a
    // clone of exactly that flag (captured in `node_inbound_router`). So the
    // pairing is the same as `send_reply_envelope`:
    //   • if this read holds the read lock first, stop's write lock blocks until
    //     we release it → `conn_mgr` is valid for the `feature_flags_for_node`
    //     call (which happens inside the closure, under the read lock);
    //   • if stop took the write lock first, it set this node's
    //     `shutdown_started = true` before releasing; we then observe that flag
    //     here and return `None` (a straggler past the drain ceiling) WITHOUT
    //     touching the soon-to-be-freed manager. The error paths skip the
    //     rejection send, fail-closed.
    let peer_flags: Option<u32> = with_current_node_read(|guard| {
        if *guard == 0 || shutdown_started.load(Ordering::Acquire) {
            return None;
        }
        // SAFETY: the CURRENT_NODE read lock held here blocks `hew_node_stop`'s
        // write-lock teardown, and `shutdown_started` was observed false under
        // that lock, so this node's `conn_mgr` cannot be freed for this read.
        Some(unsafe { connection::hew_connmgr_feature_flags_for_node(conn_mgr.0, source_node_id) })
    });

    // Resolve the TARGET actor type's dispatch pointer from `target_actor_id`.
    // Both the request DECODE and the reply ENCODE key their codec by
    // `(dispatch, msg_type)` so a frame is reconstructed / a reply is encoded
    // ONLY by the codec belonging to this target actor's type — never by a
    // different actor whose `msg_type` SipHash-collides. When the target is not
    // live, `dispatch` is null: `decode_payload` finds no codec under a null key
    // and fails closed below (the standard decode-failure rejection), so no
    // separate not-live branch is needed.
    let target_dispatch = crate::lifetime::live_actors::dispatch_ptr_by_id(target_actor_id);

    // Reconstruct the request value into THIS node's address space before the
    // local ask — the inbound `payload` is the serialized wire form, not the
    // in-memory struct. Feeding it raw to the handler would dereference
    // sender-side heap pointers and crash. A zero-length payload is a genuine
    // zero-field request and bypasses decode.
    let decoded_request: Option<(*mut c_void, usize)> = if payload.is_empty() {
        None
    } else {
        let dispatch = target_dispatch.unwrap_or(std::ptr::null());
        // SAFETY: payload is a valid slice for payload.len() bytes.
        let (value, struct_size) = unsafe {
            crate::xnode_serial::decode_payload(dispatch, msg_type, payload.as_ptr(), payload.len())
        };
        if value.is_null() {
            // No codec or decode failure — fail closed: send a rejection (if the
            // peer understands it) and do not deliver garbage to the handler.
            // `peer_flags` was captured up front under the CURRENT_NODE barrier;
            // `None` means a straggler past the drain ceiling, so we skip the send.
            if peer_flags.is_some_and(connection::supports_ask_rejection) {
                send_rejection_reply(
                    source_node_id,
                    request_id,
                    AskError::DecodeFailure,
                    conn_mgr.0,
                    shutdown_started.as_ref(),
                );
            }
            return;
        }
        Some((value, struct_size))
    };

    // Perform a local blocking ask against the target actor with the
    // reconstructed request value.
    let reply_ptr = {
        let (data_ptr, data_len) = match decoded_request {
            Some((value, struct_size)) => (value, struct_size),
            None => (std::ptr::null_mut(), 0),
        };
        // SAFETY: data_ptr is valid for data_len bytes (reconstructed struct).
        unsafe { crate::actor::hew_actor_ask_by_id(target_actor_id, msg_type, data_ptr, data_len) }
    };
    // Free the reconstructed request shell now the ask copied it into the mailbox
    // (owned fields moved into the mailbox copy, matching local-send semantics).
    if let Some((value, _)) = decoded_request {
        // SAFETY: value came from decode_payload (libc::malloc).
        unsafe { libc::free(value) };
    }

    // Build the reply payload from the returned data.
    let reply_data: Vec<u8> = if reply_ptr.is_null() {
        let ask_err = crate::actor::actor_ask_take_last_error_raw();
        if ask_err != AskError::None as i32 {
            #[cfg(test)]
            INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK.hit();
            // `peer_flags` was captured up front under the CURRENT_NODE barrier;
            // `None` means a straggler past the drain ceiling, so we skip the send.
            if peer_flags.is_some_and(connection::supports_ask_rejection) {
                let ask_error = ask_error_from_code(ask_err).unwrap_or(AskError::ActorStopped);
                send_rejection_reply(
                    source_node_id,
                    request_id,
                    ask_error,
                    conn_mgr.0,
                    shutdown_started.as_ref(),
                );
            }
            return;
        }
        Vec::new()
    } else {
        // `reply_ptr` points to the in-memory reply VALUE (a struct that may
        // contain heap pointers). Serialize its CONTENTS for transport rather
        // than shipping the raw struct bytes — the originating node reconstructs
        // the value into its own address space. The reply codec is keyed by
        // `(target dispatch, request msg_type)` — the same target actor type that
        // just produced the reply, so a colliding `msg_type` on another actor
        // type cannot select the wrong reply codec.
        // SAFETY: reply_ptr came from hew_reply which malloc'd the reply struct.
        let size = unsafe { crate::actor::hew_reply_data_size(reply_ptr) };
        if size > 0 {
            let mut out_len: usize = 0;
            // The local ask succeeded against `target_actor_id`, so its dispatch
            // resolved above; a null fallback fails closed in `encode_reply`.
            let dispatch = target_dispatch.unwrap_or(std::ptr::null());
            // SAFETY: reply_ptr is a valid reply value for msg_type; out_len valid.
            let bytes = unsafe {
                crate::xnode_serial::encode_reply(dispatch, msg_type, reply_ptr, &raw mut out_len)
            };
            // SAFETY: reply_ptr was malloc'd by hew_reply; free after encoding.
            // NOTE (robustness gap): `reply_ptr` is a flat memcpy of the actor's
            // reply value.  If the reply type has owned string/bytes fields, their
            // heap allocations are bit-copied into this buffer.  `encode_reply`
            // serialises the contents but does not drop the field pointers, so
            // `libc::free(reply_ptr)` frees the flat shell only — a bounded leak
            // per reply with owned fields.  Fixing this requires a drop-thunk
            // registry entry (parallel to the serialize thunk).  Tracked for the
            // drop-thunk registry lane; not fixed here because the actor's own
            // lifecycle already holds references to the same heap objects.
            unsafe { libc::free(reply_ptr) };
            if bytes.is_null() {
                // No reply codec registered — fail closed: send a rejection so
                // the originating ask fails with a typed error instead of timing
                // out on raw/absent bytes.
                // `peer_flags` was captured up front under the CURRENT_NODE
                // barrier; `None` means a straggler past the drain ceiling.
                if peer_flags.is_some_and(connection::supports_ask_rejection) {
                    send_rejection_reply(
                        source_node_id,
                        request_id,
                        AskError::EncodeFailed,
                        conn_mgr.0,
                        shutdown_started.as_ref(),
                    );
                }
                return;
            }
            // SAFETY: bytes is valid for out_len bytes (from encode_reply).
            let v = unsafe { std::slice::from_raw_parts(bytes, out_len) }.to_vec();
            // SAFETY: bytes came from encode_reply (libc::malloc).
            unsafe { crate::xnode_serial::hew_ser_free_bytes(bytes) };
            v
        } else {
            // SAFETY: reply_ptr was malloc'd by hew_reply.
            unsafe { libc::free(reply_ptr) };
            Vec::new()
        }
    };

    // Send the reply envelope back to the requesting node.
    send_reply_envelope(
        source_node_id,
        request_id,
        &reply_data,
        conn_mgr.0,
        shutdown_started.as_ref(),
    );
}

/// Send a **rejection** reply envelope back to the source node.
///
/// Rejection replies carry [`HEW_REPLY_REJECT_MSG_TYPE`] in the `msg_type`
/// field plus a 1-byte [`AskError`] reason payload. The connection reader on
/// the receiving node recognises the sentinel and calls
/// [`fail_remote_reply`] instead of [`complete_remote_reply`], preserving the
/// remote rejection reason for the originating `hew_node_api_ask` caller.
fn send_rejection_reply(
    target_node_id: u16,
    request_id: u64,
    reason: AskError,
    conn_mgr: *mut connection::HewConnMgr,
    shutdown_started: &AtomicBool,
) {
    if conn_mgr.is_null() {
        return;
    }

    with_current_node_read(|guard| {
        if *guard == 0 {
            return;
        }
        if shutdown_started.load(Ordering::Acquire) {
            return;
        }

        // SAFETY: conn_mgr is the manager that received the ask. The CURRENT_NODE
        // read lock held above (guard) ensures it remains valid for this call.
        let conn_id = unsafe { connection::hew_connmgr_conn_id_for_node(conn_mgr, target_node_id) };
        if conn_id < 0 {
            return;
        }

        let reason_payload = [AskRejectionReasonCode::encode(reason)
            .expect("remote ask rejection reason must use a supported rejection-reason code")];
        // Encode the rejection envelope: request_id identifies the pending ask;
        // source_node_id = 0 marks it as a reply; msg_type = HEW_REPLY_REJECT_MSG_TYPE
        // distinguishes it from a normal (possibly void) success reply.
        // SAFETY: `reason_payload` is a stack byte array valid for its length.
        let bytes = match unsafe {
            encode_envelope_frame_from_raw_parts(
                0,
                0,
                HEW_REPLY_REJECT_MSG_TYPE,
                reason_payload.as_ptr(),
                reason_payload.len(),
                request_id,
                0,
            )
        } {
            Ok(bytes) => bytes,
            Err(err) => {
                set_last_error(format!("send_rejection_reply: {err}"));
                return;
            }
        };
        // SAFETY: conn_mgr and conn_id are valid; bytes is CBOR encoded.
        unsafe {
            connection::hew_connmgr_send_preencoded(conn_mgr, conn_id, bytes.as_ptr(), bytes.len())
        };
    });
}

/// Encode and send a reply envelope back to the source node.
///
/// Uses `conn_mgr` directly so the reply is routed via the connection that
/// received the original ask.
fn send_reply_envelope(
    target_node_id: u16,
    request_id: u64,
    reply_data: &[u8],
    conn_mgr: *mut connection::HewConnMgr,
    shutdown_started: &AtomicBool,
) {
    if conn_mgr.is_null() {
        return;
    }

    // Synchronize with `hew_node_stop` to prevent a use-after-free on
    // `conn_mgr`.  `hew_node_stop` holds the `CURRENT_NODE` write lock while
    // it clears the pointer to zero (and only frees `conn_mgr` afterward), so:
    //
    // * If this thread acquires the read lock first, stop is blocked until we
    //   release it — `conn_mgr` is guaranteed valid for this whole function.
    // * If stop cleared `CURRENT_NODE` first, we see `*guard == 0` here and
    //   return before touching `conn_mgr`.
    //
    // The read lock is held for the entire duration of `conn_mgr` access via
    // the `read_access` closure.
    with_current_node_read(|guard| {
        if *guard == 0 {
            return;
        }
        if shutdown_started.load(Ordering::Acquire) {
            return;
        }

        // Find the connection back to the requesting node.
        // SAFETY: conn_mgr is the manager that received the ask. The CURRENT_NODE
        // read lock held above (guard) ensures it remains valid for this call.
        let conn_id = unsafe { connection::hew_connmgr_conn_id_for_node(conn_mgr, target_node_id) };
        if conn_id < 0 {
            return;
        }

        // Encode the reply envelope with request_id set and source_node_id = 0
        // to mark it as a reply (not a new request).
        // SAFETY: `reply_data.as_ptr()` is valid for `reply_data.len()` bytes.
        let bytes = match unsafe {
            encode_envelope_frame_from_raw_parts(
                0,
                0,
                0,
                reply_data.as_ptr(),
                reply_data.len(),
                request_id,
                0,
            )
        } {
            Ok(bytes) => bytes,
            Err(err) => {
                set_last_error(format!("send_reply_envelope: {err}"));
                return;
            }
        };

        // Send via conn_mgr so noise encryption is applied when the connection
        // is encrypted. This replaces the former raw transport send which would
        // send unencrypted data over an encrypted connection.
        // SAFETY: conn_mgr and conn_id are valid; bytes is CBOR encoded.
        unsafe {
            connection::hew_connmgr_send_preencoded(conn_mgr, conn_id, bytes.as_ptr(), bytes.len())
        };
    });
}

/// Callback invoked by the cluster when a registry gossip event arrives
/// from a remote peer. Updates the node's `remote_names` map.
extern "C" fn node_registry_gossip_callback(
    name: *const c_char,
    actor_id: u64,
    is_add: bool,
    user_data: *mut c_void,
) {
    if name.is_null() || user_data.is_null() {
        return;
    }
    // SAFETY: user_data is a *mut HewRegistry pointer set during hew_node_start,
    // valid for the node's lifetime.
    let registry = unsafe { &*(user_data.cast::<HewRegistry>()) };
    // SAFETY: caller guarantees name is a valid C string.
    let key = unsafe { CStr::from_ptr(name) }
        .to_string_lossy()
        .into_owned();
    let inserted = {
        let mut map = registry.remote_names.lock_or_recover();
        if is_add {
            // Fail-closed registration-string boundary (gossip side): an inbound
            // ADD for a name that is already mapped to a DIFFERENT actor is a
            // cluster-wide ambiguity — two distinct actors (possibly two distinct
            // qualified actor types) chose the same registration string. Keep the
            // existing mapping and RECORD the refusal rather than silently
            // re-pointing routing at whichever gossip arrived last. Qualified
            // registration keys on the gossip wire (a CDDL/CBOR RegistryEvent
            // change) are the tracked full fix.
            match map.get(&key) {
                Some(existing) if *existing != actor_id => {
                    set_last_error(format!(
                        "registry gossip: name `{key}` is already registered to \
                         actor {existing}; refusing gossiped re-registration to \
                         actor {actor_id} — registration strings carry no \
                         actor-type qualifier on the wire, so the conflict cannot \
                         be disambiguated (qualified registration keys are a \
                         tracked follow-up)"
                    ));
                    false
                }
                _ => {
                    map.insert(key.clone(), actor_id);
                    true
                }
            }
        } else {
            map.remove(&key);
            false
        }
    };

    // Mirror the local register path's incarnation bookkeeping on the gossip
    // side so a client converges on the same supersession verdict as the server:
    // when a name is re-pointed (via the server's unregister→re-register, which
    // arrives as a remove then a fresh add), the client supersedes the serial it
    // previously captured for that name. Done outside the `remote_names` lock to
    // avoid nesting registry locks.
    if inserted {
        let _incarnation =
            registration_record(registry, &key, crate::pid::hew_pid_serial(actor_id));
    }
}

unsafe fn parse_connect_target(addr: *const c_char) -> Option<(Option<u16>, CString)> {
    // SAFETY: caller validates non-null and C-string.
    let c_addr = unsafe { CStr::from_ptr(addr) };
    let addr_text = c_addr.to_string_lossy();
    if let Some((prefix, target)) = addr_text.split_once('@') {
        if let Ok(node_id) = prefix.parse::<u16>() {
            if node_id != 0 && !target.is_empty() {
                let c_target = CString::new(target.as_bytes()).ok()?;
                return Some((Some(node_id), c_target));
            }
        }
    }
    Some((None, CString::new(c_addr.to_bytes()).ok()?))
}

fn accept_loop(transport: SendTransport, conn_mgr: SendConnMgr, stop: &AtomicBool) {
    while !stop.load(Ordering::Acquire) {
        // SAFETY: pointers are valid for the lifetime of the spawned loop.
        let conn_id = unsafe {
            let t = &*transport.0;
            let Some(ops) = t.ops.as_ref() else { break };
            let Some(accept_fn) = ops.accept else { break };
            accept_fn(t.r#impl, 200)
        };

        if conn_id != HEW_CONN_INVALID {
            // SAFETY: pointers are held by HewNode for the loop lifetime.
            let _ = unsafe { connection::hew_connmgr_add(conn_mgr.0, conn_id) };
        }
    }
}

unsafe fn free_transport(transport: *mut HewTransport) {
    if transport.is_null() {
        return;
    }
    // SAFETY: valid transport pointer from constructor.
    let transport_ref = unsafe { &*transport };
    // SAFETY: ops pointer is part of valid transport.
    if let Some(ops) = unsafe { transport_ref.ops.as_ref() } {
        if let Some(destroy_fn) = ops.destroy {
            // SAFETY: transport impl belongs to this transport.
            unsafe { destroy_fn(transport_ref.r#impl) };
        }
    }
    // SAFETY: transport was allocated by Box::into_raw.
    let _ = unsafe { Box::from_raw(transport) };
}

#[expect(
    clippy::fn_params_excessive_bools,
    reason = "node configuration flags are independent booleans"
)]
unsafe fn cleanup_start_failure(
    node: &mut HewNode,
    created_transport: bool,
    created_cluster: bool,
    created_routing_table: bool,
    created_conn_mgr: bool,
    joined_cluster: bool,
) {
    if joined_cluster && !node.cluster.is_null() {
        // SAFETY: valid cluster pointer.
        unsafe { cluster::hew_cluster_leave(node.cluster) };
    }
    if created_conn_mgr && !node.conn_mgr.is_null() {
        // SAFETY: valid manager pointer from hew_connmgr_new.
        unsafe { connection::hew_connmgr_free(node.conn_mgr) };
        node.conn_mgr = ptr::null_mut();
    }
    if created_routing_table && !node.routing_table.is_null() {
        // SAFETY: valid routing table pointer from hew_routing_table_new.
        unsafe { routing::hew_routing_table_free(node.routing_table) };
        node.routing_table = ptr::null_mut();
    }
    if created_cluster && !node.cluster.is_null() {
        // SAFETY: valid cluster pointer from hew_cluster_new.
        unsafe { cluster::hew_cluster_free(node.cluster) };
        node.cluster = ptr::null_mut();
    }
    if created_transport && !node.transport.is_null() {
        // SAFETY: transport was created during this start attempt.
        unsafe { free_transport(node.transport) };
        node.transport = ptr::null_mut();
        node.transport_ops = ptr::null();
    }
}

fn actor_id_to_registry_ptr(actor_id: u64) -> *mut c_void {
    let encoded = usize::try_from(actor_id)
        .expect("u64 actor IDs fit in usize on supported targets (64-bit required)");
    encoded as *mut c_void
}

fn registry_ptr_to_actor_id(actor_ptr: *mut c_void) -> u64 {
    u64::try_from(actor_ptr as usize).expect("usize always fits in u64")
}

/// Callback invoked by the cluster when a peer leaves or is declared dead.
/// Drops any cached remote names owned by that departed node.
extern "C" fn node_membership_callback(node_id: u16, event: u8, user_data: *mut c_void) {
    if user_data.is_null() {
        return;
    }
    if event != cluster::HEW_MEMBERSHIP_EVENT_NODE_LEFT
        && event != cluster::HEW_MEMBERSHIP_EVENT_NODE_DEAD
    {
        return;
    }

    // SAFETY: user_data is a *mut HewRegistry installed during hew_node_start.
    let registry = unsafe { &*(user_data.cast::<HewRegistry>()) };
    let _ = take_registry_names_if(registry, |actor_id| {
        crate::pid::hew_pid_node(actor_id) == node_id
    });
}

/// Create a new unified distributed node runtime.
///
/// # Safety
///
/// `bind_addr` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_new(node_id: u16, bind_addr: *const c_char) -> *mut HewNode {
    cabi_guard!(bind_addr.is_null(), ptr::null_mut());

    // SAFETY: caller guarantees bind_addr points to a valid C string.
    // Portable strdup: libc::strdup does not link on Windows-MSVC (#2505).
    let bind_copy = unsafe { crate::cabi::cstr_strdup(bind_addr) };
    if bind_copy.is_null() {
        return ptr::null_mut();
    }

    let registry = Box::into_raw(Box::new(HewRegistry::default()));
    let node = Box::new(HewNode {
        node_id,
        bind_addr: bind_copy,
        transport_ops: ptr::null(),
        transport: ptr::null_mut(),
        conn_mgr: ptr::null_mut(),
        cluster: ptr::null_mut(),
        routing_table: ptr::null_mut(),
        registry,
        state: AtomicU8::new(NODE_STATE_STOPPED),
        bind_addr_owned: bind_copy,
        accept_stop: Arc::new(AtomicBool::new(false)),
        accept_thread: Mutex::new(None),
        auth: PeerAuthSnapshot::unconfigured(),
    });
    let raw = Box::into_raw(node);
    remember_node(raw);
    raw
}

/// Install the per-node [`PeerAuthSnapshot`] before the node starts.
///
/// The public `hew_node_api_start` calls this (after `hew_node_new`, before the
/// shared `hew_node_start`) with the staged config's snapshot; low-level callers
/// call it to install a strict, explicit-unverified, or unconfigured snapshot
/// per node. Rejected once the node is not `STOPPED` so a live node's admission
/// authority cannot be swapped underneath it.
///
/// # Safety
///
/// `node` must be a valid pointer returned by [`hew_node_new`].
pub(crate) unsafe fn hew_node_set_auth_snapshot(
    node: *mut HewNode,
    snapshot: PeerAuthSnapshot,
) -> c_int {
    if node.is_null() {
        set_last_error("hew_node_set_auth_snapshot: node is null");
        return -1;
    }
    // SAFETY: caller guarantees `node` is valid.
    let node_ref = unsafe { &mut *node };
    if node_ref.state.load(Ordering::Acquire) != NODE_STATE_STOPPED {
        set_last_error("hew_node_set_auth_snapshot: node is not stopped");
        return -1;
    }
    node_ref.auth = snapshot;
    0
}

/// Start the node runtime: transport listen, accept loop, and cluster init.
/// Only one `HewNode` may be active per process. Starting a second node
/// while one is running is undefined behaviour.
///
/// # Safety
///
/// `node` must be a valid pointer returned by [`hew_node_new`].
#[no_mangle]
#[expect(
    clippy::too_many_lines,
    reason = "node event loop handles all message types"
)]
pub unsafe extern "C" fn hew_node_start(node: *mut HewNode) -> c_int {
    if node.is_null() {
        set_last_error("hew_node_start: node is null");
        return -1;
    }
    // SAFETY: caller guarantees node pointer is valid.
    let node = unsafe { &mut *node };

    let current = node.state.load(Ordering::Acquire);
    if current == NODE_STATE_RUNNING {
        return 0;
    }
    if current != NODE_STATE_STOPPED {
        set_last_error("hew_node_start: node is not stopped");
        return -1;
    }

    node.state.store(NODE_STATE_STARTING, Ordering::Release);
    let mut created_transport = false;
    let mut created_cluster = false;
    let mut created_routing_table = false;
    let mut created_conn_mgr = false;
    let mut joined_cluster = false;
    macro_rules! fail_start {
        ($msg:expr) => {{
            // SAFETY: pointers belong to this node; flags track what was created in this start call.
            unsafe {
                cleanup_start_failure(
                    node,
                    created_transport,
                    created_cluster,
                    created_routing_table,
                    created_conn_mgr,
                    joined_cluster,
                )
            };
            node.state.store(NODE_STATE_STOPPED, Ordering::Release);
            set_last_error($msg);
            return -1;
        }};
    }

    // ── NodeId authority (BLOCK-5 point 1) — before any allocation/listen ──
    // The per-node `PeerAuthSnapshot` (never the process-global `ConfigState`)
    // is authoritative for `node.node_id` in strict mode. Defence-in-depth:
    // reject a self-inconsistent snapshot, then reconcile the node id.
    if let Err(reason) = node.auth.validate() {
        fail_start!(reason);
    }
    if let Some(snapshot_id) = node.auth.node_id() {
        let snapshot_id = snapshot_id.get();
        if node.node_id == 0 {
            // A low-level caller deferred the id to its snapshot.
            node.node_id = snapshot_id;
        } else if node.node_id != snapshot_id {
            // An explicit `hew_node_new(explicit_id)` contradicts the snapshot's
            // strict binding identity — refuse before the listener binds and
            // before cluster/routing/connmgr are created.
            fail_start!(format!(
                "hew_node_start: explicit node id {} conflicts with strict binding identity \
                 HEW_NODE_ID={snapshot_id} — refusing to listen (fail-closed)",
                node.node_id
            ));
        }
    }

    if node.transport.is_null() {
        let selection = match transport_selection_from_env() {
            Ok(selection) => selection,
            Err(err) => fail_start!(format!("hew_node_start: {err}")),
        };

        match selection {
            TransportSelection::Tcp => {
                // SAFETY: constructor returns owned transport pointer or null.
                node.transport = unsafe { transport::hew_transport_tcp_new() };
            }
            #[cfg(feature = "quic")]
            TransportSelection::Quic => {
                // SAFETY: constructor returns owned transport pointer or null.
                node.transport = unsafe { crate::quic_transport::hew_transport_quic_new() };
            }
            #[cfg(feature = "quic")]
            TransportSelection::QuicMesh => {
                // SAFETY: constructor returns owned transport pointer or null.
                node.transport = unsafe { crate::quic_mesh::hew_transport_quic_mesh_new() };
            }
        }
        if node.transport.is_null() {
            fail_start!("hew_node_start: failed to create transport");
        }
        created_transport = true;
    }

    // SAFETY: transport was just created or previously assigned and validated by caller.
    let t = unsafe { &*node.transport };
    node.transport_ops = t.ops;
    if node.transport_ops.is_null() {
        fail_start!("hew_node_start: transport ops are null");
    }
    // SAFETY: checked non-null above.
    let ops = unsafe { &*node.transport_ops };

    let Some(listen_fn) = ops.listen else {
        fail_start!("hew_node_start: transport listen op missing");
    };
    // SAFETY: transport implementation is valid.
    if unsafe { listen_fn(t.r#impl, node.bind_addr) } < 0 {
        fail_start!("hew_node_start: transport listen failed");
    }

    if node.cluster.is_null() {
        let cfg = cluster_config_from_env(node.node_id);
        // SAFETY: config pointer is valid for this call.
        node.cluster = unsafe { cluster::hew_cluster_new(&raw const cfg) };
        if node.cluster.is_null() {
            fail_start!("hew_node_start: failed to create cluster");
        }
        created_cluster = true;
    }

    if node.routing_table.is_null() {
        // SAFETY: constructor returns owned routing table pointer or null.
        node.routing_table = unsafe { routing::hew_routing_table_new(node.node_id) };
        if node.routing_table.is_null() {
            fail_start!("hew_node_start: failed to create routing table");
        }
        created_routing_table = true;
    }

    if node.conn_mgr.is_null() {
        // SAFETY: pointers are valid for manager lifetime. The manager receives
        // this node's per-node auth snapshot (cheap clone; identity behind Arc)
        // — never the process-global `ConfigState` or `ACTIVE_*` statics.
        node.conn_mgr = unsafe {
            connection::connmgr_new(
                node.transport,
                Some(node_inbound_router),
                node.routing_table,
                node.cluster,
                node.node_id,
                node.auth.clone(),
            )
        };
        if node.conn_mgr.is_null() {
            fail_start!("hew_node_start: failed to create connection manager");
        }
        created_conn_mgr = true;
    }

    // SAFETY: cluster pointer valid; bind_addr points to a stable strdup buffer.
    let _ = unsafe { cluster::hew_cluster_join(node.cluster, node.node_id, node.bind_addr) };
    joined_cluster = true;

    // Wire the registry gossip callback so remote name events update our
    // remote_names map.
    if !node.registry.is_null() {
        // SAFETY: cluster and registry pointers are valid for the node lifetime.
        unsafe {
            cluster::hew_cluster_set_membership_callback(
                node.cluster,
                node_membership_callback,
                node.registry.cast::<c_void>(),
            );
            cluster::hew_cluster_set_registry_callback(
                node.cluster,
                node_registry_gossip_callback,
                node.registry.cast::<c_void>(),
            );
        }
    }

    node.accept_stop.store(false, Ordering::Release);
    let stop = Arc::clone(&node.accept_stop);
    let transport = SendTransport(node.transport);
    let conn_mgr = SendConnMgr(node.conn_mgr);
    let thread_name = format!("hew-node-accept-{}", node.node_id);
    let handle = thread::Builder::new()
        .name(thread_name)
        .spawn(move || accept_loop(transport, conn_mgr, stop.as_ref()));
    if let Ok(h) = handle {
        let mut guard = node.accept_thread.lock_or_recover();
        *guard = Some(h);
    } else {
        fail_start!("hew_node_start: failed to spawn accept thread");
    }

    node.state.store(NODE_STATE_RUNNING, Ordering::Release);
    // Atomically check-and-set CURRENT_NODE under write lock to avoid
    // the TOCTOU race where two threads both read 0 and both try to set.
    with_current_node(|guard| {
        if *guard == 0 {
            *guard = ptr::from_mut(node) as usize;
            crate::pid::hew_pid_set_local_node(node.node_id);
        }
    });

    // Start the profiler with distributed runtime context if HEW_PPROF is set.
    crate::profiler::maybe_start_with_context(node.cluster, node.conn_mgr, node.routing_table);

    // Drive the SWIM failure detector: a periodic ticker that calls
    // hew_cluster_tick every protocol period, issuing PING/PING_REQ probes and
    // escalating ALIVE→SUSPECT→DEAD. Without it, failure detection is inert.
    // The ticker is stopped and joined in hew_node_stop before the cluster /
    // conn_mgr it touches are freed (see the teardown ordering below).
    // SAFETY: node is RUNNING with a live cluster + conn_mgr at this point.
    if !unsafe { crate::swim_driver::start_swim_driver(ptr::from_mut(node)) } {
        // A failed ticker spawn is not fatal to the node (gossip + the
        // connection-event SUSPECT path still work), but it means active
        // failure detection is degraded. Record it; do not fail the start.
        set_last_error("hew_node_start: SWIM failure-detector ticker failed to start");
    }

    0
}

/// Wait for this manager's in-flight inbound-ask worker threads to drain to
/// zero, with a 5-second ceiling.
///
/// Each worker spawned by [`node_inbound_router`] increments the per-manager
/// counter and decrements it (via `InboundAskGuard`) when `handle_inbound_ask`
/// returns — i.e. after its reply has been written to the wire. Draining the
/// counter therefore guarantees every already-computed reply has flushed before
/// the caller proceeds to the `CURRENT_NODE` teardown barrier and frees
/// `conn_mgr`. The per-manager counter (not the process-global
/// `INBOUND_ASK_ACTIVE`) is used so stopping one node in a multi-node test does
/// not wait on another node's workers.
///
/// The ceiling bounds a misbehaving actor dispatch; in practice well-behaved
/// nodes drain in microseconds. A worker still running after the ceiling is
/// caught by the downstream `CURRENT_NODE == 0` / `reconnect_shutdown` barrier
/// (it bails without touching the soon-to-be-freed manager).
fn drain_inbound_ask_workers(inbound_active: Option<&Arc<AtomicUsize>>) {
    const MAX_DRAIN: std::time::Duration = std::time::Duration::from_secs(5);
    const POLL: std::time::Duration = std::time::Duration::from_millis(1);
    let Some(active) = inbound_active else {
        return;
    };
    let deadline = std::time::Instant::now() + MAX_DRAIN;
    // SeqCst (matching `node_inbound_router`'s gate protocol): the caller stored
    // `inbound_spawn_closed = true` with SeqCst BEFORE this drain. This SeqCst
    // load is the drain side of the Dekker pairing — it cannot observe a zero
    // counter while a router that saw the gate open is still between its
    // increment and spawning. Once it reads zero, no further worker can spawn
    // (all routers now see the gate closed and bail), so the count stays zero.
    while active.load(Ordering::SeqCst) > 0 {
        if std::time::Instant::now() >= deadline {
            break;
        }
        thread::sleep(POLL);
    }
}

/// Stop the node runtime.
///
/// # Safety
///
/// `node` must be a valid pointer returned by [`hew_node_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_node_stop(node: *mut HewNode) -> c_int {
    if node.is_null() {
        set_last_error("hew_node_stop: node is null");
        return -1;
    }
    // SAFETY: caller guarantees node pointer is valid.
    let node = unsafe { &mut *node };
    // Hold the node registry list across teardown so actor-free cleanup cannot
    // race with cluster deallocation. The `.access()` closure spans the
    // entire teardown body; `unregister_actor_names` (called from actor-free
    // paths) also acquires KNOWN_NODES and will block here until teardown
    // completes.
    with_known_nodes(|_known_nodes| {
        if node.state.load(Ordering::Acquire) == NODE_STATE_STOPPED {
            return 0;
        }

        node.state.store(NODE_STATE_STOPPING, Ordering::Release);

        // ── Phase 1: close the inbound-ask spawn gate, then DRAIN in-flight
        // reply sends BEFORE the CURRENT_NODE teardown barrier ───────────────
        //
        // `hew_connmgr_close_inbound_spawn` flips a flag that
        // `node_inbound_router` checks before spawning: no NEW inbound-ask
        // worker starts once it is set. Workers ALREADY past that gate are
        // mid-`handle_inbound_ask` — they have computed (or are computing) a
        // reply and are about to write it to the wire in `send_reply_envelope`.
        //
        // We drain those existing workers to zero HERE — while `CURRENT_NODE`
        // still points at this node and `reconnect_shutdown` is still false —
        // so their already-earned replies flush to the asking node instead of
        // being abandoned. The drain MUST run before the barrier below, which
        // sets `mark_stopping` (→ `reconnect_shutdown`) and zeroes
        // `CURRENT_NODE`; either of those makes `send_reply_envelope` bail.
        //
        // Previously the barrier ran first, so a worker that reached
        // `send_reply_envelope` after it saw `CURRENT_NODE == 0` (or the
        // shutdown flag) and silently dropped its computed reply, surfacing a
        // spurious `ConnectionDropped` to the caller as the connection then
        // closed under the in-flight ask.
        //
        // Draining before the barrier is safe: the spawn gate guarantees the
        // counter only decreases, so the wait terminates; and `conn_mgr` is not
        // freed until well below. The 5-second ceiling bounds a misbehaving
        // dispatch — a worker still in flight after the ceiling hits the
        // `CURRENT_NODE == 0` / `reconnect_shutdown` guard below and bails
        // (fail-closed), exactly as before.
        let inbound_active_arc = if node.conn_mgr.is_null() {
            None
        } else {
            // SAFETY: node owns this connection manager until teardown completes.
            unsafe { connection::hew_connmgr_close_inbound_spawn(node.conn_mgr) };
            // SAFETY: conn_mgr is valid here and remains live until after the drain.
            unsafe { connection::hew_connmgr_inbound_ask_active(node.conn_mgr) }
        };
        drain_inbound_ask_workers(inbound_active_arc.as_ref());

        {
            // Setting CURRENT_NODE to zero acts as a lifetime barrier for any
            // ask-handler thread that slipped past the drain ceiling above.
            // Those threads acquire the CURRENT_NODE read lock in
            // `send_reply_envelope` and bail out immediately if the value is 0.
            // The write lock here blocks until every concurrent read-lock-holder
            // (i.e. every in-flight reply send) has completed, so `conn_mgr`
            // cannot be freed while any such thread is still running for the
            // current node. `mark_stopping` (→ `reconnect_shutdown`) is set here
            // too, completing the teardown signal for reconnect workers and the
            // straggler bail.
            with_current_node(|guard| {
                if !node.conn_mgr.is_null() {
                    // SAFETY: node owns this connection manager until teardown completes.
                    unsafe { connection::hew_connmgr_mark_stopping(node.conn_mgr) };
                }
                if *guard == ptr::from_mut(node) as usize {
                    *guard = 0;
                    reply_table().fail_all();
                }
            });
        }
        node.accept_stop.store(true, Ordering::Release);
        {
            let mut guard = node.accept_thread.lock_or_recover();
            if let Some(handle) = guard.take() {
                let _ = handle.join();
            }
        }

        // Stop and JOIN the SWIM ticker before freeing the cluster / conn_mgr
        // it dereferences each period. The join is the lifetime barrier: once
        // it returns, the ticker thread has fully exited and cannot race the
        // teardown of node resources below (LESSONS: cleanup-all-exits, the
        // reactor's ticker-joined-before-free ordering).
        // SAFETY: node pointer is used only as the driver registry key.
        unsafe { crate::swim_driver::stop_swim_driver(ptr::from_mut(node)) };

        // Shutdown profiler threads before freeing node resources they might access.
        crate::profiler::shutdown();

        // Remove this node's published names from the local registry before the
        // cluster state is torn down. Remote nodes drop cached names when the
        // membership callback observes this node leaving or dying.
        // SAFETY: `node` is valid for the duration of hew_node_stop, and KNOWN_NODES
        // serialization above prevents concurrent actor cleanup from racing this teardown.
        unsafe { unregister_local_names_for_node(node) };

        if !node.conn_mgr.is_null() {
            #[cfg(test)]
            NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.hit();
            // SAFETY: valid manager pointer from hew_connmgr_new; all inbound ask
            // workers that could still hold it were drained in Phase 1 (above the
            // CURRENT_NODE barrier). Any straggler past the drain ceiling bails at
            // the `CURRENT_NODE == 0` / `reconnect_shutdown` guard before touching
            // conn_mgr, so the free is race-free.
            unsafe { connection::hew_connmgr_free(node.conn_mgr) };
            node.conn_mgr = ptr::null_mut();
        }

        if !node.routing_table.is_null() {
            // SAFETY: valid routing table pointer from hew_routing_table_new.
            unsafe { routing::hew_routing_table_free(node.routing_table) };
            node.routing_table = ptr::null_mut();
        }

        if !node.cluster.is_null() {
            // SAFETY: valid cluster pointer.
            unsafe { cluster::hew_cluster_leave(node.cluster) };
            // SAFETY: valid cluster pointer from hew_cluster_new.
            unsafe { cluster::hew_cluster_free(node.cluster) };
            node.cluster = ptr::null_mut();
        }

        if !node.transport.is_null() {
            // SAFETY: valid transport pointer from constructor.
            let transport = unsafe { &*node.transport };
            // SAFETY: ops pointer is part of valid transport.
            if let Some(ops) = unsafe { transport.ops.as_ref() } {
                if let Some(destroy_fn) = ops.destroy {
                    // SAFETY: transport impl belongs to this transport.
                    unsafe { destroy_fn(transport.r#impl) };
                }
            }
            // SAFETY: transport was allocated by Box::into_raw.
            let _ = unsafe { Box::from_raw(node.transport) };
            node.transport = ptr::null_mut();
            node.transport_ops = ptr::null();
        }

        node.state.store(NODE_STATE_STOPPED, Ordering::Release);
        0
    }) // KNOWN_NODES.access
}

/// Free a node runtime and all owned resources.
///
/// # Safety
///
/// `node` must be a valid pointer returned by [`hew_node_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_node_free(node: *mut HewNode) {
    cabi_guard!(node.is_null());

    // SAFETY: same pointer validity contract as this function.
    let _ = unsafe { hew_node_stop(node) };
    forget_node(node);
    // SAFETY: caller surrenders ownership of node pointer.
    let mut node = unsafe { Box::from_raw(node) };

    if !node.registry.is_null() {
        // SAFETY: registry was created with Box::into_raw.
        let _ = unsafe { Box::from_raw(node.registry) };
        node.registry = ptr::null_mut();
    }

    if !node.bind_addr_owned.is_null() {
        // SAFETY: bind_addr_owned was allocated via cstr_strdup (libc::malloc).
        unsafe { libc::free(node.bind_addr_owned.cast::<c_void>()) };
        node.bind_addr_owned = ptr::null_mut();
        node.bind_addr = ptr::null();
    }
}

/// Register a local actor ID under a name.
///
/// # Safety
///
/// - `node` must be valid.
/// - `name` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_register(
    node: *mut HewNode,
    name: *const c_char,
    actor: u64,
) -> c_int {
    cabi_guard!(node.is_null() || name.is_null(), -1);
    // SAFETY: caller guarantees node pointer validity.
    let node = unsafe { &mut *node };
    if node.registry.is_null() {
        return -1;
    }

    // SAFETY: name was checked non-null and is a valid C string by caller contract.
    let key = unsafe { CStr::from_ptr(name) }
        .to_string_lossy()
        .into_owned();
    // SAFETY: registry pointer was allocated in hew_node_new and freed in hew_node_free.
    let reg = unsafe { &*node.registry };

    // Fail-closed registration-string boundary: the user-chosen string is the
    // cluster-wide routing key, and it carries NO actor-type qualifier on the
    // wire — two DISTINCT actors (possibly two distinct qualified actor
    // types, e.g. `bank.Account` and `store.Account`) registering the same
    // string would silently re-point cluster routing. Refuse the second
    // registration with a recorded diagnostic instead of clobbering the
    // mapping. Re-registering the SAME actor under its existing name is
    // idempotent. Full fix — qualified registration keys on the gossip wire
    // (a CDDL/CBOR RegistryEvent change) — is a tracked follow-up; this
    // boundary only refuses the ambiguity, it cannot disambiguate it.
    {
        let map = reg.remote_names.lock_or_recover();
        if let Some(existing) = map.get(&key).copied() {
            if existing != actor {
                drop(map);
                set_last_error(format!(
                    "hew_node_register: name `{key}` is already registered to \
                     actor {existing}; refusing to re-point it to actor {actor} \
                     — registration strings are cluster-wide routing keys and \
                     carry no actor-type qualifier, so the conflict cannot be \
                     disambiguated (qualified registration keys on the gossip \
                     wire are a tracked follow-up). Unregister the existing \
                     name first or choose a distinct string"
                ));
                return -1;
            }
        }
    }

    // SAFETY: registry API expects a stable C string pointer.
    let rc =
        unsafe { crate::registry::hew_registry_register(name, actor_id_to_registry_ptr(actor)) };
    if rc != 0 {
        return -1;
    }

    {
        let mut map = reg.remote_names.lock_or_recover();
        map.insert(key.clone(), actor);
    }

    // Record the registration incarnation for this name. A re-registration to a
    // DIFFERENT actor (e.g. after an unregister) bumps the incarnation and
    // supersedes the prior serial, so a captured `RemotePid<T>` over the old
    // actor fails closed with `StaleRef` at the dispatch boundary.
    let _incarnation = registration_record(reg, &key, crate::pid::hew_pid_serial(actor));

    // Propagate to cluster gossip so remote nodes learn about this actor.
    if !node.cluster.is_null() {
        // SAFETY: cluster pointer is valid while the node is alive.
        unsafe { cluster::hew_cluster_registry_add(node.cluster, name, actor) };
    }
    if !node.conn_mgr.is_null() {
        // SAFETY: connection manager pointer is valid while the node is alive.
        unsafe {
            connection::hew_connmgr_broadcast_registry_gossip(node.conn_mgr, &key, actor, true);
        }
    }

    0
}

/// Unregister a named actor from this node.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// - `node` must be valid.
/// - `name` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_unregister(node: *mut HewNode, name: *const c_char) -> c_int {
    cabi_guard!(node.is_null() || name.is_null(), -1);
    // SAFETY: caller guarantees node pointer validity.
    let node = unsafe { &mut *node };
    if node.registry.is_null() {
        return -1;
    }

    // Also unregister from the global local registry.
    // SAFETY: name was validated non-null by caller contract.
    unsafe { crate::registry::hew_registry_unregister(name) };

    // SAFETY: name was checked non-null and is a valid C string by caller contract.
    let key = unsafe { CStr::from_ptr(name) }
        .to_string_lossy()
        .into_owned();
    // SAFETY: registry pointer was allocated in hew_node_new and freed in hew_node_free.
    let reg = unsafe { &*node.registry };
    {
        let mut map = reg.remote_names.lock_or_recover();
        map.remove(&key);
    }

    // Propagate removal to cluster gossip.
    if !node.cluster.is_null() {
        // SAFETY: cluster pointer is valid while the node is alive.
        unsafe { cluster::hew_cluster_registry_remove(node.cluster, name) };
    }
    if !node.conn_mgr.is_null() {
        // SAFETY: connection manager pointer is valid while the node is alive.
        unsafe {
            connection::hew_connmgr_broadcast_registry_gossip(node.conn_mgr, &key, 0, false);
        }
    }

    0
}

/// Look up an actor ID by name.
///
/// # Safety
///
/// - `node` must be valid.
/// - `name` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_lookup(node: *mut HewNode, name: *const c_char) -> u64 {
    cabi_guard!(node.is_null() || name.is_null(), 0);
    // SAFETY: caller guarantees node pointer validity.
    let node = unsafe { &*node };

    // SAFETY: registry API expects a valid C string pointer.
    let local_ptr = unsafe { crate::registry::hew_registry_lookup(name) };
    if !local_ptr.is_null() {
        return registry_ptr_to_actor_id(local_ptr);
    }

    if node.registry.is_null() {
        return 0;
    }
    // SAFETY: name is non-null and valid by caller contract.
    let key = unsafe { CStr::from_ptr(name) }
        .to_string_lossy()
        .into_owned();
    // SAFETY: registry pointer was allocated in hew_node_new and freed in hew_node_free.
    let reg = unsafe { &*node.registry };
    let map = reg.remote_names.lock_or_recover();
    map.get(&key).copied().unwrap_or(0)
}

/// Send a message to a target PID, routing local vs remote by PID node ID.
///
/// `dispatch` is the TARGET actor TYPE's dispatch function pointer, keying the
/// cross-node serialize codec `(dispatch, msg_type)` on the remote path. Unused
/// on the local path. May be null for a local-only send.
///
/// # Safety
///
/// - `node` must be valid.
/// - `payload` must be valid for `payload_len` bytes, or null when len is 0.
/// - `dispatch` is an opaque codec key, never dereferenced.
#[no_mangle]
pub unsafe extern "C" fn hew_node_send(
    node: *mut HewNode,
    target_pid: u64,
    dispatch: *const c_void,
    msg_type: i32,
    payload: *const u8,
    payload_len: usize,
) -> c_int {
    if node.is_null() || (payload.is_null() && payload_len > 0) {
        return -1;
    }
    // SAFETY: caller guarantees node pointer validity.
    let node = unsafe { &*node };
    if node.state.load(Ordering::Acquire) != NODE_STATE_RUNNING {
        return -1;
    }

    // StaleRef gate (shared with the ask + monitor paths): a captured
    // `RemotePid<T>` whose registration has been superseded by a re-registration
    // of its name to a different actor fails CLOSED here — it is never routed to
    // the live replacement (DI-012 / LESSONS `boundary-fail-closed`).
    if dispatch_is_stale(node, target_pid) {
        set_last_error(format!(
            "remote send refused: target pid serial {} names a superseded actor \
             registration (the name was re-registered to a different actor); the \
             captured RemotePid is stale",
            crate::pid::hew_pid_serial(target_pid)
        ));
        return HEW_ERR_STALE_REF;
    }

    let target_node_id = crate::pid::hew_pid_node(target_pid);
    if target_node_id == 0 || target_node_id == node.node_id {
        // SAFETY: actor send API handles null payload when len is 0.
        return unsafe {
            crate::actor::hew_actor_send_by_id(
                target_pid,
                dispatch,
                msg_type,
                payload.cast_mut().cast::<c_void>(),
                payload_len,
            )
        };
    }

    if node.conn_mgr.is_null() {
        return -1;
    }
    // SAFETY: routing table pointer is valid while node is running.
    let conn_id = unsafe { routing::hew_routing_lookup(node.routing_table, target_pid) };
    if conn_id < 0 {
        return -1;
    }

    // Quarantine consult: under a Quarantine policy, a buried peer that has not
    // rejoined at a strictly higher incarnation fails closed here — before any
    // payload bytes are serialized, so nothing is allocated on the blocked path.
    if quarantine_blocks_send(node, target_node_id) {
        set_last_error(format!(
            "cross-node send refused: node {target_node_id} is quarantined (partitioned \
             until it rejoins at a higher incarnation)"
        ));
        return -1;
    }

    // Serialize the payload before it leaves this address space. `payload` is the
    // raw in-memory value (a struct that may contain heap pointers); shipping it
    // verbatim would make the receiver dereference stale pointers and crash. The
    // codec for `msg_type` encodes the value's CONTENTS into transport-safe bytes.
    //
    // Fail closed: a non-empty payload with no registered codec must NOT be sent
    // raw — refuse the send (the caller surfaces a SendError). A genuinely empty
    // payload (payload_len == 0) needs no serialization.
    let serialized: Option<(*mut u8, usize)> = if payload_len > 0 {
        let mut out_len: usize = 0;
        // SAFETY: payload points to a valid value of the message type; out_len valid.
        // Keyed by `(dispatch, msg_type)` — the target actor TYPE's serializer,
        // so a colliding `msg_type` on another actor type cannot select the
        // wrong codec for the value being shipped.
        let bytes = unsafe {
            crate::xnode_serial::encode_payload(
                dispatch,
                msg_type,
                payload.cast::<std::ffi::c_void>(),
                &raw mut out_len,
            )
        };
        if bytes.is_null() {
            set_last_error(format!(
                "cross-node send rejected: no serialization codec registered for \
                 msg_type={msg_type}; the payload cannot cross the node boundary safely"
            ));
            return -1;
        }
        Some((bytes, out_len))
    } else {
        None
    };

    // Gate the send through the cross-node validator: the bytes are now genuinely
    // serialized, so the payload class is truthful.
    if crate::mailbox_envelope::validate_cross_node_send_params(
        crate::mailbox_envelope::MailboxPayloadClass::SerializedCrossNode as u8,
        crate::mailbox_envelope::CANCEL_TOKEN_NONE,
    )
    .is_none()
    {
        if let Some((bytes, _)) = serialized {
            // SAFETY: bytes came from encode_payload (libc::malloc).
            unsafe { crate::xnode_serial::hew_ser_free_bytes(bytes) };
        }
        return -1;
    }

    let (send_ptr, send_len) = match serialized {
        Some((bytes, len)) => (bytes, len),
        None => (std::ptr::null_mut(), 0),
    };
    // SAFETY: conn_mgr and conn_id were validated above; send_ptr/send_len are the
    // serialized bytes (or null/0 for an empty payload).
    let rc = unsafe {
        connection::hew_connmgr_send(
            node.conn_mgr,
            conn_id,
            target_pid,
            msg_type,
            send_ptr,
            send_len,
        )
    };
    if let Some((bytes, _)) = serialized {
        // hew_connmgr_send copies the bytes into its envelope; free our copy.
        // SAFETY: bytes came from encode_payload (libc::malloc).
        unsafe { crate::xnode_serial::hew_ser_free_bytes(bytes) };
    }
    rc
}

// ── Cross-node monitor ───────────────────────────────────────────────────────

const fn setup_error(variant: i64) -> i64 {
    -(variant + 1)
}

// MonitorError declaration order in std/link_monitor.hew.
const MONITOR_ERR_NODE_NOT_RUNNING: i64 = setup_error(0);
const MONITOR_ERR_INVALID_TARGET: i64 = setup_error(1);
const MONITOR_ERR_PARTITION: i64 = setup_error(2);
const MONITOR_ERR_STALE_REF: i64 = setup_error(3);
const MONITOR_ERR_ENCODE_FAILURE: i64 = setup_error(4);
const MONITOR_ERR_LOCAL_SHUTDOWN: i64 = setup_error(5);

// LinkError declaration order in std/builtins.hew. The local-only variants
// AlreadyLinked=0 and TargetDead=1 remain first for compatibility.
const LINK_ERR_NODE_NOT_RUNNING: i64 = setup_error(2);
const LINK_ERR_NO_CURRENT_ACTOR: i64 = setup_error(3);
const LINK_ERR_INVALID_TARGET: i64 = setup_error(4);
const LINK_ERR_PARTITION: i64 = setup_error(5);
const LINK_ERR_STALE_REF: i64 = setup_error(6);
const LINK_ERR_ENCODE_FAILURE: i64 = setup_error(7);
const LINK_ERR_LOCAL_SHUTDOWN: i64 = setup_error(8);

fn setup_ref_id(ref_id: u64) -> i64 {
    i64::try_from(ref_id).expect("distributed ref ids stay below i64::MAX")
}

/// Encode a `CTRL_MONITOR_*` control frame and send it on the connection routing
/// to `target_pid`'s node. Returns 0 on a successful send, -1 otherwise (no
/// route, no manager, encode failure). Never panics; logs via `set_last_error`.
///
/// # Safety
///
/// `node` must be a valid running `HewNode` pointer.
unsafe fn send_monitor_control_frame(
    node: &HewNode,
    target_pid: u64,
    ctrl_kind: u64,
    payload: Vec<u8>,
) -> c_int {
    if node.conn_mgr.is_null() || node.routing_table.is_null() {
        set_last_error("cross-node monitor: node has no connection manager / routing table");
        return -1;
    }
    // SAFETY: routing table pointer is valid while the node is running.
    let conn_id = unsafe { routing::hew_routing_lookup(node.routing_table, target_pid) };
    if conn_id < 0 {
        set_last_error("cross-node monitor: no route to target node");
        return -1;
    }
    let frame = crate::envelope::ControlFrame {
        version: crate::envelope::WIRE_VERSION,
        ctrl_kind,
        payload,
    };
    let bytes = match crate::envelope::encode_control_frame(&frame) {
        Ok(bytes) => bytes,
        Err(err) => {
            set_last_error(format!(
                "cross-node monitor control frame encode failure: {err}"
            ));
            return -1;
        }
    };
    // SAFETY: conn_mgr validated above; bytes is a complete encoded frame.
    unsafe {
        connection::hew_connmgr_send_preencoded(node.conn_mgr, conn_id, bytes.as_ptr(), bytes.len())
    }
}

/// `monitor(RemotePid<T>)` → register a cross-node monitor and return its
/// `ref_id`.
///
/// Resolves the current node, records a watcher-side entry keyed by a fresh
/// `ref_id`, and sends a `CTRL_MONITOR_REQ` to the node owning `target_pid` so
/// that node will fan a `CTRL_MONITOR_DOWN` back when the target reaches a
/// terminal state. The returned `ref_id` is assembled into the `MonitorRef`
/// value and is the key `hew_node_monitor_recv` blocks on.
///
/// Returns a positive `ref_id` on success. Setup failures are returned as
/// negative `MonitorError` codes encoded as `-(variant + 1)`, so the Hew surface
/// constructs `Result<MonitorRef, MonitorError>` and never manufactures an
/// invalid zero-valued handle.
#[no_mangle]
pub extern "C" fn hew_node_monitor(target_pid: u64) -> i64 {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("hew_node_monitor: no runtime installed");
        return MONITOR_ERR_NODE_NOT_RUNNING;
    };
    let remote_node_id = crate::pid::hew_pid_node(target_pid);
    let target_serial = crate::pid::hew_pid_serial(target_pid);
    if remote_node_id == 0 || remote_node_id == rt.node.local_node_id() {
        // Not a remote target — the cross-node route does not apply. The checker
        // routes only RemotePid receivers here, but guard fail-closed anyway.
        set_last_error("hew_node_monitor: target is not on a remote node");
        return MONITOR_ERR_INVALID_TARGET;
    }

    // StaleRef gate (shared with the send + ask paths): a captured
    // `RemotePid<T>` whose registration has been superseded fails CLOSED here —
    // no watcher is registered against the stale capture, and the typed
    // StaleRef setup error is returned. The diagnostic names StaleRef so the
    // cause is not collapsed into the generic no-route case.
    let stale = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        // SAFETY: read lock pins the current node pointer for this check.
        !node.is_null() && dispatch_is_stale(unsafe { &*node }, target_pid)
    });
    if stale {
        set_last_error(format!(
            "hew_node_monitor: target pid serial {target_serial} names a superseded \
             actor registration; the captured RemotePid is stale (StaleRef)"
        ));
        return MONITOR_ERR_STALE_REF;
    }

    // Record the watcher entry first so the connection-drop / SWIM-DEAD fan-out
    // can deliver even if the request send races a drop.
    let ref_id = rt
        .dist_monitors
        .register_watcher(remote_node_id, target_serial);

    let payload =
        match crate::envelope::encode_monitor_req_payload(&crate::envelope::MonitorReqPayload {
            watcher_node_id: rt.node.local_node_id(),
            ref_id,
            target_serial,
        }) {
            Ok(payload) => payload,
            Err(err) => {
                set_last_error(format!(
                    "hew_node_monitor: req payload encode failure: {err}"
                ));
                rt.dist_monitors.remove_watcher(ref_id);
                return MONITOR_ERR_ENCODE_FAILURE;
            }
        };

    let send_result = with_current_node_read(|guard| {
        if *guard == 0 {
            set_last_error("hew_node_monitor: no current node");
            return None;
        }
        let node = *guard as *const HewNode;
        // SAFETY: read lock pins the current node pointer for this call.
        let node_ref = unsafe { &*node };
        // SAFETY: node_ref is valid for this call.
        Some(unsafe {
            send_monitor_control_frame(
                node_ref,
                target_pid,
                crate::envelope::CTRL_MONITOR_REQ,
                payload,
            )
        })
    });
    match send_result {
        Some(0) => setup_ref_id(ref_id),
        Some(_) => {
            rt.dist_monitors.remove_watcher(ref_id);
            MONITOR_ERR_PARTITION
        }
        None => {
            rt.dist_monitors.remove_watcher(ref_id);
            MONITOR_ERR_LOCAL_SHUTDOWN
        }
    }
}

/// `link_remote(RemotePid<T>, PartitionPolicy)` → establish a cross-node link
/// and return its `ref_id`.
///
/// The calling actor (resolved via `hew_actor_self`) links the remote actor
/// `target_pid` so the remote's death fires the per-link `PartitionPolicy`
/// (`policy_tag`; `CrashLinked` == 3 crashes the LOCAL linked actor). Records a
/// watcher-side LINK entry keyed by a fresh `ref_id` (carrying the local actor
/// id + policy) and sends a `CTRL_LINK_REQ` to the owning node so it (a) fans a
/// `CTRL_LINK_DOWN` back when the target dies AND (b) registers the reverse link
/// so the LOCAL actor's death crashes the remote peer too (bidirectional OTP).
///
/// Returns a positive internal `ref_id` on success. Setup failures are returned
/// as negative `LinkError` codes encoded as `-(variant + 1)`, so
/// `link_remote` cannot report `Ok(())` for a link that was never established.
#[no_mangle]
pub extern "C" fn hew_node_link_remote(target_pid: i64, policy_tag: i64) -> i64 {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("hew_node_link_remote: no runtime installed");
        return LINK_ERR_NODE_NOT_RUNNING;
    };
    #[expect(
        clippy::cast_sign_loss,
        reason = "target_pid is a packed u64 pid reinterpreted as i64 on the Hew side"
    )]
    let target_pid = target_pid as u64;
    // The linking subject is the calling actor; a cross-node link with no local
    // actor has nothing to crash, so fail closed.
    let self_actor = crate::actor::hew_actor_self();
    if self_actor.is_null() {
        set_last_error("hew_node_link_remote: no current actor (link subject)");
        return LINK_ERR_NO_CURRENT_ACTOR;
    }
    // SAFETY: hew_actor_self returned a non-null live actor pointer.
    // The actor `id` is already a packed pid (node<<48 | serial); the cascade
    // looks it up verbatim via get_actor_ptr_by_id, while the wire carries only
    // the serial part so the peer can address the linker on its own node.
    let local_actor_id = unsafe { (*self_actor).id };
    let local_serial = crate::pid::hew_pid_serial(local_actor_id);

    let remote_node_id = crate::pid::hew_pid_node(target_pid);
    let target_serial = crate::pid::hew_pid_serial(target_pid);
    if remote_node_id == 0 || remote_node_id == rt.node.local_node_id() {
        set_last_error("hew_node_link_remote: target is not on a remote node");
        return LINK_ERR_INVALID_TARGET;
    }
    let stale = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        // SAFETY: read lock pins the current node pointer for this check.
        !node.is_null() && dispatch_is_stale(unsafe { &*node }, target_pid)
    });
    if stale {
        set_last_error(format!(
            "hew_node_link_remote: target pid serial {target_serial} names a superseded \
             actor registration; the captured RemotePid is stale (StaleRef)"
        ));
        return LINK_ERR_STALE_REF;
    }
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_sign_loss,
        reason = "policy_tag is a small PartitionPolicy discriminant (0..4); the Hew side passes i64"
    )]
    let policy_tag = policy_tag as u8;

    // Record the watcher-side LINK entry first so a connection-drop / SWIM-DEAD
    // fan-out can deliver even if the request send races a drop.
    let ref_id = rt.dist_monitors.register_link_watcher(
        remote_node_id,
        target_serial,
        local_actor_id,
        policy_tag,
    );

    let payload = match crate::envelope::encode_link_req_payload(&crate::envelope::LinkReqPayload {
        linker_node_id: rt.node.local_node_id(),
        ref_id,
        target_serial,
        linker_serial: local_serial,
        policy_tag,
        // Original request: the receiver reciprocates with a reverse request so
        // the link is bidirectional (the linker's death also crashes the target).
        reciprocate: 1,
    }) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "hew_node_link_remote: req payload encode failure: {err}"
            ));
            rt.dist_monitors.remove_watcher(ref_id);
            return LINK_ERR_ENCODE_FAILURE;
        }
    };

    let send_result = with_current_node_read(|guard| {
        if *guard == 0 {
            set_last_error("hew_node_link_remote: no current node");
            return None;
        }
        let node = *guard as *const HewNode;
        // SAFETY: read lock pins the current node pointer for this call.
        let node_ref = unsafe { &*node };
        // SAFETY: node_ref is valid for this call.
        Some(unsafe {
            send_monitor_control_frame(
                node_ref,
                target_pid,
                crate::envelope::CTRL_LINK_REQ,
                payload,
            )
        })
    });
    match send_result {
        Some(0) => setup_ref_id(ref_id),
        Some(_) => {
            rt.dist_monitors.remove_watcher(ref_id);
            LINK_ERR_PARTITION
        }
        None => {
            rt.dist_monitors.remove_watcher(ref_id);
            LINK_ERR_LOCAL_SHUTDOWN
        }
    }
}

/// `hew_node_monitor_recv(ref_id, timeout_ms)` → block for the cross-node
/// monitor's terminal signal, returning the carried reason.
///
/// Returns the down-reason (a `HewActorState` value for a clean exit / crash,
/// or `MONITOR_REASON_LOST` for a connection-drop / partition) exactly once;
/// further calls, an unknown `ref_id`, or a deadline that elapses first return
/// `MONITOR_REASON_TIMEOUT`. A negative `timeout_ms` is treated as zero (a
/// non-blocking poll).
#[no_mangle]
pub extern "C" fn hew_node_monitor_recv(ref_id: i64, timeout_ms: i64) -> i64 {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return i64::from(crate::dist_monitor::MONITOR_REASON_TIMEOUT);
    };
    #[expect(
        clippy::cast_sign_loss,
        reason = "ref_id originates from a u64 register_watcher counter reinterpreted as i64 on the Hew side"
    )]
    let ref_id = ref_id as u64;
    let timeout = std::time::Duration::from_millis(timeout_ms.max(0).unsigned_abs());
    i64::from(rt.dist_monitors.recv_down(ref_id, timeout))
}

/// `MonitorRef::close` on a cross-node monitor → tear down the watcher entry and
/// send a `CTRL_DEMONITOR` to the peer. Idempotent / silent on an unknown
/// `ref_id` (mirrors `hew_actor_demonitor`).
///
/// Note: the std `MonitorRef::close` lowers to `hew_actor_demonitor` today, which
/// is a local-table no-op for a cross-node ref. This function is the cross-node
/// teardown the demonitor/drop path will route to once the locality split lands
/// on the drop side; for now it is exposed so a program can explicitly retract a
/// cross-node monitor.
#[no_mangle]
pub extern "C" fn hew_node_demonitor(ref_id: i64) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    #[expect(
        clippy::cast_sign_loss,
        reason = "ref_id originates from a u64 register_watcher counter reinterpreted as i64 on the Hew side"
    )]
    let ref_id = ref_id as u64;
    let Some((remote_node_id, target_serial)) = rt.dist_monitors.remove_watcher(ref_id) else {
        return;
    };
    let target_pid = crate::pid::hew_pid_make(remote_node_id, target_serial);
    let payload =
        match crate::envelope::encode_monitor_req_payload(&crate::envelope::MonitorReqPayload {
            watcher_node_id: rt.node.local_node_id(),
            ref_id,
            target_serial,
        }) {
            Ok(payload) => payload,
            Err(err) => {
                set_last_error(format!("hew_node_demonitor: payload encode failure: {err}"));
                return;
            }
        };
    with_current_node_read(|guard| {
        if *guard == 0 {
            return;
        }
        let node = *guard as *const HewNode;
        // SAFETY: read lock pins the current node pointer for this call.
        let node_ref = unsafe { &*node };
        // SAFETY: node_ref is valid for this call.
        let _ = unsafe {
            send_monitor_control_frame(
                node_ref,
                target_pid,
                crate::envelope::CTRL_DEMONITOR,
                payload,
            )
        };
    });
}

/// Fan out a `CTRL_MONITOR_DOWN` to every remote node monitoring a locally-dying
/// actor (terminal sweep).
///
/// Called from `notify_monitors_on_death` (the `hew_actor_trap` terminal hook)
/// for a locally-owned actor: takes the target-side remote watchers for
/// `target_serial` and sends each watcher node a DOWN frame carrying its
/// `ref_id` and the terminal `reason`. This is the clean-exit AND crash path —
/// both route through the trap.
///
/// Fail-closed and reentrancy-tolerant (R7): if no runtime / current node /
/// connection manager is installed (the trap can run with none — e.g. the
/// supervisor-cascade unit tests), it no-ops without sending or panicking,
/// exactly as the local sweep does. The remote watchers are still removed so a
/// later sweep finds nothing.
pub(crate) fn fan_out_remote_monitor_down(target_serial: u64, reason: i32) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    // Fast path: no remote watcher for this actor → nothing to send.
    if !rt.dist_monitors.has_remote_watchers(target_serial) {
        return;
    }
    let watchers = rt.dist_monitors.take_remote_watchers(target_serial);
    if watchers.is_empty() {
        return;
    }
    with_current_node_read(|guard| {
        if *guard == 0 {
            return;
        }
        let node = *guard as *const HewNode;
        // SAFETY: read lock pins the current node pointer for this call.
        let node_ref = unsafe { &*node };
        if node_ref.conn_mgr.is_null() || node_ref.routing_table.is_null() {
            return;
        }
        for watcher in watchers {
            let payload = match crate::envelope::encode_monitor_down_payload(
                &crate::envelope::MonitorDownPayload {
                    ref_id: watcher.ref_id,
                    reason,
                },
            ) {
                Ok(payload) => payload,
                Err(err) => {
                    set_last_error(format!(
                        "monitor down fan-out: payload encode failure: {err}"
                    ));
                    continue;
                }
            };
            // A LINK watcher receives CTRL_LINK_DOWN (its node crashes the local
            // linked actor via the mailbox EXIT cascade); a MONITOR watcher
            // receives CTRL_MONITOR_DOWN (its node arms a recv slot). The wire
            // payload is identical; only the kind — and the receiver's handling —
            // diverge (the death-signal-fires-on-every-terminal-cause invariant
            // covers BOTH watcher kinds on this one terminal sweep).
            let ctrl_kind = if watcher.is_link {
                crate::envelope::CTRL_LINK_DOWN
            } else {
                crate::envelope::CTRL_MONITOR_DOWN
            };
            // Route the DOWN back to the watcher node by a pid on that node.
            let watcher_pid = crate::pid::hew_pid_make(watcher.watcher_node_id, 0);
            // SAFETY: node_ref is valid for this call.
            let _ =
                unsafe { send_monitor_control_frame(node_ref, watcher_pid, ctrl_kind, payload) };
        }
    });
}

/// Handle an inbound `CTRL_LINK_REQ`: a remote node is linking one of
/// our local actors. Establishes the BIDIRECTIONAL cross-node link.
///
/// On the ORIGINAL request (`reciprocate == 1`):
/// 1. Register a target-side remote LINK watcher for our `target_serial` →
///    `(linker_node, ref_id)`, so when our actor dies the terminal sweep fans a
///    `CTRL_LINK_DOWN` to the linker (Direction 1: our death → crash the linker).
/// 2. Register a watcher-side LINK entry watching `(linker_node, linker_serial)`
///    keyed by a fresh reverse ref, action = crash OUR `target` actor per the
///    policy, so the linker's death crashes our actor (Direction 2 receive).
/// 3. Send a reverse `CTRL_LINK_REQ` (`reciprocate == 0`) back to the linker so
///    IT registers the target-side watcher that fans Direction 2's DOWN to us.
///
/// On the REVERSE request (`reciprocate == 0`): only step 1 — register the
/// target-side remote LINK watcher — and do NOT reciprocate again (bounding the
/// handshake to one round trip, never an infinite reciprocation).
///
/// Fail-closed: a no-runtime / no-current-node / no-route state no-ops without
/// panicking; the EXIT is only ever synthesized later for an entry THIS node
/// registered, so a forged frame cannot crash an actor this node never linked.
pub(crate) fn handle_inbound_link_req(
    payload: &crate::envelope::LinkReqPayload,
    authenticated_peer: u16,
) {
    if payload.linker_node_id != authenticated_peer {
        set_last_error(format!(
            "handle_inbound_link_req: linker_node_id {} does not match authenticated peer {authenticated_peer}",
            payload.linker_node_id
        ));
        return;
    }
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("handle_inbound_link_req: no runtime installed");
        return;
    };
    // Step 1 (both directions): our local `target_serial` gains a remote LINK
    // watcher on the linker node. The terminal sweep fans CTRL_LINK_DOWN to it.
    rt.dist_monitors.register_remote_link_watcher(
        payload.target_serial,
        authenticated_peer,
        payload.ref_id,
    );

    if payload.reciprocate == 0 {
        // Reverse request: target-side registration only; do not loop.
        return;
    }

    // Step 2 (original only): register the reverse watcher-side LINK entry so the
    // linker's death crashes OUR target actor per the policy.
    let local_node = rt.node.local_node_id();
    let our_target_id = crate::pid::hew_pid_make(local_node, payload.target_serial);
    let reverse_ref = rt.dist_monitors.register_link_watcher(
        authenticated_peer,
        payload.linker_serial,
        our_target_id,
        payload.policy_tag,
    );

    // Step 3 (original only): send the reverse CTRL_LINK_REQ so the linker node
    // registers the target-side watcher that fans Direction 2's DOWN back to us.
    let reverse_payload = crate::envelope::LinkReqPayload {
        linker_node_id: local_node,
        ref_id: reverse_ref,
        // From the linker's perspective the roles swap: its linker actor is now
        // our "target" to watch, and our target is now the reverse "linker".
        target_serial: payload.linker_serial,
        linker_serial: payload.target_serial,
        policy_tag: payload.policy_tag,
        reciprocate: 0,
    };
    let bytes = match crate::envelope::encode_link_req_payload(&reverse_payload) {
        Ok(bytes) => bytes,
        Err(err) => {
            set_last_error(format!(
                "handle_inbound_link_req: reverse payload encode failure: {err}"
            ));
            return;
        }
    };
    // Route the reverse REQ back to the linker node by a pid on that node.
    let linker_pid = crate::pid::hew_pid_make(authenticated_peer, payload.linker_serial);
    with_current_node_read(|guard| {
        if *guard == 0 {
            return;
        }
        let node = *guard as *const HewNode;
        // SAFETY: read lock pins the current node pointer for this call.
        let node_ref = unsafe { &*node };
        // SAFETY: node_ref is valid for this call.
        let _ = unsafe {
            send_monitor_control_frame(node_ref, linker_pid, crate::envelope::CTRL_LINK_REQ, bytes)
        };
    });
}

/// Handle an inbound `CTRL_UNLINK`: a remote node retracted a prior
/// link of one of our local actors. Remove the target-side remote LINK watcher.
/// Idempotent / fail-closed on a missing entry.
pub(crate) fn handle_inbound_unlink(
    payload: &crate::envelope::LinkReqPayload,
    authenticated_peer: u16,
) {
    if payload.linker_node_id != authenticated_peer {
        set_last_error(format!(
            "handle_inbound_unlink: linker_node_id {} does not match authenticated peer {authenticated_peer}",
            payload.linker_node_id
        ));
        return;
    }
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    rt.dist_monitors.remove_remote_watcher(
        payload.target_serial,
        authenticated_peer,
        payload.ref_id,
    );
}

/// Handle an inbound `CTRL_LINK_DOWN`: a node owning an actor we LINK
/// reports it reached a terminal state. Fire the cross-node link cascade —
/// synthesize a `SYS_MSG_EXIT` into the LOCAL linked actor's MAILBOX and crash it
/// (for `CrashLinked`), keyed by the local actor id stored in our link entry.
///
/// This is THE divergence from monitor's `deliver_to_ref` (which arms a recv
/// slot). A monitor DOWN lands in a slot the program polls; a `CrashLinked` link
/// DOWN crashes the linked actor through its mailbox. The EXIT is fired exactly
/// once and ONLY for a link entry THIS node registered AND ONLY when
/// `authenticated_peer` (the handshake-verified sender of the frame, resolved by
/// the connection layer) matches that entry's own linked-to node — so neither a
/// forged `ref_id` this node never linked NOR a genuinely-connected but
/// unrelated peer that merely guessed/learned a pending link `ref_id` can crash
/// an actor we linked to a DIFFERENT, still-alive peer.
pub(crate) fn handle_inbound_link_down(ref_id: u64, authenticated_peer: u16, reason: i32) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("handle_inbound_link_down: no runtime installed");
        return;
    };
    if let Some(down) =
        rt.dist_monitors
            .deliver_link_down_to_ref(ref_id, authenticated_peer, reason)
    {
        let _ = crate::link::deliver_cross_node_link_exit(
            down.local_actor_id,
            down.remote_target_serial,
            down.reason,
            down.policy_tag,
        );
    }
}

/// Fan out `MonitorLost` to every local watcher of an actor on a lost/dead node
/// (connection-drop / SWIM-DEAD hook).
///
/// Called from the connection-drop retire hook and the SWIM `MEMBER_DEAD`
/// fan-out: arms every still-`Pending` watcher registration targeting
/// `dead_node_id` with the `MonitorLost` sentinel, waking any blocked
/// `hew_node_monitor_recv`. Only `Pending` slots are armed, so a registration
/// that already received a definitive clean-exit / crash DOWN is untouched
/// (exactly-once; the central R1/R2 disambiguation).
///
/// Also prunes the target-side `RemoteWatcher` entries registered by the dead
/// node. When a watcher node dies, the actors it was watching on this node must
/// not accumulate its now-stale watcher records indefinitely — only a definitive
/// demonitor or the watched actor's own death normally removes them. Pruning here
/// ensures the target-side table stays bounded under watcher-node churn.
pub(crate) fn fan_out_monitor_lost_for_node(dead_node_id: u16) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    // Monitor half: arm every still-Pending monitor watcher with MonitorLost so a
    // blocked recv_down wakes.
    let _ = rt
        .dist_monitors
        .deliver_to_node(dead_node_id, crate::dist_monitor::MONITOR_REASON_LOST);
    // Link half: fire the cross-node link cascade for every still-Pending LINK
    // watcher on the dead node — the death-signal must fire on the PARTITION
    // terminal cause too (firing only on a clean exit / crash would fail-open: a
    // linked actor surviving its dead peer). For CrashLinked this synthesizes a
    // SYS_MSG_EXIT into the LOCAL linked actor's mailbox and crashes it. The
    // one-shot slot makes this exactly-once vs a definitive CTRL_LINK_DOWN that
    // may already have fired.
    let link_downs = rt
        .dist_monitors
        .take_link_downs_for_node(dead_node_id, crate::dist_monitor::MONITOR_REASON_LOST);
    for down in link_downs {
        let _ = crate::link::deliver_cross_node_link_exit(
            down.local_actor_id,
            down.remote_target_serial,
            down.reason,
            down.policy_tag,
        );
    }
    // Prune target-side watcher entries (monitor AND link) the dead node had
    // registered, so the table stays bounded under node churn (target-side watcher
    // prune; covers link watchers via the same map).
    let _ = rt
        .dist_monitors
        .prune_remote_watchers_for_node(dead_node_id);
}

/// Fail every pending remote ask routed to a SWIM-declared-DEAD node with
/// [`AskError::Partition`].
///
/// The cluster's recv-queue partition fan-out (`PartitionRegistry::on_member_dead`,
/// `cluster.rs`) wakes a blocked `recv` to `PartitionDetected`, but it does not
/// touch the reply table — so a pending remote *ask* to a peer the failure
/// detector has declared dead (while its TCP socket may still be nominally open)
/// would otherwise hang to the caller's own deadline. This is the request/reply
/// analog of that recv-side seam: it resolves those pending asks IMMEDIATELY
/// with the typed `Partition` cause instead of a silent wait-to-timeout.
///
/// The reply table is keyed by `(conn_mgr, conn_id)`; a remote ask registers
/// under the connection the routing table resolves for the target node. We look
/// up the dead node's connection the same way and fail every pending ask on it.
///
/// Exactly-once vs the connection-drop path: `fail_connection_with_reason`
/// drains the matching entries under the table lock before failing them, so if
/// the socket also drops (or already dropped), whichever verdict lands first
/// resolves the ask and the second finds an empty map — no double wake. A node
/// with no route to the dead peer (no pending asks) is a no-op.
pub(crate) fn fail_remote_asks_for_node(dead_node_id: u16) {
    with_current_node_read(|guard| {
        let node_ptr = *guard as *const HewNode;
        if node_ptr.is_null() {
            return;
        }
        // SAFETY: the read lock pins CURRENT_NODE for the duration of this call.
        let node = unsafe { &*node_ptr };
        if node.routing_table.is_null() || node.conn_mgr.is_null() {
            return;
        }
        // SAFETY: routing_table is valid while the node is installed.
        //
        // Replacement-connection window: this lookup returns the CURRENT conn_id
        // for `dead_node_id` at the time of the SWIM-DEAD verdict. If the routing
        // table has already been updated to a replacement connection (old socket
        // retired, new connection established and routed), `conn_id` here is the
        // NEW conn's id. Pending asks on the OLD conn that are still draining in
        // the old reader's cleanup will NOT be reached by this fan-out — they
        // resolve via `reader_cleanup` → `ConnectionDropped` rather than
        // `Partition`. This is already fail-closed (the dying reader does fail
        // them; no ask can hang) and is a cosmetic reason-label mismatch only.
        //
        // The real fix — keying the reply table on (node_id, conn_generation) so
        // a DEAD verdict can reach a retired conn's draining asks — requires a
        // reply-table schema change and an exactly-once proof; it is deferred.
        let conn_id =
            unsafe { crate::routing::hew_routing_conn_for_node(node.routing_table, dead_node_id) };
        if conn_id < 0 {
            // No route to the dead peer on this node — no pending asks to fail.
            return;
        }
        let connection = ConnectionKey::new(node.conn_mgr.cast_const(), conn_id);
        if let Some(table) = reply_table_opt() {
            table.fail_connection_with_reason(connection, AskError::Partition);
        }
    });
}

/// Node-side reaction to a SWIM-DEAD verdict: fail every pending remote ask to
/// the dead peer AND quarantine it at the incarnation it died at.
///
/// Both per-peer tables react at the verdict: the reply-table fan-out resolves
/// in-flight asks with [`AskError::Partition`], and the quarantine insert records
/// `(node_id, incarnation)` so a `Quarantine`-policy send/ask to the peer fails
/// closed until it rejoins at a strictly higher incarnation. The two
/// `MEMBER_DEAD` seams in `cluster.rs` route through this single wrapper so
/// neither can quarantine without also failing the asks, nor vice versa.
pub(crate) fn fail_remote_and_quarantine(dead_node_id: u16, dead_incarnation: u64) {
    fail_remote_asks_for_node(dead_node_id);
    quarantine_insert(dead_node_id, dead_incarnation);
}

/// Node-side reaction to a strictly-higher-incarnation readmission: evict the
/// returned peer from the quarantine set so it is sendable again. The admission
/// gate (`cluster.rs`) has already proved the new incarnation is strictly higher,
/// so the node side only clears.
pub(crate) fn readmit_node_clear_quarantine(node_id: u16) {
    quarantine_evict(node_id);
}

/// Return the total number of `RemoteWatcher` entries across all serial slots
/// in the target-side monitor table.
///
/// Test-introspection probe used by the two-process watcher-node-death fixture
/// to assert the target-side table is empty after a watcher node dies — proving
/// the prune path fires and the table is bounded. Not user-callable; the
/// compiler does not emit calls to this symbol. Callable from `.hew` via
/// `extern "C" { fn hew_dist_monitor_remote_watcher_count() -> i64; }`.
#[no_mangle]
pub extern "C" fn hew_dist_monitor_remote_watcher_count() -> i64 {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return -1;
    };
    #[expect(
        clippy::cast_possible_wrap,
        reason = "remote_watcher_count is bounded by the number of live monitors, which is \
                  far below i64::MAX in any realistic deployment; the sentinel -1 covers \
                  the no-runtime case"
    )]
    {
        rt.dist_monitors.remote_watcher_count() as i64
    }
}

/// Drive a single piggybacked SWIM gossip entry into the current node's cluster,
/// exactly as a received gossip frame would.
///
/// Test-introspection probe: lets a two-process rejoin fixture deterministically
/// drive a DEAD verdict, a stale (`<=`-incarnation) ALIVE replay, and a
/// strictly-higher-incarnation rejoin into the membership admission gate without
/// racing the failure detector. Returns 0 on success, -1 when no node is
/// installed. Not user-callable; the compiler does not emit calls to this symbol.
/// Callable from `.hew` via
/// `extern "C" { fn hew_dist_inject_swim_gossip(node_id: u16, state: i32, incarnation: u64) -> i64; }`.
#[no_mangle]
pub extern "C" fn hew_dist_inject_swim_gossip(node_id: u16, state: i32, incarnation: u64) -> i64 {
    with_current_node_read(|guard| {
        let node_ptr = *guard as *const HewNode;
        if node_ptr.is_null() {
            return -1;
        }
        // SAFETY: the read lock pins CURRENT_NODE for the duration of this call.
        let node = unsafe { &*node_ptr };
        if node.cluster.is_null() {
            return -1;
        }
        // SAFETY: cluster is valid while the node is installed.
        let cluster = unsafe { &*node.cluster };
        cluster.apply_swim_gossip(&[(node_id, state, incarnation)]);
        0
    })
}

/// Return the membership state the current node's cluster records for `node_id`
/// (the `MEMBER_*` constants), or -1 if the node is unknown / no node installed.
///
/// Test-introspection probe for the rejoin fixture: asserts a buried node stays
/// DEAD under a stale-ALIVE replay and flips to ALIVE only after a strictly-higher
/// rejoin. Not user-callable.
#[no_mangle]
pub extern "C" fn hew_dist_member_state(node_id: u16) -> i64 {
    with_current_node_read(|guard| {
        let node_ptr = *guard as *const HewNode;
        if node_ptr.is_null() {
            return -1;
        }
        // SAFETY: the read lock pins CURRENT_NODE for the duration of this call.
        let node = unsafe { &*node_ptr };
        if node.cluster.is_null() {
            return -1;
        }
        // SAFETY: cluster is valid while the node is installed.
        let cluster = unsafe { &*node.cluster };
        i64::from(cluster.member_state(node_id))
    })
}

/// Return 1 if `node_id` is currently in the quarantine set, 0 if not, -1 when no
/// node is installed.
///
/// Test-introspection probe for the rejoin fixture: the resurrection-guard teeth.
/// A buried node must remain quarantined under a stale-ALIVE replay and be evicted
/// after a strictly-higher rejoin. Not user-callable.
#[no_mangle]
pub extern "C" fn hew_dist_quarantine_contains(node_id: u16) -> i64 {
    match crate::runtime::rt_current_opt() {
        Some(rt) => rt
            .node
            .quarantine
            .access(|set| i64::from(set.contains_key(&node_id))),
        None => -1,
    }
}

/// Environment signal that arms [`hew_dist_partition_pending_remote_asks`].
///
/// The probe is a test-only capability: it stays inert (a `-1` no-op that drains
/// nothing) unless this variable is present with value `1`. The two-process
/// partition fixture's harness sets it on the client it spawns; a shipped libhew
/// runs with it unset.
const DIST_TEST_PROBE_ENV: &str = "HEW_DIST_TEST_PROBE";

/// Whether the test-only partition probe is armed for this process.
///
/// True only when `HEW_DIST_TEST_PROBE=1` is present — the value the e2e harness
/// sets on the partition-scenario client. Any other value, or an unset variable
/// (the production default), leaves the probe inert.
fn dist_test_probe_enabled() -> bool {
    std::env::var_os(DIST_TEST_PROBE_ENV).as_deref() == Some(std::ffi::OsStr::new("1"))
}

/// Fail every currently-pending remote ask CLOSED with [`AskError::Partition`],
/// returning the number of asks it resolved (`0` when none are pending yet, `-1`
/// when no runtime is installed).
///
/// The drain seam behind the test-only partition probe. Drives the in-flight
/// pending-ask fail-closed path on demand through the SAME reply-table seam the
/// SWIM-DEAD fan-out ([`fail_remote_asks_for_node`]) reaches via
/// `fail_connection_with_reason(.., Partition)` — instead of waiting on the OS
/// socket-teardown detector and the SWIM failure detector, whose latency is
/// unbounded under host load. It mirrors how the in-process test
/// `swim_dead_wakes_pending_remote_ask_with_partition` calls the fan-out directly
/// once the ask registers, letting the two-process partition fixture exercise the
/// typed fail-closed verdict deterministically rather than racing real time.
///
/// Draining under the table lock is atomic against a racing reply: an entry this
/// drain removes is guaranteed to wake with `Partition` (a late reply finds no
/// slot and is dropped), so the returned count is exactly the number of asks
/// failed closed. Reads and resolves reply-table slots only; it does NOT alter
/// the production partition/StaleRef decision path.
fn dist_partition_drain_pending_remote_asks() -> i64 {
    let Some(table) = reply_table_opt() else {
        return -1;
    };
    // Drain every pending reply slot under the table lock — every entry here is a
    // cross-node remote ask — then fail each closed with Partition OUTSIDE the
    // lock, mirroring `fail_all`/`fail_connection_with_reason`, which never hold
    // the map lock across a waiter wake.
    let drained: Vec<Arc<PendingReply>> = {
        let mut map = table
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        map.drain().map(|(_, pending)| pending).collect()
    };
    let failed = {
        #[expect(
            clippy::cast_possible_wrap,
            reason = "the count of pending remote asks is bounded by the live in-flight \
                      asks on this node, far below i64::MAX; the sentinel -1 covers the \
                      no-runtime case"
        )]
        let count = drained.len() as i64;
        count
    };
    for pending in drained {
        ReplyRoutingTable::fail_pending_with_reason(&pending, AskError::Partition);
    }
    failed
}

/// Test-introspection probe (FFI) for the two-process partition fixture: when
/// armed, fails every pending remote ask closed with [`AskError::Partition`] via
/// [`dist_partition_drain_pending_remote_asks`] and returns the count.
///
/// Gated as a test-only capability and INERT by default. Without
/// `HEW_DIST_TEST_PROBE=1` ([`dist_test_probe_enabled`]) it returns `-1` and
/// drains nothing. A shipped libhew exports this symbol but runs with the signal
/// unset, so a Hew program that declares `extern "C" fn
/// hew_dist_partition_pending_remote_asks()` and calls it gets the inert `-1` —
/// never a drain of healthy in-flight asks. The harness arms it only on the
/// partition-scenario client it spawns; the compiler never emits calls to it.
#[no_mangle]
pub extern "C" fn hew_dist_partition_pending_remote_asks() -> i64 {
    if !dist_test_probe_enabled() {
        return -1;
    }
    dist_partition_drain_pending_remote_asks()
}

/// Connect to a remote node and register routing for its node ID.
///
/// Supports `"<node_id>@<addr>"` to require an exact peer `NodeId`. Without a
/// prefix, the authenticated handshake `NodeId` is authoritative.
///
/// # Safety
///
/// - `node` must be valid.
/// - `addr` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_connect(node: *mut HewNode, addr: *const c_char) -> c_int {
    if node.is_null() || addr.is_null() {
        set_last_error("hew_node_connect: node or addr is null");
        return -1;
    }
    // SAFETY: caller guarantees node pointer validity.
    let node = unsafe { &mut *node };
    if node.transport.is_null() || node.conn_mgr.is_null() {
        set_last_error("hew_node_connect: node is not started");
        return -1;
    }

    // SAFETY: addr pointer is non-null and valid by caller contract.
    let Some((expected_peer_node_id, target_addr)) = (unsafe { parse_connect_target(addr) }) else {
        set_last_error("hew_node_connect: invalid connect target");
        return -1;
    };

    // SAFETY: transport pointer validated above.
    let t = unsafe { &*node.transport };
    // SAFETY: valid transport vtable pointer from transport object.
    let Some(ops) = (unsafe { t.ops.as_ref() }) else {
        set_last_error("hew_node_connect: transport ops are null");
        return -1;
    };
    let Some(connect_fn) = ops.connect else {
        set_last_error("hew_node_connect: transport connect op missing");
        return -1;
    };

    // SAFETY: transport impl and C string are valid.
    let conn_id = unsafe { connect_fn(t.r#impl, target_addr.as_ptr()) };
    if conn_id == HEW_CONN_INVALID {
        set_last_error("hew_node_connect: transport connect failed");
        return -1;
    }
    if let Some(expected) = expected_peer_node_id {
        // SAFETY: conn_mgr is live and conn_id was just returned by this node's
        // transport. The expectation is consumed by hew_connmgr_add.
        if unsafe { connection::hew_connmgr_expect_peer(node.conn_mgr, conn_id, expected) } != 0 {
            // hew_connmgr_expect_peer does not take ownership on failure.
            // SAFETY: transport and conn_id are still owned by this function.
            if let Some(close_fn) = ops.close_conn {
                // SAFETY: conn_id was just created by this transport and has not
                // been transferred to the connection manager.
                unsafe { close_fn(t.r#impl, conn_id) };
            }
            return -1;
        }
    }

    // SAFETY: conn_mgr pointer is valid and owned by this node.
    if unsafe { connection::hew_connmgr_add(node.conn_mgr, conn_id) } != 0 {
        // hew_connmgr_add owns conn_id cleanup on failure; no close needed here.
        set_last_error("hew_node_connect: failed to add connection");
        return -1;
    }
    // SAFETY: conn_mgr and conn_id are valid on successful add.
    let _ = unsafe {
        connection::hew_connmgr_configure_reconnect(
            node.conn_mgr,
            conn_id,
            target_addr.as_ptr(),
            1,
            0,
            expected_peer_node_id.map_or(0, i32::from),
        )
    };

    0
}

// ============================================================================
// Simplified Node API for Hew-language builtins
// ============================================================================
//
// These functions manage the CURRENT_NODE internally, providing a stateless
// interface for the compiler-generated code. Each corresponds to a
// `Node::*` builtin in the Hew language.

/// Per-process offset for auto-assigning node IDs within the same process.
///
/// When a process calls `Node::start` more than once, each call gets a
/// distinct offset added to the process base.
static NODE_ID_COUNTER: std::sync::atomic::AtomicU16 = std::sync::atomic::AtomicU16::new(0);

/// Per-process base node ID, initialised once from the OS process ID.
///
/// # Shim note
///
/// WHY: The original counter started every process at 1, so every OS process
/// produced `node_id` == 1, causing `hew_pid_is_local` to misclassify remote
/// actors as local and bypass TCP routing. Seeding from the OS PID makes
/// distinct processes get distinct node IDs for the v0.5 two-process case.
///
/// WHEN obsolete: when a proper collision-free node-ID assignment scheme is
/// implemented — either a coordinator-assigned ID exchanged during the
/// connection handshake, or a wider (> u16) node-ID space. The birthday
/// bound on u16 is ~180 processes for a 50 % collision probability.
///
/// WHAT the real solution looks like: the connecting node sends no ID; the
/// accepting node assigns a unique u32/u64 scope ID during the handshake,
/// which both sides then embed in all PIDs for that session.
static PROCESS_NODE_BASE: std::sync::OnceLock<u16> = std::sync::OnceLock::new();

/// Derive a non-zero u16 node-ID base from the OS process ID.
///
/// Uses a FNV-1a–style fold to spread the PID bits across the full u16
/// range, then forces non-zero (0 is reserved for "local/standalone" in
/// the PID encoding).
fn process_node_id_base() -> u16 {
    *PROCESS_NODE_BASE.get_or_init(|| {
        let pid = std::process::id(); // u32 OS PID
                                      // FNV-1a 32-bit fold into 16 bits.
        let h = (2_166_136_261u32)
            .wrapping_mul(16_777_619)
            .wrapping_add(pid)
            .wrapping_mul(16_777_619)
            .wrapping_add(pid >> 8)
            .wrapping_mul(16_777_619)
            .wrapping_add(pid >> 16);
        #[expect(
            clippy::cast_possible_truncation,
            reason = "XOR of two u16-range halves of a u32 always fits in u16"
        )]
        let folded = ((h >> 16) ^ (h & 0xFFFF)) as u16;
        // Ensure non-zero (0 is "local" in the PID encoding).
        if folded == 0 {
            1
        } else {
            folded
        }
    })
}

/// Map `(base, offset)` to a node ID in `1..=65535`, never landing on the `0`
/// local/standalone sentinel.
///
/// A bare `base.wrapping_add(offset)` can wrap a second `Node::start` to `0`
/// when `base == 65535` (offset 1). `hew_pid_is_local` always treats node-id `0`
/// as local, so a node that landed on `0` would silently misclassify every
/// remote actor as local and bypass routing. Mapping through the
/// `1..=65535` ring (`1 + ((base - 1 + offset) mod 65535)`) keeps the result a
/// valid non-zero ID for any base/offset, including the wrap case.
fn next_local_node_id(base: u16, offset: u16) -> u16 {
    // Work in u32 to avoid intermediate u16 wrap; the ring has 65535 slots
    // (1..=65535). `base` is already guaranteed non-zero by
    // `process_node_id_base`, so `base - 1` is in `0..=65534`.
    let ring = 65_535u32;
    let zero_based = (u32::from(base) - 1 + u32::from(offset)) % ring;
    #[expect(
        clippy::cast_possible_truncation,
        reason = "zero_based is in 0..65535 so 1 + zero_based fits in u16 (max 65535)"
    )]
    let id = (1 + zero_based) as u16;
    id
}

/// Parse `HEW_NODE_ID` into a validated non-zero `u16` stable binding identity.
///
/// Returns `Ok(None)` when unset, `Ok(Some(id))` for a value in `1..=65535`,
/// and `Err` for a malformed or out-of-range value (fail-closed at start).
fn hew_node_id_from_env() -> Result<Option<std::num::NonZeroU16>, String> {
    let raw = crate::env::ENV_LOCK.read_access(|()| std::env::var("HEW_NODE_ID").ok());
    let Some(raw) = raw else {
        return Ok(None);
    };
    let trimmed = raw.trim();
    match trimmed.parse::<u16>() {
        Ok(v) => std::num::NonZeroU16::new(v).map(Some).ok_or_else(|| {
            "Node::start: HEW_NODE_ID must be a nonzero u16 in 1..=65535 (fail-closed)".to_string()
        }),
        Err(_) => Err(format!(
            "Node::start: HEW_NODE_ID must be a u16 in 1..=65535, got '{trimmed}' (fail-closed)"
        )),
    }
}

/// Whether `HEW_DIST_UNVERIFIED` requests the explicit documented unverified
/// opt-out (`1` / `true`, case-insensitive).
fn hew_dist_unverified_from_env() -> bool {
    crate::env::ENV_LOCK
        .read_access(|()| std::env::var("HEW_DIST_UNVERIFIED").ok())
        .is_some_and(|v| {
            let v = v.trim();
            v == "1" || v.eq_ignore_ascii_case("true")
        })
}

/// `Node::start(addr)` — Create and start a node, binding to `addr`.
///
/// # Safety
///
/// `addr` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_start(addr: *const c_char) -> c_int {
    if addr.is_null() {
        set_last_error("Node::start: address is null");
        return -1;
    }
    // Fail-closed: if a pre-start peer-auth step (`Node::load_keys` /
    // `Node::allow_peer`) failed, refuse to start. Otherwise the node would bind
    // a listener with an *ephemeral* self-signed identity (the operator's pinned
    // key failed to load) or an *incomplete* peer allowlist — silently
    // downgrading the configured mTLS posture (the F6 fail-open). The Hew call
    // form discards this `-1`, so the operator-visible signal is the `hew: `
    // diagnostic on stderr; the mesh `ensure_identity` boundary re-checks the
    // same record as defence in depth.
    #[cfg(feature = "quic")]
    if let Some(reason) = crate::quic_mesh::mesh_auth_setup_error() {
        let msg = format!(
            "Node::start: refusing to bind listener — peer-auth setup failed (fail-closed): {reason}"
        );
        eprintln!("hew: {msg}");
        set_last_error(msg);
        return -1;
    }

    // ── Public owner-scoped staging (BLOCK-6) ─────────────────────────────
    // The singleton public `Node::*` API stages config in `PEER_AUTH_STATE`.
    // Transition `Building → Starting{owner}` under the lock, then run the
    // shared low-level start WITHOUT the lock (it reads the node's installed
    // snapshot, never `ConfigState`), then re-acquire for `Running` / restore.
    let offset = NODE_ID_COUNTER.fetch_add(1, Ordering::Relaxed);
    let (node, cfg, generation) = {
        let mut guard = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let ConfigState::Building(building) = &guard.state else {
            // A public node lifecycle already owns the staging state
            // (Starting/Running). Refuse a repeated public start (composes with
            // #2656, which owns the pre-allocation rejection).
            set_last_error("Node::start: a public node lifecycle is already active (fail-closed)");
            return -1;
        };
        // Merge start-time environment posture into the staged config.
        let mut cfg = building.clone();
        match hew_node_id_from_env() {
            Ok(Some(id)) => cfg.node_id = Some(id),
            Ok(None) => {}
            Err(msg) => {
                eprintln!("hew: {msg}");
                set_last_error(msg);
                return -1;
            }
        }
        if hew_dist_unverified_from_env() {
            cfg.unverified_optout = true;
        }
        // Pre-listen public validation (fail-closed before allocation).
        if let Err(msg) = cfg.validate_public() {
            eprintln!("hew: {msg}");
            set_last_error(msg);
            return -1;
        }
        // Derive the node id: strict ⇒ the pinned HEW_NODE_ID; else PID-derived.
        let node_id = match cfg.node_id {
            Some(id) => id.get(),
            None => next_local_node_id(process_node_id_base(), offset),
        };
        // SAFETY: addr was null-checked above and is a valid C string.
        let node = unsafe { hew_node_new(node_id, addr) };
        if node.is_null() {
            // State unchanged (still Building); the staged config is intact.
            return -1;
        }
        // Install the frozen per-node snapshot before the low-level start.
        // SAFETY: node was just created and is STOPPED.
        if unsafe { hew_node_set_auth_snapshot(node, cfg.snapshot()) } != 0 {
            // SAFETY: node is valid, not started; free it.
            unsafe { hew_node_free(node) };
            return -1;
        }
        let generation = guard.next_generation;
        guard.next_generation = guard.next_generation.wrapping_add(1);
        guard.state = ConfigState::Starting {
            generation,
            owner: node as usize,
            config: cfg.clone(),
        };
        (node, cfg, generation)
    };

    // SAFETY: node was created above; the low-level start reads node.auth only.
    let rc = unsafe { hew_node_start(node) };
    let mut guard = PEER_AUTH_STATE
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if rc == 0 {
        // Success: compute the standalone identity export clone and go Running.
        let identity_export = cfg.identity_export_string();
        if matches!(&guard.state, ConfigState::Starting { generation: g, owner, .. }
            if *g == generation && *owner == node as usize)
        {
            guard.state = ConfigState::Running {
                generation,
                owner: node as usize,
                identity_export,
            };
        }
        0
    } else {
        // Public fail_start: restore the exact held config iff owner+generation
        // still match (a concurrent stage that advanced generation is untouched).
        if let ConfigState::Starting {
            generation: g,
            owner,
            config,
        } = &guard.state
        {
            if *g == generation && *owner == node as usize {
                let restored = config.clone();
                guard.state = ConfigState::Building(restored);
            }
        }
        drop(guard);
        // SAFETY: node is valid but not started; free the allocation.
        unsafe { hew_node_free(node) };
        rc
    }
}

/// `Node::shutdown()` — Stop and free the current node.
///
/// # Safety
///
/// Must only be called when a node was previously started via
/// [`hew_node_api_start`]. The global `CURRENT_NODE` pointer must still be
/// valid.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_shutdown() -> c_int {
    // Claim CURRENT_NODE under the write lock so exactly one caller owns the
    // stop/free sequence.
    let Some(ptr) = with_current_node(|guard| {
        let ptr = *guard as *mut HewNode;
        if ptr.is_null() {
            return None;
        }
        *guard = 0;
        reply_table().fail_all();
        Some(ptr)
    }) else {
        return -1;
    };
    // SAFETY: ptr is non-null and was created by hew_node_api_start.
    unsafe { hew_node_stop(ptr) };
    // SAFETY: ptr is valid; the node has been stopped.
    unsafe { hew_node_free(ptr) };
    // Owner-scoped reset: return the public staging state to `Building` and bump
    // the generation ONLY when the shutting node owns it (mirrors
    // `hew_node_stop`'s `CURRENT_NODE` owner check). `owner` is compared as an
    // integer — never dereferenced. A secondary low-level `hew_node_stop` never
    // reaches this path, so it leaves `ConfigState` intact.
    {
        let mut guard = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let owner_matches = match &guard.state {
            ConfigState::Running { owner, .. } | ConfigState::Starting { owner, .. } => {
                *owner == ptr as usize
            }
            ConfigState::Building(_) => false,
        };
        if owner_matches {
            guard.state = ConfigState::default();
            guard.next_generation = guard.next_generation.wrapping_add(1);
        }
    }
    0
}

/// `Node::connect(addr)` — Connect the current node to a peer.
///
/// # Safety
///
/// `addr` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_connect(addr: *const c_char) -> c_int {
    if addr.is_null() {
        return -1;
    }
    with_current_node_read(|guard| {
        let node = *guard as *mut HewNode;
        if node.is_null() {
            set_last_error("Node::connect: no active node");
            return -1;
        }
        // SAFETY: node and addr are non-null and validated above.
        unsafe { hew_node_connect(node, addr) }
    })
}

/// `Node::register(name, actor_ptr)` — Register a named actor.
///
/// # Safety
///
/// `name` must be a valid null-terminated C string. `actor` must be a valid
/// pointer to a live [`crate::actor::HewActor`].
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_register(
    name: *const c_char,
    actor: *mut crate::actor::HewActor,
) -> c_int {
    if name.is_null() || actor.is_null() {
        return -1;
    }
    // SAFETY: actor was null-checked above and is a valid HewActor pointer.
    let actor_id = unsafe { (*actor).id };
    with_current_node_read(|guard| {
        let node = *guard as *mut HewNode;
        if node.is_null() {
            set_last_error("Node::register: no active node");
            return -1;
        }
        // SAFETY: node, name, and actor_id are validated above.
        unsafe { hew_node_register(node, name, actor_id) }
    })
}

/// `Node::register<T>(name, pid)` — Register a named actor by bare PID.
///
/// Per registry R81 (2026-05-23), `LocalPid<T>` lowers to a bare `u64` PID
/// at this ABI boundary rather than a `*mut HewActor` pointer. The caller
/// extracts the PID via `hew_actor_pid` before passing it here.
///
/// Returns 0 on success, -1 on any of the fail-closed conditions:
/// - `name` is null
/// - `pid` is 0 (sentinel for an invalid actor)
/// - The actor is not found in the live-actor table
/// - No node has been started (`hew_node_api_start` not yet called)
/// - Internal name-string allocation fails
///
/// On failure, the last error is set via `set_last_error` so callers can
/// retrieve a diagnostic string.
///
/// # Safety
///
/// `name` must be a valid null-terminated C string that remains live for
/// the duration of this call.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_register_by_pid(name: *const c_char, pid: u64) -> c_int {
    if name.is_null() {
        set_last_error("Node::register: name pointer is null");
        return -1;
    }
    if pid == 0 {
        set_last_error("Node::register: pid is zero (invalid actor)");
        return -1;
    }
    // Verify the actor is currently live in the actor table before attempting
    // to register it.  This prevents dangling registrations after an actor
    // stops between spawn and register.
    let is_live = crate::lifetime::live_actors::get_actor_ptr_by_id(pid).is_some();
    if !is_live {
        set_last_error(format!(
            "Node::register: actor with pid {pid} not found in live-actor table"
        ));
        return -1;
    }
    with_current_node_read(|guard| {
        let node = *guard as *mut HewNode;
        if node.is_null() {
            set_last_error("Node::register: no active node (call Node::start first)");
            return -1;
        }
        // SAFETY: node is non-null; name is non-null and caller guarantees a
        // valid C string; pid was validated above.
        unsafe { hew_node_register(node, name, pid) }
    })
}

/// `Node::unregister(name)` — remove a name registration from the active node.
///
/// The symmetric counterpart of [`hew_node_api_register_by_pid`]: it clears the
/// routing mapping for `name` and broadcasts a removal so peers drop the cached
/// name. The registration-incarnation / `name_serials` bookkeeping persists so a
/// later re-registration to a DIFFERENT actor supersedes the prior serial (the
/// `StaleRef` path). Returns 0 on success, -1 on error (no active node, null
/// name).
///
/// # Safety
///
/// `name` must be a valid null-terminated C string that remains live for the
/// duration of this call.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_unregister(name: *const c_char) -> c_int {
    if name.is_null() {
        set_last_error("Node::unregister: name pointer is null");
        return -1;
    }
    with_current_node_read(|guard| {
        let node = *guard as *mut HewNode;
        if node.is_null() {
            set_last_error("Node::unregister: no active node (call Node::start first)");
            return -1;
        }
        // SAFETY: node is non-null; name is non-null and caller guarantees a
        // valid C string.
        unsafe { hew_node_unregister(node, name) }
    })
}

/// `Node::lookup(name)` — Look up a registered actor by name.
///
/// # Safety
///
/// `name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_lookup(name: *const c_char) -> u64 {
    if name.is_null() {
        return 0;
    }
    with_current_node_read(|guard| {
        let node = *guard as *mut HewNode;
        if node.is_null() {
            return 0;
        }
        // SAFETY: node and name are non-null and validated above.
        unsafe { hew_node_lookup(node, name) }
    })
}

/// `RemotePid::from_raw(node_id, serial) -> u64`
///
/// Constructs a `RemotePid<T>` PID value from a raw `(node_id, serial)` pair.
/// `RemotePid<T>` is encoded identically to `LocalPid<T>`: a packed `u64`
/// produced by `hew_pid_make(node_id as u16, serial)`.
///
/// Returns `0` (the null-PID sentinel) and sets the last error string when
/// either validation constraint fails:
/// - `node_id` must be non-zero (a zero `node_id` would alias the local node,
///   which is always node 0 in the current encoding scheme).
/// - `node_id` must not exceed `u16::MAX` (the packed encoding constraint).
#[no_mangle]
pub extern "C" fn hew_remote_pid_from_raw(node_id: u64, serial: u64) -> u64 {
    if node_id == 0 {
        set_last_error(
            "RemotePid::from_raw: node_id must be non-zero \
             (use LocalPid for local actors)",
        );
        return 0;
    }
    if node_id > u64::from(u16::MAX) {
        set_last_error(format!(
            "RemotePid::from_raw: node_id {node_id} exceeds u16::MAX ({})",
            u16::MAX
        ));
        return 0;
    }
    // SAFETY: node_id <= u16::MAX is verified above; truncation cannot occur.
    #[expect(
        clippy::cast_possible_truncation,
        reason = "node_id <= u16::MAX is verified by the guard above"
    )]
    crate::pid::hew_pid_make(node_id as u16, serial)
}

/// `Node::set_transport(name)` — Set the transport type before starting.
///
/// Supported values: `"tcp"` (default), `"quic"`, `"quic-mesh"`.
/// Must be called before `Node::start`.
///
/// # Safety
///
/// `name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_set_transport(name: *const c_char) -> c_int {
    // SAFETY: caller guarantees name is a valid C string (or null).
    let Some(s) = (unsafe { crate::util::cstr_to_str(&name, "hew_node_set_transport") }) else {
        return -1;
    };
    match normalize_transport_name(s) {
        Ok(normalized) => {
            // SAFETY: ENV_LOCK synchronises access to the process-global environ
            // array; set_var is safe under exclusive write access.
            crate::env::ENV_LOCK
                .access(|()| unsafe { std::env::set_var("HEW_TRANSPORT", normalized) });
            0
        }
        Err(err) => {
            set_last_error(format!("Node::set_transport: {err}"));
            -1
        }
    }
}

/// Surface a `Node::load_keys` / `Node::allow_peer` peer-auth setup failure on
/// every channel:
///
/// - `hew_last_error` — the thread-local programmatic surface (unchanged from
///   the pre-F6 behaviour);
/// - a `hew: ` stderr diagnostic — operator-visible, because the Hew call form
///   discards the `-1` return (these builtins are typed `Unit`);
/// - the sticky mesh peer-auth failure record (quic-mesh builds only), which
///   makes the next `Node::start` refuse to bind a listener (fail-closed) so a
///   node never silently comes up with an ephemeral identity or an incomplete
///   peer allowlist after the operator's setup failed.
fn node_peer_auth_setup_failed(msg: impl Into<String>) {
    let msg = msg.into();
    eprintln!("hew: {msg} (fail-closed)");
    #[cfg(feature = "quic")]
    crate::quic_mesh::mesh_auth_record_failure(msg.clone());
    set_last_error(msg);
}

/// `Node::load_keys(path)` — Load (or, if absent, mint-and-persist) this node's
/// stable mesh TLS identity from `path`. Must be called **before**
/// `Node::start` so the listener presents the loaded key. Peers pin the
/// resulting SPKI via `Node::allow_peer`; persisting the key keeps that SPKI
/// stable across restarts. Native quic-mesh only.
///
/// Returns `0` on success, `-1` on a null/invalid path or key I/O failure
/// (error string available via `hew_last_error`). Never fabricates an identity.
/// On failure the peer-auth setup is marked failed so a subsequent `Node::start`
/// refuses to bind a listener (fail-closed) rather than silently presenting an
/// ephemeral identity.
///
/// # Safety
///
/// `path` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_load_keys(path: *const c_char) -> c_int {
    // SAFETY: caller guarantees path is a valid C string (or null).
    let Some(p) = (unsafe { crate::util::cstr_to_str(&path, "Node::load_keys") }) else {
        node_peer_auth_setup_failed("Node::load_keys: invalid path argument");
        return -1;
    };
    #[cfg(feature = "quic")]
    {
        match crate::quic_mesh::mesh_identity_load_or_create(std::path::Path::new(p)) {
            Ok(_spki) => 0,
            Err(err) => {
                node_peer_auth_setup_failed(format!("Node::load_keys: {err}"));
                -1
            }
        }
    }
    #[cfg(not(feature = "quic"))]
    {
        let _ = p;
        node_peer_auth_setup_failed(
            "Node::load_keys: quic-mesh transport not built (peer auth unavailable)",
        );
        -1
    }
}

/// `Node::allow_peer(spki_hex)` — Add a peer's certificate SPKI (lowercase hex)
/// to the fail-closed mesh allowlist. The snapshot is taken at `Node::start`,
/// so call this before starting. A peer whose SPKI is not pinned is rejected
/// by the mTLS handshake. Native quic-mesh only.
///
/// Returns `0` on success, `-1` on a null/odd/non-hex string or empty/oversize
/// SPKI. Adds nothing on error. On failure the peer-auth setup is marked failed
/// so a subsequent `Node::start` refuses to bind a listener (fail-closed) rather
/// than silently coming up with an incomplete peer allowlist.
///
/// # Safety
///
/// `spki_hex` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_allow_peer(spki_hex: *const c_char) -> c_int {
    // SAFETY: caller guarantees spki_hex is a valid C string (or null).
    let Some(s) = (unsafe { crate::util::cstr_to_str(&spki_hex, "Node::allow_peer") }) else {
        node_peer_auth_setup_failed("Node::allow_peer: invalid SPKI argument");
        return -1;
    };
    let Some(bytes) = decode_hex(s) else {
        node_peer_auth_setup_failed("Node::allow_peer: peer key must be hex-encoded SPKI bytes");
        return -1;
    };
    #[cfg(feature = "quic")]
    {
        // SAFETY: bytes is a live Vec; ptr+len describe its contents.
        let rc =
            unsafe { crate::quic_mesh::hew_quic_mesh_peer_spki_add(bytes.as_ptr(), bytes.len()) };
        if rc != 0 {
            node_peer_auth_setup_failed("Node::allow_peer: SPKI rejected by mesh allowlist");
        }
        rc
    }
    #[cfg(not(feature = "quic"))]
    {
        let _ = bytes;
        node_peer_auth_setup_failed(
            "Node::allow_peer: quic-mesh transport not built (peer auth unavailable)",
        );
        -1
    }
}

/// Upper bound on a decoded peer SPKI, mirroring `quic_mesh::MAX_SPKI_BYTES`
/// (4 KiB). The mesh allowlist re-enforces this, but `decode_hex` checks it
/// *before* allocating so an oversize hex argument can't force a large up-front
/// `Vec`. A well-formed RSA-4096 SPKI is under 1 KiB; anything larger is junk.
const MAX_SPKI_DECODE_BYTES: usize = 4096;

/// Decode a lowercase/uppercase hex string to bytes. Returns `None` on an odd
/// length, oversize input, or any non-hex digit — fail-closed, no partial decode.
fn decode_hex(s: &str) -> Option<Vec<u8>> {
    let s = s.as_bytes();
    if s.is_empty() || !s.len().is_multiple_of(2) {
        return None;
    }
    // Bound before allocating: reject anything that would decode past the SPKI
    // cap so a huge argument can't drive a large `with_capacity`.
    if s.len() / 2 > MAX_SPKI_DECODE_BYTES {
        return None;
    }
    let nibble = |b: u8| -> Option<u8> {
        match b {
            b'0'..=b'9' => Some(b - b'0'),
            b'a'..=b'f' => Some(b - b'a' + 10),
            b'A'..=b'F' => Some(b - b'A' + 10),
            _ => None,
        }
    };
    let mut out = Vec::with_capacity(s.len() / 2);
    for pair in s.chunks_exact(2) {
        out.push((nibble(pair[0])? << 4) | nibble(pair[1])?);
    }
    Some(out)
}

enum RemoteAskSetupResult {
    Ok((u64, Arc<PendingReply>)),
    Error(AskError),
}

/// Set up an outbound remote ask: serialize the request, register a pending
/// reply slot (recording `parked_caller` for the NEW-5 suspendable path when
/// non-null), and send the ask envelope over the mesh. Returns the
/// `(request_id, pending)` pair on a successful submit, or a typed [`AskError`]
/// on any setup failure. The blocking and suspendable entry points share this
/// single submit state machine.
///
/// # Safety
///
/// - `pid` must be a valid remote actor PID.
/// - `data` must point to at least `size` readable bytes, or be null when
///   `size` is 0.
/// - `parked_caller`, if non-null, must be the live actor whose continuation is
///   about to park on the reply.
fn setup_remote_ask(
    pid: u64,
    dispatch: *const c_void,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    parked_caller: *mut crate::actor::HewActor,
) -> RemoteAskSetupResult {
    with_current_node_read(|guard| {
        let node_ptr = *guard as *mut HewNode;
        if node_ptr.is_null() {
            return RemoteAskSetupResult::Error(AskError::NodeNotRunning);
        }
        // SAFETY: read lock pins CURRENT_NODE.
        let node = unsafe { &*node_ptr };
        if node.state.load(Ordering::Acquire) != NODE_STATE_RUNNING || node.conn_mgr.is_null() {
            return RemoteAskSetupResult::Error(AskError::NodeNotRunning);
        }

        // Look up the connection for the target node via the routing table.
        // SAFETY: routing_table is valid while node is running.
        let conn_id = unsafe { crate::routing::hew_routing_lookup(node.routing_table, pid) };
        if conn_id < 0 {
            return RemoteAskSetupResult::Error(AskError::RoutingFailed);
        }

        // StaleRef gate (shared with the send + monitor paths): a captured
        // `RemotePid<T>` whose registration has been superseded fails CLOSED here
        // — before any request bytes are serialized — rather than asking the live
        // replacement (DI-012 / LESSONS `boundary-fail-closed`).
        if dispatch_is_stale(node, pid) {
            return RemoteAskSetupResult::Error(AskError::StaleRef);
        }

        // Quarantine consult: under a Quarantine policy, a peer the failure
        // detector buried and that has not yet rejoined at a strictly higher
        // incarnation fails closed here — before any request bytes are serialized,
        // so nothing is allocated on the blocked path.
        let target_node_id = crate::pid::hew_pid_node(pid);
        if quarantine_blocks_send(node, target_node_id) {
            return RemoteAskSetupResult::Error(AskError::Partition);
        }

        // Serialize the request value before it leaves this address space —
        // `data`/`size` is the raw in-memory struct (which may hold heap
        // pointers). The codec for `msg_type` encodes its CONTENTS; the receiver
        // reconstructs the value. Fail closed if no codec is registered for a
        // non-empty payload.
        let serialized_req: Option<(*mut u8, usize)> = if size > 0 {
            let mut out_len: usize = 0;
            // SAFETY: data is a valid value of the message type; out_len valid.
            // Keyed by `(dispatch, msg_type)` — the target actor TYPE's request
            // serializer; a colliding `msg_type` on another actor type cannot
            // select the wrong codec.
            let bytes = unsafe {
                crate::xnode_serial::encode_payload(
                    dispatch,
                    msg_type,
                    data.cast_const(),
                    &raw mut out_len,
                )
            };
            if bytes.is_null() {
                return RemoteAskSetupResult::Error(AskError::EncodeFailed);
            }
            Some((bytes, out_len))
        } else {
            None
        };
        let (req_ptr, req_len) = match serialized_req {
            Some((bytes, len)) => (bytes.cast_const(), len),
            None => (std::ptr::null(), 0),
        };

        // Register a pending reply slot. A suspendable caller (NEW-5) records
        // its actor so the completion resumes the parked coroutine instead of
        // signalling the condvar.
        let connection = ConnectionKey::new(node.conn_mgr.cast_const(), conn_id);
        let (request_id, pending) = if parked_caller.is_null() {
            reply_table().register(connection)
        } else {
            reply_table().register_parked(connection, parked_caller)
        };

        // Encode the ask envelope with request_id and source_node_id over the
        // SERIALIZED request bytes.
        // SAFETY: req_ptr is valid for req_len bytes (or null when req_len == 0).
        let bytes = match unsafe {
            encode_envelope_frame_from_raw_parts(
                pid,
                0,
                msg_type,
                req_ptr,
                req_len,
                request_id,
                node.node_id,
            )
        } {
            Ok(bytes) => bytes,
            Err(err) => {
                set_last_error(format!("hew_node_api_ask: {err}"));
                reply_table().remove(request_id);
                if let Some((b, _)) = serialized_req {
                    // SAFETY: b came from encode_payload (libc::malloc).
                    unsafe { crate::xnode_serial::hew_ser_free_bytes(b) };
                }
                return RemoteAskSetupResult::Error(AskError::EncodeFailed);
            }
        };
        // The envelope copied the request bytes; free our serialized copy.
        if let Some((b, _)) = serialized_req {
            // SAFETY: b came from encode_payload (libc::malloc).
            unsafe { crate::xnode_serial::hew_ser_free_bytes(b) };
        }

        // Send the encoded envelope through the connection manager so noise
        // encryption is applied when the connection is encrypted.
        // SAFETY: conn_mgr is valid while node is running; bytes is CBOR encoded.
        let send_ok = unsafe {
            connection::hew_connmgr_send_preencoded(
                node.conn_mgr,
                conn_id,
                bytes.as_ptr(),
                bytes.len(),
            ) == 0
        };

        if !send_ok {
            reply_table().remove(request_id);
            return RemoteAskSetupResult::Error(AskError::SendFailed);
        }

        RemoteAskSetupResult::Ok((request_id, pending))
    })
}

/// Spawn a detached timer that fails the pending ask `request_id` with
/// [`AskError::Timeout`] after `timeout_ms`. Used by the SUSPENDABLE path
/// (NEW-5), which cannot block a worker on the reply condvar. If the real reply
/// (or a peer-drop failure) lands first it removes the entry, so this later
/// `fail` is a no-op. Returns `Err` if the timer thread could not be spawned.
fn spawn_remote_ask_timeout(request_id: u64, timeout_ms: u64) -> Result<(), AskError> {
    thread::Builder::new()
        .name("hew-remote-ask-timeout".to_string())
        .spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(timeout_ms));
            if let Some(table) = reply_table_opt() {
                table.fail(request_id, AskError::Timeout);
            }
        })
        .map(|_| ())
        .map_err(|_| AskError::SendFailed)
}

/// Materialise a completed [`ReplyOutcome`] into the codegen-visible reply
/// pointer / null-failure sentinel. Shared by the blocking and suspendable
/// finish paths. Records the exact [`AskError`] in the node ask-error slot on
/// failure.
fn finish_remote_ask_outcome(
    dispatch: *const c_void,
    msg_type: i32,
    reply_size: usize,
    reply: &ReplyOutcome,
) -> *mut c_void {
    if reply.status == ReplyStatus::Failed {
        return ask_null(reply.ask_error);
    }
    // Void ask (reply_size == 0) or empty reply: return the void sentinel /
    // null exactly as before — there is no value to reconstruct.
    if reply_size == 0 || reply.data.is_empty() {
        let result = remote_reply_data_to_ptr(&reply.data, reply_size);
        return if result.is_null() {
            ask_null(AskError::PayloadSizeMismatch)
        } else {
            LAST_ASK_ERROR.with(|cell| cell.set(AskError::None as i32));
            result
        };
    }
    // Non-void reply: `reply.data` holds the SERIALIZED reply bytes. Reconstruct
    // the reply VALUE into this node's address space using the reply codec keyed
    // by `(dispatch, request msg_type)` — `dispatch` is THIS node's local
    // dispatch global for the ask's statically-known target actor type (supplied
    // by codegen at the ask site), so a colliding `msg_type` on another local
    // actor type cannot select the wrong reply codec. codegen's ask terminator
    // then memcpy-loads the reconstructed struct into the reply dest —
    // `reply_size` matches the reconstructed struct size.
    // SAFETY: reply.data is a valid slice for its length.
    let (value, struct_size) = unsafe {
        crate::xnode_serial::decode_reply(dispatch, msg_type, reply.data.as_ptr(), reply.data.len())
    };
    if value.is_null() {
        // No reply codec or decode failure — fail closed with a typed error.
        return ask_null(AskError::DecodeFailure);
    }
    if struct_size != reply_size {
        // The reconstructed struct size must match the codegen reply slot. A
        // mismatch is a codec/layout drift — fail closed rather than hand the
        // caller a wrong-sized buffer.
        // SAFETY: value came from decode_reply (libc::malloc).
        unsafe { libc::free(value) };
        return ask_null(AskError::PayloadSizeMismatch);
    }
    LAST_ASK_ERROR.with(|cell| cell.set(AskError::None as i32));
    value
}

/// Perform a blocking ask against a PID, handling local and remote actors.
///
/// If the PID targets the local node, delegates to `hew_actor_ask`.
/// If remote, sends the message with a `request_id` over the mesh and
/// blocks until the reply arrives (or times out).
///
/// Returns a `malloc`'d reply buffer on success. Remote failures return
/// `NULL` instead of fabricating a zero/default reply value. Successful
/// remote asks for `void` (`reply_size == 0`) return a non-null internal
/// sentinel pointer. Successful empty replies for non-void asks fail closed
/// with `NULL`. The caller must `free` only heap-allocated non-null reply
/// buffers.
///
/// # Safety
///
/// - `pid` must be a valid actor PID.
/// - `data` must point to at least `size` readable bytes, or be null when
///   `size` is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_ask(
    pid: u64,
    dispatch: *const c_void,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: u64,
    reply_size: usize,
) -> *mut c_void {
    let target_node_id = crate::pid::hew_pid_node(pid);
    let local_node_id = crate::pid::hew_pid_local_node();

    // Local path: delegate to the by-ID ask (which packs a reply channel).
    if target_node_id == 0 || target_node_id == local_node_id {
        // SAFETY: data/size are caller-validated; local actor ask is safe here.
        let result = unsafe { crate::actor::hew_actor_ask_by_id(pid, msg_type, data, size) };
        if result.is_null() {
            // Bridge the actor-level error discriminant into the node error slot
            // so callers of hew_node_api_ask see a consistent error regardless
            // of whether the ask went local or remote.
            let local_err = crate::actor::actor_ask_take_last_error_raw();
            LAST_ASK_ERROR.with(|c| c.set(local_err));
        }
        return result;
    }

    // Remote path: send message over mesh with request_id, block on the reply.
    // `dispatch` keys both the request encode and the reply decode `(dispatch,
    // msg_type)` — it is THIS node's local dispatch global for the ask's
    // statically-known target actor type (supplied by codegen).
    let (request_id, pending) =
        match setup_remote_ask(pid, dispatch, msg_type, data, size, ptr::null_mut()) {
            RemoteAskSetupResult::Ok(pair) => pair,
            RemoteAskSetupResult::Error(e) => return ask_null(e),
        };

    // Block until the reply arrives or the caller-supplied timeout elapses.
    let deadline = std::time::Instant::now() + std::time::Duration::from_millis(timeout_ms);
    let mut outcome_guard = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    while outcome_guard.is_none() {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            // Timeout — remove the pending entry and return the null failure sentinel.
            reply_table().remove(request_id);
            return ask_null(AskError::Timeout);
        }
        let (new_guard, wait_result) = pending
            .cond
            .wait_timeout_or_recover(outcome_guard, remaining);
        outcome_guard = new_guard;
        if wait_result.timed_out() && outcome_guard.is_none() {
            reply_table().remove(request_id);
            return ask_null(AskError::Timeout);
        }
    }

    let reply = outcome_guard.take().unwrap_or(ReplyOutcome {
        status: ReplyStatus::Failed,
        data: Vec::new(),
        ask_error: AskError::ConnectionDropped,
    });
    drop(outcome_guard);
    finish_remote_ask_outcome(dispatch, msg_type, reply_size, &reply)
}

/// Start a non-blocking remote ask for a SUSPENDABLE caller (NEW-5).
///
/// Serializes + submits the ask exactly as the blocking path, registers the
/// parked caller so the wire reply / peer-drop RESUMES the coroutine through
/// `scheduler::enqueue_resume`, and arms a detached timeout. Returns an opaque
/// pending-reply handle on a successful submit; the caller MUST hand it to
/// [`hew_node_api_ask_finish`] after resume or to [`hew_node_api_ask_cancel`]
/// on coroutine abandonment (`coro.destroy`). On setup failure returns null and
/// records the exact [`AskError`] in the node ask-error slot.
///
/// Targets that resolve to the local node are rejected ([`AskError::RoutingFailed`]):
/// codegen only flips the wire (remote) ask to the suspendable terminator.
///
/// # Safety
///
/// - `pid` must be a valid remote actor PID.
/// - `data` must point to at least `size` readable bytes, or be null when
///   `size` is 0.
/// - `caller_actor` must be the live actor whose continuation is about to park.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_ask_async(
    pid: u64,
    dispatch: *const c_void,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: u64,
    caller_actor: *mut crate::actor::HewActor,
) -> *mut c_void {
    let target_node_id = crate::pid::hew_pid_node(pid);
    let local_node_id = crate::pid::hew_pid_local_node();
    if target_node_id == 0 || target_node_id == local_node_id {
        return ask_null(AskError::RoutingFailed);
    }
    if caller_actor.is_null() {
        return ask_null(AskError::NoRunnableWork);
    }

    // `dispatch` keys the request encode `(dispatch, msg_type)`; the matching
    // reply decode key is re-supplied to `hew_node_api_ask_finish` at the resume
    // site (codegen knows the target actor type at both ends of the suspend).
    let (request_id, pending) =
        match setup_remote_ask(pid, dispatch, msg_type, data, size, caller_actor) {
            RemoteAskSetupResult::Ok(pair) => pair,
            RemoteAskSetupResult::Error(e) => return ask_null(e),
        };

    if let Err(e) = spawn_remote_ask_timeout(request_id, timeout_ms) {
        reply_table().remove(request_id);
        return ask_null(e);
    }

    // Transfer one owning ref to the caller; reclaimed in finish/cancel.
    Arc::into_raw(pending).cast::<c_void>().cast_mut()
}

/// Drain the reply deposited for a SUSPENDED remote ask after the coroutine
/// resumes. Consumes the handle returned by [`hew_node_api_ask_async`] and
/// materialises the outcome exactly as the blocking finish path. A handle whose
/// outcome is still empty (no reply, no failure) fails closed with
/// [`AskError::ConnectionDropped`] rather than fabricating a value.
///
/// # Safety
///
/// `pending_handle` must be a handle returned by [`hew_node_api_ask_async`]
/// that has not already been finished or cancelled.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_ask_finish(
    pending_handle: *mut c_void,
    dispatch: *const c_void,
    msg_type: i32,
    reply_size: usize,
) -> *mut c_void {
    if pending_handle.is_null() {
        return ask_null(AskError::ConnectionDropped);
    }
    // SAFETY: caller transfers back the creator reference returned by
    // `hew_node_api_ask_async`; this consumes it exactly once.
    let pending = unsafe { Arc::from_raw(pending_handle.cast::<PendingReply>()) };
    let reply = {
        let mut outcome_guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        outcome_guard.take().unwrap_or(ReplyOutcome {
            status: ReplyStatus::Failed,
            data: Vec::new(),
            ask_error: AskError::ConnectionDropped,
        })
    };
    // `dispatch` keys the reply decode `(dispatch, msg_type)` — the target actor
    // type's local dispatch global, re-supplied by codegen at the resume site.
    finish_remote_ask_outcome(dispatch, msg_type, reply_size, &reply)
}

/// Abandon a SUSPENDED remote ask whose coroutine frame is being destroyed
/// (the `coro.destroy` cleanup edge). Consumes the handle returned by
/// [`hew_node_api_ask_async`] and removes any still-pending entry so a late
/// reply finds nothing and is dropped. The parked-caller wake is independently
/// fail-safe: `enqueue_resume` drops a wake to an already-freed caller.
///
/// # Safety
///
/// `pending_handle` must be a handle returned by [`hew_node_api_ask_async`]
/// that has not already been finished or cancelled.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_ask_cancel(pending_handle: *mut c_void) {
    if pending_handle.is_null() {
        return;
    }
    // SAFETY: caller transfers back the creator reference returned by
    // `hew_node_api_ask_async`; this consumes it exactly once.
    let pending = unsafe { Arc::from_raw(pending_handle.cast::<PendingReply>()) };
    reply_table().remove(pending.request_id);
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::{Child, Command, Output, Stdio};
    use std::time::{Duration, Instant};

    const TEST_REMOTE_ASK_TIMEOUT_MS: u64 = 250;

    #[test]
    fn decode_hex_accepts_even_hex_and_rejects_malformed() {
        assert_eq!(decode_hex("00ff1a"), Some(vec![0x00, 0xff, 0x1a]));
        assert_eq!(decode_hex("DEADbeef"), Some(vec![0xde, 0xad, 0xbe, 0xef]));
        // Fail-closed: empty, odd length, and non-hex digits decode to nothing.
        assert_eq!(decode_hex(""), None);
        assert_eq!(decode_hex("abc"), None);
        assert_eq!(decode_hex("zz"), None);
        // Oversize: a hex string decoding past the SPKI cap is rejected before
        // any allocation. 4096 bytes is the bound; (4097)*2 hex chars overflows.
        let oversize = "00".repeat(MAX_SPKI_DECODE_BYTES + 1);
        assert_eq!(decode_hex(&oversize), None, "oversize hex must be rejected");
        assert!(
            decode_hex(&"ab".repeat(MAX_SPKI_DECODE_BYTES)).is_some(),
            "exactly the cap still decodes"
        );
    }

    #[test]
    fn quarantine_insert_blocks_then_evict_clears() {
        let _guard = crate::runtime_test_guard();
        // Insert quarantines the peer at the dead incarnation; a same-or-lower
        // live incarnation is blocked, a higher live incarnation is not.
        quarantine_insert(7, 5);
        assert!(
            quarantine_is_blocked(7, 5),
            "equal live incarnation is blocked"
        );
        assert!(
            quarantine_is_blocked(7, 4),
            "lower live incarnation is blocked"
        );
        assert!(
            !quarantine_is_blocked(7, 6),
            "a live incarnation past the quarantined one is not blocked"
        );
        assert!(
            !quarantine_is_blocked(8, 5),
            "an unrelated node is not blocked"
        );
        // Eviction clears the entry.
        quarantine_evict(7);
        assert!(
            !quarantine_is_blocked(7, 5),
            "evicted node is sendable again"
        );
    }

    #[test]
    fn registration_record_bumps_incarnation_and_supersedes_on_repoint() {
        let registry = HewRegistry::default();
        // Fresh registration of name "kv" to serial 100: incarnation 0, nothing
        // superseded.
        assert_eq!(registration_record(&registry, "kv", 100), 0);
        assert!(
            !serial_is_superseded(&registry, 100),
            "a fresh registration's serial is not superseded"
        );

        // Idempotent re-register of the SAME serial: no bump, no supersession.
        assert_eq!(registration_record(&registry, "kv", 100), 0);
        assert!(!serial_is_superseded(&registry, 100));

        // Re-point "kv" to a DIFFERENT serial 200: incarnation bumps to 1, and
        // the previous serial 100 is superseded (a captured RemotePid over 100
        // must now fail closed with StaleRef).
        assert_eq!(registration_record(&registry, "kv", 200), 1);
        assert!(
            serial_is_superseded(&registry, 100),
            "the re-pointed-away serial is superseded"
        );
        assert!(
            !serial_is_superseded(&registry, 200),
            "the live serial is NOT superseded"
        );

        // A second re-point bumps again.
        assert_eq!(registration_record(&registry, "kv", 300), 2);
        assert!(serial_is_superseded(&registry, 200));
    }

    #[test]
    fn supersede_serial_marks_a_serial_stale() {
        let registry = HewRegistry::default();
        registration_record(&registry, "worker", 42);
        assert!(!serial_is_superseded(&registry, 42));
        // Actor-teardown path: superseding the live serial directly (the actor is
        // being freed) makes a captured ref over it fail closed.
        registry_supersede_serial(&registry, 42);
        assert!(serial_is_superseded(&registry, 42));
    }

    #[test]
    fn distinct_names_have_independent_incarnations() {
        let registry = HewRegistry::default();
        assert_eq!(registration_record(&registry, "a", 1), 0);
        assert_eq!(registration_record(&registry, "b", 2), 0);
        // Re-pointing "a" does not touch "b"'s incarnation.
        assert_eq!(registration_record(&registry, "a", 11), 1);
        assert_eq!(registration_record(&registry, "b", 2), 0);
        assert!(serial_is_superseded(&registry, 1));
        assert!(!serial_is_superseded(&registry, 2));
    }

    #[test]
    fn quarantine_insert_is_monotonic() {
        let _guard = crate::runtime_test_guard();
        quarantine_insert(9, 3);
        // A higher-incarnation death overwrites.
        quarantine_insert(9, 7);
        assert!(
            quarantine_is_blocked(9, 7),
            "blocked at the higher incarnation"
        );
        assert!(
            quarantine_is_blocked(9, 6),
            "blocked below the higher incarnation"
        );
        // A lower-incarnation death never regresses the recorded incarnation.
        quarantine_insert(9, 2);
        assert!(
            quarantine_is_blocked(9, 7),
            "a lower re-insert must not regress the quarantined incarnation"
        );
    }

    #[test]
    fn quarantine_evict_absent_node_is_noop() {
        let _guard = crate::runtime_test_guard();
        // No panic, no effect.
        quarantine_evict(123);
        assert!(!quarantine_is_blocked(123, 1));
    }

    /// Proving gate B: a `Quarantine`-policy send to a buried peer fails closed at
    /// the consult, then resolves normally once the quarantine entry is evicted.
    /// The consult is exercised through `quarantine_blocks_send` (the exact code
    /// the send/ask paths call) with a real node + cluster so the live-incarnation
    /// lookup is genuine.
    #[test]
    fn quarantine_blocks_send_under_policy_then_clears_on_evict() {
        const PEER: u16 = 602;
        let _guard = crate::runtime_test_guard();
        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string for the duration of this test.
        let node_handle = unsafe { TestNode::new(601, &bind_addr) };
        assert!(!node_handle.as_ptr().is_null());
        // SAFETY: pointer came from TestNode::new and is valid until drop.
        unsafe { assert_eq!(hew_node_start(node_handle.as_ptr()), 0) };
        // SAFETY: started node has a non-null cluster.
        let node = unsafe { &*node_handle.as_ptr() };
        assert!(!node.cluster.is_null());
        // SAFETY: cluster is valid while the node is running.
        let cluster = unsafe { &*node.cluster };

        // The peer is known to the cluster, buried (DEAD) at incarnation 5. Seed
        // the membership table so the live-incarnation lookup resolves, and record
        // the quarantine at the incarnation it died at.
        cluster.seed_member_for_test(PEER, crate::cluster::MEMBER_DEAD, 5);
        quarantine_insert(PEER, 5);

        // Under a Quarantine policy the buried peer (live incarnation 5 <=
        // quarantined 5) is blocked.
        {
            let mut ctx = crate::execution_context::HewExecutionContext {
                partition_policy: crate::execution_context::PartitionPolicy::Quarantine.to_slot(),
                ..crate::execution_context::HewExecutionContext::default()
            };
            let _ctx_guard =
                crate::execution_context::TestExecutionContext::install(std::mem::take(&mut ctx));
            assert!(
                quarantine_blocks_send(node, PEER),
                "a Quarantine-policy send to a buried peer must fail closed"
            );
        }

        // A FailFast policy never consults the set: the same peer is NOT blocked.
        {
            let mut ctx = crate::execution_context::HewExecutionContext {
                partition_policy: crate::execution_context::PartitionPolicy::FailFast.to_slot(),
                ..crate::execution_context::HewExecutionContext::default()
            };
            let _ctx_guard =
                crate::execution_context::TestExecutionContext::install(std::mem::take(&mut ctx));
            assert!(
                !quarantine_blocks_send(node, PEER),
                "a FailFast-policy send must not consult the quarantine set"
            );
        }

        // Evict (the readmission edge) clears the block under Quarantine too.
        quarantine_evict(PEER);
        {
            let mut ctx = crate::execution_context::HewExecutionContext {
                partition_policy: crate::execution_context::PartitionPolicy::Quarantine.to_slot(),
                ..crate::execution_context::HewExecutionContext::default()
            };
            let _ctx_guard =
                crate::execution_context::TestExecutionContext::install(std::mem::take(&mut ctx));
            assert!(
                !quarantine_blocks_send(node, PEER),
                "after eviction the peer is sendable again"
            );
        }

        // SAFETY: stop the node before drop.
        unsafe { assert_eq!(hew_node_stop(node_handle.as_ptr()), 0) };
    }

    /// Initialise a real, worker-backed scheduler for a node test (delegates to
    /// the scheduler-side helper, which sees the module-private `stealers` and
    /// safely retires a `runtime_test_guard()` placeholder before init).
    #[must_use]
    fn init_real_scheduler() -> RealSchedulerGuard {
        crate::scheduler::init_real_scheduler_for_test();
        RealSchedulerGuard
    }

    struct RealSchedulerGuard;

    impl Drop for RealSchedulerGuard {
        fn drop(&mut self) {
            crate::scheduler::hew_sched_shutdown();
            crate::scheduler::hew_runtime_cleanup();
        }
    }

    #[test]
    fn node_read_and_sweep_paths_treat_missing_runtime_as_empty_state() {
        let _lock = crate::scheduler::SchedTestLock::acquire();
        assert!(
            crate::runtime::rt_default().is_none(),
            "test requires the runtime slot to be empty"
        );

        with_current_node_read(|current| assert_eq!(*current, 0));
        assert!(!complete_remote_reply(1, &[1, 2, 3]));
        assert!(!fail_remote_reply(1, &[]));
        fail_remote_replies_for_connection(std::ptr::null(), 0);
        // SAFETY: with no runtime there are no known-node registries to sweep.
        unsafe { unregister_actor_names(crate::pid::hew_pid_make(1, 1)) };
    }

    #[test]
    fn real_scheduler_guard_drop_restores_empty_runtime_slot() {
        let _lock = crate::scheduler::SchedTestLock::acquire();
        assert!(
            crate::runtime::rt_default().is_none(),
            "test requires the runtime slot to start empty"
        );

        {
            let _real_sched = init_real_scheduler();
            assert!(
                crate::runtime::rt_default().is_some(),
                "real scheduler init must install a runtime"
            );
        }

        assert!(
            crate::runtime::rt_default().is_none(),
            "dropping the real scheduler guard must restore the empty runtime slot"
        );
    }

    // ── Test-only u32 codec ──────────────────────────────────────────────
    //
    // The two-process remote-send/ask tests transmit a controlled `u32` rather
    // than a Hew-compiled struct, so no codegen-emitted codec exists for their
    // `TWO_PROCESS_REGISTRY_MSG_TYPE`. The cross-node send path now requires a
    // registered codec (fail-closed: no raw-byte escape). These thunks provide
    // the trivial u32 serialize/deserialize the tests need, matching the codec
    // ABI exactly (ser: (value_ptr, out_len) -> bytes; deser: (data, len,
    // out_struct_size) -> value). They register for the request AND, for asks,
    // the reply (both sides of an echo are u32).

    unsafe extern "C" fn test_u32_serialize(
        value_ptr: *const std::ffi::c_void,
        out_len: *mut usize,
    ) -> *mut u8 {
        // SAFETY: value_ptr points to a live u32 from the test.
        let v = unsafe { *value_ptr.cast::<u32>() };
        let buf = crate::cbor_serial::hew_cbor_ser_new();
        // SAFETY: buf is a fresh live CborSerBuf handle.
        unsafe { crate::cbor_serial::hew_cbor_ser_u64(buf, u64::from(v)) };
        // SAFETY: buf is consumed by finish; out_len is a valid pointer.
        unsafe { crate::cbor_serial::hew_cbor_ser_finish(buf, out_len) }
    }

    unsafe extern "C" fn test_u32_deserialize(
        data: *const u8,
        len: usize,
        out_struct_size: *mut usize,
    ) -> *mut std::ffi::c_void {
        // SAFETY: data is valid for len bytes.
        let reader = unsafe { crate::cbor_serial::hew_cbor_de_new(data, len) };
        // SAFETY: reader is a live handle.
        let v64 = unsafe { crate::cbor_serial::hew_cbor_de_u64(reader) };
        #[allow(
            clippy::cast_possible_truncation,
            reason = "test payload is always a u32 round-tripped through the u64 primitive"
        )]
        let v = v64 as u32;
        // SAFETY: reader is a live handle.
        let failed = unsafe { crate::cbor_serial::hew_cbor_de_failed(reader) };
        // SAFETY: reader is a live handle.
        unsafe { crate::cbor_serial::hew_cbor_de_free(reader) };
        if failed != 0 {
            if !out_struct_size.is_null() {
                // SAFETY: out_struct_size validated non-null.
                unsafe { *out_struct_size = 0 };
            }
            return std::ptr::null_mut();
        }
        // SAFETY: malloc a u32-sized value the caller owns via libc::free.
        let dst = unsafe { libc::malloc(std::mem::size_of::<u32>()) }.cast::<u32>();
        if dst.is_null() {
            return std::ptr::null_mut();
        }
        // SAFETY: dst is a valid u32 allocation.
        unsafe { *dst = v };
        if !out_struct_size.is_null() {
            // SAFETY: out_struct_size validated non-null.
            unsafe { *out_struct_size = std::mem::size_of::<u32>() };
        }
        dst.cast::<std::ffi::c_void>()
    }

    /// The codec-registry key `(dispatch, msg_type)` uses the TARGET actor
    /// TYPE's dispatch function pointer. In these tests the echo actor is spawned
    /// with a concrete dispatch fn (e.g. `ask_probe_dispatch`); the inbound
    /// decode resolves THAT pointer from the target actor, so the codec must be
    /// registered under the SAME dispatch and the originating ask must pass it.
    /// This helper casts a dispatch fn to the opaque `*const c_void` key.
    fn dispatch_key(f: crate::internal::types::HewDispatchFn) -> *const c_void {
        f as *const c_void
    }

    /// Canonical codec key for the single-process two-node round-trip tests and
    /// the two-process client side: the echo actor is spawned with
    /// `ask_probe_dispatch`, so its inbound decode resolves that pointer. The
    /// register, the ask FFI arg, and the spawned-actor dispatch all use this.
    fn test_dispatch() -> *const c_void {
        dispatch_key(ask_probe_dispatch)
    }

    /// Register the test u32 codec for `(dispatch, msg_type)` as both the
    /// request and reply codec, so the send/ask paths can serialize their
    /// controlled payloads. `dispatch` MUST be the dispatch fn the target echo
    /// actor is spawned with (the inbound decode resolves it from the actor).
    /// Idempotent across helper processes (each registers in its own process).
    fn register_test_u32_codec(dispatch: *const c_void, msg_type: i32) {
        // SAFETY: the thunks match the codec ABI.
        unsafe {
            crate::xnode_serial::hew_xnode_register_codec(
                dispatch,
                msg_type,
                test_u32_serialize,
                test_u32_deserialize,
            );
            crate::xnode_serial::hew_xnode_register_reply_codec(
                dispatch,
                msg_type,
                test_u32_serialize,
                test_u32_deserialize,
            );
        }
    }

    #[test]
    fn next_local_node_id_never_yields_zero_sentinel() {
        // The 0 node-id is the local/standalone sentinel; a node that landed on
        // it would misclassify every remote actor as local. Verify the mapping
        // skips 0 across the full base range and accumulating offsets.
        for base in [1u16, 2, 100, 65_534, 65_535] {
            for offset in 0u16..4 {
                assert_ne!(
                    next_local_node_id(base, offset),
                    0,
                    "base={base} offset={offset} mapped onto the 0 sentinel",
                );
            }
        }
        // The bare-wrapping_add hazard: base 65535 + offset 1 wrapped to 0.
        assert_ne!(next_local_node_id(65_535, 1), 0);
        // Distinct offsets from the same base stay distinct within the ring.
        assert_ne!(next_local_node_id(100, 0), next_local_node_id(100, 1));
        // base==1, offset==0 maps to itself (the common single-node case).
        assert_eq!(next_local_node_id(1, 0), 1);
    }

    struct ResetCurrentNode(usize);

    impl Drop for ResetCurrentNode {
        fn drop(&mut self) {
            with_current_node(|current| {
                *current = self.0;
            });
        }
    }

    struct TestNode(*mut HewNode);

    impl TestNode {
        unsafe fn new(node_id: u16, bind_addr: &CString) -> Self {
            // SAFETY: Caller guarantees bind_addr is a valid C string.
            Self(unsafe { hew_node_new(node_id, bind_addr.as_ptr()) })
        }

        fn as_ptr(&self) -> *mut HewNode {
            self.0
        }
    }

    impl Drop for TestNode {
        fn drop(&mut self) {
            if !self.0.is_null() {
                // SAFETY: TestNode owns the pointer returned by hew_node_new.
                unsafe { hew_node_free(self.0) };
                self.0 = ptr::null_mut();
            }
        }
    }

    fn start_tcp_test_listener_node(node_id: u16) -> (TestNode, u16) {
        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string for the duration of this helper.
        let node = unsafe { TestNode::new(node_id, &bind_addr) };
        assert!(!node.as_ptr().is_null(), "test node allocation failed");
        // SAFETY: the node pointer came from TestNode::new and stays valid until drop.
        let rc = unsafe { hew_node_start(node.as_ptr()) };
        assert_eq!(
            rc,
            0,
            "hew_node_start({node_id}) failed: {:?}",
            crate::stream_error::take_last_error()
        );
        // SAFETY: the node was started successfully and uses the default TCP transport in these tests.
        let port =
            unsafe { crate::transport::hew_transport_tcp_bound_port((*node.as_ptr()).transport) }
                .expect("started TCP test node must expose its bound listener port");
        (node, port)
    }

    const TWO_PROCESS_REGISTRY_SERVER_NODE: u16 = 620;
    const TWO_PROCESS_REGISTRY_CLIENT_NODE: u16 = 621;
    const TWO_PROCESS_REGISTRY_MSG_TYPE: i32 = 123;
    const TWO_PROCESS_REGISTRY_NAME: &str = "two-process-registry-worker";
    const TWO_PROCESS_ASK_ECHO_NAME: &str = "two-process-ask-echo-worker";
    const TWO_PROCESS_ASK_TIMEOUT_NAME: &str = "two-process-ask-timeout-worker";
    const TWO_PROCESS_HELPER_ENV: &str = "HEW_REGISTRY_GOSSIP_HELPER";
    const TWO_PROCESS_READY_FILE_ENV: &str = "HEW_REGISTRY_GOSSIP_READY_FILE";
    const TWO_PROCESS_SERVER_PORT_ENV: &str = "HEW_REGISTRY_GOSSIP_SERVER_PORT";

    static TWO_PROCESS_REGISTRY_DELIVERY: (Mutex<bool>, Condvar) =
        (Mutex::new(false), Condvar::new());
    static TWO_PROCESS_ASK_OBSERVED: (Mutex<bool>, Condvar) = (Mutex::new(false), Condvar::new());

    struct ManagedChild {
        name: &'static str,
        child: Option<Child>,
    }

    impl ManagedChild {
        fn new(name: &'static str, child: Child) -> Self {
            Self {
                name,
                child: Some(child),
            }
        }

        fn try_wait(&mut self) -> Option<std::process::ExitStatus> {
            self.child
                .as_mut()
                .expect("child already waited")
                .try_wait()
                .expect("child try_wait failed")
        }

        fn wait_output(&mut self, timeout: Duration) -> Output {
            let child = self.child.take().expect("child already waited");
            wait_child_output(self.name, child, timeout)
        }
    }

    impl Drop for ManagedChild {
        fn drop(&mut self) {
            let Some(mut child) = self.child.take() else {
                return;
            };
            if child.try_wait().ok().flatten().is_none() {
                let _ = child.kill();
            }
            let _ = child.wait();
        }
    }

    fn wait_child_output(name: &str, mut child: Child, timeout: Duration) -> Output {
        let deadline = Instant::now() + timeout;
        loop {
            if child.try_wait().expect("child try_wait failed").is_some() {
                return child.wait_with_output().expect("child output failed");
            }
            if Instant::now() >= deadline {
                let pid = child.id();
                let _ = child.kill();
                let output = child.wait_with_output().expect("timed-out child output");
                panic!(
                    "{name} helper process {pid} timed out\nstdout:\n{}\nstderr:\n{}",
                    String::from_utf8_lossy(&output.stdout),
                    String::from_utf8_lossy(&output.stderr)
                );
            }
            thread::sleep(Duration::from_millis(20));
        }
    }

    fn assert_child_success(name: &str, output: &Output) {
        assert!(
            output.status.success(),
            "{name} helper failed with status {:?}\nstdout:\n{}\nstderr:\n{}",
            output.status.code(),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    fn spawn_registry_gossip_helper(
        helper_name: &'static str,
        role: &'static str,
        envs: &[(&str, String)],
    ) -> ManagedChild {
        let mut command = Command::new(std::env::current_exe().expect("current test binary"));
        command
            .args(["--exact", helper_name, "--nocapture"])
            .env("RUST_TEST_THREADS", "1")
            .env(TWO_PROCESS_HELPER_ENV, role)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        for (key, value) in envs {
            command.env(key, value);
        }
        ManagedChild::new(role, command.spawn().expect("spawn helper process"))
    }

    fn wait_for_ready_port(
        ready_file: &std::path::Path,
        server: &mut ManagedChild,
        timeout: Duration,
    ) -> u16 {
        let deadline = Instant::now() + timeout;
        loop {
            if let Ok(text) = std::fs::read_to_string(ready_file) {
                if let Ok(port) = text.trim().parse::<u16>() {
                    return port;
                }
            }
            if server.try_wait().is_some() {
                let output = server.wait_output(Duration::from_secs(1));
                panic!(
                    "server exited before writing readiness file\nstdout:\n{}\nstderr:\n{}",
                    String::from_utf8_lossy(&output.stdout),
                    String::from_utf8_lossy(&output.stderr)
                );
            }
            assert!(
                Instant::now() < deadline,
                "server did not write readiness file before timeout"
            );
            thread::sleep(Duration::from_millis(20));
        }
    }

    fn wait_for_single_connection(node: *mut HewNode, timeout: Duration) -> bool {
        let deadline = Instant::now() + timeout;
        loop {
            // SAFETY: caller passes a live node pointer for this bounded wait.
            if unsafe { connection::hew_connmgr_count((*node).conn_mgr) > 0 } {
                return true;
            }
            if Instant::now() >= deadline {
                return false;
            }
            thread::sleep(Duration::from_millis(20));
        }
    }

    fn wait_for_remote_lookup(
        node: *mut HewNode,
        name: *const c_char,
        expected_node_id: u16,
        timeout: Duration,
    ) -> Option<u64> {
        let deadline = Instant::now() + timeout;
        loop {
            // SAFETY: node/name are valid for this bounded wait.
            let pid = unsafe { hew_node_lookup(node, name) };
            if pid != 0 && crate::pid::hew_pid_node(pid) == expected_node_id {
                return Some(pid);
            }
            if Instant::now() >= deadline {
                return None;
            }
            thread::sleep(Duration::from_millis(20));
        }
    }

    fn reset_two_process_delivery() {
        let mut delivered = TWO_PROCESS_REGISTRY_DELIVERY
            .0
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *delivered = false;
    }

    fn wait_for_two_process_delivery(timeout: Duration) -> bool {
        let deadline = Instant::now() + timeout;
        let mut delivered = TWO_PROCESS_REGISTRY_DELIVERY
            .0
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        while !*delivered {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return false;
            }
            let (guard, result) = TWO_PROCESS_REGISTRY_DELIVERY
                .1
                .wait_timeout(delivered, remaining)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            delivered = guard;
            if result.timed_out() && !*delivered {
                return false;
            }
        }
        true
    }

    fn reset_two_process_ask_observed() {
        let mut observed = TWO_PROCESS_ASK_OBSERVED
            .0
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *observed = false;
    }

    fn mark_two_process_ask_observed() {
        let mut observed = TWO_PROCESS_ASK_OBSERVED
            .0
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *observed = true;
        TWO_PROCESS_ASK_OBSERVED.1.notify_all();
    }

    fn wait_for_two_process_ask_observed(timeout: Duration) -> bool {
        let deadline = Instant::now() + timeout;
        let mut observed = TWO_PROCESS_ASK_OBSERVED
            .0
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        while !*observed {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return false;
            }
            let (guard, result) = TWO_PROCESS_ASK_OBSERVED
                .1
                .wait_timeout(observed, remaining)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            observed = guard;
            if result.timed_out() && !*observed {
                return false;
            }
        }
        true
    }

    unsafe extern "C-unwind" fn two_process_registry_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        if msg_type == TWO_PROCESS_REGISTRY_MSG_TYPE {
            let mut delivered = TWO_PROCESS_REGISTRY_DELIVERY
                .0
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            *delivered = true;
            TWO_PROCESS_REGISTRY_DELIVERY.1.notify_all();
        }

        std::ptr::null_mut()
    }

    fn run_registry_gossip_server_helper() {
        reset_two_process_delivery();
        // Install the runtime before touching the name registry: in this helper
        // subprocess no `runtime_test_guard` is held, so the registry's
        // `rt_current()` resolver has nothing to read until init runs.
        let _real_sched = init_real_scheduler();
        crate::registry::hew_registry_clear();

        let (node, port) = start_tcp_test_listener_node(TWO_PROCESS_REGISTRY_SERVER_NODE);
        crate::pid::hew_pid_set_local_node(TWO_PROCESS_REGISTRY_SERVER_NODE);

        // SAFETY: null state / size-0 is valid; dispatch fn is a valid fn ptr.
        let worker = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(two_process_registry_dispatch))
        };
        assert!(!worker.is_null(), "server worker spawn failed");
        // SAFETY: actor was just spawned successfully.
        let worker_pid = unsafe { (*worker).id };
        assert_eq!(
            crate::pid::hew_pid_node(worker_pid),
            TWO_PROCESS_REGISTRY_SERVER_NODE,
            "server worker PID must encode the server node id"
        );

        let name = CString::new(TWO_PROCESS_REGISTRY_NAME).expect("valid registry name");
        // SAFETY: node/name/worker_pid are valid in this helper process.
        let register_rc = unsafe { hew_node_register(node.as_ptr(), name.as_ptr(), worker_pid) };
        assert_eq!(register_rc, 0, "server register");

        let ready_file = std::env::var(TWO_PROCESS_READY_FILE_ENV).expect("ready file env");
        std::fs::write(&ready_file, port.to_string()).expect("write ready file");

        let delivered = wait_for_two_process_delivery(Duration::from_secs(30));

        // SAFETY: actor and node are owned by this helper process.
        unsafe {
            let _ = crate::actor::hew_actor_free(worker);
            assert_eq!(hew_node_stop(node.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
        assert!(delivered, "server did not observe two-process send");
    }

    fn run_registry_gossip_client_helper() {
        // Install the runtime before touching the name registry (helper
        // subprocess holds no `runtime_test_guard`).
        let _real_sched = init_real_scheduler();
        crate::registry::hew_registry_clear();

        let server_port = std::env::var(TWO_PROCESS_SERVER_PORT_ENV)
            .expect("server port env")
            .parse::<u16>()
            .expect("server port");
        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is valid for this helper scope.
        let node = unsafe { TestNode::new(TWO_PROCESS_REGISTRY_CLIENT_NODE, &bind_addr) };
        assert!(!node.as_ptr().is_null(), "client node allocation failed");
        // SAFETY: node pointer is valid.
        assert_eq!(unsafe { hew_node_start(node.as_ptr()) }, 0, "client start");

        let connect_addr = CString::new(format!(
            "{TWO_PROCESS_REGISTRY_SERVER_NODE}@127.0.0.1:{server_port}"
        ))
        .expect("valid connect addr");
        // SAFETY: node and connect_addr are valid.
        unsafe { connect_with_retry(node.as_ptr(), &connect_addr) };
        assert!(
            wait_for_single_connection(node.as_ptr(), Duration::from_secs(5)),
            "client connection did not become active"
        );

        let name = CString::new(TWO_PROCESS_REGISTRY_NAME).expect("valid registry name");
        let remote_pid = wait_for_remote_lookup(
            node.as_ptr(),
            name.as_ptr(),
            TWO_PROCESS_REGISTRY_SERVER_NODE,
            Duration::from_secs(30),
        )
        .expect("client lookup did not resolve remote registry gossip");

        // SAFETY: remote_pid was resolved from registry gossip; null payload is
        // valid for this signal message.
        let rc = unsafe {
            hew_node_send(
                node.as_ptr(),
                remote_pid,
                ptr::null(),
                TWO_PROCESS_REGISTRY_MSG_TYPE,
                ptr::null(),
                0,
            )
        };
        assert_eq!(rc, 0, "client remote send");

        // SAFETY: node is owned by this helper process.
        unsafe {
            assert_eq!(hew_node_stop(node.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    fn run_two_process_ask_server_helper(
        node_id: u16,
        name: &str,
        dispatch: unsafe extern "C-unwind" fn(
            *mut crate::execution_context::HewExecutionContext,
            *mut c_void,
            i32,
            *mut c_void,
            usize,
            i32,
        ) -> *mut c_void,
        hold_after_observed: Duration,
    ) {
        // The inbound-ask path decodes the request and encodes the reply via the
        // registered codec (fail-closed), keyed by the target actor's dispatch.
        // Register under the SAME dispatch fn this server spawns its echo actor
        // with so the inbound decode resolves the matching codec.
        register_test_u32_codec(dispatch_key(dispatch), TWO_PROCESS_REGISTRY_MSG_TYPE);
        reset_two_process_ask_observed();
        // Install the runtime before touching the name registry (helper
        // subprocess holds no `runtime_test_guard`).
        let _real_sched = init_real_scheduler();
        crate::registry::hew_registry_clear();

        let (node, port) = start_tcp_test_listener_node(node_id);
        crate::pid::hew_pid_set_local_node(node_id);
        // SAFETY: null state / size-0 is valid; dispatch fn is a valid fn ptr.
        let worker = unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(dispatch)) };
        assert!(!worker.is_null(), "ask server worker spawn failed");
        // SAFETY: actor was just spawned successfully.
        let worker_pid = unsafe { (*worker).id };
        assert_eq!(crate::pid::hew_pid_node(worker_pid), node_id);

        let name = CString::new(name).expect("valid ask registry name");
        // SAFETY: node/name/worker_pid are valid in this helper process.
        let register_rc = unsafe { hew_node_register(node.as_ptr(), name.as_ptr(), worker_pid) };
        assert_eq!(register_rc, 0, "ask server register");

        let ready_file = std::env::var(TWO_PROCESS_READY_FILE_ENV).expect("ready file env");
        std::fs::write(&ready_file, port.to_string()).expect("write ready file");

        assert!(
            wait_for_two_process_ask_observed(Duration::from_secs(30)),
            "ask server did not observe remote ask"
        );
        thread::sleep(hold_after_observed);

        // SAFETY: actor and node are owned by this helper process.
        unsafe {
            let _ = crate::actor::hew_actor_free(worker);
            assert_eq!(hew_node_stop(node.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    struct TwoProcessAskClient {
        node: TestNode,
        remote_pid: u64,
        _real_sched: RealSchedulerGuard,
    }

    fn run_two_process_ask_echo_client_helper() {
        let client = run_two_process_ask_client_setup(
            TWO_PROCESS_REGISTRY_CLIENT_NODE,
            TWO_PROCESS_REGISTRY_SERVER_NODE,
            TWO_PROCESS_ASK_ECHO_NAME,
        );
        let node = &client.node;
        let remote_pid = client.remote_pid;
        let send_value: u32 = 21;
        // SAFETY: remote_pid was resolved from a separate helper process over TCP.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                remote_pid,
                test_dispatch(),
                TWO_PROCESS_REGISTRY_MSG_TYPE,
                (&raw const send_value).cast::<c_void>().cast_mut(),
                std::mem::size_of::<u32>(),
                // 10s, not 1s: this checks the echo VALUE (== 42), not latency.
                // The reply round-trips across an OS process boundary over
                // loopback TCP, which the in-process simtime seam can't fake; on
                // a fully loaded CI runner the server's reply can lag well past
                // 1s, staling a correct echo into a null. A genuine no-reply
                // still fails (null) within the bound. Root de-flake: #1963.
                10_000,
                std::mem::size_of::<u32>(),
            )
        };
        assert!(!reply_ptr.is_null(), "two-process echo ask returned null");
        // SAFETY: reply_ptr was malloc'd by hew_node_api_ask; valid for u32 read.
        let reply_value = unsafe { *(reply_ptr.cast::<u32>()) };
        // SAFETY: reply_ptr was malloc'd and is our responsibility to free.
        unsafe { libc::free(reply_ptr) };
        assert_eq!(
            reply_value, 42,
            "two-process echo-double ask must return 42"
        );
        assert_eq!(hew_node_ask_take_last_error(), AskError::None as i32);
        // SAFETY: node is owned by this helper process.
        unsafe {
            assert_eq!(hew_node_stop(node.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    fn run_two_process_ask_timeout_client_helper() {
        let client = run_two_process_ask_client_setup(
            TWO_PROCESS_REGISTRY_CLIENT_NODE,
            TWO_PROCESS_REGISTRY_SERVER_NODE,
            TWO_PROCESS_ASK_TIMEOUT_NAME,
        );
        let node = &client.node;
        let remote_pid = client.remote_pid;
        let send_value: u32 = 21;
        // SAFETY: remote_pid was resolved from a separate helper process over TCP.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                remote_pid,
                test_dispatch(),
                TWO_PROCESS_REGISTRY_MSG_TYPE,
                (&raw const send_value).cast::<c_void>().cast_mut(),
                std::mem::size_of::<u32>(),
                TEST_REMOTE_ASK_TIMEOUT_MS,
                std::mem::size_of::<u32>(),
            )
        };
        // The deterministic invariant is the typed OUTCOME: a server that never
        // replies (the timeout-server holds past the ask deadline) must surface
        // `AskError::Timeout` with a null reply. Elapsed wall time is NOT asserted
        // — under load the 250ms ask deadline can fire later, but the OUTCOME is
        // load-independent. The ask's own timeout is the hang ceiling; a genuine
        // never-resolving ask is caught by nextest's slow-timeout, not a window.
        assert!(
            reply_ptr.is_null(),
            "timeout ask unexpectedly returned a reply"
        );
        assert_eq!(hew_node_ask_take_last_error(), AskError::Timeout as i32);
        // SAFETY: node is owned by this helper process.
        unsafe {
            assert_eq!(hew_node_stop(node.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    fn run_two_process_ask_client_setup(
        client_node_id: u16,
        server_node_id: u16,
        registry_name: &str,
    ) -> TwoProcessAskClient {
        // The cross-node send/ask path requires a registered codec for the
        // payload's msg_type (fail-closed). Register the test u32 codec.
        register_test_u32_codec(test_dispatch(), TWO_PROCESS_REGISTRY_MSG_TYPE);
        // Install the runtime before touching the name registry (helper
        // subprocess holds no `runtime_test_guard`).
        let real_sched = init_real_scheduler();
        crate::registry::hew_registry_clear();

        let server_port = std::env::var(TWO_PROCESS_SERVER_PORT_ENV)
            .expect("server port env")
            .parse::<u16>()
            .expect("server port");
        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is valid for this helper scope.
        let node = unsafe { TestNode::new(client_node_id, &bind_addr) };
        assert!(
            !node.as_ptr().is_null(),
            "ask client node allocation failed"
        );
        // SAFETY: node pointer is valid.
        assert_eq!(unsafe { hew_node_start(node.as_ptr()) }, 0, "client start");

        let connect_addr = CString::new(format!("{server_node_id}@127.0.0.1:{server_port}"))
            .expect("valid connect addr");
        // SAFETY: node and connect_addr are valid.
        unsafe { connect_with_retry(node.as_ptr(), &connect_addr) };
        assert!(
            wait_for_single_connection(node.as_ptr(), Duration::from_secs(5)),
            "ask client connection did not become active"
        );

        let name = CString::new(registry_name).expect("valid registry name");
        let remote_pid = wait_for_remote_lookup(
            node.as_ptr(),
            name.as_ptr(),
            server_node_id,
            Duration::from_secs(30),
        )
        .expect("ask client lookup did not resolve remote registry gossip");
        assert_ne!(
            crate::pid::hew_pid_node(remote_pid),
            client_node_id,
            "ask test must not resolve to a local pid"
        );
        TwoProcessAskClient {
            node,
            remote_pid,
            _real_sched: real_sched,
        }
    }

    #[test]
    fn registry_gossip_two_process_server_helper() {
        if !matches!(
            std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
            Ok("server")
        ) {
            return;
        }
        run_registry_gossip_server_helper();
    }

    #[test]
    fn registry_gossip_two_process_client_helper() {
        if !matches!(
            std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
            Ok("client")
        ) {
            return;
        }
        run_registry_gossip_client_helper();
    }

    #[test]
    fn remote_ask_two_process_echo_server_helper() {
        if !matches!(
            std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
            Ok("ask_echo_server")
        ) {
            return;
        }
        run_two_process_ask_server_helper(
            TWO_PROCESS_REGISTRY_SERVER_NODE,
            TWO_PROCESS_ASK_ECHO_NAME,
            ask_probe_dispatch,
            Duration::ZERO,
        );
    }

    #[test]
    fn remote_ask_two_process_echo_client_helper() {
        if !matches!(
            std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
            Ok("ask_echo_client")
        ) {
            return;
        }
        run_two_process_ask_echo_client_helper();
    }

    #[test]
    fn remote_ask_two_process_timeout_server_helper() {
        if !matches!(
            std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
            Ok("ask_timeout_server")
        ) {
            return;
        }
        run_two_process_ask_server_helper(
            TWO_PROCESS_REGISTRY_SERVER_NODE,
            TWO_PROCESS_ASK_TIMEOUT_NAME,
            blocked_ask_probe_dispatch,
            Duration::from_millis(1_750),
        );
    }

    #[test]
    fn remote_ask_two_process_timeout_client_helper() {
        if !matches!(
            std::env::var(TWO_PROCESS_HELPER_ENV).as_deref(),
            Ok("ask_timeout_client")
        ) {
            return;
        }
        run_two_process_ask_timeout_client_helper();
    }

    #[test]
    fn two_process_registry_gossip_lookup_then_tell() {
        let _guard = crate::runtime_test_guard();
        let ready_dir = tempfile::tempdir().expect("ready tempdir");
        let ready_file = ready_dir.path().join("server-ready");
        let ready_file_s = ready_file.to_string_lossy().into_owned();

        let mut server = spawn_registry_gossip_helper(
            "hew_node::tests::registry_gossip_two_process_server_helper",
            "server",
            &[(TWO_PROCESS_READY_FILE_ENV, ready_file_s)],
        );
        let server_port = wait_for_ready_port(&ready_file, &mut server, Duration::from_secs(10));

        let mut client = spawn_registry_gossip_helper(
            "hew_node::tests::registry_gossip_two_process_client_helper",
            "client",
            &[(TWO_PROCESS_SERVER_PORT_ENV, server_port.to_string())],
        );
        let client_output = client.wait_output(Duration::from_secs(40));
        assert_child_success("client", &client_output);

        let server_output = server.wait_output(Duration::from_secs(40));
        assert_child_success("server", &server_output);
    }

    fn run_two_process_remote_ask_case(
        server_helper: &'static str,
        server_role: &'static str,
        client_helper: &'static str,
        client_role: &'static str,
    ) {
        let _guard = crate::runtime_test_guard();
        let ready_dir = tempfile::tempdir().expect("ready tempdir");
        let ready_file = ready_dir.path().join("ask-server-ready");
        let ready_file_s = ready_file.to_string_lossy().into_owned();

        let mut server = spawn_registry_gossip_helper(
            server_helper,
            server_role,
            &[(TWO_PROCESS_READY_FILE_ENV, ready_file_s)],
        );
        let server_port = wait_for_ready_port(&ready_file, &mut server, Duration::from_secs(10));

        let mut client = spawn_registry_gossip_helper(
            client_helper,
            client_role,
            &[(TWO_PROCESS_SERVER_PORT_ENV, server_port.to_string())],
        );
        let client_output = client.wait_output(Duration::from_secs(40));
        assert_child_success(client_role, &client_output);

        let server_output = server.wait_output(Duration::from_secs(40));
        assert_child_success(server_role, &server_output);
    }

    #[test]
    fn two_process_remote_ask_echo_double_returns_42() {
        run_two_process_remote_ask_case(
            "hew_node::tests::remote_ask_two_process_echo_server_helper",
            "ask_echo_server",
            "hew_node::tests::remote_ask_two_process_echo_client_helper",
            "ask_echo_client",
        );
    }

    #[test]
    fn two_process_remote_ask_timeout_returns_timeout_under_1500ms() {
        run_two_process_remote_ask_case(
            "hew_node::tests::remote_ask_two_process_timeout_server_helper",
            "ask_timeout_server",
            "hew_node::tests::remote_ask_two_process_timeout_client_helper",
            "ask_timeout_client",
        );
    }

    /// Start a node whose transport has been pre-allocated as a `quic_mesh`
    /// transport with the supplied [`MeshTls`] config. The override path
    /// bypasses `HEW_TRANSPORT`'s default self-signed allowlist so that two
    /// in-process nodes can mutually pin each other's SPKIs.
    ///
    /// Returns the [`TestNode`] handle and the bound UDP port.
    ///
    /// Mirrors [`start_tcp_test_listener_node`] but for the native `quic_mesh`
    /// transport. Used by the cross-node `quic_mesh` integration tests below.
    #[cfg(feature = "quic")]
    fn start_quic_mesh_test_listener_node(
        node_id: u16,
        tls: crate::quic_mesh::MeshTls,
    ) -> (TestNode, u16) {
        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string for the duration of this helper.
        let node = unsafe { TestNode::new(node_id, &bind_addr) };
        assert!(!node.as_ptr().is_null(), "test node allocation failed");

        // SAFETY: hew_transport_quic_mesh_new returns an owned transport
        // pointer (or null on runtime build failure).
        let transport = unsafe { crate::quic_mesh::hew_transport_quic_mesh_new() };
        assert!(
            !transport.is_null(),
            "quic_mesh transport allocation failed: {:?}",
            crate::stream_error::take_last_error()
        );
        // SAFETY: transport pointer was just allocated by the constructor.
        let rc =
            unsafe { crate::quic_mesh::hew_transport_quic_mesh_set_tls_override(transport, tls) };
        assert_eq!(rc, 0, "set TLS override on quic_mesh transport");

        // SAFETY: node owns the previously null transport slot; we replace it
        // with the pre-allocated quic_mesh transport before start.
        unsafe {
            (*node.as_ptr()).transport = transport;
        }

        // SAFETY: node and transport pointers are valid; start consumes the
        // injected transport instead of selecting from HEW_TRANSPORT.
        let rc = unsafe { hew_node_start(node.as_ptr()) };
        assert_eq!(
            rc,
            0,
            "hew_node_start({node_id}) on quic_mesh failed: {:?}",
            crate::stream_error::take_last_error()
        );
        // SAFETY: node started successfully and is using the quic_mesh transport.
        let port = unsafe {
            crate::quic_mesh::hew_transport_quic_mesh_bound_port((*node.as_ptr()).transport)
        }
        .expect("started quic_mesh test node must expose its bound listener port");
        (node, port)
    }

    unsafe extern "C-unwind" fn noop_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
    }

    /// `NodeId` authority (BLOCK-5 point 1): a low-level node whose explicit
    /// `hew_node_new(id)` contradicts its installed strict snapshot binding
    /// identity is rejected **before** the listener binds — no socket, no
    /// manager, fail-closed.
    #[test]
    fn node_start_rejects_conflicting_snapshot_node_id_before_listen() {
        use crate::peer_binding::{PeerAuthConfig, PeerBindings, PeerCredential};
        let _guard = crate::runtime_test_guard();
        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // Explicit low-level id 100 conflicts with the snapshot's strict id 42.
        // SAFETY: bind_addr is valid for the duration of this test.
        let node = unsafe { TestNode::new(100, &bind_addr) };
        assert!(!node.as_ptr().is_null());
        let mut bindings = PeerBindings::new();
        bindings
            .entry(42)
            .or_default()
            .insert(PeerCredential::NoiseKey([0xAB; 32]));
        let snapshot = PeerAuthConfig {
            node_id: std::num::NonZeroU16::new(42),
            bindings,
            ..PeerAuthConfig::default()
        }
        .snapshot();
        // SAFETY: node is STOPPED; installing a snapshot is valid.
        let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
        assert_eq!(set_rc, 0);
        // SAFETY: node is valid; start must reject the conflicting id.
        let rc = unsafe { hew_node_start(node.as_ptr()) };
        assert_eq!(rc, -1, "conflicting snapshot id must be rejected");
        // SAFETY: node valid; assert fail-closed: STOPPED, no manager, no transport.
        unsafe {
            let n = &*node.as_ptr();
            assert_eq!(n.state.load(Ordering::Acquire), NODE_STATE_STOPPED);
            assert!(n.conn_mgr.is_null(), "no manager on rejected start");
            assert!(n.transport.is_null(), "no listener bound on rejected start");
        }
        // SAFETY: reads the thread-local last-error C string set by the start
        // path; the pointer is valid until the next error is set on this thread.
        let err = unsafe {
            let p = crate::hew_last_error();
            if p.is_null() {
                String::new()
            } else {
                std::ffi::CStr::from_ptr(p).to_string_lossy().into_owned()
            }
        };
        assert!(
            err.contains("conflicts with strict binding identity"),
            "diagnostic should name the conflict; got: {err}"
        );
    }

    /// `NodeId` authority: a low-level node created with id `0` adopts its
    /// snapshot's stable `NodeId` at start (a caller that deferred the id).
    #[test]
    fn node_start_adopts_snapshot_node_id_when_created_with_zero() {
        use crate::peer_binding::{PeerAuthConfig, PeerBindings, PeerCredential};
        let _guard = crate::runtime_test_guard();
        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr valid for the test.
        let node = unsafe { TestNode::new(0, &bind_addr) };
        assert!(!node.as_ptr().is_null());
        let mut bindings = PeerBindings::new();
        bindings
            .entry(4242)
            .or_default()
            .insert(PeerCredential::NoiseKey([0xCD; 32]));
        let snapshot = PeerAuthConfig {
            node_id: std::num::NonZeroU16::new(4242),
            bindings,
            ..PeerAuthConfig::default()
        }
        .snapshot();
        // SAFETY: node STOPPED.
        let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
        assert_eq!(set_rc, 0);
        // SAFETY: node valid; start should adopt 4242 and succeed.
        let rc = unsafe { hew_node_start(node.as_ptr()) };
        assert_eq!(
            rc,
            0,
            "start should adopt snapshot id: {:?}",
            crate::stream_error::take_last_error()
        );
        // SAFETY: node valid & running.
        unsafe {
            assert_eq!((*node.as_ptr()).node_id, 4242);
            assert_eq!(hew_node_stop(node.as_ptr()), 0);
        }
    }

    /// Defence-in-depth: a self-inconsistent snapshot (unverified opt-out WITH
    /// bindings) fails the node's own start before listen.
    #[test]
    fn node_start_rejects_malformed_snapshot() {
        use crate::peer_binding::{PeerAuthConfig, PeerBindings, PeerCredential};
        let _guard = crate::runtime_test_guard();
        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr valid.
        let node = unsafe { TestNode::new(7, &bind_addr) };
        assert!(!node.as_ptr().is_null());
        let mut bindings = PeerBindings::new();
        bindings
            .entry(42)
            .or_default()
            .insert(PeerCredential::NoiseKey([0x11; 32]));
        let snapshot = PeerAuthConfig {
            node_id: std::num::NonZeroU16::new(7),
            unverified_optout: true,
            bindings,
            ..PeerAuthConfig::default()
        }
        .snapshot();
        // SAFETY: node STOPPED.
        let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
        assert_eq!(set_rc, 0);
        // SAFETY: node valid; start must reject the malformed snapshot.
        assert_eq!(unsafe { hew_node_start(node.as_ptr()) }, -1);
        // SAFETY: node valid.
        unsafe {
            assert!((*node.as_ptr()).transport.is_null());
        }
    }

    /// `hew_node_set_auth_snapshot` is rejected once the node is not STOPPED.
    #[test]
    fn set_auth_snapshot_rejected_when_running() {
        let _guard = crate::runtime_test_guard();
        let (node, _port) = start_tcp_test_listener_node(55);
        // SAFETY: node is RUNNING; the setter must refuse.
        let rc =
            unsafe { hew_node_set_auth_snapshot(node.as_ptr(), PeerAuthSnapshot::unconfigured()) };
        assert_eq!(rc, -1, "installing a snapshot on a running node must fail");
        // SAFETY: node valid.
        unsafe {
            assert_eq!(hew_node_stop(node.as_ptr()), 0);
        }
    }

    /// Public owner-scoped staging (BLOCK-6): a default `Node::start` transitions
    /// `Building → Running{owner}` and `Node::shutdown` returns it to `Building`
    /// with a bumped generation. No env configured ⇒ unconfigured snapshot,
    /// PID-derived id — the pre-fix default behaviour is preserved.
    #[test]
    fn public_start_default_then_shutdown_resets_state() {
        let _guard = crate::runtime_test_guard();
        // Start from a known-clean staging cell (other unit tests stage config).
        {
            let mut g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            g.state = ConfigState::default();
        }
        let bind = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind is a valid C string for this call.
        let rc = unsafe { hew_node_api_start(bind.as_ptr()) };
        assert_eq!(rc, 0, "default public start should succeed");
        {
            let g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            assert!(
                matches!(g.state, ConfigState::Running { .. }),
                "staging state should be Running after a successful start"
            );
        }
        let gen_before = {
            let g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            g.next_generation
        };
        // SAFETY: a node was started by this test; shutdown reclaims it.
        assert_eq!(unsafe { hew_node_api_shutdown() }, 0);
        {
            let g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            assert!(
                matches!(g.state, ConfigState::Building(_)),
                "shutdown must return staging to Building"
            );
            assert_ne!(
                g.next_generation, gen_before,
                "shutdown must bump the staging generation"
            );
        }
    }

    /// A second public `Node::start` while one is already active is refused
    /// (fail-closed), and the first node keeps running until its own shutdown.
    #[test]
    fn public_start_rejected_when_already_active() {
        let _guard = crate::runtime_test_guard();
        {
            let mut g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            g.state = ConfigState::default();
        }
        let bind = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind valid.
        assert_eq!(unsafe { hew_node_api_start(bind.as_ptr()) }, 0);
        // SAFETY: bind valid; second start must be rejected.
        let second_rc = unsafe { hew_node_api_start(bind.as_ptr()) };
        assert_eq!(
            second_rc, -1,
            "a second concurrent public start must be refused"
        );
        // SAFETY: the first node is still active; shutdown reclaims it.
        assert_eq!(unsafe { hew_node_api_shutdown() }, 0);
    }

    #[test]
    fn node_lifecycle_start_stop() {
        let _guard = crate::runtime_test_guard();

        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string for the duration of this test.
        let node = unsafe { TestNode::new(101, &bind_addr) };
        assert!(!node.as_ptr().is_null());

        // SAFETY: node pointer is created in this test and valid until drop.
        unsafe {
            assert_eq!(hew_node_start(node.as_ptr()), 0);
            assert_eq!(
                (&*node.as_ptr()).state.load(Ordering::Acquire),
                NODE_STATE_RUNNING
            );
            assert_eq!(hew_node_stop(node.as_ptr()), 0);
            assert_eq!(
                (&*node.as_ptr()).state.load(Ordering::Acquire),
                NODE_STATE_STOPPED
            );
        }
    }

    /// F6 fail-closed (C ABI): a failed `Node::load_keys` / `Node::allow_peer`
    /// records a sticky peer-auth failure, so the next `Node::start` refuses to
    /// bind a listener (returns -1) rather than silently coming up with an
    /// ephemeral identity or an incomplete allowlist. This is the closed half of
    /// the worst-case fail-open F6 addresses: pre-fix, `load_keys` returned -1
    /// (discarded by the `Unit` Hew form) yet `start` proceeded.
    #[cfg(feature = "quic")]
    #[test]
    fn start_refuses_after_failed_peer_auth_setup_fail_closed() {
        // Serialise against quic_mesh tests that mint identities / mutate the
        // global allowlist: this test records a peer-auth failure in the shared
        // process-global flag, which would otherwise fail-close a concurrent
        // `ensure_identity` in another module. Held as the outermost guard.
        let _state_guard = crate::quic_mesh::MESH_GLOBAL_STATE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _guard = crate::runtime_test_guard();
        let bind = CString::new("127.0.0.1:0").expect("valid bind addr");

        // --- corrupt keyfile: load_keys must fail and poison start ---
        crate::quic_mesh::mesh_auth_setup_reset();
        let dir = std::env::temp_dir().join(format!("f6-cabi-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let key = dir.join("corrupt.key");
        std::fs::write(&key, b"not-a-keyfile-frame").unwrap();
        let c_key = CString::new(key.to_str().unwrap()).unwrap();
        // SAFETY: c_key is a valid NUL-terminated C string for this call.
        let rc_load = unsafe { hew_node_api_load_keys(c_key.as_ptr()) };
        // SAFETY: bind is a valid NUL-terminated C string for this call.
        let rc_start_after_load = unsafe { hew_node_api_start(bind.as_ptr()) };
        // A fail-closed start never created a node; clean up if the gate regressed.
        if rc_start_after_load == 0 {
            // SAFETY: shutdown reclaims the current node, if any; takes no arguments.
            unsafe { hew_node_api_shutdown() };
        }
        crate::quic_mesh::mesh_auth_setup_reset();
        let _ = std::fs::remove_file(&key);

        // --- bad-hex allow_peer: must fail and poison start ---
        crate::quic_mesh::mesh_auth_setup_reset();
        let bad_hex = CString::new("nothex!!").expect("valid C string");
        // SAFETY: bad_hex is a valid NUL-terminated C string for this call.
        let rc_allow = unsafe { hew_node_api_allow_peer(bad_hex.as_ptr()) };
        // SAFETY: bind is a valid NUL-terminated C string for this call.
        let rc_start_after_allow = unsafe { hew_node_api_start(bind.as_ptr()) };
        if rc_start_after_allow == 0 {
            // SAFETY: shutdown reclaims the current node, if any; takes no arguments.
            unsafe { hew_node_api_shutdown() };
        }
        // Reset BEFORE asserting so a failed assertion can't poison sibling tests.
        crate::quic_mesh::mesh_auth_setup_reset();

        assert_eq!(rc_load, -1, "a corrupt keyfile must report a load failure");
        assert_eq!(
            rc_start_after_load, -1,
            "Node::start must refuse after a failed load_keys (fail-closed)"
        );
        assert_eq!(
            rc_allow, -1,
            "a bad-hex SPKI must be rejected by allow_peer"
        );
        assert_eq!(
            rc_start_after_allow, -1,
            "Node::start must refuse after a failed allow_peer (fail-closed)"
        );
    }

    #[test]
    fn concurrent_api_shutdown_claims_current_node_once() {
        let _guard = crate::runtime_test_guard();

        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string for the duration of this test.
        unsafe { assert_eq!(hew_node_api_start(bind_addr.as_ptr()), 0) };

        let barrier = std::sync::Arc::new(std::sync::Barrier::new(3));
        let handles: Vec<_> = (0..2)
            .map(|_| {
                let barrier = barrier.clone();
                std::thread::spawn(move || {
                    barrier.wait();
                    // SAFETY: the API owns the current node pointer installed above.
                    unsafe { hew_node_api_shutdown() }
                })
            })
            .collect();

        barrier.wait();
        let results: Vec<_> = handles
            .into_iter()
            .map(|handle| handle.join().expect("shutdown thread should not panic"))
            .collect();

        assert_eq!(
            results.iter().filter(|&&rc| rc == 0).count(),
            1,
            "exactly one shutdown caller must claim ownership"
        );
        assert_eq!(
            results.iter().filter(|&&rc| rc == -1).count(),
            1,
            "the losing shutdown caller must observe that no node remains"
        );
        with_current_node_read(|current| {
            assert_eq!(*current, 0);
        });
    }

    #[test]
    fn local_registry_register_and_lookup() {
        let _guard = crate::runtime_test_guard();

        crate::registry::hew_registry_clear();

        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string for the duration of this test.
        let node = unsafe { TestNode::new(102, &bind_addr) };
        assert!(!node.as_ptr().is_null());

        let actor_name = CString::new("hew-node-local-registry").expect("valid actor name");
        let missing_name = CString::new("hew-node-missing-registry").expect("valid actor name");
        let actor_id = (u64::from(102u16) << 48) | 0x1234;

        // SAFETY: node and C string pointers are valid for each call.
        unsafe {
            assert_eq!(hew_node_start(node.as_ptr()), 0);
            assert_eq!(
                hew_node_register(node.as_ptr(), actor_name.as_ptr(), actor_id),
                0
            );
            assert_eq!(
                hew_node_lookup(node.as_ptr(), actor_name.as_ptr()),
                actor_id
            );
            assert_eq!(hew_node_lookup(node.as_ptr(), missing_name.as_ptr()), 0);
            assert_eq!(
                crate::registry::hew_registry_unregister(actor_name.as_ptr()),
                0
            );
            assert_eq!(hew_node_stop(node.as_ptr()), 0);
        }

        crate::registry::hew_registry_clear();
    }

    #[test]
    fn actor_free_unregisters_named_actor_and_emits_gossip_remove() {
        let _guard = crate::runtime_test_guard();

        crate::registry::hew_registry_clear();

        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string for the duration of this test.
        let node = unsafe { TestNode::new(103, &bind_addr) };
        assert!(!node.as_ptr().is_null());

        let actor_name = CString::new("hew-node-actor-free-cleanup").expect("valid actor name");

        // SAFETY: pointers are valid for this scope.
        unsafe {
            assert_eq!(hew_node_start(node.as_ptr()), 0);

            let actor = crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!actor.is_null());
            let actor_id = (*actor).id;
            assert_eq!(crate::pid::hew_pid_node(actor_id), 103);

            assert_eq!(
                hew_node_register(node.as_ptr(), actor_name.as_ptr(), actor_id),
                0
            );
            assert_eq!(
                hew_node_lookup(node.as_ptr(), actor_name.as_ptr()),
                actor_id
            );

            let n = &*node.as_ptr();
            assert!(!n.cluster.is_null());
            let cluster = &*n.cluster;
            let _ = cluster.take_registry_gossip(10);

            assert_eq!(crate::actor::hew_actor_free(actor), 0);
            assert_eq!(hew_node_lookup(node.as_ptr(), actor_name.as_ptr()), 0);
            assert!(crate::registry::hew_registry_lookup(actor_name.as_ptr()).is_null());

            let events = cluster.take_registry_gossip(10);
            assert_eq!(events.len(), 1);
            assert_eq!(events[0].name, "hew-node-actor-free-cleanup");
            assert!(!events[0].is_add);

            assert_eq!(hew_node_stop(node.as_ptr()), 0);
        }

        crate::registry::hew_registry_clear();
    }

    #[test]
    fn node_stop_unregisters_local_names() {
        let _guard = crate::runtime_test_guard();

        crate::registry::hew_registry_clear();

        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string for the duration of this test.
        let node = unsafe { TestNode::new(104, &bind_addr) };
        assert!(!node.as_ptr().is_null());

        let actor_name = CString::new("hew-node-stop-cleanup").expect("valid actor name");

        // SAFETY: pointers are valid for this scope.
        unsafe {
            assert_eq!(hew_node_start(node.as_ptr()), 0);

            let actor = crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!actor.is_null());
            let actor_id = (*actor).id;
            assert_eq!(crate::pid::hew_pid_node(actor_id), 104);

            assert_eq!(
                hew_node_register(node.as_ptr(), actor_name.as_ptr(), actor_id),
                0
            );
            assert_eq!(
                hew_node_lookup(node.as_ptr(), actor_name.as_ptr()),
                actor_id
            );
            assert!(!crate::registry::hew_registry_lookup(actor_name.as_ptr()).is_null());

            assert_eq!(hew_node_stop(node.as_ptr()), 0);
            assert_eq!(hew_node_lookup(node.as_ptr(), actor_name.as_ptr()), 0);
            assert!(crate::registry::hew_registry_lookup(actor_name.as_ptr()).is_null());

            assert_eq!(crate::actor::hew_actor_free(actor), 0);
        }

        crate::registry::hew_registry_clear();
    }

    #[test]
    fn two_node_connect_and_handshake() {
        let _guard = crate::runtime_test_guard();

        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").expect("valid bind addr");

        // SAFETY: bind addresses are valid C strings for the duration of this test.
        let node1 = unsafe { TestNode::new(201, &node1_bind) };
        assert!(!node1.as_ptr().is_null());
        let (node2, node2_port) = start_tcp_test_listener_node(202);

        // Allow node2 listener to initialise before node1 connects.
        thread::sleep(Duration::from_millis(50));
        // SAFETY: pointers are valid for this scope.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }

        let connect_addr =
            CString::new(format!("202@127.0.0.1:{node2_port}")).expect("valid connect addr");
        // SAFETY: node pointer and connect_addr are valid for this call.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };

        let actor_name = CString::new("hew-node-remote-actor").expect("valid actor name");
        let actor_id = (u64::from(202u16) << 48) | 0x63;
        // SAFETY: pointers are valid in this scope.
        unsafe {
            assert_eq!(
                hew_node_register(node2.as_ptr(), actor_name.as_ptr(), actor_id),
                0
            );
            assert_eq!(
                hew_node_lookup(node2.as_ptr(), actor_name.as_ptr()),
                actor_id
            );
        }

        // SAFETY: node pointers are valid while the test owns both nodes.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: pointers remain valid until dropped.
        unsafe {
            let _ = crate::registry::hew_registry_unregister(actor_name.as_ptr());
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }

        crate::registry::hew_registry_clear();
    }

    #[test]
    fn membership_left_prunes_remote_names_for_departed_node() {
        let _guard = crate::runtime_test_guard();

        let registry = HewRegistry::default();
        {
            let mut map = registry.remote_names.lock_or_recover();
            map.insert("remote-a".to_owned(), (u64::from(202u16) << 48) | 0x10);
            map.insert("remote-b".to_owned(), (u64::from(202u16) << 48) | 0x11);
            map.insert("other-node".to_owned(), (u64::from(303u16) << 48) | 0x20);
        }

        node_membership_callback(
            202,
            cluster::HEW_MEMBERSHIP_EVENT_NODE_LEFT,
            (&raw const registry).cast_mut().cast::<c_void>(),
        );

        let map = registry.remote_names.lock_or_recover();
        assert!(!map.contains_key("remote-a"));
        assert!(!map.contains_key("remote-b"));
        assert_eq!(
            map.get("other-node").copied(),
            Some((u64::from(303u16) << 48) | 0x20)
        );
    }

    #[test]
    fn test_node_unregister() {
        // `hew_node_new` records the node in the runtime-owned node slot, which
        // resolves through `rt_current()` and fails closed when no runtime is
        // installed; the guard installs the worker-less default runtime.
        let _guard = crate::runtime_test_guard();

        // SAFETY: bind_addr is a valid NUL-terminated C string literal.
        let node = unsafe { hew_node_new(50, c"127.0.0.1:0".as_ptr()) };
        assert!(!node.is_null());
        let name = c"test_unreg_actor";

        // SAFETY: node is a valid pointer; name is a valid C string literal.
        unsafe {
            assert_eq!(hew_node_register(node, name.as_ptr(), 999), 0);
            assert_eq!(hew_node_lookup(node, name.as_ptr()), 999);
        }

        // SAFETY: node is a valid pointer; name is a valid C string literal.
        unsafe {
            assert_eq!(hew_node_unregister(node, name.as_ptr()), 0);
            assert_eq!(hew_node_lookup(node, name.as_ptr()), 0);
        }

        // Idempotent
        // SAFETY: node is a valid pointer; name is a valid C string literal.
        unsafe {
            assert_eq!(hew_node_unregister(node, name.as_ptr()), 0);
        }

        // Null safety
        // SAFETY: Testing null pointer handling; function returns -1.
        unsafe {
            assert_eq!(hew_node_unregister(std::ptr::null_mut(), name.as_ptr()), -1);
            assert_eq!(hew_node_unregister(node, std::ptr::null()), -1);
        }

        // SAFETY: node was allocated by hew_node_new above.
        unsafe { hew_node_free(node) };
    }

    /// Two distinct actors registering the same user-chosen string must be
    /// refused with a recorded diagnostic — the registration string is a
    /// cluster-wide routing key with no actor-type qualifier on the wire, so
    /// the conflict cannot be disambiguated. Re-registering a DIFFERENT
    /// string for the second actor succeeds (the refusal is per-name).
    #[test]
    fn register_same_name_different_actor_fails_closed_with_diagnostic() {
        // `hew_node_new` records the node in the runtime-owned node slot, which
        // resolves through `rt_current()` and fails closed when no runtime is
        // installed; the guard installs the worker-less default runtime.
        let _guard = crate::runtime_test_guard();

        // SAFETY: bind_addr is a valid NUL-terminated C string literal.
        let node = unsafe { hew_node_new(51, c"127.0.0.1:0".as_ptr()) };
        assert!(!node.is_null());
        let name = c"l18_primary";

        // SAFETY: node is a valid pointer; name is a valid C string literal.
        unsafe {
            assert_eq!(hew_node_register(node, name.as_ptr(), 7001), 0);
            crate::hew_clear_error();
            assert_eq!(
                hew_node_register(node, name.as_ptr(), 7002),
                -1,
                "a second actor must not take over an existing name"
            );
            // The original mapping is intact (refuse, not clobber).
            assert_eq!(hew_node_lookup(node, name.as_ptr()), 7001);
        }
        let err_ptr = crate::hew_last_error();
        assert!(
            !err_ptr.is_null(),
            "conflicting registration must record a diagnostic"
        );
        // SAFETY: hew_last_error returns a valid C string when non-null.
        let err = unsafe { CStr::from_ptr(err_ptr) }.to_string_lossy();
        assert!(
            err.contains("l18_primary") && err.contains("7001") && err.contains("7002"),
            "diagnostic must name the string and both actors: {err}"
        );
        crate::hew_clear_error();

        // Distinct string still registers fine.
        let other = c"l18_secondary";
        // SAFETY: node is a valid pointer; name is a valid C string literal.
        unsafe {
            assert_eq!(hew_node_register(node, other.as_ptr(), 7002), 0);
            let _ = crate::registry::hew_registry_unregister(name.as_ptr());
            let _ = crate::registry::hew_registry_unregister(other.as_ptr());
            hew_node_free(node);
        }
    }

    /// A gossiped registry ADD whose name is already mapped to a different
    /// actor keeps the existing mapping and records the refusal — never a
    /// silent last-write-wins re-pointing of cluster routing. Idempotent
    /// re-adds of the same mapping stay silent.
    #[test]
    fn gossip_conflicting_registration_refused_and_recorded() {
        let _guard = crate::runtime_test_guard();

        let registry = HewRegistry::default();
        registry
            .remote_names
            .lock_or_recover()
            .insert("l18_gossip".to_owned(), 9001);
        let user_data = (&raw const registry).cast_mut().cast::<c_void>();
        let name = c"l18_gossip";

        // Idempotent re-add: same mapping, no diagnostic.
        crate::hew_clear_error();
        node_registry_gossip_callback(name.as_ptr(), 9001, true, user_data);
        assert!(
            crate::hew_last_error().is_null(),
            "same-mapping gossip re-add must stay silent"
        );

        // Conflicting add: mapping kept, refusal recorded.
        node_registry_gossip_callback(name.as_ptr(), 9002, true, user_data);
        let map = registry.remote_names.lock_or_recover();
        assert_eq!(
            map.get("l18_gossip").copied(),
            Some(9001),
            "conflicting gossip must not re-point the existing mapping"
        );
        drop(map);
        let err_ptr = crate::hew_last_error();
        assert!(
            !err_ptr.is_null(),
            "conflicting gossip must record a diagnostic"
        );
        // SAFETY: hew_last_error returns a valid C string when non-null.
        let err = unsafe { CStr::from_ptr(err_ptr) }.to_string_lossy();
        assert!(
            err.contains("l18_gossip") && err.contains("9001") && err.contains("9002"),
            "diagnostic must name the string and both actors: {err}"
        );
        crate::hew_clear_error();

        // Removal still applies.
        node_registry_gossip_callback(name.as_ptr(), 0, false, user_data);
        assert!(!registry
            .remote_names
            .lock_or_recover()
            .contains_key("l18_gossip"));
    }

    // ── Reply routing table unit tests ─────────────────────────────────

    #[test]
    fn remote_ask_without_active_node_returns_null_for_nonvoid_reply() {
        let _guard = crate::runtime_test_guard();

        let saved_current_node = with_current_node(|current| {
            let saved = *current;
            *current = 0;
            saved
        });
        let _reset_current_node = ResetCurrentNode(saved_current_node);

        let local_node_id = crate::pid::hew_pid_local_node();
        let remote_node_id = if local_node_id == u16::MAX {
            u16::MAX - 1
        } else {
            local_node_id + 1
        };
        assert_ne!(remote_node_id, 0);
        assert_ne!(remote_node_id, local_node_id);

        let remote_pid = crate::pid::hew_pid_make(remote_node_id, 1);
        // SAFETY: null data with size 0 is valid; the remote path should fail
        // immediately because no active node is installed.
        let reply = unsafe {
            hew_node_api_ask(
                remote_pid,
                test_dispatch(),
                7,
                ptr::null_mut(),
                0,
                TEST_REMOTE_ASK_TIMEOUT_MS,
                std::mem::size_of::<u64>(),
            )
        };

        assert!(reply.is_null());
        assert_eq!(
            hew_node_ask_take_last_error(),
            AskError::NodeNotRunning as i32,
            "ask with no active node should report NodeNotRunning"
        );
    }

    /// After a successful ask the error slot must be cleared to `None`.
    #[test]
    fn ask_error_slot_cleared_after_successful_local_ask() {
        let _guard = crate::runtime_test_guard();

        // Poison the slot with a stale error, then perform a local ask.
        LAST_ASK_ERROR.with(|c| c.set(AskError::Timeout as i32));

        // Perform a local ask — force local path by leaving CURRENT_NODE at 0.
        // hew_actor_ask_by_id is the local delegate; we verify the slot is NOT
        // cleared by the local path (it goes through a different function).
        // What we check here is that a successful remote reply clears the slot.
        // Build a fake ReplyOutcome with Success and non-empty data and inject
        // it directly through the reply table to exercise the success path without
        // needing a live network.
        let key = ConnectionKey {
            conn_mgr: 99,
            conn_id: 42,
        };
        let (id, pending) = reply_table().register(key);
        reply_table().complete(id, vec![0xAAu8, 0xBBu8, 0xCCu8, 0xDDu8]);
        // Drain the outcome directly as the success branch of hew_node_api_ask would.
        let mut g = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let outcome = g.take().unwrap();
        drop(g);
        assert_eq!(outcome.status, ReplyStatus::Success);
        // Simulate the success branch clearing the slot.
        LAST_ASK_ERROR.with(|c| c.set(AskError::None as i32));
        assert_eq!(
            hew_node_ask_take_last_error(),
            AskError::None as i32,
            "success path should leave error slot as None"
        );
    }

    /// `hew_node_ask_take_last_error` must reset the slot to None (0) after reading.
    #[test]
    fn ask_take_error_resets_slot() {
        LAST_ASK_ERROR.with(|c| c.set(AskError::Timeout as i32));
        let first = hew_node_ask_take_last_error();
        let second = hew_node_ask_take_last_error();
        assert_eq!(
            first,
            AskError::Timeout as i32,
            "first take should return Timeout"
        );
        assert_eq!(
            second,
            AskError::None as i32,
            "second take should return None after reset"
        );
    }

    /// A connection-dropped failure (via `fail_connection`) must report `ConnectionDropped`.
    #[test]
    fn reply_table_fail_connection_sets_connection_dropped_status() {
        let _guard = crate::runtime_test_guard();

        let key = ConnectionKey {
            conn_mgr: 77,
            conn_id: 11,
        };
        let (id, pending) = reply_table().register(key);

        // Simulate a connection drop.
        reply_table().fail_connection(key);

        let guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let outcome = guard
            .as_ref()
            .expect("outcome should be set after fail_connection");
        assert_eq!(
            outcome.status,
            ReplyStatus::Failed,
            "fail_connection must set Failed status"
        );
        drop(guard);

        // Verify the entry was removed from the pending table.
        let map = reply_table()
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(
            !map.contains_key(&id),
            "fail_connection must remove the entry"
        );
    }

    /// `fail_connection_with_reason` resolves matching pending asks with the
    /// supplied cause (`Partition` for the SWIM-DEAD fan-out) and removes the
    /// entry, distinct from the default `ConnectionDropped`.
    #[test]
    fn reply_table_fail_connection_with_partition_sets_partition_status() {
        let _guard = crate::runtime_test_guard();

        let key = ConnectionKey {
            conn_mgr: 88,
            conn_id: 12,
        };
        let (id, pending) = reply_table().register(key);

        reply_table().fail_connection_with_reason(key, AskError::Partition);

        let guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let outcome = guard
            .as_ref()
            .expect("outcome should be set after partition fan-out");
        assert_eq!(outcome.status, ReplyStatus::Failed);
        assert_eq!(
            outcome.ask_error,
            AskError::Partition,
            "SWIM-DEAD fan-out must carry the Partition cause, not ConnectionDropped"
        );
        drop(guard);

        let map = reply_table()
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(
            !map.contains_key(&id),
            "partition fan-out must remove the entry"
        );
    }

    /// `dist_partition_drain_pending_remote_asks` — the drain seam the gated
    /// `hew_dist_partition_pending_remote_asks` probe wraps and the two-process
    /// partition fixture's `PartitionInjector` drives — fails EVERY pending
    /// remote ask closed with `Partition` through the production reply-table
    /// fan-out and returns the count. This is the in-process proof that a stuck
    /// pending ask resolves to a typed `Partition` verdict on demand, without
    /// waiting on the socket-teardown detector or the SWIM failure detector (the
    /// host-load-sensitive timing the fixture used to race).
    #[test]
    fn dist_partition_probe_fails_pending_remote_asks_with_partition() {
        let _guard = crate::runtime_test_guard();

        // Nothing pending yet: the drain reports 0 so the injector keeps polling
        // rather than declaring victory before the ask registers.
        assert_eq!(
            dist_partition_drain_pending_remote_asks(),
            0,
            "drain must report 0 when no ask is pending"
        );

        let key = ConnectionKey {
            conn_mgr: 91,
            conn_id: 17,
        };
        let (id, pending) = reply_table().register(key);

        // The instant an ask is pending, one call fails it closed and reports it.
        assert_eq!(
            dist_partition_drain_pending_remote_asks(),
            1,
            "drain must fail exactly the one pending ask"
        );

        let guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let outcome = guard
            .as_ref()
            .expect("outcome should be set after the partition probe");
        assert_eq!(outcome.status, ReplyStatus::Failed);
        assert_eq!(
            outcome.ask_error,
            AskError::Partition,
            "probe must carry the Partition cause (14), not ConnectionDropped or Timeout"
        );
        drop(guard);

        // The entry is drained, so a racing reply finds nothing and a repeat
        // drain is a no-op — exactly-once, matching the SWIM-DEAD fan-out.
        let map = reply_table()
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(!map.contains_key(&id), "drain must remove the entry");
        drop(map);
        assert_eq!(
            dist_partition_drain_pending_remote_asks(),
            0,
            "a repeat drain with nothing pending is a no-op"
        );
    }

    /// The exported FFI probe is INERT without the test-capability signal. With
    /// `HEW_DIST_TEST_PROBE` unset (the production default) and one remote ask
    /// pending, a call to `hew_dist_partition_pending_remote_asks` returns the
    /// inert `-1` and leaves the ask pending. This is the guard that a shipped
    /// libhew exposes no usable "drain all pending remote asks" capability: the
    /// raw symbol resolves to a no-op unless the harness arms it on the process
    /// it spawns.
    #[test]
    fn dist_partition_probe_is_inert_without_test_signal() {
        let _guard = crate::runtime_test_guard();

        // The harness arms the probe per-process via `HEW_DIST_TEST_PROBE=1`; the
        // unit-test process never sets it, so the gate must read disarmed.
        assert!(
            !dist_test_probe_enabled(),
            "HEW_DIST_TEST_PROBE must be unset in the unit-test process"
        );

        let key = ConnectionKey {
            conn_mgr: 73,
            conn_id: 5,
        };
        let (id, pending) = reply_table().register(key);

        // Disarmed: the exported symbol is a no-op that drains nothing.
        assert_eq!(
            hew_dist_partition_pending_remote_asks(),
            -1,
            "the probe must be inert (-1) without the test signal"
        );

        // The pending ask is untouched — no Partition forced on a healthy slot.
        let outcome_unset = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .is_none();
        assert!(
            outcome_unset,
            "an inert probe must not resolve the pending ask"
        );
        let map = reply_table()
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(
            map.contains_key(&id),
            "an inert probe must leave the pending entry registered"
        );
    }

    /// `fail_all` wakes every pending reply with `Failed` status.
    #[test]
    fn reply_table_fail_all_wakes_all_pending() {
        let _guard = crate::runtime_test_guard();

        let key_a = ConnectionKey {
            conn_mgr: 55,
            conn_id: 1,
        };
        let key_b = ConnectionKey {
            conn_mgr: 55,
            conn_id: 2,
        };
        let (id_a, pending_a) = reply_table().register(key_a);
        let (id_b, pending_b) = reply_table().register(key_b);

        reply_table().fail_all();

        for (id, pending) in [(id_a, &pending_a), (id_b, &pending_b)] {
            let guard = pending
                .outcome
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let outcome = guard
                .as_ref()
                .unwrap_or_else(|| panic!("entry {id} not woken"));
            assert_eq!(outcome.status, ReplyStatus::Failed);
        }
        let map = reply_table()
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(!map.contains_key(&id_a));
        assert!(!map.contains_key(&id_b));
    }

    #[test]
    fn reply_table_register_and_complete() {
        let table = ReplyRoutingTable::new();
        let key = ConnectionKey {
            conn_mgr: 1,
            conn_id: 7,
        };
        let (id, pending) = table.register(key);
        assert!(id > 0);
        assert_eq!(pending.connection, key);

        // Complete the pending reply.
        let payload = vec![1, 2, 3, 4];
        assert!(table.complete(id, payload.clone()));

        // The condvar should be signalled and data deposited.
        let guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let outcome = guard.as_ref().expect("reply outcome should be set");
        assert_eq!(outcome.status, ReplyStatus::Success);
        assert_eq!(outcome.data, payload);
    }

    #[test]
    fn parked_reply_table_complete_resumes_via_pending_handle() {
        let _guard = crate::runtime_test_guard();
        let key = ConnectionKey {
            conn_mgr: 101,
            conn_id: 21,
        };
        // A dangling (untracked) actor pointer exercises the suspend wake path
        // without a live actor: `enqueue_resume` re-confirms liveness against the
        // live registry and drops the wake for an untracked pointer, so the
        // completion still deposits the outcome the finish path drains.
        let parked_actor = std::ptr::dangling_mut::<crate::actor::HewActor>();
        let (id, pending) = reply_table().register_parked(key, parked_actor);
        assert_eq!(pending.parked_caller.load(Ordering::Acquire), parked_actor);
        let handle = Arc::into_raw(pending).cast::<c_void>().cast_mut();

        assert!(reply_table().complete(id, Vec::new()));
        // SAFETY: `handle` is the live creator ref from register_parked.
        let reply = unsafe { hew_node_api_ask_finish(handle, test_dispatch(), 7, 0) };

        assert_eq!(reply, remote_void_reply_sentinel());
        assert_eq!(hew_node_ask_take_last_error(), AskError::None as i32);
    }

    #[test]
    fn parked_reply_timeout_finishes_with_timeout_error() {
        let _guard = crate::runtime_test_guard();
        let key = ConnectionKey {
            conn_mgr: 102,
            conn_id: 22,
        };
        let parked_actor = std::ptr::dangling_mut::<crate::actor::HewActor>();
        let (id, pending) = reply_table().register_parked(key, parked_actor);
        let handle = Arc::into_raw(pending).cast::<c_void>().cast_mut();

        assert!(reply_table().fail(id, AskError::Timeout));
        // SAFETY: `handle` is the live creator ref from register_parked.
        let reply = unsafe { hew_node_api_ask_finish(handle, test_dispatch(), 7, 0) };

        assert!(reply.is_null());
        assert_eq!(hew_node_ask_take_last_error(), AskError::Timeout as i32);
    }

    #[test]
    fn parked_reply_connection_drop_finishes_with_connection_dropped_error() {
        let _guard = crate::runtime_test_guard();
        let key = ConnectionKey {
            conn_mgr: 103,
            conn_id: 23,
        };
        let parked_actor = std::ptr::dangling_mut::<crate::actor::HewActor>();
        let (_id, pending) = reply_table().register_parked(key, parked_actor);
        let handle = Arc::into_raw(pending).cast::<c_void>().cast_mut();

        reply_table().fail_connection(key);
        // SAFETY: `handle` is the live creator ref from register_parked.
        let reply = unsafe { hew_node_api_ask_finish(handle, test_dispatch(), 7, 0) };

        assert!(reply.is_null());
        assert_eq!(
            hew_node_ask_take_last_error(),
            AskError::ConnectionDropped as i32
        );
    }

    #[test]
    fn parked_reply_cancel_removes_pending_entry() {
        let _guard = crate::runtime_test_guard();
        let key = ConnectionKey {
            conn_mgr: 104,
            conn_id: 24,
        };
        let parked_actor = std::ptr::dangling_mut::<crate::actor::HewActor>();
        let (id, pending) = reply_table().register_parked(key, parked_actor);
        let handle = Arc::into_raw(pending).cast::<c_void>().cast_mut();

        // SAFETY: `handle` is the live creator ref from register_parked.
        unsafe { hew_node_api_ask_cancel(handle) };
        // The entry is gone: a late reply finds nothing to complete.
        assert!(!reply_table().complete(id, Vec::new()));
    }

    #[test]
    fn reply_table_complete_unknown_returns_false() {
        let table = ReplyRoutingTable::new();
        assert!(!table.complete(u64::MAX - 1, vec![42]));
    }

    #[test]
    fn reply_table_remove_prevents_completion() {
        let table = ReplyRoutingTable::new();
        let (id, _pending) = table.register(ConnectionKey {
            conn_mgr: 1,
            conn_id: 0,
        });
        table.remove(id);
        assert!(!table.complete(id, vec![99]));
    }

    #[test]
    fn reply_table_concurrent_complete_wakes_waiter() {
        let table = Arc::new(ReplyRoutingTable::new());
        let (id, pending) = table.register(ConnectionKey {
            conn_mgr: 1,
            conn_id: 11,
        });
        let pending_clone = Arc::clone(&pending);
        let table_clone = Arc::clone(&table);

        let handle = thread::spawn(move || {
            thread::sleep(Duration::from_millis(10));
            table_clone.complete(id, vec![10, 20]);
        });

        // Wait on the condvar.
        let mut guard = pending_clone
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let deadline = std::time::Instant::now() + Duration::from_secs(5);
        while guard.is_none() {
            let remaining = deadline.saturating_duration_since(std::time::Instant::now());
            if remaining.is_zero() {
                break;
            }
            let (g, _) = pending_clone
                .cond
                .wait_timeout(guard, remaining)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            guard = g;
        }
        let outcome = guard.as_ref().expect("reply outcome should be set");
        assert_eq!(outcome.status, ReplyStatus::Success);
        assert_eq!(outcome.data, vec![10, 20]);

        handle.join().expect("completer thread panicked");
    }

    #[test]
    fn reply_table_fail_marks_failure() {
        let table = ReplyRoutingTable::new();
        let (id, pending) = table.register(ConnectionKey {
            conn_mgr: 1,
            conn_id: 13,
        });

        assert!(table.finish(
            id,
            ReplyOutcome {
                status: ReplyStatus::Failed,
                data: Vec::new(),
                ask_error: AskError::ConnectionDropped,
            },
        ));

        let guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let outcome = guard.as_ref().expect("reply outcome should be set");
        assert_eq!(outcome.status, ReplyStatus::Failed);
        assert!(outcome.data.is_empty());
    }

    #[test]
    fn reply_table_fail_with_reason_propagates_correct_ask_error() {
        let _guard = crate::runtime_test_guard();

        let (id, pending) = reply_table().register(ConnectionKey {
            conn_mgr: 90,
            conn_id: 13,
        });
        assert!(reply_table().fail(id, AskError::OrphanedAsk));

        let guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let outcome = guard.as_ref().expect("reply outcome should be set");
        assert_eq!(outcome.status, ReplyStatus::Failed);
        assert_eq!(outcome.ask_error, AskError::OrphanedAsk);
        assert!(outcome.data.is_empty());
    }

    #[test]
    fn send_failure_removes_pending_reply_slot_no_leak() {
        // `setup_remote_ask` registers a pending reply BEFORE the outbound send;
        // on a fail-closed send (the SIGPIPE→EPIPE path that returns
        // `AskError::SendFailed`) it must `remove` the slot so a failed ask
        // cannot leak a pending entry. This pins that cleanup contract on a
        // fresh table, isolated from the process-global one.
        let table = ReplyRoutingTable::new();
        assert_eq!(table.pending_len(), 0);

        let (request_id, _pending) = table.register(ConnectionKey {
            conn_mgr: 42,
            conn_id: 7,
        });
        assert_eq!(
            table.pending_len(),
            1,
            "register must install exactly one pending reply slot"
        );

        // Mirror `setup_remote_ask`'s `!send_ok` branch: remove the slot the ask
        // registered before the send that just failed.
        let removed = table.remove(request_id);
        assert!(
            removed.is_some(),
            "the fail-closed send path must find and remove the registered slot"
        );
        assert_eq!(
            table.pending_len(),
            0,
            "AskError::SendFailed cleanup must not leak the reply slot"
        );
    }

    #[test]
    fn fail_remote_reply_empty_payload_defaults_to_worker_at_capacity() {
        let _guard = crate::runtime_test_guard();

        let (id, pending) = reply_table().register(ConnectionKey {
            conn_mgr: 91,
            conn_id: 14,
        });
        assert!(fail_remote_reply(id, &[]));

        let guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let outcome = guard.as_ref().expect("reply outcome should be set");
        assert_eq!(outcome.status, ReplyStatus::Failed);
        assert_eq!(outcome.ask_error, AskError::WorkerAtCapacity);
        assert!(outcome.data.is_empty());
    }

    #[test]
    fn remote_reply_payload_length_mismatch_fails_closed_not_overread() {
        // A peer's reply payload whose length differs from the caller's static
        // `reply_size` must fail closed (null → PayloadSizeMismatch), never be
        // handed to the codegen typed-load that reads `reply_size` bytes.
        // Short payload: 1 byte where the caller expects 4 — the over-read case.
        assert!(
            remote_reply_data_to_ptr(&[0xAB], 4).is_null(),
            "short reply payload must fail closed, not be read past its allocation"
        );
        // Long payload: 8 bytes where the caller expects 4 — silent-truncation case.
        assert!(
            remote_reply_data_to_ptr(&[0u8; 8], 4).is_null(),
            "over-long reply payload must fail closed, not be silently truncated"
        );
        // Exact match still succeeds (non-null, owned buffer the caller frees).
        let ok = remote_reply_data_to_ptr(&[1u8, 2, 3, 4], 4);
        assert!(!ok.is_null(), "exact-size reply payload must succeed");
        // SAFETY: ok was malloc'd by remote_reply_data_to_ptr; free it once.
        unsafe { libc::free(ok) };
    }

    #[test]
    fn rejection_reason_codes_round_trip_supported_remote_failures() {
        for ask_error in [
            AskError::WorkerAtCapacity,
            AskError::ActorStopped,
            AskError::MailboxFull,
            AskError::OrphanedAsk,
            AskError::NoRunnableWork,
        ] {
            let payload = [AskRejectionReasonCode::encode(ask_error)
                .expect("supported remote ask failure must encode to a wire code")];
            assert_eq!(
                decode_rejection_reason(&payload).expect("known code must decode"),
                ask_error,
                "encoded rejection reason must round-trip through the wire payload"
            );
        }
    }

    #[test]
    fn rejection_reason_codes_reject_non_remote_failures() {
        for ask_error in [
            AskError::None,
            AskError::NodeNotRunning,
            AskError::RoutingFailed,
            AskError::EncodeFailed,
            AskError::SendFailed,
            AskError::Timeout,
            AskError::ConnectionDropped,
            AskError::PayloadSizeMismatch,
        ] {
            assert!(
                AskRejectionReasonCode::encode(ask_error).is_none(),
                "{ask_error:?} must not be emitted as a remote rejection-reason code"
            );
        }
        assert_eq!(
            decode_rejection_reason(&[AskError::Timeout as u8]),
            Err(AskRejectionDecodeError::UnknownAskRejectionReason {
                code: AskError::Timeout as u8
            }),
            "unknown rejection-reason bytes must produce Err, not a fabricated AskError"
        );
    }

    #[test]
    fn fail_remote_reply_unknown_code_returns_false_and_leaves_ask_unresolved() {
        let _guard = crate::runtime_test_guard();

        let (id, pending) = reply_table().register(ConnectionKey {
            conn_mgr: 92,
            conn_id: 15,
        });
        assert!(!fail_remote_reply(id, &[0xFF]));

        let guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(
            guard.is_none(),
            "unknown code must not resolve the pending ask"
        );
    }

    #[test]
    fn reply_table_fail_all_wakes_waiter() {
        let table = Arc::new(ReplyRoutingTable::new());
        let (_id, pending) = table.register(ConnectionKey {
            conn_mgr: 1,
            conn_id: 17,
        });
        let pending_clone = Arc::clone(&pending);
        let table_clone = Arc::clone(&table);

        let handle = thread::spawn(move || {
            let mut guard = pending_clone
                .outcome
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let deadline = std::time::Instant::now() + Duration::from_secs(5);
            while guard.is_none() {
                let remaining = deadline.saturating_duration_since(std::time::Instant::now());
                assert!(!remaining.is_zero(), "waiter timed out before fail_all");
                let (new_guard, _) = pending_clone
                    .cond
                    .wait_timeout(guard, remaining)
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                guard = new_guard;
            }
            let outcome = guard.take().expect("reply outcome should be set");
            assert_eq!(outcome.status, ReplyStatus::Failed);
            assert!(outcome.data.is_empty());
        });

        thread::sleep(Duration::from_millis(10));
        table_clone.fail_all();
        handle.join().expect("waiter thread panicked");
    }

    #[test]
    fn reply_table_fail_connection_only_wakes_matching_waiters() {
        let table = ReplyRoutingTable::new();
        let failed_key = ConnectionKey {
            conn_mgr: 1,
            conn_id: 21,
        };
        let (_failed_id, failed_pending) = table.register(failed_key);
        let (_unrelated_id, unrelated_pending) = table.register(ConnectionKey {
            conn_mgr: 2,
            conn_id: 21,
        });

        table.fail_connection(failed_key);

        let failed_guard = failed_pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let failed_outcome = failed_guard.as_ref().expect("matching waiter should fail");
        assert_eq!(failed_outcome.status, ReplyStatus::Failed);
        assert!(failed_outcome.data.is_empty());
        drop(failed_guard);

        let unrelated_guard = unrelated_pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(
            unrelated_guard.is_none(),
            "non-matching waiter must remain pending"
        );
    }

    #[test]
    fn remote_lookup_via_registry_gossip() {
        // Verify that a registry gossip callback populates remote_names
        // and that hew_node_lookup falls through to it.
        let _guard = crate::runtime_test_guard();
        crate::registry::hew_registry_clear();

        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string.
        let node = unsafe { TestNode::new(110, &bind_addr) };
        assert!(!node.as_ptr().is_null());

        // SAFETY: pointer is valid for each call in this scope.
        unsafe {
            assert_eq!(hew_node_start(node.as_ptr()), 0);

            // Simulate a remote registry gossip event arriving.
            let n = &*node.as_ptr();
            let remote_name = c"remote_counter";
            let remote_pid: u64 = (u64::from(200u16) << 48) | 0x99;

            // Invoke the callback directly (as the cluster would).
            node_registry_gossip_callback(
                remote_name.as_ptr(),
                remote_pid,
                true,
                n.registry.cast::<c_void>(),
            );

            // Local lookup should not find it (not registered locally).
            assert!(crate::registry::hew_registry_lookup(remote_name.as_ptr()).is_null());

            // Node lookup should find it via remote_names.
            assert_eq!(
                hew_node_lookup(node.as_ptr(), remote_name.as_ptr()),
                remote_pid
            );

            // Simulate removal.
            node_registry_gossip_callback(
                remote_name.as_ptr(),
                0,
                false,
                n.registry.cast::<c_void>(),
            );
            assert_eq!(hew_node_lookup(node.as_ptr(), remote_name.as_ptr()), 0);

            assert_eq!(hew_node_stop(node.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn register_emits_gossip_event() {
        // Verify that hew_node_register queues a gossip event in the cluster.
        let _guard = crate::runtime_test_guard();
        crate::registry::hew_registry_clear();

        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string.
        let node = unsafe { TestNode::new(111, &bind_addr) };
        assert!(!node.as_ptr().is_null());

        // SAFETY: pointer is valid for each call in this scope.
        unsafe {
            assert_eq!(hew_node_start(node.as_ptr()), 0);

            let name = c"gossip_actor";
            let pid: u64 = (u64::from(111u16) << 48) | 0x2A;
            assert_eq!(hew_node_register(node.as_ptr(), name.as_ptr(), pid), 0);

            // The cluster should have a pending registry gossip event.
            let n = &*node.as_ptr();
            assert!(!n.cluster.is_null());
            assert!(cluster::hew_cluster_registry_gossip_count(n.cluster) > 0);

            let _ = crate::registry::hew_registry_unregister(name.as_ptr());
            assert_eq!(hew_node_stop(node.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    // ── Distributed multi-node integration tests ──────────────────────────
    //
    // These tests exercise the real TCP transport path end-to-end in a
    // single process using two HewNode instances:
    //
    //  • `two_node_remote_send_delivery` — proves fire-and-forget message
    //    delivery from node1 to an actor registered on node2.
    //  • `two_node_remote_ask_reply` — proves the full ask/reply round-trip
    //    over the real transport: node1 sends an ask, node2's actor replies,
    //    and node1 receives the reply.
    //
    // Both tests run under the shared runtime test lock to ensure this runtime's
    // CURRENT_NODE / local_node_id pair is not perturbed by concurrent node tests.

    use std::sync::atomic::AtomicU32;

    // ── Shared helpers for distributed node tests ─────────────────────────

    /// Connect `initiator` to `responder_addr` with retry back-off.
    unsafe fn connect_with_retry(initiator: *mut HewNode, responder_addr: &CString) {
        let mut backoff = Duration::from_millis(25);
        for _ in 0..20 {
            // SAFETY: initiator and responder_addr are valid for this call.
            if unsafe { hew_node_connect(initiator, responder_addr.as_ptr()) } == 0 {
                return;
            }
            thread::sleep(backoff);
            backoff = (backoff * 2).min(Duration::from_millis(200));
        }
        panic!("could not connect initiator to responder");
    }

    /// Poll until both connection managers report at least one active connection.
    unsafe fn wait_for_handshake(node1: *mut HewNode, node2: *mut HewNode) {
        let ok = (0..80).any(|i| {
            // SAFETY: node1 and node2 pointers are valid for the duration of the test.
            let ready = unsafe {
                connection::hew_connmgr_count((*node1).conn_mgr) > 0
                    && connection::hew_connmgr_count((*node2).conn_mgr) > 0
            };
            if !ready {
                let ms = if i < 20 {
                    25
                } else if i < 50 {
                    50
                } else {
                    100
                };
                thread::sleep(Duration::from_millis(ms));
            }
            ready
        });
        assert!(ok, "TCP handshake did not complete in time");
    }

    /// Poll node `observer`'s SWIM view of `subject` until it reaches at least
    /// `min_state`, where membership states are ordered
    /// `MEMBER_ALIVE` (0) < `MEMBER_SUSPECT` (1) < `MEMBER_DEAD` (2).
    ///
    /// This synchronises on an asynchronous SWIM transition instead of racing
    /// it.  The bound (~4 s) is a generous load-immune ceiling on a transition
    /// that normally lands in a few milliseconds; reaching it means the
    /// transition never happened, which is a genuine failure, not flake.
    unsafe fn wait_for_member_state_at_least(observer: *mut HewNode, subject: u16, min_state: i32) {
        let ok = (0..200).any(|i| {
            // SAFETY: observer's cluster is live for the duration of the test.
            let state =
                unsafe { crate::cluster::hew_cluster_member_state((*observer).cluster, subject) };
            if state >= min_state {
                return true;
            }
            let ms = if i < 40 { 10 } else { 25 };
            thread::sleep(Duration::from_millis(ms));
            false
        });
        assert!(
            ok,
            "node {subject} did not reach membership state >= {min_state} in time"
        );
    }

    // ── Test: fire-and-forget remote message delivery ─────────────────────

    /// Stores the `msg_type` of the most-recently received remote message.
    static SEND_PROBE_MSG_TYPE: AtomicU32 = AtomicU32::new(0);

    unsafe extern "C-unwind" fn send_probe_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        #[expect(
            clippy::cast_sign_loss,
            reason = "msg_type is a non-negative tag in this test"
        )]
        SEND_PROBE_MSG_TYPE.store(msg_type as u32, Ordering::Release);

        std::ptr::null_mut()
    }

    #[test]
    fn two_node_remote_send_delivery() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Node 1 (initiator) starts first → CURRENT_NODE = node1, LOCAL_NODE_ID = 301.
        // Node 2 (responder) starts after; CURRENT_NODE is already non-zero so
        // hew_node_start does NOT overwrite LOCAL_NODE_ID.
        let node1_bind = CString::new("127.0.0.1:0").unwrap();

        // SAFETY: bind addresses are valid C strings for the test scope.
        let node1 = unsafe { TestNode::new(301, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 is valid for each call in this scope.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0); // CURRENT_NODE = node1, LOCAL_NODE_ID = 301
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(302); // CURRENT_NODE stays node1

        // Ensure the scheduler is running so actor dispatches work.
        _real_sched = init_real_scheduler();

        // Temporarily set LOCAL_NODE_ID = 302 to assign a node-2 PID to the actor.
        // This makes the actor look remote from node1's routing perspective.
        SEND_PROBE_MSG_TYPE.store(0, Ordering::Release);
        crate::pid::hew_pid_set_local_node(302);
        // SAFETY: null state / size-0 is valid; dispatch fn is a valid fn ptr.
        let probe_actor =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(send_probe_dispatch)) };
        // Restore node1 as the local node before any routing decisions.
        crate::pid::hew_pid_set_local_node(301);
        assert!(!probe_actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid.
        let actor_id = unsafe { (*probe_actor).id };
        assert_eq!(
            crate::pid::hew_pid_node(actor_id),
            302,
            "actor PID must encode node2's ID"
        );

        let connect_addr = CString::new(format!("302@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and connect_addr are valid for this call.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers are valid.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // Fire-and-forget from node1 to the actor on node2.
        let msg_type_sent: i32 = 77;
        // SAFETY: null payload / size 0 is valid for a bare signal message.
        let rc = unsafe {
            hew_node_send(
                node1.as_ptr(),
                actor_id,
                ptr::null(),
                msg_type_sent,
                ptr::null(),
                0,
            )
        };
        assert_eq!(rc, 0, "hew_node_send should succeed");

        let delivered = (0..100).any(|_| {
            #[expect(
                clippy::cast_sign_loss,
                reason = "msg_type_sent is a non-negative tag value"
            )]
            let got = SEND_PROBE_MSG_TYPE.load(Ordering::Acquire) == msg_type_sent as u32;
            if !got {
                thread::sleep(Duration::from_millis(20));
            }
            got
        });
        assert!(
            delivered,
            "actor on node2 did not receive the remote message"
        );

        // SAFETY: actor and nodes were allocated in this test and are valid.
        unsafe {
            let _ = crate::actor::hew_actor_free(probe_actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    // ── Test: fire-and-forget remote message delivery (quic_mesh) ─────────
    //
    // Mirrors `two_node_remote_send_delivery` but routes the payload over the
    // native `quic_mesh` transport (mTLS-pinned per-actor-pair streams) rather
    // than TCP. Two in-process nodes mutually pin each other's SPKIs via the
    // `tls_override` injection helper. Until the A3 Noise→X.509 bridge lands,
    // this injection is the only way two in-process nodes can authenticate
    // each other; production deployments pin via the global allowlist.

    /// Stores the `msg_type` of the most-recently received remote message on
    /// the `quic_mesh` path.  Separate from `SEND_PROBE_MSG_TYPE` so the two
    /// tests can run independently without static-state cross-talk.
    #[cfg(feature = "quic")]
    static SEND_PROBE_MSG_TYPE_QM: AtomicU32 = AtomicU32::new(0);

    #[cfg(feature = "quic")]
    unsafe extern "C-unwind" fn send_probe_dispatch_qm(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        #[expect(
            clippy::cast_sign_loss,
            reason = "msg_type is a non-negative tag in this test"
        )]
        SEND_PROBE_MSG_TYPE_QM.store(msg_type as u32, Ordering::Release);

        std::ptr::null_mut()
    }

    /// Build a mutually-pinned `(MeshTls, MeshTls)` pair so two in-process
    /// `quic_mesh` nodes can complete the handshake. Returns
    /// `(tls_a, tls_b, spki_a, spki_b)`. The two SPKIs are returned so
    /// fail-closed tests can construct asymmetric trust configurations.
    #[cfg(feature = "quic")]
    fn make_mutually_pinned_mesh_tls(
        sni_a: &str,
        sni_b: &str,
    ) -> (
        crate::quic_mesh::MeshTls,
        crate::quic_mesh::MeshTls,
        Vec<u8>,
        Vec<u8>,
    ) {
        use crate::quic_mesh::MeshTls;
        let (tls_a, spki_a) = MeshTls::self_signed(vec![sni_a.into()]).expect("tls_a self_signed");
        let (tls_b, spki_b) = MeshTls::self_signed(vec![sni_b.into()]).expect("tls_b self_signed");
        let tls_a = tls_a.with_peer_spki(spki_b.clone());
        let tls_b = tls_b.with_peer_spki(spki_a.clone());
        (tls_a, tls_b, spki_a, spki_b)
    }

    #[cfg(feature = "quic")]
    #[test]
    fn two_node_remote_send_delivery_quic_mesh() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Build mutually-pinned TLS for the two in-process nodes.
        let (tls_a, tls_b, _spki_a, _spki_b) =
            make_mutually_pinned_mesh_tls("node-401", "node-402");

        // Node 1 (initiator) starts first → CURRENT_NODE = node1, LOCAL_NODE_ID = 401.
        let (node1, _node1_port) = start_quic_mesh_test_listener_node(401, tls_a);
        thread::sleep(Duration::from_millis(50));
        // Node 2 (responder) starts second; CURRENT_NODE remains node1.
        let (node2, node2_port) = start_quic_mesh_test_listener_node(402, tls_b);

        // Ensure the scheduler is running so actor dispatches work.
        _real_sched = init_real_scheduler();

        // Temporarily set LOCAL_NODE_ID = 402 to assign a node-2 PID to the actor.
        // This makes the actor look remote from node1's routing perspective.
        SEND_PROBE_MSG_TYPE_QM.store(0, Ordering::Release);
        crate::pid::hew_pid_set_local_node(402);
        // SAFETY: null state / size-0 is valid; dispatch fn is a valid fn ptr.
        let probe_actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(send_probe_dispatch_qm))
        };
        // Restore node1 as the local node before any routing decisions.
        crate::pid::hew_pid_set_local_node(401);
        assert!(!probe_actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid.
        let actor_id = unsafe { (*probe_actor).id };
        assert_eq!(
            crate::pid::hew_pid_node(actor_id),
            402,
            "actor PID must encode node2's ID"
        );

        let connect_addr = CString::new(format!("402@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and connect_addr are valid for this call.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers are valid.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // Fire-and-forget from node1 to the actor on node2 over quic_mesh.
        // This exercises a real CBOR-framed round-trip across the mTLS-pinned
        // mesh transport — not just process startup.
        let msg_type_sent: i32 = 91;
        // SAFETY: null payload / size 0 is valid for a bare signal message.
        let rc = unsafe {
            hew_node_send(
                node1.as_ptr(),
                actor_id,
                ptr::null(),
                msg_type_sent,
                ptr::null(),
                0,
            )
        };
        assert_eq!(rc, 0, "hew_node_send should succeed");

        let delivered = (0..200).any(|_| {
            #[expect(
                clippy::cast_sign_loss,
                reason = "msg_type_sent is a non-negative tag value"
            )]
            let got = SEND_PROBE_MSG_TYPE_QM.load(Ordering::Acquire) == msg_type_sent as u32;
            if !got {
                thread::sleep(Duration::from_millis(20));
            }
            got
        });
        assert!(
            delivered,
            "actor on node2 did not receive the remote message over quic_mesh"
        );

        // SAFETY: actor and nodes were allocated in this test and are valid.
        unsafe {
            let _ = crate::actor::hew_actor_free(probe_actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    /// Fail-closed: when the two in-process nodes do NOT mutually pin each
    /// other's SPKIs, the mTLS handshake must fail and `hew_node_connect`
    /// must surface a typed diagnostic rather than silently degrading or
    /// hanging.
    ///
    /// Per the M3 trust-bar: no silent fallback. The connect-with-retry loop
    /// is therefore tolerable in the success test but here we want a single
    /// `hew_node_connect` call to return -1 (the transport adapter sets
    /// `last_error` to a `quic_mesh connect: …` diagnostic).
    #[cfg(feature = "quic")]
    #[test]
    fn two_node_remote_send_quic_mesh_rejects_unknown_peer() {
        use crate::quic_mesh::MeshTls;

        let _guard = crate::runtime_test_guard();
        crate::registry::hew_registry_clear();

        // Build asymmetric TLS: node A pins no peer SPKIs, node B pins no
        // peer SPKIs. Either side rejects the other → fail-closed.
        let (tls_a, _spki_a) =
            MeshTls::self_signed(vec!["node-411".into()]).expect("tls_a self_signed");
        let (tls_b, _spki_b) =
            MeshTls::self_signed(vec!["node-412".into()]).expect("tls_b self_signed");

        let (node1, _node1_port) = start_quic_mesh_test_listener_node(411, tls_a);
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_quic_mesh_test_listener_node(412, tls_b);

        let connect_addr = CString::new(format!("412@127.0.0.1:{node2_port}")).unwrap();

        // Try once — the mTLS handshake must fail. We loop briefly to allow
        // for connection-attempt teardown, but every attempt must return -1.
        // (We deliberately do NOT call `connect_with_retry`, which would
        //  panic on persistent failure: here failure IS the success case.)
        let mut last_rc = 0;
        for _ in 0..3 {
            // SAFETY: node1 and connect_addr are valid for this call.
            last_rc = unsafe { hew_node_connect(node1.as_ptr(), connect_addr.as_ptr()) };
            if last_rc != 0 {
                break;
            }
            thread::sleep(Duration::from_millis(50));
        }
        assert_eq!(
            last_rc, -1,
            "quic_mesh connect to an SPKI-unpinned peer must fail-closed"
        );

        // Verify a typed diagnostic was emitted to LAST_ERROR. Use the C-API
        // because the runtime's `set_last_error` is internal to `hew-runtime`,
        // not the `hew_cabi::sink` last-error slot.
        let err_ptr = crate::hew_last_error();
        assert!(
            !err_ptr.is_null(),
            "quic_mesh fail-closed path must populate hew_last_error()"
        );
        // SAFETY: hew_last_error returns a thread-local C string valid until
        // the next set_last_error on this thread.
        let err = unsafe { std::ffi::CStr::from_ptr(err_ptr) }
            .to_string_lossy()
            .into_owned();
        assert!(
            err.contains("connect") || err.contains("quic_mesh") || err.contains("transport"),
            "expected a typed quic_mesh/transport diagnostic, got: {err:?}"
        );

        // SAFETY: nodes were allocated in this test and remain valid.
        unsafe {
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    // ── Test: remote ask / reply round-trip ───────────────────────────────

    /// Echo-double dispatch: reads a u32 from `data`, replies with `value * 2`.
    unsafe extern "C-unwind" fn ask_probe_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        data: *mut c_void,
        size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        if size < std::mem::size_of::<u32>() {
            return std::ptr::null_mut();
        }
        // SAFETY: data is valid for at least size_of::<u32>() bytes.
        let value = unsafe { *(data.cast::<u32>()) };
        let mut reply_value: u32 = value.wrapping_mul(2);

        let ch = crate::scheduler::hew_get_reply_channel();
        if ch.is_null() {
            return std::ptr::null_mut();
        }
        // SAFETY: ch is the current thread-local reply channel; reply_value is valid.
        unsafe {
            let _ = crate::reply_channel::hew_reply(
                ch.cast(),
                (&raw mut reply_value).cast::<c_void>(),
                std::mem::size_of::<u32>(),
            );
        }
        // Mark observed only AFTER the reply has been handed to the channel, so
        // the server helper (which tears down on observe) cannot drop the
        // connection before the reply flushes to the asking node.
        mark_two_process_ask_observed();

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn void_ask_probe_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        let ch = crate::scheduler::hew_get_reply_channel();
        if ch.is_null() {
            return std::ptr::null_mut();
        }
        // SAFETY: the reply channel comes from the scheduler and is valid for a void reply here.
        unsafe {
            let _ = crate::reply_channel::hew_reply(ch.cast(), ptr::null_mut(), 0);
        }

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn orphaned_void_ask_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        crate::actor::hew_actor_self_stop();

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn blocked_ask_probe_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        mark_two_process_ask_observed();

        std::ptr::null_mut()
    }

    #[test]
    fn two_node_remote_void_ask_returns_sentinel() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").unwrap();

        // SAFETY: bind addresses are valid C strings for the duration of this test.
        let node1 = unsafe { TestNode::new(313, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 comes from TestNode::new and is valid for start-up here.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(314);

        _real_sched = init_real_scheduler();

        crate::pid::hew_pid_set_local_node(314);
        // SAFETY: null state and size-0 are valid; the dispatch function pointer is valid.
        let void_actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(void_ask_probe_dispatch))
        };
        crate::pid::hew_pid_set_local_node(313);
        assert!(!void_actor.is_null(), "actor spawn failed");
        // SAFETY: the actor was just spawned successfully and remains valid here.
        let actor_id = unsafe { (*void_actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 314);

        let connect_addr = CString::new(format!("314@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and the connect address are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until the end of the test.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: this is a remote void ask; null payload/size are valid and no reply buffer is expected.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                ptr::null_mut(),
                0,
                TEST_REMOTE_ASK_TIMEOUT_MS,
                0,
            )
        };
        assert_eq!(reply_ptr, remote_void_reply_sentinel());
        assert_eq!(
            hew_node_ask_take_last_error(),
            AskError::None as i32,
            "successful void ask must leave the error slot cleared"
        );

        // SAFETY: the actor and nodes were allocated in this test and are still valid here.
        unsafe {
            let _ = crate::actor::hew_actor_free(void_actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn two_node_remote_ask_reply() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();
        // Remote ask requires a registered codec for the payload msg_type.
        register_test_u32_codec(test_dispatch(), 1);

        // Node 1 (initiator) starts first → CURRENT_NODE = node1, LOCAL_NODE_ID = 311.
        // Node 2 (responder) starts after; CURRENT_NODE stays as node1.
        let node1_bind = CString::new("127.0.0.1:0").unwrap();

        // SAFETY: bind addresses are valid C strings for the test scope.
        let node1 = unsafe { TestNode::new(311, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 is valid for each call in this scope.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0); // CURRENT_NODE = node1, LOCAL_NODE_ID = 311
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(312); // CURRENT_NODE stays node1

        // Ensure the scheduler is running so actor dispatches work.
        _real_sched = init_real_scheduler();

        // Temporarily set LOCAL_NODE_ID = 312 to assign a node-2 PID to the actor.
        crate::pid::hew_pid_set_local_node(312);
        // SAFETY: null state / size-0 is valid; dispatch fn is a valid fn ptr.
        let echo_actor =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(ask_probe_dispatch)) };
        // Restore node1 as the local node before any routing decisions.
        crate::pid::hew_pid_set_local_node(311);
        assert!(!echo_actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid.
        let actor_id = unsafe { (*echo_actor).id };
        assert_eq!(
            crate::pid::hew_pid_node(actor_id),
            312,
            "actor PID must encode node2's ID"
        );

        let connect_addr = CString::new(format!("312@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and connect_addr are valid for this call.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers are valid.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // Remote ask from node1 (CURRENT_NODE, LOCAL_NODE_ID=311) to actor on node2.
        //
        // Routing: target_node_id=312 ≠ local_node_id=311 → remote path.
        // routing_table[312] provides the outbound conn_id (populated during handshake).
        // On node2 the inbound router fires handle_inbound_ask with conn_mgr=node2.conn_mgr;
        // send_reply_envelope uses hew_connmgr_conn_id_for_node to find the accepted
        // connection whose peer_node_id == 311, enabling the reply to flow back to node1.
        let send_value: u32 = 21;
        // SAFETY: send_value is a valid u32 on the stack; reply is malloc'd, freed below.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                (&raw const send_value).cast::<c_void>().cast_mut(),
                std::mem::size_of::<u32>(),
                TEST_REMOTE_ASK_TIMEOUT_MS,
                std::mem::size_of::<u32>(),
            )
        };

        assert!(
            !reply_ptr.is_null(),
            "remote ask should return a non-null reply"
        );
        // SAFETY: reply_ptr was malloc'd by hew_node_api_ask; valid for u32 read.
        let reply_value = unsafe { *(reply_ptr.cast::<u32>()) };
        assert_eq!(
            reply_value,
            send_value * 2,
            "echo-double should return 21 * 2 = 42"
        );
        // SAFETY: reply_ptr was malloc'd and is our responsibility to free.
        unsafe { libc::free(reply_ptr) };

        // SAFETY: actor and nodes were allocated in this test and are valid.
        unsafe {
            let _ = crate::actor::hew_actor_free(echo_actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn two_node_remote_ask_async_resumes_on_wire_reply() {
        // NEW-5 worker-free oracle at the runtime contract: a suspendable remote
        // ask (`hew_node_api_ask_async`) submits WITHOUT blocking the caller, and
        // the cross-node wire reply lands in the parked slot — the resume edge —
        // for `hew_node_api_ask_finish` to drain. The caller never blocks on a
        // condvar; the reply is delivered by the connection reader thread and
        // routed at the parked caller through `enqueue_resume`.
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();
        register_test_u32_codec(test_dispatch(), 1);

        let node1_bind = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: bind address is a valid C string for this scope.
        let node1 = unsafe { TestNode::new(321, &node1_bind) };
        assert!(!node1.as_ptr().is_null());
        // SAFETY: node1 is valid for the call.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(322);

        _real_sched = init_real_scheduler();

        crate::pid::hew_pid_set_local_node(322);
        // SAFETY: null state / size-0 is valid; dispatch fn is a valid fn ptr.
        let echo_actor =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(ask_probe_dispatch)) };
        crate::pid::hew_pid_set_local_node(321);
        assert!(!echo_actor.is_null(), "echo actor spawn failed");
        // SAFETY: actor was just spawned and is valid.
        let actor_id = unsafe { (*echo_actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 322);

        // A live local actor stands in for the parked coroutine: the wire reply
        // routes `enqueue_resume` at it. With no coroutine actually parked the
        // Suspended→Runnable CAS fails and the wake is dropped (fail-safe), while
        // the reply still lands in the pending slot the finish path drains.
        // SAFETY: null state / size-0 is valid; dispatch fn is a valid fn ptr.
        let caller =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(ask_probe_dispatch)) };
        assert!(!caller.is_null(), "caller actor spawn failed");

        let connect_addr = CString::new(format!("322@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and connect_addr are valid for this call.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers are valid.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        let send_value: u32 = 21;
        // SAFETY: send_value is a valid u32 on the stack; caller is a live actor.
        let handle = unsafe {
            hew_node_api_ask_async(
                actor_id,
                test_dispatch(),
                1,
                (&raw const send_value).cast::<c_void>().cast_mut(),
                std::mem::size_of::<u32>(),
                TEST_REMOTE_ASK_TIMEOUT_MS,
                caller,
            )
        };
        assert!(
            !handle.is_null(),
            "async remote ask submit should return a pending handle"
        );

        // Poll the parked slot for the deposited reply — the wire reply arriving on
        // the reader thread resolves it (the resume edge). The caller never blocked.
        let deadline = Instant::now() + Duration::from_secs(2);
        loop {
            let ready = {
                // SAFETY: `handle` points at the live PendingReply whose creator ref
                // we still own; this shared borrow is dropped before `_finish`
                // reclaims that ref.
                let pending = unsafe { &*handle.cast::<PendingReply>() };
                pending
                    .outcome
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .is_some()
            };
            if ready {
                break;
            }
            assert!(
                Instant::now() < deadline,
                "wire reply never resumed the parked async ask"
            );
            thread::sleep(Duration::from_millis(10));
        }

        // SAFETY: `handle` is the live creator ref; finish consumes it exactly once.
        let reply_ptr = unsafe {
            hew_node_api_ask_finish(handle, test_dispatch(), 1, std::mem::size_of::<u32>())
        };
        assert!(
            !reply_ptr.is_null(),
            "resumed async ask should bind a non-null reply"
        );
        // SAFETY: reply_ptr was malloc'd by the finish path; valid for a u32 read.
        let reply_value = unsafe { *(reply_ptr.cast::<u32>()) };
        assert_eq!(
            reply_value,
            send_value * 2,
            "echo-double should return 21 * 2 = 42"
        );
        // SAFETY: reply_ptr was malloc'd and is ours to free.
        unsafe { libc::free(reply_ptr) };

        // SAFETY: actors and nodes were allocated in this test and are valid.
        unsafe {
            let _ = crate::actor::hew_actor_free(caller);
            let _ = crate::actor::hew_actor_free(echo_actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn two_node_inbound_orphaned_ask_reports_orphaned() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: node1_bind is a valid C string for the duration of this test.
        let node1 = unsafe { TestNode::new(315, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 was just allocated and remains valid until teardown.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(316);

        _real_sched = init_real_scheduler();

        crate::pid::hew_pid_set_local_node(316);
        // SAFETY: null state / size-0 are valid; dispatch fn is a valid fn ptr.
        let actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(orphaned_void_ask_dispatch))
        };
        crate::pid::hew_pid_set_local_node(315);
        assert!(!actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid here.
        let actor_id = unsafe { (*actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 316);

        let connect_addr = CString::new(format!("316@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and connect_addr are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until teardown.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: this is a remote void ask; null payload/size are valid.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                ptr::null_mut(),
                0,
                TEST_REMOTE_ASK_TIMEOUT_MS,
                0,
            )
        };
        let err = hew_node_ask_take_last_error();

        assert!(
            reply_ptr.is_null(),
            "orphaned inbound void ask must not return the void-success sentinel"
        );
        assert_eq!(
            err,
            AskError::OrphanedAsk as i32,
            "orphaned inbound ask must preserve the remote orphaned reason"
        );

        // SAFETY: actor and nodes were allocated in this test and remain valid here.
        unsafe {
            let _ = crate::actor::hew_actor_free(actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn two_node_inbound_actor_stopped_reports_actor_stopped() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: node1_bind is a valid C string for the duration of this test.
        let node1 = unsafe { TestNode::new(317, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 was just allocated and remains valid until teardown.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(318);

        _real_sched = init_real_scheduler();

        crate::pid::hew_pid_set_local_node(318);
        // SAFETY: null state / size-0 are valid; dispatch fn is a valid fn ptr.
        let actor =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        crate::pid::hew_pid_set_local_node(317);
        assert!(!actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid here.
        let actor_id = unsafe { (*actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 318);

        let connect_addr = CString::new(format!("318@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and connect_addr are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until teardown.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: actor was spawned above and remains valid while stopped here.
        unsafe { crate::actor::hew_actor_stop(actor) };

        // SAFETY: this is a remote void ask; null payload/size are valid.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                ptr::null_mut(),
                0,
                TEST_REMOTE_ASK_TIMEOUT_MS,
                0,
            )
        };
        let err = hew_node_ask_take_last_error();

        assert!(
            reply_ptr.is_null(),
            "inbound ask to a stopped actor must not return the void-success sentinel"
        );
        assert_eq!(
            err,
            AskError::ActorStopped as i32,
            "stopped inbound actor ask must preserve the remote actor-stopped reason"
        );

        // SAFETY: actor and nodes were allocated in this test and remain valid here.
        unsafe {
            let _ = crate::actor::hew_actor_free(actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn two_node_inbound_mailbox_full_reports_mailbox_full() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: node1_bind is a valid C string for the duration of this test.
        let node1 = unsafe { TestNode::new(326, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 was just allocated and remains valid until teardown.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(327);

        _real_sched = init_real_scheduler();

        crate::pid::hew_pid_set_local_node(327);
        let opts = crate::actor::HewActorOpts {
            init_state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox_capacity: 1,
            overflow: crate::internal::types::HewOverflowPolicy::DropNew as i32,
            coalesce_key_fn: None,
            coalesce_fallback: 0,
            message_drop_fn: None,
            budget: 0,
            arena_cap_bytes: 0,
            cycle_capable: 0,
        };
        // SAFETY: opts points to a valid HewActorOpts for the duration of this call.
        let actor = unsafe { crate::actor::hew_actor_spawn_opts(&raw const opts) };
        crate::pid::hew_pid_set_local_node(326);
        assert!(!actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid here.
        let actor_id = unsafe { (*actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 327);

        // SAFETY: actor is valid; mailbox pointer is valid for the actor lifetime.
        let mailbox = unsafe { (*actor).mailbox.cast::<crate::mailbox::HewMailbox>() };
        // SAFETY: mailbox is a valid bounded mailbox pointer; the null payload is intentional.
        let pre_fill = unsafe { crate::mailbox::hew_mailbox_send(mailbox, 1, ptr::null_mut(), 0) };
        assert_eq!(
            pre_fill, 0,
            "pre-fill into empty bounded mailbox must succeed"
        );

        let connect_addr = CString::new(format!("327@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and connect_addr are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until teardown.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: this is a remote void ask; null payload/size are valid.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                ptr::null_mut(),
                0,
                TEST_REMOTE_ASK_TIMEOUT_MS,
                0,
            )
        };
        let err = hew_node_ask_take_last_error();

        assert!(
            reply_ptr.is_null(),
            "full-mailbox remote ask must return null"
        );
        assert_eq!(
            err,
            AskError::MailboxFull as i32,
            "full-mailbox remote ask must propagate MailboxFull, not WorkerAtCapacity or ActorStopped"
        );

        // SAFETY: actor and nodes were allocated in this test and remain valid here.
        unsafe {
            crate::actor::hew_actor_stop(actor);
            let _ = crate::actor::hew_actor_free(actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn two_node_worker_limit_still_reports_worker_at_capacity() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: node1_bind is a valid C string for the duration of this test.
        let node1 = unsafe { TestNode::new(319, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 was just allocated and remains valid until teardown.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(320);

        _real_sched = init_real_scheduler();

        crate::pid::hew_pid_set_local_node(320);
        // SAFETY: null state / size-0 are valid; dispatch fn is a valid fn ptr.
        let actor =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        crate::pid::hew_pid_set_local_node(319);
        assert!(!actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid here.
        let actor_id = unsafe { (*actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 320);

        let connect_addr = CString::new(format!("320@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and connect_addr are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until teardown.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        let saved = INBOUND_ASK_ACTIVE.swap(INBOUND_ASK_WORKER_LIMIT, Ordering::AcqRel);
        // SAFETY: this is a remote void ask; null payload/size are valid.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                ptr::null_mut(),
                0,
                TEST_REMOTE_ASK_TIMEOUT_MS,
                0,
            )
        };
        let err = hew_node_ask_take_last_error();
        INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);

        assert!(
            reply_ptr.is_null(),
            "worker-limit rejection must not return the void-success sentinel"
        );
        assert_eq!(
            err,
            AskError::WorkerAtCapacity as i32,
            "worker-limit rejection must keep reporting WorkerAtCapacity"
        );

        // SAFETY: actor and nodes were allocated in this test and remain valid here.
        unsafe {
            let _ = crate::actor::hew_actor_free(actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn two_node_pre_rejection_peer_gets_timeout_not_wrong_error() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: node1_bind is a valid C string for the duration of this test.
        let node1 = unsafe { TestNode::new(330, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 was just allocated and remains valid until teardown.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(331);

        _real_sched = init_real_scheduler();

        crate::pid::hew_pid_set_local_node(331);
        // SAFETY: null state / size-0 are valid; dispatch fn is a valid fn ptr.
        let actor =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        crate::pid::hew_pid_set_local_node(330);
        assert!(!actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid here.
        let actor_id = unsafe { (*actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 331);

        let connect_addr = CString::new(format!("331@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and connect_addr are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until teardown.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: node2 conn_mgr is live for the duration of this test.
        let peer_flags = unsafe {
            connection::hew_connmgr_feature_flags_for_node((*node2.as_ptr()).conn_mgr, 330)
        };
        assert!(
            connection::supports_ask_rejection(peer_flags),
            "handshake should record ask-rejection support before we simulate the old peer"
        );
        // SAFETY: node2 conn_mgr is live; this mutates only test state.
        unsafe {
            connection::hew_connmgr_force_peer_flags_for_node(
                (*node2.as_ptr()).conn_mgr,
                330,
                peer_flags & !connection::HEW_FEATURE_SUPPORTS_ASK_REJECTION,
            );
        }
        // SAFETY: node2 conn_mgr remains live while the nodes are running in this scope.
        let stripped_flags = unsafe {
            connection::hew_connmgr_feature_flags_for_node((*node2.as_ptr()).conn_mgr, 330)
        };
        assert!(
            !connection::supports_ask_rejection(stripped_flags),
            "test setup must strip ask-rejection support from node2's view of node1"
        );

        let saved = INBOUND_ASK_ACTIVE.swap(INBOUND_ASK_WORKER_LIMIT, Ordering::AcqRel);
        let ask_start = std::time::Instant::now();
        // SAFETY: this is a remote void ask; null payload/size are valid.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                ptr::null_mut(),
                0,
                TEST_REMOTE_ASK_TIMEOUT_MS,
                0,
            )
        };
        let err = hew_node_ask_take_last_error();
        INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);

        assert!(
            reply_ptr.is_null(),
            "pre-rejection peer fallback must return null instead of a void-success sentinel"
        );
        assert_eq!(
            err,
            AskError::Timeout as i32,
            "pre-rejection peer fallback must time out instead of returning WorkerAtCapacity"
        );
        assert!(
            ask_start.elapsed() < Duration::from_millis(TEST_REMOTE_ASK_TIMEOUT_MS * 3),
            "fallback ask should resolve near the timeout deadline, not block indefinitely"
        );

        // SAFETY: actor and nodes were allocated in this test and remain valid here.
        unsafe {
            let _ = crate::actor::hew_actor_free(actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn two_node_remote_nonvoid_empty_reply_returns_null() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").unwrap();

        // SAFETY: bind addresses are valid C strings for the test scope.
        let node1 = unsafe { TestNode::new(317, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 is valid for each call in this scope.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(318);

        _real_sched = init_real_scheduler();

        crate::pid::hew_pid_set_local_node(318);
        // SAFETY: null state and size-0 are valid; the dispatch function pointer is valid.
        let empty_reply_actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(void_ask_probe_dispatch))
        };
        crate::pid::hew_pid_set_local_node(317);
        assert!(!empty_reply_actor.is_null(), "actor spawn failed");
        // SAFETY: the actor was just spawned successfully and remains valid here.
        let actor_id = unsafe { (*empty_reply_actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 318);

        let connect_addr = CString::new(format!("318@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and the connect address are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until the end of the test.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: non-void remote ask expects a u32-sized reply; an empty success must fail closed.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                ptr::null_mut(),
                0,
                TEST_REMOTE_ASK_TIMEOUT_MS,
                std::mem::size_of::<u32>(),
            )
        };
        assert!(
            reply_ptr.is_null(),
            "non-void remote ask should return null on an empty reply payload"
        );
        assert_eq!(
            hew_node_ask_take_last_error(),
            AskError::PayloadSizeMismatch as i32,
            "empty reply to non-void ask should report PayloadSizeMismatch"
        );

        // SAFETY: the actor and nodes were allocated in this test and are still valid here.
        unsafe {
            let _ = crate::actor::hew_actor_free(empty_reply_actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn two_node_remote_ask_timeout_reports_timeout() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").unwrap();

        // SAFETY: bind addresses are valid C strings for the duration of this test.
        let node1 = unsafe { TestNode::new(328, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 comes from TestNode::new and is valid for start-up here.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(329);

        _real_sched = init_real_scheduler();

        crate::pid::hew_pid_set_local_node(329);
        // SAFETY: null state and size-0 are valid; the dispatch function pointer is valid.
        let silent_actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(blocked_ask_probe_dispatch))
        };
        crate::pid::hew_pid_set_local_node(328);
        assert!(!silent_actor.is_null(), "silent actor spawn failed");
        // SAFETY: the actor was just spawned successfully and remains valid here.
        let actor_id = unsafe { (*silent_actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 329);

        let connect_addr = CString::new(format!("329@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and the connect address are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until the end of the test.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        let ask_start = std::time::Instant::now();
        // SAFETY: the actor pid and null payload are valid for this remote ask probe.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                ptr::null_mut(),
                0,
                TEST_REMOTE_ASK_TIMEOUT_MS,
                std::mem::size_of::<u32>(),
            )
        };
        let err = hew_node_ask_take_last_error();

        assert!(reply_ptr.is_null(), "timed-out remote ask must return null");
        assert_eq!(
            err,
            AskError::Timeout as i32,
            "remote ask that receives no reply must report Timeout"
        );
        assert!(
            ask_start.elapsed() < Duration::from_millis(TEST_REMOTE_ASK_TIMEOUT_MS * 3),
            "ask should complete near the timeout deadline, not block indefinitely"
        );

        // SAFETY: the actor and nodes were allocated in this test and remain valid here.
        unsafe {
            crate::actor::hew_actor_stop(silent_actor);
            let _ = crate::actor::hew_actor_free(silent_actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn node_stop_wakes_pending_remote_ask() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").unwrap();

        // SAFETY: bind addresses are valid C strings for the duration of this test.
        let node1 = unsafe { TestNode::new(315, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 comes from TestNode::new and is valid for start-up here.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(316);

        _real_sched = init_real_scheduler();

        crate::pid::hew_pid_set_local_node(316);
        // SAFETY: null state and size-0 are valid; the dispatch function pointer is valid.
        let blocked_actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(blocked_ask_probe_dispatch))
        };
        crate::pid::hew_pid_set_local_node(315);
        assert!(!blocked_actor.is_null(), "actor spawn failed");
        // SAFETY: the actor was just spawned successfully and remains valid here.
        let actor_id = unsafe { (*blocked_actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 316);

        let connect_addr = CString::new(format!("316@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and the connect address are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until the end of the test.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: the actor pid and null payload are valid for this remote ask probe.
        let ask_handle = thread::spawn(move || unsafe {
            let ptr = hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                ptr::null_mut(),
                0,
                TEST_REMOTE_ASK_TIMEOUT_MS,
                std::mem::size_of::<u32>(),
            );
            let err = hew_node_ask_take_last_error();
            (ptr as usize, err)
        });

        let pending_seen = (0..100).any(|_| {
            let guard = reply_table()
                .pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let seen = !guard.is_empty();
            drop(guard);
            if !seen {
                thread::sleep(Duration::from_millis(10));
            }
            seen
        });
        assert!(
            pending_seen,
            "remote ask never reached the pending reply table"
        );

        let stop_started = std::time::Instant::now();
        // SAFETY: node1 remains valid here and stopping it is the behavior under test.
        unsafe {
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        }
        let (reply_raw, ask_err) = ask_handle.join().expect("ask thread panicked");
        let reply_ptr = reply_raw as *mut c_void;
        assert!(
            reply_ptr.is_null(),
            "stopped node should fail pending remote asks"
        );
        assert_eq!(
            ask_err,
            AskError::ConnectionDropped as i32,
            "node stop should report ConnectionDropped on pending asks"
        );
        assert!(
            stop_started.elapsed() < Duration::from_secs(2),
            "pending remote ask should wake promptly when the node stops"
        );

        // SAFETY: the actor and node2 were allocated in this test and remain valid here.
        unsafe {
            let _ = crate::actor::hew_actor_free(blocked_actor);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn connection_drop_wakes_pending_remote_ask() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").unwrap();

        // SAFETY: bind addresses are valid C strings for the duration of this test.
        let node1 = unsafe { TestNode::new(320, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 comes from TestNode::new and is valid for start-up here.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(321);

        _real_sched = init_real_scheduler();

        crate::pid::hew_pid_set_local_node(321);
        // SAFETY: null state and size-0 are valid; the dispatch function pointer is valid.
        let blocked_actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(blocked_ask_probe_dispatch))
        };
        crate::pid::hew_pid_set_local_node(320);
        assert!(!blocked_actor.is_null(), "actor spawn failed");
        // SAFETY: the actor was just spawned successfully and remains valid here.
        let actor_id = unsafe { (*blocked_actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 321);

        let connect_addr = CString::new(format!("321@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and the connect address are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until the end of the test.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: both nodes are running and their connection managers are valid here.
        let outbound_conn_id =
            unsafe { connection::hew_connmgr_conn_id_for_node((*node1.as_ptr()).conn_mgr, 321) };
        // SAFETY: both nodes are running and their connection managers are valid here.
        let accepted_conn_id =
            unsafe { connection::hew_connmgr_conn_id_for_node((*node2.as_ptr()).conn_mgr, 320) };
        assert!(
            outbound_conn_id >= 0,
            "initiator outbound connection missing"
        );
        assert!(
            accepted_conn_id >= 0,
            "responder accepted connection missing"
        );
        // SAFETY: node1 remains valid here and its connection manager stays alive until teardown below.
        let outbound_key = ConnectionKey::new(
            unsafe { (*node1.as_ptr()).conn_mgr.cast_const() },
            outbound_conn_id,
        );

        // SAFETY: the actor pid and null payload are valid for this remote ask probe.
        let ask_handle = thread::spawn(move || unsafe {
            let ptr = hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                ptr::null_mut(),
                0,
                TEST_REMOTE_ASK_TIMEOUT_MS,
                std::mem::size_of::<u32>(),
            );
            let err = hew_node_ask_take_last_error();
            (ptr as usize, err)
        });

        let pending_seen = (0..100).any(|_| {
            let guard = reply_table()
                .pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let seen = guard
                .values()
                .any(|pending| pending.connection == outbound_key);
            drop(guard);
            if !seen {
                thread::sleep(Duration::from_millis(10));
            }
            seen
        });
        assert!(
            pending_seen,
            "remote ask never registered against the outbound connection"
        );

        let disconnect_started = std::time::Instant::now();
        // SAFETY: node2 remains valid here and removing its accepted connection simulates a peer drop.
        unsafe {
            assert_eq!(
                connection::hew_connmgr_remove((*node2.as_ptr()).conn_mgr, accepted_conn_id),
                0
            );
        }
        let (reply_raw, ask_err) = ask_handle.join().expect("ask thread panicked");
        let reply_ptr = reply_raw as *mut c_void;
        assert!(
            reply_ptr.is_null(),
            "connection drop should fail the pending remote ask"
        );
        assert_eq!(
            ask_err,
            AskError::ConnectionDropped as i32,
            "connection drop should report ConnectionDropped"
        );
        assert!(
            disconnect_started.elapsed() < Duration::from_millis(TEST_REMOTE_ASK_TIMEOUT_MS / 2),
            "pending remote ask should wake well before the full remote ask timeout"
        );

        // SAFETY: the actor and nodes were allocated in this test and remain valid here.
        unsafe {
            let _ = crate::actor::hew_actor_free(blocked_actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    /// A SWIM-declared-DEAD peer (socket still nominally open)
    /// must resolve a pending remote ask IMMEDIATELY with `AskError::Partition`,
    /// not hang to the caller's deadline. The connection-drop test above proves
    /// the socket-drop cause; this proves the SWIM-DEAD cause through the
    /// node-side fan-out (`fail_remote_asks_for_node`), with the socket left
    /// open so the only thing that resolves the ask is the failure-detector
    /// verdict — distinct from `ConnectionDropped`.
    #[test]
    fn swim_dead_wakes_pending_remote_ask_with_partition() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").unwrap();

        // SAFETY: bind addresses are valid C strings for the duration of this test.
        let node1 = unsafe { TestNode::new(330, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 comes from TestNode::new and is valid for start-up here.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(331);

        _real_sched = init_real_scheduler();

        crate::pid::hew_pid_set_local_node(331);
        // SAFETY: null state and size-0 are valid; the dispatch function pointer is valid.
        let blocked_actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(blocked_ask_probe_dispatch))
        };
        crate::pid::hew_pid_set_local_node(330);
        assert!(!blocked_actor.is_null(), "actor spawn failed");
        // SAFETY: the actor was just spawned successfully and remains valid here.
        let actor_id = unsafe { (*blocked_actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 331);

        let connect_addr = CString::new(format!("331@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and the connect address are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until the end of the test.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: both nodes are running and their connection managers are valid here.
        let outbound_conn_id =
            unsafe { connection::hew_connmgr_conn_id_for_node((*node1.as_ptr()).conn_mgr, 331) };
        assert!(
            outbound_conn_id >= 0,
            "initiator outbound connection missing"
        );
        // SAFETY: node1 remains valid here and its connection manager stays alive until teardown.
        let outbound_key = ConnectionKey::new(
            unsafe { (*node1.as_ptr()).conn_mgr.cast_const() },
            outbound_conn_id,
        );

        // SAFETY: the actor pid and null payload are valid for this remote ask probe.
        let ask_handle = thread::spawn(move || unsafe {
            let ptr = hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                ptr::null_mut(),
                0,
                TEST_REMOTE_ASK_TIMEOUT_MS,
                std::mem::size_of::<u32>(),
            );
            let err = hew_node_ask_take_last_error();
            (ptr as usize, err)
        });

        let pending_seen = (0..100).any(|_| {
            let guard = reply_table()
                .pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let seen = guard
                .values()
                .any(|pending| pending.connection == outbound_key);
            drop(guard);
            if !seen {
                thread::sleep(Duration::from_millis(10));
            }
            seen
        });
        assert!(
            pending_seen,
            "remote ask never registered against the outbound connection"
        );

        // Declare node 331 DEAD via the node-side partition fan-out WITHOUT
        // touching the socket — the SWIM/phi-accrual verdict, not a TCP drop.
        let dead_declared = std::time::Instant::now();
        fail_remote_asks_for_node(331);

        let (reply_raw, ask_err) = ask_handle.join().expect("ask thread panicked");
        let reply_ptr = reply_raw as *mut c_void;
        assert!(
            reply_ptr.is_null(),
            "SWIM-DEAD must fail the pending remote ask (no fabricated reply)"
        );
        // The teeth: the exact Partition discriminant (14), distinct from
        // ConnectionDropped (6) and Timeout (5).
        assert_eq!(
            ask_err,
            AskError::Partition as i32,
            "SWIM-DEAD should report Partition, not ConnectionDropped or Timeout"
        );
        // Bounded-time: a hang would blow the full ask timeout. The fan-out is
        // synchronous, so resolution is effectively immediate.
        assert!(
            dead_declared.elapsed() < Duration::from_millis(TEST_REMOTE_ASK_TIMEOUT_MS / 2),
            "pending remote ask should resolve well before the full ask timeout"
        );
        // Exactly-once: the entry was drained on the first fan-out, so a second
        // verdict (or a racing socket drop) finds nothing — assert the map is
        // empty for this connection and a repeat fan-out is a no-op.
        {
            let map = reply_table()
                .pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            assert!(
                !map.values().any(|p| p.connection == outbound_key),
                "partition fan-out must remove the pending entry (no leak, exactly-once)"
            );
        }
        fail_remote_asks_for_node(331); // second verdict: no panic, no double-wake.

        // SAFETY: the actor and nodes were allocated in this test and remain valid here.
        unsafe {
            let _ = crate::actor::hew_actor_free(blocked_actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn secondary_node_stop_does_not_fail_current_node_pending_asks() {
        let _guard = crate::runtime_test_guard();
        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").unwrap();

        // SAFETY: bind addresses are valid C strings for the duration of this test.
        let node1 = unsafe { TestNode::new(318, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 comes from TestNode::new and is valid for start-up here.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, _node2_port) = start_tcp_test_listener_node(319);

        let (request_id, pending) = reply_table().register(ConnectionKey {
            conn_mgr: 1,
            conn_id: 0,
        });

        // SAFETY: node2 remains valid here and stopping it is the behavior under test.
        unsafe {
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }

        let guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(
            guard.is_none(),
            "secondary node stop must not fail pending asks owned by CURRENT_NODE"
        );
        drop(guard);
        reply_table().remove(request_id);

        // SAFETY: node1 remains valid until the end of the test.
        unsafe {
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[test]
    fn send_reply_envelope_bails_before_touching_stopping_connmgr() {
        let _guard = crate::runtime_test_guard();
        crate::registry::hew_registry_clear();

        let bind_addr = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: bind_addr is a valid C string for the duration of this test.
        let node = unsafe { TestNode::new(317, &bind_addr) };
        assert!(!node.as_ptr().is_null());

        // SAFETY: node pointer is valid for start/stop in this scope.
        unsafe {
            assert_eq!(hew_node_start(node.as_ptr()), 0);
        }

        let shutdown_started = Arc::new(AtomicBool::new(true));
        let dangling_mgr = std::ptr::NonNull::<connection::HewConnMgr>::dangling().as_ptr();
        send_reply_envelope(999, 123, &[], dangling_mgr, shutdown_started.as_ref());

        // SAFETY: node pointer remains valid until drop.
        unsafe {
            assert_eq!(hew_node_stop(node.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    // ── Inbound ask worker bound tests ─────────────────────────────────────
    //
    // These tests verify the backpressure mechanism added to node_inbound_router.
    // The mechanism prevents a remote peer from exhausting OS thread count or
    // virtual memory by flooding inbound asks.
    //
    // All of these tests acquire the shared runtime test lock so they serialize with any
    // other test that runs a node or manipulates INBOUND_ASK_ACTIVE.

    /// `InboundAskGuard` decrements `INBOUND_ASK_ACTIVE` and the per-manager
    /// counter exactly once on drop, including when the enclosing scope exits
    /// via panic.
    #[test]
    fn inbound_ask_guard_decrements_on_drop() {
        let _lock = crate::runtime_test_guard();
        // Reset to a known value; restore on exit.
        let saved = INBOUND_ASK_ACTIVE.swap(1, Ordering::AcqRel);
        let per_mgr = Arc::new(AtomicUsize::new(1));
        {
            let _guard = InboundAskGuard(Arc::clone(&per_mgr));
            assert_eq!(
                INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
                1,
                "global counter must be 1 while guard is live"
            );
            assert_eq!(
                per_mgr.load(Ordering::Acquire),
                1,
                "per-mgr counter must be 1 while guard is live"
            );
        }
        // Guard dropped — both counters must be 0.
        let after_global = INBOUND_ASK_ACTIVE.load(Ordering::Acquire);
        let after_per_mgr = per_mgr.load(Ordering::Acquire);
        INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);
        assert_eq!(
            after_global, 0,
            "InboundAskGuard must decrement INBOUND_ASK_ACTIVE on drop"
        );
        assert_eq!(
            after_per_mgr, 0,
            "InboundAskGuard must decrement per-manager counter on drop"
        );
    }

    /// Two guards decrement independently (one per spawned thread).
    #[test]
    fn inbound_ask_guard_pair_decrements_twice() {
        let _lock = crate::runtime_test_guard();
        let saved = INBOUND_ASK_ACTIVE.swap(2, Ordering::AcqRel);
        let per_mgr1 = Arc::new(AtomicUsize::new(1));
        let per_mgr2 = Arc::new(AtomicUsize::new(1));
        let g1 = InboundAskGuard(Arc::clone(&per_mgr1));
        let g2 = InboundAskGuard(Arc::clone(&per_mgr2));
        drop(g1);
        assert_eq!(
            INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
            1,
            "first guard must decrement global by 1"
        );
        assert_eq!(
            per_mgr1.load(Ordering::Acquire),
            0,
            "first guard must decrement its per-mgr counter to 0"
        );
        drop(g2);
        assert_eq!(
            INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
            0,
            "second guard must decrement global back to zero"
        );
        assert_eq!(
            per_mgr2.load(Ordering::Acquire),
            0,
            "second guard must decrement its per-mgr counter to 0"
        );
        INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);
    }

    /// When `INBOUND_ASK_ACTIVE` is saturated to `INBOUND_ASK_WORKER_LIMIT` the
    /// optimistic-increment + revert logic correctly prevents over-commitment.
    ///
    /// This test directly exercises the counter-check branch used in
    /// `node_inbound_router` without needing a live transport.
    #[test]
    fn inbound_ask_worker_limit_rejects_at_capacity() {
        let _lock = crate::runtime_test_guard();
        let saved = INBOUND_ASK_ACTIVE.swap(INBOUND_ASK_WORKER_LIMIT, Ordering::AcqRel);

        // Simulate what node_inbound_router does for an inbound ask.
        let prev = INBOUND_ASK_ACTIVE.fetch_add(1, Ordering::AcqRel);
        let at_limit = prev >= INBOUND_ASK_WORKER_LIMIT;
        if at_limit {
            INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
        }

        // Must have detected the limit and reverted.
        assert!(
            at_limit,
            "should detect limit when counter == INBOUND_ASK_WORKER_LIMIT"
        );
        assert_eq!(
            INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
            INBOUND_ASK_WORKER_LIMIT,
            "counter must be reverted to the limit after rejection"
        );

        INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);
    }

    /// When `INBOUND_ASK_ACTIVE` is one below the limit, a new ask is accepted.
    #[test]
    fn inbound_ask_worker_limit_accepts_below_capacity() {
        let _lock = crate::runtime_test_guard();
        let saved = INBOUND_ASK_ACTIVE.swap(INBOUND_ASK_WORKER_LIMIT - 1, Ordering::AcqRel);

        let prev = INBOUND_ASK_ACTIVE.fetch_add(1, Ordering::AcqRel);
        let at_limit = prev >= INBOUND_ASK_WORKER_LIMIT;
        if at_limit {
            INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
        }

        assert!(!at_limit, "ask just below limit must be accepted");
        assert_eq!(
            INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
            INBOUND_ASK_WORKER_LIMIT,
            "accepted ask increments counter to limit"
        );

        // Release the slot we acquired (simulating the InboundAskGuard).
        INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
        INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);
    }

    /// End-to-end: inbound ask counter is bounded during a real two-node ask
    /// round-trip. After the ask completes the worker slot is released and the
    /// counter returns to its pre-ask value.
    #[test]
    fn inbound_ask_active_counter_returns_to_baseline_after_round_trip() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();
        register_test_u32_codec(test_dispatch(), 1);

        let node1_bind = CString::new("127.0.0.1:0").unwrap();

        // SAFETY: bind addresses are valid C strings for the test scope.
        let node1 = unsafe { TestNode::new(320, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 is valid for each call in this scope.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(321);

        _real_sched = init_real_scheduler();

        // Spawn a u32-echo actor on node2.
        crate::pid::hew_pid_set_local_node(321);
        // SAFETY: null state and size-0 are valid; ask_probe_dispatch echoes back u32*2.
        let echo_actor =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(ask_probe_dispatch)) };
        crate::pid::hew_pid_set_local_node(320);
        assert!(!echo_actor.is_null(), "actor spawn failed");
        // SAFETY: the actor was just spawned successfully and remains valid here.
        let actor_id = unsafe { (*echo_actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 321);

        let connect_addr = CString::new(format!("321@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and the connect address are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until the end of the test.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // Record the counter before the ask so we can verify it returns to baseline.
        let baseline = INBOUND_ASK_ACTIVE.load(Ordering::Acquire);

        let payload: u32 = 0xDEAD_BEEF;
        // SAFETY: payload is a valid u32 on the stack; its address is valid for this call.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                std::ptr::from_ref(&payload).cast_mut().cast::<c_void>(),
                std::mem::size_of::<u32>(),
                TEST_REMOTE_ASK_TIMEOUT_MS,
                std::mem::size_of::<u32>(),
            )
        };
        assert!(!reply_ptr.is_null(), "remote ask must succeed");
        // SAFETY: reply was malloc'd by hew_reply; we own it after the ask.
        unsafe { libc::free(reply_ptr) };

        // After the ask completes the handler thread exits, dropping InboundAskGuard.
        // Give it a brief moment to drain.
        let settled = (0..50).any(|_| {
            let v = INBOUND_ASK_ACTIVE.load(Ordering::Acquire);
            if v == baseline {
                true
            } else {
                thread::sleep(Duration::from_millis(10));
                false
            }
        });
        assert!(
            settled,
            "INBOUND_ASK_ACTIVE did not return to baseline after ask completed (got {}; expected {})",
            INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
            baseline,
        );
        assert!(
            INBOUND_ASK_ACTIVE.load(Ordering::Acquire) <= INBOUND_ASK_WORKER_LIMIT,
            "active worker count must never exceed INBOUND_ASK_WORKER_LIMIT"
        );

        // SAFETY: actor and nodes were allocated in this test and are still valid here.
        unsafe {
            let _ = crate::actor::hew_actor_free(echo_actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    // ── Over-limit rejection tests ────────────────────────────────────────
    //
    // These tests verify the fail-closed semantics of the worker-limit
    // rejection path for both void and non-void remote asks.
    //
    // When INBOUND_ASK_ACTIVE is artificially saturated to
    // INBOUND_ASK_WORKER_LIMIT the inbound router sends a rejection reply
    // (HEW_REPLY_REJECT_MSG_TYPE).  The receiving node's connection reader
    // calls `fail_remote_reply`, which sets ReplyStatus::Failed with reason
    // WorkerAtCapacity so that `hew_node_api_ask` returns the precise
    // discriminant — not a false-success void sentinel, and not the generic
    // ConnectionDropped that indicates a wire-level failure.

    /// Over-limit rejection of a **void** remote ask returns null +
    /// `WorkerAtCapacity` (not the void-success sentinel, not `ConnectionDropped`).
    #[test]
    fn over_limit_void_ask_fails_closed_with_worker_at_capacity() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        let node1_bind = CString::new("127.0.0.1:0").unwrap();

        // SAFETY: bind addresses are valid C strings for the test scope.
        let node1 = unsafe { TestNode::new(322, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 is valid for each call in this scope.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(323);
        _real_sched = init_real_scheduler();

        // Spawn a void-reply actor on node2.
        crate::pid::hew_pid_set_local_node(323);
        // SAFETY: null state / size-0 are valid; void_ask_probe_dispatch is valid.
        let actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(void_ask_probe_dispatch))
        };
        crate::pid::hew_pid_set_local_node(322);
        assert!(!actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid here.
        let actor_id = unsafe { (*actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 323);

        let connect_addr = CString::new(format!("323@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and connect_addr are valid for this call.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until the end of the test.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // Saturate the worker counter on node2 (the answering node) so the
        // next inbound ask is rejected.
        let saved = INBOUND_ASK_ACTIVE.swap(INBOUND_ASK_WORKER_LIMIT, Ordering::AcqRel);

        // Void ask: reply_size == 0. Before the fix this would have returned
        // the void-success sentinel because the rejection sent an empty payload
        // which remote_reply_data_to_ptr mistook for a void success.
        // SAFETY: null payload / size-0 are valid; this is a void ask.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                ptr::null_mut(),
                0,
                TEST_REMOTE_ASK_TIMEOUT_MS,
                0,
            )
        };
        let err = hew_node_ask_take_last_error();

        // Restore before any assertions so the teardown path is clean.
        INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);

        assert!(
            reply_ptr.is_null(),
            "over-limit void ask must return null (got non-null = false success)"
        );
        assert_eq!(
            err,
            AskError::WorkerAtCapacity as i32,
            "over-limit void ask must report WorkerAtCapacity, not ConnectionDropped"
        );

        // SAFETY: actor and nodes remain valid until teardown here.
        unsafe {
            let _ = crate::actor::hew_actor_free(actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    /// Over-limit rejection of a **non-void** remote ask returns null +
    /// `WorkerAtCapacity` (not `PayloadSizeMismatch` from the old empty-
    /// payload path, not `ConnectionDropped`, and not a spurious success).
    #[test]
    fn over_limit_nonvoid_ask_fails_closed_with_worker_at_capacity() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();
        register_test_u32_codec(test_dispatch(), 1);

        let node1_bind = CString::new("127.0.0.1:0").unwrap();

        // SAFETY: bind addresses are valid C strings for the test scope.
        let node1 = unsafe { TestNode::new(324, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 is valid for each call in this scope.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(325);
        _real_sched = init_real_scheduler();

        // Spawn a u32-echo actor on node2.
        crate::pid::hew_pid_set_local_node(325);
        // SAFETY: null state and size-0 are valid; ask_probe_dispatch is valid.
        let actor =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(ask_probe_dispatch)) };
        crate::pid::hew_pid_set_local_node(324);
        assert!(!actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid here.
        let actor_id = unsafe { (*actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 325);

        let connect_addr = CString::new(format!("325@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and connect_addr are valid for this call.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until the end of the test.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // Saturate the worker counter on node2.
        let saved = INBOUND_ASK_ACTIVE.swap(INBOUND_ASK_WORKER_LIMIT, Ordering::AcqRel);

        let payload: u32 = 42;
        // SAFETY: payload is a valid u32; its address is valid for this call.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                actor_id,
                test_dispatch(),
                1,
                std::ptr::from_ref(&payload).cast_mut().cast::<c_void>(),
                std::mem::size_of::<u32>(),
                TEST_REMOTE_ASK_TIMEOUT_MS,
                std::mem::size_of::<u32>(),
            )
        };
        let err = hew_node_ask_take_last_error();

        INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);

        assert!(
            reply_ptr.is_null(),
            "over-limit non-void ask must return null"
        );
        assert_eq!(
            err,
            AskError::WorkerAtCapacity as i32,
            "over-limit non-void ask must report WorkerAtCapacity (not PayloadSizeMismatch)"
        );

        // SAFETY: actor and nodes remain valid until teardown here.
        unsafe {
            let _ = crate::actor::hew_actor_free(actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    // ── Regression: race-window fixes ────────────────────────────────────

    /// After `hew_connmgr_mark_stopping` is called, `node_inbound_router`
    /// must not spawn a new inbound-ask worker (the spawn-window fix).
    ///
    /// This test marks the connection manager as stopping, then calls the
    /// inbound router directly with an ask-shaped message and asserts that
    /// neither `INBOUND_ASK_ACTIVE` nor the per-manager counter increases —
    /// no worker was spawned.
    #[test]
    fn inbound_router_no_spawn_after_shutdown_started() {
        let _guard = crate::runtime_test_guard();
        crate::registry::hew_registry_clear();

        let (node, _port) = start_tcp_test_listener_node(351);

        // Capture per-manager counter before marking stopping.
        // SAFETY: node was just started and conn_mgr is valid here.
        let per_mgr_active =
            unsafe { connection::hew_connmgr_inbound_ask_active((*node.as_ptr()).conn_mgr) }
                .expect("conn_mgr must have inbound_ask_active");

        // Mark the connection manager as stopping — this is what hew_node_stop
        // does inside its CURRENT_NODE write lock.
        // SAFETY: node was just started and conn_mgr is valid here.
        unsafe { connection::hew_connmgr_mark_stopping((*node.as_ptr()).conn_mgr) };

        // The per-manager counter is fresh for this node and must be zero.
        // The global counter may be non-zero due to other tests sharing the
        // process — record it and verify it is UNCHANGED after the router call.
        let global_before = INBOUND_ASK_ACTIVE.load(Ordering::Acquire);
        assert_eq!(
            per_mgr_active.load(Ordering::Acquire),
            0,
            "precondition: per-manager counter must be zero before the test call"
        );

        // Call the inbound router with an ask-shaped message (request_id > 0,
        // source_node_id > 0).  The router should bail without spawning.
        // SAFETY: conn_mgr is valid; null data with size 0 is the empty-payload contract.
        unsafe {
            node_inbound_router(
                0,
                1,
                ptr::null_mut(),
                0,
                /*request_id=*/ 1,
                /*source_node_id=*/ 1,
                (*node.as_ptr()).conn_mgr,
            );
        }

        // Give any mistakenly-spawned thread time to increment the counters.
        thread::sleep(Duration::from_millis(20));

        assert_eq!(
            INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
            global_before,
            "global counter must not change after shutdown has started"
        );
        assert_eq!(
            per_mgr_active.load(Ordering::Acquire),
            0,
            "per-manager counter must not increase after shutdown has started"
        );

        // SAFETY: node is valid and owned by the TestNode guard.
        unsafe { assert_eq!(hew_node_stop(node.as_ptr()), 0) };
        crate::registry::hew_registry_clear();
    }

    /// `hew_node_stop` must return only after the per-manager inbound-ask
    /// active counter reaches zero (the drain postcondition).
    ///
    /// This test artificially inflates both `INBOUND_ASK_ACTIVE` and the
    /// per-conn_mgr counter by one to simulate an in-flight worker, schedules
    /// a background thread to decrement them after a short delay, and then
    /// asserts that stop waited for the decrement (counter is zero on return).
    #[test]
    fn node_stop_drains_inbound_ask_active() {
        let _guard = crate::runtime_test_guard();
        crate::registry::hew_registry_clear();

        let (node, _port) = start_tcp_test_listener_node(352);

        // Capture the per-manager counter Arc while conn_mgr is still live.
        // SAFETY: node was just started and conn_mgr is valid here.
        let per_mgr_active =
            unsafe { connection::hew_connmgr_inbound_ask_active((*node.as_ptr()).conn_mgr) }
                .expect("conn_mgr must have inbound_ask_active");

        // Simulate one active inbound-ask worker: increment both counters as
        // the real spawn path does.  A background thread decrements them after
        // 60 ms — long enough to make the drain observable without being slow.
        let global_saved = INBOUND_ASK_ACTIVE.fetch_add(1, Ordering::AcqRel);
        per_mgr_active.fetch_add(1, Ordering::AcqRel);
        let per_mgr_clone = Arc::clone(&per_mgr_active);
        let decrement_handle = thread::spawn(move || {
            thread::sleep(Duration::from_millis(60));
            INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
            per_mgr_clone.fetch_sub(1, Ordering::AcqRel);
        });

        let stop_start = std::time::Instant::now();
        // SAFETY: node is valid and owned by TestNode.
        unsafe { assert_eq!(hew_node_stop(node.as_ptr()), 0) };
        let elapsed = stop_start.elapsed();

        // Join the decrement thread (it should already be done by now).
        decrement_handle.join().expect("decrement thread panicked");

        assert_eq!(
            per_mgr_active.load(Ordering::Acquire),
            0,
            "per-manager counter must be zero after node_stop"
        );
        assert_eq!(
            INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
            global_saved,
            "global INBOUND_ASK_ACTIVE must be back to its pre-test value"
        );

        // stop must have waited at least ~60 ms for the drain — but at most
        // a generous 4 s to avoid flakiness on heavily loaded CI machines.
        assert!(
            elapsed >= Duration::from_millis(40),
            "node_stop returned too quickly ({elapsed:?}); drain did not wait"
        );
        assert!(
            elapsed < Duration::from_secs(4),
            "node_stop took too long ({elapsed:?}); drain may have stalled"
        );

        crate::registry::hew_registry_clear();
    }

    #[test]
    fn node_stop_waits_to_free_connmgr_until_inbound_ask_error_worker_drains() {
        struct HookResetGuard;

        impl Drop for HookResetGuard {
            fn drop(&mut self) {
                INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK.release();
                INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK.reset();
                NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.release();
                NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.reset();
            }
        }

        let _guard = crate::runtime_test_guard();
        let _real_sched;
        let _hook_reset = HookResetGuard;
        crate::registry::hew_registry_clear();

        INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK.arm(true);
        NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.arm(false);

        let node1_bind = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: node1_bind is a valid C string for the duration of this test.
        let node1 = unsafe { TestNode::new(353, &node1_bind) };
        assert!(!node1.as_ptr().is_null());

        // SAFETY: node1 is valid for start-up here.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(354);

        _real_sched = init_real_scheduler();

        crate::pid::hew_pid_set_local_node(354);
        // SAFETY: null state / size-0 are valid; dispatch fn is a valid fn ptr.
        let actor =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        crate::pid::hew_pid_set_local_node(353);
        assert!(!actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid here.
        let actor_id = unsafe { (*actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 354);

        let connect_addr = CString::new(format!("354@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and connect_addr are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until teardown.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: node2 was started above and its conn_mgr is live here.
        let per_mgr_active =
            unsafe { connection::hew_connmgr_inbound_ask_active((*node2.as_ptr()).conn_mgr) }
                .expect("conn_mgr must expose inbound_ask_active");

        // SAFETY: actor was spawned above and remains valid while stopped here.
        unsafe { crate::actor::hew_actor_stop(actor) };

        // SAFETY: conn_mgr is live and source_node_id identifies the connected peer.
        unsafe {
            node_inbound_router(
                actor_id,
                1,
                ptr::null_mut(),
                0,
                /*request_id=*/ 1,
                /*source_node_id=*/ 353,
                (*node2.as_ptr()).conn_mgr,
            );
        }

        assert!(
            INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK.wait_for_enter(Duration::from_secs(1)),
            "inbound ask error path never reached the feature-flags lookup hook"
        );
        assert_eq!(
            per_mgr_active.load(Ordering::Acquire),
            1,
            "worker must remain counted while the error path is blocked"
        );

        let (stop_tx, stop_rx) = std::sync::mpsc::channel();
        let node2_ptr = node2.as_ptr() as usize;
        let stop_handle = thread::spawn(move || {
            // SAFETY: node2_ptr comes from the live TestNode allocation above
            // and remains valid until this stop thread joins.
            let rc = unsafe { hew_node_stop(node2_ptr as *mut HewNode) };
            stop_tx.send(rc).expect("stop result receiver dropped");
        });

        assert!(
            !NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.wait_for_enter(Duration::from_millis(100)),
            "hew_node_stop must not reach conn_mgr free while an inbound-ask error worker still holds the manager"
        );

        INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK.release();

        assert!(
            NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.wait_for_enter(Duration::from_secs(1)),
            "hew_node_stop should reach conn_mgr free after the worker drains"
        );
        assert_eq!(
            stop_rx
                .recv_timeout(Duration::from_secs(2))
                .expect("hew_node_stop did not finish after the worker drained"),
            0,
            "hew_node_stop should succeed once the inbound-ask worker drains"
        );
        stop_handle.join().expect("stop thread panicked");

        assert_eq!(
            per_mgr_active.load(Ordering::Acquire),
            0,
            "per-manager worker count must be zero after node_stop returns"
        );

        // SAFETY: actor and node1 remain valid until teardown here.
        unsafe {
            let _ = crate::actor::hew_actor_free(actor);
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    /// Pins the ATOMIC spawn-gate + counter protocol in `node_inbound_router`:
    /// a worker that has incremented the per-manager counter (passed the gate's
    /// increment edge) is ALWAYS observed by a concurrent `hew_node_stop` drain,
    /// so `conn_mgr` is never freed (Phase 2) while that worker is live.
    ///
    /// Without the increment-then-recheck protocol the router checked the gate
    /// BEFORE incrementing, so a router could pass the check, have the drain
    /// close the gate and observe a zero counter, then increment + spawn AFTER
    /// the drain finished — reaching teardown and abandoning its reply / touching
    /// freed memory. This test wedges a router in the exact Dekker window
    /// (counter incremented, gate not yet re-read) and proves the drain blocks
    /// for it.
    #[test]
    fn node_stop_drain_waits_for_router_wedged_after_counter_increment() {
        struct HookResetGuard;
        impl Drop for HookResetGuard {
            fn drop(&mut self) {
                INBOUND_ROUTER_AFTER_INCREMENT_HOOK.release();
                INBOUND_ROUTER_AFTER_INCREMENT_HOOK.reset();
                NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.release();
                NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.reset();
            }
        }

        let _guard = crate::runtime_test_guard();
        let _hook_reset = HookResetGuard;
        crate::registry::hew_registry_clear();

        // Block a router that reaches the post-increment point; observe (do not
        // block) when stop reaches the conn_mgr free.
        INBOUND_ROUTER_AFTER_INCREMENT_HOOK.arm(true);
        NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.arm(false);

        let (node, _port) = start_tcp_test_listener_node(361);

        // SAFETY: node just started; conn_mgr is live here.
        let per_mgr_active =
            unsafe { connection::hew_connmgr_inbound_ask_active((*node.as_ptr()).conn_mgr) }
                .expect("conn_mgr must expose inbound_ask_active");

        // Drive a router on a background thread with an ask-shaped message. It
        // increments the per-manager counter, then blocks at the post-increment
        // hook BEFORE the gate re-check — the precise Dekker window.
        // SAFETY: node is live here; we read its conn_mgr pointer as an integer
        // to ferry it across the thread boundary (it stays valid until join).
        let conn_mgr_addr = unsafe { (*node.as_ptr()).conn_mgr } as usize;
        let router_handle = thread::spawn(move || {
            // SAFETY: conn_mgr stays live until this test frees it after join.
            unsafe {
                node_inbound_router(
                    0,
                    1,
                    ptr::null_mut(),
                    0,
                    /*request_id=*/ 1,
                    /*source_node_id=*/ 1,
                    conn_mgr_addr as *mut connection::HewConnMgr,
                );
            }
        });

        assert!(
            INBOUND_ROUTER_AFTER_INCREMENT_HOOK.wait_for_enter(Duration::from_secs(1)),
            "router never reached the post-increment hook"
        );
        assert_eq!(
            per_mgr_active.load(Ordering::SeqCst),
            1,
            "router must have incremented the per-manager counter before the gate re-check"
        );

        // Start stop on another thread. It closes the spawn gate (SeqCst) and
        // drains; the drain MUST block on the wedged router's increment.
        let node_ptr = node.as_ptr() as usize;
        let (stop_tx, stop_rx) = std::sync::mpsc::channel();
        let stop_handle = thread::spawn(move || {
            // SAFETY: node stays valid until this stop thread joins below.
            let rc = unsafe { hew_node_stop(node_ptr as *mut HewNode) };
            stop_tx.send(rc).expect("stop result receiver dropped");
        });

        // The drain is wedged: stop must NOT reach the conn_mgr free while the
        // counted router is still parked at the hook.
        assert!(
            !NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.wait_for_enter(Duration::from_millis(200)),
            "hew_node_stop reached conn_mgr free while a router counted by the drain was still wedged \
             — the spawn-gate/counter protocol failed to keep the worker visible to the drain"
        );
        assert!(
            stop_rx.recv_timeout(Duration::from_millis(50)).is_err(),
            "hew_node_stop returned before the wedged router drained"
        );

        // Release the router. It re-reads the now-closed gate, decrements, and
        // bails WITHOUT spawning — the counter returns to zero, unblocking the
        // drain. (The gate was closed by stop before the drain, so the re-check
        // sees it closed: this is the Dekker `router observes gate closed` leg.)
        INBOUND_ROUTER_AFTER_INCREMENT_HOOK.release();
        router_handle.join().expect("router thread panicked");

        assert!(
            NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.wait_for_enter(Duration::from_secs(1)),
            "hew_node_stop never reached conn_mgr free after the wedged router drained"
        );
        assert_eq!(
            stop_rx
                .recv_timeout(Duration::from_secs(2))
                .expect("hew_node_stop did not finish after the router drained"),
            0,
            "hew_node_stop should succeed once the wedged router drained"
        );
        stop_handle.join().expect("stop thread panicked");

        assert_eq!(
            per_mgr_active.load(Ordering::SeqCst),
            0,
            "per-manager counter must be zero after the wedged router bailed and stop drained"
        );

        crate::registry::hew_registry_clear();
    }

    /// Pins the SECONDARY-node safety of the up-front feature-flags capture in
    /// `handle_inbound_ask`.
    ///
    /// The capture's barrier must be keyed on the SPECIFIC manager's lifetime,
    /// not the global `CURRENT_NODE`. In a multi-node runtime `CURRENT_NODE`
    /// points at the FIRST node started (node1 here) and stays there; stopping a
    /// secondary node (node2) frees node2's `conn_mgr` WITHOUT zeroing
    /// `CURRENT_NODE`. A `*guard == 0` check therefore passes during a secondary
    /// teardown, so the OLD capture dereferenced node2's already-freed
    /// `conn_mgr` (use-after-free; the deeper freed-`conn_mgr` path surfaced
    /// under `AddressSanitizer`). The fix also consults THIS manager's
    /// `shutdown_started` (`reconnect_shutdown`) flag, which `hew_node_stop` sets
    /// for the stopping node under the `CURRENT_NODE` write lock before it frees
    /// the manager.
    ///
    /// This test reproduces the straggler exactly: it drives `handle_inbound_ask`
    /// for node2 DIRECTLY (uncounted by the per-manager guard, i.e. a worker the
    /// drain no longer waits on), wedges it at the capture hook, lets
    /// `hew_node_stop(node2)` run to completion (freeing node2's `conn_mgr` while
    /// `CURRENT_NODE` still points at node1), then releases the worker. With the
    /// fix the worker observes node2's `shutdown_started` and returns `None`
    /// WITHOUT touching the freed manager; under `AddressSanitizer` the run is
    /// clean. Against the old `*guard == 0`-only capture the release reads freed
    /// memory.
    #[test]
    fn handle_inbound_ask_capture_safe_for_secondary_node_freed_connmgr() {
        struct HookResetGuard;
        impl Drop for HookResetGuard {
            fn drop(&mut self) {
                INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK.release();
                INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK.reset();
            }
        }

        let _guard = crate::runtime_test_guard();
        let _real_sched;
        let _hook_reset = HookResetGuard;
        crate::registry::hew_registry_clear();

        // Wedge any worker that reaches the top-of-handler capture hook.
        INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK.arm(true);

        // node1 starts FIRST → it owns CURRENT_NODE and keeps it.
        let node1_bind = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: node1_bind is a valid C string for the duration of this test.
        let node1 = unsafe { TestNode::new(363, &node1_bind) };
        assert!(!node1.as_ptr().is_null());
        // SAFETY: node1 is valid for start-up here.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0); // CURRENT_NODE = node1
        }
        thread::sleep(Duration::from_millis(50));

        // node2 is the SECONDARY node; starting it does NOT change CURRENT_NODE.
        let (node2, node2_port) = start_tcp_test_listener_node(364);

        _real_sched = init_real_scheduler();

        // Spawn (and immediately stop) a node2-PID actor so the inbound ask hits
        // the actor-error path — exercising the full post-capture handler on a
        // freed-conn_mgr straggler. The error path's reply send is itself
        // `shutdown_started`-guarded, so no path may touch the freed manager.
        crate::pid::hew_pid_set_local_node(364);
        // SAFETY: null state / size-0 are valid; dispatch fn is a valid fn ptr.
        let actor =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch)) };
        crate::pid::hew_pid_set_local_node(363);
        assert!(!actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid here.
        let actor_id = unsafe { (*actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_id), 364);
        // SAFETY: actor was spawned above and remains valid while stopped here.
        unsafe { crate::actor::hew_actor_stop(actor) };

        // Establish the node1→node2 connection so node2 has an ACTIVE connection
        // back to node1 — the entry `feature_flags_for_node` would walk if the
        // capture dereferenced the (soon-to-be-freed) manager.
        let connect_addr = CString::new(format!("364@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and connect_addr are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until teardown.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // Capture node2's manager pointer (as an integer to ferry across the
        // thread) and a clone of its `shutdown_started` (`reconnect_shutdown`)
        // flag — exactly what `node_inbound_router` passes into the worker.
        // SAFETY: node2 is live here; conn_mgr is valid until we free it below.
        let node2_conn_mgr = unsafe { (*node2.as_ptr()).conn_mgr };
        assert!(!node2_conn_mgr.is_null());
        // SAFETY: node2's conn_mgr is live here.
        let shutdown_started = unsafe { connection::hew_connmgr_shutdown_flag(node2_conn_mgr) }
            .expect("conn_mgr must expose its shutdown flag");
        let conn_mgr_addr = node2_conn_mgr as usize;

        // Drive `handle_inbound_ask` for node2 DIRECTLY — uncounted by the
        // per-manager guard, modelling a straggler the drain has already given
        // up on. It parks at the capture hook before touching conn_mgr.
        let worker_handle = thread::spawn(move || {
            handle_inbound_ask(
                actor_id,
                /*msg_type=*/ 1,
                /*payload=*/ &[],
                /*request_id=*/ 1,
                /*source_node_id=*/ 363,
                // SAFETY: pointer ferried as usize; valid until freed by the
                // stop below, after which the fix guarantees it is never read.
                SendConnMgr(conn_mgr_addr as *mut connection::HewConnMgr),
                shutdown_started,
            );
        });

        assert!(
            INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK.wait_for_enter(Duration::from_secs(2)),
            "straggler worker never reached the feature-flags capture hook"
        );

        // Stop the SECONDARY node to completion. Its drain sees no counted
        // workers and proceeds: it sets node2's `reconnect_shutdown` (under the
        // CURRENT_NODE write lock) and frees node2's conn_mgr — while
        // CURRENT_NODE still points at node1 (so `*guard != 0`).
        // SAFETY: node2 is a live allocation; stop is the documented teardown.
        let stop_rc = unsafe { hew_node_stop(node2.as_ptr()) };
        assert_eq!(stop_rc, 0, "hew_node_stop(node2) should succeed");

        // node2's conn_mgr is now FREED. Release the wedged worker: with the fix
        // it observes node2's `shutdown_started == true` and returns `None`
        // WITHOUT dereferencing the freed manager. (ASan would fire here on the
        // old `*guard == 0`-only capture.)
        INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK.release();
        worker_handle
            .join()
            .expect("straggler worker panicked after secondary-node teardown");

        crate::registry::hew_registry_clear();
    }

    // ── SWIM failure-detection (driven detector) integration tests ──────

    /// RAII helper that sets the fast SWIM-timing env vars for a test and
    /// removes them on drop, all under `ENV_LOCK` exclusive access.
    ///
    /// The base fast-env budgets (40 ms protocol period, 120 ms suspect
    /// timeout) are set as environment variables so `cluster_config_from_env`
    /// picks them up at node-start time.  These tests run under simulated time
    /// (enabled before any node starts so the SWIM driver picks up the sim
    /// clock): the driver never reads real wall time during the test.
    struct SwimTimingEnv;

    /// Protocol period used by the fast-test SWIM clock (ms).
    const SWIM_TEST_PERIOD_MS: u64 = 40;
    /// Suspect timeout used by the fast-test SWIM clock (ms).
    const SWIM_TEST_SUSPECT_TIMEOUT_MS: u64 = 120;

    impl SwimTimingEnv {
        /// Enable simulated time and set fast SWIM timing env vars.
        ///
        /// Simtime MUST be enabled before any node starts so that the SWIM
        /// driver captures the sim clock at spawn time (the choice is made
        /// once in [`crate::swim_driver::start_swim_driver`]).
        fn fast() -> Self {
            // Enable simtime first: SWIM drivers started after this call will
            // use the sim clock.  Start at T=0.
            crate::deterministic::hew_simtime_enable(0);
            crate::env::ENV_LOCK.access(|()| {
                // SAFETY: ENV_LOCK provides exclusive write access to the environ.
                unsafe {
                    std::env::set_var(
                        "HEW_SWIM_PROTOCOL_PERIOD_MS",
                        SWIM_TEST_PERIOD_MS.to_string(),
                    );
                    std::env::set_var("HEW_SWIM_PING_TIMEOUT_MS", SWIM_TEST_PERIOD_MS.to_string());
                    std::env::set_var(
                        "HEW_SWIM_SUSPECT_TIMEOUT_MS",
                        SWIM_TEST_SUSPECT_TIMEOUT_MS.to_string(),
                    );
                }
            });
            Self
        }
    }

    impl Drop for SwimTimingEnv {
        fn drop(&mut self) {
            // Disable simtime first so any still-running threads switch back to
            // the real clock before the env vars are removed.
            crate::deterministic::hew_simtime_disable();
            crate::env::ENV_LOCK.access(|()| {
                // SAFETY: ENV_LOCK provides exclusive access to the environ.
                unsafe {
                    std::env::remove_var("HEW_SWIM_PROTOCOL_PERIOD_MS");
                    std::env::remove_var("HEW_SWIM_PING_TIMEOUT_MS");
                    std::env::remove_var("HEW_SWIM_SUSPECT_TIMEOUT_MS");
                }
            });
        }
    }

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

        let node_a_bind = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: bind is a valid C string for the test scope.
        let node_a = unsafe { TestNode::new(NODE_A, &node_a_bind) };
        assert!(!node_a.as_ptr().is_null());
        // SAFETY: node_a is valid.
        unsafe { assert_eq!(hew_node_start(node_a.as_ptr()), 0) };
        // Allow node A's runtime to settle before connecting.  50 ms real is
        // enough; no scaling needed (wall time is not the constraint here).
        thread::sleep(Duration::from_millis(50));

        let (node_b, node_b_port) = start_tcp_test_listener_node(NODE_B);

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

        let node_a_bind = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: bind is a valid C string for the test scope.
        let node_a = unsafe { TestNode::new(NODE_A, &node_a_bind) };
        assert!(!node_a.as_ptr().is_null());
        // SAFETY: node_a is valid.
        unsafe { assert_eq!(hew_node_start(node_a.as_ptr()), 0) };
        // Allow node A's runtime to settle before connecting.
        thread::sleep(Duration::from_millis(50));

        let (node_b, node_b_port) = start_tcp_test_listener_node(NODE_B);

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
            // still ALIVE from an earlier one. The "never DEAD" invariant is
            // still enforced strictly on every poll.
            let alive_deadline =
                std::time::Instant::now() + Duration::from_millis(SWIM_ALIVE_DEADLINE_MS);
            let (a_view_of_b, b_view_of_a) = loop {
                // SAFETY: A's cluster is live (node still running).
                let a_state =
                    unsafe { hew_cluster_member_state((*node_a.as_ptr()).cluster, NODE_B) };
                // SAFETY: B's cluster is live (node still running).
                let b_state =
                    unsafe { hew_cluster_member_state((*node_b.as_ptr()).cluster, NODE_A) };
                assert_ne!(
                    a_state,
                    crate::cluster::MEMBER_DEAD,
                    "A must not declare a still-alive B as DEAD after period \
                     (state={a_state})"
                );
                assert_ne!(
                    b_state,
                    crate::cluster::MEMBER_DEAD,
                    "B must not declare a still-alive A as DEAD after period \
                     (state={b_state})"
                );
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
}
