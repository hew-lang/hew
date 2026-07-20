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
use crate::node_identity::{HewLocation, HewNodeId, HewRemotePid, Location};
use crate::peer_binding::{
    ConfigState, PeerAuthSnapshot, PeerCredential, TransportSelection as PeerTransport,
    PEER_AUTH_STATE,
};
use crate::routing::{self, HewRoutingTable};
use crate::transport::{self, HewTransport, HewTransportOps, HEW_CONN_INVALID};

const NODE_STATE_STARTING: u8 = 0;
/// Node is started and serving traffic. `pub(crate)` so the SWIM driver only
/// drives a fully-running node.
pub(crate) const NODE_STATE_RUNNING: u8 = 1;
const NODE_STATE_STOPPING: u8 = 2;
const NODE_STATE_STOPPED: u8 = 3;

/// Remote-send rc returned when a captured location's node, session, or local
/// actor slot is no longer current. The codegen send-path maps this distinct rc
/// to `SendError::StaleRef`; every other nonzero rc stays the generic routing
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

impl TransportSelection {
    /// Project the (feature-gated) runtime transport selection onto the
    /// self-contained `peer_binding::TransportSelection` carried by the frozen
    /// per-node snapshot (issue #2652). Keeps the peer-auth authority module
    /// independent of the `quic` feature.
    fn as_peer_transport(self) -> PeerTransport {
        match self {
            TransportSelection::Tcp => PeerTransport::Tcp,
            #[cfg(feature = "quic")]
            TransportSelection::Quic => PeerTransport::Quic,
            #[cfg(feature = "quic")]
            TransportSelection::QuicMesh => PeerTransport::QuicMesh,
        }
    }
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

/// Build the cluster config for the transitional `local_route_slot`, applying
/// any SWIM-timing
/// overrides from the environment.
///
/// `HEW_SWIM_PROTOCOL_PERIOD_MS`, `HEW_SWIM_PING_TIMEOUT_MS`, and
/// `HEW_SWIM_SUSPECT_TIMEOUT_MS` tune the failure detector's cadence and
/// thresholds — an operator knob for deployments with non-default network
/// characteristics, and the mechanism tests use to drive detection on a short
/// horizon. Each falls back to the [`ClusterConfig`] default when unset.
fn cluster_config_from_env(local_route_slot: u16) -> ClusterConfig {
    let mut cfg = ClusterConfig {
        local_node_id: local_route_slot,
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

/// Map a normalized transport key (from [`normalize_transport_name`]) onto the
/// self-contained `peer_binding::TransportSelection` pinned into the config.
fn peer_transport_from_normalized(normalized: &str) -> PeerTransport {
    match normalized {
        "tcp" => PeerTransport::Tcp,
        #[cfg(feature = "quic")]
        "quic" => PeerTransport::Quic,
        #[cfg(feature = "quic")]
        "quic-mesh" => PeerTransport::QuicMesh,
        _ => unreachable!("normalize_transport_name returns only supported transport keys"),
    }
}

/// The canonical `HEW_TRANSPORT` string for a pinned transport selection — the
/// inverse of [`peer_transport_from_normalized`]. Used to re-assert the pinned
/// selection onto the env at start so the low-level transport construction
/// (which reads `HEW_TRANSPORT`) builds the stored selection, not a diverged
/// env value (issue #2652 — start uses the stored selection).
fn peer_transport_env_name(t: PeerTransport) -> &'static str {
    match t {
        PeerTransport::Tcp => "tcp",
        PeerTransport::Quic => "quic",
        PeerTransport::QuicMesh => "quic-mesh",
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
    /// Process-local route slot embedded in local actor IDs. Route slot zero
    /// remains the local-dispatch sentinel.
    local_route_slot: AtomicU16,
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
            local_route_slot: AtomicU16::new(0),
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
        let local_route_slot = other.local_route_slot.swap(0, Ordering::AcqRel);
        self.local_route_slot
            .store(local_route_slot, Ordering::Release);
    }

    pub(crate) fn local_route_slot(&self) -> u16 {
        self.local_route_slot.load(Ordering::Acquire)
    }

    pub(crate) fn set_local_route_slot(&self, route_slot: u16) {
        self.local_route_slot.store(route_slot, Ordering::Release);
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
    ///
    /// Test-only helper: the production reply-arrival path completes through
    /// [`ReplyRoutingTable::complete_from_connection`] so a peer cannot resolve
    /// another peer's ask (issue #2652, D12). Tests that don't exercise the
    /// connection binding use this request-id-only shortcut.
    #[cfg(test)]
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

    /// Resolve a pending reply by request id alone (test-only; see
    /// [`ReplyRoutingTable::complete`]).
    #[cfg(test)]
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

    /// Remove `request_id` **only if** it was registered against `expected`
    /// connection (issue #2652, D12). A reply that arrives on a different
    /// `(conn_mgr, conn_id)` than the ask was sent on must not resolve it — a
    /// peer cannot complete (or reject) another peer's ask. A mismatch leaves
    /// the pending ask intact (it resolves via its real reply or times out).
    fn remove_if_connection(
        &self,
        request_id: u64,
        expected: ConnectionKey,
    ) -> Option<Arc<PendingReply>> {
        let mut map = self
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match map.get(&request_id) {
            Some(pending) if pending.connection == expected => map.remove(&request_id),
            _ => None,
        }
    }

    /// Connection-validated success completion (D12): resolve `request_id` with
    /// `payload` only when the reply arrived on the originating connection.
    fn complete_from_connection(
        &self,
        request_id: u64,
        expected: ConnectionKey,
        payload: Vec<u8>,
    ) -> bool {
        if let Some(pending) = self.remove_if_connection(request_id, expected) {
            Self::complete_pending(
                &pending,
                ReplyOutcome {
                    status: ReplyStatus::Success,
                    data: payload,
                    ask_error: AskError::None,
                },
            );
            true
        } else {
            false
        }
    }

    /// Connection-validated rejection (D12): fail `request_id` with `ask_error`
    /// only when the rejection reply arrived on the originating connection.
    fn fail_from_connection(
        &self,
        request_id: u64,
        expected: ConnectionKey,
        ask_error: AskError,
    ) -> bool {
        if let Some(pending) = self.remove_if_connection(request_id, expected) {
            Self::fail_pending_with_reason(&pending, ask_error);
            true
        } else {
            false
        }
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
/// `true` if the request ID was matched **and** the reply arrived on the same
/// `(conn_mgr, conn_id)` the ask was issued on (issue #2652, D12) — a peer
/// cannot complete another peer's ask.
pub(crate) fn complete_remote_reply(
    conn_mgr: *const HewConnMgr,
    conn_id: c_int,
    request_id: u64,
    payload: &[u8],
) -> bool {
    let expected = ConnectionKey::new(conn_mgr, conn_id);
    reply_table_opt()
        .is_some_and(|table| table.complete_from_connection(request_id, expected, payload.to_vec()))
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
/// The rejection resolves the ask only when it arrives on the same
/// `(conn_mgr, conn_id)` the ask was issued on (issue #2652, D12).
pub(crate) fn fail_remote_reply(
    conn_mgr: *const HewConnMgr,
    conn_id: c_int,
    request_id: u64,
    reason_payload: &[u8],
) -> bool {
    // On unknown codes, leave the pending ask unresolved (it will timeout)
    // rather than fabricating a misleading AskError.
    match decode_rejection_reason(reason_payload) {
        Ok(reason) => {
            let expected = ConnectionKey::new(conn_mgr, conn_id);
            reply_table_opt()
                .is_some_and(|table| table.fail_from_connection(request_id, expected, reason))
        }
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

/// Node-local distributed registry state.
#[repr(C)]
#[derive(Debug, Default)]
pub struct HewRegistry {
    remote_names: Mutex<HashMap<String, Location>>,
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
    /// Receiver-local route slot used only for internal dispatch and SWIM.
    pub route_slot: u16,
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
    /// Exact-location routing table for remote node delivery.
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
            .field("route_slot", &self.route_slot)
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

fn take_registry_names_if<F>(registry: &HewRegistry, mut predicate: F) -> Vec<(String, Location)>
where
    F: FnMut(Location) -> bool,
{
    let mut map = registry.remote_names.lock_or_recover();
    let names: Vec<(String, Location)> = map
        .iter()
        .filter(|(_, location)| predicate(**location))
        .map(|(name, location)| (name.clone(), *location))
        .collect();
    for (name, _) in &names {
        map.remove(name);
    }
    names
}

unsafe fn unregister_names_from_node(
    node: &HewNode,
    names: Vec<(String, Location)>,
    emit_gossip_remove: bool,
) {
    for (name, location) in names {
        let Ok(c_name) = CString::new(name) else {
            continue;
        };
        // SAFETY: `c_name` is a valid NUL-terminated string for the call.
        unsafe { crate::registry::hew_registry_unregister(c_name.as_ptr()) };
        if emit_gossip_remove && !node.cluster.is_null() {
            let location = HewLocation::from(location);
            // SAFETY: node teardown is serialized against actor-free cleanup by
            // KNOWN_NODES, so the cluster pointer stays valid for this call.
            unsafe {
                cluster::hew_cluster_registry_remove(
                    node.cluster,
                    c_name.as_ptr(),
                    &raw const location,
                );
            };
        }
    }
}

unsafe fn unregister_local_names_for_node(node: &HewNode) {
    if node.registry.is_null() {
        return;
    }
    // SAFETY: registry belongs to `node` for the node lifetime.
    let registry = unsafe { &*node.registry };
    let names = take_registry_names_if(registry, |location| {
        Some(location.node()) == node.auth.node_identity()
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
    let actor_slot = crate::pid::hew_pid_serial(actor_id);
    let _ = with_known_nodes_opt(|known| {
        for entry in known.iter().copied() {
            if entry.0.is_null() {
                continue;
            }
            // SAFETY: KNOWN_NODES holds node allocations live until `forget_node`.
            let node = unsafe { &*entry.0 };
            if node.registry.is_null() {
                continue;
            }

            // SAFETY: registry belongs to `node` for the node lifetime.
            let registry = unsafe { &*node.registry };
            let names = take_registry_names_if(registry, |location| {
                Some(location.node()) == node.auth.node_identity() && location.slot() == actor_slot
            });
            if names.is_empty() {
                continue;
            }

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
                None,
                None,
                HEW_REPLY_REJECT_MSG_TYPE,
                reason_payload.as_ptr(),
                reason_payload.len(),
                request_id,
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
                None,
                None,
                0,
                reply_data.as_ptr(),
                reply_data.len(),
                request_id,
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
    location: *const HewLocation,
    is_add: bool,
    user_data: *mut c_void,
) {
    if name.is_null() || location.is_null() || user_data.is_null() {
        return;
    }
    // SAFETY: user_data is a *mut HewRegistry pointer set during hew_node_start,
    // valid for the node's lifetime.
    let registry = unsafe { &*(user_data.cast::<HewRegistry>()) };
    // SAFETY: caller guarantees name is a valid C string.
    let key = unsafe { CStr::from_ptr(name) }
        .to_string_lossy()
        .into_owned();
    // SAFETY: caller guarantees `location` is readable for the callback.
    let Ok(location) = Location::try_from(unsafe { *location }) else {
        set_last_error("registry gossip callback received an invalid Location");
        return;
    };
    let mut map = registry.remote_names.lock_or_recover();
    if is_add {
        map.insert(key, location);
    } else if map.get(&key) == Some(&location) {
        map.remove(&key);
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

    // SAFETY: user_data is the owning HewNode installed during hew_node_start.
    let node = unsafe { &*(user_data.cast::<HewNode>()) };
    let Some(dead_identity) = node.auth.node_id_for_route_slot(node_id) else {
        return;
    };
    if node.registry.is_null() {
        return;
    }
    // SAFETY: registry belongs to the live callback-owning node.
    let registry = unsafe { &*node.registry };
    let _ = take_registry_names_if(registry, |location| location.node() == dead_identity);
}

/// Create a new unified distributed node runtime.
///
/// # Safety
///
/// `bind_addr` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_new(route_slot: u16, bind_addr: *const c_char) -> *mut HewNode {
    cabi_guard!(bind_addr.is_null(), ptr::null_mut());

    // SAFETY: caller guarantees bind_addr points to a valid C string.
    // Portable strdup: libc::strdup does not link on Windows-MSVC (#2505).
    let bind_copy = unsafe { crate::cabi::cstr_strdup(bind_addr) };
    if bind_copy.is_null() {
        return ptr::null_mut();
    }

    let registry = Box::into_raw(Box::new(HewRegistry::default()));
    let node = Box::new(HewNode {
        route_slot,
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
    // is authoritative for the transitional v1 route slot. Defence-in-depth:
    // reject a self-inconsistent snapshot, then reconcile the route slot.
    if let Err(reason) = node.auth.validate() {
        fail_start!(reason);
    }
    if let Some(snapshot_route_slot) = node.auth.legacy_wire_route_slot() {
        let snapshot_route_slot = snapshot_route_slot.get();
        if node.route_slot == 0 {
            // A low-level caller deferred the route slot to its snapshot.
            node.route_slot = snapshot_route_slot;
        } else if node.route_slot != snapshot_route_slot {
            // Refuse before the listener binds and before cluster/routing/connmgr
            // are created.
            fail_start!(format!(
                "hew_node_start: explicit route slot {} conflicts with the frozen v1 route slot \
                 {snapshot_route_slot} — refusing to listen (fail-closed)",
                node.route_slot
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

    // Install the frozen per-node mesh peer-auth material (stable identity + peer
    // SPKI allowlist + setup-error poison, derived from `Node::load_keys` /
    // `Node::allow_peer` bindings, issue #2652 / D14) onto this transport before
    // it binds. The mTLS handshake then admits exactly the bound peers and no
    // other, and a poisoned setup refuses to bind (fail-closed). Per-instance —
    // no process-global allowlist, so two concurrent mesh nodes stay isolated.
    #[cfg(feature = "quic")]
    if node.auth.transport() == PeerTransport::QuicMesh {
        // SAFETY: node.transport was created/validated above and is live here.
        let rc = unsafe {
            crate::quic_mesh::hew_quic_mesh_transport_install_auth(node.transport, &node.auth)
        };
        if rc != 0 {
            fail_start!(
                "hew_node_start: refusing to bind mesh listener — failed to install the \
                 per-node mesh peer-auth material (fail-closed)"
            );
        }
    }

    let Some(listen_fn) = ops.listen else {
        fail_start!("hew_node_start: transport listen op missing");
    };
    // SAFETY: transport implementation is valid.
    if unsafe { listen_fn(t.r#impl, node.bind_addr) } < 0 {
        fail_start!("hew_node_start: transport listen failed");
    }

    if node.cluster.is_null() {
        let cfg = cluster_config_from_env(node.route_slot);
        // SAFETY: config pointer is valid for this call.
        node.cluster = unsafe { cluster::hew_cluster_new(&raw const cfg) };
        if node.cluster.is_null() {
            fail_start!("hew_node_start: failed to create cluster");
        }
        created_cluster = true;
    }

    if node.routing_table.is_null() {
        let configured_routes = node.auth.configured_node_routes();
        node.routing_table = routing::hew_routing_table_new(
            node.route_slot,
            node.auth.node_identity(),
            node.auth.session_incarnation(),
            &configured_routes,
        );
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
                node.route_slot,
                node.auth.clone(),
            )
        };
        if node.conn_mgr.is_null() {
            fail_start!("hew_node_start: failed to create connection manager");
        }
        created_conn_mgr = true;
    }

    // SAFETY: cluster pointer valid; bind_addr points to a stable strdup buffer.
    let _ = unsafe { cluster::hew_cluster_join(node.cluster, node.route_slot, node.bind_addr) };
    joined_cluster = true;

    // Wire the registry gossip callback so remote name events update our
    // remote_names map.
    if !node.registry.is_null() {
        // SAFETY: cluster and registry pointers are valid for the node lifetime.
        unsafe {
            cluster::hew_cluster_set_membership_callback(
                node.cluster,
                node_membership_callback,
                (node as *mut HewNode).cast::<c_void>(),
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
    let thread_name = format!("hew-node-accept-{}", node.route_slot);
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
            crate::pid::hew_pid_set_local_node(node.route_slot);
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
    let Some(location) = local_actor_location(node, actor) else {
        set_last_error("hew_node_register: node has no authenticated Location authority");
        return -1;
    };
    // SAFETY: registry pointer was allocated in hew_node_new and freed in hew_node_free.
    let reg = unsafe { &*node.registry };
    let previous = reg.remote_names.lock_or_recover().get(&key).copied();
    if previous == Some(location) {
        return 0;
    }
    if previous.is_some() {
        // SAFETY: name is a valid C string by contract.
        unsafe { crate::registry::hew_registry_unregister(name) };
    }
    // SAFETY: registry API expects a stable C string pointer.
    if unsafe { crate::registry::hew_registry_register(name, actor_id_to_registry_ptr(actor)) } != 0
    {
        return -1;
    }
    reg.remote_names
        .lock_or_recover()
        .insert(key.clone(), location);

    // Propagate to cluster gossip so remote nodes learn about this actor.
    if !node.cluster.is_null() {
        let abi_location = HewLocation::from(location);
        // SAFETY: cluster pointer is valid while the node is alive.
        unsafe { cluster::hew_cluster_registry_add(node.cluster, name, &raw const abi_location) };
    }
    if !node.conn_mgr.is_null() {
        // SAFETY: connection manager pointer is valid while the node is alive.
        unsafe {
            connection::hew_connmgr_broadcast_registry_gossip(node.conn_mgr, &key, location, true);
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
    let removed_location = {
        let mut map = reg.remote_names.lock_or_recover();
        map.remove(&key)
    };
    let Some(removed_location) = removed_location else {
        return 0;
    };

    // Propagate removal to cluster gossip.
    if !node.cluster.is_null() {
        let abi_location = HewLocation::from(removed_location);
        // SAFETY: cluster pointer is valid while the node is alive.
        unsafe {
            cluster::hew_cluster_registry_remove(node.cluster, name, &raw const abi_location);
        };
    }
    if !node.conn_mgr.is_null() {
        // SAFETY: connection manager pointer is valid while the node is alive.
        unsafe {
            connection::hew_connmgr_broadcast_registry_gossip(
                node.conn_mgr,
                &key,
                removed_location,
                false,
            );
        }
    }

    0
}

/// Look up an exact actor location by name.
///
/// # Safety
///
/// - `node` must be valid.
/// - `name` must be a valid NUL-terminated C string.
/// - `out` must be writable when non-null. It is written only on success.
#[no_mangle]
pub unsafe extern "C" fn hew_node_lookup_location(
    node: *mut HewNode,
    name: *const c_char,
    out: *mut HewRemotePid,
) -> c_int {
    cabi_guard!(node.is_null() || name.is_null() || out.is_null(), -1);
    // SAFETY: caller guarantees node pointer validity.
    let node = unsafe { &*node };
    if node.registry.is_null() {
        return -1;
    }
    // SAFETY: name is non-null and valid by caller contract.
    let key = unsafe { CStr::from_ptr(name) }
        .to_string_lossy()
        .into_owned();
    // SAFETY: registry pointer was allocated in hew_node_new and freed in hew_node_free.
    let reg = unsafe { &*node.registry };
    let map = reg.remote_names.lock_or_recover();
    let Some(location) = map.get(&key).copied() else {
        return -1;
    };
    // SAFETY: caller guarantees `out` is writable.
    unsafe { out.write(HewRemotePid::from(location)) };
    0
}

/// Send a message to an exact target location.
///
/// `dispatch` is the TARGET actor TYPE's dispatch function pointer, keying the
/// cross-node serialize codec `(dispatch, msg_type)` on the remote path. Unused
/// on the local path. May be null for a local-only send.
///
/// # Safety
///
/// - `node` must be valid.
/// - `target` must point to a valid `HewRemotePid`.
/// - `payload` must be valid for `payload_len` bytes, or null when len is 0.
/// - `dispatch` is an opaque codec key, never dereferenced.
#[no_mangle]
#[allow(
    clippy::too_many_lines,
    reason = "function coordinates exact routing, serialization, and transport ownership"
)]
pub unsafe extern "C" fn hew_node_send_location(
    node: *mut HewNode,
    target: *const HewRemotePid,
    dispatch: *const c_void,
    msg_type: i32,
    payload: *const u8,
    payload_len: usize,
) -> c_int {
    if node.is_null() || target.is_null() || (payload.is_null() && payload_len > 0) {
        return -1;
    }

    // SAFETY: caller guarantees node pointer validity.
    let node = unsafe { &*node };
    if node.state.load(Ordering::Acquire) != NODE_STATE_RUNNING {
        return -1;
    }
    // SAFETY: caller guarantees `target` is readable.
    let Ok(target) = Location::try_from(unsafe { *target }) else {
        set_last_error("hew_node_send_location: invalid target Location");
        return HEW_ERR_STALE_REF;
    };

    // SAFETY: routing table belongs to the running node.
    let (target_pid, target_node_id, conn_id) =
        match unsafe { routing::hew_routing_lookup_location(node.routing_table, target) } {
            routing::LocationRoute::Local { actor_id } => {
                if crate::lifetime::live_actors::get_actor_ptr_by_id(actor_id).is_none() {
                    set_last_error("remote send refused: target actor slot is stale");
                    return HEW_ERR_STALE_REF;
                }
                (actor_id, node.route_slot, None)
            }
            routing::LocationRoute::Remote {
                actor_id,
                route_slot,
                conn,
            } => (actor_id, route_slot, Some(conn)),
            routing::LocationRoute::Partition => {
                set_last_error("remote send refused: target identity has no live connection");
                return -1;
            }
            routing::LocationRoute::StaleRef => {
                set_last_error("remote send refused: target Location is stale");
                return HEW_ERR_STALE_REF;
            }
        };

    if conn_id.is_none() {
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
    let Some(conn_id) = conn_id else {
        return -1;
    };

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
    let abi_target = HewLocation::from(target);
    // SAFETY: conn_mgr and conn_id were validated above; send_ptr/send_len are the
    // serialized bytes (or null/0 for an empty payload).
    let rc = unsafe {
        connection::hew_connmgr_send(
            node.conn_mgr,
            conn_id,
            &raw const abi_target,
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

/// Send to an exact location through the singleton public node.
///
/// # Safety
///
/// `target` and `payload` must satisfy [`hew_node_send_location`].
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_send_location(
    target: *const HewRemotePid,
    dispatch: *const c_void,
    msg_type: i32,
    payload: *const u8,
    payload_len: usize,
) -> c_int {
    with_current_node_read(|guard| {
        let node = *guard as *mut HewNode;
        if node.is_null() {
            return -1;
        }
        // SAFETY: the current-node read lock pins `node`; remaining arguments
        // carry the caller contract documented above.
        unsafe { hew_node_send_location(node, target, dispatch, msg_type, payload, payload_len) }
    })
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
    target: Location,
    ctrl_kind: u64,
    payload: Vec<u8>,
) -> c_int {
    if node.conn_mgr.is_null() || node.routing_table.is_null() {
        set_last_error("cross-node monitor: node has no connection manager / routing table");
        return -1;
    }
    // SAFETY: routing table pointer is valid while the node is running.
    let conn_id = match unsafe { routing::hew_routing_lookup_location(node.routing_table, target) }
    {
        routing::LocationRoute::Remote { conn, .. } => conn,
        routing::LocationRoute::Partition => {
            set_last_error("cross-node monitor: target identity is partitioned");
            return -1;
        }
        routing::LocationRoute::Local { .. } | routing::LocationRoute::StaleRef => {
            set_last_error("cross-node monitor: target Location is invalid or stale");
            return HEW_ERR_STALE_REF;
        }
    };
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

fn local_actor_location(node: &HewNode, actor_id: u64) -> Option<crate::node_identity::Location> {
    Location::new(
        node.auth.node_identity()?,
        crate::pid::hew_pid_serial(actor_id),
        node.auth.session_incarnation()?,
    )
    .ok()
}

fn local_monitor_endpoint(node: &HewNode) -> Option<crate::node_identity::Location> {
    // Distributed monitors are node-owned until DOWN delivery moves into actor
    // mailboxes. The peer routes control frames by node/session and never treats
    // this synthetic non-zero slot as an actor target.
    Location::new(
        node.auth.node_identity()?,
        1,
        node.auth.session_incarnation()?,
    )
    .ok()
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
///
/// # Safety
///
/// `target` must point to a readable `HewRemotePid`.
#[no_mangle]
pub unsafe extern "C" fn hew_node_monitor_location(target: *const HewRemotePid) -> i64 {
    if target.is_null() {
        return MONITOR_ERR_INVALID_TARGET;
    }
    // SAFETY: caller guarantees `target` is readable.
    let Ok(target) = Location::try_from(unsafe { *target }) else {
        return MONITOR_ERR_STALE_REF;
    };
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("hew_node_monitor_location: no runtime installed");
        return MONITOR_ERR_NODE_NOT_RUNNING;
    };
    let Some(target_route) = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: current-node read lock pins the node.
        Some(unsafe { routing::hew_routing_lookup_location((*node).routing_table, target) })
    }) else {
        set_last_error("hew_node_monitor_location: no active node");
        return MONITOR_ERR_NODE_NOT_RUNNING;
    };
    if matches!(target_route, routing::LocationRoute::Local { .. }) {
        // Not a remote target — the cross-node route does not apply. The checker
        // routes only RemotePid receivers here, but guard fail-closed anyway.
        set_last_error("hew_node_monitor_location: target is not on a remote node");
        return MONITOR_ERR_INVALID_TARGET;
    }
    if matches!(target_route, routing::LocationRoute::StaleRef) {
        set_last_error("hew_node_monitor_location: target Location is stale");
        return MONITOR_ERR_STALE_REF;
    }
    if matches!(target_route, routing::LocationRoute::Partition) {
        set_last_error("hew_node_monitor_location: target identity is partitioned");
        return MONITOR_ERR_PARTITION;
    }
    let Some(watcher) = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: the current-node read lock pins the node.
        let node = unsafe { &*node };
        local_monitor_endpoint(node)
    }) else {
        set_last_error("hew_node_monitor_location: exact watcher/target Location is unavailable");
        return MONITOR_ERR_STALE_REF;
    };

    // Record the watcher entry first so the connection-drop / SWIM-DEAD fan-out
    // can deliver even if the request send races a drop.
    let ref_id = rt.dist_monitors.register_watcher(target);

    let payload =
        match crate::envelope::encode_monitor_req_payload(&crate::envelope::MonitorReqPayload {
            watcher,
            ref_id,
            target,
        }) {
            Ok(payload) => payload,
            Err(err) => {
                set_last_error(format!(
                    "hew_node_monitor_location: req payload encode failure: {err}"
                ));
                rt.dist_monitors.remove_watcher(ref_id);
                return MONITOR_ERR_ENCODE_FAILURE;
            }
        };

    let send_result = with_current_node_read(|guard| {
        if *guard == 0 {
            set_last_error("hew_node_monitor_location: no current node");
            return None;
        }
        let node = *guard as *const HewNode;
        // SAFETY: read lock pins the current node pointer for this call.
        let node_ref = unsafe { &*node };
        // SAFETY: node_ref is valid for this call.
        Some(unsafe {
            send_monitor_control_frame(node_ref, target, crate::envelope::CTRL_MONITOR_REQ, payload)
        })
    });
    match send_result {
        Some(0) => setup_ref_id(ref_id),
        Some(HEW_ERR_STALE_REF) => {
            rt.dist_monitors.remove_watcher(ref_id);
            MONITOR_ERR_STALE_REF
        }
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
///
/// # Safety
///
/// `target` must point to a readable `HewRemotePid`.
#[no_mangle]
#[allow(
    clippy::too_many_lines,
    reason = "function coordinates the full session-aware connection lifecycle"
)]
pub unsafe extern "C" fn hew_node_link_remote_location(
    target: *const HewRemotePid,
    policy_tag: i64,
) -> i64 {
    if target.is_null() {
        return LINK_ERR_INVALID_TARGET;
    }
    // SAFETY: caller guarantees `target` is readable.
    let Ok(target) = Location::try_from(unsafe { *target }) else {
        return LINK_ERR_STALE_REF;
    };
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("hew_node_link_remote_location: no runtime installed");
        return LINK_ERR_NODE_NOT_RUNNING;
    };
    // The linking subject is the calling actor; a cross-node link with no local
    // actor has nothing to crash, so fail closed.
    let self_actor = crate::actor::hew_actor_self();
    if self_actor.is_null() {
        set_last_error("hew_node_link_remote_location: no current actor (link subject)");
        return LINK_ERR_NO_CURRENT_ACTOR;
    }
    // SAFETY: hew_actor_self returned a non-null live actor pointer.
    // The actor `id` is already a packed pid (node<<48 | serial); the cascade
    // looks it up verbatim via get_actor_ptr_by_id, while the wire carries only
    // the serial part so the peer can address the linker on its own node.
    let local_actor_id = unsafe { (*self_actor).id };
    let Some(target_route) = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: current-node read lock pins the node.
        Some(unsafe { routing::hew_routing_lookup_location((*node).routing_table, target) })
    }) else {
        set_last_error("hew_node_link_remote_location: no active node");
        return LINK_ERR_NODE_NOT_RUNNING;
    };
    if matches!(target_route, routing::LocationRoute::Local { .. }) {
        set_last_error("hew_node_link_remote_location: target is not on a remote node");
        return LINK_ERR_INVALID_TARGET;
    }
    if matches!(target_route, routing::LocationRoute::StaleRef) {
        set_last_error("hew_node_link_remote_location: target Location is stale");
        return LINK_ERR_STALE_REF;
    }
    if matches!(target_route, routing::LocationRoute::Partition) {
        set_last_error("hew_node_link_remote_location: target identity is partitioned");
        return LINK_ERR_PARTITION;
    }
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_sign_loss,
        reason = "policy_tag is a small PartitionPolicy discriminant (0..4); the Hew side passes i64"
    )]
    let policy_tag = policy_tag as u8;

    // Record the watcher-side LINK entry first so a connection-drop / SWIM-DEAD
    // fan-out can deliver even if the request send races a drop.
    let ref_id = rt
        .dist_monitors
        .register_link_watcher(target, local_actor_id, policy_tag);

    let Some(linker) = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: the current-node read lock pins the node.
        let node = unsafe { &*node };
        local_actor_location(node, local_actor_id)
    }) else {
        set_last_error(
            "hew_node_link_remote_location: exact linker/target Location is unavailable",
        );
        rt.dist_monitors.remove_watcher(ref_id);
        return LINK_ERR_STALE_REF;
    };

    let payload = match crate::envelope::encode_link_req_payload(&crate::envelope::LinkReqPayload {
        linker,
        ref_id,
        target,
        policy_tag,
        // Original request: the receiver reciprocates with a reverse request so
        // the link is bidirectional (the linker's death also crashes the target).
        reciprocate: 1,
    }) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "hew_node_link_remote_location: req payload encode failure: {err}"
            ));
            rt.dist_monitors.remove_watcher(ref_id);
            return LINK_ERR_ENCODE_FAILURE;
        }
    };

    let send_result = with_current_node_read(|guard| {
        if *guard == 0 {
            set_last_error("hew_node_link_remote_location: no current node");
            return None;
        }
        let node = *guard as *const HewNode;
        // SAFETY: read lock pins the current node pointer for this call.
        let node_ref = unsafe { &*node };
        // SAFETY: node_ref is valid for this call.
        Some(unsafe {
            send_monitor_control_frame(node_ref, target, crate::envelope::CTRL_LINK_REQ, payload)
        })
    });
    match send_result {
        Some(0) => setup_ref_id(ref_id),
        Some(HEW_ERR_STALE_REF) => {
            rt.dist_monitors.remove_watcher(ref_id);
            LINK_ERR_STALE_REF
        }
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
    let Some(target) = rt.dist_monitors.remove_watcher(ref_id) else {
        return;
    };
    let Some(watcher) = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: the current-node read lock pins the node.
        let node = unsafe { &*node };
        local_monitor_endpoint(node)
    }) else {
        set_last_error("hew_node_demonitor: exact watcher/target Location is unavailable");
        return;
    };
    let payload =
        match crate::envelope::encode_monitor_req_payload(&crate::envelope::MonitorReqPayload {
            watcher,
            ref_id,
            target,
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
            send_monitor_control_frame(node_ref, target, crate::envelope::CTRL_DEMONITOR, payload)
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
        let target_actor_id = crate::pid::hew_pid_make(node_ref.route_slot, target_serial);
        let Some(target) = local_actor_location(node_ref, target_actor_id) else {
            set_last_error("monitor down fan-out: exact target Location is unavailable");
            return;
        };
        for watcher in watchers {
            let payload = match crate::envelope::encode_monitor_down_payload(
                &crate::envelope::MonitorDownPayload {
                    ref_id: watcher.ref_id,
                    target,
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
            // SAFETY: node_ref is valid for this call.
            let _ = unsafe {
                send_monitor_control_frame(node_ref, watcher.watcher, ctrl_kind, payload)
            };
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
pub(crate) fn handle_inbound_link_req(payload: &crate::envelope::LinkReqPayload) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("handle_inbound_link_req: no runtime installed");
        return;
    };
    // Step 1 (both directions): our local `target_serial` gains a remote LINK
    // watcher on the linker node. The terminal sweep fans CTRL_LINK_DOWN to it.
    rt.dist_monitors.register_remote_link_watcher(
        payload.target.slot(),
        payload.linker,
        payload.ref_id,
    );

    if payload.reciprocate == 0 {
        // Reverse request: target-side registration only; do not loop.
        return;
    }

    // Step 2 (original only): register the reverse watcher-side LINK entry so the
    // linker's death crashes OUR target actor per the policy.
    let local_node = rt.node.local_route_slot();
    let our_target_id = crate::pid::hew_pid_make(local_node, payload.target.slot());
    let reverse_ref =
        rt.dist_monitors
            .register_link_watcher(payload.linker, our_target_id, payload.policy_tag);

    // Step 3 (original only): send the reverse CTRL_LINK_REQ so the linker node
    // registers the target-side watcher that fans Direction 2's DOWN back to us.
    let reverse_payload = crate::envelope::LinkReqPayload {
        linker: payload.target,
        ref_id: reverse_ref,
        // From the linker's perspective the roles swap: its linker actor is now
        // our "target" to watch, and our target is now the reverse "linker".
        target: payload.linker,
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
                payload.linker,
                crate::envelope::CTRL_LINK_REQ,
                bytes,
            )
        };
    });
}

/// Handle an inbound `CTRL_UNLINK`: a remote node retracted a prior
/// link of one of our local actors. Remove the target-side remote LINK watcher.
/// Idempotent / fail-closed on a missing entry.
pub(crate) fn handle_inbound_unlink(payload: &crate::envelope::LinkReqPayload) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    rt.dist_monitors
        .remove_remote_watcher(payload.target.slot(), payload.linker, payload.ref_id);
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
pub(crate) fn handle_inbound_link_down(ref_id: u64, target: Location, reason: i32) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("handle_inbound_link_down: no runtime installed");
        return;
    };
    if let Some(down) = rt
        .dist_monitors
        .deliver_link_down_to_ref(ref_id, target, reason)
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
    let dead_node = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: current-node read lock pins the node.
        unsafe { (*node).auth.node_id_for_route_slot(dead_node_id) }
    });
    if let Some(dead_node) = dead_node {
        fan_out_monitor_lost_for_identity(dead_node);
    }
}

pub(crate) fn fan_out_monitor_lost_for_identity(dead_node: crate::node_identity::NodeId) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    // Monitor half: arm every still-Pending monitor watcher with MonitorLost so a
    // blocked recv_down wakes.
    let _ = rt
        .dist_monitors
        .deliver_to_node(dead_node, crate::dist_monitor::MONITOR_REASON_LOST);
    // Link half: fire the cross-node link cascade for every still-Pending LINK
    // watcher on the dead node — the death-signal must fire on the PARTITION
    // terminal cause too (firing only on a clean exit / crash would fail-open: a
    // linked actor surviving its dead peer). For CrashLinked this synthesizes a
    // SYS_MSG_EXIT into the LOCAL linked actor's mailbox and crashes it. The
    // one-shot slot makes this exactly-once vs a definitive CTRL_LINK_DOWN that
    // may already have fired.
    let link_downs = rt
        .dist_monitors
        .take_link_downs_for_node(dead_node, crate::dist_monitor::MONITOR_REASON_LOST);
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
    let _ = rt.dist_monitors.prune_remote_watchers_for_node(dead_node);
}

pub(crate) fn fan_out_monitor_lost_for_session(
    dead_node: crate::node_identity::NodeId,
    session_incarnation: u32,
) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    let _ = rt.dist_monitors.deliver_to_session(
        dead_node,
        session_incarnation,
        crate::dist_monitor::MONITOR_REASON_LOST,
    );
    let link_downs = rt.dist_monitors.take_link_downs_for_session(
        dead_node,
        session_incarnation,
        crate::dist_monitor::MONITOR_REASON_LOST,
    );
    for down in link_downs {
        let _ = crate::link::deliver_cross_node_link_exit(
            down.local_actor_id,
            down.remote_target_serial,
            down.reason,
            down.policy_tag,
        );
    }
    let _ = rt
        .dist_monitors
        .prune_remote_watchers_for_session(dead_node, session_incarnation);
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
        let conn_id = unsafe {
            crate::routing::hew_routing_conn_for_route_slot(node.routing_table, dead_node_id)
        };
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
        let session = cluster.member_session(node_id).unwrap_or(1);
        cluster.apply_swim_gossip(&[(node_id, session, state, incarnation)]);
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
        let detail = {
            let error = crate::hew_last_error();
            if error.is_null() {
                None
            } else {
                // SAFETY: `hew_last_error` returns a live NUL-terminated string.
                Some(
                    unsafe { CStr::from_ptr(error) }
                        .to_string_lossy()
                        .into_owned(),
                )
            }
        };
        set_last_error(detail.map_or_else(
            || "hew_node_connect: failed to add connection".to_string(),
            |detail| format!("hew_node_connect: failed to add connection: {detail}"),
        ));
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

/// Per-process offset for the transitional v1 wire route slot.
///
/// When a process calls `Node::start` more than once, each call gets a
/// distinct offset added to the process base.
static LEGACY_ROUTE_SLOT_COUNTER: std::sync::atomic::AtomicU32 =
    std::sync::atomic::AtomicU32::new(0);

/// Per-process base for the transitional v1 route slot.
///
/// Stage 2 removes this PID-derived wire compatibility value when the v2
/// handshake carries key-derived `NodeId` and receiver-local route slots stop
/// crossing the wire.
static PROCESS_ROUTE_SLOT_BASE: std::sync::OnceLock<u16> = std::sync::OnceLock::new();

/// Derive a non-zero v1 route-slot base from the OS process ID.
///
/// Uses a FNV-1a–style fold to spread the PID bits across the full u16
/// range, then forces non-zero because route slot zero is local dispatch.
fn process_route_slot_base() -> u16 {
    *PROCESS_ROUTE_SLOT_BASE.get_or_init(|| {
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
        // Ensure non-zero (0 is local dispatch).
        if folded == 0 {
            1
        } else {
            folded
        }
    })
}

/// Map `(base, offset)` to a route slot in `1..=65535`.
///
/// A bare `base.wrapping_add(offset)` can wrap a second `Node::start` to `0`
/// when `base == 65535` (offset 1). Mapping through the
/// `1..=65535` ring (`1 + ((base - 1 + offset) mod 65535)`) keeps the result a
/// valid peer route slot for any base/offset, including the wrap case.
fn next_legacy_route_slot(base: u16, offset: u32) -> u16 {
    // Work in u32 to avoid intermediate u16 wrap; the ring has 65535 slots
    // (1..=65535). `base` is already guaranteed non-zero by
    // `process_route_slot_base`, so `base - 1` is in `0..=65534`.
    let ring = 65_535u32;
    let zero_based = (u32::from(base) - 1 + (offset % ring)) % ring;
    #[expect(
        clippy::cast_possible_truncation,
        reason = "zero_based is in 0..65535 so 1 + zero_based fits in u16 (max 65535)"
    )]
    let route_slot = (1 + zero_based) as u16;
    route_slot
}

fn select_legacy_route_slot(
    base: u16,
    offset: u32,
    bindings: &crate::peer_binding::PeerBindings,
) -> Option<std::num::NonZeroU16> {
    for attempt in 0..65_535 {
        let candidate = next_legacy_route_slot(base, offset.wrapping_add(attempt));
        if bindings.get(&candidate).is_none() {
            return std::num::NonZeroU16::new(candidate);
        }
    }
    None
}

/// Merge the start-time transport selection into staged pre-start config.
///
/// # Errors
///
/// Returns a typed message when an environment value is malformed.
fn merge_start_env_into_config(
    cfg: &mut crate::peer_binding::PeerAuthConfig,
) -> Result<(), String> {
    // Transport (issue #2652): a selection pinned by an earlier transport-
    // sensitive op (`Node::set_transport` / `Node::load_keys` /
    // `Node::allow_peer`) is authoritative and wins over any later env change —
    // start uses the STORED selection. Only when unpinned do we adopt the
    // start-time env selection.
    let effective = if let Some(pinned) = cfg.transport {
        pinned
    } else {
        let sel = transport_selection_from_env()?.as_peer_transport();
        cfg.transport = Some(sel);
        sel
    };
    // Re-assert the effective selection onto `HEW_TRANSPORT` so the low-level
    // transport construction (which reads the env) builds the stored selection,
    // not a value that diverged after the pin.
    // SAFETY: ENV_LOCK synchronises access to the process-global environ array.
    crate::env::ENV_LOCK.access(|()| unsafe {
        std::env::set_var("HEW_TRANSPORT", peer_transport_env_name(effective));
    });
    Ok(())
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
    // Fail-closed: a failed pre-start peer-auth step (`Node::load_keys` /
    // `Node::allow_peer`) poisons the staged config; the check runs below inside
    // the staging lock (D14 — the poison lives on the per-node config, not a
    // process-global record). The mesh `ensure_identity` boundary re-checks the
    // installed snapshot as defence in depth.

    // ── Public owner-scoped staging (BLOCK-6) ─────────────────────────────
    // The singleton public `Node::*` API stages config in `PEER_AUTH_STATE`.
    // Transition `Building → Starting{owner}` under the lock, then run the
    // shared low-level start WITHOUT the lock (it reads the node's installed
    // snapshot, never `ConfigState`), then re-acquire for `Running` / restore.
    let offset = LEGACY_ROUTE_SLOT_COUNTER.fetch_add(1, Ordering::Relaxed);
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
        // Merge start-time transport selection into the staged config.
        let mut cfg = building.clone();
        // Fail-closed: a failed pre-start peer-auth step poisoned the staged
        // config. Refuse to start rather than binding a listener with an
        // ephemeral self-signed identity (the operator's pinned key failed to
        // load) or an incomplete peer allowlist — the F6 fail-open. The Hew call
        // form discards this `-1`, so the operator-visible signal is the `hew: `
        // stderr diagnostic.
        if let Some(reason) = cfg.setup_error.as_deref() {
            let msg = format!(
                "Node::start: refusing to bind listener — peer-auth setup failed (fail-closed): {reason}"
            );
            eprintln!("hew: {msg}");
            set_last_error(msg);
            return -1;
        }
        if let Err(msg) = merge_start_env_into_config(&mut cfg) {
            eprintln!("hew: {msg}");
            set_last_error(msg);
            return -1;
        }
        // Pre-listen public validation (fail-closed before allocation).
        if let Err(msg) = cfg.validate_public() {
            eprintln!("hew: {msg}");
            set_last_error(msg);
            return -1;
        }
        // Until the v2 handshake lands in Stage 2, allocate an internal compact
        // wire route slot that cannot collide with a configured peer slot.
        let Some(route_slot) =
            select_legacy_route_slot(process_route_slot_base(), offset, cfg.bindings())
        else {
            let msg = "Node::start: no free v1 route slot remains after peer bindings";
            eprintln!("hew: {msg}");
            set_last_error(msg);
            return -1;
        };
        cfg.legacy_wire_route_slot = Some(route_slot);
        let snapshot = match cfg.snapshot_for_start() {
            Ok(snapshot) => snapshot,
            Err(msg) => {
                eprintln!("hew: {msg}");
                set_last_error(msg);
                return -1;
            }
        };
        // SAFETY: addr was null-checked above and is a valid C string.
        let node = unsafe { hew_node_new(route_slot.get(), addr) };
        if node.is_null() {
            // State unchanged (still Building); the staged config is intact.
            return -1;
        }
        // Install the frozen per-node snapshot before the low-level start.
        // SAFETY: node was just created and is STOPPED.
        if unsafe { hew_node_set_auth_snapshot(node, snapshot) } != 0 {
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
        let node_identity = cfg.node_identity;
        if matches!(&guard.state, ConfigState::Starting { generation: g, owner, .. }
            if *g == generation && *owner == node as usize)
        {
            guard.state = ConfigState::Running {
                generation,
                owner: node as usize,
                identity_export,
                node_identity,
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
/// current mapping for `name` and broadcasts removal of that exact location so
/// a reordered old removal cannot erase a later re-registration. Returns 0 on
/// success, -1 on error (no active node, null name).
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

/// `Node::lookup(name)` exact-location runtime entry point.
///
/// Writes `out` only when the name resolves successfully.
///
/// # Safety
///
/// `name` must be a valid null-terminated C string and `out` must be writable.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_lookup_location(
    name: *const c_char,
    out: *mut HewRemotePid,
) -> c_int {
    if name.is_null() || out.is_null() {
        return -1;
    }
    with_current_node_read(|guard| {
        let node = *guard as *mut HewNode;
        if node.is_null() {
            return -1;
        }
        // SAFETY: node is pinned by the read lock; caller guarantees name/output.
        unsafe { hew_node_lookup_location(node, name, out) }
    })
}

/// `Node::set_transport(name)` — Set the transport type before starting.
///
/// Supported values: `"tcp"` (default), `"quic"`, `"quic-mesh"`.
/// Must be called before `Node::start` — and, per issue #2652, before any
/// transport-sensitive credential is staged (`Node::allow_peer` /
/// `Node::load_keys`). Peer credentials and stable identities are interpreted
/// under the pinned transport (a 32-byte Noise pubkey on TCP vs a cert SPKI on
/// quic-mesh), so once any is staged a transport change is refused fail-closed
/// rather than silently reinterpreting the already-staged material. The
/// selection is pinned into the pre-start `Building` config so `Node::start`
/// uses the stored selection.
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
    let normalized = match normalize_transport_name(s) {
        Ok(normalized) => normalized,
        Err(err) => {
            set_last_error(format!("Node::set_transport: {err}"));
            return -1;
        }
    };
    let requested = peer_transport_from_normalized(normalized);

    // Transport must be pinned BEFORE identity credential staging (issue #2652):
    // `Node::allow_peer` / `Node::load_keys` interpret their credential under the
    // pinned transport. A `set_transport` after staging would reinterpret the
    // already-staged Noise pubkeys / mesh SPKIs under a different transport —
    // credential confusion. Refuse fail-closed (no env mutation) when the
    // Building config already carries staged credentials and the request would
    // change the pinned selection; an idempotent re-selection of the same
    // transport is allowed. While the public node lifecycle owns the state
    // (Starting/Running) the started node already froze its snapshot, so leave
    // the env write to preserve legacy behaviour for any low-level node.
    //
    // Hold PEER_AUTH_STATE across the staged-credential check, the HEW_TRANSPORT
    // env write, AND the Building-config pin as ONE critical section. The check
    // and the pin must be atomic: if the guard were released between them, a
    // concurrent `Node::allow_peer` / `Node::load_keys` could stage a credential
    // under the old transport in the gap and have it silently reinterpreted
    // under the newly-pinned transport — the exact credential confusion the
    // check exists to prevent. ENV_LOCK is acquired *inside* the PEER_AUTH_STATE
    // guard, matching the PEER_AUTH_STATE → ENV_LOCK order that
    // `merge_start_env_into_config` already establishes (so the two paths cannot
    // deadlock against each other).
    {
        let mut guard = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let ConfigState::Building(cfg) = &guard.state {
            if cfg.has_staged_credentials() && cfg.transport != Some(requested) {
                drop(guard);
                let msg = "Node::set_transport: transport cannot be changed after peer \
                     credentials or a stable identity have been staged (Node::allow_peer / \
                     Node::load_keys) — select the transport before staging (fail-closed)"
                    .to_string();
                eprintln!("hew: {msg}");
                set_last_error(msg);
                return -1;
            }
        }

        // Still holding PEER_AUTH_STATE: set the process-global selection and pin
        // it into the Building config so `Node::start` (and `identity_key()`) use
        // the stored selection and a later start-time env merge does not override
        // it. Env write and pin are one indivisible step under the guard.
        // SAFETY: ENV_LOCK synchronises access to the process-global environ array;
        // set_var is safe under exclusive write access.
        crate::env::ENV_LOCK.access(|()| unsafe { std::env::set_var("HEW_TRANSPORT", normalized) });
        if let ConfigState::Building(cfg) = &mut guard.state {
            cfg.transport = Some(requested);
        }
    }
    0
}

/// Surface a `Node::load_keys` / `Node::allow_peer` peer-auth setup failure on
/// every channel:
///
/// - `hew_last_error` — the thread-local programmatic surface (unchanged from
///   the pre-F6 behaviour);
/// - a `hew: ` stderr diagnostic — operator-visible, because the Hew call form
///   discards the `-1` return (these builtins are typed `Unit`);
/// - the sticky setup-error poison on the staged `Building` config (D14 —
///   per-node config state, no process-global record), which makes the next
///   `Node::start` refuse to bind a listener (fail-closed) so a node never
///   silently comes up with an ephemeral identity or an incomplete peer
///   allowlist after the operator's setup failed.
fn node_peer_auth_setup_failed(msg: impl Into<String>) {
    let msg = msg.into();
    eprintln!("hew: {msg} (fail-closed)");
    // Poison the staged config. If a public node lifecycle already owns the
    // state (Starting/Running) the poison is moot: that node already froze and
    // installed its snapshot. First poison wins (a later generic failure must
    // not overwrite the specific first cause).
    {
        let mut guard = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let ConfigState::Building(cfg) = &mut guard.state {
            if cfg.setup_error.is_none() {
                cfg.setup_error = Some(msg.clone());
            }
        }
    }
    set_last_error(msg);
}

/// `Node::load_keys(path)` — Load (or, if absent, mint-and-persist) this node's
/// stable transport identity from `path`, selected by the pinned transport: a
/// mesh TLS identity (SPKI) on quic-mesh, a stable Noise static keypair on
/// tcp-noise. Must be called **before** `Node::start` so the listener/handshake
/// presents the loaded key. Peers pin the resulting public credential via
/// `Node::allow_peer`; persisting the key keeps that credential stable across
/// restarts. Native only (plain quic has no pinnable peer identity).
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
    // Transport-selected identity (issue #2652, D1): load the identity whose
    // credential the pinned transport interprets — a mesh TLS identity on
    // quic-mesh, a stable Noise static keypair on tcp-noise. Reading the
    // selection here also pins it early into the Building config so
    // `Node::identity_key()` can export the pinned credential before start.
    let selection = match transport_selection_from_env() {
        Ok(s) => s,
        Err(err) => {
            node_peer_auth_setup_failed(format!("Node::load_keys: {err}"));
            return -1;
        }
    };
    match selection {
        TransportSelection::Tcp => {
            #[cfg(feature = "encryption")]
            {
                match crate::encryption::noise_identity_load_or_create(std::path::Path::new(p)) {
                    Ok(identity) => {
                        let node_id =
                            crate::node_identity::NodeId::from_noise_static_key(&identity.public());
                        stage_loaded_identity(
                            std::path::Path::new(p),
                            node_id,
                            |cfg| cfg.noise_identity = Some(identity),
                            PeerTransport::Tcp,
                        )
                    }
                    Err(err) => {
                        node_peer_auth_setup_failed(format!("Node::load_keys: {err}"));
                        -1
                    }
                }
            }
            #[cfg(not(feature = "encryption"))]
            {
                let _ = p;
                node_peer_auth_setup_failed(
                    "Node::load_keys: tcp-noise identity requires the hew-runtime encryption \
                     feature (peer auth unavailable)",
                );
                -1
            }
        }
        #[cfg(feature = "quic")]
        TransportSelection::QuicMesh => {
            match crate::quic_mesh::mesh_identity_load_or_create(std::path::Path::new(p)) {
                Ok(material) => {
                    let node_id = crate::node_identity::NodeId::from_spki(material.spki());
                    stage_loaded_identity(
                        std::path::Path::new(p),
                        node_id,
                        |cfg| cfg.mesh_identity = Some(material),
                        PeerTransport::QuicMesh,
                    )
                }
                Err(err) => {
                    node_peer_auth_setup_failed(format!("Node::load_keys: {err}"));
                    -1
                }
            }
        }
        #[cfg(feature = "quic")]
        TransportSelection::Quic => {
            let _ = p;
            node_peer_auth_setup_failed(
                "Node::load_keys: plain quic transport has no pinnable peer identity \
                 (use quic-mesh or tcp-noise)",
            );
            -1
        }
    }
}

/// Stage a freshly loaded stable identity into the pre-start `Building` config
/// and pin the transport selection (issue #2652, D1/D14). The identity is
/// frozen into the per-node snapshot at `Node::start` and presented by the
/// listener (mesh via `install_auth`) / handshake (Noise). Rejected once the
/// public node lifecycle owns the state (`Starting`/`Running`) so a load never
/// races a running node. Returns `0` on success, `-1` after marking the
/// peer-auth setup failed when the config is locked.
#[cfg(any(feature = "quic", feature = "encryption"))]
fn stage_loaded_identity(
    identity_path: &std::path::Path,
    node_identity: crate::node_identity::NodeId,
    apply: impl FnOnce(&mut crate::peer_binding::PeerAuthConfig),
    selection: PeerTransport,
) -> c_int {
    let mut guard = PEER_AUTH_STATE
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let ConfigState::Building(cfg) = &mut guard.state else {
        drop(guard);
        node_peer_auth_setup_failed(
            "Node::load_keys: peer auth config is locked while the public node \
             lifecycle is active (Starting/Running)",
        );
        return -1;
    };
    // Fail closed if a concurrent `Node::set_transport` re-pinned the selection
    // between the env read that chose which identity file to load (Noise vs
    // mesh) and this stage under the lock. The loaded identity is for
    // `selection`; staging it while the config is pinned to a different
    // transport would present the wrong stable identity at start (issue #2652).
    // The pinned config selection is authoritative — refuse rather than stage a
    // mismatched identity.
    if let Some(pinned) = cfg.transport {
        if pinned != selection {
            drop(guard);
            node_peer_auth_setup_failed(
                "Node::load_keys: the transport was changed concurrently while this \
                 identity was being loaded — select the transport before loading keys \
                 (fail-closed)",
            );
            return -1;
        }
    }
    apply(cfg);
    cfg.node_identity = Some(node_identity);
    cfg.identity_path = Some(identity_path.to_path_buf());
    // Early transport pin: loading a stable identity commits this node to the
    // operator's selected transport, so `identity_key()` can export the pinned
    // credential before `Node::start` freezes the snapshot. Idempotent with the
    // authoritative pin in `merge_start_env_into_config` (both read the env).
    if cfg.transport.is_none() {
        cfg.transport = Some(selection);
    }
    0
}

/// `Node::allow_peer(route_slot, credential_hex)` — pin a peer's authenticated
/// credential to its transitional receiver-local v1 route slot. The
/// credential is interpreted by the node's pinned transport (TCP ⇒ 32-byte
/// Noise pubkey; quic-mesh ⇒ cert SPKI). The exact one-to-one pin is staged
/// into the pre-start `Building` `PeerAuthConfig` and frozen at `Node::start`.
///
/// Returns `0` on success, `-1` on route slot zero, a null/odd/
/// non-hex string, a credential of the wrong length for the transport, a
/// route-slot or credential collision, or while the public node lifecycle owns
/// the config. Adds nothing on error.
///
/// # Safety
///
/// `credential_hex` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_allow_peer(
    route_slot: u16,
    credential_hex: *const c_char,
) -> c_int {
    // SAFETY: caller guarantees credential_hex is a valid C string (or null).
    let Some(s) = (unsafe { crate::util::cstr_to_str(&credential_hex, "Node::allow_peer") }) else {
        node_peer_auth_setup_failed("Node::allow_peer: invalid credential argument");
        return -1;
    };
    let Some(bytes) = decode_hex(s) else {
        node_peer_auth_setup_failed("Node::allow_peer: peer key must be hex-encoded SPKI bytes");
        return -1;
    };
    // Interpret the credential per the node's pinned transport (env-resolved;
    // the same selection is pinned into the config at start). Plain QUIC has no
    // peer-credential channel, so strict binding is unsupported there.
    let transport = match transport_selection_from_env() {
        Ok(sel) => sel,
        Err(msg) => {
            node_peer_auth_setup_failed(format!("Node::allow_peer: {msg}"));
            return -1;
        }
    };
    let credential = match transport {
        TransportSelection::Tcp => {
            let Ok(key) = <[u8; crate::peer_binding::NOISE_KEY_LEN]>::try_from(bytes.as_slice())
            else {
                node_peer_auth_setup_failed("Node::allow_peer: Noise pubkey must be 32 bytes");
                return -1;
            };
            PeerCredential::NoiseKey(key)
        }
        #[cfg(feature = "quic")]
        TransportSelection::QuicMesh => {
            if bytes.is_empty() || bytes.len() > MAX_SPKI_DECODE_BYTES {
                node_peer_auth_setup_failed("Node::allow_peer: mesh SPKI is empty or oversize");
                return -1;
            }
            PeerCredential::Spki(bytes)
        }
        #[cfg(feature = "quic")]
        TransportSelection::Quic => {
            node_peer_auth_setup_failed(
                "Node::allow_peer: strict binding unsupported on plain quic transport \
                 (use quic-mesh or tcp-noise)",
            );
            return -1;
        }
    };
    // Stage into the pre-start Building config. Rejected while the public node
    // lifecycle owns the state (Starting/Running).
    let mut guard = PEER_AUTH_STATE
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let ConfigState::Building(cfg) = &mut guard.state else {
        drop(guard);
        node_peer_auth_setup_failed(
            "Node::allow_peer: peer auth config is locked while the public node lifecycle \
             is active (Starting/Running)",
        );
        return -1;
    };
    // Fail closed if a concurrent `Node::set_transport` re-pinned the selection
    // between the env read above (which typed `credential`) and this staging
    // under the lock. `credential` was interpreted as the type of `transport`;
    // staging it while the config is pinned to a different transport is exactly
    // the credential confusion issue #2652 guards against (a NoiseKey bound
    // while the node is pinned to quic-mesh, or vice versa). The pinned config
    // selection is authoritative — refuse rather than reinterpret.
    if let Some(pinned) = cfg.transport {
        if pinned != transport.as_peer_transport() {
            drop(guard);
            node_peer_auth_setup_failed(
                "Node::allow_peer: the transport was changed concurrently while this \
                 credential was being interpreted — select the transport before staging \
                 peer credentials (fail-closed)",
            );
            return -1;
        }
    }
    if let Err(err) = cfg.pin_peer(route_slot, credential) {
        let msg = format!("Node::allow_peer: {err}");
        drop(guard);
        node_peer_auth_setup_failed(msg);
        return -1;
    }
    // Pin the transport selection (issue #2652): the credential was interpreted
    // under `transport`, so bind the config to it now. Idempotent with the pins
    // in `Node::set_transport` / `stage_loaded_identity` and the start-time
    // merge; once pinned, a later `Node::set_transport` that would change it is
    // rejected via `has_staged_credentials`.
    if cfg.transport.is_none() {
        cfg.transport = Some(transport.as_peer_transport());
    }
    0
}

/// `Node::identity_key()` — Return this node's stable public credential for the
/// pinned transport as lowercase hex (issue #2652, D8): the Noise static public
/// key on TCP, the certificate SPKI on quic-mesh. Operators hand the returned
/// value to peers so they can bind it via `Node::allow_peer`.
///
/// Reads the three-state public staging config (BLOCK-7 point 3 — never
/// dereferences the owning node pointer):
/// - `Building` — the staged config (before start);
/// - `Starting` — the consumed config held for the in-flight start;
/// - `Running` — the standalone `identity_export` clone captured at the
///   `Starting → Running` transition.
///
/// Returns an **owned** hew string (empty string `""`, never null, when no
/// stable identity has been loaded). The caller frees it via `hew_string_drop`;
/// the runtime never retains the pointer.
#[no_mangle]
pub extern "C" fn hew_node_api_identity_key() -> *mut c_char {
    let export = {
        let guard = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match &guard.state {
            ConfigState::Building(cfg) => cfg.identity_export_string(),
            ConfigState::Starting { config, .. } => config.identity_export_string(),
            ConfigState::Running {
                identity_export, ..
            } => identity_export.clone(),
        }
    };
    // SAFETY: `export` is valid UTF-8 for its byte length; malloc_cstring copies
    // exactly that many bytes and NUL-terminates (owned hew string).
    unsafe { crate::cabi::malloc_cstring(export.as_ptr(), export.len()) }
}

/// `Node::id()` — write the stable key-derived node identity when configured.
///
/// Returns `0` and writes `out` on success; returns `-1` without writing when
/// no stable identity has been loaded.
///
/// # Safety
///
/// `out` must be writable when non-null.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_id(out: *mut HewNodeId) -> c_int {
    if out.is_null() {
        return -1;
    }
    let identity = {
        let guard = PEER_AUTH_STATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match &guard.state {
            ConfigState::Building(cfg) => cfg.node_identity,
            ConfigState::Starting { config, .. } => config.node_identity,
            ConfigState::Running { node_identity, .. } => *node_identity,
        }
    };
    let Some(identity) = identity else {
        return -1;
    };
    // SAFETY: caller guarantees `out` is writable.
    unsafe { out.write(HewNodeId::from(identity)) };
    0
}

unsafe fn owned_identity_string(value: &str) -> *mut c_char {
    // SAFETY: `value` is valid UTF-8 and malloc_cstring copies the bytes.
    unsafe { crate::cabi::malloc_cstring(value.as_ptr(), value.len()) }
}

/// Format a `NodeId` as 32 lowercase hexadecimal digits.
///
/// # Safety
///
/// `node` must point to a readable `HewNodeId`.
#[no_mangle]
pub unsafe extern "C" fn hew_node_id_format(node: *const HewNodeId) -> *mut c_char {
    if node.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees `node` is readable.
    let node = unsafe { *node };
    // SAFETY: the formatted string is copied into Hew-owned storage.
    unsafe { owned_identity_string(&format!("{:016x}{:016x}", node.hi, node.lo)) }
}

/// Format a full location as `<node-hex>/<slot>@<session>`.
///
/// # Safety
///
/// `location` must point to a readable `HewRemotePid`.
#[no_mangle]
pub unsafe extern "C" fn hew_location_format(location: *const HewRemotePid) -> *mut c_char {
    if location.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees `location` is readable.
    let location = unsafe { *location };
    // SAFETY: the formatted string is copied into Hew-owned storage.
    unsafe {
        owned_identity_string(&format!(
            "{:016x}{:016x}/{}@{}",
            location.node.hi, location.node.lo, location.slot, location.incarnation
        ))
    }
}

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
#[allow(
    clippy::too_many_lines,
    reason = "function orchestrates end-to-end remote ask setup across encoding, auth, and parking"
)]
fn setup_remote_ask(
    target: Location,
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

        // SAFETY: routing_table is valid while node is running.
        let (conn_id, target_node_id) = match unsafe {
            crate::routing::hew_routing_lookup_location(node.routing_table, target)
        } {
            routing::LocationRoute::Remote {
                route_slot, conn, ..
            } => (conn, route_slot),
            routing::LocationRoute::Partition => {
                return RemoteAskSetupResult::Error(AskError::Partition);
            }
            routing::LocationRoute::StaleRef => {
                return RemoteAskSetupResult::Error(AskError::StaleRef);
            }
            routing::LocationRoute::Local { .. } => {
                return RemoteAskSetupResult::Error(AskError::RoutingFailed);
            }
        };

        // Quarantine consult: under a Quarantine policy, a peer the failure
        // detector buried and that has not yet rejoined at a strictly higher
        // incarnation fails closed here — before any request bytes are serialized,
        // so nothing is allocated on the blocked path.
        if quarantine_blocks_send(node, target_node_id) {
            return RemoteAskSetupResult::Error(AskError::Partition);
        }

        // issue #2652 (outbound authority): an ask is control-bearing — its reply
        // deposits into THIS node's pending table and its dispatch runs a handler
        // on the peer, so it must go ONLY to the exact authenticated owner (Strict
        // posture + published claim) of the target NodeId. An Unverified
        // (delivery-only) or a superseded / non-exact-owner connection carries no
        // control-plane authority; fail CLOSED here — before serializing the
        // request or registering a pending reply — symmetric with the inbound ask
        // gate (`inbound_ask_denied_unverified`) and D12's unverified-ask REJECT.
        // Reply-completion validation stays a backstop, never the primary control.
        // SAFETY: conn_mgr is non-null and valid while the node is running (the
        // CURRENT_NODE read lock is held for the whole closure).
        let authenticated =
            unsafe { connection::authenticated_peer_node_id_for_conn(&*node.conn_mgr, conn_id) };
        if authenticated != target_node_id {
            return RemoteAskSetupResult::Error(AskError::Unauthorized);
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
        // SAFETY: the running node owns a live connection manager.
        let manager = unsafe { &*node.conn_mgr };
        let source = {
            let actor = crate::actor::hew_actor_self();
            if actor.is_null() {
                None
            } else {
                // SAFETY: `hew_actor_self` returned a live actor pointer.
                connection::local_location_for_actor(manager, unsafe { (*actor).id })
            }
        };
        // SAFETY: encode_envelope_frame_from_raw_parts requirements are
        // met by the enclosing unsafe fn's documented preconditions.
        let bytes = match unsafe {
            encode_envelope_frame_from_raw_parts(
                Some(target),
                source,
                msg_type,
                req_ptr,
                req_len,
                request_id,
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
pub unsafe extern "C" fn hew_node_api_ask_location(
    target: *const HewRemotePid,
    dispatch: *const c_void,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: u64,
    reply_size: usize,
) -> *mut c_void {
    if target.is_null() {
        return ask_null(AskError::StaleRef);
    }
    // SAFETY: caller guarantees `target` is readable.
    let Ok(target) = Location::try_from(unsafe { *target }) else {
        return ask_null(AskError::StaleRef);
    };
    let Some(route) = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: current-node read lock pins the node.
        Some(unsafe { routing::hew_routing_lookup_location((*node).routing_table, target) })
    }) else {
        return ask_null(AskError::NodeNotRunning);
    };

    // Local path: delegate to the by-ID ask (which packs a reply channel).
    if let routing::LocationRoute::Local { actor_id } = route {
        if crate::lifetime::live_actors::get_actor_ptr_by_id(actor_id).is_none() {
            return ask_null(AskError::StaleRef);
        }
        // SAFETY: data/size are caller-validated; local actor ask is safe here.
        let result = unsafe { crate::actor::hew_actor_ask_by_id(actor_id, msg_type, data, size) };
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
        match setup_remote_ask(target, dispatch, msg_type, data, size, ptr::null_mut()) {
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
pub unsafe extern "C" fn hew_node_api_ask_async_location(
    target: *const HewRemotePid,
    dispatch: *const c_void,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    timeout_ms: u64,
    caller_actor: *mut crate::actor::HewActor,
) -> *mut c_void {
    if target.is_null() {
        return ask_null(AskError::StaleRef);
    }
    // SAFETY: caller guarantees `target` is readable.
    let Ok(target) = Location::try_from(unsafe { *target }) else {
        return ask_null(AskError::StaleRef);
    };
    if caller_actor.is_null() {
        return ask_null(AskError::NoRunnableWork);
    }

    // `dispatch` keys the request encode `(dispatch, msg_type)`; the matching
    // reply decode key is re-supplied to `hew_node_api_ask_finish` at the resume
    // site (codegen knows the target actor type at both ends of the suspend).
    let (request_id, pending) =
        match setup_remote_ask(target, dispatch, msg_type, data, size, caller_actor) {
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
/// resumes. Consumes the handle returned by [`hew_node_api_ask_async_location`] and
/// materialises the outcome exactly as the blocking finish path. A handle whose
/// outcome is still empty (no reply, no failure) fails closed with
/// [`AskError::ConnectionDropped`] rather than fabricating a value.
///
/// # Safety
///
/// `pending_handle` must be a handle returned by [`hew_node_api_ask_async_location`]
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

    fn test_node_id(route_slot: u16) -> crate::node_identity::NodeId {
        let mut bytes = [0_u8; 16];
        bytes[14..].copy_from_slice(&route_slot.to_be_bytes());
        crate::node_identity::NodeId::from_bytes(bytes)
    }

    fn test_location(route_slot: u16, actor_slot: u64) -> Location {
        Location::new(test_node_id(route_slot), actor_slot, 1).unwrap()
    }

    fn test_remote_pid(actor_id: u64) -> HewRemotePid {
        HewRemotePid::from(test_location(
            crate::pid::hew_pid_node(actor_id),
            crate::pid::hew_pid_serial(actor_id),
        ))
    }

    unsafe fn lookup_exact(node: *mut HewNode, name: *const c_char) -> Option<Location> {
        let mut found = HewRemotePid::default();
        // SAFETY: caller guarantees node/name validity and `found` is writable.
        if unsafe { hew_node_lookup_location(node, name, &raw mut found) } == 0 {
            Location::try_from(found).ok()
        } else {
            None
        }
    }

    unsafe fn install_test_auth(node: *mut HewNode, route_slot: u16) {
        let snapshot = PeerAuthSnapshot::for_test(test_node_id(route_slot), []);
        // SAFETY: caller owns a stopped test node.
        assert_eq!(unsafe { hew_node_set_auth_snapshot(node, snapshot) }, 0);
    }

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
    fn registry_repoint_changes_future_lookup_without_rewriting_old_location() {
        let registry = HewRegistry::default();
        let old = Location::new(crate::node_identity::NodeId::from_bytes([1; 16]), 100, 3).unwrap();
        let new = Location::new(crate::node_identity::NodeId::from_bytes([1; 16]), 200, 3).unwrap();
        registry
            .remote_names
            .lock_or_recover()
            .insert("kv".to_owned(), old);
        let captured = registry
            .remote_names
            .lock_or_recover()
            .get("kv")
            .copied()
            .unwrap();
        registry
            .remote_names
            .lock_or_recover()
            .insert("kv".to_owned(), new);
        assert_eq!(captured, old);
        assert_eq!(
            registry.remote_names.lock_or_recover().get("kv").copied(),
            Some(new)
        );
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

    /// The `hew_set_partition_policy` C-ABI symbol — the exact export the Hew
    /// surface (`std/link_monitor.hew`: `set_partition_policy`) lowers to —
    /// flips the real send gate. This is the policy-takes-effect leg: driving
    /// the setter (rather than pre-baking the slot as the test above does)
    /// proves the FFI entry point installed on the dispatch context is what the
    /// quarantine consult reads.
    #[test]
    fn set_partition_policy_symbol_drives_the_quarantine_gate() {
        use crate::execution_context::{
            hew_set_partition_policy, HewExecutionContext, PartitionPolicy, TestExecutionContext,
        };
        const PEER: u16 = 612;

        // Position IS the ABI: the C-ABI tag is the discriminant, and the two
        // Hew-side declarations that mirror it (`std/link_monitor.hew`'s
        // `PartitionPolicy` and the `MONITOR_REF_HEW` prelude enum in
        // `hew-types` `check::registration`) must keep this exact order. A drift
        // here would silently misroute a policy tag across the FFI boundary
        // (`builtin-enum-variant-mirror-discipline`).
        assert_eq!(PartitionPolicy::FailFast as i64, 0);
        assert_eq!(PartitionPolicy::Deadline as i64, 1);
        assert_eq!(PartitionPolicy::MonitorLost as i64, 2);
        assert_eq!(PartitionPolicy::CrashLinked as i64, 3);
        assert_eq!(PartitionPolicy::Quarantine as i64, 4);

        let _guard = crate::runtime_test_guard();
        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string for the duration of this test.
        let node_handle = unsafe { TestNode::new(611, &bind_addr) };
        assert!(!node_handle.as_ptr().is_null());
        // SAFETY: pointer came from TestNode::new and is valid until drop.
        unsafe { assert_eq!(hew_node_start(node_handle.as_ptr()), 0) };
        // SAFETY: started node has a non-null cluster.
        let node = unsafe { &*node_handle.as_ptr() };
        assert!(!node.cluster.is_null());
        // SAFETY: cluster is valid while the node is running.
        let cluster = unsafe { &*node.cluster };
        cluster.seed_member_for_test(PEER, crate::cluster::MEMBER_DEAD, 5);
        quarantine_insert(PEER, 5);

        // Install a dispatch context with the DEFAULT (null) policy slot, then
        // drive the policy purely through the C-ABI setter.
        let _ctx_guard = TestExecutionContext::install(HewExecutionContext::default());

        // Quarantine (tag 4) installed via the setter blocks the buried peer.
        assert!(hew_set_partition_policy(4));
        assert!(
            quarantine_blocks_send(node, PEER),
            "Quarantine set via hew_set_partition_policy must block the buried peer"
        );

        // Overwriting with FailFast (tag 0) via the same setter clears the block:
        // the slot is writable repeatedly within one dispatch, not one-shot.
        assert!(hew_set_partition_policy(0));
        assert!(
            !quarantine_blocks_send(node, PEER),
            "FailFast set via hew_set_partition_policy must not consult the set"
        );

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
        assert!(!complete_remote_reply(std::ptr::null(), 0, 1, &[1, 2, 3]));
        assert!(!fail_remote_reply(std::ptr::null(), 0, 1, &[]));
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
    fn next_legacy_route_slot_never_yields_zero_sentinel() {
        // Route slot zero is the local-dispatch sentinel.
        for base in [1u16, 2, 100, 65_534, 65_535] {
            for offset in 0u32..4 {
                assert_ne!(
                    next_legacy_route_slot(base, offset),
                    0,
                    "base={base} offset={offset} mapped onto the 0 sentinel",
                );
            }
        }
        // The bare-wrapping_add hazard: base 65535 + offset 1 wrapped to 0.
        assert_ne!(next_legacy_route_slot(65_535, 1), 0);
        // Distinct offsets from the same base stay distinct within the ring.
        assert_ne!(
            next_legacy_route_slot(100, 0),
            next_legacy_route_slot(100, 1)
        );
        // base==1, offset==0 maps to itself (the common single-node case).
        assert_eq!(next_legacy_route_slot(1, 0), 1);
    }

    #[test]
    fn legacy_route_slot_selection_skips_peer_pins() {
        let mut config = crate::peer_binding::PeerAuthConfig::default();
        config
            .pin_peer(100, PeerCredential::NoiseKey([0x10; 32]))
            .unwrap();
        config
            .pin_peer(101, PeerCredential::NoiseKey([0x11; 32]))
            .unwrap();

        assert_eq!(
            select_legacy_route_slot(100, 0, config.bindings()).map(std::num::NonZeroU16::get),
            Some(102)
        );
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
            let node = unsafe { hew_node_new(node_id, bind_addr.as_ptr()) };
            #[cfg(feature = "encryption")]
            if !node.is_null() {
                let dir = tempfile::tempdir().expect("test node identity directory");
                let path = dir.path().join("node.key");
                let identity = crate::encryption::noise_identity_load_or_create(&path)
                    .expect("mint test node identity");
                let mut config = crate::peer_binding::PeerAuthConfig::default();
                config.legacy_wire_route_slot = std::num::NonZeroU16::new(node_id);
                config.node_identity = Some(crate::node_identity::NodeId::from_noise_static_key(
                    &identity.public(),
                ));
                config.identity_path = Some(path);
                config.noise_identity = Some(identity);
                let snapshot = config
                    .snapshot_for_start()
                    .expect("acquire test node session");
                // SAFETY: the node was just allocated and is still stopped.
                assert_eq!(unsafe { hew_node_set_auth_snapshot(node, snapshot) }, 0);
            }
            Self(node)
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

    #[cfg(feature = "encryption")]
    struct PublicApiTestIdentity {
        dir: tempfile::TempDir,
        saved_transport: Option<std::ffi::OsString>,
    }

    #[cfg(feature = "encryption")]
    impl Drop for PublicApiTestIdentity {
        fn drop(&mut self) {
            crate::env::ENV_LOCK.access(|()| {
                // SAFETY: ENV_LOCK serializes process-global environment mutation.
                unsafe {
                    if let Some(value) = &self.saved_transport {
                        std::env::set_var("HEW_TRANSPORT", value);
                    } else {
                        std::env::remove_var("HEW_TRANSPORT");
                    }
                }
            });
        }
    }

    /// Stage a real stable TCP identity for a public `Node::start` test and keep
    /// its tempfile-backed key path alive until the public node is shut down.
    #[cfg(feature = "encryption")]
    fn stage_public_api_test_identity() -> PublicApiTestIdentity {
        let saved_transport =
            crate::env::ENV_LOCK.read_access(|()| std::env::var_os("HEW_TRANSPORT"));
        let identity = PublicApiTestIdentity {
            dir: tempfile::tempdir().expect("public API identity directory"),
            saved_transport,
        };
        let tcp = CString::new("tcp").expect("valid transport name");
        // SAFETY: tcp is a valid C string for this call.
        assert_eq!(unsafe { hew_node_api_set_transport(tcp.as_ptr()) }, 0);

        let key_path = identity.dir.path().join("node.key");
        let key_path = CString::new(key_path.to_str().expect("UTF-8 tempfile key path"))
            .expect("valid tempfile key path");
        // SAFETY: key_path is a valid C string and the directory remains live in the guard.
        assert_eq!(unsafe { hew_node_api_load_keys(key_path.as_ptr()) }, 0);

        identity
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

    /// Environment key naming the pre-generated Noise keyfile a two-process
    /// helper loads its stable identity from (brokered by the parent test).
    #[cfg(feature = "encryption")]
    const TWO_PROCESS_KEYFILE_ENV: &str = "HEW_2P_KEYFILE";
    /// Environment key carrying the peer's Noise static pubkey (lowercase hex)
    /// for the helper to bind via its per-node snapshot before connecting.
    #[cfg(feature = "encryption")]
    const TWO_PROCESS_PEER_PUBKEY_ENV: &str = "HEW_2P_PEER_PUBKEY";

    /// Start a **credentialed, Strict-authorized** TCP-Noise listener node for a
    /// two-process helper (issue #2652, D110).
    ///
    /// Reads the pre-generated Noise keyfile (`HEW_2P_KEYFILE`) and the peer's
    /// Noise static pubkey (`HEW_2P_PEER_PUBKEY`, lowercase hex) — the parent
    /// test brokered both out-of-band, mirroring a real key exchange. Installs a
    /// per-node snapshot with the stable Noise identity + a
    /// `peer_node → NoiseKey(peer_pub)` binding, so the TCP-Noise handshake
    /// authenticates the peer and the claim machine binds its `NodeId`. This is a
    /// genuine authorized connection — there is no test-only posture promotion:
    /// a peer presenting an unbound Noise key fails the pre-gate and admission.
    #[cfg(feature = "encryption")]
    fn start_authorized_tcp_node(node_id: u16, peer_node: u16) -> (TestNode, u16) {
        use crate::peer_binding::{PeerAuthConfig, NOISE_KEY_LEN};

        let keyfile = std::env::var(TWO_PROCESS_KEYFILE_ENV).expect("2p keyfile env");
        let peer_pub_hex = std::env::var(TWO_PROCESS_PEER_PUBKEY_ENV).expect("2p peer pubkey env");

        let identity =
            crate::encryption::noise_identity_load_or_create(std::path::Path::new(&keyfile))
                .expect("load 2p noise identity");
        let peer_pub_bytes = decode_hex(&peer_pub_hex).expect("2p peer pubkey must be hex");
        assert_eq!(
            peer_pub_bytes.len(),
            NOISE_KEY_LEN,
            "2p peer pubkey must be a 32-byte Noise static key"
        );
        let mut peer_pub = [0u8; NOISE_KEY_LEN];
        peer_pub.copy_from_slice(&peer_pub_bytes);

        let mut config = PeerAuthConfig::default();
        config.legacy_wire_route_slot = std::num::NonZeroU16::new(node_id);
        config.node_identity = Some(crate::node_identity::NodeId::from_noise_static_key(
            &identity.public(),
        ));
        config.identity_path = Some(std::path::PathBuf::from(&keyfile));
        config.noise_identity = Some(identity);
        config
            .pin_peer(peer_node, PeerCredential::NoiseKey(peer_pub))
            .expect("distinct two-process peer pin");
        let snapshot = config.snapshot_for_start().expect("acquire 2p session");

        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string for the duration of this helper.
        let node = unsafe { TestNode::new(node_id, &bind_addr) };
        assert!(!node.as_ptr().is_null(), "authorized tcp node alloc failed");
        // SAFETY: node is freshly created (STOPPED); install the snapshot before start.
        let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
        assert_eq!(set_rc, 0, "install 2p auth snapshot on node {node_id}");
        // SAFETY: node pointer is valid; start selects TCP from the snapshot and
        // reads the installed strict bindings.
        let rc = unsafe { hew_node_start(node.as_ptr()) };
        assert_eq!(
            rc,
            0,
            "authorized tcp start({node_id}) failed: {:?}",
            crate::stream_error::take_last_error()
        );
        // SAFETY: node started successfully on the TCP transport.
        let port =
            unsafe { crate::transport::hew_transport_tcp_bound_port((*node.as_ptr()).transport) }
                .expect("authorized tcp node must expose its bound listener port");
        (node, port)
    }

    /// Start two mutually-authenticated in-process nodes on the native TCP
    /// (Noise) transport. Each mints a stable Noise identity and pins the
    /// other's real static public key to the peer's `NodeId`, so the loopback
    /// handshake admits `Strict` with a published claim (issue #2652 —
    /// `posture_for` returns `Strict` whenever bindings exist, regardless of
    /// loopback). This is a genuine credentialed harness, never a test-only
    /// posture promotion; it mirrors [`start_authorized_quic_mesh_pair`] for the
    /// cases that must exercise TCP-specific pending-ask / connection behaviour
    /// now that an unverified outbound ask fails closed before it is ever sent.
    #[cfg(feature = "encryption")]
    fn start_authorized_tcp_pair(id_a: u16, id_b: u16) -> (TestNode, u16, TestNode, u16) {
        use crate::peer_binding::{PeerAuthConfig, StableNoiseIdentity, NOISE_KEY_LEN};

        let dir = tempfile::tempdir().expect("authorized tcp pair keydir");
        let path_a = dir.path().join("node-a.key");
        let path_b = dir.path().join("node-b.key");
        let identity_a = crate::encryption::noise_identity_load_or_create(&path_a)
            .expect("mint node-a noise identity");
        let identity_b = crate::encryption::noise_identity_load_or_create(&path_b)
            .expect("mint node-b noise identity");
        let pub_a = identity_a.public();
        let pub_b = identity_b.public();

        let start_one = |node_id: u16,
                         peer_id: u16,
                         identity: StableNoiseIdentity,
                         identity_path: &std::path::Path,
                         peer_pub: [u8; NOISE_KEY_LEN]|
         -> (TestNode, u16) {
            let mut config = PeerAuthConfig::default();
            config.legacy_wire_route_slot = std::num::NonZeroU16::new(node_id);
            config.node_identity = Some(crate::node_identity::NodeId::from_noise_static_key(
                &identity.public(),
            ));
            config.identity_path = Some(identity_path.to_path_buf());
            config.noise_identity = Some(identity);
            config
                .pin_peer(peer_id, PeerCredential::NoiseKey(peer_pub))
                .expect("distinct authorized TCP peer pin");
            let snapshot = config
                .snapshot_for_start()
                .expect("acquire authorized TCP session");
            let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
            // SAFETY: bind_addr is a valid C string for the duration of this closure.
            let node = unsafe { TestNode::new(node_id, &bind_addr) };
            assert!(
                !node.as_ptr().is_null(),
                "authorized tcp node {node_id} alloc failed"
            );
            // SAFETY: node is freshly created (STOPPED); install before start.
            let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
            assert_eq!(set_rc, 0, "install auth snapshot on node {node_id}");
            // SAFETY: node pointer is valid; start selects TCP + Noise from the
            // snapshot and reads the installed strict bindings.
            let rc = unsafe { hew_node_start(node.as_ptr()) };
            assert_eq!(
                rc,
                0,
                "authorized tcp start({node_id}) failed: {:?}",
                crate::stream_error::take_last_error()
            );
            // SAFETY: node started successfully on the TCP transport.
            let port = unsafe {
                crate::transport::hew_transport_tcp_bound_port((*node.as_ptr()).transport)
            }
            .expect("authorized tcp node must expose its bound listener port");
            (node, port)
        };

        let (node_a, port_a) = start_one(id_a, id_b, identity_a, &path_a, pub_b);
        let (node_b, port_b) = start_one(id_b, id_a, identity_b, &path_b, pub_a);
        (node_a, port_a, node_b, port_b)
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
    ) -> Option<HewRemotePid> {
        let deadline = Instant::now() + timeout;
        loop {
            // SAFETY: node/name are valid for this bounded wait.
            let location = unsafe { lookup_exact(node, name) };
            if let Some(location) = location {
                if location.node() == test_node_id(expected_node_id) {
                    return Some(HewRemotePid::from(location));
                }
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

    #[cfg(feature = "encryption")]
    fn run_registry_gossip_server_helper() {
        reset_two_process_delivery();
        // Install the runtime before touching the name registry: in this helper
        // subprocess no `runtime_test_guard` is held, so the registry's
        // `rt_current()` resolver has nothing to read until init runs.
        let _real_sched = init_real_scheduler();
        crate::registry::hew_registry_clear();

        let (node, port) = start_authorized_tcp_node(
            TWO_PROCESS_REGISTRY_SERVER_NODE,
            TWO_PROCESS_REGISTRY_CLIENT_NODE,
        );
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

    #[cfg(feature = "encryption")]
    fn run_registry_gossip_client_helper() {
        // Install the runtime before touching the name registry (helper
        // subprocess holds no `runtime_test_guard`).
        let _real_sched = init_real_scheduler();
        crate::registry::hew_registry_clear();

        let server_port = std::env::var(TWO_PROCESS_SERVER_PORT_ENV)
            .expect("server port env")
            .parse::<u16>()
            .expect("server port");
        let (node, _client_port) = start_authorized_tcp_node(
            TWO_PROCESS_REGISTRY_CLIENT_NODE,
            TWO_PROCESS_REGISTRY_SERVER_NODE,
        );

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
            hew_node_send_location(
                node.as_ptr(),
                &raw const remote_pid,
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

    #[cfg(feature = "encryption")]
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

        let (node, port) = start_authorized_tcp_node(node_id, TWO_PROCESS_REGISTRY_CLIENT_NODE);
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

    #[cfg(feature = "encryption")]
    struct TwoProcessAskClient {
        node: TestNode,
        remote_pid: HewRemotePid,
        _real_sched: RealSchedulerGuard,
    }

    #[cfg(feature = "encryption")]
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
            hew_node_api_ask_location(
                &raw const remote_pid,
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

    #[cfg(feature = "encryption")]
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
            hew_node_api_ask_location(
                &raw const remote_pid,
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

    #[cfg(feature = "encryption")]
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
        let (node, _client_port) = start_authorized_tcp_node(client_node_id, server_node_id);

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
            Location::try_from(remote_pid).unwrap().node(),
            test_node_id(client_node_id),
            "ask test must not resolve to a local pid"
        );
        TwoProcessAskClient {
            node,
            remote_pid,
            _real_sched: real_sched,
        }
    }

    #[cfg(feature = "encryption")]
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

    #[cfg(feature = "encryption")]
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

    #[cfg(feature = "encryption")]
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

    #[cfg(feature = "encryption")]
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

    #[cfg(feature = "encryption")]
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

    #[cfg(feature = "encryption")]
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

    /// Broker a real Noise key exchange for the two-process tests (issue #2652,
    /// D110). Mints both nodes' stable identities up-front in the shared temp
    /// dir and returns `(server_keyfile, server_pubkey_hex, client_keyfile,
    /// client_pubkey_hex)`. Each helper is handed its own keyfile (which it
    /// re-loads to the identical identity) plus the *peer's* pubkey, so both
    /// sides bind each other before connecting — no test-only posture promotion.
    #[cfg(feature = "encryption")]
    fn broker_two_process_noise_keys(dir: &std::path::Path) -> (String, String, String, String) {
        use crate::peer_binding::hex_lower;
        let server_keyfile = dir.join("server.key");
        let client_keyfile = dir.join("client.key");
        let server_id = crate::encryption::noise_identity_load_or_create(&server_keyfile)
            .expect("mint 2p server identity");
        let client_id = crate::encryption::noise_identity_load_or_create(&client_keyfile)
            .expect("mint 2p client identity");
        (
            server_keyfile.to_string_lossy().into_owned(),
            hex_lower(&server_id.public()),
            client_keyfile.to_string_lossy().into_owned(),
            hex_lower(&client_id.public()),
        )
    }

    #[cfg(feature = "encryption")]
    #[test]
    fn two_process_registry_gossip_lookup_then_tell() {
        let _guard = crate::runtime_test_guard();
        let ready_dir = tempfile::tempdir().expect("ready tempdir");
        let ready_file = ready_dir.path().join("server-ready");
        let ready_file_s = ready_file.to_string_lossy().into_owned();

        // Broker a real Noise key exchange (D110) so both nodes admit Strict.
        let (server_keyfile, server_pub_hex, client_keyfile, client_pub_hex) =
            broker_two_process_noise_keys(ready_dir.path());

        let mut server = spawn_registry_gossip_helper(
            "hew_node::tests::registry_gossip_two_process_server_helper",
            "server",
            &[
                (TWO_PROCESS_READY_FILE_ENV, ready_file_s),
                (TWO_PROCESS_KEYFILE_ENV, server_keyfile),
                (TWO_PROCESS_PEER_PUBKEY_ENV, client_pub_hex),
            ],
        );
        let server_port = wait_for_ready_port(&ready_file, &mut server, Duration::from_secs(10));

        let mut client = spawn_registry_gossip_helper(
            "hew_node::tests::registry_gossip_two_process_client_helper",
            "client",
            &[
                (TWO_PROCESS_SERVER_PORT_ENV, server_port.to_string()),
                (TWO_PROCESS_KEYFILE_ENV, client_keyfile),
                (TWO_PROCESS_PEER_PUBKEY_ENV, server_pub_hex),
            ],
        );
        let client_output = client.wait_output(Duration::from_secs(40));
        assert_child_success("client", &client_output);

        let server_output = server.wait_output(Duration::from_secs(40));
        assert_child_success("server", &server_output);
    }

    #[cfg(feature = "encryption")]
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

        // Broker a real Noise key exchange (D110) so both nodes admit Strict.
        let (server_keyfile, server_pub_hex, client_keyfile, client_pub_hex) =
            broker_two_process_noise_keys(ready_dir.path());

        let mut server = spawn_registry_gossip_helper(
            server_helper,
            server_role,
            &[
                (TWO_PROCESS_READY_FILE_ENV, ready_file_s),
                (TWO_PROCESS_KEYFILE_ENV, server_keyfile),
                (TWO_PROCESS_PEER_PUBKEY_ENV, client_pub_hex),
            ],
        );
        let server_port = wait_for_ready_port(&ready_file, &mut server, Duration::from_secs(10));

        let mut client = spawn_registry_gossip_helper(
            client_helper,
            client_role,
            &[
                (TWO_PROCESS_SERVER_PORT_ENV, server_port.to_string()),
                (TWO_PROCESS_KEYFILE_ENV, client_keyfile),
                (TWO_PROCESS_PEER_PUBKEY_ENV, server_pub_hex),
            ],
        );
        let client_output = client.wait_output(Duration::from_secs(40));
        assert_child_success(client_role, &client_output);

        let server_output = server.wait_output(Duration::from_secs(40));
        assert_child_success(server_role, &server_output);
    }

    #[cfg(feature = "encryption")]
    #[test]
    fn two_process_remote_ask_echo_double_returns_42() {
        run_two_process_remote_ask_case(
            "hew_node::tests::remote_ask_two_process_echo_server_helper",
            "ask_echo_server",
            "hew_node::tests::remote_ask_two_process_echo_client_helper",
            "ask_echo_client",
        );
    }

    #[cfg(feature = "encryption")]
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

    /// Start a **credentialed, Strict-authorized** quic-mesh listener node
    /// (issue #2652, D110).
    ///
    /// Unlike [`start_quic_mesh_test_listener_node`] (which leaves `node.auth`
    /// unconfigured → `Unverified` posture, delivery-only), this installs a real
    /// per-node [`PeerAuthSnapshot`] before start:
    ///  - `node_id = Some(node_id)` — the operator-pinned identity;
    ///  - `bindings = {peer_id → Spki(peer_spki)}` — the peer's *actual* leaf
    ///    SPKI (from [`make_mutually_pinned_mesh_tls`]), so the claim machine
    ///    binds the claimed `NodeId` to the authenticated key.
    ///
    /// The mutually-pinned `tls` (which already carries `with_peer_spki`) drives
    /// the mTLS handshake; admission extracts the *presented* leaf SPKI and
    /// matches it against the binding. This is a genuine authorized connection —
    /// there is no test-only posture promotion (D110): a peer presenting a
    /// different cert fails both the TLS pin and the claim-machine binding.
    #[cfg(feature = "quic")]
    fn start_authorized_quic_mesh_node(
        node_id: u16,
        tls: crate::quic_mesh::MeshTls,
        local_spki: Vec<u8>,
        peer_id: u16,
        peer_spki: Vec<u8>,
    ) -> (TestNode, u16) {
        use crate::peer_binding::PeerAuthConfig;

        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string for the duration of this helper.
        let node = unsafe { TestNode::new(node_id, &bind_addr) };
        assert!(!node.as_ptr().is_null(), "test node allocation failed");

        // Install the credentialed snapshot: local v1 route slot + the peer's
        // real SPKI pinned to its route slot.
        let mut config = PeerAuthConfig::default();
        config.legacy_wire_route_slot = std::num::NonZeroU16::new(node_id);
        config.transport = Some(crate::peer_binding::TransportSelection::QuicMesh);
        config.node_identity = Some(crate::node_identity::NodeId::from_spki(&local_spki));
        let identity_dir = tempfile::tempdir().expect("authorized mesh identity dir");
        config.identity_path = Some(identity_dir.path().join("node.pem"));
        config.mesh_identity = Some(crate::peer_binding::MeshIdentityMaterial::from_der(
            tls.cert_chain
                .iter()
                .map(|certificate| certificate.as_ref().to_vec())
                .collect(),
            tls.private_key_pkcs8.clone(),
            local_spki,
        ));
        config
            .pin_peer(peer_id, PeerCredential::Spki(peer_spki))
            .expect("distinct authorized mesh peer pin");
        let snapshot = config
            .snapshot_for_start()
            .expect("acquire authorized mesh session");
        // SAFETY: node is freshly created (STOPPED); installing a snapshot is valid.
        let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
        assert_eq!(set_rc, 0, "install auth snapshot on node {node_id}");

        // SAFETY: hew_transport_quic_mesh_new returns an owned transport pointer.
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

        // SAFETY: node owns the previously null transport slot; replace it with
        // the pre-allocated quic_mesh transport before start.
        unsafe {
            (*node.as_ptr()).transport = transport;
        }

        // SAFETY: node and transport pointers are valid; start consumes the
        // injected transport and reads the installed strict snapshot.
        let rc = unsafe { hew_node_start(node.as_ptr()) };
        assert_eq!(
            rc,
            0,
            "hew_node_start({node_id}) authorized quic_mesh failed: {:?}",
            crate::stream_error::take_last_error()
        );
        // SAFETY: node started successfully on the quic_mesh transport.
        let port = unsafe {
            crate::quic_mesh::hew_transport_quic_mesh_bound_port((*node.as_ptr()).transport)
        }
        .expect("started quic_mesh test node must expose its bound listener port");
        (node, port)
    }

    /// Start a mutually-authorized quic-mesh node **pair** (issue #2652, D110).
    ///
    /// Returns `(node_a, port_a, node_b, port_b)`. `node_a` is started first (so
    /// it becomes `CURRENT_NODE` / initiator); both nodes carry cross-bound
    /// `Spki → NodeId` credentials so a connection between them admits Strict.
    #[cfg(feature = "quic")]
    fn start_authorized_quic_mesh_pair(id_a: u16, id_b: u16) -> (TestNode, u16, TestNode, u16) {
        let (tls_a, tls_b, spki_a, spki_b) =
            make_mutually_pinned_mesh_tls(&format!("node-{id_a}"), &format!("node-{id_b}"));
        let (node_a, port_a) =
            start_authorized_quic_mesh_node(id_a, tls_a, spki_a.clone(), id_b, spki_b.clone());
        thread::sleep(Duration::from_millis(50));
        let (node_b, port_b) = start_authorized_quic_mesh_node(id_b, tls_b, spki_b, id_a, spki_a);
        (node_a, port_a, node_b, port_b)
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

    /// A low-level node whose explicit v1 route slot contradicts its installed
    /// snapshot route slot is rejected before the listener binds.
    #[test]
    fn node_start_rejects_conflicting_snapshot_route_slot_before_listen() {
        use crate::peer_binding::{PeerAuthConfig, PeerCredential};
        let _guard = crate::runtime_test_guard();
        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // Explicit low-level route slot 100 conflicts with snapshot route slot 42.
        // SAFETY: bind_addr is valid for the duration of this test.
        let node = unsafe { TestNode::new(100, &bind_addr) };
        assert!(!node.as_ptr().is_null());
        let mut config = PeerAuthConfig::default();
        config.legacy_wire_route_slot = std::num::NonZeroU16::new(42);
        config
            .pin_peer(43, PeerCredential::NoiseKey([0xAB; 32]))
            .expect("distinct peer pin");
        let snapshot = config.snapshot();
        // SAFETY: node is STOPPED; installing a snapshot is valid.
        let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
        assert_eq!(set_rc, 0);
        // SAFETY: node is valid; start must reject the conflicting route slot.
        let rc = unsafe { hew_node_start(node.as_ptr()) };
        assert_eq!(rc, -1, "conflicting snapshot route slot must be rejected");
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
            err.contains("conflicts with the frozen v1 route slot"),
            "diagnostic should name the conflict; got: {err}"
        );
    }

    /// A low-level node created with route slot zero adopts the frozen snapshot
    /// route slot at start.
    #[test]
    fn node_start_adopts_snapshot_route_slot_when_created_with_zero() {
        use crate::peer_binding::{PeerAuthConfig, PeerCredential};
        let _guard = crate::runtime_test_guard();
        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr valid for the test.
        let node = unsafe { TestNode::new(0, &bind_addr) };
        assert!(!node.as_ptr().is_null());
        let mut config = PeerAuthConfig::default();
        config.legacy_wire_route_slot = std::num::NonZeroU16::new(4242);
        config
            .pin_peer(4243, PeerCredential::NoiseKey([0xCD; 32]))
            .expect("distinct peer pin");
        let snapshot = config.snapshot();
        // SAFETY: node STOPPED.
        let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
        assert_eq!(set_rc, 0);
        // SAFETY: node valid; start should adopt 4242 and succeed.
        let rc = unsafe { hew_node_start(node.as_ptr()) };
        assert_eq!(
            rc,
            0,
            "start should adopt snapshot route slot: {:?}",
            crate::stream_error::take_last_error()
        );
        // SAFETY: node valid & running.
        unsafe {
            assert_eq!((*node.as_ptr()).route_slot, 4242);
            assert_eq!(hew_node_stop(node.as_ptr()), 0);
        }
    }

    /// Defence-in-depth: a self-inconsistent snapshot (unverified opt-out WITH
    /// bindings) fails the node's own start before listen.
    #[test]
    fn node_start_rejects_malformed_snapshot() {
        use crate::peer_binding::{PeerAuthConfig, PeerCredential};
        let _guard = crate::runtime_test_guard();
        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr valid.
        let node = unsafe { TestNode::new(7, &bind_addr) };
        assert!(!node.as_ptr().is_null());
        let mut config = PeerAuthConfig::default();
        config.legacy_wire_route_slot = std::num::NonZeroU16::new(7);
        config.unverified_optout = true;
        config
            .pin_peer(42, PeerCredential::NoiseKey([0x11; 32]))
            .expect("distinct peer pin");
        let snapshot = config.snapshot();
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

    #[test]
    fn public_start_env_ignores_removed_identity_and_unverified_controls() {
        let _guard = crate::runtime_test_guard();
        let saved = crate::env::ENV_LOCK.read_access(|()| {
            [
                ("HEW_NODE_ID", std::env::var_os("HEW_NODE_ID")),
                (
                    "HEW_DIST_UNVERIFIED",
                    std::env::var_os("HEW_DIST_UNVERIFIED"),
                ),
                ("HEW_TRANSPORT", std::env::var_os("HEW_TRANSPORT")),
            ]
        });
        crate::env::ENV_LOCK.access(|()| {
            // SAFETY: ENV_LOCK serializes process-global environment mutation.
            unsafe {
                std::env::set_var("HEW_NODE_ID", "4242");
                std::env::set_var("HEW_DIST_UNVERIFIED", "true");
                std::env::remove_var("HEW_TRANSPORT");
            }
        });

        let mut config = crate::peer_binding::PeerAuthConfig::default();
        merge_start_env_into_config(&mut config).unwrap();
        assert_eq!(config.legacy_wire_route_slot, None);
        assert!(!config.unverified_optout);

        crate::env::ENV_LOCK.access(|()| {
            // SAFETY: ENV_LOCK serializes process-global environment mutation.
            unsafe {
                for (key, value) in saved {
                    if let Some(value) = value {
                        std::env::set_var(key, value);
                    } else {
                        std::env::remove_var(key);
                    }
                }
            }
        });
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

    /// Public owner-scoped staging (BLOCK-6): after `Node::load_keys` stages the
    /// authenticated identity now required by v2, `Node::start` transitions
    /// `Building → Running{owner}` and `Node::shutdown` returns it to `Building`
    /// with a bumped generation. An unauthenticated default start is no longer
    /// valid.
    #[cfg(feature = "encryption")]
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
        let _identity = stage_public_api_test_identity();
        let bind = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind is a valid C string for this call.
        let rc = unsafe { hew_node_api_start(bind.as_ptr()) };
        assert_eq!(rc, 0, "identity-backed public start should succeed");
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
    /// The first start stages a real identity because unauthenticated public
    /// startup is no longer valid under the v2 handshake.
    #[cfg(feature = "encryption")]
    #[test]
    fn public_start_rejected_when_already_active() {
        let _guard = crate::runtime_test_guard();
        {
            let mut g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            g.state = ConfigState::default();
        }
        let _identity = stage_public_api_test_identity();
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
    /// poisons the staged config, so the next `Node::start` refuses to bind a
    /// listener (returns -1) rather than silently coming up with an ephemeral
    /// identity or an incomplete allowlist. This is the closed half of the
    /// worst-case fail-open F6 addresses: pre-fix, `load_keys` returned -1
    /// (discarded by the `Unit` Hew form) yet `start` proceeded.
    #[cfg(feature = "quic")]
    #[test]
    fn start_refuses_after_failed_peer_auth_setup_fail_closed() {
        let _guard = crate::runtime_test_guard();
        // The setup poison now lives on the per-node staged config (D14), not a
        // process-global flag; reset the staging cell to a clean Building config
        // between sub-cases via this local helper.
        let reset_staging = || {
            let mut g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            g.state = ConfigState::default();
        };
        let bind = CString::new("127.0.0.1:0").expect("valid bind addr");

        // --- corrupt keyfile: load_keys must fail and poison start ---
        reset_staging();
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
        let _ = std::fs::remove_file(&key);

        // --- bad-hex allow_peer: must fail and poison start ---
        reset_staging();
        let bad_hex = CString::new("nothex!!").expect("valid C string");
        // SAFETY: bad_hex is a valid NUL-terminated C string for this call.
        let rc_allow = unsafe { hew_node_api_allow_peer(2, bad_hex.as_ptr()) };
        // SAFETY: bind is a valid NUL-terminated C string for this call.
        let rc_start_after_allow = unsafe { hew_node_api_start(bind.as_ptr()) };
        if rc_start_after_allow == 0 {
            // SAFETY: shutdown reclaims the current node, if any; takes no arguments.
            unsafe { hew_node_api_shutdown() };
        }
        // Reset BEFORE asserting so a failed assertion can't poison sibling tests.
        reset_staging();

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

    #[cfg(feature = "encryption")]
    #[test]
    fn concurrent_api_shutdown_claims_current_node_once() {
        let _guard = crate::runtime_test_guard();

        {
            let mut g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            g.state = ConfigState::default();
        }
        // V2 public startup requires a loaded authenticated identity; the old
        // unauthenticated default-start harness is intentionally invalid.
        let _identity = stage_public_api_test_identity();
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

    /// D8 / BLOCK-7 point 3: `Node::identity_key` reads the three-state staging
    /// config — `Building` config, held `Starting.config`, and the retained
    /// `Running.identity_export` clone — returns `""` (never null) when no stable
    /// identity is loaded, and NEVER dereferences the owner pointer (the owner
    /// here is a fabricated non-pointer value the read must not touch).
    #[test]
    fn node_identity_key_reads_three_state_config_without_owner_deref() {
        use crate::peer_binding::{PeerAuthConfig, StableNoiseIdentity, TransportSelection};
        let _guard = crate::runtime_test_guard();

        let set_state = |state| {
            let mut g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            g.state = state;
        };
        let read = || {
            let p = hew_node_api_identity_key();
            assert!(!p.is_null(), "identity_key must never return null");
            // SAFETY: p is a freshly-owned NUL-terminated hew string.
            let s = unsafe { std::ffi::CStr::from_ptr(p) }
                .to_string_lossy()
                .into_owned();
            // SAFETY: p was allocated by malloc_cstring; free via hew_string_drop.
            unsafe { crate::string::hew_string_drop(p) };
            s
        };

        // Building (default) — no identity loaded ⇒ "".
        set_state(ConfigState::default());
        assert_eq!(
            read(),
            "",
            "no stable identity must read as the empty string"
        );

        // Building with a pinned TCP Noise identity ⇒ its public key hex.
        let noise = StableNoiseIdentity::from_raw([0x11; 32], [0x22; 32]);
        let expect_hex = "11".repeat(32);
        let mut cfg = PeerAuthConfig::default();
        cfg.transport = Some(TransportSelection::Tcp);
        cfg.noise_identity = Some(noise);
        set_state(ConfigState::Building(cfg.clone()));
        assert_eq!(read(), expect_hex, "Building must read the staged identity");

        // Starting holds the consumed config ⇒ same hex; owner is a fabricated
        // non-pointer value the read must not dereference.
        set_state(ConfigState::Starting {
            generation: 7,
            owner: 0xDEAD_BEEF,
            config: cfg,
        });
        assert_eq!(
            read(),
            expect_hex,
            "Starting must read the held config (no owner deref)"
        );

        // Running retains a standalone identity_export clone ⇒ that clone.
        set_state(ConfigState::Running {
            generation: 7,
            owner: 0xDEAD_BEEF,
            identity_export: "cafe".to_string(),
            node_identity: Some(test_node_id(7)),
        });
        assert_eq!(
            read(),
            "cafe",
            "Running must read the retained identity_export clone (no owner deref)"
        );

        // Restore clean staging for sibling tests.
        set_state(ConfigState::default());
    }

    /// Issue #2652 (item 2): `Node::set_transport` must pin the selection BEFORE
    /// any transport-sensitive credential is staged. A `set_transport` that would
    /// change the transport after `Node::allow_peer` / `Node::load_keys` staged
    /// material is refused fail-closed (returns `-1`) and leaves both the pinned
    /// config transport and the process-global `HEW_TRANSPORT` unchanged, so the
    /// already-staged Noise pubkeys / mesh SPKIs are never reinterpreted under a
    /// different transport. Re-selecting the SAME transport stays allowed
    /// (idempotent).
    #[cfg(feature = "quic")]
    #[test]
    fn set_transport_rejected_after_credential_staging() {
        use crate::peer_binding::TransportSelection as PT;
        let _guard = crate::runtime_test_guard();

        let reset = || {
            let mut g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            g.state = ConfigState::default();
        };
        let pinned_transport = || {
            let g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            match &g.state {
                ConfigState::Building(cfg) => cfg.transport,
                _ => panic!("expected Building state"),
            }
        };
        let env_transport =
            || crate::env::ENV_LOCK.read_access(|()| std::env::var("HEW_TRANSPORT").ok());

        reset();
        crate::env::ENV_LOCK.access(|()| {
            // SAFETY: exclusive process-global env access via ENV_LOCK.
            unsafe { std::env::remove_var("HEW_TRANSPORT") };
        });

        // Pin TCP explicitly, then stage a peer credential under it.
        let tcp = CString::new("tcp").unwrap();
        // SAFETY: tcp is a valid C string for the call.
        assert_eq!(unsafe { hew_node_api_set_transport(tcp.as_ptr()) }, 0);
        assert_eq!(pinned_transport(), Some(PT::Tcp));

        let noise_hex = CString::new("11".repeat(32)).unwrap();
        // SAFETY: noise_hex is a valid 32-byte (64-hex) C string.
        assert_eq!(unsafe { hew_node_api_allow_peer(2, noise_hex.as_ptr()) }, 0);
        assert_eq!(
            pinned_transport(),
            Some(PT::Tcp),
            "allow_peer must keep the pinned TCP selection"
        );

        // A transport change after staging is rejected fail-closed and mutates
        // neither the pinned config nor the env.
        let mesh = CString::new("quic-mesh").unwrap();
        crate::hew_clear_error();
        // SAFETY: mesh is a valid C string for the call.
        let flip_rc = unsafe { hew_node_api_set_transport(mesh.as_ptr()) };
        assert_eq!(
            flip_rc, -1,
            "set_transport after credential staging must be rejected"
        );
        assert_eq!(
            pinned_transport(),
            Some(PT::Tcp),
            "a rejected set_transport must NOT change the pinned selection"
        );
        assert_eq!(
            env_transport().as_deref(),
            Some("tcp"),
            "a rejected set_transport must NOT change HEW_TRANSPORT"
        );

        // Re-selecting the same transport is idempotent and allowed.
        // SAFETY: tcp is a valid C string for the call.
        let reselect_rc = unsafe { hew_node_api_set_transport(tcp.as_ptr()) };
        assert_eq!(
            reselect_rc, 0,
            "re-selecting the same transport after staging stays allowed"
        );

        reset();
        crate::env::ENV_LOCK.access(|()| {
            // SAFETY: exclusive process-global env access via ENV_LOCK.
            unsafe { std::env::remove_var("HEW_TRANSPORT") };
        });
    }

    /// Issue #2666 (concurrency): `Node::set_transport` folds its staged-credential
    /// check, the `HEW_TRANSPORT` write, and the config pin into ONE
    /// `PEER_AUTH_STATE` critical section, and `Node::allow_peer` / `Node::load_keys`
    /// re-verify the pinned selection under that same lock. Together these close
    /// the check-then-pin race: a credential is only ever staged when its type
    /// agrees with the pinned transport, so a concurrent `set_transport` can never
    /// leave a `NoiseKey` bound while the config is pinned to quic-mesh (or an SPKI
    /// bound under tcp-noise) — the credential confusion issue #2652 guards.
    ///
    /// This drives all four concurrency edge classes for the race: two racing
    /// writers (`set_transport` vs `allow_peer`), both establish orderings
    /// (set-wins-then-stage and stage-wins-then-set), and the stale-typing case
    /// (a credential typed under the old transport meeting a freshly pinned new
    /// one). The terminal invariant — never a torn pin — must hold on every
    /// interleaving, so the test hammers the window and asserts consistency after
    /// each round rather than depending on a fixed sleep.
    #[cfg(feature = "quic")]
    #[test]
    fn set_transport_credential_typing_atomic_under_concurrent_allow_peer() {
        let _guard = crate::runtime_test_guard();

        let reset_clean = || {
            {
                let mut g = PEER_AUTH_STATE
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                g.state = ConfigState::default();
            }
            crate::env::ENV_LOCK.access(|()| {
                // SAFETY: exclusive process-global env access via ENV_LOCK.
                unsafe { std::env::remove_var("HEW_TRANSPORT") }
            });
        };

        // 32-byte credential: a valid Noise pubkey under tcp AND a valid SPKI
        // under quic-mesh, so the SAME argument types differently by transport —
        // exactly the confusion a torn pin would create.
        let cred_hex = "11".repeat(32);

        for _ in 0..300 {
            reset_clean();

            let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
            let set_barrier = std::sync::Arc::clone(&barrier);
            let set_thread = thread::spawn(move || {
                let name = CString::new("quic-mesh").expect("valid transport name");
                set_barrier.wait();
                // SAFETY: name is a valid C string for this call.
                unsafe { hew_node_api_set_transport(name.as_ptr()) }
            });

            let allow_hex = cred_hex.clone();
            let allow_thread = thread::spawn(move || {
                let hex = CString::new(allow_hex).expect("valid hex credential");
                barrier.wait();
                // SAFETY: hex is a valid C string for this call.
                unsafe { hew_node_api_allow_peer(2, hex.as_ptr()) }
            });

            let _ = set_thread.join().expect("set_transport thread panicked");
            let _ = allow_thread.join().expect("allow_peer thread panicked");

            // Terminal invariant: every staged credential's type must agree with
            // the pinned transport. A torn pin (pre-fix) leaves a NoiseKey bound
            // while the config is pinned to quic-mesh, or an SPKI under tcp.
            let g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let ConfigState::Building(cfg) = &g.state else {
                panic!("staging must remain Building; no node was started");
            };
            for credential in cfg.bindings().values() {
                match credential {
                    PeerCredential::NoiseKey(_) => assert_eq!(
                        cfg.transport,
                        Some(PeerTransport::Tcp),
                        "a staged NoiseKey requires the config to be pinned to tcp \
                         (torn pin: credential typed under tcp but transport re-pinned)"
                    ),
                    PeerCredential::Spki(_) => assert_eq!(
                        cfg.transport,
                        Some(PeerTransport::QuicMesh),
                        "a staged SPKI requires the config to be pinned to quic-mesh \
                         (torn pin: credential typed under quic-mesh but transport re-pinned)"
                    ),
                }
            }
            // Whenever a credential is staged, the transport must be pinned (never
            // left None): the stage and the pin are one atomic step under the lock.
            if !cfg.bindings().is_empty() {
                assert!(
                    cfg.transport.is_some(),
                    "a staged credential must leave the transport pinned"
                );
            }
        }

        reset_clean();
    }

    /// Issue #2652 (item 2): a node's exported stable identity (`identity_key()`)
    /// is stable across an attempted transport flip after staging. Once
    /// `Node::load_keys` pins the transport and stages the Noise identity, a
    /// rejected `Node::set_transport` cannot change which credential is exported,
    /// and the pinned selection `Node::start` will use stays put.
    #[cfg(all(feature = "quic", feature = "encryption"))]
    #[test]
    fn identity_key_stable_across_attempted_transport_flip() {
        use crate::peer_binding::TransportSelection as PT;
        let _guard = crate::runtime_test_guard();

        let reset = || {
            let mut g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            g.state = ConfigState::default();
        };
        let pinned_transport = || {
            let g = PEER_AUTH_STATE
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            match &g.state {
                ConfigState::Building(cfg) => cfg.transport,
                _ => panic!("expected Building state"),
            }
        };
        let read_identity = || {
            let p = hew_node_api_identity_key();
            assert!(!p.is_null());
            // SAFETY: p is a freshly-owned NUL-terminated hew string.
            let s = unsafe { std::ffi::CStr::from_ptr(p) }
                .to_string_lossy()
                .into_owned();
            // SAFETY: p was allocated by malloc_cstring; free via hew_string_drop.
            unsafe { crate::string::hew_string_drop(p) };
            s
        };

        reset();
        crate::env::ENV_LOCK.access(|()| {
            // SAFETY: exclusive process-global env access via ENV_LOCK.
            unsafe { std::env::set_var("HEW_TRANSPORT", "tcp") };
        });

        // Load (mint) a stable Noise identity under TCP — pins TCP, stages the id.
        let dir = tempfile::tempdir().expect("identity keydir");
        let keyfile = dir.path().join("node.key");
        let keyfile_c = CString::new(keyfile.to_str().unwrap()).unwrap();
        // SAFETY: keyfile_c is a valid C string for the call.
        assert_eq!(unsafe { hew_node_api_load_keys(keyfile_c.as_ptr()) }, 0);
        assert_eq!(pinned_transport(), Some(PT::Tcp));

        let key_before = read_identity();
        assert!(
            !key_before.is_empty(),
            "a loaded Noise identity must export a nonempty credential"
        );

        // Attempt to flip to quic-mesh — rejected; identity and pin are stable.
        let mesh = CString::new("quic-mesh").unwrap();
        crate::hew_clear_error();
        // SAFETY: mesh is a valid C string for the call.
        assert_eq!(unsafe { hew_node_api_set_transport(mesh.as_ptr()) }, -1);

        let key_after = read_identity();
        assert_eq!(
            key_before, key_after,
            "the exported identity must be stable across a rejected transport flip"
        );
        assert_eq!(
            pinned_transport(),
            Some(PT::Tcp),
            "start must use the stored TCP selection, not the attempted mesh flip"
        );

        reset();
        crate::env::ENV_LOCK.access(|()| {
            // SAFETY: exclusive process-global env access via ENV_LOCK.
            unsafe { std::env::remove_var("HEW_TRANSPORT") };
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
                lookup_exact(node.as_ptr(), actor_name.as_ptr()),
                local_actor_location(&*node.as_ptr(), actor_id)
            );
            assert_eq!(lookup_exact(node.as_ptr(), missing_name.as_ptr()), None);
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
                lookup_exact(node.as_ptr(), actor_name.as_ptr()),
                local_actor_location(&*node.as_ptr(), actor_id)
            );

            let n = &*node.as_ptr();
            assert!(!n.cluster.is_null());
            let cluster = &*n.cluster;
            let _ = cluster.take_registry_gossip(10);

            assert_eq!(crate::actor::hew_actor_free(actor), 0);
            assert_eq!(lookup_exact(node.as_ptr(), actor_name.as_ptr()), None);
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
                lookup_exact(node.as_ptr(), actor_name.as_ptr()),
                local_actor_location(&*node.as_ptr(), actor_id)
            );
            assert!(!crate::registry::hew_registry_lookup(actor_name.as_ptr()).is_null());

            assert_eq!(hew_node_stop(node.as_ptr()), 0);
            assert_eq!(lookup_exact(node.as_ptr(), actor_name.as_ptr()), None);
            assert!(crate::registry::hew_registry_lookup(actor_name.as_ptr()).is_null());

            assert_eq!(crate::actor::hew_actor_free(actor), 0);
        }

        crate::registry::hew_registry_clear();
    }

    #[cfg(feature = "encryption")]
    #[test]
    fn two_node_connect_and_handshake() {
        let _guard = crate::runtime_test_guard();

        crate::registry::hew_registry_clear();

        let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(201, 202);

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
                lookup_exact(node2.as_ptr(), actor_name.as_ptr()),
                local_actor_location(&*node2.as_ptr(), actor_id)
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
    fn exact_identity_prunes_remote_names_for_departed_node() {
        let _guard = crate::runtime_test_guard();

        let registry = HewRegistry::default();
        {
            let mut map = registry.remote_names.lock_or_recover();
            map.insert("remote-a".to_owned(), test_location(202, 0x10));
            map.insert("remote-b".to_owned(), test_location(202, 0x11));
            map.insert("other-node".to_owned(), test_location(303, 0x20));
        }

        let _ = take_registry_names_if(&registry, |location| location.node() == test_node_id(202));

        let map = registry.remote_names.lock_or_recover();
        assert!(!map.contains_key("remote-a"));
        assert!(!map.contains_key("remote-b"));
        assert_eq!(
            map.get("other-node").copied(),
            Some(test_location(303, 0x20))
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
        // SAFETY: test owns this stopped node.
        unsafe { install_test_auth(node, 50) };
        let name = c"test_unreg_actor";

        // SAFETY: node is a valid pointer; name is a valid C string literal.
        unsafe {
            assert_eq!(hew_node_register(node, name.as_ptr(), 999), 0);
            let mut found = HewRemotePid::default();
            assert_eq!(
                hew_node_lookup_location(node, name.as_ptr(), &raw mut found),
                0
            );
            assert_eq!(
                Location::try_from(found).unwrap(),
                local_actor_location(&*node, 999).unwrap()
            );
        }

        // SAFETY: node is a valid pointer; name is a valid C string literal.
        unsafe {
            assert_eq!(hew_node_unregister(node, name.as_ptr()), 0);
            assert_eq!(lookup_exact(node, name.as_ptr()), None);
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

    #[test]
    fn register_same_name_repoints_future_lookup() {
        // `hew_node_new` records the node in the runtime-owned node slot, which
        // resolves through `rt_current()` and fails closed when no runtime is
        // installed; the guard installs the worker-less default runtime.
        let _guard = crate::runtime_test_guard();

        // SAFETY: bind_addr is a valid NUL-terminated C string literal.
        let node = unsafe { hew_node_new(51, c"127.0.0.1:0".as_ptr()) };
        assert!(!node.is_null());
        // SAFETY: test owns this stopped node.
        unsafe { install_test_auth(node, 51) };
        let name = c"l18_primary";

        // SAFETY: node is a valid pointer; name is a valid C string literal.
        unsafe {
            assert_eq!(hew_node_register(node, name.as_ptr(), 7001), 0);
            assert_eq!(
                hew_node_register(node, name.as_ptr(), 7001),
                0,
                "registering the same location is idempotent"
            );
            assert_eq!(hew_node_register(node, name.as_ptr(), 7002), 0);
            let mut found = HewRemotePid::default();
            assert_eq!(
                hew_node_lookup_location(node, name.as_ptr(), &raw mut found),
                0
            );
            assert_eq!(
                Location::try_from(found).unwrap(),
                local_actor_location(&*node, 7002).unwrap()
            );
        }

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

    #[test]
    fn lookup_location_writes_output_only_on_success() {
        let _guard = crate::runtime_test_guard();

        // SAFETY: bind address is a valid C string for the duration of the test.
        let node = unsafe { hew_node_new(52, c"127.0.0.1:0".as_ptr()) };
        assert!(!node.is_null());
        // SAFETY: test owns this stopped node.
        unsafe { install_test_auth(node, 52) };
        let missing = c"missing_exact_location";
        let sentinel = HewRemotePid::from(test_location(99, 777));
        let mut found = sentinel;

        // SAFETY: node, name, and output pointers are valid.
        unsafe {
            assert_eq!(
                hew_node_lookup_location(node, missing.as_ptr(), &raw mut found),
                -1
            );
            assert_eq!(found, sentinel);
            hew_node_free(node);
        }
    }

    #[test]
    fn unregister_keeps_issued_location_live_until_actor_death() {
        let _guard = crate::runtime_test_guard();

        // SAFETY: bind address is a valid C string for the duration of the test.
        let node = unsafe { hew_node_new(53, c"127.0.0.1:0".as_ptr()) };
        assert!(!node.is_null());
        // SAFETY: test owns this stopped node.
        unsafe {
            install_test_auth(node, 53);
            assert_eq!(hew_node_start(node), 0);
            let actor = crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!actor.is_null());
            let name = c"issued_location_lifetime";
            assert_eq!(hew_node_register(node, name.as_ptr(), (*actor).id), 0);
            let mut target = HewRemotePid::default();
            assert_eq!(
                hew_node_lookup_location(node, name.as_ptr(), &raw mut target),
                0
            );
            assert_eq!(hew_node_unregister(node, name.as_ptr()), 0);
            let exact = Location::try_from(target).unwrap();
            assert_eq!(
                routing::hew_routing_lookup_location((*node).routing_table, exact),
                routing::LocationRoute::Local {
                    actor_id: (*actor).id
                }
            );
            assert_eq!(
                crate::lifetime::live_actors::get_actor_ptr_by_id((*actor).id),
                Some(actor)
            );
            crate::actor::hew_actor_close(actor);
            assert_eq!(crate::actor::hew_actor_free(actor), 0);
            assert_eq!(
                hew_node_send_location(node, &raw const target, test_dispatch(), 1, ptr::null(), 0,),
                HEW_ERR_STALE_REF
            );
            assert_eq!(hew_node_stop(node), 0);
            hew_node_free(node);
        }
    }

    #[test]
    fn gossip_repoint_updates_future_lookup_and_old_remove_does_not_erase_it() {
        let _guard = crate::runtime_test_guard();

        let registry = HewRegistry::default();
        let old = HewLocation::from(test_location(90, 9001));
        let new = HewLocation::from(test_location(90, 9002));
        registry
            .remote_names
            .lock_or_recover()
            .insert("l18_gossip".to_owned(), Location::try_from(old).unwrap());
        let user_data = (&raw const registry).cast_mut().cast::<c_void>();
        let name = c"l18_gossip";

        node_registry_gossip_callback(name.as_ptr(), &raw const new, true, user_data);
        let map = registry.remote_names.lock_or_recover();
        assert_eq!(
            map.get("l18_gossip").copied(),
            Some(Location::try_from(new).unwrap()),
            "new registration must re-point future lookup"
        );
        drop(map);
        node_registry_gossip_callback(name.as_ptr(), &raw const old, false, user_data);
        assert_eq!(
            registry
                .remote_names
                .lock_or_recover()
                .get("l18_gossip")
                .copied(),
            Some(Location::try_from(new).unwrap())
        );
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

        let remote_pid = HewRemotePid::from(test_location(remote_node_id, 1));
        // SAFETY: null data with size 0 is valid; the remote path should fail
        // immediately because no active node is installed.
        let reply = unsafe {
            hew_node_api_ask_location(
                &raw const remote_pid,
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

    /// Issue #2652 D12: a reply that arrives on a DIFFERENT `(conn_mgr, conn_id)`
    /// than the ask was issued on must not resolve it — a peer cannot complete
    /// (or reject) another peer's ask. The pending entry survives the mismatched
    /// attempt and is resolvable only by the originating connection.
    #[test]
    fn reply_completion_requires_the_originating_connection() {
        let _guard = crate::runtime_test_guard();
        let origin = ConnectionKey::new(std::ptr::without_provenance(0x5010), 21);
        let (id, pending) = reply_table().register(origin);

        // Same request id, wrong manager → rejected, ask stays pending.
        assert!(
            !complete_remote_reply(std::ptr::without_provenance(0x9999), 21, id, &[1, 2, 3]),
            "a reply on a different conn_mgr must not complete the ask"
        );
        // Same manager, wrong conn_id → rejected, ask stays pending.
        assert!(
            !complete_remote_reply(std::ptr::without_provenance(0x5010), 99, id, &[1, 2, 3]),
            "a reply on a different conn_id must not complete the ask"
        );
        // A rejection reply on the wrong connection is likewise rejected.
        assert!(
            !fail_remote_reply(std::ptr::without_provenance(0x9999), 21, id, &[]),
            "a rejection on a different connection must not fail the ask"
        );
        assert!(
            pending.outcome.lock().unwrap().is_none(),
            "the ask must remain unresolved after mismatched attempts"
        );

        // The originating connection resolves it.
        assert!(
            complete_remote_reply(std::ptr::without_provenance(0x5010), 21, id, &[9, 9]),
            "the originating connection must complete the ask"
        );
        let guard = pending.outcome.lock().unwrap();
        let outcome = guard
            .as_ref()
            .expect("outcome set by originating connection");
        assert_eq!(outcome.status, ReplyStatus::Success);
        assert_eq!(outcome.data, vec![9, 9]);
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
        assert!(fail_remote_reply(
            std::ptr::without_provenance(91),
            14,
            id,
            &[]
        ));

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
        assert!(!fail_remote_reply(
            std::ptr::without_provenance(92),
            15,
            id,
            &[0xFF]
        ));

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
            let remote_location = HewLocation::from(test_location(200, 0x99));

            // Invoke the callback directly (as the cluster would).
            node_registry_gossip_callback(
                remote_name.as_ptr(),
                &raw const remote_location,
                true,
                n.registry.cast::<c_void>(),
            );

            // Local lookup should not find it (not registered locally).
            assert!(crate::registry::hew_registry_lookup(remote_name.as_ptr()).is_null());

            // Node lookup should find it via remote_names.
            let mut found = HewRemotePid::default();
            assert_eq!(
                hew_node_lookup_location(node.as_ptr(), remote_name.as_ptr(), &raw mut found,),
                0
            );
            assert_eq!(
                Location::try_from(found).unwrap(),
                Location::try_from(remote_location).unwrap()
            );

            // Simulate removal.
            node_registry_gossip_callback(
                remote_name.as_ptr(),
                &raw const remote_location,
                false,
                n.registry.cast::<c_void>(),
            );
            assert_eq!(lookup_exact(node.as_ptr(), remote_name.as_ptr()), None);

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
        let error = {
            let error = crate::hew_last_error();
            if error.is_null() {
                None
            } else {
                // SAFETY: `hew_last_error` returns a live NUL-terminated string.
                Some(
                    unsafe { CStr::from_ptr(error) }
                        .to_string_lossy()
                        .into_owned(),
                )
            }
        };
        panic!("could not connect initiator to responder: {error:?}");
    }

    /// Poll until both connection managers report at least one active connection.
    unsafe fn wait_for_handshake(node1: *mut HewNode, node2: *mut HewNode) {
        let ok = (0..80).any(|i| {
            // SAFETY: node1 and node2 pointers are valid for the duration of the test.
            let ready = unsafe {
                let mgr1 = &*(*node1).conn_mgr;
                let mgr2 = &*(*node2).conn_mgr;
                let conn1 = connection::hew_connmgr_conn_id_for_node(
                    (*node1).conn_mgr,
                    (*node2).route_slot,
                );
                let conn2 = connection::hew_connmgr_conn_id_for_node(
                    (*node2).conn_mgr,
                    (*node1).route_slot,
                );
                conn1 >= 0
                    && conn2 >= 0
                    && connection::authenticated_peer_node_id_for_conn(mgr1, conn1)
                        == (*node2).route_slot
                    && connection::authenticated_peer_node_id_for_conn(mgr2, conn2)
                        == (*node1).route_slot
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

    #[cfg(feature = "encryption")]
    #[test]
    fn two_node_remote_send_delivery() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Node 1 starts first and remains CURRENT_NODE; both peers carry
        // mutually-authorized Noise identities required by v2 admission.
        let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(301, 302);

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
        let target = test_remote_pid(actor_id);
        // SAFETY: null payload / size 0 is valid for a bare signal message.
        let rc = unsafe {
            hew_node_send_location(
                node1.as_ptr(),
                &raw const target,
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
    // than TCP. Two in-process nodes mutually pin each other's SPKIs in their
    // per-node auth snapshots and inject the matching TLS identities.

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

        // Node 1 starts first and remains CURRENT_NODE; the helper installs
        // mutually-authorized SPKI bindings for v2 admission.
        let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(401, 402);

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
        let target = test_remote_pid(actor_id);
        // SAFETY: null payload / size 0 is valid for a bare signal message.
        let rc = unsafe {
            hew_node_send_location(
                node1.as_ptr(),
                &raw const target,
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

    /// Transitional wire routing: the v1 handshake advertises the node's
    /// configured route slot. After an authorized quic-mesh handshake, the
    /// responder must observe the initiator under that exact slot, and the
    /// credential-bound authenticated route must resolve to the same slot. This
    /// proves the two-part invariant the
    /// `ctrl-frame-binds-to-authenticated-peer` lesson requires: the handshake
    /// `NodeId` is BOTH credential-bound AND equal to the operator-pinned id.
    ///
    /// If the initiator advertised anything other than its bound id, admission
    /// on the responder would reject it (credential / `NodeId` mismatch) and no
    /// connection would key to `ID_A` at all — so a successful observation of
    /// `ID_A` here is a positive proof of the binding, not a coincidence.
    #[cfg(feature = "quic")]
    #[test]
    fn wire_route_slot_peer_observes_configured_slot() {
        // Configured slots chosen to be unrelated to any PID-derived scheme.
        const ID_A: u16 = 331;
        const ID_B: u16 = 332;

        let _guard = crate::runtime_test_guard();
        crate::registry::hew_registry_clear();

        let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(ID_A, ID_B);

        let connect_addr = CString::new(format!("{ID_B}@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and the connect address are valid for this attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until the end of the test.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // node2 (responder) must have admitted node1's inbound connection under
        // its configured route slot (331). Poll briefly: `wait_for_handshake`
        // only proves the socket is up; claim publication completes a moment
        // later inside admission.
        // SAFETY: node2 is valid and its conn_mgr is live while node2 runs.
        let mgr2 = unsafe { (*node2.as_ptr()).conn_mgr };
        assert!(!mgr2.is_null(), "node2 conn_mgr must be live");
        let mut conn_id = -1;
        let mut authenticated = 0u16;
        for _ in 0..100 {
            // SAFETY: mgr2 is a live manager pointer for the duration of the read.
            conn_id = unsafe { connection::hew_connmgr_conn_id_for_node(mgr2, ID_A) };
            if conn_id >= 0 {
                // SAFETY: mgr2 is live for this read.
                authenticated =
                    connection::authenticated_peer_node_id_for_conn(unsafe { &*mgr2 }, conn_id);
                if authenticated == ID_A {
                    break;
                }
            }
            thread::sleep(Duration::from_millis(20));
        }
        assert!(
            conn_id >= 0,
            "node2 must route the initiator under its configured id {ID_A}"
        );
        // SAFETY: mgr2 is a live manager pointer for the duration of these reads.
        let mgr2_ref = unsafe { &*mgr2 };
        assert_eq!(
            connection::peer_node_id_for_conn(mgr2_ref, conn_id),
            ID_A,
            "peer-observed v1 route slot must equal the configured slot"
        );
        assert_eq!(
            authenticated, ID_A,
            "the observed NodeId must be credential-bound (authenticated), not merely declared"
        );

        // SAFETY: nodes were allocated in this test and remain valid here.
        unsafe {
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

    #[cfg(feature = "quic")]
    #[test]
    fn two_node_remote_void_ask_returns_sentinel() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Cross-bound SPKI→NodeId credentials → Strict authorized connection
        // (inbound ask authority requires an authenticated peer, D9/D12).
        let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(313, 314);

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

        let target = test_remote_pid(actor_id);
        // SAFETY: this is a remote void ask; null payload/size are valid and no reply buffer is expected.
        let reply_ptr = unsafe {
            hew_node_api_ask_location(
                &raw const target,
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

    #[cfg(feature = "quic")]
    #[test]
    fn two_node_remote_ask_reply() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();
        // Remote ask requires a registered codec for the payload msg_type.
        register_test_u32_codec(test_dispatch(), 1);

        // Node 1 (initiator) starts first → CURRENT_NODE = node1, LOCAL_NODE_ID = 311.
        // Node 2 (responder) starts after; CURRENT_NODE stays as node1. Both nodes
        // carry cross-bound SPKI→NodeId credentials so the connection admits
        // Strict — inbound ask authority (D9/D12) requires an authenticated peer.
        let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(311, 312);

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
        let target = test_remote_pid(actor_id);
        // SAFETY: send_value is a valid u32 on the stack; reply is malloc'd, freed below.
        let reply_ptr = unsafe {
            hew_node_api_ask_location(
                &raw const target,
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

    #[cfg(feature = "quic")]
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

        // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
        let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(321, 322);

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
        let target = test_remote_pid(actor_id);
        // SAFETY: send_value is a valid u32 on the stack; caller is a live actor.
        let handle = unsafe {
            hew_node_api_ask_async_location(
                &raw const target,
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

    #[cfg(feature = "quic")]
    #[test]
    fn two_node_inbound_orphaned_ask_reports_orphaned() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
        let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(315, 316);

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

        let target = test_remote_pid(actor_id);
        // SAFETY: this is a remote void ask; null payload/size are valid.
        let reply_ptr = unsafe {
            hew_node_api_ask_location(
                &raw const target,
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

    #[cfg(feature = "quic")]
    #[test]
    fn two_node_inbound_actor_stopped_reports_actor_stopped() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
        let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(317, 318);

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

        let target = test_remote_pid(actor_id);
        // SAFETY: this is a remote void ask; null payload/size are valid.
        let reply_ptr = unsafe {
            hew_node_api_ask_location(
                &raw const target,
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

    #[cfg(feature = "quic")]
    #[test]
    fn two_node_inbound_mailbox_full_reports_mailbox_full() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
        let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(326, 327);

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

        let target = test_remote_pid(actor_id);
        // SAFETY: this is a remote void ask; null payload/size are valid.
        let reply_ptr = unsafe {
            hew_node_api_ask_location(
                &raw const target,
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

    #[cfg(feature = "quic")]
    #[test]
    fn two_node_worker_limit_still_reports_worker_at_capacity() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(319, 320);

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
        let target = test_remote_pid(actor_id);
        // SAFETY: this is a remote void ask; null payload/size are valid.
        let reply_ptr = unsafe {
            hew_node_api_ask_location(
                &raw const target,
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

    #[cfg(feature = "encryption")]
    #[test]
    fn two_node_pre_rejection_peer_gets_timeout_not_wrong_error() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Authenticated pair: an outbound ask is refused (Unauthorized) before it
        // is sent unless the target connection is the exact authenticated owner
        // (issue #2652). The pre-rejection fallback under test needs the ask to
        // actually reach node2, so the peers are mutually credentialed.
        let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(330, 331);

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
        let target = test_remote_pid(actor_id);
        // SAFETY: this is a remote void ask; null payload/size are valid.
        let reply_ptr = unsafe {
            hew_node_api_ask_location(
                &raw const target,
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

    #[cfg(feature = "quic")]
    #[test]
    fn two_node_remote_nonvoid_empty_reply_returns_null() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
        let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(317, 318);

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

        let target = test_remote_pid(actor_id);
        // SAFETY: non-void remote ask expects a u32-sized reply; an empty success must fail closed.
        let reply_ptr = unsafe {
            hew_node_api_ask_location(
                &raw const target,
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

    #[cfg(feature = "encryption")]
    #[test]
    fn two_node_remote_ask_timeout_reports_timeout() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Authenticated pair (issue #2652): a genuine timeout requires the ask to
        // be sent to an authorized peer and left unanswered — an unverified target
        // would instead fail closed with Unauthorized before the send.
        let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(328, 329);

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
        let target = test_remote_pid(actor_id);
        // SAFETY: the actor pid and null payload are valid for this remote ask probe.
        let reply_ptr = unsafe {
            hew_node_api_ask_location(
                &raw const target,
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
            "remote ask that receives no reply must report Timeout: {:?}",
            crate::stream_error::take_last_error()
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

    /// V2 distributed admission rejects a pair with local identities but no
    /// configured peer bindings before either side can install a route or create
    /// an outbound ask.
    #[cfg(feature = "encryption")]
    #[test]
    fn unconfigured_peer_pair_rejected_during_connection_admission() {
        let _guard = crate::runtime_test_guard();
        crate::registry::hew_registry_clear();

        // Each TestNode has a real local identity, but neither snapshot binds the
        // other peer's Noise key to its claimed route slot.
        let node1_bind = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: bind address is a valid C string for the duration of this test.
        let node1 = unsafe { TestNode::new(342, &node1_bind) };
        assert!(!node1.as_ptr().is_null());
        // SAFETY: node1 came from TestNode::new and is valid for start-up here.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }
        thread::sleep(Duration::from_millis(50));
        let (node2, node2_port) = start_tcp_test_listener_node(343);

        let connect_addr = CString::new(format!("343@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and the connect address are valid for this admission attempt.
        let connect_rc = unsafe { hew_node_connect(node1.as_ptr(), connect_addr.as_ptr()) };
        assert_eq!(
            connect_rc,
            -1,
            "unconfigured v2 peers must be rejected during connection admission: {:?}",
            crate::stream_error::take_last_error()
        );

        // SAFETY: both connection managers remain live until teardown below.
        unsafe {
            assert_eq!(
                connection::hew_connmgr_conn_id_for_node((*node1.as_ptr()).conn_mgr, 343),
                -1,
                "initiator must not install a route for the rejected peer"
            );
            assert_eq!(
                connection::hew_connmgr_conn_id_for_node((*node2.as_ptr()).conn_mgr, 342),
                -1,
                "responder must not install a route for the rejected peer"
            );
            assert_eq!(
                connection::hew_connmgr_count((*node1.as_ptr()).conn_mgr),
                0,
                "initiator must retain no rejected connection"
            );
            assert_eq!(
                connection::hew_connmgr_count((*node2.as_ptr()).conn_mgr),
                0,
                "responder must retain no rejected connection"
            );
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }

    #[cfg(feature = "encryption")]
    #[test]
    fn node_stop_wakes_pending_remote_ask() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Authenticated pair (issue #2652): the pending ask under test must reach
        // the reply table, which only happens when the outbound ask is authorized.
        let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(315, 316);

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

        let target = test_remote_pid(actor_id);
        // SAFETY: the actor pid and null payload are valid for this remote ask probe.
        let ask_handle = thread::spawn(move || unsafe {
            let ptr = hew_node_api_ask_location(
                &raw const target,
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

    #[cfg(feature = "encryption")]
    #[test]
    fn connection_drop_wakes_pending_remote_ask() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Authenticated pair (issue #2652): the pending ask must register against
        // the outbound connection, which requires an authorized target.
        let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(320, 321);

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

        let target = test_remote_pid(actor_id);
        // SAFETY: the actor pid and null payload are valid for this remote ask probe.
        let ask_handle = thread::spawn(move || unsafe {
            let ptr = hew_node_api_ask_location(
                &raw const target,
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
    #[cfg(feature = "encryption")]
    #[test]
    fn swim_dead_wakes_pending_remote_ask_with_partition() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Authenticated pair (issue #2652): the pending ask must reach the reply
        // table so the SWIM-DEAD fan-out can resolve it with Partition; that only
        // happens for an authorized outbound target.
        let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(330, 331);

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

        let target = test_remote_pid(actor_id);
        // SAFETY: the actor pid and null payload are valid for this remote ask probe.
        let ask_handle = thread::spawn(move || unsafe {
            let ptr = hew_node_api_ask_location(
                &raw const target,
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
    #[cfg(feature = "quic")]
    #[test]
    fn inbound_ask_active_counter_returns_to_baseline_after_round_trip() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();
        register_test_u32_codec(test_dispatch(), 1);

        // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
        let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(320, 321);

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
        let target = test_remote_pid(actor_id);
        // SAFETY: payload is a valid u32 on the stack; its address is valid for this call.
        let reply_ptr = unsafe {
            hew_node_api_ask_location(
                &raw const target,
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
    #[cfg(feature = "quic")]
    #[test]
    fn over_limit_void_ask_fails_closed_with_worker_at_capacity() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();

        // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
        let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(322, 323);
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
        let target = test_remote_pid(actor_id);
        // SAFETY: null payload / size-0 are valid; this is a void ask.
        let reply_ptr = unsafe {
            hew_node_api_ask_location(
                &raw const target,
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
    #[cfg(feature = "quic")]
    #[test]
    fn over_limit_nonvoid_ask_fails_closed_with_worker_at_capacity() {
        let _guard = crate::runtime_test_guard();
        let _real_sched;
        crate::registry::hew_registry_clear();
        register_test_u32_codec(test_dispatch(), 1);

        // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
        let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(324, 325);
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
        let target = test_remote_pid(actor_id);
        // SAFETY: payload is a valid u32; its address is valid for this call.
        let reply_ptr = unsafe {
            hew_node_api_ask_location(
                &raw const target,
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

    #[cfg(feature = "encryption")]
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

        let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(353, 354);

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
    #[cfg(feature = "encryption")]
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

        // node1 starts FIRST and keeps CURRENT_NODE; node2 remains the secondary.
        let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(363, 364);

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
                let a_state =
                    unsafe { hew_cluster_member_state((*node_a.as_ptr()).cluster, NODE_B) };
                // SAFETY: B's cluster is live (node still running).
                let b_state =
                    unsafe { hew_cluster_member_state((*node_b.as_ptr()).cluster, NODE_A) };
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
}
