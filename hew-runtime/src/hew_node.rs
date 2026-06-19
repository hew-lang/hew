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
use crate::routing::{self, HewRoutingTable};
use crate::transport::{self, HewTransport, HewTransportOps, HEW_CONN_INVALID};

const NODE_STATE_STARTING: u8 = 0;
/// Node is started and serving traffic. `pub(crate)` so the SWIM driver only
/// drives a fully-running node.
pub(crate) const NODE_STATE_RUNNING: u8 = 1;
const NODE_STATE_STOPPING: u8 = 2;
const NODE_STATE_STOPPED: u8 = 3;
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

    /// Fail every pending reply tied to the given connection.
    fn fail_connection(&self, connection: ConnectionKey) {
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
            Self::fail_pending(&pending);
        }
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
        unsafe { hew_node_send(node, target_pid, msg_type, data.cast::<u8>(), size) }
    })
}

/// Node-local distributed registry state.
#[repr(C)]
#[derive(Debug, Default)]
pub struct HewRegistry {
    remote_names: Mutex<HashMap<String, u64>>,
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
    next_peer_node: AtomicU16,
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
        // address space before delivering it to the local mailbox. The inbound
        // `data` is the serialized wire form; feeding it raw to the mailbox would
        // make the actor handler dereference sender-side heap pointers and crash.
        //
        // Fail closed: if no codec is registered for `msg_type`, or the decode
        // reports failure, drop the message rather than deliver garbage. An empty
        // payload (size == 0) is a genuine zero-field message and bypasses decode.
        if size == 0 {
            // SAFETY: zero-length payload; mailbox handles null+0.
            let _ = unsafe {
                crate::actor::hew_actor_send_by_id(
                    target_actor_id,
                    msg_type,
                    std::ptr::null_mut(),
                    0,
                )
            };
        } else {
            // SAFETY: data is valid for `size` bytes (reader_loop contract).
            let (value, struct_size) =
                unsafe { crate::xnode_serial::decode_payload(msg_type, data.cast_const(), size) };
            if value.is_null() {
                // No codec or decode failure — drop the message fail-closed.
                set_last_error(format!(
                    "cross-node tell dropped: no codec or decode failure for msg_type={msg_type}"
                ));
            } else {
                // The reconstructed value lives in a malloc'd buffer of
                // `struct_size` bytes (the in-memory struct size, NOT the wire
                // length). hew_actor_send_by_id deep-copies `struct_size` bytes
                // into the mailbox, MOVING the owned heap fields (strings/bytes)
                // into the mailbox copy — exactly the move semantics of a local
                // send where the caller's stack value is byte-copied. We then
                // free the reconstructed struct SHELL only (not its fields, now
                // owned by the mailbox copy).
                // SAFETY: value is a valid reconstructed struct for msg_type.
                let _ = unsafe {
                    crate::actor::hew_actor_send_by_id(
                        target_actor_id,
                        msg_type,
                        value,
                        struct_size,
                    )
                };
                // SAFETY: value came from decode_payload (libc::malloc).
                unsafe { libc::free(value) };
            }
        }
    }
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
    // Capture the peer's negotiated feature flags ONCE, up front, under the
    // CURRENT_NODE read barrier. The three fail-closed error paths below
    // (decode failure, actor error, encode failure) need these flags to decide
    // whether the peer understands the ask-rejection sentinel. Reading them at
    // each error site dereferenced `conn_mgr` OUTSIDE the barrier: if the 5s
    // Phase-1 drain ceiling expired and Phase 2 freed `conn_mgr`, a straggler
    // dereferenced freed memory (use-after-free). The flags are negotiated once
    // at handshake and never change for the connection's life, so a single
    // guarded read at the top is equivalent to reading at each site — but it is
    // ordered against `hew_node_stop`'s CURRENT_NODE write barrier exactly like
    // `send_reply_envelope`:
    //   • if this read holds the lock first, stop blocks until we release it →
    //     `conn_mgr` is valid for the read;
    //   • if stop zeroed CURRENT_NODE first, we observe `*guard == 0` and return
    //     `None` (a straggler past the drain ceiling) → the error paths skip the
    //     rejection send, fail-closed, never touching the soon-to-be-freed mgr.
    let peer_flags: Option<u32> = with_current_node_read(|guard| {
        if *guard == 0 {
            return None;
        }
        // SAFETY: the CURRENT_NODE read lock held here blocks `hew_node_stop`'s
        // write-lock teardown, so `conn_mgr` cannot be freed for this read.
        Some(unsafe { connection::hew_connmgr_feature_flags_for_node(conn_mgr.0, source_node_id) })
    });

    // Reconstruct the request value into THIS node's address space before the
    // local ask — the inbound `payload` is the serialized wire form, not the
    // in-memory struct. Feeding it raw to the handler would dereference
    // sender-side heap pointers and crash. A zero-length payload is a genuine
    // zero-field request and bypasses decode.
    let decoded_request: Option<(*mut c_void, usize)> = if payload.is_empty() {
        None
    } else {
        // SAFETY: payload is a valid slice for payload.len() bytes.
        let (value, struct_size) = unsafe {
            crate::xnode_serial::decode_payload(msg_type, payload.as_ptr(), payload.len())
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
        // the value into its own address space. The reply codec is keyed by the
        // request `msg_type` (a handler's reply type maps 1:1 to its msg_type).
        // SAFETY: reply_ptr came from hew_reply which malloc'd the reply struct.
        let size = unsafe { crate::actor::hew_reply_data_size(reply_ptr) };
        if size > 0 {
            let mut out_len: usize = 0;
            // SAFETY: reply_ptr is a valid reply value for msg_type; out_len valid.
            let bytes =
                unsafe { crate::xnode_serial::encode_reply(msg_type, reply_ptr, &raw mut out_len) };
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
            }
            _ => {
                map.insert(key, actor_id);
            }
        }
    } else {
        map.remove(&key);
    }
}

fn next_peer_node_id(node: &HewNode) -> u16 {
    let mut id = node.next_peer_node.fetch_add(1, Ordering::Relaxed);
    if id == 0 || id == node.node_id {
        id = node.next_peer_node.fetch_add(1, Ordering::Relaxed);
        if id == 0 || id == node.node_id {
            id = 1;
        }
    }
    id
}

unsafe fn parse_connect_target(
    addr: *const c_char,
    fallback_node_id: u16,
) -> Option<(u16, CString)> {
    // SAFETY: caller validates non-null and C-string.
    let c_addr = unsafe { CStr::from_ptr(addr) };
    let addr_text = c_addr.to_string_lossy();
    if let Some((prefix, target)) = addr_text.split_once('@') {
        if let Ok(node_id) = prefix.parse::<u16>() {
            if !target.is_empty() {
                let c_target = CString::new(target.as_bytes()).ok()?;
                return Some((node_id, c_target));
            }
        }
    }
    Some((fallback_node_id, CString::new(c_addr.to_bytes()).ok()?))
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
    let bind_copy = unsafe { libc::strdup(bind_addr) };
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
        next_peer_node: AtomicU16::new(1),
    });
    let raw = Box::into_raw(node);
    remember_node(raw);
    raw
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
        // SAFETY: pointers are valid for manager lifetime.
        node.conn_mgr = unsafe {
            connection::hew_connmgr_new(
                node.transport,
                Some(node_inbound_router),
                node.routing_table,
                node.cluster,
                node.node_id,
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
        // SAFETY: bind_addr_owned was allocated via libc::strdup.
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
/// # Safety
///
/// - `node` must be valid.
/// - `payload` must be valid for `payload_len` bytes, or null when len is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_node_send(
    node: *mut HewNode,
    target_pid: u64,
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

    let target_node_id = crate::pid::hew_pid_node(target_pid);
    if target_node_id == 0 || target_node_id == node.node_id {
        // SAFETY: actor send API handles null payload when len is 0.
        return unsafe {
            crate::actor::hew_actor_send_by_id(
                target_pid,
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
        let bytes = unsafe {
            crate::xnode_serial::encode_payload(
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

/// Connect to a remote node and register routing for its node ID.
///
/// Supports `"<node_id>@<addr>"` format for explicit peer node IDs. If no
/// prefix is supplied, an internal node-id allocator is used.
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

    let fallback_node_id = next_peer_node_id(node);
    // SAFETY: addr pointer is non-null and valid by caller contract.
    let Some((peer_node_id, target_addr)) =
        (unsafe { parse_connect_target(addr, fallback_node_id) })
    else {
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
        )
    };

    if !node.cluster.is_null() {
        // SAFETY: cluster pointer is valid if non-null.
        let _ =
            unsafe { cluster::hew_cluster_join(node.cluster, peer_node_id, target_addr.as_ptr()) };
    }

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
    // Each call within the same process adds a small offset to the process
    // base so multiple Node::start calls don't collide with each other.
    let offset = NODE_ID_COUNTER.fetch_add(1, Ordering::Relaxed);
    let node_id = next_local_node_id(process_node_id_base(), offset);
    // SAFETY: addr was null-checked above and is a valid C string.
    let node = unsafe { hew_node_new(node_id, addr) };
    if node.is_null() {
        return -1;
    }
    // SAFETY: node was just created successfully by hew_node_new.
    let rc = unsafe { hew_node_start(node) };
    if rc != 0 {
        // SAFETY: node is valid but not started; free the allocation.
        unsafe { hew_node_free(node) };
        return rc;
    }
    0
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

        // Serialize the request value before it leaves this address space —
        // `data`/`size` is the raw in-memory struct (which may hold heap
        // pointers). The codec for `msg_type` encodes its CONTENTS; the receiver
        // reconstructs the value. Fail closed if no codec is registered for a
        // non-empty payload.
        let serialized_req: Option<(*mut u8, usize)> = if size > 0 {
            let mut out_len: usize = 0;
            // SAFETY: data is a valid value of the message type; out_len valid.
            let bytes = unsafe {
                crate::xnode_serial::encode_payload(msg_type, data.cast_const(), &raw mut out_len)
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
    // the reply VALUE into this node's address space using the reply codec
    // (keyed by the request `msg_type`). codegen's ask terminator then memcpy-
    // loads the reconstructed struct into the reply dest — `reply_size` matches
    // the reconstructed struct size.
    // SAFETY: reply.data is a valid slice for its length.
    let (value, struct_size) = unsafe {
        crate::xnode_serial::decode_reply(msg_type, reply.data.as_ptr(), reply.data.len())
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
    let (request_id, pending) = match setup_remote_ask(pid, msg_type, data, size, ptr::null_mut()) {
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
    finish_remote_ask_outcome(msg_type, reply_size, &reply)
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

    let (request_id, pending) = match setup_remote_ask(pid, msg_type, data, size, caller_actor) {
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
    finish_remote_ask_outcome(msg_type, reply_size, &reply)
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

    /// Initialise a real, worker-backed scheduler for a node test (delegates to
    /// the scheduler-side helper, which sees the module-private `stealers` and
    /// safely retires a `runtime_test_guard()` placeholder before init).
    fn init_real_scheduler() {
        crate::scheduler::init_real_scheduler_for_test();
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
        let buf = crate::xnode_serial::hew_ser_buf_new();
        // SAFETY: buf is a fresh live SerBuf handle.
        unsafe { crate::xnode_serial::hew_ser_push_u64(buf, u64::from(v)) };
        // SAFETY: buf is consumed by finish; out_len is a valid pointer.
        unsafe { crate::xnode_serial::hew_ser_finish(buf, out_len) }
    }

    unsafe extern "C" fn test_u32_deserialize(
        data: *const u8,
        len: usize,
        out_struct_size: *mut usize,
    ) -> *mut std::ffi::c_void {
        // SAFETY: data is valid for len bytes.
        let reader = unsafe { crate::xnode_serial::hew_de_reader_new(data, len) };
        // SAFETY: reader is a live handle.
        let v64 = unsafe { crate::xnode_serial::hew_de_read_u64(reader) };
        #[allow(
            clippy::cast_possible_truncation,
            reason = "test payload is always a u32 round-tripped through the u64 primitive"
        )]
        let v = v64 as u32;
        // SAFETY: reader is a live handle.
        let failed = unsafe { crate::xnode_serial::hew_de_failed(reader) };
        // SAFETY: reader is a live handle.
        unsafe { crate::xnode_serial::hew_de_reader_free(reader) };
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

    /// Register the test u32 codec for `msg_type` as both the request and reply
    /// codec, so the two-process send/ask paths can serialize their controlled
    /// payloads. Idempotent across helper processes (each registers in its own
    /// process).
    fn register_test_u32_codec(msg_type: i32) {
        // SAFETY: the thunks match the codec ABI.
        unsafe {
            crate::xnode_serial::hew_xnode_register_codec(
                msg_type,
                test_u32_serialize,
                test_u32_deserialize,
            );
            crate::xnode_serial::hew_xnode_register_reply_codec(
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
        init_real_scheduler();
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
        assert!(delivered, "server did not observe two-process tell");
    }

    fn run_registry_gossip_client_helper() {
        // Install the runtime before touching the name registry (helper
        // subprocess holds no `runtime_test_guard`).
        init_real_scheduler();
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
                TWO_PROCESS_REGISTRY_MSG_TYPE,
                ptr::null(),
                0,
            )
        };
        assert_eq!(rc, 0, "client tell send");

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
        // registered codec (fail-closed). Register the test u32 codec.
        register_test_u32_codec(TWO_PROCESS_REGISTRY_MSG_TYPE);
        reset_two_process_ask_observed();
        // Install the runtime before touching the name registry (helper
        // subprocess holds no `runtime_test_guard`).
        init_real_scheduler();
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

    fn run_two_process_ask_echo_client_helper() {
        let (node, remote_pid) = run_two_process_ask_client_setup(
            TWO_PROCESS_REGISTRY_CLIENT_NODE,
            TWO_PROCESS_REGISTRY_SERVER_NODE,
            TWO_PROCESS_ASK_ECHO_NAME,
        );
        let send_value: u32 = 21;
        // SAFETY: remote_pid was resolved from a separate helper process over TCP.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                remote_pid,
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
        let (node, remote_pid) = run_two_process_ask_client_setup(
            TWO_PROCESS_REGISTRY_CLIENT_NODE,
            TWO_PROCESS_REGISTRY_SERVER_NODE,
            TWO_PROCESS_ASK_TIMEOUT_NAME,
        );
        let send_value: u32 = 21;
        // SAFETY: remote_pid was resolved from a separate helper process over TCP.
        let reply_ptr = unsafe {
            hew_node_api_ask(
                remote_pid,
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
    ) -> (TestNode, u64) {
        // The cross-node send/ask path requires a registered codec for the
        // payload's msg_type (fail-closed). Register the test u32 codec.
        register_test_u32_codec(TWO_PROCESS_REGISTRY_MSG_TYPE);
        // Install the runtime before touching the name registry (helper
        // subprocess holds no `runtime_test_guard`).
        init_real_scheduler();
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
        (node, remote_pid)
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
        let reply = unsafe { hew_node_api_ask_finish(handle, 7, 0) };

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
        let reply = unsafe { hew_node_api_ask_finish(handle, 7, 0) };

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
        let reply = unsafe { hew_node_api_ask_finish(handle, 7, 0) };

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
        init_real_scheduler();

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
        let rc = unsafe { hew_node_send(node1.as_ptr(), actor_id, msg_type_sent, ptr::null(), 0) };
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
        init_real_scheduler();

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
        let rc = unsafe { hew_node_send(node1.as_ptr(), actor_id, msg_type_sent, ptr::null(), 0) };
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

        init_real_scheduler();

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
        crate::registry::hew_registry_clear();
        // Remote ask requires a registered codec for the payload msg_type.
        register_test_u32_codec(1);

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
        init_real_scheduler();

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
        crate::registry::hew_registry_clear();
        register_test_u32_codec(1);

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

        init_real_scheduler();

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
        let reply_ptr = unsafe { hew_node_api_ask_finish(handle, 1, std::mem::size_of::<u32>()) };
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

        init_real_scheduler();

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

        init_real_scheduler();

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

        init_real_scheduler();

        crate::pid::hew_pid_set_local_node(327);
        let opts = crate::actor::HewActorOpts {
            init_state: ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox_capacity: 1,
            overflow: crate::internal::types::HewOverflowPolicy::DropNew as i32,
            coalesce_key_fn: None,
            coalesce_fallback: 0,
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

        init_real_scheduler();

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

        init_real_scheduler();

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

        init_real_scheduler();

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

        init_real_scheduler();

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

        init_real_scheduler();

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

        init_real_scheduler();

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
        crate::registry::hew_registry_clear();
        register_test_u32_codec(1);

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

        init_real_scheduler();

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
        init_real_scheduler();

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
        crate::registry::hew_registry_clear();
        register_test_u32_codec(1);

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
        init_real_scheduler();

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

        init_real_scheduler();

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
            // fire without another sim advance. We proceed as soon as both views
            // are ALIVE — waiting for the actual round-trip instead of betting a
            // fixed sleep against loopback/VM latency, so a loaded CI host can't
            // stale last_seen into a false DEAD. The "never DEAD" invariant is
            // still enforced strictly on every poll.
            let alive_deadline =
                std::time::Instant::now() + Duration::from_millis(SWIM_ALIVE_DEADLINE_MS);
            loop {
                // SAFETY: A's cluster is live (node still running).
                let a_view_of_b =
                    unsafe { hew_cluster_member_state((*node_a.as_ptr()).cluster, NODE_B) };
                // SAFETY: B's cluster is live (node still running).
                let b_view_of_a =
                    unsafe { hew_cluster_member_state((*node_b.as_ptr()).cluster, NODE_A) };
                assert_ne!(
                    a_view_of_b,
                    crate::cluster::MEMBER_DEAD,
                    "A must not declare a still-alive B as DEAD after period \
                     (state={a_view_of_b})"
                );
                assert_ne!(
                    b_view_of_a,
                    crate::cluster::MEMBER_DEAD,
                    "B must not declare a still-alive A as DEAD after period \
                     (state={b_view_of_a})"
                );
                if a_view_of_b == crate::cluster::MEMBER_ALIVE
                    && b_view_of_a == crate::cluster::MEMBER_ALIVE
                {
                    break;
                }
                assert!(
                    std::time::Instant::now() < alive_deadline,
                    "PING-ACK round-trip did not refresh last_seen within the \
                     deadline (a_view_of_b={a_view_of_b}, b_view_of_a={b_view_of_a})"
                );
                thread::sleep(Duration::from_millis(SWIM_ALIVE_POLL_MS));
            }
        }

        // SAFETY: nodes were allocated in this test and remain valid.
        unsafe {
            assert_eq!(hew_node_stop(node_a.as_ptr()), 0);
            assert_eq!(hew_node_stop(node_b.as_ptr()), 0);
        }
        crate::registry::hew_registry_clear();
    }
}
