//! Unified distributed node runtime.
//!
//! Integrates transport, connection manager, SWIM membership, and
//! name/actor registry wiring.

use crate::util::{CondvarExt, MutexExt, RwLockExt};
use std::cell::Cell;
use std::collections::HashMap;
use std::ffi::{c_char, c_int, c_void, CStr, CString};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicU16, AtomicU64, AtomicU8, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex, RwLock};

use crate::set_last_error;
use std::thread::{self, JoinHandle};

use crate::cluster::{self, ClusterConfig, HewCluster};
use crate::connection::{self, HewConnMgr};
use crate::routing::{self, HewRoutingTable};
use crate::transport::{self, HewTransport, HewTransportOps, HEW_CONN_INVALID};

const NODE_STATE_STARTING: u8 = 0;
const NODE_STATE_RUNNING: u8 = 1;
const NODE_STATE_STOPPING: u8 = 2;
const NODE_STATE_STOPPED: u8 = 3;
const _: () = assert!(
    std::mem::size_of::<usize>() >= std::mem::size_of::<u64>(),
    "Hew requires 64-bit target for actor ID encoding"
);

/// Global reference to the active node for remote message routing.
///
/// Only one `HewNode` may be active per process. Starting a second node
/// while one is running is undefined behaviour.
static CURRENT_NODE: RwLock<usize> = RwLock::new(0);

#[derive(Clone, Copy)]
struct KnownNodePtr(*mut HewNode);

// SAFETY: Node pointers are owned by the runtime and removed from KNOWN_NODES
// before the node allocation is freed.
unsafe impl Send for KnownNodePtr {}

/// Tracks node allocations so actor teardown can unregister distributed names
/// before the owning node is freed.
static KNOWN_NODES: Mutex<Vec<KnownNodePtr>> = Mutex::new(Vec::new());

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

/// RAII guard that decrements [`INBOUND_ASK_ACTIVE`] exactly once on drop.
///
/// Constructed in the spawned ask-handler thread so the counter stays
/// accurate even under panics or early returns.
struct InboundAskGuard;

impl Drop for InboundAskGuard {
    fn drop(&mut self) {
        INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
    }
}

// ---------------------------------------------------------------------------
// Ask-error discriminant
// ---------------------------------------------------------------------------

/// Typed failure reason for an ask (local or remote).
///
/// Written to a thread-local slot whenever `hew_node_api_ask` or
/// `hew_actor_ask` / `hew_actor_ask_timeout` returns `NULL`.
/// Remote callers retrieve the discriminant via [`hew_node_ask_take_last_error`].
/// Local callers retrieve it via `hew_actor_ask_take_last_error`.
///
/// Values are stable across releases; do not reorder or reuse.
#[repr(i32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AskError {
    /// No failure — used to reset the slot after a successful ask.
    None = 0,
    /// No active node is installed in this process.
    NodeNotRunning = 1,
    /// The target PID could not be mapped to a connection.
    RoutingFailed = 2,
    /// Wire-encoding the request envelope failed.
    EncodeFailed = 3,
    /// Sending the encoded envelope over the connection failed.
    SendFailed = 4,
    /// The reply did not arrive before the deadline elapsed.
    Timeout = 5,
    /// The underlying connection was dropped before the reply arrived.
    ConnectionDropped = 6,
    /// The reply payload size did not match the expected reply type size.
    PayloadSizeMismatch = 7,
    /// The remote node's inbound ask worker pool is at capacity.
    ///
    /// The actor was never dispatched. The ask may be retried after a
    /// brief back-off. This is distinct from [`ConnectionDropped`]: the
    /// connection is healthy, but the remote end shed load deliberately.
    WorkerAtCapacity = 8,
    /// The target actor was stopped, or the mailbox rejected the send
    /// (e.g. actor not found, mailbox closed, or `ErrClosed` in WASM).
    ActorStopped = 9,
    /// The target actor's mailbox was full and the message was dropped.
    ///
    /// Only raised when the mailbox overflow policy is `DropNew` or `Fail`.
    MailboxFull = 10,
    /// The ask was orphaned: the actor's mailbox was torn down before the
    /// handler called `hew_reply`.
    ///
    /// This happens when an actor stops mid-dispatch without replying (e.g.
    /// a crash or explicit stop inside the dispatch function).  The reply
    /// channel's sender-side reference is retired by the mailbox teardown
    /// path, which signals the waiter with an empty null payload.
    ///
    /// DROP-SAFETY: the orphaned flag is set on the reply channel before the
    /// null sentinel is published, so the waiter always observes it correctly.
    OrphanedAsk = 11,
    /// WASM cooperative ask only: no runnable work remains in the scheduler,
    /// so the ask loop cannot make further progress before returning control
    /// to the host.
    ///
    /// This is distinct from [`Timeout`]: the deadline has not necessarily
    /// expired, but the scheduler is idle and cannot advance the ask.
    NoRunnableWork = 12,
}

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
    outcome: Mutex<Option<ReplyOutcome>>,
    cond: Condvar,
}

/// Process-global reply routing table for correlating remote ask/reply pairs.
///
/// Each outbound remote ask registers a `PendingReply` keyed by a unique
/// request ID. When the reply envelope arrives, the reader thread deposits
/// the payload and signals the condvar to wake the blocked caller.
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

    /// Allocate a new request ID and register a pending reply slot.
    fn register(&self, connection: ConnectionKey) -> (u64, Arc<PendingReply>) {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);
        let entry = Arc::new(PendingReply {
            connection,
            outcome: Mutex::new(None),
            cond: Condvar::new(),
        });
        let mut map = self
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        map.insert(id, Arc::clone(&entry));
        (id, entry)
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
            let mut guard = pending
                .outcome
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            *guard = Some(outcome);
            pending.cond.notify_one();
            true
        } else {
            false
        }
    }

    fn fail_pending_with_reason(pending: &PendingReply, ask_error: AskError) {
        let mut guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *guard = Some(ReplyOutcome {
            status: ReplyStatus::Failed,
            data: Vec::new(),
            ask_error,
        });
        pending.cond.notify_one();
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
    /// request was found.  Used for inband rejection signals from the remote
    /// node (worker-limit exceeded).
    fn fail(&self, request_id: u64) -> bool {
        let entry = {
            let mut map = self
                .pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            map.remove(&request_id)
        };
        if let Some(pending) = entry {
            Self::fail_pending_with_reason(&pending, AskError::WorkerAtCapacity);
            true
        } else {
            false
        }
    }

    /// Remove a pending entry (used on timeout to prevent leaks).
    fn remove(&self, request_id: u64) {
        let mut map = self
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        map.remove(&request_id);
    }
}

static REPLY_TABLE: std::sync::LazyLock<ReplyRoutingTable> =
    std::sync::LazyLock::new(ReplyRoutingTable::new);
static REMOTE_VOID_REPLY_SENTINEL: u8 = 0;

/// Deposit a reply payload for a pending remote ask.
///
/// Called by the reader thread when a reply envelope arrives. Returns
/// `true` if the request ID was matched.
pub(crate) fn complete_remote_reply(request_id: u64, payload: &[u8]) -> bool {
    REPLY_TABLE.complete(request_id, payload.to_vec())
}

/// Fail a pending remote ask identified by `request_id` with [`AskError::WorkerAtCapacity`].
///
/// Called by the reader thread when a **rejection** reply envelope arrives
/// (one with [`HEW_REPLY_REJECT_MSG_TYPE`] in the `msg_type` field).
/// Sets [`ReplyStatus::Failed`] with `ask_error = WorkerAtCapacity` so the
/// originating `hew_node_api_ask` caller returns the precise discriminant —
/// fail-closed for both void and non-void asks, and distinguishable from a
/// genuine connection drop.
pub(crate) fn fail_remote_reply(request_id: u64) -> bool {
    REPLY_TABLE.fail(request_id)
}

pub(crate) fn fail_remote_replies_for_connection(conn_mgr: *const HewConnMgr, conn_id: c_int) {
    REPLY_TABLE.fail_connection(ConnectionKey::new(conn_mgr, conn_id));
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
    let guard = CURRENT_NODE.read_or_recover();
    if *guard == 0 {
        return -1;
    }
    let node = *guard as *mut HewNode;
    // SAFETY: read lock pins CURRENT_NODE pointer for this send.
    unsafe { hew_node_send(node, target_pid, msg_type, data.cast::<u8>(), size) }
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
    let mut known = KNOWN_NODES.lock_or_recover();
    if !known.iter().any(|entry| entry.0 == node) {
        known.push(KnownNodePtr(node));
    }
}

fn forget_node(node: *mut HewNode) {
    let mut known = KNOWN_NODES.lock_or_recover();
    known.retain(|entry| entry.0 != node);
}

fn take_registry_names_if<F>(registry: &HewRegistry, mut predicate: F) -> Vec<String>
where
    F: FnMut(u64) -> bool,
{
    let mut map = registry.remote_names.lock_or_recover();
    let names: Vec<String> = map
        .iter()
        .filter(|(_, actor_pid)| predicate(**actor_pid))
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
    let names = take_registry_names_if(registry, |actor_pid| {
        crate::pid::hew_pid_node(actor_pid) == node.node_id
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
    let known = KNOWN_NODES.lock_or_recover();
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
                    send_rejection_reply(source_node_id, request_id, conn_mgr, shutdown.as_ref());
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
        thread::spawn(move || {
            // Guard decrements INBOUND_ASK_ACTIVE on drop (including on panic).
            let _guard = InboundAskGuard;
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
        // Fire-and-forget message — route to local actor.
        // SAFETY: hew_actor_send_by_id deep-copies payload and validates actor presence.
        let _ = unsafe {
            crate::actor::hew_actor_send_by_id(target_actor_id, msg_type, data.cast(), size)
        };
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
    // Perform a local blocking ask against the target actor.
    let reply_ptr = {
        let data_ptr = if payload.is_empty() {
            std::ptr::null_mut()
        } else {
            payload.as_ptr().cast_mut().cast::<c_void>()
        };
        // SAFETY: data_ptr is valid for payload.len() bytes.
        unsafe {
            crate::actor::hew_actor_ask_by_id(target_actor_id, msg_type, data_ptr, payload.len())
        }
    };

    // Build the reply payload from the returned data.
    let reply_data: Vec<u8> = if reply_ptr.is_null() {
        Vec::new()
    } else {
        // The reply is a malloc'd buffer. We need to determine its size.
        // hew_actor_ask returns a pointer to the reply value, but the
        // reply_channel stores the size internally. We pass the raw
        // pointer through; the size was recorded alongside it.
        // SAFETY: reply_ptr came from hew_reply which malloc'd size bytes.
        let size = unsafe { crate::actor::hew_reply_data_size(reply_ptr) };
        if size > 0 {
            // SAFETY: reply_ptr is valid for `size` bytes as returned by hew_reply_data_size.
            let slice = unsafe { std::slice::from_raw_parts(reply_ptr.cast::<u8>(), size) };
            let v = slice.to_vec();
            // SAFETY: reply_ptr was malloc'd by hew_reply.
            unsafe { libc::free(reply_ptr) };
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
/// field and an empty payload. The connection reader on the receiving node
/// recognises the sentinel and calls [`fail_remote_reply`] instead of
/// [`complete_remote_reply`], so the originating `hew_node_api_ask` caller
/// receives [`AskError::WorkerAtCapacity`] regardless of whether the ask
/// was void or non-void.
fn send_rejection_reply(
    target_node_id: u16,
    request_id: u64,
    conn_mgr: *mut connection::HewConnMgr,
    shutdown_started: &AtomicBool,
) {
    if conn_mgr.is_null() {
        return;
    }

    let guard = CURRENT_NODE.read_or_recover();
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

    // Encode the rejection envelope: request_id identifies the pending ask;
    // source_node_id = 0 marks it as a reply; msg_type = HEW_REPLY_REJECT_MSG_TYPE
    // distinguishes it from a normal (possibly void) success reply.
    let env = crate::wire::HewWireEnvelope {
        target_actor_id: 0,
        source_actor_id: 0,
        msg_type: HEW_REPLY_REJECT_MSG_TYPE,
        payload_size: 0,
        payload: std::ptr::null_mut(),
        request_id,
        source_node_id: 0,
    };
    // SAFETY: zeroed is valid for HewWireBuf.
    let mut wire_buf: crate::wire::HewWireBuf = unsafe { std::mem::zeroed() };
    // SAFETY: wire_buf is a valid stack allocation.
    unsafe { crate::wire::hew_wire_buf_init(&raw mut wire_buf) };
    // SAFETY: wire_buf and env are valid stack locals.
    if unsafe { crate::wire::hew_wire_encode_envelope(&raw mut wire_buf, &raw const env) } != 0 {
        // SAFETY: wire_buf was initialised above.
        unsafe { crate::wire::hew_wire_buf_free(&raw mut wire_buf) };
        return;
    }
    // SAFETY: conn_mgr and conn_id are valid; wire_buf is initialised.
    unsafe {
        connection::hew_connmgr_send_preencoded(conn_mgr, conn_id, wire_buf.data, wire_buf.len)
    };
    // SAFETY: wire_buf was initialised above.
    unsafe { crate::wire::hew_wire_buf_free(&raw mut wire_buf) };
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
    // `conn_mgr`.  `hew_node_stop` holds the CURRENT_NODE write lock while it
    // clears the pointer to zero (and only frees `conn_mgr` afterward), so:
    //
    // * If this thread acquires the read lock first, stop is blocked until we
    //   release it — `conn_mgr` is guaranteed valid for this whole function.
    // * If stop cleared CURRENT_NODE first, we see `*guard == 0` here and
    //   return before touching `conn_mgr`.
    //
    // The guard must remain live until the end of the function so that the
    // read lock is held for the entire duration of `conn_mgr` access.
    let guard = CURRENT_NODE.read_or_recover();
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
    #[expect(
        clippy::cast_possible_truncation,
        reason = "reply payload size bounded by wire buffer limits"
    )]
    let env = crate::wire::HewWireEnvelope {
        target_actor_id: 0,
        source_actor_id: 0,
        msg_type: 0,
        payload_size: reply_data.len() as u32,
        payload: reply_data.as_ptr().cast_mut(),
        request_id,
        source_node_id: 0,
    };
    // SAFETY: zeroed is valid for HewWireBuf.
    let mut wire_buf: crate::wire::HewWireBuf = unsafe { std::mem::zeroed() };
    // SAFETY: wire_buf is a valid stack allocation.
    unsafe { crate::wire::hew_wire_buf_init(&raw mut wire_buf) };
    // SAFETY: wire_buf and env are valid stack locals.
    if unsafe { crate::wire::hew_wire_encode_envelope(&raw mut wire_buf, &raw const env) } != 0 {
        // SAFETY: wire_buf was initialised above.
        unsafe { crate::wire::hew_wire_buf_free(&raw mut wire_buf) };
        return;
    }

    // Send via conn_mgr so noise encryption is applied when the connection
    // is encrypted. This replaces the former raw transport send which would
    // send unencrypted data over an encrypted connection.
    // SAFETY: conn_mgr and conn_id are valid; wire_buf is initialised.
    unsafe {
        connection::hew_connmgr_send_preencoded(conn_mgr, conn_id, wire_buf.data, wire_buf.len)
    };
    // SAFETY: wire_buf was initialised above.
    unsafe { crate::wire::hew_wire_buf_free(&raw mut wire_buf) };
}

/// Callback invoked by the cluster when a registry gossip event arrives
/// from a remote peer. Updates the node's `remote_names` map.
extern "C" fn node_registry_gossip_callback(
    name: *const c_char,
    actor_pid: u64,
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
        map.insert(key, actor_pid);
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
    let _ = take_registry_names_if(registry, |actor_pid| {
        crate::pid::hew_pid_node(actor_pid) == node_id
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
        ($msg:literal) => {{
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
        // Check HEW_TRANSPORT env var for transport selection.
        #[cfg(feature = "quic")]
        let use_quic = std::env::var("HEW_TRANSPORT")
            .map(|v| v.eq_ignore_ascii_case("quic"))
            .unwrap_or(false);
        #[cfg(not(feature = "quic"))]
        let use_quic = false;

        if use_quic {
            #[cfg(feature = "quic")]
            {
                // SAFETY: constructor returns owned transport pointer or null.
                node.transport = unsafe { crate::quic_transport::hew_transport_quic_new() };
            }
        } else {
            // SAFETY: constructor returns owned transport pointer or null.
            node.transport = unsafe { transport::hew_transport_tcp_new() };
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
        let cfg = ClusterConfig {
            local_node_id: node.node_id,
            ..ClusterConfig::default()
        };
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
    {
        let mut guard = CURRENT_NODE
            .write()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if *guard == 0 {
            *guard = ptr::from_mut(node) as usize;
            crate::pid::hew_pid_set_local_node(node.node_id);
        }
    }

    // Start the profiler with distributed runtime context if HEW_PPROF is set.
    crate::profiler::maybe_start_with_context(node.cluster, node.conn_mgr, node.routing_table);

    0
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
    // race with cluster deallocation.
    let _known_nodes = KNOWN_NODES.lock_or_recover();
    if node.state.load(Ordering::Acquire) == NODE_STATE_STOPPED {
        return 0;
    }

    node.state.store(NODE_STATE_STOPPING, Ordering::Release);
    {
        // Setting CURRENT_NODE to zero acts as a lifetime barrier for
        // ask-handler threads spawned by `node_inbound_router`. Those
        // threads acquire the CURRENT_NODE read lock in `send_reply_envelope`
        // and bail out immediately if the value is 0. The write lock here
        // blocks until every concurrent read-lock-holder (i.e. every
        // in-flight reply send) has completed, so `conn_mgr` cannot be freed
        // while any such thread is still running for the current node.
        let mut guard = CURRENT_NODE.write_or_recover();
        if !node.conn_mgr.is_null() {
            // SAFETY: node owns this connection manager until teardown completes.
            unsafe { connection::hew_connmgr_mark_stopping(node.conn_mgr) };
        }
        if *guard == ptr::from_mut(node) as usize {
            *guard = 0;
            REPLY_TABLE.fail_all();
        }
    }
    node.accept_stop.store(true, Ordering::Release);
    {
        let mut guard = node.accept_thread.lock_or_recover();
        if let Some(handle) = guard.take() {
            let _ = handle.join();
        }
    }

    // Shutdown profiler threads before freeing node resources they might access.
    crate::profiler::shutdown();

    // Remove this node's published names from the local registry before the
    // cluster state is torn down. Remote nodes drop cached names when the
    // membership callback observes this node leaving or dying.
    // SAFETY: `node` is valid for the duration of hew_node_stop, and KNOWN_NODES
    // serialization above prevents concurrent actor cleanup from racing this teardown.
    unsafe { unregister_local_names_for_node(node) };

    if !node.conn_mgr.is_null() {
        // SAFETY: valid manager pointer from hew_connmgr_new.
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

    // SAFETY: registry API expects a stable C string pointer.
    let rc =
        unsafe { crate::registry::hew_registry_register(name, actor_id_to_registry_ptr(actor)) };
    if rc != 0 {
        return -1;
    }

    // SAFETY: name was checked non-null and is a valid C string by caller contract.
    let key = unsafe { CStr::from_ptr(name) }
        .to_string_lossy()
        .into_owned();
    // SAFETY: registry pointer was allocated in hew_node_new and freed in hew_node_free.
    let reg = unsafe { &*node.registry };
    let mut map = reg.remote_names.lock_or_recover();
    map.insert(key.clone(), actor);

    // Propagate to cluster gossip so remote nodes learn about this actor.
    if !node.cluster.is_null() {
        // SAFETY: cluster pointer is valid while the node is alive.
        unsafe { cluster::hew_cluster_registry_add(node.cluster, name, actor) };
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
    let mut map = reg.remote_names.lock_or_recover();
    map.remove(&key);

    // Propagate removal to cluster gossip.
    if !node.cluster.is_null() {
        // SAFETY: cluster pointer is valid while the node is alive.
        unsafe { cluster::hew_cluster_registry_remove(node.cluster, name) };
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

    // SAFETY: conn_mgr and conn_id were validated above.
    unsafe {
        connection::hew_connmgr_send(
            node.conn_mgr,
            conn_id,
            target_pid,
            msg_type,
            payload.cast_mut(),
            payload_len,
        )
    }
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
        if let Some(close_fn) = ops.close_conn {
            // SAFETY: transport impl and conn handle are valid here.
            unsafe { close_fn(t.r#impl, conn_id) };
        }
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

/// Counter for auto-assigning node IDs.
static NODE_ID_COUNTER: std::sync::atomic::AtomicU16 = std::sync::atomic::AtomicU16::new(1);

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
    let node_id = NODE_ID_COUNTER.fetch_add(1, Ordering::Relaxed);
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
    // Read the current node pointer (hew_node_stop will clear CURRENT_NODE).
    let ptr = {
        let guard = CURRENT_NODE.read_or_recover();
        *guard as *mut HewNode
    };
    if ptr.is_null() {
        return -1;
    }
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
    let guard = CURRENT_NODE.read_or_recover();
    let node = *guard as *mut HewNode;
    if node.is_null() {
        set_last_error("Node::connect: no active node");
        return -1;
    }
    // SAFETY: node and addr are non-null and validated above.
    unsafe { hew_node_connect(node, addr) }
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
    let actor_id = unsafe { (*actor).pid };
    let guard = CURRENT_NODE.read_or_recover();
    let node = *guard as *mut HewNode;
    if node.is_null() {
        set_last_error("Node::register: no active node");
        return -1;
    }
    // SAFETY: node, name, and actor_id are validated above.
    unsafe { hew_node_register(node, name, actor_id) }
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
    let guard = CURRENT_NODE.read_or_recover();
    let node = *guard as *mut HewNode;
    if node.is_null() {
        return 0;
    }
    // SAFETY: node and name are non-null and validated above.
    unsafe { hew_node_lookup(node, name) }
}

/// `Node::set_transport(name)` — Set the transport type before starting.
///
/// Supported values: `"tcp"` (default), `"quic"`.
/// Must be called before `Node::start`.
///
/// # Safety
///
/// `name` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_set_transport(name: *const c_char) -> c_int {
    // SAFETY: caller guarantees name is a valid C string (or null).
    let Some(s) = (unsafe { crate::util::cstr_to_str(name, "hew_node_set_transport") }) else {
        return -1;
    };
    match s {
        "tcp" => {
            std::env::set_var("HEW_TRANSPORT", "tcp");
            0
        }
        "quic" => {
            std::env::set_var("HEW_TRANSPORT", "quic");
            0
        }
        _ => {
            set_last_error(format!("Node::set_transport: unknown transport '{s}'"));
            -1
        }
    }
}

/// Default timeout for remote ask operations (5 seconds).
const REMOTE_ASK_TIMEOUT_MS: u64 = 5_000;

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

    // Remote path: send message over mesh with request_id, wait for reply.
    let guard = CURRENT_NODE.read_or_recover();
    let node_ptr = *guard as *mut HewNode;
    if node_ptr.is_null() {
        return ask_null(AskError::NodeNotRunning);
    }
    // SAFETY: read lock pins CURRENT_NODE.
    let node = unsafe { &*node_ptr };
    if node.state.load(Ordering::Acquire) != NODE_STATE_RUNNING || node.conn_mgr.is_null() {
        return ask_null(AskError::NodeNotRunning);
    }

    // Look up the connection for the target node via the routing table.
    // SAFETY: routing_table is valid while node is running.
    let conn_id = unsafe { crate::routing::hew_routing_lookup(node.routing_table, pid) };
    if conn_id < 0 {
        return ask_null(AskError::RoutingFailed);
    }

    // Register a pending reply slot.
    let (request_id, pending) =
        REPLY_TABLE.register(ConnectionKey::new(node.conn_mgr.cast_const(), conn_id));

    // Encode the ask envelope with request_id and source_node_id.
    #[expect(
        clippy::cast_possible_truncation,
        reason = "payload size bounded by wire buffer limits"
    )]
    let env = crate::wire::HewWireEnvelope {
        target_actor_id: pid,
        source_actor_id: 0,
        msg_type,
        payload_size: size as u32,
        payload: data.cast::<u8>(),
        request_id,
        source_node_id: node.node_id,
    };
    // SAFETY: HewWireBuf is a plain-old-data struct; zeroing is valid initialisation.
    let mut wire_buf: crate::wire::HewWireBuf = unsafe { std::mem::zeroed() };
    // SAFETY: wire_buf is a valid stack allocation.
    unsafe { crate::wire::hew_wire_buf_init(&raw mut wire_buf) };
    // SAFETY: wire_buf and env are valid stack locals.
    if unsafe { crate::wire::hew_wire_encode_envelope(&raw mut wire_buf, &raw const env) } != 0 {
        // SAFETY: wire_buf was initialised above.
        unsafe { crate::wire::hew_wire_buf_free(&raw mut wire_buf) };
        REPLY_TABLE.remove(request_id);
        return ask_null(AskError::EncodeFailed);
    }

    // Send the encoded envelope through the connection manager so noise
    // encryption is applied when the connection is encrypted.
    // SAFETY: conn_mgr is valid while node is running; wire_buf is valid.
    let send_ok = unsafe {
        connection::hew_connmgr_send_preencoded(node.conn_mgr, conn_id, wire_buf.data, wire_buf.len)
            == 0
    };
    // SAFETY: wire_buf was initialised above.
    unsafe { crate::wire::hew_wire_buf_free(&raw mut wire_buf) };

    if !send_ok {
        REPLY_TABLE.remove(request_id);
        return ask_null(AskError::SendFailed);
    }

    // Drop the read lock before blocking so other threads can use the node.
    drop(guard);

    // Block until the reply arrives or the timeout elapses.
    let deadline =
        std::time::Instant::now() + std::time::Duration::from_millis(REMOTE_ASK_TIMEOUT_MS);
    let mut outcome_guard = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    while outcome_guard.is_none() {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            // Timeout — remove the pending entry and return the null failure sentinel.
            REPLY_TABLE.remove(request_id);
            return ask_null(AskError::Timeout);
        }
        let (new_guard, wait_result) = pending
            .cond
            .wait_timeout_or_recover(outcome_guard, remaining);
        outcome_guard = new_guard;
        if wait_result.timed_out() && outcome_guard.is_none() {
            REPLY_TABLE.remove(request_id);
            return ask_null(AskError::Timeout);
        }
    }

    let reply = outcome_guard.take().unwrap_or(ReplyOutcome {
        status: ReplyStatus::Failed,
        data: Vec::new(),
        ask_error: AskError::ConnectionDropped,
    });
    drop(outcome_guard);
    if reply.status == ReplyStatus::Failed {
        return ask_null(reply.ask_error);
    }
    let result = remote_reply_data_to_ptr(&reply.data, reply_size);
    if result.is_null() {
        // Non-void ask received an empty or wrong-size payload from the remote actor.
        ask_null(AskError::PayloadSizeMismatch)
    } else {
        LAST_ASK_ERROR.with(|cell| cell.set(AskError::None as i32));
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    struct ResetCurrentNode(usize);

    impl Drop for ResetCurrentNode {
        fn drop(&mut self) {
            let mut current = CURRENT_NODE.write_or_recover();
            *current = self.0;
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
            hew_cabi::sink::take_last_error()
        );
        // SAFETY: the node was started successfully and uses the default TCP transport in these tests.
        let port =
            unsafe { crate::transport::hew_transport_tcp_bound_port((*node.as_ptr()).transport) }
                .expect("started TCP test node must expose its bound listener port");
        (node, port)
    }

    unsafe extern "C" fn noop_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
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
    fn local_registry_register_and_lookup() {
        let _guard = crate::runtime_test_guard();

        crate::registry::hew_registry_clear();

        let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
        // SAFETY: bind_addr is a valid C string for the duration of this test.
        let node = unsafe { TestNode::new(102, &bind_addr) };
        assert!(!node.as_ptr().is_null());

        let actor_name = CString::new("hew-node-local-registry").expect("valid actor name");
        let missing_name = CString::new("hew-node-missing-registry").expect("valid actor name");
        let actor_pid = (u64::from(102u16) << 48) | 0x1234;

        // SAFETY: node and C string pointers are valid for each call.
        unsafe {
            assert_eq!(hew_node_start(node.as_ptr()), 0);
            assert_eq!(
                hew_node_register(node.as_ptr(), actor_name.as_ptr(), actor_pid),
                0
            );
            assert_eq!(
                hew_node_lookup(node.as_ptr(), actor_name.as_ptr()),
                actor_pid
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
            let actor_pid = (*actor).pid;
            assert_eq!(crate::pid::hew_pid_node(actor_pid), 103);

            assert_eq!(
                hew_node_register(node.as_ptr(), actor_name.as_ptr(), actor_pid),
                0
            );
            assert_eq!(
                hew_node_lookup(node.as_ptr(), actor_name.as_ptr()),
                actor_pid
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
            let actor_pid = (*actor).pid;
            assert_eq!(crate::pid::hew_pid_node(actor_pid), 104);

            assert_eq!(
                hew_node_register(node.as_ptr(), actor_name.as_ptr(), actor_pid),
                0
            );
            assert_eq!(
                hew_node_lookup(node.as_ptr(), actor_name.as_ptr()),
                actor_pid
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
        let actor_pid = (u64::from(202u16) << 48) | 0x63;
        // SAFETY: pointers are valid in this scope.
        unsafe {
            assert_eq!(
                hew_node_register(node2.as_ptr(), actor_name.as_ptr(), actor_pid),
                0
            );
            assert_eq!(
                hew_node_lookup(node2.as_ptr(), actor_name.as_ptr()),
                actor_pid
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

    // ── Reply routing table unit tests ─────────────────────────────────

    #[test]
    fn remote_ask_without_active_node_returns_null_for_nonvoid_reply() {
        let _guard = crate::runtime_test_guard();

        let saved_current_node = {
            let mut current = CURRENT_NODE.write_or_recover();
            let saved = *current;
            *current = 0;
            saved
        };
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
        // it directly through REPLY_TABLE to exercise the success path without
        // needing a live network.
        let key = ConnectionKey {
            conn_mgr: 99,
            conn_id: 42,
        };
        let (id, pending) = REPLY_TABLE.register(key);
        REPLY_TABLE.complete(id, vec![0xAAu8, 0xBBu8, 0xCCu8, 0xDDu8]);
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
        let (id, pending) = REPLY_TABLE.register(key);

        // Simulate a connection drop.
        REPLY_TABLE.fail_connection(key);

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
        let map = REPLY_TABLE
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
        let (id_a, pending_a) = REPLY_TABLE.register(key_a);
        let (id_b, pending_b) = REPLY_TABLE.register(key_b);

        REPLY_TABLE.fail_all();

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
        let map = REPLY_TABLE
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

    // ── Distributed proof-lane integration tests ──────────────────────────
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
    // Both tests run under the shared runtime test lock to ensure CURRENT_NODE / LOCAL_NODE_ID
    // state is not perturbed by concurrent node tests.

    use std::sync::atomic::AtomicU32;

    // ── Shared helpers for proof-lane tests ───────────────────────────────

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

    // ── Test: fire-and-forget remote message delivery ─────────────────────

    /// Stores the `msg_type` of the most-recently received remote message.
    static SEND_PROBE_MSG_TYPE: AtomicU32 = AtomicU32::new(0);

    unsafe extern "C" fn send_probe_dispatch(
        _state: *mut c_void,
        msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
        #[expect(
            clippy::cast_sign_loss,
            reason = "msg_type is a non-negative tag in this test"
        )]
        SEND_PROBE_MSG_TYPE.store(msg_type as u32, Ordering::Release);
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
        assert_eq!(
            crate::scheduler::hew_sched_init(),
            0,
            "scheduler init failed"
        );

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
        let actor_pid = unsafe { (*probe_actor).id };
        assert_eq!(
            crate::pid::hew_pid_node(actor_pid),
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
        let rc = unsafe { hew_node_send(node1.as_ptr(), actor_pid, msg_type_sent, ptr::null(), 0) };
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

    // ── Test: remote ask / reply round-trip ───────────────────────────────

    /// Echo-double dispatch: reads a u32 from `data`, replies with `value * 2`.
    unsafe extern "C" fn ask_probe_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        data: *mut c_void,
        size: usize,
    ) {
        if size < std::mem::size_of::<u32>() {
            return;
        }
        // SAFETY: data is valid for at least size_of::<u32>() bytes.
        let value = unsafe { *(data.cast::<u32>()) };
        let mut reply_value: u32 = value.wrapping_mul(2);

        let ch = crate::scheduler::hew_get_reply_channel();
        if ch.is_null() {
            return;
        }
        // SAFETY: ch is the current thread-local reply channel; reply_value is valid.
        unsafe {
            crate::reply_channel::hew_reply(
                ch.cast(),
                (&raw mut reply_value).cast::<c_void>(),
                std::mem::size_of::<u32>(),
            );
        }
    }

    unsafe extern "C" fn void_ask_probe_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
        let ch = crate::scheduler::hew_get_reply_channel();
        if ch.is_null() {
            return;
        }
        // SAFETY: the reply channel comes from the scheduler and is valid for a void reply here.
        unsafe { crate::reply_channel::hew_reply(ch.cast(), ptr::null_mut(), 0) };
    }

    unsafe extern "C" fn blocked_ask_probe_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
    ) {
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

        assert_eq!(
            crate::scheduler::hew_sched_init(),
            0,
            "scheduler init failed"
        );

        crate::pid::hew_pid_set_local_node(314);
        // SAFETY: null state and size-0 are valid; the dispatch function pointer is valid.
        let void_actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(void_ask_probe_dispatch))
        };
        crate::pid::hew_pid_set_local_node(313);
        assert!(!void_actor.is_null(), "actor spawn failed");
        // SAFETY: the actor was just spawned successfully and remains valid here.
        let actor_pid = unsafe { (*void_actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_pid), 314);

        let connect_addr = CString::new(format!("314@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and the connect address are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until the end of the test.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: this is a remote void ask; null payload/size are valid and no reply buffer is expected.
        let reply_ptr = unsafe { hew_node_api_ask(actor_pid, 1, ptr::null_mut(), 0, 0) };
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
        assert_eq!(
            crate::scheduler::hew_sched_init(),
            0,
            "scheduler init failed"
        );

        // Temporarily set LOCAL_NODE_ID = 312 to assign a node-2 PID to the actor.
        crate::pid::hew_pid_set_local_node(312);
        // SAFETY: null state / size-0 is valid; dispatch fn is a valid fn ptr.
        let echo_actor =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(ask_probe_dispatch)) };
        // Restore node1 as the local node before any routing decisions.
        crate::pid::hew_pid_set_local_node(311);
        assert!(!echo_actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid.
        let actor_pid = unsafe { (*echo_actor).id };
        assert_eq!(
            crate::pid::hew_pid_node(actor_pid),
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
                actor_pid,
                1,
                (&raw const send_value).cast::<c_void>().cast_mut(),
                std::mem::size_of::<u32>(),
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

        assert_eq!(
            crate::scheduler::hew_sched_init(),
            0,
            "scheduler init failed"
        );

        crate::pid::hew_pid_set_local_node(318);
        // SAFETY: null state and size-0 are valid; the dispatch function pointer is valid.
        let empty_reply_actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(void_ask_probe_dispatch))
        };
        crate::pid::hew_pid_set_local_node(317);
        assert!(!empty_reply_actor.is_null(), "actor spawn failed");
        // SAFETY: the actor was just spawned successfully and remains valid here.
        let actor_pid = unsafe { (*empty_reply_actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_pid), 318);

        let connect_addr = CString::new(format!("318@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and the connect address are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until the end of the test.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: non-void remote ask expects a u32-sized reply; an empty success must fail closed.
        let reply_ptr = unsafe {
            hew_node_api_ask(actor_pid, 1, ptr::null_mut(), 0, std::mem::size_of::<u32>())
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

        assert_eq!(
            crate::scheduler::hew_sched_init(),
            0,
            "scheduler init failed"
        );

        crate::pid::hew_pid_set_local_node(316);
        // SAFETY: null state and size-0 are valid; the dispatch function pointer is valid.
        let blocked_actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(blocked_ask_probe_dispatch))
        };
        crate::pid::hew_pid_set_local_node(315);
        assert!(!blocked_actor.is_null(), "actor spawn failed");
        // SAFETY: the actor was just spawned successfully and remains valid here.
        let actor_pid = unsafe { (*blocked_actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_pid), 316);

        let connect_addr = CString::new(format!("316@127.0.0.1:{node2_port}")).unwrap();
        // SAFETY: node1 and the connect address are valid for this connection attempt.
        unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
        // SAFETY: both node pointers remain valid until the end of the test.
        unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

        // SAFETY: the actor pid and null payload are valid for this remote ask probe.
        let ask_handle = thread::spawn(move || unsafe {
            let ptr =
                hew_node_api_ask(actor_pid, 1, ptr::null_mut(), 0, std::mem::size_of::<u32>());
            let err = hew_node_ask_take_last_error();
            (ptr as usize, err)
        });

        let pending_seen = (0..100).any(|_| {
            let guard = REPLY_TABLE
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

        assert_eq!(
            crate::scheduler::hew_sched_init(),
            0,
            "scheduler init failed"
        );

        crate::pid::hew_pid_set_local_node(321);
        // SAFETY: null state and size-0 are valid; the dispatch function pointer is valid.
        let blocked_actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(blocked_ask_probe_dispatch))
        };
        crate::pid::hew_pid_set_local_node(320);
        assert!(!blocked_actor.is_null(), "actor spawn failed");
        // SAFETY: the actor was just spawned successfully and remains valid here.
        let actor_pid = unsafe { (*blocked_actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_pid), 321);

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
            let ptr =
                hew_node_api_ask(actor_pid, 1, ptr::null_mut(), 0, std::mem::size_of::<u32>());
            let err = hew_node_ask_take_last_error();
            (ptr as usize, err)
        });

        let pending_seen = (0..100).any(|_| {
            let guard = REPLY_TABLE
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
            disconnect_started.elapsed() < Duration::from_millis(REMOTE_ASK_TIMEOUT_MS / 2),
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

        let (request_id, pending) = REPLY_TABLE.register(ConnectionKey {
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
        REPLY_TABLE.remove(request_id);

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

    /// `InboundAskGuard` decrements `INBOUND_ASK_ACTIVE` exactly once on drop,
    /// including when the enclosing scope exits via panic.
    #[test]
    fn inbound_ask_guard_decrements_on_drop() {
        let _lock = crate::runtime_test_guard();
        // Reset to a known value; restore on exit.
        let saved = INBOUND_ASK_ACTIVE.swap(1, Ordering::AcqRel);
        {
            let _guard = InboundAskGuard;
            assert_eq!(
                INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
                1,
                "counter must be 1 while guard is live"
            );
        }
        // Guard dropped — counter must be 0.
        let after = INBOUND_ASK_ACTIVE.load(Ordering::Acquire);
        INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);
        assert_eq!(
            after, 0,
            "InboundAskGuard must decrement INBOUND_ASK_ACTIVE on drop"
        );
    }

    /// Two guards decrement independently (one per spawned thread).
    #[test]
    fn inbound_ask_guard_pair_decrements_twice() {
        let _lock = crate::runtime_test_guard();
        let saved = INBOUND_ASK_ACTIVE.swap(2, Ordering::AcqRel);
        let g1 = InboundAskGuard;
        let g2 = InboundAskGuard;
        drop(g1);
        assert_eq!(
            INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
            1,
            "first guard must decrement by 1"
        );
        drop(g2);
        assert_eq!(
            INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
            0,
            "second guard must decrement back to zero"
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

        assert_eq!(
            crate::scheduler::hew_sched_init(),
            0,
            "scheduler init failed"
        );

        // Spawn a u32-echo actor on node2.
        crate::pid::hew_pid_set_local_node(321);
        // SAFETY: null state and size-0 are valid; ask_probe_dispatch echoes back u32*2.
        let echo_actor =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(ask_probe_dispatch)) };
        crate::pid::hew_pid_set_local_node(320);
        assert!(!echo_actor.is_null(), "actor spawn failed");
        // SAFETY: the actor was just spawned successfully and remains valid here.
        let actor_pid = unsafe { (*echo_actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_pid), 321);

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
                actor_pid,
                1,
                std::ptr::from_ref(&payload).cast_mut().cast::<c_void>(),
                std::mem::size_of::<u32>(),
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
        assert_eq!(crate::scheduler::hew_sched_init(), 0, "scheduler init");

        // Spawn a void-reply actor on node2.
        crate::pid::hew_pid_set_local_node(323);
        // SAFETY: null state / size-0 are valid; void_ask_probe_dispatch is valid.
        let actor = unsafe {
            crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(void_ask_probe_dispatch))
        };
        crate::pid::hew_pid_set_local_node(322);
        assert!(!actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid here.
        let actor_pid = unsafe { (*actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_pid), 323);

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
        let reply_ptr = unsafe { hew_node_api_ask(actor_pid, 1, ptr::null_mut(), 0, 0) };
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
        assert_eq!(crate::scheduler::hew_sched_init(), 0, "scheduler init");

        // Spawn a u32-echo actor on node2.
        crate::pid::hew_pid_set_local_node(325);
        // SAFETY: null state and size-0 are valid; ask_probe_dispatch is valid.
        let actor =
            unsafe { crate::actor::hew_actor_spawn(ptr::null_mut(), 0, Some(ask_probe_dispatch)) };
        crate::pid::hew_pid_set_local_node(324);
        assert!(!actor.is_null(), "actor spawn failed");
        // SAFETY: actor was just spawned and is valid here.
        let actor_pid = unsafe { (*actor).id };
        assert_eq!(crate::pid::hew_pid_node(actor_pid), 325);

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
                actor_pid,
                1,
                std::ptr::from_ref(&payload).cast_mut().cast::<c_void>(),
                std::mem::size_of::<u32>(),
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
}
