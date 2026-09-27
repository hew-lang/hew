//! Unified distributed node runtime.
//!
//! Integrates transport, connection manager, SWIM membership, and
//! name/actor registry wiring.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::lifetime::{PoisonSafe, PoisonSafeRw};
use crate::util::MutexExt;
use std::collections::HashMap;
use std::ffi::{c_char, c_int, c_void, CStr, CString};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicU16, AtomicU64, AtomicU8, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use crate::set_last_error;
use std::thread::{self, JoinHandle};

use crate::cluster::{self, ClusterConfig, HewCluster};
use crate::connection::{self, HewConnMgr};
use crate::node_identity::{HewLocation, HewRemotePid, Location};
use crate::peer_binding::{PeerAuthSnapshot, TransportSelection as PeerTransport};
use crate::routing::{self, HewRoutingTable};
use crate::transport::{self, HewTransport, HewTransportOps};

mod api;
mod dist_probes;
mod inbound;
mod monitor_link;
mod remote_call;
#[cfg(test)]
mod tests;

pub use self::api::*;
pub use self::dist_probes::*;
pub(crate) use self::inbound::*;
pub use self::monitor_link::*;
pub use self::remote_call::*;

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
// Ask-error discriminant
// ---------------------------------------------------------------------------

/// Re-exported from [`crate::internal::types`] so callers that already import
/// from this module keep working. The canonical definition lives in
/// `internal::types` so that WASM targets, which cannot import `hew_node`,
/// can also use the type.
pub use crate::internal::types::AskError;

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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum RemoteSetupKind {
    Monitor,
    Link,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PendingReplyKind {
    Ask,
    Setup(RemoteSetupKind, u64),
}

struct PendingReply {
    connection: ConnectionKey,
    request_id: u64,
    kind: PendingReplyKind,
    outcome: Mutex<Option<ReplyOutcome>>,
    cond: Condvar,
    /// The readiness target of a native coroutine call. Completion wakes it
    /// instead of signalling `cond`, which serves blocking callers.
    waker: Option<crate::wake::OwnedWaker>,
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

    /// Allocate a new request ID and register a pending reply slot. A waker
    /// receives the completion instead of the condvar.
    fn register_with_waker(
        &self,
        connection: ConnectionKey,
        waker: Option<crate::wake::OwnedWaker>,
        kind: PendingReplyKind,
    ) -> (u64, Arc<PendingReply>) {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);
        let entry = Arc::new(PendingReply {
            connection,
            request_id: id,
            kind,
            outcome: Mutex::new(None),
            cond: Condvar::new(),
            waker,
        });
        let mut map = self
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        map.insert(id, Arc::clone(&entry));
        (id, entry)
    }

    /// Allocate a new request ID and register a blocking pending ask reply.
    #[cfg(test)]
    fn register(&self, connection: ConnectionKey) -> (u64, Arc<PendingReply>) {
        self.register_with_waker(connection, None, PendingReplyKind::Ask)
    }

    fn register_setup(
        &self,
        connection: ConnectionKey,
        kind: RemoteSetupKind,
        publication_token: u64,
    ) -> (u64, Arc<PendingReply>) {
        self.register_with_waker(
            connection,
            None,
            PendingReplyKind::Setup(kind, publication_token),
        )
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

    /// Deposit `outcome` into `pending` and wake the waiter: a blocking caller
    /// through the condvar, a coroutine call through its waker. The outcome
    /// lock is released before the wake so the woken caller can take it.
    fn complete_pending(pending: &PendingReply, outcome: ReplyOutcome) {
        let mut guard = pending
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *guard = Some(outcome);
        drop(guard);

        match &pending.waker {
            Some(waker) => waker.wake(),
            None => pending.cond.notify_one(),
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
    /// request was found. Test-only: production failures arrive through the
    /// connection-validated paths.
    #[cfg(test)]
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
        kind: PendingReplyKind,
    ) -> Option<Arc<PendingReply>> {
        let mut map = self
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match map.get(&request_id) {
            Some(pending) if pending.connection == expected && pending.kind == kind => {
                map.remove(&request_id)
            }
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
        if let Some(pending) =
            self.remove_if_connection(request_id, expected, PendingReplyKind::Ask)
        {
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

    fn complete_setup_from_connection(
        &self,
        request_id: u64,
        expected: ConnectionKey,
        kind: RemoteSetupKind,
        publication_token: u64,
        payload: Vec<u8>,
    ) -> bool {
        if let Some(pending) = self.remove_if_connection(
            request_id,
            expected,
            PendingReplyKind::Setup(kind, publication_token),
        ) {
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
        if let Some(pending) =
            self.remove_if_connection(request_id, expected, PendingReplyKind::Ask)
        {
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

pub(crate) fn complete_remote_setup_reply(
    conn_mgr: *const HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    setup_id: u64,
    kind: RemoteSetupKind,
    payload: &[u8],
) -> bool {
    let expected = ConnectionKey::new(conn_mgr, conn_id);
    reply_table_opt().is_some_and(|table| {
        table.complete_setup_from_connection(
            setup_id,
            expected,
            kind,
            publication_token,
            payload.to_vec(),
        )
    })
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
        x if x == AskError::HandlerTrapped as i32 => Some(AskError::HandlerTrapped),
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
    let result = crate::mem::buf_try_alloc(reply_data.len());
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

    // ── NodeId authority — before any allocation/listen ──
    // The per-node `PeerAuthSnapshot` is authoritative for the receiver-local
    // route slot. Reject a self-inconsistent snapshot, then reconcile the slot.
    if let Err(reason) = node.auth.validate() {
        fail_start!(reason);
    }
    if let Some(snapshot_route_slot) = node.auth.local_route_slot() {
        let snapshot_route_slot = snapshot_route_slot.get();
        if node.route_slot == 0 {
            // A low-level caller deferred the route slot to its snapshot.
            node.route_slot = snapshot_route_slot;
        } else if node.route_slot != snapshot_route_slot {
            // Refuse before the listener binds and before cluster/routing/connmgr
            // are created.
            fail_start!(format!(
                "hew_node_start: explicit route slot {} conflicts with the frozen local route slot \
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
        if let Some(rt) = crate::runtime::rt_current_opt() {
            rt.monitors.drain_remote_observations_for_shutdown();
        }

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
        stop_node_accept_thread(node);

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
        // SAFETY: bind_addr_owned was allocated via cstr_strdup's sized-block allocation.
        unsafe { crate::mem::buf_free(node.bind_addr_owned.cast::<c_void>()) };
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

/// Send a message wrapper to an exact target location, consuming its fields.
///
/// `dispatch` is the TARGET actor TYPE's dispatch function pointer, keying the
/// member's codec `(dispatch, msg_type)`. A target on this node receives the
/// wrapper's fields directly; any other target receives the encoded bytes and
/// the fields are released here.
///
/// # Safety
///
/// - `node` must be valid.
/// - `target` must point to a valid `HewRemotePid`.
/// - `payload` must be the member's wrapper, valid for `payload_len` bytes, or
///   null when len is 0. Its fields transfer to this call.
/// - `dispatch` is an opaque codec key, never dereferenced.
#[no_mangle]
pub unsafe extern "C" fn hew_node_send_location(
    node: *mut HewNode,
    target: *const HewRemotePid,
    dispatch: *const c_void,
    msg_type: i32,
    payload: *const u8,
    payload_len: usize,
) -> c_int {
    let mut moved = false;
    // SAFETY: forwards this function's contract.
    let status = unsafe {
        send_location(
            node,
            target,
            dispatch,
            msg_type,
            payload,
            payload_len,
            &mut moved,
        )
    };
    if !moved {
        // SAFETY: the fields did not move into a local actor.
        unsafe { release_request(dispatch, msg_type, payload.cast_mut().cast()) };
    }
    status
}

/// Whether a native submission status leaves the fields with the destination.
fn submission_delivered(status: i32) -> bool {
    status == crate::actor_native::HewSubmitStatus::Accepted as i32
        || status == crate::actor_native::HewSubmitStatus::Discarded as i32
}

/// Release the fields of a request wrapper that did not move into an actor.
///
/// # Safety
/// `request` is null or a wrapper of the member registered under
/// `(dispatch, msg_type)` whose fields are still owned by the caller.
unsafe fn release_request(dispatch: *const c_void, msg_type: i32, request: *mut c_void) {
    if request.is_null() {
        return;
    }
    if let Some(drop) =
        crate::xnode_serial::lookup_request(dispatch, msg_type).and_then(|codec| codec.drop)
    {
        // SAFETY: the caller still owns the wrapper's fields.
        unsafe { drop(request) };
    }
}

/// A decoded inbound request wrapper this node owns until an actor accepts
/// it. Every path that does not deliver it releases its fields.
struct InboundRequest {
    wrapper: *mut c_void,
    size: usize,
    drop: crate::xnode_serial::DropThunk,
}

impl InboundRequest {
    /// # Safety
    /// `codec` is a registered request codec for the frame's target member.
    unsafe fn decode(codec: crate::xnode_serial::ThunkPair, payload: &[u8]) -> Option<Self> {
        let drop = codec.drop?;
        let mut size = 0;
        // SAFETY: the codec borrows the frame bytes for this synchronous decode.
        let wrapper =
            unsafe { (codec.deserialize)(payload.as_ptr(), payload.len(), &raw mut size) };
        (!wrapper.is_null()).then_some(Self {
            wrapper,
            size,
            drop,
        })
    }

    /// Move a caller's request wrapper into an owned copy for a local target;
    /// the caller's wrapper keeps only its bytes. `None` leaves the fields
    /// with the caller.
    ///
    /// # Safety
    /// `wrapper` holds `size` readable bytes of the codec's member wrapper.
    unsafe fn adopt(
        codec: crate::xnode_serial::ThunkPair,
        wrapper: *const c_void,
        size: usize,
    ) -> Option<Self> {
        let drop = codec.drop?;
        if wrapper.is_null() {
            return None;
        }
        let copy = crate::mem::buf_try_alloc(size);
        if copy.is_null() {
            return None;
        }
        // SAFETY: both buffers hold the wrapper's size.
        unsafe { ptr::copy_nonoverlapping(wrapper.cast::<u8>(), copy.cast::<u8>(), size) };
        Some(Self {
            wrapper: copy,
            size,
            drop,
        })
    }

    /// Enter the target's runtime and resolve its local handle, keeping the
    /// target pinned while the handle is read.
    fn target(
        actor_id: u64,
    ) -> Option<(
        crate::runtime::EnterGuard,
        crate::lifetime::local_handles::HewLocalPidId,
    )> {
        crate::lifetime::live_actors::with_actor_send_by_id(actor_id, |actor| {
            // SAFETY: the pin keeps the actor live for these reads.
            unsafe {
                let token = (*actor).local_pid_id;
                crate::runtime::enter_actor_runtime(actor).map(|guard| (guard, token))
            }
        })
        .flatten()
    }

    /// Submit a one-way message, keeping the fields when admission refuses.
    /// Returns the native submission status, or `None` when no live actor
    /// holds the id.
    fn submit(self, actor_id: u64, msg_type: i32) -> Option<i32> {
        use crate::actor_native::HewSubmitStatus;
        let (_runtime, token) = Self::target(actor_id)?;
        // The runtime frees a refused wrapper's bytes and leaves its fields
        // with the sender, so it takes a shallow copy of this one.
        let copy = crate::mem::buf_try_alloc(self.size);
        if copy.is_null() {
            return Some(HewSubmitStatus::Oom as i32);
        }
        // SAFETY: both buffers hold the decoded wrapper's size.
        unsafe {
            ptr::copy_nonoverlapping(self.wrapper.cast::<u8>(), copy.cast::<u8>(), self.size);
        };
        // SAFETY: the copy is an unpublished wrapper with the member's fields.
        let status = unsafe {
            crate::actor_native::hew_actor_submit_native(
                token,
                msg_type,
                copy,
                self.size,
                self.drop,
                0,
                None,
                ptr::null_mut(),
            )
        };
        if submission_delivered(status) {
            // The destination owns the fields; only the shell remains.
            // SAFETY: the shell came from the decode allocation.
            unsafe { crate::mem::buf_free(self.wrapper) };
            std::mem::forget(self);
        }
        Some(status)
    }

    /// Complete an ask against the local actor and encode its reply. The
    /// reply's fields are released once encoded.
    fn ask(
        self,
        actor_id: u64,
        msg_type: i32,
        reply: crate::xnode_serial::ThunkPair,
    ) -> Result<Vec<u8>, AskError> {
        let Some((_runtime, token)) = Self::target(actor_id) else {
            return Err(AskError::ActorStopped);
        };
        let (readiness, waker) = crate::wake::blocking::Readiness::new();
        let (wrapper, size, drop) = (self.wrapper, self.size, self.drop);
        std::mem::forget(self);
        // SAFETY: the operation owns the wrapper and its fields on every
        // outcome, and the waker descriptor is live for the call.
        let operation = unsafe {
            crate::actor_call_native::hew_actor_call_new(
                token,
                msg_type,
                wrapper,
                size,
                drop,
                reply.size,
                reply.drop,
                waker.descriptor(),
                0,
                0,
                // A full mailbox refuses the request rather than parking this
                // worker, and the peer learns `MailboxFull`.
                1,
                None,
                None,
            )
        };
        // SAFETY: this thread exclusively drives the live operation.
        let status = loop {
            // SAFETY: this thread exclusively drives the live operation.
            let status = unsafe { crate::actor_call_native::hew_actor_call_poll(operation) };
            if status != -1 {
                break status;
            }
            readiness.wait();
        };
        let mut value = vec![0_u8; reply.size];
        let mut rejected = ptr::null_mut();
        let status = if status < 0 {
            AskError::ActorStopped as i32
        } else {
            // SAFETY: the operation is ready and the output holds its reply size.
            unsafe {
                crate::actor_call_native::hew_actor_call_take(
                    operation,
                    value.as_mut_ptr().cast(),
                    &raw mut rejected,
                )
            }
        };
        // SAFETY: the operation is released once after its take; a refused
        // request comes back as its envelope, whose release drops its fields.
        unsafe {
            crate::actor_call_native::hew_actor_call_free(operation);
            if !rejected.is_null() {
                crate::mailbox::hew_msg_envelope_release(rejected);
            }
        }
        if status != AskError::None as i32 {
            return Err(ask_error_from_code(status).unwrap_or(AskError::ActorStopped));
        }
        if reply.size == 0 {
            return Ok(Vec::new());
        }
        let mut len = 0;
        // SAFETY: `value` holds the member's reply, read by its codec.
        let bytes = unsafe { (reply.serialize)(value.as_ptr().cast(), &raw mut len) };
        if let Some(drop) = reply.drop {
            // SAFETY: the reply's owned fields are released exactly once.
            unsafe { drop(value.as_mut_ptr().cast()) };
        }
        if bytes.is_null() {
            return Err(AskError::EncodeFailed);
        }
        // SAFETY: the codec returned `len` bytes it transfers to this caller.
        let encoded = unsafe { std::slice::from_raw_parts(bytes, len) }.to_vec();
        // SAFETY: bytes came from the codec's serializer allocation.
        unsafe { crate::xnode_serial::hew_ser_free_bytes(bytes) };
        Ok(encoded)
    }
}

impl Drop for InboundRequest {
    fn drop(&mut self) {
        // SAFETY: an undelivered wrapper still owns its fields and bytes.
        unsafe {
            (self.drop)(self.wrapper);
            crate::mem::buf_free(self.wrapper);
        }
    }
}

/// Route one send; `moved` reports that a local target took the fields.
#[allow(
    clippy::too_many_lines,
    reason = "function coordinates exact routing, serialization, and transport ownership"
)]
unsafe fn send_location(
    node: *mut HewNode,
    target: *const HewRemotePid,
    dispatch: *const c_void,
    msg_type: i32,
    payload: *const u8,
    payload_len: usize,
    moved: &mut bool,
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
    let (target_node_id, conn_id) = match unsafe {
        routing::hew_routing_lookup_location(node.routing_table, target)
    } {
        routing::LocationRoute::Local { actor_id } => {
            if crate::lifetime::live_actors::get_actor_ptr_by_id(actor_id).is_none() {
                set_last_error("remote send refused: target actor slot is stale");
                return HEW_ERR_STALE_REF;
            }
            // This node owns the target: its fields move into the actor's
            // mailbox with the delivery a peer gives an inbound frame.
            // SAFETY: the payload is the member's wrapper (caller contract).
            let request =
                crate::xnode_serial::lookup_request(dispatch, msg_type).and_then(|codec| unsafe {
                    InboundRequest::adopt(codec, payload.cast(), payload_len)
                });
            let Some(request) = request else {
                set_last_error("remote send dropped: the local target has no request codec");
                return 0;
            };
            *moved = true;
            return match request.submit(actor_id, msg_type) {
                Some(status) if submission_delivered(status) => 0,
                // A target that is gone or closing reports its stale
                // address, as this node's own send path always has.
                None => HEW_ERR_STALE_REF,
                Some(status) if status == crate::actor_native::HewSubmitStatus::Closed as i32 => {
                    set_last_error("remote send refused: target actor is gone");
                    HEW_ERR_STALE_REF
                }
                // A full mailbox drops the message, as on a peer.
                Some(_) => {
                    set_last_error(format!(
                        "remote send dropped: target actor {actor_id} refused msg_type={msg_type}"
                    ));
                    0
                }
            };
        }
        routing::LocationRoute::Remote {
            route_slot, conn, ..
        } => (route_slot, conn),
        routing::LocationRoute::Partition => {
            set_last_error("remote send refused: target identity has no live connection");
            return -1;
        }
        routing::LocationRoute::StaleRef => {
            set_last_error("remote send refused: target Location is stale");
            return HEW_ERR_STALE_REF;
        }
    };

    if node.conn_mgr.is_null() {
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
            // SAFETY: bytes came from encode_payload's sized-block allocation.
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
        // SAFETY: bytes came from encode_payload's sized-block allocation.
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
            // SAFETY: the fields transfer to this call on every outcome.
            unsafe { release_request(dispatch, msg_type, payload.cast_mut().cast()) };
            return -1;
        }
        // SAFETY: the current-node read lock pins `node`; remaining arguments
        // carry the caller contract documented above.
        unsafe { hew_node_send_location(node, target, dispatch, msg_type, payload, payload_len) }
    })
}
