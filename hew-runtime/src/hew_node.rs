//! Unified distributed node runtime.
//!
//! Integrates transport, connection manager, SWIM membership, and
//! name/actor registry wiring.

use crate::util::{CondvarExt, MutexExt, RwLockExt};
use std::collections::HashMap;
use std::ffi::{c_char, c_int, c_void, CStr, CString};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicU16, AtomicU64, AtomicU8, Ordering};
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

// ---------------------------------------------------------------------------
// Reply routing table for distributed ask/reply
// ---------------------------------------------------------------------------

/// A single pending remote ask waiting for its reply.
struct PendingReply {
    data: Mutex<Option<Vec<u8>>>,
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
    fn register(&self) -> (u64, Arc<PendingReply>) {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);
        let entry = Arc::new(PendingReply {
            data: Mutex::new(None),
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
        let entry = {
            let mut map = self
                .pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            map.remove(&request_id)
        };
        if let Some(pending) = entry {
            let mut guard = pending
                .data
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            *guard = Some(payload);
            pending.cond.notify_one();
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

/// Deposit a reply payload for a pending remote ask.
///
/// Called by the reader thread when a reply envelope arrives. Returns
/// `true` if the request ID was matched.
pub(crate) fn complete_remote_reply(request_id: u64, payload: &[u8]) -> bool {
    REPLY_TABLE.complete(request_id, payload.to_vec())
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
    conn_by_node: Mutex<HashMap<u16, c_int>>,
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
) {
    if request_id > 0 && source_node_id > 0 {
        // Inbound remote ask — dispatch locally and send the reply back.
        // Deep-copy the payload so we can hand it to a background thread.
        let payload = if size > 0 && !data.is_null() {
            // SAFETY: data is valid for `size` bytes (reader_loop contract).
            unsafe { std::slice::from_raw_parts(data, size) }.to_vec()
        } else {
            Vec::new()
        };
        thread::spawn(move || {
            handle_inbound_ask(
                target_actor_id,
                msg_type,
                &payload,
                request_id,
                source_node_id,
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
fn handle_inbound_ask(
    target_actor_id: u64,
    msg_type: i32,
    payload: &[u8],
    request_id: u64,
    source_node_id: u16,
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
    send_reply_envelope(source_node_id, request_id, &reply_data);
}

/// Encode and send a reply envelope back to the source node.
fn send_reply_envelope(target_node_id: u16, request_id: u64, reply_data: &[u8]) {
    let guard = CURRENT_NODE.read_or_recover();
    if *guard == 0 {
        return;
    }
    let node = *guard as *mut HewNode;
    // SAFETY: read lock pins the node pointer.
    let node_ref = unsafe { &*node };
    if node_ref.conn_mgr.is_null() {
        return;
    }

    // Find the connection for the target node.
    let conn_id = {
        let map = node_ref.conn_by_node.lock_or_recover();
        match map.get(&target_node_id) {
            Some(&id) => id,
            None => return,
        }
    };

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

    // SAFETY: transport pointer is valid while node is running.
    let t = unsafe { &*node_ref.transport };
    // SAFETY: transport ops are valid.
    if let Some(ops) = unsafe { t.ops.as_ref() } {
        if let Some(send_fn) = ops.send {
            // SAFETY: wire_buf contains the encoded envelope.
            unsafe {
                send_fn(
                    t.r#impl,
                    conn_id,
                    wire_buf.data.cast::<c_void>(),
                    wire_buf.len,
                )
            };
        }
    }
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
        conn_by_node: Mutex::new(HashMap::new()),
        accept_stop: Arc::new(AtomicBool::new(false)),
        accept_thread: Mutex::new(None),
        next_peer_node: AtomicU16::new(1),
    });
    Box::into_raw(node)
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
    if node.state.load(Ordering::Acquire) == NODE_STATE_STOPPED {
        return 0;
    }

    node.state.store(NODE_STATE_STOPPING, Ordering::Release);
    {
        let mut guard = CURRENT_NODE.write_or_recover();
        if *guard == ptr::from_mut(node) as usize {
            *guard = 0;
        }
    }
    node.accept_stop.store(true, Ordering::Release);
    {
        let mut guard = node.accept_thread.lock_or_recover();
        if let Some(handle) = guard.take() {
            let _ = handle.join();
        }
    }

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

    {
        let mut guard = node.conn_by_node.lock_or_recover();
        guard.clear();
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
    let mut conn_id = unsafe { routing::hew_routing_lookup(node.routing_table, target_pid) };
    if conn_id < 0 {
        conn_id = {
            let map = node.conn_by_node.lock_or_recover();
            let Some(conn_id) = map.get(&target_node_id) else {
                return -1;
            };
            *conn_id
        };
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

    {
        let mut map = node.conn_by_node.lock_or_recover();
        map.insert(peer_node_id, conn_id);
    }

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
    if name.is_null() {
        return -1;
    }
    // SAFETY: name was null-checked above and is a valid C string.
    let Ok(s) = unsafe { CStr::from_ptr(name) }.to_str() else {
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
/// Minimum allocation for zeroed fallback replies (covers any scalar type).
const FALLBACK_REPLY_SIZE: usize = 8;

/// Allocate a zeroed fallback buffer for failed remote asks.
///
/// The codegen unconditionally loads from the reply pointer, so we must
/// never return null for non-void ask results. A zeroed buffer produces
/// a zero/null value which is a safe default on failure.
fn alloc_zeroed_reply() -> *mut c_void {
    // SAFETY: calloc is safe with any positive size.
    unsafe { libc::calloc(1, FALLBACK_REPLY_SIZE) }
}

/// Perform a blocking ask against a PID, handling local and remote actors.
///
/// If the PID targets the local node, delegates to `hew_actor_ask`.
/// If remote, sends the message with a `request_id` over the mesh and
/// blocks until the reply arrives (or times out).
///
/// Returns a `malloc`'d reply buffer on success. On failure or timeout,
/// returns a zeroed allocation (never null) so the codegen can safely
/// load the result value. The caller must `free` the returned pointer.
///
/// # Safety
///
/// - `pid` must be a valid actor PID.
/// - `data` must point to at least `size` readable bytes, or be null when
///   `size` is 0.
#[expect(
    clippy::too_many_lines,
    reason = "local + remote ask paths are clearer in one function"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_node_api_ask(
    pid: u64,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> *mut c_void {
    let target_node_id = crate::pid::hew_pid_node(pid);
    let local_node_id = crate::pid::hew_pid_local_node();

    // Local path: delegate to the by-ID ask (which packs a reply channel).
    if target_node_id == 0 || target_node_id == local_node_id {
        // SAFETY: data/size are caller-validated; local actor ask is safe here.
        return unsafe { crate::actor::hew_actor_ask_by_id(pid, msg_type, data, size) };
    }

    // Remote path: send message over mesh with request_id, wait for reply.
    let guard = CURRENT_NODE.read_or_recover();
    let node_ptr = *guard as *mut HewNode;
    if node_ptr.is_null() {
        return alloc_zeroed_reply();
    }
    // SAFETY: read lock pins CURRENT_NODE.
    let node = unsafe { &*node_ptr };
    if node.state.load(Ordering::Acquire) != NODE_STATE_RUNNING || node.conn_mgr.is_null() {
        return alloc_zeroed_reply();
    }

    // Look up the connection for the target node.
    let conn_id = {
        let mut cid =
            // SAFETY: routing_table is valid while node is running.
            unsafe { crate::routing::hew_routing_lookup(node.routing_table, pid) };
        if cid < 0 {
            let map = node.conn_by_node.lock_or_recover();
            match map.get(&target_node_id) {
                Some(&id) => cid = id,
                None => return alloc_zeroed_reply(),
            }
        }
        cid
    };

    // Register a pending reply slot.
    let (request_id, pending) = REPLY_TABLE.register();

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
        return alloc_zeroed_reply();
    }

    // Send the encoded envelope.
    // SAFETY: transport pointer is valid while node is running.
    let send_ok = unsafe {
        let t = &*node.transport;
        if let Some(ops) = t.ops.as_ref() {
            if let Some(send_fn) = ops.send {
                send_fn(
                    t.r#impl,
                    conn_id,
                    wire_buf.data.cast::<c_void>(),
                    wire_buf.len,
                ) > 0
            } else {
                false
            }
        } else {
            false
        }
    };
    // SAFETY: wire_buf was initialised above.
    unsafe { crate::wire::hew_wire_buf_free(&raw mut wire_buf) };

    if !send_ok {
        REPLY_TABLE.remove(request_id);
        return alloc_zeroed_reply();
    }

    // Drop the read lock before blocking so other threads can use the node.
    drop(guard);

    // Block until the reply arrives or the timeout elapses.
    let deadline =
        std::time::Instant::now() + std::time::Duration::from_millis(REMOTE_ASK_TIMEOUT_MS);
    let mut data_guard = pending
        .data
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    while data_guard.is_none() {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            // Timeout — remove the pending entry and return a zeroed fallback.
            REPLY_TABLE.remove(request_id);
            return alloc_zeroed_reply();
        }
        let (new_guard, wait_result) = pending.cond.wait_timeout_or_recover(data_guard, remaining);
        data_guard = new_guard;
        if wait_result.timed_out() && data_guard.is_none() {
            REPLY_TABLE.remove(request_id);
            return alloc_zeroed_reply();
        }
    }

    // Reply received — copy into a malloc'd buffer for the caller.
    let reply_data = data_guard.take().unwrap_or_default();
    drop(data_guard);

    if reply_data.is_empty() {
        return alloc_zeroed_reply();
    }

    // SAFETY: malloc for reply buffer.
    let result = unsafe { libc::malloc(reply_data.len()) };
    if result.is_null() {
        return alloc_zeroed_reply();
    }
    // SAFETY: result was just allocated with reply_data.len() bytes.
    unsafe {
        ptr::copy_nonoverlapping(reply_data.as_ptr(), result.cast::<u8>(), reply_data.len());
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::TcpListener;
    use std::time::Duration;

    static NODE_TEST_LOCK: Mutex<()> = Mutex::new(());

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

    fn reserve_tcp_port() -> (u16, TcpListener) {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind local ephemeral port");
        let port = listener.local_addr().expect("read local address").port();
        (port, listener)
    }

    #[test]
    fn node_lifecycle_start_stop() {
        let _guard = NODE_TEST_LOCK.lock_or_recover();

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
        let _guard = NODE_TEST_LOCK.lock_or_recover();

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
    fn two_node_connect_and_handshake() {
        let _guard = NODE_TEST_LOCK.lock_or_recover();

        crate::registry::hew_registry_clear();

        // Hold the listener to prevent port reuse until node2 binds.
        let (node2_port, port_guard) = reserve_tcp_port();
        let node1_bind = CString::new("127.0.0.1:0").expect("valid bind addr");
        let node2_bind = CString::new(format!("127.0.0.1:{node2_port}")).expect("valid bind addr");

        // SAFETY: bind addresses are valid C strings for the duration of this test.
        let node1 = unsafe { TestNode::new(201, &node1_bind) };
        // Drop the listener so node2 can bind to the port.
        drop(port_guard);
        // SAFETY: bind addresses are valid C strings for the duration of this test.
        let node2 = unsafe { TestNode::new(202, &node2_bind) };
        assert!(!node1.as_ptr().is_null());
        assert!(!node2.as_ptr().is_null());

        // Start node2 first (listener) then node1 (connector).
        // SAFETY: pointers are valid for this scope.
        unsafe {
            assert_eq!(hew_node_start(node2.as_ptr()), 0);
        }
        // Allow node2 listener to initialise before node1 connects.
        thread::sleep(Duration::from_millis(50));
        // SAFETY: pointers are valid for this scope.
        unsafe {
            assert_eq!(hew_node_start(node1.as_ptr()), 0);
        }

        let connect_addr =
            CString::new(format!("202@127.0.0.1:{node2_port}")).expect("valid connect addr");
        let mut connected = false;
        let mut backoff = Duration::from_millis(25);
        for _ in 0..20 {
            // SAFETY: pointers are valid and connect_addr is a valid C string.
            if unsafe { hew_node_connect(node1.as_ptr(), connect_addr.as_ptr()) } == 0 {
                connected = true;
                break;
            }
            thread::sleep(backoff);
            backoff = (backoff * 2).min(Duration::from_millis(200));
        }
        assert!(connected, "node1 failed to connect to node2");

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

        let handshake_complete = (0..80).any(|i| {
            // SAFETY: node pointers and conn manager pointers are valid while nodes live.
            let ready = unsafe {
                let n1 = &*node1.as_ptr();
                let n2 = &*node2.as_ptr();
                connection::hew_connmgr_count(n1.conn_mgr) > 0
                    && connection::hew_connmgr_count(n2.conn_mgr) > 0
            };
            if !ready {
                let sleep_ms = if i < 20 {
                    25
                } else if i < 50 {
                    50
                } else {
                    100
                };
                thread::sleep(Duration::from_millis(sleep_ms));
            }
            ready
        });
        assert!(handshake_complete, "connection handshake did not complete");

        // SAFETY: pointers remain valid until dropped.
        unsafe {
            let _ = crate::registry::hew_registry_unregister(actor_name.as_ptr());
            assert_eq!(hew_node_stop(node1.as_ptr()), 0);
            assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        }

        crate::registry::hew_registry_clear();
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
    fn reply_table_register_and_complete() {
        let (id, pending) = REPLY_TABLE.register();
        assert!(id > 0);

        // Complete the pending reply.
        let payload = vec![1, 2, 3, 4];
        assert!(REPLY_TABLE.complete(id, payload.clone()));

        // The condvar should be signalled and data deposited.
        let guard = pending
            .data
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert_eq!(guard.as_deref(), Some(payload.as_slice()));
    }

    #[test]
    fn reply_table_complete_unknown_returns_false() {
        assert!(!REPLY_TABLE.complete(u64::MAX - 1, vec![42]));
    }

    #[test]
    fn reply_table_remove_prevents_completion() {
        let (id, _pending) = REPLY_TABLE.register();
        REPLY_TABLE.remove(id);
        assert!(!REPLY_TABLE.complete(id, vec![99]));
    }

    #[test]
    fn reply_table_concurrent_complete_wakes_waiter() {
        let (id, pending) = REPLY_TABLE.register();
        let pending_clone = Arc::clone(&pending);

        let handle = thread::spawn(move || {
            thread::sleep(Duration::from_millis(10));
            REPLY_TABLE.complete(id, vec![10, 20]);
        });

        // Wait on the condvar.
        let mut guard = pending_clone
            .data
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
        assert_eq!(guard.as_deref(), Some(&[10, 20][..]));

        handle.join().expect("completer thread panicked");
    }

    #[test]
    fn remote_lookup_via_registry_gossip() {
        // Verify that a registry gossip callback populates remote_names
        // and that hew_node_lookup falls through to it.
        let _guard = NODE_TEST_LOCK.lock_or_recover();
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
        let _guard = NODE_TEST_LOCK.lock_or_recover();
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
}
