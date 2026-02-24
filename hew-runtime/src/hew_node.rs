//! Unified distributed node runtime.
//!
//! Integrates transport, connection manager, SWIM membership, and
//! name/actor registry wiring.

use std::collections::HashMap;
use std::ffi::{c_char, c_int, c_void, CStr, CString};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicU16, AtomicU8, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};

use crate::cluster::{self, ClusterConfig, HewCluster};
use crate::connection::{self, HewConnMgr};
use crate::transport::{self, HewTransport, HewTransportOps, HEW_CONN_INVALID};

const NODE_STATE_STARTING: u8 = 0;
const NODE_STATE_RUNNING: u8 = 1;
const NODE_STATE_STOPPING: u8 = 2;
const NODE_STATE_STOPPED: u8 = 3;

/// Node-local distributed registry state.
#[repr(C)]
#[derive(Debug, Default)]
pub struct HewRegistry {
    remote_names: Mutex<HashMap<String, u64>>,
}

struct SendTransport(*mut HewTransport);
// SAFETY: transport implementations are internally synchronized and used via
// their vtable APIs.
unsafe impl Send for SendTransport {}

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
) {
    // SAFETY: hew_actor_send_by_id deep-copies payload and validates actor presence.
    let _ =
        unsafe { crate::actor::hew_actor_send_by_id(target_actor_id, msg_type, data.cast(), size) };
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

fn accept_loop(transport: SendTransport, conn_mgr: SendConnMgr, stop: Arc<AtomicBool>) {
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

/// Create a new unified distributed node runtime.
///
/// # Safety
///
/// `bind_addr` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_new(node_id: u16, bind_addr: *const c_char) -> *mut HewNode {
    if bind_addr.is_null() {
        return ptr::null_mut();
    }

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
///
/// # Safety
///
/// `node` must be a valid pointer returned by [`hew_node_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_node_start(node: *mut HewNode) -> c_int {
    if node.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees node pointer is valid.
    let node = unsafe { &mut *node };

    let current = node.state.load(Ordering::Acquire);
    if current == NODE_STATE_RUNNING {
        return 0;
    }
    if current != NODE_STATE_STOPPED {
        return -1;
    }
    node.state.store(NODE_STATE_STARTING, Ordering::Release);

    crate::pid::hew_pid_set_local_node(node.node_id);

    if node.transport.is_null() {
        // SAFETY: constructor returns owned transport pointer or null.
        node.transport = unsafe { transport::hew_transport_tcp_new() };
        if node.transport.is_null() {
            node.state.store(NODE_STATE_STOPPED, Ordering::Release);
            return -1;
        }
    }

    // SAFETY: transport was just created or previously assigned and validated by caller.
    let t = unsafe { &*node.transport };
    node.transport_ops = t.ops;
    if node.transport_ops.is_null() {
        node.state.store(NODE_STATE_STOPPED, Ordering::Release);
        return -1;
    }
    // SAFETY: checked non-null above.
    let ops = unsafe { &*node.transport_ops };

    let Some(listen_fn) = ops.listen else {
        node.state.store(NODE_STATE_STOPPED, Ordering::Release);
        return -1;
    };
    // SAFETY: transport implementation is valid.
    if unsafe { listen_fn(t.r#impl, node.bind_addr) } < 0 {
        node.state.store(NODE_STATE_STOPPED, Ordering::Release);
        return -1;
    }

    if node.conn_mgr.is_null() {
        // SAFETY: transport pointer valid for manager lifetime.
        node.conn_mgr =
            unsafe { connection::hew_connmgr_new(node.transport, Some(node_inbound_router)) };
        if node.conn_mgr.is_null() {
            node.state.store(NODE_STATE_STOPPED, Ordering::Release);
            return -1;
        }
    }

    if node.cluster.is_null() {
        let cfg = ClusterConfig {
            local_node_id: node.node_id,
            ..ClusterConfig::default()
        };
        // SAFETY: config pointer is valid for this call.
        node.cluster = unsafe { cluster::hew_cluster_new(&raw const cfg) };
        if node.cluster.is_null() {
            node.state.store(NODE_STATE_STOPPED, Ordering::Release);
            return -1;
        }
    }

    // SAFETY: cluster pointer valid; bind_addr points to a stable strdup buffer.
    let _ = unsafe { cluster::hew_cluster_join(node.cluster, node.node_id, node.bind_addr) };

    node.accept_stop.store(false, Ordering::Release);
    let stop = Arc::clone(&node.accept_stop);
    let transport = SendTransport(node.transport);
    let conn_mgr = SendConnMgr(node.conn_mgr);
    let thread_name = format!("hew-node-accept-{}", node.node_id);
    let handle = thread::Builder::new()
        .name(thread_name)
        .spawn(move || accept_loop(transport, conn_mgr, stop));
    match handle {
        Ok(h) => {
            let mut guard = match node.accept_thread.lock() {
                Ok(g) => g,
                Err(e) => e.into_inner(),
            };
            *guard = Some(h);
        }
        Err(_) => {
            node.state.store(NODE_STATE_STOPPED, Ordering::Release);
            return -1;
        }
    }

    node.state.store(NODE_STATE_RUNNING, Ordering::Release);
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
        return -1;
    }
    // SAFETY: caller guarantees node pointer is valid.
    let node = unsafe { &mut *node };
    if node.state.load(Ordering::Acquire) == NODE_STATE_STOPPED {
        return 0;
    }

    node.state.store(NODE_STATE_STOPPING, Ordering::Release);
    node.accept_stop.store(true, Ordering::Release);
    {
        let mut guard = match node.accept_thread.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        if let Some(handle) = guard.take() {
            let _ = handle.join();
        }
    }

    if !node.cluster.is_null() {
        // SAFETY: valid cluster pointer.
        unsafe { cluster::hew_cluster_leave(node.cluster) };
        // SAFETY: valid cluster pointer from hew_cluster_new.
        unsafe { cluster::hew_cluster_free(node.cluster) };
        node.cluster = ptr::null_mut();
    }

    if !node.conn_mgr.is_null() {
        // SAFETY: valid manager pointer from hew_connmgr_new.
        unsafe { connection::hew_connmgr_free(node.conn_mgr) };
        node.conn_mgr = ptr::null_mut();
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
        let mut guard = match node.conn_by_node.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
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
    if node.is_null() {
        return;
    }

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
    if node.is_null() || name.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees node pointer validity.
    let node = unsafe { &mut *node };
    if node.registry.is_null() {
        return -1;
    }

    // SAFETY: registry API expects a stable C string pointer.
    let rc = unsafe { crate::registry::hew_registry_register(name, actor as usize as *mut c_void) };
    if rc != 0 {
        return -1;
    }

    // SAFETY: name was checked non-null and is a valid C string by caller contract.
    let key = unsafe { CStr::from_ptr(name) }
        .to_string_lossy()
        .into_owned();
    // SAFETY: registry pointer was allocated in hew_node_new and freed in hew_node_free.
    let reg = unsafe { &*node.registry };
    let mut map = match reg.remote_names.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    map.insert(key, actor);
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
    if node.is_null() || name.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees node pointer validity.
    let node = unsafe { &*node };

    // SAFETY: registry API expects a valid C string pointer.
    let local_ptr = unsafe { crate::registry::hew_registry_lookup(name) };
    if !local_ptr.is_null() {
        return local_ptr as usize as u64;
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
    let map = match reg.remote_names.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
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
    let conn_id = {
        let map = match node.conn_by_node.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        let Some(conn_id) = map.get(&target_node_id) else {
            return -1;
        };
        *conn_id
    };

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
        return -1;
    }
    // SAFETY: caller guarantees node pointer validity.
    let node = unsafe { &mut *node };
    if node.transport.is_null() || node.conn_mgr.is_null() {
        return -1;
    }

    let fallback_node_id = next_peer_node_id(node);
    // SAFETY: addr pointer is non-null and valid by caller contract.
    let Some((peer_node_id, target_addr)) =
        (unsafe { parse_connect_target(addr, fallback_node_id) })
    else {
        return -1;
    };

    // SAFETY: transport pointer validated above.
    let t = unsafe { &*node.transport };
    // SAFETY: valid transport vtable pointer from transport object.
    let Some(ops) = (unsafe { t.ops.as_ref() }) else {
        return -1;
    };
    let Some(connect_fn) = ops.connect else {
        return -1;
    };

    // SAFETY: transport impl and C string are valid.
    let conn_id = unsafe { connect_fn(t.r#impl, target_addr.as_ptr()) };
    if conn_id == HEW_CONN_INVALID {
        return -1;
    }

    // SAFETY: conn_mgr pointer is valid and owned by this node.
    if unsafe { connection::hew_connmgr_add(node.conn_mgr, conn_id) } != 0 {
        if let Some(close_fn) = ops.close_conn {
            // SAFETY: transport impl and conn handle are valid here.
            unsafe { close_fn(t.r#impl, conn_id) };
        }
        return -1;
    }

    {
        let mut map = match node.conn_by_node.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        map.insert(peer_node_id, conn_id);
    }

    if !node.cluster.is_null() {
        // SAFETY: cluster pointer is valid if non-null.
        let _ =
            unsafe { cluster::hew_cluster_join(node.cluster, peer_node_id, target_addr.as_ptr()) };
    }

    0
}
