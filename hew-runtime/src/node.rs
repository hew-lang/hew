//! Distributed actor node.
//!
//! A node manages a transport listener, routes incoming messages to local
//! actors by ID, and maintains connections to peer nodes. Mirrors the C
//! implementation in `hew-codegen/runtime/src/node.c`.

use std::ffi::{c_char, c_int, c_void};
use std::mem::MaybeUninit;
use std::ptr;
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::Mutex;

use crate::transport::{HewTransport, HEW_CONN_INVALID};
use crate::wire::{self, HewWireBuf, HewWireEnvelope};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Maximum peer connections per node.
const NODE_MAX_CONNS: usize = 64;

const HEW_OK: c_int = 0;
const HEW_ERR_SERIALIZE: c_int = -12;
const HEW_ERR_TRANSPORT: c_int = -14;

// ---------------------------------------------------------------------------
// Node struct
// ---------------------------------------------------------------------------

/// Distributed actor node.
pub struct HewNode {
    name: *mut c_char,
    transport: *mut HewTransport,
    conns: [c_int; NODE_MAX_CONNS],
    conn_count: usize,
    running: AtomicI32,
    lock: Mutex<()>,
}

impl std::fmt::Debug for HewNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewNode")
            .field("conn_count", &self.conn_count)
            .finish_non_exhaustive()
    }
}

// SAFETY: Node is designed for multi-threaded access; mutable shared fields
// are protected by the internal mutex.
unsafe impl Send for HewNode {}
// SAFETY: Concurrent reads of atomic `running` are safe; mutable operations
// acquire the internal mutex.
unsafe impl Sync for HewNode {}

// ---------------------------------------------------------------------------
// Public C ABI
// ---------------------------------------------------------------------------

/// Create a new distributed node.
///
/// # Safety
///
/// - `name` must be a valid NUL-terminated C string (or null).
/// - `transport` must be a valid pointer to a [`HewTransport`].
#[no_mangle]
pub unsafe extern "C" fn hew_node_new(
    name: *const c_char,
    transport: *mut HewTransport,
) -> *mut HewNode {
    if transport.is_null() {
        return ptr::null_mut();
    }

    let name_copy = if name.is_null() {
        ptr::null_mut()
    } else {
        // SAFETY: caller guarantees `name` is a valid C string.
        unsafe { libc::strdup(name) }
    };

    let node = Box::new(HewNode {
        name: name_copy,
        transport,
        conns: [HEW_CONN_INVALID; NODE_MAX_CONNS],
        conn_count: 0,
        running: AtomicI32::new(0),
        lock: Mutex::new(()),
    });

    Box::into_raw(node)
}

/// Start listening for incoming connections on the given address.
///
/// # Safety
///
/// - `node` must be a valid pointer returned by [`hew_node_new`].
/// - `address` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_listen(node: *mut HewNode, address: *const c_char) -> c_int {
    if node.is_null() || address.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees pointers are valid.
    let n = unsafe { &mut *node };
    // SAFETY: caller guarantees node transport pointer is valid.
    let t = unsafe { &*n.transport };
    // SAFETY: transport ops vtable pointer is valid.
    let ops = unsafe { &*t.ops };

    let Some(listen_fn) = ops.listen else {
        return -1;
    };

    // SAFETY: transport impl is valid.
    let rc = unsafe { listen_fn(t.r#impl, address) };
    if rc < 0 {
        return -1;
    }

    n.running.store(1, Ordering::Release);
    0
}

/// Connect to a peer node at the given address.
///
/// # Safety
///
/// - `node` must be a valid pointer returned by [`hew_node_new`].
/// - `address` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_node_connect(node: *mut HewNode, address: *const c_char) -> c_int {
    if node.is_null() || address.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees pointers are valid.
    let n = unsafe { &mut *node };
    // SAFETY: caller guarantees node transport pointer is valid.
    let t = unsafe { &*n.transport };
    // SAFETY: transport ops vtable pointer is valid.
    let ops = unsafe { &*t.ops };

    let Some(connect_fn) = ops.connect else {
        return -1;
    };

    // SAFETY: transport impl is valid.
    let conn = unsafe { connect_fn(t.r#impl, address) };
    if conn == HEW_CONN_INVALID {
        return -1;
    }

    let Ok(_guard) = n.lock.lock() else {
        return -1;
    };
    if n.conn_count >= NODE_MAX_CONNS {
        // Close the connection we just opened.
        if let Some(close_fn) = ops.close_conn {
            // SAFETY: transport impl is valid.
            unsafe { close_fn(t.r#impl, conn) };
        }
        return -1;
    }
    n.conns[n.conn_count] = conn;
    n.conn_count += 1;
    0
}

/// Send a message to a remote actor by ID using the first connection.
///
/// # Safety
///
/// - `node` must be a valid pointer returned by [`hew_node_new`].
/// - `data` must be valid for `size` bytes (or null when `size` is 0).
#[no_mangle]
pub unsafe extern "C" fn hew_node_send_to(
    node: *mut HewNode,
    actor_id: u64,
    msg_type: c_int,
    data: *mut c_void,
    size: usize,
) -> c_int {
    if node.is_null() {
        return HEW_ERR_TRANSPORT;
    }
    // SAFETY: caller guarantees node is valid.
    let n = unsafe { &*node };

    let conn = {
        let Ok(_guard) = n.lock.lock() else {
            return HEW_ERR_TRANSPORT;
        };
        if n.conn_count == 0 {
            return HEW_ERR_TRANSPORT;
        }
        n.conns[0]
    };

    // Build envelope.
    let env = HewWireEnvelope {
        target_actor_id: actor_id,
        source_actor_id: 0,
        msg_type,
        #[expect(clippy::cast_possible_truncation, reason = "payload bounded by caller")]
        payload_size: size as u32,
        payload: data.cast::<u8>(),
    };

    let mut buf = MaybeUninit::<HewWireBuf>::uninit();
    // SAFETY: initialising a wire buffer.
    unsafe { wire::hew_wire_buf_init(buf.as_mut_ptr()) };
    let buf_ptr = buf.as_mut_ptr();

    // SAFETY: buf and env are valid stack locals.
    if unsafe { wire::hew_wire_encode_envelope(buf_ptr, &raw const env) } != 0 {
        // SAFETY: buf was initialised.
        unsafe { wire::hew_wire_buf_free(buf_ptr) };
        return HEW_ERR_SERIALIZE;
    }

    // SAFETY: transport is valid.
    // SAFETY: caller guarantees node transport pointer is valid.
    let t = unsafe { &*n.transport };
    // SAFETY: transport ops vtable pointer is valid.
    let ops = unsafe { &*t.ops };
    let Some(send_fn) = ops.send else {
        // SAFETY: buf was initialised.
        unsafe { wire::hew_wire_buf_free(buf_ptr) };
        return HEW_ERR_TRANSPORT;
    };

    // SAFETY: buf was successfully encoded.
    let b = unsafe { &*buf_ptr };
    // SAFETY: transport impl and connection are valid; `b.data` points to the
    // encoded buffer with `b.len` bytes produced by a successful encode above.
    let sent = unsafe { send_fn(t.r#impl, conn, b.data.cast::<c_void>(), b.len) };

    // SAFETY: buf was initialised.
    unsafe { wire::hew_wire_buf_free(buf_ptr) };

    if sent > 0 {
        HEW_OK
    } else {
        HEW_ERR_TRANSPORT
    }
}

/// Accept a pending connection (with optional timeout).
///
/// # Safety
///
/// `node` must be a valid pointer returned by [`hew_node_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_node_accept(node: *mut HewNode, timeout_ms: c_int) -> c_int {
    if node.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees node is valid.
    let n = unsafe { &mut *node };
    // SAFETY: caller guarantees node transport pointer is valid.
    let t = unsafe { &*n.transport };
    // SAFETY: transport ops vtable pointer is valid.
    let ops = unsafe { &*t.ops };

    let Some(accept_fn) = ops.accept else {
        return -1;
    };

    // SAFETY: transport impl is valid.
    let conn = unsafe { accept_fn(t.r#impl, timeout_ms) };
    if conn == HEW_CONN_INVALID {
        return -1;
    }

    let Ok(_guard) = n.lock.lock() else {
        return -1;
    };
    if n.conn_count >= NODE_MAX_CONNS {
        if let Some(close_fn) = ops.close_conn {
            // SAFETY: transport impl is valid.
            unsafe { close_fn(t.r#impl, conn) };
        }
        return -1;
    }
    n.conns[n.conn_count] = conn;
    n.conn_count += 1;
    0
}

/// Stop the node: close all connections and destroy the transport.
///
/// # Safety
///
/// `node` must be a valid pointer returned by [`hew_node_new`]. The pointer
/// must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_node_stop(node: *mut HewNode) {
    if node.is_null() {
        return;
    }
    // SAFETY: caller guarantees node is valid and surrenders ownership.
    let n = unsafe { Box::from_raw(node) };

    n.running.store(0, Ordering::Release);

    let _guard = match n.lock.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };

    // Close all connections.
    if !n.transport.is_null() {
        // SAFETY: transport is valid.
        // SAFETY: caller guarantees node transport pointer is valid.
        let t = unsafe { &*n.transport };
        // SAFETY: transport ops vtable pointer is valid.
        let ops = unsafe { &*t.ops };

        for i in 0..n.conn_count {
            if n.conns[i] != HEW_CONN_INVALID {
                if let Some(close_fn) = ops.close_conn {
                    // SAFETY: transport impl is valid.
                    unsafe { close_fn(t.r#impl, n.conns[i]) };
                }
            }
        }

        // Destroy the transport.
        if let Some(destroy_fn) = ops.destroy {
            // SAFETY: transport impl is valid.
            unsafe { destroy_fn(t.r#impl) };
        }
        // SAFETY: transport was heap-allocated.
        let _ = unsafe { Box::from_raw(n.transport) };
    }

    // Free name.
    if !n.name.is_null() {
        // SAFETY: name was allocated with libc::strdup.
        unsafe { libc::free(n.name.cast::<c_void>()) };
    }

    // `n` (the Box) drops here.
}

/// Free a node (alias for [`hew_node_stop`]).
///
/// # Safety
///
/// Same as [`hew_node_stop`].
#[no_mangle]
pub unsafe extern "C" fn hew_node_free(node: *mut HewNode) {
    // SAFETY: forwarded to hew_node_stop.
    unsafe { hew_node_stop(node) };
}
