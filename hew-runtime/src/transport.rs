//! Network transport abstraction with TCP and Unix socket implementations.
//!
//! Provides a vtable-based transport interface (`HewTransportOps`) and two
//! concrete implementations (TCP, Unix). Also defines `HewActorRef` for
//! network-transparent actor references.
//!
//! The TCP and Unix transports use length-prefixed (4-byte little-endian)
//! framing for send/recv. Connections are stored in a fixed-size array.

use std::collections::HashMap;
use std::ffi::{c_char, c_int, c_void, CStr, CString};
use std::io::{Read, Write};
use std::mem;
use std::net::{Shutdown, SocketAddr, TcpListener, TcpStream, ToSocketAddrs};
use std::sync::{atomic::Ordering, LazyLock, Mutex, RwLock};

use socket2::{Domain, Protocol, SockAddr, Socket, Type};

use crate::actor::{self, HewActor};
use crate::internal::types::HewActorState;
use crate::set_last_error;
use crate::wire::{self, HewWireBuf, HewWireEnvelope};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Invalid connection sentinel.
pub const HEW_CONN_INVALID: c_int = -1;

/// Maximum number of connections stored per transport.
const MAX_CONNS: usize = 64;
/// Maximum accepted framed payload size (16 MiB).
const MAX_FRAME_SIZE: usize = 16 * 1024 * 1024;

// Error codes matching the C header.
const HEW_OK: c_int = 0;
const HEW_ERR_ACTOR_STOPPED: c_int = -2;
const HEW_ERR_SERIALIZE: c_int = -12;
const HEW_ERR_TRANSPORT: c_int = -14;

// ---------------------------------------------------------------------------
// Transport vtable
// ---------------------------------------------------------------------------

/// Function-pointer vtable for pluggable transports.
#[repr(C)]
#[derive(Debug)]
pub struct HewTransportOps {
    pub connect: Option<unsafe extern "C" fn(*mut c_void, *const c_char) -> c_int>,
    pub listen: Option<unsafe extern "C" fn(*mut c_void, *const c_char) -> c_int>,
    pub accept: Option<unsafe extern "C" fn(*mut c_void, c_int) -> c_int>,
    pub send: Option<unsafe extern "C" fn(*mut c_void, c_int, *const c_void, usize) -> c_int>,
    pub recv: Option<unsafe extern "C" fn(*mut c_void, c_int, *mut c_void, usize) -> c_int>,
    pub close_conn: Option<unsafe extern "C" fn(*mut c_void, c_int)>,
    pub destroy: Option<unsafe extern "C" fn(*mut c_void)>,
}

/// Transport handle: ops vtable + opaque implementation pointer.
#[repr(C)]
#[derive(Debug)]
pub struct HewTransport {
    pub ops: *const HewTransportOps,
    pub r#impl: *mut c_void,
}

// SAFETY: Transport handles are passed between threads in the node layer.
// The implementations use `Mutex` or fd-based I/O which is thread-safe.
unsafe impl Send for HewTransport {}
// SAFETY: Concurrent access is serialised by the node's mutex.
unsafe impl Sync for HewTransport {}

// ---------------------------------------------------------------------------
// ActorRef — network-transparent actor reference
// ---------------------------------------------------------------------------

/// Remote portion of an actor reference.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewActorRefRemote {
    pub actor_id: u64,
    pub conn: c_int,
    pub transport: *mut HewTransport,
}

/// Data payload of an actor reference (union).
///
/// Active variant is determined by `HewActorRef::kind`.
#[repr(C)]
pub union HewActorRefData {
    pub local: *mut HewActor,
    pub remote: HewActorRefRemote,
}

impl std::fmt::Debug for HewActorRefData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("HewActorRefData { .. }")
    }
}

/// Unified local/remote actor reference.
#[repr(C)]
#[derive(Debug)]
pub struct HewActorRef {
    /// 0 = LOCAL, 1 = REMOTE
    pub kind: c_int,
    pub data: HewActorRefData,
}

const ACTOR_REF_LOCAL: c_int = 0;
const ACTOR_REF_REMOTE: c_int = 1;

/// Create a local actor reference.
///
/// # Safety
///
/// `actor` must be a valid pointer to a live [`HewActor`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ref_local(actor: *mut HewActor) -> HewActorRef {
    HewActorRef {
        kind: ACTOR_REF_LOCAL,
        data: HewActorRefData { local: actor },
    }
}

/// Create a remote actor reference.
///
/// # Safety
///
/// `transport` must be a valid pointer to a live [`HewTransport`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ref_remote(
    actor_id: u64,
    conn: c_int,
    transport: *mut HewTransport,
) -> HewActorRef {
    HewActorRef {
        kind: ACTOR_REF_REMOTE,
        data: HewActorRefData {
            remote: HewActorRefRemote {
                actor_id,
                conn,
                transport,
            },
        },
    }
}

/// Encode an actor-message envelope and send it over a transport connection.
///
/// Returns `HEW_OK` (0) on success, `HEW_ERR_SERIALIZE` (-12) if encoding
/// fails, or `HEW_ERR_TRANSPORT` (-14) if the transport has no send
/// implementation or the send fails.
///
/// # Safety
///
/// - `transport` must be a valid, non-null pointer to a [`HewTransport`] whose
///   `ops` vtable and `impl` remain valid for the duration of the call.
/// - `conn` must be a valid connection handle for `transport`.
/// - `payload` must be valid for `payload_len` readable bytes (or null when
///   `payload_len` is 0).
pub(crate) unsafe fn wire_send_envelope(
    transport: *mut HewTransport,
    conn: c_int,
    target_actor_id: u64,
    source_actor_id: u64,
    msg_type: i32,
    payload: *mut u8,
    payload_len: usize,
) -> c_int {
    #[expect(clippy::cast_possible_truncation, reason = "payload bounded by caller")]
    let env = HewWireEnvelope {
        target_actor_id,
        source_actor_id,
        msg_type,
        payload_size: payload_len as u32,
        payload,
    };
    // SAFETY: zeroed is valid for HewWireBuf (null data pointer, zero lengths).
    let mut buf: HewWireBuf = unsafe { mem::zeroed() };
    // SAFETY: buf is a valid stack allocation.
    unsafe { wire::hew_wire_buf_init(&raw mut buf) };
    // SAFETY: buf and env are valid stack locals; payload validity is the caller's responsibility.
    if unsafe { wire::hew_wire_encode_envelope(&raw mut buf, &raw const env) } != 0 {
        // SAFETY: buf was initialised above.
        unsafe { wire::hew_wire_buf_free(&raw mut buf) };
        return HEW_ERR_SERIALIZE;
    }
    // SAFETY: transport is valid per caller contract.
    let t = unsafe { &*transport };
    let result = if let Some(ops) = unsafe { t.ops.as_ref() } {
        if let Some(send_fn) = ops.send {
            // SAFETY: buf was successfully encoded; data/len are valid.
            unsafe { send_fn(t.r#impl, conn, buf.data.cast::<c_void>(), buf.len) }
        } else {
            -1
        }
    } else {
        -1
    };
    // SAFETY: buf was initialised above.
    unsafe { wire::hew_wire_buf_free(&raw mut buf) };
    if result > 0 {
        HEW_OK
    } else {
        HEW_ERR_TRANSPORT
    }
}

/// Send a message through an actor reference.
///
/// LOCAL path: direct call to `hew_actor_send`.
/// REMOTE path: encode as HBF envelope, send over transport.
///
/// # Safety
///
/// - `ref_ptr` must point to a valid [`HewActorRef`].
/// - `data` must be valid for `size` bytes (or null when `size` is 0).
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ref_send(
    ref_ptr: *mut HewActorRef,
    msg_type: c_int,
    data: *mut c_void,
    size: usize,
) -> c_int {
    cabi_guard!(ref_ptr.is_null(), HEW_ERR_TRANSPORT);
    // SAFETY: caller guarantees `ref_ptr` is valid.
    let r = unsafe { &*ref_ptr };

    if r.kind == ACTOR_REF_LOCAL {
        // SAFETY: local union variant is active when kind == LOCAL.
        let actor = unsafe { r.data.local };
        if actor.is_null() {
            return HEW_ERR_ACTOR_STOPPED;
        }
        // SAFETY: caller guarantees actor and data validity.
        unsafe {
            actor::hew_actor_send(actor, msg_type, data, size);
        }
        return HEW_OK;
    }

    // Remote path.
    // SAFETY: remote union variant is active when kind == REMOTE.
    let remote = unsafe { r.data.remote };
    if remote.transport.is_null() || remote.conn == HEW_CONN_INVALID {
        return HEW_ERR_TRANSPORT;
    }

    // SAFETY: remote.transport and remote.conn are valid per the earlier null-check;
    //         data is valid for size bytes per caller contract.
    unsafe {
        wire_send_envelope(
            remote.transport,
            remote.conn,
            remote.actor_id,
            0,
            msg_type,
            data.cast::<u8>(),
            size,
        )
    }
}

/// Check if an actor reference refers to a local actor.
///
/// # Safety
///
/// `ref_ptr` must be a valid pointer to a [`HewActorRef`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ref_is_local(ref_ptr: *const HewActorRef) -> c_int {
    if ref_ptr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees the pointer is valid.
    c_int::from(unsafe { (*ref_ptr).kind } == ACTOR_REF_LOCAL)
}

/// Check if a referenced actor is considered alive.
///
/// LOCAL: checks actor state. REMOTE: alive if connection is valid.
///
/// # Safety
///
/// `ref_ptr` must be a valid pointer to a [`HewActorRef`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ref_is_alive(ref_ptr: *const HewActorRef) -> c_int {
    cabi_guard!(ref_ptr.is_null(), 0);
    // SAFETY: caller guarantees the pointer is valid.
    let r = unsafe { &*ref_ptr };

    if r.kind == ACTOR_REF_LOCAL {
        // SAFETY: local variant is active.
        let actor = unsafe { r.data.local };
        if actor.is_null() {
            return 0;
        }
        // SAFETY: actor pointer is valid per caller contract.
        let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
        let alive =
            state != HewActorState::Stopped as i32 && state != HewActorState::Crashed as i32;
        return c_int::from(alive);
    }

    // Remote: alive if connection handle is valid.
    // SAFETY: remote variant is active.
    c_int::from(unsafe { r.data.remote.conn } != HEW_CONN_INVALID)
}

// ===========================================================================
// TCP transport
// ===========================================================================

/// Internal state for the TCP transport.
struct TcpTransport {
    listen_sock: Option<Socket>,
    conns: RwLock<Vec<Option<Socket>>>,
}

impl TcpTransport {
    fn new() -> Self {
        Self {
            listen_sock: None,
            conns: RwLock::new((0..MAX_CONNS).map(|_| None).collect()),
        }
    }

    fn store_conn(&self, sock: Socket) -> c_int {
        let Ok(mut conns) = self.conns.write() else {
            return HEW_CONN_INVALID;
        };
        for (i, slot) in conns.iter_mut().enumerate() {
            if slot.is_none() {
                *slot = Some(sock);
                #[expect(clippy::cast_possible_truncation, reason = "MAX_CONNS fits in c_int")]
                #[expect(clippy::cast_possible_wrap, reason = "MAX_CONNS fits in c_int")]
                return i as c_int;
            }
        }
        HEW_CONN_INVALID
    }

    fn get_conn(&self, id: c_int) -> Option<Socket> {
        if id < 0 {
            return None;
        }
        #[expect(clippy::cast_sign_loss, reason = "guarded by id >= 0")]
        let idx = id as usize;
        let conns = self.conns.read().ok()?;
        conns
            .get(idx)
            .and_then(Option::as_ref)
            .and_then(|sock| sock.try_clone().ok())
    }

    fn remove_conn(&self, id: c_int) {
        if id >= 0 {
            #[expect(clippy::cast_sign_loss, reason = "guarded by id >= 0")]
            let idx = id as usize;
            if idx < MAX_CONNS {
                if let Ok(mut conns) = self.conns.write() {
                    conns[idx] = None;
                }
            }
        }
    }
}

/// Parse a "host:port" address string.
fn parse_host_port(addr: &str) -> Option<SocketAddr> {
    addr.parse::<SocketAddr>().ok()
}

/// Send exactly `buf` bytes over a socket with length-prefixed framing.
fn framed_send(sock: &Socket, data: &[u8]) -> c_int {
    #[expect(clippy::cast_possible_truncation, reason = "payload bounded by caller")]
    let frame_len = data.len() as u32;
    let header = frame_len.to_le_bytes();

    // Write header.
    let mut written = 0usize;
    while written < 4 {
        match (&*sock).write(&header[written..]) {
            Ok(0) => {
                set_last_error("transport framed_send: peer closed while writing header");
                return -1;
            }
            Err(e) => {
                set_last_error(format!("transport framed_send: header write failed: {e}"));
                return -1;
            }
            Ok(n) => written += n,
        }
    }
    // Write payload.
    written = 0;
    while written < data.len() {
        match (&*sock).write(&data[written..]) {
            Ok(0) => {
                set_last_error("transport framed_send: peer closed while writing payload");
                return -1;
            }
            Err(e) => {
                set_last_error(format!("transport framed_send: payload write failed: {e}"));
                return -1;
            }
            Ok(n) => written += n,
        }
    }
    #[expect(clippy::cast_possible_truncation, reason = "payload bounded by caller")]
    #[expect(clippy::cast_possible_wrap, reason = "C ABI: value fits in i32")]
    {
        data.len() as c_int
    }
}

/// Receive a length-prefixed frame into `buf`. Returns bytes read or -1.
fn framed_recv(sock: &Socket, buf: &mut [u8]) -> c_int {
    // Read 4-byte LE length.
    let mut header = [0u8; 4];
    let mut read_count = 0usize;
    while read_count < 4 {
        match (&*sock).read(&mut header[read_count..]) {
            Ok(0) => {
                set_last_error("transport framed_recv: peer closed while reading header");
                return -1;
            }
            Err(e) => {
                set_last_error(format!("transport framed_recv: header read failed: {e}"));
                return -1;
            }
            Ok(n) => read_count += n,
        }
    }
    let frame_len = u32::from_le_bytes(header) as usize;
    if frame_len > MAX_FRAME_SIZE {
        let _ = sock.shutdown(Shutdown::Both);
        set_last_error(format!(
            "transport framed_recv: frame exceeds max size ({frame_len} > {MAX_FRAME_SIZE})"
        ));
        return -1;
    }
    if frame_len > buf.len() {
        let _ = sock.shutdown(Shutdown::Both);
        set_last_error(format!(
            "transport framed_recv: frame too large ({frame_len} > {}), closing connection",
            buf.len()
        ));
        return -1;
    }
    // Read payload.
    read_count = 0;
    while read_count < frame_len {
        match (&*sock).read(&mut buf[read_count..frame_len]) {
            Ok(0) => {
                set_last_error("transport framed_recv: peer closed while reading payload");
                return -1;
            }
            Err(e) => {
                set_last_error(format!("transport framed_recv: payload read failed: {e}"));
                return -1;
            }
            Ok(n) => read_count += n,
        }
    }
    #[expect(clippy::cast_possible_truncation, reason = "frame_len fits in c_int")]
    #[expect(clippy::cast_possible_wrap, reason = "C ABI: value fits in i32")]
    {
        frame_len as c_int
    }
}

fn accept_with_optional_timeout(listen_sock: &Socket, timeout_ms: c_int) -> Option<(Socket, bool)> {
    if timeout_ms >= 0 {
        let _ = listen_sock.set_nonblocking(true);
        #[expect(clippy::cast_sign_loss, reason = "guarded by >= 0")]
        let dur = std::time::Duration::from_millis(timeout_ms as u64);
        let start = std::time::Instant::now();
        loop {
            match listen_sock.accept() {
                Ok((conn, _)) => {
                    let _ = listen_sock.set_nonblocking(false);
                    return Some((conn, true));
                }
                Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    if start.elapsed() >= dur {
                        let _ = listen_sock.set_nonblocking(false);
                        return None;
                    }
                    std::thread::sleep(std::time::Duration::from_millis(1));
                }
                Err(_) => {
                    let _ = listen_sock.set_nonblocking(false);
                    return None;
                }
            }
        }
    }
    listen_sock.accept().ok().map(|(conn, _)| (conn, false))
}

// ---- TCP vtable callbacks --------------------------------------------------

unsafe extern "C" fn tcp_connect(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
    if impl_ptr.is_null() || address.is_null() {
        return HEW_CONN_INVALID;
    }
    // SAFETY: caller guarantees address is a valid C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(address) }.to_str() else {
        return HEW_CONN_INVALID;
    };
    let Some(sock_addr) = parse_host_port(addr_str) else {
        return HEW_CONN_INVALID;
    };

    let Ok(socket) = Socket::new(Domain::IPV4, Type::STREAM, Some(Protocol::TCP)) else {
        return HEW_CONN_INVALID;
    };
    if socket.connect(&SockAddr::from(sock_addr)).is_err() {
        return HEW_CONN_INVALID;
    }
    let _ = socket.set_tcp_nodelay(true);

    // SAFETY: impl_ptr points to a valid TcpTransport.
    let tcp = unsafe { &*impl_ptr.cast::<TcpTransport>() };
    tcp.store_conn(socket)
}

unsafe extern "C" fn tcp_listen(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
    if impl_ptr.is_null() || address.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees address is a valid C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(address) }.to_str() else {
        return -1;
    };
    let Some(sock_addr) = parse_host_port(addr_str) else {
        return -1;
    };

    let Ok(socket) = Socket::new(Domain::IPV4, Type::STREAM, Some(Protocol::TCP)) else {
        return -1;
    };
    let _ = socket.set_reuse_address(true);
    if socket.bind(&SockAddr::from(sock_addr)).is_err() {
        return -1;
    }
    if socket.listen(128).is_err() {
        return -1;
    }

    // SAFETY: impl_ptr points to a valid TcpTransport.
    let tcp = unsafe { &mut *impl_ptr.cast::<TcpTransport>() };
    tcp.listen_sock = Some(socket);
    0
}

unsafe extern "C" fn tcp_accept(impl_ptr: *mut c_void, timeout_ms: c_int) -> c_int {
    if impl_ptr.is_null() {
        return HEW_CONN_INVALID;
    }
    // SAFETY: impl_ptr points to a valid TcpTransport.
    let tcp = unsafe { &*impl_ptr.cast::<TcpTransport>() };
    let Some(listen_sock) = &tcp.listen_sock else {
        return HEW_CONN_INVALID;
    };

    let Some((conn, used_timeout)) = accept_with_optional_timeout(listen_sock, timeout_ms) else {
        return HEW_CONN_INVALID;
    };
    let _ = conn.set_tcp_nodelay(true);
    if used_timeout {
        let _ = conn.set_nonblocking(false);
    }
    tcp.store_conn(conn)
}

unsafe extern "C" fn tcp_send(
    impl_ptr: *mut c_void,
    conn: c_int,
    data: *const c_void,
    len: usize,
) -> c_int {
    if impl_ptr.is_null() || data.is_null() {
        return -1;
    }
    // SAFETY: impl_ptr points to a valid TcpTransport.
    let tcp = unsafe { &*impl_ptr.cast::<TcpTransport>() };
    let Some(sock) = tcp.get_conn(conn) else {
        return -1;
    };
    // SAFETY: data is valid for `len` bytes per caller contract.
    let slice = unsafe { std::slice::from_raw_parts(data.cast::<u8>(), len) };
    framed_send(&sock, slice)
}

unsafe extern "C" fn tcp_recv(
    impl_ptr: *mut c_void,
    conn: c_int,
    buf: *mut c_void,
    buf_size: usize,
) -> c_int {
    if impl_ptr.is_null() || buf.is_null() {
        return -1;
    }
    // SAFETY: impl_ptr points to a valid TcpTransport.
    let tcp = unsafe { &*impl_ptr.cast::<TcpTransport>() };
    let Some(sock) = tcp.get_conn(conn) else {
        return -1;
    };
    // SAFETY: buf is valid for buf_size bytes per caller contract.
    let slice = unsafe { std::slice::from_raw_parts_mut(buf.cast::<u8>(), buf_size) };
    framed_recv(&sock, slice)
}

unsafe extern "C" fn tcp_close_conn(impl_ptr: *mut c_void, conn: c_int) {
    if impl_ptr.is_null() {
        return;
    }
    // SAFETY: impl_ptr points to a valid TcpTransport.
    let tcp = unsafe { &*impl_ptr.cast::<TcpTransport>() };
    tcp.remove_conn(conn);
}

unsafe extern "C" fn tcp_destroy(impl_ptr: *mut c_void) {
    if impl_ptr.is_null() {
        return;
    }
    // SAFETY: impl_ptr was created by Box::into_raw in hew_transport_tcp_new.
    let _ = unsafe { Box::from_raw(impl_ptr.cast::<TcpTransport>()) };
}

static TCP_OPS: HewTransportOps = HewTransportOps {
    connect: Some(tcp_connect),
    listen: Some(tcp_listen),
    accept: Some(tcp_accept),
    send: Some(tcp_send),
    recv: Some(tcp_recv),
    close_conn: Some(tcp_close_conn),
    destroy: Some(tcp_destroy),
};

/// Create a new TCP transport.
///
/// # Safety
///
/// The returned pointer must be freed by calling the transport's `destroy` op.
#[no_mangle]
pub unsafe extern "C" fn hew_transport_tcp_new() -> *mut HewTransport {
    let tcp = Box::new(TcpTransport::new());
    let transport = Box::new(HewTransport {
        ops: &raw const TCP_OPS,
        r#impl: Box::into_raw(tcp).cast::<c_void>(),
    });
    Box::into_raw(transport)
}

// ===========================================================================
// Simple TCP API for Hew stdlib (`std::net`)
// ===========================================================================

#[derive(Debug)]
struct TcpApiState {
    next_handle: c_int,
    listeners: HashMap<c_int, TcpListener>,
    streams: HashMap<c_int, TcpStream>,
}

impl TcpApiState {
    fn new() -> Self {
        Self {
            next_handle: 1,
            listeners: HashMap::new(),
            streams: HashMap::new(),
        }
    }

    fn alloc_handle(&mut self) -> c_int {
        let handle = self.next_handle;
        self.next_handle = self.next_handle.saturating_add(1);
        if self.next_handle <= 0 {
            self.next_handle = 1;
        }
        handle
    }
}

static TCP_API_STATE: LazyLock<Mutex<TcpApiState>> =
    LazyLock::new(|| Mutex::new(TcpApiState::new()));

fn tcp_clone_listener(handle: c_int) -> Option<TcpListener> {
    let state = TCP_API_STATE.lock().ok()?;
    state.listeners.get(&handle)?.try_clone().ok()
}

fn tcp_clone_stream(handle: c_int) -> Option<TcpStream> {
    let state = TCP_API_STATE.lock().ok()?;
    state.streams.get(&handle)?.try_clone().ok()
}

/// Open a TCP listener at `addr` (`host:port`).
///
/// Returns a positive listener handle, or -1 on error.
///
/// # Safety
///
/// `addr` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_listen(addr: *const c_char) -> c_int {
    if addr.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `addr` is a valid C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(addr) }.to_str() else {
        return -1;
    };
    // Allow ":port" shorthand — Rust's ToSocketAddrs doesn't resolve empty host.
    let owned;
    let bind_addr = if addr_str.starts_with(':') {
        owned = format!("0.0.0.0{addr_str}");
        owned.as_str()
    } else {
        addr_str
    };
    let Ok(listener) = TcpListener::bind(bind_addr) else {
        return -1;
    };
    let Ok(mut state) = TCP_API_STATE.lock() else {
        return -1;
    };
    let handle = state.alloc_handle();
    state.listeners.insert(handle, listener);
    handle
}

/// Accept one incoming TCP connection from a listener handle.
///
/// Returns a positive connection handle, or -1 on error.
#[no_mangle]
pub extern "C" fn hew_tcp_accept(listener: c_int) -> c_int {
    let Some(listener) = tcp_clone_listener(listener) else {
        return -1;
    };
    let Ok((stream, _)) = listener.accept() else {
        return -1;
    };
    let _ = stream.set_nodelay(true);
    let Ok(mut state) = TCP_API_STATE.lock() else {
        return -1;
    };
    let handle = state.alloc_handle();
    state.streams.insert(handle, stream);
    handle
}

/// Connect to a TCP endpoint at `addr` (`host:port`).
///
/// Returns a positive connection handle, or -1 on error.
///
/// # Safety
///
/// `addr` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_connect(addr: *const c_char) -> c_int {
    if addr.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `addr` is a valid C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(addr) }.to_str() else {
        return -1;
    };
    // Allow ":port" shorthand — connect to localhost.
    let owned;
    let connect_addr = if addr_str.starts_with(':') {
        owned = format!("127.0.0.1{addr_str}");
        owned.as_str()
    } else {
        addr_str
    };
    let Ok(stream) = TcpStream::connect(connect_addr) else {
        return -1;
    };
    let _ = stream.set_nodelay(true);
    let Ok(mut state) = TCP_API_STATE.lock() else {
        return -1;
    };
    let handle = state.alloc_handle();
    state.streams.insert(handle, stream);
    handle
}

/// Set read timeout on a TCP connection handle.
///
/// `timeout_ms < 0` clears the timeout.
/// Returns 0 on success, -1 on error.
#[no_mangle]
pub extern "C" fn hew_tcp_set_read_timeout(fd: c_int, timeout_ms: c_int) -> c_int {
    let Some(stream) = tcp_clone_stream(fd) else {
        return -1;
    };
    let timeout = if timeout_ms < 0 {
        None
    } else {
        let Ok(timeout_ms) = u64::try_from(timeout_ms) else {
            return -1;
        };
        Some(std::time::Duration::from_millis(timeout_ms))
    };
    if stream.set_read_timeout(timeout).is_err() {
        return -1;
    }
    0
}

/// Set write timeout on a TCP connection handle.
///
/// `timeout_ms < 0` clears the timeout.
/// Returns 0 on success, -1 on error.
#[no_mangle]
pub extern "C" fn hew_tcp_set_write_timeout(fd: c_int, timeout_ms: c_int) -> c_int {
    let Some(stream) = tcp_clone_stream(fd) else {
        return -1;
    };
    let timeout = if timeout_ms < 0 {
        None
    } else {
        let Ok(timeout_ms) = u64::try_from(timeout_ms) else {
            return -1;
        };
        Some(std::time::Duration::from_millis(timeout_ms))
    };
    if stream.set_write_timeout(timeout).is_err() {
        return -1;
    }
    0
}

/// Connect to a TCP endpoint with an explicit timeout.
///
/// Returns a positive connection handle, or -1 on error.
///
/// # Safety
///
/// `host` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_connect_timeout(
    host: *const c_char,
    port: c_int,
    timeout_ms: c_int,
) -> c_int {
    if host.is_null() || timeout_ms < 0 {
        return -1;
    }
    let Ok(port) = u16::try_from(port) else {
        return -1;
    };
    let Ok(timeout_ms) = u64::try_from(timeout_ms) else {
        return -1;
    };
    // SAFETY: caller guarantees `host` is a valid C string.
    let Ok(host_str) = unsafe { CStr::from_ptr(host) }.to_str() else {
        return -1;
    };
    let Ok(mut addrs) = (host_str, port).to_socket_addrs() else {
        return -1;
    };
    let Some(sock_addr) = addrs.next() else {
        return -1;
    };
    let Ok(stream) =
        TcpStream::connect_timeout(&sock_addr, std::time::Duration::from_millis(timeout_ms))
    else {
        return -1;
    };
    let _ = stream.set_nodelay(true);
    let Ok(mut state) = TCP_API_STATE.lock() else {
        return -1;
    };
    let handle = state.alloc_handle();
    state.streams.insert(handle, stream);
    handle
}

/// Read up to 8192 bytes from a TCP connection into a new `HewVec`.
///
/// Returns a pointer to a heap-allocated `HewVec` (i32 elements, one per byte).
/// Returns an empty `HewVec` (not null) on EOF or error — callers detect
/// disconnect by checking `is_empty()`.
#[no_mangle]
pub extern "C" fn hew_tcp_read(conn: c_int) -> *mut crate::vec::HewVec {
    // SAFETY: hew_vec_new allocates and returns a valid HewVec; we own it.
    let v = unsafe { crate::vec::hew_vec_new() };
    let Some(mut stream) = tcp_clone_stream(conn) else {
        return v;
    };
    let mut buf = [0u8; 8192];
    match stream.read(&mut buf) {
        Ok(0) | Err(_) => v,
        Ok(n) => {
            for &b in &buf[..n] {
                // SAFETY: v was allocated by hew_vec_new and is non-null.
                unsafe { crate::vec::hew_vec_push_i32(v, i32::from(b)) };
            }
            v
        }
    }
}

/// Write a bytes `HewVec` to a TCP connection.
///
/// Each i32 element in `vec` is written as one byte (low 8 bits).
/// Does NOT append a newline.
/// Returns number of bytes written, or -1 on error.
///
/// # Safety
///
/// `vec` must be a valid, non-null pointer to a `HewVec` created by
/// `hew_vec_new` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_write(conn: c_int, vec: *mut crate::vec::HewVec) -> c_int {
    if vec.is_null() {
        return -1;
    }
    let Some(mut stream) = tcp_clone_stream(conn) else {
        return -1;
    };
    // SAFETY: caller guarantees `vec` is a valid HewVec pointer.
    let len = unsafe { crate::vec::hew_vec_len(vec) };
    #[expect(clippy::cast_sign_loss, reason = "vec len is always non-negative")]
    #[expect(
        clippy::cast_possible_truncation,
        reason = "vec len fits in usize on all platforms"
    )]
    let mut data = Vec::with_capacity(len as usize);
    for i in 0..len {
        // SAFETY: i < len, so index is in bounds.
        let val = unsafe { crate::vec::hew_vec_get_i32(vec, i) };
        #[expect(
            clippy::cast_possible_truncation,
            clippy::cast_sign_loss,
            reason = "byte values fit in u8"
        )]
        data.push(val as u8);
    }
    if stream.write_all(&data).is_err() {
        return -1;
    }
    #[expect(
        clippy::cast_possible_truncation,
        reason = "byte count returned as c_int; practical message sizes fit in i32"
    )]
    {
        len as c_int
    }
}

/// Convert a bytes `HewVec` (Vec<i32>) to a NUL-terminated C string.
///
/// Each i32 element is treated as a byte value (low 8 bits used).
/// The returned pointer is heap-allocated and must be freed by the caller
/// (or left to the Hew runtime's string GC).
///
/// # Safety
///
/// `vec` must be a valid, non-null pointer to a `HewVec` created by
/// `hew_vec_new` (i32 elements). The returned pointer is valid until freed.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_to_string(vec: *mut crate::vec::HewVec) -> *mut c_char {
    if vec.is_null() {
        return CString::new("")
            .map(CString::into_raw)
            .unwrap_or(std::ptr::null_mut());
    }
    // SAFETY: caller guarantees `vec` is a valid HewVec pointer.
    let len = unsafe { crate::vec::hew_vec_len(vec) };
    #[expect(clippy::cast_sign_loss, reason = "vec len is always non-negative")]
    #[expect(
        clippy::cast_possible_truncation,
        reason = "vec len fits in usize on all platforms"
    )]
    let mut bytes = Vec::with_capacity(len as usize);
    for i in 0..len {
        // SAFETY: i < len, so index is in bounds.
        let val = unsafe { crate::vec::hew_vec_get_i32(vec, i) };
        #[expect(
            clippy::cast_possible_truncation,
            clippy::cast_sign_loss,
            reason = "byte values fit in u8"
        )]
        bytes.push(val as u8);
    }
    // Strip embedded NUL bytes so CString::new succeeds.
    bytes.retain(|&b| b != 0);
    CString::new(bytes)
        .map(CString::into_raw)
        .unwrap_or(std::ptr::null_mut())
}

/// Broadcast one message to all open TCP connections except `exclude_conn`.
///
/// Appends `\n` if not already present.
/// Returns number of recipients written to, or -1 on error.
///
/// # Safety
///
/// `msg` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_broadcast_except(
    exclude_conn: c_int,
    msg: *const c_char,
) -> c_int {
    if msg.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `msg` is a valid C string.
    let Ok(text) = unsafe { CStr::from_ptr(msg) }.to_str() else {
        return -1;
    };
    let mut recipients = 0usize;
    let Ok(state) = TCP_API_STATE.lock() else {
        return -1;
    };
    for (conn, stream) in &state.streams {
        if *conn == exclude_conn {
            continue;
        }
        let Ok(mut cloned) = stream.try_clone() else {
            continue;
        };
        if cloned.write_all(text.as_bytes()).is_err() {
            continue;
        }
        if !text.ends_with('\n') && cloned.write_all(b"\n").is_err() {
            continue;
        }
        recipients += 1;
    }
    #[expect(
        clippy::cast_possible_truncation,
        reason = "recipient count is small in demos"
    )]
    #[expect(
        clippy::cast_possible_wrap,
        reason = "recipient count is small in demos"
    )]
    {
        recipients as c_int
    }
}

/// Close either a TCP connection handle or listener handle.
///
/// Returns 0 on success, -1 if handle is unknown.
#[no_mangle]
pub extern "C" fn hew_tcp_close(handle: c_int) -> c_int {
    let Ok(mut state) = TCP_API_STATE.lock() else {
        return -1;
    };
    if let Some(stream) = state.streams.remove(&handle) {
        let _ = stream.shutdown(Shutdown::Both);
        return 0;
    }
    if state.listeners.remove(&handle).is_some() {
        return 0;
    }
    -1
}

// ===========================================================================
// Unix domain socket transport
// ===========================================================================

#[cfg(unix)]
mod unix_transport {
    use super::{
        accept_with_optional_timeout, c_char, c_int, c_void, framed_recv, framed_send, CStr,
        Domain, HewTransport, HewTransportOps, RwLock, SockAddr, Socket, Type, HEW_CONN_INVALID,
        MAX_CONNS,
    };

    /// Internal state for the Unix domain socket transport.
    struct UnixTransport {
        listen_sock: Option<Socket>,
        path: Option<String>,
        conns: RwLock<Vec<Option<Socket>>>,
    }

    impl UnixTransport {
        fn new() -> Self {
            Self {
                listen_sock: None,
                path: None,
                conns: RwLock::new((0..MAX_CONNS).map(|_| None).collect()),
            }
        }

        fn store_conn(&self, sock: Socket) -> c_int {
            let Ok(mut conns) = self.conns.write() else {
                return HEW_CONN_INVALID;
            };
            for (i, slot) in conns.iter_mut().enumerate() {
                if slot.is_none() {
                    *slot = Some(sock);
                    #[expect(
                        clippy::cast_possible_truncation,
                        reason = "MAX_CONNS fits in c_int"
                    )]
                    #[expect(clippy::cast_possible_wrap, reason = "MAX_CONNS fits in c_int")]
                    return i as c_int;
                }
            }
            HEW_CONN_INVALID
        }

        fn get_conn(&self, id: c_int) -> Option<Socket> {
            if id < 0 {
                return None;
            }
            #[expect(clippy::cast_sign_loss, reason = "guarded by id >= 0")]
            let idx = id as usize;
            let conns = self.conns.read().ok()?;
            conns
                .get(idx)
                .and_then(Option::as_ref)
                .and_then(|sock| sock.try_clone().ok())
        }

        fn remove_conn(&self, id: c_int) {
            if id >= 0 {
                #[expect(clippy::cast_sign_loss, reason = "guarded by id >= 0")]
                let idx = id as usize;
                if idx < MAX_CONNS {
                    if let Ok(mut conns) = self.conns.write() {
                        conns[idx] = None;
                    }
                }
            }
        }
    }

    impl Drop for UnixTransport {
        fn drop(&mut self) {
            drop(self.listen_sock.take());
            if let Some(ref path) = self.path {
                let _ = std::fs::remove_file(path);
            }
        }
    }

    // ---- Unix vtable callbacks -------------------------------------------------

    unsafe extern "C" fn unix_connect(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
        if impl_ptr.is_null() || address.is_null() {
            return HEW_CONN_INVALID;
        }
        // SAFETY: caller guarantees address is a valid C string.
        let Ok(addr_str) = unsafe { CStr::from_ptr(address) }.to_str() else {
            return HEW_CONN_INVALID;
        };

        let Ok(socket) = Socket::new(Domain::UNIX, Type::STREAM, None) else {
            return HEW_CONN_INVALID;
        };
        let Ok(sock_addr) = SockAddr::unix(addr_str) else {
            return HEW_CONN_INVALID;
        };
        if socket.connect(&sock_addr).is_err() {
            return HEW_CONN_INVALID;
        }

        // SAFETY: impl_ptr points to a valid UnixTransport.
        let ut = unsafe { &*impl_ptr.cast::<UnixTransport>() };
        ut.store_conn(socket)
    }

    unsafe extern "C" fn unix_listen(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
        if impl_ptr.is_null() || address.is_null() {
            return -1;
        }
        // SAFETY: caller guarantees address is a valid C string.
        let Ok(addr_str) = unsafe { CStr::from_ptr(address) }.to_str() else {
            return -1;
        };

        // Remove existing socket file.
        let _ = std::fs::remove_file(addr_str);

        let Ok(socket) = Socket::new(Domain::UNIX, Type::STREAM, None) else {
            return -1;
        };
        let Ok(sock_addr) = SockAddr::unix(addr_str) else {
            return -1;
        };
        if socket.bind(&sock_addr).is_err() {
            return -1;
        }
        if socket.listen(128).is_err() {
            return -1;
        }

        // SAFETY: impl_ptr points to a valid UnixTransport.
        let ut = unsafe { &mut *impl_ptr.cast::<UnixTransport>() };
        ut.listen_sock = Some(socket);
        ut.path = Some(addr_str.to_owned());
        0
    }

    unsafe extern "C" fn unix_accept(impl_ptr: *mut c_void, timeout_ms: c_int) -> c_int {
        if impl_ptr.is_null() {
            return HEW_CONN_INVALID;
        }
        // SAFETY: impl_ptr points to a valid UnixTransport.
        let ut = unsafe { &*impl_ptr.cast::<UnixTransport>() };
        let Some(listen_sock) = &ut.listen_sock else {
            return HEW_CONN_INVALID;
        };

        match accept_with_optional_timeout(listen_sock, timeout_ms) {
            Some((conn, true)) => {
                let _ = conn.set_nonblocking(false);
                ut.store_conn(conn)
            }
            Some((conn, false)) => ut.store_conn(conn),
            None => HEW_CONN_INVALID,
        }
    }

    unsafe extern "C" fn unix_send(
        impl_ptr: *mut c_void,
        conn: c_int,
        data: *const c_void,
        len: usize,
    ) -> c_int {
        if impl_ptr.is_null() || data.is_null() {
            return -1;
        }
        // SAFETY: impl_ptr points to a valid UnixTransport.
        let ut = unsafe { &*impl_ptr.cast::<UnixTransport>() };
        let Some(sock) = ut.get_conn(conn) else {
            return -1;
        };
        // SAFETY: data is valid for `len` bytes per caller contract.
        let slice = unsafe { std::slice::from_raw_parts(data.cast::<u8>(), len) };
        framed_send(&sock, slice)
    }

    unsafe extern "C" fn unix_recv(
        impl_ptr: *mut c_void,
        conn: c_int,
        buf: *mut c_void,
        buf_size: usize,
    ) -> c_int {
        if impl_ptr.is_null() || buf.is_null() {
            return -1;
        }
        // SAFETY: impl_ptr points to a valid UnixTransport.
        let ut = unsafe { &*impl_ptr.cast::<UnixTransport>() };
        let Some(sock) = ut.get_conn(conn) else {
            return -1;
        };
        // SAFETY: buf is valid for buf_size bytes per caller contract.
        let slice = unsafe { std::slice::from_raw_parts_mut(buf.cast::<u8>(), buf_size) };
        framed_recv(&sock, slice)
    }

    unsafe extern "C" fn unix_close_conn(impl_ptr: *mut c_void, conn: c_int) {
        if impl_ptr.is_null() {
            return;
        }
        // SAFETY: impl_ptr points to a valid UnixTransport.
        let ut = unsafe { &*impl_ptr.cast::<UnixTransport>() };
        ut.remove_conn(conn);
    }

    unsafe extern "C" fn unix_destroy(impl_ptr: *mut c_void) {
        if impl_ptr.is_null() {
            return;
        }
        // SAFETY: impl_ptr was created by Box::into_raw in hew_transport_unix_new.
        let _ = unsafe { Box::from_raw(impl_ptr.cast::<UnixTransport>()) };
    }

    static UNIX_OPS: HewTransportOps = HewTransportOps {
        connect: Some(unix_connect),
        listen: Some(unix_listen),
        accept: Some(unix_accept),
        send: Some(unix_send),
        recv: Some(unix_recv),
        close_conn: Some(unix_close_conn),
        destroy: Some(unix_destroy),
    };

    /// Create a new Unix domain socket transport.
    ///
    /// # Safety
    ///
    /// The returned pointer must be freed by calling the transport's `destroy` op.
    #[no_mangle]
    pub unsafe extern "C" fn hew_transport_unix_new() -> *mut HewTransport {
        let ut = Box::new(UnixTransport::new());
        let transport = Box::new(HewTransport {
            ops: &raw const UNIX_OPS,
            r#impl: Box::into_raw(ut).cast::<c_void>(),
        });
        Box::into_raw(transport)
    }
}

#[cfg(not(unix))]
mod unix_transport {
    use super::*;

    /// Stub for non-Unix platforms.
    #[no_mangle]
    pub unsafe extern "C" fn hew_transport_unix_new() -> *mut HewTransport {
        std::ptr::null_mut()
    }
}
