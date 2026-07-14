//! Network transport abstraction with TCP and Unix socket implementations.
//!
//! Provides a vtable-based transport interface (`HewTransportOps`) and two
//! concrete implementations (TCP, Unix). Also defines `HewActorRef` for
//! network-transparent actor references.
//!
//! The TCP and Unix transports use length-prefixed (4-byte little-endian)
//! framing for send/recv. Connections are stored in a fixed-size array.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::collections::HashMap;
use std::ffi::{c_char, c_int, c_void, CStr};
use std::io::{ErrorKind, Read, Write};
use std::net::{Shutdown, SocketAddr, TcpListener, TcpStream, ToSocketAddrs};
use std::sync::{
    atomic::{AtomicBool, AtomicU64, Ordering},
    LazyLock, OnceLock,
};

use crate::blocking_pool::{shared_blocking_pool, spawn_blocking_result, BlockingPoolError};
use crate::lifetime::poison_safe::{PoisonSafe, PoisonSafeRw};

use socket2::{Domain, Protocol, SockAddr, Socket, Type};

use crate::actor::{self, HewActor};
use crate::envelope::encode_envelope_frame_from_raw_parts;
use crate::internal::types::HewActorState;
use crate::set_last_error;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Invalid connection sentinel.
pub const HEW_CONN_INVALID: c_int = -1;

/// Maximum number of connections stored per transport.
const MAX_CONNS: usize = 64;
/// Maximum accepted framed payload size (16 MiB).
///
/// `pub(crate)` so `sim_transport` can pin its mirrored copy to this value via
/// a `const _: () = assert!(...)` (the NW-1 native↔WASM parity guard) instead
/// of holding the two in sync by comment.
pub(crate) const MAX_FRAME_SIZE: usize = 16 * 1024 * 1024;

// Error codes matching the C header.
const HEW_OK: c_int = 0;
const HEW_ERR_ACTOR_STOPPED: c_int = -2;
const HEW_ERR_SERIALIZE: c_int = -12;
/// Generic transport error returned across the `c_int` channel.
///
/// `pub(crate)` so `sim_transport`'s mirrored copy can be pinned to it (NW-1).
pub(crate) const HEW_ERR_TRANSPORT: c_int = -14;

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
///
/// `incarnation` is the actor-slot / registration incarnation captured for this
/// reference — the explicit identity carrier of the spec's `(NodeId, slot,
/// incarnation)` tuple. It is a SEPARATE dimension from the packed `actor_id`
/// (whose 64 bits are fully consumed by `(node_id, serial)`), never bit-stolen
/// from the serial. `0` means "no incarnation tracked" (the fresh-registration
/// default). It is distinct from the node-level SWIM peer incarnation.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewActorRefRemote {
    pub actor_id: u64,
    pub conn: c_int,
    pub transport: *mut HewTransport,
    pub incarnation: u32,
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
/// `incarnation` is the captured actor-slot / registration incarnation (the
/// spec's `(NodeId, slot, incarnation)` carrier); pass `0` when none is
/// tracked.
///
/// # Safety
///
/// `transport` must be a valid pointer to a live [`HewTransport`].
#[no_mangle]
pub unsafe extern "C" fn hew_actor_ref_remote(
    actor_id: u64,
    conn: c_int,
    transport: *mut HewTransport,
    incarnation: u32,
) -> HewActorRef {
    HewActorRef {
        kind: ACTOR_REF_REMOTE,
        data: HewActorRefData {
            remote: HewActorRefRemote {
                actor_id,
                conn,
                transport,
                incarnation,
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
    // SAFETY: caller guarantees `payload` is valid for `payload_len` bytes.
    let bytes = match unsafe {
        encode_envelope_frame_from_raw_parts(
            target_actor_id,
            source_actor_id,
            msg_type,
            payload.cast_const(),
            payload_len,
            0,
            0,
        )
    } {
        Ok(bytes) => bytes,
        Err(err) => {
            set_last_error(format!("wire_send_envelope: {err}"));
            return HEW_ERR_SERIALIZE;
        }
    };
    // SAFETY: transport is valid per caller contract.
    let t = unsafe { &*transport };
    // SAFETY: t.ops was set during transport creation and remains valid for
    // the transport's lifetime; caller guarantees transport is not freed.
    let result = if let Some(ops) = unsafe { t.ops.as_ref() } {
        if let Some(send_fn) = ops.send {
            // SAFETY: bytes was successfully encoded; data/len are valid.
            unsafe { send_fn(t.r#impl, conn, bytes.as_ptr().cast::<c_void>(), bytes.len()) }
        } else {
            -1
        }
    } else {
        -1
    };
    if result > 0 {
        HEW_OK
    } else {
        HEW_ERR_TRANSPORT
    }
}

/// Send a message through an actor reference.
///
/// LOCAL path: direct call to `hew_actor_send`.
/// REMOTE path: encode as CBOR envelope, send over transport.
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
    cabi_guard!(ref_ptr.is_null(), 0);
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

/// Return the local `*mut HewActor` pointer (as `*mut c_void`) for a LOCAL
/// actor reference, or null for a REMOTE reference. Used by the active-mode
/// reactor, which only supports local actors (mirrors the websocket attach
/// reader's `actor_ref_local_actor`).
pub(crate) fn actor_ref_local_ptr(actor_ref: &HewActorRef) -> *mut c_void {
    if actor_ref.kind != ACTOR_REF_LOCAL {
        return std::ptr::null_mut();
    }
    // SAFETY: the local union variant is active when kind == ACTOR_REF_LOCAL.
    unsafe { actor_ref.data.local }.cast::<c_void>()
}

// ===========================================================================
// TCP transport
// ===========================================================================

// WASM-TODO(#1451): TcpCounters always returns zeros on WASM; no TCP transport
#[derive(Debug, Default)]
struct TcpCounters {
    bytes_read: AtomicU64,
    bytes_written: AtomicU64,
    accept_count: AtomicU64,
    connect_count: AtomicU64,
    /// Counts syscall-level TCP failures from `hew_tcp_read`, `hew_tcp_write`,
    /// `hew_tcp_accept`, and `hew_tcp_connect`. `WouldBlock`, `Interrupted`,
    /// and `TimedOut` are expected non-failure outcomes (the latter for
    /// user-configured socket read/write timeouts, which surface as
    /// `ErrorKind::TimedOut` on Windows) and do not increment this counter.
    error_count: AtomicU64,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct TcpCountersSnapshot {
    pub bytes_read: u64,
    pub bytes_written: u64,
    pub accept_count: u64,
    pub connect_count: u64,
    pub error_count: u64,
}

fn tcp_counters() -> &'static TcpCounters {
    static TCP_COUNTERS: OnceLock<TcpCounters> = OnceLock::new();
    TCP_COUNTERS.get_or_init(TcpCounters::default)
}

#[must_use]
pub fn tcp_counters_snapshot() -> TcpCountersSnapshot {
    let counters = tcp_counters();
    TcpCountersSnapshot {
        bytes_read: counters.bytes_read.load(Ordering::Relaxed),
        bytes_written: counters.bytes_written.load(Ordering::Relaxed),
        accept_count: counters.accept_count.load(Ordering::Relaxed),
        connect_count: counters.connect_count.load(Ordering::Relaxed),
        error_count: counters.error_count.load(Ordering::Relaxed),
    }
}

pub(crate) fn record_tcp_error_kind(kind: ErrorKind) {
    if !matches!(
        kind,
        ErrorKind::WouldBlock | ErrorKind::Interrupted | ErrorKind::TimedOut
    ) {
        tcp_counters().error_count.fetch_add(1, Ordering::Relaxed);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConnLookupError {
    Poisoned { transport: &'static str },
}

fn report_conn_table_poison(
    transport: &'static str,
    reported: &AtomicBool,
    action: &'static str,
    conn: Option<c_int>,
) {
    let message = match conn {
        Some(conn) => {
            format!("{transport} transport connection table poisoned while {action} conn {conn}")
        }
        None => format!("{transport} transport connection table poisoned while {action}"),
    };
    if !reported.swap(true, Ordering::Relaxed) {
        tracing::error!(transport, action, conn_id = ?conn, "{message}");
    }
    set_last_error(message);
}

/// Internal state for the TCP transport.
struct TcpTransport {
    listen_sock: Option<Socket>,
    conns: PoisonSafeRw<Vec<Option<Socket>>>,
    conn_poison_reported: AtomicBool,
}

impl TcpTransport {
    fn new() -> Self {
        Self {
            listen_sock: None,
            conns: PoisonSafeRw::new((0..MAX_CONNS).map(|_| None).collect()),
            conn_poison_reported: AtomicBool::new(false),
        }
    }

    fn store_conn(&self, sock: Socket) -> c_int {
        // macOS parity: mark the socket SO_NOSIGPIPE so a broken-pipe write
        // fails closed with EPIPE instead of raising SIGPIPE (no-op elsewhere;
        // Linux uses MSG_NOSIGNAL in framed_send).
        suppress_sigpipe(&sock);
        if let Ok(conn) = self.conns.write(|conns| {
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
        }) {
            conn
        } else {
            report_conn_table_poison("tcp", &self.conn_poison_reported, "storing conn", None);
            HEW_CONN_INVALID
        }
    }

    fn get_conn(&self, id: c_int) -> Result<Option<Socket>, ConnLookupError> {
        if id < 0 {
            return Ok(None);
        }
        #[expect(clippy::cast_sign_loss, reason = "guarded by id >= 0")]
        let idx = id as usize;
        self.conns
            .read(|conns| {
                conns
                    .get(idx)
                    .and_then(Option::as_ref)
                    .and_then(|sock| sock.try_clone().ok())
            })
            .map_err(|_| {
                report_conn_table_poison("tcp", &self.conn_poison_reported, "looking up", Some(id));
                ConnLookupError::Poisoned { transport: "tcp" }
            })
    }

    fn remove_conn(&self, id: c_int) {
        if id >= 0 {
            #[expect(clippy::cast_sign_loss, reason = "guarded by id >= 0")]
            let idx = id as usize;
            if idx < MAX_CONNS
                && self
                    .conns
                    .write(|conns| {
                        // Shutdown the socket before dropping it so that any
                        // cloned file descriptors (held by reader threads) are
                        // woken from blocking recv calls. Without this, dropping
                        // only decrements the refcount and the clone keeps the
                        // socket alive, causing join() on the reader thread to
                        // deadlock.
                        if let Some(ref sock) = conns[idx] {
                            let _ = sock.shutdown(Shutdown::Both);
                        }
                        conns[idx] = None;
                    })
                    .is_err()
            {
                report_conn_table_poison(
                    "tcp",
                    &self.conn_poison_reported,
                    "removing conn",
                    Some(id),
                );
            }
        }
    }
}

/// Parse a "host:port" address string.
fn parse_host_port(addr: &str) -> Option<SocketAddr> {
    addr.parse::<SocketAddr>().ok()
}

/// Extra `send(2)` flags for [`framed_send`].
///
/// On Linux/Android, `MSG_NOSIGNAL` suppresses `SIGPIPE` for THIS send even if
/// the process-wide disposition were ever reset — a belt-and-suspenders partner
/// to the `hew_sched_init` process-wide SIGPIPE ignore, so a broken-pipe write
/// returns `EPIPE` (surfacing as a typed fail-closed error) instead of killing
/// the process. macOS/iOS have no `MSG_NOSIGNAL`; they get the equivalent via
/// the `SO_NOSIGPIPE` socket option set in [`suppress_sigpipe`]. Windows has no
/// `SIGPIPE`.
#[cfg(any(target_os = "linux", target_os = "android"))]
const FRAMED_SEND_FLAGS: c_int = libc::MSG_NOSIGNAL;
#[cfg(not(any(target_os = "linux", target_os = "android")))]
const FRAMED_SEND_FLAGS: c_int = 0;

/// Suppress `SIGPIPE` for writes on a stored connection socket (macOS parity).
///
/// macOS/iOS/tvOS/watchOS have no `MSG_NOSIGNAL` send flag, so the Linux
/// [`FRAMED_SEND_FLAGS`] path has no analogue there; instead the `SO_NOSIGPIPE`
/// socket option is set once, at connection-store time, so a broken-pipe write
/// on this socket returns `EPIPE` rather than raising `SIGPIPE`. This is
/// defense-in-depth behind the process-wide SIGPIPE ignore installed in
/// `hew_sched_init`. No-op on platforms without `SO_NOSIGPIPE`.
#[cfg(any(
    target_os = "macos",
    target_os = "ios",
    target_os = "tvos",
    target_os = "watchos"
))]
fn suppress_sigpipe(sock: &Socket) {
    use std::os::fd::AsRawFd;
    let enable: c_int = 1;
    // SAFETY: `sock` owns a valid file descriptor for the duration of this call.
    // `SO_NOSIGPIPE` at `SOL_SOCKET` takes an `int`-sized option value; a failure
    // is non-fatal (the process-wide ignore still applies), so the result is
    // intentionally not surfaced.
    unsafe {
        libc::setsockopt(
            sock.as_raw_fd(),
            libc::SOL_SOCKET,
            libc::SO_NOSIGPIPE,
            (&raw const enable).cast(),
            #[expect(
                clippy::cast_possible_truncation,
                reason = "sizeof(c_int) is 4, fits in socklen_t (u32)"
            )]
            {
                std::mem::size_of::<c_int>() as libc::socklen_t
            },
        );
    }
}

/// No-op on platforms without `SO_NOSIGPIPE` (Linux/Android use
/// [`FRAMED_SEND_FLAGS`] `MSG_NOSIGNAL`; Windows has no `SIGPIPE`).
#[cfg(not(any(
    target_os = "macos",
    target_os = "ios",
    target_os = "tvos",
    target_os = "watchos"
)))]
fn suppress_sigpipe(_sock: &Socket) {}

/// Send exactly `buf` bytes over a socket with length-prefixed framing.
fn framed_send(sock: &Socket, data: &[u8]) -> c_int {
    if data.len() > u32::MAX as usize {
        return -1;
    }
    #[expect(clippy::cast_possible_truncation, reason = "payload bounded by caller")]
    let frame_len = data.len() as u32;
    let header = frame_len.to_le_bytes();

    // Write header. `send_with_flags` (not `write`) so `MSG_NOSIGNAL` can
    // suppress SIGPIPE on Linux; on macOS the socket carries `SO_NOSIGPIPE`
    // (see `suppress_sigpipe`). Either way a broken pipe returns `EPIPE` here
    // instead of killing the process, and we fail closed with `-1`.
    let mut written = 0usize;
    while written < 4 {
        match sock.send_with_flags(&header[written..], FRAMED_SEND_FLAGS) {
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
        match sock.send_with_flags(&data[written..], FRAMED_SEND_FLAGS) {
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
    cabi_guard!(impl_ptr.is_null() || address.is_null(), HEW_CONN_INVALID);
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
    cabi_guard!(impl_ptr.is_null() || address.is_null(), -1);
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
    cabi_guard!(impl_ptr.is_null(), HEW_CONN_INVALID);
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
    cabi_guard!(impl_ptr.is_null() || data.is_null(), -1);
    // SAFETY: impl_ptr points to a valid TcpTransport.
    let tcp = unsafe { &*impl_ptr.cast::<TcpTransport>() };
    let sock = match tcp.get_conn(conn) {
        Ok(Some(sock)) => sock,
        Ok(None) | Err(ConnLookupError::Poisoned { .. }) => return -1,
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
    cabi_guard!(impl_ptr.is_null() || buf.is_null(), -1);
    // SAFETY: impl_ptr points to a valid TcpTransport.
    let tcp = unsafe { &*impl_ptr.cast::<TcpTransport>() };
    let sock = match tcp.get_conn(conn) {
        Ok(Some(sock)) => sock,
        Ok(None) | Err(ConnLookupError::Poisoned { .. }) => return -1,
    };
    // SAFETY: buf is valid for buf_size bytes per caller contract.
    let slice = unsafe { std::slice::from_raw_parts_mut(buf.cast::<u8>(), buf_size) };
    framed_recv(&sock, slice)
}

unsafe extern "C" fn tcp_close_conn(impl_ptr: *mut c_void, conn: c_int) {
    cabi_guard!(impl_ptr.is_null());
    // SAFETY: impl_ptr points to a valid TcpTransport.
    let tcp = unsafe { &*impl_ptr.cast::<TcpTransport>() };
    tcp.remove_conn(conn);
}

unsafe extern "C" fn tcp_destroy(impl_ptr: *mut c_void) {
    cabi_guard!(impl_ptr.is_null());
    // SAFETY: impl_ptr was created by Box::into_raw in hew_transport_tcp_new.
    let _ = unsafe { Box::from_raw(impl_ptr.cast::<TcpTransport>()) }; // ALLOCATOR-PAIRING: GlobalAlloc
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

#[cfg(test)]
pub(crate) unsafe fn hew_transport_tcp_bound_port(transport: *mut HewTransport) -> Option<u16> {
    if transport.is_null() {
        return None;
    }
    // SAFETY: caller guarantees `transport` is valid for the duration of this helper.
    let transport = unsafe { &*transport };
    if !std::ptr::eq(transport.ops, &raw const TCP_OPS) || transport.r#impl.is_null() {
        return None;
    }
    // SAFETY: the TCP ops check above guarantees the impl pointer is a TcpTransport.
    let tcp = unsafe { &*transport.r#impl.cast::<TcpTransport>() };
    tcp.listen_sock
        .as_ref()?
        .local_addr()
        .ok()?
        .as_socket()
        .map(|addr| addr.port())
}

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
        r#impl: Box::into_raw(tcp).cast::<c_void>(), // ALLOCATOR-PAIRING: GlobalAlloc
    });
    Box::into_raw(transport) // ALLOCATOR-PAIRING: GlobalAlloc
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

static TCP_API_STATE: LazyLock<PoisonSafe<TcpApiState>> =
    LazyLock::new(|| PoisonSafe::new(TcpApiState::new()));

fn tcp_clone_listener(handle: c_int) -> Option<TcpListener> {
    TCP_API_STATE.access(|state| state.listeners.get(&handle)?.try_clone().ok())
}

/// Return the bound local port of a stdlib TCP listener handle, or `None` if
/// the handle is unknown. Useful when binding to port 0 (ephemeral) and
/// needing the OS-assigned port; consumed by active-mode e2e tests.
#[must_use]
pub fn tcp_listener_local_port(handle: c_int) -> Option<u16> {
    TCP_API_STATE.access(|state| {
        state
            .listeners
            .get(&handle)?
            .local_addr()
            .ok()
            .map(|a| a.port())
    })
}

#[no_mangle]
pub extern "C" fn hew_tcp_listener_local_port(listener: c_int) -> c_int {
    tcp_listener_local_port(listener).map_or(-1, c_int::from)
}

/// Outcome of a stream-clone attempt for the TCP bridge.
///
/// Distinguishes "no such connection handle" (a genuine EBADF) from "the
/// handle is live but `try_clone`/`dup(2)` failed" (e.g. EMFILE/ENFILE under fd
/// pressure), so `hew_tcp_stream_from_conn` can report the real errno instead
/// of mislabeling a resource-exhaustion failure as an invalid handle.
pub(crate) enum CloneOutcome {
    /// The handle was live and the clone succeeded.
    Cloned(TcpStream),
    /// No connection with this handle is registered (genuine EBADF).
    NoEntry,
    /// The handle was live but the clone/dup failed with this OS error.
    Failed(std::io::Error),
}

pub(crate) fn tcp_clone_stream(handle: c_int) -> Option<TcpStream> {
    match tcp_clone_stream_outcome(handle) {
        CloneOutcome::Cloned(stream) => Some(stream),
        CloneOutcome::NoEntry | CloneOutcome::Failed(_) => None,
    }
}

/// Clone a connection's socket, distinguishing "no such handle" from "handle
/// live but clone failed" so the stream bridge can report the real errno. The
/// `Option`-returning [`tcp_clone_stream`] delegates here for callers that only
/// need success/failure.
pub(crate) fn tcp_clone_stream_outcome(handle: c_int) -> CloneOutcome {
    TCP_API_STATE.access(|state| {
        let Some(stream) = state.streams.get(&handle) else {
            return CloneOutcome::NoEntry;
        };
        // Deterministic test-only injection: with a live table entry confirmed,
        // fail this clone if the current thread armed it (the valid-handle-
        // clone-failed path). Production builds never compile this branch.
        #[cfg(any(test, feature = "clone-failure-test"))]
        if clone_failure_hook::take_should_fail() {
            return CloneOutcome::Failed(std::io::Error::from_raw_os_error(
                clone_failure_hook::INJECTED_CLONE_ERRNO,
            ));
        }
        match stream.try_clone() {
            Ok(clone) => CloneOutcome::Cloned(clone),
            Err(err) => CloneOutcome::Failed(err),
        }
    })
}

/// Deterministic clone-failure injection seam for the consumed-connection
/// ownership tests (#2650). See the `clone-failure-test` feature.
#[cfg(any(test, feature = "clone-failure-test"))]
pub(crate) mod clone_failure_hook {
    use std::cell::RefCell;
    use std::collections::VecDeque;

    /// OS errno the injected clone failure reports. `EMFILE` ("too many open
    /// files") is the realistic `dup(2)` failure this lane exists to surface,
    /// and is deliberately distinct from `EBADF` (9, the genuine-invalid-handle
    /// code) so the diagnostic test can prove the two are no longer conflated.
    pub const INJECTED_CLONE_ERRNO: i32 = libc::EMFILE;

    thread_local! {
        /// Pending forced-failure decisions for upcoming `tcp_clone_stream`
        /// calls on this thread, in call order. `true` = fail the clone.
        static FORCE: RefCell<VecDeque<bool>> = const { RefCell::new(VecDeque::new()) };
    }

    /// Test-only: arm deterministic failure of the next one/two
    /// `tcp_clone_stream` calls on the CURRENT thread. `first` decides clone #1
    /// (the read fd), `second` decides clone #2 (the write fd). Replaces any
    /// previously-armed decisions. A clone that is never attempted — because an
    /// earlier one failed and `hew_tcp_stream_from_conn` returned early — simply
    /// leaves its arm unconsumed until the next `force_next_clone_failures`
    /// clears it, so re-arming per loop iteration is safe.
    pub fn force_next_clone_failures(first: bool, second: bool) {
        FORCE.with(|f| {
            let mut q = f.borrow_mut();
            q.clear();
            q.push_back(first);
            q.push_back(second);
        });
    }

    /// Clear any armed decisions on the current thread.
    pub fn clear_clone_failures() {
        FORCE.with(|f| f.borrow_mut().clear());
    }

    /// Consume the next armed decision (unarmed ⇒ `false`/succeed). Called by
    /// `tcp_clone_stream` only after a live table entry is confirmed.
    pub(crate) fn take_should_fail() -> bool {
        FORCE.with(|f| f.borrow_mut().pop_front().unwrap_or(false))
    }
}

#[cfg(any(test, feature = "clone-failure-test"))]
pub use clone_failure_hook::{
    clear_clone_failures, force_next_clone_failures, INJECTED_CLONE_ERRNO,
};

/// Remove a TCP connection handle from the table WITHOUT calling `shutdown`.
///
/// Used by `hew_tcp_stream_from_conn` after cloning the socket for the read
/// and write backings.  The two clones keep the underlying OS socket alive;
/// calling `shutdown` here would invalidate those clones because `TcpStream`
/// clones share a single file descriptor on Unix.
///
/// This is intentionally different from `hew_tcp_close`, which does call
/// `shutdown(Both)` because it fully releases the connection.
pub(crate) fn tcp_release_conn(handle: c_int) {
    TCP_API_STATE.access(|state| {
        state.streams.remove(&handle);
        // Drop the removed TcpStream here. No shutdown call.
    });
}

/// Close a freshly-accepted connection handle that has no Hew-side owner
/// (NEW-2 accept/abandon race). When `handle_ready_accept` accepts a connection
/// but the suspended handler was abandoned/cancelled before the deposit lands,
/// no resume edge will ever bind — and thus own and close — the accepted handle.
/// This removes it from `TCP_API_STATE.streams` and `shutdown(Both)`s the socket
/// so the fd is released exactly once.
///
/// Unlike [`tcp_release_conn`] (which keeps the socket alive for clones) this
/// fully releases the connection, mirroring `hew_tcp_close`'s stream branch. The
/// accepted fd was never registered with the reactor (the reactor polls the
/// listener, not this new conn), so no detach is required.
pub(crate) fn tcp_close_orphan_conn(handle: c_int) {
    TCP_API_STATE.access(|state| {
        if let Some(stream) = state.streams.remove(&handle) {
            let _ = stream.shutdown(Shutdown::Both);
        }
    });
}

/// Test-only: create a connected loopback TCP socketpair, register the server
/// end as a conn handle, and return `(conn_handle, client_stream)`. Lets the
/// reactor's resume-mode read branch be driven against a REAL readable socket
/// (writing to `client_stream` makes `conn_handle` readable). The caller closes
/// `conn_handle` with [`tcp_close_raw_for_test`] and drops `client_stream`.
#[cfg(test)]
pub(crate) fn tcp_socketpair_conn_for_test() -> (c_int, TcpStream) {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let addr = listener.local_addr().expect("listener addr");
    let client = TcpStream::connect(addr).expect("connect loopback client");
    let (server, _) = listener.accept().expect("accept loopback server");
    server.set_nodelay(true).ok();
    let handle = TCP_API_STATE.access(|state| {
        let handle = state.alloc_handle();
        state.streams.insert(handle, server);
        handle
    });
    (handle, client)
}

/// Test-only: remove a conn handle's stream from the table (no shutdown), the
/// counterpart to [`tcp_socketpair_conn_for_test`].
#[cfg(test)]
pub(crate) fn tcp_close_raw_for_test(handle: c_int) {
    tcp_release_conn(handle);
}

/// Test-only: bind a loopback listener, register it as a listener handle, set it
/// non-blocking, and connect a client so exactly one connection is pending in the
/// kernel accept queue. Returns `(listener_handle, client_stream)`; keeping the
/// client alive holds the pending connection so a subsequent
/// [`tcp_listener_accept_nonblocking`] yields `Accepted`. Backs the NEW-2
/// accept/abandon-race regression (the reactor accepts a real connection, then
/// the deposit fails on a cancelled slot). The caller closes the listener with
/// [`tcp_close_raw_for_test`] and drops `client_stream`.
#[cfg(test)]
pub(crate) fn tcp_listener_with_pending_conn_for_test() -> (c_int, TcpStream) {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let addr = listener.local_addr().expect("listener addr");
    let handle = TCP_API_STATE.access(|state| {
        let handle = state.alloc_handle();
        state.listeners.insert(handle, listener);
        handle
    });
    assert!(tcp_listener_set_nonblocking(handle, true));
    let client = TcpStream::connect(addr).expect("connect loopback client");
    // Give the loopback handshake a moment so the connection is queued and a
    // subsequent non-blocking accept yields `Accepted` deterministically.
    std::thread::sleep(std::time::Duration::from_millis(20));
    (handle, client)
}

/// Test-only: check whether a specific connection handle is currently in the
/// live streams table. Checks a specific token rather than the global count,
/// so concurrent transport tests adding/removing their own handles do not cause
/// false positives or false negatives.
#[cfg(any(test, feature = "clone-failure-test"))]
pub fn tcp_streams_has_handle_for_test(handle: c_int) -> bool {
    TCP_API_STATE.access(|state| state.streams.contains_key(&handle))
}

// ---- Active-mode reactor support -------------------------------------------
//
// These helpers back the non-blocking "I/O completion as a mailbox message"
// reactor (`crate::reactor`). The reactor registers a connection's raw fd for
// readiness; on readiness it reads available bytes and delivers them to the
// owning actor's mailbox. All three operate by the user-facing `Connection`
// handle (the `c_int` in `TCP_API_STATE`), so they share the same socket
// table — clone/close coordination is identical to the blocking read path.

/// Return the raw OS file descriptor for a TCP connection handle, or `None`
/// if the handle is unknown. Used by the reactor to register the fd with the
/// platform poller. The fd remains owned by the `TcpStream` in
/// `TCP_API_STATE`; the reactor must NOT close it directly (it closes via
/// `hew_tcp_close` / handle removal).
#[cfg(unix)]
pub(crate) fn tcp_conn_raw_fd(handle: c_int) -> Option<c_int> {
    use std::os::fd::AsRawFd;
    TCP_API_STATE.access(|state| state.streams.get(&handle).map(AsRawFd::as_raw_fd))
}

/// Windows token form of [`tcp_conn_raw_fd`]. A Windows `SOCKET` is pointer-width
/// and does not fit the poller's `c_int fd` ABI, so the reactor uses the
/// user-facing connection handle itself as the poller token (D-2a); the IOCP
/// poller resolves that token back to the `SOCKET` via [`tcp_handle_raw_socket`].
/// Returns the handle unchanged when it names a live stream, mirroring the unix
/// "fd is known" semantics.
#[cfg(windows)]
pub(crate) fn tcp_conn_raw_fd(handle: c_int) -> Option<c_int> {
    TCP_API_STATE.access(|state| state.streams.contains_key(&handle).then_some(handle))
}

/// Resolve a reactor poller token (a `c_int` connection or listener handle) to
/// the OS `SOCKET` it names, checking the stream table first and then the
/// listener table. The Windows IOCP poller owns the token↔`SOCKET` indirection
/// (D-2a); the engine never sees a raw `SOCKET`. Returns `None` if the handle is
/// unknown (already closed) — the poller treats that as a benign stale token.
#[cfg(windows)]
pub(crate) fn tcp_handle_raw_socket(handle: c_int) -> Option<std::os::windows::io::RawSocket> {
    use std::os::windows::io::AsRawSocket;
    TCP_API_STATE.access(|state| {
        state
            .streams
            .get(&handle)
            .map(AsRawSocket::as_raw_socket)
            .or_else(|| state.listeners.get(&handle).map(AsRawSocket::as_raw_socket))
    })
}

/// Return the raw OS file descriptor for a TCP *listener* handle, or `None` if
/// the handle is unknown (NEW-2 `await listener.accept()`). The fd-readiness
/// sibling of [`tcp_conn_raw_fd`]: the reactor registers the listener fd for
/// readability and `accept()`s when it fires. The fd remains owned by the
/// `TcpListener` in `TCP_API_STATE`; the reactor must NOT close it directly.
#[cfg(unix)]
pub(crate) fn tcp_listener_raw_fd(handle: c_int) -> Option<c_int> {
    use std::os::fd::AsRawFd;
    TCP_API_STATE.access(|state| state.listeners.get(&handle).map(AsRawFd::as_raw_fd))
}

/// Windows token form of [`tcp_listener_raw_fd`] — returns the listener handle
/// itself as the poller token (D-2a; see [`tcp_conn_raw_fd`]). The IOCP poller
/// resolves it to the `SOCKET` via [`tcp_handle_raw_socket`].
#[cfg(windows)]
pub(crate) fn tcp_listener_raw_fd(handle: c_int) -> Option<c_int> {
    TCP_API_STATE.access(|state| state.listeners.contains_key(&handle).then_some(handle))
}

/// Put a TCP *listener* handle's socket into non-blocking mode (the reactor
/// thread must never park in `accept()`). Returns `true` on success. The
/// readiness-suspension sibling of [`tcp_conn_set_nonblocking`].
pub(crate) fn tcp_listener_set_nonblocking(handle: c_int, nonblocking: bool) -> bool {
    TCP_API_STATE.access(|state| {
        state
            .listeners
            .get(&handle)
            .is_some_and(|listener| listener.set_nonblocking(nonblocking).is_ok())
    })
}

/// Outcome of a single non-blocking `accept()` on a registered listener handle
/// (NEW-2 reactor accept-readiness). The accept-path analogue of
/// [`ActiveReadOutcome`].
pub(crate) enum AcceptOutcome {
    /// A connection was accepted and registered as a new conn handle.
    Accepted(c_int),
    /// The listener was spuriously reported readable; nothing to accept yet.
    WouldBlock,
    /// The handle is unknown or a hard accept error occurred.
    Closed,
}

/// Non-blocking `accept()` on a registered listener handle (NEW-2 reactor
/// accept-readiness). Drains exactly ONE pending connection (an `await
/// listener.accept()` accepts once; the handler re-registers on its next
/// `await`), registers it as a fresh conn handle with `set_nodelay`, and returns
/// the handle. The accept-path sibling of [`tcp_conn_read_available`].
pub(crate) fn tcp_listener_accept_nonblocking(listener: c_int) -> AcceptOutcome {
    let Some(listener) = tcp_clone_listener(listener) else {
        return AcceptOutcome::Closed;
    };
    match listener.accept() {
        Ok((stream, _)) => {
            let _ = stream.set_nodelay(true);
            tcp_counters().accept_count.fetch_add(1, Ordering::Relaxed);
            let handle = TCP_API_STATE.access(|state| {
                let handle = state.alloc_handle();
                state.streams.insert(handle, stream);
                handle
            });
            AcceptOutcome::Accepted(handle)
        }
        Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => AcceptOutcome::WouldBlock,
        Err(e) => {
            record_tcp_error_kind(e.kind());
            AcceptOutcome::Closed
        }
    }
}

/// Put a TCP connection handle's socket into non-blocking mode (active mode
/// reads must never park the reactor thread). Returns `true` on success.
pub(crate) fn tcp_conn_set_nonblocking(handle: c_int, nonblocking: bool) -> bool {
    TCP_API_STATE.access(|state| {
        state
            .streams
            .get(&handle)
            .is_some_and(|stream| stream.set_nonblocking(nonblocking).is_ok())
    })
}

/// Outcome of a single non-blocking active-mode read.
pub(crate) enum ActiveReadOutcome {
    /// Bytes were read (non-empty).
    Data(Vec<u8>),
    /// The socket is readable-empty for now (`WouldBlock`); nothing to deliver.
    WouldBlock,
    /// Peer closed the connection (read returned 0) — deliver `on_close`.
    Eof,
    /// The handle is unknown or a hard read error occurred — deliver `on_close`.
    Closed,
}

/// Read all currently-available bytes from a non-blocking TCP connection
/// handle, draining the socket until it would block (so a single readiness
/// notification does not strand buffered data when the poller is edge
/// triggered). Returns the concatenated bytes, or a non-`Data` outcome
/// describing EOF / would-block / error.
pub(crate) fn tcp_conn_read_available(handle: c_int) -> ActiveReadOutcome {
    let Some(mut stream) = tcp_clone_stream(handle) else {
        return ActiveReadOutcome::Closed;
    };
    let mut out: Vec<u8> = Vec::new();
    let mut buf = [0u8; 8192];
    loop {
        match stream.read(&mut buf) {
            Ok(0) => {
                tcp_counters()
                    .bytes_read
                    .fetch_add(out.len() as u64, Ordering::Relaxed);
                // Peer closed. If we already drained some bytes, deliver them
                // first; the caller re-polls and observes EOF next time. With
                // an empty buffer this is a clean EOF.
                return if out.is_empty() {
                    ActiveReadOutcome::Eof
                } else {
                    ActiveReadOutcome::Data(out)
                };
            }
            Ok(n) => {
                out.extend_from_slice(&buf[..n]);
                // Keep draining; a partial fill (< buf len) means the kernel
                // buffer is empty and the next read would block, so stop to
                // avoid an extra syscall.
                if n < buf.len() {
                    tcp_counters()
                        .bytes_read
                        .fetch_add(out.len() as u64, Ordering::Relaxed);
                    return ActiveReadOutcome::Data(out);
                }
            }
            Err(ref e) if e.kind() == ErrorKind::WouldBlock => {
                tcp_counters()
                    .bytes_read
                    .fetch_add(out.len() as u64, Ordering::Relaxed);
                return if out.is_empty() {
                    ActiveReadOutcome::WouldBlock
                } else {
                    ActiveReadOutcome::Data(out)
                };
            }
            Err(ref e) if e.kind() == ErrorKind::Interrupted => {
                // Retry the read; EINTR is not a failure.
            }
            Err(e) => {
                record_tcp_error_kind(e.kind());
                return if out.is_empty() {
                    ActiveReadOutcome::Closed
                } else {
                    ActiveReadOutcome::Data(out)
                };
            }
        }
    }
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
    cabi_guard!(addr.is_null(), -1);
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
    let listener = match TcpListener::bind(bind_addr) {
        Ok(l) => l,
        Err(e) => {
            hew_cabi::sink::set_last_error_with_errno(
                format!("hew_tcp_listen: {e}"),
                e.raw_os_error().unwrap_or(0),
            );
            return -1;
        }
    };
    TCP_API_STATE.access(|state| {
        let handle = state.alloc_handle();
        state.listeners.insert(handle, listener);
        handle
    })
}

/// Accept one incoming TCP connection from a listener handle.
///
/// Returns a positive connection handle, or -1 on error.
#[no_mangle]
pub extern "C" fn hew_tcp_accept(listener: c_int) -> c_int {
    let Some(listener) = tcp_clone_listener(listener) else {
        return -1;
    };
    let (stream, _) = match listener.accept() {
        Ok(accepted) => accepted,
        Err(e) => {
            record_tcp_error_kind(e.kind());
            return -1;
        }
    };
    let _ = stream.set_nodelay(true);
    tcp_counters().accept_count.fetch_add(1, Ordering::Relaxed);
    TCP_API_STATE.access(|state| {
        let handle = state.alloc_handle();
        state.streams.insert(handle, stream);
        handle
    })
}

/// Resolve `addr` through the shared blocking pool with an optional deadline.
///
/// `deadline_ms <= 0` resolves with no deadline. The shared pool keeps the
/// scheduler thread free while `getaddrinfo` runs.
///
/// On `IoError::TimedOut` the caller should set errno=ETIMEDOUT (110 on
/// Linux, 60 on macOS) — caller's responsibility because errno values are
/// platform-specific. The pool returns `BlockingPoolError::PoolStopped` for
/// any non-timeout failure including a `getaddrinfo` error.
fn resolve_addr_via_pool(
    target: String,
    deadline_ms: i64,
) -> Result<Vec<SocketAddr>, BlockingPoolError> {
    let deadline = if deadline_ms <= 0 {
        None
    } else {
        #[expect(clippy::cast_sign_loss, reason = "checked > 0 above")]
        Some(std::time::Duration::from_millis(deadline_ms as u64))
    };
    // SAFETY: shared_blocking_pool returns a process-lifetime, never-stopped
    // pool whose pointer stays valid for this call.
    unsafe {
        spawn_blocking_result(
            shared_blocking_pool(),
            move || {
                target
                    .to_socket_addrs()
                    .map(Iterator::collect::<Vec<_>>)
                    .ok()
            },
            deadline,
        )
    }
    .and_then(|opt| opt.ok_or(BlockingPoolError::PoolStopped))
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
    // SAFETY: forwarded contract; deadline_ms=0 disables the deadline.
    unsafe { hew_tcp_connect_timed(addr, 0) }
}

/// Connect to a TCP endpoint at `addr` (`host:port`) with a single-budget
/// deadline that covers BOTH the DNS resolution and the TCP handshake.
///
/// `deadline_ms <= 0` disables the deadline (no time limit). Otherwise the
/// caller has at most `deadline_ms` milliseconds across:
///   1. `getaddrinfo` running on the shared blocking pool, and
///   2. `TcpStream::connect_timeout` for the residual budget.
///
/// On deadline expiry returns -1 with errno=ETIMEDOUT (110 Linux, 60 macOS).
///
/// # Safety
///
/// `addr` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_connect_timed(addr: *const c_char, deadline_ms: i64) -> c_int {
    cabi_guard!(addr.is_null(), -1);
    // SAFETY: caller guarantees `addr` is a valid C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(addr) }.to_str() else {
        return -1;
    };
    // Allow ":port" shorthand — connect to localhost.
    let owned;
    let connect_addr: &str = if addr_str.starts_with(':') {
        owned = format!("127.0.0.1{addr_str}");
        owned.as_str()
    } else {
        addr_str
    };

    let start = std::time::Instant::now();
    let addrs = match resolve_addr_via_pool(connect_addr.to_owned(), deadline_ms) {
        Ok(a) => a,
        Err(BlockingPoolError::TimedOut) => {
            tcp_counters().error_count.fetch_add(1, Ordering::Relaxed);
            // Cleanup-all-exits: no partial socket allocated; only thread-local
            // state needs an update so callers can read structured errors.
            hew_cabi::sink::set_last_error_with_errno(
                format!("hew_tcp_connect: DNS deadline expired after {deadline_ms} ms"),
                etimedout_errno(),
            );
            return -1;
        }
        Err(BlockingPoolError::PoolStopped) => {
            // getaddrinfo returned an error.
            hew_cabi::sink::set_last_error_with_errno(
                String::from("hew_tcp_connect: DNS resolution failed"),
                0,
            );
            return -1;
        }
    };
    let Some(sock_addr) = addrs.into_iter().next() else {
        hew_cabi::sink::set_last_error_with_errno(
            String::from("hew_tcp_connect: no addresses resolved"),
            0,
        );
        return -1;
    };

    let stream = if deadline_ms <= 0 {
        match TcpStream::connect(sock_addr) {
            Ok(s) => s,
            Err(e) => {
                record_tcp_error_kind(e.kind());
                hew_cabi::sink::set_last_error_with_errno(
                    format!("hew_tcp_connect: {e}"),
                    e.raw_os_error().unwrap_or(0),
                );
                return -1;
            }
        }
    } else {
        // Single budget: residual = total - DNS time spent.
        #[expect(clippy::cast_sign_loss, reason = "deadline_ms > 0 checked above")]
        let total = std::time::Duration::from_millis(deadline_ms as u64);
        let Some(remaining) = total.checked_sub(start.elapsed()) else {
            tcp_counters().error_count.fetch_add(1, Ordering::Relaxed);
            hew_cabi::sink::set_last_error_with_errno(
                format!("hew_tcp_connect: deadline exhausted in DNS phase ({deadline_ms} ms)"),
                etimedout_errno(),
            );
            return -1;
        };
        match TcpStream::connect_timeout(&sock_addr, remaining) {
            Ok(s) => s,
            Err(e) => {
                record_tcp_error_kind(e.kind());
                hew_cabi::sink::set_last_error_with_errno(
                    format!("hew_tcp_connect: {e}"),
                    e.raw_os_error().unwrap_or(0),
                );
                return -1;
            }
        }
    };
    let _ = stream.set_nodelay(true);
    tcp_counters().connect_count.fetch_add(1, Ordering::Relaxed);
    TCP_API_STATE.access(|state| {
        let handle = state.alloc_handle();
        state.streams.insert(handle, stream);
        handle
    })
}

/// Platform-specific ETIMEDOUT errno value.
const fn etimedout_errno() -> i32 {
    #[cfg(target_os = "linux")]
    {
        110
    }
    #[cfg(target_os = "macos")]
    {
        60
    }
    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    {
        // POSIX-defined ETIMEDOUT on most BSDs is 60. Fall back to 110.
        60
    }
}

/// Set read timeout on a TCP connection handle.
///
/// `timeout_ms < 0` clears the timeout.
/// Returns 0 on success, -1 on error.
#[no_mangle]
pub extern "C" fn hew_tcp_set_read_timeout(fd: c_int, timeout_ms: c_int) -> c_int {
    let Some(stream) = tcp_clone_stream(fd) else {
        hew_cabi::sink::set_last_error_with_errno(
            "hew_tcp_set_read_timeout: invalid connection handle".into(),
            9, // EBADF: Bad file descriptor
        );
        return -1;
    };
    let timeout = if timeout_ms < 0 {
        None
    } else {
        let Ok(timeout_ms) = u64::try_from(timeout_ms) else {
            hew_cabi::sink::set_last_error_with_errno(
                "hew_tcp_set_read_timeout: invalid timeout value".into(),
                22, // EINVAL: Invalid argument
            );
            return -1;
        };
        Some(std::time::Duration::from_millis(timeout_ms))
    };
    if let Err(e) = stream.set_read_timeout(timeout) {
        hew_cabi::sink::set_last_error_with_errno(
            format!("hew_tcp_set_read_timeout: {e}"),
            e.raw_os_error().unwrap_or(0),
        );
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
        hew_cabi::sink::set_last_error_with_errno(
            "hew_tcp_set_write_timeout: invalid connection handle".into(),
            9, // EBADF: Bad file descriptor
        );
        return -1;
    };
    let timeout = if timeout_ms < 0 {
        None
    } else {
        let Ok(timeout_ms) = u64::try_from(timeout_ms) else {
            hew_cabi::sink::set_last_error_with_errno(
                "hew_tcp_set_write_timeout: invalid timeout value".into(),
                22, // EINVAL: Invalid argument
            );
            return -1;
        };
        Some(std::time::Duration::from_millis(timeout_ms))
    };
    if let Err(e) = stream.set_write_timeout(timeout) {
        hew_cabi::sink::set_last_error_with_errno(
            format!("hew_tcp_set_write_timeout: {e}"),
            e.raw_os_error().unwrap_or(0),
        );
        return -1;
    }
    0
}

/// Connect to a TCP endpoint with an explicit timeout that covers BOTH
/// DNS resolution and the TCP handshake (single-budget deadline).
///
/// Returns a positive connection handle, or -1 on error. DNS now runs on the
/// shared blocking pool so the calling scheduler thread is not parked while
/// `getaddrinfo` runs.
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
    let Ok(timeout_ms_u64) = u64::try_from(timeout_ms) else {
        return -1;
    };
    // SAFETY: caller guarantees `host` is a valid C string.
    let Ok(host_str) = unsafe { CStr::from_ptr(host) }.to_str() else {
        return -1;
    };

    let target = format!("{host_str}:{port}");
    let start = std::time::Instant::now();
    let total = std::time::Duration::from_millis(timeout_ms_u64);
    let addrs = match resolve_addr_via_pool(target, i64::from(timeout_ms)) {
        Ok(a) => a,
        Err(BlockingPoolError::TimedOut) => {
            tcp_counters().error_count.fetch_add(1, Ordering::Relaxed);
            hew_cabi::sink::set_last_error_with_errno(
                format!("hew_tcp_connect_timeout: DNS deadline expired after {timeout_ms} ms"),
                etimedout_errno(),
            );
            return -1;
        }
        Err(BlockingPoolError::PoolStopped) => {
            return -1;
        }
    };
    let Some(sock_addr) = addrs.into_iter().next() else {
        return -1;
    };
    // Cleanup-all-exits: residual budget is enforced even if DNS used most
    // of the deadline; on exhaustion we set ETIMEDOUT and bail without
    // opening a socket.
    let Some(remaining) = total.checked_sub(start.elapsed()) else {
        tcp_counters().error_count.fetch_add(1, Ordering::Relaxed);
        hew_cabi::sink::set_last_error_with_errno(
            format!("hew_tcp_connect_timeout: deadline exhausted in DNS phase ({timeout_ms} ms)"),
            etimedout_errno(),
        );
        return -1;
    };
    let Ok(stream) = TcpStream::connect_timeout(&sock_addr, remaining) else {
        return -1;
    };
    let _ = stream.set_nodelay(true);
    TCP_API_STATE.access(|state| {
        let handle = state.alloc_handle();
        state.streams.insert(handle, stream);
        handle
    })
}

/// Read up to 8192 bytes from a TCP connection into a fresh `bytes` value.
///
/// Returns a by-value [`crate::bytes::BytesTriple`] — the canonical `bytes`
/// representation codegen materialises for a `bytes`-typed return (a 16-byte
/// `{ptr, offset, len}` struct passed in `rax:rdx`). The triple OWNS a freshly
/// allocated, refcount-1 buffer holding exactly the bytes read (mirrors
/// `hew_bytes_from_static`'s construction + ownership). On EOF or error an
/// empty triple (`null` ptr, len 0) is returned — callers detect disconnect by
/// checking `.len() == 0`. The Hew drop spine releases the buffer via
/// `hew_bytes_drop`.
#[no_mangle]
pub extern "C" fn hew_tcp_read(conn: c_int) -> crate::bytes::BytesTriple {
    let empty = crate::bytes::BytesTriple {
        ptr: std::ptr::null_mut(),
        offset: 0,
        len: 0,
    };
    let Some(mut stream) = tcp_clone_stream(conn) else {
        return empty;
    };
    let mut buf = [0u8; 8192];
    match stream.read(&mut buf) {
        Ok(0) => empty,
        Err(e) => {
            record_tcp_error_kind(e.kind());
            empty
        }
        Ok(n) => {
            tcp_counters()
                .bytes_read
                .fetch_add(n as u64, Ordering::Relaxed);
            #[expect(
                clippy::cast_possible_truncation,
                reason = "TCP reads are bounded by the 8192-byte stack buffer"
            )]
            let len = n as u32;
            // SAFETY: `buf` is valid for `len` bytes; `hew_bytes_from_static`
            // copies them into a fresh, refcount-1 bytes allocation the caller
            // owns (identical construction/ownership to `hew_bytes_from_str`).
            unsafe { crate::bytes::hew_bytes_from_static(buf.as_ptr(), len) }
        }
    }
}

/// Out-pointer variant of [`hew_tcp_read`] that avoids the Windows x64 MSVC
/// sret mismatch for the 16-byte `BytesTriple` return.
///
/// Writes the result to `out` and returns void. The called `hew_tcp_read`
/// already handles the null-connection case by returning an empty triple.
///
/// # Safety
///
/// `out` must point to a valid, writable `BytesTriple` slot (caller-allocated).
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_read_raw(conn: c_int, out: *mut crate::bytes::BytesTriple) {
    let triple = hew_tcp_read(conn);
    // SAFETY: caller guarantees `out` points to a valid BytesTriple slot.
    unsafe { out.write(triple) };
}

/// Write a `bytes` value to a TCP connection.
///
/// Takes a POINTER to the caller's [`crate::bytes::BytesTriple`] (the address of
/// the `bytes` value's stack slot). The active region
/// `data.ptr[data.offset .. data.offset + data.len]` is written verbatim; no
/// newline is appended. The buffer is BORROWED for the duration of the call —
/// ownership stays with the caller, whose drop spine releases it via
/// `hew_bytes_drop`.
///
/// By-pointer (not by-value): a 16-byte triple passed by value as a non-first
/// argument loses its offset/len eightbyte at the current codegen C-ABI
/// boundary; passing the address is ABI-portable (mirrors `hew_bytes_push`).
/// Codegen passes the triple alloca's address for the `data: bytes` parameter
/// (`is_bytes_by_pointer_consumer`).
///
/// Returns number of bytes written, or -1 on error.
///
/// # Safety
///
/// `data` must point to a valid `BytesTriple` (non-null pointer to the caller's
/// triple slot): either its `ptr` is null with `len == 0`, or `ptr` points to a
/// `hew_bytes_*` allocation whose active region `[offset, offset + len)` is in
/// bounds.
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_write(
    conn: c_int,
    data: *const crate::bytes::BytesTriple,
) -> c_int {
    if data.is_null() {
        return -1;
    }
    // SAFETY: `data` points to the caller's valid BytesTriple slot.
    let data = unsafe { &*data };
    let len = data.len;
    if len == 0 || data.ptr.is_null() {
        // Empty write: nothing to send, succeeds with 0 bytes (matches the
        // previous empty-HewVec behaviour). A null ptr only ever pairs with
        // len 0 in a valid triple.
        return 0;
    }
    let Some(mut stream) = tcp_clone_stream(conn) else {
        hew_cabi::sink::set_last_error_with_errno(
            "hew_tcp_write: invalid connection handle".into(),
            9, // EBADF: Bad file descriptor
        );
        return -1;
    };
    // SAFETY: `data.ptr + data.offset` is valid for `data.len` bytes per the
    // BytesTriple contract; we only read (no mutation, no free).
    let payload =
        unsafe { std::slice::from_raw_parts(data.ptr.add(data.offset as usize), len as usize) };
    if let Err(e) = stream.write_all(payload) {
        record_tcp_error_kind(e.kind());
        // Surface the OS errno so the Hew side can classify the failure as
        // backpressure (EAGAIN/EWOULDBLOCK or a write-timeout ETIMEDOUT —
        // the send buffer is full and not draining within the deadline),
        // disconnect (ECONNRESET/EPIPE/ENOTCONN), or other.
        //
        // PARTIAL-WRITE CAUTION: with SO_SNDTIMEO set, `write_all` may have
        // committed a prefix of the payload to the kernel before the timeout
        // fired and returned BackpressureExceeded. A whole-payload retry is
        // therefore UNSAFE — it would duplicate the already-sent prefix. The
        // correct recovery for BackpressureExceeded is to drop or close the
        // connection and rely on application-level message framing; retrying
        // the same payload bytes is only safe if the application knows the
        // stream position is at a frame boundary (e.g., the error occurred
        // before any bytes were sent).
        //
        // `record_tcp_error_kind` deliberately does NOT count WouldBlock /
        // Interrupted / TimedOut as transport errors — backpressure is an
        // expected flow-control outcome, not a fault.
        hew_cabi::sink::set_last_error_with_errno(
            format!("hew_tcp_write: {e}"),
            e.raw_os_error().unwrap_or(0),
        );
        return -1;
    }
    tcp_counters()
        .bytes_written
        .fetch_add(u64::from(len), Ordering::Relaxed);
    let Ok(written) = c_int::try_from(len) else {
        hew_cabi::sink::set_last_error_with_errno(
            "hew_tcp_write: payload length exceeds c_int range".into(),
            22, // EINVAL: Invalid argument
        );
        return -1;
    };
    written
}

// W4.039 — the native Vec-backed `hew_bytes_to_string(*mut HewVec)` export
// was deleted when `Bytes -> String` conversion was canonicalised onto the
// `BytesTriple` ABI. The canonical entry point now lives in
// `hew-runtime/src/bytes.rs::hew_bytes_to_string` and consumes a single
// `#[repr(C)] BytesTriple` argument cross-target.

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;
    use std::io;
    use std::sync::{Arc, Mutex, PoisonError};

    fn run_in_isolated_test_process(test_name: &str, env_key: &str, body: impl FnOnce()) {
        if std::env::var_os(env_key).is_some() {
            body();
            return;
        }

        let output = std::process::Command::new(
            std::env::current_exe().expect("resolve current test binary"),
        )
        .arg(test_name)
        .arg("--exact")
        .arg("--nocapture")
        .arg("--test-threads=1")
        .env(env_key, "1")
        .output()
        .expect("spawn isolated test process");

        assert!(
            output.status.success(),
            "isolated test process failed for {test_name} (status: {:?})\nstdout:\n{}\nstderr:\n{}",
            output.status.code(),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );
    }

    fn connected_streams() -> (TcpStream, TcpStream) {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
        let addr = listener.local_addr().expect("read listener addr");
        let client = TcpStream::connect(addr).expect("connect loopback peer");
        let (server, _) = listener.accept().expect("accept loopback peer");
        (server, client)
    }

    /// A broken-pipe outbound `framed_send` must fail closed with `-1` instead
    /// of killing the process with SIGPIPE. This exercises the send-path
    /// defense-in-depth in isolation — `MSG_NOSIGNAL` on Linux/Android
    /// ([`FRAMED_SEND_FLAGS`]) and `SO_NOSIGPIPE` on macOS ([`suppress_sigpipe`])
    /// — WITHOUT installing the process-wide SIGPIPE ignore, so a regression in
    /// either per-socket defense would terminate the test process (a loud
    /// failure) rather than pass silently.
    #[cfg(unix)]
    #[test]
    fn framed_send_to_broken_pipe_fails_closed_without_signal() {
        use std::os::fd::OwnedFd;
        use std::os::unix::net::UnixStream;

        let (near, far) = UnixStream::pair().expect("socketpair");
        // Wrap the near end as the transport's socket type and apply the same
        // macOS SIGPIPE suppression the connection-store path applies.
        let sock: Socket = OwnedFd::from(near).into();
        suppress_sigpipe(&sock);
        drop(far); // fully close the peer

        // The peer is gone, so the header write inside framed_send hits EPIPE
        // and the function fails closed. Reaching the assertion proves the
        // process was not killed by SIGPIPE.
        let payload = [0xABu8; 4096];
        let rc = framed_send(&sock, &payload);
        assert_eq!(
            rc, -1,
            "framed_send to a broken pipe must fail closed with -1, not raise SIGPIPE"
        );
    }

    fn register_stream(stream: TcpStream) -> c_int {
        TCP_API_STATE.access(|state| {
            let handle = state.alloc_handle();
            state.streams.insert(handle, stream);
            handle
        })
    }

    fn remove_stream(handle: c_int) {
        TCP_API_STATE.access(|state| {
            state.streams.remove(&handle);
        });
    }

    fn register_listener(listener: TcpListener) -> c_int {
        TCP_API_STATE.access(|state| {
            let handle = state.alloc_handle();
            state.listeners.insert(handle, listener);
            handle
        })
    }

    fn remove_listener(handle: c_int) {
        TCP_API_STATE.access(|state| {
            state.listeners.remove(&handle);
        });
    }

    #[derive(Clone, Default)]
    struct TestLogBuffer {
        bytes: Arc<Mutex<Vec<u8>>>,
    }

    impl TestLogBuffer {
        fn contents(&self) -> String {
            let bytes = self.bytes.lock().unwrap_or_else(PoisonError::into_inner);
            String::from_utf8_lossy(&bytes).into_owned()
        }
    }

    struct TestLogWriter {
        bytes: Arc<Mutex<Vec<u8>>>,
    }

    impl io::Write for TestLogWriter {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            self.bytes
                .lock()
                .unwrap_or_else(PoisonError::into_inner)
                .extend_from_slice(buf);
            Ok(buf.len())
        }

        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    impl<'a> tracing_subscriber::fmt::MakeWriter<'a> for TestLogBuffer {
        type Writer = TestLogWriter;

        fn make_writer(&'a self) -> Self::Writer {
            TestLogWriter {
                bytes: Arc::clone(&self.bytes),
            }
        }
    }

    fn last_error_string() -> Option<String> {
        let ptr = crate::hew_last_error();
        if ptr.is_null() {
            return None;
        }
        // SAFETY: `hew_last_error` returns a NUL-terminated pointer valid until
        // the next error update on this thread.
        Some(
            unsafe { CStr::from_ptr(ptr) }
                .to_string_lossy()
                .into_owned(),
        )
    }

    #[cfg(feature = "profiler")]
    fn read_rust_allocs(payload: &[u8]) -> u64 {
        let (server, mut client) = connected_streams();
        let handle = register_stream(server);
        client.write_all(payload).expect("send payload");
        let before = crate::profiler::allocator::snapshot();
        let bytes = hew_tcp_read(handle);
        let after = crate::profiler::allocator::snapshot();
        // SAFETY: `hew_tcp_read` returns a caller-owned BytesTriple (rc 1).
        unsafe { crate::bytes::hew_bytes_drop(bytes.ptr) };
        remove_stream(handle);
        after.alloc_count - before.alloc_count
    }

    /// `hew_tcp_connect_timed` with `deadline_ms=0` connects to a
    /// listening loopback peer (no deadline path).
    #[test]
    fn tcp_connect_timed_zero_deadline_connects() {
        let _guard = crate::runtime_test_guard();
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
        let addr = listener
            .local_addr()
            .expect("read listener addr")
            .to_string();
        let addr_c = std::ffi::CString::new(addr).expect("addr cstring");
        // SAFETY: addr_c is a valid C string.
        let handle = unsafe { hew_tcp_connect_timed(addr_c.as_ptr(), 0) };
        assert!(handle >= 0, "expected a positive handle, got {handle}");
        // Drain the accept side so the listener doesn't block.
        let (_server, _) = listener.accept().expect("accept loopback");
        remove_stream(handle);
    }

    /// `hew_tcp_connect_timed` honours the deadline against a non-routable
    /// address. RFC 5737 192.0.2.0/24 ("TEST-NET-1") is reserved for
    /// documentation and is guaranteed not to be routed; the connect
    /// attempt cannot succeed and must return -1 inside the deadline.
    ///
    /// This is the regression check for #1415: the scheduler thread must
    /// not be parked beyond the requested deadline.
    #[test]
    fn tcp_connect_deadline() {
        let _guard = crate::runtime_test_guard();
        let addr_c = std::ffi::CString::new("192.0.2.1:65000").expect("addr cstring");
        let start = std::time::Instant::now();
        // SAFETY: addr_c is a valid C string.
        let handle = unsafe { hew_tcp_connect_timed(addr_c.as_ptr(), 200) };
        let elapsed = start.elapsed();
        assert_eq!(handle, -1, "expected connect to fail");
        // Allow generous slack for CI noise but still prove the deadline
        // is enforced (without it the kernel default ~75 s would apply).
        assert!(
            elapsed < std::time::Duration::from_secs(2),
            "connect with 200 ms deadline should return well before 2 s; got {elapsed:?}"
        );
    }

    #[test]
    fn tcp_read_returns_bytes_triple() {
        let _guard = crate::runtime_test_guard();
        let payload = b"hello tcp";
        let (server, mut client) = connected_streams();
        let handle = register_stream(server);
        client.write_all(payload).expect("send payload");

        let bytes = hew_tcp_read(handle);
        // The read owns a fresh refcount-1 buffer holding exactly the payload.
        assert!(!bytes.ptr.is_null(), "non-empty read must own a buffer");
        assert_eq!(bytes.offset, 0);
        assert_eq!(
            bytes.len,
            u32::try_from(payload.len()).expect("payload length fits in u32")
        );
        // SAFETY: `bytes.ptr + offset` is valid for `bytes.len` bytes.
        let read_slice = unsafe {
            std::slice::from_raw_parts(bytes.ptr.add(bytes.offset as usize), bytes.len as usize)
        };
        assert_eq!(read_slice, payload);

        // SAFETY: `hew_tcp_read` transfers ownership of the rc-1 buffer.
        unsafe { crate::bytes::hew_bytes_drop(bytes.ptr) };
        remove_stream(handle);
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn tcp_get_conn_reports_poison_instead_of_missing_conn() {
        let _guard = crate::runtime_test_guard();
        crate::hew_clear_error();
        let tcp = TcpTransport::new();
        assert!(matches!(tcp.get_conn(0), Ok(None)));

        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            tcp.conns.access(|conns| {
                conns[0] = None;
                panic!("intentional poison");
            });
        }));

        let logs = TestLogBuffer::default();
        let subscriber = tracing_subscriber::fmt()
            .with_ansi(false)
            .without_time()
            .with_target(false)
            .with_writer(logs.clone())
            .finish();

        tracing::subscriber::with_default(subscriber, || {
            assert!(matches!(
                tcp.get_conn(0),
                Err(ConnLookupError::Poisoned { transport: "tcp" })
            ));
            assert!(matches!(
                tcp.get_conn(0),
                Err(ConnLookupError::Poisoned { transport: "tcp" })
            ));
        });

        let last_error =
            last_error_string().expect("poisoned lookup should update the thread-local error");
        assert!(
            last_error.contains("tcp transport connection table poisoned while looking up conn 0"),
            "unexpected last error: {last_error}"
        );
        let log_output = logs.contents();
        assert_eq!(
            log_output
                .matches("tcp transport connection table poisoned while looking up conn 0")
                .count(),
            1,
            "poison should be logged once, saw logs:\n{log_output}"
        );
        crate::hew_clear_error();
    }

    #[cfg(feature = "profiler")]
    #[test]
    fn tcp_write_streams_bytes_without_rust_allocating() {
        run_in_isolated_test_process(
            "transport::tests::tcp_write_streams_bytes_without_rust_allocating",
            "HEW_RUNTIME_TCP_WRITE_ALLOC_TEST",
            || {
                let _guard = crate::runtime_test_guard();
                let payload = b"bytes over tcp";
                let (server, mut client) = connected_streams();
                let handle = register_stream(server);
                // The bytes buffer is built via the libc-backed bytes allocator
                // BEFORE the snapshot, so it is not counted; the Rust
                // GlobalAlloc counter must stay flat across the write.
                // SAFETY: `payload` is valid for `payload.len()` bytes.
                let bytes = unsafe {
                    crate::bytes::hew_bytes_from_static(
                        payload.as_ptr(),
                        u32::try_from(payload.len()).expect("payload length fits in u32"),
                    )
                };

                let before = crate::profiler::allocator::snapshot();
                // SAFETY: `bytes` is a valid caller-owned BytesTriple.
                let written = unsafe { hew_tcp_write(handle, std::ptr::addr_of!(bytes)) };
                let after = crate::profiler::allocator::snapshot();
                let expected_written =
                    c_int::try_from(payload.len()).expect("payload length fits in c_int");
                assert_eq!(written, expected_written);
                assert_eq!(
                    after.alloc_count - before.alloc_count,
                    0,
                    "tcp write should not allocate Rust heap scratch buffers"
                );

                let mut received = vec![0u8; payload.len()];
                client
                    .read_exact(&mut received)
                    .expect("read back written payload");
                assert_eq!(received, payload);

                // SAFETY: `bytes` owns a refcount-1 buffer allocated above.
                unsafe { crate::bytes::hew_bytes_drop(bytes.ptr) };
                remove_stream(handle);
            },
        );
    }

    #[test]
    fn tcp_write_surfaces_backpressure_errno_when_send_buffer_full() {
        run_in_isolated_test_process(
            "transport::tests::tcp_write_surfaces_backpressure_errno_when_send_buffer_full",
            "HEW_RUNTIME_TCP_WRITE_BACKPRESSURE",
            || {
                let _guard = crate::runtime_test_guard();
                // A connected pair where the peer NEVER reads. Writing past the
                // combined send+receive buffer capacity blocks; a short write
                // timeout converts that block into an EAGAIN/EWOULDBLOCK (Unix)
                // or WSAETIMEDOUT (Windows) error — the backpressure signal.
                let (server, _client) = connected_streams();
                let handle = register_stream(server);
                // Bound the wait: without a timeout a full buffer blocks forever.
                assert_eq!(
                    hew_tcp_set_write_timeout(handle, 100),
                    0,
                    "set_write_timeout should succeed on a valid handle"
                );

                // Drain any stale thread-local errno from earlier setup so the
                // value we read after the failing write is the write's own.
                let _ = hew_cabi::sink::take_last_errno();

                // 16 MiB payload: reliably exceeds the combined loopback send +
                // receive buffer on Linux and macOS (default ~144 KiB send +
                // ~399 KiB receive; payloads ≥ 2 MiB block within the 100 ms
                // timeout on those platforms).
                //
                // Windows is excluded below: Windows' TCP loopback autotuning
                // can absorb payloads well beyond 16 MiB within 100 ms because
                // the kernel continuously drains the send buffer into the receive
                // buffer via a zero-copy loopback path, bypassing SO_SNDBUF
                // limits regardless of buffer size settings. A small SO_SNDBUF
                // similarly does not force backpressure on macOS loopback (macOS
                // uses an analogous fast path for small send buffers, causing the
                // kernel to complete writes faster than the SO_SNDTIMEO can fire).
                // Cross-platform coverage of the write-side backpressure path on
                // Windows is provided by the e2e fixture
                // `tests/vertical-slice/accept/tcp_write_backpressure.hew`, which
                // connects to a peer that accepts but never drains and asserts
                // `WriteError::BackpressureExceeded` is observed.
                //
                // WHY: the loopback-fast-path is an OS-internal optimisation that
                // cannot be disabled via socket options; a reliable loopback-based
                // backpressure unit test on Windows would require a kernel-mode
                // shim or a non-loopback NIC. The e2e fixture is the right seam
                // for that coverage.
                // WHEN OBSOLETE: when the runtime gains a non-loopback integration
                // test harness that can reliably exhaust the Windows TCP send
                // window.
                let payload = vec![b'x'; 16 * 1024 * 1024];
                // SAFETY: `payload` is valid for `payload.len()` bytes.
                let bytes = unsafe {
                    crate::bytes::hew_bytes_from_static(
                        payload.as_ptr(),
                        u32::try_from(payload.len()).expect("payload length fits in u32"),
                    )
                };

                let errors_before = tcp_counters_snapshot().error_count;
                // SAFETY: `bytes` is a valid caller-owned BytesTriple.
                let written = unsafe { hew_tcp_write(handle, std::ptr::addr_of!(bytes)) };
                let errors_after = tcp_counters_snapshot().error_count;

                // Windows loopback absorbs the full payload without triggering
                // backpressure regardless of SO_SNDBUF settings (see comment
                // above). The errno and error-counter assertions are gated to
                // platforms where the loopback overwhelm approach is reliable.
                #[cfg(not(windows))]
                {
                    assert_eq!(
                        written, -1,
                        "a write that cannot complete within the timeout must report failure, \
                         not silently succeed"
                    );

                    // The surfaced errno must map to a backpressure outcome: EAGAIN
                    // (Linux 11 / macOS 35), EWOULDBLOCK (== EAGAIN), or a write
                    // timeout (Linux ETIMEDOUT 110 / macOS 60).
                    let errno = hew_cabi::sink::take_last_errno();
                    assert!(
                        matches!(errno, 11 | 35 | 60 | 110),
                        "write-timeout failure must surface a backpressure errno \
                         (EAGAIN/EWOULDBLOCK/ETIMEDOUT), got {errno}"
                    );

                    // Backpressure is flow control, not a transport fault: the error
                    // counter must NOT increment for WouldBlock/TimedOut.
                    assert_eq!(
                        errors_after, errors_before,
                        "backpressure (WouldBlock/TimedOut) must not count as a TCP error"
                    );
                }

                // SAFETY: `bytes` owns a refcount-1 buffer allocated above.
                unsafe { crate::bytes::hew_bytes_drop(bytes.ptr) };
                remove_stream(handle);
            },
        );
    }

    #[test]
    fn tcp_write_invalid_handle_surfaces_ebadf() {
        run_in_isolated_test_process(
            "transport::tests::tcp_write_invalid_handle_surfaces_ebadf",
            "HEW_RUNTIME_TCP_WRITE_EBADF",
            || {
                let _guard = crate::runtime_test_guard();
                let _ = hew_cabi::sink::take_last_errno();
                let payload = b"x";
                // SAFETY: `payload` is valid for `payload.len()` bytes.
                let bytes = unsafe {
                    crate::bytes::hew_bytes_from_static(
                        payload.as_ptr(),
                        u32::try_from(payload.len()).expect("payload length fits in u32"),
                    )
                };
                // 9999 is not a registered connection handle.
                // SAFETY: `bytes` is a valid caller-owned BytesTriple.
                let written = unsafe { hew_tcp_write(9999, std::ptr::addr_of!(bytes)) };
                assert_eq!(written, -1, "write to an invalid handle must fail");
                assert_eq!(
                    hew_cabi::sink::take_last_errno(),
                    9,
                    "an invalid connection handle must surface EBADF (9)"
                );
                // SAFETY: `bytes` owns a refcount-1 buffer allocated above.
                unsafe { crate::bytes::hew_bytes_drop(bytes.ptr) };
            },
        );
    }

    #[cfg(feature = "profiler")]
    #[test]
    fn tcp_read_stays_at_zero_rust_allocs_across_payload_sizes() {
        run_in_isolated_test_process(
            "transport::tests::tcp_read_stays_at_zero_rust_allocs_across_payload_sizes",
            "HEW_RUNTIME_TCP_READ_ALLOC_TEST",
            || {
                let _guard = crate::runtime_test_guard();
                let small = read_rust_allocs(b"x");
                let large_payload = vec![b'x'; 4096];
                let large = read_rust_allocs(&large_payload);
                assert_eq!(
                    small, large,
                    "tcp read Rust allocator cost should stay constant across payload sizes"
                );
                assert!(
                    large == 0,
                    "tcp read should stay at zero Rust allocator calls, saw {large}"
                );
            },
        );
    }

    #[test]
    fn tcp_counters_zero_init() {
        run_in_isolated_test_process(
            "transport::tests::tcp_counters_zero_init",
            "HEW_RUNTIME_TCP_COUNTERS_ZERO_INIT",
            || {
                let _guard = crate::runtime_test_guard();
                assert_eq!(tcp_counters_snapshot(), TcpCountersSnapshot::default());
            },
        );
    }

    #[test]
    fn tcp_counters_read_write_increment() {
        run_in_isolated_test_process(
            "transport::tests::tcp_counters_read_write_increment",
            "HEW_RUNTIME_TCP_COUNTERS_RW",
            || {
                let _guard = crate::runtime_test_guard();
                let payload = b"hello tcp counters";
                let (server, mut client) = connected_streams();
                let handle = register_stream(server);
                let before = tcp_counters_snapshot();

                // SAFETY: `payload` is valid for `payload.len()` bytes.
                let bytes = unsafe {
                    crate::bytes::hew_bytes_from_static(
                        payload.as_ptr(),
                        u32::try_from(payload.len()).expect("payload length fits in u32"),
                    )
                };

                // SAFETY: `bytes` is a valid caller-owned BytesTriple.
                let written = unsafe { hew_tcp_write(handle, std::ptr::addr_of!(bytes)) };
                assert_eq!(
                    written,
                    c_int::try_from(payload.len()).expect("payload length fits in c_int")
                );

                let mut echoed = vec![0u8; payload.len()];
                client
                    .read_exact(&mut echoed)
                    .expect("read payload from writer");
                assert_eq!(echoed, payload);

                client.write_all(payload).expect("send payload to reader");
                let read_bytes = hew_tcp_read(handle);
                let read_len = i64::from(read_bytes.len);
                assert_eq!(
                    read_len,
                    i64::try_from(payload.len()).expect("payload length fits in i64")
                );

                let after = tcp_counters_snapshot();
                assert_eq!(
                    after.bytes_written - before.bytes_written,
                    u64::try_from(payload.len()).expect("payload length fits in u64")
                );
                assert_eq!(
                    after.bytes_read - before.bytes_read,
                    u64::try_from(payload.len()).expect("payload length fits in u64")
                );

                // SAFETY: both triples own a refcount-1 buffer.
                unsafe {
                    crate::bytes::hew_bytes_drop(bytes.ptr);
                    crate::bytes::hew_bytes_drop(read_bytes.ptr);
                }
                remove_stream(handle);
            },
        );
    }

    #[test]
    fn tcp_counters_accept_connect_increment() {
        run_in_isolated_test_process(
            "transport::tests::tcp_counters_accept_connect_increment",
            "HEW_RUNTIME_TCP_COUNTERS_ACCEPT_CONNECT",
            || {
                let _guard = crate::runtime_test_guard();
                let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
                let addr = listener.local_addr().expect("read listener addr");
                let listener_handle = register_listener(listener);
                let before = tcp_counters_snapshot();
                let addr_cstr = CString::new(addr.to_string()).expect("listener addr has no nuls");

                let accept_thread = std::thread::spawn(move || {
                    let accepted = hew_tcp_accept(listener_handle);
                    assert!(accepted >= 0, "accept returned valid handle");
                    remove_stream(accepted);
                    remove_listener(listener_handle);
                });

                // SAFETY: `addr_cstr` is a valid NUL-terminated address string.
                let conn = unsafe { hew_tcp_connect(addr_cstr.as_ptr()) };
                assert!(conn >= 0, "connect returned valid handle");
                remove_stream(conn);
                accept_thread.join().expect("join accept thread");

                let after = tcp_counters_snapshot();
                assert_eq!(after.accept_count - before.accept_count, 1);
                assert_eq!(after.connect_count - before.connect_count, 1);
            },
        );
    }

    #[test]
    fn tcp_counters_error_policy() {
        run_in_isolated_test_process(
            "transport::tests::tcp_counters_error_policy",
            "HEW_RUNTIME_TCP_COUNTERS_ERRORS",
            || {
                let _guard = crate::runtime_test_guard();
                let unused_listener =
                    TcpListener::bind("127.0.0.1:0").expect("bind unused-port probe listener");
                let unused_addr = unused_listener
                    .local_addr()
                    .expect("read unused-port probe addr");
                drop(unused_listener);

                let connect_addr =
                    CString::new(unused_addr.to_string()).expect("addr contains no interior nuls");
                let before_connect = tcp_counters_snapshot();
                // SAFETY: `connect_addr` is a valid address string.
                let result = unsafe { hew_tcp_connect(connect_addr.as_ptr()) };
                assert_eq!(result, -1, "connect to unused port should fail");
                let after_connect = tcp_counters_snapshot();
                assert_eq!(after_connect.error_count - before_connect.error_count, 1);

                let (server, _client) = connected_streams();
                server
                    .set_nonblocking(true)
                    .expect("set server stream nonblocking");
                let handle = register_stream(server);
                let before_read = tcp_counters_snapshot();
                let read_bytes = hew_tcp_read(handle);
                let after_read = tcp_counters_snapshot();
                assert_eq!(
                    after_read.error_count - before_read.error_count,
                    0,
                    "WouldBlock must not count as a TCP error"
                );
                // WouldBlock yields an empty triple (null ptr); drop is a no-op.
                assert!(read_bytes.ptr.is_null());
                // SAFETY: a null ptr is a valid no-op for hew_bytes_drop.
                unsafe { crate::bytes::hew_bytes_drop(read_bytes.ptr) };
                remove_stream(handle);

                // Explicit filter policy regression: WouldBlock, Interrupted,
                // and TimedOut must not bump error_count (e.g. TimedOut can
                // arise from user-configured SO_RCVTIMEO on Windows).
                let before_filter = tcp_counters_snapshot();
                record_tcp_error_kind(ErrorKind::WouldBlock);
                record_tcp_error_kind(ErrorKind::Interrupted);
                record_tcp_error_kind(ErrorKind::TimedOut);
                let after_filter = tcp_counters_snapshot();
                assert_eq!(
                    after_filter.error_count, before_filter.error_count,
                    "WouldBlock/Interrupted/TimedOut must not count as TCP errors"
                );
                record_tcp_error_kind(ErrorKind::ConnectionReset);
                let after_reset = tcp_counters_snapshot();
                assert_eq!(
                    after_reset.error_count - after_filter.error_count,
                    1,
                    "real I/O errors must bump error_count"
                );
            },
        );
    }

    #[test]
    fn tcp_counters_bytes_written_are_monotonic_under_concurrency() {
        run_in_isolated_test_process(
            "transport::tests::tcp_counters_bytes_written_are_monotonic_under_concurrency",
            "HEW_RUNTIME_TCP_COUNTERS_CONCURRENCY",
            || {
                let _guard = crate::runtime_test_guard();
                let payload = Arc::new(*b"tcp-counter-payload");
                let writes_per_thread = 32usize;
                let thread_count = 6usize;
                let total_expected = payload.len() * writes_per_thread * thread_count;
                let (server, mut client) = connected_streams();
                let handle = register_stream(server);
                let before = tcp_counters_snapshot();

                let reader = std::thread::spawn(move || {
                    let mut received = vec![0u8; total_expected];
                    client
                        .read_exact(&mut received)
                        .expect("drain concurrent writes");
                });

                let mut writers = Vec::new();
                for _ in 0..thread_count {
                    let payload = Arc::clone(&payload);
                    writers.push(std::thread::spawn(move || {
                        for _ in 0..writes_per_thread {
                            // SAFETY: `payload` is valid for its full length.
                            let bytes = unsafe {
                                crate::bytes::hew_bytes_from_static(
                                    payload.as_ptr(),
                                    u32::try_from(payload.len())
                                        .expect("payload length fits in u32"),
                                )
                            };
                            // SAFETY: `bytes` is a valid caller-owned BytesTriple.
                            let written =
                                unsafe { hew_tcp_write(handle, std::ptr::addr_of!(bytes)) };
                            assert_eq!(
                                written,
                                c_int::try_from(payload.len())
                                    .expect("payload length fits in c_int")
                            );
                            // SAFETY: `bytes` owns a refcount-1 buffer above.
                            unsafe { crate::bytes::hew_bytes_drop(bytes.ptr) };
                        }
                    }));
                }

                for writer in writers {
                    writer.join().expect("join writer thread");
                }
                reader.join().expect("join reader thread");

                let after = tcp_counters_snapshot();
                assert_eq!(
                    after.bytes_written - before.bytes_written,
                    u64::try_from(total_expected).expect("total expected bytes fit in u64")
                );
                remove_stream(handle);
            },
        );
    }
}

/// Broadcast one message to all open TCP connections except `exclude_conn`.
///
/// Appends `\n` if not already present.
/// Returns number of recipients written to, or -1 on error.
///
/// # Safety
///
/// `msg` must be a valid, non-null pointer to a `BytesTriple` whose active
/// region `[offset, offset + len)` contains valid UTF-8 text.
/// (`is_bytes_by_pointer_consumer` in codegen passes the triple's alloca
/// address rather than the struct value — the previous `*const c_char`
/// signature ignored `offset` and only worked by accident when offset==0.)
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_broadcast_except(
    exclude_conn: c_int,
    msg: *const crate::bytes::BytesTriple,
) -> c_int {
    cabi_guard!(msg.is_null(), {
        hew_cabi::sink::set_last_error_with_errno(
            "hew_tcp_broadcast_except: null message pointer".into(),
            22, // EINVAL: Invalid argument
        );
        -1
    });
    // SAFETY: caller guarantees `msg` points to a valid BytesTriple.
    let triple = unsafe { &*msg };
    let active = if triple.len == 0 || triple.ptr.is_null() {
        b"" as &[u8]
    } else {
        // SAFETY: BytesTriple invariant: ptr+offset..ptr+offset+len is valid.
        unsafe {
            std::slice::from_raw_parts(triple.ptr.add(triple.offset as usize), triple.len as usize)
        }
    };
    let Ok(text) = std::str::from_utf8(active) else {
        hew_cabi::sink::set_last_error_with_errno(
            "hew_tcp_broadcast_except: invalid UTF-8 in message".into(),
            22, // EINVAL: Invalid argument
        );
        return -1;
    };
    TCP_API_STATE.access(|state| {
        let mut recipients = 0usize;
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
    })
}

/// Attach a TCP connection to an actor for active-mode delivery.
///
/// After this call the connection's socket is non-blocking and registered with
/// the active-mode reactor: each chunk of inbound data is delivered to the
/// actor as an `on_data(bytes)` message (`on_data_type`), and a single
/// `on_close()` message (`on_close_type`) is delivered when the peer closes or
/// the socket errors. The actor must NOT call blocking `read`/`read_string`
/// after attach (the reactor owns the read side); outbound `send`/`write`
/// remain valid until close.
///
/// Returns 0 on success, -1 on failure (unknown handle, reactor unavailable).
///
/// # Safety
///
/// - `conn` must be a valid TCP connection handle from the stdlib `net` API.
/// - `actor_ref` must point to a valid [`HewActorRef`] for the duration of the
///   call (a by-value snapshot is taken; mirrors `hew_ws_attach`).
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_attach(
    conn: c_int,
    actor_ref: *const HewActorRef,
    on_data_type: i32,
    on_close_type: i32,
) -> c_int {
    // SAFETY: caller guarantees `actor_ref` is valid for this call; the reactor
    // takes a by-value snapshot before returning.
    unsafe { crate::reactor::reactor_attach(conn, actor_ref, on_data_type, on_close_type) }
}

/// Attach a TCP connection to a *local* actor, identified by its raw
/// `*mut HewActor` pointer rather than a fully-formed `HewActorRef`.
///
/// This is the entry point the Hew `conn.attach(handler)` surface lowers to:
/// a Hew `LocalPid<T>` reaches the C ABI as the bare actor pointer (no
/// `HewActorRef` wrapper), so codegen cannot hand `hew_tcp_attach` the
/// `*const HewActorRef` it expects. This wrapper constructs the local
/// `HewActorRef` on the runtime side (the owner of that layout) and forwards
/// to the same reactor path. Mirrors `hew_ws_attach`, which likewise accepts
/// the raw actor pointer for the active-mode WebSocket surface.
///
/// `on_data_type` / `on_close_type` are the `msg_id`s (SipHash-1-3 over the
/// handler's fully-qualified name) the compiler synthesises at the attach call
/// site from the concrete actor's `on_data` / `on_close` receive functions.
///
/// Returns 0 on success, -1 on failure (unknown handle, reactor unavailable,
/// leak-prone mailbox).
///
/// # Safety
///
/// - `conn` must be a valid TCP connection handle from the stdlib `net` API.
/// - `actor` must be a valid pointer to a live [`HewActor`] that outlives the
///   connection's active-mode registration.
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_attach_local(
    conn: c_int,
    actor: *mut HewActor,
    on_data_type: i32,
    on_close_type: i32,
) -> c_int {
    if actor.is_null() {
        set_last_error("hew_tcp_attach_local: null actor pointer");
        return -1;
    }
    // SAFETY: `actor` is non-null (checked above) and the caller guarantees it
    // points to a live actor for this call.
    let actor_ref = unsafe { hew_actor_ref_local(actor) };
    // SAFETY: `actor_ref` is a valid stack-local `HewActorRef`; `reactor_attach`
    // takes a by-value snapshot before returning, so the address is only needed
    // for the duration of this call.
    unsafe {
        crate::reactor::reactor_attach(conn, &raw const actor_ref, on_data_type, on_close_type)
    }
}

/// Register a TCP connection for a SUSPENDING `await conn.read()` (NEW-1).
///
/// The codegen ramp for `Terminator::SuspendingRead` calls this from a
/// suspendable handler: it has created a `HewReadSlot` (held across the suspend
/// in the coro frame) and parked its continuation on `actor`. This forwards to
/// the reactor's resume-mode registration: when the fd becomes readable the
/// reactor reads the bytes, deposits the result into `read_slot`, and
/// `enqueue_resume`s the parked continuation.
///
/// `actor` is the raw `*mut HewActor` the Hew `LocalPid` lowers to (same shape
/// as `hew_tcp_attach_local`); the runtime constructs the local `HewActorRef`.
///
/// Returns 0 on success, -1 on failure (null args, unknown handle, reactor
/// unavailable). On failure the caller's slot ref is untouched and the codegen
/// ramp binds the error edge.
///
/// # Safety
///
/// - `conn` must be a valid TCP connection handle from the stdlib `net` API.
/// - `actor` must be a valid pointer to a live [`HewActor`] that owns the parked
///   continuation registered for this read.
/// - `read_slot` must be a valid live `HewReadSlot` the caller holds a ref to.
#[no_mangle]
pub unsafe extern "C" fn hew_conn_await_read(
    conn: c_int,
    actor: *mut HewActor,
    read_slot: *mut crate::read_slot::HewReadSlot,
) -> c_int {
    if actor.is_null() {
        set_last_error("hew_conn_await_read: null actor pointer");
        return -1;
    }
    // SAFETY: `actor` is non-null and the caller guarantees it is live.
    let actor_ref = unsafe { hew_actor_ref_local(actor) };
    // SAFETY: `actor_ref` is a valid stack-local `HewActorRef`; the reactor takes
    // a by-value snapshot before returning. `read_slot` validity is the caller's
    // contract (the reactor takes its own ref on success).
    unsafe { crate::reactor::reactor_await_read(conn, &raw const actor_ref, read_slot) }
}

/// Register a TCP listener for a SUSPENDING `await listener.accept()` (NEW-2,
/// the listener-readiness sibling of [`hew_conn_await_read`]).
///
/// The codegen ramp for `Terminator::SuspendingAccept` calls this from a
/// suspendable handler: it has created a `HewReadSlot` (held across the suspend
/// in the coro frame) and parked its continuation on `actor`. This forwards to
/// the reactor's accept-mode registration: when the listener fd becomes readable
/// the reactor `accept()`s a new connection, deposits its i64 handle into
/// `read_slot`, and `enqueue_resume`s the parked continuation.
///
/// Returns 0 on success, -1 on failure (null args, unknown listener handle,
/// reactor unavailable). On failure the caller's slot ref is untouched and the
/// codegen ramp binds an invalid `Connection` on the resume edge.
///
/// # Safety
///
/// - `listener` must be a valid TCP listener handle from the stdlib `net` API.
/// - `actor` must be a valid pointer to a live [`HewActor`] that owns the parked
///   continuation registered for this accept.
/// - `read_slot` must be a valid live `HewReadSlot` the caller holds a ref to.
#[no_mangle]
pub unsafe extern "C" fn hew_listener_await_accept(
    listener: c_int,
    actor: *mut HewActor,
    read_slot: *mut crate::read_slot::HewReadSlot,
) -> c_int {
    if actor.is_null() {
        set_last_error("hew_listener_await_accept: null actor pointer");
        return -1;
    }
    // SAFETY: `actor` is non-null and the caller guarantees it is live.
    let actor_ref = unsafe { hew_actor_ref_local(actor) };
    // SAFETY: `actor_ref` is a valid stack-local `HewActorRef`; the reactor takes
    // a by-value snapshot before returning. `read_slot` validity is the caller's
    // contract (the reactor takes its own ref on success).
    unsafe { crate::reactor::reactor_await_accept(listener, &raw const actor_ref, read_slot) }
}

/// Detach a TCP connection from the active-mode reactor without closing it.
///
/// Idempotent; a no-op if the connection was never attached.
#[no_mangle]
pub extern "C" fn hew_tcp_detach(conn: c_int) {
    crate::reactor::reactor_detach_conn(conn);
}

/// Fully close a TCP *connection* handle: detach it from the active-mode
/// reactor, remove its table entry, and `shutdown(Both)` the socket before the
/// stored `TcpStream` drops (closing the fd). Returns `true` if a connection
/// entry was present and removed, `false` if `handle` is not a live connection.
///
/// This is the shared connection-close body used by [`hew_tcp_close`] (its
/// connection branch) and by the clone-failure early-returns in
/// `hew_tcp_stream_from_conn`. Factoring it here keeps a single close path so a
/// *consumed* connection is released on every return path of the stream bridge,
/// not only on full success.
///
/// Unlike [`tcp_release_conn`] (remove-without-shutdown, which keeps the socket
/// alive for the two clones the success path just made) this fully tears the
/// connection down. On the clone-failure paths there is no surviving clone that
/// shares this fd — either no clone was made (first-clone failure) or the one
/// live clone (`read_stream` on second-clone failure) is a distinct dup'd fd
/// that RAII-drops independently — so `shutdown(Both)` here is safe and closes
/// exactly the original fd.
pub(crate) fn tcp_full_close_conn(handle: c_int) -> bool {
    // Detach from the active-mode reactor first so the reactor stops polling a
    // fd we are about to close (reactor-fd ownership: unregister before close).
    // No-op on the bridge failure path (that path never attached to the
    // reactor); kept for symmetry with `hew_tcp_close` and robustness if the
    // call graph ever changes.
    crate::reactor::reactor_detach_conn(handle);
    TCP_API_STATE.access(|state| {
        if let Some(stream) = state.streams.remove(&handle) {
            let _ = stream.shutdown(Shutdown::Both);
            true
        } else {
            false
        }
    })
}

/// Close either a TCP connection handle or listener handle.
///
/// Returns 0 on success, -1 if handle is unknown.
#[no_mangle]
pub extern "C" fn hew_tcp_close(handle: c_int) -> c_int {
    // Connection branch: detach from the reactor, remove, and shutdown.
    if tcp_full_close_conn(handle) {
        return 0;
    }
    // Otherwise it may be a listener handle.
    TCP_API_STATE.access(|state| {
        if state.listeners.remove(&handle).is_some() {
            return 0;
        }
        -1
    })
}

/// Close a TCP listener handle.
///
/// This listener-typed shim keeps the stdlib surface method type-safe while
/// reusing the shared TCP close implementation.
#[no_mangle]
pub extern "C" fn hew_tcp_listener_close(listener: c_int) -> c_int {
    hew_tcp_close(listener)
}

/// Check whether a listener handle is valid (positive).
///
/// Returns `true` for positive handles, `false` for error values (≤ 0).
/// Used by `try_listen` to distinguish success from failure without panicking.
#[no_mangle]
pub extern "C" fn hew_listener_is_valid(handle: c_int) -> bool {
    handle > 0
}

/// Check whether a connection handle is valid (positive).
///
/// Returns `true` for positive handles, `false` for error values (≤ 0).
/// Used by `try_connect` to distinguish success from failure without panicking.
#[no_mangle]
pub extern "C" fn hew_connection_is_valid(handle: c_int) -> bool {
    handle > 0
}

// ===========================================================================
// Unix domain socket transport
// ===========================================================================

#[cfg(unix)]
mod unix_transport {
    use super::{
        accept_with_optional_timeout, c_char, c_int, c_void, framed_recv, framed_send,
        report_conn_table_poison, suppress_sigpipe, CStr, ConnLookupError, Domain, HewTransport,
        HewTransportOps, PoisonSafeRw, SockAddr, Socket, Type, HEW_CONN_INVALID, MAX_CONNS,
    };
    use std::net::Shutdown;
    use std::sync::atomic::AtomicBool;

    /// Internal state for the Unix domain socket transport.
    struct UnixTransport {
        listen_sock: Option<Socket>,
        path: Option<String>,
        conns: PoisonSafeRw<Vec<Option<Socket>>>,
        conn_poison_reported: AtomicBool,
    }

    impl UnixTransport {
        fn new() -> Self {
            Self {
                listen_sock: None,
                path: None,
                conns: PoisonSafeRw::new((0..MAX_CONNS).map(|_| None).collect()),
                conn_poison_reported: AtomicBool::new(false),
            }
        }

        fn store_conn(&self, sock: Socket) -> c_int {
            // macOS parity: see the TCP `store_conn` note — SO_NOSIGPIPE so a
            // broken-pipe write fails closed with EPIPE instead of SIGPIPE.
            suppress_sigpipe(&sock);
            if let Ok(conn) = self.conns.write(|conns| {
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
            }) {
                conn
            } else {
                report_conn_table_poison("unix", &self.conn_poison_reported, "storing conn", None);
                HEW_CONN_INVALID
            }
        }

        fn get_conn(&self, id: c_int) -> Result<Option<Socket>, ConnLookupError> {
            if id < 0 {
                return Ok(None);
            }
            #[expect(clippy::cast_sign_loss, reason = "guarded by id >= 0")]
            let idx = id as usize;
            self.conns
                .read(|conns| {
                    conns
                        .get(idx)
                        .and_then(Option::as_ref)
                        .and_then(|sock| sock.try_clone().ok())
                })
                .map_err(|_| {
                    report_conn_table_poison(
                        "unix",
                        &self.conn_poison_reported,
                        "looking up",
                        Some(id),
                    );
                    ConnLookupError::Poisoned { transport: "unix" }
                })
        }

        fn remove_conn(&self, id: c_int) {
            if id >= 0 {
                #[expect(clippy::cast_sign_loss, reason = "guarded by id >= 0")]
                let idx = id as usize;
                if idx < MAX_CONNS
                    && self
                        .conns
                        .write(|conns| {
                            // Shutdown before dropping — see TcpTransport::remove_conn.
                            if let Some(ref sock) = conns[idx] {
                                let _ = sock.shutdown(Shutdown::Both);
                            }
                            conns[idx] = None;
                        })
                        .is_err()
                {
                    report_conn_table_poison(
                        "unix",
                        &self.conn_poison_reported,
                        "removing conn",
                        Some(id),
                    );
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
        cabi_guard!(impl_ptr.is_null() || address.is_null(), HEW_CONN_INVALID);
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
        cabi_guard!(impl_ptr.is_null() || address.is_null(), -1);
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
        cabi_guard!(impl_ptr.is_null(), HEW_CONN_INVALID);
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
        cabi_guard!(impl_ptr.is_null() || data.is_null(), -1);
        // SAFETY: impl_ptr points to a valid UnixTransport.
        let ut = unsafe { &*impl_ptr.cast::<UnixTransport>() };
        let sock = match ut.get_conn(conn) {
            Ok(Some(sock)) => sock,
            Ok(None) | Err(ConnLookupError::Poisoned { .. }) => return -1,
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
        cabi_guard!(impl_ptr.is_null() || buf.is_null(), -1);
        // SAFETY: impl_ptr points to a valid UnixTransport.
        let ut = unsafe { &*impl_ptr.cast::<UnixTransport>() };
        let sock = match ut.get_conn(conn) {
            Ok(Some(sock)) => sock,
            Ok(None) | Err(ConnLookupError::Poisoned { .. }) => return -1,
        };
        // SAFETY: buf is valid for buf_size bytes per caller contract.
        let slice = unsafe { std::slice::from_raw_parts_mut(buf.cast::<u8>(), buf_size) };
        framed_recv(&sock, slice)
    }

    unsafe extern "C" fn unix_close_conn(impl_ptr: *mut c_void, conn: c_int) {
        cabi_guard!(impl_ptr.is_null());
        // SAFETY: impl_ptr points to a valid UnixTransport.
        let ut = unsafe { &*impl_ptr.cast::<UnixTransport>() };
        ut.remove_conn(conn);
    }

    unsafe extern "C" fn unix_destroy(impl_ptr: *mut c_void) {
        cabi_guard!(impl_ptr.is_null());
        // SAFETY: impl_ptr was created by Box::into_raw in hew_transport_unix_new.
        let _ = unsafe { Box::from_raw(impl_ptr.cast::<UnixTransport>()) }; // ALLOCATOR-PAIRING: GlobalAlloc
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
            r#impl: Box::into_raw(ut).cast::<c_void>(), // ALLOCATOR-PAIRING: GlobalAlloc
        });
        Box::into_raw(transport) // ALLOCATOR-PAIRING: GlobalAlloc
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
