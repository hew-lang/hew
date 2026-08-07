// WASM-TODO(websocket): WebSocket transport is unavailable on WASM (requires OS threads).
//! Hew runtime: `websocket` module.
//!
//! Provides synchronous WebSocket client functionality for compiled Hew programs.
//! All returned data pointers are allocated with `libc::malloc` so callers can
//! free them with the corresponding free function.

use crate::bind_addr::normalize_bind_addr;
use hew_cabi::cabi::{alloc_cstring, free_cstring, malloc_bytes};
use std::ffi::{c_void, CStr};
use std::io::{self, Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream, ToSocketAddrs};
use std::os::raw::{c_char, c_int};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex, MutexGuard, PoisonError};

use parking_lot::Mutex as PlMutex;
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

#[cfg(test)]
use tungstenite::client::connect_with_config;
use tungstenite::client::{client_with_config, uri_mode, IntoClientRequest};
use tungstenite::protocol::{Role, WebSocketConfig};
use tungstenite::stream::{MaybeTlsStream, Mode};
use tungstenite::{Message, WebSocket};

/// Test-only drop counter for the outer `HewWsConn` Box.
///
/// WHY: Validates that `hew_ws_close` reclaims the outer Box on every close
///      path (attached and unattached). Only enabled in `#[cfg(test)]` so
///      there is zero overhead in production builds.
/// WHEN: Remove when the counter is no longer needed for leak detection.
/// WHAT: A production alternative is an external allocator hook.
#[cfg(test)]
static OUTER_CONN_DROPS: AtomicUsize = AtomicUsize::new(0);
#[cfg(test)]
static ACTOR_RUNTIME_CALLS: AtomicUsize = AtomicUsize::new(0);

/// Opaque WebSocket connection handle.
///
/// Wraps a `tungstenite` [`WebSocket`] over a potentially-TLS TCP stream.
/// Must be closed with [`hew_ws_close`].
#[derive(Debug)]
pub struct HewWsConn {
    inner: Arc<HewWsConnInner>,
}

#[derive(Debug)]
struct HewWsConnInner {
    /// The primary WebSocket handle.  In attached mode this is used for reading
    /// only; user sends go through `write_ws` when available.  In non-attached
    /// (recv) mode the single mutex covers both directions as before.
    ws: PlMutex<Option<HewWs>>,
    /// Separate write-only WebSocket for plain-TCP connections.
    ///
    /// WHY: tungstenite's `WebSocket` is single-owner with a blocking `read()`
    ///      that holds the mutex for up to `READER_READ_TIMEOUT` (250 ms).
    ///      `hew_ws_send_text`/`hew_ws_send_binary` acquire the same mutex, so
    ///      each send used to stall on the reader framer in attached mode.
    ///      Plain TCP sends now use a separate framer plus `write_operation_gate`
    ///      so they avoid reader-framer mutex contention while still serializing
    ///      complete tungstenite write operations. Fix #1324/#1632.
    /// WHEN: This split is only possible for plain TCP; TLS connections still
    ///       use the single `ws` mutex (the TLS record layer is not thread-safe
    ///       across two independently-created contexts).
    /// WHAT: A proper TLS split would require a mutex-protected TLS write half
    ///       that the two WebSocket frames share — out of scope for this change.
    ///
    /// Both `ws` and `write_ws` use `parking_lot::Mutex` because
    /// `hew_ws_recv_timeout` needs `try_lock_for(Duration)` to bound the wait
    /// for the reader thread (which may hold the lock for up to
    /// `READER_READ_TIMEOUT` (250 ms) per cycle). `parking_lot::Mutex` does
    /// not poison, so the previous `lock_or_recover` recovery is unnecessary
    /// for these locks and is replaced with `pl_lock`.
    write_ws: Option<PlMutex<Option<HewWs>>>,
    /// Plain-TCP split-framer serialization gate.
    ///
    /// LOCK ORDER: acquire a framer mutex (`ws` / `write_ws`) before this gate;
    /// tungstenite may then acquire `SharedPlainWsStream::write_stream` inside
    /// its `Write` calls. This gate is held across the complete tungstenite
    /// operation (`send` or `read` auto-flush) so short socket writes cannot
    /// allow another framer to splice control-frame bytes into a data frame.
    write_operation_gate: Option<WriteOperationGate>,
    shutdown_stream: Option<TcpStream>,
    reader: Mutex<Option<ReaderControl>>,
    closed: AtomicBool,
    active_recvs: AtomicUsize,
}

type HewWs = WebSocket<HewWsStream>;
type WriteOperationGate = Arc<PlMutex<()>>;
type PreparedWebsockets = (HewWs, Option<HewWs>, Option<WriteOperationGate>);

/// Stream type used by Hew's websocket wrapper after connection setup.
///
/// Plain TCP can be split into independent tungstenite framers for attached
/// mode. TLS remains a single stream because two independently-created TLS
/// contexts cannot safely share one socket.
#[derive(Debug)]
enum HewWsStream {
    Plain(TcpStream),
    SplitPlain(SharedPlainWsStream),
    Tls(MaybeTlsStream<TcpStream>),
}

/// Plain-TCP attached-mode stream with a shared socket write half.
///
/// LOCK ORDER: Hew code may acquire a framer mutex (`inner.ws` or
/// `inner.write_ws`), then `write_operation_gate`, and then tungstenite may
/// call this stream's `Write` implementation, which briefly locks
/// `write_stream`. No Hew code may lock `write_stream` and then acquire the
/// operation gate or a framer mutex.
///
/// SERIALIZATION INVARIANT: `write_stream` is only the shared-fd ownership
/// mutex. Frame/operation atomicity comes from `write_operation_gate`, which is
/// held above tungstenite `send` and `read` auto-flush operations. That outer
/// gate is what prevents a short blocking `TcpStream::write` from letting a
/// competing framer splice Pong bytes into the middle of a data frame.
#[derive(Debug)]
struct SharedPlainWsStream {
    read_stream: TcpStream,
    write_stream: Arc<PlMutex<TcpStream>>,
}

impl Read for SharedPlainWsStream {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.read_stream.read(buf)
    }
}

impl Write for SharedPlainWsStream {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        pl_lock(&self.write_stream).write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        pl_lock(&self.write_stream).flush()
    }
}

impl Read for HewWsStream {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match self {
            Self::Plain(stream) => stream.read(buf),
            Self::SplitPlain(stream) => stream.read(buf),
            Self::Tls(stream) => stream.read(buf),
        }
    }
}

impl Write for HewWsStream {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            Self::Plain(stream) => stream.write(buf),
            Self::SplitPlain(stream) => stream.write(buf),
            Self::Tls(stream) => stream.write(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            Self::Plain(stream) => stream.flush(),
            Self::SplitPlain(stream) => stream.flush(),
            Self::Tls(stream) => stream.flush(),
        }
    }
}

#[derive(Debug)]
struct ReaderControl {
    cancel: Arc<AtomicBool>,
    exited: Arc<AtomicBool>,
    delivery: Arc<ActorDelivery>,
    join: Option<JoinHandle<()>>,
}

#[derive(Debug)]
struct ActiveCallGuard<'a> {
    counter: &'a AtomicUsize,
}

impl<'a> ActiveCallGuard<'a> {
    fn new(counter: &'a AtomicUsize) -> Self {
        counter.fetch_add(1, Ordering::AcqRel);
        Self { counter }
    }
}

impl Drop for ActiveCallGuard<'_> {
    fn drop(&mut self) {
        self.counter.fetch_sub(1, Ordering::AcqRel);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HewWsRecvResult {
    Message,
    Cancelled,
    Error,
}

#[derive(Debug)]
enum HewWsAcceptResult {
    Accepted(Box<WebSocket<MaybeTlsStream<TcpStream>>>),
    Cancelled,
    Error,
}

/// Message received from a WebSocket connection.
///
/// Must be freed with [`hew_ws_message_free`].
#[repr(C)]
#[derive(Debug)]
pub struct HewWsMessage {
    /// Message type: 0 = text, 1 = binary, 2 = ping, 3 = pong, 4 = close, −1 = error.
    pub msg_type: i32,
    /// Payload data allocated with `malloc`. Caller frees via [`hew_ws_message_free`].
    pub data: *mut u8,
    /// Length of `data` in bytes.
    pub data_len: usize,
}

const ACTOR_REF_LOCAL: c_int = 0;
const READER_READ_TIMEOUT: Duration = Duration::from_millis(250);
const READER_JOIN_WAIT: Duration = Duration::from_millis(500);
const READER_WAIT_POLL: Duration = Duration::from_millis(10);
const SERVER_HANDSHAKE_TIMEOUT: Duration = Duration::from_secs(2);
const WEBSOCKET_MAX_MESSAGE_SIZE_ENV: &str = "HEW_WS_MAX_MESSAGE_SIZE";
const WEBSOCKET_MAX_FRAME_SIZE_ENV: &str = "HEW_WS_MAX_FRAME_SIZE";
/// Conservative inbound message cap for all Hew WebSocket handshakes.
const WEBSOCKET_MAX_MESSAGE_SIZE_BYTES: usize = 8 << 20;
/// Conservative inbound frame cap for all Hew WebSocket handshakes.
const WEBSOCKET_MAX_FRAME_SIZE_BYTES: usize = 1 << 20;

/// Build the tungstenite config used at every Hew WebSocket attach site.
///
/// Hew applies stricter inbound caps than tungstenite's defaults so a single
/// peer cannot force large frame or message allocations during follow-on reads.
fn websocket_config(max_message_size: usize, max_frame_size: usize) -> WebSocketConfig {
    debug_assert!(max_frame_size <= max_message_size);
    WebSocketConfig::default()
        .max_message_size(Some(max_message_size))
        .max_frame_size(Some(max_frame_size))
}

fn parse_websocket_cap_from_env(env_name: &str) -> Result<Option<usize>, String> {
    match std::env::var(env_name) {
        Ok(raw) => raw
            .parse::<usize>()
            .map(Some)
            .map_err(|err| format!("{env_name} must be a usize byte count: {err}")),
        Err(std::env::VarError::NotPresent) => Ok(None),
        Err(std::env::VarError::NotUnicode(_)) => {
            Err(format!("{env_name} must contain valid UTF-8"))
        }
    }
}

fn websocket_config_from_env() -> Result<WebSocketConfig, String> {
    let max_message_size = parse_websocket_cap_from_env(WEBSOCKET_MAX_MESSAGE_SIZE_ENV)?
        .unwrap_or(WEBSOCKET_MAX_MESSAGE_SIZE_BYTES);
    let max_frame_size = parse_websocket_cap_from_env(WEBSOCKET_MAX_FRAME_SIZE_ENV)?
        .unwrap_or(WEBSOCKET_MAX_FRAME_SIZE_BYTES);
    if max_frame_size > max_message_size {
        return Err(format!(
            "{WEBSOCKET_MAX_FRAME_SIZE_ENV} ({max_frame_size}) must be less than or equal to \
{WEBSOCKET_MAX_MESSAGE_SIZE_ENV} ({max_message_size})"
        ));
    }
    Ok(websocket_config(max_message_size, max_frame_size))
}

#[repr(C)]
#[derive(Clone, Copy)]
struct HewLocation {
    node: [u8; 16],
    slot: u64,
    incarnation: u32,
    reserved: u32,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct HewActorRefRemote {
    // This struct is read by value from a runtime-produced `HewActorRef` and
    // passed back across the FFI boundary, so its layout MUST stay
    // byte-identical to the runtime definition.
    location: HewLocation,
    conn: c_int,
    transport: *mut c_void,
}

#[repr(C)]
union HewActorRefData {
    local: *mut c_void,
    remote: HewActorRefRemote,
}

#[repr(C)]
struct HewActorRef {
    kind: c_int,
    data: HewActorRefData,
}

// SAFETY: the snapshot is never exposed directly to the reader. It is held
// behind `ActorDelivery`'s mutex, and every runtime call that can dereference
// the local actor pointer holds that mutex. Revocation takes the snapshot
// while holding the same mutex, so once `revoke` returns no reader can begin
// or remain inside an actor runtime call.
unsafe impl Send for HewActorRef {}

struct ActorDelivery {
    actor_ref: Mutex<Option<HewActorRef>>,
}

impl std::fmt::Debug for ActorDelivery {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ActorDelivery")
            .field(
                "active",
                &lock_or_recover(&self.actor_ref).as_ref().is_some(),
            )
            .finish()
    }
}

impl ActorDelivery {
    fn new_local(actor: *mut c_void) -> Self {
        Self {
            actor_ref: Mutex::new(Some(HewActorRef {
                kind: ACTOR_REF_LOCAL,
                data: HewActorRefData { local: actor },
            })),
        }
    }

    /// Revoke the reader's only authority to inspect or message the actor.
    ///
    /// Every actor runtime call holds this same mutex, so returning from this
    /// method is the synchronization point after which the raw local actor
    /// pointer is inaccessible to the reader even if socket teardown or thread
    /// scheduling takes longer than expected.
    fn revoke(&self) {
        lock_or_recover(&self.actor_ref).take();
    }

    fn is_alive(&self) -> bool {
        let actor_ref = lock_or_recover(&self.actor_ref);
        actor_ref.as_ref().is_some_and(actor_ref_is_alive)
    }

    fn send(&self, msg_type: i32, data: *mut c_void, size: usize) -> Option<Result<(), i32>> {
        let actor_ref = lock_or_recover(&self.actor_ref);
        actor_ref
            .as_ref()
            .map(|actor_ref| actor_send(actor_ref, msg_type, data, size))
    }

    fn send_if_alive(
        &self,
        msg_type: i32,
        data: *mut c_void,
        size: usize,
    ) -> Option<Result<(), i32>> {
        let actor_ref = lock_or_recover(&self.actor_ref);
        let actor_ref = actor_ref.as_ref()?;
        actor_ref_is_alive(actor_ref).then(|| actor_send(actor_ref, msg_type, data, size))
    }

    #[cfg(test)]
    fn is_revoked(&self) -> bool {
        lock_or_recover(&self.actor_ref).is_none()
    }
}

impl HewWsConn {
    fn new(ws: WebSocket<MaybeTlsStream<TcpStream>>, role: Role) -> Self {
        let shutdown_stream = clone_shutdown_stream(&ws);
        let (ws, write_ws, write_operation_gate) = prepare_websockets(ws, role);
        let write_ws = write_ws.map(|w| PlMutex::new(Some(w)));
        Self {
            inner: Arc::new(HewWsConnInner {
                ws: PlMutex::new(Some(ws)),
                write_ws,
                write_operation_gate,
                shutdown_stream,
                reader: Mutex::new(None),
                closed: AtomicBool::new(false),
                active_recvs: AtomicUsize::new(0),
            }),
        }
    }

    fn close_handle(&self) {
        let first_close = !self.inner.closed.swap(true, Ordering::AcqRel);
        // Revocation is the actor-lifetime boundary. It waits for any
        // in-flight nonblocking runtime call and removes the reader's only raw
        // actor capability before close can proceed to bounded I/O teardown.
        signal_reader_cancel(&self.inner);
        if first_close {
            send_close_frame(&self.inner);
        }
        shutdown_socket(self.inner.shutdown_stream.as_ref(), Shutdown::Both);

        let reader_exited = wait_for_reader_exit_flag(&self.inner, READER_JOIN_WAIT);
        let recv_exited =
            wait_for_active_calls_to_drain(&self.inner.active_recvs, READER_JOIN_WAIT);

        if reader_exited {
            join_reader(&self.inner);
        }

        if reader_exited && recv_exited {
            drop_ws(&self.inner);
            return;
        }

        if self
            .inner
            .reader
            .lock()
            .expect("reader mutex poisoned")
            .is_none()
            && self.inner.active_recvs.load(Ordering::Acquire) == 0
        {
            drop_ws(&self.inner);
        }
    }
}

impl Drop for HewWsConn {
    fn drop(&mut self) {
        self.close_handle();
        #[cfg(test)]
        OUTER_CONN_DROPS.fetch_add(1, Ordering::Relaxed);
    }
}

#[allow(
    unexpected_cfgs,
    reason = "Matching tungstenite TLS variants depends on dependency feature cfgs"
)]
fn clone_shutdown_stream(ws: &WebSocket<MaybeTlsStream<TcpStream>>) -> Option<TcpStream> {
    match ws.get_ref() {
        MaybeTlsStream::Plain(stream) => stream.try_clone().ok(),
        #[cfg(feature = "native-tls")]
        MaybeTlsStream::NativeTls(stream) => stream.get_ref().try_clone().ok(),
        #[cfg(feature = "__rustls-tls")]
        MaybeTlsStream::Rustls(stream) => stream.sock.try_clone().ok(),
        #[allow(
            unreachable_patterns,
            reason = "MaybeTlsStream is non-exhaustive and TLS variants depend on tungstenite features"
        )]
        _ => None,
    }
}

/// Convert tungstenite's handshake stream into Hew's post-handshake stream(s).
///
/// Plain TCP uses Strategy A from #1632: both the read-side framer and the
/// write-side framer share one mutex-protected TCP write half. This preserves
/// #1324's attached-mode latency fix while closing the byte-interleave race
/// between user sends, explicit Pongs, and tungstenite 0.29's auto-Pong flush
/// on `read()`. If cloning fails, Hew falls back to a single plain stream: safe
/// (one framer mutex) but with the pre-#1324 attached-send stall.
fn prepare_websockets(ws: WebSocket<MaybeTlsStream<TcpStream>>, role: Role) -> PreparedWebsockets {
    let config = *ws.get_config();
    match ws.into_inner() {
        MaybeTlsStream::Plain(stream) => split_plain_websockets(stream, role, config)
            .unwrap_or_else(|stream| {
                (
                    WebSocket::from_raw_socket(HewWsStream::Plain(stream), role, Some(config)),
                    None,
                    None,
                )
            }),
        stream => (
            WebSocket::from_raw_socket(HewWsStream::Tls(stream), role, Some(config)),
            None,
            None,
        ),
    }
}

/// Attempt to create independent read/write WebSocket framers over plain TCP
/// with a single serialized write half.
///
/// Only succeeds for plain (non-TLS) connections. TLS connections cannot be split
/// because the TLS record layer is stateful and not safe to share across two
/// independently-created contexts writing to the same underlying file descriptor.
///
/// WHY: Fixes the reader-framer mutex stall in attached mode (issue #1324)
///      while preserving frame integrity (issue #1632). With a split write
///      WebSocket, sends acquire an independent framer mutex; the separate
///      `write_operation_gate` is only for complete-operation serialization
///      against read-side auto-Pong flushes.
/// #1632 note: the two framers must not write through dup-cloned fds without
/// operation-granularity serialization. tungstenite 0.29 queues auto-Pongs
/// during `read()` and flushes them through the read-side stream on a later
/// read entry, so routing only Hew's explicit Pong through `write_ws` would
/// leave the race open. `write_operation_gate` is held across complete
/// tungstenite `send` and `read` operations so short socket writes cannot
/// interleave frames from the sibling framer.
///
/// WHEN: Extend to TLS once a thread-safe TLS write-half abstraction is available.
/// WHAT: TLS split would require sharing a single `TlsStream` write half under a
///       mutex between both the read-context and the write-context.
fn split_plain_websockets(
    stream: TcpStream,
    role: Role,
    config: WebSocketConfig,
) -> Result<PreparedWebsockets, TcpStream> {
    let Ok(write_stream) = stream.try_clone() else {
        return Err(stream);
    };
    let Ok(write_read_stream) = stream.try_clone() else {
        return Err(stream);
    };

    let shared_write = Arc::new(PlMutex::new(write_stream));
    let write_operation_gate = Arc::new(PlMutex::new(()));
    let read_ws = WebSocket::from_raw_socket(
        HewWsStream::SplitPlain(SharedPlainWsStream {
            read_stream: stream,
            write_stream: Arc::clone(&shared_write),
        }),
        role,
        Some(config),
    );
    let write_ws = WebSocket::from_raw_socket(
        HewWsStream::SplitPlain(SharedPlainWsStream {
            read_stream: write_read_stream,
            write_stream: shared_write,
        }),
        role,
        Some(config),
    );
    Ok((read_ws, Some(write_ws), Some(write_operation_gate)))
}

#[allow(
    unexpected_cfgs,
    reason = "Matching tungstenite TLS variants depends on dependency feature cfgs"
)]
fn with_tcp_stream<R>(
    ws: &mut HewWs,
    f: impl FnOnce(&mut TcpStream) -> io::Result<R>,
) -> io::Result<R> {
    match ws.get_mut() {
        HewWsStream::Plain(stream) => f(stream),
        HewWsStream::SplitPlain(stream) => f(&mut stream.read_stream),
        HewWsStream::Tls(stream) => match stream {
            MaybeTlsStream::Plain(stream) => f(stream),
            #[cfg(feature = "native-tls")]
            MaybeTlsStream::NativeTls(stream) => f(stream.get_mut()),
            #[cfg(feature = "__rustls-tls")]
            MaybeTlsStream::Rustls(stream) => f(&mut stream.sock),
            #[allow(
                unreachable_patterns,
                reason = "MaybeTlsStream is non-exhaustive and TLS variants depend on tungstenite features"
            )]
            _ => Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "unsupported websocket stream kind",
            )),
        },
    }
}

fn set_read_timeout(ws: &mut HewWs, timeout: Option<Duration>) -> io::Result<()> {
    with_tcp_stream(ws, |stream| stream.set_read_timeout(timeout))
}

fn shutdown_socket(stream: Option<&TcpStream>, how: Shutdown) {
    if let Some(stream) = stream {
        let _ = stream.shutdown(how);
    }
}

fn lock_or_recover<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(PoisonError::into_inner)
}

/// `parking_lot::Mutex` does not poison; the lock cannot fail. This wrapper
/// matches the call shape of `lock_or_recover` for readability across the
/// two lock kinds in this crate.
fn pl_lock<T>(mutex: &PlMutex<T>) -> parking_lot::MutexGuard<'_, T> {
    mutex.lock()
}

fn with_write_operation_gate<R>(gate: Option<&WriteOperationGate>, op: impl FnOnce() -> R) -> R {
    if let Some(gate) = gate {
        let _operation_guard = pl_lock(gate);
        op()
    } else {
        op()
    }
}

fn read_ws_with_operation_gate(
    inner: &HewWsConnInner,
    ws: &mut HewWs,
) -> Result<Message, tungstenite::Error> {
    with_write_operation_gate(inner.write_operation_gate.as_ref(), || ws.read())
}

fn send_ws_with_operation_gate(
    inner: &HewWsConnInner,
    ws: &mut HewWs,
    message: Message,
) -> Result<(), tungstenite::Error> {
    with_write_operation_gate(inner.write_operation_gate.as_ref(), || ws.send(message))
}

/// Best-effort WebSocket close handshake before the transport is shut down.
///
/// A raw socket shutdown first makes tungstenite's subsequent `close(None)`
/// incapable of writing the protocol close frame (Windows reports a reset to
/// the peer particularly reliably). Attached plain connections use the
/// independent write framer; TLS and unattached connections use the primary
/// framer. Either path is serialized with all other frame writes.
fn send_close_frame(inner: &Arc<HewWsConnInner>) {
    if let Some(write_ws_mutex) = inner.write_ws.as_ref() {
        let mut guard = pl_lock(write_ws_mutex);
        if let Some(ws) = guard.as_mut() {
            let _ =
                with_write_operation_gate(inner.write_operation_gate.as_ref(), || ws.close(None));
        }
        return;
    }
    let mut guard = pl_lock(&inner.ws);
    if let Some(ws) = guard.as_mut() {
        let _ = with_write_operation_gate(inner.write_operation_gate.as_ref(), || ws.close(None));
    }
}

fn signal_reader_cancel(inner: &Arc<HewWsConnInner>) {
    if let Some(reader) = lock_or_recover(&inner.reader).as_ref() {
        reader.cancel.store(true, Ordering::Release);
        reader.delivery.revoke();
    }
}

fn wait_for_reader_exit_flag(inner: &Arc<HewWsConnInner>, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    loop {
        let exited = {
            let reader = lock_or_recover(&inner.reader);
            reader
                .as_ref()
                .is_none_or(|reader| reader.exited.load(Ordering::Acquire))
        };
        if exited {
            return true;
        }
        if Instant::now() >= deadline {
            return false;
        }
        std::thread::sleep(READER_WAIT_POLL);
    }
}

fn wait_for_active_calls_to_drain(counter: &AtomicUsize, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    loop {
        if counter.load(Ordering::Acquire) == 0 {
            return true;
        }
        if Instant::now() >= deadline {
            return false;
        }
        std::thread::sleep(READER_WAIT_POLL);
    }
}

fn join_reader(inner: &Arc<HewWsConnInner>) {
    let join = inner
        .reader
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .as_mut()
        .and_then(|reader| reader.join.take());
    if let Some(join) = join {
        let _ = join.join();
    }
}

fn drop_ws(inner: &Arc<HewWsConnInner>) {
    // Drop the write-side WebSocket first; it holds only a clone of the TCP fd.
    if let Some(write_ws_mutex) = inner.write_ws.as_ref() {
        drop(pl_lock(write_ws_mutex).take());
    }
    let mut ws = pl_lock(&inner.ws);
    let Some(ws) = ws.take() else {
        return;
    };
    drop(ws);
}

fn actor_ref_is_alive(actor_ref: &HewActorRef) -> bool {
    #[cfg(test)]
    ACTOR_RUNTIME_CALLS.fetch_add(1, Ordering::Relaxed);
    // SAFETY: `actor_ref` is held by the delivery lock for this entire call.
    unsafe { hew_actor_ref_is_alive(actor_ref) != 0 }
}

fn actor_ref_local_actor(actor_ref: &HewActorRef) -> Option<*mut c_void> {
    if actor_ref.kind != ACTOR_REF_LOCAL {
        return None;
    }
    // SAFETY: local variant is active when kind == ACTOR_REF_LOCAL.
    let actor = unsafe { actor_ref.data.local };
    (!actor.is_null()).then_some(actor)
}

fn actor_send(
    actor_ref: &HewActorRef,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> Result<(), i32> {
    let Some(actor) = actor_ref_local_actor(actor_ref) else {
        eprintln!("[attach-reader] remote ActorRef is unsupported for websocket attach");
        return Err(-1);
    };
    #[cfg(test)]
    ACTOR_RUNTIME_CALLS.fetch_add(1, Ordering::Relaxed);
    // SAFETY: `actor` comes from the local variant held by the delivery lock.
    let rc = unsafe { hew_actor_try_send(actor, msg_type, data, size) };
    if rc == 0 {
        Ok(())
    } else {
        Err(rc)
    }
}

fn reader_should_exit(inner: &Arc<HewWsConnInner>, delivery: &ActorDelivery) -> bool {
    if inner.closed.load(Ordering::Acquire) {
        return true;
    }
    let cancelled = inner
        .reader
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .as_ref()
        .is_some_and(|reader| reader.cancel.load(Ordering::Acquire));
    cancelled || !delivery.is_alive()
}

fn reader_cleanup(
    inner: &Arc<HewWsConnInner>,
    delivery: &ActorDelivery,
    on_close_type: i32,
    notify_close: bool,
) {
    if notify_close {
        if let Some(Err(rc)) = delivery.send_if_alive(on_close_type, std::ptr::null_mut(), 0) {
            eprintln!("[attach-reader] close delivery failed: rc={rc}; exiting");
        }
    }
    signal_reader_cancel(inner);
    shutdown_socket(inner.shutdown_stream.as_ref(), Shutdown::Both);
    drop_ws(inner);
    if let Some(reader) = lock_or_recover(&inner.reader).as_ref() {
        reader.exited.store(true, Ordering::Release);
    }
}

fn is_timeout_error(err: &tungstenite::Error) -> bool {
    matches!(
        err,
        tungstenite::Error::Io(io_err)
            if io_err.kind() == io::ErrorKind::TimedOut
                || io_err.kind() == io::ErrorKind::WouldBlock
    )
}

fn spawn_attach_reader(
    conn: &HewWsConn,
    actor: *mut c_void,
    on_message_type: i32,
    on_close_type: i32,
    ws_ptr: *mut HewWsConn,
) -> Result<(), String> {
    // Hold this guard through validation, spawn, and store. Two concurrent
    // callers must not both observe an empty slot and start competing readers.
    let mut reader = lock_or_recover(&conn.inner.reader);
    if reader.is_some() {
        return Err("websocket.attach: reader already attached".to_owned());
    }
    {
        let mut guard = pl_lock(&conn.inner.ws);
        let Some(ws) = guard.as_mut() else {
            return Err("websocket.attach: connection already closed".to_owned());
        };
        if let Err(err) = set_read_timeout(ws, Some(READER_READ_TIMEOUT)) {
            return Err(format!(
                "websocket.attach: failed to set read timeout for {ws_ptr:p}: {err}"
            ));
        }
    }

    // Hew `LocalPid<T>` crosses an extern C call as the bare local actor
    // pointer. The delivery authority is the only owner of that pointer and
    // can be synchronously revoked even if the reader itself has not exited.
    let delivery = Arc::new(ActorDelivery::new_local(actor));
    let cancel = Arc::new(AtomicBool::new(false));
    let exited = Arc::new(AtomicBool::new(false));
    let inner = Arc::clone(&conn.inner);
    let reader_cancel = Arc::clone(&cancel);
    let reader_exited = Arc::clone(&exited);
    let reader_delivery = Arc::clone(&delivery);
    let join = std::thread::spawn(move || {
        let mut notify_close = false;
        loop {
            if reader_should_exit(&inner, &reader_delivery) {
                break;
            }

            let read_result = {
                let mut guard = pl_lock(&inner.ws);
                let Some(ws) = guard.as_mut() else {
                    break;
                };
                read_ws_with_operation_gate(&inner, ws)
            };

            match read_result {
                Ok(tungstenite::Message::Text(text)) => {
                    if reader_should_exit(&inner, &reader_delivery) {
                        break;
                    }
                    let bytes = text.as_bytes();
                    // Header-aware (S1): passed to the callback / dropped via hew_string_drop.
                    // SAFETY: bytes is valid for bytes.len(); alloc_cstring copies it.
                    let str_ptr = unsafe { alloc_cstring(bytes.as_ptr(), bytes.len()) }; // CSTRING-ALLOC: str-open (reader str_ptr: header-aware Hew string passed to callback)
                    if str_ptr.is_null() {
                        break;
                    }
                    let mut arg_buf = [0u8; std::mem::size_of::<usize>()];
                    arg_buf.copy_from_slice(&(str_ptr as usize).to_ne_bytes());
                    let send_result = reader_delivery.send(
                        on_message_type,
                        arg_buf.as_mut_ptr().cast(),
                        arg_buf.len(),
                    );
                    match send_result {
                        Some(Ok(())) => {}
                        Some(Err(rc)) => {
                            eprintln!("[attach-reader] message delivery failed: rc={rc}; exiting");
                            // SAFETY: send failed before the actor took ownership of the string.
                            unsafe { free_cstring(str_ptr) }; // CSTRING-FREE: str-open (frees reader str_ptr on send-fail)
                            break;
                        }
                        None => {
                            // SAFETY: revocation happened before the actor took ownership.
                            unsafe { free_cstring(str_ptr) }; // CSTRING-FREE: str-open (frees reader str_ptr on revoke)
                            break;
                        }
                    }
                }
                Ok(tungstenite::Message::Ping(payload)) => {
                    if !inner.closed.load(Ordering::Acquire) {
                        // tungstenite also queues an auto-Pong on `read()`. We
                        // still send an explicit Pong here intentionally: it
                        // gives the peer a prompt payload echo instead of
                        // waiting for the next read-cycle flush, and it routes
                        // through `send_ws_message` so the serialized plain-TCP
                        // write half covers it. Duplicate Pong frames are
                        // protocol-valid and preferable to delayed liveness.
                        let _ = send_ws_message(&inner, tungstenite::Message::Pong(payload));
                    }
                }
                Ok(tungstenite::Message::Close(_)) => {
                    notify_close = true;
                    break;
                }
                Ok(_) => {}
                Err(err) => {
                    if !is_timeout_error(&err) {
                        eprintln!("[attach-reader] read failed: {err}; exiting");
                        notify_close = true;
                        break;
                    }
                }
            }
        }

        reader_cleanup(&inner, &reader_delivery, on_close_type, notify_close);
        reader_delivery.revoke();
        reader_cancel.store(true, Ordering::Release);
        reader_exited.store(true, Ordering::Release);
    });

    *reader = Some(ReaderControl {
        cancel,
        exited,
        delivery,
        join: Some(join),
    });
    Ok(())
}

/// Build a heap-allocated [`HewWsMessage`] from a type tag and byte slice.
///
/// For empty payloads, `data` will be a non-null 1-byte sentinel (canonical
/// `malloc_bytes` empty behaviour); callers check `data_len` to distinguish
/// empty from non-empty content.
fn build_message(msg_type: i32, payload: &[u8]) -> *mut HewWsMessage {
    let data = malloc_bytes(payload);
    Box::into_raw(Box::new(HewWsMessage {
        msg_type,
        data,
        data_len: payload.len(),
    }))
}

fn build_recv_message(msg: Message) -> *mut HewWsMessage {
    match msg {
        Message::Text(text) => build_message(0, text.as_bytes()),
        Message::Binary(bytes) => build_message(1, &bytes),
        Message::Ping(bytes) => build_message(2, &bytes),
        Message::Pong(bytes) => build_message(3, &bytes),
        Message::Close(_) => build_message(4, &[]),
        Message::Frame(_) => build_message(1, &[]),
    }
}

fn recv_message(inner: &Arc<HewWsConnInner>) -> (HewWsRecvResult, *mut HewWsMessage) {
    loop {
        if inner.closed.load(Ordering::Acquire) {
            return (HewWsRecvResult::Cancelled, std::ptr::null_mut());
        }

        let read_result = {
            let mut guard = pl_lock(&inner.ws);
            let Some(ws) = guard.as_mut() else {
                let kind = if inner.closed.load(Ordering::Acquire) {
                    HewWsRecvResult::Cancelled
                } else {
                    HewWsRecvResult::Error
                };
                return (kind, std::ptr::null_mut());
            };
            if let Err(err) = set_read_timeout(ws, Some(READER_READ_TIMEOUT)) {
                eprintln!("[recv] failed to set read timeout: {err}");
                return (HewWsRecvResult::Error, std::ptr::null_mut());
            }
            read_ws_with_operation_gate(inner, ws)
        };

        match read_result {
            Ok(msg) => return (HewWsRecvResult::Message, build_recv_message(msg)),
            Err(err) if is_timeout_error(&err) => {}
            Err(_) if inner.closed.load(Ordering::Acquire) => {
                return (HewWsRecvResult::Cancelled, std::ptr::null_mut());
            }
            Err(_) => return (HewWsRecvResult::Error, std::ptr::null_mut()),
        }
    }
}

/// Connect to a WebSocket server.
///
/// Supports both `ws://` and `wss://` URLs. Returns a heap-allocated
/// [`HewWsConn`] on success, or null on error.
///
/// # Safety
///
/// `url` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_connect(url: *const c_char) -> *mut HewWsConn {
    if url.is_null() {
        set_ws_last_error(-1, "websocket.connect: url is null".to_owned());
        return std::ptr::null_mut();
    }
    // SAFETY: `url` is a valid NUL-terminated C string per caller contract.
    let Ok(url_str) = unsafe { CStr::from_ptr(url) }.to_str() else {
        set_ws_last_error(-1, "websocket.connect: url is not valid UTF-8".to_owned());
        return std::ptr::null_mut();
    };

    let config = match websocket_config_from_env() {
        Ok(config) => config,
        Err(err) => {
            set_ws_last_error(-1, format!("websocket.connect: invalid config: {err}"));
            return std::ptr::null_mut();
        }
    };

    match connect_preserving_errno(url_str, config, 3) {
        Ok(ws) => {
            clear_ws_last_error();
            Box::into_raw(Box::new(HewWsConn::new(ws, Role::Client)))
        }
        Err(err) => {
            set_ws_last_error(ws_errno_of(&err), format!("websocket.connect: {err}"));
            std::ptr::null_mut()
        }
    }
}

/// Connect once per resolved address while retaining the final socket errno.
///
/// `tungstenite::connect_with_config` discards every `TcpStream::connect`
/// error and returns only `UrlError::UnableToConnect`. Reconnecting merely to
/// recover an errno creates a TOCTOU race and can establish an unwanted second
/// transport. This mirrors tungstenite's blocking client flow, but carries the
/// actual error from the one authoritative connect attempt into Hew's typed
/// error channel.
fn connect_preserving_errno(
    url_str: &str,
    config: WebSocketConfig,
    max_redirects: u8,
) -> Result<WebSocket<MaybeTlsStream<TcpStream>>, tungstenite::Error> {
    let mut current = url_str.to_owned();

    for attempt in 0..=max_redirects {
        let request = current.as_str().into_client_request()?;
        let uri = request.uri();
        let mode = uri_mode(uri)?;
        if matches!(mode, Mode::Tls) {
            // hew-std currently builds tungstenite without a TLS connector,
            // matching connect_with_config's existing fail-closed behaviour.
            return Err(tungstenite::Error::Url(
                tungstenite::error::UrlError::TlsFeatureNotEnabled,
            ));
        }
        let host = uri
            .host()
            .ok_or(tungstenite::Error::Url(
                tungstenite::error::UrlError::NoHostName,
            ))?
            .trim_start_matches('[')
            .trim_end_matches(']');
        let port = uri.port_u16().unwrap_or(80);

        let mut last_connect_error = None;
        let mut connected = None;
        for addr in (host, port).to_socket_addrs()? {
            match TcpStream::connect(addr) {
                Ok(stream) => {
                    connected = Some(stream);
                    break;
                }
                Err(err) => last_connect_error = Some(err),
            }
        }
        let Some(stream) = connected else {
            return Err(match last_connect_error {
                Some(err) => tungstenite::Error::Io(err),
                None => {
                    tungstenite::Error::Url(tungstenite::error::UrlError::UnableToConnect(current))
                }
            });
        };
        stream.set_nodelay(true)?;

        let handshake = client_with_config(request, MaybeTlsStream::Plain(stream), Some(config))
            .map_err(|err| match err {
                tungstenite::HandshakeError::Failure(failure) => failure,
                tungstenite::HandshakeError::Interrupted(_) => {
                    panic!("blocking WebSocket handshake unexpectedly interrupted")
                }
            });

        match handshake {
            Ok((ws, _response)) => return Ok(ws),
            Err(tungstenite::Error::Http(response))
                if response.status().is_redirection() && attempt < max_redirects =>
            {
                if let Some(location) = response.headers().get("Location") {
                    location.to_str()?.clone_into(&mut current);
                    continue;
                }
                return Err(tungstenite::Error::Http(response));
            }
            Err(err) => return Err(err),
        }
    }

    unreachable!("WebSocket redirect loop always returns or continues")
}

/// Recover the OS errno a tungstenite error carries, or 0 when it is a
/// protocol/handshake failure with no syscall behind it.
fn ws_errno_of(err: &tungstenite::Error) -> i64 {
    match err {
        tungstenite::Error::Io(io_err) => ws_io_errno(io_err),
        tungstenite::Error::Url(tungstenite::error::UrlError::UnableToConnect(_)) => 0,
        tungstenite::Error::Url(_) | tungstenite::Error::HttpFormat(_) => -1,
        _ => 0,
    }
}

/// Classify a WebSocket I/O failure without inventing a platform errno.
///
/// A raw errno remains authoritative. `InvalidInput` and `InvalidData` are
/// pre-syscall grammar failures and use the module's `-1` `InvalidArgument`
/// sentinel; other errors without a raw errno remain unclassified as zero.
fn ws_io_errno(err: &io::Error) -> i64 {
    if let Some(errno) = err.raw_os_error() {
        return i64::from(errno);
    }
    if matches!(
        err.kind(),
        io::ErrorKind::InvalidInput | io::ErrorKind::InvalidData
    ) {
        return -1;
    }
    0
}

/// Route an outbound WebSocket message through the write-side socket when available.
///
/// In attached mode (where the reader holds `ws` for up to 250 ms), `write_ws`
/// is an independent WebSocket framer for plain TCP. Both framers share one
/// TCP write half plus an operation-level serialization gate, so short socket
/// writes cannot let bytes from user sends, explicit Pongs, or tungstenite
/// auto-Pong flushes interleave. For TLS connections `write_ws` is absent and
/// sends fall back to the shared `ws` mutex (the 250 ms stall persists for TLS;
/// see `split_plain_websockets` for details).
fn send_ws_message(
    inner: &Arc<HewWsConnInner>,
    message: Message,
) -> Result<(), tungstenite::Error> {
    if let Some(write_ws_mutex) = inner.write_ws.as_ref() {
        let mut guard = pl_lock(write_ws_mutex);
        let Some(ws) = guard.as_mut() else {
            return Err(tungstenite::Error::AlreadyClosed);
        };
        return send_ws_with_operation_gate(inner, ws, message);
    }
    let mut guard = pl_lock(&inner.ws);
    let Some(ws) = guard.as_mut() else {
        return Err(tungstenite::Error::AlreadyClosed);
    };
    send_ws_with_operation_gate(inner, ws, message)
}

/// Send a text message over a WebSocket connection.
///
/// Returns 0 on success, −1 on error.
///
/// # Safety
///
/// * `ws` must be a valid pointer returned by [`hew_ws_connect`].
/// * `msg` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_send_text(ws: *mut HewWsConn, msg: *const c_char) -> i32 {
    if ws.is_null() || msg.is_null() {
        return -1;
    }
    // SAFETY: `ws` is a valid HewWsConn pointer per caller contract.
    let inner = Arc::clone(&unsafe { &*ws }.inner);
    // SAFETY: `msg` is a valid NUL-terminated C string per caller contract.
    let Ok(text) = unsafe { CStr::from_ptr(msg) }.to_str() else {
        return -1;
    };

    if inner.closed.load(Ordering::Acquire) {
        return -1;
    }
    match send_ws_message(&inner, Message::text(text)) {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

/// Send a binary message over a WebSocket connection.
///
/// Returns 0 on success, −1 on error.
///
/// # Safety
///
/// * `ws` must be a valid pointer returned by [`hew_ws_connect`].
/// * `data` must point to at least `len` readable bytes, or be null if `len` is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_send_binary(
    ws: *mut HewWsConn,
    data: *const u8,
    len: usize,
) -> i32 {
    if ws.is_null() {
        return -1;
    }
    // SAFETY: `ws` is a valid HewWsConn pointer per caller contract.
    let inner = Arc::clone(&unsafe { &*ws }.inner);
    if inner.closed.load(Ordering::Acquire) {
        return -1;
    }

    let slice = if len == 0 {
        &[]
    } else {
        if data.is_null() {
            return -1;
        }
        // SAFETY: `data` is valid for `len` bytes per caller contract.
        unsafe { std::slice::from_raw_parts(data, len) }
    };

    match send_ws_message(&inner, Message::binary(slice.to_vec())) {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

/// Receive the next message from a WebSocket connection (blocking).
///
/// Returns a heap-allocated [`HewWsMessage`], or null on error.
/// The caller must free the result with [`hew_ws_message_free`].
///
/// # Safety
///
/// `ws` must be a valid pointer returned by [`hew_ws_connect`].
#[no_mangle]
pub unsafe extern "C" fn hew_ws_recv(ws: *mut HewWsConn) -> *mut HewWsMessage {
    if ws.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `ws` is a valid HewWsConn pointer per caller contract.
    let inner = Arc::clone(&unsafe { &*ws }.inner);
    if inner.closed.load(Ordering::Acquire) {
        return std::ptr::null_mut();
    }

    let _recv_guard = ActiveCallGuard::new(&inner.active_recvs);
    match recv_message(&inner) {
        (HewWsRecvResult::Message, msg) => msg,
        (HewWsRecvResult::Cancelled | HewWsRecvResult::Error, _) => std::ptr::null_mut(),
    }
}

// Status sentinel for hew_ws_recv_timeout: communicated via a thread-local
// because the FFI returns a single pointer. Cleared at every call.
std::thread_local! {
    static LAST_WS_RECV_TIMED_OUT: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

fn set_ws_last_error(errno: i64, detail: String) {
    hew_runtime::parse_error_slot::set_error_with_errno(
        hew_runtime::parse_error_slot::ErrorSlotKind::Websocket,
        errno,
        detail,
    );
}

fn clear_ws_last_error() {
    hew_runtime::parse_error_slot::clear_error(
        hew_runtime::parse_error_slot::ErrorSlotKind::Websocket,
    );
}

/// Return the errno of this actor's most recent failed
/// `hew_ws_connect` or `hew_ws_server_new`, or 0 when the last call succeeded.
#[no_mangle]
pub extern "C" fn hew_ws_last_errno() -> i64 {
    hew_runtime::parse_error_slot::get_errno(
        hew_runtime::parse_error_slot::ErrorSlotKind::Websocket,
    )
}

/// Return the detail of this actor's most recent failed
/// `hew_ws_connect` or `hew_ws_server_new`, or the empty string when the last
/// call succeeded.
#[no_mangle]
pub extern "C" fn hew_ws_last_error() -> *mut c_char {
    let detail = hew_runtime::parse_error_slot::get_error(
        hew_runtime::parse_error_slot::ErrorSlotKind::Websocket,
    )
    .unwrap_or_default();
    hew_cabi::cabi::str_to_malloc(&detail)
}

/// Report whether `ws` is backed by a live WebSocket connection.
///
/// # Safety
///
/// `ws` must be null or a pointer previously returned by [`hew_ws_connect`]
/// or `hew_ws_server_accept`.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_conn_is_valid(ws: *const HewWsConn) -> bool {
    !ws.is_null()
}

/// Report whether `server` is backed by a bound listener.
///
/// # Safety
///
/// `server` must be null or a pointer previously returned by
/// [`hew_ws_server_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_ws_server_is_valid(server: *const HewWsServer) -> bool {
    !server.is_null()
}

fn set_last_ws_recv_timed_out(v: bool) {
    LAST_WS_RECV_TIMED_OUT.with(|c| c.set(v));
}

/// Receive the next message from a WebSocket connection with a per-call
/// deadline. Returns a heap-allocated [`HewWsMessage`] on success, or null
/// on deadline expiry, error, cancellation, or null input.
///
/// Callers query [`hew_ws_recv_last_timed_out`] to distinguish timeout
/// from other null returns. The sentinel is cleared at the start of every
/// call to this function.
///
/// `deadline_ms <= 0` disables the deadline (matches `hew_ws_recv`).
///
/// # Concurrency
///
/// The reader thread (when attached) holds `inner.ws` for up to
/// `READER_READ_TIMEOUT` (250 ms) per cycle. This call uses
/// `parking_lot::Mutex::try_lock_for` so the worst-case wait for the
/// lock is bounded by the user's deadline. After acquiring the lock, the
/// residual deadline is applied to the underlying TCP read via
/// `set_read_timeout` so the inner `ws.read()` call cannot exceed the
/// deadline either.
///
/// Cleanup-all-exits: the `parking_lot::MutexGuard` is dropped via RAII
/// before every return path; explicit `drop(guard)` annotations make
/// release order visible.
///
/// # Safety
///
/// `ws` must be a valid pointer returned by [`hew_ws_connect`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_recv_timeout(
    ws: *mut HewWsConn,
    deadline_ms: i32,
) -> *mut HewWsMessage {
    set_last_ws_recv_timed_out(false);
    if ws.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `ws` is a valid HewWsConn pointer per caller contract.
    let inner = Arc::clone(&unsafe { &*ws }.inner);
    if inner.closed.load(Ordering::Acquire) {
        return std::ptr::null_mut();
    }

    let _recv_guard = ActiveCallGuard::new(&inner.active_recvs);

    if deadline_ms <= 0 {
        // No deadline — match the existing recv path exactly.
        return match recv_message(&inner) {
            (HewWsRecvResult::Message, msg) => msg,
            (HewWsRecvResult::Cancelled | HewWsRecvResult::Error, _) => std::ptr::null_mut(),
        };
    }

    #[expect(clippy::cast_sign_loss, reason = "deadline_ms > 0 checked above")]
    let total = Duration::from_millis(deadline_ms as u64);
    let start = Instant::now();

    // Acquire `inner.ws` with a timed try_lock. If the reader thread is
    // mid-cycle the wait is bounded by `total`.
    let Some(mut guard) = inner.ws.try_lock_for(total) else {
        set_last_ws_recv_timed_out(true);
        return std::ptr::null_mut();
    };

    let Some(ws_ref) = guard.as_mut() else {
        // Connection closed under us. Cleanup-all-exits: guard drops on return.
        return std::ptr::null_mut();
    };

    let remaining = total.saturating_sub(start.elapsed());
    if remaining.is_zero() {
        // Lock acquire consumed the entire budget. Cleanup-all-exits:
        // explicit drop before return makes the release order visible.
        drop(guard);
        set_last_ws_recv_timed_out(true);
        return std::ptr::null_mut();
    }

    if let Err(err) = set_read_timeout(ws_ref, Some(remaining)) {
        eprintln!("[recv_timeout] failed to set read timeout: {err}");
        drop(guard);
        return std::ptr::null_mut();
    }

    let read_result = read_ws_with_operation_gate(&inner, ws_ref);
    // Restore the timeout to the reader's pacing value so subsequent
    // attached-reader cycles continue with their normal cadence.
    let _ = set_read_timeout(ws_ref, Some(READER_READ_TIMEOUT));

    match read_result {
        Ok(msg) => {
            // Cleanup-all-exits: guard drops at end of scope.
            drop(guard);
            build_recv_message(msg)
        }
        Err(err) if is_timeout_error(&err) => {
            drop(guard);
            set_last_ws_recv_timed_out(true);
            std::ptr::null_mut()
        }
        Err(_) => {
            drop(guard);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
/// Returns 1 if the most recent `hew_ws_recv_timeout` on this thread
/// observed a deadline expiry; 0 otherwise. Cleared at the start of
/// every `hew_ws_recv_timeout` call.
pub extern "C" fn hew_ws_recv_last_timed_out() -> i32 {
    LAST_WS_RECV_TIMED_OUT.with(|c| i32::from(c.get()))
}

#[no_mangle]
/// Returns true if `msg` is a non-null pointer (a successful recv result).
/// Used by Hew callers to distinguish a valid message from a null return
/// (timeout or error) without raw-pointer arithmetic.
///
/// # Safety
///
/// `msg` must be null or a valid pointer returned by [`hew_ws_recv`] /
/// [`hew_ws_recv_timeout`]. Pointer reads are not performed; only the
/// null check.
pub unsafe extern "C" fn hew_ws_message_is_valid(msg: *const HewWsMessage) -> bool {
    !msg.is_null()
}

/// Close a WebSocket connection and free its resources.
///
/// # Safety
///
/// `ws` must be a valid pointer returned by [`hew_ws_connect`], and must not
/// have been closed already. Passing null is a no-op.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_close(ws: *mut HewWsConn) {
    if ws.is_null() {
        return;
    }
    // SAFETY: `ws` was allocated with `Box::into_raw` in `hew_ws_connect` or
    // `hew_ws_server_accept`. `Drop for HewWsConn` calls `close_handle`, which
    // synchronously revokes actor delivery, then signals and joins the reader
    // when it exits within the bounded wait. A delayed reader retains only the
    // Arc-owned transport state and cannot touch the actor.
    // This path is correct for both attached and unattached connections: the
    // previous code omitted `Box::from_raw` on the attached branch, leaking the
    // outer `HewWsConn` struct (~16 bytes) on every attached close. Fixes #1324.
    drop(unsafe { Box::from_raw(ws) });
}

/// Get the message type tag from a [`HewWsMessage`].
///
/// Returns 0=text, 1=binary, 2=ping, 3=pong, 4=close, -1=error/null.
///
/// # Safety
///
/// `msg` must be a valid pointer returned by [`hew_ws_recv`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_message_type(msg: *const HewWsMessage) -> i32 {
    if msg.is_null() {
        return -1;
    }
    // SAFETY: Caller guarantees `msg` is a valid pointer returned by hew_ws_recv.
    (unsafe { &*msg }).msg_type
}

/// Extract the text content from a [`HewWsMessage`] as a NUL-terminated C string.
///
/// Returns a `malloc`-allocated string the caller must free, or null if the
/// message is null or has no data.
///
/// # Safety
///
/// `msg` must be a valid pointer returned by [`hew_ws_recv`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_message_text(msg: *const HewWsMessage) -> *mut c_char {
    if msg.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees `msg` is a valid pointer returned by hew_ws_recv.
    let m = unsafe { &*msg };
    if m.data.is_null() || m.data_len == 0 {
        return std::ptr::null_mut();
    }
    // Header-aware (S1): returned to Hew, dropped via hew_string_drop / free_cstring.
    // SAFETY: m.data is valid for m.data_len bytes; alloc_cstring copies it.
    let ptr = unsafe { alloc_cstring(m.data, m.data_len) }; // CSTRING-ALLOC: str-open (hew_ws_message_text — header-aware Hew string; dropped via hew_string_drop)
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    ptr
}

/// Free a [`HewWsMessage`] previously returned by [`hew_ws_recv`].
///
/// # Safety
///
/// `msg` must be a pointer previously returned by [`hew_ws_recv`], and must
/// not have been freed already. Passing null is a no-op.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_message_free(msg: *mut HewWsMessage) {
    if msg.is_null() {
        return;
    }
    // SAFETY: `msg` was allocated with Box::into_raw in build_message.
    let message = unsafe { Box::from_raw(msg) };
    if !message.data.is_null() {
        // SAFETY: `data` was allocated with libc::malloc in malloc_bytes.
        unsafe { libc::free(message.data.cast()) }; // CSTRING-FREE: libc-bytes (message.data = malloc_bytes payload)
    }
    // Box is dropped here, freeing the HewWsMessage struct.
}

// ── WebSocket Attach (Erlang-style active mode) ────────────────────
//
// `hew_ws_attach` transfers read authority to a background OS thread that
// delivers frames as actor messages. The caller retains the outer connection
// owner and its send/close authority. The actor never calls recv() — it just
// has receive fns that the runtime invokes. This is Erlang's "active mode"
// pattern.

/// Attach a WebSocket connection to an actor. Spawns a reader thread
/// that delivers frames as actor messages.
///
/// - `ws`: the WebSocket connection. After attach, `recv()` must not be used,
///   but outbound `send_text()` remains valid until the connection is closed.
/// - `actor`: pointer to the target actor
/// - `on_message_type`: `msg_type` index for text frame delivery
/// - `on_close_type`: `msg_type` index for close/error notification
///
/// The reader thread forwards each text frame to the actor and delivers one
/// close notification when the connection closes or errors.
///
/// # Safety
///
/// - `ws` must be a valid pointer returned by `hew_ws_connect` or
///   `hew_ws_server_accept`.
/// - `actor` must be a valid actor pointer that outlives the connection.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_attach(
    ws: *mut HewWsConn,
    actor: *mut std::ffi::c_void,
    on_message_type: i64,
    on_close_type: i64,
) -> i32 {
    if ws.is_null() || actor.is_null() {
        set_ws_last_error(
            -1,
            format!(
                "websocket.attach: null handle (ws={}, actor={})",
                ws.is_null(),
                actor.is_null()
            ),
        );
        return -1;
    }
    // The indices cross as `i64` and are range-checked here: narrowing them on
    // the Hew side would turn `2^32 + 1` into index `1` and attach the reader
    // to a different message than the caller named.
    let (Ok(on_message_type), Ok(on_close_type)) =
        (i32::try_from(on_message_type), i32::try_from(on_close_type))
    else {
        set_ws_last_error(
            -1,
            format!(
                "websocket.attach: message-type index out of range \
                 (on_message_type={on_message_type}, on_close_type={on_close_type})"
            ),
        );
        eprintln!(
            "[attach] message-type index out of range: on_message_type={on_message_type} \
             on_close_type={on_close_type}"
        );
        return -1;
    };
    // SAFETY: nulls are rejected above and `ws` remains valid for this call.
    let conn = unsafe { &*ws };
    match spawn_attach_reader(conn, actor, on_message_type, on_close_type, ws) {
        Ok(()) => {
            clear_ws_last_error();
            0
        }
        Err(detail) => {
            set_ws_last_error(-1, detail);
            -1
        }
    }
}

// Import the actor send function from the runtime.
extern "C" {
    fn hew_actor_try_send(
        actor: *mut std::ffi::c_void,
        msg_type: i32,
        data: *mut std::ffi::c_void,
        size: usize,
    ) -> i32;
    fn hew_actor_ref_is_alive(actor: *const HewActorRef) -> i32;
}

// ── WebSocket Server ────────────────────────────────────────────────

/// Opaque WebSocket server handle.
///
/// Wraps a [`TcpListener`] that accepts incoming connections and upgrades
/// them to WebSocket via tungstenite. Must be closed with [`hew_ws_server_close`].
#[derive(Debug)]
pub struct HewWsServer {
    inner: Arc<HewWsServerInner>,
}

#[derive(Debug)]
struct HewWsServerInner {
    listener: TcpListener,
    cancel: AtomicBool,
    active_accepts: ActiveAcceptState,
    active_handshakes: AtomicUsize,
}

#[derive(Debug, Default)]
struct ActiveAcceptState {
    count: Mutex<usize>,
    drained: Condvar,
}

#[derive(Debug)]
struct ActiveAcceptGuard<'a> {
    state: &'a ActiveAcceptState,
}

impl ActiveAcceptState {
    fn begin<'a>(&'a self, cancel: &AtomicBool) -> Option<ActiveAcceptGuard<'a>> {
        let mut count = lock_or_recover(&self.count);
        if cancel.load(Ordering::Acquire) {
            return None;
        }
        *count += 1;
        Some(ActiveAcceptGuard { state: self })
    }

    fn cancel_and_wait(&self, cancel: &AtomicBool) {
        let mut count = lock_or_recover(&self.count);
        cancel.store(true, Ordering::Release);
        while *count != 0 {
            count = self
                .drained
                .wait(count)
                .unwrap_or_else(PoisonError::into_inner);
        }
    }

    #[cfg(test)]
    fn count(&self) -> usize {
        *lock_or_recover(&self.count)
    }

    fn publish_if_open<T>(&self, cancel: &AtomicBool, publish: impl FnOnce() -> T) -> Option<T> {
        let _count = lock_or_recover(&self.count);
        if cancel.load(Ordering::Acquire) {
            None
        } else {
            Some(publish())
        }
    }
}

impl Drop for ActiveAcceptGuard<'_> {
    fn drop(&mut self) {
        let mut count = lock_or_recover(&self.state.count);
        assert!(*count > 0, "active websocket accept count underflow");
        *count -= 1;
        if *count == 0 {
            self.state.drained.notify_all();
        }
    }
}

impl HewWsServer {
    fn new(listener: TcpListener) -> io::Result<Self> {
        listener.set_nonblocking(true)?;
        Ok(Self {
            inner: Arc::new(HewWsServerInner {
                listener,
                cancel: AtomicBool::new(false),
                active_accepts: ActiveAcceptState::default(),
                active_handshakes: AtomicUsize::new(0),
            }),
        })
    }

    fn close_handle(&self) {
        self.inner
            .active_accepts
            .cancel_and_wait(&self.inner.cancel);
    }
}

impl Drop for HewWsServer {
    fn drop(&mut self) {
        self.close_handle();
    }
}

fn accept_connection(inner: &Arc<HewWsServerInner>) -> HewWsAcceptResult {
    loop {
        if inner.cancel.load(Ordering::Acquire) {
            return HewWsAcceptResult::Cancelled;
        }

        match inner.listener.accept() {
            Ok((stream, _addr)) => {
                if inner.cancel.load(Ordering::Acquire) {
                    return HewWsAcceptResult::Cancelled;
                }
                if let Err(err) = stream.set_nonblocking(true) {
                    eprintln!("[accept] failed to prepare cancellable handshake: {err}");
                    return HewWsAcceptResult::Error;
                }
                let tls_stream = MaybeTlsStream::Plain(stream);
                let config = match websocket_config_from_env() {
                    Ok(config) => config,
                    Err(err) => {
                        eprintln!("[accept] invalid websocket config: {err}");
                        return HewWsAcceptResult::Error;
                    }
                };
                match accept_cancellable_handshake(inner, tls_stream, config) {
                    HewWsAcceptResult::Accepted(ws) => {
                        return HewWsAcceptResult::Accepted(ws);
                    }
                    HewWsAcceptResult::Cancelled => return HewWsAcceptResult::Cancelled,
                    HewWsAcceptResult::Error => {
                        // A malformed or expired handshake rejects only that
                        // accepted socket; the server remains available for
                        // the next peer.
                    }
                }
            }
            Err(err) if err.kind() == io::ErrorKind::WouldBlock => {
                std::thread::sleep(READER_WAIT_POLL);
            }
            Err(err) => {
                eprintln!("[accept] listener accept failed: {err}");
                return if inner.cancel.load(Ordering::Acquire) {
                    HewWsAcceptResult::Cancelled
                } else {
                    HewWsAcceptResult::Error
                };
            }
        }
    }
}

fn accept_cancellable_handshake(
    inner: &Arc<HewWsServerInner>,
    stream: MaybeTlsStream<TcpStream>,
    config: WebSocketConfig,
) -> HewWsAcceptResult {
    use tungstenite::handshake::HandshakeError;

    let _handshake_guard = ActiveCallGuard::new(&inner.active_handshakes);
    let deadline = Instant::now() + SERVER_HANDSHAKE_TIMEOUT;
    let mut handshake = match tungstenite::accept_with_config(stream, Some(config)) {
        Ok(mut ws) => {
            if inner.cancel.load(Ordering::Acquire) {
                return HewWsAcceptResult::Cancelled;
            }
            if let Err(err) = set_server_websocket_blocking(&mut ws) {
                eprintln!("[accept] failed to restore blocking mode: {err}");
                return HewWsAcceptResult::Error;
            }
            return HewWsAcceptResult::Accepted(Box::new(ws));
        }
        Err(HandshakeError::Interrupted(handshake)) => handshake,
        Err(HandshakeError::Failure(err)) => {
            eprintln!("[accept] websocket handshake failed: {err}");
            return HewWsAcceptResult::Error;
        }
    };

    loop {
        if inner.cancel.load(Ordering::Acquire) {
            return HewWsAcceptResult::Cancelled;
        }
        if Instant::now() >= deadline {
            eprintln!("[accept] websocket handshake exceeded {SERVER_HANDSHAKE_TIMEOUT:?}");
            return HewWsAcceptResult::Error;
        }
        std::thread::sleep(READER_WAIT_POLL);
        match handshake.handshake() {
            Ok(mut ws) => {
                if inner.cancel.load(Ordering::Acquire) {
                    return HewWsAcceptResult::Cancelled;
                }
                if let Err(err) = set_server_websocket_blocking(&mut ws) {
                    eprintln!("[accept] failed to restore blocking mode: {err}");
                    return HewWsAcceptResult::Error;
                }
                if inner.cancel.load(Ordering::Acquire) {
                    return HewWsAcceptResult::Cancelled;
                }
                return HewWsAcceptResult::Accepted(Box::new(ws));
            }
            Err(HandshakeError::Interrupted(next)) => handshake = next,
            Err(HandshakeError::Failure(err)) => {
                eprintln!("[accept] websocket handshake failed: {err}");
                return HewWsAcceptResult::Error;
            }
        }
    }
}

#[allow(
    unexpected_cfgs,
    reason = "Matching tungstenite TLS variants depends on dependency feature cfgs"
)]
fn set_server_websocket_blocking(ws: &mut WebSocket<MaybeTlsStream<TcpStream>>) -> io::Result<()> {
    match ws.get_mut() {
        MaybeTlsStream::Plain(stream) => stream.set_nonblocking(false),
        #[cfg(feature = "native-tls")]
        MaybeTlsStream::NativeTls(stream) => stream.get_mut().set_nonblocking(false),
        #[cfg(feature = "__rustls-tls")]
        MaybeTlsStream::Rustls(stream) => stream.sock.set_nonblocking(false),
        #[allow(
            unreachable_patterns,
            reason = "MaybeTlsStream is non-exhaustive and TLS variants depend on tungstenite features"
        )]
        _ => Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "unsupported websocket server stream kind",
        )),
    }
}

/// Create a WebSocket server listening on the given address (e.g. `"0.0.0.0:8080"`).
///
/// Returns a heap-allocated [`HewWsServer`] on success, or null on error.
///
/// # Safety
///
/// `addr` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_server_new(addr: *const c_char) -> *mut HewWsServer {
    if addr.is_null() {
        set_ws_last_error(-1, "websocket.listen: address is null".to_owned());
        return std::ptr::null_mut();
    }
    // SAFETY: `addr` is a valid NUL-terminated C string per caller contract.
    let Ok(addr_str) = (unsafe { CStr::from_ptr(addr) }).to_str() else {
        set_ws_last_error(
            -1,
            "websocket.listen: address is not valid UTF-8".to_owned(),
        );
        return std::ptr::null_mut();
    };
    let bind_addr = normalize_bind_addr(addr_str);
    match TcpListener::bind(bind_addr.as_ref()) {
        Ok(listener) => match HewWsServer::new(listener) {
            Ok(server) => {
                clear_ws_last_error();
                Box::into_raw(Box::new(server))
            }
            Err(err) => {
                set_ws_last_error(
                    ws_io_errno(&err),
                    format!("websocket.listen: cannot prepare listener on `{addr_str}`: {err}"),
                );
                std::ptr::null_mut()
            }
        },
        Err(err) => {
            set_ws_last_error(
                ws_io_errno(&err),
                format!("websocket.listen: cannot bind `{addr_str}`"),
            );
            std::ptr::null_mut()
        }
    }
}

/// Get the port the server is listening on.
///
/// Returns -1 if `server` is null or the address cannot be determined.
///
/// # Safety
///
/// `server` must be a valid pointer returned by [`hew_ws_server_new`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_server_port(server: *const HewWsServer) -> i32 {
    if server.is_null() {
        return -1;
    }
    // SAFETY: Caller guarantees `server` is a valid pointer returned by hew_ws_server_new.
    match (unsafe { &*server }).inner.listener.local_addr() {
        Ok(addr) => i32::from(addr.port()),
        Err(_) => -1,
    }
}

/// Accept one WebSocket connection. Blocks until a client connects and
/// completes the WebSocket handshake.
///
/// Returns a [`HewWsConn`] (same type as client connections) on success,
/// or null on error. The returned connection works with [`hew_ws_send_text`],
/// [`hew_ws_recv`], and [`hew_ws_close`].
///
/// # Safety
///
/// `server` must be a valid pointer returned by [`hew_ws_server_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_ws_server_accept(server: *mut HewWsServer) -> *mut HewWsConn {
    if server.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees `server` is a valid pointer returned by hew_ws_server_new.
    let inner = Arc::clone(&unsafe { &*server }.inner);
    let Some(_accept_guard) = inner.active_accepts.begin(&inner.cancel) else {
        return std::ptr::null_mut();
    };
    match accept_connection(&inner) {
        HewWsAcceptResult::Accepted(ws) => inner
            .active_accepts
            .publish_if_open(&inner.cancel, || {
                Box::into_raw(Box::new(HewWsConn::new(*ws, Role::Server)))
            })
            .unwrap_or(std::ptr::null_mut()),
        HewWsAcceptResult::Cancelled | HewWsAcceptResult::Error => std::ptr::null_mut(),
    }
}

/// Close the server and stop listening.
///
/// # Safety
///
/// `server` must be a valid pointer returned by [`hew_ws_server_new`],
/// or null (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_ws_server_close(server: *mut HewWsServer) {
    if !server.is_null() {
        // SAFETY: `server` was allocated with Box::into_raw in hew_ws_server_new.
        drop(unsafe { Box::from_raw(server) });
    }
}

#[cfg(test)]
mod tests {
    #![allow(
        clippy::undocumented_unsafe_blocks,
        reason = "Test helpers call runtime and websocket FFI entrypoints directly"
    )]

    use super::*;
    use crate::net_error_slot_test_support::NetErrorSlotRuntimeGuard;
    use hew_runtime::{actor, transport};
    use std::collections::HashMap;
    #[cfg(unix)]
    use std::os::fd::AsRawFd;
    use std::sync::atomic::AtomicU64;
    use std::sync::mpsc::{self, Receiver, RecvTimeoutError, Sender};
    use std::sync::Barrier;
    use std::sync::OnceLock;

    const TEST_MSG_TYPE: i32 = 11;
    const TEST_CLOSE_TYPE: i32 = 12;
    const TEST_STOP_TYPE: i32 = 101;
    const TEST_CRASH_TYPE: i32 = 102;

    fn ws_last_error_text() -> String {
        let ptr = hew_ws_last_error();
        assert!(!ptr.is_null());
        // SAFETY: accessor returns a valid header-aware C string.
        let text = unsafe { CStr::from_ptr(ptr) }
            .to_string_lossy()
            .into_owned();
        // SAFETY: pointer came from hew_ws_last_error.
        unsafe { hew_cabi::cabi::free_cstring(ptr) };
        text
    }

    #[test]
    fn websocket_error_and_errno_follow_actor_across_worker_threads() {
        use crate::net_error_slot_test_support::{spawn_error_slot_test_actor, with_actor_context};

        let _runtime = NetErrorSlotRuntimeGuard::new();
        for _ in 0..3 {
            let actor = spawn_error_slot_test_actor();
            assert!(!actor.is_null());
            let actor_addr = actor as usize;
            let barrier = Arc::new(Barrier::new(2));
            let worker_barrier = Arc::clone(&barrier);
            let worker = std::thread::spawn(move || {
                worker_barrier.wait();
                let actor = actor_addr as *mut hew_runtime::actor::HewActor;
                with_actor_context(actor, || (hew_ws_last_errno(), ws_last_error_text()))
            });

            with_actor_context(actor, || {
                // SAFETY: null is the documented invalid-URL path.
                let conn = unsafe { hew_ws_connect(std::ptr::null()) };
                assert!(conn.is_null());
            });
            barrier.wait();
            assert_eq!(
                worker.join().expect("worker should read actor error"),
                (-1, "websocket.connect: url is null".to_owned())
            );

            with_actor_context(actor, || {
                assert_eq!(hew_ws_last_errno(), -1);
                assert_eq!(ws_last_error_text(), "websocket.connect: url is null");
            });

            // SAFETY: actor is live and owned by this test.
            unsafe { actor::hew_actor_stop(actor) };
            // SAFETY: actor was stopped immediately above and is freed once.
            assert_eq!(unsafe { actor::hew_actor_free(actor) }, 0);
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum ActorEvent {
        Message(String),
        Closed,
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    struct TestActorState {
        test_id: u64,
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    struct CancelOwnerState {
        server: usize,
        conn: usize,
    }

    #[derive(Clone)]
    struct ShortWriteLog {
        bytes: Arc<Mutex<Vec<u8>>>,
        max_per_write: usize,
    }

    impl Write for ShortWriteLog {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            let n = self.max_per_write.min(buf.len());
            self.bytes
                .lock()
                .expect("short-write log poisoned")
                .extend_from_slice(&buf[..n]);
            Ok(n)
        }

        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    static NEXT_TEST_ID: AtomicU64 = AtomicU64::new(1);
    static ACTOR_EVENTS: OnceLock<Mutex<HashMap<u64, Sender<ActorEvent>>>> = OnceLock::new();

    fn actor_events() -> &'static Mutex<HashMap<u64, Sender<ActorEvent>>> {
        ACTOR_EVENTS.get_or_init(|| Mutex::new(HashMap::new()))
    }

    fn register_actor_events() -> (u64, Receiver<ActorEvent>) {
        let test_id = NEXT_TEST_ID.fetch_add(1, Ordering::Relaxed);
        let (tx, rx) = mpsc::channel();
        actor_events()
            .lock()
            .expect("actor event registry poisoned")
            .insert(test_id, tx);
        (test_id, rx)
    }

    fn unregister_actor_events(test_id: u64) {
        actor_events()
            .lock()
            .expect("actor event registry poisoned")
            .remove(&test_id);
    }

    fn send_actor_event(test_id: u64, event: ActorEvent) {
        if let Some(sender) = actor_events()
            .lock()
            .expect("actor event registry poisoned")
            .get(&test_id)
            .cloned()
        {
            let _ = sender.send(event);
        }
    }

    unsafe extern "C-unwind" fn websocket_test_dispatch(
        _ctx: *mut hew_runtime::HewExecutionContext,
        state: *mut c_void,
        msg_type: i32,
        data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        // SAFETY: test actor state is a POD snapshot allocated by `hew_actor_spawn`.
        let state = unsafe { &*(state.cast::<TestActorState>()) };
        match msg_type {
            TEST_MSG_TYPE => {
                // SAFETY: websocket attach packs a pointer-sized value containing the
                // malloc-allocated NUL-terminated string.
                let str_ptr = unsafe { *(data.cast::<usize>()) as *mut c_char };
                let text = if str_ptr.is_null() {
                    String::new()
                } else {
                    // SAFETY: attach allocated a NUL-terminated C string for this payload.
                    let text = unsafe { CStr::from_ptr(str_ptr) }
                        .to_str()
                        .expect("websocket payload must be valid utf-8")
                        .to_owned();
                    // SAFETY: ownership transfers to the actor handler on successful send.
                    unsafe { free_cstring(str_ptr) }; // CSTRING-FREE: str-open (frees str_ptr after readback)
                    text
                };
                send_actor_event(state.test_id, ActorEvent::Message(text));
            }
            TEST_CLOSE_TYPE => send_actor_event(state.test_id, ActorEvent::Closed),
            TEST_STOP_TYPE => actor::hew_actor_self_stop(),
            TEST_CRASH_TYPE => panic!("intentional websocket test actor crash"),
            _ => {}
        }

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn websocket_cancel_owner_dispatch(
        _ctx: *mut hew_runtime::HewExecutionContext,
        state: *mut c_void,
        msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        if msg_type != TEST_STOP_TYPE {
            return std::ptr::null_mut();
        }

        // SAFETY: test actor state is a POD snapshot allocated by `hew_actor_spawn`.
        let state = unsafe { &*(state.cast::<CancelOwnerState>()) };
        if state.conn != 0 {
            unsafe { hew_ws_close(state.conn as *mut HewWsConn) };
        }
        if state.server != 0 {
            unsafe { hew_ws_server_close(state.server as *mut HewWsServer) };
        }
        actor::hew_actor_self_stop();

        std::ptr::null_mut()
    }

    fn run_in_isolated_test_process_with_env(
        test_name: &str,
        env_key: &str,
        extra_env: &[(&str, &str)],
        body: impl FnOnce(),
    ) {
        if std::env::var_os(env_key).is_some() {
            body();
            return;
        }

        let mut command = std::process::Command::new(
            std::env::current_exe().expect("resolve current test binary"),
        );
        command
            .arg(test_name)
            .arg("--exact")
            .arg("--nocapture")
            .arg("--test-threads=1")
            .env(env_key, "1");
        for (key, value) in extra_env {
            command.env(key, value);
        }
        let output = command.output().expect("spawn isolated test process");

        assert!(
            output.status.success(),
            "isolated test process failed for {test_name} (status: {:?})\nstdout:\n{}\nstderr:\n{}",
            output.status.code(),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );
    }

    fn run_in_isolated_test_process(test_name: &str, env_key: &str, body: impl FnOnce()) {
        run_in_isolated_test_process_with_env(test_name, env_key, &[], body);
    }

    fn wait_for_condition(timeout: Duration, mut condition: impl FnMut() -> bool) -> bool {
        let deadline = Instant::now() + timeout;
        loop {
            if condition() {
                return true;
            }
            if Instant::now() >= deadline {
                return false;
            }
            std::thread::sleep(Duration::from_millis(10));
        }
    }

    fn wait_for_reader_exit(inner: &Arc<HewWsConnInner>, timeout: Duration) -> bool {
        wait_for_condition(timeout, || {
            inner
                .reader
                .lock()
                .expect("reader mutex poisoned")
                .as_ref()
                .is_some_and(|reader| reader.exited.load(Ordering::Acquire))
        })
    }

    fn attached_delivery(inner: &Arc<HewWsConnInner>) -> Arc<ActorDelivery> {
        Arc::clone(
            &inner
                .reader
                .lock()
                .expect("reader mutex poisoned")
                .as_ref()
                .expect("reader should be attached")
                .delivery,
        )
    }

    fn wait_for_actor_dead(actor: *mut actor::HewActor, timeout: Duration) -> bool {
        // SAFETY: tests call this only for actors they spawned and still own.
        let actor_ref = unsafe { transport::hew_actor_ref_local(actor) };
        wait_for_condition(timeout, || unsafe {
            transport::hew_actor_ref_is_alive(&raw const actor_ref) == 0
        })
    }

    fn wait_for_thread_exit<T>(handle: &std::thread::JoinHandle<T>, timeout: Duration) -> bool {
        wait_for_condition(timeout, || handle.is_finished())
    }

    fn recv_event(rx: &Receiver<ActorEvent>, timeout: Duration) -> ActorEvent {
        rx.recv_timeout(timeout)
            .unwrap_or_else(|err| panic!("expected actor event within {timeout:?}: {err:?}"))
    }

    fn assert_no_event(rx: &Receiver<ActorEvent>, timeout: Duration) {
        match rx.recv_timeout(timeout) {
            Err(RecvTimeoutError::Timeout) => {}
            other => panic!("expected no event within {timeout:?}, got {other:?}"),
        }
    }

    fn attach_test_conn() -> (
        *mut HewWsServer,
        *mut HewWsConn,
        tungstenite::WebSocket<MaybeTlsStream<TcpStream>>,
    ) {
        // SAFETY: valid C string literal for bind address.
        let server = unsafe { hew_ws_server_new(c"127.0.0.1:0".as_ptr()) };
        assert!(!server.is_null(), "server should bind successfully");
        // SAFETY: `server` is valid.
        let port = unsafe { hew_ws_server_port(server) };
        assert!(port > 0, "server must report a port");
        let server_addr = server as usize;
        let accept_thread = std::thread::spawn(move || {
            // SAFETY: `server` remains live until the main thread joins this acceptor.
            unsafe { hew_ws_server_accept(server_addr as *mut HewWsServer) as usize }
        });
        let (client, _) = connect_with_config(
            format!("ws://127.0.0.1:{port}"),
            Some(websocket_config(
                WEBSOCKET_MAX_MESSAGE_SIZE_BYTES,
                WEBSOCKET_MAX_FRAME_SIZE_BYTES,
            )),
            3,
        )
        .expect("client connect");
        let conn = accept_thread.join().expect("accept thread should finish") as *mut HewWsConn;
        assert!(!conn.is_null(), "accept should return a connection");
        (server, conn, client)
    }

    #[allow(
        unexpected_cfgs,
        reason = "Matching tungstenite TLS variants depends on dependency feature cfgs"
    )]
    fn set_peer_read_timeout(
        ws: &mut tungstenite::WebSocket<MaybeTlsStream<TcpStream>>,
        timeout: Duration,
    ) {
        match ws.get_mut() {
            MaybeTlsStream::Plain(stream) => stream
                .set_read_timeout(Some(timeout))
                .expect("set peer read timeout"),
            #[cfg(feature = "native-tls")]
            MaybeTlsStream::NativeTls(stream) => stream
                .get_mut()
                .set_read_timeout(Some(timeout))
                .expect("set peer read timeout"),
            #[cfg(feature = "__rustls-tls")]
            MaybeTlsStream::Rustls(stream) => stream
                .sock
                .set_read_timeout(Some(timeout))
                .expect("set peer read timeout"),
            #[allow(
                unreachable_patterns,
                reason = "MaybeTlsStream is non-exhaustive and TLS variants depend on tungstenite features"
            )]
            _ => panic!("unsupported peer stream kind"),
        }
    }

    #[cfg(unix)]
    fn set_conn_send_buffer(conn: *mut HewWsConn, bytes: libc::c_int) {
        let fd = unsafe { &*conn }
            .inner
            .shutdown_stream
            .as_ref()
            .expect("plain test connection should have a shutdown stream")
            .as_raw_fd();
        let value = bytes;
        let opt_len = libc::socklen_t::try_from(std::mem::size_of_val(&value))
            .expect("SO_SNDBUF option length should fit socklen_t");
        let rc = unsafe {
            libc::setsockopt(
                fd,
                libc::SOL_SOCKET,
                libc::SO_SNDBUF,
                (&raw const value).cast(),
                opt_len,
            )
        };
        assert_eq!(rc, 0, "setsockopt(SO_SNDBUF) should succeed");
    }

    fn spawn_attached_actor(
        conn: *mut HewWsConn,
    ) -> (*mut actor::HewActor, u64, Receiver<ActorEvent>) {
        let (test_id, rx) = register_actor_events();
        let state = TestActorState { test_id };
        let actor = unsafe {
            actor::hew_actor_spawn(
                (&raw const state).cast_mut().cast(),
                std::mem::size_of::<TestActorState>(),
                Some(websocket_test_dispatch),
            )
        };
        assert!(!actor.is_null(), "test actor should spawn");
        let attach_status = unsafe {
            hew_ws_attach(
                conn,
                actor.cast(),
                TEST_MSG_TYPE.into(),
                TEST_CLOSE_TYPE.into(),
            )
        };
        assert_eq!(attach_status, 0, "first attach should succeed");
        (actor, test_id, rx)
    }

    fn teardown_attached_actor(
        actor: *mut actor::HewActor,
        test_id: u64,
        conn: *mut HewWsConn,
        server: *mut HewWsServer,
    ) {
        unsafe { hew_ws_close(conn) };
        unsafe { actor::hew_actor_stop(actor) };
        assert_eq!(unsafe { actor::hew_actor_free(actor) }, 0);
        unregister_actor_events(test_id);
        unsafe { hew_ws_server_close(server) };
    }

    #[test]
    fn local_close_writes_protocol_frame_before_transport_shutdown() {
        let (server, conn, mut peer) = attach_test_conn();
        set_peer_read_timeout(&mut peer, Duration::from_secs(1));

        // SAFETY: `conn` is the live server-side handle from `attach_test_conn`.
        unsafe { hew_ws_close(conn) };

        match peer.read() {
            Ok(Message::Close(_)) => {}
            Ok(other) => panic!("expected WebSocket close frame, got {other:?}"),
            Err(err) => panic!("peer observed transport failure instead of close frame: {err}"),
        }

        // SAFETY: `server` remains live and has not been closed.
        unsafe { hew_ws_server_close(server) };
    }

    #[test]
    fn attach_wrapped_message_type_index_is_refused_not_narrowed() {
        // `2^32 + 1` narrows to message-type index 1. Attaching under that
        // index would deliver frames to a different `receive fn` than the
        // caller named, so the attach is refused and no reader is spawned.
        let (server, conn, client) = attach_test_conn();
        let mut actor_slot: usize = 0;
        let actor_ptr = std::ptr::from_mut(&mut actor_slot).cast::<c_void>();
        // SAFETY: `conn` is a live connection and `actor_ptr` is a valid slot.
        let status = unsafe { hew_ws_attach(conn, actor_ptr, 4_294_967_297, 1) };

        assert_eq!(status, -1);
        assert_eq!(hew_ws_last_errno(), -1);
        assert_eq!(
            ws_last_error_text(),
            "websocket.attach: message-type index out of range \
             (on_message_type=4294967297, on_close_type=1)"
        );
        // SAFETY: `conn` is live for the duration of this borrow.
        let attached = { lock_or_recover(&unsafe { &*conn }.inner.reader).is_some() };
        assert!(
            !attached,
            "no reader may be attached under a narrowed message-type index"
        );

        drop(client);
        // SAFETY: `conn` and `server` were produced by `attach_test_conn`.
        unsafe { hew_ws_close(conn) };
        // SAFETY: `server` is live and not yet closed.
        unsafe { hew_ws_server_close(server) };
    }

    #[test]
    fn attach_null_handles_preserve_typed_error_authority() {
        // SAFETY: null handles exercise the guarded error path.
        let status = unsafe { hew_ws_attach(std::ptr::null_mut(), std::ptr::null_mut(), 0, 1) };

        assert_eq!(status, -1);
        assert_eq!(hew_ws_last_errno(), -1);
        assert_eq!(
            ws_last_error_text(),
            "websocket.attach: null handle (ws=true, actor=true)"
        );
        assert_eq!(
            hew_ws_last_errno(),
            -1,
            "reading detail must not consume the typed error authority"
        );
    }

    #[test]
    fn second_attach_is_refused_without_replacing_the_live_reader() {
        run_in_isolated_test_process(
            "second_attach_is_refused_without_replacing_the_live_reader",
            "HEW_WS_DOUBLE_ATTACH_ISOLATED",
            || {
                let _runtime = NetErrorSlotRuntimeGuard::new();
                let (server, conn, client) = attach_test_conn();
                let (actor, test_id, _rx) = spawn_attached_actor(conn);
                // SAFETY: both handles are live, but the connection already
                // owns a reader from `spawn_attached_actor`.
                let status = unsafe {
                    hew_ws_attach(
                        conn,
                        actor.cast(),
                        TEST_MSG_TYPE.into(),
                        TEST_CLOSE_TYPE.into(),
                    )
                };

                assert_eq!(status, -1);
                assert_eq!(hew_ws_last_errno(), -1);
                assert_eq!(
                    ws_last_error_text(),
                    "websocket.attach: reader already attached"
                );
                // SAFETY: `conn` remains live until teardown below.
                assert!(
                    lock_or_recover(&unsafe { &*conn }.inner.reader).is_some(),
                    "the rejected attach must preserve the original live reader"
                );

                drop(client);
                teardown_attached_actor(actor, test_id, conn, server);
            },
        );
    }

    #[test]
    fn connect_returns_null_for_invalid_url() {
        let listener = TcpListener::bind("127.0.0.1:0").expect("claim loopback port");
        let port = listener.local_addr().expect("read loopback port").port();
        drop(listener);
        let url =
            std::ffi::CString::new(format!("ws://127.0.0.1:{port}/")).expect("URL contains no NUL");
        // SAFETY: url is a valid C string.
        let conn = unsafe { hew_ws_connect(url.as_ptr()) };
        assert!(conn.is_null(), "expected null for unreachable address");
        let errno = hew_ws_last_errno();
        assert!(
            matches!(errno, 61 | 111 | 10061),
            "closed loopback port must retain ECONNREFUSED"
        );
        let detail = ws_last_error_text();
        assert!(
            detail.starts_with("websocket.connect: ")
                && detail.contains(&format!("os error {errno}")),
            "the precise OS transport failure must remain observable without \
             assuming a platform-localized message: {detail:?}"
        );
        assert!(
            matches!(hew_ws_last_errno(), 61 | 111 | 10061),
            "reading detail must not consume errno"
        );
    }

    #[test]
    fn connect_returns_null_for_null_url() {
        // SAFETY: Passing null is explicitly handled.
        let conn = unsafe { hew_ws_connect(std::ptr::null()) };
        assert!(conn.is_null());
    }

    #[test]
    fn message_struct_layout() {
        let msg = HewWsMessage {
            msg_type: 0,
            data: std::ptr::null_mut(),
            data_len: 42,
        };
        assert_eq!(msg.msg_type, 0);
        assert!(msg.data.is_null());
        assert_eq!(msg.data_len, 42);

        // Verify C-repr field ordering via pointer offsets.
        let base = &raw const msg as usize;
        let type_offset = &raw const msg.msg_type as usize - base;
        let data_offset = &raw const msg.data as usize - base;
        let len_offset = &raw const msg.data_len as usize - base;
        assert_eq!(type_offset, 0, "msg_type must be at offset 0");
        assert!(data_offset > type_offset, "data must come after msg_type");
        assert!(len_offset > data_offset, "data_len must come after data");
    }

    #[test]
    fn build_message_roundtrip() {
        let payload = b"hello websocket";
        let msg = build_message(0, payload);
        assert!(!msg.is_null());
        // SAFETY: msg was just allocated by build_message.
        let msg_ref = unsafe { &*msg };
        assert_eq!(msg_ref.msg_type, 0);
        assert_eq!(msg_ref.data_len, payload.len());
        // SAFETY: data was allocated with malloc_bytes from payload.
        let data_slice = unsafe { std::slice::from_raw_parts(msg_ref.data, msg_ref.data_len) };
        assert_eq!(data_slice, payload);
        // SAFETY: msg was allocated by build_message.
        unsafe { hew_ws_message_free(msg) };
    }

    #[test]
    fn write_operation_gate_spans_short_write_drain() {
        let gate = Arc::new(PlMutex::new(()));
        let log = Arc::new(Mutex::new(Vec::new()));
        let ready = Arc::new(Barrier::new(2));
        let (first_short_write_tx, first_short_write_rx) = mpsc::channel();

        let user_gate = Arc::clone(&gate);
        let user_ready = Arc::clone(&ready);
        let mut user_writer = ShortWriteLog {
            bytes: Arc::clone(&log),
            max_per_write: 4,
        };
        let user = std::thread::spawn(move || {
            user_ready.wait();
            with_write_operation_gate(Some(&user_gate), || {
                let mut written = 0;
                let data = b"USERFRAME";
                while written < data.len() {
                    let n = user_writer
                        .write(&data[written..])
                        .expect("forced short write should succeed");
                    written += n;
                    if written == n {
                        first_short_write_tx
                            .send(())
                            .expect("signal first short write");
                        // Keep the operation gate held while the competing
                        // Pong writer tries to run. Without operation-level
                        // serialization this would deterministically produce
                        // USERPONGFRAME.
                        std::thread::sleep(Duration::from_millis(50));
                    }
                }
                user_writer.flush().expect("flush user frame");
            });
        });

        let pong_gate = Arc::clone(&gate);
        let pong_ready = Arc::clone(&ready);
        let mut pong_writer = ShortWriteLog {
            bytes: Arc::clone(&log),
            max_per_write: 4,
        };
        let pong = std::thread::spawn(move || {
            pong_ready.wait();
            first_short_write_rx
                .recv_timeout(Duration::from_secs(1))
                .expect("first short user write should happen");
            with_write_operation_gate(Some(&pong_gate), || {
                let mut written = 0;
                let data = b"PONG";
                while written < data.len() {
                    written += pong_writer
                        .write(&data[written..])
                        .expect("forced pong write should succeed");
                }
                pong_writer.flush().expect("flush pong");
            });
        });

        user.join().expect("user writer should finish");
        pong.join().expect("pong writer should finish");
        let bytes = log.lock().expect("short-write log poisoned").clone();
        assert_eq!(
            bytes.as_slice(),
            b"USERFRAMEPONG",
            "operation gate must cover the whole short-write drain, not just one write syscall"
        );
    }

    #[test]
    fn message_free_null_is_noop() {
        // SAFETY: Passing null is explicitly handled.
        unsafe { hew_ws_message_free(std::ptr::null_mut()) };
    }

    /// `hew_ws_send_text` with null ws returns -1.
    #[test]
    fn send_text_null_ws_returns_error() {
        let msg = c"hello";
        assert_eq!(
            // SAFETY: null ws is explicitly handled.
            unsafe { hew_ws_send_text(std::ptr::null_mut(), msg.as_ptr()) },
            -1
        );
    }

    /// `hew_ws_send_text` with null msg returns -1.
    #[test]
    fn send_text_null_msg_returns_error() {
        // We can't create a real ws connection without a server, so test the
        // null-msg guard by passing null for both — ws null check fires first.
        assert_eq!(
            // SAFETY: null pointers are explicitly handled.
            unsafe { hew_ws_send_text(std::ptr::null_mut(), std::ptr::null()) },
            -1
        );
    }

    /// `hew_ws_send_binary` with null ws returns -1.
    #[test]
    fn send_binary_null_ws_returns_error() {
        let data = [1u8, 2, 3];
        assert_eq!(
            // SAFETY: null ws is explicitly handled.
            unsafe { hew_ws_send_binary(std::ptr::null_mut(), data.as_ptr(), data.len()) },
            -1
        );
    }

    /// `hew_ws_recv` with null ws returns null.
    #[test]
    fn recv_null_ws_returns_null() {
        // SAFETY: null ws is explicitly handled.
        assert!(unsafe { hew_ws_recv(std::ptr::null_mut()) }.is_null());
    }

    /// `hew_ws_close` with null ws is a no-op.
    #[test]
    fn close_null_ws_is_noop() {
        // SAFETY: null ws is explicitly handled.
        unsafe { hew_ws_close(std::ptr::null_mut()) };
    }

    /// `hew_ws_recv_timeout` returns null and sets the timeout sentinel
    /// when the peer accepts the WebSocket but never sends anything.
    ///
    /// The 200 ms deadline must fire well within 2 s. The previous reader
    /// architecture used `std::sync::Mutex` which has no timed acquire,
    /// so a stalled peer pinned the calling thread until the read returned.
    #[test]
    fn ws_recv_timeout_fires_on_silent_peer() {
        let (server, conn, _client) = attach_test_conn();
        // Note: `_client` is the peer-side WebSocket. We deliberately do
        // not send anything from it — the server-side recv must time out.

        let start = Instant::now();
        // SAFETY: conn is a valid HewWsConn pointer.
        let msg = unsafe { hew_ws_recv_timeout(conn, 200) };
        let elapsed = start.elapsed();
        let timed_out = hew_ws_recv_last_timed_out();

        assert!(msg.is_null(), "deadline expiry must return null message");
        assert_eq!(timed_out, 1, "timeout sentinel must be set");
        assert!(
            elapsed < Duration::from_secs(2),
            "200 ms deadline must fire well before 2 s; got {elapsed:?}"
        );

        // Cleanup-all-exits regression: a follow-up call with deadline
        // must still succeed quickly (lock was released), proving we
        // didn't leak the parking_lot guard on the timeout exit path.
        let start2 = Instant::now();
        // SAFETY: conn is a valid HewWsConn pointer.
        let msg2 = unsafe { hew_ws_recv_timeout(conn, 100) };
        let elapsed2 = start2.elapsed();
        let timed_out2 = hew_ws_recv_last_timed_out();
        assert!(msg2.is_null());
        assert_eq!(timed_out2, 1);
        assert!(
            elapsed2 < Duration::from_secs(1),
            "second recv_timeout must also be deadline-bounded; got {elapsed2:?}"
        );

        // SAFETY: conn and server are valid; close is idempotent.
        unsafe { hew_ws_close(conn) };
        unsafe { hew_ws_server_close(server) };
    }

    /// `build_message` with empty payload creates a valid message with a non-null
    /// sentinel data pointer (canonical `malloc_bytes` empty behaviour).
    #[test]
    fn build_message_empty_payload() {
        let msg = build_message(4, &[]);
        assert!(!msg.is_null());
        // SAFETY: msg was just allocated by build_message.
        let msg_ref = unsafe { &*msg };
        assert_eq!(msg_ref.msg_type, 4);
        assert_eq!(msg_ref.data_len, 0);
        // malloc_bytes returns a non-null 1-byte sentinel for empty input.
        assert!(
            !msg_ref.data.is_null(),
            "empty payload should have a non-null sentinel"
        );
        // SAFETY: msg was allocated by build_message.
        unsafe { hew_ws_message_free(msg) };
    }

    /// connect with an HTTP URL (not ws://) returns null.
    #[test]
    fn connect_http_url_returns_null() {
        let url = c"http://127.0.0.1:1/path";
        // SAFETY: url is a valid C string.
        let conn = unsafe { hew_ws_connect(url.as_ptr()) };
        assert!(conn.is_null(), "non-WebSocket URL should fail");
        assert_eq!(hew_ws_last_errno(), -1);
        assert!(
            ws_last_error_text().contains("URL error: URL scheme not supported"),
            "unsupported grammar detail must remain observable"
        );
        assert_eq!(hew_ws_last_errno(), -1);
    }

    #[test]
    fn connect_malformed_url_is_invalid_argument_not_other_zero() {
        let url = c"not a url";
        // SAFETY: url is a valid C string.
        let conn = unsafe { hew_ws_connect(url.as_ptr()) };
        assert!(conn.is_null(), "malformed URL should fail");
        assert_eq!(hew_ws_last_errno(), -1);
        assert_eq!(
            ws_last_error_text(),
            "websocket.connect: HTTP format error: invalid uri character"
        );
        assert_eq!(hew_ws_last_errno(), -1);
    }

    /// connect with an empty string returns null.
    #[test]
    fn connect_empty_url_returns_null() {
        let url = c"";
        // SAFETY: url is a valid C string.
        let conn = unsafe { hew_ws_connect(url.as_ptr()) };
        assert!(conn.is_null(), "empty URL should fail");
        assert_eq!(hew_ws_last_errno(), -1);
        assert!(!ws_last_error_text().is_empty());
    }

    // ── Server tests ────────────────────────────────────────────────

    /// Server listens, client connects, exchanges a message, closes.
    #[test]
    fn server_accept_and_echo() {
        // SAFETY: Valid C string literal passed to FFI.
        let server = unsafe { hew_ws_server_new(c"127.0.0.1:0".as_ptr()) };
        assert!(!server.is_null(), "server should bind successfully");

        // SAFETY: server is a valid pointer just returned above.
        let port = unsafe { hew_ws_server_port(server) };
        assert!(port > 0, "port should be positive");

        let addr = format!("ws://127.0.0.1:{port}");
        let client_thread = std::thread::spawn(move || {
            let (mut ws, _) = connect_with_config(
                &addr,
                Some(websocket_config(
                    WEBSOCKET_MAX_MESSAGE_SIZE_BYTES,
                    WEBSOCKET_MAX_FRAME_SIZE_BYTES,
                )),
                3,
            )
            .expect("client connect");
            ws.send(Message::text("hello from client"))
                .expect("client send");
            let reply = ws.read().expect("client read");
            assert_eq!(reply, Message::Text("echo: hello from client".into()));
            ws.close(None).ok();
            // Drain remaining frames so close handshake completes.
            while ws.read().is_ok() {}
        });

        // SAFETY: server is a valid pointer returned by hew_ws_server_new.
        let conn = unsafe { hew_ws_server_accept(server) };
        assert!(!conn.is_null(), "accept should succeed");

        // SAFETY: conn is a valid pointer returned by hew_ws_server_accept.
        let msg = unsafe { hew_ws_recv(conn) };
        assert!(!msg.is_null(), "recv should succeed");
        // SAFETY: msg is non-null, just verified above.
        let msg_ref = unsafe { &*msg };
        assert_eq!(msg_ref.msg_type, 0, "should be a text message");
        // SAFETY: msg_ref.data is valid for msg_ref.data_len bytes (from build_message).
        let text = unsafe {
            std::str::from_utf8(std::slice::from_raw_parts(msg_ref.data, msg_ref.data_len))
                .expect("valid utf8")
        };
        assert_eq!(text, "hello from client");

        // Echo back.
        let echo = std::ffi::CString::new(format!("echo: {text}")).unwrap();
        // SAFETY: conn is valid; echo is a valid CString.
        let rc = unsafe { hew_ws_send_text(conn, echo.as_ptr()) };
        assert_eq!(rc, 0, "send should succeed");

        // SAFETY: msg was returned by hew_ws_recv and has not been freed.
        unsafe { hew_ws_message_free(msg) };
        // SAFETY: conn was returned by hew_ws_server_accept and has not been closed.
        unsafe { hew_ws_close(conn) };
        // SAFETY: server was returned by hew_ws_server_new and has not been closed.
        unsafe { hew_ws_server_close(server) };

        client_thread.join().expect("client thread should finish");
    }

    #[test]
    fn accept_with_small_frame_cap_rejects_oversized_frame() {
        const TEST_FRAME_CAP_BYTES: usize = 64 * 1024;
        const OVERSIZED_FRAME_BYTES: usize = 1024 * 1024;

        run_in_isolated_test_process_with_env(
            "accept_with_small_frame_cap_rejects_oversized_frame",
            "HEW_WS_FRAME_CAP_REJECT_OVERSIZED_ISOLATED",
            &[
                (WEBSOCKET_MAX_MESSAGE_SIZE_ENV, "65536"),
                (WEBSOCKET_MAX_FRAME_SIZE_ENV, "65536"),
            ],
            || {
                // SAFETY: Valid C string literal passed to FFI.
                let server = unsafe { hew_ws_server_new(c"127.0.0.1:0".as_ptr()) };
                assert!(!server.is_null(), "server should bind successfully");
                // SAFETY: server is valid.
                let port = unsafe { hew_ws_server_port(server) };
                assert!(port > 0, "port should be positive");

                let addr = format!("ws://127.0.0.1:{port}");
                let client_thread = std::thread::spawn(move || {
                    let (mut ws, _) = connect_with_config(
                        &addr,
                        Some(websocket_config(
                            WEBSOCKET_MAX_MESSAGE_SIZE_BYTES,
                            WEBSOCKET_MAX_FRAME_SIZE_BYTES,
                        )),
                        3,
                    )
                    .expect("connect oversized-frame client");
                    match ws.send(Message::binary(vec![0u8; OVERSIZED_FRAME_BYTES])) {
                        Ok(()) => match ws.read() {
                            Ok(Message::Close(_))
                            | Err(
                                tungstenite::Error::ConnectionClosed
                                | tungstenite::Error::AlreadyClosed
                                | tungstenite::Error::Io(_)
                                | tungstenite::Error::Protocol(_),
                            ) => {}
                            other => {
                                panic!(
                                    "expected connection close after oversized frame, got {other:?}"
                                )
                            }
                        },
                        Err(tungstenite::Error::Io(err))
                            if err.kind() == io::ErrorKind::ConnectionReset
                                || err.kind() == io::ErrorKind::BrokenPipe => {}
                        Err(other) => panic!("expected oversized frame disconnect, got {other:?}"),
                    }
                });

                // SAFETY: server is a valid pointer returned by hew_ws_server_new.
                let conn = unsafe { hew_ws_server_accept(server) };
                assert!(!conn.is_null(), "accept should succeed");
                let read_result = {
                    // SAFETY: conn is valid until hew_ws_close below.
                    let conn_ref = unsafe { &*conn };
                    let mut guard = conn_ref.inner.ws.lock();
                    let ws = guard
                        .as_mut()
                        .expect("accepted websocket should be present");
                    ws.read()
                };
                match read_result {
                    Err(tungstenite::Error::Capacity(
                        tungstenite::error::CapacityError::MessageTooLong { size, max_size },
                    )) => {
                        assert_eq!(size, OVERSIZED_FRAME_BYTES);
                        assert_eq!(max_size, TEST_FRAME_CAP_BYTES);
                    }
                    other => panic!("expected message-too-large error, got {other:?}"),
                }

                unsafe { hew_ws_close(conn) };
                unsafe { hew_ws_server_close(server) };
                client_thread
                    .join()
                    .expect("oversized-frame client thread should finish");
            },
        );
    }

    #[test]
    fn accept_with_small_frame_cap_allows_exactly_at_cap_frame() {
        const TEST_FRAME_CAP_BYTES: usize = 64 * 1024;
        run_in_isolated_test_process_with_env(
            "accept_with_small_frame_cap_allows_exactly_at_cap_frame",
            "HEW_WS_FRAME_CAP_ALLOW_EXACT_ISOLATED",
            &[
                (WEBSOCKET_MAX_MESSAGE_SIZE_ENV, "65536"),
                (WEBSOCKET_MAX_FRAME_SIZE_ENV, "65536"),
            ],
            || {
                // SAFETY: Valid C string literal passed to FFI.
                let server = unsafe { hew_ws_server_new(c"127.0.0.1:0".as_ptr()) };
                assert!(!server.is_null(), "server should bind successfully");
                // SAFETY: server is valid.
                let port = unsafe { hew_ws_server_port(server) };
                assert!(port > 0, "port should be positive");

                let addr = format!("ws://127.0.0.1:{port}");
                let client_thread = std::thread::spawn(move || {
                    let (mut ws, _) = connect_with_config(
                        &addr,
                        Some(websocket_config(
                            WEBSOCKET_MAX_MESSAGE_SIZE_BYTES,
                            WEBSOCKET_MAX_FRAME_SIZE_BYTES,
                        )),
                        3,
                    )
                    .expect("connect exact-cap client");
                    ws.send(Message::binary(vec![7u8; TEST_FRAME_CAP_BYTES]))
                        .expect("send exact-cap frame");
                    ws.close(None).ok();
                });

                // SAFETY: server is a valid pointer returned by hew_ws_server_new.
                let conn = unsafe { hew_ws_server_accept(server) };
                assert!(!conn.is_null(), "accept should succeed");
                let msg = {
                    // SAFETY: conn is valid until hew_ws_close below.
                    let conn_ref = unsafe { &*conn };
                    let mut guard = conn_ref.inner.ws.lock();
                    let ws = guard
                        .as_mut()
                        .expect("accepted websocket should be present");
                    ws.read().expect("read exact-cap frame")
                };
                match msg {
                    Message::Binary(payload) => assert_eq!(payload.len(), TEST_FRAME_CAP_BYTES),
                    other => panic!("expected binary frame, got {other:?}"),
                }

                unsafe { hew_ws_close(conn) };
                unsafe { hew_ws_server_close(server) };
                client_thread
                    .join()
                    .expect("exact-cap client thread should finish");
            },
        );
    }

    #[test]
    fn accept_with_small_frame_cap_rejects_cap_plus_one_frame() {
        const TEST_FRAME_CAP_BYTES: usize = 64 * 1024;
        const OVERSIZED_FRAME_BYTES: usize = TEST_FRAME_CAP_BYTES + 1;

        run_in_isolated_test_process_with_env(
            "accept_with_small_frame_cap_rejects_cap_plus_one_frame",
            "HEW_WS_FRAME_CAP_REJECT_PLUS_ONE_ISOLATED",
            &[
                (WEBSOCKET_MAX_MESSAGE_SIZE_ENV, "65536"),
                (WEBSOCKET_MAX_FRAME_SIZE_ENV, "65536"),
            ],
            || {
                // SAFETY: Valid C string literal passed to FFI.
                let server = unsafe { hew_ws_server_new(c"127.0.0.1:0".as_ptr()) };
                assert!(!server.is_null(), "server should bind successfully");
                // SAFETY: server is valid.
                let port = unsafe { hew_ws_server_port(server) };
                assert!(port > 0, "port should be positive");

                let addr = format!("ws://127.0.0.1:{port}");
                let client_thread = std::thread::spawn(move || {
                    let (mut ws, _) = connect_with_config(
                        &addr,
                        Some(websocket_config(
                            WEBSOCKET_MAX_MESSAGE_SIZE_BYTES,
                            WEBSOCKET_MAX_FRAME_SIZE_BYTES,
                        )),
                        3,
                    )
                    .expect("connect cap-plus-one client");
                    match ws.send(Message::binary(vec![9u8; OVERSIZED_FRAME_BYTES])) {
                        Ok(()) => match ws.read() {
                            Ok(Message::Close(_))
                            | Err(
                                tungstenite::Error::ConnectionClosed
                                | tungstenite::Error::AlreadyClosed
                                | tungstenite::Error::Io(_)
                                | tungstenite::Error::Protocol(_),
                            ) => {}
                            other => {
                                panic!("expected connection close after cap+1 frame, got {other:?}")
                            }
                        },
                        Err(tungstenite::Error::Io(err))
                            if err.kind() == io::ErrorKind::ConnectionReset
                                || err.kind() == io::ErrorKind::BrokenPipe => {}
                        Err(other) => panic!("expected cap+1 frame disconnect, got {other:?}"),
                    }
                });

                // SAFETY: server is a valid pointer returned by hew_ws_server_new.
                let conn = unsafe { hew_ws_server_accept(server) };
                assert!(!conn.is_null(), "accept should succeed");
                let read_result = {
                    // SAFETY: conn is valid until hew_ws_close below.
                    let conn_ref = unsafe { &*conn };
                    let mut guard = conn_ref.inner.ws.lock();
                    let ws = guard
                        .as_mut()
                        .expect("accepted websocket should be present");
                    ws.read()
                };
                match read_result {
                    Err(tungstenite::Error::Capacity(
                        tungstenite::error::CapacityError::MessageTooLong { size, max_size },
                    )) => {
                        assert_eq!(size, OVERSIZED_FRAME_BYTES);
                        assert_eq!(max_size, TEST_FRAME_CAP_BYTES);
                    }
                    other => panic!("expected message-too-large error, got {other:?}"),
                }

                unsafe { hew_ws_close(conn) };
                unsafe { hew_ws_server_close(server) };
                client_thread
                    .join()
                    .expect("cap-plus-one client thread should finish");
            },
        );
    }

    #[test]
    fn server_accept_cancel_returns_cancelled() {
        let server = unsafe { hew_ws_server_new(c"127.0.0.1:0".as_ptr()) };
        assert!(!server.is_null(), "server should bind successfully");

        // SAFETY: `server` stays live until hew_ws_server_close below sets cancel.
        let inner = unsafe { &*server }.inner.clone();
        let server_addr = server as usize;
        let accept_thread = std::thread::spawn(move || unsafe {
            hew_ws_server_accept(server_addr as *mut HewWsServer) as usize
        });

        assert!(
            wait_for_condition(Duration::from_millis(200), || {
                inner.active_accepts.count() == 1
            }),
            "accept loop should become active before cancellation"
        );

        unsafe { hew_ws_server_close(server) };

        assert!(
            wait_for_thread_exit(&accept_thread, Duration::from_millis(750)),
            "accept thread should exit within the cancel deadline"
        );
        assert_eq!(
            accept_thread.join().expect("accept thread should join"),
            0,
            "cancelled accept should return null through the FFI surface"
        );
        assert_eq!(inner.active_accepts.count(), 0);
        assert_eq!(inner.active_handshakes.load(Ordering::Acquire), 0);
    }

    #[test]
    fn server_close_cancels_peer_that_never_handshakes() {
        let server = unsafe { hew_ws_server_new(c"127.0.0.1:0".as_ptr()) };
        assert!(!server.is_null(), "server should bind successfully");
        let port = unsafe { hew_ws_server_port(server) };
        let inner = unsafe { &*server }.inner.clone();
        let server_addr = server as usize;
        let accept_thread = std::thread::spawn(move || unsafe {
            hew_ws_server_accept(server_addr as *mut HewWsServer) as usize
        });

        let stalled_peer =
            TcpStream::connect(("127.0.0.1", u16::try_from(port).unwrap())).expect("tcp connect");
        assert!(
            wait_for_condition(Duration::from_millis(500), || {
                inner.active_handshakes.load(Ordering::Acquire) == 1
            }),
            "the accepted peer should enter the handshake"
        );

        let started = Instant::now();
        unsafe { hew_ws_server_close(server) };
        assert!(
            started.elapsed() < Duration::from_secs(1),
            "server close must cancel a stalled handshake promptly"
        );
        // close drained the accept authority before returning (count == 0 is
        // deterministic); the accept thread unwinds a few instructions later,
        // so poll for its exit rather than demanding zero scheduling lag.
        assert_eq!(inner.active_accepts.count(), 0);
        assert_eq!(inner.active_handshakes.load(Ordering::Acquire), 0);
        assert!(
            wait_for_thread_exit(&accept_thread, Duration::from_secs(1)),
            "close must drain the active accept before returning"
        );
        assert_eq!(accept_thread.join().expect("accept thread should join"), 0);
        drop(stalled_peer);
    }

    #[test]
    fn stalled_handshake_expires_and_next_valid_peer_is_accepted() {
        let server = unsafe { hew_ws_server_new(c"127.0.0.1:0".as_ptr()) };
        assert!(!server.is_null(), "server should bind successfully");
        let port = unsafe { hew_ws_server_port(server) };
        let inner = unsafe { &*server }.inner.clone();
        let server_addr = server as usize;
        let accept_thread = std::thread::spawn(move || unsafe {
            hew_ws_server_accept(server_addr as *mut HewWsServer) as usize
        });

        let stalled_peer =
            TcpStream::connect(("127.0.0.1", u16::try_from(port).unwrap())).expect("tcp connect");
        assert!(
            wait_for_condition(Duration::from_millis(500), || {
                inner.active_handshakes.load(Ordering::Acquire) == 1
            }),
            "the accepted peer should enter the handshake"
        );
        assert!(
            wait_for_condition(
                SERVER_HANDSHAKE_TIMEOUT + Duration::from_millis(750),
                || inner.active_handshakes.load(Ordering::Acquire) == 0
            ),
            "a peer that never handshakes must expire within the configured bound"
        );

        let (mut valid_peer, _) = connect_with_config(
            format!("ws://127.0.0.1:{port}"),
            Some(websocket_config(
                WEBSOCKET_MAX_MESSAGE_SIZE_BYTES,
                WEBSOCKET_MAX_FRAME_SIZE_BYTES,
            )),
            3,
        )
        .expect("valid peer should connect after stalled handshake expires");
        let conn = accept_thread.join().expect("accept thread should join") as *mut HewWsConn;
        assert!(!conn.is_null(), "the next valid peer should be published");

        valid_peer.close(None).expect("valid peer close");
        unsafe { hew_ws_close(conn) };
        unsafe { hew_ws_server_close(server) };
        assert_eq!(inner.active_accepts.count(), 0);
        assert_eq!(inner.active_handshakes.load(Ordering::Acquire), 0);
        drop(stalled_peer);
    }

    #[test]
    fn partial_and_invalid_handshakes_do_not_poison_next_accept() {
        let server = unsafe { hew_ws_server_new(c"127.0.0.1:0".as_ptr()) };
        assert!(!server.is_null(), "server should bind successfully");
        let port = unsafe { hew_ws_server_port(server) };
        let inner = unsafe { &*server }.inner.clone();
        let server_addr = server as usize;
        let accept_thread = std::thread::spawn(move || unsafe {
            hew_ws_server_accept(server_addr as *mut HewWsServer) as usize
        });

        let mut partial =
            TcpStream::connect(("127.0.0.1", u16::try_from(port).unwrap())).expect("tcp connect");
        partial
            .write_all(b"GET / HTTP/1.1\r\nHost: localhost\r\n")
            .expect("write partial handshake");
        assert!(
            wait_for_condition(Duration::from_millis(500), || {
                inner.active_handshakes.load(Ordering::Acquire) == 1
            }),
            "partial peer should enter the handshake"
        );
        drop(partial);
        assert!(
            wait_for_condition(Duration::from_millis(750), || {
                inner.active_handshakes.load(Ordering::Acquire) == 0
            }),
            "disconnecting the partial peer should end its handshake"
        );

        let mut invalid =
            TcpStream::connect(("127.0.0.1", u16::try_from(port).unwrap())).expect("tcp connect");
        invalid
            .write_all(b"definitely not a websocket handshake\r\n\r\n")
            .expect("write invalid handshake");
        drop(invalid);
        std::thread::sleep(Duration::from_millis(50));

        let (mut valid_peer, _) = connect_with_config(
            format!("ws://127.0.0.1:{port}"),
            Some(websocket_config(
                WEBSOCKET_MAX_MESSAGE_SIZE_BYTES,
                WEBSOCKET_MAX_FRAME_SIZE_BYTES,
            )),
            3,
        )
        .expect("valid peer should connect after invalid peers");
        let conn = accept_thread.join().expect("accept thread should join") as *mut HewWsConn;
        assert!(
            !conn.is_null(),
            "valid handshake should produce a connection"
        );

        valid_peer.close(None).expect("valid peer close");
        unsafe { hew_ws_close(conn) };
        unsafe { hew_ws_server_close(server) };
        assert_eq!(inner.active_accepts.count(), 0);
        assert_eq!(inner.active_handshakes.load(Ordering::Acquire), 0);
    }

    #[test]
    fn server_close_racing_completed_handshake_has_no_late_result() {
        let server = unsafe { hew_ws_server_new(c"127.0.0.1:0".as_ptr()) };
        assert!(!server.is_null(), "server should bind successfully");
        let port = unsafe { hew_ws_server_port(server) };
        let inner = unsafe { &*server }.inner.clone();
        let server_addr = server as usize;
        let accept_thread = std::thread::spawn(move || unsafe {
            hew_ws_server_accept(server_addr as *mut HewWsServer) as usize
        });

        let mut peer =
            TcpStream::connect(("127.0.0.1", u16::try_from(port).unwrap())).expect("tcp connect");
        assert!(
            wait_for_condition(Duration::from_millis(500), || {
                inner.active_handshakes.load(Ordering::Acquire) == 1
            }),
            "peer should enter the handshake before the race"
        );
        let websocket_key = ["dGhlIHNh", "bXBsZSBu", "b25jZQ=="].concat();
        let request = format!(
            "GET / HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\n\
             Upgrade: websocket\r\nConnection: Upgrade\r\n\
             Sec-WebSocket-Key: {websocket_key}\r\n\
             Sec-WebSocket-Version: 13\r\n\r\n"
        );
        peer.write_all(request.as_bytes())
            .expect("write valid websocket handshake");

        unsafe { hew_ws_server_close(server) };
        // The racing accept has published-or-cancelled before close returns —
        // its guard is dropped only after publish_if_open runs, so count == 0
        // the instant close returns proves the accept resolved. The thread that
        // ran it unwinds a few instructions later, so poll for its exit with a
        // bounded deadline rather than demanding zero scheduling lag.
        assert_eq!(inner.active_accepts.count(), 0);
        assert_eq!(inner.active_handshakes.load(Ordering::Acquire), 0);
        assert!(
            wait_for_thread_exit(&accept_thread, Duration::from_secs(1)),
            "server close must not return before the racing accept publishes or cancels"
        );
        let conn = accept_thread.join().expect("accept thread should join") as *mut HewWsConn;
        if !conn.is_null() {
            unsafe { hew_ws_close(conn) };
        }
        drop(peer);
    }

    #[test]
    fn repeated_bind_accept_close_drains_all_accept_authority() {
        let started = Instant::now();
        for _ in 0..64 {
            let server = unsafe { hew_ws_server_new(c"127.0.0.1:0".as_ptr()) };
            assert!(!server.is_null(), "server should bind successfully");
            let inner = unsafe { &*server }.inner.clone();
            let weak_inner = Arc::downgrade(&inner);
            let server_addr = server as usize;
            let accept_thread = std::thread::spawn(move || unsafe {
                hew_ws_server_accept(server_addr as *mut HewWsServer) as usize
            });
            assert!(
                wait_for_condition(Duration::from_millis(200), || {
                    inner.active_accepts.count() == 1
                }),
                "accept authority should become active"
            );

            unsafe { hew_ws_server_close(server) };
            // close is synchronous: cancel_and_wait only returns once every
            // accept guard has dropped, so the accept authority is fully drained
            // the instant close returns (count == 0 is the deterministic proof).
            // The OS thread that ran the accept unwinds a few instructions later
            // — it still has to return the sentinel and let the runtime mark the
            // JoinHandle finished — so poll for that exit with a bounded deadline
            // rather than demanding zero scheduling lag (close cannot join the
            // thread it does not own).
            assert_eq!(inner.active_accepts.count(), 0);
            assert_eq!(inner.active_handshakes.load(Ordering::Acquire), 0);
            assert!(
                wait_for_thread_exit(&accept_thread, Duration::from_secs(1)),
                "accept thread must unwind promptly once the authority is drained"
            );
            assert_eq!(accept_thread.join().expect("accept thread should join"), 0);
            drop(inner);
            assert!(
                weak_inner.upgrade().is_none(),
                "server inner allocation must be reclaimed after each close"
            );
        }
        assert!(
            started.elapsed() < Duration::from_secs(5),
            "the high-iteration close loop must remain bounded"
        );
    }

    #[test]
    fn recv_cancel_returns_cancelled() {
        let (server, conn, client) = attach_test_conn();
        // SAFETY: tests keep `conn` live until the recv thread exits.
        let inner = unsafe { &*conn }.inner.clone();
        let recv_thread = std::thread::spawn(move || {
            let _recv_guard = ActiveCallGuard::new(&inner.active_recvs);
            recv_message(&inner).0
        });

        assert!(
            wait_for_condition(Duration::from_millis(200), || {
                // SAFETY: `conn` remains live until hew_ws_close below.
                unsafe { &*conn }.inner.active_recvs.load(Ordering::Acquire) == 1
            }),
            "recv loop should become active before cancellation"
        );

        unsafe { hew_ws_close(conn) };

        assert!(
            wait_for_thread_exit(&recv_thread, Duration::from_millis(750)),
            "recv thread should exit within the cancel deadline"
        );
        assert_eq!(
            recv_thread.join().expect("recv thread should join"),
            HewWsRecvResult::Cancelled,
            "recv helper should report cancellation distinctly"
        );

        drop(client);
        unsafe { hew_ws_server_close(server) };
    }

    #[test]
    fn server_accept_unblocks_when_owner_actor_stops() {
        run_in_isolated_test_process(
            "server_accept_unblocks_when_owner_actor_stops",
            "HEW_WS_ACCEPT_STOP_ISOLATED",
            || {
                let _runtime = NetErrorSlotRuntimeGuard::new();
                let server = unsafe { hew_ws_server_new(c"127.0.0.1:0".as_ptr()) };
                assert!(!server.is_null(), "server should bind successfully");

                // SAFETY: `server` stays live until the owner actor closes it.
                let inner = unsafe { &*server }.inner.clone();
                let server_addr = server as usize;
                let accept_thread = std::thread::spawn(move || unsafe {
                    hew_ws_server_accept(server_addr as *mut HewWsServer) as usize
                });

                assert!(
                    wait_for_condition(Duration::from_millis(200), || {
                        inner.active_accepts.count() == 1
                    }),
                    "accept loop should become active before the owner stops"
                );

                let state = CancelOwnerState {
                    server: server as usize,
                    conn: 0,
                };
                let actor = unsafe {
                    actor::hew_actor_spawn(
                        (&raw const state).cast_mut().cast(),
                        std::mem::size_of::<CancelOwnerState>(),
                        Some(websocket_cancel_owner_dispatch),
                    )
                };
                assert!(!actor.is_null(), "owner actor should spawn");

                unsafe { actor::hew_actor_send(actor, TEST_STOP_TYPE, std::ptr::null_mut(), 0) };

                assert!(
                    wait_for_actor_dead(actor, Duration::from_secs(1)),
                    "owner actor should stop after closing the server"
                );
                assert!(
                    wait_for_thread_exit(&accept_thread, Duration::from_millis(750)),
                    "accept thread should exit within the cancel deadline"
                );
                assert_eq!(
                    accept_thread.join().expect("accept thread should join"),
                    0,
                    "cancelled accept should return null through the FFI surface"
                );
                assert_eq!(inner.active_accepts.count(), 0);

                assert_eq!(unsafe { actor::hew_actor_free(actor) }, 0);
            },
        );
    }

    #[test]
    fn attached_plain_ping_pong_echoes_payload() {
        run_in_isolated_test_process(
            "attached_plain_ping_pong_echoes_payload",
            "HEW_WS_ATTACH_PING_PONG_ISOLATED",
            || {
                let _runtime = NetErrorSlotRuntimeGuard::new();
                let (server, conn, mut client) = attach_test_conn();
                let (actor, test_id, _rx) = spawn_attached_actor(conn);
                set_peer_read_timeout(
                    &mut client,
                    (READER_READ_TIMEOUT * 2) + READER_WAIT_POLL + Duration::from_secs(1),
                );

                let payload = b"abc".to_vec();
                client
                    .send(Message::Ping(payload.clone().into()))
                    .expect("client ping should send");
                let msg = client.read().expect("client should receive a pong");
                match msg {
                    Message::Pong(bytes) => assert_eq!(&bytes[..], payload.as_slice()),
                    other => panic!("expected pong response, got {other:?}"),
                }

                teardown_attached_actor(actor, test_id, conn, server);
            },
        );
    }

    #[cfg(unix)]
    #[test]
    fn attached_plain_large_send_and_ping_are_serialized() {
        run_in_isolated_test_process(
            "attached_plain_large_send_and_ping_are_serialized",
            "HEW_WS_ATTACH_LARGE_SEND_PING_ISOLATED",
            || {
                let _runtime = NetErrorSlotRuntimeGuard::new();
                let (server, conn, mut client) = attach_test_conn();
                let (actor, test_id, _rx) = spawn_attached_actor(conn);
                set_conn_send_buffer(conn, 4096);
                set_peer_read_timeout(&mut client, Duration::from_secs(5));

                let payload: Vec<u8> = (0u8..=250).cycle().take(1024 * 1024).collect();
                let expected = payload.clone();
                let conn_addr = conn as usize;
                let (started_tx, started_rx) = mpsc::channel();
                let send_thread = std::thread::spawn(move || {
                    started_tx.send(()).expect("signal send start");
                    unsafe {
                        hew_ws_send_binary(
                            conn_addr as *mut HewWsConn,
                            payload.as_ptr(),
                            payload.len(),
                        )
                    }
                });
                started_rx
                    .recv_timeout(Duration::from_secs(1))
                    .expect("send thread should start");
                std::thread::sleep(Duration::from_millis(25));

                client
                    .send(Message::Ping(b"race".to_vec().into()))
                    .expect("client ping should send during large send");

                let deadline = Instant::now() + Duration::from_secs(5);
                let mut saw_binary = false;
                let mut saw_pong = false;
                while !(saw_binary && saw_pong) {
                    assert!(
                        Instant::now() < deadline,
                        "client should observe intact binary frame and pong"
                    );
                    match client
                        .read()
                        .expect("peer parser should not see corrupt frames")
                    {
                        Message::Binary(bytes) => {
                            assert_eq!(&bytes[..], expected.as_slice());
                            saw_binary = true;
                        }
                        Message::Pong(bytes) => {
                            assert_eq!(&bytes[..], b"race");
                            saw_pong = true;
                        }
                        Message::Close(close) => panic!("unexpected close frame: {close:?}"),
                        _ => {}
                    }
                }

                assert_eq!(
                    send_thread.join().expect("send thread should join"),
                    0,
                    "large Hew send should succeed"
                );
                teardown_attached_actor(actor, test_id, conn, server);
            },
        );
    }

    #[test]
    fn attached_reader_cancel_after_ping_exits_cleanly() {
        run_in_isolated_test_process(
            "attached_reader_cancel_after_ping_exits_cleanly",
            "HEW_WS_ATTACH_PING_CANCEL_ISOLATED",
            || {
                let _runtime = NetErrorSlotRuntimeGuard::new();
                OUTER_CONN_DROPS.store(0, Ordering::Relaxed);
                let (server, conn, mut client) = attach_test_conn();
                let inner = unsafe { &*conn }.inner.clone();
                let (actor, test_id, _rx) = spawn_attached_actor(conn);

                client
                    .send(Message::Ping(b"cancel".to_vec().into()))
                    .expect("client ping should send before cancellation");
                unsafe { hew_ws_close(conn) };

                assert!(
                    wait_for_condition(Duration::from_millis(750), || {
                        inner
                            .reader
                            .lock()
                            .expect("reader mutex poisoned")
                            .as_ref()
                            .is_none_or(|reader| reader.exited.load(Ordering::Acquire))
                    }),
                    "reader should exit within the cancel deadline"
                );
                assert_eq!(
                    OUTER_CONN_DROPS.load(Ordering::Relaxed),
                    1,
                    "attached close should drop the outer connection exactly once"
                );

                drop(client);
                unsafe { actor::hew_actor_stop(actor) };
                assert_eq!(unsafe { actor::hew_actor_free(actor) }, 0);
                unregister_actor_events(test_id);
                unsafe { hew_ws_server_close(server) };
            },
        );
    }

    #[test]
    fn attach_reader_exits_when_actor_stops() {
        run_in_isolated_test_process(
            "attach_reader_exits_when_actor_stops",
            "HEW_WS_ATTACH_STOP_ISOLATED",
            || {
                let _runtime = NetErrorSlotRuntimeGuard::new();
                let (server, conn, client) = attach_test_conn();
                let (actor, test_id, _rx) = spawn_attached_actor(conn);
                let inner = unsafe { &*conn }.inner.clone();
                let delivery = attached_delivery(&inner);

                unsafe { actor::hew_actor_send(actor, TEST_STOP_TYPE, std::ptr::null_mut(), 0) };

                assert!(
                    wait_for_actor_dead(actor, Duration::from_secs(1)),
                    "actor should transition to a non-live state"
                );
                assert!(
                    wait_for_reader_exit(&inner, Duration::from_millis(750)),
                    "reader should exit within the bounded deadline after actor stop"
                );
                assert!(delivery.is_revoked());

                drop(client);
                teardown_attached_actor(actor, test_id, conn, server);
            },
        );
    }

    #[test]
    fn attach_reader_exits_when_actor_crashes() {
        run_in_isolated_test_process(
            "attach_reader_exits_when_actor_crashes",
            "HEW_WS_ATTACH_CRASH_ISOLATED",
            || {
                let _runtime = NetErrorSlotRuntimeGuard::new();
                let (server, conn, client) = attach_test_conn();
                let (actor, test_id, _rx) = spawn_attached_actor(conn);
                let inner = unsafe { &*conn }.inner.clone();
                let delivery = attached_delivery(&inner);

                unsafe { actor::hew_actor_send(actor, TEST_CRASH_TYPE, std::ptr::null_mut(), 0) };

                assert!(
                    wait_for_actor_dead(actor, Duration::from_secs(1)),
                    "crashed actor should become non-live"
                );
                assert!(
                    wait_for_reader_exit(&inner, Duration::from_millis(750)),
                    "reader should exit within the bounded deadline after actor crash"
                );
                assert!(delivery.is_revoked());

                drop(client);
                teardown_attached_actor(actor, test_id, conn, server);
            },
        );
    }

    #[test]
    fn attach_reader_revokes_delivery_when_actor_is_forced_to_stop() {
        run_in_isolated_test_process(
            "attach_reader_revokes_delivery_when_actor_is_forced_to_stop",
            "HEW_WS_ATTACH_FORCED_STOP_ISOLATED",
            || {
                let _runtime = NetErrorSlotRuntimeGuard::new();
                let (server, conn, client) = attach_test_conn();
                let (actor, test_id, _rx) = spawn_attached_actor(conn);
                let inner = unsafe { &*conn }.inner.clone();
                let delivery = attached_delivery(&inner);

                unsafe { actor::hew_actor_stop(actor) };
                assert!(
                    wait_for_actor_dead(actor, Duration::from_secs(1)),
                    "forced actor stop should transition the actor to non-live"
                );
                assert!(
                    wait_for_reader_exit(&inner, Duration::from_millis(750)),
                    "reader should exit after forced actor stop"
                );
                assert!(
                    delivery.is_revoked(),
                    "reader exit must discard the raw actor capability"
                );

                drop(client);
                unsafe { hew_ws_close(conn) };
                assert_eq!(unsafe { actor::hew_actor_free(actor) }, 0);
                unregister_actor_events(test_id);
                unsafe { hew_ws_server_close(server) };
            },
        );
    }

    #[test]
    fn attached_close_race_revokes_delivery_before_return() {
        run_in_isolated_test_process(
            "attached_close_race_revokes_delivery_before_return",
            "HEW_WS_ATTACH_CLOSE_RACE_ISOLATED",
            || {
                let _runtime = NetErrorSlotRuntimeGuard::new();
                OUTER_CONN_DROPS.store(0, Ordering::Relaxed);
                ACTOR_RUNTIME_CALLS.store(0, Ordering::Relaxed);
                let (server, conn, mut client) = attach_test_conn();
                let (actor, test_id, _rx) = spawn_attached_actor(conn);
                let inner = unsafe { &*conn }.inner.clone();
                let delivery = attached_delivery(&inner);

                for index in 0..32 {
                    client
                        .send(Message::text(format!("queued-{index}")))
                        .expect("queue traffic before close");
                }

                let started = Instant::now();
                unsafe { hew_ws_close(conn) };
                assert!(
                    started.elapsed() < Duration::from_secs(1),
                    "attached close must stay bounded"
                );
                assert!(
                    delivery.is_revoked(),
                    "close must revoke the reader's actor capability before returning"
                );
                assert_eq!(
                    OUTER_CONN_DROPS.load(Ordering::Relaxed),
                    1,
                    "the outer connection box must be reclaimed exactly once"
                );

                let runtime_calls_after_close = ACTOR_RUNTIME_CALLS.load(Ordering::Relaxed);
                let _ = client.send(Message::text("late"));
                std::thread::sleep(READER_READ_TIMEOUT + Duration::from_millis(100));
                assert_eq!(
                    ACTOR_RUNTIME_CALLS.load(Ordering::Relaxed),
                    runtime_calls_after_close,
                    "queued traffic must not trigger actor runtime calls after close returns"
                );

                drop(client);
                unsafe { actor::hew_actor_stop(actor) };
                assert_eq!(unsafe { actor::hew_actor_free(actor) }, 0);
                unregister_actor_events(test_id);
                unsafe { hew_ws_server_close(server) };
            },
        );
    }

    #[test]
    fn repeated_attach_close_reclaims_every_connection_authority() {
        run_in_isolated_test_process(
            "repeated_attach_close_reclaims_every_connection_authority",
            "HEW_WS_ATTACH_RECLAIM_STRESS_ISOLATED",
            || {
                const ITERATIONS: usize = 64;

                let _runtime = NetErrorSlotRuntimeGuard::new();
                OUTER_CONN_DROPS.store(0, Ordering::Relaxed);
                for _ in 0..ITERATIONS {
                    let (server, conn, client) = attach_test_conn();
                    let (actor, test_id, _rx) = spawn_attached_actor(conn);
                    let inner = unsafe { &*conn }.inner.clone();
                    let weak_inner = Arc::downgrade(&inner);
                    let delivery = attached_delivery(&inner);

                    unsafe { hew_ws_close(conn) };
                    assert!(delivery.is_revoked());
                    drop(delivery);
                    drop(client);
                    unsafe { actor::hew_actor_stop(actor) };
                    assert_eq!(unsafe { actor::hew_actor_free(actor) }, 0);
                    unregister_actor_events(test_id);
                    unsafe { hew_ws_server_close(server) };
                    drop(inner);
                    assert!(
                        weak_inner.upgrade().is_none(),
                        "connection inner allocation must be reclaimed after close"
                    );
                }
                assert_eq!(
                    OUTER_CONN_DROPS.load(Ordering::Relaxed),
                    ITERATIONS,
                    "every attached outer connection must be reclaimed exactly once"
                );
            },
        );
    }

    #[test]
    fn attach_reader_exits_when_conn_closes_before_actor_stop() {
        run_in_isolated_test_process(
            "attach_reader_exits_when_conn_closes_before_actor_stop",
            "HEW_WS_ATTACH_CONN_DROP_ISOLATED",
            || {
                let _runtime = NetErrorSlotRuntimeGuard::new();
                let (server, conn, client) = attach_test_conn();
                let (actor, test_id, rx) = spawn_attached_actor(conn);
                let inner = unsafe { &*conn }.inner.clone();
                let delivery = attached_delivery(&inner);
                ACTOR_RUNTIME_CALLS.store(0, Ordering::Relaxed);

                unsafe { hew_ws_close(conn) };

                assert!(
                    wait_for_reader_exit(&inner, Duration::from_millis(750)),
                    "reader should exit promptly when the attached connection closes"
                );
                assert!(
                    delivery.is_revoked(),
                    "close must synchronously revoke actor delivery"
                );
                let runtime_calls_after_close = ACTOR_RUNTIME_CALLS.load(Ordering::Relaxed);
                assert_no_event(&rx, READER_READ_TIMEOUT + Duration::from_millis(100));
                assert_eq!(
                    ACTOR_RUNTIME_CALLS.load(Ordering::Relaxed),
                    runtime_calls_after_close,
                    "the reader must not call actor runtime functions after close returns"
                );

                drop(client);
                unsafe { actor::hew_actor_stop(actor) };
                assert_eq!(unsafe { actor::hew_actor_free(actor) }, 0);
                unregister_actor_events(test_id);
                unsafe { hew_ws_server_close(server) };
            },
        );
    }

    #[test]
    fn attach_reader_exits_when_remote_closes() {
        run_in_isolated_test_process(
            "attach_reader_exits_when_remote_closes",
            "HEW_WS_ATTACH_REMOTE_CLOSE_ISOLATED",
            || {
                let _runtime = NetErrorSlotRuntimeGuard::new();
                let (server, conn, mut client) = attach_test_conn();
                let (actor, test_id, rx) = spawn_attached_actor(conn);
                let inner = unsafe { &*conn }.inner.clone();
                let delivery = attached_delivery(&inner);

                client.close(None).expect("client close frame");

                assert_eq!(
                    recv_event(&rx, Duration::from_secs(1)),
                    ActorEvent::Closed,
                    "remote close should notify the actor exactly once"
                );
                assert!(
                    wait_for_reader_exit(&inner, Duration::from_millis(750)),
                    "reader should exit after the remote close handshake"
                );
                assert!(delivery.is_revoked());
                assert_no_event(&rx, Duration::from_millis(200));

                teardown_attached_actor(actor, test_id, conn, server);
            },
        );
    }

    #[test]
    fn attach_reader_cancel_is_per_connection() {
        run_in_isolated_test_process(
            "attach_reader_cancel_is_per_connection",
            "HEW_WS_ATTACH_PARALLEL_ISOLATED",
            || {
                let _runtime = NetErrorSlotRuntimeGuard::new();
                let (server1, conn1, client1) = attach_test_conn();
                let (server2, conn2, mut client2) = attach_test_conn();
                let (actor1, test_id1, _rx1) = spawn_attached_actor(conn1);
                let (actor2, test_id2, rx2) = spawn_attached_actor(conn2);
                let inner1 = unsafe { &*conn1 }.inner.clone();
                let inner2 = unsafe { &*conn2 }.inner.clone();

                unsafe { actor::hew_actor_send(actor1, TEST_STOP_TYPE, std::ptr::null_mut(), 0) };
                assert!(
                    wait_for_actor_dead(actor1, Duration::from_secs(1)),
                    "first actor should stop"
                );
                assert!(
                    wait_for_reader_exit(&inner1, Duration::from_secs(1)),
                    "first reader should exit after its actor stops"
                );
                assert!(
                    !wait_for_reader_exit(&inner2, Duration::from_millis(300)),
                    "second reader should stay live when the first actor stops"
                );

                client2
                    .send(Message::text("still-alive"))
                    .expect("second client send");
                assert_eq!(
                    recv_event(&rx2, Duration::from_secs(1)),
                    ActorEvent::Message("still-alive".to_owned()),
                    "second actor should continue receiving frames"
                );

                drop(client1);
                teardown_attached_actor(actor1, test_id1, conn1, server1);
                teardown_attached_actor(actor2, test_id2, conn2, server2);
            },
        );
    }

    /// Server with null addr returns null.
    #[test]
    fn server_null_addr_returns_null() {
        // SAFETY: Passing null is explicitly handled by hew_ws_server_new.
        let server = unsafe { hew_ws_server_new(std::ptr::null()) };
        assert!(server.is_null());
    }

    #[test]
    fn server_malformed_addr_is_invalid_argument_not_other_zero() {
        let addr = c"not an address";
        // SAFETY: addr is a valid C string.
        let server = unsafe { hew_ws_server_new(addr.as_ptr()) };
        assert!(server.is_null());
        assert_eq!(hew_ws_last_errno(), -1);
        assert_eq!(
            ws_last_error_text(),
            "websocket.listen: cannot bind `not an address`"
        );
        assert_eq!(hew_ws_last_errno(), -1);
    }

    /// Server port with null returns -1.
    #[test]
    fn server_port_null_returns_neg1() {
        // SAFETY: Passing null is explicitly handled by hew_ws_server_port.
        assert_eq!(unsafe { hew_ws_server_port(std::ptr::null()) }, -1);
    }

    /// Server accept with null returns null.
    #[test]
    fn server_accept_null_returns_null() {
        // SAFETY: Passing null is explicitly handled by hew_ws_server_accept.
        assert!(unsafe { hew_ws_server_accept(std::ptr::null_mut()) }.is_null());
    }

    /// Server close with null is a no-op.
    #[test]
    fn server_close_null_is_noop() {
        // SAFETY: Passing null is explicitly handled by hew_ws_server_close.
        unsafe { hew_ws_server_close(std::ptr::null_mut()) };
    }
}
