// WASM-TODO(websocket): WebSocket transport is unavailable on WASM (requires OS threads).
//! Hew runtime: `websocket` module.
//!
//! Provides synchronous WebSocket client functionality for compiled Hew programs.
//! Text arguments and results use managed UTF-8 strings. Raw message payloads
//! retain their pointer-and-length storage, released with the message handle.

use crate::bind_addr::normalize_bind_addr;
use crate::tls::{self, ConnectFailure, Trust};
use hew_cabi::string::{string_as_str, string_from_str, HewString};
use hew_runtime::bytes::BytesTriple;
use hew_runtime::transport::{AttachCallback, NativeActorToken, NativeAttachment};
#[cfg(test)]
use std::ffi::c_void;
use std::io;
#[cfg(test)]
use std::io::Write;
use std::net::{Shutdown, TcpListener, TcpStream};
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
    /// this gate covers writes through both socket handles. It is held across
    /// the complete tungstenite operation (`send` or `read` auto-flush), so
    /// short socket writes cannot
    /// allow another framer to splice control-frame bytes into a data frame.
    write_operation_gate: Option<WriteOperationGate>,
    shutdown_stream: Option<TcpStream>,
    reader: Mutex<Option<ReaderControl>>,
    closed: AtomicBool,
    active_recvs: AtomicUsize,
    /// The subprotocol the server selected from the client's offer.
    subprotocol: Option<String>,
    /// The payload of the last `hew_ws_recv_next` message, until taken.
    pending: Mutex<Vec<u8>>,
}

type HewWs = WebSocket<MaybeTlsStream<TcpStream>>;
type WriteOperationGate = Arc<PlMutex<()>>;
type PreparedWebsockets = (HewWs, Option<HewWs>, Option<WriteOperationGate>);

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

#[derive(Debug)]
enum HewWsAcceptResult {
    Accepted(Box<WebSocket<MaybeTlsStream<TcpStream>>>),
    Cancelled,
    Error,
}

/// One `receive` outcome. Control frames are answered by tungstenite and
/// never surface.
#[derive(Debug)]
enum Received {
    Text(String),
    Binary(Vec<u8>),
    /// The peer closed the connection, or this side did.
    Closed,
    TimedOut,
    Failed(tungstenite::Error),
}

/// `hew_ws_recv_next` statuses, mirrored by `websocket.hew`.
const RECV_TEXT: i32 = 0;
const RECV_BINARY: i32 = 1;
const RECV_CLOSED: i32 = 2;
const RECV_TIMED_OUT: i32 = 3;
const RECV_FAILED: i32 = 4;

/// Error-slot errno sentinels for failures without an OS errno, mirrored by
/// `websocket.hew`: an argument refused before any I/O, and a deadline.
const WS_INVALID_ARGUMENT: i64 = -1;
const WS_TIMED_OUT: i64 = -2;

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

#[derive(Debug)]
struct ActorDelivery {
    target: Mutex<Option<NativeAttachment>>,
}

impl ActorDelivery {
    fn new(target: NativeAttachment) -> Self {
        Self {
            target: Mutex::new(Some(target)),
        }
    }

    fn revoke(&self) {
        lock_or_recover(&self.target).take();
    }

    fn is_alive(&self) -> bool {
        lock_or_recover(&self.target)
            .as_ref()
            .is_some_and(|target| {
                #[cfg(test)]
                ACTOR_RUNTIME_CALLS.fetch_add(1, Ordering::Relaxed);
                target.is_alive()
            })
    }

    fn send_text(&self, bytes: &[u8]) -> i32 {
        lock_or_recover(&self.target).as_ref().map_or(2, |target| {
            #[cfg(test)]
            ACTOR_RUNTIME_CALLS.fetch_add(1, Ordering::Relaxed);
            target.deliver(bytes)
        })
    }

    fn close(&self) {
        if let Some(target) = lock_or_recover(&self.target).as_ref() {
            #[cfg(test)]
            ACTOR_RUNTIME_CALLS.fetch_add(1, Ordering::Relaxed);
            let _ = target.closed();
        }
    }

    #[cfg(test)]
    fn is_revoked(&self) -> bool {
        lock_or_recover(&self.target).is_none()
    }
}

impl HewWsConn {
    fn new(
        ws: WebSocket<MaybeTlsStream<TcpStream>>,
        role: Role,
        subprotocol: Option<String>,
    ) -> Self {
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
                subprotocol,
                pending: Mutex::new(Vec::new()),
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

fn clone_shutdown_stream(ws: &WebSocket<MaybeTlsStream<TcpStream>>) -> Option<TcpStream> {
    match ws.get_ref() {
        MaybeTlsStream::Plain(stream) => stream.try_clone().ok(),
        MaybeTlsStream::Rustls(stream) => stream.sock.try_clone().ok(),
        _ => None,
    }
}

/// Preserve the handshake framer, including bytes read beyond the HTTP upgrade.
/// A peer may send its first frame together with the handshake response; rebuilding
/// the reader from `into_inner()` would silently discard those buffered bytes.
/// Plain TCP gets an independent write framer over a cloned socket. The operation
/// gate serializes its complete frames against the reader's automatic control
/// replies, including short writes. TLS retains its single stateful framer.
fn prepare_websockets(ws: HewWs, role: Role) -> PreparedWebsockets {
    let write_ws = match ws.get_ref() {
        MaybeTlsStream::Plain(stream) => stream.try_clone().ok().map(|stream| {
            WebSocket::from_raw_socket(MaybeTlsStream::Plain(stream), role, Some(*ws.get_config()))
        }),
        _ => None,
    };
    let gate = write_ws.as_ref().map(|_| Arc::new(PlMutex::new(())));
    (ws, write_ws, gate)
}

fn with_tcp_stream<R>(
    ws: &mut HewWs,
    f: impl FnOnce(&mut TcpStream) -> io::Result<R>,
) -> io::Result<R> {
    match ws.get_mut() {
        MaybeTlsStream::Plain(stream) => f(stream),
        MaybeTlsStream::Rustls(stream) => f(&mut stream.sock),
        _ => Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "unsupported websocket stream kind",
        )),
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

fn reader_cleanup(inner: &Arc<HewWsConnInner>, delivery: &ActorDelivery, notify_close: bool) {
    if notify_close {
        delivery.close();
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
    delivery: ActorDelivery,
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

    // A Hew actor handle crosses an extern C call as the bare local actor
    // pointer. The delivery authority is the only owner of that pointer and
    // can be synchronously revoked even if the reader itself has not exited.
    let delivery = Arc::new(delivery);
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
                    if reader_delivery.send_text(bytes) != 0 {
                        notify_close = true;
                        break;
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

        reader_cleanup(&inner, &reader_delivery, notify_close);
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

/// Read the next data message, answering control frames, until `deadline`.
///
/// The lock and each socket read are bounded by the reader pacing interval
/// so a concurrent close or an attached reader holding the framer cannot
/// stall the caller past its deadline.
fn receive(inner: &Arc<HewWsConnInner>, deadline: Option<Instant>) -> Received {
    loop {
        if inner.closed.load(Ordering::Acquire) {
            return Received::Closed;
        }
        let slice = match deadline {
            None => READER_READ_TIMEOUT,
            Some(deadline) => {
                let left = deadline.saturating_duration_since(Instant::now());
                if left.is_zero() {
                    return Received::TimedOut;
                }
                left.min(READER_READ_TIMEOUT)
            }
        };
        let Some(mut guard) = inner.ws.try_lock_for(slice) else {
            continue;
        };
        let Some(ws) = guard.as_mut() else {
            return Received::Closed;
        };
        if let Err(err) = set_read_timeout(ws, Some(slice)) {
            return Received::Failed(tungstenite::Error::Io(err));
        }
        let read = read_ws_with_operation_gate(inner, ws);
        drop(guard);
        match read {
            Ok(Message::Text(text)) => return Received::Text(text.as_str().to_owned()),
            Ok(Message::Binary(data)) => return Received::Binary(data.to_vec()),
            Ok(Message::Close(_))
            | Err(tungstenite::Error::ConnectionClosed | tungstenite::Error::AlreadyClosed) => {
                return Received::Closed;
            }
            Ok(Message::Ping(_) | Message::Pong(_) | Message::Frame(_)) => {}
            Err(err) if is_timeout_error(&err) => {}
            Err(_) if inner.closed.load(Ordering::Acquire) => return Received::Closed,
            Err(err) => return Received::Failed(err),
        }
    }
}

/// How a client connect verifies and bounds itself.
struct ConnectOptions<'a> {
    subprotocols: Vec<&'a str>,
    trust: Trust<'a>,
    handshake: Option<Duration>,
    io: Option<Duration>,
}

/// A failed client connect: the error-slot errno and its detail.
type ConnectError = (i64, String);

fn connect_failure(failure: &ConnectFailure) -> ConnectError {
    let errno = match failure.class {
        tls::TLS_CONNECT_INVALID_ARGUMENT => WS_INVALID_ARGUMENT,
        tls::TLS_CONNECT_TIMED_OUT => WS_TIMED_OUT,
        _ => failure.errno,
    };
    (errno, failure.message.clone())
}

fn tungstenite_failure(err: &tungstenite::Error) -> ConnectError {
    (ws_errno_of(err), err.to_string())
}

/// Connect to a WebSocket server.
///
/// `protocols` is the comma-separated subprotocol offer, in preference
/// order, or empty. `wss://` verifies the server with `trust` (`0` bundled
/// roots, `1` only the CAs in `pem`). `handshake_ms` bounds the TCP connect,
/// the TLS handshake and the upgrade together; `io_ms` then bounds each
/// write. Zero means no bound. Returns null on failure with the errno (`-1`
/// invalid argument, `-2` timed out) and detail in the error slot.
///
/// # Safety
///
/// `url` and `protocols` must be live managed string handles (null means
/// empty); `pem` must be null or a live `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_connect(
    url: *const HewString,
    protocols: *const HewString,
    trust: i32,
    pem: *const BytesTriple,
    handshake_ms: i64,
    io_ms: i64,
) -> *mut HewWsConn {
    // SAFETY: the caller borrows live managed strings and bytes.
    let (url_str, protocols, pem) = unsafe {
        (
            string_as_str(url),
            string_as_str(protocols),
            tls::bytes_view(pem),
        )
    };
    let options = ConnectOptions {
        subprotocols: protocols.split(',').filter(|p| !p.is_empty()).collect(),
        trust: if trust == 1 {
            Trust::Pem(pem)
        } else {
            Trust::Bundled
        },
        handshake: tls::io_timeout(handshake_ms),
        io: tls::io_timeout(io_ms),
    };
    let attempt = websocket_config_from_env()
        .map_err(|err| (WS_INVALID_ARGUMENT, format!("invalid config: {err}")))
        .and_then(|config| connect_client(url_str, config, 3, &options));
    match attempt {
        Ok((ws, subprotocol)) => {
            clear_ws_last_error();
            Box::into_raw(Box::new(HewWsConn::new(ws, Role::Client, subprotocol)))
        }
        Err((errno, detail)) => {
            set_ws_last_error(errno, format!("websocket.connect: {detail}"));
            std::ptr::null_mut()
        }
    }
}

/// Connect, following up to `max_redirects` redirects that keep the first
/// request's transport (a `wss://` connection never continues as `ws://`),
/// and return the socket with the subprotocol the server selected.
///
/// The TCP connect is made here rather than by `tungstenite::connect` so the
/// one authoritative attempt's errno survives, `wss://` uses the TLS module's
/// trust and deadline, and every step shares one deadline.
fn connect_client(
    url_str: &str,
    config: WebSocketConfig,
    max_redirects: u8,
    options: &ConnectOptions<'_>,
) -> Result<(HewWs, Option<String>), ConnectError> {
    let deadline = options.handshake.map(|limit| Instant::now() + limit);
    let mut current = url_str.to_owned();
    // Whether the first request asked for TLS; a redirect never changes it.
    let mut first_tls = None;
    for attempt in 0..=max_redirects {
        let mut request = current
            .as_str()
            .into_client_request()
            .map_err(|err| tungstenite_failure(&err))?;
        if !options.subprotocols.is_empty() {
            let offer = options.subprotocols.join(", ");
            let value = offer
                .parse()
                .map_err(|_| (WS_INVALID_ARGUMENT, format!("subprotocol offer {offer:?}")))?;
            request
                .headers_mut()
                .insert("Sec-WebSocket-Protocol", value);
        }
        let uri = request.uri();
        let mode = uri_mode(uri).map_err(|err| tungstenite_failure(&err))?;
        keep_transport(&mut first_tls, mode)?;
        let host = uri
            .host()
            .ok_or_else(|| {
                tungstenite_failure(&tungstenite::Error::Url(
                    tungstenite::error::UrlError::NoHostName,
                ))
            })?
            .trim_start_matches('[')
            .trim_end_matches(']')
            .to_owned();
        let port = uri.port_u16().unwrap_or(match mode {
            Mode::Plain => 80,
            Mode::Tls => 443,
        });
        let mut tcp =
            tls::connect_tcp(&host, port, deadline).map_err(|failure| connect_failure(&failure))?;
        tcp.set_nodelay(true)
            .map_err(|err| connect_failure(&ConnectFailure::from_io("connect", &err)))?;
        let stream = match mode {
            Mode::Plain => MaybeTlsStream::Plain(tcp),
            Mode::Tls => {
                let config = tls::client_config(options.trust)
                    .map_err(|failure| connect_failure(&failure))?;
                let name = rustls::pki_types::ServerName::try_from(host.clone())
                    .map_err(|err| (WS_INVALID_ARGUMENT, format!("server name {host:?}: {err}")))?;
                let mut connection = rustls::ClientConnection::new(config, name)
                    .map_err(|err| (WS_INVALID_ARGUMENT, format!("tls client: {err}")))?;
                tls::complete_handshake(&mut connection, &mut tcp, deadline)
                    .map_err(|failure| connect_failure(&failure))?;
                MaybeTlsStream::Rustls(rustls::StreamOwned::new(connection, tcp))
            }
        };
        let stream = bound_upgrade(stream, deadline)?;
        let (mut ws, response) = match client_with_config(request, stream, Some(config)) {
            Ok(upgraded) => upgraded,
            Err(tungstenite::HandshakeError::Interrupted(_)) => {
                return Err((WS_TIMED_OUT, "upgrade timed out".to_owned()));
            }
            // Windows reports a passed socket timeout as `TimedOut` where Unix
            // reports `WouldBlock`, which tungstenite turns into `Interrupted`.
            Err(tungstenite::HandshakeError::Failure(tungstenite::Error::Io(error)))
                if error.kind() == std::io::ErrorKind::TimedOut =>
            {
                return Err((WS_TIMED_OUT, "upgrade timed out".to_owned()));
            }
            Err(tungstenite::HandshakeError::Failure(tungstenite::Error::Http(response)))
                if response.status().is_redirection() && attempt < max_redirects =>
            {
                let location = response
                    .headers()
                    .get("Location")
                    .and_then(|value| value.to_str().ok())
                    .ok_or_else(|| {
                        (
                            0,
                            format!("redirect {} without Location", response.status()),
                        )
                    })?;
                location.clone_into(&mut current);
                continue;
            }
            Err(tungstenite::HandshakeError::Failure(failure)) => {
                return Err(tungstenite_failure(&failure));
            }
        };
        let selected = response
            .headers()
            .get("Sec-WebSocket-Protocol")
            .and_then(|value| value.to_str().ok())
            .map(str::to_owned);
        // tungstenite refuses a response that selects none of the offer.
        with_tcp_stream(&mut ws, |tcp| {
            tcp.set_read_timeout(None)?;
            tcp.set_write_timeout(options.io)
        })
        .map_err(|err| connect_failure(&ConnectFailure::from_io("socket timeout", &err)))?;
        return Ok((ws, selected));
    }
    unreachable!("WebSocket redirect loop always returns or continues")
}

/// Refuse a redirect whose transport differs from the first request's.
fn keep_transport(first_tls: &mut Option<bool>, mode: Mode) -> Result<(), ConnectError> {
    let tls = matches!(mode, Mode::Tls);
    if *first_tls.get_or_insert(tls) == tls {
        Ok(())
    } else {
        Err((
            WS_INVALID_ARGUMENT,
            "redirect between ws:// and wss:// refused".to_owned(),
        ))
    }
}

/// Bound the upgrade's socket reads and writes by the time left.
fn bound_upgrade(
    mut stream: MaybeTlsStream<TcpStream>,
    deadline: Option<Instant>,
) -> Result<MaybeTlsStream<TcpStream>, ConnectError> {
    let left = match deadline {
        None => None,
        Some(deadline) => {
            let left = deadline.saturating_duration_since(Instant::now());
            if left.is_zero() {
                return Err((WS_TIMED_OUT, "upgrade timed out".to_owned()));
            }
            Some(left)
        }
    };
    let tcp = match &mut stream {
        MaybeTlsStream::Plain(tcp) => tcp,
        MaybeTlsStream::Rustls(tls) => &mut tls.sock,
        _ => return Ok(stream),
    };
    tcp.set_read_timeout(left)
        .and_then(|()| tcp.set_write_timeout(left))
        .map_err(|err| connect_failure(&ConnectFailure::from_io("upgrade", &err)))?;
    Ok(stream)
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
/// see `prepare_websockets` for details).
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

/// Send one message, recording a failure in the error slot.
fn send_reporting(ws: *mut HewWsConn, message: Message, operation: &str) -> i32 {
    // SAFETY: the caller passes a live connection or null.
    let Some(conn) = (unsafe { ws.as_ref() }) else {
        set_ws_last_error(WS_INVALID_ARGUMENT, format!("{operation}: null connection"));
        return -1;
    };
    let inner = Arc::clone(&conn.inner);
    let sent = if inner.closed.load(Ordering::Acquire) {
        Err(tungstenite::Error::AlreadyClosed)
    } else {
        send_ws_message(&inner, message)
    };
    match sent {
        Ok(()) => {
            clear_ws_last_error();
            0
        }
        Err(err) => {
            set_ws_last_error(ws_errno_of(&err), format!("{operation}: {err}"));
            -1
        }
    }
}

/// Send a text message. Returns 0, or -1 with the error slot set.
///
/// # Safety
///
/// * `ws` must be a valid pointer returned by [`hew_ws_connect`] or
///   `hew_ws_server_accept`, or null.
/// * `msg` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_ws_send_text(ws: *mut HewWsConn, msg: *const HewString) -> i32 {
    // SAFETY: the caller borrows a live managed string; null spells empty.
    let text = unsafe { string_as_str(msg) };
    send_reporting(ws, Message::text(text), "websocket.send_text")
}

/// Send a binary message. Returns 0, or -1 with the error slot set.
///
/// # Safety
///
/// * `ws` must be a valid connection pointer, or null.
/// * `data` must be null or a live `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_send_binary(ws: *mut HewWsConn, data: *const BytesTriple) -> i32 {
    // SAFETY: the caller passes null or a live triple.
    let data = unsafe { tls::bytes_view(data) };
    send_reporting(ws, Message::binary(data.to_vec()), "websocket.send_binary")
}

/// Receive the next text or binary message, waiting at most `deadline_ms`
/// (negative waits indefinitely, zero only checks what has arrived).
///
/// Returns `0` text or `1` binary, with the payload held for
/// [`hew_ws_take_text`] or [`hew_ws_take_binary`]; `2` when the connection
/// closed; `3` when the deadline passed; `4` on failure, with the errno and
/// detail in the error slot.
///
/// # Safety
///
/// `ws` must be a valid connection pointer, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_recv_next(ws: *mut HewWsConn, deadline_ms: i64) -> i32 {
    // SAFETY: the caller passes a live connection or null.
    let Some(conn) = (unsafe { ws.as_ref() }) else {
        set_ws_last_error(
            WS_INVALID_ARGUMENT,
            "websocket.recv: null connection".to_owned(),
        );
        return RECV_FAILED;
    };
    let inner = Arc::clone(&conn.inner);
    let _recv_guard = ActiveCallGuard::new(&inner.active_recvs);
    let deadline = u64::try_from(deadline_ms)
        .ok()
        .map(|ms| Instant::now() + Duration::from_millis(ms));
    let (status, payload) = match receive(&inner, deadline) {
        Received::Text(text) => (RECV_TEXT, text.into_bytes()),
        Received::Binary(data) => (RECV_BINARY, data),
        Received::Closed => (RECV_CLOSED, Vec::new()),
        Received::TimedOut => (RECV_TIMED_OUT, Vec::new()),
        Received::Failed(err) => {
            set_ws_last_error(ws_errno_of(&err), format!("websocket.recv: {err}"));
            (RECV_FAILED, Vec::new())
        }
    };
    *lock_or_recover(&inner.pending) = payload;
    status
}

/// Take the held text payload as a managed string.
///
/// # Safety
///
/// `ws` must be a valid connection pointer, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_take_text(ws: *mut HewWsConn) -> *mut HewString {
    // SAFETY: the caller passes a live connection or null.
    let Some(conn) = (unsafe { ws.as_ref() }) else {
        return std::ptr::null_mut();
    };
    let payload = std::mem::take(&mut *lock_or_recover(&conn.inner.pending));
    // tungstenite validated a Text frame's UTF-8 before returning it.
    string_from_str(std::str::from_utf8(&payload).unwrap_or_default())
}

/// Take the held binary payload as `bytes`.
///
/// # Safety
///
/// `ws` must be a valid connection pointer, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_take_binary(ws: *mut HewWsConn) -> BytesTriple {
    // SAFETY: the caller passes a live connection or null.
    let payload = unsafe { ws.as_ref() }
        .map(|conn| std::mem::take(&mut *lock_or_recover(&conn.inner.pending)))
        .unwrap_or_default();
    let Ok(len) = u32::try_from(payload.len()) else {
        return BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
    };
    // SAFETY: `payload` is valid for `len` bytes; the copy is a fresh owner.
    unsafe { hew_runtime::bytes::hew_bytes_from_static(payload.as_ptr(), len) }
}

/// The subprotocol the server selected, or the empty string.
///
/// # Safety
///
/// `ws` must be a valid connection pointer, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_subprotocol(ws: *const HewWsConn) -> *mut HewString {
    // SAFETY: the caller passes a live connection or null.
    let selected = unsafe { ws.as_ref() }.and_then(|conn| conn.inner.subprotocol.as_deref());
    string_from_str(selected.unwrap_or_default())
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
pub extern "C" fn hew_ws_last_error() -> *mut HewString {
    let detail = hew_runtime::parse_error_slot::get_error(
        hew_runtime::parse_error_slot::ErrorSlotKind::Websocket,
    )
    .unwrap_or_default();
    string_from_str(&detail)
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

// ── WebSocket Attach (Erlang-style active mode) ────────────────────
//
// `hew_ws_attach_native` transfers read authority to a background OS thread that
// delivers frames as actor messages. The caller retains the outer connection
// owner and its send/close authority. The actor never calls recv() — it just
// has receive fns that the runtime invokes. This is Erlang's "active mode"
// pattern.

/// Attach borrowed WebSocket read authority to generated native actor adapters.
///
/// # Safety
/// `ws` is live; callbacks match the destination's protocol.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_attach_native(
    ws: *mut HewWsConn,
    token: NativeActorToken,
    data: AttachCallback,
    close: AttachCallback,
) -> i32 {
    // SAFETY: the caller supplies a live connection, or null for refusal.
    let Some(conn) = (unsafe { ws.as_ref() }) else {
        set_ws_last_error(-1, "websocket.attach: null connection".to_owned());
        return -1;
    };
    // SAFETY: callers supply the generated adapters for this native destination.
    let Some(target) = (unsafe { NativeAttachment::new(token, data, close) }) else {
        set_ws_last_error(-1, "websocket.attach: destination is closed".to_owned());
        return -1;
    };
    match spawn_attach_reader(conn, ActorDelivery::new(target), ws) {
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
    changed: Condvar,
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
        self.changed.notify_all();
        Some(ActiveAcceptGuard { state: self })
    }

    fn cancel_and_wait(&self, cancel: &AtomicBool) {
        let mut count = lock_or_recover(&self.count);
        cancel.store(true, Ordering::Release);
        while *count != 0 {
            count = self
                .changed
                .wait(count)
                .unwrap_or_else(PoisonError::into_inner);
        }
    }

    #[cfg(test)]
    fn count(&self) -> usize {
        *lock_or_recover(&self.count)
    }

    #[cfg(test)]
    fn wait_for_count(&self, expected: usize) {
        let mut count = lock_or_recover(&self.count);
        while *count != expected {
            count = self
                .changed
                .wait(count)
                .unwrap_or_else(PoisonError::into_inner);
        }
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
        self.state.changed.notify_all();
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

fn set_server_websocket_blocking(ws: &mut WebSocket<MaybeTlsStream<TcpStream>>) -> io::Result<()> {
    match ws.get_mut() {
        MaybeTlsStream::Plain(stream) => stream.set_nonblocking(false),
        MaybeTlsStream::Rustls(stream) => stream.sock.set_nonblocking(false),
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
/// `addr` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_ws_server_new(addr: *const HewString) -> *mut HewWsServer {
    if addr.is_null() {
        set_ws_last_error(-1, "websocket.listen: address is null".to_owned());
        return std::ptr::null_mut();
    }
    // SAFETY: the caller borrows a live managed string.
    let addr_str = unsafe { string_as_str(addr) };
    if addr_str.contains('\0') {
        set_ws_last_error(-1, "websocket.listen: address contains NUL".to_owned());
        return std::ptr::null_mut();
    }
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
                Box::into_raw(Box::new(HewWsConn::new(*ws, Role::Server, None)))
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
    use crate::test_string::ManagedString;
    use hew_runtime::{actor, transport};
    use std::collections::HashMap;
    #[cfg(unix)]
    use std::os::fd::AsRawFd;
    use std::sync::atomic::AtomicU64;
    use std::sync::mpsc::{self, Receiver, Sender};
    use std::sync::Barrier;
    use std::sync::OnceLock;

    const TEST_STOP_TYPE: i32 = 101;
    const TEST_CRASH_TYPE: i32 = 102;

    fn ws_last_error_text() -> String {
        let ptr = hew_ws_last_error();
        // SAFETY: accessor returns an owned managed string.
        let text = unsafe { string_as_str(ptr) }.to_owned();
        // SAFETY: pointer came from hew_ws_last_error.
        unsafe { hew_cabi::string::string_release(ptr) };
        text
    }

    #[test]
    fn prepared_reader_preserves_frame_buffered_during_handshake() {
        let listener = TcpListener::bind("127.0.0.1:0").expect("listen");
        let peer = TcpStream::connect(listener.local_addr().expect("address")).expect("connect");
        let (stream, _) = listener.accept().expect("accept");
        stream
            .set_read_timeout(Some(Duration::from_millis(50)))
            .expect("read deadline");
        // This is the framer state produced when an HTTP upgrade and the first
        // server text frame arrive in one read. The peer sends no further bytes.
        let ws = WebSocket::from_partially_read(
            MaybeTlsStream::Plain(stream),
            b"\x81\x05ready".to_vec(),
            Role::Client,
            None,
        );
        let (mut reader, _, _) = prepare_websockets(ws, Role::Client);
        assert_eq!(
            reader.read().expect("buffered frame"),
            Message::Text("ready".into())
        );
        drop(peer);
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
                let conn = unsafe {
                    hew_ws_connect(
                        std::ptr::null(),
                        std::ptr::null(),
                        0,
                        std::ptr::null(),
                        0,
                        0,
                    )
                };
                assert!(conn.is_null());
            });
            barrier.wait();
            assert_eq!(
                worker.join().expect("worker should read actor error"),
                (
                    -1,
                    "websocket.connect: HTTP format error: empty string".to_owned()
                )
            );

            with_actor_context(actor, || {
                assert_eq!(hew_ws_last_errno(), -1);
                assert_eq!(
                    ws_last_error_text(),
                    "websocket.connect: HTTP format error: empty string"
                );
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
        if let Some(targets) = WS_TARGETS.get() {
            targets.lock().unwrap().retain(|_, id| *id != test_id);
        }
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

    static WS_TARGETS: OnceLock<Mutex<HashMap<usize, u64>>> = OnceLock::new();

    unsafe extern "C" fn ws_test_data(token: usize, data: *const u8, len: usize) -> i32 {
        let test_id = WS_TARGETS.get().unwrap().lock().unwrap()[&token];
        // SAFETY: tungstenite supplied a borrowed UTF-8 text frame for this call.
        let bytes = unsafe { std::slice::from_raw_parts(data, len) };
        let text = std::str::from_utf8(bytes).unwrap().to_owned();
        send_actor_event(test_id, ActorEvent::Message(text));
        0
    }

    unsafe extern "C" fn ws_test_close(token: usize, _: *const u8, _: usize) -> i32 {
        let test_id = WS_TARGETS.get().unwrap().lock().unwrap()[&token];
        send_actor_event(test_id, ActorEvent::Closed);
        0
    }

    unsafe fn attach_ws_for_test(conn: *mut HewWsConn, actor: *mut c_void) -> i32 {
        let token = if actor.is_null() {
            NativeActorToken::INVALID
        } else {
            // SAFETY: each test owns this live actor until attachment completes.
            let actor = unsafe { &*actor.cast::<actor::HewActor>() };
            let state = unsafe { &*actor.state.cast::<TestActorState>() };
            WS_TARGETS
                .get_or_init(|| Mutex::new(HashMap::new()))
                .lock()
                .unwrap()
                .insert(actor.local_pid_id.as_usize(), state.test_id);
            actor.local_pid_id
        };
        unsafe { hew_ws_attach_native(conn, token, ws_test_data, ws_test_close) }
    }

    unsafe extern "C-unwind" fn websocket_test_dispatch(
        _ctx: *mut hew_runtime::HewExecutionContext,
        _state: *mut c_void,
        msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        match msg_type {
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
            .arg(format!("websocket::tests::{test_name}"))
            .arg("--exact")
            .arg("--nocapture")
            .arg("--test-threads=1")
            .env(env_key, "1");
        for (key, value) in extra_env {
            command.env(key, value);
        }
        let output = command.output().expect("spawn isolated test process");

        assert!(
            output.status.success() && String::from_utf8_lossy(&output.stdout).contains("1 passed"),
            "isolated test process failed for {test_name} (status: {:?})\nstdout:\n{}\nstderr:\n{}",
            output.status.code(),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );
    }

    fn run_in_isolated_test_process(test_name: &str, env_key: &str, body: impl FnOnce()) {
        run_in_isolated_test_process_with_env(test_name, env_key, &[], body);
    }

    /// Poll `condition` until it holds; the test runner's timeout is the hang
    /// guard.
    fn wait_until(mut condition: impl FnMut() -> bool) {
        while !condition() {
            std::thread::sleep(Duration::from_millis(10));
        }
    }

    fn wait_for_reader_exit(inner: &Arc<HewWsConnInner>) {
        wait_until(|| {
            inner
                .reader
                .lock()
                .expect("reader mutex poisoned")
                .as_ref()
                .is_some_and(|reader| reader.exited.load(Ordering::Acquire))
        });
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

    fn wait_for_actor_dead(actor: *mut actor::HewActor) {
        // SAFETY: tests call this only for actors they spawned and still own.
        let actor_ref = unsafe { transport::hew_actor_ref_local(actor) };
        wait_until(|| unsafe { transport::hew_actor_ref_is_alive(&raw const actor_ref) == 0 });
    }

    fn recv_event(rx: &Receiver<ActorEvent>) -> ActorEvent {
        rx.recv()
            .unwrap_or_else(|err| panic!("expected an actor event: {err:?}"))
    }

    fn attach_test_conn() -> (
        *mut HewWsServer,
        *mut HewWsConn,
        tungstenite::WebSocket<MaybeTlsStream<TcpStream>>,
    ) {
        // SAFETY: valid C string literal for bind address.
        let server = unsafe { hew_ws_server_new(ManagedString::new("127.0.0.1:0").as_ptr()) };
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

    fn set_peer_read_timeout(
        ws: &mut tungstenite::WebSocket<MaybeTlsStream<TcpStream>>,
        timeout: Duration,
    ) {
        match ws.get_mut() {
            MaybeTlsStream::Plain(stream) => stream
                .set_read_timeout(Some(timeout))
                .expect("set peer read timeout"),
            MaybeTlsStream::Rustls(stream) => stream
                .sock
                .set_read_timeout(Some(timeout))
                .expect("set peer read timeout"),
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
        let attach_status = unsafe { attach_ws_for_test(conn, actor.cast()) };
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
    fn attach_null_handles_preserve_typed_error_authority() {
        // SAFETY: null handles exercise the guarded error path.
        let status = unsafe { attach_ws_for_test(std::ptr::null_mut(), std::ptr::null_mut()) };

        assert_eq!(status, -1);
        assert_eq!(hew_ws_last_errno(), -1);
        assert_eq!(ws_last_error_text(), "websocket.attach: null connection");
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
                let status = unsafe { attach_ws_for_test(conn, actor.cast()) };

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
        let url = ManagedString::new(format!("ws://127.0.0.1:{port}/"));
        // SAFETY: url is a live managed string.
        let conn =
            unsafe { hew_ws_connect(url.as_ptr(), std::ptr::null(), 0, std::ptr::null(), 0, 0) };
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
        let conn = unsafe {
            hew_ws_connect(
                std::ptr::null(),
                std::ptr::null(),
                0,
                std::ptr::null(),
                0,
                0,
            )
        };
        assert!(conn.is_null());
    }

    #[test]
    fn write_operation_gate_spans_short_write_drain() {
        let gate = Arc::new(PlMutex::new(()));
        let log = Arc::new(Mutex::new(Vec::new()));
        let ready = Arc::new(Barrier::new(2));
        let (first_short_write_tx, first_short_write_rx) = mpsc::channel();
        let (pong_attempt_tx, pong_attempt_rx) = mpsc::channel();

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
                        // Keep the operation gate held until the competing
                        // Pong writer is trying to run. Without
                        // operation-level serialization it would write here
                        // and produce USERPONGFRAME.
                        pong_attempt_rx.recv().expect("pong writer attempts");
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
                .recv()
                .expect("first short user write should happen");
            pong_attempt_tx.send(()).expect("user writer waits");
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

    /// `hew_ws_send_text` with null ws returns -1.
    #[test]
    fn send_text_null_ws_returns_error() {
        let msg = ManagedString::new("hello");
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
        let data = BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
        assert_eq!(
            // SAFETY: null ws is explicitly handled.
            unsafe { hew_ws_send_binary(std::ptr::null_mut(), &raw const data) },
            -1
        );
    }

    /// `hew_ws_recv_next` with null ws fails without touching memory.
    #[test]
    fn recv_null_ws_fails() {
        // SAFETY: null ws is explicitly handled.
        assert_eq!(
            unsafe { hew_ws_recv_next(std::ptr::null_mut(), 0) },
            RECV_FAILED
        );
    }

    /// `hew_ws_close` with null ws is a no-op.
    #[test]
    fn close_null_ws_is_noop() {
        // SAFETY: null ws is explicitly handled.
        unsafe { hew_ws_close(std::ptr::null_mut()) };
    }

    /// `hew_ws_recv_next` reports the deadline when the peer accepts the
    /// WebSocket but never sends anything, and a second call times out
    /// again rather than blocking on a lock the first left held.
    #[test]
    fn ws_recv_timeout_fires_on_silent_peer() {
        let (server, conn, _client) = attach_test_conn();
        // SAFETY: conn is a valid HewWsConn pointer.
        assert_eq!(unsafe { hew_ws_recv_next(conn, 200) }, RECV_TIMED_OUT);
        // SAFETY: conn is a valid HewWsConn pointer.
        assert_eq!(unsafe { hew_ws_recv_next(conn, 100) }, RECV_TIMED_OUT);
        // SAFETY: conn is a valid HewWsConn pointer.
        assert_eq!(unsafe { hew_ws_recv_next(conn, 0) }, RECV_TIMED_OUT);

        // SAFETY: conn and server are valid; close is idempotent.
        unsafe { hew_ws_close(conn) };
        unsafe { hew_ws_server_close(server) };
    }

    /// connect with an HTTP URL (not ws://) returns null.
    #[test]
    fn connect_http_url_returns_null() {
        let url = ManagedString::new("http://127.0.0.1:1/path");
        // SAFETY: url is a live managed string.
        let conn =
            unsafe { hew_ws_connect(url.as_ptr(), std::ptr::null(), 0, std::ptr::null(), 0, 0) };
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
        let url = ManagedString::new("not a url");
        // SAFETY: url is a live managed string.
        let conn =
            unsafe { hew_ws_connect(url.as_ptr(), std::ptr::null(), 0, std::ptr::null(), 0, 0) };
        assert!(conn.is_null(), "malformed URL should fail");
        assert_eq!(hew_ws_last_errno(), -1);
        assert_eq!(
            ws_last_error_text(),
            "websocket.connect: HTTP format error: invalid uri character"
        );
        assert_eq!(hew_ws_last_errno(), -1);
    }

    /// A redirect that changes the transport is refused before it is
    /// followed, in either direction; the `wss://` to `ws://` case would hand
    /// the upgrade (and anything sent after it) to a plaintext peer.
    #[test]
    fn redirect_changing_transport_is_refused() {
        use std::io::{Read, Write};
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        let server = std::thread::spawn(move || {
            let (mut peer, _) = listener.accept().unwrap();
            let mut request = Vec::new();
            let mut chunk = [0u8; 1024];
            while !request.windows(4).any(|window| window == b"\r\n\r\n") {
                let count = peer.read(&mut chunk).unwrap();
                assert_ne!(count, 0, "client closed before its request ended");
                request.extend_from_slice(&chunk[..count]);
            }
            let reply = format!(
                "HTTP/1.1 301 Moved Permanently\r\nLocation: wss://127.0.0.1:{port}/\r\nContent-Length: 0\r\n\r\n"
            );
            peer.write_all(reply.as_bytes()).unwrap();
            // A followed redirect would connect again; none may arrive.
            listener.set_nonblocking(true).unwrap();
            std::thread::sleep(Duration::from_millis(200));
            assert!(listener.accept().is_err(), "the redirect was followed");
        });
        let options = ConnectOptions {
            subprotocols: Vec::new(),
            trust: Trust::Bundled,
            handshake: Some(Duration::from_secs(5)),
            io: None,
        };
        let result = connect_client(
            &format!("ws://127.0.0.1:{port}/"),
            WebSocketConfig::default(),
            3,
            &options,
        );
        let Err((errno, detail)) = result else {
            panic!("a ws:// to wss:// redirect must be refused");
        };
        assert_eq!(errno, WS_INVALID_ARGUMENT);
        assert_eq!(detail, "redirect between ws:// and wss:// refused");
        server.join().unwrap();
    }

    /// connect with an empty string returns null.
    #[test]
    fn connect_empty_url_returns_null() {
        let url = ManagedString::new("");
        // SAFETY: url is a live managed string.
        let conn =
            unsafe { hew_ws_connect(url.as_ptr(), std::ptr::null(), 0, std::ptr::null(), 0, 0) };
        assert!(conn.is_null(), "empty URL should fail");
        assert_eq!(hew_ws_last_errno(), -1);
        assert!(!ws_last_error_text().is_empty());
    }

    // ── Server tests ────────────────────────────────────────────────

    /// Server listens, client connects, exchanges a message, closes.
    #[test]
    fn server_accept_and_echo() {
        // SAFETY: the managed string temporary lives throughout this call.
        let server = unsafe { hew_ws_server_new(ManagedString::new("127.0.0.1:0").as_ptr()) };
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
            for text in ["", "café\0雪"] {
                ws.send(Message::text(text)).expect("client send");
                let reply = ws.read().expect("client read");
                assert_eq!(reply, Message::Text(format!("echo: {text}").into()));
            }
            ws.close(None).ok();
            // Drain remaining frames so close handshake completes.
            while ws.read().is_ok() {}
        });

        // SAFETY: server is a valid pointer returned by hew_ws_server_new.
        let conn = unsafe { hew_ws_server_accept(server) };
        assert!(!conn.is_null(), "accept should succeed");

        for expected in ["", "café\0雪"] {
            // SAFETY: conn is a valid pointer returned by hew_ws_server_accept.
            assert_eq!(unsafe { hew_ws_recv_next(conn, -1) }, RECV_TEXT);
            // SAFETY: conn holds the received text until taken.
            let text_value = unsafe { hew_ws_take_text(conn) };
            // SAFETY: text_value remains owned until released after sending the echo.
            let text = unsafe { string_as_str(text_value) };
            assert_eq!(text, expected);

            // Echo back.
            let echo = ManagedString::new(format!("echo: {text}"));
            // SAFETY: conn is valid; echo is a live managed string.
            let rc = unsafe { hew_ws_send_text(conn, echo.as_ptr()) };
            assert_eq!(rc, 0, "send should succeed");

            // SAFETY: text_value is the outstanding managed string result.
            unsafe { hew_cabi::string::string_release(text_value) };
        }
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
                // SAFETY: the managed string temporary lives throughout this call.
                let server =
                    unsafe { hew_ws_server_new(ManagedString::new("127.0.0.1:0").as_ptr()) };
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
                // SAFETY: the managed string temporary lives throughout this call.
                let server =
                    unsafe { hew_ws_server_new(ManagedString::new("127.0.0.1:0").as_ptr()) };
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
                // SAFETY: the managed string temporary lives throughout this call.
                let server =
                    unsafe { hew_ws_server_new(ManagedString::new("127.0.0.1:0").as_ptr()) };
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
        let server = unsafe { hew_ws_server_new(ManagedString::new("127.0.0.1:0").as_ptr()) };
        assert!(!server.is_null(), "server should bind successfully");

        // SAFETY: `server` stays live until hew_ws_server_close below sets cancel.
        let inner = unsafe { &*server }.inner.clone();
        let server_addr = server as usize;
        let accept_thread = std::thread::spawn(move || unsafe {
            hew_ws_server_accept(server_addr as *mut HewWsServer) as usize
        });

        inner.active_accepts.wait_for_count(1);

        unsafe { hew_ws_server_close(server) };

        // join() is the exact synchronization for the thread's exit; a stuck
        // cancel fails the run via the harness's per-test timeout.
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
        let server = unsafe { hew_ws_server_new(ManagedString::new("127.0.0.1:0").as_ptr()) };
        assert!(!server.is_null(), "server should bind successfully");
        let port = unsafe { hew_ws_server_port(server) };
        let inner = unsafe { &*server }.inner.clone();
        let server_addr = server as usize;
        let accept_thread = std::thread::spawn(move || unsafe {
            hew_ws_server_accept(server_addr as *mut HewWsServer) as usize
        });

        let stalled_peer =
            TcpStream::connect(("127.0.0.1", u16::try_from(port).unwrap())).expect("tcp connect");
        // The accepted peer enters the handshake.
        wait_until(|| inner.active_handshakes.load(Ordering::Acquire) == 1);

        // The peer never handshakes, so close returns only by cancelling it.
        unsafe { hew_ws_server_close(server) };
        // close drained the accept authority before returning (count == 0 is
        // deterministic); join() is the exact synchronization for the accept
        // thread's exit — a stuck close fails via the harness's per-test timeout.
        assert_eq!(inner.active_accepts.count(), 0);
        assert_eq!(inner.active_handshakes.load(Ordering::Acquire), 0);
        assert_eq!(accept_thread.join().expect("accept thread should join"), 0);
        drop(stalled_peer);
    }

    #[test]
    fn stalled_handshake_expires_and_next_valid_peer_is_accepted() {
        let server = unsafe { hew_ws_server_new(ManagedString::new("127.0.0.1:0").as_ptr()) };
        assert!(!server.is_null(), "server should bind successfully");
        let port = unsafe { hew_ws_server_port(server) };
        let inner = unsafe { &*server }.inner.clone();
        let server_addr = server as usize;
        let accept_thread = std::thread::spawn(move || unsafe {
            hew_ws_server_accept(server_addr as *mut HewWsServer) as usize
        });

        let stalled_peer =
            TcpStream::connect(("127.0.0.1", u16::try_from(port).unwrap())).expect("tcp connect");
        // The accepted peer enters the handshake.
        wait_until(|| inner.active_handshakes.load(Ordering::Acquire) == 1);
        // A peer that never handshakes expires; one that never did would hang.
        wait_until(|| inner.active_handshakes.load(Ordering::Acquire) == 0);

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
        let server = unsafe { hew_ws_server_new(ManagedString::new("127.0.0.1:0").as_ptr()) };
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
        // Partial peer should enter the handshake.
        wait_until(|| inner.active_handshakes.load(Ordering::Acquire) == 1);
        drop(partial);
        // Disconnecting the partial peer should end its handshake.
        wait_until(|| inner.active_handshakes.load(Ordering::Acquire) == 0);

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
        let server = unsafe { hew_ws_server_new(ManagedString::new("127.0.0.1:0").as_ptr()) };
        assert!(!server.is_null(), "server should bind successfully");
        let port = unsafe { hew_ws_server_port(server) };
        let inner = unsafe { &*server }.inner.clone();
        let server_addr = server as usize;
        let accept_thread = std::thread::spawn(move || unsafe {
            hew_ws_server_accept(server_addr as *mut HewWsServer) as usize
        });

        let mut peer =
            TcpStream::connect(("127.0.0.1", u16::try_from(port).unwrap())).expect("tcp connect");
        // Peer should enter the handshake before the race.
        wait_until(|| inner.active_handshakes.load(Ordering::Acquire) == 1);
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
        // join() is the exact synchronization for the racing accept's exit; a
        // close that never resolves it fails via the harness's per-test timeout.
        let conn = accept_thread.join().expect("accept thread should join") as *mut HewWsConn;
        if !conn.is_null() {
            unsafe { hew_ws_close(conn) };
        }
        drop(peer);
    }

    #[test]
    fn repeated_bind_accept_close_drains_all_accept_authority() {
        for _ in 0..64 {
            let server = unsafe { hew_ws_server_new(ManagedString::new("127.0.0.1:0").as_ptr()) };
            assert!(!server.is_null(), "server should bind successfully");
            let inner = unsafe { &*server }.inner.clone();
            let weak_inner = Arc::downgrade(&inner);
            let server_addr = server as usize;
            let accept_thread = std::thread::spawn(move || unsafe {
                hew_ws_server_accept(server_addr as *mut HewWsServer) as usize
            });
            inner.active_accepts.wait_for_count(1);

            unsafe { hew_ws_server_close(server) };
            // close is synchronous: cancel_and_wait only returns once every
            // accept guard has dropped, so the accept authority is fully drained
            // the instant close returns (count == 0 is the deterministic proof).
            // The OS thread that ran the accept unwinds a few instructions later;
            // join() is the exact event-driven synchronization for that exit —
            // no poll, no scheduling-lag assumption. A thread that never exits
            // fails the run via the harness's per-test timeout.
            assert_eq!(inner.active_accepts.count(), 0);
            assert_eq!(inner.active_handshakes.load(Ordering::Acquire), 0);
            assert_eq!(accept_thread.join().expect("accept thread should join"), 0);
            drop(inner);
            assert!(
                weak_inner.upgrade().is_none(),
                "server inner allocation must be reclaimed after each close"
            );
        }
        // No wall-clock bound: every iteration already asserts the deterministic
        // facts this test exists to prove — the accept authority is drained
        // (count == 0) and the handshake count is zero the instant close
        // returns, the accept thread joins, and the server allocation is
        // reclaimed. A close that fails to make progress cannot pass those
        // asserts, and a hang is caught by the harness per-test timeout, so a
        // duration assert only added a contended-runner failure mode.
    }

    #[test]
    fn recv_cancel_returns_cancelled() {
        let (server, conn, client) = attach_test_conn();
        // SAFETY: tests keep `conn` live until the recv thread exits.
        let inner = unsafe { &*conn }.inner.clone();
        let recv_thread = std::thread::spawn(move || {
            let _recv_guard = ActiveCallGuard::new(&inner.active_recvs);
            matches!(receive(&inner, None), Received::Closed)
        });

        // Recv loop should become active before cancellation.
        wait_until(|| {
            // SAFETY: `conn` remains live until hew_ws_close below.
            unsafe { &*conn }.inner.active_recvs.load(Ordering::Acquire) == 1
        });

        unsafe { hew_ws_close(conn) };

        // join() is the exact synchronization for the recv thread's exit; a
        // stuck cancel fails via the harness's per-test timeout.
        assert!(
            recv_thread.join().expect("recv thread should join"),
            "a receive cancelled by close reports the connection closed"
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
                let server =
                    unsafe { hew_ws_server_new(ManagedString::new("127.0.0.1:0").as_ptr()) };
                assert!(!server.is_null(), "server should bind successfully");

                // SAFETY: `server` stays live until the owner actor closes it.
                let inner = unsafe { &*server }.inner.clone();
                let server_addr = server as usize;
                let accept_thread = std::thread::spawn(move || unsafe {
                    hew_ws_server_accept(server_addr as *mut HewWsServer) as usize
                });

                inner.active_accepts.wait_for_count(1);

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

                // owner actor should stop after closing the server
                wait_for_actor_dead(actor);
                // join() is the exact synchronization for the accept thread's
                // exit; a stuck cancel fails via the harness's per-test timeout.
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
                    let data = BytesTriple {
                        ptr: payload.as_ptr().cast_mut(),
                        offset: 0,
                        len: u32::try_from(payload.len()).expect("payload fits u32"),
                    };
                    unsafe { hew_ws_send_binary(conn_addr as *mut HewWsConn, &raw const data) }
                });
                started_rx.recv().expect("send thread should start");
                std::thread::sleep(Duration::from_millis(25));

                client
                    .send(Message::Ping(b"race".to_vec().into()))
                    .expect("client ping should send during large send");

                let mut saw_binary = false;
                let mut saw_pong = false;
                while !(saw_binary && saw_pong) {
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

                // Reader should exit within the cancel deadline.
                wait_until(|| {
                    inner
                        .reader
                        .lock()
                        .expect("reader mutex poisoned")
                        .as_ref()
                        .is_none_or(|reader| reader.exited.load(Ordering::Acquire))
                });
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

                // actor should transition to a non-live state
                wait_for_actor_dead(actor);
                // reader should exit within the bounded deadline after actor stop
                wait_for_reader_exit(&inner);
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

                // crashed actor should become non-live
                wait_for_actor_dead(actor);
                // reader should exit within the bounded deadline after actor crash
                wait_for_reader_exit(&inner);
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
                // forced actor stop should transition the actor to non-live
                wait_for_actor_dead(actor);
                // reader should exit after forced actor stop
                wait_for_reader_exit(&inner);
                assert!(
                    delivery.is_revoked(),
                    "reader exit must discard the attachment delivery authority"
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

                unsafe { hew_ws_close(conn) };
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
                // Once the reader has exited, nothing is left to deliver the
                // late frame.
                wait_for_reader_exit(&inner);
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

                // reader should exit promptly when the attached connection closes
                wait_for_reader_exit(&inner);
                assert!(
                    delivery.is_revoked(),
                    "close must synchronously revoke actor delivery"
                );
                let runtime_calls_after_close = ACTOR_RUNTIME_CALLS.load(Ordering::Relaxed);
                // The reader has exited, so nothing can arrive after close.
                assert!(rx.try_recv().is_err(), "no event may follow close");
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
                    recv_event(&rx),
                    ActorEvent::Closed,
                    "remote close should notify the actor exactly once"
                );
                // reader should exit after the remote close handshake
                wait_for_reader_exit(&inner);
                assert!(delivery.is_revoked());
                // The reader has exited, so no second close can follow.
                assert!(rx.try_recv().is_err(), "remote close must notify once");

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

                unsafe { actor::hew_actor_send(actor1, TEST_STOP_TYPE, std::ptr::null_mut(), 0) };
                // first actor should stop
                wait_for_actor_dead(actor1);
                // first reader should exit after its actor stops
                wait_for_reader_exit(&inner1);
                // The second reader stays live: the frame below still reaches
                // its actor.

                client2
                    .send(Message::text("still-alive"))
                    .expect("second client send");
                assert_eq!(
                    recv_event(&rx2),
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
        let addr = ManagedString::new("not an address");
        // SAFETY: addr is a live managed string.
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

    // ── Client options: WSS, subprotocols, deadlines ─────────────────

    /// Serve one WebSocket upgrade over TLS, selecting `select` from the
    /// client's offer, and echo one message.
    fn serve_wss_once(
        config: Arc<rustls::ServerConfig>,
        select: Option<&'static str>,
    ) -> (u16, std::thread::JoinHandle<()>) {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("addr").port();
        let server = std::thread::spawn(move || {
            let (tcp, _) = listener.accept().expect("accept");
            tcp.set_read_timeout(Some(Duration::from_secs(5)))
                .expect("timeout");
            let tls = rustls::StreamOwned::new(
                rustls::ServerConnection::new(config).expect("server connection"),
                tcp,
            );
            #[expect(
                clippy::result_large_err,
                reason = "tungstenite fixes the handshake callback's error type"
            )]
            let callback =
                |request: &tungstenite::handshake::server::Request,
                 mut response: tungstenite::handshake::server::Response| {
                    let offered = request
                        .headers()
                        .get("Sec-WebSocket-Protocol")
                        .and_then(|value| value.to_str().ok())
                        .unwrap_or_default()
                        .to_owned();
                    if let Some(chosen) = select.filter(|chosen| offered.contains(chosen)) {
                        response
                            .headers_mut()
                            .insert("Sec-WebSocket-Protocol", chosen.parse().expect("header"));
                    }
                    Ok(response)
                };
            let Ok(mut ws) = tungstenite::accept_hdr(tls, callback) else {
                return;
            };
            if let Ok(message) = ws.read() {
                let _ = ws.send(message);
            }
            let _ = ws.close(None);
            let _ = ws.flush();
        });
        (port, server)
    }

    fn connect_options(
        url: &str,
        protocols: &str,
        pem: &[u8],
        handshake_ms: i64,
    ) -> *mut HewWsConn {
        let url = ManagedString::new(url);
        let protocols = ManagedString::new(protocols);
        let pem = BytesTriple {
            ptr: pem.as_ptr().cast_mut(),
            offset: 0,
            len: u32::try_from(pem.len()).expect("pem fits u32"),
        };
        // SAFETY: every argument outlives the call.
        unsafe {
            hew_ws_connect(
                url.as_ptr(),
                protocols.as_ptr(),
                1,
                &raw const pem,
                handshake_ms,
                0,
            )
        }
    }

    #[test]
    fn wss_with_pem_trust_negotiates_a_subprotocol_and_echoes_binary() {
        let _runtime = NetErrorSlotRuntimeGuard::new();
        let (ca_pem, config) = crate::tls::tests::ca_and_server();
        let (port, server) = serve_wss_once(config, Some("mqtt"));
        let conn = connect_options(
            &format!("wss://localhost:{port}/mqtt"),
            "mqttv5,mqtt",
            ca_pem.as_bytes(),
            5_000,
        );
        assert!(!conn.is_null(), "connect failed: {}", ws_last_error_text());
        // SAFETY: conn is live until closed below.
        let selected = unsafe { hew_ws_subprotocol(conn) };
        // SAFETY: selected is a fresh managed string released below.
        assert_eq!(unsafe { string_as_str(selected) }, "mqtt");
        unsafe { hew_cabi::string::string_release(selected) };
        let payload = [0xC0u8, 0x00];
        let data = BytesTriple {
            ptr: payload.as_ptr().cast_mut(),
            offset: 0,
            len: 2,
        };
        // SAFETY: conn and data are live.
        assert_eq!(unsafe { hew_ws_send_binary(conn, &raw const data) }, 0);
        // SAFETY: conn is live.
        assert_eq!(unsafe { hew_ws_recv_next(conn, 5_000) }, RECV_BINARY);
        // SAFETY: conn holds the binary payload.
        let echoed = unsafe { hew_ws_take_binary(conn) };
        assert_eq!(echoed.len, 2);
        unsafe { hew_runtime::bytes::hew_bytes_drop(echoed.ptr) };
        // SAFETY: conn is live.
        assert_eq!(unsafe { hew_ws_recv_next(conn, 5_000) }, RECV_CLOSED);
        unsafe { hew_ws_close(conn) };
        server.join().expect("server thread");
    }

    #[test]
    fn unselected_subprotocol_and_untrusted_server_fail_the_connect() {
        let _runtime = NetErrorSlotRuntimeGuard::new();
        let (ca_pem, config) = crate::tls::tests::ca_and_server();
        let (port, server) = serve_wss_once(Arc::clone(&config), None);
        let conn = connect_options(
            &format!("wss://localhost:{port}/"),
            "mqtt",
            ca_pem.as_bytes(),
            5_000,
        );
        assert!(conn.is_null());
        assert!(
            ws_last_error_text().contains("SubProtocol error"),
            "{}",
            ws_last_error_text()
        );
        server.join().expect("server thread");

        let (port, server) = serve_wss_once(config, None);
        let url = ManagedString::new(format!("wss://localhost:{port}/"));
        // SAFETY: url is live; bundled trust does not know the test CA.
        let conn = unsafe {
            hew_ws_connect(
                url.as_ptr(),
                std::ptr::null(),
                0,
                std::ptr::null(),
                5_000,
                0,
            )
        };
        assert!(conn.is_null());
        assert!(
            ws_last_error_text().contains("invalid peer certificate"),
            "{}",
            ws_last_error_text()
        );
        server.join().expect("server thread");
    }

    #[test]
    fn handshake_deadline_bounds_a_silent_server() {
        let _runtime = NetErrorSlotRuntimeGuard::new();
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("addr").port();
        let started = Instant::now();
        let conn = connect_options(&format!("ws://127.0.0.1:{port}/"), "", &[], 300);
        assert!(conn.is_null());
        assert_eq!(hew_ws_last_errno(), WS_TIMED_OUT);
        assert!(started.elapsed() >= Duration::from_millis(250));
        drop(listener);
    }
}
