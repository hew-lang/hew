//! Hew `std::net::tls` — TLS client connections.
//!
//! Exposes a blocking C ABI over rustls for TLS client connections.
//! All `extern "C"` functions are designed to be called from compiled Hew
//! programs via FFI. Any string returned by [`hew_tls_last_error`] is allocated
//! with `libc::malloc`; callers must free it with `libc::free`.
use std::ffi::c_void;
use std::io::{self, Read, Write};
use std::net::TcpStream;
use std::os::raw::{c_char, c_int};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, PoisonError};
use std::thread::JoinHandle;

use hew_cabi::cabi::{alloc_cstring, cstr_to_str, str_to_malloc};
use hew_runtime::bytes::{hew_bytes_from_static, BytesTriple};
use rustls::pki_types::ServerName;
use rustls::RootCertStore;

type BoxError = Box<dyn std::error::Error + Send + Sync + 'static>;

/// Maximum bytes read in a single [`hew_tls_read`] call.
const READ_BUFFER_SIZE: usize = 65_536;
const TLS_STATUS_SUCCESS: c_int = 0;
const TLS_STATUS_RETRYABLE: c_int = 1;
const TLS_STATUS_TLS_ERROR: c_int = 2;
const TLS_STATUS_IO_ERROR: c_int = 3;

// ── Opaque handle ─────────────────────────────────────────────────────────────

/// TLS stream inner state, shared between the caller and any attached reader
/// thread via an [`Arc<Mutex<...>>`].
///
/// WHY the inner/outer split: `hew_tls_attach` spawns a reader thread that
/// owns read access. The caller retains write access through the outer pointer.
/// `rustls::StreamOwned` requires `&mut self` for both directions so we hold
/// it behind a single `Mutex` and take the lock for every I/O call. Both sides
/// observe the same `closed` flag; the reader exits when it sees it set.
///
/// WHEN obsolete: if/when rustls supports a split-half (read-half/write-half)
/// API we can drop the mutex and hold the halves separately.
///
/// WHAT the real solution looks like: `rustls` v0.23+ exposes
/// `ConnectionCommon::split_io` for exactly this use-case.
///
/// FOLLOW-UP (#1324 write head-of-line stall, deferred): the attached reader
/// holds this single mutex across its blocking `read` (up to
/// `TLS_READER_TIMEOUT`), so a concurrent `hew_tls_write` stalls behind the
/// in-flight read for up to that bound. The WebSocket lane split the write half
/// to kill exactly this; the TLS split awaits the `split_io` migration above.
/// Functionally correct today — this is a latency bound, not a correctness gap.
type TlsInner = Arc<TlsShared>;

pub struct TlsShared {
    stream: Mutex<Option<rustls::StreamOwned<rustls::ClientConnection, TcpStream>>>,
    closed: AtomicBool,
    /// Join handle for the background reader spawned by [`hew_tls_attach`].
    ///
    /// WHY: a detached reader can keep calling into the runtime
    /// (`hew_actor_ref_is_alive`, `hew_actor_try_send`) for up to
    /// `TLS_READER_TIMEOUT` after `hew_tls_close`, and as a detached thread can
    /// outlive the actor/runtime at teardown (#1963 reader-lifetime class).
    /// `hew_tls_close` joins this handle before returning so the reader is
    /// deterministically reaped — mirroring `websocket.rs::join_reader`.
    ///
    /// ATOMICITY: the check-then-store in `hew_tls_attach` holds this lock
    /// across both operations so two concurrent callers cannot both pass the
    /// `is_some()` guard and both spawn a reader.
    reader: Mutex<Option<JoinHandle<()>>>,
    /// Set to `true` (Release) by the reader thread just before it exits.
    ///
    /// WHY: `hew_tls_close` joins the reader, so this flag is guaranteed to be
    /// `true` the instant `hew_tls_close` returns — but only if `join.join()` is
    /// called (not `drop(join)`). Tests assert this flag immediately after
    /// `hew_tls_close` to prove the join happened rather than a silent detach.
    reader_exited: AtomicBool,
}

impl std::fmt::Debug for TlsShared {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TlsShared")
            .field("closed", &self.closed)
            .finish_non_exhaustive()
    }
}

/// An established TLS connection.
///
/// The inner state is `Arc`-shared so an attached reader thread can hold a
/// reference while the caller retains write access.
/// Must be closed with [`hew_tls_close`].
#[derive(Debug)]
pub struct HewTlsStream {
    inner: TlsInner,
}

impl HewTlsStream {
    /// Wrap an already-established `StreamOwned` for the behavioural reader test.
    ///
    /// The production path (`hew_tls_connect`) always validates against the
    /// webpki roots, which cannot trust a self-signed loopback cert. This
    /// constructor lets the loopback test inject a client connection whose
    /// config trusts the test CA, so the real `hew_tls_attach` reader and
    /// `hew_tls_close` reap path are exercised against an encrypted round-trip.
    #[cfg(test)]
    fn from_stream(
        stream: rustls::StreamOwned<rustls::ClientConnection, TcpStream>,
    ) -> *mut HewTlsStream {
        Box::into_raw(Box::new(HewTlsStream {
            inner: Arc::new(TlsShared {
                stream: Mutex::new(Some(stream)),
                closed: AtomicBool::new(false),
                reader: Mutex::new(None),
                reader_exited: AtomicBool::new(false),
            }),
        }))
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct HewTlsWriteResult {
    written: c_int,
    status: c_int,
}

#[repr(C)]
#[derive(Debug)]
pub struct HewTlsReadResult {
    data: BytesTriple,
    status: c_int,
}

// ── Helpers ───────────────────────────────────────────────────────────────────

fn ring_provider() -> Arc<rustls::crypto::CryptoProvider> {
    Arc::new(rustls::crypto::ring::default_provider())
}

/// Build a [`rustls::ClientConfig`] that trusts the standard webpki root CAs.
fn default_client_config() -> Result<rustls::ClientConfig, BoxError> {
    let mut roots = RootCertStore::empty();
    roots.extend(webpki_roots::TLS_SERVER_ROOTS.iter().cloned());
    let config = rustls::ClientConfig::builder_with_provider(ring_provider())
        .with_protocol_versions(rustls::DEFAULT_VERSIONS)?
        .with_root_certificates(roots)
        .with_no_client_auth();
    Ok(config)
}

fn connect_tls(host: &str, port: u16) -> Result<HewTlsStream, BoxError> {
    let server_name = ServerName::try_from(host.to_string())?;
    let config = default_client_config()?;
    let connector = rustls::ClientConnection::new(Arc::new(config), server_name)?;
    let addr = format!("{host}:{port}");
    let tcp = TcpStream::connect(addr)?;
    let stream = rustls::StreamOwned::new(connector, tcp);
    Ok(HewTlsStream {
        inner: Arc::new(TlsShared {
            stream: Mutex::new(Some(stream)),
            closed: AtomicBool::new(false),
            reader: Mutex::new(None),
            reader_exited: AtomicBool::new(false),
        }),
    })
}

fn set_tls_last_error(msg: impl Into<String>) {
    hew_runtime::parse_error_slot::set_error(
        hew_runtime::parse_error_slot::ErrorSlotKind::Tls,
        msg,
    );
}

fn clear_tls_last_error() {
    hew_runtime::parse_error_slot::clear_error(hew_runtime::parse_error_slot::ErrorSlotKind::Tls);
}

fn get_tls_last_error() -> String {
    hew_runtime::parse_error_slot::get_error(hew_runtime::parse_error_slot::ErrorSlotKind::Tls)
        .unwrap_or_default()
}

/// An empty, non-owning `BytesTriple` — the TLS read EOF/error sentinel.
///
/// Mirrors `hew_tcp_read`'s empty-triple convention (`transport.rs`): a null
/// `ptr` with `len == 0` is a no-op for `hew_bytes_drop`, so returning this on
/// EOF/error leaks nothing, and the record's drop of the unconsumed `data`
/// field is a no-op.
fn empty_bytes_triple() -> BytesTriple {
    BytesTriple {
        ptr: std::ptr::null_mut(),
        offset: 0,
        len: 0,
    }
}

fn classify_tls_error(op: &str, err: &io::Error) -> (c_int, String) {
    match err.kind() {
        io::ErrorKind::WouldBlock => (TLS_STATUS_RETRYABLE, format!("{op}: would block")),
        io::ErrorKind::TimedOut => (TLS_STATUS_RETRYABLE, format!("{op}: timed out")),
        io::ErrorKind::InvalidData => (
            TLS_STATUS_TLS_ERROR,
            format!("{op}: tls alert/protocol error: {err}"),
        ),
        _ => (TLS_STATUS_IO_ERROR, format!("{op}: io error: {err}")),
    }
}

fn write_out_status(out_status: *mut c_int, status: c_int) -> bool {
    if out_status.is_null() {
        return false;
    }
    // SAFETY: caller provided a non-null `out_status` pointer.
    unsafe { *out_status = status };
    true
}

fn read_tls_vec<R: Read>(reader: &mut R, size: c_int) -> HewTlsReadResult {
    let empty = empty_bytes_triple();
    let buf_size = if size == 0 {
        0
    } else {
        usize::try_from(size)
            .unwrap_or(READ_BUFFER_SIZE)
            .min(READ_BUFFER_SIZE)
    };
    if buf_size == 0 {
        clear_tls_last_error();
        return HewTlsReadResult {
            data: empty,
            status: TLS_STATUS_SUCCESS,
        };
    }

    let mut buf = vec![0u8; buf_size];
    match reader.read(&mut buf) {
        Ok(0) => {
            clear_tls_last_error();
            HewTlsReadResult {
                data: empty,
                status: TLS_STATUS_SUCCESS,
            }
        }
        Ok(n) => {
            clear_tls_last_error();
            // Bound the copy by a real subslice: an out-of-contract `n` from a
            // misbehaving reader panics here instead of reaching the unsafe copy.
            let chunk = &buf[..n];
            #[expect(
                clippy::cast_possible_truncation,
                reason = "reads are bounded by READ_BUFFER_SIZE, which fits u32"
            )]
            let len = chunk.len() as u32;
            // SAFETY: `chunk` is a live subslice valid for `chunk.len()` bytes;
            // `hew_bytes_from_static` copies them into a fresh, refcount-1 bytes
            // allocation the caller owns (mirrors `hew_tcp_read`'s construction,
            // transport.rs).
            let data = unsafe { hew_bytes_from_static(chunk.as_ptr(), len) };
            HewTlsReadResult {
                data,
                status: TLS_STATUS_SUCCESS,
            }
        }
        Err(err) => {
            let (status, message) = classify_tls_error("hew_tls_read", &err);
            set_tls_last_error(message);
            HewTlsReadResult {
                data: empty,
                status,
            }
        }
    }
}

fn write_tls_bytes<W: Write>(writer: &mut W, buf: &[u8]) -> HewTlsWriteResult {
    match writer.write_all(buf) {
        Ok(()) => match writer.flush() {
            Ok(()) => {
                clear_tls_last_error();
                HewTlsWriteResult {
                    written: c_int::try_from(buf.len()).unwrap_or(c_int::MAX),
                    status: TLS_STATUS_SUCCESS,
                }
            }
            Err(err) => {
                let (status, message) = classify_tls_error("hew_tls_write", &err);
                set_tls_last_error(message);
                HewTlsWriteResult {
                    written: -1,
                    status,
                }
            }
        },
        Err(err) => {
            let (status, message) = classify_tls_error("hew_tls_write", &err);
            set_tls_last_error(message);
            HewTlsWriteResult {
                written: -1,
                status,
            }
        }
    }
}

// ── FFI exports ───────────────────────────────────────────────────────────────

/// Open a TLS connection to `host:port` using system root certificates.
///
/// Returns a heap-allocated [`HewTlsStream`] on success, or null on error.
///
/// # Safety
///
/// `host` must be a valid NUL-terminated UTF-8 string.
#[no_mangle]
pub unsafe extern "C" fn hew_tls_connect(host: *const c_char, port: c_int) -> *mut HewTlsStream {
    // SAFETY: `host` is a valid NUL-terminated C string per caller contract.
    let Some(host_str) = (unsafe { cstr_to_str(host) }) else {
        set_tls_last_error("hew_tls_connect: invalid host string");
        return std::ptr::null_mut();
    };
    let Ok(port_u16) = u16::try_from(port) else {
        set_tls_last_error(format!("hew_tls_connect: invalid port {port}"));
        return std::ptr::null_mut();
    };
    match connect_tls(host_str, port_u16) {
        Ok(stream) => {
            clear_tls_last_error();
            Box::into_raw(Box::new(stream))
        }
        Err(err) => {
            set_tls_last_error(format!("hew_tls_connect: {err}"));
            std::ptr::null_mut()
        }
    }
}

/// Return this actor's last TLS client error.
///
/// Returns an empty string when no TLS client error has been recorded. The
/// returned string is `malloc`-allocated; callers must free it with
/// `libc::free`.
#[no_mangle]
pub extern "C" fn hew_tls_last_error() -> *mut c_char {
    str_to_malloc(&get_tls_last_error())
}

/// Write `data` to the TLS stream.
///
/// Returns the number of bytes written, or −1 on error.
/// After `hew_tls_attach`, this function still holds the write side; it
/// acquires the stream mutex before every write call.
///
/// EDGE (lazy handshake under the reader's read timeout): once an attached
/// reader sets `SO_RCVTIMEO` (`TLS_READER_TIMEOUT`) on the shared socket, a
/// write that must drive a *lazy* TLS handshake read can surface a spurious
/// `WouldBlock`/`TimedOut`, reported here as `TLS_STATUS_RETRYABLE`. In normal
/// use the handshake completes during `hew_tls_connect`, so this is not hit;
/// callers seeing a retryable status on the first write should retry.
///
/// # Safety
///
/// * `stream` must be a valid pointer returned by [`hew_tls_connect`].
/// * `data` and `data_len` must describe a valid byte buffer.
/// * `out_status` must be a valid pointer to writable status storage.
#[no_mangle]
pub unsafe extern "C" fn hew_tls_write(
    stream: *mut HewTlsStream,
    data: *const u8,
    data_len: usize,
    out_status: *mut c_int,
) -> c_int {
    if !write_out_status(out_status, TLS_STATUS_IO_ERROR) {
        set_tls_last_error("hew_tls_write: invalid status buffer");
        return -1;
    }
    if stream.is_null() {
        set_tls_last_error("hew_tls_write: invalid stream");
        return -1;
    }
    if data.is_null() && data_len > 0 {
        set_tls_last_error("hew_tls_write: invalid data buffer");
        return -1;
    }
    // SAFETY: `stream` is a valid HewTlsStream pointer per caller contract.
    let s = unsafe { &*stream };
    if s.inner.closed.load(Ordering::Acquire) {
        set_tls_last_error("hew_tls_write: stream is closed");
        let _ = write_out_status(out_status, TLS_STATUS_IO_ERROR);
        return -1;
    }
    let mut guard = s
        .inner
        .stream
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let Some(stream_ref) = guard.as_mut() else {
        set_tls_last_error("hew_tls_write: stream is closed");
        let _ = write_out_status(out_status, TLS_STATUS_IO_ERROR);
        return -1;
    };
    // SAFETY: `data` is valid for `data_len` bytes per caller contract.
    let buf = if data_len == 0 {
        &[]
    } else {
        // SAFETY: caller guarantees `data` points to `data_len` readable bytes.
        unsafe { std::slice::from_raw_parts(data, data_len) }
    };
    let result = write_tls_bytes(stream_ref, buf);
    let _ = write_out_status(out_status, result.status);
    result.written
}

/// Read up to `size` bytes from the TLS stream.
///
/// Returns an owning, refcount-1 `BytesTriple` containing the bytes read, or
/// an empty/null triple on EOF or error — the Hew drop spine releases the
/// buffer via `hew_bytes_drop`, which is a no-op on a null triple. `out_status`
/// receives `0` for success or orderly EOF, `1` for retryable would-block/timeout
/// conditions, `2` for TLS alert/protocol failures, and `3` for I/O failures.
///
/// After `hew_tls_attach` this function should not be called — the reader
/// thread owns read access; calling `hew_tls_read` concurrently with an
/// attached reader produces a deadlock on the stream mutex.
///
/// # Safety
///
/// `stream` must be a valid pointer returned by [`hew_tls_connect`].
/// `out_status` must be a valid pointer to writable status storage.
#[no_mangle]
pub unsafe extern "C" fn hew_tls_read(
    stream: *mut HewTlsStream,
    size: c_int,
    out_status: *mut c_int,
) -> BytesTriple {
    if !write_out_status(out_status, TLS_STATUS_IO_ERROR) {
        set_tls_last_error("hew_tls_read: invalid status buffer");
        return empty_bytes_triple();
    }
    if stream.is_null() {
        set_tls_last_error("hew_tls_read: invalid stream");
        return empty_bytes_triple();
    }
    // SAFETY: `stream` is a valid HewTlsStream pointer per caller contract.
    let s = unsafe { &*stream };
    let mut guard = s
        .inner
        .stream
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let Some(stream_ref) = guard.as_mut() else {
        set_tls_last_error("hew_tls_read: stream is closed");
        let _ = write_out_status(out_status, TLS_STATUS_IO_ERROR);
        return empty_bytes_triple();
    };
    let result = read_tls_vec(stream_ref, size);
    let _ = write_out_status(out_status, result.status);
    result.data
}

/// Hew-facing bridge that packages `hew_tls_write` status into a repr(C) struct
/// because Hew source cannot observe raw out-parameters directly.
///
/// # Safety
///
/// `stream` must be a valid pointer returned by [`hew_tls_connect`].
/// `data` must be a valid non-null pointer to a `BytesTriple` whose active
/// region `[offset, offset + len)` is readable.
/// (codegen passes the alloca address of the
/// Hew `bytes` argument; the previous `(*const u8, usize)` pair ignored
/// `offset`, which is incorrect for bytes slices with a non-zero offset.)
#[no_mangle]
pub unsafe extern "C" fn hew_tls_write_result(
    stream: *mut HewTlsStream,
    data: *const BytesTriple,
) -> HewTlsWriteResult {
    if data.is_null() {
        return HewTlsWriteResult {
            written: -1,
            status: TLS_STATUS_IO_ERROR,
        };
    }
    // SAFETY: caller guarantees `data` points to a valid BytesTriple.
    let triple = unsafe { &*data };
    let (data_ptr, data_len) = if triple.len == 0 || triple.ptr.is_null() {
        (std::ptr::null(), 0usize)
    } else {
        // SAFETY: BytesTriple invariant — ptr+offset..ptr+offset+len is valid.
        (
            unsafe { triple.ptr.add(triple.offset as usize).cast_const() },
            triple.len as usize,
        )
    };
    let mut status = TLS_STATUS_IO_ERROR;
    // SAFETY: data_ptr is valid for data_len bytes (or null when data_len==0).
    let written = unsafe { hew_tls_write(stream, data_ptr, data_len, &raw mut status) };
    HewTlsWriteResult { written, status }
}

/// Hew-facing bridge that packages `hew_tls_read` status into a repr(C) struct
/// because Hew source cannot observe raw out-parameters directly.
///
/// # Safety
///
/// `stream` and `size` must satisfy the same requirements as [`hew_tls_read`].
#[no_mangle]
pub unsafe extern "C" fn hew_tls_read_result(
    stream: *mut HewTlsStream,
    size: c_int,
) -> HewTlsReadResult {
    let mut status = TLS_STATUS_IO_ERROR;
    // SAFETY: this bridge forwards the exact caller-provided arguments to
    // `hew_tls_read` and supplies valid local storage for `out_status`.
    let data = unsafe { hew_tls_read(stream, size, &raw mut status) };
    HewTlsReadResult { data, status }
}

/// Take the reader join handle out of the shared state, if one was attached.
fn take_tls_reader(inner: &TlsShared) -> Option<JoinHandle<()>> {
    inner
        .reader
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .take()
}

/// Reap the background reader thread, blocking until it exits.
///
/// The reader observes `closed` at its next loop boundary and exits within at
/// most `TLS_READER_TIMEOUT` (the underlying socket read timeout). Joining here
/// guarantees the reader has stopped calling into the runtime before
/// `hew_tls_close` returns — mirroring `websocket.rs::join_reader`.
///
/// Returns `true` if a reader was attached (and joined), `false` if none was.
fn join_tls_reader(inner: &TlsShared) -> bool {
    if let Some(join) = take_tls_reader(inner) {
        let _ = join.join();
        true
    } else {
        false
    }
}

/// Close the TLS connection and free the stream handle.
///
/// Sets the `closed` flag so any attached reader thread exits at its next loop
/// boundary, then joins the reader before returning so it is deterministically
/// reaped (it can otherwise call into the runtime for up to `TLS_READER_TIMEOUT`
/// after close — the #1963 reader-lifetime hazard). Drops the inner stream,
/// which closes the underlying TCP socket.
///
/// # Safety
///
/// `stream` must be a valid pointer returned by [`hew_tls_connect`],
/// and must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_tls_close(stream: *mut HewTlsStream) {
    if stream.is_null() {
        return;
    }
    // SAFETY: `stream` was allocated by Box::into_raw in hew_tls_connect.
    let s = unsafe { Box::from_raw(stream) };
    s.inner.closed.store(true, Ordering::Release);
    // Clone the Arc first so the reader handle and stream guard outlive `s`.
    let inner = Arc::clone(&s.inner);
    drop(s);
    // Reap the reader BEFORE freeing the connection: once `closed` is set the
    // reader exits within `TLS_READER_TIMEOUT`, and joining it here ensures it
    // is no longer touching the runtime when this returns.
    //
    // The reader drops the inner stream on exit (`reader_exited` is set first,
    // then the stream is dropped — see `spawn_tls_attach_reader`). If no reader
    // was attached, close is the sole closer and must drop the stream here.
    let had_reader = join_tls_reader(&inner);
    if !had_reader {
        // No reader; we are the sole closer — drop the stream to shut the socket.
        let _ = inner.stream.lock().map(|mut g| {
            *g = None;
        });
    }
    // If a reader was attached: it dropped the stream on exit (joined above).
    // close does NOT re-acquire the stream lock, so there is no race between
    // close's stream-drop and the reader's reader_exited store. This guarantees
    // that reader_exited is observable (via the join's happens-before) the
    // instant hew_tls_close returns.
}

// ── TLS Attach (Erlang-style active mode) ────────────────────────────────────
//
// `hew_tls_attach` transfers read ownership to a background OS thread that
// reads decrypted TLS bytes and delivers them as actor messages (`on_data`).
// The caller retains write access through the outer pointer (write goes through
// the stream mutex, same as before attach). This is the Erlang active-mode
// pattern, mirroring `hew_ws_attach`.

// Mirrored from `hew-std/src/websocket.rs` — kept local to avoid a cross-module
// C-ABI dependency. The layout must match `HewActorRef` in the runtime.
const TLS_ACTOR_REF_LOCAL: c_int = 0;

#[repr(C)]
#[derive(Clone, Copy)]
struct TlsActorRefRemote {
    actor_id: u64,
    conn: c_int,
    transport: *mut c_void,
    // Mirror of the runtime's `HewActorRefRemote` (transport.rs): the captured
    // actor-slot/registration incarnation. This struct is read by value from a
    // runtime-produced `HewActorRef` and passed back across the FFI boundary,
    // so its layout MUST stay byte-identical to the runtime definition.
    incarnation: u32,
}

#[repr(C)]
union TlsActorRefData {
    local: *mut c_void,
    remote: TlsActorRefRemote,
}

#[repr(C)]
struct TlsActorRef {
    kind: c_int,
    data: TlsActorRefData,
}

// SAFETY: `TlsActorRef` snapshots are copied by value and dereferenced only
// through the runtime ABI (`hew_actor_ref_is_alive`). The reader thread holds
// this snapshot until it exits; `hew_tls_close` joins the reader before
// returning, so the reader is guaranteed to have stopped touching the actor by
// the time the connection is torn down. Callers MUST close the attached TLS
// stream (via `hew_tls_close`) before freeing the actor.
unsafe impl Send for TlsActorRef {}

extern "C" {
    fn hew_actor_try_send(actor: *mut c_void, msg_type: i32, data: *mut c_void, size: usize)
        -> i32;
    fn hew_actor_ref_is_alive(actor: *const TlsActorRef) -> i32;
}

fn tls_actor_is_alive(actor_ref: &TlsActorRef) -> bool {
    // SAFETY: `actor_ref` is the owned copy captured by the reader thread.
    unsafe { hew_actor_ref_is_alive(actor_ref) != 0 }
}

fn tls_actor_local_ptr(actor_ref: &TlsActorRef) -> Option<*mut c_void> {
    if actor_ref.kind != TLS_ACTOR_REF_LOCAL {
        return None;
    }
    // SAFETY: local variant is active when kind == TLS_ACTOR_REF_LOCAL.
    let ptr = unsafe { actor_ref.data.local };
    (!ptr.is_null()).then_some(ptr)
}

fn tls_actor_send(
    actor_ref: &TlsActorRef,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> Result<(), i32> {
    let Some(actor) = tls_actor_local_ptr(actor_ref) else {
        eprintln!("[tls-attach] remote ActorRef is unsupported for TLS attach");
        return Err(-1);
    };
    // SAFETY: `actor` is extracted from a valid local ActorRef snapshot.
    let rc = unsafe { hew_actor_try_send(actor, msg_type, data, size) };
    if rc == 0 {
        Ok(())
    } else {
        Err(rc)
    }
}

/// Read timeout for the background TLS reader thread. Short enough that the
/// reader detects actor death and closure quickly; long enough to avoid
/// spinning on a quiet connection.
///
/// WHY: `rustls::StreamOwned::read` blocks until data arrives or the connection
/// is closed. We set a read timeout on the underlying TCP socket so the reader
/// can poll the closed flag and actor liveness between reads.
const TLS_READER_TIMEOUT: std::time::Duration = std::time::Duration::from_millis(250);

fn spawn_tls_attach_reader(
    inner: TlsInner,
    actor_ref: TlsActorRef,
    on_data_type: i32,
    on_close_type: i32,
) -> JoinHandle<()> {
    std::thread::spawn(move || {
        // Set a read timeout on the underlying TCP socket so we can poll the
        // closed flag. We must do this inside the thread after acquiring the
        // lock, because the TcpStream's timeout is per-stream not per-FD.
        {
            let mut guard = inner
                .stream
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if let Some(s) = guard.as_mut() {
                if let Err(err) = s.sock.set_read_timeout(Some(TLS_READER_TIMEOUT)) {
                    eprintln!("[tls-attach] set_read_timeout failed: {err}; exiting");
                    return;
                }
            } else {
                return; // already closed before the thread started
            }
        }

        let mut buf = vec![0u8; READ_BUFFER_SIZE];
        let mut notify_close = false;

        loop {
            if inner.closed.load(Ordering::Acquire) {
                break;
            }
            if !tls_actor_is_alive(&actor_ref) {
                break;
            }

            let read_result: io::Result<usize> = {
                let mut guard = inner
                    .stream
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                let Some(s) = guard.as_mut() else {
                    break; // stream dropped by hew_tls_close
                };
                s.read(&mut buf)
            };

            match read_result {
                Ok(0) => {
                    // Orderly EOF — notify close and stop.
                    notify_close = true;
                    break;
                }
                Ok(n) => {
                    if inner.closed.load(Ordering::Acquire) {
                        break;
                    }
                    // Deliver the received bytes as a `bytes` argument.
                    // SAFETY: `buf[..n]` is valid for `n` bytes; alloc_cstring
                    // copies the content with a header word prepended.
                    let str_ptr = unsafe { alloc_cstring(buf.as_ptr(), n) }; // CSTRING-ALLOC: str-open (tls-reader str_ptr: header-aware Hew bytes passed to on_data)
                    if str_ptr.is_null() {
                        eprintln!("[tls-attach] alloc_cstring failed; exiting");
                        notify_close = true;
                        break;
                    }
                    let mut arg_buf = [0u8; std::mem::size_of::<usize>()];
                    arg_buf.copy_from_slice(&(str_ptr as usize).to_ne_bytes());
                    if let Err(rc) = tls_actor_send(
                        &actor_ref,
                        on_data_type,
                        arg_buf.as_mut_ptr().cast(),
                        arg_buf.len(),
                    ) {
                        eprintln!("[tls-attach] on_data delivery failed: rc={rc}; exiting");
                        // SAFETY: send failed before the actor took ownership.
                        unsafe { hew_cabi::cabi::free_cstring(str_ptr) }; // CSTRING-FREE: str-open (frees tls-reader str_ptr on send-fail)
                        notify_close = true;
                        break;
                    }
                }
                Err(err)
                    if err.kind() == io::ErrorKind::WouldBlock
                        || err.kind() == io::ErrorKind::TimedOut =>
                {
                    // Read timeout expired — loop to re-check flags.
                    // (no explicit `continue` needed — this is the last arm)
                }
                Err(err) => {
                    if !inner.closed.load(Ordering::Acquire) {
                        eprintln!("[tls-attach] read error: {err}; exiting");
                        notify_close = true;
                    }
                    break;
                }
            }
        }

        if notify_close && tls_actor_is_alive(&actor_ref) {
            if let Err(rc) = tls_actor_send(&actor_ref, on_close_type, std::ptr::null_mut(), 0) {
                eprintln!("[tls-attach] on_close delivery failed: rc={rc}");
            }
        }

        // Drop the inner stream to close the underlying socket on reader exit.
        // `hew_tls_close` does NOT re-acquire the stream lock when a reader was
        // attached (it relies on this drop), so there is no competing lock here.
        if let Ok(mut guard) = inner.stream.lock() {
            *guard = None;
        }

        // Signal that the reader has fully exited. This is the last write the
        // reader performs; `hew_tls_close` calls `join.join()` before returning,
        // so the join's happens-before guarantees this store is visible (Acquire)
        // the instant `hew_tls_close` returns — but ONLY if `join.join()` was
        // called (not `drop(join)`). The test asserts this flag immediately after
        // `hew_tls_close` to prove the join happened.
        inner.reader_exited.store(true, Ordering::Release);
    })
}

/// Attach a TLS stream to an actor for Erlang-style active mode.
///
/// Spawns a reader thread that reads decrypted TLS bytes in the background and
/// delivers each chunk as an `on_data` actor message. When the connection closes
/// or errors, an `on_close` message is sent. After attach, callers must not call
/// `hew_tls_read` — the reader thread owns the read side. Outbound
/// `hew_tls_write` and `hew_tls_write_result` remain valid.
///
/// - `stream`: the TLS connection. Must not be null.
/// - `actor`: pointer to the target actor's `HewActorRef` (value-snapshot taken).
/// - `on_data_type`: `msg_type` index for data delivery.
/// - `on_close_type`: `msg_type` index for close/error notification.
///
/// # Safety
///
/// - `stream` must be a valid pointer returned by [`hew_tls_connect`].
/// - `actor` must be a valid `HewActorRef` pointer that outlives the connection.
#[no_mangle]
pub unsafe extern "C" fn hew_tls_attach(
    stream: *mut HewTlsStream,
    actor: *mut c_void,
    on_data_type: c_int,
    on_close_type: c_int,
) {
    if stream.is_null() || actor.is_null() {
        eprintln!(
            "[tls-attach] null pointer: stream={} actor={}",
            stream.is_null(),
            actor.is_null()
        );
        return;
    }
    // SAFETY: `stream` is a valid HewTlsStream pointer per caller contract.
    let s = unsafe { &*stream };
    // Refuse a second attach: the stored handle would otherwise be overwritten
    // (leaking the first reader) and two readers would race the stream mutex.
    // Hold the reader lock across check + spawn + store so two concurrent
    // callers cannot both pass the `is_some()` guard and both spawn a reader
    // (a TOCTOU race if we drop and re-acquire the lock between the two steps).
    let mut reader_guard = s
        .inner
        .reader
        .lock()
        .unwrap_or_else(PoisonError::into_inner);
    if reader_guard.is_some() {
        eprintln!("[tls-attach] reader already attached for stream={stream:p}");
        return;
    }
    // SAFETY: `actor` points to a valid HewActorRef for the duration of this
    // call; we snapshot the ref by value so the reader owns its own copy.
    let actor_ref = unsafe { std::ptr::read(actor.cast::<TlsActorRef>()) };
    let join =
        spawn_tls_attach_reader(Arc::clone(&s.inner), actor_ref, on_data_type, on_close_type);
    *reader_guard = Some(join);
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::cabi::free_cstring;
    use hew_runtime::{actor, scheduler, transport};
    use rustls::pki_types::{CertificateDer, PrivateKeyDer, PrivatePkcs8KeyDer};
    use std::cell::Cell;
    use std::collections::HashMap;
    use std::ffi::CStr;
    use std::ffi::CString;
    use std::io::ErrorKind;
    use std::net::TcpListener;
    use std::os::raw::c_void;
    use std::sync::atomic::AtomicU64;
    use std::sync::mpsc::{self, Receiver, Sender};
    use std::sync::OnceLock;
    use std::thread;
    use std::time::{Duration, Instant};

    #[derive(Debug)]
    enum MockReadAction {
        Bytes(Vec<u8>),
        Eof,
        Error(io::Error),
        Panic,
    }

    #[derive(Debug)]
    struct MockReader {
        action: MockReadAction,
        called: Cell<bool>,
    }

    impl MockReader {
        fn bytes(bytes: &[u8]) -> Self {
            Self {
                action: MockReadAction::Bytes(bytes.to_vec()),
                called: Cell::new(false),
            }
        }

        fn eof() -> Self {
            Self {
                action: MockReadAction::Eof,
                called: Cell::new(false),
            }
        }

        fn error(kind: ErrorKind, message: &'static str) -> Self {
            Self {
                action: MockReadAction::Error(io::Error::new(kind, message)),
                called: Cell::new(false),
            }
        }

        fn panic() -> Self {
            Self {
                action: MockReadAction::Panic,
                called: Cell::new(false),
            }
        }
    }

    #[derive(Debug)]
    enum MockWriteAction {
        Success,
        WriteError(io::Error),
        FlushError(io::Error),
    }

    #[derive(Debug)]
    struct MockWriter {
        action: MockWriteAction,
        wrote: Cell<bool>,
        flushed: Cell<bool>,
    }

    impl MockWriter {
        fn success() -> Self {
            Self {
                action: MockWriteAction::Success,
                wrote: Cell::new(false),
                flushed: Cell::new(false),
            }
        }

        fn write_error(kind: ErrorKind, message: &'static str) -> Self {
            Self {
                action: MockWriteAction::WriteError(io::Error::new(kind, message)),
                wrote: Cell::new(false),
                flushed: Cell::new(false),
            }
        }

        fn flush_error(kind: ErrorKind, message: &'static str) -> Self {
            Self {
                action: MockWriteAction::FlushError(io::Error::new(kind, message)),
                wrote: Cell::new(false),
                flushed: Cell::new(false),
            }
        }
    }

    impl Write for MockWriter {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            self.wrote.set(true);
            match &self.action {
                MockWriteAction::Success | MockWriteAction::FlushError(_) => Ok(buf.len()),
                MockWriteAction::WriteError(err) => {
                    Err(io::Error::new(err.kind(), err.to_string()))
                }
            }
        }

        fn flush(&mut self) -> io::Result<()> {
            self.flushed.set(true);
            match &self.action {
                MockWriteAction::Success | MockWriteAction::WriteError(_) => Ok(()),
                MockWriteAction::FlushError(err) => {
                    Err(io::Error::new(err.kind(), err.to_string()))
                }
            }
        }
    }

    impl Read for MockReader {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            self.called.set(true);
            match &self.action {
                MockReadAction::Bytes(bytes) => {
                    let count = bytes.len().min(buf.len());
                    buf[..count].copy_from_slice(&bytes[..count]);
                    Ok(count)
                }
                MockReadAction::Eof => Ok(0),
                MockReadAction::Error(err) => Err(io::Error::new(err.kind(), err.to_string())),
                MockReadAction::Panic => panic!("reader should not be called"),
            }
        }
    }

    fn last_error_string() -> String {
        let ptr = hew_tls_last_error();
        if ptr.is_null() {
            return String::new();
        }
        // SAFETY: `ptr` is returned by `hew_tls_last_error` as a valid,
        // NUL-terminated string allocation.
        let message = unsafe { CStr::from_ptr(ptr) }
            .to_string_lossy()
            .into_owned();
        // SAFETY: `ptr` was allocated with `libc::malloc` by `hew_tls_last_error`.
        unsafe { hew_cabi::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open (frees hew_tls_last_error = str_to_malloc)
        message
    }

    fn vec_bytes(triple: &BytesTriple) -> Vec<u8> {
        if triple.len == 0 || triple.ptr.is_null() {
            return Vec::new();
        }
        // SAFETY: non-empty `BytesTriple` values produced by this module store
        // `len` readable bytes starting at `ptr + offset`.
        unsafe {
            std::slice::from_raw_parts(triple.ptr.add(triple.offset as usize), triple.len as usize)
        }
        .to_vec()
    }

    fn free_vec(triple: &BytesTriple) {
        // SAFETY: non-null triples produced by this module are refcount-1
        // `hew_bytes_from_static` allocations (header-bearing, per
        // `bytes.rs::alloc_buf`). `hew_bytes_drop` is the correct release —
        // freeing `triple.ptr` with `libc::free` would free the wrong base
        // (the allocation header precedes `ptr`) and corrupt the allocator.
        // `hew_bytes_drop` is a no-op on a null pointer.
        unsafe { hew_runtime::bytes::hew_bytes_drop(triple.ptr) };
    }

    fn timed_out_tcp_reader(client_timeout: Duration) -> (TcpStream, thread::JoinHandle<()>) {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let addr = listener.local_addr().unwrap();

        let server = thread::spawn(move || {
            let _accepted = listener.accept().unwrap();
            thread::sleep(Duration::from_millis(200));
        });

        let tcp = TcpStream::connect(addr).unwrap();
        tcp.set_read_timeout(Some(client_timeout)).unwrap();
        (tcp, server)
    }

    #[test]
    fn connect_null_host_returns_null() {
        clear_tls_last_error();
        // SAFETY: passing null is the test.
        let ptr = unsafe { hew_tls_connect(std::ptr::null(), 443) };
        assert!(ptr.is_null());
        assert_eq!(last_error_string(), "hew_tls_connect: invalid host string");
    }

    #[test]
    fn connect_refused_sets_last_error() {
        clear_tls_last_error();
        // Port 0 is never listening; the OS refuses the connect() attempt
        // (or DNS resolution itself fails for a bogus host), either way
        // exercising the `connect_tls(...) => Err(_)` failure path without
        // needing a live network fixture.
        let host = CString::new("127.0.0.1").unwrap();
        // SAFETY: `host` is a valid NUL-terminated C string.
        let ptr = unsafe { hew_tls_connect(host.as_ptr(), 0) };
        assert!(ptr.is_null());
        assert!(
            last_error_string().starts_with("hew_tls_connect: "),
            "expected a hew_tls_connect-prefixed message, got {:?}",
            last_error_string()
        );
        assert!(
            !last_error_string().ends_with("invalid host string")
                && !last_error_string().contains("invalid port"),
            "expected the connect_tls Err(_) path, got {:?}",
            last_error_string()
        );
    }

    #[test]
    fn write_null_stream_returns_error() {
        let data = b"hello";
        let mut status = -1;
        // SAFETY: passing null stream is the test.
        let ret = unsafe {
            hew_tls_write(
                std::ptr::null_mut(),
                data.as_ptr(),
                data.len(),
                &raw mut status,
            )
        };
        assert_eq!(ret, -1);
        assert_eq!(status, TLS_STATUS_IO_ERROR);
    }

    #[test]
    fn read_null_stream_returns_empty() {
        let mut status = -1;
        // SAFETY: passing null stream is the test.
        let triple = unsafe { hew_tls_read(std::ptr::null_mut(), 1024, &raw mut status) };
        assert_eq!(triple.len, 0);
        assert!(triple.ptr.is_null());
        assert_eq!(status, TLS_STATUS_IO_ERROR);
    }

    #[test]
    fn close_null_stream_is_noop() {
        // SAFETY: passing null is the test.
        unsafe { hew_tls_close(std::ptr::null_mut()) };
    }

    #[test]
    fn clean_eof_and_midstream_force_close_have_distinct_statuses() {
        set_tls_last_error("stale");
        let mut reader = MockReader::eof();
        let eof = read_tls_vec(&mut reader, 32);

        assert!(reader.called.get());
        assert_eq!(eof.data.len, 0);
        assert_eq!(eof.status, TLS_STATUS_SUCCESS);
        assert!(last_error_string().is_empty());

        let mut force_closed =
            MockReader::error(ErrorKind::UnexpectedEof, "peer closed mid-stream");
        let reset = read_tls_vec(&mut force_closed, 32);

        assert_eq!(reset.data.len, 0);
        assert_eq!(reset.status, TLS_STATUS_IO_ERROR);
        assert_eq!(
            last_error_string(),
            "hew_tls_read: io error: peer closed mid-stream"
        );
    }

    #[test]
    fn read_would_block_sets_retryable_status_and_last_error() {
        clear_tls_last_error();
        let mut reader = MockReader::error(ErrorKind::WouldBlock, "try again");

        let result = read_tls_vec(&mut reader, 32);

        assert_eq!(result.data.len, 0);
        assert_eq!(result.status, TLS_STATUS_RETRYABLE);
        assert_eq!(last_error_string(), "hew_tls_read: would block");
    }

    #[test]
    fn read_socket_timeout_sets_retryable_status_and_last_error() {
        clear_tls_last_error();
        let (mut stream, server) = timed_out_tcp_reader(Duration::from_millis(25));

        let result = read_tls_vec(&mut stream, 32);

        assert_eq!(result.data.len, 0);
        assert_eq!(result.status, TLS_STATUS_RETRYABLE);
        // macOS surfaces SO_RCVTIMEO expirations as WouldBlock; Linux commonly
        // reports TimedOut. Both must stay on the retryable status path.
        let last_error = last_error_string();
        assert!(
            last_error == "hew_tls_read: timed out" || last_error == "hew_tls_read: would block",
            "unexpected timeout classification: {last_error}"
        );
        server.join().unwrap();
    }

    #[test]
    fn classify_timed_out_as_retryable() {
        let (status, message) = classify_tls_error(
            "hew_tls_read",
            &io::Error::new(ErrorKind::TimedOut, "slow peer"),
        );

        assert_eq!(status, TLS_STATUS_RETRYABLE);
        assert_eq!(message, "hew_tls_read: timed out");
    }

    #[test]
    fn read_tls_protocol_error_sets_tls_status() {
        clear_tls_last_error();
        let mut reader = MockReader::error(ErrorKind::InvalidData, "received bad alert");

        let result = read_tls_vec(&mut reader, 32);

        assert_eq!(result.data.len, 0);
        assert_eq!(result.status, TLS_STATUS_TLS_ERROR);
        assert_eq!(
            last_error_string(),
            "hew_tls_read: tls alert/protocol error: received bad alert"
        );
    }

    #[test]
    fn read_transport_error_sets_last_error_text() {
        clear_tls_last_error();
        let mut reader = MockReader::error(ErrorKind::ConnectionReset, "connection reset");

        let result = read_tls_vec(&mut reader, 32);

        assert_eq!(result.data.len, 0);
        assert_eq!(result.status, TLS_STATUS_IO_ERROR);
        assert_eq!(
            last_error_string(),
            "hew_tls_read: io error: connection reset"
        );
    }

    #[test]
    fn zero_size_read_is_empty_but_not_eof() {
        clear_tls_last_error();
        let mut reader = MockReader::panic();

        let result = read_tls_vec(&mut reader, 0);

        assert!(!reader.called.get());
        assert_eq!(result.data.len, 0);
        assert_eq!(result.status, TLS_STATUS_SUCCESS);
        assert!(last_error_string().is_empty());
    }

    #[test]
    fn successful_non_empty_read_clears_last_error() {
        set_tls_last_error("timeout");
        let mut reader = MockReader::bytes(b"hello");

        let result = read_tls_vec(&mut reader, 32);

        assert_eq!(result.status, TLS_STATUS_SUCCESS);
        assert_eq!(vec_bytes(&result.data), b"hello");
        assert!(last_error_string().is_empty());
        free_vec(&result.data);
    }

    /// Teeth for #2392: the read producer must return an owning `BytesTriple`
    /// with the exact length and content read — not a garbage/uninitialised
    /// slot (an `> 0` length check alone would pass on that). Also confirms
    /// the EOF path returns a null triple, matching the ownership contract
    /// `hew_bytes_drop` relies on.
    #[test]
    fn read_tls_vec_owns_and_returns_exact_bytes() {
        clear_tls_last_error();
        let mut reader = MockReader::bytes(b"hello");

        let result = read_tls_vec(&mut reader, 32);

        assert_eq!(result.status, TLS_STATUS_SUCCESS);
        assert!(!result.data.ptr.is_null());
        assert_eq!(result.data.offset, 0);
        assert_eq!(result.data.len, 5);
        assert_eq!(vec_bytes(&result.data), b"hello");
        free_vec(&result.data);

        let mut eof_reader = MockReader::eof();
        let eof_result = read_tls_vec(&mut eof_reader, 32);
        assert_eq!(eof_result.status, TLS_STATUS_SUCCESS);
        assert!(eof_result.data.ptr.is_null());
        assert_eq!(eof_result.data.len, 0);
    }

    #[test]
    fn successful_write_clears_stale_error() {
        set_tls_last_error("old error");
        let mut writer = MockWriter::success();

        let result = write_tls_bytes(&mut writer, b"hello");

        assert!(writer.wrote.get());
        assert!(writer.flushed.get());
        assert_eq!(result.written, 5);
        assert_eq!(result.status, TLS_STATUS_SUCCESS);
        assert!(last_error_string().is_empty());
    }

    #[test]
    fn write_would_block_sets_retryable_status_and_last_error() {
        clear_tls_last_error();
        let mut writer = MockWriter::write_error(ErrorKind::WouldBlock, "try again");

        let result = write_tls_bytes(&mut writer, b"hello");

        assert_eq!(result.written, -1);
        assert_eq!(result.status, TLS_STATUS_RETRYABLE);
        assert_eq!(last_error_string(), "hew_tls_write: would block");
    }

    #[test]
    fn write_tls_protocol_error_sets_tls_status_and_last_error() {
        clear_tls_last_error();
        let mut writer = MockWriter::flush_error(ErrorKind::InvalidData, "bad close notify");

        let result = write_tls_bytes(&mut writer, b"hello");

        assert_eq!(result.written, -1);
        assert_eq!(result.status, TLS_STATUS_TLS_ERROR);
        assert_eq!(
            last_error_string(),
            "hew_tls_write: tls alert/protocol error: bad close notify"
        );
    }

    #[test]
    fn default_config_does_not_panic() {
        let config = default_client_config();
        assert!(config.is_ok(), "default_client_config should succeed");
    }

    #[test]
    fn connect_empty_host_returns_null() {
        clear_tls_last_error();
        let host = std::ffi::CString::new("").unwrap();
        // SAFETY: passing valid but empty C string.
        let ptr = unsafe { hew_tls_connect(host.as_ptr(), 443) };
        assert!(ptr.is_null());
        // An empty host is valid UTF-8 (passes `cstr_to_str`), so this hits
        // the `connect_tls(...) => Err(_)` path (invalid DNS name) rather
        // than the null/invalid-C-string path exercised by
        // `connect_null_host_returns_null`.
        assert_eq!(last_error_string(), "hew_tls_connect: invalid dns name");
    }

    #[test]
    fn connect_negative_port_returns_null() {
        clear_tls_last_error();
        let host = std::ffi::CString::new("example.com").unwrap();
        // SAFETY: passing valid host with invalid port.
        let ptr = unsafe { hew_tls_connect(host.as_ptr(), -1) };
        assert!(ptr.is_null());
        assert_eq!(last_error_string(), "hew_tls_connect: invalid port -1");
    }

    #[test]
    fn write_null_data_nonzero_len_returns_error() {
        let mut status = -1;
        // SAFETY: testing null data with non-zero length.
        let ret =
            unsafe { hew_tls_write(std::ptr::null_mut(), std::ptr::null(), 10, &raw mut status) };
        assert_eq!(ret, -1);
        assert_eq!(status, TLS_STATUS_IO_ERROR);
    }

    // ── hew_tls_write_result bridge ───────────────────────────────────────────

    #[test]
    fn write_result_null_stream_returns_io_error_status() {
        clear_tls_last_error();
        let data = b"payload";
        #[allow(clippy::cast_possible_truncation, reason = "test slice fits in u32")]
        let triple = BytesTriple {
            ptr: data.as_ptr().cast_mut(),
            offset: 0,
            len: data.len() as u32,
        };
        // SAFETY: null stream is the test; triple points to valid stack data.
        let result = unsafe { hew_tls_write_result(std::ptr::null_mut(), &raw const triple) };
        assert_eq!(result.written, -1);
        assert_eq!(result.status, TLS_STATUS_IO_ERROR);
    }

    #[test]
    fn write_result_null_data_returns_io_error_status() {
        clear_tls_last_error();
        // SAFETY: null BytesTriple pointer exercises the null-data guard path.
        let result =
            unsafe { hew_tls_write_result(std::ptr::null_mut(), std::ptr::null::<BytesTriple>()) };
        assert_eq!(result.written, -1);
        assert_eq!(result.status, TLS_STATUS_IO_ERROR);
    }

    #[test]
    fn write_result_packages_status_from_underlying_call() {
        // Verify the wrapper propagates the status written by hew_tls_write
        // (not the initial TLS_STATUS_IO_ERROR sentinel) into the returned struct.
        // With a null stream, hew_tls_write sets status to TLS_STATUS_IO_ERROR and
        // returns -1; the wrapper must capture both fields.
        clear_tls_last_error();
        let data = b"check";
        let mut direct_status = 0_i32;
        // SAFETY: null stream, valid data pointer — exercising the error path.
        let direct_written = unsafe {
            hew_tls_write(
                std::ptr::null_mut(),
                data.as_ptr(),
                data.len(),
                &raw mut direct_status,
            )
        };

        clear_tls_last_error();
        #[allow(clippy::cast_possible_truncation, reason = "test slice fits in u32")]
        let triple = BytesTriple {
            ptr: data.as_ptr().cast_mut(),
            offset: 0,
            len: data.len() as u32,
        };
        // SAFETY: same data as above via the wrapper; null stream exercises error path.
        let result = unsafe { hew_tls_write_result(std::ptr::null_mut(), &raw const triple) };

        assert_eq!(result.written, direct_written);
        assert_eq!(result.status, direct_status);
    }

    // ── hew_tls_read_result bridge ────────────────────────────────────────────

    #[test]
    fn read_result_null_stream_returns_io_error_status() {
        clear_tls_last_error();
        // SAFETY: null stream is the test.
        let result = unsafe { hew_tls_read_result(std::ptr::null_mut(), 1024) };
        assert_eq!(result.data.len, 0);
        assert!(result.data.ptr.is_null());
        assert_eq!(result.status, TLS_STATUS_IO_ERROR);
    }

    #[test]
    fn read_result_packages_status_from_underlying_call() {
        // Verify the wrapper propagates the status written by hew_tls_read
        // (not the initial TLS_STATUS_IO_ERROR sentinel) into the returned struct.
        clear_tls_last_error();
        let mut direct_status = 0_i32;
        // SAFETY: null stream — exercising the error path through hew_tls_read directly.
        let _direct_vec = unsafe { hew_tls_read(std::ptr::null_mut(), 64, &raw mut direct_status) };

        clear_tls_last_error();
        // SAFETY: same null stream via the wrapper.
        let result = unsafe { hew_tls_read_result(std::ptr::null_mut(), 64) };

        assert_eq!(result.status, direct_status);
        assert_eq!(result.data.len, 0);
    }

    // ── hew_tls_attach null-pointer guards ───────────────────────────────────

    #[test]
    fn attach_null_stream_is_noop() {
        // SAFETY: null stream exercises the guard path; actor pointer is
        // irrelevant because the null-stream check fires first.
        unsafe {
            hew_tls_attach(std::ptr::null_mut(), std::ptr::null_mut(), 0, 1);
        };
        // No panic, no crash — guard exited cleanly.
    }

    #[test]
    fn attach_null_actor_is_noop() {
        // Construct a minimal live stream so the null-stream guard does not fire.
        let shared = Arc::new(TlsShared {
            stream: Mutex::new(None),
            closed: AtomicBool::new(false),
            reader: Mutex::new(None),
            reader_exited: AtomicBool::new(false),
        });
        let boxed = Box::new(HewTlsStream {
            inner: Arc::clone(&shared),
        });
        let stream_ptr = Box::into_raw(boxed);
        // SAFETY: null actor pointer exercises the null-actor guard.
        unsafe { hew_tls_attach(stream_ptr, std::ptr::null_mut(), 0, 1) };
        // SAFETY: we created `stream_ptr` and it was not consumed.
        unsafe { hew_tls_close(stream_ptr) };
    }

    #[test]
    fn attach_on_closed_stream_read_returns_none() {
        // Verify that after close, the stream mutex yields None.
        let shared = Arc::new(TlsShared {
            stream: Mutex::new(None),
            closed: AtomicBool::new(true),
            reader: Mutex::new(None),
            reader_exited: AtomicBool::new(false),
        });
        // The stream is None and closed — a reader would exit immediately.
        let guard = shared.stream.lock().unwrap();
        assert!(guard.is_none());
        assert!(shared.closed.load(Ordering::Acquire));
    }

    // ── Behavioural reader loopback (real TLS round-trip + reap) ──────────────
    //
    // Exercises the `hew_tls_attach` reader path that the null-guard tests never
    // touch: a real rustls handshake completes, decrypted bytes arrive via
    // `on_data`, server EOF surfaces as `on_close`, and `hew_tls_close`
    // deterministically reaps the reader thread (the #1963 reader-lifetime fix).

    const TLS_ON_DATA_TYPE: i32 = 7;
    const TLS_ON_CLOSE_TYPE: i32 = 8;

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum TlsActorEvent {
        Data(Vec<u8>),
        Closed,
    }

    #[repr(C)]
    #[derive(Clone, Copy)]
    struct TlsTestActorState {
        test_id: u64,
    }

    static TLS_NEXT_TEST_ID: AtomicU64 = AtomicU64::new(1);
    static TLS_ACTOR_EVENTS: OnceLock<Mutex<HashMap<u64, Sender<TlsActorEvent>>>> = OnceLock::new();

    fn tls_actor_events() -> &'static Mutex<HashMap<u64, Sender<TlsActorEvent>>> {
        TLS_ACTOR_EVENTS.get_or_init(|| Mutex::new(HashMap::new()))
    }

    fn register_tls_actor_events() -> (u64, Receiver<TlsActorEvent>) {
        let test_id = TLS_NEXT_TEST_ID.fetch_add(1, Ordering::Relaxed);
        let (tx, rx) = mpsc::channel();
        tls_actor_events()
            .lock()
            .expect("tls actor event registry poisoned")
            .insert(test_id, tx);
        (test_id, rx)
    }

    fn unregister_tls_actor_events(test_id: u64) {
        tls_actor_events()
            .lock()
            .expect("tls actor event registry poisoned")
            .remove(&test_id);
    }

    fn send_tls_actor_event(test_id: u64, event: TlsActorEvent) {
        if let Some(sender) = tls_actor_events()
            .lock()
            .expect("tls actor event registry poisoned")
            .get(&test_id)
            .cloned()
        {
            let _ = sender.send(event);
        }
    }

    unsafe extern "C-unwind" fn tls_test_dispatch(
        _ctx: *mut hew_runtime::HewExecutionContext,
        state: *mut c_void,
        msg_type: i32,
        data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        // SAFETY: test actor state is a POD snapshot allocated by `hew_actor_spawn`.
        let state = unsafe { &*(state.cast::<TlsTestActorState>()) };
        match msg_type {
            TLS_ON_DATA_TYPE => {
                // SAFETY: the reader packs a pointer-sized value holding the
                // malloc-allocated header-aware Hew bytes payload.
                let str_ptr = unsafe { *(data.cast::<usize>()) as *mut c_char };
                let bytes = if str_ptr.is_null() {
                    Vec::new()
                } else {
                    // SAFETY: `alloc_cstring` produced a NUL-terminated buffer.
                    let bytes = unsafe { CStr::from_ptr(str_ptr) }.to_bytes().to_vec();
                    // SAFETY: ownership transfers to the handler on successful send.
                    unsafe { free_cstring(str_ptr) }; // CSTRING-FREE: str-open (frees reader str_ptr after readback)
                    bytes
                };
                send_tls_actor_event(state.test_id, TlsActorEvent::Data(bytes));
            }
            TLS_ON_CLOSE_TYPE => send_tls_actor_event(state.test_id, TlsActorEvent::Closed),
            _ => {}
        }
        std::ptr::null_mut()
    }

    static TLS_RUNTIME_TEST_LOCK: Mutex<()> = Mutex::new(());

    struct TlsRuntimeGuard {
        _lock: std::sync::MutexGuard<'static, ()>,
    }

    impl TlsRuntimeGuard {
        fn new() -> Self {
            // The scheduler is process-global: repeated init is a no-op, but
            // cleanup frees every tracked actor. Hold exclusive ownership for
            // the full test so one TLS test cannot reclaim another's live actor.
            let lock = TLS_RUNTIME_TEST_LOCK
                .lock()
                .unwrap_or_else(PoisonError::into_inner);
            assert_eq!(scheduler::hew_sched_init(), 0);
            Self { _lock: lock }
        }
    }

    impl Drop for TlsRuntimeGuard {
        fn drop(&mut self) {
            scheduler::hew_sched_shutdown();
            scheduler::hew_runtime_cleanup();
        }
    }

    fn recv_tls_event(rx: &Receiver<TlsActorEvent>, timeout: Duration) -> TlsActorEvent {
        rx.recv_timeout(timeout)
            .unwrap_or_else(|err| panic!("expected tls actor event within {timeout:?}: {err:?}"))
    }

    /// Build a rustls `ServerConfig` from a freshly generated self-signed cert,
    /// returning the config plus the cert DER so the client can trust it.
    fn self_signed_server_pair() -> (Arc<rustls::ServerConfig>, CertificateDer<'static>) {
        let cert = rcgen::generate_simple_self_signed(vec!["localhost".into()])
            .expect("self-signed test certificate");
        let cert_der = CertificateDer::from(cert.cert);
        let key_der =
            PrivateKeyDer::Pkcs8(PrivatePkcs8KeyDer::from(cert.signing_key.serialize_der()));
        let config = rustls::ServerConfig::builder_with_provider(ring_provider())
            .with_protocol_versions(rustls::DEFAULT_VERSIONS)
            .expect("server protocol versions")
            .with_no_client_auth()
            .with_single_cert(vec![cert_der.clone()], key_der)
            .expect("server single cert");
        (Arc::new(config), cert_der)
    }

    /// Build a client `StreamOwned` connected to `addr` that trusts `cert`.
    fn client_stream_trusting(
        addr: std::net::SocketAddr,
        cert: CertificateDer<'static>,
    ) -> rustls::StreamOwned<rustls::ClientConnection, TcpStream> {
        let mut roots = RootCertStore::empty();
        roots.add(cert).expect("trust test cert");
        let config = rustls::ClientConfig::builder_with_provider(ring_provider())
            .with_protocol_versions(rustls::DEFAULT_VERSIONS)
            .expect("client protocol versions")
            .with_root_certificates(roots)
            .with_no_client_auth();
        let server_name = ServerName::try_from("localhost").expect("server name");
        let conn = rustls::ClientConnection::new(Arc::new(config), server_name)
            .expect("client connection");
        let tcp = TcpStream::connect(addr).expect("client tcp connect");
        rustls::StreamOwned::new(conn, tcp)
    }

    #[test]
    fn attach_reader_delivers_decrypted_data_and_reaps_on_close() {
        let _runtime = TlsRuntimeGuard::new();

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback");
        let addr = listener.local_addr().expect("local addr");
        let (server_config, cert_der) = self_signed_server_pair();

        // Server: accept one connection, complete the handshake, send a payload,
        // then close cleanly so the client reader observes EOF.
        let payload = b"hello-over-tls";
        let server = thread::spawn(move || {
            let (tcp, _) = listener.accept().expect("server accept");
            let conn = rustls::ServerConnection::new(server_config).expect("server connection");
            let mut tls = rustls::StreamOwned::new(conn, tcp);
            // Drive the handshake + write the payload.
            tls.write_all(payload).expect("server write");
            tls.flush().expect("server flush");
            // Give the client time to read before tearing the socket down.
            thread::sleep(Duration::from_millis(150));
            // Send a TLS close_notify, then drop to close the socket → client EOF.
            tls.conn.send_close_notify();
            let _ = tls.flush();
            drop(tls);
        });

        let client = client_stream_trusting(addr, cert_der);
        let stream_ptr = HewTlsStream::from_stream(client);

        let (test_id, rx) = register_tls_actor_events();
        let state = TlsTestActorState { test_id };
        // SAFETY: `state` is a valid POD snapshot for the spawn copy.
        let actor = unsafe {
            actor::hew_actor_spawn(
                (&raw const state).cast_mut().cast(),
                std::mem::size_of::<TlsTestActorState>(),
                Some(tls_test_dispatch),
            )
        };
        assert!(!actor.is_null(), "test actor should spawn");
        // SAFETY: `actor` is a live actor we own.
        let mut actor_ref = unsafe { transport::hew_actor_ref_local(actor) };

        // SAFETY: `stream_ptr` is a live stream; `actor_ref` outlives the attach.
        unsafe {
            hew_tls_attach(
                stream_ptr,
                (&raw mut actor_ref).cast(),
                TLS_ON_DATA_TYPE,
                TLS_ON_CLOSE_TYPE,
            );
        }

        // The decrypted payload must arrive via on_data (possibly across chunks).
        let mut received = Vec::new();
        let deadline = Instant::now() + Duration::from_secs(5);
        while received.len() < payload.len() && Instant::now() < deadline {
            match recv_tls_event(&rx, Duration::from_secs(5)) {
                TlsActorEvent::Data(bytes) => received.extend_from_slice(&bytes),
                TlsActorEvent::Closed => break,
            }
        }
        assert_eq!(
            received, payload,
            "decrypted bytes must arrive via on_data exactly"
        );

        // Server close → reader observes EOF → on_close fires.
        let mut saw_close = false;
        let close_deadline = Instant::now() + Duration::from_secs(5);
        while Instant::now() < close_deadline {
            match recv_tls_event(&rx, Duration::from_secs(5)) {
                TlsActorEvent::Closed => {
                    saw_close = true;
                    break;
                }
                TlsActorEvent::Data(_) => {}
            }
        }
        assert!(saw_close, "on_close must fire on server EOF");

        // The reap proof: hew_tls_close must join the reader promptly (no hang).
        // The reader's worst-case exit latency is TLS_READER_TIMEOUT (250 ms);
        // closing must return well within a generous bound. Routed into the
        // `real-timing` nextest group (.config/nextest.toml) alongside its
        // sibling below — both measure a real OS-scheduled thread join and
        // starve under full-workspace parallel load otherwise (#2358).
        let close_start = Instant::now();
        // SAFETY: `stream_ptr` was produced by `from_stream` and not yet freed.
        unsafe { hew_tls_close(stream_ptr) };
        let close_elapsed = close_start.elapsed();
        assert!(
            close_elapsed < Duration::from_secs(2),
            "hew_tls_close must reap the reader promptly, took {close_elapsed:?}"
        );

        server.join().expect("server thread");
        // SAFETY: `actor` is the live actor we spawned; stop quiesces it.
        unsafe { actor::hew_actor_stop(actor) };
        // SAFETY: `actor` is stopped above; free reclaims it exactly once.
        assert_eq!(unsafe { actor::hew_actor_free(actor) }, 0);
        unregister_tls_actor_events(test_id);
    }

    #[test]
    fn close_reaps_a_live_blocked_reader_without_hanging() {
        // Teeth for the #1963 reap: the reader is parked in its blocking read
        // (the server never sends data and never closes) when `hew_tls_close` is
        // called. `hew_tls_close` must join the still-running reader and return
        // within ~`TLS_READER_TIMEOUT`, not block forever or detach it.
        let _runtime = TlsRuntimeGuard::new();

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback");
        let addr = listener.local_addr().expect("local addr");
        let (server_config, cert_der) = self_signed_server_pair();

        // Server: complete the handshake, then hold the connection open and
        // silent until the client tears it down. Keep the StreamOwned alive in
        // the `keep_open` channel so the socket is not closed early.
        let (done_tx, done_rx) = mpsc::channel::<()>();
        let server = thread::spawn(move || {
            let (tcp, _) = listener.accept().expect("server accept");
            let mut conn = rustls::ServerConnection::new(server_config).expect("server connection");
            // Drive the handshake to completion without sending app data.
            let _ = conn.complete_io(&mut { tcp.try_clone().expect("clone tcp") });
            let _tls = rustls::StreamOwned::new(conn, tcp);
            // Hold the connection open until the client closes.
            let _ = done_rx.recv_timeout(Duration::from_secs(10));
        });

        let client = client_stream_trusting(addr, cert_der);
        let stream_ptr = HewTlsStream::from_stream(client);

        let (test_id, rx) = register_tls_actor_events();
        let state = TlsTestActorState { test_id };
        // SAFETY: `state` is a valid POD snapshot for the spawn copy.
        let actor = unsafe {
            actor::hew_actor_spawn(
                (&raw const state).cast_mut().cast(),
                std::mem::size_of::<TlsTestActorState>(),
                Some(tls_test_dispatch),
            )
        };
        assert!(!actor.is_null(), "test actor should spawn");
        // SAFETY: `actor` is a live actor we own.
        let mut actor_ref = unsafe { transport::hew_actor_ref_local(actor) };

        // SAFETY: `stream_ptr` is a live stream; `actor_ref` outlives the attach.
        unsafe {
            hew_tls_attach(
                stream_ptr,
                (&raw mut actor_ref).cast(),
                TLS_ON_DATA_TYPE,
                TLS_ON_CLOSE_TYPE,
            );
        }

        // Let the reader settle into its blocking read loop before closing.
        thread::sleep(Duration::from_millis(50));
        // No data should have arrived on a silent connection.
        match rx.recv_timeout(Duration::from_millis(50)) {
            Err(mpsc::RecvTimeoutError::Timeout) => {}
            other => panic!("expected no event on a silent connection, got {other:?}"),
        }

        // Clone the inner Arc BEFORE close so we can inspect `reader_exited`
        // after `hew_tls_close` frees the outer HewTlsStream.
        // SAFETY: `stream_ptr` is live at this point; we read (not move) the Arc.
        let inner_arc = Arc::clone(unsafe { &(*stream_ptr).inner });

        // The reap proof: closing a live, blocked reader returns promptly.
        // This measurement is an irreducibly real OS-scheduling bound (a live
        // thread parked in a socket read, joined via a real JoinHandle), so it
        // is routed into the `real-timing` nextest group (max-threads = 1,
        // .config/nextest.toml) — full-workspace parallel runs would otherwise
        // starve it past the deadline below (#2358).
        let close_start = Instant::now();
        // SAFETY: `stream_ptr` was produced by `from_stream` and not yet freed.
        unsafe { hew_tls_close(stream_ptr) };
        let close_elapsed = close_start.elapsed();
        assert!(
            close_elapsed < Duration::from_secs(2),
            "hew_tls_close must reap a live reader within the read-timeout bound, took {close_elapsed:?}"
        );

        // JOIN-PROOF: `reader_exited` is set (Release) by the reader immediately
        // before it returns. `hew_tls_close` calls `join.join()` before returning,
        // so the join synchronises this write — meaning the flag MUST be true the
        // instant `hew_tls_close` returns. If `join.join()` is replaced with
        // `drop(join)` (detaching the reader), `hew_tls_close` can return before
        // the reader sets the flag and this assertion fails.
        assert!(
            inner_arc.reader_exited.load(Ordering::Acquire),
            "reader_exited must be set immediately after hew_tls_close — \
             this can only be true if hew_tls_close called join.join(), \
             not drop(join); a detached reader would not yet have set this flag"
        );

        let _ = done_tx.send(());
        server.join().expect("server thread");
        // SAFETY: `actor` is the live actor we spawned; stop quiesces it.
        unsafe { actor::hew_actor_stop(actor) };
        // SAFETY: `actor` is stopped above; free reclaims it exactly once.
        assert_eq!(unsafe { actor::hew_actor_free(actor) }, 0);
        unregister_tls_actor_events(test_id);
    }

    /// FFI enum/status-code parity guard.
    ///
    /// `tls.hew` declares its own `TLS_STATUS_*` constants (there is no
    /// shared Rust/Hew constant for this ABI-level status code) that must
    /// stay numerically identical to this file's `TLS_STATUS_*` consts, or
    /// the Hew-side `tls_error_from_status` dispatch silently misroutes.
    /// This test parses the `.hew` declarations via `include_str!` and pins
    /// each literal against the Rust-side value.
    #[test]
    fn hew_binding_pins_tls_status_constants() {
        let hew_src = include_str!("../../std/net/tls/tls.hew");

        let pins: &[(&str, c_int)] = &[
            ("TLS_STATUS_SUCCESS", TLS_STATUS_SUCCESS),
            ("TLS_STATUS_RETRYABLE", TLS_STATUS_RETRYABLE),
            ("TLS_STATUS_TLS_ERROR", TLS_STATUS_TLS_ERROR),
            ("TLS_STATUS_IO_ERROR", TLS_STATUS_IO_ERROR),
        ];

        for (name, expected) in pins {
            let decl = hew_src
                .lines()
                .map(str::trim)
                .find(|line| line.starts_with(&format!("const {name}:")))
                .unwrap_or_else(|| panic!("tls.hew must declare `const {name}`"));
            let literal = decl
                .rsplit('=')
                .next()
                .unwrap_or(decl)
                .trim()
                .trim_end_matches(';')
                .trim();
            let parsed: c_int = literal
                .parse()
                .unwrap_or_else(|e| panic!("tls.hew `{name}` value {literal:?} not an int: {e}"));
            assert_eq!(
                parsed, *expected,
                "tls.hew `{name}` = {parsed} must match Rust `{name}` = {expected}"
            );
        }
    }

    /// A TLS error recorded by the REAL `hew_tls_connect` failure path —
    /// while a given actor is the installed dispatch context on OS thread A —
    /// must be readable through the REAL public `hew_tls_last_error`
    /// accessor when that SAME actor is the installed dispatch context on a
    /// DIFFERENT OS thread B.
    ///
    /// This is the actual #2659 regression: an actor parked mid-connect and
    /// resumed on another scheduler worker must not lose its recorded error.
    /// Driving the module's own producer (`hew_tls_connect`) and public
    /// accessor (`hew_tls_last_error`) — rather than poking the shared
    /// `parse_error_slot` map directly — means this test is RED against the
    /// predecessor `thread_local!` implementation: a plain `thread_local!`
    /// slot is intrinsically per-OS-thread storage, so thread B's slot would
    /// stay empty no matter which actor either thread believes is
    /// dispatching. It is GREEN only because `set_/get_tls_last_error` now
    /// key on actor identity via `parse_error_slot`.
    ///
    /// Run 3× to satisfy the flake gate.
    #[test]
    fn tls_error_visible_across_worker_threads_regression_2659() {
        use crate::net_error_slot_test_support::{
            spawn_error_slot_test_actor, with_actor_context, NetErrorSlotRuntimeGuard,
        };

        // hew_actor_spawn requires an installed runtime authority; shared
        // across tls/smtp/quic so their regression tests serialize on the
        // single process-global scheduler slot instead of racing.
        let _runtime = NetErrorSlotRuntimeGuard::new();

        for run in 0..3_u32 {
            let test_actor = spawn_error_slot_test_actor();
            assert!(!test_actor.is_null(), "test actor should spawn");
            let actor_addr = test_actor as usize;

            let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
            let barrier2 = barrier.clone();

            let handle = thread::spawn(move || {
                // Simulate: the actor resumes on thread B, a different OS
                // thread than the one that recorded the error.
                barrier2.wait();
                let actor_ptr = actor_addr as *mut hew_runtime::actor::HewActor;
                with_actor_context(actor_ptr, last_error_string)
            });

            // Thread A: install the SAME actor as the dispatch context and
            // drive the real hew_tls_connect failure path — the actual
            // producer, not a direct slot poke.
            with_actor_context(test_actor, || {
                // SAFETY: passing null is the documented failure path.
                let ptr = unsafe { hew_tls_connect(std::ptr::null(), 443) };
                assert!(ptr.is_null());
            });
            barrier.wait();

            let result = handle.join().expect("thread B panicked");
            assert_eq!(
                result, "hew_tls_connect: invalid host string",
                "run {run}: TLS error recorded on thread A must be visible on thread B for the same actor"
            );

            // SAFETY: test_actor was spawned above and not yet stopped/freed.
            unsafe { actor::hew_actor_stop(test_actor) };
            // hew_actor_free reaps every parse_error_slot entry for this
            // actor via parse_error_slot::clear_all_for_actor — no manual
            // clear needed.
            // SAFETY: test_actor is stopped immediately above; free reclaims
            // it exactly once.
            assert_eq!(unsafe { actor::hew_actor_free(test_actor) }, 0);
        }
    }
}
