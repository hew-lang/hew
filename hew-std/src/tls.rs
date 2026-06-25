//! Hew `std::net::tls` — TLS client connections.
//!
//! Exposes a blocking C ABI over rustls for TLS client connections.
//! All `extern "C"` functions are designed to be called from compiled Hew
//! programs via FFI. Any string returned by [`hew_tls_last_error`] is allocated
//! with `libc::malloc`; callers must free it with `libc::free`.
use std::cell::RefCell;
use std::ffi::c_void;
use std::io::{self, Read, Write};
use std::net::TcpStream;
use std::os::raw::{c_char, c_int};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use hew_cabi::cabi::{alloc_cstring, cstr_to_str, malloc_bytes, str_to_malloc};
use hew_cabi::vec::HewVec;
use hew_runtime::bytes::BytesTriple;
use rustls::pki_types::ServerName;
use rustls::RootCertStore;

type BoxError = Box<dyn std::error::Error + Send + Sync + 'static>;

/// Maximum bytes read in a single [`hew_tls_read`] call.
const READ_BUFFER_SIZE: usize = 65_536;
const TLS_STATUS_SUCCESS: c_int = 0;
const TLS_STATUS_RETRYABLE: c_int = 1;
const TLS_STATUS_TLS_ERROR: c_int = 2;
const TLS_STATUS_IO_ERROR: c_int = 3;

std::thread_local! {
    static LAST_TLS_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
}

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
type TlsInner = Arc<TlsShared>;

pub struct TlsShared {
    stream: Mutex<Option<rustls::StreamOwned<rustls::ClientConnection, TcpStream>>>,
    closed: AtomicBool,
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

#[repr(C)]
#[derive(Debug)]
pub struct HewTlsWriteResult {
    written: c_int,
    status: c_int,
}

#[repr(C)]
#[derive(Debug)]
pub struct HewTlsReadResult {
    data: HewVec,
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
        }),
    })
}

fn set_tls_last_error(msg: impl Into<String>) {
    LAST_TLS_ERROR.with(|error| *error.borrow_mut() = Some(msg.into()));
}

fn clear_tls_last_error() {
    LAST_TLS_ERROR.with(|error| *error.borrow_mut() = None);
}

fn get_tls_last_error() -> String {
    LAST_TLS_ERROR.with(|error| error.borrow().as_ref().cloned().unwrap_or_else(String::new))
}

fn empty_hew_vec() -> HewVec {
    // SAFETY: `layout`/`elem_layout` are null so neither inline storage is read;
    // zeroed bytes are a valid (unused) initialiser for the storage fields.
    let layout_storage = unsafe { core::mem::zeroed() };
    // SAFETY: see above — `elem_layout` is null, so the storage is never read.
    let elem_layout_storage = unsafe { core::mem::zeroed() };
    HewVec {
        data: std::ptr::null_mut(),
        len: 0,
        cap: 0,
        elem_size: 1,
        elem_kind: hew_cabi::vec::ElemKind::Plain,
        layout: std::ptr::null(),
        layout_storage,
        elem_layout: std::ptr::null(),
        elem_layout_storage,
    }
}

fn build_hew_vec(bytes: &[u8]) -> Option<HewVec> {
    let len = bytes.len();
    let ptr = malloc_bytes(bytes);
    if ptr.is_null() {
        return None;
    }
    // SAFETY: `layout`/`elem_layout` are null so neither inline storage is read;
    // zeroed bytes are a valid (unused) initialiser for the storage fields.
    let layout_storage = unsafe { core::mem::zeroed() };
    // SAFETY: see above — `elem_layout` is null, so the storage is never read.
    let elem_layout_storage = unsafe { core::mem::zeroed() };
    Some(HewVec {
        data: ptr,
        len,
        cap: len.max(1),
        elem_size: 1,
        elem_kind: hew_cabi::vec::ElemKind::Plain,
        layout: std::ptr::null(),
        layout_storage,
        elem_layout: std::ptr::null(),
        elem_layout_storage,
    })
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
    let empty = empty_hew_vec();
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
            if let Some(data) = build_hew_vec(&buf[..n]) {
                clear_tls_last_error();
                HewTlsReadResult {
                    data,
                    status: TLS_STATUS_SUCCESS,
                }
            } else {
                set_tls_last_error("hew_tls_read: allocation failed");
                HewTlsReadResult {
                    data: empty,
                    status: TLS_STATUS_IO_ERROR,
                }
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
        return std::ptr::null_mut();
    };
    let Ok(port_u16) = u16::try_from(port) else {
        return std::ptr::null_mut();
    };
    match connect_tls(host_str, port_u16) {
        Ok(stream) => Box::into_raw(Box::new(stream)),
        Err(_) => std::ptr::null_mut(),
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
/// Returns a `HewVec` containing the bytes read. `out_status` receives
/// `0` for success or orderly EOF, `1` for retryable would-block/timeout
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
) -> HewVec {
    if !write_out_status(out_status, TLS_STATUS_IO_ERROR) {
        set_tls_last_error("hew_tls_read: invalid status buffer");
        return empty_hew_vec();
    }
    if stream.is_null() {
        set_tls_last_error("hew_tls_read: invalid stream");
        return empty_hew_vec();
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
        return empty_hew_vec();
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
/// (`is_bytes_by_pointer_consumer` in codegen passes the alloca address of the
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

/// Close the TLS connection and free the stream handle.
///
/// Sets the `closed` flag so any attached reader thread exits at its next loop
/// boundary. Drops the inner stream, which closes the underlying TCP socket.
/// The reader observes the closed flag or loses the mutex and exits cleanly.
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
    // Drop the inner stream to close the TCP socket; the reader thread will
    // observe either the closed flag or `None` from the mutex and exit.
    // Clone the Arc first so the lock guard's lifetime is not bound to `s`.
    let inner = Arc::clone(&s.inner);
    drop(s);
    // SAFETY: the Arc ref outlives the guard; adding `;` to avoid the
    // compiler thinking the Result temporary can escape past `inner`'s drop.
    let _ = inner.stream.lock().map(|mut g| {
        *g = None;
    });
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
// through the runtime ABI (`hew_actor_ref_is_alive`). The reader thread extends
// the required actor lifetime — the caller MUST NOT free the actor before the
// attached TLS stream is closed (the runtime's drain_actors enforces this).
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
) {
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
        if let Ok(mut guard) = inner.stream.lock() {
            *guard = None;
        }
    });
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
    // SAFETY: `actor` points to a valid HewActorRef for the duration of this
    // call; we snapshot the ref by value so the reader owns its own copy.
    let actor_ref = unsafe { std::ptr::read(actor.cast::<TlsActorRef>()) };
    spawn_tls_attach_reader(Arc::clone(&s.inner), actor_ref, on_data_type, on_close_type);
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;
    use std::ffi::CStr;
    use std::io::ErrorKind;
    use std::net::TcpListener;
    use std::os::raw::c_void;
    use std::thread;
    use std::time::Duration;

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

    fn vec_bytes(vec: &HewVec) -> Vec<u8> {
        if vec.len == 0 || vec.data.is_null() {
            return Vec::new();
        }
        // SAFETY: non-empty `HewVec` values produced by this module store `len`
        // readable bytes at `data`.
        unsafe { std::slice::from_raw_parts(vec.data.cast::<u8>(), vec.len) }.to_vec()
    }

    fn free_vec(vec: &HewVec) {
        if !vec.data.is_null() {
            // SAFETY: test vectors are allocated with `libc::malloc` in `build_hew_vec`.
            unsafe { libc::free(vec.data.cast::<c_void>()) }; // CSTRING-FREE: libc-bytes (test build_hew_vec data = malloc_bytes)
        }
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
        // SAFETY: passing null is the test.
        let ptr = unsafe { hew_tls_connect(std::ptr::null(), 443) };
        assert!(ptr.is_null());
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
        let vec = unsafe { hew_tls_read(std::ptr::null_mut(), 1024, &raw mut status) };
        assert_eq!(vec.len, 0);
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
        let host = std::ffi::CString::new("").unwrap();
        // SAFETY: passing valid but empty C string.
        let ptr = unsafe { hew_tls_connect(host.as_ptr(), 443) };
        assert!(ptr.is_null());
    }

    #[test]
    fn connect_negative_port_returns_null() {
        let host = std::ffi::CString::new("example.com").unwrap();
        // SAFETY: passing valid host with invalid port.
        let ptr = unsafe { hew_tls_connect(host.as_ptr(), -1) };
        assert!(ptr.is_null());
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
        assert!(result.data.data.is_null());
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
        });
        // The stream is None and closed — a reader would exit immediately.
        let guard = shared.stream.lock().unwrap();
        assert!(guard.is_none());
        assert!(shared.closed.load(Ordering::Acquire));
    }
}
