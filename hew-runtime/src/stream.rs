//! Hew runtime: first-class Stream<T> and Sink<T> handles.
//!
//! A `Stream<T>` is a readable sequential source; a `Sink<T>` is a writable
//! sequential destination. Both are move-only, opaque, and backed by one of
//! several implementations:
//!
//! - **channel** — bounded in-memory ring buffer (backpressure on write)
//! - **file-read** — file opened for reading, chunks returned on demand
//! - **file-write** — file opened for writing, bytes flushed on demand
//! - **vec** — drains an existing byte buffer
//! - **tcp** — wraps a TCP socket (Phase 5: net.connect integration)
//!
//! ## ABI conventions
//!
//! All functions use `#[no_mangle] extern "C"` with opaque `*mut c_void`
//! pointers for the stream/sink handles. Items are transferred as
//! header-aware string allocations (for the string-element entries) or
//! malloc-allocated byte buffers (for the bytes-element / raw-byte entries):
//!
//! - `hew_stream_next_layout` / `hew_stream_try_next_layout` /
//!   `hew_stream_pop_layout` are the recv entries: an element-layout witness
//!   selects the envelope decode (string elements materialise a refcount-1,
//!   NUL-terminated `*mut c_char` via
//!   [`crate::channel_common::bytes_to_cstr`] the MIR drop spine releases
//!   through `hew_string_drop`; bytes elements a fresh refcounted
//!   `BytesTriple`; Plain/owned elements decode into the out slot directly).
//! - `hew_sink_write` accepts a pointer+size; the runtime copies the bytes.
//! - `hew_stream_channel` returns a `HewStreamPair*`; extract the two handles
//!   with `hew_stream_pair_sink` / `hew_stream_pair_stream`, then free the pair
//!   with `hew_stream_pair_free`.
//!
//! ## RAII / Drop safety
//!
//! All handle types implement `Drop`: streams and sinks are automatically
//! closed when dropped. Explicit `.close()` is available for early release
//! but is not required for correctness. `HewStreamPair` drops any handles
//! that were not extracted by the caller.
//!
//! ## Thread safety
//!
//! A channel's Sink and Stream may live in different actors / threads. All
//! other stream types are single-owner and may not be shared across threads.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::collections::VecDeque;
use std::ffi::{c_char, c_int, c_void, CStr};
use std::fs;
use std::io::{BufReader, Read};
use std::net::TcpStream;
use std::ptr;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

// ── Re-export sink types from hew-cabi ────────────────────────────────────────
// These are the shared ABI types that native packages (e.g. HTTP) also use.
// Defining them in hew-cabi avoids pulling the full runtime into stdlib packages.

pub use crate::stream_error::{
    hew_stream_last_error, set_last_error, set_last_error_with_errno, take_last_error,
};
pub use hew_cabi::sink::{into_sink_ptr, into_write_sink_ptr, HewSink};

// hew_stream_last_error / hew_stream_last_errno are defined in crate::stream_error
// (the single owner of the C ABI); hew-cabi only declares them as imports so that
// native packages resolve them against libhew.a at final link.

use hew_cabi::vec::HewVec;

/// Returns 1 if the stream pointer is non-null (valid), 0 otherwise.
#[no_mangle]
pub extern "C" fn hew_stream_is_valid(stream: *const HewStream) -> i32 {
    i32::from(!stream.is_null())
}

/// Returns 1 if the sink pointer is non-null (valid), 0 otherwise.
#[no_mangle]
pub extern "C" fn hew_sink_is_valid(sink: *const HewSink) -> i32 {
    i32::from(!sink.is_null())
}

// ── Item envelope ────────────────────────────────────────────────────────────

/// Raw bytes item transferred through a stream.
type Item = Vec<u8>;

// ── Backing traits ────────────────────────────────────────────────────────────

trait StreamBacking: Send + std::fmt::Debug {
    /// Return the next item, or `None` on EOF. Blocks until an item is available.
    fn next(&mut self) -> Option<Item>;
    /// Non-blocking item poll. Returns `Some(item)` if one is immediately
    /// available, or `None` if the stream is empty or closed. The default
    /// falls back to `next()` (blocking); override for genuine non-blocking
    /// behaviour.
    fn try_next(&mut self) -> Option<Item> {
        // Deliberately blocking default — callers that need non-blocking
        // semantics must be created with a backing that overrides this method
        // (e.g. ChannelStream). Other backings (file, TCP) do not support
        // non-blocking reads, so returning None would be misleading; falling
        // back to blocking is safer until a poll-based abstraction is added.
        self.next()
    }
    /// Discard remaining items and signal done to the producer.
    fn close(&mut self);
    /// Check if the stream is exhausted without consuming an item.
    fn is_closed(&self) -> bool;
}

// HewSink is defined in hew_cabi::sink and re-exported above.

// ── Public handle types ────────────────────────────────────────────────────────

/// Opaque readable stream handle.
#[derive(Debug)]
pub struct HewStream {
    inner: Box<dyn StreamBacking>,
    /// Whether `close()` has already been called on the backing.
    closed: bool,
    /// Active pending-read id, or 0 if none. Set by `hew_stream_poll`,
    /// cleared by either the park thread on completion or by
    /// `hew_stream_cancel_pending_read`. The "at most one pending read
    /// per stream" invariant is enforced by CAS on this field.
    pending_read: AtomicU64,
    /// Shared state with the park thread spawned by `hew_stream_poll`.
    /// `None` when no poll is active. Guarded so cancel can acquire
    /// the same lock the park thread will check before firing the
    /// callback (closes the TOCTOU window where an item is received
    /// but the callback has not yet fired when cancel arrives).
    pending_state: Mutex<Option<Arc<Mutex<ParkState>>>>,
    /// Test-only: incremented each time the park thread exits (Done or
    /// Cancelled). Allows tests to wait deterministically for a specific
    /// park-thread generation rather than sleeping (§5.7).
    #[cfg(test)]
    park_exit_gen: Arc<(Mutex<u64>, std::sync::Condvar)>,
    /// The suspending channel core (NEW-7) when this stream is the read half of
    /// an in-memory pipe; `None` for every other backing. Shared by `Arc` with
    /// the paired sink so `await stream.recv()` can park + be woken by the
    /// producer's `await sink.send()`. Non-channel backings keep the blocking
    /// read path (no parkable producer to wake).
    channel: Option<Arc<crate::channel_core::ChannelCore>>,
}

impl Drop for HewStream {
    fn drop(&mut self) {
        if !self.closed {
            self.inner.close();
        }
    }
}

/// Park-thread state machine for `hew_stream_poll`.
///
/// State machine:
///   Pending  → Cancelled  by `hew_stream_cancel_pending_read` before `next()` returns
///   Pending  → Done       by the park thread after `next()` returns; callback fires
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParkState {
    Pending,
    Cancelled,
    Done,
}

/// Monotonic counter for [`PendingReadId`]; never reuses an id within a process.
static NEXT_PENDING_READ_ID: AtomicU64 = AtomicU64::new(1);

/// Opaque id identifying a pending read on a stream.
///
/// Returned by [`hew_stream_poll`]; passed to
/// [`hew_stream_cancel_pending_read`] to withdraw a registration. A
/// value of `0` is the error sentinel and never identifies a live
/// pending read. Ids are not reused for the lifetime of the process.
pub type PendingReadId = u64;

// HewSink is defined in hew_cabi::sink and re-exported above.

/// Pair returned by channel/tcp creation.  Extract handles, then free.
#[derive(Debug)]
pub struct HewStreamPair {
    pub sink: *mut HewSink,
    pub stream: *mut HewStream,
}

impl Drop for HewStreamPair {
    fn drop(&mut self) {
        // Drop any handles that weren't extracted by the caller.
        if !self.sink.is_null() {
            // SAFETY: sink was allocated with Box::into_raw and is still owned.
            unsafe { drop(Box::from_raw(self.sink)) }; // ALLOCATOR-PAIRING: GlobalAlloc
            self.sink = std::ptr::null_mut();
        }
        if !self.stream.is_null() {
            // SAFETY: stream was allocated with Box::into_raw and is still owned.
            unsafe { drop(Box::from_raw(self.stream)) }; // ALLOCATOR-PAIRING: GlobalAlloc
            self.stream = std::ptr::null_mut();
        }
    }
}

// SAFETY: HewStreamPair is only used to transfer two owned Box pointers
// across the channel-creation boundary; it is not shared between threads.
unsafe impl Send for HewStreamPair {}

// ── Channel backing ───────────────────────────────────────────────────────────

#[derive(Debug)]
struct ChannelStream {
    core: Arc<crate::channel_core::ChannelCore>,
}

impl StreamBacking for ChannelStream {
    fn next(&mut self) -> Option<Item> {
        // Default (non-suspending) callers block the foreign thread on the core
        // condvar. Suspending callers never reach here — they go through
        // `hew_stream_await_next` + `hew_stream_pop_bytes`.
        self.core.blocking_recv()
    }

    fn try_next(&mut self) -> Option<Item> {
        self.core.try_recv()
    }

    fn close(&mut self) {
        // Consumer-side close: local cancel/discard. Wakes parked producers.
        self.core.close_stream();
    }

    fn is_closed(&self) -> bool {
        // Channels don't know they are closed until they try to receive.
        false
    }
}

impl StreamBacking for VecStream {
    fn next(&mut self) -> Option<Item> {
        self.items.pop_front()
    }

    fn try_next(&mut self) -> Option<Item> {
        // VecStream is in-memory and never blocks; try_next is identical to next.
        self.items.pop_front()
    }

    fn close(&mut self) {
        self.items.clear();
    }

    fn is_closed(&self) -> bool {
        self.items.is_empty()
    }
}

// ── File-read backing ─────────────────────────────────────────────────────────

#[derive(Debug)]
struct FileReadStream {
    reader: BufReader<fs::File>,
    chunk_size: usize,
}

impl StreamBacking for FileReadStream {
    fn next(&mut self) -> Option<Item> {
        let mut buf = vec![0u8; self.chunk_size];
        match self.reader.read(&mut buf) {
            Ok(n) if n > 0 => {
                buf.truncate(n);
                Some(buf)
            }
            _ => None,
        }
    }

    fn close(&mut self) {
        // File handle is dropped with the struct.
    }

    fn is_closed(&self) -> bool {
        // File streams don't know they are at EOF until they try to read.
        false
    }
}

// ── TCP read backing ──────────────────────────────────────────────────────────
//
// A `StreamBacking` that reads from a cloned `TcpStream` handle.  Created
// exclusively by `hew_tcp_stream_from_conn` — never directly by user code.
//
// Thread-safety: the struct is `Send` (TcpStream is Send), so it is safe
// to drive from the park thread inside `hew_stream_poll`.  It is NOT `Sync`;
// at most one reader exists at any point (the park thread).
//
// Blocking behaviour: `next` blocks until data arrives, the peer closes,
// or the OS returns an error.  If the caller wants a deadline, they call
// `conn.set_read_timeout(ms)` on the `Connection` before calling
// `into_stream_sink` — the timeout is inherited by the clone.
// See Risk R1 in the R45 plan: callers composing this inside `select{}`
// SHOULD set a read timeout on the connection to avoid an uninterruptible
// park-thread block.

#[cfg(not(target_arch = "wasm32"))]
#[derive(Debug)]
struct TcpStreamBacking {
    stream: TcpStream,
}

/// TCP read backing size, matching `hew_tcp_read`'s buffer (transport.rs:1216).
#[cfg(not(target_arch = "wasm32"))]
const TCP_BACKING_BUF_SIZE: usize = 8192;

#[cfg(not(target_arch = "wasm32"))]
impl StreamBacking for TcpStreamBacking {
    fn next(&mut self) -> Option<Item> {
        let mut buf = [0u8; TCP_BACKING_BUF_SIZE];
        loop {
            match self.stream.read(&mut buf) {
                Ok(0) => {
                    // Peer closed the connection — clean EOF.
                    return None;
                }
                Ok(n) => return Some(buf[..n].to_vec()),
                Err(e) => {
                    // Record the error kind in the transport counters.
                    crate::transport::record_tcp_error_kind(e.kind());

                    if e.kind() == std::io::ErrorKind::Interrupted {
                        // POSIX EINTR: the read was interrupted by a signal.
                        // Retry immediately — EINTR is fully recoverable and
                        // must not be surfaced as a stream error or pause.
                    } else {
                        // EAGAIN / EWOULDBLOCK: socket is non-blocking or a
                        // read timeout fired with no data available.  Set a
                        // distinct errno so callers can call
                        // hew_stream_last_errno() to distinguish this pause
                        // from clean EOF (where errno stays 0).
                        // WouldBlock maps to EAGAIN; TimedOut to ETIMEDOUT.
                        // Persistent errors use the raw OS errno.
                        let (msg, raw) = match e.kind() {
                            std::io::ErrorKind::WouldBlock => {
                                (format!("TCP read would block: {e}"), libc::EAGAIN)
                            }
                            std::io::ErrorKind::TimedOut => {
                                (format!("TCP read timed out: {e}"), libc::ETIMEDOUT)
                            }
                            _ => (
                                format!("TCP read error: {e}"),
                                e.raw_os_error().unwrap_or(0),
                            ),
                        };
                        set_last_error_with_errno(msg, raw);
                        return None;
                    }
                }
            }
        }
    }

    fn close(&mut self) {
        // The cloned TcpStream fd closes when this struct is dropped.
        // No explicit action needed; document the drop-on-drop contract:
        //   TcpStreamBacking::drop → TcpStream::drop → OS fd table entry removed.
        // Callers that want eager half-close should set a read timeout
        // before bridging (Risk R1 in the R45 plan).
    }

    fn is_closed(&self) -> bool {
        // TCP streams can't know they're at EOF without attempting a read.
        // This matches FileReadStream's posture.
        false
    }
}

// ── Vec backing (drain) ────────────────────────────────────────────────────────

#[derive(Debug)]
struct VecStream {
    items: VecDeque<Item>,
}

// ── Lines adapter backing ─────────────────────────────────────────────────────

/// Maximum bytes the lines adapter will buffer before yielding a truncated
/// line.  Prevents unbounded memory growth when the upstream never sends a
/// newline (e.g. binary data or a malicious sender).
const MAX_LINE_BUFFER_SIZE: usize = 1024 * 1024; // 1 MiB

/// Wraps a `Stream<bytes>` and yields newline-terminated strings (as utf-8 bytes).
#[derive(Debug)]
struct LinesStream {
    /// Unconsumed bytes from the upstream stream.
    buf: Vec<u8>,
    /// Upstream bytes stream.
    upstream: Box<dyn StreamBacking>,
    done: bool,
    /// After a forced flush at the buffer limit, skip a leading line
    /// delimiter (`\n` or `\r\n`) that belongs to the oversized line.
    skip_next_delimiter: bool,
}

impl StreamBacking for LinesStream {
    fn next(&mut self) -> Option<Item> {
        loop {
            // After a forced flush, consume the delimiter that terminated
            // the oversized line (it may have arrived in a later chunk).
            if self.skip_next_delimiter {
                if self.buf.starts_with(b"\r\n") {
                    self.buf.drain(..2);
                    self.skip_next_delimiter = false;
                } else if self.buf.first() == Some(&b'\n') {
                    self.buf.remove(0);
                    self.skip_next_delimiter = false;
                } else if self.buf.first() == Some(&b'\r') && !self.done {
                    // Lone \r — could be the start of \r\n split across
                    // chunks.  Need more data before deciding.
                } else if !self.buf.is_empty() || self.done {
                    // Non-delimiter data or EOF — no delimiter to skip.
                    self.skip_next_delimiter = false;
                }
                // else: buf empty, stream open — fall through to pull more.
            }

            // Check if there's a complete line already buffered.
            if let Some(pos) = self.buf.iter().position(|&b| b == b'\n') {
                let mut line: Vec<u8> = self.buf.drain(..=pos).collect();
                // Strip the trailing newline delimiter (and \r for CRLF).
                if line.last() == Some(&b'\n') {
                    line.pop();
                }
                if line.last() == Some(&b'\r') {
                    line.pop();
                }
                return Some(line);
            }
            // Buffer full without a newline — drain exactly the limit to
            // bound memory.  Leftover bytes stay in buf for the next call.
            if self.buf.len() >= MAX_LINE_BUFFER_SIZE {
                let line: Vec<u8> = self.buf.drain(..MAX_LINE_BUFFER_SIZE).collect();
                self.skip_next_delimiter = true;
                return Some(line);
            }
            if self.done {
                // Flush remaining bytes as the last "line" even without newline.
                if self.buf.is_empty() {
                    return None;
                }
                return Some(std::mem::take(&mut self.buf));
            }
            // Pull more bytes from upstream.
            match self.upstream.next() {
                Some(chunk) => self.buf.extend_from_slice(&chunk),
                None => {
                    self.done = true;
                }
            }
        }
    }

    fn close(&mut self) {
        self.upstream.close();
        self.buf.clear();
        self.done = true;
    }

    fn is_closed(&self) -> bool {
        self.done && self.buf.is_empty()
    }
}

// ── Chunks adapter backing ────────────────────────────────────────────────────

#[derive(Debug)]
struct ChunksStream {
    buf: Vec<u8>,
    chunk_size: usize,
    upstream: Box<dyn StreamBacking>,
    done: bool,
}

impl StreamBacking for ChunksStream {
    fn next(&mut self) -> Option<Item> {
        while self.buf.len() < self.chunk_size && !self.done {
            match self.upstream.next() {
                Some(chunk) => self.buf.extend_from_slice(&chunk),
                None => self.done = true,
            }
        }
        if self.buf.is_empty() {
            return None;
        }
        let n = self.chunk_size.min(self.buf.len());
        let chunk: Vec<u8> = self.buf.drain(..n).collect();
        Some(chunk)
    }

    fn close(&mut self) {
        self.upstream.close();
        self.buf.clear();
        self.done = true;
    }

    fn is_closed(&self) -> bool {
        self.done && self.buf.is_empty()
    }
}

// ── Helper: box stream / sink into raw pointers ───────────────────────────────

fn into_stream_ptr(backing: impl StreamBacking + 'static) -> *mut HewStream {
    Box::into_raw(Box::new(HewStream {
        // ALLOCATOR-PAIRING: GlobalAlloc
        inner: Box::new(backing),
        closed: false,
        pending_read: AtomicU64::new(0),
        pending_state: Mutex::new(None),
        #[cfg(test)]
        park_exit_gen: Arc::new((Mutex::new(0u64), std::sync::Condvar::new())),
        channel: None,
    }))
}

/// Like [`into_stream_ptr`] but accepts an already-boxed backing.
fn into_stream_ptr_dyn(backing: Box<dyn StreamBacking>) -> *mut HewStream {
    Box::into_raw(Box::new(HewStream {
        // ALLOCATOR-PAIRING: GlobalAlloc
        inner: backing,
        closed: false,
        pending_read: AtomicU64::new(0),
        pending_state: Mutex::new(None),
        #[cfg(test)]
        park_exit_gen: Arc::new((Mutex::new(0u64), std::sync::Condvar::new())),
        channel: None,
    }))
}

/// Consume a `HewStream` pointer, extract its inner backing, and free the
/// outer allocation.  Equivalent to `Box::from_raw` + field move, but marks
/// the stream as closed first so `HewStream::drop` won't double-close.
///
/// # Safety
///
/// `stream` must be a valid `HewStream` pointer allocated via `Box::into_raw`.
unsafe fn consume_stream_inner(stream: *mut HewStream) -> Box<dyn StreamBacking> {
    // SAFETY: stream is a valid HewStream pointer per the function contract.
    let inner = unsafe { ptr::read(&raw const (*stream).inner) };
    // SAFETY: read+drop the non-Copy auxiliary fields so they release their
    // resources (the Mutex<Option<Arc<...>>> in particular owns heap state).
    // `closed` and `pending_read` are POD, but going through ptr::read+drop is
    // uniform and cheap.
    unsafe {
        // closed and pending_read are Copy / POD — reading them is a
        // no-op release, but we go through ptr::read for layout
        // uniformity. pending_state owns heap state and must run Drop.
        let _ = ptr::read(&raw const (*stream).closed);
        let _ = ptr::read(&raw const (*stream).pending_read);
        drop(ptr::read(&raw const (*stream).pending_state));
        #[cfg(test)]
        drop(ptr::read(&raw const (*stream).park_exit_gen));
        drop(ptr::read(&raw const (*stream).channel));
    }
    // SAFETY: stream was allocated via Box::into_raw(Box::new(HewStream { .. })),
    // so deallocating with Layout::new::<HewStream>() is correct. We use dealloc
    // instead of Box::from_raw to avoid running Drop (which would double-free inner).
    unsafe {
        std::alloc::dealloc(stream.cast::<u8>(), std::alloc::Layout::new::<HewStream>());
    }
    inner
}

// into_sink_ptr is defined in hew_cabi::sink and re-exported above.

// ── Stream transform infrastructure ────────────────────────────────────────────
//
// The four stream transform adapters (map/filter × string/bytes) share identical
// struct layout, Send safety, Drop behaviour, and C ABI entry-point patterns.
// Two macros eliminate this duplication while keeping the `next()` methods
// explicit since their marshalling logic genuinely differs.

/// Defines a map-style transform stream struct with Send, Drop, and trivial
/// `close`/`is_closed` delegating to upstream.
macro_rules! define_map_stream {
    ($name:ident, $fn_type:ty) => {
        #[derive(Debug)]
        struct $name {
            upstream: Box<dyn StreamBacking>,
            fn_ptr: $fn_type,
            env_ptr: *const c_void,
        }

        // SAFETY: fn_ptr is a plain function pointer; env_ptr is an RC'd closure
        // environment that is only accessed from one thread at a time.
        unsafe impl Send for $name {}

        impl Drop for $name {
            fn drop(&mut self) {
                // SAFETY: env_ptr is an RC'd block; decrement its reference count.
                unsafe { rc_drop_env(self.env_ptr) };
            }
        }
    };
}

/// Defines a filter-style transform stream struct with a `done` flag,
/// Send, Drop, and filter-aware `close`/`is_closed`.
macro_rules! define_filter_stream {
    ($name:ident, $fn_type:ty) => {
        #[derive(Debug)]
        struct $name {
            upstream: Box<dyn StreamBacking>,
            fn_ptr: $fn_type,
            env_ptr: *const c_void,
            done: bool,
        }

        // SAFETY: fn_ptr is a plain function pointer; env_ptr is an RC'd closure
        // environment that is only accessed from one thread at a time.
        unsafe impl Send for $name {}

        impl Drop for $name {
            fn drop(&mut self) {
                // SAFETY: env_ptr is an RC'd block; decrement its reference count.
                unsafe { rc_drop_env(self.env_ptr) };
            }
        }
    };
}

// ── Map adapter (string) ──────────────────────────────────────────────────────

/// Calling convention matching the Hew closure ABI: (env, string) → owned C string.
type StringMapFn = unsafe extern "C" fn(*const c_void, *const c_char) -> *mut c_char;

define_map_stream!(MapStringStream, StringMapFn);

impl StreamBacking for MapStringStream {
    fn next(&mut self) -> Option<Item> {
        let item = self.upstream.next()?;
        let mut with_nul = item.clone();
        with_nul.push(0);
        // SAFETY: fn_ptr is a valid Hew closure, env_ptr is its environment.
        let result_ptr = unsafe { (self.fn_ptr)(self.env_ptr, with_nul.as_ptr().cast::<c_char>()) };
        if result_ptr.is_null() {
            return Some(Vec::new());
        }
        // SAFETY: result_ptr is a valid null-terminated C string from the closure.
        let result_cstr = unsafe { std::ffi::CStr::from_ptr(result_ptr) };
        let bytes = result_cstr.to_bytes().to_vec();
        // SAFETY: result_ptr is the closure's header-aware Hew string; we own it.
        unsafe { crate::cabi::free_cstring(result_ptr) }; // CSTRING-FREE: str-open (StringMapFn closure output = Hew (string)->string; header-aware in S1)
        Some(bytes)
    }

    fn close(&mut self) {
        self.upstream.close();
    }

    fn is_closed(&self) -> bool {
        self.upstream.is_closed()
    }
}

// ── Filter adapter (string) ───────────────────────────────────────────────────

/// Calling convention: (env, string) → i32 (non-zero means keep the item).
type StringFilterFn = unsafe extern "C" fn(*const c_void, *const c_char) -> i32;

define_filter_stream!(FilterStringStream, StringFilterFn);

impl StreamBacking for FilterStringStream {
    fn next(&mut self) -> Option<Item> {
        loop {
            if self.done {
                return None;
            }
            let item = self.upstream.next()?;
            let mut with_nul = item.clone();
            with_nul.push(0);
            // SAFETY: fn_ptr is a valid Hew closure, env_ptr is its environment.
            let keep = unsafe { (self.fn_ptr)(self.env_ptr, with_nul.as_ptr().cast::<c_char>()) };
            if keep != 0 {
                return Some(item);
            }
        }
    }

    fn close(&mut self) {
        self.done = true;
        self.upstream.close();
    }

    fn is_closed(&self) -> bool {
        self.done || self.upstream.is_closed()
    }
}

// ── Map adapter (bytes) ───────────────────────────────────────────────────────

/// Calling convention for bytes map closures.
///
/// The closure receives a `*mut HewVec` (bytes) and returns a new `*mut HewVec`.
/// Both the input and the return value are owned: the closure frees the input,
/// and the caller owns the returned vec.
type BytesMapFn = unsafe extern "C" fn(*const c_void, *mut HewVec) -> *mut HewVec;

define_map_stream!(MapBytesStream, BytesMapFn);

impl StreamBacking for MapBytesStream {
    fn next(&mut self) -> Option<Item> {
        let item = self.upstream.next()?;
        // SAFETY: u8_to_hwvec allocates a fresh HewVec.
        let input_vec = unsafe { hew_cabi::vec::u8_to_hwvec(&item) };
        // SAFETY: fn_ptr is a valid Hew closure, env_ptr is its environment.
        let result_vec = unsafe { (self.fn_ptr)(self.env_ptr, input_vec) };
        // Free the input vec when the closure returned a new allocation.
        if result_vec != input_vec {
            // SAFETY: input_vec was allocated by u8_to_hwvec and is no longer
            // referenced — the closure returned a different allocation.
            unsafe { hew_cabi::vec::hew_vec_free(input_vec) };
        }
        if result_vec.is_null() {
            return Some(Vec::new());
        }
        // SAFETY: result_vec is a valid HewVec returned by the closure.
        let result_bytes = unsafe { hew_cabi::vec::hwvec_to_u8(result_vec) };
        // SAFETY: result_vec was allocated by the closure.
        unsafe { hew_cabi::vec::hew_vec_free(result_vec) };
        Some(result_bytes)
    }

    fn close(&mut self) {
        self.upstream.close();
    }

    fn is_closed(&self) -> bool {
        self.upstream.is_closed()
    }
}

// ── Filter adapter (bytes) ────────────────────────────────────────────────────

/// Calling convention for bytes filter closures.
///
/// The closure receives a `*mut HewVec` (bytes) and returns non-zero to keep
/// the item.  The closure does NOT take ownership — the caller frees the vec
/// after the predicate returns.
type BytesFilterFn = unsafe extern "C" fn(*const c_void, *mut HewVec) -> i32;

define_filter_stream!(FilterBytesStream, BytesFilterFn);

impl StreamBacking for FilterBytesStream {
    fn next(&mut self) -> Option<Item> {
        loop {
            if self.done {
                return None;
            }
            let item = self.upstream.next()?;
            // SAFETY: u8_to_hwvec allocates a fresh HewVec.
            let tmp_vec = unsafe { hew_cabi::vec::u8_to_hwvec(&item) };
            // SAFETY: fn_ptr is a valid Hew closure, env_ptr is its environment.
            let keep = unsafe { (self.fn_ptr)(self.env_ptr, tmp_vec) };
            // SAFETY: tmp_vec was allocated by u8_to_hwvec and the predicate
            // only reads it (returns i32 bool, not the vec).
            unsafe { hew_cabi::vec::hew_vec_free(tmp_vec) };
            if keep != 0 {
                return Some(item);
            }
        }
    }

    fn close(&mut self) {
        self.done = true;
        self.upstream.close();
    }

    fn is_closed(&self) -> bool {
        self.done || self.upstream.is_closed()
    }
}

// ── Take adapter ──────────────────────────────────────────────────────────────

/// Wraps a stream and yields at most `limit` items.
#[derive(Debug)]
struct TakeStream {
    upstream: Box<dyn StreamBacking>,
    remaining: usize,
}

impl StreamBacking for TakeStream {
    fn next(&mut self) -> Option<Item> {
        if self.remaining == 0 {
            return None;
        }
        let item = self.upstream.next()?;
        self.remaining -= 1;
        Some(item)
    }

    fn close(&mut self) {
        self.remaining = 0;
        self.upstream.close();
    }

    fn is_closed(&self) -> bool {
        self.remaining == 0 || self.upstream.is_closed()
    }
}

/// Decrement the RC reference count of a closure environment pointer.
///
/// A null pointer is a no-op (matches `hew_rc_drop`'s null-safe contract).
///
/// # Safety
///
/// `env_ptr` must be null or a valid Hew RC block pointer.
unsafe fn rc_drop_env(env_ptr: *const c_void) {
    extern "C" {
        fn hew_rc_drop(ptr: *mut u8);
    }
    // SAFETY: hew_rc_drop handles null and expects a valid RC block pointer.
    unsafe { hew_rc_drop(env_ptr.cast_mut().cast::<u8>()) };
}

// ── C ABI ─────────────────────────────────────────────────────────────────────

/// Create a bounded in-memory channel.
///
/// Returns a `*mut HewStreamPair` holding linked sink and stream handles.
/// Call `hew_stream_pair_sink` / `hew_stream_pair_stream` to extract them,
/// then `hew_stream_pair_free` to release the pair struct.
///
/// # Safety
///
/// The returned pointer must be freed with `hew_stream_pair_free` after both
/// handles have been extracted.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_channel(capacity: i64) -> *mut HewStreamPair {
    use crate::channel_core::ChannelCore;
    let cap = usize::try_from(capacity.max(1)).unwrap_or(1);
    let core = Arc::new(ChannelCore::new(cap));
    // Borrow the allocation address before the Arc clones move; the address is
    // stable and stays valid while either handle (each holding an Arc clone)
    // is alive.
    let core_raw = Arc::as_ptr(&core).cast::<c_void>();

    // Read half: a channel-backed stream that also carries an Arc clone so the
    // suspending `await stream.recv()` path can reach the shared queue.
    let stream_ptr = into_stream_ptr(ChannelStream {
        core: Arc::clone(&core),
    });
    // SAFETY: stream_ptr was just allocated by into_stream_ptr.
    unsafe {
        (*stream_ptr).channel = Some(Arc::clone(&core));
    }

    // Write half: a callback sink owning the last Arc clone; the opaque core
    // borrow lets `hew_stream_await_send` reach the queue + parked consumer.
    let sink_ptr = into_sink_ptr(
        core,
        channel_sink_write,
        channel_sink_flush,
        channel_sink_close,
    );
    // SAFETY: sink_ptr was just allocated by into_sink_ptr.
    unsafe {
        (*sink_ptr).set_channel_core(core_raw);
    }

    Box::into_raw(Box::new(HewStreamPair {
        // ALLOCATOR-PAIRING: GlobalAlloc
        sink: sink_ptr,
        stream: stream_ptr,
    }))
}

/// Channel-sink callback: blocking write (default callers). Suspending callers
/// route through `hew_stream_await_send` instead.
fn channel_sink_write(core: &mut Arc<crate::channel_core::ChannelCore>, data: &[u8]) {
    core.blocking_send(data.to_vec());
}

/// Channel-sink callback: flush is a no-op (the core is not write-buffered).
fn channel_sink_flush(_core: &mut Arc<crate::channel_core::ChannelCore>) {}

/// Channel-sink callback: producer EOF. Wakes a parked consumer so its
/// `await stream.recv()` resume binds `None`.
fn channel_sink_close(core: &mut Arc<crate::channel_core::ChannelCore>) {
    core.close_sink();
}

/// Bridge a live TCP connection into a `(Stream<bytes>, Sink<bytes>)` pair.
///
/// Clones the underlying socket twice (once for the read backing, once for
/// the write backing) via the existing `tcp_clone_stream` helper so the
/// two halves own independent `TcpStream` descriptors.  After a successful
/// clone, the original handle is removed from the TCP connection table (the
/// same close path as `hew_tcp_close`) so there is no fd leak — the caller
/// has transferred ownership to the returned pair.
///
/// Returns a `*mut HewStreamPair` on success.  The caller must extract the
/// stream and sink with `hew_stream_pair_stream_bytes` /
/// `hew_stream_pair_sink_bytes`, then free with `hew_stream_pair_free`.
///
/// Returns `null` with the last-error set to EBADF (errno 9) if `conn` is
/// not a registered TCP connection handle.
///
/// # Safety
///
/// `conn` must be a valid connection handle returned by `hew_tcp_accept` or
/// `hew_tcp_connect`.  After this call the original `conn` handle is
/// consumed: do not pass it to any other `hew_tcp_*` function.
///
/// # Platform
///
/// Not available on `wasm32` targets; TCP transport is unavailable there.
/// See `WASM-TODO(#1451)`.  The `wasm32` stub returns `null` so the symbol
/// resolves at link time, but every call returns `null` without side-effects.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_stream_from_conn(conn: c_int) -> *mut HewStreamPair {
    use crate::transport::{tcp_clone_stream, tcp_release_conn};

    // Clone the read fd.
    let Some(read_stream) = tcp_clone_stream(conn) else {
        set_last_error_with_errno(
            format!("hew_tcp_stream_from_conn: invalid connection handle {conn}"),
            9, // EBADF
        );
        return ptr::null_mut();
    };

    // Clone the write fd.
    let Some(write_stream) = tcp_clone_stream(conn) else {
        // read_stream RAII drops its clone here.
        set_last_error_with_errno(
            format!("hew_tcp_stream_from_conn: could not clone write fd for handle {conn}"),
            9, // EBADF
        );
        return ptr::null_mut();
    };

    // Release the original handle from the connection table WITHOUT calling
    // shutdown.  TcpStream clones share a single OS file descriptor on Unix;
    // calling shutdown on any clone shuts down the shared socket, which would
    // immediately invalidate the two backings we just created.
    // `tcp_release_conn` only removes the table entry — the two clones keep
    // the socket alive.
    tcp_release_conn(conn);

    // Build the stream (read) half via the canonical helper.
    let stream_ptr = into_stream_ptr(TcpStreamBacking {
        stream: read_stream,
    });

    // Build the sink (write) half using the Write-backed sink constructor,
    // matching the `hew_stream_from_file_write` pattern.
    let sink_ptr = into_write_sink_ptr(write_stream);

    Box::into_raw(Box::new(HewStreamPair {
        // ALLOCATOR-PAIRING: GlobalAlloc
        sink: sink_ptr,
        stream: stream_ptr,
    }))
}

/// `wasm32` stub: TCP transport is unavailable on wasm32 builds.
///
/// Returns `null` unconditionally.  The symbol is present so the wasm32 link
/// succeeds; actual rejection happens at the type-checker level
/// (`hew-types/src/check/methods.rs`: `reject_if_wasm_native_only_handle`)
/// which emits `WasmUnsupportedFeature::TcpNetworking` for any call on
/// `net.Connection`, including `into_stream_sink`.  This stub is never
/// reachable through valid Hew code compiled for wasm32.
///
/// WASM-TODO(#1451): TCP transport gap.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_stream_from_conn(_conn: c_int) -> *mut HewStreamPair {
    ptr::null_mut()
}

/// Extract the `HewSink*` from a pair without consuming the pair.
///
/// # Safety
///
/// `pair` must be a valid pointer returned by `hew_stream_channel` or
/// `hew_stream_from_tcp`. The sink must not be extracted more than once.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_pair_sink(pair: *mut HewStreamPair) -> *mut HewSink {
    cabi_guard!(pair.is_null(), ptr::null_mut());
    // SAFETY: caller guarantees pair is valid.
    // Null-out to transfer ownership (Drop won't double-free).
    unsafe {
        let s = (*pair).sink;
        (*pair).sink = ptr::null_mut();
        s
    }
}

/// Extract the `HewStream*` from a pair without consuming the pair.
///
/// # Safety
///
/// `pair` must be a valid pointer returned by `hew_stream_channel` or
/// `hew_stream_from_tcp`. The stream must not be extracted more than once.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_pair_stream(pair: *mut HewStreamPair) -> *mut HewStream {
    cabi_guard!(pair.is_null(), ptr::null_mut());
    // SAFETY: caller guarantees pair is valid.
    // Null-out to transfer ownership (Drop won't double-free).
    unsafe {
        let s = (*pair).stream;
        (*pair).stream = ptr::null_mut();
        s
    }
}

/// Free the pair struct.  Any handles that were not extracted are also freed.
///
/// # Safety
///
/// `pair` must be a valid pointer returned by `hew_stream_channel`.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_pair_free(pair: *mut HewStreamPair) {
    if !pair.is_null() {
        // SAFETY: pair was allocated with Box::into_raw.
        // Drop impl frees any remaining (non-null) handles.
        unsafe { drop(Box::from_raw(pair)) }; // ALLOCATOR-PAIRING: GlobalAlloc
    }
}

// Bytes-typed aliases for `hew_stream_pair_sink` / `hew_stream_pair_stream`.
// The runtime handles are type-erased, so these are thin wrappers that let
// the Hew type checker distinguish `Sink<String>` from `Sink<bytes>`.

/// Extract the `Sink` from a pair (bytes-typed alias).
///
/// # Safety
///
/// Same preconditions as `hew_stream_pair_sink`.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_pair_sink_bytes(pair: *mut HewStreamPair) -> *mut HewSink {
    // SAFETY: caller guarantees pair is valid; delegates to hew_stream_pair_sink.
    unsafe { hew_stream_pair_sink(pair) }
}

/// Extract the `Stream` from a pair (bytes-typed alias).
///
/// # Safety
///
/// Same preconditions as `hew_stream_pair_stream`.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_pair_stream_bytes(pair: *mut HewStreamPair) -> *mut HewStream {
    // SAFETY: caller guarantees pair is valid; delegates to hew_stream_pair_stream.
    unsafe { hew_stream_pair_stream(pair) }
}

/// Open a file for streaming reads.
///
/// Returns a `*mut HewStream` that yields the file contents in 4096-byte
/// chunks, or null on error.
///
/// # Safety
///
/// `path` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_from_file_read(path: *const c_char) -> *mut HewStream {
    cabi_guard!(path.is_null(), ptr::null_mut());
    // SAFETY: Caller guarantees path is a valid null-terminated C string.
    let s = unsafe { CStr::from_ptr(path) };
    let Ok(path_str) = s.to_str() else {
        return ptr::null_mut();
    };
    match fs::File::open(path_str) {
        Ok(f) => into_stream_ptr(FileReadStream {
            reader: BufReader::new(f),
            chunk_size: 4096,
        }),
        Err(e) => {
            set_last_error_with_errno(format!("{e}"), e.raw_os_error().unwrap_or(0));
            ptr::null_mut()
        }
    }
}

/// Open a file for streaming writes.
///
/// Returns a `*mut HewSink`, or null on error.  On failure, the error
/// message is retrievable via [`hew_stream_last_error`].
///
/// # Safety
///
/// `path` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_from_file_write(path: *const c_char) -> *mut HewSink {
    cabi_guard!(path.is_null(), ptr::null_mut());
    // SAFETY: Caller guarantees path is a valid null-terminated C string.
    let s = unsafe { CStr::from_ptr(path) };
    let Ok(path_str) = s.to_str() else {
        set_last_error("invalid UTF-8 in path".into());
        return ptr::null_mut();
    };
    match fs::File::create(path_str) {
        Ok(f) => into_write_sink_ptr(f),
        Err(e) => {
            set_last_error_with_errno(format!("{e}"), e.raw_os_error().unwrap_or(0));
            ptr::null_mut()
        }
    }
}

/// Create a stream that drains a byte buffer.
///
/// The buffer is split into `item_size`-byte chunks.  If `item_size` is 0
/// the entire buffer is yielded as a single item.  The runtime takes
/// ownership of the byte range `[data, data+len)`.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_from_bytes(
    data: *const u8,
    len: usize,
    item_size: usize,
) -> *mut HewStream {
    if data.is_null() || len == 0 {
        return into_stream_ptr(VecStream {
            items: VecDeque::default(),
        });
    }
    // SAFETY: Caller guarantees data points to len readable bytes.
    let raw: Vec<u8> = unsafe { std::slice::from_raw_parts(data, len).to_vec() };
    let chunk = if item_size == 0 { len } else { item_size };
    let items: VecDeque<Item> = raw.chunks(chunk).map(<[u8]>::to_vec).collect();
    into_stream_ptr(VecStream { items })
}

/// Get the next item from a stream, with its size written to `out_size`.
///
/// Identical to `hew_stream_next` but also writes the byte count to `out_size`.
///
/// # Safety
///
/// `stream` must be a valid stream pointer. `out_size` must be a valid pointer
/// to a `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_next_sized(
    stream: *mut HewStream,
    out_size: *mut usize,
) -> *mut c_void {
    cabi_guard!(stream.is_null(), ptr::null_mut());
    // SAFETY: stream is valid per caller contract.
    let s = unsafe { &mut *stream };
    if let Some(item) = s.inner.next() {
        let len = item.len();
        if !out_size.is_null() {
            // SAFETY: Caller guarantees out_size is valid.
            unsafe { *out_size = len };
        }
        // For empty items, allocate 1 byte so the pointer is non-null.
        let alloc_len = if len == 0 { 1 } else { len };
        // SAFETY: libc::malloc returns a valid aligned pointer or null.
        let buf = unsafe { libc::malloc(alloc_len) }; // ALLOCATOR-PAIRING: libc  // CSTRING-ALLOC: libc-bytes (hew_stream_next_sized item buffer)
        if buf.is_null() {
            return ptr::null_mut();
        }
        if len > 0 {
            // SAFETY: buf is len bytes allocated above; item.as_ptr() points to len bytes.
            unsafe { ptr::copy_nonoverlapping(item.as_ptr(), buf.cast::<u8>(), len) };
        }
        buf.cast::<c_void>()
    } else {
        if !out_size.is_null() {
            // SAFETY: Caller guarantees out_size is valid.
            unsafe { *out_size = 0 };
        }
        ptr::null_mut()
    }
}

/// Read the next item into a caller-provided buffer, avoiding per-item malloc.
///
/// On success the item bytes are copied into `*buf` and the byte count is
/// returned (>= 0).  If the item is larger than `*buf_cap`, the buffer is
/// grown via `libc::realloc`, and both `*buf` and `*buf_cap` are updated so
/// the caller can reuse the (possibly larger) buffer on subsequent calls.
///
/// Returns -1 on EOF (stream exhausted) or if `stream`, `buf`, or `buf_cap`
/// is null.
///
/// # Ownership
///
/// `*buf` must be null or a pointer previously obtained from `malloc` /
/// `realloc`.  The caller must eventually `free(*buf)`.
///
/// # Safety
///
/// `stream` must be a valid stream pointer.  `buf` and `buf_cap` must point
/// to valid, writable memory.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_next_view(
    stream: *mut HewStream,
    buf: *mut *mut u8,
    buf_cap: *mut usize,
) -> i64 {
    if stream.is_null() || buf.is_null() || buf_cap.is_null() {
        return -1;
    }
    // SAFETY: stream is valid per caller contract.
    let s = unsafe { &mut *stream };
    let Some(item) = s.inner.next() else {
        return -1;
    };
    let len = item.len();

    // SAFETY: buf_cap is valid per caller contract.
    let cap = unsafe { *buf_cap };

    // Grow the buffer when the current capacity is insufficient or the
    // buffer pointer is null (callers may pass null with a stale capacity).
    // SAFETY: buf is valid per caller contract; dereferencing to check the inner pointer.
    let needs_alloc = len > cap || (len > 0 && unsafe { (*buf).is_null() });
    if needs_alloc {
        // SAFETY: *buf is null or was obtained from malloc/realloc.
        let new_ptr = unsafe { libc::realloc((*buf).cast::<c_void>(), len) };
        if new_ptr.is_null() {
            return -1;
        }
        // SAFETY: buf and buf_cap are valid per caller contract.
        unsafe {
            *buf = new_ptr.cast::<u8>();
            *buf_cap = len;
        }
    }

    if len > 0 {
        // SAFETY: *buf has at least `len` bytes; item.as_ptr() is valid for `len` bytes.
        unsafe { ptr::copy_nonoverlapping(item.as_ptr(), *buf, len) };
    }

    // Stream items are bounded by available memory; on 64-bit systems the
    // length fits in i64.  On 32-bit systems usize is 32-bit, also lossless.
    #[allow(clippy::cast_possible_wrap, reason = "stream item length ≤ isize::MAX")]
    {
        len as i64
    }
}

/// Non-consuming, cancellable poll for the next item of a stream.
///
/// Registers a pending read on `stream` and returns a [`PendingReadId`] the
/// caller can later pass to [`hew_stream_cancel_pending_read`] to withdraw
/// the registration. When an item arrives (or the stream reaches EOF), the
/// runtime invokes `callback(userdata, item_ptr)` where `item_ptr` is a
/// `libc::malloc`-allocated copy of the item bytes (the callback MUST free
/// it via `libc::free`) or null for EOF. This is the bare-bytes adapter
/// ownership contract — distinct from [`hew_stream_next`], which on the
/// `Stream<string>::recv` surface returns a header-aware `*mut c_char` the
/// MIR drop spine frees via `hew_string_drop`.
///
/// ## Ownership contract
///
/// The caller retains ownership of `stream` across this call: neither a
/// pending registration nor a successful read drops the stream. The
/// stream remains usable after either the callback fires or
/// [`hew_stream_cancel_pending_read`] is called.
///
/// ## "At most one pending read" invariant
///
/// Calling `hew_stream_poll` on a stream that already has a pending read
/// (no intervening cancel or completion) is a runtime contract violation
/// and aborts the process with [`std::process::abort`]. This is reachable
/// only via a codegen bug — well-formed `select{}` lowering registers at
/// most one stream poll per arm per select scope, and the loser-cleanup
/// path runs cancel before the stream binding is reused.
///
/// ## Cancel-race semantics (TOCTOU)
///
/// The state transitions through `ParkState { Pending, Cancelled, Done }`
/// under a shared `Mutex`. If `hew_stream_cancel_pending_read` acquires the
/// lock first, the park thread observes `Cancelled` and discards the item it
/// just received (one item is lost from the stream — this is the documented
/// trade-off for a synchronous mpsc backing without a `try_recv` peek path).
/// If the park thread acquires the lock first, the callback fires and a
/// subsequent cancel is a no-op.
///
/// ## WASM gap
///
/// This function is unimplemented on `wasm32` targets (no
/// `std::thread::spawn`). Tracked as a named v0.5 parity gap; the
/// `cfg`-gate exposes the gap at compile time rather than as a runtime
/// fallback.
///
/// # Safety
///
/// Six-axis invariants for every `unsafe` block inside this function:
///
/// - **Provenance**: `stream` must have been returned by a `hew_stream_*`
///   constructor (`hew_stream_channel`, `hew_stream_from_bytes`, etc.) and
///   must not have been passed to `hew_stream_close`.
/// - **Type-tag**: the pointer is `*mut HewStream`; it is created by
///   `Box::into_raw(Box::new(HewStream { .. }))` and the tag never changes.
/// - **Lifetime-owner**: the stream must remain live until the callback fires
///   **and the park thread returns** (or `hew_stream_cancel_pending_read` is
///   called AND the park thread has observed the cancellation). The caller is
///   responsible for sequencing the stream's deallocation after either event.
/// - **Aliasing-concurrency**: while a pending read is registered, no other
///   thread may access `inner`, `pending_read`, or `pending_state` via
///   `hew_stream_next` / `hew_stream_poll` / `hew_stream_close`. The
///   `select{}` codegen contract enforces this; the "at most one pending read"
///   CAS is the runtime enforcement point.
/// - **Bounds**: `stream` is not indexable; every field access goes through
///   `(*stream).field` where `HewStream`'s layout is a stable `repr(Rust)`
///   struct. The park thread accesses only `inner` (exclusive by the
///   aliasing invariant above) and the two synchronization fields
///   (`pending_read`, `pending_state`) which are designed for concurrent use.
/// - **Failure mode**: a null `stream` is guarded at function entry and
///   returns the error sentinel `0`. A second poll with a live registration
///   aborts (not panics) because unwinding across `extern "C"` is UB.
///
/// `callback` is invoked at most once from an arbitrary worker thread.
/// It must be re-entrant w.r.t. the rest of the runtime and must not
/// itself call `hew_stream_poll` on the same `stream` (re-arming within
/// the callback re-enters this function — wait for the callback to return).
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_stream_poll(
    stream: *mut HewStream,
    callback: extern "C" fn(*mut c_void, *mut c_void),
    userdata: *mut c_void,
) -> PendingReadId {
    if stream.is_null() {
        return 0;
    }

    let id = NEXT_PENDING_READ_ID.fetch_add(1, Ordering::Relaxed);
    // 1 is the first valid id; in practice u64 will never wrap during a process.

    // SAFETY: Provenance + Aliasing — stream is a valid HewStream (non-null
    // checked above); pending_read is an AtomicU64 designed for concurrent
    // access. No other accessor holds `inner` yet (at-most-one invariant
    // established below on CAS success).
    let prev = unsafe {
        (*stream)
            .pending_read
            .compare_exchange(0, id, Ordering::AcqRel, Ordering::Acquire)
    };
    if let Err(existing) = prev {
        // Contract violation: a pending read is already in flight.
        // Aborting (not panicking) is mandatory across `extern "C"`.
        eprintln!(
            "hew_stream_poll: at most one pending read per stream \
             (existing id={existing}, refused id={id})",
        );
        std::process::abort();
    }

    let park_state = Arc::new(Mutex::new(ParkState::Pending));
    // Publish the Arc into the per-stream slot under its mutex. Holding the
    // lock here is brief and cancel will not see a stale registration
    // because pending_read is already set and cancel synchronises on it.
    // SAFETY: Provenance + Lifetime — stream is non-null and still live;
    // pending_state is a Mutex, safe for concurrent access.
    unsafe {
        let mut slot = (*stream)
            .pending_state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *slot = Some(Arc::clone(&park_state));
    }

    // Cross-thread share: the park thread accesses `stream` (specifically
    // `(*stream).inner`) exclusively for the duration of `next()`. The
    // "at most one pending read" contract above guarantees no other
    // thread touches `inner` while this is in flight.
    let stream_addr = stream as usize;
    let userdata_addr = userdata as usize;
    std::thread::spawn(move || {
        // SAFETY: Provenance — stream_addr was cast from a valid *mut HewStream
        // returned by a hew_stream_* constructor; the cast back is the inverse.
        let stream = stream_addr as *mut HewStream;
        // SAFETY: Aliasing-concurrency — the at-most-one-pending-read CAS
        // above guarantees exclusive access to `inner` for the duration of
        // this `next()` call; no other thread touches `inner` while
        // pending_read != 0. Lifetime: caller guarantees stream outlives
        // the park thread per the function-level Safety contract.
        let item = unsafe { (*stream).inner.next() };

        // Marshal the item into a malloc'd buffer (same ABI as
        // hew_stream_next) before taking the state lock so we minimise
        // the critical section.
        let item_ptr: *mut c_void = match item {
            None => ptr::null_mut(),
            Some(bytes) => {
                let len = bytes.len();
                // SAFETY: Bounds — malloc(len+1) returns a pointer to at
                // least len+1 bytes or null; null is handled below.
                let buf = unsafe { libc::malloc(len + 1) }; // ALLOCATOR-PAIRING: libc  // CSTRING-ALLOC: libc-bytes (hew_stream_poll item buffer)
                if buf.is_null() {
                    ptr::null_mut()
                } else {
                    if len > 0 {
                        // SAFETY: Bounds — buf has len+1 bytes (allocated
                        // above); bytes is a slice of exactly len bytes.
                        // No aliasing: buf is freshly allocated.
                        unsafe {
                            ptr::copy_nonoverlapping(bytes.as_ptr(), buf.cast::<u8>(), len);
                        };
                    }
                    // SAFETY: Bounds — buf has len+1 bytes; offset len is
                    // within that allocation. Type-tag: *mut u8 is valid.
                    unsafe { *buf.cast::<u8>().add(len) = 0 };
                    buf.cast::<c_void>()
                }
            }
        };

        // Acquire the state lock and decide whether to fire the callback.
        let mut state = park_state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match *state {
            ParkState::Cancelled => {
                // Cancel won the race. Discard the item bytes; the
                // cancel path has already cleared pending_read and the
                // pending_state slot.
                if !item_ptr.is_null() {
                    // SAFETY: Provenance + Failure mode — item_ptr was
                    // malloc'd by libc::malloc above; null is excluded
                    // by the if-guard. Ownership transfers to free here.
                    unsafe { libc::free(item_ptr) }; // ALLOCATOR-PAIRING: libc  // CSTRING-FREE: libc-bytes (stream item byte buffer)
                }
            }
            ParkState::Pending => {
                *state = ParkState::Done;
                drop(state);

                // Clear pending_read and the state slot before invoking
                // the callback so the callback may re-arm the same
                // stream (subject to "no recursion within callback"
                // documented above).
                // SAFETY: Lifetime + Aliasing — stream is still live (caller
                // contract; the callback fires before stream deallocation).
                // pending_read and pending_state are the two sync fields;
                // the park thread holds exclusive logical access to them
                // between CAS-set (in the caller) and these Release stores.
                unsafe {
                    (*stream).pending_read.store(0, Ordering::Release);
                    let mut slot = (*stream)
                        .pending_state
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner);
                    *slot = None;
                }

                callback(userdata_addr as *mut c_void, item_ptr);
            }
            ParkState::Done => {
                // Park thread observing Done shouldn't happen — only the
                // park thread itself transitions Pending → Done. Treat
                // as a no-op (defensive).
                if !item_ptr.is_null() {
                    // SAFETY: same as Cancelled branch — malloc'd above,
                    // non-null guarded, ownership transferred to free.
                    unsafe { libc::free(item_ptr) }; // ALLOCATOR-PAIRING: libc  // CSTRING-FREE: libc-bytes (stream item byte buffer)
                }
            }
        }

        // Signal park-thread exit so test waiters can unblock deterministically
        // rather than sleeping (§5.7). Only compiled in test builds.
        #[cfg(test)]
        {
            // SAFETY: stream_addr was cast from a valid *mut HewStream;
            // caller guarantees stream outlives the park thread.
            let (lock, cvar) = &*unsafe { (*stream).park_exit_gen.clone() };
            *lock
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner) += 1;
            cvar.notify_all();
        }
    });

    id
}

/// WASM stub for [`hew_stream_poll`] — the `select{}` stream-next arm has
/// no WASM substrate today. Returns the error sentinel 0 unconditionally.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_stream_poll(
    _stream: *mut HewStream,
    _callback: extern "C" fn(*mut c_void, *mut c_void),
    _userdata: *mut c_void,
) -> PendingReadId {
    // WASM-GAP: hew_stream_poll requires std::thread::spawn; tracked for v0.5.
    0
}

/// Withdraw a pending read previously registered by [`hew_stream_poll`].
///
/// If `id` matches the current pending read, transitions the park
/// thread's state to `Cancelled` and clears the per-stream registration.
/// If `id` does not match (either no pending read or a different one),
/// the call is a silent no-op — this is the documented "cancel after
/// completion is a no-op" contract.
///
/// ## Ownership contract
///
/// Does not drop the stream. The stream remains usable after the call.
///
/// ## Item-loss caveat
///
/// If the park thread has already returned from `next()` but has not
/// yet acquired the state lock to fire the callback, cancel will win
/// the race and one item is lost (see [`hew_stream_poll`] for details).
///
/// # Safety
///
/// Six-axis invariants for every `unsafe` block inside this function:
///
/// - **Provenance**: `stream` must have been returned by a `hew_stream_*`
///   constructor and must not have been passed to `hew_stream_close`.
/// - **Type-tag**: the pointer is `*mut HewStream`; cast is the inverse of
///   the `Box::into_raw` construction used by every constructor.
/// - **Lifetime-owner**: stream must remain live until this function returns.
///   The `select{}` codegen guarantees the stream binding outlives the cancel
///   call in every loser-cleanup path.
/// - **Aliasing-concurrency**: `pending_read` and `pending_state` are the two
///   sync fields; concurrent access from the park thread is expected and
///   handled by the `AtomicU64` CAS and the Mutex respectively.
/// - **Bounds**: field accesses are to fixed-offset struct fields inside the
///   allocation pointed to by `stream`.
/// - **Failure mode**: null `stream` or zero `id` returns immediately without
///   dereferencing; CAS failure is a silent no-op (documented contract).
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_stream_cancel_pending_read(stream: *mut HewStream, id: PendingReadId) {
    if stream.is_null() || id == 0 {
        return;
    }

    // SAFETY: Provenance + Aliasing — stream is non-null (checked above),
    // valid HewStream; pending_read is an AtomicU64 safe for concurrent CAS.
    // Failure mode: CAS failure means id is stale — documented no-op.
    let cas = unsafe {
        (*stream)
            .pending_read
            .compare_exchange(id, 0, Ordering::AcqRel, Ordering::Acquire)
    };
    if cas.is_err() {
        return;
    }

    // Take the shared state out from under the slot and transition
    // Pending → Cancelled if the park thread has not yet fired.
    // SAFETY: Provenance + Aliasing — stream still valid; pending_state is
    // a Mutex designed for concurrent access with the park thread.
    let arc = unsafe {
        let mut slot = (*stream)
            .pending_state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        slot.take()
    };
    if let Some(arc) = arc {
        let mut state = arc
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if *state == ParkState::Pending {
            *state = ParkState::Cancelled;
        }
        // If state is Done, the park thread has already published the
        // callback fire; we lost a race and there is nothing to do.
    }
}

/// WASM stub for [`hew_stream_cancel_pending_read`]. No-op because the
/// poll variant is also a no-op on WASM.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_stream_cancel_pending_read(
    _stream: *mut HewStream,
    _id: PendingReadId,
) {
    // WASM-GAP: paired with hew_stream_poll; tracked for v0.5.
}

/// Close (discard) a stream.
///
/// # Safety
///
/// `stream` must be a valid pointer created by one of the `hew_stream_*`
/// constructor functions, and must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_close(stream: *mut HewStream) {
    if !stream.is_null() {
        crate::tracing::record_channel_event(stream as u64, crate::tracing::SPAN_STREAM_CLOSED);
        // SAFETY: stream was allocated with Box::into_raw.
        // Drop impl calls close() on the backing.
        unsafe { drop(Box::from_raw(stream)) }; // ALLOCATOR-PAIRING: GlobalAlloc
    }
}

/// Write one item to a sink.
///
/// Blocks with backpressure if the backing buffer is full.
///
/// # Safety
///
/// `sink` must be a valid pointer. `data` must point to at least `size` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_write(sink: *mut HewSink, data: *const c_void, size: usize) {
    if sink.is_null() || data.is_null() || size == 0 {
        return;
    }
    // SAFETY: Caller guarantees data points to size readable bytes.
    let bytes = unsafe { std::slice::from_raw_parts(data.cast::<u8>(), size) };
    // SAFETY: sink is valid per caller contract.
    unsafe { (*sink).write_item(bytes) };
}

/// Flush buffered writes in a sink (no-op for in-memory sinks).
///
/// # Safety
///
/// `sink` must be a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_flush(sink: *mut HewSink) {
    if !sink.is_null() {
        // SAFETY: sink is valid per caller contract.
        unsafe { (*sink).flush() };
    }
}

/// Close and free a sink.
///
/// # Safety
///
/// `sink` must be a valid pointer created by one of the `hew_stream_*`
/// constructor functions, and must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_close(sink: *mut HewSink) {
    if !sink.is_null() {
        crate::tracing::record_channel_event(sink as u64, crate::tracing::SPAN_SINK_CLOSED);
        // SAFETY: sink was allocated with Box::into_raw.
        // Drop impl calls close() on the backing.
        unsafe { drop(Box::from_raw(sink)) }; // ALLOCATOR-PAIRING: GlobalAlloc
    }
}

/// Pipe all items from a stream into a sink, then close both.
///
/// Reads items from `stream` until EOF and writes each to `sink`.
/// Both handles are consumed — do not use them after this call.
///
/// # Safety
///
/// Both `stream` and `sink` must be valid pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_pipe(stream: *mut HewStream, sink: *mut HewSink) {
    cabi_guard!(stream.is_null() || sink.is_null());
    // SAFETY: Both pointers are valid per caller contract.
    let s = unsafe { &mut *stream };
    // SAFETY: sink is non-null (checked above) and valid per caller contract.
    let k = unsafe { &mut *sink };

    while let Some(item) = s.inner.next() {
        k.write_item(&item);
    }
    k.close();

    // Free both handles.
    // SAFETY: Both were allocated with Box::into_raw.
    unsafe {
        drop(Box::from_raw(stream)); // ALLOCATOR-PAIRING: GlobalAlloc
        drop(Box::from_raw(sink)); // ALLOCATOR-PAIRING: GlobalAlloc
    }
}

/// Wrap a `Stream<bytes>` with a lines adapter.
///
/// Returns a new `HewStream*` that yields one newline-terminated line at a
/// time (as a UTF-8 byte sequence, newline included).  Takes ownership of
/// `stream` — do not use it after this call.
///
/// # Safety
///
/// `stream` must be a valid stream pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_lines(stream: *mut HewStream) -> *mut HewStream {
    cabi_guard!(stream.is_null(), ptr::null_mut());
    // SAFETY: stream is a valid HewStream pointer from the Hew runtime ABI.
    let upstream = unsafe { consume_stream_inner(stream) };
    into_stream_ptr(LinesStream {
        buf: Vec::new(),
        upstream,
        done: false,
        skip_next_delimiter: false,
    })
}

/// Wrap a `Stream<bytes>` with a fixed-size chunks adapter.
///
/// Returns a new `HewStream*` that yields exactly `chunk_size`-byte items
/// (except possibly the last one, which may be shorter).  Takes ownership
/// of `stream`.
///
/// # Safety
///
/// `stream` must be a valid stream pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_chunks(
    stream: *mut HewStream,
    chunk_size: i64,
) -> *mut HewStream {
    cabi_guard!(stream.is_null(), ptr::null_mut());
    let size = usize::try_from(chunk_size.max(1)).unwrap_or(1);
    // SAFETY: stream is a valid HewStream pointer from the Hew runtime ABI.
    let upstream = unsafe { consume_stream_inner(stream) };
    into_stream_ptr(ChunksStream {
        buf: Vec::new(),
        chunk_size: size,
        upstream,
        done: false,
    })
}

// ── Convenience functions ─────────────────────────────────────────────────────

/// Read all remaining items from a stream and concatenate them as a C string.
///
/// Returns a header-aware null-terminated string. The caller must free it with
/// `hew_string_drop`.
/// Consumes the stream.
///
/// # Safety
///
/// `stream` must be a valid `HewStream` pointer or null.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_collect_string(stream: *mut HewStream) -> *mut c_char {
    cabi_guard!(stream.is_null(), ptr::null_mut());

    // SAFETY: stream was allocated with Box::into_raw; we take ownership.
    let mut owned = unsafe { Box::from_raw(stream) }; // ALLOCATOR-PAIRING: GlobalAlloc
    let mut buffer = Vec::new();

    while let Some(chunk) = owned.inner.next() {
        buffer.extend_from_slice(&chunk);
    }

    // Ensure null termination
    buffer.push(0);

    let len = buffer.len();
    // Header-aware (S1): buffer already includes the trailing NUL, so request
    // exactly `len` data bytes. Released via hew_string_drop / free_cstring.
    let ptr = crate::cabi::alloc_cstring_data(len); // CSTRING-ALLOC: str-open (hew_stream_collect_string — header-aware String result; reaches hew_string_drop)
    if ptr.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: ptr is len bytes allocated above; buffer.as_ptr() points to len bytes.
    unsafe { ptr::copy_nonoverlapping(buffer.as_ptr(), ptr.cast::<u8>(), len) };
    ptr.cast::<c_char>()
}

/// Count remaining items in a stream.
///
/// Consumes the stream.
///
/// # Safety
///
/// `stream` must be a valid `HewStream` pointer or null.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_count(stream: *mut HewStream) -> i64 {
    cabi_guard!(stream.is_null(), 0);

    // SAFETY: stream was allocated with Box::into_raw; we take ownership.
    let mut owned = unsafe { Box::from_raw(stream) }; // ALLOCATOR-PAIRING: GlobalAlloc
    let mut count = 0;

    while owned.inner.next().is_some() {
        count += 1;
    }
    count
}

/// Write a null-terminated C string to the sink.
///
/// # Safety
///
/// `sink` must be a valid pointer. `data` must be a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_write_string(sink: *mut HewSink, data: *const c_char) {
    cabi_guard!(sink.is_null() || data.is_null());

    // SAFETY: data is a valid C string.
    let s = unsafe { CStr::from_ptr(data) };
    let bytes = s.to_bytes();

    // SAFETY: sink is valid per caller contract.
    unsafe { (*sink).write_item(bytes) };
}

/// Check if a stream has been closed/exhausted.
///
/// Returns 1 if stream is closed/exhausted, 0 otherwise.
/// Non-consuming peek-like check.
///
/// # Safety
///
/// `stream` must be a valid stream pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_is_closed(stream: *mut HewStream) -> i32 {
    cabi_guard!(stream.is_null(), 1);

    // SAFETY: stream is valid per caller contract.
    let s = unsafe { &*stream };
    i32::from(s.inner.is_closed())
}

/// Common entry-point logic for stream transform C ABI functions.
///
/// Validates inputs, consumes the upstream stream, and invokes `build`
/// to construct the platform-specific backing.
///
/// # Safety
///
/// - `stream` must be a valid `HewStream` pointer.
/// - `fn_ptr` must be non-null.
/// - `build` must produce a valid `StreamBacking` from the given upstream and pointers.
unsafe fn stream_transform_entry(
    stream: *mut HewStream,
    fn_ptr: *const c_void,
    env_ptr: *const c_void,
    build: impl FnOnce(Box<dyn StreamBacking>, *const c_void, *const c_void) -> Box<dyn StreamBacking>,
) -> *mut HewStream {
    cabi_guard!(stream.is_null() || fn_ptr.is_null(), ptr::null_mut());
    // SAFETY: stream is a valid HewStream pointer from the Hew runtime ABI.
    let upstream = unsafe { consume_stream_inner(stream) };
    into_stream_ptr_dyn(build(upstream, fn_ptr, env_ptr))
}

/// Wrap a stream with a lazy map adapter.
///
/// Every item yielded by `stream` is transformed by calling `fn_ptr(env_ptr, item)`.
/// The adapter takes ownership of `stream` and of one RC reference to `env_ptr`
/// (the caller must have RC-cloned before passing here).
///
/// Returns a new `HewStream*`. Takes ownership of `stream`.
///
/// # Safety
///
/// - `stream` must be a valid `HewStream` pointer.
/// - `fn_ptr` must be a valid Hew closure function pointer matching the
///   `(env: *const c_void, s: *const c_char) -> *mut c_char` ABI.
/// - `env_ptr` must be null or a valid Hew RC block already retained for this call.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_map_string(
    stream: *mut HewStream,
    fn_ptr: *const c_void,
    env_ptr: *const c_void,
) -> *mut HewStream {
    // SAFETY: caller satisfies all pointer contracts.
    unsafe {
        stream_transform_entry(stream, fn_ptr, env_ptr, |upstream, fp, ep| {
            let fn_typed: StringMapFn = std::mem::transmute(fp);
            Box::new(MapStringStream {
                upstream,
                fn_ptr: fn_typed,
                env_ptr: ep,
            })
        })
    }
}

/// Wrap a stream with a lazy filter adapter.
///
/// Items for which `fn_ptr(env_ptr, item)` returns zero are skipped.
/// The adapter takes ownership of `stream` and of one RC reference to `env_ptr`.
///
/// Returns a new `HewStream*`. Takes ownership of `stream`.
///
/// # Safety
///
/// - `stream` must be a valid `HewStream` pointer.
/// - `fn_ptr` must match the `(env: *const c_void, s: *const c_char) -> i32` ABI.
/// - `env_ptr` must be null or a valid Hew RC block already retained for this call.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_filter_string(
    stream: *mut HewStream,
    fn_ptr: *const c_void,
    env_ptr: *const c_void,
) -> *mut HewStream {
    // SAFETY: caller satisfies all pointer contracts.
    unsafe {
        stream_transform_entry(stream, fn_ptr, env_ptr, |upstream, fp, ep| {
            let fn_typed: StringFilterFn = std::mem::transmute(fp);
            Box::new(FilterStringStream {
                upstream,
                fn_ptr: fn_typed,
                env_ptr: ep,
                done: false,
            })
        })
    }
}

/// Wrap a bytes stream with a lazy map adapter.
///
/// The closure receives a `*mut HewVec` (bytes) and returns a new `*mut HewVec`.
/// The adapter takes ownership of `stream` and of one RC reference to `env_ptr`.
///
/// Returns a new `HewStream*`. Takes ownership of `stream`.
///
/// # Safety
///
/// - `stream` must be a valid `HewStream` pointer.
/// - `fn_ptr` must be a valid Hew closure matching the
///   `(env: *const c_void, data: *mut HewVec) -> *mut HewVec` ABI.
/// - `env_ptr` must be null or a valid Hew RC block already retained for this call.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_map_bytes(
    stream: *mut HewStream,
    fn_ptr: *const c_void,
    env_ptr: *const c_void,
) -> *mut HewStream {
    // SAFETY: caller satisfies all pointer contracts.
    unsafe {
        stream_transform_entry(stream, fn_ptr, env_ptr, |upstream, fp, ep| {
            let fn_typed: BytesMapFn = std::mem::transmute(fp);
            Box::new(MapBytesStream {
                upstream,
                fn_ptr: fn_typed,
                env_ptr: ep,
            })
        })
    }
}

/// Wrap a bytes stream with a lazy filter adapter.
///
/// Items for which `fn_ptr(env_ptr, item_vec)` returns zero are skipped.
/// The adapter takes ownership of `stream` and of one RC reference to `env_ptr`.
///
/// Returns a new `HewStream*`. Takes ownership of `stream`.
///
/// # Safety
///
/// - `stream` must be a valid `HewStream` pointer.
/// - `fn_ptr` must match the `(env: *const c_void, data: *mut HewVec) -> i32` ABI.
/// - `env_ptr` must be null or a valid Hew RC block already retained for this call.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_filter_bytes(
    stream: *mut HewStream,
    fn_ptr: *const c_void,
    env_ptr: *const c_void,
) -> *mut HewStream {
    // SAFETY: caller satisfies all pointer contracts.
    unsafe {
        stream_transform_entry(stream, fn_ptr, env_ptr, |upstream, fp, ep| {
            let fn_typed: BytesFilterFn = std::mem::transmute(fp);
            Box::new(FilterBytesStream {
                upstream,
                fn_ptr: fn_typed,
                env_ptr: ep,
                done: false,
            })
        })
    }
}

/// Wrap a stream with a take adapter that yields at most `n` items.
///
/// Returns a new `HewStream*`. Takes ownership of `stream`.
///
/// # Safety
///
/// `stream` must be a valid `HewStream` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_take(stream: *mut HewStream, n: i64) -> *mut HewStream {
    cabi_guard!(stream.is_null(), ptr::null_mut());
    let limit = usize::try_from(n.max(0)).unwrap_or(0);
    // SAFETY: stream is a valid HewStream pointer from the Hew runtime ABI.
    let upstream = unsafe { consume_stream_inner(stream) };
    into_stream_ptr(TakeStream {
        upstream,
        remaining: limit,
    })
}

// ── Bytes bridge: HewVec ↔ raw-byte marshalling ──────────────────────────────
//
// These functions bridge the type-erased stream runtime (raw byte buffers) to
// the `bytes` (`HewVec<i32>`) representation used by the Hew language.
// The enricher dispatches here when the stream element type is `bytes`.

/// Write a `bytes` value to a sink.
///
/// Takes a POINTER to the caller's [`crate::bytes::BytesTriple`] and writes the
/// active region `data.ptr[data.offset .. data.offset + data.len]` as a single
/// stream item. Zero-length writes are forwarded — they are valid data, not
/// no-ops. Does nothing only if `sink` or `data` is null. The buffer is
/// BORROWED — ownership stays with the caller, whose drop spine releases it via
/// `hew_bytes_drop`.
///
/// By-pointer (not by-value): a 16-byte triple passed by value as a non-first
/// argument loses its offset/len eightbyte at the current codegen C-ABI
/// boundary; passing the address is ABI-portable (mirrors `hew_bytes_push`).
/// Codegen passes the triple alloca's address for the `data: bytes` parameter
/// (`is_bytes_by_pointer_consumer`).
///
/// # Safety
///
/// `sink` must be a valid sink pointer. `data` must point to a valid
/// `BytesTriple` (either its `ptr` null with `len == 0`, or `ptr` pointing to a
/// `hew_bytes_*` allocation whose active region `[offset, offset + len)` is in
/// bounds).
#[no_mangle]
pub unsafe extern "C" fn hew_sink_write_bytes(
    sink: *mut HewSink,
    data: *const crate::bytes::BytesTriple,
) {
    if sink.is_null() || data.is_null() {
        return;
    }
    // SAFETY: `data` points to the caller's valid BytesTriple slot.
    let data = unsafe { &*data };
    if data.len == 0 || data.ptr.is_null() {
        // hew_sink_write short-circuits on size=0, but an empty item is valid
        // data that must be delivered.  Write directly to the backing.
        // SAFETY: sink is valid per caller contract.
        unsafe { (*sink).write_item(&[]) };
        return;
    }
    // SAFETY: `data.ptr + data.offset` is valid for `data.len` bytes per the
    // BytesTriple contract; read-only borrow (no mutation, no free).
    let bytes = unsafe {
        std::slice::from_raw_parts(data.ptr.add(data.offset as usize), data.len as usize)
    };
    // SAFETY: sink is valid; bytes slice is valid for its length.
    unsafe {
        hew_sink_write(sink, bytes.as_ptr().cast::<c_void>(), bytes.len());
    }
}

// ── Suspending stream consumer / producer (NEW-7) ─────────────────────────────
//
// These entries flip `await stream.recv()` / `await sink.send(x)` from a
// worker-blocking call onto the read-slot / `enqueue_resume` substrate when the
// caller carries an execution context (actor handler / closure / task entry).
// The codegen suspend ramp calls `*_await_*` to register, suspends, and on the
// resume edge binds the result (`hew_stream_pop_bytes` for the consumer; unit
// for the producer). Non-channel backings (file/TCP/adapters) keep the blocking
// path: the await entry returns `STREAM_AWAIT_READY` and the bind reads through
// the existing blocking FFI. See `crate::channel_core` for the wake discipline.
// the existing blocking FFI. See `crate::channel_core` for the wake discipline.

/// Register a suspending consumer for `await stream.recv()`.
///
/// Returns [`crate::channel_core::STREAM_AWAIT_READY`] when the bind can proceed
/// immediately (an item is queued, the producer closed, or this is a
/// non-channel backing), or [`crate::channel_core::STREAM_AWAIT_SUSPEND`] after
/// parking the consumer's continuation on `slot`.
///
/// # Safety
///
/// `stream` is a live stream handle; `actor` is the awaiting actor
/// (`hew_actor_self`); `slot` is a live read slot the caller created.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_await_next(
    stream: *mut HewStream,
    actor: *mut crate::actor::HewActor,
    slot: *mut crate::read_slot::HewReadSlot,
) -> i32 {
    if stream.is_null() {
        return crate::channel_core::STREAM_AWAIT_READY;
    }
    // SAFETY: stream is a valid HewStream per caller contract.
    let channel = unsafe { (*stream).channel.as_ref() };
    match channel {
        // SAFETY: the core is alive (the stream holds an Arc clone); `actor` /
        // `slot` validity is the caller's contract.
        Some(core) => unsafe { core.await_next(actor, slot) },
        None => crate::channel_core::STREAM_AWAIT_READY,
    }
}

// ── Layout-witness element path (generic Stream<T> width) ────────────────────
//
// The `*_layout` entries carry ANY element type the compiler can describe
// through one mechanism: a `HewVecElemLayout` witness selects the envelope
// encoding and ownership discipline (see `crate::channel_common`). String and
// bytes elements stay content-encoded, so the witness path composes with
// platform backings (TCP, file, lines/chunks adapters) that produce raw byte
// envelopes natively. Layout-managed (heap-owning) elements exist only on
// in-memory channel pipes — the typed-serialise send below fails closed on any
// other sink kind.

/// Send one element of any witness-describable type into a sink.
/// Blocks with backpressure if the backing buffer is full.
///
/// # Safety
///
/// `sink` must be a valid pointer. `data` must point to one live element of
/// the witness's type (see [`crate::channel_common::encode_elem_envelope`]).
/// `layout` must point to a valid `HewVecElemLayout` for the duration of the
/// call (in practice a codegen static).
#[no_mangle]
pub unsafe extern "C" fn hew_stream_send_layout(
    sink: *mut HewSink,
    data: *const c_void,
    layout: *const crate::vec::HewVecElemLayout,
) {
    cabi_guard!(sink.is_null() || data.is_null());
    // SAFETY: layout validity is the caller's contract; the helper aborts
    // fail-closed on a malformed witness.
    let layout =
        unsafe { crate::channel_common::elem_layout_witness(layout, "hew_stream_send_layout") };
    if layout.ownership_kind == crate::vec::HewTypeOwnershipKind::LayoutManaged {
        // Owned elements ride only the in-memory pipe: the queue must be able
        // to release unconsumed envelopes via the stamped witness. A byte
        // sink (file, TCP) has no element ownership concept — fail closed.
        // SAFETY: sink is valid per caller contract.
        let core_raw = unsafe { (*sink).channel_core_ptr() };
        if core_raw.is_null() {
            crate::channel_common::abort_elem_witness(
                "hew_stream_send_layout",
                "layout-managed elements require an in-memory channel sink",
            );
        }
        // SAFETY: core_raw borrows the live `Arc<ChannelCore>` owned by the
        // sink backing (alive for the duration of this call).
        let core = unsafe { &*core_raw.cast::<crate::channel_core::ChannelCore>() };
        core.stamp_elem_layout(layout);
        // SAFETY: data points to one live element per caller contract.
        let env = unsafe {
            crate::channel_common::encode_elem_envelope(data, layout, "hew_stream_send_layout")
        };
        core.blocking_send(env);
        return;
    }
    // Plain / String / Bytes envelopes own no heap; any sink kind accepts them.
    // SAFETY: data points to one live element per caller contract.
    let env = unsafe {
        crate::channel_common::encode_elem_envelope(data, layout, "hew_stream_send_layout")
    };
    // SAFETY: sink is valid per caller contract. write_item delivers empty
    // envelopes too (an empty string element is a valid item).
    unsafe { (*sink).write_item(&env) };
}

/// Block until an element is available and decode it into `out`.
///
/// Returns 1 when an element was written to `out` (ownership transfers to the
/// caller), or 0 at EOF, letting codegen wrap the result as `Option<T>`.
///
/// # Safety
///
/// `stream` must be a valid stream handle. `out` must point to one writable
/// element slot of the witness's type. `layout` must be a valid witness.
/// No other thread may concurrently read from `stream` during this call.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_next_layout(
    stream: *mut HewStream,
    out: *mut c_void,
    layout: *const crate::vec::HewVecElemLayout,
) -> i32 {
    cabi_guard!(stream.is_null() || out.is_null(), 0);
    // SAFETY: layout validity is the caller's contract.
    let layout =
        unsafe { crate::channel_common::elem_layout_witness(layout, "hew_stream_next_layout") };
    // SAFETY: stream is valid and exclusively borrowed per caller contract.
    let item = unsafe { (*stream).inner.next() };
    // SAFETY: out points to one writable element slot per caller contract.
    unsafe {
        crate::channel_common::decode_elem_envelope(item, out, layout, "hew_stream_next_layout")
    }
}

/// Pop one element on the consumer resume / immediate bind edge. Channel
/// streams pop from the shared queue (draining a parked producer); non-channel
/// backings fall back to the blocking read (status quo for platform streams).
///
/// Returns 1 when an element was written to `out`, or 0 when no item is
/// available (EOF, or a spurious wake on an empty-and-open pipe).
///
/// # Safety
///
/// Same contract as [`hew_stream_next_layout`].
#[no_mangle]
pub unsafe extern "C" fn hew_stream_pop_layout(
    stream: *mut HewStream,
    out: *mut c_void,
    layout: *const crate::vec::HewVecElemLayout,
) -> i32 {
    cabi_guard!(stream.is_null() || out.is_null(), 0);
    // SAFETY: layout validity is the caller's contract.
    let layout =
        unsafe { crate::channel_common::elem_layout_witness(layout, "hew_stream_pop_layout") };
    // SAFETY: stream is valid per caller contract.
    let channel = unsafe { (*stream).channel.as_ref() };
    let item = match channel {
        Some(core) => core.pop(),
        // SAFETY: stream is valid; the blocking read is the status-quo path
        // for non-channel backings (file / TCP / adapters).
        None => unsafe { (*stream).inner.next() },
    };
    // SAFETY: out points to one writable element slot per caller contract.
    unsafe {
        crate::channel_common::decode_elem_envelope(item, out, layout, "hew_stream_pop_layout")
    }
}

/// Try to receive an element without blocking (channel-backed streams; other
/// backings keep their documented blocking fallback, matching
/// [`hew_stream_try_next`]).
///
/// Returns 1 when an element was written to `out`, or 0 when the stream is
/// empty or at EOF.
///
/// # Safety
///
/// Same contract as [`hew_stream_next_layout`].
#[no_mangle]
pub unsafe extern "C" fn hew_stream_try_next_layout(
    stream: *mut HewStream,
    out: *mut c_void,
    layout: *const crate::vec::HewVecElemLayout,
) -> i32 {
    cabi_guard!(stream.is_null() || out.is_null(), 0);
    // SAFETY: layout validity is the caller's contract.
    let layout =
        unsafe { crate::channel_common::elem_layout_witness(layout, "hew_stream_try_next_layout") };
    // SAFETY: stream is valid and exclusively borrowed per caller contract.
    let item = unsafe { (*stream).inner.try_next() };
    // SAFETY: out points to one writable element slot per caller contract.
    unsafe {
        crate::channel_common::decode_elem_envelope(item, out, layout, "hew_stream_try_next_layout")
    }
}

/// Detach an abandoned suspending consumer (the codegen abandon edge). Releases
/// the channel core's in-flight ref on `slot` if it is still registered.
///
/// # Safety
///
/// `stream` is a valid stream handle; `slot` is the consumer's read slot.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_detach_await(
    stream: *mut HewStream,
    slot: *mut crate::read_slot::HewReadSlot,
) {
    if stream.is_null() {
        return;
    }
    // SAFETY: stream is valid per caller contract.
    if let Some(core) = unsafe { (*stream).channel.as_ref() } {
        // SAFETY: the core is alive (stream holds an Arc clone); slot is the
        // consumer's read slot.
        unsafe { core.detach_consumer(slot) };
    }
}

/// Register a suspending producer for `await sink.send(x)`.
///
/// Returns [`crate::channel_core::STREAM_AWAIT_READY`] when the write completed
/// immediately (the ring had space, the consumer is gone, or this is a
/// non-channel sink), or [`crate::channel_core::STREAM_AWAIT_SUSPEND`] after
/// parking the producer (the ring was full; its item is owned by the runtime
/// across the suspend and enqueued by the consumer's drain).
///
/// # Safety
///
/// `sink` is a live sink handle; `actor` is the sending actor; `slot` is a live
/// read slot; `data` points to the caller's `BytesTriple` (borrowed — the
/// runtime copies it).
#[no_mangle]
pub unsafe extern "C" fn hew_stream_await_send(
    sink: *mut HewSink,
    actor: *mut crate::actor::HewActor,
    slot: *mut crate::read_slot::HewReadSlot,
    data: *const crate::bytes::BytesTriple,
) -> i32 {
    if sink.is_null() {
        return crate::channel_core::STREAM_AWAIT_READY;
    }
    // SAFETY: sink is valid per caller contract.
    let core_raw = unsafe { (*sink).channel_core_ptr() };
    if core_raw.is_null() {
        // Non-channel sink: blocking write (status quo).
        // SAFETY: sink + data validity is the caller's contract.
        unsafe { hew_sink_write_bytes(sink, data) };
        return crate::channel_core::STREAM_AWAIT_READY;
    }
    // Copy the item out of the borrowed triple (the runtime owns it across the
    // suspend / hand-off).
    let item: Vec<u8> = if data.is_null() {
        Vec::new()
    } else {
        // SAFETY: data points to the caller's valid BytesTriple slot.
        let d = unsafe { &*data };
        if d.ptr.is_null() || d.len == 0 {
            Vec::new()
        } else {
            // SAFETY: `d.ptr + d.offset` is valid for `d.len` bytes per the
            // BytesTriple contract; read-only borrow.
            unsafe {
                std::slice::from_raw_parts(d.ptr.add(d.offset as usize), d.len as usize).to_vec()
            }
        }
    };
    // SAFETY: core_raw borrows the live `Arc<ChannelCore>` owned by the sink
    // backing (alive for the duration of this call); actor / slot validity is
    // the caller's contract.
    let core = unsafe { &*core_raw.cast::<crate::channel_core::ChannelCore>() };
    // SAFETY: see above.
    unsafe { core.await_send(actor, slot, item) }
}

/// Detach an abandoned suspending producer (the codegen abandon edge). Releases
/// the channel core's in-flight ref on `slot` and drops the parked item.
///
/// # Safety
///
/// `sink` is a valid sink handle; `slot` is the producer's read slot.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_detach_await(
    sink: *mut HewSink,
    slot: *mut crate::read_slot::HewReadSlot,
) {
    if sink.is_null() {
        return;
    }
    // SAFETY: sink is valid per caller contract.
    let core_raw = unsafe { (*sink).channel_core_ptr() };
    if core_raw.is_null() {
        return;
    }
    // SAFETY: core_raw borrows the sink's live `Arc<ChannelCore>`.
    let core = unsafe { &*core_raw.cast::<crate::channel_core::ChannelCore>() };
    // SAFETY: slot is the producer's read slot.
    unsafe { core.detach_producer(slot) };
}

// ── Non-blocking stream read / sink write ─────────────────────────────────────

/// Non-blocking variant of [`hew_sink_write_string`].
///
/// Writes a null-terminated C string to the sink if the backing buffer has
/// capacity. Returns `0` (`SendError::Ok`) on success or `2`
/// (`SendError::Full`) if the buffer is at capacity and the write would have
/// blocked. For non-channel sinks the backing falls back to a blocking write
/// and always returns `0`.
///
/// Returns `1` (`SendError::Closed`) if `sink` or `data` is null.
///
/// # Safety
///
/// ## Pointer validity
/// `sink` must be a non-null pointer obtained from a `hew_stream_*` or
/// `hew_sink_*` constructor and must not have been freed.
/// `data` must be a non-null pointer to a valid NUL-terminated C string.
///
/// ## Aliasing
/// No other thread may concurrently write to `sink` during this call.
///
/// ## Lifetime
/// `data` must remain valid for the duration of this call; the runtime
/// copies the bytes before returning.
///
/// ## Return value
/// `0` = item accepted; `1` = null argument (closed); `2` = channel full.
///
/// ## Caller responsibility
/// The caller retains ownership of `data`; the runtime copies the bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_try_write_string(sink: *mut HewSink, data: *const c_char) -> i32 {
    // SendError::Closed = 1
    if sink.is_null() || data.is_null() {
        return 1;
    }
    // SAFETY:
    //   Provenance: `data` is a NUL-terminated C string per caller contract; non-null by guard.
    //   Type tag: c_char → CStr → &[u8]; the CStr scan validates NUL-termination.
    //   Lifetime owner: caller retains `data`; we only borrow for this call (no take/free).
    //   Aliasing/concurrency: read-only borrow; safe under caller's "no concurrent modification" contract.
    //   Bounds: CStr::from_ptr walks until NUL; caller-promised termination keeps it bounded.
    //   Failure mode: violation (missing NUL or invalid ptr) is UB — documented at fn level.
    let s = unsafe { CStr::from_ptr(data) };
    let bytes = s.to_bytes();
    // Channel-backed sinks use the core's genuine non-blocking `try_send`.
    // SAFETY: sink is valid per the guard above.
    if let Some(core) = unsafe { sink_channel_core(sink) } {
        return if core.try_send(bytes.to_vec()) { 0 } else { 2 };
    }
    // SAFETY:
    //   Provenance: `sink` came from a `hew_*_sink_*` constructor; non-null by guard above.
    //   Type tag: cast to `*mut HewSink` matches declared type.
    //   Lifetime owner: caller retains; we do not free.
    //   Aliasing/concurrency: caller contract bans concurrent writes; backing's try_write is internally synchronised.
    //   Bounds: not a slice access — method dispatch with the bytes slice we just borrowed.
    //   Failure mode: violations are UAF / data race; documented at fn level. Non-channel sinks fall back to blocking write per `SinkOps::try_write_item` default.
    let accepted = unsafe { (*sink).try_write_item(bytes) };
    if accepted {
        0
    } else {
        2
    } // 0 = Ok, 2 = Full
}

/// If `sink` is a channel-backed (NEW-7) sink, return a borrow of its core.
///
/// # Safety
///
/// `sink` must be a valid `HewSink` pointer; the returned borrow is valid for
/// as long as the sink is alive (it owns an `Arc<ChannelCore>` clone).
unsafe fn sink_channel_core<'a>(
    sink: *mut HewSink,
) -> Option<&'a crate::channel_core::ChannelCore> {
    if sink.is_null() {
        return None;
    }
    // SAFETY: sink is valid per caller contract.
    let core_raw = unsafe { (*sink).channel_core_ptr() };
    if core_raw.is_null() {
        None
    } else {
        // SAFETY: core_raw borrows the sink's live Arc<ChannelCore>.
        Some(unsafe { &*core_raw.cast::<crate::channel_core::ChannelCore>() })
    }
}

/// Non-blocking variant of [`hew_sink_write_bytes`].
///
/// Writes a `bytes` value to the sink if the backing buffer has capacity.
/// Returns `0` (`SendError::Ok`) on success or `2` (`SendError::Full`) if
/// the buffer is at capacity. For non-channel sinks the backing falls back
/// to a blocking write and always returns `0`.
///
/// Returns `1` (`SendError::Closed`) if `sink` or `data` is null.
///
/// Takes a POINTER to the caller's [`crate::bytes::BytesTriple`]; the active
/// region is BORROWED for the duration of the call and the caller retains
/// ownership (the Hew drop spine releases it via `hew_bytes_drop`). By-pointer
/// (not by-value) for the same ABI reason as [`hew_sink_write_bytes`]
/// (`is_bytes_by_pointer_consumer`).
///
/// # Safety
///
/// ## Pointer validity
/// `sink` must be a non-null pointer obtained from a `hew_stream_*` or
/// `hew_sink_*` constructor and must not have been freed.
/// `data` must point to a valid `BytesTriple` (its ptr null with len 0, or ptr
/// pointing to a `hew_bytes_*` allocation whose active region is in bounds).
///
/// ## Aliasing
/// No other thread may concurrently write to `sink` during this call.
///
/// ## Lifetime
/// `data`'s buffer must remain valid for the duration of this call; the
/// runtime copies the bytes before returning.
///
/// ## Return value
/// `0` = item accepted; `1` = null sink/data (closed); `2` = channel full.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_try_write_bytes(
    sink: *mut HewSink,
    data: *const crate::bytes::BytesTriple,
) -> i32 {
    // SendError::Closed = 1
    if sink.is_null() || data.is_null() {
        return 1;
    }
    // SAFETY: `data` points to the caller's valid BytesTriple slot.
    let data = unsafe { &*data };
    // Borrow the active region (empty for a null/0 triple).
    let bytes: &[u8] = if data.len == 0 || data.ptr.is_null() {
        &[]
    } else {
        // SAFETY: `data.ptr + data.offset` is valid for `data.len` bytes per
        // the BytesTriple contract; read-only borrow (no mutation, no free).
        unsafe { std::slice::from_raw_parts(data.ptr.add(data.offset as usize), data.len as usize) }
    };
    // Channel-backed sinks use the core's genuine non-blocking `try_send`
    // (the CallbackSink default would block).
    // SAFETY: sink is valid per caller contract.
    if let Some(core) = unsafe { sink_channel_core(sink) } {
        return if core.try_send(bytes.to_vec()) { 0 } else { 2 };
    }
    // SAFETY:
    //   Provenance: `sink` came from a `hew_*_sink_*` constructor; non-null by guard above.
    //   Type tag: cast to `*mut HewSink` matches declared type.
    //   Lifetime owner: caller retains; we do not free.
    //   Aliasing/concurrency: caller contract bans concurrent writes; backing's try_write is internally synchronised.
    //   Bounds: method dispatch with the bytes slice borrowed above.
    //   Failure mode: violations are UAF / data race. Non-channel sinks fall back to blocking write.
    let accepted = unsafe { (*sink).try_write_item(bytes) };
    if accepted {
        0
    } else {
        2
    } // 0 = Ok, 2 = Full
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    /// Wrapper to send raw sink pointers across thread boundaries.
    ///
    /// # Safety
    ///
    /// The inner pointer must refer to a channel-backed sink (mpsc is thread-safe).
    struct SendSink(*mut HewSink);
    // SAFETY: channel sinks are backed by mpsc::SyncSender which is Send + Sync.
    unsafe impl Send for SendSink {}

    // ── Helpers ──────────────────────────────────────────────────────────

    /// Read all items from a stream via the sized FFI, freeing each malloc'd buffer.
    ///
    /// # Safety
    ///
    /// `stream` must be a valid, non-null `HewStream` pointer.
    unsafe fn drain_stream(stream: *mut HewStream) -> Vec<Vec<u8>> {
        let mut items = Vec::new();
        loop {
            let mut size: usize = 0;
            // SAFETY: stream is valid per caller contract; size is a local.
            let ptr = unsafe { hew_stream_next_sized(stream, &raw mut size) };
            if ptr.is_null() {
                break;
            }
            // SAFETY: ptr is valid for `size` bytes per hew_stream_next_sized contract.
            let bytes = unsafe { std::slice::from_raw_parts(ptr.cast::<u8>(), size).to_vec() };
            // SAFETY: ptr was malloc'd by hew_stream_next_sized.
            unsafe { libc::free(ptr) }; // ALLOCATOR-PAIRING: libc  // CSTRING-FREE: libc-bytes (test stream byte buffer)
            items.push(bytes);
        }
        items
    }

    /// Generate a per-test temp file path that won't collide with parallel runs.
    fn temp_path(name: &str) -> std::path::PathBuf {
        std::env::temp_dir().join(format!(
            "hew_stream_test_{name}_{}_{:?}",
            std::process::id(),
            std::thread::current().id()
        ))
    }

    // ── Validity checks ─────────────────────────────────────────────────

    #[test]
    fn null_stream_reports_invalid() {
        assert_eq!(hew_stream_is_valid(ptr::null()), 0);
    }

    #[test]
    fn null_sink_reports_invalid() {
        assert_eq!(hew_sink_is_valid(ptr::null()), 0);
    }

    #[test]
    fn non_null_stream_reports_valid() {
        let data = b"hello";
        // SAFETY: data is valid for its length.
        let stream = unsafe { hew_stream_from_bytes(data.as_ptr(), data.len(), 0) };
        assert_eq!(hew_stream_is_valid(stream), 1);
        // SAFETY: stream was created above.
        unsafe { hew_stream_close(stream) };
    }

    #[test]
    fn non_null_sink_reports_valid() {
        // SAFETY: hew_stream_channel returns a valid pair.
        unsafe {
            let pair = hew_stream_channel(1);
            let sink = hew_stream_pair_sink(pair);
            assert_eq!(hew_sink_is_valid(sink), 1);
            hew_sink_close(sink);
            hew_stream_pair_free(pair);
        }
    }

    // ── Channel creation and pair extraction ────────────────────────────

    #[test]
    fn channel_creates_non_null_pair() {
        // SAFETY: FFI call with valid capacity.
        let pair = unsafe { hew_stream_channel(4) };
        assert!(!pair.is_null());
        // SAFETY: pair was just created.
        unsafe { hew_stream_pair_free(pair) };
    }

    #[test]
    fn pair_extraction_returns_non_null_handles() {
        // SAFETY: FFI calls with valid pointers.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink = hew_stream_pair_sink(pair);
            let stream = hew_stream_pair_stream(pair);
            assert!(!sink.is_null());
            assert!(!stream.is_null());
            hew_sink_close(sink);
            hew_stream_close(stream);
            hew_stream_pair_free(pair);
        }
    }

    #[test]
    fn pair_sink_null_pair_returns_null() {
        // SAFETY: null is explicitly handled by cabi_guard.
        let result = unsafe { hew_stream_pair_sink(ptr::null_mut()) };
        assert!(result.is_null());
    }

    #[test]
    fn pair_stream_null_pair_returns_null() {
        // SAFETY: null is explicitly handled by cabi_guard.
        let result = unsafe { hew_stream_pair_stream(ptr::null_mut()) };
        assert!(result.is_null());
    }

    #[test]
    fn pair_bytes_aliases_delegate_correctly() {
        // SAFETY: FFI calls with valid pointers.
        unsafe {
            let pair = hew_stream_channel(1);
            let sink = hew_stream_pair_sink_bytes(pair);
            let stream = hew_stream_pair_stream_bytes(pair);
            assert!(!sink.is_null());
            assert!(!stream.is_null());
            hew_sink_close(sink);
            hew_stream_close(stream);
            hew_stream_pair_free(pair);
        }
    }

    #[test]
    fn pair_free_null_is_safe() {
        // Must not crash.
        // SAFETY: null is explicitly handled by hew_stream_pair_free.
        unsafe { hew_stream_pair_free(ptr::null_mut()) };
    }

    // ── Channel write-read round-trip ───────────────────────────────────

    #[test]
    fn channel_write_read_roundtrip() {
        // SAFETY: all FFI calls use valid pointers from prior creation calls.
        unsafe {
            let pair = hew_stream_channel(8);
            let sink = hew_stream_pair_sink(pair);
            let stream = hew_stream_pair_stream(pair);

            let msg = b"hello, channel";
            hew_sink_write(sink, msg.as_ptr().cast(), msg.len());
            hew_sink_close(sink); // signal EOF

            let items = drain_stream(stream);
            assert_eq!(items.len(), 1);
            assert_eq!(items[0], msg);

            hew_stream_close(stream);
            hew_stream_pair_free(pair);
        }
    }

    #[test]
    fn channel_multiple_items_preserve_order() {
        // SAFETY: all FFI calls use valid pointers from prior creation calls.
        unsafe {
            let pair = hew_stream_channel(8);
            let sink = hew_stream_pair_sink(pair);
            let stream = hew_stream_pair_stream(pair);

            for i in 0..5u8 {
                let data = [i; 3]; // e.g. [0,0,0], [1,1,1], ...
                hew_sink_write(sink, data.as_ptr().cast(), data.len());
            }
            hew_sink_close(sink);

            let items = drain_stream(stream);
            assert_eq!(items.len(), 5);
            for (i, item) in items.iter().enumerate() {
                #[expect(clippy::cast_possible_truncation, reason = "test values fit in u8")]
                let expected = vec![i as u8; 3];
                assert_eq!(*item, expected, "item {i} mismatch");
            }

            hew_stream_close(stream);
            hew_stream_pair_free(pair);
        }
    }

    // ── Bytes stream (VecStream via hew_stream_from_bytes) ──────────────

    #[test]
    fn bytes_stream_single_item_when_item_size_zero() {
        let data = b"all at once";
        // SAFETY: data is valid for its length.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let items = drain_stream(stream);
            assert_eq!(items.len(), 1);
            assert_eq!(items[0], data);
            hew_stream_close(stream);
        }
    }

    #[test]
    fn bytes_stream_chunks_by_item_size() {
        let data = b"abcdefghij"; // 10 bytes
                                  // SAFETY: data is valid for its length.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 3);
            let items = drain_stream(stream);
            // 10 / 3 = 3 full chunks + 1 remainder
            assert_eq!(items.len(), 4);
            assert_eq!(items[0], b"abc");
            assert_eq!(items[1], b"def");
            assert_eq!(items[2], b"ghi");
            assert_eq!(items[3], b"j");
            hew_stream_close(stream);
        }
    }

    #[test]
    fn bytes_stream_null_data_yields_empty_stream() {
        // SAFETY: null data is explicitly handled.
        unsafe {
            let stream = hew_stream_from_bytes(ptr::null(), 0, 0);
            assert!(!stream.is_null(), "should return a valid empty stream");
            let items = drain_stream(stream);
            assert!(items.is_empty());
            hew_stream_close(stream);
        }
    }

    #[test]
    fn bytes_stream_zero_length_yields_empty_stream() {
        let data = b"ignored";
        // SAFETY: len=0 triggers the empty-stream path regardless of data pointer.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), 0, 0);
            let items = drain_stream(stream);
            assert!(items.is_empty());
            hew_stream_close(stream);
        }
    }

    #[test]
    fn bytes_stream_binary_data_roundtrip() {
        // Full byte range including embedded NULs.
        let data: Vec<u8> = (0..=255).collect();
        // SAFETY: data is valid for its length.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let items = drain_stream(stream);
            assert_eq!(items.len(), 1);
            assert_eq!(items[0], data);
            hew_stream_close(stream);
        }
    }

    // ── hew_stream_next ─────────────────────────────────────────────────

    // ── hew_stream_next_sized ───────────────────────────────────────────

    #[test]
    fn stream_next_sized_null_returns_null() {
        let mut size: usize = 999;
        // SAFETY: null stream is handled; size is a valid local.
        let result = unsafe { hew_stream_next_sized(ptr::null_mut(), &raw mut size) };
        assert!(result.is_null());
    }

    #[test]
    fn stream_next_sized_reports_correct_length() {
        let data = b"seven!!"; // 7 bytes
                               // SAFETY: data is valid; stream is created from it.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let mut size: usize = 0;
            let buf = hew_stream_next_sized(stream, &raw mut size);
            assert!(!buf.is_null());
            assert_eq!(size, 7);
            libc::free(buf); // ALLOCATOR-PAIRING: libc  // CSTRING-FREE: libc-bytes (test stream byte buffer)
            hew_stream_close(stream);
        }
    }

    #[test]
    fn stream_next_sized_eof_sets_size_zero() {
        // SAFETY: empty data creates an empty stream.
        unsafe {
            let stream = hew_stream_from_bytes(ptr::null(), 0, 0);
            let mut size: usize = 42;
            let buf = hew_stream_next_sized(stream, &raw mut size);
            assert!(buf.is_null());
            assert_eq!(size, 0, "size should be zeroed on EOF");
            hew_stream_close(stream);
        }
    }

    // ── hew_stream_next_view ─────────────────────────────────────────────

    #[test]
    fn stream_next_view_null_stream_returns_eof() {
        let mut buf: *mut u8 = ptr::null_mut();
        let mut cap: usize = 0;
        // SAFETY: null stream is explicitly handled.
        let ret = unsafe { hew_stream_next_view(ptr::null_mut(), &raw mut buf, &raw mut cap) };
        assert_eq!(ret, -1);
    }

    #[test]
    fn stream_next_view_null_buf_returns_eof() {
        let data = b"hello";
        // SAFETY: stream is valid; null buf pointer is handled.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let mut cap: usize = 0;
            let ret = hew_stream_next_view(stream, ptr::null_mut(), &raw mut cap);
            assert_eq!(ret, -1);
            hew_stream_close(stream);
        }
    }

    #[test]
    fn stream_next_view_null_cap_returns_eof() {
        let data = b"hello";
        // SAFETY: stream is valid; null buf_cap pointer is handled.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let mut buf: *mut u8 = ptr::null_mut();
            let ret = hew_stream_next_view(stream, &raw mut buf, ptr::null_mut());
            assert_eq!(ret, -1);
            hew_stream_close(stream);
        }
    }

    #[test]
    fn stream_next_view_copies_into_provided_buffer() {
        let data = b"hello";
        // SAFETY: stream + buffer are valid; buffer is large enough.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let mut buf: *mut u8 = libc::malloc(64).cast::<u8>(); // ALLOCATOR-PAIRING: libc
            let mut cap: usize = 64;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, 5);
            assert_eq!(cap, 64, "capacity unchanged when buffer is large enough");
            let n = usize::try_from(ret).unwrap();
            let slice = std::slice::from_raw_parts(buf, n);
            assert_eq!(slice, b"hello");
            libc::free(buf.cast()); // ALLOCATOR-PAIRING: libc  // CSTRING-FREE: libc-bytes (test next_view byte buffer)
            hew_stream_close(stream);
        }
    }

    #[test]
    fn stream_next_view_grows_undersized_buffer() {
        let data = b"a]longer]payload";
        // Start with a tiny 2-byte buffer — the function must realloc it.
        // SAFETY: stream + buffer are valid; buffer will be grown.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let mut buf: *mut u8 = libc::malloc(2).cast::<u8>(); // ALLOCATOR-PAIRING: libc
            let mut cap: usize = 2;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, i64::try_from(data.len()).unwrap());
            assert!(cap >= data.len(), "capacity must grow to fit the item");
            let n = usize::try_from(ret).unwrap();
            let slice = std::slice::from_raw_parts(buf, n);
            assert_eq!(slice, data);
            libc::free(buf.cast()); // ALLOCATOR-PAIRING: libc  // CSTRING-FREE: libc-bytes (test next_view byte buffer)
            hew_stream_close(stream);
        }
    }

    #[test]
    fn stream_next_view_grows_null_initial_buffer() {
        // Callers may pass a null *buf with zero capacity; realloc(NULL, n)
        // behaves like malloc(n), so this should work transparently.
        let data = b"from_null";
        // SAFETY: stream is valid; null initial buf is handled by realloc semantics.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let mut buf: *mut u8 = ptr::null_mut();
            let mut cap: usize = 0;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, i64::try_from(data.len()).unwrap());
            assert!(!buf.is_null(), "buffer must be allocated");
            let n = usize::try_from(ret).unwrap();
            let slice = std::slice::from_raw_parts(buf, n);
            assert_eq!(slice, b"from_null");
            libc::free(buf.cast()); // ALLOCATOR-PAIRING: libc  // CSTRING-FREE: libc-bytes (test next_view byte buffer)
            hew_stream_close(stream);
        }
    }

    #[test]
    fn stream_next_view_null_buf_with_nonzero_cap_allocates() {
        // Regression: a null *buf with a stale positive capacity must trigger
        // allocation rather than copying into null.
        let data = b"safe";
        // SAFETY: stream is valid; null buf with stale cap is handled.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let mut buf: *mut u8 = ptr::null_mut();
            let mut cap: usize = 128; // stale capacity, buffer is actually null
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, 4);
            assert!(!buf.is_null(), "must allocate when *buf is null");
            let slice = std::slice::from_raw_parts(buf, usize::try_from(ret).unwrap());
            assert_eq!(slice, b"safe");
            libc::free(buf.cast()); // ALLOCATOR-PAIRING: libc  // CSTRING-FREE: libc-bytes (test next_view byte buffer)
            hew_stream_close(stream);
        }
    }

    #[test]
    fn stream_next_view_eof_returns_negative_one() {
        // SAFETY: empty stream hits EOF immediately.
        unsafe {
            let stream = hew_stream_from_bytes(ptr::null(), 0, 0);
            let mut buf: *mut u8 = libc::malloc(16).cast::<u8>(); // ALLOCATOR-PAIRING: libc
            let mut cap: usize = 16;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, -1, "EOF must return -1");
            libc::free(buf.cast()); // ALLOCATOR-PAIRING: libc  // CSTRING-FREE: libc-bytes (test next_view byte buffer)
            hew_stream_close(stream);
        }
    }

    #[test]
    fn stream_next_view_exact_fit() {
        let data = b"ABCD";
        // Buffer exactly matches item size — no realloc needed.
        // SAFETY: stream + buffer are valid; buffer is exactly the right size.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let mut buf: *mut u8 = libc::malloc(4).cast::<u8>(); // ALLOCATOR-PAIRING: libc
            let mut cap: usize = 4;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, 4);
            assert_eq!(cap, 4, "capacity unchanged on exact fit");
            let slice = std::slice::from_raw_parts(buf, 4);
            assert_eq!(slice, b"ABCD");
            libc::free(buf.cast()); // ALLOCATOR-PAIRING: libc  // CSTRING-FREE: libc-bytes (test next_view byte buffer)
            hew_stream_close(stream);
        }
    }

    #[test]
    fn stream_next_view_reuses_buffer_across_calls() {
        // Two items written through a channel; one buffer reused for both reads.
        // SAFETY: channel stream + buffer are valid.
        unsafe {
            let pair = hew_stream_channel(2);
            let sink = hew_stream_pair_sink(pair);
            let stream = hew_stream_pair_stream(pair);
            hew_stream_pair_free(pair);

            let a = b"first";
            let b_data = b"second";
            hew_sink_write(sink, a.as_ptr().cast(), a.len());
            hew_sink_write(sink, b_data.as_ptr().cast(), b_data.len());
            hew_sink_close(sink);

            let mut buf: *mut u8 = libc::malloc(64).cast::<u8>(); // ALLOCATOR-PAIRING: libc
            let mut cap: usize = 64;
            let original_buf = buf;

            let r1 = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(r1, 5);
            assert_eq!(
                std::slice::from_raw_parts(buf, usize::try_from(r1).unwrap()),
                b"first"
            );
            // Buffer pointer should be unchanged (no realloc needed).
            assert_eq!(
                buf, original_buf,
                "buffer must not be reallocated when large enough"
            );

            let r2 = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(r2, 6);
            assert_eq!(
                std::slice::from_raw_parts(buf, usize::try_from(r2).unwrap()),
                b"second"
            );

            let r3 = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(r3, -1, "EOF after all items consumed");

            libc::free(buf.cast()); // ALLOCATOR-PAIRING: libc  // CSTRING-FREE: libc-bytes (test next_view byte buffer)
            hew_stream_close(stream);
        }
    }

    #[test]
    fn stream_next_view_single_byte_item() {
        // Boundary: smallest non-empty item (1 byte).
        let data = b"X";
        // SAFETY: stream + buffer are valid.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let mut buf: *mut u8 = libc::malloc(1).cast::<u8>(); // ALLOCATOR-PAIRING: libc
            let mut cap: usize = 1;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, 1);
            assert_eq!(*buf, b'X');
            let eof = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(eof, -1, "EOF after the single item");
            libc::free(buf.cast()); // ALLOCATOR-PAIRING: libc  // CSTRING-FREE: libc-bytes (test next_view byte buffer)
            hew_stream_close(stream);
        }
    }

    // ── Close / free ────────────────────────────────────────────────────

    #[test]
    fn stream_close_null_is_safe() {
        // Must not crash.
        // SAFETY: null is explicitly handled by hew_stream_close.
        unsafe { hew_stream_close(ptr::null_mut()) };
    }

    #[test]
    fn sink_close_null_is_safe() {
        // Must not crash.
        // SAFETY: null is explicitly handled by hew_sink_close.
        unsafe { hew_sink_close(ptr::null_mut()) };
    }

    // ── Sink operations ─────────────────────────────────────────────────

    #[test]
    fn sink_write_null_sink_is_safe() {
        let data = b"ignored";
        // SAFETY: null sink is handled with early return.
        unsafe { hew_sink_write(ptr::null_mut(), data.as_ptr().cast(), data.len()) };
    }

    #[test]
    fn sink_write_null_data_is_safe() {
        // SAFETY: FFI calls with valid pair; null data is handled.
        unsafe {
            let pair = hew_stream_channel(1);
            let sink = hew_stream_pair_sink(pair);
            hew_sink_write(sink, ptr::null(), 5);
            hew_sink_close(sink);
            hew_stream_pair_free(pair);
        }
    }

    #[test]
    fn sink_write_zero_size_is_noop() {
        // SAFETY: FFI calls with valid pointers; zero-size write is handled.
        unsafe {
            let pair = hew_stream_channel(1);
            let sink = hew_stream_pair_sink(pair);
            let stream = hew_stream_pair_stream(pair);

            let data = b"should not appear";
            hew_sink_write(sink, data.as_ptr().cast(), 0);
            hew_sink_close(sink);

            let items = drain_stream(stream);
            assert!(
                items.is_empty(),
                "zero-size write should not produce an item"
            );
            hew_stream_close(stream);
            hew_stream_pair_free(pair);
        }
    }

    #[test]
    fn sink_flush_null_is_safe() {
        // Must not crash.
        // SAFETY: null is explicitly handled by hew_sink_flush.
        unsafe { hew_sink_flush(ptr::null_mut()) };
    }

    #[test]
    fn sink_flush_channel_is_noop() {
        // Channel sinks have no buffering; flush should succeed silently.
        // SAFETY: FFI calls with valid pointers.
        unsafe {
            let pair = hew_stream_channel(1);
            let sink = hew_stream_pair_sink(pair);
            hew_sink_flush(sink);
            hew_sink_close(sink);
            hew_stream_pair_free(pair);
        }
    }

    // ── File-backed streams ─────────────────────────────────────────────

    #[test]
    fn file_read_null_path_returns_null() {
        // SAFETY: null path is handled by cabi_guard.
        let result = unsafe { hew_stream_from_file_read(ptr::null()) };
        assert!(result.is_null());
    }

    #[test]
    fn file_read_nonexistent_returns_null_with_error() {
        let path = CString::new("/tmp/hew_nonexistent_file_xyz_42").unwrap();
        // SAFETY: path is a valid C string.
        let result = unsafe { hew_stream_from_file_read(path.as_ptr()) };
        assert!(result.is_null());
        // An error should have been recorded.
        let err = crate::stream_error::take_last_error();
        assert!(err.is_some(), "missing file should set an error");
    }

    #[test]
    fn file_read_returns_file_contents() {
        let path = temp_path("read_contents");
        let content = b"Colour, behaviour, neighbour.";
        std::fs::write(&path, content).unwrap();

        let c_path = CString::new(path.to_str().unwrap()).unwrap();
        // SAFETY: c_path is a valid C string pointing to an existing file.
        unsafe {
            let stream = hew_stream_from_file_read(c_path.as_ptr());
            assert!(!stream.is_null());
            let items = drain_stream(stream);
            let all: Vec<u8> = items.into_iter().flatten().collect();
            assert_eq!(all, content);
            hew_stream_close(stream);
        }
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn file_read_empty_file_yields_eof() {
        let path = temp_path("read_empty");
        std::fs::write(&path, b"").unwrap();

        let c_path = CString::new(path.to_str().unwrap()).unwrap();
        // SAFETY: c_path is a valid C string pointing to an existing empty file.
        unsafe {
            let stream = hew_stream_from_file_read(c_path.as_ptr());
            assert!(!stream.is_null());
            let items = drain_stream(stream);
            assert!(items.is_empty());
            hew_stream_close(stream);
        }
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn file_write_null_path_returns_null() {
        // SAFETY: null path is handled by cabi_guard.
        let result = unsafe { hew_stream_from_file_write(ptr::null()) };
        assert!(result.is_null());
    }

    #[test]
    fn file_write_then_read_roundtrip() {
        let path = temp_path("write_roundtrip");
        let c_path = CString::new(path.to_str().unwrap()).unwrap();

        // Write via sink FFI.
        // SAFETY: c_path points to a valid path; sink is created from it.
        unsafe {
            let sink = hew_stream_from_file_write(c_path.as_ptr());
            assert!(!sink.is_null());
            let msg = b"written via FFI";
            hew_sink_write(sink, msg.as_ptr().cast(), msg.len());
            hew_sink_flush(sink);
            hew_sink_close(sink);
        }

        // Verify file contents directly.
        let mut contents = Vec::new();
        std::fs::File::open(&path)
            .unwrap()
            .read_to_end(&mut contents)
            .unwrap();
        assert_eq!(contents, b"written via FFI");
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn file_write_then_stream_read_roundtrip() {
        let path = temp_path("file_roundtrip");
        let c_path = CString::new(path.to_str().unwrap()).unwrap();
        let payload = b"roundtrip payload";

        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            // Write phase.
            let sink = hew_stream_from_file_write(c_path.as_ptr());
            assert!(!sink.is_null());
            hew_sink_write(sink, payload.as_ptr().cast(), payload.len());
            hew_sink_close(sink);

            // Read phase.
            let stream = hew_stream_from_file_read(c_path.as_ptr());
            assert!(!stream.is_null());
            let items = drain_stream(stream);
            let all: Vec<u8> = items.into_iter().flatten().collect();
            assert_eq!(all, payload);
            hew_stream_close(stream);
        }
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn file_write_binary_data_preserved() {
        let path = temp_path("binary_write");
        let c_path = CString::new(path.to_str().unwrap()).unwrap();
        let binary: Vec<u8> = (0..=255).collect();

        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let sink = hew_stream_from_file_write(c_path.as_ptr());
            assert!(!sink.is_null());
            hew_sink_write(sink, binary.as_ptr().cast(), binary.len());
            hew_sink_close(sink);

            let stream = hew_stream_from_file_read(c_path.as_ptr());
            let items = drain_stream(stream);
            let all: Vec<u8> = items.into_iter().flatten().collect();
            assert_eq!(all, binary);
            hew_stream_close(stream);
        }
        let _ = std::fs::remove_file(&path);
    }

    // ── Pipe ────────────────────────────────────────────────────────────

    #[test]
    fn pipe_transfers_all_items() {
        let path = temp_path("pipe_output");
        let c_path = CString::new(path.to_str().unwrap()).unwrap();

        let data = b"piped content";
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let sink = hew_stream_from_file_write(c_path.as_ptr());
            assert!(!stream.is_null());
            assert!(!sink.is_null());
            // pipe consumes both handles.
            hew_stream_pipe(stream, sink);
        }

        let mut contents = Vec::new();
        std::fs::File::open(&path)
            .unwrap()
            .read_to_end(&mut contents)
            .unwrap();
        assert_eq!(contents, data);
        let _ = std::fs::remove_file(&path);
    }

    // ── Lines adapter ───────────────────────────────────────────────────

    #[test]
    fn lines_splits_on_newlines() {
        let data = b"alpha\nbeta\ngamma\n";
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let lines = hew_stream_lines(raw);
            assert!(!lines.is_null());
            let items = drain_stream(lines);
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], b"alpha");
            assert_eq!(items[1], b"beta");
            assert_eq!(items[2], b"gamma");
            hew_stream_close(lines);
        }
    }

    #[test]
    fn lines_handles_crlf() {
        let data = b"line1\r\nline2\r\n";
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let lines = hew_stream_lines(raw);
            let items = drain_stream(lines);
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], b"line1");
            assert_eq!(items[1], b"line2");
            hew_stream_close(lines);
        }
    }

    #[test]
    fn lines_final_line_without_newline_is_yielded() {
        let data = b"first\nsecond";
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let lines = hew_stream_lines(raw);
            let items = drain_stream(lines);
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], b"first");
            assert_eq!(items[1], b"second");
            hew_stream_close(lines);
        }
    }

    #[test]
    fn lines_null_stream_returns_null() {
        // SAFETY: null is handled by cabi_guard.
        let result = unsafe { hew_stream_lines(ptr::null_mut()) };
        assert!(result.is_null());
    }

    #[test]
    fn lines_empty_stream_yields_nothing() {
        // SAFETY: null data yields an empty vec stream.
        unsafe {
            let raw = hew_stream_from_bytes(ptr::null(), 0, 0);
            let lines = hew_stream_lines(raw);
            let items = drain_stream(lines);
            assert!(items.is_empty());
            hew_stream_close(lines);
        }
    }

    #[test]
    fn lines_data_at_buffer_limit_not_truncated() {
        // Data exactly at the limit (with trailing newline) should yield one
        // complete line — no premature truncation.
        let mut data = vec![b'x'; MAX_LINE_BUFFER_SIZE - 1];
        data.push(b'\n');
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let lines = hew_stream_lines(raw);
            let items = drain_stream(lines);
            assert_eq!(items.len(), 1);
            assert_eq!(items[0].len(), MAX_LINE_BUFFER_SIZE - 1);
            hew_stream_close(lines);
        }
    }

    #[test]
    fn lines_exceeding_buffer_limit_yields_truncated_line() {
        // Chunked delivery: two half-chunks fill the buffer to the limit,
        // then the remaining byte arrives in a third chunk.  The adapter
        // must drain exactly MAX_LINE_BUFFER_SIZE, not the whole buffer.
        let chunk_size = MAX_LINE_BUFFER_SIZE / 2;
        let total = MAX_LINE_BUFFER_SIZE + 1;
        let data = vec![b'B'; total];
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), chunk_size);
            let lines = hew_stream_lines(raw);
            let items = drain_stream(lines);
            assert_eq!(items.len(), 2);
            assert_eq!(items[0].len(), MAX_LINE_BUFFER_SIZE);
            assert_eq!(items[1].len(), 1);
            hew_stream_close(lines);
        }
    }

    #[test]
    fn lines_single_oversized_chunk_capped_at_limit() {
        // One chunk larger than the limit arrives all at once (item_size=0).
        // The adapter must still cap the returned line at MAX_LINE_BUFFER_SIZE.
        let total = MAX_LINE_BUFFER_SIZE + 123;
        let data = vec![b'Z'; total];
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let lines = hew_stream_lines(raw);
            let items = drain_stream(lines);
            assert_eq!(items.len(), 2);
            assert_eq!(items[0].len(), MAX_LINE_BUFFER_SIZE);
            assert_eq!(items[1].len(), 123);
            hew_stream_close(lines);
        }
    }

    #[test]
    fn lines_newline_immediately_after_limit_no_spurious_empty() {
        // Buffer fills to exactly the limit, and the next byte is \n.
        // The \n is the delimiter for the oversized line and must be consumed
        // — not yielded as a spurious empty line.
        let chunk_size = MAX_LINE_BUFFER_SIZE;
        let mut data = vec![b'D'; MAX_LINE_BUFFER_SIZE];
        data.push(b'\n');
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), chunk_size);
            let lines = hew_stream_lines(raw);
            let items = drain_stream(lines);
            assert_eq!(items.len(), 1);
            assert_eq!(items[0].len(), MAX_LINE_BUFFER_SIZE);
            hew_stream_close(lines);
        }
    }

    #[test]
    fn lines_crlf_immediately_after_limit_no_spurious_empty() {
        // Same as above but with \r\n delimiter.
        let chunk_size = MAX_LINE_BUFFER_SIZE;
        let mut data = vec![b'E'; MAX_LINE_BUFFER_SIZE];
        data.extend_from_slice(b"\r\n");
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), chunk_size);
            let lines = hew_stream_lines(raw);
            let items = drain_stream(lines);
            assert_eq!(items.len(), 1);
            assert_eq!(items[0].len(), MAX_LINE_BUFFER_SIZE);
            hew_stream_close(lines);
        }
    }

    #[test]
    fn lines_newline_after_limit_splits_correctly() {
        // Buffer fills past the limit, then the next chunk has a real line.
        // The oversized prefix flushes first; subsequent data splits normally.
        let chunk_size = MAX_LINE_BUFFER_SIZE;
        let mut data = vec![b'C'; MAX_LINE_BUFFER_SIZE];
        data.extend_from_slice(b"tail\n");
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), chunk_size);
            let lines = hew_stream_lines(raw);
            let items = drain_stream(lines);
            assert_eq!(items.len(), 2);
            assert_eq!(items[0].len(), MAX_LINE_BUFFER_SIZE);
            assert_eq!(items[1], b"tail");
            hew_stream_close(lines);
        }
    }

    #[test]
    fn lines_crlf_split_across_flush_boundary_no_spurious_empty() {
        // \r and \n arrive in separate chunks after a forced flush.
        // The adapter must wait for the \n before clearing skip_next_delimiter.
        let limit = MAX_LINE_BUFFER_SIZE;
        let mut data = vec![b'F'; limit];
        data.push(b'\r');
        data.push(b'\n');
        // item_size = limit+1 puts the payload + \r in chunk 1, \n in chunk 2.
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), limit + 1);
            let lines = hew_stream_lines(raw);
            let items = drain_stream(lines);
            assert_eq!(items.len(), 1);
            assert_eq!(items[0].len(), limit);
            hew_stream_close(lines);
        }
    }

    // ── Chunks adapter ──────────────────────────────────────────────────

    #[test]
    fn chunks_yields_fixed_size_pieces() {
        let data = b"123456789"; // 9 bytes
                                 // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let chunked = hew_stream_chunks(raw, 3);
            assert!(!chunked.is_null());
            let items = drain_stream(chunked);
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], b"123");
            assert_eq!(items[1], b"456");
            assert_eq!(items[2], b"789");
            hew_stream_close(chunked);
        }
    }

    #[test]
    fn chunks_last_chunk_may_be_shorter() {
        let data = b"12345"; // 5 bytes, chunk_size=3
                             // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let chunked = hew_stream_chunks(raw, 3);
            let items = drain_stream(chunked);
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], b"123");
            assert_eq!(items[1], b"45");
            hew_stream_close(chunked);
        }
    }

    #[test]
    fn chunks_null_stream_returns_null() {
        // SAFETY: null is handled by cabi_guard.
        let result = unsafe { hew_stream_chunks(ptr::null_mut(), 10) };
        assert!(result.is_null());
    }

    #[test]
    fn chunks_negative_size_clamps_to_one() {
        let data = b"abcd";
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let chunked = hew_stream_chunks(raw, -5);
            let items = drain_stream(chunked);
            // chunk_size clamps to 1, so 4 items of 1 byte each.
            assert_eq!(items.len(), 4);
            assert_eq!(items[0], b"a");
            assert_eq!(items[3], b"d");
            hew_stream_close(chunked);
        }
    }

    // ── Collect string ──────────────────────────────────────────────────

    #[test]
    fn collect_string_concatenates_items() {
        let data = b"hello world";
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            // Split into 5-byte chunks: "hello", " worl", "d"
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 5);
            let cstr_ptr = hew_stream_collect_string(stream);
            assert!(!cstr_ptr.is_null());
            let result = CStr::from_ptr(cstr_ptr).to_str().unwrap();
            assert_eq!(result, "hello world");
            crate::cabi::free_cstring(cstr_ptr); // CSTRING-FREE: str-open (test frees hew_stream_collect_string output)
                                                 // stream is consumed by collect_string; do not close.
        }
    }

    #[test]
    fn collect_string_null_returns_null() {
        // SAFETY: null is handled by cabi_guard.
        let result = unsafe { hew_stream_collect_string(ptr::null_mut()) };
        assert!(result.is_null());
    }

    #[test]
    fn collect_string_empty_stream_returns_empty() {
        // SAFETY: null data yields an empty vec stream.
        unsafe {
            let stream = hew_stream_from_bytes(ptr::null(), 0, 0);
            let cstr_ptr = hew_stream_collect_string(stream);
            assert!(!cstr_ptr.is_null());
            let result = CStr::from_ptr(cstr_ptr).to_str().unwrap();
            assert!(result.is_empty());
            crate::cabi::free_cstring(cstr_ptr); // CSTRING-FREE: str-open (test frees hew_stream_collect_string output)
        }
    }

    // ── Count ───────────────────────────────────────────────────────────

    #[test]
    fn count_returns_item_count() {
        let data = b"abcdef"; // 6 bytes, item_size=2 → 3 items
                              // SAFETY: all FFI calls use valid pointers.
        let count = unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 2);
            hew_stream_count(stream) // consumes stream
        };
        assert_eq!(count, 3);
    }

    #[test]
    fn count_null_returns_zero() {
        // SAFETY: null is handled by cabi_guard.
        assert_eq!(unsafe { hew_stream_count(ptr::null_mut()) }, 0);
    }

    #[test]
    fn count_empty_stream_returns_zero() {
        // SAFETY: null data yields an empty vec stream.
        let count = unsafe {
            let stream = hew_stream_from_bytes(ptr::null(), 0, 0);
            hew_stream_count(stream)
        };
        assert_eq!(count, 0);
    }

    // ── Write string ────────────────────────────────────────────────────

    #[test]
    fn write_string_sends_bytes_to_channel() {
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink = hew_stream_pair_sink(pair);
            let stream = hew_stream_pair_stream(pair);

            let msg = CString::new("bonjour").unwrap();
            hew_sink_write_string(sink, msg.as_ptr());
            hew_sink_close(sink);

            let items = drain_stream(stream);
            assert_eq!(items.len(), 1);
            assert_eq!(items[0], b"bonjour");
            hew_stream_close(stream);
            hew_stream_pair_free(pair);
        }
    }

    #[test]
    fn write_string_null_sink_is_safe() {
        let msg = CString::new("ignored").unwrap();
        // SAFETY: null sink is handled by cabi_guard.
        unsafe { hew_sink_write_string(ptr::null_mut(), msg.as_ptr()) };
    }

    #[test]
    fn write_string_null_data_is_safe() {
        // SAFETY: FFI calls with valid pair; null data is handled by cabi_guard.
        unsafe {
            let pair = hew_stream_channel(1);
            let sink = hew_stream_pair_sink(pair);
            hew_sink_write_string(sink, ptr::null());
            hew_sink_close(sink);
            hew_stream_pair_free(pair);
        }
    }

    // ── Is closed ───────────────────────────────────────────────────────

    #[test]
    fn is_closed_null_returns_one() {
        // SAFETY: null is handled by cabi_guard (returns 1).
        assert_eq!(unsafe { hew_stream_is_closed(ptr::null_mut()) }, 1);
    }

    #[test]
    fn is_closed_false_for_non_empty_vec_stream() {
        let data = b"content";
        // SAFETY: data is valid; stream is created from it.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            assert_eq!(hew_stream_is_closed(stream), 0);
            hew_stream_close(stream);
        }
    }

    #[test]
    fn is_closed_true_for_exhausted_vec_stream() {
        let data = b"x";
        // SAFETY: data is valid; stream is created from it.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            // Drain the stream via the sized accessor (raw libc buffer).
            let mut size: usize = 0;
            let buf = hew_stream_next_sized(stream, std::ptr::addr_of_mut!(size));
            assert!(!buf.is_null());
            libc::free(buf); // ALLOCATOR-PAIRING: libc
            assert_eq!(hew_stream_is_closed(stream), 1);
            hew_stream_close(stream);
        }
    }

    // ── Take adapter ────────────────────────────────────────────────────

    #[test]
    fn take_limits_items_yielded() {
        let data = b"abcdef"; // item_size=1 → 6 items
                              // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 1);
            let taken = hew_stream_take(raw, 3);
            let items = drain_stream(taken);
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], b"a");
            assert_eq!(items[1], b"b");
            assert_eq!(items[2], b"c");
            hew_stream_close(taken);
        }
    }

    #[test]
    fn take_zero_yields_nothing() {
        let data = b"nonempty";
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 1);
            let taken = hew_stream_take(raw, 0);
            let items = drain_stream(taken);
            assert!(items.is_empty());
            hew_stream_close(taken);
        }
    }

    #[test]
    fn take_negative_yields_nothing() {
        let data = b"stuff";
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 1);
            let taken = hew_stream_take(raw, -10);
            let items = drain_stream(taken);
            assert!(items.is_empty());
            hew_stream_close(taken);
        }
    }

    #[test]
    fn take_null_stream_returns_null() {
        // SAFETY: null is handled by cabi_guard.
        let result = unsafe { hew_stream_take(ptr::null_mut(), 5) };
        assert!(result.is_null());
    }

    #[test]
    fn take_more_than_available_yields_all() {
        let data = b"ab"; // 2 items of 1 byte
                          // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 1);
            let taken = hew_stream_take(raw, 100);
            let items = drain_stream(taken);
            assert_eq!(items.len(), 2);
            hew_stream_close(taken);
        }
    }

    // ── Large data ──────────────────────────────────────────────────────

    #[test]
    fn large_write_read_roundtrip_via_channel() {
        // 64 KiB of patterned data — verifies no off-by-one in length handling.
        let large: Vec<u8> = (0..65536u32).map(|i| (i % 251) as u8).collect();
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink = SendSink(hew_stream_pair_sink(pair));
            let stream = hew_stream_pair_stream(pair);

            let large_clone = large.clone();
            let sink_thread = std::thread::spawn(move || {
                let s = &sink; // force capture of entire SendSink
                hew_sink_write(s.0, large_clone.as_ptr().cast(), large_clone.len());
                hew_sink_close(s.0);
            });

            let items = drain_stream(stream);
            sink_thread.join().unwrap();

            let all: Vec<u8> = items.into_iter().flatten().collect();
            assert_eq!(all, large);

            hew_stream_close(stream);
            hew_stream_pair_free(pair);
        }
    }

    #[test]
    fn large_file_roundtrip() {
        let path = temp_path("large_file");
        let c_path = CString::new(path.to_str().unwrap()).unwrap();
        let large: Vec<u8> = (0..100_000u32).map(|i| (i % 199) as u8).collect();

        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let sink = hew_stream_from_file_write(c_path.as_ptr());
            assert!(!sink.is_null());
            hew_sink_write(sink, large.as_ptr().cast(), large.len());
            hew_sink_close(sink);

            let stream = hew_stream_from_file_read(c_path.as_ptr());
            assert!(!stream.is_null());
            let items = drain_stream(stream);
            let all: Vec<u8> = items.into_iter().flatten().collect();
            assert_eq!(all.len(), large.len());
            assert_eq!(all, large);
            hew_stream_close(stream);
        }
        let _ = std::fs::remove_file(&path);
    }

    // ── Threaded channel ────────────────────────────────────────────────

    #[test]
    fn channel_concurrent_producer_consumer() {
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let pair = hew_stream_channel(2);
            let sink = SendSink(hew_stream_pair_sink(pair));
            let stream = hew_stream_pair_stream(pair);

            let producer = std::thread::spawn(move || {
                let s = &sink; // force capture of entire SendSink
                for i in 0u8..20 {
                    let data = [i];
                    hew_sink_write(s.0, data.as_ptr().cast(), 1);
                }
                hew_sink_close(s.0);
            });

            let items = drain_stream(stream);
            producer.join().unwrap();

            assert_eq!(items.len(), 20);
            for (i, item) in items.iter().enumerate() {
                #[expect(clippy::cast_possible_truncation, reason = "test values fit in u8")]
                let expected = i as u8;
                assert_eq!(item, &[expected], "item {i} mismatch");
            }

            hew_stream_close(stream);
            hew_stream_pair_free(pair);
        }
    }

    // ── Composed adapters ───────────────────────────────────────────────

    #[test]
    fn lines_then_take_limits_line_count() {
        let data = b"a\nb\nc\nd\ne\n";
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let lines = hew_stream_lines(raw);
            let taken = hew_stream_take(lines, 2);
            let items = drain_stream(taken);
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], b"a");
            assert_eq!(items[1], b"b");
            hew_stream_close(taken);
        }
    }

    #[test]
    fn chunks_then_count_yields_correct_total() {
        let data = b"0123456789"; // 10 bytes, chunks of 4 → 3 chunks (4+4+2)
                                  // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let chunked = hew_stream_chunks(raw, 4);
            let count = hew_stream_count(chunked); // consumes chunked
            assert_eq!(count, 3);
        }
    }

    // ── hew_stream_poll / hew_stream_cancel_pending_read ─────────────────────
    //
    // These tests exercise the cancellable, non-consuming poll ABI used by
    // `select{}` stream-next arms. The substrate is documented in
    // `hew_stream_poll`; the tests cover positive and negative paths per
    // architecture doc §3.3 (both-path coverage) and §5.7 (no racing polling
    // loops — we use the channel close signal as the deterministic clock).

    use std::sync::{
        atomic::{AtomicUsize, Ordering as TestOrdering},
        Arc as TestArc, Condvar as TestCondvar, Mutex as TestMutex,
    };
    use std::time::Duration as TestDuration;

    /// Shared sink the callbacks below post into. We capture both whether
    /// the callback fired and the payload bytes so the assertion can
    /// distinguish None-on-EOF from item-on-win.
    ///
    /// `ready` + `notify` form a Condvar pair so `wait_for_callback` can
    /// block without spinning (§5.7: no sleep-based polling in time-dependent
    /// tests).
    struct CallbackSink {
        fired: AtomicUsize,
        last_was_null: AtomicUsize,
        // We record one byte from the item as a deterministic check —
        // enough to distinguish "stream sent X" from "stream sent Y".
        last_first_byte: AtomicUsize,
        /// Guards `ready`; Condvar wakes `wait_for_callback`.
        ready: TestMutex<bool>,
        notify: TestCondvar,
    }

    impl Default for CallbackSink {
        fn default() -> Self {
            Self {
                fired: AtomicUsize::new(0),
                last_was_null: AtomicUsize::new(0),
                last_first_byte: AtomicUsize::new(0),
                ready: TestMutex::new(false),
                notify: TestCondvar::new(),
            }
        }
    }

    extern "C" fn record_callback(userdata: *mut c_void, item: *mut c_void) {
        // SAFETY: tests construct userdata from `Arc::into_raw(Arc<CallbackSink>)`
        // and keep that Arc alive for the duration of the test.
        let sink = unsafe { &*(userdata as *const CallbackSink) };
        // Order: record payload metadata BEFORE the fired counter so any
        // waiter that observes fired>0 also sees the metadata. The waiter
        // uses fired as the readiness signal; treating it as a happens-before
        // release for last_was_null/last_first_byte requires the metadata
        // store to precede the fired bump.
        if item.is_null() {
            sink.last_was_null.store(1, TestOrdering::Release);
        } else {
            // SAFETY: item is the malloc'd buffer; we just peek the first byte.
            let first = unsafe { *(item as *const u8) };
            sink.last_first_byte
                .store(first as usize, TestOrdering::Release);
            // The callback owns the buffer.
            // SAFETY: item was malloc'd by hew_stream_poll's park thread.
            unsafe { libc::free(item) }; // ALLOCATOR-PAIRING: libc  // CSTRING-FREE: libc-bytes (stream poll item byte buffer)
        }
        sink.fired.fetch_add(1, TestOrdering::Release);
        // Signal the waiter — set ready under the lock, then notify.
        *sink
            .ready
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = true;
        sink.notify.notify_all();
    }

    /// Block until the callback fires or `deadline_ms` elapses.
    ///
    /// Uses a Condvar rather than a sleep loop (§5.7).
    fn wait_for_callback(sink: &CallbackSink, deadline_ms: u64) -> bool {
        let deadline = TestDuration::from_millis(deadline_ms);
        let mut guard = sink
            .ready
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        loop {
            if *guard {
                return true;
            }
            let (new_guard, timed_out) = sink
                .notify
                .wait_timeout(guard, deadline)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            guard = new_guard;
            if timed_out.timed_out() {
                return false;
            }
        }
    }

    /// Block until at least `min_gen` park threads spawned by `hew_stream_poll`
    /// on `stream` have exited, or `deadline_ms` elapses. Uses the
    /// `park_exit_gen` Condvar on the stream rather than sleeping (§5.7).
    /// Returns `true` if the generation was reached in time.
    ///
    /// Pass the generation count before the poll as `min_gen` (i.e., snapshot
    /// `park_exit_gen` before calling `hew_stream_poll`, then call this after).
    ///
    /// # Safety
    ///
    /// `stream` must be a valid, live `*mut HewStream`.
    unsafe fn wait_park_exited(stream: *mut HewStream, min_gen: u64, deadline_ms: u64) -> bool {
        let deadline = TestDuration::from_millis(deadline_ms);
        // SAFETY: stream is valid and live per caller contract.
        let (lock, cvar) = &*unsafe { (*stream).park_exit_gen.clone() };
        let mut guard = lock
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        loop {
            if *guard >= min_gen {
                return true;
            }
            let (new_guard, timed_out) = cvar
                .wait_timeout(guard, deadline)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            guard = new_guard;
            if timed_out.timed_out() {
                return false;
            }
        }
    }

    /// Return the current value of the park-exit generation counter for `stream`.
    /// Use this to snapshot the counter before a `hew_stream_poll` call so that
    /// `wait_park_exited` can target a specific generation.
    ///
    /// # Safety
    ///
    /// `stream` must be a valid, live `*mut HewStream`.
    unsafe fn snapshot_park_exit_gen(stream: *mut HewStream) -> u64 {
        // SAFETY: stream is valid and live per caller contract.
        let arc = unsafe { (*stream).park_exit_gen.clone() };
        let (lock, _) = &*arc;
        let gen = *lock
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        gen
    }

    /// Positive path: when an item arrives on the stream, the registered
    /// callback fires with a non-null payload and the stream remains
    /// usable (`pending_read` cleared, next poll permitted).
    #[test]
    fn stream_poll_invokes_callback_on_item() {
        let sink = TestArc::new(CallbackSink::default());
        // SAFETY: test owns all pointers; sink is kept alive for the test.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink_ptr = hew_stream_pair_sink(pair);
            let stream_ptr = hew_stream_pair_stream(pair);
            hew_stream_pair_free(pair);

            // Spawn a writer that delivers one item after a brief delay
            // so the park thread actually has to wait inside `next()`.
            let sink_addr = sink_ptr as usize;
            let writer = std::thread::spawn(move || {
                std::thread::sleep(TestDuration::from_millis(20));
                let payload = [0x42_u8];
                hew_sink_write(
                    sink_addr as *mut HewSink,
                    payload.as_ptr().cast::<c_void>(),
                    1,
                );
            });

            let userdata = TestArc::as_ptr(&sink) as *mut c_void;
            let id = hew_stream_poll(stream_ptr, record_callback, userdata);
            assert!(id > 0, "poll must return a non-zero id");

            writer.join().unwrap();
            assert!(
                wait_for_callback(&sink, 500),
                "callback must fire once the writer sends an item",
            );
            assert_eq!(sink.fired.load(TestOrdering::Acquire), 1);
            assert_eq!(sink.last_was_null.load(TestOrdering::Acquire), 0);
            assert_eq!(sink.last_first_byte.load(TestOrdering::Acquire), 0x42);

            // pending_read should have been cleared by the park thread.
            assert_eq!((*stream_ptr).pending_read.load(Ordering::Acquire), 0);

            hew_stream_close(stream_ptr);
            // sink_ptr was passed into hew_sink_write; the channel pair's
            // Drop took care of the surviving side that wasn't extracted —
            // here we extracted both, so free the sink directly.
            drop(Box::from_raw(sink_ptr)); // ALLOCATOR-PAIRING: GlobalAlloc
        }
    }

    /// EOF path: when the stream is closed before any item arrives, the
    /// callback fires with a null item pointer (the EOF-wins signal).
    #[test]
    fn stream_poll_invokes_callback_with_null_on_eof() {
        let sink = TestArc::new(CallbackSink::default());
        // SAFETY: standard test ownership; sink survives the test.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink_ptr = hew_stream_pair_sink(pair);
            let stream_ptr = hew_stream_pair_stream(pair);
            hew_stream_pair_free(pair);

            // Drop the sink to signal EOF after the park thread blocks.
            let sink_addr = sink_ptr as usize;
            let writer = std::thread::spawn(move || {
                std::thread::sleep(TestDuration::from_millis(20));
                drop(Box::from_raw(sink_addr as *mut HewSink)); // ALLOCATOR-PAIRING: GlobalAlloc
            });

            let userdata = TestArc::as_ptr(&sink) as *mut c_void;
            let id = hew_stream_poll(stream_ptr, record_callback, userdata);
            assert!(id > 0);

            writer.join().unwrap();
            assert!(
                wait_for_callback(&sink, 500),
                "callback must fire once the sink is dropped (EOF)",
            );
            assert_eq!(sink.fired.load(TestOrdering::Acquire), 1);
            assert_eq!(
                sink.last_was_null.load(TestOrdering::Acquire),
                1,
                "EOF must surface as a null item pointer",
            );

            hew_stream_close(stream_ptr);
        }
    }

    /// Cancel-before-item path: when cancel runs before any item is
    /// produced, the callback is never fired. The stream remains usable
    /// — a subsequent direct `hew_stream_next` can still read items that
    /// arrive afterwards.
    #[test]
    fn stream_poll_cancel_before_item_suppresses_callback() {
        let sink = TestArc::new(CallbackSink::default());
        // SAFETY: standard test ownership.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink_ptr = hew_stream_pair_sink(pair);
            let stream_ptr = hew_stream_pair_stream(pair);
            hew_stream_pair_free(pair);

            let userdata = TestArc::as_ptr(&sink) as *mut c_void;
            let gen_before = snapshot_park_exit_gen(stream_ptr);
            let id = hew_stream_poll(stream_ptr, record_callback, userdata);
            assert!(id > 0);

            // Cancel before anyone writes. The park thread is still
            // blocked inside `next()`; sending an item will unblock it
            // but the state lock will surface Cancelled and the item is
            // dropped.
            hew_stream_cancel_pending_read(stream_ptr, id);

            // pending_read must be cleared by cancel.
            assert_eq!((*stream_ptr).pending_read.load(Ordering::Acquire), 0);

            // Unblock the park thread by sending one item; this item will
            // be eaten by the cancel path (the documented item-loss caveat).
            let payload = [0xAA_u8];
            hew_sink_write(sink_ptr, payload.as_ptr().cast::<c_void>(), 1);

            // Wait for the park thread to observe Cancelled and exit.
            assert!(
                wait_park_exited(stream_ptr, gen_before + 1, 30_000),
                "park thread must exit within deadline after cancel+write",
            );

            // The callback must NOT have fired.
            assert_eq!(
                sink.fired.load(TestOrdering::Acquire),
                0,
                "cancelled poll must not invoke the callback",
            );

            // The stream is still usable: send another item and read it
            // directly via the consuming variant.
            let payload2 = [0xBB_u8];
            hew_sink_write(sink_ptr, payload2.as_ptr().cast::<c_void>(), 1);
            let mut size: usize = 0;
            let next = hew_stream_next_sized(stream_ptr, std::ptr::addr_of_mut!(size));
            assert!(!next.is_null(), "stream must remain readable after cancel");
            assert_eq!(size, 1);
            assert_eq!(*next.cast::<u8>(), 0xBB);
            libc::free(next); // ALLOCATOR-PAIRING: libc

            hew_stream_close(stream_ptr);
            drop(Box::from_raw(sink_ptr)); // ALLOCATOR-PAIRING: GlobalAlloc
        }
    }

    /// Cancel-after-completion is a silent no-op: once the callback has
    /// fired, cancelling with the (now stale) id is harmless.
    #[test]
    fn stream_poll_cancel_after_completion_is_noop() {
        let sink = TestArc::new(CallbackSink::default());
        // SAFETY: standard test ownership.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink_ptr = hew_stream_pair_sink(pair);
            let stream_ptr = hew_stream_pair_stream(pair);
            hew_stream_pair_free(pair);

            // Send before polling so the item is available immediately.
            let payload = [0xCC_u8];
            hew_sink_write(sink_ptr, payload.as_ptr().cast::<c_void>(), 1);

            let userdata = TestArc::as_ptr(&sink) as *mut c_void;
            let id = hew_stream_poll(stream_ptr, record_callback, userdata);
            assert!(id > 0);

            assert!(
                wait_for_callback(&sink, 500),
                "callback must fire on the queued item",
            );
            assert_eq!(sink.fired.load(TestOrdering::Acquire), 1);
            assert_eq!(sink.last_first_byte.load(TestOrdering::Acquire), 0xCC);

            // Cancel with the stale id — no-op.
            hew_stream_cancel_pending_read(stream_ptr, id);
            // Callback count unchanged.
            assert_eq!(sink.fired.load(TestOrdering::Acquire), 1);
            // Stream is still usable.
            assert_eq!((*stream_ptr).pending_read.load(Ordering::Acquire), 0);

            hew_stream_close(stream_ptr);
            drop(Box::from_raw(sink_ptr)); // ALLOCATOR-PAIRING: GlobalAlloc
        }
    }

    /// Mismatched-id cancel is a silent no-op even when a poll is in flight.
    #[test]
    fn stream_poll_cancel_with_wrong_id_is_noop() {
        let sink = TestArc::new(CallbackSink::default());
        // SAFETY: standard test ownership.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink_ptr = hew_stream_pair_sink(pair);
            let stream_ptr = hew_stream_pair_stream(pair);
            hew_stream_pair_free(pair);

            let userdata = TestArc::as_ptr(&sink) as *mut c_void;
            let id = hew_stream_poll(stream_ptr, record_callback, userdata);
            assert!(id > 0);

            // Cancel with the wrong id; the live registration must survive.
            hew_stream_cancel_pending_read(stream_ptr, id.wrapping_add(7));
            assert_eq!(
                (*stream_ptr).pending_read.load(Ordering::Acquire),
                id,
                "mismatched cancel must not clear the pending_read slot",
            );

            // Now drop the sink — EOF wakes the park thread and the
            // callback fires normally.
            drop(Box::from_raw(sink_ptr)); // ALLOCATOR-PAIRING: GlobalAlloc
            assert!(wait_for_callback(&sink, 500));
            assert_eq!(sink.fired.load(TestOrdering::Acquire), 1);
            assert_eq!(sink.last_was_null.load(TestOrdering::Acquire), 1);

            hew_stream_close(stream_ptr);
        }
    }

    /// Null-stream poll returns the error sentinel 0.
    #[test]
    fn stream_poll_null_stream_returns_zero() {
        // SAFETY: hew_stream_poll documents null-pointer safety.
        unsafe {
            let id = hew_stream_poll(ptr::null_mut(), record_callback, ptr::null_mut());
            assert_eq!(id, 0);
        }
    }

    /// Null-stream cancel is a silent no-op.
    #[test]
    fn stream_cancel_null_stream_is_safe() {
        // SAFETY: hew_stream_cancel_pending_read documents null-pointer safety.
        unsafe {
            hew_stream_cancel_pending_read(ptr::null_mut(), 1);
        }
    }

    /// Zero-id cancel is a silent no-op (0 is the error sentinel).
    #[test]
    fn stream_cancel_zero_id_is_safe() {
        // SAFETY: standard FFI null/sentinel safety check.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink_ptr = hew_stream_pair_sink(pair);
            let stream_ptr = hew_stream_pair_stream(pair);
            hew_stream_pair_free(pair);

            hew_stream_cancel_pending_read(stream_ptr, 0);

            hew_stream_close(stream_ptr);
            drop(Box::from_raw(sink_ptr)); // ALLOCATOR-PAIRING: GlobalAlloc
        }
    }

    /// Pending-read ids are monotonic and unique within a process — the
    /// codegen contract relies on no id ever repeating for a stream's
    /// lifetime.
    #[test]
    fn stream_poll_assigns_monotonic_ids() {
        let sink = TestArc::new(CallbackSink::default());
        // SAFETY: standard test ownership.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink_ptr = hew_stream_pair_sink(pair);
            let stream_ptr = hew_stream_pair_stream(pair);
            hew_stream_pair_free(pair);

            let userdata = TestArc::as_ptr(&sink) as *mut c_void;
            let gen_0 = snapshot_park_exit_gen(stream_ptr);
            let id1 = hew_stream_poll(stream_ptr, record_callback, userdata);
            hew_stream_cancel_pending_read(stream_ptr, id1);

            // Send an item to unblock the park thread after cancel; wait for
            // it to exit before issuing the next poll.
            let payload = [0xEE_u8];
            hew_sink_write(sink_ptr, payload.as_ptr().cast::<c_void>(), 1);
            assert!(
                wait_park_exited(stream_ptr, gen_0 + 1, 30_000),
                "park thread must exit within deadline after first cancel+write",
            );

            let gen_1 = snapshot_park_exit_gen(stream_ptr);
            let id2 = hew_stream_poll(stream_ptr, record_callback, userdata);
            assert!(id2 > id1, "ids must be monotonic ({id2} > {id1})");

            hew_stream_cancel_pending_read(stream_ptr, id2);
            let payload2 = [0xEF_u8];
            hew_sink_write(sink_ptr, payload2.as_ptr().cast::<c_void>(), 1);
            assert!(
                wait_park_exited(stream_ptr, gen_1 + 1, 30_000),
                "park thread must exit within deadline after second cancel+write",
            );

            hew_stream_close(stream_ptr);
            drop(Box::from_raw(sink_ptr)); // ALLOCATOR-PAIRING: GlobalAlloc
        }
    }

    /// A second call to `hew_stream_poll` with no intervening cancel must
    /// abort the process. Tested via subprocess because `abort()` terminates
    /// the whole process — it cannot be caught in-process.
    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn stream_poll_double_register_aborts() {
        // Spawn a child that runs the helper below via HEW_DEATH_TEST gate.
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "stream::tests::_helper_stream_poll_double_register",
            ])
            .env("RUST_TEST_THREADS", "1")
            .env("HEW_DEATH_TEST", "_helper_stream_poll_double_register")
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "second poll on a stream with a live registration must terminate abnormally"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn _helper_stream_poll_double_register() {
        if std::env::var("HEW_DEATH_TEST")
            .map_or(true, |v| v != "_helper_stream_poll_double_register")
        {
            return;
        }
        // SAFETY: standard test ownership. The second poll must abort.
        unsafe {
            let pair = hew_stream_channel(4);
            let stream_ptr = hew_stream_pair_stream(pair);
            hew_stream_pair_free(pair);

            let sink = CallbackSink::default();
            let userdata = std::ptr::addr_of!(sink).cast::<c_void>().cast_mut();
            let _id = hew_stream_poll(stream_ptr, record_callback, userdata);
            // This second call must abort the process.
            hew_stream_poll(stream_ptr, record_callback, userdata);
        }
    }

    // ── TcpStreamBacking unit tests ──────────────────────────────────────

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn tcp_stream_backing_reads_bytes_until_peer_closes() {
        use std::io::Write;
        use std::net::{TcpListener, TcpStream};

        // Bind on OS-assigned port to avoid conflicts in parallel test runs.
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let addr = listener.local_addr().unwrap();

        let payload = b"hello from peer";

        // Peer thread: write payload then close.
        let t = std::thread::spawn(move || {
            let mut peer = TcpStream::connect(addr).unwrap();
            peer.write_all(payload).unwrap();
            // peer drops here, closing the connection (EOF to the reader).
        });

        let (accepted, _) = listener.accept().unwrap();
        let mut backing = TcpStreamBacking { stream: accepted };

        // Read all items until EOF.
        let mut collected = Vec::new();
        while let Some(chunk) = backing.next() {
            collected.extend_from_slice(&chunk);
        }

        t.join().unwrap();

        assert_eq!(collected, payload);
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn tcp_stream_backing_records_error_on_reset() {
        use std::net::{TcpListener, TcpStream};
        #[cfg(unix)]
        use std::os::unix::io::AsRawFd;

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let addr = listener.local_addr().unwrap();

        // Peer thread: connect, set SO_LINGER=0, then drop to send RST.
        let t = std::thread::spawn(move || {
            let peer = TcpStream::connect(addr).unwrap();
            // Force RST on close by setting SO_LINGER with l_onoff=1, l_linger=0.
            #[cfg(unix)]
            // SAFETY: setsockopt is called with a valid fd and a stack-allocated
            // linger struct whose address and size are correct for SO_LINGER.
            unsafe {
                let fd = peer.as_raw_fd();
                let linger = libc::linger {
                    l_onoff: 1,
                    l_linger: 0,
                };
                libc::setsockopt(
                    fd,
                    libc::SOL_SOCKET,
                    libc::SO_LINGER,
                    std::ptr::addr_of!(linger).cast::<libc::c_void>(),
                    // socklen_t is u32; sizeof(linger) is always <= 8 bytes, safe to cast.
                    #[allow(
                        clippy::cast_possible_truncation,
                        reason = "sizeof(linger) fits in u32"
                    )]
                    {
                        std::mem::size_of::<libc::linger>() as libc::socklen_t
                    },
                );
            }
            drop(peer);
        });

        // On FreeBSD (and other BSDs), `accept()` may return ECONNABORTED
        // (os error 53) when the peer's RST arrives before the accept
        // completes. This is POSIX-permitted BSD behaviour; Linux silently
        // drops the aborted connection and waits for the next one.  Retry
        // until we get a connection or a genuinely unexpected error.
        let accepted = loop {
            match listener.accept() {
                Ok((stream, _)) => break stream,
                Err(e) if e.kind() == std::io::ErrorKind::ConnectionAborted => {
                    // Peer RST arrived before accept completed; retry.
                }
                Err(e) => panic!("listener.accept() failed unexpectedly: {e}"),
            }
        };
        t.join().unwrap();

        // Give the RST time to arrive.
        std::thread::sleep(std::time::Duration::from_millis(20));

        let mut backing = TcpStreamBacking { stream: accepted };
        // RST produces either ConnectionReset or Ok(0) EOF — either way None.
        let result = backing.next();
        assert!(
            result.is_none(),
            "backing.next() must return None on RST/EOF, got {result:?}"
        );
    }

    // ── hew_tcp_stream_from_conn factory tests ───────────────────────────

    /// Register a real TCP connection via the runtime's own connect/accept
    /// path so `TCP_API_STATE` holds the handle, then call the factory.
    #[cfg(not(target_arch = "wasm32"))]
    fn make_loopback_conn() -> (c_int, std::net::TcpStream) {
        use std::net::TcpListener;

        // Bind a listener on an OS-assigned port.
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        let addr = CString::new(format!("127.0.0.1:{port}")).unwrap();

        // hew_tcp_connect registers the client-side conn in TCP_API_STATE.
        // SAFETY: addr is a valid NUL-terminated C string.
        let conn_handle = unsafe { crate::transport::hew_tcp_connect(addr.as_ptr()) };
        assert!(conn_handle > 0, "hew_tcp_connect failed");

        let (peer, _) = listener.accept().unwrap();
        (conn_handle, peer)
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn factory_returns_pair_for_valid_conn() {
        let (conn_handle, _peer) = make_loopback_conn();

        // SAFETY: conn_handle is a valid registered connection.
        let pair = unsafe { hew_tcp_stream_from_conn(conn_handle) };
        assert!(
            !pair.is_null(),
            "factory must return non-null for valid conn"
        );

        // Both halves must be extractable.
        // SAFETY: pair is valid.
        let stream_ptr = unsafe { hew_stream_pair_stream_bytes(pair) };
        assert!(!stream_ptr.is_null());

        // SAFETY: pair still valid (stream extraction nulls the stream slot, sink is still there).
        let sink_ptr = unsafe { hew_stream_pair_sink_bytes(pair) };
        assert!(!sink_ptr.is_null());

        // SAFETY: cleanup.
        unsafe {
            hew_stream_close(stream_ptr);
            drop(Box::from_raw(sink_ptr)); // ALLOCATOR-PAIRING: GlobalAlloc
            hew_stream_pair_free(pair);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn factory_null_for_invalid_conn() {
        use hew_cabi::sink::hew_stream_last_errno;

        // Clear any prior errno.
        let _ = hew_stream_last_errno();

        // SAFETY: -1 is never a valid registered handle.
        let pair = unsafe { hew_tcp_stream_from_conn(-1) };
        assert!(
            pair.is_null(),
            "factory must return null for invalid handle"
        );
        assert_eq!(
            hew_stream_last_errno(),
            9, // EBADF
            "factory must set EBADF errno for invalid handle"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn factory_consumes_original_conn_handle() {
        let (conn_handle, _peer) = make_loopback_conn();

        // SAFETY: conn_handle is valid.
        let pair = unsafe { hew_tcp_stream_from_conn(conn_handle) };
        assert!(!pair.is_null());

        // After the factory consumes the handle, a second call with the same
        // handle must return null (the entry is gone from TCP_API_STATE).
        // SAFETY: conn_handle is no longer valid (consumed above).
        let pair2 = unsafe { hew_tcp_stream_from_conn(conn_handle) };
        assert!(
            pair2.is_null(),
            "factory must return null when conn handle is already consumed"
        );

        // SAFETY: cleanup pair1.
        unsafe { hew_stream_pair_free(pair) };
    }

    // ── String recv (hew_stream_next / hew_stream_pop_string) ──────────────

    // ── Layout-witness element path (generic Stream<T> width) ───────────────

    use crate::vec::{HewTypeOwnershipKind, HewVecElemLayout};

    fn plain_elem_layout(size: usize, align: usize) -> HewVecElemLayout {
        HewVecElemLayout {
            size,
            align,
            ownership_kind: HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
        }
    }

    fn string_elem_layout() -> HewVecElemLayout {
        HewVecElemLayout {
            size: size_of::<*const c_char>(),
            align: align_of::<*const c_char>(),
            ownership_kind: HewTypeOwnershipKind::String,
            clone_fn: None,
            drop_fn: None,
        }
    }

    fn bytes_elem_layout() -> HewVecElemLayout {
        HewVecElemLayout {
            size: size_of::<crate::bytes::BytesTriple>(),
            align: align_of::<crate::bytes::BytesTriple>(),
            ownership_kind: HewTypeOwnershipKind::Bytes,
            clone_fn: None,
            drop_fn: None,
        }
    }

    /// String elements ride the witness path content-encoded: the sink reads
    /// the caller's string slot, the stream materialises a fresh header-aware
    /// cstring. An empty string survives as `Some("")` (rc 1).
    #[test]
    fn layout_stream_string_roundtrip_preserves_empty() {
        // SAFETY: hew_stream_channel returns a valid pair; slots are locals.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink = hew_stream_pair_sink(pair);
            let stream = hew_stream_pair_stream(pair);

            let layout = string_elem_layout();
            let alpha: *const c_char = c"alpha".as_ptr();
            hew_stream_send_layout(sink, std::ptr::addr_of!(alpha).cast(), &raw const layout);
            let empty: *const c_char = c"".as_ptr();
            hew_stream_send_layout(sink, std::ptr::addr_of!(empty).cast(), &raw const layout);
            hew_sink_close(sink);

            let mut out: *mut c_char = ptr::null_mut();
            let rc = hew_stream_next_layout(
                stream,
                std::ptr::addr_of_mut!(out).cast(),
                &raw const layout,
            );
            assert_eq!(rc, 1);
            assert_eq!(CStr::from_ptr(out).to_bytes(), b"alpha");
            crate::cabi::free_cstring(out);

            let rc = hew_stream_next_layout(
                stream,
                std::ptr::addr_of_mut!(out).cast(),
                &raw const layout,
            );
            assert_eq!(rc, 1, "empty string element is Some(\"\"), not None");
            assert_eq!(CStr::from_ptr(out).to_bytes(), b"");
            crate::cabi::free_cstring(out);

            let rc = hew_stream_next_layout(
                stream,
                std::ptr::addr_of_mut!(out).cast(),
                &raw const layout,
            );
            assert_eq!(rc, 0, "EOF binds no value");

            hew_stream_close(stream);
            hew_stream_pair_free(pair);
        }
    }

    /// Bytes elements: the envelope is the content bytes; decode materialises
    /// an owned `BytesTriple`. A present zero-length item keeps the documented
    /// EOF narrowing (rc 0), matching `hew_stream_next_bytes`.
    #[test]
    fn layout_stream_bytes_roundtrip_with_empty_narrowing() {
        // SAFETY: hew_stream_channel returns a valid pair; slots are locals.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink = hew_stream_pair_sink(pair);
            let stream = hew_stream_pair_stream(pair);

            let layout = bytes_elem_layout();
            let payload = b"\x01\x02\x03";
            let value = crate::bytes::hew_bytes_from_static(payload.as_ptr(), 3);
            hew_stream_send_layout(sink, std::ptr::addr_of!(value).cast(), &raw const layout);
            // The caller keeps its bytes value; release it independently.
            crate::bytes::hew_bytes_drop(value.ptr);

            let empty = crate::bytes::BytesTriple {
                ptr: ptr::null_mut(),
                offset: 0,
                len: 0,
            };
            hew_stream_send_layout(sink, std::ptr::addr_of!(empty).cast(), &raw const layout);
            hew_sink_close(sink);

            let mut out = crate::bytes::BytesTriple {
                ptr: ptr::null_mut(),
                offset: 0,
                len: 0,
            };
            let rc = hew_stream_pop_layout(
                stream,
                std::ptr::addr_of_mut!(out).cast(),
                &raw const layout,
            );
            assert_eq!(rc, 1);
            assert_eq!(out.len, 3);
            let got = std::slice::from_raw_parts(out.ptr.add(out.offset as usize), 3);
            assert_eq!(got, payload);
            crate::bytes::hew_bytes_drop(out.ptr);

            let rc = hew_stream_pop_layout(
                stream,
                std::ptr::addr_of_mut!(out).cast(),
                &raw const layout,
            );
            assert_eq!(
                rc, 0,
                "present zero-length bytes item keeps the EOF narrowing"
            );

            hew_stream_close(stream);
            hew_stream_pair_free(pair);
        }
    }

    /// Plain elements (f64 here) ride the witness path through a stream pipe;
    /// `try_next_layout` binds nothing on an empty-but-open pipe.
    #[test]
    fn layout_stream_plain_roundtrip_and_try_next_empty() {
        // SAFETY: hew_stream_channel returns a valid pair; slots are locals.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink = hew_stream_pair_sink(pair);
            let stream = hew_stream_pair_stream(pair);

            let layout = plain_elem_layout(8, 8);
            let mut out: f64 = 0.0;
            let rc = hew_stream_try_next_layout(
                stream,
                std::ptr::addr_of_mut!(out).cast(),
                &raw const layout,
            );
            assert_eq!(rc, 0, "empty pipe binds no value without blocking");

            let value: f64 = 2.5;
            hew_stream_send_layout(sink, std::ptr::addr_of!(value).cast(), &raw const layout);
            let rc = hew_stream_try_next_layout(
                stream,
                std::ptr::addr_of_mut!(out).cast(),
                &raw const layout,
            );
            assert_eq!(rc, 1);
            assert!((out - 2.5).abs() < f64::EPSILON);

            hew_sink_close(sink);
            hew_stream_close(stream);
            hew_stream_pair_free(pair);
        }
    }

    // Heap-owning element thunks for the stream-level round trip.
    static ST_OWNED_LOCK: Mutex<()> = Mutex::new(());
    static ST_OWNED_CLONES: AtomicUsize = AtomicUsize::new(0);
    static ST_OWNED_DROPS: AtomicUsize = AtomicUsize::new(0);

    #[repr(C)]
    struct StOwnedElem {
        tag: u64,
        heap: *mut u8,
    }

    unsafe extern "C" fn st_owned_clone(src: *const c_void, dst: *mut c_void) -> i32 {
        // SAFETY: thunk contract — src is a live element.
        let s = unsafe { &*src.cast::<StOwnedElem>() };
        // SAFETY: thunk contract — dst holds a writable memcpy of src.
        let d = unsafe { &mut *dst.cast::<StOwnedElem>() };
        // SAFETY: plain allocation; freed by st_owned_drop.
        let dup = unsafe { libc::malloc(8).cast::<u8>() };
        if !s.heap.is_null() {
            // SAFETY: both buffers are 8 bytes.
            unsafe { std::ptr::copy_nonoverlapping(s.heap, dup, 8) };
        }
        d.heap = dup;
        ST_OWNED_CLONES.fetch_add(1, Ordering::SeqCst);
        0
    }

    unsafe extern "C" fn st_owned_drop(slot: *mut c_void) {
        // SAFETY: thunk contract — slot is a live element being released.
        let e = unsafe { &mut *slot.cast::<StOwnedElem>() };
        if !e.heap.is_null() {
            // SAFETY: heap was malloc'd by st_owned_clone / the test body.
            unsafe { libc::free(e.heap.cast()) };
            e.heap = ptr::null_mut();
        }
        ST_OWNED_DROPS.fetch_add(1, Ordering::SeqCst);
    }

    fn st_owned_layout() -> HewVecElemLayout {
        HewVecElemLayout {
            size: size_of::<StOwnedElem>(),
            align: align_of::<StOwnedElem>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
            clone_fn: Some(st_owned_clone),
            drop_fn: Some(st_owned_drop),
        }
    }

    /// Heap-owning element through a typed stream pipe: clone on send, move on
    /// recv, and stream close with an unconsumed item releases it exactly once.
    #[test]
    fn layout_stream_owned_roundtrip_and_close_releases_queued() {
        let _g = ST_OWNED_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let clones_before = ST_OWNED_CLONES.load(Ordering::SeqCst);
        let drops_before = ST_OWNED_DROPS.load(Ordering::SeqCst);
        // SAFETY: hew_stream_channel returns a valid pair; slots are locals.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink = hew_stream_pair_sink(pair);
            let stream = hew_stream_pair_stream(pair);

            let layout = st_owned_layout();
            for tag in [21u64, 22u64] {
                let heap = libc::malloc(8).cast::<u8>();
                let value = StOwnedElem { tag, heap };
                hew_stream_send_layout(sink, std::ptr::addr_of!(value).cast(), &raw const layout);
                libc::free(value.heap.cast());
            }
            assert_eq!(ST_OWNED_CLONES.load(Ordering::SeqCst) - clones_before, 2);

            let mut out = StOwnedElem {
                tag: 0,
                heap: ptr::null_mut(),
            };
            let rc = hew_stream_pop_layout(
                stream,
                std::ptr::addr_of_mut!(out).cast(),
                &raw const layout,
            );
            assert_eq!(rc, 1);
            assert_eq!(out.tag, 21);
            assert!(!out.heap.is_null());
            assert_eq!(
                ST_OWNED_DROPS.load(Ordering::SeqCst) - drops_before,
                0,
                "pop transfers ownership — the queue must not drop"
            );
            // The consumer owns the element; release it exactly once.
            st_owned_drop(std::ptr::addr_of_mut!(out).cast());

            // One element (tag 22) is still queued: tearing the pipe down must
            // release it exactly once via the stamped witness.
            hew_sink_close(sink);
            hew_stream_close(stream);
            hew_stream_pair_free(pair);
        }
        assert_eq!(
            ST_OWNED_DROPS.load(Ordering::SeqCst) - drops_before,
            2,
            "consumed element dropped by the consumer; queued element by teardown"
        );
    }
}
