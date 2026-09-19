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
//! managed string allocations (for the string-element entries) or
//! malloc-allocated byte buffers (for the bytes-element / raw-byte entries):
//!
//! - `hew_stream_next_layout` / `hew_stream_try_next_layout` /
//!   `hew_stream_pop_layout` are the recv entries: an element-layout witness
//!   selects the envelope decode (string elements materialise a refcount-1,
//!   managed `*mut HewString` via
//!   [`crate::channel_common::decode_elem_envelope`] the MIR drop spine releases
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

use hew_cabi::string::{
    string_as_bytes, string_as_str, string_from_utf8, string_release, HewString,
};
use std::collections::VecDeque;
use std::ffi::{c_int, c_void};
use std::fs;
use std::io::{BufReader, Read, Write};
use std::net::TcpStream;
use std::ptr;
use std::sync::Arc;

// ── Re-export sink types from hew-cabi ────────────────────────────────────────
// These are the shared ABI types that native packages (e.g. HTTP) also use.
// Defining them in hew-cabi avoids pulling the full runtime into stdlib packages.

pub use crate::stream_error::{
    hew_stream_has_error, hew_stream_last_error, io_error_kind_tag, set_last_error,
    set_last_error_with_errno, set_last_error_with_errno_and_kind, take_last_error,
};
pub use hew_cabi::sink::{into_sink_ptr, into_write_sink_ptr, HewSink, TrySendResult};

// hew_stream_last_error / hew_stream_last_errno are defined in crate::stream_error
// (the single owner of the C ABI); hew-cabi only declares them as imports so that
// native packages resolve them against libhew.a at final link.

pub(crate) mod native;

/// Returns 1 if the stream pointer is non-null (valid), 0 otherwise.
#[no_mangle]
pub extern "C" fn hew_stream_is_valid(stream: *const HewStream) -> i32 {
    i32::from(!stream.is_null())
}

/// Nominally typed validity probe for the `std.fs.FileReadStream` adapter.
#[no_mangle]
pub extern "C" fn hew_file_read_stream_is_valid(stream: *const HewStream) -> i32 {
    hew_stream_is_valid(stream)
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
    /// The backing owns this transport handle; native operations only borrow it.
    fn native_connection(&self) -> Option<i32> {
        None
    }

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
    /// The suspending channel core (NEW-7) when this stream is the read half of
    /// an in-memory pipe; `None` for every other backing. Shared by `Arc` with
    /// the paired sink so `await stream.recv()` can park + be woken by the
    /// producer's `await sink.send()`. Non-channel backings keep the blocking
    /// read path (no parkable producer to wake).
    channel: Option<Arc<crate::channel_core::ChannelCore>>,
}

impl HewStream {
    /// The shared pipe core when this stream is the read half of an
    /// in-memory pipe; `None` for a content stream (socket, file, adapter).
    #[must_use]
    pub(crate) fn pipe_core(&self) -> Option<&Arc<crate::channel_core::ChannelCore>> {
        self.channel.as_ref()
    }
}

impl Drop for HewStream {
    fn drop(&mut self) {
        if !self.closed {
            self.inner.close();
        }
    }
}

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
            Ok(_) => None,
            Err(error) => {
                set_last_error_with_errno_and_kind(
                    format!("file stream read failed: {error}"),
                    error.raw_os_error().unwrap_or(0),
                    io_error_kind_tag(error.kind()),
                );
                None
            }
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
// Thread-safety: the struct is `Send` (TcpStream is Send). It is NOT `Sync`;
// at most one reader exists at any point.
//
// Blocking behaviour: `next` blocks until data arrives, the peer closes,
// or the OS returns an error.  If the caller wants a deadline, they call
// `conn.set_read_timeout(ms)` on the `Connection` before calling
// `into_stream_sink` — the timeout is inherited by the clone.

#[cfg(not(target_arch = "wasm32"))]
#[derive(Debug)]
struct TcpStreamBacking {
    connection: i32,
}

#[cfg(not(target_arch = "wasm32"))]
impl TcpStreamBacking {
    fn new(stream: TcpStream) -> Self {
        Self {
            connection: crate::transport::tcp_register_owned_stream(stream),
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl Drop for TcpStreamBacking {
    fn drop(&mut self) {
        crate::transport::tcp_release_conn(self.connection);
    }
}

/// TCP read backing size, matching `hew_tcp_read`'s buffer (transport.rs:1216).
#[cfg(not(target_arch = "wasm32"))]
const TCP_BACKING_BUF_SIZE: usize = 8192;

#[cfg(not(target_arch = "wasm32"))]
impl StreamBacking for TcpStreamBacking {
    fn native_connection(&self) -> Option<i32> {
        Some(self.connection)
    }

    fn next(&mut self) -> Option<Item> {
        if crate::runtime::rt_current_opt().is_some() {
            return native::blocking_tcp_read(self.connection);
        }
        let mut stream = crate::transport::tcp_clone_stream(self.connection)?;
        let mut buf = [0u8; TCP_BACKING_BUF_SIZE];
        loop {
            match stream.read(&mut buf) {
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
        // Dropping this backing releases its transport entry and socket clone.
        // Closing the readable half does not shut down the paired write half.
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

            // Check if there's a complete line already buffered — but only
            // take this fast path if the line (up to and including its
            // delimiter) fits within the cap.  A single chunk that is
            // already over MAX_LINE_BUFFER_SIZE *and* happens to carry its
            // own trailing '\n' must still be capped: otherwise the
            // size-limit check below (which only fires when no newline is
            // found) is bypassed entirely and an arbitrarily large line can
            // be returned whole, defeating the point of the cap.
            if let Some(pos) = self.buf.iter().position(|&b| b == b'\n') {
                // `pos` is the index of the delimiter itself, not the
                // content length: for a CRLF-terminated line, the '\r' just
                // before it is a delimiter byte, not content. When pos ==
                // MAX_LINE_BUFFER_SIZE and that preceding byte is '\r', the
                // actual content (everything before the '\r') still fits
                // within the cap, so this must still take the fast path —
                // otherwise the '\r' gets misclassified as content and
                // leaked into the returned line by the size-limit drain
                // below.
                if pos < MAX_LINE_BUFFER_SIZE
                    || (pos == MAX_LINE_BUFFER_SIZE && self.buf[pos - 1] == b'\r')
                {
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
                // Newline exists but only past the cap — fall through to the
                // size-limit drain below so the returned line is bounded.
                // The '\n' (and anything before it) stays in `buf` and is
                // consumed by the `skip_next_delimiter` handling above on a
                // later call, exactly as for the no-newline overflow case.
            }
            // Buffer full without a (fits-in-cap) newline — drain exactly the
            // limit to bound memory.  Leftover bytes stay in buf for the next
            // call.
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
    // resources. `closed` is POD, but going through ptr::read+drop is uniform
    // and cheap; `channel` owns an Arc and must run Drop.
    unsafe {
        let _ = ptr::read(&raw const (*stream).closed);
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
    if capacity < 0 {
        set_last_error(format!(
            "hew_stream_channel: invalid capacity {capacity} (must be >= 0)"
        ));
        return ptr::null_mut();
    }
    let Ok(cap) = usize::try_from(capacity.max(1)) else {
        set_last_error(format!(
            "hew_stream_channel: capacity {capacity} exceeds platform maximum"
        ));
        return ptr::null_mut();
    };
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
    // borrow lets a suspending write reach the queue + parked consumer.
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

/// Create one bounded element pipe for a checked stream producer request.
/// Elements move as raw bytes of `elem_size`; `elem_drop` releases whichever
/// elements are still queued when the pipe ends.
///
/// # Safety
/// `sink_out` is a writable slot that receives the owned sink half.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_pipe_native(
    capacity: i64,
    layout: *const hew_cabi::vec::HewValueLayout,
    sink_out: *mut *mut HewSink,
) -> *mut HewStream {
    // SAFETY: the pair constructor accepts any capacity.
    let pair = unsafe { hew_stream_channel(capacity) };
    if pair.is_null() {
        std::process::abort();
    }
    // SAFETY: the pair was just allocated by `hew_stream_channel`.
    let mut pair = unsafe { Box::from_raw(pair) };
    // SAFETY: the compiler lends a static descriptor for the owned element.
    let layout = unsafe {
        crate::channel_common::move_elem_layout_witness(layout, "hew_stream_pipe_native")
    };
    // SAFETY: both halves are live and share the core this pair created.
    unsafe {
        if let Some(core) = (*pair.stream).channel.as_ref() {
            core.stamp_elem_layout(layout);
        }
        *sink_out = pair.sink;
    }
    let stream = pair.stream;
    // The halves now belong to the caller; the pair drop frees only itself.
    pair.sink = std::ptr::null_mut();
    pair.stream = std::ptr::null_mut();
    drop(pair);
    stream
}

/// Return whether `pair` is a valid stream-pair handle.
#[no_mangle]
pub const extern "C" fn hew_stream_pair_is_valid(pair: *const HewStreamPair) -> bool {
    !pair.is_null()
}

/// Channel-sink callback: blocking write (default callers). Suspending callers
/// route through the channel core's own send instead.
fn channel_sink_write(core: &mut Arc<crate::channel_core::ChannelCore>, data: &[u8]) {
    core.blocking_send(data.to_vec());
}

/// Channel-sink callback: flush is a no-op (the core is not write-buffered).
fn channel_sink_flush(_core: &mut Arc<crate::channel_core::ChannelCore>) {}

/// Channel-sink callback: one producer handle finished or closed. The last
/// handle publishes EOF and wakes a parked consumer so its `recv()` binds
/// `None`. A handle released by a crashing actor faults the pipe instead:
/// the consumer traps on its next read rather than reading a clean end.
fn channel_sink_close(core: &mut Arc<crate::channel_core::ChannelCore>) {
    match crate::fault::crashing_owner() {
        Some(actor_id) => core.fault_close(actor_id),
        None => core.close_sink(),
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn tcp_sink_write(backing: &mut TcpStreamBacking, data: &[u8]) {
    if crate::runtime::rt_current_opt().is_some() {
        native::blocking_tcp_write(backing.connection, data);
    } else if let Some(mut stream) = crate::transport::tcp_clone_stream(backing.connection) {
        if let Err(error) = stream.write_all(data) {
            set_last_error(format!("TCP sink write failed: {error}"));
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn tcp_sink_flush(_backing: &mut TcpStreamBacking) {}

#[cfg(not(target_arch = "wasm32"))]
fn tcp_sink_close(backing: &mut TcpStreamBacking) {
    if let Some(stream) = crate::transport::tcp_clone_stream(backing.connection) {
        if let Err(error) = stream.shutdown(std::net::Shutdown::Write) {
            set_last_error(format!("TCP sink shutdown failed: {error}"));
        }
    }
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
/// stream and sink with `hew_stream_pair_stream` / `hew_stream_pair_sink`,
/// then free with `hew_stream_pair_free`.
///
/// The `conn` handle is **consumed on every return path**: success *and*
/// clone failure.  On either clone failure the original connection is fully
/// closed and released from the table before the null pair is returned — the
/// source-level `Connection` is dead-by-move regardless, so the runtime is the
/// sole releaser and no fd/table slot can leak (#2650).  The last-error errno
/// distinguishes the cause: **EBADF (9)** when `conn` is not a registered
/// handle, otherwise the **real `dup(2)` errno** (e.g. `EMFILE`/`ENFILE` under
/// fd pressure) so a resource-exhaustion failure is not mislabeled.
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
/// See `WASM-TODO(tcp-networking):`. The `wasm32` stub returns `null` so the symbol
/// resolves at link time, but every call returns `null` without side-effects.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_tcp_stream_from_conn(conn: c_int) -> *mut HewStreamPair {
    use crate::transport::{
        tcp_clone_stream_outcome, tcp_full_close_conn, tcp_release_conn, CloneOutcome,
    };

    // Clone the read fd.
    let read_stream = match tcp_clone_stream_outcome(conn) {
        CloneOutcome::Cloned(stream) => stream,
        CloneOutcome::NoEntry => {
            // Genuinely unknown handle: nothing is registered, so there is
            // nothing to release. Report EBADF (the real "invalid handle").
            set_last_error_with_errno(
                format!("hew_tcp_stream_from_conn: invalid connection handle {conn}"),
                9, // EBADF
            );
            return ptr::null_mut();
        }
        CloneOutcome::Failed(err) => {
            // Valid handle, but the clone/dup(2) failed (e.g. EMFILE/ENFILE
            // under fd pressure). Consumed-on-call contract: `conn` is
            // dead-by-move at the source level on every return path, so the
            // runtime is its sole releaser. No clone survives here — fully
            // close and release the original entry (removes the table slot and
            // closes the fd) before returning the null pair. Surface the *real*
            // errno so the failure is distinguishable from a bad handle.
            tcp_full_close_conn(conn);
            let errno = err.raw_os_error().unwrap_or(libc::EIO);
            set_last_error_with_errno_and_kind(
                format!(
                    "hew_tcp_stream_from_conn: could not clone read fd for handle {conn}: {err}"
                ),
                errno,
                io_error_kind_tag(err.kind()),
            );
            return ptr::null_mut();
        }
    };

    // Clone the write fd.
    let write_stream = match tcp_clone_stream_outcome(conn) {
        CloneOutcome::Cloned(stream) => stream,
        outcome => {
            // `read_stream` is a live dup of the socket and RAII-drops on
            // return (closing its own dup fd). Full-close the original table
            // entry: a distinct fd, so no double-close. Both fds are released
            // and the table slot is freed — the consumed connection is not
            // stranded.
            tcp_full_close_conn(conn);
            match outcome {
                CloneOutcome::Failed(err) => {
                    let errno = err.raw_os_error().unwrap_or(libc::EIO);
                    set_last_error_with_errno_and_kind(
                        format!(
                            "hew_tcp_stream_from_conn: could not clone write fd for handle {conn}: {err}"
                        ),
                        errno,
                        io_error_kind_tag(err.kind()),
                    );
                }
                // NoEntry after a successful first clone means the entry was
                // removed out from under us (not reachable on the single-owner
                // bridge path); report EBADF for completeness.
                _ => {
                    set_last_error_with_errno(
                        format!(
                            "hew_tcp_stream_from_conn: could not clone write fd for handle {conn}"
                        ),
                        9, // EBADF
                    );
                }
            }
            return ptr::null_mut();
        }
    };

    // Suspended listener accepts leave their accepted socket non-blocking
    // because the reactor must never park. The Stream backing, however, owns a
    // blocking `recv()` contract: a transient WouldBlock is not EOF. Restore
    // blocking mode after both clones exist; duplicated descriptors share the
    // underlying file status flags, so this covers both halves before the
    // original table entry is released.
    if let Err(error) = read_stream.set_nonblocking(false) {
        tcp_full_close_conn(conn);
        set_last_error_with_errno_and_kind(
            format!("hew_tcp_stream_from_conn: could not restore blocking mode: {error}"),
            error.raw_os_error().unwrap_or(libc::EIO),
            io_error_kind_tag(error.kind()),
        );
        return ptr::null_mut();
    }

    // Release the original handle from the connection table WITHOUT calling
    // shutdown. TcpStream clones refer to the same underlying socket;
    // calling shutdown on any clone shuts down that socket, which would
    // immediately invalidate the two backings we just created.
    // `tcp_release_conn` only removes the table entry — the two clones keep
    // the socket alive.
    tcp_release_conn(conn);

    // Build the stream (read) half via the canonical helper.
    let stream_ptr = into_stream_ptr(TcpStreamBacking::new(read_stream));

    // Build the sink (write) half with a TCP-specific close callback. Closing
    // one duplicated descriptor is not enough to publish FIN while the read
    // half still owns another clone; `shutdown(Write)` makes eager sink closure
    // observable to the peer without disturbing the live read half.
    let write_backing = TcpStreamBacking::new(write_stream);
    let write_connection = write_backing.connection;
    let sink_ptr = into_sink_ptr(
        write_backing,
        tcp_sink_write,
        tcp_sink_flush,
        tcp_sink_close,
    );
    // SAFETY: the new sink backing owns this handle until close/drop.
    unsafe { (*sink_ptr).set_native_connection(write_connection) };

    Box::into_raw(Box::new(HewStreamPair {
        // ALLOCATOR-PAIRING: GlobalAlloc
        sink: sink_ptr,
        stream: stream_ptr,
    }))
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

/// Open a file for streaming reads.
///
/// Returns a `*mut HewStream` that yields the file contents in 4096-byte
/// chunks, or null on error.
///
/// # Safety
///
/// `path` must be a live managed string, or null for an empty path.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_from_file_read(path: *const HewString) -> *mut HewStream {
    let _ = take_last_error();
    // SAFETY: Caller guarantees path is a live managed string.
    let path_str = unsafe { string_as_str(path) };
    if path_str.contains('\0') {
        set_last_error("path contains interior NUL".into());
        return ptr::null_mut();
    }
    match fs::File::open(path_str) {
        Ok(f) => into_stream_ptr(FileReadStream {
            reader: BufReader::new(f),
            chunk_size: 4096,
        }),
        Err(e) => {
            set_last_error_with_errno_and_kind(
                format!("{e}"),
                e.raw_os_error().unwrap_or(0),
                io_error_kind_tag(e.kind()),
            );
            ptr::null_mut()
        }
    }
}

/// Nominally typed file-read handle constructor used by `std.fs`.
///
/// The generic stream API retains `hew_stream_from_file_read`; this distinct
/// endpoint gives generated ownership contracts one unambiguous source type.
///
/// # Safety
///
/// Same preconditions as [`hew_stream_from_file_read`].
#[no_mangle]
pub unsafe extern "C" fn hew_file_read_stream_open(path: *const HewString) -> *mut HewStream {
    // SAFETY: This nominal adapter has exactly the delegated ABI and preconditions.
    unsafe { hew_stream_from_file_read(path) }
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
        // buf_try_alloc returns a valid, aligned pointer or null.
        let buf = crate::mem::buf_try_alloc(alloc_len); // ALLOCATOR-PAIRING: GlobalAlloc  // CSTRING-ALLOC: sized-block (hew_stream_next_sized item buffer)
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
/// grown via `buf_realloc`, and both `*buf` and `*buf_cap` are updated so
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
        let new_ptr = unsafe { crate::mem::buf_realloc((*buf).cast::<c_void>(), len) };
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

// ── Deadline-cancel cleanup callback (NEW-6b) ───────────────────────────────

/// Per-suspend cancel context for `await stream.recv()`: holds both the read
/// slot and the stream handle so the cleanup callback can cancel + detach.
///
/// Allocated as an alloca in the coroutine frame (codegen side) so its lifetime
/// spans the coro.suspend — the spilling pass moves it into the frame object.
#[repr(C)]
#[allow(
    dead_code,
    reason = "fields accessed via FFI from codegen-emitted LLVM IR, not from Rust"
)]
pub struct HewStreamRecvCancelCtx {
    /// The `HewReadSlot` this recv registered against.
    pub slot: *mut crate::read_slot::HewReadSlot,
    /// The `HewStream` handle the recv is registered against.
    pub stream: *mut HewStream,
}

impl std::fmt::Debug for HewStreamRecvCancelCtx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewStreamRecvCancelCtx")
            .field("slot", &self.slot)
            .field("stream", &self.stream)
            .finish()
    }
}

/// Await-cancel cleanup callback for a suspending `await stream.recv()`.
///
/// Fires when the deadline timer wins the one-shot CAS (`TimedOut`) or an
/// explicit cancel wins (`Cancelled`).  Cancels the read slot and detaches the
/// stream core's in-flight consumer reference so the core never tries to wake
/// a freed slot.
///
/// # Safety
///
/// `source` must be a `*mut HewStreamRecvCancelCtx` allocated in the caller's
/// coroutine frame.  The underlying slot and stream must still be alive.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_recv_cancel_cleanup(
    source: *mut std::ffi::c_void,
    _status: i32,
) {
    let ctx = source.cast::<HewStreamRecvCancelCtx>();
    if ctx.is_null() {
        return;
    }
    // SAFETY: ctx is a valid frame-alloca; slot and stream pointers are alive.
    let slot = unsafe { (*ctx).slot };
    // SAFETY: same frame-alloca guarantee as slot above.
    let stream = unsafe { (*ctx).stream };
    if !slot.is_null() {
        // SAFETY: slot is alive (frame alloca).
        unsafe { crate::read_slot::hew_read_slot_cancel(slot) };
    }
    if !stream.is_null() {
        // SAFETY: stream is alive and core still holds a reference to the slot.
        unsafe { hew_stream_detach_await(stream, slot) };
    }
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

/// Close read admission and consume queued owners before releasing the handle.
/// # Safety
/// `stream` is null or uniquely owned; no operation may retain a loan to it.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_release_begin(
    stream: *mut HewStream,
) -> *mut crate::release_walker::HewReleaseCursor {
    use crate::release_walker::{HewReleaseCursor, ReleaseItem};
    unsafe fn free_stream(owner: *mut c_void) {
        // SAFETY: the cursor owns the handle and has drained all queued values.
        unsafe { hew_stream_close(owner.cast()) };
    }
    // SAFETY: the caller supplies an exclusive live handle or null.
    let (layout, discarded) = unsafe { stream.as_ref() }
        .and_then(|stream| stream.channel.as_ref())
        .map_or_else(|| (None, Vec::new()), |core| core.close_stream_take());
    HewReleaseCursor::envelopes(
        discarded,
        layout,
        ReleaseItem::Storage {
            owner: stream.cast(),
            free: free_stream,
        },
    )
}

/// Nominally typed file-read handle release used by `std.fs`.
///
/// # Safety
///
/// Same preconditions as [`hew_stream_close`].
#[no_mangle]
pub unsafe extern "C" fn hew_file_read_stream_close(stream: *mut HewStream) {
    // SAFETY: This nominal adapter has exactly the delegated ABI and preconditions.
    unsafe { hew_stream_close(stream) };
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

/// Consume a sink and any unaccepted owners discarded by its producer fault.
/// # Safety
/// `sink` is null or uniquely owned; no operation may retain a loan to it.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_release_begin(
    sink: *mut HewSink,
    state: *const crate::coro_state::HewCoroState,
) -> *mut crate::release_walker::HewReleaseCursor {
    use crate::release_walker::{HewReleaseCursor, ReleaseItem};
    unsafe fn free_sink(owner: *mut c_void) {
        // SAFETY: the cursor owns this handle after discarded values finish.
        unsafe { hew_sink_close(owner.cast()) };
    }
    // SAFETY: the consuming callback retains its invocation through this call.
    let owner = unsafe { crate::coro_state::cleanup_fault_owner(state) }
        .or_else(crate::fault::crashing_owner);
    let (layout, discarded) = if let Some(actor) = owner {
        // SAFETY: a live channel sink retains its core through cursor completion.
        unsafe { sink_channel_core(sink) }
            .map_or_else(|| (None, Vec::new()), |core| core.fault_close_take(actor))
    } else {
        (None, Vec::new())
    };
    HewReleaseCursor::envelopes(
        discarded,
        layout,
        ReleaseItem::Storage {
            owner: sink.cast(),
            free: free_sink,
        },
    )
}

/// `Sink.finish`: publish EOF to the reader and keep the handle.
///
/// The half-close verb. A pipe sink retires its producer handle (the last
/// one publishes EOF); a TCP sink sends FIN with `shutdown(Write)` while
/// the read half stays live. The later owning drop remains a no-op because
/// [`HewSink::close`] takes the backing exactly once.
///
/// # Safety
///
/// `sink` must be a valid pointer created by a stream sink constructor.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_finish(sink: *mut HewSink) {
    if !sink.is_null() {
        // SAFETY: sink is valid per caller contract.
        unsafe { (*sink).close() };
    }
}

/// `Sink.clone`: one more producer handle on the same pipe. The pipe reaches
/// EOF when the last handle finishes or closes.
///
/// Only a pipe sink has a shareable queue behind it. A socket or file sink has
/// one writer; cloning one is refused fail-closed.
///
/// # Safety
///
/// `sink` must be a valid pointer created by a stream sink constructor.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_clone(sink: *mut HewSink) -> *mut HewSink {
    cabi_guard!(sink.is_null(), ptr::null_mut());
    // SAFETY: sink is valid per caller contract.
    let core_raw = unsafe { (*sink).channel_core_ptr() };
    if core_raw.is_null() {
        crate::channel_common::abort_elem_witness(
            "hew_sink_clone",
            "only a pipe sink can be cloned; a socket or file sink has one writer",
        );
    }
    let core_raw = core_raw.cast::<crate::channel_core::ChannelCore>();
    // SAFETY: the sink's backing owns the Arc this pointer borrows; the
    // increment mints the clone's independent owner.
    unsafe { Arc::increment_strong_count(core_raw) };
    // SAFETY: the increment above balances this reconstruction.
    let core = unsafe { Arc::from_raw(core_raw) };
    core.clone_sink();
    let clone = into_sink_ptr(
        core,
        channel_sink_write,
        channel_sink_flush,
        channel_sink_close,
    );
    // SAFETY: clone was just allocated by into_sink_ptr.
    unsafe { (*clone).set_channel_core(core_raw.cast::<c_void>()) };
    clone
}

/// Whether the sink's peer (the consumer / `Stream<T>` half) has closed or
/// detached. Read by a `receive gen fn` pump before every resume (decision
/// 6): once the peer is gone, the pump breaks its loop WITHOUT
/// resuming the generator further, so an infinite generator with a consumer
/// `break` cannot livelock the actor.
///
/// Returns 1 if the peer has closed, 0 otherwise — including a null sink or
/// a non-channel sink (a `receive gen fn` pump only ever registers a
/// channel-backed sink; a "still connected" default is the fail-safe choice
/// for an unrecognised backing since it never falsely aborts a live pump).
///
/// # Safety
///
/// `sink` must be null or a valid `HewSink` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_peer_closed(sink: *mut HewSink) -> i32 {
    if sink.is_null() {
        return 0;
    }
    // SAFETY: sink is valid per caller contract.
    let core_raw = unsafe { (*sink).channel_core_ptr() };
    if core_raw.is_null() {
        return 0;
    }
    // SAFETY: core_raw borrows the live `Arc<ChannelCore>` owned by the sink
    // backing (alive for the duration of this call).
    let core = unsafe { &*core_raw.cast::<crate::channel_core::ChannelCore>() };
    i32::from(core.is_stream_closed())
}

/// Fault-close a receive-gen producer's abandoned sink: mark the
/// shared `ChannelCore` permanently faulted, wake any parked consumer so it
/// observes the fault immediately instead of hanging, then free the sink
/// allocation. Called only by the runtime's own actor-teardown paths
/// (`hew_actor_trap`'s crash handling, `hew_actor_free_inner`'s
/// parked-activation reclaim) against a sink `hew_actor_gen_sink_register`
/// recorded — never reachable from Hew source. A no-op on a null sink.
///
/// The pump's own generated body never reaches its `hew_actor_gen_sink_complete`
/// call on this path (the activation crashed or was torn down first), so this
/// function is the sink's ONLY release: it takes the same consuming,
/// free-the-allocation contract `hew_sink_close` has, just with the fault
/// stamped on the shared core FIRST (before the sink's `Drop` clears the
/// core borrow) so a consumer racing the teardown still observes the fault
/// rather than a bare clean close.
///
/// Does NOT touch the generator companion (heap env + coro handle) living as
/// a local inside the pump's own coroutine frame — that is released via the
/// pump's own coro `cleanup` outline when the frame is destroyed (normal
/// scope-exit drop, or `coro_exec::destroy_parked` for a mid-suspend
/// teardown). This function's sole job is the SINK, so the two release paths
/// never overlap.
///
/// # Safety
///
/// `sink` must be null or a live `HewSink` pointer not yet freed — the exact
/// pointer `hew_actor_gen_sink_register` recorded. After this call `sink` is
/// dangling (mirrors `hew_sink_close`'s contract).
pub(crate) unsafe fn fault_close_registered_sink(sink: *mut HewSink, faulted_actor_id: u64) {
    if sink.is_null() {
        return;
    }
    // SAFETY: sink is valid per caller contract.
    let core_raw = unsafe { (*sink).channel_core_ptr() };
    if !core_raw.is_null() {
        // SAFETY: core_raw borrows the live `Arc<ChannelCore>` owned by the
        // sink backing; stamp the fault BEFORE `hew_sink_close` below runs
        // `HewSink::close()`, which nulls this borrow.
        let core = unsafe { &*core_raw.cast::<crate::channel_core::ChannelCore>() };
        core.fault_close(faulted_actor_id);
    }
    // SAFETY: sink is the live, not-yet-freed pointer per the fn contract;
    // this is the sink's single release on the abandoned-pump path.
    unsafe { hew_sink_close(sink) };
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
/// Returns a new `HewStream*` that yields one line at a time, with the
/// newline removed.  Takes ownership of `stream` — do not use it after this
/// call.
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

/// Read all remaining items from a stream and concatenate them as a managed string.
///
/// Returns an owned managed string. The caller must free it with
/// `hew_string_drop`. Null can represent valid empty text or failure; inspect
/// `hew_stream_has_error` before consuming the error metadata. Read and UTF-8
/// failures never return partial text. Clears stale errors when collection starts.
/// Consumes the stream. A close failure prevents success; when collection has
/// already failed, the close diagnostic is appended without replacing the primary
/// error kind or OS code.
///
/// # Safety
///
/// `stream` must be a valid `HewStream` pointer or null.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_collect_string(stream: *mut HewStream) -> *mut HewString {
    let _ = take_last_error();
    if stream.is_null() {
        set_last_error("cannot collect a null stream".into());
        return ptr::null_mut();
    }

    // SAFETY: stream was allocated with Box::into_raw; we take ownership.
    let mut owned = unsafe { Box::from_raw(stream) }; // ALLOCATOR-PAIRING: GlobalAlloc
    let mut buffer = Vec::new();
    while let Some(chunk) = owned.inner.next() {
        if hew_stream_has_error() {
            break;
        }
        buffer.extend_from_slice(&chunk);
    }

    // A read failure takes precedence over decoding any partial contents.
    let result = if hew_stream_has_error() {
        ptr::null_mut()
    } else {
        match string_from_utf8(&buffer) {
            Ok(value) => value,
            Err(error) => {
                set_last_error(format!("stream string collection: {error}"));
                ptr::null_mut()
            }
        }
    };

    // Keep the collection outcome owned while releasing the stream, then
    // capture cleanup's outcome separately. Read kind before errno clears it.
    let kind = crate::stream_error::take_last_error_kind();
    let errno = crate::stream_error::take_last_errno();
    let error = take_last_error();
    drop(owned);
    let close_kind = crate::stream_error::take_last_error_kind();
    let close_errno = crate::stream_error::take_last_errno();
    let close_error = take_last_error();
    if let Some(mut error) = error {
        if let Some(secondary) = close_error {
            // Include a labelled diagnostic even when the close message is empty.
            error = format!(
                "{error}\nstream close failed (kind {close_kind}, errno {close_errno}): {secondary}"
            );
        }
        set_last_error_with_errno_and_kind(error, errno, kind);
    } else if let Some(error) = close_error {
        // SAFETY: this operation still owns the collected string, including null/empty.
        unsafe { string_release(result) };
        set_last_error_with_errno_and_kind(error, close_errno, close_kind);
        return ptr::null_mut();
    }
    result
}

/// Drain and consume a nominal `std.fs.FileReadStream` into one string.
///
/// This distinct endpoint prevents the same C symbol from carrying both the
/// generic `Stream<string>` and `FileReadStream` source signatures. The raw
/// representation and consuming runtime operation are identical.
///
/// # Safety
///
/// Same preconditions as [`hew_stream_collect_string`].
#[no_mangle]
pub unsafe extern "C" fn hew_file_read_stream_collect_string(
    stream: *mut HewStream,
) -> *mut HewString {
    // SAFETY: This nominal adapter has exactly the delegated ABI and preconditions.
    unsafe { hew_stream_collect_string(stream) }
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

/// Write a managed string to the sink.
///
/// # Safety
///
/// `sink` must be a valid pointer. `data` must be a live managed string.
#[no_mangle]
pub unsafe extern "C" fn hew_sink_write_string(sink: *mut HewSink, data: *const HewString) {
    cabi_guard!(sink.is_null());

    // SAFETY: data is a live managed string.
    let bytes = unsafe { string_as_bytes(data) };

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
/// (the uniform by-pointer bytes-param convention).
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
// through one mechanism: a `HewValueLayout` witness selects the envelope
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
/// `layout` must point to a valid `HewValueLayout` for the duration of the
/// call (in practice a codegen static).
#[no_mangle]
pub unsafe extern "C" fn hew_stream_send_layout(
    sink: *mut HewSink,
    data: *const c_void,
    layout: *const crate::vec::HewValueLayout,
) {
    cabi_guard!(sink.is_null());
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
    layout: *const crate::vec::HewValueLayout,
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
    layout: *const crate::vec::HewValueLayout,
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
    layout: *const crate::vec::HewValueLayout,
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

/// `Sink.try_send`: deposit one element of any witness-describable type
/// without waiting. Returns `0` (accepted), `1` (`SendError.Closed`: the sink
/// finished or the reader left) or `2` (`SendError.Full`: the pipe is at
/// capacity). An open content sink (file, socket) has no bounded queue to
/// observe, so its write completes in place and reports `0`.
///
/// # Safety
///
/// `sink` must be a valid pointer. `data` must point to one live element of
/// the witness's type; the caller keeps it. `layout` must be a valid witness
/// for the duration of the call.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_try_send_layout(
    sink: *mut HewSink,
    data: *const c_void,
    layout: *const crate::vec::HewValueLayout,
) -> i32 {
    if sink.is_null() {
        return TrySendResult::Closed.into_abi_code();
    }
    // SAFETY: layout validity is the caller's contract.
    let layout =
        unsafe { crate::channel_common::elem_layout_witness(layout, "hew_stream_try_send_layout") };
    // SAFETY: sink is valid per caller contract.
    let core = unsafe { sink_channel_core(sink) };
    // A finished sink answers `Closed` for every element type: its backing is
    // gone, so no element kind can require an in-memory channel of it.
    // SAFETY: sink is valid per caller contract.
    let closed = unsafe { (*sink).is_closed() };
    if !closed
        && core.is_none()
        && layout.ownership_kind == crate::vec::HewTypeOwnershipKind::LayoutManaged
    {
        crate::channel_common::abort_elem_witness(
            "hew_stream_try_send_layout",
            "layout-managed elements require an in-memory channel sink",
        );
    }
    // SAFETY: data points to one live element per caller contract.
    let env = unsafe {
        crate::channel_common::encode_elem_envelope(data, layout, "hew_stream_try_send_layout")
    };
    if closed {
        // The caller relinquished the element at the call, so the refusal
        // releases it here rather than leaking it with the answer.
        crate::channel_common::drop_elem_envelope(Some(layout), env, "hew_stream_try_send_layout");
        return TrySendResult::Closed.into_abi_code();
    }
    match core {
        Some(core) => {
            if layout.ownership_kind == crate::vec::HewTypeOwnershipKind::LayoutManaged {
                core.stamp_elem_layout(layout);
            }
            core.try_send(env).into_abi_code()
        }
        // SAFETY: sink is valid per caller contract.
        None => unsafe { (*sink).try_write_item(&env) }.into_abi_code(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_string::ManagedString;
    use std::ffi::CString;
    use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
    use std::sync::Mutex;

    /// Wrapper to send raw sink pointers across thread boundaries.
    ///
    /// # Safety
    ///
    /// The inner pointer must refer to a channel-backed sink (mpsc is thread-safe).
    struct SendSink(*mut HewSink);
    // SAFETY: channel sinks are backed by mpsc::SyncSender which is Send + Sync.
    unsafe impl Send for SendSink {}

    // ── Helpers ──────────────────────────────────────────────────────────

    /// Read all items from a stream via the sized FFI, freeing each sized-block buffer.
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
            // SAFETY: ptr was allocated by hew_stream_next_sized's sized-block allocation.
            unsafe { crate::mem::buf_free(ptr) }; // ALLOCATOR-PAIRING: GlobalAlloc
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
    fn channel_negative_capacity_returns_invalid_pair_with_exact_error() {
        let _ = hew_cabi::sink::take_last_error();
        // SAFETY: negative capacity is explicitly validated.
        let pair = unsafe { hew_stream_channel(-1) };
        assert!(!hew_stream_pair_is_valid(pair));
        assert_eq!(
            hew_cabi::sink::take_last_error().as_deref(),
            Some("hew_stream_channel: invalid capacity -1 (must be >= 0)")
        );
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
            crate::mem::buf_free(buf); // ALLOCATOR-PAIRING: GlobalAlloc
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
            let mut buf: *mut u8 = crate::mem::buf_try_alloc(64).cast::<u8>(); // ALLOCATOR-PAIRING: GlobalAlloc
            let mut cap: usize = 64;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, 5);
            assert_eq!(cap, 64, "capacity unchanged when buffer is large enough");
            let n = usize::try_from(ret).unwrap();
            let slice = std::slice::from_raw_parts(buf, n);
            assert_eq!(slice, b"hello");
            crate::mem::buf_free(buf.cast()); // ALLOCATOR-PAIRING: GlobalAlloc
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
            let mut buf: *mut u8 = crate::mem::buf_try_alloc(2).cast::<u8>(); // ALLOCATOR-PAIRING: GlobalAlloc
            let mut cap: usize = 2;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, i64::try_from(data.len()).unwrap());
            assert!(cap >= data.len(), "capacity must grow to fit the item");
            let n = usize::try_from(ret).unwrap();
            let slice = std::slice::from_raw_parts(buf, n);
            assert_eq!(slice, data);
            crate::mem::buf_free(buf.cast()); // ALLOCATOR-PAIRING: GlobalAlloc
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
            crate::mem::buf_free(buf.cast()); // ALLOCATOR-PAIRING: GlobalAlloc
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
            crate::mem::buf_free(buf.cast()); // ALLOCATOR-PAIRING: GlobalAlloc
            hew_stream_close(stream);
        }
    }

    #[test]
    fn stream_next_view_eof_returns_negative_one() {
        // SAFETY: empty stream hits EOF immediately.
        unsafe {
            let stream = hew_stream_from_bytes(ptr::null(), 0, 0);
            let mut buf: *mut u8 = crate::mem::buf_try_alloc(16).cast::<u8>(); // ALLOCATOR-PAIRING: GlobalAlloc
            let mut cap: usize = 16;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, -1, "EOF must return -1");
            crate::mem::buf_free(buf.cast()); // ALLOCATOR-PAIRING: GlobalAlloc
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
            let mut buf: *mut u8 = crate::mem::buf_try_alloc(4).cast::<u8>(); // ALLOCATOR-PAIRING: GlobalAlloc
            let mut cap: usize = 4;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, 4);
            assert_eq!(cap, 4, "capacity unchanged on exact fit");
            let slice = std::slice::from_raw_parts(buf, 4);
            assert_eq!(slice, b"ABCD");
            crate::mem::buf_free(buf.cast()); // ALLOCATOR-PAIRING: GlobalAlloc
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

            let mut buf: *mut u8 = crate::mem::buf_try_alloc(64).cast::<u8>(); // ALLOCATOR-PAIRING: GlobalAlloc
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

            crate::mem::buf_free(buf.cast()); // ALLOCATOR-PAIRING: GlobalAlloc
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
            let mut buf: *mut u8 = crate::mem::buf_try_alloc(1).cast::<u8>(); // ALLOCATOR-PAIRING: GlobalAlloc
            let mut cap: usize = 1;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, 1);
            assert_eq!(*buf, b'X');
            let eof = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(eof, -1, "EOF after the single item");
            crate::mem::buf_free(buf.cast()); // ALLOCATOR-PAIRING: GlobalAlloc
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
        set_last_error_with_errno_and_kind("stale".into(), 17, 3);
        // SAFETY: null is the managed empty path, which must record a fresh error.
        let result = unsafe { hew_stream_from_file_read(ptr::null()) };
        assert!(result.is_null());
        assert!(hew_stream_has_error());
        assert_ne!(take_last_error().as_deref(), Some("stale"));
    }

    #[test]
    fn file_read_nonexistent_returns_null_with_error() {
        let path = ManagedString::new("/tmp/hew_nonexistent_file_xyz_42");
        // SAFETY: path is a live managed string.
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

        let c_path = ManagedString::new(path.to_str().unwrap());
        // SAFETY: c_path is a live managed string pointing to an existing file.
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
    fn nominal_file_read_resource_endpoints_share_one_live_handle() {
        let path = temp_path("nominal_file_read_resource");
        let content = b"one nominal owner";
        std::fs::write(&path, content).unwrap();

        let c_path = ManagedString::new(path.to_str().unwrap());
        // SAFETY: c_path names an existing file, and the returned handle is
        // released exactly once through its nominal close endpoint.
        unsafe {
            let stream = hew_file_read_stream_open(c_path.as_ptr());
            assert_eq!(hew_file_read_stream_is_valid(stream), 1);
            assert_eq!(hew_file_read_stream_is_valid(ptr::null()), 0);
            let items = drain_stream(stream);
            let all: Vec<u8> = items.into_iter().flatten().collect();
            assert_eq!(all, content);
            hew_file_read_stream_close(stream);
        }
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn file_read_collection_distinguishes_text_and_decode_failures() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("contents.txt");
        let path_arg = ManagedString::new(path.to_str().unwrap());
        for (contents, fails) in [
            (b"".as_slice(), false),
            ("é\0中🙂".as_bytes(), false),
            (&[b'a', 0xff, 0], true),
        ] {
            std::fs::write(&path, contents).unwrap();
            // SAFETY: the path is live; collection consumes the returned file.
            unsafe {
                let stream = hew_file_read_stream_open(path_arg.as_ptr());
                assert!(!stream.is_null());
                // Another operation may have used the thread's channel after open.
                set_last_error_with_errno_and_kind("stale".into(), 5, 2);
                let value = hew_file_read_stream_collect_string(stream);
                let failed = hew_stream_has_error();
                let bytes = string_as_bytes(value).to_vec();
                string_release(value);
                let kind = crate::stream_error::take_last_error_kind();
                let errno = crate::stream_error::take_last_errno();
                let error = take_last_error();
                assert_eq!(failed, fails, "contents: {contents:?}");
                assert_eq!((kind, errno), (0, 0));
                if fails {
                    assert!(value.is_null());
                    assert!(error.unwrap().contains("invalid utf-8"));
                } else {
                    assert_eq!(bytes, contents);
                    assert!(error.is_none());
                }
            }
        }
    }

    #[test]
    fn file_read_collection_reports_os_read_error() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("write-only.txt");
        let mut file = std::fs::File::create(path).unwrap();
        // A real file opened only for writing must fail when read on every host.
        let expected = file.read(&mut [0u8; 1]).unwrap_err();
        let stream = into_stream_ptr(FileReadStream {
            reader: BufReader::new(file),
            chunk_size: 4096,
        });
        // SAFETY: collection takes the sole live stream owner.
        let value = unsafe { hew_file_read_stream_collect_string(stream) };
        assert!(value.is_null());
        assert_eq!(
            crate::stream_error::take_last_error_kind(),
            io_error_kind_tag(expected.kind())
        );
        assert_eq!(
            crate::stream_error::take_last_errno(),
            expected.raw_os_error().unwrap_or(0)
        );
        assert!(take_last_error()
            .unwrap()
            .contains("file stream read failed"));
    }

    #[test]
    fn file_read_empty_file_yields_eof() {
        let path = temp_path("read_empty");
        std::fs::write(&path, b"").unwrap();

        let c_path = ManagedString::new(path.to_str().unwrap());
        // SAFETY: c_path is a live managed string pointing to an existing empty file.
        unsafe {
            let stream = hew_stream_from_file_read(c_path.as_ptr());
            assert!(!stream.is_null());
            let items = drain_stream(stream);
            assert!(items.is_empty());
            hew_stream_close(stream);
        }
        let _ = std::fs::remove_file(&path);
    }

    // ── Pipe ────────────────────────────────────────────────────────────

    #[test]
    fn pipe_transfers_all_items_and_finishes_the_sink() {
        let data = b"piped content";
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 5);
            let pair = hew_stream_channel(8);
            let sink = hew_stream_pair_sink(pair);
            let output = hew_stream_pair_stream(pair);
            hew_stream_pair_free(pair);
            // pipe consumes both handles and finishes the sink at EOF.
            hew_stream_pipe(stream, sink);
            assert_eq!(
                drain_stream(output),
                vec![b"piped".to_vec(), b" cont".to_vec(), b"ent".to_vec()]
            );
            hew_stream_close(output);
        }
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
    fn lines_oversized_single_chunk_with_newline_at_cap_boundary_is_capped() {
        // Regression for PR #323 (upstream, closed unmerged): a single
        // upstream chunk (item_size=0, i.e. delivered all at once) that is
        // already over MAX_LINE_BUFFER_SIZE and whose own trailing '\n'
        // lands exactly at the cap boundary. The newline-scan branch used to
        // run unconditionally before the size-cap check, so this returned
        // the whole oversized line uncapped instead of splitting at the cap.
        let total = MAX_LINE_BUFFER_SIZE; // '\n' is byte index MAX_LINE_BUFFER_SIZE
        let mut data = vec![b'Q'; total];
        data.push(b'\n');
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let lines = hew_stream_lines(raw);
            let items = drain_stream(lines);
            assert_eq!(items.len(), 1);
            assert_eq!(items[0].len(), MAX_LINE_BUFFER_SIZE);
            hew_stream_close(lines);
        }
    }

    #[test]
    fn lines_oversized_single_chunk_with_newline_past_cap_is_capped_then_splits() {
        // Same defect as above, but the '\n' lands further past the cap
        // (not immediately at the boundary), and there is trailing data
        // after it. Must yield: [capped oversized prefix, the leftover
        // pre-\n bytes as their own line, then subsequent lines split
        // normally].
        let total = MAX_LINE_BUFFER_SIZE + 500;
        let mut data = vec![b'Q'; total];
        data.push(b'\n');
        data.extend_from_slice(b"tail\n");
        data.extend_from_slice(b"more");
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let lines = hew_stream_lines(raw);
            let items = drain_stream(lines);
            assert_eq!(items.len(), 4);
            assert_eq!(items[0].len(), MAX_LINE_BUFFER_SIZE);
            assert_eq!(items[1], vec![b'Q'; 500]);
            assert_eq!(items[2], b"tail");
            assert_eq!(items[3], b"more");
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
    fn lines_crlf_content_at_cap_minus_one_not_corrupted() {
        // Regression: a single chunk (item_size=0) whose content is exactly
        // MAX_LINE_BUFFER_SIZE - 1 bytes, terminated by "\r\n". The '\n' sits
        // at index MAX_LINE_BUFFER_SIZE (the '\r' just before it is index
        // MAX_LINE_BUFFER_SIZE - 1), so `pos == MAX_LINE_BUFFER_SIZE`. The
        // boundary check must recognize the preceding '\r' as a delimiter
        // byte (not content) and still take the fast/newline path, yielding
        // exactly one line of content length MAX_LINE_BUFFER_SIZE - 1 whose
        // last byte is 'Z' — not a cap-sized line with a leaked '\r' (0x0D).
        let mut data = vec![b'Z'; MAX_LINE_BUFFER_SIZE - 1];
        data.extend_from_slice(b"\r\n");
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            let raw = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let lines = hew_stream_lines(raw);
            let items = drain_stream(lines);
            assert_eq!(items.len(), 1);
            assert_eq!(items[0].len(), MAX_LINE_BUFFER_SIZE - 1);
            assert_eq!(items[0].last(), Some(&b'Z'));
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
    fn collect_string_preserves_read_and_close_outcomes() {
        #[derive(Debug)]
        struct ReadWithCleanup {
            prefix: Option<Vec<u8>>,
            read_fails: bool,
            close_error: Option<&'static str>,
            closes: Arc<AtomicU64>,
        }
        impl StreamBacking for ReadWithCleanup {
            fn next(&mut self) -> Option<Item> {
                if let Some(prefix) = self.prefix.take() {
                    return Some(prefix);
                }
                if self.read_fails {
                    // Portable errors need not have an OS code.
                    set_last_error_with_errno_and_kind("primary read failure".into(), 0, 2);
                }
                None
            }
            fn close(&mut self) {
                self.closes.fetch_add(1, Ordering::Relaxed);
                if let Some(error) = self.close_error {
                    set_last_error_with_errno_and_kind(error.into(), 17, 3);
                }
            }
            fn is_closed(&self) -> bool {
                false
            }
        }
        for (prefix, read_fails) in [
            (b"partial text".as_slice(), true),
            (&[0xff], true),
            ("é\0中🙂".as_bytes(), false),
            (b"".as_slice(), false),
        ] {
            for close_error in [Some("secondary close failure"), Some(""), None] {
                let closes = Arc::new(AtomicU64::new(0));
                let stream = into_stream_ptr(ReadWithCleanup {
                    prefix: Some(prefix.to_vec()),
                    read_fails,
                    close_error,
                    closes: Arc::clone(&closes),
                });
                // SAFETY: collection consumes the stream and transfers any string result.
                let value = unsafe { hew_stream_collect_string(stream) };
                let failed = hew_stream_has_error();
                // SAFETY: this caller owns the collected string until its release.
                let bytes = unsafe { string_as_bytes(value) }.to_vec();
                // SAFETY: collection transfers any returned string owner to this caller.
                unsafe { string_release(value) };
                assert_eq!(closes.load(Ordering::Relaxed), 1);
                let kind = crate::stream_error::take_last_error_kind();
                let errno = crate::stream_error::take_last_errno();
                let error = take_last_error();
                if read_fails {
                    assert!(failed);
                    assert!(value.is_null(), "partial contents must not escape");
                    assert_eq!((kind, errno), (2, 0));
                    let error = error.expect("the read failure remains primary");
                    assert!(error.starts_with("primary read failure"));
                    if let Some(secondary) = close_error {
                        assert!(error.contains("stream close failed"), "{error}");
                        assert!(error.ends_with(secondary), "{error}");
                    } else {
                        assert_eq!(error, "primary read failure");
                    }
                } else if let Some(secondary) = close_error {
                    assert!(failed, "even empty close errors must remain present");
                    assert!(value.is_null(), "failed close must release collected text");
                    assert_eq!((kind, errno), (3, 17));
                    assert_eq!(error.as_deref(), Some(secondary));
                } else {
                    assert!(!failed);
                    assert_eq!(bytes, prefix);
                    assert_eq!((kind, errno), (0, 0));
                    assert!(error.is_none());
                }
            }
        }
    }

    #[test]
    fn collect_string_concatenates_items() {
        let data = b"hello world";
        // SAFETY: all FFI calls use valid pointers.
        unsafe {
            // Split into 5-byte chunks: "hello", " worl", "d"
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 5);
            let cstr_ptr = hew_stream_collect_string(stream);
            assert!(!cstr_ptr.is_null());
            let result = string_as_str(cstr_ptr);
            assert_eq!(result, "hello world");
            string_release(cstr_ptr);
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
    fn managed_stream_collection_preserves_nul_and_decodes_after_joining_chunks() {
        let payload = "é\0中🙂".as_bytes();
        // SAFETY: input bytes remain live; collect consumes the stream and transfers a string.
        unsafe {
            let stream = hew_stream_from_bytes(payload.as_ptr(), payload.len(), 1);
            let result = hew_stream_collect_string(stream);
            assert_eq!(string_as_bytes(result), payload);
            string_release(result);
            let invalid = [0xff, 0x00, b'a'];
            let stream = hew_stream_from_bytes(invalid.as_ptr(), invalid.len(), 1);
            assert!(hew_stream_collect_string(stream).is_null());
            assert!(take_last_error().unwrap().contains("invalid utf-8"));
        }
    }

    #[test]
    fn managed_stream_layout_rejects_invalid_utf8_without_publishing_a_value() {
        let sentinel = ManagedString::new("still owned");
        let invalid = [0xff];
        // SAFETY: the raw source owns its copied bytes; out and layout are live slots.
        unsafe {
            let stream = hew_stream_from_bytes(invalid.as_ptr(), invalid.len(), 0);
            let layout = string_elem_layout();
            let mut out = sentinel.as_ptr().cast_mut();
            assert_eq!(
                hew_stream_next_layout(
                    stream,
                    std::ptr::addr_of_mut!(out).cast(),
                    &raw const layout
                ),
                0
            );
            assert_eq!(out.cast_const(), sentinel.as_ptr());
            assert_eq!(string_as_str(out), "still owned");
            assert!(take_last_error()
                .unwrap()
                .contains("invalid string element"));
            hew_stream_close(stream);
        }
    }

    #[test]
    fn managed_stream_file_paths_reject_nul_without_truncating_files() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("existing.txt");
        std::fs::write(&path, "unchanged").unwrap();
        let value = ManagedString::new(format!("{}\0suffix", path.display()));
        // SAFETY: value remains a live managed string throughout both calls.
        unsafe {
            assert!(hew_stream_from_file_read(value.as_ptr()).is_null());
            assert!(take_last_error().unwrap().contains("interior NUL"));
        }
        assert_eq!(std::fs::read_to_string(path).unwrap(), "unchanged");
    }

    #[test]
    fn collect_string_empty_stream_returns_empty() {
        // SAFETY: null data yields an empty vec stream.
        unsafe {
            let stream = hew_stream_from_bytes(ptr::null(), 0, 0);
            let cstr_ptr = hew_stream_collect_string(stream);
            assert!(cstr_ptr.is_null());
            let result = string_as_str(cstr_ptr);
            assert!(result.is_empty());
            string_release(cstr_ptr);
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

            let msg = ManagedString::new("bonjour");
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
        let msg = ManagedString::new("ignored");
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
            crate::mem::buf_free(buf); // ALLOCATOR-PAIRING: GlobalAlloc
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

    // ── File-open error classification (canonical kind channel) ──────────

    #[test]
    fn from_file_read_missing_sets_not_found_kind() {
        // Cross-platform regression (host-independent by construction): opening
        // a missing file for read yields std::io::ErrorKind::NotFound on every
        // OS, so hew_stream_from_file_read must record the canonical NotFound
        // tag. std::stream's try_from_file reads that tag before the raw errno,
        // so on Windows an access-denied open (raw 5) classifies as
        // PermissionDenied instead of the old Other(5). Here we pin the NotFound
        // wiring, which is reproducible without a Windows host.
        use crate::stream_error::{take_last_errno, take_last_error_kind, IO_ERROR_KIND_NOT_FOUND};
        let c_path = ManagedString::new("/tmp/hew_stream_missing_open_read.txt");
        // SAFETY: c_path is a live managed string.
        let s = unsafe { hew_stream_from_file_read(c_path.as_ptr()) };
        assert!(s.is_null(), "opening a missing file for read must fail");
        // Read the kind BEFORE the errno (take_last_errno clears the tag).
        let kind = take_last_error_kind();
        let errno = take_last_errno();
        assert_eq!(
            kind, IO_ERROR_KIND_NOT_FOUND,
            "a missing-file open must classify as NotFound on every platform"
        );
        assert_ne!(
            errno, 0,
            "the raw OS errno must be preserved in the payload"
        );
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
        let mut backing = TcpStreamBacking::new(accepted);

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

        // The peer must not reset until the listener has accepted the
        // connection. On the BSDs a RST that arrives before `accept()` removes
        // the half-open connection from the queue and makes `accept()` return
        // ECONNABORTED (os error 53); with only one peer connecting, a retry
        // would then block forever on an empty queue. Linux instead returns the
        // RST-marked connection and surfaces the reset on the first read. Gating
        // the reset on an explicit "accepted" signal removes that race and
        // exercises the same accepted-then-reset read path on every platform.
        let (accepted_tx, accepted_rx) = std::sync::mpsc::channel::<()>();

        // Peer thread: connect, wait until accepted, set SO_LINGER=0, drop → RST.
        let t = std::thread::spawn(move || {
            let peer = TcpStream::connect(addr).unwrap();
            // Hold the connection open until the listener side has accepted it.
            accepted_rx.recv().unwrap();
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

        let (accepted, _) = listener.accept().unwrap();
        // The connection is established; tell the peer it may reset now.
        accepted_tx.send(()).unwrap();
        // join() guarantees the peer thread has set SO_LINGER=0 and dropped its
        // socket — the RST (or FIN) is sent before we read. No sleep needed: the
        // synchronization is the join, and `backing.next()` below is a BLOCKING
        // read that returns only once the reset/EOF is observed. A wall-clock
        // "give the RST time to arrive" sleep is a jitter window, not a wait.
        t.join().unwrap();

        let mut backing = TcpStreamBacking::new(accepted);
        // RST produces either ConnectionReset or Ok(0) EOF — either way None.
        // The read blocks until that event arrives (the deterministic wait).
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
        // `hew_tcp_connect` (in `make_loopback_conn`) offloads the resolve onto
        // the current runtime's blocking pool, so a runtime must be installed.
        let _guard = crate::runtime_test_guard();
        let (conn_handle, _peer) = make_loopback_conn();

        // SAFETY: conn_handle is a valid registered connection.
        let pair = unsafe { hew_tcp_stream_from_conn(conn_handle) };
        assert!(
            !pair.is_null(),
            "factory must return non-null for valid conn"
        );

        // Both halves must be extractable.
        // SAFETY: pair is valid.
        let stream_ptr = unsafe { hew_stream_pair_stream(pair) };
        assert!(!stream_ptr.is_null());

        // SAFETY: pair still valid (stream extraction nulls the stream slot, sink is still there).
        let sink_ptr = unsafe { hew_stream_pair_sink(pair) };
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
    fn tcp_sink_shutdown_publishes_eof_while_read_half_remains_live() {
        use std::io::Read as _;
        use std::time::Duration;

        let _guard = crate::runtime_test_guard();
        let (conn_handle, mut peer) = make_loopback_conn();
        peer.set_read_timeout(Some(Duration::from_secs(1)))
            .expect("set peer read timeout");

        // SAFETY: conn_handle and the returned pair are live runtime handles.
        let pair = unsafe { hew_tcp_stream_from_conn(conn_handle) };
        assert!(!pair.is_null());
        // SAFETY: each extraction consumes one pointer slot in the pair.
        let stream_ptr = unsafe { hew_stream_pair_stream(pair) };
        // SAFETY: the sink slot remains live and has not been extracted yet.
        let sink_ptr = unsafe { hew_stream_pair_sink(pair) };
        assert!(!stream_ptr.is_null() && !sink_ptr.is_null());

        // SAFETY: sink_ptr is live and remains wrapper-owned after shutdown.
        unsafe { hew_sink_finish(sink_ptr) };
        let mut byte = [0u8; 1];
        assert_eq!(
            peer.read(&mut byte).expect("peer observes sink shutdown"),
            0,
            "TCP sink shutdown must publish EOF without waiting for the read half"
        );

        // SAFETY: the wrappers and pair are still singly owned here.
        unsafe {
            hew_stream_close(stream_ptr);
            hew_sink_close(sink_ptr);
            hew_stream_pair_free(pair);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn tcp_bridge_restores_blocking_reads_after_nonblocking_accept_mode() {
        use std::time::Duration;

        let _guard = crate::runtime_test_guard();
        let _reactor = native::TestReactor::new();
        let (conn_handle, mut peer) = make_loopback_conn();
        assert!(
            crate::transport::tcp_conn_set_nonblocking(conn_handle, true),
            "test connection enters the reactor's accepted-socket mode"
        );

        // SAFETY: conn_handle and the returned pair are live runtime handles.
        let pair = unsafe { hew_tcp_stream_from_conn(conn_handle) };
        assert!(!pair.is_null());
        // SAFETY: the factory transfers the pair's sole allocation to the test.
        let mut pair = unsafe { Box::from_raw(pair) };
        // SAFETY: extraction consumes the stream slot in the pair.
        let stream_ptr = unsafe { hew_stream_pair_stream(&raw mut *pair) };
        assert!(!stream_ptr.is_null());
        // SAFETY: extraction transferred the stream's sole allocation.
        let mut stream = unsafe { Box::from_raw(stream_ptr) };

        let writer = std::thread::spawn(move || {
            std::thread::sleep(Duration::from_millis(25));
            peer.write_all(b"ready").expect("delayed peer write");
        });
        let mut size = 0usize;
        // SAFETY: stream is live and size is writable.
        let data = unsafe { hew_stream_next_sized(&raw mut *stream, &raw mut size) };
        // SAFETY: the returned bytes use the runtime buffer allocator; freeing
        // before assertions also releases them if the writer or size is wrong.
        unsafe { crate::mem::buf_free(data) };
        writer.join().expect("delayed writer joins");
        assert!(!data.is_null(), "WouldBlock must not masquerade as EOF");
        assert_eq!(size, 5);
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
        // See `factory_returns_pair_for_valid_conn`: the connect offload needs a
        // runtime-owned blocking pool.
        let _guard = crate::runtime_test_guard();
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

    use crate::vec::{HewTypeOwnershipKind, HewValueLayout};

    fn plain_elem_layout(size: usize, align: usize) -> HewValueLayout {
        HewValueLayout {
            release_start: None,
            size,
            align,
            ownership_kind: HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
        }
    }

    fn string_elem_layout() -> HewValueLayout {
        HewValueLayout {
            release_start: None,
            size: size_of::<*const HewString>(),
            align: align_of::<*const HewString>(),
            ownership_kind: HewTypeOwnershipKind::String,
            clone_fn: None,
            drop_fn: None,
        }
    }

    fn bytes_elem_layout() -> HewValueLayout {
        HewValueLayout {
            release_start: None,
            size: size_of::<crate::bytes::BytesTriple>(),
            align: align_of::<crate::bytes::BytesTriple>(),
            ownership_kind: HewTypeOwnershipKind::Bytes,
            clone_fn: None,
            drop_fn: None,
        }
    }

    /// String elements ride the witness path content-encoded: the sink reads
    /// the caller's string slot, the stream materialises a fresh managed
    /// string. An empty string survives as `Some("")` (rc 1).
    #[test]
    fn layout_stream_string_roundtrip_preserves_empty() {
        // SAFETY: hew_stream_channel returns a valid pair; slots are locals.
        unsafe {
            let pair = hew_stream_channel(4);
            let sink = hew_stream_pair_sink(pair);
            let stream = hew_stream_pair_stream(pair);

            let layout = string_elem_layout();
            let alpha_owner = ManagedString::new("alpha\0é");
            let alpha = alpha_owner.as_ptr();
            hew_stream_send_layout(sink, std::ptr::addr_of!(alpha).cast(), &raw const layout);
            let empty: *const HewString = ptr::null();
            hew_stream_send_layout(sink, std::ptr::addr_of!(empty).cast(), &raw const layout);
            hew_sink_close(sink);

            let mut out: *mut HewString = ptr::null_mut();
            let rc = hew_stream_next_layout(
                stream,
                std::ptr::addr_of_mut!(out).cast(),
                &raw const layout,
            );
            assert_eq!(rc, 1);
            assert_eq!(string_as_bytes(out), "alpha\0é".as_bytes());
            string_release(out);

            let rc = hew_stream_next_layout(
                stream,
                std::ptr::addr_of_mut!(out).cast(),
                &raw const layout,
            );
            assert_eq!(rc, 1, "empty string element is Some(\"\"), not None");
            assert_eq!(string_as_bytes(out), b"");
            string_release(out);

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
    fn layout_stream_bytes_roundtrip_preserves_empty() {
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
            assert_eq!(rc, 1, "a present empty item is distinct from EOF");
            assert_eq!(out.len, 0);
            crate::bytes::hew_bytes_drop(out.ptr);
            assert_eq!(
                hew_stream_pop_layout(
                    stream,
                    std::ptr::addr_of_mut!(out).cast(),
                    &raw const layout
                ),
                0,
                "only the following receive reaches EOF"
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
        let dup = crate::mem::buf_try_alloc(8).cast::<u8>(); // ALLOCATOR-PAIRING: GlobalAlloc
        if !s.heap.is_null() {
            // SAFETY: both buffers are 8 bytes.
            unsafe { std::ptr::copy_nonoverlapping(s.heap, dup, 8) };
        }
        d.heap = dup;
        ST_OWNED_CLONES.fetch_add(1, Ordering::SeqCst);
        0
    }

    unsafe extern "C-unwind" fn st_owned_drop(slot: *mut c_void) {
        // SAFETY: thunk contract — slot is a live element being released.
        let e = unsafe { &mut *slot.cast::<StOwnedElem>() };
        if !e.heap.is_null() {
            // SAFETY: heap was allocated via the sized-block allocator by st_owned_clone / the test body.
            unsafe { crate::mem::buf_free(e.heap.cast()) };
            e.heap = ptr::null_mut();
        }
        ST_OWNED_DROPS.fetch_add(1, Ordering::SeqCst);
    }

    fn st_owned_layout() -> HewValueLayout {
        HewValueLayout {
            release_start: None,
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
                let heap = crate::mem::buf_try_alloc(8).cast::<u8>();
                let value = StOwnedElem { tag, heap };
                hew_stream_send_layout(sink, std::ptr::addr_of!(value).cast(), &raw const layout);
                crate::mem::buf_free(value.heap.cast());
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
