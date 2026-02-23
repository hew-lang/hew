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
//! malloc-allocated byte buffers:
//!
//! - `hew_stream_next` returns a malloc'd item on success, NULL on EOF. The
//!   caller must `free()` the returned pointer.
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

use std::collections::VecDeque;
use std::ffi::{c_void, CStr};
use std::fs;
use std::io::{BufReader, Read, Write};
use std::mem::ManuallyDrop;
use std::ptr;
use std::sync::mpsc;

// ── Re-export sink types from hew-cabi ────────────────────────────────────────
// These are the shared ABI types that native packages (e.g. HTTP) also use.
// Defining them in hew-cabi avoids pulling the full runtime into stdlib packages.

pub use hew_cabi::sink::{into_sink_ptr, set_last_error, take_last_error, HewSink, SinkBacking};

// hew_stream_last_error is defined in hew-cabi::sink (with #[no_mangle])
// so we re-export it here for Rust callers but don't redefine the C symbol.
pub use hew_cabi::sink::hew_stream_last_error;

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
    /// Return the next item, or `None` on EOF.
    fn next(&mut self) -> Option<Item>;
    /// Discard remaining items and signal done to the producer.
    fn close(&mut self);
    /// Check if the stream is exhausted without consuming an item.
    fn is_closed(&self) -> bool;
}

// SinkBacking is defined in hew_cabi::sink and re-exported above.

// ── Public handle types ────────────────────────────────────────────────────────

/// Opaque readable stream handle.
#[derive(Debug)]
pub struct HewStream {
    inner: Box<dyn StreamBacking>,
    /// Whether `close()` has already been called on the backing.
    closed: bool,
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
            unsafe { drop(Box::from_raw(self.sink)) };
            self.sink = std::ptr::null_mut();
        }
        if !self.stream.is_null() {
            // SAFETY: stream was allocated with Box::into_raw and is still owned.
            unsafe { drop(Box::from_raw(self.stream)) };
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
    rx: mpsc::Receiver<Item>,
}

#[derive(Debug)]
struct ChannelSink {
    tx: mpsc::SyncSender<Item>,
}

impl StreamBacking for ChannelStream {
    fn next(&mut self) -> Option<Item> {
        self.rx.recv().ok()
    }

    fn close(&mut self) {
        // Drain the channel so the sender is unblocked and can observe disconnect.
        while self.rx.try_recv().is_ok() {}
        // Dropping `rx` here signals the sender-side that the channel is gone,
        // but we can't drop `self` from a method.  The struct will be dropped
        // when the HewStream is freed.
    }

    fn is_closed(&self) -> bool {
        // Channels don't know they are closed until they try to receive.
        false
    }
}

impl SinkBacking for ChannelSink {
    fn write_item(&mut self, data: &[u8]) {
        // Blocks (with backpressure) if the bounded channel is full.
        let _ = self.tx.send(data.to_vec());
    }

    fn flush(&mut self) {
        // In-memory channels have no buffering to flush.
    }

    fn close(&mut self) {
        // Dropping tx disconnects the channel; the stream side sees EOF
        // on the next recv().  We signal this by sending a sentinel via
        // the channel disconnect (tx drop happens when HewSink is freed).
    }
}

// ── Vec backing (drain) ────────────────────────────────────────────────────────

#[derive(Debug)]
struct VecStream {
    items: VecDeque<Item>,
}

impl StreamBacking for VecStream {
    fn next(&mut self) -> Option<Item> {
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

// ── File-write backing ────────────────────────────────────────────────────────

#[derive(Debug)]
struct FileWriteSink {
    writer: fs::File,
}

impl SinkBacking for FileWriteSink {
    fn write_item(&mut self, data: &[u8]) {
        let _ = self.writer.write_all(data);
    }

    fn flush(&mut self) {
        let _ = self.writer.flush();
    }

    fn close(&mut self) {
        let _ = self.writer.flush();
        // File is closed when the struct is dropped.
    }
}

// ── Lines adapter backing ─────────────────────────────────────────────────────

/// Wraps a `Stream<bytes>` and yields newline-terminated strings (as utf-8 bytes).
#[derive(Debug)]
struct LinesStream {
    /// Unconsumed bytes from the upstream stream.
    buf: Vec<u8>,
    /// Upstream bytes stream.
    upstream: Box<dyn StreamBacking>,
    done: bool,
}

impl StreamBacking for LinesStream {
    fn next(&mut self) -> Option<Item> {
        loop {
            // Check if there's a complete line already buffered.
            if let Some(pos) = self.buf.iter().position(|&b| b == b'\n') {
                let mut line: Vec<u8> = self.buf.drain(..=pos).collect();
                // Strip the trailing newline delimiter.
                if line.last() == Some(&b'\n') {
                    line.pop();
                }
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
        inner: Box::new(backing),
        closed: false,
    }))
}

// into_sink_ptr is defined in hew_cabi::sink and re-exported above.

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
    let cap = usize::try_from(capacity.max(1)).unwrap_or(1);
    let (tx, rx) = mpsc::sync_channel::<Item>(cap);

    let stream_ptr = into_stream_ptr(ChannelStream { rx });
    let sink_ptr = into_sink_ptr(ChannelSink { tx });

    Box::into_raw(Box::new(HewStreamPair {
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
    if pair.is_null() {
        return ptr::null_mut();
    }
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
    if pair.is_null() {
        return ptr::null_mut();
    }
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
        unsafe { drop(Box::from_raw(pair)) };
    }
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
pub unsafe extern "C" fn hew_stream_from_file_read(path: *const i8) -> *mut HewStream {
    if path.is_null() {
        return ptr::null_mut();
    }
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
            set_last_error(format!("{e}"));
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
pub unsafe extern "C" fn hew_stream_from_file_write(path: *const i8) -> *mut HewSink {
    if path.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees path is a valid null-terminated C string.
    let s = unsafe { CStr::from_ptr(path) };
    let Ok(path_str) = s.to_str() else {
        set_last_error("invalid UTF-8 in path".into());
        return ptr::null_mut();
    };
    match fs::File::create(path_str) {
        Ok(f) => into_sink_ptr(FileWriteSink { writer: f }),
        Err(e) => {
            set_last_error(format!("{e}"));
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

/// Get the next item from a stream.
///
/// Returns a malloc-allocated byte buffer that the caller must `free()`.
/// Returns null when the stream is exhausted (EOF).
///
/// # Safety
///
/// `stream` must be a valid pointer created by one of the `hew_stream_*`
/// constructor functions.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_next(stream: *mut HewStream) -> *mut c_void {
    if stream.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: stream is valid per caller contract.
    let s = unsafe { &mut *stream };
    match s.inner.next() {
        Some(item) if !item.is_empty() => {
            let len = item.len();
            // SAFETY: libc::malloc returns a valid aligned pointer or null.
            // Allocate len + 1 for a NUL terminator so the buffer can be
            // used as a C string by hew_print_str / println.
            let buf = unsafe { libc::malloc(len + 1) };
            if buf.is_null() {
                return ptr::null_mut();
            }
            // SAFETY: buf is len+1 bytes allocated above; item.as_ptr() points to len bytes.
            unsafe { ptr::copy_nonoverlapping(item.as_ptr(), buf.cast::<u8>(), len) };
            // SAFETY: buf has len+1 bytes allocated; writing the null terminator at offset len.
            unsafe { *buf.cast::<u8>().add(len) = 0 };
            buf.cast::<c_void>()
        }
        _ => ptr::null_mut(),
    }
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
    if stream.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: stream is valid per caller contract.
    let s = unsafe { &mut *stream };
    match s.inner.next() {
        Some(item) if !item.is_empty() => {
            let len = item.len();
            if !out_size.is_null() {
                // SAFETY: Caller guarantees out_size is valid.
                unsafe { *out_size = len };
            }
            // SAFETY: libc::malloc returns a valid aligned pointer or null.
            let buf = unsafe { libc::malloc(len) };
            if buf.is_null() {
                return ptr::null_mut();
            }
            // SAFETY: buf is len bytes allocated above; item.as_ptr() points to len bytes.
            unsafe { ptr::copy_nonoverlapping(item.as_ptr(), buf.cast::<u8>(), len) };
            buf.cast::<c_void>()
        }
        _ => {
            if !out_size.is_null() {
                // SAFETY: Caller guarantees out_size is valid.
                unsafe { *out_size = 0 };
            }
            ptr::null_mut()
        }
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
        // SAFETY: stream was allocated with Box::into_raw.
        // Drop impl calls close() on the backing.
        unsafe { drop(Box::from_raw(stream)) };
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
    unsafe { (*sink).inner.write_item(bytes) };
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
        unsafe { (*sink).inner.flush() };
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
        // SAFETY: sink was allocated with Box::into_raw.
        // Drop impl calls close() on the backing.
        unsafe { drop(Box::from_raw(sink)) };
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
    if stream.is_null() || sink.is_null() {
        return;
    }
    // SAFETY: Both pointers are valid per caller contract.
    let s = unsafe { &mut *stream };
    // SAFETY: sink is non-null (checked above) and valid per caller contract.
    let k = unsafe { &mut *sink };

    while let Some(item) = s.inner.next() {
        k.inner.write_item(&item);
    }
    k.inner.close();

    // Free both handles.
    // SAFETY: Both were allocated with Box::into_raw.
    unsafe {
        drop(Box::from_raw(stream));
        drop(Box::from_raw(sink));
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
    if stream.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: stream was allocated with Box::into_raw; we take ownership.
    // ManuallyDrop prevents the HewStream Drop from running (we're transferring inner).
    let owned = ManuallyDrop::new(unsafe { Box::from_raw(stream) });
    into_stream_ptr(LinesStream {
        buf: Vec::new(),
        // SAFETY: owned.inner is valid; ManuallyDrop ensures no double-free.
        upstream: unsafe { ptr::read(&raw const owned.inner) },
        done: false,
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
    if stream.is_null() {
        return ptr::null_mut();
    }
    let size = usize::try_from(chunk_size.max(1)).unwrap_or(1);
    // SAFETY: stream was allocated with Box::into_raw; we take ownership.
    // ManuallyDrop prevents the HewStream Drop from running (we're transferring inner).
    let owned = ManuallyDrop::new(unsafe { Box::from_raw(stream) });
    into_stream_ptr(ChunksStream {
        buf: Vec::new(),
        chunk_size: size,
        // SAFETY: owned.inner is valid; ManuallyDrop ensures no double-free.
        upstream: unsafe { ptr::read(&raw const owned.inner) },
        done: false,
    })
}

// ── Convenience functions ─────────────────────────────────────────────────────

/// Read all remaining items from a stream and concatenate them as a C string.
///
/// Returns a malloc-allocated null-terminated string. The caller must free it.
/// Consumes the stream.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_collect_string(stream: *mut HewStream) -> *mut i8 {
    if stream.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: stream was allocated with Box::into_raw; we take ownership.
    let mut owned = unsafe { Box::from_raw(stream) };
    let mut buffer = Vec::new();

    while let Some(chunk) = owned.inner.next() {
        buffer.extend_from_slice(&chunk);
    }

    // Ensure null termination
    buffer.push(0);

    let len = buffer.len();
    // SAFETY: libc::malloc returns a valid aligned pointer or null.
    let ptr = unsafe { libc::malloc(len) };
    if ptr.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: ptr is len bytes allocated above; buffer.as_ptr() points to len bytes.
    unsafe { ptr::copy_nonoverlapping(buffer.as_ptr(), ptr.cast::<u8>(), len) };
    ptr.cast::<i8>()
}

/// Count remaining items in a stream.
///
/// Consumes the stream.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_count(stream: *mut HewStream) -> i64 {
    if stream.is_null() {
        return 0;
    }

    // SAFETY: stream was allocated with Box::into_raw; we take ownership.
    let mut owned = unsafe { Box::from_raw(stream) };
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
pub unsafe extern "C" fn hew_sink_write_string(sink: *mut HewSink, data: *const i8) {
    if sink.is_null() || data.is_null() {
        return;
    }

    // SAFETY: data is a valid C string.
    let s = unsafe { CStr::from_ptr(data) };
    let bytes = s.to_bytes();

    // SAFETY: sink is valid per caller contract.
    unsafe { (*sink).inner.write_item(bytes) };
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
    if stream.is_null() {
        return 1;
    }

    // SAFETY: stream is valid per caller contract.
    let s = unsafe { &*stream };
    i32::from(s.inner.is_closed())
}
