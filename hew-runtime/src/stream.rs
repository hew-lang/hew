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
use std::ffi::{c_char, c_void, CStr};
use std::fs;
use std::io::{BufReader, Read, Write};
use std::ptr;
use std::sync::mpsc;

// ── Re-export sink types from hew-cabi ────────────────────────────────────────
// These are the shared ABI types that native packages (e.g. HTTP) also use.
// Defining them in hew-cabi avoids pulling the full runtime into stdlib packages.

pub use hew_cabi::sink::{into_sink_ptr, set_last_error, take_last_error, HewSink, SinkBacking};

// hew_stream_last_error is defined in hew-cabi::sink (with #[no_mangle])
// so we re-export it here for Rust callers but don't redefine the C symbol.
pub use hew_cabi::sink::hew_stream_last_error;

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
        if self.tx.send(data.to_vec()).is_err() {
            set_last_error("hew_sink_write: failed to send to channel sink".to_string());
        }
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
        if let Err(e) = self.writer.write_all(data) {
            set_last_error(format!("hew_sink_write: file write failed: {e}"));
        }
    }

    fn flush(&mut self) {
        if let Err(e) = self.writer.flush() {
            set_last_error(format!("hew_sink_flush: file flush failed: {e}"));
        }
    }

    fn close(&mut self) {
        if let Err(e) = self.writer.flush() {
            set_last_error(format!("hew_sink_close: file flush failed: {e}"));
        }
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
                // Strip the trailing newline delimiter (and \r for CRLF).
                if line.last() == Some(&b'\n') {
                    line.pop();
                }
                if line.last() == Some(&b'\r') {
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
    // SAFETY: stream was allocated via Box::into_raw(Box::new(HewStream { .. })),
    // so deallocating with Layout::new::<HewStream>() is correct. We use dealloc
    // instead of Box::from_raw to avoid running Drop (which would double-free inner).
    unsafe {
        std::alloc::dealloc(stream.cast::<u8>(), std::alloc::Layout::new::<HewStream>());
    }
    inner
}

// into_sink_ptr is defined in hew_cabi::sink and re-exported above.

// ── Map adapter ───────────────────────────────────────────────────────────────

/// Calling convention matching the Hew closure ABI: (env, string) → owned C string.
type StringMapFn = unsafe extern "C" fn(*const c_void, *const c_char) -> *mut c_char;

/// Wraps a stream and lazily applies a `fn(String) -> String` closure to every item.
#[derive(Debug)]
struct MapStringStream {
    upstream: Box<dyn StreamBacking>,
    fn_ptr: StringMapFn,
    env_ptr: *const c_void,
}

// SAFETY: fn_ptr is a plain function pointer; env_ptr is an RC'd closure environment
// that is only accessed from one thread at a time (stream ownership is single-threaded).
unsafe impl Send for MapStringStream {}

impl StreamBacking for MapStringStream {
    fn next(&mut self) -> Option<Item> {
        let item = self.upstream.next()?;
        // Build a null-terminated copy for the closure.
        let mut with_nul = item.clone();
        with_nul.push(0);
        // SAFETY: fn_ptr is a valid Hew closure, env_ptr is its environment.
        let result_ptr = unsafe { (self.fn_ptr)(self.env_ptr, with_nul.as_ptr().cast::<c_char>()) };
        if result_ptr.is_null() {
            return Some(Vec::new());
        }
        // Convert the malloc'd result string to an owned Vec<u8> (without the NUL).
        // SAFETY: result_ptr is a valid null-terminated C string from the closure.
        let result_cstr = unsafe { std::ffi::CStr::from_ptr(result_ptr) };
        let bytes = result_cstr.to_bytes().to_vec();
        // SAFETY: result_ptr was malloc'd by the closure; we own it now.
        unsafe { libc::free(result_ptr.cast::<c_void>()) };
        Some(bytes)
    }

    fn close(&mut self) {
        self.upstream.close();
    }

    fn is_closed(&self) -> bool {
        self.upstream.is_closed()
    }
}

impl Drop for MapStringStream {
    fn drop(&mut self) {
        // SAFETY: env_ptr is an RC'd block; decrement its reference count on drop.
        unsafe { hew_rc_drop_env(self.env_ptr) };
    }
}

// ── Filter adapter ────────────────────────────────────────────────────────────

/// Calling convention: (env, string) → i32 (non-zero means keep the item).
type StringFilterFn = unsafe extern "C" fn(*const c_void, *const c_char) -> i32;

/// Wraps a stream and lazily skips items for which the predicate returns false.
#[derive(Debug)]
struct FilterStringStream {
    upstream: Box<dyn StreamBacking>,
    fn_ptr: StringFilterFn,
    env_ptr: *const c_void,
    done: bool,
}

// SAFETY: same as MapStringStream.
unsafe impl Send for FilterStringStream {}

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

impl Drop for FilterStringStream {
    fn drop(&mut self) {
        // SAFETY: env_ptr is an RC'd block; decrement its reference count on drop.
        unsafe { hew_rc_drop_env(self.env_ptr) };
    }
}

// ── Map adapter (bytes) ───────────────────────────────────────────────────────

/// Calling convention for bytes map closures.
///
/// The closure receives a `*mut HewVec` (bytes) and returns a new `*mut HewVec`.
/// Both the input and the return value are owned: the closure frees the input,
/// and the caller owns the returned vec.
type BytesMapFn = unsafe extern "C" fn(*const c_void, *mut HewVec) -> *mut HewVec;

/// Wraps a stream and lazily applies a `fn(bytes) -> bytes` closure to every item.
#[derive(Debug)]
struct MapBytesStream {
    upstream: Box<dyn StreamBacking>,
    fn_ptr: BytesMapFn,
    env_ptr: *const c_void,
}

// SAFETY: fn_ptr is a plain function pointer; env_ptr is an RC'd closure
// environment that is only accessed from one thread at a time.
unsafe impl Send for MapBytesStream {}

impl StreamBacking for MapBytesStream {
    fn next(&mut self) -> Option<Item> {
        let item = self.upstream.next()?;
        // Convert raw item bytes to a HewVec for the closure.
        // SAFETY: u8_to_hwvec allocates a fresh HewVec.
        let input_vec = unsafe { hew_cabi::vec::u8_to_hwvec(&item) };
        // SAFETY: fn_ptr is a valid Hew closure, env_ptr is its environment.
        let result_vec = unsafe { (self.fn_ptr)(self.env_ptr, input_vec) };
        // Free the input vec when the closure returned a new allocation.
        // If the closure mutated and returned the same pointer, skip to
        // avoid a double-free (result_vec will be freed below).
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
        // Free the closure's returned vec.
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

impl Drop for MapBytesStream {
    fn drop(&mut self) {
        // SAFETY: env_ptr is an RC'd block; decrement its reference count on drop.
        unsafe { hew_rc_drop_env(self.env_ptr) };
    }
}

// ── Filter adapter (bytes) ────────────────────────────────────────────────────

/// Calling convention for bytes filter closures.
///
/// The closure receives a `*mut HewVec` (bytes) and returns non-zero to keep
/// the item.  The closure does NOT take ownership — the caller frees the vec
/// after the predicate returns.
type BytesFilterFn = unsafe extern "C" fn(*const c_void, *mut HewVec) -> i32;

/// Wraps a stream and lazily skips items for which the predicate returns false.
#[derive(Debug)]
struct FilterBytesStream {
    upstream: Box<dyn StreamBacking>,
    fn_ptr: BytesFilterFn,
    env_ptr: *const c_void,
    done: bool,
}

// SAFETY: same as MapBytesStream.
unsafe impl Send for FilterBytesStream {}

impl StreamBacking for FilterBytesStream {
    fn next(&mut self) -> Option<Item> {
        loop {
            if self.done {
                return None;
            }
            let item = self.upstream.next()?;
            // Build a temporary HewVec for the predicate.
            // SAFETY: u8_to_hwvec allocates a fresh HewVec.
            let tmp_vec = unsafe { hew_cabi::vec::u8_to_hwvec(&item) };
            // SAFETY: fn_ptr is a valid Hew closure, env_ptr is its environment.
            let keep = unsafe { (self.fn_ptr)(self.env_ptr, tmp_vec) };
            // Free the temporary vec — the predicate borrows it, not owns it.
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

impl Drop for FilterBytesStream {
    fn drop(&mut self) {
        // SAFETY: env_ptr is an RC'd block; decrement its reference count on drop.
        unsafe { hew_rc_drop_env(self.env_ptr) };
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
unsafe fn hew_rc_drop_env(env_ptr: *const c_void) {
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
pub unsafe extern "C" fn hew_stream_from_file_write(path: *const c_char) -> *mut HewSink {
    cabi_guard!(path.is_null(), ptr::null_mut());
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
    cabi_guard!(stream.is_null(), ptr::null_mut());
    // SAFETY: stream is valid per caller contract.
    let s = unsafe { &mut *stream };
    match s.inner.next() {
        Some(item) => {
            let len = item.len();
            // Allocate len + 1 for a NUL terminator so the buffer can be
            // used as a C string by hew_print_str / println.
            // For empty items, this yields a 1-byte buffer containing '\0'.
            // SAFETY: libc::malloc returns a valid aligned pointer or null.
            let buf = unsafe { libc::malloc(len + 1) };
            if buf.is_null() {
                return ptr::null_mut();
            }
            if len > 0 {
                // SAFETY: buf is len+1 bytes allocated above; item.as_ptr() points to len bytes.
                unsafe { ptr::copy_nonoverlapping(item.as_ptr(), buf.cast::<u8>(), len) };
            }
            // SAFETY: buf has len+1 bytes allocated; writing the null terminator at offset len.
            unsafe { *buf.cast::<u8>().add(len) = 0 };
            buf.cast::<c_void>()
        }
        None => ptr::null_mut(),
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
        let buf = unsafe { libc::malloc(alloc_len) };
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
    cabi_guard!(stream.is_null() || sink.is_null());
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
    cabi_guard!(stream.is_null(), ptr::null_mut());
    // SAFETY: stream is a valid HewStream pointer from the Hew runtime ABI.
    let upstream = unsafe { consume_stream_inner(stream) };
    into_stream_ptr(LinesStream {
        buf: Vec::new(),
        upstream,
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
/// Returns a malloc-allocated null-terminated string. The caller must free it.
/// Consumes the stream.
///
/// # Safety
///
/// `stream` must be a valid `HewStream` pointer or null.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_collect_string(stream: *mut HewStream) -> *mut c_char {
    cabi_guard!(stream.is_null(), ptr::null_mut());

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
pub unsafe extern "C" fn hew_sink_write_string(sink: *mut HewSink, data: *const c_char) {
    cabi_guard!(sink.is_null() || data.is_null());

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
    cabi_guard!(stream.is_null(), 1);

    // SAFETY: stream is valid per caller contract.
    let s = unsafe { &*stream };
    i32::from(s.inner.is_closed())
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
    cabi_guard!(stream.is_null() || fn_ptr.is_null(), ptr::null_mut());
    // SAFETY: stream is a valid HewStream pointer from the Hew runtime ABI.
    let upstream = unsafe { consume_stream_inner(stream) };
    // SAFETY: fn_ptr is a valid function pointer with the documented ABI.
    let fn_typed: StringMapFn = unsafe { std::mem::transmute(fn_ptr) };
    into_stream_ptr(MapStringStream {
        upstream,
        fn_ptr: fn_typed,
        env_ptr,
    })
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
    cabi_guard!(stream.is_null() || fn_ptr.is_null(), ptr::null_mut());
    // SAFETY: stream is a valid HewStream pointer from the Hew runtime ABI.
    let upstream = unsafe { consume_stream_inner(stream) };
    // SAFETY: fn_ptr is a valid function pointer with the documented ABI.
    let fn_typed: StringFilterFn = unsafe { std::mem::transmute(fn_ptr) };
    into_stream_ptr(FilterStringStream {
        upstream,
        fn_ptr: fn_typed,
        env_ptr,
        done: false,
    })
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
    cabi_guard!(stream.is_null() || fn_ptr.is_null(), ptr::null_mut());
    // SAFETY: stream is a valid HewStream pointer from the Hew runtime ABI.
    let upstream = unsafe { consume_stream_inner(stream) };
    // SAFETY: fn_ptr is a valid function pointer with the documented ABI.
    let fn_typed: BytesMapFn = unsafe { std::mem::transmute(fn_ptr) };
    into_stream_ptr(MapBytesStream {
        upstream,
        fn_ptr: fn_typed,
        env_ptr,
    })
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
    cabi_guard!(stream.is_null() || fn_ptr.is_null(), ptr::null_mut());
    // SAFETY: stream is a valid HewStream pointer from the Hew runtime ABI.
    let upstream = unsafe { consume_stream_inner(stream) };
    // SAFETY: fn_ptr is a valid function pointer with the documented ABI.
    let fn_typed: BytesFilterFn = unsafe { std::mem::transmute(fn_ptr) };
    into_stream_ptr(FilterBytesStream {
        upstream,
        fn_ptr: fn_typed,
        env_ptr,
        done: false,
    })
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

/// Read the next item from a stream and return it as a `bytes` value.
///
/// Returns a `*mut HewVec` (i32-element vec, one byte per slot) on success —
/// including for zero-length items — or **null** on EOF.  The caller owns the
/// returned vec and must eventually free it.
///
/// # Safety
///
/// `stream` must be a valid stream pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_stream_next_bytes(stream: *mut HewStream) -> *mut HewVec {
    if stream.is_null() {
        return std::ptr::null_mut();
    }
    let mut size: usize = 0;
    // SAFETY: stream is valid per caller contract; size is a valid local.
    let raw_ptr = unsafe { hew_stream_next_sized(stream, std::ptr::addr_of_mut!(size)) };
    if raw_ptr.is_null() {
        return std::ptr::null_mut(); // EOF
    }
    // SAFETY: raw_ptr is valid for `size` bytes (from hew_stream_next_sized contract).
    let raw = unsafe { std::slice::from_raw_parts(raw_ptr.cast::<u8>(), size) };
    // SAFETY: u8_to_hwvec allocates a new HewVec; raw slice is valid.
    let vec = unsafe { hew_cabi::vec::u8_to_hwvec(raw) };
    // SAFETY: raw_ptr was allocated by libc::malloc inside hew_stream_next_sized.
    unsafe { libc::free(raw_ptr) };
    vec
}

/// Write a `bytes` value to a sink.
///
/// Extracts the raw byte content from the `HewVec` and writes it as a single
/// stream item.  Zero-length writes are forwarded — they are valid data, not
/// no-ops.  Does nothing only if `sink` or `data` is null.
///
/// # Safety
///
/// `sink` must be a valid sink pointer.  `data` must be a valid `HewVec`
/// pointer (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_sink_write_bytes(sink: *mut HewSink, data: *mut HewVec) {
    if sink.is_null() || data.is_null() {
        return;
    }
    // SAFETY: data is a valid HewVec per caller contract.
    let bytes = unsafe { hew_cabi::vec::hwvec_to_u8(data) };
    if bytes.is_empty() {
        // hew_sink_write short-circuits on size=0, but empty bytes are valid
        // data items that must be delivered.  Write directly to the backing.
        // SAFETY: sink is valid per caller contract.
        unsafe { (*sink).inner.write_item(&[]) };
    } else {
        // SAFETY: sink is valid; bytes slice is valid for its length.
        unsafe {
            hew_sink_write(sink, bytes.as_ptr().cast::<c_void>(), bytes.len());
        }
    }
}
