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

pub use hew_cabi::sink::{
    into_sink_ptr, into_write_sink_ptr, set_last_error, set_last_error_with_errno, take_last_error,
    HewSink,
};

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

// HewSink is defined in hew_cabi::sink and re-exported above.

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

impl Write for ChannelSink {
    fn write(&mut self, data: &[u8]) -> std::io::Result<usize> {
        self.tx.send(data.to_vec()).map_err(|_| {
            std::io::Error::new(
                std::io::ErrorKind::BrokenPipe,
                "failed to send to channel sink",
            )
        })?;
        Ok(data.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
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
        inner: Box::new(backing),
        closed: false,
    }))
}

/// Like [`into_stream_ptr`] but accepts an already-boxed backing.
fn into_stream_ptr_dyn(backing: Box<dyn StreamBacking>) -> *mut HewStream {
    Box::into_raw(Box::new(HewStream {
        inner: backing,
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
    let cap = usize::try_from(capacity.max(1)).unwrap_or(1);
    let (tx, rx) = mpsc::sync_channel::<Item>(cap);

    let stream_ptr = into_stream_ptr(ChannelStream { rx });
    let sink_ptr = into_write_sink_ptr(ChannelSink { tx });

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
            // used as a C string by the generic hew_print_value string path.
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
        k.write_item(&item);
    }
    k.close();

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
        unsafe { (*sink).write_item(&[]) };
    } else {
        // SAFETY: sink is valid; bytes slice is valid for its length.
        unsafe {
            hew_sink_write(sink, bytes.as_ptr().cast::<c_void>(), bytes.len());
        }
    }
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
            unsafe { libc::free(ptr) };
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

    #[test]
    fn stream_next_null_returns_null() {
        // SAFETY: null is explicitly handled by cabi_guard.
        let result = unsafe { hew_stream_next(ptr::null_mut()) };
        assert!(result.is_null());
    }

    #[test]
    fn stream_next_returns_nul_terminated_buffer() {
        let data = b"test";
        // SAFETY: data is valid; stream is created from it.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let buf = hew_stream_next(stream);
            assert!(!buf.is_null());
            // The buffer should be NUL-terminated for C string compatibility.
            let cstr = CStr::from_ptr(buf.cast::<c_char>());
            assert_eq!(cstr.to_bytes(), b"test");
            libc::free(buf);
            hew_stream_close(stream);
        }
    }

    #[test]
    fn stream_next_eof_returns_null() {
        let data = b"one";
        // SAFETY: data is valid; stream is created from it.
        unsafe {
            let stream = hew_stream_from_bytes(data.as_ptr(), data.len(), 0);
            let first = hew_stream_next(stream);
            assert!(!first.is_null());
            libc::free(first);
            // Stream is now exhausted.
            let second = hew_stream_next(stream);
            assert!(second.is_null(), "should return null on EOF");
            hew_stream_close(stream);
        }
    }

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
            libc::free(buf);
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
            let mut buf: *mut u8 = libc::malloc(64).cast::<u8>();
            let mut cap: usize = 64;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, 5);
            assert_eq!(cap, 64, "capacity unchanged when buffer is large enough");
            let n = usize::try_from(ret).unwrap();
            let slice = std::slice::from_raw_parts(buf, n);
            assert_eq!(slice, b"hello");
            libc::free(buf.cast());
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
            let mut buf: *mut u8 = libc::malloc(2).cast::<u8>();
            let mut cap: usize = 2;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, i64::try_from(data.len()).unwrap());
            assert!(cap >= data.len(), "capacity must grow to fit the item");
            let n = usize::try_from(ret).unwrap();
            let slice = std::slice::from_raw_parts(buf, n);
            assert_eq!(slice, data);
            libc::free(buf.cast());
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
            libc::free(buf.cast());
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
            libc::free(buf.cast());
            hew_stream_close(stream);
        }
    }

    #[test]
    fn stream_next_view_eof_returns_negative_one() {
        // SAFETY: empty stream hits EOF immediately.
        unsafe {
            let stream = hew_stream_from_bytes(ptr::null(), 0, 0);
            let mut buf: *mut u8 = libc::malloc(16).cast::<u8>();
            let mut cap: usize = 16;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, -1, "EOF must return -1");
            libc::free(buf.cast());
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
            let mut buf: *mut u8 = libc::malloc(4).cast::<u8>();
            let mut cap: usize = 4;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, 4);
            assert_eq!(cap, 4, "capacity unchanged on exact fit");
            let slice = std::slice::from_raw_parts(buf, 4);
            assert_eq!(slice, b"ABCD");
            libc::free(buf.cast());
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

            let mut buf: *mut u8 = libc::malloc(64).cast::<u8>();
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

            libc::free(buf.cast());
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
            let mut buf: *mut u8 = libc::malloc(1).cast::<u8>();
            let mut cap: usize = 1;
            let ret = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(ret, 1);
            assert_eq!(*buf, b'X');
            let eof = hew_stream_next_view(stream, &raw mut buf, &raw mut cap);
            assert_eq!(eof, -1, "EOF after the single item");
            libc::free(buf.cast());
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
        let err = hew_cabi::sink::take_last_error();
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
            libc::free(cstr_ptr.cast());
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
            libc::free(cstr_ptr.cast());
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
            // Drain the stream.
            let buf = hew_stream_next(stream);
            libc::free(buf);
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
}
