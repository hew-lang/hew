//! Sink ABI types shared between the Hew runtime and native packages.
//!
//! Native packages that produce `Sink` handles (e.g. HTTP streaming responses)
//! call [`into_sink_ptr`] or [`into_write_sink_ptr`] to create heap-allocated
//! opaque handles compatible with the runtime's C ABI.

use std::io::Write;

// ── Stream/sink error channel (single owner: hew-runtime) ─────────────────────
//
// The thread-local error state is OWNED by hew-runtime (linked into libhew.a)
// and exposed through the four `hew_stream_*` C ABI symbols declared below.
// hew-cabi declares them as imports and forwards through thin wrappers so that:
//
//   * native packages that statically link hew-cabi do NOT each emit their own
//     `#[no_mangle]` copy of these symbols — duplicate `#[no_mangle]`
//     definitions in two archives make the final link fail (the macOS linker
//     rejects them unconditionally); and
//   * a producer inside a separately-compiled package and the consumer in the
//     main program share ONE thread-local, so an error raised inside a package
//     is visible to `hew_stream_last_error` rather than silently lost.
//
// This mirrors the `hew_layout_*` / `hew_vec_*` idiom already used in this crate
// (see `map.rs`): the symbol lives in the runtime, hew-cabi only declares it.
// hew-cabi's own unit tests have no hew-runtime to link against, so a
// `#[cfg(test)]` stub at the bottom of this file defines these four symbols.

extern "C" {
    #[link_name = "hew_stream_set_last_error"]
    fn rt_set_last_error(ptr: *const u8, len: usize);
    #[link_name = "hew_stream_set_last_error_with_errno"]
    fn rt_set_last_error_with_errno(ptr: *const u8, len: usize, errno: i32);
    #[link_name = "hew_stream_last_error"]
    fn rt_last_error() -> *mut std::ffi::c_char;
    #[link_name = "hew_stream_last_errno"]
    fn rt_last_errno() -> i32;
}

/// Store an error message for the current thread. Retrievable via
/// [`hew_stream_last_error`]. The associated errno is cleared to 0.
///
/// Forwards to the runtime-owned thread-local through the `hew_stream_*` C ABI.
#[allow(
    clippy::needless_pass_by_value,
    reason = "stable hew_cabi::sink API: callers (std/net/http) hand off an owned \
              error message; the forwarder serialises it across the C ABI"
)]
pub fn set_last_error(msg: String) {
    let bytes = msg.as_bytes();
    // SAFETY: ptr/len describe `msg`'s valid UTF-8 bytes; the runtime copies them
    // into its thread-local and does not retain the pointer past the call.
    unsafe { rt_set_last_error(bytes.as_ptr(), bytes.len()) };
}

/// Store an error message together with its OS errno for the current thread.
/// Both are retrievable via [`hew_stream_last_error`] and [`hew_stream_last_errno`].
///
/// Forwards to the runtime-owned thread-local through the `hew_stream_*` C ABI.
#[allow(
    clippy::needless_pass_by_value,
    reason = "stable hew_cabi::sink API: callers hand off an owned error message; \
              the forwarder serialises it across the C ABI"
)]
pub fn set_last_error_with_errno(msg: String, errno: i32) {
    let bytes = msg.as_bytes();
    // SAFETY: ptr/len describe `msg`'s valid UTF-8 bytes; the runtime copies them
    // into its thread-local and does not retain the pointer past the call.
    unsafe { rt_set_last_error_with_errno(bytes.as_ptr(), bytes.len(), errno) };
}

/// Return the last stream/sink error as a header-aware C string, or NULL if none.
///
/// Clears both the error message and the associated errno after reading.
/// The returned string is allocated by the runtime via
/// [`crate::cabi::alloc_cstring_from_str`] (the header-aware path) and MUST be
/// released through `hew_string_drop` / [`crate::cabi::free_cstring`]. Bare
/// `libc::free` will abort.
///
/// Safe wrapper over the runtime-owned `hew_stream_last_error` C symbol; the
/// caller-owns-and-frees contract is unchanged from the previous in-crate export.
#[must_use]
pub fn hew_stream_last_error() -> *mut std::ffi::c_char {
    // SAFETY: the runtime export returns either NULL or a fresh header-aware
    // cstring allocation that becomes the caller's to free.
    unsafe { rt_last_error() }
}

/// Return the OS errno associated with the last stream/sink error, or 0 if none.
///
/// Clears the errno after reading. Safe wrapper over the runtime-owned
/// `hew_stream_last_errno` C symbol — independent of [`hew_stream_last_error`],
/// so callers may read errno without consuming the message, or vice-versa.
#[must_use]
pub fn hew_stream_last_errno() -> i32 {
    // SAFETY: the runtime export reads and clears a thread-local i32.
    unsafe { rt_last_errno() }
}

/// Take the last stream/sink error as an owned `String`, or `None` if none is set.
///
/// Compatibility wrapper preserving the pre-single-owner `hew_cabi::sink` API.
/// It forwards to the runtime-owned `hew_stream_last_error` C symbol, copies the
/// returned header-aware C string into an owned `String`, and releases the C
/// string through the matching [`crate::cabi::free_cstring`] path (a bare
/// `libc::free` would abort). Reading clears both the message and the errno, so
/// a subsequent call returns `None`.
#[must_use]
pub fn take_last_error() -> Option<String> {
    let ptr = hew_stream_last_error();
    if ptr.is_null() {
        return None;
    }
    // SAFETY: `ptr` is a fresh, NUL-terminated, header-aware cstring allocation
    // handed to us by the runtime getter. We copy its bytes into an owned String
    // (ending the borrow) before releasing the allocation below.
    let msg = unsafe { std::ffi::CStr::from_ptr(ptr) }
        .to_string_lossy()
        .into_owned();
    // SAFETY: `ptr` is the same non-null, header-aware allocation returned by the
    // runtime getter and has not been freed; releasing it through the matching
    // header-aware free path that pairs with `alloc_cstring_from_str` (a bare
    // `libc::free` would abort). The earlier borrow has ended.
    unsafe { crate::cabi::free_cstring(ptr) };
    Some(msg)
}

/// Take the OS errno associated with the last stream/sink error, or 0 if none.
///
/// Compatibility wrapper preserving the pre-single-owner `hew_cabi::sink` API.
/// Forwards to the runtime-owned `hew_stream_last_errno` C symbol and clears the
/// stored errno. Independent of [`take_last_error`].
#[must_use]
pub fn take_last_errno() -> i32 {
    hew_stream_last_errno()
}

// ── Sink handle ───────────────────────────────────────────────────────────────

trait SinkOps: Send {
    fn write_item(&mut self, data: &[u8]);
    /// Non-blocking write attempt. Returns `true` if the item was accepted,
    /// `false` if the backing buffer is full. The default delegates to
    /// `write_item` (always blocks, always returns `true`).
    fn try_write_item(&mut self, data: &[u8]) -> bool {
        self.write_item(data);
        true
    }
    fn flush(&mut self);
    fn close(&mut self);
}

struct CallbackSink<T> {
    backing: T,
    write_item: fn(&mut T, &[u8]),
    flush: fn(&mut T),
    close: fn(&mut T),
}

impl<T: Send> SinkOps for CallbackSink<T> {
    fn write_item(&mut self, data: &[u8]) {
        (self.write_item)(&mut self.backing, data);
    }

    fn flush(&mut self) {
        (self.flush)(&mut self.backing);
    }

    fn close(&mut self) {
        (self.close)(&mut self.backing);
    }
}

/// Opaque writable sink handle.
pub struct HewSink {
    inner: Option<Box<dyn SinkOps>>,
    /// Opaque borrow of the suspending channel core (`ChannelCore`) when this
    /// sink is the write half of an in-memory pipe (NEW-7). Null for every
    /// other sink kind. The runtime sets this so `hew_stream_await_send` can
    /// reach the shared queue + the parked-consumer wake without downcasting
    /// the `dyn SinkOps` backing. The pointer borrows the `Arc<ChannelCore>`
    /// owned by the backing, so it stays valid for the lifetime of the sink.
    channel_core: *const std::ffi::c_void,
}

impl std::fmt::Debug for HewSink {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewSink").finish_non_exhaustive()
    }
}

impl HewSink {
    /// Attach an opaque suspending-channel-core borrow (NEW-7). Called by the
    /// runtime pipe constructor after building the channel sink backing.
    pub fn set_channel_core(&mut self, core: *const std::ffi::c_void) {
        self.channel_core = core;
    }

    /// The opaque suspending-channel-core borrow, or null when this sink is not
    /// the write half of an in-memory pipe.
    #[must_use]
    pub fn channel_core_ptr(&self) -> *const std::ffi::c_void {
        self.channel_core
    }

    /// Write one item (exact bytes). Blocks if the backing buffer is full.
    pub fn write_item(&mut self, data: &[u8]) {
        let Some(inner) = self.inner.as_mut() else {
            return;
        };
        inner.write_item(data);
    }

    /// Attempt to write one item without blocking.
    ///
    /// Returns `true` if the item was accepted; `false` if the backing buffer
    /// was full and the write would have blocked. For non-channel sinks the
    /// default behaviour is to block (delegating to `write_item`) and return
    /// `true` — callers that need genuine non-blocking semantics should create
    /// the sink with [`into_channel_sink_ptr`].
    pub fn try_write_item(&mut self, data: &[u8]) -> bool {
        let Some(inner) = self.inner.as_mut() else {
            return true; // closed sink: item silently discarded, not "full"
        };
        inner.try_write_item(data)
    }

    /// Flush any buffered writes (file sinks).
    pub fn flush(&mut self) {
        let Some(inner) = self.inner.as_mut() else {
            return;
        };
        inner.flush();
    }

    /// Signal EOF to the reader and release the backing resource.
    pub fn close(&mut self) {
        // Drop the suspending-channel-core borrow BEFORE releasing the backing:
        // closing dethrones this sink's `Arc<ChannelCore>` clone, so the raw
        // borrow could dangle once the peer (stream) half is also dropped.
        // Nulling it forces a later `hew_stream_await_send` onto the closed-sink
        // (fail-closed) path instead of dereferencing a possibly-freed core.
        self.channel_core = std::ptr::null();
        let Some(mut inner) = self.inner.take() else {
            return;
        };
        inner.close();
    }
}

impl Drop for HewSink {
    fn drop(&mut self) {
        self.close();
    }
}

/// Create a heap-allocated [`HewSink`] from custom sink callbacks.
pub fn into_sink_ptr<T: Send + 'static>(
    backing: T,
    write_item: fn(&mut T, &[u8]),
    flush: fn(&mut T),
    close: fn(&mut T),
) -> *mut HewSink {
    Box::into_raw(Box::new(HewSink {
        // ALLOCATOR-PAIRING: GlobalAlloc
        inner: Some(Box::new(CallbackSink {
            backing,
            write_item,
            flush,
            close,
        })),
        channel_core: std::ptr::null(),
    }))
}

fn write_via_write<W: Write>(backing: &mut W, data: &[u8]) {
    let result = if data.is_empty() {
        backing.write(data).map(|_| ())
    } else {
        backing.write_all(data)
    };
    if let Err(err) = result {
        set_last_error(format!("hew_sink_write: {err}"));
    }
}

fn flush_via_write<W: Write>(backing: &mut W) {
    if let Err(err) = backing.flush() {
        set_last_error(format!("hew_sink_flush: {err}"));
    }
}

fn close_via_write<W: Write>(backing: &mut W) {
    if let Err(err) = backing.flush() {
        set_last_error(format!("hew_sink_close: {err}"));
    }
}

/// Create a heap-allocated [`HewSink`] from a [`Write`] implementation.
pub fn into_write_sink_ptr(backing: impl Write + Send + 'static) -> *mut HewSink {
    Box::into_raw(Box::new(HewSink {
        // ALLOCATOR-PAIRING: GlobalAlloc
        inner: Some(Box::new(CallbackSink {
            backing,
            write_item: write_via_write::<_>,
            flush: flush_via_write::<_>,
            close: close_via_write::<_>,
        })),
        channel_core: std::ptr::null(),
    }))
}

// ── Channel-backed sink ───────────────────────────────────────────────────────

/// Channel sink: wraps an `mpsc::SyncSender` and provides genuine non-blocking
/// `try_write_item` semantics (returns `false` when the queue is at capacity).
struct ChannelSinkBacking {
    tx: std::sync::mpsc::SyncSender<Vec<u8>>,
}

// SAFETY: mpsc::SyncSender<Vec<u8>> is Send.
unsafe impl Send for ChannelSinkBacking {}

impl SinkOps for ChannelSinkBacking {
    /// Blocking send. Waits until space is available in the bounded channel.
    fn write_item(&mut self, data: &[u8]) {
        // Errors mean the receiver is gone; treat as a silent discard so that
        // pipeline tear-down does not propagate spurious errors.
        let _ = self.tx.send(data.to_vec());
    }

    /// Non-blocking send. Returns `false` when the channel queue is full.
    fn try_write_item(&mut self, data: &[u8]) -> bool {
        use std::sync::mpsc::TrySendError;
        match self.tx.try_send(data.to_vec()) {
            // Disconnected: receiver is gone; item silently discarded.
            // Report success so callers don't mistake a closed channel for a full one.
            Ok(()) | Err(TrySendError::Disconnected(_)) => true,
            Err(TrySendError::Full(_)) => false,
        }
    }

    fn flush(&mut self) {
        // mpsc channels are not buffered on the send side; flush is a no-op.
    }

    fn close(&mut self) {
        // Dropping the SyncSender signals EOF to the receiver; nothing explicit needed.
    }
}

/// Create a heap-allocated [`HewSink`] backed by a bounded `mpsc` channel sender.
///
/// Unlike [`into_write_sink_ptr`], the returned sink supports genuine non-blocking
/// writes via [`HewSink::try_write_item`]: the call returns `false` immediately if
/// the channel queue is at capacity rather than blocking until space is available.
///
/// # Ownership
///
/// The caller transfers ownership of `tx`. The sink owns the sender until it is
/// closed or dropped, at which point the sender is released and the paired receiver
/// observes EOF.
#[must_use]
pub fn into_channel_sink_ptr(tx: std::sync::mpsc::SyncSender<Vec<u8>>) -> *mut HewSink {
    Box::into_raw(Box::new(HewSink {
        // ALLOCATOR-PAIRING: GlobalAlloc
        inner: Some(Box::new(ChannelSinkBacking { tx })),
        channel_core: std::ptr::null(),
    }))
}

/// Standalone definitions of hew-runtime's stream-error C ABI, for hew-cabi's
/// own unit tests only.
///
/// In a real link the four `hew_stream_*` symbols come from hew-runtime
/// (libhew.a) and hew-cabi merely declares them (see the `extern "C"` block near
/// the top of this file). But hew-cabi's test binary has no hew-runtime to link
/// against, so these `#[no_mangle]` definitions stand in. They mirror the runtime
/// implementation — a thread-local message + errno with header-aware cstring
/// allocation — so any test exercising the forwarders observes identical
/// behaviour. Compiled ONLY under `cfg(test)`; never present in shipped archives.
#[cfg(test)]
mod runtime_abi_stub {
    use std::cell::RefCell;
    use std::ffi::c_char;

    thread_local! {
        static LAST_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
        static LAST_ERRNO: RefCell<i32> = const { RefCell::new(0) };
    }

    fn store(ptr: *const u8, len: usize, errno: i32) {
        let msg = if ptr.is_null() || len == 0 {
            String::new()
        } else {
            // SAFETY: the forwarders pass a valid ptr/len pair borrowed from a
            // live `String` for the duration of the call.
            let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
            String::from_utf8_lossy(slice).into_owned()
        };
        LAST_ERROR.with(|e| *e.borrow_mut() = Some(msg));
        LAST_ERRNO.with(|e| *e.borrow_mut() = errno);
    }

    /// # Safety
    /// `ptr`/`len` must describe a valid initialized byte range, or `ptr` may be
    /// null with `len` 0.
    #[no_mangle]
    unsafe extern "C" fn hew_stream_set_last_error(ptr: *const u8, len: usize) {
        store(ptr, len, 0);
    }

    /// # Safety
    /// As [`hew_stream_set_last_error`]; `errno` is stored verbatim.
    #[no_mangle]
    unsafe extern "C" fn hew_stream_set_last_error_with_errno(
        ptr: *const u8,
        len: usize,
        errno: i32,
    ) {
        store(ptr, len, errno);
    }

    #[no_mangle]
    extern "C" fn hew_stream_last_error() -> *mut c_char {
        LAST_ERRNO.with(|e| *e.borrow_mut() = 0);
        match LAST_ERROR.with(|e| e.borrow_mut().take()) {
            Some(m) => {
                let safe: &str = if m.contains('\0') {
                    "hew_stream_last_error: stored error message contained interior NUL"
                } else {
                    &m
                };
                crate::cabi::alloc_cstring_from_str(safe)
            }
            None => std::ptr::null_mut(),
        }
    }

    #[no_mangle]
    extern "C" fn hew_stream_last_errno() -> i32 {
        LAST_ERRNO.with(|e| {
            let v = *e.borrow();
            *e.borrow_mut() = 0;
            v
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    /// Proves the OLD allocator path (raw `libc::malloc`, no header) causes `free_cstring`
    /// to abort. Run in a subprocess so the SIGABRT does not take down the test runner.
    ///
    /// This is the falsification probe: it demonstrates that the fix is not
    /// redundant and that the previous raw-malloc path was the actual hazard.
    #[test]
    fn free_cstring_aborts_on_raw_malloc_provenance() {
        if std::env::var("HEW_SINK_RUN_ABORT_PROBE").is_ok() {
            // Simulate the OLD hew_stream_last_error allocator: a raw libc::malloc
            // result with no header sentinel, as free_cstring expects.
            let msg = b"simulated old-path error\0";
            // SAFETY: allocating len bytes via raw malloc, exactly as the old code did.
            let ptr = unsafe { libc::malloc(msg.len()) }.cast::<std::ffi::c_char>();
            assert!(!ptr.is_null());
            // SAFETY: ptr is freshly allocated with msg.len() bytes.
            unsafe { std::ptr::copy_nonoverlapping(msg.as_ptr(), ptr.cast::<u8>(), msg.len()) };
            // free_cstring will read base = ptr - CSTRING_HEADER_SIZE, find no magic
            // sentinel, print a diagnostic, and call libc::abort. This must not return.
            // SAFETY: ptr was just malloc'd; we are intentionally passing a headerless
            // pointer to prove free_cstring detects the mismatch and aborts.
            unsafe { crate::cabi::free_cstring(ptr) };
            // If we reach here the abort did NOT fire — exit cleanly so the parent fails.
            std::process::exit(0);
        }

        let exe = std::env::current_exe().expect("current_exe");
        let status = std::process::Command::new(exe)
            .args([
                "--exact",
                "sink::tests::free_cstring_aborts_on_raw_malloc_provenance",
            ])
            .env("HEW_SINK_RUN_ABORT_PROBE", "1")
            .env("RUST_BACKTRACE", "0")
            .output()
            .expect("spawn abort probe");
        assert!(
            !status.status.success(),
            "free_cstring on a raw-malloc (headerless) pointer must abort (subprocess \
             should not exit 0); stdout={:?} stderr={:?}",
            String::from_utf8_lossy(&status.stdout),
            String::from_utf8_lossy(&status.stderr),
        );
    }

    // ── Stream/sink error channel forwarders ────────────────────────────

    /// `take_last_error` returns `None` when no error has been recorded.
    #[test]
    fn take_last_error_none_when_unset() {
        let _ = take_last_error(); // clear any residual thread-local state
        assert_eq!(take_last_error(), None);
    }

    /// A message stored through `set_last_error` round-trips back through the
    /// runtime-owned C getter and the compat `take_last_error`, then clears.
    #[test]
    fn set_then_take_last_error_round_trips_and_clears() {
        let _ = take_last_error();
        set_last_error("disk offline".to_string());
        assert_eq!(take_last_error().as_deref(), Some("disk offline"));
        assert_eq!(take_last_error(), None, "second take must be None");
    }

    /// `set_last_error_with_errno` stores both fields; the errno is readable via
    /// `take_last_errno` independently, and `take_last_error` yields the message.
    #[test]
    fn set_with_errno_then_take_both() {
        let _ = take_last_error();
        set_last_error_with_errno("connection reset".to_string(), 54);
        assert_eq!(take_last_errno(), 54);
        assert_eq!(take_last_error().as_deref(), Some("connection reset"));
    }

    /// The compat `take_last_error` frees the header-aware C string handed back by
    /// the getter through `free_cstring`; an allocator-pairing mismatch would
    /// abort the process here rather than return.
    #[test]
    fn take_last_error_frees_cstring_without_abort() {
        let _ = take_last_error();
        set_last_error("freed cleanly".to_string());
        assert_eq!(take_last_error().as_deref(), Some("freed cleanly"));
    }

    // ── HewSink / into_sink_ptr / into_write_sink_ptr ───────────────────

    /// Test double that records calls to `write_item`, flush, and close.
    #[derive(Debug)]
    struct MockSink {
        written: Arc<std::sync::Mutex<Vec<Vec<u8>>>>,
        flush_count: Arc<AtomicUsize>,
        close_count: Arc<AtomicUsize>,
    }

    #[derive(Debug)]
    struct RawSink {
        written: Arc<std::sync::Mutex<Vec<Vec<u8>>>>,
        close_count: Arc<AtomicUsize>,
    }

    impl MockSink {
        #[expect(
            clippy::type_complexity,
            reason = "Test helper return type; splitting would reduce clarity"
        )]
        fn new() -> (
            Self,
            Arc<std::sync::Mutex<Vec<Vec<u8>>>>,
            Arc<AtomicUsize>,
            Arc<AtomicUsize>,
        ) {
            let written = Arc::new(std::sync::Mutex::new(Vec::new()));
            let flush_count = Arc::new(AtomicUsize::new(0));
            let close_count = Arc::new(AtomicUsize::new(0));
            (
                Self {
                    written: Arc::clone(&written),
                    flush_count: Arc::clone(&flush_count),
                    close_count: Arc::clone(&close_count),
                },
                written,
                flush_count,
                close_count,
            )
        }
    }

    impl Write for MockSink {
        fn write(&mut self, data: &[u8]) -> std::io::Result<usize> {
            self.written.lock().unwrap().push(data.to_vec());
            Ok(data.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            self.flush_count.fetch_add(1, Ordering::SeqCst);
            Ok(())
        }
    }

    impl Drop for MockSink {
        fn drop(&mut self) {
            self.close_count.fetch_add(1, Ordering::SeqCst);
        }
    }

    #[test]
    fn hew_sink_write_flush_close() {
        let (mock, written, flush_count, close_count) = MockSink::new();
        // SAFETY: into_write_sink_ptr returns a Box::into_raw allocation owned by this test.
        let mut sink = unsafe { Box::from_raw(into_write_sink_ptr(mock)) }; // ALLOCATOR-PAIRING: GlobalAlloc
        sink.write_item(b"hello");
        sink.write_item(b"world");
        sink.flush();
        sink.close();

        let items = written.lock().unwrap();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0], b"hello");
        assert_eq!(items[1], b"world");
        assert_eq!(flush_count.load(Ordering::SeqCst), 2);
        assert_eq!(close_count.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn hew_sink_drop_calls_close() {
        let (mock, _written, _flush_count, close_count) = MockSink::new();
        {
            // SAFETY: into_write_sink_ptr returns a Box::into_raw allocation owned by this test.
            let _sink = unsafe { Box::from_raw(into_write_sink_ptr(mock)) }; // ALLOCATOR-PAIRING: GlobalAlloc
            assert_eq!(close_count.load(Ordering::SeqCst), 0);
            // _sink is dropped here.
        }
        assert_eq!(
            close_count.load(Ordering::SeqCst),
            1,
            "Drop must call close"
        );
    }

    #[test]
    fn into_sink_ptr_returns_valid_heap_allocation() {
        let (mock, written, _flush_count, close_count) = MockSink::new();
        let ptr = into_write_sink_ptr(mock);
        assert!(!ptr.is_null());

        // SAFETY: ptr was just created by into_sink_ptr via Box::into_raw.
        unsafe {
            (*ptr).write_item(b"via pointer");
            // Reclaim and drop to free memory and trigger close.
            let _ = Box::from_raw(ptr); // ALLOCATOR-PAIRING: GlobalAlloc
        }

        let items = written.lock().unwrap();
        assert_eq!(items.len(), 1);
        assert_eq!(items[0], b"via pointer");
        assert_eq!(
            close_count.load(Ordering::SeqCst),
            1,
            "Drop via Box::from_raw should call close"
        );
    }

    #[test]
    fn into_sink_ptr_write_empty_data() {
        let (mock, written, _flush_count, _close_count) = MockSink::new();
        let ptr = into_write_sink_ptr(mock);
        // SAFETY: ptr was just created by into_sink_ptr.
        unsafe {
            (*ptr).write_item(b"");
            let _ = Box::from_raw(ptr); // ALLOCATOR-PAIRING: GlobalAlloc
        }
        let items = written.lock().unwrap();
        assert_eq!(items.len(), 1);
        assert!(items[0].is_empty(), "empty write should produce empty vec");
    }

    #[test]
    fn close_clears_channel_core_ptr_to_remove_stale_deref_window() {
        let (mock, _written, _flush_count, _close_count) = MockSink::new();
        let ptr = into_write_sink_ptr(mock);
        // SAFETY: ptr was just created by into_write_sink_ptr.
        unsafe {
            // Simulate the pipe constructor attaching a channel-core borrow.
            let fake_core = 0xdead_beef_usize as *const std::ffi::c_void;
            (*ptr).set_channel_core(fake_core);
            assert_eq!((*ptr).channel_core_ptr(), fake_core);
            // close() must null the borrow so a later suspending send cannot
            // dereference a (now possibly stale) channel core.
            (*ptr).close();
            assert!(
                (*ptr).channel_core_ptr().is_null(),
                "close() must clear the channel_core borrow (no stale deref)"
            );
            let _ = Box::from_raw(ptr); // ALLOCATOR-PAIRING: GlobalAlloc
        }
    }

    #[test]
    fn into_sink_ptr_custom_callbacks_preserve_item_boundaries() {
        let written = Arc::new(std::sync::Mutex::new(Vec::new()));
        let close_count = Arc::new(AtomicUsize::new(0));

        let ptr = into_sink_ptr(
            RawSink {
                written: Arc::clone(&written),
                close_count: Arc::clone(&close_count),
            },
            |sink, data| sink.written.lock().unwrap().push(data.to_vec()),
            |_sink| {},
            |sink| {
                sink.close_count.fetch_add(1, Ordering::SeqCst);
            },
        );

        // SAFETY: ptr was created by into_sink_ptr above.
        unsafe {
            (*ptr).write_item(b"abc");
            let _ = Box::from_raw(ptr); // ALLOCATOR-PAIRING: GlobalAlloc
        }

        let items = written.lock().unwrap();
        assert_eq!(items.as_slice(), &[b"abc".to_vec()]);
        assert_eq!(close_count.load(Ordering::SeqCst), 1);
    }
}
