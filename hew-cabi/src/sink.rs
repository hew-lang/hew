//! Sink ABI types shared between the Hew runtime and native packages.
//!
//! Native packages that produce `Sink` handles (e.g. HTTP streaming responses)
//! call [`into_sink_ptr`] or [`into_write_sink_ptr`] to create heap-allocated
//! opaque handles compatible with the runtime's C ABI.

use std::io::Write;

// ── Thread-local error for fallible stream/sink operations ────────────────────

std::thread_local! {
    static LAST_ERROR: std::cell::RefCell<Option<String>> = const { std::cell::RefCell::new(None) };
    /// OS errno associated with the last error, or 0 when not set.
    static LAST_ERRNO: std::cell::RefCell<i32> = const { std::cell::RefCell::new(0) };
}

/// Store an error message for the current thread. Retrievable via
/// [`hew_stream_last_error`]. The associated errno is cleared to 0.
pub fn set_last_error(msg: String) {
    LAST_ERROR.with(|e| *e.borrow_mut() = Some(msg));
    LAST_ERRNO.with(|e| *e.borrow_mut() = 0);
}

/// Store an error message together with its OS errno for the current thread.
/// Both are retrievable via [`hew_stream_last_error`] and [`hew_stream_last_errno`].
pub fn set_last_error_with_errno(msg: String, errno: i32) {
    LAST_ERROR.with(|e| *e.borrow_mut() = Some(msg));
    LAST_ERRNO.with(|e| *e.borrow_mut() = errno);
}

/// Take and clear the last error, if any. Also clears the associated errno.
#[must_use]
pub fn take_last_error() -> Option<String> {
    LAST_ERRNO.with(|e| *e.borrow_mut() = 0);
    LAST_ERROR.with(|e| e.borrow_mut().take())
}

/// Take and clear the last errno, if any. Returns 0 when none was set.
#[must_use]
pub fn take_last_errno() -> i32 {
    LAST_ERRNO.with(|e| {
        let v = *e.borrow();
        *e.borrow_mut() = 0;
        v
    })
}

/// Return the last stream/sink error as a header-aware C string, or NULL if none.
///
/// Clears both the error message and the associated errno after reading.
/// The returned string is allocated via [`crate::cabi::alloc_cstring_from_str`]
/// (the header-aware path) and MUST be released through `hew_string_drop` /
/// [`crate::cabi::free_cstring`]. Bare `libc::free` will abort.
///
/// ALLOCATOR-PAIRING: cstring (`alloc_cstring_from_str` — header-aware; must be
/// freed via `free_cstring` / `hew_string_drop`, never bare `libc::free`).
#[no_mangle]
pub extern "C" fn hew_stream_last_error() -> *mut std::ffi::c_char {
    match take_last_error() {
        Some(msg) => {
            // Use the header-aware allocator so that the Hew drop spine's
            // hew_string_drop → free_cstring can verify the magic sentinel and
            // release the allocation without aborting.  If the message contains
            // interior NUL bytes (rare: OS errors never do, but defensive), fall
            // back to a safe diagnostic string before allocating.
            let safe_msg: &str = if msg.contains('\0') {
                "hew_stream_last_error: stored error message contained interior NUL"
            } else {
                &msg
            };
            let ptr = crate::cabi::alloc_cstring_from_str(safe_msg); // ALLOCATOR-PAIRING: cstring
            if ptr.is_null() {
                set_last_error("hew_stream_last_error: allocation failed".to_string());
                return std::ptr::null_mut();
            }
            ptr
        }
        None => std::ptr::null_mut(),
    }
}

/// Return the OS errno associated with the last stream/sink error, or 0 if none.
///
/// Clears the errno after reading. This export is intentionally independent of
/// [`hew_stream_last_error`] — callers may read errno without consuming the message,
/// or vice-versa, though typical usage reads both.
///
/// INTERNAL-ABI: populated by `set_last_error_with_errno` at runtime call sites where
/// a Rust `io::Error` is in scope. String-only errors (no OS errno) leave this as 0.
#[no_mangle]
pub extern "C" fn hew_stream_last_errno() -> i32 {
    take_last_errno()
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CStr;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    // ── Thread-local error helpers ───────────────────────────────────────

    #[test]
    fn take_last_error_returns_none_when_empty() {
        // Clear any residual state from other tests in this thread.
        let _ = take_last_error();
        assert_eq!(take_last_error(), None);
    }

    #[test]
    fn set_and_take_roundtrip() {
        set_last_error("something went wrong".to_string());
        let err = take_last_error();
        assert_eq!(err.as_deref(), Some("something went wrong"));
    }

    #[test]
    fn take_clears_the_error() {
        set_last_error("first".to_string());
        let _ = take_last_error();
        assert_eq!(take_last_error(), None, "second take should be None");
    }

    #[test]
    fn set_overwrites_previous_error() {
        set_last_error("old".to_string());
        set_last_error("new".to_string());
        assert_eq!(take_last_error().as_deref(), Some("new"));
    }

    #[test]
    fn set_last_error_empty_string() {
        set_last_error(String::new());
        assert_eq!(take_last_error().as_deref(), Some(""));
    }

    // ── hew_stream_last_error (C ABI) ────────────────────────────────────

    #[test]
    fn hew_stream_last_error_returns_null_when_no_error() {
        // Clear any residual state.
        let _ = take_last_error();
        let ptr = hew_stream_last_error();
        assert!(ptr.is_null());
    }

    #[test]
    fn hew_stream_last_error_returns_valid_cstring() {
        set_last_error("connection refused".to_string());
        let ptr = hew_stream_last_error();
        assert!(!ptr.is_null());
        // SAFETY: ptr is a freshly alloc_cstring_from_str (header-aware) allocation.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, "connection refused");
            crate::cabi::free_cstring(ptr); // CSTRING-FREE: str-open (hew_stream_last_error via alloc_cstring_from_str)
        }
    }

    #[test]
    fn hew_stream_last_error_clears_after_read() {
        set_last_error("timeout".to_string());
        let ptr = hew_stream_last_error();
        assert!(!ptr.is_null());
        // SAFETY: ptr is a header-aware alloc_cstring_from_str allocation.
        unsafe { crate::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open
                                                   // Second call should return null — error was consumed.
        let ptr2 = hew_stream_last_error();
        assert!(ptr2.is_null(), "error should be cleared after first read");
    }

    #[test]
    fn hew_stream_last_error_handles_unicode() {
        set_last_error("échec de connexion 🔥".to_string());
        let ptr = hew_stream_last_error();
        assert!(!ptr.is_null());
        // SAFETY: ptr is a header-aware alloc_cstring_from_str allocation.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, "échec de connexion 🔥");
            crate::cabi::free_cstring(ptr); // CSTRING-FREE: str-open (hew_stream_last_error via alloc_cstring_from_str)
        }
    }

    #[test]
    fn hew_stream_last_error_surfaces_interior_nul_diagnostic() {
        set_last_error("bad\0message".to_string());
        let ptr = hew_stream_last_error();
        assert!(!ptr.is_null());
        // SAFETY: ptr is a header-aware alloc_cstring_from_str allocation.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert!(recovered.contains("contained interior NUL"));
            crate::cabi::free_cstring(ptr); // CSTRING-FREE: str-open (hew_stream_last_error via alloc_cstring_from_str)
        }
    }

    // ── hew_stream_last_errno (C ABI) ────────────────────────────────────

    #[test]
    fn hew_stream_last_errno_returns_zero_when_not_set() {
        // Clear any residual state.
        let _ = take_last_error();
        let _ = take_last_errno();
        assert_eq!(hew_stream_last_errno(), 0);
    }

    #[test]
    fn set_last_error_with_errno_roundtrip_via_abi() {
        set_last_error_with_errno("connection refused".to_string(), 111);
        assert_eq!(hew_stream_last_errno(), 111);
        // Consuming errno does not consume the message.
        let ptr = hew_stream_last_error();
        assert!(!ptr.is_null());
        // SAFETY: ptr is a header-aware alloc_cstring_from_str allocation.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, "connection refused");
            crate::cabi::free_cstring(ptr); // CSTRING-FREE: str-open (hew_stream_last_error via alloc_cstring_from_str)
        }
    }

    #[test]
    fn hew_stream_last_errno_clears_after_read() {
        set_last_error_with_errno("some error".to_string(), 42);
        // Consume via C ABI.
        let first = hew_stream_last_errno();
        assert_eq!(first, 42);
        // Second read must return 0 — errno was cleared.
        assert_eq!(
            hew_stream_last_errno(),
            0,
            "errno must be cleared after first read"
        );
        // Clean up the message side.
        let ptr = hew_stream_last_error();
        if !ptr.is_null() {
            // SAFETY: ptr is a header-aware alloc_cstring_from_str allocation.
            unsafe { crate::cabi::free_cstring(ptr) }; // CSTRING-FREE: str-open (hew_stream_last_error via alloc_cstring_from_str)
        }
    }

    #[test]
    fn set_last_error_plain_clears_errno() {
        // Pre-load an errno.
        set_last_error_with_errno("original".to_string(), 99);
        // Overwrite with plain set_last_error — errno must reset to 0.
        set_last_error("replacement".to_string());
        assert_eq!(
            take_last_errno(),
            0,
            "plain set_last_error must clear errno to 0"
        );
        // Clean up the message.
        let _ = take_last_error();
    }

    #[test]
    fn errno_and_message_are_independent_paths() {
        // Set with errno, consume errno first, message should still be present.
        set_last_error_with_errno("disk full".to_string(), 28);
        let errno = take_last_errno();
        assert_eq!(errno, 28);
        // Message should survive the errno read.
        let msg = take_last_error();
        assert_eq!(msg.as_deref(), Some("disk full"));
        // After take_last_error the errno path is also cleared.
        assert_eq!(take_last_errno(), 0);
    }

    // ── free_cstring roundtrip: hew_stream_last_error allocator contract ──
    //
    // These tests act as the Hew drop spine: they call hew_stream_last_error and
    // release the result via free_cstring (exactly what hew_string_drop does at
    // runtime). They assert no abort/leak, proving the allocator pairing is correct.
    //
    // The companion subprocess test proves that a raw libc::malloc pointer — the
    // OLD allocator path — would cause free_cstring to abort, confirming the fix
    // is load-bearing and not redundant.

    /// Drop-spine simulation: `hew_stream_last_error` + `free_cstring` roundtrip.
    ///
    /// Calls `hew_stream_last_error`, reads the result, and releases it via
    /// `free_cstring` (as `hew_string_drop` does). If `hew_stream_last_error` still
    /// used raw `libc::malloc` this would abort on the missing header sentinel.
    #[test]
    fn hew_stream_last_error_free_cstring_roundtrip() {
        set_last_error("connection reset by peer".to_string());
        let ptr = hew_stream_last_error();
        assert!(
            !ptr.is_null(),
            "error must be non-null after set_last_error"
        );
        // SAFETY: ptr was produced by hew_stream_last_error via alloc_cstring_from_str
        // (header-aware). Reading it as a C string and releasing via free_cstring
        // both satisfy the header-aware precondition.
        unsafe {
            let msg = std::ffi::CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(msg, "connection reset by peer");
            // This is the exact call the Hew drop spine makes via hew_string_drop.
            // With the OLD raw libc::malloc allocator, free_cstring would read
            // 16 bytes before the allocation for the header magic sentinel, find
            // garbage, and call libc::abort. With alloc_cstring_from_str the
            // sentinel is present and this returns cleanly.
            crate::cabi::free_cstring(ptr); // CSTRING-FREE: str-open
        }
        // Error was consumed; a second call must return null.
        assert!(hew_stream_last_error().is_null());
    }

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
