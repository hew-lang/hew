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

/// Return the last stream/sink error as a malloc'd C string, or NULL if none.
///
/// Clears both the error message and the associated errno after reading.
/// Callers must free the returned string.
#[no_mangle]
pub extern "C" fn hew_stream_last_error() -> *mut std::ffi::c_char {
    match take_last_error() {
        Some(msg) => {
            let c = std::ffi::CString::new(msg).unwrap_or_default();
            let len = c.as_bytes_with_nul().len();
            // SAFETY: allocating len bytes via malloc.
            let ptr = unsafe { libc::malloc(len) }.cast::<std::ffi::c_char>();
            if ptr.is_null() {
                return std::ptr::null_mut();
            }
            // SAFETY: ptr is freshly allocated with at least len bytes.
            unsafe { std::ptr::copy_nonoverlapping(c.as_ptr(), ptr, len) };
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
}

impl std::fmt::Debug for HewSink {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewSink").finish_non_exhaustive()
    }
}

impl HewSink {
    /// Write one item (exact bytes). Blocks if the backing buffer is full.
    pub fn write_item(&mut self, data: &[u8]) {
        let Some(inner) = self.inner.as_mut() else {
            return;
        };
        inner.write_item(data);
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
        inner: Some(Box::new(CallbackSink {
            backing,
            write_item,
            flush,
            close,
        })),
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
        inner: Some(Box::new(CallbackSink {
            backing,
            write_item: write_via_write::<_>,
            flush: flush_via_write::<_>,
            close: close_via_write::<_>,
        })),
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
        // SAFETY: ptr is a freshly malloc'd NUL-terminated string.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, "connection refused");
            libc::free(ptr.cast());
        }
    }

    #[test]
    fn hew_stream_last_error_clears_after_read() {
        set_last_error("timeout".to_string());
        let ptr = hew_stream_last_error();
        assert!(!ptr.is_null());
        // SAFETY: ptr was malloc'd above.
        unsafe { libc::free(ptr.cast()) };
        // Second call should return null — error was consumed.
        let ptr2 = hew_stream_last_error();
        assert!(ptr2.is_null(), "error should be cleared after first read");
    }

    #[test]
    fn hew_stream_last_error_handles_unicode() {
        set_last_error("échec de connexion 🔥".to_string());
        let ptr = hew_stream_last_error();
        assert!(!ptr.is_null());
        // SAFETY: ptr is a freshly malloc'd NUL-terminated string.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, "échec de connexion 🔥");
            libc::free(ptr.cast());
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
        // SAFETY: ptr is malloc'd; free it.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, "connection refused");
            libc::free(ptr.cast());
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
            // SAFETY: malloc'd above.
            unsafe { libc::free(ptr.cast()) };
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
        let mut sink = unsafe { Box::from_raw(into_write_sink_ptr(mock)) };
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
            let _sink = unsafe { Box::from_raw(into_write_sink_ptr(mock)) };
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
            let _ = Box::from_raw(ptr);
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
            let _ = Box::from_raw(ptr);
        }
        let items = written.lock().unwrap();
        assert_eq!(items.len(), 1);
        assert!(items[0].is_empty(), "empty write should produce empty vec");
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
            let _ = Box::from_raw(ptr);
        }

        let items = written.lock().unwrap();
        assert_eq!(items.as_slice(), &[b"abc".to_vec()]);
        assert_eq!(close_count.load(Ordering::SeqCst), 1);
    }
}
