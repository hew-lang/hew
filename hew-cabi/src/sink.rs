//! Sink ABI types shared between the Hew runtime and native packages.
//!
//! Native packages that produce `Sink` handles (e.g. HTTP streaming responses)
//! implement [`SinkBacking`] and call [`into_sink_ptr`] to create heap-allocated
//! opaque handles compatible with the runtime's C ABI.

// ── Thread-local error for fallible stream/sink operations ────────────────────

std::thread_local! {
    static LAST_ERROR: std::cell::RefCell<Option<String>> = const { std::cell::RefCell::new(None) };
}

/// Store an error message for the current thread. Retrievable via
/// [`hew_stream_last_error`].
pub fn set_last_error(msg: String) {
    LAST_ERROR.with(|e| *e.borrow_mut() = Some(msg));
}

/// Take and clear the last error, if any.
#[must_use]
pub fn take_last_error() -> Option<String> {
    LAST_ERROR.with(|e| e.borrow_mut().take())
}

/// Return the last stream/sink error as a malloc'd C string, or NULL if none.
///
/// Clears the error after reading. Callers must free the returned string.
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

// ── Sink trait and handle ─────────────────────────────────────────────────────

/// Trait for writable stream sinks. Implement this to create custom Sink
/// backends (e.g. HTTP response bodies, TCP sockets).
pub trait SinkBacking: Send + std::fmt::Debug {
    /// Write one item (exact bytes). Blocks if the backing buffer is full.
    fn write_item(&mut self, data: &[u8]);
    /// Flush any buffered writes (file sinks).
    fn flush(&mut self);
    /// Signal EOF to the reader.
    fn close(&mut self);
}

/// Opaque writable sink handle.
#[derive(Debug)]
pub struct HewSink {
    pub inner: Box<dyn SinkBacking>,
}

impl Drop for HewSink {
    fn drop(&mut self) {
        self.inner.close();
    }
}

/// Create a heap-allocated [`HewSink`] from a [`SinkBacking`] implementation.
pub fn into_sink_ptr(backing: impl SinkBacking + 'static) -> *mut HewSink {
    Box::into_raw(Box::new(HewSink {
        inner: Box::new(backing),
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

    // ── SinkBacking / HewSink / into_sink_ptr ────────────────────────────

    /// Test double that records calls to write_item, flush, and close.
    #[derive(Debug)]
    struct MockSink {
        written: Arc<std::sync::Mutex<Vec<Vec<u8>>>>,
        flush_count: Arc<AtomicUsize>,
        close_count: Arc<AtomicUsize>,
    }

    impl MockSink {
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

    impl SinkBacking for MockSink {
        fn write_item(&mut self, data: &[u8]) {
            self.written.lock().unwrap().push(data.to_vec());
        }
        fn flush(&mut self) {
            self.flush_count.fetch_add(1, Ordering::SeqCst);
        }
        fn close(&mut self) {
            self.close_count.fetch_add(1, Ordering::SeqCst);
        }
    }

    #[test]
    fn sink_backing_write_flush_close() {
        let (mock, written, flush_count, close_count) = MockSink::new();
        let mut sink = HewSink {
            inner: Box::new(mock),
        };
        sink.inner.write_item(b"hello");
        sink.inner.write_item(b"world");
        sink.inner.flush();
        sink.inner.close();

        let items = written.lock().unwrap();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0], b"hello");
        assert_eq!(items[1], b"world");
        assert_eq!(flush_count.load(Ordering::SeqCst), 1);
        assert_eq!(close_count.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn hew_sink_drop_calls_close() {
        let (mock, _written, _flush_count, close_count) = MockSink::new();
        {
            let _sink = HewSink {
                inner: Box::new(mock),
            };
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
        let ptr = into_sink_ptr(mock);
        assert!(!ptr.is_null());

        // SAFETY: ptr was just created by into_sink_ptr via Box::into_raw.
        unsafe {
            (*ptr).inner.write_item(b"via pointer");
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
        let ptr = into_sink_ptr(mock);
        // SAFETY: ptr was just created by into_sink_ptr.
        unsafe {
            (*ptr).inner.write_item(b"");
            let _ = Box::from_raw(ptr);
        }
        let items = written.lock().unwrap();
        assert_eq!(items.len(), 1);
        assert!(items[0].is_empty(), "empty write should produce empty vec");
    }
}
