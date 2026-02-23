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
