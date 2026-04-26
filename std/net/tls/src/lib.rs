//! Hew `std::net::tls` — TLS client connections.
//!
//! Exposes a blocking C ABI over rustls for TLS client connections.
//! All `extern "C"` functions are designed to be called from compiled Hew
//! programs via FFI. Any string returned by [`hew_tls_last_error`] is allocated
//! with `libc::malloc`; callers must free it with `libc::free`.

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code.
#[cfg(test)]
extern crate hew_runtime;

use std::cell::RefCell;
use std::io::{self, Read, Write};
use std::net::TcpStream;
use std::os::raw::{c_char, c_int};
use std::sync::Arc;

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use hew_cabi::vec::HewVec;
use rustls::pki_types::ServerName;
use rustls::RootCertStore;

type BoxError = Box<dyn std::error::Error + Send + Sync + 'static>;

/// Maximum bytes read in a single [`hew_tls_read`] call.
const READ_BUFFER_SIZE: usize = 65_536;

std::thread_local! {
    static LAST_TLS_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
}

// ── Opaque handle ─────────────────────────────────────────────────────────────

/// An established TLS connection.
///
/// Wraps a rustls [`StreamOwned`] over a TCP stream.
/// Must be closed with [`hew_tls_close`].
#[derive(Debug)]
pub struct HewTlsStream {
    stream: rustls::StreamOwned<rustls::ClientConnection, TcpStream>,
}

// ── Helpers ───────────────────────────────────────────────────────────────────

fn ring_provider() -> Arc<rustls::crypto::CryptoProvider> {
    Arc::new(rustls::crypto::ring::default_provider())
}

/// Build a [`rustls::ClientConfig`] that trusts the standard webpki root CAs.
fn default_client_config() -> Result<rustls::ClientConfig, BoxError> {
    let mut roots = RootCertStore::empty();
    roots.extend(webpki_roots::TLS_SERVER_ROOTS.iter().cloned());
    let config = rustls::ClientConfig::builder_with_provider(ring_provider())
        .with_protocol_versions(rustls::DEFAULT_VERSIONS)?
        .with_root_certificates(roots)
        .with_no_client_auth();
    Ok(config)
}

fn connect_tls(host: &str, port: u16) -> Result<HewTlsStream, String> {
    let server_name = ServerName::try_from(host.to_string())
        .map_err(|err| format!("invalid TLS hostname `{host}`: {err}"))?;
    let config = default_client_config()
        .map_err(|err| format!("failed to build TLS client config: {err}"))?;
    let connector = rustls::ClientConnection::new(Arc::new(config), server_name)
        .map_err(|err| format!("failed to create TLS client connection for `{host}`: {err}"))?;
    let addr = format!("{host}:{port}");
    let tcp =
        TcpStream::connect(&addr).map_err(|err| format!("failed to connect to {addr}: {err}"))?;
    let stream = rustls::StreamOwned::new(connector, tcp);
    Ok(HewTlsStream { stream })
}

fn set_tls_last_error(msg: impl Into<String>) {
    LAST_TLS_ERROR.with(|error| *error.borrow_mut() = Some(msg.into()));
}

fn clear_tls_last_error() {
    LAST_TLS_ERROR.with(|error| *error.borrow_mut() = None);
}

fn clone_tls_last_error() -> Option<String> {
    LAST_TLS_ERROR.with(|error| error.borrow().clone())
}

fn record_tls_connect_error(msg: impl Into<String>) -> *mut HewTlsStream {
    set_tls_last_error(msg);
    std::ptr::null_mut()
}

fn empty_hew_vec() -> HewVec {
    HewVec {
        data: std::ptr::null_mut(),
        len: 0,
        cap: 0,
        elem_size: 1,
        elem_kind: hew_cabi::vec::ElemKind::Plain,
    }
}

fn build_hew_vec(bytes: &[u8]) -> HewVec {
    let empty = empty_hew_vec();
    let len = bytes.len();
    // SAFETY: requesting `len` bytes from libc returns either a valid allocation or null.
    let ptr = unsafe { libc::malloc(len) }.cast::<u8>();
    if ptr.is_null() {
        set_tls_last_error("hew_tls_read: allocation failed");
        return empty;
    }
    // SAFETY: `bytes.as_ptr()` is valid for `len` bytes and `ptr` points to a
    // freshly allocated, non-overlapping `len`-byte region.
    unsafe { std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr, len) };
    HewVec {
        data: ptr,
        len,
        cap: len,
        elem_size: 1,
        elem_kind: hew_cabi::vec::ElemKind::Plain,
    }
}

fn classify_tls_io_error(err: &io::Error) -> String {
    match err.kind() {
        io::ErrorKind::TimedOut | io::ErrorKind::WouldBlock => "timeout".to_string(),
        _ => format!("io: {err}"),
    }
}

fn read_tls_vec<R: Read>(reader: &mut R, size: c_int) -> HewVec {
    let empty = empty_hew_vec();
    let buf_size = if size == 0 {
        0
    } else {
        usize::try_from(size)
            .unwrap_or(READ_BUFFER_SIZE)
            .min(READ_BUFFER_SIZE)
    };
    if buf_size == 0 {
        clear_tls_last_error();
        return empty;
    }

    let mut buf = vec![0u8; buf_size];
    match reader.read(&mut buf) {
        Ok(0) => {
            set_tls_last_error("eof");
            empty
        }
        Ok(n) => {
            clear_tls_last_error();
            build_hew_vec(&buf[..n])
        }
        Err(err) => {
            set_tls_last_error(classify_tls_io_error(&err));
            empty
        }
    }
}

// ── FFI exports ───────────────────────────────────────────────────────────────

/// Open a TLS connection to `host:port` using system root certificates.
///
/// Returns a heap-allocated [`HewTlsStream`] on success, or null on error.
///
/// # Safety
///
/// `host` must be a valid NUL-terminated UTF-8 string.
#[no_mangle]
pub unsafe extern "C" fn hew_tls_connect(host: *const c_char, port: c_int) -> *mut HewTlsStream {
    // SAFETY: `host` is a valid NUL-terminated C string per caller contract.
    let Some(host_str) = (unsafe { cstr_to_str(host) }) else {
        return record_tls_connect_error("invalid TLS host: null pointer or invalid UTF-8");
    };
    let Ok(port_u16) = u16::try_from(port) else {
        return record_tls_connect_error(format!("invalid TLS port: {port}"));
    };
    match connect_tls(host_str, port_u16) {
        Ok(stream) => {
            clear_tls_last_error();
            Box::into_raw(Box::new(stream))
        }
        Err(err) => record_tls_connect_error(err),
    }
}

/// Return the last TLS client error recorded on the current thread.
///
/// Returns a `malloc`-allocated, NUL-terminated string. The caller must free it
/// with `libc::free`. Returns null when no TLS client error has been recorded.
#[no_mangle]
pub extern "C" fn hew_tls_last_error() -> *mut c_char {
    match clone_tls_last_error() {
        Some(message) => str_to_malloc(&message),
        None => std::ptr::null_mut(),
    }
}

/// Write `data` to the TLS stream.
///
/// Returns the number of bytes written, or −1 on error.
///
/// # Safety
///
/// * `stream` must be a valid pointer returned by [`hew_tls_connect`].
/// * `data` and `data_len` must describe a valid byte buffer.
#[no_mangle]
pub unsafe extern "C" fn hew_tls_write(
    stream: *mut HewTlsStream,
    data: *const u8,
    data_len: usize,
) -> c_int {
    if stream.is_null() {
        set_tls_last_error("hew_tls_write: invalid stream");
        return -1;
    }
    if data.is_null() && data_len > 0 {
        set_tls_last_error("hew_tls_write: invalid data buffer");
        return -1;
    }
    // SAFETY: `stream` is a valid HewTlsStream pointer per caller contract.
    let s = unsafe { &mut *stream };
    // SAFETY: `data` is valid for `data_len` bytes per caller contract.
    let buf = if data_len == 0 {
        &[]
    } else {
        // SAFETY: caller guarantees `data` points to `data_len` readable bytes.
        unsafe { std::slice::from_raw_parts(data, data_len) }
    };
    match s.stream.write_all(buf) {
        Ok(()) => {
            if let Err(err) = s.stream.flush() {
                set_tls_last_error(classify_tls_io_error(&err));
                return -1;
            }
            clear_tls_last_error();
            c_int::try_from(data_len).unwrap_or(c_int::MAX)
        }
        Err(err) => {
            set_tls_last_error(classify_tls_io_error(&err));
            -1
        }
    }
}

/// Read up to `size` bytes from the TLS stream.
///
/// Returns a `HewVec` containing the bytes read. On EOF, timeout, or transport
/// error the returned vector is empty; call [`hew_tls_last_error`] to
/// distinguish `"eof"`, `"timeout"`, or an `"io: ..."` failure from an
/// intentional empty read such as `size == 0`.
///
/// # Safety
///
/// `stream` must be a valid pointer returned by [`hew_tls_connect`].
#[no_mangle]
pub unsafe extern "C" fn hew_tls_read(stream: *mut HewTlsStream, size: c_int) -> HewVec {
    if stream.is_null() {
        set_tls_last_error("hew_tls_read: invalid stream");
        return empty_hew_vec();
    }
    // SAFETY: `stream` is a valid HewTlsStream pointer per caller contract.
    let s = unsafe { &mut *stream };
    read_tls_vec(&mut s.stream, size)
}

/// Close the TLS connection and free the stream handle.
///
/// # Safety
///
/// `stream` must be a valid pointer returned by [`hew_tls_connect`],
/// and must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_tls_close(stream: *mut HewTlsStream) {
    if stream.is_null() {
        return;
    }
    // SAFETY: `stream` was allocated by Box::into_raw in hew_tls_connect.
    let _ = unsafe { Box::from_raw(stream) };
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;
    use std::ffi::{CStr, CString};
    use std::io::ErrorKind;
    use std::net::TcpListener;
    use std::os::raw::c_void;
    use std::thread;

    #[derive(Debug)]
    enum MockReadAction {
        Bytes(Vec<u8>),
        Eof,
        Error(io::Error),
        Panic,
    }

    #[derive(Debug)]
    struct MockReader {
        action: MockReadAction,
        called: Cell<bool>,
    }

    impl MockReader {
        fn bytes(bytes: &[u8]) -> Self {
            Self {
                action: MockReadAction::Bytes(bytes.to_vec()),
                called: Cell::new(false),
            }
        }

        fn eof() -> Self {
            Self {
                action: MockReadAction::Eof,
                called: Cell::new(false),
            }
        }

        fn error(kind: ErrorKind, message: &'static str) -> Self {
            Self {
                action: MockReadAction::Error(io::Error::new(kind, message)),
                called: Cell::new(false),
            }
        }

        fn panic() -> Self {
            Self {
                action: MockReadAction::Panic,
                called: Cell::new(false),
            }
        }
    }

    impl Read for MockReader {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            self.called.set(true);
            match &self.action {
                MockReadAction::Bytes(bytes) => {
                    let count = bytes.len().min(buf.len());
                    buf[..count].copy_from_slice(&bytes[..count]);
                    Ok(count)
                }
                MockReadAction::Eof => Ok(0),
                MockReadAction::Error(err) => Err(io::Error::new(err.kind(), err.to_string())),
                MockReadAction::Panic => panic!("reader should not be called"),
            }
        }
    }

    fn last_error_string() -> Option<String> {
        let ptr = hew_tls_last_error();
        if ptr.is_null() {
            return None;
        }
        // SAFETY: `ptr` is returned by `hew_tls_last_error` as a valid,
        // NUL-terminated string allocation.
        let message = unsafe { CStr::from_ptr(ptr) }
            .to_string_lossy()
            .into_owned();
        // SAFETY: `ptr` was allocated with `libc::malloc` by `hew_tls_last_error`.
        unsafe { libc::free(ptr.cast::<c_void>()) };
        Some(message)
    }

    fn vec_bytes(vec: &HewVec) -> Vec<u8> {
        if vec.len == 0 || vec.data.is_null() {
            return Vec::new();
        }
        // SAFETY: non-empty `HewVec` values produced by this module store `len`
        // readable bytes at `data`.
        unsafe { std::slice::from_raw_parts(vec.data.cast::<u8>(), vec.len) }.to_vec()
    }

    fn free_vec(vec: &HewVec) {
        if !vec.data.is_null() {
            // SAFETY: test vectors are allocated with `libc::malloc` in `build_hew_vec`.
            unsafe { libc::free(vec.data.cast::<c_void>()) };
        }
    }

    #[test]
    fn connect_null_host_returns_null() {
        // SAFETY: passing null is the test.
        let ptr = unsafe { hew_tls_connect(std::ptr::null(), 443) };
        assert!(ptr.is_null());
    }

    #[test]
    fn write_null_stream_returns_error() {
        let data = b"hello";
        // SAFETY: passing null stream is the test.
        let ret = unsafe { hew_tls_write(std::ptr::null_mut(), data.as_ptr(), data.len()) };
        assert_eq!(ret, -1);
    }

    #[test]
    fn read_null_stream_returns_empty() {
        // SAFETY: passing null stream is the test.
        let vec = unsafe { hew_tls_read(std::ptr::null_mut(), 1024) };
        assert_eq!(vec.len, 0);
    }

    #[test]
    fn close_null_stream_is_noop() {
        // SAFETY: passing null is the test.
        unsafe { hew_tls_close(std::ptr::null_mut()) };
    }

    #[test]
    fn read_clean_eof_sets_last_error() {
        clear_tls_last_error();
        let mut reader = MockReader::eof();

        let vec = read_tls_vec(&mut reader, 32);

        assert!(reader.called.get());
        assert_eq!(vec.len, 0);
        assert_eq!(last_error_string().as_deref(), Some("eof"));
    }

    #[test]
    fn read_timeout_sets_last_error() {
        clear_tls_last_error();
        let mut reader = MockReader::error(ErrorKind::TimedOut, "socket timed out");

        let vec = read_tls_vec(&mut reader, 32);

        assert_eq!(vec.len, 0);
        assert_eq!(last_error_string().as_deref(), Some("timeout"));
    }

    #[test]
    fn read_transport_error_sets_last_error_text() {
        clear_tls_last_error();
        let mut reader = MockReader::error(ErrorKind::ConnectionReset, "connection reset");

        let vec = read_tls_vec(&mut reader, 32);

        assert_eq!(vec.len, 0);
        assert_eq!(last_error_string().as_deref(), Some("io: connection reset"));
    }

    #[test]
    fn zero_size_read_is_empty_but_not_eof() {
        clear_tls_last_error();
        let mut reader = MockReader::panic();

        let vec = read_tls_vec(&mut reader, 0);

        assert!(!reader.called.get());
        assert_eq!(vec.len, 0);
        assert!(last_error_string().is_none());
    }

    #[test]
    fn successful_non_empty_read_clears_last_error() {
        set_tls_last_error("timeout");
        let mut reader = MockReader::bytes(b"hello");

        let vec = read_tls_vec(&mut reader, 32);

        assert_eq!(vec_bytes(&vec), b"hello");
        assert!(last_error_string().is_none());
        free_vec(&vec);
    }

    #[test]
    fn default_config_does_not_panic() {
        let config = default_client_config();
        assert!(config.is_ok(), "default_client_config should succeed");
    }

    #[test]
    fn connect_empty_host_returns_null() {
        let host = CString::new("").unwrap();
        // SAFETY: passing valid but empty C string.
        let ptr = unsafe { hew_tls_connect(host.as_ptr(), 443) };
        assert!(ptr.is_null());
    }

    #[test]
    fn connect_negative_port_returns_null() {
        let host = CString::new("example.com").unwrap();
        // SAFETY: passing valid host with invalid port.
        let ptr = unsafe { hew_tls_connect(host.as_ptr(), -1) };
        assert!(ptr.is_null());
    }

    #[test]
    fn connect_invalid_hostname_records_last_error() {
        clear_tls_last_error();
        let host = CString::new("bad host").unwrap();

        // SAFETY: passing valid C string with an invalid TLS hostname.
        let ptr = unsafe { hew_tls_connect(host.as_ptr(), 443) };

        assert!(ptr.is_null());
        let err = last_error_string().expect("invalid hostname should record an error");
        assert!(
            err.contains("hostname"),
            "expected hostname context, got: {err}"
        );
        assert!(err.contains("bad host"), "expected host value, got: {err}");
    }

    #[test]
    fn connect_unreachable_tcp_records_last_error() {
        clear_tls_last_error();
        let listener = TcpListener::bind(("127.0.0.1", 0)).expect("bind ephemeral port");
        let port = listener.local_addr().expect("local addr").port();
        drop(listener);
        let host = CString::new("localhost").unwrap();

        // SAFETY: passing valid host and an unused localhost port.
        let ptr = unsafe { hew_tls_connect(host.as_ptr(), i32::from(port)) };

        assert!(ptr.is_null());
        let err = last_error_string().expect("connect failure should record an error");
        assert!(
            err.contains("connect"),
            "expected connect context, got: {err}"
        );
        assert!(
            err.contains("localhost"),
            "expected address context, got: {err}"
        );
    }

    #[test]
    fn connect_success_clears_stale_last_error() {
        let stale_host = CString::new("bad host").unwrap();
        // SAFETY: passing valid C string with an invalid TLS hostname.
        let failed = unsafe { hew_tls_connect(stale_host.as_ptr(), 443) };
        assert!(failed.is_null());
        assert!(last_error_string().is_some());

        let listener = TcpListener::bind(("127.0.0.1", 0)).expect("bind ephemeral port");
        let port = listener.local_addr().expect("local addr").port();
        let accept_thread = thread::spawn(move || {
            let _ = listener.accept().expect("accept client");
        });
        let host = CString::new("localhost").unwrap();

        // SAFETY: passing valid host and port for a listening local TCP server.
        let stream = unsafe { hew_tls_connect(host.as_ptr(), i32::from(port)) };

        assert!(!stream.is_null(), "expected TLS connect wrapper to succeed");
        assert!(
            last_error_string().is_none(),
            "success should clear stale error"
        );
        // SAFETY: `stream` was returned by `hew_tls_connect`.
        unsafe { hew_tls_close(stream) };
        accept_thread.join().expect("accept thread should finish");
    }

    #[test]
    fn write_null_data_nonzero_len_returns_error() {
        // SAFETY: testing null data with non-zero length.
        let ret = unsafe { hew_tls_write(std::ptr::null_mut(), std::ptr::null(), 10) };
        assert_eq!(ret, -1);
    }
}
