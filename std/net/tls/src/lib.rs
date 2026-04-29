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
const TLS_STATUS_SUCCESS: c_int = 0;
const TLS_STATUS_RETRYABLE: c_int = 1;
const TLS_STATUS_TLS_ERROR: c_int = 2;
const TLS_STATUS_IO_ERROR: c_int = 3;

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

#[repr(C)]
#[derive(Debug)]
pub struct HewTlsWriteResult {
    written: c_int,
    status: c_int,
}

#[repr(C)]
#[derive(Debug)]
pub struct HewTlsReadResult {
    data: HewVec,
    status: c_int,
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

fn connect_tls(host: &str, port: u16) -> Result<HewTlsStream, BoxError> {
    let server_name = ServerName::try_from(host.to_string())?;
    let config = default_client_config()?;
    let connector = rustls::ClientConnection::new(Arc::new(config), server_name)?;
    let addr = format!("{host}:{port}");
    let tcp = TcpStream::connect(addr)?;
    let stream = rustls::StreamOwned::new(connector, tcp);
    Ok(HewTlsStream { stream })
}

fn set_tls_last_error(msg: impl Into<String>) {
    LAST_TLS_ERROR.with(|error| *error.borrow_mut() = Some(msg.into()));
}

fn clear_tls_last_error() {
    LAST_TLS_ERROR.with(|error| *error.borrow_mut() = None);
}

fn get_tls_last_error() -> String {
    LAST_TLS_ERROR.with(|error| error.borrow().as_ref().cloned().unwrap_or_else(String::new))
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

fn build_hew_vec(bytes: &[u8]) -> Option<HewVec> {
    let len = bytes.len();
    // SAFETY: requesting `len` bytes from libc returns either a valid allocation or null.
    let ptr = unsafe { libc::malloc(len) }.cast::<u8>();
    if ptr.is_null() {
        return None;
    }
    // SAFETY: `bytes.as_ptr()` is valid for `len` bytes and `ptr` points to a
    // freshly allocated, non-overlapping `len`-byte region.
    unsafe { std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr, len) };
    Some(HewVec {
        data: ptr,
        len,
        cap: len,
        elem_size: 1,
        elem_kind: hew_cabi::vec::ElemKind::Plain,
    })
}

fn classify_tls_error(op: &str, err: &io::Error) -> (c_int, String) {
    match err.kind() {
        io::ErrorKind::WouldBlock => (TLS_STATUS_RETRYABLE, format!("{op}: would block")),
        io::ErrorKind::TimedOut => (TLS_STATUS_RETRYABLE, format!("{op}: timed out")),
        io::ErrorKind::InvalidData => (
            TLS_STATUS_TLS_ERROR,
            format!("{op}: tls alert/protocol error: {err}"),
        ),
        _ => (TLS_STATUS_IO_ERROR, format!("{op}: io error: {err}")),
    }
}

fn write_out_status(out_status: *mut c_int, status: c_int) -> bool {
    if out_status.is_null() {
        return false;
    }
    // SAFETY: caller provided a non-null `out_status` pointer.
    unsafe { *out_status = status };
    true
}

fn read_tls_vec<R: Read>(reader: &mut R, size: c_int) -> HewTlsReadResult {
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
        return HewTlsReadResult {
            data: empty,
            status: TLS_STATUS_SUCCESS,
        };
    }

    let mut buf = vec![0u8; buf_size];
    match reader.read(&mut buf) {
        Ok(0) => {
            clear_tls_last_error();
            HewTlsReadResult {
                data: empty,
                status: TLS_STATUS_SUCCESS,
            }
        }
        Ok(n) => {
            if let Some(data) = build_hew_vec(&buf[..n]) {
                clear_tls_last_error();
                HewTlsReadResult {
                    data,
                    status: TLS_STATUS_SUCCESS,
                }
            } else {
                set_tls_last_error("hew_tls_read: allocation failed");
                HewTlsReadResult {
                    data: empty,
                    status: TLS_STATUS_IO_ERROR,
                }
            }
        }
        Err(err) => {
            let (status, message) = classify_tls_error("hew_tls_read", &err);
            set_tls_last_error(message);
            HewTlsReadResult {
                data: empty,
                status,
            }
        }
    }
}

fn write_tls_bytes<W: Write>(writer: &mut W, buf: &[u8]) -> HewTlsWriteResult {
    match writer.write_all(buf) {
        Ok(()) => match writer.flush() {
            Ok(()) => {
                clear_tls_last_error();
                HewTlsWriteResult {
                    written: c_int::try_from(buf.len()).unwrap_or(c_int::MAX),
                    status: TLS_STATUS_SUCCESS,
                }
            }
            Err(err) => {
                let (status, message) = classify_tls_error("hew_tls_write", &err);
                set_tls_last_error(message);
                HewTlsWriteResult {
                    written: -1,
                    status,
                }
            }
        },
        Err(err) => {
            let (status, message) = classify_tls_error("hew_tls_write", &err);
            set_tls_last_error(message);
            HewTlsWriteResult {
                written: -1,
                status,
            }
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
        return std::ptr::null_mut();
    };
    let Ok(port_u16) = u16::try_from(port) else {
        return std::ptr::null_mut();
    };
    match connect_tls(host_str, port_u16) {
        Ok(stream) => Box::into_raw(Box::new(stream)),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Return this actor's last TLS client error.
///
/// Returns an empty string when no TLS client error has been recorded. The
/// returned string is `malloc`-allocated; callers must free it with
/// `libc::free`.
#[no_mangle]
pub extern "C" fn hew_tls_last_error() -> *mut c_char {
    str_to_malloc(&get_tls_last_error())
}

/// Write `data` to the TLS stream.
///
/// Returns the number of bytes written, or −1 on error.
///
/// # Safety
///
/// * `stream` must be a valid pointer returned by [`hew_tls_connect`].
/// * `data` and `data_len` must describe a valid byte buffer.
/// * `out_status` must be a valid pointer to writable status storage.
#[no_mangle]
pub unsafe extern "C" fn hew_tls_write(
    stream: *mut HewTlsStream,
    data: *const u8,
    data_len: usize,
    out_status: *mut c_int,
) -> c_int {
    if !write_out_status(out_status, TLS_STATUS_IO_ERROR) {
        set_tls_last_error("hew_tls_write: invalid status buffer");
        return -1;
    }
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
    let result = write_tls_bytes(&mut s.stream, buf);
    let _ = write_out_status(out_status, result.status);
    result.written
}

/// Read up to `size` bytes from the TLS stream.
///
/// Returns a `HewVec` containing the bytes read. `out_status` receives
/// `0` for success or orderly EOF, `1` for retryable would-block/timeout
/// conditions, `2` for TLS alert/protocol failures, and `3` for I/O failures.
///
/// # Safety
///
/// `stream` must be a valid pointer returned by [`hew_tls_connect`].
/// `out_status` must be a valid pointer to writable status storage.
#[no_mangle]
pub unsafe extern "C" fn hew_tls_read(
    stream: *mut HewTlsStream,
    size: c_int,
    out_status: *mut c_int,
) -> HewVec {
    if !write_out_status(out_status, TLS_STATUS_IO_ERROR) {
        set_tls_last_error("hew_tls_read: invalid status buffer");
        return empty_hew_vec();
    }
    if stream.is_null() {
        set_tls_last_error("hew_tls_read: invalid stream");
        return empty_hew_vec();
    }
    // SAFETY: `stream` is a valid HewTlsStream pointer per caller contract.
    let s = unsafe { &mut *stream };
    let result = read_tls_vec(&mut s.stream, size);
    let _ = write_out_status(out_status, result.status);
    result.data
}

/// Hew-facing bridge that packages `hew_tls_write` status into a repr(C) struct
/// because Hew source cannot observe raw out-parameters directly.
///
/// # Safety
///
/// `stream`, `data`, and `data_len` must satisfy the same requirements as
/// [`hew_tls_write`].
#[no_mangle]
pub unsafe extern "C" fn hew_tls_write_result(
    stream: *mut HewTlsStream,
    data: *const u8,
    data_len: usize,
) -> HewTlsWriteResult {
    let mut status = TLS_STATUS_IO_ERROR;
    // SAFETY: this bridge forwards the exact caller-provided arguments to
    // `hew_tls_write` and supplies valid local storage for `out_status`.
    let written = unsafe { hew_tls_write(stream, data, data_len, &raw mut status) };
    HewTlsWriteResult { written, status }
}

/// Hew-facing bridge that packages `hew_tls_read` status into a repr(C) struct
/// because Hew source cannot observe raw out-parameters directly.
///
/// # Safety
///
/// `stream` and `size` must satisfy the same requirements as [`hew_tls_read`].
#[no_mangle]
pub unsafe extern "C" fn hew_tls_read_result(
    stream: *mut HewTlsStream,
    size: c_int,
) -> HewTlsReadResult {
    let mut status = TLS_STATUS_IO_ERROR;
    // SAFETY: this bridge forwards the exact caller-provided arguments to
    // `hew_tls_read` and supplies valid local storage for `out_status`.
    let data = unsafe { hew_tls_read(stream, size, &raw mut status) };
    HewTlsReadResult { data, status }
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
    use std::ffi::CStr;
    use std::io::ErrorKind;
    use std::net::TcpListener;
    use std::os::raw::c_void;
    use std::thread;
    use std::time::Duration;

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

    #[derive(Debug)]
    enum MockWriteAction {
        Success,
        WriteError(io::Error),
        FlushError(io::Error),
    }

    #[derive(Debug)]
    struct MockWriter {
        action: MockWriteAction,
        wrote: Cell<bool>,
        flushed: Cell<bool>,
    }

    impl MockWriter {
        fn success() -> Self {
            Self {
                action: MockWriteAction::Success,
                wrote: Cell::new(false),
                flushed: Cell::new(false),
            }
        }

        fn write_error(kind: ErrorKind, message: &'static str) -> Self {
            Self {
                action: MockWriteAction::WriteError(io::Error::new(kind, message)),
                wrote: Cell::new(false),
                flushed: Cell::new(false),
            }
        }

        fn flush_error(kind: ErrorKind, message: &'static str) -> Self {
            Self {
                action: MockWriteAction::FlushError(io::Error::new(kind, message)),
                wrote: Cell::new(false),
                flushed: Cell::new(false),
            }
        }
    }

    impl Write for MockWriter {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            self.wrote.set(true);
            match &self.action {
                MockWriteAction::Success | MockWriteAction::FlushError(_) => Ok(buf.len()),
                MockWriteAction::WriteError(err) => {
                    Err(io::Error::new(err.kind(), err.to_string()))
                }
            }
        }

        fn flush(&mut self) -> io::Result<()> {
            self.flushed.set(true);
            match &self.action {
                MockWriteAction::Success | MockWriteAction::WriteError(_) => Ok(()),
                MockWriteAction::FlushError(err) => {
                    Err(io::Error::new(err.kind(), err.to_string()))
                }
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

    fn last_error_string() -> String {
        let ptr = hew_tls_last_error();
        if ptr.is_null() {
            return String::new();
        }
        // SAFETY: `ptr` is returned by `hew_tls_last_error` as a valid,
        // NUL-terminated string allocation.
        let message = unsafe { CStr::from_ptr(ptr) }
            .to_string_lossy()
            .into_owned();
        // SAFETY: `ptr` was allocated with `libc::malloc` by `hew_tls_last_error`.
        unsafe { libc::free(ptr.cast::<c_void>()) };
        message
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

    fn timed_out_tcp_reader(client_timeout: Duration) -> (TcpStream, thread::JoinHandle<()>) {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let addr = listener.local_addr().unwrap();

        let server = thread::spawn(move || {
            let _accepted = listener.accept().unwrap();
            thread::sleep(Duration::from_millis(200));
        });

        let tcp = TcpStream::connect(addr).unwrap();
        tcp.set_read_timeout(Some(client_timeout)).unwrap();
        (tcp, server)
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
        let mut status = -1;
        // SAFETY: passing null stream is the test.
        let ret = unsafe {
            hew_tls_write(
                std::ptr::null_mut(),
                data.as_ptr(),
                data.len(),
                &raw mut status,
            )
        };
        assert_eq!(ret, -1);
        assert_eq!(status, TLS_STATUS_IO_ERROR);
    }

    #[test]
    fn read_null_stream_returns_empty() {
        let mut status = -1;
        // SAFETY: passing null stream is the test.
        let vec = unsafe { hew_tls_read(std::ptr::null_mut(), 1024, &raw mut status) };
        assert_eq!(vec.len, 0);
        assert_eq!(status, TLS_STATUS_IO_ERROR);
    }

    #[test]
    fn close_null_stream_is_noop() {
        // SAFETY: passing null is the test.
        unsafe { hew_tls_close(std::ptr::null_mut()) };
    }

    #[test]
    fn clean_eof_and_midstream_force_close_have_distinct_statuses() {
        set_tls_last_error("stale");
        let mut reader = MockReader::eof();
        let eof = read_tls_vec(&mut reader, 32);

        assert!(reader.called.get());
        assert_eq!(eof.data.len, 0);
        assert_eq!(eof.status, TLS_STATUS_SUCCESS);
        assert!(last_error_string().is_empty());

        let mut force_closed =
            MockReader::error(ErrorKind::UnexpectedEof, "peer closed mid-stream");
        let reset = read_tls_vec(&mut force_closed, 32);

        assert_eq!(reset.data.len, 0);
        assert_eq!(reset.status, TLS_STATUS_IO_ERROR);
        assert_eq!(
            last_error_string(),
            "hew_tls_read: io error: peer closed mid-stream"
        );
    }

    #[test]
    fn read_would_block_sets_retryable_status_and_last_error() {
        clear_tls_last_error();
        let mut reader = MockReader::error(ErrorKind::WouldBlock, "try again");

        let result = read_tls_vec(&mut reader, 32);

        assert_eq!(result.data.len, 0);
        assert_eq!(result.status, TLS_STATUS_RETRYABLE);
        assert_eq!(last_error_string(), "hew_tls_read: would block");
    }

    #[test]
    fn read_socket_timeout_sets_retryable_status_and_last_error() {
        clear_tls_last_error();
        let (mut stream, server) = timed_out_tcp_reader(Duration::from_millis(25));

        let result = read_tls_vec(&mut stream, 32);

        assert_eq!(result.data.len, 0);
        assert_eq!(result.status, TLS_STATUS_RETRYABLE);
        // macOS surfaces SO_RCVTIMEO expirations as WouldBlock; Linux commonly
        // reports TimedOut. Both must stay on the retryable status path.
        let last_error = last_error_string();
        assert!(
            last_error == "hew_tls_read: timed out" || last_error == "hew_tls_read: would block",
            "unexpected timeout classification: {last_error}"
        );
        server.join().unwrap();
    }

    #[test]
    fn classify_timed_out_as_retryable() {
        let (status, message) = classify_tls_error(
            "hew_tls_read",
            &io::Error::new(ErrorKind::TimedOut, "slow peer"),
        );

        assert_eq!(status, TLS_STATUS_RETRYABLE);
        assert_eq!(message, "hew_tls_read: timed out");
    }

    #[test]
    fn read_tls_protocol_error_sets_tls_status() {
        clear_tls_last_error();
        let mut reader = MockReader::error(ErrorKind::InvalidData, "received bad alert");

        let result = read_tls_vec(&mut reader, 32);

        assert_eq!(result.data.len, 0);
        assert_eq!(result.status, TLS_STATUS_TLS_ERROR);
        assert_eq!(
            last_error_string(),
            "hew_tls_read: tls alert/protocol error: received bad alert"
        );
    }

    #[test]
    fn read_transport_error_sets_last_error_text() {
        clear_tls_last_error();
        let mut reader = MockReader::error(ErrorKind::ConnectionReset, "connection reset");

        let result = read_tls_vec(&mut reader, 32);

        assert_eq!(result.data.len, 0);
        assert_eq!(result.status, TLS_STATUS_IO_ERROR);
        assert_eq!(
            last_error_string(),
            "hew_tls_read: io error: connection reset"
        );
    }

    #[test]
    fn zero_size_read_is_empty_but_not_eof() {
        clear_tls_last_error();
        let mut reader = MockReader::panic();

        let result = read_tls_vec(&mut reader, 0);

        assert!(!reader.called.get());
        assert_eq!(result.data.len, 0);
        assert_eq!(result.status, TLS_STATUS_SUCCESS);
        assert!(last_error_string().is_empty());
    }

    #[test]
    fn successful_non_empty_read_clears_last_error() {
        set_tls_last_error("timeout");
        let mut reader = MockReader::bytes(b"hello");

        let result = read_tls_vec(&mut reader, 32);

        assert_eq!(result.status, TLS_STATUS_SUCCESS);
        assert_eq!(vec_bytes(&result.data), b"hello");
        assert!(last_error_string().is_empty());
        free_vec(&result.data);
    }

    #[test]
    fn successful_write_clears_stale_error() {
        set_tls_last_error("old error");
        let mut writer = MockWriter::success();

        let result = write_tls_bytes(&mut writer, b"hello");

        assert!(writer.wrote.get());
        assert!(writer.flushed.get());
        assert_eq!(result.written, 5);
        assert_eq!(result.status, TLS_STATUS_SUCCESS);
        assert!(last_error_string().is_empty());
    }

    #[test]
    fn write_would_block_sets_retryable_status_and_last_error() {
        clear_tls_last_error();
        let mut writer = MockWriter::write_error(ErrorKind::WouldBlock, "try again");

        let result = write_tls_bytes(&mut writer, b"hello");

        assert_eq!(result.written, -1);
        assert_eq!(result.status, TLS_STATUS_RETRYABLE);
        assert_eq!(last_error_string(), "hew_tls_write: would block");
    }

    #[test]
    fn write_tls_protocol_error_sets_tls_status_and_last_error() {
        clear_tls_last_error();
        let mut writer = MockWriter::flush_error(ErrorKind::InvalidData, "bad close notify");

        let result = write_tls_bytes(&mut writer, b"hello");

        assert_eq!(result.written, -1);
        assert_eq!(result.status, TLS_STATUS_TLS_ERROR);
        assert_eq!(
            last_error_string(),
            "hew_tls_write: tls alert/protocol error: bad close notify"
        );
    }

    #[test]
    fn default_config_does_not_panic() {
        let config = default_client_config();
        assert!(config.is_ok(), "default_client_config should succeed");
    }

    #[test]
    fn connect_empty_host_returns_null() {
        let host = std::ffi::CString::new("").unwrap();
        // SAFETY: passing valid but empty C string.
        let ptr = unsafe { hew_tls_connect(host.as_ptr(), 443) };
        assert!(ptr.is_null());
    }

    #[test]
    fn connect_negative_port_returns_null() {
        let host = std::ffi::CString::new("example.com").unwrap();
        // SAFETY: passing valid host with invalid port.
        let ptr = unsafe { hew_tls_connect(host.as_ptr(), -1) };
        assert!(ptr.is_null());
    }

    #[test]
    fn write_null_data_nonzero_len_returns_error() {
        let mut status = -1;
        // SAFETY: testing null data with non-zero length.
        let ret =
            unsafe { hew_tls_write(std::ptr::null_mut(), std::ptr::null(), 10, &raw mut status) };
        assert_eq!(ret, -1);
        assert_eq!(status, TLS_STATUS_IO_ERROR);
    }

    // ── hew_tls_write_result bridge ───────────────────────────────────────────

    #[test]
    fn write_result_null_stream_returns_io_error_status() {
        clear_tls_last_error();
        let data = b"payload";
        // SAFETY: null stream is the test; data pointer is valid.
        let result =
            unsafe { hew_tls_write_result(std::ptr::null_mut(), data.as_ptr(), data.len()) };
        assert_eq!(result.written, -1);
        assert_eq!(result.status, TLS_STATUS_IO_ERROR);
    }

    #[test]
    fn write_result_null_data_nonzero_len_returns_io_error_status() {
        clear_tls_last_error();
        // SAFETY: null data with non-zero data_len is the tested invalid call.
        let result = unsafe { hew_tls_write_result(std::ptr::null_mut(), std::ptr::null(), 5) };
        assert_eq!(result.written, -1);
        assert_eq!(result.status, TLS_STATUS_IO_ERROR);
    }

    #[test]
    fn write_result_packages_status_from_underlying_call() {
        // Verify the wrapper propagates the status written by hew_tls_write
        // (not the initial TLS_STATUS_IO_ERROR sentinel) into the returned struct.
        // With a null stream, hew_tls_write sets status to TLS_STATUS_IO_ERROR and
        // returns -1; the wrapper must capture both fields.
        clear_tls_last_error();
        let data = b"check";
        let mut direct_status = 0_i32;
        // SAFETY: null stream, valid data pointer — exercising the error path.
        let direct_written = unsafe {
            hew_tls_write(
                std::ptr::null_mut(),
                data.as_ptr(),
                data.len(),
                &raw mut direct_status,
            )
        };

        clear_tls_last_error();
        // SAFETY: same inputs as above via the wrapper.
        let result =
            unsafe { hew_tls_write_result(std::ptr::null_mut(), data.as_ptr(), data.len()) };

        assert_eq!(result.written, direct_written);
        assert_eq!(result.status, direct_status);
    }

    // ── hew_tls_read_result bridge ────────────────────────────────────────────

    #[test]
    fn read_result_null_stream_returns_io_error_status() {
        clear_tls_last_error();
        // SAFETY: null stream is the test.
        let result = unsafe { hew_tls_read_result(std::ptr::null_mut(), 1024) };
        assert_eq!(result.data.len, 0);
        assert!(result.data.data.is_null());
        assert_eq!(result.status, TLS_STATUS_IO_ERROR);
    }

    #[test]
    fn read_result_packages_status_from_underlying_call() {
        // Verify the wrapper propagates the status written by hew_tls_read
        // (not the initial TLS_STATUS_IO_ERROR sentinel) into the returned struct.
        clear_tls_last_error();
        let mut direct_status = 0_i32;
        // SAFETY: null stream — exercising the error path through hew_tls_read directly.
        let _direct_vec = unsafe { hew_tls_read(std::ptr::null_mut(), 64, &raw mut direct_status) };

        clear_tls_last_error();
        // SAFETY: same null stream via the wrapper.
        let result = unsafe { hew_tls_read_result(std::ptr::null_mut(), 64) };

        assert_eq!(result.status, direct_status);
        assert_eq!(result.data.len, 0);
    }
}
