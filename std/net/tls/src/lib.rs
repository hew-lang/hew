//! Hew `std::net::tls` — TLS client connections.
//!
//! Exposes a blocking C ABI over rustls for TLS client connections.
//! All `extern "C"` functions are designed to be called from compiled Hew
//! programs via FFI.

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code.
#[cfg(test)]
extern crate hew_runtime;

use std::io::{Read, Write};
use std::net::TcpStream;
use std::os::raw::{c_char, c_int};
use std::sync::Arc;

use hew_cabi::cabi::cstr_to_str;
use hew_cabi::vec::HewVec;
use rustls::pki_types::ServerName;
use rustls::RootCertStore;

type BoxError = Box<dyn std::error::Error + Send + Sync + 'static>;

/// Maximum bytes read in a single [`hew_tls_read`] call.
const READ_BUFFER_SIZE: usize = 65_536;

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

fn connect_tls(host: &str, port: u16) -> Result<HewTlsStream, BoxError> {
    let server_name = ServerName::try_from(host.to_string())?;
    let config = default_client_config()?;
    let connector = rustls::ClientConnection::new(Arc::new(config), server_name)?;
    let addr = format!("{host}:{port}");
    let tcp = TcpStream::connect(addr)?;
    let stream = rustls::StreamOwned::new(connector, tcp);
    Ok(HewTlsStream { stream })
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
    if stream.is_null() || (data.is_null() && data_len > 0) {
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
            if s.stream.flush().is_err() {
                return -1;
            }
            c_int::try_from(data_len).unwrap_or(c_int::MAX)
        }
        Err(_) => -1,
    }
}

/// Read up to `size` bytes from the TLS stream.
///
/// Returns a `HewVec` containing the bytes read. On error or EOF the
/// returned vector is empty.
///
/// # Safety
///
/// `stream` must be a valid pointer returned by [`hew_tls_connect`].
#[no_mangle]
pub unsafe extern "C" fn hew_tls_read(stream: *mut HewTlsStream, size: c_int) -> HewVec {
    let empty = HewVec {
        data: std::ptr::null_mut(),
        len: 0,
        cap: 0,
        elem_size: 1,
        elem_kind: hew_cabi::vec::ElemKind::Plain,
    };
    if stream.is_null() {
        return empty;
    }
    let buf_size = usize::try_from(size)
        .unwrap_or(READ_BUFFER_SIZE)
        .min(READ_BUFFER_SIZE);
    let mut buf = vec![0u8; buf_size];
    // SAFETY: `stream` is a valid HewTlsStream pointer per caller contract.
    let s = unsafe { &mut *stream };
    match s.stream.read(&mut buf) {
        Ok(0) | Err(_) => empty,
        Ok(n) => {
            buf.truncate(n);
            // Allocate via libc::malloc so the Hew runtime can free it.
            // SAFETY: n > 0 and buf.as_ptr() is valid for n bytes.
            let ptr = unsafe { libc::malloc(n) }.cast::<u8>();
            if ptr.is_null() {
                return empty;
            }
            // SAFETY: both pointers are valid and non-overlapping.
            unsafe { std::ptr::copy_nonoverlapping(buf.as_ptr(), ptr, n) };
            HewVec {
                data: ptr,
                len: n,
                cap: n,
                elem_size: 1,
                elem_kind: hew_cabi::vec::ElemKind::Plain,
            }
        }
    }
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
}
