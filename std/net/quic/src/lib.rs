//! Hew runtime: `quic` module.
//!
//! Provides QUIC transport protocol functionality for compiled Hew programs.
//! Uses Quinn for QUIC implementation with a blocking FFI API.
//!
//! # Safety
//!
//! All FFI functions perform null-pointer checks and handle errors by returning
//! null pointers or error codes. Memory is managed via `Box::into_raw` and
//! `Box::from_raw`.

use std::ffi::CStr;
use std::net::SocketAddr;
use std::os::raw::c_char;
use std::sync::Arc;

use quinn::{ClientConfig, Endpoint, RecvStream, SendStream, ServerConfig};
use rustls::pki_types::{CertificateDer, PrivateKeyDer};
use tokio::runtime::Runtime;

/// Opaque QUIC endpoint handle.
///
/// Wraps a Quinn [`Endpoint`] and a Tokio runtime.
/// Must be closed with [`hew_quic_endpoint_close`].
#[derive(Debug)]
pub struct HewQuicEndpoint {
    endpoint: Endpoint,
    runtime: Runtime,
}

/// Opaque QUIC connection handle.
///
/// Wraps a Quinn connection.
/// Must be closed with [`hew_quic_connection_close`].
#[derive(Debug)]
pub struct HewQuicConnection {
    connection: quinn::Connection,
    runtime: Arc<Runtime>,
}

/// Opaque QUIC stream handle.
///
/// Wraps Quinn send and receive streams.
/// Must be closed with [`hew_quic_stream_close`].
#[derive(Debug)]
pub struct HewQuicStream {
    send: Option<SendStream>,
    recv: Option<RecvStream>,
    runtime: Arc<Runtime>,
}

/// Parse an address string, supporting both "host:port" and ":port" formats.
///
/// If the address starts with ':', assumes "0.0.0.0:port" for binding.
fn parse_socket_addr(addr: &str) -> Option<SocketAddr> {
    if addr.starts_with(':') {
        // Format like ":4433" - bind to 0.0.0.0
        format!("0.0.0.0{addr}").parse().ok()
    } else {
        // Standard "host:port" format
        addr.parse().ok()
    }
}

/// Load a PEM-encoded certificate chain from a file.
///
/// # Errors
///
/// Returns an error if the file cannot be read or parsed.
fn load_certs(path: &str) -> Result<Vec<CertificateDer<'static>>, Box<dyn std::error::Error>> {
    let cert_file = std::fs::File::open(path)?;
    let mut reader = std::io::BufReader::new(cert_file);
    let certs = rustls_pemfile::certs(&mut reader)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(certs)
}

/// Load a PEM-encoded private key from a file.
///
/// # Errors
///
/// Returns an error if the file cannot be read or parsed.
fn load_key(path: &str) -> Result<PrivateKeyDer<'static>, Box<dyn std::error::Error>> {
    let key_file = std::fs::File::open(path)?;
    let mut reader = std::io::BufReader::new(key_file);

    // Try to read as PKCS#8 or RSA key
    let key = rustls_pemfile::private_key(&mut reader)?
        .ok_or("no private key found in file")?;

    Ok(key)
}

/// Create a QUIC client endpoint.
///
/// Returns a heap-allocated [`HewQuicEndpoint`] on success, or null on error.
///
/// # Safety
///
/// This function is safe to call.
#[no_mangle]
pub extern "C" fn hew_quic_endpoint_new() -> *mut HewQuicEndpoint {
    // Create a runtime for async operations
    let runtime = match tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
    {
        Ok(rt) => rt,
        Err(_) => return std::ptr::null_mut(),
    };

    // Create a client configuration with proper TLS verification
    let mut crypto = rustls::ClientConfig::builder()
        .with_root_certificates(rustls::RootCertStore {
            roots: webpki_roots::TLS_SERVER_ROOTS.iter().cloned().collect(),
        })
        .with_no_client_auth();

    crypto.alpn_protocols = vec![b"hew-quic".to_vec()];

    let client_config = match quinn::crypto::rustls::QuicClientConfig::try_from(crypto) {
        Ok(cfg) => ClientConfig::new(Arc::new(cfg)),
        Err(_) => return std::ptr::null_mut(),
    };

    // Create endpoint within the runtime context
    let socket_addr = match "0.0.0.0:0".parse() {
        Ok(addr) => addr,
        Err(_) => return std::ptr::null_mut(),
    };

    // Need to create endpoint from within runtime context
    let mut endpoint = match runtime.block_on(async {
        Endpoint::client(socket_addr)
    }) {
        Ok(ep) => ep,
        Err(_) => return std::ptr::null_mut(),
    };

    endpoint.set_default_client_config(client_config);

    Box::into_raw(Box::new(HewQuicEndpoint { endpoint, runtime }))
}

/// Create a QUIC server endpoint listening on the given address.
///
/// Returns a heap-allocated [`HewQuicEndpoint`] on success, or null on error.
///
/// # Safety
///
/// * `addr` must be a valid NUL-terminated C string.
/// * `cert_path` must be a valid NUL-terminated C string pointing to a PEM certificate.
/// * `key_path` must be a valid NUL-terminated C string pointing to a PEM private key.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_endpoint_listen(
    addr: *const c_char,
    cert_path: *const c_char,
    key_path: *const c_char,
) -> *mut HewQuicEndpoint {
    if addr.is_null() || cert_path.is_null() || key_path.is_null() {
        return std::ptr::null_mut();
    }

    // SAFETY: Caller guarantees these are valid C strings
    let addr_str = match unsafe { CStr::from_ptr(addr) }.to_str() {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };
    // SAFETY: Caller guarantees these are valid C strings
    let cert_str = match unsafe { CStr::from_ptr(cert_path) }.to_str() {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };
    // SAFETY: Caller guarantees this is a valid C string
    let key_str = match unsafe { CStr::from_ptr(key_path) }.to_str() {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    // Parse address - support ":port" format
    let socket_addr = match parse_socket_addr(addr_str) {
        Some(addr) => addr,
        None => return std::ptr::null_mut(),
    };

    // Load certificates and key
    let certs = match load_certs(cert_str) {
        Ok(c) => c,
        Err(_) => return std::ptr::null_mut(),
    };
    let key = match load_key(key_str) {
        Ok(k) => k,
        Err(_) => return std::ptr::null_mut(),
    };

    // Create server configuration
    let mut server_crypto = match rustls::ServerConfig::builder()
        .with_no_client_auth()
        .with_single_cert(certs, key)
    {
        Ok(cfg) => cfg,
        Err(_) => return std::ptr::null_mut(),
    };

    server_crypto.alpn_protocols = vec![b"hew-quic".to_vec()];

    let quic_server_config = match quinn::crypto::rustls::QuicServerConfig::try_from(server_crypto) {
        Ok(cfg) => cfg,
        Err(_) => return std::ptr::null_mut(),
    };

    let server_config = ServerConfig::with_crypto(Arc::new(quic_server_config));

    // Create runtime
    let runtime = match tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
    {
        Ok(rt) => rt,
        Err(_) => return std::ptr::null_mut(),
    };

    // Create endpoint within runtime context
    let endpoint = match runtime.block_on(async {
        Endpoint::server(server_config, socket_addr)
    }) {
        Ok(ep) => ep,
        Err(_) => return std::ptr::null_mut(),
    };

    Box::into_raw(Box::new(HewQuicEndpoint { endpoint, runtime }))
}

/// Accept an incoming QUIC connection (server-side).
///
/// Blocks until a connection is available. Returns null on error.
///
/// # Safety
///
/// `endpoint` must be a valid pointer returned by [`hew_quic_endpoint_listen`].
#[no_mangle]
pub unsafe extern "C" fn hew_quic_endpoint_accept(
    endpoint: *mut HewQuicEndpoint,
) -> *mut HewQuicConnection {
    if endpoint.is_null() {
        return std::ptr::null_mut();
    }

    // SAFETY: Caller guarantees this is a valid HewQuicEndpoint
    let ep = unsafe { &*endpoint };

    // Block on accepting a connection
    let incoming = match ep.runtime.block_on(async { ep.endpoint.accept().await }) {
        Some(conn) => conn,
        None => return std::ptr::null_mut(),
    };

    let connection = match ep.runtime.block_on(async { incoming.await }) {
        Ok(conn) => conn,
        Err(_) => return std::ptr::null_mut(),
    };

    let runtime = match tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
    {
        Ok(rt) => Arc::new(rt),
        Err(_) => return std::ptr::null_mut(),
    };

    Box::into_raw(Box::new(HewQuicConnection {
        connection,
        runtime,
    }))
}

/// Connect to a remote QUIC server (client-side).
///
/// Returns a heap-allocated [`HewQuicConnection`] on success, or null on error.
///
/// # Safety
///
/// * `endpoint` must be a valid pointer returned by [`hew_quic_endpoint_new`].
/// * `addr` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_endpoint_connect(
    endpoint: *mut HewQuicEndpoint,
    addr: *const c_char,
) -> *mut HewQuicConnection {
    if endpoint.is_null() || addr.is_null() {
        return std::ptr::null_mut();
    }

    // SAFETY: Caller guarantees these are valid
    let ep = unsafe { &*endpoint };
    // SAFETY: Caller guarantees this is a valid C string
    let addr_str = match unsafe { CStr::from_ptr(addr) }.to_str() {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    // Parse address - support ":port" format and standard "host:port"
    let socket_addr = match parse_socket_addr(addr_str) {
        Some(addr) => addr,
        None => return std::ptr::null_mut(),
    };

    // Extract hostname for SNI from the address string
    // Support formats: "hostname:port" or "ip:port"
    let server_name = addr_str
        .split(':')
        .next()
        .unwrap_or("localhost");

    // Connect with proper SNI
    let connecting = match ep.endpoint.connect(socket_addr, server_name) {
        Ok(conn) => conn,
        Err(_) => return std::ptr::null_mut(),
    };

    let connection = match ep.runtime.block_on(connecting) {
        Ok(conn) => conn,
        Err(_) => return std::ptr::null_mut(),
    };

    let runtime = match tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
    {
        Ok(rt) => Arc::new(rt),
        Err(_) => return std::ptr::null_mut(),
    };

    Box::into_raw(Box::new(HewQuicConnection {
        connection,
        runtime,
    }))
}

/// Close a QUIC endpoint and free resources.
///
/// # Safety
///
/// `endpoint` must be a valid pointer from [`hew_quic_endpoint_new`] or
/// [`hew_quic_endpoint_listen`], and must not have been closed already.
/// Passing null is a no-op.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_endpoint_close(endpoint: *mut HewQuicEndpoint) {
    if endpoint.is_null() {
        return;
    }
    // SAFETY: Caller guarantees this pointer came from Box::into_raw
    let ep = unsafe { Box::from_raw(endpoint) };
    ep.endpoint.close(0u32.into(), b"closed");
    // Wait for the endpoint to finish closing
    ep.runtime.block_on(ep.endpoint.wait_idle());
}

/// Open a new bidirectional stream on the connection.
///
/// Returns a heap-allocated [`HewQuicStream`] on success, or null on error.
///
/// # Safety
///
/// `conn` must be a valid pointer returned by [`hew_quic_endpoint_accept`] or
/// [`hew_quic_endpoint_connect`].
#[no_mangle]
pub unsafe extern "C" fn hew_quic_connection_open_stream(
    conn: *mut HewQuicConnection,
) -> *mut HewQuicStream {
    if conn.is_null() {
        return std::ptr::null_mut();
    }

    // SAFETY: Caller guarantees valid pointer
    let connection = unsafe { &*conn };

    let (send, recv) = match connection.runtime.block_on(connection.connection.open_bi()) {
        Ok(streams) => streams,
        Err(_) => return std::ptr::null_mut(),
    };

    Box::into_raw(Box::new(HewQuicStream {
        send: Some(send),
        recv: Some(recv),
        runtime: connection.runtime.clone(),
    }))
}

/// Accept an incoming stream from the remote peer.
///
/// Blocks until a stream is available. Returns null on connection close.
///
/// # Safety
///
/// `conn` must be a valid pointer returned by [`hew_quic_endpoint_accept`] or
/// [`hew_quic_endpoint_connect`].
#[no_mangle]
pub unsafe extern "C" fn hew_quic_connection_accept_stream(
    conn: *mut HewQuicConnection,
) -> *mut HewQuicStream {
    if conn.is_null() {
        return std::ptr::null_mut();
    }

    // SAFETY: Caller guarantees valid pointer
    let connection = unsafe { &*conn };

    let (send, recv) = match connection.runtime.block_on(connection.connection.accept_bi()) {
        Ok(streams) => streams,
        Err(_) => return std::ptr::null_mut(),
    };

    Box::into_raw(Box::new(HewQuicStream {
        send: Some(send),
        recv: Some(recv),
        runtime: connection.runtime.clone(),
    }))
}

/// Close a QUIC connection gracefully.
///
/// # Safety
///
/// `conn` must be a valid pointer from connection functions, and must not
/// have been closed already. Passing null is a no-op.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_connection_close(conn: *mut HewQuicConnection) {
    if conn.is_null() {
        return;
    }
    // SAFETY: Caller guarantees this pointer came from Box::into_raw
    let connection = unsafe { Box::from_raw(conn) };
    connection.connection.close(0u32.into(), b"closed");
}

/// Get the remote address of a QUIC connection.
///
/// Returns a heap-allocated C string (via `libc::malloc`), or null on error.
///
/// # Safety
///
/// `conn` must be a valid pointer returned by connection functions.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_connection_remote_address(
    conn: *mut HewQuicConnection,
) -> *mut c_char {
    if conn.is_null() {
        return std::ptr::null_mut();
    }

    // SAFETY: Caller guarantees valid pointer
    let connection = unsafe { &*conn };
    let addr = connection.connection.remote_address().to_string();

    // Allocate C string
    let c_str = match std::ffi::CString::new(addr) {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let len = c_str.as_bytes_with_nul().len();
    // SAFETY: Requesting len bytes from malloc
    let ptr = unsafe { libc::malloc(len) }.cast::<c_char>();
    if ptr.is_null() {
        return ptr;
    }

    // SAFETY: Both pointers are valid for len bytes
    unsafe {
        std::ptr::copy_nonoverlapping(c_str.as_ptr(), ptr, len);
    }

    ptr
}

/// Send data over a QUIC stream.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// * `stream` must be a valid pointer from stream functions.
/// * `vec` must be a valid, non-null pointer to a `HewVec` (i32 elements, one per byte).
#[no_mangle]
pub unsafe extern "C" fn hew_quic_stream_send(
    stream: *mut HewQuicStream,
    vec: *mut hew_runtime::vec::HewVec,
) -> i32 {
    if stream.is_null() || vec.is_null() {
        return -1;
    }

    // SAFETY: Caller guarantees valid pointer
    let stream_ref = unsafe { &mut *stream };

    let send = match stream_ref.send.as_mut() {
        Some(s) => s,
        None => return -1,
    };

    // SAFETY: caller guarantees `vec` is a valid HewVec pointer
    let len = unsafe { hew_runtime::vec::hew_vec_len(vec) };
    #[expect(clippy::cast_sign_loss, reason = "vec len is always non-negative")]
    #[expect(
        clippy::cast_possible_truncation,
        reason = "vec len fits in usize on all platforms"
    )]
    let mut data = Vec::with_capacity(len as usize);
    for i in 0..len {
        // SAFETY: i < len, so index is in bounds
        let byte = unsafe { hew_runtime::vec::hew_vec_get_i32(vec, i) };
        #[expect(clippy::cast_sign_loss, reason = "byte values are 0-255")]
        data.push(byte as u8);
    }

    match stream_ref.runtime.block_on(send.write_all(&data)) {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

/// Receive data from a QUIC stream.
///
/// Returns a pointer to a heap-allocated `HewVec` (i32 elements, one per byte).
/// Returns an empty `HewVec` (not null) on stream close or error.
///
/// # Safety
///
/// `stream` must be a valid pointer from stream functions.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_stream_recv(
    stream: *mut HewQuicStream,
) -> *mut hew_runtime::vec::HewVec {
    // SAFETY: hew_vec_new allocates and returns a valid HewVec; we own it
    let v = unsafe { hew_runtime::vec::hew_vec_new() };

    if stream.is_null() {
        return v;
    }

    // SAFETY: Caller guarantees valid pointer
    let stream_ref = unsafe { &mut *stream };

    let recv = match stream_ref.recv.as_mut() {
        Some(r) => r,
        None => return v,
    };

    // Read available data
    let chunk = match stream_ref.runtime.block_on(recv.read_chunk(usize::MAX, true)) {
        Ok(Some(chunk)) => chunk,
        Ok(None) | Err(_) => return v,
    };

    // Convert bytes to i32 HewVec elements
    for &byte in &chunk.bytes {
        // SAFETY: v was allocated by hew_vec_new and is non-null
        unsafe { hew_runtime::vec::hew_vec_push_i32(v, i32::from(byte)) };
    }

    v
}

/// Finish sending on a QUIC stream.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// `stream` must be a valid pointer from stream functions.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_stream_finish(stream: *mut HewQuicStream) -> i32 {
    if stream.is_null() {
        return -1;
    }

    // SAFETY: Caller guarantees valid pointer
    let stream_ref = unsafe { &mut *stream };

    let mut send = match stream_ref.send.take() {
        Some(s) => s,
        None => return -1,
    };

    // finish() is not async, just call it directly
    match send.finish() {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

/// Close a QUIC stream and free resources.
///
/// # Safety
///
/// `stream` must be a valid pointer from stream functions, and must not
/// have been closed already. Passing null is a no-op.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_stream_close(stream: *mut HewQuicStream) {
    if stream.is_null() {
        return;
    }
    // SAFETY: Caller guarantees this pointer came from Box::into_raw
    let mut stream_obj = unsafe { Box::from_raw(stream) };

    // Finish send if still open - finish() is not async
    if let Some(mut send) = stream_obj.send.take() {
        let _ = send.finish();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn endpoint_new_returns_valid_pointer() {
        // Install crypto provider for tests
        let _ = rustls::crypto::ring::default_provider().install_default();

        let endpoint = hew_quic_endpoint_new();
        assert!(!endpoint.is_null(), "endpoint should not be null");
        // SAFETY: We just created this pointer
        unsafe { hew_quic_endpoint_close(endpoint) };
    }

    #[test]
    fn endpoint_close_null_is_noop() {
        // SAFETY: Passing null is explicitly handled
        unsafe { hew_quic_endpoint_close(std::ptr::null_mut()) };
    }
}
