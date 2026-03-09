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

/// Get the current round-trip time (RTT) estimate for a connection.
///
/// Returns the RTT in milliseconds, or -1 if unavailable.
///
/// # Safety
///
/// `conn` must be a valid pointer returned by connection functions.
#[no_mangle]
#[expect(clippy::cast_possible_truncation, reason = "RTT in ms fits in i32")]
pub unsafe extern "C" fn hew_quic_connection_rtt_ms(conn: *mut HewQuicConnection) -> i32 {
    if conn.is_null() {
        return -1;
    }

    // SAFETY: Caller guarantees valid pointer
    let connection = unsafe { &*conn };

    // Get RTT stats from Quinn - stats() doesn't require runtime context
    let stats = connection.connection.stats();

    stats.path.rtt.as_millis() as i32
}

/// Get the number of bytes sent on a connection.
///
/// Returns the total bytes sent, or -1 on error.
///
/// # Safety
///
/// `conn` must be a valid pointer returned by connection functions.
#[no_mangle]
#[expect(clippy::cast_possible_wrap, reason = "u64 to i64 conversion for FFI compatibility")]
pub unsafe extern "C" fn hew_quic_connection_bytes_sent(conn: *mut HewQuicConnection) -> i64 {
    if conn.is_null() {
        return -1;
    }

    // SAFETY: Caller guarantees valid pointer
    let connection = unsafe { &*conn };

    // Get bytes sent from Quinn stats
    let stats = connection.connection.stats();

    // Note: sent_packets is packet count, not bytes. Use udp_tx.bytes for actual byte count
    stats.udp_tx.bytes as i64
}

/// Get the number of bytes received on a connection.
///
/// Returns the total bytes received, or -1 on error.
///
/// # Safety
///
/// `conn` must be a valid pointer returned by connection functions.
#[no_mangle]
#[expect(clippy::cast_possible_wrap, reason = "u64 to i64 conversion for FFI compatibility")]
pub unsafe extern "C" fn hew_quic_connection_bytes_received(conn: *mut HewQuicConnection) -> i64 {
    if conn.is_null() {
        return -1;
    }

    // SAFETY: Caller guarantees valid pointer
    let connection = unsafe { &*conn };

    // Get bytes received from Quinn stats
    let stats = connection.connection.stats();

    stats.udp_rx.bytes as i64
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
    use std::ffi::CString;

    /// Generate a self-signed certificate for testing.
    ///
    /// Returns (cert_pem, key_pem) as byte vectors.
    fn generate_self_signed_cert() -> (Vec<u8>, Vec<u8>) {
        let cert = rcgen::generate_simple_self_signed(vec!["localhost".to_string()]).unwrap();
        let cert_pem = cert.cert.pem();
        let key_pem = cert.key_pair.serialize_pem();

        (cert_pem.into_bytes(), key_pem.into_bytes())
    }

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

    #[test]
    #[ignore] // Requires actual network and TLS setup - run with `cargo test -- --ignored`
    fn client_server_stream_roundtrip() {
        // Install crypto provider for tests
        let _ = rustls::crypto::ring::default_provider().install_default();

        // Generate test certificates
        let (cert_pem, key_pem) = generate_self_signed_cert();
        let cert_path = std::env::temp_dir().join("quic_test_cert.pem");
        let key_path = std::env::temp_dir().join("quic_test_key.pem");
        std::fs::write(&cert_path, &cert_pem).unwrap();
        std::fs::write(&key_path, &key_pem).unwrap();

        let cert_path_str = CString::new(cert_path.to_str().unwrap()).unwrap();
        let key_path_str = CString::new(key_path.to_str().unwrap()).unwrap();

        // Start server on localhost:0 (random port)
        let server_addr = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: Valid C strings
        let server_endpoint = unsafe {
            hew_quic_endpoint_listen(
                server_addr.as_ptr(),
                cert_path_str.as_ptr(),
                key_path_str.as_ptr(),
            )
        };
        assert!(!server_endpoint.is_null(), "server endpoint should not be null");

        // Get the actual port the server bound to
        // SAFETY: Valid pointer
        let server_ep = unsafe { &*server_endpoint };
        let actual_addr = server_ep.endpoint.local_addr().unwrap();

        // Convert pointer to usize for thread safety
        let server_endpoint_addr = server_endpoint as usize;

        // Spawn server thread
        let server_handle = std::thread::spawn(move || {
            // SAFETY: Valid pointer, owned by this thread
            unsafe {
                // Convert back to pointer
                let server_endpoint = server_endpoint_addr as *mut HewQuicEndpoint;

                // Accept connection
                let conn = hew_quic_endpoint_accept(server_endpoint);
                assert!(!conn.is_null(), "server connection should not be null");

                // Accept stream
                let stream = hew_quic_connection_accept_stream(conn);
                assert!(!stream.is_null(), "server stream should not be null");

                // Receive data
                let recv_vec = hew_quic_stream_recv(stream);
                assert!(!recv_vec.is_null(), "received data should not be null");

                // Echo data back
                let send_result = hew_quic_stream_send(stream, recv_vec);
                assert_eq!(send_result, 0, "send should succeed");

                // Finish and close
                let finish_result = hew_quic_stream_finish(stream);
                assert_eq!(finish_result, 0, "finish should succeed");
                hew_quic_stream_close(stream);
                hew_quic_connection_close(conn);
                hew_quic_endpoint_close(server_endpoint);

                // Clean up HewVec
                hew_runtime::vec::hew_vec_free(recv_vec);
            }
        });

        // Give server time to start accepting
        std::thread::sleep(std::time::Duration::from_millis(100));

        // Create client endpoint
        let client_endpoint = hew_quic_endpoint_new();
        assert!(!client_endpoint.is_null(), "client endpoint should not be null");

        // Connect to server (use IP address to avoid SNI issues with self-signed cert)
        let client_addr = CString::new(format!("127.0.0.1:{}", actual_addr.port())).unwrap();
        // SAFETY: Valid pointers
        let conn = unsafe { hew_quic_endpoint_connect(client_endpoint, client_addr.as_ptr()) };
        assert!(!conn.is_null(), "client connection should not be null");

        // Open stream
        // SAFETY: Valid pointer
        let stream = unsafe { hew_quic_connection_open_stream(conn) };
        assert!(!stream.is_null(), "client stream should not be null");

        // Send test data
        // SAFETY: Create and populate a HewVec
        let send_vec = unsafe { hew_runtime::vec::hew_vec_new() };
        let test_data = b"Hello, QUIC!";
        for &byte in test_data {
            // SAFETY: Valid HewVec
            unsafe { hew_runtime::vec::hew_vec_push_i32(send_vec, i32::from(byte)) };
        }

        // SAFETY: Valid pointers
        let send_result = unsafe { hew_quic_stream_send(stream, send_vec) };
        assert_eq!(send_result, 0, "send should succeed");

        // Finish sending
        // SAFETY: Valid pointer
        let finish_result = unsafe { hew_quic_stream_finish(stream) };
        assert_eq!(finish_result, 0, "finish should succeed");

        // Receive echo
        // SAFETY: Valid pointer
        let recv_vec = unsafe { hew_quic_stream_recv(stream) };
        assert!(!recv_vec.is_null(), "received data should not be null");

        // Verify data
        // SAFETY: Valid HewVec
        let recv_len = unsafe { hew_runtime::vec::hew_vec_len(recv_vec) };
        assert_eq!(recv_len, test_data.len() as i64, "received length should match");

        for (i, &expected_byte) in test_data.iter().enumerate() {
            // SAFETY: i < recv_len
            let actual_byte = unsafe { hew_runtime::vec::hew_vec_get_i32(recv_vec, i as i64) };
            assert_eq!(
                actual_byte,
                i32::from(expected_byte),
                "byte {} should match",
                i
            );
        }

        // Clean up
        // SAFETY: Valid pointers
        unsafe {
            hew_runtime::vec::hew_vec_free(send_vec);
            hew_runtime::vec::hew_vec_free(recv_vec);
            hew_quic_stream_close(stream);
            hew_quic_connection_close(conn);
            hew_quic_endpoint_close(client_endpoint);
        }

        // Wait for server to finish
        server_handle.join().unwrap();

        // Clean up temp files
        let _ = std::fs::remove_file(cert_path);
        let _ = std::fs::remove_file(key_path);
    }

    #[test]
    #[ignore] // Requires actual network and TLS setup - run with `cargo test -- --ignored`
    fn connection_observation_hooks() {
        // Install crypto provider for tests
        let _ = rustls::crypto::ring::default_provider().install_default();

        // Generate test certificates
        let (cert_pem, key_pem) = generate_self_signed_cert();
        let cert_path = std::env::temp_dir().join("quic_obs_test_cert.pem");
        let key_path = std::env::temp_dir().join("quic_obs_test_key.pem");
        std::fs::write(&cert_path, &cert_pem).unwrap();
        std::fs::write(&key_path, &key_pem).unwrap();

        let cert_path_str = CString::new(cert_path.to_str().unwrap()).unwrap();
        let key_path_str = CString::new(key_path.to_str().unwrap()).unwrap();

        // Start server
        let server_addr = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: Valid C strings
        let server_endpoint = unsafe {
            hew_quic_endpoint_listen(
                server_addr.as_ptr(),
                cert_path_str.as_ptr(),
                key_path_str.as_ptr(),
            )
        };
        assert!(!server_endpoint.is_null());

        // SAFETY: Valid pointer
        let server_ep = unsafe { &*server_endpoint };
        let actual_addr = server_ep.endpoint.local_addr().unwrap();

        // Convert pointer to usize for thread safety
        let server_endpoint_addr = server_endpoint as usize;

        // Spawn server thread
        let server_handle = std::thread::spawn(move || {
            // SAFETY: Valid pointer, owned by this thread
            unsafe {
                // Convert back to pointer
                let server_endpoint = server_endpoint_addr as *mut HewQuicEndpoint;

                let conn = hew_quic_endpoint_accept(server_endpoint);
                assert!(!conn.is_null());

                // Test observation hooks on server side
                let rtt = hew_quic_connection_rtt_ms(conn);
                assert!(rtt >= 0, "RTT should be non-negative");

                let bytes_sent = hew_quic_connection_bytes_sent(conn);
                assert!(bytes_sent >= 0, "bytes_sent should be non-negative");

                let bytes_received = hew_quic_connection_bytes_received(conn);
                assert!(bytes_received >= 0, "bytes_received should be non-negative");

                let stream = hew_quic_connection_accept_stream(conn);
                let recv_vec = hew_quic_stream_recv(stream);
                hew_quic_stream_send(stream, recv_vec);
                hew_quic_stream_finish(stream);

                // Check stats after data transfer
                let bytes_sent_after = hew_quic_connection_bytes_sent(conn);
                assert!(
                    bytes_sent_after >= bytes_sent,
                    "bytes_sent should increase after sending"
                );

                hew_quic_stream_close(stream);
                hew_quic_connection_close(conn);
                hew_quic_endpoint_close(server_endpoint);

                hew_runtime::vec::hew_vec_free(recv_vec);
            }
        });

        std::thread::sleep(std::time::Duration::from_millis(100));

        // Create client
        let client_endpoint = hew_quic_endpoint_new();
        let client_addr = CString::new(format!("127.0.0.1:{}", actual_addr.port())).unwrap();
        // SAFETY: Valid pointers
        let conn = unsafe { hew_quic_endpoint_connect(client_endpoint, client_addr.as_ptr()) };
        assert!(!conn.is_null());

        // Test observation hooks on client side
        // SAFETY: Valid pointer
        let rtt = unsafe { hew_quic_connection_rtt_ms(conn) };
        assert!(rtt >= 0, "RTT should be non-negative");

        // SAFETY: Valid pointer
        let bytes_sent_before = unsafe { hew_quic_connection_bytes_sent(conn) };
        assert!(bytes_sent_before >= 0);

        // SAFETY: Valid pointer
        let stream = unsafe { hew_quic_connection_open_stream(conn) };
        let send_vec = unsafe { hew_runtime::vec::hew_vec_new() };
        for &byte in b"test" {
            unsafe { hew_runtime::vec::hew_vec_push_i32(send_vec, i32::from(byte)) };
        }
        // SAFETY: Valid pointers
        unsafe {
            hew_quic_stream_send(stream, send_vec);
            hew_quic_stream_finish(stream);
        }

        // SAFETY: Valid pointer
        let recv_vec = unsafe { hew_quic_stream_recv(stream) };

        // Check stats after data transfer
        // SAFETY: Valid pointer
        let bytes_sent_after = unsafe { hew_quic_connection_bytes_sent(conn) };
        assert!(
            bytes_sent_after >= bytes_sent_before,
            "bytes_sent should increase"
        );

        // SAFETY: Valid pointer
        let bytes_received = unsafe { hew_quic_connection_bytes_received(conn) };
        assert!(bytes_received >= 0);

        // Clean up
        // SAFETY: Valid pointers
        unsafe {
            hew_runtime::vec::hew_vec_free(send_vec);
            hew_runtime::vec::hew_vec_free(recv_vec);
            hew_quic_stream_close(stream);
            hew_quic_connection_close(conn);
            hew_quic_endpoint_close(client_endpoint);
        }

        server_handle.join().unwrap();

        let _ = std::fs::remove_file(cert_path);
        let _ = std::fs::remove_file(key_path);
    }
}
