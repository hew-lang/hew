//! Hew `std::net::quic` — QUIC transport for internode messaging.
//!
//! Exposes a blocking C ABI over Quinn (async QUIC) by embedding a dedicated
//! Tokio runtime inside each `HewQuicEndpoint`. All `extern "C"` functions are
//! designed to be called from compiled Hew programs via FFI.
//!
//! # Memory model
//!
//! Every heap-allocated handle (`HewQuicEndpoint`, `HewQuicConn`,
//! `HewQuicStream`, `HewQuicEvent`) is owned by the caller and must be freed
//! by the corresponding close/disconnect/finish/free function. Passing `null`
//! to any function is safe and results in a no-op or error return value.

use std::ffi::CStr;
use std::net::SocketAddr;
use std::os::raw::{c_char, c_int};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use quinn::{ClientConfig, Connection, Endpoint, RecvStream, SendStream, ServerConfig};
use rustls::pki_types::{CertificateDer, PrivateKeyDer, PrivatePkcs8KeyDer};
use tokio::runtime::Runtime;

/// Maximum bytes read in a single [`hew_quic_stream_recv`] call.
///
/// 64 KiB is chosen because it matches the default QUIC maximum stream data
/// window and ensures a single `read()` call is always large enough to drain
/// one QUIC STREAM frame payload.
const RECV_BUFFER_SIZE: usize = 65536;

/// Timeout (seconds) for [`hew_quic_endpoint_on_event`] to wait for the next
/// incoming connection attempt before returning an error event.
const ENDPOINT_EVENT_TIMEOUT_SECS: u64 = 30;

// ── TLS helpers ───────────────────────────────────────────────────────────────

/// A rustls [`ClientConfig`] that skips all certificate verification.
///
/// **Development only.** Production code should supply a verified CA bundle.
#[derive(Debug)]
struct NoVerifier;

impl rustls::client::danger::ServerCertVerifier for NoVerifier {
    fn verify_server_cert(
        &self,
        _end_entity: &CertificateDer<'_>,
        _intermediates: &[CertificateDer<'_>],
        _server_name: &rustls::pki_types::ServerName<'_>,
        _ocsp: &[u8],
        _now: rustls::pki_types::UnixTime,
    ) -> Result<rustls::client::danger::ServerCertVerified, rustls::Error> {
        Ok(rustls::client::danger::ServerCertVerified::assertion())
    }

    fn verify_tls12_signature(
        &self,
        _message: &[u8],
        _cert: &CertificateDer<'_>,
        _dss: &rustls::DigitallySignedStruct,
    ) -> Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        Ok(rustls::client::danger::HandshakeSignatureValid::assertion())
    }

    fn verify_tls13_signature(
        &self,
        _message: &[u8],
        _cert: &CertificateDer<'_>,
        _dss: &rustls::DigitallySignedStruct,
    ) -> Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        Ok(rustls::client::danger::HandshakeSignatureValid::assertion())
    }

    fn supported_verify_schemes(&self) -> Vec<rustls::SignatureScheme> {
        rustls::crypto::ring::default_provider()
            .signature_verification_algorithms
            .supported_schemes()
    }
}

/// Build a development-only [`ClientConfig`] that skips certificate verification.
///
/// Uses the Ring crypto provider explicitly to avoid conflicts when multiple
/// rustls providers are registered in the same test process.
fn insecure_client_config() -> ClientConfig {
    let ring = Arc::new(rustls::crypto::ring::default_provider());
    let crypto = rustls::ClientConfig::builder_with_provider(ring)
        .with_protocol_versions(rustls::DEFAULT_VERSIONS)
        .expect("valid TLS versions")
        .dangerous()
        .with_custom_certificate_verifier(Arc::new(NoVerifier))
        .with_no_client_auth();
    ClientConfig::new(Arc::new(
        quinn::crypto::rustls::QuicClientConfig::try_from(crypto)
            .expect("valid crypto config"),
    ))
}

/// Generate a self-signed certificate for development server use.
///
/// Uses the Ring crypto provider explicitly to avoid conflicts when multiple
/// rustls providers are registered in the same test process.
fn self_signed_server_config() -> Result<ServerConfig, Box<dyn std::error::Error>> {
    let cert = rcgen::generate_simple_self_signed(vec!["localhost".into()])?;
    let cert_der = CertificateDer::from(cert.cert);
    let key_der = PrivateKeyDer::Pkcs8(PrivatePkcs8KeyDer::from(cert.key_pair.serialize_der()));

    let ring = Arc::new(rustls::crypto::ring::default_provider());
    let crypto = rustls::ServerConfig::builder_with_provider(ring)
        .with_protocol_versions(rustls::DEFAULT_VERSIONS)?
        .with_no_client_auth()
        .with_single_cert(vec![cert_der], key_der)?;

    Ok(ServerConfig::with_crypto(Arc::new(
        quinn::crypto::rustls::QuicServerConfig::try_from(crypto)?,
    )))
}

// ── Opaque handle types ───────────────────────────────────────────────────────

/// Opaque handle for a QUIC endpoint.
///
/// Owns the [`Endpoint`] and the Tokio [`Runtime`] that drives it.
/// The runtime is wrapped in `Arc` so that connections and streams derived
/// from this endpoint can share it — Quinn's [`Connection`] must be driven
/// by the same runtime context that created it.
#[derive(Debug)]
pub struct HewQuicEndpoint {
    rt: Arc<Runtime>,
    endpoint: Endpoint,
}

/// Opaque handle for an established QUIC connection.
#[derive(Debug)]
pub struct HewQuicConn {
    rt: Arc<Runtime>,
    conn: Connection,
}

/// Opaque handle for an open bidirectional QUIC stream.
#[derive(Debug)]
pub struct HewQuicStream {
    rt: Arc<Runtime>,
    send: Mutex<SendStream>,
    recv: Mutex<RecvStream>,
}

/// An observed QUIC event.
#[repr(C)]
#[derive(Debug)]
pub struct HewQuicEvent {
    /// 0 = connected, 1 = disconnected, 2 = stream opened,
    /// 3 = stream closed, 4 = data ready, -1 = error.
    pub kind: i32,
}

/// Heap-allocated bytes returned to the caller.
///
/// Must be freed with `libc::free(ptr)` after use; the caller is responsible.
struct AllocBytes {
    ptr: *mut u8,
    len: usize,
}

impl AllocBytes {
    /// Allocate a copy of `src` via `libc::malloc`.
    ///
    /// Returns `(ptr, len)`.  On allocation failure both are 0/null.
    ///
    /// # Safety
    ///
    /// `src` must be a valid byte slice for its entire length.
    unsafe fn from_slice(src: &[u8]) -> Self {
        let len = src.len();
        if len == 0 {
            return Self {
                ptr: std::ptr::null_mut(),
                len: 0,
            };
        }
        // SAFETY: We request `len` bytes; caller guarantees src is valid.
        let ptr = unsafe { libc::malloc(len) }.cast::<u8>();
        if !ptr.is_null() {
            // SAFETY: ptr is freshly allocated for `len` bytes; src is valid.
            unsafe { std::ptr::copy_nonoverlapping(src.as_ptr(), ptr, len) };
        }
        Self { ptr, len }
    }
}

// ── Endpoint constructors ─────────────────────────────────────────────────────

/// Create a QUIC client endpoint bound to an ephemeral local port.
///
/// Returns a heap-allocated [`HewQuicEndpoint`], or null on failure.
///
/// # Safety
///
/// The returned pointer must be released with [`hew_quic_endpoint_close`].
///
/// # Panics
///
/// Panics if `"0.0.0.0:0"` cannot be parsed as a socket address (impossible
/// at runtime since it is a compile-time literal).
#[no_mangle]
pub extern "C" fn hew_quic_new_client() -> *mut HewQuicEndpoint {
    let Ok(rt) = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
    else {
        return std::ptr::null_mut();
    };
    let rt = Arc::new(rt);

    let endpoint = rt.block_on(async {
        let mut ep = Endpoint::client("0.0.0.0:0".parse::<SocketAddr>().unwrap()).ok()?;
        ep.set_default_client_config(insecure_client_config());
        Some(ep)
    });

    match endpoint {
        Some(ep) => Box::into_raw(Box::new(HewQuicEndpoint { rt, endpoint: ep })),
        None => std::ptr::null_mut(),
    }
}

/// Bind a QUIC server endpoint on `addr`.
///
/// `addr` must be a NUL-terminated `"host:port"` or `":port"` C string.
/// Returns a heap-allocated [`HewQuicEndpoint`], or null on failure.
///
/// # Safety
///
/// * `addr` must be a valid NUL-terminated C string.
/// * The returned pointer must be released with [`hew_quic_endpoint_close`].
///
/// # Panics
///
/// Panics if the fallback address `"0.0.0.0:4433"` cannot be parsed (impossible
/// at runtime since it is a compile-time literal).
#[no_mangle]
pub unsafe extern "C" fn hew_quic_new_server(addr: *const c_char) -> *mut HewQuicEndpoint {
    if addr.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `addr` is valid NUL-terminated.
    let Ok(addr_str) = unsafe { CStr::from_ptr(addr) }.to_str() else {
        return std::ptr::null_mut();
    };

    // Resolve `:port` shorthand to `0.0.0.0:port`.
    let bind_addr: SocketAddr = if addr_str.starts_with(':') {
        format!("0.0.0.0{addr_str}").parse().ok()
    } else {
        addr_str.parse().ok()
    }
    .unwrap_or_else(|| "0.0.0.0:4433".parse().unwrap());

    let Ok(rt) = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
    else {
        return std::ptr::null_mut();
    };
    let rt = Arc::new(rt);

    let endpoint = rt.block_on(async {
        let server_cfg = self_signed_server_config().ok()?;
        Endpoint::server(server_cfg, bind_addr).ok()
    });

    match endpoint {
        Some(ep) => Box::into_raw(Box::new(HewQuicEndpoint { rt, endpoint: ep })),
        None => std::ptr::null_mut(),
    }
}

// ── Endpoint methods ──────────────────────────────────────────────────────────

/// Dial a remote QUIC server.
///
/// Blocks until the connection is established (or fails).
/// Returns a heap-allocated [`HewQuicConn`], or null on failure.
///
/// # Safety
///
/// * `ep` must be a valid pointer returned by a constructor function.
/// * `addr` and `server_name` must be valid NUL-terminated C strings.
/// * The returned pointer must be released with [`hew_quic_conn_disconnect`].
#[no_mangle]
pub unsafe extern "C" fn hew_quic_endpoint_connect(
    ep: *mut HewQuicEndpoint,
    addr: *const c_char,
    server_name: *const c_char,
) -> *mut HewQuicConn {
    if ep.is_null() || addr.is_null() || server_name.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `ep` is a valid pointer per caller contract.
    let endpoint = unsafe { &*ep };

    // SAFETY: caller guarantees these are valid NUL-terminated strings.
    let Ok(addr_str) = (unsafe { CStr::from_ptr(addr) }).to_str() else {
        return std::ptr::null_mut();
    };
    // SAFETY: caller guarantees server_name is a valid NUL-terminated string.
    let Ok(sn_str) = (unsafe { CStr::from_ptr(server_name) }).to_str() else {
        return std::ptr::null_mut();
    };

    let Ok(remote): Result<SocketAddr, _> = addr_str.parse() else {
        return std::ptr::null_mut();
    };

    let conn_result = endpoint.rt.block_on(async {
        let connecting = endpoint.endpoint.connect(remote, sn_str).ok()?;
        connecting.await.ok()
    });

    let Some(conn) = conn_result else {
        return std::ptr::null_mut();
    };

    Box::into_raw(Box::new(HewQuicConn {
        rt: Arc::clone(&endpoint.rt),
        conn,
    }))
}

/// Accept the next incoming connection on a server endpoint.
///
/// Blocks until a remote peer connects (or the endpoint is closed).
/// Returns a heap-allocated [`HewQuicConn`], or null on failure.
///
/// # Safety
///
/// * `ep` must be a valid pointer returned by [`hew_quic_new_server`].
/// * The returned pointer must be released with [`hew_quic_conn_disconnect`].
#[no_mangle]
pub unsafe extern "C" fn hew_quic_endpoint_accept(ep: *mut HewQuicEndpoint) -> *mut HewQuicConn {
    if ep.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `ep` is a valid pointer per caller contract.
    let endpoint = unsafe { &*ep };

    let conn_result = endpoint.rt.block_on(async {
        let incoming = endpoint.endpoint.accept().await?;
        incoming.await.ok()
    });

    let Some(conn) = conn_result else {
        return std::ptr::null_mut();
    };

    Box::into_raw(Box::new(HewQuicConn {
        rt: Arc::clone(&endpoint.rt),
        conn,
    }))
}

/// Close a QUIC endpoint and release its resources.
///
/// # Safety
///
/// `ep` must be a valid pointer returned by a constructor function, and must
/// not have been closed already. Passing null is a no-op.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_endpoint_close(ep: *mut HewQuicEndpoint) {
    if ep.is_null() {
        return;
    }
    // SAFETY: `ep` was allocated with Box::into_raw.
    let endpoint = unsafe { Box::from_raw(ep) };
    endpoint.rt.block_on(async {
        endpoint.endpoint.close(0u32.into(), b"closed");
        endpoint.endpoint.wait_idle().await;
    });
    // Tokio runtime is dropped here after endpoint is fully closed.
}

/// Poll for the next observable event on this endpoint.
///
/// Returns a heap-allocated [`HewQuicEvent`], or null on error.
/// The caller must release it with [`hew_quic_event_free`].
///
/// # Safety
///
/// `ep` must be a valid pointer returned by a constructor function.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_endpoint_on_event(
    ep: *mut HewQuicEndpoint,
) -> *mut HewQuicEvent {
    if ep.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `ep` is a valid pointer per caller contract.
    let endpoint = unsafe { &*ep };

    // Observe the next incoming connection attempt as a "connected" event.
    let kind = endpoint.rt.block_on(async {
        tokio::time::timeout(Duration::from_secs(ENDPOINT_EVENT_TIMEOUT_SECS), async {
            endpoint.endpoint.accept().await.map(|_| 0i32)
        })
        .await
        .unwrap_or(Some(-1))
        .unwrap_or(-1)
    });

    Box::into_raw(Box::new(HewQuicEvent { kind }))
}

// ── Connection methods ────────────────────────────────────────────────────────

/// Open a new bidirectional stream on an established connection.
///
/// Returns a heap-allocated [`HewQuicStream`], or null on failure.
///
/// # Safety
///
/// * `conn` must be a valid pointer returned by a connect/accept function.
/// * The returned pointer must be released by calling `finish` or `stop`.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_conn_open_stream(
    conn: *mut HewQuicConn,
) -> *mut HewQuicStream {
    if conn.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `conn` is a valid pointer per caller contract.
    let c = unsafe { &*conn };

    let result = c
        .rt
        .block_on(async { c.conn.open_bi().await.ok() });

    match result {
        Some((send, recv)) => Box::into_raw(Box::new(HewQuicStream {
            rt: Arc::clone(&c.rt),
            send: Mutex::new(send),
            recv: Mutex::new(recv),
        })),
        None => std::ptr::null_mut(),
    }
}

/// Accept the next incoming bidirectional stream on a connection.
///
/// Blocks until the remote peer opens a stream.
/// Returns a heap-allocated [`HewQuicStream`], or null on failure.
///
/// # Safety
///
/// * `conn` must be a valid pointer returned by a connect/accept function.
/// * The returned pointer must be released by calling `finish` or `stop`.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_conn_accept_stream(
    conn: *mut HewQuicConn,
) -> *mut HewQuicStream {
    if conn.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `conn` is a valid pointer per caller contract.
    let c = unsafe { &*conn };

    let result = c
        .rt
        .block_on(async { c.conn.accept_bi().await.ok() });

    match result {
        Some((send, recv)) => Box::into_raw(Box::new(HewQuicStream {
            rt: Arc::clone(&c.rt),
            send: Mutex::new(send),
            recv: Mutex::new(recv),
        })),
        None => std::ptr::null_mut(),
    }
}

/// Gracefully close a QUIC connection.
///
/// Returns 0 on success, −1 on failure. After this call, `conn` must not be
/// used again even if −1 is returned.
///
/// # Safety
///
/// `conn` must be a valid pointer returned by a connect/accept function,
/// and must not have been disconnected already. Passing null returns −1.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_conn_disconnect(conn: *mut HewQuicConn) -> c_int {
    if conn.is_null() {
        return -1;
    }
    // SAFETY: `conn` was allocated with Box::into_raw.
    let c = unsafe { Box::from_raw(conn) };
    c.conn.close(0u32.into(), b"disconnected");
    0
}

/// Poll for the next observable event on a connection.
///
/// Returns a heap-allocated [`HewQuicEvent`], or null on error.
/// The caller must release it with [`hew_quic_event_free`].
///
/// # Safety
///
/// `conn` must be a valid pointer returned by a connect/accept function.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_conn_on_event(conn: *mut HewQuicConn) -> *mut HewQuicEvent {
    if conn.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `conn` is a valid pointer per caller contract.
    let c = unsafe { &*conn };

    let kind = c.rt.block_on(async {
        tokio::select! {
            result = c.conn.accept_bi() => {
                if result.is_ok() { 2i32 } else { -1i32 }
            }
            result = c.conn.closed() => {
                // closed() returns the error on close; treat it as disconnected.
                let _ = result;
                1i32
            }
        }
    });

    Box::into_raw(Box::new(HewQuicEvent { kind }))
}

// ── Stream methods ────────────────────────────────────────────────────────────

/// Send bytes on a QUIC stream.
///
/// Returns 0 on success, −1 on error.
///
/// # Safety
///
/// * `stream` must be a valid pointer returned by an open/accept function.
/// * `data` must point to at least `len` readable bytes, or be null when `len` is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_stream_send(
    stream: *mut HewQuicStream,
    data: *const u8,
    len: usize,
) -> c_int {
    if stream.is_null() {
        return -1;
    }
    // SAFETY: `stream` is a valid pointer per caller contract.
    let s = unsafe { &*stream };

    let slice: &[u8] = if len == 0 {
        &[]
    } else {
        if data.is_null() {
            return -1;
        }
        // SAFETY: `data` is valid for `len` bytes per caller contract.
        unsafe { std::slice::from_raw_parts(data, len) }
    };

    let Ok(mut send) = s.send.lock() else {
        return -1;
    };
    match s.rt.block_on(async { send.write_all(slice).await }) {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

/// Receive the next chunk of bytes from a QUIC stream (blocking).
///
/// Returns a pointer to a `malloc`-allocated buffer and writes the byte count
/// into `*out_len`. The caller must `free()` the returned pointer when done.
/// Returns null on stream close, error, or when `out_len` is null.
///
/// # Safety
///
/// * `stream` must be a valid pointer returned by an open/accept function.
/// * `out_len` must be a valid non-null pointer to a `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_stream_recv(
    stream: *mut HewQuicStream,
    out_len: *mut usize,
) -> *mut u8 {
    if stream.is_null() || out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `stream` is a valid pointer per caller contract.
    let s = unsafe { &*stream };

    let mut buf = vec![0u8; RECV_BUFFER_SIZE];
    let Ok(mut recv) = s.recv.lock() else {
        return std::ptr::null_mut();
    };
    let result = s
        .rt
        .block_on(async { recv.read(&mut buf).await });

    if let Ok(Some(n)) = result {
        // SAFETY: buf[..n] is valid for `n` bytes.
        let ab = unsafe { AllocBytes::from_slice(&buf[..n]) };
        // SAFETY: out_len is a valid pointer per caller contract.
        unsafe { *out_len = ab.len };
        ab.ptr
    } else {
        // SAFETY: out_len is a valid pointer per caller contract.
        unsafe { *out_len = 0 };
        std::ptr::null_mut()
    }
}

/// Signal end-of-send on a QUIC stream's write side.
///
/// After this call, `send` may not be used again. The receive side of the
/// remote peer will see EOF after all buffered data has been delivered.
/// Returns 0 on success, −1 on error.
///
/// # Safety
///
/// `stream` must be a valid pointer returned by an open/accept function.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_stream_finish(stream: *mut HewQuicStream) -> c_int {
    if stream.is_null() {
        return -1;
    }
    // SAFETY: `stream` is a valid pointer per caller contract.
    let s = unsafe { &*stream };
    let Ok(mut send) = s.send.lock() else {
        return -1;
    };
    match s.rt.block_on(async { send.finish() }) {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

/// Abruptly abort a QUIC stream with an application error code.
///
/// Unlike [`hew_quic_stream_finish`], pending data is discarded.
/// Returns 0 on success, −1 on error.
///
/// `error_code` is passed as a QUIC application-layer error code. QUIC error
/// codes are unsigned and limited to 62 bits (`VarInt::MAX = 2^62 − 1`). If
/// the absolute value of `error_code` exceeds `VarInt::MAX` it is silently
/// clamped to `VarInt::MAX`; the caller is responsible for passing a value in
/// the valid range `[0, 2^62 − 1]`.
///
/// # Safety
///
/// `stream` must be a valid pointer returned by an open/accept function.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_stream_stop(
    stream: *mut HewQuicStream,
    error_code: i64,
) -> c_int {
    if stream.is_null() {
        return -1;
    }
    // SAFETY: `stream` is a valid pointer per caller contract.
    let s = unsafe { &*stream };
    let Ok(mut send) = s.send.lock() else {
        return -1;
    };
    // Clamp out-of-range values to VarInt::MAX (see doc comment above).
    let code = quinn::VarInt::try_from(error_code.unsigned_abs()).unwrap_or(quinn::VarInt::MAX);
    match send.reset(code) {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

// ── Event methods ─────────────────────────────────────────────────────────────

/// Return the event kind of a [`HewQuicEvent`].
///
/// Event kinds:
/// * `0` — connected
/// * `1` — disconnected
/// * `2` — stream opened
/// * `3` — stream closed
/// * `4` — data ready
/// * `−1` — error
///
/// Returns −1 if `event` is null.
///
/// # Safety
///
/// `event` must be a valid pointer returned by an `on_event` function, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_event_kind(event: *const HewQuicEvent) -> c_int {
    if event.is_null() {
        return -1;
    }
    // SAFETY: `event` is a valid pointer per caller contract.
    unsafe { (*event).kind }
}

/// Free a [`HewQuicEvent`] previously returned by an `on_event` function.
///
/// Passing null is a no-op.
///
/// # Safety
///
/// `event` must be a pointer previously returned by an `on_event` function
/// and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_event_free(event: *mut HewQuicEvent) {
    if event.is_null() {
        return;
    }
    // SAFETY: `event` was allocated with Box::into_raw.
    drop(unsafe { Box::from_raw(event) });
}

// ── Unit tests ────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_client_returns_non_null() {
        let ep = hew_quic_new_client();
        assert!(!ep.is_null(), "expected non-null endpoint for client");
        // SAFETY: ep was just created.
        unsafe { hew_quic_endpoint_close(ep) };
    }

    #[test]
    fn new_client_null_safe_close() {
        // SAFETY: null is explicitly handled.
        unsafe { hew_quic_endpoint_close(std::ptr::null_mut()) };
    }

    #[test]
    fn new_server_returns_non_null() {
        let addr = c":0";
        // SAFETY: addr is a valid C string literal.
        let ep = unsafe { hew_quic_new_server(addr.as_ptr()) };
        assert!(!ep.is_null(), "expected non-null endpoint for server");
        // SAFETY: ep was just created.
        unsafe { hew_quic_endpoint_close(ep) };
    }

    #[test]
    fn new_server_null_addr() {
        // SAFETY: null is explicitly handled.
        let ep = unsafe { hew_quic_new_server(std::ptr::null()) };
        assert!(ep.is_null());
    }

    #[test]
    fn endpoint_connect_null_ep() {
        let addr = c"127.0.0.1:4433";
        let sn = c"localhost";
        // SAFETY: null ep is explicitly handled.
        let conn = unsafe {
            hew_quic_endpoint_connect(std::ptr::null_mut(), addr.as_ptr(), sn.as_ptr())
        };
        assert!(conn.is_null());
    }

    #[test]
    fn endpoint_accept_null_ep() {
        // SAFETY: null ep is explicitly handled.
        let conn = unsafe { hew_quic_endpoint_accept(std::ptr::null_mut()) };
        assert!(conn.is_null());
    }

    #[test]
    fn conn_disconnect_null() {
        // SAFETY: null is explicitly handled.
        let rc = unsafe { hew_quic_conn_disconnect(std::ptr::null_mut()) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn conn_open_stream_null() {
        // SAFETY: null is explicitly handled.
        let s = unsafe { hew_quic_conn_open_stream(std::ptr::null_mut()) };
        assert!(s.is_null());
    }

    #[test]
    fn conn_accept_stream_null() {
        // SAFETY: null is explicitly handled.
        let s = unsafe { hew_quic_conn_accept_stream(std::ptr::null_mut()) };
        assert!(s.is_null());
    }

    #[test]
    fn stream_send_null() {
        // SAFETY: null is explicitly handled.
        let rc = unsafe { hew_quic_stream_send(std::ptr::null_mut(), std::ptr::null(), 0) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn stream_recv_null() {
        let mut len: usize = 0;
        // SAFETY: null stream is explicitly handled.
        let ptr =
            unsafe { hew_quic_stream_recv(std::ptr::null_mut(), &raw mut len) };
        assert!(ptr.is_null());
    }

    #[test]
    fn stream_finish_null() {
        // SAFETY: null is explicitly handled.
        let rc = unsafe { hew_quic_stream_finish(std::ptr::null_mut()) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn stream_stop_null() {
        // SAFETY: null is explicitly handled.
        let rc = unsafe { hew_quic_stream_stop(std::ptr::null_mut(), 0) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn event_kind_null() {
        // SAFETY: null is explicitly handled.
        let k = unsafe { hew_quic_event_kind(std::ptr::null()) };
        assert_eq!(k, -1);
    }

    #[test]
    fn event_free_null_is_noop() {
        // SAFETY: null is explicitly handled.
        unsafe { hew_quic_event_free(std::ptr::null_mut()) };
    }

    #[test]
    fn event_struct_kind_field() {
        let ev = HewQuicEvent { kind: 2 };
        let ptr = &raw const ev as *const HewQuicEvent;
        // SAFETY: ptr points to a valid stack-allocated HewQuicEvent.
        let k = unsafe { hew_quic_event_kind(ptr) };
        assert_eq!(k, 2);
    }

    /// Verify a full server/client loopback over localhost using a pair of
    /// endpoints on consecutive ports.
    #[test]
    fn loopback_send_recv() {
        use std::thread;

        // Bind to port 0 so the OS assigns a free port, avoiding conflicts when
        // tests run in parallel within the workspace (`panic = "abort"` profile).
        let server_ep_ptr = {
            let addr = c":0";
            // SAFETY: addr is a valid C string literal.
            unsafe { hew_quic_new_server(addr.as_ptr()) }
        };
        assert!(!server_ep_ptr.is_null(), "server endpoint must not be null");

        // Discover the port the OS assigned so the client can connect to it.
        // SAFETY: server_ep_ptr is non-null as asserted above.
        let server_port = unsafe { &*server_ep_ptr }
            .endpoint
            .local_addr()
            .expect("server endpoint must have a local address")
            .port();

        // Cast to usize to cross the thread boundary; usize is Send.
        // Exclusive ownership is handed to the server thread here.
        let server_ep_addr = server_ep_ptr as usize;

        // Use a barrier to synchronise the two sides:
        //   barrier.wait() #1 — server has echoed, client can recv
        //   barrier.wait() #2 — client has recv'd, server can disconnect
        let barrier = Arc::new(std::sync::Barrier::new(2));
        let barrier_server = Arc::clone(&barrier);

        // Spawn server thread: accept one connection, accept one stream,
        // echo one message, finish.
        let server_thread = thread::spawn(move || {
            // SAFETY: server_ep_addr was a valid HewQuicEndpoint pointer
            // produced above; we are the sole owner after the cast transfer.
            let server_ep_ptr = server_ep_addr as *mut HewQuicEndpoint;

            // SAFETY: server_ep_ptr is valid and owned by this thread.
            let conn_ptr = unsafe { hew_quic_endpoint_accept(server_ep_ptr) };
            assert!(!conn_ptr.is_null(), "server must accept a connection");

            // SAFETY: conn_ptr is valid.
            let stream_ptr = unsafe { hew_quic_conn_accept_stream(conn_ptr) };
            assert!(!stream_ptr.is_null(), "server must accept a stream");

            let mut out_len: usize = 0;
            // SAFETY: stream_ptr and out_len are valid.
            let data_ptr =
                unsafe { hew_quic_stream_recv(stream_ptr, &raw mut out_len) };
            assert!(!data_ptr.is_null(), "server must receive data");
            assert!(out_len > 0, "received data must be non-empty");

            // Echo the same bytes back.
            // SAFETY: data_ptr is valid for out_len bytes.
            let rc = unsafe { hew_quic_stream_send(stream_ptr, data_ptr, out_len) };
            assert_eq!(rc, 0, "server echo send must succeed");

            // SAFETY: data_ptr was malloc-allocated.
            unsafe { libc::free(data_ptr.cast()) };

            // SAFETY: stream_ptr is valid.
            unsafe { hew_quic_stream_finish(stream_ptr) };

            // Wait for the client to receive the echo before disconnecting so
            // that `conn.close()` does not discard unread send-stream data.
            barrier_server.wait();
            // Wait for the client to signal it is done before we close.
            barrier_server.wait();

            // SAFETY: conn_ptr is valid.
            unsafe { hew_quic_conn_disconnect(conn_ptr) };

            // SAFETY: server_ep_ptr is valid.
            unsafe { hew_quic_endpoint_close(server_ep_ptr) };
        });

        // Give the server a moment to start accepting before the client connects.
        std::thread::sleep(Duration::from_millis(100));

        let client_ep_ptr = hew_quic_new_client();
        assert!(!client_ep_ptr.is_null(), "client endpoint must not be null");

        // Connect to the OS-assigned server port.
        let addr_str = format!("127.0.0.1:{server_port}");
        let addr_cstr = std::ffi::CString::new(addr_str).expect("valid address string");
        let sn = c"localhost";
        // SAFETY: addr_cstr and sn are valid NUL-terminated strings; client_ep_ptr is valid.
        let conn_ptr =
            unsafe { hew_quic_endpoint_connect(client_ep_ptr, addr_cstr.as_ptr(), sn.as_ptr()) };
        assert!(!conn_ptr.is_null(), "client must connect");

        // SAFETY: conn_ptr is valid.
        let stream_ptr = unsafe { hew_quic_conn_open_stream(conn_ptr) };
        assert!(!stream_ptr.is_null(), "client must open stream");

        let msg = b"hello quic";
        // SAFETY: msg is valid for msg.len() bytes; stream_ptr is valid.
        let rc = unsafe { hew_quic_stream_send(stream_ptr, msg.as_ptr(), msg.len()) };
        assert_eq!(rc, 0, "client send must succeed");

        // Signal that the client has sent; the server can now echo.
        // Then wait until the server has finished echoing.
        barrier.wait();

        let mut out_len: usize = 0;
        // SAFETY: stream_ptr and out_len are valid.
        let reply_ptr =
            unsafe { hew_quic_stream_recv(stream_ptr, &raw mut out_len) };
        assert!(!reply_ptr.is_null(), "client must receive echo");
        assert_eq!(out_len, msg.len());
        // SAFETY: reply_ptr is valid for out_len bytes.
        let reply_slice = unsafe { std::slice::from_raw_parts(reply_ptr, out_len) };
        assert_eq!(reply_slice, msg, "echoed data must match sent data");

        // SAFETY: reply_ptr was malloc-allocated.
        unsafe { libc::free(reply_ptr.cast()) };

        // SAFETY: all pointers are valid.
        unsafe { hew_quic_stream_finish(stream_ptr) };
        unsafe { hew_quic_conn_disconnect(conn_ptr) };
        unsafe { hew_quic_endpoint_close(client_ep_ptr) };

        // Signal the server that we are done; it may now disconnect.
        barrier.wait();

        server_thread.join().expect("server thread must not panic");
    }
}
