//! Hew `std::net::quic` — QUIC transport built on `quinn`.
//! Provides blocking helpers over async QUIC connections for Hew programs.

#![expect(
    clippy::missing_errors_doc,
    reason = "FFI surface returns null/integers instead of Result"
)]

use std::net::{IpAddr, Ipv4Addr, SocketAddr, ToSocketAddrs};
use std::os::raw::c_char;
use std::sync::Arc;

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use hew_cabi::vec::{hwvec_to_u8, u8_to_hwvec, HewVec};
use once_cell::sync::Lazy;
use quinn::{Endpoint, RecvStream, SendStream};
use rustls::client::danger::{HandshakeSignatureValid, ServerCertVerified};
use rustls::pki_types::{CertificateDer, PrivatePkcs8KeyDer, UnixTime};
use rustls::{DigitallySignedStruct, RootCertStore};
use tokio::runtime::Runtime;

#[cfg(test)]
extern crate hew_runtime;

/// Global Tokio runtime used to drive async Quinn operations.
static RUNTIME: Lazy<Runtime> = Lazy::new(|| Runtime::new().expect("create tokio runtime"));

/// Listener handle storing endpoint, incoming stream, and server certificate.
#[derive(Debug)]
pub struct HewQuicListener {
    endpoint: Endpoint,
    cert_der: Vec<u8>,
}

/// Established QUIC connection handle.
#[derive(Debug)]
pub struct HewQuicConnection {
    conn: quinn::Connection,
    #[allow(dead_code, reason = "keeps the client endpoint alive for connection IO")]
    endpoint: Option<Endpoint>,
}

/// Bidirectional QUIC stream handle.
#[derive(Debug)]
pub struct HewQuicStream {
    send: SendStream,
    recv: RecvStream,
}

/// Received chunk from a stream.
#[repr(C)]
#[derive(Debug)]
pub struct HewQuicChunk {
    data: *mut HewVec,
    fin: bool,
}

fn runtime() -> &'static Runtime {
    &RUNTIME
}

/// Parse a user-supplied address into a `SocketAddr`.
fn parse_addr(addr: &str) -> Option<SocketAddr> {
    if addr.is_empty() {
        return None;
    }
    // Allow shorthand ":4433" to bind on all interfaces.
    let normalized = if addr.starts_with(':') {
        format!("0.0.0.0{addr}")
    } else {
        addr.to_string()
    };
    normalized.to_socket_addrs().ok()?.next()
}

/// Choose names to embed in the self-signed certificate.
fn cert_names(addr: &str) -> Vec<String> {
    let host = if let Some(rest) = addr.strip_prefix('[') {
        rest.split(']').next().unwrap_or("").to_string()
    } else if addr.starts_with(':') {
        String::new()
    } else {
        addr.split(':').next().unwrap_or("").to_string()
    };
    let mut names = Vec::new();
    if !host.is_empty() {
        names.push(host);
    }
    names.push("localhost".into());
    names
}

/// Build a server config with a self-signed certificate.
fn build_server_config(names: &[String]) -> Option<(quinn::ServerConfig, Vec<u8>)> {
    let cert = rcgen::generate_simple_self_signed(if names.is_empty() {
        vec!["localhost".into()]
    } else {
        names.to_vec()
    })
    .ok()?;
    let cert_der = cert.serialize_der().ok()?;
    let key = PrivatePkcs8KeyDer::from(cert.serialize_private_key_der());
    let chain = vec![CertificateDer::from(cert_der.clone())];

    let mut server_config = quinn::ServerConfig::with_single_cert(chain, key.into()).ok()?;
    server_config.transport = Arc::new(quinn::TransportConfig::default());
    Some((server_config, cert_der))
}

/// Certificate verifier that accepts any server certificate (for opt-in insecure connects).
#[derive(Debug)]
struct AcceptAnyServer;

impl rustls::client::danger::ServerCertVerifier for AcceptAnyServer {
    fn verify_server_cert(
        &self,
        _end_entity: &rustls::pki_types::CertificateDer<'_>,
        _intermediates: &[rustls::pki_types::CertificateDer<'_>],
        _server_name: &rustls::pki_types::ServerName<'_>,
        _ocsp: &[u8],
        _now: UnixTime,
    ) -> Result<rustls::client::danger::ServerCertVerified, rustls::Error> {
        Ok(ServerCertVerified::assertion())
    }

    fn verify_tls13_signature(
        &self,
        _message: &[u8],
        _cert: &CertificateDer<'_>,
        _dss: &DigitallySignedStruct,
    ) -> Result<HandshakeSignatureValid, rustls::Error> {
        Ok(HandshakeSignatureValid::assertion())
    }

    fn verify_tls12_signature(
        &self,
        _message: &[u8],
        _cert: &CertificateDer<'_>,
        _dss: &DigitallySignedStruct,
    ) -> Result<HandshakeSignatureValid, rustls::Error> {
        Ok(HandshakeSignatureValid::assertion())
    }

    fn supported_verify_schemes(&self) -> Vec<rustls::SignatureScheme> {
        rustls::crypto::ring::default_provider()
            .signature_verification_algorithms
            .supported_schemes()
    }
}

/// Build a client config using the provided root certificate (if any).
fn build_client_config(
    root_cert: Option<&[u8]>,
    allow_insecure: bool,
    force_insecure: bool,
) -> Option<quinn::ClientConfig> {
    let mut roots = RootCertStore::empty();
    if let Some(der) = root_cert {
        let _ = roots.add(CertificateDer::from(der.to_vec()));
    } else {
        let native = rustls_native_certs::load_native_certs();
        for cert in native.certs {
            let _ = roots.add(cert);
        }
    }
    let use_insecure = force_insecure || (allow_insecure && roots.is_empty() && root_cert.is_none());
    let mut crypto = rustls::ClientConfig::builder_with_provider(
        rustls::crypto::ring::default_provider().into(),
    )
        .with_safe_default_protocol_versions()
        .ok()?
        .with_root_certificates(roots)
        .with_no_client_auth();
    if use_insecure {
        crypto
            .dangerous()
            .set_certificate_verifier(Arc::new(AcceptAnyServer));
    }
    let quic_crypto = quinn::crypto::rustls::QuicClientConfig::try_from(crypto).ok()?;
    let mut cfg = quinn::ClientConfig::new(Arc::new(quic_crypto));
    cfg.transport_config(Arc::new(quinn::TransportConfig::default()));
    Some(cfg)
}

/// Helper to read a C string, returning None on null/UTF-8 error.
fn read_cstr(ptr: *const c_char) -> Option<String> {
    if ptr.is_null() {
        return None;
    }
    // SAFETY: caller guarantees pointer validity when non-null.
    unsafe { cstr_to_str(ptr) }.map(str::to_string)
}

/// Start a QUIC listener bound to `addr`. Returns null on error.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_listen(addr: *const c_char) -> *mut HewQuicListener {
    let Some(addr_str) = read_cstr(addr) else {
        return std::ptr::null_mut();
    };
    let Some(bind_addr) = parse_addr(&addr_str) else {
        return std::ptr::null_mut();
    };
    let mut names = cert_names(&addr_str);
    let bind_ip = bind_addr.ip().to_string();
    if !names.contains(&bind_ip) {
        names.push(bind_ip);
    }
    for fallback in ["127.0.0.1".to_string(), "::1".to_string()] {
        if !names.contains(&fallback) {
            names.push(fallback);
        }
    }
    let Some((server_config, cert_der)) = build_server_config(&names) else {
        return std::ptr::null_mut();
    };
    let _guard = runtime().enter();
    match Endpoint::server(server_config, bind_addr) {
        Ok(endpoint) => Box::into_raw(Box::new(HewQuicListener { endpoint, cert_der })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Return the listener's bound address as a heap-allocated C string.
#[no_mangle]
pub extern "C" fn hew_quic_listener_addr(listener: *mut HewQuicListener) -> *mut c_char {
    if listener.is_null() {
        return std::ptr::null_mut();
    }
    let addr = unsafe { &*listener }
        .endpoint
        .local_addr()
        .map(|a| {
            let ip = if a.ip().is_unspecified() {
                IpAddr::V4(Ipv4Addr::LOCALHOST)
            } else {
                a.ip()
            };
            SocketAddr::new(ip, a.port()).to_string()
        })
        .unwrap_or_default();
    str_to_malloc(&addr)
}

/// Return the listener certificate as a bytes HewVec.
#[no_mangle]
pub extern "C" fn hew_quic_listener_cert(listener: *mut HewQuicListener) -> *mut HewVec {
    if listener.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: listener is valid.
    let cert = unsafe { &*listener }.cert_der.clone();
    // SAFETY: u8_to_hwvec allocates and copies.
    unsafe { u8_to_hwvec(&cert) }
}

/// Close and free a listener.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_listener_close(listener: *mut HewQuicListener) {
    if listener.is_null() {
        return;
    }
    // SAFETY: listener was allocated via Box::into_raw in hew_quic_listen.
    drop(unsafe { Box::from_raw(listener) });
}

/// Accept the next incoming connection (blocking).
#[no_mangle]
pub unsafe extern "C" fn hew_quic_accept(listener: *mut HewQuicListener) -> *mut HewQuicConnection {
    if listener.is_null() {
        return std::ptr::null_mut();
    }
    let _guard = runtime().enter();
    let fut = unsafe { &*listener }.endpoint.accept();
    let Some(incoming) = runtime().block_on(fut) else {
        return std::ptr::null_mut();
    };
    let connecting = match incoming.accept() {
        Ok(c) => c,
        Err(_) => return std::ptr::null_mut(),
        };
    match runtime().block_on(connecting) {
        Ok(conn) => Box::into_raw(Box::new(HewQuicConnection {
            conn,
            endpoint: None,
        })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Connect to a QUIC server using system roots (falling back to insecure).
#[no_mangle]
pub unsafe extern "C" fn hew_quic_connect(addr: *const c_char) -> *mut HewQuicConnection {
    hew_quic_connect_inner(addr, None, true, false)
}

/// Connect with an explicit DER-encoded root certificate.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_connect_with_ca(
    addr: *const c_char,
    ca_der: *mut HewVec,
) -> *mut HewQuicConnection {
    let roots = if ca_der.is_null() {
        Vec::new()
    } else {
        // SAFETY: caller provides a valid HewVec.
        unsafe { hwvec_to_u8(ca_der) }
    };
    hew_quic_connect_inner(addr, Some(roots), false, false)
}

/// Connect without certificate verification (testing only).
#[no_mangle]
pub unsafe extern "C" fn hew_quic_connect_insecure(addr: *const c_char) -> *mut HewQuicConnection {
    hew_quic_connect_inner(addr, None, true, true)
}

fn hew_quic_connect_inner(
    addr: *const c_char,
    ca_der: Option<Vec<u8>>,
    allow_insecure: bool,
    force_insecure: bool,
) -> *mut HewQuicConnection {
    let Some(addr_str) = read_cstr(addr) else {
        return std::ptr::null_mut();
    };
    let Some(server_addr) = parse_addr(&addr_str) else {
        return std::ptr::null_mut();
    };
    let Some(client_cfg) = build_client_config(ca_der.as_deref(), allow_insecure, force_insecure) else {
        return std::ptr::null_mut();
    };
    let names = cert_names(&addr_str);
    let server_name = names
        .iter()
        .find(|n| n.as_str() == "localhost")
        .or_else(|| names.iter().find(|n| !n.is_empty()))
        .cloned()
        .unwrap_or_else(|| server_addr.ip().to_string());
    let _guard = runtime().enter();
    let endpoint = match Endpoint::client("0.0.0.0:0".parse().unwrap()) {
        Ok(ep) => ep,
        Err(_) => return std::ptr::null_mut(),
    };
    let connecting = match endpoint.connect_with(client_cfg, server_addr, &server_name) {
        Ok(c) => c,
        Err(_) => return std::ptr::null_mut(),
    };
    match runtime().block_on(connecting) {
        Ok(conn) => Box::into_raw(Box::new(HewQuicConnection {
            conn,
            endpoint: Some(endpoint),
        })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Return the peer address string for a connection.
#[no_mangle]
pub extern "C" fn hew_quic_peer_addr(conn: *mut HewQuicConnection) -> *mut c_char {
    if conn.is_null() {
        return std::ptr::null_mut();
    }
    let addr = unsafe { &*conn }.conn.remote_address().to_string();
    str_to_malloc(&addr)
}

/// Gracefully close a connection.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_close(conn: *mut HewQuicConnection) {
    // SAFETY: caller upholds pointer validity.
    unsafe { hew_quic_close_err(conn, 0) };
}

/// Close a connection with an application error code.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_close_err(conn: *mut HewQuicConnection, code: i64) {
    if conn.is_null() {
        return;
    }
    // SAFETY: caller guarantees pointer validity.
    let owned = unsafe { Box::from_raw(conn) };
    let code_vi = quinn::VarInt::from_u64(code as u64).unwrap_or_else(|_| quinn::VarInt::from_u32(0));
    owned.conn.close(code_vi, b"closing");
}

/// Open a new bidirectional stream.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_open_bi(conn: *mut HewQuicConnection) -> *mut HewQuicStream {
    if conn.is_null() {
        return std::ptr::null_mut();
    }
    let _guard = runtime().enter();
    let fut = unsafe { &*conn }.conn.open_bi();
    match runtime().block_on(fut) {
        Ok((send, recv)) => Box::into_raw(Box::new(HewQuicStream { send, recv })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Accept the next inbound bidirectional stream.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_accept_bi(conn: *mut HewQuicConnection) -> *mut HewQuicStream {
    if conn.is_null() {
        return std::ptr::null_mut();
    }
    let _guard = runtime().enter();
    let fut = unsafe { &*conn }.conn.accept_bi();
    match runtime().block_on(fut) {
        Ok((send, recv)) => Box::into_raw(Box::new(HewQuicStream { send, recv })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Send a bytes HewVec over the stream. Returns 0 on success, -1 on error.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_send(stream: *mut HewQuicStream, data: *mut HewVec) -> i32 {
    if stream.is_null() || data.is_null() {
        return -1;
    }
    let bytes = unsafe { hwvec_to_u8(data) };
    let fut = unsafe { &mut *stream }.send.write_all(&bytes);
    if runtime().block_on(fut).is_err() {
        return -1;
    }
    0
}

/// Receive up to `max_len` bytes from the stream.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_recv(
    stream: *mut HewQuicStream,
    max_len: i64,
) -> *mut HewQuicChunk {
    if stream.is_null() {
        return std::ptr::null_mut();
    }
    let cap = if max_len <= 0 { 65536 } else { max_len as usize };
    let fut = unsafe { &mut *stream }.recv.read_chunk(cap, true);
    match runtime().block_on(fut) {
        Ok(Some(chunk)) => {
            let data = unsafe { u8_to_hwvec(&chunk.bytes) };
            Box::into_raw(Box::new(HewQuicChunk { data, fin: false }))
        }
        Ok(None) => Box::into_raw(Box::new(HewQuicChunk {
            data: unsafe { u8_to_hwvec(&[]) },
            fin: true,
        })),
        Err(_) => Box::into_raw(Box::new(HewQuicChunk {
            data: unsafe { u8_to_hwvec(&[]) },
            fin: true,
        })),
    }
}

/// Finish the send side of the stream.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_finish(stream: *mut HewQuicStream) -> i32 {
    if stream.is_null() {
        return -1;
    }
    if unsafe { &mut *stream }.send.finish().is_ok() {
        0
    } else {
        -1
    }
}

/// Access the data buffer for a received chunk.
#[no_mangle]
pub extern "C" fn hew_quic_chunk_data(chunk: *mut HewQuicChunk) -> *mut HewVec {
    if chunk.is_null() {
        return std::ptr::null_mut();
    }
    unsafe { (*chunk).data }
}

/// Whether the stream is finished after this chunk.
#[no_mangle]
pub extern "C" fn hew_quic_chunk_fin(chunk: *mut HewQuicChunk) -> bool {
    if chunk.is_null() {
        return true;
    }
    unsafe { (*chunk).fin }
}

/// Free a received chunk and its data.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_chunk_free(chunk: *mut HewQuicChunk) {
    if chunk.is_null() {
        return;
    }
    let owned = unsafe { Box::from_raw(chunk) };
    if !owned.data.is_null() {
        unsafe { hew_cabi::vec::hew_vec_free(owned.data) };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{CStr, CString};
    use std::thread;

    #[test]
    fn listener_rejects_empty_addr() {
        let ptr = std::ptr::null();
        // SAFETY: null pointer handled explicitly.
        let listener = unsafe { hew_quic_listen(ptr) };
        assert!(listener.is_null());
    }

    #[test]
    fn round_trip_stream() {
        let addr = CString::new(":0").unwrap();
        // SAFETY: addr is a valid C string.
        let listener = unsafe { hew_quic_listen(addr.as_ptr()) };
        assert!(!listener.is_null());

        let addr_ptr = hew_quic_listener_addr(listener);
        assert!(!addr_ptr.is_null());
        let addr_str = unsafe { CStr::from_ptr(addr_ptr) }
            .to_str()
            .unwrap()
            .to_string();
        unsafe { libc::free(addr_ptr.cast()) };

        let payload = b"ping-quic";
        let reply = b"pong-quic";

        let client_handle = thread::spawn({
            let addr_str = addr_str.clone();
            move || {
                let addr_c = CString::new(addr_str).unwrap();
                let conn = unsafe { hew_quic_connect_insecure(addr_c.as_ptr()) };
                assert!(!conn.is_null());

                let chan = unsafe { hew_quic_open_bi(conn) };
                assert!(!chan.is_null());
                let send_vec = unsafe { u8_to_hwvec(payload) };
                assert_eq!(0, unsafe { hew_quic_send(chan, send_vec) });
                unsafe { hew_cabi::vec::hew_vec_free(send_vec) };
                assert_eq!(0, unsafe { hew_quic_finish(chan) });

                let chunk = unsafe { hew_quic_recv(chan, 1024) };
                assert!(!chunk.is_null());
                let data = unsafe { hwvec_to_u8(hew_quic_chunk_data(chunk)) };
                unsafe { hew_quic_chunk_free(chunk) };
                assert_eq!(reply, data.as_slice());

                unsafe { hew_quic_close(conn) };
                data
            }
        });

        let server_conn = unsafe { hew_quic_accept(listener) };
        assert!(!server_conn.is_null());
        let server_chan = unsafe { hew_quic_accept_bi(server_conn) };
        assert!(!server_chan.is_null());

        let chunk = unsafe { hew_quic_recv(server_chan, 1024) };
        assert!(!chunk.is_null());
        let data = unsafe { hwvec_to_u8(hew_quic_chunk_data(chunk)) };
        unsafe { hew_quic_chunk_free(chunk) };
        assert_eq!(payload, data.as_slice());

        let reply_vec = unsafe { u8_to_hwvec(reply) };
        assert_eq!(0, unsafe { hew_quic_send(server_chan, reply_vec) });
        unsafe { hew_cabi::vec::hew_vec_free(reply_vec) };
        assert_eq!(0, unsafe { hew_quic_finish(server_chan) });

        let echoed = client_handle.join().unwrap();
        assert_eq!(reply, echoed.as_slice());

        unsafe { hew_quic_close(server_conn) };
        unsafe { hew_quic_listener_close(listener) };
    }
}
