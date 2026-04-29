//! Hew `std::net::quic` — QUIC transport for internode messaging.
//!
//! Exposes a blocking C ABI over Quinn (async QUIC) by embedding a dedicated
//! Tokio runtime inside each `HewQuicEndpoint`. All `extern "C"` functions are
//! designed to be called from compiled Hew programs via FFI.
//!
//! Use `hew_quic_new_client_with_ca` and `hew_quic_new_server_with_tls` for
//! deployed systems. `hew_quic_new_client` and `hew_quic_new_server` are
//! development helpers for localhost-style testing.

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code.
#[cfg(test)]
extern crate hew_runtime;

use std::collections::VecDeque;
use std::io;
use std::net::{SocketAddr, ToSocketAddrs};
use std::os::raw::{c_char, c_int};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use hew_cabi::vec::HewVec;
use quinn::{ClientConfig, Connection, Endpoint, RecvStream, SendStream, ServerConfig};
use rustls::pki_types::pem::PemObject;
use rustls::pki_types::{CertificateDer, PrivateKeyDer, PrivatePkcs8KeyDer};
use rustls::RootCertStore;
use tokio::runtime::Runtime;

type BoxError = Box<dyn std::error::Error + Send + Sync + 'static>;

/// Maximum bytes read in a single [`hew_quic_stream_recv`] call.
const RECV_BUFFER_SIZE: usize = 65_536;

const EVENT_CONNECTED: i32 = 0;
const EVENT_DISCONNECTED: i32 = 1;
const EVENT_STREAM_OPENED: i32 = 2;
const EVENT_STREAM_CLOSED: i32 = 3;
const EVENT_ERROR: i32 = -1;

std::thread_local! {
    static LAST_CONSTRUCTOR_ERROR: std::cell::RefCell<Option<String>> =
        const { std::cell::RefCell::new(None) };
}

fn set_constructor_last_error(msg: impl Into<String>) {
    LAST_CONSTRUCTOR_ERROR.with(|error| *error.borrow_mut() = Some(msg.into()));
}

fn clear_constructor_last_error() {
    LAST_CONSTRUCTOR_ERROR.with(|error| *error.borrow_mut() = None);
}

fn get_constructor_last_error() -> String {
    LAST_CONSTRUCTOR_ERROR.with(|error| error.borrow().clone().unwrap_or_default())
}

#[derive(Debug, Default)]
struct EventQueue {
    queue: Mutex<VecDeque<i32>>,
    ready: Condvar,
}

impl EventQueue {
    fn push(&self, kind: i32) -> bool {
        let mut queue = self
            .queue
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        queue.push_back(kind);
        self.ready.notify_one();
        true
    }

    fn pop_blocking(&self) -> Option<i32> {
        let mut queue = self
            .queue
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        loop {
            if let Some(kind) = queue.pop_front() {
                return Some(kind);
            }
            queue = self
                .ready
                .wait(queue)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
        }
    }
}

#[derive(Debug, Default)]
struct EndpointObservationState {
    local_addr: String,
    accepted_connections: i64,
    last_error: String,
}

#[derive(Debug, Default)]
struct ConnectionObservationState {
    local_addr: String,
    peer_addr: String,
    opened_streams: i64,
    accepted_streams: i64,
    last_error: String,
}

#[derive(Debug, Default)]
struct StreamObservationState {
    bytes_sent: i64,
    bytes_received: i64,
    send_closed: bool,
    recv_closed: bool,
    last_error: String,
}

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

fn ring_provider() -> Arc<rustls::crypto::CryptoProvider> {
    Arc::new(rustls::crypto::ring::default_provider())
}

/// Build a development-only [`ClientConfig`] that skips certificate verification.
fn insecure_client_config() -> Result<ClientConfig, BoxError> {
    let crypto = rustls::ClientConfig::builder_with_provider(ring_provider())
        .with_protocol_versions(rustls::DEFAULT_VERSIONS)?
        .dangerous()
        .with_custom_certificate_verifier(Arc::new(NoVerifier))
        .with_no_client_auth();
    Ok(ClientConfig::new(Arc::new(
        quinn::crypto::rustls::QuicClientConfig::try_from(crypto)?,
    )))
}

fn certs_from_pem(pem: &str) -> Result<Vec<CertificateDer<'static>>, BoxError> {
    let certs = CertificateDer::pem_slice_iter(pem.as_bytes()).collect::<Result<Vec<_>, _>>()?;
    if certs.is_empty() {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "missing certificate PEM").into());
    }
    Ok(certs)
}

fn client_config_with_ca(ca_pem: &str) -> Result<ClientConfig, BoxError> {
    let mut roots = RootCertStore::empty();
    for cert in certs_from_pem(ca_pem)? {
        roots.add(cert)?;
    }
    let crypto = rustls::ClientConfig::builder_with_provider(ring_provider())
        .with_protocol_versions(rustls::DEFAULT_VERSIONS)?
        .with_root_certificates(roots)
        .with_no_client_auth();
    Ok(ClientConfig::new(Arc::new(
        quinn::crypto::rustls::QuicClientConfig::try_from(crypto)?,
    )))
}

fn server_config_from_pem(cert_pem: &str, key_pem: &str) -> Result<ServerConfig, BoxError> {
    let certs = certs_from_pem(cert_pem)?;
    let key = PrivateKeyDer::from_pem_slice(key_pem.as_bytes())?;
    let crypto = rustls::ServerConfig::builder_with_provider(ring_provider())
        .with_protocol_versions(rustls::DEFAULT_VERSIONS)?
        .with_no_client_auth()
        .with_single_cert(certs, key)?;
    Ok(ServerConfig::with_crypto(Arc::new(
        quinn::crypto::rustls::QuicServerConfig::try_from(crypto)?,
    )))
}

/// Generate a self-signed certificate for development server use.
fn self_signed_server_config() -> Result<ServerConfig, BoxError> {
    let cert = rcgen::generate_simple_self_signed(vec!["localhost".into()])?;
    let cert_der = CertificateDer::from(cert.cert);
    let key_der = PrivateKeyDer::Pkcs8(PrivatePkcs8KeyDer::from(cert.signing_key.serialize_der()));

    let crypto = rustls::ServerConfig::builder_with_provider(ring_provider())
        .with_protocol_versions(rustls::DEFAULT_VERSIONS)?
        .with_no_client_auth()
        .with_single_cert(vec![cert_der], key_der)?;

    Ok(ServerConfig::with_crypto(Arc::new(
        quinn::crypto::rustls::QuicServerConfig::try_from(crypto)?,
    )))
}

fn split_host_port(addr: &str) -> Result<(&str, &str), &'static str> {
    if let Some(rest) = addr.strip_prefix('[') {
        let Some((host, port)) = rest.split_once("]:") else {
            return Err("missing closing `]` or port separator");
        };
        if host.is_empty() {
            return Err("missing host");
        }
        if port.is_empty() {
            return Err("missing port");
        }
        return Ok((host, port));
    }

    let Some((host, port)) = addr.rsplit_once(':') else {
        return Err("missing port separator");
    };
    if host.is_empty() {
        return Err("missing host");
    }
    if host.contains(':') {
        return Err("unexpected `:` in host; IPv6 addresses must use `[addr]:port`");
    }
    if port.is_empty() {
        return Err("missing port");
    }
    Ok((host, port))
}

fn set_constructor_error_and_return<T>(msg: String) -> Result<T, String> {
    set_constructor_last_error(msg.clone());
    Err(msg)
}

fn resolve_addr(addr: &str, default_host: &str, context: &str) -> Result<SocketAddr, String> {
    let owned;
    let resolved_addr = if addr.starts_with(':') {
        owned = format!("{default_host}{addr}");
        owned.as_str()
    } else {
        addr
    };

    let (_, port) = split_host_port(resolved_addr).map_err(|detail| {
        format!("could not resolve {context} address `{addr}`: parse failure: {detail}")
    })?;
    port.parse::<u16>().map_err(|err| {
        format!(
            "could not resolve {context} address `{addr}`: parse failure: invalid port `{port}`: {err}"
        )
    })?;

    let mut addrs = resolved_addr.to_socket_addrs().map_err(|err| {
        format!("could not resolve {context} address `{addr}`: resolution failure: {err}")
    })?;
    addrs.next().ok_or_else(|| {
        format!("could not resolve {context} address `{addr}`: resolution returned no results")
    })
}

fn resolve_bind_addr(addr: &str) -> Result<SocketAddr, String> {
    resolve_addr(addr, "0.0.0.0", "bind").or_else(set_constructor_error_and_return)
}

fn resolve_connect_addr(addr: &str) -> Result<SocketAddr, String> {
    resolve_addr(addr, "127.0.0.1", "connect").or_else(set_constructor_error_and_return)
}

fn build_runtime() -> Result<Arc<Runtime>, String> {
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .map(Arc::new)
        .map_err(|err| format!("failed to build QUIC runtime: {err}"))
}

fn build_client_endpoint(client_config: ClientConfig) -> Result<*mut HewQuicEndpoint, String> {
    let rt = build_runtime()?;
    // "0.0.0.0:0" is a valid address literal — parse cannot fail.
    let bind: SocketAddr = "0.0.0.0:0".parse().expect("valid address literal");
    let endpoint = rt.block_on(async {
        match Endpoint::client(bind) {
            Ok(mut ep) => {
                ep.set_default_client_config(client_config);
                Ok(ep)
            }
            Err(err) => Err(format!("failed to bind client socket: {err}")),
        }
    });
    endpoint.map(|endpoint| {
        Box::into_raw(Box::new(HewQuicEndpoint {
            rt,
            observation: Mutex::new(EndpointObservationState {
                local_addr: endpoint_local_addr_string(&endpoint),
                ..EndpointObservationState::default()
            }),
            endpoint,
            events: Arc::new(EventQueue::default()),
        }))
    })
}

fn build_server_endpoint(
    bind_addr: SocketAddr,
    server_config: ServerConfig,
) -> Result<*mut HewQuicEndpoint, String> {
    let rt = build_runtime()?;
    let endpoint = rt
        .block_on(async { Endpoint::server(server_config, bind_addr) })
        .map_err(|err| format!("failed to bind server socket: {err}"))?;
    Ok(Box::into_raw(Box::new(HewQuicEndpoint {
        rt,
        observation: Mutex::new(EndpointObservationState {
            local_addr: endpoint_local_addr_string(&endpoint),
            ..EndpointObservationState::default()
        }),
        endpoint,
        events: Arc::new(EventQueue::default()),
    })))
}

fn endpoint_local_addr_string(endpoint: &Endpoint) -> String {
    endpoint
        .local_addr()
        .map_or_else(|_| String::new(), |addr| addr.to_string())
}

fn empty_c_string() -> *mut c_char {
    str_to_malloc("")
}

fn to_i64_count(value: usize) -> i64 {
    i64::try_from(value).unwrap_or(i64::MAX)
}

fn update_endpoint<F>(endpoint: &HewQuicEndpoint, f: F)
where
    F: FnOnce(&mut EndpointObservationState),
{
    if let Ok(mut observation) = endpoint.observation.lock() {
        f(&mut observation);
    }
}

fn update_connection<F>(conn: &HewQuicConn, f: F)
where
    F: FnOnce(&mut ConnectionObservationState),
{
    if let Ok(mut observation) = conn.observation.lock() {
        f(&mut observation);
    }
}

fn update_stream<F>(stream: &HewQuicStream, f: F)
where
    F: FnOnce(&mut StreamObservationState),
{
    if let Ok(mut observation) = stream.observation.lock() {
        f(&mut observation);
    }
}

fn connection_last_error(conn: &HewQuicConn) -> String {
    let from_state = conn
        .observation
        .lock()
        .ok()
        .map_or_else(String::new, |state| state.last_error.clone());
    if !from_state.is_empty() {
        return from_state;
    }
    conn.conn
        .close_reason()
        .map_or_else(String::new, |reason| reason.to_string())
}

fn empty_hwvec() -> *mut HewVec {
    // SAFETY: hew_vec_new allocates a valid empty HewVec.
    unsafe { hew_cabi::vec::hew_vec_new() }
}

fn event_ptr(kind: i32) -> *mut HewQuicEvent {
    Box::into_raw(Box::new(HewQuicEvent { kind }))
}

fn new_stream_handle(
    rt: &Arc<Runtime>,
    send: SendStream,
    recv: RecvStream,
    events: &Arc<EventQueue>,
) -> *mut HewQuicStream {
    Box::into_raw(Box::new(HewQuicStream {
        rt: Arc::clone(rt),
        send: Mutex::new(send),
        recv: Mutex::new(recv),
        events: Arc::clone(events),
        closed_notified: AtomicBool::new(false),
        observation: Mutex::new(StreamObservationState::default()),
    }))
}

fn new_conn_handle(rt: &Arc<Runtime>, conn: Connection, local_addr: String) -> *mut HewQuicConn {
    let events = Arc::new(EventQueue::default());
    let peer_addr = conn.remote_address().to_string();
    let closed_conn = conn.clone();
    let closed_events = Arc::clone(&events);
    std::mem::drop(rt.spawn(async move {
        let _ = closed_conn.closed().await;
        let _ = closed_events.push(EVENT_DISCONNECTED);
    }));
    Box::into_raw(Box::new(HewQuicConn {
        rt: Arc::clone(rt),
        conn,
        events,
        observation: Mutex::new(ConnectionObservationState {
            local_addr,
            peer_addr,
            ..ConnectionObservationState::default()
        }),
    }))
}

// ── Opaque handle types ───────────────────────────────────────────────────────

#[derive(Debug)]
pub struct HewQuicEndpoint {
    rt: Arc<Runtime>,
    endpoint: Endpoint,
    events: Arc<EventQueue>,
    observation: Mutex<EndpointObservationState>,
}

#[derive(Debug)]
pub struct HewQuicConn {
    rt: Arc<Runtime>,
    conn: Connection,
    events: Arc<EventQueue>,
    observation: Mutex<ConnectionObservationState>,
}

#[derive(Debug)]
pub struct HewQuicStream {
    rt: Arc<Runtime>,
    send: Mutex<SendStream>,
    recv: Mutex<RecvStream>,
    events: Arc<EventQueue>,
    closed_notified: AtomicBool,
    observation: Mutex<StreamObservationState>,
}

impl HewQuicStream {
    fn notify_closed(&self) {
        if !self.closed_notified.swap(true, Ordering::Relaxed) {
            let _ = self.events.push(EVENT_STREAM_CLOSED);
        }
    }
}

/// An observed QUIC event.
#[repr(C)]
#[derive(Debug)]
pub struct HewQuicEvent {
    /// 0 = connected, 1 = disconnected, 2 = stream opened,
    /// 3 = stream closed, -1 = error.
    pub kind: i32,
}

// ── Endpoint constructors ─────────────────────────────────────────────────────

#[no_mangle]
pub extern "C" fn hew_quic_new_client() -> *mut HewQuicEndpoint {
    let client_config = match insecure_client_config() {
        Ok(client_config) => client_config,
        Err(err) => {
            set_constructor_last_error(format!("failed to configure insecure client: {err}"));
            return std::ptr::null_mut();
        }
    };
    match build_client_endpoint(client_config) {
        Ok(endpoint) => {
            clear_constructor_last_error();
            endpoint
        }
        Err(err) => {
            set_constructor_last_error(err);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
/// Create a client endpoint that trusts the provided CA bundle.
///
/// # Safety
///
/// If `ca_pem` is non-null, it must point to a valid NUL-terminated UTF-8
/// string for the duration of this call.
pub unsafe extern "C" fn hew_quic_new_client_with_ca(
    ca_pem: *const c_char,
) -> *mut HewQuicEndpoint {
    // SAFETY: if non-null, `ca_pem` points to a caller-owned C string that
    // remains valid for the duration of this call.
    let Some(ca_pem) = (unsafe { cstr_to_str(ca_pem) }) else {
        set_constructor_last_error("invalid CA bundle");
        return std::ptr::null_mut();
    };
    let client_config = match client_config_with_ca(ca_pem) {
        Ok(client_config) => client_config,
        Err(err) => {
            set_constructor_last_error(format!("failed to configure client CA bundle: {err}"));
            return std::ptr::null_mut();
        }
    };
    match build_client_endpoint(client_config) {
        Ok(endpoint) => {
            clear_constructor_last_error();
            endpoint
        }
        Err(err) => {
            set_constructor_last_error(err);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
/// Create a development server endpoint with a generated self-signed
/// certificate.
///
/// # Safety
///
/// If `addr` is non-null, it must point to a valid NUL-terminated UTF-8 string
/// for the duration of this call.
pub unsafe extern "C" fn hew_quic_new_server(addr: *const c_char) -> *mut HewQuicEndpoint {
    // SAFETY: if non-null, `addr` points to a caller-owned C string that
    // remains valid for the duration of this call.
    let Some(addr_str) = (unsafe { cstr_to_str(addr) }) else {
        set_constructor_last_error("invalid bind address");
        return std::ptr::null_mut();
    };
    let bind_addr = match resolve_bind_addr(addr_str) {
        Ok(bind_addr) => bind_addr,
        Err(err) => {
            set_constructor_last_error(err);
            return std::ptr::null_mut();
        }
    };
    let server_config = match self_signed_server_config() {
        Ok(server_config) => server_config,
        Err(err) => {
            set_constructor_last_error(format!("failed to configure self-signed TLS: {err}"));
            return std::ptr::null_mut();
        }
    };
    match build_server_endpoint(bind_addr, server_config) {
        Ok(endpoint) => {
            clear_constructor_last_error();
            endpoint
        }
        Err(err) => {
            set_constructor_last_error(err);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
/// Create a server endpoint using explicit PEM-encoded certificate material.
///
/// # Safety
///
/// If non-null, `addr`, `cert_pem`, and `key_pem` must each point to valid
/// NUL-terminated UTF-8 strings for the duration of this call.
pub unsafe extern "C" fn hew_quic_new_server_with_tls(
    addr: *const c_char,
    cert_pem: *const c_char,
    key_pem: *const c_char,
) -> *mut HewQuicEndpoint {
    // SAFETY: if non-null, `addr` points to a caller-owned C string that
    // remains valid for the duration of this call.
    let Some(addr_str) = (unsafe { cstr_to_str(addr) }) else {
        set_constructor_last_error("invalid bind address");
        return std::ptr::null_mut();
    };
    // SAFETY: if non-null, `cert_pem` points to a caller-owned C string that
    // remains valid for the duration of this call.
    let Some(cert_pem) = (unsafe { cstr_to_str(cert_pem) }) else {
        set_constructor_last_error("invalid TLS certificate PEM");
        return std::ptr::null_mut();
    };
    // SAFETY: if non-null, `key_pem` points to a caller-owned C string that
    // remains valid for the duration of this call.
    let Some(key_pem) = (unsafe { cstr_to_str(key_pem) }) else {
        set_constructor_last_error("invalid TLS private key PEM");
        return std::ptr::null_mut();
    };
    let bind_addr = match resolve_bind_addr(addr_str) {
        Ok(bind_addr) => bind_addr,
        Err(err) => {
            set_constructor_last_error(err);
            return std::ptr::null_mut();
        }
    };
    let server_config = match server_config_from_pem(cert_pem, key_pem) {
        Ok(server_config) => server_config,
        Err(err) => {
            set_constructor_last_error(format!("failed to configure server TLS: {err}"));
            return std::ptr::null_mut();
        }
    };
    match build_server_endpoint(bind_addr, server_config) {
        Ok(endpoint) => {
            clear_constructor_last_error();
            endpoint
        }
        Err(err) => {
            set_constructor_last_error(err);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
/// Return this actor's last constructor/setup error.
pub extern "C" fn hew_quic_last_error() -> *mut c_char {
    str_to_malloc(&get_constructor_last_error())
}

// ── Endpoint methods ──────────────────────────────────────────────────────────

#[no_mangle]
/// Connect a client endpoint to the remote QUIC address.
///
/// # Safety
///
/// `ep` must be null or a live endpoint pointer returned by this module. If
/// non-null, `addr` and `server_name` must point to valid NUL-terminated UTF-8
/// strings for the duration of this call.
pub unsafe extern "C" fn hew_quic_endpoint_connect(
    ep: *mut HewQuicEndpoint,
    addr: *const c_char,
    server_name: *const c_char,
) -> *mut HewQuicConn {
    if ep.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `ep` is non-null and must come from this module per caller
    // contract.
    let endpoint = unsafe { &*ep };
    // SAFETY: if non-null, `addr` points to a caller-owned C string that
    // remains valid for the duration of this call.
    let Some(addr_str) = (unsafe { cstr_to_str(addr) }) else {
        update_endpoint(endpoint, |state| {
            state.last_error = String::from("invalid remote address");
        });
        let _ = endpoint.events.push(EVENT_ERROR);
        return std::ptr::null_mut();
    };
    // SAFETY: if non-null, `server_name` points to a caller-owned C string
    // that remains valid for the duration of this call.
    let Some(server_name) = (unsafe { cstr_to_str(server_name) }) else {
        update_endpoint(endpoint, |state| {
            state.last_error = String::from("invalid server name");
        });
        let _ = endpoint.events.push(EVENT_ERROR);
        return std::ptr::null_mut();
    };
    let remote = match resolve_connect_addr(addr_str) {
        Ok(remote) => remote,
        Err(err) => {
            set_constructor_last_error(err.clone());
            update_endpoint(endpoint, |state| {
                state.last_error = err;
            });
            let _ = endpoint.events.push(EVENT_ERROR);
            return std::ptr::null_mut();
        }
    };

    let conn_result = endpoint.rt.block_on(async {
        let connecting = endpoint
            .endpoint
            .connect(remote, server_name)
            .map_err(|err| err.to_string())?;
        connecting.await.map_err(|err| err.to_string())
    });
    let conn = match conn_result {
        Ok(conn) => conn,
        Err(err) => {
            update_endpoint(endpoint, |state| state.last_error = err);
            let _ = endpoint.events.push(EVENT_ERROR);
            return std::ptr::null_mut();
        }
    };

    update_endpoint(endpoint, |state| state.last_error.clear());
    let _ = endpoint.events.push(EVENT_CONNECTED);
    new_conn_handle(
        &endpoint.rt,
        conn,
        endpoint_local_addr_string(&endpoint.endpoint),
    )
}

#[no_mangle]
/// Accept the next incoming QUIC connection on a server endpoint.
///
/// # Safety
///
/// `ep` must be null or a live endpoint pointer returned by this module.
pub unsafe extern "C" fn hew_quic_endpoint_accept(ep: *mut HewQuicEndpoint) -> *mut HewQuicConn {
    if ep.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `ep` is non-null and must come from this module per caller
    // contract.
    let endpoint = unsafe { &*ep };

    let conn_result = endpoint.rt.block_on(async {
        let Some(incoming) = endpoint.endpoint.accept().await else {
            return Err("endpoint closed".to_owned());
        };
        incoming.await.map_err(|err| err.to_string())
    });
    let conn = match conn_result {
        Ok(conn) => conn,
        Err(err) => {
            update_endpoint(endpoint, |state| state.last_error = err);
            let _ = endpoint.events.push(EVENT_ERROR);
            return std::ptr::null_mut();
        }
    };

    update_endpoint(endpoint, |state| {
        state.accepted_connections += 1;
        state.last_error.clear();
    });
    let _ = endpoint.events.push(EVENT_CONNECTED);
    new_conn_handle(
        &endpoint.rt,
        conn,
        endpoint_local_addr_string(&endpoint.endpoint),
    )
}

#[no_mangle]
/// Close an endpoint and wait for its QUIC runtime to go idle.
///
/// # Safety
///
/// `ep` must be null or an endpoint pointer returned by this module that has
/// not already been closed.
pub unsafe extern "C" fn hew_quic_endpoint_close(ep: *mut HewQuicEndpoint) {
    if ep.is_null() {
        return;
    }
    // SAFETY: `ep` is non-null and ownership is transferred back exactly once
    // on close.
    let endpoint = unsafe { Box::from_raw(ep) };
    endpoint.rt.block_on(async {
        endpoint.endpoint.close(0u32.into(), b"closed");
        endpoint.endpoint.wait_idle().await;
    });
}

#[no_mangle]
/// Block until an endpoint event is available.
///
/// # Safety
///
/// `ep` must be null or a live endpoint pointer returned by this module.
pub unsafe extern "C" fn hew_quic_endpoint_on_event(ep: *mut HewQuicEndpoint) -> *mut HewQuicEvent {
    if ep.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `ep` is non-null and must come from this module per caller
    // contract.
    let endpoint = unsafe { &*ep };
    event_ptr(endpoint.events.pop_blocking().unwrap_or(EVENT_ERROR))
}

#[no_mangle]
/// Return the endpoint's bound local address as a malloc-backed C string.
///
/// # Safety
///
/// `ep` must be null or a live endpoint pointer returned by this module.
pub unsafe extern "C" fn hew_quic_endpoint_local_addr(ep: *const HewQuicEndpoint) -> *mut c_char {
    if ep.is_null() {
        return empty_c_string();
    }
    // SAFETY: `ep` is non-null and must come from this module per caller
    // contract.
    let endpoint = unsafe { &*ep };
    let value = endpoint
        .observation
        .lock()
        .ok()
        .map_or_else(String::new, |state| state.local_addr.clone());
    str_to_malloc(&value)
}

#[no_mangle]
/// Return how many incoming connections this endpoint has accepted.
///
/// # Safety
///
/// `ep` must be null or a live endpoint pointer returned by this module.
pub unsafe extern "C" fn hew_quic_endpoint_accepted_connections(ep: *const HewQuicEndpoint) -> i64 {
    if ep.is_null() {
        return 0;
    }
    // SAFETY: `ep` is non-null and must come from this module per caller
    // contract.
    let endpoint = unsafe { &*ep };
    endpoint
        .observation
        .lock()
        .ok()
        .map_or(0, |state| state.accepted_connections)
}

#[no_mangle]
/// Return the endpoint's last observed error as a malloc-backed C string.
///
/// # Safety
///
/// `ep` must be null or a live endpoint pointer returned by this module.
pub unsafe extern "C" fn hew_quic_endpoint_last_error(ep: *const HewQuicEndpoint) -> *mut c_char {
    if ep.is_null() {
        return empty_c_string();
    }
    // SAFETY: `ep` is non-null and must come from this module per caller
    // contract.
    let endpoint = unsafe { &*ep };
    let value = endpoint
        .observation
        .lock()
        .ok()
        .map_or_else(String::new, |state| state.last_error.clone());
    str_to_malloc(&value)
}

// ── Connection methods ────────────────────────────────────────────────────────

#[no_mangle]
/// Open a new bidirectional stream on an established connection.
///
/// # Safety
///
/// `conn` must be null or a live connection pointer returned by this module.
pub unsafe extern "C" fn hew_quic_conn_open_stream(conn: *mut HewQuicConn) -> *mut HewQuicStream {
    if conn.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `conn` is non-null and must come from this module per caller
    // contract.
    let conn = unsafe { &*conn };

    let result = conn
        .rt
        .block_on(async { conn.conn.open_bi().await.map_err(|err| err.to_string()) });
    let (send, recv) = match result {
        Ok(parts) => parts,
        Err(err) => {
            update_connection(conn, |state| state.last_error = err);
            let _ = conn.events.push(EVENT_ERROR);
            return std::ptr::null_mut();
        }
    };

    update_connection(conn, |state| {
        state.opened_streams += 1;
        state.last_error.clear();
    });
    let _ = conn.events.push(EVENT_STREAM_OPENED);
    new_stream_handle(&conn.rt, send, recv, &conn.events)
}

#[no_mangle]
/// Accept the next incoming bidirectional stream on a connection.
///
/// # Safety
///
/// `conn` must be null or a live connection pointer returned by this module.
pub unsafe extern "C" fn hew_quic_conn_accept_stream(conn: *mut HewQuicConn) -> *mut HewQuicStream {
    if conn.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `conn` is non-null and must come from this module per caller
    // contract.
    let conn = unsafe { &*conn };

    let result = conn
        .rt
        .block_on(async { conn.conn.accept_bi().await.map_err(|err| err.to_string()) });
    let (send, recv) = match result {
        Ok(parts) => parts,
        Err(err) => {
            update_connection(conn, |state| state.last_error = err);
            let _ = conn.events.push(EVENT_ERROR);
            return std::ptr::null_mut();
        }
    };

    update_connection(conn, |state| {
        state.accepted_streams += 1;
        state.last_error.clear();
    });
    let _ = conn.events.push(EVENT_STREAM_OPENED);
    new_stream_handle(&conn.rt, send, recv, &conn.events)
}

#[no_mangle]
/// Close a connection and release its handle.
///
/// # Safety
///
/// `conn` must be null or a connection pointer returned by this module that has
/// not already been disconnected.
pub unsafe extern "C" fn hew_quic_conn_disconnect(conn: *mut HewQuicConn) -> c_int {
    if conn.is_null() {
        return -1;
    }
    // SAFETY: `conn` is non-null and ownership is transferred back exactly once
    // on disconnect.
    let conn = unsafe { Box::from_raw(conn) };
    conn.conn.close(0u32.into(), b"disconnected");
    0
}

#[no_mangle]
/// Block until a connection event is available.
///
/// # Safety
///
/// `conn` must be null or a live connection pointer returned by this module.
pub unsafe extern "C" fn hew_quic_conn_on_event(conn: *mut HewQuicConn) -> *mut HewQuicEvent {
    if conn.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `conn` is non-null and must come from this module per caller
    // contract.
    let conn = unsafe { &*conn };
    event_ptr(conn.events.pop_blocking().unwrap_or(EVENT_ERROR))
}

#[no_mangle]
/// Return the connection's local address as a malloc-backed C string.
///
/// # Safety
///
/// `conn` must be null or a live connection pointer returned by this module.
pub unsafe extern "C" fn hew_quic_conn_local_addr(conn: *const HewQuicConn) -> *mut c_char {
    if conn.is_null() {
        return empty_c_string();
    }
    // SAFETY: `conn` is non-null and must come from this module per caller
    // contract.
    let conn = unsafe { &*conn };
    let value = conn
        .observation
        .lock()
        .ok()
        .map_or_else(String::new, |state| state.local_addr.clone());
    str_to_malloc(&value)
}

#[no_mangle]
/// Return the remote peer address as a malloc-backed C string.
///
/// # Safety
///
/// `conn` must be null or a live connection pointer returned by this module.
pub unsafe extern "C" fn hew_quic_conn_peer_addr(conn: *const HewQuicConn) -> *mut c_char {
    if conn.is_null() {
        return empty_c_string();
    }
    // SAFETY: `conn` is non-null and must come from this module per caller
    // contract.
    let conn = unsafe { &*conn };
    let value = conn.observation.lock().ok().map_or_else(
        || conn.conn.remote_address().to_string(),
        |state| state.peer_addr.clone(),
    );
    str_to_malloc(&value)
}

#[no_mangle]
/// Return how many streams were opened locally on this connection.
///
/// # Safety
///
/// `conn` must be null or a live connection pointer returned by this module.
pub unsafe extern "C" fn hew_quic_conn_opened_streams(conn: *const HewQuicConn) -> i64 {
    if conn.is_null() {
        return 0;
    }
    // SAFETY: `conn` is non-null and must come from this module per caller
    // contract.
    let conn = unsafe { &*conn };
    conn.observation
        .lock()
        .ok()
        .map_or(0, |state| state.opened_streams)
}

#[no_mangle]
/// Return how many incoming streams were accepted on this connection.
///
/// # Safety
///
/// `conn` must be null or a live connection pointer returned by this module.
pub unsafe extern "C" fn hew_quic_conn_accepted_streams(conn: *const HewQuicConn) -> i64 {
    if conn.is_null() {
        return 0;
    }
    // SAFETY: `conn` is non-null and must come from this module per caller
    // contract.
    let conn = unsafe { &*conn };
    conn.observation
        .lock()
        .ok()
        .map_or(0, |state| state.accepted_streams)
}

#[no_mangle]
/// Return the current RTT estimate in milliseconds.
///
/// # Safety
///
/// `conn` must be null or a live connection pointer returned by this module.
pub unsafe extern "C" fn hew_quic_conn_rtt_ms(conn: *const HewQuicConn) -> i32 {
    if conn.is_null() {
        return -1;
    }
    // SAFETY: `conn` is non-null and must come from this module per caller
    // contract.
    let conn = unsafe { &*conn };
    let millis = conn.conn.stats().path.rtt.as_millis();
    i32::try_from(millis).unwrap_or(i32::MAX)
}

#[no_mangle]
/// Return the total transmitted UDP bytes observed by Quinn for this
/// connection.
///
/// # Safety
///
/// `conn` must be null or a live connection pointer returned by this module.
pub unsafe extern "C" fn hew_quic_conn_bytes_sent(conn: *const HewQuicConn) -> i64 {
    if conn.is_null() {
        return 0;
    }
    // SAFETY: `conn` is non-null and must come from this module per caller
    // contract.
    let conn = unsafe { &*conn };
    i64::try_from(conn.conn.stats().udp_tx.bytes).unwrap_or(i64::MAX)
}

#[no_mangle]
/// Return the total received UDP bytes observed by Quinn for this connection.
///
/// # Safety
///
/// `conn` must be null or a live connection pointer returned by this module.
pub unsafe extern "C" fn hew_quic_conn_bytes_received(conn: *const HewQuicConn) -> i64 {
    if conn.is_null() {
        return 0;
    }
    // SAFETY: `conn` is non-null and must come from this module per caller
    // contract.
    let conn = unsafe { &*conn };
    i64::try_from(conn.conn.stats().udp_rx.bytes).unwrap_or(i64::MAX)
}

#[no_mangle]
/// Report whether this connection has observed a close reason.
///
/// # Safety
///
/// `conn` must be null or a live connection pointer returned by this module.
pub unsafe extern "C" fn hew_quic_conn_is_closed(conn: *const HewQuicConn) -> bool {
    if conn.is_null() {
        return true;
    }
    // SAFETY: `conn` is non-null and must come from this module per caller
    // contract.
    let conn = unsafe { &*conn };
    conn.conn.close_reason().is_some()
}

#[no_mangle]
/// Return the connection's last observed error as a malloc-backed C string.
///
/// # Safety
///
/// `conn` must be null or a live connection pointer returned by this module.
pub unsafe extern "C" fn hew_quic_conn_last_error(conn: *const HewQuicConn) -> *mut c_char {
    if conn.is_null() {
        return empty_c_string();
    }
    // SAFETY: `conn` is non-null and must come from this module per caller
    // contract.
    let conn = unsafe { &*conn };
    str_to_malloc(&connection_last_error(conn))
}

// ── Stream methods ────────────────────────────────────────────────────────────

#[no_mangle]
/// Send a Hew byte vector over a QUIC stream.
///
/// # Safety
///
/// `stream` must be null or a live stream pointer returned by this module.
/// `data` must be null or a valid Hew byte vector pointer for the duration of
/// this call. Ownership of `data` is not transferred.
pub unsafe extern "C" fn hew_quic_stream_send(
    stream: *mut HewQuicStream,
    data: *mut HewVec,
) -> c_int {
    if stream.is_null() || data.is_null() {
        return -1;
    }
    // SAFETY: `stream` is non-null and must come from this module per caller
    // contract.
    let stream = unsafe { &*stream };
    // SAFETY: `data` is a valid HewVec per caller contract.
    let bytes = unsafe { hew_cabi::vec::hwvec_to_u8(data) };

    let Ok(mut send) = stream.send.lock() else {
        update_stream(stream, |state| {
            state.last_error = String::from("stream send lock poisoned");
        });
        let _ = stream.events.push(EVENT_ERROR);
        return -1;
    };
    match stream.rt.block_on(async { send.write_all(&bytes).await }) {
        Ok(()) => {
            update_stream(stream, |state| {
                state.bytes_sent = state.bytes_sent.saturating_add(to_i64_count(bytes.len()));
                state.last_error.clear();
            });
            0
        }
        Err(err) => {
            update_stream(stream, |state| state.last_error = err.to_string());
            let _ = stream.events.push(EVENT_ERROR);
            -1
        }
    }
}

#[no_mangle]
/// Receive one chunk of data from a QUIC stream as a Hew byte vector.
///
/// # Safety
///
/// `stream` must be null or a live stream pointer returned by this module.
pub unsafe extern "C" fn hew_quic_stream_recv(stream: *mut HewQuicStream) -> *mut HewVec {
    if stream.is_null() {
        return empty_hwvec();
    }
    // SAFETY: `stream` is non-null and must come from this module per caller
    // contract.
    let stream = unsafe { &*stream };

    let mut buf = vec![0u8; RECV_BUFFER_SIZE];
    let Ok(mut recv) = stream.recv.lock() else {
        update_stream(stream, |state| {
            state.last_error = String::from("stream recv lock poisoned");
        });
        let _ = stream.events.push(EVENT_ERROR);
        return empty_hwvec();
    };
    match stream.rt.block_on(async { recv.read(&mut buf).await }) {
        Ok(Some(n)) => {
            update_stream(stream, |state| {
                state.bytes_received = state.bytes_received.saturating_add(to_i64_count(n));
                state.last_error.clear();
            });
            // SAFETY: `buf[..n]` is a valid slice of bytes.
            unsafe { hew_cabi::vec::u8_to_hwvec(&buf[..n]) }
        }
        Ok(None) => {
            update_stream(stream, |state| {
                state.recv_closed = true;
                state.last_error.clear();
            });
            stream.notify_closed();
            empty_hwvec()
        }
        Err(err) => {
            update_stream(stream, |state| state.last_error = err.to_string());
            let _ = stream.events.push(EVENT_ERROR);
            empty_hwvec()
        }
    }
}

#[no_mangle]
/// Gracefully finish the send side of a QUIC stream.
///
/// # Safety
///
/// `stream` must be null or a live stream pointer returned by this module.
pub unsafe extern "C" fn hew_quic_stream_finish(stream: *mut HewQuicStream) -> c_int {
    if stream.is_null() {
        return -1;
    }
    // SAFETY: `stream` is non-null and must come from this module per caller
    // contract.
    let stream = unsafe { &*stream };
    let Ok(mut send) = stream.send.lock() else {
        update_stream(stream, |state| {
            state.last_error = String::from("stream send lock poisoned");
        });
        let _ = stream.events.push(EVENT_ERROR);
        return -1;
    };
    match stream.rt.block_on(async { send.finish() }) {
        Ok(()) => {
            update_stream(stream, |state| {
                state.send_closed = true;
                state.last_error.clear();
            });
            0
        }
        Err(err) => {
            update_stream(stream, |state| state.last_error = err.to_string());
            let _ = stream.events.push(EVENT_ERROR);
            -1
        }
    }
}

#[no_mangle]
/// Abruptly reset the send side of a QUIC stream.
///
/// # Safety
///
/// `stream` must be null or a live stream pointer returned by this module.
pub unsafe extern "C" fn hew_quic_stream_stop(
    stream: *mut HewQuicStream,
    error_code: i64,
) -> c_int {
    if stream.is_null() {
        return -1;
    }
    // SAFETY: `stream` is non-null and must come from this module per caller
    // contract.
    let stream = unsafe { &*stream };
    let Ok(mut send) = stream.send.lock() else {
        update_stream(stream, |state| {
            state.last_error = String::from("stream send lock poisoned");
        });
        let _ = stream.events.push(EVENT_ERROR);
        return -1;
    };
    let code = quinn::VarInt::try_from(error_code.unsigned_abs()).unwrap_or(quinn::VarInt::MAX);
    match send.reset(code) {
        Ok(()) => {
            update_stream(stream, |state| {
                state.send_closed = true;
                state.last_error.clear();
            });
            stream.notify_closed();
            0
        }
        Err(err) => {
            update_stream(stream, |state| state.last_error = err.to_string());
            let _ = stream.events.push(EVENT_ERROR);
            -1
        }
    }
}

#[no_mangle]
/// Release a stream handle after finishing or stopping it.
///
/// # Safety
///
/// `stream` must be null or a stream pointer returned by this module that has
/// not already been closed.
pub unsafe extern "C" fn hew_quic_stream_close(stream: *mut HewQuicStream) -> c_int {
    if stream.is_null() {
        return -1;
    }
    // SAFETY: `stream` is non-null and ownership is transferred back exactly
    // once on close.
    let stream = unsafe { Box::from_raw(stream) };
    stream.notify_closed();
    0
}

#[no_mangle]
/// Return the total bytes sent through this stream handle.
///
/// # Safety
///
/// `stream` must be null or a live stream pointer returned by this module.
pub unsafe extern "C" fn hew_quic_stream_bytes_sent(stream: *const HewQuicStream) -> i64 {
    if stream.is_null() {
        return 0;
    }
    // SAFETY: `stream` is non-null and must come from this module per caller
    // contract.
    let stream = unsafe { &*stream };
    stream
        .observation
        .lock()
        .ok()
        .map_or(0, |state| state.bytes_sent)
}

#[no_mangle]
/// Return the total bytes received through this stream handle.
///
/// # Safety
///
/// `stream` must be null or a live stream pointer returned by this module.
pub unsafe extern "C" fn hew_quic_stream_bytes_received(stream: *const HewQuicStream) -> i64 {
    if stream.is_null() {
        return 0;
    }
    // SAFETY: `stream` is non-null and must come from this module per caller
    // contract.
    let stream = unsafe { &*stream };
    stream
        .observation
        .lock()
        .ok()
        .map_or(0, |state| state.bytes_received)
}

#[no_mangle]
/// Report whether the local send side has been finished or stopped.
///
/// # Safety
///
/// `stream` must be null or a live stream pointer returned by this module.
pub unsafe extern "C" fn hew_quic_stream_send_closed(stream: *const HewQuicStream) -> bool {
    if stream.is_null() {
        return true;
    }
    // SAFETY: `stream` is non-null and must come from this module per caller
    // contract.
    let stream = unsafe { &*stream };
    stream
        .observation
        .lock()
        .ok()
        .is_none_or(|state| state.send_closed)
}

#[no_mangle]
/// Report whether EOF has been observed on the receive side.
///
/// # Safety
///
/// `stream` must be null or a live stream pointer returned by this module.
pub unsafe extern "C" fn hew_quic_stream_recv_closed(stream: *const HewQuicStream) -> bool {
    if stream.is_null() {
        return true;
    }
    // SAFETY: `stream` is non-null and must come from this module per caller
    // contract.
    let stream = unsafe { &*stream };
    stream
        .observation
        .lock()
        .ok()
        .is_none_or(|state| state.recv_closed)
}

#[no_mangle]
/// Return the stream's last observed error as a malloc-backed C string.
///
/// # Safety
///
/// `stream` must be null or a live stream pointer returned by this module.
pub unsafe extern "C" fn hew_quic_stream_last_error(stream: *const HewQuicStream) -> *mut c_char {
    if stream.is_null() {
        return empty_c_string();
    }
    // SAFETY: `stream` is non-null and must come from this module per caller
    // contract.
    let stream = unsafe { &*stream };
    let value = stream
        .observation
        .lock()
        .ok()
        .map_or_else(String::new, |state| state.last_error.clone());
    str_to_malloc(&value)
}

// ── Event methods ─────────────────────────────────────────────────────────────

#[no_mangle]
/// Read the numeric kind for an event handle.
///
/// # Safety
///
/// `event` must be null or a live event pointer returned by this module.
pub unsafe extern "C" fn hew_quic_event_kind(event: *const HewQuicEvent) -> c_int {
    if event.is_null() {
        return -1;
    }
    // SAFETY: `event` is non-null and must point to a live event allocated by
    // this module.
    unsafe { (*event).kind }
}

#[no_mangle]
/// Free an event handle returned by this module.
///
/// # Safety
///
/// `event` must be null or an event pointer returned by this module that has
/// not already been freed.
pub unsafe extern "C" fn hew_quic_event_free(event: *mut HewQuicEvent) {
    if event.is_null() {
        return;
    }
    // SAFETY: `event` is non-null and ownership is transferred back exactly
    // once on free.
    drop(unsafe { Box::from_raw(event) });
}

// ── Unit tests ────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{c_void, CStr, CString};
    use std::thread;

    use hew_cabi::vec::{hew_vec_free, hwvec_to_u8, u8_to_hwvec};

    unsafe extern "C" {
        fn free(ptr: *mut c_void);
    }

    unsafe fn take_event_kind(event: *mut HewQuicEvent) -> i32 {
        assert!(!event.is_null(), "event pointer must not be null");
        // SAFETY: event is asserted non-null and comes from the QUIC API.
        let kind = unsafe { hew_quic_event_kind(event) };
        // SAFETY: event was allocated by the QUIC API.
        unsafe { hew_quic_event_free(event) };
        kind
    }

    unsafe fn take_bytes(vec: *mut HewVec) -> Vec<u8> {
        // SAFETY: vec comes from the QUIC API.
        let bytes = unsafe { hwvec_to_u8(vec) };
        // SAFETY: vec was allocated by the Hew runtime.
        unsafe { hew_vec_free(vec) };
        bytes
    }

    unsafe fn take_string(ptr: *mut c_char) -> String {
        if ptr.is_null() {
            return String::new();
        }
        // SAFETY: ptr is a valid NUL-terminated string allocated by str_to_malloc.
        let value = unsafe { CStr::from_ptr(ptr) }
            .to_string_lossy()
            .into_owned();
        // SAFETY: ptr was allocated via libc::malloc.
        unsafe { free(ptr.cast()) };
        value
    }

    fn assert_resolver_error(
        result: Result<SocketAddr, String>,
        expected_context: &str,
        expected_fragment: &str,
    ) {
        let err = result.expect_err("resolver should fail");
        assert!(
            err.contains(expected_context),
            "expected `{expected_context}` in resolver error: {err}"
        );
        assert!(
            err.contains(expected_fragment),
            "expected `{expected_fragment}` in resolver error: {err}"
        );
        assert_eq!(get_constructor_last_error(), err);
    }

    fn poison_event_queue(events: &Arc<EventQueue>, queued_kind: Option<i32>) {
        let poison_events = Arc::clone(events);
        let poisoner = thread::spawn(move || {
            let mut guard = poison_events
                .queue
                .lock()
                .expect("queue lock should not be poisoned yet");
            if let Some(kind) = queued_kind {
                guard.push_back(kind);
                poison_events.ready.notify_one();
            }
            panic!("poison event queue mutex");
        });
        assert!(poisoner.join().is_err(), "poisoner thread must panic");
    }

    #[expect(
        clippy::too_many_lines,
        reason = "Complex QUIC protocol handling; splitting would reduce clarity"
    )]
    fn run_loopback(server_ep_ptr: *mut HewQuicEndpoint, client_ep_ptr: *mut HewQuicEndpoint) {
        assert!(!server_ep_ptr.is_null(), "server endpoint must not be null");
        assert!(!client_ep_ptr.is_null(), "client endpoint must not be null");

        // SAFETY: server_ep_ptr is non-null as asserted above.
        let server_port = unsafe { &*server_ep_ptr }
            .endpoint
            .local_addr()
            .expect("server endpoint must have a local address")
            .port();
        let server_ep_addr = server_ep_ptr as usize;
        let barrier = Arc::new(std::sync::Barrier::new(2));
        let barrier_server = Arc::clone(&barrier);

        let server_thread = thread::spawn(move || {
            let server_ep_ptr = server_ep_addr as *mut HewQuicEndpoint;

            // SAFETY: server_ep_ptr is valid and owned by this thread.
            let conn_ptr = unsafe { hew_quic_endpoint_accept(server_ep_ptr) };
            assert!(!conn_ptr.is_null(), "server must accept a connection");
            assert_eq!(
                // SAFETY: event pointer comes from QUIC API.
                unsafe { take_event_kind(hew_quic_endpoint_on_event(server_ep_ptr)) },
                EVENT_CONNECTED
            );
            // SAFETY: endpoint getters return malloc-backed strings or primitive values.
            let server_local = unsafe { take_string(hew_quic_endpoint_local_addr(server_ep_ptr)) };
            // SAFETY: repeated observation must be non-destructive.
            let server_local_again =
                unsafe { take_string(hew_quic_endpoint_local_addr(server_ep_ptr)) };
            assert_eq!(server_local, server_local_again);
            assert!(server_local.contains(':'));
            assert_eq!(
                // SAFETY: accepted connection count is readable on a live endpoint.
                unsafe { hew_quic_endpoint_accepted_connections(server_ep_ptr) },
                1
            );
            // SAFETY: endpoint getters return malloc-backed strings or primitive values.
            assert!(unsafe { take_string(hew_quic_endpoint_last_error(server_ep_ptr)) }.is_empty());

            // SAFETY: connection getters return malloc-backed strings or primitive values.
            let server_conn_local = unsafe { take_string(hew_quic_conn_local_addr(conn_ptr)) };
            // SAFETY: connection getters return malloc-backed strings or primitive values.
            let server_conn_peer = unsafe { take_string(hew_quic_conn_peer_addr(conn_ptr)) };
            assert!(!server_conn_local.is_empty());
            assert!(server_conn_peer.contains(':'));
            // SAFETY: telemetry getters are valid on a live connection.
            assert_eq!(unsafe { hew_quic_conn_opened_streams(conn_ptr) }, 0);
            // SAFETY: telemetry getters are valid on a live connection.
            assert_eq!(unsafe { hew_quic_conn_accepted_streams(conn_ptr) }, 0);
            // SAFETY: telemetry getters are valid on a live connection.
            assert!(unsafe { hew_quic_conn_rtt_ms(conn_ptr) } >= 0);
            // SAFETY: telemetry getters are valid on a live connection.
            assert!(unsafe { hew_quic_conn_bytes_sent(conn_ptr) } >= 0);
            // SAFETY: telemetry getters are valid on a live connection.
            assert!(unsafe { hew_quic_conn_bytes_received(conn_ptr) } >= 0);
            // SAFETY: last error getter returns malloc-backed strings.
            assert!(unsafe { take_string(hew_quic_conn_last_error(conn_ptr)) }.is_empty());

            // SAFETY: conn_ptr is valid.
            let stream_ptr = unsafe { hew_quic_conn_accept_stream(conn_ptr) };
            assert!(!stream_ptr.is_null(), "server must accept a stream");
            assert_eq!(
                // SAFETY: event pointer comes from QUIC API.
                unsafe { take_event_kind(hew_quic_conn_on_event(conn_ptr)) },
                EVENT_STREAM_OPENED
            );
            // SAFETY: accepted stream count is readable on a live connection.
            assert_eq!(unsafe { hew_quic_conn_accepted_streams(conn_ptr) }, 1);

            // SAFETY: stream_ptr is valid.
            let data = unsafe { take_bytes(hew_quic_stream_recv(stream_ptr)) };
            assert_eq!(data, b"hello quic", "server must receive sent bytes");
            assert_eq!(
                // SAFETY: stream observation getters are valid on a live stream.
                unsafe { hew_quic_stream_bytes_received(stream_ptr) },
                i64::try_from(data.len()).unwrap()
            );
            // SAFETY: stream observation getters are valid on a live stream.
            assert_eq!(unsafe { hew_quic_stream_bytes_sent(stream_ptr) }, 0);
            // SAFETY: stream observation getters are valid on a live stream.
            assert!(!unsafe { hew_quic_stream_send_closed(stream_ptr) });
            // SAFETY: stream observation getters are valid on a live stream.
            assert!(!unsafe { hew_quic_stream_recv_closed(stream_ptr) });
            // SAFETY: stream last error getter returns malloc-backed strings.
            assert!(unsafe { take_string(hew_quic_stream_last_error(stream_ptr)) }.is_empty());
            // SAFETY: connection telemetry getters are valid on a live connection.
            assert!(unsafe { hew_quic_conn_bytes_received(conn_ptr) } > 0);

            // SAFETY: data slice is valid.
            let reply = unsafe { u8_to_hwvec(&data) };
            // SAFETY: stream_ptr and reply are valid.
            let rc = unsafe { hew_quic_stream_send(stream_ptr, reply) };
            assert_eq!(rc, 0, "server echo send must succeed");
            // SAFETY: reply was allocated by the Hew runtime.
            unsafe { hew_vec_free(reply) };
            assert_eq!(
                // SAFETY: stream telemetry getters are valid on a live stream.
                unsafe { hew_quic_stream_bytes_sent(stream_ptr) },
                i64::try_from(data.len()).unwrap()
            );
            // SAFETY: connection telemetry getters are valid on a live connection.
            assert!(unsafe { hew_quic_conn_bytes_sent(conn_ptr) } > 0);

            // SAFETY: stream_ptr is valid.
            assert_eq!(unsafe { hew_quic_stream_finish(stream_ptr) }, 0);
            // SAFETY: stream state getters are valid on a live stream.
            assert!(unsafe { hew_quic_stream_send_closed(stream_ptr) });
            // SAFETY: stream_ptr is still valid after finish.
            assert_eq!(unsafe { hew_quic_stream_close(stream_ptr) }, 0);
            assert_eq!(
                // SAFETY: event pointer comes from QUIC API.
                unsafe { take_event_kind(hew_quic_conn_on_event(conn_ptr)) },
                EVENT_STREAM_CLOSED
            );

            barrier_server.wait();
            assert_eq!(
                // SAFETY: event pointer comes from QUIC API.
                unsafe { take_event_kind(hew_quic_conn_on_event(conn_ptr)) },
                EVENT_DISCONNECTED
            );
            // SAFETY: closed-state getter is valid on a live connection.
            assert!(unsafe { hew_quic_conn_is_closed(conn_ptr) });

            // SAFETY: conn_ptr and server_ep_ptr are valid.
            assert_eq!(unsafe { hew_quic_conn_disconnect(conn_ptr) }, 0);
            // SAFETY: server_ep_ptr is valid.
            unsafe { hew_quic_endpoint_close(server_ep_ptr) };
        });

        // SAFETY: endpoint getters return malloc-backed strings or primitive values.
        let client_local = unsafe { take_string(hew_quic_endpoint_local_addr(client_ep_ptr)) };
        // SAFETY: repeated observation must be non-destructive.
        let client_local_again =
            unsafe { take_string(hew_quic_endpoint_local_addr(client_ep_ptr)) };
        assert_eq!(client_local, client_local_again);
        assert!(client_local.contains(':'));
        assert_eq!(
            // SAFETY: endpoint state getters are valid on a live endpoint.
            unsafe { hew_quic_endpoint_accepted_connections(client_ep_ptr) },
            0
        );
        // SAFETY: last error getter returns malloc-backed strings.
        assert!(unsafe { take_string(hew_quic_endpoint_last_error(client_ep_ptr)) }.is_empty());

        let addr = CString::new(format!("127.0.0.1:{server_port}")).expect("valid address string");
        let sn = c"localhost";
        // SAFETY: addr, sn, and client_ep_ptr are valid.
        let conn_ptr =
            unsafe { hew_quic_endpoint_connect(client_ep_ptr, addr.as_ptr(), sn.as_ptr()) };
        assert!(!conn_ptr.is_null(), "client must connect");
        assert_eq!(
            // SAFETY: event pointer comes from QUIC API.
            unsafe { take_event_kind(hew_quic_endpoint_on_event(client_ep_ptr)) },
            EVENT_CONNECTED
        );
        // SAFETY: connection getters return malloc-backed strings or primitive values.
        let client_conn_local = unsafe { take_string(hew_quic_conn_local_addr(conn_ptr)) };
        // SAFETY: connection getters return malloc-backed strings or primitive values.
        let client_conn_peer = unsafe { take_string(hew_quic_conn_peer_addr(conn_ptr)) };
        assert!(!client_conn_local.is_empty());
        assert!(client_conn_peer.ends_with(&server_port.to_string()));
        // SAFETY: telemetry getters are valid on a live connection.
        assert_eq!(unsafe { hew_quic_conn_opened_streams(conn_ptr) }, 0);
        // SAFETY: telemetry getters are valid on a live connection.
        assert_eq!(unsafe { hew_quic_conn_accepted_streams(conn_ptr) }, 0);
        // SAFETY: telemetry getters are valid on a live connection.
        let bytes_sent_before = unsafe { hew_quic_conn_bytes_sent(conn_ptr) };
        // SAFETY: telemetry getters are valid on a live connection.
        let bytes_received_before = unsafe { hew_quic_conn_bytes_received(conn_ptr) };
        // SAFETY: telemetry getters are valid on a live connection.
        assert!(unsafe { hew_quic_conn_rtt_ms(conn_ptr) } >= 0);
        // SAFETY: last error getter returns malloc-backed strings.
        assert!(unsafe { take_string(hew_quic_conn_last_error(conn_ptr)) }.is_empty());

        // SAFETY: conn_ptr is valid.
        let stream_ptr = unsafe { hew_quic_conn_open_stream(conn_ptr) };
        assert!(!stream_ptr.is_null(), "client must open stream");
        assert_eq!(
            // SAFETY: event pointer comes from QUIC API.
            unsafe { take_event_kind(hew_quic_conn_on_event(conn_ptr)) },
            EVENT_STREAM_OPENED
        );
        // SAFETY: opened stream count is readable on a live connection.
        assert_eq!(unsafe { hew_quic_conn_opened_streams(conn_ptr) }, 1);

        // SAFETY: byte slice is valid.
        let msg = unsafe { u8_to_hwvec(b"hello quic") };
        // SAFETY: stream_ptr and msg are valid.
        assert_eq!(unsafe { hew_quic_stream_send(stream_ptr, msg) }, 0);
        // SAFETY: msg was allocated by the Hew runtime.
        unsafe { hew_vec_free(msg) };
        // SAFETY: stream state getters are valid on a live stream.
        assert_eq!(unsafe { hew_quic_stream_bytes_sent(stream_ptr) }, 10);
        // SAFETY: stream state getters are valid on a live stream.
        assert_eq!(unsafe { hew_quic_stream_bytes_received(stream_ptr) }, 0);
        // SAFETY: stream state getters are valid on a live stream.
        assert!(!unsafe { hew_quic_stream_send_closed(stream_ptr) });
        // SAFETY: stream state getters are valid on a live stream.
        assert!(!unsafe { hew_quic_stream_recv_closed(stream_ptr) });
        // SAFETY: connection telemetry getters are valid on a live connection.
        assert!(unsafe { hew_quic_conn_bytes_sent(conn_ptr) } >= bytes_sent_before);

        // SAFETY: stream_ptr is valid.
        let reply = unsafe { take_bytes(hew_quic_stream_recv(stream_ptr)) };
        assert_eq!(reply, b"hello quic", "echoed data must match sent data");
        // SAFETY: state getters are valid on a live connection or stream.
        assert_eq!(unsafe { hew_quic_stream_bytes_received(stream_ptr) }, 10);
        // SAFETY: state getters are valid on a live connection or stream.
        assert!(unsafe { hew_quic_conn_bytes_received(conn_ptr) } >= bytes_received_before);
        // SAFETY: state getters are valid on a live connection or stream.
        assert!(unsafe { hew_quic_conn_rtt_ms(conn_ptr) } >= 0);
        // SAFETY: last error getter returns malloc-backed strings.
        assert!(unsafe { take_string(hew_quic_stream_last_error(stream_ptr)) }.is_empty());

        // SAFETY: stream_ptr is valid.
        assert_eq!(unsafe { hew_quic_stream_finish(stream_ptr) }, 0);
        // SAFETY: stream state getters are valid on a live stream.
        assert!(unsafe { hew_quic_stream_send_closed(stream_ptr) });
        // SAFETY: stream_ptr is valid.
        let eof = unsafe { take_bytes(hew_quic_stream_recv(stream_ptr)) };
        assert!(eof.is_empty(), "recv after peer finish must report EOF");
        // SAFETY: stream state getters are valid on a live stream.
        assert!(unsafe { hew_quic_stream_recv_closed(stream_ptr) });
        // SAFETY: stream_ptr is still valid after finish.
        assert_eq!(unsafe { hew_quic_stream_close(stream_ptr) }, 0);
        assert_eq!(
            // SAFETY: event pointer comes from QUIC API.
            unsafe { take_event_kind(hew_quic_conn_on_event(conn_ptr)) },
            EVENT_STREAM_CLOSED
        );

        // SAFETY: conn_ptr and client_ep_ptr are valid.
        assert_eq!(unsafe { hew_quic_conn_disconnect(conn_ptr) }, 0);
        // SAFETY: client_ep_ptr is valid.
        unsafe { hew_quic_endpoint_close(client_ep_ptr) };
        barrier.wait();

        server_thread.join().expect("server thread must not panic");
    }

    #[test]
    fn new_client_returns_non_null() {
        let ep = hew_quic_new_client();
        assert!(!ep.is_null(), "expected non-null endpoint for client");
        // SAFETY: ep was just created.
        unsafe { hew_quic_endpoint_close(ep) };
    }

    #[test]
    fn new_client_with_ca_null_returns_null() {
        clear_constructor_last_error();
        // SAFETY: null is explicitly handled.
        let ep = unsafe { hew_quic_new_client_with_ca(std::ptr::null()) };
        assert!(ep.is_null());
    }

    #[test]
    fn new_client_null_safe_close() {
        // SAFETY: null is explicitly handled.
        unsafe { hew_quic_endpoint_close(std::ptr::null_mut()) };
    }

    #[test]
    fn insecure_client_config_succeeds() {
        let cfg = insecure_client_config();
        assert!(
            cfg.is_ok(),
            "insecure_client_config() should succeed: {cfg:?}"
        );
    }

    #[test]
    fn resolve_bind_addr_reports_parse_failure() {
        clear_constructor_last_error();
        assert_resolver_error(resolve_bind_addr("bad:host:here"), "bind", "parse");
    }

    #[test]
    fn resolve_bind_addr_reports_resolution_failure() {
        // `.invalid.` is reserved by RFC 2606, so it should fail resolution
        // without depending on a real service existing in CI.
        clear_constructor_last_error();
        assert_resolver_error(
            resolve_bind_addr("unresolvable.invalid.:443"),
            "bind",
            "resolution",
        );
    }

    #[test]
    fn resolve_bind_addr_accepts_socket_addr_literal() {
        clear_constructor_last_error();
        assert_eq!(
            resolve_bind_addr("127.0.0.1:4433").expect("literal bind address must resolve"),
            "127.0.0.1:4433".parse().expect("valid socket address")
        );
    }

    #[test]
    fn resolve_connect_addr_reports_parse_failure() {
        clear_constructor_last_error();
        assert_resolver_error(resolve_connect_addr("bad:host:here"), "connect", "parse");
    }

    #[test]
    fn resolve_connect_addr_reports_resolution_failure() {
        // `.invalid.` is reserved by RFC 2606, so it should fail resolution
        // without depending on a real service existing in CI.
        clear_constructor_last_error();
        assert_resolver_error(
            resolve_connect_addr("unresolvable.invalid.:443"),
            "connect",
            "resolution",
        );
    }

    #[test]
    fn resolve_connect_addr_accepts_socket_addr_literal() {
        clear_constructor_last_error();
        assert_eq!(
            resolve_connect_addr("127.0.0.1:4433").expect("literal remote address must resolve"),
            "127.0.0.1:4433".parse().expect("valid socket address")
        );
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
    fn event_queue_push_recovers_from_poisoned_lock() {
        let events = Arc::new(EventQueue::default());
        poison_event_queue(&events, None);

        assert!(events.push(EVENT_CONNECTED));
        let mut queue = events
            .queue
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert_eq!(queue.pop_front(), Some(EVENT_CONNECTED));
        assert!(queue.is_empty());
    }

    #[test]
    fn event_queue_pop_blocking_recovers_from_poisoned_initial_lock() {
        let events = Arc::new(EventQueue::default());
        poison_event_queue(&events, Some(EVENT_CONNECTED));

        assert_eq!(events.pop_blocking(), Some(EVENT_CONNECTED));
        let queue = events
            .queue
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(queue.is_empty());
    }

    #[test]
    fn event_queue_pop_blocking_recovers_from_poisoned_wait() {
        let events = Arc::new(EventQueue::default());
        let queue_guard = events
            .queue
            .lock()
            .expect("queue lock should not be poisoned before the test starts");
        let started = Arc::new(AtomicBool::new(false));
        let waiter_started = Arc::clone(&started);
        let waiter_events = Arc::clone(&events);
        let waiter = thread::spawn(move || {
            waiter_started.store(true, Ordering::Release);
            waiter_events.pop_blocking()
        });

        for _ in 0..1_000 {
            if started.load(Ordering::Acquire) {
                break;
            }
            thread::yield_now();
        }
        assert!(
            started.load(Ordering::Acquire),
            "waiter thread did not start"
        );
        // Drop the guard to let the waiter proceed into condvar.wait().
        // No explicit sleep is needed: poison_event_queue internally acquires
        // queue.lock(), which blocks until the waiter has released it via
        // condvar.wait() — that lock acquisition is the deterministic signal.
        drop(queue_guard);
        assert_eq!(
            {
                poison_event_queue(&events, Some(EVENT_CONNECTED));
                waiter.join().expect("waiter thread must not panic")
            },
            Some(EVENT_CONNECTED)
        );
    }

    #[test]
    fn new_server_with_tls_null_returns_null() {
        clear_constructor_last_error();
        // SAFETY: null is explicitly handled.
        let ep = unsafe {
            hew_quic_new_server_with_tls(std::ptr::null(), std::ptr::null(), std::ptr::null())
        };
        assert!(ep.is_null());
    }

    #[test]
    fn new_server_null_addr() {
        clear_constructor_last_error();
        // SAFETY: null is explicitly handled.
        let ep = unsafe { hew_quic_new_server(std::ptr::null()) };
        assert!(ep.is_null());
    }

    #[test]
    fn quic_last_error_is_empty_by_default() {
        clear_constructor_last_error();
        // SAFETY: the getter returns an owned malloc string for this thread.
        assert!(unsafe { take_string(hew_quic_last_error()) }.is_empty());
    }

    #[test]
    fn new_server_null_addr_sets_constructor_last_error() {
        clear_constructor_last_error();

        // SAFETY: null is explicitly handled.
        let ep = unsafe { hew_quic_new_server(std::ptr::null()) };
        assert!(ep.is_null());
        assert_eq!(
            // SAFETY: the getter returns an owned malloc string for this thread.
            unsafe { take_string(hew_quic_last_error()) },
            "invalid bind address"
        );
    }

    #[test]
    fn new_client_with_ca_invalid_pem_sets_constructor_last_error() {
        clear_constructor_last_error();

        let pem = c"not a pem bundle";
        // SAFETY: pem is a valid C string literal.
        let ep = unsafe { hew_quic_new_client_with_ca(pem.as_ptr()) };
        assert!(ep.is_null());
        // SAFETY: the getter returns an owned malloc string for this thread.
        let error = unsafe { take_string(hew_quic_last_error()) };
        assert!(
            error.contains("failed to configure client CA bundle"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn successful_constructor_clears_constructor_last_error() {
        clear_constructor_last_error();

        let bad_addr = c"not-an-address";
        // SAFETY: bad_addr is a valid C string literal.
        let ep = unsafe { hew_quic_new_server(bad_addr.as_ptr()) };
        assert!(ep.is_null());
        // SAFETY: the getter returns an owned malloc string for this thread.
        let error = unsafe { take_string(hew_quic_last_error()) };
        assert!(error.contains("could not resolve bind address"));
        assert!(error.contains("parse failure"));

        let good_addr = c":0";
        // SAFETY: good_addr is a valid C string literal.
        let ep = unsafe { hew_quic_new_server(good_addr.as_ptr()) };
        assert!(
            !ep.is_null(),
            "expected successful constructor to clear error"
        );
        // SAFETY: the getter returns an owned malloc string for this thread.
        assert!(unsafe { take_string(hew_quic_last_error()) }.is_empty());
        // SAFETY: ep was just created.
        unsafe { hew_quic_endpoint_close(ep) };
    }

    #[test]
    fn endpoint_connect_null_ep() {
        let addr = c"127.0.0.1:4433";
        let sn = c"localhost";
        // SAFETY: null ep is explicitly handled.
        let conn =
            unsafe { hew_quic_endpoint_connect(std::ptr::null_mut(), addr.as_ptr(), sn.as_ptr()) };
        assert!(conn.is_null());
    }

    #[test]
    fn endpoint_observation_tracks_connect_errors() {
        let ep = hew_quic_new_client();
        assert!(!ep.is_null(), "expected non-null endpoint for client");

        let bad_addr = c"not-an-address";
        let sn = c"localhost";
        // SAFETY: ep is live and both strings are valid C string literals.
        let conn = unsafe { hew_quic_endpoint_connect(ep, bad_addr.as_ptr(), sn.as_ptr()) };
        assert!(conn.is_null(), "invalid address must fail to connect");
        // SAFETY: error getter returns a malloc-backed string.
        let error = unsafe { take_string(hew_quic_endpoint_last_error(ep)) };
        assert!(
            error.contains("could not resolve connect address"),
            "expected a parse error, got `{error}`"
        );
        assert!(error.contains("parse failure"));
        assert_eq!(get_constructor_last_error(), error);

        // SAFETY: ep was created by the constructor above.
        unsafe { hew_quic_endpoint_close(ep) };
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
        let rc = unsafe { hew_quic_stream_send(std::ptr::null_mut(), std::ptr::null_mut()) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn stream_recv_null_returns_empty_vec() {
        // SAFETY: null stream is explicitly handled.
        let data = unsafe { take_bytes(hew_quic_stream_recv(std::ptr::null_mut())) };
        assert!(data.is_empty());
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
    fn stream_close_null() {
        // SAFETY: null is explicitly handled.
        let rc = unsafe { hew_quic_stream_close(std::ptr::null_mut()) };
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
        let ev = HewQuicEvent {
            kind: EVENT_STREAM_OPENED,
        };
        let ptr = (&raw const ev).cast::<HewQuicEvent>();
        // SAFETY: ptr points to a valid stack-allocated HewQuicEvent.
        let k = unsafe { hew_quic_event_kind(ptr) };
        assert_eq!(k, EVENT_STREAM_OPENED);
    }

    #[test]
    fn loopback_send_recv() {
        let addr = c":0";
        // SAFETY: addr is a valid C string literal.
        let server_ep_ptr = unsafe { hew_quic_new_server(addr.as_ptr()) };
        let client_ep_ptr = hew_quic_new_client();
        run_loopback(server_ep_ptr, client_ep_ptr);
    }

    #[test]
    fn tls_configured_loopback() {
        let cert = rcgen::generate_simple_self_signed(vec!["localhost".into()])
            .expect("self-signed test certificate");
        let cert_pem = CString::new(cert.cert.pem()).expect("certificate PEM must be NUL-free");
        let key_pem =
            CString::new(cert.signing_key.serialize_pem()).expect("key PEM must be NUL-free");
        let addr = c":0";

        // SAFETY: all C strings are valid and NUL-terminated.
        let server_ep_ptr = unsafe {
            hew_quic_new_server_with_tls(addr.as_ptr(), cert_pem.as_ptr(), key_pem.as_ptr())
        };
        // SAFETY: cert_pem is a valid C string.
        let client_ep_ptr = unsafe { hew_quic_new_client_with_ca(cert_pem.as_ptr()) };
        run_loopback(server_ep_ptr, client_ep_ptr);
    }
}
