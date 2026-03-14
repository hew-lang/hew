//! QUIC transport implementation for the Hew node mesh.
//!
//! Provides a [`HewTransportOps`] vtable backed by [quinn](https://docs.rs/quinn)
//! with built-in TLS 1.3 encryption via rustls. When using QUIC transport, the
//! Noise XX handshake is redundant and should be skipped — QUIC already provides
//! authenticated encryption.
//!
//! Each logical connection uses a single bidirectional QUIC stream. The same
//! 4-byte LE length-prefixed framing used by the TCP transport is layered on
//! top for HBF wire-format compatibility.
//!
//! # Certificate Management
//!
//! By default, a self-signed certificate is generated for mesh-internal
//! communication. Custom certificates can be provided via environment variables:
//!
//! - `HEW_QUIC_CERT` — PEM file path for the server certificate chain
//! - `HEW_QUIC_KEY` — PEM file path for the server private key
//! - `HEW_QUIC_CA` — PEM file path for the CA certificate (client trust root)

use std::ffi::{c_char, c_int, c_void, CStr};
use std::sync::Arc;
use std::time::Duration;

use quinn::{ClientConfig, Endpoint, ServerConfig};
use rcgen::generate_simple_self_signed;
use rustls::pki_types::{CertificateDer, PrivateKeyDer, PrivatePkcs8KeyDer};
use tokio::runtime::Runtime;

use crate::set_last_error;
use crate::transport::{HewTransport, HewTransportOps, HEW_CONN_INVALID};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Maximum connections (matches TCP transport).
const MAX_CONNS: usize = 64;

/// Maximum framed payload (16 MiB, matches TCP transport).
const MAX_FRAME_SIZE: usize = 16 * 1024 * 1024;

/// Number of tokio worker threads for the QUIC runtime.
const QUIC_WORKER_THREADS: usize = 2;

/// ALPN protocol identifier for Hew mesh traffic.
const HEW_ALPN: &[u8] = b"hew-mesh/1";

/// Default server name used for self-signed certificates.
const DEFAULT_SERVER_NAME: &str = "hew-mesh.local";

// ---------------------------------------------------------------------------
// QuicConn — per-connection state
// ---------------------------------------------------------------------------

/// A single QUIC connection, possibly with a bidirectional stream.
///
/// The stream is established lazily: the connecting side opens it on first
/// send/recv, and the accepting side accepts it on first send/recv.
struct QuicConn {
    connection: quinn::Connection,
    send: Option<quinn::SendStream>,
    recv: Option<quinn::RecvStream>,
    /// Whether this side initiated the connection (true) or accepted it (false).
    is_initiator: bool,
}

impl QuicConn {
    /// Ensure the bidirectional stream is open. Initiators call `open_bi()`,
    /// acceptors call `accept_bi()`.
    fn ensure_stream(&mut self, rt: &Runtime) -> Result<(), String> {
        if self.send.is_some() && self.recv.is_some() {
            return Ok(());
        }
        let conn = &self.connection;
        if self.is_initiator {
            let (send, recv) = rt
                .block_on(async { conn.open_bi().await })
                .map_err(|e| format!("open_bi: {e}"))?;
            self.send = Some(send);
            self.recv = Some(recv);
        } else {
            let (send, recv) = rt
                .block_on(async { conn.accept_bi().await })
                .map_err(|e| format!("accept_bi: {e}"))?;
            self.send = Some(send);
            self.recv = Some(recv);
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// QuicTransport
// ---------------------------------------------------------------------------

/// QUIC transport implementation.
///
/// Wraps a quinn [`Endpoint`] and a tokio [`Runtime`] for async bridging.
/// Connections are stored in a fixed-size slot array (matching the TCP model).
///
/// When listening, a background tokio task accepts incoming QUIC connections
/// and sends them through a channel. The synchronous `accept` vtable function
/// reads from this channel, avoiding `block_on` contention.
pub(crate) struct QuicTransport {
    rt: Arc<Runtime>,
    endpoint: Option<Endpoint>,
    conns: Vec<std::sync::Mutex<Option<QuicConn>>>,
    server_cert_der: Vec<u8>,
    /// Channel receiver for incoming connections (populated by background task).
    incoming_rx: std::sync::Mutex<Option<std::sync::mpsc::Receiver<QuicConn>>>,
}

impl QuicTransport {
    /// Create a new QUIC transport with the given tokio runtime and TLS config.
    fn new(rt: Arc<Runtime>) -> Self {
        let conns = (0..MAX_CONNS)
            .map(|_| std::sync::Mutex::new(None))
            .collect();
        Self {
            rt,
            endpoint: None,
            conns,
            server_cert_der: Vec::new(),
            incoming_rx: std::sync::Mutex::new(None),
        }
    }

    /// Allocate a slot for a new connection, returning its index as `conn_id`.
    fn store_conn(&self, conn: QuicConn) -> c_int {
        for (i, slot) in self.conns.iter().enumerate() {
            let Ok(mut guard) = slot.lock() else { continue };
            if guard.is_none() {
                *guard = Some(conn);
                return c_int::try_from(i).unwrap_or(HEW_CONN_INVALID);
            }
        }
        HEW_CONN_INVALID
    }

    /// Remove and drop a connection by slot index.
    fn remove_conn(&self, id: c_int) {
        let Ok(idx) = usize::try_from(id) else { return };
        if let Some(slot) = self.conns.get(idx) {
            if let Ok(mut guard) = slot.lock() {
                *guard = None;
            }
        }
    }
}

// ---------------------------------------------------------------------------
// TLS certificate helpers
// ---------------------------------------------------------------------------

/// Result of loading or generating TLS credentials.
pub(crate) struct TlsCreds {
    pub cert_chain: Vec<CertificateDer<'static>>,
    pub private_key: PrivateKeyDer<'static>,
    /// DER bytes of the leaf certificate (for client trust).
    pub cert_der: Vec<u8>,
}

/// Generate a self-signed certificate for mesh communication.
pub(crate) fn generate_self_signed_creds() -> Result<TlsCreds, String> {
    let subject_alt_names = vec![DEFAULT_SERVER_NAME.to_string(), "localhost".to_string()];
    let certified_key =
        generate_simple_self_signed(subject_alt_names).map_err(|e| format!("rcgen: {e}"))?;

    let cert_der = certified_key.cert.der().to_vec();
    let key_der = certified_key.key_pair.serialize_der();

    let cert = CertificateDer::from(cert_der.clone());
    let key = PrivateKeyDer::Pkcs8(PrivatePkcs8KeyDer::from(key_der));

    Ok(TlsCreds {
        cert_chain: vec![cert],
        private_key: key,
        cert_der,
    })
}

/// Load TLS credentials from PEM files specified by environment variables.
/// Returns `None` if neither `HEW_QUIC_CERT` nor `HEW_QUIC_KEY` is set.
pub(crate) fn load_pem_creds() -> Result<Option<TlsCreds>, String> {
    let cert_path = std::env::var("HEW_QUIC_CERT").ok();
    let key_path = std::env::var("HEW_QUIC_KEY").ok();

    let (Some(cert_path), Some(key_path)) = (cert_path, key_path) else {
        return Ok(None);
    };

    let cert_pem = std::fs::read(&cert_path).map_err(|e| format!("reading {cert_path}: {e}"))?;
    let key_pem = std::fs::read(&key_path).map_err(|e| format!("reading {key_path}: {e}"))?;

    let certs: Vec<CertificateDer<'static>> = rustls_pemfile::certs(&mut &cert_pem[..])
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| format!("parsing cert PEM: {e}"))?;

    if certs.is_empty() {
        return Err("no certificates found in PEM file".into());
    }

    let key = rustls_pemfile::private_key(&mut &key_pem[..])
        .map_err(|e| format!("parsing key PEM: {e}"))?
        .ok_or("no private key found in PEM file")?;

    let cert_der = certs[0].to_vec();

    Ok(Some(TlsCreds {
        cert_chain: certs,
        private_key: key,
        cert_der,
    }))
}

/// Load a CA certificate from PEM (for client trust).
/// If `HEW_QUIC_CA` is not set, returns `Ok(None)`.
#[allow(dead_code, reason = "reserved for custom CA trust-root support")]
pub(crate) fn load_ca_cert() -> Result<Option<CertificateDer<'static>>, String> {
    let Some(ca_path) = std::env::var("HEW_QUIC_CA").ok() else {
        return Ok(None);
    };
    let ca_pem = std::fs::read(&ca_path).map_err(|e| format!("reading {ca_path}: {e}"))?;
    let certs: Vec<CertificateDer<'static>> = rustls_pemfile::certs(&mut &ca_pem[..])
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| format!("parsing CA PEM: {e}"))?;
    Ok(certs.into_iter().next())
}

/// Build a [`ServerConfig`] from TLS credentials.
pub(crate) fn build_server_config(creds: &TlsCreds) -> Result<ServerConfig, String> {
    let provider = Arc::new(rustls::crypto::ring::default_provider());

    let mut server_crypto_config = rustls::ServerConfig::builder_with_provider(provider)
        .with_safe_default_protocol_versions()
        .map_err(|e| format!("rustls: {e}"))?
        .with_no_client_auth()
        .with_single_cert(creds.cert_chain.clone(), creds.private_key.clone_key())
        .map_err(|e| format!("rustls cert: {e}"))?;

    server_crypto_config.alpn_protocols = vec![HEW_ALPN.to_vec()];

    let mut server_config = ServerConfig::with_crypto(Arc::new(
        quinn::crypto::rustls::QuicServerConfig::try_from(server_crypto_config)
            .map_err(|e| format!("quinn crypto: {e}"))?,
    ));
    let transport = Arc::get_mut(&mut server_config.transport).unwrap();
    transport.max_idle_timeout(Some(
        quinn::IdleTimeout::try_from(Duration::from_secs(30)).unwrap(),
    ));

    Ok(server_config)
}

/// Build a [`ClientConfig`] that trusts the given server certificate DER bytes.
pub(crate) fn build_client_config(trusted_cert_der: &[u8]) -> Result<ClientConfig, String> {
    let provider = Arc::new(rustls::crypto::ring::default_provider());

    let mut root_store = rustls::RootCertStore::empty();
    let cert = CertificateDer::from(trusted_cert_der.to_vec());
    root_store
        .add(cert)
        .map_err(|e| format!("adding root cert: {e}"))?;

    let mut client_crypto = rustls::ClientConfig::builder_with_provider(provider)
        .with_safe_default_protocol_versions()
        .map_err(|e| format!("rustls: {e}"))?
        .with_root_certificates(root_store)
        .with_no_client_auth();

    client_crypto.alpn_protocols = vec![HEW_ALPN.to_vec()];

    let mut client_config = ClientConfig::new(Arc::new(
        quinn::crypto::rustls::QuicClientConfig::try_from(client_crypto)
            .map_err(|e| format!("quinn crypto: {e}"))?,
    ));
    let mut transport = quinn::TransportConfig::default();
    transport.max_idle_timeout(Some(
        quinn::IdleTimeout::try_from(Duration::from_secs(30)).unwrap(),
    ));
    client_config.transport_config(Arc::new(transport));

    Ok(client_config)
}

// ---------------------------------------------------------------------------
// Framing helpers (4-byte LE length prefix, same as TCP)
// ---------------------------------------------------------------------------

/// Write a length-prefixed frame to a QUIC [`SendStream`](quinn::SendStream) (blocking via runtime).
fn framed_send_quic(rt: &Runtime, send: &mut quinn::SendStream, data: &[u8]) -> c_int {
    if data.len() > MAX_FRAME_SIZE {
        set_last_error("frame exceeds MAX_FRAME_SIZE");
        return -1;
    }
    // data.len() ≤ MAX_FRAME_SIZE (16 MiB), which always fits in u32.
    let frame_len = u32::try_from(data.len()).unwrap_or(u32::MAX);
    let header = frame_len.to_le_bytes();

    let result = rt.block_on(async {
        send.write_all(&header).await?;
        send.write_all(data).await?;
        Ok::<_, quinn::WriteError>(())
    });

    match result {
        Ok(()) => c_int::try_from(data.len()).unwrap_or(-1),
        Err(e) => {
            set_last_error(format!("quic send: {e}"));
            -1
        }
    }
}

/// Read a length-prefixed frame from a QUIC [`RecvStream`](quinn::RecvStream) (blocking via runtime).
fn framed_recv_quic(rt: &Runtime, recv: &mut quinn::RecvStream, buf: &mut [u8]) -> c_int {
    let result = rt.block_on(async {
        // Read 4-byte header.
        let mut header = [0u8; 4];
        recv.read_exact(&mut header)
            .await
            .map_err(|e| format!("header: {e}"))?;

        let frame_len = u32::from_le_bytes(header) as usize;
        if frame_len > MAX_FRAME_SIZE {
            return Err(format!("frame too large: {frame_len}"));
        }
        if frame_len > buf.len() {
            return Err(format!("frame {frame_len} exceeds buffer {}", buf.len()));
        }

        recv.read_exact(&mut buf[..frame_len])
            .await
            .map_err(|e| format!("payload: {e}"))?;

        Ok(c_int::try_from(frame_len).unwrap_or(-1))
    });

    match result {
        Ok(n) => n,
        Err(e) => {
            set_last_error(format!("quic recv: {e}"));
            -1
        }
    }
}

// ---------------------------------------------------------------------------
// VTable callback implementations
// ---------------------------------------------------------------------------

unsafe extern "C" fn quic_connect(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
    if impl_ptr.is_null() || address.is_null() {
        return HEW_CONN_INVALID;
    }
    // SAFETY: impl_ptr checked non-null above; points to the QuicTransport
    // allocated by `hew_transport_quic_new`.
    let qt = unsafe { &*impl_ptr.cast::<QuicTransport>() };
    // SAFETY: address checked non-null above; caller guarantees a valid
    // NUL-terminated C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(address) }.to_str() else {
        return HEW_CONN_INVALID;
    };

    let Some(endpoint) = &qt.endpoint else {
        set_last_error("quic: no endpoint (call listen first or create client endpoint)");
        return HEW_CONN_INVALID;
    };

    let addr: std::net::SocketAddr = match addr_str.parse() {
        Ok(a) => a,
        Err(_) => {
            // Try resolving as host:port.
            match std::net::ToSocketAddrs::to_socket_addrs(&addr_str) {
                Ok(mut addrs) => {
                    if let Some(a) = addrs.next() {
                        a
                    } else {
                        set_last_error(format!("quic: cannot resolve {addr_str}"));
                        return HEW_CONN_INVALID;
                    }
                }
                Err(e) => {
                    set_last_error(format!("quic: resolve {addr_str}: {e}"));
                    return HEW_CONN_INVALID;
                }
            }
        }
    };

    let ep = endpoint.clone();
    let result = qt.rt.block_on(async {
        let conn = ep.connect(addr, DEFAULT_SERVER_NAME)?.await?;
        Ok::<_, Box<dyn std::error::Error>>(QuicConn {
            connection: conn,
            send: None,
            recv: None,
            is_initiator: true,
        })
    });

    match result {
        Ok(qc) => qt.store_conn(qc),
        Err(e) => {
            set_last_error(format!("quic connect: {e}"));
            HEW_CONN_INVALID
        }
    }
}

unsafe extern "C" fn quic_listen(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
    if impl_ptr.is_null() || address.is_null() {
        return -1;
    }
    // SAFETY: impl_ptr checked non-null above; points to the QuicTransport
    // allocated by `hew_transport_quic_new`. Exclusive access is sound because
    // the vtable contract requires single-threaded listen setup.
    let qt = unsafe { &mut *impl_ptr.cast::<QuicTransport>() };
    // SAFETY: address checked non-null above; caller guarantees a valid
    // NUL-terminated C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(address) }.to_str() else {
        return -1;
    };

    let addr: std::net::SocketAddr = match addr_str.parse() {
        Ok(a) => a,
        Err(_) => match std::net::ToSocketAddrs::to_socket_addrs(&addr_str) {
            Ok(mut addrs) => match addrs.next() {
                Some(a) => a,
                None => return -1,
            },
            Err(_) => return -1,
        },
    };

    // Load or generate TLS credentials.
    let creds = match load_pem_creds() {
        Ok(Some(c)) => c,
        Ok(None) => match generate_self_signed_creds() {
            Ok(c) => c,
            Err(e) => {
                set_last_error(format!("quic tls: {e}"));
                return -1;
            }
        },
        Err(e) => {
            set_last_error(format!("quic tls: {e}"));
            return -1;
        }
    };

    qt.server_cert_der.clone_from(&creds.cert_der);

    let server_config = match build_server_config(&creds) {
        Ok(c) => c,
        Err(e) => {
            set_last_error(format!("quic server config: {e}"));
            return -1;
        }
    };

    // Build a client config that trusts our own cert (for outbound connects).
    let client_config = match build_client_config(&creds.cert_der) {
        Ok(c) => c,
        Err(e) => {
            set_last_error(format!("quic client config: {e}"));
            return -1;
        }
    };

    let result = {
        // Enter the runtime context for Endpoint::server (registers I/O with reactor).
        let _guard = qt.rt.enter();
        let mut endpoint = match Endpoint::server(server_config, addr) {
            Ok(ep) => ep,
            Err(e) => {
                set_last_error(format!("quic listen: {e}"));
                return -1;
            }
        };
        endpoint.set_default_client_config(client_config);
        endpoint
    };

    // Spawn a background task that accepts incoming connections and feeds
    // them through a channel. This decouples the async accept from the
    // synchronous vtable interface.
    let (tx, rx) = std::sync::mpsc::channel::<QuicConn>();
    let accept_ep = result.clone();
    qt.rt.spawn(async move {
        loop {
            let Some(incoming) = accept_ep.accept().await else {
                break;
            };
            let Ok(conn) = incoming.await else {
                continue;
            };
            let qc = QuicConn {
                connection: conn,
                send: None,
                recv: None,
                is_initiator: false,
            };
            if tx.send(qc).is_err() {
                break;
            }
        }
    });

    qt.endpoint = Some(result);
    if let Ok(mut guard) = qt.incoming_rx.lock() {
        *guard = Some(rx);
    }
    0
}

unsafe extern "C" fn quic_accept(impl_ptr: *mut c_void, timeout_ms: c_int) -> c_int {
    if impl_ptr.is_null() {
        return HEW_CONN_INVALID;
    }
    // SAFETY: impl_ptr checked non-null above; points to the QuicTransport
    // allocated by `hew_transport_quic_new`.
    let qt = unsafe { &*impl_ptr.cast::<QuicTransport>() };

    let Ok(guard) = qt.incoming_rx.lock() else {
        return HEW_CONN_INVALID;
    };
    let Some(rx) = guard.as_ref() else {
        return HEW_CONN_INVALID;
    };

    let result = if timeout_ms < 0 {
        rx.recv().ok()
    } else {
        // timeout_ms ≥ 0 (negative branch handled above), safe to convert.
        rx.recv_timeout(Duration::from_millis(
            u64::try_from(timeout_ms).unwrap_or(0),
        ))
        .ok()
    };

    match result {
        Some(qc) => qt.store_conn(qc),
        None => HEW_CONN_INVALID,
    }
}

unsafe extern "C" fn quic_send(
    impl_ptr: *mut c_void,
    conn: c_int,
    data: *const c_void,
    len: usize,
) -> c_int {
    if impl_ptr.is_null() || data.is_null() {
        return -1;
    }
    // SAFETY: impl_ptr checked non-null above; points to the QuicTransport
    // allocated by `hew_transport_quic_new`.
    let qt = unsafe { &*impl_ptr.cast::<QuicTransport>() };
    // SAFETY: data checked non-null above; caller guarantees [data, data+len)
    // is a valid readable region.
    let slice = unsafe { std::slice::from_raw_parts(data.cast::<u8>(), len) };

    let Ok(idx) = usize::try_from(conn) else {
        return -1;
    };
    let Some(slot) = qt.conns.get(idx) else {
        return -1;
    };
    let Ok(mut guard) = slot.lock() else {
        return -1;
    };
    let Some(qc) = guard.as_mut() else {
        return -1;
    };

    if let Err(e) = qc.ensure_stream(&qt.rt) {
        set_last_error(e);
        return -1;
    }

    framed_send_quic(&qt.rt, qc.send.as_mut().unwrap(), slice)
}

unsafe extern "C" fn quic_recv(
    impl_ptr: *mut c_void,
    conn: c_int,
    buf: *mut c_void,
    buf_size: usize,
) -> c_int {
    if impl_ptr.is_null() || buf.is_null() {
        return -1;
    }
    // SAFETY: impl_ptr checked non-null above; points to the QuicTransport
    // allocated by `hew_transport_quic_new`.
    let qt = unsafe { &*impl_ptr.cast::<QuicTransport>() };
    // SAFETY: buf checked non-null above; caller guarantees [buf, buf+buf_size)
    // is a valid writable region.
    let slice = unsafe { std::slice::from_raw_parts_mut(buf.cast::<u8>(), buf_size) };

    let Ok(idx) = usize::try_from(conn) else {
        return -1;
    };
    let Some(slot) = qt.conns.get(idx) else {
        return -1;
    };
    let Ok(mut guard) = slot.lock() else {
        return -1;
    };
    let Some(qc) = guard.as_mut() else {
        return -1;
    };

    if let Err(e) = qc.ensure_stream(&qt.rt) {
        set_last_error(e);
        return -1;
    }

    framed_recv_quic(&qt.rt, qc.recv.as_mut().unwrap(), slice)
}

unsafe extern "C" fn quic_close_conn(impl_ptr: *mut c_void, conn: c_int) {
    if impl_ptr.is_null() {
        return;
    }
    // SAFETY: impl_ptr checked non-null above; points to the QuicTransport
    // allocated by `hew_transport_quic_new`.
    let qt = unsafe { &*impl_ptr.cast::<QuicTransport>() };
    qt.remove_conn(conn);
}

unsafe extern "C" fn quic_destroy(impl_ptr: *mut c_void) {
    if impl_ptr.is_null() {
        return;
    }
    // SAFETY: takes back ownership of the Box<QuicTransport> allocated in
    // `hew_transport_quic_new`; called at most once per transport.
    let mut qt = unsafe { Box::from_raw(impl_ptr.cast::<QuicTransport>()) };
    // Close the endpoint to stop the accept task.
    if let Some(ep) = qt.endpoint.take() {
        ep.close(quinn::VarInt::from_u32(0), b"shutdown");
    }
    // Drop all connections.
    for slot in &qt.conns {
        if let Ok(mut guard) = slot.lock() {
            *guard = None;
        }
    }
    // Drop the incoming channel receiver.
    if let Ok(mut guard) = qt.incoming_rx.lock() {
        *guard = None;
    }
    // Intentionally leak the tokio runtime to avoid "cannot drop runtime
    // in async context" panics from worker threads still winding down.
    // The process is shutting down and the OS will reclaim resources.
    let rt = qt.rt.clone();
    drop(qt);
    std::mem::forget(rt);
}

// ---------------------------------------------------------------------------
// Static vtable & constructor
// ---------------------------------------------------------------------------

static QUIC_OPS: HewTransportOps = HewTransportOps {
    connect: Some(quic_connect),
    listen: Some(quic_listen),
    accept: Some(quic_accept),
    send: Some(quic_send),
    recv: Some(quic_recv),
    close_conn: Some(quic_close_conn),
    destroy: Some(quic_destroy),
};

/// Check whether a transport is QUIC-based.
///
/// When the transport uses QUIC, Noise encryption should be skipped because
/// QUIC provides its own TLS 1.3 encryption.
///
/// # Safety
///
/// `transport` must be a valid, non-null pointer to a [`HewTransport`].
#[no_mangle]
pub unsafe extern "C" fn hew_transport_is_quic(transport: *const HewTransport) -> bool {
    if transport.is_null() {
        return false;
    }
    // SAFETY: transport checked non-null above; caller guarantees a live
    // `HewTransport`.
    let t = unsafe { &*transport };
    std::ptr::eq(t.ops, &raw const QUIC_OPS)
}

/// Create a new QUIC transport.
///
/// Returns a heap-allocated [`HewTransport`] with the QUIC vtable, or null on
/// failure. The caller owns the returned pointer and must eventually call
/// `destroy` via the vtable (or [`hew_transport_quic_free`]).
///
/// # Safety
///
/// The returned pointer must not be used after `destroy` is called.
#[no_mangle]
pub unsafe extern "C" fn hew_transport_quic_new() -> *mut HewTransport {
    let rt = match tokio::runtime::Builder::new_multi_thread()
        .worker_threads(QUIC_WORKER_THREADS)
        .enable_all()
        .build()
    {
        Ok(rt) => Arc::new(rt),
        Err(e) => {
            set_last_error(format!("quic runtime: {e}"));
            return std::ptr::null_mut();
        }
    };

    let qt = Box::new(QuicTransport::new(rt));
    let transport = Box::new(HewTransport {
        ops: &raw const QUIC_OPS,
        r#impl: Box::into_raw(qt).cast::<c_void>(),
    });
    Box::into_raw(transport)
}

/// Free a QUIC transport created by [`hew_transport_quic_new`].
///
/// # Safety
///
/// `transport` must have been returned by [`hew_transport_quic_new`] and must
/// not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_transport_quic_free(transport: *mut HewTransport) {
    if transport.is_null() {
        return;
    }
    // SAFETY: transport checked non-null above; caller guarantees it was
    // returned by `hew_transport_quic_new` and has not been freed.
    let t = unsafe { &*transport };
    // SAFETY: t.ops points to the static QUIC_OPS vtable, always valid.
    if let Some(destroy_fn) = unsafe { (*t.ops).destroy } {
        // SAFETY: t.r#impl is the QuicTransport pointer from the constructor.
        unsafe { destroy_fn(t.r#impl) };
    }
    // SAFETY: reclaims the Box<HewTransport> allocated in `hew_transport_quic_new`.
    let _ = unsafe { Box::from_raw(transport) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    // -- Helper: set up a server+client loopback pair -------------------------

    /// Wrapper for safe cleanup in tests.
    struct TestTransport {
        ptr: *mut HewTransport,
    }

    impl TestTransport {
        fn new() -> Self {
            let ptr = unsafe { hew_transport_quic_new() };
            assert!(!ptr.is_null(), "transport allocation failed");
            Self { ptr }
        }

        fn ops(&self) -> &HewTransportOps {
            unsafe { &*(*self.ptr).ops }
        }

        fn impl_ptr(&self) -> *mut c_void {
            unsafe { (*self.ptr).r#impl }
        }

        fn qt(&self) -> &QuicTransport {
            unsafe { &*self.impl_ptr().cast::<QuicTransport>() }
        }

        fn qt_mut(&self) -> &mut QuicTransport {
            unsafe { &mut *self.impl_ptr().cast::<QuicTransport>() }
        }

        fn listen(&self, addr: &str) -> c_int {
            let c_addr = CString::new(addr).unwrap();
            unsafe { (self.ops().listen.unwrap())(self.impl_ptr(), c_addr.as_ptr()) }
        }

        fn connect(&self, addr: &str) -> c_int {
            let c_addr = CString::new(addr).unwrap();
            unsafe { (self.ops().connect.unwrap())(self.impl_ptr(), c_addr.as_ptr()) }
        }

        fn accept(&self, timeout_ms: c_int) -> c_int {
            unsafe { (self.ops().accept.unwrap())(self.impl_ptr(), timeout_ms) }
        }

        fn send(&self, conn: c_int, data: &[u8]) -> c_int {
            unsafe {
                (self.ops().send.unwrap())(self.impl_ptr(), conn, data.as_ptr().cast(), data.len())
            }
        }

        fn recv_into(&self, conn: c_int, buf: &mut [u8]) -> c_int {
            unsafe {
                (self.ops().recv.unwrap())(
                    self.impl_ptr(),
                    conn,
                    buf.as_mut_ptr().cast(),
                    buf.len(),
                )
            }
        }

        fn close_conn(&self, conn: c_int) {
            unsafe { (self.ops().close_conn.unwrap())(self.impl_ptr(), conn) }
        }

        fn bound_addr(&self) -> std::net::SocketAddr {
            self.qt()
                .endpoint
                .as_ref()
                .expect("no endpoint")
                .local_addr()
                .unwrap()
        }

        fn server_cert_der(&self) -> &[u8] {
            &self.qt().server_cert_der
        }

        /// Configure this transport as a client that trusts the given cert.
        fn configure_client_for(&self, server_cert_der: &[u8]) {
            let qt = self.qt_mut();
            let client_config = build_client_config(server_cert_der).expect("client config");
            let _guard = qt.rt.enter();
            let mut ep = Endpoint::client("0.0.0.0:0".parse().unwrap()).expect("client endpoint");
            ep.set_default_client_config(client_config);
            qt.endpoint = Some(ep);
        }
    }

    impl Drop for TestTransport {
        fn drop(&mut self) {
            unsafe { hew_transport_quic_free(self.ptr) };
        }
    }

    // SAFETY: Test transports are used from a single test thread plus
    // dedicated connect/accept helper threads.
    unsafe impl Send for TestTransport {}
    unsafe impl Sync for TestTransport {}

    /// Set up a connected server+client pair. Returns (server, client,
    /// server_conn_id, client_conn_id).
    ///
    /// Uses a background thread for accept so the blocking connect and accept
    /// can proceed concurrently (QUIC handshake requires both sides).
    fn setup_loopback() -> (TestTransport, TestTransport, c_int, c_int) {
        let server = TestTransport::new();
        assert_eq!(server.listen("127.0.0.1:0"), 0);
        let bound = server.bound_addr();

        let client = TestTransport::new();
        client.configure_client_for(server.server_cert_der());

        // Accept must run concurrently with connect.
        let mut sc = HEW_CONN_INVALID;
        let mut cc = HEW_CONN_INVALID;
        std::thread::scope(|s| {
            s.spawn(|| {
                sc = server.accept(10_000);
            });
            std::thread::sleep(Duration::from_millis(50));
            cc = client.connect(&bound.to_string());
        });

        assert!(cc >= 0, "client connect failed: {cc}");
        assert!(sc >= 0, "server accept failed: {sc}");

        (server, client, sc, cc)
    }

    // -- Cert generation tests ------------------------------------------------

    #[test]
    fn self_signed_cert_generates_valid_creds() {
        let creds = generate_self_signed_creds().expect("cert generation failed");
        assert!(!creds.cert_der.is_empty(), "cert DER should not be empty");
        assert!(
            !creds.cert_chain.is_empty(),
            "cert chain should not be empty"
        );
    }

    #[test]
    fn server_config_builds_from_self_signed() {
        let creds = generate_self_signed_creds().unwrap();
        let config = build_server_config(&creds);
        assert!(
            config.is_ok(),
            "server config should build: {:?}",
            config.err()
        );
    }

    #[test]
    fn client_config_trusts_self_signed() {
        let creds = generate_self_signed_creds().unwrap();
        let config = build_client_config(&creds.cert_der);
        assert!(
            config.is_ok(),
            "client config should build: {:?}",
            config.err()
        );
    }

    #[test]
    fn client_config_rejects_empty_cert() {
        let result = build_client_config(&[]);
        assert!(result.is_err(), "should reject empty cert");
    }

    #[test]
    fn load_pem_creds_returns_none_when_unset() {
        std::env::remove_var("HEW_QUIC_CERT");
        std::env::remove_var("HEW_QUIC_KEY");
        let result = load_pem_creds().unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn load_ca_cert_returns_none_when_unset() {
        std::env::remove_var("HEW_QUIC_CA");
        let result = load_ca_cert().unwrap();
        assert!(result.is_none());
    }

    // -- Transport construction tests -----------------------------------------

    #[test]
    fn transport_new_and_destroy() {
        let _t = TestTransport::new();
    }

    #[test]
    fn destroy_null_is_noop() {
        unsafe { hew_transport_quic_free(std::ptr::null_mut()) };
    }

    // -- Connection slot management tests -------------------------------------

    #[test]
    fn remove_conn_invalid_ids_no_panic() {
        let rt = Arc::new(
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap(),
        );
        let qt = QuicTransport::new(rt);
        qt.remove_conn(-1);
        qt.remove_conn(0);
        qt.remove_conn(MAX_CONNS as c_int);
    }

    // -- VTable null-safety tests ---------------------------------------------

    #[test]
    fn connect_null_impl_returns_invalid() {
        let result = unsafe { quic_connect(std::ptr::null_mut(), std::ptr::null()) };
        assert_eq!(result, HEW_CONN_INVALID);
    }

    #[test]
    fn connect_null_address_returns_invalid() {
        let t = TestTransport::new();
        let result = unsafe { quic_connect(t.impl_ptr(), std::ptr::null()) };
        assert_eq!(result, HEW_CONN_INVALID);
    }

    #[test]
    fn connect_without_endpoint_returns_invalid() {
        let t = TestTransport::new();
        let addr = CString::new("127.0.0.1:9999").unwrap();
        let result = unsafe { quic_connect(t.impl_ptr(), addr.as_ptr()) };
        assert_eq!(result, HEW_CONN_INVALID);
    }

    #[test]
    fn listen_null_impl_returns_error() {
        let result = unsafe { quic_listen(std::ptr::null_mut(), std::ptr::null()) };
        assert_eq!(result, -1);
    }

    #[test]
    fn accept_null_impl_returns_invalid() {
        let result = unsafe { quic_accept(std::ptr::null_mut(), 100) };
        assert_eq!(result, HEW_CONN_INVALID);
    }

    #[test]
    fn accept_without_endpoint_returns_invalid() {
        let t = TestTransport::new();
        let result = unsafe { quic_accept(t.impl_ptr(), 100) };
        assert_eq!(result, HEW_CONN_INVALID);
    }

    #[test]
    fn send_null_impl_returns_error() {
        let result = unsafe { quic_send(std::ptr::null_mut(), 0, std::ptr::null(), 0) };
        assert_eq!(result, -1);
    }

    #[test]
    fn send_null_data_returns_error() {
        let t = TestTransport::new();
        let result = unsafe { quic_send(t.impl_ptr(), 0, std::ptr::null(), 10) };
        assert_eq!(result, -1);
    }

    #[test]
    fn send_invalid_conn_returns_error() {
        let t = TestTransport::new();
        let data = [1u8; 4];
        let result = unsafe { quic_send(t.impl_ptr(), 99, data.as_ptr().cast(), data.len()) };
        assert_eq!(result, -1);
    }

    #[test]
    fn recv_null_impl_returns_error() {
        let result = unsafe { quic_recv(std::ptr::null_mut(), 0, std::ptr::null_mut(), 0) };
        assert_eq!(result, -1);
    }

    #[test]
    fn recv_null_buf_returns_error() {
        let t = TestTransport::new();
        let result = unsafe { quic_recv(t.impl_ptr(), 0, std::ptr::null_mut(), 64) };
        assert_eq!(result, -1);
    }

    #[test]
    fn recv_invalid_conn_returns_error() {
        let t = TestTransport::new();
        let mut buf = [0u8; 64];
        let result = unsafe { quic_recv(t.impl_ptr(), 99, buf.as_mut_ptr().cast(), buf.len()) };
        assert_eq!(result, -1);
    }

    #[test]
    fn close_conn_null_is_noop() {
        unsafe { quic_close_conn(std::ptr::null_mut(), 0) };
    }

    #[test]
    fn close_conn_invalid_id_is_noop() {
        let t = TestTransport::new();
        unsafe { quic_close_conn(t.impl_ptr(), -1) };
        unsafe { quic_close_conn(t.impl_ptr(), 99) };
    }

    #[test]
    fn destroy_null_is_noop_via_vtable() {
        unsafe { quic_destroy(std::ptr::null_mut()) };
    }

    // -- VTable structure test ------------------------------------------------

    #[test]
    fn vtable_has_all_ops() {
        assert!(QUIC_OPS.connect.is_some());
        assert!(QUIC_OPS.listen.is_some());
        assert!(QUIC_OPS.accept.is_some());
        assert!(QUIC_OPS.send.is_some());
        assert!(QUIC_OPS.recv.is_some());
        assert!(QUIC_OPS.close_conn.is_some());
        assert!(QUIC_OPS.destroy.is_some());
    }

    // -- Loopback integration tests -------------------------------------------

    #[test]
    fn loopback_listen_connect_send_recv() {
        let (server, client, sc, cc) = setup_loopback();

        // Send from client → server.
        let msg = b"hello from client";
        assert_eq!(client.send(cc, msg), msg.len() as c_int);

        let mut buf = [0u8; 256];
        let n = server.recv_into(sc, &mut buf);
        assert_eq!(n, msg.len() as c_int);
        assert_eq!(&buf[..msg.len()], msg);

        // Send from server → client.
        let reply = b"reply from server";
        assert_eq!(server.send(sc, reply), reply.len() as c_int);

        let n = client.recv_into(cc, &mut buf);
        assert_eq!(n, reply.len() as c_int);
        assert_eq!(&buf[..reply.len()], reply);

        client.close_conn(cc);
        server.close_conn(sc);
    }

    #[test]
    fn accept_timeout_returns_invalid() {
        let t = TestTransport::new();
        assert_eq!(t.listen("127.0.0.1:0"), 0);

        // No incoming connection — should return invalid after timeout.
        let conn = t.accept(50);
        assert_eq!(conn, HEW_CONN_INVALID);
    }

    #[test]
    fn listen_on_port_zero_binds_ephemeral() {
        let t = TestTransport::new();
        assert_eq!(t.listen("127.0.0.1:0"), 0);
        assert_ne!(t.bound_addr().port(), 0);
    }

    #[test]
    fn multiple_connections_get_distinct_ids() {
        let server = TestTransport::new();
        assert_eq!(server.listen("127.0.0.1:0"), 0);
        let bound = server.bound_addr();

        let client = TestTransport::new();
        client.configure_client_for(server.server_cert_der());

        // Accept two connections concurrently.
        let mut s1 = HEW_CONN_INVALID;
        let mut s2 = HEW_CONN_INVALID;
        let mut c1 = HEW_CONN_INVALID;
        let mut c2 = HEW_CONN_INVALID;
        let addr = bound.to_string();
        std::thread::scope(|s| {
            s.spawn(|| {
                s1 = server.accept(10_000);
                s2 = server.accept(10_000);
            });
            std::thread::sleep(Duration::from_millis(50));
            c1 = client.connect(&addr);
            c2 = client.connect(&addr);
        });

        assert!(c1 >= 0, "first connect failed");
        assert!(c2 >= 0, "second connect failed");
        assert_ne!(c1, c2, "connections should have distinct IDs");

        assert!(s1 >= 0, "first accept failed");
        assert!(s2 >= 0, "second accept failed");
        assert_ne!(s1, s2, "server connections should have distinct IDs");
    }

    #[test]
    fn send_after_close_returns_error() {
        let (server, client, _sc, cc) = setup_loopback();
        client.close_conn(cc);

        let data = b"should fail";
        let result = client.send(cc, data);
        assert_eq!(result, -1, "send after close should fail");

        drop(server);
    }

    #[test]
    fn large_frame_send_recv() {
        let (server, client, sc, cc) = setup_loopback();

        // Send a 1 MiB frame.
        let big_data: Vec<u8> = (0..1_048_576).map(|i| (i % 256) as u8).collect();
        let sent = client.send(cc, &big_data);
        assert_eq!(sent, big_data.len() as c_int);

        let mut recv_buf = vec![0u8; 2 * 1024 * 1024];
        let received = server.recv_into(sc, &mut recv_buf);
        assert_eq!(received, big_data.len() as c_int);
        assert_eq!(&recv_buf[..big_data.len()], &big_data[..]);

        client.close_conn(cc);
        server.close_conn(sc);
    }

    #[test]
    fn empty_frame_send_recv() {
        let (server, client, sc, cc) = setup_loopback();

        assert_eq!(client.send(cc, &[]), 0);

        let mut buf = [0u8; 64];
        let n = server.recv_into(sc, &mut buf);
        assert_eq!(n, 0, "empty frame should return 0 bytes");

        client.close_conn(cc);
        server.close_conn(sc);
    }

    #[test]
    fn multiple_sequential_messages() {
        let (server, client, sc, cc) = setup_loopback();

        for i in 0u8..10 {
            let msg = vec![i; (i as usize + 1) * 100];
            assert_eq!(client.send(cc, &msg), msg.len() as c_int);

            let mut buf = vec![0u8; 2048];
            let n = server.recv_into(sc, &mut buf) as usize;
            assert_eq!(n, msg.len());
            assert_eq!(&buf[..n], &msg[..]);
        }

        client.close_conn(cc);
        server.close_conn(sc);
    }
}
