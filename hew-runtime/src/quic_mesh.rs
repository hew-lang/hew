//! Per-actor-pair QUIC mesh transport for the Hew node mesh.
//!
//! This module replaces the legacy [`quic_transport`] HOL-blocking model
//! (single bidirectional stream per connection) with per-actor-pair stream
//! multiplexing backed by Quinn's native per-stream scheduling.
//!
//! # Architecture
//!
//! ```text
//! QuicMesh::listen(addr, tls)
//!     └─ Mesh (one per node)
//!         ├─ Mesh::connect(peer_addr)
//!         │   └─ PeerConn (one per remote peer)
//!         │       ├─ open_stream(LaneKind)  → Quinn open_bi() per actor-pair
//!         │       ├─ accept_stream()         → Quinn accept_bi()
//!         │       ├─ send_datagram(bytes)    → Quinn unreliable datagram
//!         │       └─ register_datagram_handler(fn) → spawned recv loop
//!         └─ Mesh::accept() → PeerConn
//! ```
//!
//! # HOL-blocker mitigation
//!
//! `quic_transport` opens one `open_bi()` per connection. A slow consumer on
//! that single stream stalls all actors sharing the connection (head-of-line
//! blocking at the QUIC application level, even though QUIC itself eliminates
//! HOL at the TLS/IP level).
//!
//! `quic_mesh` opens a fresh Quinn bidirectional stream for each `(peer,
//! lane_kind)` actor-pair. Streams are cached in a bounded LRU-like structure
//! per [`PeerConn`]. Each stream is independently flow-controlled by Quinn, so
//! a slow consumer on one stream cannot stall siblings.
//!
//! A3 will build the production stream-multiplex scheduler on top of this
//! skeleton. Native-M3 A5 added per-peer connection-cache LRU eviction
//! (`QuicMeshTransport::store_conn`) and bounded the inbound accept queue to
//! `MAX_CONNS` via `sync_channel`; the per-stream cache (`PeerConn`) still uses
//! the bounded `VecDeque` approximation and will be swapped to a proper LRU
//! crate once the stream-mux concurrency model stabilises.
//!
//! # mTLS peer authentication
//!
//! Connections use QUIC/TLS 1.3. Peer certificates are verified against a
//! per-[`Mesh`] SPKI allowlist ([`MeshTls::allowed_peer_spkis`]). The
//! verifier is installed as both [`rustls::server::danger::ClientCertVerifier`]
//! and [`rustls::client::danger::ServerCertVerifier`].
//!
//! The global Noise allowlist (`ACTIVE_ALLOWLIST` in `encryption.rs`) is not
//! bridged here — Noise uses raw X25519 DH keys, which cannot sign X.509
//! certificates. The bridge from Noise identity to X.509 SPKI is owned by A3
//! (cert minting from the existing keypair).
//!
//! NATIVE-TODO(A3): bridge `ACTIVE_ALLOWLIST` Noise public keys → X.509 SPKI.
//! Once A3 lands, `MeshTls::from_noise_allowlist()` can walk the global
//! allowlist and populate `allowed_peer_spkis` from the pinned X.509 certs.
//!
//! # Wire framing
//!
//! Payloads are passed as raw `&[u8]` / [`bytes::Bytes`] so W4 can plug a
//! CBOR codec (ciborium) without changing call signatures. The skeleton makes
//! no framing assumptions; length-prefix or chunk shape is W1's decision.
//!
//! # Transport selector
//!
//! This module exposes a [`crate::transport`] vtable adapter selected by
//! `HEW_TRANSPORT=quic-mesh`. The adapter uses a single control stream for the
//! existing byte-stream connection-manager contract; W4/A5 move actor payloads
//! onto typed per-actor-pair lanes once the CBOR envelope cut-over lands.
//!
//! [`quic_transport`]: crate::quic_transport

use std::collections::VecDeque;
use std::ffi::{c_char, c_int, c_void, CStr};
use std::net::ToSocketAddrs;
use std::sync::Arc;
use std::sync::OnceLock;
use std::time::Duration;

use bytes::Bytes;
use quinn::{Endpoint, RecvStream, SendStream, VarInt};
use rcgen::generate_simple_self_signed;
use rustls_pki_types::{CertificateDer, PrivateKeyDer, PrivatePkcs8KeyDer};
use tokio::runtime::Runtime;
use tokio::sync::Mutex;

use crate::set_last_error;
use crate::transport::{HewTransport, HewTransportOps, HEW_CONN_INVALID};

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

/// Identifies the lane kind for a per-actor-pair stream.
///
/// A thin `u32` wrapper; W1 defines the encoding and value space. A4/W4 map
/// Hew actor-pair identifiers onto lane kinds. The skeleton treats all values
/// as opaque so no commit is required if the encoding changes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LaneKind(pub u32);

/// Identifies an open bidirectional stream on a [`PeerConn`].
///
/// Issued by [`PeerConn::open_stream`] and [`PeerConn::accept_stream`].
/// Opaque to callers; A3/A5 will use Quinn's internal stream ID once the
/// stream-mux layer stabilises.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StreamId(pub u64);

/// A cached open bidirectional stream.
///
/// Fields are populated by A5's stream-mux layer. The skeleton declares the
/// struct so the cache type is well-founded; A5 will add accessor methods.
#[allow(
    dead_code,
    reason = "skeleton struct: fields consumed by A5 stream-mux layer"
)]
struct StreamHandle {
    id: StreamId,
    lane: LaneKind,
    send: SendStream,
    recv: RecvStream,
}

// ---------------------------------------------------------------------------
// TLS configuration
// ---------------------------------------------------------------------------

/// mTLS configuration for a [`QuicMesh`] or [`Mesh`] endpoint.
///
/// `allowed_peer_spkis` is the set of DER-encoded `SubjectPublicKeyInfo` bytes
/// that this node accepts. A connection whose leaf certificate SPKI is not in
/// the set is rejected at the TLS handshake — fail-closed.
///
/// Use [`MeshTls::with_peer_spki`] to add a pinned peer certificate.
#[derive(Clone, Debug)]
pub struct MeshTls {
    /// DER-encoded certificate chain for this node.
    pub cert_chain: Vec<CertificateDer<'static>>,
    /// Raw PKCS8 DER bytes for this node's private key.
    ///
    /// Stored as `Vec<u8>` because `PrivateKeyDer` does not implement `Clone`.
    /// Call `private_key_der()` to obtain a `PrivateKeyDer<'_>` view when
    /// needed by rustls builders.
    pub private_key_pkcs8: Vec<u8>,
    /// Set of DER-encoded SPKI bytes for allowed peers (fail-closed: empty
    /// means no peers are allowed).
    pub allowed_peer_spkis: std::collections::HashSet<Vec<u8>>,
}

impl MeshTls {
    /// Return an owned `PrivateKeyDer<'static>` from the stored PKCS8 bytes.
    ///
    /// Clones the key bytes. The clone is necessary because `PrivateKeyDer`
    /// does not implement `Clone` and rustls builders require `'static`.
    #[must_use]
    pub fn private_key_der(&self) -> PrivateKeyDer<'static> {
        PrivateKeyDer::Pkcs8(PrivatePkcs8KeyDer::from(self.private_key_pkcs8.clone()))
    }
}

impl MeshTls {
    /// Build a [`MeshTls`] from a self-signed rcgen certificate.
    ///
    /// Returns the [`MeshTls`] and the raw SPKI bytes of the leaf certificate
    /// so the peer can add them to its own `allowed_peer_spkis`.
    ///
    /// # Errors
    ///
    /// Returns [`MeshError::Tls`] if rcgen fails to generate the certificate
    /// or if the DER walk to extract the SPKI fails.
    pub fn self_signed(subject_alt_names: Vec<String>) -> Result<(Self, Vec<u8>), MeshError> {
        let certified_key = generate_simple_self_signed(subject_alt_names)
            .map_err(|e| MeshError::Tls(format!("rcgen: {e}")))?;

        let cert_der: CertificateDer<'static> =
            CertificateDer::from(certified_key.cert.der().to_vec());

        // Extract the SPKI from the DER-encoded cert so callers can pin it on
        // the peer side without parsing the full certificate.
        let spki_bytes = extract_spki_from_cert_der(cert_der.as_ref())?;

        let private_key_pkcs8 = certified_key.signing_key.serialize_der();

        Ok((
            MeshTls {
                cert_chain: vec![cert_der],
                private_key_pkcs8,
                allowed_peer_spkis: std::collections::HashSet::new(),
            },
            spki_bytes,
        ))
    }

    /// Allow a peer identified by its SPKI bytes (as returned by
    /// [`MeshTls::self_signed`] or extracted from a certificate).
    #[must_use]
    pub fn with_peer_spki(mut self, spki: Vec<u8>) -> Self {
        self.allowed_peer_spkis.insert(spki);
        self
    }
}

/// Extract the `SubjectPublicKeyInfo` DER bytes from a DER-encoded X.509
/// certificate.
///
/// The SPKI is the `subjectPublicKeyInfo` field, which starts at a fixed
/// offset in a well-formed certificate. We use a minimal ASN.1 walk to avoid
/// pulling in a full X.509 parser as a dependency.
///
/// Format:
/// ```text
/// Certificate ::= SEQUENCE {
///   tbsCertificate  TBSCertificate,
///   ...
/// }
/// TBSCertificate ::= SEQUENCE {
///   version           [0] EXPLICIT INTEGER OPTIONAL,
///   serialNumber      INTEGER,
///   signature         AlgorithmIdentifier,
///   issuer            Name,
///   validity          Validity,
///   subject           Name,
///   subjectPublicKeyInfo SubjectPublicKeyInfo,  ← we want this
///   ...
/// }
/// ```
///
/// This walks the DER without a dependency on an X.509 crate. A3 can
/// replace this with `webpki` or `x509-cert` once that crate is added.
fn extract_spki_from_cert_der(cert_der: &[u8]) -> Result<Vec<u8>, MeshError> {
    // The approach: rustls-webpki's EndEntityCert already validates and can
    // give us the SPKI via the public_key() method — but that requires a trust
    // anchor. Instead we use a minimal DER walk.
    //
    // Strategy: use rcgen to round-trip through rustls-pki-types' DER parser.
    // We rely on the fact that rustls's built-in SPKI extraction is available
    // via the server verifier path.
    //
    // Minimal ASN.1 DER walk:
    // 1. Unwrap outer SEQUENCE (Certificate)
    // 2. Unwrap inner SEQUENCE (TBSCertificate)
    // 3. Skip fields until we hit the subjectPublicKeyInfo SEQUENCE
    //
    // This is intentionally simple and brittle — it works for rcgen-generated
    // certs (PKCS8 / P-256 / P-384) that A2/A3 use. A full X.509 parser
    // belongs in a follow-on lane.
    //
    // NATIVE-TODO(A3): replace with x509-cert crate or webpki SPKI extraction.

    fn read_length(data: &[u8], pos: &mut usize) -> Option<usize> {
        let first = *data.get(*pos)? as usize;
        *pos += 1;
        if first < 0x80 {
            return Some(first);
        }
        let len_bytes = first & 0x7f;
        if len_bytes > 4 || *pos + len_bytes > data.len() {
            return None;
        }
        let mut length = 0usize;
        for _ in 0..len_bytes {
            length = (length << 8) | (*data.get(*pos)? as usize);
            *pos += 1;
        }
        Some(length)
    }

    fn expect_tag(data: &[u8], pos: &mut usize, tag: u8) -> Option<usize> {
        if *data.get(*pos)? != tag {
            return None;
        }
        *pos += 1;
        read_length(data, pos)
    }

    fn skip_element(data: &[u8], pos: &mut usize) -> Option<()> {
        let _tag = *data.get(*pos)?;
        *pos += 1;
        let len = read_length(data, pos)?;
        *pos += len;
        if *pos > data.len() {
            return None;
        }
        Some(())
    }

    const SEQUENCE: u8 = 0x30;
    const CONTEXT_0: u8 = 0xa0; // [0] EXPLICIT

    let mut pos = 0;

    // Certificate SEQUENCE
    let cert_len = expect_tag(cert_der, &mut pos, SEQUENCE)
        .ok_or_else(|| MeshError::Tls("cert DER: expected outer SEQUENCE".into()))?;
    if pos + cert_len > cert_der.len() {
        return Err(MeshError::Tls("cert DER: truncated".into()));
    }

    // TBSCertificate SEQUENCE
    let tbs_start = pos;
    let tbs_len = expect_tag(cert_der, &mut pos, SEQUENCE)
        .ok_or_else(|| MeshError::Tls("cert DER: expected TBSCertificate SEQUENCE".into()))?;
    let tbs_end = pos + tbs_len;
    let _ = tbs_start;

    // version [0] EXPLICIT (optional, present in v2/v3 certs)
    if pos < tbs_end && cert_der[pos] == CONTEXT_0 {
        skip_element(cert_der, &mut pos)
            .ok_or_else(|| MeshError::Tls("cert DER: bad version field".into()))?;
    }

    // serialNumber INTEGER
    skip_element(cert_der, &mut pos)
        .ok_or_else(|| MeshError::Tls("cert DER: bad serialNumber".into()))?;

    // signature AlgorithmIdentifier (SEQUENCE)
    skip_element(cert_der, &mut pos)
        .ok_or_else(|| MeshError::Tls("cert DER: bad signature AlgId".into()))?;

    // issuer Name (SEQUENCE)
    skip_element(cert_der, &mut pos)
        .ok_or_else(|| MeshError::Tls("cert DER: bad issuer".into()))?;

    // validity Validity (SEQUENCE)
    skip_element(cert_der, &mut pos)
        .ok_or_else(|| MeshError::Tls("cert DER: bad validity".into()))?;

    // subject Name (SEQUENCE)
    skip_element(cert_der, &mut pos)
        .ok_or_else(|| MeshError::Tls("cert DER: bad subject".into()))?;

    // subjectPublicKeyInfo — this is what we want
    let spki_start = pos;
    let spki_len = expect_tag(cert_der, &mut pos, SEQUENCE)
        .ok_or_else(|| MeshError::Tls("cert DER: expected subjectPublicKeyInfo SEQUENCE".into()))?;
    let spki_end = pos + spki_len;
    if spki_end > cert_der.len() {
        return Err(MeshError::Tls(
            "cert DER: subjectPublicKeyInfo truncated".into(),
        ));
    }

    Ok(cert_der[spki_start..spki_end].to_vec())
}

// ---------------------------------------------------------------------------
// SPKI-pinning certificate verifiers
// ---------------------------------------------------------------------------

/// A rustls [`ClientCertVerifier`] that checks the peer SPKI against an
/// allowlist. Fail-closed: any cert whose SPKI is not in the set is rejected.
///
/// Installed on the server side to verify connecting clients.
#[derive(Debug)]
struct SpkiClientVerifier {
    /// DER-encoded SPKI bytes that are accepted.
    allowed: std::collections::HashSet<Vec<u8>>,
}

impl rustls::server::danger::ClientCertVerifier for SpkiClientVerifier {
    fn root_hint_subjects(&self) -> &[rustls::DistinguishedName] {
        &[]
    }

    fn verify_client_cert(
        &self,
        end_entity: &CertificateDer<'_>,
        _intermediates: &[CertificateDer<'_>],
        _now: rustls_pki_types::UnixTime,
    ) -> Result<rustls::server::danger::ClientCertVerified, rustls::Error> {
        let spki = extract_spki_from_cert_der(end_entity.as_ref())
            .map_err(|e| rustls::Error::General(format!("SPKI extraction failed: {e}")))?;
        if self.allowed.contains(&spki) {
            Ok(rustls::server::danger::ClientCertVerified::assertion())
        } else {
            Err(rustls::Error::General(
                "peer certificate SPKI not in allowlist (fail-closed)".into(),
            ))
        }
    }

    fn verify_tls12_signature(
        &self,
        _message: &[u8],
        _cert: &CertificateDer<'_>,
        _dss: &rustls::DigitallySignedStruct,
    ) -> Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        Err(rustls::Error::General("TLS 1.2 not supported".into()))
    }

    fn verify_tls13_signature(
        &self,
        message: &[u8],
        cert: &CertificateDer<'_>,
        dss: &rustls::DigitallySignedStruct,
    ) -> Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        rustls::crypto::verify_tls13_signature(
            message,
            cert,
            dss,
            &rustls::crypto::ring::default_provider().signature_verification_algorithms,
        )
    }

    fn supported_verify_schemes(&self) -> Vec<rustls::SignatureScheme> {
        rustls::crypto::ring::default_provider()
            .signature_verification_algorithms
            .supported_schemes()
    }
}

/// A rustls [`ServerCertVerifier`] that checks the server SPKI against an
/// allowlist. Fail-closed: any cert whose SPKI is not pinned is rejected.
///
/// Installed on the client side to verify the server it connects to.
#[derive(Debug)]
struct SpkiServerVerifier {
    /// DER-encoded SPKI bytes that are accepted.
    allowed: std::collections::HashSet<Vec<u8>>,
}

impl rustls::client::danger::ServerCertVerifier for SpkiServerVerifier {
    fn verify_server_cert(
        &self,
        end_entity: &CertificateDer<'_>,
        _intermediates: &[CertificateDer<'_>],
        _server_name: &rustls_pki_types::ServerName<'_>,
        _ocsp_response: &[u8],
        _now: rustls_pki_types::UnixTime,
    ) -> Result<rustls::client::danger::ServerCertVerified, rustls::Error> {
        let spki = extract_spki_from_cert_der(end_entity.as_ref())
            .map_err(|e| rustls::Error::General(format!("SPKI extraction failed: {e}")))?;
        if self.allowed.contains(&spki) {
            Ok(rustls::client::danger::ServerCertVerified::assertion())
        } else {
            Err(rustls::Error::General(
                "server certificate SPKI not in allowlist (fail-closed)".into(),
            ))
        }
    }

    fn verify_tls12_signature(
        &self,
        _message: &[u8],
        _cert: &CertificateDer<'_>,
        _dss: &rustls::DigitallySignedStruct,
    ) -> Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        Err(rustls::Error::General("TLS 1.2 not supported".into()))
    }

    fn verify_tls13_signature(
        &self,
        message: &[u8],
        cert: &CertificateDer<'_>,
        dss: &rustls::DigitallySignedStruct,
    ) -> Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        rustls::crypto::verify_tls13_signature(
            message,
            cert,
            dss,
            &rustls::crypto::ring::default_provider().signature_verification_algorithms,
        )
    }

    fn supported_verify_schemes(&self) -> Vec<rustls::SignatureScheme> {
        rustls::crypto::ring::default_provider()
            .signature_verification_algorithms
            .supported_schemes()
    }
}

// ---------------------------------------------------------------------------
// Error type
// ---------------------------------------------------------------------------

/// Errors returned by the `quic_mesh` module. Fail-closed: every failure
/// path returns a typed error rather than silently returning a fabricated
/// value.
#[derive(Debug)]
pub enum MeshError {
    /// TLS configuration or certificate error.
    Tls(String),
    /// Quinn endpoint or connection error.
    Quic(String),
    /// Stream operation error.
    Stream(String),
    /// Datagram send/receive error.
    Datagram(String),
    /// The connection is closing or already closed.
    Closed(String),
}

impl std::fmt::Display for MeshError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MeshError::Tls(s) => write!(f, "quic_mesh TLS error: {s}"),
            MeshError::Quic(s) => write!(f, "quic_mesh QUIC error: {s}"),
            MeshError::Stream(s) => write!(f, "quic_mesh stream error: {s}"),
            MeshError::Datagram(s) => write!(f, "quic_mesh datagram error: {s}"),
            MeshError::Closed(s) => write!(f, "quic_mesh connection closed: {s}"),
        }
    }
}

impl std::error::Error for MeshError {}

// ---------------------------------------------------------------------------
// HewTransport vtable adapter
// ---------------------------------------------------------------------------

const MAX_CONNS: usize = 64;
const MAX_FRAME_SIZE: usize = 16 * 1024 * 1024;
const QUIC_MESH_WORKER_THREADS: usize = 2;

struct QuicMeshConn {
    peer: PeerConn,
    send: Option<SendStream>,
    recv: Option<RecvStream>,
    is_initiator: bool,
    close_reason: Arc<OnceLock<String>>,
}

impl QuicMeshConn {
    fn new(peer: PeerConn, is_initiator: bool, rt: &Runtime) -> Self {
        let close_reason = Arc::new(OnceLock::new());
        spawn_close_watcher(rt, peer.conn.clone(), Arc::clone(&close_reason));
        Self {
            peer,
            send: None,
            recv: None,
            is_initiator,
            close_reason,
        }
    }

    fn new_in_current_runtime(peer: PeerConn, is_initiator: bool) -> Self {
        let close_reason = Arc::new(OnceLock::new());
        let watcher_conn = peer.conn.clone();
        let watcher_reason = Arc::clone(&close_reason);
        tokio::spawn(async move {
            let err = watcher_conn.closed().await;
            let _ = watcher_reason.set(format!("quic_mesh connection closed: {err}"));
        });
        Self {
            peer,
            send: None,
            recv: None,
            is_initiator,
            close_reason,
        }
    }

    fn ensure_control_stream(&mut self, rt: &Runtime) -> Result<(), String> {
        if self.send.is_some() && self.recv.is_some() {
            return Ok(());
        }

        if self.is_initiator {
            let (send, recv) = rt
                .block_on(async { self.peer.conn.open_bi().await })
                .map_err(|e| format!("control open_bi: {e}"))?;
            self.send = Some(send);
            self.recv = Some(recv);
        } else {
            let (send, recv) = rt
                .block_on(async { self.peer.conn.accept_bi().await })
                .map_err(|e| format!("control accept_bi: {e}"))?;
            self.send = Some(send);
            self.recv = Some(recv);
        }
        Ok(())
    }
}

pub(crate) struct QuicMeshTransport {
    rt: Arc<Runtime>,
    mesh: Option<Mesh>,
    conns: Vec<std::sync::Mutex<Option<QuicMeshConn>>>,
    /// Per-peer connection LRU bookkeeping. Tracks occupancy + access recency
    /// for the fixed-size [`QuicMeshTransport::conns`] slot vector. When the
    /// slot vector is saturated, [`SlotLru::alloc`] selects the
    /// least-recently-used slot for eviction so new inbound/outbound
    /// connections can always be admitted instead of being silently dropped.
    ///
    /// Native-M3 A5: bounded peer-connection cache with LRU eviction.
    slot_lru: std::sync::Mutex<SlotLru>,
    incoming_rx: std::sync::Mutex<Option<std::sync::mpsc::Receiver<QuicMeshConn>>>,
}

impl QuicMeshTransport {
    fn new(rt: Arc<Runtime>) -> Self {
        let conns = (0..MAX_CONNS)
            .map(|_| std::sync::Mutex::new(None))
            .collect();
        Self {
            rt,
            mesh: None,
            conns,
            slot_lru: std::sync::Mutex::new(SlotLru::new(MAX_CONNS)),
            incoming_rx: std::sync::Mutex::new(None),
        }
    }

    /// Install `conn` into a slot. If all slots are occupied, the LRU slot is
    /// evicted (its peer connection is closed with a typed reason) and reused.
    ///
    /// Returns the slot index as a `c_int` handle, or [`HEW_CONN_INVALID`] only
    /// in the (impossible) case that `MAX_CONNS` exceeds `c_int::MAX`.
    fn store_conn(&self, conn: QuicMeshConn) -> c_int {
        let alloc = {
            let mut lru = self
                .slot_lru
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            lru.alloc()
        };
        let Some(idx) = self.conns.get(alloc.idx).map(|_| alloc.idx) else {
            return HEW_CONN_INVALID;
        };
        let mut guard = self.conns[idx]
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if alloc.evicted {
            if let Some(old) = guard.take() {
                old.peer
                    .conn
                    .close(VarInt::from_u32(0), b"quic-mesh conn evicted (LRU)");
                tracing::warn!(
                    slot = idx,
                    "quic_mesh: connection cache full; evicted LRU peer to admit new connection"
                );
            }
        }
        *guard = Some(conn);
        c_int::try_from(idx).unwrap_or(HEW_CONN_INVALID)
    }

    fn remove_conn(&self, id: c_int) {
        let Ok(idx) = usize::try_from(id) else { return };
        if let Some(slot) = self.conns.get(idx) {
            let mut guard = slot
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if let Some(conn) = guard.take() {
                conn.peer
                    .conn
                    .close(VarInt::from_u32(0), b"quic-mesh conn closed");
            }
            self.slot_lru
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .release(idx);
        }
    }

    /// Mark slot `idx` as most-recently-used. Called on send/recv hot path.
    fn touch_slot(&self, idx: usize) {
        self.slot_lru
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .touch(idx);
    }
}

// ---------------------------------------------------------------------------
// SlotLru — per-peer connection cache LRU bookkeeping
// ---------------------------------------------------------------------------

/// Outcome of a [`SlotLru::alloc`] call.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SlotAlloc {
    /// Slot index to install the new value into.
    idx: usize,
    /// `true` if the slot was previously occupied (caller must close/drop the
    /// old value). `false` if the slot was empty.
    evicted: bool,
}

/// Bounded LRU bookkeeping for a fixed-size slot array.
///
/// Tracks per-slot occupancy and access recency. [`SlotLru::alloc`] returns
/// the first empty slot if one exists, else evicts the LRU slot. [`Self::touch`]
/// promotes a slot to most-recently-used. [`Self::release`] marks a slot empty.
///
/// Recency is tracked with a monotonic `u64` counter: each slot stores its
/// last-used sequence number (0 = empty). Eviction picks the slot with the
/// smallest non-zero sequence number. O(N) per operation with N = capacity;
/// for the runtime's `MAX_CONNS = 64` this is trivial.
#[derive(Debug)]
struct SlotLru {
    last_used: Vec<u64>,
    counter: u64,
}

impl SlotLru {
    fn new(capacity: usize) -> Self {
        Self {
            last_used: vec![0; capacity],
            counter: 0,
        }
    }

    /// Allocate a slot. Prefers an empty slot; if none, evicts LRU.
    ///
    /// The returned slot is marked most-recently-used. If `evicted` is `true`,
    /// the caller is responsible for closing/dropping the previous occupant.
    ///
    /// Panics if the LRU is constructed with `capacity == 0`. Production
    /// construction always uses `MAX_CONNS > 0`.
    fn alloc(&mut self) -> SlotAlloc {
        assert!(
            !self.last_used.is_empty(),
            "SlotLru::alloc called on zero-capacity LRU"
        );
        self.counter = self.counter.wrapping_add(1);
        if self.counter == 0 {
            // u64 wrap guard: if we ever overflow back to 0 after ~5e8 years at
            // 1 GHz of allocations, reset all sequence numbers monotonically.
            self.counter = 1;
            for s in &mut self.last_used {
                if *s != 0 {
                    *s = 1;
                }
            }
        }
        if let Some(idx) = self.last_used.iter().position(|&s| s == 0) {
            self.last_used[idx] = self.counter;
            return SlotAlloc {
                idx,
                evicted: false,
            };
        }
        // All occupied: pick the slot with the smallest sequence number.
        let (idx, _) = self
            .last_used
            .iter()
            .enumerate()
            .min_by_key(|&(_, s)| s)
            .expect("non-empty by assertion above");
        self.last_used[idx] = self.counter;
        SlotAlloc { idx, evicted: true }
    }

    /// Promote `idx` to most-recently-used. No-op if `idx` is out of range or
    /// the slot is empty.
    fn touch(&mut self, idx: usize) {
        if let Some(slot) = self.last_used.get_mut(idx) {
            if *slot != 0 {
                self.counter = self.counter.wrapping_add(1);
                if self.counter == 0 {
                    self.counter = 1;
                }
                *slot = self.counter;
            }
        }
    }

    /// Mark `idx` empty. No-op if out of range.
    fn release(&mut self, idx: usize) {
        if let Some(slot) = self.last_used.get_mut(idx) {
            *slot = 0;
        }
    }
}

fn parse_socket_addr(addr_str: &str) -> Result<std::net::SocketAddr, String> {
    match addr_str.parse() {
        Ok(addr) => Ok(addr),
        Err(_) => addr_str
            .to_socket_addrs()
            .map_err(|e| format!("resolve {addr_str}: {e}"))?
            .next()
            .ok_or_else(|| format!("resolve {addr_str}: no addresses")),
    }
}

fn self_tls_allowing_own_spki() -> Result<MeshTls, MeshError> {
    let (mut tls, spki) = MeshTls::self_signed(vec!["hew-mesh.local".into(), "localhost".into()])?;
    tls.allowed_peer_spkis.insert(spki);
    Ok(tls)
}

fn framed_send_mesh(rt: &Runtime, send: &mut SendStream, data: &[u8]) -> c_int {
    if data.len() > MAX_FRAME_SIZE {
        set_last_error("quic_mesh send: frame exceeds MAX_FRAME_SIZE");
        return -1;
    }
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
            set_last_error(format!("quic_mesh send: {e}"));
            -1
        }
    }
}

fn framed_recv_mesh(rt: &Runtime, recv: &mut RecvStream, buf: &mut [u8]) -> c_int {
    let result = rt.block_on(async {
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
            set_last_error(format!("quic_mesh recv: {e}"));
            -1
        }
    }
}

fn spawn_close_watcher(rt: &Runtime, conn: quinn::Connection, close_reason: Arc<OnceLock<String>>) {
    rt.spawn(async move {
        let err = conn.closed().await;
        let _ = close_reason.set(format!("quic_mesh connection closed: {err}"));
    });
}

/// Outcome of dispatching an accepted [`QuicMeshConn`] into the bounded accept
/// queue.
///
/// Native-M3 A4 P1.1 + A5 — fail-closed back-pressure handling for the
/// inbound-accept channel. The accept queue is a `sync_channel(MAX_CONNS)`; if
/// the consumer (application calling `quic_mesh_accept`) has not drained it
/// when the producer (the spawned accept loop) tries to enqueue a new
/// connection, the new connection is closed with a typed reason and the loop
/// continues. This bounds queue memory and ensures inbound back-pressure flows
/// back to peers as a clean connection close, never as a silent drop.
#[derive(Debug, PartialEq, Eq)]
enum AcceptDispatch {
    /// Connection successfully placed into the accept queue.
    Delivered,
    /// Accept queue was at capacity. The dispatched connection has been
    /// closed; caller should log and continue.
    QueueFull,
    /// Receiver side of the accept channel has been dropped. The dispatched
    /// connection has been closed; the accept loop should exit.
    ChannelClosed,
}

/// Attempt to deliver `conn` to the bounded accept queue. On `Full` or
/// `Disconnected`, close the underlying peer connection with a typed reason so
/// peers observe an explicit refusal rather than a hang.
fn dispatch_accept(
    tx: &std::sync::mpsc::SyncSender<QuicMeshConn>,
    conn: QuicMeshConn,
) -> AcceptDispatch {
    match tx.try_send(conn) {
        Ok(()) => AcceptDispatch::Delivered,
        Err(std::sync::mpsc::TrySendError::Full(dropped)) => {
            dropped
                .peer
                .conn
                .close(VarInt::from_u32(1), b"quic-mesh accept queue full");
            AcceptDispatch::QueueFull
        }
        Err(std::sync::mpsc::TrySendError::Disconnected(dropped)) => {
            dropped
                .peer
                .conn
                .close(VarInt::from_u32(0), b"quic-mesh accept channel closed");
            AcceptDispatch::ChannelClosed
        }
    }
}

unsafe extern "C" fn quic_mesh_connect(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
    if impl_ptr.is_null() || address.is_null() {
        return HEW_CONN_INVALID;
    }

    // SAFETY: impl_ptr checked non-null above; it is allocated by hew_transport_quic_mesh_new.
    let qmt = unsafe { &*impl_ptr.cast::<QuicMeshTransport>() };
    let Some(mesh) = &qmt.mesh else {
        set_last_error("quic_mesh connect: no listening mesh endpoint");
        return HEW_CONN_INVALID;
    };
    // SAFETY: address checked non-null above; caller guarantees a valid C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(address) }.to_str() else {
        set_last_error("quic_mesh connect: address is not valid UTF-8");
        return HEW_CONN_INVALID;
    };
    let addr = match parse_socket_addr(addr_str) {
        Ok(addr) => addr,
        Err(e) => {
            set_last_error(format!("quic_mesh connect: {e}"));
            return HEW_CONN_INVALID;
        }
    };

    let result = qmt.rt.block_on(async { mesh.connect(addr).await });
    match result {
        Ok(peer) => qmt.store_conn(QuicMeshConn::new(peer, true, &qmt.rt)),
        Err(e) => {
            set_last_error(format!("quic_mesh connect: {e}"));
            HEW_CONN_INVALID
        }
    }
}

unsafe extern "C" fn quic_mesh_listen(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
    if impl_ptr.is_null() || address.is_null() {
        return -1;
    }

    // SAFETY: impl_ptr checked non-null above; it is allocated by hew_transport_quic_mesh_new.
    let qmt = unsafe { &mut *impl_ptr.cast::<QuicMeshTransport>() };
    // SAFETY: address checked non-null above; caller guarantees a valid C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(address) }.to_str() else {
        set_last_error("quic_mesh listen: address is not valid UTF-8");
        return -1;
    };
    let addr = match parse_socket_addr(addr_str) {
        Ok(addr) => addr,
        Err(e) => {
            set_last_error(format!("quic_mesh listen: {e}"));
            return -1;
        }
    };
    let tls = match self_tls_allowing_own_spki() {
        Ok(tls) => tls,
        Err(e) => {
            set_last_error(format!("quic_mesh listen: {e}"));
            return -1;
        }
    };

    let mesh = {
        let _guard = qmt.rt.enter();
        match QuicMesh::listen(addr, tls) {
            Ok(mesh) => mesh,
            Err(e) => {
                set_last_error(format!("quic_mesh listen: {e}"));
                return -1;
            }
        }
    };

    let (tx, rx) = std::sync::mpsc::sync_channel::<QuicMeshConn>(MAX_CONNS);
    let accept_mesh = mesh.clone_endpoint();
    qmt.rt.spawn(async move {
        loop {
            let Some(result) = accept_mesh.accept().await else {
                break;
            };
            let Ok(peer) = result else {
                continue;
            };
            let conn = QuicMeshConn::new_in_current_runtime(peer, false);
            match dispatch_accept(&tx, conn) {
                AcceptDispatch::Delivered => {}
                AcceptDispatch::QueueFull => {
                    // Native-M3 A4 P1.1 + A5: bounded accept queue. When the
                    // application has not drained accepts and the queue is at
                    // MAX_CONNS, refuse the new connection with a typed
                    // diagnostic rather than growing the queue without bound.
                    tracing::warn!(
                        capacity = MAX_CONNS,
                        "quic_mesh: accept queue full; dropping inbound connection (back-pressure)"
                    );
                    // Connection was closed inside dispatch_accept via the
                    // QuicMeshConn it owned; nothing else to do here.
                }
                AcceptDispatch::ChannelClosed => break,
            }
        }
    });

    qmt.mesh = Some(mesh);
    *qmt.incoming_rx
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(rx);
    0
}

unsafe extern "C" fn quic_mesh_accept(impl_ptr: *mut c_void, timeout_ms: c_int) -> c_int {
    if impl_ptr.is_null() {
        return HEW_CONN_INVALID;
    }
    // SAFETY: impl_ptr checked non-null above; it is allocated by hew_transport_quic_mesh_new.
    let qmt = unsafe { &*impl_ptr.cast::<QuicMeshTransport>() };
    let guard = qmt
        .incoming_rx
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let Some(rx) = guard.as_ref() else {
        return HEW_CONN_INVALID;
    };

    let result = if timeout_ms < 0 {
        rx.recv().ok()
    } else {
        rx.recv_timeout(Duration::from_millis(
            u64::try_from(timeout_ms).unwrap_or(0),
        ))
        .ok()
    };

    match result {
        Some(conn) => qmt.store_conn(conn),
        None => HEW_CONN_INVALID,
    }
}

unsafe extern "C" fn quic_mesh_send(
    impl_ptr: *mut c_void,
    conn: c_int,
    data: *const c_void,
    len: usize,
) -> c_int {
    if impl_ptr.is_null() || data.is_null() {
        return -1;
    }
    // SAFETY: impl_ptr checked non-null above; it is allocated by hew_transport_quic_mesh_new.
    let qmt = unsafe { &*impl_ptr.cast::<QuicMeshTransport>() };
    // SAFETY: data checked non-null above; caller guarantees len readable bytes.
    let slice = unsafe { std::slice::from_raw_parts(data.cast::<u8>(), len) };

    let Ok(idx) = usize::try_from(conn) else {
        return -1;
    };
    let Some(slot) = qmt.conns.get(idx) else {
        return -1;
    };
    let mut guard = slot
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let Some(mesh_conn) = guard.as_mut() else {
        return -1;
    };

    if let Some(reason) = mesh_conn.close_reason.get() {
        set_last_error(reason.clone());
        return -1;
    }
    if let Err(e) = mesh_conn.ensure_control_stream(&qmt.rt) {
        set_last_error(format!("quic_mesh send: {e}"));
        return -1;
    }

    let result = framed_send_mesh(&qmt.rt, mesh_conn.send.as_mut().unwrap(), slice);
    drop(guard);
    qmt.touch_slot(idx);
    result
}

unsafe extern "C" fn quic_mesh_recv(
    impl_ptr: *mut c_void,
    conn: c_int,
    buf: *mut c_void,
    buf_size: usize,
) -> c_int {
    if impl_ptr.is_null() || buf.is_null() {
        return -1;
    }
    // SAFETY: impl_ptr checked non-null above; it is allocated by hew_transport_quic_mesh_new.
    let qmt = unsafe { &*impl_ptr.cast::<QuicMeshTransport>() };
    // SAFETY: buf checked non-null above; caller guarantees buf_size writable bytes.
    let slice = unsafe { std::slice::from_raw_parts_mut(buf.cast::<u8>(), buf_size) };

    let Ok(idx) = usize::try_from(conn) else {
        return -1;
    };
    let Some(slot) = qmt.conns.get(idx) else {
        return -1;
    };
    let mut guard = slot
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let Some(mesh_conn) = guard.as_mut() else {
        return -1;
    };

    if let Some(reason) = mesh_conn.close_reason.get() {
        set_last_error(reason.clone());
        return -1;
    }
    if let Err(e) = mesh_conn.ensure_control_stream(&qmt.rt) {
        set_last_error(format!("quic_mesh recv: {e}"));
        return -1;
    }

    let result = framed_recv_mesh(&qmt.rt, mesh_conn.recv.as_mut().unwrap(), slice);
    drop(guard);
    qmt.touch_slot(idx);
    result
}

unsafe extern "C" fn quic_mesh_close_conn(impl_ptr: *mut c_void, conn: c_int) {
    if impl_ptr.is_null() {
        return;
    }
    // SAFETY: impl_ptr checked non-null above; it is allocated by hew_transport_quic_mesh_new.
    let qmt = unsafe { &*impl_ptr.cast::<QuicMeshTransport>() };
    qmt.remove_conn(conn);
}

unsafe extern "C" fn quic_mesh_destroy(impl_ptr: *mut c_void) {
    if impl_ptr.is_null() {
        return;
    }
    // SAFETY: takes ownership of the Box allocated in hew_transport_quic_mesh_new.
    let mut qmt = unsafe { Box::from_raw(impl_ptr.cast::<QuicMeshTransport>()) };
    if let Some(mesh) = qmt.mesh.take() {
        mesh.close();
    }
    for slot in &qmt.conns {
        let mut guard = slot
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let Some(conn) = guard.take() {
            conn.peer.conn.close(VarInt::from_u32(0), b"shutdown");
        }
    }
    *qmt.incoming_rx
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = None;

    let rt_arc = Arc::clone(&qmt.rt);
    drop(qmt);
    match Arc::try_unwrap(rt_arc) {
        Ok(rt) => rt.shutdown_background(),
        Err(arc) => {
            #[cfg(debug_assertions)]
            eprintln!("[hew-runtime] quic_mesh_destroy: Arc::try_unwrap failed, runtime leaked");
            std::mem::forget(arc);
        }
    }
}

static QUIC_MESH_OPS: HewTransportOps = HewTransportOps {
    connect: Some(quic_mesh_connect),
    listen: Some(quic_mesh_listen),
    accept: Some(quic_mesh_accept),
    send: Some(quic_mesh_send),
    recv: Some(quic_mesh_recv),
    close_conn: Some(quic_mesh_close_conn),
    destroy: Some(quic_mesh_destroy),
};

/// Check whether a transport is the native `quic_mesh` transport.
///
/// # Safety
///
/// `transport` must be a valid, non-null pointer to a [`HewTransport`].
#[no_mangle]
pub unsafe extern "C" fn hew_transport_is_quic_mesh(transport: *const HewTransport) -> bool {
    if transport.is_null() {
        return false;
    }
    // SAFETY: transport checked non-null above; caller guarantees a live HewTransport.
    let t = unsafe { &*transport };
    std::ptr::eq(t.ops, &raw const QUIC_MESH_OPS)
}

/// Create a new native `quic_mesh` transport.
///
/// # Safety
///
/// The returned pointer must not be used after its vtable `destroy` is called.
#[no_mangle]
pub unsafe extern "C" fn hew_transport_quic_mesh_new() -> *mut HewTransport {
    let rt = match tokio::runtime::Builder::new_multi_thread()
        .worker_threads(QUIC_MESH_WORKER_THREADS)
        .enable_all()
        .build()
    {
        Ok(rt) => Arc::new(rt),
        Err(e) => {
            set_last_error(format!("quic_mesh runtime: {e}"));
            return std::ptr::null_mut();
        }
    };

    let qmt = Box::new(QuicMeshTransport::new(rt));
    let transport = Box::new(HewTransport {
        ops: &raw const QUIC_MESH_OPS,
        r#impl: Box::into_raw(qmt).cast::<c_void>(),
    });
    Box::into_raw(transport)
}

/// Free a native `quic_mesh` transport created by [`hew_transport_quic_mesh_new`].
///
/// # Safety
///
/// `transport` must have been returned by [`hew_transport_quic_mesh_new`] and
/// must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_transport_quic_mesh_free(transport: *mut HewTransport) {
    if transport.is_null() {
        return;
    }
    // SAFETY: transport checked non-null above and belongs to this free call.
    let t = unsafe { &*transport };
    // SAFETY: t.ops points to the static QUIC_MESH_OPS vtable.
    if let Some(destroy_fn) = unsafe { (*t.ops).destroy } {
        // SAFETY: t.impl is the QuicMeshTransport pointer from the constructor.
        unsafe { destroy_fn(t.r#impl) };
    }
    // SAFETY: reclaims the Box<HewTransport> allocated by the constructor.
    let _ = unsafe { Box::from_raw(transport) };
}

// ---------------------------------------------------------------------------
// QuicMesh — listener factory
// ---------------------------------------------------------------------------

/// Entry-point for creating a mesh endpoint.
///
/// `QuicMesh` is stateless; it is a factory for [`Mesh`] instances.
#[derive(Debug)]
pub struct QuicMesh;

impl QuicMesh {
    /// Bind a UDP socket, configure mTLS, and return a listening [`Mesh`].
    ///
    /// `addr` is a `SocketAddr`-parseable address (e.g. `"127.0.0.1:0"` for
    /// an ephemeral port). `tls` supplies the node's certificate and the set
    /// of allowed peer SPKIs.
    ///
    /// The current Tokio runtime is used (caller must be inside a Tokio
    /// context). A4 will supply an `Arc<Runtime>` in `MeshConfig` when it
    /// wires the vtable selector; for now `tokio::runtime::Handle::current()`
    /// is sufficient for the skeleton.
    ///
    /// # Errors
    ///
    /// Returns [`MeshError::Tls`] if TLS configuration fails, or
    /// [`MeshError::Quic`] if the UDP socket cannot be bound.
    pub fn listen(addr: std::net::SocketAddr, tls: MeshTls) -> Result<Mesh, MeshError> {
        let (server_config, client_config) = build_tls_configs(&tls)?;
        let mut endpoint =
            Endpoint::server(server_config, addr).map_err(|e| MeshError::Quic(e.to_string()))?;
        endpoint.set_default_client_config(client_config);
        Ok(Mesh {
            endpoint,
            tls: Arc::new(tls),
        })
    }
}

// ---------------------------------------------------------------------------
// TLS config builders
// ---------------------------------------------------------------------------

const HEW_MESH_ALPN: &[u8] = b"hew-mesh/2";
const MESH_IDLE_TIMEOUT_SECS: u64 = 30;

/// Build Quinn server and client configs from a [`MeshTls`].
fn build_tls_configs(
    tls: &MeshTls,
) -> Result<(quinn::ServerConfig, quinn::ClientConfig), MeshError> {
    let provider = Arc::new(rustls::crypto::ring::default_provider());

    // ── Server config (verifies client certs via SPKI allowlist) ────────────

    let client_verifier = Arc::new(SpkiClientVerifier {
        allowed: tls.allowed_peer_spkis.clone(),
    });

    let key_der = tls.private_key_der();
    let signing_key = rustls::crypto::ring::sign::any_supported_type(&key_der)
        .map_err(|e| MeshError::Tls(format!("signing key: {e}")))?;
    let certified_key = rustls::sign::CertifiedKey::new(tls.cert_chain.clone(), signing_key);

    let mut server_rustls = rustls::ServerConfig::builder_with_provider(Arc::clone(&provider))
        .with_safe_default_protocol_versions()
        .map_err(|e| MeshError::Tls(format!("rustls server proto: {e}")))?
        .with_client_cert_verifier(client_verifier)
        .with_cert_resolver(Arc::new(AlwaysResolveCert(Arc::new(certified_key))));

    server_rustls.alpn_protocols = vec![HEW_MESH_ALPN.to_vec()];

    let quinn_server_crypto = quinn::crypto::rustls::QuicServerConfig::try_from(server_rustls)
        .map_err(|e| MeshError::Tls(format!("quinn server crypto: {e}")))?;

    let mut server_config = quinn::ServerConfig::with_crypto(Arc::new(quinn_server_crypto));
    let transport = Arc::get_mut(&mut server_config.transport)
        .ok_or_else(|| MeshError::Tls("could not configure transport".into()))?;
    transport.max_idle_timeout(Some(
        quinn::IdleTimeout::try_from(std::time::Duration::from_secs(MESH_IDLE_TIMEOUT_SECS))
            .unwrap(),
    ));
    // Enable datagrams (required for send_datagram / recv_datagram surface).
    transport.datagram_receive_buffer_size(Some(64 * 1024));

    // ── Client config (verifies server cert via SPKI allowlist) ─────────────

    let server_verifier = Arc::new(SpkiServerVerifier {
        allowed: tls.allowed_peer_spkis.clone(),
    });

    let mut client_rustls = rustls::ClientConfig::builder_with_provider(Arc::clone(&provider))
        .with_safe_default_protocol_versions()
        .map_err(|e| MeshError::Tls(format!("rustls client proto: {e}")))?
        .dangerous()
        .with_custom_certificate_verifier(server_verifier)
        .with_client_auth_cert(tls.cert_chain.clone(), tls.private_key_der())
        .map_err(|e| MeshError::Tls(format!("client auth cert: {e}")))?;

    client_rustls.alpn_protocols = vec![HEW_MESH_ALPN.to_vec()];

    let quinn_client_crypto = quinn::crypto::rustls::QuicClientConfig::try_from(client_rustls)
        .map_err(|e| MeshError::Tls(format!("quinn client crypto: {e}")))?;

    let mut client_transport = quinn::TransportConfig::default();
    client_transport.max_idle_timeout(Some(
        quinn::IdleTimeout::try_from(std::time::Duration::from_secs(MESH_IDLE_TIMEOUT_SECS))
            .unwrap(),
    ));
    client_transport.datagram_receive_buffer_size(Some(64 * 1024));

    let mut client_config = quinn::ClientConfig::new(Arc::new(quinn_client_crypto));
    client_config.transport_config(Arc::new(client_transport));

    Ok((server_config, client_config))
}

/// A rustls [`ResolvesServerCert`] that always resolves to the same key.
struct AlwaysResolveCert(Arc<rustls::sign::CertifiedKey>);

impl rustls::server::ResolvesServerCert for AlwaysResolveCert {
    fn resolve(
        &self,
        _client_hello: rustls::server::ClientHello<'_>,
    ) -> Option<Arc<rustls::sign::CertifiedKey>> {
        Some(Arc::clone(&self.0))
    }
}

impl std::fmt::Debug for AlwaysResolveCert {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AlwaysResolveCert").finish()
    }
}

// ---------------------------------------------------------------------------
// Mesh — per-node endpoint
// ---------------------------------------------------------------------------

/// A bound QUIC mesh endpoint. Accepts inbound connections and initiates
/// outbound connections.
///
/// One `Mesh` per node; a node may participate in multiple meshes if needed
/// (e.g. separate control-plane and data-plane meshes), though the typical
/// deployment is one.
pub struct Mesh {
    endpoint: Endpoint,
    /// TLS config retained for A4 selector wiring (not read yet by this module).
    #[allow(dead_code, reason = "A4 will access tls for vtable registration")]
    tls: Arc<MeshTls>,
}

impl std::fmt::Debug for Mesh {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Mesh")
            .field("local_addr", &self.endpoint.local_addr().ok())
            .finish_non_exhaustive()
    }
}

impl Mesh {
    fn clone_endpoint(&self) -> Self {
        Self {
            endpoint: self.endpoint.clone(),
            tls: Arc::clone(&self.tls),
        }
    }

    /// Local socket address of this endpoint.
    ///
    /// # Errors
    ///
    /// Returns [`MeshError::Quic`] if the endpoint has been closed.
    pub fn local_addr(&self) -> Result<std::net::SocketAddr, MeshError> {
        self.endpoint
            .local_addr()
            .map_err(|e| MeshError::Quic(e.to_string()))
    }

    /// Connect to a remote mesh node. Performs the QUIC+mTLS handshake.
    ///
    /// `peer_addr` is the `SocketAddr` of the peer's listening endpoint.
    /// The SNI used is `"hew-mesh.local"` (internal identifier; peers verify
    /// SPKI, not hostname).
    ///
    /// # Errors
    ///
    /// Returns [`MeshError::Quic`] if the connection or mTLS handshake fails.
    pub async fn connect(&self, peer_addr: std::net::SocketAddr) -> Result<PeerConn, MeshError> {
        let conn = self
            .endpoint
            .connect(peer_addr, "hew-mesh.local")
            .map_err(|e| MeshError::Quic(format!("connect: {e}")))?
            .await
            .map_err(|e| MeshError::Quic(format!("handshake: {e}")))?;

        Ok(PeerConn::new(conn, StreamCap::default()))
    }

    /// Accept the next inbound connection. Returns `None` when the endpoint
    /// has been closed.
    pub async fn accept(&self) -> Option<Result<PeerConn, MeshError>> {
        let incoming = self.endpoint.accept().await?;
        let result = incoming
            .await
            .map(|conn| PeerConn::new(conn, StreamCap::default()))
            .map_err(|e| MeshError::Quic(format!("accept handshake: {e}")));
        Some(result)
    }

    /// Close the endpoint, refusing new inbound connections.
    pub fn close(&self) {
        self.endpoint.close(VarInt::from_u32(0), b"mesh shutdown");
    }
}

// ---------------------------------------------------------------------------
// PeerConn — per-peer connection
// ---------------------------------------------------------------------------

/// Configurable cap on the number of cached open streams per [`PeerConn`].
///
/// When the cache is full and a new stream is needed, the oldest entry is
/// evicted (LRU approximation via `VecDeque`). A5 replaces this with a proper
/// `lru::LruCache` once the concurrency model stabilises.
///
/// NATIVE-TODO(A5): replace bounded `VecDeque` with `lru::LruCache<(ActorId, LaneKind), StreamHandle>`.
/// Current behaviour: cap=16, evict-oldest. A5 adds actor-pair keying and
/// configurable cap per Q150/A159.
#[derive(Debug, Clone, Copy)]
pub struct StreamCap(pub usize);

impl Default for StreamCap {
    fn default() -> Self {
        StreamCap(16)
    }
}

/// Handle to an established QUIC connection to a single remote peer.
///
/// Maintains a bounded cache of open streams (one per actor-pair / lane kind)
/// to avoid the overhead of stream setup for frequent short messages. Each
/// cached stream is independently flow-controlled by Quinn — a slow consumer
/// on stream A does NOT stall stream B.
pub struct PeerConn {
    conn: quinn::Connection,
    /// Bounded stream cache (`VecDeque`-backed LRU approximation).
    /// Protected by a tokio `Mutex` so concurrent `open_stream` calls don't race.
    ///
    /// Key: (`StreamId`, `LaneKind`). Eviction: front of deque (oldest).
    stream_cache: Mutex<VecDeque<StreamHandle>>,
    /// Maximum number of cached streams.
    cap: usize,
    /// Next stream ID (monotonically increasing; not the Quinn stream ID).
    next_stream_id: std::sync::atomic::AtomicU64,
    /// Optional datagram handler task handle. Aborted on `close()`.
    datagram_task: Mutex<Option<tokio::task::JoinHandle<()>>>,
}

impl std::fmt::Debug for PeerConn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PeerConn")
            .field("remote_addr", &self.conn.remote_address())
            .field("cap", &self.cap)
            .finish_non_exhaustive()
    }
}

impl PeerConn {
    fn new(conn: quinn::Connection, cap: StreamCap) -> Self {
        PeerConn {
            conn,
            stream_cache: Mutex::new(VecDeque::with_capacity(cap.0)),
            cap: cap.0,
            next_stream_id: std::sync::atomic::AtomicU64::new(0),
            datagram_task: Mutex::new(None),
        }
    }

    /// Remote socket address of the peer.
    pub fn remote_address(&self) -> std::net::SocketAddr {
        self.conn.remote_address()
    }

    /// Open a new bidirectional stream for the given `lane_kind`.
    ///
    /// Each call opens a fresh Quinn bidirectional stream. Quinn schedules each
    /// stream independently: a slow consumer on one stream cannot stall sends
    /// or receives on sibling streams (no HOL blocking at the application
    /// level).
    ///
    /// The stream is added to the bounded cache. If the cache is full, the
    /// oldest stream is evicted (half-closed and dropped).
    ///
    /// Returns a `(StreamId, SendStream, RecvStream)` tuple. Callers hold the
    /// `SendStream`/`RecvStream` for the duration of the actor exchange; the
    /// [`StreamId`] is a stable identifier for logging and metrics.
    ///
    /// Wire-encoding of payloads is caller-controlled (raw `&[u8]`). A CBOR
    /// codec can be plugged without changing this signature.
    ///
    /// # Errors
    ///
    /// Returns [`MeshError::Stream`] if Quinn cannot open a new stream (e.g.
    /// the connection is closed or the peer's stream limit is reached).
    pub async fn open_stream(
        &self,
        lane_kind: LaneKind,
    ) -> Result<(StreamId, SendStream, RecvStream), MeshError> {
        let (send, recv) = self
            .conn
            .open_bi()
            .await
            .map_err(|e| MeshError::Stream(format!("open_bi: {e}")))?;

        let id = StreamId(
            self.next_stream_id
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        );

        // Cache the stream (evict oldest if at cap).
        {
            let mut cache = self.stream_cache.lock().await;
            if cache.len() >= self.cap {
                // Evict oldest (front). The evicted SendStream / RecvStream are
                // dropped here; Quinn half-closes the stream as a result.
                cache.pop_front();
            }
            // We don't push the stream into the cache here — callers own the
            // SendStream/RecvStream. The cache stores an entry to track the
            // slot so A5 can look up streams by (ActorId, LaneKind).
            // For the skeleton, we push a record with placeholder send/recv
            // and note that A5 will restructure this.
            //
            // NATIVE-TODO(A5): restructure cache to separate metadata
            // (StreamId, LaneKind) from the stream handles, which should live
            // in a per-actor-pair slot. The current skeleton gives callers
            // direct ownership of the handles.
            let _ = (id, lane_kind); // recorded via return value
        }

        Ok((id, send, recv))
    }

    /// Accept an inbound bidirectional stream from the peer.
    ///
    /// Returns `(StreamId, LaneKind, SendStream, RecvStream)`. The
    /// `LaneKind` is read from the first 4 bytes of the stream (big-endian
    /// u32). This convention is provisional and will be formalised by W1's
    /// CDDL schema.
    ///
    /// Coordination note for W1: the lane-kind wire encoding is currently a
    /// 4-byte big-endian u32 prefix written by the initiator. If W1's control
    /// frame schema uses a different encoding (e.g. CBOR map), update this
    /// read and the corresponding write in `open_stream_with_lane_header`.
    ///
    /// # Errors
    ///
    /// Returns [`MeshError::Stream`] if the connection is closed or the
    /// lane-kind header cannot be read.
    pub async fn accept_stream(
        &self,
    ) -> Result<(StreamId, LaneKind, SendStream, RecvStream), MeshError> {
        let (send, mut recv) = self
            .conn
            .accept_bi()
            .await
            .map_err(|e| MeshError::Stream(format!("accept_bi: {e}")))?;

        // Read the 4-byte lane-kind header written by the initiator.
        let mut header = [0u8; 4];
        recv.read_exact(&mut header)
            .await
            .map_err(|e| MeshError::Stream(format!("lane header read: {e}")))?;
        let lane_kind = LaneKind(u32::from_be_bytes(header));

        let id = StreamId(
            self.next_stream_id
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        );

        Ok((id, lane_kind, send, recv))
    }

    /// Open a bidirectional stream and write the lane-kind header before
    /// returning to the caller.
    ///
    /// This is the counterpart to [`accept_stream`]: the acceptor reads the
    /// 4-byte big-endian `lane_kind` before handing back the recv handle.
    ///
    /// Coordination note for W1: same framing convention as `accept_stream`.
    ///
    /// # Errors
    ///
    /// Returns [`MeshError::Stream`] if the stream cannot be opened or the
    /// lane-kind header write fails.
    pub async fn open_stream_with_lane_header(
        &self,
        lane_kind: LaneKind,
    ) -> Result<(StreamId, SendStream, RecvStream), MeshError> {
        let (mut send, recv) = self
            .conn
            .open_bi()
            .await
            .map_err(|e| MeshError::Stream(format!("open_bi: {e}")))?;

        // Write the lane-kind header so the acceptor can dispatch.
        let header = lane_kind.0.to_be_bytes();
        send.write_all(&header)
            .await
            .map_err(|e| MeshError::Stream(format!("lane header write: {e}")))?;

        let id = StreamId(
            self.next_stream_id
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        );

        Ok((id, send, recv))
    }

    /// Send an unreliable datagram to the peer.
    ///
    /// Datagrams are best-effort and unordered. Suitable for SWIM gossip
    /// messages (per A161) and other latency-sensitive fire-and-forget traffic.
    /// Payloads are accepted as raw [`Bytes`] so a CBOR codec can be plugged
    /// without changing this signature.
    ///
    /// # Errors
    ///
    /// Returns [`MeshError::Datagram`] if the datagram is too large or the
    /// connection has been closed.
    pub fn send_datagram(&self, data: Bytes) -> Result<(), MeshError> {
        self.conn
            .send_datagram(data)
            .map_err(|e| MeshError::Datagram(format!("send_datagram: {e}")))
    }

    /// Receive the next inbound datagram from the peer.
    ///
    /// Returns `None` when the connection is closed.
    ///
    /// # Errors
    ///
    /// Returns [`MeshError::Datagram`] if the connection is closed or an
    /// error occurs while reading.
    pub async fn recv_datagram(&self) -> Result<Bytes, MeshError> {
        self.conn
            .read_datagram()
            .await
            .map_err(|e| MeshError::Datagram(format!("read_datagram: {e}")))
    }

    /// Register a typed datagram handler.
    ///
    /// The handler is called for every inbound datagram in a spawned Tokio
    /// task. Only one handler may be registered per [`PeerConn`]; a second
    /// call replaces the first (the previous task is aborted).
    ///
    /// This is the datagram callback surface per A161. C2 (SWIM gossip) will
    /// register the cluster's gossip handler against this surface.
    ///
    /// The handler receives raw [`Bytes`]; W4 plugs CBOR decoding inside the
    /// handler without changing this registration signature.
    pub async fn register_datagram_handler<F>(&self, handler: F)
    where
        F: Fn(Bytes) + Send + Sync + 'static,
    {
        let conn = self.conn.clone();
        let handler = Arc::new(handler);
        let task = tokio::spawn(async move {
            while let Ok(data) = conn.read_datagram().await {
                handler(data);
            }
            // Loop exits when read_datagram returns Err (connection closed).
        });

        let mut guard = self.datagram_task.lock().await;
        if let Some(prev) = guard.replace(task) {
            prev.abort();
        }
    }

    /// Close this connection with a numeric code and reason bytes.
    ///
    /// `code` is a QUIC application error code (0–2^62-1). `reason` is an
    /// optional human-readable string (max ~16 bytes per QUIC spec).
    ///
    /// After `close()` returns, `open_stream`, `accept_stream`, `send_datagram`,
    /// and `recv_datagram` will all return errors.
    pub async fn close(&self, code: u32, reason: &[u8]) {
        // Abort the datagram handler task before closing the connection so the
        // task does not attempt to read from a closed connection.
        let mut guard = self.datagram_task.lock().await;
        if let Some(task) = guard.take() {
            task.abort();
        }
        drop(guard);

        self.conn.close(VarInt::from_u32(code), reason);
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Verify that the ASN.1 DER walk correctly extracts the SPKI from a
    /// rcgen-generated certificate.
    #[test]
    fn spki_extraction_roundtrips() {
        let (tls, spki) = MeshTls::self_signed(vec!["test".into()]).unwrap();
        assert!(!spki.is_empty(), "SPKI must not be empty");
        // The SPKI we extracted during self_signed() must be re-extractable
        // from the certificate.
        let extracted =
            extract_spki_from_cert_der(tls.cert_chain[0].as_ref()).expect("SPKI extraction");
        assert_eq!(
            spki, extracted,
            "SPKI extracted from cert must match the one returned by self_signed()"
        );
    }

    /// Verify that listen + `local_addr` works.
    #[test]
    fn mesh_listen_binds_port() {
        let (tls, _spki) = MeshTls::self_signed(vec!["test".into()]).unwrap();
        // listen is synchronous — Quinn binds the UDP socket immediately
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();
        let _guard = rt.enter();
        let mesh = QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls).expect("listen failed");
        let addr = mesh.local_addr().expect("local_addr");
        assert_ne!(addr.port(), 0, "expected ephemeral port");
    }

    // -----------------------------------------------------------------------
    // SlotLru — peer-connection cache LRU bookkeeping
    // -----------------------------------------------------------------------

    /// Empty LRU returns the first slot and reports `evicted=false`.
    #[test]
    fn slot_lru_empty_alloc_uses_first_empty_slot() {
        let mut lru = SlotLru::new(4);
        let a = lru.alloc();
        assert_eq!(a.idx, 0);
        assert!(!a.evicted);
        let b = lru.alloc();
        assert_eq!(b.idx, 1);
        assert!(!b.evicted);
    }

    /// Filling to capacity uses each slot exactly once, never evicts.
    #[test]
    fn slot_lru_fills_all_slots_without_evicting() {
        let mut lru = SlotLru::new(4);
        for expected in 0..4 {
            let a = lru.alloc();
            assert_eq!(a.idx, expected);
            assert!(!a.evicted, "slot {expected} alloc should not evict");
        }
    }

    /// Once full, the next alloc evicts the oldest-touched slot.
    #[test]
    fn slot_lru_evicts_oldest_when_full() {
        let mut lru = SlotLru::new(3);
        // Fill: alloc order 0, 1, 2 → slot 0 is LRU
        for _ in 0..3 {
            let _ = lru.alloc();
        }
        let evict = lru.alloc();
        assert!(evict.evicted, "must evict when full");
        assert_eq!(evict.idx, 0, "LRU eviction must pick the oldest slot");

        // Next alloc evicts slot 1 (now the LRU since 0 was just touched).
        let evict2 = lru.alloc();
        assert!(evict2.evicted);
        assert_eq!(evict2.idx, 1);
    }

    /// Touching a slot promotes it to MRU, so the previously-second-oldest
    /// becomes the next eviction victim.
    #[test]
    fn slot_lru_touch_promotes_to_mru() {
        let mut lru = SlotLru::new(3);
        for _ in 0..3 {
            let _ = lru.alloc();
        }
        // Touch slot 0 → now slot 1 is LRU.
        lru.touch(0);
        let evict = lru.alloc();
        assert!(evict.evicted);
        assert_eq!(evict.idx, 1, "touched slot must not be evicted next");
    }

    /// Releasing a slot makes it available as an empty (non-evicting) target.
    #[test]
    fn slot_lru_release_frees_slot() {
        let mut lru = SlotLru::new(3);
        for _ in 0..3 {
            let _ = lru.alloc();
        }
        lru.release(1);
        let a = lru.alloc();
        assert!(!a.evicted, "released slot reuse must not be marked evicted");
        assert_eq!(a.idx, 1);
    }

    /// Touch on an empty slot is a no-op (does not promote a phantom entry).
    #[test]
    fn slot_lru_touch_empty_slot_is_noop() {
        let mut lru = SlotLru::new(2);
        lru.touch(0); // empty, no-op
        let a = lru.alloc();
        assert_eq!(a.idx, 0);
        assert!(!a.evicted);
    }

    // -----------------------------------------------------------------------
    // dispatch_accept — bounded accept-queue back-pressure (P1.1)
    // -----------------------------------------------------------------------

    /// Mirror of [`dispatch_accept`] over a generic item type so we can exercise
    /// the Full / Disconnected paths without constructing a real Quinn
    /// [`QuicMeshConn`] (which requires an established QUIC handshake).
    ///
    /// This helper intentionally matches `dispatch_accept`'s control flow so
    /// the tests pin the contract: `try_send` + typed outcome, never a
    /// blocking `send` and never an unbounded fallback.
    fn dispatch_accept_generic<T>(tx: &std::sync::mpsc::SyncSender<T>, item: T) -> AcceptDispatch {
        match tx.try_send(item) {
            Ok(()) => AcceptDispatch::Delivered,
            Err(std::sync::mpsc::TrySendError::Full(_)) => AcceptDispatch::QueueFull,
            Err(std::sync::mpsc::TrySendError::Disconnected(_)) => AcceptDispatch::ChannelClosed,
        }
    }

    /// The accept channel must be `sync_channel(MAX_CONNS)`-backed: filling to
    /// capacity succeeds, the +1th send returns `Full`, and after a drain the
    /// queue accepts again.
    #[test]
    fn accept_queue_is_bounded_and_returns_full_on_saturation() {
        let (tx, rx) = std::sync::mpsc::sync_channel::<u32>(MAX_CONNS);

        // Fill exactly MAX_CONNS items — must all succeed without blocking.
        for i in 0u32..u32::try_from(MAX_CONNS).expect("MAX_CONNS fits in u32") {
            assert_eq!(
                dispatch_accept_generic(&tx, i),
                AcceptDispatch::Delivered,
                "item {i} must be delivered while queue has capacity"
            );
        }

        // The (MAX_CONNS + 1)-th item must be rejected with `Full`, never a
        // silent drop and never an unbounded grow.
        assert_eq!(
            dispatch_accept_generic(&tx, u32::MAX),
            AcceptDispatch::QueueFull,
            "saturation must surface a typed Full result"
        );

        // Drain one item — the queue must accept again, confirming the bound
        // is a soft back-pressure boundary (not a permanent shutdown).
        let _ = rx.recv().expect("queue had items");
        assert_eq!(
            dispatch_accept_generic(&tx, 999),
            AcceptDispatch::Delivered,
            "queue must accept after partial drain"
        );
    }

    /// Closing the receiver must yield a typed `ChannelClosed` outcome (used
    /// by the accept loop to break cleanly instead of looping forever).
    #[test]
    fn accept_dispatch_reports_channel_closed_after_receiver_drop() {
        let (tx, rx) = std::sync::mpsc::sync_channel::<u32>(2);
        drop(rx);
        assert_eq!(
            dispatch_accept_generic(&tx, 1),
            AcceptDispatch::ChannelClosed
        );
    }
}
