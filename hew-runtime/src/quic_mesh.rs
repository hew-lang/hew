//! QUIC mesh transport for the Hew node mesh.
//!
//! Quinn provides independently scheduled streams on each connection.
//!
//! # Architecture
//!
//! ```text
//! QuicMesh::listen(addr, tls)
//!     └─ Mesh (one per node)
//!         ├─ Mesh::connect(peer_addr)
//!         │   └─ PeerConn (one per remote peer)
//!         │       ├─ open_stream(LaneKind)  → Quinn open_bi()
//!         │       ├─ accept_stream()         → Quinn accept_bi()
//!         │       ├─ send_datagram(bytes)    → Quinn unreliable datagram
//!         │       └─ register_datagram_handler(fn) → spawned recv loop
//!         └─ Mesh::accept() → PeerConn
//! ```
//!
//! # Streams
//!
//! `PeerConn` can open independently flow-controlled Quinn streams. Opened
//! stream handles are returned to the caller; the per-stream cache is not
//! populated and its capacity does not currently limit open streams. The
//! transport adapter maintains a separate bounded peer-connection cache.
//!
//! # mTLS peer authentication
//!
//! Connections use QUIC/TLS 1.3. Peer certificates are verified against a
//! per-[`Mesh`] SPKI allowlist ([`MeshTls::allowed_peer_spkis`]). The
//! verifier is installed as both [`rustls::server::danger::ClientCertVerifier`]
//! and [`rustls::client::danger::ServerCertVerifier`].
//!
//! Node listeners install their frozen cluster admission facts through
//! [`hew_quic_mesh_transport_install_auth`]. TLS proves possession of the leaf
//! SPKI; the common checked node handshake then verifies its enrolment under
//! the installed authority before publishing a peer session. Standalone
//! [`MeshTls`] users retain their explicit SPKI allowlist.
//!
//! # Transport selector
//!
//! The [`crate::transport`] adapter is selected by `HEW_TRANSPORT=quic-mesh`.
//! It uses a control stream for the byte-stream connection-manager contract.
//! Payload encoding is handled by the caller.
//!

use std::collections::{HashSet, VecDeque};
use std::ffi::{c_char, c_int, c_void, CStr};
use std::net::ToSocketAddrs;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, OnceLock};
use std::time::Duration;
use zeroize::Zeroizing;

use bytes::Bytes;
use quinn::{Endpoint, RecvStream, SendStream, VarInt};
use rcgen::generate_simple_self_signed;
use rustls_pki_types::{CertificateDer, PrivateKeyDer, PrivatePkcs8KeyDer};
use tokio::runtime::Runtime;
use tokio::sync::{Mutex, Semaphore};

use crate::peer_binding::{MeshIdentityMaterial, PeerAuthSnapshot, RemoteIpClass};
use crate::set_last_error;
use crate::transport::{HewTransport, HewTransportOps, HEW_CONN_INVALID, MAX_FRAME_SIZE};

// The system resolver cannot be cancelled. Its admission belongs to the
// process, so destroying and recreating nodes cannot accumulate stuck lookups.
static ADDRESS_RESOLVER_SLOT: OnceLock<Arc<Semaphore>> = OnceLock::new();

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

/// Identifies the lane kind for a per-actor-pair stream.
///
/// An opaque `u32` value carried in the stream lane header.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LaneKind(pub u32);

/// Identifies an open bidirectional stream on a [`PeerConn`].
///
/// Issued by [`PeerConn::open_stream`] and [`PeerConn::accept_stream`].
/// Opaque to callers and assigned locally by `PeerConn`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StreamId(pub u64);

/// Reserved stream-cache entry. Opened streams are currently caller-owned.
#[allow(dead_code, reason = "reserved stream-cache entry is not populated yet")]
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
#[derive(Clone)]
pub struct MeshTls {
    /// DER-encoded certificate chain for this node.
    pub cert_chain: Vec<CertificateDer<'static>>,
    /// Raw PKCS8 DER bytes for this node's private key.
    ///
    /// Clones share one zeroizing owner. Only the TLS key provider receives
    /// an owned private-key copy at configuration construction.
    pub(crate) private_key_pkcs8: Arc<Zeroizing<Vec<u8>>>,
    /// Set of DER-encoded SPKI bytes for allowed peers (fail-closed: empty
    /// means no peers are allowed).
    pub allowed_peer_spkis: std::collections::HashSet<Vec<u8>>,
}

impl std::fmt::Debug for MeshTls {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MeshTls")
            .field("cert_chain", &self.cert_chain)
            .field("allowed_peer_spkis", &self.allowed_peer_spkis)
            .finish_non_exhaustive()
    }
}

impl MeshTls {
    /// Return an owned `PrivateKeyDer<'static>` from the stored PKCS8 bytes.
    ///
    /// Clones the key bytes. The clone is necessary because `PrivateKeyDer`
    /// does not implement `Clone` and rustls builders require `'static`.
    #[must_use]
    pub fn private_key_der(&self) -> PrivateKeyDer<'static> {
        PrivateKeyDer::Pkcs8(PrivatePkcs8KeyDer::from(
            self.private_key_pkcs8.as_ref().to_vec(),
        ))
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

        let private_key_pkcs8 = Arc::new(Zeroizing::new(certified_key.signing_key.serialize_der()));

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

/// Extract the `SubjectPublicKeyInfo` DER bytes from a parsed X.509 certificate.
pub(crate) fn extract_spki_from_cert_der(cert_der: &[u8]) -> Result<Vec<u8>, MeshError> {
    let certificate = rustls_pki_types::CertificateDer::from(cert_der);
    let parsed = webpki::EndEntityCert::try_from(&certificate)
        .map_err(|error| MeshError::Tls(format!("invalid peer certificate: {error}")))?;
    Ok(parsed.subject_public_key_info().as_ref().to_vec())
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
    /// Verify transport key possession; node enrollment gates publication.
    cluster_admission: bool,
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
        if self.cluster_admission || self.allowed.contains(&spki) {
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
    /// Verify transport key possession; node enrollment gates publication.
    cluster_admission: bool,
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
        if self.cluster_admission || self.allowed.contains(&spki) {
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
    /// The peer connection failed before its authenticated channel opened.
    Connection(quinn::ConnectionError),
    /// Stream operation error.
    Stream(String),
    /// Datagram send/receive error.
    Datagram(String),
    /// The connection is closing or already closed.
    Closed(String),
    /// Control-stream handshake timeout (P1.3). The peer did not open or
    /// accept the control stream within [`MESH_HANDSHAKE_TIMEOUT`].
    HandshakeTimeout(String),
    /// Oversize-frame protocol violation (P1.2). The peer sent a frame
    /// length-prefix that exceeds [`MAX_FRAME_SIZE`]; the stream is closed
    /// rather than draining undefined bytes.
    OversizeFrame(String),
    /// `connect` was called but no mesh endpoint is listening at the target
    /// address; routing is not wired up.
    RoutingNotWired,
    /// The supplied address is not valid UTF-8 (byte sequences from C callers
    /// may be malformed).  The inner string names the operation that failed.
    InvalidAddress(String),
    /// The control stream for the named direction ("send" or "recv") is
    /// absent; the connection never completed its control-stream handshake.
    ControlStreamMissing(String),
}

impl std::fmt::Display for MeshError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MeshError::Tls(s) => write!(f, "quic_mesh TLS error: {s}"),
            MeshError::Quic(s) => write!(f, "quic_mesh QUIC error: {s}"),
            MeshError::Connection(error) => write!(f, "quic_mesh handshake: {error}"),
            MeshError::Stream(s) => write!(f, "quic_mesh stream error: {s}"),
            MeshError::Datagram(s) => write!(f, "quic_mesh datagram error: {s}"),
            MeshError::Closed(s) => write!(f, "quic_mesh connection closed: {s}"),
            MeshError::HandshakeTimeout(s) => {
                write!(f, "quic_mesh handshake timeout: {s}")
            }
            MeshError::OversizeFrame(s) => {
                write!(f, "quic_mesh oversize frame: {s}")
            }
            MeshError::RoutingNotWired => {
                write!(f, "quic_mesh connect: no listening mesh endpoint")
            }
            MeshError::InvalidAddress(op) => {
                write!(f, "quic_mesh {op}: address is not valid UTF-8")
            }
            MeshError::ControlStreamMissing(dir) => {
                write!(f, "quic_mesh {dir}: control {dir} stream missing")
            }
        }
    }
}

impl std::error::Error for MeshError {}

// ---------------------------------------------------------------------------
// HewTransport vtable adapter
// ---------------------------------------------------------------------------

const MAX_CONNS: usize = 64;
const QUIC_MESH_WORKER_THREADS: usize = 2;
/// Control-stream handshake timeout (P1.3). Bounds how long
/// `ensure_control_stream` will wait for the peer's `open_bi`/`accept_bi`
/// before returning a typed [`MeshError::HandshakeTimeout`]. Prevents
/// slow-handshake `DoS` from a peer that completes mTLS but never opens the
/// expected control stream.
const MESH_HANDSHAKE_TIMEOUT: Duration = Duration::from_secs(5);

// ---------------------------------------------------------------------------
// Per-instance mesh peer-auth material (issue #2652, D14)
// ---------------------------------------------------------------------------
//
// The QUIC-mesh identity, peer-SPKI allowlist, and setup-error poison live on
// each `QuicMeshTransport` instance (populated from the owning node's
// `PeerAuthSnapshot` before `listen`), never on process-global statics. Two
// concurrent mesh nodes therefore hold independent identities + allowlists +
// setup state — no `ACTIVE_*` cross-contamination.

/// A loaded mesh identity: TLS config plus its leaf SPKI bytes.
type MeshIdentity = (MeshTls, Vec<u8>);

/// Per-node mesh peer-auth material installed onto a `QuicMeshTransport` before
/// `listen` (D14). An unconfigured transport (no install) has an empty
/// allowlist, no pinned identity (self-signed loopback-dev fallback), and no
/// setup error.
#[derive(Default)]
struct InstalledMeshAuth {
    cluster_admission: bool,
    /// This node's stable mesh identity (from `Node::load_keys`), or `None` for
    /// the self-signed loopback-dev fallback.
    identity: Option<MeshIdentity>,
    /// DER-encoded peer `SubjectPublicKeyInfo` bytes permitted to connect.
    allowed_peer_spkis: HashSet<Vec<u8>>,
    /// Sticky fail-closed poison: a failed `Node::load_keys`
    /// makes `ensure_identity` refuse to mint/reuse a listener identity.
    setup_error: Option<String>,
}

/// Build a listener-ready [`MeshTls`] (+ leaf SPKI) from type-erased snapshot
/// identity material (DER cert chain + PKCS#8 key + precomputed SPKI).
pub(crate) fn mesh_tls_from_material(material: &MeshIdentityMaterial) -> MeshIdentity {
    let cert_chain = material
        .cert_chain_der()
        .iter()
        .map(|der| CertificateDer::from(der.clone()))
        .collect();
    let tls = MeshTls {
        cert_chain,
        private_key_pkcs8: material.shared_private_key(),
        allowed_peer_spkis: HashSet::new(),
    };
    (tls, material.spki().to_vec())
}

/// Project a minted/loaded [`MeshTls`] identity into type-erased snapshot
/// material for the `Node::load_keys` -> `PeerAuthConfig` path (D14).
fn material_from_mesh_tls(tls: &MeshTls, spki: &[u8]) -> MeshIdentityMaterial {
    let cert_chain_der = tls.cert_chain.iter().map(|c| c.as_ref().to_vec()).collect();
    MeshIdentityMaterial::from_shared_der(
        cert_chain_der,
        Arc::clone(&tls.private_key_pkcs8),
        spki.to_vec(),
    )
}

// ---------------------------------------------------------------------------
// Mesh identity keyfile (Node::load_keys)
// ---------------------------------------------------------------------------

/// Magic prefix for the binary keyfile written/read by [`mesh_identity_load_or_create`].
/// A self-describing, dependency-free frame: `MAGIC | cert_len(u32 LE) | cert |
/// key_len(u32 LE) | key`. No PEM/X.509 parser is pulled in — the cert and
/// PKCS8 key are stored as the same DER bytes the mesh already produces.
const MESH_KEYFILE_MAGIC: &[u8] = b"HEWMESHKEY1\0";
/// Maximum accepted single-cert DER size; an rcgen P-256 leaf is well under 1 KiB.
const MAX_KEYFILE_CERT_BYTES: usize = 8192;
/// Maximum accepted PKCS8 key DER size.
const MAX_KEYFILE_KEY_BYTES: usize = 8192;
/// Hard cap on the whole keyfile: `MAGIC | cert_len | cert | key_len | key`.
/// `mesh_identity_load_or_create` refuses to read past this so a giant file
/// can't drive an unbounded allocation before the frame parser runs.
const MAX_KEYFILE_BYTES: usize =
    MESH_KEYFILE_MAGIC.len() + 4 + MAX_KEYFILE_CERT_BYTES + 4 + MAX_KEYFILE_KEY_BYTES;

impl MeshTls {
    /// Serialize this identity to the dependency-free keyfile frame. The mesh
    /// always carries a single leaf cert; serialization rejects any other shape.
    fn to_keyfile_bytes(&self) -> Result<Zeroizing<Vec<u8>>, MeshError> {
        let [cert] = self.cert_chain.as_slice() else {
            return Err(MeshError::Tls(
                "keyfile: expected exactly one leaf certificate".into(),
            ));
        };
        let cert = cert.as_ref();
        if cert.len() > MAX_KEYFILE_CERT_BYTES
            || self.private_key_pkcs8.len() > MAX_KEYFILE_KEY_BYTES
        {
            return Err(MeshError::Tls("keyfile: cert/key exceeds bound".into()));
        }
        let cert_len = u32::try_from(cert.len())
            .map_err(|_| MeshError::Tls("keyfile: cert too large".into()))?;
        let key_len = u32::try_from(self.private_key_pkcs8.len())
            .map_err(|_| MeshError::Tls("keyfile: key too large".into()))?;
        let mut out = Zeroizing::new(Vec::with_capacity(
            MESH_KEYFILE_MAGIC.len() + 8 + cert.len() + self.private_key_pkcs8.len(),
        ));
        out.extend_from_slice(MESH_KEYFILE_MAGIC);
        out.extend_from_slice(&cert_len.to_le_bytes());
        out.extend_from_slice(cert);
        out.extend_from_slice(&key_len.to_le_bytes());
        out.extend_from_slice(&self.private_key_pkcs8);
        Ok(out)
    }

    /// Parse the dependency-free keyfile frame, returning the identity and its
    /// leaf SPKI. Fail-closed on any length/bound/magic violation.
    fn from_keyfile_bytes(bytes: &[u8]) -> Result<(Self, Vec<u8>), MeshError> {
        let mut pos = MESH_KEYFILE_MAGIC.len();
        if !bytes.starts_with(MESH_KEYFILE_MAGIC) {
            return Err(MeshError::Tls("keyfile: bad magic".into()));
        }
        let take_u32 = |bytes: &[u8], pos: &mut usize| -> Result<usize, MeshError> {
            let end = pos.checked_add(4).filter(|e| *e <= bytes.len());
            let end = end.ok_or_else(|| MeshError::Tls("keyfile: truncated length".into()))?;
            let n = u32::from_le_bytes([
                bytes[*pos],
                bytes[*pos + 1],
                bytes[*pos + 2],
                bytes[*pos + 3],
            ]);
            *pos = end;
            Ok(n as usize)
        };
        let take_bytes = |bytes: &[u8], pos: &mut usize, n: usize| -> Result<Vec<u8>, MeshError> {
            let end = pos.checked_add(n).filter(|e| *e <= bytes.len());
            let end = end.ok_or_else(|| MeshError::Tls("keyfile: truncated body".into()))?;
            let v = bytes[*pos..end].to_vec();
            *pos = end;
            Ok(v)
        };
        let cert_len = take_u32(bytes, &mut pos)?;
        if cert_len == 0 || cert_len > MAX_KEYFILE_CERT_BYTES {
            return Err(MeshError::Tls("keyfile: cert length out of range".into()));
        }
        let cert = take_bytes(bytes, &mut pos, cert_len)?;
        let key_len = take_u32(bytes, &mut pos)?;
        if key_len == 0 || key_len > MAX_KEYFILE_KEY_BYTES {
            return Err(MeshError::Tls("keyfile: key length out of range".into()));
        }
        let key = Arc::new(Zeroizing::new(take_bytes(bytes, &mut pos, key_len)?));
        // Fail-closed: the frame is exactly MAGIC|cert|key. Trailing bytes mean
        // a malformed or padded file; reject rather than silently ignore them.
        if pos != bytes.len() {
            return Err(MeshError::Tls("keyfile: trailing bytes after key".into()));
        }
        let spki = extract_spki_from_cert_der(&cert)?;
        let tls = MeshTls {
            cert_chain: vec![CertificateDer::from(cert)],
            private_key_pkcs8: key,
            allowed_peer_spkis: std::collections::HashSet::new(),
        };
        let certified = rustls::sign::CertifiedKey::from_der(
            tls.cert_chain.clone(),
            tls.private_key_der(),
            &rustls::crypto::ring::default_provider(),
        )
        .map_err(|_| MeshError::Tls("keyfile: invalid certificate/private key".into()))?;
        certified
            .keys_match()
            .map_err(|_| MeshError::Tls("keyfile: certificate/private key mismatch".into()))?;
        Ok((tls, spki))
    }
}

/// Load (or, if absent, mint-and-persist) a stable mesh identity from `path`,
/// returning the type-erased [`MeshIdentityMaterial`] for the node to stage into
/// its `PeerAuthConfig` (D14 — no process-global override). This is the runtime
/// behind `Node::load_keys`: peers pin a stable public key only when the local
/// key survives restarts, so a missing file is created with a fresh self-signed
/// identity rather than failing.
///
/// # Errors
///
/// Returns [`MeshError::Tls`] if the keyfile cannot be read/written, if its
/// frame is malformed, or if a fresh identity cannot be minted.
pub fn mesh_identity_load_or_create(
    path: &std::path::Path,
) -> Result<MeshIdentityMaterial, MeshError> {
    let invalid = |error: MeshError| std::io::Error::new(std::io::ErrorKind::InvalidData, error);
    let bytes = crate::identity_file::load_or_create(
        path,
        MAX_KEYFILE_BYTES,
        || {
            let (tls, _) = MeshTls::self_signed(vec!["hew-mesh.local".into(), "localhost".into()])
                .map_err(invalid)?;
            tls.to_keyfile_bytes().map_err(invalid)
        },
        |bytes| {
            MeshTls::from_keyfile_bytes(bytes)
                .map(|_| ())
                .map_err(invalid)
        },
    )
    .map_err(|error| MeshError::Tls(format!("load_keys: {}: {error}", path.display())))?;
    let (tls, spki) = MeshTls::from_keyfile_bytes(&bytes)?;
    Ok(material_from_mesh_tls(&tls, &spki))
}

/// Receive framing belongs to the stream, including a header awaiting capacity.
struct MeshRecv {
    stream: RecvStream,
    pending_length: Option<usize>,
}

impl MeshRecv {
    fn new(stream: RecvStream) -> Self {
        Self {
            stream,
            pending_length: None,
        }
    }
}

struct QuicMeshConn {
    peer: PeerConn,
    send: std::sync::Mutex<Option<SendStream>>,
    recv: std::sync::Mutex<Option<MeshRecv>>,
    is_initiator: bool,
    /// Set once the control stream has been opened (or accepted) and both
    /// halves are stashed in `send`/`recv`. Once true, the per-direction
    /// mutexes hold the streams stably and callers can lock just the
    /// direction they need. Required to avoid deadlock between the reader
    /// loop (holding `recv` across `read_exact`) and a concurrent sender
    /// that would otherwise try to lock `recv` to verify initialisation.
    streams_ready: AtomicBool,
    /// Serialises lazy first-time initialisation of the control stream.
    /// Held only during `open_bi`/`accept_bi`; never while doing I/O on
    /// the streams.
    init_lock: std::sync::Mutex<()>,
    close_reason: Arc<OnceLock<String>>,
}

impl QuicMeshConn {
    fn new(peer: PeerConn, is_initiator: bool, rt: &Runtime) -> Self {
        let close_reason = Arc::new(OnceLock::new());
        spawn_close_watcher(rt, peer.conn.clone(), Arc::clone(&close_reason));
        Self {
            peer,
            send: std::sync::Mutex::new(None),
            recv: std::sync::Mutex::new(None),
            is_initiator,
            streams_ready: AtomicBool::new(false),
            init_lock: std::sync::Mutex::new(()),
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
            send: std::sync::Mutex::new(None),
            recv: std::sync::Mutex::new(None),
            is_initiator,
            streams_ready: AtomicBool::new(false),
            init_lock: std::sync::Mutex::new(()),
            close_reason,
        }
    }

    /// Establish the bidirectional control stream lazily on first use. Both
    /// halves are opened together (Quinn binds them) and then stashed in the
    /// per-direction mutexes. Subsequent calls observe `streams_ready` and
    /// return immediately without touching the `send`/`recv` mutexes — this
    /// is critical so concurrent senders do not deadlock against the reader
    /// loop, which holds the `recv` mutex across a blocking `read_exact`.
    fn ensure_control_stream(&self, rt: &Runtime) -> Result<(), String> {
        if self.streams_ready.load(Ordering::Acquire) {
            return Ok(());
        }
        // Serialise the first-time setup; do NOT touch send/recv mutexes
        // until after the streams are populated.
        let _init_guard = self
            .init_lock
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if self.streams_ready.load(Ordering::Acquire) {
            return Ok(());
        }

        // P1.3: bound the time we wait for the peer's open_bi/accept_bi. Once
        // mTLS completes, a malicious peer could otherwise hang here for the
        // full QUIC idle timeout (30s) on every accept slot.
        let (send, recv) = if self.is_initiator {
            let result = rt.block_on(async {
                tokio::time::timeout(
                    crate::transport::connection_attempt_remaining()
                        .map_or(MESH_HANDSHAKE_TIMEOUT, |remaining| {
                            remaining.min(MESH_HANDSHAKE_TIMEOUT)
                        }),
                    self.peer.conn.open_bi(),
                )
                .await
            });
            let bi = result.map_err(|_| {
                format!(
                    "control open_bi: handshake timeout after {}s",
                    MESH_HANDSHAKE_TIMEOUT.as_secs()
                )
            })?;
            bi.map_err(|e| format!("control open_bi: {e}"))?
        } else {
            let result = rt.block_on(async {
                tokio::time::timeout(
                    crate::transport::connection_attempt_remaining()
                        .map_or(MESH_HANDSHAKE_TIMEOUT, |remaining| {
                            remaining.min(MESH_HANDSHAKE_TIMEOUT)
                        }),
                    self.peer.conn.accept_bi(),
                )
                .await
            });
            let bi = result.map_err(|_| {
                format!(
                    "control accept_bi: handshake timeout after {}s",
                    MESH_HANDSHAKE_TIMEOUT.as_secs()
                )
            })?;
            bi.map_err(|e| format!("control accept_bi: {e}"))?
        };
        *self
            .send
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(send);
        *self
            .recv
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(MeshRecv::new(recv));
        self.streams_ready.store(true, Ordering::Release);
        Ok(())
    }
}

pub(crate) struct QuicMeshTransport {
    rt: Arc<Runtime>,
    /// One owned system lookup may outlive its caller. Keep its admission
    /// until it actually exits, so retries cannot accumulate blocked resolvers.
    resolver_slot: Arc<Semaphore>,
    mesh: Option<Mesh>,
    conns: Vec<std::sync::Mutex<Option<Arc<QuicMeshConn>>>>,
    /// Occupancy for the fixed-size [`QuicMeshTransport::conns`] slot vector.
    /// A live handle is released only by [`Self::remove_conn`].
    slot_table: std::sync::Mutex<SlotTable>,
    incoming_rx: std::sync::Mutex<Option<std::sync::mpsc::Receiver<QuicMeshConn>>>,
    /// Optional TLS override injected by tests or trusted callers before
    /// `listen` is invoked. When `Some`, `quic_mesh_listen` consumes this
    /// value instead of generating a fresh self-signed certificate that
    /// allow-lists only the local node's SPKI. This is the bridge by which
    /// two in-process nodes can mutually pin each other's SPKIs for
    /// cross-node integration tests.
    tls_override: std::sync::Mutex<Option<MeshTls>>,
    /// Cached per-transport TLS identity. Populated lazily on first
    /// [`Self::ensure_identity`] call so callers can fetch the local SPKI via
    /// [`hew_quic_mesh_local_spki`] **before** `listen` mints the rustls
    /// configs. Subsequent `listen` reuses this cached identity so the
    /// caller-published SPKI matches the cert presented on the wire.
    identity: std::sync::Mutex<Option<(MeshTls, Vec<u8>)>>,
    /// Per-node mesh peer-auth material (D14): the stable identity, the peer
    /// SPKI allowlist, and the fail-closed setup-error poison, installed from
    /// the owning node's `PeerAuthSnapshot` by
    /// [`hew_quic_mesh_transport_install_auth`] before `listen`. Replaces the
    /// retired process-global `ACTIVE_MESH_*` statics so two concurrent mesh
    /// nodes cannot cross-contaminate.
    installed_auth: std::sync::Mutex<InstalledMeshAuth>,
}

impl QuicMeshTransport {
    fn new(rt: Arc<Runtime>) -> Self {
        let conns = (0..MAX_CONNS)
            .map(|_| std::sync::Mutex::new(None))
            .collect();
        Self {
            rt,
            resolver_slot: Arc::clone(
                ADDRESS_RESOLVER_SLOT.get_or_init(|| Arc::new(Semaphore::new(1))),
            ),
            mesh: None,
            conns,
            slot_table: std::sync::Mutex::new(SlotTable::new(MAX_CONNS)),
            incoming_rx: std::sync::Mutex::new(None),
            tls_override: std::sync::Mutex::new(None),
            identity: std::sync::Mutex::new(None),
            installed_auth: std::sync::Mutex::new(InstalledMeshAuth::default()),
        }
    }

    /// Install this node's mesh peer-auth material (identity + peer allowlist +
    /// setup-error poison) from its frozen snapshot, before `listen` (D14).
    /// Idempotent replace; call once per node start.
    fn install_auth(&self, auth: InstalledMeshAuth) {
        *self
            .installed_auth
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = auth;
    }

    /// Ensure a TLS identity (cert + SPKI) is minted and cached for this
    /// transport. Returns a clone of the cached `MeshTls` (without an
    /// allowlist applied) and the local SPKI bytes.
    fn ensure_identity(&self) -> Result<(MeshTls, Vec<u8>), MeshError> {
        // Fail-closed: a failed `Node::load_keys` poisons
        // this node's mesh identity. Refuse to mint or reuse any identity so the
        // listener can never bind with an ephemeral self-signed cert (the
        // operator's pinned identity failed to load) or an incomplete allowlist.
        // Read the per-instance poison (D14), never a process-global.
        let installed_identity = {
            let auth = self
                .installed_auth
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if let Some(reason) = &auth.setup_error {
                return Err(MeshError::Tls(format!(
                    "peer-auth setup failed; refusing to mint mesh listener identity (fail-closed): {reason}"
                )));
            }
            auth.identity.clone()
        };
        let mut guard = self
            .identity
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let Some((tls, spki)) = guard.as_ref() {
            return Ok((tls.clone(), spki.clone()));
        }
        // A `Node::load_keys` identity pins a stable on-disk key; adopt it so the
        // SPKI presented on the wire survives restarts. Falls back to a fresh
        // self-signed cert (loopback dev default) when none is installed.
        let (tls, spki) = match installed_identity {
            Some(pair) => pair,
            None => MeshTls::self_signed(vec!["hew-mesh.local".into(), "localhost".into()])?,
        };
        *guard = Some((tls.clone(), spki.clone()));
        Ok((tls, spki))
    }

    /// Build the listen-time TLS config: cached identity + own SPKI
    /// (self-loopback) + this transport's installed peer SPKI allowlist (D14,
    /// per-instance — no process-global).
    fn listen_tls(&self) -> Result<MeshTls, MeshError> {
        let (mut tls, own_spki) = self.ensure_identity()?;
        tls.allowed_peer_spkis.insert(own_spki);
        let auth = self
            .installed_auth
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        for peer_spki in &auth.allowed_peer_spkis {
            tls.allowed_peer_spkis.insert(peer_spki.clone());
        }
        Ok(tls)
    }

    /// Install `conn` into an empty slot.
    ///
    /// Live slots are never recycled. A connection manager retains the numeric
    /// handle until it observes transport closure and removes the connection;
    /// replacing that slot early would let a stale owner send to, or close, an
    /// unrelated newly-admitted peer. Saturation therefore rejects the new
    /// connection fail-closed instead of evicting an established one.
    ///
    /// Returns the slot index as a `c_int` handle, or [`HEW_CONN_INVALID`] when
    /// the bounded table is full.
    fn store_conn(&self, conn: QuicMeshConn) -> c_int {
        let Some(idx) = ({
            let mut slots = self
                .slot_table
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            slots.alloc()
        }) else {
            conn.peer
                .conn
                .close(VarInt::from_u32(0), b"quic-mesh connection table full");
            set_last_error("quic_mesh: connection table full; refusing new connection");
            tracing::warn!(
                capacity = MAX_CONNS,
                "quic_mesh: connection table full; refusing new connection"
            );
            return HEW_CONN_INVALID;
        };
        let Some(slot) = self.conns.get(idx) else {
            return HEW_CONN_INVALID;
        };
        let mut guard = slot
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        debug_assert!(guard.is_none(), "allocated QUIC slot must be empty");
        *guard = Some(Arc::new(conn));
        c_int::try_from(idx).unwrap_or(HEW_CONN_INVALID)
    }

    fn get_conn(&self, id: c_int) -> Option<Arc<QuicMeshConn>> {
        let idx = usize::try_from(id).ok()?;
        let slot = self.conns.get(idx)?;
        let guard = slot
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        guard.as_ref().map(Arc::clone)
    }

    /// Close every stored connection without releasing its slot, so each
    /// thread blocked on one of its streams wakes with an error while the
    /// owner still closes the handle. Each slot's lock orders this sweep
    /// against [`Self::store_conn`]; a connection stored afterwards belongs to
    /// an admission that next observes the manager's shutdown flag.
    fn cancel_all(&self) {
        for slot in &self.conns {
            let guard = slot
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if let Some(conn) = guard.as_ref() {
                conn.peer
                    .conn
                    .close(VarInt::from_u32(0), b"quic-mesh node stopping");
            }
        }
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
                self.slot_table
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .release(idx);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// SlotTable — stable per-peer connection handle bookkeeping
// ---------------------------------------------------------------------------

/// Bounded occupancy bookkeeping for a fixed-size slot array.
///
/// [`SlotTable::alloc`] returns the first empty slot if one exists and returns
/// `None` at capacity. Live slots are never recycled because the connection
/// manager may still hold their numeric handles. [`Self::release`] marks a slot
/// empty. O(N) allocation with the runtime's `MAX_CONNS = 64` is trivial.
#[derive(Debug)]
struct SlotTable {
    occupied: Vec<bool>,
}

impl SlotTable {
    fn new(capacity: usize) -> Self {
        Self {
            occupied: vec![false; capacity],
        }
    }

    /// Allocate an empty slot, returning `None` when every slot is occupied.
    ///
    /// Panics if the table is constructed with `capacity == 0`. Production
    /// construction always uses `MAX_CONNS > 0`.
    fn alloc(&mut self) -> Option<usize> {
        assert!(
            !self.occupied.is_empty(),
            "SlotTable::alloc called on zero-capacity table"
        );
        if let Some(idx) = self.occupied.iter().position(|&occupied| !occupied) {
            self.occupied[idx] = true;
            return Some(idx);
        }
        None
    }

    /// Mark `idx` empty. No-op if out of range.
    fn release(&mut self, idx: usize) {
        if let Some(occupied) = self.occupied.get_mut(idx) {
            *occupied = false;
        }
    }
}

async fn parse_socket_addr(
    addr_str: &str,
    resolver_slot: &Arc<Semaphore>,
) -> Result<std::net::SocketAddr, String> {
    match addr_str.parse() {
        Ok(addr) => Ok(addr),
        Err(_) => {
            resolve_hostname(addr_str.to_owned(), resolver_slot, |address| {
                address
                    .to_socket_addrs()
                    .map_err(|e| format!("resolve {address}: {e}"))?
                    .next()
                    .ok_or_else(|| format!("resolve {address}: no addresses"))
            })
            .await
        }
    }
}

async fn resolve_hostname(
    address: String,
    resolver_slot: &Arc<Semaphore>,
    resolve: impl FnOnce(String) -> Result<std::net::SocketAddr, String> + Send + 'static,
) -> Result<std::net::SocketAddr, String> {
    let permit = Arc::clone(resolver_slot)
        .try_acquire_owned()
        .map_err(|_| "address resolver is busy; retry later".to_owned())?;
    // Dropping this future detaches the blocking job. It owns no transport,
    // endpoint or runtime reference, and cannot publish a late connection.
    // The permit stays with the actual lookup, including while it is queued.
    tokio::task::spawn_blocking(move || {
        let _permit = permit;
        resolve(address)
    })
    .await
    .map_err(|error| format!("address resolver: {error}"))?
}

// `self_tls_allowing_own_spki` was removed in A3; identity is now minted +
// cached per `QuicMeshTransport` via `ensure_identity` and unioned with the
// process-global mesh SPKI allowlist via `listen_tls`. The legacy helper
// could only ever trust its own loopback SPKI, which broke cross-process
// peers (see module docs).

async fn connection_attempt<T>(future: impl std::future::Future<Output = T>) -> Result<T, String> {
    match crate::transport::connection_attempt_remaining() {
        Some(remaining) => tokio::time::timeout(remaining, future)
            .await
            .map_err(|_| "connection attempt deadline".to_owned()),
        None => Ok(future.await),
    }
}

fn framed_send_mesh(rt: &Runtime, send: &mut SendStream, data: &[u8]) -> c_int {
    if data.len() > MAX_FRAME_SIZE {
        set_last_error(
            MeshError::OversizeFrame("send: frame exceeds MAX_FRAME_SIZE".into()).to_string(),
        );
        return -1;
    }
    let frame_len = u32::try_from(data.len()).unwrap_or(u32::MAX);
    let header = frame_len.to_le_bytes();

    let result = rt
        .block_on(connection_attempt(async {
            send.write_all(&header).await?;
            send.write_all(data).await?;
            Ok::<_, quinn::WriteError>(())
        }))
        .and_then(|result| result.map_err(|error| error.to_string()));

    match result {
        Ok(()) => c_int::try_from(data.len()).unwrap_or(-1),
        Err(e) => {
            set_last_error(format!("quic_mesh send: {e}"));
            -1
        }
    }
}

fn framed_recv_mesh(rt: &Runtime, recv: &mut MeshRecv, buf: &mut [u8]) -> c_int {
    let result: Result<c_int, (String, bool)> = rt.block_on(connection_attempt(async {
        let frame_len = if let Some(length) = recv.pending_length {
            length
        } else {
            let mut header = [0u8; 4];
            recv.stream.read_exact(&mut header)
                .await
                .map_err(|e| (format!("header: {e}"), false))?;
            u32::from_le_bytes(header) as usize
        };
        if frame_len > MAX_FRAME_SIZE {
            // P1.2: do NOT attempt to drain `frame_len` undefined bytes off
            // the wire — a malicious peer could have written a small header
            // and no payload, or set frame_len to ~u32::MAX. Stop the recv
            // side with a typed application error code so the peer observes
            // an explicit refusal, then return an OversizeFrame error.
            // The send half remains open so the application can still emit a
            // final diagnostic; the connection-level close is the caller's
            // decision.
            let _ = recv.stream.stop(VarInt::from_u32(2));
            return Err((
                format!(
                    "frame too large: {frame_len} bytes exceeds MAX_FRAME_SIZE {MAX_FRAME_SIZE}; recv stream stopped"
                ),
                true,
            ));
        }
        recv.pending_length = Some(frame_len);
        if frame_len > buf.len() {
            // Retain the checked header without consuming payload or writing
            // into the short buffer. The next call resumes this exact frame.
            return Ok(c_int::try_from(frame_len).unwrap_or(-1));
        }

        recv.stream.read_exact(&mut buf[..frame_len])
            .await
            .map_err(|e| (format!("payload: {e}"), false))?;

        recv.pending_length = None;
        Ok(c_int::try_from(frame_len).unwrap_or(-1))
    })).map_err(|error| (error, false)).and_then(std::convert::identity);

    match result {
        Ok(n) => n,
        Err((e, oversize)) => {
            if oversize {
                set_last_error(format!("quic_mesh recv: oversize frame: {e}"));
            } else {
                set_last_error(format!("quic_mesh recv: {e}"));
            }
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
        set_last_error(MeshError::RoutingNotWired.to_string());
        return HEW_CONN_INVALID;
    };
    // SAFETY: address checked non-null above; caller guarantees a valid C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(address) }.to_str() else {
        set_last_error(MeshError::InvalidAddress("connect".into()).to_string());
        return HEW_CONN_INVALID;
    };
    let result = crate::transport::with_connection_attempt_deadline(|| {
        qmt.rt.block_on(connection_attempt(async {
            let addr = parse_socket_addr(addr_str, &qmt.resolver_slot).await?;
            Ok::<_, String>(mesh.connect(addr).await)
        }))
    })
    .and_then(std::convert::identity);
    let result = match result {
        Ok(result) => result,
        Err(error) => {
            set_last_error(format!("quic_mesh connect: {error}"));
            return HEW_CONN_INVALID;
        }
    };
    match result {
        Ok(peer) => qmt.store_conn(QuicMeshConn::new(peer, true, &qmt.rt)),
        Err(e) => {
            let kind = match &e {
                MeshError::Connection(quinn::ConnectionError::TimedOut) => {
                    crate::stream_error::IO_ERROR_KIND_TIMED_OUT
                }
                MeshError::Connection(_) | MeshError::Tls(_) => {
                    crate::stream_error::IO_ERROR_KIND_PERMISSION_DENIED
                }
                _ => crate::stream_error::IO_ERROR_KIND_UNCLASSIFIED,
            };
            crate::stream_error::set_last_error_with_errno_and_kind(
                format!("quic_mesh connect: {e}"),
                0,
                kind,
            );
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
        set_last_error(MeshError::InvalidAddress("listen".into()).to_string());
        return -1;
    };
    let addr = match crate::transport::with_connection_attempt_deadline(|| {
        qmt.rt.block_on(connection_attempt(parse_socket_addr(
            addr_str,
            &qmt.resolver_slot,
        )))
    })
    .and_then(std::convert::identity)
    {
        Ok(addr) => addr,
        Err(e) => {
            set_last_error(format!("quic_mesh listen: {e}"));
            return -1;
        }
    };
    let tls = {
        let override_tls = qmt
            .tls_override
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take();
        match override_tls {
            Some(tls) => tls,
            None => match qmt.listen_tls() {
                Ok(tls) => tls,
                Err(e) => {
                    set_last_error(format!("quic_mesh listen: {e}"));
                    return -1;
                }
            },
        }
    };

    let mesh = {
        let _guard = qmt.rt.enter();
        let cluster_admission = qmt
            .installed_auth
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .cluster_admission;
        match QuicMesh::listen_mode(addr, tls, cluster_admission) {
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

    let Some(mesh_conn) = qmt.get_conn(conn) else {
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

    let mut send_guard = mesh_conn
        .send
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let Some(send) = send_guard.as_mut() else {
        set_last_error(MeshError::ControlStreamMissing("send".into()).to_string());
        return -1;
    };
    let result = framed_send_mesh(&qmt.rt, send, slice);
    drop(send_guard);
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

    let Some(mesh_conn) = qmt.get_conn(conn) else {
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

    let mut recv_guard = mesh_conn
        .recv
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let Some(recv) = recv_guard.as_mut() else {
        set_last_error(MeshError::ControlStreamMissing("recv".into()).to_string());
        return -1;
    };
    let result = framed_recv_mesh(&qmt.rt, recv, slice);
    drop(recv_guard);
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
    let mut qmt = unsafe { Box::from_raw(impl_ptr.cast::<QuicMeshTransport>()) }; // ALLOCATOR-PAIRING: GlobalAlloc
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

/// The `quic_mesh` arm of [`crate::transport::hew_transport_cancel_io`].
pub(crate) fn cancel_io(transport: &HewTransport) {
    if std::ptr::eq(transport.ops, &raw const QUIC_MESH_OPS) {
        // SAFETY: the ops check guarantees the impl pointer is a
        // QuicMeshTransport, and the caller checked it non-null.
        unsafe { &*transport.r#impl.cast::<QuicMeshTransport>() }.cancel_all();
    }
}

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
        r#impl: Box::into_raw(qmt).cast::<c_void>(), // ALLOCATOR-PAIRING: GlobalAlloc
    });
    Box::into_raw(transport) // ALLOCATOR-PAIRING: GlobalAlloc
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
    let _ = unsafe { Box::from_raw(transport) }; // ALLOCATOR-PAIRING: GlobalAlloc
}

/// Inject a [`MeshTls`] configuration that the next `listen` call will
/// consume in place of the default self-signed-only allowlist.
///
/// This is the bridge by which two in-process nodes (e.g. in integration
/// tests) can mutually pin each other's SPKIs.
///
/// Returns `0` on success, `-1` if the transport pointer is null or is not
/// a `quic_mesh` transport.
///
/// # Safety
///
/// `transport` must be a valid pointer returned by
/// [`hew_transport_quic_mesh_new`] (and not yet freed). `listen` must not
/// have been called yet, or the override will only apply to the *next*
/// listen on this transport.
#[cfg(test)]
pub(crate) unsafe fn hew_transport_quic_mesh_set_tls_override(
    transport: *mut HewTransport,
    tls: MeshTls,
) -> c_int {
    if transport.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees transport is valid for the duration of this call.
    let t = unsafe { &*transport };
    if !std::ptr::eq(t.ops, &raw const QUIC_MESH_OPS) || t.r#impl.is_null() {
        return -1;
    }
    // SAFETY: ops-check above guarantees impl is a QuicMeshTransport.
    let qmt = unsafe { &*t.r#impl.cast::<QuicMeshTransport>() };
    *qmt.tls_override
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(tls);
    0
}

/// Classify a quic-mesh connection's remote endpoint (issue #2652).
///
/// Ops-verifies the transport is quic-mesh (never a raw `impl` cast on a
/// non-mesh transport), resolves the connection to its live `QuicMeshConn`, and
/// classifies `PeerConn::remote_address()`. Any lookup miss ⇒
/// [`RemoteIpClass::Unknown`] (strict, fail-closed).
///
/// # Safety
///
/// `transport` must be null or a valid pointer for the duration of this call.
pub(crate) unsafe fn hew_quic_mesh_transport_conn_remote_ip_class(
    transport: *mut HewTransport,
    conn_id: c_int,
) -> RemoteIpClass {
    if transport.is_null() {
        return RemoteIpClass::Unknown;
    }
    // SAFETY: caller guarantees transport is valid for the duration of this call.
    let t = unsafe { &*transport };
    if !std::ptr::eq(t.ops, &raw const QUIC_MESH_OPS) || t.r#impl.is_null() {
        return RemoteIpClass::Unknown;
    }
    // SAFETY: ops-check above guarantees impl is a QuicMeshTransport.
    let qmt = unsafe { &*t.r#impl.cast::<QuicMeshTransport>() };
    match qmt.get_conn(conn_id) {
        Some(conn) => crate::transport::classify_remote_socket_addr(conn.peer.remote_address()),
        None => RemoteIpClass::Unknown,
    }
}

///
/// # Safety
///
/// `transport` must be a valid pointer returned by
/// [`hew_transport_quic_mesh_new`] (and not yet freed).
pub(crate) unsafe fn hew_transport_quic_mesh_bound_port(
    transport: *mut HewTransport,
) -> Option<u16> {
    if transport.is_null() {
        return None;
    }
    // SAFETY: caller guarantees transport is valid for the duration of this call.
    let t = unsafe { &*transport };
    if !std::ptr::eq(t.ops, &raw const QUIC_MESH_OPS) || t.r#impl.is_null() {
        return None;
    }
    // SAFETY: ops-check above guarantees impl is a QuicMeshTransport.
    let qmt = unsafe { &*t.r#impl.cast::<QuicMeshTransport>() };
    qmt.mesh.as_ref()?.local_addr().ok().map(|a| a.port())
}

/// Extract the peer's leaf-certificate SPKI (DER `SubjectPublicKeyInfo`) for the
/// mesh connection `conn_id` (issue #2652, D6).
///
/// The mTLS handshake has already pinned the peer's SPKI at the transport layer
/// (`allowed_peer_spkis`); admission calls this to recover that authenticated
/// key so it can bind the *claimed* `NodeId` to it — an allowlisted key must not
/// be able to claim a `NodeId` bound to a different key. Returns `None` for a
/// null/non-mesh transport, an unknown conn, or a connection that presented no
/// peer certificate (fail-closed: the caller treats `None` as a strict reject).
///
/// # Safety
///
/// `transport` must be null or a valid pointer for the duration of this call.
pub(crate) unsafe fn hew_transport_quic_mesh_peer_spki(
    transport: *mut HewTransport,
    conn_id: c_int,
) -> Option<Vec<u8>> {
    if transport.is_null() {
        return None;
    }
    // SAFETY: caller guarantees transport is valid for the duration of this call.
    let t = unsafe { &*transport };
    if !std::ptr::eq(t.ops, &raw const QUIC_MESH_OPS) || t.r#impl.is_null() {
        return None;
    }
    // SAFETY: ops-check above guarantees impl is a QuicMeshTransport.
    let qmt = unsafe { &*t.r#impl.cast::<QuicMeshTransport>() };
    let conn = qmt.get_conn(conn_id)?;
    peer_spki(&conn.peer.conn)
}

fn peer_spki(connection: &quinn::Connection) -> Option<Vec<u8>> {
    // quinn 0.11: `peer_identity()` yields `Box<dyn Any>` downcasting to the
    // presented `Vec<CertificateDer<'static>>` (the peer's cert chain).
    let identity = connection.peer_identity()?;
    let certs = identity.downcast::<Vec<CertificateDer<'static>>>().ok()?;
    let leaf = certs.first()?;
    extract_spki_from_cert_der(leaf.as_ref()).ok()
}

// ---------------------------------------------------------------------------
// Per-instance mesh peer-auth install seam (D14) + per-transport local SPKI
// ---------------------------------------------------------------------------

/// Install this node's mesh peer-auth material (identity + peer SPKI allowlist +
/// setup-error poison) onto its `QuicMeshTransport`, sourced from the node's
/// frozen [`PeerAuthSnapshot`], **before** `listen` (issue #2652, D14). Replaces
/// the retired process-global `ACTIVE_MESH_*` statics: two concurrent mesh nodes
/// present + pin independent credentials with no cross-contamination.
///
/// Ops-verifies the transport is quic-mesh (never a raw `impl` cast on a
/// non-mesh transport). A non-mesh / null / null-`impl` transport is a no-op
/// returning `-1`.
///
/// # Safety
///
/// `transport` must be null or a valid pointer for the duration of this call.
pub unsafe fn hew_quic_mesh_transport_install_auth(
    transport: *mut HewTransport,
    snapshot: &PeerAuthSnapshot,
) -> c_int {
    if transport.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees transport is valid for the duration of this call.
    let t = unsafe { &*transport };
    if !std::ptr::eq(t.ops, &raw const QUIC_MESH_OPS) || t.r#impl.is_null() {
        return -1;
    }
    // SAFETY: ops-check above guarantees impl is a QuicMeshTransport.
    let qmt = unsafe { &*t.r#impl.cast::<QuicMeshTransport>() };
    let auth = InstalledMeshAuth {
        cluster_admission: snapshot.admission().is_some(),
        identity: snapshot.mesh_identity().map(mesh_tls_from_material),
        allowed_peer_spkis: snapshot.mesh_spki_allowlist().clone(),
        setup_error: snapshot.setup_error().map(str::to_owned),
    };
    qmt.install_auth(auth);
    0
}

/// Mint (if needed) and copy out the listener's local SPKI bytes.
///
/// Calling this **before** `listen` ensures the same cert is used by
/// subsequent `listen`; callers can publish the returned SPKI to peers
/// to the cluster operator for enrolment
/// before they `listen`.
///
/// On the first call the cert + SPKI are generated and cached on the
/// transport; subsequent calls return the same bytes.
///
/// Calling convention (mirrors POSIX `getsockname`):
/// - Pass `out_buf == NULL` to query the required length: returns required
///   length as a positive value.
/// - Pass a buffer of size `>= required`: returns the number of bytes
///   written.
/// - Pass a buffer that is too small: returns the required length (positive)
///   without writing anything. Caller can detect this by comparing to
///   `out_buf_len`.
///
/// Returns `-1` on null `impl_ptr` or identity-mint failure.
///
/// # Safety
///
/// `impl_ptr` must be a valid `QuicMeshTransport` pointer; `out_buf`, if
/// non-null, must be writable for `out_buf_len` bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_quic_mesh_local_spki(
    transport: *mut HewTransport,
    out_buf: *mut u8,
    out_buf_len: usize,
) -> c_int {
    if transport.is_null() {
        set_last_error("quic_mesh local_spki: null transport");
        return -1;
    }
    // SAFETY: transport checked non-null; caller guarantees it is a live
    // HewTransport returned by hew_transport_quic_mesh_new.
    let t = unsafe { &*transport };
    if !std::ptr::eq(t.ops, &raw const QUIC_MESH_OPS) {
        set_last_error("quic_mesh local_spki: transport is not quic-mesh");
        return -1;
    }
    // SAFETY: t.impl is the QuicMeshTransport pointer from the constructor.
    let qmt = unsafe { &*t.r#impl.cast::<QuicMeshTransport>() };
    let spki = match qmt.ensure_identity() {
        Ok((_tls, spki)) => spki,
        Err(e) => {
            set_last_error(format!("quic_mesh local_spki: {e}"));
            return -1;
        }
    };
    let required = c_int::try_from(spki.len()).unwrap_or(-1);
    if required < 0 {
        set_last_error("quic_mesh local_spki: SPKI exceeds c_int");
        return -1;
    }
    if out_buf.is_null() || out_buf_len < spki.len() {
        return required;
    }
    // SAFETY: out_buf checked non-null and out_buf_len >= spki.len().
    unsafe { std::ptr::copy_nonoverlapping(spki.as_ptr(), out_buf, spki.len()) };
    required
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
    /// The caller must be inside a Tokio runtime context.
    ///
    /// # Errors
    ///
    /// Returns [`MeshError::Tls`] if TLS configuration fails, or
    /// [`MeshError::Quic`] if the UDP socket cannot be bound.
    pub fn listen(addr: std::net::SocketAddr, tls: MeshTls) -> Result<Mesh, MeshError> {
        Self::listen_mode(addr, tls, false)
    }

    fn listen_mode(
        addr: std::net::SocketAddr,
        tls: MeshTls,
        cluster_admission: bool,
    ) -> Result<Mesh, MeshError> {
        let (server_config, client_config) = build_tls_configs(&tls, cluster_admission)?;
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

const HEW_MESH_ALPN: &[u8] = b"hew-mesh/3";
const MESH_IDLE_TIMEOUT_SECS: u64 = 30;

/// Build mutually authenticated TLS configs for direct and relayed carriage.
pub(crate) fn checked_tls_configs(
    tls: &MeshTls,
    cluster_admission: bool,
) -> Result<(rustls::ServerConfig, rustls::ClientConfig), MeshError> {
    let provider = Arc::new(rustls::crypto::ring::default_provider());

    // ── Server config (verifies client certs via SPKI allowlist) ────────────

    let client_verifier = Arc::new(SpkiClientVerifier {
        cluster_admission,
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

    // ── Client config (verifies server cert via SPKI allowlist) ─────────────

    let server_verifier = Arc::new(SpkiServerVerifier {
        cluster_admission,
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

    Ok((server_rustls, client_rustls))
}

/// Build Quinn carriage from the shared authenticated TLS configuration.
fn build_tls_configs(
    tls: &MeshTls,
    cluster_admission: bool,
) -> Result<(quinn::ServerConfig, quinn::ClientConfig), MeshError> {
    let (server_rustls, client_rustls) = checked_tls_configs(tls, cluster_admission)?;
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
    /// Returns [`MeshError::Quic`] on setup failure or [`MeshError::Connection`]
    /// when the peer connection or authenticated handshake fails.
    pub async fn connect(&self, peer_addr: std::net::SocketAddr) -> Result<PeerConn, MeshError> {
        let conn = self
            .endpoint
            .connect(peer_addr, "hew-mesh.local")
            .map_err(|e| MeshError::Quic(format!("connect: {e}")))?
            .await
            .map_err(MeshError::Connection)?;

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

/// Capacity reserved for a future per-peer stream cache.
///
/// Open streams are currently returned to callers and are not inserted into
/// the cache, so this value does not enforce an open-stream limit.
#[derive(Debug, Clone, Copy)]
pub struct StreamCap(pub usize);

impl Default for StreamCap {
    fn default() -> Self {
        StreamCap(16)
    }
}

/// Handle to an established QUIC connection to a single remote peer.
///
/// Opened stream handles belong to the caller; this type does not reuse them.
pub struct PeerConn {
    conn: quinn::Connection,
    /// Reserved for stream tracking; `open_stream` does not populate it.
    stream_cache: Mutex<VecDeque<StreamHandle>>,
    /// Reserved cache capacity.
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

    /// Node identity derived from the peer certificate authenticated by mTLS.
    #[must_use]
    pub fn authenticated_node_id(&self) -> Option<crate::node_identity::NodeId> {
        Some(crate::node_identity::NodeId::from_spki(&peer_spki(
            &self.conn,
        )?))
    }

    /// Open a new bidirectional stream for the given `lane_kind`.
    ///
    /// Each call opens a fresh Quinn bidirectional stream. Quinn schedules each
    /// stream independently: a slow consumer on one stream cannot stall sends
    /// or receives on sibling streams (no HOL blocking at the application
    /// level).
    ///
    /// The caller owns the stream handles. They are not cached or limited by
    /// `StreamCap`.
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

        // No handles are inserted; this only removes an existing cache entry.
        {
            let mut cache = self.stream_cache.lock().await;
            if cache.len() >= self.cap {
                // Evict oldest (front). The evicted SendStream / RecvStream are
                // dropped here; Quinn half-closes the stream as a result.
                cache.pop_front();
            }
            // Callers own the handles; no cache entry is inserted yet.
            let _ = (id, lane_kind); // recorded via return value
        }

        Ok((id, send, recv))
    }

    /// Accept an inbound bidirectional stream from the peer.
    ///
    /// Returns `(StreamId, LaneKind, SendStream, RecvStream)`. The
    /// `LaneKind` is read from the first four bytes of the stream as a
    /// big-endian `u32`, matching `open_stream_with_lane_header`.
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
    /// The handler receives raw [`Bytes`]; decoding belongs to the caller.
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

    #[test]
    fn resolver_deadline_retires_the_dial_without_accumulating_retry_work() {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();
        let slots = Arc::new(Semaphore::new(1));
        let published = AtomicBool::new(false);
        let (started, entered) = tokio::sync::oneshot::channel();
        let (release, blocked) = std::sync::mpsc::channel();
        let mut dial = Box::pin(async {
            let address = resolve_hostname("blocked:443".into(), &slots, move |_| {
                started.send(()).unwrap();
                // Disconnect also releases the producer if an assertion fails.
                let _ = blocked.recv();
                Ok("127.0.0.1:443".parse().unwrap())
            })
            .await?;
            published.store(true, Ordering::Release);
            Ok::<_, String>(address)
        });
        // Start the budget only after observing the actual blocking lookup;
        // worker scheduling latency is not part of this regression's oracle.
        rt.block_on(async {
            tokio::select! {
                entered = entered => entered.expect("lookup entered"),
                result = &mut dial => panic!("lookup completed before release: {result:?}"),
            }
        });
        crate::transport::with_connection_attempt_deadline(|| {
            rt.block_on(async {
                let result = connection_attempt(dial).await;
                assert_eq!(result.unwrap_err(), "connection attempt deadline");
                for _ in 0..8 {
                    let retry = resolve_hostname("retry:443".into(), &slots, |_| {
                        panic!("a retry must not submit another blocked resolver")
                    })
                    .await;
                    assert_eq!(retry.unwrap_err(), "address resolver is busy; retry later");
                }
                for numeric in ["127.0.0.1:443", "[::1]:443"] {
                    assert_eq!(
                        parse_socket_addr(numeric, &slots).await.unwrap(),
                        numeric.parse().unwrap()
                    );
                }
                release.send(()).unwrap();
                // Observe actual producer completion, rather than assuming that
                // a timed-out await released its resolver admission.
                let permit = tokio::time::timeout(Duration::from_secs(10), slots.acquire())
                    .await
                    .expect("resolver finishes after release")
                    .unwrap();
                drop(permit);
                assert!(
                    !published.load(Ordering::Acquire),
                    "no late dial publication"
                );
                assert_eq!(
                    resolve_hostname("retry:443".into(), &slots, |_| {
                        Ok("127.0.0.1:443".parse().unwrap())
                    })
                    .await
                    .unwrap(),
                    "127.0.0.1:443".parse().unwrap()
                );
            });
        });
    }

    #[test]
    fn cancelled_resolver_owns_its_inputs_after_transport_shutdown() {
        // SAFETY: the test owns the transport through the single free below.
        let transport = unsafe { hew_transport_quic_mesh_new() };
        assert!(!transport.is_null());
        // SAFETY: this is the live transport implementation just constructed.
        let qmt = unsafe { &*(*transport).r#impl.cast::<QuicMeshTransport>() };
        let slots = Arc::clone(&qmt.resolver_slot);
        let producer_slots = Arc::clone(&slots);
        let (started, entered) = tokio::sync::oneshot::channel();
        let (release, blocked) = std::sync::mpsc::channel();
        let (finished, completed) = std::sync::mpsc::channel();
        let dial = qmt.rt.spawn(async move {
            resolve_hostname("owned:443".into(), &producer_slots, move |address| {
                started.send(()).unwrap();
                let _ = blocked.recv();
                assert_eq!(address, "owned:443");
                finished.send(()).unwrap();
                Ok("127.0.0.1:443".parse().unwrap())
            })
            .await
        });
        qmt.rt.block_on(entered).unwrap();
        dial.abort();
        assert!(qmt.rt.block_on(dial).unwrap_err().is_cancelled());
        assert!(
            slots.try_acquire().is_err(),
            "the producer still owns admission"
        );
        // SAFETY: the cancelled caller is terminal; only its owned resolver
        // job remains, with no reference into this transport or its runtime.
        unsafe { hew_transport_quic_mesh_free(transport) };
        assert!(matches!(
            completed.try_recv(),
            Err(std::sync::mpsc::TryRecvError::Empty)
        ));
        for _ in 0..3 {
            // SAFETY: each successor is separately owned through its free.
            let successor = unsafe { hew_transport_quic_mesh_new() };
            assert!(!successor.is_null());
            // SAFETY: successor is a live transport of this implementation.
            let implementation = unsafe { (*successor).r#impl };
            let hostname = std::ffi::CString::new("localhost:0").unwrap();
            let numeric = std::ffi::CString::new("127.0.0.1:0").unwrap();
            // SAFETY: implementation and the borrowed addresses remain live.
            unsafe {
                assert_eq!(quic_mesh_listen(implementation, hostname.as_ptr()), -1);
                assert!(CStr::from_ptr(crate::hew_last_error())
                    .to_str()
                    .unwrap()
                    .contains("address resolver is busy"));
                assert_eq!(quic_mesh_listen(implementation, numeric.as_ptr()), 0);
                hew_transport_quic_mesh_free(successor);
            }
        }
        release.send(()).unwrap();
        completed
            .recv_timeout(Duration::from_secs(10))
            .expect("owned producer can finish after transport destruction");
    }

    #[test]
    fn hostname_dial_and_numeric_dial_with_busy_resolver_authenticate() {
        let rt = Arc::new(
            tokio::runtime::Builder::new_multi_thread()
                .worker_threads(2)
                .enable_all()
                .build()
                .unwrap(),
        );
        let mut server = QuicMeshTransport::new(Arc::clone(&rt));
        let mut client = QuicMeshTransport::new(rt);
        // Isolate this handshake test from the deliberately blocked process
        // resolver exercised concurrently by the shutdown/recreation test.
        server.resolver_slot = Arc::new(Semaphore::new(1));
        client.resolver_slot = Arc::clone(&server.resolver_slot);
        let (tls_a, spki_a) = MeshTls::self_signed(vec!["a".into()]).unwrap();
        let (tls_b, spki_b) = MeshTls::self_signed(vec!["b".into()]).unwrap();
        let expected_server = crate::node_identity::NodeId::from_spki(&spki_a);
        let expected_client = crate::node_identity::NodeId::from_spki(&spki_b);
        *server.tls_override.lock().unwrap() = Some(tls_a.with_peer_spki(spki_b));
        *client.tls_override.lock().unwrap() = Some(tls_b.with_peer_spki(spki_a));
        let bind = std::ffi::CString::new("localhost:0").unwrap();
        // SAFETY: both implementations and the borrowed address remain live.
        unsafe {
            assert_eq!(quic_mesh_listen((&raw mut server).cast(), bind.as_ptr()), 0);
            assert_eq!(quic_mesh_listen((&raw mut client).cast(), bind.as_ptr()), 0);
        }
        let address = server.mesh.as_ref().unwrap().local_addr().unwrap();
        for hostname in [true, false, true] {
            let target = if hostname {
                format!("localhost:{}", address.port())
            } else {
                address.to_string()
            };
            let target = std::ffi::CString::new(target).unwrap();
            let busy = (!hostname).then(|| client.resolver_slot.try_acquire().unwrap());
            if busy.is_some() {
                let hostname =
                    std::ffi::CString::new(format!("localhost:{}", address.port())).unwrap();
                assert_eq!(
                    // SAFETY: the implementation and hostname remain live.
                    unsafe {
                        quic_mesh_connect((&raw const client).cast_mut().cast(), hostname.as_ptr())
                    },
                    HEW_CONN_INVALID
                );
            }
            // SAFETY: client and target remain live throughout the dial.
            let connection = unsafe {
                quic_mesh_connect((&raw const client).cast_mut().cast(), target.as_ptr())
            };
            assert_ne!(connection, HEW_CONN_INVALID, "dial {target:?}");
            assert_eq!(
                server
                    .incoming_rx
                    .lock()
                    .unwrap()
                    .as_ref()
                    .unwrap()
                    .recv_timeout(Duration::from_secs(10))
                    .unwrap()
                    .peer
                    .authenticated_node_id(),
                Some(expected_client)
            );
            assert_eq!(
                client.conns[usize::try_from(connection).unwrap()]
                    .lock()
                    .unwrap()
                    .as_ref()
                    .unwrap()
                    .peer
                    .authenticated_node_id(),
                Some(expected_server)
            );
            if hostname {
                let original = client.get_conn(connection).unwrap();
                {
                    let mut slots = client.slot_table.lock().unwrap();
                    while slots.alloc().is_some() {}
                }
                // SAFETY: client and target remain live throughout the dial.
                let rejected = unsafe {
                    quic_mesh_connect((&raw const client).cast_mut().cast(), target.as_ptr())
                };
                assert_eq!(rejected, HEW_CONN_INVALID);
                assert!(Arc::ptr_eq(
                    &original,
                    &client.get_conn(connection).unwrap()
                ));
                // Drain the refused connection and clear test-only reservations.
                server
                    .incoming_rx
                    .lock()
                    .unwrap()
                    .as_ref()
                    .unwrap()
                    .recv_timeout(Duration::from_secs(10))
                    .unwrap();
                let mut slots = client.slot_table.lock().unwrap();
                for idx in 0..MAX_CONNS {
                    if idx != usize::try_from(connection).unwrap() {
                        slots.release(idx);
                    }
                }
            }
            drop(busy);
            client.remove_conn(connection);
        }
    }

    /// Verify that webpki extracts the SPKI from a valid certificate and
    /// rejects malformed certificate bytes.
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
        assert!(extract_spki_from_cert_der(b"not a certificate").is_err());
    }

    /// CAP-12: keyfile frame round-trips, preserving the leaf SPKI.
    #[test]
    fn keyfile_bytes_roundtrip_preserve_spki() {
        let (tls, spki) = MeshTls::self_signed(vec!["cap12".into()]).unwrap();
        let cloned = tls.clone();
        assert!(Arc::ptr_eq(
            &tls.private_key_pkcs8,
            &cloned.private_key_pkcs8
        ));
        assert!(!format!("{tls:?}").contains("private_key_pkcs8"));
        let frame = tls.to_keyfile_bytes().expect("serialize keyfile");
        assert!(frame.starts_with(MESH_KEYFILE_MAGIC));
        let (loaded, loaded_spki) = MeshTls::from_keyfile_bytes(&frame).expect("parse keyfile");
        assert_eq!(
            spki, loaded_spki,
            "leaf SPKI must survive a keyfile round-trip"
        );
        assert_eq!(tls.cert_chain, loaded.cert_chain, "cert must survive");
        assert_eq!(
            tls.private_key_pkcs8.as_slice(),
            loaded.private_key_pkcs8.as_slice(),
            "key must survive"
        );
    }

    /// CAP-12: corrupt frames are rejected fail-closed (no fabricated identity).
    #[test]
    fn keyfile_rejects_corrupt_frames() {
        assert!(
            MeshTls::from_keyfile_bytes(b"not-a-keyfile").is_err(),
            "bad magic"
        );
        let (tls, _) = MeshTls::self_signed(vec!["cap12".into()]).unwrap();
        let (other, _) = MeshTls::self_signed(vec!["different".into()]).unwrap();
        let mut mismatched = tls.clone();
        mismatched.private_key_pkcs8 = other.private_key_pkcs8;
        assert!(MeshTls::from_keyfile_bytes(&mismatched.to_keyfile_bytes().unwrap()).is_err());
        let mut frame = tls.to_keyfile_bytes().unwrap();
        let truncated_len = frame.len() - 4;
        frame.truncate(truncated_len);
        assert!(
            MeshTls::from_keyfile_bytes(&frame).is_err(),
            "truncated body"
        );
        // Trailing bytes after a complete frame are rejected, not ignored.
        let mut padded = tls.to_keyfile_bytes().unwrap();
        padded.push(0x00);
        assert!(
            MeshTls::from_keyfile_bytes(&padded).is_err(),
            "trailing bytes after key must be rejected"
        );
    }

    /// CAP-12: a keyfile larger than the hard cap is rejected before parse,
    /// bounding allocation. Fail-closed, no fabricated identity.
    #[test]
    fn load_keys_rejects_oversize_keyfile() {
        let dir = std::env::temp_dir().join(format!("cap12-oversize-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("big.key");
        // Magic-prefixed so it isn't a magic miss; bulk pushes it over the cap.
        let mut blob = MESH_KEYFILE_MAGIC.to_vec();
        blob.resize(MAX_KEYFILE_BYTES + 64, 0u8);
        mesh_identity_load_or_create(&path).expect("create owner-only identity");
        std::fs::write(&path, &blob).unwrap();
        assert!(
            mesh_identity_load_or_create(&path).is_err(),
            "an over-cap keyfile must be rejected, not loaded"
        );
        let _ = std::fs::remove_file(&path);
    }

    /// CAP-12 (unix): a minted keyfile carries private-key material, so it is
    /// created owner-only (0600), never inheriting a wide umask.
    #[cfg(unix)]
    #[test]
    fn load_keys_writes_owner_only_perms() {
        use std::os::unix::fs::PermissionsExt;
        let dir = std::env::temp_dir().join(format!("cap12-perms-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("node.key");
        let _ = std::fs::remove_file(&path);
        mesh_identity_load_or_create(&path).expect("mint+persist");
        let mode = std::fs::metadata(&path).unwrap().permissions().mode() & 0o777;
        assert_eq!(mode, 0o600, "keyfile must be owner-only rw");
        let _ = std::fs::remove_file(&path);
    }

    /// CAP-12: `load_keys` creates a stable, persisted identity; a second load
    /// yields byte-identical material, and a transport that installs it adopts
    /// that identity rather than minting a fresh one (D14 — per-instance
    /// install, no process-global override).
    #[test]
    fn load_keys_persists_and_transport_adopts_installed_identity() {
        let dir = std::env::temp_dir().join(format!("cap12-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("node.key");
        let _ = std::fs::remove_file(&path);

        let material1 = mesh_identity_load_or_create(&path).expect("mint+persist");
        assert!(path.exists(), "keyfile must be written when absent");
        let material2 = mesh_identity_load_or_create(&path).expect("reload");
        assert_eq!(
            material1.spki(),
            material2.spki(),
            "reload must yield the same identity"
        );

        // A transport that installs the loaded identity adopts it, not a fresh mint.
        let rt = std::sync::Arc::new(
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap(),
        );
        let qmt = QuicMeshTransport::new(rt);
        qmt.install_auth(InstalledMeshAuth {
            cluster_admission: false,
            identity: Some(mesh_tls_from_material(&material1)),
            allowed_peer_spkis: HashSet::new(),
            setup_error: None,
        });
        let (_tls, adopted) = qmt.ensure_identity().expect("ensure_identity");
        assert_eq!(
            adopted,
            material1.spki(),
            "ensure_identity must adopt the installed load_keys identity"
        );
        let _ = std::fs::remove_file(&path);
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

    /// A peer that opened the control stream and then went silent leaves a
    /// reader blocked with no deadline. Node stop's sweep must wake it while
    /// the connection's slot stays owned until its owner closes it.
    #[test]
    fn transport_cancel_wakes_blocked_reader_and_keeps_its_slot() {
        let (tls_a, spki_a) = MeshTls::self_signed(vec!["cancel-a".into()]).expect("tls a");
        let (tls_b, spki_b) = MeshTls::self_signed(vec!["cancel-b".into()]).expect("tls b");
        // SAFETY: each constructor returns an owned transport destroyed below.
        let (a, b) = unsafe { (hew_transport_quic_mesh_new(), hew_transport_quic_mesh_new()) };
        assert!(!a.is_null() && !b.is_null());
        // SAFETY: both transports are live and not yet listening.
        unsafe {
            assert_eq!(
                hew_transport_quic_mesh_set_tls_override(a, tls_a.with_peer_spki(spki_b)),
                0
            );
            assert_eq!(
                hew_transport_quic_mesh_set_tls_override(b, tls_b.with_peer_spki(spki_a)),
                0
            );
        }
        let any = std::ffi::CString::new("127.0.0.1:0").unwrap();
        // SAFETY: the transports and C strings are live for every call below.
        let (a_conn, b_conn) = unsafe {
            let (ta, tb) = (&*a, &*b);
            let (oa, ob) = (&*ta.ops, &*tb.ops);
            assert_eq!(oa.listen.unwrap()(ta.r#impl, any.as_ptr()), 0);
            assert_eq!(ob.listen.unwrap()(tb.r#impl, any.as_ptr()), 0);
            let port = hew_transport_quic_mesh_bound_port(a).expect("bound port");
            let target = std::ffi::CString::new(format!("127.0.0.1:{port}")).unwrap();
            let b_conn = ob.connect.unwrap()(tb.r#impl, target.as_ptr());
            assert_ne!(b_conn, HEW_CONN_INVALID);
            let a_conn = oa.accept.unwrap()(ta.r#impl, 10_000);
            assert_ne!(a_conn, HEW_CONN_INVALID);
            // One frame opens the control stream; the reader consumes it.
            assert_eq!(
                ob.send.unwrap()(tb.r#impl, b_conn, b"x".as_ptr().cast(), 1),
                1
            );
            let mut first = [0u8; 8];
            assert_eq!(
                oa.recv.unwrap()(ta.r#impl, a_conn, first.as_mut_ptr().cast(), first.len()),
                1
            );
            (a_conn, b_conn)
        };

        let a_addr = a as usize;
        let (done_tx, done_rx) = std::sync::mpsc::channel();
        let reader = std::thread::spawn(move || {
            let a = a_addr as *mut HewTransport;
            let mut bytes = [0u8; 8];
            // SAFETY: the transport outlives this joined reader.
            let n = unsafe {
                ((*(*a).ops).recv.unwrap())((*a).r#impl, a_conn, bytes.as_mut_ptr().cast(), 8)
            };
            done_tx.send(n).unwrap();
        });
        // B stays silent and connected, so only the sweep can end the read
        // before the idle timeout.
        // SAFETY: `a` is live.
        unsafe { crate::transport::hew_transport_cancel_io(a) };
        let woken = done_rx.recv_timeout(Duration::from_secs(20));
        assert!(
            matches!(woken, Ok(n) if n < 0),
            "sweep must wake the reader: {woken:?}"
        );
        reader.join().unwrap();

        // SAFETY: `a` is live; the ops check passed when it was created.
        let qmt = unsafe { &*(*a).r#impl.cast::<QuicMeshTransport>() };
        assert!(
            qmt.get_conn(a_conn).is_some(),
            "the owner still holds its slot"
        );
        // SAFETY: each owner closes its own handle, then destroys its transport.
        unsafe {
            ((*(*a).ops).close_conn.unwrap())((*a).r#impl, a_conn);
            ((*(*b).ops).close_conn.unwrap())((*b).r#impl, b_conn);
            assert!(qmt.get_conn(a_conn).is_none());
            hew_transport_quic_mesh_free(a);
            hew_transport_quic_mesh_free(b);
        }
    }

    // -----------------------------------------------------------------------
    // SlotTable — stable peer-connection handle bookkeeping
    // -----------------------------------------------------------------------

    #[test]
    fn closing_empty_slot_preserves_pending_reservation() {
        let rt = Arc::new(tokio::runtime::Runtime::new().unwrap());
        let transport = QuicMeshTransport::new(rt);
        let idx = transport.slot_table.lock().unwrap().alloc().unwrap();
        transport.remove_conn(c_int::try_from(idx).unwrap());
        assert_ne!(transport.slot_table.lock().unwrap().alloc(), Some(idx));
    }

    /// An empty table returns the first slot.
    #[test]
    fn slot_table_empty_alloc_uses_first_empty_slot() {
        let mut slots = SlotTable::new(4);
        assert_eq!(slots.alloc(), Some(0));
        assert_eq!(slots.alloc(), Some(1));
    }

    /// Filling to capacity uses each slot exactly once.
    #[test]
    fn slot_table_fills_all_slots_without_recycling() {
        let mut slots = SlotTable::new(4);
        for expected in 0..4 {
            assert_eq!(slots.alloc(), Some(expected));
        }
    }

    /// Once full, allocation refuses to recycle any live handle.
    #[test]
    fn slot_table_refuses_allocation_when_full() {
        let mut slots = SlotTable::new(3);
        for _ in 0..3 {
            let _ = slots.alloc();
        }
        assert_eq!(slots.alloc(), None);
        assert_eq!(slots.alloc(), None);
    }

    /// Releasing a slot makes it available as an empty target.
    #[test]
    fn slot_table_release_frees_slot() {
        let mut slots = SlotTable::new(3);
        for _ in 0..3 {
            let _ = slots.alloc();
        }
        slots.release(1);
        assert_eq!(slots.alloc(), Some(1));
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

    // -----------------------------------------------------------------------
    // D14 — per-instance mesh peer-auth (identity + allowlist + setup poison)
    // -----------------------------------------------------------------------

    /// `QuicMeshTransport::ensure_identity` is idempotent: the cached cert +
    /// SPKI survive across calls (required so `local_spki` returns the same
    /// bytes that the eventual `listen` uses).
    #[test]
    fn transport_identity_is_cached() {
        let rt = Arc::new(
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap(),
        );
        let qmt = QuicMeshTransport::new(rt);
        let (_, spki_a) = qmt.ensure_identity().expect("mint identity");
        let (_, spki_b) = qmt.ensure_identity().expect("re-fetch identity");
        assert_eq!(
            spki_a, spki_b,
            "ensure_identity must return the same SPKI on repeated calls"
        );
    }

    /// `listen_tls` unions the cached own-SPKI with this transport's installed
    /// allowlist (D14, per-instance). Self-loopback (own SPKI) must always be
    /// trusted; installed peers must be trusted; un-installed SPKIs must NOT be.
    #[test]
    fn listen_tls_unions_installed_allowlist_with_own_spki() {
        let rt = Arc::new(
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap(),
        );
        let qmt = QuicMeshTransport::new(rt);
        let (_, own_spki) = qmt.ensure_identity().expect("mint identity");

        let registered = vec![0x55u8; 48];
        let other = vec![0x66u8; 48];
        qmt.install_auth(InstalledMeshAuth {
            cluster_admission: false,
            identity: None,
            allowed_peer_spkis: [registered.clone()].into_iter().collect(),
            setup_error: None,
        });

        let tls = qmt.listen_tls().expect("listen_tls");
        assert!(
            tls.allowed_peer_spkis.contains(&own_spki),
            "own SPKI must always be trusted (self-loopback)"
        );
        assert!(
            tls.allowed_peer_spkis.contains(&registered),
            "installed SPKI must be in the allowlist"
        );
        assert!(
            !tls.allowed_peer_spkis.contains(&other),
            "un-installed SPKI must not be in the allowlist"
        );
    }

    /// D14 isolation: two concurrent transports hold independent identities,
    /// allowlists, and setup-error poison with no cross-contamination. Node1
    /// admits only peer B; node2 (poisoned) fails closed; neither leaks into
    /// the other. This is the invariant the retired `ACTIVE_MESH_*` globals
    /// could not provide.
    #[test]
    fn mesh_transport_auth_isolation() {
        let mk_rt = || {
            Arc::new(
                tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .unwrap(),
            )
        };
        let node1 = QuicMeshTransport::new(mk_rt());
        let node2 = QuicMeshTransport::new(mk_rt());

        let peer_b = vec![0xB1u8; 48];
        let peer_d = vec![0xD2u8; 48];
        node1.install_auth(InstalledMeshAuth {
            cluster_admission: false,
            identity: None,
            allowed_peer_spkis: [peer_b.clone()].into_iter().collect(),
            setup_error: None,
        });
        node2.install_auth(InstalledMeshAuth {
            cluster_admission: false,
            identity: None,
            allowed_peer_spkis: [peer_d.clone()].into_iter().collect(),
            setup_error: Some("node2: simulated load_keys failure".into()),
        });

        // node1 admits only its own allowed peer B, not node2's peer D.
        let tls1 = node1.listen_tls().expect("node1 listen_tls");
        assert!(
            tls1.allowed_peer_spkis.contains(&peer_b),
            "node1 must admit its own allowed peer B"
        );
        assert!(
            !tls1.allowed_peer_spkis.contains(&peer_d),
            "node1 must NOT admit node2's peer D (no cross-contamination)"
        );

        // node2's poison is independent: it fails closed while node1 stays live.
        let blocked = node2.ensure_identity();
        assert!(
            blocked.is_err(),
            "node2's installed setup_error must fail closed"
        );
        assert!(
            format!("{}", blocked.unwrap_err()).contains("simulated load_keys failure"),
            "node2's fail-closed error must echo its own reason"
        );
        assert!(
            node1.ensure_identity().is_ok(),
            "node1 must stay live despite node2's independent poison"
        );
    }

    /// F6 fail-closed: an installed peer-auth setup failure (a failed
    /// `Node::load_keys`, staged into the snapshot) makes
    /// `ensure_identity` refuse to mint or reuse an identity, so the mesh
    /// listener cannot bind with an ephemeral cert or an incomplete allowlist. A
    /// transport with no installed poison mints the loopback-dev default (a
    /// fresh self-signed identity), so the gate only fires after an *actual*
    /// failure, never when peer-auth was simply never configured.
    #[test]
    fn installed_peer_auth_failure_makes_ensure_identity_fail_closed() {
        // No poison installed: the loopback-dev default mints a self-signed
        // identity (peer-auth was never configured — not a failure).
        let rt0 = Arc::new(
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap(),
        );
        assert!(
            QuicMeshTransport::new(rt0).ensure_identity().is_ok(),
            "with no installed failure, ensure_identity mints the dev default"
        );

        // An installed failure poisons the identity: ensure_identity fails closed.
        let rt1 = Arc::new(
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap(),
        );
        let poisoned = QuicMeshTransport::new(rt1);
        poisoned.install_auth(InstalledMeshAuth {
            cluster_admission: false,
            identity: None,
            allowed_peer_spkis: HashSet::new(),
            setup_error: Some("test: simulated load_keys failure".into()),
        });
        let blocked = poisoned.ensure_identity();
        assert!(
            blocked.is_err(),
            "an installed peer-auth failure must block the listener identity (fail-closed)"
        );
        let msg = format!("{}", blocked.unwrap_err());
        assert!(
            msg.contains("fail-closed") && msg.contains("simulated load_keys failure"),
            "the fail-closed error must echo the installed reason, got: {msg}"
        );
    }

    // -----------------------------------------------------------------------
    // P1.2 oversize-frame rejection + P1.3 handshake timeout
    // -----------------------------------------------------------------------

    /// P1.2 — `framed_recv_mesh` must reject an oversize-frame header and
    /// stop the recv stream rather than attempting to drain undefined bytes.
    /// We exercise the path end-to-end with a real Quinn stream so the
    /// `recv.stop` call is wired correctly.
    #[tokio::test]
    async fn framed_recv_rejects_oversize_frame_and_stops_stream() {
        let (tls_a, spki_a) = MeshTls::self_signed(vec!["a".into()]).expect("tls_a self_signed");
        let (tls_b, spki_b) = MeshTls::self_signed(vec!["b".into()]).expect("tls_b self_signed");
        let tls_a = tls_a.with_peer_spki(spki_b);
        let tls_b = tls_b.with_peer_spki(spki_a);

        let mesh_a = QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_a).expect("listen a");
        let addr_a = mesh_a.local_addr().expect("addr_a");
        let mesh_b = QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_b).expect("listen b");

        let (b_connect, a_accept) = tokio::join!(mesh_b.connect(addr_a), mesh_a.accept());
        let conn_b = b_connect.expect("connect");
        let conn_a = a_accept.expect("accept None").expect("accept err");

        // B opens a control stream and writes an OVERSIZE frame header.
        let (mut bsend, _brecv) = conn_b.conn.open_bi().await.expect("open_bi");
        let oversize: u32 = u32::try_from(MAX_FRAME_SIZE)
            .expect("MAX_FRAME_SIZE fits in u32")
            .saturating_add(1);
        bsend
            .write_all(&oversize.to_le_bytes())
            .await
            .expect("write header");

        // A accepts the stream and calls framed_recv_mesh — must return -1
        // and set last-error mentioning oversize.
        let (_asend, arecv) = conn_a.conn.accept_bi().await.expect("accept_bi");

        // Run framed_recv_mesh on a dedicated current-thread runtime since
        // it expects to block_on a separate runtime handle. `set_last_error`
        // is thread-local, so read the error message inside the same blocking
        // closure.
        let result = tokio::task::spawn_blocking(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            let mut buf = vec![0u8; 64];
            let n = framed_recv_mesh(&rt, &mut MeshRecv::new(arecv), &mut buf);
            let err_ptr = crate::hew_last_error();
            let err = if err_ptr.is_null() {
                String::new()
            } else {
                // SAFETY: hew_last_error returns a thread-local C string ptr
                // valid for the current scope.
                unsafe { std::ffi::CStr::from_ptr(err_ptr) }
                    .to_string_lossy()
                    .into_owned()
            };
            (n, err)
        })
        .await
        .expect("join");
        let (n, err) = result;
        assert_eq!(n, -1, "oversize frame must return -1");
        assert!(
            err.contains("oversize") || err.contains("MAX_FRAME_SIZE"),
            "expected oversize diagnostic, got: {err}"
        );
    }

    /// A capacity retry retains the frame header and the following frame.
    #[tokio::test]
    async fn framed_recv_resizes_without_losing_large_or_following_frame() {
        tokio::time::timeout(std::time::Duration::from_secs(30), async {
            let (tls_a, spki_a) = MeshTls::self_signed(vec!["a".into()]).unwrap();
            let (tls_b, spki_b) = MeshTls::self_signed(vec!["b".into()]).unwrap();
            let mesh_a =
                QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_a.with_peer_spki(spki_b))
                    .unwrap();
            let mesh_b =
                QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_b.with_peer_spki(spki_a))
                    .unwrap();
            let (connect, accept) = tokio::join!(
                mesh_b.connect(mesh_a.local_addr().unwrap()),
                mesh_a.accept()
            );
            let conn_b = connect.unwrap();
            let conn_a = accept.unwrap().unwrap();
            let (mut send, _recv) = conn_b.conn.open_bi().await.unwrap();
            let expected: Vec<u8> = (0..131_072)
                .map(|index| u8::try_from(index % 251).unwrap())
                .collect();
            let payload = expected.clone();
            let sending = tokio::spawn(async move {
                send.write_all(&u32::try_from(payload.len()).unwrap().to_le_bytes())
                    .await
                    .unwrap();
                send.write_all(&payload).await.unwrap();
                send.write_all(&5u32.to_le_bytes()).await.unwrap();
                send.write_all(b"after").await.unwrap();
            });
            let (_send, recv) = conn_a.conn.accept_bi().await.unwrap();
            tokio::task::spawn_blocking(move || {
                let rt = tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .unwrap();
                let mut recv = MeshRecv::new(recv);
                let mut small = vec![0xa5; 65536];
                for _ in 0..2 {
                    assert_eq!(framed_recv_mesh(&rt, &mut recv, &mut small), 131_072);
                    assert!(small.iter().all(|byte| *byte == 0xa5));
                }
                let mut large = vec![0; 131_072];
                assert_eq!(framed_recv_mesh(&rt, &mut recv, &mut large), 131_072);
                assert_eq!(large, expected);
                assert_eq!(framed_recv_mesh(&rt, &mut recv, &mut small), 5);
                assert_eq!(&small[..5], b"after");
            })
            .await
            .unwrap();
            sending.await.unwrap();
        })
        .await
        .expect("mesh capacity test watchdog");
    }

    /// P1.3 — `ensure_control_stream` must time out instead of hanging when
    /// the peer never opens the control stream. We connect two meshes,
    /// initiate the control stream from one side, and expect the other side
    /// to surface a typed timeout when its `ensure_control_stream` runs in
    /// `accept_bi` mode against a peer that never calls `open_bi`.
    #[tokio::test]
    async fn ensure_control_stream_times_out_when_peer_silent() {
        let (tls_a, spki_a) = MeshTls::self_signed(vec!["a-to".into()]).expect("tls_a self_signed");
        let (tls_b, spki_b) = MeshTls::self_signed(vec!["b-to".into()]).expect("tls_b self_signed");
        let tls_a = tls_a.with_peer_spki(spki_b);
        let tls_b = tls_b.with_peer_spki(spki_a);

        let mesh_a = QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_a).expect("listen a");
        let addr_a = mesh_a.local_addr().expect("addr_a");
        let mesh_b = QuicMesh::listen("127.0.0.1:0".parse().unwrap(), tls_b).expect("listen b");

        let (b_connect, a_accept) = tokio::join!(mesh_b.connect(addr_a), mesh_a.accept());
        let conn_b = b_connect.expect("connect");
        let conn_a = a_accept.expect("accept None").expect("accept err");

        // A waits for B to open a control stream — but B never does.
        // ensure_control_stream on the acceptor side must time out under
        // MESH_HANDSHAKE_TIMEOUT (5s) and return an error mentioning the
        // timeout. Drive it on a blocking thread (it uses block_on) but cap
        // the wall clock at 2× the configured timeout to fail fast if the
        // timeout itself is broken.
        let acceptor_conn = conn_a;
        let started = std::time::Instant::now();
        let result = tokio::task::spawn_blocking(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            let conn = QuicMeshConn::new(acceptor_conn, /* is_initiator = */ false, &rt);
            conn.ensure_control_stream(&rt)
        })
        .await
        .expect("join");
        let elapsed = started.elapsed();
        assert!(
            result.is_err(),
            "ensure_control_stream must surface a timeout error"
        );
        let msg = result.unwrap_err();
        assert!(
            msg.contains("timeout"),
            "expected typed timeout diagnostic, got: {msg}"
        );
        assert!(
            elapsed < MESH_HANDSHAKE_TIMEOUT * 3,
            "timeout fired too late: {elapsed:?}"
        );
        // Keep `conn_b` alive until after the timeout fires so the
        // connection isn't torn down before A's accept_bi can observe the
        // silent peer.
        drop(conn_b);
    }
}
