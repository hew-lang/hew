//! Peer authentication authority: binds an authenticated peer credential
//! (Noise static key on TCP, certificate SPKI on quic-mesh) to one receiver-
//! local route slot and its key-derived [`NodeId`].
//!
//! # Why this module exists
//!
//! Historically the distributed runtime authenticated a *key* against a flat
//! process-global allowlist (a single set of admitted Noise/SPKI credentials
//! shared across every node) and, *independently*, checked that a
//! peer's self-declared handshake numeric id was plausible
//! (`connection::peer_identity_compatible`). Nothing bound the two: any admitted
//! key could claim any route. This module is the authority that closes that
//! gap — it maps one route slot to one authenticated credential and returns a
//! structured [`PeerAuthz`] verdict, never a bare bool.
//!
//! # Two authorities (never one process-global that governs all nodes)
//!
//! * [`PeerAuthSnapshot`] — the **per-node** frozen authority the connection
//!   manager and transport consume. Each [`crate::hew_node::HewNode`] /
//!   [`crate::connection::HewConnMgr`] owns its own; concurrent low-level nodes
//!   never share one. Its identity material lives behind [`std::sync::Arc`], so
//!   cloning is cheap and never duplicates secret bytes.
//! * [`ConfigState`] (in [`PEER_AUTH_STATE`]) — the **public-API-only** staging
//!   area where the singleton `Node::*` builtins accumulate config before a
//!   public node exists, plus an owner-scoped lifecycle guard. No low-level
//!   `hew_node_*` path consults it.
//!
//! This module is native-only (the distributed runtime requires OS threads) and
//! feature-agnostic: it stores identity material as opaque bytes so it compiles
//! under every native feature combination, including `--no-default-features`.
//! The `encryption` / `quic` transport layers convert these byte containers to
//! and from their `snow` / `rustls` types at the transport boundary.

use std::collections::{HashMap, HashSet};
use std::num::NonZeroU16;
use std::os::raw::c_int;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use zeroize::Zeroizing;

use crate::node_identity::{NodeId, NodeSessionLease};

/// Length in bytes of a Noise static public/private key (X25519).
pub const NOISE_KEY_LEN: usize = 32;

/// Which transport a node's credentials are interpreted for.
///
/// Self-contained (mirrors the private `hew_node::TransportSelection`) so this
/// module compiles regardless of the `quic` feature; the runtime rejects an
/// unsupported selection at start.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum TransportSelection {
    /// TCP with optional Noise-XX encryption.
    Tcp,
    /// Plain QUIC (TLS 1.3, no peer-credential binding mechanism).
    Quic,
    /// QUIC mesh with mutual SPKI-pinned TLS.
    QuicMesh,
}

/// An authenticated peer credential, keyed to the `NodeId` it may claim.
///
/// `NoiseKey` is a 32-byte X25519 static public key recovered from the Noise-XX
/// handshake (`get_remote_static`); `Spki` is the DER `SubjectPublicKeyInfo`
/// extracted from a quic-mesh peer's leaf certificate.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PeerCredential {
    /// Noise static public key (TCP transport).
    NoiseKey([u8; NOISE_KEY_LEN]),
    /// Certificate `SubjectPublicKeyInfo` (quic-mesh transport).
    Spki(Vec<u8>),
}

impl PeerCredential {
    /// Key-derived identity for this canonical credential.
    #[must_use]
    pub fn node_id(&self) -> NodeId {
        match self {
            Self::NoiseKey(key) => NodeId::from_noise_static_key(key),
            Self::Spki(spki) => NodeId::from_spki(spki),
        }
    }
}

/// Compact receiver-local routing alias. Slot zero is reserved for local
/// dispatch and is never a peer pin.
pub type RouteSlot = u16;

/// Exact one-to-one route-slot/credential configuration.
pub type PeerBindings = HashMap<RouteSlot, PeerCredential>;

/// Result of installing a peer pin.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PinOutcome {
    /// A new one-to-one pin was installed.
    Inserted,
    /// The same slot/credential pair was already installed.
    Unchanged,
}

/// Invalid or conflicting peer pin.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PeerBindingError {
    /// Route slot zero is reserved for local dispatch.
    ReservedRouteSlot,
    /// The route slot is already pinned to a different credential.
    RouteSlotAlreadyBound {
        /// Conflicting slot.
        route_slot: RouteSlot,
    },
    /// The credential is already pinned under a different route slot.
    CredentialAlreadyBound {
        /// Existing slot.
        route_slot: RouteSlot,
    },
    /// Distinct canonical credentials derived the same truncated identity.
    NodeIdCollision {
        /// Colliding identity.
        node_id: NodeId,
        /// Existing slot whose credential already derives this identity.
        route_slot: RouteSlot,
    },
}

impl std::fmt::Display for PeerBindingError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ReservedRouteSlot => {
                formatter.write_str("route slot 0 is reserved for local dispatch")
            }
            Self::RouteSlotAlreadyBound { route_slot } => {
                write!(
                    formatter,
                    "route slot {route_slot} is already bound to another credential"
                )
            }
            Self::CredentialAlreadyBound { route_slot } => {
                write!(
                    formatter,
                    "credential is already bound to route slot {route_slot}"
                )
            }
            Self::NodeIdCollision {
                node_id,
                route_slot,
            } => write!(
                formatter,
                "credential NodeId collision with route slot {route_slot}: {node_id}"
            ),
        }
    }
}

impl std::error::Error for PeerBindingError {}

/// The posture a single connection is admitted under.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Posture {
    /// Credential-bound admission is required; a missing/mismatched credential
    /// is rejected fail-closed.
    Strict,
    /// Loopback-dev / explicit opt-out: delivery only, no authenticated
    /// identity, no cluster/gossip/ask-routing authority.
    Unverified,
}

/// Classification of a connection's remote endpoint, used to pick a posture
/// per connection without a transport-vtable change.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RemoteIpClass {
    /// The peer address is a loopback address (127.0.0.0/8, `::1`).
    Loopback,
    /// The peer address is a routable/non-loopback address.
    NonLoopback,
    /// The remote address could not be determined (plain-quic, stub, custom
    /// transport) — treated as strict, fail-closed.
    Unknown,
}

/// Structured authorization verdict — never a bare bool. Every non-`Authorized`
/// / non-`Unverified` variant maps to a distinct diagnostic at the call site.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PeerAuthz {
    /// Strict: the presented credential is bound to the selected route slot.
    Authorized(RouteSlot),
    /// The route slot has no configured credential.
    NoBindingForRoute,
    /// The presented credential is bound to a *different* route slot.
    CredentialBoundElsewhere {
        /// The route slot the credential is actually bound to.
        route_slot: RouteSlot,
    },
    /// The route slot is configured, but its credential does not match.
    CredentialMismatch,
    /// Route slot zero is reserved for local dispatch.
    InvalidRouteSlot,
    /// Strict posture, but no credential was presented.
    MissingCredential,
    /// Loopback-dev / opt-out — delivery only; a `None` credential is legal.
    Unverified,
}

/// Whether a `LiveClaim` is mid-admission (reserved) or established (published).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ClaimState {
    /// An admission is in flight for this `NodeId`; a concurrent admission waits.
    Reserved,
    /// The connection is established and owns this `NodeId`'s routes/tokens.
    Published,
    /// The last live connection retired; the durable session remains as a replay
    /// fence for later admissions.
    Retired,
}

/// The live owner of a `NodeId` on one connection manager.
#[derive(Clone, Debug)]
pub struct LiveClaim {
    /// The authenticated credential owning this `NodeId`; `None` only under
    /// `Unverified` posture.
    pub credential: Option<PeerCredential>,
    /// Receiver-local route slot resolved from the authenticated credential.
    pub route_slot: RouteSlot,
    /// Durable session incarnation advertised by the peer.
    pub session_incarnation: u32,
    /// The transport connection id that holds this claim.
    pub conn_id: c_int,
    /// The publication token uniquely identifying this admission.
    pub publication_token: u64,
    /// Reserved (mid-admission) vs Published (established).
    pub state: ClaimState,
}

/// A node's stable Noise static keypair (TCP transport identity).
///
/// Stored as opaque bytes behind an [`Arc`] so cloning a snapshot never
/// duplicates the private key. The `encryption` layer converts the raw bytes to
/// a `snow` keypair at handshake time.
#[derive(Clone)]
pub struct StableNoiseIdentity {
    public: [u8; NOISE_KEY_LEN],
    private: Arc<Zeroizing<[u8; NOISE_KEY_LEN]>>,
}

impl StableNoiseIdentity {
    /// Construct from raw 32-byte public/private key material.
    #[must_use]
    pub fn from_raw(public: [u8; NOISE_KEY_LEN], private: [u8; NOISE_KEY_LEN]) -> Self {
        Self {
            public,
            private: Arc::new(Zeroizing::new(private)),
        }
    }

    /// The stable Noise static public key (the bindable peer identity).
    #[must_use]
    pub fn public(&self) -> [u8; NOISE_KEY_LEN] {
        self.public
    }

    /// The stable Noise static private key.
    #[must_use]
    pub fn private(&self) -> [u8; NOISE_KEY_LEN] {
        **self.private
    }
}

impl std::fmt::Debug for StableNoiseIdentity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Never print private key material.
        f.debug_struct("StableNoiseIdentity")
            .field("public", &hex_lower(&self.public))
            .finish_non_exhaustive()
    }
}

/// A node's stable mesh (quic-mesh) TLS identity material.
///
/// Stored as opaque DER bytes behind an [`Arc`]; the `quic_mesh` layer converts
/// them to a rustls certificate/key pair at listener construction.
#[derive(Clone)]
pub struct MeshIdentityMaterial {
    inner: Arc<MeshIdentityBytes>,
}

struct MeshIdentityBytes {
    /// DER-encoded certificate chain (leaf first).
    cert_chain_der: Vec<Vec<u8>>,
    /// DER-encoded PKCS#8 private key.
    private_key_der: Zeroizing<Vec<u8>>,
    /// The local leaf certificate's `SubjectPublicKeyInfo` (the bindable id).
    spki: Vec<u8>,
}

impl MeshIdentityMaterial {
    /// Construct from DER material and the precomputed local leaf SPKI.
    #[must_use]
    pub fn from_der(cert_chain_der: Vec<Vec<u8>>, private_key_der: Vec<u8>, spki: Vec<u8>) -> Self {
        Self {
            inner: Arc::new(MeshIdentityBytes {
                cert_chain_der,
                private_key_der: Zeroizing::new(private_key_der),
                spki,
            }),
        }
    }

    /// DER-encoded certificate chain (leaf first).
    #[must_use]
    pub fn cert_chain_der(&self) -> &[Vec<u8>] {
        &self.inner.cert_chain_der
    }

    /// DER-encoded PKCS#8 private key.
    #[must_use]
    pub fn private_key_der(&self) -> &[u8] {
        &self.inner.private_key_der
    }

    /// The local leaf certificate's `SubjectPublicKeyInfo`.
    #[must_use]
    pub fn spki(&self) -> &[u8] {
        &self.inner.spki
    }
}

impl std::fmt::Debug for MeshIdentityMaterial {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MeshIdentityMaterial")
            .field("spki", &hex_lower(&self.inner.spki))
            .field("cert_chain_len", &self.inner.cert_chain_der.len())
            .finish_non_exhaustive()
    }
}

/// Lowercase-hex encode a byte slice.
#[must_use]
pub fn hex_lower(bytes: &[u8]) -> String {
    use std::fmt::Write as _;
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        let _ = write!(s, "{b:02x}");
    }
    s
}

/// The mutable public-API staging config, accumulated by `Node::allow_peer` /
/// `Node::load_keys` / `Node::set_transport` before a public node exists.
///
/// Frozen into a [`PeerAuthSnapshot`] via [`PeerAuthConfig::snapshot`] at start.
#[derive(Clone, Debug, Default)]
pub struct PeerAuthConfig {
    /// Receiver-local route slot advertised by this node.
    pub local_route_slot: Option<NonZeroU16>,
    /// Low-level diagnostic-only unverified posture. The public start path does
    /// not populate this from an environment bypass.
    pub unverified_optout: bool,
    /// Pinned transport selection (pinned at the first transport-sensitive op).
    pub transport: Option<TransportSelection>,
    /// Exact receiver-local route-slot/credential bindings.
    bindings: PeerBindings,
    /// Stable Noise identity (TCP), populated by `load_keys`.
    pub noise_identity: Option<StableNoiseIdentity>,
    /// Stable mesh identity (quic-mesh), populated by `load_keys`.
    pub mesh_identity: Option<MeshIdentityMaterial>,
    /// Key-derived identity for the staged stable credential.
    pub node_identity: Option<NodeId>,
    /// Stable identity path whose sibling journal owns the session counter.
    pub identity_path: Option<PathBuf>,
    /// Sticky fail-closed setup poison (a failed `load_keys` / `allow_peer`).
    pub setup_error: Option<String>,
}

impl PeerAuthConfig {
    /// Read-only access to the exact peer pins.
    #[must_use]
    pub(crate) fn bindings(&self) -> &PeerBindings {
        &self.bindings
    }

    /// Install one exact non-zero route-slot/credential pin.
    ///
    /// Repeating the same pair is idempotent. A distinct credential on the same
    /// slot, the same credential on another slot, or a derived `NodeId` collision
    /// is rejected without changing the configuration.
    ///
    /// # Errors
    ///
    /// Returns the exact one-to-one or collision violation.
    pub fn pin_peer(
        &mut self,
        route_slot: RouteSlot,
        credential: PeerCredential,
    ) -> Result<PinOutcome, PeerBindingError> {
        self.pin_peer_with_deriver(route_slot, credential, PeerCredential::node_id)
    }

    fn pin_peer_with_deriver(
        &mut self,
        route_slot: RouteSlot,
        credential: PeerCredential,
        derive: impl Fn(&PeerCredential) -> NodeId,
    ) -> Result<PinOutcome, PeerBindingError> {
        if route_slot == 0 {
            return Err(PeerBindingError::ReservedRouteSlot);
        }
        if let Some(existing) = self.bindings.get(&route_slot) {
            return if *existing == credential {
                Ok(PinOutcome::Unchanged)
            } else {
                Err(PeerBindingError::RouteSlotAlreadyBound { route_slot })
            };
        }
        if let Some((&bound_slot, _)) = self
            .bindings
            .iter()
            .find(|(_, existing)| **existing == credential)
        {
            return Err(PeerBindingError::CredentialAlreadyBound {
                route_slot: bound_slot,
            });
        }
        let node_id = derive(&credential);
        if let Some((&bound_slot, _)) = self
            .bindings
            .iter()
            .find(|(_, existing)| derive(existing) == node_id)
        {
            return Err(PeerBindingError::NodeIdCollision {
                node_id,
                route_slot: bound_slot,
            });
        }
        self.bindings.insert(route_slot, credential);
        Ok(PinOutcome::Inserted)
    }

    /// The identity string `Node::identity_key` returns for this config: the
    /// lowercase-hex of the stable credential for the pinned transport, or the
    /// empty string when no stable identity has been loaded.
    #[must_use]
    pub fn identity_export_string(&self) -> String {
        match self.transport {
            Some(TransportSelection::QuicMesh) => self
                .mesh_identity
                .as_ref()
                .map(|m| hex_lower(m.spki()))
                .unwrap_or_default(),
            // TCP (or unpinned) exports the Noise pubkey when present.
            _ => self
                .noise_identity
                .as_ref()
                .map(|n| hex_lower(&n.public()))
                .unwrap_or_default(),
        }
    }

    /// Whether any transport-sensitive credential or stable identity has been
    /// staged (issue #2652). Peer bindings (`allow_peer`) and stable identities
    /// (`load_keys`) are interpreted under the pinned [`Self::transport`] — a
    /// Noise pubkey vs a mesh SPKI — so once any is staged the transport MUST NOT
    /// change: a later flip would silently reinterpret already-staged material.
    /// `Node::set_transport` consults this to reject a post-staging transport
    /// change (fail-closed) rather than corrupt the credential interpretation.
    #[must_use]
    pub fn has_staged_credentials(&self) -> bool {
        !self.bindings.is_empty() || self.noise_identity.is_some() || self.mesh_identity.is_some()
    }

    /// Validate the *public* config before listen/allocation (D109 pre-listen).
    ///
    /// * strict-bound (`bindings` non-empty) requires a stable local credential;
    /// * the unverified opt-out cannot coexist with configured bindings.
    ///
    /// # Errors
    ///
    /// Returns a typed message on an invalid posture combination.
    pub fn validate_public(&self) -> Result<(), String> {
        if self.node_identity.is_none() || self.identity_path.is_none() {
            return Err(
                "Node::start: protocol v2 requires Node::load_keys before start \
                 (authenticated identity and durable session are mandatory)"
                    .to_string(),
            );
        }
        let has_transport_identity = match self.transport.unwrap_or(TransportSelection::Tcp) {
            TransportSelection::Tcp => self.noise_identity.is_some(),
            #[cfg(feature = "quic")]
            TransportSelection::QuicMesh => self.mesh_identity.is_some(),
            #[cfg(feature = "quic")]
            TransportSelection::Quic => false,
        };
        if !has_transport_identity {
            return Err(
                "Node::start: protocol v2 requires an authenticated tcp-noise or quic-mesh identity"
                    .to_string(),
            );
        }
        if self.unverified_optout && !self.bindings.is_empty() {
            return Err(
                "Node::start: unverified opt-out cannot be combined with configured peer \
                 bindings (fail-closed)"
                    .to_string(),
            );
        }
        Ok(())
    }

    /// Freeze this staging config into a per-node [`PeerAuthSnapshot`].
    ///
    /// The `transport` defaults to TCP when unpinned (mirrors
    /// `transport_selection_from_env`'s default).
    #[must_use]
    pub fn snapshot(&self) -> PeerAuthSnapshot {
        self.snapshot_with_session(None)
    }

    /// Freeze this staging config for one public node start.
    ///
    /// When a stable credential has been loaded, this acquires and advances its
    /// exclusive session journal before any listener or claim can be published.
    ///
    /// # Errors
    ///
    /// Returns an error for inconsistent identity staging or journal failure.
    pub fn snapshot_for_start(&self) -> Result<PeerAuthSnapshot, String> {
        let session = match (self.node_identity, self.identity_path.as_deref()) {
            (None, None) => None,
            (Some(node_id), Some(path)) => Some(Arc::new(
                NodeSessionLease::acquire(path, node_id)
                    .map_err(|error| format!("Node::start: {error}"))?,
            )),
            _ => {
                return Err(
                    "Node::start: loaded identity is missing its identity path (fail-closed)"
                        .to_string(),
                );
            }
        };
        Ok(self.snapshot_with_session(session))
    }

    fn snapshot_with_session(
        &self,
        session_lease: Option<Arc<NodeSessionLease>>,
    ) -> PeerAuthSnapshot {
        let mut mesh_spki_allowlist: HashSet<Vec<u8>> = HashSet::new();
        for credential in self.bindings.values() {
            if let PeerCredential::Spki(spki) = credential {
                mesh_spki_allowlist.insert(spki.clone());
            }
        }
        PeerAuthSnapshot {
            inner: Arc::new(SnapshotInner {
                local_route_slot: self.local_route_slot,
                unverified: self.unverified_optout,
                transport: self.transport.unwrap_or(TransportSelection::Tcp),
                bindings: self.bindings.clone(),
                noise_identity: self.noise_identity.clone(),
                mesh_identity: self.mesh_identity.clone(),
                node_identity: self.node_identity,
                session_lease,
                mesh_spki_allowlist,
                setup_error: self.setup_error.clone(),
            }),
        }
    }
}

/// The frozen, per-node authority the manager and transport consume.
///
/// Immutable once installed on a node; cloning shares the inner state (identity
/// material behind [`Arc`]) so it is cheap and never duplicates secret bytes.
#[derive(Clone)]
pub struct PeerAuthSnapshot {
    inner: Arc<SnapshotInner>,
}

struct SnapshotInner {
    local_route_slot: Option<NonZeroU16>,
    unverified: bool,
    transport: TransportSelection,
    bindings: PeerBindings,
    noise_identity: Option<StableNoiseIdentity>,
    mesh_identity: Option<MeshIdentityMaterial>,
    node_identity: Option<NodeId>,
    session_lease: Option<Arc<NodeSessionLease>>,
    mesh_spki_allowlist: HashSet<Vec<u8>>,
    setup_error: Option<String>,
}

impl PeerAuthSnapshot {
    #[cfg(test)]
    pub(crate) fn for_test(
        node_identity: NodeId,
        peer_bindings: impl IntoIterator<Item = (RouteSlot, PeerCredential)>,
    ) -> Self {
        use std::sync::atomic::{AtomicU64, Ordering};

        static NEXT_TEST_LEASE: AtomicU64 = AtomicU64::new(1);
        let lease_id = NEXT_TEST_LEASE.fetch_add(1, Ordering::Relaxed);
        let lease_path = std::env::temp_dir().join(format!(
            "hew-peer-auth-test-{}-{lease_id}",
            std::process::id()
        ));
        let session_lease = NodeSessionLease::acquire(&lease_path, node_identity)
            .expect("test node session lease should be acquired");
        let _ = std::fs::remove_file(&lease_path);
        Self {
            inner: Arc::new(SnapshotInner {
                local_route_slot: None,
                unverified: false,
                transport: TransportSelection::Tcp,
                bindings: peer_bindings.into_iter().collect(),
                noise_identity: None,
                mesh_identity: None,
                node_identity: Some(node_identity),
                session_lease: Some(Arc::new(session_lease)),
                mesh_spki_allowlist: HashSet::new(),
                setup_error: None,
            }),
        }
    }

    /// The low-level default posture: an *unconfigured* node.
    ///
    /// No legacy wire slot, `unverified: false`, empty bindings/allowlist. This is
    /// **not** a blanket `Unverified` pass — posture is decided per connection
    /// (loopback ⇒ `Unverified` delivery-only; non-loopback / `Unknown` ⇒
    /// strict-reject). An unconfigured node never authenticates or silently
    /// accepts a non-loopback peer.
    #[must_use]
    pub fn unconfigured() -> Self {
        Self {
            inner: Arc::new(SnapshotInner {
                local_route_slot: None,
                unverified: false,
                transport: TransportSelection::Tcp,
                bindings: PeerBindings::new(),
                noise_identity: None,
                mesh_identity: None,
                node_identity: None,
                session_lease: None,
                mesh_spki_allowlist: HashSet::new(),
                setup_error: None,
            }),
        }
    }

    /// Receiver-local route slot advertised by the handshake.
    #[must_use]
    pub fn local_route_slot(&self) -> Option<NonZeroU16> {
        self.inner.local_route_slot
    }

    /// Whether this snapshot is the explicit documented unverified opt-out.
    #[must_use]
    pub fn is_unverified_optout(&self) -> bool {
        self.inner.unverified
    }

    /// The pinned transport selection.
    #[must_use]
    pub fn transport(&self) -> TransportSelection {
        self.inner.transport
    }

    /// The stable Noise identity, if loaded.
    #[must_use]
    pub fn noise_identity(&self) -> Option<&StableNoiseIdentity> {
        self.inner.noise_identity.as_ref()
    }

    /// The stable mesh identity, if loaded.
    #[must_use]
    pub fn mesh_identity(&self) -> Option<&MeshIdentityMaterial> {
        self.inner.mesh_identity.as_ref()
    }

    /// Key-derived identity for this frozen node snapshot.
    #[must_use]
    pub fn node_identity(&self) -> Option<NodeId> {
        self.inner.node_identity
    }

    /// Durable session incarnation held by this frozen node snapshot.
    #[must_use]
    pub fn session_incarnation(&self) -> Option<u32> {
        self.inner
            .session_lease
            .as_ref()
            .map(|lease| lease.incarnation())
    }

    /// The per-node mesh SPKI allowlist (the transport pre-gate for quic-mesh).
    #[must_use]
    pub fn mesh_spki_allowlist(&self) -> &HashSet<Vec<u8>> {
        &self.inner.mesh_spki_allowlist
    }

    /// The sticky fail-closed setup poison, if any.
    #[must_use]
    pub fn setup_error(&self) -> Option<&str> {
        self.inner.setup_error.as_deref()
    }

    /// Whether any credential bindings are configured (strict-bound node).
    #[must_use]
    pub fn has_bindings(&self) -> bool {
        !self.inner.bindings.is_empty()
    }

    /// True if the given Noise static public key is in this node's Noise
    /// allowlist (the per-node pre-gate replacing the global allowlist check).
    #[must_use]
    pub fn noise_pubkey_allowlisted(&self, pubkey: &[u8; NOISE_KEY_LEN]) -> bool {
        let cred = PeerCredential::NoiseKey(*pubkey);
        self.route_slot_for_credential(&cred).is_some()
    }

    /// Resolve an authenticated credential to its receiver-local route slot.
    ///
    /// Admission uses this after transport authentication: the v2 handshake
    /// carries a key-derived [`NodeId`], never the receiver's compact route slot.
    #[must_use]
    pub fn route_slot_for_credential(&self, credential: &PeerCredential) -> Option<RouteSlot> {
        self.inner
            .bindings
            .iter()
            .find(|(_, bound)| *bound == credential)
            .map(|(route_slot, _)| *route_slot)
    }

    /// Resolve a configured key-derived identity to its receiver-local route slot.
    #[must_use]
    pub fn route_slot_for_node_id(&self, node_id: NodeId) -> Option<RouteSlot> {
        self.inner
            .bindings
            .iter()
            .find(|(_, credential)| credential.node_id() == node_id)
            .map(|(route_slot, _)| *route_slot)
    }

    /// Resolve a configured route slot to the peer's key-derived identity.
    #[must_use]
    pub fn node_id_for_route_slot(&self, route_slot: RouteSlot) -> Option<NodeId> {
        self.inner
            .bindings
            .get(&route_slot)
            .map(PeerCredential::node_id)
    }

    /// Snapshot the configured receiver-local route aliases and identities.
    #[must_use]
    pub fn configured_node_routes(&self) -> Vec<(RouteSlot, NodeId)> {
        self.inner
            .bindings
            .iter()
            .map(|(route_slot, credential)| (*route_slot, credential.node_id()))
            .collect()
    }

    /// Validate the snapshot is self-consistent (defence-in-depth at the shared
    /// `hew_node_start`, applies to low-level callers too).
    ///
    /// * strict (`bindings` non-empty) requires a local route slot;
    /// * explicit opt-out (`unverified == true`) requires empty `bindings`;
    /// * `unconfigured` (`unverified == false`, empty bindings) is legal.
    ///
    /// # Errors
    ///
    /// Returns a typed message on a malformed snapshot.
    pub fn validate(&self) -> Result<(), String> {
        if self.inner.unverified && !self.inner.bindings.is_empty() {
            return Err(
                "hew_node_start: unverified opt-out snapshot must not carry peer bindings \
                 (fail-closed)"
                    .to_string(),
            );
        }
        if !self.inner.bindings.is_empty() && self.inner.local_route_slot.is_none() {
            return Err(
                "hew_node_start: strict binding snapshot requires a nonzero local route slot \
                 (fail-closed)"
                    .to_string(),
            );
        }
        Ok(())
    }

    /// Decide the posture for one connection from its remote endpoint class.
    ///
    /// | Condition | Posture |
    /// |---|---|
    /// | explicit unverified opt-out (bindings empty) | `Unverified` |
    /// | bindings non-empty (any remote) | `Strict` |
    /// | remote `NonLoopback` or `Unknown` | `Strict` |
    /// | remote `Loopback`, bindings empty, not opt-out | `Unverified` |
    #[must_use]
    pub fn posture_for(&self, remote: RemoteIpClass) -> Posture {
        if self.inner.unverified {
            return Posture::Unverified;
        }
        if self.has_bindings() {
            return Posture::Strict;
        }
        match remote {
            RemoteIpClass::Loopback => Posture::Unverified,
            RemoteIpClass::NonLoopback | RemoteIpClass::Unknown => Posture::Strict,
        }
    }

    /// The structured authorization verdict for a connection.
    ///
    /// In `Unverified` posture returns [`PeerAuthz::Unverified`] (a `None`
    /// credential is legal). In `Strict` posture resolves the credential
    /// against the exact route-slot/credential bindings.
    #[must_use]
    pub fn authorize(
        &self,
        posture: Posture,
        route_slot: RouteSlot,
        cred: Option<&PeerCredential>,
    ) -> PeerAuthz {
        if posture == Posture::Unverified {
            return PeerAuthz::Unverified;
        }
        if route_slot == 0 {
            return PeerAuthz::InvalidRouteSlot;
        }
        let Some(cred) = cred else {
            return PeerAuthz::MissingCredential;
        };
        match self.inner.bindings.get(&route_slot) {
            Some(bound) if *bound == *cred => PeerAuthz::Authorized(route_slot),
            Some(_) => {
                if let Some(bound) = self.credential_bound_route_slot(cred) {
                    PeerAuthz::CredentialBoundElsewhere { route_slot: bound }
                } else {
                    PeerAuthz::CredentialMismatch
                }
            }
            None => {
                if let Some(bound) = self.credential_bound_route_slot(cred) {
                    PeerAuthz::CredentialBoundElsewhere { route_slot: bound }
                } else {
                    PeerAuthz::NoBindingForRoute
                }
            }
        }
    }

    /// The route slot this credential is bound to, if any.
    fn credential_bound_route_slot(&self, cred: &PeerCredential) -> Option<RouteSlot> {
        self.route_slot_for_credential(cred)
    }
}

impl std::fmt::Debug for PeerAuthSnapshot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PeerAuthSnapshot")
            .field("local_route_slot", &self.inner.local_route_slot)
            .field("unverified", &self.inner.unverified)
            .field("transport", &self.inner.transport)
            .field("binding_node_count", &self.inner.bindings.len())
            .field(
                "mesh_spki_allowlist_len",
                &self.inner.mesh_spki_allowlist.len(),
            )
            .field("has_noise_identity", &self.inner.noise_identity.is_some())
            .field("has_mesh_identity", &self.inner.mesh_identity.is_some())
            .field("node_identity", &self.inner.node_identity)
            .field("session_incarnation", &self.session_incarnation())
            .field("setup_error", &self.inner.setup_error)
            .finish()
    }
}

/// The public-API-only staging + owner-scoped lifecycle guard.
///
/// Governs *only* the singleton public `Node::*` API. `owner` is the owning
/// `HewNode*` as a `usize` (mirroring `CURRENT_NODE`'s representation) — used
/// only for lifecycle matching (who may transition/reset), **never**
/// dereferenced. `identity_export` and `node_identity` in `Running` are cloned
/// values so public identity access never reads a raw node pointer.
#[derive(Debug)]
pub enum ConfigState {
    /// Pre-start public staging; `allow_peer` / `load_keys` / `set_transport`
    /// mutate the held config here.
    Building(PeerAuthConfig),
    /// The public node was created and its snapshot installed; the consumed
    /// config is retained for `identity_key` and `fail_start`.
    Starting {
        /// The lifecycle generation this transition owns.
        generation: u64,
        /// The owning `HewNode*` as a `usize` (never dereferenced).
        owner: usize,
        /// The consumed staging config (retained for restore/read).
        config: PeerAuthConfig,
    },
    /// The public node is live; `identity_export` is a standalone clone.
    Running {
        /// The lifecycle generation this transition owns.
        generation: u64,
        /// The owning `HewNode*` as a `usize` (never dereferenced).
        owner: usize,
        /// The cloned identity string `identity_key` returns while running.
        identity_export: String,
        /// The stable key-derived node identity, when configured.
        node_identity: Option<NodeId>,
    },
}

impl Default for ConfigState {
    fn default() -> Self {
        ConfigState::Building(PeerAuthConfig::default())
    }
}

/// The public-API staging state paired with a monotonic generation counter.
///
/// The generation is bumped on every owner-scoped reset so a stale `fail_start`
/// cannot clobber a concurrently-staged config.
pub static PEER_AUTH_STATE: std::sync::LazyLock<Mutex<PeerAuthStateCell>> =
    std::sync::LazyLock::new(|| Mutex::new(PeerAuthStateCell::new()));

/// The mutex-protected public staging cell.
#[derive(Debug)]
pub struct PeerAuthStateCell {
    /// The current public-API config lifecycle state.
    pub state: ConfigState,
    /// Monotonic generation for the next `Starting` transition.
    pub next_generation: u64,
}

impl PeerAuthStateCell {
    fn new() -> Self {
        Self {
            state: ConfigState::default(),
            next_generation: 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn noise(byte: u8) -> PeerCredential {
        PeerCredential::NoiseKey([byte; NOISE_KEY_LEN])
    }

    fn nz(v: u16) -> Option<NonZeroU16> {
        NonZeroU16::new(v)
    }

    fn bound_config(bindings: &[(RouteSlot, PeerCredential)]) -> PeerAuthConfig {
        let mut cfg = PeerAuthConfig {
            local_route_slot: nz(7),
            ..PeerAuthConfig::default()
        };
        for (route_slot, credential) in bindings {
            assert_eq!(
                cfg.pin_peer(*route_slot, credential.clone()),
                Ok(PinOutcome::Inserted)
            );
        }
        cfg
    }

    #[test]
    fn unconfigured_is_not_a_blanket_unverified_pass() {
        let snap = PeerAuthSnapshot::unconfigured();
        assert_eq!(
            snap.posture_for(RemoteIpClass::Loopback),
            Posture::Unverified
        );
        assert_eq!(
            snap.posture_for(RemoteIpClass::NonLoopback),
            Posture::Strict
        );
        assert_eq!(snap.posture_for(RemoteIpClass::Unknown), Posture::Strict);
        assert!(!snap.is_unverified_optout());
    }

    #[test]
    fn low_level_explicit_optout_is_unverified_regardless_of_endpoint() {
        let cfg = PeerAuthConfig {
            unverified_optout: true,
            ..PeerAuthConfig::default()
        };
        let snap = cfg.snapshot();
        for remote in [
            RemoteIpClass::Loopback,
            RemoteIpClass::NonLoopback,
            RemoteIpClass::Unknown,
        ] {
            assert_eq!(snap.posture_for(remote), Posture::Unverified);
        }
    }

    #[test]
    fn strict_bindings_force_strict_on_any_endpoint() {
        let snap = bound_config(&[(42, noise(0xAB))]).snapshot();
        for remote in [
            RemoteIpClass::Loopback,
            RemoteIpClass::NonLoopback,
            RemoteIpClass::Unknown,
        ] {
            assert_eq!(snap.posture_for(remote), Posture::Strict);
        }
    }

    #[test]
    fn authorize_binds_credential_to_route_slot() {
        let snap = bound_config(&[(42, noise(0xBB))]).snapshot();
        assert_eq!(
            snap.authorize(Posture::Strict, 42, Some(&noise(0xBB))),
            PeerAuthz::Authorized(42)
        );
        assert_eq!(
            snap.authorize(Posture::Strict, 42, Some(&noise(0xCC))),
            PeerAuthz::CredentialMismatch
        );
    }

    #[test]
    fn credential_lookup_resolves_route_slot_and_derived_node_id() {
        let credential = noise(0xBB);
        let snap = bound_config(&[(42, credential.clone())]).snapshot();
        let node_id = credential.node_id();

        assert_eq!(snap.route_slot_for_credential(&credential), Some(42));
        assert_eq!(snap.route_slot_for_node_id(node_id), Some(42));
        assert_eq!(snap.node_id_for_route_slot(42), Some(node_id));
    }

    #[test]
    fn authorize_reports_credential_bound_elsewhere() {
        let snap = bound_config(&[(42, noise(0xBB)), (43, noise(0xCC))]).snapshot();
        assert_eq!(
            snap.authorize(Posture::Strict, 42, Some(&noise(0xCC))),
            PeerAuthz::CredentialBoundElsewhere { route_slot: 43 }
        );
    }

    #[test]
    fn authorize_rejects_missing_credential_and_no_binding_and_zero() {
        let snap = bound_config(&[(42, noise(0xBB))]).snapshot();
        assert_eq!(
            snap.authorize(Posture::Strict, 42, None),
            PeerAuthz::MissingCredential
        );
        assert_eq!(
            snap.authorize(Posture::Strict, 99, Some(&noise(0xDD))),
            PeerAuthz::NoBindingForRoute
        );
        assert_eq!(
            snap.authorize(Posture::Strict, 0, Some(&noise(0xBB))),
            PeerAuthz::InvalidRouteSlot
        );
    }

    #[test]
    fn authorize_unverified_allows_none_credential() {
        let snap = PeerAuthSnapshot::unconfigured();
        assert_eq!(
            snap.authorize(Posture::Unverified, 42, None),
            PeerAuthz::Unverified
        );
    }

    #[test]
    fn route_slot_zero_is_rejected() {
        let mut cfg = PeerAuthConfig::default();
        assert_eq!(
            cfg.pin_peer(0, noise(0x01)),
            Err(PeerBindingError::ReservedRouteSlot)
        );
        assert!(cfg.bindings.is_empty());
    }

    #[test]
    fn same_slot_different_key_is_rejected() {
        let mut cfg = PeerAuthConfig::default();
        assert_eq!(cfg.pin_peer(42, noise(0x01)), Ok(PinOutcome::Inserted));
        assert_eq!(
            cfg.pin_peer(42, noise(0x02)),
            Err(PeerBindingError::RouteSlotAlreadyBound { route_slot: 42 })
        );
        assert_eq!(cfg.bindings.len(), 1);
    }

    #[test]
    fn same_key_different_slot_is_rejected() {
        let mut cfg = PeerAuthConfig::default();
        assert_eq!(cfg.pin_peer(42, noise(0x01)), Ok(PinOutcome::Inserted));
        assert_eq!(
            cfg.pin_peer(43, noise(0x01)),
            Err(PeerBindingError::CredentialAlreadyBound { route_slot: 42 })
        );
        assert_eq!(cfg.bindings.len(), 1);
    }

    #[test]
    fn repeated_identical_pin_is_idempotent() {
        let mut cfg = PeerAuthConfig::default();
        assert_eq!(cfg.pin_peer(42, noise(0x01)), Ok(PinOutcome::Inserted));
        assert_eq!(cfg.pin_peer(42, noise(0x01)), Ok(PinOutcome::Unchanged));
        assert_eq!(cfg.bindings.len(), 1);
    }

    #[test]
    fn injected_node_id_collision_is_rejected() {
        let collision = NodeId::from_bytes([0xCC; 16]);
        let mut cfg = PeerAuthConfig::default();
        assert_eq!(
            cfg.pin_peer_with_deriver(42, noise(0x01), |_| collision),
            Ok(PinOutcome::Inserted)
        );
        assert_eq!(
            cfg.pin_peer_with_deriver(43, noise(0x02), |_| collision),
            Err(PeerBindingError::NodeIdCollision {
                node_id: collision,
                route_slot: 42,
            })
        );
        assert_eq!(cfg.bindings.len(), 1);
    }

    #[test]
    fn snapshot_derives_mesh_spki_allowlist_from_bindings() {
        let mut cfg = bound_config(&[
            (42, PeerCredential::Spki(vec![1, 2, 3])),
            (43, PeerCredential::Spki(vec![4, 5, 6])),
        ]);
        cfg.transport = Some(TransportSelection::QuicMesh);
        let snap = cfg.snapshot();
        assert!(snap.mesh_spki_allowlist().contains(&vec![1, 2, 3]));
        assert!(snap.mesh_spki_allowlist().contains(&vec![4, 5, 6]));
        assert_eq!(snap.mesh_spki_allowlist().len(), 2);
    }

    #[test]
    fn noise_pubkey_allowlisted_reflects_bindings() {
        let snap = bound_config(&[(42, noise(0xEE))]).snapshot();
        assert!(snap.noise_pubkey_allowlisted(&[0xEE; NOISE_KEY_LEN]));
        assert!(!snap.noise_pubkey_allowlisted(&[0x00; NOISE_KEY_LEN]));
    }

    #[test]
    fn validate_public_rejects_bindings_without_loaded_identity() {
        let cfg = bound_config(&[(42, noise(0xBB))]);
        assert!(cfg.validate_public().is_err());
    }

    #[test]
    fn validate_public_rejects_optout_with_bindings() {
        let mut cfg = bound_config(&[(42, noise(0xBB))]);
        cfg.node_identity = Some(NodeId::from_bytes([0x11; 16]));
        cfg.unverified_optout = true;
        assert!(cfg.validate_public().is_err());
    }

    #[test]
    fn validate_snapshot_rejects_malformed_unverified_with_bindings() {
        let mut cfg = bound_config(&[(42, noise(0xBB))]);
        cfg.unverified_optout = true;
        let snap = cfg.snapshot();
        assert!(snap.validate().is_err());
    }

    #[test]
    fn validate_snapshot_accepts_unconfigured() {
        assert!(PeerAuthSnapshot::unconfigured().validate().is_ok());
    }

    #[test]
    fn identity_export_string_is_empty_without_identity() {
        assert_eq!(PeerAuthConfig::default().identity_export_string(), "");
    }

    #[test]
    fn identity_export_string_noise_pubkey_on_tcp() {
        let cfg = PeerAuthConfig {
            transport: Some(TransportSelection::Tcp),
            noise_identity: Some(StableNoiseIdentity::from_raw(
                [0xAB; NOISE_KEY_LEN],
                [0x01; NOISE_KEY_LEN],
            )),
            ..PeerAuthConfig::default()
        };
        assert_eq!(cfg.identity_export_string(), "ab".repeat(NOISE_KEY_LEN));
    }

    #[test]
    fn start_snapshot_holds_and_advances_session_lease() {
        let temp = tempfile::tempdir().unwrap();
        let identity_path = temp.path().join("node.key");
        let cfg = PeerAuthConfig {
            node_identity: Some(NodeId::from_bytes([0x44; 16])),
            identity_path: Some(identity_path),
            ..PeerAuthConfig::default()
        };

        let first = cfg.snapshot_for_start().unwrap();
        assert_eq!(first.node_identity(), cfg.node_identity);
        assert_eq!(first.session_incarnation(), Some(1));
        assert!(cfg.snapshot_for_start().is_err());
        drop(first);

        let second = cfg.snapshot_for_start().unwrap();
        assert_eq!(second.session_incarnation(), Some(2));
    }
}
