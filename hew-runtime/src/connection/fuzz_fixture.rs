//! In-process control-plane fixture for `fuzz_control_handler`.
//!
//! One node (route slot 1) with a cluster and a connection manager whose
//! transport records sends. Three peers are connected: slots 2 and 3 on
//! admitted, published `Strict` connections and slot 4 on an `Unverified`
//! delivery-only connection. Frames are delivered exactly as the reader
//! delivers them: wire-frame decode, then the control handler, then the parked
//! registry-flush retry.

use std::ffi::{c_int, c_void};
use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex, OnceLock};

use super::channel::ChannelProtection;
use super::control::handle_control_frame;
use super::gossip::retry_pending_registry_flush;
use super::identity_claim::{test_node_identity, test_publish_claim};
use super::{
    hew_connmgr_free, hew_connmgr_new, ConnectionActor, HewConnMgr, HewHandshake,
    CONN_STATE_ACTIVE, HEW_FEATURE_SUPPORTS_GOSSIP, NOISE_STATIC_PUBKEY_LEN,
};
use crate::cluster::{
    hew_cluster_free, hew_cluster_new,
    hew_cluster_notify_connection_established_for_token_if_not_removed, HewCluster,
};
use crate::envelope::{decode_wire_frame, NodeSessionIdentity, WireFrame};
use crate::peer_binding::Posture;
use crate::transport::{HewTransport, HewTransportOps};
use crate::util::MutexExt;

/// The local route slot.
const LOCAL_SLOT: u16 = 1;
/// `(route slot, connection id, posture)` for each connected peer.
const PEERS: [(u16, c_int, Posture); 3] = [
    (2, 10, Posture::Strict),
    (3, 11, Posture::Strict),
    (4, 12, Posture::Unverified),
];
/// Number of connected peers a fuzz input can deliver from.
pub const HARNESS_PEERS: usize = PEERS.len();

/// One member as the local cluster sees it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Member {
    pub slot: u16,
    pub session: Option<u32>,
    pub state: i32,
    pub incarnation: Option<u64>,
}

/// Bytes the manager sent, per connection.
type Sends = Mutex<Vec<(c_int, usize)>>;

unsafe extern "C" fn record_send(
    impl_ptr: *mut c_void,
    conn_id: c_int,
    _data: *const c_void,
    len: usize,
) -> c_int {
    // SAFETY: the harness installs its `Sends` as the transport impl and
    // frees it only after the manager.
    let sends = unsafe { &*impl_ptr.cast::<Sends>() };
    sends.lock_or_recover().push((conn_id, len));
    c_int::try_from(len).unwrap_or(c_int::MAX)
}

static SEND_OPS: HewTransportOps = HewTransportOps {
    connect: None,
    listen: None,
    accept: None,
    send: Some(record_send),
    recv: None,
    close_conn: None,
    destroy: None,
};

/// Noise key generation and the XX handshake dominate fixture setup, and the
/// channel only seals what the harness never reads, so every harness shares
/// one channel.
fn shared_channel() -> Arc<ChannelProtection> {
    static CHANNEL: OnceLock<Arc<ChannelProtection>> = OnceLock::new();
    Arc::clone(CHANNEL.get_or_init(|| Arc::new(ChannelProtection::test_noise_pair().0)))
}

/// A connected node that control frames are delivered to.
#[derive(Debug)]
pub struct ControlHarness {
    cluster: *mut HewCluster,
    mgr: *mut HewConnMgr,
    transport: *mut HewTransport,
    sends: *mut Sends,
    tokens: [u64; HARNESS_PEERS],
}

impl ControlHarness {
    /// Build the node and connect its peers.
    ///
    /// # Panics
    /// Panics when the cluster or manager cannot be created.
    #[must_use]
    pub fn new() -> Self {
        let sends = Box::into_raw(Box::new(Sends::default()));
        let transport = Box::into_raw(Box::new(HewTransport {
            ops: &raw const SEND_OPS,
            r#impl: sends.cast(),
        }));
        let config = crate::cluster::ClusterConfig {
            local_node_id: LOCAL_SLOT,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: config is valid for the call.
        let cluster = unsafe { hew_cluster_new(&raw const config) };
        assert!(!cluster.is_null(), "fuzz cluster");
        // SAFETY: transport and cluster stay live until the harness drops.
        let mgr =
            unsafe { hew_connmgr_new(transport, None, std::ptr::null_mut(), cluster, LOCAL_SLOT) };
        assert!(!mgr.is_null(), "fuzz connection manager");
        // SAFETY: the harness owns the manager and nothing else references it yet.
        let mgr_mut = unsafe { &mut *mgr };
        mgr_mut.local_identity = Some(test_node_identity(LOCAL_SLOT));
        mgr_mut.local_session_incarnation = Some(1);

        let mut tokens = [0; HARNESS_PEERS];
        for (token, &(slot, conn_id, posture)) in tokens.iter_mut().zip(&PEERS) {
            let mut actor = ConnectionActor::new(conn_id, shared_channel());
            actor.peer_node_id = slot;
            actor.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
            actor.posture = posture;
            actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            let publication_sync = Arc::clone(&actor.publication_sync);
            let publication_removed = Arc::clone(&actor.publication_removed);
            mgr_mut.connections.access(|conns| conns.push(actor));
            if posture == Posture::Strict {
                // Admission publishes the claim, then the session to the
                // cluster; an `Unverified` peer gets neither.
                *token = test_publish_claim(mgr_mut, slot, conn_id);
                // SAFETY: cluster is live; the publication guards belong to
                // the installed connection.
                let published = unsafe {
                    hew_cluster_notify_connection_established_for_token_if_not_removed(
                        cluster,
                        slot,
                        1,
                        *token,
                        &publication_sync,
                        &publication_removed,
                    )
                };
                assert!(published >= 0, "fuzz peer {slot} publishes");
            }
        }
        Self {
            cluster,
            mgr,
            transport,
            sends,
            tokens,
        }
    }

    /// The local node's identity and session.
    #[must_use]
    pub fn local_session() -> NodeSessionIdentity {
        session(LOCAL_SLOT)
    }

    /// The identity and session peer `index` authenticated as.
    #[must_use]
    pub fn peer_session(index: usize) -> NodeSessionIdentity {
        session(PEERS[index % HARNESS_PEERS].0)
    }

    /// The route slot a harness identity is admitted on.
    #[must_use]
    pub fn slot_of(node_id: crate::node_identity::NodeId) -> Option<u16> {
        Self::slots()
            .into_iter()
            .find(|&slot| test_node_identity(slot) == node_id)
    }

    /// Route slots of the local node and every peer.
    #[must_use]
    pub fn slots() -> Vec<u16> {
        std::iter::once(LOCAL_SLOT)
            .chain(PEERS.iter().map(|peer| peer.0))
            .collect()
    }

    /// Deliver one wire frame from peer `index` and return how many frames
    /// the node sent in response.
    pub fn deliver(&mut self, index: usize, bytes: &[u8]) -> usize {
        let index = index % HARNESS_PEERS;
        let conn_id = PEERS[index].1;
        let token = self.tokens[index];
        let before = self.sent();
        // The reader skips undecodable frames and envelopes without a router.
        if let Ok(WireFrame::Control(control)) = decode_wire_frame(bytes) {
            handle_control_frame(
                self.mgr,
                HEW_FEATURE_SUPPORTS_GOSSIP,
                conn_id,
                token,
                &control,
            );
        }
        // SAFETY: the manager is live for the harness lifetime.
        retry_pending_registry_flush(unsafe { &*self.mgr }, conn_id, token);
        self.sent() - before
    }

    fn sent(&self) -> usize {
        // SAFETY: sends outlives the manager.
        unsafe { &*self.sends }.lock_or_recover().len()
    }

    /// Number of members in the local table.
    #[must_use]
    pub fn member_count(&self) -> usize {
        // SAFETY: the cluster is live for the harness lifetime.
        let count = unsafe { crate::cluster::hew_cluster_member_count(self.cluster) };
        usize::try_from(count).unwrap_or(usize::MAX)
    }

    /// The local view of the node and its peers.
    #[must_use]
    pub fn members(&self) -> Vec<Member> {
        // SAFETY: the cluster is live for the harness lifetime.
        let cluster = unsafe { &*self.cluster };
        Self::slots()
            .into_iter()
            .map(|slot| Member {
                slot,
                session: cluster.member_session(slot),
                state: cluster.member_state(slot),
                incarnation: cluster.member_incarnation(slot),
            })
            .collect()
    }

    /// The local node's own incarnation.
    #[must_use]
    pub fn local_incarnation(&self) -> u64 {
        // SAFETY: the cluster is live for the harness lifetime.
        unsafe { &*self.cluster }.local_incarnation()
    }
}

impl Default for ControlHarness {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for ControlHarness {
    fn drop(&mut self) {
        // SAFETY: the harness owns every pointer; the manager goes first
        // because it borrows the transport and cluster.
        unsafe {
            hew_connmgr_free(self.mgr);
            hew_cluster_free(self.cluster);
            drop(Box::from_raw(self.transport));
            drop(Box::from_raw(self.sends));
        }
    }
}

fn session(slot: u16) -> NodeSessionIdentity {
    NodeSessionIdentity {
        node_id: test_node_identity(slot),
        session_incarnation: 1,
    }
}

/// Handshake round trip: an accepted record re-serializes to a fixed point.
pub(crate) fn check_handshake(bytes: &[u8]) {
    let Some(handshake) = HewHandshake::deserialize(bytes) else {
        return;
    };
    let encoded = handshake.serialize();
    let again = HewHandshake::deserialize(&encoded)
        .unwrap_or_else(|| panic!("re-encoded handshake is refused: {handshake:?}"));
    assert_eq!(again, handshake, "re-encoded handshake decodes differently");
    assert_eq!(
        again.serialize(),
        encoded,
        "handshake re-encoding is not stable"
    );
}

pub(crate) fn handshake_seeds() -> Vec<Vec<u8>> {
    let local = super::handshake::local_handshake(
        test_node_identity(2),
        1,
        [0x42; NOISE_STATIC_PUBKEY_LEN],
    );
    let mut enrolled = local.clone();
    enrolled.enrollment = vec![0xa3, 0x01, 0x40, 0x02, 0x40, 0x03, 0x40];
    vec![local.serialize(), enrolled.serialize()]
}
