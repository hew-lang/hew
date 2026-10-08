//! Channel protection for an admitted node connection.
//!
//! Every admitted connection carries exactly one [`ChannelProtection`]: TCP
//! connections are protected by Noise-XX and quic-mesh connections by the
//! transport's mutually pinned TLS 1.3. There is no unprotected variant, so a
//! connection actor cannot exist without a channel and no send or read path
//! has a plaintext arm. [`establish`] is the only constructor reachable
//! outside tests, and it picks the protection from the transport's ops
//! identity, never from anything the peer sends.

use std::ffi::c_int;
use std::fmt;
use std::sync::Mutex;

use zeroize::Zeroizing;

use crate::peer_binding::{PeerAuthSnapshot, PeerCredential};
use crate::transport::HewTransport;
use crate::util::MutexExt;

use super::handshake::{send_frame, supports_encryption, upgrade_noise};
use super::{HewHandshake, NOISE_MAX_MSG_SIZE, NOISE_STATIC_PUBKEY_LEN};

/// Authentication tag `ChaChaPoly` appends to every Noise transport message.
const NOISE_TAG_LEN: usize = 16;

/// The protection every frame on one connection passes through.
pub(super) enum ChannelProtection {
    /// TCP: a Noise-XX channel.
    TcpNoise(NoiseChannel),
    /// quic-mesh: the transport's mutually pinned TLS 1.3 protects every byte.
    #[cfg(feature = "quic")]
    QuicMeshTls,
}

/// A Noise-XX transport channel with independent send and receive nonces, so
/// the reader never waits on a sender blocked in the transport.
pub(super) struct NoiseChannel {
    cipher: snow::StatelessTransportState,
    /// Held across seal and send so frames reach the wire in nonce order.
    send: Mutex<NoiseSend>,
    /// Next receiving nonce; only the connection's reader opens frames.
    recv_nonce: Mutex<u64>,
}

struct NoiseSend {
    next_nonce: u64,
    /// A send failed after its nonce was spent: the peer can no longer
    /// authenticate this channel's frames, and a nonce is never reused.
    faulted: bool,
}

impl NoiseChannel {
    fn new(cipher: snow::StatelessTransportState) -> Self {
        Self {
            cipher,
            send: Mutex::new(NoiseSend {
                next_nonce: 0,
                faulted: false,
            }),
            recv_nonce: Mutex::new(0),
        }
    }
}

/// Which protection a transport provides, decided by its ops identity.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ChannelKind {
    TcpNoise,
    #[cfg(feature = "quic")]
    QuicMeshTls,
}

/// Why a connection could not establish a protected channel.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ChannelRefusal {
    /// The transport has no channel protection (plain QUIC, Unix sockets,
    /// stubs); node traffic is never carried over it.
    UnprotectedTransport,
    /// This node has no stable Noise identity to authenticate a TCP channel.
    NoLocalNoiseIdentity,
    /// The TCP peer does not speak Noise; node traffic is never plaintext.
    PlaintextPeer,
    /// The Noise-XX handshake failed or could not pick roles.
    NoiseHandshake,
    /// The peer's authenticated Noise key is not pinned on this node.
    KeyNotPinned,
    /// The authenticated Noise key differs from the key in the peer's record.
    KeyMismatch,
    /// A quic-mesh peer's record carried a Noise key.
    MeshRecordCarriesNoiseKey,
    /// The quic-mesh transport has no authenticated peer certificate.
    MeshCredentialUnavailable,
}

impl fmt::Display for ChannelRefusal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::UnprotectedTransport => {
                "transport has no channel protection (use tcp-noise or quic-mesh)"
            }
            Self::NoLocalNoiseIdentity => "no stable local Noise identity is loaded",
            Self::PlaintextPeer => "peer does not offer Noise; plaintext node channels are refused",
            Self::NoiseHandshake => "noise handshake failed",
            Self::KeyNotPinned => "peer Noise key is not pinned",
            Self::KeyMismatch => "authenticated Noise key does not match the v2 handshake key",
            Self::MeshRecordCarriesNoiseKey => {
                "quic-mesh v2 handshake carried a non-zero Noise key"
            }
            Self::MeshCredentialUnavailable => "quic-mesh peer certificate is unavailable",
        })
    }
}

impl std::error::Error for ChannelRefusal {}

/// Why one frame could not pass through the channel.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ChannelError {
    /// The frame does not fit one Noise transport message.
    FrameTooLarge { len: usize },
    /// Noise refused to encrypt the frame.
    Seal,
    /// An earlier send failed after spending its nonce; the channel is dead.
    Faulted,
    /// The transport did not accept the whole sealed frame.
    Transport,
    /// The received message failed authentication.
    Open,
}

impl fmt::Display for ChannelError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FrameTooLarge { len } => write!(
                f,
                "frame of {len} bytes exceeds the {} byte channel limit",
                NOISE_MAX_MSG_SIZE - NOISE_TAG_LEN
            ),
            Self::Seal => f.write_str("channel encryption failed"),
            Self::Faulted => f.write_str("channel faulted after a failed send"),
            Self::Transport => f.write_str("transport did not accept the sealed frame"),
            Self::Open => f.write_str("channel message failed authentication"),
        }
    }
}

impl std::error::Error for ChannelError {}

impl ChannelProtection {
    /// Seal one frame and send it whole on `conn_id`.
    ///
    /// # Safety
    ///
    /// `transport` must be valid and `conn_id` must name the connection this
    /// channel protects.
    pub(super) unsafe fn send(
        &self,
        transport: *mut HewTransport,
        conn_id: c_int,
        frame: &[u8],
    ) -> Result<(), ChannelError> {
        match self {
            Self::TcpNoise(noise) => {
                if frame.len() + NOISE_TAG_LEN > NOISE_MAX_MSG_SIZE {
                    return Err(ChannelError::FrameTooLarge { len: frame.len() });
                }
                let mut send = noise.send.lock_or_recover();
                if send.faulted {
                    return Err(ChannelError::Faulted);
                }
                let mut sealed = vec![0u8; frame.len() + NOISE_TAG_LEN];
                let n = noise
                    .cipher
                    .write_message(send.next_nonce, frame, &mut sealed)
                    .map_err(|_| ChannelError::Seal)?;
                send.next_nonce += 1;
                // SAFETY: forwarded caller contract.
                if unsafe { send_frame(transport, conn_id, &sealed[..n]) } {
                    Ok(())
                } else {
                    send.faulted = true;
                    Err(ChannelError::Transport)
                }
            }
            #[cfg(feature = "quic")]
            Self::QuicMeshTls => {
                // SAFETY: forwarded caller contract.
                if unsafe { send_frame(transport, conn_id, frame) } {
                    Ok(())
                } else {
                    Err(ChannelError::Transport)
                }
            }
        }
    }

    /// Recover one inbound frame in place; returns its length.
    pub(super) fn open(&self, message: &mut [u8]) -> Result<usize, ChannelError> {
        match self {
            Self::TcpNoise(noise) => {
                let mut plaintext = vec![0u8; message.len()];
                let mut nonce = noise.recv_nonce.lock_or_recover();
                let n = noise
                    .cipher
                    .read_message(*nonce, message, &mut plaintext)
                    .map_err(|_| ChannelError::Open)?;
                *nonce += 1;
                message[..n].copy_from_slice(&plaintext[..n]);
                Ok(n)
            }
            #[cfg(feature = "quic")]
            Self::QuicMeshTls => Ok(message.len()),
        }
    }

    /// A connected Noise pair for tests that record or inject frames: the
    /// first half protects the connection under test and the second half is
    /// the peer's, used to read what the connection sent.
    #[cfg(test)]
    pub(super) fn test_noise_pair() -> (Self, snow::TransportState) {
        let pattern: snow::params::NoiseParams = super::NOISE_PATTERN.parse().expect("pattern");
        let initiator_key = snow::Builder::new(pattern.clone())
            .generate_keypair()
            .expect("initiator keypair");
        let responder_key = snow::Builder::new(pattern.clone())
            .generate_keypair()
            .expect("responder keypair");
        let mut initiator = snow::Builder::new(pattern.clone())
            .local_private_key(&initiator_key.private)
            .expect("initiator key")
            .build_initiator()
            .expect("initiator");
        let mut responder = snow::Builder::new(pattern)
            .local_private_key(&responder_key.private)
            .expect("responder key")
            .build_responder()
            .expect("responder");
        let mut message = vec![0u8; NOISE_MAX_MSG_SIZE];
        let mut payload = vec![0u8; NOISE_MAX_MSG_SIZE];
        let n = initiator.write_message(&[], &mut message).expect("xx 1");
        responder
            .read_message(&message[..n], &mut payload)
            .expect("xx 1 read");
        let n = responder.write_message(&[], &mut message).expect("xx 2");
        initiator
            .read_message(&message[..n], &mut payload)
            .expect("xx 2 read");
        let n = initiator.write_message(&[], &mut message).expect("xx 3");
        responder
            .read_message(&message[..n], &mut payload)
            .expect("xx 3 read");
        (
            Self::TcpNoise(NoiseChannel::new(
                initiator
                    .into_stateless_transport_mode()
                    .expect("initiator transport"),
            )),
            responder
                .into_transport_mode()
                .expect("responder transport"),
        )
    }
}

/// The protection a transport provides, or `None` when it provides none.
///
/// # Safety
///
/// `transport` must be null or valid for the duration of the call.
pub(super) unsafe fn channel_kind(transport: *const HewTransport) -> Option<ChannelKind> {
    // SAFETY: forwarded caller contract.
    if unsafe { crate::transport::hew_transport_is_tcp(transport) } {
        return Some(ChannelKind::TcpNoise);
    }
    #[cfg(feature = "quic")]
    // SAFETY: forwarded caller contract; the check is null-safe.
    if unsafe { crate::quic_mesh::hew_transport_is_quic_mesh(transport) } {
        return Some(ChannelKind::QuicMeshTls);
    }
    None
}

/// The static key this node presents in its v2 record, and the private half
/// the Noise handshake uses. quic-mesh presents a zero key and no private key.
pub(super) fn local_channel_key(
    kind: ChannelKind,
    auth: &PeerAuthSnapshot,
) -> Result<([u8; NOISE_STATIC_PUBKEY_LEN], Zeroizing<Vec<u8>>), ChannelRefusal> {
    match kind {
        ChannelKind::TcpNoise => {
            let identity = auth
                .noise_identity()
                .ok_or(ChannelRefusal::NoLocalNoiseIdentity)?;
            Ok((
                identity.public(),
                Zeroizing::new(identity.private().to_vec()),
            ))
        }
        #[cfg(feature = "quic")]
        ChannelKind::QuicMeshTls => Ok(([0; NOISE_STATIC_PUBKEY_LEN], Zeroizing::new(Vec::new()))),
    }
}

/// Establish the channel for a connection whose v2 records have been
/// exchanged, returning its protection and the peer credential it
/// authenticated.
///
/// # Safety
///
/// `transport` and `conn_id` must be valid for the duration of the call.
pub(super) unsafe fn establish(
    kind: ChannelKind,
    transport: *mut HewTransport,
    conn_id: c_int,
    auth: &PeerAuthSnapshot,
    local: &HewHandshake,
    peer: &HewHandshake,
    local_private_key: &[u8],
) -> Result<(ChannelProtection, PeerCredential), ChannelRefusal> {
    match kind {
        ChannelKind::TcpNoise => {
            if !supports_encryption(peer.feature_flags) {
                return Err(ChannelRefusal::PlaintextPeer);
            }
            // SAFETY: forwarded caller contract; the records are stack-local.
            let (noise, peer_static_key) =
                unsafe { upgrade_noise(transport, conn_id, local, peer, local_private_key) }
                    .ok_or(ChannelRefusal::NoiseHandshake)?;
            // Per-node pre-gate (issue #2652, D14): the authenticated key must
            // be pinned in this node's snapshot. Claim reservation later binds
            // it to the claimed NodeId.
            if !auth.noise_pubkey_allowlisted(&peer_static_key) {
                return Err(ChannelRefusal::KeyNotPinned);
            }
            if peer.static_noise_pubkey != peer_static_key {
                return Err(ChannelRefusal::KeyMismatch);
            }
            Ok((
                ChannelProtection::TcpNoise(NoiseChannel::new(noise)),
                PeerCredential::NoiseKey(peer_static_key),
            ))
        }
        #[cfg(feature = "quic")]
        ChannelKind::QuicMeshTls => {
            if peer.static_noise_pubkey != [0; NOISE_STATIC_PUBKEY_LEN] {
                return Err(ChannelRefusal::MeshRecordCarriesNoiseKey);
            }
            // The mTLS handshake already pinned the peer's leaf SPKI (D6);
            // binding it ties the claimed NodeId to that key.
            // SAFETY: forwarded caller contract.
            let spki =
                unsafe { crate::quic_mesh::hew_transport_quic_mesh_peer_spki(transport, conn_id) }
                    .ok_or(ChannelRefusal::MeshCredentialUnavailable)?;
            Ok((ChannelProtection::QuicMeshTls, PeerCredential::Spki(spki)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A transport whose sends fail while `fail` is set and are recorded
    /// otherwise.
    struct RecordingTransport {
        fail: std::sync::atomic::AtomicBool,
        sent: Mutex<Vec<Vec<u8>>>,
    }

    unsafe extern "C" fn record_send(
        impl_ptr: *mut std::ffi::c_void,
        _conn_id: c_int,
        data: *const std::ffi::c_void,
        len: usize,
    ) -> c_int {
        // SAFETY: the test installs a RecordingTransport as the impl payload.
        let state = unsafe { &*impl_ptr.cast::<RecordingTransport>() };
        if state.fail.load(std::sync::atomic::Ordering::Acquire) {
            return -1;
        }
        // SAFETY: send passes a sealed frame valid for len bytes.
        let bytes = unsafe { std::slice::from_raw_parts(data.cast::<u8>(), len) };
        state.sent.lock_or_recover().push(bytes.to_vec());
        c_int::try_from(len).expect("test frame fits c_int")
    }

    static RECORD_OPS: crate::transport::HewTransportOps = crate::transport::HewTransportOps {
        connect: None,
        listen: None,
        accept: None,
        send: Some(record_send),
        recv: None,
        close_conn: None,
        destroy: None,
    };

    fn recording_transport() -> (Box<RecordingTransport>, HewTransport) {
        let mut state = Box::new(RecordingTransport {
            fail: std::sync::atomic::AtomicBool::new(false),
            sent: Mutex::new(Vec::new()),
        });
        let transport = HewTransport {
            ops: &raw const RECORD_OPS,
            r#impl: std::ptr::from_mut(&mut *state).cast(),
        };
        (state, transport)
    }

    #[test]
    fn noise_send_open_round_trips_and_hides_the_frame() {
        let (local, mut peer) = ChannelProtection::test_noise_pair();
        let (state, mut transport) = recording_transport();
        let frame = b"a frame that must never travel in the clear";
        // SAFETY: the recording transport is live for the call.
        unsafe { local.send(&raw mut transport, 1, frame) }.expect("send");
        let sealed = state
            .sent
            .lock_or_recover()
            .pop()
            .expect("one sealed frame");
        assert!(
            !sealed.windows(frame.len()).any(|w| w == frame),
            "sealed bytes must not contain the frame"
        );
        let mut opened = vec![0u8; sealed.len()];
        let n = peer.read_message(&sealed, &mut opened).expect("peer opens");
        assert_eq!(&opened[..n], frame);

        let mut reply = vec![0u8; frame.len() + NOISE_TAG_LEN];
        let n = peer.write_message(frame, &mut reply).expect("peer seals");
        reply.truncate(n);
        let len = local.open(&mut reply).expect("open");
        assert_eq!(&reply[..len], frame);
    }

    #[test]
    fn noise_open_refuses_a_tampered_message() {
        let (local, mut peer) = ChannelProtection::test_noise_pair();
        let mut message = vec![0u8; 64 + NOISE_TAG_LEN];
        let n = peer.write_message(&[7u8; 64], &mut message).expect("seal");
        message.truncate(n);
        message[3] ^= 1;
        assert_eq!(local.open(&mut message), Err(ChannelError::Open));
    }

    #[test]
    fn noise_send_refuses_a_frame_over_the_message_limit() {
        let (local, _peer) = ChannelProtection::test_noise_pair();
        let (_state, mut transport) = recording_transport();
        let limit = NOISE_MAX_MSG_SIZE - NOISE_TAG_LEN;
        // SAFETY: the recording transport is live for both calls.
        unsafe {
            assert!(local.send(&raw mut transport, 1, &vec![0u8; limit]).is_ok());
            assert_eq!(
                local.send(&raw mut transport, 1, &vec![0u8; limit + 1]),
                Err(ChannelError::FrameTooLarge { len: limit + 1 })
            );
        }
    }

    #[test]
    fn noise_send_failure_faults_the_channel_without_reusing_a_nonce() {
        let (local, _peer) = ChannelProtection::test_noise_pair();
        let (state, mut transport) = recording_transport();
        state.fail.store(true, std::sync::atomic::Ordering::Release);
        // SAFETY: the recording transport is live for every call.
        unsafe {
            assert_eq!(
                local.send(&raw mut transport, 1, b"first"),
                Err(ChannelError::Transport)
            );
            state
                .fail
                .store(false, std::sync::atomic::Ordering::Release);
            assert_eq!(
                local.send(&raw mut transport, 1, b"second"),
                Err(ChannelError::Faulted)
            );
        }
        assert!(
            state.sent.lock_or_recover().is_empty(),
            "a faulted channel sends nothing"
        );
    }

    #[test]
    fn concurrent_noise_sends_reach_the_wire_in_nonce_order() {
        let (local, mut peer) = ChannelProtection::test_noise_pair();
        let (state, transport) = recording_transport();
        let local = std::sync::Arc::new(local);
        let transport_addr = std::ptr::from_ref(&transport) as usize;
        let senders: Vec<_> = (0..4u8)
            .map(|id| {
                let local = std::sync::Arc::clone(&local);
                std::thread::spawn(move || {
                    for _ in 0..50 {
                        // SAFETY: the transport outlives every sender thread.
                        unsafe { local.send(transport_addr as *mut HewTransport, 1, &[id; 32]) }
                            .expect("send");
                    }
                })
            })
            .collect();
        for sender in senders {
            sender.join().expect("sender thread");
        }
        let sent = state.sent.lock_or_recover();
        assert_eq!(sent.len(), 200);
        let mut opened = vec![0u8; 64];
        for sealed in sent.iter() {
            peer.read_message(sealed, &mut opened)
                .expect("every frame opens in wire order");
        }
    }

    #[test]
    fn only_tcp_and_quic_mesh_transports_have_a_channel() {
        // SAFETY: null is accepted by every ops-identity check.
        assert_eq!(unsafe { channel_kind(std::ptr::null()) }, None);
        let stub = HewTransport {
            ops: std::ptr::null(),
            r#impl: std::ptr::null_mut(),
        };
        // SAFETY: the stub transport is live for the call.
        assert_eq!(unsafe { channel_kind(&raw const stub) }, None);
    }
}
