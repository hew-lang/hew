//! Protocol handshake, framing and the Noise upgrade.

use std::ffi::c_int;

use sha2::{Digest, Sha256};
use zeroize::Zeroizing;

use crate::envelope::{FRAME_TYPE_CONTROL, FRAME_TYPE_ENVELOPE, WIRE_VERSION};
use crate::node_identity::NodeId;
use crate::set_last_error;
use crate::transport::HewTransport;

use super::channel::ChannelProtection;
use super::{
    HewHandshake, FNV1A32_OFFSET_BASIS, FNV1A32_PRIME, HEW_FEATURE_SUPPORTS_GOSSIP,
    MAX_HANDSHAKE_BYTES, NOISE_STATIC_PUBKEY_LEN,
};
use super::{
    HEW_FEATURE_RESPONDER_COMMIT, HEW_FEATURE_SUPPORTS_ENCRYPTION, NOISE_MAX_MSG_SIZE,
    NOISE_PATTERN,
};

fn local_feature_flags() -> u32 {
    HEW_FEATURE_SUPPORTS_ENCRYPTION | HEW_FEATURE_SUPPORTS_GOSSIP | HEW_FEATURE_RESPONDER_COMMIT
}

/// Whether both records advertise the responder-commit step. The records are
/// bound into the channel's handshake transcript, so neither end can be talked
/// out of it.
pub(super) fn negotiates_responder_commit(local: &HewHandshake, peer: &HewHandshake) -> bool {
    local.feature_flags & peer.feature_flags & HEW_FEATURE_RESPONDER_COMMIT != 0
}

/// Send the responder's commit, or the initiator's echo of it: the first
/// frame from that end on the protected channel, carrying the order in which
/// the responder reserved this connection.
///
/// # Safety
///
/// `transport` is live and `conn_id` names the connection `channel` protects.
pub(super) unsafe fn send_commit(
    channel: &ChannelProtection,
    transport: *mut HewTransport,
    conn_id: c_int,
    order: u64,
) -> bool {
    // SAFETY: forwarded caller contract.
    unsafe { channel.send_prepared(transport, conn_id, || Some(order.to_be_bytes())) }.is_ok()
}

/// Receive the peer's commit or echo, or `None` when the peer closed the
/// connection instead (it yielded, refused or gave up) or sent anything else.
///
/// # Safety
///
/// `transport` is live and `conn_id` names the connection `channel` protects;
/// no reader has started on it.
pub(super) unsafe fn recv_commit(
    channel: &ChannelProtection,
    transport: *mut HewTransport,
    conn_id: c_int,
) -> Option<u64> {
    // SAFETY: forwarded caller contract.
    let transport = unsafe { transport.as_ref() }?;
    // SAFETY: a live transport owns its operations table.
    let recv = unsafe { transport.ops.as_ref() }?.recv?;
    let mut message = vec![0u8; NOISE_MAX_MSG_SIZE];
    // SAFETY: the buffer is writable for its advertised capacity.
    let length = unsafe {
        recv(
            transport.r#impl,
            conn_id,
            message.as_mut_ptr().cast(),
            message.len(),
        )
    };
    let length = usize::try_from(length)
        .ok()
        .filter(|&length| length > 0 && length <= message.len())?;
    let opened = channel.open(&mut message[..length]).ok()?;
    let order = u64::from_be_bytes(message[..opened].try_into().ok()?);
    (order != 0).then_some(order)
}

pub(crate) fn supports_gossip(flags: u32) -> bool {
    flags & HEW_FEATURE_SUPPORTS_GOSSIP != 0
}

pub(super) fn local_schema_hash() -> u32 {
    fn fnv1a32_update(mut hash: u32, bytes: &[u8]) -> u32 {
        for &byte in bytes {
            hash ^= u32::from(byte);
            hash = hash.wrapping_mul(FNV1A32_PRIME);
        }
        hash
    }

    let mut hash = FNV1A32_OFFSET_BASIS;
    hash = fnv1a32_update(hash, b"CBOR-ENVELOPE");
    hash = fnv1a32_update(hash, &[WIRE_VERSION]);
    hash = fnv1a32_update(hash, &[FRAME_TYPE_CONTROL, FRAME_TYPE_ENVELOPE]);
    fnv1a32_update(hash, &[1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
}

pub(super) fn local_handshake(
    local_node_id: NodeId,
    session_incarnation: u32,
    static_noise_pubkey: [u8; NOISE_STATIC_PUBKEY_LEN],
) -> HewHandshake {
    HewHandshake {
        protocol_version: u16::from(WIRE_VERSION),
        schema_hash: local_schema_hash(),
        feature_flags: local_feature_flags(),
        node_id: local_node_id,
        session_incarnation,
        static_noise_pubkey,
        enrollment: Vec::new(),
    }
}

/// A genuine checked handshake record for a raw-socket test peer, optionally without the
/// Noise capability so the peer presents itself as plaintext.
#[cfg(test)]
pub(crate) fn test_handshake_record(
    node_id: NodeId,
    session_incarnation: u32,
    static_noise_pubkey: [u8; NOISE_STATIC_PUBKEY_LEN],
    offers_noise: bool,
) -> Vec<u8> {
    let mut record = local_handshake(node_id, session_incarnation, static_noise_pubkey);
    if !offers_noise {
        record.feature_flags &= !HEW_FEATURE_SUPPORTS_ENCRYPTION;
    }
    record.serialize()
}

pub(super) fn version_compatible(local: &HewHandshake, peer: &HewHandshake) -> bool {
    local.protocol_version == peer.protocol_version
}

pub(super) fn schema_compatible(local: &HewHandshake, peer: &HewHandshake) -> bool {
    local.schema_hash == peer.schema_hash
}

pub(super) fn peer_identity_compatible(local_node_id: NodeId, peer_node_id: NodeId) -> bool {
    peer_node_id != local_node_id
}

pub(super) unsafe fn send_frame(
    transport: *mut HewTransport,
    conn_id: c_int,
    payload: &[u8],
) -> bool {
    if transport.is_null() {
        return false;
    }
    // SAFETY: transport pointer validity is guaranteed by caller.
    let t = unsafe { &*transport };
    // SAFETY: vtable pointer validity is guaranteed by transport construction.
    let Some(ops) = (unsafe { t.ops.as_ref() }) else {
        return false;
    };
    let Some(send_fn) = ops.send else {
        return false;
    };
    let Ok(expected) = c_int::try_from(payload.len()) else {
        return false;
    };
    // SAFETY: payload pointer is valid for payload.len() bytes.
    unsafe { send_fn(t.r#impl, conn_id, payload.as_ptr().cast(), payload.len()) == expected }
}

unsafe fn handshake_send(
    transport: *mut HewTransport,
    conn_id: c_int,
    handshake: &HewHandshake,
) -> c_int {
    // SAFETY: transport and conn_id are validated by caller; serialization produces one checked metadata frame.
    -c_int::from(!unsafe { send_frame(transport, conn_id, &handshake.serialize()) })
}

unsafe fn handshake_recv(transport: *mut HewTransport, conn_id: c_int) -> Option<HewHandshake> {
    let mut buf = vec![0u8; MAX_HANDSHAKE_BYTES];
    // SAFETY: caller pins the transport and connection during admission.
    let transport = unsafe { transport.as_ref() }?;
    // SAFETY: a live transport owns its operations table.
    let recv = unsafe { transport.ops.as_ref() }?.recv?;
    // SAFETY: buf is writable for its advertised bounded capacity.
    let length = unsafe {
        recv(
            transport.r#impl,
            conn_id,
            buf.as_mut_ptr().cast(),
            buf.len(),
        )
    };
    let length = usize::try_from(length).ok()?;
    if length == 0 || length > buf.len() {
        return None;
    }
    buf.truncate(length);
    let Some(handshake) = HewHandshake::deserialize(&buf) else {
        set_last_error(format!(
            "hew_connmgr_add: invalid handshake payload for conn {conn_id}"
        ));
        return None;
    };
    Some(handshake)
}

pub(super) unsafe fn handshake_exchange(
    transport: *mut HewTransport,
    conn_id: c_int,
    local: &HewHandshake,
) -> Option<HewHandshake> {
    // SAFETY: transport and conn_id validated by caller contract.
    if unsafe { handshake_send(transport, conn_id, local) } != 0 {
        set_last_error(format!(
            "hew_connmgr_add: failed to send handshake for conn {conn_id}"
        ));
        return None;
    }
    // SAFETY: same contract — transport remains valid through handshake sequence.
    let peer = unsafe { handshake_recv(transport, conn_id) }?;
    if !version_compatible(local, &peer) {
        set_last_error(format!(
            "hew_connmgr_add: handshake protocol mismatch for conn {conn_id} (local={}, peer={})",
            local.protocol_version, peer.protocol_version
        ));
        return None;
    }
    if !schema_compatible(local, &peer) {
        set_last_error(format!(
            "hew_connmgr_add: handshake schema hash mismatch for conn {conn_id} (local={:#010x}, peer={:#010x})",
            local.schema_hash, peer.schema_hash
        ));
        return None;
    }
    Some(peer)
}

pub(super) unsafe fn close_transport_conn(transport: *mut HewTransport, conn_id: c_int) {
    if transport.is_null() {
        return;
    }
    // SAFETY: transport pointer validity is guaranteed by caller.
    let t = unsafe { &*transport };
    // SAFETY: vtable pointer validity is guaranteed by transport construction.
    if let Some(ops) = unsafe { t.ops.as_ref() } {
        if let Some(close_fn) = ops.close_conn {
            // SAFETY: conn_id is a transport-provided handle.
            unsafe { close_fn(t.r#impl, conn_id) };
        }
    }
}

pub(super) fn supports_encryption(flags: u32) -> bool {
    flags & HEW_FEATURE_SUPPORTS_ENCRYPTION != 0
}

fn noise_is_initiator(local: &HewHandshake, peer: &HewHandshake) -> Option<bool> {
    if local.node_id != peer.node_id {
        return Some(local.node_id.to_bytes() < peer.node_id.to_bytes());
    }
    match local.static_noise_pubkey.cmp(&peer.static_noise_pubkey) {
        std::cmp::Ordering::Less => Some(true),
        std::cmp::Ordering::Greater => Some(false),
        std::cmp::Ordering::Equal => None,
    }
}

/// Construct the one Noise handshake used by direct and relayed admission.
/// Canonical records bind identity, session, contract and enrolment to key proof.
pub(super) fn noise_handshake_state(
    local: &HewHandshake,
    peer: &HewHandshake,
    local_private_key: &[u8],
) -> Option<(snow::HandshakeState, bool)> {
    let initiator = noise_is_initiator(local, peer)?;
    let (first, second) = if initiator {
        (local, peer)
    } else {
        (peer, local)
    };
    let mut transcript = Sha256::new();
    transcript.update(b"hew-node-handshake\0");
    // Each checked CBOR record is self-delimiting; both endpoints hash the
    // exact same initiator/responder order through the shared codec.
    transcript.update(first.serialize());
    transcript.update(second.serialize());
    let prologue = transcript.finalize();
    let builder = snow::Builder::new(NOISE_PATTERN.parse().ok()?)
        .prologue(&prologue)
        .ok()?
        .local_private_key(local_private_key)
        .ok()?;
    let handshake = if initiator {
        builder.build_initiator().ok()?
    } else {
        builder.build_responder().ok()?
    };
    Some((handshake, initiator))
}

pub(super) unsafe fn upgrade_noise(
    transport: *mut HewTransport,
    conn_id: c_int,
    local: &HewHandshake,
    peer: &HewHandshake,
    local_private_key: &[u8],
) -> Option<(snow::StatelessTransportState, [u8; NOISE_STATIC_PUBKEY_LEN])> {
    let (mut handshake, initiator) = noise_handshake_state(local, peer, local_private_key)?;
    // SAFETY: transport pointer validity is guaranteed by caller.
    let t = unsafe { &*transport };
    // SAFETY: vtable pointer validity is guaranteed by transport construction.
    let ops = unsafe { t.ops.as_ref() }?;
    let send_fn = ops.send?;
    let recv_fn = ops.recv?;

    // Wrap handshake buffers in Zeroizing so ephemeral key material is
    // zeroised on all exit paths (normal return, early `?`, and unwind).
    let mut msg = Zeroizing::new(vec![0u8; NOISE_MAX_MSG_SIZE]);
    let mut payload = Zeroizing::new(vec![0u8; NOISE_MAX_MSG_SIZE]);

    if initiator {
        let n = handshake.write_message(&[], &mut msg).ok()?;
        // SAFETY: msg points to n readable bytes.
        if unsafe { send_fn(t.r#impl, conn_id, msg.as_ptr().cast(), n) } < 0 {
            return None;
        }
        // SAFETY: msg has NOISE_MAX_MSG_SIZE writable bytes.
        let n = unsafe { recv_fn(t.r#impl, conn_id, msg.as_mut_ptr().cast(), msg.len()) };
        if n <= 0 {
            return None;
        }
        #[expect(clippy::cast_sign_loss, reason = "n > 0 checked above")]
        let n = n as usize;
        if n > msg.len() {
            return None;
        }
        handshake.read_message(&msg[..n], &mut payload).ok()?;
        let n = handshake.write_message(&[], &mut msg).ok()?;
        // SAFETY: msg points to n readable bytes.
        if unsafe { send_fn(t.r#impl, conn_id, msg.as_ptr().cast(), n) } < 0 {
            return None;
        }
    } else {
        // SAFETY: msg has NOISE_MAX_MSG_SIZE writable bytes.
        let n = unsafe { recv_fn(t.r#impl, conn_id, msg.as_mut_ptr().cast(), msg.len()) };
        if n <= 0 {
            return None;
        }
        #[expect(clippy::cast_sign_loss, reason = "n > 0 checked above")]
        let n = n as usize;
        if n > msg.len() {
            return None;
        }
        handshake.read_message(&msg[..n], &mut payload).ok()?;
        let n = handshake.write_message(&[], &mut msg).ok()?;
        // SAFETY: msg points to n readable bytes.
        if unsafe { send_fn(t.r#impl, conn_id, msg.as_ptr().cast(), n) } < 0 {
            return None;
        }
        // SAFETY: msg has NOISE_MAX_MSG_SIZE writable bytes.
        let n = unsafe { recv_fn(t.r#impl, conn_id, msg.as_mut_ptr().cast(), msg.len()) };
        if n <= 0 {
            return None;
        }
        #[expect(clippy::cast_sign_loss, reason = "n > 0 checked above")]
        let n = n as usize;
        if n > msg.len() {
            return None;
        }
        handshake.read_message(&msg[..n], &mut payload).ok()?;
    }

    let remote_static = handshake.get_remote_static()?;
    if remote_static.len() != NOISE_STATIC_PUBKEY_LEN {
        return None;
    }
    let mut remote_pubkey = [0u8; NOISE_STATIC_PUBKEY_LEN];
    remote_pubkey.copy_from_slice(remote_static);
    let transport = handshake.into_stateless_transport_mode().ok()?;
    Some((transport, remote_pubkey))
}

/// Managers that withhold the responder-commit bit, so a test can pair a node
/// with one that predates the step.
#[cfg(test)]
pub(crate) mod withheld_commit {
    use std::collections::HashSet;
    use std::sync::{LazyLock, Mutex};

    use crate::util::MutexExt;

    use super::super::HewConnMgr;

    static WITHHELD: LazyLock<Mutex<HashSet<usize>>> = LazyLock::new(Mutex::default);

    /// Withholds the bit until dropped, so a later manager at a reused
    /// address advertises it again.
    pub(crate) struct Withheld(usize);

    pub(crate) fn withhold(mgr: *const HewConnMgr) -> Withheld {
        WITHHELD.lock_or_recover().insert(mgr as usize);
        Withheld(mgr as usize)
    }

    impl Drop for Withheld {
        fn drop(&mut self) {
            WITHHELD.lock_or_recover().remove(&self.0);
        }
    }

    pub(in crate::connection) fn applies(mgr: &HewConnMgr) -> bool {
        WITHHELD
            .lock_or_recover()
            .contains(&(std::ptr::from_ref(mgr) as usize))
    }
}
