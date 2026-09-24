//! Protocol handshake, framing and the Noise upgrade.

use std::ffi::c_int;

#[cfg(feature = "encryption")]
use zeroize::Zeroizing;

use crate::envelope::{FRAME_TYPE_CONTROL, FRAME_TYPE_ENVELOPE, WIRE_VERSION};
use crate::node_identity::NodeId;
use crate::set_last_error;
use crate::transport::HewTransport;

use super::{
    HewHandshake, FNV1A32_OFFSET_BASIS, FNV1A32_PRIME, HEW_FEATURE_SUPPORTS_ASK_REJECTION,
    HEW_FEATURE_SUPPORTS_GOSSIP, HEW_HANDSHAKE_SIZE, HEW_PROTOCOL_VERSION, NOISE_STATIC_PUBKEY_LEN,
};
#[cfg(feature = "encryption")]
use super::{HEW_FEATURE_SUPPORTS_ENCRYPTION, NOISE_MAX_MSG_SIZE, NOISE_PATTERN};

fn local_feature_flags() -> u32 {
    #[cfg_attr(
        not(feature = "encryption"),
        allow(
            unused_mut,
            reason = "mut is only exercised by the encryption-gated flags |= below"
        )
    )]
    let mut flags = HEW_FEATURE_SUPPORTS_GOSSIP | HEW_FEATURE_SUPPORTS_ASK_REJECTION;
    #[cfg(feature = "encryption")]
    {
        flags |= HEW_FEATURE_SUPPORTS_ENCRYPTION;
    }
    flags
}

pub(crate) fn supports_ask_rejection(flags: u32) -> bool {
    flags & HEW_FEATURE_SUPPORTS_ASK_REJECTION != 0
}

pub(crate) fn supports_gossip(flags: u32) -> bool {
    flags & HEW_FEATURE_SUPPORTS_GOSSIP != 0
}

pub(super) fn is_ask_rejection_reply(msg_type: i32, peer_feature_flags: u32) -> bool {
    msg_type == crate::hew_node::HEW_REPLY_REJECT_MSG_TYPE
        && supports_ask_rejection(peer_feature_flags)
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
        protocol_version: HEW_PROTOCOL_VERSION,
        schema_hash: local_schema_hash(),
        feature_flags: local_feature_flags(),
        node_id: local_node_id,
        session_incarnation,
        static_noise_pubkey,
    }
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

unsafe fn recv_frame_exact(
    transport: *mut HewTransport,
    conn_id: c_int,
    payload: &mut [u8],
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
    let Some(recv_fn) = ops.recv else {
        return false;
    };
    let Ok(expected) = c_int::try_from(payload.len()) else {
        return false;
    };
    // SAFETY: payload pointer is valid for payload.len() writable bytes.
    unsafe {
        recv_fn(
            t.r#impl,
            conn_id,
            payload.as_mut_ptr().cast(),
            payload.len(),
        ) == expected
    }
}

unsafe fn handshake_send(
    transport: *mut HewTransport,
    conn_id: c_int,
    handshake: HewHandshake,
) -> c_int {
    // SAFETY: transport and conn_id are validated by caller; serialize returns a fixed-size buffer.
    -c_int::from(!unsafe { send_frame(transport, conn_id, &handshake.serialize()) })
}

unsafe fn handshake_recv(transport: *mut HewTransport, conn_id: c_int) -> Option<HewHandshake> {
    let mut buf = [0u8; HEW_HANDSHAKE_SIZE];
    // SAFETY: transport and conn_id are validated by caller; buf is stack-allocated with correct size.
    if !unsafe { recv_frame_exact(transport, conn_id, &mut buf) } {
        set_last_error(format!(
            "hew_connmgr_add: failed to receive handshake for conn {conn_id}"
        ));
        return None;
    }
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
    local: HewHandshake,
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
    if !version_compatible(&local, &peer) {
        set_last_error(format!(
            "hew_connmgr_add: handshake protocol mismatch for conn {conn_id} (local={}, peer={})",
            local.protocol_version, peer.protocol_version
        ));
        return None;
    }
    if !schema_compatible(&local, &peer) {
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

#[cfg(feature = "encryption")]
pub(super) fn supports_encryption(flags: u32) -> bool {
    flags & HEW_FEATURE_SUPPORTS_ENCRYPTION != 0
}

#[cfg(feature = "encryption")]
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

#[cfg(feature = "encryption")]
pub(super) unsafe fn upgrade_noise(
    transport: *mut HewTransport,
    conn_id: c_int,
    local: &HewHandshake,
    peer: &HewHandshake,
    local_private_key: &[u8],
) -> Option<(snow::TransportState, [u8; NOISE_STATIC_PUBKEY_LEN])> {
    let initiator = noise_is_initiator(local, peer)?;
    // SAFETY: transport pointer validity is guaranteed by caller.
    let t = unsafe { &*transport };
    // SAFETY: vtable pointer validity is guaranteed by transport construction.
    let ops = unsafe { t.ops.as_ref() }?;
    let send_fn = ops.send?;
    let recv_fn = ops.recv?;

    let builder = snow::Builder::new(NOISE_PATTERN.parse().ok()?);
    let mut handshake = if initiator {
        builder
            .local_private_key(local_private_key)
            .ok()?
            .build_initiator()
            .ok()?
    } else {
        builder
            .local_private_key(local_private_key)
            .ok()?
            .build_responder()
            .ok()?
    };

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
        handshake.read_message(&msg[..n], &mut payload).ok()?;
    }

    let remote_static = handshake.get_remote_static()?;
    if remote_static.len() != NOISE_STATIC_PUBKEY_LEN {
        return None;
    }
    let mut remote_pubkey = [0u8; NOISE_STATIC_PUBKEY_LEN];
    remote_pubkey.copy_from_slice(remote_static);
    let transport = handshake.into_transport_mode().ok()?;
    Some((transport, remote_pubkey))
}
