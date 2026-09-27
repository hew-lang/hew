//! Handshake encoding and compatibility tests.

use super::*;

#[test]
fn null_mgr_safety() {
    // All operations on null manager should return gracefully.
    // SAFETY: testing null safety.
    unsafe {
        let null_mgr: *mut HewConnMgr = std::ptr::null_mut();
        assert_eq!(hew_connmgr_count(null_mgr), -1);
        assert_eq!(hew_connmgr_last_activity(null_mgr, 0), 0);
        assert_eq!(hew_connmgr_conn_state(null_mgr, 0), CONN_STATE_CLOSED);
        hew_connmgr_free(null_mgr); // should not crash
    }
}

#[test]
fn mgr_null_transport_rejected() {
    // SAFETY: testing null transport rejection.
    unsafe {
        let mgr = hew_connmgr_new(
            std::ptr::null_mut(),
            None,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            0,
        );
        assert!(mgr.is_null());
    }
}

#[test]
fn handshake_round_trip() {
    let hs = HewHandshake {
        protocol_version: HEW_PROTOCOL_VERSION,
        schema_hash: 0x1234_5678,
        feature_flags: HEW_FEATURE_SUPPORTS_GOSSIP,
        node_id: test_node_identity(42),
        session_incarnation: 7,
        static_noise_pubkey: [7; NOISE_STATIC_PUBKEY_LEN],
    };
    let encoded = hs.serialize();
    let decoded = HewHandshake::deserialize(&encoded).expect("valid handshake");
    assert_eq!(decoded, hs);
}

#[test]
fn handshake_rejects_invalid_magic() {
    let mut bytes = [0u8; HEW_HANDSHAKE_SIZE];
    bytes.copy_from_slice(
        &local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]).serialize(),
    );
    bytes[0] = b'X';
    assert!(HewHandshake::deserialize(&bytes).is_none());
}

#[test]
fn protocol_version_mismatch_rejected() {
    let local = local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]);
    let mut peer = local;
    peer.protocol_version = local.protocol_version.wrapping_add(1);
    assert!(!version_compatible(&local, &peer));
}

#[test]
fn handshake_rejects_future_protocol_version() {
    let local = local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]);
    let peer = HewHandshake {
        protocol_version: 999,
        ..local
    };
    assert!(!version_compatible(&local, &peer));
}

#[test]
fn schema_hash_mismatch_rejected() {
    let local = local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]);
    let mut peer = local;
    peer.schema_hash ^= 0x0100_0000;
    assert!(!schema_compatible(&local, &peer));
}

#[test]
fn handshake_rejects_zero_session_and_nonzero_reserved_fields() {
    let encoded =
        local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]).serialize();

    let mut zero_session = encoded;
    zero_session[32..36].fill(0);
    assert!(HewHandshake::deserialize(&zero_session).is_none());

    let mut reserved_header = encoded;
    reserved_header[6] = 1;
    assert!(HewHandshake::deserialize(&reserved_header).is_none());

    let mut reserved_identity = encoded;
    reserved_identity[36] = 1;
    assert!(HewHandshake::deserialize(&reserved_identity).is_none());
}

#[test]
fn handshake_rejects_v1_length_and_magic() {
    let old = [0_u8; 48];
    assert!(HewHandshake::deserialize(&old).is_none());
    assert!(HewHandshake::deserialize(&[0_u8; 71]).is_none());
    assert!(HewHandshake::deserialize(&[0_u8; 73]).is_none());

    let mut encoded =
        local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]).serialize();
    encoded[..4].copy_from_slice(b"HEW\x01");
    assert!(HewHandshake::deserialize(&encoded).is_none());
}

#[test]
fn handshake_rejects_zero_node_id() {
    let mut encoded =
        local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]).serialize();
    encoded[16..32].fill(0);
    assert!(HewHandshake::deserialize(&encoded).is_none());
}

#[test]
fn local_schema_hash_covers_acknowledged_setup_controls() {
    assert_eq!(local_schema_hash(), 0x774b_20fa);
}
