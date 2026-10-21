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
        protocol_version: u16::from(crate::envelope::WIRE_VERSION),
        schema_hash: 0x1234_5678,
        feature_flags: HEW_FEATURE_SUPPORTS_GOSSIP,
        node_id: test_node_identity(42),
        session_incarnation: 7,
        static_noise_pubkey: [7; NOISE_STATIC_PUBKEY_LEN],
        enrollment: vec![1, 2, 3],
    };
    let encoded = hs.serialize();
    let decoded = HewHandshake::deserialize(&encoded).expect("valid handshake");
    assert_eq!(decoded, hs);
}

#[test]
fn protocol_version_mismatch_rejected() {
    let local = local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]);
    let mut peer = local.clone();
    peer.protocol_version = local.protocol_version.wrapping_add(1);
    assert!(!version_compatible(&local, &peer));
    peer.protocol_version = local.protocol_version - 1;
    assert!(!version_compatible(&local, &peer));
}

#[test]
fn handshake_rejects_future_protocol_version() {
    let local = local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]);
    let peer = HewHandshake {
        protocol_version: 999,
        ..local.clone()
    };
    assert!(!version_compatible(&local, &peer));
}

#[test]
fn schema_hash_mismatch_rejected() {
    let local = local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]);
    let mut peer = local.clone();
    peer.schema_hash ^= 0x0100_0000;
    assert!(!schema_compatible(&local, &peer));
}

#[test]
fn handshake_rejects_invalid_identity_session_key_and_old_fixed_record() {
    let local = local_handshake(test_node_identity(1), 1, [3; NOISE_STATIC_PUBKEY_LEN]);
    let encoded = local.serialize();
    let wire =
        crate::node_protocol_generated::NodeHandshake::decode(hew_codec::Format::Cbor, &encoded)
            .expect("checked record");
    let mut invalid = wire.clone();
    invalid.incarnation = 0;
    assert!(HewHandshake::deserialize(&invalid.encode(hew_codec::Format::Cbor)).is_none());
    invalid = wire.clone();
    invalid.node.fill(0);
    assert!(HewHandshake::deserialize(&invalid.encode(hew_codec::Format::Cbor)).is_none());
    invalid = wire;
    invalid.noise_key.pop();
    assert!(HewHandshake::deserialize(&invalid.encode(hew_codec::Format::Cbor)).is_none());
    assert!(HewHandshake::deserialize(&[0; 72]).is_none());
    assert!(HewHandshake::deserialize(&vec![0; MAX_HANDSHAKE_BYTES + 1]).is_none());
}

#[test]
fn handshake_rejects_unknown_duplicate_missing_and_trailing_fields() {
    let encoded =
        local_handshake(test_node_identity(1), 1, [3; NOISE_STATIC_PUBKEY_LEN]).serialize();
    let mut invalid = encoded.clone();
    assert_eq!(invalid[0], 0xa7);
    invalid[0] = 0xa8;
    invalid.extend_from_slice(&[0x18, 99, 0]);
    assert!(HewHandshake::deserialize(&invalid).is_none());
    invalid = encoded.clone();
    invalid[0] = 0xa8;
    invalid.extend_from_slice(&[1, crate::envelope::WIRE_VERSION]);
    assert!(HewHandshake::deserialize(&invalid).is_none());
    invalid = encoded.clone();
    invalid[0] = 0xa6;
    invalid.truncate(invalid.len() - 2); // Omit the required empty enrollment.
    assert!(HewHandshake::deserialize(&invalid).is_none());
    invalid = encoded;
    invalid.push(0);
    assert!(HewHandshake::deserialize(&invalid).is_none());
}

#[test]
fn generated_handshake_u16_bounds_share_existing_formats() {
    let encoded =
        local_handshake(test_node_identity(1), 1, [3; NOISE_STATIC_PUBKEY_LEN]).serialize();
    let mut wire =
        crate::node_protocol_generated::NodeHandshake::decode(hew_codec::Format::Cbor, &encoded)
            .expect("handshake");
    wire.protocol_version = u16::MAX;
    for format in [hew_codec::Format::Cbor, hew_codec::Format::Msgpack] {
        let encoded = wire.encode(format);
        assert_eq!(
            crate::node_protocol_generated::NodeHandshake::decode(format, &encoded)
                .expect("width boundary"),
            wire
        );
    }
    let mut invalid = encoded;
    invalid[2] = 0x1a;
    invalid.splice(3..3, [0, 1, 0, 0]); // Protocol version 65536 exceeds u16.
    assert!(HewHandshake::deserialize(&invalid).is_none());
}

#[test]
fn noise_transcript_binds_canonical_sessions_contract_and_enrollment() {
    let builder = snow::Builder::new(NOISE_PATTERN.parse().expect("pattern"));
    let a = builder.generate_keypair().expect("a key");
    let b = builder.generate_keypair().expect("b key");
    let a_public: [u8; 32] = a.public.as_slice().try_into().expect("a public");
    let b_public: [u8; 32] = b.public.as_slice().try_into().expect("b public");
    let a_record = local_handshake(NodeId::from_noise_static_key(&a_public), 17, a_public);
    let b_record = local_handshake(NodeId::from_noise_static_key(&b_public), 23, b_public);
    let mutations: [fn(&mut HewHandshake); 4] = [
        |record| record.session_incarnation += 1,
        |record| record.schema_hash ^= 1,
        |record| record.feature_flags ^= HEW_FEATURE_SUPPORTS_GOSSIP,
        |record| record.enrollment.push(1),
    ];
    for mutation in std::iter::once(None).chain(mutations.into_iter().map(Some)) {
        let mut altered_b = b_record.clone();
        if let Some(mutate) = mutation {
            mutate(&mut altered_b);
        }
        let (a_state, a_initiator) =
            crate::connection::handshake::noise_handshake_state(&a_record, &altered_b, &a.private)
                .expect("a handshake");
        let (b_state, _) =
            crate::connection::handshake::noise_handshake_state(&b_record, &a_record, &b.private)
                .expect("b handshake");
        let (mut initiator, mut responder) = if a_initiator {
            (a_state, b_state)
        } else {
            (b_state, a_state)
        };
        let mut message = vec![0; NOISE_MAX_MSG_SIZE];
        let mut payload = vec![0; NOISE_MAX_MSG_SIZE];
        let n = initiator
            .write_message(&[], &mut message)
            .expect("message one");
        responder
            .read_message(&message[..n], &mut payload)
            .expect("ephemeral key");
        let n = responder
            .write_message(&[], &mut message)
            .expect("message two");
        let authenticated = initiator.read_message(&message[..n], &mut payload).is_ok();
        assert_eq!(
            authenticated,
            mutation.is_none(),
            "altered metadata cannot authenticate"
        );
        if authenticated {
            let n = initiator
                .write_message(&[], &mut message)
                .expect("message three");
            responder
                .read_message(&message[..n], &mut payload)
                .expect("mutual authentication");
            assert!(initiator.is_handshake_finished() && responder.is_handshake_finished());
        }
    }
}
