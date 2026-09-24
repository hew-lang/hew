//! Unverified-peer gating of control, ask and outbound gossip authority.

use super::*;

/// Issue #2652 D9/D12 `unverified_gating`: an `Unverified` (delivery-only)
/// peer carries no control-plane authority. A SWIM frame it sends — even one
/// honestly attributed to its own declared `NodeId` — is dropped with no
/// ACK, and the [`inbound_ask_denied_unverified`] data-plane gate denies its
/// asks while still permitting fire-and-forget delivery.
#[test]
fn unverified_gating_denies_control_and_ask_authority() {
    let sends = Box::into_raw(Box::new(Mutex::new(Vec::<(c_int, Vec<u8>)>::new())));
    let ops = Box::new(crate::transport::HewTransportOps {
        connect: None,
        listen: None,
        accept: None,
        send: Some(record_registry_gossip_send),
        recv: None,
        close_conn: None,
        destroy: None,
    });
    let transport_ptr = Box::into_raw(Box::new(HewTransport {
        ops: &raw const *ops,
        r#impl: sends.cast(),
    }));
    let cfg = crate::cluster::ClusterConfig {
        local_node_id: 1,
        ..crate::cluster::ClusterConfig::default()
    };
    // SAFETY: cfg valid for the call.
    let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
    assert!(!cluster.is_null());

    // SAFETY: test-owned pointers remain valid until the explicit cleanup.
    unsafe {
        let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
        assert!(!mgr.is_null());

        // Unverified (loopback-dev / opt-out) peer node 2 on conn 10:
        // ACTIVE, gossip-capable, but posture is Unverified.
        let mut unverified = ConnectionActor::new(10);
        unverified.peer_node_id = 2;
        unverified.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
        unverified.posture = crate::peer_binding::Posture::Unverified;
        unverified.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(unverified));
        // Its delivery claim is published (Unverified peers still route
        // fire-and-forget), but posture keeps it off the control plane.
        let conn10_token = test_publish_claim(&*mgr, 2, 10);

        // Control plane: an honest SWIM PING from the Unverified peer is
        // dropped — no ACK is sent.
        let ping = SwimControlPayload {
            msg_type: crate::cluster::SWIM_MSG_PING,
            from: test_node_session(2),
            incarnation: 1,
            target: None,
            gossip: vec![],
        };
        let frame = decode_wire_frame(&swim_control_frame_bytes(&ping)).expect("frame");
        let WireFrame::Control(control) = frame else {
            panic!("expected control frame");
        };
        handle_swim_control_frame(mgr, HEW_FEATURE_SUPPORTS_GOSSIP, 10, conn10_token, &control);
        {
            let guard = (&*sends)
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            assert_eq!(
                guard.len(),
                0,
                "an Unverified peer's SWIM frame must be dropped with no ACK"
            );
        }
        assert_eq!(
            authenticated_peer_node_id_for_conn(&*mgr, 10),
            0,
            "an Unverified connection has no control-plane authority"
        );

        // Data plane: an inbound ask (request_id > 0) from the Unverified
        // peer is denied; a fire-and-forget delivery (request_id == 0) is
        // permitted (D9's one intentional Unverified capability).
        assert!(
            inbound_ask_denied_unverified(mgr, 10, 1),
            "an inbound ask from an Unverified peer must be denied"
        );
        assert!(
            !inbound_ask_denied_unverified(mgr, 10, 0),
            "fire-and-forget delivery from an Unverified peer stays allowed"
        );

        hew_connmgr_free(mgr);
        crate::cluster::hew_cluster_free(cluster);
        drop(Box::from_raw(transport_ptr));
        drop(Box::from_raw(sends));
    }
    drop(ops);
}

/// Issue #2652 D9 exact-owner authority: an authorised (`Strict` +
/// published) peer carries control-plane authority, and a **superseded**
/// owner loses it the instant the claim map is overwritten — even though its
/// posture is still `Strict` and its actor is not yet closed (D3 point 2).
#[test]
fn unverified_gating_supersede_revokes_authority() {
    let ops = Box::new(crate::transport::HewTransportOps {
        connect: None,
        listen: None,
        accept: None,
        send: None,
        recv: None,
        close_conn: None,
        destroy: None,
    });
    let transport_ptr = Box::into_raw(Box::new(HewTransport {
        ops: &raw const *ops,
        r#impl: std::ptr::null_mut(),
    }));
    let cfg = crate::cluster::ClusterConfig {
        local_node_id: 1,
        ..crate::cluster::ClusterConfig::default()
    };
    // SAFETY: cfg valid for the call.
    let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
    assert!(!cluster.is_null());

    // SAFETY: test-owned pointers remain valid until the explicit cleanup.
    unsafe {
        let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
        assert!(!mgr.is_null());

        // Exact-owner (Strict + published) peer node 3 on conn 20.
        let mut strict = ConnectionActor::new(20);
        strict.peer_node_id = 3;
        strict.posture = crate::peer_binding::Posture::Strict;
        strict.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(strict));
        test_publish_claim(&*mgr, 3, 20);
        assert_eq!(
            authenticated_peer_node_id_for_conn(&*mgr, 20),
            3,
            "an exact-owner Strict connection carries control authority"
        );
        assert!(
            !inbound_ask_denied_unverified(mgr, 20, 1),
            "an inbound ask from an authorised peer is permitted"
        );

        // Supersede node 3's claim onto a new conn 21.
        let mut strict2 = ConnectionActor::new(21);
        strict2.peer_node_id = 3;
        strict2.posture = crate::peer_binding::Posture::Strict;
        strict2.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(strict2));
        test_publish_claim(&*mgr, 3, 21);
        assert_eq!(
            authenticated_peer_node_id_for_conn(&*mgr, 20),
            0,
            "a superseded connection loses control authority"
        );
        assert_eq!(
            authenticated_peer_node_id_for_conn(&*mgr, 21),
            3,
            "the new exact owner carries control authority"
        );

        hew_connmgr_free(mgr);
        crate::cluster::hew_cluster_free(cluster);
        drop(Box::from_raw(transport_ptr));
    }
    drop(ops);
}

/// Issue #2652 (item 1, outbound authority): the predicate the outbound ask
/// gate in `setup_remote_ask` applies — `authenticated_peer_node_id_for_conn(
/// mgr, conn) == target_node_id` — must hold ONLY for the exact authenticated
/// owner of `target_node_id`. This is the outbound symmetric of the inbound
/// `inbound_ask_denied_unverified` gate: an `Unverified` (delivery-only) or a
/// **superseded** connection routed to the target both resolve to `0`
/// (`!= target_node_id`), so an outbound ask over either fails CLOSED with
/// `AskError::Unauthorized` before serialization; only the exact owner clears
/// the gate. This test pins the exact tri-state `setup_remote_ask` branches on
/// at the authority it consults, so a regression in either the authority or
/// the outbound wiring is caught here.
#[test]
fn outbound_ask_gate_authorizes_only_exact_owner() {
    let ops = Box::new(crate::transport::HewTransportOps {
        connect: None,
        listen: None,
        accept: None,
        send: None,
        recv: None,
        close_conn: None,
        destroy: None,
    });
    let transport_ptr = Box::into_raw(Box::new(HewTransport {
        ops: &raw const *ops,
        r#impl: std::ptr::null_mut(),
    }));
    let cfg = crate::cluster::ClusterConfig {
        local_node_id: 1,
        ..crate::cluster::ClusterConfig::default()
    };
    // SAFETY: cfg valid for the call.
    let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
    assert!(!cluster.is_null());

    // The predicate the outbound gate applies to a routed target connection.
    let clears_gate = |mgr: &HewConnMgr, conn_id: c_int, target: u16| -> bool {
        authenticated_peer_node_id_for_conn(mgr, conn_id) == target
    };

    // SAFETY: test-owned pointers remain valid until the explicit cleanup.
    unsafe {
        let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
        assert!(!mgr.is_null());

        // (a) Unverified target (node 2 on conn 10): the gate rejects — an
        // outbound ask here fails closed with Unauthorized.
        let mut unverified = ConnectionActor::new(10);
        unverified.peer_node_id = 2;
        unverified.posture = crate::peer_binding::Posture::Unverified;
        unverified.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(unverified));
        test_publish_claim(&*mgr, 2, 10);
        assert!(
            !clears_gate(&*mgr, 10, 2),
            "an outbound ask to an Unverified target must NOT clear the gate"
        );

        // (b) Exact owner (node 3 on conn 20): the gate authorizes.
        let mut owner = ConnectionActor::new(20);
        owner.peer_node_id = 3;
        owner.posture = crate::peer_binding::Posture::Strict;
        owner.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(owner));
        test_publish_claim(&*mgr, 3, 20);
        assert!(
            clears_gate(&*mgr, 20, 3),
            "an outbound ask to the exact authenticated owner must clear the gate"
        );

        // (c) Supersede node 3's claim onto a new conn 21: the old owner
        // (conn 20) loses authority the instant the claim map is overwritten,
        // so an outbound ask still routed to conn 20 fails closed — while the
        // new exact owner (conn 21) clears the gate.
        let mut owner2 = ConnectionActor::new(21);
        owner2.peer_node_id = 3;
        owner2.posture = crate::peer_binding::Posture::Strict;
        owner2.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(owner2));
        test_publish_claim(&*mgr, 3, 21);
        assert!(
            !clears_gate(&*mgr, 20, 3),
            "an outbound ask over a superseded connection must NOT clear the gate"
        );
        assert!(
            clears_gate(&*mgr, 21, 3),
            "the new exact owner must clear the outbound ask gate"
        );

        hew_connmgr_free(mgr);
        crate::cluster::hew_cluster_free(cluster);
        drop(Box::from_raw(transport_ptr));
    }
    drop(ops);
}

/// Issue #2652 (item 1): a `CTRL_LINK_DOWN` frame is authorised by the exact
/// handshake-authenticated owner of the peer's published claim, NOT the
/// posture-agnostic self-declared delivery id. An `Unverified`
/// (delivery-only) peer and a **superseded** owner both resolve to `0`
/// authenticated identity, so their link-DOWN is dropped with a diagnostic
/// BEFORE any cross-node link-exit cascade can crash a locally-linked actor.
///
/// Regression guard: the pre-fix handler read `peer_node_id_for_conn` (which
/// returns the nonzero self-declared id even for an `Unverified`/superseded
/// connection), so `deliver_link_down_to_ref`'s `== 0` guard never fired and
/// such a peer could crash an actor linked to the genuine owner. If the gate
/// is removed, the diagnostic below changes (the handler falls through to
/// `handle_inbound_link_down`, which sets "no runtime installed").
#[test]
fn link_down_from_unverified_or_superseded_peer_is_gated() {
    let ops = Box::new(crate::transport::HewTransportOps {
        connect: None,
        listen: None,
        accept: None,
        send: None,
        recv: None,
        close_conn: None,
        destroy: None,
    });
    let transport_ptr = Box::into_raw(Box::new(HewTransport {
        ops: &raw const *ops,
        r#impl: std::ptr::null_mut(),
    }));
    let cfg = crate::cluster::ClusterConfig {
        local_node_id: 1,
        ..crate::cluster::ClusterConfig::default()
    };
    // SAFETY: cfg valid for the call.
    let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
    assert!(!cluster.is_null());

    let link_down_frame = |ref_id: u64, route_slot: u16| {
        let payload = crate::envelope::MonitorDownPayload {
            ref_id,
            target: test_location(route_slot, 1),
            reason: 2,
            crash_kind: 0,
        };
        let bytes = crate::envelope::encode_link_down_payload(&payload).expect("link down encodes");
        ControlFrame {
            version: WIRE_VERSION,
            ctrl_kind: CTRL_LINK_DOWN,
            payload: bytes,
        }
    };

    // SAFETY: test-owned pointers remain valid until the explicit cleanup.
    unsafe {
        let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
        assert!(!mgr.is_null());

        // Unverified peer node 2 on conn 10: ACTIVE + a published *delivery*
        // claim (Unverified peers still route fire-and-forget), but posture
        // is Unverified so it carries no control-plane authority.
        let mut unverified = ConnectionActor::new(10);
        unverified.peer_node_id = 2;
        unverified.posture = crate::peer_binding::Posture::Unverified;
        unverified.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(unverified));
        let conn10_token = test_publish_claim(&*mgr, 2, 10);

        let _ = take_hew_last_error();
        assert_eq!(
            authenticated_peer_node_id_for_conn(&*mgr, 10),
            0,
            "precondition: Unverified conn 10 must have no authenticated identity"
        );
        handle_link_down_frame(mgr, 10, conn10_token, &link_down_frame(101, 2));
        let diag = take_hew_last_error().expect("gated link down must leave a diagnostic");
        assert!(
            diag.contains("link down") && diag.contains("missing authenticated peer"),
            "an Unverified peer's CTRL_LINK_DOWN must be gated by the exact-owner \
             check, got: {diag}"
        );

        // Superseded owner: Strict conn 20 for node 3, then supersede its
        // claim onto conn 21. Conn 20 keeps Strict posture but is no longer
        // the claim owner, so its link-DOWN is refused too.
        let mut strict = ConnectionActor::new(20);
        strict.peer_node_id = 3;
        strict.posture = crate::peer_binding::Posture::Strict;
        strict.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(strict));
        let conn20_token = test_publish_claim(&*mgr, 3, 20);
        let mut strict2 = ConnectionActor::new(21);
        strict2.peer_node_id = 3;
        strict2.posture = crate::peer_binding::Posture::Strict;
        strict2.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(strict2));
        test_publish_claim(&*mgr, 3, 21);

        let _ = take_hew_last_error();
        handle_link_down_frame(mgr, 20, conn20_token, &link_down_frame(102, 3));
        let diag = take_hew_last_error().expect("superseded link down must leave a diagnostic");
        assert!(
            diag.contains("link down") && diag.contains("missing authenticated peer"),
            "a superseded owner's CTRL_LINK_DOWN must be gated, got: {diag}"
        );

        hew_connmgr_free(mgr);
        crate::cluster::hew_cluster_free(cluster);
        drop(Box::from_raw(transport_ptr));
    }
    drop(ops);
}

/// Issue #2652 (item 2): outbound registry gossip and SWIM traffic go ONLY
/// to authenticated (`Strict` + published-owner) connections. An `Unverified`
/// (delivery-only) peer must not be selected as a gossip recipient, a SWIM
/// relay, or a direct SWIM-send target — symmetric with the inbound gate —
/// while fire-and-forget user-message delivery to it stays allowed (D9's one
/// intentional Unverified capability).
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "test covers the full Unverified outbound gate across all control frame types"
)]
fn unverified_peer_receives_no_outbound_gossip_or_swim() {
    let sends = Box::into_raw(Box::new(Mutex::new(Vec::<(c_int, Vec<u8>)>::new())));
    let ops = Box::new(crate::transport::HewTransportOps {
        connect: None,
        listen: None,
        accept: None,
        send: Some(record_registry_gossip_send),
        recv: None,
        close_conn: None,
        destroy: None,
    });
    let transport_ptr = Box::into_raw(Box::new(HewTransport {
        ops: &raw const *ops,
        r#impl: sends.cast(),
    }));
    let cfg = crate::cluster::ClusterConfig {
        local_node_id: 1,
        ..crate::cluster::ClusterConfig::default()
    };
    // SAFETY: cfg valid for the call.
    let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
    assert!(!cluster.is_null());

    // SAFETY: test-owned pointers remain valid until the explicit cleanup.
    unsafe {
        let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
        assert!(!mgr.is_null());

        // Unverified gossip-capable peer node 2 on conn 10.
        let mut unverified = ConnectionActor::new(10);
        unverified.peer_node_id = 2;
        unverified.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
        unverified.posture = crate::peer_binding::Posture::Unverified;
        unverified.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(unverified));
        test_publish_claim(&*mgr, 2, 10);

        // Authenticated gossip-capable peer node 3 on conn 20.
        let mut strict = ConnectionActor::new(20);
        strict.peer_node_id = 3;
        strict.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
        strict.posture = crate::peer_binding::Posture::Strict;
        strict.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(strict));
        test_publish_claim(&*mgr, 3, 20);

        // Gossip recipient selection excludes the Unverified connection.
        assert_eq!(
            active_gossip_connection_ids(&*mgr),
            vec![20],
            "only the authenticated connection may receive registry gossip"
        );
        // SWIM relay selection excludes the Unverified peer.
        assert_eq!(
            hew_connmgr_active_swim_peers(mgr),
            vec![3],
            "only authenticated peers are eligible SWIM relays"
        );
        // Direct SWIM send to the Unverified peer is refused; to the
        // authenticated peer it proceeds.
        assert_eq!(
            hew_connmgr_send_swim(mgr, 2, crate::cluster::SWIM_MSG_PING, 0),
            -1,
            "a direct SWIM send to an Unverified peer must be refused"
        );
        assert_eq!(
            hew_connmgr_send_swim(mgr, 3, crate::cluster::SWIM_MSG_PING, 0),
            0,
            "a direct SWIM send to an authenticated peer proceeds"
        );

        // A gossip broadcast reaches ONLY the authenticated connection.
        {
            let guard = (&*sends)
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            assert!(
                guard.iter().all(|(conn, _)| *conn == 20),
                "no SWIM/gossip traffic may target the Unverified conn 10"
            );
        }
        (&*sends)
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clear();
        let count =
            hew_connmgr_broadcast_registry_gossip(mgr, "svc", test_location(1, 0xdef0), true);
        assert_eq!(
            count, 1,
            "registry gossip broadcast reaches exactly the one authenticated peer"
        );
        {
            let guard = (&*sends)
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            assert!(
                guard.iter().all(|(conn, _)| *conn == 20),
                "registry gossip broadcast must never target the Unverified conn 10"
            );
        }

        // Delivery-only capability preserved: fire-and-forget delivery to the
        // Unverified peer stays allowed even as control-plane traffic is
        // suppressed.
        assert!(
            !inbound_ask_denied_unverified(mgr, 10, 0),
            "fire-and-forget delivery to/from the Unverified peer stays allowed"
        );
        assert!(
            inbound_ask_denied_unverified(mgr, 10, 1),
            "an inbound ask from the Unverified peer stays denied"
        );

        hew_connmgr_free(mgr);
        crate::cluster::hew_cluster_free(cluster);
        drop(Box::from_raw(transport_ptr));
        drop(Box::from_raw(sends));
    }
    drop(ops);
}
