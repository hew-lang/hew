//! Control-frame authority, ask rejection, registry gossip and SWIM tests.

use super::*;

#[test]
fn link_controls_from_wrong_authenticated_peer_are_rejected() {
    let _guard = crate::runtime_test_guard();
    let mut peer = ConnectionActor::new(10);
    peer.peer_node_id = 2;
    peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
    let mgr = HewConnMgr {
        connections: PoisonSafe::new(vec![peer]),
        expected_peer_ids: PoisonSafe::new(HashMap::new()),
        transport: std::ptr::null_mut(),
        inbound_router: None,
        routing_table: std::ptr::null_mut(),
        cluster: std::ptr::null_mut(),
        reconnect_enabled: AtomicBool::new(false),
        reconnect_max_retries: AtomicU32::new(RECONNECT_DEFAULT_MAX_RETRIES),
        reconnect_shutdown: Arc::new(AtomicBool::new(false)),
        inbound_spawn_closed: Arc::new(AtomicBool::new(false)),
        inbound_ask_active: Arc::new(AtomicUsize::new(0)),
        reconnect_workers: PoisonSafe::new(Vec::new()),
        reverse_link_workers: PoisonSafe::new(Vec::new()),
        reader_lifecycle: Arc::new(ReaderLifecycle::default()),
        next_publication_token: AtomicU64::new(1),
        local_node_id: 1,
        local_identity: None,
        local_session_incarnation: None,
        auth: PeerAuthSnapshot::unconfigured(),
        claims: (
            std::sync::Mutex::new(std::collections::HashMap::new()),
            std::sync::Condvar::new(),
        ),
        pending_registry_flush: PoisonSafe::new(HashMap::new()),
        pending_registry_flush_count: AtomicUsize::new(0),
    };
    let mgr_ptr = std::ptr::from_ref(&mgr).cast_mut();
    // No claim exists for conn 10; any reader token denies immediately.
    let conn10_token = 1_u64;
    let payload = crate::envelope::LinkReqPayload {
        linker: test_location(9, 88),
        ref_id: 123,
        target: test_location(1, 77),
        policy_tag: 1,
        reciprocate: 0,
        setup_id: 456,
    };

    let link_req = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: CTRL_LINK_REQ,
        payload: crate::envelope::encode_link_req_payload(&payload)
            .expect("link request payload must encode"),
    };
    handle_control_frame(mgr_ptr, 0, 10, conn10_token, &link_req);
    assert!(
        crate::runtime::rt_current()
            .monitors
            .take_remote_watchers(payload.target.slot())
            .is_empty(),
        "a link request claiming another node must not register a watcher"
    );

    crate::runtime::rt_current()
        .monitors
        .register_remote_link_watcher_for_test(
            payload.target.slot(),
            test_location(2, payload.linker.slot()),
            payload.ref_id,
        );
    let unlink = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: CTRL_UNLINK,
        payload: crate::envelope::encode_link_req_payload(&payload)
            .expect("unlink payload must encode"),
    };
    handle_control_frame(mgr_ptr, 0, 10, conn10_token, &unlink);

    let watchers = crate::runtime::rt_current()
        .monitors
        .take_remote_watchers(payload.target.slot());
    assert_eq!(
        watchers.len(),
        1,
        "mismatched unlink must not remove watcher"
    );
    assert_eq!(watchers[0].watcher.node(), test_node_id(2));
    assert_eq!(watchers[0].ref_id, payload.ref_id);
    assert!(watchers[0].is_link);
}

#[test]
fn ask_rejection_reply_requires_negotiated_feature_flag() {
    assert!(is_ask_rejection_reply(
        crate::hew_node::HEW_REPLY_REJECT_MSG_TYPE,
        HEW_FEATURE_SUPPORTS_ASK_REJECTION
    ));
    assert!(
        !is_ask_rejection_reply(crate::hew_node::HEW_REPLY_REJECT_MSG_TYPE, 0),
        "sentinel replies from peers without the feature bit must stay on the normal reply path"
    );
    assert!(
        !is_ask_rejection_reply(0, HEW_FEATURE_SUPPORTS_ASK_REJECTION),
        "normal replies must not be reclassified as rejections"
    );
}

#[test]
fn registry_gossip_broadcast_targets_only_active_gossip_peers() {
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

    // SAFETY: test-owned pointers remain valid until the explicit cleanup below.
    unsafe {
        let mgr = hew_connmgr_new(
            transport_ptr,
            None,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            1,
        );
        assert!(!mgr.is_null());

        let mut gossip_peer = ConnectionActor::new(10);
        gossip_peer.peer_node_id = 2;
        gossip_peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
        gossip_peer.posture = crate::peer_binding::Posture::Strict;
        gossip_peer
            .state
            .store(CONN_STATE_ACTIVE, Ordering::Release);

        let mut old_peer = ConnectionActor::new(11);
        old_peer.peer_node_id = 3;
        old_peer.peer_feature_flags = 0;
        old_peer.posture = crate::peer_binding::Posture::Strict;
        old_peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);

        let mut draining_peer = ConnectionActor::new(12);
        draining_peer.peer_node_id = 4;
        draining_peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
        draining_peer.posture = crate::peer_binding::Posture::Strict;
        draining_peer
            .state
            .store(CONN_STATE_DRAINING, Ordering::Release);

        (&*mgr).connections.access(|conns| {
            conns.push(gossip_peer);
            conns.push(old_peer);
            conns.push(draining_peer);
        });
        // issue #2652 (item 2): outbound gossip goes only to authenticated
        // (`Strict` + published-owner) peers, so each candidate needs a
        // published claim. Only conn 10 also supports gossip and is ACTIVE,
        // so it remains the sole recipient.
        test_publish_claim(&*mgr, 2, 10);
        test_publish_claim(&*mgr, 3, 11);
        test_publish_claim(&*mgr, 4, 12);

        let location = test_location(2, 0x42);
        assert_eq!(
            hew_connmgr_broadcast_registry_gossip(mgr, "worker", location, true),
            1
        );

        {
            let sends_ref = &*sends;
            let sends_guard = sends_ref
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            assert_eq!(sends_guard.len(), 1);
            assert_eq!(sends_guard[0].0, 10);
            let WireFrame::Control(control) =
                decode_wire_frame(&sends_guard[0].1).expect("control frame")
            else {
                panic!("registry gossip broadcast must send a control frame");
            };
            assert_eq!(control.ctrl_kind, CTRL_REGISTRY_GOSSIP);
            let payload = decode_registry_gossip_payload(&control.payload).expect("gossip payload");
            assert_eq!(payload.op, crate::cluster::GOSSIP_REGISTRY_ADD);
            assert_eq!(payload.name, "worker");
            assert_eq!(payload.location, test_location(2, 0x42));
        }

        hew_connmgr_free(mgr);
        drop(Box::from_raw(transport_ptr));
        drop(Box::from_raw(sends));
    }
    drop(ops);
}

/// A PING SWIM frame is answered with an ACK, and the cross-attribution
/// guard rejects a frame whose `from` identity does not match the handshake
/// identity and session of the connection it arrived on.
#[test]
fn swim_ping_is_acked_and_cross_attribution_is_rejected() {
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

        // Active gossip-capable peer node 2 on conn 10.
        let mut peer = ConnectionActor::new(10);
        peer.peer_node_id = 2;
        peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
        peer.posture = Posture::Strict;
        peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(peer));
        // Model node 2 as the admitted, published owner of its NodeId so the
        // issue #2652 exact-owner control gate accepts its SWIM frames.
        let conn10_token = test_publish_claim(&*mgr, 2, 10);

        // Honest PING from node 2 (matches the conn's authenticated id).
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

        // An ACK must have been sent back on conn 10.
        {
            let guard = (&*sends)
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            assert_eq!(guard.len(), 1, "PING must produce exactly one ACK send");
            assert_eq!(guard[0].0, 10);
            let WireFrame::Control(ctrl) = decode_wire_frame(&guard[0].1).expect("ack frame")
            else {
                panic!("ack must be a control frame");
            };
            assert_eq!(ctrl.ctrl_kind, CTRL_SWIM);
            let ack = decode_swim_payload(&ctrl.payload).expect("ack payload");
            assert_eq!(ack.msg_type, crate::cluster::SWIM_MSG_ACK);
            assert_eq!(
                ack.from,
                test_node_session(1),
                "ACK is stamped with our local node identity and session"
            );
        }

        // Spoofed PING claiming to be node 9 on node 2's connection is
        // rejected — no further send.
        let spoof = SwimControlPayload {
            msg_type: crate::cluster::SWIM_MSG_PING,
            from: test_node_session(9),
            incarnation: 1,
            target: None,
            gossip: vec![],
        };
        let spoof_frame = decode_wire_frame(&swim_control_frame_bytes(&spoof)).expect("frame");
        let WireFrame::Control(spoof_control) = spoof_frame else {
            panic!("expected control frame");
        };
        handle_swim_control_frame(
            mgr,
            HEW_FEATURE_SUPPORTS_GOSSIP,
            10,
            conn10_token,
            &spoof_control,
        );
        {
            let guard = (&*sends)
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            assert_eq!(
                guard.len(),
                1,
                "cross-attributed PING must be dropped without an ACK"
            );
        }

        hew_connmgr_free(mgr);
        crate::cluster::hew_cluster_free(cluster);
        drop(Box::from_raw(transport_ptr));
        drop(Box::from_raw(sends));
    }
    drop(ops);
}

/// An inbound SWIM frame carrying piggybacked DEAD gossip about a third
/// node folds that transition into the local membership view (C6 import).
#[test]
fn swim_frame_imports_piggybacked_gossip() {
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
        // Pre-seed node 3 as ALIVE so the gossip can transition it.
        let addr = std::ffi::CString::new("10.0.0.3:9000").unwrap();
        crate::cluster::hew_cluster_join(cluster, 3, addr.as_ptr());

        let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
        assert!(!mgr.is_null());

        let mut peer = ConnectionActor::new(10);
        peer.peer_node_id = 2;
        peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
        peer.posture = Posture::Strict;
        peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(peer));
        // Model node 2 as the admitted, published owner of its NodeId so the
        // issue #2652 exact-owner control gate accepts its gossip.
        let conn10_token = test_publish_claim(&*mgr, 2, 10);

        // ACK from node 2 carrying DEAD-about-node-3 gossip.
        let ack = SwimControlPayload {
            msg_type: crate::cluster::SWIM_MSG_ACK,
            from: test_node_session(2),
            incarnation: 1,
            target: None,
            gossip: vec![SwimGossipEntry {
                member: test_node_session(3),
                state: crate::cluster::MEMBER_DEAD,
                incarnation: 5,
            }],
        };
        let frame = decode_wire_frame(&swim_control_frame_bytes(&ack)).expect("frame");
        let WireFrame::Control(control) = frame else {
            panic!("expected control frame");
        };
        handle_swim_control_frame(mgr, HEW_FEATURE_SUPPORTS_GOSSIP, 10, conn10_token, &control);

        // Node 3 must now be DEAD in our membership view.
        assert_eq!(
            crate::cluster::hew_cluster_member_state(cluster, 3),
            crate::cluster::MEMBER_DEAD,
            "piggybacked DEAD gossip must transition node 3 to DEAD"
        );

        hew_connmgr_free(mgr);
        crate::cluster::hew_cluster_free(cluster);
        drop(Box::from_raw(transport_ptr));
        drop(Box::from_raw(sends));
    }
    drop(ops);
}
