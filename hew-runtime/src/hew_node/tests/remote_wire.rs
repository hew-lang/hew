//! Two-node remote send and ask tests over real transports.

use super::*;

#[cfg(feature = "encryption")]
#[test]
fn two_node_remote_send_delivery() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Node 1 starts first and remains CURRENT_NODE; both peers carry
    // mutually-authorized Noise identities required by v2 admission.
    let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(301, 302);

    // Ensure the scheduler is running so actor dispatches work.
    _real_sched = init_real_scheduler();

    // Temporarily set LOCAL_NODE_ID = 302 to assign a node-2 PID to the actor.
    // This makes the actor look remote from node1's routing perspective.
    SEND_PROBE_MSG_TYPE.store(0, Ordering::Release);
    crate::pid::hew_pid_set_local_node(302);
    let probe_actor = spawn_remote_test_actor(send_probe_dispatch);
    // Restore node1 as the local node before any routing decisions.
    crate::pid::hew_pid_set_local_node(301);
    assert!(!probe_actor.is_null(), "actor spawn failed");
    // SAFETY: actor was just spawned and is valid.
    let actor_id = unsafe { (*probe_actor).id };
    assert_eq!(
        crate::pid::hew_pid_node(actor_id),
        302,
        "actor PID must encode node2's ID"
    );

    let connect_addr = CString::new(format!("302@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this call.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers are valid.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // Fire-and-forget from node1 to the actor on node2.
    let msg_type_sent: i32 = 77;
    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: null payload / size 0 is valid for a bare signal message.
    let rc = unsafe {
        hew_node_send_location(
            node1.as_ptr(),
            &raw const target,
            inbound_test_request_codec(send_probe_dispatch, msg_type_sent),
            msg_type_sent,
            TEST_U32_REQUEST.cast(),
            std::mem::size_of::<u32>(),
        )
    };
    assert_eq!(rc, 0, "hew_node_send should succeed");

    let delivered = (0..100).any(|_| {
        #[expect(
            clippy::cast_sign_loss,
            reason = "msg_type_sent is a non-negative tag value"
        )]
        let got = SEND_PROBE_MSG_TYPE.load(Ordering::Acquire) == msg_type_sent as u32;
        if !got {
            thread::sleep(Duration::from_millis(20));
        }
        got
    });
    assert!(
        delivered,
        "actor on node2 did not receive the remote message"
    );

    // SAFETY: actor and nodes were allocated in this test and are valid.
    unsafe {
        let _ = crate::actor::hew_actor_free(probe_actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "quic")]
#[test]
fn two_node_remote_send_delivery_quic_mesh() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Node 1 starts first and remains CURRENT_NODE; the helper installs
    // mutually-authorized SPKI bindings for v2 admission.
    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(401, 402);

    // Ensure the scheduler is running so actor dispatches work.
    _real_sched = init_real_scheduler();

    // Temporarily set LOCAL_NODE_ID = 402 to assign a node-2 PID to the actor.
    // This makes the actor look remote from node1's routing perspective.
    SEND_PROBE_MSG_TYPE_QM.store(0, Ordering::Release);
    crate::pid::hew_pid_set_local_node(402);
    let probe_actor = spawn_remote_test_actor(send_probe_dispatch_qm);
    // Restore node1 as the local node before any routing decisions.
    crate::pid::hew_pid_set_local_node(401);
    assert!(!probe_actor.is_null(), "actor spawn failed");
    // SAFETY: actor was just spawned and is valid.
    let actor_id = unsafe { (*probe_actor).id };
    assert_eq!(
        crate::pid::hew_pid_node(actor_id),
        402,
        "actor PID must encode node2's ID"
    );

    let connect_addr = CString::new(format!("402@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this call.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers are valid.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // Fire-and-forget from node1 to the actor on node2 over quic_mesh.
    // This exercises a real CBOR-framed round-trip across the mTLS-pinned
    // mesh transport — not just process startup.
    let msg_type_sent: i32 = 91;
    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: null payload / size 0 is valid for a bare signal message.
    let rc = unsafe {
        hew_node_send_location(
            node1.as_ptr(),
            &raw const target,
            inbound_test_request_codec(send_probe_dispatch_qm, msg_type_sent),
            msg_type_sent,
            TEST_U32_REQUEST.cast(),
            std::mem::size_of::<u32>(),
        )
    };
    assert_eq!(rc, 0, "hew_node_send should succeed");

    let delivered = (0..200).any(|_| {
        #[expect(
            clippy::cast_sign_loss,
            reason = "msg_type_sent is a non-negative tag value"
        )]
        let got = SEND_PROBE_MSG_TYPE_QM.load(Ordering::Acquire) == msg_type_sent as u32;
        if !got {
            thread::sleep(Duration::from_millis(20));
        }
        got
    });
    assert!(
        delivered,
        "actor on node2 did not receive the remote message over quic_mesh"
    );

    // SAFETY: actor and nodes were allocated in this test and are valid.
    unsafe {
        let _ = crate::actor::hew_actor_free(probe_actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// Transitional wire routing: the v1 handshake advertises the node's
/// configured route slot. After an authorized quic-mesh handshake, the
/// responder must observe the initiator under that exact slot, and the
/// credential-bound authenticated route must resolve to the same slot. This
/// proves the two-part invariant the
/// `ctrl-frame-binds-to-authenticated-peer` lesson requires: the handshake
/// `NodeId` is BOTH credential-bound AND equal to the operator-pinned id.
///
/// If the initiator advertised anything other than its bound id, admission
/// on the responder would reject it (credential / `NodeId` mismatch) and no
/// connection would key to `ID_A` at all — so a successful observation of
/// `ID_A` here is a positive proof of the binding, not a coincidence.
#[cfg(feature = "quic")]
#[test]
fn wire_route_slot_peer_observes_configured_slot() {
    // Configured slots chosen to be unrelated to any PID-derived scheme.
    const ID_A: u16 = 331;
    const ID_B: u16 = 332;

    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();

    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(ID_A, ID_B);

    let connect_addr = CString::new(format!("{ID_B}@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and the connect address are valid for this attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until the end of the test.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // node2 (responder) must have admitted node1's inbound connection under
    // its configured route slot (331). Poll briefly: `wait_for_handshake`
    // only proves the socket is up; claim publication completes a moment
    // later inside admission.
    // SAFETY: node2 is valid and its conn_mgr is live while node2 runs.
    let mgr2 = unsafe { (*node2.as_ptr()).conn_mgr };
    assert!(!mgr2.is_null(), "node2 conn_mgr must be live");
    let mut conn_id = -1;
    let mut authenticated = 0u16;
    for _ in 0..100 {
        // SAFETY: mgr2 is a live manager pointer for the duration of the read.
        conn_id = unsafe { connection::hew_connmgr_conn_id_for_node(mgr2, ID_A) };
        if conn_id >= 0 {
            // SAFETY: mgr2 is live for this read.
            authenticated =
                connection::authenticated_peer_node_id_for_conn(unsafe { &*mgr2 }, conn_id);
            if authenticated == ID_A {
                break;
            }
        }
        thread::sleep(Duration::from_millis(20));
    }
    assert!(
        conn_id >= 0,
        "node2 must route the initiator under its configured id {ID_A}"
    );
    // SAFETY: mgr2 is a live manager pointer for the duration of these reads.
    let mgr2_ref = unsafe { &*mgr2 };
    assert_eq!(
        connection::peer_node_id_for_conn(mgr2_ref, conn_id),
        ID_A,
        "peer-observed route slot must equal the configured slot"
    );
    assert_eq!(
        authenticated, ID_A,
        "the observed NodeId must be credential-bound (authenticated), not merely declared"
    );

    // SAFETY: nodes were allocated in this test and remain valid here.
    unsafe {
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// Fail-closed: when the two in-process nodes do NOT mutually pin each
/// other's SPKIs, the mTLS handshake must fail and `hew_node_connect`
/// must surface a typed diagnostic rather than silently degrading or
/// hanging.
///
/// Per the M3 trust-bar: no silent fallback. The connect-with-retry loop
/// is therefore tolerable in the success test but here we want a single
/// `hew_node_connect` call to return -1 (the transport adapter sets
/// `last_error` to a `quic_mesh connect: …` diagnostic).
#[cfg(feature = "quic")]
#[test]
fn two_node_remote_send_quic_mesh_rejects_unknown_peer() {
    use crate::quic_mesh::MeshTls;

    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();

    // Build asymmetric TLS: node A pins no peer SPKIs, node B pins no
    // peer SPKIs. Either side rejects the other → fail-closed.
    let (tls_a, _spki_a) =
        MeshTls::self_signed(vec!["node-411".into()]).expect("tls_a self_signed");
    let (tls_b, _spki_b) =
        MeshTls::self_signed(vec!["node-412".into()]).expect("tls_b self_signed");

    let (node1, _node1_port) = start_quic_mesh_test_listener_node(411, tls_a);
    thread::sleep(Duration::from_millis(50));
    let (node2, node2_port) = start_quic_mesh_test_listener_node(412, tls_b);

    let connect_addr = CString::new(format!("412@127.0.0.1:{node2_port}")).unwrap();

    // Try once — the mTLS handshake must fail. We loop briefly to allow
    // for connection-attempt teardown, but every attempt must return -1.
    // (We deliberately do NOT call `connect_with_retry`, which would
    //  panic on persistent failure: here failure IS the success case.)
    let mut last_rc = 0;
    for _ in 0..3 {
        // SAFETY: node1 and connect_addr are valid for this call.
        last_rc = unsafe { hew_node_connect(node1.as_ptr(), connect_addr.as_ptr()) };
        if last_rc != 0 {
            break;
        }
        thread::sleep(Duration::from_millis(50));
    }
    assert_eq!(
        last_rc, -1,
        "quic_mesh connect to an SPKI-unpinned peer must fail-closed"
    );

    // Verify a typed diagnostic was emitted to LAST_ERROR. Use the C-API
    // because the runtime's `set_last_error` is internal to `hew-runtime`,
    // not the `hew_cabi::sink` last-error slot.
    let err_ptr = crate::hew_last_error();
    assert!(
        !err_ptr.is_null(),
        "quic_mesh fail-closed path must populate hew_last_error()"
    );
    // SAFETY: hew_last_error returns a thread-local C string valid until
    // the next set_last_error on this thread.
    let err = unsafe { std::ffi::CStr::from_ptr(err_ptr) }
        .to_string_lossy()
        .into_owned();
    assert!(
        err.contains("connect") || err.contains("quic_mesh") || err.contains("transport"),
        "expected a typed quic_mesh/transport diagnostic, got: {err:?}"
    );

    // SAFETY: nodes were allocated in this test and remain valid.
    unsafe {
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "quic")]
#[test]
fn two_node_remote_void_ask_returns_sentinel() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Cross-bound SPKI→NodeId credentials → Strict authorized connection
    // (inbound ask authority requires an authenticated peer, D9/D12).
    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(313, 314);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(314);
    let void_actor = spawn_remote_test_actor(void_ask_probe_dispatch);
    crate::pid::hew_pid_set_local_node(313);
    assert!(!void_actor.is_null(), "actor spawn failed");
    // SAFETY: the actor was just spawned successfully and remains valid here.
    let actor_id = unsafe { (*void_actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 314);

    let connect_addr = CString::new(format!("314@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and the connect address are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until the end of the test.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: this is a remote void ask; null payload/size are valid and no reply buffer is expected.
    let (reply, status) = unsafe {
        ask_for_test(
            &raw const target,
            inbound_test_codec(void_ask_probe_dispatch, 0),
            1,
            TEST_U32_REQUEST.cast_mut(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            0,
        )
    };
    assert!(status == AskError::None as i32 && reply.is_empty());
    assert_eq!(
        status,
        AskError::None as i32,
        "successful void ask must leave the error slot cleared"
    );

    // SAFETY: the actor and nodes were allocated in this test and are still valid here.
    unsafe {
        let _ = crate::actor::hew_actor_free(void_actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "quic")]
#[test]
fn two_node_remote_ask_reply() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();
    // Remote ask requires a registered codec for the payload msg_type.
    register_test_u32_codec(test_dispatch(), 1);

    // Node 1 (initiator) starts first → CURRENT_NODE = node1, LOCAL_NODE_ID = 311.
    // Node 2 (responder) starts after; CURRENT_NODE stays as node1. Both nodes
    // carry cross-bound SPKI→NodeId credentials so the connection admits
    // Strict — inbound ask authority (D9/D12) requires an authenticated peer.
    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(311, 312);

    // Ensure the scheduler is running so actor dispatches work.
    _real_sched = init_real_scheduler();

    // Temporarily set LOCAL_NODE_ID = 312 to assign a node-2 PID to the actor.
    crate::pid::hew_pid_set_local_node(312);
    let echo_actor = spawn_remote_test_actor(ask_probe_dispatch);
    // Restore node1 as the local node before any routing decisions.
    crate::pid::hew_pid_set_local_node(311);
    assert!(!echo_actor.is_null(), "actor spawn failed");
    // SAFETY: actor was just spawned and is valid.
    let actor_id = unsafe { (*echo_actor).id };
    assert_eq!(
        crate::pid::hew_pid_node(actor_id),
        312,
        "actor PID must encode node2's ID"
    );

    let connect_addr = CString::new(format!("312@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this call.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers are valid.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // Remote ask from node1 (CURRENT_NODE, LOCAL_NODE_ID=311) to actor on node2.
    //
    // Routing: target_node_id=312 ≠ local_node_id=311 → remote path.
    // routing_table[312] provides the outbound conn_id (populated during handshake).
    // On node2 the inbound router fires handle_inbound_ask with conn_mgr=node2.conn_mgr;
    // send_reply_envelope uses hew_connmgr_conn_id_for_node to find the accepted
    // connection whose peer_node_id == 311, enabling the reply to flow back to node1.
    let send_value: u32 = 21;
    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: send_value is a valid u32 on the stack; reply is sized-block-allocated, freed below.
    let (reply, status) = unsafe {
        ask_for_test(
            &raw const target,
            test_dispatch(),
            1,
            (&raw const send_value).cast::<c_void>().cast_mut(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            std::mem::size_of::<u32>(),
        )
    };

    assert!(
        status == AskError::None as i32,
        "remote ask should return a non-null reply"
    );
    let reply_value = u32::from_ne_bytes(reply[..4].try_into().expect("u32 reply"));
    assert_eq!(
        reply_value,
        send_value * 2,
        "echo-double should return 21 * 2 = 42"
    );

    // SAFETY: actor and nodes were allocated in this test and are valid.
    unsafe {
        let _ = crate::actor::hew_actor_free(echo_actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "quic")]
#[test]
fn two_node_remote_call_wakes_on_wire_reply() {
    // A coroutine remote call submits without blocking its caller; the
    // wire reply, delivered by the connection reader thread, wakes the
    // call's waker and the decoded reply is taken exactly once.
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();
    register_test_u32_codec(test_dispatch(), 1);

    // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(321, 322);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(322);
    let echo_actor = spawn_remote_test_actor(ask_probe_dispatch);
    crate::pid::hew_pid_set_local_node(321);
    assert!(!echo_actor.is_null(), "echo actor spawn failed");
    // SAFETY: actor was just spawned and is valid.
    let actor_id = unsafe { (*echo_actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 322);

    let connect_addr = CString::new(format!("322@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this call.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers are valid.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    let send_value: u32 = 21;
    let target = remote_pid_for_node(&node2, actor_id);
    let (readiness, waker) = crate::wake::blocking::Readiness::new();
    // SAFETY: send_value is a valid u32 on the stack; the waker is live.
    let call = unsafe {
        hew_remote_call_new(
            &raw const target,
            test_dispatch(),
            1,
            (&raw const send_value).cast::<c_void>().cast_mut(),
            std::mem::size_of::<u32>(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            waker.descriptor(),
        )
    };
    // SAFETY: `call` is live and exclusively driven here.
    while unsafe { hew_remote_call_poll(call) } == -1 {
        readiness.wait();
    }
    let mut reply_value: u32 = 0;
    // SAFETY: the call is ready and the output holds its u32 reply.
    let status = unsafe { hew_remote_call_take(call, (&raw mut reply_value).cast::<c_void>()) };
    // SAFETY: the call is released once after its take.
    unsafe { hew_remote_call_free(call) };
    assert_eq!(status, AskError::None as i32, "the wire reply decodes");
    assert_eq!(
        reply_value,
        send_value * 2,
        "echo-double should return 21 * 2 = 42"
    );

    // SAFETY: actors and nodes were allocated in this test and are valid.
    unsafe {
        let _ = crate::actor::hew_actor_free(echo_actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "quic")]
#[test]
fn two_node_inbound_orphaned_ask_reports_orphaned() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(315, 316);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(316);
    let actor = spawn_remote_test_actor(orphaned_void_ask_dispatch);
    crate::pid::hew_pid_set_local_node(315);
    assert!(!actor.is_null(), "actor spawn failed");
    // SAFETY: actor was just spawned and is valid here.
    let actor_id = unsafe { (*actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 316);

    let connect_addr = CString::new(format!("316@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until teardown.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: this is a remote void ask; null payload/size are valid.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const target,
            inbound_test_codec(orphaned_void_ask_dispatch, 0),
            1,
            TEST_U32_REQUEST.cast_mut(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            0,
        )
    };
    let err = status;

    assert!(
        status != AskError::None as i32,
        "orphaned inbound void ask must not return the void-success sentinel"
    );
    assert_eq!(
        err,
        AskError::OrphanedAsk as i32,
        "orphaned inbound ask must preserve the remote orphaned reason"
    );

    // SAFETY: actor and nodes were allocated in this test and remain valid here.
    unsafe {
        let _ = crate::actor::hew_actor_free(actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "quic")]
#[test]
fn two_node_inbound_actor_stopped_reports_actor_stopped() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(317, 318);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(318);
    let actor = spawn_remote_test_actor(noop_dispatch);
    crate::pid::hew_pid_set_local_node(317);
    assert!(!actor.is_null(), "actor spawn failed");
    // SAFETY: actor was just spawned and is valid here.
    let actor_id = unsafe { (*actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 318);

    let connect_addr = CString::new(format!("318@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until teardown.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // SAFETY: actor was spawned above and remains valid while stopped here.
    unsafe { crate::actor::hew_actor_stop(actor) };

    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: this is a remote void ask; null payload/size are valid.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const target,
            inbound_test_codec(noop_dispatch, 0),
            1,
            TEST_U32_REQUEST.cast_mut(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            0,
        )
    };
    let err = status;

    assert!(
        status != AskError::None as i32,
        "inbound ask to a stopped actor must not return the void-success sentinel"
    );
    assert_eq!(
        err,
        AskError::ActorStopped as i32,
        "stopped inbound actor ask must preserve the remote actor-stopped reason"
    );

    // SAFETY: actor and nodes were allocated in this test and remain valid here.
    unsafe {
        let _ = crate::actor::hew_actor_free(actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// A remote ask targeting an actor id that was never spawned at all (a
/// bogus serial under the peer's real node identity and session) must
/// report `ActorStopped`/`Dead`, the same as a genuinely freed one below.
/// Before the fix, the connection reader's `location_matches_local` gate
/// silently dropped ANY envelope whose target wasn't a tracked live actor
/// — never spawned or already freed alike — before `node_inbound_router`
/// ever ran, so the asking peer only ever observed a bare `Timeout`.
#[cfg(feature = "quic")]
#[test]
fn two_node_inbound_ask_to_never_spawned_actor_reports_dead() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();
    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(321, 322);
    _real_sched = init_real_scheduler();
    crate::pid::hew_pid_set_local_node(322);
    // A live actor establishes node2's identity/session for `Location`
    // construction below; its own id is never used as the ask target.
    let anchor = spawn_remote_test_actor(noop_dispatch);
    crate::pid::hew_pid_set_local_node(321);
    assert!(!anchor.is_null(), "anchor actor spawn failed");
    // SAFETY: anchor was just spawned and is valid here.
    let anchor_id = unsafe { (*anchor).id };
    let bogus_serial = crate::pid::hew_pid_serial(anchor_id) + 1_000_000;

    let connect_addr = CString::new(format!("322@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until teardown.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // SAFETY: node2 is a live test node.
    let node2_ref = unsafe { node2.as_ptr().as_ref() }.expect("test node must be live");
    let location = Location::new(
        node2_ref
            .auth
            .node_identity()
            .expect("authorized test node has an identity"),
        bogus_serial,
        node2_ref
            .auth
            .session_incarnation()
            .expect("authorized test node has a session"),
    )
    .expect("bogus location must still be well-formed");
    let target = HewRemotePid::from(location);

    // SAFETY: this is a remote void ask; null payload/size are valid.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const target,
            inbound_test_codec(noop_dispatch, 0),
            1,
            TEST_U32_REQUEST.cast_mut(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            0,
        )
    };

    assert!(
        status != AskError::None as i32,
        "inbound ask to a never-spawned actor must not return the void-success sentinel"
    );
    assert_eq!(
        status,
        AskError::ActorStopped as i32,
        "a remote ask to a never-spawned actor must report ActorStopped, not a silent drop"
    );
    assert_eq!(
        crate::internal::types::hew_ask_error_translate_for_public_result(status),
        3,
        "a dead remote actor must surface the same public ActorError.Dead ordinal (3) the \
         local ask route reports (D526)"
    );

    // SAFETY: nodes were allocated in this test and remain valid here.
    unsafe {
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// A remote ask targeting an actor that is genuinely dead — fully freed on
/// its own node, not merely stopped-but-still-tracked (the scenario above)
/// — must report the same reason a local ask to a dead target reports
/// (D526's `ActorError.Dead`), not `DecodeFailure`. Before this fix, a
/// freed actor had no tracked dispatch pointer at all, and
/// `handle_inbound_ask` read that absence as a codec-lookup failure.
#[cfg(feature = "quic")]
#[test]
fn two_node_inbound_dead_actor_ask_reports_dead_not_decode_failure() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(319, 320);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(320);
    let actor = spawn_remote_test_actor(noop_dispatch);
    crate::pid::hew_pid_set_local_node(319);
    assert!(!actor.is_null(), "actor spawn failed");
    // SAFETY: actor was just spawned and is valid here.
    let actor_id = unsafe { (*actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 320);

    let connect_addr = CString::new(format!("320@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until teardown.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    let target = remote_pid_for_node(&node2, actor_id);

    // Fully free the actor — not merely stop it — before the ask arrives,
    // so `dispatch_ptr_by_id` on the receiving node has no tracked entry
    // at all (the dead case), distinct from the stopped-but-tracked case
    // `two_node_inbound_actor_stopped_reports_actor_stopped` covers.
    // SAFETY: actor was spawned above and remains valid until freed here.
    unsafe {
        crate::actor::hew_actor_stop(actor);
        let _ = crate::actor::hew_actor_free(actor);
    }

    // SAFETY: this is a remote void ask; null payload/size are valid.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const target,
            inbound_test_codec(noop_dispatch, 0),
            1,
            TEST_U32_REQUEST.cast_mut(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            0,
        )
    };

    assert!(
        status != AskError::None as i32,
        "inbound ask to a dead actor must not return the void-success sentinel"
    );
    assert_eq!(
        status,
        AskError::ActorStopped as i32,
        "a remote ask to a dead (fully freed) actor must report ActorStopped, matching a \
         local dead target's reason rather than DecodeFailure"
    );
    assert_eq!(
        crate::internal::types::hew_ask_error_translate_for_public_result(status),
        3,
        "a dead remote actor must surface the same public ActorError.Dead ordinal (3) the \
         local ask route reports (D526)"
    );

    // SAFETY: nodes were allocated in this test and remain valid here.
    unsafe {
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "quic")]
#[test]
fn two_node_inbound_mailbox_full_reports_mailbox_full() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(326, 327);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(327);
    let actor = spawn_bounded_test_actor(noop_dispatch, 1);
    crate::pid::hew_pid_set_local_node(326);
    assert!(!actor.is_null(), "actor spawn failed");
    // SAFETY: actor was just spawned and is valid here.
    let actor_id = unsafe { (*actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 327);

    // SAFETY: actor is valid; mailbox pointer is valid for the actor lifetime.
    let mailbox = unsafe { (*actor).mailbox.cast::<crate::mailbox::HewMailbox>() };
    // SAFETY: mailbox is a valid bounded mailbox pointer; the null payload is intentional.
    let pre_fill = unsafe { crate::mailbox::hew_mailbox_send(mailbox, 1, ptr::null_mut(), 0) };
    assert_eq!(
        pre_fill, 0,
        "pre-fill into empty bounded mailbox must succeed"
    );

    let connect_addr = CString::new(format!("327@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until teardown.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: this is a remote void ask; null payload/size are valid.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const target,
            inbound_test_codec(noop_dispatch, 0),
            1,
            TEST_U32_REQUEST.cast_mut(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            0,
        )
    };
    let err = status;

    assert!(
        status != AskError::None as i32,
        "full-mailbox remote ask must return null"
    );
    assert_eq!(
        err,
        AskError::MailboxFull as i32,
        "full-mailbox remote ask must propagate MailboxFull, not WorkerAtCapacity or ActorStopped"
    );

    // SAFETY: actor and nodes were allocated in this test and remain valid here.
    unsafe {
        crate::actor::hew_actor_stop(actor);
        let _ = crate::actor::hew_actor_free(actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "quic")]
#[test]
fn two_node_worker_limit_still_reports_worker_at_capacity() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(319, 320);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(320);
    let actor = spawn_remote_test_actor(noop_dispatch);
    crate::pid::hew_pid_set_local_node(319);
    assert!(!actor.is_null(), "actor spawn failed");
    // SAFETY: actor was just spawned and is valid here.
    let actor_id = unsafe { (*actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 320);

    let connect_addr = CString::new(format!("320@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until teardown.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    let saved = INBOUND_ASK_ACTIVE.swap(INBOUND_ASK_WORKER_LIMIT, Ordering::AcqRel);
    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: this is a remote void ask; null payload/size are valid.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const target,
            test_dispatch(),
            1,
            ptr::null_mut(),
            0,
            TEST_REMOTE_ASK_TIMEOUT_MS,
            0,
        )
    };
    let err = status;
    INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);

    assert!(
        status != AskError::None as i32,
        "worker-limit rejection must not return the void-success sentinel"
    );
    assert_eq!(
        err,
        AskError::WorkerAtCapacity as i32,
        "worker-limit rejection must keep reporting WorkerAtCapacity"
    );

    // SAFETY: actor and nodes were allocated in this test and remain valid here.
    unsafe {
        let _ = crate::actor::hew_actor_free(actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "encryption")]
#[test]
fn two_node_pre_rejection_peer_gets_timeout_not_wrong_error() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Authenticated pair: an outbound ask is refused (Unauthorized) before it
    // is sent unless the target connection is the exact authenticated owner
    // (issue #2652). The pre-rejection fallback under test needs the ask to
    // actually reach node2, so the peers are mutually credentialed.
    let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(330, 331);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(331);
    let actor = spawn_remote_test_actor(noop_dispatch);
    crate::pid::hew_pid_set_local_node(330);
    assert!(!actor.is_null(), "actor spawn failed");
    // SAFETY: actor was just spawned and is valid here.
    let actor_id = unsafe { (*actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 331);

    let connect_addr = CString::new(format!("331@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until teardown.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // SAFETY: node2 conn_mgr is live for the duration of this test.
    let peer_flags =
        unsafe { connection::hew_connmgr_feature_flags_for_node((*node2.as_ptr()).conn_mgr, 330) };
    assert!(
        connection::supports_ask_rejection(peer_flags),
        "handshake should record ask-rejection support before we simulate the old peer"
    );
    // SAFETY: node2 conn_mgr is live; this mutates only test state.
    unsafe {
        connection::hew_connmgr_force_peer_flags_for_node(
            (*node2.as_ptr()).conn_mgr,
            330,
            peer_flags & !connection::HEW_FEATURE_SUPPORTS_ASK_REJECTION,
        );
    }
    // SAFETY: node2 conn_mgr remains live while the nodes are running in this scope.
    let stripped_flags =
        unsafe { connection::hew_connmgr_feature_flags_for_node((*node2.as_ptr()).conn_mgr, 330) };
    assert!(
        !connection::supports_ask_rejection(stripped_flags),
        "test setup must strip ask-rejection support from node2's view of node1"
    );

    let saved = INBOUND_ASK_ACTIVE.swap(INBOUND_ASK_WORKER_LIMIT, Ordering::AcqRel);
    let ask_start = std::time::Instant::now();
    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: this is a remote void ask; null payload/size are valid.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const target,
            test_dispatch(),
            1,
            ptr::null_mut(),
            0,
            TEST_REMOTE_ASK_TIMEOUT_MS,
            0,
        )
    };
    let err = status;
    INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);

    assert!(
        status != AskError::None as i32,
        "pre-rejection peer fallback must return null instead of a void-success sentinel"
    );
    assert_eq!(
        err,
        AskError::Timeout as i32,
        "pre-rejection peer fallback must time out instead of returning WorkerAtCapacity"
    );
    assert!(
        ask_start.elapsed() < Duration::from_millis(TEST_REMOTE_ASK_TIMEOUT_MS * 3),
        "fallback ask should resolve near the timeout deadline, not block indefinitely"
    );

    // SAFETY: actor and nodes were allocated in this test and remain valid here.
    unsafe {
        let _ = crate::actor::hew_actor_free(actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "quic")]
#[test]
fn two_node_remote_nonvoid_empty_reply_returns_null() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(317, 318);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(318);
    let empty_reply_actor = spawn_remote_test_actor(void_ask_probe_dispatch);
    crate::pid::hew_pid_set_local_node(317);
    assert!(!empty_reply_actor.is_null(), "actor spawn failed");
    // SAFETY: the actor was just spawned successfully and remains valid here.
    let actor_id = unsafe { (*empty_reply_actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 318);

    let connect_addr = CString::new(format!("318@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and the connect address are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until the end of the test.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: non-void remote ask expects a u32-sized reply; an empty success must fail closed.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const target,
            inbound_test_codec(void_ask_probe_dispatch, 0),
            1,
            TEST_U32_REQUEST.cast_mut(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            std::mem::size_of::<u32>(),
        )
    };
    assert!(
        status != AskError::None as i32,
        "non-void remote ask should return null on an empty reply payload"
    );
    assert_eq!(
        status,
        AskError::PayloadSizeMismatch as i32,
        "empty reply to non-void ask should report PayloadSizeMismatch"
    );

    // SAFETY: the actor and nodes were allocated in this test and are still valid here.
    unsafe {
        let _ = crate::actor::hew_actor_free(empty_reply_actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "encryption")]
#[test]
fn two_node_remote_ask_timeout_reports_timeout() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Authenticated pair (issue #2652): a genuine timeout requires the ask to
    // be sent to an authorized peer and left unanswered — an unverified target
    // would instead fail closed with Unauthorized before the send.
    let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(328, 329);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(329);
    let silent_actor = spawn_remote_test_actor(blocked_ask_probe_dispatch);
    crate::pid::hew_pid_set_local_node(328);
    assert!(!silent_actor.is_null(), "silent actor spawn failed");
    // SAFETY: the actor was just spawned successfully and remains valid here.
    let actor_id = unsafe { (*silent_actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 329);

    let connect_addr = CString::new(format!("329@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and the connect address are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until the end of the test.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    let ask_start = std::time::Instant::now();
    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: the actor pid and null payload are valid for this remote ask probe.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const target,
            inbound_test_codec(blocked_ask_probe_dispatch, std::mem::size_of::<u32>()),
            1,
            TEST_U32_REQUEST.cast_mut(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            std::mem::size_of::<u32>(),
        )
    };
    let err = status;

    assert!(
        status != AskError::None as i32,
        "timed-out remote ask must return null"
    );
    assert_eq!(
        err,
        AskError::Timeout as i32,
        "remote ask that receives no reply must report Timeout: {:?}",
        crate::stream_error::take_last_error()
    );
    assert!(
        ask_start.elapsed() < Duration::from_millis(TEST_REMOTE_ASK_TIMEOUT_MS * 3),
        "ask should complete near the timeout deadline, not block indefinitely"
    );

    // SAFETY: the actor and nodes were allocated in this test and remain valid here.
    unsafe {
        crate::actor::hew_actor_stop(silent_actor);
        let _ = crate::actor::hew_actor_free(silent_actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// V2 distributed admission rejects a pair with local identities but no
/// configured peer bindings before either side can install a route or create
/// an outbound ask.
#[cfg(feature = "encryption")]
#[test]
fn unconfigured_peer_pair_rejected_during_connection_admission() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();

    // Each TestNode has a real local identity, but neither snapshot binds the
    // other peer's Noise key to its claimed route slot.
    let node1_bind = CString::new("127.0.0.1:0").unwrap();
    // SAFETY: bind address is a valid C string for the duration of this test.
    let node1 = unsafe { TestNode::new(342, &node1_bind) };
    assert!(!node1.as_ptr().is_null());
    // SAFETY: node1 came from TestNode::new and is valid for start-up here.
    unsafe {
        assert_eq!(hew_node_start(node1.as_ptr()), 0);
    }
    thread::sleep(Duration::from_millis(50));
    let (node2, node2_port) = start_tcp_test_listener_node(343);

    let connect_addr = CString::new(format!("343@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and the connect address are valid for this admission attempt.
    let connect_rc = unsafe { hew_node_connect(node1.as_ptr(), connect_addr.as_ptr()) };
    assert_eq!(
        connect_rc,
        -1,
        "unconfigured v2 peers must be rejected during connection admission: {:?}",
        crate::stream_error::take_last_error()
    );

    // SAFETY: both connection managers remain live until teardown below.
    unsafe {
        assert_eq!(
            connection::hew_connmgr_conn_id_for_node((*node1.as_ptr()).conn_mgr, 343),
            -1,
            "initiator must not install a route for the rejected peer"
        );
        assert_eq!(
            connection::hew_connmgr_conn_id_for_node((*node2.as_ptr()).conn_mgr, 342),
            -1,
            "responder must not install a route for the rejected peer"
        );
        assert_eq!(
            connection::hew_connmgr_count((*node1.as_ptr()).conn_mgr),
            0,
            "initiator must retain no rejected connection"
        );
        assert_eq!(
            connection::hew_connmgr_count((*node2.as_ptr()).conn_mgr),
            0,
            "responder must retain no rejected connection"
        );
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "encryption")]
#[test]
fn node_stop_wakes_pending_remote_ask() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Authenticated pair (issue #2652): the pending ask under test must reach
    // the reply table, which only happens when the outbound ask is authorized.
    let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(315, 316);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(316);
    let blocked_actor = spawn_remote_test_actor(blocked_ask_probe_dispatch);
    crate::pid::hew_pid_set_local_node(315);
    assert!(!blocked_actor.is_null(), "actor spawn failed");
    // SAFETY: the actor was just spawned successfully and remains valid here.
    let actor_id = unsafe { (*blocked_actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 316);

    let connect_addr = CString::new(format!("316@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and the connect address are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until the end of the test.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: the actor pid and null payload are valid for this remote ask probe.
    let ask_handle = thread::spawn(move || unsafe {
        let (_, err) = ask_for_test(
            &raw const target,
            inbound_test_codec(blocked_ask_probe_dispatch, std::mem::size_of::<u32>()),
            1,
            TEST_U32_REQUEST.cast_mut(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            std::mem::size_of::<u32>(),
        );
        (usize::from(err == AskError::None as i32), err)
    });

    let pending_seen = (0..100).any(|_| {
        let guard = reply_table()
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let seen = !guard.is_empty();
        drop(guard);
        if !seen {
            thread::sleep(Duration::from_millis(10));
        }
        seen
    });
    assert!(
        pending_seen,
        "remote ask never reached the pending reply table"
    );

    let stop_started = std::time::Instant::now();
    // SAFETY: node1 remains valid here and stopping it is the behavior under test.
    unsafe {
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
    }
    let (replied, ask_err) = ask_handle.join().expect("ask thread panicked");
    assert!(replied == 0, "stopped node should fail pending remote asks");
    assert_eq!(
        ask_err,
        AskError::ConnectionDropped as i32,
        "node stop should report ConnectionDropped on pending asks"
    );
    assert!(
        stop_started.elapsed() < Duration::from_secs(2),
        "pending remote ask should wake promptly when the node stops"
    );

    // SAFETY: the actor and node2 were allocated in this test and remain valid here.
    unsafe {
        let _ = crate::actor::hew_actor_free(blocked_actor);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[cfg(feature = "encryption")]
#[test]
fn connection_drop_wakes_pending_remote_ask() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Authenticated pair (issue #2652): the pending ask must register against
    // the outbound connection, which requires an authorized target.
    let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(320, 321);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(321);
    let blocked_actor = spawn_remote_test_actor(blocked_ask_probe_dispatch);
    crate::pid::hew_pid_set_local_node(320);
    assert!(!blocked_actor.is_null(), "actor spawn failed");
    // SAFETY: the actor was just spawned successfully and remains valid here.
    let actor_id = unsafe { (*blocked_actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 321);

    let connect_addr = CString::new(format!("321@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and the connect address are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until the end of the test.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // SAFETY: both nodes are running and their connection managers are valid here.
    let outbound_conn_id =
        unsafe { connection::hew_connmgr_conn_id_for_node((*node1.as_ptr()).conn_mgr, 321) };
    // SAFETY: both nodes are running and their connection managers are valid here.
    let accepted_conn_id =
        unsafe { connection::hew_connmgr_conn_id_for_node((*node2.as_ptr()).conn_mgr, 320) };
    assert!(
        outbound_conn_id >= 0,
        "initiator outbound connection missing"
    );
    assert!(
        accepted_conn_id >= 0,
        "responder accepted connection missing"
    );
    // SAFETY: node1 remains valid here and its connection manager stays alive until teardown below.
    let outbound_key = ConnectionKey::new(
        unsafe { (*node1.as_ptr()).conn_mgr.cast_const() },
        outbound_conn_id,
    );

    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: the actor pid and null payload are valid for this remote ask probe.
    let ask_handle = thread::spawn(move || unsafe {
        let (_, err) = ask_for_test(
            &raw const target,
            inbound_test_codec(blocked_ask_probe_dispatch, std::mem::size_of::<u32>()),
            1,
            TEST_U32_REQUEST.cast_mut(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            std::mem::size_of::<u32>(),
        );
        (usize::from(err == AskError::None as i32), err)
    });

    let pending_seen = (0..100).any(|_| {
        let guard = reply_table()
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let seen = guard
            .values()
            .any(|pending| pending.connection == outbound_key);
        drop(guard);
        if !seen {
            thread::sleep(Duration::from_millis(10));
        }
        seen
    });
    assert!(
        pending_seen,
        "remote ask never registered against the outbound connection"
    );

    // SAFETY: node2 remains valid here and removing its accepted connection simulates a peer drop.
    unsafe {
        assert_eq!(
            connection::hew_connmgr_remove((*node2.as_ptr()).conn_mgr, accepted_conn_id),
            0
        );
    }
    let (replied, ask_err) = ask_handle.join().expect("ask thread panicked");
    assert!(
        replied == 0,
        "connection drop should fail the pending remote ask"
    );
    assert_eq!(
        ask_err,
        AskError::ConnectionDropped as i32,
        "connection drop should report ConnectionDropped"
    );
    // SAFETY: the actor and nodes were allocated in this test and remain valid here.
    unsafe {
        let _ = crate::actor::hew_actor_free(blocked_actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// A SWIM-declared-DEAD peer (socket still nominally open)
/// must resolve a pending remote ask IMMEDIATELY with `AskError::Partition`,
/// not hang to the caller's deadline. The connection-drop test above proves
/// the socket-drop cause; this proves the SWIM-DEAD cause through the
/// node-side fan-out (`fail_remote_asks_for_node`), with the socket left
/// open so the only thing that resolves the ask is the failure-detector
/// verdict — distinct from `ConnectionDropped`.
#[cfg(feature = "encryption")]
#[test]
fn swim_dead_wakes_pending_remote_ask_with_partition() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Authenticated pair (issue #2652): the pending ask must reach the reply
    // table so the SWIM-DEAD fan-out can resolve it with Partition; that only
    // happens for an authorized outbound target.
    let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(330, 331);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(331);
    let blocked_actor = spawn_remote_test_actor(blocked_ask_probe_dispatch);
    crate::pid::hew_pid_set_local_node(330);
    assert!(!blocked_actor.is_null(), "actor spawn failed");
    // SAFETY: the actor was just spawned successfully and remains valid here.
    let actor_id = unsafe { (*blocked_actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 331);

    let connect_addr = CString::new(format!("331@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and the connect address are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until the end of the test.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // SAFETY: both nodes are running and their connection managers are valid here.
    let outbound_conn_id =
        unsafe { connection::hew_connmgr_conn_id_for_node((*node1.as_ptr()).conn_mgr, 331) };
    assert!(
        outbound_conn_id >= 0,
        "initiator outbound connection missing"
    );
    // SAFETY: node1 remains valid here and its connection manager stays alive until teardown.
    let outbound_key = ConnectionKey::new(
        unsafe { (*node1.as_ptr()).conn_mgr.cast_const() },
        outbound_conn_id,
    );

    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: the actor pid and null payload are valid for this remote ask probe.
    let ask_handle = thread::spawn(move || unsafe {
        let (_, err) = ask_for_test(
            &raw const target,
            inbound_test_codec(blocked_ask_probe_dispatch, std::mem::size_of::<u32>()),
            1,
            TEST_U32_REQUEST.cast_mut(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            std::mem::size_of::<u32>(),
        );
        (usize::from(err == AskError::None as i32), err)
    });

    let pending_seen = (0..100).any(|_| {
        let guard = reply_table()
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let seen = guard
            .values()
            .any(|pending| pending.connection == outbound_key);
        drop(guard);
        if !seen {
            thread::sleep(Duration::from_millis(10));
        }
        seen
    });
    assert!(
        pending_seen,
        "remote ask never registered against the outbound connection"
    );

    // Declare node 331 DEAD via the node-side partition fan-out WITHOUT
    // touching the socket — the SWIM/phi-accrual verdict, not a TCP drop.
    let dead_declared = std::time::Instant::now();
    fail_remote_asks_for_node(331);

    let (replied, ask_err) = ask_handle.join().expect("ask thread panicked");
    assert!(
        replied == 0,
        "SWIM-DEAD must fail the pending remote ask (no fabricated reply)"
    );
    // The teeth: the exact Partition discriminant (14), distinct from
    // ConnectionDropped (6) and Timeout (5).
    assert_eq!(
        ask_err,
        AskError::Partition as i32,
        "SWIM-DEAD should report Partition, not ConnectionDropped or Timeout"
    );
    // Bounded-time: a hang would blow the full ask timeout. The fan-out is
    // synchronous, so resolution is effectively immediate.
    assert!(
        dead_declared.elapsed() < Duration::from_millis(TEST_REMOTE_ASK_TIMEOUT_MS / 2),
        "pending remote ask should resolve well before the full ask timeout"
    );
    // Exactly-once: the entry was drained on the first fan-out, so a second
    // verdict (or a racing socket drop) finds nothing — assert the map is
    // empty for this connection and a repeat fan-out is a no-op.
    {
        let map = reply_table()
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        assert!(
            !map.values().any(|p| p.connection == outbound_key),
            "partition fan-out must remove the pending entry (no leak, exactly-once)"
        );
    }
    fail_remote_asks_for_node(331); // second verdict: no panic, no double-wake.

    // SAFETY: the actor and nodes were allocated in this test and remain valid here.
    unsafe {
        let _ = crate::actor::hew_actor_free(blocked_actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}
