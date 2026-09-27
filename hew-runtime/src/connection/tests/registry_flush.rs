//! Parked registry-gossip flush and retry tests.

use super::*;

/// Shared state for the failed-flush regression test: fails the first
/// `fail_remaining` sends, records the rest.
struct FailingOnceSends {
    fail_remaining: AtomicUsize,
    sends: Mutex<Vec<(c_int, Vec<u8>)>>,
}

unsafe extern "C" fn fail_once_then_record_send(
    impl_ptr: *mut std::ffi::c_void,
    conn_id: c_int,
    data: *const std::ffi::c_void,
    len: usize,
) -> c_int {
    // SAFETY: test installs a FailingOnceSends as the transport impl payload.
    let state = unsafe { &*(impl_ptr.cast::<FailingOnceSends>()) };
    if state
        .fail_remaining
        .fetch_update(Ordering::AcqRel, Ordering::Acquire, |n| n.checked_sub(1))
        .is_ok()
    {
        return -1;
    }
    // SAFETY: send_preencoded_on_manager passes an encoded frame valid for len bytes.
    let bytes = unsafe { std::slice::from_raw_parts(data.cast::<u8>(), len) }.to_vec();
    state
        .sends
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .push((conn_id, bytes));
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "test payload lengths fit c_int"
    )]
    {
        len as c_int
    }
}

/// A transiently failed connection-establish gossip flush must not lose the
/// cluster's registered names (the third lookup-unresolved mechanism): the
/// failed frames are PARKED, a stale admission token cannot consume them,
/// and the connection's next inbound-frame retry delivers them.
#[test]
fn failed_gossip_flush_parks_frames_and_retry_delivers() {
    let state = Box::into_raw(Box::new(FailingOnceSends {
        fail_remaining: AtomicUsize::new(1),
        sends: Mutex::new(Vec::new()),
    }));
    let ops = Box::new(crate::transport::HewTransportOps {
        connect: None,
        listen: None,
        accept: None,
        send: Some(fail_once_then_record_send),
        recv: None,
        close_conn: None,
        destroy: None,
    });
    let transport_ptr = Box::into_raw(Box::new(HewTransport {
        ops: &raw const *ops,
        r#impl: state.cast(),
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

        let mut peer = ConnectionActor::new(10);
        peer.peer_node_id = 2;
        peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
        peer.posture = crate::peer_binding::Posture::Strict;
        peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(peer));
        let token = test_publish_claim(&*mgr, 2, 10);

        (&*cluster).emit_registry_add("svc", test_location(1, 0x77));

        // Initial flush: the send fails; the frame must be parked, not lost.
        flush_registry_gossip_to_connection(&*mgr, 10, token, HEW_FEATURE_SUPPORTS_GOSSIP);
        assert!(
            (*state)
                .sends
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .is_empty(),
            "the failed initial send must record nothing"
        );
        assert_eq!(
            (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
            1,
            "the failed flush must park its frames"
        );

        // A stale admission token (a reused conn_id's old reader) must not
        // consume the parked frames.
        retry_pending_registry_flush(&*mgr, 10, token + 1);
        assert_eq!(
            (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
            1,
            "a stale token must not consume the parked flush"
        );

        // The connection's own retry (next inbound frame) delivers.
        retry_pending_registry_flush(&*mgr, 10, token);
        {
            let sends = (*state)
                .sends
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            assert_eq!(sends.len(), 1, "the retry must deliver the parked frame");
            assert_eq!(sends[0].0, 10);
            let WireFrame::Control(ctrl) =
                decode_wire_frame(&sends[0].1).expect("delivered frame decodes")
            else {
                panic!("delivered frame must be a control frame");
            };
            assert_eq!(ctrl.ctrl_kind, CTRL_REGISTRY_GOSSIP);
            let payload = decode_registry_gossip_payload(&ctrl.payload).expect("gossip payload");
            assert_eq!(payload.name, "svc");
            assert_eq!(payload.op, REGISTRY_GOSSIP_OP_ADD);
        }
        assert_eq!(
            (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
            0,
            "a delivered retry must clear the parked entry"
        );

        hew_connmgr_free(mgr);
        crate::cluster::hew_cluster_free(cluster);
        drop(Box::from_raw(transport_ptr));
        drop(Box::from_raw(state));
    }
    drop(ops);
}

/// Ordering safety across a failed flush (the stale-replay hazard): a
/// broadcast REMOVE for a name whose ADD is still parked must queue BEHIND
/// the parked ADD, and the retry must deliver both in original order — the
/// receiver's final state is the REMOVE, never a replayed stale ADD.
#[test]
fn broadcast_parks_behind_undelivered_flush_preserving_order() {
    let state = Box::into_raw(Box::new(FailingOnceSends {
        fail_remaining: AtomicUsize::new(1),
        sends: Mutex::new(Vec::new()),
    }));
    let ops = Box::new(crate::transport::HewTransportOps {
        connect: None,
        listen: None,
        accept: None,
        send: Some(fail_once_then_record_send),
        recv: None,
        close_conn: None,
        destroy: None,
    });
    let transport_ptr = Box::into_raw(Box::new(HewTransport {
        ops: &raw const *ops,
        r#impl: state.cast(),
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

        let mut peer = ConnectionActor::new(10);
        peer.peer_node_id = 2;
        peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
        peer.posture = crate::peer_binding::Posture::Strict;
        peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(peer));
        let token = test_publish_claim(&*mgr, 2, 10);

        let location = test_location(1, 0x88);
        (&*cluster).emit_registry_add("svc", location);
        // The ADD flush fails and parks.
        flush_registry_gossip_to_connection(&*mgr, 10, token, HEW_FEATURE_SUPPORTS_GOSSIP);
        assert_eq!(
            (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
            1,
            "the failed ADD must park"
        );

        // A later REMOVE broadcast must park BEHIND it, not overtake it.
        assert_eq!(
            hew_connmgr_broadcast_registry_gossip(mgr, "svc", location, false),
            0,
            "the REMOVE must not report a direct send while the ADD is parked"
        );
        assert!(
            (*state)
                .sends
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .is_empty(),
            "nothing may reach the wire before the retry drains in order"
        );

        // The retry drains BOTH, in original order: ADD then REMOVE.
        retry_pending_registry_flush(&*mgr, 10, token);
        {
            let sends = (*state)
                .sends
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let ops_seen: Vec<u8> = sends
                .iter()
                .map(|(_, bytes)| {
                    let WireFrame::Control(ctrl) = decode_wire_frame(bytes).expect("frame decodes")
                    else {
                        panic!("expected control frame");
                    };
                    decode_registry_gossip_payload(&ctrl.payload)
                        .expect("gossip payload")
                        .op
                })
                .collect();
            assert_eq!(
                ops_seen,
                vec![REGISTRY_GOSSIP_OP_ADD, REGISTRY_GOSSIP_OP_REMOVE],
                "the drain must preserve ADD-before-REMOVE order"
            );
        }
        assert_eq!(
            (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
            0,
            "a full drain clears the parked entry"
        );

        hew_connmgr_free(mgr);
        crate::cluster::hew_cluster_free(cluster);
        drop(Box::from_raw(transport_ptr));
        drop(Box::from_raw(state));
    }
    drop(ops);
}

/// The retry budget is BOUNDED: after `MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS`
/// failed drains the parked frames are dropped fail-closed (with a
/// diagnostic), never retried forever.
#[test]
fn retry_attempts_ceiling_drops_parked_frames() {
    let state = Box::into_raw(Box::new(FailingOnceSends {
        fail_remaining: AtomicUsize::new(usize::MAX),
        sends: Mutex::new(Vec::new()),
    }));
    let ops = Box::new(crate::transport::HewTransportOps {
        connect: None,
        listen: None,
        accept: None,
        send: Some(fail_once_then_record_send),
        recv: None,
        close_conn: None,
        destroy: None,
    });
    let transport_ptr = Box::into_raw(Box::new(HewTransport {
        ops: &raw const *ops,
        r#impl: state.cast(),
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

        let mut peer = ConnectionActor::new(10);
        peer.peer_node_id = 2;
        peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
        peer.posture = crate::peer_binding::Posture::Strict;
        peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        (&*mgr).connections.access(|conns| conns.push(peer));
        let token = test_publish_claim(&*mgr, 2, 10);

        (&*cluster).emit_registry_add("svc", test_location(1, 0x99));
        flush_registry_gossip_to_connection(&*mgr, 10, token, HEW_FEATURE_SUPPORTS_GOSSIP);
        assert_eq!(
            (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
            1
        );

        // Every retry fails; the ceiling must eventually drop the entry.
        for _ in 0..MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS {
            retry_pending_registry_flush(&*mgr, 10, token);
            assert_eq!(
                (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
                1,
                "within budget, a failed drain re-parks"
            );
        }
        let _ = take_hew_last_error();
        retry_pending_registry_flush(&*mgr, 10, token);
        assert_eq!(
            (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
            0,
            "the attempt over budget must drop the parked entry"
        );
        let diag = take_hew_last_error().expect("budget exhaustion must leave a diagnostic");
        assert!(
            diag.contains("retry budget exhausted"),
            "diagnostic must name the budget exhaustion, got: {diag}"
        );
        assert!(
            (*state)
                .sends
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .is_empty(),
            "no frame ever reached the wire"
        );

        hew_connmgr_free(mgr);
        crate::cluster::hew_cluster_free(cluster);
        drop(Box::from_raw(transport_ptr));
        drop(Box::from_raw(state));
    }
    drop(ops);
}

/// Recorder for the real-order admission test: collects every registry
/// event the cluster applies.
extern "C" fn record_registry_apply(
    name: *const std::ffi::c_char,
    location: *const HewLocation,
    is_add: bool,
    user_data: *mut std::ffi::c_void,
) {
    // SAFETY: test installs a Mutex<Vec<_>> as the callback user data.
    let applied = unsafe { &*(user_data.cast::<Mutex<Vec<(String, Location, bool)>>>()) };
    // SAFETY: the cluster passes a valid NUL-terminated name.
    let name = unsafe { std::ffi::CStr::from_ptr(name) }
        .to_string_lossy()
        .into_owned();
    // SAFETY: cluster callback supplies a valid location pointer.
    let location = Location::try_from(unsafe { *location }).unwrap();
    applied
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .push((name, location, is_add));
}

/// Real admission ordering through the production pieces (the full
/// `hew_connmgr_add` path additionally requires a live Noise handshake to
/// reach `Strict` posture, which only the two-process e2e suite drives;
/// this test pins the same ordering seam with the real reserve / install /
/// publish / gate / decode functions):
/// `reserve_claim` → a registry-gossip control frame processed by
/// `handle_control_frame` (real decode + waiting gate) → REAL
/// `install_connection_actor` → REAL `publish_connection_established` →
/// the cluster registry callback applies the name. The frame processing
/// begins strictly BEFORE the install (channel-sequenced, plus a
/// scheduling grace biasing it into the pre-install window), modelling
/// the reader thread racing `hew_connmgr_add`; the gate must hold the
/// frame through install + publication and then apply it — an instant
/// deny loses the peer's one-shot flush and the name never resolves.
#[test]
fn control_frame_before_install_applies_registry_event_after_real_publish() {
    let applied = Box::into_raw(Box::new(Mutex::new(Vec::<(String, Location, bool)>::new())));
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
    // SAFETY: cluster is live; the recorder's user data outlives it.
    unsafe {
        crate::cluster::hew_cluster_set_registry_callback(
            cluster,
            record_registry_apply,
            applied.cast(),
        );
    }

    // SAFETY: test-owned pointers remain valid until the explicit cleanup.
    unsafe {
        let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
        assert!(!mgr.is_null());

        // Step 1 (real): reserve the claim, as hew_connmgr_add does before
        // spawning the reader.
        let token =
            next_publication_token(&*mgr).expect("the publication token space is not exhausted");
        let ClaimReservation::Reserved { superseded: None } =
            reserve_claim(&*mgr, 5, None, 30, token)
        else {
            panic!("fresh reservation must succeed without supersession");
        };

        // Step 2 (real decode + gate): a reader surrogate processes the
        // peer's one-shot registry-gossip frame. It signals right before
        // handing the frame to the production dispatch.
        let location = test_location(5, 0x99);
        let payload = RegistryGossipPayload {
            op: crate::cluster::GOSSIP_REGISTRY_ADD,
            name: "svc".to_owned(),
            location: test_location(5, 0x99),
        };
        let frame_bytes = encode_control_frame(&ControlFrame {
            version: WIRE_VERSION,
            ctrl_kind: CTRL_REGISTRY_GOSSIP,
            payload: encode_registry_gossip_payload(&payload).expect("gossip payload encodes"),
        })
        .expect("gossip frame encodes");
        let WireFrame::Control(control) = decode_wire_frame(&frame_bytes).expect("frame decodes")
        else {
            panic!("expected a control frame");
        };
        let (started_tx, started_rx) = std::sync::mpsc::channel();
        let reader = SendConnMgr(mgr);
        let reader_handle = std::thread::spawn(move || {
            let reader = reader;
            started_tx.send(()).expect("reader start signal");
            // SAFETY: manager outlives the join below.
            handle_control_frame(reader.0, HEW_FEATURE_SUPPORTS_GOSSIP, 30, token, &control);
            crate::stream_error::take_last_error()
        });
        started_rx.recv().expect("reader surrogate started");
        // Scheduling grace biasing the frame into the PRE-INSTALL window;
        // the assertion is interleaving-independent (the gate is correct
        // for frames arriving anywhere in the admission window).
        std::thread::sleep(Duration::from_millis(50));

        // Steps 3+4 (real): install, then publish, exactly as
        // hew_connmgr_add does after spawning the reader.
        let mut actor = ConnectionActor::new(30);
        actor.peer_node_id = 5;
        actor.publication_token = token;
        actor.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
        actor.posture = crate::peer_binding::Posture::Strict;
        actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        let Ok(publication) = install_connection_actor(&*mgr, actor) else {
            panic!("install must succeed");
        };
        publish_connection_established(
            &*mgr,
            5,
            30,
            HEW_FEATURE_SUPPORTS_GOSSIP,
            publication.token,
            &publication.sync,
            &publication.removed,
            None,
        );

        let reader_error = reader_handle.join().expect("reader surrogate");
        {
            let applied = (*applied)
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            assert_eq!(
                applied.as_slice(),
                &[("svc".to_owned(), location, true)],
                "the pre-install frame must apply exactly once after publication: {reader_error:?}",
            );
        }

        hew_connmgr_free(mgr);
        crate::cluster::hew_cluster_free(cluster);
        drop(Box::from_raw(transport_ptr));
        drop(Box::from_raw(applied));
    }
    drop(ops);
}
