//! A peer this node buried while it was still alive, reconnecting at the same
//! session: admitted over a refuting claim that carries SWIM alone, published
//! once its refutation readmits it, and closed when it breaks its bounds.
//!
//! Each test runs the real reservation, publication and reader against a
//! scripted transport whose peer end seals frames with the connection's
//! channel.

use super::*;
use crate::cluster::{MEMBER_ALIVE, MEMBER_DEAD, SWIM_MSG_ACK, SWIM_MSG_PING};
use crate::connection::control::encode_registry_gossip_control;
use crate::envelope::{
    encode_envelope_frame, EnvelopeFrame, EnvelopeMessage, NodeSessionIdentity, CTRL_SERVICE_OFFER,
};
use std::collections::VecDeque;

const LOCAL: u16 = 1;
const BURIED: u16 = 2;
const CONN: c_int = 20;
/// The buried peer's durable session, unchanged across its reconnect.
const SESSION: u32 = 1;
/// The incarnation this node buried the peer at.
const BURIED_AT: u64 = 5;

/// A frame the peer end delivers to the reader.
enum Inbound {
    Sealed(Vec<u8>),
    /// A frame announced at this length and never delivered.
    Oversized(usize),
}

#[derive(Default)]
struct Wire {
    inbound: VecDeque<Inbound>,
    closed: bool,
    sent: Vec<Vec<u8>>,
}

/// The transport under one connection: the test end queues inbound frames and
/// waits on what the node sends or on the close.
#[derive(Default)]
struct Script {
    wire: Mutex<Wire>,
    changed: Condvar,
}

impl Script {
    fn wait_until<T>(&self, mut ready: impl FnMut(&mut Wire) -> Option<T>) -> T {
        let mut wire = self
            .wire
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        loop {
            if let Some(value) = ready(&mut wire) {
                return value;
            }
            wire = self
                .changed
                .wait(wire)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
        }
    }

    fn push(&self, frame: Inbound) {
        self.wire
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .inbound
            .push_back(frame);
        self.changed.notify_all();
    }
}

unsafe extern "C" fn script_send(
    implementation: *mut std::ffi::c_void,
    _: c_int,
    data: *const std::ffi::c_void,
    len: usize,
) -> c_int {
    // SAFETY: the fixture installs its `Script` as the transport implementation.
    let script = unsafe { &*implementation.cast::<Script>() };
    // SAFETY: the manager passes an encoded frame valid for `len` bytes.
    let bytes = unsafe { std::slice::from_raw_parts(data.cast::<u8>(), len) }.to_vec();
    script
        .wire
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .sent
        .push(bytes);
    script.changed.notify_all();
    c_int::try_from(len).expect("test frames fit c_int")
}

unsafe extern "C" fn script_recv(
    implementation: *mut std::ffi::c_void,
    _: c_int,
    buffer: *mut std::ffi::c_void,
    capacity: usize,
) -> c_int {
    // SAFETY: the fixture installs its `Script` as the transport implementation.
    let script = unsafe { &*implementation.cast::<Script>() };
    script.wait_until(|wire| {
        if wire.closed {
            return Some(-1);
        }
        let length = match wire.inbound.front()? {
            Inbound::Oversized(length) => *length,
            Inbound::Sealed(frame) if frame.len() > capacity => frame.len(),
            Inbound::Sealed(frame) => {
                // SAFETY: the reader's buffer holds `capacity` writable bytes.
                unsafe {
                    std::ptr::copy_nonoverlapping(frame.as_ptr(), buffer.cast::<u8>(), frame.len());
                }
                let length = frame.len();
                wire.inbound.pop_front();
                length
            }
        };
        Some(c_int::try_from(length).expect("test frames fit c_int"))
    })
}

unsafe extern "C" fn script_close(implementation: *mut std::ffi::c_void, _: c_int) {
    // SAFETY: the fixture installs its `Script` as the transport implementation.
    let script = unsafe { &*implementation.cast::<Script>() };
    script
        .wire
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .closed = true;
    script.changed.notify_all();
}

/// Envelopes the node routed to a local actor.
static ROUTED: AtomicUsize = AtomicUsize::new(0);

unsafe extern "C" fn count_routed(
    _: crate::node_identity::HewLocation,
    _: *const std::ffi::c_char,
    _: *mut u8,
    _: usize,
    _: u64,
    _: u16,
    _: *mut HewConnMgr,
    _: crate::node_identity::HewNodeId,
    _: u64,
    _: u64,
    _: i64,
    _: u8,
) {
    ROUTED.fetch_add(1, Ordering::SeqCst);
}

/// This node with a peer it buried, reconnected at the buried session.
struct BuriedPeer {
    script: Box<Script>,
    ops: Box<crate::transport::HewTransportOps>,
    transport: *mut HewTransport,
    cluster: *mut crate::cluster::HewCluster,
    registry: Box<crate::hew_node::HewRegistry>,
    mgr: *mut HewConnMgr,
    /// The peer's half of the connection's channel.
    peer: snow::TransportState,
    /// How many of the node's sent frames the test has read.
    read: usize,
    token: u64,
}

impl BuriedPeer {
    fn admit() -> Self {
        let script = Box::<Script>::default();
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: Some(script_send),
            recv: Some(script_recv),
            close_conn: Some(script_close),
            destroy: None,
        });
        let transport = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: std::ptr::from_ref::<Script>(&script).cast_mut().cast(),
        }));
        let config = crate::cluster::ClusterConfig {
            local_node_id: LOCAL,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: the config is valid for the call; the fixture frees the cluster.
        let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const config) };
        let registry = Box::new(crate::hew_node::HewRegistry::default());
        // This node's own name, flushed to the peer once it is published.
        assert!(registry.apply_origin_name("local", test_location(LOCAL, 0x10), true));
        // SAFETY: the transport and cluster outlive the manager, freed in Drop.
        let mgr = unsafe {
            hew_connmgr_new(
                transport,
                Some(count_routed),
                std::ptr::null_mut(),
                cluster,
                LOCAL,
            )
        };
        // SAFETY: the manager was just created and the registry outlives it.
        unsafe { (*mgr).registry = &raw const *registry };

        // The peer was admitted, then buried while it was alive.
        // SAFETY: the manager owns the live cluster.
        let members = unsafe { &*cluster };
        assert!(members.admit_authenticated_session(BURIED, SESSION));
        members.apply_swim_gossip(&[(BURIED, SESSION, MEMBER_DEAD, BURIED_AT)]);
        assert_eq!(members.member_state(BURIED), MEMBER_DEAD);

        // SAFETY: the manager is live.
        let manager = unsafe { &*mgr };
        let token = next_publication_token(manager).expect("publication token");
        let identity = test_node_identity(BURIED);
        let ClaimReservation::Reserved {
            superseded: None,
            buried: true,
        } = reserve_identity_claim(
            manager,
            identity,
            BURIED,
            SESSION,
            None,
            CONN,
            token,
            None,
            TEST_ADMISSION,
            Commit::Unordered,
        )
        else {
            panic!("a buried peer's same-session reconnect reserves to refute");
        };

        let (channel, peer) = ChannelProtection::test_noise_pair();
        let actor = reading_actor(mgr, transport, token, channel);
        let sync = Arc::clone(&actor.publication_sync);
        let removed = Arc::clone(&actor.publication_removed);
        manager
            .connections
            .access(|connections| connections.push(actor));
        assert!(publish_identity_connection_established(
            manager,
            identity,
            BURIED,
            SESSION,
            CONN,
            HEW_FEATURE_SUPPORTS_GOSSIP,
            token,
            &sync,
            &removed,
            None,
            None,
            true,
        ));
        Self {
            script,
            ops,
            transport,
            cluster,
            registry,
            mgr,
            peer,
            read: 0,
            token,
        }
    }

    fn manager(&self) -> &HewConnMgr {
        // SAFETY: the fixture owns the live manager.
        unsafe { &*self.mgr }
    }

    fn member_state(&self) -> i32 {
        // SAFETY: the fixture owns the live cluster.
        unsafe { &*self.cluster }.member_state(BURIED)
    }

    fn claim_state(&self) -> Option<ClaimState> {
        self.manager()
            .claims
            .0
            .lock_or_recover()
            .get(&test_node_identity(BURIED))
            .filter(|claim| claim.publication_token == self.token)
            .map(|claim| claim.state)
    }

    fn installed(&self) -> bool {
        self.manager().connections.access(|connections| {
            connections
                .iter()
                .any(|connection| connection.conn_id == CONN)
        })
    }

    /// Seal `frame` with the peer's half of the channel and deliver it.
    fn deliver(&mut self, frame: &[u8]) {
        let mut sealed = vec![0_u8; frame.len() + 64];
        let length = self
            .peer
            .write_message(frame, &mut sealed)
            .expect("the peer seals its frame");
        sealed.truncate(length);
        self.script.push(Inbound::Sealed(sealed));
    }

    fn deliver_swim(&mut self, msg_type: i32, incarnation: u64) {
        let frame = swim_control_frame_bytes(&SwimControlPayload {
            msg_type,
            from: peer_session(),
            incarnation,
            target: None,
            gossip: Vec::new(),
        });
        self.deliver(&frame);
    }

    /// The peer's ordinary traffic: a name, an actor request and a service
    /// offer, none of which a buried peer may publish.
    fn deliver_ordinary_traffic(&mut self) {
        let name = encode_registry_gossip_control("peer", peer_name(), true).expect("name");
        self.deliver(&name);
        let request = encode_envelope_frame(&EnvelopeFrame {
            version: WIRE_VERSION,
            target: Some(test_location(LOCAL, 0x10)),
            source: Some(peer_name()),
            message: EnvelopeMessage::Service {
                handler: "tests.Peer.handle".to_owned(),
                grant_id: 1,
                kind: crate::request_state::RequestKind::CallWait,
            },
            payload: vec![1],
            request_id: 9,
            budget_ns: None,
        })
        .expect("request");
        self.deliver(&request);
        let offer = encode_control_frame(&ControlFrame {
            version: WIRE_VERSION,
            ctrl_kind: CTRL_SERVICE_OFFER,
            payload: vec![0xa0],
        })
        .expect("offer");
        self.deliver(&offer);
    }

    /// The next frame this node sent the peer, opened and decoded.
    fn next_frame(&mut self) -> WireFrame {
        let index = self.read;
        let sealed = self.script.wait_until(|wire| wire.sent.get(index).cloned());
        self.read += 1;
        decode_wire_frame(&open_sent_frame(&mut self.peer, &sealed)).expect("frame")
    }

    /// The next control frame this node sent the peer.
    fn next_sent(&mut self) -> ControlFrame {
        let WireFrame::Control(control) = self.next_frame() else {
            panic!("the node answers the buried peer's requests only with refusals");
        };
        control
    }

    /// The node's refusal of the peer's call `request_id`, sent unread.
    fn expect_refused_call(&mut self, request_id: u64) {
        let WireFrame::Envelope(reply) = self.next_frame() else {
            panic!("the call is answered with an envelope");
        };
        assert_eq!(reply.message, EnvelopeMessage::NotAdmitted);
        assert_eq!(reply.request_id, request_id);
        assert_eq!(reply.payload, [crate::hew_node::refusal_reason_code()]);
        assert!(reply.target.is_none() && reply.source.is_none());
    }

    fn next_swim(&mut self) -> SwimControlPayload {
        let control = self.next_sent();
        assert_eq!(
            control.ctrl_kind, CTRL_SWIM,
            "only SWIM reaches a buried peer"
        );
        decode_swim_payload(&control.payload).expect("SWIM payload")
    }

    /// Wait until the connection is removed: its claim retires last.
    fn wait_removed(&self) {
        let (lock, retired) = &self.manager().claims;
        let mut claims = lock.lock_or_recover();
        while claims
            .get(&test_node_identity(BURIED))
            .is_none_or(|claim| claim.state != ClaimState::Retired)
        {
            claims = retired
                .wait(claims)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
        }
    }
}

impl Drop for BuriedPeer {
    fn drop(&mut self) {
        // SAFETY: the fixture owns every pointer and frees each exactly once,
        // the manager first since it borrows the transport and cluster.
        unsafe {
            hew_connmgr_free(self.mgr);
            crate::cluster::hew_cluster_free(self.cluster);
            drop(Box::from_raw(self.transport));
        }
        let _ = (&self.ops, &self.registry);
    }
}

/// The buried peer's connection, with its reader running as admission runs
/// it for a buried peer.
fn reading_actor(
    mgr: *mut HewConnMgr,
    transport: *mut HewTransport,
    token: u64,
    channel: ChannelProtection,
) -> ConnectionActor {
    let mut actor = ConnectionActor::new(CONN, Arc::new(channel));
    actor.transport = ConnectionTransport::borrowed(transport);
    actor.publication_token = token;
    actor.peer_node_id = BURIED;
    actor.peer_identity = Some(test_node_identity(BURIED));
    actor.peer_session_incarnation = SESSION;
    actor.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
    actor.posture = crate::peer_binding::Posture::Strict;
    actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
    let mgr = SendConnMgr(mgr);
    let transport = ConnectionTransport::borrowed(transport);
    let stop = Arc::clone(&actor.reader_stop);
    let activity = Arc::clone(&actor.last_activity_ms);
    let channel = Arc::clone(&actor.channel);
    actor.reader_handle = Some(std::thread::spawn(move || {
        super::super::reader::reader_loop(
            mgr,
            transport,
            CONN,
            token,
            stop,
            activity,
            Some(count_routed),
            HEW_FEATURE_SUPPORTS_GOSSIP,
            channel,
            None,
            true,
        );
    }));
    actor
}

fn peer_session() -> NodeSessionIdentity {
    NodeSessionIdentity {
        node_id: test_node_identity(BURIED),
        session_incarnation: SESSION,
    }
}

fn peer_name() -> Location {
    Location::direct(test_node_identity(BURIED), 0x20, SESSION).expect("peer location")
}

fn obituary(payload: &SwimControlPayload) -> bool {
    payload.gossip.first().is_some_and(|entry| {
        entry.member == peer_session()
            && entry.state == MEMBER_DEAD
            && entry.incarnation == BURIED_AT
    })
}

/// The buried peer learns of its burial over the refuting connection, which
/// carries nothing else, and its refutation readmits and publishes it: names,
/// routes and actor requests then flow both ways. Counterfactual: the
/// same-session reconnect is refused at reservation and the peer stays DEAD.
#[test]
fn buried_peer_refutes_over_swim_and_is_published() {
    let mut node = BuriedPeer::admit();
    assert!(matches!(
        node.claim_state(),
        Some(ClaimState::Refuting { .. })
    ));

    // The node opens with a PING that leads with the peer's obituary, so a
    // peer that buried this node as well still hears of its own burial.
    let opening = node.next_swim();
    assert_eq!(opening.msg_type, SWIM_MSG_PING);
    assert!(
        obituary(&opening),
        "the opening PING leads with the obituary"
    );

    // Ordinary traffic is dropped, its call refused, and a PING at the
    // buried incarnation is answered without readmitting the peer. The ACK
    // fences what came before.
    node.deliver_ordinary_traffic();
    node.deliver_swim(SWIM_MSG_PING, BURIED_AT);
    node.expect_refused_call(9);
    let ack = node.next_swim();
    assert_eq!(ack.msg_type, SWIM_MSG_ACK);
    assert!(
        obituary(&ack),
        "every frame to the buried peer leads with it"
    );
    assert_eq!(
        ROUTED.load(Ordering::SeqCst),
        0,
        "no actor request is routed"
    );
    assert_eq!(
        node.registry.current_local_name(peer_session(), "peer"),
        None
    );
    assert!(!routable(node.manager()));
    assert!(matches!(
        node.claim_state(),
        Some(ClaimState::Refuting { .. })
    ));
    assert_eq!(node.member_state(), MEMBER_DEAD);

    // The refutation readmits the peer and publishes its connection.
    node.deliver_swim(SWIM_MSG_PING, BURIED_AT + 1);
    let ack = node.next_swim();
    assert_eq!(ack.msg_type, SWIM_MSG_ACK);
    let flush = node.next_sent();
    assert_eq!(
        flush.ctrl_kind, CTRL_REGISTRY_GOSSIP,
        "this node's names follow"
    );
    assert_eq!(
        decode_registry_gossip_payload(&flush.payload)
            .expect("name")
            .name,
        "local"
    );
    assert_eq!(node.claim_state(), Some(ClaimState::Published));
    assert_eq!(node.member_state(), MEMBER_ALIVE);
    assert!(routable(node.manager()));

    // The published connection now carries the peer's ordinary traffic.
    node.deliver_ordinary_traffic();
    node.deliver_swim(SWIM_MSG_PING, BURIED_AT + 1);
    assert_eq!(node.next_swim().msg_type, SWIM_MSG_ACK);
    assert_eq!(
        node.registry.current_local_name(peer_session(), "peer"),
        Some(peer_name())
    );
    assert_eq!(
        ROUTED.load(Ordering::SeqCst),
        1,
        "the actor request is routed"
    );
}

/// Each request a buried peer sends before refuting is refused unread: its
/// call with a `NotAdmitted` reply and its monitor setup with a refused
/// result, so neither waits on a connection that stays open once the peer
/// refutes. Counterfactual: both are dropped silently and a call without a
/// deadline never resolves.
#[test]
fn buried_peer_requests_are_refused_before_it_refutes() {
    let mut node = BuriedPeer::admit();
    assert!(obituary(&node.next_swim()), "the opening PING");

    let call = encode_envelope_frame(&EnvelopeFrame {
        version: WIRE_VERSION,
        target: Some(test_location(LOCAL, 0x10)),
        source: Some(peer_name()),
        message: EnvelopeMessage::Service {
            handler: "tests.Peer.handle".to_owned(),
            grant_id: 1,
            kind: crate::request_state::RequestKind::CallWait,
        },
        payload: vec![1],
        request_id: 9,
        budget_ns: None,
    })
    .expect("call");
    node.deliver(&call);
    let monitor = crate::envelope::MonitorReqPayload {
        grant_id: 1,
        watcher: peer_name(),
        ref_id: 4,
        target: test_location(LOCAL, 0x10),
        setup_id: 11,
    };
    let monitor = encode_control_frame(&ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: crate::envelope::CTRL_MONITOR_REQ,
        payload: crate::envelope::encode_monitor_req_payload(&monitor).expect("monitor"),
    })
    .expect("monitor frame");
    node.deliver(&monitor);

    node.expect_refused_call(9);
    let result = node.next_sent();
    assert_eq!(result.ctrl_kind, crate::envelope::CTRL_MONITOR_SETUP_RESULT);
    let result =
        crate::envelope::decode_setup_result_payload(&result.payload).expect("setup result");
    assert_eq!(
        (result.setup_id, result.ref_id, result.status),
        (11, 4, crate::envelope::SETUP_STATUS_REFUSED)
    );
    assert_eq!(ROUTED.load(Ordering::SeqCst), 0, "nothing is routed");
    assert!(matches!(
        node.claim_state(),
        Some(ClaimState::Refuting { .. })
    ));
}

/// Whether user traffic to the peer has a published connection to take.
fn routable(manager: &HewConnMgr) -> bool {
    super::super::send::claim_send_to_route(manager, BURIED, None).is_some()
}

/// A buried peer that keeps sending without refuting is closed once it
/// passes the frame cap, and nothing it sent is applied.
#[test]
fn buried_peer_past_the_frame_cap_is_closed() {
    let mut node = BuriedPeer::admit();
    let name = encode_registry_gossip_control("peer", peer_name(), true).expect("name");
    for _ in 0..=1024 {
        node.deliver(&name);
    }
    node.wait_removed();
    assert_eq!(node.claim_state(), Some(ClaimState::Retired));
    assert!(!node.installed());
    assert_eq!(
        node.registry.current_local_name(peer_session(), "peer"),
        None
    );
    assert_eq!(node.member_state(), MEMBER_DEAD);
}

/// A frame larger than the read buffer closes a buried peer's connection
/// before any of it is read.
#[test]
fn buried_peer_oversized_frame_is_closed() {
    let node = BuriedPeer::admit();
    node.script.push(Inbound::Oversized(64 * 1024 + 1));
    node.wait_removed();
    assert_eq!(node.claim_state(), Some(ClaimState::Retired));
    assert!(!node.installed());
    assert_eq!(node.member_state(), MEMBER_DEAD);
}

/// A buried peer that never refutes is closed at its deadline, one
/// suspicion window after admission, and not before.
#[test]
fn buried_peer_is_closed_at_its_refutation_deadline() {
    let node = BuriedPeer::admit();
    let Some(ClaimState::Refuting { expires_ms }) = node.claim_state() else {
        panic!("the buried peer refutes");
    };
    expire_refuting_claims(node.manager(), expires_ms - 1);
    assert!(node.installed());
    assert!(matches!(
        node.claim_state(),
        Some(ClaimState::Refuting { .. })
    ));

    expire_refuting_claims(node.manager(), expires_ms);
    node.wait_removed();
    assert_eq!(node.claim_state(), Some(ClaimState::Retired));
    assert!(!node.installed());
    assert_eq!(node.member_state(), MEMBER_DEAD);
}
