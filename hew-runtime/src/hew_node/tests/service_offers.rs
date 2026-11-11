//! Session-scoped service offers, their acknowledgements and retractions.

use super::*;
use crate::envelope::{NodeSessionIdentity, ServiceOfferPayload};
use std::time::Instant;

const HANDLER: &str = "tests.Counter.handle";

struct ReleaseGate(Arc<super::super::inbound::TestGate>);

impl Drop for ReleaseGate {
    fn drop(&mut self) {
        self.0.release();
    }
}

/// Wait for a cross-node effect; `false` when it did not happen in time.
fn eventually(mut ready: impl FnMut() -> bool) -> bool {
    let deadline = Instant::now() + Duration::from_secs(10);
    while Instant::now() < deadline {
        if ready() {
            return true;
        }
        thread::sleep(Duration::from_millis(5));
    }
    ready()
}

struct Pair {
    issuer: TestNode,
    recipient: TestNode,
    address: CString,
    actor: *mut crate::actor::HewActor,
}

impl Pair {
    fn start(issuer_slot: u16, recipient_slot: u16) -> Self {
        let (issuer, port, recipient, _) = start_authorized_tcp_pair(issuer_slot, recipient_slot);
        crate::pid::hew_pid_set_local_node(issuer_slot);
        let actor = spawn_remote_test_actor(noop_dispatch);
        inbound_test_codec(noop_dispatch, 0);
        let address = CString::new(format!("127.0.0.1:{port}")).unwrap();
        // SAFETY: both nodes stay live for the pair.
        unsafe {
            connect_with_retry(recipient.as_ptr(), &address);
            wait_for_handshake(recipient.as_ptr(), issuer.as_ptr());
        }
        Self {
            issuer,
            recipient,
            address,
            actor,
        }
    }

    fn issuer_manager(&self) -> &HewConnMgr {
        // SAFETY: the running issuer owns its manager for the pair's lifetime.
        unsafe { &*(*self.issuer.as_ptr()).conn_mgr }
    }

    fn recipient_manager(&self) -> &HewConnMgr {
        // SAFETY: the running recipient owns its manager for the pair's lifetime.
        unsafe { &*(*self.recipient.as_ptr()).conn_mgr }
    }

    fn issuer_id(&self) -> crate::node_identity::NodeId {
        // SAFETY: the issuer node is live.
        unsafe { (*self.issuer.as_ptr()).auth.node_identity().unwrap() }
    }

    fn recipient_id(&self) -> crate::node_identity::NodeId {
        // SAFETY: the recipient node is live.
        unsafe { (*self.recipient.as_ptr()).auth.node_identity().unwrap() }
    }

    /// The issuer's current connection to the recipient.
    fn issuer_connection(&self) -> ConnectionKey {
        let manager = self.issuer_manager();
        let claim = connection::claim_send_by_identity(manager, self.recipient_id(), None).unwrap();
        ConnectionKey::new(manager, claim.connection_id())
    }

    fn recipient_session(&self) -> NodeSessionIdentity {
        // SAFETY: the recipient node is live.
        let recipient = unsafe { (*self.recipient.as_ptr()).auth.node_identity().unwrap() };
        connection::claim_send_by_identity(self.issuer_manager(), recipient, None)
            .unwrap()
            .peer_session()
    }

    fn issuer_session(&self) -> NodeSessionIdentity {
        // SAFETY: the issuer node is live.
        let issuer = unsafe { (*self.issuer.as_ptr()).auth.node_identity().unwrap() };
        connection::claim_send_by_identity(self.recipient_manager(), issuer, None)
            .unwrap()
            .peer_session()
    }

    fn export(&self, request_id: u64) -> (super::super::service::ExportLease, ServiceOfferPayload) {
        let manager = self.issuer_manager();
        // SAFETY: the actor is live for the pair.
        let location =
            connection::local_location_for_actor(manager, unsafe { (*self.actor).id }).unwrap();
        manager
            .service_exports
            .export(
                manager.local_node_id,
                self.recipient_session(),
                request_id,
                location,
                vec![HANDLER.into()],
            )
            .unwrap()
    }

    fn register_receive(&self) -> Arc<PendingReply> {
        let manager = self.recipient_manager();
        let claim = connection::claim_send_by_identity(manager, self.issuer_id(), None).unwrap();
        let (_, pending) = reply_table().register_with_waker(
            ConnectionKey::new(manager, claim.connection_id()),
            None,
            PendingReplyKind::Import,
        );
        pending
    }

    /// Register a recipient-side wait for this issuer's offer.
    fn receive(&self) -> Arc<PendingReply> {
        let pending = self.register_receive();
        self.recipient_manager()
            .service_exports
            .receive(self.issuer_session(), vec![HANDLER.into()], &pending)
            .unwrap();
        pending
    }

    fn admits(&self, grant: u64) -> bool {
        let manager = self.issuer_manager();
        let recipient = self.recipient_session();
        // SAFETY: the actor is live for the pair.
        let location =
            connection::local_location_for_actor(manager, unsafe { (*self.actor).id }).unwrap();
        manager
            .service_exports
            .claim_observation(location, recipient, grant, &AtomicBool::new(false), || ())
            .is_ok()
    }

    fn stop(self) {
        // SAFETY: the test released every grant and wait it registered.
        unsafe {
            crate::actor::hew_actor_free(self.actor);
            assert_eq!(hew_node_stop(self.recipient.as_ptr()), 0);
            assert_eq!(hew_node_stop(self.issuer.as_ptr()), 0);
        }
    }
}

fn imported(pending: &PendingReply) -> Option<u64> {
    let outcome = pending.outcome.lock().unwrap();
    let outcome = outcome.as_ref()?;
    assert_eq!(outcome.status, ReplyStatus::Success);
    Some(
        crate::envelope::decode_service_offer_payload(&outcome.data)
            .unwrap()
            .grant_id,
    )
}

/// The recipient imported a grant but its acknowledgement was lost with the
/// connection. The successor publication re-sends the same grant, the
/// recipient acknowledges it again without a second import, and the grant
/// stays live at the issuer.
#[test]
fn lost_offer_acknowledgement_survives_a_connection_replacement() {
    let _guard = crate::runtime_test_guard();
    let pair = Pair::start(375, 376);
    let issuer_manager = pair.issuer_manager();
    // Both waits are registered on the connection the replacement drops.
    let (request_id, offer_pending) = reply_table().register_with_waker(
        pair.issuer_connection(),
        None,
        PendingReplyKind::SessionOffer,
    );
    let (export, offer) = pair.export(request_id);
    issuer_manager
        .service_exports
        .mark_delivered_for_test(offer.grant_id);
    // The offer arrived and was imported; its acknowledgement never did.
    let first = pair.receive();
    pair.recipient_manager()
        .service_exports
        .offer(offer.clone(), true)
        .unwrap();
    assert_eq!(imported(&first), Some(offer.grant_id));
    let waiting = pair.receive();

    // SAFETY: both nodes stay live; this publishes a successor connection.
    unsafe { connect_with_retry(pair.recipient.as_ptr(), &pair.address) };
    assert!(
        eventually(|| offer_pending.outcome.lock().unwrap().is_some()),
        "the successor publication never acknowledged the re-sent offer"
    );
    let outcome = offer_pending.outcome.lock().unwrap().take().unwrap();
    assert_eq!(
        outcome.status,
        ReplyStatus::Success,
        "{:?}",
        outcome.ask_error
    );
    assert!(pair.admits(offer.grant_id), "the acknowledged grant closed");

    // The re-sent grant is not importable a second time: the receive that
    // waited across the replacement takes the next grant instead.
    assert!(!pair
        .recipient_manager()
        .service_exports
        .holds_offer(pair.issuer_session(), offer.grant_id));
    assert_eq!(imported(&waiting), None);
    let (next_export, next) = pair.export(request_id + 1_000_000);
    issuer_manager.service_exports.deliver(next.grant_id, None);
    assert!(eventually(|| imported(&waiting).is_some()));
    assert_eq!(imported(&waiting), Some(next.grant_id));
    drop((export, next_export));
    pair.stop();
}

/// A still-live imported grant must not become receivable again when newer
/// imports fill the bounded history, even if their retractions were lost.
#[test]
fn bounded_import_history_preserves_live_grant_deduplication() {
    let _guard = crate::runtime_test_guard();
    let pair = Pair::start(410, 411);
    let (original_export, original) = pair.export(90_000);
    let exports = &pair.recipient_manager().service_exports;
    let first = pair.receive();
    exports.offer(original.clone(), true).unwrap();
    assert_eq!(imported(&first), Some(original.grant_id));

    let mut capacity_refused = false;
    for index in 0..256 {
        let (temporary, offer) = pair.export(91_000 + index);
        let pending = pair.receive();
        match exports.offer(offer.clone(), true) {
            Ok(()) => assert_eq!(imported(&pending), Some(offer.grant_id)),
            Err(AskError::WorkerAtCapacity) => {
                capacity_refused = true;
                reply_table().fail(pending.request_id, AskError::LocalShutdown);
            }
            Err(error) => panic!("unexpected offer refusal: {error:?}"),
        }
        // Deliver directly and omit retraction, modelling a lost control frame.
        // The issuer keeps at most two live exports throughout the history.
        drop(temporary);
    }
    assert!(pair.admits(original.grant_id));
    let waiting = pair.receive();
    exports.offer(original.clone(), true).unwrap();
    let repeated = imported(&waiting);
    reply_table().fail(waiting.request_id, AskError::LocalShutdown);
    drop(original_export);
    pair.stop();
    assert_eq!(repeated, None, "an old live grant was imported twice");
    assert!(
        capacity_refused,
        "history capacity must use explicit backpressure"
    );
}

/// Capacity refusal preserves a queued offer and its receive registration;
/// retractions free space for both queued and subsequently arriving offers.
#[test]
fn import_history_capacity_preserves_queued_and_waiting_receives() {
    let _guard = crate::runtime_test_guard();
    let pair = Pair::start(412, 413);
    let exports = &pair.recipient_manager().service_exports;
    let issuer = pair.issuer_session();
    let mut remembered = Vec::new();
    for index in 0..256 {
        let (temporary, offer) = pair.export(100_000 + index);
        let pending = pair.receive();
        exports.offer(offer.clone(), true).unwrap();
        assert_eq!(imported(&pending), Some(offer.grant_id));
        remembered.push(offer.grant_id);
        // The issuer closes each grant, but the recipient misses retraction.
        drop(temporary);
    }

    let (queued_export, queued) = pair.export(110_000);
    exports.offer(queued.clone(), true).unwrap();
    exports.offer(queued.clone(), true).unwrap();
    let pending = pair.register_receive();
    assert_eq!(
        exports.receive(issuer, vec![HANDLER.into()], &pending),
        Err(AskError::WorkerAtCapacity)
    );
    assert_eq!(imported(&pending), None);
    assert!(exports.holds_offer(issuer, queued.grant_id));
    exports.retract(issuer, remembered[0]);
    exports
        .receive(issuer, vec![HANDLER.into()], &pending)
        .unwrap();
    assert_eq!(imported(&pending), Some(queued.grant_id));
    assert!(!exports.holds_offer(issuer, queued.grant_id));
    // Even at capacity, an imported duplicate is accepted without redelivery.
    exports.offer(queued.clone(), true).unwrap();

    let waiting = pair.receive();
    let (request_id, refused) = reply_table().register_with_waker(
        pair.issuer_connection(),
        None,
        PendingReplyKind::SessionOffer,
    );
    let (refused_export, refused_offer) = pair.export(request_id);
    pair.issuer_manager()
        .service_exports
        .deliver(refused_offer.grant_id, None);
    assert!(eventually(|| refused.outcome.lock().unwrap().is_some()));
    {
        let outcome = refused.outcome.lock().unwrap();
        let outcome = outcome.as_ref().unwrap();
        assert_eq!(outcome.status, ReplyStatus::Failed);
        assert_eq!(outcome.ask_error, AskError::WorkerAtCapacity);
    }
    assert_eq!(imported(&waiting), None);
    drop(refused_export);

    exports.retract(issuer, remembered[1]);
    let (request_id, accepted) = reply_table().register_with_waker(
        pair.issuer_connection(),
        None,
        PendingReplyKind::SessionOffer,
    );
    let (next_export, next) = pair.export(request_id);
    pair.issuer_manager()
        .service_exports
        .deliver(next.grant_id, None);
    assert!(eventually(|| imported(&waiting).is_some()));
    assert_eq!(imported(&waiting), Some(next.grant_id));
    assert!(eventually(|| accepted.outcome.lock().unwrap().is_some()));
    assert_eq!(
        accepted.outcome.lock().unwrap().as_ref().unwrap().status,
        ReplyStatus::Success
    );
    drop((queued_export, next_export));
    pair.stop();
}

/// A failed receive can retain its Arc until the caller releases the operation.
/// Its stale waiter must not strand another receive that is already waiting.
#[test]
fn stale_first_receive_does_not_strand_later_waiters() {
    let _guard = crate::runtime_test_guard();
    let pair = Pair::start(414, 415);
    let stale = pair.receive();
    let live = pair.receive();
    reply_table().fail(stale.request_id, AskError::LocalShutdown);
    let (export, offer) = pair.export(120_000);
    pair.recipient_manager()
        .service_exports
        .offer(offer.clone(), true)
        .unwrap();
    let received = imported(&live);
    reply_table().fail(live.request_id, AskError::LocalShutdown);
    drop(export);
    pair.stop();
    assert_eq!(
        received,
        Some(offer.grant_id),
        "a stale first wait must not strand a live receive"
    );
}

/// Closing a non-current node settles its own session offer without failing
/// an unrelated node's pending export in the same runtime.
#[test]
fn non_current_node_stop_settles_only_its_own_session_offer() {
    let _guard = crate::runtime_test_guard();
    for sent in [false, true] {
        let owner = Pair::start(416, 417);
        let (request_id, pending) = reply_table().register_with_waker(
            owner.issuer_connection(),
            None,
            PendingReplyKind::SessionOffer,
        );
        let (export, offer) = owner.export(request_id);
        let owner_gate = ReleaseGate(
            owner
                .issuer_manager()
                .service_exports
                .offer_send_gate_for_test(offer.grant_id),
        );
        owner_gate.0.arm(true);
        if sent {
            owner
                .issuer_manager()
                .service_exports
                .mark_delivered_for_test(offer.grant_id);
            owner
                .issuer_manager()
                .service_exports
                .observe_session(owner.recipient_session());
            assert!(owner_gate.0.wait_for_enter(Duration::from_secs(10)));
        }
        let other = Pair::start(418, 419);
        let (other_id, unrelated) = reply_table().register_with_waker(
            other.issuer_connection(),
            None,
            PendingReplyKind::SessionOffer,
        );
        let (other_export, other_offer) = other.export(other_id);
        let other_gate = ReleaseGate(
            other
                .issuer_manager()
                .service_exports
                .offer_send_gate_for_test(other_offer.grant_id),
        );
        other_gate.0.arm(true);
        other
            .issuer_manager()
            .service_exports
            .mark_delivered_for_test(other_offer.grant_id);
        // Active connections can precede the publication tail that replays
        // outstanding offers. Force that replay, holding sends so a real ACK
        // cannot complete either wait before its shutdown assertion.
        other
            .issuer_manager()
            .service_exports
            .observe_session(other.recipient_session());
        assert!(other_gate.0.wait_for_enter(Duration::from_secs(10)));
        with_current_node(|current| *current = other.issuer.as_ptr() as usize);
        let owner_addr = owner.issuer.as_ptr() as usize;
        let stopper = thread::spawn(move || {
            // SAFETY: the owner remains allocated until this thread is joined.
            unsafe { hew_node_stop(owner_addr as *mut HewNode) }
        });
        let settled = eventually(|| pending.outcome.lock().unwrap().is_some());
        let outcome = pending
            .outcome
            .lock()
            .unwrap()
            .as_ref()
            .map(|reply| (reply.status, reply.ask_error));
        // Closing the owner's export revokes its held send before settling
        // this wait. Release the gate so teardown can join all local work.
        owner_gate.0.release();
        let stopped = stopper.join().expect("stop the non-current owner");
        let other_settled = unrelated.outcome.lock().unwrap().is_some();
        reply_table().fail(request_id, AskError::LocalShutdown);
        reply_table().fail(other_id, AskError::LocalShutdown);
        drop((export, other_export));
        other_gate.0.release();
        owner.stop();
        other.stop();
        assert_eq!(stopped, 0);
        assert!(settled, "the owner's stop must settle its pending offer");
        assert_eq!(
            outcome,
            Some((ReplyStatus::Failed, AskError::LocalShutdown))
        );
        assert!(
            !other_settled,
            "an unrelated node's offer must stay pending"
        );
    }
}

/// A repeated offer is acknowledged without queueing or importing it twice,
/// and a different interface under the same grant is refused.
#[test]
fn duplicate_session_offer_is_deduplicated() {
    let _guard = crate::runtime_test_guard();
    let pair = Pair::start(377, 378);
    let (export, offer) = pair.export(5);
    let exports = &pair.recipient_manager().service_exports;
    exports.offer(offer.clone(), true).unwrap();
    exports.offer(offer.clone(), true).unwrap();
    let mut altered = offer.clone();
    altered.handlers = vec!["tests.Counter.other".into()];
    assert!(exports.offer(altered.clone(), true).is_err());

    let first = pair.receive();
    assert_eq!(imported(&first), Some(offer.grant_id));
    // Once imported, a re-sent offer is still acknowledged but not queued.
    exports.offer(offer.clone(), true).unwrap();
    assert!(exports.offer(altered, true).is_err());
    let second = pair.receive();
    assert_eq!(imported(&second), None, "the grant was imported twice");
    reply_table().remove(second.request_id);
    drop(export);
    pair.stop();
}

/// An issuer that closes an export before the recipient receives it must not
/// leave the recipient a grant the issuer no longer admits.
#[test]
fn closed_export_queued_at_the_recipient_is_not_receivable() {
    let _guard = crate::runtime_test_guard();
    let pair = Pair::start(373, 374);
    let (export, offer) = pair.export(7);
    pair.issuer_manager()
        .service_exports
        .deliver(offer.grant_id, None);
    let issuer = pair.issuer_session();
    let recipient = &pair.recipient_manager().service_exports;
    assert!(eventually(|| recipient.holds_offer(issuer, offer.grant_id)));
    // The offer is queued; its issuer then closes the export.
    drop(export);
    assert!(
        eventually(|| !recipient.holds_offer(issuer, offer.grant_id)),
        "the retraction never reached the recipient"
    );
    let pending = pair.receive();
    assert_eq!(
        imported(&pending),
        None,
        "the recipient imported a grant its issuer already closed"
    );
    reply_table().remove(pending.request_id);
    pair.stop();
}

/// An issuer that sees its recipient without the service-session capability
/// sends neither a session-scoped offer nor a retraction. The recipient's
/// queued offer stays receivable after the close; the issuer refuses the grant.
#[test]
fn closed_grant_is_not_retracted_to_a_peer_without_service_sessions() {
    let _guard = crate::runtime_test_guard();
    let pair = Pair::start(379, 380);
    let _legacy =
        connection::clear_service_sessions_for_test(pair.issuer_manager(), pair.recipient_id());
    let issuer = pair.issuer_session();
    let exports = &pair.recipient_manager().service_exports;

    let (export, offer) = pair.export(9);
    pair.issuer_manager()
        .service_exports
        .deliver(offer.grant_id, None);
    // The peer received it as a publication-scoped offer.
    exports.offer(offer.clone(), false).unwrap();
    drop(export);

    // A publication-scoped frame sent after the close fences any retraction.
    let (fence_export, fence) = pair.export(10);
    let claim =
        connection::claim_send_by_identity(pair.issuer_manager(), pair.recipient_id(), None)
            .unwrap();
    assert!(!claim.service_sessions());
    let bytes = crate::envelope::encode_control_frame(&crate::envelope::ControlFrame {
        version: crate::envelope::WIRE_VERSION,
        ctrl_kind: crate::envelope::CTRL_SERVICE_OFFER,
        payload: crate::envelope::encode_service_offer_payload(&fence).unwrap(),
    })
    .unwrap();
    assert!(claim.submit(move || Some(bytes), |_| {}));
    assert!(eventually(|| exports.holds_offer(issuer, fence.grant_id)));

    assert!(exports.holds_offer(issuer, offer.grant_id));
    assert!(!pair.admits(offer.grant_id));
    let pending = pair.receive();
    assert_eq!(imported(&pending), Some(offer.grant_id));
    drop(fence_export);
    pair.stop();
}

/// A recipient ignores a retraction from an issuer whose handshake did not set
/// the service-session capability; that issuer's closed grant stays queued.
#[test]
fn retraction_from_a_peer_without_service_sessions_is_ignored() {
    let _guard = crate::runtime_test_guard();
    let pair = Pair::start(385, 386);
    let _legacy =
        connection::clear_service_sessions_for_test(pair.recipient_manager(), pair.issuer_id());
    let issuer = pair.issuer_session();
    let exports = &pair.recipient_manager().service_exports;

    let (export, offer) = pair.export(13);
    pair.issuer_manager()
        .service_exports
        .deliver(offer.grant_id, None);
    assert!(eventually(|| exports.holds_offer(issuer, offer.grant_id)));
    drop(export);
    // An offer sent after the retraction on the same connection fences it.
    let (fence_export, fence) = pair.export(14);
    pair.issuer_manager()
        .service_exports
        .deliver(fence.grant_id, None);
    assert!(eventually(|| exports.holds_offer(issuer, fence.grant_id)));
    assert!(exports.holds_offer(issuer, offer.grant_id));
    drop(fence_export);
    pair.stop();
}

/// Membership burying a peer session fails the offers awaiting its
/// acknowledgement and the receives waiting on it with `Partition`, instead
/// of leaving them for a publication of that session that cannot come.
#[test]
fn buried_session_fails_its_pending_offers_and_receives() {
    let _guard = crate::runtime_test_guard();
    let pair = Pair::start(383, 384);
    let issuer_manager = pair.issuer_manager();
    // The issuer node is current; the burial is observed by its membership.
    let (request_id, offer_pending) = reply_table().register_with_waker(
        pair.issuer_connection(),
        None,
        PendingReplyKind::SessionOffer,
    );
    let (export, offer) = pair.export(request_id);
    issuer_manager
        .service_exports
        .mark_delivered_for_test(offer.grant_id);
    let recipient = pair.recipient_session();
    let (_, receive_pending) =
        reply_table().register_with_waker(pair.issuer_connection(), None, PendingReplyKind::Import);
    issuer_manager
        .service_exports
        .receive(recipient, vec![HANDLER.into()], &receive_pending)
        .unwrap();

    // SAFETY: the issuer node and its cluster are live.
    let cluster = unsafe { &*(*pair.issuer.as_ptr()).cluster };
    // SAFETY: the recipient node is live.
    let recipient_slot = unsafe { (*pair.recipient.as_ptr()).route_slot };
    cluster.apply_swim_gossip(&[(
        recipient_slot,
        recipient.session_incarnation,
        crate::cluster::MEMBER_DEAD,
        u64::MAX / 2,
    )]);
    assert_eq!(
        cluster.member_state(recipient_slot),
        crate::cluster::MEMBER_DEAD
    );
    for pending in [&offer_pending, &receive_pending] {
        assert!(eventually(|| pending.outcome.lock().unwrap().is_some()));
        let outcome = pending.outcome.lock().unwrap().take().unwrap();
        assert_eq!(outcome.status, ReplyStatus::Failed);
        assert_eq!(outcome.ask_error, AskError::Partition);
    }
    drop(export);
    pair.stop();
}

/// Membership callbacks settle the observing node's waits even when another
/// node in the same runtime is `CURRENT_NODE`.
#[test]
fn burial_settles_the_observing_nodes_receive() {
    let _guard = crate::runtime_test_guard();
    for terminal in [crate::cluster::MEMBER_DEAD, crate::cluster::MEMBER_LEFT] {
        let pair = Pair::start(387, 388);
        let waiting = pair.receive();
        let issuer = pair.issuer_session();
        // SAFETY: the recipient owns its live cluster throughout the test.
        let cluster = unsafe { &*(*pair.recipient.as_ptr()).cluster };
        // SAFETY: the issuer remains live and its route slot is immutable.
        let issuer_slot = unsafe { (*pair.issuer.as_ptr()).route_slot };
        cluster.apply_swim_gossip(&[(
            issuer_slot,
            issuer.session_incarnation,
            terminal,
            u64::MAX / 2,
        )]);
        assert_eq!(cluster.member_state(issuer_slot), terminal);
        {
            let outcome = waiting.outcome.lock().unwrap();
            let outcome = outcome
                .as_ref()
                .expect("the observing node's wait must settle");
            assert_eq!(outcome.status, ReplyStatus::Failed);
            assert_eq!(outcome.ask_error, AskError::Partition);
        }
        drop(waiting);
        pair.stop();
    }
}

/// A send admitted before export close may still be waiting for a pool worker
/// or the channel lock. It must not publish the closed grant afterwards.
#[test]
fn closing_an_export_with_a_delayed_offer_does_not_publish_it() {
    let _guard = crate::runtime_test_guard();
    let pair = Pair::start(389, 390);
    let waiting = pair.receive();
    let (export, offer) = pair.export(20);
    let manager = pair.issuer_manager();
    let gate = ReleaseGate(
        manager
            .service_exports
            .offer_send_gate_for_test(offer.grant_id),
    );
    gate.0.arm(true);
    manager.service_exports.deliver(offer.grant_id, None);
    assert!(gate.0.wait_for_enter(Duration::from_secs(10)));
    drop(export);
    let (next_export, next) = pair.export(21);
    manager.service_exports.deliver(next.grant_id, None);
    gate.0.release();
    assert!(eventually(|| imported(&waiting).is_some()));
    assert_eq!(
        imported(&waiting),
        Some(next.grant_id),
        "a closed grant must not be offered later"
    );
    drop(next_export);
    pair.stop();
}
