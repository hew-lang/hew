//! Node-session service offers and semantic-target export admission.

use super::{
    ConnectionKey, PendingReply, PendingReplyKind, ReplyOutcome, ReplyRoutingTable, ReplyStatus,
};
use crate::envelope::{NodeSessionIdentity, ServiceOfferPayload};
use crate::internal::types::AskError;
use crate::lifetime::live_actors::{self, ActorIncarnation};
use crate::lifetime::local_handles::{self, HewLocalPidId};
use crate::node_identity::{Location, Target};
use crate::util::MutexExt;
use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, Weak};

const SERVICE_EXPORT_LIMIT: usize = 256;
const SERVICE_OFFER_LIMIT: usize = 256;
/// Closed session-scoped grants whose retraction each successor publication of
/// the recipient session repeats. A conforming recipient that misses more than
/// this many retractions keeps the oldest ones until the issuer session ends;
/// the issuer still refuses every call on them.
const SERVICE_RETRACT_LIMIT: usize = SERVICE_EXPORT_LIMIT;
/// Session-scoped grants this recipient already imported, so a re-sent offer is
/// acknowledged without importing the grant twice. New session-scoped imports
/// are refused at capacity until a retraction or session replacement frees an
/// entry; losing a retraction must not make a live grant importable again.
const SERVICE_IMPORTED_LIMIT: usize = SERVICE_EXPORT_LIMIT;

#[derive(Debug)]
struct ExportRecord {
    authority: ExportAuthority,
    dispatch: usize,
    offer: ServiceOfferPayload,
    /// The recipient session acknowledges this offer on any of its
    /// publications, and closing the grant retracts it.
    session_scoped: bool,
    acknowledged: bool,
    delivery: Arc<OfferDelivery>,
}

struct OfferDelivery {
    live: AtomicBool,
    #[cfg(test)]
    before_send: Arc<super::inbound::TestGate>,
}

impl std::fmt::Debug for OfferDelivery {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OfferDelivery")
            .field("live", &self.live)
            .finish_non_exhaustive()
    }
}

impl Drop for ExportRecord {
    fn drop(&mut self) {
        self.delivery.live.store(false, Ordering::Release);
    }
}

impl ExportRecord {
    fn awaiting_acknowledgement(&self) -> bool {
        self.session_scoped && !self.acknowledged
    }
}

/// An offer queued at the recipient until a receive takes it.
struct QueuedOffer {
    offer: ServiceOfferPayload,
    session_scoped: bool,
}

/// A closed grant whose retraction the recipient may not have read yet.
struct Retraction {
    recipient: NodeSessionIdentity,
    grant_id: u64,
    target: Location,
}

fn issuer_session(offer: &ServiceOfferPayload) -> NodeSessionIdentity {
    NodeSessionIdentity {
        node_id: offer.target.node(),
        session_incarnation: offer.target.incarnation(),
    }
}

fn superseded_by(session: NodeSessionIdentity, current: NodeSessionIdentity) -> bool {
    session.node_id == current.node_id && session.session_incarnation < current.session_incarnation
}

/// A role grant owns the declared role authority, independently of its worker.
#[derive(Debug)]
enum ExportAuthority {
    Direct(ActorIncarnation),
    Role(HewLocalPidId),
}

impl ExportAuthority {
    fn accepts(&self, selected: ActorIncarnation) -> bool {
        match self {
            Self::Direct(target) => *target == selected,
            Self::Role(_) => true,
        }
    }

    fn live(&self) -> bool {
        match self {
            Self::Direct(target) => live_actors::with_live_incarnation(*target, |_| ()).is_some(),
            Self::Role(root) => crate::supervisor::actor_role_authority_live(*root),
        }
    }
}

fn export_metadata(
    local_route: u16,
    location: Location,
) -> Result<(ExportAuthority, usize), AskError> {
    match location.target() {
        Target::Direct(_) => {
            local_handles::with_current_target_actor(location.target(), local_route, |actor| {
                // SAFETY: the target helper pins this exact actor during the borrow.
                let dispatch = unsafe { (*actor).dispatch }.ok_or(AskError::Unauthorized)? as usize;
                // SAFETY: the same send pin retains this incarnation.
                let target = unsafe { ActorIncarnation::of(actor) };
                Ok((ExportAuthority::Direct(target), dispatch))
            })
            .ok_or(AskError::StaleRef)?
        }
        Target::Role(id) => {
            let token = local_handles::carried_role(id.get()).ok_or(AskError::StaleRef)?;
            let (root, dispatch) =
                crate::supervisor::actor_role_export_metadata(token).ok_or(AskError::StaleRef)?;
            Ok((ExportAuthority::Role(root), dispatch))
        }
    }
}

struct ImportWaiter {
    issuer: NodeSessionIdentity,
    handlers: Vec<String>,
    pending: Weak<PendingReply>,
}

struct ServiceState {
    closed: bool,
    /// The connection manager that owns these exports. It is used only under
    /// this lock while `closed` is false: manager free closes the exports
    /// before releasing the manager.
    manager: *const crate::connection::HewConnMgr,
    next_grant: u64,
    exports: HashMap<u64, ExportRecord>,
    retractions: VecDeque<Retraction>,
    offers: VecDeque<QueuedOffer>,
    imported: VecDeque<ServiceOfferPayload>,
    waiters: Vec<ImportWaiter>,
}

// SAFETY: the manager pointer is dereferenced only under the state lock while
// the owning manager is open; see `ServiceState::manager`.
unsafe impl Send for ServiceState {}

impl ServiceState {
    /// Send one control frame to `recipient` on its current publication when
    /// that publication negotiated service sessions. A missing or failed send
    /// is repeated by the recipient session's next publication.
    fn send_to_session(
        &self,
        recipient: NodeSessionIdentity,
        ctrl_kind: u64,
        payload: Vec<u8>,
        fallback: Option<crate::connection::SendClaim>,
        delivery: Option<Arc<OfferDelivery>>,
    ) {
        if self.closed || self.manager.is_null() {
            return;
        }
        // SAFETY: an open state's manager outlives this lock; see `manager`.
        let manager = unsafe { &*self.manager };
        let Some(claim) =
            crate::connection::claim_send_by_identity(manager, recipient.node_id, None)
                .or(fallback)
        else {
            return;
        };
        if claim.peer_session() != recipient || !claim.service_sessions() {
            return;
        }
        let Ok(bytes) = crate::envelope::encode_control_frame(&crate::envelope::ControlFrame {
            version: crate::envelope::WIRE_VERSION,
            ctrl_kind,
            payload,
        }) else {
            return;
        };
        let _ = claim.submit(
            move || {
                if let Some(delivery) = delivery {
                    #[cfg(test)]
                    delivery.before_send.hit();
                    // Preparation runs under the channel's send lock. Closing
                    // a grant before this point must not publish a later offer
                    // after its retraction, even if pool workers run out of order.
                    if !delivery.live.load(Ordering::Acquire) {
                        return None;
                    }
                }
                Some(bytes)
            },
            |_| {},
        );
    }

    fn send_offer(
        &self,
        offer: &ServiceOfferPayload,
        fallback: Option<crate::connection::SendClaim>,
        delivery: Arc<OfferDelivery>,
    ) {
        if let Ok(payload) = crate::envelope::encode_service_offer_payload(offer) {
            self.send_to_session(
                offer.recipient,
                crate::envelope::CTRL_SERVICE_OFFER,
                payload,
                fallback,
                Some(delivery),
            );
        }
    }

    fn send_retraction(&self, retraction: &Retraction) {
        self.send_to_session(
            retraction.recipient,
            crate::envelope::CTRL_SERVICE_RETRACT,
            crate::envelope::encode_service_retract_payload(
                &crate::envelope::ServiceRetractPayload {
                    grant_id: retraction.grant_id,
                    target: retraction.target,
                },
            ),
            None,
            None,
        );
    }

    fn complete_import(
        &mut self,
        pending: &PendingReply,
        offer: &ServiceOfferPayload,
        session_scoped: bool,
    ) -> Result<bool, AskError> {
        if session_scoped && self.imported.len() >= SERVICE_IMPORTED_LIMIT {
            return Err(AskError::WorkerAtCapacity);
        }
        let data = crate::envelope::encode_service_offer_payload(offer)
            .map_err(|_| AskError::EncodeFailed)?;
        if super::reply_table()
            .remove_if_connection(pending.request_id, pending.connection, pending.kind)
            .is_none()
        {
            return Ok(false);
        }
        // Capacity acceptance and history recording precede publication. A
        // woken receiver can never observe a value without its replay fence.
        if session_scoped {
            self.imported.push_back(offer.clone());
        }
        ReplyRoutingTable::complete_pending(
            pending,
            ReplyOutcome {
                status: ReplyStatus::Success,
                data,
                ask_error: AskError::None,
            },
        );
        Ok(true)
    }
}

pub(crate) struct ServiceExports {
    state: Mutex<ServiceState>,
}

impl std::fmt::Debug for ServiceExports {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ServiceExports").finish_non_exhaustive()
    }
}

impl Default for ServiceExports {
    fn default() -> Self {
        Self {
            state: Mutex::new(ServiceState {
                closed: false,
                manager: std::ptr::null(),
                next_grant: 1,
                exports: HashMap::new(),
                retractions: VecDeque::new(),
                offers: VecDeque::new(),
                imported: VecDeque::new(),
                waiters: Vec::new(),
            }),
        }
    }
}

impl ServiceExports {
    /// Bind the manager that sends this node's offers and retractions.
    pub(crate) fn bind(&self, manager: *const crate::connection::HewConnMgr) {
        self.state.lock_or_recover().manager = manager;
    }

    /// A publication of `peer` is current. Grants and offers bound to an older
    /// session of that node retire; offers and retractions for this session
    /// that it may not have read are sent again on the new publication.
    pub(crate) fn observe_session(&self, peer: NodeSessionIdentity) {
        let mut failed = Vec::new();
        {
            let mut state = self.state.lock_or_recover();
            state.exports.retain(|_, record| {
                let current = !superseded_by(record.offer.recipient, peer);
                if !current && record.awaiting_acknowledgement() {
                    failed.push(record.offer.request_id);
                }
                current
            });
            state
                .retractions
                .retain(|retraction| !superseded_by(retraction.recipient, peer));
            state
                .offers
                .retain(|queued| !superseded_by(issuer_session(&queued.offer), peer));
            state
                .imported
                .retain(|offer| !superseded_by(issuer_session(offer), peer));
            state.waiters.retain(|waiter| {
                let current = !superseded_by(waiter.issuer, peer);
                if !current {
                    failed.extend(waiter.pending.upgrade().map(|pending| pending.request_id));
                }
                current
            });
            for retraction in state.retractions.iter().filter(|r| r.recipient == peer) {
                state.send_retraction(retraction);
            }
            for record in state.exports.values().filter(|record| {
                record.offer.recipient == peer && record.awaiting_acknowledgement()
            }) {
                state.send_offer(&record.offer, None, Arc::clone(&record.delivery));
            }
        }
        if let Some(table) = super::reply_table_opt() {
            for request_id in failed {
                table.fail(request_id, AskError::StaleRef);
            }
        }
    }

    /// Membership buried `peer`: an offer to it or a receive from it cannot
    /// complete on a later publication of that session, so it fails now.
    pub(crate) fn retire_session(&self, peer: NodeSessionIdentity) {
        let failed = {
            let state = self.state.lock_or_recover();
            state
                .exports
                .values()
                .filter(|record| {
                    record.offer.recipient == peer && record.awaiting_acknowledgement()
                })
                .map(|record| record.offer.request_id)
                .chain(
                    state
                        .waiters
                        .iter()
                        .filter(|waiter| waiter.issuer == peer)
                        .filter_map(|waiter| waiter.pending.upgrade())
                        .map(|pending| pending.request_id),
                )
                .collect::<Vec<_>>()
        };
        if let Some(table) = super::reply_table_opt() {
            for request_id in failed {
                table.fail(request_id, AskError::Partition);
            }
        }
    }

    pub(crate) fn close(&self) {
        let failed = {
            let mut state = self.state.lock_or_recover();
            state.closed = true;
            // Own every pending export, including one allocated before its
            // first send marks it session-scoped. Accepted offers already left
            // the reply table, so failing their request ID is harmless.
            let failed = state
                .exports
                .values()
                .map(|record| record.offer.request_id)
                .chain(state.waiters.iter().filter_map(|waiter| {
                    waiter.pending.upgrade().map(|pending| pending.request_id)
                }))
                .collect::<Vec<_>>();
            state.exports.clear();
            state.retractions.clear();
            state.offers.clear();
            state.imported.clear();
            state.waiters.clear();
            failed
        };
        for request_id in failed {
            super::reply_table().fail(request_id, AskError::LocalShutdown);
        }
    }

    pub(super) fn export(
        self: &Arc<Self>,
        local_route: u16,
        recipient: NodeSessionIdentity,
        request_id: u64,
        location: Location,
        mut handlers: Vec<String>,
    ) -> Result<(ExportLease, ServiceOfferPayload), AskError> {
        handlers.sort();
        let (authority, dispatch) = export_metadata(local_route, location)?;
        let mut state = self.state.lock_or_recover();
        if state.closed {
            return Err(AskError::LocalShutdown);
        }
        if state.exports.len() >= SERVICE_EXPORT_LIMIT {
            return Err(AskError::WorkerAtCapacity);
        }
        let grant_id = state.next_grant;
        state.next_grant = grant_id.checked_add(1).ok_or(AskError::WorkerAtCapacity)?;
        let offer = ServiceOfferPayload {
            request_id,
            grant_id,
            target: location,
            recipient,
            handlers,
        };
        crate::envelope::encode_service_offer_payload(&offer)
            .map_err(|_| AskError::EncodeFailed)?;
        if !authority.live() {
            return Err(AskError::StaleRef);
        }
        let dispatch_ptr = dispatch as *const std::ffi::c_void;
        if !offer.handlers.iter().all(|handler| {
            crate::xnode_serial::local_message(dispatch_ptr, handler).is_some_and(|message| {
                crate::xnode_serial::lookup_request(dispatch_ptr, message).is_some()
                    && crate::xnode_serial::lookup_reply(dispatch_ptr, message).is_some()
            })
        }) {
            return Err(AskError::Unauthorized);
        }
        state.exports.insert(
            grant_id,
            ExportRecord {
                authority,
                dispatch,
                offer: offer.clone(),
                session_scoped: false,
                acknowledged: false,
                delivery: Arc::new(OfferDelivery {
                    live: AtomicBool::new(true),
                    #[cfg(test)]
                    before_send: Arc::default(),
                }),
            },
        );
        Ok((
            ExportLease {
                owner: Arc::downgrade(self),
                grant_id,
            },
            offer,
        ))
    }

    /// Send an exported grant's offer and repeat it on each publication of the
    /// recipient session until that session acknowledges it. The offer goes
    /// on the session's current publication; `acquired`, the publication the
    /// export was requested on, carries it while a successor is still being
    /// admitted, since that successor's publication repeats it.
    pub(super) fn deliver(&self, grant_id: u64, acquired: Option<crate::connection::SendClaim>) {
        let mut state = self.state.lock_or_recover();
        let Some(record) = state.exports.get_mut(&grant_id) else {
            return;
        };
        record.session_scoped = true;
        let offer = record.offer.clone();
        let delivery = Arc::clone(&record.delivery);
        state.send_offer(&offer, acquired, delivery);
    }

    #[cfg(test)]
    pub(super) fn offer_send_gate_for_test(&self, grant_id: u64) -> Arc<super::inbound::TestGate> {
        Arc::clone(
            &self.state.lock_or_recover().exports[&grant_id]
                .delivery
                .before_send,
        )
    }

    /// Record that this grant's offer reached its session-scoped recipient
    /// without sending it, as if the acknowledgement were lost in transit.
    #[cfg(test)]
    pub(super) fn mark_delivered_for_test(&self, grant_id: u64) {
        if let Some(record) = self.state.lock_or_recover().exports.get_mut(&grant_id) {
            record.session_scoped = true;
        }
    }

    /// Whether an offer for `grant_id` from `issuer` is queued for a receive.
    #[cfg(test)]
    pub(super) fn holds_offer(&self, issuer: NodeSessionIdentity, grant_id: u64) -> bool {
        self.state.lock_or_recover().offers.iter().any(|queued| {
            issuer_session(&queued.offer) == issuer && queued.offer.grant_id == grant_id
        })
    }

    /// Admit an authenticated offer. A session-scoped offer this node already
    /// holds or imported is acknowledged again without a second import.
    pub(crate) fn offer(
        &self,
        offer: ServiceOfferPayload,
        session_scoped: bool,
    ) -> Result<(), AskError> {
        let mut state = self.state.lock_or_recover();
        if state.closed {
            return Err(AskError::LocalShutdown);
        }
        let issuer = issuer_session(&offer);
        let same_grant = |held: &ServiceOfferPayload| {
            issuer_session(held) == issuer && held.grant_id == offer.grant_id
        };
        if let Some(held) = state
            .offers
            .iter()
            .map(|queued| &queued.offer)
            .chain(state.imported.iter())
            .find(|held| same_grant(held))
        {
            return if *held == offer {
                Ok(())
            } else {
                Err(AskError::Unauthorized)
            };
        }
        state
            .waiters
            .retain(|waiter| waiter.pending.strong_count() != 0);
        while let Some(index) = state
            .waiters
            .iter()
            .position(|waiter| waiter.issuer == issuer)
        {
            let compatible = state.waiters[index].handlers == offer.handlers;
            let pending = state.waiters[index].pending.upgrade();
            if let Some(pending) = pending {
                if compatible {
                    if state.complete_import(&pending, &offer, session_scoped)? {
                        state.waiters.remove(index);
                        return Ok(());
                    }
                } else {
                    super::reply_table().fail(pending.request_id, AskError::Unauthorized);
                }
            }
            state.waiters.remove(index);
        }
        if state.offers.len() >= SERVICE_OFFER_LIMIT {
            return Err(AskError::WorkerAtCapacity);
        }
        state.offers.push_back(QueuedOffer {
            offer,
            session_scoped,
        });
        Ok(())
    }

    /// The issuer closed `grant_id`: an offer still queued for it is no longer
    /// receivable, and the grant needs no further de-duplication.
    pub(crate) fn retract(&self, issuer: NodeSessionIdentity, grant_id: u64) {
        let mut state = self.state.lock_or_recover();
        let other_grant = |held: &ServiceOfferPayload| {
            issuer_session(held) != issuer || held.grant_id != grant_id
        };
        state.offers.retain(|queued| other_grant(&queued.offer));
        state.imported.retain(|offer| other_grant(offer));
    }

    pub(super) fn receive(
        &self,
        issuer: NodeSessionIdentity,
        mut handlers: Vec<String>,
        pending: &Arc<PendingReply>,
    ) -> Result<(), AskError> {
        handlers.sort();
        let mut state = self.state.lock_or_recover();
        if state.closed {
            return Err(AskError::LocalShutdown);
        }
        if let Some(index) = state
            .offers
            .iter()
            .position(|queued| issuer_session(&queued.offer) == issuer)
        {
            if state.offers[index].offer.handlers != handlers {
                return Err(AskError::Unauthorized);
            }
            let offer = state.offers[index].offer.clone();
            let session_scoped = state.offers[index].session_scoped;
            if state.complete_import(pending, &offer, session_scoped)? {
                state.offers.remove(index);
            }
            return Ok(());
        }
        state
            .waiters
            .retain(|waiter| waiter.pending.strong_count() != 0);
        if state.waiters.len() >= SERVICE_OFFER_LIMIT {
            return Err(AskError::WorkerAtCapacity);
        }
        state.waiters.push(ImportWaiter {
            issuer,
            handlers,
            pending: Arc::downgrade(pending),
        });
        Ok(())
    }

    pub(crate) fn acknowledge(
        &self,
        connection: ConnectionKey,
        peer: NodeSessionIdentity,
        publication: u64,
        result: &crate::envelope::SetupResultPayload,
    ) -> bool {
        let mut state = self.state.lock_or_recover();
        let Some(record) = state.exports.get_mut(&result.ref_id) else {
            return false;
        };
        if record.offer.request_id != result.setup_id
            || record.offer.target != result.target
            || record.offer.recipient != peer
        {
            return false;
        }
        let accepted = result.status == crate::envelope::SETUP_STATUS_ACCEPTED;
        let pending = if record.session_scoped {
            record.acknowledged |= accepted;
            super::reply_table().remove_if_manager(
                result.setup_id,
                connection,
                PendingReplyKind::SessionOffer,
            )
        } else {
            super::reply_table().remove_if_connection(
                result.setup_id,
                connection,
                PendingReplyKind::Offer(publication),
            )
        };
        drop(state);
        let Some(pending) = pending else {
            return false;
        };
        if accepted {
            ReplyRoutingTable::complete_pending(
                &pending,
                ReplyOutcome {
                    status: ReplyStatus::Success,
                    data: Vec::new(),
                    ask_error: AskError::None,
                },
            );
        } else {
            let reason = if result.status == crate::envelope::SETUP_STATUS_RESOURCE_EXHAUSTED {
                AskError::WorkerAtCapacity
            } else {
                AskError::Unauthorized
            };
            ReplyRoutingTable::fail_pending_with_reason(&pending, reason);
        }
        true
    }

    /// Lifecycle rights share the export's recipient and semantic target; each
    /// setup must also retain its currently authenticated publication.
    pub(crate) fn claim_observation<R>(
        &self,
        location: Location,
        peer: NodeSessionIdentity,
        grant: u64,
        publication_removed: &AtomicBool,
        claim: impl FnOnce() -> R,
    ) -> Result<R, AskError> {
        let state = self.state.lock_or_recover();
        if state.closed {
            return Err(AskError::LocalShutdown);
        }
        let record = state.exports.get(&grant).ok_or(AskError::Unauthorized)?;
        if grant == 0
            || record.offer.target != location
            || record.offer.recipient != peer
            || publication_removed.load(Ordering::Acquire)
        {
            return Err(AskError::Unauthorized);
        }
        if !record.authority.live() {
            return Err(AskError::StaleRef);
        }
        Ok(claim())
    }

    fn claim<R>(
        &self,
        destination: (Location, ActorIncarnation),
        peer: NodeSessionIdentity,
        grant: u64,
        handler: &str,
        publication_removed: &AtomicBool,
        claim: impl FnOnce() -> R,
    ) -> Result<R, AskError> {
        let state = self.state.lock_or_recover();
        if state.closed {
            return Err(AskError::LocalShutdown);
        }
        let (location, target) = destination;
        let dispatch = live_actors::with_live_incarnation(target, |pin| {
            pin.actor().dispatch.map(|dispatch| dispatch as usize)
        })
        .flatten()
        .ok_or(AskError::StaleRef)?;
        let record = state.exports.get(&grant).ok_or(AskError::Unauthorized)?;
        if grant == 0
            || record.offer.target != location
            || !record.authority.accepts(target)
            || record.dispatch != dispatch
            || record.offer.recipient != peer
            || record
                .offer
                .handlers
                .binary_search_by(|candidate| candidate.as_str().cmp(handler))
                .is_err()
        {
            return Err(AskError::Unauthorized);
        }
        if !record.authority.live() {
            return Err(AskError::StaleRef);
        }
        if publication_removed.load(Ordering::Acquire) {
            return Err(AskError::ConnectionDropped);
        }
        Ok(claim())
    }
}

pub(super) struct ExportLease {
    owner: Weak<ServiceExports>,
    grant_id: u64,
}

impl Drop for ExportLease {
    /// Closing the export revokes the grant; a session-scoped recipient is
    /// told so that an offer it has not yet received stays unreceivable.
    fn drop(&mut self) {
        let Some(owner) = self.owner.upgrade() else {
            return;
        };
        let mut state = owner.state.lock_or_recover();
        let Some(record) = state.exports.remove(&self.grant_id) else {
            return;
        };
        if !record.session_scoped || state.closed {
            return;
        }
        if state.retractions.len() >= SERVICE_RETRACT_LIMIT {
            state.retractions.pop_front();
        }
        let retraction = Retraction {
            recipient: record.offer.recipient,
            grant_id: record.offer.grant_id,
            target: record.offer.target,
        };
        // Revoke queued offers before a worker can send the retraction.
        drop(record);
        state.send_retraction(&retraction);
        state.retractions.push_back(retraction);
    }
}

#[derive(Debug)]
pub(crate) struct RemoteAdmission {
    pub(crate) owner: Arc<ServiceExports>,
    pub(crate) location: Location,
    pub(crate) target: ActorIncarnation,
    pub(crate) peer: NodeSessionIdentity,
    pub(crate) grant: u64,
    pub(crate) handler: String,
    pub(crate) publication_removed: Arc<AtomicBool>,
}

impl RemoteAdmission {
    #[allow(
        clippy::too_many_arguments,
        reason = "admission binds a semantic destination, selected incarnation and authenticated publication"
    )]
    pub(crate) fn new(
        manager: &crate::connection::HewConnMgr,
        location: Location,
        target: ActorIncarnation,
        peer: crate::node_identity::NodeId,
        publication: u64,
        grant: u64,
        handler: &str,
    ) -> Result<Self, AskError> {
        let connection =
            crate::connection::claim_send_by_identity(manager, peer, Some(publication))
                .ok_or(AskError::Unauthorized)?;
        let admission = Self {
            owner: Arc::clone(&manager.service_exports),
            location,
            target,
            peer: connection.peer_session(),
            grant,
            handler: handler.to_owned(),
            publication_removed: connection.publication_removed(),
        };
        admission.claim(|| ())?;
        Ok(admission)
    }

    pub(crate) fn claim<R>(&self, claim: impl FnOnce() -> R) -> Result<R, AskError> {
        self.owner.claim(
            (self.location, self.target),
            self.peer,
            self.grant,
            &self.handler,
            &self.publication_removed,
            claim,
        )
    }
}
