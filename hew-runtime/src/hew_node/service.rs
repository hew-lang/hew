//! Node-session service offers and exact-actor export admission.

use super::{
    ConnectionKey, PendingReply, PendingReplyKind, ReplyOutcome, ReplyRoutingTable, ReplyStatus,
};
use crate::envelope::{NodeSessionIdentity, ServiceOfferPayload};
use crate::internal::types::AskError;
use crate::lifetime::live_actors::{self, ActorIncarnation};
use crate::node_identity::Location;
use crate::util::MutexExt;
use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, Weak};

const SERVICE_EXPORT_LIMIT: usize = 256;
const SERVICE_OFFER_LIMIT: usize = 256;

#[derive(Debug)]
struct ExportRecord {
    target: ActorIncarnation,
    offer: ServiceOfferPayload,
}

struct ImportWaiter {
    issuer: NodeSessionIdentity,
    handlers: Vec<String>,
    pending: Weak<PendingReply>,
}

struct ServiceState {
    closed: bool,
    next_grant: u64,
    exports: HashMap<u64, ExportRecord>,
    offers: VecDeque<ServiceOfferPayload>,
    waiters: Vec<ImportWaiter>,
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
                next_grant: 1,
                exports: HashMap::new(),
                offers: VecDeque::new(),
                waiters: Vec::new(),
            }),
        }
    }
}

impl ServiceExports {
    pub(crate) fn observe_session(&self, peer: NodeSessionIdentity) {
        let retired = {
            let mut state = self.state.lock_or_recover();
            state.exports.retain(|_, record| {
                record.offer.recipient.node_id != peer.node_id
                    || record.offer.recipient.session_incarnation >= peer.session_incarnation
            });
            state.offers.retain(|offer| {
                offer.target.node() != peer.node_id
                    || offer.target.incarnation() >= peer.session_incarnation
            });
            let mut retired = Vec::new();
            state.waiters.retain(|waiter| {
                let current = waiter.issuer.node_id != peer.node_id
                    || waiter.issuer.session_incarnation >= peer.session_incarnation;
                if !current {
                    retired.extend(waiter.pending.upgrade());
                }
                current
            });
            retired
        };
        for pending in retired {
            super::reply_table().fail(pending.request_id, AskError::StaleRef);
        }
    }

    pub(crate) fn close(&self) {
        let waiters = {
            let mut state = self.state.lock_or_recover();
            state.closed = true;
            state.exports.clear();
            state.offers.clear();
            std::mem::take(&mut state.waiters)
        };
        for waiter in waiters {
            if let Some(pending) = waiter.pending.upgrade() {
                super::reply_table().fail(pending.request_id, AskError::LocalShutdown);
            }
        }
    }

    pub(super) fn export(
        self: &Arc<Self>,
        recipient: NodeSessionIdentity,
        request_id: u64,
        target: ActorIncarnation,
        location: Location,
        mut handlers: Vec<String>,
    ) -> Result<(ExportLease, ServiceOfferPayload), AskError> {
        handlers.sort();
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
        let pin = live_actors::pin_actor_by_id(target.actor_id()).ok_or(AskError::StaleRef)?;
        if pin.actor().spawn_serial != target.spawn_serial() {
            return Err(AskError::StaleRef);
        }
        let dispatch =
            pin.actor().dispatch.ok_or(AskError::Unauthorized)? as *const std::ffi::c_void;
        if !offer.handlers.iter().all(|handler| {
            crate::xnode_serial::local_message(dispatch, handler).is_some_and(|message| {
                crate::xnode_serial::lookup_request(dispatch, message).is_some()
                    && crate::xnode_serial::lookup_reply(dispatch, message).is_some()
            })
        }) {
            return Err(AskError::Unauthorized);
        }
        state.exports.insert(
            grant_id,
            ExportRecord {
                target,
                offer: offer.clone(),
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

    pub(crate) fn offer(
        &self,
        offer: ServiceOfferPayload,
        publication: u64,
    ) -> Result<(), AskError> {
        let mut state = self.state.lock_or_recover();
        if state.closed {
            return Err(AskError::LocalShutdown);
        }
        if state.offers.iter().any(|existing| {
            existing.target.node() == offer.target.node()
                && existing.target.incarnation() == offer.target.incarnation()
                && existing.grant_id == offer.grant_id
        }) {
            return Err(AskError::Unauthorized);
        }
        state
            .waiters
            .retain(|waiter| waiter.pending.strong_count() != 0);
        if let Some(index) = state.waiters.iter().position(|waiter| {
            waiter.issuer.node_id == offer.target.node()
                && waiter.issuer.session_incarnation == offer.target.incarnation()
                && waiter
                    .pending
                    .upgrade()
                    .is_some_and(|pending| pending.kind == PendingReplyKind::Import(publication))
        }) {
            let waiter = state.waiters.remove(index);
            if let Some(pending) = waiter.pending.upgrade() {
                if waiter.handlers == offer.handlers {
                    if Self::complete_import(&pending, &offer) {
                        return Ok(());
                    }
                } else {
                    super::reply_table().fail(pending.request_id, AskError::Unauthorized);
                }
            }
        }
        if state.offers.len() >= SERVICE_OFFER_LIMIT {
            return Err(AskError::WorkerAtCapacity);
        }
        state.offers.push_back(offer);
        Ok(())
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
        if let Some(index) = state.offers.iter().position(|offer| {
            offer.target.node() == issuer.node_id
                && offer.target.incarnation() == issuer.session_incarnation
        }) {
            if state.offers[index].handlers != handlers {
                return Err(AskError::Unauthorized);
            }
            if Self::complete_import(pending, &state.offers[index]) {
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

    fn complete_import(pending: &PendingReply, offer: &ServiceOfferPayload) -> bool {
        let Ok(data) = crate::envelope::encode_service_offer_payload(offer) else {
            return false;
        };
        if super::reply_table()
            .remove_if_connection(pending.request_id, pending.connection, pending.kind)
            .is_none()
        {
            return false;
        }
        ReplyRoutingTable::complete_pending(
            pending,
            ReplyOutcome {
                status: ReplyStatus::Success,
                data,
                ask_error: AskError::None,
            },
        );
        true
    }

    pub(crate) fn acknowledge(
        &self,
        connection: ConnectionKey,
        peer: NodeSessionIdentity,
        publication: u64,
        result: &crate::envelope::SetupResultPayload,
    ) -> bool {
        let state = self.state.lock_or_recover();
        let Some(record) = state.exports.get(&result.ref_id) else {
            return false;
        };
        if record.offer.request_id != result.setup_id
            || record.offer.target != result.target
            || record.offer.recipient != peer
        {
            return false;
        }
        let Some(pending) = super::reply_table().remove_if_connection(
            result.setup_id,
            connection,
            PendingReplyKind::Offer(publication),
        ) else {
            return false;
        };
        if result.status == crate::envelope::SETUP_STATUS_ACCEPTED {
            ReplyRoutingTable::complete_pending(
                &pending,
                ReplyOutcome {
                    status: ReplyStatus::Success,
                    data: Vec::new(),
                    ask_error: AskError::None,
                },
            );
        } else {
            ReplyRoutingTable::fail_pending_with_reason(&pending, AskError::Unauthorized);
        }
        true
    }

    /// Lifecycle rights share the export's recipient and exact incarnation; each
    /// setup must also retain its currently authenticated publication.
    pub(crate) fn claim_observation<R>(
        &self,
        target: ActorIncarnation,
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
            || record.target != target
            || record.offer.recipient != peer
            || publication_removed.load(Ordering::Acquire)
        {
            return Err(AskError::Unauthorized);
        }
        Ok(claim())
    }

    fn claim<R>(
        &self,
        target: ActorIncarnation,
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
        live_actors::with_live_incarnation(target, |_| ()).ok_or(AskError::StaleRef)?;
        let record = state.exports.get(&grant).ok_or(AskError::Unauthorized)?;
        if grant == 0
            || record.target != target
            || record.offer.recipient != peer
            || record
                .offer
                .handlers
                .binary_search_by(|candidate| candidate.as_str().cmp(handler))
                .is_err()
        {
            return Err(AskError::Unauthorized);
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
    fn drop(&mut self) {
        if let Some(owner) = self.owner.upgrade() {
            owner.state.lock_or_recover().exports.remove(&self.grant_id);
        }
    }
}

#[derive(Debug)]
pub(crate) struct RemoteAdmission {
    pub(crate) owner: Arc<ServiceExports>,
    pub(crate) target: ActorIncarnation,
    pub(crate) peer: NodeSessionIdentity,
    pub(crate) grant: u64,
    pub(crate) handler: String,
    pub(crate) publication_removed: Arc<AtomicBool>,
}

impl RemoteAdmission {
    pub(crate) fn new(
        manager: &crate::connection::HewConnMgr,
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
            self.target,
            self.peer,
            self.grant,
            &self.handler,
            &self.publication_removed,
            claim,
        )
    }
}
