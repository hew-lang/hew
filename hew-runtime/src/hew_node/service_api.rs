//! Source service handles and acquisition through owned remote operations.

use super::remote_call::{HewRemoteCall, RemoteCallRoute};
use super::service::{ExportLease, ServiceExports};
use super::{
    reply_table, with_current_node_read, AskError, ConnectionKey, HewNode, PendingReplyKind,
    ReplyStatus, NODE_STATE_RUNNING,
};
use crate::connection::{self, HewConnMgr, SendClaim};
use crate::envelope::{NodeSessionIdentity, ServiceOfferPayload};
use crate::lifetime::live_actors::{self, ActorIncarnation};
use crate::peer_binding::{PeerCredential, TransportSelection};
use crate::request_deadline::RequestDeadline;
use crate::util::MutexExt;
use std::ffi::{c_char, c_void, CStr};
use std::ptr;
use std::sync::atomic::Ordering;
use std::sync::{Arc, Weak};

struct PeerSession {
    owner: Weak<ServiceExports>,
    local: NodeSessionIdentity,
    peer: NodeSessionIdentity,
}

pub(crate) struct ImportedService {
    owner: Weak<ServiceExports>,
    offer: ServiceOfferPayload,
}

impl ImportedService {
    pub(crate) fn identity(&self) -> crate::node_identity::Location {
        self.offer.target
    }
}

/// Resolve a direct local incarnation through the same location authority used
/// for service exports, while the current node keeps its manager alive.
pub(crate) fn local_actor_identity(
    token: crate::lifetime::local_handles::HewLocalPidId,
) -> Option<crate::node_identity::Location> {
    let actor = crate::lifetime::local_handles::resolve_current_observation_actor(token)?;
    with_manager(|manager| {
        connection::local_location_for_actor(manager, actor).ok_or(AskError::StaleRef)
    })
    .ok()
}

fn local_session(manager: &HewConnMgr) -> Option<NodeSessionIdentity> {
    Some(NodeSessionIdentity {
        node_id: manager.auth.node_identity()?,
        session_incarnation: manager.auth.session_incarnation()?,
    })
}

fn with_manager<T>(run: impl FnOnce(&HewConnMgr) -> Result<T, AskError>) -> Result<T, AskError> {
    with_current_node_read(|guard| {
        // SAFETY: the current-node read guard pins the node and its manager.
        let node =
            unsafe { (*guard as *const HewNode).as_ref() }.ok_or(AskError::NodeNotRunning)?;
        if node.state.load(Ordering::Acquire) != NODE_STATE_RUNNING || node.conn_mgr.is_null() {
            return Err(AskError::NodeNotRunning);
        }
        // SAFETY: a running node owns this manager for the guard's duration.
        run(unsafe { &*node.conn_mgr })
    })
}

fn peer_claim(manager: &HewConnMgr, session: &PeerSession) -> Result<SendClaim, AskError> {
    if local_session(manager) != Some(session.local)
        || !session
            .owner
            .ptr_eq(&Arc::downgrade(&manager.service_exports))
    {
        return Err(AskError::StaleRef);
    }
    let claim = connection::claim_send_by_identity(manager, session.peer.node_id, None)
        .ok_or(AskError::Partition)?;
    if claim.peer_session() != session.peer {
        return Err(AskError::StaleRef);
    }
    Ok(claim)
}

/// Acquire the currently authenticated configured peer, without connecting.
///
/// # Safety
/// `credential` is readable and `output` is writable for one handle.
#[no_mangle]
pub unsafe extern "C" fn hew_node_peer_new(
    credential: *const hew_cabi::string::HewString,
    output: *mut *mut c_void,
) -> i32 {
    // SAFETY: the source passes a live string for this synchronous borrow.
    let credential = unsafe { hew_cabi::string::string_as_str(credential) };
    let result = with_manager(|manager| {
        let bytes = super::api::decode_hex(credential).ok_or(AskError::Unauthorized)?;
        let credential = match manager.auth.transport() {
            TransportSelection::Tcp => {
                PeerCredential::NoiseKey(bytes.try_into().map_err(|_| AskError::Unauthorized)?)
            }
            TransportSelection::QuicMesh if !bytes.is_empty() => PeerCredential::Spki(bytes),
            TransportSelection::QuicMesh => return Err(AskError::Unauthorized),
        };
        manager
            .auth
            .route_slot_for_credential(&credential)
            .ok_or(AskError::Unauthorized)?;
        let claim = connection::claim_send_by_identity(manager, credential.node_id(), None)
            .ok_or(AskError::Partition)?;
        Ok(PeerSession {
            owner: Arc::downgrade(&manager.service_exports),
            local: local_session(manager).ok_or(AskError::Unauthorized)?,
            peer: claim.peer_session(),
        })
    });
    match result {
        Ok(peer) => {
            // SAFETY: the caller supplies its fresh handle slot.
            unsafe { output.write(Box::into_raw(Box::new(peer)).cast()) };
            0
        }
        Err(AskError::Partition) => hew_cabi::node::NodeFailure::Unreachable as i32,
        Err(_) => hew_cabi::node::NodeFailure::Refused as i32,
    }
}

/// Release the local authenticated peer handle.
///
/// # Safety
/// `handle` is null or one live peer handle transferred once.
#[no_mangle]
pub unsafe extern "C" fn hew_peer_session_close(handle: *mut c_void) {
    if !handle.is_null() {
        // SAFETY: the resource's consuming close transfers this allocation.
        drop(unsafe { Box::from_raw(handle.cast::<PeerSession>()) });
    }
}

/// Revoke future admission for an exported actor interface.
///
/// # Safety
/// `handle` is null or one live export handle transferred once.
#[no_mangle]
pub unsafe extern "C" fn hew_service_export_close(handle: *mut c_void) {
    if !handle.is_null() {
        // SAFETY: the resource's consuming close transfers this lease.
        drop(unsafe { Box::from_raw(handle.cast::<ExportLease>()) });
    }
}

unsafe fn handler_interface(
    handlers: *const *const c_char,
    count: usize,
) -> Result<Vec<String>, AskError> {
    if count == 0 || count > crate::envelope::MAX_SERVICE_HANDLERS || handlers.is_null() {
        return Err(AskError::Unauthorized);
    }
    // SAFETY: generated code supplies this bounded array of declaration IDs.
    unsafe { std::slice::from_raw_parts(handlers, count) }
        .iter()
        .map(|handler| {
            // SAFETY: each generated declaration ID is a live C string.
            unsafe { CStr::from_ptr(*handler) }
                .to_str()
                .map(str::to_owned)
                .map_err(|_| AskError::Unauthorized)
        })
        .collect()
}

/// Begin an acknowledged offer or a cancellable wait for an offer.
///
/// # Safety
/// `peer` is borrowed, `actor` is a local actor token for exports, `handlers`
/// contains `count` live declaration-ID C strings and `waker` is live.
#[no_mangle]
pub unsafe extern "C" fn hew_service_acquire_new(
    peer: *const c_void,
    actor: crate::lifetime::local_handles::HewLocalPidId,
    handlers: *const *const c_char,
    count: usize,
    export: bool,
    deadline_ns: i64,
    waker: *const crate::wake::HewWaker,
) -> *mut HewRemoteCall {
    let deadline = RequestDeadline::from_raw(deadline_ns);
    // SAFETY: all three borrows are generated from live source/descriptor values.
    let setup = unsafe { peer.cast::<PeerSession>().as_ref() }
        .ok_or(AskError::StaleRef)
        .and_then(|peer| {
            // SAFETY: forwards the generated declaration array contract.
            let handlers = unsafe { handler_interface(handlers, count) }?;
            with_manager(|manager| {
                if deadline.expired() {
                    return Err(AskError::Timeout);
                }
                let claim = peer_claim(manager, peer)?;
                let connection = ConnectionKey::new(manager, claim.connection_id());
                let kind = if export {
                    PendingReplyKind::Offer(claim.publication_token)
                } else {
                    PendingReplyKind::Import(claim.publication_token)
                };
                // SAFETY: the operation retains the generated readiness target.
                let owned = unsafe { crate::wake::OwnedWaker::retain(&*waker) };
                let (request_id, pending) =
                    reply_table().register_with_waker(connection, Some(owned), kind);
                let result = if export {
                    start_export(manager, peer, actor, handlers, request_id, claim)
                        .map(|lease| RemoteCallRoute::Offer(Arc::clone(&pending), Some(lease)))
                } else {
                    manager
                        .service_exports
                        .receive(peer.peer, handlers, &pending)
                        .map(|()| {
                            RemoteCallRoute::Import(
                                Arc::clone(&pending),
                                Arc::downgrade(&manager.service_exports),
                            )
                        })
                };
                if result.is_err() {
                    reply_table().remove(request_id);
                }
                result
            })
        });
    let route = setup.unwrap_or_else(RemoteCallRoute::Failed);
    Box::into_raw(Box::new(HewRemoteCall {
        route,
        // SAFETY: the timer retains a separate reference to the live descriptor.
        timer: if deadline_ns < 0 {
            ptr::null_mut()
        } else {
            // SAFETY: the timer retains the live descriptor separately.
            unsafe { crate::coro_sleep::hew_coro_sleep_until_new(deadline_ns, waker) }
        },
        timed_out: false,
        deadline,
        dispatch: ptr::null(),
        msg_type: 0,
        reply_size: std::mem::size_of::<*mut c_void>(),
    }))
}

fn start_export(
    manager: &HewConnMgr,
    peer: &PeerSession,
    actor: crate::lifetime::local_handles::HewLocalPidId,
    handlers: Vec<String>,
    request_id: u64,
    claim: SendClaim,
) -> Result<ExportLease, AskError> {
    let actor_id =
        crate::lifetime::local_handles::resolve_current_actor(actor).ok_or(AskError::StaleRef)?;
    let target = live_actors::with_actor_send_by_id(actor_id, |actor| {
        // SAFETY: the send pin retains this exact incarnation.
        unsafe { ActorIncarnation::of(actor) }
    })
    .ok_or(AskError::StaleRef)?;
    let location =
        connection::local_location_for_actor(manager, actor_id).ok_or(AskError::StaleRef)?;
    let (lease, offer) = manager
        .service_exports
        .export(peer.peer, request_id, target, location, handlers)?;
    let payload = crate::envelope::encode_service_offer_payload(&offer)
        .map_err(|_| AskError::EncodeFailed)?;
    let bytes = crate::envelope::encode_control_frame(&crate::envelope::ControlFrame {
        version: crate::envelope::WIRE_VERSION,
        ctrl_kind: crate::envelope::CTRL_SERVICE_OFFER,
        payload,
    })
    .map_err(|_| AskError::EncodeFailed)?;
    let table = Arc::clone(&crate::runtime::rt_current().node.reply_table);
    let completion_table = Arc::clone(&table);
    if !claim.submit(
        move || {
            table
                .pending
                .lock_or_recover()
                .contains_key(&request_id)
                .then_some(bytes)
        },
        move |result| {
            if result.is_err() {
                completion_table.fail(request_id, AskError::ConnectionDropped);
            }
        },
    ) {
        return Err(AskError::SendFailed);
    }
    Ok(lease)
}

pub(super) unsafe fn take_acquisition(operation: &mut HewRemoteCall, output: *mut c_void) -> i32 {
    if operation.timed_out {
        return AskError::Timeout as i32;
    }
    let (RemoteCallRoute::Offer(pending, _) | RemoteCallRoute::Import(pending, _)) =
        &operation.route
    else {
        unreachable!()
    };
    let Some(reply) = pending.outcome.lock_or_recover().take() else {
        return AskError::ConnectionDropped as i32;
    };
    if reply.status != ReplyStatus::Success {
        return reply.ask_error as i32;
    }
    let value = match &mut operation.route {
        RemoteCallRoute::Offer(_, lease) => {
            let Some(lease) = lease.take() else {
                return AskError::StaleRef as i32;
            };
            Box::into_raw(Box::new(lease)) as usize
        }
        RemoteCallRoute::Import(_, owner) => {
            let Ok(offer) = crate::envelope::decode_service_offer_payload(&reply.data) else {
                return AskError::DecodeFailure as i32;
            };
            let descriptor = Arc::new(ImportedService {
                owner: owner.clone(),
                offer,
            });
            match crate::lifetime::local_handles::register_current_imported_actor(descriptor) {
                Ok(token) => token.as_usize(),
                Err(_) => return AskError::LocalShutdown as i32,
            }
        }
        _ => unreachable!(),
    };
    // SAFETY: exports and actor values each have one target-word carrier.
    unsafe { output.cast::<usize>().write(value) };
    AskError::None as i32
}

fn imported_actor_target(
    service: &ImportedService,
    dispatch: *const c_void,
    message: i32,
) -> Result<(crate::node_identity::Location, u64), AskError> {
    with_manager(|manager| {
        if !service
            .owner
            .ptr_eq(&Arc::downgrade(&manager.service_exports))
            || local_session(manager) != Some(service.offer.recipient)
        {
            return Err(AskError::StaleRef);
        }
        let handler =
            crate::xnode_serial::lookup_request(dispatch, message).ok_or(AskError::Unauthorized)?;
        if service
            .offer
            .handlers
            .binary_search_by(|candidate| candidate.as_str().cmp(handler.identity))
            .is_err()
        {
            return Err(AskError::Unauthorized);
        }
        Ok((service.offer.target, service.offer.grant_id))
    })
}

/// Borrow a prepared actor request during authenticated encoding. Its actor
/// call remains the owner until the request is taken or cleanup completes.
/// # Safety
/// The exact request wrapper and waker remain live throughout this call.
#[expect(
    clippy::too_many_arguments,
    reason = "one exact imported actor request"
)]
pub(crate) unsafe fn start_imported_actor_call(
    service: &ImportedService,
    dispatch: *const c_void,
    message: i32,
    request: *mut c_void,
    request_size: usize,
    reply_size: usize,
    deadline_ns: i64,
    waker: *const crate::wake::HewWaker,
    kind: crate::request_state::RequestKind,
) -> *mut HewRemoteCall {
    let target = imported_actor_target(service, dispatch, message);
    // SAFETY: encoding is synchronous and the source retains its wrapper.
    unsafe {
        super::remote_call::start_granted_call(
            target,
            dispatch,
            message,
            request,
            request_size,
            reply_size,
            deadline_ns,
            waker,
            kind,
        )
    }
}
