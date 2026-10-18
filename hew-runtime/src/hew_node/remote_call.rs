//! Remote actor call operations.

use super::{
    quarantine_blocks_send, release_request, remote_reply_data_to_ptr, reply_table,
    with_current_node_read, AskError, ConnectionKey, HewNode, InboundRequest, PendingReply,
    PendingReplyKind, ReplyOutcome, ReplyStatus, NODE_STATE_RUNNING,
};
use crate::connection;
use crate::envelope::encode_envelope_frame_from_raw_parts;
use crate::node_identity::{HewRemotePid, Location};
use crate::request_deadline::RequestDeadline;
use crate::routing;
use crate::set_last_error;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::Ordering;
use std::sync::Arc;

enum RemoteAskSetupResult {
    Ok((Arc<PendingReply>, Location, u64)),
    /// This node owns the target actor.
    Local(u64),
    Error(AskError),
}

/// Set up an outbound remote ask: serialize the request, register a pending
/// reply slot woken through `waker` and send the ask envelope over the mesh.
/// Returns the pending reply and exact route on successful submission, or a typed
/// [`AskError`] on any setup failure.
///
/// # Safety
///
/// - `pid` must be a valid remote actor PID.
/// - `data` must point to at least `size` readable bytes, or be null when
///   `size` is 0.
#[allow(
    clippy::too_many_lines,
    reason = "function orchestrates end-to-end remote ask setup across encoding, auth, and parking"
)]
#[expect(
    clippy::too_many_arguments,
    reason = "the owned request retains its exact grant and deadline"
)]
fn setup_remote_ask(
    target: Location,
    dispatch: *const c_void,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    waker: crate::wake::OwnedWaker,
    deadline: RequestDeadline,
    grant: Option<u64>,
) -> RemoteAskSetupResult {
    if deadline.expired() {
        return RemoteAskSetupResult::Error(AskError::Timeout);
    }
    with_current_node_read(|guard| {
        let node_ptr = *guard as *mut HewNode;
        if node_ptr.is_null() {
            return RemoteAskSetupResult::Error(AskError::NodeNotRunning);
        }
        // SAFETY: read lock pins CURRENT_NODE.
        let node = unsafe { &*node_ptr };
        if node.state.load(Ordering::Acquire) != NODE_STATE_RUNNING || node.conn_mgr.is_null() {
            return RemoteAskSetupResult::Error(AskError::NodeNotRunning);
        }

        // SAFETY: routing_table is valid while node is running.
        let (conn_id, target_node_id) = match unsafe {
            crate::routing::hew_routing_lookup_location(node.routing_table, target)
        } {
            routing::LocationRoute::Remote {
                route_slot, conn, ..
            } => (conn, route_slot),
            routing::LocationRoute::Partition => {
                return RemoteAskSetupResult::Error(AskError::Partition);
            }
            routing::LocationRoute::StaleRef => {
                return RemoteAskSetupResult::Error(AskError::StaleRef);
            }
            routing::LocationRoute::Local { actor_id } => {
                return if grant.is_some() {
                    RemoteAskSetupResult::Error(AskError::Unauthorized)
                } else {
                    RemoteAskSetupResult::Local(actor_id)
                };
            }
        };

        // Quarantine consult: under a Quarantine policy, a peer the failure
        // detector buried and that has not yet rejoined at a strictly higher
        // incarnation fails closed here — before any request bytes are serialized,
        // so nothing is allocated on the blocked path.
        if quarantine_blocks_send(node, target_node_id) {
            return RemoteAskSetupResult::Error(AskError::Partition);
        }

        // Bind this exact actor location to the authenticated manager claim
        // before encoding or allocating a pending reply slot.
        // SAFETY: CURRENT_NODE pins the running node's connection manager.
        let Some(send_claim) =
            (unsafe { connection::claim_send_to_location(&*node.conn_mgr, conn_id, target) })
        else {
            return RemoteAskSetupResult::Error(AskError::Unauthorized);
        };

        let Some(codec) = crate::xnode_serial::lookup_request(dispatch, msg_type) else {
            return RemoteAskSetupResult::Error(AskError::EncodeFailed);
        };
        // Serialize the request value before it leaves this address space —
        // `data`/`size` is the raw in-memory struct (which may hold heap
        // pointers). The codec for `msg_type` encodes its CONTENTS; the receiver
        // reconstructs the value. Every request needs a registered descriptor
        // for its declaration identity, including an empty payload.
        let serialized_req: Option<(*mut u8, usize)> = if size > 0 {
            let mut out_len: usize = 0;
            // SAFETY: data is a valid value of the message type; out_len valid.
            // Keyed by `(dispatch, msg_type)` — the target actor TYPE's request
            // serializer; a colliding `msg_type` on another actor type cannot
            // select the wrong codec.
            let bytes = unsafe {
                crate::xnode_serial::encode_payload(
                    dispatch,
                    msg_type,
                    data.cast_const(),
                    &raw mut out_len,
                )
            };
            if bytes.is_null() {
                return RemoteAskSetupResult::Error(AskError::EncodeFailed);
            }
            Some((bytes, out_len))
        } else {
            None
        };
        let request = match serialized_req {
            Some((bytes, len)) => {
                // SAFETY: the encoder returned this live sized-block allocation.
                let request = unsafe { std::slice::from_raw_parts(bytes, len) }.to_vec();
                // SAFETY: the pool now owns the copied bytes, not the sized block.
                unsafe { crate::xnode_serial::hew_ser_free_bytes(bytes) };
                request
            }
            None => Vec::new(),
        };

        let connection = ConnectionKey::new(node.conn_mgr.cast_const(), conn_id);
        let (request_id, pending) = reply_table().register_with_waker(
            connection,
            Some(waker),
            PendingReplyKind::Ask(send_claim.publication_token),
        );

        // SAFETY: the running node owns a live connection manager.
        let manager = unsafe { &*node.conn_mgr };
        let source = {
            let actor = crate::actor::hew_actor_self();
            if actor.is_null() {
                None
            } else {
                // SAFETY: `hew_actor_self` returned a live actor pointer.
                connection::local_location_for_actor(manager, unsafe { (*actor).id })
            }
        };
        let table = Arc::clone(&crate::runtime::rt_current().node.reply_table);
        let completion_table = Arc::clone(&table);
        let publication = send_claim.publication_token;
        let submitted = send_claim.submit(
            move || {
                // A cancelled or expired call must not start after waiting for
                // the pool or another sender to release the channel.
                if !table
                    .pending
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .contains_key(&request_id)
                {
                    return None;
                }
                let remaining = deadline.remaining();
                if remaining.is_some_and(|budget| budget <= 0) {
                    table.fail(request_id, AskError::Timeout);
                    return None;
                }
                // SAFETY: the job owns the serialized request through preparation.
                match unsafe {
                    encode_envelope_frame_from_raw_parts(
                        Some(target),
                        source,
                        match grant {
                            Some(grant_id) => crate::envelope::EnvelopeMessage::Service {
                                handler: codec.identity.to_owned(),
                                grant_id,
                            },
                            None => {
                                crate::envelope::EnvelopeMessage::Handler(codec.identity.to_owned())
                            }
                        },
                        request.as_ptr(),
                        request.len(),
                        request_id,
                        remaining.map(i64::cast_unsigned),
                    )
                } {
                    Ok(bytes) => Some(bytes),
                    Err(error) => {
                        set_last_error(format!("hew_node_api_ask: {error}"));
                        table.fail(request_id, AskError::EncodeFailed);
                        None
                    }
                }
            },
            move |result| {
                if result.is_err_and(|error| error != connection::ChannelError::Preparation) {
                    completion_table.fail_connection(connection);
                }
            },
        );
        if !submitted {
            reply_table().fail(request_id, AskError::SendFailed);
        }
        RemoteAskSetupResult::Ok((pending, target, publication))
    })
}

/// Materialise a completed [`ReplyOutcome`] into this node's address space:
/// the void sentinel, or a sized-block value the caller owns.
fn remote_reply_value(
    dispatch: *const c_void,
    msg_type: i32,
    reply_size: usize,
    reply: &ReplyOutcome,
) -> Result<*mut c_void, AskError> {
    if reply.status == ReplyStatus::Failed {
        return Err(reply.ask_error);
    }
    // Void ask (reply_size == 0) or empty reply: there is no value to
    // reconstruct.
    if reply_size == 0 || reply.data.is_empty() {
        let result = remote_reply_data_to_ptr(&reply.data, reply_size);
        return if result.is_null() {
            Err(AskError::PayloadSizeMismatch)
        } else {
            Ok(result)
        };
    }
    // `reply.data` holds the serialized reply. `dispatch` is this node's
    // dispatch global for the ask's statically known target actor type, so a
    // colliding `msg_type` on another local actor type cannot select the
    // wrong reply codec.
    // SAFETY: reply.data is a valid slice for its length.
    let (value, struct_size) = unsafe {
        crate::xnode_serial::decode_reply(dispatch, msg_type, reply.data.as_ptr(), reply.data.len())
    };
    if value.is_null() {
        return Err(AskError::DecodeFailure);
    }
    if struct_size != reply_size {
        // A size other than the caller's reply slot is codec or layout drift.
        // SAFETY: value came from decode_reply's sized-block allocation.
        unsafe { crate::mem::buf_free(value) };
        return Err(AskError::PayloadSizeMismatch);
    }
    Ok(value)
}

/// Where a remote call's reply comes from.
pub(super) enum RemoteCallRoute {
    /// A peer owns the target: the pending registration of the encoded ask.
    Wire(Arc<PendingReply>, Location, u64),
    /// This node owns the target: the local actor call carrying the request.
    Local(*mut crate::actor_call_native::HewActorCall),
    Offer(Arc<PendingReply>, Option<super::service::ExportLease>),
    Import(
        Arc<PendingReply>,
        std::sync::Weak<super::service::ServiceExports>,
    ),
    Failed(AskError),
}

/// One coroutine remote ask, from its submission to the reply. The operation
/// owns its route and the deadline until freed.
#[expect(
    missing_debug_implementations,
    reason = "an opaque generated-code operation; its pending reply has no Debug"
)]
pub struct HewRemoteCall {
    pub(super) route: RemoteCallRoute,
    pub(super) timer: *mut crate::coro_sleep::HewCoroSleep,
    pub(super) timed_out: bool,
    pub(super) deadline: RequestDeadline,
    pub(super) dispatch: *const c_void,
    pub(super) msg_type: i32,
    pub(super) reply_size: usize,
}

impl HewRemoteCall {
    pub(crate) fn has_deadline(&self) -> bool {
        self.deadline.raw() >= 0
    }

    fn ready(&mut self) -> bool {
        let pending = match &self.route {
            RemoteCallRoute::Wire(pending, _, _)
            | RemoteCallRoute::Offer(pending, _)
            | RemoteCallRoute::Import(pending, _) => pending,
            RemoteCallRoute::Local(call) => {
                // SAFETY: the operation exclusively drives its live local call.
                return unsafe { crate::actor_call_native::hew_actor_call_poll(*call) } != -1;
            }
            RemoteCallRoute::Failed(_) => return true,
        };
        if self.timed_out
            || pending
                .outcome
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .is_some()
        {
            return true;
        }
        if self.deadline.raw() < 0 {
            return false;
        }
        // SAFETY: this operation uniquely owns its timer.
        let timer_status = unsafe { crate::coro_sleep::hew_coro_sleep_status(self.timer) };
        if !self.deadline.expired() && timer_status == 0 {
            return false;
        }
        // Withdrawing the registration settles the race: a completion that
        // already removed it deposits its outcome and wakes this call again.
        self.timed_out = reply_table().remove(pending.request_id).is_some();
        if self.timed_out {
            withdraw_remote(&self.route);
        }
        self.timed_out
    }
}

/// Start an authenticated call while its actor operation retains the original
/// request. Encoding borrows its fields; no byte or field loan escapes setup.
///
/// # Safety
/// The request and waker remain live through this synchronous encoding step.
#[expect(
    clippy::too_many_arguments,
    reason = "one exact granted request contract"
)]
pub(super) unsafe fn start_granted_call(
    target: Result<(Location, u64), AskError>,
    dispatch: *const c_void,
    message: i32,
    request: *mut c_void,
    request_size: usize,
    reply_size: usize,
    deadline_ns: i64,
    waker: *const crate::wake::HewWaker,
) -> *mut HewRemoteCall {
    let deadline = RequestDeadline::from_raw(deadline_ns);
    let setup = match target {
        Ok((target, grant)) => {
            // SAFETY: the caller retains the live descriptor through setup.
            let wake = unsafe { crate::wake::OwnedWaker::retain(&*waker) };
            setup_remote_ask(
                target,
                dispatch,
                message,
                request,
                request_size,
                wake,
                deadline,
                Some(grant),
            )
        }
        Err(error) => RemoteAskSetupResult::Error(error),
    };
    let route = match setup {
        RemoteAskSetupResult::Ok((pending, target, publication)) => {
            RemoteCallRoute::Wire(pending, target, publication)
        }
        RemoteAskSetupResult::Error(error) => RemoteCallRoute::Failed(error),
        // A peer grant never authorizes a local-location fallback.
        RemoteAskSetupResult::Local(_) => RemoteCallRoute::Failed(AskError::Unauthorized),
    };
    Box::into_raw(Box::new(HewRemoteCall {
        route,
        // SAFETY: the deadline operation retains the caller's live waker.
        timer: if deadline_ns >= 0 {
            unsafe { crate::coro_sleep::hew_coro_sleep_until_new(deadline_ns, waker) }
        } else {
            ptr::null_mut()
        },
        timed_out: false,
        deadline,
        dispatch,
        msg_type: message,
        reply_size,
    }))
}

impl Drop for HewRemoteCall {
    fn drop(&mut self) {
        match &self.route {
            // A late reply finds no registration and is dropped.
            RemoteCallRoute::Wire(pending, _, _)
            | RemoteCallRoute::Offer(pending, _)
            | RemoteCallRoute::Import(pending, _) => {
                if reply_table().remove(pending.request_id).is_some() {
                    withdraw_remote(&self.route);
                }
            }
            // SAFETY: generated code drained the call's cleanup before freeing.
            RemoteCallRoute::Local(call) => unsafe {
                crate::actor_call_native::hew_actor_call_free(*call);
            },
            RemoteCallRoute::Failed(_) => {}
        }
        // SAFETY: the operation uniquely owns its timer, released once here.
        unsafe { crate::coro_sleep::hew_coro_sleep_free(self.timer) };
    }
}

/// Best-effort withdrawal is bound to the same authenticated connection publication.
fn withdraw_remote(route: &RemoteCallRoute) {
    let RemoteCallRoute::Wire(pending, target, publication) = route else {
        return;
    };
    with_current_node_read(|guard| {
        // SAFETY: the read guard pins the optional current node for this closure.
        let Some(node) = (unsafe { (*guard as *const HewNode).as_ref() }) else {
            return;
        };
        if node.conn_mgr as usize != pending.connection.conn_mgr
            || node.state.load(Ordering::Acquire) != NODE_STATE_RUNNING
        {
            return;
        }
        // SAFETY: CURRENT_NODE pins the manager until its owned claim is acquired.
        let Some(claim) = connection::claim_send(
            unsafe { &*node.conn_mgr },
            pending.connection.conn_id,
            Some(*publication),
        ) else {
            return;
        };
        // SAFETY: the withdrawal has no payload.
        if let Ok(bytes) = unsafe {
            encode_envelope_frame_from_raw_parts(
                Some(*target),
                None,
                crate::envelope::EnvelopeMessage::Withdraw,
                ptr::null(),
                0,
                pending.request_id,
                None,
            )
        } {
            let _ = claim.submit(move || Some(bytes), |_| {});
        }
    });
}

/// Submit `request` to the actor at `target`, consuming its fields. A peer
/// receives the bytes of the target actor's registered codec; an actor on this
/// node receives the fields through the local call path, as the peer's inbound
/// ask would deliver them. Setup failures are latched as the call's outcome,
/// so the caller always polls, takes, drains and frees the operation.
///
/// # Safety
///
/// - `target` must be null or a readable carried location.
/// - `request` must hold `request_size` readable bytes of the member's message
///   wrapper; its fields transfer to this call and the caller keeps the bytes.
/// - `waker` must be a live descriptor.
#[no_mangle]
pub unsafe extern "C" fn hew_remote_call_new(
    target: *const HewRemotePid,
    dispatch: *const c_void,
    msg_type: i32,
    request: *mut c_void,
    request_size: usize,
    reply_size: usize,
    timeout_ms: u64,
    inherited_deadline_ns: i64,
    waker: *const crate::wake::HewWaker,
) -> *mut HewRemoteCall {
    // SAFETY: forwards the caller's owned-request and readable target contracts.
    unsafe {
        new_call(
            target,
            None,
            dispatch,
            msg_type,
            request,
            request_size,
            reply_size,
            timeout_ms,
            inherited_deadline_ns,
            waker,
        )
    }
}

#[expect(
    clippy::too_many_arguments,
    reason = "the existing owned remote operation ABI"
)]
pub(super) unsafe fn new_call(
    target: *const HewRemotePid,
    grant: Option<u64>,
    dispatch: *const c_void,
    msg_type: i32,
    request: *mut c_void,
    request_size: usize,
    reply_size: usize,
    timeout_ms: u64,
    inherited_deadline_ns: i64,
    waker: *const crate::wake::HewWaker,
) -> *mut HewRemoteCall {
    // SAFETY: the caller supplies a live descriptor.
    let owned = unsafe { crate::wake::OwnedWaker::retain(&*waker) };
    let duration_ns = i64::try_from(u128::from(timeout_ms) * 1_000_000).unwrap_or(i64::MAX);
    let deadline = RequestDeadline::after(duration_ns)
        .earlier(RequestDeadline::from_raw(inherited_deadline_ns));
    // SAFETY: caller guarantees `target` is null or readable.
    let setup = match unsafe { target.as_ref() }.map(|target| Location::try_from(*target)) {
        Some(Ok(target)) => setup_remote_ask(
            target,
            dispatch,
            msg_type,
            request,
            request_size,
            owned,
            deadline,
            grant,
        ),
        _ => RemoteAskSetupResult::Error(AskError::StaleRef),
    };
    let route = match setup {
        RemoteAskSetupResult::Ok((pending, target, publication)) => {
            RemoteCallRoute::Wire(pending, target, publication)
        }
        RemoteAskSetupResult::Error(error) => RemoteCallRoute::Failed(error),
        RemoteAskSetupResult::Local(actor_id) => {
            // SAFETY: forwards the request and waker contracts.
            return unsafe {
                start_local_call(
                    actor_id,
                    dispatch,
                    msg_type,
                    request,
                    request_size,
                    reply_size,
                    deadline.raw(),
                    waker,
                )
            };
        }
    };
    // The peer holds the encoded bytes; the fields are released here.
    // SAFETY: the caller transferred the request's fields.
    unsafe { release_request(dispatch, msg_type, request) };
    Box::into_raw(Box::new(HewRemoteCall {
        route,
        // SAFETY: the descriptor is live; the timer retains its own reference.
        timer: unsafe { crate::coro_sleep::hew_coro_sleep_until_new(deadline.raw(), waker) },
        timed_out: false,
        deadline,
        dispatch,
        msg_type,
        reply_size,
    }))
}

/// Start the call to an actor this node owns on the local call path, with the
/// admission a peer gives an inbound ask: a full mailbox refuses the request
/// and a missing actor has stopped. The local call carries the deadline.
///
/// # Safety
/// As [`hew_remote_call_new`].
#[expect(
    clippy::too_many_arguments,
    reason = "forwards the complete remote call contract"
)]
unsafe fn start_local_call(
    actor_id: u64,
    dispatch: *const c_void,
    msg_type: i32,
    request: *mut c_void,
    request_size: usize,
    reply_size: usize,
    deadline_ns: i64,
    waker: *const crate::wake::HewWaker,
) -> *mut HewRemoteCall {
    let operation = |route| {
        Box::into_raw(Box::new(HewRemoteCall {
            route,
            timer: ptr::null_mut(),
            timed_out: false,
            deadline: RequestDeadline::from_raw(deadline_ns),
            dispatch,
            msg_type,
            reply_size,
        }))
    };
    // A name lookup alone does not prove the caller's concrete payload layout.
    let target_dispatch = crate::lifetime::live_actors::dispatch_ptr_by_id(actor_id);
    if !target_dispatch
        .is_some_and(|target| super::local_request_matches(dispatch, target, msg_type))
    {
        // SAFETY: the fields have not moved into the target actor.
        unsafe { release_request(dispatch, msg_type, request) };
        let error = if target_dispatch.is_some() {
            AskError::DecodeFailure
        } else {
            AskError::ActorStopped
        };
        return operation(RemoteCallRoute::Failed(error));
    }
    let reply_drop = crate::xnode_serial::lookup_reply(dispatch, msg_type).map(|codec| codec.drop);
    // SAFETY: the request is the member's wrapper (caller contract).
    let adopted = crate::xnode_serial::lookup_request(dispatch, msg_type)
        .and_then(|codec| unsafe { InboundRequest::adopt(codec, request, request_size) });
    let Some(adopted) = adopted else {
        // SAFETY: the fields did not move.
        unsafe { release_request(dispatch, msg_type, request) };
        return operation(RemoteCallRoute::Failed(AskError::DecodeFailure));
    };
    let Some(reply_drop) = reply_drop else {
        return operation(RemoteCallRoute::Failed(AskError::DecodeFailure));
    };
    let request = adopted;
    let Some((_runtime, token)) = InboundRequest::target(actor_id) else {
        return operation(RemoteCallRoute::Failed(AskError::ActorStopped));
    };
    let (wrapper, size, drop) = (request.wrapper, request.size, request.drop);
    std::mem::forget(request);
    // SAFETY: the call owns the wrapper and its fields on every outcome, and
    // the waker descriptor is live.
    let call = unsafe {
        crate::actor_call_native::hew_actor_call_new(
            token,
            msg_type,
            wrapper,
            size,
            drop,
            reply_size,
            reply_drop,
            waker,
            deadline_ns,
            1,
            None,
            None,
            &crate::actor_caller::local_sender(),
            dispatch,
        )
    };
    operation(RemoteCallRoute::Local(call))
}

/// Observe a remote call without taking its outcome: `-1` while the reply is
/// outstanding and the deadline has not passed, `0` once it can be taken.
///
/// # Safety
/// `operation` is a live call from [`hew_remote_call_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_remote_call_poll(operation: *mut HewRemoteCall) -> i32 {
    // SAFETY: the caller exclusively drives this live operation.
    if unsafe { &mut *operation }.ready() {
        0
    } else {
        -1
    }
}

/// Take a ready call's outcome. Zero decodes the reply into `output`, which
/// has the member's reply size; any other value is the [`AskError`] tag.
///
/// # Safety
/// `operation` is a ready call taken once; `output` is writable for the reply.
#[no_mangle]
pub unsafe extern "C" fn hew_remote_call_take(
    operation: *mut HewRemoteCall,
    output: *mut c_void,
) -> i32 {
    // SAFETY: the caller exclusively drives this live operation.
    let operation = unsafe { &mut *operation };
    if matches!(
        operation.route,
        RemoteCallRoute::Offer(..) | RemoteCallRoute::Import(..)
    ) {
        // SAFETY: the acquisition uses the same ready-once operation contract.
        return unsafe { super::service_api::take_acquisition(operation, output) };
    }
    let pending = match &operation.route {
        RemoteCallRoute::Wire(..) if operation.timed_out => return AskError::Timeout as i32,
        RemoteCallRoute::Wire(pending, _, _) => pending,
        // SAFETY: the local call is ready and `output` holds its reply.
        RemoteCallRoute::Local(call) => return unsafe { take_local_call(*call, output) },
        RemoteCallRoute::Failed(error) => return *error as i32,
        RemoteCallRoute::Offer(..) | RemoteCallRoute::Import(..) => unreachable!(),
    };
    let Some(reply) = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .take()
    else {
        return AskError::ConnectionDropped as i32;
    };
    match remote_reply_value(
        operation.dispatch,
        operation.msg_type,
        operation.reply_size,
        &reply,
    ) {
        Ok(value) => {
            if operation.reply_size > 0 {
                // SAFETY: the decoded value and output both hold reply_size
                // bytes; the value's owners move into output.
                unsafe {
                    ptr::copy_nonoverlapping(
                        value.cast::<u8>(),
                        output.cast::<u8>(),
                        operation.reply_size,
                    );
                    crate::mem::buf_free(value);
                }
            }
            AskError::None as i32
        }
        Err(error) => error as i32,
    }
}

/// Take a ready local call's reply into `output`. A refused request comes back
/// as its envelope, whose release drops its fields.
///
/// # Safety
/// `call` is ready and untaken; `output` is writable for its reply.
unsafe fn take_local_call(
    call: *mut crate::actor_call_native::HewActorCall,
    output: *mut c_void,
) -> i32 {
    // SAFETY: the caller exclusively drives the ready call.
    let status = unsafe { crate::actor_call_native::hew_actor_call_poll(call) };
    if status < 0 {
        return AskError::ActorStopped as i32;
    }
    let output = if output.is_null() {
        ptr::NonNull::<u8>::dangling().as_ptr().cast()
    } else {
        output
    };
    let mut rejected = ptr::null_mut();
    // SAFETY: the call is ready and untaken; the output holds its reply.
    let status =
        unsafe { crate::actor_call_native::hew_actor_call_take(call, output, &raw mut rejected) };
    if !rejected.is_null() {
        // SAFETY: the refused envelope is released once with its fields.
        unsafe { crate::mailbox::hew_msg_envelope_release(rejected) };
    }
    status
}

/// Drain the owners a completed or cancelled local call abandoned; a call a
/// peer answers has none. As [`crate::actor_call_native::hew_actor_call_cleanup_poll`].
///
/// # Safety
/// The operation and parent invocation remain live until this returns complete.
#[no_mangle]
pub unsafe extern "C" fn hew_remote_call_cleanup_poll(
    operation: *mut HewRemoteCall,
    parent: *mut crate::coro_state::HewCoroState,
) -> i32 {
    // SAFETY: generated cleanup exclusively owns this live operation.
    match unsafe { &(*operation).route } {
        // SAFETY: forwards the cleanup contract to the local call.
        RemoteCallRoute::Local(call) => unsafe {
            crate::actor_call_native::hew_actor_call_cleanup_poll(*call, parent)
        },
        RemoteCallRoute::Wire(..)
        | RemoteCallRoute::Offer(..)
        | RemoteCallRoute::Import(..)
        | RemoteCallRoute::Failed(_) => 1,
    }
}

/// Transfer the completed cleanup diagnostic, if any.
///
/// # Safety
/// The operation's cleanup has completed and this fault is taken at most once.
#[no_mangle]
pub unsafe extern "C" fn hew_remote_call_cleanup_fault(
    operation: *mut HewRemoteCall,
) -> *mut crate::fault::HewFault {
    // SAFETY: generated cleanup exclusively owns this live operation.
    match unsafe { &(*operation).route } {
        // SAFETY: forwards the cleanup contract to the local call.
        RemoteCallRoute::Local(call) => unsafe {
            crate::actor_call_native::hew_actor_call_cleanup_fault(*call)
        },
        RemoteCallRoute::Wire(..)
        | RemoteCallRoute::Offer(..)
        | RemoteCallRoute::Import(..)
        | RemoteCallRoute::Failed(_) => ptr::null_mut(),
    }
}

/// Release a call, withdrawing its registration so a late reply is dropped.
///
/// # Safety
/// `operation` is null or a live call released once.
#[no_mangle]
pub unsafe extern "C" fn hew_remote_call_free(operation: *mut HewRemoteCall) {
    if !operation.is_null() {
        // SAFETY: the caller transfers its one operation owner.
        drop(unsafe { Box::from_raw(operation) });
    }
}
