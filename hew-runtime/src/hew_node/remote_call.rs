//! Remote actor call operations.

use super::{
    quarantine_blocks_send, remote_reply_data_to_ptr, reply_table, with_current_node_read,
    AskError, ConnectionKey, HewNode, PendingReply, PendingReplyKind, ReplyOutcome, ReplyStatus,
    NODE_STATE_RUNNING,
};
use crate::connection;
use crate::envelope::encode_envelope_frame_from_raw_parts;
use crate::node_identity::Location;
use crate::request_deadline::RequestDeadline;
use crate::routing;
use crate::set_last_error;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::Ordering;
use std::sync::Arc;

enum RemoteAskSetupResult {
    Ok((Arc<PendingReply>, Location, u64)),
    Error(AskError),
}

/// Set up an outbound remote ask: serialize the request, register a pending
/// reply slot woken through `waker` and send the ask envelope over the mesh.
/// Returns the pending reply and exact route on successful submission, or a typed
/// [`AskError`] on any setup failure.
///
/// # Safety
///
/// - `target` and `grant` identify an authenticated recipient-bound actor grant.
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
    grant: u64,
    kind: crate::request_state::RequestKind,
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
            routing::LocationRoute::Local { .. } => {
                return RemoteAskSetupResult::Error(AskError::Unauthorized);
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
                    table.refuse(request_id, AskError::Timeout);
                    return None;
                }
                // SAFETY: the job owns the serialized request through preparation.
                match unsafe {
                    encode_envelope_frame_from_raw_parts(
                        Some(target),
                        source,
                        crate::envelope::EnvelopeMessage::Service {
                            handler: codec.identity.to_owned(),
                            grant_id: grant,
                            kind,
                        },
                        request.as_ptr(),
                        request.len(),
                        request_id,
                        remaining.map(i64::cast_unsigned),
                    )
                } {
                    Ok(bytes) => Some(bytes),
                    Err(error) => {
                        set_last_error(format!("granted actor call: {error}"));
                        table.refuse(request_id, AskError::EncodeFailed);
                        None
                    }
                }
            },
            move |result| {
                match result {
                    Err(connection::ChannelError::FrameTooLarge { .. }) => {
                        // The channel proved nonpublication before writes or nonce use.
                        completion_table.refuse(request_id, AskError::MessageTooLarge);
                    }
                    Ok(()) | Err(connection::ChannelError::Preparation) => {}
                    Err(_) => completion_table.fail_connection(connection),
                }
            },
        );
        if !submitted {
            reply_table().refuse(request_id, AskError::SendFailed);
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
    if reply.status != ReplyStatus::Success {
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
    Wire(
        Arc<PendingReply>,
        Location,
        u64,
        crate::request_state::RequestKind,
    ),
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
            RemoteCallRoute::Wire(pending, _, _, _)
            | RemoteCallRoute::Offer(pending, _)
            | RemoteCallRoute::Import(pending, _) => pending,
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
    kind: crate::request_state::RequestKind,
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
                grant,
                kind,
            )
        }
        Err(error) => RemoteAskSetupResult::Error(error),
    };
    let route = match setup {
        RemoteAskSetupResult::Ok((pending, target, publication)) => {
            RemoteCallRoute::Wire(pending, target, publication, kind)
        }
        RemoteAskSetupResult::Error(error) => RemoteCallRoute::Failed(error),
    };
    Box::into_raw(Box::new(HewRemoteCall {
        route,
        timer: if deadline_ns >= 0 {
            // SAFETY: the deadline operation retains the caller's live waker.
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
            RemoteCallRoute::Wire(pending, _, _, _)
            | RemoteCallRoute::Offer(pending, _)
            | RemoteCallRoute::Import(pending, _) => {
                if reply_table().remove(pending.request_id).is_some() {
                    withdraw_remote(&self.route);
                }
            }
            RemoteCallRoute::Failed(_) => {}
        }
        // SAFETY: the operation uniquely owns its timer, released once here.
        unsafe { crate::coro_sleep::hew_coro_sleep_free(self.timer) };
    }
}

/// Best-effort withdrawal is bound to the same authenticated connection publication.
fn withdraw_remote(route: &RemoteCallRoute) {
    let RemoteCallRoute::Wire(pending, target, publication, _) = route else {
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

/// Observe a remote call without taking its outcome: `-1` while the reply is
/// outstanding and the deadline has not passed, `0` once it can be taken.
///
/// # Safety
/// `operation` is a live call from [`HewRemoteCall`].
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
        RemoteCallRoute::Wire(pending, _, _, _) => pending,
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
    // SAFETY: the ready operation and output retain the declared reply layout.
    unsafe { write_remote_reply(operation, &reply, output) }
}

unsafe fn write_remote_reply(
    operation: &HewRemoteCall,
    reply: &ReplyOutcome,
    output: *mut c_void,
) -> i32 {
    match remote_reply_value(
        operation.dispatch,
        operation.msg_type,
        operation.reply_size,
        reply,
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

/// Consume an imported request's ready outcome, retaining non-admission proof.
/// # Safety
/// The operation is a ready imported request taken once; output holds its reply.
pub(crate) unsafe fn take_imported_request(
    operation: *mut HewRemoteCall,
    output: *mut c_void,
) -> crate::request_state::RequestOutcome {
    use crate::request_state::{RequestKind, RequestOutcome};
    // SAFETY: the caller exclusively owns this ready operation.
    let operation = unsafe { &mut *operation };
    let (pending, kind) = match &operation.route {
        RemoteCallRoute::Failed(error) => return RequestOutcome::Refused(*error),
        RemoteCallRoute::Wire(..) if operation.timed_out => {
            return RequestOutcome::Completed(AskError::Timeout as i32)
        }
        RemoteCallRoute::Wire(pending, _, _, kind) => (pending, *kind),
        _ => return RequestOutcome::Completed(AskError::DecodeFailure as i32),
    };
    let Some(reply) = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .take()
    else {
        return RequestOutcome::Completed(AskError::ConnectionDropped as i32);
    };
    match reply.status {
        ReplyStatus::Refused => RequestOutcome::Refused(reply.ask_error),
        ReplyStatus::Failed => RequestOutcome::Completed(reply.ask_error as i32),
        ReplyStatus::Success => match kind {
            RequestKind::CallWait | RequestKind::CallReject => {
                // SAFETY: the original checked completion protocol owns this output.
                RequestOutcome::Completed(unsafe { write_remote_reply(operation, &reply, output) })
            }
            RequestKind::SendWait | RequestKind::SendReject | RequestKind::SendDropNewest => {
                match reply.data.as_slice() {
                    [0] => RequestOutcome::Submitted { discarded: false },
                    [1] => RequestOutcome::Submitted { discarded: true },
                    _ => RequestOutcome::Completed(AskError::DecodeFailure as i32),
                }
            }
        },
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
