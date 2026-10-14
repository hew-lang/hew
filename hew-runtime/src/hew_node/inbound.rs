//! Inbound frame routing and retained ask completion.

use super::{
    submission_delivered, with_current_node_read, AskError, AskRejectionReasonCode, HewNode,
    InboundRequest, SendConnMgr, SendTransport,
};
use crate::connection;
use crate::envelope::encode_envelope_frame_from_raw_parts;
use crate::set_last_error;
use crate::transport::HEW_CONN_INVALID;
#[cfg(test)]
use crate::util::CondvarExt;
use crate::util::MutexExt;
use std::ffi::c_void;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
#[cfg(test)]
use std::sync::{Condvar, Mutex};
use std::thread;

/// One admitted observation in this manager's shutdown drain.
pub(super) struct InboundAskGuard(pub(super) Arc<AtomicUsize>);

impl Drop for InboundAskGuard {
    fn drop(&mut self) {
        self.0.fetch_sub(1, Ordering::SeqCst);
    }
}

#[cfg(test)]
#[derive(Default)]
enum TestGateMode {
    #[default]
    Disabled,
    Notify,
    Blocked,
    Released,
}

#[cfg(test)]
#[derive(Default)]
struct TestGateState {
    mode: TestGateMode,
    entered: bool,
}

#[cfg(test)]
#[derive(Default)]
pub(super) struct TestGate {
    state: Mutex<TestGateState>,
    cond: Condvar,
}

#[cfg(test)]
impl TestGate {
    pub(super) fn arm(&self, block_on_enter: bool) {
        let mut state = self.state.lock_or_recover();
        *state = TestGateState {
            mode: if block_on_enter {
                TestGateMode::Blocked
            } else {
                TestGateMode::Notify
            },
            entered: false,
        };
        self.cond.notify_all();
    }

    pub(super) fn hit(&self) {
        let mut state = self.state.lock_or_recover();
        if matches!(state.mode, TestGateMode::Disabled) {
            return;
        }
        state.entered = true;
        self.cond.notify_all();
        while matches!(state.mode, TestGateMode::Blocked) {
            state = self.cond.wait_or_recover(state);
        }
    }

    pub(super) fn release(&self) {
        let mut state = self.state.lock_or_recover();
        if matches!(state.mode, TestGateMode::Disabled) {
            return;
        }
        state.mode = TestGateMode::Released;
        self.cond.notify_all();
    }

    pub(super) fn wait_for_enter(&self, timeout: std::time::Duration) -> bool {
        let deadline = std::time::Instant::now() + timeout;
        let mut state = self.state.lock_or_recover();
        while !matches!(state.mode, TestGateMode::Disabled) && !state.entered {
            let remaining = deadline.saturating_duration_since(std::time::Instant::now());
            if remaining.is_zero() {
                return false;
            }
            let (next, wait_result) = self.cond.wait_timeout_or_recover(state, remaining);
            state = next;
            if wait_result.timed_out() && !state.entered {
                return false;
            }
        }
        state.entered
    }

    /// Block until the gate is hit and report whether it was; the test
    /// runner's timeout is the hang guard.
    pub(super) fn wait_until_entered(&self) -> bool {
        let mut state = self.state.lock_or_recover();
        while !matches!(state.mode, TestGateMode::Disabled) && !state.entered {
            state = self.cond.wait_or_recover(state);
        }
        state.entered
    }

    pub(super) fn reset(&self) {
        let mut state = self.state.lock_or_recover();
        *state = TestGateState::default();
        self.cond.notify_all();
    }
}

#[cfg(test)]
pub(super) static INBOUND_ASK_ERROR_REPLY_HOOK: std::sync::LazyLock<TestGate> =
    std::sync::LazyLock::new(TestGate::default);
#[cfg(test)]
pub(super) static NODE_STOP_BEFORE_CONNMGR_FREE_HOOK: std::sync::LazyLock<TestGate> =
    std::sync::LazyLock::new(TestGate::default);
/// Fires in `node_inbound_router` AFTER the per-manager counter increment and
/// BEFORE the spawn-gate re-check. Lets a test wedge a router exactly in the
/// Dekker window: counter already incremented, gate not yet re-read. A
/// concurrent `hew_node_stop` drain that closes the gate and then loads the
/// counter MUST observe this worker (count ≥ 1) and wait — proving the atomic
/// gate/counter protocol.
#[cfg(test)]
pub(super) static INBOUND_ROUTER_AFTER_INCREMENT_HOOK: std::sync::LazyLock<TestGate> =
    std::sync::LazyLock::new(TestGate::default);
/// Parks a worker before reply admission so shutdown can invalidate its manager.
#[cfg(test)]
pub(super) static INBOUND_ASK_REPLY_ADMISSION_HOOK: std::sync::LazyLock<TestGate> =
    std::sync::LazyLock::new(TestGate::default);

pub(super) unsafe extern "C" fn node_inbound_router(
    target_actor_id: u64,
    identity: *const std::ffi::c_char,
    data: *mut u8,
    size: usize,
    request_id: u64,
    source_node_id: u16,
    conn_mgr: *mut connection::HewConnMgr,
    peer: crate::node_identity::HewNodeId,
    publication: u64,
    deadline_ns: i64,
) {
    // SAFETY: the reader supplies a NUL-terminated UTF-8 declaration for this call.
    let identity = unsafe { std::ffi::CStr::from_ptr(identity) }
        .to_str()
        .expect("decoded wire identity");
    let caller = crate::actor_caller::HewCallerStamp::peer(peer);
    if request_id > 0 && source_node_id > 0 {
        // SAFETY: the reader pins its manager and frame throughout this call.
        let Some(shutdown) = (unsafe { connection::hew_connmgr_shutdown_flag(conn_mgr) }) else {
            return;
        };
        let payload = if size > 0 && !data.is_null() {
            // SAFETY: the reader lends the complete frame until routing returns.
            unsafe { std::slice::from_raw_parts(data, size) }
        } else {
            &[]
        };
        let _remote_trace_source = crate::tracing::enter_remote_trace_source();
        handle_inbound_ask(
            target_actor_id,
            identity,
            payload,
            request_id,
            source_node_id,
            SendConnMgr(conn_mgr),
            shutdown,
            caller,
            publication,
            crate::request_deadline::RequestDeadline::from_raw(deadline_ns),
        );
    } else {
        // Fire-and-forget message — reconstruct the value into THIS node's
        // address space before delivering it to the local mailbox.
        let _remote_trace_source = crate::tracing::enter_remote_trace_source();
        // SAFETY: data is valid for `size` bytes (reader_loop contract).
        unsafe { deliver_inbound_send(target_actor_id, identity, data, size, caller) };
    }
}

/// Deliver an inbound fire-and-forget (`send`) frame to its target actor's local
/// mailbox, reconstructing the value into THIS node's address space first. The
/// inbound `data` is the serialized wire form; feeding it raw to the mailbox
/// would make the actor handler dereference sender-side heap pointers and crash.
///
/// Fail closed throughout: an unregistered codec, a decode failure, or a
/// target actor that is no longer live all DROP the message rather than deliver
/// garbage. An empty payload (`size == 0`) is a genuine zero-field message and
/// bypasses decode.
///
/// # Safety
/// `data` must be valid for `size` bytes (or null when `size == 0`).
unsafe fn deliver_inbound_send(
    target_actor_id: u64,
    identity: &str,
    data: *mut u8,
    size: usize,
    caller: crate::actor_caller::HewCallerStamp,
) {
    let Some(msg_type) = crate::lifetime::live_actors::dispatch_ptr_by_id(target_actor_id)
        .and_then(|dispatch| crate::xnode_serial::local_message(dispatch, identity))
    else {
        set_last_error(format!(
            "cross-node send refused unknown handler {identity}"
        ));
        return;
    };
    let codec = crate::lifetime::live_actors::dispatch_ptr_by_id(target_actor_id)
        .and_then(|dispatch| crate::xnode_serial::lookup_request(dispatch, msg_type));
    let payload = if size == 0 {
        &[][..]
    } else {
        // SAFETY: data is valid for `size` bytes (caller contract).
        unsafe { std::slice::from_raw_parts(data.cast_const(), size) }
    };
    // SAFETY: a registered codec decodes into a fresh owned wrapper.
    let request = codec.and_then(|codec| unsafe { InboundRequest::decode(codec, payload) });
    let Some(request) = request else {
        set_last_error(format!(
            "cross-node send dropped: target actor {target_actor_id} has no codec \
             or the frame failed to decode for msg_type={msg_type}"
        ));
        return;
    };
    if !request
        .submit(target_actor_id, msg_type, caller)
        .is_some_and(submission_delivered)
    {
        set_last_error(format!(
            "cross-node send dropped: target actor {target_actor_id} refused msg_type={msg_type}"
        ));
    }
}

/// Decode and admit an inbound ask without waiting on a reader or OS thread.
#[expect(
    clippy::too_many_arguments,
    reason = "keeps authenticated request and caller metadata together at admission"
)]
#[expect(
    clippy::needless_pass_by_value,
    reason = "the retained reply operation owns the shutdown flag clone"
)]
pub(super) fn handle_inbound_ask(
    target_actor_id: u64,
    identity: &str,
    payload: &[u8],
    request_id: u64,
    source_node_id: u16,
    conn_mgr: SendConnMgr,
    shutdown_started: Arc<AtomicBool>,
    caller: crate::actor_caller::HewCallerStamp,
    publication: u64,
    deadline: crate::request_deadline::RequestDeadline,
) {
    // Hold a router before it acquires the manager's publication barrier.
    #[cfg(test)]
    INBOUND_ASK_REPLY_ADMISSION_HOOK.hit();

    // Clone ownership only while the reply barrier still pins the manager.
    let admission = with_current_node_read(|guard| {
        if *guard == 0 || conn_mgr.0.is_null() || shutdown_started.load(Ordering::Acquire) {
            return None;
        }
        // SAFETY: the barrier and shutdown flag keep this manager live.
        let manager = unsafe { &*conn_mgr.0 };
        Some((
            Arc::clone(&manager.inbound_asks),
            Arc::clone(&manager.inbound_ask_active),
            // SAFETY: the same barrier pins this manager for the flag clone.
            unsafe { connection::hew_connmgr_inbound_spawn_closed_flag(conn_mgr.0) }?,
        ))
    });
    let Some((pending, active, gate)) = admission else {
        return;
    };
    active.fetch_add(1, Ordering::SeqCst);
    let guard = InboundAskGuard(active);
    #[cfg(test)]
    INBOUND_ROUTER_AFTER_INCREMENT_HOOK.hit();
    if gate.load(Ordering::SeqCst) || shutdown_started.load(Ordering::SeqCst) {
        return;
    }
    let reject = |reason: AskError| {
        #[cfg(test)]
        INBOUND_ASK_ERROR_REPLY_HOOK.hit();
        send_rejection_reply(
            source_node_id,
            request_id,
            reason,
            conn_mgr.0,
            shutdown_started.as_ref(),
        );
    };
    if deadline.expired() {
        reject(AskError::Timeout);
        return;
    }
    // A dead target (never spawned, or already freed) has no dispatch pointer
    // tracked at all: that is an ActorStopped fact (folds to the public
    // `ActorError.Dead`, matching the local ask route, D526), never a decode
    // problem. Only once a live actor's dispatch is resolved does a missing
    // codec registration for `msg_type` become a genuine DecodeFailure.
    let target = crate::lifetime::live_actors::with_actor_send_by_id(target_actor_id, |actor| {
        // SAFETY: the send pin keeps the complete actor incarnation live.
        unsafe {
            Some((
                crate::lifetime::live_actors::ActorIncarnation::of(actor),
                (*actor).local_pid_id,
                (*actor).dispatch? as *const c_void,
            ))
        }
    })
    .flatten();
    let Some((target, token, dispatch)) = target else {
        reject(AskError::ActorStopped);
        return;
    };
    let Some(msg_type) = crate::xnode_serial::local_message(dispatch, identity) else {
        reject(AskError::DecodeFailure);
        return;
    };
    // The codecs are keyed by the target actor type's dispatch pointer, so a
    // frame is decoded, and its reply encoded, only by that actor's member.
    let codecs = crate::xnode_serial::lookup_request(dispatch, msg_type)
        .zip(crate::xnode_serial::lookup_reply(dispatch, msg_type));
    let Some((request_codec, reply_codec)) = codecs else {
        reject(AskError::DecodeFailure);
        return;
    };
    // SAFETY: the registered codec decodes into a fresh owned wrapper.
    let Some(request) = (unsafe { InboundRequest::decode(request_codec, payload) }) else {
        reject(AskError::DecodeFailure);
        return;
    };
    let call = super::inbound_call::PendingCall::new(
        token,
        msg_type,
        request,
        reply_codec,
        caller,
        source_node_id,
        request_id,
        conn_mgr,
        Arc::clone(&shutdown_started),
        guard,
        publication,
        deadline,
    );
    if let Err(reason) = pending.admit(target, call, gate.as_ref()) {
        if reason != AskError::LocalShutdown {
            reject(reason);
        }
    }
}

/// Send a **rejection** reply envelope back to the source node.
///
/// Rejection replies carry explicit [`crate::envelope::EnvelopeMessage::Rejected`]
/// status plus a 1-byte [`AskError`] reason payload. The connection reader on
/// the receiving node recognises the sentinel and calls
/// [`fail_remote_reply`] instead of [`complete_remote_reply`], preserving the
/// remote rejection reason for the originating `hew_node_api_ask` caller.
pub(super) fn send_rejection_reply(
    target_node_id: u16,
    request_id: u64,
    reason: AskError,
    conn_mgr: *mut connection::HewConnMgr,
    shutdown_started: &AtomicBool,
) {
    if conn_mgr.is_null() {
        return;
    }

    with_current_node_read(|guard| {
        if *guard == 0 {
            return;
        }
        if shutdown_started.load(Ordering::Acquire) {
            return;
        }

        // SAFETY: conn_mgr is the manager that received the ask. The CURRENT_NODE
        // read lock held above (guard) ensures it remains valid for this call.
        let conn_id = unsafe { connection::hew_connmgr_conn_id_for_node(conn_mgr, target_node_id) };
        if conn_id < 0 {
            return;
        }
        // SAFETY: the current-node guard pins this manager for the reply.
        let Some(send_claim) =
            (unsafe { connection::claim_send_to_route(&*conn_mgr, conn_id, target_node_id) })
        else {
            return;
        };

        let reason_payload = [AskRejectionReasonCode::encode(reason)
            .expect("remote ask rejection reason must use a supported rejection-reason code")];
        // Encode the rejection envelope: request_id identifies the pending ask;
        // explicit rejection status distinguishes it from a successful reply.
        // SAFETY: `reason_payload` is a stack byte array valid for its length.
        let bytes = match unsafe {
            encode_envelope_frame_from_raw_parts(
                None,
                None,
                crate::envelope::EnvelopeMessage::Rejected,
                reason_payload.as_ptr(),
                reason_payload.len(),
                request_id,
                None,
            )
        } {
            Ok(bytes) => bytes,
            Err(err) => {
                set_last_error(format!("send_rejection_reply: {err}"));
                return;
            }
        };
        send_claim.send(&bytes);
    });
}

/// Encode and send a reply envelope back to the source node.
///
/// Uses `conn_mgr` directly so the reply is routed via the connection that
/// received the original ask.
pub(super) fn send_reply_envelope(
    target_node_id: u16,
    request_id: u64,
    reply_data: &[u8],
    conn_mgr: *mut connection::HewConnMgr,
    shutdown_started: &AtomicBool,
) {
    if conn_mgr.is_null() {
        return;
    }

    // Synchronize with `hew_node_stop` to prevent a use-after-free on
    // `conn_mgr`.  `hew_node_stop` holds the `CURRENT_NODE` write lock while
    // it clears the pointer to zero (and only frees `conn_mgr` afterward), so:
    //
    // * If this thread acquires the read lock first, stop is blocked until we
    //   release it — `conn_mgr` is guaranteed valid for this whole function.
    // * If stop cleared `CURRENT_NODE` first, we see `*guard == 0` here and
    //   return before touching `conn_mgr`.
    //
    // The read lock is held for the entire duration of `conn_mgr` access via
    // the `read_access` closure.
    with_current_node_read(|guard| {
        if *guard == 0 {
            return;
        }
        if shutdown_started.load(Ordering::Acquire) {
            return;
        }

        // Find the connection back to the requesting node.
        // SAFETY: conn_mgr is the manager that received the ask. The CURRENT_NODE
        // read lock held above (guard) ensures it remains valid for this call.
        let conn_id = unsafe { connection::hew_connmgr_conn_id_for_node(conn_mgr, target_node_id) };
        if conn_id < 0 {
            return;
        }
        // SAFETY: the current-node guard pins this manager for the reply.
        let Some(send_claim) =
            (unsafe { connection::claim_send_to_route(&*conn_mgr, conn_id, target_node_id) })
        else {
            return;
        };

        // Encode the reply envelope with request_id set and source_node_id = 0
        // to mark it as a reply (not a new request).
        // SAFETY: `reply_data.as_ptr()` is valid for `reply_data.len()` bytes.
        let bytes = match unsafe {
            encode_envelope_frame_from_raw_parts(
                None,
                None,
                crate::envelope::EnvelopeMessage::Reply,
                reply_data.as_ptr(),
                reply_data.len(),
                request_id,
                None,
            )
        } {
            Ok(bytes) => bytes,
            Err(err) => {
                set_last_error(format!("send_reply_envelope: {err}"));
                return;
            }
        };

        send_claim.send(&bytes);
    });
}

pub(super) fn accept_loop(transport: SendTransport, conn_mgr: SendConnMgr, stop: &AtomicBool) {
    while !stop.load(Ordering::Acquire) {
        // SAFETY: pointers are valid for the lifetime of the spawned loop.
        let conn_id = unsafe {
            let t = &*transport.0;
            let Some(ops) = t.ops.as_ref() else { break };
            let Some(accept_fn) = ops.accept else { break };
            accept_fn(t.r#impl, 200)
        };

        if conn_id != HEW_CONN_INVALID {
            // SAFETY: pointers are held by HewNode for the loop lifetime.
            let _ = unsafe { connection::hew_connmgr_add(conn_mgr.0, conn_id) };
        }
    }
}

pub(super) fn actor_id_to_registry_ptr(actor_id: u64) -> *mut c_void {
    let encoded = usize::try_from(actor_id)
        .expect("u64 actor IDs fit in usize on supported targets (64-bit required)");
    encoded as *mut c_void
}

/// Drain this manager's admitted requests and reverse-link work before teardown.
/// Pending actor observations are cancelled after the existing five-second ceiling.
pub(super) fn drain_inbound_requests(inbound_active: Option<&Arc<AtomicUsize>>) {
    const MAX_DRAIN: std::time::Duration = std::time::Duration::from_secs(5);
    const POLL: std::time::Duration = std::time::Duration::from_millis(1);
    let Some(active) = inbound_active else {
        return;
    };
    let deadline = std::time::Instant::now() + MAX_DRAIN;
    // Pair with the router's increment/re-check after shutdown closes admission.
    while active.load(Ordering::SeqCst) > 0 {
        if std::time::Instant::now() >= deadline {
            break;
        }
        thread::sleep(POLL);
    }
}

pub(super) fn stop_node_accept_thread(node: &mut HewNode) {
    node.accept_stop.store(true, Ordering::Release);
    let handle = node.accept_thread.lock_or_recover().take();
    if let Some(handle) = handle {
        crate::util::report_join_panic("hew node accept thread", handle.join());
    }
}
