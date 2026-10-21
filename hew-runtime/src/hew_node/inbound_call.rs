//! Node-owned inbound asks driven by ordinary actor-call readiness.

use super::inbound::{
    send_refusal_reply, send_rejection_reply, send_reply_envelope, InboundAskGuard,
};
use super::{AskError, InboundRequest, SendConnMgr};
use crate::actor_call_native::{hew_actor_call_free, hew_actor_call_poll, HewActorCall};
use crate::actor_caller::HewCallerStamp;
use crate::lifetime::live_actors::ActorIncarnation;
use crate::lifetime::local_handles::HewLocalPidId;
use crate::util::MutexExt;
use crate::wake::scheduled::{enqueue, ReadyState, ReadyWork};
use crate::wake::HewWaker;
use crate::xnode_serial::ThunkPair;
use std::collections::HashMap;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, Weak};

const INBOUND_ASK_TARGET_LIMIT: usize = 64;

#[derive(Default)]
pub(crate) struct InboundAsks {
    targets: Mutex<HashMap<ActorIncarnation, Vec<Arc<InboundAsk>>>>,
}

impl std::fmt::Debug for InboundAsks {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InboundAsks").finish_non_exhaustive()
    }
}

impl InboundAsks {
    pub(super) fn admit(
        self: &Arc<Self>,
        target: ActorIncarnation,
        call: PendingCall,
        closed: &AtomicBool,
    ) -> Result<(), AskError> {
        let mut targets = self.targets.lock_or_recover();
        if closed.load(Ordering::SeqCst) {
            return Err(AskError::LocalShutdown);
        }
        let calls = targets.entry(target).or_default();
        if calls.len() >= INBOUND_ASK_TARGET_LIMIT {
            return Err(AskError::WorkerAtCapacity);
        }
        let work = Arc::new(InboundAsk {
            phase: ReadyState::queued(),
            call: Mutex::new(Some(call)),
            owner: Arc::downgrade(self),
            target,
        });
        calls.push(Arc::clone(&work));
        drop(targets);
        enqueue(work);
        Ok(())
    }

    fn retire(&self, target: ActorIncarnation, work: &InboundAsk) {
        let mut targets = self.targets.lock_or_recover();
        if let Some(calls) = targets.get_mut(&target) {
            if let Some(index) = calls
                .iter()
                .position(|call| ptr::eq(Arc::as_ptr(call), work))
            {
                calls.swap_remove(index);
            }
            if calls.is_empty() {
                targets.remove(&target);
            }
        }
    }

    /// Only the authenticated connection publication that submitted an ask may retract it.
    pub(crate) fn withdraw(
        &self,
        destination: crate::node_identity::Location,
        publication: u64,
        request_id: u64,
    ) {
        let mut targets = self.targets.lock_or_recover();
        let selected = targets.iter().find_map(|(target, calls)| {
            calls
                .iter()
                .position(|work| {
                    let call = work.call.lock_or_recover();
                    call.as_ref().is_some_and(|call| {
                        call.publication == publication
                            && call.request_id == request_id
                            && call
                                .admission
                                .as_ref()
                                .is_some_and(|admission| admission.location == destination)
                    })
                })
                .map(|index| (*target, index))
        });
        let Some((target, index)) = selected else {
            return;
        };
        let calls = targets.get_mut(&target).expect("selected admitted request");
        let work = calls.swap_remove(index);
        if calls.is_empty() {
            targets.remove(&target);
        }
        drop(targets);
        let pending = work.call.lock_or_recover().take();
        drop(pending);
    }

    /// Admission is closed before this is called. Cleanup does not need a worker.
    pub(crate) fn cancel_all(&self) {
        let targets = std::mem::take(&mut *self.targets.lock_or_recover());
        for call in targets.into_values().flatten() {
            let pending = call.call.lock_or_recover().take();
            drop(pending);
        }
    }
}

struct InboundAsk {
    phase: ReadyState,
    call: Mutex<Option<PendingCall>>,
    owner: Weak<InboundAsks>,
    target: ActorIncarnation,
}

impl InboundAsk {
    fn notify(self: &Arc<Self>) {
        if self.phase.notify() {
            enqueue(self.clone());
        }
    }
}

impl ReadyWork for InboundAsk {
    fn poll(self: Arc<Self>) {
        self.phase.begin();
        let mut call = self.call.lock_or_recover();
        let complete = call.as_mut().is_none_or(|call| {
            let waker = HewWaker {
                context: Arc::as_ptr(&self).cast_mut().cast(),
                wake,
                retain,
                release,
            };
            call.poll(&waker)
        });
        if complete {
            // Keep publication and cancellation serialized until the reply is sent.
            call.take();
        }
        drop(call);
        let requeue = self.phase.finish(complete);
        if complete {
            if let Some(owner) = self.owner.upgrade() {
                owner.retire(self.target, &self);
            }
        } else if requeue {
            enqueue(self.clone());
        }
    }
}

enum PendingState {
    Input(Option<InboundRequest>),
    Active(*mut HewActorCall),
    Queued {
        request: Arc<crate::request_state::RequestState>,
        timer: *mut crate::coro_sleep::HewCoroSleep,
    },
    Done,
}

impl Drop for PendingState {
    fn drop(&mut self) {
        // SAFETY: the pending owner exclusively owns each operation and timer.
        unsafe {
            match self {
                Self::Active(operation) => hew_actor_call_free(*operation),
                Self::Queued { request, timer } => {
                    request.withdraw();
                    crate::coro_sleep::hew_coro_sleep_free(*timer);
                }
                Self::Input(_) | Self::Done => {}
            }
        }
    }
}

pub(super) struct PendingCall {
    token: HewLocalPidId,
    kind: crate::request_state::RequestKind,
    message: i32,
    state: PendingState,
    reply: Option<ThunkPair>,
    caller: HewCallerStamp,
    source_node: u16,
    request_id: u64,
    publication: u64,
    deadline: crate::request_deadline::RequestDeadline,
    admission: Option<super::service::RemoteAdmission>,
    manager: SendConnMgr,
    shutdown: Arc<AtomicBool>,
    _guard: InboundAskGuard,
}

// SAFETY: the decoded request is transferable wire data. The mutex serializes
// the operation, and the shutdown barrier protects every manager access.
unsafe impl Send for PendingCall {}

impl PendingCall {
    #[expect(
        clippy::too_many_arguments,
        reason = "one owned request and its reply route"
    )]
    pub(super) fn new(
        token: HewLocalPidId,
        message: i32,
        request: InboundRequest,
        reply: Option<ThunkPair>,
        caller: HewCallerStamp,
        source_node: u16,
        request_id: u64,
        manager: SendConnMgr,
        shutdown: Arc<AtomicBool>,
        guard: InboundAskGuard,
        publication: u64,
        deadline: crate::request_deadline::RequestDeadline,
        admission: super::service::RemoteAdmission,
        kind: crate::request_state::RequestKind,
    ) -> Self {
        Self {
            token,
            kind,
            message,
            state: PendingState::Input(Some(request)),
            reply,
            caller,
            source_node,
            request_id,
            publication,
            deadline,
            admission: Some(admission),
            manager,
            shutdown,
            _guard: guard,
        }
    }

    fn take_submission(&mut self, operation: *mut HewActorCall) -> bool {
        // SAFETY: ready admission is exclusively owned; mailbox sends have no reply payload.
        let outcome = unsafe { (*operation).take_outcome(ptr::null_mut()) };
        match outcome {
            crate::request_state::RequestOutcome::Submitted { discarded } => {
                let queued = if discarded {
                    PendingState::Done
                } else {
                    // SAFETY: the taken local admission transfers its timer once.
                    let (request, timer) = unsafe { (*operation).take_queued() };
                    PendingState::Queued { request, timer }
                };
                self.state = queued;
                send_reply_envelope(
                    self.source_node,
                    self.request_id,
                    self.publication,
                    &[u8::from(discarded)],
                    self.manager.0,
                    self.shutdown.as_ref(),
                );
                return discarded;
            }
            crate::request_state::RequestOutcome::Refused(reason) => {
                send_refusal_reply(
                    self.source_node,
                    self.request_id,
                    self.publication,
                    reason,
                    self.manager.0,
                    self.shutdown.as_ref(),
                );
            }
            crate::request_state::RequestOutcome::Completed(code) => {
                let reason = AskError::ALL
                    .iter()
                    .copied()
                    .find(|error| *error as i32 == code)
                    .unwrap_or(AskError::SendFailed);
                send_rejection_reply(
                    self.source_node,
                    self.request_id,
                    self.publication,
                    reason,
                    self.manager.0,
                    self.shutdown.as_ref(),
                );
            }
        }
        true
    }

    fn poll(&mut self, waker: &HewWaker) -> bool {
        if self.shutdown.load(Ordering::Acquire) {
            return true;
        }
        let _remote_trace_source = crate::tracing::enter_remote_trace_source();
        if let PendingState::Queued { request, timer } = &self.state {
            if request.deadline.expired()
                // SAFETY: this pending publication retains the moved timer.
                || (!timer.is_null() && unsafe { crate::coro_sleep::hew_coro_sleep_status(*timer) } > 0)
            {
                request.withdraw();
            }
            return request.terminal();
        }
        if let PendingState::Input(request) = &mut self.state {
            let request = request.take().expect("unpublished request");
            self.state = PendingState::Active(
                request.ask_start(
                    self.token,
                    self.message,
                    self.reply,
                    self.caller,
                    waker,
                    self.deadline,
                    self.admission
                        .take()
                        .expect("unpublished authenticated request"),
                    self.kind,
                ),
            );
        }
        let PendingState::Active(operation) = self.state else {
            return true;
        };
        // SAFETY: the mutex gives this poll exclusive access to the operation.
        let status = unsafe { hew_actor_call_poll(operation) };
        if status == -1 {
            return false;
        }
        if self.kind.completion_policy().is_none() {
            return self.take_submission(operation);
        }
        // ask_take owns and frees the active call; disarm this state's destructor.
        if let PendingState::Active(active) = &mut self.state {
            *active = ptr::null_mut();
        }
        self.state = PendingState::Done;
        // SAFETY: the ready operation remains exclusively owned until take below.
        let refused = unsafe { (*operation).unaccepted() };
        // SAFETY: this ready operation is uniquely owned and consumed here.
        match unsafe {
            InboundRequest::ask_take(
                operation,
                self.reply.expect("completion reply codec"),
                status,
            )
        } {
            Ok(reply) => send_reply_envelope(
                self.source_node,
                self.request_id,
                self.publication,
                &reply,
                self.manager.0,
                self.shutdown.as_ref(),
            ),
            Err(reason) => (if refused {
                send_refusal_reply
            } else {
                send_rejection_reply
            })(
                self.source_node,
                self.request_id,
                self.publication,
                reason,
                self.manager.0,
                self.shutdown.as_ref(),
            ),
        }
        true
    }
}

unsafe extern "C" fn wake(context: *mut c_void) {
    // SAFETY: the descriptor retains this Arc throughout notification.
    let work = std::mem::ManuallyDrop::new(unsafe { Arc::from_raw(context.cast::<InboundAsk>()) });
    work.notify();
}

unsafe extern "C" fn retain(context: *mut c_void) {
    // SAFETY: one live descriptor reference protects this allocation.
    unsafe { Arc::increment_strong_count(context.cast::<InboundAsk>()) };
}

unsafe extern "C" fn release(context: *mut c_void) {
    // SAFETY: consume exactly one retained descriptor reference.
    unsafe { Arc::decrement_strong_count(context.cast::<InboundAsk>()) };
}
