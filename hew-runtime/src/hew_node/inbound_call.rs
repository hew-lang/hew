//! Node-owned inbound asks driven by ordinary actor-call readiness.

use super::inbound::{send_rejection_reply, send_reply_envelope, InboundAskGuard};
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
    pub(crate) fn withdraw(&self, actor_id: u64, publication: u64, request_id: u64) {
        let mut targets = self.targets.lock_or_recover();
        let target = targets
            .keys()
            .find(|target| target.actor_id() == actor_id)
            .copied();
        let Some(target) = target else {
            return;
        };
        let calls = targets.get_mut(&target).expect("selected target");
        let index = calls.iter().position(|work| {
            let call = work.call.lock_or_recover();
            call.as_ref().is_some_and(|call| {
                call.publication == publication && call.request_id == request_id
            })
        });
        let Some(index) = index else {
            return;
        };
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

pub(super) struct PendingCall {
    token: HewLocalPidId,
    message: i32,
    request: Option<InboundRequest>,
    operation: *mut HewActorCall,
    reply: ThunkPair,
    caller: HewCallerStamp,
    source_node: u16,
    request_id: u64,
    publication: u64,
    deadline: crate::request_deadline::RequestDeadline,
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
        reply: ThunkPair,
        caller: HewCallerStamp,
        source_node: u16,
        request_id: u64,
        manager: SendConnMgr,
        shutdown: Arc<AtomicBool>,
        guard: InboundAskGuard,
        publication: u64,
        deadline: crate::request_deadline::RequestDeadline,
    ) -> Self {
        Self {
            token,
            message,
            request: Some(request),
            operation: ptr::null_mut(),
            reply,
            caller,
            source_node,
            request_id,
            publication,
            deadline,
            manager,
            shutdown,
            _guard: guard,
        }
    }

    fn poll(&mut self, waker: &HewWaker) -> bool {
        if self.shutdown.load(Ordering::Acquire) {
            return true;
        }
        let _remote_trace_source = crate::tracing::enter_remote_trace_source();
        if let Some(request) = self.request.take() {
            self.operation = request.ask_start(
                self.token,
                self.message,
                self.reply,
                self.caller,
                waker,
                self.deadline,
            );
        }
        // SAFETY: the mutex gives this poll exclusive access to the operation.
        let status = unsafe { hew_actor_call_poll(self.operation) };
        if status == -1 {
            return false;
        }
        let operation = std::mem::replace(&mut self.operation, ptr::null_mut());
        // SAFETY: this ready operation is uniquely owned and consumed here.
        match unsafe { InboundRequest::ask_take(operation, self.reply, status) } {
            Ok(reply) => send_reply_envelope(
                self.source_node,
                self.request_id,
                &reply,
                self.manager.0,
                self.shutdown.as_ref(),
            ),
            Err(reason) => send_rejection_reply(
                self.source_node,
                self.request_id,
                reason,
                self.manager.0,
                self.shutdown.as_ref(),
            ),
        }
        true
    }
}

impl Drop for PendingCall {
    fn drop(&mut self) {
        // SAFETY: cancelling this owner withdraws its request or tombstones its reply.
        unsafe { hew_actor_call_free(self.operation) };
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
