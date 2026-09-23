//! Borrowed readiness observation for source selection. A selection registers
//! its sources in arm order — a checked task, actor call or stream receive — and an
//! optional timer last. Observing readiness never consumes a source: the
//! winning branch performs the ordinary await or receive.

use super::{
    checked, hew_checked_task_wait_new, retain, HewCheckedTaskWait, HewTask, COMPLETION_ORDER,
    PENDING, TAKEN,
};
use crate::actor_call_native::{hew_actor_call_poll, HewActorCall};
use crate::actor_native::wait_graph::{
    hew_actor_wait_edge_fault, hew_actor_wait_edge_free, hew_actor_wait_edge_pending,
    hew_actor_wait_edge_prepare, select_alternatives, HewActorWaitEdge,
};
use crate::async_io::{
    hew_async_io_free, hew_async_io_status, start_tcp_readable, AsyncIoStatus, HewAsyncIo,
};
use crate::coro_sleep::{
    hew_coro_sleep_free, hew_coro_sleep_new, hew_coro_sleep_status, HewCoroSleep,
};
use crate::coro_state::CoroStatus;
use crate::lifetime::live_actors::ActorIncarnation;
use crate::stream::{HewStream, SelectReadiness};
use crate::util::MutexExt;
use crate::wake::{HewWaker, OwnedWaker};
use std::ptr;
use std::sync::atomic::Ordering;
use std::sync::Arc;

/// One registered source. Sources keep arm order, so the poll result is the
/// arm's own index and the timer's index is the source count.
#[derive(Debug)]
enum SelectSource {
    /// The surrounding SIR lifetime owns this ephemeral call, including every
    /// losing and fault edge; selection borrows only its readiness.
    ActorCall {
        operation: *mut HewActorCall,
        ready_order: Option<u64>,
    },
    /// An independently retained observation of a checked task handle.
    Task(HewCheckedTaskWait),
    /// A borrowed stream plus the completion order stamped when its readiness
    /// was first observed. The selection never consumes an element: a pipe
    /// reports its queue, a socket stream owns a readability watch, and a
    /// file stream's next read never waits.
    Stream {
        stream: *mut HewStream,
        ready_order: Option<u64>,
        waker: Arc<OwnedWaker>,
        readable: *const HewAsyncIo,
    },
}

/// Independently retained observations of the source handles and optional timer.
#[derive(Debug)]
pub struct HewCheckedTaskSelect {
    sources: Vec<SelectSource>,
    timer: *mut HewCoroSleep,
    waker: *const HewWaker,
    owner: ActorIncarnation,
    wait: *const HewActorWaitEdge,
    /// A source this selection cannot observe. The poll answers the cycle
    /// status so the selecting actor traps with this message on its own turn,
    /// instead of the process aborting under a program the checker admits.
    refusal: Option<String>,
}

impl Drop for HewCheckedTaskSelect {
    fn drop(&mut self) {
        for source in &self.sources {
            if let SelectSource::Stream { readable, .. } = source {
                // SAFETY: the selection owns each watch, which reads nothing.
                unsafe { hew_async_io_free(*readable) };
            }
        }
        // SAFETY: the selection uniquely owns this timer and all observations.
        unsafe { hew_coro_sleep_free(self.timer) };
        // SAFETY: the selection owns this optional wait registration.
        unsafe { hew_actor_wait_edge_free(self.wait) };
    }
}

/// Open a selection over the caller's waker. Sources are registered in arm
/// order, and the timer is armed last so it starts after every observation.
///
/// # Safety
/// The retained waker descriptor obeys its contract and stays live for the
/// selection. Free the returned selection exactly once.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_new(
    waker: *const HewWaker,
) -> *mut HewCheckedTaskSelect {
    Box::into_raw(Box::new(HewCheckedTaskSelect {
        sources: Vec::new(),
        timer: ptr::null_mut(),
        waker,
        owner: ActorIncarnation::NONE,
        wait: ptr::null(),
        refusal: None,
    }))
}

/// Bind an enclosing strict actor turn before any source is registered.
///
/// # Safety
/// Selection and invocation state are live, and selection has not been polled.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_set_context(
    selection: *mut HewCheckedTaskSelect,
    state: *const crate::coro_state::HewCoroState,
) {
    // SAFETY: the invocation state is borrowed only for this identity capture.
    unsafe { (*selection).owner = (*state).actor_turn };
}

/// Register a borrowed ephemeral actor completion in source order.
///
/// # Safety
/// Both handles are live and the surrounding scope retains the operation until
/// the selection is freed. No other observer consumes its reply.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_add_actor(
    selection: *mut HewCheckedTaskSelect,
    operation: *mut HewActorCall,
) {
    // SAFETY: the caller uniquely drives the live selection.
    unsafe {
        (*selection).sources.push(SelectSource::ActorCall {
            operation,
            ready_order: None,
        });
    };
}

/// Register one checked task observation without transferring its ownership.
///
/// # Safety
/// `selection` is the live handle from `hew_checked_task_select_new` and
/// `task` is a live checked handle that outlives this selection.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_add_task(
    selection: *mut HewCheckedTaskSelect,
    task: *mut HewTask,
) {
    if selection.is_null() {
        return;
    }
    // SAFETY: the observation acquires its own task reference before passing
    // that reference to the wait constructor.
    unsafe {
        let selection = &mut *selection;
        retain(task);
        let wait = *Box::from_raw(hew_checked_task_wait_new(task, selection.waker));
        selection.sources.push(SelectSource::Task(wait));
    }
}

/// Register one borrowed stream receive. Readiness is observed, never taken:
/// a pipe reports its queue, a socket stream is watched for readability, and a
/// regular file or in-memory stream is ready at once because its next read
/// never waits. A stream adapter over a socket may hold only part of its next
/// item when the socket turns readable, and a file stream over a pipe, device
/// or terminal can wait on its next read, so both refuse: the selecting actor
/// traps on its own turn and its supervisor rules on the crash.
///
/// # Safety
/// `selection` is the live handle from `hew_checked_task_select_new` and
/// `stream` is a live stream that outlives this selection.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_add_stream(
    selection: *mut HewCheckedTaskSelect,
    stream: *mut HewStream,
) {
    if selection.is_null() {
        return;
    }
    // SAFETY: the caller owns the selection for the duration of this call.
    let selection = unsafe { &mut *selection };
    let mut readable = ptr::null();
    // SAFETY: the stream is a live borrowed handle per the caller's contract.
    if let Some(stream) = unsafe { stream.as_ref() } {
        if stream.pipe_core().is_none() {
            match stream.select_readiness() {
                SelectReadiness::Ready => {}
                SelectReadiness::Socket(connection) => {
                    // SAFETY: the stream keeps its connection live through the
                    // selection; the waker descriptor is the selection's own.
                    readable = unsafe { start_tcp_readable(connection, selection.waker) };
                }
                SelectReadiness::Unsupported(reason) => {
                    selection.refusal.get_or_insert_with(|| reason.to_string());
                }
            }
        }
    }
    selection.sources.push(SelectSource::Stream {
        stream,
        ready_order: None,
        // SAFETY: the caller's retained waker descriptor is live.
        waker: Arc::new(unsafe { OwnedWaker::retain(&*selection.waker) }),
        readable,
    });
}

/// Arm the selection's timer after every source has been registered.
///
/// # Safety
/// `selection` is the live handle from `hew_checked_task_select_new` and has
/// no timer yet.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_arm_timer(
    selection: *mut HewCheckedTaskSelect,
    duration_ns: i64,
) {
    if selection.is_null() {
        return;
    }
    // SAFETY: caller provides a retained-target descriptor with the selection.
    unsafe {
        let selection = &mut *selection;
        selection.timer = hew_coro_sleep_new(duration_ns, selection.waker);
    }
}

/// Return a ready source's arm index, or the source count when the timer wins.
/// Pending is -1; an invalid consumed source or failed timer is -2.
/// When several sources are ready, arm order breaks the tie.
///
/// # Safety
/// `selection` is a live observation owned by the caller. Its tasks have no
/// concurrent consuming observer; checked source ownership establishes this.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_poll(selection: *mut HewCheckedTaskSelect) -> i64 {
    // SAFETY: caller retains the observation throughout this poll.
    unsafe { poll(selection, false) }
}

/// Select the earliest completion, even when several sources are already ready.
/// Arm order breaks an equal completion-order tie.
///
/// # Safety
/// The same retained observation contract as `hew_checked_task_select_poll`.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_poll_first(
    selection: *mut HewCheckedTaskSelect,
) -> i64 {
    // SAFETY: caller retains the observation throughout this poll.
    unsafe { poll(selection, true) }
}

#[allow(
    clippy::too_many_lines,
    reason = "one readiness sweep over every source kind; splitting it would hide the order"
)]
unsafe fn poll(selection: *mut HewCheckedTaskSelect, first_completion: bool) -> i64 {
    // SAFETY: caller retains the observation throughout this poll.
    let selection = unsafe { &mut *selection };
    if selection.refusal.is_some() {
        return -3;
    }
    if selection.wait.is_null() && !selection.owner.is_none() {
        let mut targets = Vec::new();
        let mut external_progress = !selection.timer.is_null();
        for source in &selection.sources {
            match source {
                SelectSource::ActorCall { operation, .. } => {
                    // SAFETY: the surrounding SIR lifetime retains every call.
                    let operation = unsafe { &**operation };
                    external_progress |= operation.has_deadline();
                    if let Some(target) = operation.target {
                        targets.push(target);
                    } else {
                        external_progress = true;
                    }
                }
                SelectSource::Task(_) | SelectSource::Stream { .. } => external_progress = true,
            }
        }
        selection.wait = select_alternatives(selection.owner, targets, external_progress);
    }
    // SAFETY: the selection owns its candidate; prepare precedes every predicate.
    unsafe { hew_actor_wait_edge_prepare(selection.wait) };
    let mut first = None;
    for index in 0..selection.sources.len() {
        let order = match &mut selection.sources[index] {
            SelectSource::ActorCall {
                operation,
                ready_order,
            } => {
                if let Some(order) = *ready_order {
                    order
                } else {
                    // SAFETY: readiness borrows the owned operation without taking.
                    let status = unsafe { hew_actor_call_poll(*operation) };
                    if status == -1 {
                        continue;
                    }
                    if status < 0 {
                        return -2;
                    }
                    let order = COMPLETION_ORDER.fetch_add(1, Ordering::Relaxed);
                    *ready_order = Some(order);
                    order
                }
            }
            SelectSource::Task(wait) => {
                // SAFETY: each wait independently retains the checked task storage.
                let state = unsafe { checked(wait.task) }.lock_or_recover();
                let status = state.outcome();
                if status == TAKEN {
                    return -2;
                }
                if status == PENDING {
                    continue;
                }
                state.order
            }
            SelectSource::Stream {
                stream,
                ready_order,
                waker,
                readable,
            } => {
                if let Some(order) = *ready_order {
                    order
                } else {
                    if stream.is_null() {
                        return -2;
                    }
                    // SAFETY: the stream is borrowed for the selection and
                    // the waker descriptor is live per the caller's contract.
                    let ready = match unsafe { (**stream).pipe_core() } {
                        Some(core) => core.poll_recv_ready(waker) != 0,
                        None if readable.is_null() => true,
                        None => {
                            // SAFETY: the selection owns this live watch.
                            let status = unsafe { hew_async_io_status(*readable) };
                            status != AsyncIoStatus::Pending as i32
                        }
                    };
                    if !ready {
                        continue;
                    }
                    // A closed, faulted or failed stream is ready too: the
                    // winning arm's receive resolves it to `None` or its fault.
                    let order = COMPLETION_ORDER.fetch_add(1, Ordering::Relaxed);
                    *ready_order = Some(order);
                    order
                }
            }
        };
        if !first_completion {
            return i64::try_from(index).unwrap_or(-2);
        }
        let candidate = (order, index);
        first = Some(first.map_or(candidate, |previous| std::cmp::min(previous, candidate)));
    }
    if let Some((_, index)) = first {
        return i64::try_from(index).unwrap_or(-2);
    }
    if !selection.timer.is_null() {
        // SAFETY: the selection owns its timer until detached.
        let status = unsafe { hew_coro_sleep_status(selection.timer) };
        if status == CoroStatus::Complete as i32 {
            return i64::try_from(selection.sources.len()).unwrap_or(-2);
        }
        if status != CoroStatus::Pending as i32 {
            return -2;
        }
    }
    // SAFETY: all alternatives are still pending after the matching prepare.
    if unsafe { hew_actor_wait_edge_pending(selection.wait) } != 0 {
        -3
    } else {
        -1
    }
}

/// Materialize the proven actor knot reported by a -3 poll result.
///
/// # Safety
/// The selection is live and its last poll diagnosed an actor wait cycle.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_fault(
    selection: *mut HewCheckedTaskSelect,
) -> *mut crate::fault::HewFault {
    // SAFETY: the selection is live per the caller's contract.
    if let Some(reason) = unsafe { &(*selection).refusal } {
        return Box::into_raw(Box::new(crate::fault::HewFault::with_message(
            crate::internal::types::HEW_TRAP_USER_PANIC,
            reason.clone(),
        )));
    }
    // SAFETY: the selection retains its diagnosed edge until this copy returns.
    unsafe { hew_actor_wait_edge_fault((*selection).wait) }
}

/// Detach every observation without consuming or cancelling a source.
/// Late notifications retain their own readiness target, never frame storage.
///
/// # Safety
/// `selection` is null or the uniquely owned live handle, with no active poll.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_free(selection: *mut HewCheckedTaskSelect) {
    if !selection.is_null() {
        // SAFETY: caller transfers the unique selection allocation.
        drop(unsafe { Box::from_raw(selection) });
    }
}
