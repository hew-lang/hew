//! Checked completion at the existing actor dispatch boundary.
//!
//! The execution context owns a returned logical fault until its scheduler
//! activation consumes it. Generated callbacks retain the nullable continuation
//! ABI and never encode a fault as a continuation or unwind a native frame.

use std::ptr;
use std::sync::Arc;

use crate::execution_context::HewExecutionContext;
use crate::fault::HewFault;
use crate::lifetime::live_actors::ActorIncarnation;

#[path = "actor_native_cleanup.rs"]
pub(crate) mod cleanup;
pub use cleanup::HewNativeCrashContext;
#[path = "actor_native_close.rs"]
mod close;
pub(crate) use close::{
    finish_actor_terminal, finish_native_terminal, finish_native_terminal_after_terminate,
};
pub use close::{
    hew_actor_stop_native, hew_actor_terminate_native, hew_actor_wait_free, hew_actor_wait_new,
    hew_actor_wait_poll, hew_actor_wait_take_fault, HewNativeActorWait, NativeActorCompletion,
};
pub(crate) use close::{wait_from_target, NativeWaitTarget};
#[path = "actor_native_wait_graph.rs"]
pub(crate) mod wait_graph;

unsafe extern "C" fn wake_actor(context: *mut std::ffi::c_void) {
    // SAFETY: each descriptor retains the immutable incarnation allocation.
    let target = unsafe { *context.cast::<ActorIncarnation>() };
    wait_graph::ready(target);
    crate::resume::enqueue_resume_by_incarnation(target);
}

unsafe extern "C" fn retain_actor_wake(context: *mut std::ffi::c_void) {
    // SAFETY: the descriptor's owner keeps an Arc reference live during retain.
    unsafe { Arc::increment_strong_count(context.cast::<ActorIncarnation>()) };
}

unsafe extern "C" fn release_actor_wake(context: *mut std::ffi::c_void) {
    // SAFETY: consumes exactly one reference acquired by descriptor retention.
    unsafe { Arc::decrement_strong_count(context.cast::<ActorIncarnation>()) };
}

/// Create a handler invocation whose readiness resumes its exact actor turn.
/// Operations may retain the wake target after the invocation or actor is gone;
/// the live incarnation registry rejects those late notifications.
///
/// # Safety
/// The current execution context must belong to the live actor dispatch that
/// owns this invocation. Release the state only after destroying its frame.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_coro_state_new() -> *mut crate::coro_state::HewCoroState {
    let context = crate::execution_context::current_context();
    // SAFETY: the generated dispatch adapter runs under its live context.
    let context = unsafe { &*context };
    // SAFETY: the current dispatch pins both actor and cancellation ancestry.
    unsafe { new_actor_state(context.actor, context.cancel_token) }
}

/// Associate one generated actor stream handler with its exact output sink.
/// The retained channel registration owns cancellation and a wake target, not
/// this state pointer or the generated coroutine frame.
///
/// # Safety
/// `state` must be the live invocation for the current exclusive actor turn;
/// `sink` must be that handler's live, channel-backed final `Sink<T>` argument.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_coro_bind_stream(
    state: *mut crate::coro_state::HewCoroState,
    sink: *mut crate::stream::HewSink,
) {
    if state.is_null() || sink.is_null() {
        std::process::abort();
    }
    // SAFETY: the generated adapter retains this body-owned sink through bind.
    let Some(core) = (unsafe { crate::stream::clone_sink_channel_core(sink) }) else {
        std::process::abort();
    };
    // SAFETY: caller retains the invocation and exclusively owns its context.
    if !unsafe { (&mut *state).bind_actor_stream(core) } {
        std::process::abort();
    }
}

fn normalize_actor_stream_cancellation(
    state: &crate::coro_state::HewCoroState,
    fault: Box<HewFault>,
    external_trap: bool,
    stronger_stop: bool,
) -> Option<Box<HewFault>> {
    if fault.code() != crate::fault::HEW_FAULT_CANCELLED || external_trap || stronger_stop {
        return Some(fault);
    }
    let reader_closed = state
        .execution()
        .actor_stream
        .as_ref()
        .is_some_and(|binding| binding.cancellation.caused_cancellation(state));
    if !reader_closed {
        return Some(fault);
    }
    // SAFETY: this cancellation is the exact reader-close request; cleanup
    // diagnostics remain owned by and are returned from the normalizer.
    let normalized = unsafe { crate::fault::hew_fault_finish_cleanup(Box::into_raw(fault)) };
    if normalized.is_null() {
        None
    } else {
        // SAFETY: the cleanup normalizer returned one unique fault owner.
        Some(unsafe { Box::from_raw(normalized) })
    }
}

unsafe fn new_actor_state(
    actor: *mut crate::actor::HewActor,
    token: *mut crate::cancel_token::HewCancellationToken,
) -> *mut crate::coro_state::HewCoroState {
    // SAFETY: activation ownership keeps the actor alive during capture.
    let incarnation = unsafe { ActorIncarnation::of(actor) };
    // SAFETY: as above; the incarnation's identity fields never change, so
    // one wake target serves every turn.
    let target = match unsafe { (*actor).native_completion.as_ref() } {
        Some(completion) => completion
            .turn_wake
            .get_or_init(|| Arc::new(incarnation))
            .clone(),
        None => Arc::new(incarnation),
    };
    let waker = crate::wake::HewWaker {
        context: Arc::as_ptr(&target).cast_mut().cast(),
        wake: wake_actor,
        retain: retain_actor_wake,
        release: release_actor_wake,
    };
    // SAFETY: the local Arc and current context retain both inputs for creation.
    let state = unsafe { crate::coro_state::hew_coro_state_new(&raw const waker, token) };
    // SAFETY: the new invocation belongs exclusively to this strict actor turn.
    unsafe {
        let execution = (*state).execution_mut();
        execution.actor_turn = *target;
        execution.value_owner = (*actor)
            .native_completion
            .as_ref()
            .and_then(|completion| completion.cleanup.value_owner.clone());
        execution.rc_owner = (*actor)
            .native_completion
            .as_ref()
            .map(|completion| completion.cleanup.rc.clone());
    }
    // SAFETY: this activation owns the actor and its one strict turn. The
    // adapter clears the borrowed slot after child completion under the same
    // activation ownership, before another turn can start.
    unsafe { &*actor }
        .checked_invocation
        .store(state.cast(), std::sync::atomic::Ordering::Release);
    state
}

pub(crate) unsafe fn new_cleanup_state(
    actor: &crate::actor::HewActor,
) -> *mut crate::coro_state::HewCoroState {
    // SAFETY: the exclusive cleanup activation retains the actor; cleanup
    // begins fresh cancellation ancestry while preserving readiness identity.
    unsafe { new_actor_state(ptr::from_ref(actor).cast_mut(), ptr::null_mut()) }
}

/// Complete a checked actor invocation after its generated frame is destroyed.
/// Stream reader-close cancellation is normalized only for the exact bound
/// producer token; cleanup faults remain the actor's checked failure.
///
/// # Safety
/// `state` is the current actor activation's live invocation. Its frame has
/// been destroyed. `fault` is null or its unique logical fault owner, which is
/// relinquished here.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_coro_finish(
    state: *mut crate::coro_state::HewCoroState,
    fault: *mut HewFault,
) {
    if state.is_null() {
        std::process::abort();
    }
    let context = crate::execution_context::current_context();
    // SAFETY: the generated adapter has returned under its exclusive actor
    // activation, keeping both context and invocation live.
    let context_ref = unsafe { &*context };
    // SAFETY: the exclusive activation retains its actor through completion.
    let actor = unsafe { &*context_ref.actor };
    // SAFETY: the caller transfers the one optional fault owner.
    let fault = if fault.is_null() {
        None
    } else {
        // SAFETY: caller transfers the unique non-null fault owner.
        Some(unsafe { Box::from_raw(fault) })
    };
    let fault = if let Some(current_fault) = fault {
        let external_trap = actor
            .pending_external_trap_code
            .load(std::sync::atomic::Ordering::Acquire)
            != 0;
        // SAFETY: actor belongs to the current activation, which pins mailbox.
        let stopped = unsafe { crate::mailbox::mailbox_stop_requested(actor.mailbox.cast()) };
        let shutdown = crate::fault::cancelled_by_shutdown(current_fault.code());
        // SAFETY: state remains live until after normalization and detachment.
        normalize_actor_stream_cancellation(
            unsafe { &*state },
            current_fault,
            external_trap,
            stopped || shutdown,
        )
    } else {
        None
    };
    let actor_state = &actor.checked_invocation;
    if actor_state
        .compare_exchange(
            state.cast(),
            ptr::null_mut(),
            std::sync::atomic::Ordering::AcqRel,
            std::sync::atomic::Ordering::Acquire,
        )
        .is_err()
    {
        std::process::abort();
    }
    // SAFETY: its LLVM frame is already destroyed and the exact actor slot is
    // detached; dropping state unregisters only its channel association.
    unsafe { crate::coro_state::hew_coro_state_free(state) };
    // SAFETY: resume installs a fresh context before entering generated code.
    unsafe { hew_actor_dispatch_set_fault(context, fault.map_or(ptr::null_mut(), Box::into_raw)) };
}

/// Request cooperative cleanup while preserving the scheduler's strict turn.
///
/// # Safety
/// The caller owns the actor activation, keeping its invocation and current
/// token stable during this call. External stoppers must latch mailbox stop
/// and wake the actor.
pub(crate) unsafe fn cancel_checked_turn(actor: &crate::actor::HewActor) -> bool {
    let state = actor
        .checked_invocation
        .load(std::sync::atomic::Ordering::Acquire);
    if state.is_null() {
        return false;
    }
    // SAFETY: activation ownership keeps the borrowed invocation state live.
    unsafe { crate::coro_state::hew_coro_state_cancel(state.cast()) };
    true
}

/// Restore the actor's outer checked invocation after a terminal-hook poll.
#[derive(Debug)]
pub(crate) struct CheckedInvocationBinding<'a> {
    actor: &'a crate::actor::HewActor,
    previous: *mut std::ffi::c_void,
}

impl Drop for CheckedInvocationBinding<'_> {
    fn drop(&mut self) {
        self.actor
            .checked_invocation
            .store(self.previous, std::sync::atomic::Ordering::Release);
    }
}

/// Lend the current terminal hook to activation-owned stop polling.
/// # Safety
/// The caller exclusively owns the activation and keeps both invocations live
/// until this binding is restored, before destroying the hook invocation.
pub(crate) unsafe fn bind_checked_invocation(
    actor: &crate::actor::HewActor,
    state: *mut crate::coro_state::HewCoroState,
) -> CheckedInvocationBinding<'_> {
    let previous = actor
        .checked_invocation
        .swap(state.cast(), std::sync::atomic::Ordering::AcqRel);
    CheckedInvocationBinding { actor, previous }
}

/// Observe a latched terminal request while the current strict turn computes.
/// Cleanup invocations keep their independent cancellation ancestry.
pub(crate) fn poll_turn_stop() {
    let context = crate::execution_context::current_context();
    if context.is_null() {
        return;
    }
    // SAFETY: an installed actor execution context owns its live activation.
    let actor = unsafe { (*context).actor };
    // SAFETY: the installed context pins this optional actor until dispatch ends.
    let Some(actor) = (unsafe { actor.as_ref() }) else {
        return;
    };
    if actor.actor_state.load(std::sync::atomic::Ordering::Acquire)
        != crate::internal::types::HewActorState::Running as i32
    {
        return;
    }
    let terminal_cleanup = actor
        .native_completion
        .as_ref()
        .is_some_and(|completion| completion.cleanup.terminal_in_progress());
    // A graceful stop admits terminal hooks; only forced termination cancels
    // them. Mandatory release callbacks retain their independent ancestry.
    // SAFETY: activation ownership pins the mailbox and checked invocation.
    let stop_requested = unsafe {
        if terminal_cleanup {
            crate::mailbox::mailbox_terminate_requested(actor.mailbox.cast())
        } else {
            crate::mailbox::mailbox_stop_requested(actor.mailbox.cast())
        }
    };
    if stop_requested {
        // SAFETY: this is the activation owner, not an external stopper.
        unsafe {
            cancel_checked_turn(actor);
        }
    }
}

/// Finish the process root after its source cleanup and child joins complete.
/// Queued actor work and supervisor decisions use the existing shutdown path.
#[no_mangle]
pub extern "C" fn hew_native_runtime_finish(source_status: i32) -> i32 {
    let shutdown_status = drain_to_quiescence();
    let status = if source_status != 0 {
        // A native `main` return is the process exit code directly (unlike
        // the `exit()` builtin, it never passes through `hew_exit`), so it
        // needs the same portable byte truncation applied here.
        #[cfg(not(target_arch = "wasm32"))]
        {
            crate::exit_status::to_process_exit_byte(i64::from(source_status))
        }
        #[cfg(target_arch = "wasm32")]
        {
            source_status
        }
    } else if shutdown_status != 0 {
        1
    } else {
        crate::exit_status::hew_runtime_exit_status()
    };
    crate::output::flush();
    crate::test_report::finish(status);
    status
}

/// Run every actor to a stop and reclaim the runtime, returning a non-zero
/// status when shutdown itself failed.
///
/// Native processes use the shutdown phase machine. WASI drives the same
/// supervisor and actor terminal owners inline, then closes local admission
/// and verifies that all routes and teardown leases have drained.
fn drain_to_quiescence() -> i32 {
    #[cfg(not(target_arch = "wasm32"))]
    {
        if crate::driver::active() {
            crate::driver::run_to_quiescence();
        }
        crate::shutdown::hew_shutdown_initiate_implicit(0);
        let status = crate::shutdown::hew_shutdown_wait();
        crate::scheduler::hew_runtime_cleanup_after_main();
        status
    }
    #[cfg(target_arch = "wasm32")]
    {
        crate::driver::run_to_quiescence();
        if !crate::supervisor::roots::stop_registered_supervisors() {
            return 1;
        }
        // Root teardown can enqueue detached incarnation frees. Drain them
        // before selecting the remaining process-owned actors.
        crate::driver::run_to_quiescence();
        let actors = crate::lifetime::live_actors::snapshot_live_incarnations();
        for &target in &actors {
            crate::lifetime::live_actors::with_live_incarnation(target, |actor| {
                // SAFETY: the exact incarnation pin protects this stop request.
                unsafe { crate::actor::hew_actor_request_stop(actor.as_ptr()) };
            });
        }
        for target in actors {
            let actor = crate::lifetime::live_actors::with_live_incarnation(
                target,
                crate::lifetime::live_actors::ActorPin::as_ptr,
            );
            let Some(actor) = actor else { continue };
            // SAFETY: the single driver cannot retire this freshly resolved
            // actor before free takes over. Release the lookup pin first so
            // free can drain admitted pins; never retain another actor address
            // across the scheduler progress made by this call.
            if unsafe { crate::actor::hew_actor_free(actor) } != 0 {
                return 1;
            }
        }
        crate::driver::run_to_quiescence();
        crate::lifetime::local_handles::begin_current_shutdown();
        crate::lifetime::local_handles::close_current_supervisor_teardown_admission();
        if !crate::lifetime::local_handles::wait_for_current_supervisor_teardowns(
            std::time::Duration::from_secs(5),
        ) {
            return 1;
        }
        crate::lifetime::local_handles::assert_current_actor_routes_empty();
        crate::lifetime::local_handles::assert_current_supervisors_empty();
        0
    }
}

/// Allocate a generated state or message wrapper with the native allocator.
/// Allocation failure has the same process-fatal policy as other value buffers.
#[no_mangle]
#[must_use]
pub extern "C" fn hew_actor_payload_alloc(size: usize) -> *mut std::ffi::c_void {
    crate::mem::buf_alloc(size) // ALLOCATOR-PAIRING: GlobalAlloc
}

/// Allocate an unpublished message wrapper, preserving the source on failure.
#[no_mangle]
#[must_use]
pub extern "C" fn hew_actor_payload_try_alloc(size: usize) -> *mut std::ffi::c_void {
    crate::mem::buf_try_alloc(size) // ALLOCATOR-PAIRING: GlobalAlloc
}

/// Release a wrapper handed out by [`hew_actor_payload_alloc`] or
/// [`hew_actor_payload_try_alloc`].
///
/// Generated code calls this to discard an actor's state wrapper when `init`
/// or `start` faults before the actor is published.
///
/// # Safety
///
/// `payload` must be null, or a live wrapper from the two allocators above.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_payload_free(payload: *mut std::ffi::c_void) {
    // SAFETY: caller guarantees the wrapper came from the payload allocator.
    unsafe { crate::mem::buf_free(payload) }; // ALLOCATOR-PAIRING: GlobalAlloc
}

/// A one-way submission's outcome as generated code reads it. `Accepted` and
/// `Discarded` both mean the destination owns the typed fields; every other
/// status leaves them with the caller.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HewSubmitStatus {
    /// A waiting submission found the mailbox still full.
    Pending = -1,
    Accepted = 0,
    Full = 1,
    Closed = 2,
    Oom = 3,
    /// The message was taken and lost by policy: the sender's own newest-message
    /// discard, or the destination's declared coalescing or `drop_new` fallback.
    Discarded = 4,
    /// A supervised role whose restart budget is spent took no message; the
    /// message stays with the caller. Generated role sends answer it before
    /// reaching the runtime.
    RoleSpent = 5,
    /// The runtime is shutting down and refused this root submission; the
    /// message stays with the caller.
    ShuttingDown = 6,
}

/// The sender policy code that discards the newest message on a full mailbox.
pub const SUBMIT_DROP_NEWEST: i32 = 2;

/// Interpret admission for the incoming submission. Replacement accepts it;
/// only discarding that incoming payload reports `Discarded`. A refusal keeps
/// the message with the sender, whose own policy chooses the next action.
pub(crate) fn submission_status(outcome: crate::mailbox::SendOutcome) -> HewSubmitStatus {
    use crate::mailbox::SendOutcome;
    match outcome {
        SendOutcome::Enqueued | SendOutcome::Coalesced => HewSubmitStatus::Accepted,
        SendOutcome::Dropped => HewSubmitStatus::Discarded,
        SendOutcome::Failed => HewSubmitStatus::Full,
        SendOutcome::Closed => HewSubmitStatus::Closed,
        SendOutcome::Oom => HewSubmitStatus::Oom,
        SendOutcome::ShuttingDown => HewSubmitStatus::ShuttingDown,
        SendOutcome::DroppedOld => {
            unreachable!("a `drop_old` coalesce fallback enqueues rather than reporting eviction")
        }
    }
}

/// Try to transfer a generated message wrapper into its exact destination,
/// answering a [`HewSubmitStatus`]. A full mailbox under the sender's
/// [`SUBMIT_DROP_NEWEST`] policy discards the message instead.
///
/// # Safety
/// `caller` points to a live stamp copied before this function returns.
/// `payload` is an unpublished malloc wrapper containing shallowly transferred
/// typed fields. Acceptance or discard consumes those fields; rejection frees
/// only the wrapper bytes, leaving the original typed message with the caller.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_submit_native(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    payload: *mut std::ffi::c_void,
    size: usize,
    drop_payload: crate::mailbox::HewMsgEnvelopeDropFn,
    policy: i32,
    payload_release: Option<hew_cabi::value::HewValueReleaseStart>,
    discarded_release_out: *mut *mut crate::release_walker::HewReleaseCursor,
    caller: *const crate::actor_caller::HewCallerStamp,
    payload_layout: *const hew_cabi::value::HewValueLayout,
) -> i32 {
    // SAFETY: forwards the unpublished wrapper and typed ownership contract.
    unsafe {
        submit_native(
            token,
            message,
            payload,
            size,
            drop_payload,
            policy,
            false,
            payload_release,
            discarded_release_out,
            *caller,
            payload_layout,
        )
    }
}

/// Submit an attachment close event after queued data without capacity refusal.
///
/// # Safety
/// `caller` points to a live stamp copied before this function returns.
/// The wrapper is uniquely owned, with the same contract as native submission.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_submit_native_terminal(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    payload: *mut std::ffi::c_void,
    size: usize,
    drop_payload: crate::mailbox::HewMsgEnvelopeDropFn,
    payload_release: Option<hew_cabi::value::HewValueReleaseStart>,
    discarded_release_out: *mut *mut crate::release_walker::HewReleaseCursor,
    caller: *const crate::actor_caller::HewCallerStamp,
    payload_layout: *const hew_cabi::value::HewValueLayout,
) -> i32 {
    // SAFETY: forwards the unpublished wrapper and typed ownership contract.
    unsafe {
        submit_native(
            token,
            message,
            payload,
            size,
            drop_payload,
            0,
            true,
            payload_release,
            discarded_release_out,
            *caller,
            payload_layout,
        )
    }
}

#[expect(
    clippy::too_many_arguments,
    reason = "complete compiler-private payload admission and cleanup contract"
)]
pub(crate) unsafe fn submit_native(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    payload: *mut std::ffi::c_void,
    size: usize,
    drop_payload: crate::mailbox::HewMsgEnvelopeDropFn,
    policy: i32,
    terminal: bool,
    payload_release: Option<hew_cabi::value::HewValueReleaseStart>,
    discarded_release_out: *mut *mut crate::release_walker::HewReleaseCursor,
    caller: crate::actor_caller::HewCallerStamp,
    payload_layout: *const hew_cabi::value::HewValueLayout,
) -> i32 {
    // SAFETY: the generated caller supplies its writable cursor output.
    if !discarded_release_out.is_null() {
        // SAFETY: generated code supplies a writable optional cursor output.
        unsafe { discarded_release_out.write(ptr::null_mut()) };
    }
    if payload.is_null() {
        return HewSubmitStatus::Oom as i32;
    }
    // SAFETY: the wrapper is uniquely owned until the mailbox accepts it.
    let envelope =
        unsafe { crate::mailbox::hew_msg_envelope_new(payload, size, Some(drop_payload)) };
    if envelope.is_null() {
        // SAFETY: the source still owns all typed fields in the unpublished wrapper.
        unsafe {
            crate::mem::buf_free(payload); // ALLOCATOR-PAIRING: GlobalAlloc
        }
        return HewSubmitStatus::Oom as i32;
    }
    // SAFETY: attach the selected consuming callback before publication.
    unsafe {
        (*envelope).release_start = payload_release;
        crate::cow_envelope::set_layout(envelope, payload_layout);
    };
    // SAFETY: the envelope is unpublished and transfers only on admission.
    let outcome = unsafe {
        if terminal {
            crate::actor::submit_native_terminal(token, message, envelope)
        } else {
            crate::actor::try_submit_native_envelope(token, message, envelope, caller)
        }
    };
    let status = match submission_status(outcome) {
        status @ (HewSubmitStatus::Accepted | HewSubmitStatus::Discarded) => return status as i32,
        HewSubmitStatus::Full if policy == SUBMIT_DROP_NEWEST => {
            // SAFETY: explicit DropNewest transfers the typed payload for destruction.
            unsafe {
                let cursor = crate::cow_envelope::release_cursor(envelope);
                if discarded_release_out.is_null() {
                    assert!(cursor.is_null());
                } else {
                    discarded_release_out.write(cursor);
                }
            }
            return HewSubmitStatus::Discarded as i32;
        }
        status => status as i32,
    };
    // SAFETY: admission failed without publishing or aliasing. The source retains
    // the typed fields; these two allocations contain no other owning resources.
    unsafe {
        crate::mem::buf_free(payload); // ALLOCATOR-PAIRING: GlobalAlloc
        crate::cow_envelope::free_allocation(envelope); // ALLOCATOR-PAIRING: GlobalAlloc
    }
    status
}

/// One pointer-sized owning slot in the opaque runtime execution context.
#[repr(transparent)]
#[derive(Debug)]
pub struct CheckedActorFault(*mut HewFault);

impl Default for CheckedActorFault {
    fn default() -> Self {
        Self(ptr::null_mut())
    }
}

impl CheckedActorFault {
    /// Transfer the current fault to the scheduler, leaving the slot empty.
    pub(crate) fn take(&mut self) -> Option<Box<HewFault>> {
        let fault = std::mem::replace(&mut self.0, ptr::null_mut());
        if fault.is_null() {
            None
        } else {
            // SAFETY: this slot owns the allocation and has relinquished it.
            Some(unsafe { Box::from_raw(fault) })
        }
    }
}

impl Drop for CheckedActorFault {
    fn drop(&mut self) {
        // Also release a fault if activation setup or lock teardown fails.
        drop(self.take());
    }
}

/// Transfer one checked handler fault into its current execution context.
///
/// # Safety
/// `ctx` must be the live, exclusively accessed dispatch context supplied to
/// this handler. `fault` must be a unique owner from the native fault API or
/// null. The caller relinquishes it, including on a secondary failure.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_dispatch_set_fault(
    ctx: *mut HewExecutionContext,
    fault: *mut HewFault,
) {
    // SAFETY: the generated dispatch adapter supplies its live context.
    let ctx = unsafe { &mut *ctx };
    // SAFETY: the context slot and argument are distinct optional owners.
    ctx.checked_fault.0 = unsafe { crate::fault::hew_fault_combine(ctx.checked_fault.0, fault) };
}

pub(crate) enum DispatchFailure {
    Checked(Box<HewFault>),
    Unwind(Box<dyn std::any::Any + Send>),
}

/// Join the callback's continuation result with its owned checked completion.
///
/// # Safety
/// `ctx` must remain the exclusively accessed current activation context.
pub(crate) unsafe fn dispatch_result(
    ctx: *mut HewExecutionContext,
    outcome: std::thread::Result<*mut std::ffi::c_void>,
) -> Result<*mut std::ffi::c_void, DispatchFailure> {
    match outcome {
        Err(payload) => Err(DispatchFailure::Unwind(payload)),
        Ok(handle) => {
            // SAFETY: the callback returned and this activation owns the slot.
            let fault = unsafe { (*ctx).checked_fault.take() }.and_then(|fault| {
                // SAFETY: the returned dispatch context still owns this actor.
                unsafe { normalize_dispatch_completion(ctx, fault) }
            });
            match fault {
                None => Ok(handle),
                Some(fault) => {
                    if !handle.is_null() {
                        eprintln!("fatal: actor dispatch returned both a fault and a continuation");
                        std::process::abort();
                    }
                    Err(DispatchFailure::Checked(fault))
                }
            }
        }
    }
}

/// Complete a refused delivery after the wrapper has released its owned values.
/// Cleanup faults retain their ordinary actor-failure behaviour.
///
/// # Safety
/// The caller exclusively owns this installed activation and its fault.
pub(crate) unsafe fn normalize_dispatch_completion(
    ctx: *mut HewExecutionContext,
    fault: Box<HewFault>,
) -> Option<Box<HewFault>> {
    if fault.code() != crate::fault::HEW_FAULT_REFUSED {
        // SAFETY: this activation owns its actor and fault.
        return unsafe { normalize_stopped_turn((*ctx).actor, fault) };
    }
    if let Some(fault) = fault.without_primary() {
        // SAFETY: this activation owns the remaining cleanup failure.
        return unsafe { normalize_stopped_turn((*ctx).actor, Box::new(fault)) };
    }
    // SAFETY: the context retains its current reply sender until publication.
    let channel: *mut crate::reply_channel::HewReplyChannel =
        unsafe { (*ctx).reply_channel.cast() };
    if !channel.is_null() && !crate::activation::current_reply_channel_consumed_on(ctx) {
        // SAFETY: a refused wrapper never initialized a success reply. Publishing
        // the classified empty completion retires this one sender reference.
        unsafe {
            crate::reply_channel::hew_reply_channel_mark_failed(
                channel,
                crate::internal::types::HEW_REPLY_FAIL_REFUSED,
            );
            let _ = crate::reply_channel::hew_reply(channel, ptr::null_mut(), 0);
        }
    }
    None
}

/// Remove an expected stop's cancellation after its checked source cleanup,
/// preserving any destructor failure carried as a secondary diagnostic.
///
/// # Safety
/// The caller owns the actor activation and this unique returned fault.
pub(crate) unsafe fn normalize_stopped_turn(
    actor: *mut crate::actor::HewActor,
    fault: Box<HewFault>,
) -> Option<Box<HewFault>> {
    if fault.code() != crate::fault::HEW_FAULT_CANCELLED
        || !(crate::fault::cancelled_by_shutdown(fault.code())
            // SAFETY: this activation owns the live actor and its mailbox.
            || unsafe { crate::mailbox::mailbox_stop_requested((*actor).mailbox.cast()) })
    {
        return Some(fault);
    }
    // SAFETY: normalization consumes and returns unique optional fault owners.
    let fault = unsafe { crate::fault::hew_fault_finish_cleanup(Box::into_raw(fault)) };
    if fault.is_null() {
        None
    } else {
        // SAFETY: normalization returned one uniquely owned fault.
        Some(unsafe { Box::from_raw(fault) })
    }
}

/// Preserve the checked diagnostic when an unhandled actor fault reaches its
/// existing scheduler crash and supervisor boundary.
pub(crate) fn report_checked_failure(fault: &HewFault) -> i32 {
    // SAFETY: the returned fault remains owned during this reporting borrow.
    let _ = unsafe { crate::fault::hew_fault_report(fault) };
    fault.code()
}

/// Preserve a checked actor fault across cleanup and terminal observation.
pub(crate) fn report_actor_failure(actor: &crate::actor::HewActor, fault: HewFault) -> i32 {
    if let Some(completion) = &actor.native_completion {
        completion.record_fault(fault)
    } else {
        report_checked_failure(&fault)
    }
}

/// Finish the matching native completion or legacy unwind cleanup boundary.
///
/// # Safety
/// This scheduler activation must exclusively own `actor` and its dispatch
/// cleanup scope; generated native code has already run its checked cleanup.
pub(crate) unsafe fn finish_dispatch_failure(
    actor: *mut crate::actor::HewActor,
    failure: DispatchFailure,
) -> i32 {
    match failure {
        DispatchFailure::Checked(fault) => {
            // SAFETY: checked completion owns the still-open dispatch scope.
            if !unsafe { crate::cont::finish_dispatch_crash_cleanup() } {
                eprintln!("fatal: checked actor failure retained crash-cleanup owners");
                std::process::abort();
            }
            // SAFETY: the completed activation retains the exact incarnation.
            report_actor_failure(unsafe { &*actor }, *fault)
        }
        DispatchFailure::Unwind(payload) => {
            crate::execution_context::reply_channel_swap_unwind();
            // SAFETY: the scheduler owns the legacy state-recovery boundary.
            let outcome =
                unsafe { crate::cont::recover_dispatch_crash_cleanup_with_outcome(false) };
            if outcome.state_authority_consumed {
                // SAFETY: this activation exclusively owns the actor.
                unsafe { crate::actor::record_dispatch_state_drop_consumed(actor) };
            }
            let code = payload
                .downcast_ref::<crate::actor::HewPanic>()
                .map_or(101, |panic| panic.code);
            crate::util::quarantine_panic_payload(payload);
            code
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::c_void;
    use std::sync::atomic::Ordering;

    fn stream_invocation(
        core: Arc<crate::channel_core::ChannelCore>,
    ) -> (
        Box<crate::coro_state::HewCoroState>,
        Arc<crate::wake::blocking::Readiness>,
    ) {
        let (ready, waker) = crate::wake::blocking::Readiness::new();
        // SAFETY: the state retains this live waker descriptor.
        let state =
            unsafe { crate::coro_state::hew_coro_state_new(waker.descriptor(), ptr::null_mut()) };
        assert!(!state.is_null());
        // SAFETY: this test uniquely owns the newly allocated invocation.
        let mut state = unsafe { Box::from_raw(state) };
        assert!(state.bind_actor_stream(core));
        (state, ready)
    }

    #[test]
    fn actor_stream_close_cancels_only_its_bound_invocation() {
        let _guard = crate::runtime_test_guard();
        let core = Arc::new(crate::channel_core::ChannelCore::new(1));
        let (state, ready) = stream_invocation(Arc::clone(&core));

        core.close_stream();

        assert_eq!(
            // SAFETY: this test owns the live invocation throughout the query.
            unsafe { crate::coro_state::hew_coro_state_is_cancelled(&raw const *state) },
            1
        );
        assert!(ready.take_ready());
        let cancellation = crate::fault::hew_fault_new(crate::fault::HEW_FAULT_CANCELLED);
        assert!(normalize_actor_stream_cancellation(
            &state,
            // SAFETY: the freshly allocated fault has one owner, transferred here.
            unsafe { Box::from_raw(cancellation) },
            false,
            false,
        )
        .is_none());
    }

    #[test]
    fn actor_stream_close_keeps_cleanup_fault_and_obeys_stop_precedence() {
        let _guard = crate::runtime_test_guard();
        let core = Arc::new(crate::channel_core::ChannelCore::new(1));
        let (state, _ready) = stream_invocation(Arc::clone(&core));
        core.close_stream();
        // SAFETY: these two fresh fault owners are distinct and transferred.
        let combined = unsafe {
            crate::fault::hew_fault_combine(
                crate::fault::hew_fault_new(crate::fault::HEW_FAULT_CANCELLED),
                crate::fault::hew_fault_new(212),
            )
        };
        let preserved = normalize_actor_stream_cancellation(
            &state,
            // SAFETY: combine returned one unique fault owner, transferred here.
            unsafe { Box::from_raw(combined) },
            false,
            false,
        )
        .expect("cleanup failure survives stream cancellation");
        assert_eq!(preserved.code(), 212);

        let cancellation = crate::fault::hew_fault_new(crate::fault::HEW_FAULT_CANCELLED);
        let stopped = normalize_actor_stream_cancellation(
            &state,
            // SAFETY: the freshly allocated fault has one owner, transferred here.
            unsafe { Box::from_raw(cancellation) },
            false,
            true,
        )
        .expect("actor stop remains authoritative after reader close");
        assert_eq!(stopped.code(), crate::fault::HEW_FAULT_CANCELLED);
    }

    #[test]
    fn actor_stop_that_wins_before_stream_close_is_not_relabelled() {
        let _guard = crate::runtime_test_guard();
        let core = Arc::new(crate::channel_core::ChannelCore::new(1));
        let (state, _ready) = stream_invocation(Arc::clone(&core));
        // SAFETY: this test owns the live invocation; stop wins its token CAS.
        unsafe { crate::coro_state::hew_coro_state_cancel(&raw const *state) };
        core.close_stream();

        let cancellation = crate::fault::hew_fault_new(crate::fault::HEW_FAULT_CANCELLED);
        let preserved = normalize_actor_stream_cancellation(
            &state,
            // SAFETY: the freshly allocated fault has one owner, transferred here.
            unsafe { Box::from_raw(cancellation) },
            false,
            false,
        )
        .expect("the actor's earlier stop reason wins");
        assert_eq!(preserved.code(), crate::fault::HEW_FAULT_CANCELLED);
    }

    #[test]
    fn checked_waker_retains_incarnation_after_invocation_and_actor_replacement() {
        let _guard = crate::runtime_test_guard();
        let scheduler = crate::scheduler::NoWorkerSchedulerForTest::install();
        let actor = crate::test_actor::TrackedTestActor::install_parked();
        let mut context = HewExecutionContext {
            actor: actor.ptr(),
            ..HewExecutionContext::default()
        };
        let previous = crate::execution_context::set_current_context(&raw mut context);
        // SAFETY: the fixture owns the installed actor context and invocation.
        // The retained operation descriptor outlives both, as a late I/O wake can.
        let operation = unsafe {
            let state = hew_actor_coro_state_new();
            let operation =
                crate::wake::OwnedWaker::retain(&*crate::coro_state::hew_coro_state_waker(state));
            operation.wake();
            crate::test_actor::assert_woken(&scheduler, &actor, "checked handler");
            hew_actor_coro_finish(state, ptr::null_mut());
            operation
        };
        assert_eq!(
            crate::execution_context::set_current_context(previous),
            &raw mut context
        );
        actor.reincarnate_parked_reusing_id();
        operation.wake();
        crate::test_actor::assert_not_woken(&scheduler, &actor, "checked handler");
    }

    unsafe extern "C-unwind" fn checked_failure(
        ctx: *mut HewExecutionContext,
        state: *mut c_void,
        _message: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        // SAFETY: the fixture supplies a live scalar state and dispatch context.
        unsafe {
            *state.cast::<i64>() = 7;
            hew_actor_dispatch_set_fault(ctx, crate::fault::hew_fault_new(202));
        }
        ptr::null_mut()
    }

    #[test]
    fn checked_dispatch_crashes_and_completes_its_reply_without_unwinding() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = crate::scheduler::NoWorkerSchedulerForTest::install();
        let baseline = crate::reply_channel::active_channel_count();
        let channel = crate::reply_channel::hew_reply_channel_new();
        assert!(!channel.is_null());
        let mut state = 3_i64;
        let mut actor = crate::test_actor::stub_actor();
        actor.state = (&raw mut state).cast();
        actor.state_size = std::mem::size_of::<i64>();
        actor.dispatch = Some(checked_failure);
        // SAFETY: all allocations and both reply references remain live until
        // this single synchronous scheduler activation finishes.
        unsafe {
            let mailbox = crate::mailbox::hew_mailbox_new();
            assert!(!mailbox.is_null());
            actor.mailbox = mailbox.cast();
            crate::reply_channel::hew_reply_channel_retain(channel);
            assert_eq!(
                crate::mailbox::hew_mailbox_send_with_reply(
                    mailbox.cast(),
                    1,
                    ptr::null_mut(),
                    0,
                    channel.cast(),
                ),
                0
            );
            crate::scheduler::activate_actor_for_test(&raw mut actor);
            assert_eq!(
                actor.actor_state.load(Ordering::Acquire),
                crate::internal::types::HewActorState::Crashed as i32
            );
            assert_eq!(actor.error_code.load(Ordering::Acquire), 202);
            assert_eq!(state, 7);
            assert!(!actor.state_drop_consumed.load(Ordering::Acquire));
            assert_eq!(crate::reply_channel::ref_count_for_test(channel), 1);
            assert_eq!(
                crate::reply_channel::hew_reply_channel_await_status(channel),
                crate::await_cancel::AwaitCancelStatus::Completed as i32
            );
            crate::reply_channel::hew_reply_channel_free(channel);
            crate::mailbox::hew_mailbox_free(mailbox);
        }
        assert_eq!(crate::reply_channel::active_channel_count(), baseline);
        assert!(crate::execution_context::current_context().is_null());
    }

    unsafe extern "C" fn checked_resume(frame: *mut c_void) {
        // SAFETY: the scheduler owns this live scratch frame and installs the
        // resumed actor's context before invoking its continuation.
        unsafe {
            let frame = &mut *frame.cast::<crate::coro_exec::test_support::ScratchFrame>();
            frame.resumes.fetch_add(1, Ordering::AcqRel);
            frame.resume = None;
            let context = crate::execution_context::current_context();
            *(*context).actor.as_ref().unwrap().state.cast::<i64>() = 7;
            hew_actor_dispatch_set_fault(context, crate::fault::hew_fault_new(202));
        }
    }

    #[test]
    fn checked_resume_crashes_and_retires_reply_after_normal_frame_cleanup() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = crate::scheduler::NoWorkerSchedulerForTest::install();
        let baseline = crate::reply_channel::active_channel_count();
        let channel = crate::reply_channel::hew_reply_channel_new();
        assert!(!channel.is_null());
        let mut state = 3_i64;
        let mut actor = crate::test_actor::stub_actor();
        let mut frame = crate::coro_exec::test_support::ScratchFrameOwner::new(1);
        frame.resume = Some(checked_resume);
        actor.state = (&raw mut state).cast();
        actor.state_size = std::mem::size_of::<i64>();
        actor
            .suspended_cont
            .store(frame.handle(), Ordering::Release);
        actor.cont_tag.store(
            crate::internal::types::ContTag::Parked as i32,
            Ordering::Release,
        );
        actor.actor_state.store(
            crate::internal::types::HewActorState::Runnable as i32,
            Ordering::Release,
        );
        // SAFETY: the fixture owns the frame and mailbox, and transfers one
        // retained reply reference to the parked actor before resuming it.
        unsafe {
            let mailbox = crate::mailbox::hew_mailbox_new();
            assert!(!mailbox.is_null());
            actor.mailbox = mailbox.cast();
            crate::reply_channel::hew_reply_channel_retain(channel);
            actor
                .suspended_reply_channel
                .store(channel.cast(), Ordering::Release);
            crate::scheduler::activate_actor_for_test(&raw mut actor);
            assert_eq!(
                actor.actor_state.load(Ordering::Acquire),
                crate::internal::types::HewActorState::Crashed as i32
            );
            assert_eq!(actor.error_code.load(Ordering::Acquire), 202);
            assert_eq!(state, 7);
            assert!(!actor.state_drop_consumed.load(Ordering::Acquire));
            assert_eq!(frame.resumes.load(Ordering::Acquire), 1);
            assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);
            assert!(frame.heap_guard.load(Ordering::Acquire).is_null());
            assert!(actor.suspended_cont.load(Ordering::Acquire).is_null());
            assert!(actor
                .suspended_reply_channel
                .load(Ordering::Acquire)
                .is_null());
            assert_eq!(crate::reply_channel::ref_count_for_test(channel), 1);
            assert_eq!(
                crate::reply_channel::hew_reply_channel_failure_kind(channel),
                crate::internal::types::HEW_REPLY_FAIL_HANDLER_TRAPPED
            );
            assert_eq!(
                crate::reply_channel::hew_reply_channel_await_status(channel),
                crate::await_cancel::AwaitCancelStatus::Completed as i32
            );
            crate::reply_channel::hew_reply_channel_free(channel);
            crate::mailbox::hew_mailbox_free(mailbox);
        }
        assert_eq!(crate::reply_channel::active_channel_count(), baseline);
        assert!(crate::execution_context::current_context().is_null());
    }

    unsafe extern "C" fn resume_checked_cleanup(frame: *mut c_void) {
        // SAFETY: the fixture's scheduler owns this scratch frame and installs
        // its actor context. The first cleanup poll deliberately stays pending.
        unsafe {
            let frame = &mut *frame.cast::<crate::coro_exec::test_support::ScratchFrame>();
            let actor = &*(*crate::execution_context::current_context()).actor;
            let state = actor.checked_invocation.load(Ordering::Acquire).cast();
            assert_eq!(crate::coro_state::hew_coro_state_is_cancelled(state), 1);
            if frame.resumes.fetch_add(1, Ordering::AcqRel) == 1 {
                crate::coro_state::hew_coro_state_publish(
                    state,
                    crate::coro_state::CoroStatus::Cancelled as i32,
                );
                hew_actor_coro_finish(state, ptr::null_mut());
                frame.resume = None;
            }
        }
    }

    #[test]
    fn checked_stop_keeps_turn_until_pending_cleanup_finishes() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = crate::scheduler::NoWorkerSchedulerForTest::install();
        let (_, waker) = crate::wake::blocking::Readiness::new();
        let mut actor = crate::test_actor::stub_actor();
        let mut frame = crate::coro_exec::test_support::ScratchFrameOwner::new(1);
        frame.resume = Some(resume_checked_cleanup);
        // SAFETY: all fixture resources remain locally owned until scheduler
        // completion; the frame relinquishes its state on its terminal poll.
        unsafe {
            let state = crate::coro_state::hew_coro_state_new(waker.descriptor(), ptr::null_mut());
            actor
                .checked_invocation
                .store(state.cast(), Ordering::Release);
            actor
                .suspended_cont
                .store(frame.handle(), Ordering::Release);
            actor.cont_tag.store(
                crate::internal::types::ContTag::Parked as i32,
                Ordering::Release,
            );
            actor.actor_state.store(
                crate::internal::types::HewActorState::Runnable as i32,
                Ordering::Release,
            );
            let mailbox = crate::mailbox::hew_mailbox_new();
            actor.mailbox = mailbox.cast();
            crate::mailbox::mailbox_close(mailbox);
            crate::mailbox::mailbox_request_stop(mailbox);
            crate::scheduler::activate_actor_for_test(&raw mut actor);
            assert_eq!(frame.resumes.load(Ordering::Acquire), 1);
            assert_eq!(frame.destroyed.load(Ordering::Acquire), 0);
            assert_eq!(
                actor.actor_state.load(Ordering::Acquire),
                crate::internal::types::HewActorState::Suspended as i32
            );
            assert!(!actor.checked_invocation.load(Ordering::Acquire).is_null());
            actor.actor_state.store(
                crate::internal::types::HewActorState::Runnable as i32,
                Ordering::Release,
            );
            crate::scheduler::activate_actor_for_test(&raw mut actor);
            assert_eq!(frame.resumes.load(Ordering::Acquire), 2);
            assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);
            assert_eq!(
                actor.actor_state.load(Ordering::Acquire),
                crate::internal::types::HewActorState::Stopped as i32
            );
            assert!(actor.checked_invocation.load(Ordering::Acquire).is_null());
            assert!(actor.suspended_cont.load(Ordering::Acquire).is_null());
            crate::mailbox::hew_mailbox_free(mailbox);
        }
    }

    #[derive(Clone, Copy, Default)]
    struct ExternalTrapHooks {
        calls: usize,
        drops: usize,
    }

    unsafe extern "C" fn external_trap_hook(
        context: *mut c_void,
        _fault: *mut *mut c_void,
        invocation: *mut c_void,
    ) -> *mut c_void {
        // SAFETY: terminal cleanup retains the context and initialized state.
        let context = unsafe { &mut *context.cast::<super::HewNativeCrashContext>() };
        // SAFETY: the fixture lends this exact scalar record until hook drain.
        let state = unsafe { &mut *context.state.cast::<ExternalTrapHooks>() };
        assert_eq!(context.kind, crate::internal::types::CrashKind::Crashed);
        assert_eq!(state.calls, 0);
        assert_eq!(state.drops, 0);
        // SAFETY: the context retains a managed diagnostic string.
        assert!(!unsafe { hew_cabi::string::string_as_str(context.message) }.is_empty());
        state.calls += 1;
        context.action = crate::supervisor::CRASH_ACTION_KILL;
        // SAFETY: this synchronous callback owns its supplied invocation.
        unsafe { crate::coro_state::hew_coro_state_finish(invocation.cast(), 0) };
        ptr::null_mut()
    }

    unsafe extern "C-unwind" fn external_trap_state_clone(state: *const c_void) -> *mut c_void {
        // SAFETY: the fixture lends an initialized scalar state record.
        let copy = unsafe { *state.cast::<ExternalTrapHooks>() };
        // This allocation matches the runtime state wrapper convention.
        let allocation = crate::mem::buf_alloc(std::mem::size_of::<ExternalTrapHooks>()) // ALLOCATOR-PAIRING: GlobalAlloc
            .cast::<ExternalTrapHooks>();
        assert!(!allocation.is_null());
        // SAFETY: the allocation has the exact state size and alignment.
        unsafe { allocation.write(copy) };
        allocation.cast()
    }

    unsafe extern "C" fn external_trap_state_drop(state: *mut c_void) {
        // SAFETY: the fixture keeps this initialized state live through terminal cleanup.
        let state = unsafe { &mut *state.cast::<ExternalTrapHooks>() };
        assert_eq!(state.calls, 1);
        assert_eq!(state.drops, 0);
        state.drops += 1;
    }

    #[test]
    fn external_trap_drains_a_parked_checked_turn_before_crashing() {
        let _guard = crate::runtime_test_guard();
        let _scheduler = crate::scheduler::NoWorkerSchedulerForTest::install();
        let (_, waker) = crate::wake::blocking::Readiness::new();
        let mut actor = crate::test_actor::stub_actor();
        let mut hooks = ExternalTrapHooks::default();
        actor.state = (&raw mut hooks).cast();
        actor.state_drop_fn = Some(external_trap_state_drop);
        actor.state_clone_fn = Some(external_trap_state_clone);
        actor.state_size = std::mem::size_of::<ExternalTrapHooks>();
        let completion = std::sync::Arc::new(super::NativeActorCompletion::with_crash(Some(
            external_trap_hook,
        )));
        actor.native_completion = Some(completion.clone());

        let mut frame = crate::coro_exec::test_support::ScratchFrameOwner::new(1);
        frame.resume = Some(resume_checked_cleanup);
        // SAFETY: this fixture exclusively owns its actor, invocation, frame,
        // and mailbox until the checked cancellation has completed.
        unsafe {
            let state = crate::coro_state::hew_coro_state_new(waker.descriptor(), ptr::null_mut());
            actor
                .checked_invocation
                .store(state.cast(), Ordering::Release);
            actor
                .suspended_cont
                .store(frame.handle(), Ordering::Release);
            actor.cont_tag.store(
                crate::internal::types::ContTag::Parked as i32,
                Ordering::Release,
            );
            actor.actor_state.store(
                crate::internal::types::HewActorState::Suspended as i32,
                Ordering::Release,
            );
            let mailbox = crate::mailbox::hew_mailbox_new();
            actor.mailbox = mailbox.cast();

            crate::actor::hew_actor_trap(&raw mut actor, 91);
            assert_eq!(
                actor.actor_state.load(Ordering::Acquire),
                crate::internal::types::HewActorState::Runnable as i32,
                "the external trap defers terminal publication until cleanup"
            );
            assert_eq!(actor.pending_external_trap_code.load(Ordering::Acquire), 91);

            crate::scheduler::activate_actor_for_test(&raw mut actor);
            assert_eq!(frame.resumes.load(Ordering::Acquire), 1);
            assert_eq!(
                actor.actor_state.load(Ordering::Acquire),
                crate::internal::types::HewActorState::Suspended as i32,
                "the first cancellation poll remains live"
            );
            assert!(!actor.checked_invocation.load(Ordering::Acquire).is_null());
            assert_eq!((hooks.calls, hooks.drops), (0, 0));
            assert_eq!(completion.crash_action(), None);

            actor.actor_state.store(
                crate::internal::types::HewActorState::Runnable as i32,
                Ordering::Release,
            );
            crate::scheduler::activate_actor_for_test(&raw mut actor);
            assert_eq!(frame.resumes.load(Ordering::Acquire), 2);
            assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);
            assert_eq!((hooks.calls, hooks.drops), (0, 0));
            crate::scheduler::activate_actor_for_test(&raw mut actor);
            assert_eq!(
                actor.actor_state.load(Ordering::Acquire),
                crate::internal::types::HewActorState::Crashed as i32,
                "terminal cleanup publishes the original crash after its hook"
            );
            assert_eq!(actor.error_code.load(Ordering::Acquire), 91);
            assert!(actor.checked_invocation.load(Ordering::Acquire).is_null());
            assert!(actor.suspended_cont.load(Ordering::Acquire).is_null());
            assert_eq!(actor.pending_external_trap_code.load(Ordering::Acquire), 0);
            assert_eq!((hooks.calls, hooks.drops), (1, 1));
            assert_eq!(
                completion.crash_action(),
                Some(crate::supervisor::CRASH_ACTION_KILL)
            );
            super::finish_native_terminal(&actor);
            assert_eq!((hooks.calls, hooks.drops), (1, 1));
            crate::mailbox::hew_mailbox_free(mailbox);
        }
    }
}
