//! Completion and cancellation boundary for checked native coroutines.
//!
//! The execution owner retains this object until the LLVM frame is destroyed.
//! Operations retain its independent wake target, never this object or a frame
//! address. Result and fault out-pointers remain owned by the invocation's
//! caller; publishing a terminal status follows initialization of those slots.

use crate::cancel_token::{
    cancel_token_reason, hew_cancel_observe, hew_cancel_token_cancel,
    hew_cancel_token_is_requested, hew_cancel_token_new_child, hew_cancel_token_release,
    hew_cancel_token_retain, hew_cancel_unobserve, HewCancelObserver, HewCancellationToken,
};
use crate::wake::{HewWaker, OwnedWaker};
use std::sync::atomic::{AtomicI32, AtomicU32, Ordering};

/// Private token reason identifying cancellation requested by the reader of an
/// actor stream. The source-facing failure remains `HEW_FAULT_CANCELLED`.
const ACTOR_STREAM_READER_CLOSED_REASON: i32 = i32::MIN + 1;

/// ABI-visible coroutine outcome. A yielded value is available until the
/// generator's next resume; every terminal outcome requires frame destruction.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoroStatus {
    Pending = 0,
    Complete = 1,
    Fault = 2,
    Cancelled = 3,
    Yielded = 4,
}

impl CoroStatus {
    pub(crate) fn from_private_status(status: i32) -> Self {
        if status == 0 {
            Self::Complete
        } else if status == crate::fault::HEW_FAULT_CANCELLED {
            Self::Cancelled
        } else {
            Self::Fault
        }
    }
}

/// One local completion record, with the exact compiler-private C ABI.
#[repr(transparent)]
#[derive(Debug)]
pub struct HewCoroState {
    invocation: hew_cabi::coro::HewCoroInvocation,
}

/// Owned only at an execution or lexical cancellation boundary. Ordinary calls
/// borrow the suspended caller's context and keep their own completion record.
#[derive(Debug)]
pub(crate) struct InvocationContext {
    pub(crate) actor_turn: crate::lifetime::live_actors::ActorIncarnation,
    pub(crate) rc_owner: Option<std::sync::Arc<crate::rc::RcAllocations>>,
    pub(crate) actor_message_type: i32,
    pub(crate) actor_caller: crate::actor_caller::HewCallerStamp,
    pub(crate) request_deadline: crate::request_deadline::RequestDeadline,
    budget: std::sync::Arc<AtomicU32>,
    waker: OwnedWaker,
    token: *mut HewCancellationToken,
    observer: *mut HewCancelObserver,
    previous: *mut InvocationContext,
    pub(crate) actor_stream: Option<ActorStreamBinding>,
}

/// Retained cancellation token and wake target for an invocation that may be
/// cancelled by an owner other than the suspended invocation itself.
/// It never points into a coroutine frame or invocation state.
#[derive(Debug)]
pub(crate) struct RetainedCancellation {
    token: *mut HewCancellationToken,
    waker: OwnedWaker,
}

// SAFETY: the retained token and readiness descriptor support concurrent requests.
unsafe impl Send for RetainedCancellation {}

impl RetainedCancellation {
    pub(crate) fn new(state: &HewCoroState) -> Self {
        let execution = state.execution();
        // SAFETY: the live invocation retains this token through the increment.
        unsafe { hew_cancel_token_retain(execution.token) };
        Self {
            token: execution.token,
            waker: execution.waker.clone(),
        }
    }

    pub(crate) fn cancel(&self, reason: i32) {
        // SAFETY: this registration retains the independent construction token.
        unsafe { hew_cancel_token_cancel(self.token, reason) };
        self.waker.wake();
    }
}

impl Drop for RetainedCancellation {
    fn drop(&mut self) {
        // SAFETY: this registration owns one retained reference.
        unsafe { hew_cancel_token_release(self.token) };
    }
}

/// Cancellation association for one actor stream producer. The channel owns
/// this registration while open; the invocation also retains it until finish.
#[derive(Debug)]
pub(crate) struct ActorStreamCancellation {
    retained: RetainedCancellation,
    reader_closed: std::sync::atomic::AtomicBool,
}

// SAFETY: cancellation only touches atomics, the retained token, and waker.
unsafe impl Send for ActorStreamCancellation {}
// SAFETY: cancellation may be requested concurrently with invocation finish.
unsafe impl Sync for ActorStreamCancellation {}

impl ActorStreamCancellation {
    pub(crate) fn new(state: &HewCoroState) -> Self {
        Self {
            retained: RetainedCancellation::new(state),
            reader_closed: std::sync::atomic::AtomicBool::new(false),
        }
    }

    pub(crate) fn close_reader(&self) {
        self.reader_closed.store(true, Ordering::Release);
        self.retained.cancel(ACTOR_STREAM_READER_CLOSED_REASON);
    }

    pub(crate) fn reader_closed(&self) -> bool {
        self.reader_closed.load(Ordering::Acquire)
    }

    pub(crate) fn caused_cancellation(&self, state: &HewCoroState) -> bool {
        self.reader_closed()
            // SAFETY: `state` is live through actor completion; its token owns
            // the parent chain consulted by the reason lookup.
            && unsafe { cancel_token_reason(state.execution().token) }
                == ACTOR_STREAM_READER_CLOSED_REASON
    }
}

/// Invocation-owned half of a channel's producer-cancellation registration.
/// It keeps the core alive through completion and removes only this binding.
#[derive(Debug)]
pub(crate) struct ActorStreamBinding {
    pub(crate) core: std::sync::Arc<crate::channel_core::ChannelCore>,
    pub(crate) cancellation: std::sync::Arc<ActorStreamCancellation>,
}

#[repr(u8)]
enum CleanupFault {
    None,
    Local,
    Inherited,
}

#[repr(u8)]
enum CleanupIo {
    Normal,
    Abandon,
    InheritedAbandon,
}

impl Drop for InvocationContext {
    fn drop(&mut self) {
        if let Some(binding) = self.actor_stream.take() {
            binding.core.unbind_actor_stream(&binding.cancellation);
        }
        // SAFETY: this context owns its subscription and retained token.
        unsafe {
            hew_cancel_unobserve(self.observer);
            hew_cancel_token_release(self.token);
        }
    }
}

impl HewCoroState {
    pub(crate) fn bind_actor_stream(
        &mut self,
        core: std::sync::Arc<crate::channel_core::ChannelCore>,
    ) -> bool {
        if self.execution().actor_stream.is_some() {
            return false;
        }
        let cancellation = std::sync::Arc::new(ActorStreamCancellation::new(self));
        if !core.bind_actor_stream(std::sync::Arc::clone(&cancellation)) {
            return false;
        }
        self.execution_mut().actor_stream = Some(ActorStreamBinding { core, cancellation });
        true
    }

    fn with_context(context: *mut InvocationContext, flags: u8, poll_suppression: u32) -> Self {
        Self {
            invocation: hew_cabi::coro::HewCoroInvocation {
                context: context.cast(),
                status: AtomicI32::new(CoroStatus::Pending as i32),
                private_status: AtomicI32::new(1),
                poll_suppression,
                lexical_depth: 0,
                flags,
                cleanup_fault: std::sync::atomic::AtomicU8::new(CleanupFault::None as u8),
                cleanup_abandon_io: std::sync::atomic::AtomicU8::new(CleanupIo::Normal as u8),
            },
        }
    }

    pub(crate) fn execution(&self) -> &InvocationContext {
        // SAFETY: an initialized invocation owns its context or borrows it from
        // its suspended caller, which outlives this invocation.
        unsafe { &*self.invocation.context.cast::<InvocationContext>() }
    }

    pub(crate) fn execution_mut(&mut self) -> &mut InvocationContext {
        assert!(
            self.invocation.flags & hew_cabi::coro::OWNS_CONTEXT != 0
                || self.invocation.lexical_depth != 0
        );
        // SAFETY: this invocation owns the context and exclusively executes.
        unsafe { &mut *self.invocation.context.cast::<InvocationContext>() }
    }

    fn leave_context(&mut self) {
        assert_ne!(self.invocation.lexical_depth, 0);
        // SAFETY: every entered lexical context is exclusively owned by this
        // record; all child operations have finished before its scope leaves.
        let context = unsafe { Box::from_raw(self.invocation.context.cast::<InvocationContext>()) };
        self.invocation.context = context.previous.cast();
        self.invocation.lexical_depth -= 1;
        drop(context);
    }
}

impl Drop for HewCoroState {
    fn drop(&mut self) {
        while self.invocation.lexical_depth != 0 {
            self.leave_context();
        }
        if self.invocation.flags & hew_cabi::coro::OWNS_CONTEXT != 0 {
            // SAFETY: this record owns its base context and every nested frame
            // has been destroyed before the invocation is released.
            drop(unsafe { Box::from_raw(self.invocation.context.cast::<InvocationContext>()) });
        }
    }
}

/// Build a context from a transferred token reference and a retained wake target.
unsafe fn context_with_token(
    waker: *const HewWaker,
    token: *mut HewCancellationToken,
    budget: std::sync::Arc<AtomicU32>,
    previous: *mut InvocationContext,
) -> InvocationContext {
    // SAFETY: the caller keeps the descriptor and transferred token live.
    let waker = unsafe { OwnedWaker::retain(&*waker) };
    // SAFETY: the retained descriptor and token stay live in this context.
    let observer = unsafe { hew_cancel_observe(token, waker.descriptor()) };
    InvocationContext {
        actor_turn: crate::lifetime::live_actors::ActorIncarnation::NONE,
        rc_owner: crate::rc::current_allocations(),
        actor_message_type: 0,
        actor_caller: crate::actor_caller::HewCallerStamp::default(),
        request_deadline: crate::request_deadline::RequestDeadline::current(),
        budget,
        waker,
        token,
        observer,
        previous,
        actor_stream: None,
    }
}

unsafe fn new_state(
    waker: *const HewWaker,
    parent_token: *mut HewCancellationToken,
    budget: std::sync::Arc<AtomicU32>,
    poll_suppression: u32,
) -> *mut HewCoroState {
    if waker.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: the caller keeps the optional parent token live during creation.
    let token = unsafe { hew_cancel_token_new_child(parent_token) };
    // SAFETY: the new context owns token and retains the caller's wake target.
    let context = unsafe { context_with_token(waker, token, budget, std::ptr::null_mut()) };
    Box::into_raw(Box::new(HewCoroState::with_context(
        Box::into_raw(Box::new(context)),
        hew_cabi::coro::OWNS_CONTEXT | hew_cabi::coro::HEAP_ALLOCATED,
        poll_suppression,
    )))
}

/// Create an independently owned execution boundary.
///
/// # Safety
/// `waker` points to a live descriptor obeying `HewWaker`'s contract. `token` is
/// null or a live parent token. Destroy the LLVM frame before freeing the state.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_new(
    waker: *const HewWaker,
    token: *mut HewCancellationToken,
) -> *mut HewCoroState {
    // SAFETY: the caller keeps both inputs live during construction.
    unsafe { new_state(waker, token, std::sync::Arc::new(AtomicU32::new(0)), 0) }
}

/// Release an invocation record after its LLVM frame and operation handles.
///
/// # Safety
/// `state` is null or live, allocated by `hew_coro_state_new`/`cleanup_child` or
/// initialized in caller-owned storage by `hew_coro_state_init_child`. Release
/// exactly once, without concurrent access; borrowed parent context stays live.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_free(state: *mut HewCoroState) {
    if !state.is_null() {
        // SAFETY: the initialized record remains live until released here.
        if unsafe { (*state).invocation.flags } & hew_cabi::coro::HEAP_ALLOCATED != 0 {
            // SAFETY: the heap flag names a Box allocated by the runtime.
            drop(unsafe { Box::from_raw(state) });
        } else {
            // SAFETY: this is initialized caller storage, retained past drop.
            unsafe { std::ptr::drop_in_place(state) };
        }
    }
}

/// Initialize an ordinary invocation in the caller's LLVM frame.
///
/// # Safety
/// `storage` is writable and aligned for `HewCoroInvocation`, and uninitialized
/// or previously released. `parent` stays live and suspended throughout the
/// child invocation. Destroy the child's LLVM frame, then release its record,
/// before reusing this storage or resuming/releasing its parent.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_init_child(
    storage: *mut HewCoroState,
    parent: *const HewCoroState,
) {
    // SAFETY: the caller retains the suspended, initialized parent record.
    let parent = unsafe { &*parent };
    let child = HewCoroState::with_context(
        parent.invocation.context.cast(),
        0,
        parent.invocation.poll_suppression,
    );
    // SAFETY: the caller provides uninitialized, aligned frame storage.
    unsafe { storage.write(child) };
}

/// Initialize a checked actor construction in ordinary caller-owned storage.
/// Its allocation list is transferred unchanged when the actor is published.
///
/// # Safety
/// The storage and parent obey `hew_coro_state_init_child`'s lifetime contract.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_init_construction(
    storage: *mut HewCoroState,
    parent: *const HewCoroState,
) {
    // SAFETY: the suspended caller retains its execution context and token.
    let parent = unsafe { &*parent };
    let execution = parent.execution();
    // SAFETY: construction can be cancelled independently of its spawning caller.
    let token = unsafe { hew_cancel_token_new_child(execution.token) };
    // SAFETY: parent retains the descriptor through initialization.
    let mut context = unsafe {
        context_with_token(
            execution.waker.descriptor(),
            token,
            execution.budget.clone(),
            std::ptr::null_mut(),
        )
    };
    context.actor_turn = crate::lifetime::live_actors::ActorIncarnation::NONE;
    context.actor_message_type = execution.actor_message_type;
    context.actor_caller = execution.actor_caller;
    context.request_deadline = execution.request_deadline;
    context.rc_owner = Some(std::sync::Arc::default());
    let child = HewCoroState::with_context(
        Box::into_raw(Box::new(context)),
        hew_cabi::coro::OWNS_CONTEXT,
        parent.invocation.poll_suppression,
    );
    // SAFETY: the caller grants aligned, uninitialized invocation storage.
    unsafe { storage.write(child) };
}

/// Detach the next failed-construction survivor wave, or finish its owner.
/// Successful publication has already transferred the list and returns null.
///
/// # Safety
/// `state` is the exclusively executing construction invocation. Prior waves
/// and scoped children have drained before calling again.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_rc_sweep(
    state: *mut HewCoroState,
) -> *mut crate::release_walker::HewReleaseCursor {
    // SAFETY: the generated construction body owns this execution context.
    let execution = unsafe { &mut *state }.execution_mut();
    let Some(owner) = &execution.rc_owner else {
        return std::ptr::null_mut();
    };
    // SAFETY: ordinary body cleanup and scoped children have quiesced.
    if let Some(cursor) = unsafe { owner.sweep() } {
        cursor
    } else {
        execution.rc_owner = None;
        std::ptr::null_mut()
    }
}

/// Create a cleanup invocation with independent cancellation ancestry.
///
/// # Safety
/// `parent` is live through construction. Destroy the cleanup LLVM frame before
/// freeing its returned state, exactly as for an ordinary invocation.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_cleanup_child(
    parent: *const HewCoroState,
) -> *mut HewCoroState {
    // SAFETY: the caller retains the optional parent during construction.
    let Some(parent) = (unsafe { parent.as_ref() }) else {
        return std::ptr::null_mut();
    };
    // SAFETY: parent retains its descriptor; null starts fresh token ancestry.
    let mut state = unsafe {
        owned_child(
            parent,
            std::ptr::null_mut(),
            crate::request_deadline::RequestDeadline::default(),
        )
    };
    state.invocation.flags |= hew_cabi::coro::HEAP_ALLOCATED;
    let child = Box::into_raw(Box::new(state));
    // SAFETY: the new state and context are uniquely owned until returned.
    unsafe {
        (*child).invocation.cleanup_fault.store(
            if parent.invocation.cleanup_fault.load(Ordering::Acquire) == CleanupFault::None as u8 {
                CleanupFault::None as u8
            } else {
                CleanupFault::Inherited as u8
            },
            Ordering::Release,
        );
        if cleanup_abandons_io(parent) || hew_coro_state_is_cancelled(parent) != 0 {
            abandon_cleanup_io(child);
        }
    }
    child
}

/// A native operation can cancel its callback without cancelling the caller.
/// Its record lives in the operation, which destroys the frame before this state.
/// # Safety
/// The parent is live during construction and its wake target obeys the ABI.
pub(crate) unsafe fn operation_child(parent: &HewCoroState) -> HewCoroState {
    // SAFETY: the parent retains its token and context during construction.
    unsafe {
        owned_child(
            parent,
            parent.execution().token,
            parent.execution().request_deadline,
        )
    }
}

unsafe fn owned_child(
    parent: &HewCoroState,
    parent_token: *mut HewCancellationToken,
    deadline: crate::request_deadline::RequestDeadline,
) -> HewCoroState {
    let execution = parent.execution();
    // SAFETY: parent retains its token and wake target during child creation.
    let token = unsafe { hew_cancel_token_new_child(parent_token) };
    // SAFETY: the new context takes this token and retains the shared wake target.
    let mut context = unsafe {
        context_with_token(
            execution.waker.descriptor(),
            token,
            execution.budget.clone(),
            std::ptr::null_mut(),
        )
    };
    context.actor_turn = execution.actor_turn;
    context.rc_owner.clone_from(&execution.rc_owner);
    context.actor_message_type = execution.actor_message_type;
    context.actor_caller = execution.actor_caller;
    context.request_deadline = deadline;
    HewCoroState::with_context(
        Box::into_raw(Box::new(context)),
        hew_cabi::coro::OWNS_CONTEXT,
        parent.invocation.poll_suppression,
    )
}

/// Retain the fault-disclosure state of one consuming cleanup invocation.
/// The diagnostic remains borrowed; freeing the invocation ends this scope.
/// # Safety
/// Both pointers are null or live through this call.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_set_cleanup_fault(
    state: *const HewCoroState,
    fault: *const crate::fault::HewFault,
) {
    // SAFETY: the caller retains the optional invocation and diagnostic.
    if let Some(state) = unsafe { state.as_ref() } {
        // SAFETY: the caller retains this optional diagnostic for this call.
        let failed = unsafe { fault.as_ref() }.is_some_and(|fault| {
            !matches!(
                fault.code(),
                crate::fault::HEW_FAULT_CANCELLED
                    | crate::fault::HEW_FAULT_DEADLINE
                    | crate::fault::HEW_FAULT_RACE_LOST
            )
        });
        // A nested release's empty local fault slot cannot erase the fault
        // whose cleanup created it. The parent owns and eventually clears that
        // fault; this shielded invocation retains its inherited disposition.
        if state.invocation.cleanup_fault.load(Ordering::Acquire) != CleanupFault::Inherited as u8 {
            state.invocation.cleanup_fault.store(
                if failed {
                    CleanupFault::Local as u8
                } else {
                    CleanupFault::None as u8
                },
                Ordering::Release,
            );
        }
        if state.invocation.cleanup_abandon_io.load(Ordering::Acquire)
            != CleanupIo::InheritedAbandon as u8
        {
            state.invocation.cleanup_abandon_io.store(
                if fault.is_null() {
                    CleanupIo::Normal as u8
                } else {
                    CleanupIo::Abandon as u8
                },
                Ordering::Release,
            );
        }
    }
}

pub(crate) unsafe fn cleanup_fault_owner(state: *const HewCoroState) -> Option<u64> {
    // SAFETY: this optional state is retained by the consuming callback.
    let state = unsafe { state.as_ref() }?;
    let id = state.execution().actor_turn.actor_id();
    (id != 0 && state.invocation.cleanup_fault.load(Ordering::Acquire) != CleanupFault::None as u8)
        .then_some(id)
}

/// # Safety
/// The optional cleanup invocation stays live while its disposition is set.
pub(crate) unsafe fn abandon_cleanup_io(state: *const HewCoroState) {
    // SAFETY: the caller retains the cleanup state for this invocation.
    if let Some(state) = unsafe { state.as_ref() } {
        state
            .invocation
            .cleanup_abandon_io
            .store(CleanupIo::InheritedAbandon as u8, Ordering::Release);
    }
}

/// # Safety
/// The optional cleanup invocation stays live during inspection.
pub(crate) unsafe fn cleanup_abandons_io(state: *const HewCoroState) -> bool {
    // SAFETY: the caller retains this optional invocation.
    unsafe { state.as_ref() }.is_some_and(|state| {
        state.invocation.cleanup_abandon_io.load(Ordering::Acquire) != CleanupIo::Normal as u8
    })
}

/// Request cancellation of an independently owned execution boundary.
/// Ordinary nested calls inherit their parent's cancellation authority.
///
/// # Safety
/// `state` is live and was created by `hew_coro_state_new` or
/// `hew_coro_state_cleanup_child`, never `hew_coro_state_init_child`.
/// The caller excludes concurrent execution and teardown, keeping its current
/// lexical context stable. External cancellers use the retained token instead.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_cancel(state: *const HewCoroState) {
    if !state.is_null() {
        // SAFETY: the caller keeps the state live through this call.
        if unsafe { (*state).invocation.flags } & hew_cabi::coro::OWNS_CONTEXT == 0 {
            std::process::abort();
        }
        // SAFETY: the caller keeps the state and its owned token live.
        unsafe { hew_cancel_token_cancel((*state).execution().token, 1) };
    }
}

/// Borrow the invocation's token when creating a nested structured scope.
///
/// # Safety
/// `state` must remain live for the returned borrow. The scope must retain the
/// token or create its child before returning control to the caller.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_token(
    state: *const HewCoroState,
) -> *mut HewCancellationToken {
    if state.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: the caller keeps the state live for this borrow.
    unsafe { (*state).execution().token }
}

/// Capture the incoming request budget and explicit enclosing scope deadlines.
/// Expiry constrains outgoing calls without cancelling the receiving actor.
/// # Safety
/// `state` remains live through this call.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_request_deadline(state: *const HewCoroState) -> i64 {
    // SAFETY: the caller retains the state and its token ancestry.
    let state = unsafe { &*state };
    // SAFETY: the invocation retains the active lexical token and its ancestry.
    let lexical = unsafe { crate::cancel_token::token_deadline(state.execution().token) };
    state.execution().request_deadline.earlier(lexical).raw()
}

/// Enter a lexical cancellation boundary for this invocation's operations.
///
/// # Safety
/// State is exclusively borrowed by its executing frame. Token is a live
/// descendant of the current token. Every entry is paired with a leave after
/// the scope's children and operation producers have drained.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_enter_token(
    state: *mut HewCoroState,
    token: *mut HewCancellationToken,
) {
    // SAFETY: the executing frame exclusively owns its invocation record.
    let state = unsafe { &mut *state };
    let previous = state.invocation.context.cast::<InvocationContext>();
    let execution = state.execution();
    // SAFETY: the scope supplies a live token, retained by this new context.
    unsafe { hew_cancel_token_retain(token) };
    // SAFETY: the previous context stays live until this scope has left.
    let mut context = unsafe {
        context_with_token(
            execution.waker.descriptor(),
            token,
            execution.budget.clone(),
            previous,
        )
    };
    context.actor_turn = execution.actor_turn;
    context.rc_owner.clone_from(&execution.rc_owner);
    context.actor_message_type = execution.actor_message_type;
    context.actor_caller = execution.actor_caller;
    context.request_deadline = execution.request_deadline;
    state.invocation.context = Box::into_raw(Box::new(context)).cast();
    state.invocation.lexical_depth += 1;
}

/// Restore the enclosing execution context after lexical cleanup.
///
/// # Safety
/// State is exclusively executing with a matching token entry. Its scope's
/// children and operations have drained before this boundary is released.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_leave_token(state: *mut HewCoroState) {
    // SAFETY: the executing frame owns the record and its lexical context.
    unsafe { &mut *state }.leave_context();
}

/// Borrow the readiness descriptor for registration of an operation.
///
/// # Safety
/// `state` must be live. The returned pointer is borrowed until state teardown;
/// an operation must retain the descriptor before returning to its caller.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_waker(state: *const HewCoroState) -> *const HewWaker {
    if state.is_null() {
        return std::ptr::null();
    }
    // SAFETY: caller keeps state live for the borrow.
    unsafe { &*state }.execution().waker.descriptor()
}

/// Read the retained cancellation token at a compiler-authored safepoint.
///
/// # Safety
/// `state` must be a live invocation state.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_is_cancelled(state: *const HewCoroState) -> i32 {
    if state.is_null() {
        return 1;
    }
    // SAFETY: the owned or borrowed context keeps its token ancestry live.
    let token = unsafe { &*state }.execution().token;
    if token.is_null() {
        0
    } else {
        // SAFETY: the current execution context retains this non-null token.
        unsafe { hew_cancel_token_is_requested(token) }
    }
}

/// Classify cancellation at the current lexical boundary for fault transport.
///
/// # Safety
/// `state` is a live invocation state. Call after observing cancellation, or
/// while cooperatively destroying its frame.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_cancel_code(state: *const HewCoroState) -> i32 {
    // SAFETY: the live state's retained token owns its complete ancestry.
    let reason = unsafe { cancel_token_reason((*state).execution().token) };
    // The actor's existing latch is the authority for an external construction fault.
    // SAFETY: the caller retains this live invocation throughout the query.
    let target = unsafe { &*state }.execution().actor_turn;
    let external = crate::lifetime::live_actors::with_live_incarnation(target, |actor| {
        // SAFETY: the send guard pins this incarnation.
        actor
            .actor()
            .pending_external_trap_code
            .load(Ordering::Acquire)
    })
    .unwrap_or(0);
    if external != 0 {
        return external;
    }
    match reason {
        crate::fault::HEW_FAULT_DEADLINE | crate::fault::HEW_FAULT_RACE_LOST => reason,
        _ => crate::fault::HEW_FAULT_CANCELLED,
    }
}

/// Read the outcome after a ramp/resume returns to its execution owner.
///
/// # Safety
/// `state` must be a live invocation state. Terminal status publishes the
/// caller-owned result/fault slots, which must remain live for this invocation.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_status(state: *const HewCoroState) -> i32 {
    if state.is_null() {
        return -1;
    }
    // SAFETY: state is live; Acquire observes preceding result writes.
    unsafe { &*state }.invocation.status.load(Ordering::Acquire)
}

/// Publish a checked function's existing private status after writing its
/// result or owned fault output. Returns 0 on repeated completion.
///
/// # Safety
/// The caller must exclusively own execution of a live `state`, and have
/// initialized its result (zero status) or owned fault (non-zero status).
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_finish(
    state: *mut HewCoroState,
    private_status: i32,
) -> i32 {
    if state.is_null() {
        return 0;
    }
    // SAFETY: the caller owns execution and keeps state live.
    let state_ref = unsafe { &*state };
    if state_ref.invocation.status.load(Ordering::Acquire) != CoroStatus::Pending as i32 {
        return 0;
    }
    state_ref
        .invocation
        .private_status
        .store(private_status, Ordering::Relaxed);
    // SAFETY: output initialization and exclusive execution are caller duties.
    unsafe {
        hew_coro_state_publish(
            state,
            CoroStatus::from_private_status(private_status) as i32,
        )
    }
}

/// Recover the checked private status after observing terminal completion.
///
/// # Safety
/// `state` must be live and its ramp/resume must have returned terminally.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_private_status(state: *const HewCoroState) -> i32 {
    if state.is_null() {
        return 1;
    }
    // SAFETY: caller retains state and observes it only after execution returns.
    let state = unsafe { &*state };
    match state.invocation.status.load(Ordering::Acquire) {
        status if status == CoroStatus::Complete as i32 => 0,
        status if status == CoroStatus::Fault as i32 || status == CoroStatus::Cancelled as i32 => {
            state.invocation.private_status.load(Ordering::Relaxed)
        }
        _ => 1,
    }
}

/// Publish a yielded value or a terminal result after initializing its output.
/// Returns 0 for an invalid status or a second terminal publication. A generator
/// must reset Yielded to Pending before executing another resume.
///
/// # Safety
/// `state` must be live and the caller must own execution of its frame. The
/// corresponding output must be initialized before publication and cannot be
/// accessed concurrently before the owner's ramp/resume has returned.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_publish(state: *mut HewCoroState, status: i32) -> i32 {
    if state.is_null() || !(1..=4).contains(&status) {
        return 0;
    }
    // SAFETY: caller owns execution; CAS rejects repeated publication.
    i32::from(
        unsafe { &*state }
            .invocation
            .status
            .compare_exchange(
                CoroStatus::Pending as i32,
                status,
                Ordering::Release,
                Ordering::Relaxed,
            )
            .is_ok(),
    )
}

/// Reset a consumed generator yield before resuming its frame.
///
/// # Safety
/// `state` must be live, with exclusive execution ownership. Its yielded value
/// must have been taken or destroyed before this call.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_state_resume_yield(state: *mut HewCoroState) -> i32 {
    if state.is_null() {
        return 0;
    }
    // SAFETY: caller owns the yield and the next resume.
    i32::from(
        unsafe { &*state }
            .invocation
            .status
            .compare_exchange(
                CoroStatus::Yielded as i32,
                CoroStatus::Pending as i32,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok(),
    )
}

/// Borrow the invocation chain's countdown for generated safepoints.
/// A deferred invocation returns null and runs through without polling.
///
/// # Safety
/// The state and its retained budget outlive every use of the returned pointer.
/// Only the currently executing invocation in this serial call chain may read
/// or write the countdown, including after migration to another worker.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_safepoint_budget(
    state: *mut HewCoroState,
    budget: u32,
) -> *mut u32 {
    // SAFETY: the invocation owner retains state throughout execution.
    let state = unsafe { &*state };
    if state.invocation.poll_suppression != 0 {
        return std::ptr::null_mut();
    }
    if state.execution().budget.load(Ordering::Relaxed) == 0 {
        state.execution().budget.store(budget, Ordering::Relaxed);
    }
    state.execution().budget.as_ptr()
}

/// Poll an exhausted compiler-authored countdown. Returns continue, yield or cancel.
/// The execution chain shares its budget, including recursive calls.
///
/// # Safety
/// `state` is live and exclusively executing on the current worker.
/// `budget` is the positive scheduling budget supplied by the compiler.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_safepoint(state: *mut HewCoroState, budget: u32) -> i32 {
    // SAFETY: the invocation owner keeps the state live during execution.
    let state = unsafe { &*state };
    if state.invocation.poll_suppression != 0 {
        return 0;
    }
    state.execution().budget.store(budget, Ordering::Relaxed);
    crate::driver::safepoint_tick();
    crate::actor_native::poll_turn_stop();
    // SAFETY: state retains the current lexical cancellation ancestry.
    if unsafe { hew_coro_state_is_cancelled(state) } != 0 {
        return 2;
    }
    #[cfg(not(target_arch = "wasm32"))]
    let runnable = crate::scheduler::has_runnable_work();
    #[cfg(target_arch = "wasm32")]
    let runnable = crate::driver::has_queued_work();
    if runnable {
        state.execution().waker.wake();
        return 1;
    }
    0
}

/// Enter a non-suspending deferred region; child invocations inherit suppression.
///
/// # Safety
/// `state` is live and exclusively executing; every enter has one matching leave.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_safepoint_suppress(state: *mut HewCoroState) {
    // SAFETY: the caller exclusively owns execution of the state.
    unsafe {
        (*state).invocation.poll_suppression += 1;
    }
}

/// Leave a non-suspending deferred region.
///
/// # Safety
/// `state` is live, exclusively executing, and has an unmatched suppression enter.
#[no_mangle]
pub unsafe extern "C" fn hew_coro_safepoint_restore(state: *mut HewCoroState) {
    // SAFETY: the caller pairs this with the region's enter on every exit.
    unsafe {
        (*state).invocation.poll_suppression -= 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::wake::blocking::Readiness;

    #[test]
    fn lexical_cancellation_reaches_nested_calls_then_restores_parent() {
        let (ready, waker) = Readiness::new();
        // SAFETY: locally owned invocation and lexical tokens have balanced
        // references; nested calls stop before their enclosing scope is left.
        unsafe {
            let root = hew_coro_state_new(waker.descriptor(), std::ptr::null_mut());
            let mut state_storage = std::mem::MaybeUninit::uninit();
            let state = state_storage.as_mut_ptr();
            hew_coro_state_init_child(state, root);
            let parent = hew_coro_state_token(state);
            let lexical = hew_cancel_token_new_child(parent);
            hew_coro_state_enter_token(state, lexical);
            let mut child_storage = std::mem::MaybeUninit::uninit();
            let child = child_storage.as_mut_ptr();
            hew_coro_state_init_child(child, state);
            hew_cancel_token_cancel(lexical, crate::fault::HEW_FAULT_DEADLINE);
            hew_cancel_token_cancel(lexical, 1);
            assert!(ready.take_ready());
            assert_eq!(hew_coro_state_is_cancelled(state), 1);
            assert_eq!(hew_coro_state_is_cancelled(child), 1);
            assert_eq!(
                hew_coro_state_cancel_code(child),
                crate::fault::HEW_FAULT_DEADLINE
            );
            assert_eq!(hew_cancel_token_is_requested(parent), 0);
            hew_coro_state_free(child);
            hew_coro_state_leave_token(state);
            hew_cancel_token_release(lexical);
            assert_eq!(hew_coro_state_is_cancelled(state), 0);
            hew_cancel_token_cancel(parent, 1);
            assert!(ready.take_ready());
            assert_eq!(hew_coro_state_is_cancelled(state), 1);
            assert_eq!(
                hew_coro_state_cancel_code(state),
                crate::fault::HEW_FAULT_CANCELLED
            );
            hew_coro_state_free(state);
            hew_coro_state_free(root);
        }
    }

    #[test]
    fn expired_lexical_deadline_is_observed_before_its_timer_wake() {
        let (_, waker) = Readiness::new();
        // SAFETY: the test owns every state and token and releases children first.
        unsafe {
            let parent = hew_coro_state_new(waker.descriptor(), std::ptr::null_mut());
            (*parent).execution_mut().request_deadline =
                crate::request_deadline::RequestDeadline::from_raw(0);
            assert_eq!(hew_coro_state_is_cancelled(parent), 0);
            let lexical = hew_cancel_token_new_child(hew_coro_state_token(parent));
            (*lexical).deadline_ns.store(0, Ordering::Release);
            hew_coro_state_enter_token(parent, lexical);
            let mut child_storage = std::mem::MaybeUninit::uninit();
            let child = child_storage.as_mut_ptr();
            hew_coro_state_init_child(child, parent);
            assert_eq!(hew_coro_state_is_cancelled(child), 1);
            assert_eq!(
                hew_coro_state_cancel_code(child),
                crate::fault::HEW_FAULT_DEADLINE
            );
            hew_cancel_token_cancel(lexical, crate::fault::HEW_FAULT_CANCELLED);
            assert_eq!(
                hew_coro_state_cancel_code(child),
                crate::fault::HEW_FAULT_DEADLINE
            );
            hew_coro_state_free(child);
            hew_coro_state_leave_token(parent);
            hew_cancel_token_release(lexical);
            assert_eq!(hew_coro_state_is_cancelled(parent), 0);
            hew_coro_state_free(parent);
        }
    }

    #[test]
    fn cancelled_invocation_keeps_token_alive_and_late_wake_survives_teardown() {
        let (ready, waker) = Readiness::new();
        // SAFETY: all invocation handles are locally owned; the operation
        // retains the independent wake descriptor before state teardown.
        unsafe {
            let token = hew_cancel_token_new_child(std::ptr::null_mut());
            let state = hew_coro_state_new(waker.descriptor(), token);
            let operation = OwnedWaker::retain(&*hew_coro_state_waker(state));
            hew_cancel_token_cancel(token, 1);
            hew_cancel_token_release(token);
            assert!(ready.take_ready());
            assert_eq!(hew_coro_state_is_cancelled(state), 1);
            assert_eq!(
                hew_coro_state_publish(state, CoroStatus::Cancelled as i32),
                1
            );
            hew_coro_state_free(state);
            operation.wake();
            assert!(ready.take_ready());
        }
    }

    #[test]
    fn independent_boundaries_cancel_separately_and_nested_calls_inherit() {
        let (ready, waker) = Readiness::new();
        // SAFETY: the test owns all invocation states and releases children first.
        unsafe {
            let parent = hew_coro_state_new(waker.descriptor(), std::ptr::null_mut());
            let first = hew_coro_state_new(waker.descriptor(), hew_coro_state_token(parent));
            let second = hew_coro_state_new(waker.descriptor(), hew_coro_state_token(parent));
            let mut grandchild_storage = std::mem::MaybeUninit::uninit();
            let grandchild = grandchild_storage.as_mut_ptr();
            hew_coro_state_init_child(grandchild, first);
            hew_coro_state_cancel(first);
            assert!(ready.take_ready());
            assert_eq!(hew_coro_state_is_cancelled(first), 1);
            assert_eq!(hew_coro_state_is_cancelled(grandchild), 1);
            assert_eq!(hew_coro_state_is_cancelled(parent), 0);
            assert_eq!(hew_coro_state_is_cancelled(second), 0);
            hew_coro_state_free(grandchild);
            hew_coro_state_free(first);
            hew_coro_state_cancel(parent);
            assert!(ready.take_ready());
            assert_eq!(hew_coro_state_is_cancelled(second), 1);
            hew_coro_state_free(second);
            hew_coro_state_free(parent);
        }
    }

    #[test]
    fn terminal_completion_cannot_be_republished_or_resumed_as_a_yield() {
        let (_, waker) = Readiness::new();
        // SAFETY: locally owned invocation and valid descriptor.
        unsafe {
            let state = hew_coro_state_new(waker.descriptor(), std::ptr::null_mut());
            assert_eq!(hew_coro_state_publish(state, CoroStatus::Yielded as i32), 1);
            assert_eq!(
                hew_coro_state_publish(state, CoroStatus::Complete as i32),
                0
            );
            assert_eq!(hew_coro_state_resume_yield(state), 1);
            assert_eq!(
                hew_coro_state_publish(state, CoroStatus::Complete as i32),
                1
            );
            assert_eq!(hew_coro_state_resume_yield(state), 0);
            assert_eq!(hew_coro_state_publish(state, CoroStatus::Fault as i32), 0);
            assert_eq!(hew_coro_state_status(state), CoroStatus::Complete as i32);
            hew_coro_state_free(state);
        }
    }
}
