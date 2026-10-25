//! Checked structured tasks driven by the shared scheduler.
//!
//! The scope, source handle and worker own independent task references. A
//! terminal result is published only after the callable driver has destroyed
//! its frame; task observers retain readiness targets independently of frames.

use super::{
    free_scope_tasks, hew_cancel_token_cancel, hew_cancel_token_new_child, hew_task_new,
    hew_task_scope_spawn, release_task_ref, HewCancellationToken, HewTask, HewTaskScope,
};
use crate::callable::HewCallableValue;
#[path = "task_scope_executor.rs"]
mod executor;
#[path = "task_scope_group.rs"]
mod group;
use crate::coro_state::{hew_coro_state_free, hew_coro_state_new, HewCoroState};
use crate::fault::{hew_fault_combine, hew_fault_drop, HewFault, HEW_FAULT_CANCELLED};
use crate::release_walker::{HewReleaseCursor, ReleaseDriver};
use crate::util::MutexExt;
use crate::wake::{HewWaker, OwnedWaker};
pub(crate) use executor::TaskExecution;
use hew_cabi::value::HewValueLayout;
use std::alloc::{alloc, dealloc, handle_alloc_error, Layout};
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Weak;
use std::sync::{Arc, Mutex};

#[path = "task_scope_select.rs"]
mod select;
pub use select::{
    hew_checked_task_select_add_actor, hew_checked_task_select_add_stream,
    hew_checked_task_select_add_task, hew_checked_task_select_arm_timer,
    hew_checked_task_select_fault, hew_checked_task_select_free, hew_checked_task_select_new,
    hew_checked_task_select_poll, hew_checked_task_select_poll_first,
    hew_checked_task_select_set_context, HewCheckedTaskSelect,
};

const PENDING: i32 = 0;
const READY: i32 = 1;
const FAULT: i32 = 2;
const CANCELLED: i32 = 3;
const TAKEN: i32 = 4;
static COMPLETION_ORDER: AtomicU64 = AtomicU64::new(1);

#[derive(Clone, Copy, PartialEq, Eq)]
enum TaskAccounting {
    Scope,
    Group,
}

pub(super) struct CheckedTaskState {
    invocation: Option<CheckedTaskInvocation>,
    accounting: TaskAccounting,
    layout: *const HewValueLayout,
    provider_layout: *const HewValueLayout,
    value_owner: Option<crate::owned_value_budget::ValueOwner>,
    execution: Weak<TaskExecution>,
    allocation: Option<Layout>,
    result: *mut c_void,
    initialized: bool,
    started: bool,
    completed: bool,
    taken: bool,
    status: i32,
    fault: *mut HewFault,
    order: u64,
    waiters: Vec<Weak<TaskWake>>,
    _result_code: crate::code_owner::CodeRoot,
    _provider_code: crate::code_owner::CodeRoot,
}

// SAFETY: the compiler establishes Send for captures and the result. A mutex
// protects publication and consumption; immutable generated layouts outlive tasks.
unsafe impl Send for CheckedTaskState {}

impl CheckedTaskState {
    fn outcome(&self) -> i32 {
        if !self.completed {
            PENDING
        } else if self.taken {
            TAKEN
        } else if self.status == 0 {
            READY
        } else if self.status == HEW_FAULT_CANCELLED {
            CANCELLED
        } else {
            FAULT
        }
    }
}

impl Drop for CheckedTaskState {
    fn drop(&mut self) {
        // The scope barrier owns every consuming callback. Final handle drop
        // reclaims only raw result storage and an unobserved fault diagnostic.
        assert!(
            self.invocation.is_none(),
            "task invocation must consume its captures"
        );
        // SAFETY: the final task owner has exclusive access to raw storage.
        unsafe {
            assert!(
                !self.initialized,
                "task result must be consumed before its final storage owner"
            );
            if let Some(allocation) = self.allocation {
                dealloc(self.result.cast(), allocation);
            }
            hew_fault_drop(self.fault);
        }
    }
}

#[derive(Debug)]
struct TaskWake {
    waker: OwnedWaker,
    armed: AtomicBool,
}

/// One disarmable observation, retaining a source task handle until released.
#[derive(Debug)]
pub struct HewCheckedTaskWait {
    task: *mut HewTask,
    wake: Option<Arc<TaskWake>>,
    owns_account_edge: bool,
}

impl Drop for HewCheckedTaskWait {
    fn drop(&mut self) {
        if let Some(wake) = &self.wake {
            wake.armed.store(false, Ordering::Release);
        }
        if self.owns_account_edge {
            // SAFETY: a consuming observation owns the source handle's edge.
            unsafe { release_account(self.task) };
        }
        // SAFETY: this operation owns one task reference independently of scope.
        unsafe { release_task_ref(self.task) };
    }
}

/// One scope drain retains every child through cooperative result disposal.
#[derive(Debug)]
pub struct HewCheckedScopeWait {
    scope: *mut HewTaskScope,
    tasks: Vec<HewCheckedTaskWait>,
    cancellation: ScopeCancellation,
    subset: bool,
    close: Mutex<ScopeResultClose>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScopeCancellation {
    Ordinary,
    RaceLosers,
}

#[derive(Debug)]
struct ScopeResultClose {
    state: *mut HewCoroState,
    driver: Option<Box<ReleaseDriver>>,
    collected: bool,
    complete: bool,
    fault: *mut HewFault,
}

impl Drop for ScopeResultClose {
    fn drop(&mut self) {
        // An active collector borrows task result storage. Its owners cannot
        // be released until the retained readiness target reports quiescence.
        if self.collected && !self.complete {
            std::process::abort();
        }
        // SAFETY: no collector borrows remain; this drain owns state and fault.
        unsafe {
            hew_coro_state_free(self.state);
            hew_fault_drop(self.fault);
        }
    }
}

unsafe fn retain(task: *mut HewTask) {
    // SAFETY: caller retains a live reference throughout this increment.
    let previous = unsafe { &*task }.refs.fetch_add(1, Ordering::Relaxed);
    if previous > isize::MAX as usize {
        std::process::abort();
    }
}

unsafe fn checked<'a>(task: *mut HewTask) -> &'a Mutex<CheckedTaskState> {
    // SAFETY: all callers bound this internal borrow by an owning task reference.
    unsafe { &*task }
        .checked
        .as_ref()
        .expect("checked task contract")
}

unsafe fn complete(task: *mut HewTask, status: i32, fault: *mut HewFault) {
    // SAFETY: the executor retains its task until after frame destruction.
    unsafe {
        let waiters = {
            let mut state = checked(task).lock_or_recover();
            if status == 0 && state.layout.is_null() {
                std::process::abort();
            }
            state.initialized = status == 0;
            state.fault = fault;
            state.status = status;
            state.completed = true;
            state.order = COMPLETION_ORDER.fetch_add(1, Ordering::Relaxed);
            std::mem::take(&mut state.waiters)
        };
        for waiter in waiters.into_iter().filter_map(|waiter| waiter.upgrade()) {
            if waiter.armed.load(Ordering::Acquire) {
                waiter.waker.wake();
            }
        }
    }
}

/// Create a scope with explicit cancellation ancestry.
///
/// # Safety
/// `parent` is null or a live token. Close this scope only after a completed
/// checked drain; never use legacy forced-cancellation APIs for checked tasks.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_new(
    parent: *mut HewCancellationToken,
) -> *mut HewTaskScope {
    // SAFETY: constructor takes a reference to the caller's live parent token.
    let cancel_token = unsafe { hew_cancel_token_new_child(parent) };
    Box::into_raw(Box::new(HewTaskScope {
        tasks: ptr::null_mut(),
        cancel_token,
        checked_deadline: ptr::null_mut(),
        rc_owner: crate::rc::current_allocations(),
        value_owner: crate::owned_value_budget::current_owner(),
    }))
}

/// Bind independently scheduled children to their invocation's allocation owner.
/// # Safety
/// Both pointers are live and exclusively owned before any task is submitted.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_set_rc_owner(
    scope: *mut HewTaskScope,
    state: *const crate::coro_state::HewCoroState,
) {
    // SAFETY: construction of this lexical scope has not published children.
    unsafe {
        (*scope).rc_owner.clone_from(&(*state).execution().rc_owner);
        (*scope)
            .value_owner
            .clone_from(&(*state).execution().value_owner);
    }
}

unsafe extern "C" fn deadline_wake(token: *mut c_void) {
    // SAFETY: the timer retains this token independently of the scope frame.
    unsafe { hew_cancel_token_cancel(token.cast(), crate::fault::HEW_FAULT_DEADLINE) };
}

unsafe extern "C" fn deadline_retain(token: *mut c_void) {
    // SAFETY: retaining a live token is thread-safe.
    unsafe { super::hew_cancel_token_retain(token.cast()) };
}

unsafe extern "C" fn deadline_release(token: *mut c_void) {
    // SAFETY: callback ownership transfers exactly one retained reference.
    unsafe { super::hew_cancel_token_release(token.cast()) };
}

/// Arm this lexical scope's deadline on the shared timer wheel.
///
/// # Safety
/// Scope is live, exclusively accessed and has no existing checked deadline.
/// Its close path must drain all child frames before releasing the scope.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_deadline(scope: *mut HewTaskScope, duration_ns: i64) {
    // SAFETY: scope owns its token and receives sole ownership of the timer.
    unsafe {
        let scope = &mut *scope;
        if !scope.checked_deadline.is_null() {
            std::process::abort();
        }
        let deadline = crate::request_deadline::RequestDeadline::after(duration_ns);
        (*scope.cancel_token)
            .deadline_ns
            .store(deadline.raw(), Ordering::Release);
        let waker = HewWaker {
            context: scope.cancel_token.cast(),
            wake: deadline_wake,
            retain: deadline_retain,
            release: deadline_release,
        };
        scope.checked_deadline =
            crate::coro_sleep::hew_coro_sleep_until_new(deadline.raw(), &raw const waker);
        if crate::coro_sleep::hew_coro_sleep_status(scope.checked_deadline)
            != crate::coro_state::CoroStatus::Pending as i32
        {
            // An already expired deadline or unavailable timer must prevent
            // the body from running without its promised cancellation bound.
            deadline_wake(waker.context);
        }
    }
}

enum CheckedTaskInvocation {
    Callable {
        code: crate::code_owner::CodeRoot,
        value: HewCallableValue,
        arguments: *const *mut c_void,
        consuming: bool,
        slot_bytes: u64,
    },
    Race {
        members: Vec<*mut HewTask>,
        slot_bytes: u64,
    },
}

impl CheckedTaskInvocation {
    fn carrier_storage(&self) -> crate::owned_value_budget::ManagedStorage {
        let (identity, bytes) = match self {
            Self::Callable {
                value, slot_bytes, ..
            } => (ptr::from_ref(value) as usize, *slot_bytes),
            Self::Race {
                members,
                slot_bytes,
            } => (
                members.as_ptr() as usize,
                slot_bytes
                    .checked_mul(members.capacity() as u64)
                    .unwrap_or_else(|| std::process::abort()),
            ),
        };
        crate::owned_value_budget::ManagedStorage { identity, bytes }
    }

    unsafe fn visit_managed(
        &self,
        visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    ) -> i32 {
        use crate::owned_value_budget::visit::{hew_managed_visit_callable, storage};
        use hew_cabi::managed::{DESCEND, HEAP_EXCEEDED, SKIP_DESCENDANTS};
        // SAFETY: the task state lock keeps the pending invocation immutable.
        unsafe {
            match self {
                Self::Callable {
                    value,
                    consuming,
                    slot_bytes,
                    ..
                } => {
                    match storage(visitor, ptr::from_ref(value).cast(), *slot_bytes) {
                        SKIP_DESCENDANTS => return 0,
                        DESCEND => {}
                        failure => return failure,
                    }
                    if *consuming {
                        hew_managed_visit_callable(ptr::from_ref(value).cast(), visitor)
                    } else {
                        0
                    }
                }
                Self::Race {
                    members,
                    slot_bytes,
                } => {
                    let Some(bytes) = slot_bytes.checked_mul(members.capacity() as u64) else {
                        return HEAP_EXCEEDED;
                    };
                    match storage(visitor, members.as_ptr().cast(), bytes) {
                        SKIP_DESCENDANTS => return 0,
                        DESCEND => {}
                        failure => return failure,
                    }
                    for member in members {
                        let status = hew_managed_visit_task(ptr::from_ref(member).cast(), visitor);
                        if status != 0 {
                            return status;
                        }
                    }
                    0
                }
            }
        }
    }
}

/// Visit a retained task's real result allocation and pending owning inputs.
///
/// # Safety
/// `slot` contains a live checked task handle. The synchronous visitor remains
/// live; it must not mutate the task or call account APIs while visiting it.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_task(
    slot: *const c_void,
    visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
) -> i32 {
    use crate::owned_value_budget::visit::{slot_bytes, storage};
    use hew_cabi::managed::{visit_value, DESCEND, SKIP_DESCENDANTS};
    // SAFETY: the owning handle retains the task and its typed metadata.
    unsafe {
        let task = slot.cast::<*mut HewTask>().read();
        if task.is_null() {
            return 0;
        }
        match storage(visitor, task.cast(), 0) {
            SKIP_DESCENDANTS => return 0,
            DESCEND => {}
            failure => return failure,
        }
        use crate::owned_value_budget::transfer::{
            destination_owner, transfer_identity, transfer_waker,
        };
        use hew_cabi::managed::{HewManagedWalkKind, TRANSFER_PENDING};
        let kind = (*visitor).kind;
        let transfer = matches!(
            kind,
            HewManagedWalkKind::PrepareTransfer
                | HewManagedWalkKind::CommitTransfer
                | HewManagedWalkKind::AbortTransfer
        );
        let identity = if transfer {
            transfer_identity(visitor) as usize
        } else {
            0
        };
        let execution = checked(task).lock_or_recover().execution.upgrade();
        if let Some(execution) = &execution {
            if kind == HewManagedWalkKind::PrepareTransfer
                && !execution.freeze(identity, transfer_waker(visitor))
            {
                return TRANSFER_PENDING;
            }
            if kind == HewManagedWalkKind::AbortTransfer && !execution.frozen_by(identity) {
                execution.unfreeze(identity);
                return 0;
            }
        }
        let status = (|| {
            let mut state = checked(task).lock_or_recover();
            if let Some(layout) = state.layout.as_ref() {
                let width = match slot_bytes(layout) {
                    Ok(width) => width,
                    Err(failure) => return failure,
                };
                match storage(visitor, state.result, width) {
                    DESCEND if state.initialized => {
                        let status = visit_value(layout, state.result, visitor);
                        if status != 0 {
                            return status;
                        }
                    }
                    DESCEND | SKIP_DESCENDANTS => {}
                    failure => return failure,
                }
            }
            let status = state
                .invocation
                .as_ref()
                .map_or(0, |invocation| invocation.visit_managed(visitor));
            if status != 0 {
                return status;
            }
            if kind == HewManagedWalkKind::CommitTransfer {
                state.value_owner = destination_owner(visitor);
            }
            let completed = state.completed;
            drop(state);
            if completed {
                0
            } else {
                execution
                    .as_ref()
                    .map_or(0, |execution| execution.visit_frozen_frame(visitor))
            }
        })();
        if matches!(
            kind,
            HewManagedWalkKind::CommitTransfer | HewManagedWalkKind::AbortTransfer
        ) {
            if let Some(execution) = execution {
                execution.unfreeze(identity);
            }
        }
        status
    }
}

/// Visit a suspended invocation's lexical child scope through its actual tasks.
/// # Safety
/// The scope belongs to a frozen invocation for the duration of this walk.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_task_scope(
    slot: *const c_void,
    visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
) -> i32 {
    use hew_cabi::managed::{HewManagedWalkKind, DESCEND, SKIP_DESCENDANTS};
    // SAFETY: the invocation gate excludes scope mutation and destruction.
    unsafe {
        let scope = slot.cast::<*mut HewTaskScope>().read();
        if scope.is_null() {
            return 0;
        }
        match crate::owned_value_budget::visit::storage(visitor, scope.cast(), 0) {
            SKIP_DESCENDANTS => return 0,
            DESCEND => {}
            failure => return failure,
        }
        if (*visitor).kind == HewManagedWalkKind::CommitTransfer {
            (*scope).value_owner = crate::owned_value_budget::transfer::destination_owner(visitor);
            if let Some(binding) = crate::owned_value_budget::transfer::destination_binding(visitor)
            {
                (*scope).rc_owner = binding.rc_owner;
            }
        }
        let mut task = (*scope).tasks;
        while !task.is_null() {
            let status = hew_managed_visit_task(ptr::from_ref(&task).cast(), visitor);
            if status != 0 {
                return status;
            }
            task = (*task).next;
        }

        0
    }
}

unsafe fn spawn_invocation(
    scope: *mut HewTaskScope,
    invocation: CheckedTaskInvocation,
    layout: *const HewValueLayout,
    provider_layout: *const HewValueLayout,
) -> *mut HewTask {
    // SAFETY: generated result layouts remain immutable and live through execution.
    let allocation = unsafe { layout.as_ref() }.map(|layout| {
        Layout::from_size_align(layout.size.max(1), layout.align)
            .unwrap_or_else(|_| std::process::abort())
    });
    let result = allocation.map_or(ptr::null_mut(), |allocation| {
        // SAFETY: allocation is nonzero and released by the final task owner.
        let result = unsafe { alloc(allocation) };
        if result.is_null() {
            handle_alloc_error(allocation);
        }
        result
    });
    // SAFETY: caller transfers invocation ownership and exclusively accesses scope.
    unsafe {
        let task = hew_task_new();
        (*task).checked = Some(Mutex::new(CheckedTaskState {
            _provider_code: crate::code_owner::CodeRoot::acquire(
                provider_layout
                    .as_ref()
                    .map_or(ptr::null(), |layout| layout.code_owner),
            ),
            _result_code: crate::code_owner::CodeRoot::acquire(
                layout
                    .as_ref()
                    .map_or(ptr::null(), |layout| layout.code_owner),
            ),
            invocation: Some(invocation),
            accounting: TaskAccounting::Scope,
            layout,
            provider_layout,
            value_owner: (*scope).value_owner.clone(),
            execution: Weak::new(),
            allocation,
            result: result.cast(),
            initialized: false,
            started: false,
            completed: false,
            taken: false,
            status: 0,
            fault: ptr::null_mut(),
            order: 0,
            waiters: Vec::new(),
        }));
        hew_task_scope_spawn(scope, task);
        retain(task);
        task
    }
}

/// Publish a prepared task to the scheduler after checked value adoption.
///
/// # Safety
/// The task is an unstarted constructor result whose materialization committed.
/// Its parent scope and source handle remain live through this publication.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_start(task: *mut HewTask) {
    // SAFETY: the caller owns the unpublished task and its live lexical scope.
    unsafe {
        let (owner, layout) = {
            let mut state = checked(task).lock_or_recover();
            assert!(!state.started, "a checked task can start only once");
            state.started = true;
            (state.value_owner.clone(), state.provider_layout)
        };
        if let Some(owner) = owner {
            // The published source handle already owns this graph. This extra
            // reference keeps its result allocation charged through scope drain.
            owner
                .acquire_value(&*layout, (&raw const task).cast())
                .unwrap_or_else(|_| std::process::abort())
                .commit();
        }
        retain(task);
        TaskExecution::spawn(task);
    }
}

pub(super) unsafe fn release_account(task: *mut HewTask) {
    // SAFETY: the final Rust reference keeps the complete provider graph live.
    let (owner, layout) = {
        let state = unsafe { checked(task) }.lock_or_recover();
        (state.value_owner.clone(), state.provider_layout)
    };
    if let Some(owner) = owner {
        // SAFETY: no execution or observer remains; the graph is immutable.
        unsafe { owner.release_value(&*layout, (&raw const task).cast()) }
            .unwrap_or_else(|_| std::process::abort());
    }
}

/// Transfer a checked nullary once callable into a scope and return one handle.
/// # Safety
/// Scope is exclusively accessed and outlives its drained tasks. Callable and
/// layout obey the compiler's exact Send input/result contract. Null layout is !.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_spawn(
    scope: *mut HewTaskScope,
    callable: *mut HewCallableValue,
    layout: *const HewValueLayout,
    invocation_slot_bytes: u64,
    provider_layout: *const HewValueLayout,
) -> *mut HewTask {
    // SAFETY: generated code transfers the owning carrier and writable source slot.
    unsafe {
        let owner = ptr::read(callable);
        (*callable).environment = ptr::null_mut();
        (*callable).descriptor = ptr::null();
        spawn_invocation(
            scope,
            CheckedTaskInvocation::Callable {
                code: crate::code_owner::CodeRoot::acquire(
                    (*(*owner.descriptor).environment).code_owner,
                ),
                value: owner,
                arguments: ptr::null(),
                consuming: true,
                slot_bytes: invocation_slot_bytes,
            },
            layout,
            provider_layout,
        )
    }
}

/// Start a scoped invocation borrowing its parent's typed argument slots.
///
/// # Safety
/// The compiler keeps callable borrows and argument storage live until the
/// task's completed drain. Shared inputs are Sync; exclusive or transferred
/// inputs and the result are Send. A consuming callable transfers ownership.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_invoke(
    scope: *mut HewTaskScope,
    callable: *mut HewCallableValue,
    arguments: *const *mut c_void,
    consuming: i32,
    layout: *const HewValueLayout,
    invocation_slot_bytes: u64,
    provider_layout: *const HewValueLayout,
) -> *mut HewTask {
    // SAFETY: generated code supplies the exact descriptor and scoped slots.
    unsafe {
        let value = ptr::read(callable);
        if consuming != 0 {
            (*callable).environment = ptr::null_mut();
            (*callable).descriptor = ptr::null();
        }
        spawn_invocation(
            scope,
            CheckedTaskInvocation::Callable {
                code: crate::code_owner::CodeRoot::acquire(
                    (*(*value.descriptor).environment).code_owner,
                ),
                value,
                arguments,
                consuming: consuming != 0,
                slot_bytes: invocation_slot_bytes,
            },
            layout,
            provider_layout,
        )
    }
}

/// Adopt owning handles into a group without changing their cancellation ancestry.
/// # Safety
/// Members are distinct owning checked Task<T> references whose original scopes
/// outlive the group. Scope is exclusively accessed; layout describes their T.
/// The member array is borrowed only during this call.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_race(
    scope: *mut HewTaskScope,
    members: *const *mut HewTask,
    count: u32,
    layout: *const HewValueLayout,
    task_slot_bytes: u64,
    provider_layout: *const HewValueLayout,
) -> *mut HewTask {
    // SAFETY: generated code supplies distinct owned members and a live array.
    unsafe {
        let members = std::slice::from_raw_parts(members, count as usize).to_vec();
        if members.is_empty() {
            std::process::abort();
        }
        for member in &members {
            let mut state = checked(*member).lock_or_recover();
            if state.accounting == TaskAccounting::Group || state.taken {
                std::process::abort();
            }
            state.accounting = TaskAccounting::Group;
        }
        spawn_invocation(
            scope,
            CheckedTaskInvocation::Race {
                members,
                slot_bytes: task_slot_bytes,
            },
            layout,
            provider_layout,
        )
    }
}

/// Observe completion, consuming one source task reference into the wait.
///
/// # Safety
/// `task` is one owning checked task handle. `waker` is a valid retained-target
/// descriptor. Poll status before parking: an already completed task needs no
/// registration or wake. Free the wait after taking or abandoning its result.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_wait_new(
    task: *mut HewTask,
    waker: *const HewWaker,
) -> *mut HewCheckedTaskWait {
    // SAFETY: the caller transfers its task reference and retains the descriptor.
    Box::into_raw(Box::new(unsafe {
        HewCheckedTaskWait::new(task, waker, true)
    }))
}

/// Visit the consumed task retained by an active await operation.
/// # Safety
/// The operation belongs to a frozen invocation and remains live throughout.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_task_wait(
    slot: *const c_void,
    visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the frozen frame retains this exact private operation handle.
    unsafe {
        let wait = slot.cast::<*const HewCheckedTaskWait>().read();
        let Some(wait) = wait.as_ref() else {
            return 0;
        };
        if wait.owns_account_edge {
            hew_managed_visit_task(ptr::from_ref(&wait.task).cast(), visitor)
        } else {
            0
        }
    }
}

impl HewCheckedTaskWait {
    /// Take one task reference and register readiness only while it is pending.
    unsafe fn new(task: *mut HewTask, waker: *const HewWaker, owns_account_edge: bool) -> Self {
        let wake = {
            // SAFETY: this operation has taken ownership of the task reference.
            let mut state = unsafe { checked(task) }.lock_or_recover();
            if state.completed {
                None
            } else {
                let wake = Arc::new(TaskWake {
                    // SAFETY: the caller retains the descriptor during registration.
                    waker: unsafe { OwnedWaker::retain(&*waker) },
                    armed: AtomicBool::new(true),
                });
                state.waiters.retain(|waiter| waiter.strong_count() != 0);
                state.waiters.push(Arc::downgrade(&wake));
                Some(wake)
            }
        };
        Self {
            task,
            wake,
            owns_account_edge,
        }
    }
}

/// # Safety
/// Wait remains live for the poll. Status: pending 0, value 1, fault 2,
/// cancellation 3, or already consumed 4.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_wait_status(wait: *const HewCheckedTaskWait) -> i32 {
    // SAFETY: the live wait retains its task.
    unsafe { checked((*wait).task) }.lock_or_recover().outcome()
}

/// # Safety
/// Wait retains a live child. Cancellation still requires a completed drain.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_wait_cancel(wait: *const HewCheckedTaskWait) {
    // SAFETY: the wait retains its task and the task retains its token.
    unsafe { hew_cancel_token_cancel((*(*wait).task).cancel_token, 1) };
}

/// # Safety
/// Wait remains live and its task has completed.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_wait_private_status(
    wait: *const HewCheckedTaskWait,
) -> i32 {
    // SAFETY: the wait retains its task while the outcome is read.
    unsafe { checked((*wait).task) }.lock_or_recover().status
}

/// Transfer a completed result or fault, leaving outputs untouched if pending.
/// # Panics
/// Panics if an owned result is discarded without a writable release cursor output.
///
/// # Safety
/// Wait is live; output has its exact checked result layout and fault is a
/// writable empty slot. Only one consumer may take this task's output.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_wait_take(
    wait: *const HewCheckedTaskWait,
    output: *mut c_void,
    fault: *mut *mut HewFault,
    release_out: *mut *mut HewReleaseCursor,
) -> i32 {
    if !release_out.is_null() {
        // SAFETY: generated code supplies an empty writable cursor output.
        unsafe { release_out.write(ptr::null_mut()) };
    }
    // SAFETY: the wait retains task storage until this operation returns.
    let mut state = unsafe { checked((*wait).task) }.lock_or_recover();
    let outcome = state.outcome();
    if outcome == PENDING || outcome == TAKEN {
        return outcome;
    }
    if state.initialized && !output.is_null() {
        let (result, layout, owner) = (state.result, state.layout, state.value_owner.clone());
        // Claim the result before releasing the provider lock. Scope cleanup
        // must not dispose it while the account transaction moves its edges.
        state.taken = true;
        drop(state);
        // SAFETY: completion stopped the producer; this claimed value is stable.
        unsafe {
            ptr::copy_nonoverlapping(result.cast::<u8>(), output.cast(), (*layout).size);
            if let Some(owner) = owner {
                let adopted = owner
                    .acquire_value(&*layout, output)
                    .unwrap_or_else(|_| std::process::abort());
                owner
                    .release_value(&*layout, result)
                    .unwrap_or_else(|_| std::process::abort());
                adopted.commit();
            }
        }
        // SAFETY: the wait still retains the provider after destination adoption.
        unsafe { checked((*wait).task) }
            .lock_or_recover()
            .initialized = false;
        return outcome;
    }
    // SAFETY: completed value/fault has one owner and caller supplies exact slots.
    unsafe {
        if state.initialized {
            let size = (*state.layout).size;
            if output.is_null() {
                let cursor = HewReleaseCursor::values([(state.result, *state.layout)]);
                if release_out.is_null() {
                    assert!(
                        cursor.is_null(),
                        "discarded owned result needs a cleanup cursor"
                    );
                } else {
                    release_out.write(cursor);
                }
            } else if size != 0 {
                ptr::copy_nonoverlapping(state.result.cast::<u8>(), output.cast(), size);
            }
            state.initialized = false;
        } else {
            fault.write(std::mem::replace(&mut state.fault, ptr::null_mut()));
        }
    }
    state.taken = true;
    outcome
}

/// # Safety
/// Free one owning wait after no execution can access it. This disarms its
/// registration without cancelling the independently scope-owned child.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_wait_free(wait: *mut HewCheckedTaskWait) {
    if !wait.is_null() {
        // SAFETY: caller transfers the unique wait allocation.
        drop(unsafe { Box::from_raw(wait) });
    }
}

/// # Safety
/// Scope is live and has no further parent-side spawns during this drain.
/// Waker obeys the retained readiness contract. Scope outlives the returned wait.
/// Only one drain may own the scope's result cleanup at a time.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_wait_new(
    scope: *mut HewTaskScope,
    waker: *const HewWaker,
    parent: *const HewCoroState,
) -> *mut HewCheckedScopeWait {
    let mut tasks = Vec::new();
    // SAFETY: the exclusively accessed scope retains every listed task.
    unsafe {
        let mut task = (*scope).tasks;
        while !task.is_null() {
            retain(task);
            tasks.push(HewCheckedTaskWait::new(task, waker, false));
            task = (*task).next;
        }
    }
    // SAFETY: scope retains the cancellation ancestry and registration retains
    // the caller's readiness target independently of its suspended frame.
    let state = unsafe {
        if parent.is_null() {
            hew_coro_state_new(waker, ptr::null_mut())
        } else {
            crate::coro_state::hew_coro_state_cleanup_child(parent)
        }
    };
    Box::into_raw(Box::new(HewCheckedScopeWait {
        scope,
        tasks,
        cancellation: ScopeCancellation::Ordinary,
        subset: false,
        close: Mutex::new(ScopeResultClose {
            state,
            driver: None,
            collected: false,
            complete: false,
            fault: ptr::null_mut(),
        }),
    }))
}

/// Request cooperative cancellation; user code and operation producers must
/// still complete before a scope drain can report ready.
///
/// # Safety
/// Scope remains live throughout this call.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_cancel(scope: *mut HewTaskScope) {
    // SAFETY: scope owns the cancellation token and tasks retain its ancestry.
    unsafe {
        let token = (*scope).cancel_token;
        let reason = crate::cancel_token::cancel_token_reason(token);
        hew_cancel_token_cancel(token, if reason == 0 { 1 } else { reason });
    }
}

/// Request cancellation after the selected race child has completed. The drain
/// removes only its own cancellation marker, preserving every actual failure.
///
/// # Safety
/// The caller exclusively owns this live drain; all children belong to the race.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_wait_cancel_losers(wait: *mut HewCheckedScopeWait) {
    // SAFETY: exclusive access precedes polling and the drain retains its scope.
    unsafe {
        let wait = &mut *wait;
        wait.cancellation = ScopeCancellation::RaceLosers;
        let token = (*wait.scope).cancel_token;
        let reason = crate::cancel_token::cancel_token_reason(token);
        let reason = if reason == 0 {
            crate::fault::HEW_FAULT_RACE_LOST
        } else {
            reason
        };
        for task in &wait.tasks {
            // Completed winners can own values that survive this scope. Their
            // cancellation ancestry must not be changed by losing siblings.
            let pending = !checked(task.task).lock_or_recover().completed;
            if pending {
                hew_cancel_token_cancel((*task.task).cancel_token, reason);
            }
        }
    }
}

/// # Safety
/// Wait and scope remain live. Returns 1 only after all child frames have
/// completed cleanup and abandoned results have closed. An observed child fault
/// requests cancellation of siblings. Calls on one wait must be serialized.
/// # Panics
/// Panics if completed task storage violates the checked scope ownership contract.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_checked_scope_wait_status(
    wait: *const HewCheckedScopeWait,
) -> i32 {
    // SAFETY: caller retains the scope drain and its task references.
    let wait = unsafe { &*wait };
    let mut pending = false;
    let mut failed = false;
    for task in &wait.tasks {
        // SAFETY: each wait retains the corresponding task.
        let state = unsafe { checked(task.task) }.lock_or_recover();
        pending |= !state.completed;
        failed |= (wait.subset || state.accounting == TaskAccounting::Scope)
            && state.completed
            && !state.taken
            && state.status != 0;
    }
    if failed && wait.cancellation == ScopeCancellation::Ordinary {
        // SAFETY: wait borrows its still-live scope until released.
        unsafe {
            if wait.subset {
                for member in &wait.tasks {
                    if !checked(member.task).lock_or_recover().completed {
                        hew_cancel_token_cancel((*member.task).cancel_token, HEW_FAULT_CANCELLED);
                    }
                }
            } else {
                hew_checked_scope_cancel(wait.scope);
            }
        };
    }
    if pending {
        return PENDING;
    }
    let mut close = wait.close.lock_or_recover();
    if close.complete {
        return READY;
    }
    // SAFETY: all child invocations have stopped. The drain retains every task
    // allocation and claims initialized results before borrowing their children.
    unsafe {
        if !close.collected {
            let mut values = Vec::new();
            for task in &wait.tasks {
                let mut state = checked(task.task).lock_or_recover();
                if (wait.subset || state.accounting == TaskAccounting::Scope)
                    && state.initialized
                    && !state.taken
                {
                    state.taken = true;
                    state.initialized = false;
                    values.push((state.result, *state.layout));
                }
            }
            close.driver = Some(ReleaseDriver::new(HewReleaseCursor::values(values)));
            close.collected = true;
        }
        let parent = close.state;
        let driver = close.driver.as_mut().expect("collected scope results");
        if !driver.poll(parent) {
            return PENDING;
        }
        close.fault = driver.take_fault();
    }
    close.complete = true;
    READY
}

/// Transfer unobserved child faults in completion order after a complete drain.
/// Result cleanup faults follow child faults without replacing their primary.
///
/// # Safety
/// Wait remains live and fault is an empty, writable owning slot. A pending
/// drain leaves both the slot and every child fault untouched.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_checked_scope_wait_take_fault(
    wait: *const HewCheckedScopeWait,
    fault: *mut *mut HewFault,
) -> i32 {
    // SAFETY: caller retains this drain and its task references.
    let wait = unsafe { &*wait };
    // Completion is monotonic. Check every child before transferring any owner,
    // so a pending child cannot discard faults already removed from siblings.
    // SAFETY: this poll preserves fault ownership until result cleanup completes.
    if unsafe { hew_checked_scope_wait_status(wait) } == PENDING {
        return PENDING;
    }
    let mut failures = Vec::new();
    for task in &wait.tasks {
        // SAFETY: the drain retains the task.
        let mut state = unsafe { checked(task.task) }.lock_or_recover();
        if (wait.subset || state.accounting == TaskAccounting::Scope)
            && !state.taken
            && state.status != 0
        {
            state.taken = true;
            let mut owner = std::mem::replace(&mut state.fault, ptr::null_mut());
            if wait.cancellation == ScopeCancellation::RaceLosers {
                // SAFETY: this completed child transfers its optional fault owner.
                owner = unsafe { crate::fault::finish_race_loser(owner) };
            }
            // The cancellation marker may have been removed or a secondary
            // cleanup fault promoted; the remaining fault owns its status.
            // SAFETY: owner is null or the uniquely transferred live fault.
            if let Some(fault) = unsafe { owner.as_ref() } {
                failures.push((state.order, fault.code(), owner));
            }
        }
    }
    failures.sort_by_key(|failure| failure.0);
    let mut status = 0;
    let mut primary = ptr::null_mut();
    for (_, code, owner) in failures {
        if status == 0 {
            status = code;
        }
        // SAFETY: each child transfers a distinct owned fault at most once.
        primary = unsafe { hew_fault_combine(primary, owner) };
    }
    let cleanup = std::mem::replace(&mut wait.close.lock_or_recover().fault, ptr::null_mut());
    // SAFETY: the completed collector transfers one optional cleanup fault.
    unsafe {
        if status == 0 {
            status = cleanup.as_ref().map_or(0, HewFault::code);
        }
        primary = hew_fault_combine(primary, cleanup);
    }
    // SAFETY: caller supplies an empty writable fault slot.
    unsafe { fault.write(primary) };
    status
}

/// # Safety
/// Caller transfers the unique drain handle; no poll may access it afterwards.
/// Started result cleanup must have completed and its fault must be taken.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_wait_free(wait: *mut HewCheckedScopeWait) {
    if !wait.is_null() {
        // SAFETY: the caller transfers the unique allocation.
        drop(unsafe { Box::from_raw(wait) });
    }
}

/// Release the scope's task references after all user frames have stopped.
/// Worker epilogues retain their own references and never access parent data.
///
/// # Safety
/// Scope has completed a checked drain, has no active waits or deadline nodes,
/// and is consumed by this call. Source handles cannot escape their scope.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_close(scope: *mut HewTaskScope) {
    // SAFETY: the caller transfers a drained scope allocation.
    let mut scope = unsafe { Box::from_raw(scope) };
    // The checked drain has closed and disposed every abandoned result before
    // enclosing source resources can be released.
    // SAFETY: scope retains every task and the completed frames no longer use results.
    unsafe {
        let mut task = scope.tasks;
        while !task.is_null() {
            let state = checked(task).lock_or_recover();
            if !state.completed || state.initialized {
                std::process::abort();
            }
            task = (*task).next;
        }
        free_scope_tasks(&mut scope);
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
#[path = "task_scope_checked_tests.rs"]
mod tests;
