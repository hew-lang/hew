//! Cancellation tokens: the runtime's cancellation authority.
//!
//! Tokens form a parent-child tree rooted at the process entry. A scope, a
//! task, an actor call and the root driver each hold one; cancelling a parent
//! is observed by every descendant. The tree carries no thread state, so it is
//! the same authority on every target.

use std::sync::atomic::{AtomicI32, AtomicI64, AtomicUsize, Ordering};
use std::sync::Mutex;

use crate::util::MutexExt as _;

#[path = "cancel_token_wake.rs"]
mod wake;
pub use wake::{hew_cancel_observe, hew_cancel_unobserve, HewCancelObserver};

// ── Cancellation tokens ─────────────────────────────────────────────────

#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HewCancellationState {
    Active = 0,
    CancelRequested = 1,
    Observed = 2,
    Trapped = 3,
    Completed = 4,
}

impl HewCancellationState {
    fn from_i32(raw: i32) -> Self {
        match raw {
            0 => Self::Active,
            1 => Self::CancelRequested,
            2 => Self::Observed,
            3 => Self::Trapped,
            4 => Self::Completed,
            _ => panic!("HewCancellationToken.state contained an invalid discriminant"),
        }
    }

    fn is_requested(self) -> bool {
        matches!(self, Self::CancelRequested | Self::Observed | Self::Trapped)
    }
}

/// Opaque, ref-counted cancellation token used by scope-owned tasks.
///
/// Tokens form a parent-child tree: cancelling a parent is observed by every
/// descendant through [`hew_cancel_token_is_requested`]. The tree is owned by
/// task scopes and tasks; raw FFI handles returned by scope accessors are
/// borrowed unless the function explicitly says otherwise.
#[derive(Debug)]
pub struct HewCancellationToken {
    refs: AtomicUsize,
    state: AtomicI32,
    reason: AtomicI32,
    parent: *mut HewCancellationToken,
    pub(crate) deadline_ns: AtomicI64,
    /// Readiness subscribers; token state remains the cancellation authority.
    observers: Mutex<Vec<std::sync::Weak<HewCancelObserver>>>,
    children_total: AtomicI32,
}

// SAFETY: all mutable token state is atomic. The parent pointer is retained
// for the token lifetime, and released only when this token's ref-count
// reaches zero.
unsafe impl Send for HewCancellationToken {}
// SAFETY: all shared token state is atomic and parent lifetime is retained by
// token ref-counting.
unsafe impl Sync for HewCancellationToken {}

unsafe fn hew_cancel_token_retain_impl(token: *mut HewCancellationToken) {
    if !token.is_null() {
        // SAFETY: caller guarantees `token` is a live token pointer.
        unsafe { (*token).refs.fetch_add(1, Ordering::Relaxed) };
    }
}

unsafe fn hew_cancel_token_release_impl(mut token: *mut HewCancellationToken) {
    while !token.is_null() {
        // SAFETY: the caller or the child being freed owns this reference.
        if unsafe { (*token).refs.fetch_sub(1, Ordering::Release) } != 1 {
            return;
        }
        std::sync::atomic::fence(Ordering::Acquire);
        // SAFETY: this was the last reference, so reclaim the allocation.
        let boxed = unsafe { Box::from_raw(token) };
        token = boxed.parent;
    }
}

pub(crate) fn token_state(token: &HewCancellationToken) -> HewCancellationState {
    HewCancellationState::from_i32(token.state.load(Ordering::Acquire))
}

/// Increment the reference count on `token`.
///
/// Null is accepted and treated as a no-op.
///
/// # Safety
///
/// If non-null, `token` must be a live pointer returned by
/// [`hew_cancel_token_new_child`] or borrowed from
/// [`hew_task_scope_cancel_token`]. The caller must guarantee the token is not
/// concurrently freed.
#[no_mangle]
pub unsafe extern "C" fn hew_cancel_token_retain(token: *mut HewCancellationToken) {
    // SAFETY: delegated to impl; caller upholds the same contract.
    unsafe { hew_cancel_token_retain_impl(token) }
}

/// Decrement the reference count on `token`, freeing it when it reaches zero.
///
/// Null is accepted and treated as a no-op. When the last reference is dropped,
/// ancestor references are released until a retained ancestor is reached.
///
/// # Safety
///
/// If non-null, `token` must be a live pointer returned by
/// [`hew_cancel_token_new_child`] or borrowed from
/// [`hew_task_scope_cancel_token`]. After this call returns the pointer must
/// not be used (it may have been freed).
#[no_mangle]
pub unsafe extern "C" fn hew_cancel_token_release(token: *mut HewCancellationToken) {
    // SAFETY: delegated to impl; caller upholds the same contract.
    unsafe { hew_cancel_token_release_impl(token) }
}

/// Create a cancellation token derived from `parent`.
///
/// Passing null creates a root token. The returned token is owned by the caller
/// and must be attached to a scope/task that will release it.
///
/// # Safety
///
/// If `parent` is non-null, it must be a valid pointer returned by
/// [`hew_cancel_token_new_child`] or borrowed from
/// [`hew_task_scope_cancel_token`].
#[no_mangle]
pub unsafe extern "C" fn hew_cancel_token_new_child(
    parent: *mut HewCancellationToken,
) -> *mut HewCancellationToken {
    if !parent.is_null() {
        // SAFETY: caller guarantees `parent` is valid.
        unsafe {
            hew_cancel_token_retain_impl(parent);
            (*parent).children_total.fetch_add(1, Ordering::Relaxed);
        }
    }

    let token = Box::new(HewCancellationToken {
        refs: AtomicUsize::new(1),
        state: AtomicI32::new(HewCancellationState::Active as i32),
        reason: AtomicI32::new(0),
        parent,
        deadline_ns: AtomicI64::new(-1),
        observers: Mutex::new(Vec::new()),
        children_total: AtomicI32::new(0),
    });
    Box::into_raw(token)
}

/// Read the minimum explicit scope deadline in this retained ancestry.
pub(crate) unsafe fn token_deadline(
    mut token: *mut HewCancellationToken,
) -> crate::request_deadline::RequestDeadline {
    let mut deadline = crate::request_deadline::RequestDeadline::default();
    // SAFETY: each live token retains its parent until this borrow ends.
    while let Some(current) = unsafe { token.as_ref() } {
        deadline = deadline.earlier(crate::request_deadline::RequestDeadline::from_raw(
            current.deadline_ns.load(Ordering::Acquire),
        ));
        token = current.parent;
    }
    deadline
}

/// Request cancellation on `token`.
///
/// The transition from `Active` to `CancelRequested` happens at most once.
/// Descendant tokens observe the request transitively.
///
/// # Safety
///
/// `token` must be a valid pointer returned by [`hew_cancel_token_new_child`]
/// or borrowed from [`hew_task_scope_cancel_token`].
#[no_mangle]
pub unsafe extern "C" fn hew_cancel_token_cancel(token: *mut HewCancellationToken, reason: i32) {
    cabi_guard!(token.is_null());
    // SAFETY: caller guarantees `token` is valid.
    let t = unsafe { &*token };
    // Publish the winning reason before making cancellation observable. A
    // competing requester may publish the state, but cannot replace the reason.
    let reason = if reason == 0 { 1 } else { reason };
    let _ = t
        .reason
        .compare_exchange(0, reason, Ordering::AcqRel, Ordering::Acquire);
    if t.state
        .compare_exchange(
            HewCancellationState::Active as i32,
            HewCancellationState::CancelRequested as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        let observers = {
            let mut registered = t.observers.lock_or_recover();
            std::mem::take(&mut *registered)
                .into_iter()
                .filter_map(|observer| observer.upgrade())
                .collect::<Vec<_>>()
        };
        for observer in observers {
            observer.wake();
        }
    }
}

// The timer wakes parked work; a safepoint must also observe the same expiry
// before a ready operation can outrun that callback.
unsafe fn poll_token_state(token: *mut HewCancellationToken) -> HewCancellationState {
    // SAFETY: the caller retains this token throughout the observation.
    let current = unsafe { &*token };
    if token_state(current) == HewCancellationState::Active
        && crate::request_deadline::RequestDeadline::from_raw(
            current.deadline_ns.load(Ordering::Acquire),
        )
        .expired()
    {
        // SAFETY: expiry publishes through the token's existing cancellation authority.
        unsafe { hew_cancel_token_cancel(token, crate::fault::HEW_FAULT_DEADLINE) };
    }
    token_state(current)
}

unsafe fn cancel_token_is_requested_raw(mut token: *mut HewCancellationToken) -> bool {
    while !token.is_null() {
        // SAFETY: the caller retains the token and each token retains its parent.
        let current = unsafe { &*token };
        // SAFETY: this token and its ancestry remain retained during the poll.
        if unsafe { poll_token_state(token) }.is_requested() {
            return true;
        }
        token = current.parent;
    }
    false
}

/// Return whether `token` or any ancestor has requested cancellation.
///
/// Returns `1` when cancellation is requested, otherwise `0`.
///
/// # Safety
///
/// `token` must be a valid pointer returned by [`hew_cancel_token_new_child`]
/// or borrowed from [`hew_task_scope_cancel_token`].
#[no_mangle]
pub unsafe extern "C" fn hew_cancel_token_is_requested(token: *mut HewCancellationToken) -> i32 {
    cabi_guard!(token.is_null(), 0);
    i32::from(
        // SAFETY: caller guarantees `token` is valid.
        unsafe { cancel_token_is_requested_raw(token) },
    )
}

/// Read the reason of the nearest requested cancellation boundary.
///
/// Zero means neither this token nor an ancestor has requested cancellation.
///
/// # Safety
/// `token` is null or a live token retained through this call.
pub(crate) unsafe fn cancel_token_reason(mut token: *mut HewCancellationToken) -> i32 {
    let mut race_lost = 0;
    while !token.is_null() {
        // SAFETY: the caller retains the token and each token retains its parent.
        let current = unsafe { &*token };
        // SAFETY: this token and its ancestry remain retained during the poll.
        if unsafe { poll_token_state(token) }.is_requested() {
            let reason = current.reason.load(Ordering::Acquire);
            if reason != crate::fault::HEW_FAULT_RACE_LOST {
                return reason;
            }
            // Losing a race never masks cancellation of an enclosing operation.
            race_lost = reason;
        }
        token = current.parent;
    }
    race_lost
}
