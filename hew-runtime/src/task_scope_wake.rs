//! Retained wake subscriptions to the existing cancellation-token tree.

use super::{hew_cancel_token_release, hew_cancel_token_retain, token_state, HewCancellationToken};
use crate::util::MutexExt;
use crate::wake::{HewWaker, OwnedWaker};
use std::sync::Arc;

/// One subscription to a token and its ancestors. It retains the leaf token,
/// which in turn retains every ancestor, and one stable readiness target.
#[derive(Debug)]
pub struct HewCancelObserver {
    token: *mut HewCancellationToken,
    waker: Arc<OwnedWaker>,
}

/// Subscribe a readiness target to cancellation of a token or any ancestor.
/// An already-requested cancellation wakes immediately. Notifications may
/// coalesce; callers inspect the token after every wake.
///
/// # Safety
/// `token` must be null or a live token. `waker` must point to a valid retained
/// descriptor obeying [`HewWaker`]'s contract throughout this call.
#[no_mangle]
pub unsafe extern "C" fn hew_cancel_observe(
    token: *mut HewCancellationToken,
    waker: *const HewWaker,
) -> *mut HewCancelObserver {
    if waker.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: the caller keeps both inputs live; these references own their
    // lifetimes after this call returns.
    let retained = unsafe { Arc::new(OwnedWaker::retain(&*waker)) };
    // SAFETY: the caller supplies a live token or null.
    unsafe { hew_cancel_token_retain(token) };
    let mut current = token;
    let mut cancelled = false;
    while !current.is_null() {
        // SAFETY: the retained leaf keeps its entire parent chain live.
        let ancestor = unsafe { &*current };
        {
            let mut observers = ancestor.observers.lock_or_recover();
            // The same lock serializes insertion with the cancellation sweep.
            // Cancellation published before insertion is observed here; a
            // later publication must sweep the newly installed subscription.
            if token_state(ancestor).is_requested() {
                cancelled = true;
            } else {
                observers.push(Arc::downgrade(&retained));
            }
        }
        current = ancestor.parent;
    }
    if cancelled {
        retained.wake();
    }
    Box::into_raw(Box::new(HewCancelObserver {
        token,
        waker: retained,
    }))
}

/// Detach a cancellation subscription and release its retained references.
/// An already-dispatched wake may still run against its own retained target;
/// it can never access coroutine-frame storage.
///
/// # Safety
/// `observer` must be null or an owned observer returned by
/// [`hew_cancel_observe`], released exactly once without concurrent access.
#[no_mangle]
pub unsafe extern "C" fn hew_cancel_unobserve(observer: *mut HewCancelObserver) {
    if observer.is_null() {
        return;
    }
    // SAFETY: caller transfers the sole subscription handle.
    let observer = unsafe { Box::from_raw(observer) };
    let identity = Arc::as_ptr(&observer.waker);
    let mut current = observer.token;
    while !current.is_null() {
        // SAFETY: the observer's retained leaf keeps every ancestor live.
        let ancestor = unsafe { &*current };
        ancestor
            .observers
            .lock_or_recover()
            .retain(|entry| entry.as_ptr() != identity);
        current = ancestor.parent;
    }
    // SAFETY: consumes the reference acquired by observe.
    unsafe { hew_cancel_token_release(observer.token) };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::task_scope::{hew_cancel_token_cancel, hew_cancel_token_new_child};
    use crate::wake::blocking::Readiness;

    #[test]
    fn ancestor_cancellation_wakes_descendant_and_detaches_without_retaining_tree() {
        let (ready, waker) = Readiness::new();
        // SAFETY: this test owns the complete token tree and observer handles.
        unsafe {
            let root = hew_cancel_token_new_child(std::ptr::null_mut());
            let child = hew_cancel_token_new_child(root);
            let observer = hew_cancel_observe(child, waker.descriptor());
            hew_cancel_token_release(child);
            hew_cancel_token_cancel(root, 1);
            assert!(ready.take_ready());
            hew_cancel_unobserve(observer);
            assert!((*root).observers.lock_or_recover().is_empty());
            hew_cancel_token_release(root);
        }
        assert_eq!(Arc::strong_count(&ready), 2);
    }

    #[test]
    fn already_cancelled_ancestor_notifies_registration() {
        let (ready, waker) = Readiness::new();
        // SAFETY: every token and observer is locally owned until released.
        unsafe {
            let root = hew_cancel_token_new_child(std::ptr::null_mut());
            let child = hew_cancel_token_new_child(root);
            hew_cancel_token_cancel(root, 1);
            let observer = hew_cancel_observe(child, waker.descriptor());
            assert!(ready.take_ready());
            hew_cancel_unobserve(observer);
            hew_cancel_token_release(child);
            hew_cancel_token_release(root);
        }
    }

    #[test]
    fn detached_wait_is_not_notified_by_later_cancellation() {
        let (ready, waker) = Readiness::new();
        // SAFETY: locally owned token and observer, with a live waker.
        unsafe {
            let token = hew_cancel_token_new_child(std::ptr::null_mut());
            let observer = hew_cancel_observe(token, waker.descriptor());
            hew_cancel_unobserve(observer);
            hew_cancel_token_cancel(token, 1);
            assert!(!ready.take_ready());
            hew_cancel_token_release(token);
        }
        assert_eq!(Arc::strong_count(&ready), 2);
    }
}
