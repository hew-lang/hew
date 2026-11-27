//! Retained wake subscriptions to the existing cancellation-token tree.

use super::{hew_cancel_token_release, hew_cancel_token_retain, token_state, HewCancellationToken};
use crate::util::MutexExt;
use crate::wake::{HewWaker, OwnedWaker};
use smallvec::SmallVec;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Weak};

/// Sentinel slot index indicating this observer is not registered in the
/// corresponding ancestor's observer list.
const UNLINKED_SLOT: u32 = u32::MAX;

/// Entry in a token's observer list pairing a weak observer handle with its
/// depth in that observer's ancestry chain so `swap_remove` can update the
/// moved entry's back-index in O(1).
#[derive(Debug)]
pub(super) struct ObserverEntry {
    pub(super) observer: Weak<HewCancelObserver>,
    pub(super) depth: usize,
}

/// One subscription to a token and its ancestors. It retains the leaf token,
/// which in turn retains every ancestor, and one stable readiness target.
#[derive(Debug)]
pub struct HewCancelObserver {
    token: *mut HewCancellationToken,
    waker: OwnedWaker,
    /// Per-ancestor slot index in `ancestor.observers`, indexed by distance
    /// from `token` (`0` = `token`, `1` = `token.parent`, ...). Mutated only
    /// while holding the corresponding ancestor's `observers` mutex.
    slots: SmallVec<[AtomicU32; 4]>,
}

// SAFETY: the observer retains its thread-safe token and readiness descriptor;
// its fields are immutable or synchronized by each ancestor's `observers` mutex.
unsafe impl Send for HewCancelObserver {}
// SAFETY: waking and releasing the retained descriptor obey its shared contract.
unsafe impl Sync for HewCancelObserver {}

impl HewCancelObserver {
    pub(super) fn wake(&self) {
        self.waker.wake();
    }

    pub(super) fn clear_slot(&self, depth: usize) {
        if let Some(slot) = self.slots.get(depth) {
            slot.store(UNLINKED_SLOT, Ordering::Relaxed);
        }
    }
}

impl Drop for HewCancelObserver {
    fn drop(&mut self) {
        // SAFETY: this consumes the reference acquired when observing.
        unsafe { hew_cancel_token_release(self.token) };
    }
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
    let retained = unsafe { OwnedWaker::retain(&*waker) };
    // SAFETY: the caller supplies a live token or null.
    unsafe { hew_cancel_token_retain(token) };

    let mut depth_count = 0usize;
    let mut probe = token;
    while !probe.is_null() {
        depth_count += 1;
        // SAFETY: the retained leaf keeps its entire parent chain live.
        probe = unsafe { (*probe).parent };
    }
    let mut slots = SmallVec::with_capacity(depth_count);
    for _ in 0..depth_count {
        slots.push(AtomicU32::new(UNLINKED_SLOT));
    }

    let observer = Arc::new(HewCancelObserver {
        token,
        waker: retained,
        slots,
    });
    let mut current = token;
    let mut depth = 0usize;
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
                let index = u32::try_from(observers.len()).unwrap_or(UNLINKED_SLOT);
                if index != UNLINKED_SLOT {
                    observer.slots[depth].store(index, Ordering::Relaxed);
                }
                observers.push(ObserverEntry {
                    observer: Arc::downgrade(&observer),
                    depth,
                });
            }
        }
        current = ancestor.parent;
        depth += 1;
    }
    if cancelled {
        observer.wake();
    }
    Arc::into_raw(observer).cast_mut()
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
    // SAFETY: caller transfers the sole subscription handle. Keeping `observer`
    // alive until this loop completes guarantees every registered `Weak` for
    // this observer still upgrades until it is unlinked under the lock.
    let observer = unsafe { Arc::from_raw(observer) };
    let identity = Arc::as_ptr(&observer);
    let mut current = observer.token;
    let mut depth = 0usize;
    while !current.is_null() {
        // SAFETY: the observer's retained leaf keeps every ancestor live.
        let ancestor = unsafe { &*current };
        let mut observers = ancestor.observers.lock_or_recover();
        let slot = observer.slots[depth].swap(UNLINKED_SLOT, Ordering::Relaxed);
        if slot != UNLINKED_SLOT {
            let index = slot as usize;
            if index < observers.len() && observers[index].observer.as_ptr() == identity {
                observers.swap_remove(index);
                if let Some(moved) = observers.get(index) {
                    if let Some(moved_obs) = moved.observer.upgrade() {
                        moved_obs.slots[moved.depth].store(slot, Ordering::Relaxed);
                    }
                }
            } else {
                // Fallback if index overflowed `u32::MAX` (impossible in practice).
                observers.retain(|entry| entry.observer.as_ptr() != identity);
                for (new_idx, entry) in observers.iter().enumerate() {
                    if let Some(obs) = entry.observer.upgrade() {
                        obs.slots[entry.depth].store(
                            u32::try_from(new_idx).unwrap_or(UNLINKED_SLOT),
                            Ordering::Relaxed,
                        );
                    }
                }
            }
        }
        current = ancestor.parent;
        depth += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cancel_token::{hew_cancel_token_cancel, hew_cancel_token_new_child};
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
    fn cancellation_wakes_inline_and_spilled_subscriptions_after_detach() {
        for count in [1, 4] {
            let (detached, detached_waker) = Readiness::new();
            // SAFETY: the fixture owns the token and every subscription handle.
            unsafe {
                let token = hew_cancel_token_new_child(std::ptr::null_mut());
                let observer = hew_cancel_observe(token, detached_waker.descriptor());
                hew_cancel_unobserve(observer);
                let subscriptions: Vec<_> = (0..count)
                    .map(|_| {
                        let (ready, waker) = Readiness::new();
                        let observer = hew_cancel_observe(token, waker.descriptor());
                        (ready, waker, observer)
                    })
                    .collect();
                // Detach from the spilled list as well as from its inline form.
                if count > 1 {
                    let observer = hew_cancel_observe(token, detached_waker.descriptor());
                    hew_cancel_unobserve(observer);
                }
                hew_cancel_token_cancel(token, 1);
                assert!(!detached.take_ready());
                for (ready, waker, observer) in subscriptions {
                    assert!(ready.take_ready());
                    hew_cancel_unobserve(observer);
                    drop(waker);
                    assert_eq!(Arc::strong_count(&ready), 1);
                }
                assert!((*token).observers.lock_or_recover().is_empty());
                hew_cancel_token_release(token);
            }
            drop(detached_waker);
            assert_eq!(Arc::strong_count(&detached), 1);
        }
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

    #[test]
    fn fanout_out_of_order_unobserve_preserves_back_indices_and_wakes_survivors() {
        const COUNT: usize = 500;
        // SAFETY: this test owns the complete token tree and every observer handle.
        unsafe {
            let root = hew_cancel_token_new_child(std::ptr::null_mut());
            let mid = hew_cancel_token_new_child(root);
            let mut entries = Vec::with_capacity(COUNT);
            for _ in 0..COUNT {
                let leaf = hew_cancel_token_new_child(mid);
                let (ready, waker) = Readiness::new();
                let observer = hew_cancel_observe(leaf, waker.descriptor());
                entries.push((leaf, ready, waker, observer));
            }
            assert_eq!((*root).observers.lock_or_recover().len(), COUNT);
            assert_eq!((*mid).observers.lock_or_recover().len(), COUNT);

            // Detach even indices in reverse order (exercises head, middle, and tail swap_remove).
            let mut survivors = Vec::with_capacity(COUNT / 2);
            for (i, (leaf, ready, waker, observer)) in entries.into_iter().enumerate().rev() {
                if i % 2 == 0 {
                    hew_cancel_unobserve(observer);
                    hew_cancel_token_release(leaf);
                    drop(waker);
                    assert!(!ready.take_ready());
                    assert_eq!(Arc::strong_count(&ready), 1);
                } else {
                    survivors.push((leaf, ready, waker, observer));
                }
            }
            assert_eq!((*root).observers.lock_or_recover().len(), COUNT / 2);
            assert_eq!((*mid).observers.lock_or_recover().len(), COUNT / 2);

            hew_cancel_token_cancel(root, 1);
            assert!((*root).observers.lock_or_recover().is_empty());

            for (leaf, ready, waker, observer) in survivors {
                assert!(ready.take_ready());
                hew_cancel_unobserve(observer);
                hew_cancel_token_release(leaf);
                drop(waker);
                assert_eq!(Arc::strong_count(&ready), 1);
            }
            assert!((*mid).observers.lock_or_recover().is_empty());
            hew_cancel_token_release(mid);
            hew_cancel_token_release(root);
        }
    }
}
