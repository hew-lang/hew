//! Timer-list payload ownership. The caller serializes list mutations.
use super::HewTimerCb;
use crate::timer_deadline::Deadline;
use std::ffi::{c_int, c_void};
use std::ptr;

/// A single timer entry stored inside the wheel.
#[repr(C)]
#[derive(Debug)]
pub struct HewTimerEntry {
    pub(super) deadline_ms: u64,
    pub(super) generation: u64,
    pub(super) cb: Option<HewTimerCb>,
    pub(super) data: *mut c_void,
    pub(super) cancelled: c_int,
    pub(super) fraction_ns: u32,
    pub(super) next: *mut HewTimerEntry,
}

impl HewTimerEntry {
    pub(super) fn deadline(&self) -> Deadline {
        Deadline {
            millis: self.deadline_ms,
            fraction_ns: self.fraction_ns,
        }
    }
}

// SAFETY: Entries are only accessed under the wheel's Mutex.
unsafe impl Send for HewTimerEntry {}

/// Free every entry in a linked list starting at `head`.
pub(super) fn free_entry_list(mut head: *mut HewTimerEntry) {
    while !head.is_null() {
        // SAFETY: each node was Box-allocated and is owned by the wheel.
        unsafe {
            let next = (*head).next;
            drop(Box::from_raw(head));
            head = next;
        }
    }
}

/// Collected timer entry ready to fire outside the lock.
pub(super) struct ExpiredEntry {
    pub(super) cb: Option<HewTimerCb>,
    pub(super) data: *mut c_void,
    pub(super) cancelled: c_int,
    pub(super) raw: *mut HewTimerEntry,
}

// SAFETY: The raw pointer and data pointer are only used on the thread that
// collected them, after the lock is released.
unsafe impl Send for ExpiredEntry {}

/// Unlink all entries whose deadline ≤ `now` from the list rooted at
/// `*slot_head` and push them onto `expired`.  Entries that are not yet due
/// remain in the list.
///
/// Caller must hold the lock.  Callbacks are **not** invoked here — the
/// caller fires them after releasing the lock to avoid deadlock.
#[cfg(any(test, kani))]
pub(super) fn collect_expired_entries(
    slot_head: *mut *mut HewTimerEntry,
    now: u64,
    expired: &mut Vec<ExpiredEntry>,
) {
    collect_expired_entries_at(slot_head, Deadline::from_millis(now), expired);
}

pub(super) fn collect_expired_entries_at(
    slot_head: *mut *mut HewTimerEntry,
    now: Deadline,
    expired: &mut Vec<ExpiredEntry>,
) {
    let mut prev: *mut HewTimerEntry = ptr::null_mut();
    // SAFETY: caller holds the lock; slot_head points into the wheel arrays.
    let mut cur = unsafe { *slot_head };

    while !cur.is_null() {
        // SAFETY: cur is a valid node in the slot list; all fields accessed
        // under the caller's lock.
        let next = unsafe { (*cur).next };
        // SAFETY: cur is a valid node; reading deadline under lock.
        let cur_deadline = unsafe { (*cur).deadline() };

        if cur_deadline <= now {
            // Unlink from list.
            if prev.is_null() {
                // SAFETY: updating the slot head.
                unsafe {
                    *slot_head = next;
                }
            } else {
                // SAFETY: prev is a valid preceding node.
                unsafe {
                    (*prev).next = next;
                }
            }

            // SAFETY: accessing fields of a valid entry under lock.
            unsafe {
                expired.push(ExpiredEntry {
                    cb: (*cur).cb,
                    data: (*cur).data,
                    cancelled: (*cur).cancelled,
                    raw: cur,
                });
            }
        } else {
            prev = cur;
        }

        cur = next;
    }
}

/// Unlink the entry matching `entry`/`generation` from the list rooted at
/// `*slot_head`, free its node, and return its `data` pointer. Returns `None`
/// if no matching live entry is present in this list.
///
/// Caller must hold the lock.
pub(super) fn remove_entry_from_list(
    slot_head: &mut *mut HewTimerEntry,
    entry: *mut HewTimerEntry,
    generation: u64,
) -> Option<*mut c_void> {
    let mut prev: *mut HewTimerEntry = ptr::null_mut();
    let mut cur = *slot_head;
    while !cur.is_null() {
        // SAFETY: `cur` is a live node in this slot list; caller holds the lock.
        let next = unsafe { (*cur).next };
        // SAFETY: reading identity fields of a live node under the lock.
        let matches = cur == entry && unsafe { (*cur).generation } == generation;
        if matches {
            if prev.is_null() {
                *slot_head = next;
            } else {
                // SAFETY: `prev` is a live preceding node.
                unsafe {
                    (*prev).next = next;
                }
            }
            // SAFETY: `cur` was unlinked and is exclusively owned now; capture
            // its data before freeing the node.
            let data = unsafe { (*cur).data };
            // SAFETY: `cur` was Box-allocated by schedule_handle; reclaim it.
            unsafe {
                drop(Box::from_raw(cur));
            }
            return Some(data);
        }
        prev = cur;
        cur = next;
    }
    None
}

pub(super) fn fire_expired_entries(expired: Vec<ExpiredEntry>) -> c_int {
    let mut total_fired: c_int = 0;
    for e in expired {
        // SAFETY: entry was unlinked from the wheel under the lock and is
        // exclusively owned.  The callback and data pointers are valid per
        // the caller's contract.
        unsafe {
            if e.cancelled == 0 {
                if let Some(cb) = e.cb {
                    cb(e.data);
                    total_fired += 1;
                }
            }
            drop(Box::from_raw(e.raw));
        }
    }

    total_fired
}
