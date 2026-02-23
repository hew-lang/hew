//! Two-level hierarchical timer wheel for the Hew runtime.
//!
//! * **Level 0:** 256 slots × 1 ms = 256 ms range
//! * **Level 1:** 64 slots × 256 ms ≈ 16.4 s range
//! * **Overflow:** sorted linked list for timers > 16.4 s
//!
//! O(1) insert for the vast majority of timers.  O(1) per-tick firing.
//! Only the overflow list has O(n) insertion for rare long-duration timers.
//!
//! The wheel struct is `Box`-allocated (opaque to callers).  Internal locking
//! uses [`std::sync::Mutex`].

use std::ffi::{c_int, c_void};
use std::ptr;
use std::sync::Mutex;

use crate::io_time::hew_now_ms;
use crate::timer::HewTimerCb;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const L0_BITS: u32 = 8;
const L1_BITS: u32 = 6;
const L0_SIZE: usize = 1 << L0_BITS; // 256
const L1_SIZE: usize = 1 << L1_BITS; // 64
const L0_MS: u64 = 1;
const L1_MS: u64 = (L0_SIZE as u64) * L0_MS; // 256
const MAX_MS: u64 = (L1_SIZE as u64) * L1_MS; // 16384

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// A single timer entry stored inside the wheel.
#[repr(C)]
#[derive(Debug)]
pub struct HewTimerEntry {
    deadline_ms: u64,
    cb: Option<HewTimerCb>,
    data: *mut c_void,
    cancelled: c_int,
    next: *mut HewTimerEntry,
}

// SAFETY: Entries are only accessed under the wheel's Mutex.
unsafe impl Send for HewTimerEntry {}

/// Internal state protected by a [`Mutex`].
#[derive(Debug)]
struct WheelInner {
    l0: [*mut HewTimerEntry; L0_SIZE],
    l1: [*mut HewTimerEntry; L1_SIZE],
    overflow: *mut HewTimerEntry,
    current_ms: u64,
}

// SAFETY: All raw pointers are owned exclusively by the wheel and only
// accessed under the Mutex.
unsafe impl Send for WheelInner {}

/// Opaque, heap-allocated timer wheel exposed to C via raw pointer.
#[derive(Debug)]
pub struct HewTimerWheel {
    inner: Mutex<WheelInner>,
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Insert `entry` into the correct level (or overflow) based on its deadline.
///
/// Caller must hold the lock.
fn insert_entry(w: &mut WheelInner, entry: *mut HewTimerEntry) {
    // SAFETY: `entry` is a valid, owned node. Caller holds the lock.
    let deadline = unsafe { (*entry).deadline_ms };
    let delta = deadline.saturating_sub(w.current_ms);

    if delta < (L0_SIZE as u64) * L0_MS {
        #[expect(
            clippy::cast_possible_truncation,
            reason = "masked to L0_SIZE which fits in usize"
        )]
        let slot = (deadline / L0_MS) as usize & (L0_SIZE - 1);
        // SAFETY: slot is bounded by mask.
        unsafe {
            (*entry).next = w.l0[slot];
        }
        w.l0[slot] = entry;
    } else if delta < MAX_MS {
        let slot = (deadline / L1_MS) as usize & (L1_SIZE - 1);
        // SAFETY: slot is bounded by mask.
        unsafe {
            (*entry).next = w.l1[slot];
        }
        w.l1[slot] = entry;
    } else {
        // Overflow: sorted insertion.
        let mut pp: *mut *mut HewTimerEntry = &raw mut w.overflow;
        // SAFETY: walking the overflow list under lock.
        unsafe {
            while !(*pp).is_null() && (**pp).deadline_ms <= deadline {
                pp = &raw mut (**pp).next;
            }
            (*entry).next = *pp;
            *pp = entry;
        }
    }
}

/// Free every entry in a linked list starting at `head`.
fn free_entry_list(mut head: *mut HewTimerEntry) {
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
struct ExpiredEntry {
    cb: Option<HewTimerCb>,
    data: *mut c_void,
    cancelled: c_int,
    raw: *mut HewTimerEntry,
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
fn collect_expired_entries(
    slot_head: *mut *mut HewTimerEntry,
    now: u64,
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
        let cur_deadline = unsafe { (*cur).deadline_ms };

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

/// Cascade entries from the current L1 slot into L0.
fn cascade_l1_to_l0(w: &mut WheelInner) {
    let l1_slot = (w.current_ms / L1_MS) as usize & (L1_SIZE - 1);
    let mut entry = w.l1[l1_slot];
    w.l1[l1_slot] = ptr::null_mut();

    while !entry.is_null() {
        // SAFETY: entry is a valid node formerly in the L1 slot.
        let next = unsafe { (*entry).next };
        // SAFETY: entry is a valid node being re-inserted; caller holds the lock.
        unsafe {
            (*entry).next = ptr::null_mut();
        }
        insert_entry(w, entry);
        entry = next;
    }
}

/// Cascade entries from overflow into L1 when we cross the max range boundary.
fn cascade_overflow(w: &mut WheelInner) {
    let max_deadline = w.current_ms + MAX_MS;

    while !w.overflow.is_null() {
        // SAFETY: overflow head is valid.
        let deadline = unsafe { (*w.overflow).deadline_ms };
        if deadline >= max_deadline {
            break;
        }
        let entry = w.overflow;
        // SAFETY: unlinking the head of the overflow list.
        unsafe {
            w.overflow = (*entry).next;
            (*entry).next = ptr::null_mut();
        }
        insert_entry(w, entry);
    }
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Create a new timer wheel.
///
/// # Safety
///
/// No preconditions.  The caller must eventually call [`hew_timer_wheel_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_timer_wheel_new() -> *mut HewTimerWheel {
    // SAFETY: hew_now_ms has no preconditions.
    let now = unsafe { hew_now_ms() };
    let tw = Box::new(HewTimerWheel {
        inner: Mutex::new(WheelInner {
            l0: [ptr::null_mut(); L0_SIZE],
            l1: [ptr::null_mut(); L1_SIZE],
            overflow: ptr::null_mut(),
            current_ms: now,
        }),
    });
    Box::into_raw(tw)
}

/// Destroy a timer wheel, freeing all pending entries.
///
/// # Safety
///
/// `tw` must be a valid pointer returned by [`hew_timer_wheel_new`] and must
/// not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_timer_wheel_free(tw: *mut HewTimerWheel) {
    if tw.is_null() {
        return;
    }
    // SAFETY: caller guarantees `tw` is valid and surrenders ownership.
    let wheel = unsafe { Box::from_raw(tw) };
    let mut w = wheel
        .inner
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    for slot in &mut w.l0 {
        free_entry_list(*slot);
        *slot = ptr::null_mut();
    }
    for slot in &mut w.l1 {
        free_entry_list(*slot);
        *slot = ptr::null_mut();
    }
    free_entry_list(w.overflow);
    w.overflow = ptr::null_mut();
    // Mutex + Box dropped automatically.
}

/// Schedule a timer to fire after `delay_ms` milliseconds.
///
/// Returns a pointer to the entry (for cancellation).
///
/// # Safety
///
/// `tw` must be a valid pointer returned by [`hew_timer_wheel_new`].  `cb`
/// and `data` must remain valid until the timer fires or is cancelled.
#[no_mangle]
pub unsafe extern "C" fn hew_timer_wheel_schedule(
    tw: *mut HewTimerWheel,
    delay_ms: u64,
    cb: HewTimerCb,
    data: *mut c_void,
) -> *mut HewTimerEntry {
    if tw.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees `tw` is valid.
    let wheel = unsafe { &*tw };
    let mut w = wheel
        .inner
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);

    let entry = Box::into_raw(Box::new(HewTimerEntry {
        deadline_ms: w.current_ms + delay_ms,
        cb: Some(cb),
        data,
        cancelled: 0,
        next: ptr::null_mut(),
    }));

    insert_entry(&mut w, entry);
    entry
}

/// Mark a timer entry as cancelled.
///
/// # Safety
///
/// `tw` must be valid.  `entry` must have been returned by
/// [`hew_timer_wheel_schedule`] on the same wheel and not yet freed.
#[no_mangle]
pub unsafe extern "C" fn hew_timer_wheel_cancel(tw: *mut HewTimerWheel, entry: *mut HewTimerEntry) {
    if tw.is_null() || entry.is_null() {
        return;
    }
    // SAFETY: caller guarantees both pointers are valid.
    let wheel = unsafe { &*tw };
    let _w = wheel
        .inner
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    // SAFETY: entry is valid per caller contract; accessed under the lock.
    unsafe {
        (*entry).cancelled = 1;
    }
}

/// Advance the wheel to the current time and fire all expired timers.
///
/// Returns the total number of timers fired.
///
/// # Safety
///
/// `tw` must be a valid pointer returned by [`hew_timer_wheel_new`].  All
/// callback/data pairs must still be valid.
#[no_mangle]
pub unsafe extern "C" fn hew_timer_wheel_tick(tw: *mut HewTimerWheel) -> c_int {
    if tw.is_null() {
        return 0;
    }
    // SAFETY: hew_now_ms has no preconditions; caller guarantees `tw` is valid.
    let now = unsafe { hew_now_ms() };
    // SAFETY: caller guarantees `tw` is valid.
    let wheel = unsafe { &*tw };

    // Collect expired entries under the lock, then fire them after releasing
    // it.  This prevents deadlock when a callback schedules or cancels a
    // timer on the same wheel.
    let mut expired: Vec<ExpiredEntry> = Vec::new();

    {
        let mut w = wheel
            .inner
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        while w.current_ms < now {
            w.current_ms += 1;

            #[expect(
                clippy::cast_possible_truncation,
                reason = "masked to L0_SIZE which fits in usize"
            )]
            let l0_slot = (w.current_ms / L0_MS) as usize & (L0_SIZE - 1);
            collect_expired_entries(&raw mut w.l0[l0_slot], w.current_ms, &mut expired);

            // Cascade at L1 boundary (every 256 ms).
            if l0_slot == 0 {
                cascade_l1_to_l0(&mut w);

                let l1_slot = (w.current_ms / L1_MS) as usize & (L1_SIZE - 1);
                if l1_slot == 0 {
                    cascade_overflow(&mut w);
                }
            }
        }

        // Collect any overflow entries past their deadline.
        while !w.overflow.is_null() {
            // SAFETY: overflow head is valid under lock.
            if unsafe { (*w.overflow).deadline_ms } > now {
                break;
            }
            let entry = w.overflow;
            // SAFETY: unlinking overflow head; accessing fields under lock.
            unsafe {
                w.overflow = (*entry).next;
                expired.push(ExpiredEntry {
                    cb: (*entry).cb,
                    data: (*entry).data,
                    cancelled: (*entry).cancelled,
                    raw: entry,
                });
            }
        }
        // Lock is released here.
    }

    // Fire callbacks and free entries outside the lock.
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

/// Return milliseconds until the next pending timer, or −1 if none exist.
///
/// # Safety
///
/// `tw` must be a valid pointer returned by [`hew_timer_wheel_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_timer_wheel_next_deadline_ms(tw: *mut HewTimerWheel) -> i64 {
    if tw.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `tw` is valid.
    let wheel = unsafe { &*tw };
    let w = wheel
        .inner
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let now = w.current_ms;
    let mut earliest: u64 = u64::MAX;

    // Scan L0.
    for slot in &w.l0 {
        let mut e = *slot;
        while !e.is_null() {
            // SAFETY: entry is valid under lock.
            unsafe {
                if (*e).cancelled == 0 && (*e).deadline_ms < earliest {
                    earliest = (*e).deadline_ms;
                }
                e = (*e).next;
            }
        }
    }

    // Check L1 only if L0 had nothing.
    if earliest == u64::MAX {
        for slot in &w.l1 {
            let mut e = *slot;
            while !e.is_null() {
                // SAFETY: entry is valid under lock.
                unsafe {
                    if (*e).cancelled == 0 && (*e).deadline_ms < earliest {
                        earliest = (*e).deadline_ms;
                    }
                    e = (*e).next;
                }
            }
        }
    }

    // Check overflow.
    if earliest == u64::MAX && !w.overflow.is_null() {
        // SAFETY: overflow head is valid under lock.
        unsafe {
            if (*w.overflow).cancelled == 0 {
                earliest = (*w.overflow).deadline_ms;
            }
        }
    }

    drop(w);

    if earliest == u64::MAX {
        return -1;
    }

    #[expect(
        clippy::cast_possible_wrap,
        reason = "monotonic ms values fit in i64 for many centuries"
    )]
    let remaining = earliest as i64 - now as i64;
    if remaining > 0 {
        remaining
    } else {
        0
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicI32, Ordering};

    static FIRE_COUNT: AtomicI32 = AtomicI32::new(0);

    unsafe extern "C" fn test_cb(_data: *mut c_void) {
        FIRE_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    #[test]
    fn schedule_and_tick_immediate() {
        FIRE_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: standard lifecycle calls.
        unsafe {
            let tw = hew_timer_wheel_new();
            assert!(!tw.is_null());
            hew_timer_wheel_schedule(tw, 1, test_cb, ptr::null_mut());
            // Wait for the 1ms timer to expire.
            std::thread::sleep(std::time::Duration::from_millis(5));
            let fired = hew_timer_wheel_tick(tw);
            assert!(fired >= 1);
            assert!(FIRE_COUNT.load(Ordering::SeqCst) >= 1);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn cancel_prevents_firing() {
        FIRE_COUNT.store(0, Ordering::SeqCst);
        // SAFETY: standard lifecycle calls.
        unsafe {
            let tw = hew_timer_wheel_new();
            let e = hew_timer_wheel_schedule(tw, 0, test_cb, ptr::null_mut());
            hew_timer_wheel_cancel(tw, e);
            let fired = hew_timer_wheel_tick(tw);
            assert_eq!(fired, 0);
            assert_eq!(FIRE_COUNT.load(Ordering::SeqCst), 0);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn next_deadline_empty() {
        // SAFETY: standard lifecycle calls.
        unsafe {
            let tw = hew_timer_wheel_new();
            assert_eq!(hew_timer_wheel_next_deadline_ms(tw), -1);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn null_safety() {
        // SAFETY: testing null-pointer guards.
        unsafe {
            assert!(
                hew_timer_wheel_schedule(ptr::null_mut(), 100, test_cb, ptr::null_mut()).is_null()
            );
            hew_timer_wheel_cancel(ptr::null_mut(), ptr::null_mut());
            assert_eq!(hew_timer_wheel_tick(ptr::null_mut()), 0);
            assert_eq!(hew_timer_wheel_next_deadline_ms(ptr::null_mut()), -1);
            hew_timer_wheel_free(ptr::null_mut());
        }
    }
}
