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
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::ffi::{c_int, c_void};
use std::ptr;
use std::sync::{Mutex, OnceLock};

// ---------------------------------------------------------------------------
// Platform clock
//
// On native targets `hew_now_ms` lives in `crate::io_time` (respects the
// deterministic simulation clock).  On wasm32 (and in native tests of the
// cooperative scheduler) the symbol is provided by the host as `extern "C"`
// — on wasm32 this resolves to `wasm_stubs::hew_now_ms`, in native builds to
// `io_time::hew_now_ms` which is already `#[no_mangle]`.
// ---------------------------------------------------------------------------

#[cfg(not(target_arch = "wasm32"))]
fn now_ms_for_wheel() -> u64 {
    // SAFETY: hew_now_ms has no preconditions on native targets.
    unsafe { crate::io_time::hew_now_ms() }
}

#[cfg(target_arch = "wasm32")]
fn now_ms_for_wheel() -> u64 {
    extern "C" {
        fn hew_now_ms() -> u64;
    }
    // SAFETY: symbol is always present on wasm32 (wasm_stubs::hew_now_ms).
    unsafe { hew_now_ms() }
}

/// Timer callback signature matching the C `hew_timer_cb` typedef.
pub type HewTimerCb = unsafe extern "C" fn(*mut c_void);

// ---------------------------------------------------------------------------
// Ticker-notify hook
//
// The ticker thread in `timer_periodic` registers a lightweight `fn()` here
// at startup.  `hew_timer_wheel_schedule_handle` calls it after every insert
// so the ticker re-evaluates its park deadline.  An unconditional call on
// every insert is correct: a spurious wake just re-computes and re-parks.
//
// WHY here: the wheel is the insert site; `timer_periodic` depends on
// `timer_wheel`, so the hook must live in `timer_wheel` to avoid a circular
// dependency.
// ---------------------------------------------------------------------------

static TICKER_NOTIFY_HOOK: OnceLock<fn()> = OnceLock::new();

/// Register the ticker's wakeup function.  Called once from
/// `timer_periodic::start_ticker_thread`.  Subsequent calls are ignored.
pub(crate) fn register_ticker_notify_hook(f: fn()) {
    // OnceLock: only the first registration wins; safe to call again.
    let _ = TICKER_NOTIFY_HOOK.set(f);
}

/// Notify the ticker that a new timer has been inserted.  Called from the
/// schedule path while the wheel lock is **not** held.
#[inline]
fn notify_ticker() {
    if let Some(f) = TICKER_NOTIFY_HOOK.get() {
        f();
    }
}

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
    generation: u64,
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
    next_generation: u64,
}

// SAFETY: All raw pointers are owned exclusively by the wheel and only
// accessed under the Mutex.
unsafe impl Send for WheelInner {}

/// Opaque, heap-allocated timer wheel exposed to C via raw pointer.
#[derive(Debug)]
pub struct HewTimerWheel {
    inner: Mutex<WheelInner>,
}

/// Identity-bearing timer handle used for cancellation.
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct HewTimerHandle {
    pub entry: *mut HewTimerEntry,
    pub generation: u64,
}

impl HewTimerHandle {
    pub(crate) const fn null() -> Self {
        Self {
            entry: ptr::null_mut(),
            generation: 0,
        }
    }
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

fn mark_entry_cancelled_in_list(
    mut head: *mut HewTimerEntry,
    entry: *mut HewTimerEntry,
    generation: u64,
) -> bool {
    while !head.is_null() {
        // SAFETY: `head` is a live entry in the wheel list; caller holds the lock.
        let head_generation = unsafe { (*head).generation };
        if head == entry && head_generation == generation {
            // SAFETY: `head` is a live entry in the wheel list; caller holds the lock.
            unsafe {
                (*head).cancelled = 1;
            }
            return true;
        }
        // SAFETY: walking live wheel entries under the lock.
        unsafe {
            head = (*head).next;
        }
    }
    false
}

fn mark_entry_cancelled(w: &WheelInner, entry: *mut HewTimerEntry, generation: u64) {
    for slot in &w.l0 {
        if mark_entry_cancelled_in_list(*slot, entry, generation) {
            return;
        }
    }
    for slot in &w.l1 {
        if mark_entry_cancelled_in_list(*slot, entry, generation) {
            return;
        }
    }
    let _ = mark_entry_cancelled_in_list(w.overflow, entry, generation);
}

/// Unlink the entry matching `entry`/`generation` from the list rooted at
/// `*slot_head`, free its node, and return its `data` pointer. Returns `None`
/// if no matching live entry is present in this list.
///
/// Caller must hold the lock.
fn remove_entry_from_list(
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

fn remove_entry(w: &mut WheelInner, entry: *mut HewTimerEntry, generation: u64) -> *mut c_void {
    for slot in &mut w.l0 {
        if let Some(data) = remove_entry_from_list(slot, entry, generation) {
            return data;
        }
    }
    for slot in &mut w.l1 {
        if let Some(data) = remove_entry_from_list(slot, entry, generation) {
            return data;
        }
    }
    if let Some(data) = remove_entry_from_list(&mut w.overflow, entry, generation) {
        return data;
    }
    ptr::null_mut()
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
    let now = now_ms_for_wheel();
    let tw = Box::new(HewTimerWheel {
        inner: Mutex::new(WheelInner {
            l0: [ptr::null_mut(); L0_SIZE],
            l1: [ptr::null_mut(); L1_SIZE],
            overflow: ptr::null_mut(),
            current_ms: now,
            next_generation: 1,
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
    cabi_guard!(tw.is_null());
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
/// Returns the raw timer entry pointer. Callers that need to cancel must use
/// [`hew_timer_wheel_schedule_handle`] instead so cancellation can validate the
/// entry generation.
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
    // SAFETY: upheld by this function's caller.
    unsafe { hew_timer_wheel_schedule_handle(tw, delay_ms, cb, data).entry }
}

/// Schedule a timer and return the identity-bearing handle needed to cancel it.
///
/// # Safety
///
/// `tw` must be a valid pointer returned by [`hew_timer_wheel_new`].  `cb`
/// and `data` must remain valid until the timer fires or is cancelled.
#[no_mangle]
pub unsafe extern "C" fn hew_timer_wheel_schedule_handle(
    tw: *mut HewTimerWheel,
    delay_ms: u64,
    cb: HewTimerCb,
    data: *mut c_void,
) -> HewTimerHandle {
    cabi_guard!(tw.is_null(), HewTimerHandle::null());
    // SAFETY: caller guarantees `tw` is valid.
    let wheel = unsafe { &*tw };
    let mut w = wheel
        .inner
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);

    if w.next_generation == 0 {
        crate::set_last_error("timer entry generation space exhausted");
        return HewTimerHandle::null();
    }
    let generation = w.next_generation;
    w.next_generation = w.next_generation.wrapping_add(1);

    let entry = Box::into_raw(Box::new(HewTimerEntry {
        deadline_ms: w.current_ms + delay_ms,
        generation,
        cb: Some(cb),
        data,
        cancelled: 0,
        next: ptr::null_mut(),
    }));

    insert_entry(&mut w, entry);
    let handle = HewTimerHandle { entry, generation };
    // Release the wheel lock before notifying the ticker so the ticker can
    // immediately call hew_timer_wheel_next_deadline_ms without contending.
    drop(w);
    notify_ticker();
    handle
}

/// Mark a timer entry as cancelled.
///
/// # Safety
///
/// `tw` must be valid.  `entry` must have been returned by
/// [`hew_timer_wheel_schedule_handle`] on the same wheel and `generation` must
/// be the generation returned in the same handle.  Concurrent deadline firing
/// may already have unlinked and freed `entry`; stale entries are ignored after
/// validating liveness and identity under the wheel lock.
#[no_mangle]
pub unsafe extern "C" fn hew_timer_wheel_cancel(
    tw: *mut HewTimerWheel,
    entry: *mut HewTimerEntry,
    generation: u64,
) {
    cabi_guard!(tw.is_null() || entry.is_null() || generation == 0);
    // SAFETY: caller guarantees `tw` is valid.
    let wheel = unsafe { &*tw };
    let w = wheel
        .inner
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    mark_entry_cancelled(&w, entry, generation);
}

/// Remove a timer entry from the wheel and return its `data` pointer so the
/// caller can reclaim ownership of the callback payload.
///
/// Returns null if no matching live entry is present (it already fired, was
/// collected for firing, or was never scheduled on this wheel). Unlike
/// [`hew_timer_wheel_cancel`], this unlinks and frees the entry node
/// immediately and hands the `data` pointer back to the caller — the wheel no
/// longer references it. Removal is atomic with deadline firing: an entry that
/// the tick has already collected (unlinked) will not be found here, so the
/// payload is reclaimed by exactly one party (this call or the firing
/// callback), never both.
///
/// # Safety
///
/// `tw` must be valid. `entry` must have been returned by
/// [`hew_timer_wheel_schedule_handle`] on the same wheel and `generation` must
/// be the generation returned in the same handle.
#[no_mangle]
pub unsafe extern "C" fn hew_timer_wheel_remove(
    tw: *mut HewTimerWheel,
    entry: *mut HewTimerEntry,
    generation: u64,
) -> *mut c_void {
    cabi_guard!(
        tw.is_null() || entry.is_null() || generation == 0,
        ptr::null_mut()
    );
    // SAFETY: caller guarantees `tw` is valid.
    let wheel = unsafe { &*tw };
    let mut w = wheel
        .inner
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    remove_entry(&mut w, entry, generation)
}

pub(crate) unsafe fn timer_wheel_tick_to(tw: *mut HewTimerWheel, now: u64) -> c_int {
    cabi_guard!(tw.is_null(), 0);
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
    let now = now_ms_for_wheel();
    // SAFETY: upheld by this function's caller.
    unsafe { timer_wheel_tick_to(tw, now) }
}

/// Return milliseconds until the next pending timer, or −1 if none exist.
///
/// # Safety
///
/// `tw` must be a valid pointer returned by [`hew_timer_wheel_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_timer_wheel_next_deadline_ms(tw: *mut HewTimerWheel) -> i64 {
    cabi_guard!(tw.is_null(), -1);
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

    // Check overflow: scan past cancelled heads to find the first live entry.
    // The overflow list is sorted by deadline_ms ascending.  A cancelled head
    // must not shadow a live entry further down the list — if it did,
    // next_deadline_ms would return -1 and the ticker would park indefinitely,
    // never waking to fire the live timer.
    if earliest == u64::MAX {
        let mut e = w.overflow;
        while !e.is_null() {
            // SAFETY: overflow list nodes are valid under lock.
            unsafe {
                if (*e).cancelled == 0 {
                    earliest = (*e).deadline_ms;
                    break;
                }
                e = (*e).next;
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

/// Test-only: return the earliest **absolute** deadline (`deadline_ms`) of any
/// live entry, or `None` when the wheel is empty.
///
/// Unlike [`hew_timer_wheel_next_deadline_ms`], which returns the delay
/// *relative* to the wheel's `current_ms`, this returns the absolute fire time
/// an entry was scheduled at. Timer-wheel tests must drive
/// `hew_wasm_timer_tick` with this absolute value: the wheel fires an entry
/// only once its `current_ms` reaches the entry's absolute `deadline_ms`, and
/// on native (where the wasm virtual-clock seam is inert) that value is
/// `current_ms_at_creation + delay`, which drifts from any freshly re-derived
/// `now + delay`.
///
/// # Safety
///
/// `tw` must be null or a valid pointer returned by [`hew_timer_wheel_new`].
#[cfg(test)]
pub(crate) unsafe fn timer_wheel_earliest_abs_deadline_ms(tw: *mut HewTimerWheel) -> Option<u64> {
    if tw.is_null() {
        return None;
    }
    // SAFETY: caller guarantees `tw` is valid.
    let wheel = unsafe { &*tw };
    let w = wheel
        .inner
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let mut earliest: u64 = u64::MAX;
    let mut scan = |mut e: *mut HewTimerEntry| {
        while !e.is_null() {
            // SAFETY: entries are valid under lock.
            unsafe {
                if (*e).cancelled == 0 && (*e).deadline_ms < earliest {
                    earliest = (*e).deadline_ms;
                }
                e = (*e).next;
            }
        }
    };
    for slot in &w.l0 {
        scan(*slot);
    }
    for slot in &w.l1 {
        scan(*slot);
    }
    scan(w.overflow);
    if earliest == u64::MAX {
        None
    } else {
        Some(earliest)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicI32, Ordering};

    /// Per-test fire counter passed through the timer callback's `data`
    /// pointer. Using a callback-local counter instead of a shared `static`
    /// keeps each test's fire count isolated: the timer callbacks run under a
    /// parallel test runner, so a module-global counter let a sibling test's
    /// fire (`schedule_and_tick_immediate`) leak into `cancel_prevents_firing`
    /// and flip its `== 0` assertion (a pre-existing cross-test race).
    unsafe extern "C" fn test_cb(data: *mut c_void) {
        if !data.is_null() {
            // SAFETY: every scheduler in these tests passes a pointer to a
            // live `AtomicI32` that outlives the tick that fires the callback.
            let counter = unsafe { &*data.cast::<AtomicI32>() };
            counter.fetch_add(1, Ordering::SeqCst);
        }
    }

    #[test]
    fn schedule_and_tick_immediate() {
        let fire_count = AtomicI32::new(0);
        let data = (&raw const fire_count).cast::<c_void>().cast_mut();
        // SAFETY: standard lifecycle calls.
        unsafe {
            let tw = hew_timer_wheel_new();
            assert!(!tw.is_null());
            hew_timer_wheel_schedule(tw, 1, test_cb, data);
            // Wait for the 1ms timer to expire.
            std::thread::sleep(std::time::Duration::from_millis(5));
            let fired = hew_timer_wheel_tick(tw);
            assert!(fired >= 1);
            assert!(fire_count.load(Ordering::SeqCst) >= 1);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn cancel_prevents_firing() {
        let fire_count = AtomicI32::new(0);
        let data = (&raw const fire_count).cast::<c_void>().cast_mut();
        // SAFETY: standard lifecycle calls.
        unsafe {
            let tw = hew_timer_wheel_new();
            let h = hew_timer_wheel_schedule_handle(tw, 0, test_cb, data);
            hew_timer_wheel_cancel(tw, h.entry, h.generation);
            let fired = hew_timer_wheel_tick(tw);
            assert_eq!(fired, 0);
            assert_eq!(fire_count.load(Ordering::SeqCst), 0);
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
            assert!(hew_timer_wheel_schedule_handle(
                ptr::null_mut(),
                100,
                test_cb,
                ptr::null_mut()
            )
            .entry
            .is_null());
            hew_timer_wheel_cancel(ptr::null_mut(), ptr::null_mut(), 0);
            assert_eq!(hew_timer_wheel_tick(ptr::null_mut()), 0);
            assert_eq!(hew_timer_wheel_next_deadline_ms(ptr::null_mut()), -1);
            hew_timer_wheel_free(ptr::null_mut());
        }
    }

    // ── Helpers for deterministic (wall-clock-free) tests ─────────────

    fn make_wheel_at(current_ms: u64) -> WheelInner {
        WheelInner {
            l0: [ptr::null_mut(); L0_SIZE],
            l1: [ptr::null_mut(); L1_SIZE],
            overflow: ptr::null_mut(),
            current_ms,
            next_generation: 1,
        }
    }

    fn make_entry_at(deadline_ms: u64) -> *mut HewTimerEntry {
        Box::into_raw(Box::new(HewTimerEntry {
            deadline_ms,
            generation: 1,
            cb: None,
            data: ptr::null_mut(),
            cancelled: 0,
            next: ptr::null_mut(),
        }))
    }

    fn count_list(mut head: *mut HewTimerEntry) -> usize {
        let mut n = 0;
        while !head.is_null() {
            n += 1;
            // SAFETY: head is a valid entry in a test-owned list.
            unsafe {
                head = (*head).next;
            }
        }
        n
    }

    fn collect_deadlines(mut head: *mut HewTimerEntry) -> Vec<u64> {
        let mut out = Vec::new();
        while !head.is_null() {
            // SAFETY: head is a valid entry in a test-owned list.
            unsafe {
                out.push((*head).deadline_ms);
                head = (*head).next;
            }
        }
        out
    }

    fn free_wheel(w: &mut WheelInner) {
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
    }

    /// Construct a `HewTimerWheel` at a fixed time, bypassing `hew_now_ms`.
    fn make_timer_wheel(current_ms: u64) -> *mut HewTimerWheel {
        Box::into_raw(Box::new(HewTimerWheel {
            inner: Mutex::new(WheelInner {
                l0: [ptr::null_mut(); L0_SIZE],
                l1: [ptr::null_mut(); L1_SIZE],
                overflow: ptr::null_mut(),
                current_ms,
                next_generation: 1,
            }),
        }))
    }

    // ── insert_entry: level placement ─────────────────────────────────

    #[test]
    fn insert_zero_delta_lands_in_l0() {
        let mut w = make_wheel_at(0);
        let e = make_entry_at(0);
        insert_entry(&mut w, e);
        assert_eq!(count_list(w.l0[0]), 1);
        free_wheel(&mut w);
    }

    #[test]
    fn insert_small_delta_lands_in_l0() {
        let mut w = make_wheel_at(0);
        let e = make_entry_at(100);
        insert_entry(&mut w, e);
        assert_eq!(count_list(w.l0[100]), 1);
        free_wheel(&mut w);
    }

    #[test]
    fn insert_l0_max_boundary() {
        let mut w = make_wheel_at(0);
        let e = make_entry_at(255);
        insert_entry(&mut w, e);
        assert_eq!(count_list(w.l0[255]), 1);
        for slot in &w.l1 {
            assert!((*slot).is_null());
        }
        assert!(w.overflow.is_null());
        free_wheel(&mut w);
    }

    #[test]
    fn insert_l1_min_boundary() {
        let mut w = make_wheel_at(0);
        let e = make_entry_at(256);
        insert_entry(&mut w, e);
        // slot = (256 / 256) % 64 = 1
        assert_eq!(count_list(w.l1[1]), 1);
        for slot in &w.l0 {
            assert!((*slot).is_null());
        }
        free_wheel(&mut w);
    }

    #[test]
    fn insert_l1_max_boundary() {
        let mut w = make_wheel_at(0);
        let e = make_entry_at(16383);
        insert_entry(&mut w, e);
        // slot = (16383 / 256) % 64 = 63
        assert_eq!(count_list(w.l1[63]), 1);
        assert!(w.overflow.is_null());
        free_wheel(&mut w);
    }

    #[test]
    fn insert_overflow_min_boundary() {
        let mut w = make_wheel_at(0);
        let e = make_entry_at(16384);
        insert_entry(&mut w, e);
        assert_eq!(count_list(w.overflow), 1);
        for slot in &w.l0 {
            assert!((*slot).is_null());
        }
        for slot in &w.l1 {
            assert!((*slot).is_null());
        }
        free_wheel(&mut w);
    }

    #[test]
    fn insert_overflow_large_delta() {
        let mut w = make_wheel_at(0);
        let e = make_entry_at(100_000);
        insert_entry(&mut w, e);
        assert_eq!(count_list(w.overflow), 1);
        free_wheel(&mut w);
    }

    #[test]
    fn multiple_entries_same_l0_slot() {
        let mut w = make_wheel_at(0);
        insert_entry(&mut w, make_entry_at(42));
        insert_entry(&mut w, make_entry_at(42));
        assert_eq!(count_list(w.l0[42]), 2);
        free_wheel(&mut w);
    }

    #[test]
    fn overflow_sorted_by_deadline() {
        let mut w = make_wheel_at(0);
        insert_entry(&mut w, make_entry_at(30_000));
        insert_entry(&mut w, make_entry_at(20_000));
        insert_entry(&mut w, make_entry_at(25_000));
        assert_eq!(collect_deadlines(w.overflow), vec![20_000, 25_000, 30_000]);
        free_wheel(&mut w);
    }

    #[test]
    fn insert_with_nonzero_current_ms() {
        let mut w = make_wheel_at(1000);
        let e = make_entry_at(1050); // delta = 50 → L0, slot = 1050 & 255 = 26
        insert_entry(&mut w, e);
        assert_eq!(count_list(w.l0[26]), 1);
        free_wheel(&mut w);
    }

    // ── collect_expired_entries ────────────────────────────────────────

    #[test]
    fn collect_expired_fires_due_entries() {
        let e2 = make_entry_at(15);
        let e1 = make_entry_at(5);
        // SAFETY: building a test list; both entries are valid.
        unsafe {
            (*e1).next = e2;
        }
        let mut slot_head = e1;

        let mut expired = Vec::new();
        collect_expired_entries(&raw mut slot_head, 5, &mut expired);
        assert_eq!(expired.len(), 1); // deadline 5 ≤ now(5)
        assert_eq!(count_list(slot_head), 1); // deadline 15 stays

        for e in expired {
            // SAFETY: freeing collected entries.
            unsafe {
                drop(Box::from_raw(e.raw));
            }
        }
        free_entry_list(slot_head);
    }

    #[test]
    fn collect_expired_leaves_future_entries() {
        let e = make_entry_at(100);
        let mut slot_head = e;

        let mut expired = Vec::new();
        collect_expired_entries(&raw mut slot_head, 50, &mut expired);
        assert!(expired.is_empty());
        assert_eq!(count_list(slot_head), 1);
        free_entry_list(slot_head);
    }

    #[test]
    fn collect_expired_mixed_deadlines() {
        // Three entries: 5 (due), 10 (due at boundary), 20 (future). now = 10.
        let e3 = make_entry_at(20);
        let e2 = make_entry_at(10);
        let e1 = make_entry_at(5);
        // SAFETY: building a test list.
        unsafe {
            (*e1).next = e2;
            (*e2).next = e3;
        }
        let mut slot_head = e1;

        let mut expired = Vec::new();
        collect_expired_entries(&raw mut slot_head, 10, &mut expired);
        assert_eq!(expired.len(), 2);
        assert_eq!(count_list(slot_head), 1);
        // SAFETY: reading the remaining entry's deadline.
        unsafe {
            assert_eq!((*slot_head).deadline_ms, 20);
        }

        for e in expired {
            // SAFETY: freeing collected entries.
            unsafe {
                drop(Box::from_raw(e.raw));
            }
        }
        free_entry_list(slot_head);
    }

    #[test]
    fn collect_expired_cancelled_entry_still_collected() {
        let e = make_entry_at(5);
        // SAFETY: setting cancelled flag on a test-owned entry.
        unsafe {
            (*e).cancelled = 1;
        }
        let mut slot_head = e;

        let mut expired = Vec::new();
        collect_expired_entries(&raw mut slot_head, 10, &mut expired);
        // Cancelled entries are still unlinked; caller decides whether to fire.
        assert_eq!(expired.len(), 1);
        assert_eq!(expired[0].cancelled, 1);

        for e in expired {
            // SAFETY: freeing collected entries.
            unsafe {
                drop(Box::from_raw(e.raw));
            }
        }
    }

    // ── cascade ───────────────────────────────────────────────────────

    #[test]
    fn cascade_l1_to_l0_redistributes() {
        let mut w = make_wheel_at(256);
        // Entry deadline 300 in L1 slot 1 → after cascade, delta = 44 → L0 slot 44.
        let e = make_entry_at(300);
        w.l1[1] = e;

        cascade_l1_to_l0(&mut w);
        assert!(w.l1[1].is_null());
        assert_eq!(count_list(w.l0[44]), 1);
        free_wheel(&mut w);
    }

    #[test]
    fn cascade_overflow_moves_entries_in_range() {
        let mut w = make_wheel_at(0);
        let e3 = make_entry_at(40_000);
        let e2 = make_entry_at(20_000);
        let e1 = make_entry_at(16_384);
        // SAFETY: building a sorted overflow list.
        unsafe {
            (*e1).next = e2;
            (*e2).next = e3;
        }
        w.overflow = e1;
        w.current_ms = 16_384;

        cascade_overflow(&mut w);
        // max_deadline = 32768. e1,e2 < 32768 → cascaded. e3 stays.
        assert_eq!(count_list(w.overflow), 1);
        // SAFETY: reading remaining overflow entry.
        unsafe {
            assert_eq!((*w.overflow).deadline_ms, 40_000);
        }
        // e1 (16384): delta=0 → L0 slot 0. e2 (20000): delta=3616 → L1 slot 14.
        assert_eq!(count_list(w.l0[0]), 1);
        assert_eq!(count_list(w.l1[14]), 1);
        free_wheel(&mut w);
    }

    // ── C ABI: schedule + next_deadline (controlled time) ─────────────

    #[test]
    fn schedule_via_cabi_lands_in_l0() {
        // SAFETY: using make_timer_wheel for deterministic time.
        unsafe {
            let tw = make_timer_wheel(1000);
            hew_timer_wheel_schedule(tw, 50, test_cb, ptr::null_mut());
            // deadline = 1050, slot = 1050 & 255 = 26
            let w = (*tw).inner.lock().unwrap();
            assert_eq!(count_list(w.l0[26]), 1);
            drop(w);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn schedule_via_cabi_lands_in_l1() {
        // SAFETY: using make_timer_wheel for deterministic time.
        unsafe {
            let tw = make_timer_wheel(1000);
            hew_timer_wheel_schedule(tw, 500, test_cb, ptr::null_mut());
            // deadline = 1500, delta = 500 → L1 slot = (1500 / 256) % 64 = 5
            let w = (*tw).inner.lock().unwrap();
            assert_eq!(count_list(w.l1[5]), 1);
            drop(w);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn next_deadline_returns_soonest_timer() {
        // SAFETY: using make_timer_wheel for deterministic time.
        unsafe {
            let tw = make_timer_wheel(1000);
            hew_timer_wheel_schedule(tw, 100, test_cb, ptr::null_mut());
            hew_timer_wheel_schedule(tw, 50, test_cb, ptr::null_mut());
            hew_timer_wheel_schedule(tw, 200, test_cb, ptr::null_mut());
            assert_eq!(hew_timer_wheel_next_deadline_ms(tw), 50);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn next_deadline_skips_cancelled_entries() {
        // SAFETY: using make_timer_wheel for deterministic time.
        unsafe {
            let tw = make_timer_wheel(1000);
            let h = hew_timer_wheel_schedule_handle(tw, 10, test_cb, ptr::null_mut());
            hew_timer_wheel_schedule(tw, 100, test_cb, ptr::null_mut());
            hew_timer_wheel_cancel(tw, h.entry, h.generation);
            assert_eq!(hew_timer_wheel_next_deadline_ms(tw), 100);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn next_deadline_all_cancelled_returns_minus_one() {
        // SAFETY: using make_timer_wheel for deterministic time.
        unsafe {
            let tw = make_timer_wheel(0);
            let h1 = hew_timer_wheel_schedule_handle(tw, 10, test_cb, ptr::null_mut());
            let h2 = hew_timer_wheel_schedule_handle(tw, 500, test_cb, ptr::null_mut());
            hew_timer_wheel_cancel(tw, h1.entry, h1.generation);
            hew_timer_wheel_cancel(tw, h2.entry, h2.generation);
            assert_eq!(hew_timer_wheel_next_deadline_ms(tw), -1);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn next_deadline_l1_when_l0_empty() {
        // SAFETY: using make_timer_wheel for deterministic time.
        unsafe {
            let tw = make_timer_wheel(1000);
            hew_timer_wheel_schedule(tw, 500, test_cb, ptr::null_mut());
            assert_eq!(hew_timer_wheel_next_deadline_ms(tw), 500);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn next_deadline_overflow_when_l0_l1_empty() {
        // SAFETY: using make_timer_wheel for deterministic time.
        unsafe {
            let tw = make_timer_wheel(1000);
            hew_timer_wheel_schedule(tw, 20_000, test_cb, ptr::null_mut());
            assert_eq!(hew_timer_wheel_next_deadline_ms(tw), 20_000);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn free_cleans_all_levels() {
        // Schedule entries across L0, L1, and overflow, then free.
        // SAFETY: standard lifecycle; leak would be caught by Miri/ASAN.
        unsafe {
            let tw = make_timer_wheel(0);
            hew_timer_wheel_schedule(tw, 10, test_cb, ptr::null_mut());
            hew_timer_wheel_schedule(tw, 500, test_cb, ptr::null_mut());
            hew_timer_wheel_schedule(tw, 20_000, test_cb, ptr::null_mut());
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn cancel_idempotent() {
        // SAFETY: double-cancel must not panic.
        unsafe {
            let tw = make_timer_wheel(0);
            let h = hew_timer_wheel_schedule_handle(tw, 100, test_cb, ptr::null_mut());
            hew_timer_wheel_cancel(tw, h.entry, h.generation);
            hew_timer_wheel_cancel(tw, h.entry, h.generation);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn probe_b_stale_cancel_does_not_cancel_reused_entry_address() {
        let fire_count = AtomicI32::new(0);
        let data = (&raw const fire_count).cast::<c_void>().cast_mut();
        // SAFETY: this test reuses one allocation slot with explicit lifetime
        // boundaries to model allocator reuse of E's address for E2.
        unsafe {
            let mut storage = Box::new(std::mem::MaybeUninit::<HewTimerEntry>::uninit());
            let entry = storage.as_mut_ptr().cast::<HewTimerEntry>();
            ptr::write(
                entry,
                HewTimerEntry {
                    deadline_ms: 1,
                    generation: 41,
                    cb: Some(test_cb),
                    data,
                    cancelled: 0,
                    next: ptr::null_mut(),
                },
            );
            let stale_entry = entry;
            let stale_generation = (*entry).generation;
            ptr::drop_in_place(entry);

            ptr::write(
                entry,
                HewTimerEntry {
                    deadline_ms: 10,
                    generation: 42,
                    cb: Some(test_cb),
                    data,
                    cancelled: 0,
                    next: ptr::null_mut(),
                },
            );
            let mut w = make_wheel_at(0);
            insert_entry(&mut w, entry);
            mark_entry_cancelled(&w, stale_entry, stale_generation);
            assert_eq!(
                (*entry).cancelled,
                0,
                "stale generation must not cancel a new live entry at the same address"
            );

            let mut expired = Vec::new();
            collect_expired_entries(&raw mut w.l0[10], 10, &mut expired);
            assert_eq!(expired.len(), 1);
            let expired_entry = expired.pop().unwrap();
            assert_eq!(expired_entry.cancelled, 0);
            if let Some(cb) = expired_entry.cb {
                cb(expired_entry.data);
            }
            assert_eq!(fire_count.load(Ordering::SeqCst), 1);
            ptr::drop_in_place(entry);
        }
    }

    #[test]
    fn cancel_after_tick_free_is_ignored() {
        let fire_count = AtomicI32::new(0);
        let data = (&raw const fire_count).cast::<c_void>().cast_mut();
        // SAFETY: the stale cancel models a source-completion thread that
        // already captured the timer entry before the deadline thread fired it.
        unsafe {
            let tw = make_timer_wheel(0);
            let h = hew_timer_wheel_schedule_handle(tw, 1, test_cb, data);

            assert_eq!(timer_wheel_tick_to(tw, 1), 1);
            assert_eq!(fire_count.load(Ordering::SeqCst), 1);

            hew_timer_wheel_cancel(tw, h.entry, h.generation);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn next_deadline_past_due_returns_zero() {
        // Entry with deadline already in the past relative to current_ms.
        // SAFETY: using make_timer_wheel for deterministic time.
        unsafe {
            let tw = make_timer_wheel(500);
            // Schedule with 0 delay → deadline = 500. Wheel is at 500.
            hew_timer_wheel_schedule(tw, 0, test_cb, ptr::null_mut());
            // earliest == current_ms → remaining = 0
            assert_eq!(hew_timer_wheel_next_deadline_ms(tw), 0);
            hew_timer_wheel_free(tw);
        }
    }
}
