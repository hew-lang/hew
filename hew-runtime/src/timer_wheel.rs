//! Two-level hierarchical timer wheel for the Hew runtime.
//!
//! * **Level 0:** 256 slots × 1 ms = 256 ms range
//! * **Level 1:** 64 slots × 256 ms ≈ 16.4 s range
//! * **Overflow:** sorted linked list for timers > 16.4 s
//!
//! Entries retain nanosecond fractions; buckets index time without rounding
//! deadlines. A tick only collects entries whose exact deadline has elapsed.
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

use crate::runtime_sync::Mutex;
use crate::timer_deadline::Deadline;
use std::ffi::{c_int, c_void};
use std::ptr;
use std::sync::OnceLock;

#[cfg(kani)]
#[path = "../../proofs/runtime_protocols/src/wheel_registration.rs"]
mod formal_registration;

// ---------------------------------------------------------------------------
// Platform clock
//
// On native targets `hew_now_ms` lives in `crate::io_time` (respects the
// deterministic simulation clock).  On wasm32 (and in native tests of the
// cooperative scheduler) the symbol is provided by the host as `extern "C"`
// — on wasm32 this resolves to `wasm_stubs::hew_now_ms`, in native builds to
// `clock::hew_now_ms` which is already `#[no_mangle]`.
// ---------------------------------------------------------------------------

#[cfg(not(target_arch = "wasm32"))]
fn now_ms_for_wheel() -> u64 {
    // SAFETY: hew_now_ms has no preconditions on native targets.
    unsafe { crate::clock::hew_now_ms() }
}

#[cfg(target_arch = "wasm32")]
fn now_ms_for_wheel() -> u64 {
    extern "C" {
        fn hew_now_ms() -> u64;
    }
    // SAFETY: symbol is always present on wasm32 (wasm_stubs::hew_now_ms).
    unsafe { hew_now_ms() }
}

pub(crate) fn now_for_wheel() -> Deadline {
    // SAFETY: monotonic clock sampling has no preconditions.
    Deadline::from_nanos(
        unsafe { crate::clock::hew_instant_now() }
            .max(0)
            .cast_unsigned(),
    )
}

/// Timer callback signature matching the C `hew_timer_cb` typedef.
pub type HewTimerCb = unsafe extern "C" fn(*mut c_void);

// ---------------------------------------------------------------------------
// Insert hook
//
// The reactor ticks the process wheel and sleeps until its next deadline.
// `timer_periodic` registers a hook here that wakes the reactor when an insert
// lands before that deadline. The hook lives in `timer_wheel` because the
// wheel is the insert site and `timer_periodic` depends on `timer_wheel`.
// ---------------------------------------------------------------------------

type InsertHook = fn(*mut HewTimerWheel, u64);

static INSERT_HOOK: OnceLock<InsertHook> = OnceLock::new();

/// Register the process wheel's insert hook. Only the first registration
/// takes effect.
//
// KEEP(wasm32): wasm32 ticks its wheel from the host driver and never
// registers, so `notify_inserted` is a no-op there.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn register_insert_hook(hook: InsertHook) {
    let _ = INSERT_HOOK.set(hook);
}

/// Report an insert at `deadline_ns`. Called with the wheel lock released.
#[inline]
fn notify_inserted(tw: *mut HewTimerWheel, deadline_ns: u64) {
    if let Some(hook) = INSERT_HOOK.get() {
        hook(tw, deadline_ns);
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

#[path = "timer_entry.rs"]
mod entry;
pub use entry::HewTimerEntry;
use entry::{
    collect_expired_entries_at, fire_expired_entries, free_entry_list, remove_entry_from_list,
    ExpiredEntry,
};

/// Internal state protected by a [`Mutex`].
#[derive(Debug)]
struct WheelInner {
    l0: [*mut HewTimerEntry; L0_SIZE],
    l1: [*mut HewTimerEntry; L1_SIZE],
    overflow: *mut HewTimerEntry,
    current_ms: u64,
    current_fraction_ns: u32,
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
            while !(*pp).is_null() && (**pp).deadline() <= (*entry).deadline() {
                pp = &raw mut (**pp).next;
            }
            (*entry).next = *pp;
            *pp = entry;
        }
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
            current_fraction_ns: 0,
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

#[derive(Clone, Copy)]
enum TimerSchedule {
    After(u64),
    #[cfg(any(test, hew_protocol_host))]
    At(u64),
    Precise(Deadline),
}

/// Advance across a long reactor stall without visiting every elapsed
/// millisecond. All entries are already linked under this lock, so one scan can
/// separate due callbacks from future entries and re-bucket the latter around
/// the new cursor.
fn advance_large_gap(w: &mut WheelInner, now: Deadline, expired: &mut Vec<ExpiredEntry>) {
    let mut entries = Vec::new();
    let mut take = |mut entry: *mut HewTimerEntry| {
        while !entry.is_null() {
            entries.push(entry);
            // SAFETY: every entry is owned by one wheel list under this lock.
            let next = unsafe { (*entry).next };
            // SAFETY: the entry is detached before it is classified.
            unsafe { (*entry).next = ptr::null_mut() };
            entry = next;
        }
    };
    for slot in &mut w.l0 {
        take(std::mem::replace(slot, ptr::null_mut()));
    }
    for slot in &mut w.l1 {
        take(std::mem::replace(slot, ptr::null_mut()));
    }
    take(std::mem::replace(&mut w.overflow, ptr::null_mut()));
    w.current_ms = now.millis;
    for entry in entries {
        // SAFETY: detached entries remain live and exclusively owned here.
        if unsafe { (*entry).deadline() } <= now {
            // SAFETY: fields are read while this function exclusively owns the
            // detached node; the callback runs after the wheel lock is released.
            unsafe {
                expired.push(ExpiredEntry {
                    cb: (*entry).cb,
                    data: (*entry).data,
                    cancelled: (*entry).cancelled,
                    raw: entry,
                });
            }
        } else {
            insert_entry(w, entry);
        }
    }
    expired.sort_by_key(|entry| {
        // SAFETY: collected nodes stay owned until their callbacks finish.
        unsafe { (*entry.raw).deadline() }
    });
}

unsafe fn timer_wheel_schedule_handle_inner(
    tw: *mut HewTimerWheel,
    schedule: TimerSchedule,
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
    let deadline = match schedule {
        TimerSchedule::After(delay_ms) => {
            let coarse = now_ms_for_wheel();
            let sampled = now_for_wheel();
            let start = sampled.max(Deadline::from_millis(coarse)).max(Deadline {
                millis: w.current_ms,
                fraction_ns: w.current_fraction_ns,
            });
            let Some(millis) = start.millis.checked_add(delay_ms) else {
                crate::set_last_error("timer deadline exceeds wheel range");
                return HewTimerHandle::null();
            };
            Deadline {
                millis,
                fraction_ns: start.fraction_ns,
            }
        }
        // A deadline that elapsed before registration is already due. Keep it
        // in the current slot so the next tick can collect it immediately.
        #[cfg(any(test, hew_protocol_host))]
        TimerSchedule::At(deadline_ms) => Deadline::from_millis(deadline_ms.max(w.current_ms)),
        TimerSchedule::Precise(deadline) => deadline.max(Deadline {
            millis: w.current_ms,
            fraction_ns: w.current_fraction_ns,
        }),
    };
    // `hew_timer_wheel_next_deadline_ms` reserves u64::MAX as its empty-wheel
    // sentinel. Reject that deadline explicitly instead of accepting a timer
    // that the ticker would then treat as absent forever.
    if deadline.millis == u64::MAX {
        crate::set_last_error("timer deadline u64::MAX is reserved");
        return HewTimerHandle::null();
    }
    let generation = w.next_generation;
    w.next_generation = w.next_generation.wrapping_add(1);

    let entry = Box::into_raw(Box::new(HewTimerEntry {
        deadline_ms: deadline.millis,
        fraction_ns: deadline.fraction_ns,
        generation,
        cb: Some(cb),
        data,
        cancelled: 0,
        next: ptr::null_mut(),
    }));

    insert_entry(&mut w, entry);
    let handle = HewTimerHandle { entry, generation };
    // Release the wheel lock before notifying, so the woken reactor can read
    // the next deadline without contending.
    drop(w);
    notify_inserted(tw, deadline.saturated_nanos().min(u64::MAX - 1));
    handle
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
    // SAFETY: upheld by this function's caller.
    unsafe { timer_wheel_schedule_handle_inner(tw, TimerSchedule::After(delay_ms), cb, data) }
}

/// Schedule a timer at an absolute monotonic deadline.
///
/// A deadline already behind the wheel cursor is treated as immediately due
/// and fires on the next tick. Coroutine sleeps and cooperative WASM callers
/// use this form so a lagging wheel cursor cannot shorten their wait.
///
/// # Safety
///
/// `tw` must be a valid pointer returned by [`hew_timer_wheel_new`]. `cb` and
/// `data` must remain valid until the timer fires or is cancelled.
#[cfg(any(test, hew_protocol_host))]
pub(crate) unsafe fn timer_wheel_schedule_at_handle(
    tw: *mut HewTimerWheel,
    deadline_ms: u64,
    cb: HewTimerCb,
    data: *mut c_void,
) -> HewTimerHandle {
    // SAFETY: upheld by this function's caller.
    unsafe { timer_wheel_schedule_handle_inner(tw, TimerSchedule::At(deadline_ms), cb, data) }
}

/// Schedule an exact absolute deadline; wheel buckets do not quantize it.
/// # Safety
/// Same wheel and callback ownership contract as `timer_wheel_schedule_at_handle`.
pub(crate) unsafe fn timer_wheel_schedule_precise_handle(
    tw: *mut HewTimerWheel,
    deadline: Deadline,
    cb: HewTimerCb,
    data: *mut c_void,
) -> HewTimerHandle {
    // SAFETY: caller supplies the live wheel and callback ownership.
    unsafe { timer_wheel_schedule_handle_inner(tw, TimerSchedule::Precise(deadline), cb, data) }
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
    // SAFETY: caller guarantees the wheel remains live.
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

/// Advance `tw` through `now` and synchronously run every callback due in that
/// interval. Returning from this function is the completion boundary for all
/// callbacks included in its fired count.
#[cfg(any(test, hew_protocol_host))]
pub(crate) unsafe fn timer_wheel_tick_to(tw: *mut HewTimerWheel, now: u64) -> c_int {
    // SAFETY: caller supplies the live wheel and callback ownership.
    unsafe { timer_wheel_tick_at(tw, Deadline::from_millis(now)) }
}

/// Advance using exact time, retaining not-yet-due entries in the current bucket.
/// # Safety
/// The wheel and callback data must remain live through this call.
pub(crate) unsafe fn timer_wheel_tick_at(tw: *mut HewTimerWheel, now: Deadline) -> c_int {
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

        let now = now.max(Deadline {
            millis: w.current_ms,
            fraction_ns: w.current_fraction_ns,
        });
        if now.millis.saturating_sub(w.current_ms) > MAX_MS {
            advance_large_gap(&mut w, now, &mut expired);
        }

        // Absolute registrations can already be due when inserted. They are
        // placed in the current slot and must be observable without requiring
        // the cursor to wrap through that slot again.
        #[expect(
            clippy::cast_possible_truncation,
            reason = "masked to L0_SIZE which fits in usize"
        )]
        let current_slot = (w.current_ms / L0_MS) as usize & (L0_SIZE - 1);
        collect_expired_entries_at(&raw mut w.l0[current_slot], now, &mut expired);

        while w.current_ms < now.millis {
            w.current_ms += 1;

            #[expect(
                clippy::cast_possible_truncation,
                reason = "masked to L0_SIZE which fits in usize"
            )]
            let l0_slot = (w.current_ms / L0_MS) as usize & (L0_SIZE - 1);

            // Cascade before collecting the boundary slot. An L1 entry whose
            // deadline is exactly this boundary is redistributed into the
            // current L0 slot and must complete in this tick, not after the
            // slot wraps around again.
            if l0_slot == 0 {
                cascade_l1_to_l0(&mut w);

                let l1_slot = (w.current_ms / L1_MS) as usize & (L1_SIZE - 1);
                if l1_slot == 0 {
                    cascade_overflow(&mut w);
                }
            }

            collect_expired_entries_at(&raw mut w.l0[l0_slot], now, &mut expired);
        }

        // Collect any overflow entries past their deadline.
        while !w.overflow.is_null() {
            // SAFETY: overflow head is valid under lock.
            if unsafe { (*w.overflow).deadline() } > now {
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
        w.current_fraction_ns = now.fraction_ns;
        // Lock is released here.
    }

    fire_expired_entries(expired)
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
    let now = now_for_wheel();
    // SAFETY: upheld by this function's caller.
    unsafe { timer_wheel_tick_at(tw, now) }
}

/// The earliest live deadline in one list, if any.
fn list_earliest(mut entry: *mut HewTimerEntry) -> Option<Deadline> {
    let mut earliest = None;
    while !entry.is_null() {
        // SAFETY: list entries are owned by the wheel; the caller holds its lock.
        unsafe {
            if (*entry).cancelled == 0 {
                earliest = Some(earliest.map_or((*entry).deadline(), |d: Deadline| {
                    d.min((*entry).deadline())
                }));
            }
            entry = (*entry).next;
        }
    }
    earliest
}

/// The earliest live deadline, visiting slots in time order from the cursor
/// and stopping at the first level that holds one. Each level's slots cover
/// consecutive, non-overlapping ranges, so the first non-empty slot holds the
/// level's earliest entry; entries left in a slot the cursor has passed are
/// overdue and found in the cursor's own slot first.
fn earliest_deadline(w: &WheelInner) -> Option<Deadline> {
    #[expect(
        clippy::cast_possible_truncation,
        reason = "masked to the level size, which fits in usize"
    )]
    let l0_start = (w.current_ms / L0_MS) as usize & (L0_SIZE - 1);
    for offset in 0..L0_SIZE {
        if let Some(deadline) = list_earliest(w.l0[(l0_start + offset) & (L0_SIZE - 1)]) {
            return Some(deadline);
        }
    }
    // The current L1 bucket was already cascaded. A newly inserted entry
    // there belongs to the next rotation, so visit that bucket last. Starting
    // there can hide a nearer deadline after a large-gap re-bucket.
    let l1_start = ((w.current_ms / L1_MS) as usize + 1) & (L1_SIZE - 1);
    for offset in 0..L1_SIZE {
        if let Some(deadline) = list_earliest(w.l1[(l1_start + offset) & (L1_SIZE - 1)]) {
            return Some(deadline);
        }
    }
    // Overflow is kept sorted, so its first live entry is the earliest.
    let mut entry = w.overflow;
    while !entry.is_null() {
        // SAFETY: overflow entries are owned by the wheel under its lock.
        unsafe {
            if (*entry).cancelled == 0 {
                return Some((*entry).deadline());
            }
            entry = (*entry).next;
        }
    }
    None
}

/// Return milliseconds until the next pending timer, or −1 if none exist,
/// measured from the wheel cursor.
///
/// # Safety
///
/// `tw` must be a valid pointer returned by [`hew_timer_wheel_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_timer_wheel_next_deadline_ms(tw: *mut HewTimerWheel) -> i64 {
    cabi_guard!(tw.is_null(), -1);
    // SAFETY: caller guarantees `tw` is valid.
    // SAFETY: caller guarantees the wheel remains live.
    let wheel = unsafe { &*tw };
    let w = wheel
        .inner
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let Some(earliest) = earliest_deadline(&w) else {
        return -1;
    };
    let now = w.current_ms;
    drop(w);
    i64::try_from(
        earliest
            .ceil_millis()
            .unwrap_or(u64::MAX)
            .saturating_sub(now),
    )
    .unwrap_or(i64::MAX)
}

#[cfg(test)]
#[allow(dead_code, reason = "used by wasm32-only shutdown tests")]
pub(crate) unsafe fn timer_wheel_earliest_abs_deadline_ms(tw: *mut HewTimerWheel) -> Option<u64> {
    if tw.is_null() {
        return None;
    }
    // SAFETY: caller guarantees `tw` is valid.
    // SAFETY: caller guarantees the wheel remains live.
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

/// Exact earliest pending deadline for native reactors and drivers.
/// # Safety
/// The wheel must be live for this call.
pub(crate) unsafe fn timer_wheel_next_deadline(tw: *mut HewTimerWheel) -> Option<Deadline> {
    if tw.is_null() {
        return None;
    }
    // SAFETY: caller guarantees the wheel remains live.
    let wheel = unsafe { &*tw };
    let w = wheel
        .inner
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    earliest_deadline(&w)
}

/// Test-only: move the wheel cursor forward by `delta_ms` without changing any
/// entries. This models a wheel cursor that was advanced independently of a
/// caller's separately sampled monotonic deadline.
///
/// # Safety
///
/// `tw` must be a valid pointer returned by [`hew_timer_wheel_new`]. The wheel
/// must not be accessed concurrently.
#[cfg(test)]
pub(crate) unsafe fn timer_wheel_advance_cursor_for_test(tw: *mut HewTimerWheel, delta_ms: u64) {
    // SAFETY: caller guarantees `tw` is valid and exclusively accessed.
    let wheel = unsafe { &*tw };
    let mut w = wheel
        .inner
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    w.current_ms = w.current_ms.saturating_add(delta_ms);
    w.current_fraction_ns = 0;
}

/// Read the exact cursor most recently serviced by the wheel.
/// # Safety
/// The wheel must be live.
pub(crate) unsafe fn timer_wheel_cursor(tw: *mut HewTimerWheel) -> Deadline {
    // SAFETY: caller guarantees the wheel remains live.
    let wheel = unsafe { &*tw };
    let w = wheel
        .inner
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    Deadline {
        millis: w.current_ms,
        fraction_ns: w.current_fraction_ns,
    }
}

/// Read the current wheel cursor without advancing it.
///
/// # Safety
///
/// `tw` must be a valid pointer returned by [`hew_timer_wheel_new`].
#[cfg(any(test, hew_protocol_host, target_arch = "wasm32"))]
pub(crate) unsafe fn timer_wheel_cursor_ms(tw: *mut HewTimerWheel) -> u64 {
    // SAFETY: caller guarantees `tw` is valid.
    let wheel = unsafe { &*tw };
    wheel
        .inner
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .current_ms
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::entry::collect_expired_entries;
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
            // Tick the wheel past the 1ms deadline.
            let fired = timer_wheel_tick_to(tw, crate::clock::hew_now_ms().saturating_add(5));
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
            current_fraction_ns: 0,
            next_generation: 1,
        }
    }

    fn make_entry_at(deadline_ms: u64) -> *mut HewTimerEntry {
        Box::into_raw(Box::new(HewTimerEntry {
            deadline_ms,
            fraction_ns: 0,
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
                current_fraction_ns: 0,
                next_generation: 1,
            }),
        }))
    }

    // ── insert_entry: level placement ─────────────────────────────────

    #[test]
    fn past_due_absolute_deadline_fires_once_on_next_tick() {
        let fire_count = AtomicI32::new(0);
        let data = (&raw const fire_count).cast::<c_void>().cast_mut();
        let tw = make_timer_wheel(1_000);

        // SAFETY: `tw` is test-owned and `data` outlives every tick below.
        unsafe {
            let handle = timer_wheel_schedule_at_handle(tw, 900, test_cb, data);
            assert!(!handle.entry.is_null());

            assert_eq!(timer_wheel_tick_to(tw, 1_001), 1);
            assert_eq!(fire_count.load(Ordering::SeqCst), 1);
            assert_eq!(timer_wheel_tick_to(tw, 1_002), 0);
            assert_eq!(fire_count.load(Ordering::SeqCst), 1);

            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn zero_delay_fires_on_next_tick_without_l0_wrap() {
        let fire_count = AtomicI32::new(0);
        let data = (&raw const fire_count).cast::<c_void>().cast_mut();

        // SAFETY: the wheel is test-owned and `data` outlives every tick below.
        unsafe {
            let tw = hew_timer_wheel_new();
            let handle = hew_timer_wheel_schedule_handle(tw, 0, test_cb, data);
            assert!(!handle.entry.is_null());

            let now = now_for_wheel();
            assert!((*handle.entry).deadline() <= now);
            assert_eq!(timer_wheel_tick_at(tw, now), 1);
            assert_eq!(fire_count.load(Ordering::SeqCst), 1);
            assert_eq!(timer_wheel_tick_to(tw, now.millis.saturating_add(255)), 0);
            assert_eq!(fire_count.load(Ordering::SeqCst), 1);

            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn l1_deadline_on_l0_boundary_fires_in_that_tick() {
        let fire_count = AtomicI32::new(0);
        let data = (&raw const fire_count).cast::<c_void>().cast_mut();
        let tw = make_timer_wheel(0);

        // SAFETY: `tw` is test-owned and `data` outlives the synchronous tick.
        unsafe {
            let handle = timer_wheel_schedule_at_handle(tw, L1_MS, test_cb, data);
            assert!(!handle.entry.is_null());

            assert_eq!(timer_wheel_tick_to(tw, L1_MS - 1), 0);
            assert_eq!(fire_count.load(Ordering::SeqCst), 0);
            assert_eq!(timer_wheel_tick_to(tw, L1_MS), 1);
            assert_eq!(fire_count.load(Ordering::SeqCst), 1);

            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn maximum_deadline_is_rejected_at_schedule_boundary() {
        let fire_count = AtomicI32::new(0);
        let data = (&raw const fire_count).cast::<c_void>().cast_mut();
        let tw = make_timer_wheel(1);
        crate::hew_clear_error();

        // SAFETY: `tw` is test-owned and `data` outlives every call below.
        unsafe {
            let relative = hew_timer_wheel_schedule_handle(tw, u64::MAX, test_cb, data);
            assert!(relative.entry.is_null());

            let absolute = timer_wheel_schedule_at_handle(tw, u64::MAX, test_cb, data);
            assert!(absolute.entry.is_null());
            assert_eq!(hew_timer_wheel_next_deadline_ms(tw), -1);
            assert_eq!(fire_count.load(Ordering::SeqCst), 0);

            let error = crate::hew_last_error();
            assert!(!error.is_null());
            assert_eq!(
                std::ffi::CStr::from_ptr(error).to_str().unwrap(),
                "timer deadline u64::MAX is reserved"
            );

            let wheel = &*tw;
            assert_eq!(
                wheel
                    .inner
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .next_generation,
                1
            );
            hew_timer_wheel_free(tw);
        }
    }

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

    // ── Absolute schedule + next_deadline (controlled time) ─────────────

    #[test]
    fn schedule_at_lands_in_l0() {
        // SAFETY: using make_timer_wheel for deterministic time.
        unsafe {
            let tw = make_timer_wheel(1000);
            timer_wheel_schedule_at_handle(tw, 1_050, test_cb, ptr::null_mut());
            // deadline = 1050, slot = 1050 & 255 = 26
            let w = (*tw).inner.lock().unwrap();
            assert_eq!(count_list(w.l0[26]), 1);
            drop(w);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn schedule_at_lands_in_l1() {
        // SAFETY: using make_timer_wheel for deterministic time.
        unsafe {
            let tw = make_timer_wheel(1000);
            timer_wheel_schedule_at_handle(tw, 1_500, test_cb, ptr::null_mut());
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
            timer_wheel_schedule_at_handle(tw, 1_100, test_cb, ptr::null_mut());
            timer_wheel_schedule_at_handle(tw, 1_050, test_cb, ptr::null_mut());
            timer_wheel_schedule_at_handle(tw, 1_200, test_cb, ptr::null_mut());
            assert_eq!(hew_timer_wheel_next_deadline_ms(tw), 50);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn next_deadline_skips_cancelled_entries() {
        // SAFETY: using make_timer_wheel for deterministic time.
        unsafe {
            let tw = make_timer_wheel(1000);
            let h = timer_wheel_schedule_at_handle(tw, 1_010, test_cb, ptr::null_mut());
            timer_wheel_schedule_at_handle(tw, 1_100, test_cb, ptr::null_mut());
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
            let h1 = timer_wheel_schedule_at_handle(tw, 10, test_cb, ptr::null_mut());
            let h2 = timer_wheel_schedule_at_handle(tw, 500, test_cb, ptr::null_mut());
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
            timer_wheel_schedule_at_handle(tw, 1_500, test_cb, ptr::null_mut());
            assert_eq!(hew_timer_wheel_next_deadline_ms(tw), 500);
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn next_deadline_overflow_when_l0_l1_empty() {
        // SAFETY: using make_timer_wheel for deterministic time.
        unsafe {
            let tw = make_timer_wheel(1000);
            timer_wheel_schedule_at_handle(tw, 21_000, test_cb, ptr::null_mut());
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
            timer_wheel_schedule_at_handle(tw, 10, test_cb, ptr::null_mut());
            timer_wheel_schedule_at_handle(tw, 500, test_cb, ptr::null_mut());
            timer_wheel_schedule_at_handle(tw, 20_000, test_cb, ptr::null_mut());
            hew_timer_wheel_free(tw);
        }
    }

    #[test]
    fn cancel_idempotent() {
        // SAFETY: double-cancel must not panic.
        unsafe {
            let tw = make_timer_wheel(0);
            let h = timer_wheel_schedule_at_handle(tw, 100, test_cb, ptr::null_mut());
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
                    fraction_ns: 0,
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
                    fraction_ns: 0,
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
            let h = timer_wheel_schedule_at_handle(tw, 1, test_cb, data);

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
            // An elapsed deadline is clamped to the wheel cursor.
            timer_wheel_schedule_at_handle(tw, 499, test_cb, ptr::null_mut());
            // earliest == current_ms → remaining = 0
            assert_eq!(hew_timer_wheel_next_deadline_ms(tw), 0);
            hew_timer_wheel_free(tw);
        }
    }
    #[test]
    fn next_deadline_after_large_gap_orders_l1_rotation_correctly() {
        // SAFETY: this fixture owns the wheel and uses inert callbacks.
        let (next, fired) = unsafe {
            let wheel = make_timer_wheel(0);
            timer_wheel_schedule_at_handle(wheel, 49_152, test_cb, ptr::null_mut());
            timer_wheel_schedule_at_handle(wheel, 33_259, test_cb, ptr::null_mut());
            let fired = timer_wheel_tick_to(wheel, 32_769);
            let next = hew_timer_wheel_next_deadline_ms(wheel);
            hew_timer_wheel_free(wheel);
            (next, fired)
        };
        assert_eq!(fired, 0);
        assert_eq!(next, 490);
    }

    // Independent absolute-nanosecond oracle, exercising bucket rollover,
    // cancellation, cascades and overflow with arbitrary fractional deadlines.
    #[cfg(not(target_arch = "wasm32"))]
    mod failure_properties {
        use super::*;
        use proptest::prelude::*;

        unsafe extern "C" fn count(data: *mut c_void) {
            // SAFETY: each callback borrows one fixture-owned counter.
            unsafe { &*data.cast::<AtomicI32>() }.fetch_add(1, Ordering::Relaxed);
        }

        struct TestWheel(*mut HewTimerWheel);
        impl Drop for TestWheel {
            fn drop(&mut self) {
                // SAFETY: this owner is unique and outlives callback counters.
                unsafe { hew_timer_wheel_free(self.0) };
            }
        }

        proptest! {
            #![proptest_config(ProptestConfig { cases: 1024, ..ProptestConfig::default() })]
            #[test]
            fn exact_wheel_matches_absolute_deadline_oracle(
                entries in prop::collection::vec((prop_oneof![0u64..2_000_000, 0u64..50_000_000_000, 255_999_900u64..256_000_100, 16_383_999_900u64..16_384_000_100], any::<bool>()), 1..64),
                mut ticks in prop::collection::vec(prop_oneof![0u64..2_000_000, 0u64..50_000_000_000, 255_999_900u64..256_000_100, 16_383_999_900u64..16_384_000_100], 1..32),
            ) {
                let counters: Vec<_> = entries.iter().map(|_| AtomicI32::new(0)).collect();
                ticks.push(50_000_000_000);
                ticks.sort_unstable();
                // SAFETY: callbacks only borrow fixture-owned counters. The
                // wheel is destroyed before any counter goes out of scope.
                unsafe {
                    let owner = TestWheel(make_timer_wheel(0));
                    let wheel = owner.0;
                    for (index, &(ns, cancel)) in entries.iter().enumerate() {
                        let handle = timer_wheel_schedule_precise_handle(wheel, Deadline::from_nanos(ns), count, std::ptr::from_ref(&counters[index]).cast_mut().cast());
                        assert!(!handle.entry.is_null());
                        if cancel {
                            let removed = hew_timer_wheel_remove(wheel, handle.entry, handle.generation);
                            assert_eq!(removed, std::ptr::from_ref(&counters[index]).cast_mut().cast());
                        }
                    }
                    for now in ticks {
                        timer_wheel_tick_at(wheel, Deadline::from_nanos(now));
                        for (index, &(deadline, cancel)) in entries.iter().enumerate() {
                            assert_eq!(counters[index].load(Ordering::Relaxed), i32::from(!cancel && deadline <= now), "deadline={deadline} now={now} cancelled={cancel}");
                        }
                        let expected = entries.iter().filter(|(deadline, cancel)| !cancel && *deadline > now).map(|(deadline, _)| *deadline).min();
                        assert_eq!(timer_wheel_next_deadline(wheel).map(Deadline::saturated_nanos), expected);
                    }

                }
            }
        }
    }
}
