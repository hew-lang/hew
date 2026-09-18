//! The single-threaded wasm32 process driver.
//!
//! `wasm32-wasip1` has no worker thread, so the readiness latch the native root
//! waits on ([`crate::wake::blocking::Readiness`]) can never be satisfied from
//! elsewhere. The process itself is the driver: while the root coroutine is
//! parked, [`step`] advances the timer wheel against the WASI clock and hands
//! control back as soon as anything becomes ready.
//!
//! This module owns the wasm32 half of every process-runtime symbol codegen
//! emits — the timer wheel, `hew_sched_init`, `hew_native_runtime_finish` and
//! `hew_exit` — so there is exactly one process driver on this target.

use std::cell::RefCell;
use std::collections::VecDeque;
use std::ptr;
use std::sync::atomic::{AtomicPtr, Ordering};

use crate::activation::{activate_queued_actor, SchedulerQueueEntry};
use crate::timer_wheel::{
    hew_timer_wheel_new, hew_timer_wheel_next_deadline_ms, timer_wheel_tick_to, HewTimerWheel,
};

static WHEEL: AtomicPtr<HewTimerWheel> = AtomicPtr::new(ptr::null_mut());

/// The process-wide timer wheel, created on first use.
///
/// Nothing ticks it in the background: [`step`] is the only cursor advance, so
/// a timer fires exactly when the parked root next looks.
pub(crate) fn global_wheel() -> *mut HewTimerWheel {
    let existing = WHEEL.load(Ordering::Acquire);
    if !existing.is_null() {
        return existing;
    }
    // SAFETY: allocation only; the wheel is never freed for the process life.
    let created = unsafe { hew_timer_wheel_new() };
    match WHEEL.compare_exchange(
        ptr::null_mut(),
        created,
        Ordering::AcqRel,
        Ordering::Acquire,
    ) {
        Ok(_) => created,
        Err(winner) => winner,
    }
}

thread_local! {
    /// Actors this target has made runnable, in the order they became so.
    ///
    /// The native scheduler hands an entry to a work-stealing deque and wakes a
    /// worker; wasm32 has no worker, so the entry waits here until the process
    /// next looks. One thread, so a `RefCell` is the whole synchronisation
    /// story.
    static RUN_QUEUE: RefCell<VecDeque<SchedulerQueueEntry>> = RefCell::new(VecDeque::new());
}

/// Accept a runnable actor from [`crate::resume`].
pub(crate) fn publish_queue_entry(mut entry: SchedulerQueueEntry) {
    entry.disarm();
    let actor = entry.actor;
    RUN_QUEUE.with(|queue| queue.borrow_mut().push_back(entry));
    let _ = actor;
}

/// Run one queued activation, if any. Returns whether one ran.
///
/// The queue borrow is released before the activation, because an activation
/// that yields or exhausts its budget re-publishes the same actor.
fn run_one_queued_activation() -> bool {
    let Some(entry) = RUN_QUEUE.with(|queue| queue.borrow_mut().pop_front()) else {
        return false;
    };
    let actor = entry.actor;
    std::mem::forget(entry);
    activate_queued_actor(actor);
    true
}

/// Whether any actor is waiting to run.
pub(crate) fn has_queued_work() -> bool {
    RUN_QUEUE.with(|queue| !queue.borrow().is_empty())
}

/// Advance the process one readiness step while the root is parked.
///
/// Ticks the wheel to the WASI clock; when nothing is due yet, sleeps until the
/// earliest deadline so the module yields instead of spinning. With no pending
/// readiness source at all the parked root can never resume, which is a
/// compiler or runtime defect rather than a program outcome, so the module
/// fails closed instead of hanging.
pub(crate) fn step() {
    if run_one_queued_activation() {
        return;
    }
    let wheel = global_wheel();
    // SAFETY: `global_wheel` returns a live process-owned wheel or null.
    let fired = unsafe { timer_wheel_tick_to(wheel, crate::clock::hew_now_ms()) };
    if fired > 0 {
        return;
    }
    // SAFETY: same live wheel; the query takes the wheel's own lock.
    let remaining = unsafe { hew_timer_wheel_next_deadline_ms(wheel) };
    if remaining < 0 && !has_queued_work() {
        eprintln!(
            "hew: fail-closed: the wasm32 process is parked with no runnable actor and no \
             pending timer"
        );
        std::process::abort();
    }
    // A zero remainder means the deadline is due and the next tick fires it.
    if remaining > 0 {
        std::thread::sleep(std::time::Duration::from_millis(remaining.cast_unsigned()));
    }
}

/// Start the process runtime. The wasm32 driver has no scheduler to start, so
/// this only materializes the wheel the parked root will tick.
///
/// # Safety
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_sched_init() -> i32 {
    let _ = global_wheel();
    0
}

/// Run the process to quiescence: every runnable actor, then every timer that
/// is already due, until neither has anything left.
///
/// A periodic handler re-arms its timer, so the wheel is never permanently
/// empty while one is live; quiescence is therefore "no runnable actor and no
/// timer due now", which is what the native shutdown phase settles on once its
/// workers park.
pub(crate) fn drain_to_quiescence() -> i32 {
    let wheel = global_wheel();
    loop {
        if run_one_queued_activation() {
            continue;
        }
        // SAFETY: `global_wheel` returns the live process-owned wheel.
        let fired = unsafe { timer_wheel_tick_to(wheel, crate::clock::hew_now_ms()) };
        if fired == 0 && !has_queued_work() {
            return 0;
        }
    }
}
