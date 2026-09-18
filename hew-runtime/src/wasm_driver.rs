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

use std::ptr;
use std::sync::atomic::{AtomicPtr, Ordering};

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

/// Advance the process one readiness step while the root is parked.
///
/// Ticks the wheel to the WASI clock; when nothing is due yet, sleeps until the
/// earliest deadline so the module yields instead of spinning. With no pending
/// readiness source at all the parked root can never resume, which is a
/// compiler or runtime defect rather than a program outcome, so the module
/// fails closed instead of hanging.
pub(crate) fn step() {
    let wheel = global_wheel();
    // SAFETY: `global_wheel` returns a live process-owned wheel or null.
    let fired = unsafe { timer_wheel_tick_to(wheel, crate::clock::hew_now_ms()) };
    if fired > 0 {
        return;
    }
    // SAFETY: same live wheel; the query takes the wheel's own lock.
    let remaining = unsafe { hew_timer_wheel_next_deadline_ms(wheel) };
    if remaining < 0 {
        eprintln!("hew: fail-closed: the wasm32 process root is parked with no pending timer");
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

/// Finish the process runtime and resolve the process exit status.
///
/// Nothing is left to drain: the wasm32 driver runs every unit of work inline
/// on the process thread, so by the time the entry adapter returns the run is
/// over. The status still goes through the one exit-status authority.
///
/// # Safety
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_native_runtime_finish(status: i32) -> i32 {
    crate::exit_status::hew_process_exit_byte(status)
}
