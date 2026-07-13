//! Internal cancellation/deadline record for suspended awaits.
//!
//! The record is source-agnostic: it owns the one-shot state transition, the
//! retained scope cancellation token snapshot, the optional deadline timer, and
//! the actor wake.  Each wait source supplies exactly one cleanup callback for
//! its own in-flight resource.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at each fn signature."
)]

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64, AtomicUsize, Ordering};

use crate::actor::HewActor;
use crate::task_scope::{hew_cancel_token_release, hew_cancel_token_retain, HewCancellationToken};
use crate::timer_wheel::{hew_timer_wheel_cancel, hew_timer_wheel_schedule_handle};
use crate::timer_wheel::{HewTimerEntry, HewTimerWheel};

/// Test-only per-instance final-free probe. Replaces the former process-global
/// `AWAIT_CANCEL_FINAL_FREE_COUNT` counter, whose delta assertions were
/// corrupted under parallel test execution by unrelated registrations freed in
/// other modules' tests (same contamination class as the runtime-drop counter
/// de-globalized in #2574). Each "reclaimed exactly once" oracle binds a fresh
/// counter to the single registration it owns via
/// [`install_await_cancel_free_probe_for_test`], so it counts only *that
/// registration's* final free.
#[cfg(test)]
pub(crate) type AwaitCancelFreeProbe = std::sync::Arc<AtomicUsize>;

#[cfg(test)]
pub(crate) fn new_await_cancel_free_probe_for_test() -> AwaitCancelFreeProbe {
    std::sync::Arc::new(AtomicUsize::new(0))
}

/// Bind a per-instance final-free probe to a single live registration the test
/// owns. The registration's final [`hew_await_cancel_free`] bumps this probe, so
/// the count is immune to concurrent frees of other registrations in parallel
/// tests.
///
/// # Safety
///
/// `reg` must be a valid live `HewAwaitCancel` the caller holds a ref to.
#[cfg(test)]
pub(crate) unsafe fn install_await_cancel_free_probe_for_test(
    reg: *mut HewAwaitCancel,
    probe: &AwaitCancelFreeProbe,
) {
    // SAFETY: caller holds a ref, so the box is live; `final_free_probe` uses
    // interior mutability (a `Mutex`) so a shared reference may set it.
    let inner = unsafe { &*reg };
    *inner
        .final_free_probe
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(std::sync::Arc::clone(probe));
}

#[cfg(test)]
pub(crate) fn await_cancel_free_probe_count(probe: &AwaitCancelFreeProbe) -> usize {
    probe.load(Ordering::Acquire)
}

/// Terminal state for an internal suspended await registration.
///
/// Values are ABI-visible to codegen/runtime adapters.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AwaitCancelStatus {
    Pending = 0,
    Completed = 1,
    Cancelled = 2,
    TimedOut = 3,
}

impl AwaitCancelStatus {
    fn terminal_for(raw: i32) -> Self {
        match raw {
            1 => Self::Completed,
            2 => Self::Cancelled,
            3 => Self::TimedOut,
            _ => Self::Pending,
        }
    }
}

pub type HewAwaitCleanup = Option<unsafe extern "C" fn(*mut c_void, i32)>;

/// Shared cancellation/deadline registration for a suspended wait source.
#[repr(C)]
#[derive(Debug)]
pub struct HewAwaitCancel {
    refs: AtomicUsize,
    state: AtomicI32,
    cancel_token: *mut HewCancellationToken,
    timer_wheel: AtomicPtr<HewTimerWheel>,
    timer_entry: AtomicPtr<HewTimerEntry>,
    timer_generation: AtomicU64,
    timer_ref_held: AtomicBool,
    actor: AtomicPtr<HewActor>,
    cleanup: HewAwaitCleanup,
    source: *mut c_void,
    /// Test-only per-instance final-free probe. When a test binds one via
    /// [`install_await_cancel_free_probe_for_test`], the registration's final
    /// [`hew_await_cancel_free`] bumps it, so an "exactly once" oracle observes
    /// only this registration's free rather than a process-global count
    /// contaminated by concurrent parallel-test frees.
    #[cfg(test)]
    final_free_probe: std::sync::Mutex<Option<AwaitCancelFreeProbe>>,
}

// SAFETY: shared mutable state is atomic; raw pointers are retained or source-
// owned for the lifetime promised by the FFI attachment contracts.
unsafe impl Send for HewAwaitCancel {}
// SAFETY: concurrent access is synchronized by atomics and the manual refcount.
unsafe impl Sync for HewAwaitCancel {}

unsafe fn await_cancel_retain(reg: *mut HewAwaitCancel) {
    if reg.is_null() {
        return;
    }
    // SAFETY: caller holds or is installing a live reference.
    unsafe { (*reg).refs.fetch_add(1, Ordering::Relaxed) };
}

unsafe fn release_timer_ref_if_held(reg: *mut HewAwaitCancel) {
    if reg.is_null() {
        return;
    }
    // SAFETY: caller holds a live reference while testing the timer-ref latch.
    if unsafe { (*reg).timer_ref_held.swap(false, Ordering::AcqRel) } {
        // SAFETY: consumes the timer-owned reference.
        unsafe { hew_await_cancel_free(reg) };
    }
}

unsafe fn cancel_timer(reg: *mut HewAwaitCancel) {
    if reg.is_null() {
        return;
    }
    // SAFETY: caller holds a live reference.
    let r = unsafe { &*reg };
    let entry = r.timer_entry.swap(ptr::null_mut(), Ordering::AcqRel);
    let generation = r.timer_generation.swap(0, Ordering::AcqRel);
    let wheel = r.timer_wheel.swap(ptr::null_mut(), Ordering::AcqRel);
    if !entry.is_null() && !wheel.is_null() && generation != 0 {
        // SAFETY: the entry was scheduled on this wheel and has not been
        // detached by this registration yet.
        unsafe { hew_timer_wheel_cancel(wheel, entry, generation) };
    }
    // If the timer was cancelled before it fired, release the ref retained for
    // the callback.  If it already fired, the callback's own release wins this
    // latch and this is a no-op.
    if !entry.is_null() {
        // SAFETY: caller holds a live reference while cancelling the timer.
        unsafe { release_timer_ref_if_held(reg) };
    }
}

unsafe extern "C" fn await_cancel_timer_cb(data: *mut c_void) {
    let reg = data.cast::<HewAwaitCancel>();
    if reg.is_null() {
        return;
    }
    // This callback is running because the timer entry fired and is about to be
    // freed by the timer wheel, so this registration no longer owns an entry
    // pointer that can be cancelled.
    // SAFETY: the timer retained `reg` before scheduling this callback.
    unsafe {
        (*reg).timer_entry.store(ptr::null_mut(), Ordering::Release);
        (*reg).timer_generation.store(0, Ordering::Release);
        (*reg).timer_wheel.store(ptr::null_mut(), Ordering::Release);
        let _ = await_cancel_finish(reg, AwaitCancelStatus::TimedOut, true);
        release_timer_ref_if_held(reg);
    }
}

unsafe fn await_cancel_finish(
    reg: *mut HewAwaitCancel,
    status: AwaitCancelStatus,
    wake_actor: bool,
) -> bool {
    if reg.is_null() {
        return false;
    }
    debug_assert!(
        matches!(
            status,
            AwaitCancelStatus::Completed
                | AwaitCancelStatus::Cancelled
                | AwaitCancelStatus::TimedOut
        ),
        "await_cancel_finish requires a terminal status"
    );

    // SAFETY: caller holds a live registration reference.
    let r = unsafe { &*reg };
    if r.state
        .compare_exchange(
            AwaitCancelStatus::Pending as i32,
            status as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_err()
    {
        return false;
    }

    // SAFETY: caller holds a live registration reference.
    unsafe { cancel_timer(reg) };

    if matches!(
        status,
        AwaitCancelStatus::Cancelled | AwaitCancelStatus::TimedOut
    ) {
        if let Some(cleanup) = r.cleanup {
            // SAFETY: the wait source installed this callback together with its
            // source pointer and the common CAS above guarantees it runs once.
            unsafe { cleanup(r.source, status as i32) };
        }
        if wake_actor {
            let actor = r.actor.load(Ordering::Acquire);
            if !actor.is_null() {
                // SAFETY: enqueue_resume revalidates liveness before deref.
                unsafe { crate::scheduler::enqueue_resume(actor, ptr::null_mut()) };
            }
        }
    }

    true
}

/// Allocate a suspended-await cancellation registration.
///
/// The current execution context's cancel token is retained as a snapshot when
/// present. `actor` may be null; null means cancellation can still clean up the
/// source but cannot enqueue a resume.
///
/// # Safety
///
/// `cleanup`, when present, must be valid for the lifetime of `source`. `source`
/// must remain valid until the registration completes/cancels or the source
/// detaches the registration.
#[no_mangle]
pub unsafe extern "C" fn hew_await_cancel_new(
    actor: *mut HewActor,
    cleanup: HewAwaitCleanup,
    source: *mut c_void,
) -> *mut HewAwaitCancel {
    let token = {
        let ctx = crate::execution_context::current_context();
        if ctx.is_null() {
            ptr::null_mut()
        } else {
            // SAFETY: scheduler-installed context is live for this call.
            let token = unsafe { (*ctx).cancel_token };
            // SAFETY: retain is null-safe and token is live while in context.
            unsafe { hew_cancel_token_retain(token) };
            token
        }
    };

    Box::into_raw(Box::new(HewAwaitCancel {
        refs: AtomicUsize::new(1),
        state: AtomicI32::new(AwaitCancelStatus::Pending as i32),
        cancel_token: token,
        timer_wheel: AtomicPtr::new(ptr::null_mut()),
        timer_entry: AtomicPtr::new(ptr::null_mut()),
        timer_generation: AtomicU64::new(0),
        timer_ref_held: AtomicBool::new(false),
        actor: AtomicPtr::new(actor),
        cleanup,
        source,
        #[cfg(test)]
        final_free_probe: std::sync::Mutex::new(None),
    }))
}

/// Retain a registration reference.
///
/// # Safety
///
/// `reg` must be null or a live registration pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_await_cancel_retain(reg: *mut HewAwaitCancel) {
    // SAFETY: upheld by this function's caller.
    unsafe { await_cancel_retain(reg) };
}

/// Release a registration reference.
///
/// # Safety
///
/// `reg` must be null or a live registration pointer. The pointer must not be
/// used after releasing the caller's last reference.
#[no_mangle]
pub unsafe extern "C" fn hew_await_cancel_free(reg: *mut HewAwaitCancel) {
    if reg.is_null() {
        return;
    }
    // SAFETY: caller holds one live reference.
    let prev = unsafe { (*reg).refs.fetch_sub(1, Ordering::AcqRel) };
    debug_assert!(prev > 0, "await cancel release on released registration");
    if prev != 1 {
        return;
    }
    #[cfg(test)]
    // SAFETY: refs reached 0, so we own the box exclusively; read the optional
    // per-instance probe before the box is consumed below and bump it.
    unsafe {
        if let Ok(mut guard) = (*reg).final_free_probe.lock() {
            if let Some(probe) = guard.take() {
                probe.fetch_add(1, Ordering::AcqRel);
            }
        }
    }
    // SAFETY: last ref; no timer ref can still be held or refs would exceed 1.
    let boxed = unsafe { Box::from_raw(reg) };
    // SAFETY: this registration retained the token at creation.
    unsafe { hew_cancel_token_release(boxed.cancel_token) };
}

/// Return the current terminal state.
///
/// # Safety
///
/// `reg` must be null or a live registration pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_await_cancel_status(reg: *mut HewAwaitCancel) -> i32 {
    if reg.is_null() {
        return AwaitCancelStatus::Cancelled as i32;
    }
    // SAFETY: caller holds a live reference.
    unsafe { (*reg).state.load(Ordering::Acquire) }
}

/// Mark the wait completed. Returns 1 if this call won the one-shot transition.
///
/// # Safety
///
/// `reg` must be null or a live registration pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_await_cancel_complete(reg: *mut HewAwaitCancel) -> i32 {
    // SAFETY: upheld by this function's caller.
    let finished = unsafe { await_cancel_finish(reg, AwaitCancelStatus::Completed, false) };
    i32::from(finished)
}

/// Cancel or time out the wait. Returns 1 if this call ran source cleanup.
///
/// `status` values other than `TimedOut` are treated as explicit cancellation.
///
/// # Safety
///
/// `reg` must be null or a live registration pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_await_cancel_cancel(
    reg: *mut HewAwaitCancel,
    status: i32,
    wake_actor: i32,
) -> i32 {
    let terminal = match AwaitCancelStatus::terminal_for(status) {
        AwaitCancelStatus::TimedOut => AwaitCancelStatus::TimedOut,
        _ => AwaitCancelStatus::Cancelled,
    };
    // SAFETY: upheld by this function's caller.
    let finished = unsafe { await_cancel_finish(reg, terminal, wake_actor != 0) };
    i32::from(finished)
}

/// Schedule this registration to time out on `tw` after `delay_ms`.
///
/// Returns 0 on success and -1 on invalid input or if the registration is no
/// longer pending.
///
/// # Safety
///
/// `reg` and `tw` must be live. `tw` must outlive either timer firing or
/// cancellation through this registration.
#[no_mangle]
pub unsafe extern "C" fn hew_await_cancel_schedule_deadline_ms(
    reg: *mut HewAwaitCancel,
    tw: *mut HewTimerWheel,
    delay_ms: u64,
) -> i32 {
    if reg.is_null() || tw.is_null() {
        return -1;
    }
    // SAFETY: caller holds a live reference.
    let r = unsafe { &*reg };
    if r.state.load(Ordering::Acquire) != AwaitCancelStatus::Pending as i32 {
        return -1;
    }
    if r.timer_ref_held.swap(true, Ordering::AcqRel) {
        return -1;
    }
    // SAFETY: the timer callback owns this retained reference until it fires or
    // cancellation releases it.
    unsafe { await_cancel_retain(reg) };
    // SAFETY: caller guarantees `tw` is live for this timer.
    let handle =
        unsafe { hew_timer_wheel_schedule_handle(tw, delay_ms, await_cancel_timer_cb, reg.cast()) };
    if handle.entry.is_null() {
        r.timer_ref_held.store(false, Ordering::Release);
        // SAFETY: undo the callback ref retained above.
        unsafe { hew_await_cancel_free(reg) };
        return -1;
    }
    r.timer_wheel.store(tw, Ordering::Release);
    r.timer_generation
        .store(handle.generation, Ordering::Release);
    r.timer_entry.store(handle.entry, Ordering::Release);
    0
}

/// Observe the retained scope token and cancel if cancellation was requested.
///
/// Returns 1 when this call transitioned the wait to `Cancelled`.
///
/// # Safety
///
/// `reg` must be null or a live registration pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_await_cancel_observe_token(reg: *mut HewAwaitCancel) -> i32 {
    if reg.is_null() {
        return 0;
    }
    // SAFETY: caller holds a live reference.
    let token = unsafe { (*reg).cancel_token };
    if token.is_null() {
        return 0;
    }
    // SAFETY: the registration retained the token.
    if unsafe { crate::task_scope::hew_cancel_token_is_requested(token) } == 0 {
        return 0;
    }
    // SAFETY: caller holds a live registration reference.
    i32::from(unsafe { await_cancel_finish(reg, AwaitCancelStatus::Cancelled, true) })
}

#[cfg(test)]
mod tests {
    use super::*;

    static CLEANUPS: AtomicUsize = AtomicUsize::new(0);

    unsafe extern "C" fn count_cleanup(_source: *mut c_void, _status: i32) {
        CLEANUPS.fetch_add(1, Ordering::AcqRel);
    }

    #[test]
    fn cancel_cleanup_runs_exactly_once() {
        CLEANUPS.store(0, Ordering::Release);
        // SAFETY: test callback/source are valid for the registration lifetime.
        let reg =
            unsafe { hew_await_cancel_new(ptr::null_mut(), Some(count_cleanup), ptr::null_mut()) };
        assert_eq!(
            // SAFETY: reg is live and owned by this test.
            unsafe { hew_await_cancel_cancel(reg, AwaitCancelStatus::Cancelled as i32, 0) },
            1
        );
        assert_eq!(
            // SAFETY: reg is still live; the previous cancel retained terminal state.
            unsafe { hew_await_cancel_cancel(reg, AwaitCancelStatus::TimedOut as i32, 0) },
            0
        );
        assert_eq!(CLEANUPS.load(Ordering::Acquire), 1);
        // SAFETY: release the test's final registration reference.
        unsafe { hew_await_cancel_free(reg) };
    }

    /// Suspending-sleep abandon edge drop-safety. Mirrors the codegen abandon
    /// arm of `emit_suspending_sleep_terminator`: a parked cooperative
    /// `sleep_ms` whose continuation is dropped before the timer fires must
    /// cancel the armed deadline (so the wake never lands on a torn-down
    /// continuation) and reclaim BOTH the `HewAwaitCancel` registration and the
    /// timer-wheel entry exactly once.
    ///
    /// The lifecycle this pins (identical to the emitted sleep ramp):
    /// 1. `hew_await_cancel_new` allocates the registration (refs = 1).
    /// 2. `hew_await_cancel_schedule_deadline_ms` arms a wheel deadline,
    ///    retaining a second ref for the timer callback (refs = 2) and
    ///    allocating the timer entry node.
    /// 3. Abandon: `hew_await_cancel_cancel(Cancelled, no_wake)` wins the
    ///    one-shot transition, cancels the armed timer (marking the entry so it
    ///    cannot fire), and releases the timer's retained ref (refs = 1).
    /// 4. `hew_await_cancel_free` drops the last ref (refs = 0 → box freed).
    /// 5. Ticking the wheel past the deadline collects the cancelled entry and
    ///    frees its node WITHOUT firing the callback — so the callback never
    ///    dereferences the already-freed registration.
    ///
    /// Under `ASan` this proves single-free of the registration and the timer
    /// node and the absence of a use-after-free wake. The `no_wake` cancel matches the
    /// codegen abandon arm, which passes wake = 0 because the continuation is
    /// being destroyed.
    #[test]
    fn suspending_sleep_abandon_reclaims_registration_and_timer_entry_once() {
        // SAFETY: the test owns the wheel and registration for the whole
        // lifecycle and drives each transition in order; all pointers are valid
        // at each call.
        unsafe {
            let tw = crate::timer_wheel::hew_timer_wheel_new();

            // (1) + (2): the sleep ramp's `hew_await_cancel_new` +
            // `schedule_deadline_ms`. A short delay so a post-cancel tick lands
            // past the deadline. No actor / cleanup / source — a sleep owns no
            // read slot or channel registration to tear down (the wheel cancel
            // is the whole teardown).
            let reg = hew_await_cancel_new(ptr::null_mut(), None, ptr::null_mut());
            let rc = hew_await_cancel_schedule_deadline_ms(reg, tw, 2);
            assert_eq!(rc, 0, "arming the sleep deadline must succeed");

            // (3): the codegen abandon arm — Cancelled status, no wake.
            let won = hew_await_cancel_cancel(reg, AwaitCancelStatus::Cancelled as i32, 0);
            assert_eq!(won, 1, "abandon must win the one-shot terminal transition");

            // (4): release the codegen-held registration ref.
            hew_await_cancel_free(reg);

            // (5): advance the wheel past the deadline. The cancelled entry must
            // be reclaimed without firing — a fired callback here would be a
            // use-after-free against the freed registration.
            std::thread::sleep(std::time::Duration::from_millis(5));
            let fired = crate::timer_wheel::hew_timer_wheel_tick(tw);
            assert_eq!(
                fired, 0,
                "a cancelled sleep deadline must not fire — a non-zero count would \
                 mean the callback ran against the freed registration"
            );

            crate::timer_wheel::hew_timer_wheel_free(tw);
        }
    }
}
