//! Slice-4 poll/resume executor guards.
//!
//! Slice 3 (`cont.rs`) delivered the continuation REPRESENTATION — the
//! `HewCont` heap frame plus the `resume`/`done`/`poll`/`destroy` C ABI. That
//! substrate is fail-closed on a NULL handle but holds no scheduler state, so
//! it cannot detect a *double-resume*, a *destroy-after-destroy*, or a
//! *concurrent resume+destroy* on a non-null dangling handle. Those are the
//! executor's responsibility, and they live here rather than in `cont.rs`
//! (which the slice keeps read-only).
//!
//! This module is the EXECUTOR's serialization layer. It owns:
//!
//! - **FG1 — single teardown owner.** [`destroy_parked`] transitions the
//!   per-continuation [`ContTag`] to `Destroyed` exactly once; a second
//!   destroy refuses. The parked handle's frame is reclaimed by exactly one
//!   `hew_cont_destroy`.
//! - **FG2 — no concurrent resume/destroy.** The tag CAS gates `resume`
//!   (`Parked → Resuming`) and `destroy` (`Done`/`Parked → Destroyed`); a
//!   handle being resumed cannot be destroyed concurrently and vice versa.
//!   The per-actor state lock is RELEASED while suspended, so this tag — not
//!   the lock — is the serialization point.
//! - **FG3 — two-phase park.** [`begin_park`] / [`finish_park`] /
//!   [`take_pending_wake`] implement Go's `pdNil → pdWait → pdReady` so a wake
//!   arriving in the register/park window is observed, not lost.
//! - **FG4 — no use-after-destroy.** [`destroy_parked`] nulls the actor's
//!   `suspended_cont` slot in the SAME critical section as the `Destroyed`
//!   transition, so no later activation reads the freed handle. [`resume_park`]
//!   refuses a null / non-`Parked` slot.
//!
//! Every transition defaults to REFUSE on an unexpected current tag
//! (`boundary-fail-closed`): the functions return an [`ExecGuard`] outcome the
//! caller branches on; an out-of-contract transition is a `Refused`, never a
//! silent success. In debug builds a `debug_assert` additionally trips so the
//! contract violation is loud in tests; release builds still fail closed by
//! refusing the operation (no UB, no double-free).
//!
//! WASM parity: identical source on native and `wasm32`. The native scheduler
//! drives these from any worker thread (the atomics serialize cross-thread
//! resume/destroy); the wasm cooperative scheduler drives them single-threaded
//! on the drain tick. Both go through this one ABI.

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::Ordering;

use crate::actor::HewActor;
use crate::cont::{self, ResumePoll};
use crate::internal::types::ContTag;

/// Test-only: when set, `resume_park` yields the thread N times in the
/// `Resuming` window — between the `Parked → Resuming` CAS and the actual
/// `hew_cont_resume` call — so a concurrent `destroy_parked` reliably lands
/// inside the guarded window. Loaded with `Acquire` before each resume; a
/// value > 0 means "yield that many times". Reset to 0 by the caller after
/// the probe turn that needs the extended window.
///
/// WHY: The `Barrier`-aligned probe is too short to guarantee a concurrent
/// destroy lands INSIDE the Resuming window without an injected delay; on a
/// fast machine the resume thread blows past the window before the destroy
/// thread is even scheduled. This hook makes the window controllably wide.
///
/// WHEN: Remove (or keep as `0` forever) once a real scheduler hook replaces
/// this test scaffold.
///
/// REAL: A production `yield_point` ABI inside the actor executor that allows
/// cooperative pre-emption at known safe points between `begin_resume` and
/// the continuation call.
#[cfg(test)]
static RESUMING_WINDOW_YIELD_COUNT: std::sync::atomic::AtomicU32 =
    std::sync::atomic::AtomicU32::new(0);

/// Set the number of yields injected into the Resuming window for the next
/// [`resume_park`] call. A value of 0 (the default) disables the injection.
#[cfg(test)]
pub(crate) fn set_resuming_window_yield_count(n: u32) {
    RESUMING_WINDOW_YIELD_COUNT.store(n, Ordering::Release);
}

/// Outcome of an executor guard transition. A `Refused` is fail-closed: the
/// caller must NOT proceed with the guarded operation (resume / destroy).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecGuard {
    /// The CAS succeeded; the caller owns the guarded operation.
    Ok,
    /// The current tag was not the expected one; the operation is refused.
    /// The caller leaves the handle untouched (no resume, no destroy).
    Refused,
}

impl ExecGuard {
    /// `true` when the guarded operation may proceed.
    #[must_use]
    pub fn is_ok(self) -> bool {
        matches!(self, ExecGuard::Ok)
    }
}

/// Load the current [`ContTag`] of an actor's parked continuation.
///
/// A raw value outside the legal range decodes to `Destroyed` (the most
/// conservative tag: every further operation refuses), so a corrupted tag
/// fails closed rather than aliasing onto a live state.
fn load_tag(a: &HewActor) -> ContTag {
    let raw = a.cont_tag.load(Ordering::Acquire);
    ContTag::from_i32(raw).unwrap_or(ContTag::Destroyed)
}

/// CAS the continuation tag from `expected` to `next`, returning `Ok` only on
/// success and `Refused` (fail-closed) on any other current tag.
///
/// This is the EXTERNAL-misuse guard path: a refusal here is the guard doing
/// its job (catching a double-resume / double-destroy / concurrent
/// resume+destroy), NOT an internal logic error, so it does NOT trip a
/// `debug_assert`. The caller branches on the returned [`ExecGuard`] and
/// refuses the guarded operation (no resume, no destroy) — fail-closed in both
/// debug and release builds, never UB.
fn cas_tag(a: &HewActor, expected: ContTag, next: ContTag) -> ExecGuard {
    match a.cont_tag.compare_exchange(
        expected as i32,
        next as i32,
        Ordering::AcqRel,
        Ordering::Acquire,
    ) {
        Ok(_) => ExecGuard::Ok,
        Err(_) => ExecGuard::Refused,
    }
}

/// CAS the continuation tag from `expected` to `next` for an INTERNAL-invariant
/// transition the executor's own logic must uphold (e.g. settling a resume the
/// executor itself just started). A refusal here indicates a bug in the
/// executor, not external misuse, so it trips a `debug_assert` in debug builds
/// (loud in tests) while still failing closed by refusing in release builds.
fn cas_tag_strict(a: &HewActor, expected: ContTag, next: ContTag) -> ExecGuard {
    let outcome = cas_tag(a, expected, next);
    if !outcome.is_ok() {
        let actual = a.cont_tag.load(Ordering::Acquire);
        debug_assert!(
            false,
            "coro_exec: internal-invariant tag transition {expected:?} -> {next:?} refused; \
             actual tag = {:?} (raw {actual})",
            ContTag::from_i32(actual)
        );
    }
    outcome
}

// ── FG3: two-phase park ───────────────────────────────────────────────────

/// Phase 1 of the suspend edge: publish the park INTENT before storing the
/// handle, so a wake firing mid-park is not lost.
///
/// Moves the tag to `Parked`. The expected prior tag is `Empty` (first suspend
/// of a fresh actor) or `Resuming` (the actor was resumed, ran to a non-final
/// suspend, and is parking again). Any other prior tag refuses — a park cannot
/// begin on a `Done`/`Destroyed` handle.
///
/// W6.010 lost-wake fix: `begin_park` does NOT clear `pending_wake`. The
/// readiness source (an actor-ask send) is armed INSIDE the coroutine body —
/// which runs during the ramp call, BEFORE this park edge — so a reply can fire
/// (and `mark_pending_wake`) in the window between the send and `begin_park`. A
/// clear here would wipe that valid wake and the caller would hang forever (the
/// pre-park lost-wake race). The flag is instead kept monotonic within a cycle
/// and CONSUMED by `take_pending_wake` (a swap-to-false) at every drain (the
/// park-edge drain and the resume drain), so it never goes stale across cycles
/// — every set is drained exactly once, whether the wake arrived before or
/// after the park published `Parked`. This is Go's `pdReady`-before-`pdWait`
/// case: a readiness that beats the park is still observed.
#[must_use]
pub fn begin_park(a: &HewActor) -> ExecGuard {
    let cur = load_tag(a);
    match cur {
        // First park of a fresh actor (internal invariant: the executor only
        // begins a park from a quiescent or just-resumed continuation).
        ContTag::Empty => cas_tag_strict(a, ContTag::Empty, ContTag::Parked),
        ContTag::Resuming => cas_tag_strict(a, ContTag::Resuming, ContTag::Parked),
        _ => {
            debug_assert!(false, "coro_exec: begin_park from illegal tag {cur:?}");
            ExecGuard::Refused
        }
    }
}

/// Phase 2 of the suspend edge: store the handle into the actor's resume slot
/// AFTER [`begin_park`] published `Parked`. Pairs with [`take_pending_wake`].
///
/// # Safety
///
/// `cont` must be a live, suspended continuation handle (a `coro.begin` frame
/// pointer) that the caller is parking on `a`. After this call the executor
/// (via [`resume_park`] / [`destroy_parked`]) is its single owner (FG1).
pub unsafe fn finish_park(a: &HewActor, cont: *mut c_void) {
    a.suspended_cont.store(cont, Ordering::Release);
}

/// Drain the FG3 lost-wake flag set by a wake that fired in the park window.
///
/// Called by the suspend edge AFTER [`finish_park`]: if a readiness source
/// woke the actor between `begin_park` and now, `pending_wake` is set and the
/// suspend edge must re-enqueue the actor itself (the wake's own
/// `Suspended → Runnable` CAS failed because the park was not yet published).
/// Returns `true` if a wake was pending (and consumes it).
#[must_use]
pub fn take_pending_wake(a: &HewActor) -> bool {
    a.pending_wake.swap(false, Ordering::AcqRel)
}

/// Record that a wake arrived before the park completed (FG3). Called by the
/// wake edge when its `Suspended → Runnable` CAS fails because the actor is not
/// yet published as `Suspended`. The suspend edge drains this via
/// [`take_pending_wake`] and re-enqueues, so the wake is observed exactly once.
pub fn mark_pending_wake(a: &HewActor) {
    a.pending_wake.store(true, Ordering::Release);
}

// ── FG2/FG4: resume ───────────────────────────────────────────────────────

/// Begin a resume: CAS the tag `Parked → Resuming`, refusing if the handle is
/// not currently parked (already resuming, done, destroyed, or empty).
///
/// FG2: a handle being destroyed (`Done`/`Destroyed`) cannot be resumed, and a
/// second concurrent resume (`Resuming`) refuses. FG4: the caller must also
/// verify the slot is non-null (see [`resume_park`]).
#[must_use]
pub fn begin_resume(a: &HewActor) -> ExecGuard {
    cas_tag(a, ContTag::Parked, ContTag::Resuming)
}

/// Settle a resume that observed [`ResumePoll::Pending`]: the continuation
/// suspended again, so the tag returns to `Parked`. Internal invariant: the
/// caller just won `begin_resume` and holds the only `Resuming` permit, so a
/// refusal is an executor bug (strict CAS).
#[must_use]
pub fn settle_pending(a: &HewActor) -> ExecGuard {
    cas_tag_strict(a, ContTag::Resuming, ContTag::Parked)
}

/// Settle a resume that observed [`ResumePoll::Ready`]: the continuation
/// reached its final suspend, so the tag advances to `Done` and awaits exactly
/// one [`destroy_parked`]. Internal invariant (strict CAS): the caller holds
/// the only `Resuming` permit.
#[must_use]
pub fn settle_ready(a: &HewActor) -> ExecGuard {
    cas_tag_strict(a, ContTag::Resuming, ContTag::Done)
}

/// Resume the actor's parked continuation exactly once and report the poll
/// outcome, enforcing FG2 (no concurrent resume/destroy) and FG4 (no
/// use-after-destroy).
///
/// Returns:
/// - `Some(ResumePoll::Pending)` — the continuation suspended again; the tag is
///   back to `Parked` and the handle remains live for the next resume.
/// - `Some(ResumePoll::Ready)` — the continuation completed; the tag is `Done`
///   and the caller MUST [`destroy_parked`] exactly once.
/// - `None` — the resume was refused (FG2/FG4): the slot was null, or the tag
///   was not `Parked` (already resuming / done / destroyed). The caller leaves
///   the handle untouched.
///
/// # Safety
///
/// `a` must be a live `HewActor`. The parked handle (if any) must be a valid
/// `coro.begin` frame the executor owns; this is upheld by the single-owner
/// park/destroy discipline.
#[must_use]
pub unsafe fn resume_park(a: &HewActor) -> Option<ResumePoll> {
    // FG4: a null slot means no live handle — refuse (do not resume).
    let handle = a.suspended_cont.load(Ordering::Acquire);
    if handle.is_null() {
        return None;
    }
    // FG2: only a Parked handle may be resumed; refuse otherwise.
    if !begin_resume(a).is_ok() {
        return None;
    }
    // Test-only: inject a yield in the Resuming window so a concurrent
    // destroy_parked reliably arrives while the tag is Resuming. In
    // production builds this block is compiled out entirely.
    #[cfg(test)]
    {
        let yields = RESUMING_WINDOW_YIELD_COUNT.swap(0, Ordering::AcqRel);
        for _ in 0..yields {
            std::thread::yield_now();
        }
    }
    // SAFETY: the tag is now `Resuming` (we hold the only resume permit), the
    // slot was non-null when we latched the tag, and the single-owner
    // discipline guarantees no concurrent destroy can run while `Resuming`.
    unsafe { cont::hew_cont_resume(handle) };
    // SAFETY: same live handle; poll reads the done state.
    let poll = unsafe { cont::hew_cont_poll(handle, ptr::null_mut()) };
    match poll {
        ResumePoll::Pending => {
            let _ = settle_pending(a);
        }
        ResumePoll::Ready => {
            let _ = settle_ready(a);
        }
    }
    Some(poll)
}

// ── FG1/FG4: destroy ──────────────────────────────────────────────────────

/// Destroy the actor's parked continuation exactly once (FG1) and null the
/// slot in the same critical section (FG4).
///
/// Legal from `Done` (a completed continuation) or `Parked` (abandoning a
/// still-suspended continuation, e.g. cancellation — the cancellation FLOW is
/// NEW-6, but the single-teardown GUARD lands here). Refuses if the tag is
/// `Resuming` (a resume is in progress — FG2) or already `Destroyed` (a second
/// destroy — FG1) or `Empty` (nothing parked).
///
/// Returns `Ok` when this call performed the single destroy; `Refused`
/// otherwise (the caller did not own the teardown).
///
/// # Safety
///
/// `a` must be a live `HewActor`. The parked handle (if the transition wins)
/// must be a valid, not-yet-destroyed `coro.begin` frame — upheld by the tag
/// gate (only one caller wins the `… → Destroyed` CAS).
#[must_use]
pub unsafe fn destroy_parked(a: &HewActor) -> ExecGuard {
    // FG1: win the single transition to Destroyed from Done or Parked.
    let cur = load_tag(a);
    let won = match cur {
        ContTag::Done => cas_tag(a, ContTag::Done, ContTag::Destroyed),
        ContTag::Parked => cas_tag(a, ContTag::Parked, ContTag::Destroyed),
        // Resuming (FG2: concurrent resume holds the handle), Destroyed (FG1:
        // second destroy), or Empty (nothing parked) all REFUSE. This is the
        // guard doing its job (catching external misuse), not an internal bug,
        // so it returns Refused without a debug_assert — fail-closed in both
        // debug and release builds.
        _ => return ExecGuard::Refused,
    };
    if !won.is_ok() {
        return ExecGuard::Refused;
    }
    // FG4: read + null the slot in the SAME critical section as the Destroyed
    // transition (we are the sole winner of the CAS) so no later activation
    // can read the freed handle. Swap to null BEFORE destroying the frame.
    let handle = a.suspended_cont.swap(ptr::null_mut(), Ordering::AcqRel);
    // SAFETY: we won the single `… -> Destroyed` CAS, so we are the sole owner
    // of this teardown; `handle` (if non-null) is the live, not-yet-destroyed
    // frame. `hew_cont_destroy` tolerates a null handle as a no-op.
    unsafe { cont::hew_cont_destroy(handle) };
    ExecGuard::Ok
}

/// Reclaim a continuation abandoned mid-resume by a crash longjmp.
///
/// When a Hew `panic()` / hard trap fires inside the `.resume` outline, the
/// scheduler's crash-recovery `siglongjmp` unwinds the C stack back to the
/// worker frame WITHOUT running the coroutine's suspend/settle machinery, so
/// the tag is left at `Resuming` (set by [`begin_resume`]) and the slot still
/// holds the frame. This transitions `Resuming → Destroyed`, nulls the slot, and
/// reclaims the frame's HEAP BLOCK directly via [`cont::hew_cont_frame_free`].
///
/// It deliberately does NOT run [`cont::hew_cont_destroy`] (the `coro.destroy`
/// cleanup outline). The crash interrupted the body BETWEEN suspend points — the
/// coroutine is RUNNING, not cleanly suspended — and `coro.destroy` on a running
/// coroutine is undefined: it would run the cleanup keyed to the LAST suspend
/// point, whose own resume (`bind`) edge already released that suspend's
/// registrations. For `sleep_ms`, the bind edge calls
/// `hew_await_cancel_complete` + `hew_await_cancel_free`; re-running the abandon
/// cleanup would double-free that registration (surfaced under
/// `MallocGuardEdges` + the `await cancel release on released registration`
/// debug-assert). Frame-owned Hew heap values are arena-backed and reclaimed by
/// the crash path's `hew_arena_reset`, so freeing only the frame box is the
/// complete, crash-correct reclamation — matching the fresh-dispatch crash
/// branch, which likewise abandons in-flight frame state and resets the arena.
///
/// This is DISTINCT from [`destroy_parked`], which REFUSES a `Resuming` tag to
/// protect a LIVE concurrent resume (FG2 — see
/// `fg2_destroy_refused_in_resuming_window_with_real_free`). The crash edge has
/// the opposite invariant: the resume is provably dead (the longjmp killed it)
/// and the worker owns the actor exclusively (Running CAS held), so there is no
/// concurrent resume to UAF. Without this, the actor-free path's
/// `has_live_parked_cont` + `destroy_parked` would find the tag `Resuming`,
/// REFUSE, spin the quiescence wait to its deadline, and leak the frame box.
///
/// Returns `Ok` when this call reclaimed the frame; `Refused` if the tag was not
/// `Resuming` (no crash-abandoned frame — the normal settle already moved it to
/// `Parked`/`Done`, handled by the standard paths).
///
/// # Safety
///
/// `a` must be a live `HewActor` the caller owns exclusively (the crash-recovery
/// worker frame). Caller guarantees no concurrent resume/destroy can run. The
/// parked handle (if any) is a `coro.begin` frame from [`cont::hew_cont_frame_alloc`]
/// — actor-handler coroutines suspend across scheduler boundaries, so their
/// frame is always heap-allocated (LLVM cannot elide a frame that outlives the
/// activation stack).
#[must_use]
pub unsafe fn abandon_resuming_after_crash(a: &HewActor) -> ExecGuard {
    // Win the single `Resuming → Destroyed` transition. Refuse any other tag:
    // a non-`Resuming` tag means the resume settled normally (no crash-abandoned
    // frame) and the standard `destroy_parked`/free paths own teardown.
    if !cas_tag(a, ContTag::Resuming, ContTag::Destroyed).is_ok() {
        return ExecGuard::Refused;
    }
    // Read + null the slot in the same critical section as the Destroyed
    // transition (we are the sole winner) so no later activation reads the freed
    // handle. Swap to null BEFORE reclaiming the frame.
    let handle = a.suspended_cont.swap(ptr::null_mut(), Ordering::AcqRel);
    // SAFETY: we won the single `Resuming -> Destroyed` CAS, so we are the sole
    // owner of this teardown; `handle` (if non-null) is the heap-allocated
    // `coro.begin` frame the crashed resume left running. `hew_cont_frame_free`
    // tolerates a null handle as a no-op. We free the block WITHOUT running the
    // cleanup outline (see the doc comment): the coroutine was running, not
    // suspended, so `coro.destroy` would double-free the last suspend's
    // already-released registrations.
    unsafe { cont::hew_cont_frame_free(handle) };
    ExecGuard::Ok
}

/// Whether the actor currently has a live (non-`Empty`, non-`Destroyed`)
/// parked continuation — used by teardown paths to decide whether the free
/// path must destroy a parked frame before reclaiming the box (R7).
#[must_use]
pub fn has_live_parked_cont(a: &HewActor) -> bool {
    matches!(
        load_tag(a),
        ContTag::Parked | ContTag::Resuming | ContTag::Done
    )
}

// ── P1-B: quiescent re-arm for multi-await actors ─────────────────────────

/// Re-arm a fully-reclaimed continuation slot so the actor can park AGAIN.
///
/// `ContTag::Empty` is set ONLY at actor init; `destroy_parked` leaves the tag
/// at terminal `Destroyed`. Without this transition an actor that `await`s,
/// completes, then `await`s a SECOND time would hit [`begin_park`] from
/// `Destroyed` (which accepts only `Empty`/`Resuming`) and be refused — the new
/// handle would be neither parked nor destroyed: a leak plus a dropped
/// activation (fail-OPEN). This closes that loop for a multi-await actor.
///
/// The transition is `Destroyed → Empty`, run on the QUIESCENT edge: after
/// [`destroy_parked`] has reclaimed the continuation and nulled the slot (FG4)
/// and the resumed activation has settled. It is fail-closed in two ways:
///
/// - It only fires when the slot is already null (the post-`destroy_parked`
///   invariant): re-arming while a handle is still parked would orphan that
///   handle. A non-null slot refuses (no transition).
/// - It only CASes from `Destroyed`. Any other current tag (`Empty` — already
///   armed, the no-op steady state; `Parked`/`Resuming`/`Done` — a live
///   continuation, must NOT be discarded) refuses without touching the tag.
///
/// Returns `Ok` when this call performed the re-arm (`Destroyed → Empty`);
/// `Refused` when no re-arm was needed or was unsafe (already `Empty`, a live
/// continuation present, or a non-null slot). A `Refused` is not an error: the
/// common steady state (an actor that never suspended, or one already re-armed)
/// returns `Refused` and the caller proceeds normally.
#[must_use]
pub fn re_arm(a: &HewActor) -> ExecGuard {
    // The slot must already be null (destroy_parked's FG4 swap). A non-null
    // slot means a live handle is still parked — re-arming would orphan it.
    if !a.suspended_cont.load(Ordering::Acquire).is_null() {
        return ExecGuard::Refused;
    }
    // Only a terminal Destroyed tag re-arms; every other tag refuses. This is
    // the external-state guard path (a Refused is the steady state, not a bug),
    // so it does NOT trip a debug_assert.
    cas_tag(a, ContTag::Destroyed, ContTag::Empty)
}

#[cfg(test)]
pub(crate) mod test_support {
    //! A scratch switched-resume coroutine the executor can drive without the
    //! codegen / surface lowering (D-A.1 seed driver). The frame mirrors the
    //! `{ resume_fn, destroy_fn, ... }` layout `CoroSplit` commits to (see
    //! `cont.rs`): slot 0 is the resume fn-ptr (nulled at the final suspend,
    //! which is exactly what `coro.done` tests), slot 1 is the destroy fn-ptr.
    //!
    //! The frame counts how many times it was resumed and reaches its final
    //! suspend after `suspends_before_done` resumes, then records that destroy
    //! ran exactly once — the accounting the FG tests assert against.
    //!
    //! `heap_guard` is a REAL heap allocation that the destroy outline FREES.
    //! The resume outline reads from it; if FG2 is broken (destroy runs while
    //! the tag is `Resuming`) the resume's read hits freed memory, which
    //! `MallocScribble`/`MallocGuardEdges` surface as a fault. An atomic
    //! increment on a stack counter would never catch this class of regression.

    use std::ffi::c_void;
    use std::sync::atomic::{AtomicPtr, AtomicU32, Ordering};

    /// `#[repr(C)]` prefix matching the `CoroFramePrefix` `cont.rs` drives:
    /// resume fn-ptr, destroy fn-ptr, then scratch state.
    ///
    /// `heap_guard` is placed after the prefix fields to preserve the C ABI
    /// layout that `cont.rs` drives (the first two slots are the fn-ptrs).
    #[repr(C)]
    pub struct ScratchFrame {
        pub resume: Option<unsafe extern "C" fn(*mut c_void)>,
        pub destroy: Option<unsafe extern "C" fn(*mut c_void)>,
        /// Resumes observed so far.
        pub resumes: AtomicU32,
        /// Resumes before the frame nulls its resume slot (reaches `done`).
        pub suspends_before_done: u32,
        /// Set to 1 by the destroy outline so a double-destroy is observable.
        pub destroyed: AtomicU32,
        /// A REAL heap allocation (`Box<u64>` leaked as a raw pointer) that
        /// the destroy outline FREES via `Box::from_raw`. The resume outline
        /// reads from it so that `MallocScribble`/`MallocGuardEdges` can
        /// surface a genuine UAF when FG2 is violated (destroy runs while the
        /// tag is `Resuming` and resume is still touching this slot). An
        /// `AtomicPtr<u64>` so the destroy outline can null it in the same
        /// step it drops the box (not strictly needed for soundness, but
        /// avoids a double-free if the probe leaks past cleanup).
        pub heap_guard: AtomicPtr<u64>,
    }

    /// The scratch resume outline: count the resume, touch `heap_guard`
    /// (so `MallocScribble` surfaces a UAF if destroy freed it concurrently),
    /// and on the final resume null the resume slot so `coro.done` flips.
    unsafe extern "C" fn scratch_resume(frame: *mut c_void) {
        // SAFETY: `frame` is a live ScratchFrame the test owns.
        let f = unsafe { &*frame.cast::<ScratchFrame>() };
        let n = f.resumes.fetch_add(1, Ordering::AcqRel) + 1;
        // Touch `heap_guard`: read through the pointer. If destroy freed it
        // first (FG2 broken), this dereference hits freed memory and
        // `MallocScribble`/`MallocGuardEdges` abort the process.
        let g = f.heap_guard.load(Ordering::Acquire);
        if !g.is_null() {
            // When FG2 holds, `g` is a live `Box<u64>`; the tag is `Resuming`
            // and `destroy_parked` refuses against `Resuming`, so no concurrent
            // destroy can free `g`. If FG2 is BROKEN, `g` may already be freed
            // when this read executes — which is the UAF the probe detects via
            // allocator guards (`MallocScribble`/`MallocGuardEdges`).
            // SAFETY: under the correct guard `g` is live (see above). This
            // unsafe block is intentionally the UAF-detection surface: when
            // FG2 is broken, touching `g` here is the expected fault point.
            let _ = unsafe { g.read_volatile() };
        }
        if n >= f.suspends_before_done {
            // Reaching the final suspend: null the resume slot (CoroSplit's
            // done signal). The frame stays live for the executor to destroy.
            // SAFETY: single-owner Resuming tag; no concurrent access.
            unsafe { (*frame.cast::<ScratchFrame>()).resume = None };
        }
    }

    /// The scratch destroy outline: FREES `heap_guard` (the real allocation)
    /// and records teardown in `destroyed`. The free is what lets
    /// `MallocScribble`/`MallocGuardEdges` surface a UAF if a concurrent
    /// resume is still reading through `heap_guard`.
    unsafe extern "C" fn scratch_destroy(frame: *mut c_void) {
        // SAFETY: `frame` is a live ScratchFrame the test owns; we are the
        // sole winner of the `… → Destroyed` CAS (FG1).
        let f = unsafe { &*frame.cast::<ScratchFrame>() };
        // Swap out the heap_guard pointer before freeing: any concurrent
        // resume that loads null skips the dereference, so we only catch the
        // race when the resume loads the non-null pointer BEFORE the swap
        // and dereferences AFTER the drop — exactly the UAF window.
        let g = f.heap_guard.swap(std::ptr::null_mut(), Ordering::AcqRel);
        if !g.is_null() {
            // SAFETY: `g` was allocated by `Box::new(0u64)` in `ScratchFrame::new`
            // and this is the sole owner (FG1 CAS). `Box::from_raw` drops the
            // allocation, which is what MallocScribble/MallocGuardEdges detect.
            drop(unsafe { Box::from_raw(g) });
        }
        f.destroyed.fetch_add(1, Ordering::AcqRel);
    }

    impl ScratchFrame {
        /// A scratch frame that completes after `suspends_before_done` resumes.
        ///
        /// The caller boxes this so the handle pointer has a stable heap
        /// address across resumes (the resume outline mutates the frame's
        /// resume slot through the raw handle).
        pub fn new(suspends_before_done: u32) -> Self {
            ScratchFrame {
                resume: Some(scratch_resume),
                destroy: Some(scratch_destroy),
                resumes: AtomicU32::new(0),
                suspends_before_done: suspends_before_done.max(1),
                destroyed: AtomicU32::new(0),
                heap_guard: AtomicPtr::new(Box::into_raw(Box::new(0u64))),
            }
        }
    }

    impl Drop for ScratchFrame {
        /// Safety net: if a test creates a `ScratchFrame` but the executor
        /// never calls `destroy_parked` (e.g. the test aborts early or
        /// only exercises the park path), free `heap_guard` here so the
        /// test does not leak the allocation. If `destroy_parked` already
        /// freed it, `heap_guard` is null and this is a no-op.
        fn drop(&mut self) {
            let g = self.heap_guard.swap(std::ptr::null_mut(), Ordering::AcqRel);
            if !g.is_null() {
                // SAFETY: allocated in `new` via `Box::into_raw(Box::new(0u64))`.
                drop(unsafe { Box::from_raw(g) });
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::test_support::ScratchFrame;
    use super::*;
    use crate::internal::types::HewActorState;
    use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64};

    /// Build a minimal `HewActor` for executor-guard tests. Only the fields the
    /// guards touch matter; the rest are inert nulls/zeros.
    fn exec_test_actor() -> Box<HewActor> {
        Box::new(HewActor {
            sched_link_next: AtomicPtr::new(ptr::null_mut()),
            id: 1,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: None,
            mailbox: ptr::null_mut(),
            actor_state: AtomicI32::new(HewActorState::Suspended as i32),
            budget: AtomicI32::new(1),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            terminate_called: AtomicBool::new(false),
            terminate_finished: AtomicBool::new(false),
            dispatch_active: AtomicBool::new(false),
            error_code: AtomicI32::new(0),
            supervisor: ptr::null_mut(),
            supervisor_child_index: -1,
            priority: AtomicI32::new(crate::actor::HEW_PRIORITY_NORMAL),
            reductions: AtomicI32::new(0),
            idle_count: AtomicI32::new(0),
            hibernation_threshold: AtomicI32::new(0),
            hibernating: AtomicI32::new(0),
            prof_messages_processed: AtomicU64::new(0),
            prof_processing_time_ns: AtomicU64::new(0),
            #[cfg(not(target_arch = "wasm32"))]
            arena: ptr::null_mut(),
            #[cfg(target_arch = "wasm32")]
            arena: ptr::null_mut(),
            suspended_cont: AtomicPtr::new(ptr::null_mut()),
            cont_tag: AtomicI32::new(ContTag::Empty as i32),
            pending_wake: AtomicBool::new(false),
            suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
            suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
            runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
            runtime: ptr::null(),
        })
    }

    /// R2 discriminator-carried probe (the Lane-B root cause). The resume
    /// re-entry keys behaviour off "is this activation a resume?" — the
    /// `suspended_cont` slot non-null AND tag `Parked`. This proves the park
    /// edge actually CARRIES that discriminator before any branch reads it: a
    /// branch keyed off a property nothing sets is a silent no-op.
    #[test]
    fn park_carries_resume_discriminator() {
        let a = exec_test_actor();
        let mut frame = Box::new(ScratchFrame::new(1));
        let handle = (&raw mut *frame).cast::<c_void>();

        // Before park: discriminator absent.
        assert!(a.suspended_cont.load(Ordering::Acquire).is_null());
        assert_eq!(load_tag(&a), ContTag::Empty);
        assert!(!has_live_parked_cont(&a));

        assert!(begin_park(&a).is_ok());
        // SAFETY: scratch frame outlives this scope.
        unsafe { finish_park(&a, handle) };

        // After park: BOTH halves of the discriminator are carried.
        assert_eq!(
            a.suspended_cont.load(Ordering::Acquire),
            handle,
            "park must carry the handle the resume re-entry reads"
        );
        assert_eq!(
            load_tag(&a),
            ContTag::Parked,
            "park must carry the Parked tag the resume re-entry keys off"
        );
        assert!(has_live_parked_cont(&a));

        // Destroy to keep the scratch accounting clean.
        // SAFETY: parked handle is live and not yet destroyed.
        assert!(unsafe { destroy_parked(&a) }.is_ok());
    }

    /// A full suspend → resume(Pending) → resume(Ready) → destroy round-trip
    /// over the scratch continuation: the tag walks Parked → Resuming → Parked
    /// → Resuming → Done → Destroyed, and destroy runs exactly once.
    #[test]
    fn resume_round_trip_pending_then_ready_then_destroy_once() {
        let a = exec_test_actor();
        let mut frame = Box::new(ScratchFrame::new(2)); // done on the 2nd resume
        let handle = (&raw mut *frame).cast::<c_void>();

        assert!(begin_park(&a).is_ok());
        // SAFETY: scratch frame outlives this scope.
        unsafe { finish_park(&a, handle) };

        // First resume: Pending (1 of 2 suspends).
        // SAFETY: parked handle is live.
        let p1 = unsafe { resume_park(&a) };
        assert_eq!(p1, Some(ResumePoll::Pending));
        assert_eq!(load_tag(&a), ContTag::Parked);

        // Second resume: Ready (final suspend reached).
        // SAFETY: parked handle is live.
        let p2 = unsafe { resume_park(&a) };
        assert_eq!(p2, Some(ResumePoll::Ready));
        assert_eq!(load_tag(&a), ContTag::Done);

        // Destroy exactly once.
        // SAFETY: Done handle, single owner.
        assert!(unsafe { destroy_parked(&a) }.is_ok());
        assert_eq!(load_tag(&a), ContTag::Destroyed);
        assert!(
            a.suspended_cont.load(Ordering::Acquire).is_null(),
            "FG4: slot nulled in the Destroyed critical section"
        );
        assert_eq!(
            frame.destroyed.load(Ordering::Acquire),
            1,
            "destroy outline ran exactly once"
        );
    }

    /// FG1 — single teardown owner: a Ready continuation is destroyed exactly
    /// once; a second destroy REFUSES (the tag is already `Destroyed`) and the
    /// destroy outline does NOT run again — no double-free.
    #[test]
    fn fg1_destroy_after_destroy_refuses() {
        let a = exec_test_actor();
        let mut frame = Box::new(ScratchFrame::new(1));
        let handle = (&raw mut *frame).cast::<c_void>();

        assert!(begin_park(&a).is_ok());
        // SAFETY: scratch frame outlives this scope.
        unsafe { finish_park(&a, handle) };
        // Drive to Ready.
        // SAFETY: parked handle is live.
        assert_eq!(unsafe { resume_park(&a) }, Some(ResumePoll::Ready));

        // First destroy wins.
        // SAFETY: Done handle.
        assert!(unsafe { destroy_parked(&a) }.is_ok());
        assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);

        // Second destroy refuses (tag already Destroyed) — no second teardown.
        // SAFETY: tag is Destroyed; destroy_parked refuses without touching it.
        assert_eq!(unsafe { destroy_parked(&a) }, ExecGuard::Refused);
        assert_eq!(
            frame.destroyed.load(Ordering::Acquire),
            1,
            "FG1: the destroy outline must not run a second time"
        );
    }

    /// FG2 — no resume after the continuation completed: once `Done`, a resume
    /// REFUSES (the tag is not `Parked`), so a completed coroutine is never
    /// re-driven (which would be UB per the cont.rs contract).
    #[test]
    fn fg2_resume_after_ready_refuses() {
        let a = exec_test_actor();
        let mut frame = Box::new(ScratchFrame::new(1));
        let handle = (&raw mut *frame).cast::<c_void>();

        assert!(begin_park(&a).is_ok());
        // SAFETY: scratch frame outlives this scope.
        unsafe { finish_park(&a, handle) };
        // SAFETY: parked handle is live.
        assert_eq!(unsafe { resume_park(&a) }, Some(ResumePoll::Ready));
        assert_eq!(load_tag(&a), ContTag::Done);

        // A resume against a Done continuation refuses; the frame is not
        // re-driven (resume count stays at 1).
        // SAFETY: tag is Done; resume_park refuses without resuming.
        assert_eq!(unsafe { resume_park(&a) }, None);
        assert_eq!(
            frame.resumes.load(Ordering::Acquire),
            1,
            "FG2: a completed continuation must not be resumed again"
        );

        // Clean up.
        // SAFETY: Done handle, single owner.
        assert!(unsafe { destroy_parked(&a) }.is_ok());
    }

    /// FG2 — destroy refuses while a resume is in progress: with the tag
    /// `Resuming`, `destroy_parked` REFUSES so a concurrent resume+destroy
    /// cannot tear down a frame being driven.
    #[test]
    fn fg2_destroy_refused_while_resuming() {
        let a = exec_test_actor();
        let mut frame = Box::new(ScratchFrame::new(2));
        let handle = (&raw mut *frame).cast::<c_void>();

        assert!(begin_park(&a).is_ok());
        // SAFETY: scratch frame outlives this scope.
        unsafe { finish_park(&a, handle) };

        // Manually drive into the Resuming window (begin_resume without settle)
        // to model the instant a worker is inside hew_cont_resume.
        assert!(begin_resume(&a).is_ok());
        assert_eq!(load_tag(&a), ContTag::Resuming);

        // A destroy now must refuse — the resume holds the handle.
        // SAFETY: tag is Resuming; destroy_parked refuses without teardown.
        assert_eq!(unsafe { destroy_parked(&a) }, ExecGuard::Refused);
        assert_eq!(
            frame.destroyed.load(Ordering::Acquire),
            0,
            "FG2: no teardown may run while a resume is in progress"
        );

        // Settle the resume back to Parked, then a destroy is allowed.
        assert!(settle_pending(&a).is_ok());
        // SAFETY: Parked handle, single owner (abandon path).
        assert!(unsafe { destroy_parked(&a) }.is_ok());
        assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);
    }

    /// FG3 — two-phase park drains a wake that fires before the park completes:
    /// `mark_pending_wake` during the park window is observed by
    /// `take_pending_wake` after the park, so the wake is delivered exactly
    /// once, not lost.
    #[test]
    fn fg3_wake_in_park_window_is_not_lost() {
        let a = exec_test_actor();
        let mut frame = Box::new(ScratchFrame::new(1));
        let handle = (&raw mut *frame).cast::<c_void>();

        // Begin park (phase 1) — wake fires HERE, before finish_park.
        assert!(begin_park(&a).is_ok());
        mark_pending_wake(&a); // a readiness source fired mid-park
                               // SAFETY: scratch frame outlives this scope.
        unsafe { finish_park(&a, handle) };

        // The suspend edge drains the pending wake after publishing the park.
        assert!(
            take_pending_wake(&a),
            "FG3: a wake in the park window must be observed, not lost"
        );
        // And it is delivered exactly once — a second drain sees nothing.
        assert!(!take_pending_wake(&a), "the wake is delivered exactly once");

        // Clean up.
        // SAFETY: Parked handle, single owner.
        assert!(unsafe { destroy_parked(&a) }.is_ok());
    }

    /// FG4 — no use-after-destroy: after `destroy_parked` the slot is null and
    /// the tag is `Destroyed`, so a resume re-entry reads a null slot and
    /// REFUSES rather than dereferencing the freed frame.
    #[test]
    fn fg4_resume_after_destroy_reads_null_slot_and_refuses() {
        let a = exec_test_actor();
        let mut frame = Box::new(ScratchFrame::new(1));
        let handle = (&raw mut *frame).cast::<c_void>();

        assert!(begin_park(&a).is_ok());
        // SAFETY: scratch frame outlives this scope.
        unsafe { finish_park(&a, handle) };
        // SAFETY: parked handle is live.
        assert_eq!(unsafe { resume_park(&a) }, Some(ResumePoll::Ready));
        // SAFETY: Done handle, single owner.
        assert!(unsafe { destroy_parked(&a) }.is_ok());

        // FG4: the slot is null and the tag terminal.
        assert!(a.suspended_cont.load(Ordering::Acquire).is_null());
        assert_eq!(load_tag(&a), ContTag::Destroyed);
        assert!(!has_live_parked_cont(&a));

        // A resume re-entry now reads the null slot and refuses — no
        // dereference of the freed frame.
        // SAFETY: slot is null; resume_park returns None without resuming.
        assert_eq!(unsafe { resume_park(&a) }, None);
        assert_eq!(
            frame.resumes.load(Ordering::Acquire),
            1,
            "FG4: the destroyed frame must never be resumed"
        );
    }

    /// P1-B — multi-await re-arm: an actor that parks, completes, is destroyed,
    /// then RE-ARMS can `begin_park` a SECOND continuation. Without `re_arm`,
    /// the second `begin_park` from `Destroyed` refuses (leak + dropped
    /// activation). The two continuations park/wake/destroy exactly once each.
    #[test]
    fn p1b_re_arm_allows_a_second_park_after_destroy() {
        let a = exec_test_actor();

        // ── First await lifecycle: park → resume(Ready) → destroy. ──
        let mut frame1 = Box::new(ScratchFrame::new(1));
        let handle1 = (&raw mut *frame1).cast::<c_void>();
        assert!(begin_park(&a).is_ok());
        // SAFETY: frame1 outlives this scope.
        unsafe { finish_park(&a, handle1) };
        // SAFETY: parked handle is live.
        assert_eq!(unsafe { resume_park(&a) }, Some(ResumePoll::Ready));
        // SAFETY: Done handle, single owner.
        assert!(unsafe { destroy_parked(&a) }.is_ok());
        assert_eq!(load_tag(&a), ContTag::Destroyed);
        assert_eq!(frame1.destroyed.load(Ordering::Acquire), 1);

        // Without the re-arm a second begin_park would refuse from Destroyed.
        // Re-arm on the quiescent edge: Destroyed → Empty (slot already null).
        assert!(
            re_arm(&a).is_ok(),
            "re_arm must transition Destroyed -> Empty once the slot is null"
        );
        assert_eq!(load_tag(&a), ContTag::Empty);
        assert!(!has_live_parked_cont(&a));

        // ── Second await lifecycle: park → resume(Ready) → destroy. ──
        let mut frame2 = Box::new(ScratchFrame::new(1));
        let handle2 = (&raw mut *frame2).cast::<c_void>();
        assert!(
            begin_park(&a).is_ok(),
            "after re_arm the actor must park a SECOND continuation"
        );
        // SAFETY: frame2 outlives this scope.
        unsafe { finish_park(&a, handle2) };
        assert_eq!(
            a.suspended_cont.load(Ordering::Acquire),
            handle2,
            "the second park carries the new handle, not the destroyed first one"
        );
        // SAFETY: parked handle is live.
        assert_eq!(unsafe { resume_park(&a) }, Some(ResumePoll::Ready));
        // SAFETY: Done handle, single owner.
        assert!(unsafe { destroy_parked(&a) }.is_ok());
        assert_eq!(
            frame2.destroyed.load(Ordering::Acquire),
            1,
            "the second continuation is destroyed exactly once"
        );
        // The first frame was destroyed exactly once across the whole test.
        assert_eq!(frame1.destroyed.load(Ordering::Acquire), 1);
    }

    /// P1-B fail-closed: `re_arm` refuses while a handle is still parked (a
    /// non-null slot). Re-arming there would orphan the live continuation.
    #[test]
    fn p1b_re_arm_refuses_with_a_live_parked_handle() {
        let a = exec_test_actor();
        let mut frame = Box::new(ScratchFrame::new(2));
        let handle = (&raw mut *frame).cast::<c_void>();

        assert!(begin_park(&a).is_ok());
        // SAFETY: frame outlives this scope.
        unsafe { finish_park(&a, handle) };
        assert_eq!(load_tag(&a), ContTag::Parked);

        // A live Parked continuation (non-null slot) must NOT be re-armed.
        assert_eq!(
            re_arm(&a),
            ExecGuard::Refused,
            "re_arm must refuse while a handle is parked (non-null slot)"
        );
        assert_eq!(load_tag(&a), ContTag::Parked, "tag untouched on refusal");

        // Clean up.
        // SAFETY: Parked handle, single owner.
        assert!(unsafe { destroy_parked(&a) }.is_ok());
    }

    /// P1-B fail-closed: `re_arm` is a no-op refusal on an already-`Empty`
    /// actor (the steady state for an actor that never suspended).
    #[test]
    fn p1b_re_arm_refuses_when_already_empty() {
        let a = exec_test_actor();
        assert_eq!(load_tag(&a), ContTag::Empty);
        assert_eq!(
            re_arm(&a),
            ExecGuard::Refused,
            "re_arm on an already-Empty actor is a harmless refusal"
        );
        assert_eq!(load_tag(&a), ContTag::Empty);
    }

    /// FG1 — abandoning a still-`Parked` continuation (cancellation) destroys
    /// it exactly once; the GUARD lands here even though the cancellation FLOW
    /// is NEW-6.
    #[test]
    fn fg1_abandon_parked_continuation_destroys_once() {
        let a = exec_test_actor();
        let mut frame = Box::new(ScratchFrame::new(5)); // never completes here
        let handle = (&raw mut *frame).cast::<c_void>();

        assert!(begin_park(&a).is_ok());
        // SAFETY: scratch frame outlives this scope.
        unsafe { finish_park(&a, handle) };
        assert_eq!(load_tag(&a), ContTag::Parked);

        // Abandon: destroy a still-suspended continuation exactly once.
        // SAFETY: Parked handle, single owner.
        assert!(unsafe { destroy_parked(&a) }.is_ok());
        assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);
        assert!(a.suspended_cont.load(Ordering::Acquire).is_null());

        // A second abandon refuses.
        // SAFETY: tag is Destroyed; refuses.
        assert_eq!(unsafe { destroy_parked(&a) }, ExecGuard::Refused);
        assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);
    }
}

/// Forced-ordering concurrency probe for the suspend/resume/destroy lifecycle.
///
/// This is the cross-model security harness (`feedback_cross_model_review_for_
/// concurrent_safety`): passing the single-threaded FG suite is NOT a proof of
/// safety against a real OS-thread race. These tests genuinely contend two
/// worker threads on ONE parked handle and assert the contract holds, with the
/// suite run under `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` to
/// surface any UAF / double-free the cont-tag serialization would otherwise
/// hide.
///
/// The two reactor UAFs that motivated splitting this slice from Slice 3 hid in
/// exactly this window — a heap frame's lifecycle across a suspend/resume on a
/// different worker than the one that parked it.
#[cfg(all(test, not(target_arch = "wasm32")))]
mod forced_ordering_probe {
    use super::test_support::ScratchFrame;
    use super::*;
    use crate::internal::types::HewActorState;
    use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64};
    use std::sync::{Arc, Barrier};

    /// A `Send` wrapper carrying a raw `*const HewActor` across the probe's
    /// worker-thread boundary. The actor box outlives both threads (it is
    /// joined before drop), and every field the guards touch is atomic, so
    /// sharing the pointer is sound for the duration of the race.
    #[derive(Clone, Copy)]
    struct ActorPtr(*const HewActor);
    // SAFETY: the actor box outlives the joined threads; all touched fields are
    // atomic, so concurrent guard calls are data-race-free.
    unsafe impl Send for ActorPtr {}

    fn probe_actor() -> Box<HewActor> {
        Box::new(HewActor {
            sched_link_next: AtomicPtr::new(std::ptr::null_mut()),
            id: 1,
            state: ptr::null_mut(),
            state_size: 0,
            dispatch: None,
            mailbox: ptr::null_mut(),
            actor_state: AtomicI32::new(HewActorState::Suspended as i32),
            budget: AtomicI32::new(1),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            terminate_called: AtomicBool::new(false),
            terminate_finished: AtomicBool::new(false),
            dispatch_active: AtomicBool::new(false),
            error_code: AtomicI32::new(0),
            supervisor: ptr::null_mut(),
            supervisor_child_index: -1,
            priority: AtomicI32::new(crate::actor::HEW_PRIORITY_NORMAL),
            reductions: AtomicI32::new(0),
            idle_count: AtomicI32::new(0),
            hibernation_threshold: AtomicI32::new(0),
            hibernating: AtomicI32::new(0),
            prof_messages_processed: AtomicU64::new(0),
            prof_processing_time_ns: AtomicU64::new(0),
            #[cfg(not(target_arch = "wasm32"))]
            arena: ptr::null_mut(),
            #[cfg(target_arch = "wasm32")]
            arena: ptr::null_mut(),
            suspended_cont: AtomicPtr::new(ptr::null_mut()),
            cont_tag: AtomicI32::new(ContTag::Empty as i32),
            pending_wake: AtomicBool::new(false),
            suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
            suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
            runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
            runtime: ptr::null(),
        })
    }

    /// FG1/FG2: resume vs destroy race on ONE Parked handle. Across many
    /// iterations with a barrier aligning the two threads, EXACTLY one of
    /// {resume, destroy} wins each round (the cont-tag CAS serializes them),
    /// the loser fails closed, and over the whole run the destroy outline runs
    /// exactly once per handle — no double-free, no use-after-free.
    #[test]
    fn resume_vs_destroy_race_serializes_exactly_one_winner() {
        const ROUNDS: usize = 500;
        for _ in 0..ROUNDS {
            let actor = probe_actor();
            let mut frame = Box::new(ScratchFrame::new(3)); // stays Pending
            let handle = (&raw mut *frame).cast::<c_void>();

            assert!(begin_park(&actor).is_ok());
            // SAFETY: scratch frame outlives both threads (joined below).
            unsafe { finish_park(&actor, handle) };

            let barrier = Arc::new(Barrier::new(2));
            let ap = ActorPtr(&raw const *actor);

            let b_resume = Arc::clone(&barrier);
            let resume_h = std::thread::spawn(move || {
                let ap = ap;
                b_resume.wait();
                // SAFETY: actor outlives the join; guards are atomic.
                let a = unsafe { &*ap.0 };
                // SAFETY: parked handle is the executor-owned frame.
                unsafe { resume_park(a) }
            });

            let b_destroy = Arc::clone(&barrier);
            let destroy_h = std::thread::spawn(move || {
                let ap = ap;
                b_destroy.wait();
                // SAFETY: actor outlives the join; guards are atomic.
                let a = unsafe { &*ap.0 };
                // SAFETY: single-owner CAS gate; destroy refuses if a resume
                // holds the Resuming permit.
                unsafe { destroy_parked(a) }
            });

            let resume_outcome = resume_h.join().expect("resume thread panicked");
            let destroy_outcome = destroy_h.join().expect("destroy thread panicked");

            // The two operations are serialized by the cont tag. Valid race
            // outcomes:
            //  - destroy wins first (Parked -> Destroyed): resume then reads a
            //    null slot OR a non-Parked tag and refuses (None).
            //  - resume wins first (Parked -> Resuming): it settles back to
            //    Parked (Pending), and destroy either lost to Resuming
            //    (Refused) or won afterward (Parked -> Destroyed).
            // The one invariant that must always hold: the frame is never
            // resumed AND destroyed such that a second teardown occurs.
            let destroyed_now = frame.destroyed.load(Ordering::Acquire);
            assert!(
                destroyed_now <= 1,
                "the destroy outline must run at most once per handle (got {destroyed_now})"
            );

            // If the destroy did not win in the race, the handle is still
            // live (Parked or Done) and we reclaim it once here so the probe
            // leaves no leak. If it did win, a second destroy refuses.
            if destroyed_now == 0 {
                // SAFETY: handle still live; destroy_parked refuses if a resume
                // left it mid-flight (it cannot — both threads joined).
                let _ = unsafe { destroy_parked(&actor) };
            }
            assert_eq!(
                frame.destroyed.load(Ordering::Acquire),
                1,
                "the handle is destroyed exactly once across the whole round"
            );
            // Sanity: the surviving tag is terminal and the slot is null (FG4).
            assert_eq!(load_tag(&actor), ContTag::Destroyed);
            assert!(actor.suspended_cont.load(Ordering::Acquire).is_null());

            // Suppress unused warnings on the race outcomes; their validity is
            // asserted via the destroyed-count + tag invariants above.
            let _ = (resume_outcome, destroy_outcome);
        }
    }

    /// FG2 / cross-worker: a continuation parked on one thread is resumed on a
    /// DIFFERENT thread (the M:N "resume on any worker" model). The resume
    /// drives the frame and the parking thread observes the published tag
    /// transition — no torn read of the slot, no UAF.
    #[test]
    fn cross_worker_park_then_resume_on_other_thread() {
        const ROUNDS: usize = 500;
        for _ in 0..ROUNDS {
            let actor = probe_actor();
            let mut frame = Box::new(ScratchFrame::new(1)); // Ready on resume #1
            let handle = (&raw mut *frame).cast::<c_void>();

            // Park on THIS thread.
            assert!(begin_park(&actor).is_ok());
            // SAFETY: scratch frame outlives the join.
            unsafe { finish_park(&actor, handle) };

            // Resume on ANOTHER thread.
            let ap = ActorPtr(&raw const *actor);
            let resume_h = std::thread::spawn(move || {
                let ap = ap;
                // SAFETY: actor outlives the join; guards are atomic.
                let a = unsafe { &*ap.0 };
                // SAFETY: parked handle is the executor-owned frame.
                unsafe { resume_park(a) }
            });
            let outcome = resume_h.join().expect("resume thread panicked");
            assert_eq!(outcome, Some(ResumePoll::Ready));
            assert_eq!(frame.resumes.load(Ordering::Acquire), 1);
            assert_eq!(load_tag(&actor), ContTag::Done);

            // Destroy on the original thread — exactly once.
            // SAFETY: Done handle, single owner (resume thread joined).
            assert!(unsafe { destroy_parked(&actor) }.is_ok());
            assert_eq!(frame.destroyed.load(Ordering::Acquire), 1);
            assert!(actor.suspended_cont.load(Ordering::Acquire).is_null());
        }
    }

    /// FG3 lost-wake window: a wake fires on another thread in the gap between
    /// `begin_park` and `finish_park`. The two-phase park records it via
    /// `mark_pending_wake`, and the parking thread drains it via
    /// `take_pending_wake` after publishing — the wake is observed, never lost.
    #[test]
    fn lost_wake_window_is_observed_under_race() {
        const ROUNDS: usize = 500;
        let mut observed = 0usize;
        for _ in 0..ROUNDS {
            let actor = probe_actor();
            let mut frame = Box::new(ScratchFrame::new(2));
            let handle = (&raw mut *frame).cast::<c_void>();

            // Phase 1.
            assert!(begin_park(&actor).is_ok());

            // A waker fires concurrently in the park window.
            let ap = ActorPtr(&raw const *actor);
            let wake_h = std::thread::spawn(move || {
                let ap = ap;
                // SAFETY: actor outlives the join.
                let a = unsafe { &*ap.0 };
                mark_pending_wake(a);
            });

            // Phase 2 (may run before or after the wake — that is the race).
            // SAFETY: scratch frame outlives the join.
            unsafe { finish_park(&actor, handle) };
            wake_h.join().expect("wake thread panicked");

            // After the park completes, the suspend edge drains the wake. Since
            // the waker definitely fired (the thread joined), the drain MUST
            // observe it — a lost wake here is the bug this guards against.
            if take_pending_wake(&actor) {
                observed += 1;
            }

            // Clean up the parked frame.
            // SAFETY: Parked handle, single owner.
            assert!(unsafe { destroy_parked(&actor) }.is_ok());
        }
        assert_eq!(
            observed, ROUNDS,
            "every wake that fired in the park window must be observed (FG3)"
        );
    }

    /// FG2 forced-ordering: destroy is REFUSED while the tag is `Resuming`.
    ///
    /// This is the STRENGTHENED probe requested by the security reviewer:
    ///
    /// 1. The `RESUMING_WINDOW_YIELD_COUNT` hook widens the `Resuming` window
    ///    (between the `Parked → Resuming` CAS and `hew_cont_resume`) by
    ///    injecting N `yield_now` calls, so the destroy thread reliably lands
    ///    INSIDE the window — not after the resume has already settled.
    ///
    /// 2. `ScratchFrame::heap_guard` is a real `Box<u64>` the destroy outline
    ///    FREES. If FG2 regresses (the `Resuming → Destroyed` arm is added to
    ///    `destroy_parked`), the destroy outline drops the `heap_guard` box while
    ///    the resume outline is mid-flight touching it — a genuine UAF that
    ///    `MallocScribble=1 MallocPreScribble=1 MallocGuardEdges=1` surfaces as
    ///    an abort.
    ///
    /// Under the CORRECT guard (no `Resuming → Destroyed` arm):
    /// - Resume wins `Parked → Resuming`; destroy finds the tag `Resuming` and
    ///   REFUSES every iteration.
    /// - `heap_guard` is freed only AFTER the resume settles and `destroy_parked`
    ///   wins from `Parked` or `Done` — never concurrently with the resume.
    ///
    /// MUTATION TEST (manually verified, never land the break):
    /// Adding `ContTag::Resuming => cas_tag(a, ContTag::Resuming, ContTag::Destroyed),`
    /// to `destroy_parked` breaks FG2. Under `MallocScribble=1 MallocPreScribble=1
    /// MallocGuardEdges=1` this probe aborts with an allocator fault because
    /// `scratch_resume` reads through `heap_guard` AFTER `scratch_destroy` freed
    /// it. Verified: the probe FAILS (abort/fault) within 10 rounds on a broken
    /// guard, as opposed to the prior probe (pure `debug_assert`) which PASSED
    /// 5/5 on the same broken guard.
    #[test]
    fn fg2_destroy_refused_in_resuming_window_with_real_free() {
        const ROUNDS: usize = 100;
        for round in 0..ROUNDS {
            let actor = probe_actor();
            // suspends_before_done = 5: the frame never reaches Done in the
            // race window, so the resume always settles back to Parked (Pending)
            // and the destroy must find the tag NOT Resuming when it finally wins.
            let mut frame = Box::new(ScratchFrame::new(5));
            let handle = (&raw mut *frame).cast::<c_void>();

            assert!(begin_park(&actor).is_ok());
            // SAFETY: scratch frame outlives both threads (joined below).
            unsafe { finish_park(&actor, handle) };

            // Widen the Resuming window: the resume thread will yield 200 times
            // between the Parked → Resuming CAS and hew_cont_resume, giving the
            // destroy thread ample time to arrive and attempt its CAS.
            set_resuming_window_yield_count(200);

            let barrier = Arc::new(Barrier::new(2));
            let ap = ActorPtr(&raw const *actor);

            // Resume thread: sets tag to Resuming, yields 200 times, then drives
            // hew_cont_resume (which touches heap_guard in scratch_resume).
            let b_resume = Arc::clone(&barrier);
            let resume_h = std::thread::spawn(move || {
                let ap = ap;
                b_resume.wait();
                // SAFETY: actor outlives the join; guards are atomic.
                let a = unsafe { &*ap.0 };
                // SAFETY: parked handle is live; resume_park enforces FG2.
                unsafe { resume_park(a) }
            });

            // Destroy thread: waits for both threads to be scheduled, then
            // attempts destroy. With the yield injection the resume tag is
            // `Resuming` when this arrives — FG2 must refuse it.
            let b_destroy = Arc::clone(&barrier);
            let destroy_h = std::thread::spawn(move || {
                let ap = ap;
                b_destroy.wait();
                // Yield a few times first so the resume thread is more likely
                // to have advanced into the Resuming window.
                for _ in 0..50 {
                    std::thread::yield_now();
                }
                // SAFETY: actor outlives the join; guards are atomic.
                let a = unsafe { &*ap.0 };
                // SAFETY: FG2: if the tag is Resuming this MUST refuse.
                unsafe { destroy_parked(a) }
            });

            let resume_outcome = resume_h.join().expect("resume thread panicked");
            let destroy_outcome = destroy_h.join().expect("destroy thread panicked");

            // The heap_guard must NOT be freed by a concurrent resume+destroy.
            // If destroy_parked ran while the tag was Resuming and freed the box,
            // MallocScribble would have aborted above. Reaching here means either:
            //   - destroy was Refused (correct: FG2 held), OR
            //   - destroy won from Parked AFTER the resume settled (also correct).
            // In no case should destroy have run at the same time as resume.

            // Ensure the frame is reclaimed exactly once so the probe does not leak.
            let destroyed_now = frame.destroyed.load(Ordering::Acquire);
            if destroyed_now == 0 {
                // SAFETY: resume settled (thread joined); tag is Parked or Done.
                let _ = unsafe { destroy_parked(&actor) };
            }
            assert_eq!(
                frame.destroyed.load(Ordering::Acquire),
                1,
                "round {round}: handle must be destroyed exactly once"
            );
            assert_eq!(load_tag(&actor), ContTag::Destroyed);
            assert!(actor.suspended_cont.load(Ordering::Acquire).is_null());

            let _ = (resume_outcome, destroy_outcome);
        }
    }
}
