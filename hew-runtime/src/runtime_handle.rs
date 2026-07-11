//! Public host/embedder handle to a Hew runtime.
//!
//! This is the external, handle-shaped boundary through which a host (an
//! embedder, and later the JIT `:reset` session driver) names, holds, and
//! tears down a runtime — the first point at which "an external owner keeps the
//! runtime alive while it owns work" exists at all.
//!
//! # Scope (AOT-first)
//!
//! In this milestone there is exactly one runtime — the process default in
//! `DEFAULT_RUNTIME`. The handle is an **owner token** for that single
//! runtime's lifecycle, reference-counted so multiple host owners can share
//! it; the last release runs the runtime teardown. It does **not** yet create a
//! second concurrent runtime — two-runtime isolation is a later milestone. The
//! handle adds no `.hew` user-language surface and no new JIT capability; it is
//! a host C ABI plus a refcount.
//!
//! Because there is exactly one runtime, every `hew_runtime_new` returns the
//! same process-singleton handle (`DEFAULT_HANDLE`) and the owner refcount is
//! **per-runtime** — shared across all `new`/`retain` owners of that one
//! runtime — not per call. So `h1 = new(); h2 = new();` are two owners of the
//! *same* runtime (count 2), and teardown runs only when the last of them
//! releases. A later milestone that mints a second concurrent runtime gives
//! each runtime its own handle + per-runtime count; the per-runtime invariant
//! is the same, the singleton is the single-runtime specialisation of it.
//!
//! # Lifecycle ABI
//!
//! - [`hew_runtime_new`] — initialise the runtime (delegating to
//!   `hew_sched_init`) and return the singleton handle, adding one strong
//!   reference (the first `new` takes the count 0→1; a second `new` takes it
//!   1→2 — two owners of the one runtime).
//! - [`hew_runtime_retain`] — add a strong reference.
//! - [`hew_runtime_release`] — drop a strong reference; the last release (count
//!   →0) runs the runtime teardown ([`hew_runtime_shutdown`]'s sequence). The
//!   handle is a process-static singleton, so the release never frees it — a
//!   release past zero is a safe no-op (tombstone), not a double-free.
//! - [`hew_runtime_shutdown`] — explicit owner teardown (join workers + the
//!   `hew_runtime_cleanup` order) without freeing the handle, so a host that
//!   wants deterministic shutdown separate from the last `release` can drive
//!   it; idempotent.
//!
//! The legacy no-arg lifecycle symbols (`hew_sched_init`, `hew_sched_shutdown`,
//! `hew_runtime_cleanup`) keep their signatures and behaviour for existing
//! compiled programs — the handle owner-teardown deliberately uses distinct
//! names (`hew_runtime_shutdown` / `hew_runtime_release`) so it does not shadow
//! the no-arg `hew_runtime_cleanup`.
//!
//! # Ownership contract (`ffi-ownership-contracts`)
//!
//! The handle is `#[repr(C)]` and opaque to the host (`*mut HewRuntime`). It
//! carries an atomic strong-reference count and an atomic `shut` flag. The
//! handle is a **process-static singleton** (`DEFAULT_HANDLE`): it is never
//! heap-allocated and never freed, so its pointer is stable for the life of the
//! process. This makes the documented guarantees true on the *real* ABI:
//!
//! - **Per-runtime refcount.** The count lives on the one static handle, so all
//!   `new`/`retain` owners share it; teardown runs exactly once, on the release
//!   that drops it to zero — never out from under a second owner.
//! - **Over-release is a safe no-op (tombstone, not double-free).** Because the
//!   handle is never freed, a `release` past the last reference dereferences
//!   valid (tombstoned) static storage, observes the count already at zero, and
//!   returns without touching anything. There is no freed box to dereference,
//!   so there is no use-after-free and no double-free — `ASan`-clean. Teardown
//!   stays run-once via the `shut` flag. This is the "leak-on-close" the doc
//!   promised, made real: the one tombstoned handle is process-static (not even
//!   a heap leak), matching the single-runtime singleton.
//!
//! # Concurrency at the 0↔1 boundary (the revival/teardown serialization)
//!
//! M1 documents host-thread concurrency: multiple host threads may call
//! `new`/`retain`/`release`/`shutdown` on the singleton at once, even without
//! unbalanced retain/release. The refcount alone is **not** enough to make the
//! lifecycle race-safe, because the count dropping to zero and the teardown it
//! triggers are not one atomic step: `run_runtime_teardown` ends by
//! `take_default`-ing and dropping the installed runtime, and until that final
//! drop the runtime is *still installed*. A `new` that observed count 0 in that
//! window would clear the tombstone, find `hew_sched_init` a CAS no-op (the old
//! runtime is still in the slot), and be handed a runtime another thread is in
//! the middle of tearing down — and a `release` that drove the count back to 0
//! in that window could run a *second* concurrent teardown.
//!
//! The fix serializes every init/teardown transition under [`LIFECYCLE`] so a
//! teardown and an init can never interleave:
//!
//! - **Every `new`** runs its owner-take + `shut`-clear + `hew_sched_init`
//!   (re-)install under `LIFECYCLE`. The `shut`-clear is tied to the install,
//!   not the owner-count transition: because `new` always reinstalls the
//!   runtime, it always clears the tombstone, so a `new` after *any* teardown —
//!   a count-0 last release *or* an explicit `hew_runtime_shutdown` that tore
//!   the runtime down while owners remained — revives it and the next
//!   last-release tears the re-inited runtime down once. `new` is a host
//!   *lifecycle* call, invoked a handful of times per process by an embedder
//!   (not a per-message hot path), so the lock is free in practice; in return it
//!   guarantees no `new` installs a runtime concurrently with a teardown
//!   detaching one, and no two `new`s race the install.
//! - **The last release** (the thread that drove the count 1→0) runs the whole
//!   teardown — through the final `take_default`/drop — while holding
//!   `LIFECYCLE`. Because the 1→0 decrement and the teardown decision are not
//!   one atomic step, the teardown **re-reads the count under the lock** and
//!   aborts if a reviver reappeared (count back to non-zero): tearing down then
//!   would pull the runtime out from under that new owner. If still zero, it is
//!   the genuine last release and tears down.
//!
//! Whichever of a reviving `new` and a last-release teardown takes `LIFECYCLE`
//! first runs to completion before the other observes the slot. If teardown
//! wins, it empties the slot (`take_default`) before the reviver's
//! `hew_sched_init` installs a *fresh* runtime — never aliasing the torn-down
//! one; the reviving `new` also clears `shut` (every `new` does, in lock-step
//! with its install), so its set's last release tears the re-inited runtime
//! down once. If the reviver wins, the runtime stays live
//! (count non-zero) and the teardown's under-lock re-read aborts. A second
//! concurrent teardown cannot run: a boundary-crossing `release` would have to
//! observe its own 1→0 CAS, but the count is pinned at 0 until a `new` revives
//! it, so a redundant release short-circuits at `cur == 0`.
//!
//! [`hew_runtime_retain`] stays lock-free (a pure increment that never installs
//! or tears down). `LIFECYCLE` is acquired before any scheduler-internal lock on
//! every path (init and teardown alike), so the ordering is consistent and
//! deadlock-free, and neither init nor teardown re-enters the handle ABI.
//!
//! A leak detector running green is non-evidence that the runtime was released
//! (a thread-backed handle leak is invisible to it); the honest oracle is the
//! per-instance `RuntimeInner` drop probe under a known release sequence (see
//! `runtime.rs`).

#![cfg(not(target_arch = "wasm32"))]

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Mutex;

/// Opaque host handle to a Hew runtime. The host holds `*mut HewRuntime` and
/// never inspects the fields.
///
/// In this milestone there is exactly one runtime, so there is exactly one of
/// these — the process-static [`DEFAULT_HANDLE`]. It is never heap-allocated
/// and never freed; its fields are the *per-runtime* owner state shared by all
/// host owners of that single runtime.
#[repr(C)]
#[derive(Debug)]
pub struct HewRuntime {
    /// Per-runtime strong-reference (owner) count, shared by every `new`/
    /// `retain` owner of this runtime. Starts at zero; each `hew_runtime_new`
    /// and `hew_runtime_retain` increments it, each `hew_runtime_release`
    /// decrements it. The runtime is torn down on the release that drops it to
    /// zero — never while another owner still holds it.
    strong: AtomicUsize,
    /// Set once the runtime has been torn down (by `shutdown` or the last
    /// `release`). Makes teardown idempotent: a later `shutdown`, or the last
    /// `release` after an explicit `shutdown`, observes this and does not run
    /// the teardown twice. Reset to `false` by [`hew_runtime_new`] whenever it
    /// (re-)installs the runtime, so the flag tracks the runtime INSTALL state:
    /// `new` always reinstalls via `hew_sched_init`, so a `new` after any
    /// teardown — whether the count had reached zero or an explicit
    /// `hew_runtime_shutdown` tore the runtime down while owners remained —
    /// revives the runtime and the next last-release tears the re-inited runtime
    /// down once more.
    shut: AtomicBool,
}

impl HewRuntime {
    /// The const-constructible zero-owner state for the static singleton.
    const fn new_singleton() -> HewRuntime {
        HewRuntime {
            strong: AtomicUsize::new(0),
            shut: AtomicBool::new(false),
        }
    }
}

/// The process-singleton runtime handle for the single default runtime.
///
/// Every [`hew_runtime_new`] hands the host this same pointer (bumping the
/// shared owner count), so the refcount is per-runtime rather than per-call.
/// Being static, it is never freed: the last `release` tombstones it (count
/// zero, `shut` set) instead of `Box::from_raw`-ing it, which is what makes an
/// over-release a safe no-op rather than a use-after-free.
static DEFAULT_HANDLE: HewRuntime = HewRuntime::new_singleton();

/// Serializes the singleton's runtime init and teardown so the two can never
/// interleave.
///
/// Held by **every `hew_runtime_new`** across its owner-take + `shut`-clear +
/// `hew_sched_init` (re-)install, by the **last
/// release** across its whole teardown (`teardown_once` → `take_default`/drop),
/// and by an explicit `hew_runtime_shutdown` across its teardown. Because every
/// install and every teardown take this one lock, a `new` can neither install a
/// runtime concurrently with a teardown detaching one nor race another `new`'s
/// install, and a teardown re-reads the owner count under the lock so a reviver
/// that reappeared aborts the teardown. [`hew_runtime_retain`] is exempt: it is
/// a pure increment that never installs or tears down.
///
/// Always acquired *before* any scheduler-internal lock (init and teardown
/// alike), so the ordering is consistent and deadlock-free; neither init nor
/// teardown re-enters the handle ABI under the lock.
///
/// A poisoned lock (a panic inside a teardown or re-init while held) is
/// recovered with `into_inner`: the lifecycle must keep serializing across a
/// panic rather than wedge every later `new`/`release`, matching how the
/// runtime's other process-lifecycle locks treat poison.
static LIFECYCLE: Mutex<()> = Mutex::new(());

/// Acquire [`LIFECYCLE`], recovering from poison. A panic in a prior critical
/// section must not wedge the lifecycle for the rest of the process.
fn lifecycle_lock() -> std::sync::MutexGuard<'static, ()> {
    LIFECYCLE
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}

/// The stable `*mut HewRuntime` the host sees. Casting `&'static` to `*mut` is
/// sound here: the host never writes through the pointer (the fields it cares
/// about are the interior atomics, which are mutated through shared `&` via
/// atomic ops), and the static outlives every handle.
fn default_handle_ptr() -> *mut HewRuntime {
    std::ptr::addr_of!(DEFAULT_HANDLE).cast_mut()
}

/// Run the runtime teardown exactly once for this handle.
///
/// Idempotent: the first caller to flip `shut` false→true runs the
/// `hew_sched_shutdown` (join workers) + `hew_runtime_cleanup` (reactor/ticker
/// stop, supervisor/actor/registry sweep, detach-and-drop the runtime) sequence
/// — the same order the no-arg lifecycle path uses, never inverted. Every later
/// caller observes `shut` already set and returns without re-tearing-down.
///
/// # Safety
///
/// `handle` must be a valid `*mut HewRuntime` returned by [`hew_runtime_new`].
/// The handle is the process-static singleton and is never freed, so it stays
/// valid to pass after a prior teardown (where this is a no-op).
///
/// Callers that drive a 0↔1 boundary transition (the last release, an explicit
/// `shutdown`, a reviving `new`) must hold [`LIFECYCLE`] so the teardown body —
/// which ends by `take_default`-ing and dropping the installed runtime — cannot
/// run concurrently with a revival re-installing it.
unsafe fn teardown_once(handle: *mut HewRuntime) {
    // SAFETY: caller guarantees `handle` is a live handle.
    let h = unsafe { &*handle };
    if h.shut.swap(true, Ordering::AcqRel) {
        // Another caller already tore the runtime down.
        return;
    }
    run_runtime_teardown();
}

/// Run the runtime teardown sequence: join workers first, then the
/// reactor/ticker/supervisor/actor/registry sweep and the detach-and-drop of
/// the `RuntimeInner` — the same order the no-arg lifecycle path uses, never
/// inverted.
///
/// Split out from [`teardown_once`] so the handle's refcount / leak-on-close /
/// idempotence contracts can be exercised in unit tests without driving the
/// real global teardown against the shared default runtime the test harness
/// installs (which would race other serialized tests). The `#[cfg(test)]`
/// variant records that teardown was reached and returns; the production
/// variant runs the real sequence.
#[cfg(not(test))]
fn run_runtime_teardown() {
    crate::scheduler::hew_sched_shutdown();
    crate::scheduler::hew_runtime_cleanup();
}

/// Test-only teardown: record the invocation instead of tearing down the
/// shared default runtime the test guard owns. The real ordering is covered by
/// the existing `hew_runtime_cleanup` lifecycle tests; here we only need to
/// prove the handle drives teardown exactly once — and, for the concurrency
/// tests, that the [`LIFECYCLE`] lock makes the teardown body mutually exclusive
/// with a concurrent revive/teardown.
///
/// The `IN_TEARDOWN` gauge detects a second teardown body running concurrently:
/// it is incremented on entry and the entry asserts the previous value was zero,
/// then decremented on exit. With the lock held across the real teardown the
/// gauge never exceeds one; without it, an overlapping teardown trips the assert.
///
/// `teardown_hook` lets a test hold a teardown body open at a chosen point so it
/// can deterministically drive a concurrent revive+release while this body is
/// still in flight (see `teardown_lock_blocks_concurrent_revive_teardown`),
/// rather than relying on a probabilistic spin window.
#[cfg(test)]
fn run_runtime_teardown() {
    let overlap = tests::IN_TEARDOWN.fetch_add(1, Ordering::SeqCst);
    assert_eq!(
        overlap, 0,
        "two teardown bodies ran concurrently — the 0↔1 boundary lock failed"
    );
    tests::run_teardown_hook();
    tests::TEARDOWN_INVOCATIONS.fetch_add(1, Ordering::SeqCst);
    tests::IN_TEARDOWN.fetch_sub(1, Ordering::SeqCst);
}

/// Initialise the Hew runtime and return an owning host handle.
///
/// Delegates to `hew_sched_init` (idempotent: a second init against an
/// already-installed default runtime is a harmless no-op), then returns the
/// process-singleton handle, adding one strong (owner) reference to the shared
/// per-runtime count. Two `new` calls are two owners of the *same* runtime
/// (count goes 0→1→2), not two independent runtimes — so the runtime is not
/// torn down until the last of them releases.
///
/// Never returns null: the handle is a process-static singleton, so there is no
/// allocation that can fail.
///
/// If the runtime was previously torn down — either the count reached zero, or
/// an explicit [`hew_runtime_shutdown`] tore it down while owners still held it
/// — calling `new` again revives it: this clears the tombstone `shut` flag in
/// lock-step with bringing the runtime back up via `hew_sched_init`, so the next
/// last-release of the owning set tears the re-inited runtime down exactly once.
/// The clear is tied to the (re-)install, not the owner-count transition, so it
/// fires whether `new` increments 0→1 or N→N+1 over a tombstoned runtime.
///
/// The returned handle must be released with [`hew_runtime_release`] (balanced
/// against this `new` and any [`hew_runtime_retain`]).
#[no_mangle]
pub extern "C" fn hew_runtime_new() -> *mut HewRuntime {
    // SAFETY: the static is always live; `addr_of!` yields a valid pointer.
    let h = &DEFAULT_HANDLE;
    // The whole owner-take + (re-)init runs under LIFECYCLE so it serializes
    // against a last-release teardown and against any other `new`. `new` is a
    // host *lifecycle* call (invoked a handful of times per process by an
    // embedder), not a per-message hot path, so a lock here is free in practice
    // — and it is what makes init fully race-safe:
    //
    // - A reviver (the owner whose increment is 0→1) and a last-release teardown
    //   cannot interleave: whichever takes the lock first runs to completion
    //   before the other sees the slot. If teardown wins, it empties the slot
    //   (`take_default`) before the reviver's `hew_sched_init` installs a fresh
    //   runtime; if the reviver wins, it keeps the runtime live (count back to
    //   non-zero) and the last-release re-reads the count under the lock and
    //   aborts its teardown (see `hew_runtime_release`).
    // - A non-reviver (increment from a non-zero count) does NOT independently
    //   install: holding the lock, it observes the runtime the reviver already
    //   brought up. This closes the window where a lock-free `hew_sched_init`
    //   could install a second worker-backed runtime concurrently with a
    //   teardown detaching the first.
    let _g = lifecycle_lock();
    h.strong.fetch_add(1, Ordering::AcqRel);
    // Tie the tombstone to the runtime INSTALL state, not the owner-count
    // transition: `new` always (re-)installs the runtime via `hew_sched_init`
    // below, so it always clears `shut`. Gating the clear on the 0→1 transition
    // alone was wrong — an explicit `hew_runtime_shutdown` tears the runtime
    // down while the count is still nonzero, so a following `new` increments
    // N→N+1 (not 0→1), reinstalls the runtime, yet would leave `shut` set; the
    // final release's `teardown_once` would then short-circuit on the stale
    // tombstone and leak the reinstalled runtime. Clearing unconditionally under
    // the lock keeps the tombstone in lock-step with the install: every `new`
    // that (re-)installs a torn-down runtime also revives it. (When `shut` was
    // already false this is a harmless no-op store; when it was true — a revive
    // over either a count-0 teardown or an explicit shutdown — it is the revive.)
    // It is sound under `LIFECYCLE`: a concurrent last-release teardown either
    // ran fully before this `new` took the lock (so we revive after it) or runs
    // after and re-reads the now-nonzero count and aborts.
    h.shut.store(false, Ordering::Release);
    // Bring the process default runtime up if it is not already (idempotent CAS:
    // a no-op when an owner already installed it). Under the lock this never
    // races a teardown's detach or another `new`'s install.
    let _ = crate::scheduler::hew_sched_init();
    default_handle_ptr()
}

/// Add a strong reference to a runtime handle.
///
/// Every `retain` must be balanced by a `release`. A null handle is a no-op
/// (a defensive boundary, not a normal call).
///
/// # Safety
///
/// `handle`, if non-null, must be a valid `*mut HewRuntime` from
/// [`hew_runtime_new`]. The handle is the process-static singleton and is never
/// freed, so it stays valid to pass after a prior teardown.
#[no_mangle]
pub unsafe extern "C" fn hew_runtime_retain(handle: *mut HewRuntime) {
    if handle.is_null() {
        return;
    }
    // SAFETY: caller guarantees `handle` is live.
    let h = unsafe { &*handle };
    // Relaxed is sufficient for an increment: the caller already holds a
    // reference that happens-before this retain.
    h.strong.fetch_add(1, Ordering::Relaxed);
}

/// Drop a strong reference to a runtime handle.
///
/// When the last strong reference is dropped (count →0), the runtime is torn
/// down (workers joined, reactor/ticker stopped, actors/registry swept, the
/// `RuntimeInner` detached and dropped). The handle is a process-static
/// singleton, so it is **never freed**: the last release tombstones it (count
/// zero, `shut` set) in place. A null handle is a no-op.
///
/// Over-release is a safe no-op (tombstone, not double-free): releasing more
/// times than retained past the last reference dereferences valid (tombstoned)
/// static storage, observes the count already at zero, and returns without
/// touching anything — no freed memory is dereferenced, so it is neither a
/// use-after-free nor a double-free. Teardown runs exactly once via
/// [`teardown_once`]; an over-release never re-runs it.
///
/// # Safety
///
/// `handle`, if non-null, must be a valid `*mut HewRuntime` from
/// [`hew_runtime_new`]. The handle is never freed, so it remains valid to pass
/// after a last release (where it is a no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_runtime_release(handle: *mut HewRuntime) {
    if handle.is_null() {
        return;
    }
    // SAFETY: caller guarantees `handle` is one of our handles. It is the
    // process-static singleton, which is always live — passing it after the last
    // release is sound (it is never freed) and resolves to a no-op below.
    let h = unsafe { &*handle };
    // Decrement with a saturating-at-zero CAS loop rather than a bare
    // `fetch_sub`: a `fetch_sub` on a zero count wraps to `usize::MAX` and would
    // make a subsequent release appear to have references left. The loop never
    // lets the count go below zero, so an over-release is observably a no-op,
    // not a wrap.
    let mut cur = h.strong.load(Ordering::Acquire);
    loop {
        if cur == 0 {
            // Released past zero: a host contract violation (more releases than
            // retains). The handle is static (never freed), so this is a safe
            // no-op on a tombstoned handle — not a double-free.
            eprintln!(
                "hew-runtime: hew_runtime_release called with no outstanding reference; \
                 ignoring (over-release no-op on the tombstoned singleton handle)"
            );
            return;
        }
        match h
            .strong
            .compare_exchange_weak(cur, cur - 1, Ordering::AcqRel, Ordering::Acquire)
        {
            Ok(_) => break,
            Err(observed) => cur = observed,
        }
    }
    if cur != 1 {
        // Other owners remain (we decremented from `cur` > 1).
        return;
    }
    // TEST HOOK: between the lock-free 1→0 CAS above and taking LIFECYCLE below
    // is the exact window a reviving `new` can slip into (grab LIFECYCLE first,
    // bump the count back to 1). A test installs a hook here to force that
    // interleaving deterministically and prove the under-lock re-read aborts.
    #[cfg(test)]
    tests::run_pre_teardown_lock_hook();
    // Last owner dropped (we drove the count 1→0): tear the runtime down under
    // LIFECYCLE. Holding the lock across the whole teardown — through
    // `run_runtime_teardown`'s final `take_default`/drop — keeps the count-0/
    // torn-down state from being observable to a reviving `new` until the slot
    // is empty, and keeps a concurrent boundary-crossing `release` from running
    // a second teardown (it would have to observe its own 1→0 CAS, but the count
    // is pinned at 0 until a `new` revives it, so it short-circuits at `cur == 0`
    // above). The handle is the process-static singleton — NOT freed, only
    // tombstoned (count is now zero; `teardown_once` sets `shut`).
    let _g = lifecycle_lock();
    // Re-validate under the lock: a reviving `new` may have raced our 1→0 CAS
    // and grabbed LIFECYCLE first (count 0→1, runtime kept live). The 1→0 CAS
    // and this teardown decision are not one atomic step, so we must re-read the
    // count while holding the lock. If an owner reappeared, the runtime is live
    // again and tearing it down would pull it out from under that owner — abort.
    // If still zero, we are the genuine last release; tear down. Either way the
    // outcome is decided atomically with respect to revival because both this
    // check and the reviver's re-init run under LIFECYCLE.
    if h.strong.load(Ordering::Acquire) != 0 {
        #[cfg(test)]
        tests::ABORTED_TEARDOWNS.fetch_add(1, Ordering::SeqCst);
        return;
    }
    // SAFETY: `handle` is the live static singleton; under the lock the count is
    // still zero, so we are the unique last-release caller with no live owner.
    unsafe { teardown_once(handle) };
}

/// Explicitly tear down the runtime owned by `handle` without freeing the
/// handle.
///
/// Joins the workers and runs the `hew_runtime_cleanup` sequence, in the same
/// order the no-arg lifecycle path uses. Idempotent: a second `shutdown`, or a
/// later `release`, observes the teardown already ran and does not repeat it.
///
/// The handle is a **process-static singleton and is never freed** — so a host
/// may `shutdown` for deterministic teardown timing and still call the balancing
/// [`hew_runtime_release`] afterwards; that release drops the owner count and,
/// finding the teardown already ran, only tombstones the (still-valid) static
/// handle rather than re-tearing-down or freeing anything. A null handle is a
/// no-op.
///
/// Runs the teardown under the same [`LIFECYCLE`] lock the last-release path
/// uses, so an explicit `shutdown` cannot race a concurrent reviving `new`'s
/// `hew_sched_init` re-install — the two are serialized at the 0↔1 boundary.
///
/// # Safety
///
/// `handle`, if non-null, must be a valid `*mut HewRuntime` from
/// [`hew_runtime_new`]. The handle is never freed, so it stays valid to pass
/// after teardown (where the teardown half is a no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_runtime_shutdown(handle: *mut HewRuntime) {
    if handle.is_null() {
        return;
    }
    // Serialize against a reviving `new`/last-release teardown — see the
    // `LIFECYCLE` doc. The lock makes the explicit teardown mutually exclusive
    // with the 0↔1 re-init/teardown transitions.
    let _g = lifecycle_lock();
    // SAFETY: caller guarantees `handle` is live.
    unsafe { teardown_once(handle) };
}

// ── Tier-B: explicit-runtime resolution seam (split-by-danger, safe half) ──
//
// These forms take an explicit runtime handle and resolve an id / name /
// capability against THAT runtime, instead of reading the ambient
// `CURRENT_RUNTIME`. This is the safe-by-construction half of the master plan's
// split-by-danger: resolution by id/name returns a value the caller then routes
// (and a cross-runtime route is refused by the held-pointer send check), so the
// explicit-`rt` form never dereferences a foreign actor. The dangerous half —
// held-actor-pointer send/ask/by-id — stays ambient and is guarded by the
// `runtime_id` mismatch check in `actor.rs`, never the reverse.
//
// In this milestone the handle owns the single default runtime, so each
// `_with_runtime` form resolves the default runtime's state; the legacy bare
// forms (`hew_registry_lookup`, …) keep resolving the ambient runtime
// unchanged. The seam exists so the multi-runtime milestone can route these to
// a named non-default runtime without a new ABI.

/// Read the runtime id of the runtime owned by `handle`.
///
/// The id is the plain discriminant a host compares to decide whether two
/// pieces of work belong to the same runtime (it pairs with the held-pointer
/// cross-runtime send check, which refuses a mismatch). Returns
/// [`crate::runtime_id::RuntimeId::DEFAULT`]'s value for a null handle, matching
/// the single-runtime default. In this milestone the handle owns the default
/// runtime, so this is always the default id.
///
/// # Safety
///
/// `handle`, if non-null, must be a valid `*mut HewRuntime` from
/// [`hew_runtime_new`]. The handle is the process-static singleton and is never
/// freed, so it stays valid to pass after a prior teardown.
#[no_mangle]
pub unsafe extern "C" fn hew_runtime_id_of(handle: *mut HewRuntime) -> u64 {
    if handle.is_null() {
        return crate::runtime_id::RuntimeId::DEFAULT.as_u64();
    }
    // The handle owns the default runtime in this milestone; resolve its id from
    // the installed runtime (falling back to DEFAULT before init). A later
    // milestone gives handles distinct ids carried on the handle itself.
    crate::runtime::rt_default()
        .map_or(
            crate::runtime_id::RuntimeId::DEFAULT,
            crate::runtime::RuntimeInner::runtime_id,
        )
        .as_u64()
}

/// Look up an actor by name in the registry of the runtime owned by `handle`
/// (the explicit-runtime form of `hew_registry_lookup`).
///
/// Resolves the name against the handle's runtime rather than the ambient
/// `CURRENT_RUNTIME`. In this milestone the handle owns the default runtime, so
/// this resolves the default registry — identical to the bare
/// `hew_registry_lookup`. Returns the registered actor pointer, or null when the
/// name is unregistered, the handle is null, or no runtime is installed.
///
/// The returned pointer is a resolution result the caller then routes; routing
/// it cross-runtime is refused by the held-pointer send check, so this form
/// never dereferences a foreign actor itself.
///
/// # Safety
///
/// - `handle`, if non-null, must be a valid `*mut HewRuntime` from
///   [`hew_runtime_new`]. The handle is the process-static singleton and is
///   never freed, so it stays valid to pass after a prior teardown.
/// - `name`, if non-null, must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_registry_lookup_with_runtime(
    handle: *mut HewRuntime,
    name: *const std::ffi::c_char,
) -> *mut std::ffi::c_void {
    if handle.is_null() {
        return std::ptr::null_mut();
    }
    // The handle owns the default runtime in this milestone, which the bare
    // `hew_registry_lookup` already resolves (ambient TLS first, then the
    // default-slot fallback). Delegate to it so the explicit-runtime form is
    // behaviour-identical single-runtime and shares the one resolution path; a
    // later milestone routes to the handle's own runtime's registry.
    // SAFETY: `name` upholds `hew_registry_lookup`'s C-string contract.
    unsafe { crate::registry::hew_registry_lookup(name) }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Count of `run_runtime_teardown` invocations (the test build records here
    /// instead of tearing the shared default runtime down — see
    /// `run_runtime_teardown`). Lets a test assert teardown ran exactly once
    /// without driving the real global cleanup.
    pub(super) static TEARDOWN_INVOCATIONS: AtomicUsize = AtomicUsize::new(0);

    /// Number of teardown bodies currently executing. The `cfg(test)`
    /// `run_runtime_teardown` increments this on entry (asserting it was zero)
    /// and decrements on exit, so a second teardown body running concurrently —
    /// the unserialized-revival bug this commit fixes — trips the entry assert.
    /// With the [`LIFECYCLE`] lock held across each teardown it never exceeds
    /// one. Process-global rather than per-test because teardown runs off worker
    /// threads in the concurrency test.
    pub(super) static IN_TEARDOWN: AtomicUsize = AtomicUsize::new(0);

    /// Count of last-release teardowns that aborted under the lock because a
    /// reviving `new` reappeared (the count re-read found a live owner). The
    /// stress test records this to confirm the abort branch — the residual race
    /// the under-lock re-read closes — is actually exercised, not dead code.
    pub(super) static ABORTED_TEARDOWNS: AtomicUsize = AtomicUsize::new(0);

    use std::sync::{Arc, Condvar};

    /// A one-shot rendezvous a test installs to hold a code path open at a
    /// chosen point: the hooked path flips `entered` true (notifying the test)
    /// then blocks until the test flips `release`. Lets a test force a precise
    /// interleaving deterministically rather than relying on timing.
    struct Gate {
        entered: Mutex<bool>,
        entered_cv: Condvar,
        release: Mutex<bool>,
        release_cv: Condvar,
    }

    impl Gate {
        fn new() -> Arc<Gate> {
            Arc::new(Gate {
                entered: Mutex::new(false),
                entered_cv: Condvar::new(),
                release: Mutex::new(false),
                release_cv: Condvar::new(),
            })
        }

        /// Called from inside a hooked path: signal entry, then block until the
        /// driving test opens the gate.
        fn enter_and_block(&self) {
            {
                let mut entered = self
                    .entered
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                *entered = true;
                self.entered_cv.notify_all();
            }
            let mut released = self
                .release
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            while !*released {
                released = self
                    .release_cv
                    .wait(released)
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
            }
        }

        /// Block (on the test thread) until the hooked path has entered.
        fn wait_entered(&self) {
            let mut entered = self
                .entered
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            while !*entered {
                entered = self
                    .entered_cv
                    .wait(entered)
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
            }
        }

        /// Open the gate so the blocked hooked path resumes.
        fn open(&self) {
            let mut release = self
                .release
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            *release = true;
            self.release_cv.notify_all();
        }
    }

    /// Gate installed inside the `cfg(test)` teardown body (`run_teardown_hook`)
    /// to hold a teardown in flight. `None` for ordinary tests.
    static TEARDOWN_GATE: Mutex<Option<Arc<Gate>>> = Mutex::new(None);

    /// Gate installed at the pre-LIFECYCLE point in `hew_runtime_release`
    /// (`run_pre_teardown_lock_hook`) to pause a last-release after its 1→0 CAS
    /// but before it takes the lock. `None` for ordinary tests.
    static PRE_TEARDOWN_LOCK_GATE: Mutex<Option<Arc<Gate>>> = Mutex::new(None);

    fn installed_gate(slot: &'static Mutex<Option<Arc<Gate>>>) -> Option<Arc<Gate>> {
        slot.lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
    }

    /// Called from inside the `cfg(test)` teardown body. If a gate is installed,
    /// signal that the teardown body has entered, then block until released.
    pub(super) fn run_teardown_hook() {
        if let Some(gate) = installed_gate(&TEARDOWN_GATE) {
            gate.enter_and_block();
        }
    }

    /// Called from `hew_runtime_release` between the lock-free 1→0 CAS and taking
    /// LIFECYCLE. If a gate is installed, pause here so a test can deterministi-
    /// cally let a reviving `new` reappear before the teardown re-reads the count.
    pub(super) fn run_pre_teardown_lock_hook() {
        if let Some(gate) = installed_gate(&PRE_TEARDOWN_LOCK_GATE) {
            gate.enter_and_block();
        }
    }

    /// Serializes these handle tests. Every test drives the *one* process-static
    /// [`DEFAULT_HANDLE`] (the real ABI's singleton), so they share its owner
    /// count, its `shut` flag, and the `TEARDOWN_INVOCATIONS` recorder; the lock
    /// keeps one test from observing another's transitions.
    static HANDLE_TEST_LOCK: Mutex<()> = Mutex::new(());

    /// Reset the singleton handle and the teardown recorder to a clean baseline
    /// (no owners, not torn down) for a test. Caller must hold
    /// [`HANDLE_TEST_LOCK`].
    fn reset_singleton() {
        DEFAULT_HANDLE.strong.store(0, Ordering::SeqCst);
        DEFAULT_HANDLE.shut.store(false, Ordering::SeqCst);
        TEARDOWN_INVOCATIONS.store(0, Ordering::SeqCst);
        IN_TEARDOWN.store(0, Ordering::SeqCst);
        ABORTED_TEARDOWNS.store(0, Ordering::SeqCst);
        *TEARDOWN_GATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = None;
        *PRE_TEARDOWN_LOCK_GATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = None;
    }

    /// Read the singleton's current owner count.
    fn owner_count() -> usize {
        DEFAULT_HANDLE.strong.load(Ordering::Acquire)
    }

    /// A retain/release pair brackets a single owner against the real singleton
    /// handle: the first owner is count 1, `retain` takes it to 2, `release`
    /// back to 1, and the final `release` drops the last owner — which runs
    /// teardown exactly once. We drive the real `retain`/`release` ABI on the
    /// real handle pointer (no synthetic box) and assert the per-runtime count
    /// transitions plus a single teardown on the last release, not before.
    #[test]
    fn retain_release_runs_teardown_once_on_last_release() {
        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_singleton();

        // Start one owner directly on the singleton (the count the first `new`
        // would have taken) — this test exercises the refcount ABI without
        // spawning the real scheduler, so it does not go through `new`.
        DEFAULT_HANDLE.strong.store(1, Ordering::SeqCst);
        let handle = default_handle_ptr();
        assert_eq!(owner_count(), 1, "one owner");

        // SAFETY: `handle` is the live process-static singleton.
        unsafe { hew_runtime_retain(handle) };
        assert_eq!(owner_count(), 2, "retain → 2");

        // SAFETY: `handle` is live; this release drops back to one owner and must
        // NOT tear down (an owner remains).
        unsafe { hew_runtime_release(handle) };
        assert_eq!(owner_count(), 1, "release → 1");
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            0,
            "teardown must not run while an owner remains"
        );

        // Final release drops the last owner: teardown runs once. The handle is
        // static, so nothing is freed.
        // SAFETY: `handle` is the live static singleton.
        unsafe { hew_runtime_release(handle) };
        assert_eq!(owner_count(), 0, "last release → 0 owners");
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "last release runs teardown exactly once"
        );
    }

    /// Multi-owner teardown safety (the per-runtime-refcount proving gate). Two
    /// real `hew_runtime_new()` calls are two owners of the *same* default
    /// runtime, so `release(h1)` must NOT tear it down while `h2` still owns it;
    /// only the second `release` (the last owner) runs teardown — exactly once.
    ///
    /// This test must FAIL on a per-handle-refcount design (where each `new`
    /// mints a fresh count-1 box and the first release tears the shared runtime
    /// down out from under the second owner). The `runtime_test_guard`
    /// pre-installs the default runtime slot, so `hew_sched_init` inside `new`
    /// is a CAS no-op (no worker threads spawned) and the `cfg(test)`
    /// `run_runtime_teardown` only records.
    #[test]
    fn two_new_owners_teardown_only_on_last_release() {
        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _guard = crate::runtime_test_guard();
        reset_singleton();

        // Two independent host owners of the one runtime.
        let h1 = hew_runtime_new();
        assert_eq!(owner_count(), 1, "first new → 1 owner");
        let h2 = hew_runtime_new();
        assert_eq!(owner_count(), 2, "second new → 2 owners (same runtime)");
        assert_eq!(h1, h2, "both new calls return the one singleton handle");

        // Releasing the first owner must NOT tear the shared runtime down — the
        // second owner still holds it. This is the bug the per-handle design hit.
        // SAFETY: `h1` is the live singleton.
        unsafe { hew_runtime_release(h1) };
        assert_eq!(owner_count(), 1, "release(h1) → 1 owner remains");
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            0,
            "runtime must NOT be torn down while a second owner holds it"
        );

        // Releasing the last owner tears it down exactly once.
        // SAFETY: `h2` is the live singleton.
        unsafe { hew_runtime_release(h2) };
        assert_eq!(owner_count(), 0, "release(h2) → 0 owners");
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "last owner's release runs teardown exactly once"
        );
    }

    /// Over-release is a safe no-op on the real handle, not a use-after-free.
    /// Because the handle is the process-static singleton (never freed), a
    /// release past the last owner dereferences valid (tombstoned) static
    /// storage, sees the count already zero, and returns — touching nothing. We
    /// drive the *real* release path (not a synthetic count-0 box that the real
    /// ABI never produces): the previous test's pattern would UAF on a freed box
    /// here, this one is `ASan`-clean.
    #[test]
    fn over_release_is_safe_noop_not_use_after_free() {
        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_singleton();

        // One owner, then the real last release: count → 0, teardown once,
        // handle tombstoned (NOT freed).
        DEFAULT_HANDLE.strong.store(1, Ordering::SeqCst);
        let handle = default_handle_ptr();
        // SAFETY: `handle` is the live static singleton.
        unsafe { hew_runtime_release(handle) };
        assert_eq!(owner_count(), 0, "last release → 0 owners (tombstoned)");
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "last release tears down once"
        );

        // Two further releases on the SAME (now tombstoned) real handle. On a
        // freed-box design these would be use-after-frees; on the static
        // singleton they dereference valid memory, observe count 0, and no-op.
        // The saturating CAS pins the count at zero — never wrapping to a huge
        // value that would make a later release think owners remain.
        // SAFETY: the singleton is never freed; passing it post-last-release is
        // sound and resolves to a no-op.
        unsafe { hew_runtime_release(handle) };
        assert_eq!(
            owner_count(),
            0,
            "over-release pins count at zero, not wrap"
        );
        // SAFETY: same — still the live tombstoned singleton.
        unsafe { hew_runtime_release(handle) };
        assert_eq!(owner_count(), 0, "a second over-release is still a no-op");
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "over-releases must not re-run teardown"
        );
    }

    /// `shutdown` is idempotent on the real handle: calling it twice runs
    /// teardown once. The `shut` flag flips on the first call and the second
    /// observes it. A later `release` after an explicit `shutdown` drops the
    /// owner without a second teardown (the handle is static — nothing freed).
    #[test]
    fn shutdown_is_idempotent() {
        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_singleton();

        // One owner against the real singleton.
        DEFAULT_HANDLE.strong.store(1, Ordering::SeqCst);
        let handle = default_handle_ptr();
        assert!(
            !DEFAULT_HANDLE.shut.load(Ordering::Acquire),
            "fresh singleton not shut"
        );

        // SAFETY: `handle` is the live static singleton.
        unsafe { hew_runtime_shutdown(handle) };
        assert!(
            DEFAULT_HANDLE.shut.load(Ordering::Acquire),
            "shutdown sets shut"
        );
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "first shutdown runs teardown once"
        );

        // SAFETY: `handle` is live; second shutdown is a no-op.
        unsafe { hew_runtime_shutdown(handle) };
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "second shutdown does not re-run teardown"
        );

        // Final release drops the owner; teardown already ran, so no second one,
        // and nothing is freed (the handle is static).
        // SAFETY: `handle` is the live static singleton.
        unsafe { hew_runtime_release(handle) };
        assert_eq!(owner_count(), 0, "release after shutdown → 0 owners");
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "release after shutdown does not re-run teardown"
        );
    }

    /// `new` after a torn-down owner set revives the runtime: it clears the
    /// tombstone `shut` flag (here the count had reached zero first, the 0→1
    /// revive case) so the next last-release tears the re-inited runtime down
    /// once more. Proves the singleton is re-usable across owner-set boundaries,
    /// not a one-shot. (The N→N+1 revive over an explicit `shutdown` — where the
    /// count never reached zero — is covered by
    /// `shutdown_then_new_revives_runtime_and_final_release_tears_down`.)
    #[test]
    fn new_after_teardown_revives_and_tears_down_again() {
        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _guard = crate::runtime_test_guard();
        reset_singleton();

        // First owner set: new → release → teardown once, tombstoned.
        let h1 = hew_runtime_new();
        // SAFETY: `h1` is the live singleton.
        unsafe { hew_runtime_release(h1) };
        assert_eq!(owner_count(), 0, "first set torn down");
        assert!(DEFAULT_HANDLE.shut.load(Ordering::Acquire), "tombstoned");
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "one teardown"
        );

        // Second owner set: `new` clears the tombstone (here a 0→1 revive) so
        // the next last-release can tear down again.
        let h2 = hew_runtime_new();
        assert_eq!(owner_count(), 1, "second set has one owner");
        assert!(
            !DEFAULT_HANDLE.shut.load(Ordering::Acquire),
            "new cleared the tombstone for the revived runtime"
        );
        // SAFETY: `h2` is the live singleton.
        unsafe { hew_runtime_release(h2) };
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            2,
            "second set's last release tears the re-inited runtime down again"
        );
    }

    /// `shutdown` then `new` revives the runtime over a NON-zero owner count,
    /// and the final release tears the reinstalled runtime down exactly once
    /// (never skipped). This is the shutdown-then-revive interleaving the
    /// install-tied tombstone fix closes:
    ///
    /// 1. `h1 = new` → one owner, runtime installed, `shut == false`.
    /// 2. `hew_runtime_shutdown(h1)` tears the runtime down while `h1` still
    ///    owns it: `shut == true`, count STILL 1 (shutdown does not drop the
    ///    owner).
    /// 3. `h2 = new` increments the count 1→2 — NOT a 0→1 transition — and
    ///    reinstalls the runtime via `hew_sched_init`. The fix clears `shut`
    ///    here because `new` (re-)installed the runtime; the old 0→1-gated clear
    ///    left `shut == true`.
    /// 4. `release(h1)` drops 2→1: an owner remains, no teardown.
    /// 5. `release(h2)` drops 1→0: the genuine last release. It must run
    ///    teardown on the reinstalled runtime — which it does only because step
    ///    3 cleared `shut`. With the old code `shut` was still set, so
    ///    `teardown_once` short-circuited and the reinstalled runtime leaked.
    ///
    /// Teeth: teardown runs exactly TWICE total (once for the `shutdown`, once
    /// for the final release of the revived runtime). On the pre-fix code the
    /// final teardown is SKIPPED, so the count is 1 — that is the leak this test
    /// catches. (The first `new` here is a 0→1 transition, so it does not depend
    /// on the fix; the revive at step 3 is the N→N+1 case that does.)
    #[test]
    fn shutdown_then_new_revives_runtime_and_final_release_tears_down() {
        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _guard = crate::runtime_test_guard();
        reset_singleton();

        // h1: one owner, runtime installed.
        let h1 = hew_runtime_new();
        assert_eq!(owner_count(), 1, "first new → 1 owner");
        assert!(
            !DEFAULT_HANDLE.shut.load(Ordering::Acquire),
            "fresh runtime not shut"
        );

        // Explicit deterministic shutdown WHILE h1 still owns the runtime: tears
        // it down (teardown #1) and tombstones it, but does NOT drop the owner —
        // the count stays at 1. This is the state the old 0→1-gated clear could
        // not recover from on the next `new`.
        // SAFETY: `h1` is the live process-static singleton.
        unsafe { hew_runtime_shutdown(h1) };
        assert_eq!(owner_count(), 1, "shutdown does not drop the owner");
        assert!(
            DEFAULT_HANDLE.shut.load(Ordering::Acquire),
            "shutdown tombstoned the runtime"
        );
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "shutdown ran teardown once"
        );

        // h2: revive over a NON-zero count (1→2). The install-tied clear must
        // clear `shut` here even though this is not a 0→1 transition, because
        // `new` reinstalled the runtime via `hew_sched_init`. On the pre-fix
        // code this assertion fails (shut still true) — proving it caught the bug.
        let h2 = hew_runtime_new();
        assert_eq!(owner_count(), 2, "second new → 2 owners (count 1→2)");
        assert!(
            !DEFAULT_HANDLE.shut.load(Ordering::Acquire),
            "new over a torn-down runtime cleared the tombstone (install-tied, \
             not 0→1-gated)"
        );

        // release(h1): 2→1, an owner remains, no teardown.
        // SAFETY: `h1` is the live process-static singleton.
        unsafe { hew_runtime_release(h1) };
        assert_eq!(owner_count(), 1, "release(h1) → 1 owner remains");
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "still only the shutdown teardown — an owner remains"
        );

        // release(h2): 1→0, the genuine last release. It MUST tear the
        // reinstalled runtime down — teardown #2. On the pre-fix code `shut` is
        // still set, so `teardown_once` short-circuits and this is SKIPPED,
        // leaking the live reinstalled runtime.
        // SAFETY: `h2` is the live process-static singleton.
        unsafe { hew_runtime_release(h2) };
        assert_eq!(owner_count(), 0, "release(h2) → 0 owners");
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            2,
            "the final release tears the REINSTALLED runtime down (not skipped \
             on the stale tombstone) — the leak the install-tied clear closes"
        );
    }

    /// The Tier-B id form reads the discriminant of the handle's runtime. In
    /// this milestone the handle owns the default runtime, so it reports the
    /// default id; a null handle also reports the default.
    #[test]
    fn runtime_id_of_reports_default() {
        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _guard = crate::runtime_test_guard();
        reset_singleton();

        let handle = hew_runtime_new();
        let default_id = crate::runtime_id::RuntimeId::DEFAULT.as_u64();
        // SAFETY: `handle` is the live singleton; the id form only reads the
        // discriminant.
        let id = unsafe { hew_runtime_id_of(handle) };
        assert_eq!(id, default_id, "the handle owns the default runtime");

        // A null handle reports the default id too.
        // SAFETY: null is an explicit, contracted input.
        let null_id = unsafe { hew_runtime_id_of(std::ptr::null_mut()) };
        assert_eq!(null_id, default_id, "null handle → default id");

        // Balance the owner this test took. Force the tombstone first so the
        // release does not drive the guard's runtime teardown (the `cfg(test)`
        // recorder is harmless, but this keeps the guard's slot untouched).
        DEFAULT_HANDLE.shut.store(true, Ordering::SeqCst);
        // SAFETY: `handle` is the live static singleton.
        unsafe { hew_runtime_release(handle) };
        assert_eq!(owner_count(), 0, "owner balanced");
    }

    /// The Tier-B name form resolves a registration against the handle's
    /// runtime. In this milestone it resolves the default registry, so a name
    /// registered through the bare `hew_registry_register` is found through the
    /// explicit-handle form, and the explicit form agrees with the bare lookup.
    #[test]
    fn registry_lookup_with_runtime_resolves_default_registry() {
        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _guard = crate::runtime_test_guard();
        reset_singleton();

        let handle = hew_runtime_new();
        let name = std::ffi::CString::new("tier_b_probe").unwrap();
        let sentinel = 0x1234_usize as *mut std::ffi::c_void;

        // SAFETY: `name` is a live C string; `sentinel` is an opaque pointer the
        // registry stores and returns verbatim (never dereferenced here).
        unsafe {
            assert_eq!(
                crate::registry::hew_registry_register(name.as_ptr(), sentinel),
                0,
                "registration of a fresh name succeeds"
            );

            // The explicit-handle form resolves the same registration.
            let via_handle = hew_registry_lookup_with_runtime(handle, name.as_ptr());
            assert_eq!(
                via_handle, sentinel,
                "explicit-runtime lookup finds the name"
            );

            // It agrees with the bare ambient lookup (single-runtime).
            let via_bare = crate::registry::hew_registry_lookup(name.as_ptr());
            assert_eq!(via_handle, via_bare, "explicit and bare lookups agree");

            // A null handle resolves nothing.
            let via_null = hew_registry_lookup_with_runtime(std::ptr::null_mut(), name.as_ptr());
            assert!(via_null.is_null(), "null handle resolves nothing");

            // Clean up the registration.
            assert_eq!(crate::registry::hew_registry_unregister(name.as_ptr()), 0);
        }

        // Balance the owner without driving the guard's runtime teardown.
        DEFAULT_HANDLE.shut.store(true, Ordering::SeqCst);
        // SAFETY: `handle` is the live static singleton.
        unsafe { hew_runtime_release(handle) };
        assert_eq!(owner_count(), 0, "owner balanced");
    }

    /// Concurrency proving gate for the 0↔1 boundary. Many host threads hammer
    /// balanced `new`/`release` pairs against the one singleton, driving the
    /// count across 1→0 (last release → teardown) and 0→1 (revive) repeatedly
    /// and concurrently. The serialization invariant is:
    ///
    /// - **No double-teardown.** Two teardown bodies never run at once — the
    ///   `IN_TEARDOWN` gauge (incremented under a widened window in the
    ///   `cfg(test)` teardown) asserts it stayed ≤ 1. Without the [`LIFECYCLE`]
    ///   lock a revive could clear `shut` mid-teardown and a following release
    ///   would start a second teardown; the gauge assert trips on that.
    /// - **No torn-down handle revived under an in-flight teardown.** A reviving
    ///   `new` clears `shut`/re-inits only under the same lock the teardown
    ///   holds, so it cannot observe the count-0 state until teardown finished.
    ///
    /// The `runtime_test_guard` keeps a worker-less runtime installed, so every
    /// `hew_sched_init` is a CAS no-op and the `cfg(test)` teardown only records
    /// (no real scheduler is spawned or torn down). What is exercised is purely
    /// the handle's count/lock state machine — which is exactly what races.
    ///
    /// Run this under TSan/ASan (`RUSTFLAGS="-Zsanitizer=thread"` /
    /// `-Zsanitizer=address`) to catch a data race on the count/flag in addition
    /// to the logical double-teardown the gauge already fails on.
    #[test]
    fn concurrent_new_release_never_double_teardown_or_revive_torn_down() {
        const THREADS: usize = 8;
        const ITERS: usize = 2_000;

        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _guard = crate::runtime_test_guard();
        reset_singleton();

        let start = std::sync::Arc::new(std::sync::Barrier::new(THREADS));
        let handles: Vec<_> = (0..THREADS)
            .map(|_| {
                let start = std::sync::Arc::clone(&start);
                std::thread::spawn(move || {
                    // Release the barrier so every thread piles onto the 0↔1
                    // boundary at once, maximising the chance of an overlap.
                    start.wait();
                    for _ in 0..ITERS {
                        let h = hew_runtime_new();
                        // SAFETY: `h` is the live process-static singleton.
                        unsafe { hew_runtime_release(h) };
                    }
                })
            })
            .collect();
        for t in handles {
            t.join().expect(
                "a worker thread panicked — a concurrent teardown overlap tripped the \
                 IN_TEARDOWN gauge assert",
            );
        }

        // Every owner was balanced: the count is back to zero and no teardown is
        // mid-flight.
        assert_eq!(owner_count(), 0, "all owners released → count 0");
        assert_eq!(
            IN_TEARDOWN.load(Ordering::SeqCst),
            0,
            "no teardown body left in flight"
        );
        // The boundary was actually crossed: balanced new/release pairs must have
        // driven the count to zero (and thus run teardown) many times, proving
        // the test exercised the revive/teardown transitions rather than a
        // perpetually-non-zero count that never tests the lock.
        assert!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst) > 0,
            "the 0↔1 boundary was crossed at least once (teardown ran)"
        );
    }

    /// Deterministic proof that the [`LIFECYCLE`] lock serializes a teardown
    /// against a concurrent revive+release — the exact double-teardown /
    /// revive-under-teardown race the independent review found. Unlike the
    /// probabilistic stress test, this forces the dangerous interleaving:
    ///
    /// 1. Main thread holds one owner (count 1) and installs a teardown gate.
    /// 2. Main thread `release`s the last owner → count 1→0, the last-release
    ///    takes `LIFECYCLE` and enters the teardown body, which blocks in the
    ///    gate (still holding `LIFECYCLE`).
    /// 3. A second thread, once it sees the teardown body has entered, calls
    ///    `new()` (would revive: 0→1, clear `shut`) then `release()` (would
    ///    drive 1→0 and run a SECOND teardown). With the lock, that `new`'s
    ///    revive blocks on `LIFECYCLE` until step 4; without it, it would clear
    ///    `shut` mid-teardown and the release would start an overlapping
    ///    teardown, tripping the `IN_TEARDOWN` gauge.
    /// 4. Main thread opens the gate; the first teardown completes and releases
    ///    `LIFECYCLE`; only THEN does the second thread's revive proceed and its
    ///    release run the second teardown — sequentially, never overlapping.
    ///
    /// The assertion with teeth is that the second thread joins cleanly (the
    /// gauge never tripped) and teardown ran exactly twice, in order.
    #[test]
    fn teardown_lock_blocks_concurrent_revive_teardown() {
        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _guard = crate::runtime_test_guard();
        reset_singleton();

        // Install the gate the teardown body will block in.
        let gate = Gate::new();
        *TEARDOWN_GATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(Arc::clone(&gate));

        // One owner whose release will drive the first (gated) teardown.
        DEFAULT_HANDLE.strong.store(1, Ordering::SeqCst);

        // Reviver thread: blocks until the first teardown body has entered, then
        // does new()+release(). Its new() must block on LIFECYCLE until the first
        // teardown finishes, so its teardown cannot overlap the first.
        let gate_for_reviver = Arc::clone(&gate);
        let reviver = std::thread::spawn(move || {
            gate_for_reviver.wait_entered();
            // The first teardown body is now in flight (and holds LIFECYCLE). A
            // revive here must wait for it; without the lock it would race.
            let h = hew_runtime_new();
            // SAFETY: `h` is the live process-static singleton.
            unsafe { hew_runtime_release(h) };
        });

        // Drive the first, gated teardown on this thread. The release enters the
        // teardown body (under LIFECYCLE), which blocks in the gate.
        let releaser = std::thread::spawn(|| {
            let h = default_handle_ptr();
            // SAFETY: `h` is the live process-static singleton.
            unsafe { hew_runtime_release(h) };
        });

        // Wait until the first teardown body has entered, then let the reviver
        // make its (blocked) attempt before opening the gate. The short sleep
        // gives the reviver time to reach (and block on) LIFECYCLE inside its
        // new(); if the lock were missing it would already have raced by now.
        gate.wait_entered();
        std::thread::sleep(std::time::Duration::from_millis(50));
        // Open the gate so the first teardown can finish and drop LIFECYCLE.
        gate.open();

        releaser.join().expect("releaser thread panicked");
        reviver
            .join()
            .expect("reviver thread panicked — a teardown overlap tripped IN_TEARDOWN");

        // Both teardowns ran, sequentially: first the gated last-release, then
        // the reviver's release after its revive completed under the lock.
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            2,
            "two teardowns ran in sequence, never overlapping"
        );
        assert_eq!(owner_count(), 0, "balanced: count back to zero");
        assert_eq!(
            IN_TEARDOWN.load(Ordering::SeqCst),
            0,
            "no teardown left in flight"
        );
    }

    /// Deterministic proof of the under-lock count re-read abort. The last
    /// release's lock-free 1→0 CAS and its teardown decision are not one atomic
    /// step; a reviving `new` can slip into the gap (grab LIFECYCLE first, drive
    /// the count back to 1) and keep the runtime live. The teardown must then
    /// abort rather than tear a now-owned runtime down. This forces that exact
    /// interleaving:
    ///
    /// 1. Main holds one owner and installs the pre-LIFECYCLE-lock gate.
    /// 2. A releaser `release`s the owner → 1→0 CAS, then pauses in the gate
    ///    (before taking LIFECYCLE).
    /// 3. A reviver `new()`s → takes LIFECYCLE, drives count 0→1, re-inits, and
    ///    returns a live handle (one owner now holds the runtime).
    /// 4. Main opens the gate; the releaser takes LIFECYCLE, re-reads the count
    ///    (now 1), and ABORTS its teardown — no teardown runs, the runtime stays
    ///    live under the reviver's owner.
    /// 5. The reviver balances its owner; that release is the genuine last one
    ///    and tears down exactly once.
    ///
    /// Teeth: `TEARDOWN_INVOCATIONS == 1` (only the reviver's final release tore
    /// down — the raced release aborted) and `ABORTED_TEARDOWNS == 1`. Without
    /// the re-read the raced release would tear the live runtime down, giving two
    /// teardowns and a runtime ripped out from under a live owner.
    #[test]
    fn last_release_aborts_teardown_when_new_revives_first() {
        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _guard = crate::runtime_test_guard();
        reset_singleton();

        let gate = Gate::new();
        *PRE_TEARDOWN_LOCK_GATE
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(Arc::clone(&gate));

        // One owner whose release will (try to) drive teardown but pause first.
        DEFAULT_HANDLE.strong.store(1, Ordering::SeqCst);

        // Releaser: last release drives 1→0, then blocks in the pre-lock gate.
        let releaser = std::thread::spawn(|| {
            let h = default_handle_ptr();
            // SAFETY: `h` is the live process-static singleton.
            unsafe { hew_runtime_release(h) };
        });

        // Wait until the releaser has done its 1→0 CAS and parked in the gate
        // (count is now 0), then revive on this thread: new() takes LIFECYCLE,
        // drives 0→1, re-inits — keeping the runtime live.
        gate.wait_entered();
        assert_eq!(owner_count(), 0, "releaser drove the count to zero");
        let revived = hew_runtime_new();
        assert_eq!(owner_count(), 1, "reviver brought an owner back");

        // Open the gate; the releaser now takes LIFECYCLE, re-reads count == 1,
        // and aborts its teardown.
        gate.open();
        releaser.join().expect("releaser thread panicked");

        assert_eq!(
            ABORTED_TEARDOWNS.load(Ordering::SeqCst),
            1,
            "the raced last release aborted its teardown under the lock"
        );
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            0,
            "no teardown ran: the runtime stayed live under the reviver"
        );

        // The reviver's own release is the genuine last one: it tears down once.
        // SAFETY: `revived` is the live process-static singleton.
        unsafe { hew_runtime_release(revived) };
        assert_eq!(owner_count(), 0, "reviver balanced");
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "the reviver's last release tears the live runtime down exactly once"
        );
    }
}
