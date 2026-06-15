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
//! A leak detector running green is non-evidence that the runtime was released
//! (a thread-backed handle leak is invisible to it); the honest oracle is the
//! `RUNTIME_INNER_DROPS` drop-count under a known release sequence (see
//! `runtime.rs`).

#![cfg(not(target_arch = "wasm32"))]

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

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
    /// the teardown twice. Reset to `false` by [`hew_runtime_new`] when it
    /// brings a previously-torn-down runtime back up (re-init), so the next
    /// last-release tears the re-inited runtime down once more.
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
/// `handle` must be a valid `*mut HewRuntime` returned by [`hew_runtime_new`]
/// and not yet freed.
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
/// prove the handle drives teardown exactly once.
#[cfg(test)]
fn run_runtime_teardown() {
    tests::TEARDOWN_INVOCATIONS.fetch_add(1, Ordering::SeqCst);
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
/// If a prior owner set tore the runtime down (count reached zero) and `new` is
/// called again, this clears the tombstone `shut` flag in lock-step with
/// bringing the runtime back up via `hew_sched_init`, so the next last-release
/// of this fresh owner set tears the re-inited runtime down exactly once.
///
/// The returned handle must be released with [`hew_runtime_release`] (balanced
/// against this `new` and any [`hew_runtime_retain`]).
#[no_mangle]
pub extern "C" fn hew_runtime_new() -> *mut HewRuntime {
    // SAFETY: the static is always live; `addr_of!` yields a valid pointer.
    let h = &DEFAULT_HANDLE;
    // Take this owner's reference first. If we are the owner that revives a
    // torn-down runtime (transition 0→1), clear the tombstone so the next
    // teardown can run, and only then (re-)install the default runtime. Ordering
    // matters: clearing `shut` before `hew_sched_init` means a concurrent
    // `shutdown`/`release` from a *previous* owner set cannot observe a half
    // state — by the time a new owner exists the count is already non-zero.
    let prev = h.strong.fetch_add(1, Ordering::AcqRel);
    if prev == 0 {
        // First owner of a fresh (or revived) runtime: clear any tombstone left
        // by the previous owner set's teardown so this set's last release tears
        // the re-inited runtime down once.
        h.shut.store(false, Ordering::Release);
    }
    // Bring the process default runtime up if it is not already (idempotent).
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
/// [`hew_runtime_new`] that has not been freed.
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
    // Last owner dropped: tear the runtime down (idempotent). The handle is the
    // process-static singleton — it is NOT freed, only tombstoned (count is now
    // zero; `teardown_once` sets `shut`). This is what makes a subsequent
    // over-release a safe no-op rather than a use-after-free.
    // SAFETY: `handle` is the live static singleton and this is the unique
    // last-release caller (the one that observed `cur == 1`).
    unsafe { teardown_once(handle) };
}

/// Explicitly tear down the runtime owned by `handle` without freeing the
/// handle.
///
/// Joins the workers and runs the `hew_runtime_cleanup` sequence, in the same
/// order the no-arg lifecycle path uses. Idempotent: a second `shutdown`, or a
/// later `release`, observes the teardown already ran and does not repeat it.
/// The handle box itself is freed by the final [`hew_runtime_release`], so a
/// host may `shutdown` for deterministic teardown timing and still `release` to
/// reclaim the handle. A null handle is a no-op.
///
/// # Safety
///
/// `handle`, if non-null, must be a valid `*mut HewRuntime` from
/// [`hew_runtime_new`] that has not been freed.
#[no_mangle]
pub unsafe extern "C" fn hew_runtime_shutdown(handle: *mut HewRuntime) {
    if handle.is_null() {
        return;
    }
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
/// [`hew_runtime_new`] that has not been freed.
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
///   [`hew_runtime_new`] that has not been freed.
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
    use std::sync::Mutex;

    /// Count of `run_runtime_teardown` invocations (the test build records here
    /// instead of tearing the shared default runtime down — see
    /// `run_runtime_teardown`). Lets a test assert teardown ran exactly once
    /// without driving the real global cleanup.
    pub(super) static TEARDOWN_INVOCATIONS: AtomicUsize = AtomicUsize::new(0);

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
    /// tombstone `shut` flag (0→1 owner transition) so the next last-release
    /// tears the re-inited runtime down once more. Proves the singleton is
    /// re-usable across owner-set boundaries, not a one-shot.
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

        // Second owner set: `new` clears the tombstone (0→1 owner) so the next
        // last-release can tear down again.
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
}
