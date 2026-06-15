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
//! # Lifecycle ABI
//!
//! - [`hew_runtime_new`] — initialise the runtime (delegating to
//!   `hew_sched_init`) and return a handle with one strong reference.
//! - [`hew_runtime_retain`] — add a strong reference.
//! - [`hew_runtime_release`] — drop a strong reference; the last release runs
//!   the runtime teardown ([`hew_runtime_shutdown`]'s sequence) and frees the
//!   handle box.
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
//! carries an atomic strong-reference count and an atomic `shut` flag. Teardown
//! is **leak-on-close**: a double `release` past the last reference, or a
//! `shutdown` after teardown, is a no-op rather than a double-free — the
//! `shut`/refcount atomics serialize the one teardown. A leak detector running
//! green is non-evidence that the runtime was released (a thread-backed handle
//! leak is invisible to it); the honest oracle is the `RUNTIME_INNER_DROPS`
//! drop-count under a known release sequence (see `runtime.rs`).

#![cfg(not(target_arch = "wasm32"))]

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

/// Opaque host handle to a Hew runtime. The host holds `*mut HewRuntime` and
/// never inspects the fields.
#[repr(C)]
#[derive(Debug)]
pub struct HewRuntime {
    /// Strong-reference count. The runtime is torn down when this reaches zero
    /// on a `release`. Starts at 1 from `hew_runtime_new`.
    strong: AtomicUsize,
    /// Set once the runtime has been torn down (by `shutdown` or the last
    /// `release`). Makes teardown idempotent: a later `shutdown`, or the last
    /// `release` after an explicit `shutdown`, observes this and does not run
    /// the teardown twice.
    shut: AtomicBool,
}

impl HewRuntime {
    fn new_boxed() -> *mut HewRuntime {
        Box::into_raw(Box::new(HewRuntime {
            strong: AtomicUsize::new(1),
            shut: AtomicBool::new(false),
        }))
    }
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
/// already-installed default runtime is a harmless no-op), then returns a fresh
/// handle holding one strong reference. Returns null only if the handle
/// allocation itself fails (the box allocation), which the host treats as
/// initialisation failure.
///
/// The returned handle must be released with [`hew_runtime_release`] (balanced
/// against this `new` and any [`hew_runtime_retain`]).
#[no_mangle]
pub extern "C" fn hew_runtime_new() -> *mut HewRuntime {
    // Bring the process default runtime up if it is not already (idempotent).
    let _ = crate::scheduler::hew_sched_init();
    HewRuntime::new_boxed()
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
/// When the last strong reference is dropped, the runtime is torn down (workers
/// joined, reactor/ticker stopped, actors/registry swept, the `RuntimeInner`
/// detached and dropped) and the handle box is freed. A null handle is a no-op.
///
/// Leak-on-close: releasing more times than retained past the last reference is
/// a no-op rather than a double-free — the refcount never goes below zero
/// observably, and teardown runs exactly once via [`teardown_once`].
///
/// # Safety
///
/// `handle`, if non-null, must be a valid `*mut HewRuntime` from
/// [`hew_runtime_new`]. After the call that drops the last reference returns,
/// the handle is freed and must not be used again.
#[no_mangle]
pub unsafe extern "C" fn hew_runtime_release(handle: *mut HewRuntime) {
    if handle.is_null() {
        return;
    }
    // SAFETY: caller guarantees `handle` is live until this release decrements.
    let h = unsafe { &*handle };
    // Decrement with a saturating-at-zero CAS loop rather than a bare
    // `fetch_sub`: a `fetch_sub` on a zero count wraps to `usize::MAX` and would
    // make a subsequent release appear to have references left — a use-after-free
    // window. The loop never lets the count go below zero, so releasing past the
    // last reference is observably a no-op (leak-on-close), not a wrap.
    let mut cur = h.strong.load(Ordering::Acquire);
    loop {
        if cur == 0 {
            // Released past zero: a host contract violation (more releases than
            // retains). Fail closed by leaking rather than double-freeing.
            eprintln!(
                "hew-runtime: hew_runtime_release called with no outstanding reference; \
                 ignoring (leak-on-close — refusing a double free)"
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
        // Other strong references remain (we decremented from `cur` > 1).
        return;
    }
    // Last reference dropped: tear the runtime down (idempotent), then free the
    // handle box itself.
    // SAFETY: `handle` is live and this is the unique last-release caller.
    unsafe { teardown_once(handle) };
    // SAFETY: the box came from `Box::into_raw` in `new_boxed`; no other thread
    // can reach it now (strong count is zero) so reclaiming it is sound.
    drop(unsafe { Box::from_raw(handle) });
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

    /// Serializes these handle tests' use of the shared `TEARDOWN_INVOCATIONS`
    /// counter so a parallel test cannot observe another's increments.
    static HANDLE_TEST_LOCK: Mutex<()> = Mutex::new(());

    /// A retain/release pair brackets a single owner: `new` is count 1, `retain`
    /// takes it to 2, `release` back to 1, and the final `release` drops the
    /// last reference — which runs teardown exactly once and frees the box. We
    /// assert the refcount transitions (the honest ownership oracle) and that
    /// teardown ran exactly once on the last release, not before.
    #[test]
    fn retain_release_runs_teardown_once_on_last_release() {
        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        TEARDOWN_INVOCATIONS.store(0, Ordering::SeqCst);

        let handle = HewRuntime::new_boxed();
        // SAFETY: `handle` is a fresh box owned by this test.
        let h = unsafe { &*handle };
        assert_eq!(h.strong.load(Ordering::Acquire), 1, "new starts at 1");

        // SAFETY: `handle` is live.
        unsafe { hew_runtime_retain(handle) };
        assert_eq!(h.strong.load(Ordering::Acquire), 2, "retain → 2");

        // SAFETY: `handle` is live; this release drops back to one reference and
        // must NOT tear down (references remain).
        unsafe { hew_runtime_release(handle) };
        assert_eq!(h.strong.load(Ordering::Acquire), 1, "release → 1");
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            0,
            "teardown must not run while references remain"
        );

        // Final release drops the last reference: teardown runs once, box freed.
        // SAFETY: last reference; `handle` is freed by this call.
        unsafe { hew_runtime_release(handle) };
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "last release runs teardown exactly once"
        );
    }

    /// Releasing more times than retained must not double-free: a release past
    /// the last reference is refused (leak-on-close), and the saturating CAS
    /// pins the count at zero rather than wrapping to a huge value that would
    /// make a later release think references remain. Teardown runs at most once.
    /// Under `ASan` a double free here would abort.
    #[test]
    fn double_release_is_refused_not_double_free() {
        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        TEARDOWN_INVOCATIONS.store(0, Ordering::SeqCst);

        let handle = HewRuntime::new_boxed();
        // SAFETY: `handle` is a fresh box owned by this test.
        let h = unsafe { &*handle };

        // The real last release: drops to zero, runs teardown once, frees box.
        // SAFETY: single reference; `handle` is freed here.
        unsafe { hew_runtime_release(handle) };
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "last release tears down once"
        );

        // A separate handle forced to zero exercises the release-past-zero
        // refusal without a use-after-free of the freed box above.
        let z = HewRuntime::new_boxed();
        // SAFETY: `z` is a fresh box owned exclusively by this test.
        unsafe {
            (*z).strong.store(0, Ordering::Release);
            // Two releases past zero: both refused (no teardown, no free). The
            // saturating CAS keeps the count pinned at zero — never wrapping.
            hew_runtime_release(z);
            assert_eq!(
                (*z).strong.load(Ordering::Acquire),
                0,
                "release past zero pins the count at zero, not wrap"
            );
            hew_runtime_release(z);
            assert_eq!(
                (*z).strong.load(Ordering::Acquire),
                0,
                "a second release past zero is still a no-op"
            );
            // No extra teardown ran for the refused releases.
            assert_eq!(
                TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
                1,
                "refused releases must not run teardown"
            );
            // The refused releases never freed `z`; reclaim it here.
            drop(Box::from_raw(z));
        }

        let _ = h; // `handle` was freed by its last release; do not touch again.
    }

    /// `shutdown` is idempotent: calling it twice runs teardown once. The `shut`
    /// flag flips on the first call and the second observes it. A later
    /// `release` after an explicit `shutdown` frees the box without a second
    /// teardown.
    #[test]
    fn shutdown_is_idempotent() {
        let _lock = HANDLE_TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        TEARDOWN_INVOCATIONS.store(0, Ordering::SeqCst);

        let handle = HewRuntime::new_boxed();
        // SAFETY: `handle` is a fresh box owned by this test.
        let h = unsafe { &*handle };
        assert!(!h.shut.load(Ordering::Acquire), "fresh handle not shut");

        // SAFETY: `handle` is live.
        unsafe { hew_runtime_shutdown(handle) };
        assert!(h.shut.load(Ordering::Acquire), "shutdown sets shut");
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

        // Final release frees the box; teardown already ran, so no second one.
        // SAFETY: last reference; frees the box.
        unsafe { hew_runtime_release(handle) };
        assert_eq!(
            TEARDOWN_INVOCATIONS.load(Ordering::SeqCst),
            1,
            "release after shutdown does not re-run teardown"
        );
    }

    /// The Tier-B id form reads the discriminant of the handle's runtime. In
    /// this milestone the handle owns the default runtime, so it reports the
    /// default id; a null handle also reports the default. These id forms only
    /// read the discriminant — they never construct or shut a runtime — so they
    /// do not need the handle teardown lock.
    #[test]
    fn runtime_id_of_reports_default() {
        let _guard = crate::runtime_test_guard();
        let handle = HewRuntime::new_boxed();

        let default_id = crate::runtime_id::RuntimeId::DEFAULT.as_u64();
        // SAFETY: `handle` is live; the id form only reads the discriminant.
        let id = unsafe { hew_runtime_id_of(handle) };
        assert_eq!(id, default_id, "the handle owns the default runtime");

        // A null handle reports the default id too.
        // SAFETY: null is an explicit, contracted input.
        let null_id = unsafe { hew_runtime_id_of(std::ptr::null_mut()) };
        assert_eq!(null_id, default_id, "null handle → default id");

        // Reclaim the handle without driving teardown of the guard's runtime:
        // force the count to zero so `release` refuses, then free the box here.
        // SAFETY: `handle` is exclusively owned by this test.
        unsafe {
            (*handle).strong.store(0, Ordering::Release);
            drop(Box::from_raw(handle));
        }
    }

    /// The Tier-B name form resolves a registration against the handle's
    /// runtime. In this milestone it resolves the default registry, so a name
    /// registered through the bare `hew_registry_register` is found through the
    /// explicit-handle form, and the explicit form agrees with the bare lookup.
    #[test]
    fn registry_lookup_with_runtime_resolves_default_registry() {
        let _guard = crate::runtime_test_guard();
        let handle = HewRuntime::new_boxed();

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

            // Clean up the registration and reclaim the handle box.
            assert_eq!(crate::registry::hew_registry_unregister(name.as_ptr()), 0);
            (*handle).strong.store(0, Ordering::Release);
            drop(Box::from_raw(handle));
        }
    }
}
