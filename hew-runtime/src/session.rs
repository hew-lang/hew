//! Session-scoped reset-hook registry.
//!
//! Provides a centralised list of cleanup callbacks that must run whenever the
//! runtime resets between JIT redefinition cycles.  Each subsystem registers a
//! `ResetHook` once (guarded by a `std::sync::Once`) at init time.  When a
//! session boundary is crossed — i.e. `session_reset()` is called from
//! `hew_sched_shutdown` — every registered hook fires in registration order.
//!
//! ## Design
//!
//! The registry itself is unconditionally compiled so both the WASM cooperative
//! scheduler and the native work-stealing scheduler share the same plumbing.
//! Individual hooks are registered only when their owning module is compiled
//! (e.g. `hew_trace_reset` is registered on both native and WASM builds since
//! the `tracing` module is unconditionally compiled; the profiler
//! dispatch-registry clear is registered only when the `profiler` feature is
//! active on a native target).
//!
//! ## Hook ordering
//!
//! Hooks run in the order they were registered.  The intended order is:
//! 1. Tracing (`hew_trace_reset`)           — disable + clear events first.
//! 2. Bridge handler-name clear             — clear the WASM/test msg-type side-table.
//! 3. Profiler dispatch-registry clear      — clear native profiler side-tables last.
//!
//! This ordering is enforced by the init call sites: tracing registers from
//! `scheduler::hew_sched_init` / `scheduler_wasm::hew_sched_init`, bridge
//! registers from `bridge::bridge_init`, and profiler registers from
//! `profiler::register_reset_hooks`.

use crate::lifetime::PoisonSafe;

/// A zero-argument, infallible cleanup callback.
pub(crate) type ResetHook = fn();

/// Global list of reset hooks, populated at init time.
static RESET_HOOKS: PoisonSafe<Vec<ResetHook>> = PoisonSafe::new(Vec::new());

/// Register a reset hook.
///
/// Hooks registered here will be called in order when `session_reset` fires.
/// Callers are responsible for guarding against duplicate registration (e.g.
/// with `std::sync::Once`).
pub(crate) fn register_reset_hook(hook: ResetHook) {
    RESET_HOOKS.access(|hooks| hooks.push(hook));
}

/// Fire all registered reset hooks in registration order.
///
/// Called from both `hew_sched_shutdown` paths (WASM cooperative and native
/// work-stealing) after the actor drain completes and before scheduler statics
/// are cleared.
pub(crate) fn session_reset() {
    // Snapshot the hooks under the lock, then release the lock before
    // calling each hook.  This avoids a potential deadlock if a hook
    // attempts to register another hook during teardown.
    let hooks: Vec<ResetHook> = RESET_HOOKS.access(|hooks| hooks.clone());
    for hook in hooks {
        hook();
    }
}

// ── Cross-module test helpers ────────────────────────────────────────────
//
// Exposed so tests in other modules (e.g. tracing) can serialise with
// session-global-touching operations and reset the hook list to a known
// state before asserting on `session_reset()` behaviour.

/// Serialisation lock for the global `RESET_HOOKS` list.
///
/// Any test that modifies or reads `RESET_HOOKS` must hold this lock for the
/// duration of the test.  Acquiring it from outside `session::tests` ensures
/// safe cross-module test isolation.
///
/// INTENTIONAL: SESSION_TEST_LOCK uses raw Mutex (not PoisonSafe) because it
/// is a test-serialisation barrier — the guard is held across the test body
/// and the closure API provides no benefit here.
#[cfg(test)]
pub(crate) static SESSION_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

/// Clear all registered reset hooks and return the serialisation guard.
///
/// Callers receive the `SESSION_TEST_LOCK` guard; drop it to release.
#[cfg(test)]
pub(crate) fn reset_hooks_for_test() -> std::sync::MutexGuard<'static, ()> {
    let guard = SESSION_TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    RESET_HOOKS.access(|hooks| hooks.clear());
    guard
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::MutexExt;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Mutex;

    // ── Shared hook-ordering helpers ──────────────────────────────────────

    // INTENTIONAL: CALL_ORDER uses Mutex + lock_or_recover — it is a
    // test-local accumulator; no PoisonSafe benefit in this context.
    static CALL_ORDER: Mutex<Vec<usize>> = Mutex::new(Vec::new());
    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn hook_a() {
        CALL_ORDER.lock_or_recover().push(1);
    }
    fn hook_b() {
        CALL_ORDER.lock_or_recover().push(2);
    }
    fn hook_c() {
        CALL_ORDER.lock_or_recover().push(3);
    }
    fn hook_counted() {
        COUNTER.fetch_add(1, Ordering::Relaxed);
    }

    // Use the pub(crate) reset helper — this ensures cross-module tests that
    // also call reset_hooks_for_test() serialise correctly with these tests.
    fn drain_hooks() -> std::sync::MutexGuard<'static, ()> {
        super::reset_hooks_for_test()
    }

    #[test]
    fn hooks_fire_in_registration_order() {
        let _guard = drain_hooks();
        CALL_ORDER.lock_or_recover().clear();

        register_reset_hook(hook_a);
        register_reset_hook(hook_b);
        register_reset_hook(hook_c);

        session_reset();

        let order = CALL_ORDER.lock_or_recover();
        assert_eq!(*order, vec![1usize, 2, 3]);
    }

    #[test]
    fn duplicate_hook_registration_is_distinct() {
        let _guard = drain_hooks();
        COUNTER.store(0, Ordering::Relaxed);

        // Registering the same function pointer twice is allowed; it fires twice.
        register_reset_hook(hook_counted);
        register_reset_hook(hook_counted);

        session_reset();

        assert_eq!(COUNTER.load(Ordering::Relaxed), 2);
    }

    #[test]
    fn session_reset_with_no_hooks_is_a_noop() {
        let _guard = drain_hooks();
        // Should not panic or hang.
        session_reset();
    }
}
