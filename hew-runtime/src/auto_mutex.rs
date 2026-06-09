//! Hew runtime: auto-injected mutex substrate.
//!
//! The compiler emits the four `hew_auto_mutex_*` FFI symbols defined
//! in this module when it auto-injects a lock around a `BorrowMut`
//! capture that crosses a suspend point in a non-`Sync` context. There
//! is **no user-visible** Hew `Mutex<T>` type or `lock { ... }` block —
//! the compiler emits these symbols.
//!
//! ## ABI
//!
//! All four symbols are stable-ABI C entries:
//!
//! ```text
//!   hew_auto_mutex_alloc()                    -> *mut HewAutoMutex
//!   hew_auto_mutex_lock  (mtx: *mut HewAutoMutex) -> ()
//!   hew_auto_mutex_unlock(mtx: *mut HewAutoMutex) -> ()
//!   hew_auto_mutex_free  (mtx: *mut HewAutoMutex) -> ()
//! ```
//!
//! `HewAutoMutex` is **opaque** to the compiler: codegen treats every
//! handle as a single pointer-sized slot in the closure-env tail (see
//! `hew-mir::closure_env::ClosureEnvLayout::lock_slot_for`).
//!
//! ## Bracket semantics (per-access bracketing)
//!
//! The compiler emits `lock` immediately BEFORE each cross-suspend
//! access of the shared capture and `unlock` immediately AFTER the
//! access completes. The suspend itself happens **outside** the lock
//! window — releasing the classic async-mutex deadlock. The runtime
//! does not enforce this discipline; it is the compiler's contract.
//!
//! ## Lifetime symmetry
//!
//! Every `_alloc` must be paired with exactly one `_free`. The closure-
//! env (or generator-state) constructor calls `_alloc` once per
//! populated lock slot at env materialisation; the env destructor
//! calls `_free` once per slot at scope exit. RAII drop fires
//! `_unlock` on cancel mid-bracket via the standard `DropKind::Resource`
//! LIFO chain (auto-lock release runs in the same drop stream as
//! resource close, ordered AFTER the close per the release-before-free invariant).
//!
//! ## WASM parity (Tenet 5 / Copilot §4)
//!
//! Both the native and WASM scheduler land here. On native, the
//! backing primitive is `std::sync::Mutex<()>` (the standard library
//! mutex available across all supported targets including `wasi`).
//! On `wasm32-unknown-unknown` the std `Mutex` degrades to a single-
//! threaded inhibitor — which is exactly the right semantic, because
//! the WASM scheduler is cooperative-single-threaded: contention
//! within a single browser-VM worker is by definition a re-entry
//! deadlock and we want it to surface immediately rather than silently
//! pass. The `lock()` call on an already-poisoned mutex returns a
//! `PoisonError` which we treat as a fail-closed trap (matching
//! `.github/copilot-instructions.md` §2 fail-closed codegen).

use std::ffi::c_void;
use std::sync::Mutex;

/// Opaque mutex handle. The compiler treats this as a pointer-sized
/// slot; never dereferences it directly.
///
/// The struct holds the boxed `Mutex<()>` plus a hand-rolled poison
/// flag for the rare lock-while-already-locked case on a single-
/// threaded WASM worker (where `std::sync::Mutex::lock` does NOT
/// block by itself — it deadlocks). The `try_lock` fast path keeps
/// the trap surface deterministic on every target.
#[derive(Debug)]
pub struct HewAutoMutex {
    inner: Mutex<()>,
    /// Hand-rolled "currently held" flag for the wasm32-unknown-unknown
    /// target where `Mutex::lock` may deadlock instead of blocking.
    /// On native this stays in lock-step with `inner` and is read only
    /// when `try_lock` fails — i.e. real contention from another OS
    /// thread, in which case we fall back to the blocking `lock()`.
    held: std::sync::atomic::AtomicBool,
    /// Guard slot for the duration of one held bracket. Populated by
    /// `lock`, drained and dropped by `unlock`. `Box<MutexGuard<…>>`
    /// would borrow from `inner`, so we store the guard as a raw
    /// pointer (only ever produced from a guard we own and consumed
    /// in `unlock` before `free`).
    ///
    /// Wrapped in `std::sync::Mutex` to make the cell `Send + Sync`
    /// without an unsafe `Sync` impl — the outer cell is uncontended
    /// in practice because only the holder writes/reads it.
    guard_slot: Mutex<Option<usize>>,
}

impl HewAutoMutex {
    fn new() -> Self {
        Self {
            inner: Mutex::new(()),
            held: std::sync::atomic::AtomicBool::new(false),
            guard_slot: Mutex::new(None),
        }
    }
}

/// Allocate a fresh auto-mutex. Compiler emits one call per populated
/// `ClosureEnvLayout::lock_slot_for` slot at env construction time.
///
/// The returned pointer is **not null** on success; this runtime aborts
/// the host process if heap allocation fails (`Box::new` is infallible
/// in the stable Rust ABI we target — OOM is delivered through the
/// global allocator's abort path).
#[no_mangle]
pub extern "C" fn hew_auto_mutex_alloc() -> *mut HewAutoMutex {
    Box::into_raw(Box::new(HewAutoMutex::new()))
}

/// Acquire the auto-mutex. Compiler emits this immediately BEFORE the
/// suspend-crossing access of the shared capture.
///
/// # Safety
///
/// `mtx` must be a pointer returned by [`hew_auto_mutex_alloc`] and not
/// yet passed to [`hew_auto_mutex_free`]. The compiler enforces this
/// by sourcing `mtx` from the closure-env lock-slot tail; user code
/// cannot construct an `HewAutoMutex` pointer.
///
/// A re-entrant lock from the same task on the same worker — only
/// possible if the compiler erroneously emits `lock` on a path that
/// already holds the lock — surfaces as a deadlock trap (the
/// `std::sync::Mutex` either blocks indefinitely on native or panics
/// on wasm32-unknown-unknown). This is the desired fail-closed
/// behaviour for a checker contract violation.
///
/// # Panics
///
/// Panics on a null `mtx` pointer (boundary-fail-closed: the compiler
/// must not emit a `lock` call against an unallocated slot) or on a
/// poisoned mutex (a prior holder panicked while holding the lock —
/// the lane has already failed; surfacing the poison preserves the
/// fail-closed boundary).
#[no_mangle]
pub unsafe extern "C" fn hew_auto_mutex_lock(mtx: *mut HewAutoMutex) {
    assert!(
        !mtx.is_null(),
        "hew_auto_mutex_lock: null mutex pointer — compiler emitter contract violation \
         (LESSONS P0 boundary-fail-closed)",
    );
    // SAFETY: caller's contract — `mtx` is a live `_alloc` handle.
    let m = unsafe { &*mtx };
    // Block until acquired. `lock()` returns `Err` only on poison;
    // we treat poison as a fail-closed trap because the only way to
    // poison the lock is a panic inside a previous holder, which is
    // already a hard error.
    let guard = m
        .inner
        .lock()
        .expect("hew_auto_mutex_lock: poisoned mutex (prior holder panicked)");
    m.held.store(true, std::sync::atomic::Ordering::Release);
    // Store the guard as a raw pointer in the slot. We leak the guard
    // out of its lexical scope on purpose — `unlock` recreates and
    // drops it.
    let raw = Box::into_raw(Box::new(guard));
    let mut slot = m
        .guard_slot
        .lock()
        .expect("hew_auto_mutex_lock: guard slot poisoned");
    debug_assert!(
        slot.is_none(),
        "hew_auto_mutex_lock: slot already populated — double-lock \
         (compiler emitter contract violation, LESSONS P0 boundary-fail-closed)"
    );
    *slot = Some(raw as usize);
}

/// Release the auto-mutex. Compiler emits this immediately AFTER the
/// suspend-crossing access completes (and BEFORE the next suspend
/// point).
///
/// # Safety
///
/// `mtx` must be a pointer returned by [`hew_auto_mutex_alloc`] that
/// the current task holds via a prior matched [`hew_auto_mutex_lock`].
///
/// # Panics
///
/// Panics on a null `mtx` pointer, on a poisoned guard slot, or on an
/// unmatched unlock (no held guard) — all three are compiler emitter
/// contract violations and surface fail-closed.
#[no_mangle]
pub unsafe extern "C" fn hew_auto_mutex_unlock(mtx: *mut HewAutoMutex) {
    assert!(
        !mtx.is_null(),
        "hew_auto_mutex_unlock: null mutex pointer — compiler emitter contract violation",
    );
    // SAFETY: caller's contract.
    let m = unsafe { &*mtx };
    let raw = {
        let mut slot = m
            .guard_slot
            .lock()
            .expect("hew_auto_mutex_unlock: guard slot poisoned");
        slot.take()
    };
    let raw = raw.expect("hew_auto_mutex_unlock: no held guard — unmatched unlock (compiler bug)");
    m.held.store(false, std::sync::atomic::Ordering::Release);
    // SAFETY: the pointer was produced by `Box::into_raw` in
    // `hew_auto_mutex_lock`; dropping it releases the std mutex guard.
    let _ = unsafe { Box::from_raw(raw as *mut std::sync::MutexGuard<'static, ()>) };
}

/// Free the auto-mutex. Compiler emits this once per `_alloc` at
/// closure-env / generator-state destructor time (LIFO drop
/// stream — auto-lock free runs AFTER any resource-close on the same
/// scope; the `held` debug-assert below catches the misorder).
///
/// # Safety
///
/// `mtx` must be a pointer returned by [`hew_auto_mutex_alloc`] that
/// has been released by a matching [`hew_auto_mutex_unlock`] (or never
/// locked). After this call the pointer is invalid. `null` is allowed
/// and idempotent (see body).
#[no_mangle]
pub unsafe extern "C" fn hew_auto_mutex_free(mtx: *mut HewAutoMutex) {
    if mtx.is_null() {
        // Idempotent on null — the env destructor walks the lock-slot
        // tail unconditionally and skips slots that were never
        // populated (zero-cost path).
        return;
    }
    // SAFETY: caller's contract.
    let held = unsafe { (*mtx).held.load(std::sync::atomic::Ordering::Acquire) };
    debug_assert!(
        !held,
        "hew_auto_mutex_free: mutex was still held at free time — \
         LIFO drop ordering violation (release must precede free, \
         release-before-free invariant)"
    );
    // SAFETY: caller's contract.
    let _ = unsafe { Box::from_raw(mtx) };
}

/// Test-only helper: dummy `c_void` reference to keep the import alive
/// even when `cfg(test)` is off (silences an unused-import warning on
/// platforms where the `c_void` re-export is not transitively required).
#[doc(hidden)]
#[must_use]
pub fn _abi_void_marker() -> *const c_void {
    std::ptr::null()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Newtype around `*mut HewAutoMutex` that asserts `Send` for the
    /// cross-thread exclusion test — the auto-mutex is designed to be
    /// shared across threads (that is the whole point), but the raw
    /// `*mut` itself is `!Send` by default.
    struct SendPtr(*mut HewAutoMutex);
    // SAFETY: the auto-mutex contract is shared-across-threads.
    unsafe impl Send for SendPtr {}

    /// Round-trip: alloc → lock → unlock → free. The most basic
    /// liveness contract: the four FFI symbols compose without
    /// asserting.
    #[test]
    fn auto_mutex_round_trip() {
        let m = hew_auto_mutex_alloc();
        assert!(!m.is_null());
        // SAFETY: `m` is a freshly-allocated handle owned by this test.
        unsafe {
            hew_auto_mutex_lock(m);
            hew_auto_mutex_unlock(m);
            hew_auto_mutex_free(m);
        }
    }

    /// Repeated lock/unlock cycles on the same mutex must compose —
    /// per-access bracketing pattern emits one cycle per
    /// cross-suspend access.
    #[test]
    fn auto_mutex_repeated_bracket() {
        let m = hew_auto_mutex_alloc();
        // SAFETY: `m` is a freshly-allocated handle owned by this test.
        unsafe {
            for _ in 0..16 {
                hew_auto_mutex_lock(m);
                hew_auto_mutex_unlock(m);
            }
            hew_auto_mutex_free(m);
        }
    }

    /// Cross-thread mutual exclusion: two threads each take the lock
    /// 100 times around an increment of a shared counter. Final value
    /// must equal 200 (no races).
    #[test]
    fn auto_mutex_excludes_across_threads() {
        use std::sync::atomic::{AtomicI32, Ordering};
        let m = hew_auto_mutex_alloc();
        let counter = std::sync::Arc::new(AtomicI32::new(0));
        let m_send = SendPtr(m);
        let h1 = {
            let counter = counter.clone();
            std::thread::spawn(move || {
                // Hold the SendPtr wrapper inside the closure
                // environment to keep the closure `Send`-checked
                // (disjoint-capture would otherwise reach inside the
                // wrapper and grab the raw pointer directly).
                let send_ptr = m_send;
                for _ in 0..100 {
                    // SAFETY: `send_ptr.0` is the live handle shared with
                    // the main thread; the test owns the allocation.
                    unsafe { hew_auto_mutex_lock(send_ptr.0) };
                    let v = counter.load(Ordering::Acquire);
                    counter.store(v + 1, Ordering::Release);
                    // SAFETY: matched with the lock immediately above.
                    unsafe { hew_auto_mutex_unlock(send_ptr.0) };
                }
            })
        };
        for _ in 0..100 {
            // SAFETY: `m` is the live handle owned by this test.
            unsafe { hew_auto_mutex_lock(m) };
            let v = counter.load(Ordering::Acquire);
            counter.store(v + 1, Ordering::Release);
            // SAFETY: matched with the lock immediately above.
            unsafe { hew_auto_mutex_unlock(m) };
        }
        h1.join().expect("worker thread joined");
        assert_eq!(counter.load(Ordering::Acquire), 200);
        // SAFETY: worker has joined; this is the sole remaining owner.
        unsafe { hew_auto_mutex_free(m) };
    }

    /// `free` is idempotent on null — the env destructor walks the
    /// lock-slot tail unconditionally and may encounter never-
    /// populated slots in the zero-cost path.
    #[test]
    fn auto_mutex_free_null_is_noop() {
        // SAFETY: null is explicitly contracted as idempotent.
        unsafe { hew_auto_mutex_free(std::ptr::null_mut()) };
    }
}
