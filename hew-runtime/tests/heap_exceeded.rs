//! Integration tests: arena cap exhaustion → `HeapExceeded` exit reason.
//!
//! Verifies the G1 + G2 substrate:
//!
//! - `hew_arena_malloc` on a cap-constrained arena returns null when the cap
//!   would be exceeded.
//! - `HEW_TRAP_HEAP_EXCEEDED` (200) is distinct from every POSIX signal number.
//! - `ExitReason::from_error_code` maps the constant to `ExitReason::HeapExceeded`
//!   and round-trips all other named variants.
//!
//! The full actor-crash-and-supervisor-restart path (slice A3) requires the
//! `#[max_heap]` attribute to wire `hew_arena_new_with_cap` into the spawn
//! path; that end-to-end test lives in `tests/heap_exceeded_e2e.rs` (added in
//! slice A3).

#![cfg(not(target_arch = "wasm32"))]

use hew_runtime::arena::{
    hew_arena_free_all, hew_arena_malloc, hew_arena_new_with_cap, hew_arena_reset,
    hew_arena_set_current,
};
use hew_runtime::supervisor::{ExitReason, HEW_TRAP_HEAP_EXCEEDED};
use hew_runtime::{set_current_context, HewExecutionContext};

struct ContextGuard {
    ctx: Box<HewExecutionContext>,
    prev: *mut HewExecutionContext,
}

impl ContextGuard {
    fn install() -> Self {
        let mut ctx = Box::new(HewExecutionContext::default());
        let raw = (&raw mut *ctx).cast::<HewExecutionContext>();
        let prev = set_current_context(raw);
        Self { ctx, prev }
    }
}

impl Drop for ContextGuard {
    fn drop(&mut self) {
        let raw = (&raw mut *self.ctx).cast::<HewExecutionContext>();
        let restored = set_current_context(self.prev);
        debug_assert_eq!(restored, raw);
    }
}

// ── Arena cap enforcement via C ABI ──────────────────────────────────────

/// Cap-constrained arena: malloc under the cap succeeds.
#[test]
fn heap_exceeded_malloc_under_cap_succeeds() {
    let arena = hew_arena_new_with_cap(256);
    assert!(!arena.is_null(), "hew_arena_new_with_cap(256) must succeed");
    let _ctx = ContextGuard::install();

    // SAFETY: arena is valid from hew_arena_new_with_cap.
    unsafe { hew_arena_set_current(arena) };

    // SAFETY: arena is installed; malloc routes through it.
    let p1 = unsafe { hew_arena_malloc(128) };
    assert!(
        !p1.is_null(),
        "128-byte alloc under 256-byte cap must succeed"
    );

    // SAFETY: arena is installed; second malloc still under cap.
    let p2 = unsafe { hew_arena_malloc(128) };
    assert!(
        !p2.is_null(),
        "second 128-byte alloc exactly at cap must succeed"
    );

    // SAFETY: null restores no-arena state.
    unsafe { hew_arena_set_current(std::ptr::null_mut()) };
    // SAFETY: arena is valid and not installed.
    unsafe { hew_arena_free_all(arena) };
}

/// Cap-constrained arena: malloc that would exceed cap returns null.
#[test]
fn heap_exceeded_malloc_over_cap_returns_null() {
    let arena = hew_arena_new_with_cap(64);
    assert!(!arena.is_null(), "hew_arena_new_with_cap(64) must succeed");
    let _ctx = ContextGuard::install();

    // SAFETY: arena is valid from hew_arena_new_with_cap.
    unsafe { hew_arena_set_current(arena) };

    // SAFETY: 64-byte alloc exactly fills the cap.
    let p1 = unsafe { hew_arena_malloc(64) };
    assert!(
        !p1.is_null(),
        "64-byte alloc exactly at 64-byte cap must succeed"
    );

    // SAFETY: one more byte would push used to 65 > 64.
    let p2 = unsafe { hew_arena_malloc(1) };
    assert!(
        p2.is_null(),
        "malloc that would exceed cap must return null"
    );

    // SAFETY: null restores no-arena state.
    unsafe { hew_arena_set_current(std::ptr::null_mut()) };
    // SAFETY: arena is valid and not installed.
    unsafe { hew_arena_free_all(arena) };
}

/// After reset, the used counter is zero and the cap restarts.
#[test]
fn heap_exceeded_reset_allows_fresh_cycle() {
    let arena = hew_arena_new_with_cap(64);
    assert!(!arena.is_null());
    let _ctx = ContextGuard::install();

    // SAFETY: arena is valid.
    unsafe { hew_arena_set_current(arena) };

    // Fill to cap.
    // SAFETY: 64-byte alloc fills the cap.
    let p1 = unsafe { hew_arena_malloc(64) };
    assert!(!p1.is_null());

    // Over cap: must fail.
    // SAFETY: arena is installed; next byte exceeds cap.
    let p2 = unsafe { hew_arena_malloc(1) };
    assert!(p2.is_null(), "over-cap alloc must return null");

    // Reset the arena (clears used counter).
    // SAFETY: arena is valid.
    unsafe { hew_arena_reset(arena) };

    // Now the cap restarts — same 64 bytes available.
    // SAFETY: fresh cycle after reset; under cap.
    let p3 = unsafe { hew_arena_malloc(64) };
    assert!(!p3.is_null(), "alloc after reset must succeed up to cap");

    // Over cap again.
    // SAFETY: arena is installed; 1 byte exceeds the 64-byte cap.
    let p4 = unsafe { hew_arena_malloc(1) };
    assert!(
        p4.is_null(),
        "over-cap alloc after reset must still return null"
    );

    // SAFETY: null restores no-arena state.
    unsafe { hew_arena_set_current(std::ptr::null_mut()) };
    // SAFETY: arena is valid and not installed.
    unsafe { hew_arena_free_all(arena) };
}

/// cap=0 arena is unbounded — allocations of any size succeed.
#[test]
fn heap_exceeded_cap_zero_is_unbounded() {
    let arena = hew_arena_new_with_cap(0);
    assert!(!arena.is_null());
    let _ctx = ContextGuard::install();

    // SAFETY: arena is valid.
    unsafe { hew_arena_set_current(arena) };

    // Make many large allocations — none should fail.
    for _ in 0..50 {
        // SAFETY: arena is installed; unbounded so every alloc succeeds.
        let p = unsafe { hew_arena_malloc(4096) };
        assert!(!p.is_null(), "unbounded arena must never return null");
    }

    // SAFETY: null restores no-arena state.
    unsafe { hew_arena_set_current(std::ptr::null_mut()) };
    // SAFETY: arena is valid and not installed.
    unsafe { hew_arena_free_all(arena) };
}

// ── ExitReason mapping ────────────────────────────────────────────────────

/// `HEW_TRAP_HEAP_EXCEEDED` maps to `ExitReason::HeapExceeded`.
#[test]
fn exit_reason_heap_exceeded_maps_correctly() {
    let reason = ExitReason::from_error_code(HEW_TRAP_HEAP_EXCEEDED);
    assert_eq!(reason, ExitReason::HeapExceeded);
}

/// `ExitReason::from_error_code` round-trips all named variants correctly.
#[test]
fn exit_reason_from_error_code_round_trips() {
    assert_eq!(ExitReason::from_error_code(0), ExitReason::Normal);
    assert_eq!(
        ExitReason::from_error_code(HEW_TRAP_HEAP_EXCEEDED),
        ExitReason::HeapExceeded
    );
    // POSIX signal numbers map to Signal(n).
    assert_eq!(ExitReason::from_error_code(11), ExitReason::Signal(11)); // SIGSEGV
    assert_eq!(ExitReason::from_error_code(8), ExitReason::Signal(8)); // SIGFPE
    assert_eq!(ExitReason::from_error_code(4), ExitReason::Signal(4)); // SIGILL
    assert_eq!(ExitReason::from_error_code(10), ExitReason::Signal(10)); // SIGBUS (macOS)
                                                                         // Hew-internal trap code 77 maps to Signal.
    assert_eq!(ExitReason::from_error_code(77), ExitReason::Signal(77));
}

/// `HEW_TRAP_HEAP_EXCEEDED` is 200 — strictly above the POSIX signal range.
///
/// POSIX guarantees signal numbers are positive integers; implementations use
/// values 1–31 (Linux) or 1–31 (macOS). 200 leaves a safe gap.
#[test]
fn heap_exceeded_code_is_above_posix_signal_range() {
    // Verify at compile time — these are const comparisons.
    const { assert!(HEW_TRAP_HEAP_EXCEEDED > 64) }
    const { assert!(HEW_TRAP_HEAP_EXCEEDED == 200) }
}
