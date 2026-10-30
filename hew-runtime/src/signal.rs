//! Process-fatal synchronous hardware-fault boundary.
//!
//! Hew logical failures use typed Rust/C-unwind propagation through generated
//! LLVM cleanup landing pads. A synchronous hardware fault can interrupt an
//! arbitrary ownership operation, so resuming it—especially with longjmp—is
//! unsound, and every such fault is process-fatal (§5.7), stack exhaustion
//! included. Every thread that runs Hew code registers its stack bounds; the
//! fault handler classifies a fault in the stack guard as `StackOverflow`,
//! writes one allocation-free record, and lets the process die by the original
//! signal or exception, so the operating system's crash artefacts (cores,
//! macOS crash reports, Windows Error Reporting dumps) are still produced.
//! Other targets retain their platform default fatal disposition.

use std::cell::Cell;

#[cfg(not(target_arch = "wasm32"))]
mod frames;
#[cfg(not(target_arch = "wasm32"))]
mod record;
#[cfg(not(target_arch = "wasm32"))]
mod stack;

thread_local! {
    /// Nested actor-state finalizers. This remains an explicit invariant guard
    /// for state transactions, but every hardware signal is fatal at every depth.
    static STATE_FIELD_FINALIZER_DEPTH: Cell<usize> = const { Cell::new(0) };
    #[cfg(not(target_arch = "wasm32"))]
    static CRASH_CLEANUP_DRAIN_ACTIVE: Cell<bool> = const { Cell::new(false) };
}

/// Mark whether this thread is draining compatibility actor-state escrow.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn set_crash_cleanup_drain_active(active: bool) {
    CRASH_CLEANUP_DRAIN_ACTIVE.with(|flag| flag.set(active));
}

/// Enter a nested actor-state finalizer critical section.
#[must_use]
pub(crate) fn enter_state_field_finalizer() -> bool {
    STATE_FIELD_FINALIZER_DEPTH.with(|depth| {
        let Some(next) = depth.get().checked_add(1) else {
            std::process::abort();
        };
        depth.set(next);
        true
    })
}

/// Leave a nested actor-state finalizer critical section.
#[must_use]
pub(crate) fn leave_state_field_finalizer() -> bool {
    STATE_FIELD_FINALIZER_DEPTH.with(|depth| {
        let current = depth.get();
        if current == 0 {
            std::process::abort();
        }
        depth.set(current - 1);
        true
    })
}

/// Whether this thread is inside a non-idempotent actor-state finalizer.
///
/// Logical unwinds are process-fatal in this interval because the old value
/// may already be partially destroyed and no sound retry or cleanup authority
/// exists until the replacement transaction commits.
pub(crate) fn state_field_finalizer_active() -> bool {
    STATE_FIELD_FINALIZER_DEPTH.with(|depth| depth.get() != 0)
}

#[cfg(test)]
pub(crate) fn state_field_finalizer_depth() -> usize {
    STATE_FIELD_FINALIZER_DEPTH.with(Cell::get)
}

#[cfg(unix)]
mod unix;
#[cfg(unix)]
use unix as platform;

#[cfg(windows)]
mod windows;
#[cfg(windows)]
use windows as platform;

#[cfg(all(not(any(unix, windows)), not(target_arch = "wasm32")))]
mod platform {
    pub(crate) fn init_crash_handling() {}
    pub(crate) fn init_worker_recovery(_worker_id: u32) {}
    pub(crate) fn ignore_sigpipe() {}
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) use platform::{ignore_sigpipe, init_crash_handling, init_worker_recovery};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finalizer_depth_is_strictly_nested() {
        assert_eq!(state_field_finalizer_depth(), 0);
        assert!(enter_state_field_finalizer());
        assert!(enter_state_field_finalizer());
        assert_eq!(state_field_finalizer_depth(), 2);
        assert!(leave_state_field_finalizer());
        assert!(leave_state_field_finalizer());
        assert_eq!(state_field_finalizer_depth(), 0);
    }
}
