//! Stack-exhaustion limit behind the entry check generated for every Hew
//! function (HEW-SPEC-2026 §2.2).
//!
//! Generated code owns one thread-local limit slot, initialized to
//! `usize::MAX`. Each function entry compares its frame address with the
//! slot and calls [`hew_stack_check`] only when the address is below it. The
//! first call on a thread therefore always lands here, and this module
//! replaces the sentinel with the thread's real limit; every thread that runs
//! Hew code is covered without the runtime enumerating them.
//!
//! The limit sits [`SOFT_RESERVE`] bytes above the lowest usable stack
//! address. A frame below it fails with `StackOverflow`, and the slot drops to
//! the hard limit [`HARD_RESERVE`] so the unwind can run cleanup that calls
//! further Hew functions. The slot returns to the soft limit when the fault is
//! released ([`rearm`]). A frame below the hard limit fails again, so the
//! remaining reserve always covers creating the fault and returning it.

use std::cell::Cell;

/// Reserve below the soft limit that cleanup during a stack-overflow unwind
/// may use, including runtime calls such as formatting and allocation.
#[cfg(not(target_arch = "wasm32"))]
const SOFT_RESERVE: usize = 256 * 1024;
#[cfg(target_arch = "wasm32")]
const SOFT_RESERVE: usize = 64 * 1024;

/// Reserve that the hard limit always keeps for creating and returning the
/// fault itself.
#[cfg(not(target_arch = "wasm32"))]
const HARD_RESERVE: usize = 64 * 1024;
#[cfg(target_arch = "wasm32")]
const HARD_RESERVE: usize = 16 * 1024;

/// Stack every thread that runs Hew code gets, sized so 10,000 nested calls of
/// an ordinary function fit at O0 (HEW-SPEC-2026 §2.2): scheduler workers are
/// spawned with it, and the main thread grows to it.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) const HEW_STACK_BYTES: usize = 16 * 1024 * 1024;

/// Let the process entry thread's stack grow to [`HEW_STACK_BYTES`]. Linux and
/// FreeBSD grow the main stack on demand up to the soft `RLIMIT_STACK`, so
/// raising it before the thread records its bounds is enough; macOS and
/// Windows fix the size at link time instead. A hard limit below the target
/// caps the raise.
#[cfg(any(target_os = "linux", target_os = "freebsd"))]
pub(crate) fn reserve_main_stack() {
    let mut limit = libc::rlimit {
        rlim_cur: 0,
        rlim_max: 0,
    };
    // SAFETY: `limit` is valid writable storage for one rlimit.
    if unsafe { libc::getrlimit(libc::RLIMIT_STACK, &raw mut limit) } != 0
        || limit.rlim_cur == libc::RLIM_INFINITY
    {
        return;
    }
    let wanted = HEW_STACK_BYTES as libc::rlim_t;
    let target = if limit.rlim_max == libc::RLIM_INFINITY {
        wanted
    } else {
        wanted.min(limit.rlim_max)
    };
    if limit.rlim_cur < target {
        limit.rlim_cur = target;
        // SAFETY: raising the soft limit within the hard limit is always valid.
        unsafe { libc::setrlimit(libc::RLIMIT_STACK, &raw const limit) };
    }
}

/// macOS links the main thread's stack size into the executable.
#[cfg(all(unix, not(any(target_os = "linux", target_os = "freebsd"))))]
pub(crate) fn reserve_main_stack() {}

#[derive(Clone, Copy)]
struct Limits {
    slot: *mut usize,
    soft: usize,
    hard: usize,
}

thread_local! {
    static LIMITS: Cell<Option<Limits>> = const { Cell::new(None) };
}

/// Lowest usable address of the current thread's stack, if known. The
/// thread's recorded bounds are the one source; a thread that never set up
/// fault reporting records them here on its first check.
#[cfg(not(target_arch = "wasm32"))]
fn stack_floor() -> Option<usize> {
    use crate::signal::stack;
    if !stack::is_registered() {
        stack::register_current_thread(stack::ThreadLabel::Other);
    }
    let bounds = stack::current();
    bounds.is_known().then_some(bounds.low)
}

/// The linear-memory shadow stack starts at `__stack_low` (wasm-ld, placed
/// first with `--stack-first`) and grows down towards it.
#[cfg(target_arch = "wasm32")]
fn stack_floor() -> Option<usize> {
    extern "C" {
        static __stack_low: u8;
    }
    // Only the symbol's address is taken; wasm-ld defines it.
    Some(std::ptr::addr_of!(__stack_low) as usize)
}

/// Settle a frame whose address `sp` is below the value in `slot`.
///
/// Returns nonzero when the frame must fail with `StackOverflow`. On an
/// unknown stack the slot is cleared, which disables the check on this thread.
///
/// # Safety
/// `slot` is the calling thread's generated limit slot.
#[no_mangle]
pub unsafe extern "C" fn hew_stack_check(slot: *mut usize, sp: usize) -> i32 {
    let limits = LIMITS.with(|cell| {
        if let Some(limits) = cell.get().filter(|limits| limits.slot == slot) {
            return Some(limits);
        }
        let floor = stack_floor()?;
        let limits = Limits {
            slot,
            soft: floor.saturating_add(SOFT_RESERVE),
            hard: floor.saturating_add(HARD_RESERVE),
        };
        cell.set(Some(limits));
        Some(limits)
    });
    let Some(limits) = limits else {
        // SAFETY: the caller passes its own thread's slot.
        unsafe { slot.write(0) };
        return 0;
    };
    if sp >= limits.soft {
        // SAFETY: as above.
        unsafe { slot.write(limits.soft) };
        return 0;
    }
    // SAFETY: as above.
    unsafe { slot.write(limits.hard) };
    1
}

/// Restore the soft limit once a `StackOverflow` fault is released. A thread
/// that never installed a limit keeps its sentinel.
pub(crate) fn rearm() {
    LIMITS.with(|cell| {
        if let Some(limits) = cell.get() {
            // SAFETY: the slot belongs to this thread and lives as long as it.
            unsafe { limits.slot.write(limits.soft) };
        }
    });
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;

    #[inline(never)]
    fn frame_address() -> usize {
        let marker = 0_u8;
        std::hint::black_box(&raw const marker) as usize
    }

    #[test]
    fn first_check_installs_the_thread_limit() {
        std::thread::spawn(|| {
            let mut slot = usize::MAX;
            let here = frame_address();
            // SAFETY: `slot` stands in for this thread's generated slot.
            assert_eq!(unsafe { hew_stack_check(&raw mut slot, here) }, 0);
            assert!(slot < here, "the soft limit lies below the live frame");
            let soft = slot;
            // SAFETY: as above.
            assert_eq!(unsafe { hew_stack_check(&raw mut slot, soft - 1) }, 1);
            assert!(slot < soft, "an overflow lowers the slot to the hard limit");
            let hard = slot;
            // SAFETY: as above.
            assert_eq!(unsafe { hew_stack_check(&raw mut slot, hard - 1) }, 1);
            rearm();
            assert_eq!(slot, soft, "releasing the fault restores the soft limit");
        })
        .join()
        .unwrap();
    }
}
