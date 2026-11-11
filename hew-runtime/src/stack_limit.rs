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
//! address. A frame below it fails with `StackOverflow` and starts an unwind,
//! during which cleanup may use frames down to [`HARD_RESERVE`]. Each loaded
//! image has its own limit slot, so the unwind state belongs to the thread;
//! every slot keeps the soft limit and the runtime retains no image pointer.
//! The soft limit applies again once no Hew frame is live ([`rearm`]). A frame
//! below the hard limit still fails, preserving room to return that fault.

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
/// spawned with it, and the main thread grows to it. `AddressSanitizer` redzones
/// roughly double an O0 frame, so instrumented builds get twice the stack to
/// keep the same floor.
#[cfg(all(not(target_arch = "wasm32"), not(hew_sanitized)))]
pub(crate) const HEW_STACK_BYTES: usize = 16 * 1024 * 1024;
#[cfg(all(not(target_arch = "wasm32"), hew_sanitized))]
pub(crate) const HEW_STACK_BYTES: usize = 32 * 1024 * 1024;

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
    soft: usize,
    hard: usize,
}

thread_local! {
    static LIMITS: Cell<Option<Limits>> = const { Cell::new(None) };
    static UNWINDING: Cell<bool> = const { Cell::new(false) };
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

fn limits() -> Option<Limits> {
    LIMITS.with(|cell| {
        if let Some(limits) = cell.get() {
            return Some(limits);
        }
        let floor = stack_floor()?;
        let limits = Limits {
            soft: floor.saturating_add(SOFT_RESERVE),
            hard: floor.saturating_add(HARD_RESERVE),
        };
        cell.set(Some(limits));
        Some(limits)
    })
}

/// Settle a frame whose address `sp` is below the value in `slot`.
///
/// Returns nonzero when the frame must fail with `StackOverflow`. Each image's
/// slot keeps the soft limit; thread-owned unwind state allows cleanup across
/// images to use the same reserve. An unknown stack clears this image's slot.
///
/// # Safety
/// `slot` is the calling thread's generated limit slot.
#[no_mangle]
pub unsafe extern "C" fn hew_stack_check(slot: *mut usize, sp: usize) -> i32 {
    let Some(limits) = limits() else {
        // SAFETY: the caller passes its own thread's slot.
        unsafe { slot.write(0) };
        return 0;
    };
    // SAFETY: the caller keeps its own slot live throughout this call.
    unsafe { slot.write(limits.soft) };
    if sp >= limits.soft {
        return 0;
    }
    if !UNWINDING.with(|unwinding| unwinding.replace(true)) {
        return 1;
    }
    i32::from(sp < limits.hard)
}

/// Restore the soft limit once a `StackOverflow` unwind has finished on this
/// thread. Called where no Hew frame is live on the calling thread (between
/// scheduler jobs and process-root polls), so every frame that ran on the
/// reserve has returned. No generated slot needs to remain loaded here.
pub(crate) fn rearm() {
    UNWINDING.with(|unwinding| unwinding.set(false));
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;

    #[test]
    fn overflow_cleanup_shares_the_reserve_across_images() {
        std::thread::spawn(|| {
            let (mut first, mut second) = (usize::MAX, usize::MAX);
            // SAFETY: these live slots represent two images on the same thread.
            unsafe {
                assert_eq!(hew_stack_check(&raw mut first, usize::MAX), 0);
                assert_eq!(hew_stack_check(&raw mut second, usize::MAX), 0);
            }
            let soft = first;
            assert_eq!(second, soft);
            let hard = soft - (SOFT_RESERVE - HARD_RESERVE);
            // SAFETY: the addresses model overflow followed by cleanup in the
            // other image; both generated slots remain live throughout.
            unsafe {
                assert_eq!(hew_stack_check(&raw mut first, soft - 1), 1);
                assert_eq!(hew_stack_check(&raw mut second, hard), 0);
                assert_eq!(hew_stack_check(&raw mut first, hard - 1), 1);
            }
            rearm();
            // SAFETY: the next independent invocation must use the soft limit.
            assert_eq!(unsafe { hew_stack_check(&raw mut second, soft - 1) }, 1);
            rearm();
        })
        .join()
        .unwrap();
    }

    #[test]
    fn an_image_slot_can_expire_before_rearm() {
        std::thread::spawn(|| {
            let mut slot = Box::new(usize::MAX);
            // SAFETY: the image's slot remains live throughout this call.
            assert_eq!(unsafe { hew_stack_check(&raw mut *slot, usize::MAX) }, 0);
            // Unloading an image ends its slot's lifetime before the next root
            // poll. ASan observes any later write through a retained pointer.
            drop(slot);
            rearm();
        })
        .join()
        .unwrap();
    }

    #[test]
    fn first_check_installs_the_thread_limit() {
        std::thread::spawn(|| {
            let mut slot = usize::MAX;
            let here = crate::signal::stack::live_stack_address();
            // SAFETY: `slot` stands in for this thread's generated slot.
            assert_eq!(unsafe { hew_stack_check(&raw mut slot, here) }, 0);
            assert!(slot < here, "the soft limit lies below the live frame");
            let soft = slot;
            // SAFETY: as above.
            assert_eq!(unsafe { hew_stack_check(&raw mut slot, soft - 1) }, 1);
            assert_eq!(slot, soft, "the image keeps the shared soft limit");
            let hard = soft - (SOFT_RESERVE - HARD_RESERVE);
            // SAFETY: cleanup on the reserve remains inside the hard limit.
            assert_eq!(unsafe { hew_stack_check(&raw mut slot, hard) }, 0);
            // SAFETY: as above.
            assert_eq!(unsafe { hew_stack_check(&raw mut slot, hard - 1) }, 1);
            rearm();
            // SAFETY: a new invocation must fail at the soft limit again.
            assert_eq!(unsafe { hew_stack_check(&raw mut slot, soft - 1) }, 1);
            rearm();
        })
        .join()
        .unwrap();
    }
}
