//! Process-fatal synchronous hardware-fault boundary.
//!
//! Hew logical failures use typed Rust/C-unwind propagation through generated
//! LLVM cleanup landing pads. A synchronous hardware fault can interrupt an
//! arbitrary ownership operation, so resuming it—especially with longjmp—is
//! unsound. Unix installs an alternate-stack handler that performs only
//! async-signal-safe diagnostic output and `_exit`. Other targets retain their
//! platform default fatal disposition.

use std::cell::Cell;

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
mod platform {
    use std::cell::RefCell;
    use std::mem::MaybeUninit;
    use std::ptr;
    use std::sync::Once;

    const ALT_STACK_BYTES: usize = 64 * 1024;
    const FATAL_MESSAGE: &[u8] = b"hew: fatal synchronous hardware fault\n";

    thread_local! {
        /// Owns the memory registered with `sigaltstack` for this worker's
        /// lifetime. The handler never touches this Rust container.
        static ALT_STACK: RefCell<Option<Box<[u8]>>> = const { RefCell::new(None) };
    }

    unsafe extern "C" fn fatal_signal_handler(
        signal: libc::c_int,
        _info: *mut libc::siginfo_t,
        _context: *mut libc::c_void,
    ) {
        // SAFETY: write and _exit are async-signal-safe POSIX operations; the
        // buffer is static and its exact length is supplied.
        unsafe {
            let _ = libc::write(
                libc::STDERR_FILENO,
                FATAL_MESSAGE.as_ptr().cast(),
                FATAL_MESSAGE.len(),
            );
            libc::_exit(128_i32.saturating_add(signal));
        }
    }

    pub(crate) fn init_crash_handling() {
        static INSTALL: Once = Once::new();
        INSTALL.call_once(|| {
            let signals = [
                libc::SIGSEGV,
                libc::SIGBUS,
                libc::SIGFPE,
                libc::SIGILL,
                libc::SIGTRAP,
            ];
            for signal in signals {
                let mut action = MaybeUninit::<libc::sigaction>::zeroed();
                // SAFETY: zero is a valid initial representation; all required
                // sigaction fields are initialized below.
                let action = unsafe { action.assume_init_mut() };
                action.sa_sigaction = fatal_signal_handler as *const () as usize;
                action.sa_flags = libc::SA_SIGINFO | libc::SA_ONSTACK;
                // SAFETY: action points to initialized writable storage.
                unsafe {
                    libc::sigemptyset(&raw mut action.sa_mask);
                    if libc::sigaction(signal, action, ptr::null_mut()) != 0 {
                        std::process::abort();
                    }
                }
            }
        });
    }

    pub(crate) fn init_worker_recovery(_worker_id: u32) {
        ALT_STACK.with(|slot| {
            if slot.borrow().is_some() {
                return;
            }
            let mut memory = vec![0_u8; ALT_STACK_BYTES].into_boxed_slice();
            let stack = libc::stack_t {
                ss_sp: memory.as_mut_ptr().cast(),
                ss_flags: 0,
                ss_size: memory.len(),
            };
            // SAFETY: memory remains owned by ALT_STACK until thread teardown.
            if unsafe { libc::sigaltstack(&raw const stack, ptr::null_mut()) } != 0 {
                std::process::abort();
            }
            *slot.borrow_mut() = Some(memory);
        });
    }

    pub(crate) fn ignore_sigpipe() {
        // SAFETY: setting SIGPIPE to SIG_IGN is process-wide and idempotent.
        unsafe {
            libc::signal(libc::SIGPIPE, libc::SIG_IGN);
        }
    }
}

#[cfg(all(not(unix), not(target_arch = "wasm32")))]
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
