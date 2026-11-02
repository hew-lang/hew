//! Per-thread stack bounds and stack-overflow classification.
//!
//! Hew coroutines are stackless LLVM coroutines: a resumed frame runs on the
//! stack of the thread that resumes it. The thread stack is therefore the only
//! stack whose guard region a fault can hit, on every target. Each thread that
//! runs Hew code records its bounds once, outside any handler; the fault
//! handlers only read the const-initialized thread-local.

use std::cell::Cell;

/// The region below the lowest usable stack address that counts as the guard.
///
/// A thread's guard page sits directly below its stack. The Linux main thread
/// has no page of its own: the kernel keeps `stack_guard_gap` (256 pages)
/// unmapped below the growable stack. A fault anywhere in this window cannot be
/// an access to a live mapping, so it is classified as overflow.
pub(crate) const GUARD_WINDOW_BYTES: usize = 1 << 20;

/// Bounds of one thread's stack: `[low, high)` is usable, `guard` bytes below
/// `low` are the OS guard.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct StackBounds {
    pub(crate) low: usize,
    pub(crate) high: usize,
    pub(crate) guard: usize,
}

impl StackBounds {
    pub(crate) const UNKNOWN: Self = Self {
        low: 0,
        high: 0,
        guard: 0,
    };

    pub(crate) const fn is_known(self) -> bool {
        self.low < self.high
    }

    /// Whether `addr` is inside the usable stack.
    pub(crate) const fn contains(self, addr: usize) -> bool {
        self.is_known() && self.low <= addr && addr < self.high
    }

    /// Whether a fault at `addr` is a stack overflow.
    ///
    /// The window extends `guard` (at least [`GUARD_WINDOW_BYTES`]) below
    /// `low`, and `guard` above it as well: glibc has reported thread stacks
    /// both with and without the guard included, so both halves are treated as
    /// guard, as Rust's own overflow handler does.
    pub(crate) const fn in_guard(self, addr: usize) -> bool {
        if !self.is_known() {
            return false;
        }
        let below = if self.guard > GUARD_WINDOW_BYTES {
            self.guard
        } else {
            GUARD_WINDOW_BYTES
        };
        let above = if self.guard > page_size_floor() {
            self.guard
        } else {
            page_size_floor()
        };
        addr >= self.low.saturating_sub(below) && addr < self.low.saturating_add(above)
    }
}

/// The smallest page size on any supported target; only used to size the
/// upper half of the guard window when the OS reports no guard.
const fn page_size_floor() -> usize {
    4096
}

/// How a thread is named in a failure record.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ThreadLabel {
    /// The process entry thread.
    Main,
    /// A scheduler worker.
    Worker(u32),
    /// Any other thread that registered its stack.
    Other,
}

thread_local! {
    static BOUNDS: Cell<StackBounds> = const { Cell::new(StackBounds::UNKNOWN) };
    static LABEL: Cell<ThreadLabel> = const { Cell::new(ThreadLabel::Other) };
    static REGISTERED: Cell<bool> = const { Cell::new(false) };
}

/// The calling thread's recorded stack bounds. Async-signal-safe: the
/// thread-local is const-initialized and registered before any fault.
pub(crate) fn current() -> StackBounds {
    BOUNDS.with(Cell::get)
}

/// The calling thread's label for failure records.
pub(crate) fn current_label() -> ThreadLabel {
    LABEL.with(Cell::get)
}

/// Whether the calling thread has registered.
pub(crate) fn is_registered() -> bool {
    REGISTERED.with(Cell::get)
}

/// Record the calling thread's stack bounds and label.
pub(crate) fn register_current_thread(label: ThreadLabel) {
    REGISTERED.with(|slot| slot.set(true));
    LABEL.with(|slot| slot.set(label));
    if let Some(bounds) = os::query_current() {
        BOUNDS.with(|slot| slot.set(bounds));
    }
}

#[cfg(test)]
pub(crate) fn set_current_for_test(bounds: StackBounds) -> StackBounds {
    BOUNDS.with(|slot| slot.replace(bounds))
}

#[cfg(any(target_os = "linux", target_os = "android"))]
mod os {
    use super::StackBounds;
    use std::mem::MaybeUninit;

    pub(super) fn query_current() -> Option<StackBounds> {
        let mut attr = MaybeUninit::<libc::pthread_attr_t>::uninit();
        // SAFETY: pthread_getattr_np initializes `attr` on success; it is
        // destroyed on every path after that. This runs at thread setup, never
        // inside a signal handler (it reads /proc/self/maps for the main thread).
        unsafe {
            if libc::pthread_getattr_np(libc::pthread_self(), attr.as_mut_ptr()) != 0 {
                return None;
            }
            let mut addr = std::ptr::null_mut();
            let mut size = 0;
            let mut guard = 0;
            let stack_ok = libc::pthread_attr_getstack(attr.as_ptr(), &raw mut addr, &raw mut size);
            let guard_ok = libc::pthread_attr_getguardsize(attr.as_ptr(), &raw mut guard);
            libc::pthread_attr_destroy(attr.as_mut_ptr());
            if stack_ok != 0 {
                return None;
            }
            let low = addr as usize;
            Some(StackBounds {
                low,
                high: low.checked_add(size)?,
                guard: if guard_ok == 0 { guard } else { 0 },
            })
        }
    }
}

#[cfg(target_os = "freebsd")]
mod os {
    use super::StackBounds;
    use std::mem::MaybeUninit;

    pub(super) fn query_current() -> Option<StackBounds> {
        let mut attr = MaybeUninit::<libc::pthread_attr_t>::uninit();
        // SAFETY: the attribute object is initialized before
        // pthread_attr_get_np fills it, and destroyed on every path.
        unsafe {
            if libc::pthread_attr_init(attr.as_mut_ptr()) != 0 {
                return None;
            }
            let filled = libc::pthread_attr_get_np(libc::pthread_self(), attr.as_mut_ptr());
            let mut addr = std::ptr::null_mut();
            let mut size = 0;
            let mut guard = 0;
            let stack_ok = filled == 0
                && libc::pthread_attr_getstack(attr.as_ptr(), &raw mut addr, &raw mut size) == 0;
            let guard_ok = libc::pthread_attr_getguardsize(attr.as_ptr(), &raw mut guard) == 0;
            libc::pthread_attr_destroy(attr.as_mut_ptr());
            if !stack_ok {
                return None;
            }
            let low = addr as usize;
            Some(StackBounds {
                low,
                high: low.checked_add(size)?,
                guard: if guard_ok { guard } else { 0 },
            })
        }
    }
}

#[cfg(target_vendor = "apple")]
mod os {
    use super::StackBounds;

    pub(super) fn query_current() -> Option<StackBounds> {
        // SAFETY: both queries take the calling thread's own handle.
        let (top, size) = unsafe {
            let thread = libc::pthread_self();
            (
                libc::pthread_get_stackaddr_np(thread) as usize,
                libc::pthread_get_stacksize_np(thread),
            )
        };
        let low = top.checked_sub(size)?;
        // SAFETY: sysconf has no preconditions.
        let page = usize::try_from(unsafe { libc::sysconf(libc::_SC_PAGESIZE) }).unwrap_or(0);
        Some(StackBounds {
            low,
            high: top,
            guard: page,
        })
    }
}

#[cfg(windows)]
mod os {
    use super::StackBounds;
    use windows_sys::Win32::System::Threading::GetCurrentThreadStackLimits;

    pub(super) fn query_current() -> Option<StackBounds> {
        let mut low = 0_usize;
        let mut high = 0_usize;
        // SAFETY: both out-pointers are valid for writes.
        unsafe { GetCurrentThreadStackLimits(&raw mut low, &raw mut high) };
        // The reservation's lowest pages hold the guard and the hard limit;
        // the window below `low` therefore lies inside the reservation.
        Some(StackBounds {
            low,
            high,
            guard: 0,
        })
    }
}

#[cfg(not(any(
    target_os = "linux",
    target_os = "android",
    target_os = "freebsd",
    target_vendor = "apple",
    windows
)))]
mod os {
    use super::StackBounds;

    pub(super) fn query_current() -> Option<StackBounds> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const LOW: usize = 0x7000_0000;
    const HIGH: usize = 0x7080_0000;

    fn bounds(guard: usize) -> StackBounds {
        StackBounds {
            low: LOW,
            high: HIGH,
            guard,
        }
    }

    #[test]
    fn faults_just_below_the_stack_are_overflow() {
        let thread = bounds(4096);
        assert!(thread.in_guard(LOW - 8));
        assert!(thread.in_guard(LOW - 4096));
        // The Linux main thread's gap reaches a mebibyte below the stack.
        assert!(thread.in_guard(LOW - GUARD_WINDOW_BYTES));
        assert!(!thread.in_guard(LOW - GUARD_WINDOW_BYTES - 1));
    }

    #[test]
    fn a_guard_counted_inside_the_reported_stack_is_overflow() {
        let thread = bounds(64 * 1024);
        assert!(thread.in_guard(LOW));
        assert!(thread.in_guard(LOW + 64 * 1024 - 1));
        assert!(!thread.in_guard(LOW + 64 * 1024));
    }

    #[test]
    fn ordinary_wild_pointers_are_not_overflow() {
        let thread = bounds(4096);
        assert!(!thread.in_guard(0));
        assert!(!thread.in_guard(8));
        assert!(!thread.in_guard(HIGH - 8));
        assert!(!thread.in_guard(HIGH + 4096));
    }

    #[test]
    fn unknown_bounds_never_classify() {
        assert!(!StackBounds::UNKNOWN.in_guard(0));
        assert!(!StackBounds::UNKNOWN.in_guard(8));
        assert!(!StackBounds::UNKNOWN.contains(8));
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn registered_bounds_contain_the_live_stack() {
        let previous = set_current_for_test(StackBounds::UNKNOWN);
        register_current_thread(ThreadLabel::Other);
        let local = 0_u64;
        let here = (&raw const local) as usize;
        let current = current();
        let _ = set_current_for_test(previous);
        assert!(
            current.contains(here),
            "{current:x?} must contain {here:#x}"
        );
        assert!(!current.in_guard(here));
    }
}
