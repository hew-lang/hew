//! Legacy sorted-linked-list timer for the Hew runtime.
//!
//! Provides a simple timer list where entries are sorted by absolute deadline.
//! Insert is O(n); tick checks the head and fires all expired timers.
//!
//! The [`HewTimerList`] struct is `#[repr(C)]` because generated code creates
//! it on the stack.  Locking uses `libc::pthread_mutex_*` to match the C ABI
//! layout.

use std::ffi::{c_int, c_void};
use std::ptr;

use crate::io_time::hew_now_ms;

// ── Cross-platform mutex for #[repr(C)] structs ─────────────────────────

#[cfg(unix)]
type PlatformMutex = libc::pthread_mutex_t;

#[cfg(windows)]
#[repr(C)]
struct PlatformMutex(*mut std::ffi::c_void);

#[cfg(unix)]
const MUTEX_INIT: PlatformMutex = libc::PTHREAD_MUTEX_INITIALIZER;
#[cfg(windows)]
const MUTEX_INIT: PlatformMutex = PlatformMutex(std::ptr::null_mut());

#[cfg(windows)]
#[link(name = "kernel32")]
unsafe extern "system" {
    fn AcquireSRWLockExclusive(lock: *mut PlatformMutex);
    fn ReleaseSRWLockExclusive(lock: *mut PlatformMutex);
}

unsafe fn mutex_init(m: *mut PlatformMutex) {
    #[cfg(unix)]
    unsafe {
        libc::pthread_mutex_init(m, std::ptr::null())
    };
    #[cfg(windows)]
    let _ = m;
}

unsafe fn mutex_lock(m: *mut PlatformMutex) {
    #[cfg(unix)]
    unsafe {
        libc::pthread_mutex_lock(m)
    };
    #[cfg(windows)]
    unsafe {
        AcquireSRWLockExclusive(m)
    };
}

unsafe fn mutex_unlock(m: *mut PlatformMutex) {
    #[cfg(unix)]
    unsafe {
        libc::pthread_mutex_unlock(m)
    };
    #[cfg(windows)]
    unsafe {
        ReleaseSRWLockExclusive(m)
    };
}

unsafe fn mutex_destroy(m: *mut PlatformMutex) {
    #[cfg(unix)]
    unsafe {
        libc::pthread_mutex_destroy(m)
    };
    #[cfg(windows)]
    let _ = m;
}

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// Timer callback signature matching the C `hew_timer_cb` typedef.
pub type HewTimerCb = unsafe extern "C" fn(*mut c_void);

/// A single timer node in the sorted linked list.
///
/// Layout intentionally matches `hew_timer_entry` / `hew_timer` in the C
/// runtime so pointers can be exchanged across the FFI boundary.
#[repr(C)]
#[derive(Debug)]
pub struct HewTimer {
    deadline_ms: u64,
    cb: Option<HewTimerCb>,
    data: *mut c_void,
    cancelled: c_int,
    next: *mut HewTimer,
}

// SAFETY: Timer nodes are only accessed under the list's pthread mutex.
unsafe impl Send for HewTimer {}

/// Thread-safe sorted timer list.
///
/// Must be `#[repr(C)]` because compiled Hew code allocates this on the stack.
#[repr(C)]
pub struct HewTimerList {
    head: *mut HewTimer,
    lock: PlatformMutex,
}

// SAFETY: All access is protected by the internal pthread mutex.
unsafe impl Send for HewTimerList {}
// SAFETY: All access is protected by the internal pthread mutex.
unsafe impl Sync for HewTimerList {}

// Manual Debug impl since libc::pthread_mutex_t doesn't implement Debug.
impl std::fmt::Debug for HewTimerList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewTimerList")
            .field("head", &self.head)
            .field("lock", &"<platform_mutex>")
            .finish()
    }
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Initialise a timer list (zero the head, init the mutex).
///
/// # Safety
///
/// `tl` must point to a valid, uninitialised `HewTimerList`.
#[no_mangle]
pub unsafe extern "C" fn hew_timer_list_init(tl: *mut HewTimerList) {
    if tl.is_null() {
        return;
    }
    // SAFETY: caller guarantees `tl` is valid and writeable.
    unsafe {
        (*tl).head = ptr::null_mut();
        (*tl).lock = MUTEX_INIT;
        mutex_init(&raw mut (*tl).lock);
    }
}

/// Destroy a timer list, freeing all pending timer nodes and the mutex.
///
/// # Safety
///
/// `tl` must point to an initialised `HewTimerList` that will not be used
/// after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_timer_list_destroy(tl: *mut HewTimerList) {
    if tl.is_null() {
        return;
    }
    // SAFETY: caller guarantees `tl` was initialised.
    unsafe {
        mutex_lock(&raw mut (*tl).lock);
        let mut cur = (*tl).head;
        while !cur.is_null() {
            let next = (*cur).next;
            drop(Box::from_raw(cur));
            cur = next;
        }
        (*tl).head = ptr::null_mut();
        mutex_unlock(&raw mut (*tl).lock);
        mutex_destroy(&raw mut (*tl).lock);
    }
}

/// Schedule a new timer that fires after `delay_ms` milliseconds.
///
/// Returns a pointer to the timer node (for cancellation) or null on OOM.
///
/// # Safety
///
/// `tl` must point to an initialised `HewTimerList`.  `cb` and `data` must
/// remain valid until the timer fires or is cancelled.
#[no_mangle]
pub unsafe extern "C" fn hew_timer_schedule(
    tl: *mut HewTimerList,
    delay_ms: u64,
    cb: HewTimerCb,
    data: *mut c_void,
) -> *mut HewTimer {
    if tl.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: hew_now_ms has no preconditions.
    let now = unsafe { hew_now_ms() };
    let timer = Box::into_raw(Box::new(HewTimer {
        deadline_ms: now + delay_ms,
        cb: Some(cb),
        data,
        cancelled: 0,
        next: ptr::null_mut(),
    }));

    // SAFETY: caller guarantees `tl` was initialised.
    unsafe {
        mutex_lock(&raw mut (*tl).lock);

        // Sorted insertion by deadline.
        let mut pp: *mut *mut HewTimer = &raw mut (*tl).head;
        while !(*pp).is_null() && (**pp).deadline_ms <= (*timer).deadline_ms {
            pp = &raw mut (**pp).next;
        }
        (*timer).next = *pp;
        *pp = timer;

        mutex_unlock(&raw mut (*tl).lock);
    }
    timer
}

/// Mark a timer as cancelled.  It will be skipped (and freed) on the next tick.
///
/// # Safety
///
/// `tl` must point to an initialised `HewTimerList`.  `timer` must have been
/// returned by [`hew_timer_schedule`] on the same list and not yet freed.
#[no_mangle]
pub unsafe extern "C" fn hew_timer_cancel(tl: *mut HewTimerList, timer: *mut HewTimer) {
    if tl.is_null() || timer.is_null() {
        return;
    }
    // SAFETY: caller guarantees both pointers are valid.
    unsafe {
        mutex_lock(&raw mut (*tl).lock);
        (*timer).cancelled = 1;
        mutex_unlock(&raw mut (*tl).lock);
    }
}

/// Fire all expired timers whose deadline ≤ now.
///
/// Returns the number of timers actually fired (excluding cancelled ones).
///
/// # Safety
///
/// `tl` must point to an initialised `HewTimerList`.  All callback/data pairs
/// registered via [`hew_timer_schedule`] must still be valid.
#[no_mangle]
pub unsafe extern "C" fn hew_timer_tick(tl: *mut HewTimerList) -> c_int {
    if tl.is_null() {
        return 0;
    }
    // SAFETY: hew_now_ms has no preconditions.
    let now = unsafe { hew_now_ms() };
    let mut fired: c_int = 0;

    // SAFETY: caller guarantees `tl` was initialised.
    unsafe {
        mutex_lock(&raw mut (*tl).lock);
        while !(*tl).head.is_null() && (*(*tl).head).deadline_ms <= now {
            let t = (*tl).head;
            (*tl).head = (*t).next;
            mutex_unlock(&raw mut (*tl).lock);

            if (*t).cancelled == 0 {
                if let Some(cb) = (*t).cb {
                    // SAFETY: callback and data are valid per caller contract.
                    cb((*t).data);
                    fired += 1;
                }
            }
            drop(Box::from_raw(t));

            mutex_lock(&raw mut (*tl).lock);
        }
        mutex_unlock(&raw mut (*tl).lock);
    }
    fired
}

/// Return milliseconds until the next timer fires, or −1 if the list is empty.
///
/// # Safety
///
/// `tl` must point to an initialised `HewTimerList`.
#[no_mangle]
pub unsafe extern "C" fn hew_timer_next_deadline_ms(tl: *mut HewTimerList) -> i64 {
    if tl.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `tl` was initialised.
    unsafe {
        mutex_lock(&raw mut (*tl).lock);
        if (*tl).head.is_null() {
            mutex_unlock(&raw mut (*tl).lock);
            return -1;
        }
        let now = hew_now_ms();
        let deadline = (*(*tl).head).deadline_ms;
        mutex_unlock(&raw mut (*tl).lock);

        let remaining = deadline.cast_signed() - now.cast_signed();
        if remaining > 0 {
            remaining
        } else {
            0
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicI32, Ordering};

    static FIRE_COUNT: AtomicI32 = AtomicI32::new(0);

    unsafe extern "C" fn test_cb(_data: *mut c_void) {
        FIRE_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    #[test]
    fn schedule_and_tick() {
        FIRE_COUNT.store(0, Ordering::SeqCst);
        let mut tl = std::mem::MaybeUninit::<HewTimerList>::uninit();
        // SAFETY: initialising a fresh MaybeUninit.
        unsafe {
            hew_timer_list_init(tl.as_mut_ptr());
            let tl = tl.as_mut_ptr();

            // Schedule a timer with 0ms delay — should fire immediately.
            hew_timer_schedule(tl, 0, test_cb, ptr::null_mut());
            let fired = hew_timer_tick(tl);
            assert_eq!(fired, 1);
            assert_eq!(FIRE_COUNT.load(Ordering::SeqCst), 1);

            hew_timer_list_destroy(tl);
        }
    }

    #[test]
    fn cancel_prevents_firing() {
        FIRE_COUNT.store(0, Ordering::SeqCst);
        let mut tl = std::mem::MaybeUninit::<HewTimerList>::uninit();
        // SAFETY: initialising a fresh MaybeUninit.
        unsafe {
            hew_timer_list_init(tl.as_mut_ptr());
            let tl = tl.as_mut_ptr();

            let t = hew_timer_schedule(tl, 0, test_cb, ptr::null_mut());
            hew_timer_cancel(tl, t);
            let fired = hew_timer_tick(tl);
            assert_eq!(fired, 0);
            assert_eq!(FIRE_COUNT.load(Ordering::SeqCst), 0);

            hew_timer_list_destroy(tl);
        }
    }

    #[test]
    fn next_deadline_empty() {
        let mut tl = std::mem::MaybeUninit::<HewTimerList>::uninit();
        // SAFETY: initialising a fresh MaybeUninit.
        unsafe {
            hew_timer_list_init(tl.as_mut_ptr());
            let tl = tl.as_mut_ptr();
            assert_eq!(hew_timer_next_deadline_ms(tl), -1);
            hew_timer_list_destroy(tl);
        }
    }
}
