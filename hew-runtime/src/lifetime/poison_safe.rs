//! Closure-only, poison-recovering wrappers over [`Mutex`] and [`RwLock`].
//!
//! # Why a closure-only API?
//!
//! A guard returned from `Mutex::lock` or `RwLock::{read,write}` has a
//! lifetime tied to the guard binding, which makes it easy to accidentally
//! hold the guard across an `await`, a re-entrant lock, or a free that
//! invalidates the pointer protected by the lock. A closure API pins the
//! guard to the closure body — the guard is dropped when the closure
//! returns, and it is syntactically impossible to keep a reference into
//! the locked value beyond that point.
//!
//! # Poison recovery
//!
//! A panic inside the closure poisons the inner primitive. The wrappers
//! recover transparently: the next call still takes the lock, gets the
//! (possibly inconsistent) inner value, and runs the closure. This
//! matches the `MutexExt::lock_or_recover` / `RwLockExt::*_or_recover`
//! helpers in `crate::util`, which this module supersedes for named
//! runtime globals.
//!
//! # Safety: no escape hatch
//!
//! The inner `Mutex` / `RwLock` is module-private. No method on
//! `PoisonSafe` or `PoisonSafeRw` returns a guard; no method exposes
//! `&Mutex<T>` or `&RwLock<T>`. Raw `.lock()` / `.read()` / `.write()`
//! on a wrapped global is unreachable by type.

use std::sync::{Mutex, PoisonError, RwLock, TryLockError};

/// Mutex wrapper that recovers from poisoning and exposes access only
/// through a closure.
///
/// Use [`PoisonSafe::access`] for exclusive access; the `&mut T`
/// passed to the closure is valid only for the closure body.
#[allow(
    dead_code,
    reason = "first callers land with the LINK_TABLE/ENV_LOCK sweep"
)]
pub(crate) struct PoisonSafe<T>(Mutex<T>);

#[allow(
    dead_code,
    reason = "first callers land with the LINK_TABLE/ENV_LOCK sweep"
)]
impl<T> PoisonSafe<T> {
    /// Construct a new `PoisonSafe<T>` wrapping `value`.
    pub(crate) const fn new(value: T) -> Self {
        Self(Mutex::new(value))
    }

    /// Exclusive access. Blocks until the mutex is available. Recovers
    /// from poison by taking the inner value via
    /// [`PoisonError::into_inner`].
    #[inline]
    pub(crate) fn access<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        let mut guard = self.0.lock().unwrap_or_else(PoisonError::into_inner);
        f(&mut *guard)
    }

    /// Non-blocking exclusive access. Returns `None` if the mutex is
    /// currently held by another thread. Recovers from poison
    /// transparently — a poisoned-but-uncontended mutex yields
    /// `Some(f(..))`, not `None`.
    #[inline]
    pub(crate) fn try_access<R>(&self, f: impl FnOnce(&mut T) -> R) -> Option<R> {
        match self.0.try_lock() {
            Ok(mut guard) => Some(f(&mut *guard)),
            Err(TryLockError::Poisoned(poisoned)) => {
                let mut guard = poisoned.into_inner();
                Some(f(&mut *guard))
            }
            Err(TryLockError::WouldBlock) => None,
        }
    }

    /// Test-only: report whether the inner mutex is currently poisoned.
    /// Used by unit tests to verify poison-recovery semantics without
    /// exposing runtime observability.
    #[cfg(test)]
    pub(crate) fn is_poisoned_for_test(&self) -> bool {
        self.0.is_poisoned()
    }
}

/// `RwLock` wrapper with the same closure-only access discipline.
///
/// Use [`PoisonSafeRw::read_access`] for shared-read access,
/// [`PoisonSafeRw::access`] for exclusive-write access, and
/// [`PoisonSafeRw::try_access`] for non-blocking write attempts.
#[allow(
    dead_code,
    reason = "first callers land with the LINK_TABLE/ENV_LOCK sweep"
)]
pub(crate) struct PoisonSafeRw<T>(RwLock<T>);

#[allow(
    dead_code,
    reason = "first callers land with the LINK_TABLE/ENV_LOCK sweep"
)]
impl<T> PoisonSafeRw<T> {
    /// Construct a new `PoisonSafeRw<T>` wrapping `value`.
    pub(crate) const fn new(value: T) -> Self {
        Self(RwLock::new(value))
    }

    /// Shared-read access. Blocks until a read lock is available.
    /// Recovers from poison transparently.
    #[inline]
    pub(crate) fn read_access<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        let guard = self.0.read().unwrap_or_else(PoisonError::into_inner);
        f(&*guard)
    }

    /// Exclusive-write access. Blocks until a write lock is available.
    /// Recovers from poison transparently.
    #[inline]
    pub(crate) fn access<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        let mut guard = self.0.write().unwrap_or_else(PoisonError::into_inner);
        f(&mut *guard)
    }

    /// Non-blocking exclusive-write access. Returns `None` if any
    /// reader or writer currently holds the lock. Recovers from
    /// poison transparently.
    #[inline]
    pub(crate) fn try_access<R>(&self, f: impl FnOnce(&mut T) -> R) -> Option<R> {
        match self.0.try_write() {
            Ok(mut guard) => Some(f(&mut *guard)),
            Err(TryLockError::Poisoned(poisoned)) => {
                let mut guard = poisoned.into_inner();
                Some(f(&mut *guard))
            }
            Err(TryLockError::WouldBlock) => None,
        }
    }

    /// Test-only: report whether the inner `RwLock` is currently poisoned.
    #[cfg(test)]
    pub(crate) fn is_poisoned_for_test(&self) -> bool {
        self.0.is_poisoned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Arc, Barrier};
    use std::thread;

    #[test]
    fn poison_safe_access_returns_closure_value() {
        let ps = PoisonSafe::new(0u32);
        let doubled = ps.access(|v| {
            *v = 21;
            *v * 2
        });
        assert_eq!(doubled, 42);
        assert_eq!(ps.access(|v| *v), 21);
    }

    #[test]
    fn poison_safe_recovers_and_access_returns_value() {
        let ps = PoisonSafe::new(Vec::<u32>::new());
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            ps.access(|v| {
                v.push(1);
                panic!("intentional poison");
            });
        }));
        assert!(
            ps.is_poisoned_for_test(),
            "panic inside access must poison the inner mutex"
        );
        let len = ps.access(|v| {
            v.push(2);
            v.len()
        });
        assert_eq!(len, 2, "poison must be recovered and the push applied");
    }

    #[test]
    fn poison_safe_rw_read_and_write_round_trip() {
        let ps = PoisonSafeRw::new(0i64);
        ps.access(|v| *v = -17);
        let read = ps.read_access(|v| *v);
        assert_eq!(read, -17);
    }

    #[test]
    fn poison_safe_rw_recovers_from_write_panic() {
        let ps = PoisonSafeRw::new(0u32);
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            ps.access(|v| {
                *v = 5;
                panic!("intentional poison");
            });
        }));
        assert!(ps.is_poisoned_for_test());
        // Reads recover from poison just like writes.
        let v = ps.read_access(|v| *v);
        assert_eq!(v, 5);
        let w = ps.access(|v| {
            *v = 9;
            *v
        });
        assert_eq!(w, 9);
    }

    #[test]
    fn poison_safe_try_access_returns_none_on_contention() {
        let ps = Arc::new(PoisonSafe::new(0u32));
        let hold = Arc::new(Barrier::new(2));
        let release = Arc::new(Barrier::new(2));

        let ps2 = Arc::clone(&ps);
        let hold2 = Arc::clone(&hold);
        let release2 = Arc::clone(&release);
        let joiner = thread::spawn(move || {
            ps2.access(|_| {
                hold2.wait();
                release2.wait();
            });
        });

        hold.wait();
        // The helper thread holds the lock; try_access must observe
        // contention and return None.
        let result = ps.try_access(|v| *v);
        assert!(
            result.is_none(),
            "try_access must report contention while another thread holds the lock"
        );
        release.wait();
        joiner.join().expect("helper thread panicked");

        // Once the helper has dropped the guard, try_access succeeds.
        let got = ps.try_access(|v| *v);
        assert_eq!(got, Some(0));
    }

    #[test]
    fn poison_safe_rw_try_access_returns_none_on_contention() {
        let ps = Arc::new(PoisonSafeRw::new(0u32));
        let hold = Arc::new(Barrier::new(2));
        let release = Arc::new(Barrier::new(2));

        let ps2 = Arc::clone(&ps);
        let hold2 = Arc::clone(&hold);
        let release2 = Arc::clone(&release);
        let joiner = thread::spawn(move || {
            ps2.access(|_| {
                hold2.wait();
                release2.wait();
            });
        });

        hold.wait();
        let result = ps.try_access(|v| *v);
        assert!(
            result.is_none(),
            "try_access must report contention while another thread holds the write lock"
        );
        release.wait();
        joiner.join().expect("helper thread panicked");

        let got = ps.try_access(|v| *v);
        assert_eq!(got, Some(0));
    }

    #[test]
    fn poison_safe_is_sync_when_t_is_send() {
        fn assert_sync<T: Sync>() {}
        assert_sync::<PoisonSafe<Vec<u8>>>();
        assert_sync::<PoisonSafeRw<Vec<u8>>>();
    }
}
