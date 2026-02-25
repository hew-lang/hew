//! Actor pool abstraction with configurable routing strategies.

use std::ffi::c_char;
use std::ffi::c_int;
use std::sync::Mutex;
use std::sync::MutexGuard;

use rand::rng;
use rand::RngExt;

/// Routing strategy for actor pools.
#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PoolStrategy {
    RoundRobin = 0,
    Random = 1,
}

#[derive(Debug, Default)]
struct PoolState {
    members: Vec<u64>,
    next_index: usize,
}

/// A pool of actors behind a single logical name.
#[repr(C)]
#[derive(Debug)]
pub struct HewActorPool {
    name: *const c_char,
    strategy: PoolStrategy,
    state: Mutex<PoolState>,
}

fn lock_state(pool: &HewActorPool) -> MutexGuard<'_, PoolState> {
    match pool.state.lock() {
        Ok(state) => state,
        Err(poisoned) => poisoned.into_inner(),
    }
}

/// Create a new actor pool with the provided name and routing strategy.
///
/// # Safety
///
/// `name` must be a valid C string pointer for the lifetime of the pool.
/// Returned pointer must be freed with [`hew_pool_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_pool_new(name: *const c_char, strategy: c_int) -> *mut HewActorPool {
    if name.is_null() {
        return std::ptr::null_mut();
    }

    let strategy = match strategy {
        1 => PoolStrategy::Random,
        2 => PoolStrategy::RoundRobin,
        _ => PoolStrategy::RoundRobin,
    };

    Box::into_raw(Box::new(HewActorPool {
        name,
        strategy,
        state: Mutex::new(PoolState::default()),
    }))
}

/// Add an actor PID to a pool.
///
/// Returns `0` on success, `-1` if `pool` is null.
///
/// # Safety
///
/// `pool` must be a valid pointer returned by [`hew_pool_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_pool_add(pool: *mut HewActorPool, actor_pid: u64) -> c_int {
    if pool.is_null() {
        return -1;
    }
    // SAFETY: Caller guarantees `pool` is valid.
    let pool = unsafe { &mut *pool };
    let mut state = lock_state(pool);
    state.members.push(actor_pid);
    0
}

/// Remove an actor PID from a pool.
///
/// Returns `0` on success, `-1` if `pool` is null or PID not found.
///
/// # Safety
///
/// `pool` must be a valid pointer returned by [`hew_pool_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_pool_remove(pool: *mut HewActorPool, actor_pid: u64) -> c_int {
    if pool.is_null() {
        return -1;
    }
    // SAFETY: Caller guarantees `pool` is valid.
    let pool = unsafe { &mut *pool };
    let mut state = lock_state(pool);
    if let Some(idx) = state.members.iter().position(|&pid| pid == actor_pid) {
        state.members.swap_remove(idx);
        return 0;
    }
    -1
}

/// Return the number of members in a pool.
///
/// # Safety
///
/// `pool` must be a valid pointer returned by [`hew_pool_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_pool_size(pool: *const HewActorPool) -> usize {
    if pool.is_null() {
        return 0;
    }
    // SAFETY: Caller guarantees `pool` is valid.
    let pool = unsafe { &*pool };
    lock_state(pool).members.len()
}

/// Select a member PID according to the pool strategy.
///
/// Returns `0` when the pool is null or has no members.
///
/// # Safety
///
/// `pool` must be a valid pointer returned by [`hew_pool_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_pool_select(pool: *mut HewActorPool) -> u64 {
    if pool.is_null() {
        return 0;
    }
    // SAFETY: Caller guarantees `pool` is valid.
    let pool = unsafe { &mut *pool };
    let mut state = lock_state(pool);
    if state.members.is_empty() {
        return 0;
    }

    match pool.strategy {
        PoolStrategy::RoundRobin => {
            let idx = state.next_index % state.members.len();
            state.next_index = state.next_index.wrapping_add(1);
            state.members[idx]
        }
        PoolStrategy::Random => {
            let mut rng = rng();
            let idx = rng.random_range(0..state.members.len());
            state.members[idx]
        }
    }
}

/// Free a previously allocated actor pool.
///
/// # Safety
///
/// `pool` must be a valid pointer returned by [`hew_pool_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_pool_free(pool: *mut HewActorPool) {
    if pool.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `pool` came from `hew_pool_new`.
    let _ = unsafe { Box::from_raw(pool) };
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::ffi::CString;

    use super::*;

    #[test]
    fn round_robin_selection_cycles_members() {
        let name = CString::new("workers").expect("valid C string");
        // SAFETY: pool pointer is created and used only within this test.
        unsafe {
            let pool = hew_pool_new(name.as_ptr(), PoolStrategy::RoundRobin as c_int);
            assert!(!pool.is_null());
            assert_eq!(hew_pool_add(pool, 11), 0);
            assert_eq!(hew_pool_add(pool, 22), 0);
            assert_eq!(hew_pool_add(pool, 33), 0);
            assert_eq!(hew_pool_size(pool), 3);
            assert_eq!(hew_pool_select(pool), 11);
            assert_eq!(hew_pool_select(pool), 22);
            assert_eq!(hew_pool_select(pool), 33);
            assert_eq!(hew_pool_select(pool), 11);
            hew_pool_free(pool);
        }
    }

    #[test]
    fn pool_round_robin_distributes_evenly() {
        let name = CString::new("workers-even").expect("valid C string");
        // SAFETY: pool pointer is created and used only within this test.
        unsafe {
            let pool = hew_pool_new(name.as_ptr(), PoolStrategy::RoundRobin as c_int);
            assert!(!pool.is_null());
            assert_eq!(hew_pool_add(pool, 101), 0);
            assert_eq!(hew_pool_add(pool, 202), 0);
            assert_eq!(hew_pool_add(pool, 303), 0);

            let mut counts: HashMap<u64, usize> = HashMap::new();
            for _ in 0..9 {
                let selected = hew_pool_select(pool);
                *counts.entry(selected).or_default() += 1;
            }

            assert_eq!(counts.get(&101), Some(&3));
            assert_eq!(counts.get(&202), Some(&3));
            assert_eq!(counts.get(&303), Some(&3));
            hew_pool_free(pool);
        }
    }

    #[test]
    fn pool_add_remove_updates_size() {
        let name = CString::new("workers-size").expect("valid C string");
        // SAFETY: pool pointer is created and used only within this test.
        unsafe {
            let pool = hew_pool_new(name.as_ptr(), PoolStrategy::RoundRobin as c_int);
            assert!(!pool.is_null());
            assert_eq!(hew_pool_size(pool), 0);

            assert_eq!(hew_pool_add(pool, 11), 0);
            assert_eq!(hew_pool_add(pool, 22), 0);
            assert_eq!(hew_pool_add(pool, 33), 0);
            assert_eq!(hew_pool_size(pool), 3);

            assert_eq!(hew_pool_remove(pool, 22), 0);
            assert_eq!(hew_pool_size(pool), 2);
            hew_pool_free(pool);
        }
    }
}
