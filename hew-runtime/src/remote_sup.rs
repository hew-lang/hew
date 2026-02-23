//! Cross-node (remote) actor supervision for the Hew runtime.
//!
//! Enables a supervisor on one node to monitor and restart actors on
//! remote nodes via proxy supervision. When a remote actor crashes,
//! the local supervisor receives a notification and can decide to
//! restart it on the remote node or escalate.
//!
//! # Architecture
//!
//! ```text
//! Node A (supervisor)              Node B (actor)
//! ┌──────────────────┐             ┌──────────────────┐
//! │  HewSupervisor   │──monitor──▶ │  Remote Actor    │
//! │    + RemoteProxy │◀──notify───│                  │
//! └──────────────────┘             └──────────────────┘
//! ```
//!
//! A `RemoteChildSpec` describes a child actor running on a remote
//! node. The local supervisor creates a `RemoteProxy` that monitors
//! the remote actor via the connection manager and handles failure
//! notifications.
//!
//! # Partition Handling
//!
//! When a network partition is detected (via cluster membership),
//! remote children are marked as `PARTITION` instead of `DEAD`.
//! The supervisor can be configured to either:
//! - **Escalate**: Notify the parent supervisor.
//! - **Wait**: Keep the child in `PARTITION` state until reconnect.
//! - **Restart locally**: Spawn a replacement on the local node.
//!
//! # C ABI
//!
//! - [`hew_remote_sup_add_child`] — Add a remote child to a supervisor.
//! - [`hew_remote_sup_remove_child`] — Remove a remote child.
//! - [`hew_remote_sup_notify`] — Notify supervisor of remote event.
//! - [`hew_remote_sup_child_count`] — Count remote children.
//! - [`hew_remote_sup_set_partition_policy`] — Set partition handling.

use std::ffi::c_int;
use std::sync::Mutex;

// ── Remote child states ────────────────────────────────────────────────

/// Remote child is running normally on the remote node.
pub const REMOTE_CHILD_ALIVE: i32 = 0;
/// Remote child has crashed on the remote node.
pub const REMOTE_CHILD_CRASHED: i32 = 1;
/// Remote child is being restarted on the remote node.
pub const REMOTE_CHILD_RESTARTING: i32 = 2;
/// Remote child is unreachable due to network partition.
pub const REMOTE_CHILD_PARTITION: i32 = 3;
/// Remote child has been stopped.
pub const REMOTE_CHILD_STOPPED: i32 = 4;

// ── Partition policies ─────────────────────────────────────────────────

/// On partition, escalate to the parent supervisor.
pub const PARTITION_ESCALATE: i32 = 0;
/// On partition, keep the child in PARTITION state and wait.
pub const PARTITION_WAIT: i32 = 1;
/// On partition, restart the child locally.
pub const PARTITION_RESTART_LOCAL: i32 = 2;

// ── Remote child spec ──────────────────────────────────────────────────

/// Specification for a remote child actor.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct RemoteChildSpec {
    /// Actor ID on the remote node.
    pub remote_actor_id: u64,
    /// Node ID where the actor lives.
    pub node_id: u16,
    /// Current state of the remote child.
    pub state: i32,
    /// Number of restarts observed.
    pub restart_count: u32,
    /// Maximum restarts before giving up (0 = unlimited).
    pub max_restarts: u32,
}

/// The remote supervision manager.
///
/// Tracks remote child actors and their states. Typically one per
/// supervisor that has remote children.
#[derive(Debug)]
pub struct HewRemoteSup {
    /// Remote children managed by this supervisor.
    children: Mutex<Vec<RemoteChildSpec>>,
    /// Partition handling policy.
    partition_policy: i32,
    /// Callback for remote child events.
    /// Signature: `fn(remote_actor_id: u64, node_id: u16, new_state: i32)`
    callback: Option<RemoteEventCallback>,
}

/// Remote event callback type.
type RemoteEventCallback = unsafe extern "C" fn(u64, u16, i32);

impl HewRemoteSup {
    fn new() -> Self {
        Self {
            children: Mutex::new(Vec::with_capacity(8)),
            partition_policy: PARTITION_ESCALATE,
            callback: None,
        }
    }
}

// ── C ABI ──────────────────────────────────────────────────────────────

/// Create a new remote supervision manager.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub extern "C" fn hew_remote_sup_new() -> *mut HewRemoteSup {
    let sup = Box::new(HewRemoteSup::new());
    Box::into_raw(sup)
}

/// Destroy a remote supervision manager.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_remote_sup_new`],
/// or null (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_free(sup: *mut HewRemoteSup) {
    if !sup.is_null() {
        // SAFETY: caller guarantees `sup` is valid.
        let _ = unsafe { Box::from_raw(sup) };
    }
}

/// Add a remote child to the supervisor.
///
/// Returns 0 on success, -1 on error (e.g., duplicate).
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_remote_sup_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_add_child(
    sup: *mut HewRemoteSup,
    remote_actor_id: u64,
    node_id: u16,
    max_restarts: u32,
) -> c_int {
    if sup.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `sup` is valid.
    let sup = unsafe { &*sup };
    let mut children = match sup.children.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };

    // Check for duplicate.
    if children
        .iter()
        .any(|c| c.remote_actor_id == remote_actor_id)
    {
        return -1;
    }

    children.push(RemoteChildSpec {
        remote_actor_id,
        node_id,
        state: REMOTE_CHILD_ALIVE,
        restart_count: 0,
        max_restarts,
    });
    0
}

/// Remove a remote child from the supervisor.
///
/// Returns 0 on success, -1 if not found.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_remote_sup_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_remove_child(
    sup: *mut HewRemoteSup,
    remote_actor_id: u64,
) -> c_int {
    if sup.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `sup` is valid.
    let sup = unsafe { &*sup };
    let mut children = match sup.children.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };

    let idx = children
        .iter()
        .position(|c| c.remote_actor_id == remote_actor_id);
    match idx {
        Some(i) => {
            children.swap_remove(i);
            0
        }
        None => -1,
    }
}

/// Notify the remote supervisor of a child event.
///
/// `new_state` should be one of the `REMOTE_CHILD_*` constants.
///
/// Returns 0 on success, -1 if the child is not found.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_remote_sup_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_notify(
    sup: *mut HewRemoteSup,
    remote_actor_id: u64,
    new_state: i32,
) -> c_int {
    if sup.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `sup` is valid.
    let sup = unsafe { &mut *sup };
    let mut children = match sup.children.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };

    let child = children
        .iter_mut()
        .find(|c| c.remote_actor_id == remote_actor_id);
    let Some(child) = child else { return -1 };

    let node_id = child.node_id;

    match new_state {
        REMOTE_CHILD_CRASHED => {
            child.restart_count += 1;
            if child.max_restarts > 0 && child.restart_count > child.max_restarts {
                child.state = REMOTE_CHILD_STOPPED;
            } else {
                child.state = REMOTE_CHILD_RESTARTING;
            }
        }
        REMOTE_CHILD_PARTITION => {
            child.state = if sup.partition_policy == PARTITION_RESTART_LOCAL {
                REMOTE_CHILD_RESTARTING
            } else {
                REMOTE_CHILD_PARTITION // ESCALATE and WAIT both use PARTITION state
            };
        }
        _ => {
            child.state = new_state;
        }
    }

    let final_state = child.state;
    drop(children);

    // Fire callback if registered.
    if let Some(cb) = sup.callback {
        // SAFETY: callback is valid per caller contract.
        unsafe { cb(remote_actor_id, node_id, final_state) };
    }

    0
}

/// Return the number of remote children.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_remote_sup_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_child_count(sup: *mut HewRemoteSup) -> c_int {
    if sup.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `sup` is valid.
    let sup = unsafe { &*sup };
    let children = match sup.children.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "remote child count will not exceed c_int range"
    )]
    {
        children.len() as c_int
    }
}

/// Set the partition handling policy.
///
/// `policy` should be one of `PARTITION_ESCALATE`, `PARTITION_WAIT`,
/// or `PARTITION_RESTART_LOCAL`.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_remote_sup_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_set_partition_policy(sup: *mut HewRemoteSup, policy: i32) {
    if sup.is_null() {
        return;
    }
    // SAFETY: caller guarantees `sup` is valid.
    let sup = unsafe { &mut *sup };
    sup.partition_policy = policy;
}

/// Register a callback for remote child events.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_remote_sup_new`].
/// - `callback` must be a valid function pointer, or null to clear.
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_set_callback(
    sup: *mut HewRemoteSup,
    callback: Option<RemoteEventCallback>,
) {
    if sup.is_null() {
        return;
    }
    // SAFETY: caller guarantees `sup` is valid.
    let sup = unsafe { &mut *sup };
    sup.callback = callback;
}

/// Get the state of a remote child.
///
/// Returns the state, or -1 if not found.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_remote_sup_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_child_state(
    sup: *mut HewRemoteSup,
    remote_actor_id: u64,
) -> c_int {
    if sup.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `sup` is valid.
    let sup = unsafe { &*sup };
    let children = match sup.children.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    children
        .iter()
        .find(|c| c.remote_actor_id == remote_actor_id)
        .map_or(-1, |c| c.state)
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_and_destroy() {
        let sup = hew_remote_sup_new();
        assert!(!sup.is_null());
        // SAFETY: sup was just created.
        unsafe {
            assert_eq!(hew_remote_sup_child_count(sup), 0);
            hew_remote_sup_free(sup);
        }
    }

    #[test]
    fn add_remove_children() {
        let sup = hew_remote_sup_new();
        // SAFETY: sup is valid.
        unsafe {
            assert_eq!(hew_remote_sup_add_child(sup, 100, 2, 5), 0);
            assert_eq!(hew_remote_sup_add_child(sup, 200, 3, 10), 0);
            assert_eq!(hew_remote_sup_child_count(sup), 2);

            // Duplicate should fail.
            assert_eq!(hew_remote_sup_add_child(sup, 100, 2, 5), -1);

            // Remove.
            assert_eq!(hew_remote_sup_remove_child(sup, 100), 0);
            assert_eq!(hew_remote_sup_child_count(sup), 1);

            // Remove nonexistent.
            assert_eq!(hew_remote_sup_remove_child(sup, 999), -1);

            hew_remote_sup_free(sup);
        }
    }

    #[test]
    fn crash_increments_restart_count() {
        let sup = hew_remote_sup_new();
        // SAFETY: sup is valid.
        unsafe {
            hew_remote_sup_add_child(sup, 100, 2, 3);

            // First crash → restarting.
            hew_remote_sup_notify(sup, 100, REMOTE_CHILD_CRASHED);
            assert_eq!(
                hew_remote_sup_child_state(sup, 100),
                REMOTE_CHILD_RESTARTING
            );

            // Second crash → restarting.
            hew_remote_sup_notify(sup, 100, REMOTE_CHILD_CRASHED);
            assert_eq!(
                hew_remote_sup_child_state(sup, 100),
                REMOTE_CHILD_RESTARTING
            );

            // Third crash → restarting.
            hew_remote_sup_notify(sup, 100, REMOTE_CHILD_CRASHED);
            assert_eq!(
                hew_remote_sup_child_state(sup, 100),
                REMOTE_CHILD_RESTARTING
            );

            // Fourth crash exceeds max_restarts=3 → stopped.
            hew_remote_sup_notify(sup, 100, REMOTE_CHILD_CRASHED);
            assert_eq!(hew_remote_sup_child_state(sup, 100), REMOTE_CHILD_STOPPED);

            hew_remote_sup_free(sup);
        }
    }

    #[test]
    fn partition_escalate_policy() {
        let sup = hew_remote_sup_new();
        // SAFETY: sup is valid.
        unsafe {
            hew_remote_sup_set_partition_policy(sup, PARTITION_ESCALATE);
            hew_remote_sup_add_child(sup, 100, 2, 5);

            hew_remote_sup_notify(sup, 100, REMOTE_CHILD_PARTITION);
            assert_eq!(hew_remote_sup_child_state(sup, 100), REMOTE_CHILD_PARTITION);

            hew_remote_sup_free(sup);
        }
    }

    #[test]
    fn partition_wait_policy() {
        let sup = hew_remote_sup_new();
        // SAFETY: sup is valid.
        unsafe {
            hew_remote_sup_set_partition_policy(sup, PARTITION_WAIT);
            hew_remote_sup_add_child(sup, 100, 2, 5);

            hew_remote_sup_notify(sup, 100, REMOTE_CHILD_PARTITION);
            assert_eq!(hew_remote_sup_child_state(sup, 100), REMOTE_CHILD_PARTITION);

            hew_remote_sup_free(sup);
        }
    }

    #[test]
    fn partition_restart_local_policy() {
        let sup = hew_remote_sup_new();
        // SAFETY: sup is valid.
        unsafe {
            hew_remote_sup_set_partition_policy(sup, PARTITION_RESTART_LOCAL);
            hew_remote_sup_add_child(sup, 100, 2, 5);

            hew_remote_sup_notify(sup, 100, REMOTE_CHILD_PARTITION);
            assert_eq!(
                hew_remote_sup_child_state(sup, 100),
                REMOTE_CHILD_RESTARTING
            );

            hew_remote_sup_free(sup);
        }
    }

    #[test]
    fn null_safety() {
        // SAFETY: testing null safety.
        unsafe {
            let null: *mut HewRemoteSup = std::ptr::null_mut();
            assert_eq!(hew_remote_sup_child_count(null), 0);
            assert_eq!(hew_remote_sup_child_state(null, 0), -1);
            assert_eq!(hew_remote_sup_notify(null, 0, 0), -1);
            assert_eq!(hew_remote_sup_add_child(null, 0, 0, 0), -1);
            assert_eq!(hew_remote_sup_remove_child(null, 0), -1);
            hew_remote_sup_free(null);
        }
    }

    #[test]
    fn callback_fires_on_crash() {
        use std::sync::atomic::{AtomicI32, Ordering};
        static CB_CALLED: AtomicI32 = AtomicI32::new(0);

        unsafe extern "C" fn test_cb(_id: u64, _node: u16, _state: i32) {
            CB_CALLED.fetch_add(1, Ordering::Relaxed);
        }

        CB_CALLED.store(0, Ordering::Relaxed);
        let sup = hew_remote_sup_new();
        // SAFETY: sup and callback are valid.
        unsafe {
            hew_remote_sup_set_callback(sup, Some(test_cb));
            hew_remote_sup_add_child(sup, 100, 2, 5);
            hew_remote_sup_notify(sup, 100, REMOTE_CHILD_CRASHED);
            assert_eq!(CB_CALLED.load(Ordering::Relaxed), 1);
            hew_remote_sup_free(sup);
        }
    }
}
