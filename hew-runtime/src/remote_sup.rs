//! Remote supervision wired to unified [`crate::hew_node::HewNode`].
//!
//! This is scaffolding for remote restart orchestration. It currently monitors
//! remote PIDs, watches SWIM membership for remote node death, and invokes a
//! callback when monitored actors are considered dead.

use std::ffi::{c_int, c_void};
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use crate::cluster::{self, HEW_MEMBERSHIP_EVENT_NODE_DEAD, MEMBER_DEAD};
use crate::hew_node::HewNode;

const DEFAULT_HEARTBEAT_INTERVAL_MS: u64 = 1_000;

/// Restart strategy used by remote supervision.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SupervisorStrategy {
    /// Restart only the failed actor.
    OneForOne = 0,
    /// Restart all monitored actors together.
    OneForAll = 1,
}

impl SupervisorStrategy {
    fn from_c_int(raw: c_int) -> Option<Self> {
        match raw {
            0 => Some(Self::OneForOne),
            1 => Some(Self::OneForAll),
            _ => None,
        }
    }
}

/// Callback fired when a monitored actor is considered dead.
/// Signature: `fn(remote_pid, remote_node_id, reason)`.
type RemoteDeathCallback = unsafe extern "C" fn(u64, u16, c_int);

/// A remote supervisor monitors actors on a remote node.
#[repr(C)]
#[derive(Debug)]
pub struct HewRemoteSupervisor {
    /// The node this supervisor is running on
    node: *mut HewNode,
    /// Remote node being supervised
    remote_node_id: u16,
    /// Actors being monitored (remote PIDs)
    monitored: Vec<u64>,
    /// Strategy (one-for-one, one-for-all)
    strategy: SupervisorStrategy,
    /// Heartbeat interval for liveness checks
    heartbeat_interval_ms: u64,
    callback: Option<RemoteDeathCallback>,
    running: AtomicBool,
    heartbeat_thread: Option<JoinHandle<()>>,
}

impl HewRemoteSupervisor {
    fn dispatch_remote_death(&self) {
        let Some(cb) = self.callback else {
            return;
        };

        let monitored = self.monitored.clone();
        match self.strategy {
            SupervisorStrategy::OneForOne => {
                for remote_pid in monitored {
                    // SAFETY: callback pointer validity is guaranteed by caller contract.
                    unsafe { cb(remote_pid, self.remote_node_id, MEMBER_DEAD) };
                }
            }
            SupervisorStrategy::OneForAll => {
                for remote_pid in monitored {
                    // SAFETY: callback pointer validity is guaranteed by caller contract.
                    unsafe { cb(remote_pid, self.remote_node_id, MEMBER_DEAD) };
                }
            }
        }
    }
}

extern "C" fn noop_membership_callback(_node_id: u16, _event: u8, _user_data: *mut c_void) {}

extern "C" fn remote_sup_membership_callback(node_id: u16, event: u8, user_data: *mut c_void) {
    if event != HEW_MEMBERSHIP_EVENT_NODE_DEAD || user_data.is_null() {
        return;
    }

    // SAFETY: user_data is set to HewRemoteSupervisor* by hew_remote_sup_start.
    let sup = unsafe { &*user_data.cast::<HewRemoteSupervisor>() };
    if !sup.running.load(Ordering::Acquire) || node_id != sup.remote_node_id {
        return;
    }

    sup.dispatch_remote_death();
}

/// Create a new remote supervisor bound to a local node and remote node ID.
///
/// # Safety
///
/// `node` must be a valid pointer returned by [`crate::hew_node::hew_node_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_new(
    node: *mut HewNode,
    remote_node_id: u16,
    strategy: c_int,
) -> *mut HewRemoteSupervisor {
    if node.is_null() || remote_node_id == 0 {
        return ptr::null_mut();
    }
    let Some(strategy) = SupervisorStrategy::from_c_int(strategy) else {
        return ptr::null_mut();
    };

    let sup = Box::new(HewRemoteSupervisor {
        node,
        remote_node_id,
        monitored: Vec::new(),
        strategy,
        heartbeat_interval_ms: DEFAULT_HEARTBEAT_INTERVAL_MS,
        callback: None,
        running: AtomicBool::new(false),
        heartbeat_thread: None,
    });
    Box::into_raw(sup)
}

/// Monitor a remote actor PID.
///
/// Returns 0 on success, -1 on error/duplicate.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_remote_sup_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_monitor(
    sup: *mut HewRemoteSupervisor,
    remote_pid: u64,
) -> c_int {
    if sup.is_null() || remote_pid == 0 {
        return -1;
    }

    // SAFETY: caller guarantees `sup` is valid.
    let sup = unsafe { &mut *sup };
    let pid_node = crate::pid::hew_pid_node(remote_pid);
    if pid_node != 0 && pid_node != sup.remote_node_id {
        return -1;
    }
    if sup.monitored.contains(&remote_pid) {
        return -1;
    }

    sup.monitored.push(remote_pid);
    0
}

/// Stop monitoring a remote actor PID.
///
/// Returns 0 on success, -1 if the PID was not monitored.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_remote_sup_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_unmonitor(
    sup: *mut HewRemoteSupervisor,
    remote_pid: u64,
) -> c_int {
    if sup.is_null() {
        return -1;
    }

    // SAFETY: caller guarantees `sup` is valid.
    let sup = unsafe { &mut *sup };
    let Some(idx) = sup.monitored.iter().position(|pid| *pid == remote_pid) else {
        return -1;
    };

    sup.monitored.swap_remove(idx);
    0
}

/// Start remote supervision: register SWIM callback and heartbeat tick loop.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_remote_sup_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_start(sup: *mut HewRemoteSupervisor) -> c_int {
    if sup.is_null() {
        return -1;
    }

    // SAFETY: caller guarantees `sup` is valid.
    let sup = unsafe { &mut *sup };
    if sup.running.swap(true, Ordering::AcqRel) {
        return 0;
    }

    // SAFETY: `node` is validated by constructor contract.
    let node = unsafe { &mut *sup.node };
    if node.cluster.is_null() {
        sup.running.store(false, Ordering::Release);
        return -1;
    }

    // SAFETY: cluster pointer is valid and callback/user_data remain valid while running.
    unsafe {
        cluster::hew_cluster_set_membership_callback(
            node.cluster,
            remote_sup_membership_callback,
            ptr::from_mut::<HewRemoteSupervisor>(sup).cast::<c_void>(),
        );
    }

    let interval_ms = sup.heartbeat_interval_ms.max(10);
    let sup_addr = ptr::from_mut::<HewRemoteSupervisor>(sup) as usize;
    let cluster_addr = node.cluster as usize;

    let handle = thread::Builder::new()
        .name(format!("hew-remote-sup-{}", sup.remote_node_id))
        .spawn(move || {
            let sup_ptr = sup_addr as *mut HewRemoteSupervisor;
            loop {
                // SAFETY: sup_ptr remains valid until stop joins this thread.
                let keep_running = unsafe { (*sup_ptr).running.load(Ordering::Acquire) };
                if !keep_running {
                    break;
                }
                // SAFETY: cluster pointer belongs to local node and is valid while supervisor is running.
                let _ =
                    unsafe { cluster::hew_cluster_tick(cluster_addr as *mut cluster::HewCluster) };
                thread::sleep(Duration::from_millis(interval_ms));
            }
        });

    match handle {
        Ok(h) => {
            sup.heartbeat_thread = Some(h);
            0
        }
        Err(_) => {
            sup.running.store(false, Ordering::Release);
            -1
        }
    }
}

/// Stop remote supervision and heartbeat checks.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_remote_sup_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_stop(sup: *mut HewRemoteSupervisor) -> c_int {
    if sup.is_null() {
        return -1;
    }

    // SAFETY: caller guarantees `sup` is valid.
    let sup = unsafe { &mut *sup };
    if !sup.running.swap(false, Ordering::AcqRel) {
        return 0;
    }

    if let Some(handle) = sup.heartbeat_thread.take() {
        let _ = handle.join();
    }

    // SAFETY: `node` is valid while supervisor lives.
    let node = unsafe { &mut *sup.node };
    if !node.cluster.is_null() {
        // SAFETY: resets callback to no-op to avoid dangling callback userdata.
        unsafe {
            cluster::hew_cluster_set_membership_callback(
                node.cluster,
                noop_membership_callback,
                ptr::null_mut(),
            );
        }
    }

    0
}

/// Register callback fired when monitored remote actors are considered dead.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_remote_sup_new`].
/// - `callback` must remain valid while set.
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_set_callback(
    sup: *mut HewRemoteSupervisor,
    callback: Option<RemoteDeathCallback>,
) {
    if sup.is_null() {
        return;
    }

    // SAFETY: caller guarantees `sup` is valid.
    let sup = unsafe { &mut *sup };
    sup.callback = callback;
}

/// Free a remote supervisor.
///
/// # Safety
///
/// `sup` must be null or a valid pointer returned by [`hew_remote_sup_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_remote_sup_free(sup: *mut HewRemoteSupervisor) {
    if sup.is_null() {
        return;
    }

    // SAFETY: pointer validity guaranteed by caller.
    let _ = unsafe { hew_remote_sup_stop(sup) };
    // SAFETY: caller transfers ownership back to this function.
    let _ = unsafe { Box::from_raw(sup) };
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    struct TestNode(*mut HewNode);

    impl TestNode {
        unsafe fn new(node_id: u16) -> Self {
            let bind = CString::new("127.0.0.1:0").expect("valid bind addr");
            // SAFETY: bind pointer is valid C string for this call.
            let node = unsafe { crate::hew_node::hew_node_new(node_id, bind.as_ptr()) };
            assert!(!node.is_null());
            // SAFETY: node pointer is valid.
            assert_eq!(unsafe { crate::hew_node::hew_node_start(node) }, 0);
            Self(node)
        }

        fn as_ptr(&self) -> *mut HewNode {
            self.0
        }
    }

    impl Drop for TestNode {
        fn drop(&mut self) {
            if self.0.is_null() {
                return;
            }
            // SAFETY: TestNode owns pointer from hew_node_new.
            unsafe { crate::hew_node::hew_node_free(self.0) };
            self.0 = ptr::null_mut();
        }
    }

    #[test]
    fn monitor_and_unmonitor() {
        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(3001) };
        let remote_node_id = 3002;
        let remote_pid = (u64::from(remote_node_id) << 48) | 7;

        // SAFETY: pointers are valid for this scope.
        unsafe {
            let sup = hew_remote_sup_new(node.as_ptr(), remote_node_id, 0);
            assert!(!sup.is_null());
            assert_eq!(hew_remote_sup_monitor(sup, remote_pid), 0);
            assert_eq!(hew_remote_sup_monitor(sup, remote_pid), -1);
            assert_eq!(hew_remote_sup_unmonitor(sup, remote_pid), 0);
            assert_eq!(hew_remote_sup_unmonitor(sup, remote_pid), -1);
            hew_remote_sup_free(sup);
        }
    }

    #[test]
    fn dead_membership_event_triggers_callback_for_monitored() {
        static CALLED: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);

        unsafe extern "C" fn on_death(_remote_pid: u64, _remote_node_id: u16, _reason: c_int) {
            CALLED.fetch_add(1, Ordering::Relaxed);
        }

        CALLED.store(0, Ordering::Relaxed);

        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(3011) };
        let remote_node_id = 3012;
        let pid1 = (u64::from(remote_node_id) << 48) | 10;
        let pid2 = (u64::from(remote_node_id) << 48) | 11;

        // SAFETY: pointers are valid for this scope.
        unsafe {
            let sup = hew_remote_sup_new(node.as_ptr(), remote_node_id, 1);
            assert!(!sup.is_null());
            hew_remote_sup_set_callback(sup, Some(on_death));
            assert_eq!(hew_remote_sup_monitor(sup, pid1), 0);
            assert_eq!(hew_remote_sup_monitor(sup, pid2), 0);
            assert_eq!(hew_remote_sup_start(sup), 0);

            remote_sup_membership_callback(
                remote_node_id,
                HEW_MEMBERSHIP_EVENT_NODE_DEAD,
                sup.cast::<c_void>(),
            );

            assert_eq!(CALLED.load(Ordering::Relaxed), 2);
            assert_eq!(hew_remote_sup_stop(sup), 0);
            hew_remote_sup_free(sup);
        }
    }
}
