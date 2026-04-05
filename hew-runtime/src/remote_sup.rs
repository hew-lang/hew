//! Remote supervision wired to unified [`crate::hew_node::HewNode`].
//!
//! This is scaffolding for remote restart orchestration. It currently monitors
//! remote PIDs, watches SWIM membership for remote node death, and invokes a
//! callback when monitored actors are considered dead.

use crate::util::{CondvarExt, MutexExt};
use std::collections::HashMap;
use std::ffi::{c_int, c_void};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex, OnceLock};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

use crate::cluster::{
    self, HEW_MEMBERSHIP_EVENT_NODE_DEAD, HEW_MEMBERSHIP_EVENT_NODE_JOINED,
    HEW_MEMBERSHIP_EVENT_NODE_SUSPECT, MEMBER_DEAD,
};
use crate::hew_node::HewNode;

const DEFAULT_HEARTBEAT_INTERVAL_MS: u64 = 1_000;
const DEFAULT_DEAD_QUARANTINE_MS: u64 = 5_000;

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

#[derive(Debug, Default)]
struct QuarantineState {
    suspect_since: Option<Instant>,
    pending_dead: bool,
    notified_dead: bool,
}

#[derive(Debug)]
struct RemoteDeathDispatch {
    callback: RemoteDeathCallback,
    remote_node_id: u16,
    monitored: Vec<u64>,
    strategy: SupervisorStrategy,
}

impl RemoteDeathDispatch {
    fn execute(self) {
        match self.strategy {
            SupervisorStrategy::OneForOne | SupervisorStrategy::OneForAll => {
                for remote_pid in self.monitored {
                    // SAFETY: callback pointer validity is guaranteed by caller contract.
                    unsafe { (self.callback)(remote_pid, self.remote_node_id, MEMBER_DEAD) };
                }
            }
        }
    }
}

#[derive(Debug)]
struct ClusterCallbackContext {
    cluster_key: usize,
    previous_callback: cluster::MembershipCallbackBinding,
    callback_epoch: Arc<cluster::MembershipCallbackEpoch>,
}

#[derive(Debug)]
struct ClusterSubscription {
    supervisors: Vec<Arc<SupervisorMembershipLease>>,
    callback_context: Arc<ClusterCallbackContext>,
    callback_user_data_bits: usize,
}

impl ClusterSubscription {
    fn callback_user_data(&self) -> *mut c_void {
        self.callback_user_data_bits as *mut c_void
    }
}

fn owned_callback_context_user_data(context: &Arc<ClusterCallbackContext>) -> *mut c_void {
    Arc::into_raw(Arc::clone(context)).cast_mut().cast()
}

fn reclaim_owned_callback_context(user_data: *mut c_void) -> Arc<ClusterCallbackContext> {
    let context_ptr = user_data.cast::<ClusterCallbackContext>();
    // SAFETY: `user_data` was produced by `owned_callback_context_user_data()`
    // and is reclaimed exactly once when its callback generation retires.
    unsafe { Arc::from_raw(context_ptr) }
}

#[derive(Debug)]
struct SupervisorMembershipLease {
    supervisor_bits: AtomicUsize,
    in_flight: AtomicUsize,
    wait_lock: Mutex<()>,
    wait_cv: Condvar,
}

impl Default for SupervisorMembershipLease {
    fn default() -> Self {
        Self {
            supervisor_bits: AtomicUsize::new(0),
            in_flight: AtomicUsize::new(0),
            wait_lock: Mutex::new(()),
            wait_cv: Condvar::new(),
        }
    }
}

impl SupervisorMembershipLease {
    fn bind_supervisor(&self, supervisor: *mut HewRemoteSupervisor) {
        self.supervisor_bits
            .store(supervisor as usize, Ordering::Release);
    }

    fn supervisor_addr(&self) -> usize {
        self.supervisor_bits.load(Ordering::Acquire)
    }

    fn acquire(self: &Arc<Self>) -> SupervisorMembershipGuard {
        self.in_flight.fetch_add(1, Ordering::AcqRel);
        SupervisorMembershipGuard {
            lease: Arc::clone(self),
        }
    }

    fn wait_for_drain(&self) {
        if self.in_flight.load(Ordering::Acquire) == 0 {
            return;
        }
        let mut wait_guard = self.wait_lock.lock_or_recover();
        while self.in_flight.load(Ordering::Acquire) != 0 {
            wait_guard = self.wait_cv.wait_or_recover(wait_guard);
        }
    }
}

#[derive(Debug)]
struct SupervisorMembershipGuard {
    lease: Arc<SupervisorMembershipLease>,
}

impl SupervisorMembershipGuard {
    fn supervisor(&self) -> Option<&HewRemoteSupervisor> {
        let supervisor_addr = self.lease.supervisor_addr();
        if supervisor_addr == 0 {
            return None;
        }
        // SAFETY: the guard increments the in-flight counter before the
        // subscription lock is released, and stop/free wait for that counter to
        // drain before releasing the supervisor allocation.
        Some(unsafe { &*(supervisor_addr as *const HewRemoteSupervisor) })
    }
}

impl Drop for SupervisorMembershipGuard {
    fn drop(&mut self) {
        if self.lease.in_flight.fetch_sub(1, Ordering::AcqRel) == 1 {
            let _wait_guard = self.lease.wait_lock.lock_or_recover();
            self.lease.wait_cv.notify_all();
        }
    }
}

fn cluster_subscriptions() -> &'static Mutex<HashMap<usize, ClusterSubscription>> {
    static SUBSCRIPTIONS: OnceLock<Mutex<HashMap<usize, ClusterSubscription>>> = OnceLock::new();
    SUBSCRIPTIONS.get_or_init(|| Mutex::new(HashMap::new()))
}

fn retired_callback_contexts() -> &'static Mutex<Vec<Arc<ClusterCallbackContext>>> {
    static RETIRED: OnceLock<Mutex<Vec<Arc<ClusterCallbackContext>>>> = OnceLock::new();
    RETIRED.get_or_init(|| Mutex::new(Vec::new()))
}

fn retain_callback_context(user_data: *mut c_void) -> Arc<ClusterCallbackContext> {
    let context_ptr = user_data.cast::<ClusterCallbackContext>();
    // SAFETY: the installed callback binding owns a strong ref for `user_data`,
    // and teardown moves that ref into `retired_callback_contexts()` until the
    // cluster reports no in-flight membership callbacks for this generation.
    unsafe {
        Arc::increment_strong_count(context_ptr);
        Arc::from_raw(context_ptr)
    }
}

fn retire_callback_context(context: Arc<ClusterCallbackContext>) {
    retired_callback_contexts().lock_or_recover().push(context);
}

fn reap_retired_callback_contexts() {
    retired_callback_contexts()
        .lock_or_recover()
        .retain(|context| Arc::strong_count(context) > 1 || context.callback_epoch.in_flight() > 0);
}

fn forward_previous_membership_callback(context: &ClusterCallbackContext, node_id: u16, event: u8) {
    if let Some(callback) = context.previous_callback.callback {
        callback(node_id, event, context.previous_callback.user_data());
    }
}

fn unregister_remote_sup_subscription(
    cluster: *mut cluster::HewCluster,
    cluster_key: usize,
    lease: &Arc<SupervisorMembershipLease>,
) {
    let retired_context = {
        let subscriptions = cluster_subscriptions();
        let mut registry = subscriptions.lock_or_recover();
        let mut retired_context = None;
        let mut previous_callback = None;
        if let Some(entry) = registry.get_mut(&cluster_key) {
            entry
                .supervisors
                .retain(|registered| !Arc::ptr_eq(registered, lease));
            if entry.supervisors.is_empty() {
                previous_callback = Some(entry.callback_context.previous_callback);
                retired_context = Some(reclaim_owned_callback_context(entry.callback_user_data()));
            }
        }
        if let Some(binding) = previous_callback {
            if !cluster.is_null() {
                // SAFETY: restore the prior callback before removing the
                // subscription entry so teardown never leaves the cluster
                // callback slot pointing at a removed remote-supervisor
                // generation.
                unsafe {
                    cluster::hew_cluster_replace_membership_callback(cluster, binding);
                }
            }
            registry.remove(&cluster_key);
        }
        retired_context
    };
    if let Some(context) = retired_context {
        retire_callback_context(context);
    }
    reap_retired_callback_contexts();
}

#[cfg(test)]
#[derive(Debug, Default)]
struct MembershipCallbackRetainPauseState {
    armed: bool,
    entered: bool,
    released: bool,
}

#[cfg(test)]
fn membership_callback_retain_pause_state() -> &'static (
    Mutex<MembershipCallbackRetainPauseState>,
    std::sync::Condvar,
) {
    static STATE: OnceLock<(
        Mutex<MembershipCallbackRetainPauseState>,
        std::sync::Condvar,
    )> = OnceLock::new();
    STATE.get_or_init(|| {
        (
            Mutex::new(MembershipCallbackRetainPauseState::default()),
            std::sync::Condvar::new(),
        )
    })
}

#[cfg(test)]
fn maybe_pause_membership_callback_before_context_retain() {
    let (state_lock, condvar) = membership_callback_retain_pause_state();
    let mut state = state_lock.lock_or_recover();
    if !state.armed || state.entered {
        return;
    }
    state.entered = true;
    condvar.notify_all();
    while !state.released {
        state = condvar.wait_or_recover(state);
    }
}

#[cfg(test)]
struct MembershipCallbackRetainPauseGuard;

#[cfg(test)]
impl MembershipCallbackRetainPauseGuard {
    fn arm() -> Self {
        let (state_lock, _) = membership_callback_retain_pause_state();
        let mut state = state_lock.lock_or_recover();
        *state = MembershipCallbackRetainPauseState {
            armed: true,
            entered: false,
            released: false,
        };
        Self
    }

    fn wait_until_entered() {
        let (state_lock, condvar) = membership_callback_retain_pause_state();
        let mut state = state_lock.lock_or_recover();
        while !state.entered {
            state = condvar.wait_or_recover(state);
        }
    }

    fn release() {
        let (state_lock, condvar) = membership_callback_retain_pause_state();
        let mut state = state_lock.lock_or_recover();
        state.released = true;
        condvar.notify_all();
    }
}

#[cfg(test)]
impl Drop for MembershipCallbackRetainPauseGuard {
    fn drop(&mut self) {
        let (state_lock, condvar) = membership_callback_retain_pause_state();
        let mut state = state_lock.lock_or_recover();
        state.armed = false;
        state.released = true;
        condvar.notify_all();
    }
}

#[cfg(test)]
#[derive(Debug, Default)]
struct MembershipCallbackPauseState {
    armed: bool,
    entered: bool,
    released: bool,
}

#[cfg(test)]
fn membership_callback_pause_state(
) -> &'static (Mutex<MembershipCallbackPauseState>, std::sync::Condvar) {
    static STATE: OnceLock<(Mutex<MembershipCallbackPauseState>, std::sync::Condvar)> =
        OnceLock::new();
    STATE.get_or_init(|| {
        (
            Mutex::new(MembershipCallbackPauseState::default()),
            std::sync::Condvar::new(),
        )
    })
}

#[cfg(test)]
fn maybe_pause_membership_callback_before_subscriptions_lock() {
    let (state_lock, condvar) = membership_callback_pause_state();
    let mut state = state_lock.lock_or_recover();
    if !state.armed || state.entered {
        return;
    }
    state.entered = true;
    condvar.notify_all();
    while !state.released {
        state = condvar.wait_or_recover(state);
    }
}

#[cfg(test)]
struct MembershipCallbackPauseGuard;

#[cfg(test)]
impl MembershipCallbackPauseGuard {
    fn arm() -> Self {
        let (state_lock, _) = membership_callback_pause_state();
        let mut state = state_lock.lock_or_recover();
        *state = MembershipCallbackPauseState {
            armed: true,
            entered: false,
            released: false,
        };
        Self
    }

    fn wait_until_entered() {
        let (state_lock, condvar) = membership_callback_pause_state();
        let mut state = state_lock.lock_or_recover();
        while !state.entered {
            state = condvar.wait_or_recover(state);
        }
    }

    fn release() {
        let (state_lock, condvar) = membership_callback_pause_state();
        let mut state = state_lock.lock_or_recover();
        state.released = true;
        condvar.notify_all();
    }
}

#[cfg(test)]
impl Drop for MembershipCallbackPauseGuard {
    fn drop(&mut self) {
        let (state_lock, condvar) = membership_callback_pause_state();
        let mut state = state_lock.lock_or_recover();
        state.armed = false;
        state.released = true;
        condvar.notify_all();
    }
}

#[cfg(test)]
#[derive(Debug, Default)]
struct MembershipCallbackGuardPauseState {
    armed: bool,
    entered: bool,
    released: bool,
}

#[cfg(test)]
fn membership_callback_guard_pause_state(
) -> &'static (Mutex<MembershipCallbackGuardPauseState>, std::sync::Condvar) {
    static STATE: OnceLock<(Mutex<MembershipCallbackGuardPauseState>, std::sync::Condvar)> =
        OnceLock::new();
    STATE.get_or_init(|| {
        (
            Mutex::new(MembershipCallbackGuardPauseState::default()),
            std::sync::Condvar::new(),
        )
    })
}

#[cfg(test)]
fn maybe_pause_membership_callback_after_supervisor_acquire() {
    let (state_lock, condvar) = membership_callback_guard_pause_state();
    let mut state = state_lock.lock_or_recover();
    if !state.armed || state.entered {
        return;
    }
    state.entered = true;
    condvar.notify_all();
    while !state.released {
        state = condvar.wait_or_recover(state);
    }
}

#[cfg(test)]
struct MembershipCallbackGuardPauseGuard;

#[cfg(test)]
impl MembershipCallbackGuardPauseGuard {
    fn arm() -> Self {
        let (state_lock, _) = membership_callback_guard_pause_state();
        let mut state = state_lock.lock_or_recover();
        *state = MembershipCallbackGuardPauseState {
            armed: true,
            entered: false,
            released: false,
        };
        Self
    }

    fn wait_until_entered() {
        let (state_lock, condvar) = membership_callback_guard_pause_state();
        let mut state = state_lock.lock_or_recover();
        while !state.entered {
            state = condvar.wait_or_recover(state);
        }
    }

    fn release() {
        let (state_lock, condvar) = membership_callback_guard_pause_state();
        let mut state = state_lock.lock_or_recover();
        state.released = true;
        condvar.notify_all();
    }
}

#[cfg(test)]
impl Drop for MembershipCallbackGuardPauseGuard {
    fn drop(&mut self) {
        let (state_lock, condvar) = membership_callback_guard_pause_state();
        let mut state = state_lock.lock_or_recover();
        state.armed = false;
        state.released = true;
        condvar.notify_all();
    }
}

/// A remote supervisor monitors actors on a remote node.
#[repr(C)]
#[derive(Debug)]
pub struct HewRemoteSupervisor {
    /// The node this supervisor is running on
    node: *mut HewNode,
    /// Remote node being supervised
    remote_node_id: u16,
    /// Actors being monitored (remote PIDs)
    monitored: Mutex<Vec<u64>>,
    /// Strategy (one-for-one, one-for-all)
    strategy: SupervisorStrategy,
    /// Heartbeat interval for liveness checks
    heartbeat_interval_ms: u64,
    dead_quarantine_ms: u64,
    callback: Mutex<Option<RemoteDeathCallback>>,
    quarantine_state: Mutex<QuarantineState>,
    running: AtomicBool,
    heartbeat_thread: Option<JoinHandle<()>>,
    membership_lease: Arc<SupervisorMembershipLease>,
}

impl HewRemoteSupervisor {
    fn remote_death_dispatch(&self) -> Option<RemoteDeathDispatch> {
        let callback = *self.callback.lock_or_recover();
        let callback = callback?;

        let monitored = self.monitored.lock_or_recover().clone();
        Some(RemoteDeathDispatch {
            callback,
            remote_node_id: self.remote_node_id,
            monitored,
            strategy: self.strategy,
        })
    }

    fn reset_quarantine_state(&self) {
        let mut state = self.quarantine_state.lock_or_recover();
        *state = QuarantineState::default();
    }

    fn process_membership_event(&self, event: u8) -> Option<RemoteDeathDispatch> {
        let now = Instant::now();
        let mut state = self.quarantine_state.lock_or_recover();
        let quarantine = Duration::from_millis(self.dead_quarantine_ms);

        let should_dispatch = match event {
            HEW_MEMBERSHIP_EVENT_NODE_JOINED => {
                *state = QuarantineState::default();
                false
            }
            HEW_MEMBERSHIP_EVENT_NODE_SUSPECT => {
                if state.suspect_since.is_none() {
                    state.suspect_since = Some(now);
                }
                state.pending_dead = true;
                false
            }
            HEW_MEMBERSHIP_EVENT_NODE_DEAD => {
                if state.notified_dead {
                    false
                } else {
                    let suspect_since = if let Some(ts) = state.suspect_since {
                        ts
                    } else {
                        state.suspect_since = Some(now);
                        now
                    };
                    state.pending_dead = true;
                    if now.duration_since(suspect_since) >= quarantine {
                        state.pending_dead = false;
                        state.notified_dead = true;
                        true
                    } else {
                        false
                    }
                }
            }
            _ => false,
        };
        drop(state);

        if should_dispatch {
            self.remote_death_dispatch()
        } else {
            None
        }
    }

    fn poll_quarantine(&self) -> Option<RemoteDeathDispatch> {
        let now = Instant::now();
        let quarantine = Duration::from_millis(self.dead_quarantine_ms);
        let mut state = self.quarantine_state.lock_or_recover();

        if !state.pending_dead || state.notified_dead {
            return None;
        }

        let Some(suspect_since) = state.suspect_since else {
            state.suspect_since = Some(now);
            return None;
        };

        if now.duration_since(suspect_since) < quarantine {
            return None;
        }

        state.pending_dead = false;
        state.notified_dead = true;
        drop(state);
        self.remote_death_dispatch()
    }
}

extern "C" fn remote_sup_membership_callback(node_id: u16, event: u8, user_data: *mut c_void) {
    if user_data.is_null() {
        return;
    }

    #[cfg(test)]
    maybe_pause_membership_callback_before_context_retain();
    let callback_context = retain_callback_context(user_data);
    #[cfg(test)]
    maybe_pause_membership_callback_before_subscriptions_lock();
    // Forward the previously installed callback before leasing supervisors so a
    // reentrant stop/free can unregister the same supervisor without waiting on
    // this callback's lease to drain.
    forward_previous_membership_callback(&callback_context, node_id, event);
    let supervisor_guards = {
        let subscriptions = cluster_subscriptions();
        let registry = subscriptions.lock_or_recover();
        registry
            .get(&callback_context.cluster_key)
            .and_then(|subscription| {
                Arc::ptr_eq(&subscription.callback_context, &callback_context).then(|| {
                    subscription
                        .supervisors
                        .iter()
                        .map(SupervisorMembershipLease::acquire)
                        .collect::<Vec<_>>()
                })
            })
            .unwrap_or_default()
    };
    #[cfg(test)]
    maybe_pause_membership_callback_after_supervisor_acquire();

    let mut dispatches = Vec::new();
    for guard in supervisor_guards {
        let Some(sup) = guard.supervisor() else {
            continue;
        };
        if !sup.running.load(Ordering::Acquire) || node_id != sup.remote_node_id {
            continue;
        }
        if let Some(dispatch) = sup.process_membership_event(event) {
            dispatches.push(dispatch);
        }
    }
    for dispatch in dispatches {
        dispatch.execute();
    }
    drop(callback_context);
    reap_retired_callback_contexts();
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

    let mut sup = Box::new(HewRemoteSupervisor {
        node,
        remote_node_id,
        monitored: Mutex::new(Vec::new()),
        strategy,
        heartbeat_interval_ms: DEFAULT_HEARTBEAT_INTERVAL_MS,
        dead_quarantine_ms: DEFAULT_DEAD_QUARANTINE_MS,
        callback: Mutex::new(None),
        quarantine_state: Mutex::new(QuarantineState::default()),
        running: AtomicBool::new(false),
        heartbeat_thread: None,
        membership_lease: Arc::new(SupervisorMembershipLease::default()),
    });
    let sup_ptr = ptr::from_mut::<HewRemoteSupervisor>(&mut *sup);
    sup.membership_lease.bind_supervisor(sup_ptr);
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
    let sup = unsafe { &*sup };
    let pid_node = crate::pid::hew_pid_node(remote_pid);
    if pid_node != 0 && pid_node != sup.remote_node_id {
        return -1;
    }
    let mut monitored = sup.monitored.lock_or_recover();
    if monitored.contains(&remote_pid) {
        return -1;
    }

    monitored.push(remote_pid);
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
    let sup = unsafe { &*sup };
    let mut monitored = sup.monitored.lock_or_recover();
    let Some(idx) = monitored.iter().position(|pid| *pid == remote_pid) else {
        return -1;
    };

    monitored.swap_remove(idx);
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
    sup.reset_quarantine_state();

    // SAFETY: `node` is validated by constructor contract.
    let node = unsafe { &mut *sup.node };
    if node.cluster.is_null() {
        sup.running.store(false, Ordering::Release);
        return -1;
    }

    let sup_ptr = ptr::from_mut::<HewRemoteSupervisor>(sup);
    let sup_addr = sup_ptr as usize;
    let cluster_key = node.cluster as usize;
    sup.membership_lease.bind_supervisor(sup_ptr);
    {
        let subscriptions = cluster_subscriptions();
        let mut registry = subscriptions.lock_or_recover();
        if let Some(entry) = registry.get_mut(&cluster_key) {
            entry.supervisors.push(Arc::clone(&sup.membership_lease));
        } else {
            let callback_context = Arc::new(ClusterCallbackContext {
                cluster_key,
                // SAFETY: cluster pointer is valid while node is alive.
                previous_callback: unsafe {
                    cluster::hew_cluster_membership_callback_binding(node.cluster)
                },
                // SAFETY: cluster pointer is valid while node is alive.
                callback_epoch: unsafe {
                    cluster::hew_cluster_membership_callback_epoch(node.cluster)
                },
            });
            let callback_user_data = owned_callback_context_user_data(&callback_context);
            // SAFETY: cluster pointer is valid while node is alive.
            unsafe {
                cluster::hew_cluster_replace_membership_callback(
                    node.cluster,
                    cluster::MembershipCallbackBinding::new(
                        Some(remote_sup_membership_callback),
                        callback_user_data,
                    ),
                );
            }
            registry.insert(
                cluster_key,
                ClusterSubscription {
                    supervisors: vec![Arc::clone(&sup.membership_lease)],
                    callback_context,
                    callback_user_data_bits: callback_user_data as usize,
                },
            );
        }
    }
    reap_retired_callback_contexts();

    let interval_ms = sup.heartbeat_interval_ms.max(10);
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
                // SAFETY: sup_ptr remains valid until stop joins this thread.
                if let Some(dispatch) = unsafe { (*sup_ptr).poll_quarantine() } {
                    dispatch.execute();
                }
                thread::sleep(Duration::from_millis(interval_ms));
            }
        });

    if let Ok(h) = handle {
        sup.heartbeat_thread = Some(h);
        0
    } else {
        sup.running.store(false, Ordering::Release);
        unregister_remote_sup_subscription(node.cluster, cluster_key, &sup.membership_lease);
        sup.membership_lease.wait_for_drain();
        -1
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
    sup.running.store(false, Ordering::Release);

    // SAFETY: `node` is valid while supervisor lives.
    let node = unsafe { &mut *sup.node };
    let cluster_key = node.cluster as usize;
    if let Some(handle) = sup.heartbeat_thread.take() {
        let _ = handle.join();
    }

    unregister_remote_sup_subscription(node.cluster, cluster_key, &sup.membership_lease);
    sup.membership_lease.wait_for_drain();

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
    let sup = unsafe { &*sup };
    *sup.callback.lock_or_recover() = callback;
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
    // SAFETY: pointer validity guaranteed by caller until ownership is reclaimed below.
    unsafe { (&*sup).membership_lease.bind_supervisor(ptr::null_mut()) };
    // SAFETY: caller transfers ownership back to this function.
    let _ = unsafe { Box::from_raw(sup) };
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    fn remote_sup_test_lock() -> &'static Mutex<()> {
        static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        LOCK.get_or_init(|| Mutex::new(()))
    }

    fn reset_membership_callback_pause_states() {
        let (pause_lock, pause_condvar) = membership_callback_pause_state();
        *pause_lock.lock_or_recover() = MembershipCallbackPauseState::default();
        pause_condvar.notify_all();

        let (guard_lock, guard_condvar) = membership_callback_guard_pause_state();
        *guard_lock.lock_or_recover() = MembershipCallbackGuardPauseState::default();
        guard_condvar.notify_all();

        let (retain_lock, retain_condvar) = membership_callback_retain_pause_state();
        *retain_lock.lock_or_recover() = MembershipCallbackRetainPauseState::default();
        retain_condvar.notify_all();
    }

    /// Serialize tests that spin up real nodes because the runtime exposes a
    /// single process-global active node slot.
    struct RemoteSupTestGuard {
        _lock_guard: std::sync::MutexGuard<'static, ()>,
    }

    impl RemoteSupTestGuard {
        fn acquire() -> Self {
            let guard = remote_sup_test_lock().lock_or_recover();
            reset_membership_callback_pause_states();
            reap_retired_callback_contexts();
            assert!(
                cluster_subscriptions().lock_or_recover().is_empty(),
                "remote supervisor subscriptions should be empty before each test"
            );
            assert!(
                retired_callback_contexts().lock_or_recover().is_empty(),
                "retired callback contexts should be empty before each test"
            );
            Self { _lock_guard: guard }
        }
    }

    impl Drop for RemoteSupTestGuard {
        fn drop(&mut self) {
            reset_membership_callback_pause_states();
            reap_retired_callback_contexts();
            assert!(
                cluster_subscriptions().lock_or_recover().is_empty(),
                "remote supervisor subscriptions should be empty after each test"
            );
            assert!(
                retired_callback_contexts().lock_or_recover().is_empty(),
                "retired callback contexts should be empty after each test"
            );
        }
    }

    struct TestNode {
        ptr: *mut HewNode,
        _guard: RemoteSupTestGuard,
    }

    impl TestNode {
        unsafe fn new(node_id: u16) -> Self {
            let guard = RemoteSupTestGuard::acquire();
            let bind = CString::new("127.0.0.1:0").expect("valid bind addr");
            // SAFETY: bind pointer is valid C string for this call.
            let node = unsafe { crate::hew_node::hew_node_new(node_id, bind.as_ptr()) };
            assert!(!node.is_null());
            // SAFETY: node pointer is valid.
            assert_eq!(unsafe { crate::hew_node::hew_node_start(node) }, 0);
            Self {
                ptr: node,
                _guard: guard,
            }
        }

        fn as_ptr(&self) -> *mut HewNode {
            self.ptr
        }
    }

    impl Drop for TestNode {
        fn drop(&mut self) {
            if self.ptr.is_null() {
                return;
            }
            // SAFETY: TestNode owns pointer from hew_node_new.
            unsafe { crate::hew_node::hew_node_free(self.ptr) };
            self.ptr = ptr::null_mut();
        }
    }

    #[test]
    fn monitor_and_unmonitor() {
        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(3001) };
        let remote_node_id = 3002;
        let remote_pid = (u64::from(remote_node_id) << 48) | 0x0007;

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
        let pid1 = (u64::from(remote_node_id) << 48) | 0x0A;
        let pid2 = (u64::from(remote_node_id) << 48) | 0x0B;

        // SAFETY: pointers are valid for this scope.
        unsafe {
            let sup = hew_remote_sup_new(node.as_ptr(), remote_node_id, 1);
            assert!(!sup.is_null());
            (*sup).dead_quarantine_ms = 0;
            hew_remote_sup_set_callback(sup, Some(on_death));
            assert_eq!(hew_remote_sup_monitor(sup, pid1), 0);
            assert_eq!(hew_remote_sup_monitor(sup, pid2), 0);
            assert_eq!(hew_remote_sup_start(sup), 0);

            // SAFETY: `node` and cluster pointer are valid in this scope.
            cluster::hew_cluster_test_fire_membership_callback(
                (*node.as_ptr()).cluster,
                remote_node_id,
                HEW_MEMBERSHIP_EVENT_NODE_DEAD,
            );

            assert_eq!(CALLED.load(Ordering::Relaxed), 2);
            assert_eq!(hew_remote_sup_stop(sup), 0);
            hew_remote_sup_free(sup);
        }
    }

    #[test]
    fn remote_supervision_preserves_registry_pruning_callbacks() {
        static CALLED: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);

        unsafe extern "C" fn on_death(_remote_pid: u64, _remote_node_id: u16, _reason: c_int) {
            CALLED.fetch_add(1, Ordering::Relaxed);
        }

        CALLED.store(0, Ordering::Relaxed);

        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(3013) };
        let remote_node_id = 3014;
        let remote_pid = (u64::from(remote_node_id) << 48) | 0x21;
        let active_name = CString::new("remote-sup-active-registry-cleanup").expect("valid name");
        let restored_name =
            CString::new("remote-sup-restored-registry-cleanup").expect("valid name");

        // SAFETY: pointers are valid for this scope.
        unsafe {
            let cluster = &*(*node.as_ptr()).cluster;
            cluster.apply_registry_event(active_name.to_str().expect("utf8"), remote_pid, true);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), active_name.as_ptr()),
                remote_pid
            );

            let sup = hew_remote_sup_new(node.as_ptr(), remote_node_id, 0);
            assert!(!sup.is_null());
            (*sup).dead_quarantine_ms = 0;
            hew_remote_sup_set_callback(sup, Some(on_death));
            assert_eq!(hew_remote_sup_monitor(sup, remote_pid), 0);
            assert_eq!(hew_remote_sup_start(sup), 0);

            cluster::hew_cluster_test_fire_membership_callback(
                (*node.as_ptr()).cluster,
                remote_node_id,
                HEW_MEMBERSHIP_EVENT_NODE_DEAD,
            );
            assert_eq!(CALLED.load(Ordering::Relaxed), 1);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), active_name.as_ptr()),
                0
            );

            cluster.apply_registry_event(restored_name.to_str().expect("utf8"), remote_pid, true);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), restored_name.as_ptr()),
                remote_pid
            );

            assert_eq!(hew_remote_sup_stop(sup), 0);

            cluster::hew_cluster_test_fire_membership_callback(
                (*node.as_ptr()).cluster,
                remote_node_id,
                HEW_MEMBERSHIP_EVENT_NODE_DEAD,
            );
            assert_eq!(CALLED.load(Ordering::Relaxed), 1);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), restored_name.as_ptr()),
                0
            );

            hew_remote_sup_free(sup);
        }
    }

    #[test]
    fn stop_window_membership_events_still_prune_remote_names() {
        static CALLED: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);

        unsafe extern "C" fn on_death(_remote_pid: u64, _remote_node_id: u16, _reason: c_int) {
            CALLED.fetch_add(1, Ordering::Relaxed);
        }

        CALLED.store(0, Ordering::Relaxed);

        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(3015) };
        let remote_node_id = 3016;
        let remote_pid = (u64::from(remote_node_id) << 48) | 0x31;
        let shutdown_name =
            CString::new("remote-sup-stop-window-registry-cleanup").expect("valid name");
        let restored_name =
            CString::new("remote-sup-stop-window-restored-cleanup").expect("valid name");

        // SAFETY: pointers are valid for this scope.
        unsafe {
            let cluster_ptr = (*node.as_ptr()).cluster;
            let cluster = &*cluster_ptr;
            cluster.apply_registry_event(shutdown_name.to_str().expect("utf8"), remote_pid, true);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), shutdown_name.as_ptr()),
                remote_pid
            );

            let sup = hew_remote_sup_new(node.as_ptr(), remote_node_id, 0);
            assert!(!sup.is_null());
            (*sup).dead_quarantine_ms = 0;
            (*sup).heartbeat_interval_ms = 500;
            hew_remote_sup_set_callback(sup, Some(on_death));
            assert_eq!(hew_remote_sup_monitor(sup, remote_pid), 0);
            assert_eq!(hew_remote_sup_start(sup), 0);

            // Give the heartbeat thread time to enter its sleep so stop() must
            // join it before restoring the callback.
            thread::sleep(Duration::from_millis(100));

            let sup_addr = sup as usize;
            let cluster_addr = cluster_ptr as usize;
            let (stop_tx, stop_rx) = std::sync::mpsc::channel();
            let stop_thread = thread::spawn(move || {
                let rc = hew_remote_sup_stop(sup_addr as *mut HewRemoteSupervisor);
                stop_tx.send(rc).expect("stop result");
            });

            while (*(sup_addr as *mut HewRemoteSupervisor))
                .running
                .load(Ordering::Acquire)
            {
                thread::yield_now();
            }
            assert!(
                stop_rx.try_recv().is_err(),
                "stop should still be waiting on the heartbeat thread"
            );

            cluster::hew_cluster_test_fire_membership_callback(
                cluster_addr as *mut cluster::HewCluster,
                remote_node_id,
                HEW_MEMBERSHIP_EVENT_NODE_DEAD,
            );

            assert_eq!(
                stop_rx
                    .recv_timeout(Duration::from_secs(2))
                    .expect("stop result"),
                0
            );
            stop_thread.join().expect("stop thread");

            assert_eq!(CALLED.load(Ordering::Relaxed), 0);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), shutdown_name.as_ptr()),
                0
            );

            cluster.apply_registry_event(restored_name.to_str().expect("utf8"), remote_pid, true);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), restored_name.as_ptr()),
                remote_pid
            );
            cluster::hew_cluster_test_fire_membership_callback(
                cluster_ptr,
                remote_node_id,
                HEW_MEMBERSHIP_EVENT_NODE_DEAD,
            );
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), restored_name.as_ptr()),
                0
            );
            assert_eq!(CALLED.load(Ordering::Relaxed), 0);

            hew_remote_sup_free(sup);
        }
    }

    #[test]
    fn blocked_inflight_membership_callback_still_forwards_pruning_after_stop() {
        static CALLED: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);

        unsafe extern "C" fn on_death(_remote_pid: u64, _remote_node_id: u16, _reason: c_int) {
            CALLED.fetch_add(1, Ordering::Relaxed);
        }

        CALLED.store(0, Ordering::Relaxed);

        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(3017) };
        let remote_node_id = 3018;
        let remote_pid = (u64::from(remote_node_id) << 48) | 0x41;
        let blocked_name =
            CString::new("remote-sup-blocked-callback-registry-cleanup").expect("valid name");
        let restored_name =
            CString::new("remote-sup-blocked-callback-restored-cleanup").expect("valid name");

        // SAFETY: pointers are valid for this scope.
        unsafe {
            let cluster_ptr = (*node.as_ptr()).cluster;
            let cluster = &*cluster_ptr;
            cluster.apply_registry_event(blocked_name.to_str().expect("utf8"), remote_pid, true);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), blocked_name.as_ptr()),
                remote_pid
            );

            let sup = hew_remote_sup_new(node.as_ptr(), remote_node_id, 0);
            assert!(!sup.is_null());
            (*sup).dead_quarantine_ms = 0;
            (*sup).heartbeat_interval_ms = 10;
            hew_remote_sup_set_callback(sup, Some(on_death));
            assert_eq!(hew_remote_sup_monitor(sup, remote_pid), 0);
            assert_eq!(hew_remote_sup_start(sup), 0);
            // Let the heartbeat thread finish its initial tick and enter sleep
            // so this test's manual callback owns the pause window.
            thread::sleep(Duration::from_millis(20));

            let pause = MembershipCallbackPauseGuard::arm();
            let callback_cluster = cluster_ptr as usize;
            let callback_thread = thread::spawn(move || {
                cluster::hew_cluster_test_fire_membership_callback(
                    callback_cluster as *mut cluster::HewCluster,
                    remote_node_id,
                    HEW_MEMBERSHIP_EVENT_NODE_DEAD,
                );
            });

            MembershipCallbackPauseGuard::wait_until_entered();
            assert_eq!(hew_remote_sup_stop(sup), 0);
            MembershipCallbackPauseGuard::release();
            callback_thread.join().expect("callback thread");
            drop(pause);

            assert_eq!(CALLED.load(Ordering::Relaxed), 0);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), blocked_name.as_ptr()),
                0
            );

            cluster.apply_registry_event(restored_name.to_str().expect("utf8"), remote_pid, true);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), restored_name.as_ptr()),
                remote_pid
            );
            cluster::hew_cluster_test_fire_membership_callback(
                cluster_ptr,
                remote_node_id,
                HEW_MEMBERSHIP_EVENT_NODE_DEAD,
            );
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), restored_name.as_ptr()),
                0
            );
            assert_eq!(CALLED.load(Ordering::Relaxed), 0);

            hew_remote_sup_free(sup);
        }
    }

    #[test]
    fn stale_inflight_membership_callback_does_not_retarget_restarted_supervisor() {
        static OLD_CALLED: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);
        static NEW_CALLED: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);

        unsafe extern "C" fn on_old_death(_remote_pid: u64, _remote_node_id: u16, _reason: c_int) {
            OLD_CALLED.fetch_add(1, Ordering::Relaxed);
        }

        unsafe extern "C" fn on_new_death(_remote_pid: u64, _remote_node_id: u16, _reason: c_int) {
            NEW_CALLED.fetch_add(1, Ordering::Relaxed);
        }

        OLD_CALLED.store(0, Ordering::Relaxed);
        NEW_CALLED.store(0, Ordering::Relaxed);

        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(4019) };
        let remote_node_id = 4020;
        let remote_pid = (u64::from(remote_node_id) << 48) | 0x51;
        let stale_name =
            CString::new("remote-sup-restart-old-generation-cleanup").expect("valid name");
        let restarted_name =
            CString::new("remote-sup-restart-new-generation-cleanup").expect("valid name");
        let fresh_name =
            CString::new("remote-sup-restart-fresh-generation-cleanup").expect("valid name");

        // SAFETY: pointers are valid for this scope.
        unsafe {
            let cluster_ptr = (*node.as_ptr()).cluster;
            let cluster = &*cluster_ptr;
            cluster.apply_registry_event(stale_name.to_str().expect("utf8"), remote_pid, true);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), stale_name.as_ptr()),
                remote_pid
            );

            let old_sup = hew_remote_sup_new(node.as_ptr(), remote_node_id, 0);
            assert!(!old_sup.is_null());
            (*old_sup).dead_quarantine_ms = 0;
            (*old_sup).heartbeat_interval_ms = 10;
            hew_remote_sup_set_callback(old_sup, Some(on_old_death));
            assert_eq!(hew_remote_sup_monitor(old_sup, remote_pid), 0);
            assert_eq!(hew_remote_sup_start(old_sup), 0);
            // Let the heartbeat thread finish its initial tick and enter sleep
            // so this test's manual callback owns the pause window.
            thread::sleep(Duration::from_millis(20));

            let pause = MembershipCallbackPauseGuard::arm();
            let callback_cluster = cluster_ptr as usize;
            let callback_thread = thread::spawn(move || {
                cluster::hew_cluster_test_fire_membership_callback(
                    callback_cluster as *mut cluster::HewCluster,
                    remote_node_id,
                    HEW_MEMBERSHIP_EVENT_NODE_DEAD,
                );
            });

            MembershipCallbackPauseGuard::wait_until_entered();
            assert_eq!(hew_remote_sup_stop(old_sup), 0);

            let new_sup = hew_remote_sup_new(node.as_ptr(), remote_node_id, 0);
            assert!(!new_sup.is_null());
            (*new_sup).dead_quarantine_ms = 0;
            (*new_sup).heartbeat_interval_ms = 10;
            hew_remote_sup_set_callback(new_sup, Some(on_new_death));
            assert_eq!(hew_remote_sup_monitor(new_sup, remote_pid), 0);
            assert_eq!(hew_remote_sup_start(new_sup), 0);

            cluster.apply_registry_event(restarted_name.to_str().expect("utf8"), remote_pid, true);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), restarted_name.as_ptr()),
                remote_pid
            );

            MembershipCallbackPauseGuard::release();
            callback_thread.join().expect("callback thread");
            drop(pause);

            assert_eq!(OLD_CALLED.load(Ordering::Relaxed), 0);
            assert_eq!(NEW_CALLED.load(Ordering::Relaxed), 0);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), stale_name.as_ptr()),
                0
            );
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), restarted_name.as_ptr()),
                0
            );

            cluster.apply_registry_event(fresh_name.to_str().expect("utf8"), remote_pid, true);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), fresh_name.as_ptr()),
                remote_pid
            );
            cluster::hew_cluster_test_fire_membership_callback(
                cluster_ptr,
                remote_node_id,
                HEW_MEMBERSHIP_EVENT_NODE_DEAD,
            );
            assert_eq!(OLD_CALLED.load(Ordering::Relaxed), 0);
            assert_eq!(NEW_CALLED.load(Ordering::Relaxed), 1);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), fresh_name.as_ptr()),
                0
            );

            assert_eq!(hew_remote_sup_stop(new_sup), 0);
            hew_remote_sup_free(new_sup);
            hew_remote_sup_free(old_sup);
        }
    }

    #[test]
    fn pre_retain_inflight_membership_callback_survives_stop_and_restart() {
        static OLD_CALLED: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);
        static NEW_CALLED: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);

        unsafe extern "C" fn on_old_death(_remote_pid: u64, _remote_node_id: u16, _reason: c_int) {
            OLD_CALLED.fetch_add(1, Ordering::Relaxed);
        }

        unsafe extern "C" fn on_new_death(_remote_pid: u64, _remote_node_id: u16, _reason: c_int) {
            NEW_CALLED.fetch_add(1, Ordering::Relaxed);
        }

        OLD_CALLED.store(0, Ordering::Relaxed);
        NEW_CALLED.store(0, Ordering::Relaxed);

        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(4021) };
        let remote_node_id = 4022;
        let remote_pid = (u64::from(remote_node_id) << 48) | 0x61;
        let stale_name =
            CString::new("remote-sup-preretain-old-generation-cleanup").expect("valid name");
        let restarted_name =
            CString::new("remote-sup-preretain-new-generation-cleanup").expect("valid name");
        let fresh_name =
            CString::new("remote-sup-preretain-fresh-generation-cleanup").expect("valid name");

        // SAFETY: pointers are valid for this scope.
        unsafe {
            let cluster_ptr = (*node.as_ptr()).cluster;
            let cluster = &*cluster_ptr;
            cluster.apply_registry_event(stale_name.to_str().expect("utf8"), remote_pid, true);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), stale_name.as_ptr()),
                remote_pid
            );

            let old_sup = hew_remote_sup_new(node.as_ptr(), remote_node_id, 0);
            assert!(!old_sup.is_null());
            (*old_sup).dead_quarantine_ms = 0;
            (*old_sup).heartbeat_interval_ms = 10;
            hew_remote_sup_set_callback(old_sup, Some(on_old_death));
            assert_eq!(hew_remote_sup_monitor(old_sup, remote_pid), 0);
            assert_eq!(hew_remote_sup_start(old_sup), 0);
            // Let the heartbeat thread finish its initial tick and enter sleep
            // so this test's manual callback owns the pause window.
            thread::sleep(Duration::from_millis(20));

            let pause = MembershipCallbackRetainPauseGuard::arm();
            let callback_cluster = cluster_ptr as usize;
            let callback_thread = thread::spawn(move || {
                cluster::hew_cluster_test_fire_membership_callback(
                    callback_cluster as *mut cluster::HewCluster,
                    remote_node_id,
                    HEW_MEMBERSHIP_EVENT_NODE_DEAD,
                );
            });

            MembershipCallbackRetainPauseGuard::wait_until_entered();
            assert_eq!(hew_remote_sup_stop(old_sup), 0);

            let new_sup = hew_remote_sup_new(node.as_ptr(), remote_node_id, 0);
            assert!(!new_sup.is_null());
            (*new_sup).dead_quarantine_ms = 0;
            (*new_sup).heartbeat_interval_ms = 10;
            hew_remote_sup_set_callback(new_sup, Some(on_new_death));
            assert_eq!(hew_remote_sup_monitor(new_sup, remote_pid), 0);
            assert_eq!(hew_remote_sup_start(new_sup), 0);

            cluster.apply_registry_event(restarted_name.to_str().expect("utf8"), remote_pid, true);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), restarted_name.as_ptr()),
                remote_pid
            );

            MembershipCallbackRetainPauseGuard::release();
            callback_thread.join().expect("callback thread");
            drop(pause);

            assert_eq!(OLD_CALLED.load(Ordering::Relaxed), 0);
            assert_eq!(NEW_CALLED.load(Ordering::Relaxed), 0);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), stale_name.as_ptr()),
                0
            );
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), restarted_name.as_ptr()),
                0
            );

            cluster.apply_registry_event(fresh_name.to_str().expect("utf8"), remote_pid, true);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), fresh_name.as_ptr()),
                remote_pid
            );
            cluster::hew_cluster_test_fire_membership_callback(
                cluster_ptr,
                remote_node_id,
                HEW_MEMBERSHIP_EVENT_NODE_DEAD,
            );
            assert_eq!(OLD_CALLED.load(Ordering::Relaxed), 0);
            assert_eq!(NEW_CALLED.load(Ordering::Relaxed), 1);
            assert_eq!(
                crate::hew_node::hew_node_lookup(node.as_ptr(), fresh_name.as_ptr()),
                0
            );

            assert_eq!(hew_remote_sup_stop(new_sup), 0);
            hew_remote_sup_free(new_sup);
            hew_remote_sup_free(old_sup);
        }
    }

    #[test]
    fn free_waits_for_inflight_non_heartbeat_membership_callback() {
        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(4023) };
        let remote_node_id = 4024;
        let remote_pid = (u64::from(remote_node_id) << 48) | 0x71;
        let remote_addr = CString::new("127.0.0.1:4024").expect("valid addr");

        // SAFETY: pointers are valid for this scope.
        unsafe {
            let cluster_ptr = (*node.as_ptr()).cluster;
            assert_eq!(
                cluster::hew_cluster_join(cluster_ptr, remote_node_id, remote_addr.as_ptr()),
                0
            );

            let sup = hew_remote_sup_new(node.as_ptr(), remote_node_id, 0);
            assert!(!sup.is_null());
            (*sup).heartbeat_interval_ms = 10;
            assert_eq!(hew_remote_sup_monitor(sup, remote_pid), 0);
            assert_eq!(hew_remote_sup_start(sup), 0);
            // Let the heartbeat thread finish its initial tick and enter sleep
            // so this test's connection-lost callback owns the pause window.
            thread::sleep(Duration::from_millis(20));

            let pause = MembershipCallbackGuardPauseGuard::arm();
            let callback_cluster = cluster_ptr as usize;
            let callback_thread = thread::spawn(move || {
                // SAFETY: test keeps the cluster alive until the callback returns.
                assert_eq!(
                    cluster::hew_cluster_notify_connection_lost(
                        callback_cluster as *mut cluster::HewCluster,
                        remote_node_id,
                    ),
                    0
                );
            });

            MembershipCallbackGuardPauseGuard::wait_until_entered();

            let sup_addr = sup as usize;
            let (free_tx, free_rx) = std::sync::mpsc::channel();
            let free_thread = thread::spawn(move || {
                // SAFETY: ownership transfers to the free thread for this call.
                hew_remote_sup_free(sup_addr as *mut HewRemoteSupervisor);
                free_tx.send(()).expect("free completion");
            });

            assert!(
                free_rx.recv_timeout(Duration::from_millis(50)).is_err(),
                "free should wait for the in-flight connection callback"
            );

            MembershipCallbackGuardPauseGuard::release();
            callback_thread.join().expect("callback thread");
            free_rx
                .recv_timeout(Duration::from_secs(2))
                .expect("free completion");
            free_thread.join().expect("free thread");
            drop(pause);
        }
    }

    #[test]
    fn forwarded_callback_can_reentrantly_stop_and_free_supervisor() {
        struct ReentrantFreeContext {
            sup_bits: AtomicUsize,
            frees: std::sync::atomic::AtomicI32,
        }

        extern "C" fn free_supervisor_from_forwarded_callback(
            _node_id: u16,
            _event: u8,
            user_data: *mut c_void,
        ) {
            if user_data.is_null() {
                return;
            }
            // SAFETY: test installs a valid pointer to `ReentrantFreeContext`
            // for the duration of the callback.
            let context = unsafe { &*user_data.cast::<ReentrantFreeContext>() };
            context.frees.fetch_add(1, Ordering::Relaxed);
            let sup_addr = context.sup_bits.swap(0, Ordering::AcqRel);
            if sup_addr != 0 {
                // SAFETY: callback consumes the live supervisor exactly once.
                unsafe { hew_remote_sup_free(sup_addr as *mut HewRemoteSupervisor) };
            }
        }

        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(4025) };
        let remote_node_id = 4026;

        // SAFETY: pointers are valid for this scope.
        unsafe {
            let cluster_ptr = (*node.as_ptr()).cluster;
            let original_binding = cluster::hew_cluster_membership_callback_binding(cluster_ptr);
            let sup = hew_remote_sup_new(node.as_ptr(), remote_node_id, 0);
            assert!(!sup.is_null());
            (*sup).heartbeat_interval_ms = 500;

            let context = ReentrantFreeContext {
                sup_bits: AtomicUsize::new(sup as usize),
                frees: std::sync::atomic::AtomicI32::new(0),
            };
            cluster::hew_cluster_replace_membership_callback(
                cluster_ptr,
                cluster::MembershipCallbackBinding::new(
                    Some(free_supervisor_from_forwarded_callback),
                    (&raw const context).cast_mut().cast(),
                ),
            );

            assert_eq!(hew_remote_sup_start(sup), 0);
            // Let the heartbeat thread finish its initial tick and enter sleep
            // so the forwarded callback owns the reentrant free path.
            thread::sleep(Duration::from_millis(100));

            let callback_cluster = cluster_ptr as usize;
            let (done_tx, done_rx) = std::sync::mpsc::channel();
            let callback_thread = thread::spawn(move || {
                // SAFETY: test keeps the cluster alive until the callback returns.
                cluster::hew_cluster_test_fire_membership_callback(
                    callback_cluster as *mut cluster::HewCluster,
                    remote_node_id,
                    HEW_MEMBERSHIP_EVENT_NODE_DEAD,
                );
                done_tx.send(()).expect("callback completion");
            });

            done_rx
                .recv_timeout(Duration::from_secs(2))
                .expect("reentrant callback completion");
            callback_thread.join().expect("callback thread");
            assert_eq!(context.frees.load(Ordering::Relaxed), 1);
            assert_eq!(context.sup_bits.load(Ordering::Acquire), 0);

            cluster::hew_cluster_replace_membership_callback(cluster_ptr, original_binding);
        }
    }

    // ── Helper for internal-method tests ────────────────────────────────
    //
    // process_membership_event / poll_quarantine only touch monitored,
    // callback, quarantine_state, strategy, and dead_quarantine_ms — they
    // never dereference `node`, so a null pointer is safe here.

    unsafe extern "C" fn noop_death_cb(_pid: u64, _node: u16, _reason: c_int) {}

    fn bare_supervisor(
        remote_node_id: u16,
        strategy: SupervisorStrategy,
        dead_quarantine_ms: u64,
    ) -> HewRemoteSupervisor {
        HewRemoteSupervisor {
            node: ptr::null_mut(),
            remote_node_id,
            monitored: Mutex::new(Vec::new()),
            strategy,
            heartbeat_interval_ms: DEFAULT_HEARTBEAT_INTERVAL_MS,
            dead_quarantine_ms,
            callback: Mutex::new(None),
            quarantine_state: Mutex::new(QuarantineState::default()),
            running: AtomicBool::new(false),
            heartbeat_thread: None,
            membership_lease: Arc::new(SupervisorMembershipLease::default()),
        }
    }

    // ── SupervisorStrategy ─────────────────────────────────────────────

    #[test]
    fn strategy_from_valid_values_returns_variant() {
        assert_eq!(
            SupervisorStrategy::from_c_int(0),
            Some(SupervisorStrategy::OneForOne)
        );
        assert_eq!(
            SupervisorStrategy::from_c_int(1),
            Some(SupervisorStrategy::OneForAll)
        );
    }

    #[test]
    fn strategy_from_invalid_values_returns_none() {
        assert_eq!(SupervisorStrategy::from_c_int(-1), None);
        assert_eq!(SupervisorStrategy::from_c_int(2), None);
        assert_eq!(SupervisorStrategy::from_c_int(i32::MAX), None);
    }

    // ── hew_remote_sup_new null/invalid guards ─────────────────────────

    #[test]
    fn new_null_node_returns_null() {
        // SAFETY: testing null guard — no real node needed.
        unsafe {
            let sup = hew_remote_sup_new(ptr::null_mut(), 1, 0);
            assert!(sup.is_null());
        }
    }

    #[test]
    fn new_zero_remote_node_id_returns_null() {
        // SAFETY: null node, but node_id == 0 guard fires first.
        unsafe {
            let sup = hew_remote_sup_new(ptr::null_mut(), 0, 0);
            assert!(sup.is_null());
        }
    }

    #[test]
    fn new_invalid_strategy_returns_null() {
        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(3020) };
        // SAFETY: pointers are valid for this scope.
        unsafe {
            let sup = hew_remote_sup_new(node.as_ptr(), 1, 99);
            assert!(sup.is_null());
        }
    }

    #[test]
    fn new_one_for_all_strategy_accepted() {
        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(3021) };
        // SAFETY: pointers are valid for this scope.
        unsafe {
            let sup = hew_remote_sup_new(node.as_ptr(), 1, 1);
            assert!(!sup.is_null());
            assert_eq!((*sup).strategy, SupervisorStrategy::OneForAll);
            hew_remote_sup_free(sup);
        }
    }

    // ── hew_remote_sup_monitor guards ──────────────────────────────────

    #[test]
    fn monitor_null_sup_returns_error() {
        // SAFETY: testing null guard.
        unsafe {
            assert_eq!(hew_remote_sup_monitor(ptr::null_mut(), 1), -1);
        }
    }

    #[test]
    fn monitor_zero_pid_returns_error() {
        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(3030) };
        // SAFETY: pointers are valid for this scope.
        unsafe {
            let sup = hew_remote_sup_new(node.as_ptr(), 1, 0);
            assert_eq!(hew_remote_sup_monitor(sup, 0), -1);
            hew_remote_sup_free(sup);
        }
    }

    #[test]
    fn monitor_wrong_node_in_pid_returns_error() {
        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(3031) };
        let remote_node: u16 = 10;
        let wrong_node: u16 = 99;
        let pid_with_wrong_node = (u64::from(wrong_node) << 48) | 0x01;
        // SAFETY: pointers are valid for this scope.
        unsafe {
            let sup = hew_remote_sup_new(node.as_ptr(), remote_node, 0);
            assert_eq!(hew_remote_sup_monitor(sup, pid_with_wrong_node), -1);
            hew_remote_sup_free(sup);
        }
    }

    #[test]
    fn monitor_pid_with_zero_node_accepted() {
        // A PID with node bits == 0 is accepted regardless of remote_node_id.
        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(3032) };
        // SAFETY: pointers are valid for this scope.
        unsafe {
            let sup = hew_remote_sup_new(node.as_ptr(), 50, 0);
            // PID 0x0001 has zero in the node bits.
            assert_eq!(hew_remote_sup_monitor(sup, 0x0001), 0);
            hew_remote_sup_free(sup);
        }
    }

    // ── hew_remote_sup_unmonitor guard ─────────────────────────────────

    #[test]
    fn unmonitor_null_sup_returns_error() {
        // SAFETY: testing null guard.
        unsafe {
            assert_eq!(hew_remote_sup_unmonitor(ptr::null_mut(), 1), -1);
        }
    }

    // ── hew_remote_sup_set_callback ────────────────────────────────────

    #[test]
    fn set_callback_null_sup_does_not_crash() {
        // SAFETY: testing null guard.
        unsafe {
            hew_remote_sup_set_callback(ptr::null_mut(), None);
        }
    }

    // ── hew_remote_sup_free ────────────────────────────────────────────

    #[test]
    fn free_null_does_not_crash() {
        // SAFETY: null is explicitly documented as safe.
        unsafe {
            hew_remote_sup_free(ptr::null_mut());
        }
    }

    // ── hew_remote_sup_stop ────────────────────────────────────────────

    #[test]
    fn stop_null_returns_error() {
        // SAFETY: testing null guard.
        unsafe {
            assert_eq!(hew_remote_sup_stop(ptr::null_mut()), -1);
        }
    }

    #[test]
    fn stop_when_not_running_returns_zero() {
        // SAFETY: node lifecycle handled by TestNode.
        let node = unsafe { TestNode::new(3040) };
        // SAFETY: pointers are valid for this scope.
        unsafe {
            let sup = hew_remote_sup_new(node.as_ptr(), 1, 0);
            assert_eq!(hew_remote_sup_stop(sup), 0);
            hew_remote_sup_free(sup);
        }
    }

    // ── hew_remote_sup_start ───────────────────────────────────────────

    #[test]
    fn start_null_returns_error() {
        // SAFETY: testing null guard.
        unsafe {
            assert_eq!(hew_remote_sup_start(ptr::null_mut()), -1);
        }
    }

    // ── process_membership_event (internal state-machine tests) ────────

    #[test]
    fn joined_event_resets_quarantine_state() {
        let sup = bare_supervisor(100, SupervisorStrategy::OneForOne, 5_000);
        {
            let mut state = sup.quarantine_state.lock_or_recover();
            state.suspect_since = Some(Instant::now());
            state.pending_dead = true;
        }

        let dispatch = sup.process_membership_event(HEW_MEMBERSHIP_EVENT_NODE_JOINED);
        assert!(dispatch.is_none());

        let state = sup.quarantine_state.lock_or_recover();
        assert!(state.suspect_since.is_none());
        assert!(!state.pending_dead);
        assert!(!state.notified_dead);
    }

    #[test]
    fn suspect_event_begins_quarantine_window() {
        let sup = bare_supervisor(100, SupervisorStrategy::OneForOne, 5_000);
        let dispatch = sup.process_membership_event(HEW_MEMBERSHIP_EVENT_NODE_SUSPECT);
        assert!(dispatch.is_none());

        let state = sup.quarantine_state.lock_or_recover();
        assert!(state.suspect_since.is_some());
        assert!(state.pending_dead);
    }

    #[test]
    fn suspect_event_does_not_overwrite_existing_timestamp() {
        let sup = bare_supervisor(100, SupervisorStrategy::OneForOne, 5_000);
        let early = Instant::now().checked_sub(Duration::from_secs(60)).unwrap();
        {
            let mut state = sup.quarantine_state.lock_or_recover();
            state.suspect_since = Some(early);
        }

        sup.process_membership_event(HEW_MEMBERSHIP_EVENT_NODE_SUSPECT);

        let state = sup.quarantine_state.lock_or_recover();
        let ts = state.suspect_since.unwrap();
        assert!(ts.duration_since(early) < Duration::from_millis(1));
    }

    #[test]
    fn dead_event_within_quarantine_defers_dispatch() {
        let sup = bare_supervisor(100, SupervisorStrategy::OneForOne, 60_000);
        sup.process_membership_event(HEW_MEMBERSHIP_EVENT_NODE_SUSPECT);
        let dispatch = sup.process_membership_event(HEW_MEMBERSHIP_EVENT_NODE_DEAD);
        assert!(dispatch.is_none());

        let state = sup.quarantine_state.lock_or_recover();
        assert!(state.pending_dead);
        assert!(!state.notified_dead);
    }

    #[test]
    fn dead_event_after_quarantine_dispatches() {
        use std::sync::atomic::AtomicI32;
        static DEATH_COUNT: AtomicI32 = AtomicI32::new(0);
        unsafe extern "C" fn count_deaths(_pid: u64, _node: u16, _reason: c_int) {
            DEATH_COUNT.fetch_add(1, Ordering::Relaxed);
        }
        DEATH_COUNT.store(0, Ordering::Relaxed);

        let sup = bare_supervisor(100, SupervisorStrategy::OneForOne, 0);
        *sup.callback.lock_or_recover() = Some(count_deaths);
        sup.monitored.lock_or_recover().push(42);

        let dispatch = sup.process_membership_event(HEW_MEMBERSHIP_EVENT_NODE_DEAD);
        assert!(dispatch.is_some());
        dispatch.unwrap().execute();
        assert_eq!(DEATH_COUNT.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn dead_event_already_notified_does_not_dispatch() {
        let sup = bare_supervisor(100, SupervisorStrategy::OneForOne, 0);
        sup.monitored.lock_or_recover().push(1);
        {
            let mut state = sup.quarantine_state.lock_or_recover();
            state.notified_dead = true;
        }
        let dispatch = sup.process_membership_event(HEW_MEMBERSHIP_EVENT_NODE_DEAD);
        assert!(dispatch.is_none());
    }

    #[test]
    fn unknown_event_does_not_dispatch() {
        let sup = bare_supervisor(100, SupervisorStrategy::OneForOne, 0);
        let dispatch = sup.process_membership_event(255);
        assert!(dispatch.is_none());
    }

    // ── poll_quarantine ────────────────────────────────────────────────

    #[test]
    fn poll_quarantine_not_pending_returns_none() {
        let sup = bare_supervisor(100, SupervisorStrategy::OneForOne, 0);
        assert!(sup.poll_quarantine().is_none());
    }

    #[test]
    fn poll_quarantine_already_notified_returns_none() {
        let sup = bare_supervisor(100, SupervisorStrategy::OneForOne, 0);
        {
            let mut state = sup.quarantine_state.lock_or_recover();
            state.pending_dead = true;
            state.notified_dead = true;
        }
        assert!(sup.poll_quarantine().is_none());
    }

    #[test]
    fn poll_quarantine_within_window_returns_none() {
        let sup = bare_supervisor(100, SupervisorStrategy::OneForOne, 60_000);
        {
            let mut state = sup.quarantine_state.lock_or_recover();
            state.pending_dead = true;
            state.suspect_since = Some(Instant::now());
        }
        assert!(sup.poll_quarantine().is_none());
    }

    #[test]
    fn poll_quarantine_no_suspect_since_sets_timestamp_and_defers() {
        let sup = bare_supervisor(100, SupervisorStrategy::OneForOne, 0);
        {
            let mut state = sup.quarantine_state.lock_or_recover();
            state.pending_dead = true;
        }
        // First poll with no suspect_since sets timestamp, returns None.
        assert!(sup.poll_quarantine().is_none());
        let state = sup.quarantine_state.lock_or_recover();
        assert!(state.suspect_since.is_some());
    }

    #[test]
    fn poll_quarantine_expired_dispatches() {
        let sup = bare_supervisor(100, SupervisorStrategy::OneForOne, 10);
        *sup.callback.lock_or_recover() = Some(noop_death_cb);
        sup.monitored.lock_or_recover().push(1);
        {
            let mut state = sup.quarantine_state.lock_or_recover();
            state.pending_dead = true;
            state.suspect_since = Some(
                Instant::now()
                    .checked_sub(Duration::from_millis(100))
                    .unwrap(),
            );
        }
        let dispatch = sup.poll_quarantine();
        assert!(dispatch.is_some());

        let state = sup.quarantine_state.lock_or_recover();
        assert!(!state.pending_dead);
        assert!(state.notified_dead);
    }

    // ── RemoteDeathDispatch::execute ───────────────────────────────────

    #[test]
    fn dispatch_execute_calls_callback_for_each_monitored_pid() {
        use std::sync::atomic::AtomicI32;
        static COUNT: AtomicI32 = AtomicI32::new(0);
        unsafe extern "C" fn counting_cb(_pid: u64, _node: u16, _reason: c_int) {
            COUNT.fetch_add(1, Ordering::Relaxed);
        }
        COUNT.store(0, Ordering::Relaxed);

        let dispatch = RemoteDeathDispatch {
            callback: counting_cb,
            remote_node_id: 42,
            monitored: vec![1, 2, 3],
            strategy: SupervisorStrategy::OneForAll,
        };
        dispatch.execute();
        assert_eq!(COUNT.load(Ordering::Relaxed), 3);
    }

    // ── remote_sup_membership_callback ─────────────────────────────────

    #[test]
    fn membership_callback_null_userdata_does_not_crash() {
        remote_sup_membership_callback(1, HEW_MEMBERSHIP_EVENT_NODE_DEAD, ptr::null_mut());
    }
}
