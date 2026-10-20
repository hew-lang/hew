//! Remote monitor and link setup, teardown and failure fan-out.

use super::{
    quarantine_evict, quarantine_insert, reply_table, reply_table_opt, with_current_node_read,
    AskError, ConnectionKey, HewNode, PendingReply, RemoteSetupKind, ReplyStatus, SendConnMgr,
    HEW_ERR_STALE_REF,
};
use crate::connection;
use crate::connection::HewConnMgr;
use crate::node_identity::Location;
use crate::routing;
use crate::set_last_error;
use std::ffi::c_int;
use std::sync::atomic::{AtomicU8, AtomicUsize, Ordering};
use std::sync::{Arc, Weak};

// ── Cross-node monitor ───────────────────────────────────────────────────────

use crate::monitor::HEW_OBSERVATION_NO_CONTEXT;

/// A remote link this node cannot establish fires the link's partition
/// policy at once, exactly as a route lost after setup would.
fn link_unreachable(
    rt: &crate::runtime::RuntimeInner,
    ref_id: Option<u64>,
    local_actor_id: u64,
    target: Location,
    policy_tag: u8,
) -> i32 {
    let fire = ref_id.is_none_or(|ref_id| {
        rt.monitors
            .deliver_link_down_to_ref(
                ref_id,
                target,
                crate::monitor::MONITOR_REASON_LOST,
                crate::internal::types::CrashKind::PartitionDetected.tag(),
            )
            .is_some()
    });
    if fire {
        let _ = crate::link::deliver_cross_node_link_exit(
            local_actor_id,
            target,
            crate::monitor::MONITOR_REASON_LOST,
            policy_tag,
            crate::internal::types::CrashKind::PartitionDetected.tag(),
        );
    }
    0
}

const SETUP_RACE_MONITOR: u8 = 1;
const SETUP_RACE_LINK: u8 = 2;
static SETUP_RACE_KIND: AtomicU8 = AtomicU8::new(0);
const REVERSE_LINK_SETUP_LIMIT: usize = 64;
static REVERSE_LINK_SETUP_ACTIVE: AtomicUsize = AtomicUsize::new(0);

struct ReverseLinkSetupGuard(Arc<AtomicUsize>);

impl Drop for ReverseLinkSetupGuard {
    fn drop(&mut self) {
        REVERSE_LINK_SETUP_ACTIVE.fetch_sub(1, Ordering::AcqRel);
        self.0.fetch_sub(1, Ordering::SeqCst);
    }
}

unsafe fn resolve_monitor_connection(node: &HewNode, target: Location) -> Result<c_int, c_int> {
    if node.conn_mgr.is_null() || node.routing_table.is_null() {
        set_last_error("cross-node monitor: node has no connection manager / routing table");
        return Err(-1);
    }
    // SAFETY: routing table pointer is valid while the node is running.
    match unsafe { routing::hew_routing_lookup_location(node.routing_table, target) } {
        routing::LocationRoute::Remote { conn, .. } => Ok(conn),
        routing::LocationRoute::Partition => {
            set_last_error("cross-node monitor: target identity is partitioned");
            Err(-1)
        }
        routing::LocationRoute::Local { .. } | routing::LocationRoute::StaleRef => {
            set_last_error("cross-node monitor: target Location is invalid or stale");
            Err(HEW_ERR_STALE_REF)
        }
    }
}

fn encode_node_control_frame(ctrl_kind: u64, payload: Vec<u8>) -> Option<Vec<u8>> {
    let frame = crate::envelope::ControlFrame {
        version: crate::envelope::WIRE_VERSION,
        ctrl_kind,
        payload,
    };
    match crate::envelope::encode_control_frame(&frame) {
        Ok(bytes) => Some(bytes),
        Err(err) => {
            set_last_error(format!(
                "cross-node monitor control frame encode failure: {err}"
            ));
            None
        }
    }
}

unsafe fn send_control_frame_on_connection(
    conn_mgr: *mut HewConnMgr,
    conn_id: c_int,
    recipient: Location,
    ctrl_kind: u64,
    payload: Vec<u8>,
) -> c_int {
    // SAFETY: the caller pins the node's manager for this call.
    let Some(claim) =
        (unsafe { connection::claim_send_to_location(&*conn_mgr, conn_id, recipient) })
    else {
        return -1;
    };
    let Some(bytes) = encode_node_control_frame(ctrl_kind, payload) else {
        return -1;
    };
    claim.send(&bytes)
}

unsafe fn send_control_frame_on_claimed_connection(
    conn_mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    ctrl_kind: u64,
    payload: Vec<u8>,
) -> c_int {
    let Some(bytes) = encode_node_control_frame(ctrl_kind, payload) else {
        return -1;
    };
    // SAFETY: caller pins the manager while the exact publication is claimed.
    let manager = unsafe { &*conn_mgr };
    let Some(claim) = connection::claim_send(manager, conn_id, Some(claim_token)) else {
        return -1;
    };
    if claim.submit(move || Some(bytes), |_| {}) {
        0
    } else {
        -1
    }
}

struct PreparedRemoteSetup {
    setup_id: u64,
    pending: Arc<PendingReply>,
    bytes: Vec<u8>,
}

fn prepare_acknowledged_setup_with_waker(
    conn_mgr: *mut HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    ctrl_kind: u64,
    kind: RemoteSetupKind,
    waker: Option<crate::wake::OwnedWaker>,
    encode_payload: impl FnOnce(u64) -> Result<Vec<u8>, crate::envelope::MonitorPayloadError>,
) -> Result<PreparedRemoteSetup, ()> {
    let (setup_id, pending) = reply_table().register_with_waker(
        ConnectionKey::new(conn_mgr, conn_id),
        waker,
        super::PendingReplyKind::Setup(kind, publication_token),
    );
    let payload = match encode_payload(setup_id) {
        Ok(payload) => payload,
        Err(err) => {
            reply_table().remove(setup_id);
            set_last_error(format!("remote setup payload encode failure: {err}"));
            return Err(());
        }
    };
    let Some(bytes) = encode_node_control_frame(ctrl_kind, payload) else {
        reply_table().remove(setup_id);
        return Err(());
    };
    Ok(PreparedRemoteSetup {
        setup_id,
        pending,
        bytes,
    })
}

unsafe fn acquire_reverse_link_setup(
    conn_mgr: *mut HewConnMgr,
) -> Result<ReverseLinkSetupGuard, u8> {
    let active = REVERSE_LINK_SETUP_ACTIVE.fetch_add(1, Ordering::AcqRel);
    if active >= REVERSE_LINK_SETUP_LIMIT {
        REVERSE_LINK_SETUP_ACTIVE.fetch_sub(1, Ordering::AcqRel);
        return Err(crate::envelope::SETUP_STATUS_RESOURCE_EXHAUSTED);
    }

    // SAFETY: the connection reader pins `conn_mgr` for this handler.
    let Some(per_mgr_active) = (unsafe { connection::hew_connmgr_inbound_ask_active(conn_mgr) })
    else {
        REVERSE_LINK_SETUP_ACTIVE.fetch_sub(1, Ordering::AcqRel);
        return Err(crate::envelope::SETUP_STATUS_TARGET_GONE);
    };
    // SAFETY: the connection reader pins `conn_mgr` for this handler.
    let Some(spawn_gate) = (unsafe { connection::hew_connmgr_inbound_spawn_closed_flag(conn_mgr) })
    else {
        REVERSE_LINK_SETUP_ACTIVE.fetch_sub(1, Ordering::AcqRel);
        return Err(crate::envelope::SETUP_STATUS_TARGET_GONE);
    };
    // SAFETY: the connection reader pins `conn_mgr` for this handler.
    let Some(shutdown) = (unsafe { connection::hew_connmgr_shutdown_flag(conn_mgr) }) else {
        REVERSE_LINK_SETUP_ACTIVE.fetch_sub(1, Ordering::AcqRel);
        return Err(crate::envelope::SETUP_STATUS_TARGET_GONE);
    };

    per_mgr_active.fetch_add(1, Ordering::SeqCst);
    if spawn_gate.load(Ordering::SeqCst) || shutdown.load(Ordering::SeqCst) {
        per_mgr_active.fetch_sub(1, Ordering::SeqCst);
        REVERSE_LINK_SETUP_ACTIVE.fetch_sub(1, Ordering::AcqRel);
        return Err(crate::envelope::SETUP_STATUS_TARGET_GONE);
    }
    Ok(ReverseLinkSetupGuard(per_mgr_active))
}

pub(crate) fn remote_setup_race_probe_wait(kind: RemoteSetupKind, target_actor_id: u64) {
    let enabled = crate::env::ENV_LOCK
        .read_access(|()| std::env::var("HEW_DIST_SETUP_RACE_PROBE").ok())
        .is_some_and(|value| value == "1");
    if !enabled {
        return;
    }
    let kind = match kind {
        RemoteSetupKind::Monitor => SETUP_RACE_MONITOR,
        RemoteSetupKind::Link => SETUP_RACE_LINK,
    };
    if SETUP_RACE_KIND
        .compare_exchange(0, kind, Ordering::AcqRel, Ordering::Acquire)
        .is_err()
    {
        return;
    }
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    loop {
        let terminal =
            crate::lifetime::live_actors::with_actor_send_by_id(target_actor_id, |actor| {
                // SAFETY: the send pin keeps the actor allocation alive.
                let state = unsafe { (*actor).actor_state.load(Ordering::Acquire) };
                state == crate::internal::types::HewActorState::Stopped as i32
                    || state == crate::internal::types::HewActorState::Crashed as i32
            })
            .unwrap_or(true);
        if terminal || std::time::Instant::now() >= deadline {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
    SETUP_RACE_KIND.store(0, Ordering::Release);
}

/// Test-only distributed setup-race seam. Inert unless
/// `HEW_DIST_SETUP_RACE_PROBE=1`.
#[no_mangle]
pub extern "C" fn hew_dist_setup_race_kind() -> i64 {
    let enabled = crate::env::ENV_LOCK
        .read_access(|()| std::env::var("HEW_DIST_SETUP_RACE_PROBE").ok())
        .is_some_and(|value| value == "1");
    if enabled {
        i64::from(SETUP_RACE_KIND.load(Ordering::Acquire))
    } else {
        0
    }
}

/// A notice from the observation protocol for the node that owns `recipient`.
enum ObservationNotice {
    Demonitor(crate::envelope::MonitorReqPayload),
    MonitorDown(crate::envelope::MonitorDownPayload),
    LinkDown(crate::envelope::MonitorDownPayload),
}

/// Deliver an observation notice to the node that owns `recipient`: this node
/// applies its own notice as its connection reader would, and any other node
/// receives the encoded control frame. Returns 0 on delivery, -1 otherwise (no
/// route, no manager, encode failure). Never panics; logs via `set_last_error`.
///
/// # Safety
///
/// `node` must be a valid running `HewNode` pointer.
unsafe fn send_observation_notice(
    node: &HewNode,
    recipient: Location,
    notice: ObservationNotice,
) -> c_int {
    if node.routing_table.is_null() {
        set_last_error("observation notice: node has no routing table");
        return -1;
    }
    // SAFETY: the routing table is valid while the node is running.
    match unsafe { routing::hew_routing_lookup_location(node.routing_table, recipient) } {
        routing::LocationRoute::Local { .. } => {
            apply_observation_notice(notice);
            return 0;
        }
        routing::LocationRoute::Remote { .. } => {}
        routing::LocationRoute::Partition => {
            set_last_error("observation notice: recipient identity is partitioned");
            return -1;
        }
        routing::LocationRoute::StaleRef => {
            set_last_error("observation notice: recipient Location is stale");
            return HEW_ERR_STALE_REF;
        }
    }
    let encoded = match &notice {
        ObservationNotice::Demonitor(payload) => {
            crate::envelope::encode_monitor_req_payload(payload)
                .map(|bytes| (crate::envelope::CTRL_DEMONITOR, bytes))
        }
        ObservationNotice::MonitorDown(payload) => {
            crate::envelope::encode_monitor_down_payload(payload)
                .map(|bytes| (crate::envelope::CTRL_MONITOR_DOWN, bytes))
        }
        ObservationNotice::LinkDown(payload) => {
            crate::envelope::encode_monitor_down_payload(payload)
                .map(|bytes| (crate::envelope::CTRL_LINK_DOWN, bytes))
        }
    };
    let (ctrl_kind, payload) = match encoded {
        Ok(encoded) => encoded,
        Err(err) => {
            set_last_error(format!("observation notice payload encode failure: {err}"));
            return -1;
        }
    };
    // SAFETY: inherited node/routing validity from the caller.
    let conn_id = match unsafe { resolve_monitor_connection(node, recipient) } {
        Ok(conn_id) => conn_id,
        Err(error) => return error,
    };
    // SAFETY: node pins the manager for this call.
    unsafe {
        send_control_frame_on_connection(node.conn_mgr, conn_id, recipient, ctrl_kind, payload)
    }
}

/// Apply a notice this node addressed to itself.
fn apply_observation_notice(notice: ObservationNotice) {
    match notice {
        ObservationNotice::Demonitor(payload) => {
            if let Some(rt) = crate::runtime::rt_current_opt() {
                rt.monitors.remove_remote_watcher(
                    payload.target.slot(),
                    payload.watcher,
                    payload.ref_id,
                );
            }
        }
        ObservationNotice::MonitorDown(payload) => handle_inbound_monitor_down(&payload),
        ObservationNotice::LinkDown(payload) => {
            handle_inbound_link_down(
                payload.ref_id,
                payload.target,
                payload.reason,
                payload.crash_kind,
            );
        }
    }
}

pub(super) fn local_actor_location(
    node: &HewNode,
    actor_id: u64,
) -> Option<crate::node_identity::Location> {
    Location::new(
        node.auth.node_identity()?,
        crate::pid::hew_pid_serial(actor_id),
        node.auth.session_incarnation()?,
    )
    .ok()
}

pub(crate) fn send_remote_demonitor(ref_id: u64, target: Location, watcher_actor_id: u64) {
    let Some(watcher) = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: the current-node read lock pins the node.
        let node = unsafe { &*node };
        local_actor_location(node, watcher_actor_id)
    }) else {
        set_last_error("remote demonitor: exact watcher Location is unavailable");
        return;
    };
    let notice = ObservationNotice::Demonitor(crate::envelope::MonitorReqPayload {
        grant_id: 0,
        watcher,
        ref_id,
        target,
        setup_id: 0,
    });
    with_current_node_read(|guard| {
        if *guard == 0 {
            return;
        }
        let node = *guard as *const HewNode;
        // SAFETY: read lock pins the current node pointer for this call.
        let node_ref = unsafe { &*node };
        // SAFETY: node_ref is valid for this call.
        let _ = unsafe { send_observation_notice(node_ref, target, notice) };
    });
}

/// Retract a monitor regardless of whether its target is local or remote.
#[no_mangle]
pub extern "C" fn hew_node_demonitor(ref_id: i64) {
    #[expect(
        clippy::cast_sign_loss,
        reason = "the Hew monitor id is the bit-preserving form of a runtime u64"
    )]
    crate::monitor::hew_actor_demonitor(ref_id as u64);
}

/// Fan out a `CTRL_MONITOR_DOWN` to every remote node monitoring a locally-dying
/// actor (terminal sweep).
///
/// Called from `notify_monitors_on_death` (the `hew_actor_trap` terminal hook)
/// for a locally-owned actor: takes the target-side remote watchers for
/// `target_serial` and sends each watcher node a DOWN frame carrying its
/// `ref_id` and the terminal `reason`. This is the clean-exit AND crash path —
/// both route through the trap.
///
/// Fail-closed and reentrancy-tolerant (R7): if no runtime / current node /
/// connection manager is installed (the trap can run with none — e.g. the
/// supervisor-cascade unit tests), it no-ops without sending or panicking,
/// exactly as the local sweep does. The remote watchers are still removed so a
/// later sweep finds nothing.
pub(crate) fn fan_out_remote_monitor_down(
    target_actor_id: u64,
    watchers: Vec<crate::monitor::RemoteWatcherTarget>,
    reason: i32,
    crash_kind: u32,
    end: crate::internal::types::ActorEndReason,
    close_fault: Option<&(i32, String)>,
) {
    if watchers.is_empty() {
        return;
    }
    with_current_node_read(|guard| {
        if *guard == 0 {
            return;
        }
        let node = *guard as *const HewNode;
        // SAFETY: read lock pins the current node pointer for this call.
        let node_ref = unsafe { &*node };
        if node_ref.conn_mgr.is_null() || node_ref.routing_table.is_null() {
            return;
        }
        let Some(target) = local_actor_location(node_ref, target_actor_id) else {
            set_last_error("monitor down fan-out: exact target Location is unavailable");
            return;
        };
        for watcher in watchers {
            let payload = crate::envelope::MonitorDownPayload {
                ref_id: watcher.ref_id,
                target,
                reason,
                end_reason: end as u8,
                crash_kind: crash_kind.cast_signed(),
                close_fault: close_fault.cloned(),
            };
            // A LINK watcher receives a link down (its node crashes the local
            // linked actor via the mailbox EXIT cascade); a MONITOR watcher
            // receives a monitor down (its node queues mailbox DOWN). The
            // payload is identical; only the system-message kind and terminal
            // policy diverge.
            let notice = if watcher.is_link {
                ObservationNotice::LinkDown(payload)
            } else {
                ObservationNotice::MonitorDown(payload)
            };
            if let Some(publication) = watcher.publication {
                // SAFETY: the current-node guard pins the manager during claim acquisition.
                let manager = unsafe { &*node_ref.conn_mgr };
                let Some(claim) = connection::claim_send_by_identity(
                    manager,
                    watcher.watcher.node(),
                    Some(publication),
                ) else {
                    continue;
                };
                let (kind, encoded) = match notice {
                    ObservationNotice::LinkDown(payload) => (
                        crate::envelope::CTRL_LINK_DOWN,
                        crate::envelope::encode_link_down_payload(&payload),
                    ),
                    ObservationNotice::MonitorDown(payload) => (
                        crate::envelope::CTRL_MONITOR_DOWN,
                        crate::envelope::encode_monitor_down_payload(&payload),
                    ),
                    ObservationNotice::Demonitor(_) => unreachable!(),
                };
                if let Some(bytes) = encoded
                    .ok()
                    .and_then(|payload| encode_node_control_frame(kind, payload))
                {
                    claim.submit(move || Some(bytes), |_| {});
                }
            } else {
                // SAFETY: internal unbound observations retain their hosting boundary.
                let _ = unsafe { send_observation_notice(node_ref, watcher.watcher, notice) };
            }
        }
    });
}

/// A late subscription still observes a known terminal incarnation. Publish
/// its ordinary terminal event only after the target's native cleanup finished.
pub(crate) fn send_terminal_setup_notice(
    manager: *mut HewConnMgr,
    connection: c_int,
    publication: u64,
    ref_id: u64,
    target: Location,
    terminal: crate::monitor::RemoteTerminal,
    link: bool,
) {
    let reason = terminal.reason;
    let payload = crate::envelope::MonitorDownPayload {
        ref_id,
        target,
        reason: reason.state,
        end_reason: reason.end as u8,
        close_fault: terminal.close_fault,
        crash_kind: if reason.end == crate::internal::types::ActorEndReason::Crashed {
            reason.crash_kind.cast_signed()
        } else {
            0
        },
    };
    if let Ok(payload) = crate::envelope::encode_monitor_down_payload(&payload) {
        // SAFETY: the authenticated connection reader pins the manager here.
        unsafe {
            send_control_frame_on_claimed_connection(
                manager,
                connection,
                publication,
                if link {
                    crate::envelope::CTRL_LINK_DOWN
                } else {
                    crate::envelope::CTRL_MONITOR_DOWN
                },
                payload,
            )
        };
    }
}

/// Handle an inbound `CTRL_LINK_REQ`: a remote node is linking one of
/// our local actors. Establishes the BIDIRECTIONAL cross-node link.
///
/// On the ORIGINAL request (`reciprocate == 1`):
/// 1. Register a target-side remote LINK watcher for our `target_serial` →
///    `(linker_node, ref_id)`, so when our actor dies the terminal sweep fans a
///    `CTRL_LINK_DOWN` to the linker (Direction 1: our death → crash the linker).
/// 2. Register a watcher-side LINK entry watching `(linker_node, linker_serial)`
///    keyed by a fresh reverse ref, action = crash OUR `target` actor per the
///    policy, so the linker's death crashes our actor (Direction 2 receive).
/// 3. Send a reverse `CTRL_LINK_REQ` (`reciprocate == 0`) back to the linker so
///    IT registers the target-side watcher that fans Direction 2's DOWN to us.
/// 4. Complete the original setup only after the reverse ACK arrives. The wait
///    runs off the connection reader so that reader remains free to consume it.
///
/// On the REVERSE request (`reciprocate == 0`): only step 1 — register the
/// target-side remote LINK watcher — and do NOT reciprocate again (bounding the
/// handshake to one round trip, never an infinite reciprocation).
///
/// Fail-closed: a no-runtime / no-current-node / no-route state no-ops without
/// panicking; the EXIT is only ever synthesized later for an entry THIS node
/// registered, so a forged frame cannot crash an actor this node never linked.
pub(crate) fn handle_inbound_link_req(
    payload: &crate::envelope::LinkReqPayload,
    target_actor_id: u64,
    conn_mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
) -> Option<u8> {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("handle_inbound_link_req: no runtime installed");
        return Some(crate::envelope::SETUP_STATUS_TARGET_GONE);
    };
    if payload.reciprocate == 0 {
        // Reverse request: target-side registration only; do not loop.
        return Some(
            match rt
                .monitors
                .register_reciprocal_watcher(target_actor_id, payload, claim_token)
            {
                crate::monitor::RemoteWatcherSetup::Registered => {
                    crate::envelope::SETUP_STATUS_ACCEPTED
                }
                crate::monitor::RemoteWatcherSetup::Terminal(reason) => {
                    send_terminal_setup_notice(
                        conn_mgr,
                        conn_id,
                        claim_token,
                        payload.ref_id,
                        payload.target,
                        reason,
                        true,
                    );
                    crate::envelope::SETUP_STATUS_ACCEPTED
                }
                crate::monitor::RemoteWatcherSetup::Refused => {
                    crate::envelope::SETUP_STATUS_REFUSED
                }
                crate::monitor::RemoteWatcherSetup::TargetGone => {
                    crate::envelope::SETUP_STATUS_TARGET_GONE
                }
            },
        );
    }

    handle_inbound_original_link_req(rt, payload, target_actor_id, conn_mgr, conn_id, claim_token)
}

struct ReverseLinkSetup {
    ready: crate::wake::scheduled::ReadyState,
    state: std::sync::Mutex<Option<ReverseLinkSetupState>>,
}

struct ReverseLinkSetupState {
    prepared: PreparedRemoteSetup,
    payload: crate::envelope::LinkReqPayload,
    reverse_ref: u64,
    conn_mgr: SendConnMgr,
    owner: Weak<super::service::ServiceExports>,
    conn_id: c_int,
    publication: u64,
    timer: *mut crate::coro_sleep::HewCoroSleep,
    _guard: ReverseLinkSetupGuard,
}

// SAFETY: the manager pointer is only an identity, revalidated under the node
// registry before use. Scheduler polls serialize the timer through the mutex.
unsafe impl Send for ReverseLinkSetupState {}

impl Drop for ReverseLinkSetupState {
    fn drop(&mut self) {
        reply_table().remove(self.prepared.setup_id);
        if !self.timer.is_null() {
            // SAFETY: this state owns the unique timer handle.
            unsafe { crate::coro_sleep::hew_coro_sleep_free(self.timer) };
        }
    }
}

impl ReverseLinkSetup {
    fn descriptor(self: &Arc<Self>) -> crate::wake::HewWaker {
        unsafe extern "C" fn retain(context: *mut std::ffi::c_void) {
            // SAFETY: an existing Arc keeps this allocation live during retain.
            unsafe { Arc::increment_strong_count(context.cast::<ReverseLinkSetup>()) };
        }
        unsafe extern "C" fn release(context: *mut std::ffi::c_void) {
            // SAFETY: this callback receives one retained Arc reference.
            drop(unsafe { Arc::from_raw(context.cast::<ReverseLinkSetup>()) });
        }
        unsafe extern "C" fn wake(context: *mut std::ffi::c_void) {
            // SAFETY: this descriptor owns a live reference for the whole call.
            let job = unsafe { &*context.cast::<ReverseLinkSetup>() };
            if job.ready.notify() {
                // SAFETY: the descriptor remains live while retaining queued work.
                unsafe { Arc::increment_strong_count(context.cast::<ReverseLinkSetup>()) };
                // SAFETY: the increment above transfers one Arc to the queue.
                crate::wake::scheduled::enqueue(unsafe {
                    Arc::from_raw(context.cast::<ReverseLinkSetup>())
                });
            }
        }
        crate::wake::HewWaker {
            context: Arc::as_ptr(self).cast_mut().cast(),
            wake,
            retain,
            release,
        }
    }
}

impl crate::wake::scheduled::ReadyWork for ReverseLinkSetup {
    fn poll(self: Arc<Self>) {
        use crate::util::MutexExt;
        self.ready.begin();
        let completed = {
            let mut state = self.state.lock_or_recover();
            let operation = state.as_mut().expect("queued reverse setup owns its state");
            let outcome = operation.prepared.pending.outcome.lock_or_recover().take();
            // SAFETY: the state mutex serializes the unique timer operation.
            let expired = unsafe { crate::coro_sleep::hew_coro_sleep_status(operation.timer) } != 0;
            if outcome.is_none() && !expired {
                None
            } else {
                let status = outcome
                    .filter(|outcome| outcome.status == ReplyStatus::Success)
                    .and_then(|outcome| {
                        crate::envelope::decode_setup_result_payload(&outcome.data).ok()
                    })
                    .filter(|result| {
                        result.setup_id == operation.prepared.setup_id
                            && result.ref_id == operation.reverse_ref
                            && result.target == operation.payload.linker
                    })
                    .map_or(crate::envelope::SETUP_STATUS_TARGET_GONE, |result| {
                        result.status
                    });
                Some((state.take().expect("completed setup state"), status))
            }
        };
        let done = completed.is_some();
        if let Some((operation, status)) = completed {
            complete_inbound_link_setup(
                &operation.payload,
                operation.reverse_ref,
                status,
                operation.conn_mgr,
                &operation.owner,
                operation.conn_id,
                operation.publication,
            );
        }
        if self.ready.finish(done) {
            crate::wake::scheduled::enqueue(self);
        }
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "one reciprocal setup owns reservation, publication and rollback"
)]
fn handle_inbound_original_link_req(
    rt: &crate::runtime::RuntimeInner,
    payload: &crate::envelope::LinkReqPayload,
    target_actor_id: u64,
    conn_mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
) -> Option<u8> {
    use crate::util::MutexExt;
    // SAFETY: the connection reader pins the manager while reserving its drain owner.
    let guard = match unsafe { acquire_reverse_link_setup(conn_mgr) } {
        Ok(guard) => guard,
        Err(status) => return Some(status),
    };
    let Some(reverse_ref) = rt.monitors.next_observation_id() else {
        return Some(crate::envelope::SETUP_STATUS_RESOURCE_EXHAUSTED);
    };
    let job = Arc::new(ReverseLinkSetup {
        ready: crate::wake::scheduled::ReadyState::queued(),
        state: std::sync::Mutex::new(None),
    });
    let descriptor = job.descriptor();
    // SAFETY: job retains the descriptor allocation throughout registration.
    let waker = unsafe { crate::wake::OwnedWaker::retain(&descriptor) };
    let Ok(prepared) = prepare_acknowledged_setup_with_waker(
        conn_mgr,
        conn_id,
        claim_token,
        crate::envelope::CTRL_LINK_REQ,
        RemoteSetupKind::Link,
        Some(waker),
        |setup_id| {
            crate::envelope::encode_link_req_payload(&crate::envelope::LinkReqPayload {
                grant_id: payload.grant_id,
                reciprocal_ref: payload.ref_id,
                linker: payload.target,
                ref_id: reverse_ref,
                target: payload.linker,
                policy_tag: payload.policy_tag,
                reciprocate: 0,
                setup_id,
            })
        },
    ) else {
        return Some(crate::envelope::SETUP_STATUS_RESOURCE_EXHAUSTED);
    };
    rt.monitors.insert_admitted_observation(
        reverse_ref,
        payload.linker,
        crate::monitor::WatcherAction::Link {
            local_actor_id: target_actor_id,
            policy_tag: payload.policy_tag,
        },
        crate::monitor::ObservationAuthority {
            grant: payload.grant_id,
            publication: claim_token,
            watcher: payload.target,
        },
        None,
    );
    match rt.monitors.register_admitted_watcher(
        target_actor_id,
        payload.linker,
        payload.ref_id,
        true,
        claim_token,
        reverse_ref,
    ) {
        crate::monitor::RemoteWatcherSetup::Registered => {}
        terminal => {
            reply_table().remove(prepared.setup_id);
            rt.monitors.remove_remote_observation(reverse_ref);
            return Some(match terminal {
                crate::monitor::RemoteWatcherSetup::Terminal(reason) => {
                    send_terminal_setup_notice(
                        conn_mgr,
                        conn_id,
                        claim_token,
                        payload.ref_id,
                        payload.target,
                        reason,
                        true,
                    );
                    crate::envelope::SETUP_STATUS_ACCEPTED
                }
                _ => crate::envelope::SETUP_STATUS_TARGET_GONE,
            });
        }
    }
    // SAFETY: timer retains this live descriptor separately.
    let timer =
        unsafe { crate::coro_sleep::hew_coro_sleep_new(4_000_000_000, &raw const descriptor) };
    let setup_id = prepared.setup_id;
    let bytes = prepared.bytes.clone();
    *job.state.lock_or_recover() = Some(ReverseLinkSetupState {
        prepared,
        payload: payload.clone(),
        reverse_ref,
        conn_mgr: SendConnMgr(conn_mgr),
        // SAFETY: the connection reader pins this manager during registration.
        owner: Arc::downgrade(unsafe { &(*conn_mgr).service_exports }),
        conn_id,
        publication: claim_token,
        timer,
        _guard: guard,
    });
    // SAFETY: the connection reader pins this manager during claim acquisition.
    let claim = unsafe {
        connection::claim_send_by_identity(&*conn_mgr, payload.linker.node(), Some(claim_token))
    };
    let table = Arc::clone(&rt.node.reply_table);
    let completion = Arc::clone(&table);
    if !claim.is_some_and(|claim| {
        claim.submit(
            move || {
                table
                    .pending
                    .lock_or_recover()
                    .contains_key(&setup_id)
                    .then_some(bytes)
            },
            move |result| {
                if result.is_err() {
                    completion.fail(setup_id, AskError::ConnectionDropped);
                }
            },
        )
    }) {
        reply_table().fail(setup_id, AskError::SendFailed);
    }
    crate::wake::scheduled::enqueue(job);
    None
}

fn rollback_inbound_link_setup(payload: &crate::envelope::LinkReqPayload, reverse_ref: u64) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    rt.monitors
        .remove_remote_watcher(payload.target.slot(), payload.linker, payload.ref_id);
    if let Some(down) = rt.monitors.deliver_link_down_to_ref(
        reverse_ref,
        payload.linker,
        crate::monitor::MONITOR_REASON_LOST,
        crate::internal::types::CrashKind::PartitionDetected.tag(),
    ) {
        let _ = crate::link::deliver_cross_node_link_exit(
            down.local_actor_id,
            down.target,
            down.reason,
            down.policy_tag,
            down.crash_kind,
        );
    }
}

fn discard_inbound_link_setup(payload: &crate::envelope::LinkReqPayload, reverse_ref: u64) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    rt.monitors
        .remove_remote_watcher(payload.target.slot(), payload.linker, payload.ref_id);
    rt.monitors.remove_remote_observation(reverse_ref);
}

/// Deferred setup owns no node allocation. The registry pins a matching live
/// manager while taking a send claim; the canonical owner prevents address reuse
/// from turning an old setup into authority over a replacement node.
fn send_deferred_setup_control(
    manager: SendConnMgr,
    owner: &Weak<super::service::ServiceExports>,
    connection: c_int,
    publication: u64,
    kind: u64,
    payload: Vec<u8>,
) -> i32 {
    let Some(bytes) = encode_node_control_frame(kind, payload) else {
        return -1;
    };
    let claim = super::with_known_nodes_opt(|nodes| {
        nodes.iter().find_map(|known| {
            // SAFETY: the known-node registry excludes node teardown and removal.
            let node = unsafe { &*known.0 };
            if node.state.load(Ordering::Acquire) != super::NODE_STATE_RUNNING
                || node.conn_mgr != manager.0
            {
                return None;
            }
            // SAFETY: a running node owns its manager under this registry guard.
            let manager = unsafe { &*node.conn_mgr };
            if !owner.ptr_eq(&Arc::downgrade(&manager.service_exports)) {
                return None;
            }
            connection::claim_send(manager, connection, Some(publication))
        })
    })
    .flatten();
    if claim.is_some_and(|claim| claim.submit(move || Some(bytes), |_| {})) {
        0
    } else {
        -1
    }
}

fn send_reverse_unlink(
    payload: &crate::envelope::LinkReqPayload,
    reverse_ref: u64,
    conn_mgr: SendConnMgr,
    owner: &Weak<super::service::ServiceExports>,
    conn_id: c_int,
    claim_token: u64,
) {
    let unlink = crate::envelope::LinkReqPayload {
        grant_id: 0,
        reciprocal_ref: 0,
        linker: payload.target,
        ref_id: reverse_ref,
        target: payload.linker,
        policy_tag: payload.policy_tag,
        reciprocate: 0,
        setup_id: 0,
    };
    let bytes = match crate::envelope::encode_link_req_payload(&unlink) {
        Ok(bytes) => bytes,
        Err(err) => {
            set_last_error(format!(
                "reverse link cleanup payload encode failure: {err}"
            ));
            return;
        }
    };
    let _ = send_deferred_setup_control(
        conn_mgr,
        owner,
        conn_id,
        claim_token,
        crate::envelope::CTRL_UNLINK,
        bytes,
    );
}

fn complete_inbound_link_setup(
    payload: &crate::envelope::LinkReqPayload,
    reverse_ref: u64,
    status: u8,
    conn_mgr: SendConnMgr,
    owner: &Weak<super::service::ServiceExports>,
    conn_id: c_int,
    claim_token: u64,
) {
    match status {
        crate::envelope::SETUP_STATUS_ACCEPTED => {}
        crate::envelope::SETUP_STATUS_RESOURCE_EXHAUSTED
        | crate::envelope::SETUP_STATUS_REFUSED => {
            discard_inbound_link_setup(payload, reverse_ref);
            send_reverse_unlink(payload, reverse_ref, conn_mgr, owner, conn_id, claim_token);
        }
        _ => {
            rollback_inbound_link_setup(payload, reverse_ref);
            send_reverse_unlink(payload, reverse_ref, conn_mgr, owner, conn_id, claim_token);
        }
    }

    // The original grant was admitted before reciprocal setup. A caller that
    // retired that registration may refuse the reverse request; this is not a
    // proved refusal of the original operation after publication.
    let status = if status == crate::envelope::SETUP_STATUS_REFUSED {
        crate::envelope::SETUP_STATUS_TARGET_GONE
    } else {
        status
    };

    let result =
        match crate::envelope::encode_setup_result_payload(&crate::envelope::SetupResultPayload {
            setup_id: payload.setup_id,
            ref_id: payload.ref_id,
            target: payload.target,
            status,
        }) {
            Ok(result) => result,
            Err(err) => {
                if status == crate::envelope::SETUP_STATUS_ACCEPTED {
                    rollback_inbound_link_setup(payload, reverse_ref);
                    send_reverse_unlink(
                        payload,
                        reverse_ref,
                        conn_mgr,
                        owner,
                        conn_id,
                        claim_token,
                    );
                }
                set_last_error(format!("deferred link setup result encode failure: {err}"));
                return;
            }
        };
    if send_deferred_setup_control(
        conn_mgr,
        owner,
        conn_id,
        claim_token,
        crate::envelope::CTRL_LINK_SETUP_RESULT,
        result,
    ) != 0
        && status == crate::envelope::SETUP_STATUS_ACCEPTED
    {
        rollback_inbound_link_setup(payload, reverse_ref);
        send_reverse_unlink(payload, reverse_ref, conn_mgr, owner, conn_id, claim_token);
    }
}

/// Handle an inbound `CTRL_MONITOR_DOWN`: a node owning an actor we monitor
/// reports its terminal state. Queue the DOWN for a monitor this node holds.
pub(crate) fn handle_inbound_monitor_down(payload: &crate::envelope::MonitorDownPayload) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("handle_inbound_monitor_down: no runtime installed");
        return;
    };
    if let Some(down) = rt
        .monitors
        .deliver_monitor_to_ref(payload.ref_id, payload.target)
    {
        rt.monitors.enqueue_terminal(
            down,
            payload.crash_kind.cast_unsigned(),
            payload.end_reason,
            payload.close_fault.clone(),
        );
    }
}

/// Handle an inbound `CTRL_LINK_DOWN`: a node owning an actor we LINK
/// reports it reached a terminal state. Fire the cross-node link cascade —
/// synthesize a `HewSysMsg::Exit` into the LOCAL linked actor's MAILBOX and crash it
/// (for `CrashLinked`), keyed by the local actor id stored in our link entry.
///
/// A monitor DOWN queues a typed mailbox notification; a `CrashLinked` link DOWN
/// queues EXIT and crashes the linked actor. The EXIT is fired exactly once and
/// ONLY for a link entry THIS node registered AND ONLY when
/// `authenticated_peer` (the handshake-verified sender of the frame, resolved by
/// the connection layer) matches that entry's own linked-to node — so neither a
/// forged `ref_id` this node never linked NOR a genuinely-connected but
/// unrelated peer that merely guessed/learned a pending link `ref_id` can crash
/// an actor we linked to a DIFFERENT, still-alive peer.
pub(crate) fn handle_inbound_link_down(
    ref_id: u64,
    target: Location,
    reason: i32,
    crash_kind: i32,
) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("handle_inbound_link_down: no runtime installed");
        return;
    };
    // Links retain the original terminal classification. The separate cleanup
    // diagnostic is consumed only by completion observers (`stopped`).
    if let Some(down) = rt
        .monitors
        .deliver_link_down_to_ref(ref_id, target, reason, crash_kind)
    {
        let _ = crate::link::deliver_cross_node_link_exit(
            down.local_actor_id,
            down.target,
            down.reason,
            down.policy_tag,
            down.crash_kind,
        );
    }
}

/// Fan out `MonitorLost` to every local watcher of an actor on a lost/dead node
/// (connection-drop / SWIM-DEAD hook).
///
/// Called from the connection-drop retire hook and the SWIM `MEMBER_DEAD`
/// fan-out. Each pending observation is removed before a `MonitorLost` record is
/// queued, so a definitive DOWN and a partition signal cannot both win.
///
/// Also prunes the target-side `RemoteWatcher` entries registered by the dead
/// node. When a watcher node dies, the actors it was watching on this node must
/// not accumulate its now-stale watcher records indefinitely — only a definitive
/// demonitor or the watched actor's own death normally removes them. Pruning here
/// ensures the target-side table stays bounded under watcher-node churn.
pub(crate) fn fan_out_monitor_lost_for_node(dead_node_id: u16) {
    let dead_node = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: current-node read lock pins the node.
        unsafe { (*node).auth.node_id_for_route_slot(dead_node_id) }
    });
    if let Some(dead_node) = dead_node {
        fan_out_monitor_lost_for_identity(dead_node);
    }
}

pub(crate) fn fan_out_monitor_lost_for_identity(dead_node: crate::node_identity::NodeId) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    let monitor_downs = rt.monitors.take_monitor_downs_for_node(dead_node);
    for down in monitor_downs {
        rt.monitors.enqueue_lost(down);
    }
    // Link half: fire the cross-node link cascade for every still-Pending LINK
    // watcher on the dead node — the death-signal must fire on the PARTITION
    // terminal cause too (firing only on a clean exit / crash would fail-open: a
    // linked actor surviving its dead peer). For CrashLinked this synthesizes a
    // HewSysMsg::Exit into the LOCAL linked actor's mailbox and crashes it. The
    // one-shot slot makes this exactly-once vs a definitive CTRL_LINK_DOWN that
    // may already have fired.
    let link_downs = rt.monitors.take_link_downs_for_node(dead_node);
    for down in link_downs {
        let _ = crate::link::deliver_cross_node_link_exit(
            down.local_actor_id,
            down.target,
            down.reason,
            down.policy_tag,
            down.crash_kind,
        );
    }
    // Prune target-side watcher entries (monitor AND link) the dead node had
    // registered, so the table stays bounded under node churn (target-side watcher
    // prune; covers link watchers via the same map).
    let _ = rt.monitors.prune_remote_watchers_for_node(dead_node);
}

pub(crate) fn fan_out_monitor_lost_for_session(
    dead_node: crate::node_identity::NodeId,
    session_incarnation: u32,
) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    let monitor_downs = rt
        .monitors
        .take_monitor_downs_for_session(dead_node, session_incarnation);
    for down in monitor_downs {
        rt.monitors.enqueue_lost(down);
    }
    let link_downs = rt
        .monitors
        .take_link_downs_for_session(dead_node, session_incarnation);
    for down in link_downs {
        let _ = crate::link::deliver_cross_node_link_exit(
            down.local_actor_id,
            down.target,
            down.reason,
            down.policy_tag,
            down.crash_kind,
        );
    }
    let _ = rt
        .monitors
        .prune_remote_watchers_for_session(dead_node, session_incarnation);
}

/// Fail every pending remote ask routed to a SWIM-declared-DEAD node with
/// [`AskError::Partition`].
///
/// The cluster's recv-queue partition fan-out (`PartitionRegistry::on_member_dead`,
/// `cluster.rs`) wakes a blocked `recv` to `PartitionDetected`, but it does not
/// touch the reply table — so a pending remote *ask* to a peer the failure
/// detector has declared dead (while its TCP socket may still be nominally open)
/// would otherwise hang to the caller's own deadline. This is the request/reply
/// analog of that recv-side seam: it resolves those pending asks IMMEDIATELY
/// with the typed `Partition` cause instead of a silent wait-to-timeout.
///
/// The reply table is keyed by `(conn_mgr, conn_id)`; a remote ask registers
/// under the connection the routing table resolves for the target node. We look
/// up the dead node's connection the same way and fail every pending ask on it.
///
/// Exactly-once vs the connection-drop path: `fail_connection_with_reason`
/// drains the matching entries under the table lock before failing them, so if
/// the socket also drops (or already dropped), whichever verdict lands first
/// resolves the ask and the second finds an empty map — no double wake. A node
/// with no route to the dead peer (no pending asks) is a no-op.
pub(crate) fn fail_remote_asks_for_node(dead_node_id: u16) {
    with_current_node_read(|guard| {
        let node_ptr = *guard as *const HewNode;
        if node_ptr.is_null() {
            return;
        }
        // SAFETY: the read lock pins CURRENT_NODE for the duration of this call.
        let node = unsafe { &*node_ptr };
        if node.routing_table.is_null() || node.conn_mgr.is_null() {
            return;
        }
        // SAFETY: routing_table is valid while the node is installed.
        //
        // Replacement-connection window: this lookup returns the CURRENT conn_id
        // for `dead_node_id` at the time of the SWIM-DEAD verdict. If the routing
        // table has already been updated to a replacement connection (old socket
        // retired, new connection established and routed), `conn_id` here is the
        // NEW conn's id. Pending asks on the OLD conn that are still draining in
        // the old reader's cleanup will NOT be reached by this fan-out — they
        // resolve via `reader_cleanup` → `ConnectionDropped` rather than
        // `Partition`. This is already fail-closed (the dying reader does fail
        // them; no ask can hang) and is a cosmetic reason-label mismatch only.
        //
        // The real fix — keying the reply table on (node_id, conn_generation) so
        // a DEAD verdict can reach a retired conn's draining asks — requires a
        // reply-table schema change and an exactly-once proof; it is deferred.
        let conn_id = unsafe {
            crate::routing::hew_routing_conn_for_route_slot(node.routing_table, dead_node_id)
        };
        if conn_id < 0 {
            // No route to the dead peer on this node — no pending asks to fail.
            return;
        }
        let connection = ConnectionKey::new(node.conn_mgr.cast_const(), conn_id);
        if let Some(table) = reply_table_opt() {
            crate::grant_diagnostic_event(format_args!(
                "partition-origin=swim-dead route_slot={dead_node_id} conn={conn_id}"
            ));
            table.fail_connection_with_reason(connection, AskError::Partition);
        }
    });
}

/// Node-side reaction to a SWIM-DEAD verdict: fail every pending remote ask to
/// the dead peer AND quarantine it at the incarnation it died at.
///
/// Both per-peer tables react at the verdict: the reply-table fan-out resolves
/// in-flight asks with [`AskError::Partition`], and the quarantine insert records
/// `(node_id, incarnation)` so a `Quarantine`-policy send/ask to the peer fails
/// closed until it rejoins at a strictly higher incarnation. The two
/// `MEMBER_DEAD` seams in `cluster.rs` route through this single wrapper so
/// neither can quarantine without also failing the asks, nor vice versa.
pub(crate) fn fail_remote_and_quarantine(dead_node_id: u16, dead_incarnation: u64) {
    fail_remote_asks_for_node(dead_node_id);
    quarantine_insert(dead_node_id, dead_incarnation);
}

/// Node-side reaction to a strictly-higher-incarnation readmission: evict the
/// returned peer from the quarantine set so it is sendable again. The admission
/// gate (`cluster.rs`) has already proved the new incarnation is strictly higher,
/// so the node side only clears.
pub(crate) fn readmit_node_clear_quarantine(node_id: u16) {
    quarantine_evict(node_id);
}

/// One ordinary observation owner, shared by local and admitted actor values.
/// Setup acknowledgement, cancellation and its readiness registration retire together.
pub struct HewNativeActorObservation {
    pending: Option<(u64, Arc<PendingReply>)>,
    timer: *mut crate::coro_sleep::HewCoroSleep,
    remote: Option<(Location, crate::monitor::ObservationAuthority)>,
    ref_id: u64,
    actor_id: u64,
    local_link: Option<u64>,
    kind: u8,
    policy: u8,
    status: Option<i32>,
    taken: bool,
    completion: Option<Arc<crate::actor_native::NativeActorCompletion>>,
}

impl std::fmt::Debug for HewNativeActorObservation {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("HewNativeActorObservation")
            .field("ref_id", &self.ref_id)
            .field("status", &self.status)
            .finish_non_exhaustive()
    }
}

impl HewNativeActorObservation {
    fn poll(&mut self) -> bool {
        use crate::util::MutexExt;
        if self.status.is_some() {
            return true;
        }
        let Some((setup_id, pending)) = &self.pending else {
            return false;
        };
        let outcome = pending.outcome.lock_or_recover().take();
        // SAFETY: this operation owns its timer until drop.
        let expired = unsafe { crate::coro_sleep::hew_coro_sleep_status(self.timer) } != 0;
        if outcome.is_none() && !expired {
            return false;
        }
        let status = outcome
            .and_then(|outcome| {
                if outcome.status != ReplyStatus::Success {
                    return None;
                }
                crate::envelope::decode_setup_result_payload(&outcome.data).ok()
            })
            .filter(|result| {
                result.setup_id == *setup_id
                    && result.ref_id == self.ref_id
                    && self
                        .remote
                        .is_some_and(|(target, _)| result.target == target)
            })
            .map_or(crate::envelope::SETUP_STATUS_TARGET_GONE, |result| {
                result.status
            });
        reply_table().remove(*setup_id);
        self.pending = None;
        self.status = Some(match status {
            crate::envelope::SETUP_STATUS_ACCEPTED => 0,
            crate::envelope::SETUP_STATUS_REFUSED => {
                -crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED
            }
            crate::envelope::SETUP_STATUS_RESOURCE_EXHAUSTED => {
                -crate::internal::types::HEW_TRAP_HEAP_EXCEEDED
            }
            _ => {
                let (target, _) = self.remote.expect("remote setup owns target");
                if self.kind == 1 {
                    link_unreachable(
                        crate::runtime::rt_current(),
                        Some(self.ref_id),
                        self.actor_id,
                        target,
                        self.policy,
                    );
                } else if let Some(down) = crate::runtime::rt_current()
                    .monitors
                    .deliver_monitor_to_ref(self.ref_id, target)
                {
                    // A target-gone ACK is conclusive only for an authorized live
                    // publication. Transport loss and timeout remain unknown fate.
                    crate::runtime::rt_current().monitors.enqueue_lost(down);
                }
                0
            }
        });
        if self.status != Some(0) {
            if let Some(completion) = &self.completion {
                completion.finish(crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED);
            }
        }
        true
    }
}

impl Drop for HewNativeActorObservation {
    fn drop(&mut self) {
        if let Some((setup_id, _)) = self.pending.take() {
            reply_table().remove(setup_id);
        }
        if !self.timer.is_null() {
            // SAFETY: the unique operation owns this timer.
            unsafe { crate::coro_sleep::hew_coro_sleep_free(self.timer) };
        }
        if !self.taken {
            if let Some((target, authority)) = self.remote {
                crate::runtime::rt_current()
                    .monitors
                    .remove_remote_observation(self.ref_id);
                send_admitted_cleanup(target, authority, self.ref_id, self.kind == 1);
            } else if self.status == Some(0) {
                if self.kind == 0 {
                    crate::monitor::hew_actor_demonitor(self.ref_id);
                } else if let Some(target) = self.local_link {
                    crate::link::unlink_by_ids(self.actor_id, target);
                }
            }
        }
    }
}

/// Begin a monitor (kind 0) or link (kind 1), capturing the current execution
/// context once. Local registration completes inline; imported setup suspends.
/// # Safety
/// The caller borrows the actor token and wake descriptor throughout creation.
#[no_mangle]
pub unsafe extern "C" fn hew_native_actor_observe_new(
    token: crate::lifetime::local_handles::HewLocalPidId,
    kind: u8,
    waker: *const crate::wake::HewWaker,
) -> *mut HewNativeActorObservation {
    let mut operation = HewNativeActorObservation {
        pending: None,
        timer: std::ptr::null_mut(),
        remote: None,
        ref_id: 0,
        actor_id: 0,
        local_link: None,
        kind,
        policy: crate::execution_context::current_partition_policy() as u8,
        status: None,
        taken: false,
        completion: None,
    };
    let actor = crate::actor::hew_actor_self();
    if actor.is_null() {
        operation.status = Some(HEW_OBSERVATION_NO_CONTEXT);
    } else if kind > 1 {
        operation.status = Some(-crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED);
    } else {
        // SAFETY: the current activation owns its actor allocation.
        operation.actor_id = unsafe { (*actor).id };
        if let Some(imported) = crate::lifetime::local_handles::current_imported_actor(token) {
            // SAFETY: forwarded live wake descriptor contract.
            unsafe { start_imported_observation(&mut operation, &imported, waker) };
        } else {
            operation.status = Some(if kind == 0 {
                // SAFETY: the local stack output is writable.
                unsafe {
                    crate::monitor::hew_native_actor_monitor(token, &raw mut operation.ref_id)
                }
            } else {
                let (status, selected) = crate::link::link_actor_for_observation(token);
                operation.local_link = selected;
                status
            });
        }
    }
    Box::into_raw(Box::new(operation))
}

#[expect(
    clippy::too_many_lines,
    reason = "one observation owner captures grant, publication, readiness and expiry"
)]
unsafe fn start_imported_observation(
    operation: &mut HewNativeActorObservation,
    imported: &super::service_api::ImportedService,
    waker: *const crate::wake::HewWaker,
) {
    use crate::util::MutexExt;
    let result = super::service_api::with_imported_actor(imported, |manager, claim, offer| {
        let rt = crate::runtime::rt_current();
        let ref_id = rt
            .monitors
            .next_observation_id()
            .ok_or(AskError::WorkerAtCapacity)?;
        // A stopped observer outside actor dispatch uses its unique observation
        // id as a return endpoint; only links require a live actor endpoint.
        let watcher = connection::local_location_for_actor(manager, operation.actor_id)
            .or_else(|| {
                Location::new(
                    offer.recipient.node_id,
                    ref_id,
                    offer.recipient.session_incarnation,
                )
                .ok()
            })
            .ok_or(AskError::StaleRef)?;
        let authority = crate::monitor::ObservationAuthority {
            grant: offer.grant_id,
            publication: claim.publication_token,
            watcher,
        };
        let setup_kind = if operation.kind == 1 {
            RemoteSetupKind::Link
        } else {
            RemoteSetupKind::Monitor
        };
        // SAFETY: the caller retains this descriptor throughout creation.
        let owned = unsafe { crate::wake::OwnedWaker::retain(&*waker) };
        let (setup_id, pending) = reply_table().register_with_waker(
            ConnectionKey::new(manager, claim.connection_id()),
            Some(owned),
            super::PendingReplyKind::Setup(setup_kind, claim.publication_token),
        );
        let encoded = (|| {
            let payload = if operation.kind == 1 {
                crate::envelope::encode_link_req_payload(&crate::envelope::LinkReqPayload {
                    grant_id: offer.grant_id,
                    reciprocal_ref: 0,
                    linker: watcher,
                    ref_id,
                    target: offer.target,
                    policy_tag: operation.policy,
                    reciprocate: 1,
                    setup_id,
                })
            } else {
                crate::envelope::encode_monitor_req_payload(&crate::envelope::MonitorReqPayload {
                    grant_id: offer.grant_id,
                    watcher,
                    ref_id,
                    target: offer.target,
                    setup_id,
                })
            }
            .map_err(|_| AskError::EncodeFailed)?;
            encode_node_control_frame(
                if operation.kind == 1 {
                    crate::envelope::CTRL_LINK_REQ
                } else {
                    crate::envelope::CTRL_MONITOR_REQ
                },
                payload,
            )
            .ok_or(AskError::EncodeFailed)
        })();
        let bytes = match encoded {
            Ok(bytes) => bytes,
            Err(error) => {
                reply_table().remove(setup_id);
                return Err(error);
            }
        };
        rt.monitors.insert_admitted_observation(
            ref_id,
            offer.target,
            if operation.kind == 1 {
                crate::monitor::WatcherAction::Link {
                    local_actor_id: operation.actor_id,
                    policy_tag: operation.policy,
                }
            } else {
                crate::monitor::WatcherAction::Monitor {
                    watcher_actor_id: operation.actor_id,
                }
            },
            authority,
            operation.completion.clone(),
        );
        operation.remote = Some((offer.target, authority));
        operation.ref_id = ref_id;
        operation.pending = Some((setup_id, pending));
        // SAFETY: the timer retains the descriptor; this operation owns its handle.
        operation.timer = unsafe { crate::coro_sleep::hew_coro_sleep_new(5_000_000_000, waker) };
        let table = Arc::clone(&rt.node.reply_table);
        let completion_table = Arc::clone(&table);
        if !claim.submit(
            move || {
                table
                    .pending
                    .lock_or_recover()
                    .contains_key(&setup_id)
                    .then_some(bytes)
            },
            move |result| {
                if result.is_err() {
                    completion_table.fail(setup_id, AskError::ConnectionDropped);
                }
            },
        ) {
            reply_table().fail(setup_id, AskError::SendFailed);
        }
        Ok(())
    });
    if result == Err(AskError::ConnectionDropped) {
        // The imported owner and recipient session were validated before route
        // selection failed. Report lost reachability without publishing setup.
        let rt = crate::runtime::rt_current();
        if let Some(ref_id) = rt.monitors.next_observation_id() {
            operation.ref_id = ref_id;
            if operation.kind == 1 {
                link_unreachable(
                    rt,
                    None,
                    operation.actor_id,
                    imported.identity(),
                    operation.policy,
                );
            } else {
                rt.monitors.enqueue_lost(crate::monitor::MonitorDown {
                    completion: operation.completion.clone(),
                    monitor_id: ref_id,
                    watcher_actor_id: operation.actor_id,
                    target: imported.identity(),
                });
            }
            operation.status = Some(0);
            return;
        }
    }
    if let Err(error) = result {
        operation.status = Some(-if error == AskError::WorkerAtCapacity {
            crate::internal::types::HEW_TRAP_HEAP_EXCEEDED
        } else {
            crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED
        });
        if let Some(completion) = &operation.completion {
            completion.finish(crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED);
        }
    }
}

/// Return -1 while setup is pending, or 0 once its result can be taken.
/// # Safety
/// `operation` is uniquely borrowed and live.
#[no_mangle]
pub unsafe extern "C" fn hew_native_actor_observe_poll(
    operation: *mut HewNativeActorObservation,
) -> i32 {
    // SAFETY: the caller owns this operation.
    if unsafe { operation.as_mut() }.is_none_or(HewNativeActorObservation::poll) {
        0
    } else {
        -1
    }
}

/// Transfer a completed setup result. Only successful monitor results initialize
/// `out_id`; links have no source-visible registration id.
/// # Safety
/// The operation is live, ready and uniquely borrowed; `out_id` is writable.
/// # Panics
/// Panics if the caller has not acquired readiness before taking the result.
#[no_mangle]
pub unsafe extern "C" fn hew_native_actor_observe_take(
    operation: *mut HewNativeActorObservation,
    out_id: *mut u64,
) -> i32 {
    // SAFETY: the caller owns the completed operation and output slot.
    let Some(operation) = (unsafe { operation.as_mut() }) else {
        return -crate::internal::types::HEW_TRAP_HEAP_EXCEEDED;
    };
    let status = operation
        .status
        .expect("observation setup ready before take");
    if status == 0 {
        if operation.kind == 0 {
            // SAFETY: the caller provides a writable monitor output slot.
            unsafe { out_id.write(operation.ref_id) };
        }
        operation.taken = true;
    }
    status
}

/// Cancel unfinished setup or release a taken operation.
/// # Safety
/// The caller transfers its unique operation owner; no concurrent poll remains.
#[no_mangle]
pub unsafe extern "C" fn hew_native_actor_observe_free(operation: *mut HewNativeActorObservation) {
    // SAFETY: the unique operation owner is transferred here.
    if !operation.is_null() {
        // SAFETY: the caller transfers the unique non-null operation owner.
        drop(unsafe { Box::from_raw(operation) });
    }
}

pub(crate) fn send_admitted_cleanup(
    target: Location,
    authority: crate::monitor::ObservationAuthority,
    ref_id: u64,
    link: bool,
) {
    with_current_node_read(|guard| {
        // SAFETY: the read guard pins the node and manager.
        let Some(node) = (unsafe { (*guard as *const HewNode).as_ref() }) else {
            return;
        };
        if node.conn_mgr.is_null() {
            return;
        }
        // SAFETY: the node read guard pins this manager.
        let manager = unsafe { &*node.conn_mgr };
        let Some(claim) =
            connection::claim_send_by_identity(manager, target.node(), Some(authority.publication))
        else {
            return;
        };
        let payload = if link {
            crate::envelope::encode_link_req_payload(&crate::envelope::LinkReqPayload {
                grant_id: authority.grant,
                reciprocal_ref: 0,
                linker: authority.watcher,
                ref_id,
                target,
                policy_tag: 0,
                reciprocate: 1,
                setup_id: 0,
            })
        } else {
            crate::envelope::encode_monitor_req_payload(&crate::envelope::MonitorReqPayload {
                grant_id: authority.grant,
                watcher: authority.watcher,
                ref_id,
                target,
                setup_id: 0,
            })
        };
        if let Some(bytes) = payload.ok().and_then(|payload| {
            encode_node_control_frame(
                if link {
                    crate::envelope::CTRL_UNLINK
                } else {
                    crate::envelope::CTRL_DEMONITOR
                },
                payload,
            )
        }) {
            claim.submit(move || Some(bytes), |_| {});
        }
    });
}

pub(crate) unsafe fn imported_actor_wait(
    imported: &super::service_api::ImportedService,
    waker: *const crate::wake::HewWaker,
) -> (
    Arc<crate::actor_native::NativeActorCompletion>,
    Box<HewNativeActorObservation>,
) {
    let completion = Arc::new(crate::actor_native::NativeActorCompletion::default());
    let mut operation = Box::new(HewNativeActorObservation {
        pending: None,
        timer: std::ptr::null_mut(),
        remote: None,
        ref_id: 0,
        actor_id: 0,
        local_link: None,
        kind: 0,
        policy: 0,
        status: None,
        taken: false,
        completion: Some(completion.clone()),
    });
    // SAFETY: the caller retains this descriptor throughout registration.
    unsafe { start_imported_observation(&mut operation, imported, waker) };
    (completion, operation)
}
