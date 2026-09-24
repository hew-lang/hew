//! Remote monitor and link setup, teardown and failure fan-out.

use super::{
    quarantine_evict, quarantine_insert, reply_table, reply_table_opt, with_current_node_read,
    AskError, ConnectionKey, HewNode, PendingReply, RemoteSetupKind, ReplyStatus, SendConnMgr,
    HEW_ERR_STALE_REF, NODE_STATE_RUNNING,
};
use crate::connection;
use crate::connection::HewConnMgr;
use crate::node_identity::{HewRemotePid, Location};
use crate::routing;
use crate::set_last_error;
use crate::util::CondvarExt;
use std::ffi::c_int;
use std::sync::atomic::{AtomicU8, AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;

// ── Cross-node monitor ───────────────────────────────────────────────────────

const fn setup_error(variant: i32) -> i32 {
    variant + 1
}

// MonitorError declaration order in std/link_monitor.hew.
pub(super) const MONITOR_ERR_NODE_NOT_RUNNING: i32 = setup_error(0);
const MONITOR_ERR_INVALID_TARGET: i32 = setup_error(1);
const MONITOR_ERR_PARTITION: i32 = setup_error(2);
const MONITOR_ERR_STALE_REF: i32 = setup_error(3);
const MONITOR_ERR_ENCODE_FAILURE: i32 = setup_error(4);
pub(super) const MONITOR_ERR_LOCAL_SHUTDOWN: i32 = setup_error(5);
pub(super) const MONITOR_ERR_RESOURCE_EXHAUSTED: i32 = setup_error(10);

// LinkError declaration order in std/builtins.hew: Dead, Partition, NoContext.
// A pid that no longer names a live incarnation is Dead; every failure to
// reach or register with the peer leaves it unreachable.
pub(super) const LINK_ERR_DEAD: i32 = setup_error(0);
pub(super) const LINK_ERR_PARTITION: i32 = setup_error(1);
pub(super) const LINK_ERR_NO_CURRENT_ACTOR: i32 = setup_error(2);
const LINK_ERR_NODE_NOT_RUNNING: i32 = LINK_ERR_PARTITION;
const LINK_ERR_INVALID_TARGET: i32 = LINK_ERR_DEAD;
const LINK_ERR_STALE_REF: i32 = LINK_ERR_DEAD;
const LINK_ERR_ENCODE_FAILURE: i32 = LINK_ERR_PARTITION;
const LINK_ERR_LOCAL_SHUTDOWN: i32 = LINK_ERR_PARTITION;
const LINK_ERR_RESOURCE_EXHAUSTED: i32 = LINK_ERR_PARTITION;
const REMOTE_SETUP_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(5);
const REVERSE_LINK_SETUP_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(4);
const SETUP_RACE_MONITOR: u8 = 1;
const SETUP_RACE_LINK: u8 = 2;
static SETUP_RACE_KIND: AtomicU8 = AtomicU8::new(0);
const REVERSE_LINK_SETUP_WORKER_LIMIT: usize = 64;
static REVERSE_LINK_SETUP_ACTIVE: AtomicUsize = AtomicUsize::new(0);

struct ReverseLinkSetupGuard(Arc<AtomicUsize>);

impl Drop for ReverseLinkSetupGuard {
    fn drop(&mut self) {
        REVERSE_LINK_SETUP_ACTIVE.fetch_sub(1, Ordering::AcqRel);
        self.0.fetch_sub(1, Ordering::SeqCst);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SetupSendError {
    StaleRef,
    Encode,
    Send,
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
    ctrl_kind: u64,
    payload: Vec<u8>,
) -> c_int {
    let Some(bytes) = encode_node_control_frame(ctrl_kind, payload) else {
        return -1;
    };
    // SAFETY: caller pins the manager and connection for this call; send copies
    // the complete encoded frame.
    unsafe {
        connection::hew_connmgr_send_preencoded(conn_mgr, conn_id, bytes.as_ptr(), bytes.len())
    }
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
    // SAFETY: caller pins the manager; the connection layer additionally
    // requires this exact publication token before copying the frame.
    unsafe {
        connection::hew_connmgr_send_preencoded_claimed(
            conn_mgr,
            conn_id,
            claim_token,
            bytes.as_ptr(),
            bytes.len(),
        )
    }
}

unsafe fn send_acknowledged_setup(
    conn_mgr: *mut HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    ctrl_kind: u64,
    kind: RemoteSetupKind,
    encode_payload: impl FnOnce(u64) -> Result<Vec<u8>, crate::envelope::MonitorPayloadError>,
) -> Result<(u64, Arc<PendingReply>), SetupSendError> {
    let prepared = prepare_acknowledged_setup(
        conn_mgr,
        conn_id,
        publication_token,
        ctrl_kind,
        kind,
        encode_payload,
    )?;
    // SAFETY: inherited manager/connection validity from the caller.
    unsafe {
        send_prepared_acknowledged_setup(conn_mgr, conn_id, publication_token, &prepared)?;
    }
    Ok((prepared.setup_id, prepared.pending))
}

struct PreparedRemoteSetup {
    setup_id: u64,
    pending: Arc<PendingReply>,
    bytes: Vec<u8>,
}

fn prepare_acknowledged_setup(
    conn_mgr: *mut HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    ctrl_kind: u64,
    kind: RemoteSetupKind,
    encode_payload: impl FnOnce(u64) -> Result<Vec<u8>, crate::envelope::MonitorPayloadError>,
) -> Result<PreparedRemoteSetup, SetupSendError> {
    let (setup_id, pending) = reply_table().register_setup(
        ConnectionKey::new(conn_mgr, conn_id),
        kind,
        publication_token,
    );
    let payload = match encode_payload(setup_id) {
        Ok(payload) => payload,
        Err(err) => {
            reply_table().remove(setup_id);
            set_last_error(format!("remote setup payload encode failure: {err}"));
            return Err(SetupSendError::Encode);
        }
    };
    let Some(bytes) = encode_node_control_frame(ctrl_kind, payload) else {
        reply_table().remove(setup_id);
        return Err(SetupSendError::Encode);
    };
    Ok(PreparedRemoteSetup {
        setup_id,
        pending,
        bytes,
    })
}

unsafe fn send_prepared_acknowledged_setup(
    conn_mgr: *mut HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    prepared: &PreparedRemoteSetup,
) -> Result<(), SetupSendError> {
    // SAFETY: caller pins the manager; the connection layer additionally
    // requires this exact publication token before copying the frame.
    if unsafe {
        connection::hew_connmgr_send_preencoded_claimed(
            conn_mgr,
            conn_id,
            publication_token,
            prepared.bytes.as_ptr(),
            prepared.bytes.len(),
        )
    } != 0
    {
        reply_table().remove(prepared.setup_id);
        return Err(SetupSendError::Send);
    }
    Ok(())
}

fn wait_for_setup_result(
    setup_id: u64,
    pending: &PendingReply,
    expected_ref_id: u64,
    expected_target: Location,
) -> u8 {
    wait_for_setup_result_with_timeout(
        setup_id,
        pending,
        expected_ref_id,
        expected_target,
        REMOTE_SETUP_TIMEOUT,
    )
}

fn wait_for_setup_result_with_timeout(
    setup_id: u64,
    pending: &PendingReply,
    expected_ref_id: u64,
    expected_target: Location,
    timeout: std::time::Duration,
) -> u8 {
    let deadline = std::time::Instant::now() + timeout;
    let mut outcome = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    while outcome.is_none() {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            reply_table().remove(setup_id);
            return crate::envelope::SETUP_STATUS_TARGET_GONE;
        }

        let (next, wait_result) = pending.cond.wait_timeout_or_recover(outcome, remaining);
        outcome = next;
        if wait_result.timed_out() && outcome.is_none() {
            reply_table().remove(setup_id);
            return crate::envelope::SETUP_STATUS_TARGET_GONE;
        }
    }

    let Some(result) = outcome.take() else {
        return crate::envelope::SETUP_STATUS_TARGET_GONE;
    };
    if result.status != ReplyStatus::Success {
        return crate::envelope::SETUP_STATUS_TARGET_GONE;
    }
    match crate::envelope::decode_setup_result_payload(&result.data) {
        Ok(payload)
            if payload.setup_id == setup_id
                && payload.ref_id == expected_ref_id
                && payload.target == expected_target =>
        {
            payload.status
        }
        Ok(_) => {
            set_last_error("remote setup result did not match the pending request");
            crate::envelope::SETUP_STATUS_TARGET_GONE
        }
        Err(err) => {
            set_last_error(format!("remote setup result decode failure: {err}"));
            crate::envelope::SETUP_STATUS_TARGET_GONE
        }
    }
}

unsafe fn acquire_reverse_link_setup_worker(
    conn_mgr: *mut HewConnMgr,
) -> Result<ReverseLinkSetupGuard, u8> {
    let active = REVERSE_LINK_SETUP_ACTIVE.fetch_add(1, Ordering::AcqRel);
    if active >= REVERSE_LINK_SETUP_WORKER_LIMIT {
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
    let deadline = std::time::Instant::now() + REMOTE_SETUP_TIMEOUT;
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
    Unlink(crate::envelope::LinkReqPayload),
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
        ObservationNotice::Unlink(payload) => crate::envelope::encode_link_req_payload(payload)
            .map(|bytes| (crate::envelope::CTRL_UNLINK, bytes)),
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
    unsafe { send_control_frame_on_connection(node.conn_mgr, conn_id, ctrl_kind, payload) }
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
        ObservationNotice::Unlink(payload) => handle_inbound_unlink(&payload),
        ObservationNotice::MonitorDown(payload) => handle_inbound_monitor_down(&payload),
        ObservationNotice::LinkDown(payload) => {
            handle_inbound_link_down(payload.ref_id, payload.target, payload.reason);
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

fn current_node_accepts_observations() -> Option<bool> {
    with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: the current-node read lock pins the node.
        Some(unsafe { (*node).state.load(Ordering::Acquire) == NODE_STATE_RUNNING })
    })
}

/// Send a peer the acknowledged monitor setup request for `target`.
fn monitor_setup_request(
    watcher: Location,
    ref_id: u64,
    target: Location,
) -> Option<Result<(u64, Arc<PendingReply>), SetupSendError>> {
    with_current_node_read(|guard| {
        if *guard == 0 {
            set_last_error("hew_node_monitor_location: no current node");
            return None;
        }
        let node = *guard as *const HewNode;
        // SAFETY: read lock pins the current node pointer for this call.
        let node_ref = unsafe { &*node };
        // SAFETY: node_ref pins the routing table and manager for this call.
        let conn_id = match unsafe { resolve_monitor_connection(node_ref, target) } {
            Ok(conn_id) => conn_id,
            Err(HEW_ERR_STALE_REF) => return Some(Err(SetupSendError::StaleRef)),
            Err(_) => return Some(Err(SetupSendError::Send)),
        };
        // SAFETY: the current-node read lock pins this manager for the lookup.
        let Some(publication_token) = (unsafe {
            connection::hew_connmgr_publication_token_for_target(node_ref.conn_mgr, conn_id, target)
        }) else {
            return Some(Err(SetupSendError::Send));
        };
        // SAFETY: node_ref pins the manager/connection while the frame is encoded
        // and copied into the transport.
        Some(unsafe {
            send_acknowledged_setup(
                node_ref.conn_mgr,
                conn_id,
                publication_token,
                crate::envelope::CTRL_MONITOR_REQ,
                RemoteSetupKind::Monitor,
                |setup_id| {
                    crate::envelope::encode_monitor_req_payload(
                        &crate::envelope::MonitorReqPayload {
                            watcher,
                            ref_id,
                            target,
                            setup_id,
                        },
                    )
                },
            )
        })
    })
}

/// `monitor(RemotePid<T>)` → register a cross-node monitor.
///
/// Resolves the current node, records a watcher-side entry keyed by a fresh
/// `ref_id`, and sends a `CTRL_MONITOR_REQ` to the node owning `target_pid` so
/// that node will fan a `CTRL_MONITOR_DOWN` back when the target reaches a
/// terminal state. The returned `ref_id` is assembled into the `MonitorRef`
/// value. Returns status 0 and writes `out_monitor_id` on success; non-zero
/// statuses are one-based `MonitorError` discriminants.
///
/// # Safety
///
/// `target` and `out_monitor_id` must point to writable/readable storage.
#[no_mangle]
#[allow(
    clippy::too_many_lines,
    reason = "function coordinates checked actor identity, route, observation, and wire setup"
)]
pub unsafe extern "C" fn hew_node_monitor_location(
    target: *const HewRemotePid,
    out_monitor_id: *mut u64,
) -> i32 {
    if target.is_null() || out_monitor_id.is_null() {
        return MONITOR_ERR_INVALID_TARGET;
    }
    // SAFETY: caller guarantees `target` is readable.
    let Ok(target) = Location::try_from(unsafe { *target }) else {
        return MONITOR_ERR_STALE_REF;
    };
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("hew_node_monitor_location: no runtime installed");
        return MONITOR_ERR_NODE_NOT_RUNNING;
    };
    let self_actor = crate::actor::hew_actor_self();
    if self_actor.is_null() {
        set_last_error("hew_node_monitor_location: no current actor (monitor watcher)");
        return MONITOR_ERR_INVALID_TARGET;
    }
    match current_node_accepts_observations() {
        Some(true) => {}
        Some(false) => {
            set_last_error("hew_node_monitor_location: node is shutting down");
            return MONITOR_ERR_LOCAL_SHUTDOWN;
        }
        None => {
            set_last_error("hew_node_monitor_location: no active node");
            return MONITOR_ERR_NODE_NOT_RUNNING;
        }
    }
    // SAFETY: hew_actor_self returned a live actor pointer.
    let watcher_actor_id = unsafe { (*self_actor).id };
    let Some(target_route) = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: current-node read lock pins the node.
        Some(unsafe { routing::hew_routing_lookup_location((*node).routing_table, target) })
    }) else {
        set_last_error("hew_node_monitor_location: no active node");
        return MONITOR_ERR_NODE_NOT_RUNNING;
    };
    if matches!(target_route, routing::LocationRoute::StaleRef) {
        set_last_error("hew_node_monitor_location: target Location is stale");
        return MONITOR_ERR_STALE_REF;
    }
    if matches!(target_route, routing::LocationRoute::Partition) {
        set_last_error("hew_node_monitor_location: target identity is partitioned");
        return MONITOR_ERR_PARTITION;
    }
    let Some(watcher) = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: the current-node read lock pins the node.
        let node = unsafe { &*node };
        local_actor_location(node, watcher_actor_id)
    }) else {
        set_last_error("hew_node_monitor_location: exact watcher/target Location is unavailable");
        return MONITOR_ERR_STALE_REF;
    };

    // Record the watcher entry first so the connection-drop / SWIM-DEAD fan-out
    // can deliver even if the request send races a drop.
    let Some(ref_id) = rt
        .monitors
        .register_remote_monitor(target, watcher_actor_id)
    else {
        set_last_error("hew_node_monitor_location: monitor id space exhausted");
        return MONITOR_ERR_RESOURCE_EXHAUSTED;
    };
    if current_node_accepts_observations() != Some(true) {
        rt.monitors.remove_remote_observation(ref_id);
        set_last_error("hew_node_monitor_location: node shut down during monitor setup");
        return MONITOR_ERR_LOCAL_SHUTDOWN;
    }

    let setup = if let routing::LocationRoute::Local { actor_id } = target_route {
        // This node owns the target: register its watcher as this node's
        // connection reader would for a peer's request.
        Some(Ok(
            match rt
                .monitors
                .register_remote_watcher(actor_id, watcher, ref_id)
            {
                crate::monitor::RemoteWatcherSetup::Registered => {
                    crate::envelope::SETUP_STATUS_ACCEPTED
                }
                crate::monitor::RemoteWatcherSetup::TargetGone => {
                    crate::envelope::SETUP_STATUS_TARGET_GONE
                }
            },
        ))
    } else {
        monitor_setup_request(watcher, ref_id, target).map(|sent| {
            sent.map(|(setup_id, pending)| {
                wait_for_setup_result(setup_id, &pending, ref_id, target)
            })
        })
    };
    match setup {
        Some(Ok(status)) => {
            match status {
                crate::envelope::SETUP_STATUS_ACCEPTED => {}
                crate::envelope::SETUP_STATUS_RESOURCE_EXHAUSTED => {
                    send_remote_demonitor(ref_id, target, watcher_actor_id);
                    rt.monitors.remove_remote_observation(ref_id);
                    return MONITOR_ERR_RESOURCE_EXHAUSTED;
                }
                _ => {
                    send_remote_demonitor(ref_id, target, watcher_actor_id);
                    if let Some(down) = rt.monitors.deliver_monitor_to_ref(ref_id, target) {
                        rt.monitors.enqueue_lost(down);
                    }
                }
            }
            // SAFETY: caller provided a writable out pointer; write only on success.
            unsafe { *out_monitor_id = ref_id };
            0
        }
        Some(Err(SetupSendError::StaleRef)) => {
            rt.monitors.remove_remote_observation(ref_id);
            MONITOR_ERR_STALE_REF
        }
        Some(Err(SetupSendError::Encode)) => {
            rt.monitors.remove_remote_observation(ref_id);
            MONITOR_ERR_ENCODE_FAILURE
        }
        Some(Err(SetupSendError::Send)) => {
            rt.monitors.remove_remote_observation(ref_id);
            if current_node_accepts_observations() == Some(true) {
                MONITOR_ERR_PARTITION
            } else {
                MONITOR_ERR_LOCAL_SHUTDOWN
            }
        }
        None => {
            rt.monitors.remove_remote_observation(ref_id);
            MONITOR_ERR_LOCAL_SHUTDOWN
        }
    }
}

/// Send a peer the acknowledged link setup request for `target`.
fn link_setup_request(
    linker: Location,
    ref_id: u64,
    target: Location,
    policy_tag: u8,
) -> Option<Result<(u64, Arc<PendingReply>), SetupSendError>> {
    with_current_node_read(|guard| {
        if *guard == 0 {
            set_last_error("hew_node_link_remote_location: no current node");
            return None;
        }
        let node = *guard as *const HewNode;
        // SAFETY: read lock pins the current node pointer for this call.
        let node_ref = unsafe { &*node };
        // SAFETY: node_ref pins the routing table and manager for this call.
        let conn_id = match unsafe { resolve_monitor_connection(node_ref, target) } {
            Ok(conn_id) => conn_id,
            Err(HEW_ERR_STALE_REF) => return Some(Err(SetupSendError::StaleRef)),
            Err(_) => return Some(Err(SetupSendError::Send)),
        };
        // SAFETY: the current-node read lock pins this manager for the lookup.
        let Some(publication_token) = (unsafe {
            connection::hew_connmgr_publication_token_for_target(node_ref.conn_mgr, conn_id, target)
        }) else {
            return Some(Err(SetupSendError::Send));
        };
        // SAFETY: node_ref pins the manager/connection while the frame is encoded
        // and copied into the transport.
        Some(unsafe {
            send_acknowledged_setup(
                node_ref.conn_mgr,
                conn_id,
                publication_token,
                crate::envelope::CTRL_LINK_REQ,
                RemoteSetupKind::Link,
                |setup_id| {
                    crate::envelope::encode_link_req_payload(&crate::envelope::LinkReqPayload {
                        linker,
                        ref_id,
                        target,
                        policy_tag,
                        // Original request: the receiver reciprocates with a reverse request so
                        // the link is bidirectional (the linker's death also crashes the target).
                        reciprocate: 1,
                        setup_id,
                    })
                },
            )
        })
    })
}

/// Complete both directions of a link whose target this node owns, as this
/// node's connection reader completes a peer's original and reverse requests.
fn link_local_target(
    rt: &crate::runtime::RuntimeInner,
    payload: &crate::envelope::LinkReqPayload,
    target_actor_id: u64,
    linker_actor_id: u64,
) -> u8 {
    let Some(reverse_ref) = rt.monitors.next_observation_id() else {
        set_last_error("hew_node_link_remote_location: observation id space exhausted");
        return crate::envelope::SETUP_STATUS_RESOURCE_EXHAUSTED;
    };
    rt.monitors.register_link_watcher_with_id(
        reverse_ref,
        payload.linker,
        target_actor_id,
        payload.policy_tag,
    );
    if rt
        .monitors
        .register_remote_link_watcher(target_actor_id, payload.linker, payload.ref_id)
        == crate::monitor::RemoteWatcherSetup::TargetGone
    {
        rt.monitors.remove_remote_observation(reverse_ref);
        return crate::envelope::SETUP_STATUS_TARGET_GONE;
    }
    if rt
        .monitors
        .register_remote_link_watcher(linker_actor_id, payload.target, reverse_ref)
        == crate::monitor::RemoteWatcherSetup::TargetGone
    {
        rollback_inbound_link_setup(payload, reverse_ref);
        return crate::envelope::SETUP_STATUS_TARGET_GONE;
    }
    crate::envelope::SETUP_STATUS_ACCEPTED
}

/// `link_remote(RemotePid<T>, PartitionPolicy)` → establish a cross-node link
/// and return its `ref_id`.
///
/// The calling actor (resolved via `hew_actor_self`) links the remote actor
/// `target_pid` so the remote's death fires the per-link `PartitionPolicy`
/// (`policy_tag`; `CrashLinked` == 3 crashes the LOCAL linked actor). Records a
/// watcher-side LINK entry keyed by a fresh `ref_id` (carrying the local actor
/// id + policy) and sends a `CTRL_LINK_REQ` to the owning node so it (a) fans a
/// `CTRL_LINK_DOWN` back when the target dies AND (b) registers the reverse link
/// so the LOCAL actor's death crashes the remote peer too (bidirectional OTP).
///
/// Returns a positive internal `ref_id` on success. Setup failures are returned
/// as negative `LinkError` codes encoded as `-(variant + 1)`, so
/// `link_remote` cannot report `Ok(())` for a link that was never established.
///
/// # Safety
///
/// `target` must point to a readable `HewRemotePid`.
#[no_mangle]
#[allow(
    clippy::too_many_lines,
    reason = "function coordinates the full session-aware connection lifecycle"
)]
pub unsafe extern "C" fn hew_node_link_remote_location(
    target: *const HewRemotePid,
    policy_tag: i64,
) -> i32 {
    if target.is_null() {
        return LINK_ERR_INVALID_TARGET;
    }
    // SAFETY: caller guarantees `target` is readable.
    let Ok(target) = Location::try_from(unsafe { *target }) else {
        return LINK_ERR_STALE_REF;
    };
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("hew_node_link_remote_location: no runtime installed");
        return LINK_ERR_NODE_NOT_RUNNING;
    };
    // The linking subject is the calling actor; a cross-node link with no local
    // actor has nothing to crash, so fail closed.
    let self_actor = crate::actor::hew_actor_self();
    if self_actor.is_null() {
        set_last_error("hew_node_link_remote_location: no current actor (link subject)");
        return LINK_ERR_NO_CURRENT_ACTOR;
    }
    match current_node_accepts_observations() {
        Some(true) => {}
        Some(false) => {
            set_last_error("hew_node_link_remote_location: node is shutting down");
            return LINK_ERR_LOCAL_SHUTDOWN;
        }
        None => {
            set_last_error("hew_node_link_remote_location: no active node");
            return LINK_ERR_NODE_NOT_RUNNING;
        }
    }
    // SAFETY: hew_actor_self returned a non-null live actor pointer.
    // The actor `id` is already a packed pid (node<<48 | serial); the cascade
    // looks it up verbatim via get_actor_ptr_by_id, while the wire carries only
    // the serial part so the peer can address the linker on its own node.
    let local_actor_id = unsafe { (*self_actor).id };
    let Some(target_route) = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: current-node read lock pins the node.
        Some(unsafe { routing::hew_routing_lookup_location((*node).routing_table, target) })
    }) else {
        set_last_error("hew_node_link_remote_location: no active node");
        return LINK_ERR_NODE_NOT_RUNNING;
    };
    if matches!(target_route, routing::LocationRoute::StaleRef) {
        set_last_error("hew_node_link_remote_location: target Location is stale");
        return LINK_ERR_STALE_REF;
    }
    if matches!(target_route, routing::LocationRoute::Partition) {
        set_last_error("hew_node_link_remote_location: target identity is partitioned");
        return LINK_ERR_PARTITION;
    }
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_sign_loss,
        reason = "policy_tag is a small PartitionPolicy discriminant (0..4); the Hew side passes i64"
    )]
    let policy_tag = policy_tag as u8;

    // Record the watcher-side LINK entry first so a connection-drop / SWIM-DEAD
    // fan-out can deliver even if the request send races a drop.
    let Some(ref_id) = rt
        .monitors
        .register_link_watcher(target, local_actor_id, policy_tag)
    else {
        set_last_error("hew_node_link_remote_location: observation id space exhausted");
        return LINK_ERR_RESOURCE_EXHAUSTED;
    };
    if current_node_accepts_observations() != Some(true) {
        rt.monitors.remove_remote_observation(ref_id);
        set_last_error("hew_node_link_remote_location: node shut down during link setup");
        return LINK_ERR_LOCAL_SHUTDOWN;
    }

    let Some(linker) = with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return None;
        }
        // SAFETY: the current-node read lock pins the node.
        let node = unsafe { &*node };
        local_actor_location(node, local_actor_id)
    }) else {
        set_last_error(
            "hew_node_link_remote_location: exact linker/target Location is unavailable",
        );
        rt.monitors.remove_remote_observation(ref_id);
        return LINK_ERR_STALE_REF;
    };

    let setup = if let routing::LocationRoute::Local { actor_id } = target_route {
        Some(Ok(link_local_target(
            rt,
            &crate::envelope::LinkReqPayload {
                linker,
                ref_id,
                target,
                policy_tag,
                reciprocate: 1,
                setup_id: 0,
            },
            actor_id,
            local_actor_id,
        )))
    } else {
        link_setup_request(linker, ref_id, target, policy_tag).map(|sent| {
            sent.map(|(setup_id, pending)| {
                wait_for_setup_result(setup_id, &pending, ref_id, target)
            })
        })
    };
    match setup {
        Some(Ok(status)) => {
            match status {
                crate::envelope::SETUP_STATUS_ACCEPTED => {}
                crate::envelope::SETUP_STATUS_RESOURCE_EXHAUSTED => {
                    send_outbound_unlink(ref_id, target, linker, policy_tag);
                    rt.monitors.remove_remote_observation(ref_id);
                    return LINK_ERR_RESOURCE_EXHAUSTED;
                }
                _ => {
                    send_outbound_unlink(ref_id, target, linker, policy_tag);
                    if let Some(down) = rt.monitors.deliver_link_down_to_ref(
                        ref_id,
                        target,
                        crate::monitor::MONITOR_REASON_LOST,
                    ) {
                        let _ = crate::link::deliver_cross_node_link_exit(
                            down.local_actor_id,
                            down.remote_target_serial,
                            down.reason,
                            down.policy_tag,
                        );
                    }
                }
            }
            0
        }
        Some(Err(SetupSendError::StaleRef)) => {
            rt.monitors.remove_remote_observation(ref_id);
            LINK_ERR_STALE_REF
        }
        Some(Err(SetupSendError::Encode)) => {
            rt.monitors.remove_remote_observation(ref_id);
            LINK_ERR_ENCODE_FAILURE
        }
        Some(Err(SetupSendError::Send)) => {
            rt.monitors.remove_remote_observation(ref_id);
            if current_node_accepts_observations() == Some(true) {
                LINK_ERR_PARTITION
            } else {
                LINK_ERR_LOCAL_SHUTDOWN
            }
        }
        None => {
            rt.monitors.remove_remote_observation(ref_id);
            LINK_ERR_LOCAL_SHUTDOWN
        }
    }
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

fn send_outbound_unlink(ref_id: u64, target: Location, linker: Location, policy_tag: u8) {
    let unlink = crate::envelope::LinkReqPayload {
        linker,
        ref_id,
        target,
        policy_tag,
        reciprocate: 1,
        setup_id: 0,
    };
    with_current_node_read(|guard| {
        let node = *guard as *const HewNode;
        if node.is_null() {
            return;
        }
        // SAFETY: the current-node read lock pins the node for this call.
        send_link_unlink_frame(unsafe { &*node }, target, &unlink);
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
                crash_kind: crash_kind.cast_signed(),
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
            // SAFETY: node_ref is valid for this call.
            let _ = unsafe { send_observation_notice(node_ref, watcher.watcher, notice) };
        }
    });
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
            match rt.monitors.register_remote_link_watcher(
                target_actor_id,
                payload.linker,
                payload.ref_id,
            ) {
                crate::monitor::RemoteWatcherSetup::Registered => {
                    crate::envelope::SETUP_STATUS_ACCEPTED
                }
                crate::monitor::RemoteWatcherSetup::TargetGone => {
                    crate::envelope::SETUP_STATUS_TARGET_GONE
                }
            },
        );
    }

    handle_inbound_original_link_req(rt, payload, target_actor_id, conn_mgr, conn_id, claim_token)
}

fn handle_inbound_original_link_req(
    rt: &crate::runtime::RuntimeInner,
    payload: &crate::envelope::LinkReqPayload,
    target_actor_id: u64,
    conn_mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
) -> Option<u8> {
    // Reserve every fallible setup resource before publishing the target-side
    // watcher. A ResourceExhausted result must never race a LINK_DOWN from a
    // link that the caller was told did not exist.
    // SAFETY: the connection reader pins the manager for this handler.
    let worker_guard = match unsafe { acquire_reverse_link_setup_worker(conn_mgr) } {
        Ok(guard) => guard,
        Err(status) => return Some(status),
    };
    let job_tx = match spawn_reverse_link_setup_worker(worker_guard, conn_mgr, conn_id, claim_token)
    {
        Ok(sender) => sender,
        Err(status) => return Some(status),
    };

    // Reserve the reverse ref without publishing an observation yet, so payload
    // encoding can still fail with ResourceExhausted without activating a link.
    let Some(reverse_ref) = rt.monitors.next_observation_id() else {
        set_last_error("handle_inbound_link_req: observation id space exhausted");
        return Some(crate::envelope::SETUP_STATUS_RESOURCE_EXHAUSTED);
    };

    let reverse_setup = prepare_acknowledged_setup(
        conn_mgr,
        conn_id,
        claim_token,
        crate::envelope::CTRL_LINK_REQ,
        RemoteSetupKind::Link,
        |setup_id| {
            crate::envelope::encode_link_req_payload(&crate::envelope::LinkReqPayload {
                linker: payload.target,
                ref_id: reverse_ref,
                // From the linker's perspective the roles swap: its linker actor is now
                // our "target" to watch, and our target is now the reverse "linker".
                target: payload.linker,
                policy_tag: payload.policy_tag,
                reciprocate: 0,
                setup_id,
            })
        },
    );
    let prepared = match reverse_setup {
        Ok(prepared) => prepared,
        Err(SetupSendError::Encode) => {
            rt.monitors.remove_remote_observation(reverse_ref);
            return Some(crate::envelope::SETUP_STATUS_RESOURCE_EXHAUSTED);
        }
        Err(SetupSendError::StaleRef | SetupSendError::Send) => {
            rt.monitors.remove_remote_observation(reverse_ref);
            return Some(crate::envelope::SETUP_STATUS_TARGET_GONE);
        }
    };

    // Step 2: all fallible reservations are complete, so publish the reverse
    // observation before sending. Connection-loss fan-out can now safely claim it.
    rt.monitors.register_link_watcher_with_id(
        reverse_ref,
        payload.linker,
        target_actor_id,
        payload.policy_tag,
    );

    // Step 1: publish the target watcher only after all setup resources have
    // been reserved. The terminal sweep shares the shard lock with registration.
    if rt
        .monitors
        .register_remote_link_watcher(target_actor_id, payload.linker, payload.ref_id)
        == crate::monitor::RemoteWatcherSetup::TargetGone
    {
        reply_table().remove(prepared.setup_id);
        rt.monitors.remove_remote_observation(reverse_ref);
        return Some(crate::envelope::SETUP_STATUS_TARGET_GONE);
    }

    // Step 3 (original only): send the already-encoded reverse CTRL_LINK_REQ so
    // the linker node registers the target-side watcher for Direction 2.
    // SAFETY: the connection reader pins this manager/connection for the handler.
    if unsafe { send_prepared_acknowledged_setup(conn_mgr, conn_id, claim_token, &prepared) }
        .is_err()
    {
        rollback_inbound_link_setup(payload, reverse_ref);
        return Some(crate::envelope::SETUP_STATUS_TARGET_GONE);
    }

    let setup_id = prepared.setup_id;
    let pending = prepared.pending;
    if job_tx
        .send((setup_id, pending, payload.clone(), reverse_ref))
        .is_err()
    {
        reply_table().remove(setup_id);
        rollback_inbound_link_setup(payload, reverse_ref);
        send_reverse_unlink(
            payload,
            reverse_ref,
            SendConnMgr(conn_mgr),
            conn_id,
            claim_token,
        );
        return Some(crate::envelope::SETUP_STATUS_TARGET_GONE);
    }
    None
}

type ReverseLinkSetupJob = (u64, Arc<PendingReply>, crate::envelope::LinkReqPayload, u64);

fn spawn_reverse_link_setup_worker(
    worker_guard: ReverseLinkSetupGuard,
    conn_mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
) -> Result<std::sync::mpsc::SyncSender<ReverseLinkSetupJob>, u8> {
    let (job_tx, job_rx) = std::sync::mpsc::sync_channel::<ReverseLinkSetupJob>(1);
    let deferred_conn_mgr = SendConnMgr(conn_mgr);
    let worker = thread::Builder::new()
        .name("hew-reverse-link-setup".to_string())
        .spawn(move || {
            let _worker_guard = worker_guard;
            let Ok((setup_id, pending, deferred_payload, reverse_ref)) = job_rx.recv() else {
                return;
            };
            let status = wait_for_setup_result_with_timeout(
                setup_id,
                &pending,
                reverse_ref,
                deferred_payload.linker,
                REVERSE_LINK_SETUP_TIMEOUT,
            );
            complete_inbound_link_setup(
                &deferred_payload,
                reverse_ref,
                status,
                deferred_conn_mgr,
                conn_id,
                claim_token,
            );
        })
        .map_err(|_| crate::envelope::SETUP_STATUS_RESOURCE_EXHAUSTED)?;
    // SAFETY: the connection reader pins the manager for this handler.
    unsafe { connection::hew_connmgr_track_reverse_link_worker(conn_mgr, worker) };
    Ok(job_tx)
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
    ) {
        let _ = crate::link::deliver_cross_node_link_exit(
            down.local_actor_id,
            down.remote_target_serial,
            down.reason,
            down.policy_tag,
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

fn send_reverse_unlink(
    payload: &crate::envelope::LinkReqPayload,
    reverse_ref: u64,
    conn_mgr: SendConnMgr,
    conn_id: c_int,
    claim_token: u64,
) {
    let unlink = crate::envelope::LinkReqPayload {
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
    // SAFETY: the worker's per-manager guard keeps the manager alive; the
    // connection send fails closed if this exact connection has retired.
    let _ = unsafe {
        send_control_frame_on_claimed_connection(
            conn_mgr.0,
            conn_id,
            claim_token,
            crate::envelope::CTRL_UNLINK,
            bytes,
        )
    };
}

fn send_link_unlink_frame(
    node: &HewNode,
    recipient: Location,
    unlink: &crate::envelope::LinkReqPayload,
) {
    // SAFETY: the current-node read lock held by the caller pins `node`.
    let _ = unsafe {
        send_observation_notice(node, recipient, ObservationNotice::Unlink(unlink.clone()))
    };
}

fn complete_inbound_link_setup(
    payload: &crate::envelope::LinkReqPayload,
    reverse_ref: u64,
    status: u8,
    conn_mgr: SendConnMgr,
    conn_id: c_int,
    claim_token: u64,
) {
    match status {
        crate::envelope::SETUP_STATUS_ACCEPTED => {}
        crate::envelope::SETUP_STATUS_RESOURCE_EXHAUSTED => {
            discard_inbound_link_setup(payload, reverse_ref);
            send_reverse_unlink(payload, reverse_ref, conn_mgr, conn_id, claim_token);
        }
        _ => {
            rollback_inbound_link_setup(payload, reverse_ref);
            send_reverse_unlink(payload, reverse_ref, conn_mgr, conn_id, claim_token);
        }
    }

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
                    send_reverse_unlink(payload, reverse_ref, conn_mgr, conn_id, claim_token);
                }
                set_last_error(format!("deferred link setup result encode failure: {err}"));
                return;
            }
        };
    // SAFETY: the worker's per-manager guard keeps the manager alive; the
    // connection send fails closed if this exact connection has retired.
    if unsafe {
        send_control_frame_on_claimed_connection(
            conn_mgr.0,
            conn_id,
            claim_token,
            crate::envelope::CTRL_LINK_SETUP_RESULT,
            result,
        )
    } != 0
        && status == crate::envelope::SETUP_STATUS_ACCEPTED
    {
        rollback_inbound_link_setup(payload, reverse_ref);
        send_reverse_unlink(payload, reverse_ref, conn_mgr, conn_id, claim_token);
    }
}

/// Handle an inbound `CTRL_UNLINK`: a remote node retracted a prior
/// link of one of our local actors. Remove the target-side remote LINK watcher.
/// Idempotent / fail-closed on a missing entry.
pub(crate) fn handle_inbound_unlink(payload: &crate::envelope::LinkReqPayload) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    rt.monitors
        .remove_remote_watcher(payload.target.slot(), payload.linker, payload.ref_id);
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
        rt.monitors.enqueue_down(
            down.watcher_actor_id,
            down.monitor_id,
            down.target,
            payload.reason,
            payload.crash_kind.cast_unsigned(),
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
pub(crate) fn handle_inbound_link_down(ref_id: u64, target: Location, reason: i32) {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("handle_inbound_link_down: no runtime installed");
        return;
    };
    if let Some(down) = rt.monitors.deliver_link_down_to_ref(ref_id, target, reason) {
        let _ = crate::link::deliver_cross_node_link_exit(
            down.local_actor_id,
            down.remote_target_serial,
            down.reason,
            down.policy_tag,
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
    let link_downs = rt
        .monitors
        .take_link_downs_for_node(dead_node, crate::monitor::MONITOR_REASON_LOST);
    for down in link_downs {
        let _ = crate::link::deliver_cross_node_link_exit(
            down.local_actor_id,
            down.remote_target_serial,
            down.reason,
            down.policy_tag,
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
    let link_downs = rt.monitors.take_link_downs_for_session(
        dead_node,
        session_incarnation,
        crate::monitor::MONITOR_REASON_LOST,
    );
    for down in link_downs {
        let _ = crate::link::deliver_cross_node_link_exit(
            down.local_actor_id,
            down.remote_target_serial,
            down.reason,
            down.policy_tag,
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
