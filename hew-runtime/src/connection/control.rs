//! Inbound control frames: registry gossip, monitor and link control.

use std::ffi::c_int;
use std::sync::atomic::Ordering;

// Used by both plain and encryption-enabled envelope send paths.
use crate::envelope::encode_envelope_frame_from_raw_parts;
use crate::envelope::{
    decode_link_down_payload, decode_link_req_payload, decode_monitor_down_payload,
    decode_monitor_req_payload, decode_registry_gossip_payload, decode_setup_result_payload,
    encode_control_frame, encode_registry_gossip_payload, encode_setup_result_payload,
    ControlFrame, RegistryGossipPayload, SetupResultPayload, CTRL_DEMONITOR, CTRL_LINK_DOWN,
    CTRL_LINK_REQ, CTRL_LINK_SETUP_RESULT, CTRL_MONITOR_DOWN, CTRL_MONITOR_REQ,
    CTRL_MONITOR_SETUP_RESULT, CTRL_REGISTRY_GOSSIP, CTRL_SWIM, CTRL_UNLINK,
    REGISTRY_GOSSIP_OP_ADD, REGISTRY_GOSSIP_OP_REMOVE, SETUP_STATUS_ACCEPTED,
    SETUP_STATUS_TARGET_GONE, WIRE_VERSION,
};
use crate::mailbox_envelope::validate_cross_node_send_params;
use crate::node_identity::Location;
use crate::set_last_error;

use super::handshake::supports_gossip;
use super::peer::wait_authenticated_peer_node_id_for_conn;
use super::send::hew_connmgr_send_preencoded_claimed;
use super::swim::handle_swim_control_frame;
use super::{HewConnMgr, CONN_STATE_ACTIVE};

// Used by both plain and encryption-enabled send paths in `hew_connmgr_send`.
pub(super) unsafe fn encode_envelope(
    target: Location,
    msg_type: i32,
    payload: *mut u8,
    payload_len: usize,
    payload_class: u8,
    cancel_token_handle: u64,
) -> Option<Vec<u8>> {
    // Cross-node send gates (Gate 1 before Gate 2; fail-closed in all build
    // profiles via set_last_error + return None, not debug_assert).
    validate_cross_node_send_params(payload_class, cancel_token_handle)?;
    // SAFETY: caller guarantees `payload` is valid for `payload_len` bytes.
    match unsafe {
        encode_envelope_frame_from_raw_parts(
            Some(target),
            None,
            msg_type,
            payload.cast_const(),
            payload_len,
            0,
        )
    } {
        Ok(bytes) => Some(bytes),
        Err(err) => {
            set_last_error(format!("hew_connmgr_send: {err}"));
            None
        }
    }
}

pub(super) fn encode_registry_gossip_control(
    name: &str,
    location: Location,
    is_add: bool,
) -> Option<Vec<u8>> {
    let op = if is_add {
        crate::cluster::GOSSIP_REGISTRY_ADD
    } else {
        crate::cluster::GOSSIP_REGISTRY_REMOVE
    };
    let payload = RegistryGossipPayload {
        op,
        name: name.to_owned(),
        location,
    };
    let payload = match encode_registry_gossip_payload(&payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "registry gossip control payload encode failure: {err}"
            ));
            return None;
        }
    };
    let frame = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: CTRL_REGISTRY_GOSSIP,
        payload,
    };
    match encode_control_frame(&frame) {
        Ok(bytes) => Some(bytes),
        Err(err) => {
            set_last_error(format!(
                "registry gossip control frame encode failure: {err}"
            ));
            None
        }
    }
}

pub(super) fn handle_control_frame(
    mgr: *mut HewConnMgr,
    peer_feature_flags: u32,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    match control.ctrl_kind {
        CTRL_REGISTRY_GOSSIP => {}
        CTRL_SWIM => {
            handle_swim_control_frame(mgr, peer_feature_flags, conn_id, claim_token, control);
            return;
        }
        CTRL_MONITOR_REQ => {
            handle_monitor_req_frame(mgr, conn_id, claim_token, control);
            return;
        }
        CTRL_DEMONITOR => {
            handle_demonitor_frame(mgr, conn_id, claim_token, control);
            return;
        }
        CTRL_MONITOR_DOWN => {
            handle_monitor_down_frame(mgr, conn_id, claim_token, control);
            return;
        }
        CTRL_MONITOR_SETUP_RESULT => {
            handle_setup_result_frame(
                mgr,
                conn_id,
                claim_token,
                control,
                crate::hew_node::RemoteSetupKind::Monitor,
            );
            return;
        }
        CTRL_LINK_REQ => {
            handle_link_req_frame(mgr, conn_id, claim_token, control);
            return;
        }
        CTRL_UNLINK => {
            handle_unlink_frame(mgr, conn_id, claim_token, control);
            return;
        }
        CTRL_LINK_DOWN => {
            handle_link_down_frame(mgr, conn_id, claim_token, control);
            return;
        }
        CTRL_LINK_SETUP_RESULT => {
            handle_setup_result_frame(
                mgr,
                conn_id,
                claim_token,
                control,
                crate::hew_node::RemoteSetupKind::Link,
            );
            return;
        }
        other => {
            set_last_error(format!(
                "connection reader unknown control frame kind {other}"
            ));
            return;
        }
    }
    if !supports_gossip(peer_feature_flags) {
        set_last_error("connection reader rejected registry gossip from non-gossip peer");
        return;
    }
    // D9/D12: only an authenticated (`Strict`) peer may inject registry gossip.
    // An `Unverified` (delivery-only) connection carries no control-plane
    // authority — drop its gossip with a diagnostic and apply no registry
    // mutation (fail-closed).
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "registry gossip")
    else {
        return;
    };

    let payload = match decode_registry_gossip_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader registry gossip payload decode failure: {err}"
            ));
            return;
        }
    };
    let is_add = match payload.op {
        REGISTRY_GOSSIP_OP_ADD => true,
        REGISTRY_GOSSIP_OP_REMOVE => false,
        op => {
            set_last_error(format!("connection reader registry gossip unknown op {op}"));
            return;
        }
    };
    if mgr.is_null() {
        set_last_error("connection reader registry gossip missing manager");
        return;
    }
    // SAFETY: reader_loop owns a live manager pointer for this connection.
    let mgr_ref = unsafe { &*mgr };
    if mgr_ref.cluster.is_null() {
        set_last_error("connection reader registry gossip missing cluster");
        return;
    }
    if !location_matches_node_session(payload.location, peer_identity) {
        set_last_error("registry gossip rejected a Location not owned by the authenticated peer");
        return;
    }
    // SAFETY: cluster pointer is owned by the live node/manager and remains
    // valid while the reader thread is running.
    unsafe {
        (&*mgr_ref.cluster).apply_registry_event(&payload.name, payload.location, is_add);
    }
}

fn authenticated_peer_node_id(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    context: &str,
) -> Option<u16> {
    if mgr.is_null() {
        set_last_error(format!("connection reader {context}: missing manager"));
        return None;
    }
    // SAFETY: reader_loop owns a live manager pointer for this connection.
    let mgr_ref = unsafe { &*mgr };
    // Waiting variant: an inbound control frame can race this connection's own
    // Reserved → Published publication (the reader thread starts before
    // `publish_connection_established`); one-shot frames like the peer's
    // registry-gossip flush must not be dropped inside that window.
    let authenticated = wait_authenticated_peer_node_id_for_conn(mgr_ref, conn_id, claim_token);
    if authenticated == 0 {
        set_last_error(format!(
            "connection reader {context}: missing authenticated peer for conn {conn_id} \
             (unverified/delivery-only connections carry no control-plane authority)"
        ));
        return None;
    }
    Some(authenticated)
}

pub(super) fn authenticated_peer_identity(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    context: &str,
) -> Option<(u16, crate::envelope::NodeSessionIdentity)> {
    let route_slot = authenticated_peer_node_id(mgr, conn_id, claim_token, context)?;
    // SAFETY: `authenticated_peer_node_id` verified the live manager.
    let mgr = unsafe { &*mgr };
    let identity = mgr.connections.access(|connections| {
        connections
            .iter()
            .find(|connection| {
                connection.conn_id == conn_id
                    && connection.publication_token == claim_token
                    && connection.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
            })
            .and_then(|connection| {
                Some(crate::envelope::NodeSessionIdentity {
                    node_id: connection.peer_identity?,
                    session_incarnation: connection.peer_session_incarnation,
                })
            })
    });
    let Some(identity) = identity else {
        set_last_error(format!(
            "connection reader {context}: authenticated connection is missing its NodeId/session"
        ));
        return None;
    };
    Some((route_slot, identity))
}

pub(super) fn location_matches_node_session(
    location: Location,
    identity: crate::envelope::NodeSessionIdentity,
) -> bool {
    location.node() == identity.node_id
        && location.incarnation() == identity.session_incarnation
        && crate::pid::actor_slot_fits_internal_alias(location.slot())
}

pub(super) fn location_matches_local_session(mgr: &HewConnMgr, location: Location) -> bool {
    Some(location.node()) == mgr.local_identity
        && Some(location.incarnation()) == mgr.local_session_incarnation
        && crate::pid::actor_slot_fits_internal_alias(location.slot())
}

fn send_setup_result_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    ctrl_kind: u64,
    result: &SetupResultPayload,
) {
    let payload = match encode_setup_result_payload(result) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader setup result payload encode failure: {err}"
            ));
            return;
        }
    };
    let frame = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind,
        payload,
    };
    let bytes = match encode_control_frame(&frame) {
        Ok(bytes) => bytes,
        Err(err) => {
            set_last_error(format!(
                "connection reader setup result frame encode failure: {err}"
            ));
            return;
        }
    };
    // SAFETY: the reader owns this exact manager/connection publication claim;
    // the send copies the encoded frame before returning.
    let _ = unsafe {
        hew_connmgr_send_preencoded_claimed(mgr, conn_id, claim_token, bytes.as_ptr(), bytes.len())
    };
}

/// Handle an inbound `CTRL_MONITOR_REQ`: a remote node is monitoring
/// one of our local actors. Record a target-side remote-watcher entry so the
/// terminal sweep can fan out a `CTRL_MONITOR_DOWN` when that actor dies.
///
/// Fail-closed: a malformed / oversized payload is dropped with `set_last_error`
/// and never registers a watcher — no fabricated state from untrusted bytes.
fn handle_monitor_req_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    let payload = match decode_monitor_req_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader monitor req payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "monitor req")
    else {
        return;
    };
    // SAFETY: the authenticated identity helper verified the manager.
    let mgr_ref = unsafe { &*mgr };
    if payload.setup_id == 0
        || !location_matches_node_session(payload.watcher, peer_identity)
        || !location_matches_local_session(mgr_ref, payload.target)
    {
        set_last_error("connection reader monitor req Location mismatch");
        return;
    }
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("connection reader monitor req: no runtime installed");
        return;
    };
    let target_actor_id = crate::pid::hew_pid_make(mgr_ref.local_node_id, payload.target.slot());
    crate::hew_node::remote_setup_race_probe_wait(
        crate::hew_node::RemoteSetupKind::Monitor,
        target_actor_id,
    );
    let status =
        match rt
            .monitors
            .register_remote_watcher(target_actor_id, payload.watcher, payload.ref_id)
        {
            crate::monitor::RemoteWatcherSetup::Registered => SETUP_STATUS_ACCEPTED,
            crate::monitor::RemoteWatcherSetup::TargetGone => SETUP_STATUS_TARGET_GONE,
        };
    send_setup_result_frame(
        mgr,
        conn_id,
        claim_token,
        CTRL_MONITOR_SETUP_RESULT,
        &SetupResultPayload {
            setup_id: payload.setup_id,
            ref_id: payload.ref_id,
            target: payload.target,
            status,
        },
    );
}

/// Handle an inbound `CTRL_DEMONITOR`: a remote node retracted its
/// monitor of one of our local actors. Remove the target-side remote-watcher
/// entry. Idempotent / fail-closed on malformed input.
fn handle_demonitor_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    let payload = match decode_monitor_req_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader demonitor payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "demonitor")
    else {
        return;
    };
    // SAFETY: the authenticated identity helper verified the manager.
    let mgr_ref = unsafe { &*mgr };
    if !location_matches_node_session(payload.watcher, peer_identity)
        || !location_matches_local_session(mgr_ref, payload.target)
    {
        set_last_error("connection reader demonitor Location mismatch");
        return;
    }
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    rt.monitors
        .remove_remote_watcher(payload.target.slot(), payload.watcher, payload.ref_id);
}

fn handle_setup_result_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
    kind: crate::hew_node::RemoteSetupKind,
) {
    let payload = match decode_setup_result_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader setup result payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "setup result")
    else {
        return;
    };
    if payload.setup_id == 0
        || payload.ref_id == 0
        || !location_matches_node_session(payload.target, peer_identity)
    {
        set_last_error("connection reader setup result Location mismatch");
        return;
    }
    if !crate::hew_node::complete_remote_setup_reply(
        mgr,
        conn_id,
        claim_token,
        payload.setup_id,
        kind,
        &control.payload,
    ) {
        set_last_error("connection reader setup result did not match a pending setup");
    }
}

/// Handle an inbound `CTRL_MONITOR_DOWN`: the node owning an actor we
/// monitor reports that actor reached a terminal state. Atomically claim the
/// observation and enqueue DOWN in the local watcher's system mailbox.
///
/// Fail-closed on malformed input. Removing the observation before enqueue is
/// the exactly-once disambiguation: a definitive DOWN beats a later partition
/// signal for the same registration.
fn handle_monitor_down_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    let payload = match decode_monitor_down_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader monitor down payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "monitor down")
    else {
        return;
    };
    if !location_matches_node_session(payload.target, peer_identity) {
        set_last_error("connection reader monitor down target Location mismatch");
        return;
    }
    let crashed = payload.reason == crate::internal::types::HewActorState::Crashed as i32;
    if !crashed && payload.reason != crate::internal::types::HewActorState::Stopped as i32 {
        set_last_error("connection reader monitor down invalid terminal reason");
        return;
    }
    if !(0..=2).contains(&payload.crash_kind) || (!crashed && payload.crash_kind != 0) {
        set_last_error("connection reader monitor down invalid crash kind");
        return;
    }
    crate::hew_node::handle_inbound_monitor_down(&payload);
}

/// Handle an inbound `CTRL_LINK_REQ`: a remote node is linking one of our local
/// actors. Establish the bidirectional cross-node link.
///
/// Fail-closed: a malformed / oversized payload is dropped with `set_last_error`
/// and never registers a link — no fabricated state from untrusted bytes. The
/// decode bar is HIGHER than monitor because a registered link can later crash a
/// real actor.
fn handle_link_req_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    let payload = match decode_link_req_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader link req payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "link req")
    else {
        return;
    };
    // SAFETY: the authenticated identity helper verified the manager.
    let mgr_ref = unsafe { &*mgr };
    if payload.setup_id == 0
        || !location_matches_node_session(payload.linker, peer_identity)
        || !location_matches_local_session(mgr_ref, payload.target)
    {
        set_last_error("connection reader link req Location mismatch");
        return;
    }
    let target_actor_id = crate::pid::hew_pid_make(mgr_ref.local_node_id, payload.target.slot());
    if payload.reciprocate == 1 {
        crate::hew_node::remote_setup_race_probe_wait(
            crate::hew_node::RemoteSetupKind::Link,
            target_actor_id,
        );
    }
    if let Some(status) = crate::hew_node::handle_inbound_link_req(
        &payload,
        target_actor_id,
        mgr,
        conn_id,
        claim_token,
    ) {
        send_setup_result_frame(
            mgr,
            conn_id,
            claim_token,
            CTRL_LINK_SETUP_RESULT,
            &SetupResultPayload {
                setup_id: payload.setup_id,
                ref_id: payload.ref_id,
                target: payload.target,
                status,
            },
        );
    }
}

/// Handle an inbound `CTRL_UNLINK`: a remote node retracted a prior
/// link of one of our local actors. Idempotent / fail-closed on malformed input.
fn handle_unlink_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    let payload = match decode_link_req_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader unlink payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some((_, peer_identity)) = authenticated_peer_identity(mgr, conn_id, claim_token, "unlink")
    else {
        return;
    };
    // SAFETY: the authenticated identity helper verified the manager.
    let mgr_ref = unsafe { &*mgr };
    if !location_matches_node_session(payload.linker, peer_identity)
        || !location_matches_local_session(mgr_ref, payload.target)
    {
        set_last_error("connection reader unlink Location mismatch");
        return;
    }
    crate::hew_node::handle_inbound_unlink(&payload);
}

/// Handle an inbound `CTRL_LINK_DOWN`: the node owning an actor we LINK
/// reports it reached a terminal state. Fire the cross-node link cascade —
/// synthesize a `HewSysMsg::Exit` into the LOCAL linked actor's mailbox and crash it
/// (for `CrashLinked`). A monitor DOWN queues a typed mailbox notification,
/// while a link DOWN queues EXIT and applies the link policy. Fail-closed on
/// malformed input; the EXIT fires exactly once and ONLY for a link entry
/// this node registered AND ONLY when the handshake-authenticated sender of this
/// connection is the same peer that entry is linked to — otherwise neither a
/// forged `ref_id` this node never linked NOR a different, genuinely-connected
/// peer that merely guessed/learned a pending link `ref_id` can crash an actor
/// linked to another, still-alive peer.
pub(super) fn handle_link_down_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    let payload = match decode_link_down_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader link down payload decode failure: {err}"
            ));
            return;
        }
    };
    // issue #2652: a link DOWN crashes the LOCAL linked actor, so it must be
    // authorised by the exact handshake-authenticated owner of the peer's
    // published claim — NOT the posture-agnostic delivery id. Using
    // `peer_node_id_for_conn` here was a bypass: it returns the *self-declared*
    // node id even for an `Unverified` (delivery-only) or a superseded
    // connection, so such a peer whose declared id happened to match a stored
    // link's `remote_node_id` could crash an actor linked to the genuine,
    // still-alive owner (`deliver_link_down_to_ref`'s `== 0` guard never fires
    // because the delivery id is nonzero). Route through the exact-owner gate so
    // an `Unverified`/superseded connection yields `None` and is dropped with a
    // diagnostic before any cross-node exit cascade.
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "link down")
    else {
        return;
    };
    if !location_matches_node_session(payload.target, peer_identity) {
        set_last_error("connection reader link down target Location mismatch");
        return;
    }
    crate::hew_node::handle_inbound_link_down(payload.ref_id, payload.target, payload.reason);
}
