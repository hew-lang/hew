//! SWIM failure-detection transport.

use std::ffi::c_int;
use std::sync::atomic::Ordering;

use crate::cluster::HewCluster;
use crate::envelope::{
    decode_swim_payload, encode_control_frame, encode_swim_payload, ControlFrame,
    SwimControlPayload, SwimGossipEntry, CTRL_SWIM, WIRE_VERSION,
};
use crate::node_identity::NodeId;
use crate::set_last_error;

use super::control::authenticated_peer_identity;
use super::handshake::supports_gossip;
use super::peer::authenticated_peer_node_id_for_conn;
use super::send::{
    hew_connmgr_conn_id_for_node, hew_connmgr_feature_flags_for_node, send_preencoded_on_manager,
};
use super::{HewConnMgr, CONN_STATE_ACTIVE};

/// Encode a SWIM control frame from a payload.
pub(super) fn encode_swim_control(payload: &SwimControlPayload) -> Option<Vec<u8>> {
    let payload_bytes = match encode_swim_payload(payload) {
        Ok(bytes) => bytes,
        Err(err) => {
            set_last_error(format!("SWIM control payload encode failure: {err}"));
            return None;
        }
    };
    let frame = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: CTRL_SWIM,
        payload: payload_bytes,
    };
    match encode_control_frame(&frame) {
        Ok(bytes) => Some(bytes),
        Err(err) => {
            set_last_error(format!("SWIM control frame encode failure: {err}"));
            None
        }
    }
}

/// Drain the cluster's pending membership gossip as wire entries for
/// piggybacking on an outbound SWIM frame (C6).
fn node_session_for_route(
    mgr: &HewConnMgr,
    cluster: &HewCluster,
    route_slot: u16,
) -> Option<crate::envelope::NodeSessionIdentity> {
    if route_slot == mgr.local_node_id {
        return Some(crate::envelope::NodeSessionIdentity {
            node_id: mgr.local_identity?,
            session_incarnation: mgr.local_session_incarnation?,
        });
    }
    Some(crate::envelope::NodeSessionIdentity {
        node_id: mgr.auth.node_id_for_route_slot(route_slot)?,
        session_incarnation: cluster.member_session(route_slot)?,
    })
}

#[allow(
    clippy::unnecessary_lazy_evaluations,
    reason = "cfg(test) block inside closure may return Some; not lazy-evaluable"
)]
fn route_slot_for_identity(mgr: &HewConnMgr, node_id: NodeId) -> Option<u16> {
    if Some(node_id) == mgr.local_identity {
        Some(mgr.local_node_id)
    } else {
        mgr.auth.route_slot_for_node_id(node_id).or_else(|| {
            #[cfg(test)]
            {
                let bytes = node_id.to_bytes();
                if bytes[..14].iter().all(|byte| *byte == 0) {
                    return Some(u16::from_be_bytes([bytes[14], bytes[15]]));
                }
            }
            None
        })
    }
}

fn collect_swim_gossip(mgr: &HewConnMgr, cluster: &HewCluster) -> Vec<SwimGossipEntry> {
    cluster
        .take_swim_gossip(cluster.max_gossip_per_msg())
        .into_iter()
        .filter_map(|(route_slot, state, incarnation)| {
            Some(SwimGossipEntry {
                member: node_session_for_route(mgr, cluster, route_slot)?,
                state,
                incarnation,
            })
        })
        .collect()
}

/// Build a SWIM frame of `msg_type` to `target_node`, stamped with the local
/// node's identity and incarnation, carrying a fresh piggybacked gossip batch.
fn build_swim_frame(
    mgr: &HewConnMgr,
    cluster: &HewCluster,
    msg_type: i32,
    target_node: u16,
) -> Option<Vec<u8>> {
    let payload = SwimControlPayload {
        msg_type,
        from: node_session_for_route(mgr, cluster, mgr.local_node_id)?,
        incarnation: cluster.local_incarnation(),
        target: if target_node == 0 {
            None
        } else {
            Some(node_session_for_route(mgr, cluster, target_node)?)
        },
        gossip: collect_swim_gossip(mgr, cluster),
    };
    encode_swim_control(&payload)
}

/// Send a single SWIM protocol message to a specific peer node.
///
/// Used by the SWIM driver to issue direct PINGs and by the indirect-probe
/// path (C4) to relay `PING` / `PING_REQ`. `target_node` is the
/// indirect-probe subject for `PING_REQ` frames; `0` for direct PING / ACK.
///
/// Returns `0` on a successful send, `-1` if no active gossip-capable
/// connection to `peer_node_id` exists or the send failed.
///
/// # Safety
///
/// `mgr` must be a valid connection manager pointer for the duration of the
/// call when non-null.
pub(crate) unsafe fn hew_connmgr_send_swim(
    mgr: *mut HewConnMgr,
    peer_node_id: u16,
    msg_type: i32,
    target_node: u16,
) -> c_int {
    if mgr.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees manager pointer validity for this call.
    let mgr_ref = unsafe { &*mgr };
    if mgr_ref.cluster.is_null() {
        return -1;
    }

    // SAFETY: mgr validity is the caller's contract, re-checked non-null above.
    let conn_id = unsafe { hew_connmgr_conn_id_for_node(mgr, peer_node_id) };
    if conn_id < 0 {
        return -1;
    }
    // SAFETY: mgr validity is the caller's contract, re-checked non-null above.
    let flags = unsafe { hew_connmgr_feature_flags_for_node(mgr, peer_node_id) };
    if !supports_gossip(flags) {
        return -1;
    }
    // issue #2652 (D9, outbound): SWIM membership traffic goes only to the exact
    // authenticated owner of a published claim. An `Unverified` (delivery-only)
    // or superseded connection resolves to 0 here and is refused — symmetric
    // with the inbound SWIM cross-attribution gate.
    if authenticated_peer_node_id_for_conn(mgr_ref, conn_id) == 0 {
        return -1;
    }

    // SAFETY: cluster pointer is owned by the live manager.
    let cluster = unsafe { &*mgr_ref.cluster };
    let Some(bytes) = build_swim_frame(mgr_ref, cluster, msg_type, target_node) else {
        return -1;
    };
    // SAFETY: manager is live, bytes is a complete encoded control frame.
    if unsafe { send_preencoded_on_manager(mgr_ref, conn_id, None, bytes.as_ptr(), bytes.len()) }
        == 0
    {
        0
    } else {
        -1
    }
}

/// Return the node IDs of all active gossip-capable peer connections.
///
/// The SWIM driver uses this to choose indirect-probe relays (K random peers
/// excluding the probe target).
///
/// # Safety
///
/// `mgr` must be a valid pointer for the duration of the call.
pub(crate) unsafe fn hew_connmgr_active_swim_peers(mgr: *const HewConnMgr) -> Vec<u16> {
    if mgr.is_null() {
        return Vec::new();
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };
    mgr_ref
        .connections
        .access(|conns| {
            conns
                .iter()
                .filter(|c| {
                    c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                        && supports_gossip(c.peer_feature_flags)
                        && c.peer_node_id != 0
                })
                .map(|c| (c.conn_id, c.peer_node_id))
                .collect::<Vec<(c_int, u16)>>()
        })
        .into_iter()
        // issue #2652 (D9, outbound): SWIM is control-plane traffic — an
        // `Unverified` (delivery-only) or superseded peer must not be probed or
        // relayed to. Keep only exact authenticated claim owners. The snapshot above
        // ensures the claim-lock check never nests inside the `connections` guard.
        .filter(|&(conn_id, _)| authenticated_peer_node_id_for_conn(mgr_ref, conn_id) != 0)
        .map(|(_, node)| node)
        .collect()
}

/// Handle an inbound SWIM control frame.
///
/// 1. Decode + bound the payload (fail-closed).
/// 2. Reject cross-attribution: the claimed `from_node` MUST match the
///    handshake-authenticated identity of the connection it arrived on.
/// 3. Apply piggybacked membership gossip (C6 import) and run incarnation
///    self-refutation (C5).
/// 4. Run the SWIM state machine via `process_message`.
/// 5. Respond per message type: PING -> ACK; `PING_REQ` -> forward a real PING
///    to the indirect-probe target (C4); ACK -> no response.
pub(super) fn handle_swim_control_frame(
    mgr: *mut HewConnMgr,
    peer_feature_flags: u32,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    if !supports_gossip(peer_feature_flags) {
        set_last_error("connection reader rejected SWIM frame from non-gossip peer");
        return;
    }
    let payload = match decode_swim_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader SWIM payload decode failure: {err}"
            ));
            return;
        }
    };
    if mgr.is_null() {
        set_last_error("connection reader SWIM frame missing manager");
        return;
    }
    // SAFETY: reader_loop owns a live manager pointer for this connection.
    let mgr_ref = unsafe { &*mgr };
    if mgr_ref.cluster.is_null() {
        set_last_error("connection reader SWIM frame missing cluster");
        return;
    }

    // Cross-attribution defence + D9/D12 control-plane gate: the frame's
    // claimed sender must equal the *authenticated* (`Strict`) handshake
    // identity of the connection it arrived on. An `Unverified` (delivery-only)
    // connection resolves to 0 here and is rejected — it cannot inject SWIM
    // membership state. Waiting variant: the first inbound SWIM frame can race
    // this connection's own Reserved → Published publication.
    let Some((authenticated, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "SWIM")
    else {
        return;
    };
    if payload.from != peer_identity {
        set_last_error("connection reader SWIM sender does not match authenticated peer");
        return;
    }
    // SAFETY: cluster pointer is owned by the live node/manager.
    let cluster = unsafe { &*mgr_ref.cluster };

    let mut gossip = Vec::with_capacity(payload.gossip.len());
    for entry in &payload.gossip {
        let Some(route_slot) = route_slot_for_identity(mgr_ref, entry.member.node_id) else {
            set_last_error("connection reader SWIM gossip contains unknown NodeId");
            return;
        };
        gossip.push((
            route_slot,
            entry.member.session_incarnation,
            entry.state,
            entry.incarnation,
        ));
    }
    let target_node = match payload.target {
        Some(target) => {
            let Some(route_slot) = route_slot_for_identity(mgr_ref, target.node_id) else {
                set_last_error("connection reader SWIM target contains unknown NodeId");
                return;
            };
            let expected_session = if route_slot == mgr_ref.local_node_id {
                mgr_ref.local_session_incarnation
            } else {
                cluster.member_session(route_slot)
            };
            if expected_session != Some(target.session_incarnation) {
                set_last_error("connection reader SWIM target session mismatch");
                return;
            }
            route_slot
        }
        None => 0,
    };

    // SAFETY: cluster pointer is owned by the live node/manager and remains
    // valid while the reader thread is running. All driven methods
    // (apply_swim_gossip / refute_if_suspected / process_message) take &self
    // and synchronize internally via Mutex/Atomic, so a shared reference is
    // sound even with the SWIM driver thread concurrently calling tick().
    // C6: fold piggybacked membership gossip into our view.
    cluster.apply_swim_gossip(&gossip);
    // C5: if any gossip suspects us, refute by bumping our incarnation. The
    // refutation rides out on our next PING/ACK as an ALIVE-about-self event.
    let _ = cluster.refute_if_suspected(&gossip);

    // Run the SWIM state machine for this message. The authenticated peer ID
    // is supplied as the source-connection identity so the cluster's
    // source-mismatch guard validates the claim.
    cluster.process_message(
        payload.msg_type,
        authenticated,
        payload.incarnation,
        authenticated,
    );

    // Respond per message type.
    match payload.msg_type {
        crate::cluster::SWIM_MSG_PING => {
            // Direct probe: ACK the sender.
            // SAFETY: manager is live for this call.
            let _ = unsafe {
                hew_connmgr_send_swim(mgr, authenticated, crate::cluster::SWIM_MSG_ACK, 0)
            };
        }
        crate::cluster::SWIM_MSG_PING_REQ
            if target_node != 0 && target_node != mgr_ref.local_node_id =>
        {
            // C4 indirect probing: forward a real PING to the probe target so
            // the relay actually exercises the target's liveness, then the
            // target's ACK propagates membership back via gossip. A no-op if
            // we have no active connection to the target.
            // SAFETY: manager is live for this call.
            let _ = unsafe {
                hew_connmgr_send_swim(mgr, target_node, crate::cluster::SWIM_MSG_PING, 0)
            };
        }
        _ => {
            // ACK (and any other type) requires no response; state already
            // updated via process_message above.
        }
    }
}
