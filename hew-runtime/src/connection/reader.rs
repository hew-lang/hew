//! Per-connection reader thread.

use std::ffi::c_int;
use std::sync::atomic::{AtomicI32, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use crate::envelope::{decode_wire_frame, WireFrame};
use crate::set_last_error;
use crate::util::MutexExt;

use super::admission::hew_connmgr_remove;
use super::channel::ChannelProtection;
use super::control::{
    authenticated_peer_identity, handle_control_frame, location_matches_local_session,
    location_matches_node_session,
};
use super::gossip::retry_pending_registry_flush;
use super::identity_claim::DeferredLoser;
use super::peer::deposit_reply_envelope;
use super::reconnect::{reconnect_plan, spawn_reconnect_worker};
use super::{ConnectionTransport, HewConnMgr, InboundRouter, SendConnMgr};

// ── Reader thread ──────────────────────────────────────────────────────

/// Frames a buried peer's refuting connection may carry before it closes. The
/// peer does not know it was buried, so its ordinary traffic is dropped rather
/// than refused, and counts here; its refutation needs a handful of SWIM frames.
const REFUTING_FRAME_CAP: u32 = 1024;

/// Initial read buffer, and the largest frame a refuting connection may carry.
const READ_BUFFER_BYTES: usize = 64 * 1024;

/// Close a buried peer's refuting connection from its own reader, as an
/// expected stop: nothing reconnects to a peer that broke its bounds.
fn close_refuting(mgr: *mut HewConnMgr, conn_id: c_int, claim_token: u64, stop_flag: &AtomicI32) {
    stop_flag.store(1, Ordering::Release);
    // SAFETY: reader_loop owns a live manager pointer for this connection.
    if let Some(mgr) = unsafe { mgr.as_ref() } {
        let _ = super::admission::remove_connection(mgr, conn_id, Some(claim_token));
    }
}

/// Whether this admission's claim is published: a refuting claim becomes so
/// when its peer's refutation readmits it.
fn claim_published(mgr: *mut HewConnMgr, conn_id: c_int, claim_token: u64) -> bool {
    // SAFETY: reader_loop owns a live manager pointer for this connection.
    unsafe { mgr.as_ref() }.is_some_and(|mgr| {
        mgr.claims.0.lock_or_recover().values().any(|claim| {
            claim.conn_id == conn_id
                && claim.publication_token == claim_token
                && claim.state == crate::peer_binding::ClaimState::Published
        })
    })
}

/// Reader thread: loops calling transport recv, decodes envelopes,
/// and routes to local actors via the inbound router callback.
/// Cleanup after reader loop exit: remove from manager and attempt reconnection
/// if the drop was unexpected (not triggered by explicit stop).
pub(super) fn reader_cleanup(mgr: *mut HewConnMgr, conn_id: c_int, stop_flag: &AtomicI32) {
    // Evict any stashed I/O span context (idempotent; no-op if tracing was disabled).
    crate::tracing::io_span_evict(conn_id);

    let unexpected_drop = stop_flag.load(Ordering::Acquire) == 0;
    if unexpected_drop {
        if !mgr.is_null() {
            crate::hew_node::fail_remote_replies_for_connection(mgr.cast_const(), conn_id);
        }
        // SAFETY: `mgr` and `conn_id` originate from a live connection manager.
        let reconnect_plan = unsafe {
            if mgr.is_null() {
                None
            } else {
                reconnect_plan(&*mgr, conn_id)
            }
        };
        // SAFETY: manager and conn_id come from active reader state.
        let _ = unsafe { hew_connmgr_remove(mgr, conn_id) };
        if let Some(plan) = reconnect_plan {
            spawn_reconnect_worker(mgr, conn_id, plan);
        }
    }
}

#[expect(
    clippy::needless_pass_by_value,
    reason = "ConnectionTransport and Arc values are moved into this thread from spawn closure"
)]
#[expect(
    clippy::too_many_arguments,
    reason = "reader_loop captures all per-connection state; splitting into a struct \
              would require unsafe Send impls for the contained raw pointers"
)]
#[allow(
    clippy::too_many_lines,
    reason = "reader_loop spans the full connection read path; refactoring would require unsafe Send impls"
)]
pub(super) fn reader_loop(
    mgr: SendConnMgr,
    transport: ConnectionTransport,
    conn_id: c_int,
    claim_token: u64,
    stop_flag: Arc<AtomicI32>,
    last_activity: Arc<AtomicU64>,
    router: Option<InboundRouter>,
    peer_feature_flags: u32,
    channel: Arc<ChannelProtection>,
    mut confirm: Option<(u64, Arc<Mutex<DeferredLoser>>)>,
    buried: bool,
) {
    let mgr = mgr.0;
    let transport_ptr = transport.as_ptr();
    let mut buf = vec![0u8; READ_BUFFER_BYTES];
    // Frames read while the peer refutes its burial; `None` once published.
    let mut refuting = buried.then_some(0_u32);

    while stop_flag.load(Ordering::Acquire) == 0 {
        // SAFETY: transport is valid for the manager's lifetime; conn_id
        // is valid for this connection's lifetime.
        let bytes_read = unsafe {
            let t = &*transport_ptr;
            if let Some(ops) = t.ops.as_ref() {
                if let Some(recv_fn) = ops.recv {
                    recv_fn(t.r#impl, conn_id, buf.as_mut_ptr().cast(), buf.len())
                } else {
                    -1
                }
            } else {
                -1
            }
        };

        if bytes_read <= 0 {
            reader_cleanup(mgr, conn_id, &stop_flag);
            break;
        }

        #[expect(clippy::cast_sign_loss, reason = "bytes_read > 0 checked above")]
        let read_len = bytes_read as usize;
        if read_len > buf.len() {
            if refuting.is_some() {
                set_last_error(format!(
                    "connection reader: buried peer on conn {conn_id} sent a frame larger \
                     than {READ_BUFFER_BYTES} bytes"
                ));
                close_refuting(mgr, conn_id, claim_token, &stop_flag);
                break;
            }
            if read_len > crate::transport::MAX_FRAME_SIZE {
                set_last_error(
                    "connection reader: requested frame capacity exceeds transport limit",
                );
                reader_cleanup(mgr, conn_id, &stop_flag);
                break;
            }
            buf.resize(read_len, 0);
            continue;
        }

        // Every frame passes through the connection's channel; a message that
        // fails authentication ends the connection, never a plaintext retry.
        let payload_len = match channel.open(&mut buf[..read_len]) {
            Ok(len) => len,
            Err(err) => {
                set_last_error(format!("connection reader: {err}"));
                reader_cleanup(mgr, conn_id, &stop_flag);
                break;
            }
        };
        let payload_ptr = buf.as_mut_ptr();

        // A responder-commit responder's first frame from the initiator is
        // its confirmation, echoing the commit order: it has switched to
        // this connection, so the connection this one superseded can close.
        if let Some((order, deferred)) = confirm.take() {
            if buf[..payload_len] != order.to_be_bytes() {
                set_last_error("connection reader: initiator did not confirm the commit");
                reader_cleanup(mgr, conn_id, &stop_flag);
                break;
            }
            if !mgr.is_null() {
                // SAFETY: reader_loop owns a live manager pointer for this connection.
                deferred.lock_or_recover().release(unsafe { &*mgr });
            }
            continue;
        }

        if let Some(frames) = refuting.as_mut() {
            *frames += 1;
            if *frames > REFUTING_FRAME_CAP {
                set_last_error(format!(
                    "connection reader: buried peer on conn {conn_id} exceeded \
                     {REFUTING_FRAME_CAP} frames without refuting"
                ));
                close_refuting(mgr, conn_id, claim_token, &stop_flag);
                break;
            }
        }

        // Update heartbeat.
        // SAFETY: hew_now_ms has no preconditions.
        let now = unsafe { crate::clock::hew_now_ms() };
        last_activity.store(now, Ordering::Release);

        // Decode CBOR wire frame and route.
        // SAFETY: buf contains `payload_len` valid bytes from recv/decrypt.
        let frame_bytes =
            unsafe { std::slice::from_raw_parts(payload_ptr.cast_const(), payload_len) };
        let wire_frame = match decode_wire_frame(frame_bytes) {
            Ok(frame) => frame,
            Err(err) => {
                set_last_error(format!(
                    "connection reader CBOR wire-frame decode failure: {err}"
                ));
                continue;
            }
        };

        // A buried peer's connection carries SWIM alone until its refutation
        // publishes the claim; everything else it sends is dropped.
        if refuting.is_some() {
            match wire_frame {
                WireFrame::Control(control) if control.ctrl_kind == crate::envelope::CTRL_SWIM => {
                    handle_control_frame(mgr, peer_feature_flags, conn_id, claim_token, &control);
                    if claim_published(mgr, conn_id, claim_token) {
                        refuting = None;
                    }
                }
                _ => set_last_error(format!(
                    "connection reader dropped a non-SWIM frame from buried peer on conn {conn_id}"
                )),
            }
            continue;
        }

        match wire_frame {
            WireFrame::Control(control) => {
                handle_control_frame(mgr, peer_feature_flags, conn_id, claim_token, &control);
            }
            WireFrame::Envelope(mut envelope) => {
                let deadline = envelope.budget_ns.map_or_else(
                    crate::request_deadline::RequestDeadline::default,
                    |budget| crate::request_deadline::RequestDeadline::after(budget.cast_signed()),
                );
                let Some(router_fn) = router else {
                    continue;
                };

                // Reply envelopes (request_id > 0, no target or source) are
                // deposited directly into the reply routing table, bypassing
                // the normal inbound router.
                if envelope.request_id > 0 && envelope.target.is_none() && envelope.source.is_none()
                {
                    deposit_reply_envelope(mgr, conn_id, claim_token, &envelope);
                } else {
                    let Some((authenticated, peer_identity)) =
                        authenticated_peer_identity(mgr, conn_id, claim_token, "envelope")
                    else {
                        continue;
                    };
                    // SAFETY: the authenticated identity helper verified the manager.
                    let mgr_ref = unsafe { &*mgr };
                    let Some(target) = envelope.target else {
                        set_last_error("connection reader envelope missing target Location");
                        continue;
                    };
                    // Validate the envelope targets THIS node's identity and
                    // session — routing correctness only. Whether the specific
                    // actor is still live is `node_inbound_router`'s decision:
                    // an ask against a dead actor gets a typed `ActorStopped`
                    // rejection (`handle_inbound_ask`). Gating
                    // on liveness here (the former `location_matches_local`)
                    // silently dropped envelopes for an already-freed actor
                    // before either path ran, leaving the asking peer to only
                    // ever observe a timeout instead of `Dead`.
                    if !location_matches_local_session(mgr_ref, target) {
                        set_last_error("connection reader envelope target Location mismatch");
                        continue;
                    }
                    if envelope
                        .source
                        .is_some_and(|source| !location_matches_node_session(source, peer_identity))
                    {
                        set_last_error("connection reader envelope source Location mismatch");
                        continue;
                    }
                    if envelope.message == crate::envelope::EnvelopeMessage::Withdraw {
                        mgr_ref
                            .inbound_asks
                            .withdraw(target, claim_token, envelope.request_id);
                        continue;
                    }
                    let crate::envelope::EnvelopeMessage::Service {
                        handler: identity,
                        grant_id: grant,
                        kind,
                    } = &envelope.message
                    else {
                        set_last_error("request envelope has reply status");
                        continue;
                    };
                    let identity = std::ffi::CString::new(identity.as_str())
                        .expect("decoded wire identity excludes NUL");
                    // I/O recv span: bracket the router call so the
                    // mailbox enqueue captures the io_recv span as the
                    // parent of the actor-dispatch span.
                    // Fast path: `io_recv_span_begin` returns None when
                    // tracing is disabled (a single atomic load).
                    let saved_ctx = crate::tracing::io_recv_span_begin(conn_id);
                    let payload_ptr = if envelope.payload.is_empty() {
                        std::ptr::null_mut()
                    } else {
                        envelope.payload.as_mut_ptr()
                    };
                    // SAFETY: router_fn is the manager's configured inbound
                    // router; payload_ptr is null for empty payloads or points
                    // at envelope-owned bytes valid for this call.
                    unsafe {
                        router_fn(
                            target.into(),
                            identity.as_ptr(),
                            payload_ptr,
                            envelope.payload.len(),
                            envelope.request_id,
                            authenticated,
                            mgr,
                            peer_identity.node_id.into(),
                            claim_token,
                            *grant,
                            deadline.raw(),
                            *kind as u8,
                        );
                    }
                    if let Some(saved) = saved_ctx {
                        crate::tracing::io_recv_span_end(saved);
                    }
                }
            }
        }

        // A parked one-shot registry-gossip flush (initial send failed)
        // retries on this connection's next inbound traffic — SWIM keeps
        // frames flowing on an established connection, so retry latency is
        // bounded by the protocol period. One atomic load when nothing is
        // parked.
        if !mgr.is_null() {
            // SAFETY: reader_loop owns a live manager pointer for this connection.
            retry_pending_registry_flush(unsafe { &*mgr }, conn_id, claim_token);
        }
    }
}
