//! Per-connection reader thread.

use std::ffi::c_int;
use std::sync::atomic::{AtomicI32, AtomicU64, Ordering};
use std::sync::Arc;
#[cfg(feature = "encryption")]
use std::sync::Mutex;

use crate::envelope::{decode_wire_frame, WireFrame};
use crate::set_last_error;
#[cfg(feature = "encryption")]
use crate::util::MutexExt;

use super::admission::hew_connmgr_remove;
use super::control::{
    authenticated_peer_identity, handle_control_frame, location_matches_local_session,
    location_matches_node_session,
};
use super::gossip::retry_pending_registry_flush;
use super::peer::deposit_reply_envelope;
use super::reconnect::{reconnect_plan, spawn_reconnect_worker};
use super::{HewConnMgr, InboundRouter, SendConnMgr, SendTransport};

// ── Reader thread ──────────────────────────────────────────────────────

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
    reason = "SendTransport and Arc values are moved into this thread from spawn closure"
)]
// Eight arguments even without the `encryption` feature (`noise_transport`
// is the ninth), so the arg list exceeds the lint threshold in both feature
// configurations and the expectation is unconditional.
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
    transport: SendTransport,
    conn_id: c_int,
    claim_token: u64,
    stop_flag: Arc<AtomicI32>,
    last_activity: Arc<AtomicU64>,
    router: Option<InboundRouter>,
    peer_feature_flags: u32,
    #[cfg(feature = "encryption")] noise_transport: Arc<Mutex<Option<snow::TransportState>>>,
) {
    let mgr = mgr.0;
    let transport = transport.0;
    let mut buf = vec![0u8; 65536]; // 64KiB read buffer (heap-allocated)

    while stop_flag.load(Ordering::Acquire) == 0 {
        // SAFETY: transport is valid for the manager's lifetime; conn_id
        // is valid for this connection's lifetime.
        let bytes_read = unsafe {
            let t = &*transport;
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

        // Decrypt in place when encryption is on; `buf.as_mut_ptr()` is stable
        // across the in-place copy, so only the length can change.
        #[cfg(feature = "encryption")]
        let payload_len = {
            let mut len = read_len;
            let mut decrypted = vec![0u8; read_len];
            let mut guard = noise_transport.lock_or_recover();
            if let Some(noise) = guard.as_mut() {
                let Ok(n) = noise.read_message(&buf[..read_len], &mut decrypted) else {
                    set_last_error("connection decrypt failure".to_string());
                    reader_cleanup(mgr, conn_id, &stop_flag);
                    break;
                };
                len = n;
                buf[..len].copy_from_slice(&decrypted[..len]);
            }
            len
        };
        #[cfg(not(feature = "encryption"))]
        let payload_len = read_len;
        let payload_ptr = buf.as_mut_ptr();

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

        match wire_frame {
            WireFrame::Control(control) => {
                handle_control_frame(mgr, peer_feature_flags, conn_id, claim_token, &control);
            }
            WireFrame::Envelope(mut envelope) => {
                let Some(router_fn) = router else {
                    continue;
                };

                // Reply envelopes (request_id > 0, no target or source) are
                // deposited directly into the reply routing table, bypassing
                // the normal inbound router.
                if envelope.request_id > 0 && envelope.target.is_none() && envelope.source.is_none()
                {
                    deposit_reply_envelope(mgr, conn_id, peer_feature_flags, &envelope);
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
                    // rejection (`handle_inbound_ask`), and a send against one
                    // is fail-closed dropped (`deliver_inbound_send`). Gating
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
                            crate::pid::hew_pid_make(mgr_ref.local_node_id, target.slot()),
                            envelope.msg_type,
                            payload_ptr,
                            envelope.payload.len(),
                            envelope.request_id,
                            authenticated,
                            mgr,
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
