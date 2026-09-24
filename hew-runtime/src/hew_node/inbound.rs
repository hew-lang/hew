//! Inbound frame routing, ask handling and the inbound worker bound.

use super::{
    submission_delivered, with_current_node_read, AskError, AskRejectionReasonCode, HewNode,
    InboundRequest, SendConnMgr, SendTransport,
};
use crate::connection;
use crate::envelope::encode_envelope_frame_from_raw_parts;
use crate::set_last_error;
use crate::transport::HEW_CONN_INVALID;
#[cfg(test)]
use crate::util::CondvarExt;
use crate::util::MutexExt;
use std::ffi::c_void;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
#[cfg(test)]
use std::sync::{Condvar, Mutex};
use std::thread;

// ---------------------------------------------------------------------------
// Inbound ask worker bound
// ---------------------------------------------------------------------------

/// Maximum number of OS threads that may concurrently service inbound remote
/// asks.  A remote peer cannot exceed this by flooding ask requests, bounding
/// both OS thread count and virtual-memory usage.
///
/// The value is intentionally generous (64) so that legitimate high-fanout
/// workloads are unaffected while still preventing unbounded growth.
pub(crate) const INBOUND_ASK_WORKER_LIMIT: usize = 64;

/// Sentinel `msg_type` value used in reply envelopes to signal that the
/// inbound ask was **rejected** (worker-limit exceeded) rather than answered.
///
/// Normal reply envelopes carry `msg_type = 0`. The connection reader checks
/// for this sentinel to call [`fail_remote_reply`] instead of
/// [`complete_remote_reply`], ensuring both void and non-void asks fail
/// closed with [`AskError::WorkerAtCapacity`] on the originating node.
///
/// The value `65535` is the maximum valid `msg_type` in the Hew wire
/// protocol (`0..=MAX_MSG_TYPE`).  Reply envelopes always use `msg_type = 0`
/// by convention, so `65535` is unambiguous as a rejection marker.
pub(crate) const HEW_REPLY_REJECT_MSG_TYPE: i32 = 65_535;

/// Count of currently active inbound ask-handler threads.
///
/// Incremented before spawning, decremented by [`InboundAskGuard`] when the
/// handler thread exits (or panics — the `Drop` impl runs in both cases).
pub(super) static INBOUND_ASK_ACTIVE: AtomicUsize = AtomicUsize::new(0);

/// RAII guard that decrements [`INBOUND_ASK_ACTIVE`] and the per-manager
/// active counter exactly once on drop.
///
/// Constructed in the spawned ask-handler thread so both counters stay
/// accurate even under panics or early returns.
pub(super) struct InboundAskGuard(pub(super) Arc<AtomicUsize>);

impl Drop for InboundAskGuard {
    fn drop(&mut self) {
        INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
        // SeqCst (matching the spawn-gate protocol in `node_inbound_router` and
        // the drain in `drain_inbound_ask_workers`): this decrement is the
        // signal the Phase-1 drain waits on. A worker only reaches here after
        // `handle_inbound_ask` has flushed its reply, so when the drain observes
        // the counter hit zero, every counted worker's reply has been sent.
        self.0.fetch_sub(1, Ordering::SeqCst);
    }
}

#[cfg(test)]
#[derive(Default)]
enum TestGateMode {
    #[default]
    Disabled,
    Notify,
    Blocked,
    Released,
}

#[cfg(test)]
#[derive(Default)]
struct TestGateState {
    mode: TestGateMode,
    entered: bool,
}

#[cfg(test)]
#[derive(Default)]
pub(super) struct TestGate {
    state: Mutex<TestGateState>,
    cond: Condvar,
}

#[cfg(test)]
impl TestGate {
    pub(super) fn arm(&self, block_on_enter: bool) {
        let mut state = self.state.lock_or_recover();
        *state = TestGateState {
            mode: if block_on_enter {
                TestGateMode::Blocked
            } else {
                TestGateMode::Notify
            },
            entered: false,
        };
        self.cond.notify_all();
    }

    pub(super) fn hit(&self) {
        let mut state = self.state.lock_or_recover();
        if matches!(state.mode, TestGateMode::Disabled) {
            return;
        }
        state.entered = true;
        self.cond.notify_all();
        while matches!(state.mode, TestGateMode::Blocked) {
            state = self.cond.wait_or_recover(state);
        }
    }

    pub(super) fn release(&self) {
        let mut state = self.state.lock_or_recover();
        if matches!(state.mode, TestGateMode::Disabled) {
            return;
        }
        state.mode = TestGateMode::Released;
        self.cond.notify_all();
    }

    pub(super) fn wait_for_enter(&self, timeout: std::time::Duration) -> bool {
        let deadline = std::time::Instant::now() + timeout;
        let mut state = self.state.lock_or_recover();
        while !matches!(state.mode, TestGateMode::Disabled) && !state.entered {
            let remaining = deadline.saturating_duration_since(std::time::Instant::now());
            if remaining.is_zero() {
                return false;
            }
            let (next, wait_result) = self.cond.wait_timeout_or_recover(state, remaining);
            state = next;
            if wait_result.timed_out() && !state.entered {
                return false;
            }
        }
        state.entered
    }

    pub(super) fn reset(&self) {
        let mut state = self.state.lock_or_recover();
        *state = TestGateState::default();
        self.cond.notify_all();
    }
}

#[cfg(test)]
pub(super) static INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK: std::sync::LazyLock<TestGate> =
    std::sync::LazyLock::new(TestGate::default);
#[cfg(test)]
pub(super) static NODE_STOP_BEFORE_CONNMGR_FREE_HOOK: std::sync::LazyLock<TestGate> =
    std::sync::LazyLock::new(TestGate::default);
/// Fires in `node_inbound_router` AFTER the per-manager counter increment and
/// BEFORE the spawn-gate re-check. Lets a test wedge a router exactly in the
/// Dekker window: counter already incremented, gate not yet re-read. A
/// concurrent `hew_node_stop` drain that closes the gate and then loads the
/// counter MUST observe this worker (count ≥ 1) and wait — proving the atomic
/// gate/counter protocol.
#[cfg(test)]
pub(super) static INBOUND_ROUTER_AFTER_INCREMENT_HOOK: std::sync::LazyLock<TestGate> =
    std::sync::LazyLock::new(TestGate::default);
/// Fires at the TOP of `handle_inbound_ask`, BEFORE the up-front feature-flags
/// capture acquires the `CURRENT_NODE` read lock. Lets a test wedge an uncounted
/// straggler worker (one driven directly, past the drain ceiling) so a
/// concurrent `hew_node_stop` on THAT worker's node can complete teardown —
/// including freeing the node's `conn_mgr` — while the worker is parked. On
/// release, the worker evaluates the capture's barrier: the fix returns `None`
/// via this node's `shutdown_started` flag instead of dereferencing the freed
/// `conn_mgr`. Proves the capture is safe for a SECONDARY node whose teardown
/// does not zero the global `CURRENT_NODE`.
#[cfg(test)]
pub(super) static INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK: std::sync::LazyLock<TestGate> =
    std::sync::LazyLock::new(TestGate::default);

pub(super) unsafe extern "C" fn node_inbound_router(
    target_actor_id: u64,
    msg_type: i32,
    data: *mut u8,
    size: usize,
    request_id: u64,
    source_node_id: u16,
    conn_mgr: *mut connection::HewConnMgr,
) {
    if request_id > 0 && source_node_id > 0 {
        // Inbound remote ask — dispatch locally and send the reply back.

        // ── Backpressure: bounded concurrent inbound ask workers ─────────────
        //
        // Optimistically increment the counter. If we were already at the
        // limit we revert and — if the source peer understands the rejection
        // sentinel (HEW_FEATURE_SUPPORTS_ASK_REJECTION) — send a rejection
        // reply envelope back (msg_type = HEW_REPLY_REJECT_MSG_TYPE). The
        // connection reader on the originating node dispatches this sentinel
        // to `fail_remote_reply`, which sets ReplyStatus::Failed with reason
        // WorkerAtCapacity so `hew_node_api_ask` returns the precise
        // discriminant — fail-closed for both void and non-void asks.
        //
        // If the source peer does NOT advertise the feature flag (old node),
        // we send no reply at all.  The originating ask will time out through
        // its normal deadline path — this is the safe fail-closed fallback: an
        // old peer that never sends the sentinel cannot misinterpret 65535 as
        // a void-success, and we avoid the silent-success regression.
        let prev = INBOUND_ASK_ACTIVE.fetch_add(1, Ordering::AcqRel);
        if prev >= INBOUND_ASK_WORKER_LIMIT {
            INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
            // SAFETY: the inbound router is only called while `conn_mgr` is live.
            let peer_flags =
                unsafe { connection::hew_connmgr_feature_flags_for_node(conn_mgr, source_node_id) };
            if connection::supports_ask_rejection(peer_flags) {
                // SAFETY: conn_mgr is live for the duration of the inbound router call.
                let shutdown = unsafe { connection::hew_connmgr_shutdown_flag(conn_mgr) };
                if let Some(shutdown) = shutdown {
                    send_rejection_reply(
                        source_node_id,
                        request_id,
                        AskError::WorkerAtCapacity,
                        conn_mgr,
                        shutdown.as_ref(),
                    );
                }
            }
            return;
        }
        // ─────────────────────────────────────────────────────────────────────

        // Deep-copy the payload so we can hand it to a background thread.
        let payload = if size > 0 && !data.is_null() {
            // SAFETY: data is valid for `size` bytes (reader_loop contract).
            unsafe { std::slice::from_raw_parts(data, size) }.to_vec()
        } else {
            Vec::new()
        };
        let conn_mgr_send = SendConnMgr(conn_mgr);
        // SAFETY: the inbound router is only called while `conn_mgr` is live.
        let Some(shutdown_started) = (unsafe { connection::hew_connmgr_shutdown_flag(conn_mgr) })
        else {
            // Undo the increment we already committed above.
            INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
            return;
        };
        // Acquire the per-manager active counter and the spawn-gate flag. The
        // counter tracks workers for THIS conn_mgr specifically, so hew_node_stop
        // drains only its own workers (not another node's in a multi-node test).
        // SAFETY: conn_mgr is live for the duration of this router call.
        let Some(per_mgr_active) =
            (unsafe { connection::hew_connmgr_inbound_ask_active(conn_mgr) })
        else {
            INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
            return;
        };
        // SAFETY: conn_mgr is live for the duration of this router call.
        let Some(spawn_gate) =
            (unsafe { connection::hew_connmgr_inbound_spawn_closed_flag(conn_mgr) })
        else {
            per_mgr_active.fetch_sub(1, Ordering::SeqCst);
            INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
            return;
        };
        // ── ATOMIC spawn-gate + counter protocol (close the teardown race) ───
        //
        // The spawn-gate check and the counter increment MUST be atomic with
        // respect to `hew_node_stop`'s Phase-1 drain, or a worker can pass the
        // gate, then have the drain close the gate and observe a zero counter,
        // then increment + spawn AFTER the drain finished — reaching Phase 2 and
        // abandoning its reply (the production reply-abandon race this fix
        // closes). We make it atomic with the increment-then-recheck (Dekker)
        // pattern under SeqCst:
        //
        //   router: per_mgr_active.fetch_add(1, SeqCst); gate.load(SeqCst)
        //   drain : gate.store(true, SeqCst);            per_mgr_active.load(SeqCst)
        //
        // SeqCst on BOTH sides (not AcqRel — that permits the store/load on the
        // two distinct atomics to reorder) gives a single total order in which
        // at least one thread observes the other's store. So either the drain
        // sees count ≥ 1 and waits for this worker, OR this router sees the gate
        // closed and bails before spawning. It is impossible for the router to
        // see the gate open AND the drain to see count 0 — i.e. a worker that
        // passes this gate is guaranteed visible to a concurrent drain, and a
        // worker counted-out by the drain cannot spawn.
        //
        // Two signals close the window:
        //   • `inbound_spawn_closed` — set FIRST by `hew_node_stop`, before its
        //     drain, so already-running `handle_inbound_ask` threads still flush
        //     their replies while NEW workers are turned away here.
        //   • `reconnect_shutdown` — set in Phase 2 (`hew_connmgr_mark_stopping`
        //     / `hew_connmgr_free`); a worker spawned past it would bail inside
        //     `handle_inbound_ask` with conn_mgr on the verge of being freed.
        per_mgr_active.fetch_add(1, Ordering::SeqCst);
        // Test seam: wedge a router in the Dekker window (incremented, gate not
        // yet re-read) so a test can prove a concurrent drain always observes
        // this worker. No-op in production (Disabled mode).
        #[cfg(test)]
        INBOUND_ROUTER_AFTER_INCREMENT_HOOK.hit();
        if spawn_gate.load(Ordering::SeqCst) || shutdown_started.load(Ordering::SeqCst) {
            per_mgr_active.fetch_sub(1, Ordering::SeqCst);
            INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
            return;
        }
        // Construct the guard *before* spawning so both counter decrements are
        // covered regardless of whether spawn succeeds:
        //   • spawn succeeds: guard moves into the closure; Drop runs when the
        //     thread exits or panics.
        //   • spawn fails (OOM): thread::spawn drops the closure, which calls
        //     InboundAskGuard::drop and decrements both counters immediately.
        // Previously the guard was created inside the closure body, leaving a
        // window where a spawn failure would leak both INBOUND_ASK_ACTIVE and
        // the per-manager counter.
        let guard = InboundAskGuard(per_mgr_active);
        let _ = thread::spawn(move || {
            let _guard = guard;
            handle_inbound_ask(
                target_actor_id,
                msg_type,
                &payload,
                request_id,
                source_node_id,
                conn_mgr_send,
                shutdown_started,
            );
        });
    } else {
        // Fire-and-forget message — reconstruct the value into THIS node's
        // address space before delivering it to the local mailbox.
        // SAFETY: data is valid for `size` bytes (reader_loop contract).
        unsafe { deliver_inbound_send(target_actor_id, msg_type, data, size) };
    }
}

/// Deliver an inbound fire-and-forget (`send`) frame to its target actor's local
/// mailbox, reconstructing the value into THIS node's address space first. The
/// inbound `data` is the serialized wire form; feeding it raw to the mailbox
/// would make the actor handler dereference sender-side heap pointers and crash.
///
/// Fail closed throughout: an unregistered codec, a decode failure, or a
/// target actor that is no longer live all DROP the message rather than deliver
/// garbage. An empty payload (`size == 0`) is a genuine zero-field message and
/// bypasses decode.
///
/// # Safety
/// `data` must be valid for `size` bytes (or null when `size == 0`).
unsafe fn deliver_inbound_send(target_actor_id: u64, msg_type: i32, data: *mut u8, size: usize) {
    let codec = crate::lifetime::live_actors::dispatch_ptr_by_id(target_actor_id)
        .and_then(|dispatch| crate::xnode_serial::lookup_request(dispatch, msg_type));
    let payload = if size == 0 {
        &[][..]
    } else {
        // SAFETY: data is valid for `size` bytes (caller contract).
        unsafe { std::slice::from_raw_parts(data.cast_const(), size) }
    };
    // SAFETY: a registered codec decodes into a fresh owned wrapper.
    let request = codec.and_then(|codec| unsafe { InboundRequest::decode(codec, payload) });
    let Some(request) = request else {
        set_last_error(format!(
            "cross-node send dropped: target actor {target_actor_id} has no codec \
             or the frame failed to decode for msg_type={msg_type}"
        ));
        return;
    };
    if !request
        .submit(target_actor_id, msg_type)
        .is_some_and(submission_delivered)
    {
        set_last_error(format!(
            "cross-node send dropped: target actor {target_actor_id} refused msg_type={msg_type}"
        ));
    }
}

/// Handle an inbound remote ask by performing a local blocking ask and
/// sending the reply envelope back to the requesting node.
#[expect(
    clippy::needless_pass_by_value,
    reason = "spawned ask-handler thread must own the shutdown flag clone"
)]
pub(super) fn handle_inbound_ask(
    target_actor_id: u64,
    msg_type: i32,
    payload: &[u8],
    request_id: u64,
    source_node_id: u16,
    conn_mgr: SendConnMgr,
    shutdown_started: Arc<AtomicBool>,
) {
    // Test seam: wedge a straggler worker at the top of the handler, BEFORE the
    // feature-flags capture acquires the CURRENT_NODE read lock, so a test can
    // let a concurrent secondary-node stop free `conn_mgr` while we are parked
    // here. No-op in production (Disabled mode).
    #[cfg(test)]
    INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK.hit();

    // Capture the peer's negotiated feature flags ONCE, up front, under the
    // SAME barrier that protects `send_reply_envelope` / `send_rejection_reply`:
    // the CURRENT_NODE read lock AND THIS manager's `shutdown_started`
    // (`reconnect_shutdown`) flag. The three fail-closed error paths below
    // (decode failure, actor error, encode failure) need these flags to decide
    // whether the peer understands the ask-rejection sentinel. Reading them at
    // each error site dereferenced `conn_mgr` OUTSIDE any barrier: if the 5s
    // Phase-1 drain ceiling expired and Phase 2 freed `conn_mgr`, a straggler
    // dereferenced freed memory (use-after-free). The flags are negotiated once
    // at handshake and never change for the connection's life, so a single
    // guarded read at the top is equivalent to reading at each site.
    //
    // `*guard == 0` alone is INSUFFICIENT for a SECONDARY node. `hew_node_stop`
    // zeroes CURRENT_NODE only for the node that owns it (`*guard == this node`);
    // stopping a secondary node in a multi-node runtime leaves CURRENT_NODE
    // pointing at a DIFFERENT (still-running) node, so `*guard != 0` and the
    // old check passed — then `feature_flags_for_node` dereferenced the
    // secondary node's ALREADY-FREED `conn_mgr`. What IS set per-node is
    // `reconnect_shutdown`: `hew_node_stop` calls `mark_stopping` on the
    // stopping node's own `conn_mgr` (regardless of CURRENT_NODE) INSIDE the
    // CURRENT_NODE write lock, before Phase 2 frees it. `shutdown_started` is a
    // clone of exactly that flag (captured in `node_inbound_router`). So the
    // pairing is the same as `send_reply_envelope`:
    //   • if this read holds the read lock first, stop's write lock blocks until
    //     we release it → `conn_mgr` is valid for the `feature_flags_for_node`
    //     call (which happens inside the closure, under the read lock);
    //   • if stop took the write lock first, it set this node's
    //     `shutdown_started = true` before releasing; we then observe that flag
    //     here and return `None` (a straggler past the drain ceiling) WITHOUT
    //     touching the soon-to-be-freed manager. The error paths skip the
    //     rejection send, fail-closed.
    let peer_flags: Option<u32> = with_current_node_read(|guard| {
        if *guard == 0 || shutdown_started.load(Ordering::Acquire) {
            return None;
        }
        // SAFETY: the CURRENT_NODE read lock held here blocks `hew_node_stop`'s
        // write-lock teardown, and `shutdown_started` was observed false under
        // that lock, so this node's `conn_mgr` cannot be freed for this read.
        Some(unsafe { connection::hew_connmgr_feature_flags_for_node(conn_mgr.0, source_node_id) })
    });

    let reject = |reason: AskError| {
        #[cfg(test)]
        INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK.hit();
        // `peer_flags` was captured up front under the CURRENT_NODE barrier;
        // `None` means a straggler past the drain ceiling, so no rejection
        // is sent and the originating ask resolves on its own deadline.
        if peer_flags.is_some_and(connection::supports_ask_rejection) {
            send_rejection_reply(
                source_node_id,
                request_id,
                reason,
                conn_mgr.0,
                shutdown_started.as_ref(),
            );
        }
    };
    // A dead target (never spawned, or already freed) has no dispatch pointer
    // tracked at all: that is an ActorStopped fact (folds to the public
    // `ActorError.Dead`, matching the local ask route, D526), never a decode
    // problem. Only once a live actor's dispatch is resolved does a missing
    // codec registration for `msg_type` become a genuine DecodeFailure.
    let Some(dispatch) = crate::lifetime::live_actors::dispatch_ptr_by_id(target_actor_id) else {
        reject(AskError::ActorStopped);
        return;
    };
    // The codecs are keyed by the target actor type's dispatch pointer, so a
    // frame is decoded, and its reply encoded, only by that actor's member.
    let codecs = crate::xnode_serial::lookup_request(dispatch, msg_type)
        .zip(crate::xnode_serial::lookup_reply(dispatch, msg_type));
    let Some((request_codec, reply_codec)) = codecs else {
        reject(AskError::DecodeFailure);
        return;
    };
    // SAFETY: the registered codec decodes into a fresh owned wrapper.
    let Some(request) = (unsafe { InboundRequest::decode(request_codec, payload) }) else {
        reject(AskError::DecodeFailure);
        return;
    };
    let reply_data = match request.ask(target_actor_id, msg_type, reply_codec) {
        Ok(reply) => reply,
        Err(reason) => {
            reject(reason);
            return;
        }
    };

    // Send the reply envelope back to the requesting node.
    send_reply_envelope(
        source_node_id,
        request_id,
        &reply_data,
        conn_mgr.0,
        shutdown_started.as_ref(),
    );
}

/// Send a **rejection** reply envelope back to the source node.
///
/// Rejection replies carry [`HEW_REPLY_REJECT_MSG_TYPE`] in the `msg_type`
/// field plus a 1-byte [`AskError`] reason payload. The connection reader on
/// the receiving node recognises the sentinel and calls
/// [`fail_remote_reply`] instead of [`complete_remote_reply`], preserving the
/// remote rejection reason for the originating `hew_node_api_ask` caller.
fn send_rejection_reply(
    target_node_id: u16,
    request_id: u64,
    reason: AskError,
    conn_mgr: *mut connection::HewConnMgr,
    shutdown_started: &AtomicBool,
) {
    if conn_mgr.is_null() {
        return;
    }

    with_current_node_read(|guard| {
        if *guard == 0 {
            return;
        }
        if shutdown_started.load(Ordering::Acquire) {
            return;
        }

        // SAFETY: conn_mgr is the manager that received the ask. The CURRENT_NODE
        // read lock held above (guard) ensures it remains valid for this call.
        let conn_id = unsafe { connection::hew_connmgr_conn_id_for_node(conn_mgr, target_node_id) };
        if conn_id < 0 {
            return;
        }

        let reason_payload = [AskRejectionReasonCode::encode(reason)
            .expect("remote ask rejection reason must use a supported rejection-reason code")];
        // Encode the rejection envelope: request_id identifies the pending ask;
        // source_node_id = 0 marks it as a reply; msg_type = HEW_REPLY_REJECT_MSG_TYPE
        // distinguishes it from a normal (possibly void) success reply.
        // SAFETY: `reason_payload` is a stack byte array valid for its length.
        let bytes = match unsafe {
            encode_envelope_frame_from_raw_parts(
                None,
                None,
                HEW_REPLY_REJECT_MSG_TYPE,
                reason_payload.as_ptr(),
                reason_payload.len(),
                request_id,
            )
        } {
            Ok(bytes) => bytes,
            Err(err) => {
                set_last_error(format!("send_rejection_reply: {err}"));
                return;
            }
        };
        // SAFETY: conn_mgr and conn_id are valid; bytes is CBOR encoded.
        unsafe {
            connection::hew_connmgr_send_preencoded(conn_mgr, conn_id, bytes.as_ptr(), bytes.len())
        };
    });
}

/// Encode and send a reply envelope back to the source node.
///
/// Uses `conn_mgr` directly so the reply is routed via the connection that
/// received the original ask.
pub(super) fn send_reply_envelope(
    target_node_id: u16,
    request_id: u64,
    reply_data: &[u8],
    conn_mgr: *mut connection::HewConnMgr,
    shutdown_started: &AtomicBool,
) {
    if conn_mgr.is_null() {
        return;
    }

    // Synchronize with `hew_node_stop` to prevent a use-after-free on
    // `conn_mgr`.  `hew_node_stop` holds the `CURRENT_NODE` write lock while
    // it clears the pointer to zero (and only frees `conn_mgr` afterward), so:
    //
    // * If this thread acquires the read lock first, stop is blocked until we
    //   release it — `conn_mgr` is guaranteed valid for this whole function.
    // * If stop cleared `CURRENT_NODE` first, we see `*guard == 0` here and
    //   return before touching `conn_mgr`.
    //
    // The read lock is held for the entire duration of `conn_mgr` access via
    // the `read_access` closure.
    with_current_node_read(|guard| {
        if *guard == 0 {
            return;
        }
        if shutdown_started.load(Ordering::Acquire) {
            return;
        }

        // Find the connection back to the requesting node.
        // SAFETY: conn_mgr is the manager that received the ask. The CURRENT_NODE
        // read lock held above (guard) ensures it remains valid for this call.
        let conn_id = unsafe { connection::hew_connmgr_conn_id_for_node(conn_mgr, target_node_id) };
        if conn_id < 0 {
            return;
        }

        // Encode the reply envelope with request_id set and source_node_id = 0
        // to mark it as a reply (not a new request).
        // SAFETY: `reply_data.as_ptr()` is valid for `reply_data.len()` bytes.
        let bytes = match unsafe {
            encode_envelope_frame_from_raw_parts(
                None,
                None,
                0,
                reply_data.as_ptr(),
                reply_data.len(),
                request_id,
            )
        } {
            Ok(bytes) => bytes,
            Err(err) => {
                set_last_error(format!("send_reply_envelope: {err}"));
                return;
            }
        };

        // Send via conn_mgr so noise encryption is applied when the connection
        // is encrypted. This replaces the former raw transport send which would
        // send unencrypted data over an encrypted connection.
        // SAFETY: conn_mgr and conn_id are valid; bytes is CBOR encoded.
        unsafe {
            connection::hew_connmgr_send_preencoded(conn_mgr, conn_id, bytes.as_ptr(), bytes.len())
        };
    });
}

pub(super) fn accept_loop(transport: SendTransport, conn_mgr: SendConnMgr, stop: &AtomicBool) {
    while !stop.load(Ordering::Acquire) {
        // SAFETY: pointers are valid for the lifetime of the spawned loop.
        let conn_id = unsafe {
            let t = &*transport.0;
            let Some(ops) = t.ops.as_ref() else { break };
            let Some(accept_fn) = ops.accept else { break };
            accept_fn(t.r#impl, 200)
        };

        if conn_id != HEW_CONN_INVALID {
            // SAFETY: pointers are held by HewNode for the loop lifetime.
            let _ = unsafe { connection::hew_connmgr_add(conn_mgr.0, conn_id) };
        }
    }
}

pub(super) fn actor_id_to_registry_ptr(actor_id: u64) -> *mut c_void {
    let encoded = usize::try_from(actor_id)
        .expect("u64 actor IDs fit in usize on supported targets (64-bit required)");
    encoded as *mut c_void
}

/// Wait for this manager's in-flight inbound-ask worker threads to drain to
/// zero, with a 5-second ceiling.
///
/// Each worker spawned by [`node_inbound_router`] increments the per-manager
/// counter and decrements it (via `InboundAskGuard`) when `handle_inbound_ask`
/// returns — i.e. after its reply has been written to the wire. Draining the
/// counter therefore guarantees every already-computed reply has flushed before
/// the caller proceeds to the `CURRENT_NODE` teardown barrier and frees
/// `conn_mgr`. The per-manager counter (not the process-global
/// `INBOUND_ASK_ACTIVE`) is used so stopping one node in a multi-node test does
/// not wait on another node's workers.
///
/// The ceiling bounds a misbehaving actor dispatch; in practice well-behaved
/// nodes drain in microseconds. A worker still running after the ceiling is
/// caught by the downstream `CURRENT_NODE == 0` / `reconnect_shutdown` barrier
/// (it bails without touching the soon-to-be-freed manager).
pub(super) fn drain_inbound_ask_workers(inbound_active: Option<&Arc<AtomicUsize>>) {
    const MAX_DRAIN: std::time::Duration = std::time::Duration::from_secs(5);
    const POLL: std::time::Duration = std::time::Duration::from_millis(1);
    let Some(active) = inbound_active else {
        return;
    };
    let deadline = std::time::Instant::now() + MAX_DRAIN;
    // SeqCst (matching `node_inbound_router`'s gate protocol): the caller stored
    // `inbound_spawn_closed = true` with SeqCst BEFORE this drain. This SeqCst
    // load is the drain side of the Dekker pairing — it cannot observe a zero
    // counter while a router that saw the gate open is still between its
    // increment and spawning. Once it reads zero, no further worker can spawn
    // (all routers now see the gate closed and bail), so the count stays zero.
    while active.load(Ordering::SeqCst) > 0 {
        if std::time::Instant::now() >= deadline {
            break;
        }
        thread::sleep(POLL);
    }
}

pub(super) fn stop_node_accept_thread(node: &mut HewNode) {
    node.accept_stop.store(true, Ordering::Release);
    let handle = node.accept_thread.lock_or_recover().take();
    if let Some(handle) = handle {
        crate::util::report_join_panic("hew node accept thread", handle.join());
    }
}
