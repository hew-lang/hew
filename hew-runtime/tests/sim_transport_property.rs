//! Property tests for the deterministic in-process `SimTransport`,
//! anchoring the first two `HEW-DIST-SPEC` v0 §14 invariants.
//!
//! This integration test crate is gated behind the dev-only
//! `sim-transport` feature and on `not(target_family = "wasm")` per
//! `HEW-DIST-SPEC` §15. It does **not** mutate `hew_simtime_*` or any other
//! process-global testing primitive — every property test runs against an
//! independently-owned [`SimTransport`] instance, sidestepping the
//! `global-test-isolation` hazard entirely. Determinism comes from the
//! per-instance seeded PRNG that drives drop decisions.
//!
//! ## Invariants
//!
//! - **Invariant 1 — typed-failure on partition (`HEW-DIST-SPEC` §14.1):**
//!   while `partition = true`, every `send` and `recv` resolves to a typed
//!   transport-error variant ([`SimSendOutcome::Partitioned`] /
//!   [`SimRecvOutcome::Partitioned`]). The classifier enumerates every
//!   variant explicitly so a future addition that maps to "silent success"
//!   trips the exhaustive match (`match-fail-closed`).
//!
//! - **Invariant 2 — live-session FIFO (`HEW-DIST-SPEC` §14.3):** within a
//!   single negotiated `(client, server)` session and with `drop_rate_ppm = 0`,
//!   the receiver observes payloads in the same order the sender enqueued
//!   them. The assertion does not extend across reconnect — that is the
//!   §14.4 invariant, intentionally deferred.
//!
//! ## Why this stays at the transport seam
//!
//! Bringing up two `HewNode` instances would require either extending
//! `hew_node_api_set_transport` to recognise `"sim"` (forbidden by the lane
//! brief) or reaching into private node-internal APIs (out of scope for the
//! first PR-sized slice). The §14.1 / §14.3 invariants are observable at the
//! `HewTransportOps` vtable boundary: a partition surfaces as a typed
//! transport-error variant rather than a fabricated success, and FIFO is a
//! property of the per-conn queue. Slices that ship the remaining §14
//! invariants will need ask/timeout/cancellation plumbing and will graduate
//! the harness to two real `HewNode` instances at that point.
//!
//! ## Determinism reproducibility
//!
//! `proptest` reseeds itself per case. Every case also constructs a fresh
//! `SimTransport` whose own [`SimConfig::seed`] is a function of the
//! property-test input, so re-running with `PROPTEST_CASES=… PROPTEST_SEED=…`
//! reproduces a failure deterministically. We do **not** rely on
//! `hew_simtime_*` for these two invariants because neither depends on
//! simulated time — wiring it would expand the lock-isolation surface
//! without buying any coverage.

#![cfg(all(feature = "sim-transport", not(target_family = "wasm")))]

use std::ffi::{c_int, CString};

use hew_runtime::sim_transport::{
    sim_transport_free, sim_transport_new, sim_transport_recv_classified,
    sim_transport_send_classified, sim_transport_set_partition, SimConfig, SimRecvOutcome,
    SimSendOutcome,
};
use hew_runtime::transport::HewTransport;
use proptest::prelude::*;

/// RAII guard that destroys the transport on drop. Mirrors the
/// `cleanup-all-exits` requirement: even if `prop_assert!` panics mid-test,
/// the boxed impl plus outer struct are released before the test process
/// continues, so a flaky property never leaks the `SimTransport`.
struct OwnedTransport(*mut HewTransport);

impl OwnedTransport {
    fn new(cfg: SimConfig) -> Self {
        // SAFETY: `sim_transport_new` returns a freshly-allocated pointer
        // owned exclusively by this guard.
        Self(unsafe { sim_transport_new(cfg) })
    }
    fn ptr(&self) -> *mut HewTransport {
        self.0
    }
}

impl Drop for OwnedTransport {
    fn drop(&mut self) {
        if !self.0.is_null() {
            // SAFETY: `self.0` was returned by `sim_transport_new` and has
            // not been freed elsewhere (this is the canonical release path).
            unsafe { sim_transport_free(self.0) };
            self.0 = std::ptr::null_mut();
        }
    }
}

/// Allocate a `(client_conn, server_conn)` pair on a fresh in-process
/// address. Each property-test case uses a unique address derived from the
/// case index so concurrent property cases (within or across tests) never
/// collide on listener state.
///
/// # Panics
///
/// Panics if `listen` / `connect` / `accept` returns an unexpected value.
/// Property tests treat this as a hard scaffold failure — these calls do
/// not exercise the §14 invariants under test.
fn paired_conns(t: *mut HewTransport, addr: &str) -> (c_int, c_int) {
    let caddr = CString::new(addr).expect("address contained an embedded NUL");
    // SAFETY: `t` was returned by `sim_transport_new`; `caddr` outlives every
    // call below.
    unsafe {
        let transport_ref = &*t;
        let ops = &*transport_ref.ops;
        let listen_rc =
            ops.listen.expect("sim listen vtable slot")(transport_ref.r#impl, caddr.as_ptr());
        assert_eq!(listen_rc, 0, "sim listen returned {listen_rc}");
        let client =
            ops.connect.expect("sim connect vtable slot")(transport_ref.r#impl, caddr.as_ptr());
        assert!(client > 0, "sim connect returned {client}");
        let server = ops.accept.expect("sim accept vtable slot")(transport_ref.r#impl, 0);
        assert!(server > 0, "sim accept returned {server}");
        (client, server)
    }
}

/// Whitelist of `SimSendOutcome` variants acceptable while a partition is
/// active. `match-fail-closed` discipline: any future addition to the enum
/// will fail the exhaustive match below and force the test author to
/// classify the new variant explicitly.
fn is_typed_partition_send_failure(outcome: SimSendOutcome) -> bool {
    match outcome {
        SimSendOutcome::Partitioned => true,
        SimSendOutcome::Sent { .. }
        | SimSendOutcome::DroppedByPolicy
        | SimSendOutcome::InvalidConn
        | SimSendOutcome::FrameTooLarge => false,
    }
}

/// Whitelist of `SimRecvOutcome` variants acceptable while a partition is
/// active.
fn is_typed_partition_recv_failure(outcome: SimRecvOutcome) -> bool {
    match outcome {
        SimRecvOutcome::Partitioned => true,
        SimRecvOutcome::Received { .. }
        | SimRecvOutcome::InvalidConn
        | SimRecvOutcome::BufferTooSmall => false,
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        // 64 cases per invariant is enough for confidence without slowing
        // `cargo test` past the 30-second runtime-test budget.
        cases: 64,
        // Stick to the default forking model: shrinking runs in-process.
        // If a test ever leaks a transport pointer, the `OwnedTransport`
        // RAII guard catches it before the next case starts.
        ..ProptestConfig::default()
    })]

    /// HEW-DIST-SPEC §14.1 — typed failure on partition.
    ///
    /// For any seed, any drop rate, any send sequence, while the partition
    /// flag is set: every `send` returns a typed transport-error variant
    /// (never a fabricated success), and any subsequent `recv` returns a
    /// typed transport-error variant (never a fabricated payload).
    #[test]
    fn partition_yields_typed_failure(
        seed in any::<u64>(),
        drop_rate_ppm in 0u32..=1_000_000,
        msg_count in 1usize..16,
        case_id in any::<u32>(),
    ) {
        let cfg = SimConfig {
            seed,
            drop_rate_ppm,
            ..SimConfig::default()
        };
        let t = OwnedTransport::new(cfg);
        let addr = format!("sim://partition-typed-failure/{case_id}");
        let (client, server) = paired_conns(t.ptr(), &addr);

        // Activate the partition before any send so every operation sees it.
        // SAFETY: `t.ptr()` is the guard's live pointer.
        unsafe { sim_transport_set_partition(t.ptr(), true) };

        for i in 0..msg_count {
            let payload = (i as u64).to_le_bytes();
            // SAFETY: payload is a stack slice valid for its own length.
            let outcome = unsafe { sim_transport_send_classified(t.ptr(), client, &payload) };
            prop_assert!(
                is_typed_partition_send_failure(outcome),
                "send #{i} during partition returned non-partition outcome {outcome:?}; \
                 SimTransport must surface a typed transport-error variant under partition, \
                 never a fabricated success or sentinel"
            );
        }

        let mut buf = [0u8; 64];
        // SAFETY: buf is a live local slice for its own length.
        let recv_outcome = unsafe { sim_transport_recv_classified(t.ptr(), server, &mut buf) };
        prop_assert!(
            is_typed_partition_recv_failure(recv_outcome),
            "recv during partition returned non-partition outcome {recv_outcome:?}; \
             SimTransport must surface a typed transport-error variant under partition, \
             never a fabricated payload"
        );

        // OwnedTransport drop frees the transport regardless of how the
        // assertions resolve (`cleanup-all-exits`).
    }

    /// HEW-DIST-SPEC §14.3 — live-session FIFO.
    ///
    /// Within a single negotiated `(client, server)` session and with
    /// `drop_rate_ppm = 0`, the receiver observes payloads in the same order
    /// the sender enqueued them. Reconnection / cross-session ordering is
    /// not part of this assertion — that is §14.4, deferred.
    #[test]
    fn live_session_preserves_send_order(
        seed in any::<u64>(),
        payloads in proptest::collection::vec(any::<u32>(), 1..32),
        case_id in any::<u32>(),
    ) {
        let cfg = SimConfig {
            seed,
            drop_rate_ppm: 0, // FIFO assertion requires no drops.
            ..SimConfig::default()
        };
        let t = OwnedTransport::new(cfg);
        let addr = format!("sim://live-session-fifo/{case_id}");
        let (client, server) = paired_conns(t.ptr(), &addr);

        for (idx, p) in payloads.iter().enumerate() {
            let bytes = p.to_le_bytes();
            // SAFETY: bytes is a stack slice valid for its own length.
            let outcome = unsafe { sim_transport_send_classified(t.ptr(), client, &bytes) };
            // Match exhaustively to keep the assertion fail-closed.
            match outcome {
                SimSendOutcome::Sent { bytes_sent } => {
                    prop_assert_eq!(bytes_sent, bytes.len(),
                        "send #{} reported {} bytes, expected {}", idx, bytes_sent, bytes.len());
                }
                SimSendOutcome::DroppedByPolicy
                | SimSendOutcome::Partitioned
                | SimSendOutcome::InvalidConn
                | SimSendOutcome::FrameTooLarge => {
                    prop_assert!(false,
                        "send #{idx} returned unexpected outcome {outcome:?}; \
                         live-session FIFO scenario expects every send to be Sent");
                }
            }
        }

        let mut received: Vec<u32> = Vec::with_capacity(payloads.len());
        for idx in 0..payloads.len() {
            let mut buf = [0u8; 4];
            // SAFETY: buf is a live local slice for its own length.
            let outcome = unsafe { sim_transport_recv_classified(t.ptr(), server, &mut buf) };
            match outcome {
                SimRecvOutcome::Received { bytes_received } => {
                    prop_assert_eq!(bytes_received, 4,
                        "recv #{} reported {} bytes, expected 4", idx, bytes_received);
                    received.push(u32::from_le_bytes(buf));
                }
                SimRecvOutcome::Partitioned
                | SimRecvOutcome::InvalidConn
                | SimRecvOutcome::BufferTooSmall => {
                    prop_assert!(false,
                        "recv #{idx} returned unexpected outcome {outcome:?}; \
                         live-session FIFO scenario expects every recv to be Received");
                }
            }
        }

        prop_assert_eq!(received, payloads,
            "receiver observed reordering inside a single negotiated session — \
             violates HEW-DIST-SPEC §14.3");

        // Drop guard releases the transport before the next proptest case.
    }
}
