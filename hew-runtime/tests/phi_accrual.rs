//! Integration tests for the phi-accrual failure detector.
//!
//! Exercises [`hew_runtime::phi_accrual::PhiAccrualDetector`] through its
//! public surface to cover end-to-end scenarios that would otherwise
//! require a full SWIM cluster harness:
//!
//! - SUSPECT triggers above the production phi threshold on a real
//!   stall pattern.
//! - SUSPECT does NOT trigger below the threshold under normal,
//!   jittery cadence.
//! - Sliding-window eviction holds memory steady under sustained
//!   heartbeat traffic.
//! - The detector's stability is preserved across simulated
//!   suspect/recovery cycles when `heartbeat_anchor_only` is used.
//!
//! These tests, combined with the in-module unit tests, satisfy the
//! ≥5-unit-tests gate in the implementation brief.

#![allow(
    clippy::float_cmp,
    clippy::cast_sign_loss,
    reason = "tests assert exact sentinel return values and use signed jitter arithmetic"
)]

use hew_runtime::phi_accrual::PhiAccrualDetector;

/// Production-equivalent phi threshold used by the SWIM substrate.
/// Mirrored from `cluster.rs::PHI_THRESHOLD` (kept private to that
/// module to avoid leaking tuning knobs into the public ABI).
const PHI_THRESHOLD: f64 = 8.0;

#[test]
fn suspect_triggers_when_phi_exceeds_threshold_after_stall() {
    // Train on a realistic 1Hz heartbeat with ±20ms jitter, then stall.
    let mut det = PhiAccrualDetector::new(200, 10);
    let jitters: [i64; 8] = [-15, 10, -5, 20, -20, 5, 15, -10];
    let mut now: i64 = 0;
    for i in 0..100 {
        now += 1000 + jitters[i % jitters.len()];
        det.heartbeat(now as u64);
    }
    assert!(
        det.is_warm(),
        "detector should be warm after 100 heartbeats"
    );

    // A 30-second silence on a 1Hz heartbeat is overwhelmingly anomalous.
    let last = det
        .last_heartbeat_ms()
        .expect("trained detector has anchor");
    let phi = det.phi(last + 30_000);
    assert!(
        phi > PHI_THRESHOLD,
        "30s stall on a 1Hz heartbeat should drive phi above {PHI_THRESHOLD}, got {phi}"
    );
}

#[test]
fn suspect_does_not_trigger_under_normal_jitter() {
    // Same training pattern; query at a small fraction of one period
    // past the last heartbeat. Phi must remain well below threshold.
    let mut det = PhiAccrualDetector::new(200, 10);
    let jitters: [i64; 8] = [-15, 10, -5, 20, -20, 5, 15, -10];
    let mut now: i64 = 0;
    for i in 0..100 {
        now += 1000 + jitters[i % jitters.len()];
        det.heartbeat(now as u64);
    }
    let last = det
        .last_heartbeat_ms()
        .expect("trained detector has anchor");

    // One full period after the last heartbeat — completely normal.
    let phi = det.phi(last + 1_000);
    assert!(
        phi < PHI_THRESHOLD,
        "normal cadence one period out should not exceed threshold {PHI_THRESHOLD}, got {phi}"
    );

    // Even at 1.1× the mean interval, the small-jitter network is still
    // within normal operating range (a tight σ makes the detector more
    // confident, which is the correct adaptive behaviour — note that on
    // a sleepy/jittery network the same factor would be more tolerated).
    let phi_11x = det.phi(last + 1_100);
    assert!(
        phi_11x < PHI_THRESHOLD,
        "1.1× mean interval should not trigger SUSPECT, got {phi_11x}"
    );
}

#[test]
fn sliding_window_caps_memory_under_sustained_traffic() {
    // Pump 10,000 heartbeats through a 200-sample window and confirm
    // the sample count never exceeds the window cap.
    let window = 200usize;
    let mut det = PhiAccrualDetector::new(window, 10);
    for i in 0..10_000u64 {
        det.heartbeat(i * 100);
        assert!(
            det.sample_count() <= window,
            "sample count {} exceeded window cap {window} at i={i}",
            det.sample_count()
        );
    }
    assert_eq!(det.sample_count(), window);
}

#[test]
fn cold_start_never_suspects() {
    // Until we have enough samples, phi is forced to 0 — the SWIM
    // substrate falls back to its fixed `ping_timeout_ms` policy.
    let mut det = PhiAccrualDetector::new(200, 10);
    // 5 heartbeats → 4 intervals; below min_samples=10.
    for i in 0..5u64 {
        det.heartbeat(i * 1000);
    }
    let last = det.last_heartbeat_ms().unwrap();

    // Even after an absurd 10-minute silence, phi must report 0 while
    // cold — the brand-new-peer blind-spot is intentional and handed
    // off to the fallback path.
    let phi = det.phi(last + 600_000);
    assert_eq!(phi, 0.0, "cold-start detector must never raise phi");
}

#[test]
fn recovery_anchor_does_not_pollute_distribution() {
    // Train on a 1Hz heartbeat; the peer goes silent for 60 seconds
    // (would be SUSPECT in the live system); the SWIM caller routes
    // the recovery through `heartbeat_anchor_only` so the gap is
    // *not* folded into the distribution. The next genuine interval
    // should look entirely normal — phi at next-period+ε must stay
    // small.
    let mut det = PhiAccrualDetector::new(200, 10);
    for i in 0..50u64 {
        det.heartbeat(i * 1000);
    }
    let trained_mean = det.mean();
    let trained_std = det.std_dev();

    // Recovery from suspect: anchor only.
    let recovery_at = 50 * 1000 + 60_000;
    det.heartbeat_anchor_only(recovery_at);

    // Mean and stddev should be unchanged (no new sample pushed).
    assert!((det.mean() - trained_mean).abs() < 1e-9);
    assert!((det.std_dev() - trained_std).abs() < 1e-9);

    // The next genuine heartbeat at +1s after the anchor pushes a
    // 1000ms interval — fully in-distribution.
    det.heartbeat(recovery_at + 1000);

    // Phi at a normal one-period query after that should remain low.
    let phi = det.phi(recovery_at + 1000 + 1100);
    assert!(
        phi < PHI_THRESHOLD,
        "recovery-anchor pattern should not pollute distribution; phi={phi}"
    );
}

#[test]
fn known_input_phi_round_trip_through_public_api() {
    // Reproduce the in-module hand-calc test through the integration
    // entry-point so the public API surface is exercised end-to-end.
    let mut det = PhiAccrualDetector::new(200, 5).with_min_std_dev_ms(0.0);
    for t in [0u64, 100, 210, 300, 400, 500] {
        det.heartbeat(t);
    }
    assert_eq!(det.sample_count(), 5);
    // mean = 100, σ = √40 ≈ 6.3246.
    assert!((det.mean() - 100.0).abs() < 1e-9);
    assert!((det.std_dev() - 40f64.sqrt()).abs() < 1e-9);

    // 150ms silence → phi well above the SUSPECT threshold.
    let phi = det.phi(650);
    assert!(
        phi > PHI_THRESHOLD,
        "150ms silence on a μ=100/σ=6.3 distribution should exceed phi={PHI_THRESHOLD}, got {phi}"
    );
}
