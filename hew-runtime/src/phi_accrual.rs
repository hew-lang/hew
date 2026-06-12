//! Phi-accrual failure detector (Hayashibara et al., 2004).
//!
//! Adaptive heartbeat-based failure detection that estimates the
//! probability that a peer has failed given the inter-arrival
//! distribution of recent heartbeats. Used by SWIM (`cluster.rs`)
//! to replace the fixed `ping_timeout_ms` ALIVE → SUSPECT trigger
//! with a network-condition-adaptive one.
//!
//! # Reference
//!
//! N. Hayashibara, X. Défago, R. Yared, T. Katayama,
//! *"The φ Accrual Failure Detector"*, SRDS 2004.
//! <https://www.computer.org/csdl/proceedings-article/srds/2004/22390066/12OmNvT2phv>
//!
//! Akka, Cassandra, and Hashicorp memberlist all derive their
//! production failure detectors from this paper.
//!
//! # Algorithm
//!
//! 1. Record inter-arrival intervals of heartbeats in a sliding window.
//! 2. Estimate distribution parameters (mean μ, stddev σ) over the window,
//!    assuming an approximately normal distribution.
//! 3. On every "is peer alive?" check at time `now`, compute:
//!    ```text
//!    t        = now - last_heartbeat
//!    P_later  = P(interval > t | history)  ≈ 1 - Φ((t - μ) / σ)
//!    φ        = -log10(P_later)
//!    ```
//! 4. The peer is suspected when `φ > threshold` (typically 8.0,
//!    meaning the observed silence has probability ≤ 10⁻⁸ under the
//!    learned distribution).
//!
//! Higher `φ` ⇒ more confidence the peer is dead. The mapping from
//! `t` to suspicion is automatically tuned by the observed jitter:
//! a snappy network detects failures fast; a sleepy/congested network
//! tolerates longer silences before suspecting.
//!
//! # Normal CDF approximation
//!
//! `std` does not provide `erf`. We use the closed-form approximation
//! popularised by Akka:
//!
//! ```text
//! y = (t - μ) / σ
//! e = exp(-y · (1.5976 + 0.070566 · y²))
//! P_later(t) = if t > μ { e / (1 + e) }
//!              else     { 1 - 1 / (1 + e) }
//! ```
//!
//! This matches the survival function `1 - Φ(y)` of the standard
//! normal to within ~10⁻⁴ for the |y| range that matters here.

use std::collections::VecDeque;

/// Floor on the estimated standard deviation, in milliseconds.
///
/// Without a floor, a streak of perfectly-regular heartbeats yields
/// `σ = 0`, which makes the phi calculation degenerate (any
/// `t > μ` would map to `φ = ∞`). Setting a floor models the
/// irreducible network/OS-scheduling jitter that always exists in
/// practice. Akka's default is `100 ms`; we pick a slightly tighter
/// value because we expect snappier intra-cluster RTTs.
pub const DEFAULT_MIN_STD_DEV_MS: f64 = 50.0;

/// Phi-accrual failure detector for a single peer.
///
/// Tracks heartbeat arrival intervals in a bounded sliding window
/// and computes `φ(t)` for the current silence duration.
#[derive(Debug, Clone)]
pub struct PhiAccrualDetector {
    /// Recent inter-arrival intervals (milliseconds). Oldest at front,
    /// newest at back. Capped at `window_size`.
    intervals: VecDeque<f64>,
    /// Timestamp (ms) of the most recent observed heartbeat, if any.
    last_heartbeat_ms: Option<u64>,
    /// Maximum number of intervals retained.
    window_size: usize,
    /// Minimum interval count before phi is computed; below this
    /// threshold `phi()` returns `0.0` (conservative cold-start —
    /// never suspect a peer until we have enough samples to estimate
    /// its distribution). Callers fall back to a fixed-timeout policy
    /// while cold.
    min_samples: usize,
    /// Floor on the estimated standard deviation (ms).
    min_std_dev_ms: f64,
}

impl PhiAccrualDetector {
    /// Build a detector with the given window size and minimum sample count.
    ///
    /// `window_size` must be ≥ 1. `min_samples` is clamped to
    /// `[1, window_size]`.
    #[must_use]
    pub fn new(window_size: usize, min_samples: usize) -> Self {
        let window_size = window_size.max(1);
        let min_samples = min_samples.clamp(1, window_size);
        Self {
            intervals: VecDeque::with_capacity(window_size),
            last_heartbeat_ms: None,
            window_size,
            min_samples,
            min_std_dev_ms: DEFAULT_MIN_STD_DEV_MS,
        }
    }

    /// Override the minimum stddev floor (primarily for tests).
    #[must_use]
    pub fn with_min_std_dev_ms(mut self, min_std_dev_ms: f64) -> Self {
        self.min_std_dev_ms = min_std_dev_ms.max(0.0);
        self
    }

    /// Number of intervals currently in the sliding window.
    #[must_use]
    pub fn sample_count(&self) -> usize {
        self.intervals.len()
    }

    /// Whether the detector has enough samples to produce a non-zero phi.
    #[must_use]
    pub fn is_warm(&self) -> bool {
        self.intervals.len() >= self.min_samples
    }

    /// Timestamp of the last recorded heartbeat, if any.
    #[must_use]
    pub fn last_heartbeat_ms(&self) -> Option<u64> {
        self.last_heartbeat_ms
    }

    /// Record a heartbeat arrival at the given monotonic timestamp.
    ///
    /// Pushes the interval `now_ms - last_heartbeat_ms` into the sliding
    /// window (evicting the oldest sample if the window is full) and
    /// advances `last_heartbeat_ms`.
    ///
    /// Out-of-order or duplicate observations (`now_ms <= last_heartbeat_ms`)
    /// are silently dropped — only the timestamp is updated forward when
    /// `now_ms > last_heartbeat_ms`.
    pub fn heartbeat(&mut self, now_ms: u64) {
        if let Some(prev) = self.last_heartbeat_ms {
            if now_ms > prev {
                #[expect(
                    clippy::cast_precision_loss,
                    reason = "heartbeat intervals fit easily in f64 mantissa for practical timescales"
                )]
                let interval = (now_ms - prev) as f64;
                self.push_interval(interval);
            }
        }
        // Always advance the "last seen" anchor, even on the first sample
        // (no interval to push yet) and on equal timestamps.
        self.last_heartbeat_ms = Some(match self.last_heartbeat_ms {
            Some(prev) => prev.max(now_ms),
            None => now_ms,
        });
    }

    /// Record a heartbeat *anchor* without contributing the gap as a
    /// sample interval.
    ///
    /// Use this on recovery from a SUSPECT state: the recovery gap is
    /// anomalous (it includes whatever wall-clock period the peer was
    /// silent), and folding it into the distribution would teach the
    /// detector that multi-second silences are normal — slowing all
    /// future detections. We still advance `last_heartbeat_ms` so the
    /// next genuine interval is measured correctly.
    pub fn heartbeat_anchor_only(&mut self, now_ms: u64) {
        self.last_heartbeat_ms = Some(match self.last_heartbeat_ms {
            Some(prev) => prev.max(now_ms),
            None => now_ms,
        });
    }

    fn push_interval(&mut self, interval_ms: f64) {
        if self.intervals.len() == self.window_size {
            self.intervals.pop_front();
        }
        self.intervals.push_back(interval_ms);
    }

    /// Mean of the sample window. Returns `0.0` on an empty window.
    #[must_use]
    pub fn mean(&self) -> f64 {
        if self.intervals.is_empty() {
            return 0.0;
        }
        let sum: f64 = self.intervals.iter().sum();
        #[expect(
            clippy::cast_precision_loss,
            reason = "window size is bounded (<= PHI_WINDOW_SIZE) so no precision is lost"
        )]
        let n = self.intervals.len() as f64;
        sum / n
    }

    /// Population standard deviation of the sample window, with a floor
    /// at `min_std_dev_ms`. Returns the floor on an empty window.
    #[must_use]
    pub fn std_dev(&self) -> f64 {
        if self.intervals.is_empty() {
            return self.min_std_dev_ms;
        }
        let mean = self.mean();
        #[expect(
            clippy::cast_precision_loss,
            reason = "window size is bounded (<= PHI_WINDOW_SIZE) so no precision is lost"
        )]
        let n = self.intervals.len() as f64;
        let var: f64 = self
            .intervals
            .iter()
            .map(|x| {
                let d = x - mean;
                d * d
            })
            .sum::<f64>()
            / n;
        var.sqrt().max(self.min_std_dev_ms)
    }

    /// Compute `φ(now_ms)`.
    ///
    /// Returns `0.0` (i.e. "no suspicion") in any of the following
    /// fail-closed cases (Hew tenet 1: never spuriously declare a peer
    /// suspect):
    ///
    /// - the detector has fewer than `min_samples` intervals (cold start),
    /// - no heartbeat has ever been observed,
    /// - `now_ms <= last_heartbeat_ms` (clock skew / out-of-order tick).
    #[must_use]
    pub fn phi(&self, now_ms: u64) -> f64 {
        if !self.is_warm() {
            return 0.0;
        }
        let Some(last) = self.last_heartbeat_ms else {
            return 0.0;
        };
        if now_ms <= last {
            return 0.0;
        }
        #[expect(
            clippy::cast_precision_loss,
            reason = "silence durations fit easily in f64 mantissa for practical timescales"
        )]
        let t = (now_ms - last) as f64;
        let mean = self.mean();
        let std_dev = self.std_dev();
        let p = p_later(t, mean, std_dev);
        // Clamp the survival probability away from zero to avoid log10(0)
        // returning -inf when t is extremely far from the mean.
        let p_clamped = p.max(f64::MIN_POSITIVE);
        -p_clamped.log10()
    }
}

/// Akka-style approximation of `P(X > t)` where `X ~ Normal(mean, std_dev)`.
///
/// Equivalent to `1 - Φ((t - μ) / σ)`. The Hayashibara paper assumes a
/// normal distribution; this closed-form approximation avoids needing
/// `erf` (which `std` does not provide) while remaining within `~10⁻⁴`
/// of the true survival function over the range of interest.
fn p_later(t: f64, mean: f64, std_dev: f64) -> f64 {
    // `std_dev` is floored by the detector before getting here, but be
    // defensive in case this is called directly with a degenerate input.
    if std_dev <= 0.0 {
        return if t > mean { 0.0 } else { 1.0 };
    }
    let y = (t - mean) / std_dev;
    let e = (-y * (1.5976 + 0.070_566 * y * y)).exp();
    if t > mean {
        e / (1.0 + e)
    } else {
        1.0 - 1.0 / (1.0 + e)
    }
}

#[cfg(test)]
#[allow(
    clippy::float_cmp,
    clippy::cast_sign_loss,
    reason = "tests assert exact sentinel return values and use signed jitter arithmetic"
)]
mod tests {
    use super::*;

    fn approx_eq(a: f64, b: f64, eps: f64) -> bool {
        (a - b).abs() < eps
    }

    #[test]
    fn cold_start_returns_zero_phi() {
        let det = PhiAccrualDetector::new(200, 10);
        assert_eq!(det.phi(1_000_000), 0.0);
        assert!(!det.is_warm());
        assert_eq!(det.sample_count(), 0);
    }

    #[test]
    fn under_min_samples_returns_zero_phi() {
        let mut det = PhiAccrualDetector::new(200, 10).with_min_std_dev_ms(0.0);
        // 5 heartbeats → 4 intervals, below min_samples=10.
        for i in 0..5 {
            det.heartbeat(100 * (i + 1));
        }
        assert_eq!(det.sample_count(), 4);
        assert!(!det.is_warm());
        // Even a long silence yields zero phi while cold.
        assert_eq!(det.phi(100_000), 0.0);
    }

    #[test]
    fn known_input_mean_and_stddev() {
        // Intervals: 100, 110, 90, 100, 100  → mean 100, var = 40, σ ≈ 6.3246.
        let mut det = PhiAccrualDetector::new(200, 5).with_min_std_dev_ms(0.0);
        let arrivals = [0u64, 100, 210, 300, 400, 500];
        for t in arrivals {
            det.heartbeat(t);
        }
        assert_eq!(det.sample_count(), 5);
        assert!(approx_eq(det.mean(), 100.0, 1e-9));
        assert!(approx_eq(det.std_dev(), 40f64.sqrt(), 1e-9));
    }

    #[test]
    fn known_input_phi_matches_handcalc() {
        // Same intervals as above; mean=100, σ=√40≈6.3246.
        // Query at now = 500 + 150 = 650 → t = 150, y = 50/σ ≈ 7.906.
        // p_later ≈ e / (1+e) where e = exp(-y·(1.5976 + 0.070566·y²)).
        let mut det = PhiAccrualDetector::new(200, 5).with_min_std_dev_ms(0.0);
        for t in [0u64, 100, 210, 300, 400, 500] {
            det.heartbeat(t);
        }
        let phi = det.phi(650);

        // Hand-computed expectation: with σ≈6.3246, t=150,
        // y ≈ 7.9057, y² ≈ 62.5,
        // exponent ≈ -7.9057 · (1.5976 + 0.070566 · 62.5)
        //        ≈ -7.9057 · (1.5976 + 4.4104)
        //        ≈ -7.9057 · 6.0080  ≈ -47.491
        // e ≈ exp(-47.491) ≈ 2.62e-21
        // p_later ≈ e (e<<1, denominator ≈ 1), so phi ≈ -log10(2.62e-21) ≈ 20.6.
        // Allow generous tolerance for the approximation.
        assert!(
            phi > 18.0 && phi < 23.0,
            "phi should be ~20.6 for a 150ms silence vs μ=100, σ≈6.32; got {phi}"
        );
    }

    #[test]
    fn phi_low_under_normal_cadence() {
        // Train on ~100ms intervals with realistic jitter, then query just
        // after the next expected heartbeat. Phi should be small — the
        // network is behaving as learned.
        let mut det = PhiAccrualDetector::new(200, 10).with_min_std_dev_ms(0.0);
        // Jitter pattern with σ ≈ 10ms so a 10ms early/late query
        // is ~1σ off-mean, not deep in the tail.
        let jitters = [0i64, 15, -10, 5, -5, 12, -8, 3, -12, 8];
        let mut t: i64 = 0;
        for i in 0..60 {
            t += 100 + jitters[i % jitters.len()];
            det.heartbeat(t as u64);
        }
        let last = det.last_heartbeat_ms().unwrap();
        // 100ms after the last heartbeat is right at the mean.
        let phi_at_mean = det.phi(last + 100);
        assert!(
            phi_at_mean < 1.0,
            "phi should stay below 1.0 at the mean interval; got {phi_at_mean}"
        );
    }

    #[test]
    fn phi_high_on_stalled_heartbeat() {
        // Train on ~100ms intervals; then a 10-second silence should
        // produce a very high phi.
        let mut det = PhiAccrualDetector::new(200, 10);
        for i in 0..50u64 {
            let jitter = if i % 2 == 0 { 0 } else { 5 };
            det.heartbeat(i * 100 + jitter);
        }
        let last = det.last_heartbeat_ms().unwrap();
        let phi = det.phi(last + 10_000);
        assert!(
            phi > 8.0,
            "phi should exceed the SUSPECT threshold (8.0) after a 10s stall; got {phi}"
        );
    }

    #[test]
    fn sliding_window_evicts_oldest() {
        let mut det = PhiAccrualDetector::new(3, 1).with_min_std_dev_ms(0.0);
        // 4 intervals into a window of size 3.
        // Heartbeats at t = 0, 100, 300, 600, 1000 → intervals 100, 200, 300, 400.
        // After eviction, window should contain {200, 300, 400}.
        for t in [0u64, 100, 300, 600, 1000] {
            det.heartbeat(t);
        }
        assert_eq!(det.sample_count(), 3);
        assert!(approx_eq(det.mean(), 300.0, 1e-9));
        let var = ((200.0 - 300.0_f64).powi(2)
            + (300.0 - 300.0_f64).powi(2)
            + (400.0 - 300.0_f64).powi(2))
            / 3.0;
        assert!(approx_eq(det.std_dev(), var.sqrt(), 1e-9));
    }

    #[test]
    fn heartbeat_anchor_only_does_not_train_detector() {
        let mut det = PhiAccrualDetector::new(200, 1).with_min_std_dev_ms(0.0);
        det.heartbeat(0);
        det.heartbeat(100);
        let before = det.sample_count();
        // Recovery after a 5s silence — should NOT pollute the window.
        det.heartbeat_anchor_only(5_100);
        assert_eq!(det.sample_count(), before);
        assert_eq!(det.last_heartbeat_ms(), Some(5_100));

        // The next real interval is measured from the anchor.
        det.heartbeat(5_200);
        assert_eq!(det.sample_count(), before + 1);
        assert!(approx_eq(*det.intervals.back().unwrap(), 100.0, 1e-9));
    }

    #[test]
    fn out_of_order_heartbeat_ignored() {
        let mut det = PhiAccrualDetector::new(200, 1).with_min_std_dev_ms(0.0);
        det.heartbeat(1000);
        det.heartbeat(2000);
        let count = det.sample_count();
        let last = det.last_heartbeat_ms();
        det.heartbeat(1500); // out of order
        assert_eq!(det.sample_count(), count);
        assert_eq!(det.last_heartbeat_ms(), last);
    }

    #[test]
    fn min_std_dev_floor_prevents_degenerate_phi() {
        // Perfectly regular intervals → raw σ = 0. With the floor in
        // place, std_dev should clamp to the floor, not 0.
        let mut det = PhiAccrualDetector::new(20, 5).with_min_std_dev_ms(50.0);
        for i in 0..10u64 {
            det.heartbeat(i * 100);
        }
        assert!(approx_eq(det.mean(), 100.0, 1e-9));
        assert!(approx_eq(det.std_dev(), 50.0, 1e-9));
        // Phi must be finite — not NaN or inf.
        let phi = det.phi(det.last_heartbeat_ms().unwrap() + 1_000);
        assert!(phi.is_finite());
        assert!(phi > 0.0);
    }

    #[test]
    fn phi_zero_when_time_not_advanced() {
        let mut det = PhiAccrualDetector::new(200, 5).with_min_std_dev_ms(0.0);
        for t in [0u64, 100, 200, 300, 400, 500] {
            det.heartbeat(t);
        }
        // now == last_heartbeat → no silence yet.
        assert_eq!(det.phi(500), 0.0);
        // now < last_heartbeat (clock skew) → fail-closed.
        assert_eq!(det.phi(400), 0.0);
    }
}
