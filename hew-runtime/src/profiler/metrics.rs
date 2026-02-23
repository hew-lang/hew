//! Metrics snapshot and time-series ring buffer.
//!
//! The profiler's sampler thread captures a [`MetricsSnapshot`] every second
//! and pushes it into a fixed-size ring buffer. The HTTP API reads the buffer
//! to serve time-series data for the dashboard charts.

use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::time::Instant;

use crate::profiler::allocator;
use crate::scheduler;

/// Number of snapshots retained (5 minutes at 1 s interval).
const RING_SIZE: usize = 300;

// ── Snapshot ────────────────────────────────────────────────────────────

/// A point-in-time capture of all runtime counters.
#[derive(Debug, Clone, Copy, Default)]
pub struct MetricsSnapshot {
    /// Monotonic timestamp in seconds since the ring buffer was created.
    pub timestamp_secs: u64,

    // Scheduler counters.
    pub tasks_spawned: u64,
    pub tasks_completed: u64,
    pub steals: u64,
    pub messages_sent: u64,
    pub messages_received: u64,
    pub active_workers: u64,

    // Allocator counters.
    pub alloc_count: u64,
    pub dealloc_count: u64,
    pub bytes_allocated: u64,
    pub bytes_freed: u64,
    pub bytes_live: u64,
    pub peak_bytes_live: u64,
}

impl MetricsSnapshot {
    /// Capture a snapshot of all runtime metrics right now.
    pub fn capture(epoch: Instant) -> Self {
        let alloc = allocator::snapshot();
        Self {
            timestamp_secs: epoch.elapsed().as_secs(),
            tasks_spawned: scheduler::TASKS_SPAWNED.load(Ordering::Relaxed),
            tasks_completed: scheduler::TASKS_COMPLETED.load(Ordering::Relaxed),
            steals: scheduler::STEALS_TOTAL.load(Ordering::Relaxed),
            messages_sent: scheduler::MESSAGES_SENT.load(Ordering::Relaxed),
            messages_received: scheduler::MESSAGES_RECEIVED.load(Ordering::Relaxed),
            active_workers: scheduler::ACTIVE_WORKERS.load(Ordering::Relaxed),
            alloc_count: alloc.alloc_count,
            dealloc_count: alloc.dealloc_count,
            bytes_allocated: alloc.bytes_allocated,
            bytes_freed: alloc.bytes_freed,
            bytes_live: alloc.bytes_live,
            peak_bytes_live: alloc.peak_bytes_live,
        }
    }
}

// ── Ring buffer ─────────────────────────────────────────────────────────

/// Fixed-size ring buffer of [`MetricsSnapshot`]s.
///
/// Single-writer (sampler thread), multi-reader (HTTP handlers).
/// Readers may see partially-written snapshots (torn reads), which is
/// acceptable for dashboard display.
#[derive(Debug)]
pub struct MetricsRing {
    buf: Box<[MetricsSnapshot; RING_SIZE]>,
    /// Index of the next slot to write (monotonically increasing).
    write_idx: AtomicUsize,
    /// How many entries have been written in total.
    total_written: AtomicU64,
    /// Instant when the ring buffer was created (epoch for timestamps).
    epoch: Instant,
}

impl MetricsRing {
    /// Create a new ring buffer.
    ///
    /// # Panics
    ///
    /// Panics if the boxed slice length doesn't match `RING_SIZE` (should
    /// never happen since both are the same constant).
    #[must_use]
    pub fn new() -> Self {
        Self {
            buf: vec![MetricsSnapshot::default(); RING_SIZE]
                .into_boxed_slice()
                .try_into()
                .expect("vec length matches RING_SIZE"),
            write_idx: AtomicUsize::new(0),
            total_written: AtomicU64::new(0),
            epoch: Instant::now(),
        }
    }

    /// Push a new snapshot (called by the sampler thread).
    pub fn push(&mut self, snap: MetricsSnapshot) {
        let idx = self.write_idx.load(Ordering::Relaxed);
        self.buf[idx] = snap;
        self.write_idx
            .store((idx + 1) % RING_SIZE, Ordering::Release);
        self.total_written.fetch_add(1, Ordering::Relaxed);
    }

    /// Capture and push a snapshot right now.
    pub fn sample(&mut self) {
        let snap = MetricsSnapshot::capture(self.epoch);
        self.push(snap);
    }

    /// Read all valid entries, oldest first.
    ///
    /// Returns up to `RING_SIZE` entries. The returned slice is ordered
    /// by ascending timestamp.
    pub fn read_all(&self) -> Vec<MetricsSnapshot> {
        let total = self.total_written.load(Ordering::Relaxed);
        if total == 0 {
            return Vec::new();
        }

        #[expect(clippy::cast_possible_truncation, reason = "RING_SIZE fits in usize")]
        let count = (total as usize).min(RING_SIZE);
        let write_pos = self.write_idx.load(Ordering::Acquire);

        let mut result = Vec::with_capacity(count);
        for i in 0..count {
            // Oldest entry is at write_pos (it was just overwritten most
            // recently if the buffer is full).
            let idx = (write_pos + RING_SIZE - count + i) % RING_SIZE;
            result.push(self.buf[idx]);
        }
        result
    }

    /// Return the epoch instant for this ring.
    pub fn epoch(&self) -> Instant {
        self.epoch
    }
}

impl Default for MetricsRing {
    fn default() -> Self {
        Self::new()
    }
}
