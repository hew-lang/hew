//! Metrics snapshot and time-series ring buffer.
//!
//! The profiler's sampler thread captures a [`MetricsSnapshot`] every second
//! and pushes it into a fixed-size ring buffer. The HTTP API reads the buffer
//! to serve time-series data for the dashboard charts.

use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::time::Instant;

use crate::profiler::allocator;
use crate::scheduler;
use crate::transport;

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
    pub scheduler_queue_depth: u64,
    pub scheduler_runnable_actors: u64,
    pub scheduler_runnable_coroutines: u64,
    pub scheduler_parks_total: u64,
    pub scheduler_unparks_total: u64,
    pub actors_live: u64,
    pub actors_turns_total: u64,
    pub actors_turn_duration_ns_total: u64,
    pub actors_mailbox_depth_sum: u64,
    pub actors_mailbox_depth_max: u64,
    pub actors_mailbox_depth_p99: u64,
    pub actors_crashes_total: u64,
    pub actors_restarts_total: u64,
    pub coroutines_live: u64,
    pub coroutines_suspended: u64,
    pub coroutines_resumes_total: u64,
    pub coroutines_suspends_total: u64,
    pub coroutines_frame_bytes_live: u64,
    pub threads_blocking_count: u64,
    pub reactor_registrations_live: u64,
    pub reactor_ready_events_total: u64,
    pub arena_resets_total: u64,

    // Allocator counters.
    pub alloc_count: u64,
    pub dealloc_count: u64,
    pub bytes_allocated: u64,
    pub bytes_freed: u64,
    pub bytes_live: u64,
    pub peak_bytes_live: u64,

    // TCP transport counters.
    pub tcp_bytes_read: u64,
    pub tcp_bytes_written: u64,
    pub tcp_accept_count: u64,
    pub tcp_connect_count: u64,
    pub tcp_error_count: u64,
}

impl MetricsSnapshot {
    /// Capture a snapshot of all runtime metrics right now.
    pub fn capture(epoch: Instant) -> Self {
        let alloc = allocator::snapshot();
        let tcp = transport::tcp_counters_snapshot();
        let hot = crate::observe::hot_counters_snapshot();
        let coroutines = crate::observe::coroutine_snapshot();
        let hooks = crate::observe::runtime_hook_snapshot();
        let mailbox = crate::profiler::actor_registry::mailbox_aggregate();
        Self {
            timestamp_secs: epoch.elapsed().as_secs(),
            tasks_spawned: scheduler::TASKS_SPAWNED.load(Ordering::Relaxed),
            tasks_completed: scheduler::TASKS_COMPLETED.load(Ordering::Relaxed),
            steals: scheduler::STEALS_TOTAL.load(Ordering::Relaxed),
            messages_sent: scheduler::MESSAGES_SENT.load(Ordering::Relaxed),
            messages_received: scheduler::MESSAGES_RECEIVED.load(Ordering::Relaxed),
            active_workers: scheduler::ACTIVE_WORKERS.load(Ordering::Relaxed),
            scheduler_queue_depth: scheduler::scheduler_queue_depth(),
            scheduler_runnable_actors: crate::profiler::actor_registry::state_count(
                crate::internal::types::HewActorState::Runnable as i32,
            ),
            scheduler_runnable_coroutines:
                crate::profiler::actor_registry::runnable_coroutine_count(),
            scheduler_parks_total: crate::observe::scheduler_parks_total(),
            scheduler_unparks_total: crate::observe::scheduler_unparks_total(),
            actors_live: crate::profiler::actor_registry::snapshot_all().len() as u64,
            actors_turns_total: hot.actor_turns_total,
            actors_turn_duration_ns_total: hot.actor_turn_duration_ns_total,
            actors_mailbox_depth_sum: mailbox.sum_depth,
            actors_mailbox_depth_max: mailbox.max_depth,
            actors_mailbox_depth_p99: mailbox.p99_depth,
            actors_crashes_total: hooks.actors_crashes_total,
            actors_restarts_total: hooks.actors_restarts_total,
            coroutines_live: coroutines.live,
            coroutines_suspended: coroutines.suspended,
            coroutines_resumes_total: coroutines.resumes_total,
            coroutines_suspends_total: coroutines.suspends_total,
            coroutines_frame_bytes_live: coroutines.frame_bytes_live,
            threads_blocking_count: hooks.threads_blocking_count,
            reactor_registrations_live: hooks.reactor_registrations_live,
            reactor_ready_events_total: hooks.reactor_ready_events_total,
            arena_resets_total: hooks.arena_resets_total,
            alloc_count: alloc.alloc_count,
            dealloc_count: alloc.dealloc_count,
            bytes_allocated: alloc.bytes_allocated,
            bytes_freed: alloc.bytes_freed,
            bytes_live: alloc.bytes_live,
            peak_bytes_live: alloc.peak_bytes_live,
            tcp_bytes_read: tcp.bytes_read,
            tcp_bytes_written: tcp.bytes_written,
            tcp_accept_count: tcp.accept_count,
            tcp_connect_count: tcp.connect_count,
            tcp_error_count: tcp.error_count,
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
