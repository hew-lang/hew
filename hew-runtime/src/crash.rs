//! Actor crash recording and per-actor crash statistics.
//!
//! An actor crash is recorded from its [`FailureRecord`]: the record raised
//! where the actor failed, completed with the actor's identity. The record is
//! printed once, kept in a ring of the 64 most recent crashes (`/api/crashes`)
//! and counted in the labelled crash metric.
//!
//! [`CrashStats`] is the supervisor's per-child sliding window, exchanged with
//! generated code through the C ABI below.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

use crate::failure_record::{copy_name, FailureRecord};
use crate::lifetime::PoisonSafe;

// ── Per-actor crash statistics ─────────────────────────────────────────

/// Per-actor crash statistics with sliding window tracking.
///
/// Maintains a count of total crashes and a circular buffer of recent
/// crash timestamps for rate limiting and supervision decisions.
#[repr(C)]
#[derive(Debug)]
pub struct CrashStats {
    /// Total number of crashes for this actor over its lifetime.
    pub total_crashes: u32,
    /// Circular buffer of recent crash timestamps (monotonic nanoseconds).
    pub recent_timestamps: [u64; 8],
    /// Head index into the circular buffer (next write position).
    pub recent_head: u32,
    /// Signal number from the most recent crash.
    pub last_signal: i32,
}

impl CrashStats {
    /// Create new crash statistics with all fields zeroed.
    const fn new() -> Self {
        Self {
            total_crashes: 0,
            recent_timestamps: [0; 8],
            recent_head: 0,
            last_signal: 0,
        }
    }

    /// Record a new crash, updating statistics.
    fn record_crash(&mut self, signal: i32, timestamp_ns: u64) {
        self.total_crashes = self.total_crashes.saturating_add(1);
        self.last_signal = signal;

        // Add to circular buffer
        let idx = (self.recent_head as usize) % 8;
        self.recent_timestamps[idx] = timestamp_ns;
        self.recent_head = self.recent_head.wrapping_add(1);
    }

    /// Count crashes within a time window (in nanoseconds).
    fn recent_crash_count(&self, window_ns: u64, now_ns: u64) -> u32 {
        let cutoff_time = now_ns.saturating_sub(window_ns);
        let valid_entries = (self.total_crashes as usize).min(self.recent_timestamps.len());
        let mut count = 0;

        // Only examine slots that have been written to. The circular buffer
        // is zero-initialized, and zero timestamps would be spuriously
        // counted when the monotonic epoch is younger than the window.
        for i in 0..valid_entries {
            // Walk backwards from the most recent entry.
            let idx = (self.recent_head as usize + self.recent_timestamps.len() - 1 - i)
                % self.recent_timestamps.len();
            let timestamp = self.recent_timestamps[idx];
            if timestamp >= cutoff_time && timestamp <= now_ns {
                count += 1;
            }
        }

        count
    }
}

// ── Crash ring ──────────────────────────────────────────────────────────

/// The most recent crashes, oldest first.
static RECENT_CRASHES: PoisonSafe<VecDeque<StoredCrash>> = PoisonSafe::new(VecDeque::new());

/// A crash-ring entry with its own source identity.
#[derive(Clone, Debug)]
pub struct StoredCrash {
    record: FailureRecord,
    // Shared allocations keep the pointers in every cloned record valid.
    _site_file: Option<std::sync::Arc<std::ffi::CString>>,
    _frame_names: [Option<std::sync::Arc<std::ffi::CString>>; crate::failure_record::MAX_FRAMES],
    _frame_files: [Option<std::sync::Arc<std::ffi::CString>>; crate::failure_record::MAX_FRAMES],
}

impl std::ops::Deref for StoredCrash {
    type Target = FailureRecord;

    fn deref(&self) -> &Self::Target {
        &self.record
    }
}

impl StoredCrash {
    /// The source file retained by this crash entry, if available.
    #[must_use]
    pub fn site_file(&self) -> Option<&str> {
        self.record.site_file()
    }
}

/// Maximum number of crashes to keep in the ring.
const MAX_CRASH_LOG_SIZE: usize = 64;

/// Actor crashes no supervisor recovered: unsupervised crashes plus children a
/// supervisor gave up on.
static UNRECOVERED_CRASHES: AtomicU64 = AtomicU64::new(0);

fn push_crash_to(log: &PoisonSafe<VecDeque<StoredCrash>>, record: &FailureRecord) {
    fn owned_text(source: *const std::ffi::c_char) -> Option<std::sync::Arc<std::ffi::CString>> {
        if source.is_null() {
            return None;
        }
        // SAFETY: the caller retains the failure and its source image while
        // the crash entry is made.
        let source = unsafe { std::ffi::CStr::from_ptr(source) };
        if source.to_bytes_with_nul().len() > 4096 {
            return None;
        }
        Some(std::sync::Arc::new(source.to_owned()))
    }
    let site_file = owned_text(record.site.file);
    let mut copy = *record;
    copy.site.file = site_file.as_ref().map_or(std::ptr::null(), |s| s.as_ptr());
    let mut names = std::array::from_fn(|_| None);
    let mut files = std::array::from_fn(|_| None);
    for (index, frame) in record.frames().iter().enumerate() {
        names[index] = owned_text(frame.hew_fn);
        files[index] = owned_text(frame.file);
        copy.frames[index].hew_fn = names[index]
            .as_ref()
            .map_or(std::ptr::null(), |s| s.as_ptr());
        copy.frames[index].file = files[index]
            .as_ref()
            .map_or(std::ptr::null(), |s| s.as_ptr());
    }
    log.access(|crashes| {
        while crashes.len() >= MAX_CRASH_LOG_SIZE {
            crashes.pop_front();
        }
        crashes.push_back(StoredCrash {
            record: copy,
            _site_file: site_file,
            _frame_names: names,
            _frame_files: files,
        });
    });
}

/// Fill the actor half of `record` from the running turn: identity, type,
/// handler, supervisor role, worker and the incarnation it replaced.
pub(crate) fn describe_current_actor(record: &mut FailureRecord) {
    let context = crate::execution_context::current_context();
    if context.is_null() {
        return;
    }
    // SAFETY: a non-null canonical context is the live activation frame.
    let (actor, supervisor, child_index) = unsafe {
        (
            (*context).actor,
            (*context).parent_supervisor,
            (*context).supervisor_child_index,
        )
    };
    if actor.is_null() {
        return;
    }
    // SAFETY: the context's actor is live for the current turn.
    let (id, serial, dispatch) = unsafe {
        (
            (*actor).id,
            (*actor).spawn_serial,
            (*actor).dispatch.map_or(0, |dispatch| dispatch as usize),
        )
    };
    let msg_type = crate::observe::current_msg_type();
    describe_actor(record, id, msg_type, dispatch);
    record.actor.spawn_serial = serial;
    record.worker_id = crate::observe::current_worker_id();
    #[cfg(not(target_arch = "wasm32"))]
    if !supervisor.is_null() {
        copy_name(
            &mut record.supervisor,
            &crate::supervisor::role_path(supervisor.cast(), child_index),
        );
        note_incarnation(record, supervisor as usize, child_index);
    }
    #[cfg(target_arch = "wasm32")]
    let _ = (supervisor, child_index);
}

/// Fill the actor identity of `record` from its id and dispatch registration.
fn describe_actor(record: &mut FailureRecord, id: u64, msg_type: i32, dispatch: usize) {
    use crate::profiler::actor_registry;

    record.actor.id = id;
    record.actor.msg_type = msg_type;
    copy_name(
        &mut record.actor.actor_type,
        &actor_registry::lookup_dispatch_type_by_ptr_owned(dispatch),
    );
    if let Some(handler) = actor_registry::handler_name_by_ptr(dispatch, msg_type) {
        copy_name(&mut record.actor.handler, &handler);
    }
}

/// The last incarnation seen failing in each supervisor child slot.
/// `(supervisor, child index)` to `(actor id, spawn serial)`.
#[cfg(not(target_arch = "wasm32"))]
type Incarnations = Vec<((usize, i32), (u64, u64))>;
#[cfg(not(target_arch = "wasm32"))]
static INCARNATIONS: PoisonSafe<Incarnations> = PoisonSafe::new(Vec::new());

/// Link a failing actor to the incarnation its supervisor slot held before,
/// then remember it as that slot's latest.
#[cfg(not(target_arch = "wasm32"))]
fn note_incarnation(record: &mut FailureRecord, supervisor: usize, child_index: i32) {
    let slot = (supervisor, child_index);
    let current = (record.actor.id, record.actor.spawn_serial);
    INCARNATIONS.access(|incarnations| {
        match incarnations.iter_mut().find(|(key, _)| *key == slot) {
            Some((_, previous)) => {
                if previous.0 != current.0 {
                    record.actor.previous_id = previous.0;
                    record.actor.previous_spawn_serial = previous.1;
                    *previous = current;
                }
            }
            None => incarnations.push((slot, current)),
        }
    });
}

/// Record a logical actor failure caught by the scheduler's unwind boundary.
///
/// The failure was raised at its origin on this thread; its record is taken
/// from there and printed now unless the fault report already printed it.
/// A crash with no raised failure (a bare trap code) is described here.
/// `dispatch_ptr` is the crashing actor's dispatch function, the key of the
/// type and handler registries.
pub(crate) fn record_logical_crash(
    actor_id: u64,
    code: i32,
    msg_type: i32,
    dispatch_ptr: usize,
    link_target: Option<crate::monitor::HewNotificationTarget>,
) {
    let (mut record, printed, _owner) = crate::fault::take_crash_cause(code).unwrap_or_else(|| {
        let mut record = FailureRecord::new(code);
        let _ = crate::monotonic::monotonic_ns();
        record.wall_time_ns = crate::failure_record::wall_time_ns();
        record.build = crate::failure_record::build_info();
        crate::failure_record::publish_last_fault(&record);
        (record, false, None)
    });
    // The scheduler's identity of the failed turn is authoritative; the
    // origin may have run outside the turn's bookkeeping (a resumed frame).
    if record.actor.id != actor_id || record.actor.msg_type != msg_type {
        describe_actor(&mut record, actor_id, msg_type, dispatch_ptr);
    }
    if !printed {
        let mut text = vec![0u8; 16 * 1024];
        let len = crate::failure_record::format_into(&record, &mut text);
        text.truncate(len);
        text.extend_from_slice(remote_origin(link_target).as_bytes());
        crate::output::write(crate::output::Stream::Err, &text);
    }
    if record.supervisor[0] == 0 {
        UNRECOVERED_CRASHES.fetch_add(1, Ordering::Relaxed);
    }
    crate::observe::record_actor_crash(
        record.kind_name(),
        crate::failure_record::padded_str(&record.actor.actor_type),
    );
    push_crash_to(&RECENT_CRASHES, &record);
}

#[cfg(not(target_arch = "wasm32"))]
fn remote_origin(link_target: Option<crate::monitor::HewNotificationTarget>) -> String {
    link_target
        .and_then(crate::monitor::HewNotificationTarget::remote_location)
        .map_or_else(String::new, |location| {
            format!("hew:   while handling link exit from remote {location}\n")
        })
}

#[cfg(target_arch = "wasm32")]
fn remote_origin(_link_target: Option<crate::monitor::HewNotificationTarget>) -> String {
    String::new()
}

/// Record a fault-injected crash. Deliberately silent: a deterministic
/// simulation hook is the harness asking for a crash, not a program fault the
/// user has to diagnose, and the injecting frame has already released the
/// actor it would name.
pub(crate) fn record_injected_crash(actor_id: u64) {
    let mut record = FailureRecord::new(0);
    record.set_message("crash injected by the deterministic driver");
    record.actor.id = actor_id;
    record.wall_time_ns = crate::failure_record::wall_time_ns();
    crate::observe::record_actor_crash(record.kind_name(), "");
    push_crash_to(&RECENT_CRASHES, &record);
}

/// A supervisor gave up on a child: its crash is now unrecovered.
pub(crate) fn note_supervisor_giveup() {
    UNRECOVERED_CRASHES.fetch_add(1, Ordering::Relaxed);
}

/// Say why the process exits with status 1, once, when unrecovered actor
/// crashes decided it.
pub(crate) fn report_unrecovered_exit() {
    static REPORTED: AtomicBool = AtomicBool::new(false);
    let count = UNRECOVERED_CRASHES.load(Ordering::Relaxed);
    if count == 0 || REPORTED.swap(true, Ordering::AcqRel) {
        return;
    }
    let noun = if count == 1 { "crash" } else { "crashes" };
    crate::output::write(
        crate::output::Stream::Err,
        format!("hew: exiting 1: {count} unrecovered actor {noun}\n").as_bytes(),
    );
}

/// The most recent crash, if any.
#[must_use]
pub fn last_crash() -> Option<StoredCrash> {
    RECENT_CRASHES.access(|crashes| crashes.back().cloned())
}

/// Every crash in the ring, oldest first.
#[must_use]
pub fn recent_crashes() -> Vec<StoredCrash> {
    RECENT_CRASHES.access(|crashes| crashes.iter().cloned().collect())
}

// ── C ABI functions ─────────────────────────────────────────────────────

/// Allocate a new crash statistics structure.
///
/// Returns a pointer to a heap-allocated [`CrashStats`] initialized to zero.
/// The caller must free it with [`hew_crash_stats_free`].
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_crash_stats_new() -> *mut CrashStats {
    let stats = Box::new(CrashStats::new());
    Box::into_raw(stats) // ALLOCATOR-PAIRING: GlobalAlloc
}

/// Record a crash in the statistics, updating counters and timestamps.
///
/// # Safety
///
/// `stats` must be a valid pointer returned by [`hew_crash_stats_new`]
/// and must not be used concurrently from multiple threads.
#[no_mangle]
pub unsafe extern "C" fn hew_crash_stats_record(
    stats: *mut CrashStats,
    signal: i32,
    timestamp_ns: u64,
) {
    if stats.is_null() {
        return;
    }

    // SAFETY: Caller guarantees stats is valid and non-null.
    let stats_ref = unsafe { &mut *stats };
    stats_ref.record_crash(signal, timestamp_ns);
}

/// Count crashes within a time window ending at `now_ns`.
///
/// Returns the number of recorded crashes whose timestamp falls in
/// `[now_ns - window_ns, now_ns]`. `now_ns` MUST come from the **same monotonic
/// clock** that produced the timestamps passed to [`hew_crash_stats_record`]:
/// counting against a second epoch reads a fresh timestamp as "in the future"
/// and undercounts, leaving the circuit breaker closed when it should open.
///
/// # Safety
///
/// `stats` must be a valid pointer returned by [`hew_crash_stats_new`]
/// and must not be used concurrently from multiple threads.
#[no_mangle]
pub unsafe extern "C" fn hew_crash_stats_recent_count(
    stats: *mut CrashStats,
    window_ns: u64,
    now_ns: u64,
) -> u32 {
    if stats.is_null() {
        return 0;
    }

    // SAFETY: Caller guarantees stats is valid and non-null.
    let stats_ref = unsafe { &*stats };
    stats_ref.recent_crash_count(window_ns, now_ns)
}

/// Free a crash statistics structure.
///
/// # Safety
///
/// `stats` must be a valid pointer returned by [`hew_crash_stats_new`]
/// and must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_crash_stats_free(stats: *mut CrashStats) {
    if stats.is_null() {
        return;
    }

    // SAFETY: Caller guarantees stats was created by hew_crash_stats_new.
    drop(unsafe { Box::from_raw(stats) }); // ALLOCATOR-PAIRING: GlobalAlloc
}

/// The number of crashes in the ring.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_crash_log_count() -> i32 {
    crash_count_of(&RECENT_CRASHES)
}

fn crash_count_of(log: &PoisonSafe<VecDeque<StoredCrash>>) -> i32 {
    log.access(|crashes| i32::try_from(crashes.len()).expect("the ring holds 64 entries"))
}

/// The `/api/crashes` body: the ring, newest first.
#[cfg(feature = "profiler")]
#[must_use]
pub fn snapshot_crashes_json() -> String {
    // Seconds since process start, as the observe timeline shows them.
    let now_wall = crate::failure_record::wall_time_ns();
    let start_wall = now_wall.saturating_sub(crate::monotonic::monotonic_ns());
    let crashes: Vec<serde_json::Value> = recent_crashes()
        .iter()
        .rev()
        .map(|crash| {
            #[allow(
                clippy::cast_precision_loss,
                reason = "nanosecond precision loss is acceptable for display"
            )]
            let time_s = crash.wall_time_ns.saturating_sub(start_wall) as f64 / 1_000_000_000.0;
            serde_json::json!({
                "time_s": time_s,
                "actor_id": crash.actor.id.to_string(),
                "signal": crash.code,
                "trap_kind": crash.kind_name(),
                "msg_type": crash.actor.msg_type,
                "fault_addr": crash.signal.fault_addr.to_string(),
                "actor_type": crate::failure_record::padded_str(&crash.actor.actor_type),
                "handler": crate::failure_record::padded_str(&crash.actor.handler),
                "spawn_serial": crash.actor.spawn_serial.to_string(),
                "supervisor": crate::failure_record::padded_str(&crash.supervisor),
                "message": String::from_utf8_lossy(crash.message_bytes()),
                "file": crash.site_file(),
                "line": crash.site.line,
                "col": crash.site.col,
            })
        })
        .collect();
    serde_json::Value::Array(crashes).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn crash_stats_new_initializes_zero() {
        // SAFETY: No preconditions for this test.
        let stats = unsafe { hew_crash_stats_new() };
        assert!(!stats.is_null());

        // SAFETY: stats was just created and is valid.
        unsafe {
            assert_eq!((*stats).total_crashes, 0);
            assert_eq!((*stats).recent_head, 0);
            assert_eq!((*stats).last_signal, 0);
            hew_crash_stats_free(stats);
        }
    }

    #[test]
    fn crash_stats_record_updates_counters() {
        // SAFETY: No preconditions for this test.
        let stats = unsafe { hew_crash_stats_new() };

        // SAFETY: stats is valid and we're single-threaded in test.
        unsafe {
            hew_crash_stats_record(stats, 11, 1000);

            assert_eq!((*stats).total_crashes, 1);
            assert_eq!((*stats).last_signal, 11);
            assert_eq!((*stats).recent_timestamps[0], 1000);
            assert_eq!((*stats).recent_head, 1);

            hew_crash_stats_free(stats);
        }
    }

    #[test]
    fn crash_stats_circular_buffer_wraps() {
        // SAFETY: No preconditions for this test.
        let stats = unsafe { hew_crash_stats_new() };

        // SAFETY: stats is valid and we're single-threaded in test.
        unsafe {
            for i in 0..10 {
                hew_crash_stats_record(stats, 11, 1000 + i);
            }

            assert_eq!((*stats).total_crashes, 10);
            assert_eq!((*stats).recent_head, 10);
            assert_eq!((*stats).recent_timestamps[0], 1008);
            assert_eq!((*stats).recent_timestamps[1], 1009);

            hew_crash_stats_free(stats);
        }
    }

    #[test]
    fn recent_count_uses_caller_clock_not_a_second_epoch() {
        // SAFETY: stats is valid, single-threaded test.
        let stats = unsafe { hew_crash_stats_new() };
        // SAFETY: stats is the valid pointer from hew_crash_stats_new above;
        // all record/recent_count/free calls are single-threaded in this test.
        unsafe {
            let t0: u64 = 5_000_000_000;
            let t1: u64 = t0 + 1_000;
            hew_crash_stats_record(stats, -1, t0);
            hew_crash_stats_record(stats, -1, t1);
            let window_ns: u64 = 60 * 1_000_000_000;
            assert_eq!(hew_crash_stats_recent_count(stats, window_ns, t1), 2);
            // Timestamps after a stale now_ns are excluded: the skew failure mode.
            assert_eq!(hew_crash_stats_recent_count(stats, window_ns, t0 - 1), 0);
            assert_eq!(hew_crash_stats_recent_count(stats, window_ns, t0), 1);
            hew_crash_stats_free(stats);
        }
    }

    fn crash(id: u64, code: i32) -> FailureRecord {
        let mut record = FailureRecord::new(code);
        record.actor.id = id;
        record
    }

    #[test]
    fn ring_keeps_the_newest_sixty_four() {
        let log = PoisonSafe::new(VecDeque::new());
        for id in 1..=70 {
            push_crash_to(&log, &crash(id, 202));
        }
        assert_eq!(crash_count_of(&log), 64);
        log.access(|crashes| {
            assert_eq!(crashes.front().unwrap().actor.id, 7);
            assert_eq!(crashes.back().unwrap().actor.id, 70);
        });
    }

    #[test]
    fn ring_retains_source_after_the_failure_owner_is_dropped() {
        let log = PoisonSafe::new(VecDeque::new());
        let source = std::ffi::CString::new("retired.hew").unwrap();
        let mut record = crash(1, 212);
        record.site.file = source.as_ptr();
        record.site.line = 4;
        record.push_frame(123);
        record.frames[0].hew_fn = source.as_ptr();
        push_crash_to(&log, &record);
        drop(source);
        log.access(|crashes| {
            let crash = crashes.back().unwrap();
            assert_eq!(crash.site_file(), Some("retired.hew"));
            assert_eq!(crash.site.line, 4);
            assert_eq!(
                // SAFETY: the crash entry owns its copied frame name.
                unsafe { std::ffi::CStr::from_ptr(crash.frames[0].hew_fn) }.to_bytes(),
                b"retired.hew"
            );
        });
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn ring_records_survive_poison() {
        let log: PoisonSafe<VecDeque<StoredCrash>> = PoisonSafe::new(VecDeque::new());
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            log.access(|_| panic!("intentional poison"));
        }));
        assert!(log.is_poisoned_for_test());
        push_crash_to(&log, &crash(99, 7));
        assert_eq!(crash_count_of(&log), 1);
        log.access(|crashes| assert_eq!(crashes.back().unwrap().actor.id, 99));
    }

    #[test]
    fn a_restarted_slot_links_its_successor_to_the_crashed_incarnation() {
        let slot = (0x5000_usize, 3);
        let mut first = crash(10, 202);
        first.actor.spawn_serial = 100;
        note_incarnation(&mut first, slot.0, slot.1);
        assert_eq!(first.actor.previous_id, 0);

        // The same incarnation failing again is not a restart.
        let mut again = first;
        note_incarnation(&mut again, slot.0, slot.1);
        assert_eq!(again.actor.previous_id, 0);

        let mut successor = crash(11, 202);
        successor.actor.spawn_serial = 101;
        note_incarnation(&mut successor, slot.0, slot.1);
        assert_eq!(
            (
                successor.actor.previous_id,
                successor.actor.previous_spawn_serial
            ),
            (10, 100)
        );

        // The ring still holds the predecessor's record after the restart.
        let log = PoisonSafe::new(VecDeque::new());
        push_crash_to(&log, &first);
        push_crash_to(&log, &successor);
        log.access(|crashes| {
            assert_eq!(crashes[0].actor.id, 10);
            assert_eq!(crashes[1].actor.previous_id, 10);
        });
    }
}
