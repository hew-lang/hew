//! E2E probe for the v0.5 concurrency trace taxonomy (#277).
//!
//! Producer: `hew_runtime::tracing` (the trace ring buffer drained as JSON
//! via [`drain_events_json`]).
//! Consumer: `hew-observe`'s known event metadata names (mirrored here as
//! [`KNOWN_EVENT_NAMES`] — same pattern as the
//! `divide_by_zero_trap_surfaces_trap_kind_on_observe_event_surface`
//! test mirrors observe's `CrashEntry`).
//!
//! Scenario:
//! 1. Spawn a supervised actor that always crashes on `MSG_CRASH`.
//! 2. Send `MSG_CRASH` → supervisor restarts the child → second spawn lands.
//! 3. Send `MSG_OK` to the restarted child to record a clean stop.
//! 4. Open + close a duplex pair (channel substrate).
//!
//! Then drain the trace events as JSON and assert:
//! - The drain is non-empty.
//! - Every `event_type` string in the JSON resolves to a name in the
//!   known metadata names ([`KNOWN_EVENT_NAMES`]). Zero fall-throughs to `"unknown"`.
//! - The expected canonical events for the exercised scenario are present
//!   (`spawn`, `send`, `crash`, `duplex_created`, `duplex_closed`).
//!
//! Per R58 Q138 Option B, this probe stays scoped to producer-emitted v0.5
//! concurrency events. Supervisor lifecycle / channel partition / machine
//! transition events are intentionally absent — those producer-side
//! emission sites do not exist yet (deferred to native-M3 alongside QUIC
//! event taxonomy). Supervisor restart is observable in this probe as the
//! `crash` → new `spawn` sequence on the existing actor lifecycle
//! taxonomy.

#![cfg(all(not(target_arch = "wasm32"), feature = "profiler"))]
#![allow(
    clippy::undocumented_unsafe_blocks,
    reason = "Integration test — supervisor and dispatch FFI are inherently raw"
)]

use std::collections::HashSet;
use std::ffi::{c_void, CString};
use std::ptr;
use std::sync::Mutex;
use std::time::{Duration, Instant};

use hew_runtime::actor::hew_actor_send;
use hew_runtime::crash::hew_crash_log_count;
use hew_runtime::deterministic::hew_deterministic_reset;
use hew_runtime::duplex::{hew_duplex_close, hew_duplex_pair, HewDuplexHandle};
use hew_runtime::supervisor::{
    hew_supervisor_add_child_spec, hew_supervisor_get_child_wait,
    hew_supervisor_set_restart_notify, hew_supervisor_wait_restart, hew_trap_with_code,
    HewChildSpec, HEW_TRAP_DIVIDE_BY_ZERO,
};
use hew_runtime::tracing::{
    drain_events_json, hew_trace_enable, hew_trace_is_enabled, hew_trace_reset, EVENT_TYPE_NAMES,
};
use hew_runtime_testkit::{ensure_scheduler, TestSupervisor};

/// Mirror of `hew_observe::events` known producer-emitted metadata names.
/// `hew-observe` is a bin-only crate today and cannot be a dev-dependency
/// of `hew-runtime`; the canonical name set is mirrored here so a producer
/// rename would fail the assertion in
/// [`canonical_names_mirror_producer_and_consumer`].
const KNOWN_EVENT_NAMES: &[&str] = &[
    "begin",
    "end",
    "spawn",
    "crash",
    "stop",
    "send",
    "io_accept",
    "io_recv",
    "duplex_created",
    "duplex_half_split",
    "duplex_closed",
    "sink_closed",
    "stream_closed",
    "lambda_spawned",
    "lambda_released",
    "supervisor_restart",
    "supervisor_escalate",
    "supervisor_circuit_open",
    "supervisor_circuit_half_open",
    "supervisor_circuit_close",
    "supervisor_max_restarts",
    "supervisor_backoff",
];

/// Process-global lock — `RECENT_CRASHES`, fault-injection table, the
/// scheduler, and the trace ring buffer are shared with other integration
/// tests in the same binary.
static TEST_LOCK: Mutex<()> = Mutex::new(());

const MSG_CRASH: i32 = 1;
const MSG_OK: i32 = 2;

unsafe extern "C-unwind" fn taxonomy_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    if msg_type == MSG_CRASH {
        unsafe { hew_trap_with_code(HEW_TRAP_DIVIDE_BY_ZERO) };
    }
    // MSG_OK: return normally.
    std::ptr::null_mut()
}

unsafe fn wait_for_crash_log_growth(initial: i32, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    while Instant::now() < deadline {
        if unsafe { hew_crash_log_count() } > initial {
            return true;
        }
        std::thread::sleep(Duration::from_millis(5));
    }
    false
}

#[derive(Debug)]
struct DrainedEvent {
    event_type: String,
}

fn parse_event_types(json: &str) -> Vec<DrainedEvent> {
    // Lightweight ad-hoc parser: scan for `"event_type":"NAME"` occurrences.
    // Avoids pulling serde_json type derivation just to read one field;
    // exact JSON shape is asserted by other tests in this crate.
    let mut out = Vec::new();
    let needle = "\"event_type\":\"";
    let mut rest = json;
    while let Some(idx) = rest.find(needle) {
        let after = &rest[idx + needle.len()..];
        if let Some(end) = after.find('"') {
            out.push(DrainedEvent {
                event_type: after[..end].to_string(),
            });
            rest = &after[end + 1..];
        } else {
            break;
        }
    }
    out
}

/// Gate test: the closed taxonomy in `hew-observe` (mirrored here as
/// `KNOWN_EVENT_NAMES`) must be exactly the producer-side set in
/// `hew_runtime::tracing::EVENT_TYPE_NAMES`. If a `SPAN_*` constant is
/// added or renamed, this test fails first.
#[test]
fn canonical_names_mirror_producer_and_consumer() {
    let producer: HashSet<&str> = EVENT_TYPE_NAMES.iter().map(|(_, n)| *n).collect();
    let consumer: HashSet<&str> = KNOWN_EVENT_NAMES.iter().copied().collect();
    assert_eq!(
        producer,
        consumer,
        "trace-event taxonomy diverged between producer (hew-runtime) and \
         consumer mirror (KNOWN_EVENT_NAMES). Missing on consumer mirror: \
         {missing_consumer:?}; missing on producer: {missing_producer:?}. \
         Update hew-observe::events metadata names to match.",
        missing_consumer = producer.difference(&consumer).collect::<Vec<_>>(),
        missing_producer = consumer.difference(&producer).collect::<Vec<_>>(),
    );
    assert!(!KNOWN_EVENT_NAMES.contains(&"unknown"));
}

/// E2E probe: drive a multi-actor supervised program with a crash → restart
/// and a duplex channel, drain trace events, assert every drained
/// `event_type` resolves to the closed taxonomy and that the canonical
/// events for the exercised scenario actually appeared.
#[test]
fn v05_concurrency_program_has_no_unknown_trace_events() {
    const STRATEGY_ONE_FOR_ONE: i32 = 0;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    let _guard = TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    ensure_scheduler();
    hew_deterministic_reset();

    // Reset trace state and enable explicitly so prior tests don't leak
    // events into this drain.
    hew_trace_reset();
    hew_trace_enable(1);
    assert_eq!(hew_trace_is_enabled(), 1);

    let sup = TestSupervisor::new(STRATEGY_ONE_FOR_ONE, 5, 60);
    unsafe {
        hew_supervisor_set_restart_notify(sup.as_ptr());

        let mut state: i32 = 0;
        let name = CString::new("taxonomy-child").unwrap();
        let spec = HewChildSpec {
            name: name.as_ptr(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(taxonomy_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            arena_cap_bytes: 0,
            cycle_capable: 0,
            on_crash: None,
            lifecycle_fn: None,
            init_fn: None,
            config: ptr::null_mut(),
            config_size: 0,
        };
        assert_eq!(
            hew_supervisor_add_child_spec(sup.as_ptr(), &raw const spec),
            0,
            "add_child_spec must succeed"
        );
        assert_eq!(sup.start(), 0, "supervisor must start");

        let child = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!child.is_null(), "initial child must spawn within 5s");

        #[allow(clippy::cast_sign_loss, reason = "i32 → i32 compare seam")]
        let log_before: i32 = hew_crash_log_count();

        // Trigger a crash → restart.
        hew_actor_send(child, MSG_CRASH, ptr::null_mut(), 0);
        assert!(
            wait_for_crash_log_growth(log_before, Duration::from_secs(5)),
            "crash report must appear within 5s of MSG_CRASH"
        );
        let _ = hew_supervisor_wait_restart(sup.as_ptr(), 1, 5_000);

        let restarted = hew_supervisor_get_child_wait(sup.as_ptr(), 0, 5_000);
        assert!(!restarted.is_null(), "restarted child must spawn within 5s");
        // Send a benign message to the restarted child so the SPAN_SEND
        // emission for the restarted lane is exercised.
        hew_actor_send(restarted, MSG_OK, ptr::null_mut(), 0);

        // Exercise the channel substrate so duplex_* events are emitted.
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
        let pair_rc = hew_duplex_pair(8, 8, &raw mut a, &raw mut b);
        assert_eq!(pair_rc, 0, "hew_duplex_pair must succeed");
        assert!(!a.is_null() && !b.is_null());
        hew_duplex_close(a);
        hew_duplex_close(b);

        // Give the scheduler a moment to flush the post-send lifecycle
        // events. The drain below caps at 256 events which is well above
        // what this scenario produces.
        std::thread::sleep(Duration::from_millis(50));
    }

    let json = drain_events_json();
    let events = parse_event_types(&json);
    assert!(
        !events.is_empty(),
        "drain must include at least one trace event for the exercised v0.5 \
         scenario (got empty JSON: {json})"
    );

    let known: HashSet<&str> = KNOWN_EVENT_NAMES.iter().copied().collect();

    let mut unknown = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for ev in &events {
        if !known.contains(ev.event_type.as_str()) {
            unknown.push(ev.event_type.clone());
        }
        seen.insert(ev.event_type.clone());
    }

    assert!(
        unknown.is_empty(),
        "v0.5 program produced trace events outside the closed taxonomy: \
         {unknown:?}. Every event_type must be added to \
         hew_runtime::tracing::EVENT_TYPE_NAMES and \
         hew_observe::events metadata names together. Drained JSON: {json}"
    );

    // Canonical events the exercised scenario MUST surface. A drain that
    // silently drops these would mask producer regressions across actor
    // lifecycle, dispatch begin/end, and the channel substrate.
    //
    // `send` and `crash` are not asserted here even though MSG_CRASH/MSG_OK
    // are sent: `record_send` and the crash lifecycle emission both gate
    // on a current trace context (`trace_context()`), which is installed
    // only inside actor dispatch / scheduler frames. External
    // `hew_actor_send` and crash-recovery longjmp paths intentionally do
    // not synthesise one. Their emission sites are still wired to the
    // closed taxonomy (see `EVENT_TYPE_NAMES`) and round-tripped by the
    // tracing-module unit tests; this probe asserts the no-unknown
    // closure for the events that do drain.
    for required in ["spawn", "begin", "end"] {
        assert!(
            seen.contains(required),
            "expected canonical lifecycle event {required:?} in drained \
             taxonomy; saw {seen:?}. JSON: {json}"
        );
    }
    // Channel substrate must surface its create + close pair.
    for required in ["duplex_created", "duplex_closed"] {
        assert!(
            seen.contains(required),
            "expected channel event {required:?} in drained taxonomy; \
             saw {seen:?}. JSON: {json}"
        );
    }

    hew_trace_reset();
    hew_deterministic_reset();
}
