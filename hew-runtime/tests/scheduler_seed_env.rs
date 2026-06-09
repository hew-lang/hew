#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI test harness reads scheduler dispatch context and actor IDs"
)]

use std::ffi::c_void;
use std::process::Command;
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

use hew_runtime::deterministic::hew_deterministic_get_seed;
use hew_runtime_testkit::{HewExecutionContext, TestActor};

static TRACE: (Mutex<Vec<(u64, i32)>>, Condvar) = (Mutex::new(Vec::new()), Condvar::new());

unsafe extern "C-unwind" fn record_dispatch(
    ctx: *mut HewExecutionContext,
    _state: *mut c_void,
    msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) {
    let actor_id = if ctx.is_null() {
        0
    } else {
        unsafe { (*ctx).actor_id }
    };
    let mut trace = TRACE.0.lock().unwrap();
    trace.push((actor_id, msg_type));
    TRACE.1.notify_all();
}

fn wait_for_trace_len(expected: usize) {
    let deadline = Instant::now() + Duration::from_secs(10);
    let mut trace = TRACE.0.lock().unwrap();
    while trace.len() < expected {
        let remaining = deadline.saturating_duration_since(Instant::now());
        assert!(!remaining.is_zero(), "timed out waiting for dispatch trace");
        let (guard, result) = TRACE.1.wait_timeout(trace, remaining).unwrap();
        trace = guard;
        assert!(
            !(result.timed_out() && trace.len() < expected),
            "timed out waiting for dispatch trace"
        );
    }
}

fn trace_lines(seed: Option<&str>) -> Vec<String> {
    let mut command = Command::new(std::env::current_exe().unwrap());
    command
        .env("HEW_WORKERS", "1")
        .arg("--ignored")
        .arg("--exact")
        .arg("seed_trace_helper")
        .arg("--nocapture");
    match seed {
        Some(seed) => {
            command.env("HEW_SEED", seed);
        }
        None => {
            command.env_remove("HEW_SEED");
        }
    }

    let output = command.output().unwrap();
    assert!(output.status.success(), "helper failed: {output:?}");

    String::from_utf8(output.stdout)
        .unwrap()
        .lines()
        .filter_map(|line| line.strip_prefix("TRACE ").map(str::to_owned))
        .collect()
}

fn trace_with_stderr(seed: &str) -> (Vec<String>, String) {
    let mut command = Command::new(std::env::current_exe().unwrap());
    let output = command
        .env("HEW_WORKERS", "1")
        .env("HEW_SEED", seed)
        .arg("--ignored")
        .arg("--exact")
        .arg("seed_trace_helper")
        .arg("--nocapture")
        .output()
        .unwrap();
    assert!(output.status.success(), "helper failed: {output:?}");

    let trace = String::from_utf8(output.stdout)
        .unwrap()
        .lines()
        .filter_map(|line| line.strip_prefix("TRACE ").map(str::to_owned))
        .collect();
    (trace, String::from_utf8(output.stderr).unwrap())
}

#[test]
fn hew_seed_env_controls_scheduler_seed() {
    let seeded_a = trace_lines(Some("42"));
    let seeded_b = trace_lines(Some("42"));
    assert_eq!(
        seeded_a, seeded_b,
        "same HEW_SEED must replay byte-identically"
    );

    let unset_a = trace_lines(None);
    let unset_b = trace_lines(None);
    assert_ne!(
        unset_a, unset_b,
        "unset HEW_SEED must keep randomized startup"
    );

    let (garbage_a, stderr) = trace_with_stderr("not_a_number");
    let (garbage_b, _) = trace_with_stderr("not_a_number");
    assert!(
        stderr.contains("HEW_SEED='not_a_number' not parseable as u64"),
        "malformed HEW_SEED should emit warning, got: {stderr}"
    );
    assert_ne!(
        garbage_a, garbage_b,
        "malformed HEW_SEED must fall back to randomized startup"
    );
}

#[test]
#[ignore = "subprocess helper; parent test invokes this in isolated scheduler processes"]
fn seed_trace_helper() {
    TRACE.0.lock().unwrap().clear();
    hew_runtime::scheduler::hew_sched_init();

    let mut actors = Vec::new();
    for _ in 0..3 {
        actors.push(TestActor::spawn(record_dispatch));
    }

    let actor_ids = actors
        .iter()
        .map(|actor| unsafe { (*actor.as_ptr()).id }.to_string())
        .collect::<Vec<_>>()
        .join(",");
    println!("TRACE seed={}", hew_deterministic_get_seed());
    println!("TRACE actor_ids={actor_ids}");

    for (idx, actor) in actors.iter().enumerate() {
        for offset in 0..2 {
            actor.send_empty(i32::try_from(idx * 10 + offset).unwrap());
        }
    }

    wait_for_trace_len(6);
    let messages = TRACE
        .0
        .lock()
        .unwrap()
        .iter()
        .map(|(actor_id, msg_type)| format!("{actor_id}:{msg_type}"))
        .collect::<Vec<_>>()
        .join(",");
    println!("TRACE message_ids={messages}");
}
