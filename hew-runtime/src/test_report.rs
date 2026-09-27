//! Private per-process outcome record for `hew test`.
//!
//! The test runner supplies a fresh `HEW_TEST_REPORT` path. Terminal fault
//! reporting records the fault's typed data here; the process exit authority
//! writes one JSON record after cleanup and actor shutdown have settled.

use std::sync::Mutex;

use crate::util::MutexExt;

#[derive(Clone)]
struct FaultEvidence {
    kind: &'static str,
    code: i32,
    message: Option<String>,
    site_offset: Option<u32>,
    assertion: Option<crate::fault::AssertionOperands>,
}

static FAULT: Mutex<Option<FaultEvidence>> = Mutex::new(None);

/// Return the zero-based entry chosen by the test runner. An absent or
/// malformed selection is invalid, so the generated dispatcher can fail
/// closed before calling any test body.
#[no_mangle]
pub extern "C" fn hew_test_selected() -> i64 {
    if std::env::var_os("HEW_TEST_TRACE_PATH").is_some() {
        crate::tracing::hew_trace_enable(1);
    }
    std::env::var("HEW_TEST")
        .ok()
        .and_then(|value| value.parse::<u32>().ok())
        .map_or(-1, i64::from)
}

pub(crate) fn note_fault(
    kind: &'static str,
    code: i32,
    message: Option<&str>,
    site_offset: Option<u32>,
    assertion: Option<&crate::fault::AssertionOperands>,
) {
    if std::env::var_os("HEW_TEST_REPORT").is_none() {
        return;
    }
    let mut slot = FAULT.lock_or_recover();
    if slot.is_none() {
        *slot = Some(FaultEvidence {
            kind,
            code,
            message: message.map(str::to_owned),
            site_offset,
            assertion: assertion.cloned(),
        });
    }
}

pub(crate) fn finish(status: i32) {
    let Some(path) = std::env::var_os("HEW_TEST_REPORT") else {
        return;
    };
    let fault = FAULT.lock_or_recover().clone();
    let outcome = if status == 0 {
        "passed"
    } else if fault.is_some() {
        "fault"
    } else {
        "exit"
    };
    let driver = crate::driver::report_state();
    let record = serde_json::json!({
        "version": 1,
        "outcome": outcome,
        "status": status,
        "fault_kind": fault.as_ref().map(|fault| fault.kind),
        "fault_code": fault.as_ref().map(|fault| fault.code),
        "message": fault.as_ref().and_then(|fault| fault.message.as_deref()),
        "site_offset": fault.as_ref().and_then(|fault| fault.site_offset),
        "assertion": fault.as_ref().and_then(|fault| fault.assertion.as_ref()),
        "schedule": driver.map(|(schedule, _, _, _)| schedule),
        "seed": driver.map(|(_, seed, _, _)| seed.to_string()),
        "steps": driver.map(|(_, _, steps, _)| steps),
        "virtual_time_ms": driver.map(|(_, _, _, time)| time),
    });
    if let Err(error) = std::fs::write(path, record.to_string()) {
        eprintln!("hew: cannot write test report: {error}");
    }
    write_test_trace(status, fault.as_ref(), driver);
}

fn write_test_trace(
    status: i32,
    fault: Option<&FaultEvidence>,
    driver: Option<(&str, u64, u64, u64)>,
) {
    let Some(path) = std::env::var_os("HEW_TEST_TRACE_PATH") else {
        return;
    };
    let result = if status == 0 {
        "ok"
    } else if fault.is_some_and(|fault| fault.kind == "UserPanic") {
        "panic"
    } else {
        "runtime_failure"
    };
    let seed = driver.map_or_else(|| "0".to_string(), |(_, seed, _, _)| seed.to_string());
    let steps = driver.map_or(0, |(_, _, steps, _)| steps);
    let current_ms = driver.map_or(0, |(_, _, _, current_ms)| current_ms);
    let budget = std::env::var("HEW_DETERMINISTIC")
        .ok()
        .and_then(|config| {
            config
                .split(',')
                .find_map(|part| part.strip_prefix("budget=")?.parse::<u64>().ok())
        })
        .unwrap_or(10_000_000);
    let clock = serde_json::json!({"epoch_ms": 0, "tick_ms": 1, "current_ms": current_ms});
    let mut events = vec![serde_json::json!({
        "seq": 0,
        "type": "trace.started",
        "phase": "run",
        "span": null,
    })];
    for event in crate::tracing::drain_events(16_384) {
        let name = crate::tracing::event_type_name(event.event_type).unwrap_or("unknown");
        events.push(serde_json::json!({
            "seq": events.len(),
            "type": "step.committed",
            "phase": "run",
            "span": null,
            "message": name,
            "id_kind": "actor",
            "id": event.actor_id.to_string(),
            "step_count": steps,
            "timestamp_ns": event.timestamp_ns.to_string(),
        }));
    }
    let runtime_failures = fault.map_or_else(Vec::new, |fault| {
        vec![serde_json::json!({
            "kind": if fault.kind == "UserPanic" { "panic" } else { "trap" },
            "message": fault.message.as_deref().unwrap_or(fault.kind),
            "span": null,
            "trap_kind": null,
            "hew_fault": {"code": fault.code, "kind": fault.kind},
            "assertion": fault.assertion,
        })]
    });
    if let Some(failure) = runtime_failures.first() {
        events.push(serde_json::json!({
            "seq": events.len(),
            "type": "runtime.failure",
            "phase": "run",
            "span": null,
            "failure": failure,
        }));
    }
    events.push(serde_json::json!({
        "seq": events.len(),
        "type": "trace.ended",
        "phase": "run",
        "span": null,
        "message": result,
    }));
    let trace = serde_json::json!({
        "schema_version": "hew.sandbox.trace.v0",
        "trace_id": "trace:hew-test-native",
        "fixture_id": "hew-test",
        "profile": "native",
        "hew_version": env!("CARGO_PKG_VERSION"),
        "sandbox_version": "native",
        "result": result,
        "replay": {"seed": seed, "step_budget": budget, "virtual_clock": clock, "inputs": []},
        "events": events,
        "final_state": {
            "status": result,
            "exit_code": if status == 0 { Some(status) } else { None },
            "step_count": steps,
            "budget_remaining": budget.saturating_sub(steps),
            "virtual_clock": clock,
            "stdout": [],
            "stderr": [],
            "ids": {"actors": [], "channels": [], "tasks": [], "supervisors": [], "machines": []},
            "diagnostics": [],
            "sandbox_rejections": [],
            "runtime_failures": runtime_failures,
            "globals": [],
        },
    });
    if let Err(error) = std::fs::write(path, format!("{trace}\n")) {
        eprintln!("hew: cannot write test trace: {error}");
    }
}
