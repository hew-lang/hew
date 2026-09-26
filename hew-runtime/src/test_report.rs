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
}

static FAULT: Mutex<Option<FaultEvidence>> = Mutex::new(None);

/// Return the zero-based entry chosen by the test runner. An absent or
/// malformed selection is invalid, so the generated dispatcher can fail
/// closed before calling any test body.
#[no_mangle]
pub extern "C" fn hew_test_selected() -> i64 {
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
    let record = serde_json::json!({
        "version": 1,
        "outcome": outcome,
        "status": status,
        "fault_kind": fault.as_ref().map(|fault| fault.kind),
        "fault_code": fault.as_ref().map(|fault| fault.code),
        "message": fault.as_ref().and_then(|fault| fault.message.as_deref()),
        "site_offset": fault.as_ref().and_then(|fault| fault.site_offset),
    });
    if let Err(error) = std::fs::write(path, record.to_string()) {
        eprintln!("hew: cannot write test report: {error}");
    }
}
