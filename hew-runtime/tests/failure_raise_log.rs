//! A raised failure is published once, at its origin, to every projection:
//! the last-fault record a debugger reads and the NDJSON failure log.
//!
//! One test per binary: the failure policy is read from the environment once.

use hew_runtime::debug_accessors::hew_debug_last_fault;
use hew_runtime::fault::{hew_fault_drop, hew_fault_new, hew_fault_report, hew_fault_set_site};

#[test]
fn a_raised_failure_reaches_the_last_fault_record_and_the_log() {
    let dir = std::env::temp_dir().join(format!("hew-failure-log-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let log = dir.join("failures.ndjson");
    let _ = std::fs::remove_file(&log);
    // SAFETY: no other thread of this single-test binary reads the environment.
    unsafe { std::env::set_var("HEW_FAILURE_LOG", &log) };

    // A cancellation is a completion marker, not a failure: nothing is raised.
    let cancelled = hew_fault_new(-1);
    // SAFETY: each fault is a live unique owner until dropped.
    unsafe {
        hew_fault_set_site(cancelled, 7);
        hew_fault_drop(cancelled);
    }
    assert!(hew_debug_last_fault().is_null());

    let fault = hew_fault_new(202);
    // SAFETY: as above.
    unsafe {
        hew_fault_set_site(fault, 80);
        // Reporting later does not raise the failure a second time.
        assert_eq!(hew_fault_report(fault), 0);
        hew_fault_drop(fault);
    }

    let last = hew_debug_last_fault();
    assert!(!last.is_null());
    // SAFETY: the last-fault record is static storage.
    let last = unsafe { *last };
    assert_eq!(last.code, 202);
    assert_eq!(last.kind_name(), "DivideByZero");
    assert_eq!(last.site.offset, 80);
    assert!(last.frame_count > 0, "frames are captured at the origin");

    let text = std::fs::read_to_string(&log).unwrap();
    let lines: Vec<&str> = text.lines().collect();
    assert_eq!(lines.len(), 1, "one line per raised failure:\n{text}");
    let record: serde_json::Value = serde_json::from_str(lines[0]).unwrap();
    assert_eq!(record["kind"], "DivideByZero");
    assert_eq!(record["code"], 202);
    assert_eq!(record["site"]["offset"], 80);
    assert!(!record["frames"].as_array().unwrap().is_empty());
    let _ = std::fs::remove_dir_all(&dir);
}
