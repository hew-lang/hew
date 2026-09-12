//! Exact ownership oracle for a retained local copy of a matched enum's
//! `bytes` payload.
//!
//! Destructuring transfers the parent's rc=1 payload into the payload binder
//! and neutralizes the enum slot. A local copy therefore needs one explicit
//! retain: the binder balances the original reference and the destination
//! balances the new one. The direct control emits no retain.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    compile_to_native, measure_leaks_exact, run_probe_witness, run_under_malloc_scribble,
    HIGH_FRAMES, LOW_FRAMES,
};
use support::{describe_output, require_codegen};

const SOURCE_TEMPLATE: &str = r#"
enum Blob {
    Data(bytes),
    Empty,
}

fn same_scope(i: i64) -> i64 {
    let b = Blob.Data(f"same-{i}".to_bytes());
    match b {
        .Data(data) => {
            let inner = data;
            inner.len() + data.len()
        },
        .Empty => 0,
    }
}

fn nested_scope(i: i64) -> i64 {
    let b = Blob.Data(f"nested-{i}".to_bytes());
    match b {
        .Data(data) => {
            let n = {
                let inner = data;
                inner.len()
            };
            n + data.len()
        },
        .Empty => 0,
    }
}

fn direct(i: i64) -> i64 {
    let b = Blob.Data(f"direct-{i}".to_bytes());
    match b {
        .Data(data) => data.len(),
        .Empty => 0,
    }
}

fn main() -> i64 {
    var total = 0;
    for i in 0..__FRAMES__ {
        total = total + same_scope(i) + nested_scope(i) + direct(i);
        println("frame");
    }
    if total > 0 { 0 } else { 91 }
}
"#;

fn source(frames: usize) -> String {
    SOURCE_TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

// Lost coverage: `mir_pins_retain_and_noncompeting_drop_authorities` used
// `--dump-mir raw`/`elab` (both retired) to count `bytes.retain` and
// `hew_bytes_drop` release-authority occurrences per function section,
// pinning the transferred-vs-retained payload owner split and exactly-once
// release on every normal and exceptional exit plan. Physical MIR's
// structured (Debug) dump has no equivalent single-line text to grep or
// count per function, so this MIR-emission coverage has no direct
// replacement. The same leak/double-free behaviour for the same shapes is
// still proven end to end below by the leak-slope and malloc-scribble
// oracles in this file.

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact leak oracle needs macOS leaks(1); absent capability must be a counted skip"
)]
#[test]
fn low_and_high_bytes_payload_handoffs_are_exactly_leak_clean() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("bytes-payload-handoff-leaks-")
        .tempdir()
        .expect("tempdir");
    for frames in [LOW_FRAMES, HIGH_FRAMES] {
        let bin = compile_to_native(
            &source(frames),
            dir.path(),
            &format!("bytes_payload_handoff_{frames}"),
        );
        assert_eq!(
            run_probe_witness(&bin, &[]),
            frames,
            "the leak sample must execute every requested frame"
        );
        assert_eq!(
            measure_leaks_exact(&bin),
            (0, 0),
            "{frames} frames must release both retained handoffs and the direct control"
        );
    }
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the deterministic poisoned-allocator contract is macOS-only"
)]
#[test]
fn high_bytes_payload_handoffs_do_not_double_free_or_read_poison() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("bytes-payload-handoff-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &source(HIGH_FRAMES),
        dir.path(),
        "bytes_payload_handoff_scribble",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "both aliases must remain live through their reads and release exactly once:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout)
            .lines()
            .filter(|line| *line == "frame")
            .count(),
        HIGH_FRAMES,
        "the poisoned run must execute every requested frame"
    );
}
