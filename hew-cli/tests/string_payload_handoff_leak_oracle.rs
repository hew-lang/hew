//! Exact ownership oracle for a retained local copy of a matched enum's
//! `string` payload.
//!
//! Destructuring transfers the parent's rc=1 payload into the payload binder
//! and neutralizes the enum slot. A later local-to-local share emits one
//! retain, so the binder balances the original reference and the destination
//! balances the independent `+1`. The same rule applies in nested scopes.

#![cfg(unix)]

#[path = "support/payload_handoff_mir.rs"]
mod payload_handoff_mir;
mod support;

use std::process::Command;

use payload_handoff_mir::{
    drop_plan_counts, function_section, projected_payload_binder_is_alias, retained_payload_locals,
    unique_drop_locals,
};
use support::leak_slope::{
    compile_to_native, measure_leaks_exact, run_probe_witness, run_under_malloc_scribble,
    HIGH_FRAMES, LOW_FRAMES,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

const SOURCE_TEMPLATE: &str = r#"
enum Box {
    Text(string);
    Empty;
}

fn same_scope(i: i64) -> i64 {
    let b = Box.Text(f"same-{i}");
    match b {
        Text(s) => {
            let inner = s;
            inner.len() + s.len()
        },
        Empty => 0,
    }
}

fn nested_scope(i: i64) -> i64 {
    let b = Box.Text(f"nested-{i}");
    match b {
        Text(s) => {
            let n = {
                let inner = s;
                inner.len()
            };
            n + s.len()
        },
        Empty => 0,
    }
}

fn chained(i: i64) -> i64 {
    let b = Box.Text(f"chain-{i}");
    match b {
        Text(s) => {
            let first = s;
            let second = first;
            second.len() + first.len() + s.len()
        },
        Empty => 0,
    }
}

fn direct(i: i64) -> i64 {
    let b = Box.Text(f"direct-{i}");
    match b {
        Text(s) => s.len(),
        Empty => 0,
    }
}

fn main() -> i64 {
    var total = 0;
    for i in 0..__FRAMES__ {
        total = total + same_scope(i) + nested_scope(i) + chained(i) + direct(i);
        println("frame");
    }
    if total > 0 { 0 } else { 91 }
}
"#;

fn source(frames: usize) -> String {
    SOURCE_TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

fn dump_mir(stage: &str) -> String {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("string-payload-handoff-mir-")
        .tempdir()
        .expect("tempdir");
    let path = dir.path().join("string_payload_handoff.hew");
    std::fs::write(&path, source(1)).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--dump-mir",
            stage,
            path.to_str().expect("Hew source path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .unwrap_or_else(|error| panic!("invoke hew compile --dump-mir {stage}: {error}"));
    assert!(
        output.status.success(),
        "{stage} MIR dump failed:\n{}",
        describe_output(&output)
    );
    String::from_utf8(output.stdout).expect("MIR dump is UTF-8")
}

fn assert_payload_drop_authority(
    name: &str,
    raw_section: &str,
    elaborated_section: &str,
    expected_payload_owners: usize,
) {
    assert_eq!(
        unique_drop_locals(elaborated_section, "ty=Box kind=enum_in_place").len(),
        1,
        "{name} must preserve exactly one parent composite authority:\n{elaborated_section}"
    );
    let payload_locals = retained_payload_locals(raw_section, "string.retain_fresh_share ");
    assert_eq!(
        payload_locals.len(),
        expected_payload_owners,
        "{name} must track exactly {expected_payload_owners} payload owner(s):\n{raw_section}"
    );
    // A `string` payload binder keeps its own flag-guarded release, so it is
    // not demoted here; the check is shared with the `bytes` oracle so both
    // read the release authority off the stream rather than assuming one (#2523).
    let binder_is_alias = projected_payload_binder_is_alias(raw_section);
    for (index, local) in payload_locals.iter().enumerate() {
        let expected_releases = usize::from(!(binder_is_alias && index == 0));
        let raw_normal = raw_section
            .matches(&format!(
                "drop {local} ty=string fn=release(hew_string_drop)"
            ))
            .count();
        let marker = format!("drop {local} ty=string kind=cow_heap(hew_string_drop)");
        let (planned_normal, exceptional, max_per_plan) =
            drop_plan_counts(elaborated_section, &marker);
        assert_eq!(
            raw_normal + planned_normal,
            expected_releases,
            "{name} must release {local} exactly {expected_releases} time(s) on successful \
             normal flow:\nraw:\n{raw_section}\nelaborated:\n{elaborated_section}"
        );
        if expected_releases == 0 {
            assert_eq!(
                exceptional, 0,
                "{name} must leave every exit of the aliased binder {local} to the parent \
                 composite drop:\n{elaborated_section}"
            );
            continue;
        }
        assert!(
            exceptional > 0 && max_per_plan == 1,
            "{name} must clean {local} on each applicable exceptional exit without duplicating \
             it within one plan:\n{elaborated_section}"
        );
    }
}

#[test]
fn mir_pins_retains_and_noncompeting_drop_authorities() {
    let raw = dump_mir("raw");
    for name in ["same_scope", "nested_scope"] {
        let section = function_section(&raw, name);
        assert_eq!(
            section.match_indices("string.retain").count(),
            1,
            "{name} must mint exactly one independent local owner:\n{section}"
        );
    }
    let chained_raw = function_section(&raw, "chained");
    assert_eq!(
        chained_raw.match_indices("string.retain").count(),
        2,
        "each link in a live retained handoff chain must mint one owner:\n{chained_raw}"
    );

    let elaborated = dump_mir("elab");
    for name in ["same_scope", "nested_scope"] {
        let raw_section = function_section(&raw, name);
        let section = function_section(&elaborated, name);
        assert_payload_drop_authority(name, raw_section, section, 2);
    }

    let chained = function_section(&elaborated, "chained");
    assert_payload_drop_authority("chained", chained_raw, chained, 3);

    let direct_raw = function_section(&raw, "direct");
    let direct = function_section(&elaborated, "direct");
    assert_payload_drop_authority("direct", direct_raw, direct, 1);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact leak oracle needs macOS leaks(1); absent capability must be a counted skip"
)]
#[test]
fn low_and_high_string_payload_handoffs_are_exactly_leak_clean() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("string-payload-handoff-leaks-")
        .tempdir()
        .expect("tempdir");
    for frames in [LOW_FRAMES, HIGH_FRAMES] {
        let bin = compile_to_native(
            &source(frames),
            dir.path(),
            &format!("string_payload_handoff_{frames}"),
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
fn high_string_payload_handoffs_do_not_double_free_or_read_poison() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("string-payload-handoff-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &source(HIGH_FRAMES),
        dir.path(),
        "string_payload_handoff_scribble",
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
