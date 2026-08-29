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
    drop_plan_counts, function_section, retained_payload_locals, unique_drop_locals,
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
        assert_eq!(
            unique_drop_locals(section, "ty=Box kind=enum_in_place").len(),
            1,
            "{name} must retain exactly one parent composite authority:\n{section}"
        );
        let payload_locals = retained_payload_locals(raw_section, "string.retain_fresh_share ");
        assert_eq!(
            payload_locals.len(),
            2,
            "{name} must have one transferred payload owner and one retained owner:\n\
             {raw_section}"
        );
        for local in payload_locals {
            let raw_normal = raw_section
                .matches(&format!(
                    "drop {local} ty=string fn=release(hew_string_drop)"
                ))
                .count();
            let marker = format!("drop {local} ty=string kind=cow_heap(hew_string_drop)");
            let (planned_normal, exceptional, max_per_plan) = drop_plan_counts(section, &marker);
            assert_eq!(
                raw_normal + planned_normal,
                1,
                "{name} must release {local} exactly once on successful normal flow:\n\
                 raw:\n{raw_section}\nelaborated:\n{section}"
            );
            assert!(
                exceptional > 0 && max_per_plan == 1,
                "{name} must give {local} one cleanup in each applicable mutually exclusive \
                 unwind/cancel/panic plan, never duplicate it within one plan:\n{section}"
            );
        }
    }
    let chained = function_section(&elaborated, "chained");
    let chained_raw = function_section(&raw, "chained");
    let chained_locals = retained_payload_locals(chained_raw, "string.retain_fresh_share ");
    assert_eq!(
        unique_drop_locals(chained, "ty=Box kind=enum_in_place").len(),
        1,
        "the retained chain must preserve one parent composite authority:\n{chained}"
    );
    assert_eq!(
        chained_locals.len(),
        3,
        "the chain must track its transferred payload owner and both retained owners:\n\
         {chained_raw}"
    );
    for local in chained_locals {
        let raw_normal = chained_raw
            .matches(&format!(
                "drop {local} ty=string fn=release(hew_string_drop)"
            ))
            .count();
        let marker = format!("drop {local} ty=string kind=cow_heap(hew_string_drop)");
        let (planned_normal, exceptional, max_per_plan) = drop_plan_counts(chained, &marker);
        assert_eq!(
            raw_normal + planned_normal,
            1,
            "the retained chain must release {local} exactly once on successful normal flow:\n\
             raw:\n{chained_raw}\nelaborated:\n{chained}"
        );
        assert!(
            exceptional > 0 && max_per_plan == 1,
            "the retained chain must clean {local} on each applicable exceptional exit \
             without duplicating it within one plan:\n{chained}"
        );
    }
    let direct = function_section(&elaborated, "direct");
    let direct_raw = function_section(&raw, "direct");
    assert_eq!(
        unique_drop_locals(direct, "ty=Box kind=enum_in_place").len(),
        1,
        "the no-handoff control must keep its parent composite authority:\n{direct}"
    );
    let direct_locals = retained_payload_locals(direct_raw, "string.retain_fresh_share ");
    assert_eq!(
        direct_locals.len(),
        1,
        "the direct payload transfer must create no retained share:\n{direct_raw}"
    );
    let local = direct_locals[0];
    let raw_normal = direct_raw
        .matches(&format!(
            "drop {local} ty=string fn=release(hew_string_drop)"
        ))
        .count();
    let marker = format!("drop {local} ty=string kind=cow_heap(hew_string_drop)");
    let (planned_normal, exceptional, max_per_plan) = drop_plan_counts(direct, &marker);
    assert_eq!(
        raw_normal + planned_normal,
        1,
        "the direct payload owner must release exactly once on successful normal flow:\n\
         raw:\n{direct_raw}\nelaborated:\n{direct}"
    );
    assert!(
        exceptional > 0 && max_per_plan == 1,
        "the direct payload owner must have one cleanup on each applicable exceptional exit:\n\
         {direct}"
    );
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
