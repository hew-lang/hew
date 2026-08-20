//! Exact ownership oracle for a retained local copy of a matched enum's
//! `string` payload.
//!
//! The destructure binder aliases the parent's rc=1 payload. A later
//! local-to-local share emits one retain, so the destination must balance that
//! independent `+1` while the parent enum balances the original ref. The same
//! rule applies when the destination lives in a nested lexical block.

#![cfg(unix)]

mod support;

use std::process::Command;

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

fn function_section<'a>(dump: &'a str, name: &str) -> &'a str {
    let marker = format!("fn {name}");
    let start = dump
        .find(&marker)
        .unwrap_or_else(|| panic!("missing `{marker}` in MIR dump:\n{dump}"));
    let tail = &dump[start..];
    tail.find("\nfn ").map_or(tail, |next| &tail[..next])
}

fn unique_drop_locals<'a>(section: &'a str, marker: &str) -> Vec<&'a str> {
    let mut locals = section
        .lines()
        .filter(|line| line.contains(marker))
        .filter_map(|line| line.split_whitespace().nth(1))
        .collect::<Vec<_>>();
    locals.sort_unstable();
    locals.dedup();
    locals
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
        let section = function_section(&elaborated, name);
        assert_eq!(
            unique_drop_locals(section, "ty=Box kind=enum_in_place").len(),
            1,
            "{name} must retain exactly one parent payload authority:\n{section}"
        );
        assert_eq!(
            unique_drop_locals(section, "ty=string kind=cow_heap(hew_string_drop)").len(),
            1,
            "{name} must balance the retained destination exactly once:\n{section}"
        );
    }
    let chained = function_section(&elaborated, "chained");
    assert_eq!(
        unique_drop_locals(chained, "ty=Box kind=enum_in_place").len(),
        1,
        "the retained chain must preserve the original parent owner:\n{chained}"
    );
    assert_eq!(
        unique_drop_locals(chained, "ty=string kind=cow_heap(hew_string_drop)").len(),
        2,
        "both retained chain destinations must balance their independent refs:\n{chained}"
    );
    let direct = function_section(&elaborated, "direct");
    assert_eq!(
        unique_drop_locals(direct, "ty=Box kind=enum_in_place").len(),
        1,
        "the no-handoff control must keep its parent authority:\n{direct}"
    );
    assert!(
        unique_drop_locals(direct, "ty=string kind=cow_heap(hew_string_drop)").is_empty(),
        "the non-retained payload alias must not gain a competing drop:\n{direct}"
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
