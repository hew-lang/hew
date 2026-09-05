//! Exact ownership oracle for nested-enum payloads handed to a shorter-lived
//! arm-local owner before a second match.
//!
//! The direct nested-match controls already retained their outer tag-aware
//! drop. The leak lived in the whole-local, record-field, and tuple-field
//! forwarding shapes: lexical child scopes were mistaken for escaping scopes,
//! suppressing every final owner. The allocator checks pin exact zero at low
//! and high iteration counts, while the MIR checks identify which local keeps
//! the sole drop authority. The poisoned-allocator run guards the opposite
//! regression: admitting both a parent alias and its forwarded child.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::{
    compile_to_native, measure_leaks_exact, run_probe_witness, run_under_malloc_scribble,
    HIGH_FRAMES, LOW_FRAMES,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

const SOURCE_TEMPLATE: &str = r#"
enum Inner {
    Text(string),
    Empty,
}

enum Outer {
    Wrap(Inner),
    Empty,
}

enum Middle {
    Wrap(Inner),
    Empty,
}

enum DeepOuter {
    Wrap(Middle),
    Empty,
}

type Holder {
    value: Inner,
    sibling: i64,
}

enum RecordOuter {
    Wrap(Holder),
    Empty,
}

enum TupleOuter {
    Wrap((Inner, i64)),
    Empty,
}

fn whole(i: i64) -> i64 {
    let outer = Outer.Wrap(Inner.Text(f"whole-{i}"));
    match outer {
        .Wrap(st) => {
            let w = st;
            match w {
                .Text(s) => s.len(),
                .Empty => 0,
            }
        },
        .Empty => 0,
    }
}

fn direct(i: i64) -> i64 {
    let outer = Outer.Wrap(Inner.Text(f"direct-{i}"));
    match outer {
        .Wrap(st) => match st {
            .Text(s) => s.len(),
            .Empty => 0,
        },
        .Empty => 0,
    }
}

fn depth_two(i: i64) -> i64 {
    let outer = DeepOuter.Wrap(Middle.Wrap(Inner.Text(f"deep-{i}")));
    match outer {
        .Wrap(mid) => match mid {
            .Wrap(st) => match st {
                .Text(s) => s.len(),
                .Empty => 0,
            },
            .Empty => 0,
        },
        .Empty => 0,
    }
}

fn record_field(i: i64) -> i64 {
    let outer = RecordOuter.Wrap(Holder {
        value: Inner.Text(f"record-{i}"),
        sibling: i,
    });
    match outer {
        .Wrap(st) => {
            let w = st.value;
            match w {
                .Text(s) => s.len() + st.sibling,
                .Empty => st.sibling,
            }
        },
        .Empty => 0,
    }
}

fn tuple_field(i: i64) -> i64 {
    let outer = TupleOuter.Wrap((Inner.Text(f"tuple-{i}"), i));
    match outer {
        .Wrap(st) => {
            let w = st.0;
            match w {
                .Text(s) => s.len() + st.1,
                .Empty => st.1,
            }
        },
        .Empty => 0,
    }
}

fn main() -> i64 {
    var total = 0;
    for i in 0..__FRAMES__ {
        total = total + direct(i) + depth_two(i) + whole(i)
            + record_field(i) + tuple_field(i);
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
        .prefix("nested-enum-whole-forward-mir-")
        .tempdir()
        .expect("tempdir");
    let path = dir.path().join("nested_enum_whole_forward.hew");
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

fn count(section: &str, needle: &str) -> usize {
    section.match_indices(needle).count()
}

fn enum_drop_locals<'a>(section: &'a str, ty: &str) -> Vec<&'a str> {
    let marker = format!("ty={ty} kind=enum_in_place");
    let mut locals = section
        .lines()
        .filter(|line| line.contains(&marker))
        .filter_map(|line| line.split_whitespace().nth(1))
        .collect::<Vec<_>>();
    locals.sort_unstable();
    locals.dedup();
    locals
}

#[test]
fn raw_and_elaborated_mir_pin_forwarding_shapes_and_single_drop_authority() {
    let raw = dump_mir("raw");
    let whole_raw = function_section(&raw, "whole");
    let record_raw = function_section(&raw, "record_field");
    let tuple_raw = function_section(&raw, "tuple_field");
    assert!(
        whole_raw.contains("neutralize_payload")
            && whole_raw.match_indices(" = move ").count() >= 2,
        "whole-local handoff must retain its move-out neutralization and forwarding move:\n\
         {whole_raw}"
    );
    assert!(
        record_raw.contains(".field[0]") && record_raw.contains(" = move "),
        "record-field handoff must remain visible before elaboration:\n{record_raw}"
    );
    assert!(
        tuple_raw.contains(".0") && tuple_raw.contains(" = move "),
        "tuple-field handoff must remain visible before elaboration:\n{tuple_raw}"
    );

    let elaborated = dump_mir("elab");
    let whole = function_section(&elaborated, "whole");
    let direct = function_section(&elaborated, "direct");
    let depth_two = function_section(&elaborated, "depth_two");
    let record = function_section(&elaborated, "record_field");
    let tuple = function_section(&elaborated, "tuple_field");

    assert_eq!(
        enum_drop_locals(whole, "Inner"),
        ["_16"],
        "every child-scope handoff exit must use the same final Inner owner:\n{whole}"
    );
    assert_eq!(
        enum_drop_locals(whole, "Outer"),
        ["_8"],
        "the neutralized outer shell may retain only its one tag-aware no-op authority:\n{whole}"
    );
    for (name, section, outer_ty) in [
        ("direct", direct, "Outer"),
        ("depth_two", depth_two, "DeepOuter"),
        ("record_field", record, "RecordOuter"),
        ("tuple_field", tuple, "TupleOuter"),
    ] {
        assert_eq!(
            enum_drop_locals(section, outer_ty).len(),
            1,
            "{name} must use one outer local as its tag-aware owner on every exit:\n{section}"
        );
        assert_eq!(
            count(section, "ty=Inner kind=enum_in_place"),
            0,
            "{name} must not grant a competing nested-enum drop:\n{section}"
        );
    }
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact leak oracle needs macOS leaks(1); absent capability must be a counted skip"
)]
#[test]
fn low_and_high_whole_forward_handoffs_are_exactly_leak_clean() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("nested-enum-whole-forward-leaks-")
        .tempdir()
        .expect("tempdir");
    for frames in [LOW_FRAMES, HIGH_FRAMES] {
        let bin = compile_to_native(
            &source(frames),
            dir.path(),
            &format!("nested_enum_whole_forward_{frames}"),
        );
        assert_eq!(
            run_probe_witness(&bin, &[]),
            frames,
            "the exact leak sample must execute every requested helper frame"
        );
        assert_eq!(
            measure_leaks_exact(&bin),
            (0, 0),
            "{frames} helper frames must release every direct-local, record-field, and \
             tuple-field forwarded String"
        );
    }
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the deterministic poisoned-allocator contract is macOS-only"
)]
#[test]
fn high_whole_forward_handoffs_do_not_double_free_or_read_poison() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("nested-enum-whole-forward-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &source(HIGH_FRAMES),
        dir.path(),
        "nested_enum_whole_forward_scribble",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "all forwarded payloads must remain live through their final read and release exactly \
         once:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout)
            .lines()
            .filter(|line| *line == "frame")
            .count(),
        HIGH_FRAMES,
        "the poisoned run must execute every direct and forwarded helper"
    );
}
