//! Divergent returned-resource ownership oracles.
//!
//! Returned-member discovery is intentionally flow-insensitive: every owned
//! member reaching the return slot is removed from ordinary scope teardown.
//! That prevents a callee from closing a resource transferred to its caller,
//! but it used to strand the unselected members of a divergent return:
//!
//! ```text
//! if c { (s1, r1) } else { (s2, r2) }
//! ```
//!
//! The per-member transfer-block map already distinguishes those arms. These
//! tests pin the missing affine-resource half of its path-sensitive
//! re-admission: the selected pair transfers, while the unselected pair closes
//! exactly once on its arm-to-join edge. Tuple and record returns get allocator
//! slopes, raw/elaborated MIR checks, and a poisoned-allocator opposite-
//! direction pin. An early `Result::Err` return covers cleanup before either
//! resource pair enters the return flow.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

const DIVERGENT_TUPLE_TEMPLATE: &str = r#"
import std::stream;

fn choose(c: bool) -> (Sink<string>, Stream<string>) {
    let (s1, r1) = stream.pipe(8);
    let (s2, r2) = stream.pipe(8);
    if c { (s1, r1) } else { (s2, r2) }
}

fn main() {
    for i in 0..__FRAMES__ {
        let (sink, input) = choose(i % 2 == 0);
        sink.close();
        input.close();
        println("frame");
    }
}
"#;

const DIVERGENT_RECORD_TEMPLATE: &str = r#"
import std::stream;

type Pipe {
    sink: Sink<string>,
    input: Stream<string>,
}

fn choose(c: bool) -> Pipe {
    let (s1, r1) = stream.pipe(8);
    let (s2, r2) = stream.pipe(8);
    if c {
        Pipe { sink: s1, input: r1 }
    } else {
        Pipe { sink: s2, input: r2 }
    }
}

fn main() {
    for i in 0..__FRAMES__ {
        let selected = choose(i % 2 != 0);
        selected.sink.close();
        selected.input.close();
        println("frame");
    }
}
"#;

const EARLY_ERROR_TEMPLATE: &str = r#"
import std::stream;

type Pipe {
    sink: Sink<string>,
    input: Stream<string>,
}

fn choose_or_error(bail: bool, c: bool) -> Result<Pipe, string> {
    let (s1, r1) = stream.pipe(8);
    let (s2, r2) = stream.pipe(8);
    if bail {
        return Err("bailed");
    }
    if c {
        Ok(Pipe { sink: s1, input: r1 })
    } else {
        Ok(Pipe { sink: s2, input: r2 })
    }
}

fn main() {
    for i in 0..__FRAMES__ {
        match choose_or_error(true, i % 2 == 0) {
            Ok(unexpected) => {
                unexpected.sink.close();
                unexpected.input.close();
            },
            Err(_) => {},
        }
        println("frame");
    }
}
"#;

const COMBINED_TEMPLATE: &str = r#"
import std::stream;

type Pipe {
    sink: Sink<string>,
    input: Stream<string>,
}

fn divergent_tuple(c: bool) -> (Sink<string>, Stream<string>) {
    let (s1, r1) = stream.pipe(8);
    let (s2, r2) = stream.pipe(8);
    if c { (s1, r1) } else { (s2, r2) }
}

fn divergent_record(c: bool) -> Pipe {
    let (s1, r1) = stream.pipe(8);
    let (s2, r2) = stream.pipe(8);
    if c {
        Pipe { sink: s1, input: r1 }
    } else {
        Pipe { sink: s2, input: r2 }
    }
}

fn same_tuple(c: bool) -> (Sink<string>, Stream<string>) {
    let (s1, r1) = stream.pipe(8);
    let (s2, r2) = stream.pipe(8);
    if c { (s1, r1) } else { (s1, r1) }
}

fn error_before_transfer(bail: bool, c: bool) -> Result<Pipe, string> {
    let (s1, r1) = stream.pipe(8);
    let (s2, r2) = stream.pipe(8);
    if bail {
        return Err("bailed");
    }
    if c {
        Ok(Pipe { sink: s1, input: r1 })
    } else {
        Ok(Pipe { sink: s2, input: r2 })
    }
}

fn cancel_before_transfer(c: bool) -> (Sink<string>, Stream<string>) {
    let (s1, r1) = stream.pipe(8);
    let (s2, r2) = stream.pipe(8);
    var i = 0;
    while i < 1 {
        i = i + 1;
    }
    if c { (s1, r1) } else { (s2, r2) }
}

fn main() {
    for i in 0..__FRAMES__ {
        let (sink1, input1) = divergent_tuple(i % 2 == 0);
        sink1.close();
        input1.close();

        let selected = divergent_record(i % 2 != 0);
        selected.sink.close();
        selected.input.close();

        let (sink2, input2) = same_tuple(i % 2 == 0);
        sink2.close();
        input2.close();

        match error_before_transfer(true, i % 2 == 0) {
            Ok(unexpected) => {
                unexpected.sink.close();
                unexpected.input.close();
            },
            Err(_) => {},
        }
        match error_before_transfer(false, i % 2 != 0) {
            Ok(chosen) => {
                chosen.sink.close();
                chosen.input.close();
            },
            Err(_) => {},
        }
        let (sink3, input3) = cancel_before_transfer(i % 2 == 0);
        sink3.close();
        input3.close();
        println("frame");
    }
}
"#;

fn with_frames(template: &str, frames: usize) -> String {
    template.replace("__FRAMES__", &frames.to_string())
}

fn divergent_tuple_source(frames: usize) -> String {
    with_frames(DIVERGENT_TUPLE_TEMPLATE, frames)
}

fn divergent_record_source(frames: usize) -> String {
    with_frames(DIVERGENT_RECORD_TEMPLATE, frames)
}

fn early_error_source(frames: usize) -> String {
    with_frames(EARLY_ERROR_TEMPLATE, frames)
}

fn expected_lines(frames: usize) -> usize {
    frames
}

fn dump_mir(stage: &str) -> String {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("divergent-return-resource-mir-")
        .tempdir()
        .expect("tempdir");
    let source_path = dir.path().join("divergent_return_resource.hew");
    std::fs::write(&source_path, with_frames(COMBINED_TEMPLATE, 1)).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args(["compile", "--dump-mir", stage])
        .arg(&source_path)
        .current_dir(repo_root())
        .output()
        .unwrap_or_else(|error| panic!("invoke hew compile --dump-mir {stage}: {error}"));
    assert!(
        output.status.success(),
        "{stage} MIR dump failed:\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("ObligationUnderReleased"),
        "{stage} MIR still reports a live returned-resource member with no discharge:\n{stderr}"
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

fn drop_plan_bodies<'a>(section: &'a str, header_fragment: &str) -> Vec<Vec<&'a str>> {
    let plans = section
        .split("  drop_plans:\n")
        .nth(1)
        .unwrap_or_else(|| panic!("missing drop plans:\n{section}"));
    let mut matched = Vec::new();
    let mut current = None;
    for line in plans.lines() {
        let is_header =
            line.starts_with("    ") && !line.starts_with("      ") && line.ends_with(" ->");
        if is_header {
            if let Some(body) = current.take() {
                matched.push(body);
            }
            current = line.contains(header_fragment).then(Vec::new);
        } else if let Some(body) = &mut current {
            body.push(line);
        }
    }
    if let Some(body) = current {
        matched.push(body);
    }
    matched
}

#[test]
fn raw_and_elaborated_mir_attribute_only_unselected_resource_members() {
    let raw = dump_mir("raw");
    let tuple_raw = function_section(&raw, "divergent_tuple");
    let record_raw = function_section(&raw, "divergent_record");
    assert_eq!(
        count(tuple_raw, " = tuple ("),
        2,
        "tuple return must keep two distinct arm-local constructors:\n{tuple_raw}"
    );
    assert_eq!(
        count(record_raw, " = record_init Pipe"),
        2,
        "record return must keep two distinct arm-local constructors:\n{record_raw}"
    );

    let elaborated = dump_mir("elab");
    for name in ["divergent_tuple", "divergent_record"] {
        let section = function_section(&elaborated, name);
        assert_eq!(
            count(section, "fn=rt(SinkClose)"),
            2,
            "{name} must close exactly one unselected Sink on each arm:\n{section}"
        );
        assert_eq!(
            count(section, "fn=rt(StreamClose)"),
            2,
            "{name} must close exactly one unselected Stream on each arm:\n{section}"
        );
        let return_plans = drop_plan_bodies(section, "return[");
        assert!(
            return_plans.len() == 1 && return_plans[0] == ["      (none)"],
            "{name} must not close the selected resources after handing them to the caller:\n\
             {section}"
        );
    }

    let same = function_section(&elaborated, "same_tuple");
    assert_eq!(
        (
            count(same, "fn=rt(SinkClose)"),
            count(same, "fn=rt(StreamClose)")
        ),
        (1, 1),
        "same-member control must retain only the untouched pair's ordinary teardown:\n{same}"
    );

    let error = function_section(&elaborated, "error_before_transfer");
    let early_returns = drop_plan_bodies(error, "return[bb3]");
    let early_return = early_returns
        .first()
        .unwrap_or_else(|| panic!("missing early error return plan:\n{error}"));
    assert_eq!(
        (
            early_return
                .iter()
                .filter(|line| line.contains("fn=rt(SinkClose)"))
                .count(),
            early_return
                .iter()
                .filter(|line| line.contains("fn=rt(StreamClose)"))
                .count()
        ),
        (2, 2),
        "early error return must close both still-local resource pairs:\n{error}"
    );

    let cancel = function_section(&elaborated, "cancel_before_transfer");
    let live_cancel = drop_plan_bodies(cancel, "cancel[")
        .into_iter()
        .find(|plan| {
            plan.iter()
                .filter(|line| line.contains("fn=rt(SinkClose)"))
                .count()
                == 2
                && plan
                    .iter()
                    .filter(|line| line.contains("fn=rt(StreamClose)"))
                    .count()
                    == 2
        })
        .unwrap_or_else(|| panic!("missing live pre-transfer cancellation cleanup:\n{cancel}"));
    assert!(
        live_cancel
            .iter()
            .all(|line| line.contains("SinkClose") || line.contains("StreamClose")),
        "cancellation before transfer must close both still-local resource pairs:\n{cancel}"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)`; a host without it must record a skip"
)]
#[test]
fn divergent_tuple_resource_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance_exact_lines(
        "divergent_tuple_resource",
        divergent_tuple_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)`; a host without it must record a skip"
)]
#[test]
fn divergent_record_resource_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance_exact_lines(
        "divergent_record_resource",
        divergent_record_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)`; a host without it must record a skip"
)]
#[test]
fn early_error_resource_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance_exact_lines(
        "early_error_resource",
        early_error_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "poisoned allocator is macOS-only; a host without it must record a skip"
)]
#[test]
fn divergent_and_same_member_returns_run_clean_under_malloc_scribble() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("divergent-return-resource-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &with_frames(COMBINED_TEMPLATE, 20),
        dir.path(),
        "divergent_return_resource",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "divergent/same-member return controls must close every local resource \
         exactly once under the poisoned allocator:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).lines().count(),
        20,
        "poisoned-allocator probe must complete every loop iteration:\n{}",
        describe_output(&output)
    );
}
