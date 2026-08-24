//! Exact ownership oracle for a nested enum payload stored in a returned record.
//!
//! A nested-pattern binder is an alias of storage still owned by the matched
//! call-result carrier. Returning that alias inside a record must mint one
//! independent string owner before the carrier is dropped. The control puts an
//! explicit `clone()` at that one boundary and therefore needs no implicit
//! retain.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::{assert_frame_slope_below_tolerance_exact_lines, compile_to_native};
use support::{describe_output, hew_binary, repo_root, require_codegen};

const SOURCE_TEMPLATE: &str = r#"
enum CleanupError {
    Dirty(string);
}

type Retirement {
    blocked: bool;
    detail: string;
}

type Outcome {
    status: string;
}

fn persist(value: string) {
    if value.len() == 0 {
        panic("empty");
    }
}

fn cleanup() -> Result<(), CleanupError> {
    Err(CleanupError.Dirty("dirty worktree " + f"{42}"))
}

fn retire() -> Result<Retirement, string> {
    match cleanup() {
        .Ok(_) => Ok(Retirement { blocked: false, detail: "" }),
        .Err(CleanupError.Dirty(message)) => {
            persist(message.clone());
            Ok(Retirement { blocked: true, detail: __DETAIL__ })
        },
    }
}

fn lifecycle() -> Result<Outcome, string> {
    let retirement = match retire() {
        .Err(message) => return Err(message),
        .Ok(value) => value,
    };
    Ok(Outcome {
        status: if retirement.blocked { "blocked" } else { "merged" },
    })
}

fn main() {
    for _ in 0..64 {
        match lifecycle() {
            .Err(message) => panic(message),
            .Ok(outcome) => {
                if outcome.status != "blocked" {
                    panic("wrong outcome");
                }
            },
        }
    }
    println("ok");
}
"#;

fn source(detail: &str) -> String {
    SOURCE_TEMPLATE.replace("__DETAIL__", detail)
}

fn repeated_source(frames: usize) -> String {
    repeated_source_with_alias(frames, false)
}

fn aliased_repeated_source(frames: usize) -> String {
    repeated_source_with_alias(frames, true)
}

fn repeated_source_with_alias(frames: usize, alias: bool) -> String {
    let alias_binding = if alias { "let alias = message;" } else { "" };
    let second = if alias { "alias" } else { "message" };
    format!(
        r#"
enum CleanupError {{
    Dirty(string);
}}

type Pair {{
    first: string;
    second: string;
}}

fn cleanup() -> Result<(), CleanupError> {{
    Err(CleanupError.Dirty("dirty worktree " + f"{{42}}"))
}}

fn build() -> Result<Pair, string> {{
    match cleanup() {{
        .Ok(_) => Ok(Pair {{ first: "", second: "" }}),
        .Err(CleanupError.Dirty(message)) => {{
            {alias_binding}
            Ok(Pair {{ first: message, second: {second} }})
        }},
    }}
}}

fn main() {{
    for _ in 0..{frames} {{
        match build() {{
            .Err(message) => panic(message),
            .Ok(pair) => {{
                if pair.first != pair.second {{
                    panic("wrong pair");
                }}
            }},
        }}
        println("frame");
    }}
}}
"#
    )
}

fn overwritten_alias_source(frames: usize) -> String {
    format!(
        r#"
enum CleanupError {{
    Dirty(string);
}}

type Pair {{
    first: string;
    second: string;
}}

fn cleanup() -> Result<(), CleanupError> {{
    Err(CleanupError.Dirty("dirty worktree " + f"{{42}}"))
}}

fn build(i: i64) -> Result<Pair, string> {{
    match cleanup() {{
        .Ok(_) => Ok(Pair {{ first: "", second: "" }}),
        .Err(CleanupError.Dirty(message)) => {{
            var current = message;
            let alias = current;
            current = "replacement " + f"{{i}}";
            Ok(Pair {{ first: current, second: alias }})
        }},
    }}
}}

fn main() {{
    for i in 0..{frames} {{
        match build(i) {{
            .Err(message) => panic(message),
            .Ok(pair) => {{
                if pair.first == pair.second {{
                    panic("wrong generations");
                }}
            }},
        }}
        println("frame");
    }}
}}
"#
    )
}

fn stable_loop_alias_source(frames: usize) -> String {
    format!(
        r#"
enum CleanupError {{
    Dirty(string);
}}

type Pair {{
    first: string;
    second: string;
}}

fn persist(value: string) {{
    if value.is_empty() {{
        panic("empty");
    }}
}}

fn cleanup() -> Result<(), CleanupError> {{
    Err(CleanupError.Dirty("dirty worktree " + f"{{42}}"))
}}

fn build() -> Result<Pair, string> {{
    match cleanup() {{
        .Ok(_) => Ok(Pair {{ first: "", second: "" }}),
        .Err(CleanupError.Dirty(message)) => {{
            let alias = message;
            for _ in 0..1 {{
                persist(alias.clone());
            }}
            Ok(Pair {{ first: message, second: alias }})
        }},
    }}
}}

fn main() {{
    for _ in 0..{frames} {{
        match build() {{
            .Err(message) => panic(message),
            .Ok(pair) => {{
                if pair.first != pair.second {{
                    panic("wrong pair");
                }}
            }},
        }}
        println("frame");
    }}
}}
"#
    )
}

fn overwritten_before_fork_source(frames: usize) -> String {
    format!(
        r#"
enum CleanupError {{
    Dirty(string);
}}

type Pair {{
    first: string;
    second: string;
}}

fn cleanup() -> Result<(), CleanupError> {{
    Err(CleanupError.Dirty("dirty worktree " + f"{{42}}"))
}}

fn build(i: i64) -> Result<Pair, string> {{
    match cleanup() {{
        .Ok(_) => Ok(Pair {{ first: "", second: "" }}),
        .Err(CleanupError.Dirty(message)) => {{
            var current = message;
            current = "replacement " + f"{{i}}";
            let alias = current;
            Ok(Pair {{ first: current, second: alias }})
        }},
    }}
}}

fn main() {{
    for i in 0..{frames} {{
        match build(i) {{
            .Err(message) => panic(message),
            .Ok(pair) => {{
                if pair.first != pair.second {{
                    panic("wrong pair");
                }}
            }},
        }}
        println("frame");
    }}
}}
"#
    )
}

fn dump_raw_mir(source: &str, name: &str) -> String {
    let dir = tempfile::Builder::new()
        .prefix("returned-record-projected-string-mir-")
        .tempdir()
        .expect("tempdir");
    let path = dir.path().join(format!("{name}.hew"));
    std::fs::write(&path, source).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--dump-mir",
            "raw",
            path.to_str().expect("Hew source path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile --dump-mir raw");
    assert!(
        output.status.success(),
        "raw MIR dump failed for {name}:\n{}",
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

fn compile_and_run(source: &str, name: &str) {
    let dir = tempfile::Builder::new()
        .prefix("returned-record-projected-string-run-")
        .tempdir()
        .expect("tempdir");
    let binary = compile_to_native(source, dir.path(), name);
    let output = Command::new(&binary)
        .output()
        .unwrap_or_else(|error| panic!("run {}: {error}", binary.display()));
    assert!(
        output.status.success(),
        "{name} must release the carrier and returned record exactly once:\n{}",
        describe_output(&output)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "ok\n");
}

#[test]
fn nested_payload_return_mints_only_the_missing_owner() {
    require_codegen();

    let implicit = source("message");
    let explicit_control = source("message.clone()");

    let implicit_raw = dump_raw_mir(&implicit, "implicit_retain");
    let implicit_retire = function_section(&implicit_raw, "retire");
    assert_eq!(
        implicit_retire.match_indices("string.retain").count(),
        1,
        "the borrowed nested payload needs exactly one owner for the returned record:\n\
         {implicit_retire}"
    );

    let control_raw = dump_raw_mir(&explicit_control, "explicit_clone");
    let control_retire = function_section(&control_raw, "retire");
    assert_eq!(
        control_retire.match_indices("string.retain").count(),
        0,
        "the explicit one-boundary clone already owns the returned field:\n{control_retire}"
    );

    let repeated_raw = dump_raw_mir(&repeated_source(1), "repeated_source");
    let repeated_build = function_section(&repeated_raw, "build");
    assert_eq!(
        repeated_build.match_indices("string.retain").count(),
        2,
        "two returned fields sharing one borrowed nested payload need exactly two owners:\n\
         {repeated_build}"
    );

    let aliased_raw = dump_raw_mir(&aliased_repeated_source(1), "aliased_repeated_source");
    let aliased_build = function_section(&aliased_raw, "build");
    assert_eq!(
        aliased_build.match_indices("string.retain").count(),
        2,
        "distinct locals in one projected-owner family still need exactly two returned owners:\n\
         {aliased_build}"
    );

    let overwritten_raw = dump_raw_mir(&overwritten_alias_source(1), "overwritten_alias");
    let overwritten_build = function_section(&overwritten_raw, "build");
    assert_eq!(
        overwritten_build.match_indices("string.retain").count(),
        2,
        "an overwritten binding must not join its replacement generation to a historical alias:\n\
         {overwritten_build}"
    );

    let looped_raw = dump_raw_mir(&stable_loop_alias_source(1), "stable_loop_alias");
    let looped_build = function_section(&looped_raw, "build");
    assert_eq!(
        looped_build.match_indices("string.retain").count(),
        2,
        "a read-only loop preserves the forked generation and needs no extra owner:\n\
         {looped_build}"
    );

    let before_fork_raw = dump_raw_mir(
        &overwritten_before_fork_source(1),
        "overwritten_before_fork",
    );
    let before_fork_build = function_section(&before_fork_raw, "build");
    assert_eq!(
        before_fork_build.match_indices("string.retain").count(),
        2,
        "the fresh replacement base owner and its retained fork are sufficient:\n\
         {before_fork_build}"
    );
}

#[test]
fn nested_payload_return_and_explicit_clone_control_release_exactly_once() {
    require_codegen();
    compile_and_run(&source("message"), "implicit_retain");
    compile_and_run(&source("message.clone()"), "explicit_clone");
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the low/high leak-slope oracle requires macOS leaks(1)"
)]
#[test]
fn repeated_nested_payload_return_has_no_per_iteration_leak() {
    require_codegen();
    assert_frame_slope_below_tolerance_exact_lines(
        "repeated_returned_nested_string_payload",
        repeated_source,
        |frames| frames,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the low/high leak-slope oracle requires macOS leaks(1)"
)]
#[test]
fn aliased_repeated_nested_payload_return_has_no_per_iteration_leak() {
    require_codegen();
    assert_frame_slope_below_tolerance_exact_lines(
        "aliased_repeated_returned_nested_string_payload",
        aliased_repeated_source,
        |frames| frames,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the low/high leak-slope oracle requires macOS leaks(1)"
)]
#[test]
fn overwritten_alias_generations_have_no_per_iteration_leak() {
    require_codegen();
    assert_frame_slope_below_tolerance_exact_lines(
        "overwritten_returned_string_alias_generations",
        overwritten_alias_source,
        |frames| frames,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the low/high leak-slope oracle requires macOS leaks(1)"
)]
#[test]
fn stable_alias_across_read_only_loop_has_no_per_iteration_leak() {
    require_codegen();
    assert_frame_slope_below_tolerance_exact_lines(
        "stable_alias_across_read_only_loop",
        stable_loop_alias_source,
        |frames| frames,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the low/high leak-slope oracle requires macOS leaks(1)"
)]
#[test]
fn fresh_overwrite_before_fork_has_no_per_iteration_leak() {
    require_codegen();
    assert_frame_slope_below_tolerance_exact_lines(
        "fresh_overwrite_before_fork",
        overwritten_before_fork_source,
        |frames| frames,
    );
}
