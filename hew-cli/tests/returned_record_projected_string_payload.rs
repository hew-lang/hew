//! Exact ownership oracle for a nested enum payload stored in a returned record.
//!
//! A nested-pattern binder moved out of a consumed call-result carrier already
//! owns its payload. Returning that binder inside a record should transfer the
//! existing owner without a retain; only additional aliases or record fields
//! need new references. The explicit-`clone()` control likewise needs no
//! implicit retain beyond the clone call itself.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

const SOURCE_TEMPLATE: &str = r#"
enum CleanupError {
    Dirty(string),
}

type Retirement {
    blocked: bool,
    detail: string,
}

type Outcome {
    status: string,
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
    Dirty(string) }}

type Pair {{ 
    first: string, second: string }}

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
    Dirty(string),
}}

type Pair {{
    first: string,
    second: string,
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
    Dirty(string),
}}

type Pair {{
    first: string,
    second: string,
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
    Dirty(string),
}}

type Pair {{
    first: string,
    second: string,
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

fn reused_non_returned_alias_source(frames: usize) -> String {
    format!(
        r#"
type Pair {{
    first: string,
    second: string,
}}

fn main() {{
    let source = "reused fork " + f"{{42}}";
    let alias = source;
    for _ in 0..{frames} {{
        let pair = Pair {{ first: source, second: alias }};
        if pair.first != pair.second {{
            panic("mismatch");
        }}
        println("frame");
    }}
    if source != alias {{
        panic("source damaged");
    }}
}}
"#
    )
}

// Lost coverage: `nested_payload_return_retains_only_additional_owners` used
// `--dump-mir raw` (retired) to count `string.retain` occurrences per
// function section across several owner/alias/overwrite shapes (0, 1, 2 or
// 3 expected retains depending on shape). Physical MIR's structured
// (Debug) dump has no equivalent single-line text to grep or count per
// function, so this MIR-emission coverage has no direct replacement. The
// same exactly-once-release / no-per-iteration-leak behaviour for the same
// shapes is still proven end to end below by the leak-slope and
// poisoned-allocator oracles in this file.
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

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator is a macOS ownership oracle"
)]
#[test]
fn reused_non_returned_alias_is_clean_under_poisoned_allocator() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("reused-non-returned-alias-poison-")
        .tempdir()
        .expect("tempdir");
    let binary = compile_to_native(
        &reused_non_returned_alias_source(64),
        dir.path(),
        "reused_non_returned_alias_poison",
    );
    let output = run_under_malloc_scribble(&binary);
    assert!(
        output.status.success(),
        "reused source and alias must survive each temporary Pair:\n{}",
        describe_output(&output)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout).lines().count(), 64);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the low/high leak-slope oracle requires macOS leaks(1)"
)]
#[test]
fn reused_non_returned_alias_has_no_per_iteration_leak() {
    require_codegen();
    assert_frame_slope_below_tolerance_exact_lines(
        "reused_non_returned_alias",
        reused_non_returned_alias_source,
        |frames| frames,
    );
}
