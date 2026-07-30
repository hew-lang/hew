//! Caller-side release oracle for fresh strings returned through shipped Hew
//! standard-library wrappers.
//!
//! Red baseline at `f7b703131`, before `hew_markdown_to_html` had a measured
//! retention row: 3 calls leaked 3 nodes / 192 bytes and 50 calls leaked
//! 50 nodes / 3,200 bytes. The admitted contract makes both probes exact zero.

#![cfg(unix)]

mod support;

use support::leak_slope::assert_frame_slope_below_tolerance_exact_lines;

fn markdown_wrapper_source(frames: usize) -> String {
    format!(
        "import std::encoding::markdown;\n\
         fn main() {{\n\
         \x20   for _ in 0..{frames} {{\n\
         \x20       println(markdown.to_html(\"# retained-owner probe\").len());\n\
         \x20   }}\n\
         }}\n"
    )
}

/// Closure-invoke carrier probe. Each frame exercises the three return sources
/// a compiler-generated invoke shim normalizes to one caller-owned share:
///
/// - a captured string (`ClosureEnvFieldLoad`, retained by codegen);
/// - a heap-producing by-value string argument (the caller releases its share,
///   while the closure retains the returned parameter at the return edge);
/// - a fresh transform result (its existing `+1` transfers).
///
/// The fourth call forwards the captured result through a Hew wrapper whose
/// tail is indirect. Before the string-only carrier authority, both a direct
/// `borrow_len(make())` and `borrow_len(invoke(make))` leaked one allocation per
/// call because `CallClosure` was not a fresh-string producer and the wrapper's
/// general return provenance was intentionally `OPAQUE`.
fn closure_carrier_source(frames: usize) -> String {
    format!(
        "fn borrow_len(value: string) -> i64 {{ value.len() }}\n\
         fn invoke(make: fn() -> string) -> string {{ make() }}\n\
         fn main() {{\n\
         \x20   let seed = \"captured-owner\".to_upper();\n\
         \x20   let captured = || seed;\n\
         \x20   let parameter = |value: string| value;\n\
         \x20   let fresh = || \"fresh-owner\".to_upper();\n\
         \x20   for _ in 0..{frames} {{\n\
         \x20       println(borrow_len(captured()));\n\
         \x20       println(borrow_len(parameter(\"parameter-owner\".to_upper())));\n\
         \x20       println(borrow_len(fresh()));\n\
         \x20       println(borrow_len(invoke(captured)));\n\
         \x20   }}\n\
         }}\n"
    )
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn shipped_markdown_string_wrapper_has_no_per_call_leak() {
    assert_frame_slope_below_tolerance_exact_lines(
        "markdown_string_wrapper",
        markdown_wrapper_source,
        std::convert::identity,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn closure_invoke_string_returns_have_no_per_call_leak() {
    assert_frame_slope_below_tolerance_exact_lines(
        "closure_invoke_string_carrier",
        closure_carrier_source,
        |frames| frames * 4,
    );
}
