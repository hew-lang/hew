//! Leak / double-free oracle for the #2434 root-cause split.
//!
//! The generic-record path is pinned with a single fresh `string.repeat` field,
//! while the real leak shape is record-free nested concat:
//! `first + " " + last`. The first concat result is a fresh
//! `hew_string_concat` temp immediately borrowed by the second concat; the MIR
//! nested-temp collector must release that intermediate exactly once without
//! broadening to ownership-transfer sinks.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance_with, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

const LOW_FRAMES: usize = 1;
const HIGH_FRAMES: usize = 50;
const SLOPE_TOLERANCE: usize = 3;

fn generic_record_repeat_source(frames: usize) -> String {
    format!(
        "import std::string;\n\
         type Pair<A, B> {{ first: A; second: B; }}\n\
         fn emit_pair(print_it: bool) -> i64 {{\n\
         \x20   let full = string.repeat(\"field\", 1);\n\
         \x20   let pair = Pair {{ first: 1, second: full }};\n\
         \x20   if print_it {{ println(pair.second); }}\n\
         \x20   pair.first\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + emit_pair(i == 0);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total != {frames} {{ return 91; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn nested_concat_no_record_source(frames: usize) -> String {
    let expected = frames * 12;
    format!(
        "fn emit_full(print_it: bool) -> i64 {{\n\
         \x20   let first = \"Ada\";\n\
         \x20   let last = \"Lovelace\";\n\
         \x20   let full = first + \" \" + last;\n\
         \x20   if print_it {{ println(full); }}\n\
         \x20   full.len()\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + emit_full(i == 0);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total != {expected} {{ return 92; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("nested-concat-temp-df-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{shape_name}: string temps must be released exactly once; a crash \
         indicates a double-free and a non-zero status indicates a scribbled-read \
         miscompute:\n{}",
        describe_output(&output)
    );
}

#[test]
fn generic_record_repeat_field_has_no_per_iteration_leak_slope() {
    assert_frame_slope_below_tolerance_with(
        "generic_record_repeat_clean",
        generic_record_repeat_source,
        LOW_FRAMES,
        HIGH_FRAMES,
        SLOPE_TOLERANCE,
    );
}

#[test]
fn nested_concat_without_record_has_no_per_iteration_leak_slope() {
    assert_frame_slope_below_tolerance_with(
        "nested_concat_no_record",
        nested_concat_no_record_source,
        LOW_FRAMES,
        HIGH_FRAMES,
        SLOPE_TOLERANCE,
    );
}

#[test]
fn generic_record_repeat_field_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "generic_record_repeat_df",
        &generic_record_repeat_source(200),
    );
}

#[test]
fn nested_concat_without_record_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "nested_concat_no_record_df",
        &nested_concat_no_record_source(200),
    );
}
