//! Generator-local enum overwrite leak oracle.
//!
//! A generator body is lowered by a synthetic child `Builder`. That child must
//! inherit the module enum layouts: they are the authority that classifies a
//! `Box::Full(string)` local as heap-owning and emits the tag-aware release when
//! it is overwritten with `Box::Empty` after a yield. Before the fix the child
//! was assembled from a partial table list, so this generator leaked one string
//! node per iteration while the byte-identical ordinary function stayed flat.
//!
//! The two slope probes preserve that differential. Each iteration allocates an
//! enum string payload, overwrites it, and prints one witness line only after
//! the operation completed. The generator probe crosses a yield/resume boundary;
//! the ordinary control does not. On macOS, `leaks(1)` must report a flat node
//! count from three to fifty iterations for both shapes.

#![cfg(unix)]

mod support;

use support::leak_slope::assert_frame_slope_below_tolerance_exact_lines;

fn expected_lines(frames: usize) -> usize {
    frames
}

fn generator_overwrite_source(frames: usize) -> String {
    format!(
        "enum Box {{ Full(string); Empty; }}\n\
         \n\
         fn generate_once() -> i64 {{\n\
         \x20   let g = gen {{\n\
         \x20       var value: Box = Box.Full(string_concat(\"generator-\", \"payload\"));\n\
         \x20       yield 1;\n\
         \x20       value = Box.Empty;\n\
         \x20   }};\n\
         \x20   var yielded: i64 = 0;\n\
         \x20   for n in g {{ yielded = yielded + n; }}\n\
         \x20   yielded\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       if generate_once() != 1 {{ panic(\"generator did not resume through overwrite\"); }}\n\
         \x20       println(\"tick\");\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         }}\n"
    )
}

fn ordinary_overwrite_control_source(frames: usize) -> String {
    format!(
        "enum Box {{ Full(string); Empty; }}\n\
         \n\
         fn overwrite_once() {{\n\
         \x20   var value: Box = Box.Full(string_concat(\"control-\", \"payload\"));\n\
         \x20   value = Box.Empty;\n\
         }}\n\
         \n\
         fn main() {{\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       overwrite_once();\n\
         \x20       println(\"tick\");\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         }}\n"
    )
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS leaks(1); absent capability must be a counted skip"
)]
#[test]
fn generator_enum_payload_overwrite_has_no_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "generator_enum_payload_overwrite",
        generator_overwrite_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS leaks(1); absent capability must be a counted skip"
)]
#[test]
fn ordinary_enum_payload_overwrite_control_stays_clean() {
    assert_frame_slope_below_tolerance_exact_lines(
        "ordinary_enum_payload_overwrite_control",
        ordinary_overwrite_control_source,
        expected_lines,
    );
}
