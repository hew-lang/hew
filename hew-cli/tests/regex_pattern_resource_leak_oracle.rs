//! Regex `Pattern` resource close leak oracle.
//!
//! The fixture compiles a standalone native binary, repeatedly creates and
//! explicitly closes a compiled regex, then compares the `leaks --atExit`
//! allocation count at low and high iteration counts. A `Pattern` release
//! regression produces a per-iteration slope; a fixed close path stays flat.

#![cfg(unix)]

mod support;

use support::leak_slope::assert_frame_slope_below_tolerance;

fn explicit_close_loop_source(frames: usize) -> String {
    format!(
        "import std::text::regex;\n\
         fn main() -> i64 {{\n\
         \x20   var matched: i64 = 0;\n\
         \x20   for _ in 0..{frames} {{\n\
         \x20       let pattern = regex.new(\"[a-z]+[0-9]+\");\n\
         \x20       if pattern.is_match(\"abc123\") {{ matched = matched + 1; }}\n\
         \x20       pattern.close();\n\
         \x20   }}\n\
         \x20   matched\n\
         }}\n"
    )
}

#[test]
fn regex_pattern_explicit_close_has_no_per_iteration_leak() {
    assert_frame_slope_below_tolerance("regex_pattern_explicit_close", explicit_close_loop_source);
}
