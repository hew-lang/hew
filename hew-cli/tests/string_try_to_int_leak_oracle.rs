//! Leak oracle for `std::string::try_to_int`.
//!
//! Each parsed character is obtained with `string.slice`, which allocates a
//! fresh heap string. The parser must release that temporary after
//! `digit_value` borrows it, both on the loop back-edge and on early returns.

#![cfg(unix)]

mod support;

use support::leak_slope::assert_frame_slope_below_tolerance;

fn valid_multi_digit_source(frames: usize) -> String {
    let expected = frames * 1_234_567;
    format!(
        "import std::string;\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       match string.try_to_int(\"1234567\") {{\n\
         \x20           Ok(value) => {{ total = total + value; }},\n\
         \x20           Err(_) => {{ return 91; }},\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   if total == {expected} {{ 0 }} else {{ 92 }}\n\
         }}\n"
    )
}

fn invalid_trailing_digit_source(frames: usize) -> String {
    format!(
        "import std::string;\n\
         fn main() -> i64 {{\n\
         \x20   var failures: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       match string.try_to_int(\"123456x\") {{\n\
         \x20           Ok(_) => {{ return 93; }},\n\
         \x20           Err(_) => {{ failures = failures + 1; }},\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   if failures == {frames} {{ 0 }} else {{ 94 }}\n\
         }}\n"
    )
}

#[test]
fn valid_multi_digit_parse_has_no_per_digit_leak_slope() {
    assert_frame_slope_below_tolerance("try_to_int_valid_multi_digit", valid_multi_digit_source);
}

#[test]
fn invalid_digit_early_return_has_no_per_digit_leak_slope() {
    assert_frame_slope_below_tolerance(
        "try_to_int_invalid_trailing_digit",
        invalid_trailing_digit_source,
    );
}
