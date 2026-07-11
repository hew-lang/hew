//! Leak oracle for `std::string::try_to_float`.
//!
//! Float validation and parsing obtain each character with `string.slice`,
//! which allocates a fresh heap string. The parser must release each temporary
//! immediately after the comparison or `digit_value` call borrows it.

#![cfg(unix)]

mod support;

use support::leak_slope::assert_frame_slope_below_tolerance;

fn valid_decimal_source(frames: usize) -> String {
    format!(
        "import std::string;\n\
         fn main() -> i64 {{\n\
         \x20   var checks: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       match string.try_to_float(\"1.25\") {{\n\
         \x20           Ok(value) => {{ if value != 1.25 {{ return 91; }} checks = checks + 1; }},\n\
         \x20           Err(_) => {{ return 91; }},\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   if checks == {frames} {{ 0 }} else {{ 92 }}\n\
         }}\n"
    )
}

fn varied_float_source(frames: usize) -> String {
    let expected_checks = frames * 10;
    format!(
        "import std::string;\n\
         fn main() -> i64 {{\n\
         \x20   var checks: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       match string.try_to_float(\"1234567\") {{\n\
         \x20           Ok(value) => {{ if value != 1234567.0 {{ return 101; }} checks = checks + 1; }},\n\
         \x20           Err(_) => {{ return 102; }},\n\
         \x20       }}\n\
         \x20       match string.try_to_float(\"12.5\") {{\n\
         \x20           Ok(value) => {{ if value != 12.5 {{ return 103; }} checks = checks + 1; }},\n\
         \x20           Err(_) => {{ return 104; }},\n\
         \x20       }}\n\
         \x20       match string.try_to_float(\"6e2\") {{\n\
         \x20           Ok(value) => {{ if value != 600.0 {{ return 105; }} checks = checks + 1; }},\n\
         \x20           Err(_) => {{ return 106; }},\n\
         \x20       }}\n\
         \x20       match string.try_to_float(\"-2.5E-1\") {{\n\
         \x20           Ok(value) => {{ if value != -0.25 {{ return 107; }} checks = checks + 1; }},\n\
         \x20           Err(_) => {{ return 108; }},\n\
         \x20       }}\n\
         \x20       match string.try_to_float(\"+.75\") {{\n\
         \x20           Ok(value) => {{ if value != 0.75 {{ return 109; }} checks = checks + 1; }},\n\
         \x20           Err(_) => {{ return 110; }},\n\
         \x20       }}\n\
         \x20       match string.try_to_float(\"4.\") {{\n\
         \x20           Ok(value) => {{ if value != 4.0 {{ return 111; }} checks = checks + 1; }},\n\
         \x20           Err(_) => {{ return 112; }},\n\
         \x20       }}\n\
         \x20       match string.try_to_float(\"1.2.3\") {{\n\
         \x20           Ok(_) => {{ return 113; }},\n\
         \x20           Err(_) => {{ checks = checks + 1; }},\n\
         \x20       }}\n\
         \x20       match string.try_to_float(\"9e2ms\") {{\n\
         \x20           Ok(_) => {{ return 114; }},\n\
         \x20           Err(_) => {{ checks = checks + 1; }},\n\
         \x20       }}\n\
         \x20       match string.try_to_float(\"1e2e3\") {{\n\
         \x20           Ok(_) => {{ return 115; }},\n\
         \x20           Err(_) => {{ checks = checks + 1; }},\n\
         \x20       }}\n\
         \x20       match string.try_to_float(\"-\") {{\n\
         \x20           Ok(_) => {{ return 116; }},\n\
         \x20           Err(_) => {{ checks = checks + 1; }},\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   if checks == {expected_checks} {{ 0 }} else {{ 117 }}\n\
         }}\n"
    )
}

#[test]
fn valid_decimal_parse_has_no_per_character_leak_slope() {
    assert_frame_slope_below_tolerance("try_to_float_valid_decimal", valid_decimal_source);
}

#[test]
fn varied_valid_and_invalid_parses_have_no_per_character_leak_slope() {
    assert_frame_slope_below_tolerance("try_to_float_varied", varied_float_source);
}
