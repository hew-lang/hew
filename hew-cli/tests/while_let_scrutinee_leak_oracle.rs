//! Leak and exactly-once guards for from-call `while let` scrutinee owners.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn successful_iterations_source(frames: usize) -> String {
    format!(
        "fn next(i: i64, cap: i64) -> Result<string, string> {{\n\
         \x20   if i < cap {{ Ok(\"while-let-ok\".to_upper()) }} else {{ Err(\"done\".to_upper()) }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var i = 0;\n\
         \x20   var total = 0;\n\
         \x20   while let Ok(value) = next(i, {frames}) {{\n\
         \x20       total = total + value.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

fn final_false_source(frames: usize) -> String {
    format!(
        "fn next() -> Result<string, string> {{ Err(\"while-let-false\".to_upper()) }}\n\
         fn one() {{\n\
         \x20   while let Ok(value) = next() {{ print(value); }}\n\
         }}\n\
         fn main() {{\n\
         \x20   for i in 0..{frames} {{ one(); }}\n\
         }}\n"
    )
}

const EXIT_EDGES_SCRIBBLE_SOURCE: &str = "\
fn next(i: i64) -> Result<string, string> {\n\
\x20   if i < 4 { Ok(\"edge-payload\".to_upper()) } else { Err(\"done\".to_upper()) }\n\
}\n\
\n\
fn run_continue() {\n\
\x20   var i = 0;\n\
\x20   while let Ok(value) = next(i) {\n\
\x20       i = i + 1;\n\
\x20       if value.len() > 0 { continue; }\n\
\x20   }\n\
}\n\
\n\
fn run_break() {\n\
\x20   var i = 0;\n\
\x20   while let Ok(value) = next(i) {\n\
\x20       if value.len() > 0 { break; }\n\
\x20   }\n\
}\n\
\n\
fn run_return() -> i64 {\n\
\x20   while let Ok(value) = next(0) {\n\
\x20       if value.len() > 0 { return 1; }\n\
\x20   }\n\
\x20   0\n\
}\n\
\n\
fn main() {\n\
\x20   run_continue();\n\
\x20   run_break();\n\
\x20   if run_return() == 1 { print(\"ok\"); }\n\
}\n";

#[test]
fn while_let_successful_scrutinees_have_flat_leak_slope() {
    assert_frame_slope_below_tolerance("while_let_success", successful_iterations_source);
}

#[test]
fn while_let_final_false_scrutinee_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("while_let_false", final_false_source);
}

#[test]
fn while_let_break_continue_and_return_release_exactly_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("while-let-scrutinee-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        EXIT_EDGES_SCRIBBLE_SOURCE,
        dir.path(),
        "while_let_exit_edges",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "while-let edge cleanup must not double-free or read freed payloads:\n{}",
        describe_output(&output)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "ok");
}
