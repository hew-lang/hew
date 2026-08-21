//! Leak and double-close oracle for resources bound by `if let`.
//!
//! The implicit-close probe opens one file sink per loop iteration and leaves
//! the binder live at the end of the conditional scope. Its LOW/HIGH leak-node
//! slope must stay flat, proving the drop plan closes every matched resource.
//! The explicit-close probe consumes the same binder and runs under the Darwin
//! poisoned allocator; any scope-exit close left behind would free the sink a
//! second time and abort.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn if_let_sink_source(frames: usize, explicit_close: bool) -> String {
    let close = if explicit_close {
        "        sink.close();\n"
    } else {
        ""
    };
    format!(
        "import std.stream;\n\
         fn main() -> i64 {{\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       if let .Ok(sink) = stream.to_file(\"/tmp/hew-if-let-resource-oracle.txt\") {{\n\
         \x20           sink.write(\"frame\");\n\
         {close}\
         \x20           println(\"closed\");\n\
         \x20       }} else {{\n\
         \x20           return 70;\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn implicit_close_source(frames: usize) -> String {
    if_let_sink_source(frames, false)
}

fn expected_lines(frames: usize) -> usize {
    frames
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn if_let_resource_implicit_close_has_no_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "if_let_resource_implicit_close",
        implicit_close_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn if_let_resource_explicit_close_is_not_closed_twice() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("if-let-resource-double-close-")
        .tempdir()
        .expect("tempdir");
    let source = if_let_sink_source(200, true);
    let bin = compile_to_native(&source, dir.path(), "explicit_close");
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "an explicitly closed `if let` resource must not retain a second scope-exit close;\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).lines().count(),
        200,
        "the poisoned-allocator probe must execute every iteration"
    );
}
