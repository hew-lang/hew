//! Ownership gates for the concrete `std::pipeline` S1 chain.
//!
//! Every source uses `pipeline.PipelineItemI64`, whose `label: string` crosses
//! the source, stage, and sink mailboxes. The stage also constructs a new label,
//! exercising the transform input-to-output ownership edge.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn source_with_frames(template: &str, frames: usize) -> String {
    template.replace("__FRAMES__", &frames.to_string())
}

fn expected_lines(frames: usize) -> usize {
    frames
}

fn normal_source(frames: usize) -> String {
    source_with_frames(
        r#"import std.pipeline;

fn main() -> i64 {
    let source = pipeline.run(pipeline.from(__FRAMES__));
    var i: i64 = 0;
    while i < __FRAMES__ {
        let item: pipeline.PipelineItemI64 = PipelineItemI64 {
            value: i,
            label: f"normal-owned-{i}",
            crash_stage: false,
        };
        match await source.push(item) {
            Ok(admitted) => { if !admitted { return 2; } },
            Err(_) => { return 3; },
        }
        println("normal");
        i = i + 1;
    }
    match await source.shutdown(__FRAMES__) {
        Ok(drained) => if drained { 0 } else { 4 },
        Err(_) => 5,
    }
}
"#,
        frames,
    )
}

fn cancellation_source(frames: usize) -> String {
    source_with_frames(
        r#"import std.pipeline;

fn item(value: i64, label: string) -> pipeline.PipelineItemI64 {
    PipelineItemI64 { value: value, label: label, crash_stage: false }
}

fn main() -> i64 {
    let source = pipeline.run(pipeline.from(0));
    let control = match await source.control_handle() {
        Ok(value) => value,
        Err(_) => { return 10; },
    };
    match await source.push(item(-2, "cancel-seed-one")) {
        Ok(admitted) => { if !admitted { return 11; } },
        Err(_) => { return 12; },
    }
    match await source.push(item(-1, "cancel-seed-two")) {
        Ok(admitted) => { if !admitted { return 13; } },
        Err(_) => { return 14; },
    }

    var i: i64 = 0;
    while i < __FRAMES__ {
        let outcome = select {
            reply from source.push(item(i, f"cancel-owned-{i}")) => if reply { 1 } else { -1 },
            after 1ms => 0,
        };
        if outcome != 0 {
            return 15;
        }
        control.release();
        var posts: i64 = 0;
        while posts < i + 3 {
            match await control.post_sends() {
                Ok(value) => { posts = value; },
                Err(_) => { return 16; },
            }
            if posts < i + 3 {
                sleep(1ms);
            }
        }
        println("cancelled");
        i = i + 1;
    }

    control.release();
    control.release();
    match await source.shutdown(__FRAMES__ + 2) {
        Ok(drained) => if drained { 0 } else { 17 },
        Err(_) => 18,
    }
}
"#,
        frames,
    )
}

fn crash_source(frames: usize) -> String {
    source_with_frames(
        r#"import std.pipeline;

fn main() -> i64 {
    var i: i64 = 0;
    while i < __FRAMES__ {
        let source = pipeline.run(pipeline.from(1));
        let item: pipeline.PipelineItemI64 = PipelineItemI64 {
            value: i,
            label: f"crash-owned-{i}",
            crash_stage: true,
        };
        match await source.push(item) {
            Ok(admitted) => { if admitted { return 20; } },
            Err(_) => { return 21; },
        }
        match await source.count() {
            Ok(count) => { if count != 0 { return 22; } },
            Err(_) => { return 23; },
        }
        println("crashed");
        i = i + 1;
    }
    0
}
"#,
        frames,
    )
}

fn shutdown_drain_source(frames: usize) -> String {
    source_with_frames(
        r#"import std.pipeline;

fn main() -> i64 {
    let source = pipeline.run(pipeline.from(__FRAMES__));
    var i: i64 = 0;
    while i < __FRAMES__ {
        let item: pipeline.PipelineItemI64 = PipelineItemI64 {
            value: i,
            label: f"drain-owned-{i}",
            crash_stage: false,
        };
        match await source.push(item) {
            Ok(admitted) => { if !admitted { return 30; } },
            Err(_) => { return 31; },
        }
        println("queued");
        i = i + 1;
    }
    match await source.shutdown(__FRAMES__) {
        Ok(drained) => if drained { 0 } else { 32 },
        Err(_) => 33,
    }
}
"#,
        frames,
    )
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn pipeline_owned_payload_normal_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "pipeline_owned_normal",
        normal_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn pipeline_owned_payload_cancellation_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "pipeline_owned_cancellation",
        cancellation_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn pipeline_owned_payload_crash_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "pipeline_owned_crash",
        crash_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn pipeline_owned_payload_shutdown_drain_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "pipeline_owned_shutdown_drain",
        shutdown_drain_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "poisoned allocator gate needs the Darwin malloc diagnostics"
)]
#[test]
fn pipeline_owned_payload_lifecycle_edges_do_not_double_free() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("pipeline-owned-double-free-")
        .tempdir()
        .expect("tempdir");

    for (name, source) in [
        ("normal", normal_source(20)),
        ("cancellation", cancellation_source(20)),
        ("crash", crash_source(20)),
        ("shutdown_drain", shutdown_drain_source(20)),
    ] {
        let bin = compile_to_native(&source, dir.path(), name);
        let output = run_under_malloc_scribble(&bin);
        assert!(
            output.status.success(),
            "pipeline {name} ownership edge aborted under the poisoned allocator:\n{}",
            describe_output(&output)
        );
    }
}
