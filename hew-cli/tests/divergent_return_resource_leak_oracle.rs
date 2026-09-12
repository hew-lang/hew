//! Divergent returned-resource ownership oracles.
//!
//! Returned-member discovery is intentionally flow-insensitive: every owned
//! member reaching the return slot is removed from ordinary scope teardown.
//! That prevents a callee from closing a resource transferred to its caller,
//! but it used to strand the unselected members of a divergent return:
//!
//! ```text
//! if c { (s1, r1) } else { (s2, r2) }
//! ```
//!
//! The per-member transfer-block map already distinguishes those arms. These
//! tests pin the missing affine-resource half of its path-sensitive
//! re-admission: the selected pair transfers, while the unselected pair closes
//! exactly once on its arm-to-join edge. Tuple and record returns get allocator
//! slopes, raw/elaborated MIR checks, and a poisoned-allocator opposite-
//! direction pin. An early `Result::Err` return covers cleanup before either
//! resource pair enters the return flow.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

const DIVERGENT_TUPLE_TEMPLATE: &str = r#"
import std.stream;

fn choose(c: bool) -> (Sink<string>, Stream<string>) {
    let (s1, r1) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let (s2, r2) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    if c { (s1, r1) } else { (s2, r2) }
}

fn main() {
    for i in 0..__FRAMES__ {
        let (sink, input) = choose(i % 2 == 0);
        sink.close();
        input.close();
        println("frame");
    }
}
"#;

const DIVERGENT_RECORD_TEMPLATE: &str = r#"
import std.stream;

type Pipe {
    sink: Sink<string>,
    input: Stream<string>,
}

fn choose(c: bool) -> Pipe {
    let (s1, r1) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let (s2, r2) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    if c {
        Pipe { sink: s1, input: r1 }
    } else {
        Pipe { sink: s2, input: r2 }
    }
}

fn main() {
    for i in 0..__FRAMES__ {
        let selected = choose(i % 2 != 0);
        selected.sink.close();
        selected.input.close();
        println("frame");
    }
}
"#;

const EARLY_ERROR_TEMPLATE: &str = r#"
import std.stream;

type Pipe {
    sink: Sink<string>,
    input: Stream<string>,
}

fn choose_or_error(bail: bool, c: bool) -> Result<Pipe, string> {
    let (s1, r1) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let (s2, r2) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    if bail {
        return Err("bailed");
    }
    if c {
        Ok(Pipe { sink: s1, input: r1 })
    } else {
        Ok(Pipe { sink: s2, input: r2 })
    }
}

fn main() {
    for i in 0..__FRAMES__ {
        match choose_or_error(true, i % 2 == 0) {
            .Ok(unexpected) => {
                unexpected.sink.close();
                unexpected.input.close();
            },
            .Err(_) => {},
        }
        println("frame");
    }
}
"#;

const COMBINED_TEMPLATE: &str = r#"
import std.stream;

type Pipe {
    sink: Sink<string>,
    input: Stream<string>,
}

fn divergent_tuple(c: bool) -> (Sink<string>, Stream<string>) {
    let (s1, r1) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let (s2, r2) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    if c { (s1, r1) } else { (s2, r2) }
}

fn divergent_record(c: bool) -> Pipe {
    let (s1, r1) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let (s2, r2) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    if c {
        Pipe { sink: s1, input: r1 }
    } else {
        Pipe { sink: s2, input: r2 }
    }
}

fn same_tuple(c: bool) -> (Sink<string>, Stream<string>) {
    let (s1, r1) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let (s2, r2) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    if c { (s1, r1) } else { (s1, r1) }
}

fn error_before_transfer(bail: bool, c: bool) -> Result<Pipe, string> {
    let (s1, r1) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let (s2, r2) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    if bail {
        return Err("bailed");
    }
    if c {
        Ok(Pipe { sink: s1, input: r1 })
    } else {
        Ok(Pipe { sink: s2, input: r2 })
    }
}

fn cancel_before_transfer(c: bool) -> (Sink<string>, Stream<string>) {
    let (s1, r1) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let (s2, r2) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
    var i = 0;
    while i < 1 {
        i = i + 1;
    }
    if c { (s1, r1) } else { (s2, r2) }
}

fn main() {
    for i in 0..__FRAMES__ {
        let (sink1, input1) = divergent_tuple(i % 2 == 0);
        sink1.close();
        input1.close();

        let selected = divergent_record(i % 2 != 0);
        selected.sink.close();
        selected.input.close();

        let (sink2, input2) = same_tuple(i % 2 == 0);
        sink2.close();
        input2.close();

        match error_before_transfer(true, i % 2 == 0) {
            .Ok(unexpected) => {
                unexpected.sink.close();
                unexpected.input.close();
            },
            .Err(_) => {},
        }
        match error_before_transfer(false, i % 2 != 0) {
            .Ok(chosen) => {
                chosen.sink.close();
                chosen.input.close();
            },
            .Err(_) => {},
        }
        let (sink3, input3) = cancel_before_transfer(i % 2 == 0);
        sink3.close();
        input3.close();
        println("frame");
    }
}
"#;

fn with_frames(template: &str, frames: usize) -> String {
    template.replace("__FRAMES__", &frames.to_string())
}

fn divergent_tuple_source(frames: usize) -> String {
    with_frames(DIVERGENT_TUPLE_TEMPLATE, frames)
}

fn divergent_record_source(frames: usize) -> String {
    with_frames(DIVERGENT_RECORD_TEMPLATE, frames)
}

fn early_error_source(frames: usize) -> String {
    with_frames(EARLY_ERROR_TEMPLATE, frames)
}

fn expected_lines(frames: usize) -> usize {
    frames
}

// Lost coverage: `raw_and_elaborated_mir_attribute_only_unselected_resource_members`
// used `--dump-mir raw`/`elab` (both retired) to count constructor and
// `SinkClose`/`StreamClose` release-authority occurrences per function
// section across divergent-return, same-member, early-error and
// pre-transfer-cancellation shapes. Physical MIR's structured (Debug) dump
// has no equivalent single-line text to grep or count per function, so this
// MIR-emission coverage has no direct replacement. The same
// leak/double-free behaviour for the same shapes is still proven end to end
// below by the leak-slope and malloc-scribble oracles in this file.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)`; a host without it must record a skip"
)]
#[test]
fn divergent_tuple_resource_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance_exact_lines(
        "divergent_tuple_resource",
        divergent_tuple_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)`; a host without it must record a skip"
)]
#[test]
fn divergent_record_resource_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance_exact_lines(
        "divergent_record_resource",
        divergent_record_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)`; a host without it must record a skip"
)]
#[test]
fn early_error_resource_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance_exact_lines(
        "early_error_resource",
        early_error_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "poisoned allocator is macOS-only; a host without it must record a skip"
)]
#[test]
fn divergent_and_same_member_returns_run_clean_under_malloc_scribble() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("divergent-return-resource-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &with_frames(COMBINED_TEMPLATE, 20),
        dir.path(),
        "divergent_return_resource",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "divergent/same-member return controls must close every local resource \
         exactly once under the poisoned allocator:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).lines().count(),
        20,
        "poisoned-allocator probe must complete every loop iteration:\n{}",
        describe_output(&output)
    );
}
