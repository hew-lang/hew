//! Exact ownership oracle for nested-enum payloads handed to a shorter-lived
//! arm-local owner before a second match.
//!
//! The direct nested-match controls already retained their outer tag-aware
//! drop. The leak lived in the whole-local, record-field, and tuple-field
//! forwarding shapes: lexical child scopes were mistaken for escaping scopes,
//! suppressing every final owner. The allocator checks pin exact zero at low
//! and high iteration counts, while the MIR checks identify which local keeps
//! the sole drop authority. The poisoned-allocator run guards the opposite
//! regression: admitting both a parent alias and its forwarded child.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    compile_to_native, measure_leaks_exact, run_probe_witness, run_under_malloc_scribble,
    HIGH_FRAMES, LOW_FRAMES,
};
use support::{describe_output, require_codegen};

const SOURCE_TEMPLATE: &str = r#"
enum Inner {
    Text(string),
    Empty,
}

enum Outer {
    Wrap(Inner),
    Empty,
}

enum Middle {
    Wrap(Inner),
    Empty,
}

enum DeepOuter {
    Wrap(Middle),
    Empty,
}

type Holder {
    value: Inner,
    sibling: i64,
}

enum RecordOuter {
    Wrap(Holder),
    Empty,
}

enum TupleOuter {
    Wrap((Inner, i64)),
    Empty,
}

fn whole(i: i64) -> i64 {
    let outer = Outer.Wrap(Inner.Text(f"whole-{i}"));
    match outer {
        .Wrap(st) => {
            let w = st;
            match w {
                .Text(s) => s.len(),
                .Empty => 0,
            }
        },
        .Empty => 0,
    }
}

fn direct(i: i64) -> i64 {
    let outer = Outer.Wrap(Inner.Text(f"direct-{i}"));
    match outer {
        .Wrap(st) => match st {
            .Text(s) => s.len(),
            .Empty => 0,
        },
        .Empty => 0,
    }
}

fn depth_two(i: i64) -> i64 {
    let outer = DeepOuter.Wrap(Middle.Wrap(Inner.Text(f"deep-{i}")));
    match outer {
        .Wrap(mid) => match mid {
            .Wrap(st) => match st {
                .Text(s) => s.len(),
                .Empty => 0,
            },
            .Empty => 0,
        },
        .Empty => 0,
    }
}

fn record_field(i: i64) -> i64 {
    let outer = RecordOuter.Wrap(Holder {
        value: Inner.Text(f"record-{i}"),
        sibling: i,
    });
    match outer {
        .Wrap(st) => {
            let w = st.value;
            match w {
                .Text(s) => s.len() + st.sibling,
                .Empty => st.sibling,
            }
        },
        .Empty => 0,
    }
}

fn tuple_field(i: i64) -> i64 {
    let outer = TupleOuter.Wrap((Inner.Text(f"tuple-{i}"), i));
    match outer {
        .Wrap(st) => {
            let w = st.0;
            match w {
                .Text(s) => s.len() + st.1,
                .Empty => st.1,
            }
        },
        .Empty => 0,
    }
}

fn main() -> i64 {
    var total = 0;
    for i in 0..__FRAMES__ {
        total = total + direct(i) + depth_two(i) + whole(i)
            + record_field(i) + tuple_field(i);
        println("frame");
    }
    if total > 0 { 0 } else { 91 }
}
"#;

fn source(frames: usize) -> String {
    SOURCE_TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

// Lost coverage: `raw_and_elaborated_mir_pin_forwarding_shapes_and_single_drop_authority`
// used `--dump-mir raw`/`elab` (both retired) to pin the move/neutralization
// text visible pre-elaboration and to count post-elaboration tag-aware
// drop-local occurrences per function section for the whole-local,
// record-field and tuple-field forwarding shapes. Physical MIR's structured
// (Debug) dump has no equivalent single-line text to grep or count per
// function, so this MIR-emission coverage has no direct replacement. The
// same leak/double-free behaviour for the same shapes is still proven end to
// end below by the leak-slope and malloc-scribble oracles in this file.

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact leak oracle needs macOS leaks(1); absent capability must be a counted skip"
)]
#[test]
fn low_and_high_whole_forward_handoffs_are_exactly_leak_clean() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("nested-enum-whole-forward-leaks-")
        .tempdir()
        .expect("tempdir");
    for frames in [LOW_FRAMES, HIGH_FRAMES] {
        let bin = compile_to_native(
            &source(frames),
            dir.path(),
            &format!("nested_enum_whole_forward_{frames}"),
        );
        assert_eq!(
            run_probe_witness(&bin, &[]),
            frames,
            "the exact leak sample must execute every requested helper frame"
        );
        assert_eq!(
            measure_leaks_exact(&bin),
            (0, 0),
            "{frames} helper frames must release every direct-local, record-field, and \
             tuple-field forwarded String"
        );
    }
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the deterministic poisoned-allocator contract is macOS-only"
)]
#[test]
fn high_whole_forward_handoffs_do_not_double_free_or_read_poison() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("nested-enum-whole-forward-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &source(HIGH_FRAMES),
        dir.path(),
        "nested_enum_whole_forward_scribble",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "all forwarded payloads must remain live through their final read and release exactly \
         once:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout)
            .lines()
            .filter(|line| *line == "frame")
            .count(),
        HIGH_FRAMES,
        "the poisoned run must execute every direct and forwarded helper"
    );
}
