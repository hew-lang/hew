//! Regression oracle for an owned record transferred out of a returned tuple.
//!
//! The failing shape repeatedly returned `(Holder, Slot, i64)`, assigned
//! `pair.0` back into a loop-carried `Holder`, and matched `pair.1`. The tuple
//! field load was conservatively classified as an interior alias even though
//! lowering cleared the transferred source slot. That taint propagated through
//! the assignment and suppressed the final `Holder` drop. A `Holder` containing
//! `Vec<string> { "first", "second" }` leaked four nodes / 208 bytes per helper
//! frame while the record-only and direct-Vec allocation controls stayed clean.
//!
//! Each slope fixture uses a returning helper so leaked storage is no longer
//! reachable from `main`'s final stack frame. Both the payload and empty enum
//! arms execute, and a second helper call takes an early `break` with an empty
//! Vec. The poisoned-allocator pin reads every live field and the scalar tuple
//! sibling after the last reassignment, catching an over-drop or an incorrect
//! tuple-field neutralization.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, measure_leaks_exact,
    run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

const TUPLE_RECORD_ENUM_TEMPLATE: &str = r#"
record Holder {
    items: Vec<string>,
    tag: string,
}

enum Slot {
    Filled(i64);
    Empty;
}

fn step(h: Holder, i: i64) -> (Holder, Slot, i64) {
    if i < 2 {
        let next = Holder { items: h.items, tag: f"tag-{i}" };
        (next, Filled(i), 100 + i)
    } else {
        (h, Empty, 100 + i)
    }
}

fn run_case(seed: i64, fill: bool, exit_early: bool) -> i64 {
    var owner = Holder { items: Vec::new(), tag: f"seed-{seed}" };
    if fill {
        owner.items.push("first");
        owner.items.push("second");
    }

    var i = 0;
    var checksum = 0;
    var done = false;
    while !done {
        let pair = step(owner, i);
        owner = pair.0;
        checksum = checksum + pair.2;
        match pair.1 {
            Filled(v) => {
                checksum = checksum + v;
                if exit_early {
                    break;
                }
            },
            Empty => {
                done = true;
            },
        }
        i = i + 1;
    }
    checksum + owner.items.len() + owner.tag.len()
}

fn main() -> i64 {
    var total = 0;
    for frame in 0..__FRAMES__ {
        total = total + run_case(frame, true, false);
        total = total + run_case(frame, false, true);
        println("frame");
    }
    if total >= 0 { 0 } else { 91 }
}
"#;

const RECORD_ONLY_CONTROL_SOURCE: &str = r#"
record Holder {
    items: Vec<string>,
    tag: string,
}

fn step(h: Holder, i: i64) -> Holder {
    if i < 2 {
        Holder { items: h.items, tag: f"tag-{i}" }
    } else {
        h
    }
}

fn main() {
    var owner = Holder { items: Vec::new(), tag: "seed" };
    owner.items.push("first");
    owner.items.push("second");
    for i in 0..3 {
        owner = step(owner, i);
    }
    println(f"{owner.tag}|{owner.items.len()}");
}
"#;

const VEC_ONLY_CONTROL_SOURCE: &str = r#"
fn main() {
    var items = Vec::new();
    items.push("first");
    items.push("second");
    println(f"len={items.len()}");
}
"#;

const NO_DOUBLE_FREE_SOURCE: &str = r#"
record Holder {
    items: Vec<string>,
    tag: string,
}

enum Slot {
    Filled(i64);
    Empty;
}

fn step(h: Holder, i: i64) -> (Holder, Slot, i64) {
    if i < 2 {
        let next = Holder { items: h.items, tag: f"tag-{i}" };
        (next, Filled(i), 100 + i)
    } else {
        (h, Empty, 100 + i)
    }
}

fn main() {
    var owner = Holder { items: Vec::new(), tag: "seed" };
    owner.items.push("first");
    owner.items.push("second");

    var i = 0;
    var checksum = 0;
    var sibling = 0;
    var done = false;
    while !done {
        let pair = step(owner, i);
        owner = pair.0;
        sibling = pair.2;
        match pair.1 {
            Filled(v) => {
                checksum = checksum + v;
            },
            Empty => {
                done = true;
            },
        }
        i = i + 1;
    }

    println(f"{owner.tag}|{owner.items[0]}|{owner.items[1]}|{checksum}|{sibling}");
}
"#;

const FORWARD_REBIND_SIBLING_SOURCE: &str = r#"
record Holder {
    items: Vec<string>,
    tag: string,
}

fn main() {
    let pair = (Holder { items: ["transferred"], tag: "tag" }, [1, 2]);
    var owner = Holder { items: [], tag: "" };
    owner = pair.0;
    let rebound = owner;
    if rebound.items[0] != "transferred" || pair.1.len() != 2 {
        return;
    }
    print("OK");
}
"#;

const FORWARD_REBIND_CONTROL_SOURCE: &str = r#"
record Holder {
    items: Vec<string>,
    tag: string,
}

fn main() {
    let pair = (Holder { items: ["transferred"], tag: "tag" }, [1, 2]);
    if pair.0.items[0] != "transferred" || pair.1.len() != 2 {
        return;
    }
    print("OK");
}
"#;

fn source_from(template: &str, frames: usize) -> String {
    template.replace("__FRAMES__", &frames.to_string())
}

fn tuple_record_enum_source(frames: usize) -> String {
    source_from(TUPLE_RECORD_ENUM_TEMPLATE, frames)
}

fn expected_lines(frames: usize) -> usize {
    frames
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn transferred_record_from_tuple_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "tuple_record_enum_transfer",
        tuple_record_enum_source,
        expected_lines,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn record_only_and_direct_vec_controls_are_exactly_clean() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("tuple-record-enum-controls-")
        .tempdir()
        .expect("tempdir");
    let record_bin = compile_to_native(
        RECORD_ONLY_CONTROL_SOURCE,
        dir.path(),
        "record_only_control",
    );
    let vec_bin = compile_to_native(VEC_ONLY_CONTROL_SOURCE, dir.path(), "direct_vec_control");

    let record_output = run_under_malloc_scribble(&record_bin);
    let vec_output = run_under_malloc_scribble(&vec_bin);
    assert!(
        record_output.status.success(),
        "record-only control must run cleanly:\n{}",
        describe_output(&record_output)
    );
    assert_eq!(
        String::from_utf8_lossy(&record_output.stdout),
        "tag-1|2\n",
        "record-only control must preserve its final fields"
    );
    assert!(
        vec_output.status.success(),
        "direct Vec control must run cleanly:\n{}",
        describe_output(&vec_output)
    );
    assert_eq!(
        String::from_utf8_lossy(&vec_output.stdout),
        "len=2\n",
        "direct Vec control must preserve both elements"
    );

    let record_leaks = measure_leaks_exact(&record_bin);
    let vec_leaks = measure_leaks_exact(&vec_bin);
    assert_eq!(
        record_leaks,
        (0, 0),
        "record-only reassignment is the clean ownership control"
    );
    assert_eq!(
        vec_leaks,
        (0, 0),
        "a directly owned Vec is the clean runtime allocation control"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator contract is macOS-only; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn transferred_record_and_tuple_siblings_remain_live_until_single_drop() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("tuple-record-enum-container-drop-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(NO_DOUBLE_FREE_SOURCE, dir.path(), "no_double_free");
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "the transferred record must remain live and release exactly once; a crash indicates \
         a poisoned read, double-free, or wrong tuple-field neutralization:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "tag-1|first|second|1|102\n",
        "final record fields, enum payload sum, and scalar tuple sibling must survive every \
         reassignment before the single final drop:\n{}",
        describe_output(&output)
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator contract is macOS-only; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn forward_rebind_transfer_drops_transferred_field_once_and_sibling_fields() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("tuple-record-forward-rebind-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        FORWARD_REBIND_SIBLING_SOURCE,
        dir.path(),
        "forward_rebind_sibling",
    );
    let control_bin = compile_to_native(
        FORWARD_REBIND_CONTROL_SOURCE,
        dir.path(),
        "forward_rebind_control",
    );
    let output = run_under_malloc_scribble(&bin);
    let control_output = run_under_malloc_scribble(&control_bin);

    assert!(
        output.status.success(),
        "forward rebind of a corroborated projection transfer must not double-free:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "OK",
        "all transferred and sibling values must remain readable before one final drop"
    );
    assert!(
        control_output.status.success(),
        "the non-transfer control must run cleanly:\n{}",
        describe_output(&control_output)
    );
    assert_eq!(
        String::from_utf8_lossy(&control_output.stdout),
        "OK",
        "the allocation-equivalent control must complete its visible work witness"
    );
    assert_eq!(
        measure_leaks_exact(&bin),
        (0, 0),
        "the transferred field and tuple sibling fields must release exactly once"
    );
    assert_eq!(
        measure_leaks_exact(&control_bin),
        (0, 0),
        "the allocation-equivalent control must release every allocation"
    );
}
