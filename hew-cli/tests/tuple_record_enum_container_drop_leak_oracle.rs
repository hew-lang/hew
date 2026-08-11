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
use support::{describe_output, hew_binary, repo_root, require_codegen};

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

const FORWARD_REBIND_HELPER_SIBLING_SOURCE: &str = r#"
record Holder {
    items: Vec<string>,
    tag: string,
}

fn helper(i: i64) -> i64 {
    let pair: (Holder, Vec<string>) = (
        Holder { items: ["transferred"], tag: "tag" },
        ["left", "right"],
    );
    var owner = Holder { items: [], tag: "" };
    owner = pair.0;
    let rebound = owner;
    if rebound.items[0] != "transferred"
        || rebound.tag != "tag"
        || pair.1[0] != "left"
        || pair.1[1] != "right"
        || pair.1.len() != 2 {
        return -1000;
    }
    rebound.items[0].len() + rebound.tag.len() + pair.1[0].len() + pair.1[1].len() + pair.1.len() + i
}

fn main() -> i64 {
    var checksum = 0;
    for frame in 0..64 {
        checksum = checksum + helper(frame);
    }
    println(f"checksum={checksum}");
    if checksum == 3616 { 0 } else { 91 }
}
"#;

const ENUM_CARRIER_FORWARD_REBIND_SOURCE: &str = r#"
record Payload {
    values: Vec<string>,
    label: string,
}

fn helper(i: i64) -> i64 {
    let pair: (Option<Payload>, Vec<string>) = (
        Some(Payload {
            values: ["transferred"],
            label: "tag",
        }),
        ["left", "right"],
    );

    var carrier: Option<Payload> = None;
    if i % 2 == 0 {
        carrier = pair.0;
    } else {
        carrier = None;
    }

    let rebound = carrier;
    let payload_score = match rebound {
        Some(payload) => {
            if payload.values[0] != "transferred"
                || payload.label != "tag"
                || payload.values.len() != 1 {
                -100000
            } else {
                payload.values[0].len() + payload.label.len() + payload.values.len()
            }
        },
        None => {
            7
        },
    };

    let sibling_score =
        if pair.1[0] != "left" || pair.1[1] != "right" || pair.1.len() != 2 {
            -200000
        } else {
            pair.1[0].len() + pair.1[1].len() + pair.1.len()
        };
    payload_score + sibling_score + i
}

fn main() -> i64 {
    var checksum = 0;
    for frame in 0..64 {
        checksum = checksum + helper(frame);
    }
    println(f"checksum={checksum}");
    if checksum == 3424 { 0 } else { 91 }
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

fn dump_elaborated_mir(source: &str, name: &str) -> String {
    let dir = tempfile::Builder::new()
        .prefix("tuple-record-enum-elab-")
        .tempdir()
        .expect("tempdir");
    let path = dir.path().join(format!("{name}.hew"));
    std::fs::write(&path, source).expect("write Hew source");
    let output = std::process::Command::new(hew_binary())
        .args([
            "compile",
            "--dump-mir",
            "elab",
            path.to_str().expect("Hew source path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("run hew compile --dump-mir elab");
    assert!(
        output.status.success(),
        "elaborated MIR dump failed:\n{}",
        describe_output(&output)
    );
    String::from_utf8(output.stdout).expect("MIR dump is UTF-8")
}

fn enum_carrier_one_frame_source(frame: i64, expected: i64) -> String {
    let helper = ENUM_CARRIER_FORWARD_REBIND_SOURCE
        .split("\nfn main()")
        .next()
        .expect("fixture helper section");
    format!(
        "{helper}\n\nfn main() -> i64 {{\n    let checksum = helper({frame});\n    println(f\"checksum={{checksum}}\");\n    if checksum == {expected} {{ 0 }} else {{ 91 }}\n}}\n"
    )
}

#[test]
fn empty_enum_carrier_projection_forwarding_keeps_tuple_and_enum_drop_authorities() {
    let dump = dump_elaborated_mir(
        ENUM_CARRIER_FORWARD_REBIND_SOURCE,
        "enum_carrier_forward_rebind",
    );
    let helper = dump
        .split("fn helper")
        .nth(1)
        .and_then(|section| section.split("\nfn main").next())
        .expect("helper elaborated MIR section");

    assert!(
        helper.contains("ty=(Option<Payload>, Vec<string>) kind=tuple_in_place"),
        "the terminal plan must retain TupleInPlace for pair after the exact empty-enum carrier \
         overwrite:\n{helper}"
    );
    assert!(
        helper.contains("ty=Option<Payload> kind=enum_in_place"),
        "the forwarded enum owner must retain its active Payload Vec release authority:\n{helper}"
    );
    assert!(
        !helper.contains("kind=record_in_place"),
        "the payload alias must not receive a separate RecordInPlace drop:\n{helper}"
    );
    let helper_lines: Vec<_> = helper.lines().collect();
    for exit in ["cancel[", "panic[", "return["] {
        assert!(
            helper_lines.iter().enumerate().any(|(index, line)| {
                if !line.trim_start().starts_with(exit) {
                    return false;
                }
                let drops: Vec<_> = helper_lines[index + 1..]
                    .iter()
                    .take_while(|drop| drop.starts_with("      "))
                    .copied()
                    .collect();
                drops
                    .iter()
                    .any(|drop| drop.contains("ty=Option<Payload> kind=enum_in_place"))
                    && drops.iter().any(|drop| {
                        drop.contains("ty=(Option<Payload>, Vec<string>) kind=tuple_in_place")
                    })
            }),
            "an active forwarded payload must keep both its enum authority and the residual \
             tuple-sibling authority on a {exit} exit:\n{helper}"
        );
    }
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
fn helper_frame_forward_rebind_releases_transferred_and_sibling_vecs() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("tuple-record-forward-rebind-helper-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        FORWARD_REBIND_HELPER_SIBLING_SOURCE,
        dir.path(),
        "forward_rebind_helper_sibling",
    );
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "forward rebind of a corroborated projection transfer must not double-free or \
         poison the helper's transferred payload or sibling Vec:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "checksum=3616\n",
        "64 helper calls must read both the transferred data and sibling Vec before \
         producing the exact work checksum"
    );
    assert_eq!(
        measure_leaks_exact(&bin),
        (0, 0),
        "each helper frame must release the transferred data and sibling Vec allocations"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the poisoned allocator contract is macOS-only; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn empty_enum_carrier_helper_releases_payload_and_tuple_sibling_on_both_paths() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("tuple-enum-carrier-forward-rebind-helper-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        ENUM_CARRIER_FORWARD_REBIND_SOURCE,
        dir.path(),
        "enum_carrier_forward_rebind_helper",
    );
    let even_bin = compile_to_native(
        &enum_carrier_one_frame_source(0, 26),
        dir.path(),
        "enum_carrier_forward_rebind_even",
    );
    let odd_bin = compile_to_native(
        &enum_carrier_one_frame_source(1, 19),
        dir.path(),
        "enum_carrier_forward_rebind_odd",
    );
    let output = run_under_malloc_scribble(&bin);
    let even_output = run_under_malloc_scribble(&even_bin);
    let odd_output = run_under_malloc_scribble(&odd_bin);

    assert!(
        output.status.success(),
        "the even transfer and odd non-transfer helper paths must survive poisoned allocation \
         without a double-free or use-after-free:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "checksum=3424\n",
        "64 helper frames must execute both paths and produce the exact work checksum"
    );
    assert_eq!(
        measure_leaks_exact(&bin),
        (0, 0),
        "the tuple sibling Vec and the active enum Payload Vec must each release exactly once"
    );
    for (name, output, expected) in [
        ("even", &even_output, "checksum=26\n"),
        ("odd", &odd_output, "checksum=19\n"),
    ] {
        assert!(
            output.status.success(),
            "{name} one-frame helper must exit successfully:\n{}",
            describe_output(output)
        );
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            expected,
            "{name} one-frame helper must print its exact checksum"
        );
    }
    assert_eq!(measure_leaks_exact(&even_bin), (0, 0));
    assert_eq!(measure_leaks_exact(&odd_bin), (0, 0));
}
