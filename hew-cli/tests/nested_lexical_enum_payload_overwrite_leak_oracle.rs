//! Ownership oracle for overwriting a local inline enum after a matched payload
//! alias has left its lexical scope.
//!
//! The old active payload belongs to the enum slot. A match binder is only a
//! byte-copy alias, so replacing `Full(string)` with `Empty` must release the
//! slot's old payload after the binder's arm/block has closed. The raw-MIR pin
//! proves that exact tag-aware release. Exact allocator checks prove the
//! release is neither missing nor duplicated across direct, nested-block, and
//! joined control-flow shapes.
//!
//! Live-alias cases overwrite inside the arm and read the binder afterward.
//! Their old payload generation is released through a path-sensitive binder
//! drop at the arm boundary, after the last read; repeated overwrites release
//! each newer parent generation before replacement. The whole-enum forward
//! control proves a neutralized payload transfer remains the sole-owner path,
//! and the scalar enum control proves heap-free variants do not gain ownership
//! churn. Unsupported live aliases reject before codegen.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::{
    compile_to_native, measure_leaks_exact, run_probe_witness, run_under_malloc_scribble,
    HIGH_FRAMES, LOW_FRAMES,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

const SOURCE_TEMPLATE: &str = r#"
enum Box {
    Full(string);
    Empty;
}

enum Inner {
    Text(string);
    Empty;
}

enum Outer {
    Full(Inner);
    Empty;
}

enum ScalarBox {
    Full(i64);
    Empty;
}

fn direct(i: i64) -> i64 {
    var opt = Box.Full(f"direct-{i}");
    var n = 0;
    match opt {
        Full(s) => {
            n = s.len();
        },
        Empty => {},
    }
    opt = Box.Empty;
    n
}

fn nested(i: i64) -> i64 {
    var opt = Box.Full(f"nested-{i}");
    var n = 0;
    {
        match opt {
            Full(s) => {
                n = s.len();
            },
            Empty => {},
        }
    }
    opt = Box.Empty;
    n
}

fn expression_arm(i: i64) -> i64 {
    var opt = Box.Full(f"expression-{i}");
    let n = match opt {
        Full(s) => s.len(),
        Empty => 0,
    };
    opt = Box.Empty;
    n
}

fn joined(i: i64) -> i64 {
    var opt = Box.Full(f"joined-{i}");
    var n = 0;
    if i % 2 == 0 {
        match opt {
            Full(s) => {
                n = s.len();
            },
            Empty => {},
        }
    } else {
        {
            match opt {
                Full(s) => {
                    n = s.len();
                },
                Empty => {},
            }
        }
    }
    opt = Box.Empty;
    n
}

fn fresh_nonempty(i: i64) -> i64 {
    var opt = Box.Full(f"old-{i}");
    opt = Box.Full(f"new-{i}");
    match opt {
        Full(s) => s.len(),
        Empty => 0,
    }
}

fn guarded(i: i64) -> i64 {
    var opt = Box.Full(f"guard-old-{i}");
    if i < 0 {
        let moved = opt;
        return match moved {
            Full(s) => s.len(),
            Empty => 0,
        };
    }
    opt = Box.Full(f"guard-new-{i}");
    match opt {
        Full(s) => s.len(),
        Empty => 0,
    }
}

fn whole_forward(i: i64) -> i64 {
    var opt = Outer.Full(Inner.Text(f"forward-{i}"));
    match opt {
        Full(inner) => {
            let owner = inner;
            opt = Outer.Empty;
            match owner {
                Text(s) => s.len(),
                Empty => 0,
            }
        },
        Empty => 0,
    }
}

fn scalar(i: i64) -> i64 {
    var opt = ScalarBox.Full(i);
    var n = 0;
    match opt {
        Full(v) => {
            n = v;
        },
        Empty => {},
    }
    opt = ScalarBox.Empty;
    n
}

fn main() -> i64 {
    var total = 0;
    for i in 0..__FRAMES__ {
        total = total + direct(i) + nested(i) + expression_arm(i) + joined(i)
            + fresh_nonempty(i) + guarded(i)
            + whole_forward(i) + scalar(i);
        println("frame");
    }
    if total > 0 { 0 } else { 91 }
}
"#;

const LIVE_ALIAS_TEMPLATE: &str = r#"
enum Box {
    Full(string);
    Empty;
}

enum PairBox {
    Both(string, string);
    Empty;
}

fn live_alias(i: i64) -> i64 {
    var opt = Box.Full(f"live-{i}");
    match opt {
        Full(s) => {
            opt = Box.Empty;
            s.len()
        },
        Empty => 0,
    }
}

fn live_alias_return(i: i64) -> i64 {
    var opt = Box.Full(f"return-{i}");
    match opt {
        Full(s) => {
            opt = Box.Empty;
            return s.len();
        },
        Empty => 0,
    }
}

fn repeated_live_alias(i: i64) -> i64 {
    var opt = Box.Full(f"repeat-old-{i}");
    match opt {
        Full(s) => {
            opt = Box.Full(f"repeat-one-{i}");
            opt = Box.Full(f"repeat-two-{i}");
            let old_len = s.len();
            let new_len = match opt {
                Full(current) => current.len(),
                Empty => 0,
            };
            old_len + new_len
        },
        Empty => 0,
    }
}

fn multiple_live_aliases(i: i64) -> i64 {
    var opt = PairBox.Both(f"left-{i}", f"right-{i}");
    match opt {
        Both(left, right) => {
            opt = PairBox.Empty;
            left.len() + right.len()
        },
        Empty => 0,
    }
}

fn main() -> i64 {
    var total = 0;
    for i in 0..__FRAMES__ {
        total = total + live_alias(i) + live_alias_return(i) + repeated_live_alias(i)
            + multiple_live_aliases(i);
        println("frame");
    }
    if total > 0 { 0 } else { 91 }
}
"#;

const SELF_ALIAS_TEMPLATE: &str = r#"
enum Box {
    Full(string);
    Empty;
}

fn self_alias(i: i64) -> i64 {
    var opt = Box.Full(f"self-{i}");
    opt = opt;
    match opt {
        Full(s) => s.len(),
        Empty => 0,
    }
}

fn main() -> i64 {
    var total = 0;
    for i in 0..__FRAMES__ {
        total = total + self_alias(i);
        println("frame");
    }
    if total > 0 { 0 } else { 91 }
}
"#;

const CONDITIONAL_CONSUME_TEMPLATE: &str = r#"
enum Box {
    Full(string);
    Empty;
}

fn exercise(take: bool, i: i64) -> string {
    var opt = Box.Full(f"conditional-consume-{i}");
    match opt {
        Full(s) => {
            opt = Box.Empty;
            if take {
                return s;
            }
            f"fallback-{i}"
        },
        Empty => f"empty-{i}",
    }
}

fn main() {
    var total = 0;
    for i in 0..__FRAMES__ {
        total = total + exercise(i % 2 == 0, i).len();
        println("frame");
    }
    if total == 0 {
        panic("missing payload");
    }
}
"#;

const UNSUPPORTED_LIVE_ALIAS_CASES: &[(&str, &str, &str)] = &[
    (
        "mixed_string_vec",
        r#"
enum Mixed { Full(string, Vec<i64>); Empty }
fn main() {
    let values: Vec<i64> = Vec.new();
    var opt = Mixed.Full(f"mixed", values);
    match opt {
        Full(s, xs) => {
            opt = Mixed.Empty;
            s.len() + xs.len();
        },
        Empty => {},
    }
}
"#,
        "enum overwrite with a live non-string payload alias",
    ),
    (
        "record_payload",
        r#"
type Row { text: string }
enum Box { Full(Row); Empty }
fn main() {
    var opt = Box.Full(Row { text: f"record" });
    match opt {
        Full(row) => {
            opt = Box.Empty;
            row.text.len();
        },
        Empty => {},
    }
}
"#,
        "enum overwrite with a live non-string payload alias",
    ),
    (
        "nested_enum_payload",
        r#"
enum Inner { Text(string); Empty }
enum Outer { Full(Inner); Empty }
fn main() {
    var opt = Outer.Full(Inner.Text(f"nested"));
    match opt {
        Full(inner) => {
            opt = Outer.Empty;
            match inner {
                Text(s) => { s.len(); },
                Empty => {},
            }
        },
        Empty => {},
    }
}
"#,
        "enum overwrite with a live non-string payload alias",
    ),
    (
        "guard_fallthrough",
        r#"
enum Box { Full(string); Empty }
fn main() {
    var opt = Box.Full(f"guard");
    let n = match opt {
        Full(s) if {
            opt = Box.Empty;
            false
        } => s.len(),
        _ => 0,
    };
    println(n);
}
"#,
        "enum overwrite in a fallthrough match guard",
    ),
];

fn source(frames: usize) -> String {
    SOURCE_TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

fn live_alias_source(frames: usize) -> String {
    LIVE_ALIAS_TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

fn self_alias_source(frames: usize) -> String {
    SELF_ALIAS_TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

fn conditional_consume_source(frames: usize) -> String {
    CONDITIONAL_CONSUME_TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

fn dump_raw(source: &str, name: &str) -> String {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("nested-lexical-enum-overwrite-mir-")
        .tempdir()
        .expect("tempdir");
    let path = dir.path().join(format!("{name}.hew"));
    std::fs::write(&path, source).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--dump-mir",
            "raw",
            path.to_str().expect("Hew source path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .unwrap_or_else(|error| panic!("invoke hew compile --dump-mir raw: {error}"));
    assert!(
        output.status.success(),
        "raw MIR dump failed:\n{}",
        describe_output(&output)
    );
    String::from_utf8(output.stdout).expect("MIR dump is UTF-8")
}

fn function_section<'a>(dump: &'a str, name: &str) -> &'a str {
    let marker = format!("fn {name}");
    let start = dump
        .find(&marker)
        .unwrap_or_else(|| panic!("missing `{marker}` in MIR dump:\n{dump}"));
    let tail = &dump[start..];
    tail.find("\nfn ").map_or(tail, |next| &tail[..next])
}

#[test]
fn raw_mir_pins_release_authority_and_fail_closed_controls() {
    let raw = dump_raw(&source(1), "nested_lexical_enum_overwrite");
    for name in ["direct", "nested", "expression_arm", "joined"] {
        let section = function_section(&raw, name);
        assert_eq!(
            section.match_indices("ty=Box fn=in_place(Enum)").count(),
            1,
            "{name} must release the old active Box payload exactly once before Empty \
             overwrites its slot:\n{section}"
        );
    }

    for name in ["fresh_nonempty", "guarded"] {
        let section = function_section(&raw, name);
        assert_eq!(
            section.match_indices("ty=Box fn=in_place(Enum)").count(),
            1,
            "{name} must release the old active payload on the proven non-aliasing \
             constructor path:\n{section}"
        );
    }

    let live_raw = dump_raw(&live_alias_source(1), "live_alias_counterfactual");
    let self_alias_raw = dump_raw(&self_alias_source(1), "self_alias_counterfactual");
    let self_alias = function_section(&self_alias_raw, "self_alias");
    assert_eq!(
        self_alias.match_indices("ty=Box fn=in_place(Enum)").count(),
        0,
        "a self/forwarding RHS may alias the old payload and must not release it before the \
         store:\n{self_alias}"
    );

    let whole = function_section(&raw, "whole_forward");
    assert!(
        whole.contains("neutralize_payload"),
        "the whole-enum payload transfer must retain its source-slot neutralization:\n{whole}"
    );
    assert_eq!(
        whole.match_indices("ty=Outer fn=in_place(Enum)").count(),
        0,
        "a live, transferred payload owner must not gain a competing parent overwrite \
         release:\n{whole}"
    );

    let scalar = function_section(&raw, "scalar");
    assert!(
        !scalar.contains("fn=in_place(Enum)"),
        "a scalar-only enum overwrite owns no heap and needs no release:\n{scalar}"
    );

    let live = function_section(&live_raw, "live_alias");
    assert_eq!(
        live.match_indices("ty=Box fn=in_place(Enum)").count(),
        0,
        "the old payload must stay alive when its projected alias is read after the \
         overwrite cursor:\n{live}"
    );
    let repeated = function_section(&live_raw, "repeated_live_alias");
    assert_eq!(
        repeated.match_indices("ty=Box fn=in_place(Enum)").count(),
        1,
        "after the first overwrite transfers the old generation to the live binder, a \
         repeated overwrite must release the parent's newer generation:\n{repeated}"
    );
}

#[test]
fn unsupported_live_alias_shapes_reject_instead_of_leaking() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("unsupported-live-enum-alias-overwrite-")
        .tempdir()
        .expect("tempdir");
    for (name, source, marker) in UNSUPPORTED_LIVE_ALIAS_CASES {
        let path = dir.path().join(format!("{name}.hew"));
        std::fs::write(&path, source).expect("write Hew source");
        let output = Command::new(hew_binary())
            .args([
                "compile",
                "--dump-mir",
                "raw",
                path.to_str().expect("Hew source path is UTF-8"),
            ])
            .current_dir(repo_root())
            .output()
            .unwrap_or_else(|error| panic!("invoke hew compile for {name}: {error}"));
        assert!(
            !output.status.success(),
            "{name} has no represented delayed-release protocol and must reject rather than \
             compile a known leak:\n{}",
            describe_output(&output)
        );
        assert!(
            String::from_utf8_lossy(&output.stderr).contains(marker),
            "{name} must reject at the live-alias overwrite authority:\n{}",
            describe_output(&output)
        );
    }
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact leak oracle needs macOS leaks(1); absent capability must be a counted skip"
)]
#[test]
fn low_and_high_closed_alias_overwrites_are_exactly_leak_clean() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("nested-lexical-enum-overwrite-leaks-")
        .tempdir()
        .expect("tempdir");
    for frames in [LOW_FRAMES, HIGH_FRAMES] {
        let bin = compile_to_native(
            &source(frames),
            dir.path(),
            &format!("nested_lexical_enum_overwrite_{frames}"),
        );
        assert_eq!(
            run_probe_witness(&bin, &[]),
            frames,
            "the exact leak sample must execute every requested frame"
        );
        assert_eq!(
            measure_leaks_exact(&bin),
            (0, 0),
            "{frames} frames must release every direct, nested, joined, fresh, guarded, and \
             transferred payload exactly once"
        );

        let live_alias_bin = compile_to_native(
            &live_alias_source(frames),
            dir.path(),
            &format!("live_alias_overwrite_{frames}"),
        );
        assert_eq!(
            run_probe_witness(&live_alias_bin, &[]),
            frames,
            "the delayed-release sample must execute every requested frame"
        );
        assert_eq!(
            measure_leaks_exact(&live_alias_bin),
            (0, 0),
            "{frames} live-alias overwrites must delay the old payload release until the \
             binder's final scope-exit read"
        );
    }
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the deterministic poisoned-allocator contract is macOS-only"
)]
#[test]
fn overwrite_ownership_controls_do_not_double_free_or_read_poison() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("nested-lexical-enum-overwrite-scribble-")
        .tempdir()
        .expect("tempdir");
    for (name, source) in [
        ("closed_aliases", source(HIGH_FRAMES)),
        ("live_alias", live_alias_source(HIGH_FRAMES)),
        ("self_alias", self_alias_source(HIGH_FRAMES)),
    ] {
        let bin = compile_to_native(&source, dir.path(), name);
        let output = run_under_malloc_scribble(&bin);
        assert!(
            output.status.success(),
            "{name} must preserve every payload through its final read and release no owner \
             twice:\n{}",
            describe_output(&output)
        );
        assert_eq!(
            String::from_utf8_lossy(&output.stdout)
                .lines()
                .filter(|line| *line == "frame")
                .count(),
            HIGH_FRAMES,
            "{name} must execute every requested frame under allocator poisoning"
        );
    }
}

/// The parent overwrite transfers the old payload to the arm binder
/// (`flag = 0`). A later conditional consume must re-arm that same flag only
/// on the taken path; otherwise a shared arm-close plan can drop a string after
/// release authority has moved onward.
#[test]
fn conditional_consume_rearms_the_delayed_release_flag() {
    let raw = dump_raw(
        &conditional_consume_source(1),
        "conditional_consume_flag_transition",
    );
    let section = function_section(&raw, "exercise");
    let mut assignments: std::collections::BTreeMap<&str, (usize, usize)> =
        std::collections::BTreeMap::new();
    for line in section.lines() {
        let Some((dest, value)) = line.trim().split_once(" = const.i64 ") else {
            continue;
        };
        let counts = assignments.entry(dest).or_default();
        match value {
            "0" => counts.0 += 1,
            "1" => counts.1 += 1,
            _ => {}
        }
    }
    assert!(
        assignments
            .values()
            .any(|(zeros, ones)| *zeros >= 1 && *ones >= 2),
        "one projected-payload flag must transition 1 (parent-owned) -> 0 \
         (binder-owned) -> 1 (consumed onward) on the conditional consume path:\n{section}"
    );
}

/// Runtime counterpart for the conditional flag transition: both the
/// transferred return path and the live-binder fallthrough path release the
/// old payload exactly once.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the deterministic poisoned-allocator contract is macOS-only"
)]
#[test]
fn conditional_consume_after_live_alias_overwrite_drops_exactly_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("conditional-consume-live-enum-alias-")
        .tempdir()
        .expect("tempdir");
    let source = conditional_consume_source(HIGH_FRAMES);
    let bin = compile_to_native(&source, dir.path(), "conditional_consume_live_alias");
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "the consumed path must suppress the delayed binder drop while the live path keeps it:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout)
            .lines()
            .filter(|line| *line == "frame")
            .count(),
        HIGH_FRAMES,
        "both conditional ownership paths must execute under allocator poisoning"
    );
    assert_eq!(
        measure_leaks_exact(&bin),
        (0, 0),
        "conditional consume and non-consume paths must each release the payload exactly once"
    );
}
