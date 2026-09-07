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
    Full(string),
    Empty,
}

enum Inner {
    Text(string),
    Empty,
}

enum Outer {
    Full(Inner),
    Empty,
}

enum ScalarBox {
    Full(i64),
    Empty,
}

fn direct(i: i64) -> i64 {
    var opt = Box.Full(f"direct-{i}");
    var n = 0;
    match opt {
        .Full(s) => {
            n = s.len();
        },
        .Empty => {},
    }
    opt = Box.Empty;
    n
}

fn nested(i: i64) -> i64 {
    var opt = Box.Full(f"nested-{i}");
    var n = 0;
    {
        match opt {
            .Full(s) => {
                n = s.len();
            },
            .Empty => {},
        }
    }
    opt = Box.Empty;
    n
}

fn expression_arm(i: i64) -> i64 {
    var opt = Box.Full(f"expression-{i}");
    let n = match opt {
        .Full(s) => s.len(),
        .Empty => 0,
    };
    opt = Box.Empty;
    n
}

fn joined(i: i64) -> i64 {
    var opt = Box.Full(f"joined-{i}");
    var n = 0;
    if i % 2 == 0 {
        match opt {
            .Full(s) => {
                n = s.len();
            },
            .Empty => {},
        }
    } else {
        {
            match opt {
                .Full(s) => {
                    n = s.len();
                },
                .Empty => {},
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
        .Full(s) => s.len(),
        .Empty => 0,
    }
}

fn guarded(i: i64) -> i64 {
    var opt = Box.Full(f"guard-old-{i}");
    if i < 0 {
        let moved = opt;
        return match moved {
            .Full(s) => s.len(),
            .Empty => 0,
        };
    }
    opt = Box.Full(f"guard-new-{i}");
    match opt {
        .Full(s) => s.len(),
        .Empty => 0,
    }
}

fn whole_forward(i: i64) -> i64 {
    var opt = Outer.Full(Inner.Text(f"forward-{i}"));
    match opt {
        .Full(inner) => {
            let owner = inner;
            opt = Outer.Empty;
            match owner {
                .Text(s) => s.len(),
                .Empty => 0,
            }
        },
        .Empty => 0,
    }
}

fn scalar(i: i64) -> i64 {
    var opt = ScalarBox.Full(i);
    var n = 0;
    match opt {
        .Full(v) => {
            n = v;
        },
        .Empty => {},
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
    Full(string),
    Empty,
}

enum PairBox {
    Both(string, string),
    Empty,
}

fn live_alias(i: i64) -> i64 {
    var opt = Box.Full(f"live-{i}");
    match opt {
        .Full(s) => {
            opt = Box.Empty;
            s.len()
        },
        .Empty => 0,
    }
}

fn live_alias_return(i: i64) -> i64 {
    var opt = Box.Full(f"return-{i}");
    match opt {
        .Full(s) => {
            opt = Box.Empty;
            return s.len();
        },
        .Empty => 0,
    }
}

fn repeated_live_alias(i: i64) -> i64 {
    var opt = Box.Full(f"repeat-old-{i}");
    match opt {
        .Full(s) => {
            opt = Box.Full(f"repeat-one-{i}");
            opt = Box.Full(f"repeat-two-{i}");
            let old_len = s.len();
            let new_len = match opt {
                .Full(current) => current.len(),
                .Empty => 0,
            };
            old_len + new_len
        },
        .Empty => 0,
    }
}

fn multiple_live_aliases(i: i64) -> i64 {
    var opt = PairBox.Both(f"left-{i}", f"right-{i}");
    match opt {
        .Both(left, right) => {
            opt = PairBox.Empty;
            left.len() + right.len()
        },
        .Empty => 0,
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
    Full(string),
    Empty,
}

fn self_alias(i: i64) -> i64 {
    var opt = Box.Full(f"self-{i}");
    opt = opt;
    match opt {
        .Full(s) => s.len(),
        .Empty => 0,
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
    Full(string),
    Empty,
}

fn exercise(take: bool, i: i64) -> string {
    var opt = Box.Full(f"conditional-consume-{i}");
    match opt {
        .Full(s) => {
            opt = Box.Empty;
            if take {
                return s;
            }
            f"fallback-{i}"
        },
        .Empty => f"empty-{i}",
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
enum Mixed { Full(string, Vec<i64>), Empty }
fn main() {
    let values: Vec<i64> = Vec.new();
    var opt = Mixed.Full(f"mixed", values);
    match opt {
        .Full(s, xs) => {
            opt = Mixed.Empty;
            s.len() + xs.len();
        },
        .Empty => {},
    }
}
"#,
        "enum overwrite with a live non-string payload alias",
    ),
    (
        "record_payload",
        r#"
type Row { text: string }
enum Box { Full(Row), Empty }
fn main() {
    var opt = Box.Full(Row { text: f"record" });
    match opt {
        .Full(row) => {
            opt = Box.Empty;
            row.text.len();
        },
        .Empty => {},
    }
}
"#,
        "enum overwrite with a live non-string payload alias",
    ),
    (
        "nested_enum_payload",
        r#"
enum Inner { Text(string), Empty }
enum Outer { Full(Inner), Empty }
fn main() {
    var opt = Outer.Full(Inner.Text(f"nested"));
    match opt {
        .Full(inner) => {
            opt = Outer.Empty;
            match inner {
                .Text(s) => { s.len(); },
                .Empty => {},
            }
        },
        .Empty => {},
    }
}
"#,
        "enum overwrite with a live non-string payload alias",
    ),
    (
        "guard_fallthrough",
        r#"
enum Box { Full(string), Empty }
fn main() {
    var opt = Box.Full(f"guard");
    let n = match opt {
        .Full(s) if {
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

// Lost coverage: `raw_mir_pins_release_authority_and_fail_closed_controls`
// used `--dump-mir raw` (retired) to grep/count exact release-authority
// markers (`ty=Box fn=in_place(Enum)`, `neutralize_payload`, etc.) per
// function section across a dozen aliasing/self-alias/live-alias/scalar
// shapes, including negative controls (self-alias and scalar overwrites
// that must NOT emit a release). Physical MIR's structured (Debug) dump has
// no equivalent single-line text to grep or count per function, so this
// MIR-emission coverage has no direct replacement. The same
// leak/double-free/exactly-once behaviour for the same shapes (self-alias,
// live-alias, conditional-consume, closed-alias overwrite) is still proven
// end to end below by the leak-slope and malloc-scribble oracles in this
// file.
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
            .args(["compile", path.to_str().expect("Hew source path is UTF-8")])
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

// Lost coverage: `conditional_consume_rearms_the_delayed_release_flag` used
// `--dump-mir raw` (retired) to grep the guard-flag assignment sequence
// (`= const.i64 0` / `= const.i64 1`) in the "exercise" function section,
// pinning that the delayed-release flag transitions
// parent-owned -> binder-owned -> consumed-onward on the taken path.
// Physical MIR's structured (Debug) dump has no equivalent single-line text
// to grep for the same per-local assignment sequence, so this MIR-level
// coverage has no direct replacement. The runtime counterpart below,
// `conditional_consume_after_live_alias_overwrite_drops_exactly_once`,
// still proves the same flag-transition invariant end to end (exactly-once
// release under a poisoned allocator).
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
