//! Exact-ownership oracles for a heap-boxed trait object returned by value.
//!
//! Each factory allocates a dynamically built string inside a concrete record,
//! erases the record, and transfers the resulting box to its caller. Repeating
//! that boundary exposes a missing caller-side release as a frame-proportional
//! leak. Running the same shape under the poisoned allocator exposes a stale
//! callee release or a duplicate caller release.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn returned_dyn_source(frames: usize) -> String {
    format!(
        r#"
import std::string;

trait Labeled {{
    fn label_size(val: Self) -> i64;
}}

type Person {{
    label: string;
}}

impl Person {{
    fn label_size(val: Person) -> i64 {{
        val.label.len()
    }}
}}

type Parcel {{
    label: string;
    route: string;
}}

impl Parcel {{
    fn label_size(val: Parcel) -> i64 {{
        val.label.len()
    }}
}}

fn make_labeled(seed: string, kind: i64) -> dyn Labeled {{
    if kind == 0 {{
        Person {{ label: string.repeat(seed, 64) }}
    }} else {{
        Parcel {{
            label: string.repeat(seed, 96),
            route: string.repeat(seed, 17),
        }}
    }}
}}

fn inspect(value: dyn Labeled) -> i64 {{
    value.label_size()
}}

fn main() -> i64 {{
    for frame in 0..{frames} {{
        let person = make_labeled(f"person-{{frame}}", 0);
        let parcel = make_labeled(f"parcel-{{frame}}", 1);
        if inspect(person) < 64 || inspect(parcel) < 96 {{ return 91; }}
    }}
    print("OK");
    0
}}
"#
    )
}

/// The pass-through shape: a `dyn` parameter returned unchanged. The carrier
/// transfer must MOVE the existing box (callee guard suppresses the terminal
/// drop; caller keeps the sole release authority) — a stale callee release
/// shows up under the scribble run, a dropped caller release as a leak slope.
fn passed_through_dyn_source(frames: usize) -> String {
    format!(
        r#"
import std::string;

trait Labeled {{
    fn label_size(val: Self) -> i64;
}}

type Person {{
    label: string;
}}

impl Person {{
    fn label_size(val: Person) -> i64 {{
        val.label.len()
    }}
}}

fn make_labeled(seed: string) -> dyn Labeled {{
    Person {{ label: string.repeat(seed, 64) }}
}}

fn identity(value: dyn Labeled) -> dyn Labeled {{
    value
}}

fn inspect(value: dyn Labeled) -> i64 {{
    value.label_size()
}}

fn main() -> i64 {{
    for frame in 0..{frames} {{
        let direct = identity(make_labeled(f"direct-{{frame}}"));
        let chained = identity(identity(make_labeled(f"chained-{{frame}}")));
        if inspect(direct) < 64 || inspect(chained) < 64 {{ return 92; }}
    }}
    print("OK");
    0
}}
"#
    )
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn returned_dyn_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("dyn_trait_return", returned_dyn_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn passed_through_dyn_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("dyn_trait_pass_through", passed_through_dyn_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "double-free oracle needs the Darwin poisoned allocator; other hosts cover the MIR structure and compiled Hew fixture"
)]
#[test]
fn passed_through_dyn_is_live_and_freed_once_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("dyn-trait-pass-through-")
        .tempdir()
        .expect("tempdir");
    let source = passed_through_dyn_source(16);
    let bin = compile_to_native(&source, dir.path(), "dyn_trait_pass_through");
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "passed-through dyn values must remain live and release exactly once:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "OK",
        "the probe must cross every pass-through boundary:\n{}",
        describe_output(&output)
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "double-free oracle needs the Darwin poisoned allocator; other hosts cover the MIR structure and compiled Hew fixture"
)]
#[test]
fn returned_dyn_is_live_and_freed_once_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("dyn-trait-return-")
        .tempdir()
        .expect("tempdir");
    let source = returned_dyn_source(16);
    let bin = compile_to_native(&source, dir.path(), "dyn_trait_return");
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "returned dyn values must remain live and release exactly once:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "OK",
        "the probe must cross every factory return boundary:\n{}",
        describe_output(&output)
    );
}
