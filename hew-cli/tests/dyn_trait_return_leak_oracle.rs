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
}}

impl Parcel {{
    fn label_size(val: Parcel) -> i64 {{
        val.label.len()
    }}
}}

fn make_person(seed: string) -> dyn Labeled {{
    let erased: dyn Labeled = Person {{ label: string.repeat(seed, 64) }};
    erased
}}

fn make_parcel(seed: string) -> dyn Labeled {{
    let erased: dyn Labeled = Parcel {{ label: string.repeat(seed, 96) }};
    erased
}}

fn inspect(value: dyn Labeled) -> i64 {{
    value.label_size()
}}

fn main() -> i64 {{
    for frame in 0..{frames} {{
        let person = make_person(f"person-{{frame}}");
        let parcel = make_parcel(f"parcel-{{frame}}");
        if inspect(person) < 64 || inspect(parcel) < 96 {{ return 91; }}
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
