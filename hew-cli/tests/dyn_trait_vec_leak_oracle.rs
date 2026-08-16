//! Exact-ownership oracles for heterogeneous `Vec<dyn Trait>` storage.
//!
//! Every concrete carries a dynamically built string. Each frame exercises
//! scope-exit teardown, pop/remove move-out, and a consuming iterator that
//! stops after one element so its cursor must release both moved-from and
//! still-live slots correctly.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn dyn_vec_source(frames: usize) -> String {
    format!(
        r#"
import std.string;

trait Labeled {{
    fn label_size(val: Self) -> i64;
}}

type Person {{ label: string }}
impl Person {{
    fn label_size(val: Person) -> i64 {{ val.label.len() }}
}}

type Parcel {{ label: string }}
impl Parcel {{
    fn label_size(val: Parcel) -> i64 {{ val.label.len() }}
}}

fn make_person(seed: string) -> dyn Labeled {{
    let erased: dyn Labeled = Person {{ label: string.repeat(seed, 64) }};
    erased
}}

fn make_parcel(seed: string) -> dyn Labeled {{
    let erased: dyn Labeled = Parcel {{ label: string.repeat(seed, 96) }};
    erased
}}

fn inspect(value: dyn Labeled) -> i64 {{ value.label_size() }}

fn run_frame(frame: i64) -> i64 {{
    let values: Vec<dyn Labeled> = Vec::new();
    values.push(make_person(f"person-{{frame}}"));
    values.push(make_parcel(f"parcel-{{frame}}"));
    values.push(make_person(f"tail-{{frame}}"));

    let last = values.pop();
    if inspect(last) < 64 {{ return 81; }}
    values.push(make_parcel(f"again-{{frame}}"));
    let first = values.remove(0);
    if inspect(first) < 64 {{ return 82; }}

    let iterator = values.into_iter();
    var seen: i64 = 0;
    for _value in iterator {{
        seen = seen + 1;
        break;
    }}
    seen
}}

fn main() -> i64 {{
    for frame in 0..{frames} {{
        if run_frame(frame) != 1 {{ return 84; }}
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
fn dyn_trait_vec_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("dyn_trait_vec", dyn_vec_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "double-free oracle needs the Darwin poisoned allocator; other hosts cover compiled Hew and structural descriptor tests"
)]
#[test]
fn dyn_trait_vec_releases_each_slot_once_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("dyn-trait-vec-")
        .tempdir()
        .expect("tempdir");
    let source = dyn_vec_source(16);
    let bin = compile_to_native(&source, dir.path(), "dyn_trait_vec");
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "dyn Vec elements must stay live and release exactly once:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "OK",
        "the probe must finish every move and partial-iteration path:\n{}",
        describe_output(&output)
    );
}
