//! Branch-local owned-carrier return oracles.
//!
//! An owned `Vec<string>` parameter returned from both arms of an `if` must
//! transfer its release authority in each mutually-exclusive arm. MIR lowering
//! visits those arms sequentially; consuming a compiler-side authority fact in
//! the first arm must not make the second arm return an alias that the callee's
//! terminal snapshot drop then frees.
//!
//! The witness repeatedly forwards a non-empty vector through the helper,
//! reassigns it from the returned tuple, and matches the sibling enum. A stale
//! vector or corrupted sibling tag traps before `OK`; an omitted release grows
//! with the frame count; a duplicate release aborts under the poisoned
//! allocator.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn branch_return_source(frames: usize) -> String {
    format!(
        r#"
enum Slot {{
    Filled(i64);
    Empty;
}}

fn step(items: Vec<string>, i: i64) -> (Vec<string>, Slot) {{
    if i < 2 {{
        (items, Filled(i))
    }} else {{
        (items, Empty)
    }}
}}

fn run_case(frame: i64) -> i64 {{
    var items: Vec<string> = Vec.new();
    items.push(f"left-{{frame}}");
    items.push(f"right-{{frame}}");

    var i = 0;
    var done = false;
    while !done {{
        let pair = step(items, i);
        items = pair.0;
        match pair.1 {{
            Filled(tag) => {{
                if tag != i {{ return 81; }}
            }},
            Empty => {{
                done = true;
            }},
        }}
        i = i + 1;
    }}
    items.len()
}}

fn main() -> i64 {{
    for frame in 0..{frames} {{
        if run_case(frame) != 2 {{ return 82; }}
    }}
    print("OK");
    0
}}
"#
    )
}

/// Every helper frame releases its final Vec exactly once. A missing terminal
/// owner produces a frame-proportional leak slope.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn branch_returned_vec_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("owned_carrier_branch_return", branch_return_source);
}

/// The returned Vec and sibling enum tag remain valid through all three helper
/// calls, and terminal cleanup does not free either the source or destination
/// twice.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "use-after-free and double-free oracle needs the Darwin poisoned allocator; other hosts cover the MIR structure and sanitizer fixture"
)]
#[test]
fn branch_returned_vec_is_live_and_freed_once_under_malloc_scribble() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("owned-carrier-branch-return-")
        .tempdir()
        .expect("tempdir");
    let source = branch_return_source(8);
    let bin = compile_to_native(&source, dir.path(), "owned_carrier_branch_return");
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "branch-returned Vec must remain live and release exactly once:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "OK",
        "the helper must preserve both vector length and enum tags:\n{}",
        describe_output(&output)
    );
}
