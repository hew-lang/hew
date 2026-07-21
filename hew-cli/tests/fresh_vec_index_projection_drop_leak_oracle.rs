//! Direct projection from a fresh `Vec` composite-index result must release the
//! cloned root once, without changing the bound-index control's ownership.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn direct_projection_source(frames: usize) -> String {
    format!(
        "record Holder {{\n\
         \x20   items: [string],\n\
         }}\n\
         \n\
         fn frame() -> i64 {{\n\
         \x20   let v: [Holder] = [];\n\
         \x20   v.push(Holder {{ items: [\"left\", \"right\"] }});\n\
         \x20   v[0].items.len()\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + frame();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total == {} {{ 0 }} else {{ 71 }}\n\
         }}\n",
        frames * 2
    )
}

fn bound_projection_source(frames: usize) -> String {
    format!(
        "record Holder {{\n\
         \x20   items: [string],\n\
         }}\n\
         \n\
         fn frame() -> i64 {{\n\
         \x20   let v: [Holder] = [];\n\
         \x20   v.push(Holder {{ items: [\"left\", \"right\"] }});\n\
         \x20   let h = v[0];\n\
         \x20   h.items.len()\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + frame();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total == {} {{ 0 }} else {{ 72 }}\n\
         }}\n",
        frames * 2
    )
}

fn early_return_projection_source(frames: usize) -> String {
    format!(
        "record Holder {{\n\
         \x20   items: [string],\n\
         }}\n\
         \n\
         fn frame(flag: bool) -> i64 {{\n\
         \x20   let v: [Holder] = [];\n\
         \x20   v.push(Holder {{ items: [\"left\", \"right\"] }});\n\
         \x20   if flag {{\n\
         \x20       return v[0].items.len();\n\
         \x20   }}\n\
         \x20   99\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + frame(true);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total == {} {{ 0 }} else {{ 73 }}\n\
         }}\n",
        frames * 2
    )
}

fn break_projection_source(frames: usize) -> String {
    format!(
        "record Holder {{\n\
         \x20   items: [string],\n\
         }}\n\
         \n\
         fn frame() -> i64 {{\n\
         \x20   let v: [Holder] = [];\n\
         \x20   v.push(Holder {{ items: [\"left\", \"right\"] }});\n\
         \x20   var total: i64 = 0;\n\
         \x20   while total == 0 {{\n\
         \x20       total = v[0].items.len();\n\
         \x20       break;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + frame();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total == {} {{ 0 }} else {{ 74 }}\n\
         }}\n",
        frames * 2
    )
}

/// Moving the projected field out is deliberately a correctness-only pin.
/// The generic owned-record field-escape path has a separately proven,
/// pre-existing leak and is outside this narrow fix; this fixture prevents the
/// new synthetic root owner from turning that known leak into a double-free or
/// use-after-free without falsely claiming leak closure.
fn escaped_projection_source() -> String {
    "record Holder {\n\
     \x20   items: [string],\n\
     }\n\
     \n\
     fn take_items() -> [string] {\n\
     \x20   let v: [Holder] = [];\n\
     \x20   v.push(Holder { items: [\"left\", \"right\"] });\n\
     \x20   let items = v[0].items;\n\
     \x20   items\n\
     }\n\
     \n\
     fn main() -> i64 {\n\
     \x20   let items = take_items();\n\
     \x20   if items.len() == 2 { 0 } else { 75 }\n\
     }\n"
    .to_string()
}

#[test]
fn direct_fresh_vec_projection_has_no_per_frame_leak() {
    assert_frame_slope_below_tolerance("fresh_vec_index_projection", direct_projection_source);
}

#[test]
fn bound_fresh_vec_index_control_has_no_per_frame_leak() {
    assert_frame_slope_below_tolerance("bound_vec_index_projection", bound_projection_source);
}

#[test]
fn early_return_fresh_vec_projection_has_no_per_frame_leak() {
    assert_frame_slope_below_tolerance(
        "early_return_fresh_vec_index_projection",
        early_return_projection_source,
    );
}

#[test]
fn break_fresh_vec_projection_has_no_per_frame_leak() {
    assert_frame_slope_below_tolerance("break_fresh_vec_index_projection", break_projection_source);
}

#[test]
fn direct_fresh_vec_projection_returns_exact_result_under_malloc_scribble() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("fresh-vec-index-projection-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&direct_projection_source(3), dir.path(), "projection");
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "direct projection must return its exact accumulated value under the poisoned allocator;\n{}",
        describe_output(&output)
    );
}

#[test]
fn escaped_projection_field_remains_live_under_malloc_scribble() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("fresh-vec-index-projection-escape-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &escaped_projection_source(),
        dir.path(),
        "projection_escape",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "a transferred projected field must remain live under the poisoned allocator;\n{}",
        describe_output(&output)
    );
}
