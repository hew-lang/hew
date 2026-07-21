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

#[test]
fn direct_fresh_vec_projection_has_no_per_frame_leak() {
    assert_frame_slope_below_tolerance("fresh_vec_index_projection", direct_projection_source);
}

#[test]
fn bound_fresh_vec_index_control_has_no_per_frame_leak() {
    assert_frame_slope_below_tolerance("bound_vec_index_projection", bound_projection_source);
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
