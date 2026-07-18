//! Nested-`Vec` index-read (`hew_vec_get_clone`) drop canary (hew-lang/hew#2725, site a).
//!
//! ## The shape
//!
//! Indexing a `Vec<Vec<i64>>` binds a fresh clone of the inner collection:
//!
//! ```text
//! let vv: [[i64]] = [];  vv.push([1, 2, 3]);  vv.push([4, 5, 6]);
//! let inner = vv[i];     // hew_vec_get_clone — deep-clones the inner Vec
//! ```
//!
//! #2725 site (a) reported the caller-side fresh transient from `hew_vec_get_clone`
//! never recursing drop through the cloned inner buffer — 4 leaks/frame on the
//! issue's base `dac280f36`.
//!
//! ## Status on the Stage-1 tip (`882ff31d9`)
//!
//! CLEAN on `882ff31d9` — the index-read transient of a collection-bearing
//! element is freed at transient scope exit across bind-and-len, bind-and-index,
//! and bind-and-for-in uses. This is a CANARY locking that behaviour with a flat
//! per-frame leak slope; the emitted code routes through `hew_vec_get_clone`
//! (asserted by the pre-fix issue and confirmed in the `.ll` for these shapes).

#![cfg(unix)]

mod support;

use support::leak_slope::assert_frame_slope_below_tolerance;

/// `let inner = vv[i]` then iterate the clone (for-in consumes the transient).
fn get_clone_for_in_loop_source(frames: usize) -> String {
    format!(
        "fn frame() -> i64 {{\n\
         \x20   let vv: [[i64]] = [];\n\
         \x20   vv.push([1, 2, 3]);\n\
         \x20   vv.push([4, 5, 6]);\n\
         \x20   let inner = vv[0];\n\
         \x20   var sum: i64 = 0;\n\
         \x20   for v in inner {{ sum = sum + v; }}\n\
         \x20   sum\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + frame();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total < 0 {{ 72 }} else {{ 0 }}\n\
         }}\n"
    )
}

/// `let inner = vv[i]` then read `inner.len()` — the clone is bound and read but
/// not iterated; the transient must still be freed at scope exit.
fn get_clone_bind_len_loop_source(frames: usize) -> String {
    format!(
        "fn frame() -> i64 {{\n\
         \x20   let vv: [[i64]] = [];\n\
         \x20   vv.push([1, 2, 3]);\n\
         \x20   vv.push([4, 5, 6]);\n\
         \x20   let inner = vv[1];\n\
         \x20   inner.len()\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + frame();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total < 0 {{ 72 }} else {{ 0 }}\n\
         }}\n"
    )
}

#[test]
fn get_clone_for_in_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("nested_vec_get_clone_for_in", get_clone_for_in_loop_source);
}

#[test]
fn get_clone_bind_len_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "nested_vec_get_clone_bind_len",
        get_clone_bind_len_loop_source,
    );
}
