//! Leak and poisoned-allocator oracle for owned record/tuple project matches
//! over by-value parameters.
//!
//! Tail and non-tail full projections transfer both string fields to arm
//! binders, while wildcard-only helpers borrow their parameter so the caller
//! can reuse and release the original composite. The LOW/HIGH run pins a flat
//! leak slope; the exact-output scribble run pins valid reads and exactly-once
//! releases.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn param_tail_project_source(frames: usize) -> String {
    format!(
        "type Packet {{ tag: string, body: string }}\n\
         fn record_tail(p: Packet) -> i64 {{\n\
         \x20   match p {{ Packet {{ tag, body }} => tag.len() + body.len() }}\n\
         }}\n\
         fn record_nontail(p: Packet) -> i64 {{\n\
         \x20   let total = match p {{ Packet {{ tag, body }} => tag.len() + body.len() }};\n\
         \x20   total\n\
         }}\n\
         fn tuple_tail(p: (string, string)) -> i64 {{\n\
         \x20   match p {{ (left, right) => left.len() + right.len() }}\n\
         }}\n\
         fn tuple_nontail(p: (string, string)) -> i64 {{\n\
         \x20   let total = match p {{ (left, right) => left.len() + right.len() }};\n\
         \x20   total\n\
         }}\n\
         fn borrow_record(p: Packet) -> i64 {{ match p {{ _ => 1 }} }}\n\
         fn borrow_tuple(p: (string, string)) -> i64 {{ match p {{ _ => 1 }} }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       total = total + record_tail(Packet {{ tag: \"r\" + \"t\", body: \"a\" + \"b\" }});\n\
         \x20       total = total + record_nontail(Packet {{ tag: \"r\" + \"n\", body: \"c\" + \"d\" }});\n\
         \x20       total = total + tuple_tail((\"t\" + \"t\", \"e\" + \"f\"));\n\
         \x20       total = total + tuple_nontail((\"t\" + \"n\", \"g\" + \"h\"));\n\
         \x20       let packet = Packet {{ tag: \"o\" + \"k\", body: \"i\" + \"j\" }};\n\
         \x20       total = total + borrow_record(packet) + packet.tag.len();\n\
         \x20       let tuple = (\"k\" + \"l\", \"m\" + \"n\");\n\
         \x20       total = total + borrow_tuple(tuple) + tuple.0.len();\n\
         \x20   }}\n\
         \x20   if total != {frames} * 22 {{ return 71; }}\n\
         \x20   println(\"param-tail-ok\");\n\
         \x20   0\n\
         }}\n"
    )
}

#[test]
fn owned_project_param_tail_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("owned_project_param_tail", param_tail_project_source);
}

#[test]
fn owned_project_param_tail_is_clean_under_malloc_scribble() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("owned-project-param-tail-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &param_tail_project_source(50),
        dir.path(),
        "owned_project_param_tail_scribble",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "owned project parameter paths must run clean under the poisoned allocator;\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "param-tail-ok\n",
        "owned project parameter paths must preserve exact output;\n{}",
        describe_output(&output)
    );
}
