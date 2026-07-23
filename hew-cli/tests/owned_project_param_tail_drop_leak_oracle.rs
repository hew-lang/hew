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

/// Guard-before-destructure: an early `return` branches AROUND the consuming
/// project match, so the two exits split the release authority. The
/// branch-around exit must release the untouched carrier via the terminal
/// snapshot drop (skipping it leaks every owned field per call); the
/// through-match exit must release via arm binders with the terminal drop
/// discharged on that path only (keeping it double-frees). Both `skip`
/// directions run in the same binary so the slope pins the leak on either
/// exit and the scribble run pins exactly-once on both.
fn guard_branch_around_source(frames: usize) -> String {
    format!(
        "type Packet {{ tag: string, body: string }}\n\
         fn guarded_record(p: Packet, skip: bool) -> i64 {{\n\
         \x20   if skip {{ return 1; }}\n\
         \x20   match p {{ Packet {{ tag, body }} => tag.len() + body.len() }}\n\
         }}\n\
         fn guarded_tuple(p: (string, string), skip: bool) -> i64 {{\n\
         \x20   if skip {{ return 1; }}\n\
         \x20   match p {{ (left, right) => left.len() + right.len() }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       total = total + guarded_record(Packet {{ tag: \"g\" + \"r\", body: \"a\" + \"b\" }}, true);\n\
         \x20       total = total + guarded_record(Packet {{ tag: \"g\" + \"r\", body: \"c\" + \"d\" }}, false);\n\
         \x20       total = total + guarded_tuple((\"g\" + \"t\", \"e\" + \"f\"), true);\n\
         \x20       total = total + guarded_tuple((\"g\" + \"t\", \"g\" + \"h\"), false);\n\
         \x20   }}\n\
         \x20   if total != {frames} * 10 {{ return 72; }}\n\
         \x20   println(\"guard-branch-around-ok\");\n\
         \x20   0\n\
         }}\n"
    )
}

/// Partitioned consume: BOTH paths of an early-return/tail split end in
/// their own consuming project match on the same carrier, each with its own
/// exit. EVERY consuming site must join the carrier's consume set — the
/// first site alone leaves the second path's exit outside the set, so that
/// exit keeps the terminal snapshot drop over fields its arm discharge
/// already released (a double release; observable as a `#[resource]` close
/// firing twice). The string loop pins the leak slope on both paths; the
/// resource pair pins exactly-once close per value via exact stdout
/// (`closed-1` then `closed-2`, each once, in call order — a regression
/// prints a third close line).
fn partitioned_consume_source(frames: usize) -> String {
    format!(
        "#[resource] type Conn {{ fd: i64; }}\n\
         impl Conn {{ fn close(self) {{ println(\"closed-\" + self.fd.fmt()); }} }}\n\
         type Wire {{ conn: Conn }}\n\
         type Packet {{ tag: string, body: string }}\n\
         fn pick(p: Packet, left: bool) -> i64 {{\n\
         \x20   if left {{\n\
         \x20       return match p {{ Packet {{ tag, body }} => tag.len() }};\n\
         \x20   }}\n\
         \x20   match p {{ Packet {{ tag, body }} => body.len() }}\n\
         }}\n\
         fn pick_wire(w: Wire, left: bool) -> i64 {{\n\
         \x20   if left {{\n\
         \x20       return match w {{ Wire {{ conn }} => conn.fd }};\n\
         \x20   }}\n\
         \x20   match w {{ Wire {{ conn }} => conn.fd }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       total = total + pick(Packet {{ tag: \"l\" + \"t\", body: \"ab\" + \"c\" }}, true);\n\
         \x20       total = total + pick(Packet {{ tag: \"r\" + \"t\", body: \"de\" + \"f\" }}, false);\n\
         \x20   }}\n\
         \x20   if total != {frames} * 5 {{ return 73; }}\n\
         \x20   let a = pick_wire(Wire {{ conn: Conn {{ fd: 1 }} }}, true);\n\
         \x20   let b = pick_wire(Wire {{ conn: Conn {{ fd: 2 }} }}, false);\n\
         \x20   if a + b != 3 {{ return 74; }}\n\
         \x20   println(\"partitioned-ok\");\n\
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

#[test]
fn guard_branch_around_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "owned_project_guard_branch_around",
        guard_branch_around_source,
    );
}

#[test]
fn guard_branch_around_is_clean_under_malloc_scribble() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("owned-project-guard-branch-around-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &guard_branch_around_source(50),
        dir.path(),
        "owned_project_guard_branch_around_scribble",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "a guard that branches around a consuming project match must release \
         the carrier exactly once on BOTH exits;\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "guard-branch-around-ok\n",
        "guard-branch-around paths must preserve exact output;\n{}",
        describe_output(&output)
    );
}

#[test]
fn partitioned_consume_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "owned_project_partitioned_consume",
        partitioned_consume_source,
    );
}

#[test]
fn partitioned_consume_closes_each_resource_exactly_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("owned-project-partitioned-consume-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &partitioned_consume_source(50),
        dir.path(),
        "owned_project_partitioned_consume_scribble",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "every partitioned consuming match must join the carrier's consume \
         set — an unrecorded site's exit re-releases the discharged fields;\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "closed-1\nclosed-2\npartitioned-ok\n",
        "each partitioned carrier's resource must close exactly once, in call \
         order — a third close line is the terminal-drop double release;\n{}",
        describe_output(&output)
    );
}
