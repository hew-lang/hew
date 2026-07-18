//! Leak and exactly-once guards for reassigned `BindingRef` while-let scrutinees.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn source(frames: usize, move_payload: bool) -> String {
    let payload_use = if move_payload {
        "let moved = s;\n        total = total + moved.len();"
    } else {
        "total = total + s.len();"
    };
    let payload = if move_payload {
        "wl-backedge-payload"
    } else {
        "while-let-backedge-payload-abcdefghijklmnopqrstuvwxyz"
    };
    format!(
        "enum Packet {{\n\
         \x20   Payload(string);\n\
         \x20   Done;\n\
         }}\n\
         fn next(i: i64, cap: i64) -> Packet {{\n\
         \x20   if i < cap {{\n\
         \x20       Packet::Payload(\"{payload}\".to_upper())\n\
         \x20   }} else {{\n\
         \x20       Packet::Done\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var i = 0;\n\
         \x20   var q = next(i, {frames});\n\
         \x20   var total = 0;\n\
         \x20   while let Packet::Payload(s) = q {{\n\
         \x20       {payload_use}\n\
         \x20       i = i + 1;\n\
         \x20       q = next(i, {frames});\n\
         \x20   }}\n\
         \x20   if total >= 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

fn fully_bound_source(frames: usize) -> String {
    source(frames, false)
}

fn moved_out_source(frames: usize) -> String {
    source(frames, true)
}

const EXIT_EDGES_SOURCE: &str = r#"
enum Packet {
    Payload(string);
    Done;
}

fn next(i: i64, cap: i64) -> Packet {
    if i < cap {
        Packet::Payload("while-let-backedge-edge-payload-abcdefghijklmnopqrstuvwxyz".to_upper())
    } else {
        Packet::Done
    }
}

fn run_break() {
    var i = 0;
    var q = next(i, 4);
    while let Packet::Payload(s) = q {
        if s.len() > 0 {
            break;
        }
        i = i + 1;
        q = next(i, 4);
    }
}

fn run_tag_false() {
    var i = 0;
    var q = next(i, 4);
    while let Packet::Payload(s) = q {
        i = i + 1;
        q = next(i, 4);
    }
}

fn main() {
    run_break();
    run_tag_false();
    print("ok");
}
"#;

#[test]
fn while_let_reassigned_fully_bound_payload_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("while_let_reassign_fully_bound", fully_bound_source);
}

#[test]
fn while_let_reassigned_moved_out_payload_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("while_let_reassign_moved_out", moved_out_source);
}

#[test]
fn while_let_reassigned_break_and_tag_false_release_exactly_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("while-let-reassign-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(EXIT_EDGES_SOURCE, dir.path(), "while_let_reassign_edges");
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "reassigned while-let edge cleanup must not double-free:\n{}",
        describe_output(&output)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "ok");
}
