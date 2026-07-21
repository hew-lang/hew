//! Compiled leak and poisoned-allocator oracle for actor snapshot sends.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn fanout_source(frames: usize) -> String {
    format!(
        r#"
type Boxed {{
    payload: Vec<i64>,
}}

actor Sink {{
    var seen: i64;

    receive fn take(value: Boxed, tag: i64) {{
        value.payload.push(tag);
        if value.payload[2] == tag {{
            seen = seen + 1;
        }}
    }}

    receive fn count() -> i64 {{
        seen
    }}
}}

fn main() -> i64 {{
    let a = spawn Sink(seen: 0);
    let b = spawn Sink(seen: 0);
    var sender_ok: i64 = 0;
    var i: i64 = 0;
    while i < {frames} {{
        let value = Boxed {{ payload: [1, 2] }};
        a.take(value, 7);
        b.take(value, 8);
        value.payload.push(9);
        if value.payload[2] == 9 {{
            sender_ok = sender_ok + 1;
        }}
        i = i + 1;
    }}
    let av = match await a.count() {{ Ok(v) => v, Err(_) => -1 }};
    let bv = match await b.count() {{ Ok(v) => v, Err(_) => -1 }};
    print(f"{{av}}:{{bv}}:{{sender_ok}}");
    0
}}
"#
    )
}

fn last_use_source(frames: usize) -> String {
    format!(
        r"
type Boxed {{
    payload: Vec<i64>,
}}

actor Sink {{
    var seen: i64;
    receive fn take(value: Boxed) {{
        seen = seen + value.payload.len();
    }}
    receive fn count() -> i64 {{ seen }}
}}

fn main() -> i64 {{
    let sink = spawn Sink(seen: 0);
    var i: i64 = 0;
    while i < {frames} {{
        sink.take(Boxed {{ payload: [i] }});
        i = i + 1;
    }}
    match await sink.count() {{
        Ok(v) => {{ if v == {frames} {{ 0 }} else {{ 1 }} }},
        Err(_) => 2,
    }}
}}
"
    )
}

#[test]
fn snapshot_send_fanout_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("snapshot-send-fanout", fanout_source);
}

#[test]
fn snapshot_send_last_use_transfer_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("snapshot-send-last-use", last_use_source);
}

#[test]
fn snapshot_send_fanout_is_scribble_clean_with_exact_output() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("snapshot-send-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(&fanout_source(6), dir.path(), "snapshot_send_scribble");
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "snapshot fan-out must be clean under MallocScribble:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "6:6:6",
        "receiver snapshots and sender mutation must remain isolated:\n{}",
        describe_output(&output)
    );
}
