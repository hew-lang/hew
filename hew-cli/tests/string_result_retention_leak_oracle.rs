//! Caller-side release oracle for fresh strings returned through shipped Hew
//! standard-library wrappers.
//!
//! Red baseline at `f7b703131`, before `hew_markdown_to_html` had a measured
//! retention row: 3 calls leaked 3 nodes / 192 bytes and 50 calls leaked
//! 50 nodes / 3,200 bytes. The admitted contract makes both probes exact zero.

#![cfg(unix)]

mod support;

use std::path::Path;
use std::process::Command;

use support::leak_slope::{assert_frame_slope_below_tolerance_exact_lines, compile_to_native};
use support::{describe_output, require_codegen, run_bounded_command};

fn markdown_wrapper_source(frames: usize) -> String {
    format!(
        "import std.encoding.markdown;\n\
         fn main() {{\n\
         \x20   for _ in 0..{frames} {{\n\
         \x20       println(markdown.to_html(\"# retained-owner probe\").len());\n\
         \x20   }}\n\
         }}\n"
    )
}

/// Closure-invoke carrier probe. Each frame exercises the three return sources
/// a compiler-generated invoke shim normalizes to one caller-owned share:
///
/// - a captured string (`ClosureEnvFieldLoad`, retained by codegen);
/// - a heap-producing by-value string argument (the caller releases its share,
///   while the closure retains the returned parameter at the return edge);
/// - a fresh transform result (its existing `+1` transfers).
///
/// The fourth call forwards the captured result through a Hew wrapper whose
/// tail is indirect. Before the string-only carrier authority, both a direct
/// `borrow_len(make())` and `borrow_len(invoke(make))` leaked one allocation per
/// call because `CallClosure` was not a fresh-string producer and the wrapper's
/// general return provenance was intentionally `OPAQUE`.
fn closure_carrier_source(frames: usize) -> String {
    format!(
        "fn borrow_len(value: string) -> i64 {{ value.len() }}\n\
         fn invoke(make: fn() -> string) -> string {{ make() }}\n\
         fn main() {{\n\
         \x20   let seed = \"captured-owner\".to_upper();\n\
         \x20   let captured = || seed;\n\
         \x20   let parameter = |value: string| value;\n\
         \x20   let fresh = || \"fresh-owner\".to_upper();\n\
         \x20   for _ in 0..{frames} {{\n\
         \x20       println(borrow_len(captured()));\n\
         \x20       println(borrow_len(parameter(\"parameter-owner\".to_upper())));\n\
         \x20       println(borrow_len(fresh()));\n\
         \x20       println(borrow_len(invoke(captured)));\n\
         \x20   }}\n\
         }}\n"
    )
}

fn ordinary_helper_snapshot_normal_source(frames: usize) -> String {
    const TEMPLATE: &str = r#"
type Bundle { text: string, data: bytes }
#[resource] type Witness { fd: i64 }
impl Witness { fn close(consume self) { println("closed"); } }
fn make_nested(label: string) -> fn() -> i64 { || label.len() }
fn helper_normal() -> i64 {
    let text = "helper-string".to_upper();
    let data = "helper-bytes".to_bytes();
    let bundle = Bundle {
        text: "helper-record".to_upper(),
        data: "helper-record-bytes".to_bytes(),
    };
    let witness = Witness { fd: 7 };
    let nested = make_nested("helper-nested".to_upper());
    let table = HashMap.new<string, i64>();
    text.len() + data.len() + bundle.text.len() + bundle.data.len()
        + witness.fd + nested() + table.len()
}
actor Gate { receive fn tick() -> i64 { 1 } }
actor Runner {
    let gate: LocalPid<Gate>,
    receive fn go(frames: i64) -> i64 {
        let _ = gate.tick();
        for _ in 0..frames {
            if helper_normal() < 0 { panic("impossible"); }
            println("completed");
        }
        frames
    }
}
fn main() {
    let gate = spawn Gate;
    let runner = spawn Runner(gate: gate);
    let _ = runner.go(__FRAMES__);
}
"#;
    TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

fn ordinary_helper_snapshot_crash_source(frames: usize) -> String {
    const TEMPLATE: &str = r#"
type Bundle { text: string, data: bytes }
#[resource] type Witness { fd: i64 }
impl Witness { fn close(consume self) { println("closed"); } }
fn make_nested(label: string) -> fn() -> i64 { || label.len() }
fn helper_trap() -> i64 {
    let text = "helper-string".to_upper();
    let data = "helper-bytes".to_bytes();
    let bundle = Bundle {
        text: "helper-record".to_upper(),
        data: "helper-record-bytes".to_bytes(),
    };
    let witness = Witness { fd: 7 };
    let nested = make_nested("helper-nested".to_upper());
    let table = HashMap.new<string, i64>();
    text.len() + data.len() + bundle.text.len() + bundle.data.len()
        + witness.fd + nested() + table["missing"]
}
actor Gate { receive fn tick() -> i64 { 1 } }
actor Runner {
    let gate: LocalPid<Gate>,
    receive fn go() -> i64 {
        let _ = gate.tick();
        helper_trap()
    }
}
fn main() {
    let gate = spawn Gate;
    for _ in 0..__FRAMES__ {
        let runner = spawn Runner(gate: gate);
        match runner.go() {
            .Ok(_) => println("unexpected"),
            .Err(_) => println("crashed"),
        }
    }
}
"#;
    TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

fn run_fixture(bin: &Path, label: &str) -> std::process::Output {
    let mut command = Command::new(bin);
    command.env("HEW_WORKERS", "1");
    run_bounded_command(command, label)
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn shipped_markdown_string_wrapper_has_no_per_call_leak() {
    assert_frame_slope_below_tolerance_exact_lines(
        "markdown_string_wrapper",
        markdown_wrapper_source,
        std::convert::identity,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn closure_invoke_string_returns_have_no_per_call_leak() {
    assert_frame_slope_below_tolerance_exact_lines(
        "closure_invoke_string_carrier",
        closure_carrier_source,
        |frames| frames * 4,
    );
}

#[test]
fn ordinary_helper_snapshot_normal_return_drops_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("ordinary-helper-snapshot-normal-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &ordinary_helper_snapshot_normal_source(3),
        dir.path(),
        "ordinary_helper_snapshot_normal",
    );
    let output = run_fixture(&bin, "run ordinary-helper snapshot normal fixture");
    assert!(
        output.status.success(),
        "ordinary-helper normal fixture failed (a poisoned-header abort here is a \
         double drop):\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "closed\ncompleted\nclosed\ncompleted\nclosed\ncompleted\n",
        "normal lexical retirement must leave MIR as the sole typed-drop authority"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "helper snapshot leak slope needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn ordinary_helper_snapshot_normal_return_has_zero_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "ordinary_helper_snapshot_normal",
        ordinary_helper_snapshot_normal_source,
        |frames| frames * 2,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "helper raw-trap leak slope needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn ordinary_helper_snapshot_raw_trap_has_zero_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "ordinary_helper_snapshot_raw_trap",
        ordinary_helper_snapshot_crash_source,
        |frames| frames * 2,
    );
}
