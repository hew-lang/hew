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

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, measure_leaks_exact,
};
use support::{describe_output, require_codegen, run_bounded_command};

fn markdown_wrapper_source(frames: usize) -> String {
    format!(
        "import std::encoding::markdown;\n\
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

/// A real `SuspendKind::CallClosure` completion probe. Every iteration hands a
/// fresh string share to a closure that awaits an actor ask before returning
/// it. With one scheduler worker the target actor cannot run until the caller
/// parks, so every exact-line witness crosses the suspend/resume boundary.
fn suspending_closure_completion_source(frames: usize) -> String {
    const TEMPLATE: &str = r#"
actor Gate {
    receive fn tick() -> i64 {
        1
    }
}

fn borrow_len(value: string) -> i64 {
    value.len()
}

actor Runner {
    let gate: LocalPid<Gate>;

    receive fn go(frames: i64) -> i64 {
        let gate_pid = gate;
        let delayed_identity = |value: string| {
            let _ = match await gate_pid.tick() {
                Ok(n) => n,
                Err(_) => 0,
            };
            value
        };
        for _ in 0..frames {
            let returned = delayed_identity("suspending-owner".to_upper());
            if borrow_len(returned) != 16 {
                panic("suspending closure returned the wrong string");
            }
            println("completed");
        }
        frames
    }
}

fn main() {
    let gate = spawn Gate;
    let runner = spawn Runner(gate: gate);
    let _ = await runner.go(__FRAMES__);
}
"#;

    TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

const SUSPENDING_CLOSURE_ABANDON_SOURCE: &str = r#"
import std::net::{Listener};
import std::observe;

extern "C" {
    fn hew_tcp_listener_local_port(listener: Listener) -> i32;
    fn hew_sched_metrics_active_workers() -> i64;
    fn hew_shutdown_initiate(drain_timeout_ms: i64);
    fn hew_shutdown_wait() -> i32;
}

actor Reader {
    let addr: string;

    receive fn go(unused: i64) {
        let conn = net.connect(addr);
        let delayed_identity = |value: string| {
            let _ = await conn.read_string();
            value
        };
        let _ = delayed_identity("x".to_upper());
    }
}

fn main() {
    let listener = net.listen("127.0.0.1:0");
    let port = unsafe {
        hew_tcp_listener_local_port(listener)
    };
    let reader = spawn Reader(addr: f"127.0.0.1:{port}");
    reader.go(0);
    let _peer = listener.accept();
    while observe.read("reactor.registrations_live") == 0 {}
    while unsafe {
        hew_sched_metrics_active_workers()
    } != 0 {}
    println("parked");
    unsafe {
        hew_shutdown_initiate(0);
    };
    let _ = unsafe {
        hew_shutdown_wait()
    };
    println("shutdown");
}
"#;

const SUSPENDING_CLOSURE_FRESH_CRASH_SOURCE: &str = r#"
import std::net::{Listener};

extern "C" {
    fn hew_tcp_listener_local_port(listener: Listener) -> i32;
}

actor Reader {
    let addr: string;

    receive fn go(trigger: i64) -> i64 {
        let conn = net.connect(addr);
        let read_once = |value: string| {
            if trigger >= 0 {
                panic("crash before child suspend");
            }
            let _ = await conn.read_string();
            value
        };
        let _ = read_once("crash-owner".to_upper());
        7
    }
}

fn main() {
    let listener = net.listen("127.0.0.1:0");
    let port = unsafe {
        hew_tcp_listener_local_port(listener)
    };
    let reader = spawn Reader(addr: f"127.0.0.1:{port}");
    let r = await reader.go(0);
    match r {
        Ok(_) => println("unexpected-ok"),
        Err(_) => println("crash-fallback"),
    }
    println("main-done");
}
"#;

fn static_crash_source() -> String {
    SUSPENDING_CLOSURE_FRESH_CRASH_SOURCE.replace(
        r#"read_once("crash-owner".to_upper())"#,
        r#"read_once("crash-owner")"#,
    )
}

fn llvm_function_body<'a>(ir: &'a str, name: &str) -> &'a str {
    let marker = format!("@{name}(");
    let function_start = ir
        .match_indices("define ")
        .filter(|(start, _)| ir[*start..].contains(&marker))
        .find_map(|(start, _)| {
            let line_end = ir[start..].find('\n').map(|offset| start + offset)?;
            ir[start..line_end].contains(&marker).then_some(start)
        })
        .unwrap_or_else(|| panic!("missing LLVM function `{name}`"));
    let tail = &ir[function_start..];
    let function_end = tail
        .find("\n}\n")
        .unwrap_or_else(|| panic!("unterminated LLVM function `{name}`"))
        + 3;
    &tail[..function_end]
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

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn suspending_closure_fresh_string_argument_has_no_completion_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "suspending_closure_fresh_string_completion",
        suspending_closure_completion_source,
        std::convert::identity,
    );
}

#[test]
fn suspending_closure_parked_abandon_completes_shutdown() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("suspending-closure-abandon-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        SUSPENDING_CLOSURE_ABANDON_SOURCE,
        dir.path(),
        "suspending_closure_abandon",
    );
    let output = run_fixture(&bin, "run suspending-closure parked-abandon fixture");
    assert!(
        output.status.success(),
        "parked-abandon fixture failed:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "parked\nshutdown\n",
        "the fixture must prove the fresh argument was live across a real park \
         before the runtime destroyed the suspended handler"
    );
}

#[test]
fn suspending_closure_codegen_registers_fresh_arg_for_initial_ramp_and_resume() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("suspending-closure-codegen-")
        .tempdir()
        .expect("tempdir");
    let fresh_bin = compile_to_native(
        SUSPENDING_CLOSURE_FRESH_CRASH_SOURCE,
        dir.path(),
        "suspending_closure_fresh_crash_codegen",
    );
    let static_bin = compile_to_native(
        &static_crash_source(),
        dir.path(),
        "suspending_closure_static_crash_codegen",
    );
    let fresh_ir =
        std::fs::read_to_string(fresh_bin.with_extension("ll")).expect("read fresh LLVM IR");
    let static_ir =
        std::fs::read_to_string(static_bin.with_extension("ll")).expect("read static LLVM IR");
    let fresh_handler = llvm_function_body(&fresh_ir, "Reader__recv__go");
    let static_handler = llvm_function_body(&static_ir, "Reader__recv__go");
    let registration = "call void @hew_context_reply_channel_swap_add_string_cleanup";

    assert_eq!(
        fresh_handler.matches(registration).count(),
        2,
        "the fresh string argument must be registered once for the initial child \
         ramp and once for a later child resume:\n{fresh_handler}"
    );
    assert_eq!(
        static_handler.matches(registration).count(),
        0,
        "a borrowed static literal carries no crash-unwind ownership obligation:\n\
         {static_handler}"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact crash differential needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn suspending_closure_sync_crash_releases_only_the_fresh_argument_delta() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("suspending-closure-crash-")
        .tempdir()
        .expect("tempdir");
    let fresh_bin = compile_to_native(
        SUSPENDING_CLOSURE_FRESH_CRASH_SOURCE,
        dir.path(),
        "suspending_closure_fresh_crash",
    );
    let static_bin = compile_to_native(
        &static_crash_source(),
        dir.path(),
        "suspending_closure_static_crash",
    );

    for (label, bin) in [("fresh", &fresh_bin), ("static", &static_bin)] {
        let output = run_fixture(bin, &format!("run {label} synchronous-crash fixture"));
        assert!(
            output.status.success(),
            "{label} synchronous-crash fixture failed:\n{}",
            describe_output(&output)
        );
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            "crash-fallback\nmain-done\n",
            "{label} fixture must exercise the actor crash fallback and return \
             control to main"
        );
    }

    let fresh_leaks = measure_leaks_exact(&fresh_bin);
    let static_leaks = measure_leaks_exact(&static_bin);
    assert_eq!(
        fresh_leaks, static_leaks,
        "the heap-producing argument must add no leak over the static-literal \
         control after a synchronous child-ramp crash; fresh={fresh_leaks:?}, \
         static={static_leaks:?}"
    );
}
