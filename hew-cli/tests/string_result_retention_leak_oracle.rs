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
    run_probe_witness, HIGH_FRAMES, LOW_FRAMES,
};
use support::{describe_output, require_codegen, run_bounded_command};

type CrashSource = fn(usize) -> String;

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
import std.channel.channel;

extern "C" {
    fn hew_sched_metrics_active_workers() -> i64;
    fn hew_shutdown_initiate(drain_timeout_ms: i64);
    fn hew_shutdown_wait() -> i32;
}

actor Reader {
    let ready: channel.Sender<i64>;

    receive fn go(unused: i64) {
        let ready_tx = ready;
        let delayed_identity = |value: string| {
            ready_tx.send(1);
            sleep(10s);
            value
        };
        let _ = delayed_identity("x".to_upper());
    }
}

fn main() {
    let (ready_tx, ready_rx): (channel.Sender<i64>, channel.Receiver<i64>) = channel.new(1);
    let reader = spawn Reader(ready: ready_tx);
    reader.go(0);
    let _ = ready_rx.recv();
    ready_rx.close();
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
    exit(0);
}
"#;

const SUSPENDING_CLOSURE_FRESH_CRASH_SOURCE: &str = r#"
import std.net.{Listener};
import std.observe;

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
        0
    }
}

fn main() {
    let frame_baseline = observe.read("coroutines.frame_bytes_live");
    let listener = net.listen("127.0.0.1:0");
    let port = listener.local_port();
    let reader = spawn Reader(addr: f"127.0.0.1:{port}");
    let result = await reader.go(0);
    let peer = listener.accept();
    peer.close();
    listener.close();
    match result {
        Ok(_) => println("unexpected-ok"),
        Err(_) => println("crash-fallback"),
    }
    println("main-done");
    println(observe.read("coroutines.frame_bytes_live") - frame_baseline);
}
"#;

fn static_crash_source() -> String {
    SUSPENDING_CLOSURE_FRESH_CRASH_SOURCE.replace(
        r#"read_once("crash-owner".to_upper())"#,
        r#"read_once("crash-owner")"#,
    )
}

/// The observable-close variant of `SUSPENDING_CLOSURE_FRESH_CRASH_SOURCE`:
/// main keeps the accepted peer of the reader-owned connection alive across
/// the crash and then reads from it. An orderly EOF is only possible when the
/// crash path closed the reader's connection — `leaks(1)` cannot see an
/// unclosed `Connection` (the handle registration keeps it reachable and the
/// file descriptor is not a heap node), so peer-side EOF is the behavioural
/// witness for the close obligation itself.
const SUSPENDING_CLOSURE_PEER_EOF_SOURCE: &str = r#"
import std.net.{Listener};

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
        0
    }
}

fn main() {
    let listener = net.listen("127.0.0.1:0");
    let port = listener.local_port();
    let reader = spawn Reader(addr: f"127.0.0.1:{port}");
    let result = await reader.go(0);
    match result {
        Ok(_) => println("unexpected-ok"),
        Err(_) => println("crash-fallback"),
    }
    let peer = listener.accept();
    match peer.try_read() {
        Ok(buf) => {
            if buf.len() == 0 {
                println("peer-eof");
            } else {
                println("peer-data");
            }
        },
        Err(_) => println("peer-error"),
    }
    peer.close();
    listener.close();
    println("main-done");
}
"#;

/// A bounded low/high-frame crash probe for the TCP resource pair.  Each frame
/// creates one listener, connects one reader-owned connection, accepts the
/// peer, then crashes the reader before its closure can suspend.  The main
/// frame waits for that crash before closing the accepted connection and the
/// listener, so this is an ownership test rather than a shutdown-race test.
fn tcp_resource_crash_source(frames: usize, fresh_argument: bool) -> String {
    let argument = if fresh_argument {
        r#""tcp-crash-owner".to_upper()"#
    } else {
        r#""tcp-crash-owner""#
    };

    format!(
        r#"
import std.net.{{Listener}};

actor Reader {{
    let addr: string;

    receive fn go(trigger: i64) -> i64 {{
        let conn = net.connect(addr);
        let read_once = |value: string| {{
            if trigger >= 0 {{
                panic("crash before child suspend");
            }}
            let _ = await conn.read_string();
            value
        }};
        let _ = read_once({argument});
        0
    }}
}}

fn main() {{
    for _ in 0..{frames} {{
        // Keep the endpoint static: formatting the ephemeral port allocates a
        // String per frame, which would test string concatenation rather than
        // the Listener/Connection lifecycle this oracle owns.
        let listener = net.listen("127.0.0.1:39467");
        let reader = spawn Reader(addr: "127.0.0.1:39467");
        let result = await reader.go(0);
        let peer = listener.accept();
        peer.close();
        listener.close();
        match result {{
            Ok(_) => panic("reader unexpectedly returned"),
            Err(_) => println("crash-fallback"),
        }}
    }}
}}
"#
    )
}

fn tcp_fresh_crash_source(frames: usize) -> String {
    tcp_resource_crash_source(frames, true)
}

fn tcp_static_crash_source(frames: usize) -> String {
    tcp_resource_crash_source(frames, false)
}

/// Static, no-resource baseline for the same synchronous closure-crash path.
/// It proves the exact-zero result below is not an accidental TCP-independent
/// allocator floor while retaining the scheduler/crash shape of the probes.
fn no_resource_static_crash_source(frames: usize) -> String {
    format!(
        r#"
actor Gate {{
    receive fn tick() -> i64 {{ 1 }}
}}

actor Crasher {{
    let gate: LocalPid<Gate>;

    receive fn go(trigger: i64) -> i64 {{
        let gate_pid = gate;
        let read_once = |value: string| {{
            if trigger >= 0 {{
                panic("crash before child suspend");
            }}
            let _ = await gate_pid.tick();
            value
        }};
        let _ = read_once("static-crash-owner");
        0
    }}
}}

fn main() {{
    let gate = spawn Gate;
    for _ in 0..{frames} {{
        let crasher = spawn Crasher(gate: gate);
        match await crasher.go(0) {{
            Ok(_) => panic("crasher unexpectedly returned"),
            Err(_) => println("crash-fallback"),
        }}
    }}
}}
"#
    )
}

fn ordinary_helper_snapshot_normal_source(frames: usize) -> String {
    const TEMPLATE: &str = r#"
record Bundle { text: string, data: bytes }
#[resource] type Witness { fd: i64 }
impl Witness { fn close(self) { println("closed"); } }
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
    let gate: LocalPid<Gate>;
    receive fn go(frames: i64) -> i64 {
        let _ = await gate.tick();
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
    let _ = await runner.go(__FRAMES__);
}
"#;
    TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

fn ordinary_helper_snapshot_crash_source(frames: usize) -> String {
    const TEMPLATE: &str = r#"
record Bundle { text: string, data: bytes }
#[resource] type Witness { fd: i64 }
impl Witness { fn close(self) { println("closed"); } }
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
    let gate: LocalPid<Gate>;
    receive fn go() -> i64 {
        let _ = await gate.tick();
        helper_trap()
    }
}
fn main() {
    let gate = spawn Gate;
    for _ in 0..__FRAMES__ {
        let runner = spawn Runner(gate: gate);
        match await runner.go() {
            Ok(_) => println("unexpected"),
            Err(_) => println("crashed"),
        }
    }
}
"#;
    TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

const SUSPENDING_CLOSURE_FRESH_RESUME_CRASH_SOURCE: &str = r#"
import std.observe;

record RootBundle {
    text: string,
    data: bytes,
}

fn make_root_nested(label: string) -> fn() -> i64 {
    || label.len()
}

actor Gate {
    receive fn tick() -> i64 {
        sleep(5ms);
        1
    }
}

actor Runner {
    let gate: LocalPid<Gate>;

    receive fn go(trigger: i64) -> i64 {
        let gate_pid = gate;
        let root_string = "root-resume-owner".to_upper();
        let root_bytes = "root-resume-bytes".to_bytes();
        let root_record = RootBundle {
            text: "root-record-owner".to_upper(),
            data: "root-record-bytes".to_bytes(),
        };
        let root_nested = make_root_nested("root-nested-owner".to_upper());
        let resume_then_crash = |value: string| {
            let _ = match await gate_pid.tick() {
                Ok(n) => n,
                Err(_) => 0,
            };
            if trigger >= 0 {
                panic("crash after child resume");
            }
            value
        };
        let _ = resume_then_crash("resume-crash-owner".to_upper());
        if root_string.len()
            + root_bytes.len()
            + root_record.text.len()
            + root_record.data.len()
            + root_nested()
            == -1 {
            panic("root owner guard");
        }
        7
    }
}

fn main() {
    let frame_baseline = observe.read("coroutines.frame_bytes_live");
    let gate = spawn Gate;
    let runner = spawn Runner(gate: gate);
    let r = await runner.go(0);
    match r {
        Ok(_) => println("unexpected-ok"),
        Err(_) => println("crash-fallback"),
    }
    println("main-done");
    println(observe.read("coroutines.frame_bytes_live") - frame_baseline);
}
"#;

fn static_resume_crash_source() -> String {
    SUSPENDING_CLOSURE_FRESH_RESUME_CRASH_SOURCE.replace(
        r#"resume_then_crash("resume-crash-owner".to_upper())"#,
        r#"resume_then_crash("resume-crash-owner")"#,
    )
}

/// The child owns every typed value before it reaches its first suspend. They
/// are nevertheless live across the syntactically later await, so `CoroSplit`
/// places them in the child frame. A trap on the initial ramp must therefore
/// drain the `DIRECT_FRAME` registry before raw frame reclamation.
fn suspending_closure_child_owner_pre_first_await_crash_source(frames: usize) -> String {
    const TEMPLATE: &str = r#"
record ChildBundle {
    text: string,
    data: bytes,
}

fn make_child_nested(label: string) -> fn() -> i64 {
    || label.len()
}

actor Gate {
    receive fn tick() -> i64 {
        1
    }
}

actor Crasher {
    let gate: LocalPid<Gate>;

    receive fn run() -> i64 {
        let child = |child_gate: LocalPid<Gate>| {
            let child_string = "pre-await-string-owner".to_upper();
            let child_bytes = "pre-await-bytes-owner".to_bytes();
            let child_record = ChildBundle {
                text: "pre-await-record-owner".to_upper(),
                data: "pre-await-record-bytes".to_bytes(),
            };
            let child_nested = make_child_nested("pre-await-nested-owner".to_upper());
            if child_string.len()
                + child_bytes.len()
                + child_record.text.len()
                + child_record.data.len()
                + child_nested()
                >= 0 {
                panic("crash before first child await");
            }
            let _ = match await child_gate.tick() {
                Ok(n) => n,
                Err(_) => 0,
            };
            child_string.len()
                + child_bytes.len()
                + child_record.text.len()
                + child_record.data.len()
                + child_nested()
        };
        child(gate)
    }
}

fn main() {
    let gate = spawn Gate;
    for _ in 0..__FRAMES__ {
        let crasher = spawn Crasher(gate: gate);
        match await crasher.run() {
            Ok(_) => panic("pre-await child crash unexpectedly returned"),
            Err(_) => println("restarted"),
        }
    }
}
"#;
    TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

/// Reassignment sibling for the initial-ramp crash. The first heap string must
/// be released by the overwrite, the stable registry token must be rearmed for
/// the second string, and the crash drain must release only that current owner.
fn suspending_closure_child_owner_reassign_then_crash_source(frames: usize) -> String {
    const TEMPLATE: &str = r#"
actor Gate {
    receive fn tick() -> i64 {
        1
    }
}

actor Crasher {
    let gate: LocalPid<Gate>;

    receive fn run() -> i64 {
        let child = |child_gate: LocalPid<Gate>| {
            var child_string = "pre-overwrite-owner".to_upper();
            child_string = "post-overwrite-owner".to_upper();
            if child_string.len() >= 0 {
                panic("crash after child-owner reassignment");
            }
            let _ = match await child_gate.tick() {
                Ok(n) => n,
                Err(_) => 0,
            };
            child_string.len()
        };
        child(gate)
    }
}

fn main() {
    let gate = spawn Gate;
    for _ in 0..__FRAMES__ {
        let crasher = spawn Crasher(gate: gate);
        match await crasher.run() {
            Ok(_) => panic("reassigned child crash unexpectedly returned"),
            Err(_) => println("restarted"),
        }
    }
}
"#;
    TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

/// Child-frame counterpart to the resumed-root fixture. Every owner below is
/// allocated inside the suspending closure before its await and is read only
/// after the child resumes. A resumed panic therefore abandons the running
/// child frame before its ordinary return or parked-destroy drop authority can
/// run.
fn suspending_closure_child_owner_resume_crash_source(frames: usize) -> String {
    const TEMPLATE: &str = r#"
record ChildBundle {
    text: string,
    data: bytes,
}

fn make_child_nested(label: string) -> fn() -> i64 {
    || label.len()
}

actor Gate {
    receive fn tick() -> i64 {
        1
    }
}

actor Crasher {
    let gate: LocalPid<Gate>;

    receive fn run() -> i64 {
        let child = |child_gate: LocalPid<Gate>| {
            let child_string = "child-string-owner".to_upper();
            let child_bytes = "child-bytes-owner".to_bytes();
            let child_record = ChildBundle {
                text: "child-record-owner".to_upper(),
                data: "child-record-bytes".to_bytes(),
            };
            let child_nested = make_child_nested("child-nested-owner".to_upper());
            let _ = match await child_gate.tick() {
                Ok(n) => n,
                Err(_) => 0,
            };
            if child_string.len()
                + child_bytes.len()
                + child_record.text.len()
                + child_record.data.len()
                + child_nested()
                >= 0 {
                panic("crash after child-owner resume");
            }
            7
        };
        child(gate)
    }
}

fn main() {
    let gate = spawn Gate;
    for _ in 0..__FRAMES__ {
        let crasher = spawn Crasher(gate: gate);
        match await crasher.run() {
            Ok(_) => panic("child-owner crash unexpectedly returned"),
            Err(_) => println("restarted"),
        }
    }
}
"#;
    TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

/// Three nested suspending-closure ramps per actor crash, repeated through a
/// fresh actor restart loop. Each line is printed only after the crash fallback
/// has resolved and the live coroutine-frame byte gauge has returned to the
/// main coroutine's baseline.
fn nested_suspending_closure_crash_restart_source(frames: usize) -> String {
    const TEMPLATE: &str = r#"
import std.observe;

actor Gate {
    receive fn tick() -> i64 {
        1
    }
}

actor Crasher {
    let gate: LocalPid<Gate>;

    receive fn run() -> i64 {
        let gate_pid = gate;
        let outer = |outer_gate: LocalPid<Gate>, outer_value: string| {
            let middle = |middle_gate: LocalPid<Gate>, middle_value: string| {
                let inner = |inner_gate: LocalPid<Gate>, inner_value: string| {
                    panic("nested synchronous ramp crash");
                    let _ = match await inner_gate.tick() {
                        Ok(n) => n,
                        Err(_) => 0,
                    };
                    inner_value
                };
                inner(middle_gate, middle_value)
            };
            middle(outer_gate, outer_value)
        };
        let _ = outer(gate_pid, "nested-crash-owner".to_upper());
        7
    }
}

fn main() {
    let frame_baseline = observe.read("coroutines.frame_bytes_live");
    let gate = spawn Gate;
    for _ in 0..__FRAMES__ {
        let crasher = spawn Crasher(gate: gate);
        let result = await crasher.run();
        if observe.read("coroutines.frame_bytes_live") != frame_baseline {
            panic("nested crash left coroutine-frame bytes live");
        }
        match result {
            Ok(_) => panic("nested crash unexpectedly returned"),
            Err(_) => println("restarted"),
        }
    }
}
"#;

    TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

/// Capture-bearing twin of the frame-only restart fixture. Each of the three
/// caller frames owns one fresh string in the exact `ExitPath::Suspend` plan
/// opened around its synchronous child ramp.
fn nested_captured_string_crash_restart_source(frames: usize) -> String {
    const TEMPLATE: &str = r#"
actor Gate {
    receive fn tick() -> i64 { 1 }
}

actor Crasher {
    let gate: LocalPid<Gate>;

    receive fn run() -> i64 {
        let outer_owner = "outer-crash-owner".to_upper();
        let outer = |outer_gate: LocalPid<Gate>| {
            if outer_owner == "unreachable" { panic("outer capture guard"); }
            let middle_owner = "middle-crash-owner".to_upper();
            let middle = |middle_gate: LocalPid<Gate>| {
                if middle_owner == "unreachable" { panic("middle capture guard"); }
                let inner_owner = "inner-crash-owner".to_upper();
                let inner = |inner_gate: LocalPid<Gate>| {
                    if inner_owner == "unreachable" { panic("inner capture guard"); }
                    panic("nested synchronous ramp crash");
                    let _ = match await inner_gate.tick() {
                        Ok(n) => n,
                        Err(_) => 0,
                    };
                    inner_owner
                };
                inner(middle_gate)
            };
            middle(outer_gate)
        };
        let _ = outer(gate);
        7
    }
}

fn main() {
    let gate = spawn Gate;
    for _ in 0..__FRAMES__ {
        let crasher = spawn Crasher(gate: gate);
        match await crasher.run() {
            Ok(_) => panic("nested crash unexpectedly returned"),
            Err(_) => println("restarted"),
        }
    }
}
"#;
    TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

// Only the capture-escrow-spec IR oracle reads a specific function body out of
// the emitted LLVM IR; gated with its sole caller so the default build carries
// no dead helper (issue #2863).
#[cfg(feature = "capture-escrow-spec")]
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

/// Red-first spec for the capture-escrow feature (issue #2863): asserts a
/// captured owner's crash-cleanup moves INTO the closure child. Current codegen
/// is already memory-safe (the peer-EOF oracle below proves it closes exactly
/// once) but arms the cleanup in the PARENT frame, so this assertion fails until
/// #2863 lands. It is compiled out of the default build behind the
/// `capture-escrow-spec` feature rather than `#[ignore]`d: an ignored test the
/// reachability gate cannot see a CI target behind is dark-zoned, whereas a
/// feature-gated body is honestly absent until re-enabled. RC2 re-enables it by
/// building `-p hew-cli --features capture-escrow-spec` (or by deleting this cfg
/// when #2863 makes it green).
#[cfg(feature = "capture-escrow-spec")]
#[test]
fn suspending_closure_codegen_uses_one_typed_fresh_arg_cleanup_authority() {
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
    let typed_arm = "call i64 @hew_cont_crash_cleanup_arm";

    assert_eq!(
        fresh_handler.matches(typed_arm).count(),
        1,
        "the fresh string producer must arm one typed cleanup authority:\n{fresh_handler}"
    );
    assert_eq!(
        static_handler.matches(typed_arm).count(),
        0,
        "a borrowed static literal carries no typed crash-unwind ownership obligation:\n\
         {static_handler}"
    );
}

/// Crash-path close-EXACTLY-once witness for the reader-owned connection of a
/// suspending closure, from the peer's point of view.
///
/// - **At least once**: the peer's `try_read` must return an orderly EOF.
///   An unclosed connection blocks the bounded read until its timeout and the
///   run fails — the leak polarity a `leaks(1)` slope cannot detect.
/// - **At most once**: the run executes under the poisoned-allocator pair
///   (`MallocScribble`/`MallocPreScribble`), so a second cleanup authority
///   releasing the same connection touches scribbled memory and aborts before
///   stdout settles — the double-free polarity.
///
/// Both polarities must keep holding through any change to how the typed
/// crash-cleanup arms in `Reader__recv__go` and its closure frame are
/// distributed (the arm-count oracle above counts sites; this one proves the
/// close behaviour those sites exist for).
#[test]
fn suspending_closure_crash_peer_observes_connection_close_exactly_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("suspending-closure-peer-eof-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        SUSPENDING_CLOSURE_PEER_EOF_SOURCE,
        dir.path(),
        "suspending_closure_crash_peer_eof",
    );
    let mut command = Command::new(&bin);
    command
        .env("HEW_WORKERS", "1")
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1");
    let output = run_bounded_command(command, "run suspending-closure peer-EOF crash fixture");
    assert_eq!(
        output.status.code(),
        Some(1),
        "peer-EOF crash fixture failed (a hang here means the crash path never \
         closed the reader connection; an abort means it was closed twice):\n{}",
        describe_output(&output)
    );
    assert!(String::from_utf8_lossy(&output.stderr).contains("crash before child suspend"));
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "crash-fallback\npeer-eof\nmain-done\n",
        "the peer must observe an orderly EOF exactly once after the reader \
         crashes, before main's own controls close"
    );
}

#[test]
fn suspending_closure_sync_crash_explicitly_closes_tcp_controls() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("suspending-closure-crash-tcp-controls-")
        .tempdir()
        .expect("tempdir");
    let fresh_bin = compile_to_native(
        SUSPENDING_CLOSURE_FRESH_CRASH_SOURCE,
        dir.path(),
        "suspending_closure_fresh_crash_tcp_controls",
    );
    let static_bin = compile_to_native(
        &static_crash_source(),
        dir.path(),
        "suspending_closure_static_crash_tcp_controls",
    );

    for (label, bin) in [("fresh", &fresh_bin), ("static", &static_bin)] {
        let output = run_fixture(bin, &format!("run {label} crash TCP-control fixture"));
        assert_eq!(
            output.status.code(),
            Some(1),
            "{label} crash TCP-control fixture failed:\n{}",
            describe_output(&output)
        );
        assert!(String::from_utf8_lossy(&output.stderr).contains("crash before child suspend"));
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            "crash-fallback\nmain-done\n0\n",
            "{label} must await the crashing reader after explicitly closing \
             both the listener and accepted peer; only the reader connection \
             remains for typed crash cleanup"
        );
    }
}

#[test]
fn suspending_closure_later_resume_crash_drains_typed_root_owners_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("suspending-closure-resume-root-owners-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        SUSPENDING_CLOSURE_FRESH_RESUME_CRASH_SOURCE,
        dir.path(),
        "suspending_closure_resume_root_owners",
    );
    let output = run_fixture(&bin, "run later-resume typed-root cleanup fixture");
    assert_eq!(
        output.status.code(),
        Some(1),
        "later-resume typed-root cleanup fixture failed (a poisoned-header abort \
         here is a double drop):\n{}",
        describe_output(&output)
    );
    assert!(String::from_utf8_lossy(&output.stderr).contains("crash after child resume"));
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "crash-fallback\nmain-done\n0\n",
        "string, Bytes, record, and nested-closure owners must be drained before \
         the crashed root frame is reclaimed"
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
        assert_eq!(
            output.status.code(),
            Some(1),
            "{label} synchronous-crash fixture failed:\n{}",
            describe_output(&output)
        );
        assert!(String::from_utf8_lossy(&output.stderr).contains("crash before child suspend"));
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            "crash-fallback\nmain-done\n0\n",
            "{label} fixture must close the listener and accepted peer explicitly, \
             then await the crash teardown before reporting live coroutine-frame bytes"
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
    assert_eq!(
        static_leaks,
        (0, 0),
        "explicit listener and accepted-peer closes leave no TCP baseline: the \
         reader connection and both coroutine frames must be reclaimed by their \
         typed crash cleanup"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact TCP crash leak evidence needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn tcp_crash_resource_lifecycles_are_exact_zero_at_low_and_high_frames() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("tcp-crash-resource-lifecycle-")
        .tempdir()
        .expect("tempdir");

    // The low/high pair detects a per-frame owner leak.  The static TCP
    // variant controls fresh-string ownership, while the no-resource variant
    // controls the same actor/closure crash path without either resource.
    let sources: [(&str, CrashSource); 3] = [
        ("tcp_fresh", tcp_fresh_crash_source),
        ("tcp_static", tcp_static_crash_source),
        ("no_resource_static", no_resource_static_crash_source),
    ];
    for (label, source) in sources {
        for frames in [LOW_FRAMES, HIGH_FRAMES] {
            let bin = compile_to_native(
                &source(frames),
                dir.path(),
                &format!("{label}_{frames}_frames"),
            );
            let lines = run_probe_witness(&bin, &[]);
            assert_eq!(
                lines, frames,
                "{label} must complete all {frames} crash/cleanup frames before its leak \
                 measurement is trusted"
            );
            let leaks = measure_leaks_exact(&bin);
            assert_eq!(
                leaks,
                (0, 0),
                "{label} must release every owner at {frames} frames; leaks={leaks:?}"
            );
        }
    }
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact crash differential needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn suspending_closure_later_resume_crash_reclaims_nested_frame_only() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("suspending-closure-resume-crash-")
        .tempdir()
        .expect("tempdir");
    let fresh_bin = compile_to_native(
        SUSPENDING_CLOSURE_FRESH_RESUME_CRASH_SOURCE,
        dir.path(),
        "suspending_closure_fresh_resume_crash",
    );
    let static_bin = compile_to_native(
        &static_resume_crash_source(),
        dir.path(),
        "suspending_closure_static_resume_crash",
    );

    for (label, bin) in [("fresh", &fresh_bin), ("static", &static_bin)] {
        let output = run_fixture(bin, &format!("run {label} later-resume crash fixture"));
        assert_eq!(
            output.status.code(),
            Some(1),
            "{label} later-resume crash fixture failed:\n{}",
            describe_output(&output)
        );
        assert!(String::from_utf8_lossy(&output.stderr).contains("crash after child resume"));
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            "crash-fallback\nmain-done\n0\n",
            "{label} later-resume crash must preserve the scheduler root's sole \
             teardown while returning live frame bytes to baseline"
        );
    }

    let fresh_leaks = measure_leaks_exact(&fresh_bin);
    let static_leaks = measure_leaks_exact(&static_bin);
    assert_eq!(
        fresh_leaks, static_leaks,
        "a later child-resume crash must add neither a frame leak nor a fresh \
         string leak over the static control; fresh={fresh_leaks:?}, \
         static={static_leaks:?}"
    );
    assert_eq!(
        static_leaks,
        (0, 0),
        "the actor-ask resume fixture carries no unrelated typed resource floor"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact child-frame crash leak slope needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn suspending_closure_child_owners_survive_await_without_crash_leaks() {
    assert_frame_slope_below_tolerance_exact_lines(
        "suspending_closure_child_owner_resume_crash",
        suspending_closure_child_owner_resume_crash_source,
        std::convert::identity,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact initial-ramp child-frame crash leak slope needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn suspending_closure_child_owners_crash_before_first_await_without_leaks() {
    assert_frame_slope_below_tolerance_exact_lines(
        "suspending_closure_child_owner_pre_first_await_crash",
        suspending_closure_child_owner_pre_first_await_crash_source,
        std::convert::identity,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact reassigned child-frame crash leak slope needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn suspending_closure_child_owner_reassignment_rearms_current_value_once() {
    assert_frame_slope_below_tolerance_exact_lines(
        "suspending_closure_child_owner_reassign_then_crash",
        suspending_closure_child_owner_reassign_then_crash_source,
        std::convert::identity,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn nested_suspending_closure_crash_restart_has_zero_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "nested_suspending_closure_crash_restart",
        nested_suspending_closure_crash_restart_source,
        std::convert::identity,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact crash leak slope needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn nested_captured_strings_crash_restart_has_zero_leak_slope() {
    assert_frame_slope_below_tolerance_exact_lines(
        "nested_captured_strings_crash_restart",
        nested_captured_string_crash_restart_source,
        std::convert::identity,
    );
}
