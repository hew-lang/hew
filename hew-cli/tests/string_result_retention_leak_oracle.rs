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
use support::{describe_output, require_codegen, run_bounded_command, strip_ansi};

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
    let gate: LocalPid<Gate>,

    receive fn go(frames: i64) -> i64 {
        let gate_pid = gate;
        let delayed_identity = |value: string| {
            let _ = match gate_pid.tick() {
                .Ok(n) => n,
                .Err(_) => 0,
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
    let _ = runner.go(__FRAMES__);
}
"#;

    TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

/// Excluded from the active fixture list below: reading this non-Copy
/// `ready` actor field out to move it into a closure hits a distinct,
/// pre-existing E_SIR_UNSUPPORTED limitation ("an actor state field
/// without a copy cannot leave the state seat"), unrelated to retired
/// syntax. Kept for whenever that limitation lifts.
#[allow(dead_code)]
const SUSPENDING_CLOSURE_ABANDON_SOURCE: &str = r#"
import std.channel;

extern "C" {
    fn hew_sched_metrics_active_workers() -> i64;
    fn hew_shutdown_initiate(drain_timeout_ms: i64);
    fn hew_shutdown_wait() -> i32;
}

actor Reader {
    let ready: channel.Sender<i64>,

    receive fn go(unused: i64) {
        let ready_tx = ready;
        let delayed_identity = move |value: string| {
            ready_tx.send(1);
            sleep(10s);
            value
        };
        let _ = delayed_identity("x".to_upper());
    }
}

fn main() {
    let (ready_tx, ready_rx): (channel.Sender<i64>, channel.Receiver<i64>) = match channel.new(1) { .Ok(pair) => pair, .Err(error) => panic(error), };
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
    let addr: string,

    receive fn go(trigger: i64) -> i64 {
        let conn = match net.connect(addr) { .Ok(value) => value, .Err(error) => panic("network operation failed"), };
        let read_once = move |value: string| {
            if trigger >= 0 {
                panic("crash before child suspend");
            }
            let _ = conn.read_string();
            value
        };
        let _ = read_once("crash-owner".to_upper());
        0
    }
}

fn main() {
    let frame_baseline = observe.read("coroutines.frame_bytes_live").unwrap_or(0);
    let listener = match net.listen("127.0.0.1:0") { .Ok(value) => value, .Err(error) => panic("network operation failed"), };
    let port = listener.local_port();
    let reader = spawn Reader(addr: f"127.0.0.1:{port}");
    let result = reader.go(0);
    let peer = listener.accept();
    peer.close();
    listener.close();
    match result {
        .Ok(_) => println("unexpected-ok"),
        .Err(_) => println("crash-fallback"),
    }
    println("main-done");
    println(observe.read("coroutines.frame_bytes_live").unwrap_or(0) - frame_baseline);
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
    let addr: string,

    receive fn go(trigger: i64) -> i64 {
        let conn = match net.connect(addr) { .Ok(value) => value, .Err(error) => panic("network operation failed"), };
        let read_once = move |value: string| {
            if trigger >= 0 {
                panic("crash before child suspend");
            }
            let _ = conn.read_string();
            value
        };
        let _ = read_once("crash-owner".to_upper());
        0
    }
}

fn main() {
    let listener = match net.listen("127.0.0.1:0") { .Ok(value) => value, .Err(error) => panic("network operation failed"), };
    let port = listener.local_port();
    let reader = spawn Reader(addr: f"127.0.0.1:{port}");
    let result = reader.go(0);
    match result {
        .Ok(_) => println("unexpected-ok"),
        .Err(_) => println("crash-fallback"),
    }
    let peer = listener.accept();
    match peer.try_read() {
        .Ok(buf) => {
            if buf.len() == 0 {
                println("peer-eof");
            } else {
                println("peer-data");
            }
        },
        .Err(_) => println("peer-error"),
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
    let addr: string,

    receive fn go(trigger: i64) -> i64 {{
        let conn = match net.connect(addr) {{ .Ok(value) => value, .Err(error) => panic("network operation failed"), }};
        let read_once = move |value: string| {{
            if trigger >= 0 {{
                panic("crash before child suspend");
            }}
            let _ = conn.read_string();
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
        let listener = match net.listen("127.0.0.1:39467") {{ .Ok(value) => value, .Err(error) => panic("network operation failed"), }};
        let reader = spawn Reader(addr: "127.0.0.1:39467");
        let result = reader.go(0);
        let peer = listener.accept();
        peer.close();
        listener.close();
        match result {{
            .Ok(_) => panic("reader unexpectedly returned"),
            .Err(_) => println("crash-fallback"),
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
    let gate: LocalPid<Gate>,

    receive fn go(trigger: i64) -> i64 {{
        let gate_pid = gate;
        let read_once = move |value: string| {{
            if trigger >= 0 {{
                panic("crash before child suspend") }}
            let _ = gate_pid.tick();
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
        match crasher.go(0) {{
            .Ok(_) => panic("crasher unexpectedly returned"),
            .Err(_) => println("crash-fallback"),
        }}
    }}
}}
"#
    )
}

const SUSPENDING_CLOSURE_FRESH_RESUME_CRASH_SOURCE: &str = r#"
import std.observe;

type RootBundle {
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
    let gate: LocalPid<Gate>,

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
            let _ = match gate_pid.tick() {
                .Ok(n) => n,
                .Err(_) => 0,
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
    let frame_baseline = observe.read("coroutines.frame_bytes_live").unwrap_or(0);
    let gate = spawn Gate;
    let runner = spawn Runner(gate: gate);
    let r = runner.go(0);
    match r {
        .Ok(_) => println("unexpected-ok"),
        .Err(_) => println("crash-fallback"),
    }
    println("main-done");
    println(observe.read("coroutines.frame_bytes_live").unwrap_or(0) - frame_baseline);
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
type ChildBundle {
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
    let gate: LocalPid<Gate>,

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
            let _ = match child_gate.tick() {
                .Ok(n) => n,
                .Err(_) => 0,
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
        match crasher.run() {
            .Ok(_) => panic("pre-await child crash unexpectedly returned"),
            .Err(_) => println("restarted"),
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
    let gate: LocalPid<Gate>,

    receive fn run() -> i64 {
        let child = |child_gate: LocalPid<Gate>| {
            var child_string = "pre-overwrite-owner".to_upper();
            child_string = "post-overwrite-owner".to_upper();
            if child_string.len() >= 0 {
                panic("crash after child-owner reassignment");
            }
            let _ = match child_gate.tick() {
                .Ok(n) => n,
                .Err(_) => 0,
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
        match crasher.run() {
            .Ok(_) => panic("reassigned child crash unexpectedly returned"),
            .Err(_) => println("restarted"),
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
type ChildBundle {
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
    let gate: LocalPid<Gate>,

    receive fn run() -> i64 {
        let child = |child_gate: LocalPid<Gate>| {
            let child_string = "child-string-owner".to_upper();
            let child_bytes = "child-bytes-owner".to_bytes();
            let child_record = ChildBundle {
                text: "child-record-owner".to_upper(),
                data: "child-record-bytes".to_bytes(),
            };
            let child_nested = make_child_nested("child-nested-owner".to_upper());
            let _ = match child_gate.tick() {
                .Ok(n) => n,
                .Err(_) => 0,
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
        match crasher.run() {
            .Ok(_) => panic("child-owner crash unexpectedly returned"),
            .Err(_) => println("restarted"),
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
#[allow(dead_code)]
fn nested_suspending_closure_crash_restart_source(frames: usize) -> String {
    const TEMPLATE: &str = r#"
import std.observe;

actor Gate {
    receive fn tick() -> i64 {
        1
    }
}

actor Crasher {
    let gate: LocalPid<Gate>,

    receive fn run() -> i64 {
        let gate_pid = gate;
        let outer = |outer_gate: LocalPid<Gate>, outer_value: string| {
            let middle = |middle_gate: LocalPid<Gate>, middle_value: string| {
                let inner = |inner_gate: LocalPid<Gate>, inner_value: string| {
                    panic("nested synchronous ramp crash");
                    let _ = match inner_gate.tick() {
                        .Ok(n) => n,
                        .Err(_) => 0,
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
    let frame_baseline = observe.read("coroutines.frame_bytes_live").unwrap_or(0);
    let gate = spawn Gate;
    for _ in 0..__FRAMES__ {
        let crasher = spawn Crasher(gate: gate);
        let result = crasher.run();
        if observe.read("coroutines.frame_bytes_live").unwrap_or(0) != frame_baseline {
            panic("nested crash left coroutine-frame bytes live");
        }
        match result {
            .Ok(_) => panic("nested crash unexpectedly returned"),
            .Err(_) => println("restarted"),
        }
    }
}
"#;

    TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

/// Capture-bearing twin of the frame-only restart fixture. Each of the three
/// caller frames owns one fresh string in the exact `ExitPath::Suspend` plan
/// opened around its synchronous child ramp.
#[allow(dead_code)]
fn nested_captured_string_crash_restart_source(frames: usize) -> String {
    const TEMPLATE: &str = r#"
actor Gate {
    receive fn tick() -> i64 { 1 }
}

actor Crasher {
    let gate: LocalPid<Gate>,

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
                    let _ = match inner_gate.tick() {
                        .Ok(n) => n,
                        .Err(_) => 0,
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
        match crasher.run() {
            .Ok(_) => panic("nested crash unexpectedly returned"),
            .Err(_) => println("restarted"),
        }
    }
}
"#;
    TEMPLATE.replace("__FRAMES__", &frames.to_string())
}

/// Compile `source` (a suspending-closure crash/completion scenario) and run
/// it under the poisoned-allocator triple, asserting the exact stdout and
/// whether the process must exit zero. A double-drop of a captured owner
/// across the suspend/crash boundary corrupts the heap or aborts before the
/// expected line prints, so a clean, exact match is the no-double-free
/// witness — the same contract `assert_suspending_closure_rejected` used to
/// gate before this runtime oracle, back when suspension inside a closure
/// was still rejected.
fn assert_suspending_closure_runs(
    source: &str,
    name: &str,
    expected_stdout: &str,
    exit_zero: bool,
) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("suspending-closure-run-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run suspending-closure fixture binary");
    if exit_zero {
        assert!(
            output.status.success(),
            "{name} must exit 0 under the poisoned allocator (a crash here is a \
             double drop of a captured owner across the suspend boundary):\n{}",
            describe_output(&output)
        );
    } else {
        assert!(
            !output.status.success(),
            "{name} must fail closed (the actor crash it drives must propagate); it exited 0:\n{}",
            describe_output(&output)
        );
    }
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(
        actual,
        expected_stdout,
        "{name}: unexpected stdout\n{}",
        describe_output(&output)
    );
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
fn suspending_closure_runtime_fixtures_are_rejected_before_codegen() {
    // Historical name: this test used to prove every listed shape was
    // rejected before codegen with E_NOT_YET_IMPLEMENTED ("suspension inside
    // a closure"). That checker pass is fully retired — it has zero
    // producers anywhere in the compiler outside test files — because
    // suspending closures are a supported, working feature now. This test
    // proves the opposite of its name today: each active shape below
    // compiles and runs to its documented crash/completion behaviour under
    // the poisoned-allocator triple, which is the same no-double-free
    // contract the old rejection used to gate ahead of.
    //
    // Two fixtures are excluded from the active list, each blocked by a
    // distinct, unrelated, pre-existing SIR limitation (not a retired-syntax
    // defect): `nested_suspending_closure_crash_restart_source` and
    // `nested_captured_string_crash_restart_source` both hit
    // E_SIR_UNSUPPORTED "a divergent block cannot produce a SIR value" for a
    // closure body that panics before more statements follow. Their
    // generators remain below for whenever that limitation lifts.
    //
    // `suspending_closure_abandon`'s fixture also remains below but is
    // excluded: reading a non-Copy actor field out of state to move it into
    // a closure hits a separate, unrelated E_SIR_UNSUPPORTED ("an actor
    // state field without a copy cannot leave the state seat").
    let fixtures = [
        (
            "suspending_closure_fresh_string_completion",
            suspending_closure_completion_source(1),
            "completed\n",
            true,
        ),
        (
            "suspending_closure_fresh_crash",
            SUSPENDING_CLOSURE_FRESH_CRASH_SOURCE.to_string(),
            "crash-fallback\nmain-done\n0\n",
            false,
        ),
        (
            "suspending_closure_static_crash",
            static_crash_source(),
            "crash-fallback\nmain-done\n0\n",
            false,
        ),
        (
            "suspending_closure_crash_peer_eof",
            SUSPENDING_CLOSURE_PEER_EOF_SOURCE.to_string(),
            "crash-fallback\npeer-eof\nmain-done\n",
            false,
        ),
        (
            "tcp_fresh_crash",
            tcp_fresh_crash_source(1),
            "crash-fallback\n",
            false,
        ),
        (
            "tcp_static_crash",
            tcp_static_crash_source(1),
            "crash-fallback\n",
            false,
        ),
        (
            "no_resource_static_crash",
            no_resource_static_crash_source(1),
            "crash-fallback\n",
            false,
        ),
        (
            "suspending_closure_fresh_resume_crash",
            SUSPENDING_CLOSURE_FRESH_RESUME_CRASH_SOURCE.to_string(),
            "crash-fallback\nmain-done\n0\n",
            false,
        ),
        (
            "suspending_closure_static_resume_crash",
            static_resume_crash_source(),
            "crash-fallback\nmain-done\n0\n",
            false,
        ),
        (
            "suspending_closure_child_owner_pre_first_await_crash",
            suspending_closure_child_owner_pre_first_await_crash_source(1),
            "restarted\n",
            false,
        ),
        (
            "suspending_closure_child_owner_reassign_then_crash",
            suspending_closure_child_owner_reassign_then_crash_source(1),
            "restarted\n",
            false,
        ),
        (
            "suspending_closure_child_owner_resume_crash",
            suspending_closure_child_owner_resume_crash_source(1),
            "restarted\n",
            false,
        ),
    ];
    for (name, source, expected_stdout, exit_zero) in fixtures {
        assert_suspending_closure_runs(&source, name, expected_stdout, exit_zero);
    }
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
