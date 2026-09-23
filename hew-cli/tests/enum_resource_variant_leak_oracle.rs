#![cfg(unix)]

mod support;

use std::fmt::Write as _;
use std::path::Path;

use support::leak_slope::{
    compile_to_native, measure_leaks_exact, require_leaks_tool, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

const RESOURCE_FRAMES: usize = 8;

fn resource_enum_source(frames: usize) -> String {
    format!(
        "\
#[opaque]\n\
type Dq {{}}\n\
#[resource]\n\
type Handle {{ raw: Dq, }}\n\
impl Handle {{\n\
    fn value(self) -> i64 {{ 7 }}\n\
    fn sink(consume self) {{ self.close(); }}\n\
    fn close(consume self) {{ unsafe {{ hew_deque_free(self.raw) }}; print(\"C\"); }}\n\
}}\n\
extern \"C\" {{\n\
    fn hew_deque_new() -> Dq;\n\
    fn hew_deque_free(consume dq: Dq);\n\
}}\n\
enum Outcome {{ Loaded(Handle), Failed(string), }}\n\
fn make(ok: bool) -> Outcome {{\n\
    if ok {{ Outcome.Loaded(Handle {{ raw: unsafe {{ hew_deque_new() }} }}) }}\n\
    else {{ Outcome.Failed(\"bad\".to_upper()) }}\n\
}}\n\
fn main() {{\n\
    for _ in 0..{frames} {{\n\
        match make(true) {{\n\
            Outcome.Loaded(handle) => print(handle.value()),\n\
            Outcome.Failed(message) => print(message + \"!\"),\n\
        }}\n\
        match make(false) {{\n\
            Outcome.Loaded(handle) => print(handle.value()),\n\
            Outcome.Failed(message) => print(message + \"!\"),\n\
        }}\n\
        match make(true) {{\n\
            Outcome.Loaded(handle) => handle.sink(),\n\
            Outcome.Failed(message) => print(message + \"!\"),\n\
        }}\n\
    }}\n\
}}\n"
    )
}

const XML_STRING_TEMP_SOURCE: &str = "\
import std.encoding.xml;\n\
fn parse_result(s: string) -> Result<xml.Node, string> {\n\
    if xml.is_wellformed(s) { .Ok(xml.parse(s).expect(\"well-formed checked\")) } else { .Err(\"not well-formed\") }\n\
}\n\
fn main() {\n\
    match parse_result(\"<a><b>hi</b></a>\") {\n\
        .Ok(node) => { println(node.to_string()); node.close(); }\n\
        .Err(message) => println(message),\n\
    }\n\
}\n";

const XML_PROJECTED_HELPER_CRASH_SOURCE: &str = r#"
import std.encoding.xml;

enum Pair {
    Both(xml.Node, string),
    Nothing,
}

fn consume(consume pair: Pair, trigger: i64) -> i64 {
    match pair {
        Pair.Both(node, text) => {
            node.close();
            if trigger != 0 {
                panic("crash after explicit close");
            }
            text.len()
        },
        Pair.Nothing => 0,
    }
}

actor Helper {
    receive fn ping() -> i64 {
        7
    }
}

actor Crasher {
    let helper: Helper,

    receive fn run(trigger: i64) -> i64 {
        let seed = match helper.ping() {
            .Ok(value) => value,
            .Err(_) => 0,
        };
        let pair = Pair.Both(xml.parse("<x/>").expect("well-formed literal"), "hi");
        consume(pair, trigger) + seed
    }
}

fn main() {
    let helper = spawn Helper;
    let crasher = spawn Crasher(helper: helper);
    match crasher.run(0) {
        .Ok(value) => println(f"ok={value}"),
        .Err(_) => println("bad"),
    }
    match crasher.run(1) {
        .Ok(value) => println(f"unexpected={value}"),
        .Err(_) => println("handled-crash"),
    }
    println("survived");
}
"#;

const BYTES_MUTATING_HELPER_CRASH_SOURCE: &str = r#"
fn clear_then_maybe_crash(trigger: i64) -> i64 {
    var value: bytes = "owned-bytes".to_bytes();
    value.clear();
    if trigger != 0 {
        panic("crash after bytes.clear");
    }
    value.len()
}

actor Gate {
    receive fn tick() -> i64 {
        7
    }
}

actor Runner {
    let gate: Gate,

    receive fn run(trigger: i64) -> i64 {
        let seed = match gate.tick() {
            .Ok(value) => value,
            .Err(_) => 0,
        };
        clear_then_maybe_crash(trigger) + seed
    }
}

fn main() {
    let gate = spawn Gate;
    let runner = spawn Runner(gate: gate);
    match runner.run(0) {
        .Ok(value) => println(f"ok={value}"),
        .Err(_) => println("bad"),
    }
    match runner.run(1) {
        .Ok(value) => println(f"unexpected={value}"),
        .Err(_) => println("handled-crash"),
    }
    println("survived");
}
"#;

const RETURNED_BYTES_LOAN_CRASH_SOURCE: &str = r#"
fn push_then_maybe_crash(var value: bytes, trigger: i64) {
    value.push(0x40 as u8);
    if trigger != 0 {
        panic("crash during returned bytes loan");
    }
}

fn build_packet(trigger: i64) -> bytes {
    let value = bytes.new();
    push_then_maybe_crash(value, trigger);
    value
}

actor Gate {
    receive fn tick() -> i64 {
        7
    }
}

actor Runner {
    let gate: Gate,

    receive fn run(trigger: i64) -> i64 {
        let seed = match gate.tick() {
            .Ok(value) => value,
            .Err(_) => 0,
        };
        let packet = build_packet(trigger);
        packet.len() + seed
    }
}

fn main() {
    let gate = spawn Gate;
    let runner = spawn Runner(gate: gate);
    match runner.run(0) {
        .Ok(value) => println(f"ok={value}"),
        .Err(_) => println("bad"),
    }
    match runner.run(1) {
        .Ok(value) => println(f"unexpected={value}"),
        .Err(_) => println("handled-crash"),
    }
    println("survived");
}
"#;

fn assert_exact_zero_leaks(bin: &Path, shape: &str) {
    require_leaks_tool();
    let (count, bytes) = measure_leaks_exact(bin);
    assert_eq!(
        (count, bytes),
        (0, 0),
        "{shape}: expected `0 leaks for 0 total leaked bytes`, got \
         {count} leak(s) for {bytes} bytes; re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}`",
        bin.display()
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn resource_record_enum_payload_closes_exactly_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("enum-resource-close-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &resource_enum_source(RESOURCE_FRAMES),
        dir.path(),
        "resource_enum",
    );

    // Each frame closes the borrowed handle when its arm ends, before the
    // `Failed` arm prints, and the consumed handle inside `sink`.
    let mut expected = String::new();
    for _ in 0..RESOURCE_FRAMES {
        let _ = write!(expected, "7CBAD!C");
    }

    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "resource enum loop must run clean under the poisoned allocator; a crash \
         indicates a double-close or use-after-free:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected,
        "borrowed and consuming resource payload methods must each close exactly once"
    );
    assert_exact_zero_leaks(&bin, "resource_record_enum_payload");
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn xml_string_return_temporary_is_released() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("enum-resource-xml-string-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(XML_STRING_TEMP_SOURCE, dir.path(), "xml_string_temp");

    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "XML string-return temporary must run clean under the poisoned allocator:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "<a><b>hi</b></a>\n"
    );
    assert_exact_zero_leaks(&bin, "xml_string_return_temporary");
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn projected_resource_close_then_crash_releases_source_snapshot_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("enum-resource-helper-crash-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        XML_PROJECTED_HELPER_CRASH_SOURCE,
        dir.path(),
        "xml_projected_helper_crash",
    );

    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        output.status.code(),
        Some(1),
        "projected resource close followed by a handled actor crash must not \
         double-release the source snapshot:\n{}",
        describe_output(&output)
    );
    assert!(String::from_utf8_lossy(&output.stderr).contains("crash after explicit close"));
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "ok=9\nhandled-crash\nsurvived\n",
        "the normal and crash paths must both complete before main survives"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn bytes_runtime_mutation_then_crash_releases_refreshed_snapshot_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("bytes-helper-mutation-crash-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        BYTES_MUTATING_HELPER_CRASH_SOURCE,
        dir.path(),
        "bytes_mutating_helper_crash",
    );

    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        output.status.code(),
        Some(1),
        "bytes.clear followed by a handled actor crash must not release the \
         helper's pre-mutation snapshot:\n{}",
        describe_output(&output)
    );
    assert!(String::from_utf8_lossy(&output.stderr).contains("crash after bytes.clear"));
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "ok=7\nhandled-crash\nsurvived\n",
        "the normal and crash paths must both complete before main survives"
    );
    #[cfg(target_os = "macos")]
    assert_exact_zero_leaks(&bin, "bytes_mutating_helper_crash");
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn returned_bytes_loan_releases_once_on_success_and_crash() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("returned-bytes-loan-crash-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        RETURNED_BYTES_LOAN_CRASH_SOURCE,
        dir.path(),
        "returned_bytes_loan_crash",
    );

    let output = run_under_malloc_scribble(&bin);
    assert_eq!(
        output.status.code(),
        Some(1),
        "returned bytes must survive its successful ReturnSlot handoff and the \
         crashing loan must release its buffer exactly once:\n{}",
        describe_output(&output)
    );
    assert!(String::from_utf8_lossy(&output.stderr).contains("crash during returned bytes loan"));
    // `push_then_maybe_crash` takes `var value: bytes`, which is the callee's
    // own copy, so the packet the caller returns is still empty and the sum is
    // the gate's seed alone.
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "ok=7\nhandled-crash\nsurvived\n",
        "the normal return and recovered crash must both complete"
    );
    #[cfg(target_os = "macos")]
    assert_exact_zero_leaks(&bin, "returned_bytes_loan_crash");
}
