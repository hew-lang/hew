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
type Handle {{ raw: Dq; }}\n\
impl Handle {{\n\
    fn value(self) -> i64 {{ 7 }}\n\
    fn close(self) {{ unsafe {{ hew_deque_free(self.raw) }}; print(\"C\"); }}\n\
}}\n\
extern \"C\" {{\n\
    fn hew_deque_new() -> Dq;\n\
    fn hew_deque_free(consume dq: Dq);\n\
}}\n\
enum Outcome {{ Loaded(Handle); Failed(string); }}\n\
fn make(ok: bool) -> Outcome {{\n\
    if ok {{ Outcome::Loaded(Handle {{ raw: unsafe {{ hew_deque_new() }} }}) }}\n\
    else {{ Outcome::Failed(\"bad\".to_upper()) }}\n\
}}\n\
fn main() {{\n\
    for _ in 0..{frames} {{\n\
        match make(true) {{\n\
            Outcome::Loaded(handle) => print(handle.value()),\n\
            Outcome::Failed(message) => print(message + \"!\"),\n\
        }}\n\
        match make(false) {{\n\
            Outcome::Loaded(handle) => print(handle.value()),\n\
            Outcome::Failed(message) => print(message + \"!\"),\n\
        }}\n\
    }}\n\
}}\n"
    )
}

const XML_STRING_TEMP_SOURCE: &str = "\
import std::encoding::xml;\n\
fn parse_result(s: string) -> Result<xml.Node, string> {\n\
    if xml.is_wellformed(s) { Ok(xml.parse(s)) } else { Err(\"not well-formed\") }\n\
}\n\
fn main() {\n\
    match parse_result(\"<a><b>hi</b></a>\") {\n\
        Ok(node) => { println(node.to_string()); node.close(); }\n\
        Err(message) => println(message),\n\
    }\n\
}\n";

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

    let mut expected = String::new();
    for _ in 0..RESOURCE_FRAMES {
        let _ = write!(expected, "7BAD!C");
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
        "each resource payload must be read, then closed exactly once"
    );
    assert_exact_zero_leaks(&bin, "resource_record_enum_payload");

    let ll = std::fs::read_to_string(dir.path().join("resource_enum.ll")).expect("read LLVM IR");
    let enum_drop = function_body(&ll, "@__hew_enum_drop_inplace_Outcome(")
        .expect("enum drop helper must be defined");
    assert!(
        enum_drop.contains("@__hew_record_drop_inplace_Handle"),
        "enum drop must recurse through the resource record:\n{enum_drop}"
    );
    let record_drop = function_body(&ll, "@__hew_record_drop_inplace_Handle(")
        .expect("resource record drop helper must be defined");
    assert_eq!(
        record_drop.matches("@\"Handle::close\"").count(),
        1,
        "resource record drop must call close exactly once:\n{record_drop}"
    );
    let record_clone = function_body(&ll, "@__hew_record_clone_inplace_Handle(")
        .expect("resource record clone helper must be defined");
    assert!(
        record_clone.contains("step_0_clone:")
            && record_clone.contains("br label %rb_step_0")
            && record_clone.contains("step_0_store:")
            && record_clone.contains("No predecessors!")
            && !record_clone.contains(" call "),
        "the opaque field clone step must unconditionally branch to rollback, \
         leaving its success store unreachable:\n{record_clone}"
    );
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

fn function_body(ll: &str, needle: &str) -> Option<String> {
    let mut in_function = false;
    let mut body = String::new();
    for line in ll.lines() {
        if !in_function {
            if line.starts_with("define") && line.contains(needle) {
                in_function = true;
                body.push_str(line);
                body.push('\n');
            }
            continue;
        }
        body.push_str(line);
        body.push('\n');
        if line == "}" {
            return Some(body);
        }
    }
    None
}
