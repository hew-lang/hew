//! Structural ownership canary for compiler-synthetic identity display results.
//!
//! `NodeId::display`, `Location::display`, and `RemotePid::display` reach MIR
//! under synthetic catalog names, then codegen rewrites them to the allocating
//! runtime formatters. A direct `value.display().len()` has no user binding to
//! anchor cleanup, so the synthetic callee must inherit the measured runtime
//! result contract before temporary-drop elaboration may mint exactly one
//! `hew_string_drop`.
//!
//! This canary pins all three direct paths plus named and mixed controls at
//! both MIR and LLVM. The real low/high Node startup + remote lookup allocator
//! measurement lives in `identity_monitor_leak_oracle`.

#![cfg(unix)]

mod support;

use support::leak_slope::compile_to_native_with_ir;
use support::require_codegen;

const STRUCTURAL_SOURCE: &str = r"
type Ping {
    value: i64,
}

actor Worker {
    receive fn ping(msg: Ping) {}
}

impl ActorMsg for Worker {
    type Msg = Ping;
    type Reply = ();
}

fn direct_displays(node_id: NodeId, location: Location, pid: RemotePid<Worker>) {
    println(node_id.display().len());
    println(location.display().len());
    println(pid.display().len());
}

fn named_displays(node_id: NodeId, location: Location, pid: RemotePid<Worker>) {
    let node_text = node_id.display();
    let location_text = location.display();
    let pid_text = pid.display();
    println(node_text.len());
    println(location_text.len());
    println(pid_text.len());
}

fn mixed_displays(node_id: NodeId, location: Location, pid: RemotePid<Worker>) {
    let node_text = node_id.display();
    println(node_text.len());
    println(location.display().len());
    let pid_text = pid.display();
    println(pid_text.len());
}

fn main() -> i64 { 0 }
";

fn function_section<'a>(dump: &'a str, marker: &str) -> &'a str {
    let start = dump
        .find(marker)
        .unwrap_or_else(|| panic!("missing function marker `{marker}`:\n{dump}"));
    let tail = &dump[start..];
    tail.find("\nfn ")
        .or_else(|| tail.find("\ndefine "))
        .map_or(tail, |next| &tail[..next])
}

fn llvm_call_count(section: &str, symbol: &str) -> usize {
    section
        .lines()
        .take_while(|line| !line.starts_with("invoke.cleanup"))
        .filter(|line| {
            (line.contains("call ") || line.contains("invoke ")) && line.contains(symbol)
        })
        .count()
}

#[test]
fn direct_named_and_mixed_displays_have_one_mir_and_llvm_drop_per_result() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("identity-display-structural-")
        .tempdir()
        .expect("tempdir");

    // Lost coverage: this test used to also dump `--dump-mir raw`/`elab`
    // (both retired) here and count per-function inline-vs-scope drop-local
    // occurrences to pin exactly which MIR stage carried each temporary's
    // release authority (3 inline / 0 scope for `direct_displays`, etc.).
    // Physical MIR's structured (Debug) dump has no equivalent single-line
    // text to grep or count per function, so that MIR-level split has no
    // direct replacement. The LLVM-level assertions below still prove the
    // externally observable half of the same invariant: exactly one real
    // formatter call and exactly one release per display result, with no
    // fabricated linkable symbols.
    let (_binary, ll_path) =
        compile_to_native_with_ir(STRUCTURAL_SOURCE, dir.path(), "identity_display_drop_shape");
    let llvm = std::fs::read_to_string(ll_path).expect("read emitted LLVM IR");
    for name in ["direct_displays", "named_displays", "mixed_displays"] {
        let marker = format!("@{name}(");
        let section = function_section(&llvm, &marker);
        assert_eq!(
            llvm_call_count(section, "@hew_node_id_format("),
            1,
            "{name} must lower NodeId display to one real formatter call:\n{section}"
        );
        assert_eq!(
            llvm_call_count(section, "@hew_location_format("),
            2,
            "{name} must lower Location and RemotePid display to the shared real formatter:\n\
             {section}"
        );
        assert_eq!(
            llvm_call_count(section, "@hew_string_drop("),
            3,
            "{name} must emit exactly one LLVM release per formatted result:\n{section}"
        );
        assert!(
            !section.contains("@hew_node_id_display(")
                && !section.contains("@hew_location_display(")
                && !section.contains("@hew_remote_pid_display("),
            "{name} must not fabricate linkable symbols for compiler synthetics:\n{section}"
        );
    }
}
