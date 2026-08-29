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

use std::path::Path;
use std::process::Command;

use support::leak_slope::compile_to_native_with_ir;
use support::{describe_output, hew_binary, repo_root, require_codegen};

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

fn dump_mir(source: &str, stage: &str, dir: &Path) -> String {
    let path = dir.join("identity_display_drop_shape.hew");
    std::fs::write(&path, source).expect("write identity display source");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--dump-mir",
            stage,
            path.to_str().expect("Hew source path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .unwrap_or_else(|error| panic!("invoke hew compile --dump-mir {stage}: {error}"));
    assert!(
        output.status.success(),
        "{stage} MIR dump failed:\n{}",
        describe_output(&output)
    );
    String::from_utf8(output.stdout).expect("MIR dump is UTF-8")
}

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

fn unique_scope_drop_locals(section: &str) -> Vec<&str> {
    let mut locals = section
        .lines()
        .filter(|line| line.contains("ty=string kind=cow_heap(hew_string_drop)"))
        .filter_map(|line| line.split_whitespace().nth(1))
        .collect::<Vec<_>>();
    locals.sort_unstable();
    locals.dedup();
    locals
}

fn unique_inline_drop_locals(section: &str) -> Vec<&str> {
    let mut locals = section
        .lines()
        .filter(|line| line.contains("ty=string fn=release(hew_string_drop)"))
        .filter_map(|line| line.split_whitespace().nth(1))
        .collect::<Vec<_>>();
    locals.sort_unstable();
    locals.dedup();
    locals
}

#[test]
fn direct_named_and_mixed_displays_have_one_mir_and_llvm_drop_per_result() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("identity-display-structural-")
        .tempdir()
        .expect("tempdir");

    let raw = dump_mir(STRUCTURAL_SOURCE, "raw", dir.path());
    let elaborated = dump_mir(STRUCTURAL_SOURCE, "elab", dir.path());
    for (name, expected_inline, expected_scope) in [
        ("direct_displays", 3, 0),
        ("named_displays", 0, 3),
        ("mixed_displays", 1, 2),
    ] {
        let raw_section = function_section(&raw, &format!("fn {name}"));
        for callee in [
            "hew_node_id_display",
            "hew_location_display",
            "hew_remote_pid_display",
        ] {
            assert_eq!(
                raw_section.match_indices(callee).count(),
                1,
                "{name} must contain exactly one {callee} call:\n{raw_section}"
            );
        }
        let inline = unique_inline_drop_locals(raw_section);
        let elab_section = function_section(&elaborated, &format!("fn {name}"));
        let scope = unique_scope_drop_locals(elab_section);
        assert_eq!(
            inline.len(),
            expected_inline,
            "{name} must carry exactly {expected_inline} inline temporary-drop authorities:\n\
             {raw_section}"
        );
        assert_eq!(
            scope.len(),
            expected_scope,
            "{name} must carry exactly {expected_scope} named scope-drop authorities:\n\
             {elab_section}"
        );
        assert_eq!(
            inline.len() + scope.len(),
            3,
            "{name} must carry one non-competing MIR drop authority per display result"
        );
    }

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
