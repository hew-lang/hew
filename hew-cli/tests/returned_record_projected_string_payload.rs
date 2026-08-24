//! Exact ownership oracle for a nested enum payload stored in a returned record.
//!
//! A nested-pattern binder is an alias of storage still owned by the matched
//! call-result carrier. Returning that alias inside a record must mint one
//! independent string owner before the carrier is dropped. The control puts an
//! explicit `clone()` at that one boundary and therefore needs no implicit
//! retain.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::compile_to_native;
use support::{describe_output, hew_binary, repo_root, require_codegen};

const SOURCE_TEMPLATE: &str = r#"
enum CleanupError {
    Dirty(string);
}

type Retirement {
    blocked: bool;
    detail: string;
}

type Outcome {
    status: string;
}

fn persist(value: string) {
    if value.len() == 0 {
        panic("empty");
    }
}

fn cleanup() -> Result<(), CleanupError> {
    Err(CleanupError.Dirty("dirty worktree " + f"{42}"))
}

fn retire() -> Result<Retirement, string> {
    match cleanup() {
        .Ok(_) => Ok(Retirement { blocked: false, detail: "" }),
        .Err(CleanupError.Dirty(message)) => {
            persist(message.clone());
            Ok(Retirement { blocked: true, detail: __DETAIL__ })
        },
    }
}

fn lifecycle() -> Result<Outcome, string> {
    let retirement = match retire() {
        .Err(message) => return Err(message),
        .Ok(value) => value,
    };
    Ok(Outcome {
        status: if retirement.blocked { "blocked" } else { "merged" },
    })
}

fn main() {
    for _ in 0..64 {
        match lifecycle() {
            .Err(message) => panic(message),
            .Ok(outcome) => {
                if outcome.status != "blocked" {
                    panic("wrong outcome");
                }
            },
        }
    }
    println("ok");
}
"#;

fn source(detail: &str) -> String {
    SOURCE_TEMPLATE.replace("__DETAIL__", detail)
}

fn dump_raw_mir(source: &str, name: &str) -> String {
    let dir = tempfile::Builder::new()
        .prefix("returned-record-projected-string-mir-")
        .tempdir()
        .expect("tempdir");
    let path = dir.path().join(format!("{name}.hew"));
    std::fs::write(&path, source).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--dump-mir",
            "raw",
            path.to_str().expect("Hew source path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile --dump-mir raw");
    assert!(
        output.status.success(),
        "raw MIR dump failed for {name}:\n{}",
        describe_output(&output)
    );
    String::from_utf8(output.stdout).expect("MIR dump is UTF-8")
}

fn function_section<'a>(dump: &'a str, name: &str) -> &'a str {
    let marker = format!("fn {name}");
    let start = dump
        .find(&marker)
        .unwrap_or_else(|| panic!("missing `{marker}` in MIR dump:\n{dump}"));
    let tail = &dump[start..];
    tail.find("\nfn ").map_or(tail, |next| &tail[..next])
}

fn compile_and_run(source: &str, name: &str) {
    let dir = tempfile::Builder::new()
        .prefix("returned-record-projected-string-run-")
        .tempdir()
        .expect("tempdir");
    let binary = compile_to_native(source, dir.path(), name);
    let output = Command::new(&binary)
        .output()
        .unwrap_or_else(|error| panic!("run {}: {error}", binary.display()));
    assert!(
        output.status.success(),
        "{name} must release the carrier and returned record exactly once:\n{}",
        describe_output(&output)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "ok\n");
}

#[test]
fn nested_payload_return_mints_only_the_missing_owner() {
    require_codegen();

    let implicit = source("message");
    let explicit_control = source("message.clone()");

    let implicit_raw = dump_raw_mir(&implicit, "implicit_retain");
    let implicit_retire = function_section(&implicit_raw, "retire");
    assert_eq!(
        implicit_retire.match_indices("string.retain").count(),
        1,
        "the borrowed nested payload needs exactly one owner for the returned record:\n\
         {implicit_retire}"
    );

    let control_raw = dump_raw_mir(&explicit_control, "explicit_clone");
    let control_retire = function_section(&control_raw, "retire");
    assert_eq!(
        control_retire.match_indices("string.retain").count(),
        0,
        "the explicit one-boundary clone already owns the returned field:\n{control_retire}"
    );
}

#[test]
fn nested_payload_return_and_explicit_clone_control_release_exactly_once() {
    require_codegen();
    compile_and_run(&source("message"), "implicit_retain");
    compile_and_run(&source("message.clone()"), "explicit_clone");
}
