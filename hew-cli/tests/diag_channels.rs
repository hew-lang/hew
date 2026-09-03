//! End-to-end coverage for the three diagnostic channels (V060-DIAG-1):
//! exit code, text prefix, and the `--format json` `channel` field.
//!
//! User and Limitation are exercised here from the CLI. No `.hew` program on
//! `main` reaches the Internal channel through the CLI (it requires a
//! compiler-invariant failure, not a legal program), so Internal is asserted
//! at unit level instead: see `hew-mir/src/model.rs`'s
//! `mir_diagnostic_channel_*` tests for `channel()` classification, and
//! `hew-cli/src/diagnostic.rs`'s `internal_channel_exit_code_and_prefix` for
//! the exit code and rendered bug-report note.

mod support;

use std::fs;
use std::process::Command;

use serde_json::Value;
use support::{describe_output, hew_binary, strip_ansi};

fn write_fixture(source: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = support::tempdir();
    let path = dir.path().join("main.hew");
    fs::write(&path, source).expect("write fixture");
    (dir, path)
}

fn run(args: &[&str]) -> std::process::Output {
    Command::new(hew_binary())
        .args(args)
        .output()
        .expect("hew binary must run")
}

fn parse_json_array(output: &std::process::Output) -> Vec<Value> {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let value: Value = serde_json::from_str(&stdout).unwrap_or_else(|error| {
        panic!(
            "stdout must be a parseable JSON array; parse error: {error}\nstdout:\n{stdout}\n{}",
            describe_output(output)
        )
    });
    value
        .as_array()
        .unwrap_or_else(|| panic!("top-level JSON must be an array; got:\n{stdout}"))
        .clone()
}

/// A plain type error: the User channel. Exit 1, no channel prefix on the
/// rendered text (today's rendering), and `"channel": "user"` in JSON.
const USER_FIXTURE: &str = "fn main() {\n    let x: i64 = \"oops\";\n}\n";

/// D9: a `fork` inside `scope { .. }` reached from `main`, which carries no
/// execution context in this release. The Limitation channel's first code,
/// `E_LIMIT_MAIN_CONTEXT` (ledger D340).
const LIMITATION_FIXTURE: &str = "fn main() {\n    scope { fork { println(\"x\"); } }\n}\n";

/// D26 as amended by D340: `<` on a record with no user `impl Ord` — the
/// checker's own first Limitation-channel kind (every prior `TypeError` was
/// User-channel; HIR/MIR diagnostics carried the only prior Limitation
/// codes).
const CHECKER_LIMITATION_FIXTURE: &str = "type Pt {\n    x: i64;\n    y: i64;\n}\n\nfn main() {\n    let a = Pt { x: 1, y: 2 };\n    let b = Pt { x: 1, y: 3 };\n    println(a < b);\n}\n";

#[test]
fn user_channel_exits_1_with_no_prefix_and_json_channel_user() {
    let (_dir, path) = write_fixture(USER_FIXTURE);

    let text_output = run(&["check", path.to_str().unwrap()]);
    assert_eq!(
        text_output.status.code(),
        Some(1),
        "a User-channel diagnostic must exit 1\n{}",
        describe_output(&text_output),
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&text_output.stderr));
    assert!(
        !stderr.contains("compiler limitation:") && !stderr.contains("internal compiler error:"),
        "a User-channel diagnostic must carry no channel prefix; got:\n{stderr}",
    );

    let json_output = run(&["check", "--format=json", path.to_str().unwrap()]);
    assert_eq!(json_output.status.code(), Some(1));
    let diagnostics = parse_json_array(&json_output);
    let mismatch = diagnostics
        .iter()
        .find(|d| d["code"] == "Mismatch")
        .expect("expected a Mismatch diagnostic");
    assert_eq!(
        mismatch["channel"], "user",
        "a type error's JSON channel field must be \"user\": {mismatch}",
    );
}

#[test]
fn limitation_channel_exits_3_with_prefix_and_json_channel_limitation() {
    let (_dir, path) = write_fixture(LIMITATION_FIXTURE);

    let text_output = run(&["check", path.to_str().unwrap()]);
    assert_eq!(
        text_output.status.code(),
        Some(3),
        "a Limitation-channel diagnostic must exit 3\n{}",
        describe_output(&text_output),
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&text_output.stderr));
    assert!(
        stderr.contains("compiler limitation:"),
        "must render the Limitation channel prefix; got:\n{stderr}",
    );
    assert!(
        stderr.contains("E_LIMIT_MAIN_CONTEXT"),
        "must name the D9 code; got:\n{stderr}",
    );
    assert!(
        stderr.contains("actor") && stderr.contains("join"),
        "the day-one journey asserts the words `actor` and `join`; got:\n{stderr}",
    );

    let json_output = run(&["check", "--format=json", path.to_str().unwrap()]);
    assert_eq!(json_output.status.code(), Some(3));
    let diagnostics = parse_json_array(&json_output);
    let main_context = diagnostics
        .iter()
        .find(|d| d["code"] == "MainContextRequired")
        .expect("expected a MainContextRequired diagnostic");
    assert_eq!(
        main_context["channel"], "limitation",
        "D9's JSON channel field must be \"limitation\": {main_context}",
    );
}

/// D26/D340: the checker's own `TypeErrorKind::DerivedOrdUnavailable`
/// carries the same exit-3 / prefix / JSON-channel contract as the
/// HIR/MIR-authored Limitation diagnostics above, proving `TypeErrorKind::
/// channel()` is actually wired into the CLI rather than only unit-tested.
#[test]
fn checker_limitation_channel_exits_3_with_prefix_and_json_channel_limitation() {
    let (_dir, path) = write_fixture(CHECKER_LIMITATION_FIXTURE);

    let text_output = run(&["check", path.to_str().unwrap()]);
    assert_eq!(
        text_output.status.code(),
        Some(3),
        "a checker Limitation-channel diagnostic must exit 3\n{}",
        describe_output(&text_output),
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&text_output.stderr));
    assert!(
        stderr.contains("compiler limitation:"),
        "must render the Limitation channel prefix; got:\n{stderr}",
    );
    assert!(
        stderr.contains("E_LIMIT_DERIVED_ORD"),
        "must name the D26/D340 code; got:\n{stderr}",
    );

    let json_output = run(&["check", "--format=json", path.to_str().unwrap()]);
    assert_eq!(json_output.status.code(), Some(3));
    let diagnostics = parse_json_array(&json_output);
    let derived_ord = diagnostics
        .iter()
        .find(|d| d["code"] == "E_LIMIT_DERIVED_ORD")
        .expect("expected an E_LIMIT_DERIVED_ORD diagnostic");
    assert_eq!(
        derived_ord["channel"], "limitation",
        "the checker's JSON channel field must be \"limitation\": {derived_ord}",
    );
}
