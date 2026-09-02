//! End-to-end coverage for `hew tool playground-verify` against real spawned
//! `hew` processes.
//!
//! These exercise the fix for the bug where a curated playground source file
//! that defines its own `fn main` was rejected with "`main` is defined
//! multiple times": `hew tool playground-verify` runs each manifest entry
//! through the REPL-chunked eval path (`hew-cli/src/eval/session.rs`), which
//! used to append a synthetic `fn main() {}` after every top-level item chunk
//! unconditionally — including a chunk that was itself the file's real
//! `fn main`. A companion case also covers the `#[wire]`-attribute chunking
//! fix in `hew-cli/src/eval/classify.rs`.
//!
//! Run as process-level integration tests (not in-process unit tests) because
//! `hew tool playground-verify` compiles and links a real native binary per
//! entry; the `CARGO_BIN_EXE_hew` binary sits at the on-disk depth the
//! linker's `libhew.a` search assumes, while a `cargo test`/`nextest` unit
//! test binary (built one directory deeper, under `target/debug/deps/`) does
//! not.

mod support;

use std::fs;

use support::{hew_command, repo_root, require_codegen, run_bounded_command, tempdir};

/// Write a manifest.json plus its listed source/.expected files into `dir`,
/// return the manifest path.
fn write_manifest(
    dir: &std::path::Path,
    entries: &[(&str, &str, &str, &str)],
) -> std::path::PathBuf {
    // entries: (id, source_filename, source_contents, expected_contents)
    let mut manifest_entries = Vec::new();
    for (id, filename, source, expected) in entries {
        fs::write(dir.join(filename), source).expect("write source fixture");
        let expected_filename = format!("{filename}.expected");
        fs::write(dir.join(&expected_filename), expected).expect("write expected fixture");
        manifest_entries.push(format!(
            r#"{{"id": "{id}", "source_path": "{filename}", "expected_path": "{expected_filename}", "capabilities": {{"wasi": "runnable"}}}}"#
        ));
    }
    let manifest_path = dir.join("manifest.json");
    fs::write(&manifest_path, format!("[{}]", manifest_entries.join(",")))
        .expect("write manifest.json");
    manifest_path
}

fn run_playground_verify(manifest_path: &std::path::Path) -> std::process::Output {
    let mut command = hew_command();
    command
        .arg("tool")
        .arg("playground-verify")
        .arg("--manifest")
        .arg(manifest_path)
        .current_dir(repo_root());
    run_bounded_command(command, "hew tool playground-verify")
}

/// A source file that defines its own `fn main` (the shape of every curated
/// playground example) must verify successfully.
#[test]
fn source_with_own_main_verifies() {
    require_codegen();
    let dir = tempdir();
    let manifest = write_manifest(
        dir.path(),
        &[(
            "own_main",
            "own_main.hew",
            "fn main() {\n    println(\"hi\");\n}\n",
            "hi\n",
        )],
    );

    let output = run_playground_verify(&manifest);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "expected success for a source file defining its own main\nstdout:\n{stdout}\nstderr:\n{stderr}"
    );
    assert!(
        stdout.contains("PASS own_main"),
        "expected a PASS line\nstdout:\n{stdout}"
    );
}

/// A source file with a helper function and a `#[wire]`-attributed type
/// declared before its own `fn main` — the multi-chunk REPL-eval shape —
/// must also verify. Covers both fixes together: the duplicate-main
/// synthesis and the bare-attribute-line chunking.
#[test]
fn source_with_helper_wire_type_and_own_main_verifies() {
    require_codegen();
    let dir = tempdir();
    let manifest = write_manifest(
        dir.path(),
        &[(
            "helper_wire_main",
            "helper_wire_main.hew",
            "fn fib(n: i64) -> i64 {\n    if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }\n}\n\n#[wire]\ntype Msg {\n    name: string @1,\n}\n\nfn main() {\n    println(fib(6));\n}\n",
            "8\n",
        )],
    );

    let output = run_playground_verify(&manifest);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "expected success for a helper + wire type + own main\nstdout:\n{stdout}\nstderr:\n{stderr}"
    );
    assert!(
        stdout.contains("PASS helper_wire_main"),
        "expected a PASS line\nstdout:\n{stdout}"
    );
}

/// Negative control: a source file with a genuine duplicate `fn main` (two
/// literal definitions, not one synthesised by the evaluator) must still be
/// rejected. Proves the fix only suppresses the *synthetic* main, not real
/// duplicate-main detection.
#[test]
fn source_with_genuine_duplicate_main_fails() {
    require_codegen();
    let dir = tempdir();
    let manifest = write_manifest(
        dir.path(),
        &[(
            "duplicate_main",
            "duplicate_main.hew",
            "fn main() {\n    println(\"first\");\n}\n\nfn main() {\n    println(\"second\");\n}\n",
            "first\n",
        )],
    );

    let output = run_playground_verify(&manifest);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !output.status.success(),
        "expected failure for a genuinely duplicated main\nstdout:\n{stdout}\nstderr:\n{stderr}"
    );
    // `print_results` writes PASS/SKIP to stdout but FAIL to stderr.
    assert!(
        stderr.contains("FAIL duplicate_main"),
        "expected a FAIL line\nstderr:\n{stderr}"
    );
    assert!(
        stderr.contains("defined multiple times"),
        "expected the real duplicate-main diagnostic\nstderr:\n{stderr}"
    );
}
