//! Stdlib invariant tests for `hew doc`.
//!
//! Runs the doc generator over the committed `std/` tree and asserts
//! a small set of invariants that encode the correctness guarantees
//! called out in hew-lang/hew#1278. These are invariant checks, not
//! snapshot comparisons — they should survive unrelated stdlib edits.

use std::process::Command;

mod support;
use support::{describe_output, hew_binary, repo_root};

fn read_module(docs_dir: &std::path::Path, name: &str) -> String {
    let path = docs_dir.join(name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()))
}

/// Generates the stdlib docs once and asserts every invariant against that
/// one pass. A single test-owned `TempDir` (rather than a `OnceLock<TempDir>`
/// static shared across three `#[test]` fns) means its `Drop` — and the
/// `fs::remove_dir_all` cleanup it runs — actually fires at scope exit:
/// `static` values are never dropped, so the previous three-test split leaked
/// its generated-docs directory on every run (#3129).
#[test]
fn stdlib_docs_invariants() {
    let dir = support::tempdir();
    let out_dir = dir.path();

    let std_dir = repo_root().join("std");
    let output = Command::new(hew_binary())
        .arg("doc")
        .arg(std_dir)
        .arg("--output-dir")
        .arg(out_dir)
        .output()
        .expect("spawn hew doc");
    assert!(
        output.status.success(),
        "hew doc failed:\n{}",
        describe_output(&output),
    );

    // Private items must not leak into stdlib docs.
    let datetime = read_module(out_dir, "std.time.datetime.html");
    assert!(
        !datetime.contains("parse_error_message"),
        "private helper leaked into std::time::datetime HTML",
    );

    let json = read_module(out_dir, "std.encoding.json.html");

    // Enum variants render in stdlib docs.
    assert!(
        json.contains("Invalid"),
        "ParseError::Invalid variant missing from std::encoding::json HTML",
    );
    assert!(
        json.contains("Variants"),
        "Variants section missing from std::encoding::json HTML",
    );

    // Trait method docstrings render in stdlib docs. `ValueMethods::stringify`
    // keeps the JSON-facing docstring while extending the shared canonical
    // Value contract; verify the docstring reaches the rendered HTML.
    assert!(
        json.contains("Serialize the value back to a JSON string."),
        "trait method docstring missing from std::encoding::json HTML",
    );
}
