//! Stdlib invariant tests for `hew doc`.
//!
//! Runs the doc generator over the committed `std/` tree and asserts
//! a small set of invariants that encode the correctness guarantees
//! called out in hew-lang/hew#1278. These are invariant checks, not
//! snapshot comparisons — they should survive unrelated stdlib edits.

use std::path::PathBuf;
use std::process::Command;
use std::sync::OnceLock;

mod support;
use support::{describe_output, hew_binary, repo_root};

/// Generate the stdlib docs once per test process and hand back the
/// output directory. The `TempDir` handle is held in a `OnceLock` so
/// all three tests share a single generation pass and cleanup runs
/// when the process exits.
fn stdlib_docs() -> &'static PathBuf {
    static CELL: OnceLock<(tempfile::TempDir, PathBuf)> = OnceLock::new();
    &CELL
        .get_or_init(|| {
            let dir = support::tempdir();
            let out_dir = dir.path().to_path_buf();

            let std_dir = repo_root().join("std");
            let output = Command::new(hew_binary())
                .arg("doc")
                .arg(std_dir)
                .arg("--output-dir")
                .arg(&out_dir)
                .output()
                .expect("spawn hew doc");

            assert!(
                output.status.success(),
                "hew doc failed:\n{}",
                describe_output(&output),
            );
            (dir, out_dir)
        })
        .1
}

fn read_module(name: &str) -> String {
    let path = stdlib_docs().join(name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()))
}

#[test]
fn private_items_do_not_leak_into_stdlib_docs() {
    let datetime = read_module("std.time.datetime.html");
    assert!(
        !datetime.contains("parse_error_message"),
        "private helper leaked into std::time::datetime HTML",
    );
}

#[test]
fn enum_variants_render_in_stdlib_docs() {
    let json = read_module("std.encoding.json.html");
    assert!(
        json.contains("Invalid"),
        "ParseError::Invalid variant missing from std::encoding::json HTML",
    );
    assert!(
        json.contains("Variants"),
        "Variants section missing from std::encoding::json HTML",
    );
}

#[test]
fn trait_method_docstrings_render_in_stdlib_docs() {
    let json = read_module("std.encoding.json.html");
    // `ValueMethods::stringify` keeps the JSON-facing docstring while extending
    // the shared canonical Value contract. Verify the docstring reaches the
    // rendered HTML.
    assert!(
        json.contains("Serialize the value back to a JSON string."),
        "trait method docstring missing from std::encoding::json HTML",
    );
}
