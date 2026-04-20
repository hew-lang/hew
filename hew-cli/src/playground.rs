//! `hew playground` — curated playground example management tools.
//!
//! # Subcommands
//!
//! * `hew playground verify [--manifest PATH] [--timeout DURATION]`
//!
//!   Iterates the curated manifest, compiles and runs every example whose
//!   `capabilities.wasi` is `"runnable"`, and checks its stdout against the
//!   checked-in `.expected` file.  Examples marked `"unsupported"` are
//!   reported as skipped; no other capability values are treated as runnable.
//!
//! # Invariants preserved
//!
//! * `capabilities.browser` is analysis-only metadata and is never examined
//!   here — the browser path has no local runtime counterpart.
//! * The `"unsupported"` distinction is explicit: skipped entries are counted
//!   separately from passing ones so regressions are visible.
//! * This command does NOT start a server, JIT, or HTTP endpoint.

use std::path::{Path, PathBuf};
use std::time::Duration;

use serde::Deserialize;

// ---------------------------------------------------------------------------
// Manifest schema
// ---------------------------------------------------------------------------

/// A single entry in the playground manifest JSON.
#[derive(Debug, Deserialize)]
pub struct ManifestEntry {
    pub id: String,
    pub source_path: String,
    pub expected_path: String,
    pub capabilities: Capabilities,
}

/// Per-entry runtime capability flags.
#[derive(Debug, Deserialize)]
pub struct Capabilities {
    /// `"runnable"` | `"unsupported"` — local WASI / native execution.
    pub wasi: String,
    // `browser` is intentionally not stored — it is analysis-only metadata
    // and has no bearing on local verification.
}

// ---------------------------------------------------------------------------
// Verification result types
// ---------------------------------------------------------------------------

#[derive(Debug)]
enum VerifyOutcome {
    Pass,
    Fail { actual: String, expected: String },
    RunError(String),
    IoError(String),
}

struct EntryResult {
    id: String,
    outcome: EntryOutcome,
}

enum EntryOutcome {
    Skipped,
    Verified(VerifyOutcome),
}

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Run the `hew playground verify` subcommand.
pub fn cmd_playground(args: &crate::args::PlaygroundCommand) {
    match &args.command {
        crate::args::PlaygroundSubcommand::Verify(verify_args) => cmd_verify(verify_args),
    }
}

fn cmd_verify(args: &crate::args::PlaygroundVerifyArgs) {
    let timeout = crate::util::parse_timeout(&args.timeout).unwrap_or_else(|e| {
        eprintln!("Error: {e}");
        std::process::exit(1);
    });

    // Resolve manifest path.
    let manifest_path = args
        .manifest
        .clone()
        .unwrap_or_else(|| PathBuf::from("examples/playground/manifest.json"));

    let manifest_dir = manifest_path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();

    // Parse manifest.
    let manifest_src = std::fs::read_to_string(&manifest_path).unwrap_or_else(|e| {
        eprintln!(
            "Error: cannot read manifest '{}': {e}",
            manifest_path.display()
        );
        std::process::exit(1);
    });
    let entries: Vec<ManifestEntry> = serde_json::from_str(&manifest_src).unwrap_or_else(|e| {
        eprintln!(
            "Error: cannot parse manifest '{}': {e}",
            manifest_path.display()
        );
        std::process::exit(1);
    });

    if entries.is_empty() {
        eprintln!("Warning: manifest contains no entries.");
        return;
    }

    // Run verification.
    let results = run_verify(&entries, &manifest_dir, timeout);
    print_results(&results);

    let failed = results
        .iter()
        .filter(|r| {
            matches!(
                &r.outcome,
                EntryOutcome::Verified(
                    VerifyOutcome::Fail { .. }
                        | VerifyOutcome::RunError(_)
                        | VerifyOutcome::IoError(_)
                )
            )
        })
        .count();

    if failed > 0 {
        std::process::exit(1);
    }
}

// ---------------------------------------------------------------------------
// Core verification logic
// ---------------------------------------------------------------------------

fn run_verify(
    entries: &[ManifestEntry],
    manifest_dir: &Path,
    timeout: Duration,
) -> Vec<EntryResult> {
    entries
        .iter()
        .map(|entry| {
            let outcome = verify_entry(entry, manifest_dir, timeout);
            EntryResult {
                id: entry.id.clone(),
                outcome,
            }
        })
        .collect()
}

/// Format the error message for a program that exited with a non-zero status.
///
/// The format matches how `exit_eval_error` (via `emit_runtime_failure_output`)
/// surfaces both stdout and stderr — both streams are included in the message
/// when non-empty.
fn format_runtime_failure_message(exit_code: i32, stdout: &str, stderr: &str) -> String {
    format!(
        "program exited with status {exit_code}{}{}",
        if stdout.is_empty() {
            String::new()
        } else {
            format!("\nstdout:\n{stdout}")
        },
        if stderr.is_empty() {
            String::new()
        } else {
            format!("\nstderr:\n{stderr}")
        },
    )
}

fn verify_entry(entry: &ManifestEntry, manifest_dir: &Path, timeout: Duration) -> EntryOutcome {
    if entry.capabilities.wasi != "runnable" {
        return EntryOutcome::Skipped;
    }

    let source_path = manifest_dir.join(&entry.source_path);
    let expected_path = manifest_dir.join(&entry.expected_path);

    // Read expected output first — a missing .expected file is an IO error,
    // not a test failure, so it counts as a configuration problem.
    let expected = match std::fs::read_to_string(&expected_path) {
        Ok(s) => s,
        Err(e) => {
            return EntryOutcome::Verified(VerifyOutcome::IoError(format!(
                "cannot read expected file '{}': {e}",
                expected_path.display()
            )));
        }
    };

    let source_path_str = source_path.display().to_string();

    // Run the source file and capture its stdout.
    let actual = match crate::eval::repl::eval_file_captured(&source_path_str, timeout, None) {
        Ok(out) => out,
        Err(crate::eval::repl::CliEvalError::Message(msg)) => {
            return EntryOutcome::Verified(VerifyOutcome::RunError(msg));
        }
        Err(crate::eval::repl::CliEvalError::DiagnosticsRendered) => {
            return EntryOutcome::Verified(VerifyOutcome::RunError(
                "compilation failed (diagnostics rendered to stderr)".to_string(),
            ));
        }
        Err(crate::eval::repl::CliEvalError::RuntimeFailure {
            stdout,
            stderr,
            exit_code,
        }) => {
            return EntryOutcome::Verified(VerifyOutcome::RunError(
                format_runtime_failure_message(exit_code, &stdout, &stderr),
            ));
        }
    };

    // Normalize trailing whitespace for comparison.
    let actual_trimmed = actual.trim_end_matches('\n');
    let expected_trimmed = expected.trim_end_matches('\n');

    if actual_trimmed == expected_trimmed {
        EntryOutcome::Verified(VerifyOutcome::Pass)
    } else {
        EntryOutcome::Verified(VerifyOutcome::Fail {
            actual: actual.clone(),
            expected: expected.clone(),
        })
    }
}

// ---------------------------------------------------------------------------
// Output formatting
// ---------------------------------------------------------------------------

fn print_results(results: &[EntryResult]) {
    let mut passed = 0usize;
    let mut failed = 0usize;
    let mut skipped = 0usize;

    for result in results {
        match &result.outcome {
            EntryOutcome::Skipped => {
                skipped += 1;
                println!("  SKIP {}", result.id);
            }
            EntryOutcome::Verified(VerifyOutcome::Pass) => {
                passed += 1;
                println!("  PASS {}", result.id);
            }
            EntryOutcome::Verified(VerifyOutcome::Fail { actual, expected }) => {
                failed += 1;
                eprintln!("  FAIL {}", result.id);
                eprintln!("       expected: {}", format_output_inline(expected));
                eprintln!("       actual:   {}", format_output_inline(actual));
            }
            EntryOutcome::Verified(VerifyOutcome::RunError(msg)) => {
                failed += 1;
                eprintln!("  FAIL {} — runtime error: {}", result.id, msg);
            }
            EntryOutcome::Verified(VerifyOutcome::IoError(msg)) => {
                failed += 1;
                eprintln!("  FAIL {} — i/o error: {}", result.id, msg);
            }
        }
    }

    println!();
    println!("playground verify: {passed} passed, {failed} failed, {skipped} skipped");
}

fn format_output_inline(s: &str) -> String {
    let trimmed = s.trim_end_matches('\n');
    if trimmed.contains('\n') {
        // Multi-line: show first line with an ellipsis
        let first = trimmed.lines().next().unwrap_or("");
        format!("{first:?} (…{} more lines)", trimmed.lines().count() - 1)
    } else {
        format!("{trimmed:?}")
    }
}

// ---------------------------------------------------------------------------
// Unit tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- format_runtime_failure_message ---

    #[test]
    fn runtime_failure_message_includes_stderr() {
        let msg = format_runtime_failure_message(1, "", "fatal: segmentation fault\n");
        assert_eq!(
            msg,
            "program exited with status 1\nstderr:\nfatal: segmentation fault\n"
        );
    }

    #[test]
    fn runtime_failure_message_includes_stdout_and_stderr() {
        let msg = format_runtime_failure_message(2, "partial output\n", "error: oops\n");
        assert!(
            msg.contains("stdout:\npartial output\n"),
            "stdout missing from: {msg}"
        );
        assert!(
            msg.contains("stderr:\nerror: oops\n"),
            "stderr missing from: {msg}"
        );
    }

    #[test]
    fn runtime_failure_message_omits_empty_streams() {
        let msg = format_runtime_failure_message(1, "", "");
        assert_eq!(msg, "program exited with status 1");
    }

    // --- ManifestEntry deserialization ---

    #[test]
    fn manifest_entry_deserializes() {
        let json = r#"[
          {
            "id": "basics/hello_world",
            "category": "basics",
            "name": "Hello World",
            "description": "Your first Hew program",
            "source_path": "basics/hello_world.hew",
            "expected_path": "basics/hello_world.expected",
            "capabilities": {
              "browser": "analysis-only",
              "wasi": "runnable"
            }
          }
        ]"#;

        let entries: Vec<ManifestEntry> = serde_json::from_str(json).unwrap();
        assert_eq!(entries.len(), 1);
        let e = &entries[0];
        assert_eq!(e.id, "basics/hello_world");
        assert_eq!(e.source_path, "basics/hello_world.hew");
        assert_eq!(e.expected_path, "basics/hello_world.expected");
        assert_eq!(e.capabilities.wasi, "runnable");
    }

    #[test]
    fn manifest_entry_unsupported_deserializes() {
        let json = r#"[
          {
            "id": "concurrency/supervisor",
            "category": "concurrency",
            "name": "Supervisor",
            "description": "Supervision tree for fault-tolerant actors",
            "source_path": "concurrency/supervisor.hew",
            "expected_path": "concurrency/supervisor.expected",
            "capabilities": {
              "browser": "analysis-only",
              "wasi": "unsupported"
            }
          }
        ]"#;

        let entries: Vec<ManifestEntry> = serde_json::from_str(json).unwrap();
        assert_eq!(entries[0].capabilities.wasi, "unsupported");
    }

    // --- Capability filtering ---

    #[test]
    fn unsupported_entry_is_skipped() {
        let entry = ManifestEntry {
            id: "concurrency/supervisor".to_string(),
            source_path: "concurrency/supervisor.hew".to_string(),
            expected_path: "concurrency/supervisor.expected".to_string(),
            capabilities: Capabilities {
                wasi: "unsupported".to_string(),
            },
        };
        let outcome = verify_entry(&entry, Path::new("."), Duration::from_secs(5));
        assert!(matches!(outcome, EntryOutcome::Skipped));
    }

    #[test]
    fn unknown_wasi_value_is_skipped() {
        // Any value other than "runnable" is treated as not locally executable.
        let entry = ManifestEntry {
            id: "future/feature".to_string(),
            source_path: "future/feature.hew".to_string(),
            expected_path: "future/feature.expected".to_string(),
            capabilities: Capabilities {
                wasi: "partial".to_string(),
            },
        };
        let outcome = verify_entry(&entry, Path::new("."), Duration::from_secs(5));
        assert!(matches!(outcome, EntryOutcome::Skipped));
    }

    #[test]
    fn missing_expected_file_is_io_error() {
        // Create a temp source file but deliberately omit the expected file.
        let dir = tempfile::tempdir().unwrap();
        let src = dir.path().join("test.hew");
        std::fs::write(&src, "fn main() { println(\"hi\"); }\n").unwrap();

        let entry = ManifestEntry {
            id: "test/missing_expected".to_string(),
            source_path: "test.hew".to_string(),
            expected_path: "test.expected".to_string(),
            capabilities: Capabilities {
                wasi: "runnable".to_string(),
            },
        };
        let outcome = verify_entry(&entry, dir.path(), Duration::from_secs(30));
        assert!(
            matches!(outcome, EntryOutcome::Verified(VerifyOutcome::IoError(_))),
            "expected IoError when expected file is absent"
        );
    }

    // --- Output formatting ---

    #[test]
    fn format_output_inline_single_line() {
        assert_eq!(format_output_inline("Hello, World!\n"), "\"Hello, World!\"");
    }

    #[test]
    fn format_output_inline_multiline() {
        let s = "line1\nline2\nline3\n";
        let formatted = format_output_inline(s);
        assert!(formatted.contains("\"line1\""));
        assert!(formatted.contains("2 more lines"));
    }

    #[test]
    fn format_output_inline_empty() {
        assert_eq!(format_output_inline(""), "\"\"");
    }

    // --- Manifest parsing of real manifest ---

    #[test]
    fn real_manifest_parses_and_has_runnable_entries() {
        // Locate manifest relative to repo root using CARGO_MANIFEST_DIR.
        let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let repo_root = manifest_dir.parent().unwrap();
        let manifest_path = repo_root.join("examples/playground/manifest.json");

        let src = std::fs::read_to_string(&manifest_path)
            .expect("examples/playground/manifest.json must exist");
        let entries: Vec<ManifestEntry> =
            serde_json::from_str(&src).expect("manifest must parse as JSON");

        assert!(!entries.is_empty(), "manifest must have at least one entry");

        let runnable = entries
            .iter()
            .filter(|e| e.capabilities.wasi == "runnable")
            .count();
        assert!(
            runnable > 0,
            "manifest must have at least one runnable entry"
        );

        // Verify "unsupported" entries exist too (tests the skip path).
        let unsupported = entries
            .iter()
            .filter(|e| e.capabilities.wasi == "unsupported")
            .count();
        assert!(
            unsupported > 0,
            "manifest must have at least one unsupported entry (tests skip path)"
        );
    }
}
