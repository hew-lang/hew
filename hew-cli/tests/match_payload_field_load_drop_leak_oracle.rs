//! Retained string field-load drop leak oracle — the match/if-let payload arm.
//!
//! When a `match`/`if let` binds a record payload (`Some(r)`) and reads a heap
//! `string` field back out (`let name = r.name`), the field-load dest is a
//! FRESH `+1`-retained owner (codegen `retain_string_field_load`), not a
//! no-retain interior alias. `derive_cow_fresh_borrowed_owner` admits it for one
//! balancing `hew_string_drop`. The scope-close `Goto` filter must therefore
//! EXEMPT it from the projection-alias taint so its drop fires when the binder
//! leaves scope crossing the match join — it threads the REAL `locals` table so
//! `string_field_load_producer_dest` keeps the exemption (an empty table would
//! taint every field-load dest, strand the release at the join, and leak). The
//! payload alias itself (`r`) stays tainted via the `Move`-from-interior arm and
//! is freed once by the composite, so there is no double-free either way.
//!
//! The cloned-field arm (`r.name.to_upper()`) must report exactly `0 leaks` and
//! run clean under the poisoned allocator; the bare-field-load arm
//! (`let name = r.name`) must at minimum run clean — the field share drops once,
//! never twice — which the scribble guard pins.
//!
//! macOS-only for the zero-leak assertion (`leaks(1)` is Darwin's allocator
//! inspector); the scribble correctness pins run on any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// `match`-arm record payload, a heap `string` field cloned out
/// (`r.name.to_upper()`) and borrow-only final use, wrapped in a helper so the
/// scrutinee slot is dead at exit. The cloned owner must drop on the scope-close
/// edge; if it were tainted out (empty `locals`) the `to_upper()` buffer leaks.
const MATCH_FIELD_CLONE_SOURCE: &str = "\
type Row { name: string; }\n\
\n\
fn make(n: i64) -> Option<Row> {\n\
\x20   if n > 0 { Some(Row { name: \"g64-fieldload-heap\".to_upper() }) } else { None }\n\
}\n\
\n\
fn run() {\n\
\x20   let opt = make(1);\n\
\x20   match opt {\n\
\x20       Some(r) => { let name = r.name.to_upper(); if !name.is_empty() { print(\"m\"); } }\n\
\x20       None => { print(\"e\"); }\n\
\x20   }\n\
}\n\
\n\
fn main() {\n\
\x20   run();\n\
}\n";

/// Bare string field-load binder (`let name = r.name`): the field share must be
/// freed exactly once. The composite frees the record's copy; the binder's
/// retained share drops on scope-close. Borrow-only final use; must run clean
/// (no double-free) under the poisoned allocator.
const MATCH_FIELD_BARE_SOURCE: &str = "\
type Row { name: string; }\n\
\n\
fn make(n: i64) -> Option<Row> {\n\
\x20   if n > 0 { Some(Row { name: \"g64-bare-fieldload\".to_upper() }) } else { None }\n\
}\n\
\n\
fn run() {\n\
\x20   let opt = make(1);\n\
\x20   match opt {\n\
\x20       Some(r) => { let name = r.name; if !name.is_empty() { print(\"m\"); } }\n\
\x20       None => { print(\"e\"); }\n\
\x20   }\n\
}\n\
\n\
fn main() {\n\
\x20   run();\n\
}\n";

// ── leak measurement plumbing ─────────────────────────────────────────────

/// Compile `source` to a native binary via `hew compile --emit-dir`.
fn compile_to_native(source: &str, dir: &std::path::Path, name: &str) -> PathBuf {
    let hew_src = dir.join(format!("{name}.hew"));
    std::fs::write(&hew_src, source).expect("write hew source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit-dir utf-8"),
            hew_src.to_str().expect("hew src utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");

    assert!(
        output.status.success(),
        "hew compile failed for {name}:\n{}",
        describe_output(&output)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let bin = stdout
        .lines()
        .find_map(|l| l.strip_prefix("native: "))
        .unwrap_or_else(|| panic!("no `native:` line for {name}:\n{stdout}"))
        .to_string();
    PathBuf::from(bin)
}

/// Run `bin` under `leaks --atExit` + poisoned-allocator and parse the summary.
fn measure_leaks_exact(bin: &std::path::Path) -> Option<(usize, usize)> {
    let output = Command::new("leaks")
        .arg("--atExit")
        .arg("--")
        .arg(bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .ok()?;
    if !output.status.success() && output.stdout.is_empty() {
        eprintln!(
            "skip: leaks declined to attach to {}: {}",
            bin.display(),
            String::from_utf8_lossy(&output.stderr)
        );
        return None;
    }
    let report = String::from_utf8_lossy(&output.stdout);
    for line in report.lines() {
        if !line.contains(" leaks for ") && !line.contains(" leak for ") {
            continue;
        }
        let Some(rest) = line.strip_prefix("Process ") else {
            continue;
        };
        if !rest.chars().next().is_some_and(|c| c.is_ascii_digit()) {
            continue;
        }
        let Some(after_colon) = rest.split_once(": ").map(|(_, s)| s) else {
            continue;
        };
        let mut words = after_colon.split_whitespace();
        let count = words.next().and_then(|w| w.parse::<usize>().ok())?;
        let _ = words.next(); // "leaks" or "leak"
        let _ = words.next(); // "for"
        let bytes = words.next().and_then(|w| w.parse::<usize>().ok())?;
        return Some((count, bytes));
    }
    eprintln!(
        "skip: leaks attached to {} but produced no parseable summary",
        bin.display()
    );
    None
}

/// Compile + run under the poisoned allocator; assert clean exit + verbatim out.
fn assert_clean_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("match-field-load-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = Command::new(&bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .unwrap_or_else(|error| panic!("run {name} binary: {error}"));
    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash here means the field-load \
         share was double-freed (the binder release and composite both freed it);\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected,
        "{name} must read the bound field back verbatim — scribbled output is a use-after-free;\n{}",
        describe_output(&output)
    );
}

/// Compile + run under `leaks --atExit`; assert exactly 0 leaks for 0 bytes.
fn assert_zero_leaks_exact(name: &str, source: &str) {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: {name}: leaks(1) is macOS-only");
        return;
    }
    let leaks_avail = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !leaks_avail {
        eprintln!("skip: {name}: `leaks` binary not on PATH");
        return;
    }
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("match-field-load-zero-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let Some((leak_count, leaked_bytes)) = measure_leaks_exact(&bin) else {
        return;
    };
    assert_eq!(
        leak_count,
        0,
        "{name}: {leak_count} leak(s) — expected 0. The cloned field-load owner was NOT freed on \
         the scope-close edge (empty `locals` tainted the field-load dest). Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}`.",
        bin.display()
    );
    assert_eq!(
        leaked_bytes, 0,
        "{name}: {leaked_bytes} leaked bytes — expected 0."
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Cloned field-load (`r.name.to_upper()`) in a match arm: the fresh owner must
/// drop on the scope-close edge, exactly 0 leaks. Empty-`locals` taint regressed
/// this; the real-`locals` exemption keeps it droppable.
#[test]
fn match_payload_field_clone_zero_leaks_exact() {
    assert_zero_leaks_exact("g64_match_field_clone", MATCH_FIELD_CLONE_SOURCE);
}

#[test]
fn match_payload_field_clone_no_double_free_under_malloc_scribble() {
    assert_clean_under_malloc_scribble(
        "g64_match_field_clone_scribble",
        MATCH_FIELD_CLONE_SOURCE,
        "m",
    );
}

/// Bare field-load binder (`let name = r.name`): the field share drops exactly
/// once — never double-freed against the composite's recursive record drop.
#[test]
fn match_payload_bare_field_load_no_double_free_under_malloc_scribble() {
    assert_clean_under_malloc_scribble("g64_match_bare_field", MATCH_FIELD_BARE_SOURCE, "m");
}
