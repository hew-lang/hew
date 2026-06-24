//! Generic `Vec<T>.get` owned-element leak oracle (hew-lang/hew#1929 Stage 2).
//!
//! `Vec<T>.get` where `T` is a type parameter bound to a non-Copy record type
//! routes through the owned-element ABI: the getter calls `hew_vec_get_owned`
//! (borrows the live slot and transfers ownership to the caller), while the
//! Vec's scope-exit drop calls `hew_vec_free_owned` (runs the per-element
//! record drop thunk over every live slot). Pre-fix these two paths aliased
//! the same heap string — the getter transferred ownership while `free_owned`
//! also released the slot — producing a double-free on the first element's
//! `label` field within a few iterations. A missed drop on the other side
//! would grow the leak-node count linearly with iteration count.
//!
//! ## What each oracle pins
//!
//! - **Exact contents under the poisoned-allocator triple** (any unix): push an
//!   owned `Name { label: "owned-ok" }`, get it back through a generic helper,
//!   read the field, and drop. Under `MallocScribble`/`MallocPreScribble`/
//!   `MallocGuardEdges` a double-free aborts the process; a use-after-free read
//!   returns scribbled memory, changing the output. The string equality plus the
//!   clean exit pin both regression directions.
//!
//! - **Exact zero-leak assertion** (macOS-only via `leaks(1)`): run a SINGLE
//!   owned-record `Vec<T>.get` fixture (one allocation cycle) under the leak
//!   detector and assert exactly `0 leaks for 0 total leaked bytes`. The
//!   fixture wraps the push-get-drop in a helper function so the stack slot is
//!   not live at process exit, making any leaked heap genuinely unreachable.
//!   Post-fix: `hew_vec_free_owned` runs `__hew_record_drop_inplace_Name` over
//!   every live slot, freeing each `label` buffer before the Vec handle is
//!   released — both the count and the byte total are exactly 0.
//!   A regression that omits the per-element drop thunk (no
//!   `__hew_record_drop_inplace_Name` synthesis for a type-parameter-bound
//!   element type) leaks the `label` buffer and fails at `!= 0`.
//!
//! ## Skip behaviour
//!
//! The zero-leak oracle is macOS-only (`leaks(1)` is Darwin's allocator inspector);
//! elsewhere it logs `skip:` and returns. The scribble correctness pin runs on
//! any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// Single round-trip under the poisoned allocator: push one `Name { label:
/// "owned-ok" }`, call `first<T>` (the generic returning helper), read
/// `got.label`, and let both the returned record and the Vec drop at scope
/// exit. Under `MallocScribble` a double-free aborts; a UAF read on `got.label`
/// returns scribbled memory (not `"owned-ok"`). The exact-string equality plus
/// clean exit pin both directions simultaneously.
const GENERIC_GET_SINGLE_ROUNDTRIP_SOURCE: &str = "\
type Name { label: string; }\n\
\n\
fn first<T>(v: Vec<T>) -> Option<T> {\n\
\x20   v.get(0)\n\
}\n\
\n\
fn main() {\n\
\x20   let vn: Vec<Name> = Vec::new();\n\
\x20   vn.push(Name { label: \"owned-ok\" });\n\
\x20   let got = match first(vn) {\n\
\x20       Some(g) => g,\n\
\x20       None => { print(\"none\"); return; }\n\
\x20   };\n\
\x20   print(got.label);\n\
}\n";

/// Expected exact output for `GENERIC_GET_SINGLE_ROUNDTRIP_SOURCE`. Any
/// aliasing or UAF changes the `got.label` read.
const GENERIC_GET_SINGLE_ROUNDTRIP_EXPECTED: &str = "owned-ok";

/// Zero-leak fixture: the push-get-drop cycle lives inside a helper function
/// (`run_one_cycle`) so the stack slot holding the Vec and the returned record
/// are gone by the time `leaks --atExit` inspects the heap. Any allocation that
/// `hew_vec_free_owned` fails to release (e.g. a `label` buffer whose drop thunk
/// was not synthesised for the type-parameter-bound element) becomes genuinely
/// unreachable and is counted by `leaks(1)`.
///
/// Post-fix: `hew_vec_free_owned` runs `__hew_record_drop_inplace_Name` on the
/// single live slot → frees `label` → releases the Vec handle. Zero live heap
/// at process exit: `0 leaks for 0 total leaked bytes`.
const GENERIC_GET_ZERO_LEAK_SOURCE: &str = "\
type Name { label: string; }\n\
\n\
fn first<T>(v: Vec<T>) -> Option<T> {\n\
\x20   v.get(0)\n\
}\n\
\n\
fn run_one_cycle() {\n\
\x20   let vn: Vec<Name> = Vec::new();\n\
\x20   vn.push(Name { label: \"owned-ok\" });\n\
\x20   let got = match first(vn) {\n\
\x20       Some(g) => g,\n\
\x20       None => { print(\"none\"); return; }\n\
\x20   };\n\
\x20   print(got.label);\n\
}\n\
\n\
fn main() {\n\
\x20   run_one_cycle();\n\
}\n";

// ── leak measurement plumbing ─────────────────────────────────────────────────

/// Compile `source` to a native binary via `hew compile --emit-dir` and return
/// the binary path.
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

/// Run `bin` under the poisoned-allocator triple + `leaks --atExit` and return
/// `Some((leak_count, leaked_bytes))` when `leaks` produced a usable summary.
///
/// The parsed summary line has the form:
/// ```text
/// Process <pid>: N leaks for B total leaked bytes.
/// ```
/// where `N` is the leak count and `B` is the total leaked bytes. Both are
/// extracted so the caller can assert exactly `(0, 0)`.
///
/// Returns `None` (with a `skip:` notice on stderr) when `leaks(1)` declines
/// to attach or does not emit the expected summary line.
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
        // Match: "Process <pid>: N leak(s) for B total leaked bytes."
        if !line.contains(" leaks for ") && !line.contains(" leak for ") {
            continue;
        }
        let Some(rest) = line.strip_prefix("Process ") else {
            continue;
        };
        if !rest.chars().next().is_some_and(|c| c.is_ascii_digit()) {
            continue;
        }
        // rest = "<pid>: N leaks for B total leaked bytes."
        let Some(after_colon) = rest.split_once(": ").map(|(_, s)| s) else {
            continue;
        };
        // after_colon = "N leaks for B total leaked bytes."
        let mut words = after_colon.split_whitespace();
        let Some(count_str) = words.next() else {
            continue;
        };
        let Ok(count) = count_str.parse::<usize>() else {
            continue;
        };
        // skip "leaks" / "leak", "for"
        let _ = words.next(); // "leaks" or "leak"
        let _ = words.next(); // "for"
        let Some(bytes_str) = words.next() else {
            // No bytes field — can't form an exact assertion; skip gracefully.
            eprintln!(
                "skip: leaks summary for {} has count ({count}) but no bytes field: {line}",
                bin.display()
            );
            return None;
        };
        let Ok(bytes) = bytes_str.parse::<usize>() else {
            eprintln!(
                "skip: leaks summary for {} bytes field not a number ({bytes_str}): {line}",
                bin.display()
            );
            return None;
        };
        eprintln!("  leaks(1) summary: count={count} bytes={bytes} (from: {line})");
        return Some((count, bytes));
    }
    eprintln!(
        "skip: leaks did not emit a `Process <pid>: N leak(s) for B total leaked bytes.` \
         summary for {}: stderr=\n{}",
        bin.display(),
        String::from_utf8_lossy(&output.stderr)
    );
    None
}

/// Compile `source`, run it under the poisoned-allocator triple, and assert it
/// exits cleanly with `expected` on stdout.
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("vec-generic-get-owned-{name}-"))
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
        "{name} must run clean under the poisoned allocator — a crash here indicates a \
         double-free of the owned `Name.label` string (the generic getter and \
         `hew_vec_free_owned` both released the same slot);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read back the label verbatim — scribbled/empty output indicates a \
         use-after-free read on `got.label` (the slot was freed before the caller read it);\n{}",
        describe_output(&output)
    );
}

/// Compile `source`, run it under `leaks --atExit`, and assert exactly
/// `0 leaks for 0 total leaked bytes`. Any non-zero count or non-zero byte
/// total fails immediately — no tolerance, no slope.
///
/// The fixture must wrap the allocation cycle in a helper function so stack
/// slots holding the Vec and returned record are gone at process exit, making
/// any un-dropped heap genuinely unreachable and visible to `leaks(1)`.
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
        .prefix(&format!("vec-generic-get-owned-zero-leak-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);

    let Some((leak_count, leaked_bytes)) = measure_leaks_exact(&bin) else {
        return;
    };

    assert_eq!(
        leak_count,
        0,
        "{name}: leaks(1) reported {leak_count} leak(s) — expected exactly 0. \
         A non-zero count means `hew_vec_free_owned` did not run \
         `__hew_record_drop_inplace_Name` on the live slot, leaving the `label` buffer \
         unreachable. Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see \
         the leaked allocation stack.",
        bin.display()
    );
    assert_eq!(
        leaked_bytes,
        0,
        "{name}: leaks(1) reported 0 leak nodes but {leaked_bytes} total leaked bytes — \
         expected exactly 0 bytes. The per-element record drop thunk \
         (`__hew_record_drop_inplace_Name`) must free the `label` heap buffer completely. \
         Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked stack.",
        bin.display()
    );

    eprintln!(
        "{name}: leaks(1) confirmed 0 leaks for 0 total leaked bytes — exact zero-leak oracle PASS"
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Exact-contents pin: a single push-get-read-drop round-trip through a
/// generic `first<T>` helper must print `"owned-ok"` and exit clean under the
/// poisoned allocator. Reverting the owned-getter routing (so the getter aliases
/// the Vec's slot while `hew_vec_free_owned` also releases it) fails this with
/// either an abort (double-free) or garbled output (UAF read).
#[test]
fn vec_generic_get_owned_element_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "vec_generic_get_single_roundtrip",
        GENERIC_GET_SINGLE_ROUNDTRIP_SOURCE,
        GENERIC_GET_SINGLE_ROUNDTRIP_EXPECTED,
    );
}

/// Exact zero-leak oracle: a push-get-drop cycle through a generic `first<T>`
/// helper (wrapped in a `run_one_cycle()` function so stack slots are gone at
/// process exit) must report exactly `0 leaks for 0 total leaked bytes` under
/// `leaks --atExit`. Post-fix `hew_vec_free_owned` runs
/// `__hew_record_drop_inplace_Name` on the single live slot, freeing the `label`
/// buffer before the Vec handle is released. A regression that omits the per-
/// element drop thunk synthesis for a type-parameter-bound element type leaves
/// the `label` buffer unreachable and fails the `== 0` assertion on ANY leaked
/// byte.
#[test]
fn vec_generic_get_owned_element_zero_leaks_exact() {
    assert_zero_leaks_exact("vec_generic_get_zero_leak", GENERIC_GET_ZERO_LEAK_SOURCE);
}
