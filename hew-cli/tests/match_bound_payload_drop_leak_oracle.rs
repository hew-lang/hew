//! Match/if-let-bound owned-payload drop leak oracle (hew-lang/hew gap #63 guard).
//!
//! When a `match`/`if let` binds an owned heap payload out of an enum scrutinee
//! the binder is a non-owning ALIAS of the payload — `derive_cow_sole_owner`'s
//! projection-alias taint already excludes the binder from its OWN drop. The
//! composite scrutinee stays the single owner and frees the payload through its
//! tag-aware `DropKind::EnumInPlace` scope-exit drop (the `EnumInPlace` safety
//! net in `derive_enum_composite_drop_allowed`): a binder that only READS the
//! payload (a borrowing transform / comparison / `.len()` / `.is_empty()`) does
//! not transfer ownership out, so the composite keeps its drop and the payload
//! is freed exactly once. The #63 dispute is settled — this is the established
//! behaviour and needs NO prover change. This oracle is the regression GUARD
//! that pins it.
//!
//! ## Why the fixture allocates real heap and borrows without an f-string
//!
//! `Some("…".to_upper())` produces a FRESH refcounted buffer (a static string
//! literal would be vacuously leak-clean — there is nothing to free, so the
//! assertion would have no teeth). The bound payload's final use is a pure
//! BORROW (`!s.is_empty()`), never an f-string interpolation: an f-string in the
//! borrow/println path would construct an out-of-scope concat temp whose own
//! drop is a SEPARATE obligation, masking whether the match-bound payload itself
//! was freed (the f-string-temp leak the #54 work documented). Keeping the read
//! a bare borrow isolates the question to the composite's `EnumInPlace` drop.
//!
//! Two shapes (the Stage-0 `g63_iso_A2_clean` / `g63_iso_F` templates):
//!
//! * `match opt { Some(s) => … }` — the match-arm binder; and
//! * `if let Some(s) = opt { … }` — the if-let binder.
//!
//! Both bind the same owned `Option<string>` payload and borrow it; both must
//! report exactly `0 leaks for 0 total leaked bytes`. The cycle is wrapped in a
//! helper so the scrutinee's stack slot is dead at `leaks --atExit`.
//!
//! macOS-only for the zero-leak assertion (`leaks(1)` is Darwin's allocator
//! inspector); the scribble correctness pin runs on any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// `match`-arm binder over an owned `Option<string>` heap payload, borrow-only
/// final use (`!s.is_empty()`), wrapped in a helper so the scrutinee slot is
/// dead at exit. The composite's `EnumInPlace` drop is the single owner of the
/// payload buffer; if it were wrongly excluded the `to_upper()` buffer leaks.
const G63_MATCH_BOUND_SOURCE: &str = "\
fn make(n: i64) -> Option<string> {\n\
\x20   if n > 0 {\n\
\x20       Some(\"g63-match-heap-payload\".to_upper())\n\
\x20   } else {\n\
\x20       None\n\
\x20   }\n\
}\n\
\n\
fn run() {\n\
\x20   let opt = make(1);\n\
\x20   match opt {\n\
\x20       Some(s) => { if !s.is_empty() { print(\"m\"); } }\n\
\x20       None => { print(\"e\"); }\n\
\x20   }\n\
}\n\
\n\
fn main() {\n\
\x20   run();\n\
}\n";

/// `if let` binder over the same owned `Option<string>` heap payload, borrow-only
/// final use. The if-let desugars to the same composite-owned destructure; the
/// bound payload must be freed by the scrutinee's `EnumInPlace` drop exactly once.
const G63_IFLET_BOUND_SOURCE: &str = "\
fn make(n: i64) -> Option<string> {\n\
\x20   if n > 0 {\n\
\x20       Some(\"g63-iflet-heap-payload\".to_upper())\n\
\x20   } else {\n\
\x20       None\n\
\x20   }\n\
}\n\
\n\
fn run() {\n\
\x20   let opt = make(1);\n\
\x20   if let Some(s) = opt {\n\
\x20       if !s.is_empty() { print(\"i\"); }\n\
\x20   }\n\
}\n\
\n\
fn main() {\n\
\x20   run();\n\
}\n";

// ── leak measurement plumbing ─────────────────────────────────────────────

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
        let Some(count_str) = words.next() else {
            continue;
        };
        let Ok(count) = count_str.parse::<usize>() else {
            continue;
        };
        let _ = words.next(); // "leaks" or "leak"
        let _ = words.next(); // "for"
        let Some(bytes_str) = words.next() else {
            continue;
        };
        let Ok(bytes) = bytes_str.parse::<usize>() else {
            continue;
        };
        return Some((count, bytes));
    }
    eprintln!(
        "skip: leaks attached to {} but produced no parseable summary",
        bin.display()
    );
    None
}

/// Compile `source`, run it under the poisoned-allocator triple, and assert it
/// exits cleanly with `expected` on stdout. A double-free of the payload buffer
/// (the composite `EnumInPlace` drop firing while a binder release also freed it)
/// aborts here; a use-after-free read garbles the borrow.
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("match-bound-payload-{name}-"))
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
        "{name} must run clean under the poisoned allocator — a crash here means the \
         match-bound payload was double-freed (the composite `EnumInPlace` drop and a \
         spurious binder release both freed the `to_upper()` buffer);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read the bound payload back verbatim — scribbled/empty output indicates a \
         use-after-free read on the match-bound string;\n{}",
        describe_output(&output)
    );
}

/// Compile `source`, run it under `leaks --atExit`, and assert exactly
/// `0 leaks for 0 total leaked bytes`. The fixture wraps the destructure cycle in
/// a helper so the scrutinee's stack slot is dead at process exit.
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
        .prefix(&format!("match-bound-payload-zero-leak-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);

    let Some((leak_count, leaked_bytes)) = measure_leaks_exact(&bin) else {
        return;
    };

    assert_eq!(
        leak_count,
        0,
        "{name}: leaks(1) reported {leak_count} leak(s) — expected exactly 0. A non-zero count \
         means the match-bound owned payload was NOT freed by the scrutinee's `EnumInPlace` drop \
         (the #63 safety net regressed). Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see the stack.",
        bin.display()
    );
    assert_eq!(
        leaked_bytes,
        0,
        "{name}: leaks(1) reported 0 leak nodes but {leaked_bytes} total leaked bytes — \
         expected exactly 0 bytes. Re-run with `MallocStackLogging=1 leaks --atExit -- {}`.",
        bin.display()
    );

    eprintln!(
        "{name}: leaks(1) confirmed 0 leaks for 0 total leaked bytes — #63 match-bound \
         payload drop GUARD PASS"
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Exact zero-leak guard: a `match`-arm-bound owned `Option<string>` payload,
/// final use a bare borrow, must report exactly `0 leaks for 0 total leaked
/// bytes` — the scrutinee's `EnumInPlace` drop frees the `to_upper()` buffer.
#[test]
fn match_bound_owned_payload_zero_leaks_exact() {
    assert_zero_leaks_exact("g63_match_bound", G63_MATCH_BOUND_SOURCE);
}

/// Exact zero-leak guard for the `if let` binder shape: same owned payload bound
/// through `if let Some(s) = opt`, borrow-only, must also report exactly 0 leaks.
#[test]
fn iflet_bound_owned_payload_zero_leaks_exact() {
    assert_zero_leaks_exact("g63_iflet_bound", G63_IFLET_BOUND_SOURCE);
}

/// Double-free / use-after-free guard under the poisoned allocator: the
/// match-bound payload must run clean and read back its borrow. The composite's
/// single `EnumInPlace` drop owns the payload; the binder earns no second release.
#[test]
fn match_bound_owned_payload_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble("g63_match_bound_scribble", G63_MATCH_BOUND_SOURCE, "m");
}
