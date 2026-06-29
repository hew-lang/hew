//! Interior-alias clone-out leak oracle (hew-lang/hew gap #54).
//!
//! `match rows.get(i) { Some(r) => { let copied = r.name.to_upper(); … } }`
//! over an owned-element `Vec<Row>` exercises THREE shares of the same heap
//! string before the fix:
//!
//!   1. the Vec element's `name` buffer (owned by the Vec, freed by
//!      `hew_vec_free_owned`);
//!   2. `first.row.name` — `rows.get(i)` is `hew_vec_get_clone`, which deep-clones
//!      the owned record and retains its string field (`+1`), so the
//!      `Option<Row>` payload aliases the element buffer;
//!   3. `r.name` loaded out of the match-bound payload — codegen retains the field
//!      load (`retain_string_field_load` → `hew_string_clone`, another `+1`).
//!
//! `r.name.to_upper()` then allocates a FRESH buffer (`copied`), a fourth owner.
//!
//! Pre-fix the interior-alias taint over-excluded the whole `Some(Row)` payload
//! composite from its `EnumInPlace` drop (the destructure binder `r` is seeded
//! interior-alias-tainted even though `first` is a freshly-cloned sole owner), so
//! the payload buffer leaked; the field-load retain temp had no balancing drop and
//! leaked too; and `copied` — a match-arm-scoped fresh owner used only by a borrow
//! — was `Live` at the arm's closing `Goto` but `Uninit` at the post-match join, so
//! the function-exit pass never fired its drop. Net: a per-call leak that scales
//! with the surviving shares.
//!
//! ## What each oracle pins
//!
//! - **Exact contents under the poisoned-allocator triple** (any unix): clone a
//!   heap field out of an aliased Vec element and read the result back. Under
//!   `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` a double-free aborts
//!   the process and a use-after-free read returns scribbled memory. The exact
//!   string plus the clean exit pin both regression directions — most importantly
//!   that admitting the composite's `EnumInPlace` drop and the field-load retain's
//!   balancing drop did NOT introduce a double-free of the shared buffer.
//!
//! - **Exact zero-leak assertion** (macOS-only via `leaks(1)`): a single
//!   clone-out cycle wrapped in a helper function (so the stack slot is dead at
//!   `leaks --atExit`) must report exactly `0 leaks for 0 total leaked bytes`.
//!   Pre-fix this leaked the payload buffer, the field-load retain, and `copied`.
//!
//! - **True-interior-alias negative control** (double-free guard): a Vec element
//!   bound through the BORROW getter (`rows[0]` → `hew_vec_get_owned`, an interior
//!   pointer into the still-live Vec's element slot) and only borrowed must stay
//!   leak-clean AND not double-free — the Vec's `hew_vec_free_owned` owns the
//!   element, so the interior-alias taint must keep the binding excluded from any
//!   independent release.
//!
//! ## Skip behaviour
//!
//! The zero-leak oracle is macOS-only (`leaks(1)` is Darwin's allocator
//! inspector); elsewhere it logs `skip:` and returns. The scribble correctness
//! pins run on any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// Clone a heap `string` field out of an aliased `Vec<Row>` element and read the
/// uppercased result back. Under the poisoned allocator a double-free of the
/// shared element buffer aborts; a UAF read returns scribbled memory (not the
/// expected uppercased string). The exact-string equality plus clean exit pin
/// both directions.
const INTERIOR_CLONE_CONTENTS_SOURCE: &str = "\
type Row { name: string; }\n\
\n\
fn run() {\n\
\x20   let rows: Vec<Row> = [ Row { name: \"interior-clone-ok\".to_upper() } ];\n\
\x20   let first = rows.get(0);\n\
\x20   match first {\n\
\x20       Some(r) => { let copied = r.name.to_upper(); print(copied); }\n\
\x20       None => { print(\"none\"); }\n\
\x20   }\n\
}\n\
\n\
fn main() {\n\
\x20   run();\n\
}\n";

/// Expected exact output for `INTERIOR_CLONE_CONTENTS_SOURCE`. Any double-free
/// abort or UAF read changes this.
const INTERIOR_CLONE_CONTENTS_EXPECTED: &str = "INTERIOR-CLONE-OK";

/// Zero-leak fixture: the clone-out cycle lives in a helper (`run`) so the stack
/// slots holding the Vec, the `Option<Row>` payload, and `copied` are gone by the
/// time `leaks --atExit` inspects the heap. The final use of `copied` is a BORROW
/// (`copied.len()`) so the clone-out's own balancing drop must fire at the
/// match-arm scope close — this is exactly the leak the fix closes. Any
/// un-released share (the payload composite, the field-load retain, or `copied`)
/// becomes unreachable and is counted by `leaks(1)`.
const INTERIOR_CLONE_ZERO_LEAK_SOURCE: &str = "\
type Row { name: string; }\n\
\n\
fn run() {\n\
\x20   let rows: Vec<Row> = [ Row { name: \"interior-clone-heap-name\".to_upper() } ];\n\
\x20   let first = rows.get(0);\n\
\x20   match first {\n\
\x20       Some(r) => { let copied = r.name.to_upper(); if copied.len() > 0 { print(\"nz\"); } }\n\
\x20       None => { print(\"e\"); }\n\
\x20   }\n\
}\n\
\n\
fn main() {\n\
\x20   run();\n\
}\n";

/// True-interior-alias NEGATIVE control (the hard double-free guard): bind a Vec
/// element through the BORROW getter (`rows[0]` → `hew_vec_get_owned`, which
/// returns an interior pointer into the still-live Vec's element slot, NOT a fresh
/// owner) and only BORROW its field. The element is owned by the Vec; its
/// `hew_vec_free_owned` runs the per-element record drop. Admitting `r` to its own
/// `RecordInPlace` — or the composite to an `EnumInPlace` over a borrowed payload —
/// would free the same buffer twice. The interior-alias taint
/// (`compute_collection_interior_alias_taint`) must keep `r` excluded; this oracle
/// proves the #54 narrowing did not re-admit a genuine interior alias: leak-clean
/// AND no double-free under the poisoned allocator.
const INTERIOR_ALIAS_NEG_CONTROL_SOURCE: &str = "\
type Row { name: string; }\n\
\n\
fn run() {\n\
\x20   let rows: Vec<Row> = [ Row { name: \"neg-alias-heap-name\".to_upper() } ];\n\
\x20   let r = rows[0];\n\
\x20   if r.name.len() > 0 { print(\"nz\"); }\n\
}\n\
\n\
fn main() {\n\
\x20   run();\n\
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
        .prefix(&format!("interior-alias-clone-{name}-"))
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
         double-free of the shared `Row.name` buffer (the composite `EnumInPlace` drop and a \
         field-load/alias release both freed it);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read the cloned value back verbatim — scribbled/empty output indicates a \
         use-after-free read on the cloned-out string;\n{}",
        describe_output(&output)
    );
}

/// Compile `source`, run it under `leaks --atExit`, and assert exactly
/// `0 leaks for 0 total leaked bytes`. The fixture must wrap the clone-out cycle
/// in a helper function so stack slots are gone at process exit.
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
        .prefix(&format!("interior-alias-clone-zero-leak-{name}-"))
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
         A non-zero count means an interior-alias clone-out share was not released \
         (the payload composite `EnumInPlace`, the field-load retain, or the arm-scoped \
         `copied`). Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see the stack.",
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
        "{name}: leaks(1) confirmed 0 leaks for 0 total leaked bytes — exact zero-leak oracle PASS"
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Exact-contents pin: clone a heap field out of an aliased Vec element and read
/// it back. Must print `"INTERIOR-CLONE-OK"` and exit clean under the poisoned
/// allocator. A regression that double-frees the shared buffer aborts; a UAF read
/// garbles the output.
#[test]
fn interior_alias_clone_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "interior_clone_contents",
        INTERIOR_CLONE_CONTENTS_SOURCE,
        INTERIOR_CLONE_CONTENTS_EXPECTED,
    );
}

/// Exact zero-leak oracle: a clone-out cycle (borrowing final use, wrapped in a
/// helper so stack slots are dead at exit) must report exactly `0 leaks for 0
/// total leaked bytes`. Pre-fix this leaked the payload composite, the field-load
/// retain, and the arm-scoped `copied` string.
#[test]
fn interior_alias_clone_zero_leaks_exact() {
    assert_zero_leaks_exact("interior_clone_zero_leak", INTERIOR_CLONE_ZERO_LEAK_SOURCE);
}

/// Negative control under the poisoned allocator: a true interior-alias rebind
/// with NO clone-out must stay clean (no double-free). The composite's single
/// `EnumInPlace` drop owns the payload; the alias earns no second release.
#[test]
fn interior_alias_true_alias_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "interior_alias_neg_control",
        INTERIOR_ALIAS_NEG_CONTROL_SOURCE,
        "nz",
    );
}

/// Exact zero-leak oracle for the negative control: a true interior-alias rebind
/// must also be leak-clean (the composite drop is the single owner — no leak, no
/// double-free).
#[test]
fn interior_alias_true_alias_zero_leaks_exact() {
    assert_zero_leaks_exact(
        "interior_alias_neg_control_leak",
        INTERIOR_ALIAS_NEG_CONTROL_SOURCE,
    );
}
