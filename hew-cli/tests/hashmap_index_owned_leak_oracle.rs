//! `HashMap` `m[k]` owned-value leak oracle (indexed-accessor convergence).
//!
//! `m[k]` over a `HashMap<K, V>` is the trapping `Index::at` accessor: on a hit
//! the runtime semantic-clones the stored value into the caller's slot through
//! `hew_hashmap_get_clone_layout`, producing a FRESH, independently-droppable
//! owner — never a borrow into the live table. The table keeps its own copy and
//! frees it on `remove`/overwrite/drop. If the read aliased the table's slot
//! instead of cloning, removing the key would free the buffer the caller still
//! holds, producing a use-after-free read (scribbled bytes) or a double-free
//! (the caller's drop and `hew_hashmap_remove_layout` both releasing it).
//!
//! This is the headline GAP-2 drop-safety invariant for the `HashMap` indexed accessor
//! (`by-value-heap-params-are-borrows` P0), proven on the new trapping `m[k]`
//! path. `m.get(k) -> Option<V>` routes through the same clone choke, so this
//! oracle pins the choke for both siblings.
//!
//! ## What each oracle pins
//!
//! - **Exact contents under the poisoned-allocator triple** (any unix): insert
//!   an owned `Name { label: "owned-ok" }`, read it back through `m["k"]`,
//!   remove the key (freeing the table's copy), then read the cloned record's
//!   field. Under `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` a
//!   double-free aborts the process and a use-after-free read returns scribbled
//!   memory, changing the output. The string equality plus the clean exit pin
//!   both regression directions.
//!
//! - **Exact zero-leak assertion** (macOS-only via `leaks(1)`): run a SINGLE
//!   insert/index/remove cycle under the leak detector and assert exactly
//!   `0 leaks for 0 total leaked bytes`. The cycle is wrapped in a helper
//!   function so the stack slot holding the cloned record is gone at process
//!   exit, making any leaked heap genuinely unreachable. A regression that
//!   double-frees fails the scribble pin; one that leaks the clone (the read
//!   path retains but no drop runs) fails here on any non-zero byte.
//!
//! ## Skip behaviour
//!
//! The zero-leak oracle is macOS-only (`leaks(1)` is Darwin's allocator
//! inspector); elsewhere it logs `skip:` and returns. The scribble correctness
//! pin runs on any unix host.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// Single round-trip under the poisoned allocator: insert one `Name { label:
/// "owned-ok" }`, read it back through the trapping `m["k"]` accessor, remove
/// the key (freeing the table's copy), and read `got.label`. Under
/// `MallocScribble` a double-free aborts; a UAF read on `got.label` returns
/// scribbled memory (not `"owned-ok"`). The exact-string equality plus clean
/// exit pin both directions simultaneously.
const INDEX_GET_SINGLE_ROUNDTRIP_SOURCE: &str = "\
type Name { label: string; }\n\
\n\
fn main() {\n\
\x20   let m: HashMap<string, Name> = HashMap::new();\n\
\x20   m.insert(\"k\", Name { label: \"owned-ok\" });\n\
\x20   let got: Name = m[\"k\"];\n\
\x20   let _ = m.remove(\"k\");\n\
\x20   print(got.label);\n\
}\n";

/// Expected exact output for `INDEX_GET_SINGLE_ROUNDTRIP_SOURCE`. Any aliasing
/// or UAF changes the `got.label` read.
const INDEX_GET_SINGLE_ROUNDTRIP_EXPECTED: &str = "owned-ok";

/// Zero-leak fixture: the insert/index/remove cycle lives inside a helper
/// function (`run_one_cycle`) so the stack slot holding the cloned record is
/// gone by the time `leaks --atExit` inspects the heap. Any allocation that the
/// cycle fails to release (the cloned `label` buffer whose drop never runs)
/// becomes genuinely unreachable and is counted by `leaks(1)`.
///
/// Post-fix: `m["k"]` clones `label` into `got`; `m.remove("k")` frees the
/// table's `label`; `got` drops at the end of `run_one_cycle`, freeing the
/// clone. Zero live heap at process exit: `0 leaks for 0 total leaked bytes`.
const INDEX_GET_ZERO_LEAK_SOURCE: &str = "\
type Name { label: string; }\n\
\n\
fn run_one_cycle() {\n\
\x20   let m: HashMap<string, Name> = HashMap::new();\n\
\x20   m.insert(\"k\", Name { label: \"owned-ok\" });\n\
\x20   let got: Name = m[\"k\"];\n\
\x20   let _ = m.remove(\"k\");\n\
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
/// Returns `None` (with a `skip:` notice on stderr) when `leaks(1)` declines to
/// attach or does not emit the expected summary line.
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
        .prefix(&format!("hashmap-index-owned-{name}-"))
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
         double-free of the owned `Name.label` string (the `m[k]` read aliased the table's \
         slot while `hew_hashmap_remove_layout` also released it);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read back the label verbatim — scribbled/empty output indicates a \
         use-after-free read on `got.label` (the table slot was freed by `remove` before the \
         caller read the clone);\n{}",
        describe_output(&output)
    );
}

/// Compile `source`, run it under `leaks --atExit`, and assert exactly
/// `0 leaks for 0 total leaked bytes`. Any non-zero count or non-zero byte total
/// fails immediately — no tolerance, no slope.
///
/// The fixture must wrap the cycle in a helper function so the stack slot
/// holding the cloned record is gone at process exit, making any un-dropped heap
/// genuinely unreachable and visible to `leaks(1)`.
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
        .prefix(&format!("hashmap-index-owned-zero-leak-{name}-"))
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
         means the cloned `label` buffer was never freed (the `m[k]` retain has no matching \
         drop). Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked \
         allocation stack.",
        bin.display()
    );
    assert_eq!(
        leaked_bytes,
        0,
        "{name}: leaks(1) reported 0 leak nodes but {leaked_bytes} total leaked bytes — expected \
         exactly 0 bytes. The cloned record's `label` heap buffer must be freed when `got` drops. \
         Re-run with `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked stack.",
        bin.display()
    );

    eprintln!(
        "{name}: leaks(1) confirmed 0 leaks for 0 total leaked bytes — exact zero-leak oracle PASS"
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Exact-contents pin: a single insert/index/remove/read round-trip must print
/// `"owned-ok"` and exit clean under the poisoned allocator. Reverting the
/// trapping read to alias the table's slot (so `m[k]` borrows while
/// `hew_hashmap_remove_layout` also releases it) fails this with either an abort
/// (double-free) or garbled output (UAF read).
#[test]
fn hashmap_index_owned_value_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "hashmap_index_single_roundtrip",
        INDEX_GET_SINGLE_ROUNDTRIP_SOURCE,
        INDEX_GET_SINGLE_ROUNDTRIP_EXPECTED,
    );
}

/// Exact zero-leak oracle: an insert/index/remove cycle (wrapped in
/// `run_one_cycle()` so the stack slot is gone at process exit) must report
/// exactly `0 leaks for 0 total leaked bytes` under `leaks --atExit`. The
/// `m[k]` clone retains the `label` buffer; `got` dropping at scope exit frees
/// it; `m.remove` frees the table's copy. A regression that leaks the clone
/// fails the `== 0` assertion on any leaked byte.
#[test]
fn hashmap_index_owned_value_zero_leaks_exact() {
    assert_zero_leaks_exact("hashmap_index_zero_leak", INDEX_GET_ZERO_LEAK_SOURCE);
}
