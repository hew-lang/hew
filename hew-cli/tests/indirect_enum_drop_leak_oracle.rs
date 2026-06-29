//! Indirect-enum heap-node drop leak oracle.
//!
//! An `indirect enum` value (spec §3.7.4) is a heap pointer to a tagged-union
//! node. Two pre-fix defects leaked those nodes on every program:
//!
//! - The eager heap-allocation prologue minted a node for EVERY non-parameter
//!   indirect-enum local — including move-temps and match destructure binders
//!   that immediately overwrite their slot with a received pointer, orphaning
//!   the freshly-allocated node (a per-binding leak no scope-exit drop could
//!   reclaim).
//! - No scope-exit drop freed the node at all.
//!
//! Pre-fix a 4-leaf tree leaked 19 nodes (`hew_alloc + 156`); a single leaf
//! leaked 3. Post-fix the construction-site-gated prologue + the recursive
//! `__hew_indirect_enum_free_<Enum>` free thunk (driven by the fail-closed
//! `derive_indirect_enum_drop_allowed` sole-owner allow-set) free every node
//! exactly once.
//!
//! ## What each oracle pins
//!
//! - **Exact zero-leak** (macOS-only via `leaks(1)`): a single leaf, a depth-2
//!   balanced tree, and a mutually-recursive `A` ↔ `B` chain each wrap their
//!   construct-consume cycle in a helper fn so the stack slot is dead at exit,
//!   then assert exactly `0 leaks for 0 total leaked bytes`. A regression that
//!   drops the free arm (or the prologue gate) leaks ≥1 node and fails `!= 0`.
//!
//! - **No double-free under the poisoned-allocator triple** (any unix): the
//!   double-free landmine control — a NAMED child node nested into a parent
//!   (`let left = Node(..); let t = Node(left, ..)`). The parent's recursive
//!   free reclaims `left`'s subtree; if `left` ALSO fired its own free this
//!   aborts under `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges`. The
//!   correct value output plus the clean exit pin both directions.
//!
//! ## Fixture-authoring rule (non-vacuous)
//!
//! Every fixture provably heap-allocates: an `indirect enum` value is ALWAYS a
//! `hew_alloc`'d node (there is no inline representation), so the `0 leaks`
//! assertions are never vacuous over a non-heap literal. The pre-fix leak
//! counts above (19 / 3) are the non-zero baseline these oracles ratchet to 0.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// Single-leaf: construct `Leaf(5)`, borrow-pass it to `val`, drop at scope
/// exit. The constructing binding is the sole owner; `val`'s by-value param is
/// a borrow and frees nothing. Pre-fix: 3 leaked nodes (the constructed node +
/// two overwritten eager allocs). Post-fix: 0.
const SINGLE_LEAF_SOURCE: &str = "\
indirect enum Tree { Leaf(i64); Node(Tree, Tree); }\n\
fn val(t: Tree) -> i64 { match t { Leaf(n) => n, Node(l, r) => val(l) + val(r), } }\n\
fn run_one_cycle() {\n\
\x20   let t = Leaf(5);\n\
\x20   let s = val(t);\n\
\x20   print(\"ok\");\n\
}\n\
fn main() { run_one_cycle(); }\n";

/// Depth-2 balanced tree: the constructing binding owns the root; its recursive
/// free reclaims every interior + leaf node. Pre-fix: 19 leaked nodes.
/// Post-fix: 0 (proves per-node free + recursion across depth).
const DEEP_TREE_SOURCE: &str = "\
indirect enum Tree { Leaf(i64); Node(Tree, Tree); }\n\
fn val(t: Tree) -> i64 { match t { Leaf(n) => n, Node(l, r) => val(l) + val(r), } }\n\
fn run_one_cycle() {\n\
\x20   let t = Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4)));\n\
\x20   let s = val(t);\n\
\x20   print(\"ok\");\n\
}\n\
fn main() { run_one_cycle(); }\n";

/// Mutually-recursive `A` ↔ `B`: each variant's free thunk recurses into the
/// other's via the partner thunk. Exercises the cross-thunk synthesis (and the
/// in-progress-thunk re-entry guard that prevents an infinite synthesis loop).
/// Post-fix: 0 leaks for the whole chain.
const MUTUAL_SOURCE: &str = "\
indirect enum A { AEnd(i64); AWrap(B); }\n\
indirect enum B { BEnd(i64); BWrap(A); }\n\
fn depthA(a: A) -> i64 { match a { AEnd(n) => n, AWrap(b) => depthB(b) + 1, } }\n\
fn depthB(b: B) -> i64 { match b { BEnd(n) => n, BWrap(a) => depthA(a) + 1, } }\n\
fn run_one_cycle() {\n\
\x20   let v = AWrap(BWrap(AWrap(BEnd(7))));\n\
\x20   let d = depthA(v);\n\
\x20   print(\"ok\");\n\
}\n\
fn main() { run_one_cycle(); }\n";

/// Double-free landmine control: a NAMED child node is nested into a parent.
/// `left` is moved into `t`'s `Node` payload, so the parent's recursive free
/// owns it — `left` must NOT fire its own free. A regression that admits `left`
/// double-frees under the poisoned allocator. The `sum` output (1+2+3 = 6) plus
/// the clean exit pin both directions.
const NAMED_CHILD_DF_CONTROL_SOURCE: &str = "\
indirect enum Tree { Leaf(i64); Node(Tree, Tree); }\n\
fn sum(t: Tree) -> i64 { match t { Leaf(n) => n, Node(l, r) => sum(l) + sum(r), } }\n\
fn main() {\n\
\x20   let left = Node(Leaf(1), Leaf(2));\n\
\x20   let t = Node(left, Leaf(3));\n\
\x20   let s = sum(t);\n\
\x20   if s == 6 { print(\"ok\"); } else { print(\"BAD\"); }\n\
}\n";

/// Expected output for the double-free control (`1 + 2 + 3 == 6`).
const NAMED_CHILD_DF_CONTROL_EXPECTED: &str = "ok";

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
        let _ = words.next(); // "leaks" / "leak"
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
        "skip: leaks did not emit a summary for {}: stderr=\n{}",
        bin.display(),
        String::from_utf8_lossy(&output.stderr)
    );
    None
}

/// Compile `source`, run under `leaks --atExit`, assert exactly `(0, 0)`.
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
        .prefix(&format!("indirect-enum-drop-zero-leak-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);

    let Some((leak_count, leaked_bytes)) = measure_leaks_exact(&bin) else {
        return;
    };

    assert_eq!(
        leak_count,
        0,
        "{name}: leaks(1) reported {leak_count} leak(s) — expected exactly 0. A non-zero \
         count means an indirect-enum heap node was not freed: either the construction-site \
         prologue gate over-allocated an orphaned node, or the recursive \
         `__hew_indirect_enum_free_<Enum>` free arm did not reach a node. Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked stack.",
        bin.display()
    );
    assert_eq!(
        leaked_bytes,
        0,
        "{name}: leaks(1) reported 0 leak nodes but {leaked_bytes} total leaked bytes — \
         expected exactly 0. The recursive free thunk must `hew_dealloc` every owned node \
         with its full outer-struct size. Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}`.",
        bin.display()
    );

    eprintln!("{name}: leaks(1) confirmed 0 leaks for 0 total leaked bytes — PASS");
}

/// Compile `source`, run under the poisoned-allocator triple, assert clean exit
/// + exact stdout (the double-free / use-after-free pin).
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("indirect-enum-drop-df-{name}-"))
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
        "{name} must run clean under the poisoned allocator — an abort here is a double-free \
         of an indirect-enum node (a child nested into a parent fired its own free in addition \
         to the parent's recursive free);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must print the computed sum verbatim — wrong/scribbled output is a \
         use-after-free read of a node freed too early;\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Single-leaf node: constructed-and-borrow-passed, freed exactly once at scope
/// exit. Pre-fix 3 leaks → 0.
#[test]
fn indirect_enum_single_leaf_zero_leaks_exact() {
    assert_zero_leaks_exact("single_leaf", SINGLE_LEAF_SOURCE);
}

/// Depth-2 tree: the recursive free reclaims every interior + leaf node.
/// Pre-fix 19 leaks → 0 (per-node free + recursion).
#[test]
fn indirect_enum_deep_tree_zero_leaks_exact() {
    assert_zero_leaks_exact("deep_tree", DEEP_TREE_SOURCE);
}

/// Mutually-recursive `A` ↔ `B`: the cross-thunk recursive free frees the whole
/// chain with no leak (and the synthesis does not loop forever).
#[test]
fn indirect_enum_mutual_recursion_zero_leaks_exact() {
    assert_zero_leaks_exact("mutual", MUTUAL_SOURCE);
}

/// Double-free landmine control: a named child nested into a parent must be
/// freed by the parent's recursive free EXACTLY once. A regression that also
/// admits the child's own free aborts under the poisoned allocator.
#[test]
fn indirect_enum_named_child_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "named_child_df_control",
        NAMED_CHILD_DF_CONTROL_SOURCE,
        NAMED_CHILD_DF_CONTROL_EXPECTED,
    );
}
