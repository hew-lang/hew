//! Indirect-enum heap-node drop leak oracle.
//!
//! An `indirect enum` value (spec §3.7.4) is a heap pointer to a tagged-union
//! node. Three pre-fix defects leaked or corrupted those nodes:
//!
//! - The heap-allocation prologue minted a node for EVERY non-parameter
//!   indirect-enum local — including move-temps and match destructure binders
//!   that immediately overwrite their slot with a received pointer, orphaning
//!   the freshly-allocated node (a per-binding leak no scope-exit drop could
//!   reclaim). Fixed by the construction-site gate.
//! - No scope-exit drop freed the node at all. Fixed by the recursive
//!   `__hew_indirect_enum_free_<Enum>` free thunk.
//! - A node constructed AND consumed inside a LOOP was allocated ONCE by the
//!   entry-block prologue but freed every iteration, so the second iteration
//!   wrote through a freed pointer (use-after-free / double-free); and an
//!   inline-match-consumed construction binding was excluded from the drop
//!   allow-set (a per-iteration leak). Fixed (#46) by allocating each node at
//!   its construction site (`emit_indirect_enum_node_alloc`, so each iteration
//!   owns a fresh node) and by admitting the construction binding in
//!   `derive_indirect_enum_drop_allowed` (whole-value node-flow edges only, so a
//!   destructure binder no longer entangles its parent in the fan-out collapse).
//!
//! Pre-fix a 4-leaf tree leaked 19 nodes (`hew_alloc + 156`); a single leaf
//! leaked 3; a 50-iteration build-and-consume loop corrupted the allocator.
//! Post-fix the construction-site allocation + the recursive
//! `__hew_indirect_enum_free_<Enum>` free thunk (driven by the fail-closed
//! `derive_indirect_enum_drop_allowed` sole-owner allow-set) free every node
//! exactly once, per iteration.
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
//! - **Build-and-consume loop, zero slope** (#46): a recursive `indirect enum`
//!   constructed AND consumed every iteration, measured at two iteration counts.
//!   The leak count must not grow with the iteration count (a non-zero slope is
//!   a per-iteration leak from a construction binding the drop allow-set
//!   wrongly excluded), must be exactly 0 (no constant hoisted-node leak), and
//!   the loop must run clean under the poisoned allocator (no use-after-free of
//!   a re-entered node). Pins the lazy construction-site allocation and the
//!   inline-match drop admission together.
//!
//! - **Var-overwrite loop, zero slope** (#46 regression guard): a SINGLE `var t`
//!   reassigned a fresh `Node(Leaf, Leaf)` every iteration. With lazy
//!   construction-site allocation each right-hand side mints a fresh node, so the
//!   prior node `t` held must be freed at the binding overwrite
//!   (`Instr::Move { dest: Local(owner) }` → `emit_indirect_enum_overwrite_release`,
//!   gated on the MIR-admitted owner set). This shape regressed to ~3 leaks/iter
//!   when construction-site allocation first replaced the eager entry-prologue
//!   (whose single reused node masked the missing release); the oracle measures
//!   two iteration counts (slope 0), asserts exactly 0, and runs clean under the
//!   poisoned allocator (the first reassignment must read the null-initialised
//!   slot, not free uninitialised bytes; later ones must not double-free).
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

/// Build-and-consume LOOP fixture (#46), parameterised on the iteration count.
///
/// Every iteration constructs `Node(Leaf(1), Leaf(2))` (three heap nodes) and
/// consumes it via the borrow-taking `val`, whose by-value param frees nothing —
/// so the constructing binding `t` is the sole owner and its scope-exit
/// `DropKind::IndirectEnum` free must reclaim all three nodes each iteration. The
/// loop body lives in `run_loop` so the binding slot is dead at process exit (the
/// `leaks --atExit` root set does not see a stale stack pointer). `run_loop`
/// returns the running total (`3 * iters`), checked in `main`, so the loop is
/// never dead-code-eliminated and a use-after-free corrupts the printed result.
fn loop_build_consume_source(iters: u64) -> String {
    let expected_total = iters * 3; // val(Node(Leaf(1), Leaf(2))) == 1 + 2 == 3
    format!(
        "indirect enum Tree {{ Leaf(i64); Node(Tree, Tree); }}\n\
         fn val(t: Tree) -> i64 {{ match t {{ Leaf(n) => n, Node(l, r) => val(l) + val(r), }} }}\n\
         fn run_loop() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   for i in 0..{iters} {{\n\
         \x20       let t = Node(Leaf(1), Leaf(2));\n\
         \x20       let s = val(t);\n\
         \x20       total = total + s;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() {{\n\
         \x20   let r = run_loop();\n\
         \x20   if r == {expected_total} {{ print(\"ok\"); }} else {{ print(\"BAD\"); }}\n\
         }}\n"
    )
}

/// Build-and-consume LOOP via VAR-OVERWRITE (#46 regression guard), parameterised
/// on the iteration count.
///
/// Distinct from `loop_build_consume_source`: there a FRESH per-iteration `let t`
/// is freed by its own scope-exit drop. Here a SINGLE `var t` is REASSIGNED every
/// iteration (`t = Node(Leaf(1), Leaf(2))`), so the node `t` previously held is
/// orphaned at the binding overwrite unless codegen releases it there. Lazy
/// construction-site allocation mints a fresh node for each right-hand side, so
/// without the overwrite-release — `Instr::Move { dest: Local(owner), .. }` freeing
/// the owned prior node before the slot is overwritten — this leaks one
/// `Node(Leaf, Leaf)` (three heap nodes) per iteration. `t` is seeded with
/// `Leaf(0)` before the loop (reclaimed at the first reassignment) and its final
/// value is reclaimed by `run_loop`'s scope-exit drop. `run_loop` returns the
/// running total (`3 * iters`) so the loop is never dead-code-eliminated and the
/// binding slot is dead at process exit.
///
/// This shape regressed to ~3 leaks/iteration when construction-site allocation
/// first replaced the eager entry-prologue (whose single reused node masked the
/// missing overwrite release); the oracle pins it so a future refactor of either
/// the allocation site or the owner-set gate cannot silently reintroduce it.
fn var_overwrite_loop_source(iters: u64) -> String {
    let expected_total = iters * 3; // val(Node(Leaf(1), Leaf(2))) == 1 + 2 == 3
    format!(
        "indirect enum Tree {{ Leaf(i64); Node(Tree, Tree); }}\n\
         fn val(t: Tree) -> i64 {{ match t {{ Leaf(n) => n, Node(l, r) => val(l) + val(r), }} }}\n\
         fn run_loop() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   var t = Leaf(0);\n\
         \x20   for i in 0..{iters} {{\n\
         \x20       t = Node(Leaf(1), Leaf(2));\n\
         \x20       total = total + val(t);\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() {{\n\
         \x20   let r = run_loop();\n\
         \x20   if r == {expected_total} {{ print(\"ok\"); }} else {{ print(\"BAD\"); }}\n\
         }}\n"
    )
}

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

/// #46 — the headline build-and-consume LOOP oracle. A recursive `indirect enum`
/// constructed AND consumed every iteration must free its nodes per iteration:
/// the leak count must NOT grow with the iteration count (slope 0). Measuring at
/// two iteration counts and asserting equal counts cancels the constant
/// `leaks --atExit` root-set noise, isolating the per-iteration slope. A growing
/// slope means the construction binding `t` was excluded from the drop allow-set
/// (`derive_indirect_enum_drop_allowed`), so each iteration orphaned its three
/// freshly-allocated nodes.
#[test]
fn indirect_enum_loop_build_consume_leak_slope_is_zero() {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: loop leak slope: leaks(1) is macOS-only");
        return;
    }
    let leaks_avail = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !leaks_avail {
        eprintln!("skip: loop leak slope: `leaks` not on PATH");
        return;
    }
    require_codegen();

    let lo_iters = 1u64;
    let hi_iters = 50u64;

    let dir = tempfile::Builder::new()
        .prefix("indirect-enum-loop-slope-")
        .tempdir()
        .expect("tempdir");
    let bin_lo = compile_to_native(&loop_build_consume_source(lo_iters), dir.path(), "loop_lo");
    let bin_hi = compile_to_native(&loop_build_consume_source(hi_iters), dir.path(), "loop_hi");

    let (Some((lo_count, _)), Some((hi_count, _))) =
        (measure_leaks_exact(&bin_lo), measure_leaks_exact(&bin_hi))
    else {
        eprintln!("skip: loop leak slope: leaks declined to attach");
        return;
    };

    assert_eq!(
        hi_count,
        lo_count,
        "indirect-enum build-and-consume loop leaks {hi_count} nodes at {hi_iters} iterations \
         vs {lo_count} at {lo_iters} — a NON-ZERO per-iteration slope ({delta} extra nodes over \
         {span} iterations). Every iteration constructs `Node(Leaf(1), Leaf(2))` and consumes it \
         via `val`; the scope-exit `DropKind::IndirectEnum` free must reclaim all three nodes each \
         iteration. A growing slope means the construction binding was not admitted for drop in \
         `derive_indirect_enum_drop_allowed`, orphaning each iteration's freshly-allocated nodes.",
        delta = hi_count.saturating_sub(lo_count),
        span = hi_iters - lo_iters,
    );

    // Belt-and-suspenders: post-fix the count is not merely FLAT, it is exactly 0
    // — a flat-but-nonzero count would be a hoisted entry-prologue allocation no
    // per-iteration drop reclaims.
    assert_eq!(
        hi_count, 0,
        "indirect-enum build-and-consume loop has a flat but NON-ZERO leak count ({hi_count}); \
         the slope is 0 yet a constant set of nodes leaks every run.",
    );

    eprintln!(
        "loop_build_consume: leak slope 0 ({lo_count}@{lo_iters} == {hi_count}@{hi_iters}), \
         exact 0 — PASS"
    );
}

/// #46 — the build-and-consume loop must run clean under the poisoned allocator:
/// no use-after-free of a re-entered node, no double-free. Pre-fix the
/// entry-block prologue allocated the node ONCE; the second iteration's tag store
/// wrote through the pointer the first iteration's scope-exit free had already
/// reclaimed (`EXC_BAD_ACCESS` / abort under `MallocScribble`). The `ok` output
/// (total `3 * 50 == 150`) plus the clean exit pin both directions.
#[test]
fn indirect_enum_loop_build_consume_no_corruption_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "loop_build_consume_df",
        &loop_build_consume_source(50),
        "ok",
    );
}

/// #46 — the build-and-consume loop, exact-zero-leak at a representative
/// iteration count (the loop body is wrapped in `run_loop` so the binding slot is
/// dead at process exit). Complements the slope oracle: slope-0 proves no
/// per-iteration leak; this proves no constant leak either.
#[test]
fn indirect_enum_loop_build_consume_zero_leaks_exact() {
    assert_zero_leaks_exact("loop_build_consume", &loop_build_consume_source(50));
}

/// #46 regression guard — the VAR-OVERWRITE build-and-consume loop has a zero
/// per-iteration leak slope. A single `var t` is reassigned each iteration; the
/// prior node must be released at the binding overwrite. A non-zero slope means
/// the overwrite-release did not fire (the owner-set gate missed the binding, or
/// construction-site allocation orphaned the prior node).
#[test]
fn indirect_enum_var_overwrite_loop_leak_slope_is_zero() {
    if !cfg!(target_os = "macos") {
        eprintln!("skip: var-overwrite loop leak slope: leaks(1) is macOS-only");
        return;
    }
    let leaks_avail = Command::new("which")
        .arg("leaks")
        .output()
        .is_ok_and(|o| o.status.success());
    if !leaks_avail {
        eprintln!("skip: var-overwrite loop leak slope: `leaks` not on PATH");
        return;
    }
    require_codegen();

    let lo_iters = 1u64;
    let hi_iters = 50u64;

    let dir = tempfile::Builder::new()
        .prefix("indirect-enum-var-overwrite-slope-")
        .tempdir()
        .expect("tempdir");
    let bin_lo = compile_to_native(&var_overwrite_loop_source(lo_iters), dir.path(), "var_lo");
    let bin_hi = compile_to_native(&var_overwrite_loop_source(hi_iters), dir.path(), "var_hi");

    let (Some((lo_count, _)), Some((hi_count, _))) =
        (measure_leaks_exact(&bin_lo), measure_leaks_exact(&bin_hi))
    else {
        eprintln!("skip: var-overwrite loop leak slope: leaks declined to attach");
        return;
    };

    assert_eq!(
        hi_count,
        lo_count,
        "indirect-enum VAR-OVERWRITE loop leaks {hi_count} nodes at {hi_iters} iterations vs \
         {lo_count} at {lo_iters} — a NON-ZERO per-iteration slope ({delta} extra nodes over \
         {span} iterations). A single `var t` is reassigned `Node(Leaf(1), Leaf(2))` each \
         iteration; codegen must release the prior node at the binding overwrite \
         (`Instr::Move {{ dest: Local(owner) }}`). A growing slope means the overwrite-release \
         did not fire — the binding was absent from `indirect_enum_owned_locals`, or \
         construction-site allocation orphaned the prior node.",
        delta = hi_count.saturating_sub(lo_count),
        span = hi_iters - lo_iters,
    );

    assert_eq!(
        hi_count, 0,
        "indirect-enum var-overwrite loop has a flat but NON-ZERO leak count ({hi_count}); the \
         slope is 0 yet a constant set of nodes leaks every run.",
    );

    eprintln!(
        "var_overwrite_loop: leak slope 0 ({lo_count}@{lo_iters} == {hi_count}@{hi_iters}), \
         exact 0 — PASS"
    );
}

/// #46 regression guard — the var-overwrite loop runs clean under the poisoned
/// allocator. The overwrite-release frees the prior node exactly once; a stale
/// per-iteration reuse (double-free) or a wild free of an uninitialised slot (the
/// first reassignment, before the prologue null-init) aborts under `MallocScribble`.
#[test]
fn indirect_enum_var_overwrite_loop_no_corruption_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "var_overwrite_loop_df",
        &var_overwrite_loop_source(50),
        "ok",
    );
}

/// #46 regression guard — the var-overwrite loop is exact-zero-leak at a
/// representative iteration count. Complements the slope oracle: slope-0 proves no
/// per-iteration leak; this proves the seed `Leaf(0)` and final node are reclaimed
/// too (no constant leak).
#[test]
fn indirect_enum_var_overwrite_loop_zero_leaks_exact() {
    assert_zero_leaks_exact("var_overwrite_loop", &var_overwrite_loop_source(50));
}
