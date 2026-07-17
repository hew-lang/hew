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
//! - **Per-iteration leak slope** (macOS-only via `leaks(1)`): a single leaf, a
//!   depth-2 balanced tree, and a mutually-recursive `A` ↔ `B` chain are each
//!   built-and-consumed in a loop (the construct-consume cycle wrapped in a helper
//!   fn so the stack slot is dead at exit), compiled at a LOW and a HIGH iteration
//!   count, and the leak NODE counts differenced via the shared
//!   [`support::leak_slope`] harness. The slope must stay flat
//!   (`high <= low + tolerance`); a regression that drops the free arm (or the
//!   prologue gate) leaks ≥1 node per iteration and shows a positive slope. The
//!   delta cancels the constant `leaks --atExit` baseline noise a single-shot
//!   exact-zero count cannot, so the gate is deterministic.
//!
//! - **No double-free under the poisoned-allocator triple** (any unix): the
//!   double-free landmine control — a NAMED child node nested into a parent
//!   (`let left = Node(..); let t = Node(left, ..)`). The parent's recursive
//!   free reclaims `left`'s subtree; if `left` ALSO fired its own free this
//!   aborts under `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges`. The
//!   correct value output plus the clean exit pin both directions.
//!
//! - **Build-and-consume loop, flat slope** (#46): a recursive `indirect enum`
//!   constructed AND consumed every iteration. The leak count must not grow with
//!   the iteration count (a positive slope is a per-iteration leak from a
//!   construction binding the drop allow-set wrongly excluded), and the loop must
//!   run clean under the poisoned allocator (no use-after-free of a re-entered
//!   node). Pins the lazy construction-site allocation and the inline-match drop
//!   admission together.
//!
//! - **Var-overwrite loop, flat slope** (#46 regression guard): a SINGLE `var t`
//!   reassigned a fresh `Node(Leaf, Leaf)` every iteration. With lazy
//!   construction-site allocation each right-hand side mints a fresh node, so the
//!   prior node `t` held must be freed at the binding overwrite
//!   (`Instr::Move { dest: Local(owner) }` → `emit_indirect_enum_overwrite_release`,
//!   gated on the MIR-admitted owner set). This shape regressed to ~3 leaks/iter
//!   when construction-site allocation first replaced the eager entry-prologue
//!   (whose single reused node masked the missing release); the slope oracle holds
//!   it flat across iteration counts and the scribble pin runs it clean under the
//!   poisoned allocator (the first reassignment must read the null-initialised
//!   slot, not free uninitialised bytes; later ones must not double-free).
//!
//! ## Fixture-authoring rule (non-vacuous)
//!
//! Every fixture provably heap-allocates: an `indirect enum` value is ALWAYS a
//! `hew_alloc`'d node (there is no inline representation), so a per-iteration leak
//! would always register as a positive slope — the assertions are never vacuous
//! over a non-heap literal. The pre-fix per-cycle leak counts above (19 / 3) are
//! the non-zero baseline these oracles ratchet flat.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

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
fn loop_build_consume_source(iters: usize) -> String {
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
fn var_overwrite_loop_source(iters: usize) -> String {
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

/// Single-leaf build-and-consume loop. Each iteration constructs `Leaf(5)`,
/// borrow-passes it to `val` (a by-value param frees nothing), and lets the
/// sole-owner binding `t` free it at scope exit. `run_loop` returns `5 * iters`,
/// self-checked in `main`, so the loop is never dead-code-eliminated and the
/// binding slot is dead at process exit. Pre-fix each cycle leaked 3 nodes;
/// post-fix the per-iteration slope is flat.
fn single_leaf_loop_source(iters: usize) -> String {
    let expected_total = iters * 5; // val(Leaf(5)) == 5
    format!(
        "indirect enum Tree {{ Leaf(i64); Node(Tree, Tree); }}\n\
         fn val(t: Tree) -> i64 {{ match t {{ Leaf(n) => n, Node(l, r) => val(l) + val(r), }} }}\n\
         fn run_loop() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   for i in 0..{iters} {{\n\
         \x20       let t = Leaf(5);\n\
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

/// Depth-2 balanced-tree build-and-consume loop. Each iteration constructs
/// `Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4)))` (seven heap nodes) and
/// consumes it via the recursive `val`; the sole-owner root's recursive free must
/// reclaim every interior + leaf node each iteration. `run_loop` returns
/// `10 * iters`, self-checked. Pre-fix each cycle leaked 19 nodes; post-fix the
/// slope is flat (per-node free + recursion across depth).
fn deep_tree_loop_source(iters: usize) -> String {
    let expected_total = iters * 10; // val(Node(Node(1,2),Node(3,4))) == 1+2+3+4 == 10
    format!(
        "indirect enum Tree {{ Leaf(i64); Node(Tree, Tree); }}\n\
         fn val(t: Tree) -> i64 {{ match t {{ Leaf(n) => n, Node(l, r) => val(l) + val(r), }} }}\n\
         fn run_loop() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   for i in 0..{iters} {{\n\
         \x20       let t = Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4)));\n\
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

/// Mutually-recursive `A` ↔ `B` build-and-consume loop. Each iteration constructs
/// `AWrap(BWrap(AWrap(BEnd(7))))` and consumes it via `depthA`, whose cross-thunk
/// recursive free walks the whole chain through the partner thunk. `run_loop`
/// returns `10 * iters`, self-checked. Post-fix the slope is flat (the cross-thunk
/// synthesis frees the chain and does not loop forever).
fn mutual_loop_source(iters: usize) -> String {
    let expected_total = iters * 10; // depthA(AWrap(BWrap(AWrap(BEnd(7))))) == 7 + 1 + 1 + 1 == 10
    format!(
        "indirect enum A {{ AEnd(i64); AWrap(B); }}\n\
         indirect enum B {{ BEnd(i64); BWrap(A); }}\n\
         fn depthA(a: A) -> i64 {{ match a {{ AEnd(n) => n, AWrap(b) => depthB(b) + 1, }} }}\n\
         fn depthB(b: B) -> i64 {{ match b {{ BEnd(n) => n, BWrap(a) => depthA(a) + 1, }} }}\n\
         fn run_loop() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   for i in 0..{iters} {{\n\
         \x20       let v = AWrap(BWrap(AWrap(BEnd(7))));\n\
         \x20       total = total + depthA(v);\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() {{\n\
         \x20   let r = run_loop();\n\
         \x20   if r == {expected_total} {{ print(\"ok\"); }} else {{ print(\"BAD\"); }}\n\
         }}\n"
    )
}

/// Build-and-consume LOOP through an INLINE enum wrapping an indirect child
/// (F4 / #2208), parameterised on the iteration count.
///
/// `Wrap` is an INLINE enum (`enum Wrap`, no heap node of its own) whose payload
/// owns a `Tree` — an `indirect enum` (a heap pointer). `Tree::Node(Wrap)` embeds
/// the inline `Wrap` struct in its heap node. Freeing the outer `Tree::Node`
/// therefore has to run `Wrap`'s in-place drop so the `Tree` nested inside `Wrap`
/// is reclaimed through `__hew_indirect_enum_free_Tree`. Pre-fix the indirect
/// free thunk recursed only into DIRECT indirect-enum payload fields and skipped
/// the inline-enum field entirely, orphaning the inner `Tree` node every
/// iteration (a positive leak slope). Each iteration constructs
/// `Node(W(Leaf(3)))` — an outer `Node` node plus an inner `Leaf` node — and
/// consumes it via the borrow-taking `val`, so the sole-owner binding `t`'s
/// scope-exit `DropKind::IndirectEnum` free must reclaim BOTH nodes. `run_loop`
/// returns `3 * iters`, self-checked in `main`, so the loop is never
/// dead-code-eliminated and the binding slot is dead at process exit.
fn inline_enum_wrapped_indirect_loop_source(iters: usize) -> String {
    let expected_total = iters * 3; // val(Node(W(Leaf(3)))) == 3
    format!(
        "enum Wrap {{ W(Tree); }}\n\
         indirect enum Tree {{ Leaf(i64); Node(Wrap); }}\n\
         fn val(t: Tree) -> i64 {{\n\
         \x20   match t {{\n\
         \x20       Leaf(n) => n,\n\
         \x20       Node(w) => match w {{ W(inner) => val(inner), }},\n\
         \x20   }}\n\
         }}\n\
         fn run_loop() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   for i in 0..{iters} {{\n\
         \x20       let t = Node(W(Leaf(3)));\n\
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

/// Compile `source`, run under the poisoned-allocator triple, assert clean exit
/// + exact stdout (the double-free / use-after-free pin).
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("indirect-enum-drop-df-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);

    let output = run_under_malloc_scribble(&bin);

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

// __NEW_ORACLES__

/// Single-leaf node, looped: constructed-and-borrow-passed, freed exactly once at
/// scope exit each iteration — flat leak slope. Pre-fix each cycle leaked 3 nodes.
#[test]
fn indirect_enum_single_leaf_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("single_leaf", single_leaf_loop_source);
}

/// Depth-2 tree, looped: the recursive free reclaims every interior + leaf node
/// each iteration — flat leak slope. Pre-fix each cycle leaked 19 nodes.
#[test]
fn indirect_enum_deep_tree_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("deep_tree", deep_tree_loop_source);
}

/// Mutually-recursive `A` ↔ `B`, looped: the cross-thunk recursive free frees the
/// whole chain every iteration — flat leak slope (and the synthesis does not loop
/// forever).
#[test]
fn indirect_enum_mutual_recursion_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("mutual", mutual_loop_source);
}

/// Double-free landmine control: a named child nested into a parent must be freed
/// by the parent's recursive free EXACTLY once. A regression that also admits the
/// child's own free aborts under the poisoned allocator.
#[test]
fn indirect_enum_named_child_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "named_child_df_control",
        NAMED_CHILD_DF_CONTROL_SOURCE,
        NAMED_CHILD_DF_CONTROL_EXPECTED,
    );
}

/// #46 — the headline build-and-consume LOOP oracle. A recursive `indirect enum`
/// constructed AND consumed every iteration must free its nodes per iteration —
/// the leak count must not grow with the iteration count (flat slope). A growing
/// slope means the construction binding `t` was excluded from the drop allow-set
/// (`derive_indirect_enum_drop_allowed`), orphaning each iteration's three nodes.
#[test]
fn indirect_enum_loop_build_consume_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("loop_build_consume", loop_build_consume_source);
}

/// #46 — the build-and-consume loop must run clean under the poisoned allocator:
/// no use-after-free of a re-entered node, no double-free. Pre-fix the entry-block
/// prologue allocated the node ONCE; the second iteration's tag store wrote
/// through the pointer the first iteration's scope-exit free had already
/// reclaimed. The `ok` output (total `3 * 50 == 150`) plus the clean exit pin both
/// directions.
#[test]
fn indirect_enum_loop_build_consume_no_corruption_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "loop_build_consume_df",
        &loop_build_consume_source(50),
        "ok",
    );
}

/// #46 regression guard — the VAR-OVERWRITE build-and-consume loop has a flat
/// per-iteration leak slope. A single `var t` is reassigned each iteration; the
/// prior node must be released at the binding overwrite. A growing slope means the
/// overwrite-release did not fire (the owner-set gate missed the binding, or
/// construction-site allocation orphaned the prior node).
#[test]
fn indirect_enum_var_overwrite_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("var_overwrite_loop", var_overwrite_loop_source);
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

/// F4 / #2208 — an inline enum wrapping an indirect-enum child, looped: freeing
/// the outer indirect node must run the inline `Wrap`'s in-place drop so the
/// `Tree` nested inside it is reclaimed through `__hew_indirect_enum_free_Tree`.
/// Pre-fix the indirect free thunk recursed only into DIRECT indirect-enum
/// payload fields and skipped the inline-enum field, orphaning the inner `Tree`
/// node every iteration — a positive leak slope of one node/iteration. Post-fix
/// the shared payload-teardown drops every owned field, so the slope is flat.
#[test]
fn indirect_enum_inline_wrapped_child_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "inline_wrapped_indirect",
        inline_enum_wrapped_indirect_loop_source,
    );
}

/// F4 / #2208 — the inline-enum-wrapped indirect-child loop runs clean under the
/// poisoned allocator: the inner `Tree` node is freed exactly once through the
/// inline `Wrap`'s in-place drop. An over-eager free (the inner node reclaimed by
/// both `Wrap`'s drop and a spurious second path) aborts under `MallocScribble`;
/// a missed free reads scribbled bytes. The `ok` output (total `3 * 50 == 150`)
/// plus the clean exit pin both directions.
#[test]
fn indirect_enum_inline_wrapped_child_no_corruption_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "inline_wrapped_indirect_df",
        &inline_enum_wrapped_indirect_loop_source(50),
        "ok",
    );
}
