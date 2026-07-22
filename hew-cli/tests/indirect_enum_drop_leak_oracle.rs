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
//! - **Queued actor-message teardown**: a supervised actor acknowledges entry
//!   into a parked handler before the fixture queues direct and packed
//!   indirect-enum tells behind it. Stopping the supervisor therefore destroys
//!   those messages from the mailbox without dispatching them. The macOS leak
//!   slope proves that the registered message destructor recursively reclaims
//!   every node, while the poisoned allocator catches double-free and
//!   use-after-free failures on the same teardown path.
//!
//! - **Local actor-request carriers**: every iteration sends fresh recursive
//!   trees through blocking ask, suspending ask, blocking and suspending select,
//!   and both join branches. The exact aggregate plus a flat leak slope proves
//!   each uniquely typed packed carrier transfers and releases its pointer field;
//!   the poisoned allocator rejects an over-release on any producer shape.
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
    assert_frame_slope_below_tolerance, compile_to_native, leaks_supported, measure_leaks,
    run_under_malloc_scribble, HIGH_FRAMES, LOW_FRAMES, SLOPE_TOLERANCE,
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

/// Build-and-consume LOOP where a value RECORD owns the indirect child through
/// an inline-enum field (F4 / #2208 record-container path), parameterised on the
/// iteration count.
///
/// `Holder { tag: i64, wrap: Wrap }` is a value record whose `wrap` field is the
/// INLINE enum `Wrap`, whose `W` variant owns a `Tree` — an `indirect enum` (a
/// heap pointer). The record is dropped IN SCOPE each iteration (only the
/// `BitCopy` `tag` is read out, so the sole-owner binding `h` retains `wrap` and
/// its scope-exit `DropKind` reclaims the whole record). The record's synthesised
/// `__hew_record_drop_inplace_Holder` must run `Wrap`'s in-place drop, which in
/// turn frees the nested `Tree` through `__hew_indirect_enum_free_Tree`. This is
/// the RECORD leg of the propagation the inline-enum-wrapped oracle covers for
/// the indirect-enum-payload leg: the record drop body reaches an indirect child
/// only via the one witness-aware `emit_field_drop_step`. Pre-fix the record
/// drop path routed its enum field witness-blind and did not synthesise the
/// nested child's teardown on demand, orphaning the inner `Tree`'s three nodes
/// every iteration (a positive leak slope). `run_loop` returns `3 * iters`,
/// self-checked, so the loop is never dead-code-eliminated and the binding slot
/// is dead at process exit.
fn record_of_inline_enum_loop_source(iters: usize) -> String {
    let expected_total = iters * 3; // h.tag == 3
    format!(
        "enum Wrap {{ W(Tree); }}\n\
         indirect enum Tree {{ Leaf(i64); Node(Tree, Tree); }}\n\
         type Holder {{ tag: i64, wrap: Wrap }}\n\
         fn run_loop() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   for i in 0..{iters} {{\n\
         \x20       let h = Holder {{ tag: 3, wrap: W(Node(Leaf(1), Leaf(2))) }};\n\
         \x20       total = total + h.tag;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() {{\n\
         \x20   let r = run_loop();\n\
         \x20   if r == {expected_total} {{ print(\"ok\"); }} else {{ print(\"BAD\"); }}\n\
         }}\n"
    )
}

/// Build-and-consume LOOP where a TUPLE owns the indirect child through an
/// inline-enum element (F4 / #2208 tuple-container path), parameterised on the
/// iteration count.
///
/// `(Wrap, i64)` is a value tuple whose first element is the INLINE enum `Wrap`
/// owning a `Tree` (`indirect enum`, a heap pointer). The tuple is dropped IN
/// SCOPE each iteration (only the `BitCopy` `.1` element is read, so the tuple
/// binding `pair` retains the `Wrap` and its scope-exit drop reclaims it). The
/// synthesised `__hew_tuple_drop_inplace_tuple_Wrap_i64` must run `Wrap`'s
/// in-place drop, which frees the nested `Tree` through
/// `__hew_indirect_enum_free_Tree`. Pre-fix the tuple drop body — like the
/// record body — routed its enum element witness-blind, orphaning the inner
/// `Tree` every iteration. `run_loop` returns `3 * iters`, self-checked.
fn tuple_of_inline_enum_loop_source(iters: usize) -> String {
    let expected_total = iters * 3; // pair.1 == 3
    format!(
        "enum Wrap {{ W(Tree); }}\n\
         indirect enum Tree {{ Leaf(i64); Node(Tree, Tree); }}\n\
         fn run_loop() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   for i in 0..{iters} {{\n\
         \x20       let pair = (W(Node(Leaf(1), Leaf(2))), 3);\n\
         \x20       total = total + pair.1;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() {{\n\
         \x20   let r = run_loop();\n\
         \x20   if r == {expected_total} {{ print(\"ok\"); }} else {{ print(\"BAD\"); }}\n\
         }}\n"
    )
}

/// Queue direct and packed indirect-enum messages behind a handler that is
/// known to be parked, then stop the supervising actor tree. Receiving the
/// ready signal proves `hold` has started before any payload tell is sent, and
/// its long sleep keeps every payload queued until `supervisor_stop` destroys
/// the mailbox. Each iteration contributes two depth-2 trees (14 heap nodes),
/// so a missing message destructor produces a steep leak slope.
fn actor_mailbox_teardown_source(frames: usize) -> String {
    format!(
        "import std::channel::channel;\n\
         indirect enum Tree {{ Leaf(i64); Node(Tree, Tree); }}\n\
         fn sum(t: Tree) -> i64 {{ match t {{ Leaf(n) => n, Node(l, r) => sum(l) + sum(r), }} }}\n\
         actor Sink {{\n\
         \x20   receive fn hold(ready: channel.Sender<i64>) {{\n\
         \x20       ready.send(1);\n\
         \x20       sleep(10s);\n\
         \x20   }}\n\
         \x20   receive fn take(t: Tree) {{ let _ = sum(t); }}\n\
         \x20   receive fn tagged(tag: i64, t: Tree) {{ let _ = tag + sum(t); }}\n\
         }}\n\
         supervisor App {{\n\
         \x20   strategy: one_for_one;\n\
         \x20   intensity: 3 within 60s;\n\
         \x20   child sink: Sink;\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let sup = spawn App;\n\
         \x20   let sink = sup.sink;\n\
         \x20   let (ready_tx, ready_rx): (channel.Sender<i64>, channel.Receiver<i64>) = channel.new(1);\n\
         \x20   sink.hold(ready_tx);\n\
         \x20   let started = match await ready_rx.recv() {{ Some(n) => n, None => 0, }};\n\
         \x20   if started != 1 {{ print(\"BAD\"); return 1; }}\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       sink.take(Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4))));\n\
         \x20       sink.tagged(i, Node(Node(Leaf(5), Leaf(6)), Node(Leaf(7), Leaf(8))));\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   supervisor_stop(sup);\n\
         \x20   print(\"ok\");\n\
         \x20   0\n\
         }}\n"
    )
}

/// Exercise every local multi-argument actor-request carrier with fresh trees.
/// Each frame crosses ordinary Ask, `SuspendKind::Ask`, Select `ActorAsk`,
/// `SuspendingSelect` `ActorAsk`, and two Join branches. The six trees contribute
/// eighteen heap nodes per frame, so an inline-layout carrier or missed release
/// produces a steep leak slope. The exact total makes every request observable.
fn actor_request_carrier_source(frames: usize) -> String {
    let expected_total = 3 * frames * frames + 75 * frames;
    format!(
        "indirect enum Tree {{ Leaf(i64); Node(Tree, Tree); }}\n\
         fn sum(t: Tree) -> i64 {{ match t {{ Leaf(n) => n, Node(l, r) => sum(l) + sum(r), }} }}\n\
         actor Scorer {{\n\
         \x20   receive fn score(tag: i64, tree: Tree) -> i64 {{ tag + sum(tree) }}\n\
         }}\n\
         actor Coordinator {{\n\
         \x20   let scorer: LocalPid<Scorer>;\n\
         \x20   receive fn ask_score(tag: i64, tree: Tree) -> i64 {{\n\
         \x20       match await scorer.score(tag, tree) {{ Ok(value) => value, Err(_) => -1, }}\n\
         \x20   }}\n\
         \x20   receive fn select_score(tag: i64, tree: Tree) -> i64 {{\n\
         \x20       select {{ reply from scorer.score(tag, tree) => reply, after 5s => -2, }}\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let scorer = spawn Scorer;\n\
         \x20   let coordinator = spawn Coordinator(scorer: scorer);\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let direct = match await scorer.score(i, Node(Leaf(1), Leaf(2))) {{ Ok(value) => value, Err(_) => -3, }};\n\
         \x20       let selected = select {{ reply from scorer.score(i, Node(Leaf(3), Leaf(4))) => reply, after 5s => -4, }};\n\
         \x20       let (joined_a, joined_b) = join {{\n\
         \x20           scorer.score(i, Node(Leaf(5), Leaf(6))),\n\
         \x20           scorer.score(i, Node(Leaf(7), Leaf(8))),\n\
         \x20       }};\n\
         \x20       let suspended_ask = match await coordinator.ask_score(i, Node(Leaf(9), Leaf(10))) {{ Ok(value) => value, Err(_) => -5, }};\n\
         \x20       let suspended_select = match await coordinator.select_score(i, Node(Leaf(11), Leaf(12))) {{ Ok(value) => value, Err(_) => -6, }};\n\
         \x20       total = total + direct + selected + joined_a + joined_b + suspended_ask + suspended_select;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total == {expected_total} {{ print(\"ok\"); }} else {{ print(\"BAD\"); }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Shape-identical scalar control for [`actor_request_carrier_source`]. Actor,
/// ask/select/join, reply-channel, and scheduler allocations are unchanged; only
/// each recursive three-node Tree payload becomes one `i64`. Comparing slopes
/// isolates payload ownership from unrelated request-runtime allocations.
fn actor_request_carrier_scalar_source(frames: usize) -> String {
    let expected_total = 3 * frames * frames + 75 * frames;
    format!(
        "actor Scorer {{\n\
         \x20   receive fn score(tag: i64, value: i64) -> i64 {{ tag + value }}\n\
         }}\n\
         actor Coordinator {{\n\
         \x20   let scorer: LocalPid<Scorer>;\n\
         \x20   receive fn ask_score(tag: i64, value: i64) -> i64 {{\n\
         \x20       match await scorer.score(tag, value) {{ Ok(result) => result, Err(_) => -1, }}\n\
         \x20   }}\n\
         \x20   receive fn select_score(tag: i64, value: i64) -> i64 {{\n\
         \x20       select {{ reply from scorer.score(tag, value) => reply, after 5s => -2, }}\n\
         \x20   }}\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let scorer = spawn Scorer;\n\
         \x20   let coordinator = spawn Coordinator(scorer: scorer);\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let direct = match await scorer.score(i, 3) {{ Ok(value) => value, Err(_) => -3, }};\n\
         \x20       let selected = select {{ reply from scorer.score(i, 7) => reply, after 5s => -4, }};\n\
         \x20       let (joined_a, joined_b) = join {{ scorer.score(i, 11), scorer.score(i, 15), }};\n\
         \x20       let suspended_ask = match await coordinator.ask_score(i, 19) {{ Ok(value) => value, Err(_) => -5, }};\n\
         \x20       let suspended_select = match await coordinator.select_score(i, 23) {{ Ok(value) => value, Err(_) => -6, }};\n\
         \x20       total = total + direct + selected + joined_a + joined_b + suspended_ask + suspended_select;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total == {expected_total} {{ print(\"ok\"); }} else {{ print(\"BAD\"); }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Compare recursive-payload and scalar-control leak slopes for the exact same
/// request topology. The recursive shape may add only constant measurement
/// noise; a per-frame excess means one or more three-node Tree payloads escaped.
fn assert_actor_request_payload_slope_matches_scalar() {
    let shape = "indirect_enum_actor_request_carriers";
    if !leaks_supported(shape) {
        return;
    }
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("leak-slope-indirect-enum-actor-requests-")
        .tempdir()
        .expect("tempdir");
    let recursive_low = compile_to_native(
        &actor_request_carrier_source(LOW_FRAMES),
        dir.path(),
        "request_recursive_low",
    );
    let recursive_high = compile_to_native(
        &actor_request_carrier_source(HIGH_FRAMES),
        dir.path(),
        "request_recursive_high",
    );
    let scalar_low = compile_to_native(
        &actor_request_carrier_scalar_source(LOW_FRAMES),
        dir.path(),
        "request_scalar_low",
    );
    let scalar_high = compile_to_native(
        &actor_request_carrier_scalar_source(HIGH_FRAMES),
        dir.path(),
        "request_scalar_high",
    );

    let (Some(recursive_low), Some(recursive_high), Some(scalar_low), Some(scalar_high)) = (
        measure_leaks(&recursive_low),
        measure_leaks(&recursive_high),
        measure_leaks(&scalar_low),
        measure_leaks(&scalar_high),
    ) else {
        return;
    };
    let recursive_slope = recursive_high.saturating_sub(recursive_low);
    let scalar_slope = scalar_high.saturating_sub(scalar_low);
    eprintln!(
        "{shape}: recursive={recursive_low}->{recursive_high} slope={recursive_slope}; \
         scalar={scalar_low}->{scalar_high} slope={scalar_slope}; tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        recursive_slope <= scalar_slope + SLOPE_TOLERANCE,
        "{shape}: recursive payloads add a leak slope beyond the shape-identical scalar \
         request baseline — recursive={recursive_low}->{recursive_high} ({recursive_slope}), \
         scalar={scalar_low}->{scalar_high} ({scalar_slope}), tolerance={SLOPE_TOLERANCE}"
    );
}

/// F4 / #2208 — actor ASK-REPLY of an `indirect enum`, the ABI-boundary leg.
///
/// An `indirect enum` return value is ABI-lowered to a bare heap-node POINTER
/// (`declare_function`'s `resolve_value_ty` override), so an actor handler
/// returning one deposits the node pointer into the reply channel buffer — NOT
/// the inline `{ tag, payload }` tagged union. The channel's registered reply
/// destructor (`#1739`) reaps a reply that is deposited but never consumed
/// (timeout / cancel / shutdown race). Before the fix that destructor was the
/// inline `__hew_enum_drop_inplace_<E>` helper, which reads the pointer bits as
/// a discriminant tag and frees nothing — the node and its subtree leak. The
/// fix routes the destructor through the same slot-type authority the per-field
/// path uses, so a pointer-backed reply frees the subtree through the recursive
/// `__hew_indirect_enum_free_<E>` thunk.
///
/// The ask is a PLAIN `await` inside an actor handler (a suspending context):
/// that is the `SuspendingAsk` lowering that wires the reply destructor
/// (`wire_reply_drop_fn`). A blocking `await` in `main` uses the direct
/// `hew_actor_ask` path and wires no destructor.
const ASK_REPLY_INDIRECT_ENUM_SOURCE: &str = "\
indirect enum Tree { Leaf(i64); Node(Tree, Tree); }\n\
fn val(t: Tree) -> i64 { match t { Leaf(n) => n, Node(l, r) => val(l) + val(r), } }\n\
actor SlowReplier {\n\
\x20   receive fn fetch() -> Tree { Node(Leaf(1), Leaf(2)) }\n\
}\n\
actor Driver {\n\
\x20   var slow: LocalPid<SlowReplier>;\n\
\x20   receive fn run() -> i64 {\n\
\x20       match await slow.fetch() {\n\
\x20           Ok(t) => { val(t) }\n\
\x20           Err(e) => { let _ = e; 0 }\n\
\x20       }\n\
\x20   }\n\
}\n\
fn main() -> i64 {\n\
\x20   let slow = spawn SlowReplier;\n\
\x20   let driver = spawn Driver(slow: slow);\n\
\x20   match await driver.run() {\n\
\x20       Ok(v) => { print(\"${v}\"); }\n\
\x20       Err(e) => { let _ = e; }\n\
\x20   }\n\
\x20   0\n\
}\n";

/// Slice the body text of the LLVM function `@<name>(` from `ir` — from its
/// `define ... @<name>(` line through the matching closing `}` at column 0.
/// Returns `None` when the function is not defined in `ir`.
fn llvm_fn_body<'a>(ir: &'a str, name: &str) -> Option<&'a str> {
    let needle = format!("@{name}(");
    let define_at = ir
        .match_indices("define ")
        .find(|(idx, _)| {
            ir[*idx..]
                .lines()
                .next()
                .is_some_and(|l| l.contains(&needle))
        })
        .map(|(idx, _)| idx)?;
    let rest = &ir[define_at..];
    let end = rest.find("\n}\n").map(|e| define_at + e + 3)?;
    Some(&ir[define_at..end])
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

/// F4 / #2208 — a value RECORD owning the indirect child through an inline-enum
/// field, dropped in scope each iteration: the record's
/// `__hew_record_drop_inplace_Holder` must run the `Wrap` field's in-place drop,
/// which frees the nested `Tree` through `__hew_indirect_enum_free_Tree` — the
/// record leg of the container propagation. Pre-fix this shape did not compile
/// (the overwrite-release capacity walk overflowed the cyclic-graph depth guard
/// on the self-recursive `Tree`); with that unblocked but the drop routing
/// reverted, the record drop routes the inline-enum field's heap-node child to
/// the inline helper and traps (misreads the node pointer as a tag). Post-fix
/// the slope is flat.
#[test]
fn indirect_enum_record_of_inline_enum_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("record_of_inline_enum", record_of_inline_enum_loop_source);
}

/// F4 / #2208 — the record-of-inline-enum loop runs clean under the poisoned
/// allocator: the nested `Tree` node is freed exactly once through the record →
/// inline-enum → `__hew_indirect_enum_free_Tree` chain. The `ok` output
/// (total `3 * 50 == 150`) plus the clean exit pin both a leak and a double-free.
#[test]
fn indirect_enum_record_of_inline_enum_no_corruption_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "record_of_inline_enum_df",
        &record_of_inline_enum_loop_source(50),
        "ok",
    );
}

/// F4 / #2208 — a value TUPLE owning the indirect child through an inline-enum
/// element, dropped in scope each iteration: the synthesised
/// `__hew_tuple_drop_inplace_tuple_Wrap_i64` must run the `Wrap` element's
/// in-place drop, freeing the nested `Tree` through `__hew_indirect_enum_free_Tree`
/// — the tuple leg of the container propagation. Same pre-fix story as the record
/// leg (fail-closed depth guard, then a trap with routing reverted); post-fix the
/// slope is flat.
#[test]
fn indirect_enum_tuple_of_inline_enum_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("tuple_of_inline_enum", tuple_of_inline_enum_loop_source);
}

/// F4 / #2208 — the tuple-of-inline-enum loop runs clean under the poisoned
/// allocator: the nested `Tree` node is freed exactly once through the tuple →
/// inline-enum → `__hew_indirect_enum_free_Tree` chain. The `ok` output
/// (total `3 * 50 == 150`) plus the clean exit pin both a leak and a double-free.
#[test]
fn indirect_enum_tuple_of_inline_enum_no_corruption_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "tuple_of_inline_enum_df",
        &tuple_of_inline_enum_loop_source(50),
        "ok",
    );
}

/// Direct and packed indirect-enum tells queued behind a parked handler are
/// destroyed from the mailbox when the supervisor stops. The leak count must
/// remain flat as the number of queued recursive payloads grows; a missing or
/// inline-layout message destructor leaks at least fourteen nodes per frame.
#[test]
fn indirect_enum_actor_mailbox_teardown_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "indirect_enum_actor_mailbox_teardown",
        actor_mailbox_teardown_source,
    );
}

/// The same queued-message teardown runs clean under the poisoned allocator.
/// The exact sentinel proves `supervisor_stop` completed after draining the
/// mailbox; an ABI mismatch, recursive double-free, or poisoned read aborts
/// before it can print `ok`.
#[test]
fn indirect_enum_actor_mailbox_teardown_no_corruption_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "indirect_enum_actor_mailbox_teardown_df",
        &actor_mailbox_teardown_source(50),
        "ok",
    );
}

/// Every local actor-request producer consumes and releases its recursive Tree
/// payload once per frame. A missed packed pointer field grows by at least three
/// nodes per affected request and cannot hide inside the constant runtime floor.
#[test]
fn indirect_enum_actor_request_carriers_leak_slope_below_tolerance() {
    assert_actor_request_payload_slope_matches_scalar();
}

/// The same request matrix runs under poisoned allocation. An ABI mismatch,
/// double-free, or use-after-free aborts before the exact aggregate prints `ok`.
#[test]
fn indirect_enum_actor_request_carriers_no_corruption_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "indirect_enum_actor_request_carriers_df",
        &actor_request_carrier_source(10),
        "ok",
    );
}

/// F4 / #2208 — the actor ask-reply ABI-boundary leg, pinned at the IR level.
///
/// ## Why an IR assertion here, not a `leaks --atExit` slope
///
/// The registered reply destructor fires at RUNTIME only on a reply that is
/// deposited-but-never-consumed — the cancel / timeout / select-loser teardown
/// legs. Every source construct that abandons a reply that way (`await … |
/// after d`, `select { reply from … }`) currently FAILS TO COMPILE for an
/// `indirect enum` reply: the suspending deadline/select consume path moves the
/// pointer-lowered reply into an inline-`{ tag, payload }`-typed match/result
/// binder and fails closed (`Move type mismatch: src=ptr dest=%Tree`,
/// `hew-codegen-rs/src/llvm.rs`). That is a SEPARATE consume-side ABI defect —
/// the reply DESTRUCTOR routing this oracle pins is independent of it — so a
/// runtime per-iteration leak slope for the abandoned-reply leg is not
/// expressible until that consume path is ABI-consistent. The runtime firing of
/// the channel destructor itself is already proven for a heap reply by
/// `ask_reply_owned_leak_oracle` (the owned-`string` cancel leg); this oracle
/// pins the remaining unknown — that a pointer-backed `indirect enum` reply is
/// routed through the recursive node free, not the inline helper.
///
/// ## Teeth (fail-without-fix)
///
/// The reply channel must register `__hew_reply_drop_indirect_Tree` (which loads
/// the node pointer and calls the recursive `__hew_indirect_enum_free_Tree`).
/// Reverting the routing re-registers the inline `__hew_enum_drop_inplace_Tree`
/// on the pointer buffer — it reads the node pointer's bits as a tag and frees
/// nothing — flipping both the registered-symbol assertion and the
/// recursive-free-in-thunk assertion. Platform-independent (reads emitted IR,
/// no `leaks(1)`), so it holds the invariant on every codegen target.
#[test]
fn indirect_enum_ask_reply_drop_routes_through_recursive_free() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("indirect-enum-ask-reply-")
        .tempdir()
        .expect("tempdir");
    let _bin = compile_to_native(
        ASK_REPLY_INDIRECT_ENUM_SOURCE,
        dir.path(),
        "ask_reply_indirect",
    );
    let ir = std::fs::read_to_string(dir.path().join("ask_reply_indirect.ll"))
        .expect("read emitted LLVM IR for the indirect-enum ask-reply fixture");

    // Every reply-destructor registration for this fixture must name the
    // indirect-aware wrapper; none may name the inline in-place helper.
    let registrations: Vec<&str> = ir
        .lines()
        .filter(|l| l.contains("call void @hew_reply_channel_set_reply_drop_fn("))
        .collect();
    assert!(
        !registrations.is_empty(),
        "expected the SuspendingAsk lowering to register a reply destructor \
         (`hew_reply_channel_set_reply_drop_fn`) for the pointer-backed indirect-enum reply; \
         found none — the ask no longer wires a destructor, so a never-consumed reply leaks its \
         heap node unconditionally.\n--- IR ---\n{ir}"
    );
    assert!(
        registrations
            .iter()
            .all(|l| l.contains("@__hew_reply_drop_indirect_Tree")),
        "the indirect-enum reply destructor must be `__hew_reply_drop_indirect_Tree` (loads the \
         node pointer, frees the subtree via `__hew_indirect_enum_free_Tree`). A registration \
         naming `__hew_enum_drop_inplace_Tree` runs the inline `{{ tag, payload }}` helper over a \
         buffer that holds only a heap-node pointer — it misreads the pointer as a tag and frees \
         nothing, leaking the node (#2208 F4 ask-reply boundary). Registrations found:\n{}",
        registrations.join("\n")
    );

    // The wrapper body must reach the recursive node free — not the inline helper.
    let thunk = llvm_fn_body(&ir, "__hew_reply_drop_indirect_Tree").unwrap_or_else(|| {
        panic!(
            "reply channel registered `__hew_reply_drop_indirect_Tree` but its body is not defined \
             in the module — a dangling reply destructor.\n--- IR ---\n{ir}"
        )
    });
    assert!(
        thunk.contains("call void @__hew_indirect_enum_free_Tree("),
        "`__hew_reply_drop_indirect_Tree` must free the reply through the recursive \
         `__hew_indirect_enum_free_Tree` node walk; its body does not call it, so the subtree \
         under the reply node leaks:\n{thunk}"
    );
}
