//! Select-arm owned-binding leak oracle (#1875, select-arm half).
//!
//! A `select` arm's value binding (`reply from actor.ask(...)`,
//! `msg from rx.recv()`) entered `binding_locals` but never
//! `register_owned_local`, so it never reached `owned_locals` and
//! `build_lifo_drops` released it on NO exit edge: every selected owned value
//! leaked once per iteration (~1 leak-node/iter, 32 bytes per owned-string
//! reply in the original repro). The fix registers the binding at the shared
//! body-block site (`lower_select`), giving it the same scope-exit drop
//! discipline as a `let`-bound owned local.
//!
//! ## What these oracles pin
//!
//! 1. **Win-path release, flat slope.** An owned-`string` ask reply (and its
//!    channel-recv mirror) selected every iteration with the binding UNUSED —
//!    the headline #1875 shape. Pre-fix: ~1 leak/iter; post-fix: flat.
//! 2. **Timeout-wins path.** An `after` arm that always wins over owned-string
//!    ask/recv losers: loser replies are reaped by the reply channel's
//!    registered destructor (the #1739 single-reaper contract), and the new
//!    scope-exit drop must not fire for a binding whose slot was never
//!    written. Flat slope, no stranded losers.
//! 3. **Loser-leg no-double-free.** The select-loser race (fast winner + slow
//!    owned loser + `after` net) under the poisoned allocator: the scope-exit
//!    drop fires only on the winning arm's body path; stacking on the channel
//!    destructor's loser-leg release aborts here.
//! 4. **Selected-value escape.** `let x = select { r from a.m() => r }` with
//!    `x` read back verbatim — the arm body's move into the select result must
//!    suppress the scope-exit drop (no double-free, poisoned-allocator clean)
//!    while the non-escaping siblings above still drop (no leak).
//!
//! Slope methodology (LOW/HIGH iteration delta, node counts, tolerance) is the
//! shared `support::leak_slope` harness; `leaks(1)` is macOS-only, so the
//! slope probes skip gracefully elsewhere while the scribble pins run on any
//! unix host.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── slope shapes ────────────────────────────────────────────────────────────

/// The #1875 headline repro: an owned-`string` ask reply selected per
/// iteration, binding unused (`=> 1`), worker spawned ONCE so the abandoned
/// reply binding is the only per-iteration heap.
fn ask_unused_binding_loop_source(frames: usize) -> String {
    format!(
        "actor StringWorker {{\n\
         \x20   receive fn get_string(n: i64) -> string {{\n\
         \x20       to_string(n)\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let w = spawn StringWorker;\n\
         \x20   var i: i64 = 0;\n\
         \x20   var total: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let r = select {{\n\
         \x20           _reply from w.get_string(i) => 1,\n\
         \x20       }};\n\
         \x20       total = total + r;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total == {frames} {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Channel-recv mirror of the headline: an owned `string` selected per
/// iteration (`Option<string>` binding), binding unused. The payload is a
/// STATIC literal deliberately: `hew_channel_send_layout` deep-copies the
/// element into the envelope, so the recv'd value is fresh heap owned solely
/// by the arm binding — a heap send argument would contaminate the slope with
/// the pre-existing send-side temp leak (MIR marks `send` consuming, the
/// runtime clones and never frees the original), which reproduces without any
/// `select` and is a separate seam.
fn recv_unused_binding_loop_source(frames: usize) -> String {
    format!(
        "import std::channel::channel;\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(1);\n\
         \x20   var i: i64 = 0;\n\
         \x20   var hits: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       tx.send(\"recv-owned-heap-payload\");\n\
         \x20       let r = select {{\n\
         \x20           _msg from rx.recv() => 1,\n\
         \x20           after 1s => 0,\n\
         \x20       }};\n\
         \x20       hits = hits + r;\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   tx.close();\n\
         \x20   rx.close();\n\
         \x20   if hits == {frames} {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Timeout-wins path: the `after` arm always beats a slow owned-string ask
/// loser and a never-ready recv loser. Loser replies are the reply channel
/// destructor's to reap; the arm bindings' slots are never written, so the
/// scope-exit drop must not fire for them — flat slope, no stranded losers,
/// no drop of an unwritten slot.
fn after_wins_owned_losers_loop_source(frames: usize) -> String {
    format!(
        "import std::channel::channel;\n\
         \n\
         actor SlowReplier {{\n\
         \x20   receive fn fetch() -> string {{\n\
         \x20       sleep(5ms);\n\
         \x20       \"after-wins-loser-reply\".to_upper()\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let slow = spawn SlowReplier;\n\
         \x20   let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(1);\n\
         \x20   var i: i64 = 0;\n\
         \x20   var timeouts: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let r = select {{\n\
         \x20           _reply from slow.fetch() => 1,\n\
         \x20           _msg from rx.recv() => 2,\n\
         \x20           after 1ms => 0,\n\
         \x20       }};\n\
         \x20       if r == 0 {{ timeouts = timeouts + 1; }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   tx.close();\n\
         \x20   rx.close();\n\
         \x20   if timeouts == {frames} {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Selected-value escape as a slope: the winning reply moves out of the arm
/// into the select result and is `let`-bound each iteration — released once
/// per iteration by the standard `let` discipline, not leaked and not
/// double-freed by a stacked scope-exit drop of the arm binding.
fn escape_binding_loop_source(frames: usize) -> String {
    format!(
        "actor Maker {{\n\
         \x20   receive fn make(n: i64) -> string {{\n\
         \x20       to_string(n)\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let m = spawn Maker;\n\
         \x20   var i: i64 = 0;\n\
         \x20   var total: i64 = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let x = select {{\n\
         \x20           r from m.make(i) => r,\n\
         \x20       }};\n\
         \x20       total = total + x.len();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

// ── scribble exactly-once pins ──────────────────────────────────────────────

/// Select-loser race (the `ask_reply_owned_select_loser.hew` design): the fast
/// arm's owned reply wins and ESCAPES via `=> reply`; the slow loser's owned
/// reply is reaped by the channel destructor. Winner printed back verbatim —
/// a use-after-free garbles it; a loser-leg or escape double-free aborts.
const SELECT_LOSER_SCRIBBLE_SOURCE: &str = "\
actor FastWorker {\n\
\x20   receive fn label() -> string {\n\
\x20       \"fast-owned-reply\".to_upper()\n\
\x20   }\n\
}\n\
\n\
actor SlowWorker {\n\
\x20   receive fn label() -> string {\n\
\x20       sleep(50ms);\n\
\x20       \"slow-owned-reply\".to_upper()\n\
\x20   }\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let fast = spawn FastWorker;\n\
\x20   let slow = spawn SlowWorker;\n\
\x20   let winner = select {\n\
\x20       reply from fast.label() => reply,\n\
\x20       reply from slow.label() => reply,\n\
\x20       after 100ms => \"timeout-owned-reply\".to_upper(),\n\
\x20   };\n\
\x20   print(winner);\n\
\x20   0\n\
}\n";

/// Straight-line selected-value escape: the arm body moves the owned reply
/// into the select result; the move must suppress the arm binding's
/// scope-exit drop so the value is released exactly once (as `x`).
const ESCAPE_SCRIBBLE_SOURCE: &str = "\
actor Maker {\n\
\x20   receive fn make() -> string {\n\
\x20       \"escape-owned-reply\".to_upper()\n\
\x20   }\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let m = spawn Maker;\n\
\x20   let x = select {\n\
\x20       r from m.make() => r,\n\
\x20   };\n\
\x20   print(x);\n\
\x20   0\n\
}\n";

/// Straight-line unused owned binding: the scope-exit drop releases the
/// selected reply exactly once on the single Return edge — no leak partner
/// for the slope shapes, pinned here against a double release.
const UNUSED_BINDING_SCRIBBLE_SOURCE: &str = "\
actor Maker {\n\
\x20   receive fn make() -> string {\n\
\x20       \"unused-owned-reply\".to_upper()\n\
\x20   }\n\
}\n\
\n\
fn main() -> i64 {\n\
\x20   let m = spawn Maker;\n\
\x20   let r = select {\n\
\x20       _reply from m.make() => 7,\n\
\x20   };\n\
\x20   if r == 7 { print(\"k\"); }\n\
\x20   0\n\
}\n";

fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("select-arm-owned-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — an abort here means the \
         select-arm binding's scope-exit drop stacked on another release (the reply \
         channel's loser-leg reap, or the escape move's destination owner);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read the selected value back verbatim — scribbled/short output \
         indicates a use-after-free read on a prematurely released reply;\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// #1875 headline: an unused owned ask-reply binding is released per
/// iteration — flat slope (pre-fix ~1 leak/iter).
#[test]
fn ask_unused_owned_binding_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g1875_ask_unused", ask_unused_binding_loop_source);
}

/// Channel-recv mirror: an unused owned recv binding is released per
/// iteration — flat slope.
#[test]
fn recv_unused_owned_binding_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g1875_recv_unused", recv_unused_binding_loop_source);
}

/// Timeout-wins: `after` beats owned ask/recv losers every iteration; losers
/// reaped by the channel destructor, unwritten binding slots not dropped —
/// flat slope.
#[test]
fn after_wins_owned_losers_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g1875_after_wins", after_wins_owned_losers_loop_source);
}

/// Selected-value escape holds a flat slope: the moved-out reply is released
/// exactly once per iteration as the `let`-bound result.
#[test]
fn escape_binding_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g1875_escape_loop", escape_binding_loop_source);
}

/// Loser-leg exactly-once wall: winner escapes, loser reaped by the channel
/// destructor, no double-free under the poisoned allocator, winner readable.
#[test]
fn select_loser_owned_reply_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "g1875_select_loser_scribble",
        SELECT_LOSER_SCRIBBLE_SOURCE,
        "FAST-OWNED-REPLY",
    );
}

/// Escape exactly-once wall: the arm-body move suppresses the scope-exit
/// drop; the escaped value reads back verbatim and the run exits clean.
#[test]
fn selected_value_escape_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "g1875_escape_scribble",
        ESCAPE_SCRIBBLE_SOURCE,
        "ESCAPE-OWNED-REPLY",
    );
}

/// Straight-line unused binding: single Return-edge release, no double-free.
#[test]
fn unused_owned_binding_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "g1875_unused_binding_scribble",
        UNUSED_BINDING_SCRIBBLE_SOURCE,
        "k",
    );
}
