//! From-call `let … else` scrutinee enum-payload leak oracle (#2429 class).
//!
//! `let Some(b) = f() else { … }` over an owned `Option`/`Result` returned
//! from a called function consumes the composite through an anonymous MIR temp.
//! Before the fix, `lower_let_else_stmt` computed the #2648 admission but
//! minted NO from-call owner, so the unwrapped payload leaked once per
//! evaluation (validated slope 47). The fix mints the synthetic scrutinee
//! owner symmetric with match/while-let/if-let; the scope-exit drop
//! elaboration releases it on the match edge (payload moved into the escaping
//! binder, shell composite drop) AND on the divergent else edge
//! (`return`/`break`/`continue`/`panic` — no move-out, so the FULL temp drops
//! once).
//!
//! Care point B (the divergent-else edge): the else block diverges, and the
//! full scrutinee temp must drop exactly once on that edge. The scribble pins
//! exercise `return`/`break`/`continue` divergent edges under the poisoned
//! allocator so a divergent-edge double free or under-drop is caught.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── slope shapes ────────────────────────────────────────────────────────────

/// Owned `string` payload from a call, unwrapped by `let … else { continue }`
/// — the match edge is taken every iteration and the payload escapes into the
/// binder. Slope 47 pre-fix.
fn letelse_option_string_loop_source(frames: usize) -> String {
    format!(
        "fn f() -> Option<string> {{ Some(\"g2429-letelse-string\".to_upper()) }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       i = i + 1;\n\
         \x20       let Some(b) = f() else {{ continue; }};\n\
         \x20       total = total + b.len();\n\
         \x20   }}\n\
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// `Result<bytes, _>` payload unwrapped by `let Ok(b) = … else { continue }`.
fn letelse_result_bytes_loop_source(frames: usize) -> String {
    format!(
        "fn f() -> Result<bytes, string> {{ Ok(\"g2429-letelse-bytes\".to_bytes()) }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       i = i + 1;\n\
         \x20       let Ok(b) = f() else {{ continue; }};\n\
         \x20       total = total + b.len();\n\
         \x20   }}\n\
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// The DIVERGENT else edge is taken every iteration: a `None`-tag call whose
/// full composite temp must drop on the `continue` edge. Flat slope proves the
/// divergent-else release fires (care point B).
fn letelse_divergent_edge_loop_source(frames: usize) -> String {
    format!(
        "fn f() -> Option<string> {{ None }}\n\
         \n\
         fn body() -> i64 {{\n\
         \x20   var misses = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < 64 {{\n\
         \x20       i = i + 1;\n\
         \x20       let Some(b) = f() else {{ misses = misses + 1; continue; }};\n\
         \x20       misses = misses + b.len();\n\
         \x20   }}\n\
         \x20   misses\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var acc = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       acc = acc + body();\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if acc > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

// ── scribble exactly-once pins ──────────────────────────────────────────────

/// Divergent-else edges (`return` and `continue` on a refuted call) must each
/// drop the FULL temp once. A divergent-edge double free or under-drop is
/// caught here: clean exit + verbatim output required.
const DIVERGENT_ELSE_SCRIBBLE_SOURCE: &str = "\
fn some_f() -> Option<string> { Some(\"g2429-letelse-taken\".to_upper()) }\n\
fn none_f() -> Option<string> { None }\n\
\n\
fn use_return() -> i64 {\n\
\x20   let Some(b) = some_f() else { return 0; };\n\
\x20   b.len()\n\
}\n\
\n\
fn miss_return() -> i64 {\n\
\x20   let Some(b) = none_f() else { return 7; };\n\
\x20   b.len()\n\
}\n\
\n\
fn loop_continue() -> i64 {\n\
\x20   var seen = 0;\n\
\x20   var i = 0;\n\
\x20   while i < 3 {\n\
\x20       i = i + 1;\n\
\x20       let Some(b) = some_f() else { continue; };\n\
\x20       seen = seen + b.len();\n\
\x20   }\n\
\x20   seen\n\
}\n\
\n\
fn main() {\n\
\x20   let a = use_return();\n\
\x20   let b = miss_return();\n\
\x20   let c = loop_continue();\n\
\x20   if a > 0 && b == 7 && c > 0 { print(\"ok\"); }\n\
}\n";

/// Straight-line matched `let … else`: the payload escapes into the binder,
/// released once at scope exit, read back verbatim.
const SINGLE_LETELSE_SCRIBBLE_SOURCE: &str = "\
fn f() -> Option<string> {\n\
\x20   Some(\"g2429-letelse-single\".to_upper())\n\
}\n\
\n\
fn main() {\n\
\x20   let Some(b) = f() else { return; };\n\
\x20   if b.len() == 20 { print(\"m\"); }\n\
}\n";

fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("let-else-call-scrutinee-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash here means a \
         divergent-else edge double-freed the from-call let-else scrutinee temp;\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read the payload back verbatim — scribbled/short output indicates a \
         use-after-free read on a prematurely released payload;\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// #2429 let-else headline: owned-string payload holds a flat slope (slope 47
/// pre-fix).
#[test]
fn letelse_option_string_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_letelse_option", letelse_option_string_loop_source);
}

/// `Result<bytes, _>` payload through the same let-else seam holds a flat
/// slope.
#[test]
fn letelse_result_bytes_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_letelse_result", letelse_result_bytes_loop_source);
}

/// The divergent (else) edge releases the full composite temp — flat slope
/// proves the divergent-else drop fires (care point B).
#[test]
fn letelse_divergent_edge_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "g2429_letelse_divergent",
        letelse_divergent_edge_loop_source,
    );
}

/// Exactly-once wall across divergent edges: `return`/`continue` on both taken
/// and refuted calls, clean under the poisoned allocator.
#[test]
fn letelse_divergent_edges_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "g2429_letelse_divergent_scribble",
        DIVERGENT_ELSE_SCRIBBLE_SOURCE,
        "ok",
    );
}

/// Straight-line matched let-else: single scope-exit release, payload
/// readable, no double free.
#[test]
fn single_from_call_letelse_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "g2429_single_letelse_scribble",
        SINGLE_LETELSE_SCRIBBLE_SOURCE,
        "m",
    );
}
