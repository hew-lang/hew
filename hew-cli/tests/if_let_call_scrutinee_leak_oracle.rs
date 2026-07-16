//! From-call `if let` scrutinee enum-payload leak oracle (#2429 class).
//!
//! `if let Some(b) = f() { … }` over an owned `Option`/`Result`/enum returned
//! from a called function consumes the composite through an anonymous MIR temp.
//! Before the fix, `lower_if_let` computed the #2648 admission but minted NO
//! from-call owner (only `lower_match_enum_tag`/`lower_while_let` did), so the
//! matched-arm payload was released on NO edge: each evaluation stranded one
//! payload allocation — a `while … { if let Some(b) = f() { … } }` loop leaked
//! one payload per iteration (validated slope 47 == 1 leak/iteration). The fix
//! mints the synthetic scrutinee owner in `lower_if_let`, symmetric with
//! match/while-let; the scope-exit drop elaboration then releases it on the
//! matched edge (payload moved out, shell composite drop), the refuted `else`
//! edge, and any nested-predicate fallthrough.
//!
//! Slope, not single-shot exact-zero: each shape is measured at LOW and HIGH
//! iteration counts and the leak-node delta asserted within tolerance (the
//! delta cancels the constant runtime baseline). A regression re-opens a
//! 1-node-per-iteration slope and trips the assertion. The scribble pins are
//! the exactly-once wall: the payload reads back verbatim before release, and
//! the run exits clean under the poisoned-allocator triple (a double free
//! aborts).

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── slope shapes ────────────────────────────────────────────────────────────

/// Owned `string` payload from a call, unwrapped by `if let Some` in a loop —
/// the headline validated shape (slope 47 pre-fix).
fn iflet_option_string_loop_source(frames: usize) -> String {
    format!(
        "fn f() -> Option<string> {{ Some(\"g2429-iflet-string\".to_upper()) }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       if let Some(b) = f() {{ total = total + b.len(); }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// `Result<bytes, _>` payload from a call, unwrapped by `if let Ok`.
fn iflet_result_bytes_loop_source(frames: usize) -> String {
    format!(
        "fn f() -> Result<bytes, string> {{ Ok(\"g2429-iflet-bytes\".to_bytes()) }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       if let Ok(b) = f() {{ total = total + b.len(); }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Composite (record) payload with an owned `string` field: rides the
/// in-place composite drop through the same scrutinee seam.
fn iflet_record_payload_loop_source(frames: usize) -> String {
    format!(
        "type Row {{\n\
         \x20   name: string;\n\
         \x20   id: i64;\n\
         }}\n\
         \n\
         fn f(n: i64) -> Option<Row> {{\n\
         \x20   Some(Row {{ name: \"g2429-iflet-row\".to_upper(), id: n }})\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       if let Some(r) = f(i) {{ total = total + r.id; }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total >= 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// The refuted (else) edge is the taken path every iteration: a `None`-tag
/// call still allocates and returns the composite, whose shell must drop on
/// the else edge. Flat slope proves the else-edge release fires.
fn iflet_refuted_edge_loop_source(frames: usize) -> String {
    format!(
        "fn f() -> Option<string> {{ None }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var misses = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       if let Some(b) = f() {{ misses = misses + b.len(); }} else {{ misses = misses + 1; }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if misses == {frames} {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

// ── scope-guard shapes (must NOT over-widen the mint) ────────────────────────

/// `Option<i64>` call scrutinee: no heap payload, so no owner is owed and no
/// leak is possible. Flat slope pins that widening the mint did not introduce
/// a spurious release of a scalar payload.
fn scalar_iflet_loop_source(frames: usize) -> String {
    format!(
        "fn f() -> Option<i64> {{ Some(7) }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       if let Some(b) = f() {{ total = total + b; }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// An owned `Some(string)` bound to a LOCAL, then unwrapped by `if let` over
/// the local — NOT a call scrutinee. The local already owns and drops the
/// payload; `classify_call_scrutinee_admission` returns `NotApplicable` for a
/// binding-ref scrutinee, so NO synthetic owner is minted. Flat slope pins
/// that the mint stays gated on fresh owned-payload CALL scrutinees only —
/// double-minting here would double-free.
fn local_iflet_loop_source(frames: usize) -> String {
    format!(
        "fn main() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       let opt = Some(\"g2429-local-owned\".to_upper());\n\
         \x20       if let Some(b) = opt {{ total = total + b.len(); }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

// ── scribble exactly-once pins ──────────────────────────────────────────────

/// Straight-line from-call `if let` scrutinee: released once, payload read
/// back before the release. A double free (matched-edge release stacking on
/// the shell composite drop) aborts under the poisoned allocator.
const SINGLE_IFLET_SCRIBBLE_SOURCE: &str = "\
fn f() -> Option<string> {\n\
\x20   Some(\"g2429-iflet-single\".to_upper())\n\
}\n\
\n\
fn main() {\n\
\x20   if let Some(b) = f() {\n\
\x20       if b.len() == 18 { print(\"m\"); }\n\
\x20   }\n\
}\n";

/// Refuted edge straight-line: a `None`-tag composite must drop its shell on
/// the else edge exactly once, no double free.
const REFUTED_IFLET_SCRIBBLE_SOURCE: &str = "\
fn f() -> Option<string> {\n\
\x20   None\n\
}\n\
\n\
fn main() {\n\
\x20   if let Some(b) = f() {\n\
\x20       print(b);\n\
\x20   } else {\n\
\x20       print(\"e\");\n\
\x20   }\n\
}\n";

fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("if-let-call-scrutinee-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash here means the \
         from-call if-let scrutinee's payload was double-freed;\n{}",
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

/// #2429 if-let headline: owned-string payload holds a flat slope (slope 47
/// pre-fix).
#[test]
fn iflet_option_string_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_iflet_option", iflet_option_string_loop_source);
}

/// `Result<bytes, _>` payload through the same if-let seam holds a flat slope.
#[test]
fn iflet_result_bytes_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_iflet_result", iflet_result_bytes_loop_source);
}

/// Record payload with an owned field rides the composite drop through the
/// if-let seam.
#[test]
fn iflet_record_payload_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_iflet_record", iflet_record_payload_loop_source);
}

/// The refuted (else) edge releases the composite shell — flat slope proves
/// the else-edge drop fires (care point A: mint precedes the branch).
#[test]
fn iflet_refuted_edge_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_iflet_refuted", iflet_refuted_edge_loop_source);
}

/// Scope guard: a scalar `Option<i64>` call scrutinee stays flat — the mint
/// owes nothing and introduces no spurious release.
#[test]
fn scalar_iflet_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_iflet_scalar", scalar_iflet_loop_source);
}

/// Scope guard: an owned LOCAL unwrapped by if-let (not a call scrutinee)
/// stays flat — the mint must not fire on a binding-ref scrutinee.
#[test]
fn local_iflet_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_iflet_local", local_iflet_loop_source);
}

/// Exactly-once wall: straight-line if-let releases the payload once, reads it
/// back verbatim, no double free.
#[test]
fn single_from_call_iflet_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "g2429_single_iflet_scribble",
        SINGLE_IFLET_SCRIBBLE_SOURCE,
        "m",
    );
}

/// Refuted-edge exactly-once wall: the `None`-tag composite shell drops once
/// on the else edge, no double free.
#[test]
fn refuted_iflet_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "g2429_refuted_iflet_scribble",
        REFUTED_IFLET_SCRIBBLE_SOURCE,
        "e",
    );
}
