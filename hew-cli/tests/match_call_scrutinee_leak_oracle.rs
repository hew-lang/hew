//! From-call match-scrutinee enum-payload leak oracle (#2429 guard).
//!
//! `match f() { Ok(b) => …, Err(e) => {} }` over a `Result`/`Option` returned
//! from a called function consumes the composite through an anonymous MIR
//! temp. Before the fix the temp had no owner, so the arm-destructured payload
//! was released on NO edge: each iteration of a
//! `while … { match f() { … } }` loop leaked one payload allocation — the
//! primary consumption shape for every read-style API (`while` over
//! `tls.read()`, chunked HTTP bodies, line framing). A `bytes` payload leaked
//! even when LET-bound, because the composite prover classified the borrowing
//! `b.len()` (`hew_bytes_len` receiver read) as an owning escape.
//!
//! These slope guards pin the fix per shape: the scrutinee temp now carries a
//! synthetic owned binding released through the standard machinery (back-edge
//! per iteration, Return for the straight line, scope-close for
//! break/continue), and the bytes receiver-borrow contract joined the
//! `binder_read_is_borrow_safe_*` exemption.
//!
//! ## De-flake: slope, not single-shot exact-zero
//!
//! Each shape is measured at LOW and HIGH iteration counts and the leak-node
//! delta asserted within tolerance — the delta cancels the constant runtime
//! baseline. A regression re-opens a 1-node-per-iteration slope
//! (LOW→HIGH delta of 40 nodes) and trips the assertion.
//!
//! The scribble pins are the #2384 exactly-once wall: the payload is read back
//! verbatim before its release (a use-after-free garbles output) and the run
//! must exit clean under the poisoned-allocator triple (a double free — the
//! back-edge release stacking on a scope-exit release — aborts).

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── slope shapes ────────────────────────────────────────────────────────────

/// The #2429 zero-FFI headline repro: `Result<bytes, string>` from a call,
/// matched directly (no `let`) inside a `while` loop, payload borrowed by
/// `.len()`.
fn bytes_scrutinee_loop_source(frames: usize) -> String {
    format!(
        "fn f() -> Result<bytes, string> {{\n\
         \x20   Ok(\"g2429-bytes-payload\".to_bytes())\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       match f() {{\n\
         \x20           Ok(b) => {{ total = total + b.len(); }}\n\
         \x20           Err(e) => {{}}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// String payload through the same unbound-scrutinee seam.
fn string_scrutinee_loop_source(frames: usize) -> String {
    format!(
        "fn f() -> Result<string, string> {{\n\
         \x20   Ok(\"g2429-string-payload\".to_upper())\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       match f() {{\n\
         \x20           Ok(b) => {{ total = total + b.len(); }}\n\
         \x20           Err(e) => {{}}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// The Err arm is the taken path: a fresh heap error payload per iteration
/// must be released on the back-edge exactly like an Ok payload.
fn err_arm_loop_source(frames: usize) -> String {
    format!(
        "fn f(n: i64) -> Result<i64, string> {{\n\
         \x20   if n >= 0 {{ Err(\"g2429-err-payload\".to_upper()) }} else {{ Ok(n) }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var hits = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       match f(i) {{\n\
         \x20           Ok(v) => {{}}\n\
         \x20           Err(e) => {{ if !e.is_empty() {{ hits = hits + 1; }} }}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if hits > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Composite (record) payload with an owned string field: rides the in-place
/// composite drop machinery through the same scrutinee seam.
fn composite_payload_loop_source(frames: usize) -> String {
    format!(
        "type Row {{\n\
         \x20   name: string;\n\
         \x20   id: i64;\n\
         }}\n\
         \n\
         fn f(n: i64) -> Result<Row, string> {{\n\
         \x20   Ok(Row {{ name: \"g2429-row-payload\".to_upper(), id: n }})\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       match f(i) {{\n\
         \x20           Ok(r) => {{ total = total + r.id; }}\n\
         \x20           Err(e) => {{}}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total >= 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// `continue` inside the taken arm: the payload of a continued iteration is
/// released on the continue edge / back-edge, not stranded.
fn continue_loop_source(frames: usize) -> String {
    format!(
        "fn f() -> Result<bytes, string> {{\n\
         \x20   Ok(\"g2429-continue-payload\".to_bytes())\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var seen = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       i = i + 1;\n\
         \x20       match f() {{\n\
         \x20           Ok(b) => {{ if b.len() > 3 {{ continue; }} }}\n\
         \x20           Err(e) => {{}}\n\
         \x20       }}\n\
         \x20       seen = seen + 1;\n\
         \x20   }}\n\
         \x20   if seen == 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// Early `return` out of the consuming arm mid-loop, from a helper called
/// per measurement frame: the returning iteration's payload is released on
/// the Return plan; prior iterations released on the back-edge.
fn early_return_loop_source(frames: usize) -> String {
    format!(
        "fn f() -> Result<bytes, string> {{\n\
         \x20   Ok(\"g2429-return-payload\".to_bytes())\n\
         }}\n\
         \n\
         fn run(cap: i64) -> i64 {{\n\
         \x20   var i = 0;\n\
         \x20   while i < cap {{\n\
         \x20       match f() {{\n\
         \x20           Ok(b) => {{ if b.len() == 20 && i == cap - 1 {{ return i; }} }}\n\
         \x20           Err(e) => {{}}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   -1\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let r = run({frames});\n\
         \x20   if r >= 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

/// The #2429 acceptance surface: a looped `tls.read()` (offline-deterministic
/// per the tls fixture precedent — port 1 refuses fast, the Err arm is taken
/// every iteration). Covers both the from-call scrutinee release and the TLS
/// bridge no longer materialising a discarded `last_error()` string per Err.
fn tls_read_loop_source(frames: usize) -> String {
    format!(
        "import std::net::tls;\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let stream = tls.connect(\"127.0.0.1\", 1);\n\
         \x20   var errs = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       match tls.read(stream, 16) {{\n\
         \x20           Ok(data) => {{ let n = data.len(); }}\n\
         \x20           Err(e) => {{ errs = errs + 1; }}\n\
         \x20       }}\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if errs > 0 {{ 0 }} else {{ 1 }}\n\
         }}\n"
    )
}

// ── scribble exactly-once pins ──────────────────────────────────────────────

/// Loop whose `break` fires on the LAST iteration — the shape where the
/// back-edge release and the loop-exit/scope-close release are adjacent. The
/// payload is read back verbatim each iteration; a double free (back-edge
/// release stacking on a second release) aborts under the poisoned allocator.
const LAST_ITERATION_BREAK_SCRIBBLE_SOURCE: &str = "\
fn f() -> Result<string, string> {\n\
\x20   Ok(\"g2429-scribble-payload\".to_upper())\n\
}\n\
\n\
fn main() {\n\
\x20   var i = 0;\n\
\x20   while i < 3 {\n\
\x20       match f() {\n\
\x20           Ok(b) => {\n\
\x20               print(\"o\");\n\
\x20               if i == 2 { break; }\n\
\x20           }\n\
\x20           Err(e) => { print(\"e\"); }\n\
\x20       }\n\
\x20       i = i + 1;\n\
\x20   }\n\
}\n";

/// Straight-line from-call scrutinee: released once on Return, payload read
/// back before the release.
const SINGLE_MATCH_SCRIBBLE_SOURCE: &str = "\
fn f() -> Result<bytes, string> {\n\
\x20   Ok(\"g2429-single\".to_bytes())\n\
}\n\
\n\
fn main() {\n\
\x20   match f() {\n\
\x20       Ok(b) => { if b.len() == 12 { print(\"m\"); } }\n\
\x20       Err(e) => { print(\"e\"); }\n\
\x20   }\n\
}\n";

fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("match-call-scrutinee-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash here means the \
         from-call scrutinee's payload was double-freed (the back-edge release stacking \
         on a second release of the same buffer);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read the arm-bound payload back verbatim — scribbled/short output \
         indicates a use-after-free read on a prematurely released payload;\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// #2429 headline: the zero-FFI bytes repro holds a flat leak slope.
#[test]
fn from_call_bytes_scrutinee_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_bytes_scrutinee", bytes_scrutinee_loop_source);
}

/// String payload through the unbound-scrutinee seam holds a flat slope.
#[test]
fn from_call_string_scrutinee_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_string_scrutinee", string_scrutinee_loop_source);
}

/// The Err-arm payload is released per iteration, not just the Ok arm.
#[test]
fn err_arm_payload_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_err_arm", err_arm_loop_source);
}

/// A record payload with an owned field rides the composite in-place drop
/// through the same seam.
#[test]
fn composite_payload_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_composite_payload", composite_payload_loop_source);
}

/// `continue` from the consuming arm releases the continued iteration's
/// payload.
#[test]
fn continue_edge_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_continue_edge", continue_loop_source);
}

/// Early `return` out of the consuming arm releases the in-flight payload on
/// the Return plan and prior iterations on the back-edge.
#[test]
fn early_return_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_early_return", early_return_loop_source);
}

/// The #2429 acceptance surface: a looped `tls.read()` (offline Err path)
/// holds a flat slope — one composite + one payload per read, each released.
#[test]
fn tls_read_loop_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g2429_tls_read_loop", tls_read_loop_source);
}

/// Exactly-once wall: back-edge release adjacent to the break/loop-exit edge
/// on the last iteration must not double-free, and the payload reads back
/// verbatim first.
#[test]
fn last_iteration_break_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "g2429_last_iter_break_scribble",
        LAST_ITERATION_BREAK_SCRIBBLE_SOURCE,
        "ooo",
    );
}

/// Straight-line from-call scrutinee: single Return-plan release, no
/// double-free, payload readable.
#[test]
fn single_from_call_match_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "g2429_single_match_scribble",
        SINGLE_MATCH_SCRIBBLE_SOURCE,
        "m",
    );
}
