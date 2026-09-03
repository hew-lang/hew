//! Match-result record ownership oracle for the panic-sibling join shape.
//!
//! `let Endpoint { host, port } = match try_parse_endpoint(...) { .Ok(e) => e,
//! .Err(_) => panic(...) }` moves a heap-owning record payload out of a fresh
//! call carrier while the sibling arm diverges through `hew_panic_msg`. The
//! panic shim's poison continuation is structurally reachable (a
//! `Terminator::Call` requires a `next` block) but can never execute; letting
//! it contribute to the join's liveness meet killed the payload binder at the
//! join, which moved the binder's admitted `record_in_place` release from the
//! function exit to the arm's scope-close edge — BEFORE the join's field loads
//! read the record. Every `net.connect_timeout` call then read and retained a
//! freed `host` string and aborted with `free_cstring: C-string header
//! sentinel missing` (double-free) once the extracted binding dropped too.
//!
//! The dual failure direction is a leak: with the continuation edge excluded
//! from the meet, the binder must STAY Live through the join so its single
//! release still fires at the true exits — starving the return exits instead
//! leaks one heap block per call (the pre-existing behaviour these shapes had
//! before the balancing drop was admitted at all).
//!
//! ## What each oracle pins
//!
//! - **Exact contents under the poisoned-allocator triple**: destructure and
//!   plain-let variants must print the parsed endpoint verbatim and exit
//!   clean. Under `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` a
//!   drop placed before the join's field loads aborts (double-free) or
//!   garbles the output (use-after-free read).
//!
//! - **Per-iteration leak slope** (macOS `leaks(1)`): the parse-destructure
//!   cycle with a NON-VACUOUS heap address (`.to_upper()` — a literal-backed
//!   view needs no free and would mask the leak) must hold the leak-node
//!   count flat. Suppressing the binder's release without re-admitting it at
//!   the exits leaks one block per iteration.

#![cfg(unix)]

mod support;

use support::describe_output;
use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};

// ── fixtures ──────────────────────────────────────────────────────────────

/// Record destructure of a match-result payload whose sibling arm panics —
/// the `net.connect_timeout` shape. `host` is read AFTER the join, so a
/// release scheduled on the arm's scope-close edge is a use-after-free.
const DESTRUCTURE_SOURCE: &str = "\
import std.net;\n\
\n\
fn main() {\n\
\x20   let addr = \"10.11.12.13:8080\".to_upper();\n\
\x20   let Endpoint { host, port } = match net.try_parse_endpoint(\"oracle\", addr) {\n\
\x20       Ok(e) => e,\n\
\x20       Err(err) => { panic(f\"bad: {err}\"); }\n\
\x20   };\n\
\x20   println(host);\n\
\x20   println(f\"{port}\");\n\
}\n";

const DESTRUCTURE_EXPECTED: &str = "10.11.12.13\n8080\n";

/// Plain-let variant: the record binding survives the join whole and its
/// `host` field is read through a retained field load. Same defect surface,
/// without the destructure desugar.
const PLAIN_LET_SOURCE: &str = "\
import std.net;\n\
\n\
fn main() {\n\
\x20   let addr = \"10.11.12.13:8080\".to_upper();\n\
\x20   let e = match net.try_parse_endpoint(\"oracle\", addr) {\n\
\x20       Ok(x) => x,\n\
\x20       Err(err) => { panic(f\"bad: {err}\"); }\n\
\x20   };\n\
\x20   println(e.host);\n\
}\n";

const PLAIN_LET_EXPECTED: &str = "10.11.12.13\n";

/// Looped parse-destructure cycle for the slope probe. Each iteration builds
/// a FRESH heap address, moves the `Endpoint` payload out of the `Result`
/// carrier past a panicking sibling arm, and returns `host.len()` so the
/// extraction cannot be elided.
fn parse_destructure_loop_source(frames: usize) -> String {
    format!(
        "import std.net;\n\
         \n\
         fn cycle() -> i64 {{\n\
         \x20   let addr = \"10.0.0.1:8080\".to_upper();\n\
         \x20   let Endpoint {{ host, port }} = match net.try_parse_endpoint(\"oracle\", addr) {{\n\
         \x20       Ok(e) => e,\n\
         \x20       Err(err) => {{ panic(\"bad\"); }}\n\
         \x20   }};\n\
         \x20   host.len() + port\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       total = total + cycle();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n"
    )
}

// ── scribble correctness pins ─────────────────────────────────────────────

fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    support::require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("match-result-record-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — an abort here means the \
         match-result record was released on the arm's scope-close edge, before the \
         join's field loads read it (the net.connect_timeout double-free);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read the parsed endpoint back verbatim — scribbled or empty output \
         means the record slot was freed before the join read it;\n{}",
        describe_output(&output)
    );
}

// ── oracles ───────────────────────────────────────────────────────────────

/// Destructure pin: `let Endpoint { host, port } = match ...` past a panicking
/// sibling arm must print the parsed endpoint and exit clean under the
/// poisoned allocator.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "poisoned-allocator oracle needs the Darwin allocator triple; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn match_result_record_destructure_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "endpoint_destructure",
        DESTRUCTURE_SOURCE,
        DESTRUCTURE_EXPECTED,
    );
}

/// Plain-let pin: the whole-record binding variant of the same join shape.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "poisoned-allocator oracle needs the Darwin allocator triple; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn match_result_record_plain_let_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble("endpoint_plain_let", PLAIN_LET_SOURCE, PLAIN_LET_EXPECTED);
}

/// Slope oracle: the parse-destructure cycle must hold the leak-node count
/// flat. One leaked block per iteration means the binder's balancing release
/// was suppressed without being re-admitted at the exits.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)`; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn match_result_record_destructure_leak_slope_below_tolerance() {
    support::require_codegen();
    assert_frame_slope_below_tolerance(
        "match_result_record_destructure",
        parse_destructure_loop_source,
    );
}
