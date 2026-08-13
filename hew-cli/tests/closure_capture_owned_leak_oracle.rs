//! Per-iteration leak / double-free oracle for the closure-env KEYSTONE: an
//! escaping (heap-boxed) closure that captures an OWNED value (`string`,
//! `Vec`, a heap-owning record, an owned-payload enum) must free that
//! captured value EXACTLY once when the closure is dropped.
//!
//! ## What this proves
//!
//! A closure that escapes its introducing scope (returned, stored, passed to a
//! higher-order callee) has its capture environment heap-boxed at the literal
//! site: `[free_thunk: ptr][captures...]`. The captured owning value's handle
//! is byte-copied into the captures region and the caller's own binding drop is
//! suppressed (its local is read by the env `RecordInit` ingress → marked
//! aliased → excluded from the kept-drop set), so the env becomes the SOLE
//! owner of that handle. The env free thunk must release each owned captured
//! field through the per-field drop authority before freeing the box, so the
//! captured value is freed exactly once as the env's sole owner.
//!
//! A checker-`Borrow` capture (read-only body use, no `move`) whose source
//! stays live takes a RETAINED SHARE instead: the env field is
//! `OwnsClonedOrRetained`, an explicit retain mints the env's co-owner, and
//! the source binding keeps its own scope-exit drop — two owners, each
//! released exactly once (D3, amends #2933's closure-surface claim).
//!
//! The failure modes this oracle catches:
//!   * a LEAK (the env frees the box but not the captured handle) — a positive
//!     per-iteration leak slope;
//!   * a SECOND owner (the caller binding AND the env both freeing the handle)
//!     — a double-free that aborts under the poisoned-allocator triple;
//!   * an OVER-DROP corrupting a live value — a scribbled output / non-zero
//!     exit.
//!
//! ## Methodology: per-iteration leak slope
//!
//! Each capturing shape is built into a loop that constructs the owned value,
//! captures it into a returned (escaping) closure, invokes the closure, and
//! lets it drop — once per iteration. The shape is compiled at a LOW and a HIGH
//! iteration count and the leak NODE counts are differenced (see
//! [`support::leak_slope`]): a correct escaping capture frees the captured
//! handle every iteration and holds the slope flat; a leaked capture grows the
//! node count with the iteration count. The delta cancels the constant baseline
//! noise a single-shot `leaks --atExit` count cannot, so the gate is
//! deterministic.
//!
//! The captured `string` is built with `.to_upper()` (a fresh runtime heap
//! string with no concat-temp of its own), and the captured `Vec` with
//! `Vec::new()` + `push` (a provably-heap buffer) — so the only per-iteration
//! heap node the slope can see is the captured handle the env must free.
//!
//! ## Skip behaviour
//!
//! `leaks(1)` is Darwin's allocator inspector; on non-macOS hosts the slope
//! probes log `skip:` and return. The `MallocScribble` no-double-free pins run
//! on any unix host.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

// -- fixtures ----------------------------------------------------------------

/// Escaping closure captures a runtime-built owned `string`. `make_label`
/// builds a fresh heap `string` via `.to_upper()` and returns `|| label.len()`,
/// so the closure env is heap-boxed and owns the captured `label`; the env free
/// thunk must release it exactly once when the returned closure drops at the end
/// of each loop iteration. `label.len()` (16 for the uppercased seed) plus the
/// iteration index is summed so the calls cannot be eliminated; `main`
/// self-checks the total so the scribble pin's `success()` holds.
fn captures_string_loop_source(frames: usize) -> String {
    let seed_len: usize = "row-payload-seed".len();
    let expected_total = frames * seed_len + frames * frames.saturating_sub(1) / 2;
    format!(
        "fn make_label(n: i64) -> fn() -> i64 {{\n\
         \x20   let label = \"row-payload-seed\".to_upper();\n\
         \x20   || label.len() + n\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let f = make_label(i);\n\
         \x20       total = total + f();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 91; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Escaping closure captures an owned `Vec<i64>`. `make_counter` builds the Vec
/// with `Vec::new()` + `push` (a provably-heap buffer) and returns `|| xs.len()`,
/// so the env is heap-boxed and owns the captured `xs`; the free thunk must
/// release the Vec exactly once when the returned closure drops. Each call
/// returns `2 + n`, summed so the calls cannot be eliminated and `main`
/// self-checks the total.
fn captures_vec_loop_source(frames: usize) -> String {
    let expected_total = frames * 2 + frames * frames.saturating_sub(1) / 2;
    format!(
        "fn make_counter(n: i64) -> fn() -> i64 {{\n\
         \x20   var xs: Vec<i64> = Vec::new();\n\
         \x20   xs.push(10);\n\
         \x20   xs.push(20);\n\
         \x20   || xs.len() + n\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let f = make_counter(i);\n\
         \x20       total = total + f();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 92; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Escaping closure captures a heap-owning RECORD (`Holder { counts: Vec<i64> }`)
/// built from a local binding and moved into the env (#2419). The record
/// capture dispatches through the synthesised `__hew_record_drop_inplace_Holder`
/// (seeded by the closure-capture drop-seed pass); the free thunk must run it
/// exactly once per iteration so the record's owned Vec is released — a missing
/// body was an LLVM verify reject, an empty one is a positive slope here.
fn captures_record_loop_source(frames: usize) -> String {
    let expected_total = frames * 2 + frames * frames.saturating_sub(1) / 2;
    format!(
        "type Holder {{\n\
         \x20   counts: Vec<i64>;\n\
         }}\n\
         fn make_holder(n: i64) -> fn() -> i64 {{\n\
         \x20   let counts: Vec<i64> = Vec::new();\n\
         \x20   counts.push(10);\n\
         \x20   counts.push(20);\n\
         \x20   let h = Holder {{ counts: counts }};\n\
         \x20   || h.counts.len() + n\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let f = make_holder(i);\n\
         \x20       total = total + f();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 93; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Escaping closure captures an ENUM whose active variant owns a runtime-built
/// `string` payload — the enum twin of the record capture (#2419). The free
/// thunk dispatches `__hew_enum_drop_inplace_Tag`, whose tag-aware body must
/// release the payload string exactly once per iteration.
fn captures_enum_loop_source(frames: usize) -> String {
    let expected_total = frames + frames * frames.saturating_sub(1) / 2;
    format!(
        "enum Tag {{\n\
         \x20   Named(string);\n\
         \x20   Anon;\n\
         }}\n\
         fn make_tagger(n: i64) -> fn() -> i64 {{\n\
         \x20   let t = Tag::Named(\"row-payload-seed\".to_upper());\n\
         \x20   || match t {{\n\
         \x20       Tag::Named(_) => 1 + n,\n\
         \x20       Tag::Anon => n,\n\
         \x20   }}\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let f = make_tagger(i);\n\
         \x20       total = total + f();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 94; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn stack_env_record_field_loop_source(frames: usize) -> String {
    let left_len = "left-payload".len();
    let right_len = "right-payload".len();
    let expected_total = frames * (left_len + right_len) + frames * frames.saturating_sub(1) / 2;
    format!(
        "type Holder {{\n\
         \x20   left: string;\n\
         \x20   right: string;\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let h = Holder {{\n\
         \x20           left: \"left-payload\".to_upper(),\n\
         \x20           right: \"right-payload\".to_upper(),\n\
         \x20       }};\n\
         \x20       let f = || h.left.len() + h.right.len() + i;\n\
         \x20       total = total + f();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 95; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn parameter_record_one_field_loop_source(frames: usize) -> String {
    let left_len = "left-payload".len();
    let expected_total = frames * left_len + frames * frames.saturating_sub(1) / 2;
    format!(
        "type Holder {{\n\
         \x20   left: string;\n\
         \x20   right: string;\n\
         }}\n\
         fn make_reader(h: Holder, n: i64) -> fn() -> i64 {{\n\
         \x20   || h.left.len() + n\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let h = Holder {{\n\
         \x20           left: \"left-payload\".to_upper(),\n\
         \x20           right: \"right-payload\".to_upper(),\n\
         \x20       }};\n\
         \x20       let f = make_reader(h, i);\n\
         \x20       total = total + f();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 96; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn parameter_record_both_fields_loop_source(frames: usize) -> String {
    let left_len = "left-payload".len();
    let right_len = "right-payload".len();
    let expected_total = frames * (left_len + right_len) + frames * frames.saturating_sub(1) / 2;
    format!(
        "type Holder {{\n\
         \x20   left: string;\n\
         \x20   right: string;\n\
         }}\n\
         fn make_reader(h: Holder, n: i64) -> fn() -> i64 {{\n\
         \x20   || h.left.len() + h.right.len() + n\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let h = Holder {{\n\
         \x20           left: \"left-payload\".to_upper(),\n\
         \x20           right: \"right-payload\".to_upper(),\n\
         \x20       }};\n\
         \x20       let f = make_reader(h, i);\n\
         \x20       total = total + f();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 97; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn string_parameter_capture_loop_source(frames: usize) -> String {
    let label_len = "row-payload-seed".len();
    let expected_total = frames * label_len + frames * frames.saturating_sub(1) / 2;
    format!(
        "fn make_reader(label: string, n: i64) -> fn() -> i64 {{\n\
         \x20   || label.len() + n\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let label = \"row-payload-seed\".to_upper();\n\
         \x20       let f = make_reader(label, i);\n\
         \x20       total = total + f();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 98; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn nested_closure_parameter_control_loop_source(frames: usize) -> String {
    let expected_total = frames * frames.saturating_sub(1);
    format!(
        "fn make_base(n: i64) -> fn() -> i64 {{\n\
         \x20   || n\n\
         }}\n\
         fn wrap(f: fn() -> i64, n: i64) -> fn() -> i64 {{\n\
         \x20   || f() + n\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let inner = make_base(i);\n\
         \x20       let outer = wrap(inner, i);\n\
         \x20       total = total + outer();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 99; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Read-only (`Borrow`) capture with the SOURCE STILL LIVE after the capture —
/// the retained-share contract: each escaping env takes an
/// `OwnsClonedOrRetained` field backed by an explicit retain, the source
/// binding keeps its own scope-exit owner, and both release exactly once.
/// Shapes: read-after-capture (string), two closures sharing one source plus a
/// direct read, and a record source read after a field-projecting capture.
fn borrow_capture_read_after_loop_source(frames: usize) -> String {
    let label_len = "row-payload-seed".len();
    let expected_total = frames * 2 * label_len + frames * frames.saturating_sub(1) / 2;
    format!(
        "fn make_reader(n: i64) -> fn() -> i64 {{\n\
         \x20   let label = \"row-payload-seed\".to_upper();\n\
         \x20   let f = || label.len() + n;\n\
         \x20   let after = label.len();\n\
         \x20   || f() + after\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       let r = make_reader(i);\n\
         \x20       total = total + r();\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 90; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn borrow_capture_two_shares_loop_source(frames: usize) -> String {
    let label_len = "row-payload-seed".len();
    let expected_total = frames * 3 * label_len + frames * frames.saturating_sub(1);
    format!(
        "fn two_shares(n: i64) -> i64 {{\n\
         \x20   let label = \"row-payload-seed\".to_upper();\n\
         \x20   let a = || label.len() + n;\n\
         \x20   let b = || label.len() + n;\n\
         \x20   a() + b() + label.len()\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       total = total + two_shares(i);\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 89; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// Record source read after a field-projecting capture, closure invoked
/// directly (no std combinator): isolates the retained-share capture ownership
/// from the pre-existing closure-pair-argument leak the `iter::any` pin below
/// tracks.
fn borrow_capture_record_read_after_loop_source(frames: usize) -> String {
    let item_len = "row-item".len();
    let expected_total = frames * (1 + item_len) + frames * frames.saturating_sub(1) / 2;
    format!(
        "type Claim {{\n\
         \x20   item: string;\n\
         \x20   run_id: string;\n\
         }}\n\
         fn probe() -> i64 {{\n\
         \x20   let claim = Claim {{ item: \"row-item\".to_upper(), run_id: \"row-run\".to_upper() }};\n\
         \x20   let matches_run = |t: string| t == claim.run_id;\n\
         \x20   var score: i64 = 0;\n\
         \x20   if matches_run(\"ROW-RUN\") {{ score = score + 1; }}\n\
         \x20   if matches_run(\"nope\") {{ score = score + 100; }}\n\
         \x20   score + claim.item.len()\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       total = total + probe() + i;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 87; }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// The elegance-probe shape that falsified #2933's "closures solid" claim: a
/// record binding field-projected inside an `iter::any` predicate closure, with
/// the record still used AFTERWARDS. Pre-fix this was rejected outright
/// (`E_MIR_CHECK UseAfterConsume` — the Borrow capture lowered as `OwnsMoved`),
/// which forced index loops in every filter+side-output pattern.
///
/// This shape has NO leak-slope oracle: the closure-pair temp passed into the
/// generic `iter::any` leaks its env on main independently of capture
/// ownership (differentially confirmed: main-era and post-fix binaries leak
/// the same ~4 nodes/iteration; a capture-free predicate still leaks
/// ~1/iteration) — a separate std/iter call-carrier seam. The
/// direct-invocation slope oracle above owns the capture-ownership leak
/// question; this pin owns compile + correctness + no-double-free.
fn borrow_capture_record_iter_any_loop_source(frames: usize) -> String {
    let expected_total = frames * 2 + frames * frames.saturating_sub(1) / 2;
    format!(
        "import std::iter;\n\
         pub type Claim {{\n\
         \x20   item: string;\n\
         \x20   run_id: string;\n\
         }}\n\
         fn probe() -> i64 {{\n\
         \x20   let claim = Claim {{ item: \"row-item\".to_upper(), run_id: \"row-run\".to_upper() }};\n\
         \x20   let runs = Vec::new();\n\
         \x20   runs.push(\"ROW-RUN\");\n\
         \x20   let hit = iter::any(runs.into_iter(), |terminal: string| terminal == claim.run_id);\n\
         \x20   let out = Vec::new();\n\
         \x20   out.push(claim.item);\n\
         \x20   var score: i64 = 0;\n\
         \x20   if hit {{ score = score + 1; }}\n\
         \x20   score + out.len()\n\
         }}\n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{\n\
         \x20       total = total + probe() + i;\n\
         \x20   }}\n\
         \x20   total\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   let total = run_loop({frames});\n\
         \x20   if total != {expected_total} {{ return 88; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn shared_source_two_closures_source() -> String {
    "type PairFns {\n\
     \x20   a: fn() -> i64;\n\
     \x20   b: fn() -> i64;\n\
     }\n\
     fn make_pair(n: i64) -> PairFns {\n\
     \x20   let label: string = \"row-payload-seed\".to_upper();\n\
     \x20   let a = || label.len() + n;\n\
     \x20   let b = || label.len() + n;\n\
     \x20   PairFns { a: a, b: b }\n\
     }\n\
     fn main() -> i64 {\n\
     \x20   let p = make_pair(1);\n\
     \x20   p.a() + p.b()\n\
     }\n"
    .to_string()
}

fn shared_source_closure_plus_original_store_source() -> String {
    "type ClosureAndLabel {\n\
     \x20   f: fn() -> i64;\n\
     \x20   label: string;\n\
     }\n\
     fn make_pair(n: i64) -> ClosureAndLabel {\n\
     \x20   let label: string = \"row-payload-seed\".to_upper();\n\
     \x20   let f = || label.len() + n;\n\
     \x20   ClosureAndLabel { f: f, label: label }\n\
     }\n\
     fn main() -> i64 {\n\
     \x20   let p = make_pair(1);\n\
     \x20   p.f() + p.label.len()\n\
     }\n"
    .to_string()
}

// -- correctness pins --------------------------------------------------------

/// Run `source` to native, execute under the poisoned-allocator triple, and
/// assert clean exit. A crash here is a double-free (the caller binding AND the
/// env both freeing the captured handle); a non-zero exit is a miscomputed read
/// off a scribbled capture, or the fixture's own total check failing.
fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("closure-capture-owned-df-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{shape_name}: escaping closure capture must free the owned value exactly once -- a \
         crash here indicates a double-free: the caller binding freed the same handle the env \
         now owns. The env must be the sole owner (the caller's scope-exit drop is suppressed by \
         the aliased-local scan); a non-zero exit is a scribbled-read miscompute or the fixture's \
         own total check;\n{}",
        describe_output(&output)
    );
}

fn assert_compile_fails(shape_name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("closure-capture-fail-closed-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let hew_src = dir.path().join(format!("{shape_name}.hew"));
    std::fs::write(&hew_src, source).expect("write hew source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.path().to_str().expect("emit-dir utf-8"),
            hew_src.to_str().expect("hew src utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");

    assert!(
        !output.status.success(),
        "{shape_name}: expected fail-closed compile rejection, but compile succeeded:\n{}",
        describe_output(&output)
    );
    let combined = format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains(expected),
        "{shape_name}: compile failed but did not mention `{expected}`:\n{combined}"
    );
}

// -- oracles -----------------------------------------------------------------

/// Slope oracle (string): a returned closure capturing a runtime-built owned
/// `string` frees the captured handle every iteration — flat leak slope.
/// Pre-keystone the free thunk freed only the box and this leaked the captured
/// string per iteration (positive slope).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn return_closure_captures_string_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("captures_string", captures_string_loop_source);
}

/// Slope oracle (Vec): a returned closure capturing an owned `Vec<i64>` frees
/// the captured buffer every iteration — flat leak slope.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn return_closure_captures_vec_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("captures_vec", captures_vec_loop_source);
}

/// Slope oracle (record, #2419): a returned closure capturing a heap-owning
/// record frees the record's owned Vec every iteration through the synthesised
/// `__hew_record_drop_inplace_Holder` — flat leak slope.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn return_closure_captures_record_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("captures_record", captures_record_loop_source);
}

/// Slope oracle (enum, #2419 twin): a returned closure capturing an enum with
/// an owned string payload frees the payload every iteration through the
/// synthesised `__hew_enum_drop_inplace_Tag` — flat leak slope.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn return_closure_captures_enum_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("captures_enum", captures_enum_loop_source);
}

/// Stack-env oracle (#2433 shape 1): a direct-call-only closure borrows its
/// stack env; the source record remains the sole owner and must drop once per
/// loop iteration.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn stack_env_capture_record_field_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "stack_env_record_field",
        stack_env_record_field_loop_source,
    );
}

/// Parameter aggregate oracle (#2433 shape 3): env-loaded record string fields
/// in the closure invoke shim are retained temporaries and need balancing drops.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn parameter_record_capture_one_field_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "parameter_record_one_field",
        parameter_record_one_field_loop_source,
    );
}

/// Parameter aggregate oracle with two owned field reads: catches one-drop-per-
/// field gaps in the closure invoke shim.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn parameter_record_capture_both_fields_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "parameter_record_both_fields",
        parameter_record_both_fields_loop_source,
    );
}

/// Narrowed-scope control: direct string parameter capture already had a flat
/// slope; keep it compiling and leak-free.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn string_parameter_capture_control_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "string_parameter_capture_control",
        string_parameter_capture_loop_source,
    );
}

/// Narrowed-scope control: nested closure-parameter capture remains in scope for
/// closure-pair ownership and must not be rejected by aggregate capture guards.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn nested_closure_parameter_control_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "nested_closure_parameter_control",
        nested_closure_parameter_control_loop_source,
    );
}

/// No-double-free pin (string): the escaping capture frees the owned `string`
/// EXACTLY once across 200 iterations. A second owner aborts under the poisoned
/// allocator. Runs on any unix host.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn return_closure_captures_string_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free("captures_string_df", &captures_string_loop_source(200));
}

/// No-double-free pin (Vec): the escaping capture frees the owned `Vec` EXACTLY
/// once across 200 iterations. Runs on any unix host.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn return_closure_captures_vec_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free("captures_vec_df", &captures_vec_loop_source(200));
}

/// No-double-free pin (record, #2419): the escaping record capture frees the
/// record's owned Vec EXACTLY once across 200 iterations — the caller binding's
/// drop is suppressed (the env is the sole owner), so a second release aborts
/// under the poisoned allocator. Runs on any unix host.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn return_closure_captures_record_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free("captures_record_df", &captures_record_loop_source(200));
}

/// No-double-free pin (enum, #2419 twin): the escaping enum capture frees the
/// owned string payload EXACTLY once across 200 iterations. Runs on any unix
/// host.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn return_closure_captures_enum_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free("captures_enum_df", &captures_enum_loop_source(200));
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn stack_env_capture_record_field_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "stack_env_record_field_df",
        &stack_env_record_field_loop_source(200),
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn parameter_record_capture_both_fields_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "parameter_record_both_fields_df",
        &parameter_record_both_fields_loop_source(200),
    );
}

// -- retained-share captures (D3 / #2933 amendment) --------------------------

/// Slope oracle: read-after-capture (string). The checker-`Borrow` capture
/// takes a RETAINED SHARE (`OwnsClonedOrRetained` + explicit retain) instead of
/// consuming the source — pre-fix this shape failed closed with
/// `UseAfterConsume`. The env free thunk and the source's own scope-exit drop
/// each release one share; the slope stays flat.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn borrow_capture_read_after_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "borrow_capture_read_after",
        borrow_capture_read_after_loop_source,
    );
}

/// No-double-free pin: read-after-capture (string) across 200 iterations under
/// the poisoned allocator — the retained env share and the live source binding
/// are independent owners; a second release of EITHER aborts.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn borrow_capture_read_after_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "borrow_capture_read_after_df",
        &borrow_capture_read_after_loop_source(200),
    );
}

/// Slope oracle: two closures sharing one source string, plus a direct read of
/// the source — three co-owners (two retained env shares + the original), three
/// releases, flat slope.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn borrow_capture_two_shares_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "borrow_capture_two_shares",
        borrow_capture_two_shares_loop_source,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn borrow_capture_two_shares_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "borrow_capture_two_shares_df",
        &borrow_capture_two_shares_loop_source(200),
    );
}

/// Slope oracle: record source read after a field-projecting capture (direct
/// invocation) — the env's retained record share and the source's own drop
/// each release once; flat slope.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn borrow_capture_record_read_after_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "borrow_capture_record_read_after",
        borrow_capture_record_read_after_loop_source,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn borrow_capture_record_read_after_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "borrow_capture_record_read_after_df",
        &borrow_capture_record_read_after_loop_source(200),
    );
}

/// Elegance-probe regression (D3, amends #2933): compile + correctness +
/// no-double-free for the `iter::any` shape. See the fixture doc for why this
/// shape carries no slope oracle (pre-existing std/iter closure-pair-argument
/// leak, independent of capture ownership).
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn borrow_capture_record_iter_any_freed_exactly_once_under_malloc_scribble() {
    assert_no_double_free(
        "borrow_capture_record_iter_any_df",
        &borrow_capture_record_iter_any_loop_source(200),
    );
}

// -- remaining fail-closed seams ---------------------------------------------

/// The `label` sharing between two escaping closures is now legal (each env
/// takes a retained share), but storing a CAPTURING closure pair into a record
/// field still fails closed — `UseAfterConsume` fires on `a`/`b` at the
/// `RecordInit` (the closure-pair-into-record transfer has no ownership
/// protocol yet). This pin holds the fail-closed line at that seam.
#[test]
fn shared_source_two_escaping_closures_fail_closed() {
    assert_compile_fails(
        "shared_source_two_closures",
        &shared_source_two_closures_source(),
        "UseAfterConsume",
    );
}

/// Same seam as above: the capture itself (closure + original both live) is now
/// a legal retained share, but the record store of the capturing closure pair
/// `f` still fails closed at the `RecordInit`.
#[test]
fn shared_source_closure_plus_original_store_fail_closed() {
    assert_compile_fails(
        "shared_source_original_store",
        &shared_source_closure_plus_original_store_source(),
        "UseAfterConsume",
    );
}
