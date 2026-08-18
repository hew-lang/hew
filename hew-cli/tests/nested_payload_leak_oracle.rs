//! Nested-constructor payload leak oracle: the W5.020 nested-destructure
//! drop-discipline regression.
//!
//! The match-pattern-width lane admits nested constructor subpatterns in
//! tuple-variant payload position (`Err(ParseError::Invalid(_))`,
//! `Err(ParseError::Invalid(m))`, `Ok(Ok(v))`, arbitrary depth). The first
//! cut leaked the nested payload's heap allocation on every executed arm —
//! both the **wildcarded** inner payload (`Invalid(_)`) and the **bound**
//! inner payload (`Invalid(m)`) — because the
//! `derive_enum_composite_drop_allowed` escape scan misread the predicate's
//! inner tag read (and the i64 tag destination it lands in) as a payload
//! escape, wrongly excluding the matched composite from its tag-aware
//! `DropKind::EnumInPlace` scope-exit drop. The bound case additionally
//! tripped a scope-comparison fragility: the synthetic predicate transient
//! has no HIR scope, so its onward hand-off to the real inner binder failed
//! the same-scope check whenever scope ids were shifted by other functions
//! in the module (a cross-function-ordering-dependent leak).
//!
//! The fix (in `hew-mir/src/lower.rs`):
//!   * tag reads (`Place::EnumTag` / `Place::MachineTag`) of a payload binder
//!     are exempted from the payload-escape classification (a discriminant is
//!     a bitcopy, it carries no heap ownership);
//!   * the payload-binder propagation only taints heap-owning destinations
//!     (the i64 tag is never a binder); and
//!   * a synthetic (scopeless) transient propagates its payload-binder taint
//!     to inner same-arm binders freely, instead of comparing against an
//!     absent scope.
//!
//! With the fix the matched composite keeps its recursive `EnumInPlace` drop,
//! which descends through the nesting depth and frees the inner payload
//! exactly once; the bound inner binder is read-but-not-independently-dropped
//! so there is no double-free.
//!
//! ## Slope methodology
//!
//! Mirrors `bytes_drop_leak_oracle.rs` / `recv_loop_leak_oracle.rs`: compile
//! the same nested-match shape at a LOW and a HIGH iteration count, measure
//! leak NODE counts under `leaks --atExit` with the poisoned allocator, and
//! assert the delta stays within a small constant independent of iterations.
//! The pre-fix bug is PER-ITERATION GROWTH (slope ~1.0 leak/iter per nested
//! arm), which over the iteration delta lands an order of magnitude above the
//! tolerance. Absolute counts are not asserted (runtime one-offs jitter ±1).
//!
//! The combined shape additionally serves as a MallocScribble/GuardEdges
//! over-eager-free probe: a non-matching nested predicate must NOT free what
//! the next predicate still inspects, and a bound inner payload must NOT be
//! double-freed. Any such fault aborts under the poisoned allocator before a
//! leak count is read.
//!
//! macOS-only (`leaks(1)` is Darwin's allocator inspector); skips elsewhere.

#![cfg(unix)]

mod support;

use support::leak_slope::{measure_leaks, require_leaks_tool};

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// Low iteration count: exercises each nested arm at least twice while
/// staying close to the constant-overhead floor.
const LOW_ITERS: usize = 20;

/// High iteration count for the slope check. A slope of ~1.0 leak/iter per
/// nested arm (the pre-fix measurement) produces `HIGH - LOW` excess nodes
/// per leaking arm against the tolerance.
const HIGH_ITERS: usize = 2000;

/// Maximum permitted leak-node delta between the HIGH and LOW probes. Same
/// headroom rationale as the sibling oracles: absorbs one-off runtime
/// allocations while still catching a slope of ~0.1 leaks/iter.
const SLOPE_TOLERANCE: usize = 8;

// ── fixtures ──────────────────────────────────────────────────────────────

/// A heap inner payload (`to_uppercase` allocates a fresh refcounted buffer,
/// unlike a static string literal) destructured through a nested constructor
/// pattern. Both the wildcard (`Invalid(_)`) and the bound (`Invalid(m)`)
/// inner forms are exercised, plus a multi-variant nested enum whose
/// non-matching arms are probed before the matching one (the "must not free
/// what the next predicate inspects" path). The scrutinees are `let`-bound so
/// the composite earns its `EnumInPlace` scope-exit drop. `wild` and `bound`
/// share the module so the cross-function scope-id-shift regression is
/// covered.
fn nested_match_source(iters: usize) -> String {
    format!(
        "enum ParseError {{ Invalid(string); }}\n\
         enum Tri {{ A(string); B(string); C(string); }}\n\
         fn make(n: i64) -> Result<i64, ParseError> {{\n\
         \x20   if n == 0 {{ Ok(1) }} else {{ Err(ParseError.Invalid(\"bad-input\".to_upper())) }}\n\
         }}\n\
         fn pick(n: i64) -> Result<i64, Tri> {{\n\
         \x20   match n % 3 {{\n\
         \x20       0 => Err(Tri.A(\"alpha\".to_upper())),\n\
         \x20       1 => Err(Tri.B(\"beta\".to_upper())),\n\
         \x20       _ => Err(Tri.C(\"gamma\".to_upper())),\n\
         \x20   }}\n\
         }}\n\
         fn wild(n: i64) -> i64 {{\n\
         \x20   let r = make(n);\n\
         \x20   match r {{ Ok(v) => v, Err(ParseError.Invalid(_)) => -1 }}\n\
         }}\n\
         fn bound(n: i64) -> i64 {{\n\
         \x20   let r = make(n);\n\
         \x20   match r {{ Ok(v) => v, Err(ParseError.Invalid(m)) => m.len() }}\n\
         }}\n\
         fn probe(n: i64) -> i64 {{\n\
         \x20   let r = pick(n);\n\
         \x20   match r {{\n\
         \x20       Ok(v) => v,\n\
         \x20       Err(Tri.A(_)) => 1,\n\
         \x20       Err(Tri.B(m)) => m.len(),\n\
         \x20       Err(Tri.C(_)) => 3,\n\
         \x20   }}\n\
         }}\n\
         fn main() {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   var i: i64 = 0;\n\
         \x20   while i < {iters} {{\n\
         \x20       total = total + wild(1);\n\
         \x20       total = total + bound(1);\n\
         \x20       total = total + probe(i);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   println(f\"{{total}}\");\n\
         }}\n"
    )
}

// ── leak measurement plumbing (same shape as bytes_drop_leak_oracle) ───────

/// Compile `source` to a native binary via `hew compile --emit-dir` and
/// return the binary path.
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

/// Run `bin` under the full poisoned-allocator triple WITHOUT `leaks`, with
/// periodic heap verification, and assert it exits cleanly. This is the
/// over-eager-free / double-free / use-after-free guard: a nested predicate
/// that frees a payload the next predicate still inspects, or a bound inner
/// payload double-freed alongside the composite's recursive drop, aborts here.
fn assert_no_poisoned_allocator_abort(bin: &std::path::Path) {
    let output = Command::new(bin)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .env("MallocCheckHeapEach", "256")
        .output()
        .expect("run nested-match binary under poisoned allocator");
    assert!(
        output.status.success(),
        "nested-match shape aborted under the poisoned-allocator triple \
         (double-free / use-after-free from over-eager nested-payload drop): {}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
}

// ── oracle ─────────────────────────────────────────────────────────────────

/// Nested-constructor payload drop discipline: bound + wildcard inner
/// payloads must each free their heap exactly once. Pre-fix slope is ~1.0
/// leak/iter per leaking nested arm; post-fix the count holds flat. Also
/// asserts no over-eager free under the poisoned-allocator triple.
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn nested_payload_no_per_iter_leak_slope() {
    require_leaks_tool();

    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("nested-payload-leak-")
        .tempdir()
        .expect("tempdir");

    let bin_low = compile_to_native(&nested_match_source(LOW_ITERS), dir.path(), "nested_low");
    let bin_high = compile_to_native(&nested_match_source(HIGH_ITERS), dir.path(), "nested_high");

    // Over-eager-free / double-free / UAF guard first: a crash here would
    // also break the leak probe, so surface the clearer failure mode.
    assert_no_poisoned_allocator_abort(&bin_high);

    let low_leaks = measure_leaks(&bin_low);
    let high_leaks = measure_leaks(&bin_high);

    eprintln!(
        "nested_payload: low_iters={LOW_ITERS} low_leaks={low_leaks} \
         high_iters={HIGH_ITERS} high_leaks={high_leaks} tolerance={SLOPE_TOLERANCE}"
    );
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "nested_payload: per-iteration leak SLOPE — low_iters={LOW_ITERS} low_leaks={low_leaks}, \
         high_iters={HIGH_ITERS} high_leaks={high_leaks}. Excess of {} NODES over the tolerance \
         of {SLOPE_TOLERANCE} indicates a nested-constructor payload is not released once per \
         match (pre-fix slope is ~1.0 leak/iter per nested arm). Re-run with \
         `MallocStackLogging=1 leaks --atExit -- {}` to see the leaked block's stack.",
        high_leaks.saturating_sub(low_leaks + SLOPE_TOLERANCE),
        bin_high.display()
    );
}
