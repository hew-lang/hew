//! Match/if-let-bound owned-payload drop leak oracle (hew-lang/hew gap #63 guard).
//!
//! When a `match`/`if let` binds an owned heap payload out of an enum scrutinee
//! the binder is a non-owning ALIAS of the payload — `derive_cow_sole_owner`'s
//! projection-alias taint already excludes the binder from its OWN drop. The
//! composite scrutinee stays the single owner and frees the payload through its
//! tag-aware `DropKind::EnumInPlace` scope-exit drop (the `EnumInPlace` safety
//! net in `derive_enum_composite_drop_allowed`): a binder that only READS the
//! payload (a borrowing transform / comparison / `.len()` / `.is_empty()`) does
//! not transfer ownership out, so the composite keeps its drop and the payload
//! is freed exactly once. The #63 dispute is settled — this is the established
//! behaviour and needs NO prover change. This oracle is the regression GUARD
//! that pins it.
//!
//! ## Why the fixture allocates real heap and borrows without an f-string
//!
//! `Some("…".to_upper())` produces a FRESH refcounted buffer (a static string
//! literal would be vacuously leak-clean — there is nothing to free, so the
//! probe would have no teeth). The bound payload's final use is a pure BORROW
//! (`!s.is_empty()`), never an f-string interpolation: an f-string in the
//! borrow/println path would construct an out-of-scope concat temp whose own
//! drop is a SEPARATE obligation, masking whether the match-bound payload itself
//! was freed (the f-string-temp leak the #54 work documented). Keeping the read
//! a bare borrow isolates the question to the composite's `EnumInPlace` drop.
//!
//! ## De-flake: slope, not single-shot exact-zero
//!
//! A single `leaks --atExit` count==0 over one destructure is nondeterministic
//! (a constant root-set baseline can read non-zero spuriously). Instead the two
//! binder shapes are looped at a LOW and a HIGH iteration count and the leak-node
//! delta is asserted within a small tolerance — the delta cancels the baseline,
//! so the gate is deterministic. A composite that wrongly dropped its
//! `EnumInPlace` arm would leak one `to_upper()` buffer per iteration (positive
//! slope); the settled behaviour holds the count flat.
//!
//! Two shapes (the Stage-0 `g63_iso_A2_clean` / `g63_iso_F` templates):
//!
//! * `match opt { Some(s) => … }` — the match-arm binder; and
//! * `if let Some(s) = opt { … }` — the if-let binder.
//!
//! macOS-only for the slope assertion (`leaks(1)` is Darwin's allocator
//! inspector); the scribble correctness pin runs on any unix host.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// Single-cycle `match`-arm binder over an owned `Option<string>` heap payload,
/// borrow-only final use (`!s.is_empty()`), printing `"m"`. The scribble
/// correctness pin: a double-free of the payload buffer (the composite
/// `EnumInPlace` drop firing while a binder release also freed it) aborts here;
/// a use-after-free read garbles the borrow.
const G63_MATCH_BOUND_SCRIBBLE_SOURCE: &str = "\
fn make(n: i64) -> Option<string> {\n\
\x20   if n > 0 {\n\
\x20       Some(\"g63-match-heap-payload\".to_upper())\n\
\x20   } else {\n\
\x20       None\n\
\x20   }\n\
}\n\
\n\
fn run() {\n\
\x20   let opt = make(1);\n\
\x20   match opt {\n\
\x20       Some(s) => { if !s.is_empty() { print(\"m\"); } }\n\
\x20       None => { print(\"e\"); }\n\
\x20   }\n\
}\n\
\n\
fn main() {\n\
\x20   run();\n\
}\n";

/// Looped `match`-arm binder for the per-iteration slope probe. Each cycle binds
/// a FRESH `Some("…".to_upper())` heap payload, borrows it (`!s.is_empty()`), and
/// returns 1 so the loop is not dead code. The composite's `EnumInPlace` drop is
/// the sole owner of the payload buffer; if it were wrongly excluded the
/// `to_upper()` buffer leaks one node per iteration.
fn match_bound_loop_source(frames: usize) -> String {
    format!(
        "fn make(n: i64) -> Option<string> {{\n\
         \x20   if n > 0 {{ Some(\"g63-match-heap-payload\".to_upper()) }} else {{ None }}\n\
         }}\n\
         \n\
         fn run_cycle(n: i64) -> i64 {{\n\
         \x20   let opt = make(n);\n\
         \x20   var got: i64 = 0;\n\
         \x20   match opt {{\n\
         \x20       Some(s) => {{ if !s.is_empty() {{ got = 1; }} }}\n\
         \x20       None => {{}}\n\
         \x20   }}\n\
         \x20   got\n\
         }}\n\
         \n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{ total = total + run_cycle(i + 1); }}\n\
         \x20   total\n\
         }}\n\
         \n\
         fn main() -> i64 {{ run_loop({frames}) }}\n"
    )
}

/// Looped `if let` binder over the same owned `Option<string>` heap payload. The
/// `if let` desugars to the same composite-owned destructure; the bound payload
/// must be freed by the scrutinee's `EnumInPlace` drop every iteration.
fn iflet_bound_loop_source(frames: usize) -> String {
    format!(
        "fn make(n: i64) -> Option<string> {{\n\
         \x20   if n > 0 {{ Some(\"g63-iflet-heap-payload\".to_upper()) }} else {{ None }}\n\
         }}\n\
         \n\
         fn run_cycle(n: i64) -> i64 {{\n\
         \x20   let opt = make(n);\n\
         \x20   var got: i64 = 0;\n\
         \x20   if let Some(s) = opt {{\n\
         \x20       if !s.is_empty() {{ got = 1; }}\n\
         \x20   }}\n\
         \x20   got\n\
         }}\n\
         \n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{ total = total + run_cycle(i + 1); }}\n\
         \x20   total\n\
         }}\n\
         \n\
         fn main() -> i64 {{ run_loop({frames}) }}\n"
    )
}

// ── scribble correctness pin ──────────────────────────────────────────────

/// Compile `source`, run it under the poisoned-allocator triple, and assert it
/// exits cleanly with `expected` on stdout. A double-free of the payload buffer
/// (the composite `EnumInPlace` drop firing while a binder release also freed it)
/// aborts here; a use-after-free read garbles the borrow.
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("match-bound-payload-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash here means the \
         match-bound payload was double-freed (the composite `EnumInPlace` drop and a \
         spurious binder release both freed the `to_upper()` buffer);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read the bound payload back verbatim — scribbled/empty output indicates a \
         use-after-free read on the match-bound string;\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Slope guard: a `match`-arm-bound owned `Option<string>` payload looped at LOW
/// vs HIGH iteration counts holds the leak-node count flat — the scrutinee's
/// `EnumInPlace` drop frees the `to_upper()` buffer every iteration. A regression
/// that excluded the composite from its drop grows the count with the iteration
/// count and trips the slope assertion.
#[test]
fn match_bound_owned_payload_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g63_match_bound", match_bound_loop_source);
}

/// Slope guard for the `if let` binder shape: same owned payload bound through
/// `if let Some(s) = opt`, borrow-only, must also hold a flat leak-node slope.
#[test]
fn iflet_bound_owned_payload_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("g63_iflet_bound", iflet_bound_loop_source);
}

/// Double-free / use-after-free guard under the poisoned allocator: the
/// match-bound payload must run clean and read back its borrow. The composite's
/// single `EnumInPlace` drop owns the payload; the binder earns no second release.
#[test]
fn match_bound_owned_payload_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "g63_match_bound_scribble",
        G63_MATCH_BOUND_SCRIBBLE_SOURCE,
        "m",
    );
}
