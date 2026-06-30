//! Interior-alias clone-out leak oracle (hew-lang/hew gap #54).
//!
//! `match rows.get(i) { Some(r) => { let copied = r.name.to_upper(); … } }`
//! over an owned-element `Vec<Row>` exercises THREE shares of the same heap
//! string before the fix:
//!
//!   1. the Vec element's `name` buffer (owned by the Vec, freed by
//!      `hew_vec_free_owned`);
//!   2. `first.row.name` — `rows.get(i)` is `hew_vec_get_clone`, which deep-clones
//!      the owned record and retains its string field (`+1`), so the
//!      `Option<Row>` payload aliases the element buffer;
//!   3. `r.name` loaded out of the match-bound payload — codegen retains the field
//!      load (`retain_string_field_load` → `hew_string_clone`, another `+1`).
//!
//! `r.name.to_upper()` then allocates a FRESH buffer (`copied`), a fourth owner.
//!
//! Pre-fix the interior-alias taint over-excluded the whole `Some(Row)` payload
//! composite from its `EnumInPlace` drop (the destructure binder `r` is seeded
//! interior-alias-tainted even though `first` is a freshly-cloned sole owner), so
//! the payload buffer leaked; the field-load retain temp had no balancing drop and
//! leaked too; and `copied` — a match-arm-scoped fresh owner used only by a borrow
//! — was `Live` at the arm's closing `Goto` but `Uninit` at the post-match join, so
//! the function-exit pass never fired its drop. Net: a per-call leak that scales
//! with the surviving shares.
//!
//! ## What each oracle pins
//!
//! - **Exact contents under the poisoned-allocator triple** (any unix): clone a
//!   heap field out of an aliased Vec element and read the result back. Under
//!   `MallocScribble`/`MallocPreScribble`/`MallocGuardEdges` a double-free aborts
//!   the process and a use-after-free read returns scribbled memory. The exact
//!   string plus the clean exit pin both regression directions — most importantly
//!   that admitting the composite's `EnumInPlace` drop and the field-load retain's
//!   balancing drop did NOT introduce a double-free of the shared buffer.
//!
//! - **Per-iteration leak slope** (macOS-only via `leaks(1)`): the clone-out
//!   cycle looped at a LOW and a HIGH iteration count must hold the leak-node
//!   count flat (within tolerance). The delta cancels the nondeterministic
//!   constant baseline an exact `== 0` count cannot. Pre-fix this leaked the
//!   payload buffer, the field-load retain, and `copied` — three nodes per
//!   iteration of positive slope.
//!
//! - **True-interior-alias negative control** (double-free guard + flat slope): a
//!   Vec element bound through the BORROW getter (`rows[0]` → `hew_vec_get_owned`,
//!   an interior pointer into the still-live Vec's element slot) and only borrowed
//!   must stay leak-clean AND not double-free — the Vec's `hew_vec_free_owned`
//!   owns the element, so the interior-alias taint must keep the binding excluded
//!   from any independent release.
//!
//! ## Skip behaviour
//!
//! The slope oracle is macOS-only (`leaks(1)` is Darwin's allocator inspector);
//! elsewhere it logs `skip:` and returns. The scribble correctness pins run on
//! any unix host.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

// ── fixtures ──────────────────────────────────────────────────────────────

/// Clone a heap `string` field out of an aliased `Vec<Row>` element and read the
/// uppercased result back. Under the poisoned allocator a double-free of the
/// shared element buffer aborts; a UAF read returns scribbled memory (not the
/// expected uppercased string). The exact-string equality plus clean exit pin
/// both directions.
const INTERIOR_CLONE_CONTENTS_SOURCE: &str = "\
type Row { name: string; }\n\
\n\
fn run() {\n\
\x20   let rows: Vec<Row> = [ Row { name: \"interior-clone-ok\".to_upper() } ];\n\
\x20   let first = rows.get(0);\n\
\x20   match first {\n\
\x20       Some(r) => { let copied = r.name.to_upper(); print(copied); }\n\
\x20       None => { print(\"none\"); }\n\
\x20   }\n\
}\n\
\n\
fn main() {\n\
\x20   run();\n\
}\n";

/// Expected exact output for `INTERIOR_CLONE_CONTENTS_SOURCE`. Any double-free
/// abort or UAF read changes this.
const INTERIOR_CLONE_CONTENTS_EXPECTED: &str = "INTERIOR-CLONE-OK";

/// True-interior-alias NEGATIVE control (the hard double-free guard): bind a Vec
/// element through the BORROW getter (`rows[0]` → `hew_vec_get_owned`, which
/// returns an interior pointer into the still-live Vec's element slot, NOT a fresh
/// owner) and only BORROW its field. The element is owned by the Vec; its
/// `hew_vec_free_owned` runs the per-element record drop. Admitting `r` to its own
/// `RecordInPlace` — or the composite to an `EnumInPlace` over a borrowed payload —
/// would free the same buffer twice. The interior-alias taint
/// (`compute_collection_interior_alias_taint`) must keep `r` excluded.
const INTERIOR_ALIAS_NEG_CONTROL_SOURCE: &str = "\
type Row { name: string; }\n\
\n\
fn run() {\n\
\x20   let rows: Vec<Row> = [ Row { name: \"neg-alias-heap-name\".to_upper() } ];\n\
\x20   let r = rows[0];\n\
\x20   if r.name.len() > 0 { print(\"nz\"); }\n\
}\n\
\n\
fn main() {\n\
\x20   run();\n\
}\n";

/// Looped clone-out cycle for the per-iteration slope probe. Each cycle builds a
/// FRESH single-element `Vec<Row>` with a heap `name`, clones the field out of
/// the `rows.get(0)` payload (`r.name.to_upper()`), and returns its length so the
/// loop is not dead code. The payload composite, the field-load retain, and the
/// arm-scoped `copied` owner must each be released every iteration; any stranded
/// share leaks one node per iteration.
fn interior_clone_loop_source(frames: usize) -> String {
    format!(
        "type Row {{ name: string; }}\n\
         \n\
         fn run_cycle() -> i64 {{\n\
         \x20   let rows: Vec<Row> = [ Row {{ name: \"interior-clone-heap-name\".to_upper() }} ];\n\
         \x20   let first = rows.get(0);\n\
         \x20   var got: i64 = 0;\n\
         \x20   match first {{\n\
         \x20       Some(r) => {{ let copied = r.name.to_upper(); if copied.len() > 0 {{ got = copied.len(); }} }}\n\
         \x20       None => {{}}\n\
         \x20   }}\n\
         \x20   got\n\
         }}\n\
         \n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{ total = total + run_cycle(); }}\n\
         \x20   total\n\
         }}\n\
         \n\
         fn main() -> i64 {{ run_loop({frames}) }}\n"
    )
}

/// Looped true-interior-alias negative control for the slope probe. Each cycle
/// builds a FRESH single-element `Vec<Row>`, rebinds the element through the
/// BORROW getter (`rows[0]`), and borrows its field. The Vec's `hew_vec_free_owned`
/// is the single owner; a flat slope proves the interior alias earns no
/// independent per-iteration release (no leak, no double-free).
fn interior_neg_control_loop_source(frames: usize) -> String {
    format!(
        "type Row {{ name: string; }}\n\
         \n\
         fn run_cycle() -> i64 {{\n\
         \x20   let rows: Vec<Row> = [ Row {{ name: \"neg-alias-heap-name\".to_upper() }} ];\n\
         \x20   let r = rows[0];\n\
         \x20   var got: i64 = 0;\n\
         \x20   if r.name.len() > 0 {{ got = r.name.len(); }}\n\
         \x20   got\n\
         }}\n\
         \n\
         fn run_loop(frames: i64) -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for i in 0..frames {{ total = total + run_cycle(); }}\n\
         \x20   total\n\
         }}\n\
         \n\
         fn main() -> i64 {{ run_loop({frames}) }}\n"
    )
}

// ── scribble correctness pin ──────────────────────────────────────────────

/// Compile `source`, run it under the poisoned-allocator triple, and assert it
/// exits cleanly with `expected` on stdout.
fn assert_exact_under_malloc_scribble(name: &str, source: &str, expected: &str) {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix(&format!("interior-alias-clone-{name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "{name} must run clean under the poisoned allocator — a crash here indicates a \
         double-free of the shared `Row.name` buffer (the composite `EnumInPlace` drop and a \
         field-load/alias release both freed it);\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout,
        expected,
        "{name} must read the cloned value back verbatim — scribbled/empty output indicates a \
         use-after-free read on the cloned-out string;\n{}",
        describe_output(&output)
    );
}

// ── oracles ─────────────────────────────────────────────────────────────────

/// Exact-contents pin: clone a heap field out of an aliased Vec element and read
/// it back. Must print `"INTERIOR-CLONE-OK"` and exit clean under the poisoned
/// allocator. A regression that double-frees the shared buffer aborts; a UAF read
/// garbles the output.
#[test]
fn interior_alias_clone_exact_contents_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "interior_clone_contents",
        INTERIOR_CLONE_CONTENTS_SOURCE,
        INTERIOR_CLONE_CONTENTS_EXPECTED,
    );
}

/// Slope oracle: the clone-out cycle looped at LOW vs HIGH iteration counts holds
/// the leak-node count flat. Pre-fix this leaked the payload composite, the
/// field-load retain, and the arm-scoped `copied` string — a positive slope of
/// three nodes per iteration.
#[test]
fn interior_alias_clone_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("interior_clone", interior_clone_loop_source);
}

/// Negative control under the poisoned allocator: a true interior-alias rebind
/// with NO clone-out must stay clean (no double-free). The composite's single
/// `EnumInPlace` drop owns the payload; the alias earns no second release.
#[test]
fn interior_alias_true_alias_no_double_free_under_malloc_scribble() {
    assert_exact_under_malloc_scribble(
        "interior_alias_neg_control",
        INTERIOR_ALIAS_NEG_CONTROL_SOURCE,
        "nz",
    );
}

/// Slope oracle for the negative control: a true interior-alias rebind must also
/// hold a flat leak-node slope (the Vec's free is the single owner — no leak, no
/// double-free).
#[test]
fn interior_alias_true_alias_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance(
        "interior_alias_neg_control",
        interior_neg_control_loop_source,
    );
}
