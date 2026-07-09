//! Domain-separation canary for the C-string allocator migration (W5.011
//! P1.5b → P2b).
//!
//! ## Why this test exists
//!
//! The header-aware C-string allocator wiring (P1.5b-S1) makes `hew_string_drop`
//! — the universal `String` consumer — header-aware (`free_cstring`, which reads
//! `data - 16`). At P1.5b, *all* container string elements
//! (`vec`/`hashmap`/`hashset`) were produced by bare `libc::strdup` and were
//! **headerless**; a headerless element reaching the header-aware
//! `hew_string_drop` would make `free_cstring` read `data - 16` out of bounds —
//! silent heap corruption (CLAUDE.md §1, no-silent-corruption).
//!
//! **That headerless-corruption danger is now fully retired for every string
//! container.** W5.011 P2b-vec made `vec<string>` elements header-aware and
//! refcounted; W5.011 P2b-maps validated the same discipline for
//! `hashmap`/`hashset`: codegen wires their string keys/values through the
//! header-aware String-ownership descriptors (`hew_layout_key_string` /
//! `hew_layout_val_string`), insert is an ownership-transfer MOVE of an
//! already-header-aware string (no `strdup`, no headerless map producer), clone
//! retains via `hew_string_clone`, and free/remove release via the descriptor
//! drop thunk (`hew_string_drop`, free-at-zero). So a `vec`/`hashmap`/`hashset`
//! string element reaching `hew_string_drop` is now safe for all three domains.
//!
//! ## What this canary still guards
//!
//! Header-awareness is settled, but MIR lowering must only emit a
//! String-element accessor when it also balances that accessor's ownership
//! contract. `hew_vec_get_str` returns a retained/header-aware owner; a lowered
//! `for word in words` binding must therefore be followed by exactly one
//! `hew_string_drop` after the loop body uses `word`. Two reasons keep this
//! guard load-bearing post-P2b-maps:
//!
//! 1. **The vec getter retain must be drop-balanced.** The typed string getter
//!    returns a retained owner. Emitting it without a balancing release would
//!    leak every accessed element; emitting a drop on a borrowed/aliased value
//!    would double-free. The emitters are fail-closed per position:
//!    - **for-in** pairs the per-iteration binding with one `hew_string_drop`
//!      (escape-scanned: suppressed when the element is moved out of the body).
//!    - **scalar `xs[i]` in DISCARD, BOUND, and NESTED positions** all route
//!      through the same retained getter and are released exactly once by the
//!      general owned-`string` temporary substrate:
//!      `derive_cow_fresh_borrowed_owner` emits a scope-exit `CowHeap` drop for
//!      the bound `let y = xs[i]; y.len()`, and `apply_nested_fresh_string_temp_drops`
//!      splices an inline drop for `xs[i].len()` and the discarded `xs[i];`. The
//!      exactly-once DROP BALANCE for those index shapes is proven in
//!      `owned_string_temp_drop_canary.rs` (`index_*`); this file pins the
//!      COMPLEMENTARY container-domain fact — that the index paths keep selecting
//!      the retained getter `hew_vec_get_str`, never the borrow getter
//!      `hew_vec_get_owned` (which would double-free against the substrate's
//!      release).
//! 2. **No map String-element accessor exists.** `get_layout` hands back a
//!    BORROWED slot, not a retained owner; emitting a map string getter is a
//!    P3+ change that must first wire retain-on-read, so this guard keeps the
//!    accessor out of lowering until then.
//!
//! This canary fails loudly if a future change emits a String-element accessor
//! for a container path without the corresponding ownership balancing. It is a
//! compile-time / unit guard with **zero release-path behaviour change** — it
//! only inspects lowered MIR, it does not alter it.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::runtime_symbols::is_known_runtime_symbol;
use hew_mir::{lower_hir_module, Instr, IrPipeline, MirDiagnosticKind};
use hew_types::{module_registry::ModuleRegistry, Checker};

/// Lower a Hew source string through the full type-checked HIR→MIR pipeline.
fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

/// Collect every `CallRuntimeAbi` symbol emitted in the pipeline's raw MIR.
fn emitted_runtime_symbols(pl: &IrPipeline) -> Vec<String> {
    let mut out = Vec::new();
    for f in &pl.raw_mir {
        for b in &f.blocks {
            for instr in &b.instructions {
                if let Instr::CallRuntimeAbi(call) = instr {
                    out.push(call.symbol().to_string());
                }
            }
        }
    }
    out
}

/// Premise of the domain separation: the String-element vec *getter* exists in
/// the runtime-ABI allowlist and may be emitted only when the retained owner is
/// balanced; the String *pop* accessor is not even allowlisted (a stronger
/// separation). Pin both facts so the guard keeps its teeth.
#[test]
fn string_element_vec_accessors_are_allowlisted_but_guarded() {
    assert!(
        is_known_runtime_symbol("hew_vec_get_str"),
        "`hew_vec_get_str` must remain allowlisted; the container-domain canary \
         asserts emitted uses are balanced, which is only meaningful while the \
         symbol exists",
    );
    // `hew_vec_pop_str` is intentionally NOT allowlisted in MIR today; the
    // emission guard below covers it regardless of allowlist status.
    assert!(
        !is_known_runtime_symbol("hew_vec_pop_str"),
        "`hew_vec_pop_str` is expected to remain un-allowlisted until retained \
         pop ownership is wired; if it becomes allowlisted, confirm the \
         frontend balances the retained owner it returns and update this canary",
    );
}

/// `for word in words` over `Vec<string>` may lower to `hew_vec_get_str`, but
/// only if the retained per-iteration owner is balanced with exactly one
/// `hew_string_drop` after the body uses `word`.
#[test]
fn vec_string_for_in_emits_retained_getter_with_iteration_drop() {
    // `println(count)` (a direct scalar print) rather than an f-string: the
    // fixture's subject is the for-in retained-getter drop, and an
    // interpolated `f"count={count}"` tail would add its OWN
    // `hew_string_drop`s (the fresh-`string`-temp release seam, W5.011 P3 /
    // the f-string interpolation temp fix) into `string_drops` below,
    // conflating two unrelated things
    // this test is not about.
    let pl = pipeline_with_tc(
        r#"fn main() {
            let words: Vec<string> = Vec::new();
            words.push("red");
            words.push("blue");
            var count: i64 = 0;
            for word in words {
                println(word);
                count = count + 1;
            }
            println(count);
        }"#,
    );

    let symbols = emitted_runtime_symbols(&pl);
    assert!(
        symbols.iter().any(|s| s == "hew_vec_get_str"),
        "Vec<string> for-in must lower through the real retained getter, not \
        a fake-green literal/range/get workaround; symbols: {symbols:?}",
    );
    assert!(
        !symbols.iter().any(|s| s == "hew_vec_pop_str"),
        "Vec<string> for-in must not route through the unallowlisted pop accessor; \
        symbols: {symbols:?}",
    );

    let mut string_drops = Vec::new();
    for f in &pl.raw_mir {
        for b in &f.blocks {
            for instr in &b.instructions {
                if let Instr::Drop {
                    ty: hew_types::ResolvedTy::String,
                    drop_fn: Some(drop_fn),
                    ..
                } = instr
                {
                    string_drops.push(drop_fn.clone());
                }
            }
        }
    }
    assert_eq!(
        string_drops,
        vec![hew_mir::DropFnSpec::Release("hew_string_drop")],
        "hew_vec_get_str returns a retained owner; the for-in word binding must \
        receive exactly one explicit hew_string_drop after the body, not leak \
        or double-drop. Full drop list: {string_drops:?}; diagnostics: {:?}",
        pl.diagnostics,
    );

    assert!(
        !pl.diagnostics
            .iter()
            .any(|d| matches!(&d.kind, MirDiagnosticKind::NotYetImplemented { .. })),
        "Vec<string> for-in must not trip a MIR NotYetImplemented gate; diagnostics: {:?}",
        pl.diagnostics,
    );
}

/// Count every `hew_string_drop` `Instr::Drop` across the pipeline's raw MIR.
fn string_drop_count(pl: &IrPipeline) -> usize {
    let mut n = 0;
    for f in &pl.raw_mir {
        // Each canary measures the drop balance of the construct under test,
        // which always lives in a named helper fn (`transform`, `keep`, `pick`,
        // `count`, …). `main` is pure test scaffolding that calls the helper and
        // prints the result. A harness that wraps an `i64`-returning helper in a
        // bare single-interpolation f-string — `println(f"{count(v)}")` — now
        // earns its own legitimate inline `hew_string_drop` for the
        // `to_string_i64` result (the f-string interpolation leak fix). That
        // harness-side drop is correct but unrelated to the construct under test,
        // so counting it pipeline-wide would conflate two independent ownership
        // balances. Scope the count to the helper functions by skipping `main`.
        if f.name == "main" {
            continue;
        }
        for b in &f.blocks {
            for instr in &b.instructions {
                if let Instr::Drop {
                    ty: hew_types::ResolvedTy::String,
                    drop_fn: Some(drop_fn),
                    ..
                } = instr
                {
                    if *drop_fn == hew_mir::DropFnSpec::Release("hew_string_drop") {
                        n += 1;
                    }
                }
            }
        }
    }
    n
}

fn has_nyi(pl: &IrPipeline) -> bool {
    pl.diagnostics
        .iter()
        .any(|d| matches!(&d.kind, MirDiagnosticKind::NotYetImplemented { .. }))
}

/// `hew_vec_get_str` must be the element fetch (proves the real retained-getter
/// for-in path lowered, not a fake-green workaround).
fn emits_vec_get_str(pl: &IrPipeline) -> bool {
    emitted_runtime_symbols(pl)
        .iter()
        .any(|s| s == "hew_vec_get_str")
}

/// A BRANCHED body (`if/else`, both arms reading the binding by-value via string
/// concat) lowers without an NYI and places the post-body retained-element drop
/// at the branch-join block reached once per iteration regardless of which arm
/// ran. The then arm also has the #2434 nested-concat shape
/// `out + "P:" + line`: the intermediate concat temp is borrowed by the second
/// concat and now earns its own inline `hew_string_drop`.
#[test]
fn vec_string_for_in_branched_body_drops_once_at_join() {
    let pl = pipeline_with_tc(
        r#"fn transform(lines: Vec<string>) -> string {
            var out = "";
            for line in lines {
                if line.len() > 0 { out = out + "P:" + line; }
                else { out = out + "."; }
            }
            out
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            println(transform(v));
        }"#,
    );
    assert!(!has_nyi(&pl), "branched body NYI: {:?}", pl.diagnostics);
    assert!(
        emits_vec_get_str(&pl),
        "branched body must use the retained getter"
    );
    assert_eq!(
        string_drop_count(&pl),
        4,
        "branched body drops the retained `line` once at the branch-join PLUS \
         the prior `out` value on each arm's `out = out + ...` reassignment \
         (#53 var-overwrite-release) PLUS the then-arm nested-concat \
         intermediate: two overwrite releases + one nested-temp release + the \
         single post-body element drop",
    );
}

/// A `continue` inside the body must free the current iteration's retained
/// element on the continue edge AND on the fall-through edge — two distinct,
/// mutually-exclusive CFG drop sites (the back-edge drop is a no-op via
/// null-after-free on whichever path did not execute). Without the edge drop the
/// continued iteration's element would leak; without the fall-through drop the
/// non-continued iterations would leak.
#[test]
fn vec_string_for_in_continue_drops_on_edge_and_fallthrough() {
    let pl = pipeline_with_tc(
        r#"fn keep(lines: Vec<string>) -> string {
            var out = "";
            for line in lines {
                if line.len() == 0 { continue; }
                out = out + line;
            }
            out
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            println(keep(v));
        }"#,
    );
    assert!(!has_nyi(&pl), "continue body NYI: {:?}", pl.diagnostics);
    assert!(emits_vec_get_str(&pl));
    assert_eq!(
        string_drop_count(&pl),
        3,
        "continue body emits the continue-edge drop and the fall-through \
         body-end drop (one per mutually-exclusive iteration-exit path) PLUS \
         the prior `out` value released on `out = out + line` (#53 \
         var-overwrite-release)",
    );
}

/// A `break` inside the body must free the current iteration's retained element
/// on the break edge (before the loop-exit goto) AND on the fall-through edge.
#[test]
fn vec_string_for_in_break_drops_on_edge_and_fallthrough() {
    let pl = pipeline_with_tc(
        r#"fn until(lines: Vec<string>) -> string {
            var out = "";
            for line in lines {
                if line == "STOP" { break; }
                out = out + line;
            }
            out
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            println(until(v));
        }"#,
    );
    assert!(!has_nyi(&pl), "break body NYI: {:?}", pl.diagnostics);
    assert!(emits_vec_get_str(&pl));
    assert_eq!(
        string_drop_count(&pl),
        3,
        "break body emits the break-edge drop and the fall-through body-end \
         drop (one per mutually-exclusive iteration-exit path) PLUS the prior \
         `out` value released on `out = out + line` (#53 var-overwrite-release)",
    );
}

/// An empty-effect body (the binding is bound but never read) still lowers
/// without an NYI and drops the retained element exactly once — the element is
/// retained by the getter on every iteration, so it must be released even when
/// unused. Regression for the already-passing unused-binding shape.
#[test]
fn vec_string_for_in_unused_binding_drops_once() {
    let pl = pipeline_with_tc(
        r#"fn count(lines: Vec<string>) -> i64 {
            var n = 0;
            for line in lines {
                n = n + 1;
            }
            n
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            println(f"{count(v)}");
        }"#,
    );
    assert!(
        !has_nyi(&pl),
        "unused-binding body NYI: {:?}",
        pl.diagnostics
    );
    assert!(emits_vec_get_str(&pl));
    assert_eq!(
        string_drop_count(&pl),
        1,
        "unused-binding body must still drop the retained element once \
         (the getter retains on every iteration regardless of use)",
    );
}

/// NEGATIVE / ownership-escape: a body that `return`s the binding genuinely
/// moves the single retained reference to the caller. The body-end drop is
/// SUPPRESSED on every path (leak-not-double-free), so NO `hew_string_drop` is
/// emitted for the iteration binding — emitting one would over-release a
/// reference the caller now owns. This proves the escape suppression fires
/// rather than blindly dropping. (It does NOT trip an NYI — an escaping element
/// is a legitimate program shape, matching the generator-yield posture.)
#[test]
fn vec_string_for_in_returned_binding_suppresses_drop() {
    let pl = pipeline_with_tc(
        r#"fn first_match(lines: Vec<string>, needle: string) -> string {
            for line in lines {
                if line == needle { return line; }
            }
            "none"
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            println(first_match(v, "x"));
        }"#,
    );
    assert!(
        !has_nyi(&pl),
        "an escaping (returned) element is a legitimate shape, not an NYI: {:?}",
        pl.diagnostics,
    );
    assert!(emits_vec_get_str(&pl), "the getter still lowers");
    assert_eq!(
        string_drop_count(&pl),
        0,
        "the returned binding's single retained reference escapes to the caller; \
         the body-end drop must be suppressed (leak-not-double-free), so no \
         hew_string_drop is emitted for the iteration binding",
    );
}

/// SCALAR INDEX, BOUND form: `let y = xs[i]` over `Vec<string>` lowers through
/// the SAME retained `hew_vec_get_str` getter the for-in, discard, and nested
/// paths use — NOT the borrow getter `hew_vec_get_owned`. `hew_vec_get_str`
/// returns a fresh refcount-bumped owner (`retain_string_element`) while the Vec
/// keeps its own reference, so indexing does NOT move out of the Vec and the
/// program may keep using the Vec afterwards.
///
/// This canary pins GETTER SELECTION for the bound path: routing String scalar
/// index to `hew_vec_get_owned` (a borrow) would double-free against the
/// retained owner's release. The bound owner's exactly-once release is the
/// general owned-`string` temporary substrate's BOUND path
/// (`derive_cow_fresh_borrowed_owner` → one scope-exit `CowHeap` drop), proven in
/// `owned_string_temp_drop_canary.rs::index_bound_releases_exactly_once`; this
/// canary deliberately scopes itself to the raw-MIR getter selection.
#[test]
fn vec_string_scalar_index_bound_lowers_through_retained_getter() {
    let pl = pipeline_with_tc(
        r"fn pick(xs: Vec<string>, i: i64) -> i64 {
            let y = xs[i];
            y.len()
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            let _ = pick(v, 0);
        }",
    );

    assert!(
        !has_nyi(&pl),
        "scalar Vec<string> index must not trip a MIR NotYetImplemented gate; diagnostics: {:?}",
        pl.diagnostics,
    );
    assert!(
        emits_vec_get_str(&pl),
        "Vec<string> scalar index must lower through the real retained getter \
         (hew_vec_get_str), not a borrow/pop/range workaround; symbols: {:?}",
        emitted_runtime_symbols(&pl),
    );
}

/// SCALAR INDEX, DISCARD form (the security-review follow-on): a discarded
/// `Vec<string>` index — the bare statement `xs[i];`, and the `let _ = xs[i];`
/// form HIR desugars to it — emits the retained `hew_vec_get_str` getter into a
/// temporary that is never bound, read, or aliased. Left alone that retained
/// owner leaks one reference per evaluation (one per loop iteration). The general
/// owned-`string` temporary substrate's DISCARD path
/// (`apply_nested_fresh_string_temp_drops`) splices exactly one inline
/// `hew_string_drop` after the getter. (This subsumed the Vec-specific
/// `release_discarded_vec_string_index` helper the lane carried before rebasing
/// onto the substrate, which the lane removed to avoid a double drop.)
#[test]
fn vec_string_discarded_scalar_index_emits_balancing_drop() {
    let pl = pipeline_with_tc(
        r"fn touch(xs: Vec<string>, i: i64) {
            xs[i];
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            touch(v, 0);
        }",
    );

    assert!(
        !has_nyi(&pl),
        "discarded Vec<string> index must not trip a MIR NotYetImplemented gate; diagnostics: {:?}",
        pl.diagnostics,
    );
    assert!(
        emits_vec_get_str(&pl),
        "discarded Vec<string> index must lower through the real retained getter; symbols: {:?}",
        emitted_runtime_symbols(&pl),
    );
    let drops = string_drop_count(&pl);
    assert_eq!(
        drops, 1,
        "a discarded Vec<string> index leaks the hew_vec_get_str retain unless it \
         is balanced by exactly one inline hew_string_drop; got {drops}; diagnostics: {:?}",
        pl.diagnostics,
    );
}

/// DISCARD in a LOOP — the case the security review flagged as the worst leak:
/// the substrate's inline drop lands inside the loop body block, so it fires
/// once per iteration at runtime. Raw MIR holds exactly ONE `Instr::Drop` site
/// (drops are per-static-site, not per-iteration), proving the per-iteration
/// leak is closed without a second, double-freeing drop.
#[test]
fn vec_string_discarded_scalar_index_in_loop_emits_one_drop_site() {
    let pl = pipeline_with_tc(
        r"fn churn(xs: Vec<string>, n: i64) {
            for i in 0 .. n {
                xs[i];
            }
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            churn(v, 0);
        }",
    );

    assert!(
        emits_vec_get_str(&pl),
        "looped discarded Vec<string> index must lower through the real retained getter; symbols: {:?}",
        emitted_runtime_symbols(&pl),
    );
    let drops = string_drop_count(&pl);
    assert_eq!(
        drops, 1,
        "a looped discarded Vec<string> index must emit exactly one in-body \
         hew_string_drop site (fires per iteration), not zero (leak) or two \
         (double-free); got {drops}; diagnostics: {:?}",
        pl.diagnostics,
    );
}

/// `let _ = xs[i];` is the explicit-discard spelling; HIR desugars it to the
/// same discarded expression statement as `xs[i];`, so it must be balanced
/// identically — a single inline `hew_string_drop`.
#[test]
fn vec_string_let_underscore_index_emits_balancing_drop() {
    let pl = pipeline_with_tc(
        r"fn touch(xs: Vec<string>, i: i64) {
            let _ = xs[i];
        }
        fn main() {
            let v: Vec<string> = Vec::new();
            touch(v, 0);
        }",
    );

    assert!(
        emits_vec_get_str(&pl),
        "`let _ = xs[i]` must lower through the real retained getter; symbols: {:?}",
        emitted_runtime_symbols(&pl),
    );
    let drops = string_drop_count(&pl);
    assert_eq!(
        drops, 1,
        "`let _ = xs[i]` is a discard and must be balanced by exactly one inline \
         hew_string_drop; got {drops}; diagnostics: {:?}",
        pl.diagnostics,
    );
}

/// NEGATIVE CONTROL: only the `String` element getter returns a fresh owner.
/// A discarded `Vec<i64>` index borrows / bit-copies a scalar — no retained
/// owner, so no `hew_string_drop` is owed. This pins that the discard-release
/// fires for `String` elements ALONE; a spurious drop on a scalar element would
/// be a type-confused free.
#[test]
fn vec_scalar_discarded_index_emits_no_string_drop() {
    let pl = pipeline_with_tc(
        r"fn touch(xs: Vec<i64>, i: i64) {
            xs[i];
        }
        fn main() {
            let v: Vec<i64> = Vec::new();
            touch(v, 0);
        }",
    );

    assert!(
        !has_nyi(&pl),
        "discarded Vec<i64> index must not trip a MIR NotYetImplemented gate; diagnostics: {:?}",
        pl.diagnostics,
    );
    let drops = string_drop_count(&pl);
    assert_eq!(
        drops, 0,
        "a discarded Vec<i64> index returns no retained string owner and must emit \
         no hew_string_drop; got {drops}; diagnostics: {:?}",
        pl.diagnostics,
    );
}
