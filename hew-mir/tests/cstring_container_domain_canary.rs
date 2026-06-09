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
//! Header-awareness is settled, but MIR lowering must not *yet* emit a
//! String-element accessor for any container. Indexing a `vec<string>`
//! (`xs[i]`) still falls through the element-type dispatch in `lower_vec_index`
//! (`hew-mir/src/lower.rs`) to a `NotYetImplemented` diagnostic — it never
//! lowers to `hew_vec_get_str` / `hew_vec_pop_str`. Two reasons keep this guard
//! load-bearing post-P2b-maps:
//!
//! 1. **The P3 retain/drop elaboration is not wired yet.** The typed string
//!    getters now return a *retained* owner (vec) or expose a *borrowed* slot
//!    (`hew_hashmap_get_layout` returns a borrowed, not retained, pointer).
//!    Emitting either before the frontend knows to balance the retain (vec) or
//!    to retain-on-read the borrowed slot (maps) would leak or alias every
//!    accessed element. The VWT bindings (`hew-hir/src/value_class.rs`,
//!    `LayoutWitness::VEC` / `::HASHMAP` / `::HASHSET`) are declarative and not
//!    consumed until P3.
//! 2. **No map String-element accessor exists.** `get_layout` hands back a
//!    BORROWED slot, not a retained owner; emitting a map string getter is a
//!    P3+ change that must first wire retain-on-read, so this guard keeps the
//!    accessor out of lowering until then.
//!
//! This canary fails loudly if a future change starts emitting a String-element
//! accessor for a container index/pop before those conditions are met. It is a
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
/// the runtime-ABI allowlist (so a future P2b slice can wire it) but is not
/// emitted by today's lowering; the String *pop* accessor is not even
/// allowlisted (a stronger separation). If `hew_vec_get_str` were ever removed
/// from the allowlist, the canary below (which asserts it is not emitted) would
/// silently become vacuous — pin its presence so the guard keeps its teeth.
#[test]
fn string_element_vec_accessors_are_allowlisted_but_guarded() {
    assert!(
        is_known_runtime_symbol("hew_vec_get_str"),
        "`hew_vec_get_str` must remain allowlisted; the container-domain canary \
         asserts it is never *emitted*, which is only meaningful while the symbol \
         exists",
    );
    // `hew_vec_pop_str` is intentionally NOT allowlisted in MIR today; the
    // emission guard below covers it regardless of allowlist status.
    assert!(
        !is_known_runtime_symbol("hew_vec_pop_str"),
        "`hew_vec_pop_str` is expected to remain un-allowlisted until the P3 \
         retain/drop elaboration is wired; if it becomes allowlisted, confirm \
         the frontend balances the retained owner it returns and update this \
         canary",
    );
}

/// Indexing a `vec<string>` must NOT lower to `hew_vec_get_str` — it must hit
/// the `NotYetImplemented` element-dispatch arm in `lower_vec_index`. This is
/// the load-bearing guard: post-P2b-vec/P2b-maps every container string element
/// is header-aware, so the danger is no longer heap corruption but that the P3
/// retain/drop balancing is not wired yet — `hew_vec_get_str` returns a
/// *retained* owner (vec) and the map slot accessor exposes a *borrowed* slot
/// (`hew_hashmap_get_layout`), both of which must be balanced by P3 before any
/// String-element accessor may be emitted.
#[test]
fn vec_string_index_never_emits_headerless_string_accessor() {
    let pl = pipeline_with_tc(
        "fn get_first(xs: vec<string>) -> string {
             xs[0]
         }",
    );

    let symbols = emitted_runtime_symbols(&pl);
    assert!(
        !symbols
            .iter()
            .any(|s| s == "hew_vec_get_str" || s == "hew_vec_pop_str"),
        "DOMAIN-SEPARATION VIOLATION: lowering a vec<string> index emitted a \
         String-element accessor {symbols:?}. Post-P2b-vec/P2b-maps every \
         container string element is header-aware, so this no longer corrupts \
         the heap, but `hew_vec_get_str` now returns a *retained* owner — \
         emitting it before the P3 retain/drop elaboration is wired would leak \
         every indexed element, and the same arm covers hashmap/hashset \
         elements whose `get_layout` slot is *borrowed* (not retained). Wire P3 \
         (retain-on-read / drop-balancing) before emitting String-element \
         accessors.",
    );

    assert!(
        pl.diagnostics.iter().any(|d| matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("element type for xs[i]")
        )),
        "expected a NotYetImplemented element-dispatch diagnostic for vec<string> \
         indexing (the gate that keeps container String elements out of \
         hew_string_drop); diagnostics: {:?}",
        pl.diagnostics,
    );
}
