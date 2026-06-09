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
//!    returns a retained owner. Emitting it without an owned binding drop would
//!    leak every accessed element; emitting a drop on a borrowed/aliased value
//!    would double-free. The for-in lowering is therefore fail-closed: it emits
//!    `hew_vec_get_str` only for the synthetic Vec iterator path and pairs the
//!    per-iteration binding with `hew_string_drop`.
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
            println(f"count={count}");
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
        vec!["hew_string_drop".to_string()],
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
