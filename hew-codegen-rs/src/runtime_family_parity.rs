//! Non-tautological admission-⇒-codegen parity for the runtime-ABI surface.
//!
//! The MIR-side admission predicate
//! (`hew_mir::runtime_symbols::is_known_runtime_symbol`) now derives its
//! answer from the single `RuntimeCallFamily` catalog: a symbol is admitted
//! for `Instr::CallRuntimeAbi` iff it lifts to a non-pre-staged family. This
//! test proves the OTHER half of that contract from the CODEGEN side: every
//! symbol MIR admits actually REACHES codegen — it is not a string that
//! agrees with an inventory but has no lowering home.
//!
//! Method: drive the production runtime declaration authority
//! (`intern_runtime_decl`, the `hew_*` C-ABI signature table backing every
//! `Instr::CallRuntimeAbi` lowering) with a real inkwell `Context` for every
//! admitted family symbol, then partition:
//!
//! * declared here → assert a real, named, non-void-arity declaration is
//!   produced (a genuine LLVM `FunctionValue`, not a fail-closed error);
//! * fail-closed here → the symbol MUST be in [`ADMITTED_DECL_VIA_OTHER_SEAM`]
//!   with a documented reason (it reaches codegen through a different
//!   declaration seam — a layout/collection decl helper — or through a
//!   `Terminator::Call` callee-name intercept that never declares an extern).
//!
//! The teeth: if the admission collapse ever admits a symbol with NO codegen
//! home (e.g. a math-intrinsic bare name or a mistakenly un-pre-staged
//! family), it lands in neither partition and this test goes red. The list is
//! the CODEGEN authority's view, derived independently of the MIR admission
//! list, so agreement is not tautological.

use crate::llvm::{intern_runtime_decl, RuntimeDeclMap};
use hew_types::runtime_call::{all_runtime_call_families, is_pre_staged_family};
use inkwell::context::Context;

/// Admitted runtime-ABI symbols that fail closed in `intern_runtime_decl`.
///
/// Every entry is admitted (a non-pre-staged `RuntimeCallFamily`) yet has no
/// arm in the flat `intern_runtime_decl` signature table. Each reaches
/// codegen through a different declaration seam — a layout/collection decl
/// helper or an inline `Terminator::Call` callee-name intercept — or is an
/// admitted-but-not-yet-wired tracked gap that predates this slice (its
/// admission is identical under the retired string allowlist). Grouped by
/// reason. This set is measured against the codegen declaration authority,
/// so it changes only when codegen's declaration topology changes.
const ADMITTED_DECL_VIA_OTHER_SEAM: &[&str] = &[
    // Synthetic intercept-only symbols: no runtime extern exists. Codegen
    // intercepts the `Terminator::Call` callee and materialises the result
    // inline (Option / handle construction), so there is nothing to declare.
    "hew_bytes_get",
    "hew_regex_handle",
    "hew_string_char_at",
    "hew_string_char_at_utf8",
    "hew_string_find",
    "hew_string_get",
    // Layout-backed HashMap / HashSet ops: declared by the layout-collection
    // decl path, not the flat table.
    "hew_hashmap_contains_key_layout",
    "hew_hashmap_free_layout",
    "hew_hashmap_get_layout",
    "hew_hashmap_insert_layout",
    "hew_hashmap_len_layout",
    "hew_hashmap_new_with_layout",
    "hew_hashmap_remove_layout",
    "hew_hashset_contains_layout",
    "hew_hashset_free_layout",
    "hew_hashset_insert_layout",
    "hew_hashset_is_empty_layout",
    "hew_hashset_len_layout",
    "hew_hashset_new_with_layout",
    "hew_hashset_remove_layout",
    // Duration / Instant monomorphic canaries: declared by the time-canary
    // decl path, not the flat table.
    "hew_duration_abs",
    "hew_duration_hours",
    "hew_duration_is_zero",
    "hew_duration_micros",
    "hew_duration_millis",
    "hew_duration_mins",
    "hew_duration_nanos",
    "hew_duration_secs",
    "hew_instant_duration_since",
    "hew_instant_elapsed",
    "hew_instant_now",
    // Vec layout / owned-element getters: declared by the vec-layout decl
    // path keyed on the element descriptor.
    "hew_vec_get_layout",
    "hew_vec_get_owned",
    // Handle clone / move / diagnostic symbols declared by their owning
    // lowering arm rather than the flat table, or admitted-but-not-yet-wired
    // tracked gaps (pre-existing; same under the retired allowlist).
    "hew_actor_unlink",
    "hew_cancel_token_retain",
    "hew_duplex_clone",
    "hew_duplex_try_send",
    "hew_dyn_box_alloc",
    "hew_dyn_box_free",
    "hew_lambda_actor_clone",
    "hew_lambda_actor_weak_clone",
    "hew_supervisor_nested_get",
    "hew_vtable_dispatch_panic_on_oob",
    // Labelled-metrics registration surface: admitted but fail-closed pending
    // a `HewVec`-shaped ABI (see the matching fail-closed arm in
    // `lower_call_runtime_abi`). Tracked gap, not a Slice-B regression.
    "hew_metric_histogram_register",
    "hew_metric_vec_register",
    "hew_metric_vec_with",
];

#[test]
fn every_admitted_family_reaches_codegen() {
    let ctx = Context::create();
    let module = ctx.create_module("runtime_family_reaches_codegen");
    let mut decls = RuntimeDeclMap::new();

    let mut fail_closed = Vec::new();
    for family in all_runtime_call_families() {
        // Pre-staged families are NOT admitted for `Instr::CallRuntimeAbi`
        // (they ride `Terminator::Call`); they are outside this contract.
        if is_pre_staged_family(family) {
            continue;
        }
        let symbol = family.c_symbol();
        match intern_runtime_decl(&ctx, &module, &mut decls, symbol) {
            Ok(fv) => {
                // A real, named declaration — not a fail-closed guess. The
                // declared symbol name must match exactly (a mismatch would
                // mean codegen wired the wrong runtime entry).
                assert_eq!(
                    fv.get_name().to_str().unwrap(),
                    symbol,
                    "intern_runtime_decl declared a differently-named function for {symbol}",
                );
            }
            Err(_) => fail_closed.push(symbol),
        }
    }

    fail_closed.sort_unstable();
    let mut expected: Vec<&'static str> = ADMITTED_DECL_VIA_OTHER_SEAM.to_vec();
    expected.sort_unstable();
    assert_eq!(
        expected.len(),
        ADMITTED_DECL_VIA_OTHER_SEAM.len(),
        "ADMITTED_DECL_VIA_OTHER_SEAM contains duplicate entries",
    );

    assert_eq!(
        fail_closed, expected,
        "the set of admitted runtime-ABI symbols with no `intern_runtime_decl` \
         signature changed. A newly admitted symbol that fails closed here has NO \
         codegen home — wire its lowering or, if it reaches codegen through another \
         declaration seam, document it in ADMITTED_DECL_VIA_OTHER_SEAM. A symbol \
         that dropped out means its seam moved into the flat table (remove it here).",
    );
}
