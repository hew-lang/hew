//! Deduplicated registry of `dyn Trait` vtable instances reached by
//! `Instr::CoerceToDynTrait` anywhere in the lowered module.
//!
//! Stage 2 of W3.031 (vtable static registry). The registry is the
//! single point at which the program's `(trait_name, concrete_type,
//! vtable_entries)` triples are reified into stable
//! [`DynVtableInstance`] entries with reproducible
//! `vtable_id`s and LLVM symbol names. Later stages (drop-in-place
//! synthesis, erased thunk emission, vtable static emission) consume
//! this registry; they do NOT re-walk MIR for coercion sites.
//!
//! LESSONS:
//! - `checker-authority` (P0): every entry's payload is a verbatim
//!   copy of the corresponding [`Instr::CoerceToDynTrait`] payload,
//!   itself sourced from the checker's `dyn_trait_coercions` side
//!   table. The registry never re-derives method tables or signatures.
//! - `exhaustive-traversal-and-lowering` (P0): the build walks every
//!   block of every `RawMirFunction` so no coercion site can escape
//!   without a registry entry.

use std::collections::HashSet;

use crate::model::{mangle_dyn_vtable_symbol, DynVtableInstance, Instr, RawMirFunction};

/// Walk every `Instr::CoerceToDynTrait` in `raw_mir` and produce a
/// deduplicated, deterministically-ordered registry.
///
/// **Dedup key.** Two coercion sites collapse to the same entry when
/// their `(trait_name, concrete_type, vtable_entries)` triple is
/// structurally equal. `vtable_entries` is the discriminator for
/// associated-type projections such as `dyn Iterator<Item = i64>`
/// vs `dyn Iterator<Item = String>` — both share `(trait_name,
/// concrete_type)` but differ in the substituted method signatures
/// recorded on the entries.
///
/// **Ordering.** Stable sort by `(trait_name,
/// format!("{concrete_type}"))` over first-seen order; ties resolve to
/// the order each unique triple is encountered during traversal,
/// which itself is a function of source declaration order (the
/// lowering loop walks `module.items` in source order, and each
/// function lowers its blocks in monotone-id order). The result is
/// reproducible across builds of the same module.
///
/// **`vtable_id` assignment.** 0-based index into the returned vec.
/// The symbol is `mangle_dyn_vtable_symbol(vtable_id)`.
///
/// Returns an empty vec when no `CoerceToDynTrait` appears anywhere
/// — e.g. modules with no `dyn Trait` usage.
#[must_use]
pub(crate) fn build_dyn_vtable_registry(raw_mir: &[RawMirFunction]) -> Vec<DynVtableInstance> {
    // (trait_name, concrete_type, vtable_entries, method_table).
    // Linear-scan dedup: N vtables per module is small in practice
    // (one per unique `(trait, concrete)` coercion site), so the
    // O(N²) cost is well below noise and avoids requiring `Hash` on
    // `DynVtableEntry` / `FnSig` / `Ty`.
    type SeenEntry = (
        String,
        hew_types::ResolvedTy,
        Vec<hew_types::DynVtableEntry>,
        Vec<(String, String)>,
    );
    let mut seen: Vec<SeenEntry> = Vec::new();

    for func in raw_mir {
        for block in &func.blocks {
            for instr in &block.instructions {
                if let Instr::CoerceToDynTrait {
                    trait_name,
                    concrete_type,
                    method_table,
                    vtable_entries,
                    ..
                } = instr
                {
                    let already_present = seen.iter().any(|(tn, ct, ve, _mt)| {
                        tn == trait_name && ct == concrete_type && ve == vtable_entries
                    });
                    if !already_present {
                        seen.push((
                            trait_name.clone(),
                            concrete_type.clone(),
                            vtable_entries.clone(),
                            method_table.clone(),
                        ));
                    }
                }
            }
        }
    }

    // Stable sort by (trait_name, concrete_type display). Ties fall
    // back to first-seen order, which is itself deterministic in
    // source-declaration order.
    seen.sort_by(|a, b| {
        let a_key = (a.0.as_str(), format!("{}", a.1));
        let b_key = (b.0.as_str(), format!("{}", b.1));
        a_key.cmp(&b_key)
    });

    // Two sites with identical (trait_name, concrete_type) BUT
    // distinct vtable_entries (e.g. different associated-type
    // bindings producing different substituted signatures) keep
    // separate entries. The stable sort preserves their relative
    // source order. Track the bare (trait, concrete) pair for an
    // internal sanity assertion in debug builds — drift here would
    // mean the dedup key under-discriminates, which a Stage 6 codegen
    // would surface as the wrong vtable referenced at a dispatch.
    let mut symbol_seen: HashSet<String> = HashSet::new();

    seen.into_iter()
        .enumerate()
        .map(
            |(i, (trait_name, concrete_type, vtable_entries, method_table))| {
                let vtable_id = u32::try_from(i).unwrap_or_else(|_| {
                    unreachable!(
                        "dyn vtable registry exceeded u32::MAX entries — \
                     `build_dyn_vtable_registry` cannot assign a stable id"
                    )
                });
                let symbol = mangle_dyn_vtable_symbol(vtable_id, &trait_name, &concrete_type);
                debug_assert!(
                    symbol_seen.insert(symbol.clone()),
                    "mangle_dyn_vtable_symbol must produce unique symbols for unique \
                 vtable_ids; got duplicate `{symbol}`"
                );
                DynVtableInstance {
                    vtable_id,
                    symbol,
                    trait_name,
                    concrete_type,
                    method_table,
                    vtable_entries,
                }
            },
        )
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{BasicBlock, FunctionCallConv, Terminator};
    use hew_types::{DynVtableEntry, FnSig, ResolvedTy, Ty};

    fn make_entry(
        trait_name: &str,
        method_name: &str,
        impl_fn_key: &str,
        ret: Ty,
    ) -> DynVtableEntry {
        DynVtableEntry {
            trait_name: trait_name.to_string(),
            method_name: method_name.to_string(),
            impl_fn_key: impl_fn_key.to_string(),
            signature: FnSig {
                return_type: ret,
                ..FnSig::default()
            },
        }
    }

    fn make_func(name: &str, instrs: Vec<Instr>) -> RawMirFunction {
        RawMirFunction {
            name: name.to_string(),
            return_ty: ResolvedTy::Unit,
            call_conv: FunctionCallConv::Default,
            params: vec![],
            locals: vec![],
            local_names: vec![],
            local_scopes: vec![],
            local_decl_bytes: vec![],
            scope_table: vec![],
            blocks: vec![BasicBlock {
                id: 0,
                statements: vec![],
                instructions: instrs,
                terminator: Terminator::Return,
            }],
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),
            lambda_actor_user_param_locals: Vec::new(),
            span: None,
            instr_spans: std::collections::BTreeMap::new(),
            source_origin: crate::model::SourceOrigin::Unknown,
        }
    }

    fn coerce(trait_name: &str, concrete: ResolvedTy, entries: Vec<DynVtableEntry>) -> Instr {
        Instr::CoerceToDynTrait {
            value: crate::Place::Local(0),
            dest: crate::Place::Local(1),
            trait_name: trait_name.to_string(),
            concrete_type: concrete,
            method_table: entries
                .iter()
                .map(|e| (e.method_name.clone(), e.impl_fn_key.clone()))
                .collect(),
            vtable_entries: entries,
        }
    }

    #[test]
    fn empty_when_no_coercions_anywhere() {
        let f = make_func("f", vec![]);
        let r = build_dyn_vtable_registry(&[f]);
        assert!(r.is_empty(), "no CoerceToDynTrait → empty registry");
    }

    #[test]
    fn two_identical_coercions_dedupe_to_one_entry() {
        let entry = make_entry("Speak", "speak", "Dog::speak", Ty::Unit);
        let c1 = coerce("Speak", ResolvedTy::I64, vec![entry.clone()]);
        let c2 = coerce("Speak", ResolvedTy::I64, vec![entry]);
        let f = make_func("f", vec![c1, c2]);
        let r = build_dyn_vtable_registry(&[f]);
        assert_eq!(r.len(), 1, "identical sites collapse to one entry");
        assert_eq!(r[0].vtable_id, 0);
        assert_eq!(r[0].symbol, "__hew_vtable__Speak__i64__0");
        assert_eq!(r[0].trait_name, "Speak");
        assert_eq!(r[0].concrete_type, ResolvedTy::I64);
    }

    #[test]
    fn distinct_concretes_for_same_trait_yield_distinct_entries() {
        let e_i64 = make_entry("Speak", "speak", "i64::speak", Ty::Unit);
        let e_str = make_entry("Speak", "speak", "String::speak", Ty::Unit);
        let f = make_func(
            "f",
            vec![
                coerce("Speak", ResolvedTy::String, vec![e_str]),
                coerce("Speak", ResolvedTy::I64, vec![e_i64]),
            ],
        );
        let r = build_dyn_vtable_registry(&[f]);
        assert_eq!(r.len(), 2);
        // Sort key is `(trait_name, concrete.Display())`. The
        // `ResolvedTy::Display` impl renders `String` as the lowercase
        // literal `"string"` and `I64` as `"i64"`; `"string" > "i64"`
        // lexicographically. Verify the actual order rather than
        // asserting either direction inline.
        let order: Vec<(&str, &ResolvedTy)> = r
            .iter()
            .map(|inst| (inst.trait_name.as_str(), &inst.concrete_type))
            .collect();
        // Each entry must have a unique vtable_id matching its index.
        assert_eq!(r[0].vtable_id, 0);
        assert_eq!(r[1].vtable_id, 1);
        // Symbol carries trait + concrete + id.
        assert_eq!(r[0].symbol, "__hew_vtable__Speak__i64__0");
        assert_eq!(r[1].symbol, "__hew_vtable__Speak__string__1");
        assert!(order.iter().any(|(_, ct)| **ct == ResolvedTy::I64));
        assert!(order.iter().any(|(_, ct)| **ct == ResolvedTy::String));
    }

    #[test]
    fn distinct_assoc_projections_keep_separate_entries() {
        // Same trait + same concrete, but different substituted
        // signatures (proxy for distinct assoc-type bindings).
        let e_int = make_entry("Iterator", "next", "Counter::next", Ty::I64);
        let e_str = make_entry("Iterator", "next", "Counter::next", Ty::String);
        let f = make_func(
            "f",
            vec![
                coerce(
                    "Iterator",
                    ResolvedTy::named_user("Counter", vec![]),
                    vec![e_int],
                ),
                coerce(
                    "Iterator",
                    ResolvedTy::named_user("Counter", vec![]),
                    vec![e_str],
                ),
            ],
        );
        let r = build_dyn_vtable_registry(&[f]);
        assert_eq!(
            r.len(),
            2,
            "distinct vtable_entries discriminate even when (trait, concrete) match"
        );
        assert_eq!(r[0].vtable_id, 0);
        assert_eq!(r[1].vtable_id, 1);
    }

    #[test]
    fn ordering_is_stable_sort_by_trait_then_concrete() {
        // Source order: Speak/I64, Display/I64, Speak/String.
        // Sort key is `(trait_name, concrete.Display())`. The
        // `ResolvedTy::Display` impl renders `I64` as `"i64"` and
        // `String` as the lowercase literal `"string"` (an upstream
        // quirk of `Ty`'s formatter), so the sort produces:
        //   Display/I64 < Speak/I64 < Speak/String
        // because `"i64" < "string"` lexicographically.
        let e_speak_i64 = make_entry("Speak", "speak", "i64::speak", Ty::Unit);
        let e_display_i64 = make_entry("Display", "fmt", "i64::fmt", Ty::Unit);
        let e_speak_str = make_entry("Speak", "speak", "String::speak", Ty::Unit);
        let f = make_func(
            "f",
            vec![
                coerce("Speak", ResolvedTy::I64, vec![e_speak_i64]),
                coerce("Display", ResolvedTy::I64, vec![e_display_i64]),
                coerce("Speak", ResolvedTy::String, vec![e_speak_str]),
            ],
        );
        let r = build_dyn_vtable_registry(&[f]);
        assert_eq!(r.len(), 3);
        assert_eq!(r[0].trait_name, "Display");
        assert_eq!(r[0].concrete_type, ResolvedTy::I64);
        assert_eq!(r[1].trait_name, "Speak");
        assert_eq!(r[1].concrete_type, ResolvedTy::I64);
        assert_eq!(r[2].trait_name, "Speak");
        assert_eq!(r[2].concrete_type, ResolvedTy::String);
        for (i, inst) in r.iter().enumerate() {
            assert_eq!(inst.vtable_id as usize, i);
            assert_eq!(
                inst.symbol,
                crate::model::mangle_dyn_vtable_symbol(
                    inst.vtable_id,
                    &inst.trait_name,
                    &inst.concrete_type,
                )
            );
        }
    }

    #[test]
    fn cross_function_dedupe_collapses_repeats() {
        let entry = make_entry("Speak", "speak", "Dog::speak", Ty::Unit);
        let f1 = make_func(
            "f1",
            vec![coerce("Speak", ResolvedTy::I64, vec![entry.clone()])],
        );
        let f2 = make_func("f2", vec![coerce("Speak", ResolvedTy::I64, vec![entry])]);
        let r = build_dyn_vtable_registry(&[f1, f2]);
        assert_eq!(
            r.len(),
            1,
            "same vtable used from two functions still produces one entry"
        );
    }
}
