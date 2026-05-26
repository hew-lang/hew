//! Structured static-trait-dispatch lookup.
//!
//! `CallTraitMethodStatic` carries `(receiver_type_param, declaring_trait,
//! method_name)`. Monomorphisation and MIR lowering need to resolve that
//! triple into the concrete impl method symbol (`<Self>::<method>`) plus
//! the impl-level type-parameter names — without reverse-parsing the
//! flattened symbol or inferring impl identity from display-name strings.
//!
//! The lookup is keyed on `(declaring_trait, self_type_name, method_name)`,
//! all of which come straight from `HirImplBlock` / `CallTraitMethodStatic`
//! fields. The final `<Self>::<method>` symbol comes back from the
//! `HirImplBlock` (where it was emitted via `HirImplBlock::method_symbol`)
//! so the canonical encoding lives in exactly one place.
//!
//! See LESSONS.md (`checker-authority`, `type-info-survival`,
//! `string-identifier-fragility`).

use std::collections::HashMap;

use crate::node::HirItem;
use hew_types::ResolvedTy;

/// One impl-method entry in the structured static-dispatch registry.
#[derive(Debug, Clone)]
pub struct TraitImplMethodEntry {
    /// Canonical `<Self>::<method>` symbol that the corresponding
    /// `HirItem::Function` was emitted under. Treat as an opaque
    /// identifier produced by `HirImplBlock::method_symbol`.
    pub method_symbol: String,
    /// Impl-level type parameter names (e.g. `["U"]` for
    /// `impl<U> Show for Wrapper<U>`). Empty for non-generic impls.
    /// Order matches the impl-method's `HirFn::type_params` prefix.
    pub impl_type_params: Vec<String>,
}

/// Key into the static-dispatch registry. Every field is structured —
/// `declaring_trait` and `method_name` come straight from the call site's
/// `CallTraitMethodStatic` node, `self_type_name` from `HirImplBlock`.
pub type TraitImplKey = (String, String, String);

/// Build `(declaring_trait, self_type_name, method_name) → TraitImplMethodEntry`
/// from the module's `HirItem::Impl` entries.
///
/// Iterates trait-bearing impl blocks (`HirImplBlock::trait_name == Some(_)`)
/// and zips `method_names` with `method_symbols` (parallel arrays maintained
/// by `lower_impl_block`). Inherent impls (no trait bound) do not participate
/// in static trait dispatch and are skipped.
#[must_use]
pub fn build_trait_impl_method_index(
    items: &[HirItem],
) -> HashMap<TraitImplKey, TraitImplMethodEntry> {
    let mut index: HashMap<TraitImplKey, TraitImplMethodEntry> = HashMap::new();
    for item in items {
        let HirItem::Impl(block) = item else { continue };
        let Some(trait_name) = block.trait_name.clone() else {
            continue;
        };
        // `method_names` and `method_symbols` are produced together in
        // `lower_impl_block` and MUST be parallel. Defensive zip: any
        // length mismatch indicates upstream HIR construction drift and
        // produces no entries for the extra slots.
        for (method_name, method_symbol) in
            block.method_names.iter().zip(block.method_symbols.iter())
        {
            let key = (
                trait_name.clone(),
                block.self_type_name.clone(),
                method_name.clone(),
            );
            index.insert(
                key,
                TraitImplMethodEntry {
                    method_symbol: method_symbol.clone(),
                    impl_type_params: block.type_params.clone(),
                },
            );
        }
    }
    index
}

/// Canonical impl-self-type-name + type-arg vector for a substituted
/// receiver `ResolvedTy`. Used to drive the structured registry lookup.
///
/// Returns `None` for receiver shapes that cannot anchor an impl (closures,
/// function types, unsubstituted-only types). Callers must fail-closed.
///
/// The returned name is the canonical nominal identifier that
/// `HirImplBlock::self_type_name` was populated with at impl lowering —
/// e.g. `"Wrapper"` for `Wrapper<i64>`. Primitive impls (e.g.
/// `impl Show for i64`) anchor on the canonical builtin name as seen by
/// the parser; this helper maps `ResolvedTy::I64` to `"i64"` etc. so the
/// same registry serves builtin-receiver static dispatch.
#[must_use]
pub fn receiver_self_type_for_impl_lookup(ty: &ResolvedTy) -> Option<(String, Vec<ResolvedTy>)> {
    match ty {
        ResolvedTy::Named { name, args, .. } => Some((name.clone(), args.clone())),
        ResolvedTy::I64 => Some(("i64".to_string(), Vec::new())),
        ResolvedTy::F64 => Some(("f64".to_string(), Vec::new())),
        ResolvedTy::Bool => Some(("bool".to_string(), Vec::new())),
        ResolvedTy::String => Some(("string".to_string(), Vec::new())),
        ResolvedTy::Bytes => Some(("bytes".to_string(), Vec::new())),
        _ => None,
    }
}
