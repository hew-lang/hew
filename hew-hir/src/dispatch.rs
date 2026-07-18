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
///
/// For concrete specialised impls (empty `type_params`, non-empty
/// `self_type_concrete_args`), the key self-type name is the mangled form
/// incorporating the concrete args — e.g. `"Wrapper$$i64"` for
/// `impl Describe for Wrapper<i64>`. This ensures `impl Describe for Wrapper<i64>`
/// and `impl Describe for Wrapper<string>` produce distinct keys and never
/// collide in the index.
#[must_use]
pub fn build_trait_impl_method_index(
    items: &[HirItem],
) -> HashMap<TraitImplKey, TraitImplMethodEntry> {
    let mut index: HashMap<TraitImplKey, TraitImplMethodEntry> = HashMap::new();
    for item in items {
        let HirItem::Impl(block) = item else { continue };
        if block.trait_name.is_none() {
            continue;
        }
        // Key self-type name: bare for generic impls (type_params non-empty)
        // or impls with no target type args; mangled for concrete specialised
        // impls (`impl Trait for Wrapper<i64>` → `"Wrapper$$i64"`).
        let key_self_type =
            if block.type_params.is_empty() && !block.self_type_concrete_args.is_empty() {
                crate::monomorph::mangle(&block.self_type_name, &block.self_type_concrete_args)
            } else {
                block.self_type_name.clone()
            };
        // `method_names` and `method_symbols` are produced together in
        // `lower_impl_block` and MUST be parallel. Defensive zip: any
        // length mismatch indicates upstream HIR construction drift and
        // produces no entries for the extra slots.
        for ((method_name, method_symbol), declaring_trait) in block
            .method_names
            .iter()
            .zip(block.method_symbols.iter())
            .zip(block.method_declaring_traits.iter())
        {
            let key = (
                declaring_trait.clone(),
                key_self_type.clone(),
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

/// Resolve a `(declaring_trait, self_type_name, method_name)` key against the
/// static-dispatch index, tolerating a module-qualified `self_type_name`.
///
/// The index is keyed on the impl block's bare `self_type_name` (`Map`), but a
/// receiver observed at a cross-module call site carries its module-qualified
/// spelling (`iter.Map`). The qualifier is an outer-identity concern
/// (`per-module-type-identity`): two adapters of the same bare name in different
/// modules are distinct user types, but the impl-method dispatch key is the bare
/// nominal the impl block declared. Look up the key as given first, then retry
/// with the bare leaf so an imported generic adapter resolves the same way it
/// does in its defining module.
///
/// `self_type_concrete_args` carries the resolved type arguments of the receiver
/// at the call site (e.g. `[ResolvedTy::I64]` for a `Wrapper<i64>` receiver).
/// When non-empty, the lookup first tries the mangled self-type key
/// (`"Wrapper$$i64"`) to find a concrete specialised impl, then falls back to
/// the bare name for a generic impl (`impl<U> Trait for Wrapper<U>`). This
/// ensures two concrete instantiations with distinct args resolve to their own
/// impl entries rather than colliding on the bare name.
#[must_use]
pub fn lookup_trait_impl_entry<'a, S: std::hash::BuildHasher>(
    index: &'a HashMap<TraitImplKey, TraitImplMethodEntry, S>,
    declaring_trait: &str,
    self_type_name: &str,
    method_name: &str,
    self_type_concrete_args: &[hew_types::ResolvedTy],
) -> Option<&'a TraitImplMethodEntry> {
    // 1. When the receiver has concrete type args, try the mangled concrete key
    //    first — this matches `impl Describe for Wrapper<i64>` over
    //    `impl<U> Describe for Wrapper<U>` when both are present.
    if !self_type_concrete_args.is_empty() {
        let mangled = crate::monomorph::mangle(self_type_name, self_type_concrete_args);
        let concrete_key = (
            declaring_trait.to_string(),
            mangled.clone(),
            method_name.to_string(),
        );
        if let Some(entry) = index.get(&concrete_key) {
            return Some(entry);
        }
        // Also try bare-leaf of the mangled name for module-qualified receivers.
        let bare_mangled = hew_types::short_name(&mangled);
        if bare_mangled != mangled {
            let bare_concrete_key = (
                declaring_trait.to_string(),
                bare_mangled.to_string(),
                method_name.to_string(),
            );
            if let Some(entry) = index.get(&bare_concrete_key) {
                return Some(entry);
            }
        }
    }
    // 2. Fall back to bare self_type_name (generic impls and impls without
    //    target type args — the common case prior to this fix).
    let key = (
        declaring_trait.to_string(),
        self_type_name.to_string(),
        method_name.to_string(),
    );
    if let Some(entry) = index.get(&key) {
        return Some(entry);
    }
    let bare = hew_types::short_name(self_type_name);
    if bare == self_type_name {
        return None;
    }
    let bare_key = (
        declaring_trait.to_string(),
        bare.to_string(),
        method_name.to_string(),
    );
    index.get(&bare_key)
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
        ResolvedTy::I8 => Some(("i8".to_string(), Vec::new())),
        ResolvedTy::I16 => Some(("i16".to_string(), Vec::new())),
        ResolvedTy::I32 => Some(("i32".to_string(), Vec::new())),
        ResolvedTy::I64 => Some(("i64".to_string(), Vec::new())),
        ResolvedTy::U8 => Some(("u8".to_string(), Vec::new())),
        ResolvedTy::U16 => Some(("u16".to_string(), Vec::new())),
        ResolvedTy::U32 => Some(("u32".to_string(), Vec::new())),
        ResolvedTy::U64 => Some(("u64".to_string(), Vec::new())),
        ResolvedTy::Isize => Some(("isize".to_string(), Vec::new())),
        ResolvedTy::Usize => Some(("usize".to_string(), Vec::new())),
        ResolvedTy::F32 => Some(("f32".to_string(), Vec::new())),
        ResolvedTy::F64 => Some(("f64".to_string(), Vec::new())),
        ResolvedTy::Bool => Some(("bool".to_string(), Vec::new())),
        ResolvedTy::Char => Some(("char".to_string(), Vec::new())),
        ResolvedTy::String => Some(("string".to_string(), Vec::new())),
        ResolvedTy::Bytes => Some(("bytes".to_string(), Vec::new())),
        _ => None,
    }
}
