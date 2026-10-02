//! Structured static-trait-dispatch lookup.
//!
//! `CallTraitMethodStatic` carries a checker-selected `CallTarget` containing
//! the declaring trait and method identities. Monomorphisation needs to
//! resolve that identity into the emitted function body's symbol plus the
//! impl-level type-parameter names.
//!
//! The lookup is keyed on `(declaring_trait, self_type, method)`, all of which
//! come straight from `HirImplBlock` declaration IDs / `CallTraitMethodStatic`
//! fields. The corresponding `HirItem::Function` owns the physical symbol.
//!
//! See `docs/internal/engineering-invariants.md` for the single semantic
//! authority principle and
//! `string-identifier-fragility`).

use std::collections::HashMap;

use crate::{node::HirItem, ItemId};
use hew_types::{DefId, NominalInstance};

/// One impl-method entry in the structured static-dispatch registry.
#[derive(Debug, Clone)]
pub struct TraitImplMethodEntry {
    /// Exact HIR function item whose emitted body implements this method.
    pub item: ItemId,
    /// Canonical identity of the emitted impl method.
    pub method: DefId,
    /// Canonical `<Self>::<method>` symbol that the corresponding
    /// `HirItem::Function` was emitted under. Treat as an opaque
    /// identifier produced by `HirImplBlock::method_symbol`.
    pub method_symbol: String,
    /// Impl-level type parameter names (e.g. `["U"]` for
    /// `impl<U> Show for Wrapper<U>`). Empty for non-generic impls.
    /// Order matches the impl-method's `HirFn::type_params` prefix.
    pub impl_type_params: Vec<hew_types::ParamHead>,
    /// The impl target's type arguments as written; a generic impl binds
    /// `impl_type_params` by matching them against the concrete `Self`.
    pub self_type_args: Vec<hew_types::ResolvedTy>,
    /// The implemented trait's arguments as the impl writes them; they tell
    /// `impl From<Low> for E` from `impl From<High> for E`.
    pub trait_args: Vec<hew_types::ResolvedTy>,
}

/// Every impl method that can answer one `(trait, Self, method)` key, in
/// declaration order. More than one exists only for a generic trait the type
/// implements at several argument lists.
pub type TraitImplIndex = HashMap<TraitImplKey, Vec<TraitImplMethodEntry>>;

/// Key into the static-dispatch registry. Every field is structured —
/// `declaring_trait` and `method` come straight from declaration/call-site
/// IDs, and `self_type` from `HirImplBlock`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitImplKey {
    pub declaring_trait: DefId,
    pub self_type: NominalInstance,
    pub method: DefId,
}

/// Build `(declaring_trait, self_type, method) → TraitImplMethodEntry`
/// from the module's `HirItem::Impl` entries.
///
/// Iterates trait-bearing impl blocks and joins each method item identity to
/// its emitted `HirFn`. Inherent impls do not participate in static trait
/// dispatch.
///
/// A concrete specialised impl (empty `type_params`, non-empty
/// `self_type_args`) keys on its concrete args, so
/// `impl Describe for Wrapper<i64>` and `impl Describe for Wrapper<string>`
/// never collide in the index.
///
/// A structural witness files the method that satisfies a trait method,
/// unless a nominal impl already provides it.
#[must_use]
pub fn build_trait_impl_method_index(
    items: &[HirItem],
    structural_witnesses: &[hew_types::StructuralWitness],
) -> TraitImplIndex {
    let mut index = TraitImplIndex::new();
    let functions: HashMap<ItemId, _> = items
        .iter()
        .filter_map(|item| match item {
            HirItem::Function(function) => Some((function.id, function)),
            _ => None,
        })
        .collect();
    for item in items {
        let HirItem::Impl(block) = item else { continue };
        // A target with no impl-dispatch anchor can never be reached from a
        // receiver, so it contributes no entries.
        let (Some(_), Some(nominal)) = (&block.trait_name, &block.self_type) else {
            continue;
        };
        // Generic impls are registered under their declaration nominal with no
        // concrete instance args. A specialised impl retains the concrete args
        // structurally instead of encoding them into a mangled string key.
        let self_type = NominalInstance {
            nominal: *nominal,
            args: if block.type_params.is_empty() {
                block.self_type_args.clone()
            } else {
                Vec::new()
            },
        };
        for (((declaring_trait, trait_method_id), impl_method_id), method_item) in block
            .method_declaring_trait_ids
            .iter()
            .zip(block.method_trait_method_ids.iter())
            .zip(block.method_ids.iter())
            .zip(block.method_item_ids.iter())
        {
            let (Some(declaring_trait), Some(trait_method_id), Some(impl_method_id)) =
                (declaring_trait, trait_method_id, impl_method_id)
            else {
                continue;
            };
            let Some(function) = functions.get(method_item) else {
                continue;
            };
            if function.declaration != *impl_method_id {
                continue;
            }
            let key = TraitImplKey {
                // Static calls carry the trait declaration identity.  The
                // emitted method has a separate implementation declaration
                // identity, retained in the entry for direct-call projection.
                method: *trait_method_id,
                declaring_trait: *declaring_trait,
                self_type: self_type.clone(),
            };
            index.entry(key).or_default().push(TraitImplMethodEntry {
                item: *method_item,
                method: *impl_method_id,
                method_symbol: function.name.clone(),
                impl_type_params: block.type_params.clone(),
                self_type_args: block.self_type_args.clone(),
                trait_args: block.trait_args.clone(),
            });
        }
    }
    for item in items {
        // The filler is the method the checker matched: an inherent one, or
        // another trait's impl method of the same signature.
        let HirItem::Impl(block) = item else { continue };
        for (method_id, method_item) in block.method_ids.iter().zip(&block.method_item_ids) {
            let (Some(method_id), Some(function)) = (method_id, functions.get(method_item)) else {
                continue;
            };
            for witness in structural_witnesses
                .iter()
                .filter(|witness| witness.inherent == *method_id)
            {
                let key = TraitImplKey {
                    declaring_trait: witness.declaring_trait,
                    self_type: NominalInstance {
                        nominal: witness.self_type,
                        args: Vec::new(),
                    },
                    method: witness.method,
                };
                let entries = index.entry(key).or_default();
                if entries.is_empty() {
                    entries.push(TraitImplMethodEntry {
                        item: *method_item,
                        method: *method_id,
                        method_symbol: function.name.clone(),
                        impl_type_params: block.type_params.clone(),
                        self_type_args: block.self_type_args.clone(),
                        trait_args: Vec::new(),
                    });
                }
            }
        }
    }
    index
}

/// Build the exact declaration-ID → emitted-symbol projection for every impl
/// method in a HIR module.  This is a linker-layout projection only: the
/// checker owns the `DefId`, and MIR uses this table to avoid reverse-parsing a
/// `Type::method` presentation string when lowering `CallTarget::ImplMethod`.
#[must_use]
pub fn build_direct_call_symbol_index(items: &[HirItem]) -> HashMap<DefId, String> {
    let mut index = HashMap::new();
    for item in items {
        match item {
            HirItem::Function(function) => {
                index.insert(function.declaration, function.name.clone());
            }
            HirItem::ExternFn(extern_fn) => {
                index.insert(extern_fn.declaration, extern_fn.name.clone());
            }
            _ => {}
        }
    }
    index
}

/// Resolve canonical trait, nominal-instance, and method identities against
/// the static-dispatch index. Concrete specialisations are tried first; the
/// only permitted fallback is the exact same nominal's generic impl entry.
///
/// Within a key, a sole entry answers: the checker proved the bound the call
/// relies on. Several entries are impls of one generic trait at different
/// arguments, and `trait_args` (the call's, substituted) must name exactly
/// one; otherwise the lookup fails closed.
#[must_use]
pub fn lookup_trait_impl_entry_by_id<'a, S: std::hash::BuildHasher>(
    index: &'a HashMap<TraitImplKey, Vec<TraitImplMethodEntry>, S>,
    declaring_trait: &DefId,
    self_type: &NominalInstance,
    trait_args: &[hew_types::ResolvedTy],
    method: &DefId,
) -> Option<&'a TraitImplMethodEntry> {
    let select = |self_type: NominalInstance| {
        let entries = index.get(&TraitImplKey {
            declaring_trait: *declaring_trait,
            self_type,
            method: *method,
        })?;
        if let [only] = entries.as_slice() {
            return Some(only);
        }
        let mut matching = entries
            .iter()
            .filter(|entry| entry.trait_args == trait_args);
        let selected = matching.next()?;
        matching.next().is_none().then_some(selected)
    };
    if let Some(entry) = select(self_type.clone()) {
        return Some(entry);
    }
    if self_type.args.is_empty() {
        return None;
    }
    select(NominalInstance {
        nominal: self_type.nominal,
        args: Vec::new(),
    })
}

#[cfg(test)]
mod tests {
    use super::{lookup_trait_impl_entry_by_id, TraitImplKey, TraitImplMethodEntry};
    use hew_types::{BuiltinType, DefId, NominalId, NominalInstance, ResolvedTy};
    use std::collections::HashMap;

    fn entry(method: DefId, symbol: &str) -> TraitImplMethodEntry {
        TraitImplMethodEntry {
            item: crate::ItemId(0),
            method,
            method_symbol: symbol.to_string(),
            impl_type_params: Vec::new(),
            self_type_args: Vec::new(),
            trait_args: Vec::new(),
        }
    }

    #[test]
    fn canonical_static_dispatch_keeps_same_leaf_declarations_distinct() {
        let alpha_trait = DefId::for_test("alpha.Render");
        let alpha_method = DefId::for_test("alpha.Render::show");
        let beta_trait = DefId::for_test("beta.Render");
        let beta_method = DefId::for_test("beta.Render::show");
        let alpha_thing = NominalInstance {
            nominal: NominalId::for_test("alpha.Thing"),
            args: Vec::new(),
        };
        let beta_thing = NominalInstance {
            nominal: NominalId::for_test("beta.Thing"),
            args: Vec::new(),
        };
        let mut index = HashMap::new();
        index.insert(
            TraitImplKey {
                declaring_trait: alpha_trait,
                self_type: alpha_thing.clone(),
                method: alpha_method,
            },
            vec![entry(alpha_method, "Thing::show__alpha")],
        );
        index.insert(
            TraitImplKey {
                declaring_trait: beta_trait,
                self_type: beta_thing.clone(),
                method: beta_method,
            },
            vec![entry(beta_method, "Thing::show__beta")],
        );

        assert_eq!(
            lookup_trait_impl_entry_by_id(&index, &alpha_trait, &alpha_thing, &[], &alpha_method)
                .map(|entry| entry.method_symbol.as_str()),
            Some("Thing::show__alpha")
        );
        assert_eq!(
            lookup_trait_impl_entry_by_id(&index, &beta_trait, &beta_thing, &[], &beta_method)
                .map(|entry| entry.method_symbol.as_str()),
            Some("Thing::show__beta")
        );
        assert!(lookup_trait_impl_entry_by_id(
            &index,
            &alpha_trait,
            &beta_thing,
            &[],
            &alpha_method
        )
        .is_none());
    }

    #[test]
    fn canonical_static_dispatch_prefers_specialized_instance_then_exact_generic() {
        let trait_id = DefId::for_test("render.Render");
        let method_id = DefId::for_test("render.Render::show");
        let generic = NominalInstance {
            nominal: NominalId::for_test("pkg.Box"),
            args: Vec::new(),
        };
        let i64_instance = NominalInstance {
            nominal: NominalId::for_test("pkg.Box"),
            args: vec![ResolvedTy::I64],
        };
        let string_instance = NominalInstance {
            nominal: NominalId::for_test("pkg.Box"),
            args: vec![ResolvedTy::String],
        };
        let mut index = HashMap::new();
        index.insert(
            TraitImplKey {
                declaring_trait: trait_id,
                self_type: generic,
                method: method_id,
            },
            vec![entry(method_id, "Box::show__generic")],
        );
        index.insert(
            TraitImplKey {
                declaring_trait: trait_id,
                self_type: i64_instance.clone(),
                method: method_id,
            },
            vec![entry(method_id, "Box::show__i64")],
        );

        assert_eq!(
            lookup_trait_impl_entry_by_id(&index, &trait_id, &i64_instance, &[], &method_id)
                .map(|entry| entry.method_symbol.as_str()),
            Some("Box::show__i64")
        );
        assert_eq!(
            lookup_trait_impl_entry_by_id(&index, &trait_id, &string_instance, &[], &method_id)
                .map(|entry| entry.method_symbol.as_str()),
            Some("Box::show__generic")
        );
    }

    #[test]
    fn builtin_impl_receiver_identity_requires_the_typed_discriminator() {
        let builtin = ResolvedTy::named_builtin(
            BuiltinType::HashMapIter,
            vec![ResolvedTy::I64, ResolvedTy::String],
        );
        let mut defs = hew_types::DefTable::new();
        let cursor = defs.mint_for_test("std.builtins.HashMapIter");
        let shadow = defs.mint_for_test("HashMapIter");
        let user = ResolvedTy::named_user(
            hew_types::NominalHead::new(
                hew_types::NominalId::of_declaration(shadow),
                "HashMapIter",
            ),
            vec![ResolvedTy::I64, ResolvedTy::String],
        );
        let builtin_instance = builtin
            .impl_receiver_instance(&defs)
            .expect("the compiler cursor has an exact std impl identity");
        assert_eq!(builtin_instance.nominal.declaration(), cursor);
        assert_eq!(
            user.impl_receiver_instance(&defs)
                .expect("the user nominal remains independently dispatchable")
                .nominal
                .declaration(),
            shadow
        );
    }

    #[test]
    fn impls_of_one_generic_trait_are_told_apart_by_their_arguments() {
        let from = DefId::for_test("std.From");
        let from_method = DefId::for_test("std.From::from");
        let app_error = NominalInstance {
            nominal: NominalId::for_test("app.AppError"),
            args: Vec::new(),
        };
        let low = ResolvedTy::named_for_test("app.LowError", Vec::new());
        let io = ResolvedTy::named_for_test("app.IoError", Vec::new());
        let at = |args: Vec<ResolvedTy>, symbol: &str| TraitImplMethodEntry {
            trait_args: args,
            ..entry(from_method, symbol)
        };
        let mut index = HashMap::new();
        index.insert(
            TraitImplKey {
                declaring_trait: from,
                self_type: app_error.clone(),
                method: from_method,
            },
            vec![
                at(vec![low.clone()], "AppError::from__low"),
                at(vec![io.clone()], "AppError::from__io"),
            ],
        );
        let select = |args: &[ResolvedTy]| {
            lookup_trait_impl_entry_by_id(&index, &from, &app_error, args, &from_method)
                .map(|entry| entry.method_symbol.as_str())
        };
        assert_eq!(select(&[low]), Some("AppError::from__low"));
        assert_eq!(select(&[io]), Some("AppError::from__io"));
        assert_eq!(select(&[]), None, "unknown arguments cannot choose an impl");
    }
}
