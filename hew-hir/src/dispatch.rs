#![allow(
    deprecated,
    reason = "temporary named identity reconstruction migration seam"
)]

//! Structured static-trait-dispatch lookup.
//!
//! `CallTraitMethodStatic` carries a checker-selected `CallTarget` containing
//! the declaring trait and method identities. Monomorphisation needs to
//! resolve that identity into the concrete impl method symbol (`<Self>::<method>`) plus
//! the impl-level type-parameter names — without reverse-parsing the
//! flattened symbol or inferring impl identity from display-name strings.
//!
//! The lookup is keyed on `(declaring_trait, self_type, method)`, all of which
//! come straight from `HirImplBlock` declaration IDs / `CallTraitMethodStatic`
//! fields. The final `<Self>::<method>` symbol comes back from the
//! `HirImplBlock` (where it was emitted via `HirImplBlock::method_symbol`)
//! so the canonical encoding lives in exactly one place.
//!
//! See `docs/internal/engineering-invariants.md` for the single semantic
//! authority principle and
//! `string-identifier-fragility`).

use std::collections::HashMap;

use crate::{node::HirItem, ItemId};
use hew_types::{DefId, NominalId, NominalInstance, ResolvedTy};

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
    pub impl_type_params: Vec<String>,
}

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
        // Generic impls are registered under their declaration nominal with no
        // concrete instance args. A specialised impl retains the concrete args
        // structurally instead of encoding them into a mangled string key.
        let self_type = NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path(block.self_type_name.clone()),
            args: if block.type_params.is_empty() {
                block.self_type_concrete_args.clone()
            } else {
                Vec::new()
            },
        };
        // `method_names` and `method_symbols` are produced together in
        // `lower_impl_block` and MUST be parallel. Defensive zip: any
        // length mismatch indicates upstream HIR construction drift and
        // produces no entries for the extra slots.
        for ((((method_symbol, declaring_trait), trait_method_id), impl_method_id), method_item) in
            block
                .method_symbols
                .iter()
                .zip(block.method_declaring_trait_ids.iter())
                .zip(block.method_trait_method_ids.iter())
                .zip(block.method_ids.iter())
                .zip(block.method_item_ids.iter())
        {
            let (Some(declaring_trait), Some(trait_method_id), Some(impl_method_id)) =
                (declaring_trait, trait_method_id, impl_method_id)
            else {
                continue;
            };
            let key = TraitImplKey {
                // Static calls carry the trait declaration identity.  The
                // emitted method has a separate implementation declaration
                // identity, retained in the entry for direct-call projection.
                method: trait_method_id.clone(),
                declaring_trait: declaring_trait.clone(),
                self_type: self_type.clone(),
            };
            index.insert(
                key,
                TraitImplMethodEntry {
                    item: *method_item,
                    method: impl_method_id.clone(),
                    method_symbol: method_symbol.clone(),
                    impl_type_params: block.type_params.clone(),
                },
            );
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
                index.insert(function.declaration.clone(), function.name.clone());
            }
            HirItem::Impl(block) => {
                for (method_id, method_symbol) in block.method_ids.iter().zip(&block.method_symbols)
                {
                    if let Some(method_id) = method_id {
                        index.insert(method_id.clone(), method_symbol.clone());
                    }
                }
            }
            HirItem::ExternFn(extern_fn) => {
                index.insert(extern_fn.declaration.clone(), extern_fn.name.clone());
            }
            _ => {}
        }
    }
    index
}

/// Resolve canonical trait, nominal-instance, and method identities against
/// the static-dispatch index. Concrete specialisations are tried first; the
/// only permitted fallback is the exact same nominal's generic impl entry.
#[must_use]
pub fn lookup_trait_impl_entry_by_id<'a, S: std::hash::BuildHasher>(
    index: &'a HashMap<TraitImplKey, TraitImplMethodEntry, S>,
    declaring_trait: &DefId,
    self_type: &NominalInstance,
    method: &DefId,
) -> Option<&'a TraitImplMethodEntry> {
    let key = TraitImplKey {
        declaring_trait: declaring_trait.clone(),
        self_type: self_type.clone(),
        method: method.clone(),
    };
    if let Some(entry) = index.get(&key) {
        return Some(entry);
    }
    if self_type.args.is_empty() {
        return None;
    }
    index.get(&TraitImplKey {
        declaring_trait: declaring_trait.clone(),
        self_type: NominalInstance {
            nominal: self_type.nominal.clone(),
            args: Vec::new(),
        },
        method: method.clone(),
    })
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
pub fn receiver_self_type_for_impl_lookup_instance(ty: &ResolvedTy) -> Option<NominalInstance> {
    match ty {
        ResolvedTy::Named {
            args,
            builtin: Some(builtin),
            ..
        } => {
            // Builtin trait impls are registered from `std/builtins.hew`
            // under checker-owned nominal identities. Select those identities
            // from the closed builtin discriminator, never from the source
            // leaf: a user `type HashMapIter` carries `builtin: None` and stays
            // on the ordinary user-nominal arm below.
            let nominal = match builtin {
                hew_types::BuiltinType::VecIter => "std.builtins.VecIter",
                hew_types::BuiltinType::HashMapIter => "std.builtins.HashMapIter",
                hew_types::BuiltinType::Generator => "Generator",
                hew_types::BuiltinType::AsyncGenerator => "AsyncGenerator",
                hew_types::BuiltinType::Vec => "Vec",
                hew_types::BuiltinType::HashMap => "HashMap",
                hew_types::BuiltinType::ChildRef => "ChildRef",
                hew_types::BuiltinType::LocalPid => "LocalPid",
                hew_types::BuiltinType::RemotePid => "RemotePid",
                hew_types::BuiltinType::NodeId => "NodeId",
                hew_types::BuiltinType::Location => "Location",
                _ => return None,
            };
            Some(NominalInstance {
                nominal: NominalId::legacy_reconstruct_from_full_path(nominal),
                args: args.clone(),
            })
        }
        ResolvedTy::Named { .. } => ty.nominal_instance(),
        ResolvedTy::I8 => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("i8"),
            args: Vec::new(),
        }),
        ResolvedTy::I16 => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("i16"),
            args: Vec::new(),
        }),
        ResolvedTy::I32 => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("i32"),
            args: Vec::new(),
        }),
        ResolvedTy::I64 => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("i64"),
            args: Vec::new(),
        }),
        ResolvedTy::U8 => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("u8"),
            args: Vec::new(),
        }),
        ResolvedTy::U16 => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("u16"),
            args: Vec::new(),
        }),
        ResolvedTy::U32 => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("u32"),
            args: Vec::new(),
        }),
        ResolvedTy::U64 => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("u64"),
            args: Vec::new(),
        }),
        ResolvedTy::Isize => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("isize"),
            args: Vec::new(),
        }),
        ResolvedTy::Usize => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("usize"),
            args: Vec::new(),
        }),
        ResolvedTy::F32 => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("f32"),
            args: Vec::new(),
        }),
        ResolvedTy::F64 => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("f64"),
            args: Vec::new(),
        }),
        ResolvedTy::Bool => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("bool"),
            args: Vec::new(),
        }),
        ResolvedTy::Char => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("char"),
            args: Vec::new(),
        }),
        ResolvedTy::String => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("string"),
            args: Vec::new(),
        }),
        ResolvedTy::Bytes => Some(NominalInstance {
            nominal: NominalId::legacy_reconstruct_from_full_path("bytes"),
            args: Vec::new(),
        }),
        _ => None,
    }
}

/// Compatibility projection for MIR's legacy string-shaped static-dispatch
/// consumer. New HIR lowering uses
/// [`receiver_self_type_for_impl_lookup_instance`] instead.
#[must_use]
pub fn receiver_self_type_for_impl_lookup(ty: &ResolvedTy) -> Option<(String, Vec<ResolvedTy>)> {
    receiver_self_type_for_impl_lookup_instance(ty).map(|instance| {
        (
            instance.nominal.declaration().full_path().to_string(),
            instance.args,
        )
    })
}

#[cfg(test)]
mod tests {
    use super::{
        lookup_trait_impl_entry_by_id, receiver_self_type_for_impl_lookup_instance, TraitImplKey,
        TraitImplMethodEntry,
    };
    use hew_types::{BuiltinType, DefId, NominalId, NominalInstance, ResolvedTy};
    use std::collections::HashMap;

    fn entry(method: &DefId, symbol: &str) -> TraitImplMethodEntry {
        TraitImplMethodEntry {
            item: crate::ItemId(0),
            method: method.clone(),
            method_symbol: symbol.to_string(),
            impl_type_params: Vec::new(),
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
                declaring_trait: alpha_trait.clone(),
                self_type: alpha_thing.clone(),
                method: alpha_method.clone(),
            },
            entry(&alpha_method, "Thing::show__alpha"),
        );
        index.insert(
            TraitImplKey {
                declaring_trait: beta_trait.clone(),
                self_type: beta_thing.clone(),
                method: beta_method.clone(),
            },
            entry(&beta_method, "Thing::show__beta"),
        );

        assert_eq!(
            lookup_trait_impl_entry_by_id(&index, &alpha_trait, &alpha_thing, &alpha_method)
                .map(|entry| entry.method_symbol.as_str()),
            Some("Thing::show__alpha")
        );
        assert_eq!(
            lookup_trait_impl_entry_by_id(&index, &beta_trait, &beta_thing, &beta_method)
                .map(|entry| entry.method_symbol.as_str()),
            Some("Thing::show__beta")
        );
        assert!(
            lookup_trait_impl_entry_by_id(&index, &alpha_trait, &beta_thing, &alpha_method)
                .is_none()
        );
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
                declaring_trait: trait_id.clone(),
                self_type: generic,
                method: method_id.clone(),
            },
            entry(&method_id, "Box::show__generic"),
        );
        index.insert(
            TraitImplKey {
                declaring_trait: trait_id.clone(),
                self_type: i64_instance.clone(),
                method: method_id.clone(),
            },
            entry(&method_id, "Box::show__i64"),
        );

        assert_eq!(
            lookup_trait_impl_entry_by_id(&index, &trait_id, &i64_instance, &method_id)
                .map(|entry| entry.method_symbol.as_str()),
            Some("Box::show__i64")
        );
        assert_eq!(
            lookup_trait_impl_entry_by_id(&index, &trait_id, &string_instance, &method_id)
                .map(|entry| entry.method_symbol.as_str()),
            Some("Box::show__generic")
        );
    }

    #[test]
    fn builtin_impl_receiver_identity_requires_the_typed_discriminator() {
        let builtin = ResolvedTy::named_builtin(
            "HashMapIter",
            BuiltinType::HashMapIter,
            vec![ResolvedTy::I64, ResolvedTy::String],
        );
        let user = ResolvedTy::named_user("HashMapIter", vec![ResolvedTy::I64, ResolvedTy::String]);

        let builtin_instance = receiver_self_type_for_impl_lookup_instance(&builtin)
            .expect("the compiler cursor has an exact std impl identity");
        assert_eq!(
            builtin_instance.nominal.full_path(),
            "std.builtins.HashMapIter"
        );
        assert_eq!(
            receiver_self_type_for_impl_lookup_instance(&user)
                .expect("the user nominal remains independently dispatchable")
                .nominal
                .full_path(),
            "HashMapIter"
        );
        assert_ne!(builtin_instance.nominal.full_path(), "HashMapIter");
    }
}
