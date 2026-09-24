//! Checker registration/method/expression logic, split into submodules.
#![allow(
    unused_imports,
    redundant_imports,
    reason = "header retained verbatim from the pre-split file"
)]
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::builtin_names::BuiltinNamedType;
use crate::check::calls::SignatureArgApplication;
use crate::check::dispatch::resolve_method_call;
use crate::check::types::GenericCallee;
use crate::check::types::{BareActorResolution, DeferredBuiltinCloneAdmission, DeferredWireCodec};
use crate::method_resolution::{
    collect_method_sigs_for_receiver, instantiate_stdlib_method_sig, lookup_builtin_method_sig,
    lookup_named_method_sig as shared_lookup_named_method_sig,
};
use crate::runtime_call::{FloatMethodOp, IntArithKind, IntBitOp, IntMethodWidth};
use crate::stdlib::{STD_NET_CONNECTION, STD_NET_LISTENER};
use crate::BuiltinType;

mod part1;
mod part2;
mod part3;
mod part4;
mod part5;
#[cfg(test)]
mod tests;

impl Checker {
    /// Return the checker-minted trait declaration IDs for a call-site trait
    /// spelling. Import aliases are resolved through their full source owner;
    /// short registry keys remain compatibility-only and are never used to mint
    /// a fresh ID here.
    pub(in crate::check) fn trait_method_call_target_ids(
        &self,
        trait_name: &str,
        method_name: &str,
    ) -> Option<(crate::DefId, crate::DefId)> {
        self.trait_method_ids_by_binding
            .get(&(
                self.current_module.clone(),
                self.current_module_idx,
                trait_name.to_string(),
                method_name.to_string(),
            ))
            .cloned()
            .or_else(|| {
                self.trait_method_ids_for_key(&self.trait_ref_lookup_key(trait_name), method_name)
            })
    }

    /// The trait and method declaration IDs for `method_name` declared by the
    /// trait registered under `trait_defs` key `key`.
    pub(in crate::check) fn trait_method_ids_for_key(
        &self,
        key: &str,
        method_name: &str,
    ) -> Option<(crate::DefId, crate::DefId)> {
        // `trait_method_ids` is keyed by the trait method's minted path, so
        // the key has to be resolved to the trait's own identity first: a
        // `trait_defs` key is a registry spelling and a flat-imported trait is
        // registered under its bare name while the declaration renders under
        // the file that declares it.
        let declaring_trait = self
            .lookup_declaration(key)
            .map_or(key, |declaration| declaration.full_path());
        self.trait_method_ids
            .get(&format!("{declaring_trait}::{method_name}"))
            .cloned()
    }
}

/// Peel the `Ok` payload type out of a `Result<T, E>` `Ty`. Returns `None` for a
/// non-`Result` type or a malformed (wrong-arity) one. Used to recover the wire
/// type from a `from_json`/`from_yaml` `Result<Self, string>` return so the codec
/// rewrite carries the produced wire type, not the `Result` wrapper.
fn result_ok_payload(ty: &Ty) -> Option<Ty> {
    if let Ty::Named {
        builtin: Some(BuiltinType::Result),
        args,
        ..
    } = ty
    {
        if args.len() == 2 {
            return Some(args[0].clone());
        }
    }
    None
}

/// Which builtin collection a [`check_collection_method`](Checker::check_collection_method)
/// call is resolving.  This is the single discriminant the descriptor-driven
/// resolver dispatches on; it carries no per-method behaviour itself.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(in crate::check) enum CollectionKind {
    HashMap,
    HashSet,
}

impl CollectionKind {
    /// User-facing collection name used in arity contexts and `UndefinedMethod`
    /// diagnostics (e.g. `` `HashMap::insert` ``, `no method `x` on Vec`).
    fn name(self) -> &'static str {
        match self {
            CollectionKind::HashMap => "HashMap",
            CollectionKind::HashSet => "HashSet",
        }
    }

    fn builtin(self) -> BuiltinType {
        match self {
            CollectionKind::HashMap => BuiltinType::HashMap,
            CollectionKind::HashSet => BuiltinType::HashSet,
        }
    }
}

/// Outcome of [`Checker::record_clone_admissibility`].
#[derive(Debug)]
pub(in crate::check) enum RecordCloneAdmissibility {
    /// The record can be cloned end-to-end via
    /// `__hew_record_clone_inplace_<R>`.
    Admissible,
    /// The record itself, or a transitively reachable field/payload, carries an
    /// affine `#[resource]` / `#[linear]` contract. Structural record clone
    /// cannot invent a semantic duplicate for such a value.
    AffineValue {
        type_name: String,
        marker: hew_parser::ast::ResourceMarker,
        member: String,
    },
    /// The record (or a transitive field) contains an opaque handle; fail closed.
    OpaqueField { opaque_name: String, member: String },
    /// A stored member has no semantic clone capability.
    MissingClone { member: String, member_ty: Ty },
    /// The record has un-substituted generic type parameters; not yet supported.
    GenericRecord,
    /// The receiver is a bare type parameter (`x: T`) carrying a `Clone` bound
    /// (`fn f<T: Clone>(x: T)`). The clone is admitted and deferred to
    /// monomorphization: MIR re-lowers the body per concrete instantiation,
    /// `subst_ty(T)` resolves the concrete leaf, and the per-value-class clone
    /// dispatch (`lower.rs` `RecordCloneCall`) emits the matching copy path.
    /// No bare-name thunk is seeded — `T` names no monomorphic layout.
    AbstractParamClone,
    /// The receiver is a user enum, clone-eligible via the enum twin of the
    /// record thunk (`__hew_enum_clone_inplace_<E>`). `enum_name` is the bare
    /// declared name; MIR keys the synthesised helper by the monomorphised
    /// tagged-union layout (`Maybe$$i64` for a generic instantiation).
    EnumClone { enum_name: String },
    /// The named type is not a clone-eligible record kind (actor, machine).
    NotARecord,
}

#[derive(Debug)]
enum CloneCapabilityBlocker {
    Affine {
        type_name: String,
        marker: hew_parser::ast::ResourceMarker,
        member: String,
    },
    Opaque {
        type_name: String,
        member: String,
    },
    Missing {
        member: String,
        member_ty: Ty,
    },
}

/// Pure-data shape of a single collection-method argument slot.
///
/// Templates name the *type shape* an argument is checked against, instantiated
/// against the receiver's concrete `K`/`V`/`elem` at the call site.  They are
/// deliberately data-only: the genuinely divergent argument *checking strategy*
/// (e.g. `HashSet`'s `check_hashset_element_arg` coercion) stays a code-side hook
/// in the driver, not a template flag (see `dedup-semantic-boundary`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::check) enum ArgTemplate {
    /// `HashMap` key type `K`.
    Key,
    /// `HashMap` value type `V`.
    Value,
    /// `HashSet` element type `elem`.
    Elem,
}

/// Pure-data shape of a collection-method return type, instantiated against the
/// receiver's concrete `K`/`V`/`elem`/`Self` at the call site.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::check) enum RetTemplate {
    Unit,
    Bool,
    I64,
    /// `Vec<K>` (`HashMap` `keys`).
    VecOfKey,
    /// `Vec<V>` (`HashMap` `values`).
    VecOfVal,
    /// `Vec<(K, V)>` (`HashMap` `entries`).
    VecOfPair,
    /// `Vec<T>` (`HashSet` `to_vec`).
    VecOfElem,
    /// The receiver collection type itself (`clone`).
    SelfTy,
}

/// The data descriptor for one `(collection, method)` pair: arity, argument
/// shape, and return shape.  This is the single source of truth for the shared
/// arity → arg → return *walk*; the per-collection element validation, lowering
/// facts and recording remain code-side hooks
/// (`dedup-semantic-boundary`: centralise the walk, not the decision).
#[derive(Clone, Copy, Debug)]
pub(in crate::check) struct CollectionMethodDesc {
    /// `Some(n)` checks arity against `n`; `None` deliberately skips the arity
    /// check (preserving `len`/`is_empty` arms that historically never called
    /// `check_arity`).
    arity: Option<usize>,
    arg_templates: &'static [ArgTemplate],
    ret: RetTemplate,
}

const fn desc(
    arity: Option<usize>,
    arg_templates: &'static [ArgTemplate],
    ret: RetTemplate,
) -> CollectionMethodDesc {
    CollectionMethodDesc {
        arity,
        arg_templates,
        ret,
    }
}

/// The descriptor table: the pure-data front-half admission shape for every
/// table-driven builtin collection method.  Returns `None` for methods that are
/// unknown (→ fail-closed fallback). Vec is sourced from `std/builtins.hew`.
#[allow(
    clippy::match_same_arms,
    reason = "rows are kept per-method even when arity/arg/ret coincide; their downstream validation/record hooks differ (e.g. HashMap remove uses the owned validator, contains_key the key_value one)"
)]
fn collection_method_desc(kind: CollectionKind, method: &str) -> Option<CollectionMethodDesc> {
    use ArgTemplate::{Elem, Key, Value};
    use RetTemplate::{
        Bool, SelfTy, Unit, VecOfElem, VecOfKey, VecOfPair, VecOfVal, I64 as RetI64,
    };
    Some(match kind {
        CollectionKind::HashMap => match method {
            "insert" => desc(Some(2), &[Key, Value], Unit),
            // `get` is intentionally ABSENT: it is routed through the
            // `Index<K>` trait (`<HashMap<K, V> as Index>::get -> Option<V>`)
            // so the accessor model is uniform with `Vec`. `check_hashmap_method`
            // has an explicit `get` arm that records the `Index` primitive-trait
            // dispatch and the `hew_hashmap_get_layout` resolved call; the bare
            // `m[k]` read is the trapping `Index::at` (`-> V`).
            //
            // `remove` is likewise intentionally ABSENT: it projects
            // `Option<V>` (A233), so `check_hashmap_method` has an explicit
            // `remove` arm — mirroring `get` — that records the
            // `hew_hashmap_remove_take_layout` move-out call (drop the key,
            // MOVE the value out into the `Some` payload).
            "contains_key" => desc(Some(1), &[Key], Bool),
            "keys" => desc(Some(0), &[], VecOfKey),
            "values" => desc(Some(0), &[], VecOfVal),
            "entries" => desc(Some(0), &[], VecOfPair),
            "clone" => desc(Some(0), &[], SelfTy),
            "len" => desc(None, &[], RetI64),
            "is_empty" => desc(Some(0), &[], Bool),
            "clear" => desc(Some(0), &[], Unit),
            _ => return None,
        },
        CollectionKind::HashSet => match method {
            "insert" => desc(Some(1), &[Elem], Bool),
            "contains" | "remove" => desc(Some(1), &[Elem], Bool),
            "clone" => desc(Some(0), &[], SelfTy),
            "to_vec" => desc(Some(0), &[], VecOfElem),
            "len" => desc(None, &[], RetI64),
            "is_empty" => desc(None, &[], Bool),
            "clear" => desc(Some(0), &[], Unit),
            _ => return None,
        },
    })
}

/// The receiver's concrete type arguments for a collection method call, carried
/// through the descriptor-driven driver.  Only the fields relevant to a given
/// `CollectionKind` are meaningful (`HashMap` uses `key`/`val`; `HashSet`/`Vec`
/// use `elem`); the rest hold placeholder values.
pub(in crate::check) struct CollectionTyCx {
    key: Ty,
    val: Ty,
    elem: Ty,
}

impl CollectionTyCx {
    /// `HashMap<K, V>` receiver type context.
    fn hashmap(key: Ty, val: Ty) -> Self {
        Self {
            key,
            val,
            elem: Ty::Unit,
        }
    }

    /// `HashSet<elem>` receiver type context.
    fn hashset(elem: Ty) -> Self {
        Self {
            key: Ty::Unit,
            val: Ty::Unit,
            elem,
        }
    }

    /// Reconstruct the concrete receiver `Ty` (carrying type arguments) for the
    /// given collection kind, so user-trait dispatch on a builtin receiver can
    /// bind the impl's type parameters from the element/key/value types.
    /// `HashMap<K, V>` → `[K, V]`, `HashSet<E>` / `Vec<E>` → `[E]`.
    fn receiver_with_args(&self, kind: CollectionKind) -> Ty {
        let args = match kind {
            CollectionKind::HashMap => vec![self.key.clone(), self.val.clone()],
            CollectionKind::HashSet => vec![self.elem.clone()],
        };
        Ty::Named {
            builtin: Some(kind.builtin()),
            name: kind.name().to_string(),
            args,
        }
    }
}

/// Map a checked receiver type to its `RuntimeCallFamily::IntMethod`/
/// `IntArith` width, covering every integer width Hew has.
fn int_method_width(ty: &Ty) -> Option<IntMethodWidth> {
    match ty {
        Ty::I8 => Some(IntMethodWidth::I8),
        Ty::I16 => Some(IntMethodWidth::I16),
        Ty::I32 => Some(IntMethodWidth::I32),
        Ty::I64 => Some(IntMethodWidth::I64),
        Ty::Isize => Some(IntMethodWidth::Isize),
        Ty::U8 => Some(IntMethodWidth::U8),
        Ty::U16 => Some(IntMethodWidth::U16),
        Ty::U32 => Some(IntMethodWidth::U32),
        Ty::U64 => Some(IntMethodWidth::U64),
        Ty::Usize => Some(IntMethodWidth::Usize),
        _ => None,
    }
}

/// Test/inspection accessor for the Stage B `HashMap` / `HashSet` dispatch
/// registry seed (W4.001 Stage B). Returns the same `ImplRegistry` the
/// checker consumes internally; exposed so the
/// `resolved_call_kernel_symbols` and `resolved_call_hashmap_scalar_k_unit`
/// gates can enumerate `MethodTarget.symbol_name` strings without
/// constructing a full typecheck pipeline.
///
/// **Stable across C0b only.** This accessor exists for the duration of
/// the Stage B transitional registry; it dies with that registry at
/// Stage C (DI-017 combined-commit).
#[must_use]
#[doc(hidden)]
pub fn collection_dispatch_registry_for_tests() -> ImplRegistry {
    collection_dispatch_registry_impl()
}

#[allow(
    clippy::too_many_lines,
    reason = "transitional registry spells out every collection method target"
)]
fn collection_dispatch_registry_impl() -> ImplRegistry {
    let mut registry = ImplRegistry::new();
    let hashmap_pattern = TyPattern::App {
        ctor: "HashMap".to_string(),
        args: vec![
            TyPattern::Var("K".to_string()),
            TyPattern::Var("V".to_string()),
        ],
    };
    registry.register(ImplDef {
        trait_name: "Map".to_string(),
        self_pattern: hashmap_pattern,
        where_bounds: vec![
            Bound {
                trait_name: MarkerTrait::Hash,
                var: "K".to_string(),
            },
            Bound {
                trait_name: MarkerTrait::Eq,
                var: "K".to_string(),
            },
        ],
        methods: vec![
            (
                "insert".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashmap_insert_layout".to_string(),
                    family: MethodTargetFamily::HashMap(HashMapMethod::Insert),
                    abi: RuntimeAbi::ByRefMut,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "get".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashmap_get_layout".to_string(),
                    family: MethodTargetFamily::HashMap(HashMapMethod::Get),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "contains_key".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashmap_contains_key_layout".to_string(),
                    family: MethodTargetFamily::HashMap(HashMapMethod::ContainsKey),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "remove".to_string(),
                MethodTarget {
                    // `remove(k) -> Option<V>` moves the value out (A233); the
                    // kernel drops the key and MOVES the value into the `Some`
                    // payload (the bool-returning `hew_hashmap_remove_layout`
                    // that drops BOTH K and V remains for HashSet / callers that
                    // discard the value).
                    symbol_name: "hew_hashmap_remove_take_layout".to_string(),
                    family: MethodTargetFamily::HashMap(HashMapMethod::Remove),
                    abi: RuntimeAbi::ByRefMut,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "len".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashmap_len_layout".to_string(),
                    family: MethodTargetFamily::HashMap(HashMapMethod::Len),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "is_empty".to_string(),
                MethodTarget {
                    // HIR composes semantic Len == 0 from this typed method.
                    // There is no separate runtime entry point to invoke.
                    symbol_name: String::new(),
                    family: MethodTargetFamily::HashMap(HashMapMethod::IsEmpty),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "keys".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashmap_keys_layout".to_string(),
                    family: MethodTargetFamily::HashMap(HashMapMethod::Keys),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "values".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashmap_values_layout".to_string(),
                    family: MethodTargetFamily::HashMap(HashMapMethod::Values),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "entries".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashmap_entries_layout".to_string(),
                    family: MethodTargetFamily::HashMap(HashMapMethod::Entries),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "clone".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashmap_clone_layout".to_string(),
                    family: MethodTargetFamily::HashMap(HashMapMethod::Clone),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "clear".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashmap_clear_layout".to_string(),
                    family: MethodTargetFamily::HashMap(HashMapMethod::Clear),
                    abi: RuntimeAbi::ByRefMut,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
        ],
    });
    registry.register(ImplDef {
        trait_name: "Set".to_string(),
        self_pattern: TyPattern::App {
            ctor: "HashSet".to_string(),
            args: vec![TyPattern::Var("T".to_string())],
        },
        where_bounds: vec![
            Bound {
                trait_name: MarkerTrait::Hash,
                var: "T".to_string(),
            },
            Bound {
                trait_name: MarkerTrait::Eq,
                var: "T".to_string(),
            },
        ],
        methods: vec![
            (
                "insert".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashset_insert_layout".to_string(),
                    family: MethodTargetFamily::HashSet(HashSetMethod::Insert),
                    abi: RuntimeAbi::ByRefMut,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "contains".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashset_contains_layout".to_string(),
                    family: MethodTargetFamily::HashSet(HashSetMethod::Contains),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "remove".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashset_remove_layout".to_string(),
                    family: MethodTargetFamily::HashSet(HashSetMethod::Remove),
                    abi: RuntimeAbi::ByRefMut,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "len".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashset_len_layout".to_string(),
                    family: MethodTargetFamily::HashSet(HashSetMethod::Len),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "is_empty".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashset_is_empty_layout".to_string(),
                    family: MethodTargetFamily::HashSet(HashSetMethod::IsEmpty),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "clone".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashset_clone_layout".to_string(),
                    family: MethodTargetFamily::HashSet(HashSetMethod::Clone),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                // Element snapshot into an owned `Vec<T>` — the projection the
                // `for x in s` desugar consumes. Borrows the set (reads its
                // elements, clones each into the fresh Vec); does not consume.
                "to_vec".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashset_to_vec_layout".to_string(),
                    family: MethodTargetFamily::HashSet(HashSetMethod::ToVec),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "clear".to_string(),
                MethodTarget {
                    symbol_name: "hew_hashset_clear_layout".to_string(),
                    family: MethodTargetFamily::HashSet(HashSetMethod::Clear),
                    abi: RuntimeAbi::ByRefMut,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
        ],
    });
    registry.register(ImplDef {
        trait_name: "Seq".to_string(),
        self_pattern: TyPattern::App {
            ctor: "Vec".to_string(),
            args: vec![TyPattern::Var("T".to_string())],
        },
        where_bounds: vec![],
        methods: crate::vec_authority::method_specs()
            .iter()
            .map(|spec| {
                (
                    spec.name.clone(),
                    MethodTarget {
                        symbol_name: format!("hew_vec_{}_FAMILY", spec.name),
                        family: MethodTargetFamily::Vec(spec.method),
                        abi: spec.method.runtime_abi(),
                        call_hint: CallAbiHint::RuntimeShim,
                        consumes_receiver: false,
                    },
                )
            })
            .collect(),
    });
    registry
}
