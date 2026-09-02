#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::builtin_names::BuiltinNamedType;
use crate::check::admissibility::{
    compute_copy_record_layout, hash_key_record_layout, identity_aggregate_layout,
};
use crate::check::calls::SignatureArgApplication;
use crate::check::dispatch::resolve_method_call;
use crate::check::types::GenericCallee;
use crate::check::types::{BareActorResolution, DeferredBuiltinCloneAdmission};
use crate::hash_eligibility::{ty_is_hash_eligible_with_resources, HashEligibility};
use crate::lowering_facts::{
    hashmap_layout_key_fact, hashmap_layout_key_layout_value_fact, hashset_layout_fact,
    HashMapValueType,
};
use crate::method_resolution::{
    collect_method_sigs_for_receiver, instantiate_stdlib_method_sig, lookup_builtin_method_sig,
    lookup_named_method_sig as shared_lookup_named_method_sig,
};
use crate::stdlib::{STD_NET_CONNECTION, STD_NET_LISTENER};
use crate::BuiltinType;

/// Resolve the closed set of compiler-lowered active transport attach methods.
///
/// The fully-qualified receiver identity is load-bearing. A user record may be
/// named `Connection`, `TlsStream`, or `Conn`; admitting a short-name match
/// would replace its ordinary inherent method with a runtime ABI call that
/// expects an opaque transport handle and actor PID.
fn transport_attach_runtime_symbol(receiver_name: &str, method: &str) -> Option<&'static str> {
    if method != "attach" {
        return None;
    }
    match receiver_name {
        STD_NET_CONNECTION => Some("hew_tcp_attach_local"),
        "std.net.tls.TlsStream" => Some("hew_tls_attach_local"),
        "std.net.websocket.Conn" => Some("hew_ws_attach_local"),
        _ => None,
    }
}

impl Checker {
    /// Return the checker-minted trait declaration IDs for a call-site trait
    /// spelling. Import aliases are resolved through their full source owner;
    /// short registry keys remain compatibility-only and are never used to mint
    /// a fresh ID here.
    pub(super) fn trait_method_call_target_ids(
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
                // `trait_method_ids` is keyed by the trait method's minted
                // path, so the trait reference has to be resolved to the
                // trait's own identity first: a `trait_defs` key is a
                // registry spelling and a flat-imported trait is registered
                // under its bare name while the declaration renders under the
                // file that declares it.
                let lookup_key = self.trait_ref_lookup_key(trait_name);
                let declaring_trait = self
                    .identity
                    .declaration_by_path(&lookup_key)
                    .map_or(lookup_key, |declaration| {
                        declaration.full_path().to_string()
                    });
                self.trait_method_ids
                    .get(&format!("{declaring_trait}::{method_name}"))
                    .cloned()
            })
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
pub(super) enum CollectionKind {
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
pub(super) enum RecordCloneAdmissibility {
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
    /// A stored member is a refcounted shared handle (`Rc`/`Weak`) whose
    /// aggregate-ingress retain is missing, so the composite drop plan
    /// over-releases. See `CloneCapabilityBlocker::UnbalancedSharedHandle`.
    UnbalancedSharedHandle { type_name: String, member: String },
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
    /// A refcounted shared handle (`Rc`/`Weak`) sitting INSIDE an aggregate.
    ///
    /// Cloning the handle itself is a retain and is fine; the aggregate is not,
    /// because aggregate ingress of an `Rc` emits no retain while both the
    /// source binder and the aggregate's composite drop release it (see
    /// `alias_moved_owned_operand` in `hew-mir/src/lower/ownership.rs`, which
    /// exempts only `string`/`bytes` from the `AggregateAlias` marker because
    /// only those have an ingress-retain derivation). Admitting the clone would
    /// hand codegen a plan whose inverse drop over-releases.
    ///
    /// WHY a refusal rather than a fix here: the missing ingress retain is not
    /// a clone bug — `let pair = (shared, "tag");` aborts with
    /// `Rc double-free` on `origin/main` with no `clone` in the program at all.
    /// WHEN obsolete: when `Rc`/`Weak` gain an aggregate-ingress retain
    /// derivation alongside `StringRetain`, at which point this arm is deleted
    /// and the member walks through as a plain retain-on-clone leaf.
    /// WHAT the real solution looks like: an `RcRetain` ingress instruction
    /// with the same prover/codegen treatment `StringRetain` already has.
    UnbalancedSharedHandle {
        type_name: String,
        member: String,
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
pub(super) enum ArgTemplate {
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
pub(super) enum RetTemplate {
    Unit,
    Bool,
    I64,
    /// `Vec<K>` (`HashMap` `keys`).
    VecOfKey,
    /// `Vec<V>` (`HashMap` `values`).
    VecOfVal,
    /// `Vec<(K, V)>` (`HashMap` `entries`).
    VecOfPair,
    /// The receiver collection type itself (`clone`).
    SelfTy,
}

/// The data descriptor for one `(collection, method)` pair: arity, argument
/// shape, and return shape.  This is the single source of truth for the shared
/// arity → arg → return *walk*; the per-collection element validation, lowering
/// facts and recording remain code-side hooks
/// (`dedup-semantic-boundary`: centralise the walk, not the decision).
#[derive(Clone, Copy, Debug)]
pub(super) struct CollectionMethodDesc {
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
    use RetTemplate::{Bool, SelfTy, Unit, VecOfKey, VecOfPair, VecOfVal, I64 as RetI64};
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
            "is_empty" => desc(None, &[], Bool),
            "clear" => desc(Some(0), &[], Unit),
            _ => return None,
        },
        CollectionKind::HashSet => match method {
            "insert" => desc(Some(1), &[Elem], Bool),
            "contains" | "remove" => desc(Some(1), &[Elem], Bool),
            "clone" => desc(Some(0), &[], SelfTy),
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
pub(super) struct CollectionTyCx {
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

impl Checker {
    /// Reject a mutable-receiver store-back through a non-receiver by-value
    /// parameter when the declaration was admitted only because some separate
    /// projection reaches shared collection storage.
    ///
    /// For example, `VecIter<T>` contains a shared `Vec<T>` but its `idx`
    /// cursor is inline value storage. Calling `next()` on a by-value
    /// `VecIter<T>` parameter advances only the callee's copy. Method bodies
    /// can be external or dynamically dispatched, so the call site cannot
    /// prove that their mutation crosses the shared boundary; reject
    /// fail-closed and direct users to an explicit collection projection.
    fn reject_private_param_mutable_receiver_call(
        &mut self,
        receiver_name: &str,
        operation: &str,
        span: &Span,
    ) {
        let Some(binding) = self.env.lookup_ref(receiver_name) else {
            return;
        };
        if !binding.is_param() || binding.is_receiver() {
            return;
        }
        let binding_ty = self.subst.resolve(&binding.ty);
        if !self.param_ty_has_caller_visible_projection(&binding_ty) {
            // Value-only aggregates were already rejected at their parameter
            // declaration. Avoid a second diagnostic at every use.
            return;
        }
        self.report_error_with_suggestions(
            TypeErrorKind::MutabilityError,
            span,
            format!(
                "`{receiver_name}` is a by-value parameter; {operation} writes back only to its \
                 private copy, so the mutation is not proven caller-visible"
            ),
            vec![
                "return the modified value to the caller".to_string(),
                "mutate through a shared collection projection instead".to_string(),
            ],
        );
    }

    fn numeric_method_signedness(ty: &Ty) -> Option<NumericSignedness> {
        match ty {
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::Isize => Some(NumericSignedness::Signed),
            Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64 | Ty::Usize => Some(NumericSignedness::Unsigned),
            _ => None,
        }
    }

    fn numeric_method_width(ty: &Ty) -> Option<NumericWidth> {
        match ty {
            Ty::I8 | Ty::U8 => Some(NumericWidth::Bits(8)),
            Ty::I16 | Ty::U16 => Some(NumericWidth::Bits(16)),
            Ty::I32 | Ty::U32 => Some(NumericWidth::Bits(32)),
            Ty::I64 | Ty::U64 => Some(NumericWidth::Bits(64)),
            Ty::Isize | Ty::Usize => Some(NumericWidth::Pointer),
            _ => None,
        }
    }

    pub(super) fn record_hashset_lowering_fact(&mut self, span: &Span, elem_ty: &Ty) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        // If deferred admission was already recorded for this span, the
        // lowering-fact finalizer becomes the sole authority for any
        // InferenceFailed diagnostic at this site.  Remove the deferred entry
        // to prevent a duplicate error from finalize_hashset_admission.
        self.deferred_hashset_admission.remove(&key);
        self.pending_lowering_facts.insert(
            key,
            PendingLoweringFact::hashset(elem_ty.clone(), self.current_module.clone()),
        );
    }

    /// Drain `pending_lowering_facts`, resolve element types through the
    /// substitution, and materialize concrete [`LoweringFact`] entries.
    ///
    /// Any fact whose element type is still unresolved after inference emits a
    /// checker error and is **not** inserted into the returned map.  Downstream
    /// codegen (`requireLoweringFactOf`) will detect the missing entry and fail
    /// closed rather than guessing.
    #[allow(
        clippy::too_many_lines,
        reason = "branching over HashSet element types including the new C-2c Named path is \
                  inherently wide; factoring into sub-functions would obscure the single-pass flow"
    )]
    pub(super) fn finalize_lowering_facts(&mut self) -> HashMap<SpanKey, LoweringFact> {
        let pending = std::mem::take(&mut self.pending_lowering_facts);
        let mut result = HashMap::with_capacity(pending.len());
        let mut new_errors: Vec<crate::error::TypeError> = Vec::new();
        // Track which unresolved TypeVars have already produced a diagnostic so
        // that repeated method calls on the same unresolved HashSet (e.g.
        // `s.len(); s.is_empty()`) emit exactly one InferenceFailed rather than
        // one per call site.  Each unique unresolved root var gets one error.
        let mut reported_unresolved_vars: std::collections::HashSet<TypeVar> =
            std::collections::HashSet::new();

        for (span_key, pending_fact) in pending {
            let resolved_ty = self
                .subst
                .resolve(&pending_fact.hashset_element_ty)
                .materialize_literal_defaults();
            // Guard 1: element type is already erroneous — a prior diagnostic was
            // reported on the upstream expression.  Drop the pending fact silently;
            // downstream codegen fails closed via the absent lowering-fact entry.
            // Emitting a new diagnostic here would produce a spurious secondary
            // "element type is unresolved" error even though inference completed
            // correctly — it just completed to Ty::Error.
            if resolved_ty.contains_error() {
                continue;
            }
            match LoweringFact::from_hashset_element_type(&resolved_ty) {
                Ok(fact) => {
                    result.insert(span_key, fact);
                }
                Err(LoweringFactError::UnresolvedHashSetElementType) => {
                    // Inference did not resolve the element type by the checker
                    // boundary.  Emit a clear diagnostic (at most once per
                    // unique unresolved TypeVar) and prune the fact so
                    // downstream codegen fails closed via requireLoweringFactOf.
                    if let Ty::Var(var) = resolved_ty {
                        if !reported_unresolved_vars.insert(var) {
                            // Already emitted for this root var (another call
                            // site on the same unresolved set).  Skip to avoid
                            // spraying one error per method-call site.
                            continue;
                        }
                    }
                    let span = span_key.start..span_key.end;
                    let mut err = crate::error::TypeError::new(
                        TypeErrorKind::InferenceFailed,
                        span,
                        "cannot lower HashSet: element type is unresolved at the checker \
                         boundary — add an explicit type annotation, e.g. \
                         `HashSet<i64>` or `HashSet<String>`"
                            .to_string(),
                    );
                    if let Some(module) = &pending_fact.source_module {
                        err = err.with_source_module(module.clone());
                    }
                    new_errors.push(err);
                    // Fact NOT inserted — downstream will fail closed.
                }
                Err(LoweringFactError::UnsupportedHashSetElementType { .. }) => {
                    // For Named (record) element types: run hash-eligibility (C-2c).
                    // The inline `validate_hashset_element_type` pass already admitted Named
                    // types optimistically; here we either produce a HashSetLoweringFact
                    // (Eligible) or a diagnostic (ineligible).
                    if let Ty::Named { name, .. } = &resolved_ty {
                        let type_defs_snapshot = self.type_defs.clone();
                        match ty_is_hash_eligible_with_resources(
                            &resolved_ty,
                            &type_defs_snapshot,
                            self.registry.resource_type_names(),
                        ) {
                            HashEligibility::Eligible => {
                                let type_def = self.lookup_type_def(name);
                                let layout =
                                    identity_aggregate_layout(&resolved_ty).or_else(|| {
                                        type_def.as_ref().and_then(|td| {
                                            hash_key_record_layout(td, &type_defs_snapshot)
                                        })
                                    });
                                if let Some((elem_size, elem_align)) = layout {
                                    let fact =
                                        hashset_layout_fact(name.clone(), elem_size, elem_align);
                                    self.hashset_layout_facts.insert(span_key, fact);
                                    // Fact inserted into hashset_layout_facts;
                                    // NOT inserted into lowering_facts result.
                                } else if type_def.is_some() {
                                    let span = span_key.start..span_key.end;
                                    let mut err = crate::error::TypeError::new(
                                        TypeErrorKind::InvalidOperation,
                                        span,
                                        format!(
                                            "`HashSet` element type `{name}` has zero size \
                                             or contains a type whose layout cannot be \
                                             determined; layout element types must have \
                                             non-zero size",
                                        ),
                                    );
                                    if let Some(module) = &pending_fact.source_module {
                                        err = err.with_source_module(module.clone());
                                    }
                                    new_errors.push(err);
                                }
                                // TypeDef not found — silently drop; lookup failure
                                // is a pre-existing error from the type-resolution pass.
                            }
                            HashEligibility::IneligibleManaged(bad_ty) => {
                                let span = span_key.start..span_key.end;
                                let msg = if bad_ty == resolved_ty {
                                    format!(
                                        "layout-managed HashSet elements require Copy; \
                                         `{name}` is an indirect (managed) record and is not yet \
                                         supported as a layout HashSet element"
                                    )
                                } else {
                                    format!(
                                        "`HashSet` element type `{name}` contains a managed field \
                                         (`{}`); layout-element hashing requires fixed-size Copy \
                                         fields — use a type without heap-managed fields",
                                        bad_ty.user_facing(),
                                    )
                                };
                                let mut err = crate::error::TypeError::new(
                                    TypeErrorKind::InvalidOperation,
                                    span,
                                    msg,
                                );
                                if let Some(module) = &pending_fact.source_module {
                                    err = err.with_source_module(module.clone());
                                }
                                new_errors.push(err);
                            }
                            HashEligibility::IneligibleOwned(bad_ty)
                            | HashEligibility::IneligibleTuple(bad_ty) => {
                                let span = span_key.start..span_key.end;
                                let mut err = crate::error::TypeError::new(
                                    TypeErrorKind::InvalidOperation,
                                    span,
                                    format!(
                                        "`HashSet` element type `{name}` contains a field of type \
                                         `{}` which is not a fixed-size Copy type; layout element \
                                         types require all fields to be fixed-width primitives or \
                                         nested Copy records",
                                        bad_ty.user_facing(),
                                    ),
                                );
                                if let Some(module) = &pending_fact.source_module {
                                    err = err.with_source_module(module.clone());
                                }
                                new_errors.push(err);
                            }
                            HashEligibility::IneligibleNamedNonRecord(bad_ty) => {
                                let span = span_key.start..span_key.end;
                                let mut err = crate::error::TypeError::new(
                                    TypeErrorKind::InvalidOperation,
                                    span,
                                    format!(
                                        "`HashSet` element type `{}` must be a `record`-keyword type \
                                         to use the layout element ABI; non-record named types are \
                                         not guaranteed to be Copy value-semantic",
                                        bad_ty.user_facing(),
                                    ),
                                );
                                if let Some(module) = &pending_fact.source_module {
                                    err = err.with_source_module(module.clone());
                                }
                                new_errors.push(err);
                            }
                            HashEligibility::IneligibleVar | HashEligibility::IneligibleError => {
                                // Ty::Var / Ty::Error already guarded above; silently drop.
                            }
                        }
                    }
                    // For non-Named types that are unsupported: the checker already
                    // rejected them via validate_hashset_element_type; skip silently
                    // to avoid a duplicate diagnostic.
                }
            }
        }

        self.errors.extend(new_errors);
        result
    }

    /// Drain `deferred_hashmap_admission`, resolve key/value types through the
    /// current substitution, and fail closed on any that are still unresolved
    /// or error-typed at the checker boundary.
    ///
    /// * `Ty::Var` → `InferenceFailed`: inference did not resolve the type.
    /// * `Ty::Error` → silent drop: upstream already emitted a diagnostic.
    /// * `Ty::Named` key → hash-eligibility check via C-2a predicate; produces a
    ///   `HashMapLoweringFact` on success or a diagnostic on failure.
    /// * Fully-resolved scalar (String) unsupported pairs → already caught inline;
    ///   silently skipped here to avoid duplicate diagnostics.
    #[allow(
        clippy::too_many_lines,
        clippy::single_match_else,
        reason = "branching over HashEligibility + key/value layout paths is inherently wide; \
                  factoring into sub-functions would obscure the flow more than help"
    )]
    pub(super) fn finalize_hashmap_admission(&mut self) {
        let checks = std::mem::take(&mut self.deferred_hashmap_admission);
        let mut new_errors: Vec<crate::error::TypeError> = Vec::new();
        let mut new_layout_facts: Vec<(SpanKey, crate::lowering_facts::HashMapLoweringFact)> =
            Vec::new();
        // Track which (key_var, val_var) pairs have already produced a
        // diagnostic so that repeated method calls on the same unresolved
        // HashMap (e.g. `m.len(); m.is_empty()`) emit exactly one
        // InferenceFailed rather than one per call site.
        let mut reported_var_pairs: std::collections::HashSet<(Option<TypeVar>, Option<TypeVar>)> =
            std::collections::HashSet::new();

        for (span_key, check) in checks {
            let resolved_key = self
                .subst
                .resolve(&check.key_ty)
                .materialize_literal_defaults();
            let resolved_val = self
                .subst
                .resolve(&check.val_ty)
                .materialize_literal_defaults();

            // Already-errored types: fail closed without cascading.
            if matches!(resolved_key, Ty::Error) || matches!(resolved_val, Ty::Error) {
                continue;
            }

            // Bare type-parameter keys are checked against their declared
            // `K: Hash + Eq` bounds at the call site.  They have no concrete
            // layout fact before monomorphization; the HashMap handle bakes the
            // substituted K/V layouts at `HashMap::new()` for each instantiation.
            if check.is_abstract_key_param {
                continue;
            }

            // Still unresolved at the checker boundary → fail closed, but
            // deduplicate across multiple call sites that share the same
            // unresolved root vars.
            if matches!(resolved_key, Ty::Var(_)) || matches!(resolved_val, Ty::Var(_)) {
                let key_var = if let Ty::Var(v) = resolved_key {
                    Some(v)
                } else {
                    None
                };
                let val_var = if let Ty::Var(v) = resolved_val {
                    Some(v)
                } else {
                    None
                };
                if !reported_var_pairs.insert((key_var, val_var)) {
                    // Already emitted for this root (key_var, val_var) pair.
                    continue;
                }
                let key_resolved_display = self
                    .subst
                    .resolve(&check.key_ty)
                    .materialize_literal_defaults();
                let val_resolved_display = self
                    .subst
                    .resolve(&check.val_ty)
                    .materialize_literal_defaults();
                let key_display = key_resolved_display.user_facing();
                let val_display = val_resolved_display.user_facing();
                let mut err = crate::error::TypeError::new(
                    TypeErrorKind::InferenceFailed,
                    check.span.clone(),
                    format!(
                        "cannot infer HashMap key or value type at the checker boundary \
                         (HashMap<{key_display}, {val_display}>); add an explicit type \
                         annotation, e.g. `HashMap<String, i64>`",
                    ),
                );
                if let Some(module) = check.source_module {
                    err = err.with_source_module(module);
                }
                new_errors.push(err);
                continue;
            }

            // Named record key: run hash-eligibility check and produce a
            // HashMapLoweringFact (C-2c). Fail closed with a diagnostic on
            // any ineligibility reason.
            if let Ty::Named { name: key_name, .. } = &resolved_key {
                // Collect the type_defs snapshot before borrowing self mutably below.
                let type_defs_snapshot = self.type_defs.clone();

                match ty_is_hash_eligible_with_resources(
                    &resolved_key,
                    &type_defs_snapshot,
                    self.registry.resource_type_names(),
                ) {
                    HashEligibility::Eligible => {
                        let key_type_def = self.lookup_type_def(key_name);
                        let key_layout = identity_aggregate_layout(&resolved_key).or_else(|| {
                            key_type_def
                                .as_ref()
                                .and_then(|td| hash_key_record_layout(td, &type_defs_snapshot))
                        });
                        match key_layout {
                            Some((key_size, key_align)) => {
                                // Determine value type routing.
                                match HashMapValueType::from_ty(&resolved_val) {
                                    Ok(HashMapValueType::Layout) => {
                                        // Value is also a Named record.
                                        if let Ty::Named { name: val_name, .. } = &resolved_val {
                                            let val_type_def = self.lookup_type_def(val_name);
                                            match val_type_def {
                                                Some(ref vtd) => {
                                                    match compute_copy_record_layout(
                                                        vtd,
                                                        &type_defs_snapshot,
                                                    ) {
                                                        Some((val_size, val_align)) => {
                                                            let fact =
                                                                        hashmap_layout_key_layout_value_fact(
                                                                            key_name.clone(),
                                                                            key_size,
                                                                            key_align,
                                                                            val_name,
                                                                            val_size,
                                                                            val_align,
                                                                        );
                                                            new_layout_facts.push((span_key, fact));
                                                        }
                                                        None => {
                                                            let mut err =
                                                                crate::error::TypeError::new(
                                                                    TypeErrorKind::InvalidOperation,
                                                                    check.span.clone(),
                                                                    format!(
                                                                        "`HashMap` value type `{val_name}` has zero size or contains a type whose layout cannot be determined; layout-value types must have non-zero size",
                                                                    ),
                                                                );
                                                            if let Some(module) =
                                                                check.source_module
                                                            {
                                                                err =
                                                                    err.with_source_module(module);
                                                            }
                                                            new_errors.push(err);
                                                        }
                                                    }
                                                }
                                                None => {
                                                    let mut err = crate::error::TypeError::new(
                                                        TypeErrorKind::InvalidOperation,
                                                        check.span.clone(),
                                                        format!(
                                                            "`HashMap` value type `{val_name}` is not defined; cannot compute layout for layout-key `HashMap`",
                                                        ),
                                                    );
                                                    if let Some(module) = check.source_module {
                                                        err = err.with_source_module(module);
                                                    }
                                                    new_errors.push(err);
                                                }
                                            }
                                        } else {
                                            // Should not happen: HashMapValueType::Layout implies Named.
                                            unreachable!(
                                                "HashMapValueType::Layout produced for non-Named value type"
                                            );
                                        }
                                    }
                                    Ok(val_type) => {
                                        // Scalar value path.
                                        let fact = hashmap_layout_key_fact(
                                            key_name.clone(),
                                            key_size,
                                            key_align,
                                            val_type,
                                        );
                                        new_layout_facts.push((span_key, fact));
                                    }
                                    Err(e) => {
                                        let mut err = crate::error::TypeError::new(
                                            TypeErrorKind::InvalidOperation,
                                            check.span.clone(),
                                            format!(
                                                "HashMap<{key_name}, {}> value type is not supported for layout-key HashMap: {:?}",
                                                resolved_val.user_facing(),
                                                e,
                                            ),
                                        );
                                        if let Some(module) = check.source_module {
                                            err = err.with_source_module(module);
                                        }
                                        new_errors.push(err);
                                    }
                                }
                            }
                            None => {
                                let message = if key_type_def.is_some() {
                                    format!(
                                        "HashMap key type `{key_name}` has zero size or contains a type \
                                         whose layout cannot be determined; layout keys must have non-zero size",
                                    )
                                } else {
                                    format!(
                                        "HashMap key type `{key_name}` is not defined; \
                                         cannot verify hash eligibility for layout-key HashMap",
                                    )
                                };
                                let mut err = crate::error::TypeError::new(
                                    TypeErrorKind::InvalidOperation,
                                    check.span.clone(),
                                    message,
                                );
                                if let Some(module) = check.source_module {
                                    err = err.with_source_module(module);
                                }
                                new_errors.push(err);
                            }
                        }
                    }

                    HashEligibility::IneligibleManaged(bad_ty) => {
                        // Distinguish: is the key itself a managed (indirect) record,
                        // or does it contain a managed field?
                        let msg = if bad_ty == resolved_key {
                            format!(
                                "layout-managed HashMap keys require Copy; \
                                 `{key_name}` is an indirect (managed) record and is not yet supported \
                                 as a layout HashMap key"
                            )
                        } else {
                            format!(
                                "HashMap key type `{key_name}` contains a managed field \
                                 (`{}`); layout-key hashing requires fixed-size Copy fields — \
                                 use a type without heap-managed fields as the key",
                                bad_ty.user_facing(),
                            )
                        };
                        let mut err = crate::error::TypeError::new(
                            TypeErrorKind::InvalidOperation,
                            check.span.clone(),
                            msg,
                        );
                        if let Some(module) = check.source_module {
                            err = err.with_source_module(module);
                        }
                        new_errors.push(err);
                    }

                    HashEligibility::IneligibleOwned(bad_ty) => {
                        let mut err = crate::error::TypeError::new(
                            TypeErrorKind::InvalidOperation,
                            check.span.clone(),
                            format!(
                                "HashMap key type `{key_name}` contains a field of type `{}` \
                                 which is not a fixed-size Copy type; layout keys require all fields \
                                 to be fixed-width primitives or nested Copy records",
                                bad_ty.user_facing(),
                            ),
                        );
                        if let Some(module) = check.source_module {
                            err = err.with_source_module(module);
                        }
                        new_errors.push(err);
                    }

                    HashEligibility::IneligibleTuple(_) => {
                        let mut err = crate::error::TypeError::new(
                            TypeErrorKind::InvalidOperation,
                            check.span.clone(),
                            format!(
                                "HashMap key type `{key_name}` is or contains a tuple; \
                                 tuple keys are not supported for the layout key ABI",
                            ),
                        );
                        if let Some(module) = check.source_module {
                            err = err.with_source_module(module);
                        }
                        new_errors.push(err);
                    }

                    HashEligibility::IneligibleNamedNonRecord(bad_ty) => {
                        let kind_name = self.lookup_type_def(key_name).map_or(
                            "a non-record type",
                            |td| match td.kind {
                                TypeDefKind::Enum => "an enum",
                                TypeDefKind::Struct => "a type",
                                TypeDefKind::Actor => "an actor",
                                TypeDefKind::Machine => "a machine",
                                TypeDefKind::Record => "a record",
                            },
                        );
                        let mut err = crate::error::TypeError::new(
                            TypeErrorKind::InvalidOperation,
                            check.span.clone(),
                            format!(
                                "HashMap key type `{}` must be a `record`-keyword type to use the \
                                 layout key ABI; found {kind_name} which is not guaranteed Copy \
                                 value-semantic",
                                bad_ty.user_facing(),
                            ),
                        );
                        if let Some(module) = check.source_module {
                            err = err.with_source_module(module);
                        }
                        new_errors.push(err);
                    }

                    HashEligibility::IneligibleVar | HashEligibility::IneligibleError => {
                        // Ty::Var / Ty::Error already handled above; should not reach here
                        // for a Named key. Fail closed silently.
                    }
                }
            }

            // Fully resolved scalar (String/i64/u64) unsupported pair: the inline
            // check should have already emitted a diagnostic. Skip to avoid duplicates.
        }

        self.errors.extend(new_errors);
        for (span_key, fact) in new_layout_facts {
            self.hashmap_layout_facts.insert(span_key, fact);
        }
    }

    /// Drain `deferred_hashset_admission`, resolve element types through the
    /// current substitution, and fail closed on any that are still unresolved
    /// or error-typed at the checker boundary.
    ///
    /// * `Ty::Var` → `InferenceFailed`: inference did not resolve the element type.
    /// * `Ty::Error` → silent drop: upstream already emitted a diagnostic.
    /// * Fully-resolved unsupported elements → already caught inline; silently
    ///   skipped here to avoid duplicate diagnostics.
    pub(super) fn finalize_hashset_admission(&mut self) {
        let checks = std::mem::take(&mut self.deferred_hashset_admission);
        let mut new_errors: Vec<crate::error::TypeError> = Vec::new();

        for (_span_key, check) in checks {
            let resolved = self
                .subst
                .resolve(&check.elem_ty)
                .materialize_literal_defaults();

            // Already-errored type: fail closed without cascading.
            if matches!(resolved, Ty::Error) {
                continue;
            }

            // Still unresolved at the checker boundary → fail closed.
            if matches!(resolved, Ty::Var(_)) {
                let mut err = crate::error::TypeError::new(
                    TypeErrorKind::InferenceFailed,
                    check.span.clone(),
                    format!(
                        "cannot infer HashSet element type at the checker boundary \
                         (HashSet<{}>); add an explicit type annotation, \
                         e.g. `HashSet<String>` or `HashSet<i64>`",
                        resolved.user_facing(),
                    ),
                );
                if let Some(module) = check.source_module {
                    err = err.with_source_module(module);
                }
                new_errors.push(err);
            }

            // Fully resolved but unsupported element: the inline check should
            // have already emitted a diagnostic. Skip to avoid duplicates.
        }

        self.errors.extend(new_errors);
    }

    /// Drain `deferred_vec_admission`, resolve element types through the
    /// current substitution, and fail closed on any that are still unresolved
    /// or error-typed at the checker boundary.
    ///
    /// * Any surviving inference variable inside the element type →
    ///   [`TypeErrorKind::InferenceFailed`].
    /// * `Ty::Error` (anywhere inside the element type) → silent drop:
    ///   upstream already emitted a diagnostic.
    /// * Fully-resolved types are revalidated so late-resolved unsupported
    ///   element types are rejected just like inline admission sites.
    pub(super) fn finalize_vec_admission(&mut self) {
        let checks = std::mem::take(&mut self.deferred_vec_admission);
        let mut new_errors: Vec<crate::error::TypeError> = Vec::new();
        let mut reported_unresolved_roots: std::collections::HashSet<Vec<u32>> =
            std::collections::HashSet::new();

        for (_span_key, check) in checks {
            let resolved = self
                .subst
                .resolve(&check.elem_ty)
                .materialize_literal_defaults();

            if resolved.contains_error() {
                continue;
            }

            if resolved.has_inference_var() {
                let mut unresolved_vars = HashSet::new();
                collect_unresolved_inference_vars(&resolved, &mut unresolved_vars);
                let mut unresolved_roots: Vec<u32> =
                    unresolved_vars.into_iter().map(|var| var.0).collect();
                unresolved_roots.sort_unstable();
                unresolved_roots.dedup();
                if !reported_unresolved_roots.insert(unresolved_roots) {
                    continue;
                }

                let mut err = crate::error::TypeError::new(
                    TypeErrorKind::InferenceFailed,
                    check.span.clone(),
                    format!(
                        "cannot infer Vec element type at the checker boundary \
                         (Vec<{}>); add an explicit type annotation",
                        resolved.user_facing(),
                    ),
                );
                if let Some(module) = check.source_module {
                    err = err.with_source_module(module);
                }
                new_errors.push(err);
                continue;
            }

            let _ = self.validate_resolved_vec_element_type(&resolved, &check.span);
        }

        self.errors.extend(new_errors);
    }

    /// Recheck built-in value-container clones after inference has settled.
    ///
    /// A call can be visited while its payload is still `Ty::Var`, then become
    /// affine through a later source branch even though that branch executes
    /// before the clone at runtime. Inline admission alone is therefore
    /// source-order dependent. This finalizer closes that gap using the same
    /// transitive affine authority as the immediate clone gate.
    pub(super) fn finalize_builtin_clone_admission(&mut self) {
        let checks = std::mem::take(&mut self.deferred_builtin_clone_admission);
        let mut new_errors = Vec::new();

        for (_span_key, check) in checks {
            let resolved = self
                .subst
                .resolve(&check.receiver_ty)
                .materialize_literal_defaults();
            if resolved.contains_error() || resolved.has_inference_var() {
                continue;
            }
            let Some(blocker) = self.structural_clone_blocker(&resolved) else {
                continue;
            };
            let receiver_name = resolved.user_facing().to_string();
            let message = match blocker {
                CloneCapabilityBlocker::Affine {
                    type_name,
                    marker,
                    member,
                } => Self::affine_record_clone_error_message(
                    &receiver_name,
                    &type_name,
                    marker,
                    &member,
                ),
                CloneCapabilityBlocker::Opaque { type_name, member } => format!(
                    "type `{receiver_name}` cannot be cloned because member `{member}` contains \
                     opaque value `{type_name}`"
                ),
                CloneCapabilityBlocker::Missing { member, member_ty } => format!(
                    "type `{receiver_name}` cannot be cloned because member `{member}` of type \
                     `{}` has no Clone capability",
                    member_ty.user_facing()
                ),
                CloneCapabilityBlocker::UnbalancedSharedHandle { type_name, member } => {
                    Self::unbalanced_shared_handle_clone_error_message(
                        &receiver_name,
                        &type_name,
                        &member,
                    )
                }
            };
            let mut err =
                crate::error::TypeError::new(TypeErrorKind::InvalidOperation, check.span, message);
            if let Some(module) = check.source_module {
                err = err.with_source_module(module);
            }
            new_errors.push(err);
        }

        self.errors.extend(new_errors);
    }

    /// Drain `deferred_channel_rewrites`, resolve each inner type through the
    /// current substitution, and record the correct type-specific C symbol.
    ///
    /// This must be called from `check_program` **after** all inference has
    /// settled (i.e. after `check_item` for every item in the program, and
    /// after all other inference-driving passes like `apply_deferred_range_bound_types`).
    ///
    /// * Fully resolved concrete type (`String` or integer) → select symbol and
    ///   record [`MethodCallRewrite::RewriteToFunction`].
    /// * `Ty::Error` → skip silently; a diagnostic was already emitted upstream.
    /// * `Ty::Var` (still unresolved) → emit [`TypeErrorKind::InferenceFailed`];
    ///   leave the span absent from `method_call_rewrites` so codegen fails
    ///   closed rather than silently using the wrong ABI.
    /// * Unsupported concrete type → emit [`TypeErrorKind::InvalidOperation`];
    ///   the inline validation pass may have already emitted a diagnostic, but
    ///   deferred entries bypass that guard, so we re-check here.
    #[expect(
        clippy::too_many_lines,
        reason = "deferred channel resolution validates type, ABI, and ownership together"
    )]
    pub(super) fn finalize_channel_rewrites(&mut self) {
        use crate::runtime_call::{
            ProducedArgumentBoundary as Boundary, ProducedValueAcquisition as Acquisition,
            ProducedValueOwnership as Ownership,
        };

        let deferred = std::mem::take(&mut self.deferred_channel_rewrites);
        let mut new_errors: Vec<crate::error::TypeError> = Vec::new();

        for (span_key, entry) in deferred {
            let resolved = self
                .subst
                .resolve(&entry.inner_ty)
                .materialize_literal_defaults();

            // Already-errored upstream: fail closed, no duplicate diagnostic.
            if resolved.contains_error() {
                continue;
            }

            // Still unresolved: inference did not converge on a concrete type.
            if let Ty::Var(_) = &resolved {
                let mut err = crate::error::TypeError::new(
                    TypeErrorKind::InferenceFailed,
                    span_key.start..span_key.end,
                    format!(
                        "cannot resolve channel method `{}`: inner type of \
                         {}<T> is still unknown after inference — \
                         add an explicit type annotation, e.g. \
                         `Sender<int>` or `Receiver<string>`",
                        entry.method, entry.handle_kind,
                    ),
                );
                if let Some(module) = &entry.source_module {
                    err = err.with_source_module(module.clone());
                }
                new_errors.push(err);
                // Span intentionally absent from method_call_rewrites → codegen fails closed.
                continue;
            }

            // Reject element types the layout witness cannot describe (guard
            // against deferred entries that escaped the inline validation
            // because T was Var at visit time but resolved to something the
            // queue cannot clone or drop, e.g. a Vec element).
            if !self.queue_elem_admissible(&resolved) {
                let reason = self.queue_elem_rejection_reason(&resolved);
                let mut err = crate::error::TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    span_key.start..span_key.end,
                    format!("`Channel<{resolved}>` is not supported: {reason}"),
                );
                if let Some(module) = &entry.source_module {
                    err = err.with_source_module(module.clone());
                }
                new_errors.push(err);
                continue;
            }

            // Concrete, supported type: select the correct C symbol.
            if let Some(c_symbol) = crate::stdlib::resolve_channel_method(
                &entry.handle_kind,
                &entry.method,
                Some(&resolved),
            ) {
                // Deferred channel-method resolution bypasses
                // `record_runtime_method_call_rewrite`, so derive the same
                // consume verdict from the resolved symbol here (channel
                // `Sender`/`Receiver` `close` are consuming releases).
                let consumes_receiver =
                    crate::builtin_names::runtime_symbol_consumes_receiver(c_symbol);
                // Lift the resolved symbol into the typed runtime-call
                // descriptor when the substrate enumerates it (closed set:
                // channel close peers in this branch). User-defined open-set
                // FFI strings cannot reach this site; from_c_symbol asserts
                // None below would only fire if the channel registry grew
                // a symbol the substrate doesn't know about.
                let descriptor = crate::runtime_call::RuntimeCallFamily::from_c_symbol(c_symbol)
                    .map(|family| {
                        crate::runtime_call::RuntimeCallDescriptor::new(family, None)
                            .expect("channel close family rejects elem; substrate invariant")
                    });
                self.method_call_rewrites.insert(
                    span_key.clone(),
                    MethodCallRewrite::RewriteToFunction {
                        target: descriptor.as_ref().map_or_else(
                            || CallTarget::Unsupported {
                                reason: format!(
                                    "channel runtime method `{c_symbol}` has no registered family"
                                ),
                            },
                            |descriptor| CallTarget::Runtime(descriptor.family()),
                        ),
                        c_symbol: c_symbol.to_string(),
                        descriptor,
                        extern_identity: None,
                        elem_ty: None,
                        consumes_receiver,
                        returns_receiver_identity: false,
                    },
                );
                let source_arg_count = self
                    .produced_call_arities
                    .get(&span_key)
                    .map_or(0, |(_, count)| *count);
                let ownership = match entry.method.as_str() {
                    "recv" | "try_recv" => Ownership::owned(Acquisition::Delivery),
                    "close" => Ownership::NoOwner,
                    _ => Ownership::Unknown,
                };
                self.resolved_method_call_ownership.insert(
                    span_key,
                    PendingMethodCallOwnership {
                        fact: ProducedValueFact {
                            ownership,
                            receiver_span: None,
                            receiver_boundary: Some(if consumes_receiver {
                                Boundary::Transfer
                            } else {
                                Boundary::Borrow
                            }),
                            arguments: match entry.method.as_str() {
                                "send" => vec![Boundary::Transfer; source_arg_count],
                                "recv" | "try_recv" | "close" => {
                                    vec![Boundary::Borrow; source_arg_count]
                                }
                                _ => vec![Boundary::Unknown; source_arg_count],
                            },
                        },
                        extern_identity: None,
                        resolved_result_ty: resolved,
                    },
                );
            } else {
                let mut err = crate::error::TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    span_key.start..span_key.end,
                    format!(
                        "internal compiler error: builtin {}.{} is missing runtime rewrite metadata",
                        entry.handle_kind, entry.method
                    ),
                );
                if let Some(module) = &entry.source_module {
                    err = err.with_source_module(module.clone());
                }
                new_errors.push(err);
            }
        }

        self.errors.extend(new_errors);
    }

    pub(super) fn record_method_call_receiver_kind(
        &mut self,
        span: &Span,
        kind: MethodCallReceiverKind,
    ) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        if matches!(
            self.method_call_receiver_kinds.get(&key),
            Some(MethodCallReceiverKind::LexicalBinding { .. })
        ) {
            return;
        }
        self.method_call_receiver_kinds.insert(key, kind);
    }

    /// Returns whether the qualified method name `Trait::method` is in the
    /// recognised consume-receiver set. PR 1 (#1295) ships an empty set; PR 2
    /// populates it for `Closable::close` when the trait is registered.
    fn is_consume_receiver_method(&self, qualified_name: &str) -> bool {
        self.consume_receiver_methods.contains(qualified_name)
    }

    /// Returns true if any trait impl on `type_name` registered a method
    /// named `method` that is in the recognised consume-receiver set.
    ///
    /// Stdlib `impl Closable for T { fn close }` flattens trait methods into
    /// the inherent-method table on `T`, so the dispatch at the named-type
    /// site doesn't carry the originating trait. To honour
    /// `consumes_receiver` declared on the trait, we walk the
    /// `trait_impls_set` for matching `(type, trait)` pairs and check the
    /// qualified `Trait::method` form against the consume set.
    fn named_type_method_consumes_receiver(&self, type_name: &str, method: &str) -> bool {
        if self.consume_receiver_methods.is_empty() {
            return false;
        }
        self.trait_impls_set
            .iter()
            .filter(|(ty, _)| ty == type_name)
            .any(|(_, trait_name)| {
                self.is_consume_receiver_method(&format!("{trait_name}::{method}"))
            })
    }

    /// Returns true when the dispatched call is a `#[resource]` type's inherent
    /// terminal `close(self)` — the implicit-drop dispatch target (W3.030) that
    /// also moves its receiver when called explicitly (#1295).
    ///
    /// The match is precise: the receiver type must carry the `#[resource]`
    /// marker, the method must be the discipline-mandated unit-returning
    /// `close`, and the receiver must be by-value `self` (a `var self` /
    /// mutable-receiver method takes the in-place-mutation path and is NOT an
    /// ownership-transfer move — R4). A `#[resource]` type's `close` is required
    /// to be `fn close(self)` by `check_resource_close_discipline`; this guard
    /// keeps the consume marking aligned with that contract.
    fn named_type_inherent_close_consumes_receiver(
        &self,
        type_name: &str,
        builtin: Option<BuiltinType>,
        method: &str,
        sig: &FnSig,
    ) -> bool {
        if method != "close" || sig.requires_mutable_receiver {
            return false;
        }
        // Compiler carriers such as `MonitorRef` already carry their exact,
        // shadow-proof identity on the resolved `Ty`.  Use that discriminator
        // instead of asking the name-indexed source registry to rediscover a
        // prelude spelling: imported source declarations are registry-owned,
        // while compiler carriers are catalog-owned.
        if builtin.is_some_and(|kind| kind.close_method() == Some(method)) {
            return true;
        }
        // The trait registry is the single authority for source-declared
        // `#[resource]` facts, keyed by exact declaration identity.
        self.registry.is_resource(type_name)
    }

    pub(super) fn record_method_call_rewrite(&mut self, span: &Span, rewrite: MethodCallRewrite) {
        self.method_call_rewrites
            .insert(SpanKey::in_module(span, self.current_module_idx), rewrite);
    }

    /// Resolve the reply type used for the ask-reply `Send` gate to the
    /// module-qualified identity of the dispatched actor's defining module.
    ///
    /// The reply `Ty::Named` carries the bare type name (`Reply`) as written in
    /// the imported actor's `receive fn` return annotation. The trait registry
    /// keys marker derivation by name and the bare key is last-write-wins across
    /// modules: two imported packages each exporting `Reply` collide, so a Send
    /// lookup on the bare name can read the wrong module's fields and either
    /// over-accept a non-Send reply (it reaches codegen and trips the D10 gate)
    /// or over-reject a Send one. `method_id` is `{module}.{Actor}::{method}` for
    /// a module actor, so the reply type is defined in `{module}`; if a
    /// collision-free `{module}.{Name}` registry alias exists (seeded by
    /// `register_qualified_type_alias` → `alias_type_markers`), derive `Send`
    /// through that qualified identity. Root / flat-file actors retain their
    /// bare identity. A module actor whose lexical import binding or canonical
    /// marker row is absent returns `None`: the Send gate must reject rather
    /// than consulting a same-name bare marker row.
    fn send_gate_reply_ty(&self, method_id: &str, resolved_reply: &Ty) -> Option<Ty> {
        let Ty::Named {
            name,
            args,
            builtin,
        } = resolved_reply
        else {
            return Some(resolved_reply.clone());
        };
        // Builtins carry their own marker authority. A qualified user name,
        // by contrast, must have an exact structural marker row.
        if builtin.is_some() {
            return Some(resolved_reply.clone());
        }
        if name.contains('.') {
            return self
                .registry
                .has_type_markers(name)
                .then(|| resolved_reply.clone());
        }
        let Some((actor_identity, _method)) = method_id.rsplit_once("::") else {
            return Some(resolved_reply.clone());
        };
        let Some((module_short, _actor)) = actor_identity.rsplit_once('.') else {
            return Some(resolved_reply.clone());
        };
        // `method_id` carries the imported actor's lexical module binding;
        // marker derivation keys on the source declaration's full owner. A
        // missing binding is not evidence for a bare reply type — fail closed
        // so a same-name sibling cannot lend it a Send marker.
        let module_owner = self.module_import_bindings.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            module_short.to_string(),
        ))?;
        let qualified = format!("{module_owner}.{name}");
        if self.registry.has_type_markers(&qualified) {
            Some(Ty::Named {
                name: qualified,
                args: args.clone(),
                builtin: *builtin,
            })
        } else {
            None
        }
    }

    fn record_actor_method_dispatch(&mut self, span: &Span, method_id: String, reply_ty: Ty) -> Ty {
        let resolved_reply = self.subst.resolve(&reply_ty);
        let dispatch = if self.receive_generator_methods.contains(&method_id) {
            // `receive_generator_methods` is checker authority for gen-ness —
            // HIR/MIR consume this discriminator directly rather than
            // re-deriving stream-producer-ness from `is_generator` or from
            // `reply_ty`'s shape (`type-info-survival`). `register_receive_fn`
            // (registration.rs) always wraps a generator method's `fn_sigs`
            // return type in `Ty::stream(declared_return_type)`, so every
            // `record_actor_method_dispatch` call site for a gen method passes
            // a `Stream<T>` here; unwrap to the element type `T`.
            let elem_ty = match reply_ty.as_stream() {
                Some(elem) => elem.clone(),
                None => unreachable!(
                    "receive_generator_methods `{method_id}` recorded with a non-Stream \
                     reply type `{reply_ty:?}` — registration.rs always wraps a generator \
                     method's fn_sigs return_type in Ty::stream(..)"
                ),
            };
            ActorMethodKind::StreamProducer(method_id, elem_ty)
        } else if matches!(resolved_reply, Ty::Unit) {
            let actor_identity = method_id
                .rsplit_once("::")
                .map_or(method_id.as_str(), |(actor, _)| actor);
            let overflow_policy = self.actor_overflow_policies.get(actor_identity);
            let is_policy_sensitive = overflow_policy.is_some_and(|policy| {
                matches!(
                    policy,
                    hew_parser::ast::OverflowPolicy::DropNew
                        | hew_parser::ast::OverflowPolicy::DropOld
                        | hew_parser::ast::OverflowPolicy::Fail
                        | hew_parser::ast::OverflowPolicy::Coalesce { .. }
                )
            });
            if is_policy_sensitive {
                ActorMethodKind::CheckedFire(method_id)
            } else if overflow_policy == Some(&hew_parser::ast::OverflowPolicy::Block) {
                ActorMethodKind::BlockingFire(method_id)
            } else {
                ActorMethodKind::Fire(method_id)
            }
        } else {
            // Ask-shaped: the reply value crosses the actor boundary back to the
            // caller, so `R` must be `Send` — the same obligation the lambda
            // actor reply gate enforces (`E_DUPLEX_NON_SEND`, see
            // `check_lambda_actor` in expressions.rs). Declared-actor asks
            // previously gated only the message arguments
            // (`enforce_actor_method_send_args`), so a non-Send reply type slipped
            // past the checker and surfaced only later at codegen, where the
            // #1739 reply-drop classifier fails closed on it with a far less
            // actionable diagnostic. Gating it here — at the single
            // dispatch-recording chokepoint shared by every declared-actor ask
            // site — turns it into a clean type error at the call.
            //
            // The guard requires a fully resolved, error-free type: an
            // inference-in-progress reply (`Var`) or a reply that already carries
            // an error must not mis-fire a spurious Send rejection (the
            // admissibility output-contract pruner applies the same
            // `!has_inference_var() && !contains_error()` discipline). Every
            // value that is constructible and returnable in safe Hew is Send
            // (handles are `Send + Copy`; `Stream`/`Sink`/`Duplex` are `Send`
            // iff their element is), so in practice this fires only on genuinely
            // non-transferable replies (`Rc`, and any record/tuple/enum that
            // transitively carries one).
            // Derive `Send` through the reply type's module-qualified identity
            // so two imported packages that both export a same-bare-named reply
            // (`badpkg.Reply` vs `goodpkg.Reply`) do not collide on the bare
            // registry key. The qualified form is used only for the marker
            // lookup and diagnostic text; the dispatch table keeps the original
            // bare `reply_ty` the rest of the pipeline expects.
            match self.send_gate_reply_ty(&method_id, &resolved_reply) {
                Some(send_check_ty)
                    if !send_check_ty.has_inference_var()
                        && !send_check_ty.contains_error()
                        && !self
                            .registry
                            .implements_marker(&send_check_ty, MarkerTrait::Send) =>
                {
                    self.report_error(
                        TypeErrorKind::InvalidSend,
                        span,
                        format!(
                            "ask-shaped actor reply type `{}` is not Send (E_DUPLEX_NON_SEND)",
                            resolved_reply.user_facing()
                        ),
                    );
                }
                Some(_) => {}
                None => self.report_error(
                    TypeErrorKind::InvalidSend,
                    span,
                    format!(
                        "ask-shaped actor reply type `{}` has no exact module-owned Send proof \
                         (E_DUPLEX_NON_SEND)",
                        resolved_reply.user_facing()
                    ),
                ),
            }
            ActorMethodKind::Ask(method_id, reply_ty.clone())
        };
        let call_ty = match &dispatch {
            ActorMethodKind::CheckedFire(_) => Ty::result(Ty::Unit, Ty::send_error()),
            _ => reply_ty,
        };
        self.actor_method_dispatch
            .insert(SpanKey::in_module(span, self.current_module_idx), dispatch);
        call_ty
    }

    pub(super) fn canonical_handle_receiver_type_name(&self, receiver_ty: &Ty) -> Option<String> {
        let Ty::Named { name, .. } = receiver_ty else {
            return None;
        };
        self.module_registry.canonical_handle_type_identity(name)
    }

    pub(super) fn record_handle_method_call_receiver_kind_if_any(
        &mut self,
        receiver_ty: &Ty,
        span: &Span,
    ) {
        let Some(type_name) = self.canonical_handle_receiver_type_name(receiver_ty) else {
            return;
        };
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::HandleInstance { type_name },
        );
    }

    /// Return the runtime family for a canonical stdlib extern method whose
    /// ABI is compiler-lowered.
    ///
    /// The endpoint spelling is deliberately insufficient here: user impls
    /// may publish the same `#[extern_symbol]`.  This join requires the exact
    /// registered method identity, its canonical stdlib provenance, and the
    /// checked parameter/result shape before a runtime descriptor can cross
    /// the checker boundary.
    fn canonical_std_io_runtime_method_family(
        &self,
        signature_key: &str,
        c_symbol: &str,
        sig: &FnSig,
    ) -> Option<crate::runtime_call::RuntimeCallFamily> {
        let declaration = crate::runtime_call::canonical_std_io_extern_signature(
            signature_key,
            c_symbol,
            &sig.params,
            &sig.return_type,
        )?;
        let canonical_stdlib = self.extern_method_origins
                .get(signature_key)
                .is_some_and(|(module, trusted)| {
                    *trusted && module.as_deref() == Some(declaration.module)
                })
            // Directly checking a shipped stdlib file has a root module id
            // unrelated to its dotted import owner. `canonical_std_root_sources`
            // is populated only for that exact root path (not for an ordinary
            // user program that merely imports the module), so it is a safe
            // provenance substitute for the registration origin here.
            || self.canonical_std_root_sources.contains(declaration.module);
        if !canonical_stdlib {
            return None;
        }
        let family = declaration.family?;
        // The descriptor is a three-way join: the source table names the only
        // signature that may lift this family, and the typed family
        // independently confirms that it emits the same ABI endpoint.
        (family.c_symbol() == c_symbol).then_some(family)
    }

    /// Record a rewrite for a **closed-set builtin** runtime-ABI method call.
    ///
    /// Every `c_symbol` reaching this helper is one the checker resolved from
    /// its own builtin tables — stdlib method resolution
    /// (`require_builtin_runtime_symbol` / `resolve_*_method`), the literal
    /// close-family handle releases, and handle-method auto-derivation. Open-set
    /// `#[extern_symbol]` FFI strings do NOT come here: they route through
    /// [`Self::record_extern_symbol_method_call_rewrite`], which records
    /// `descriptor: None`. Keeping the two producers split is the
    /// `checker-output-boundary` guarantee — a user FFI symbol that happens to
    /// collide with a catalog name must never be reclassified into a typed
    /// runtime descriptor.
    fn record_runtime_method_family_rewrite(
        &mut self,
        span: &Span,
        family: crate::runtime_call::RuntimeCallFamily,
    ) {
        let c_symbol = family.c_symbol().to_string();
        // The consume verdict is derived once, here, from the resolved runtime
        // symbol — the single rewrite-recording authority for runtime-symbol
        // method calls (close-family handle releases route through this helper).
        // Keying on the symbol (the dispatch discriminant) rather than a
        // receiver type name keeps `.send()`/`.recv()` borrowing and only the
        // `.close()`-family consuming (LESSONS: drop-allowset-from-value-flow).
        let consumes_receiver = crate::builtin_names::runtime_symbol_consumes_receiver(&c_symbol);
        // Recover the typed family for this closed builtin symbol. Because the
        // helper only ever sees checker-emitted catalog symbols (the extern
        // split routes every open-set `#[extern_symbol]` string elsewhere), this
        // is a bijection-guarded catalog round-trip of the checker's OWN output
        // — not a reverse-parse of arbitrary input. `from_c_symbol` returns
        // `None` only for the few builtin symbols the substrate does not yet
        // enumerate (pre-staged families); those keep `descriptor: None` and
        // consumers fall back to `c_symbol`.
        let descriptor = crate::runtime_call::RuntimeCallDescriptor::new(family, None)
            .expect("substrate variant rejects elem; runtime symbols never carry elem here");
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteToFunction {
                target: CallTarget::Runtime(descriptor.family()),
                c_symbol,
                descriptor: Some(descriptor),
                extern_identity: None,
                elem_ty: None,
                consumes_receiver,
                returns_receiver_identity: false,
            },
        );
    }

    fn record_runtime_method_call_rewrite(&mut self, span: &Span, c_symbol: impl Into<String>) {
        let c_symbol = c_symbol.into();
        let Some(family) = crate::runtime_call::RuntimeCallFamily::from_c_symbol(&c_symbol) else {
            // Some compiler-synthetic identity accessors are closed catalog
            // endpoints but do not need a `RuntimeCallFamily`: their lowering
            // is owned by the identity producer in MIR/codegen.  Preserve the
            // checker-selected catalog endpoint instead of degrading a valid
            // source method to an unsupported call just because that producer
            // is not represented in the runtime-family enum.
            if crate::stdlib_catalog_identity::compiler_synthetic_identity_endpoint(&c_symbol)
                .is_some()
            {
                self.record_method_call_rewrite(
                    span,
                    MethodCallRewrite::RewriteToFunction {
                        target: CallTarget::Builtin {
                            endpoint: c_symbol.clone(),
                        },
                        c_symbol,
                        descriptor: None,
                        extern_identity: None,
                        elem_ty: None,
                        consumes_receiver: false,
                        returns_receiver_identity: false,
                    },
                );
                return;
            }
            self.record_method_call_rewrite(
                span,
                MethodCallRewrite::RewriteToFunction {
                    target: CallTarget::Unsupported {
                        reason: format!("unregistered runtime method `{c_symbol}`"),
                    },
                    c_symbol,
                    descriptor: None,
                    extern_identity: None,
                    elem_ty: None,
                    consumes_receiver: false,
                    returns_receiver_identity: false,
                },
            );
            return;
        };
        self.record_runtime_method_family_rewrite(span, family);
    }

    /// Record a direct opaque-handle call through the exact source extern
    /// declaration that owns its ABI endpoint.
    ///
    /// Extracted registry metadata is intentionally only a signature surface:
    /// it may use the legacy `net.Listener` presentation spelling while the
    /// source declaration is owned by `std.net`.  A non-catalog endpoint must
    /// therefore not be fabricated as a runtime call merely because the
    /// registry found it.  This bridge admits it only when the canonical
    /// receiver owner and the source extern declaration agree exactly.
    fn record_source_extern_handle_method_rewrite(
        &mut self,
        span: &Span,
        receiver_name: &str,
        c_symbol: String,
    ) -> bool {
        let Some((owner_module, _)) = receiver_name.rsplit_once('.') else {
            return false;
        };
        // rc1-F1 stage B: resolve through the extern table's declaration
        // index — the receiver's canonical owner must ITSELF declare the
        // symbol, and the published identity is THAT declaration (its own
        // key, its own provenance), never whichever declaration happened to
        // mint the symbol's ABI contract.
        let Some((declaration_key, declaration)) = self
            .extern_table
            .declaration_by_symbol_and_module(&c_symbol, owner_module)
        else {
            return false;
        };

        let extern_identity = ExternMethodCallIdentity {
            endpoint: declaration.symbol.clone(),
            signature_key: declaration_key.full_path().to_string(),
            declaring_module: declaration.declaring_module.clone(),
            trusted_compiled_stdlib: self.canonical_std_module_sources.contains(owner_module),
        };
        let consumes_receiver =
            crate::builtin_names::runtime_symbol_consumes_receiver(&extern_identity.endpoint);
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteToFunction {
                target: CallTarget::Extern {
                    declaration: declaration_key.clone(),
                    endpoint: extern_identity.endpoint.clone(),
                    trusted_compiled_stdlib: extern_identity.trusted_compiled_stdlib,
                },
                c_symbol,
                descriptor: None,
                extern_identity: Some(extern_identity),
                elem_ty: None,
                consumes_receiver,
                returns_receiver_identity: false,
            },
        );
        true
    }

    /// Record a rewrite for an **open-set** `#[extern_symbol]` FFI method call.
    ///
    /// Unlike [`Self::record_runtime_method_call_rewrite`], the typed
    /// `descriptor` is unconditionally `None`. An `#[extern_symbol]` method —
    /// stdlib `duration` / `instant` / `LambdaActorHandle` bindings as well as
    /// user-authored FFI on inherent impls — is open-set *by mechanism*: the
    /// checker has no first-class runtime-call-family knowledge for it. The
    /// family would only be recoverable by reverse-parsing the symbol string,
    /// which is exactly the `checker-output-boundary` violation this split
    /// closes. So even when the raw/expanded symbol collides with a catalog
    /// name (e.g. `hew_duration_hours` == `RuntimeCallFamily::DurationHours`,
    /// or a user binding that string-matches `hew_vec_push_layout`), no typed
    /// descriptor is produced.
    ///
    /// `consumes_receiver` IS still derived from the resolved symbol via the
    /// single consume authority. This is NOT string reclassification but a
    /// load-bearing ownership fact with no other source: stdlib declares
    /// `#[extern_symbol(hew_lambda_actor_release)]`, a genuine consuming handle
    /// release, and dropping its consume mark would let the handle's scope-exit
    /// drop fire on already-freed memory (double-free). The verdict stays
    /// fail-closed (LESSONS: drop-allowset-from-value-flow): any symbol the
    /// allow-set does not name is borrowing, so an FFI binding that merely
    /// collides with a non-release name at worst leaks — it never double-frees.
    fn record_extern_symbol_method_call_rewrite(
        &mut self,
        span: &Span,
        c_symbol: String,
        signature_key: String,
    ) {
        let consumes_receiver = crate::builtin_names::runtime_symbol_consumes_receiver(&c_symbol);
        let (declaring_module, trusted_compiled_stdlib) = self
            .extern_method_origins
            .get(&signature_key)
            .cloned()
            .unwrap_or((None, false));
        let extern_identity = ExternMethodCallIdentity {
            endpoint: c_symbol.clone(),
            signature_key,
            declaring_module,
            trusted_compiled_stdlib,
        };
        // The endpoint is an ABI spelling, not a declaration identity.  In
        // particular an imported receiver may be written through an alias at
        // the call site, while the source impl was registered under its full
        // owner path.  Carry the ID allocated at that registration boundary;
        // do not manufacture one from the call-site signature key.
        let target = crate::stdlib_catalog_identity::compiler_synthetic_identity_endpoint(
            &extern_identity.endpoint,
        )
        .map_or_else(
            || {
                self.impl_method_declaration_ids
                    .get(&extern_identity.signature_key)
                    .cloned()
                    .map_or_else(
                        || CallTarget::Unsupported {
                            reason: format!(
                                "extern-symbol method `{}` has no registered declaration identity",
                                extern_identity.signature_key
                            ),
                        },
                        |declaration| CallTarget::Extern {
                            declaration,
                            endpoint: extern_identity.endpoint.clone(),
                            trusted_compiled_stdlib: extern_identity.trusted_compiled_stdlib,
                        },
                    )
            },
            |endpoint| CallTarget::Builtin {
                endpoint: endpoint.to_string(),
            },
        );
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteToFunction {
                target,
                c_symbol,
                descriptor: None,
                extern_identity: Some(extern_identity),
                elem_ty: None,
                consumes_receiver,
                returns_receiver_identity: false,
            },
        );
    }

    fn record_monomorphic_extern_symbol_rewrite_if_any(
        &mut self,
        sig: &FnSig,
        signature_key: &str,
        span: &Span,
    ) -> bool {
        let Some(spec) = &sig.extern_symbol else {
            return false;
        };
        if let Some(family) =
            self.canonical_std_io_runtime_method_family(signature_key, &spec.template.raw, sig)
        {
            self.record_runtime_method_family_rewrite(span, family);
            return true;
        }
        if !spec.template.is_monomorphic() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "extern-symbol template `{}` is not monomorphic; this receiver dispatch \
                     path only supports monomorphic FFI symbols",
                    spec.template.raw
                ),
            );
            return false;
        }
        self.record_extern_symbol_method_call_rewrite(
            span,
            spec.template.raw.clone(),
            signature_key.to_string(),
        );
        true
    }

    fn record_named_extern_symbol_rewrite_if_any(
        &mut self,
        receiver_type_name: &str,
        type_args: &[Ty],
        method: &str,
        sig: &FnSig,
        span: &Span,
    ) -> bool {
        let Some(spec) = &sig.extern_symbol else {
            return false;
        };
        let signature_key = format!("{receiver_type_name}::{method}");
        if let Some(family) =
            self.canonical_std_io_runtime_method_family(&signature_key, &spec.template.raw, sig)
        {
            self.record_runtime_method_family_rewrite(span, family);
            return true;
        }
        if spec.template.is_monomorphic() {
            self.record_extern_symbol_method_call_rewrite(
                span,
                spec.template.raw.clone(),
                signature_key,
            );
            return true;
        }
        if !matches!(receiver_type_name, "Option" | "Result") {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "extern-symbol template `{}` is not monomorphic; this receiver dispatch \
                     path only supports monomorphic FFI symbols",
                    spec.template.raw
                ),
            );
            return false;
        }
        let Some(type_arg) = type_args.first() else {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "extern-symbol template `{}` requires receiver type argument `T`, \
                     but `{receiver_type_name}` has no type argument",
                    spec.template.raw
                ),
            );
            return false;
        };
        let resolved_type_arg = self.subst.resolve(type_arg).materialize_literal_defaults();
        let expanded = match spec.template.expand(&resolved_type_arg, &self.type_defs) {
            Ok(symbol) => symbol,
            Err(crate::extern_symbol::TemplateExpansionError::UnsupportedCallingConvention {
                expected_symbol,
                convention,
            }) => {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "cannot lower {receiver_type_name}::{method}: extern-symbol template \
                         `{}` expands to unsupported runtime calling convention {:?} \
                         (would require `{expected_symbol}`)",
                        spec.template.raw, convention
                    ),
                );
                return false;
            }
        };
        self.record_extern_symbol_method_call_rewrite(
            span,
            expanded,
            format!("{receiver_type_name}::{method}"),
        );
        true
    }

    fn record_builtin_option_result_method_rewrite_if_any(
        &mut self,
        receiver_builtin: BuiltinType,
        receiver_type_name: &str,
        type_args: &[Ty],
        method: &str,
        span: &Span,
    ) -> bool {
        use crate::check::OptionResultMethod as M;
        use MethodCallRewrite::BuiltinOptionResult;

        let Some(marker) = (match (receiver_builtin, method) {
            (BuiltinType::Option, "is_some") => Some(M::OptionIsSome),
            (BuiltinType::Option, "is_none") => Some(M::OptionIsNone),
            (BuiltinType::Option, "unwrap") => Some(M::OptionUnwrap),
            (BuiltinType::Option, "unwrap_or") => Some(M::OptionUnwrapOr),
            (BuiltinType::Result, "is_ok") => Some(M::ResultIsOk),
            (BuiltinType::Result, "is_err") => Some(M::ResultIsErr),
            (BuiltinType::Result, "unwrap") => Some(M::ResultUnwrap),
            (BuiltinType::Result, "unwrap_or") => Some(M::ResultUnwrapOr),
            _ => None,
        }) else {
            return false;
        };

        let expected_args = if receiver_builtin == BuiltinType::Option {
            1
        } else {
            2
        };
        if type_args.len() != expected_args {
            if type_args
                .iter()
                .any(|ty| matches!(ty, Ty::Error | Ty::Var(_)))
            {
                return true;
            }
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "cannot lower {receiver_type_name}::{method}: expected {expected_args} \
                     receiver type argument(s), found {}",
                    type_args.len()
                ),
            );
            return true;
        }

        if type_args.iter().any(|ty| {
            let resolved = self.subst.resolve(ty);
            matches!(resolved, Ty::Error | Ty::Var(_))
        }) {
            return true;
        }

        self.record_method_call_rewrite(span, BuiltinOptionResult { method: marker });
        true
    }

    fn is_builtin_option_result_marker_method(
        receiver_builtin: Option<BuiltinType>,
        method: &str,
    ) -> bool {
        matches!(
            (receiver_builtin, method),
            (
                Some(BuiltinType::Option),
                "is_some" | "is_none" | "unwrap" | "unwrap_or"
            ) | (
                Some(BuiltinType::Result),
                "is_ok" | "is_err" | "unwrap" | "unwrap_or"
            )
        )
    }

    fn dispatch_monomorphic_extern_symbol_method(
        &mut self,
        receiver_type_name: &str,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let sig = self.lookup_named_method_sig(receiver_type_name, type_args, method)?;
        sig.extern_symbol.as_ref()?;
        let method_key = format!("{receiver_type_name}::{method}");
        let assoc_bindings = self
            .fn_type_param_assoc_bindings
            .get(&method_key)
            .cloned()
            .unwrap_or_default();
        let applied_sig = self.apply_instantiated_call_signature_with_assoc(
            &sig,
            &assoc_bindings,
            None,
            args,
            span,
            SignatureArgApplication::PositionalOnly {
                arity_context: format!("method `{method}`"),
            },
            true,
            Some(GenericCallee::Method {
                type_name: receiver_type_name,
                method,
                owner_type_args: type_args,
            }),
        );
        self.record_monomorphic_extern_symbol_rewrite_if_any(&sig, &method_key, span);
        Some(applied_sig.return_type)
    }

    fn check_primitive_receiver_method_fallback(
        &mut self,
        receiver_ty: &Ty,
        receiver_label: &str,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        if let Some(ret_ty) =
            self.try_dispatch_primitive_trait_method(receiver_ty, method, args, span)
        {
            return ret_ty;
        }
        for arg in args {
            let (expr, sp) = arg.expr();
            self.synthesize(expr, sp);
        }
        self.report_error(
            TypeErrorKind::UndefinedMethod,
            span,
            format!("no method `{method}` on {receiver_label}"),
        );
        Ty::Error
    }

    fn missing_builtin_contract_error(
        &mut self,
        span: &Span,
        builtin: &str,
        method: &str,
        item: &str,
    ) {
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "internal compiler error: builtin {builtin}.{method} is missing {item} metadata"
            ),
        );
    }

    fn require_builtin_runtime_symbol(
        &mut self,
        span: &Span,
        builtin: &str,
        method: &str,
        symbol: Option<&'static str>,
    ) -> Option<&'static str> {
        symbol.or_else(|| {
            self.missing_builtin_contract_error(span, builtin, method, "runtime rewrite");
            None
        })
    }

    fn require_builtin_method_sig(
        &mut self,
        span: &Span,
        receiver_ty: &Ty,
        builtin: &str,
        method: &str,
    ) -> Option<FnSig> {
        lookup_builtin_method_sig(receiver_ty, method).or_else(|| {
            self.missing_builtin_contract_error(span, builtin, method, "type signature");
            None
        })
    }

    fn record_module_qualified_method_call_rewrite(
        &mut self,
        span: &Span,
        c_symbol: impl Into<String>,
        source_declaration: impl Into<String>,
    ) {
        let c_symbol = c_symbol.into();
        let source_declaration = source_declaration.into();
        let target = if let Some(family) =
            crate::runtime_call::RuntimeCallFamily::from_c_symbol(&c_symbol)
        {
            CallTarget::Runtime(family)
        } else {
            self.identity
                .declaration_by_path(&source_declaration)
                .cloned()
                .map_or_else(
                    || CallTarget::Builtin {
                        endpoint: c_symbol.clone(),
                    },
                    CallTarget::User,
                )
        };
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteModuleQualifiedToFunction {
                target,
                c_symbol,
                elem_ty: None,
            },
        );
    }

    /// Resolve a module spelling in the current source file to the exact
    /// imported module path.  The spelling is an input-namespace key only;
    /// declaration IDs must use this source owner, never an alias or final
    /// path segment.
    pub(super) fn canonical_module_import_owner(&self, module_name: &str) -> String {
        self.module_import_bindings
            .get(&(
                self.current_module.clone(),
                self.current_module_idx,
                module_name.to_string(),
            ))
            .cloned()
            .unwrap_or_else(|| module_name.to_string())
    }

    /// Whether `module_name` is a lexical module binding in the current file.
    /// The process-wide module registry is deliberately not consulted.
    pub(super) fn module_binding_in_current_file(&self, module_name: &str) -> bool {
        self.module_import_bindings.contains_key(&(
            self.current_module.clone(),
            self.current_module_idx,
            module_name.to_string(),
        )) || crate::stdlib_authority::authority()
            .prelude_exports()
            .iter()
            .filter(|export| export.kind == crate::PreludeExportKind::Module)
            .any(|export| {
                export.alias.as_deref().unwrap_or_else(|| {
                    export
                        .module
                        .rsplit_once('.')
                        .map_or(export.module.as_str(), |(_, leaf)| leaf)
                }) == module_name
            })
    }

    /// Whether this module spelling resolves to a user-source declaration.
    /// `user_modules` is intentionally not consulted: it is a legacy lexical
    /// spelling set and therefore cannot distinguish two paths with the same
    /// final component.
    pub(super) fn module_binding_has_user_declaration(
        &self,
        module_name: &str,
        method: &str,
    ) -> bool {
        let owner = self.canonical_module_import_owner(module_name);
        let declaration = format!("{owner}.{method}");
        self.fn_def_spans
            .get(&declaration)
            .is_some_and(|(_, declaring_module)| {
                declaring_module.as_deref() == Some(owner.as_str())
            })
    }

    /// Reject an exact native-only function at the semantic call/reference
    /// boundary. Module spellings are lexical only: aliases resolve to their
    /// canonical imported owner before consulting the fully-qualified
    /// manifest policy, while user declarations with the same spelling remain
    /// valid. Whole-module policy remains owned by
    /// `wasm_native_only_module_feature` so the two tables cannot emit duplicate
    /// diagnostics for the same call.
    pub(super) fn reject_wasm_native_only_module_function(
        &mut self,
        module_name: &str,
        method: &str,
        span: &Span,
    ) {
        if !self.wasm_target {
            return;
        }
        let owner = self.canonical_module_import_owner(module_name);
        if !self.canonical_std_module_sources.contains(&owner) {
            return;
        }
        // Function policy is fully qualified and therefore cannot be shadowed
        // by a user module whose leaf happens to be `fs`.
        for rejection in crate::NATIVE_ONLY_WASM_FUNCTION_REJECTIONS {
            if owner == rejection.module && method == rejection.function {
                self.reject_wasm_feature(span, rejection.feature);
            }
        }
    }

    /// Apply exact function policy after a named import has resolved its
    /// declaration owner.  Unlike a bare surface spelling this carries the
    /// source identity (`std.fs.try_read`) and cannot be captured by a user
    /// function or a same-leaf module.
    pub(super) fn reject_wasm_native_only_function_identity(
        &mut self,
        source_identity: &str,
        span: &Span,
    ) {
        if !self.wasm_target {
            return;
        }
        let Some((module, function)) = source_identity.rsplit_once('.') else {
            return;
        };
        if !self.canonical_std_module_sources.contains(module) {
            return;
        }
        for rejection in crate::NATIVE_ONLY_WASM_FUNCTION_REJECTIONS {
            if module == rejection.module && function == rejection.function {
                self.reject_wasm_feature(span, rejection.feature);
            }
        }
    }

    /// Record a channel method rewrite to be resolved after inference settles.
    ///
    /// Called instead of `record_runtime_method_call_rewrite` when the inner
    /// type `T` of `Sender<T>` / `Receiver<T>` is still a `Ty::Var` at the
    /// call site.  The deferred entry is drained by `finalize_channel_rewrites`
    /// in `check_program`.
    fn record_deferred_channel_method_rewrite(
        &mut self,
        span: &Span,
        handle_kind: &str,
        method: &str,
        inner_ty: Ty,
    ) {
        self.deferred_channel_rewrites.insert(
            SpanKey::in_module(span, self.current_module_idx),
            DeferredChannelMethodRewrite {
                handle_kind: handle_kind.to_string(),
                method: method.to_string(),
                inner_ty,
                source_module: self.current_module.clone(),
            },
        );
    }

    fn record_handle_method_call_rewrite_if_any(
        &mut self,
        receiver_ty: &Ty,
        method: &str,
        span: &Span,
    ) {
        self.record_handle_method_call_receiver_kind_if_any(receiver_ty, span);
        let Ty::Named { name, .. } = receiver_ty else {
            return;
        };
        // Active-mode transport `attach(handler)` methods rewrite to
        // callee-name-dispatch symbols intercepted by the LLVM backend. The
        // backend resolves the concrete actor type from the `handler` arg's
        // recorded `LocalPid<Actor>` (the structural handler coercion
        // deliberately does not erase that recorded type), synthesises each
        // transport protocol's handler `msg_id`s, and emits the real four-arg
        // runtime attach ABI. The source impl bodies are stubs, so these
        // explicit rewrites are the authority. Mirrors `RemotePid::send`.
        if let Some(symbol) = self.resolved_transport_attach_runtime_symbol(name, method) {
            self.record_runtime_method_call_rewrite(span, symbol);
            return;
        }
        if let Some(c_symbol) = self.module_registry.resolve_handle_method(name, method) {
            // Only a genuine fieldless `#[opaque]` runtime handle — where the
            // receiver value IS the runtime pointer — may be rewritten to a
            // direct extern call that passes the receiver as the handle
            // argument. A fielded `#[resource]` wrapper (e.g.
            // `regex.Pattern { handle }`) registers its thin-forward methods for
            // imported-signature resolution only; rewriting it would pass the
            // whole struct by value to a pointer-typed extern
            // (`hew_regex_is_match(%Pattern, …)`), and for a handle-returning
            // method (`clone -> Pattern`) a bare rewrite cannot reconstruct the
            // wrapper from the extern's inner-handle return. Those dispatch
            // through their real impl body, which forwards `self.handle` and
            // rebuilds the wrapper. This is the `is_handle_type` gate the
            // wrapper-registration path documents but must actually enforce here.
            if self.receiver_is_opaque_handle(name)
                && !self
                    .module_registry
                    .handle_method_dispatches_through_impl(name, method)
            {
                if crate::runtime_call::RuntimeCallFamily::from_c_symbol(&c_symbol).is_some() {
                    self.record_runtime_method_call_rewrite(span, c_symbol);
                } else {
                    let _ = self.record_source_extern_handle_method_rewrite(span, name, c_symbol);
                }
            }
        }
    }

    /// Resolve an active transport handle to its compiler-lowered attach symbol.
    ///
    /// Imported method return types may retain their defining module's bare
    /// spelling (`Connection`, `TlsStream`, or `Conn`). Canonicalise that
    /// already-resolved nominal type through the checker's owner tables before
    /// consulting the closed symbol map. Registry membership then proves the
    /// identity is a loaded fieldless `#[opaque]` handle, while the user-module
    /// guard prevents an alias-qualified user type from impersonating a stdlib
    /// carrier. A root-local same-named type deliberately remains bare under
    /// `canonical_nominal_name` and therefore cannot reach this rewrite.
    fn resolved_transport_attach_runtime_symbol(
        &self,
        receiver_name: &str,
        method: &str,
    ) -> Option<&'static str> {
        let canonical = self
            .canonical_nominal_name(receiver_name)
            .unwrap_or_else(|| receiver_name.to_string());
        let symbol = transport_attach_runtime_symbol(&canonical, method)?;
        let (module, _) = canonical.rsplit_once('.')?;
        // The registry carries the original loaded spelling (`tls.TlsStream` /
        // `websocket.Conn`), while nominal resolution carries its exact full
        // source owner. Join the two identities here rather than asking the
        // registry to recognise the canonical spelling directly: a user
        // module may use either leaf spelling but cannot produce the same
        // loaded source identity.
        if self.user_modules.contains(module)
            || self
                .module_registry
                .canonical_handle_type_identity(receiver_name)
                .as_deref()
                != Some(canonical.as_str())
        {
            return None;
        }
        Some(symbol)
    }

    /// True when `name` (qualified `regex.PatternHandle` or bare `Listener`)
    /// resolves to a fieldless `#[opaque]` runtime handle: the receiver value is
    /// itself the runtime pointer, so a handle-method call is safe to rewrite to
    /// a direct extern that takes the receiver as the handle argument. False for
    /// a fielded `#[resource]` wrapper — whose qualified name is not in the
    /// opaque `handle_types` set and whose short name matches no fieldless
    /// handle — so its methods keep dispatching through their real impl body.
    pub(super) fn receiver_is_opaque_handle(&self, name: &str) -> bool {
        self.module_registry.is_handle_type(name)
    }

    pub(super) fn record_module_qualified_stdlib_call_rewrite_if_any(
        &mut self,
        module_name: &str,
        method: &str,
        span: &Span,
    ) {
        let canonical_owner = self.canonical_module_import_owner(module_name);
        let source_declaration = format!("{canonical_owner}.{method}");
        if let Some(target) = self.intrinsic_runtime_target_for_signature(&source_declaration) {
            // The checker has already proved the exact canonical declaration
            // and catalog identity. Keep the user-facing callee spelling only
            // for HIR presentation; the typed target is the executable
            // authority and no lowering re-parses this string.
            self.record_method_call_rewrite(
                span,
                MethodCallRewrite::RewriteModuleQualifiedToFunction {
                    target: CallTarget::Runtime(target),
                    c_symbol: method.to_string(),
                    elem_ty: None,
                },
            );
            return;
        }
        if self.module_binding_has_user_declaration(module_name, method) {
            return;
        }
        if let Some(c_symbol) = self
            .module_registry
            .resolve_module_call(&canonical_owner, method)
        {
            let symbol = if c_symbol == method {
                let source_qualified = format!("{canonical_owner}.{method}");
                let surface_qualified = format!("{module_name}.{method}");
                if !self.fn_sigs.contains_key(&source_qualified)
                    && !self.fn_sigs.contains_key(&surface_qualified)
                {
                    return;
                }
                // Linker presentation remains the checker-selected registry
                // spelling for now; the target below carries the canonical
                // source declaration identity.
                surface_qualified
            } else {
                c_symbol
            };
            self.record_module_qualified_method_call_rewrite(span, symbol, source_declaration);
        }
    }

    /// Record a direct-call rewrite for a `module.fn(args)` invocation
    /// against a user-defined module.
    ///
    /// Mirrors `record_module_qualified_stdlib_call_rewrite_if_any` but for
    /// user modules: the qualified `module.fn` key is the rewrite target, no
    /// receiver is injected (per LESSONS `module-qualified-rewrite-authority`
    /// — argument list preserved). HIR's `RewriteModuleQualifiedToFunction`
    /// arm consumes the rewrite to emit a direct function call against the
    /// qualified symbol.
    pub(super) fn record_module_qualified_user_call_rewrite_if_any(
        &mut self,
        module_name: &str,
        method: &str,
        span: &Span,
    ) {
        if !self.module_binding_has_user_declaration(module_name, method) {
            return;
        }
        let canonical_owner = self.canonical_module_import_owner(module_name);
        let source_declaration = format!("{canonical_owner}.{method}");
        // A canonical compiler-intrinsic declaration is source-backed but not
        // an ordinary user function. Its typed runtime or type-directed math
        // rewrite was selected by the stdlib path; never overwrite it with a
        // linker-name `User` fallback merely because it also has a source fn.
        if self
            .intrinsic_runtime_target_for_signature(&source_declaration)
            .is_some()
            || self
                .intrinsic_math_generic_op_for_signature(&source_declaration)
                .is_some()
        {
            return;
        }
        if self.fn_sigs.contains_key(&source_declaration)
            || self
                .fn_sigs
                .contains_key(&format!("{module_name}.{method}"))
        {
            self.record_module_qualified_method_call_rewrite(
                span,
                source_declaration.clone(),
                source_declaration,
            );
        }
    }

    fn reject_if_wasm_native_only_handle(&mut self, receiver_ty: &Ty, span: &Span) {
        let Ty::Named { name, builtin, .. } = receiver_ty else {
            return;
        };
        if builtin.is_some_and(|builtin| {
            builtin.has_role(crate::builtin_type::BuiltinTypeRole::WasmNativeOnlyHandle)
        }) {
            self.reject_wasm_feature(span, WasmUnsupportedFeature::TcpNetworking);
            return;
        }
        let Some(module_name) = name.split('.').next() else {
            return;
        };
        if self.user_modules.contains(module_name) {
            return;
        }
        match name.as_str() {
            "http_client.Response" | "std.net.http.http_client.Response" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::HttpClient);
            }
            "smtp.Conn" | "std.net.smtp.Conn" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Smtp);
            }
            "websocket.Conn"
            | "websocket.Server"
            | "websocket.Message"
            | "std.net.websocket.Conn"
            | "std.net.websocket.Server"
            | "std.net.websocket.Message" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::WebSocket);
            }
            "process.Child" | "std.process.Child" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::ProcessExecution);
            }
            "http.Server" | "http.Request" | "std.net.http.Server" | "std.net.http.Request" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::HttpServer);
            }
            STD_NET_LISTENER | STD_NET_CONNECTION => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::TcpNetworking);
            }
            "tls.TlsStream" | "std.net.tls.TlsStream" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Tls);
            }
            "quic.QUICEndpoint"
            | "quic.QUICConnection"
            | "quic.QUICStream"
            | "quic.QUICEvent"
            | "std.net.quic.QUICEndpoint"
            | "std.net.quic.QUICConnection"
            | "std.net.quic.QUICStream"
            | "std.net.quic.QUICEvent" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Quic);
            }
            _ => {}
        }
    }

    fn reject_if_wasm_blocking_semaphore_method(
        &mut self,
        receiver_ty: &Ty,
        method: &str,
        span: &Span,
    ) {
        let Ty::Named { name, .. } = receiver_ty else {
            return;
        };
        if name != "std.semaphore.Semaphore" {
            return;
        }
        if matches!(method, "acquire" | "acquire_timeout") {
            self.reject_wasm_feature(span, WasmUnsupportedFeature::BlockingSemaphoreAcquire);
        }
    }

    pub(super) fn runtime_stream_element_name(ty: &Ty) -> Option<&'static str> {
        match ty {
            Ty::String => Some("string"),
            Ty::Bytes => Some("bytes"),
            _ => None,
        }
    }

    pub(super) fn strip_module_prefix<'a>(&self, name: &'a str) -> Option<&'a str> {
        let dot = name.find('.')?;
        if self.module_binding_in_current_file(&name[..dot]) {
            Some(&name[dot + 1..])
        } else {
            None
        }
    }

    /// Look up a type definition, handling module-qualified names like `json.Value`.
    pub(super) fn lookup_type_def(&self, name: &str) -> Option<TypeDef> {
        let current_module_key = if name.contains('.') {
            None
        } else {
            self.current_module_identity()
                .map(|owner| format!("{owner}.{name}"))
        };
        self.type_defs
            .get(name)
            .or_else(|| {
                current_module_key
                    .as_ref()
                    .and_then(|key| self.type_defs.get(key))
            })
            .or_else(|| {
                self.strip_module_prefix(name)
                    .and_then(|u| self.type_defs.get(u))
            })
            .cloned()
    }

    /// Look up a type definition mutably, handling module-qualified names.
    pub(super) fn lookup_type_def_mut(&mut self, name: &str) -> Option<&mut TypeDef> {
        if self.type_defs.contains_key(name) {
            return self.type_defs.get_mut(name);
        }
        if !name.contains('.') {
            if let Some(owner) = self.current_module_identity() {
                let current_module_key = format!("{owner}.{name}");
                if self.type_defs.contains_key(&current_module_key) {
                    return self.type_defs.get_mut(&current_module_key);
                }
            }
        }
        let unqualified = self.strip_module_prefix(name)?;
        self.type_defs.get_mut(unqualified)
    }

    /// Resolve a `(module, type)` pair to its `TypeDef`, gated on the type being
    /// in the imported module's exported set.  Returns `None` if the module is
    /// not a known alias, the type is not exported, or the qualified type alias
    /// was not registered (latter would be a registration bug — callers should
    /// treat as "type not exported" for diagnostic purposes).
    ///
    /// Mirrors the `module_fn_exports` guard pattern used by
    /// `check_method_call` for module-qualified function dispatch.
    pub(super) fn resolve_module_type(
        &self,
        module_short: &str,
        type_name: &str,
    ) -> Option<TypeDef> {
        if !self.module_binding_in_current_file(module_short) {
            return None;
        }
        let resolved_module = self
            .module_import_bindings
            .get(&(
                self.current_module.clone(),
                self.current_module_idx,
                module_short.to_string(),
            ))
            .map(String::as_str)?;
        let exports = self.module_type_exports.get(resolved_module)?;
        if !exports.contains(type_name) {
            return None;
        }
        let qualified = format!("{resolved_module}.{type_name}");
        self.type_defs.get(&qualified).cloned()
    }

    /// Return the exact source owner's exported type set for a lexical module
    /// binding. Diagnostics use this helper as well as successful resolution so
    /// suggestions never accidentally consult a same-leaf surface key.
    pub(super) fn module_type_exports_for_binding(
        &self,
        module_short: &str,
    ) -> Option<&HashSet<String>> {
        if !self.module_binding_in_current_file(module_short) {
            return None;
        }
        let owner = self.module_import_bindings.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            module_short.to_string(),
        ))?;
        self.module_type_exports.get(owner)
    }

    /// Canonicalize a supervisor child's user-spelled `actor_type` to the
    /// registered actor identity.
    ///
    /// A supervisor child records its actor type as the raw source string the
    /// user wrote (`child b: bank.Account` stores `bank.Account`). For a
    /// package-module child that spelling carries the user's import *alias*
    /// (`bank`), whereas the checker registers the actor under its exact source
    /// owner (`hew.bank.Account`, keyed off `current_module`). Left raw, the
    /// alias-prefixed string never matches the canonical `fn_sigs` /
    /// `actor_init_params` / `type_defs` keys, so a `LocalPid<bank.Account>`
    /// finds no `receive fn` and every wall keyed on the actor identity silently
    /// skips.
    ///
    /// Resolve dotted module bindings and bare named/aliased import bindings
    /// through the same lexical facts ordinary type resolution consumes. A
    /// declaration authored in the current scope wins before an import, and a
    /// bare import resolves only when that exact binding published one source
    /// identity. There is deliberately no scan over globally loaded exports.
    pub(super) fn resolve_supervisor_child_type(&self, raw: &str) -> Option<String> {
        if let Some((module_short, type_name)) = raw.split_once('.') {
            return self
                .resolve_module_type(module_short, type_name)
                .map(|td| td.name);
        }

        if self.supervisor_children.contains_key(raw) {
            return Some(raw.to_string());
        }

        // A supervisor declared inside a non-root module shares that module's
        // nominal scope with its actors. Resolve only the exact owner-qualified
        // actor declaration; never search another loaded module by leaf name.
        // This rung precedes selected imports so a same-file actor retains
        // lexical authority over an imported binding with the same spelling.
        if let Some(owner) = self.current_module_identity() {
            let local_actor = format!("{owner}.{raw}");
            if self
                .type_defs
                .get(&local_actor)
                .is_some_and(|type_def| type_def.kind == TypeDefKind::Actor)
            {
                return Some(local_actor);
            }
        }

        if self.local_type_defs.contains(raw) || self.source_type_defs.contains(raw) {
            let local = self.declaration_identity(raw);
            if self.type_defs.contains_key(&local) {
                return Some(local);
            }
            if self.type_defs.contains_key(raw) {
                return Some(raw.to_string());
            }
            // A flattened file import is root-visible in the source sets but
            // its compatibility leaf key is retired after registration. Fall
            // through to the exact published bare binding below; a genuine
            // current-scope declaration returned from one of the two keys.
        }

        // Root actors and flattened file-import actors both publish an exact
        // root-surface key. This is not a leaf search: the key exists only
        // because that spelling was registered into the current root scope.
        if self.current_module_identity().is_none() && self.type_defs.contains_key(raw) {
            return Some(raw.to_string());
        }

        if let Some(identity) = self.published_bare_type_qualified(raw) {
            if let Some(owner) = self.unqualified_to_module.get(&(
                self.current_module.clone(),
                self.current_module_idx,
                raw.to_string(),
            )) {
                self.mark_module_owner_bindings_used(owner);
            }
            return Some(identity);
        }
        None
    }

    pub(super) fn canonical_supervisor_child_type(&self, raw: &str) -> String {
        self.resolve_supervisor_child_type(raw)
            .unwrap_or_else(|| raw.to_string())
    }

    /// Resolve a `(module, type, variant)` triple to its `VariantDef`, gated on
    /// the type being exported by the module.  Returns `None` if the module
    /// alias is unknown, the type is not exported, or the variant does not
    /// exist on the type.  The caller is responsible for emitting the
    /// fail-closed diagnostic in each failure case.
    pub(super) fn resolve_module_variant(
        &self,
        module_short: &str,
        type_name: &str,
        variant_name: &str,
    ) -> Option<(TypeDef, VariantDef)> {
        let td = self.resolve_module_type(module_short, type_name)?;
        let v = td.variants.get(variant_name).cloned()?;
        Some((td, v))
    }

    /// Full canonical owner path of the module whose declarations are being
    /// checked. Use this for declaration identity and layout-facing type
    /// lookup.
    pub(super) fn current_module_identity(&self) -> Option<&str> {
        self.current_module.as_deref()
    }

    /// The identity a declaration written in the scope currently being checked
    /// is published under: `{module}.{bare_name}` inside a module, the bare
    /// name at the root program.
    ///
    /// This is the one formula for a declaration's own name. Registration mints
    /// the `TypeDef` key and the declaration's `Ty::Named` with it, and every
    /// later pass that has to name that same declaration - a machine's
    /// transition bodies, for instance - must mint it the same way, because the
    /// bare spelling is only a transient row on the import path and is retired
    /// once the canonical owner is published (`retire_imported_type_keys`).
    pub(super) fn declaration_identity(&self, bare_name: &str) -> String {
        self.current_module_identity().map_or_else(
            || bare_name.to_string(),
            |module| format!("{module}.{bare_name}"),
        )
    }

    /// Resolve a bare actor reference to its registered checker identity.
    ///
    /// Resolution order (local-first, mirroring `per-module-type-identity`):
    /// 1. the current module's own actor (`{current_full_path}.{name}`)
    /// 2. a root/flat actor registered under the bare name
    /// 3. a named-import binding (`unqualified_to_module`)
    /// 4. the modules exporting an actor of that name: exactly one resolves
    ///    to it; two or more is `Ambiguous` (never silent first-wins).
    pub(super) fn resolve_bare_actor_identity(&self, name: &str) -> BareActorResolution {
        let is_actor = |key: &str| {
            self.type_defs
                .get(key)
                .is_some_and(|td| td.kind == TypeDefKind::Actor)
        };
        if let Some(module) = self.current_module.as_deref() {
            let dotted = format!("{module}.{name}");
            if is_actor(&dotted) {
                return BareActorResolution::Resolved(dotted);
            }
        }
        if is_actor(name) {
            return BareActorResolution::Resolved(name.to_string());
        }
        if let Some(owners) = self.published_bare_type_owners.get(&(
            self.current_module.clone(),
            self.current_module_idx,
            name.to_string(),
        )) {
            let candidates: Vec<String> = owners
                .iter()
                .filter(|identity| is_actor(identity))
                .cloned()
                .collect();
            match candidates.as_slice() {
                [identity] => return BareActorResolution::Resolved(identity.clone()),
                [] => {}
                _ => {
                    let modules = candidates
                        .iter()
                        .filter_map(|identity| identity.rsplit_once('.'))
                        .map(|(module, _)| module.to_string())
                        .collect();
                    return BareActorResolution::Ambiguous(modules);
                }
            }
        }
        let mut candidates: Vec<&str> = self
            .module_type_exports
            .iter()
            .filter(|(module, exports)| {
                exports.contains(name) && is_actor(&format!("{module}.{name}"))
            })
            .map(|(module, _)| module.as_str())
            .collect();
        candidates.sort_unstable();
        match candidates.as_slice() {
            [] => BareActorResolution::Unknown,
            [module] => BareActorResolution::Resolved(format!("{module}.{name}")),
            _ => {
                BareActorResolution::Ambiguous(candidates.iter().map(ToString::to_string).collect())
            }
        }
    }

    /// Resolve a method signature against the *module-local* type definition.
    ///
    /// When the checker is inside module `m` and resolving `Type::method`, the
    /// authoritative definition is `m`'s own `Type` (registered under the
    /// qualified `{short}.{Type}` key), not the bare `Type` key which is
    /// last-write-wins across every module that declares a same-named type.
    /// Used by the impl-body return-type check so a method body in module `m`
    /// is validated against `m`'s type, not whichever module registered the
    /// bare key last. Returns `None` outside a module or when the qualified
    /// type def / method is absent (caller falls back to the bare lookup).
    pub(super) fn module_local_method_sig(&self, type_name: &str, method: &str) -> Option<FnSig> {
        let owner = self.current_module_identity()?;
        let qualified = format!("{owner}.{type_name}");
        let td = self.type_defs.get(&qualified)?;
        td.methods.get(method).cloned()
    }

    pub(super) fn lookup_named_method_sig(
        &self,
        type_name: &str,
        type_args: &[Ty],
        method: &str,
    ) -> Option<FnSig> {
        shared_lookup_named_method_sig(&self.type_defs, &self.fn_sigs, type_name, type_args, method)
            .or_else(|| {
                let target = self.alias_target_for_instance(type_name, type_args)?;
                crate::method_resolution::lookup_method_sig(
                    &self.type_defs,
                    &self.fn_sigs,
                    &target,
                    method,
                )
            })
            .or_else(|| {
                self.module_registry
                    .resolve_handle_method_sig(type_name, method)
                    .map(|(_c_symbol, params, return_type, canonical_owner)| {
                        // The registry's own projection sees only the loaded
                        // module and its imports, so a nominal that module
                        // neither declares nor imports (`stream.Sink` reached
                        // from `std.net.http`) comes back at the legacy short
                        // owner while source resolution mints the complete one.
                        // Re-resolve through the shared ladder so a method
                        // signature and the source around it name one owner
                        // (rc1-F1 stage D, registry producer).
                        FnSig {
                            params: params
                                .iter()
                                .map(|ty| {
                                    self.canonicalize_registry_signature(ty, &canonical_owner)
                                })
                                .collect(),
                            return_type: self
                                .canonicalize_registry_signature(&return_type, &canonical_owner),
                            ..FnSig::default()
                        }
                    })
            })
    }

    /// Resolve a method on a builtin `Result`/`Option` receiver against the
    /// canonical stdlib method surface ONLY.
    ///
    /// Dispatch on a builtin `Result<T, E>` / `Option<T>` receiver (e.g. the
    /// `Result<T, AskError>` wrapper an actor ask produces) must never consult
    /// the user `type_defs`/`fn_sigs`: a user package may declare its own
    /// `type Result`/`type Option` whose methods land under the same bare
    /// `Result::<method>` keys and shadow the stdlib entries by registration
    /// order. Resolving here against the origin-based
    /// [`Checker::builtin_result_option_method_sigs`] snapshot guarantees the
    /// builtin surface (and its `extern_symbol` rewrite) is selected for every
    /// method, not just a fixed allowlist of names. A method absent from the
    /// snapshot returns `None`, so the caller falls through to the
    /// `no method on Result<...>`/`Option<...>` diagnostic.
    pub(super) fn lookup_builtin_result_option_method_sig(
        &self,
        builtin: BuiltinType,
        type_args: &[Ty],
        method: &str,
    ) -> Option<FnSig> {
        let sig = self
            .builtin_result_option_method_sigs
            .get(&(builtin, method.to_string()))?;
        Some(instantiate_stdlib_method_sig(
            sig,
            &sig.type_params,
            type_args,
        ))
    }

    /// Resolve a runtime-backed Vec method from the compiled-in stdlib source,
    /// never from user-shadowable `Vec::<method>` keys.
    fn lookup_builtin_vec_method_sig(&self, type_args: &[Ty], method: &str) -> Option<FnSig> {
        let sig = self.builtin_vec_method_sigs.get(method)?;
        Some(instantiate_stdlib_method_sig(
            sig,
            &sig.type_params,
            type_args,
        ))
    }

    /// Try to resolve a method call on a named type via `type_defs` and `fn_sigs`.
    ///
    /// Used as a fallback from hardcoded handle-type dispatch tables so that
    /// methods added via `.hew` impl blocks work without updating the tables.
    pub(super) fn try_resolve_named_method(
        &mut self,
        receiver_ty: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let Ty::Named {
            name,
            args: type_args,
            ..
        } = receiver_ty
        else {
            return None;
        };
        let canonical_name = self
            .canonical_nominal_name(name)
            .unwrap_or_else(|| name.clone());
        let sig = self.lookup_named_method_sig(&canonical_name, type_args, method)?;
        let return_type = self
            .apply_instantiated_call_signature(
                &sig,
                None,
                args,
                span,
                SignatureArgApplication::PositionalOnly {
                    arity_context: format!("method `{method}`"),
                },
                true,
                Some(GenericCallee::Method {
                    type_name: &canonical_name,
                    method,
                    owner_type_args: type_args,
                }),
            )
            .return_type;
        // The successful signature lookup and the emitted impl body must use
        // the same declaration identity.  In particular, a fielded resource
        // wrapper such as `std.text.regex.Pattern` deliberately cannot be
        // rewritten to its raw handle extern: its source impl forwards
        // `self.handle` and reconstructs wrappers where needed.  Resolve the
        // canonical, source-qualified impl key here, at the lookup boundary,
        // rather than making HIR rediscover it from a presentation name.
        self.record_named_source_method_rewrite(receiver_ty, method, &sig, span);
        Some(self.qualify_method_return_to_receiver_owner(&canonical_name, &return_type))
    }

    /// Record a direct call to the exact source implementation that supplied
    /// a named-method signature.  This is intentionally keyed only by the
    /// checker-owned declaration map: same-leaf user types and registry
    /// aliases cannot mint an imported stdlib impl dispatch.
    fn record_named_source_method_rewrite(
        &mut self,
        receiver_ty: &Ty,
        method: &str,
        sig: &FnSig,
        span: &Span,
    ) {
        let Ty::Named {
            name,
            args: type_args,
            builtin,
            ..
        } = receiver_ty
        else {
            return;
        };
        let canonical_name = self
            .canonical_nominal_name(name)
            .unwrap_or_else(|| name.clone());
        let method_key = format!("{canonical_name}::{method}");
        let dispatch_key = if type_args.is_empty() {
            method_key.clone()
        } else {
            type_args
                .iter()
                .map(|ty| ResolvedTy::from_ty(&self.subst.resolve(ty)).ok())
                .collect::<Option<Vec<_>>>()
                .as_ref()
                .and_then(|args| crate::resolved_ty::mangle_impl_self_name(&canonical_name, args))
                .map(|owner| format!("{owner}::{method}"))
                .filter(|key| self.impl_method_declaration_ids.contains_key(key))
                .unwrap_or_else(|| method_key.clone())
        };
        let Some(declaration) = self.impl_method_declaration_ids.get(&dispatch_key).cloned() else {
            return;
        };
        let consumes_receiver = sig.consumes_receiver
            || self.named_type_method_consumes_receiver(&canonical_name, method)
            || self.named_type_inherent_close_consumes_receiver(
                &canonical_name,
                *builtin,
                method,
                sig,
            );
        if consumes_receiver {
            self.method_call_consumes_receiver
                .insert(SpanKey::in_module(span, self.current_module_idx));
        }
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteToFunction {
                target: CallTarget::impl_method(declaration),
                c_symbol: dispatch_key,
                descriptor: None,
                extern_identity: None,
                elem_ty: None,
                consumes_receiver,
                returns_receiver_identity: sig.returns_receiver_identity,
            },
        );
    }

    /// Restore the source owner on bare nominal types in a qualified receiver's
    /// method result.
    ///
    /// Impl signatures are registered while their declaring module is active,
    /// where a self-module type is legitimately spelled bare (`Listener::accept
    /// -> Connection`). At an imported call site the receiver has already
    /// acquired its exact identity (`net.Listener`), so letting that bare return
    /// escape would lose the owner again and make downstream layout/codegen
    /// confuse it with a root or foreign `Connection`.
    ///
    /// Only names proven in the same owner's `type_defs` are qualified. An
    /// already-qualified result (including `foo.Connection`) is authoritative
    /// and unchanged, as are builtins and a method on a bare/root receiver.
    pub(super) fn qualify_method_return_to_receiver_owner(
        &self,
        receiver_name: &str,
        ty: &Ty,
    ) -> Ty {
        let canonical_registry_receiver = self
            .module_registry
            .canonical_method_receiver_identity(receiver_name);
        let exact_receiver = if let Some(canonical) = canonical_registry_receiver.as_deref() {
            canonical
        } else if self.type_defs.contains_key(receiver_name) {
            receiver_name
        } else {
            return ty.clone();
        };
        let Some((owner, _)) = exact_receiver.rsplit_once('.') else {
            return ty.clone();
        };
        self.qualify_method_return_to_owner(owner, ty)
    }

    fn qualify_method_return_to_owner(&self, owner: &str, ty: &Ty) -> Ty {
        let mapped =
            ty.map_children_pub(&|child| self.qualify_method_return_to_owner(owner, child));
        let Ty::Named {
            name,
            args,
            builtin: None,
        } = mapped
        else {
            return mapped;
        };
        if name.contains('.') {
            let name = self
                .module_registry
                .canonical_registry_signature_type_identity(&name, owner)
                .unwrap_or(name);
            return Ty::Named {
                name,
                args,
                builtin: None,
            };
        }
        let qualified = format!("{owner}.{name}");
        Ty::Named {
            name: if self.type_defs.contains_key(&qualified)
                || self.module_registry.is_method_receiver_type(&qualified)
            {
                qualified
            } else {
                name
            },
            args,
            builtin: None,
        }
    }

    /// Enforce the actor mailbox boundary on every arg of an actor receive
    /// method dispatch. Called after [`Self::try_resolve_named_method`] has
    /// already type-checked the args (so `self.expr_types` is populated), to
    /// avoid double synthesis.
    ///
    /// Each arg's type is looked up from `expr_types`; on a miss (e.g. the
    /// program already has an error at that arg) we skip the boundary record
    /// for that arg rather than re-synthesize. Codegen's fail-closed lookup
    /// is gated to non-error programs.
    fn enforce_actor_method_send_args(&mut self, args: &[CallArg]) {
        // Snapshot per-arg types from `expr_types` first; calling
        // `enforce_actor_boundary_send` mutates `self`, so we cannot hold a
        // borrow into `self.expr_types` across the call.
        let arg_types: Vec<Option<Ty>> = args
            .iter()
            .map(|arg| {
                let (_expr, sp) = arg.expr();
                self.expr_types
                    .get(&SpanKey::in_module(sp, self.current_module_idx))
                    .cloned()
            })
            .collect();
        for (arg, ty_opt) in args.iter().zip(arg_types) {
            let (expr, sp) = arg.expr();
            if let Some(ty) = ty_opt {
                self.enforce_actor_boundary_send(expr, sp, sp, &ty);
            }
        }
    }

    fn call_arg_types(&self, args: &[CallArg]) -> Vec<Option<Ty>> {
        args.iter()
            .map(|arg| {
                let (_expr, sp) = arg.expr();
                self.expr_types
                    .get(&SpanKey::in_module(sp, self.current_module_idx))
                    .cloned()
            })
            .collect()
    }

    fn serializable_failure_reason(&self, ty: &Ty) -> String {
        let mut missing = Vec::new();
        if !self.registry.implements_marker(ty, MarkerTrait::Encode) {
            missing.push("Encode");
        }
        if !self.registry.implements_marker(ty, MarkerTrait::Decode) {
            missing.push("Decode");
        }
        if missing.is_empty() && self.contains_bytes_collection_key(ty, &mut HashSet::new()) {
            "an owned `bytes` map key or set element has no complete insertion ownership \
             protocol yet: duplicate insertion cannot release the caller-owned value; use \
             `string` or a supported fixed-width key"
                .to_string()
        } else if missing.is_empty() {
            "it is outside the current Serializable codec subset (including only \
             collection key/element layouts with a complete encode, decode, clone, and drop path)"
                .to_string()
        } else {
            format!("missing required marker trait(s): {}", missing.join(" + "))
        }
    }

    pub(super) fn record_generic_wire_codec_rewrite(
        &mut self,
        canonical_owner: &str,
        method: &str,
        params: &[Ty],
        return_type: &Ty,
        span: &Span,
    ) -> bool {
        if canonical_owner != "std.encoding.wire" {
            return false;
        }
        let direction = match method {
            "encode" => WireCodecDirection::Encode,
            "decode" => WireCodecDirection::Decode,
            "to_json" => WireCodecDirection::ToJson,
            "from_json" => WireCodecDirection::FromJson,
            "to_yaml" => WireCodecDirection::ToYaml,
            "from_yaml" => WireCodecDirection::FromYaml,
            _ => return false,
        };
        let value_source = if direction.is_serialize() {
            params.first().cloned()
        } else if direction == WireCodecDirection::Decode {
            Some(return_type.clone())
        } else {
            result_ok_payload(return_type)
        };
        if let Some(value_source) = value_source.map(|ty| self.subst.resolve(&ty)) {
            if let Ok(value_ty) = ResolvedTy::from_ty(&value_source) {
                self.record_method_call_rewrite(
                    span,
                    MethodCallRewrite::GenericWireCodec {
                        direction,
                        value_ty,
                    },
                );
            }
        }
        true
    }

    fn contains_bytes_collection_key(&self, ty: &Ty, visiting: &mut HashSet<String>) -> bool {
        match ty {
            Ty::Named {
                builtin: Some(BuiltinType::HashMap),
                args,
                ..
            } => {
                matches!(args.first(), Some(Ty::Bytes))
                    || args
                        .iter()
                        .any(|arg| self.contains_bytes_collection_key(arg, visiting))
            }
            Ty::Named {
                builtin: Some(BuiltinType::HashSet),
                args,
                ..
            } => {
                matches!(args.first(), Some(Ty::Bytes))
                    || args
                        .iter()
                        .any(|arg| self.contains_bytes_collection_key(arg, visiting))
            }
            Ty::Named {
                builtin: Some(_),
                args,
                ..
            } => args
                .iter()
                .any(|arg| self.contains_bytes_collection_key(arg, visiting)),
            Ty::Named {
                name,
                builtin: None,
                ..
            } => {
                if !visiting.insert(name.clone()) {
                    return false;
                }
                let found = self.registry.member_types(name).is_some_and(|members| {
                    members
                        .iter()
                        .any(|member| self.contains_bytes_collection_key(member, visiting))
                });
                visiting.remove(name);
                found
            }
            Ty::Tuple(items) => items
                .iter()
                .any(|item| self.contains_bytes_collection_key(item, visiting)),
            Ty::Array(item, _) | Ty::Slice(item) => {
                self.contains_bytes_collection_key(item, visiting)
            }
            _ => false,
        }
    }

    fn report_nonserializable_remote_actor_msg(&mut self, ty: &Ty, span: &Span) {
        self.report_error(
            TypeErrorKind::BoundsNotSatisfied,
            span,
            format!(
                "remote actor message type `{}` must implement Serializable before it can \
                 cross a RemotePid boundary; {}",
                ty.user_facing(),
                self.serializable_failure_reason(ty)
            ),
        );
    }

    fn report_nonserializable_remote_actor_reply(&mut self, ty: &Ty, span: &Span) {
        self.report_error(
            TypeErrorKind::BoundsNotSatisfied,
            span,
            format!(
                "remote actor reply type `{}` must implement Serializable before it can \
                 cross a RemotePid ask boundary; {}",
                ty.user_facing(),
                self.serializable_failure_reason(ty)
            ),
        );
    }

    fn enforce_remote_actor_msg_serializable(&mut self, ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(ty);
        if matches!(resolved, Ty::Var(_) | Ty::Error) {
            return true;
        }
        if self
            .registry
            .implements_marker(&resolved, MarkerTrait::Serializable)
        {
            true
        } else {
            self.report_nonserializable_remote_actor_msg(&resolved, span);
            false
        }
    }

    fn enforce_remote_actor_reply_serializable(&mut self, ty: &Ty, span: &Span) -> bool {
        let projected = self.project_assoc_types(ty);
        let resolved = self.subst.resolve(&projected);
        if matches!(resolved, Ty::Var(_) | Ty::Error) {
            return true;
        }
        if self
            .registry
            .implements_marker(&resolved, MarkerTrait::Serializable)
        {
            true
        } else {
            self.report_nonserializable_remote_actor_reply(&resolved, span);
            false
        }
    }

    fn enforce_remote_actor_ask_reply_serializable(&mut self, return_ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(return_ty);
        let Ty::Named {
            builtin: Some(BuiltinType::Result),
            args,
            ..
        } = resolved
        else {
            return true;
        };
        let Some(reply_ty) = args.first() else {
            return true;
        };
        self.enforce_remote_actor_reply_serializable(reply_ty, span)
    }

    /// Enforce the A640 remote serializability floor after method signature
    /// application has populated `expr_types` for every argument.
    fn enforce_remote_actor_method_serializable_args(&mut self, args: &[CallArg]) -> bool {
        let arg_types = self.call_arg_types(args);
        let mut all_serializable = true;
        for (arg, ty_opt) in args.iter().zip(arg_types) {
            let (_expr, sp) = arg.expr();
            if let Some(ty) = ty_opt {
                all_serializable &= self.enforce_remote_actor_msg_serializable(&ty, sp);
            }
        }
        all_serializable
    }

    fn is_unresolved_pid_msg_projection(ty: &Ty) -> bool {
        matches!(
            ty,
            Ty::AssocType {
                trait_name,
                assoc_name,
                ..
            } if trait_name.as_ref() == "std.builtins.Pid" && assoc_name.as_ref() == "Msg"
        )
    }

    fn report_pid_polymorphic_send_fail_closed(
        &mut self,
        type_param_name: &str,
        ty: &Ty,
        span: &Span,
    ) {
        self.report_error(
            TypeErrorKind::BoundsNotSatisfied,
            span,
            format!(
                "generic `Pid.send` on `{type_param_name}` is fail-closed: `{}` must be proven \
                 Serializable, but the current checker cannot express the required \
                 `P.Msg: Serializable` associated-type projection bound yet (TODO A640)",
                ty.user_facing()
            ),
        );
    }

    fn enforce_pid_polymorphic_send_serializable_args(
        &mut self,
        args: &[CallArg],
        type_param_name: &str,
    ) -> bool {
        let arg_types = self.call_arg_types(args);
        let mut all_serializable = true;
        for (arg, ty_opt) in args.iter().zip(arg_types) {
            let (_expr, sp) = arg.expr();
            if let Some(ty) = ty_opt {
                let resolved = self.subst.resolve(&ty);
                if Self::is_unresolved_pid_msg_projection(&resolved) {
                    self.report_pid_polymorphic_send_fail_closed(type_param_name, &resolved, sp);
                    all_serializable = false;
                } else {
                    all_serializable &= self.enforce_remote_actor_msg_serializable(&resolved, sp);
                }
            }
        }
        all_serializable
    }

    #[allow(
        clippy::too_many_lines,
        reason = "handles all named-type method dispatch; splitting would scatter related intercepts"
    )]
    pub(super) fn check_named_method_fallback(
        &mut self,
        receiver_ty: &Ty,
        method_name: &str,
        args: &[CallArg],
        span: &Span,
        type_display_name: &str,
    ) -> Ty {
        if let Some(ty) = self.try_resolve_named_method(receiver_ty, method_name, args, span) {
            if let Ty::Named { name, .. } = receiver_ty {
                // If the receiver type is a registered actor declaration AND
                // the resolved method is a receive handler (tracked in
                // `actor_receive_methods`), this dispatch crosses the
                // actor mailbox boundary. Record the per-arg alias-vs-copy
                // decision so codegen does not have to guess. Non-receive
                // `methods` declared on the same actor (also keyed
                // `{Actor}::{name}` in `fn_sigs`) stay on the regular
                // method-call path.
                let method_key = format!("{name}::{method_name}");
                let is_actor_receive_dispatch = self
                    .type_defs
                    .get(name)
                    .is_some_and(|td| td.kind == TypeDefKind::Actor)
                    && self.actor_receive_methods.contains(&method_key);
                if is_actor_receive_dispatch {
                    self.record_method_call_receiver_kind(
                        span,
                        MethodCallReceiverKind::ActorInstance {
                            actor_name: name.clone(),
                        },
                    );
                    self.enforce_actor_method_send_args(args);
                    // Ask-without-await guard: if this receive fn returns a value
                    // (ask-shaped), is not a generator (those use `for await`, not
                    // bare `await`), and the call is not directly under `await`,
                    // reject it with a clear diagnostic pointing at the fix.
                    let resolved_ty = self.subst.resolve(&ty);
                    let is_ask_shaped = !matches!(resolved_ty, Ty::Unit)
                        && !self.receive_generator_methods.contains(&method_key);
                    if is_ask_shaped && !self.inside_await_expr {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            format!(
                                "actor ask `{name}.{method_name}` requires `await`; \
                                 write `let v? = await ref.{method_name}(...)` \
                                 or `match await ref.{method_name}(...) {{ Ok(v) => ..., Err(e) => ... }}`",
                            ),
                        );
                        // Still record the dispatch so HIR/MIR have a sane entry; the
                        // type checker already emitted the error so this is recovery.
                    }
                    return self.record_actor_method_dispatch(span, method_key, ty.clone());
                }
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::NamedTypeInstance {
                        type_name: name.clone(),
                    },
                );
            }
            self.record_handle_method_call_rewrite_if_any(receiver_ty, method_name, span);
            return ty;
        }

        // Fn-typed field call: `w.cb(args)` where `cb` is a record field of
        // function type dispatches as a field-load + closure call, not a
        // method lookup. Pre-validated here (arity + per-arg types against
        // the field's signature) and recorded as a structured rewrite so HIR
        // never guesses (`checker-codegen-pattern-contract`). A field that
        // exists but is NOT fn-typed falls through to `UndefinedMethod` —
        // the gate keeps rejecting what it claims to.
        if let Some(ret_ty) = self.try_record_fn_field_call(receiver_ty, method_name, args, span) {
            return ret_ty;
        }

        // `clone` on a user-defined record type: intercept before `UndefinedMethod`
        // and record a `RecordCloneInplace` rewrite when the record is admissible
        // (no opaque fields, not a generic record, not an enum/actor/machine).
        // Fail closed with a named diagnostic for unclonable shapes (opaque fields,
        // generic params). LESSONS: `checker-authority`, `admit-only-what-you-lower`,
        // `unclonable-leaf-fails-closed-transitively`.
        if method_name == "clone" && args.is_empty() {
            if let Ty::Named {
                name,
                args: type_args,
                builtin: None,
            } = receiver_ty
            {
                match self.record_clone_admissibility(name, type_args, span) {
                    RecordCloneAdmissibility::Admissible => {
                        let record_ty = receiver_ty.clone();
                        self.record_method_call_rewrite(
                            span,
                            MethodCallRewrite::RecordCloneInplace {
                                record_name: name.clone(),
                            },
                        );
                        // Seed for codegen's `emit_state_clone_drop_synthesis`.
                        // Bare-seed MONOMORPHIC records only: a generic
                        // instantiation (`type_args` present) is keyed by its
                        // monomorphised layout (`Pair$$i64$i64`) in MIR and
                        // seeded from the `RecordCloneInplace` walk in codegen
                        // (`collect_record_clone_inplace_seeds`). The bare name
                        // names no monomorphic layout, so seeding it here would
                        // register a dead key. This mirrors the MIR keying
                        // (`monomorphic_user_record_key`, `args.is_empty()`).
                        if type_args.is_empty() && !self.user_clone_record_seeds.contains(name) {
                            self.user_clone_record_seeds.push(name.clone());
                        }
                        return record_ty;
                    }
                    RecordCloneAdmissibility::OpaqueField {
                        opaque_name,
                        member,
                    } => {
                        // Synthesize args (none here) for error-recovery symmetry.
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "type `{name}` cannot be cloned because member `{member}` contains \
                                 opaque value `{opaque_name}`"
                            ),
                        );
                        return Ty::Error;
                    }
                    RecordCloneAdmissibility::AffineValue {
                        type_name,
                        marker,
                        member,
                    } => {
                        let receiver_name = receiver_ty.user_facing().to_string();
                        self.report_affine_record_clone_error(
                            &receiver_name,
                            &type_name,
                            marker,
                            &member,
                            span,
                        );
                        return Ty::Error;
                    }
                    RecordCloneAdmissibility::MissingClone { member, member_ty } => {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "type `{}` cannot be cloned because member `{member}` of type `{}` \
                                 has no Clone capability",
                                receiver_ty.user_facing(),
                                member_ty.user_facing()
                            ),
                        );
                        return Ty::Error;
                    }
                    RecordCloneAdmissibility::UnbalancedSharedHandle { type_name, member } => {
                        let receiver_name = receiver_ty.user_facing().to_string();
                        self.report_unbalanced_shared_handle_clone_error(
                            &receiver_name,
                            &type_name,
                            &member,
                            span,
                        );
                        return Ty::Error;
                    }
                    RecordCloneAdmissibility::GenericRecord => {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "cloning generic record `{name}` is not yet supported; \
                                 only monomorphic (non-generic) records can be cloned"
                            ),
                        );
                        return Ty::Error;
                    }
                    RecordCloneAdmissibility::AbstractParamClone => {
                        // Bare type param `x: T` with `T: Clone`. Record the
                        // clone rewrite but DO NOT seed `user_clone_record_seeds`
                        // — `T` names no monomorphic record layout; codegen
                        // would synthesise a dead `__hew_record_clone_inplace_T`.
                        // The concrete copy path is selected per-mono in MIR
                        // (`subst_ty(T)` → value-class dispatch).
                        let param_ty = receiver_ty.clone();
                        self.record_method_call_rewrite(
                            span,
                            MethodCallRewrite::RecordCloneInplace {
                                record_name: name.clone(),
                            },
                        );
                        return param_ty;
                    }
                    RecordCloneAdmissibility::EnumClone { enum_name } => {
                        // A user enum routes through the SAME rewrite + HIR node
                        // as a record clone; MIR demuxes record-vs-enum by the
                        // resolved monomorphised layout (`enum_clone_layout_key`)
                        // and emits `EnumCloneInplace`, lowered to
                        // `__hew_enum_clone_inplace_<E>`. No bare-name seed: the
                        // enum clone-site is seeded from the `EnumCloneInplace`
                        // walk in the MIR thunk registry (`collect_enum_clone_inplace_seeds`),
                        // keyed by the monomorphised layout, so a generic
                        // instantiation never registers a dead bare key.
                        let enum_ty = receiver_ty.clone();
                        self.record_method_call_rewrite(
                            span,
                            MethodCallRewrite::RecordCloneInplace {
                                record_name: enum_name,
                            },
                        );
                        return enum_ty;
                    }
                    RecordCloneAdmissibility::NotARecord => {
                        // Fall through to `UndefinedMethod` below for non-record
                        // Named types (actors, machines, etc.); enums are
                        // handled by the `EnumClone` arm above.
                    }
                }
            }
        }

        // Synthesize args for error recovery so independent arg diagnostics are not suppressed.
        for arg in args {
            let (expr, sp) = arg.expr();
            self.synthesize(expr, sp);
        }
        self.report_error_with_suggestions(
            TypeErrorKind::UndefinedMethod,
            span,
            format!("no method `{method_name}` on {type_display_name}"),
            self.similar_methods(receiver_ty, method_name),
        );
        Ty::Error
    }

    /// Recognise `receiver.field(args)` where `field` resolves to a record
    /// field of function/closure type. Returns the call's type (the field
    /// signature's return type) after checking arity and arguments, or
    /// `None` when the receiver is not a record, the field does not exist,
    /// or the field is not function-typed (the caller's `UndefinedMethod`
    /// fall-through then applies).
    fn try_record_fn_field_call(
        &mut self,
        receiver_ty: &Ty,
        method_name: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let Ty::Named {
            name,
            args: type_args,
            builtin: None,
        } = receiver_ty
        else {
            return None;
        };
        let type_def = self.lookup_type_def(name)?;
        let field_ty = type_def.fields.get(method_name)?;
        let field_ty =
            Self::instantiate_type_def_member(field_ty, &type_def.type_params, type_args);
        let resolved_field = self.subst.resolve(&field_ty);
        let (params, ret) = match &resolved_field {
            Ty::Function { params, ret } | Ty::Closure { params, ret, .. } => {
                (params.clone(), (**ret).clone())
            }
            _ => return None,
        };
        if args.len() != params.len() {
            self.report_error(
                TypeErrorKind::ArityMismatch,
                span,
                format!(
                    "field `{method_name}` on `{name}` is `{}` and takes {} argument(s), \
                     but {} were supplied",
                    resolved_field.user_facing(),
                    params.len(),
                    args.len()
                ),
            );
            return Some(Ty::Error);
        }
        for (arg, param_ty) in args.iter().zip(params.iter()) {
            let (expr, sp) = arg.expr();
            self.check_against(expr, sp, param_ty);
        }
        if let Ok(field_resolved) = crate::resolved_ty::ResolvedTy::from_ty(&resolved_field) {
            self.record_method_call_rewrite(
                span,
                MethodCallRewrite::RecordFnFieldCall {
                    field_ty: field_resolved,
                },
            );
        }
        Some(ret)
    }

    /// Extract the signed value of a syntactic integer-literal expression,
    /// folding a single leading unary negation (`-2` parses as
    /// `Unary { Negate, Literal::Integer(2) }`).  Returns `None` for any
    /// non-literal expression — those are validated at runtime, never const.
    fn literal_integer_value(expr: &Expr) -> Option<i64> {
        match expr {
            Expr::Literal(Literal::Integer { value, .. }) => Some(*value),
            Expr::Unary {
                op: UnaryOp::Negate,
                operand,
            } => match &operand.0 {
                Expr::Literal(Literal::Integer { value, .. }) => Some(value.wrapping_neg()),
                _ => None,
            },
            _ => None,
        }
    }

    fn similar_methods(&self, receiver_ty: &Ty, method_name: &str) -> Vec<String> {
        crate::error::find_similar(
            method_name,
            collect_method_sigs_for_receiver(&self.type_defs, &self.fn_sigs, receiver_ty)
                .iter()
                .map(|(name, _)| name.as_str()),
        )
    }

    /// Decide whether a user-defined `Ty::Named` record type is admissible for
    /// `clone`. Returns one of the following outcomes:
    ///
    /// - `Admissible`: the record can be cloned end-to-end via the synthesised
    ///   `__hew_record_clone_inplace_<R>` thunk.
    /// - `OpaqueField { opaque_name }`: the record (or a transitively reachable
    ///   field) contains an opaque handle — fail closed with a named diagnostic.
    /// - `GenericRecord`: the record has un-substituted generic type parameters
    ///   — not yet supported; fail closed with an NYI diagnostic.
    /// - `EnumClone { enum_name }`: the receiver is a user enum — clone via the
    ///   enum twin `__hew_enum_clone_inplace_<E>` (tag-dispatched payload clone).
    /// - `NotARecord`: not a clone-eligible named type (actor, machine, etc.) —
    ///   fall through to `UndefinedMethod`.
    ///
    /// LESSONS: `checker-authority` (sole authority for clone admissibility),
    /// `unclonable-leaf-fails-closed-transitively`, `admit-only-what-you-lower`.
    pub(super) fn record_clone_admissibility(
        &self,
        name: &str,
        type_args: &[Ty],
        _span: &Span,
    ) -> RecordCloneAdmissibility {
        use TypeDefKind::{Enum, Record, Struct};
        let receiver_ty = Ty::Named {
            name: name.to_string(),
            args: type_args.to_vec(),
            builtin: None,
        };
        if self.registry.is_resource(name) {
            return RecordCloneAdmissibility::AffineValue {
                type_name: name.to_string(),
                marker: hew_parser::ast::ResourceMarker::Resource,
                member: "value".to_string(),
            };
        }
        if self.registry.is_linear(name) {
            return RecordCloneAdmissibility::AffineValue {
                type_name: name.to_string(),
                marker: hew_parser::ast::ResourceMarker::Linear,
                member: "value".to_string(),
            };
        }
        let Some(type_def) = self.type_defs.get(name) else {
            // A bare type parameter (`x: T`) has no `type_defs` entry. When it
            // carries a `Clone` bound in scope (`fn f<T: Clone>(x: T)`), admit
            // the clone and defer the concrete copy path to monomorphization
            // (`AbstractParamClone`). This is the abstract-`T: Clone` spine
            // (mirrors `type_param_has_marker_bound` as used by abstract-key
            // HashMap dispatch). Without the bound, fall through to `NotARecord`
            // → `UndefinedMethod` (fail closed — `admit-only-what-you-lower`).
            if self.is_type_param_in_scope(name)
                && self.type_param_has_marker_bound(name, MarkerTrait::Clone)
            {
                return RecordCloneAdmissibility::AbstractParamClone;
            }
            return RecordCloneAdmissibility::NotARecord;
        };
        if !type_def.type_params.is_empty() && type_args.iter().any(|arg| matches!(arg, Ty::Var(_)))
        {
            return RecordCloneAdmissibility::GenericRecord;
        }
        if let Some(blocker) = self.structural_clone_blocker(&receiver_ty) {
            return match blocker {
                CloneCapabilityBlocker::Affine {
                    type_name,
                    marker,
                    member,
                } => RecordCloneAdmissibility::AffineValue {
                    type_name,
                    marker,
                    member,
                },
                CloneCapabilityBlocker::Opaque { type_name, member } => {
                    RecordCloneAdmissibility::OpaqueField {
                        opaque_name: type_name,
                        member,
                    }
                }
                CloneCapabilityBlocker::Missing { member, member_ty } => {
                    RecordCloneAdmissibility::MissingClone { member, member_ty }
                }
                CloneCapabilityBlocker::UnbalancedSharedHandle { type_name, member } => {
                    RecordCloneAdmissibility::UnbalancedSharedHandle { type_name, member }
                }
            };
        }
        // An enum is clone-eligible via the enum twin of the record thunk. It is
        // checked BEFORE the Record/Struct gate because the two paths diverge:
        // an enum's owned leaves live in variant payloads, not declared fields.
        if matches!(type_def.kind, Enum) {
            return RecordCloneAdmissibility::EnumClone {
                enum_name: name.to_string(),
            };
        }
        // Only Record and Struct (value-type) kinds are clone-eligible.
        if !matches!(type_def.kind, Record | Struct) {
            return RecordCloneAdmissibility::NotARecord;
        }
        RecordCloneAdmissibility::Admissible
    }

    fn report_affine_record_clone_error(
        &mut self,
        receiver_name: &str,
        affine_name: &str,
        marker: hew_parser::ast::ResourceMarker,
        member: &str,
        span: &Span,
    ) {
        let message =
            Self::affine_record_clone_error_message(receiver_name, affine_name, marker, member);
        self.report_error(TypeErrorKind::InvalidOperation, span, message);
    }

    fn affine_record_clone_error_message(
        receiver_name: &str,
        affine_name: &str,
        marker: hew_parser::ast::ResourceMarker,
        member: &str,
    ) -> String {
        let (attribute, contract) = match marker {
            hew_parser::ast::ResourceMarker::Resource => (
                "#[resource]",
                "has an affine close contract and no semantic clone",
            ),
            hew_parser::ast::ResourceMarker::Linear => (
                "#[linear]",
                "must be consumed exactly once and has no semantic clone",
            ),
            hew_parser::ast::ResourceMarker::None => {
                unreachable!("affine clone blocker cannot carry ResourceMarker::None")
            }
        };
        let subject = if receiver_name == affine_name {
            format!("type `{receiver_name}` is `{attribute}`")
        } else {
            format!("type `{receiver_name}` contains `{attribute}` value `{affine_name}`")
        };
        format!(
            "{subject} and cannot be cloned: member `{member}` contains `{affine_name}`, which \
             {contract}"
        )
    }

    /// Diagnostic for a refcounted shared handle sitting inside an aggregate.
    ///
    /// Names the exact member path so the programmer can see which leaf blocks
    /// the clone, and states the mechanism rather than a bare "not supported".
    fn unbalanced_shared_handle_clone_error_message(
        receiver_name: &str,
        type_name: &str,
        member: &str,
    ) -> String {
        format!(
            "type `{receiver_name}` cannot be cloned because member `{member}` of type \
             `{type_name}` is a shared refcounted handle with no aggregate-ingress retain: \
             the composite drop would release it once per owner"
        )
    }

    fn report_unbalanced_shared_handle_clone_error(
        &mut self,
        receiver_name: &str,
        type_name: &str,
        member: &str,
        span: &Span,
    ) {
        let message =
            Self::unbalanced_shared_handle_clone_error_message(receiver_name, type_name, member);
        // Deliberately NO workaround: cloning the handle on its own and
        // rebuilding the aggregate re-enters the same ingress path and aborts
        // at `hew-runtime/src/rc.rs` `Rc double-free`. Suggesting it would hand
        // the programmer a crash. State the limitation instead.
        self.report_error_with_suggestions(
            TypeErrorKind::UndefinedMethod,
            span,
            message,
            vec![format!(
                "this is a known gap in shared-handle ownership, not a property of \
                 `{receiver_name}`; a fix is pending. Until then keep the `{type_name}` handle \
                 out of a cloned aggregate — pass the aggregate by move, or hold the handle in a \
                 collection (`Vec<{type_name}>`), whose element clone retains correctly"
            )],
        );
    }

    fn clone_member_path(parent: &str, member: &str) -> String {
        if parent.is_empty() {
            member.to_string()
        } else {
            format!("{parent}.{member}")
        }
    }

    fn structural_clone_blocker(&self, ty: &Ty) -> Option<CloneCapabilityBlocker> {
        self.structural_clone_blocker_inner(ty, "", false, &mut std::collections::HashSet::new())
    }

    /// Validate the exceptional element types that cannot use `Vec`'s
    /// borrow-only index path.
    ///
    /// Ordinary `#[resource]` / `#[linear]` elements are deliberately admitted:
    /// MIR lowers `values[i]` through the owned-layout getter as an interior
    /// borrow and its escape/consume/rebind checks keep that borrow inside the
    /// collection's release authority. A `Receiver`, however, is an opaque
    /// single-consumer endpoint with no readable borrowed-value surface, so it
    /// remains rejected regardless of its payload type.
    pub(super) fn validate_vec_index_borrow_surface(&mut self, ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        if matches!(
            resolved,
            Ty::Named {
                builtin: Some(BuiltinType::Receiver),
                ..
            }
        ) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "cannot index `Vec<{}>` by value: `Receiver` is a non-cloneable, \
                     single-consumer endpoint; use `pop`, `remove`, or consuming iteration \
                     to move the endpoint out",
                    resolved.user_facing()
                ),
            );
            return false;
        }
        true
    }

    #[expect(
        clippy::too_many_lines,
        reason = "the closed member walk keeps clone refusal paths aligned with every stored shape"
    )]
    /// `in_value_aggregate` is true only when this position is a member of a
    /// VALUE aggregate — a tuple element, an `Option`/`Result` payload, a record
    /// field, or an enum variant payload. It is deliberately NOT inherited: a
    /// builtin heap container resets it for its own elements, because a
    /// container clones its elements through the owned-element thunk (which
    /// retains) rather than by bit-copying a shared handle into a second
    /// composite drop plan. `clone Vec<Rc<T>>` is balanced today and must stay
    /// admitted; `clone (Rc<T>, string)` is not.
    fn structural_clone_blocker_inner(
        &self,
        ty: &Ty,
        path: &str,
        in_value_aggregate: bool,
        visiting: &mut std::collections::HashSet<String>,
    ) -> Option<CloneCapabilityBlocker> {
        use hew_parser::ast::ResourceMarker;

        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        match &resolved {
            Ty::Tuple(items) => {
                for (index, item) in items.iter().enumerate() {
                    let member = Self::clone_member_path(path, &index.to_string());
                    if let Some(blocker) =
                        self.structural_clone_blocker_inner(item, &member, true, visiting)
                    {
                        return Some(blocker);
                    }
                }
            }
            Ty::Named {
                name,
                args,
                builtin,
            } => {
                if in_value_aggregate
                    && matches!(builtin, Some(BuiltinType::Rc | BuiltinType::Weak))
                {
                    return Some(CloneCapabilityBlocker::UnbalancedSharedHandle {
                        type_name: resolved.user_facing().to_string(),
                        member: path.to_string(),
                    });
                }
                if builtin.is_some_and(BuiltinType::is_affine_clone_terminal) {
                    return None;
                }
                let member = if path.is_empty() { "value" } else { path };
                // Type-parameter capability inside a generic template comes
                // from the parameter's declared BOUND, never from a concrete
                // type (there is none yet). `T: Clone` makes every `T`-shaped
                // member clonable in the template; an unbounded `T` is not.
                // The concrete capability is decided at instantiation, where
                // `enforce_type_param_bounds` rejects an argument that does not
                // satisfy the bound — so an affine resource can never reach a
                // `T: Clone` position. This is the single template-capability
                // authority; structural equality applies the same split from
                // the other side (see `finalize_generic_structural_eq`), where
                // the checker re-runs the eligibility walk per instantiation
                // because a semantic `Eq` bound does not imply a structural
                // compare path.
                if let Some(capability) = self.type_param_template_clone_capability(&resolved) {
                    return if capability {
                        None
                    } else {
                        Some(CloneCapabilityBlocker::Missing {
                            member: member.to_string(),
                            member_ty: resolved.clone(),
                        })
                    };
                }
                if self.registry.is_resource(name) {
                    return Some(CloneCapabilityBlocker::Affine {
                        type_name: name.clone(),
                        marker: ResourceMarker::Resource,
                        member: member.to_string(),
                    });
                }
                if self.registry.is_linear(name) {
                    return Some(CloneCapabilityBlocker::Affine {
                        type_name: name.clone(),
                        marker: ResourceMarker::Linear,
                        member: member.to_string(),
                    });
                }
                if self.canonical_owned_handle_type_name(name).is_some()
                    || self.user_opaque_type_names.contains(name.as_str())
                {
                    // The carrier's own capability is canonical. In particular,
                    // Receiver<T> is one non-cloneable endpoint regardless of T;
                    // walking T first made diagnostics and semantics payload-dependent.
                    return Some(CloneCapabilityBlocker::Opaque {
                        type_name: name.clone(),
                        member: member.to_string(),
                    });
                }
                if matches!(builtin, Some(BuiltinType::Option | BuiltinType::Result)) {
                    for (index, arg) in args.iter().enumerate() {
                        let label = if args.len() == 1 {
                            "Some"
                        } else if index == 0 {
                            "Ok"
                        } else {
                            "Err"
                        };
                        let member = Self::clone_member_path(path, label);
                        if let Some(blocker) =
                            self.structural_clone_blocker_inner(arg, &member, true, visiting)
                        {
                            return Some(blocker);
                        }
                    }
                    return None;
                }
                if builtin.is_some() {
                    for (index, arg) in args.iter().enumerate() {
                        let label = if args.len() == 1 {
                            "element".to_string()
                        } else {
                            index.to_string()
                        };
                        let member = Self::clone_member_path(path, &label);
                        if let Some(blocker) =
                            self.structural_clone_blocker_inner(arg, &member, false, visiting)
                        {
                            return Some(blocker);
                        }
                    }
                }
                if let Some(type_def) = self.lookup_type_def(name) {
                    let visit_key = type_def.name.clone();
                    if !visiting.insert(visit_key.clone()) {
                        return None;
                    }
                    let mut field_names: Vec<&String> = type_def.fields.keys().collect();
                    field_names.sort();
                    for field_name in field_names {
                        let field_ty = type_def
                            .fields
                            .get(field_name)
                            .expect("field name came from this type definition");
                        let field_ty = Self::instantiate_type_def_member(
                            field_ty,
                            &type_def.type_params,
                            args,
                        );
                        let member = Self::clone_member_path(path, field_name);
                        if let Some(blocker) =
                            self.structural_clone_blocker_inner(&field_ty, &member, true, visiting)
                        {
                            visiting.remove(&visit_key);
                            return Some(blocker);
                        }
                    }
                    for (index, field_ty) in self
                        .tuple_record_constructor_fields(name, &type_def)
                        .iter()
                        .enumerate()
                    {
                        let field_ty = Self::instantiate_type_def_member(
                            field_ty,
                            &type_def.type_params,
                            args,
                        );
                        let member = Self::clone_member_path(path, &index.to_string());
                        if let Some(blocker) =
                            self.structural_clone_blocker_inner(&field_ty, &member, true, visiting)
                        {
                            visiting.remove(&visit_key);
                            return Some(blocker);
                        }
                    }
                    let mut variant_names: Vec<&String> = type_def.variants.keys().collect();
                    variant_names.sort();
                    for variant_name in variant_names {
                        let variant = type_def
                            .variants
                            .get(variant_name)
                            .expect("variant name came from this type definition");
                        let blocker = match variant {
                            VariantDef::Unit => None,
                            VariantDef::Tuple(fields) => {
                                fields.iter().enumerate().find_map(|(index, field_ty)| {
                                    let field_ty = Self::instantiate_type_def_member(
                                        field_ty,
                                        &type_def.type_params,
                                        args,
                                    );
                                    let member = Self::clone_member_path(
                                        path,
                                        &format!("{variant_name}.{index}"),
                                    );
                                    self.structural_clone_blocker_inner(
                                        &field_ty, &member, true, visiting,
                                    )
                                })
                            }
                            VariantDef::Struct(fields) => {
                                fields.iter().find_map(|(field_name, field_ty)| {
                                    let field_ty = Self::instantiate_type_def_member(
                                        field_ty,
                                        &type_def.type_params,
                                        args,
                                    );
                                    let member = Self::clone_member_path(
                                        path,
                                        &format!("{variant_name}.{field_name}"),
                                    );
                                    self.structural_clone_blocker_inner(
                                        &field_ty, &member, true, visiting,
                                    )
                                })
                            }
                        };
                        if blocker.is_some() {
                            visiting.remove(&visit_key);
                            return blocker;
                        }
                    }
                    visiting.remove(&visit_key);
                    return None;
                }
            }
            Ty::Array(elem, _) => {
                let member = Self::clone_member_path(path, "element");
                if let Some(blocker) =
                    self.structural_clone_blocker_inner(elem, &member, false, visiting)
                {
                    return Some(blocker);
                }
            }
            _ => {}
        }

        if self
            .registry
            .implements_marker(&resolved, MarkerTrait::Clone)
        {
            return None;
        }
        // The template-capability authority applies at EVERY position a type
        // parameter appears, not just at a bare `T`. Reaching here means the
        // structural walk above found no blocker, so every type-parameter
        // position inside `resolved` was already decided by its declared bound.
        // The marker registry cannot answer for a partially abstract type — it
        // has no impl for `Vec<T>` — so letting it veto here rejected
        // `fn dup<T: Clone>(v: Option<Vec<T>>)` even though every leaf was
        // clonable. An unbounded parameter still refuses, with a member path,
        // from the recursive call that examined it.
        let in_scope: Vec<String> = self.current_type_param_names().into_iter().collect();
        if Self::ty_mentions_type_params(&resolved, &in_scope) {
            return None;
        }
        Some(CloneCapabilityBlocker::Missing {
            member: if path.is_empty() {
                "value".to_string()
            } else {
                path.to_string()
            },
            member_ty: resolved,
        })
    }

    /// Transitive, substitution-aware walk of a (possibly generic) record's
    /// fields looking for an opaque handle leaf. `type_args` are the concrete
    /// arguments at the clone site; each field is instantiated with them before
    /// recursing, so a concrete instantiation like `Box<Handle>` resolves its
    /// `item: T` field to `item: Handle` and the opaque leaf is detected. A
    /// monomorphic record passes `type_args = []`, so the substitution is a
    /// no-op and behaviour is unchanged. Returns the first opaque field-type
    /// name found, or `None` if clean. Uses `canonical_owned_handle_type_name`
    /// as the single opaque-detection authority (mirrors `ty_contains_owned_handle`
    /// in `registration.rs`); the substitution mirrors
    /// `vec_element_contains_structural_array` (admissibility.rs).
    fn record_field_contains_opaque(
        &self,
        name: &str,
        type_args: &[Ty],
        visiting: &mut std::collections::HashSet<String>,
        skip_channel_handles: bool,
    ) -> Option<String> {
        if !visiting.insert(name.to_string()) {
            return None; // cycle protection
        }
        let mut found = None;
        if let Some(type_def) = self.type_defs.get(name) {
            for field_ty in type_def.fields.values() {
                let field_ty =
                    Self::instantiate_type_def_member(field_ty, &type_def.type_params, type_args);
                if let Some(opaque) =
                    self.ty_field_contains_opaque(&field_ty, visiting, skip_channel_handles)
                {
                    found = Some(opaque);
                    break;
                }
            }
        }
        visiting.remove(name);
        found
    }

    /// Transitive, substitution-aware walk of a (possibly generic) enum's
    /// variant payloads looking for an opaque-handle leaf — the enum twin of
    /// [`Self::record_field_contains_opaque`]. Each variant payload type
    /// (`Tuple` positional, `Struct` named) is instantiated with the concrete
    /// clone-site `type_args` before recursing, so `Maybe<Handle>` resolves its
    /// `Some(T)` payload to `Some(Handle)` and the opaque leaf is detected. A
    /// monomorphic enum passes `type_args = []` (a no-op substitution). Returns
    /// the first opaque payload-type name found, or `None` if clean. Shares the
    /// `ty_field_contains_opaque` leaf classifier with the record walk, so the
    /// two stay in lockstep.
    fn enum_variant_contains_opaque(
        &self,
        name: &str,
        type_args: &[Ty],
        visiting: &mut std::collections::HashSet<String>,
        skip_channel_handles: bool,
    ) -> Option<String> {
        if !visiting.insert(name.to_string()) {
            return None; // cycle protection
        }
        let mut found = None;
        if let Some(type_def) = self.type_defs.get(name) {
            'variants: for variant in type_def.variants.values() {
                let payload_tys: Vec<Ty> = match variant {
                    VariantDef::Unit => Vec::new(),
                    VariantDef::Tuple(tys) => tys.clone(),
                    VariantDef::Struct(fields) => fields.iter().map(|(_, ty)| ty.clone()).collect(),
                };
                for payload_ty in &payload_tys {
                    let payload_ty = Self::instantiate_type_def_member(
                        payload_ty,
                        &type_def.type_params,
                        type_args,
                    );
                    if let Some(opaque) =
                        self.ty_field_contains_opaque(&payload_ty, visiting, skip_channel_handles)
                    {
                        found = Some(opaque);
                        break 'variants;
                    }
                }
            }
        }
        visiting.remove(name);
        found
    }

    /// Return the first opaque message-payload type, exempting compiler-built-in
    /// channel endpoints and actor references that local actor delivery
    /// transfers as handle values.
    pub(super) fn ty_message_payload_contains_opaque(
        &self,
        ty: &Ty,
        visiting: &mut std::collections::HashSet<String>,
    ) -> Option<String> {
        self.ty_field_contains_opaque(ty, visiting, true)
    }

    fn ty_field_contains_opaque(
        &self,
        ty: &Ty,
        visiting: &mut std::collections::HashSet<String>,
        skip_channel_handles: bool,
    ) -> Option<String> {
        // Resolve inference vars so a field whose type is still a `Ty::Var`
        // bound in the substitution environment is walked at its concrete type
        // (mirrors `vec_element_contains_structural_array`).
        let resolved = self.subst.resolve(ty);
        match &resolved {
            Ty::Named {
                name,
                args,
                builtin,
                ..
            } => {
                if skip_channel_handles
                    && matches!(
                        builtin,
                        Some(
                            crate::BuiltinType::Sender
                                | crate::BuiltinType::Receiver
                                | crate::BuiltinType::LocalPid
                                | crate::BuiltinType::RemotePid
                        )
                    )
                {
                    return None;
                }
                // Direct opaque handle (imported via module registry OR user-declared #[opaque])?
                if self.canonical_owned_handle_type_name(name).is_some()
                    || self.user_opaque_type_names.contains(name.as_str())
                {
                    return Some(name.clone());
                }
                // Recurse into type args (e.g. `Vec<Handle>`, `Option<Handle>`).
                for arg in args {
                    if let Some(n) =
                        self.ty_field_contains_opaque(arg, visiting, skip_channel_handles)
                    {
                        return Some(n);
                    }
                }
                // Recurse into the type def's fields, substituting the def's
                // params with the concrete `args` at this use site.
                if let Some(n) =
                    self.record_field_contains_opaque(name, args, visiting, skip_channel_handles)
                {
                    return Some(n);
                }
                // Recurse into enum variant payloads too — a nested enum with a
                // hard-coded opaque payload (`enum Inner { B(Handle) }`) is
                // reachable from neither the type-arg walk nor the record-field
                // walk, so without this an outer clone would admit an
                // unclonable leaf. (`record_field_contains_opaque` is a no-op
                // for an enum, and vice-versa, so both calls are kind-safe.)
                if let Some(n) =
                    self.enum_variant_contains_opaque(name, args, visiting, skip_channel_handles)
                {
                    return Some(n);
                }
                None
            }
            Ty::Tuple(items) => {
                for item in items {
                    if let Some(n) =
                        self.ty_field_contains_opaque(item, visiting, skip_channel_handles)
                    {
                        return Some(n);
                    }
                }
                None
            }
            _ => None,
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "builtin stream typing and checker-owned rewrite metadata stay together"
    )]
    pub(super) fn check_stream_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let Some(inner) = self.validate_stream_sink_element_type(
            type_args,
            BuiltinNamedType::Stream.canonical_name(),
            method,
            span,
        ) else {
            return Ty::Error;
        };
        if method == "decode" {
            return self.report_unlowerable_stream_codec_boundary(
                BuiltinNamedType::Stream.canonical_name(),
                &inner,
                method,
                span,
            );
        }
        // Gate 2: lowering-capability check. The element-layout witness
        // carries every describable element type through the layout recv
        // entries; only elements the witness provably cannot describe
        // (containers, handles, closures) fail closed here. Emit a
        // user-facing diagnostic rather than the ICE-flavoured "missing
        // runtime rewrite metadata" from require_builtin_runtime_symbol.
        let resolved_inner = self.subst.resolve(&inner);
        if !matches!(resolved_inner, Ty::Var(_)) && !self.queue_elem_admissible(&resolved_inner) {
            let reason = self.queue_elem_rejection_reason(&resolved_inner);
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`Stream<{}>` is not supported: {reason}",
                    inner.user_facing()
                ),
            );
            return Ty::Error;
        }
        let receiver_ty = Ty::stream(inner.clone());
        let Some(sig) = lookup_builtin_method_sig(&receiver_ty, method) else {
            for arg in args {
                let (expr, sp) = arg.expr();
                self.synthesize(expr, sp);
            }
            self.report_error_with_suggestions(
                TypeErrorKind::UndefinedMethod,
                span,
                format!("no method `{method}` on `Stream<{}>`", inner.user_facing()),
                self.similar_methods(&receiver_ty, method),
            );
            return Ty::Error;
        };
        let resolved_inner = self.subst.resolve(&inner);
        match method {
            // Channel-family naming: .recv() replaced .next() as the fundamental
            // recv surface (routes to the layout-witness `hew_stream_next_layout`
            // entry for every describable element type).
            // .try_recv() routes to hew_stream_try_next_layout (non-blocking).
            // .lines() is an iterator-style op removed from the fundamental
            // surface; it will land via trait impls in stdlib work.
            // .collect() drains a Stream<string> into a string via
            // hew_stream_collect_string; the element-type gate above ensures
            // only string elements reach this arm.
            "recv" | "try_recv" | "close" | "collect" => {
                let Some(c_symbol) = self.require_builtin_runtime_symbol(
                    span,
                    BuiltinNamedType::Stream.canonical_name(),
                    method,
                    crate::stdlib::resolve_stream_method(
                        BuiltinNamedType::Stream.canonical_name(),
                        method,
                        Self::runtime_stream_element_name(&resolved_inner),
                    ),
                ) else {
                    return Ty::Error;
                };
                self.record_runtime_method_call_rewrite(span, c_symbol);
                sig.return_type
            }
            "chunks" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    if let Some(param_ty) = sig.params.first() {
                        self.check_against(expr, sp, param_ty);
                    }
                }
                let Some(c_symbol) = self.require_builtin_runtime_symbol(
                    span,
                    BuiltinNamedType::Stream.canonical_name(),
                    method,
                    crate::stdlib::resolve_stream_method(
                        BuiltinNamedType::Stream.canonical_name(),
                        method,
                        None,
                    ),
                ) else {
                    return Ty::Error;
                };
                self.record_runtime_method_call_rewrite(span, c_symbol);
                sig.return_type
            }
            "take" | "map" | "filter" => {
                // These lazy adapters have builtin signatures (so they
                // type-check) but no MIR lowering: they routed to the legacy
                // `DeferToLowering` codegen path the Rust MIR pipeline does not
                // consume, dead-ending in HIR lowering with two misleading,
                // internal-shaped `E_NOT_YET_IMPLEMENTED` notes. Fail closed here
                // at the checker with one honest capability-boundary diagnostic
                // so the user sees a single clear message pointing at the
                // supported alternative, and lowering never reaches the stub.
                // Still check the argument so an ill-typed adapter arg is not
                // masked by this boundary error.
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    if let Some(param_ty) = sig.params.first() {
                        self.check_against(expr, sp, param_ty);
                    }
                }
                self.report_error(
                    TypeErrorKind::StreamAdapterNotSupported {
                        method: method.to_string(),
                        element_ty: inner.user_facing().to_string(),
                    },
                    span,
                    format!(
                        "`Stream<{}>.{method}` is not yet supported: the lazy \
                         stream adapters (`take`/`map`/`filter`) have no lowering \
                         yet; consume the stream directly with `for await x in \
                         s {{ ... }}` (applying the `take`/`map`/`filter` logic in \
                         the loop body), or `.recv()` in a loop \
                         [E_STREAM_ADAPTER_UNSUPPORTED]",
                        inner.user_facing()
                    ),
                );
                Ty::Error
            }
            _ => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `Stream<{}>`", inner.user_facing()),
                    self.similar_methods(&receiver_ty, method),
                );
                Ty::Error
            }
        }
    }

    /// Type-check a method call on `Duplex<S, R>`.
    ///
    /// Wired methods:
    ///   - `.send(msg: S)` → `Result<(), SendError>`  — verifies `S: @send`.
    ///   - `.try_send(msg: S)` → `Result<(), SendError>` — non-blocking; same
    ///     Send bound as `.send()`; returns `SendError::Full` if at capacity.
    ///   - `.recv()` → `Result<R, RecvError>`.
    ///   - `.try_recv()` → `Result<R, RecvError>` — non-blocking; returns
    ///     `RecvError::Empty` if no message is waiting.
    ///   - `.send_half()` → `SendHalf<S>`  — consuming; moves the receiver.
    ///   - `.recv_half()` → `RecvHalf<R>`  — consuming; moves the receiver.
    ///   - `.close()` → `Result<(), CloseError>`  — consuming; moves the receiver.
    ///
    /// Lambda-actor handles type as `Duplex<Msg, Reply>` underneath, so this
    /// function handles both raw-duplex and lambda-actor method calls.
    ///
    /// Unknown methods fall through to a targeted `UndefinedMethod` error.
    #[allow(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "mirrors check_stream_method arity; all params are load-bearing; \
                  the match arms each encode a distinct method contract"
    )]
    pub(super) fn check_duplex_method(
        &mut self,
        type_args: &[Ty],
        receiver_ty: &Ty,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        // Extract S and R from Duplex<S, R>; fabricate fresh vars if malformed.
        let (s_ty, r_ty) = if let [s, r] = type_args {
            (s.clone(), r.clone())
        } else {
            for arg in args {
                let (expr, sp) = arg.expr();
                self.synthesize(expr, sp);
            }
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "internal error: Duplex type has wrong arity".to_string(),
            );
            return Ty::Error;
        };

        match method {
            "send" => {
                // Check the argument against S.
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let ty = self.check_against(expr, sp, &s_ty);
                    // Enforce Send bound: the payload must cross thread boundaries.
                    let resolved = self.subst.resolve(&ty);
                    self.enforce_actor_boundary_send(expr, sp, span, &resolved);
                } else {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        "Duplex.send expects one argument (the message)".to_string(),
                    );
                }
                // Synthesize extra args for recovery diagnostics.
                for arg in args.iter().skip(1) {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_send");
                // Return type depends on reply direction, mirroring call-syntax dispatch:
                //   tell-shaped (R = ())  → Result<(), SendError>
                //   ask-shaped  (R = R)   → Result<R, AskError>
                let resolved_r = self.subst.resolve(&r_ty);
                if matches!(resolved_r, Ty::Unit) {
                    Ty::result(Ty::Unit, Ty::send_error())
                } else {
                    Ty::result(resolved_r, Ty::ask_error())
                }
            }
            "try_send" => {
                // Non-blocking send: same argument and Send-bound check as
                // `.send()`, but routes to hew_duplex_try_send which returns
                // SendError::Full instead of blocking when at capacity.
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let ty = self.check_against(expr, sp, &s_ty);
                    let resolved = self.subst.resolve(&ty);
                    self.enforce_actor_boundary_send(expr, sp, span, &resolved);
                } else {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        "Duplex.try_send expects one argument (the message)".to_string(),
                    );
                }
                for arg in args.iter().skip(1) {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_try_send");
                Ty::result(Ty::Unit, Ty::send_error())
            }
            "recv" => {
                // No arguments expected.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_recv");
                let resolved_r = self.subst.resolve(&r_ty);
                Ty::result(resolved_r, Ty::recv_error())
            }
            "try_recv" => {
                // Non-blocking recv: returns RecvError::Empty instead of
                // blocking when no message is waiting.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_try_recv");
                let resolved_r = self.subst.resolve(&r_ty);
                Ty::result(resolved_r, Ty::recv_error())
            }
            "send_half" => {
                // No arguments expected.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_send_half");
                // Consuming: the Duplex<S, R> binding is moved.
                self.method_call_consumes_receiver
                    .insert(SpanKey::in_module(span, self.current_module_idx));
                let resolved_recv = self.subst.resolve(receiver_ty);
                self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                let resolved_s = self.subst.resolve(&s_ty);
                Ty::send_half(resolved_s)
            }
            "recv_half" => {
                // No arguments expected.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_recv_half");
                // Consuming: the Duplex<S, R> binding is moved.
                self.method_call_consumes_receiver
                    .insert(SpanKey::in_module(span, self.current_module_idx));
                let resolved_recv = self.subst.resolve(receiver_ty);
                self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                let resolved_r = self.subst.resolve(&r_ty);
                Ty::recv_half(resolved_r)
            }
            "close" => {
                // No arguments expected.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_close");
                // Consuming: the Duplex<S, R> binding is moved.
                self.method_call_consumes_receiver
                    .insert(SpanKey::in_module(span, self.current_module_idx));
                let resolved_recv = self.subst.resolve(receiver_ty);
                self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                Ty::result(Ty::Unit, Ty::duplex_close_error())
            }
            _ => {
                // Synthesize args for error recovery.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!(
                        "no method `{method}` on `{}`; \
                         supported methods: \
                         send / try_send / recv / try_recv / \
                         send_half / recv_half / close",
                        receiver_ty.user_facing()
                    ),
                );
                Ty::Error
            }
        }
    }

    /// Type-check a method call on `LambdaPid<M, R>` — the lambda-actor handle.
    ///
    /// Wired methods (the actor surface, NOT the channel surface):
    ///   - `.send(msg: M)` → `Result<(), SendError>` (tell-shaped, R = ()) or
    ///     `Result<R, AskError>` (ask-shaped). Verifies `M: @send`. Secondary
    ///     surface to the canonical call-syntax `handle(msg)`; both route
    ///     through `Place::LambdaActorHandle` to `hew_lambda_actor_send` at MIR.
    ///   - `.close()` → `()` — consuming; moves the handle. Deliberately returns
    ///     plain `()` rather than `Result<(), CloseError>` (unlike `Duplex::close`):
    ///     the lambda-actor release is unconditionally successful, and the
    ///     `CloseError` layout is not yet codegen-able. Lowers to
    ///     `hew_lambda_actor_release` via the `Place::LambdaActorHandle` drop
    ///     discriminator.
    ///
    /// `.recv()` / `.try_recv()` / `.try_send()` / `.send_half()` / `.recv_half()`
    /// are NOT a lambda-actor surface: a lambda actor is not a channel. The caller
    /// never reads the actor's mailbox, and an actor handle cannot be split in two.
    /// These names are rejected with a targeted `UndefinedMethod` diagnostic.
    pub(super) fn check_lambda_pid_method(
        &mut self,
        type_args: &[Ty],
        receiver_ty: &Ty,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        // Extract M and R from LambdaPid<M, R>; fabricate fresh vars if malformed.
        let (m_ty, r_ty) = if let [m, r] = type_args {
            (m.clone(), r.clone())
        } else {
            for arg in args {
                let (expr, sp) = arg.expr();
                self.synthesize(expr, sp);
            }
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "internal error: LambdaPid type has wrong arity".to_string(),
            );
            return Ty::Error;
        };

        match method {
            "send" => {
                if args.len() != 1 {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "LambdaPid.send expects one argument (the message), but {} were supplied",
                            args.len()
                        ),
                    );
                }
                // Check the argument against M (the message type) when present so
                // the caller still gets the most specific message-type diagnostic
                // alongside any arity error.
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let ty = self.check_against(expr, sp, &m_ty);
                    // Enforce Send bound: the message crosses the actor boundary.
                    let resolved = self.subst.resolve(&ty);
                    self.enforce_actor_boundary_send(expr, sp, span, &resolved);
                }
                // Synthesize extra args for recovery diagnostics, but do not accept
                // them: MIR only lowers the receiver plus the first message arg.
                for arg in args.iter().skip(1) {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                // Records the duplex-send entry hint; MIR's `lower_duplex_send`
                // re-routes by `Place::LambdaActorHandle` to `hew_lambda_actor_send`
                // (the two-level checker-type vs MIR-discriminator design).
                self.record_runtime_method_call_rewrite(span, "hew_duplex_send");
                // Return type depends on reply direction, mirroring call-syntax dispatch:
                //   tell-shaped (R = ())  → Result<(), SendError>
                //   ask-shaped  (R = R)   → Result<R, AskError>
                let resolved_r = self.subst.resolve(&r_ty);
                if matches!(resolved_r, Ty::Unit) {
                    Ty::result(Ty::Unit, Ty::send_error())
                } else {
                    Ty::result(resolved_r, Ty::ask_error())
                }
            }
            "close" => {
                // No arguments expected. Synthesize any supplied args for
                // recovery diagnostics, but do not accept them: MIR lowers only
                // the receiver for LambdaPid::close.
                if !args.is_empty() {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "LambdaPid.close expects no arguments, but {} were supplied",
                            args.len()
                        ),
                    );
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                // Records the duplex-close rewrite symbol.  MIR's
                // `lower_duplex_close` routes by the receiver's `Place` variant:
                //   - `Place::LambdaActorHandle` → `hew_lambda_actor_release`
                //     (the lambda stop-on-last-drop ritual).
                //   - anything else → raw `Duplex` close (not yet lowered).
                // Mirrors `.send`'s two-level routing: checker records one symbol;
                // MIR selects the real ABI from the Place discriminator.
                self.record_runtime_method_call_rewrite(span, "hew_duplex_close");
                // Consuming: the LambdaPid<M, R> binding is moved.
                self.method_call_consumes_receiver
                    .insert(SpanKey::in_module(span, self.current_module_idx));
                let resolved_recv = self.subst.resolve(receiver_ty);
                self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                // Returns `()` — the lambda-actor release is unconditionally
                // successful (the runtime refcount decrement / stop signal never
                // fails for a well-formed handle). Using `Unit` rather than
                // `Result<(), CloseError>` avoids registering the `CloseError`
                // enum instantiation in HIR, which would require a codegen layout
                // for the `CloseError` payload that the pipeline does not yet have.
                // A raw `Duplex::close()` keeps `Result<(), CloseError>` because
                // its close CAN fail (I/O flush errors on streams and connections).
                Ty::Unit
            }
            _ => {
                // Synthesize args for error recovery.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!(
                        "no method `{method}` on `{}`; \
                         a lambda actor is not a channel — supported methods: \
                         send / close (the canonical call surface is `handle(msg)`)",
                        receiver_ty.user_facing()
                    ),
                );
                Ty::Error
            }
        }
    }

    /// Type-check a method call on `SendHalf<S>`.
    ///
    /// Wired methods:
    ///   - `.send(msg: S)` → `Result<(), SendError>`  — verifies `S: @send`.
    ///   - `.try_send(msg: S)` → `Result<(), SendError>` — non-blocking variant;
    ///     returns `SendError::Full` if at capacity.
    ///   - `.close()` → `Result<(), CloseError>`  — consuming; moves the receiver.
    ///
    /// `.recv()` / `.try_recv()` are rejected with targeted `UndefinedMethod` diagnostics.
    pub(super) fn check_send_half_method(
        &mut self,
        type_args: &[Ty],
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let s_ty = type_args
            .first()
            .cloned()
            .unwrap_or_else(|| Ty::Var(TypeVar::fresh()));

        let receiver_ty = Ty::send_half(self.subst.resolve(&s_ty));

        match method {
            "send" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let ty = self.check_against(expr, sp, &s_ty);
                    let resolved = self.subst.resolve(&ty);
                    self.enforce_actor_boundary_send(expr, sp, span, &resolved);
                } else {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        "SendHalf::send expects one argument (the message)".to_string(),
                    );
                }
                for arg in args.iter().skip(1) {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_send_half_send");
                Ty::result(Ty::Unit, Ty::send_error())
            }
            "try_send" => {
                // Non-blocking: same Send bound as .send(); routes to
                // hew_send_half_try_send which returns SendError::Full at capacity.
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let ty = self.check_against(expr, sp, &s_ty);
                    let resolved = self.subst.resolve(&ty);
                    self.enforce_actor_boundary_send(expr, sp, span, &resolved);
                } else {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        "SendHalf::try_send expects one argument (the message)".to_string(),
                    );
                }
                for arg in args.iter().skip(1) {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_send_half_try_send");
                Ty::result(Ty::Unit, Ty::send_error())
            }
            "close" => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_close_half");
                // Consuming: the SendHalf<S> binding is moved.
                self.method_call_consumes_receiver
                    .insert(SpanKey::in_module(span, self.current_module_idx));
                let resolved_recv = self.subst.resolve(&receiver_ty);
                self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                Ty::result(Ty::Unit, Ty::duplex_close_error())
            }
            _ => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!(
                        "no method `{method}` on `{}`; \
                         `SendHalf` only supports `.send()`, `.try_send()`, and `.close()`",
                        receiver_ty.user_facing()
                    ),
                );
                Ty::Error
            }
        }
    }

    /// Type-check a method call on `RecvHalf<R>`.
    ///
    /// Wired methods:
    ///   - `.recv()` → `Result<R, RecvError>`.
    ///   - `.try_recv()` → `Result<R, RecvError>` — non-blocking; returns
    ///     `RecvError::Empty` if no message is waiting.
    ///   - `.close()` → `Result<(), CloseError>`  — consuming; moves the receiver.
    ///
    /// `.send()` / `.try_send()` are rejected with targeted `UndefinedMethod` diagnostics.
    pub(super) fn check_recv_half_method(
        &mut self,
        type_args: &[Ty],
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let r_ty = type_args
            .first()
            .cloned()
            .unwrap_or_else(|| Ty::Var(TypeVar::fresh()));

        let receiver_ty = Ty::recv_half(self.subst.resolve(&r_ty));

        match method {
            "recv" => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_recv_half_recv");
                let resolved_r = self.subst.resolve(&r_ty);
                Ty::result(resolved_r, Ty::recv_error())
            }
            "try_recv" => {
                // Non-blocking: returns RecvError::Empty instead of blocking
                // when no message is waiting.
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_recv_half_try_recv");
                let resolved_r = self.subst.resolve(&r_ty);
                Ty::result(resolved_r, Ty::recv_error())
            }
            "close" => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.record_runtime_method_call_rewrite(span, "hew_duplex_close_half");
                // Consuming: the RecvHalf<R> binding is moved.
                self.method_call_consumes_receiver
                    .insert(SpanKey::in_module(span, self.current_module_idx));
                let resolved_recv = self.subst.resolve(&receiver_ty);
                self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                Ty::result(Ty::Unit, Ty::duplex_close_error())
            }
            _ => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!(
                        "no method `{method}` on `{}`; \
                         `RecvHalf` only supports `.recv()`, `.try_recv()`, and `.close()`",
                        receiver_ty.user_facing()
                    ),
                );
                Ty::Error
            }
        }
    }

    /// Resolve a method call on `Ty::String` through the declarative
    /// `impl string` block declared in `std/string.hew`. Anything else —
    /// including user `impl MyTrait for string` dispatch — falls through to
    /// primitive-trait lookup so primitive-trait-impl metadata continues to be
    /// recorded for codegen.
    pub(super) fn dispatch_string_method(
        &mut self,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        if let Some(ret_ty) =
            self.dispatch_monomorphic_extern_symbol_method("string", &[], method, args, span)
        {
            return ret_ty;
        }
        self.check_primitive_receiver_method_fallback(&Ty::String, "string", method, args, span)
    }

    fn check_hashset_element_arg(&mut self, elem_ty: &Ty, arg: &CallArg) -> bool {
        let (expr, sp) = arg.expr();
        let err_before = self.errors.len();
        let actual = self.check_against(expr, sp, elem_ty);
        if self.errors.len() > err_before || matches!(actual, Ty::Error) {
            return false;
        }

        let err_before = self.errors.len();
        self.expect_type(elem_ty, &actual, sp);
        self.errors.len() == err_before
    }

    fn dispatch_primitive_pattern_name(ty: &Ty) -> Option<&'static str> {
        Some(match ty {
            Ty::I8 => "i8",
            Ty::I16 => "i16",
            Ty::I32 | Ty::IntLiteral => "i32",
            Ty::I64 => "i64",
            Ty::U8 => "u8",
            Ty::U16 => "u16",
            Ty::U32 => "u32",
            Ty::U64 => "u64",
            Ty::Isize => "isize",
            Ty::Usize => "usize",
            Ty::F32 => "f32",
            Ty::F64 | Ty::FloatLiteral => "f64",
            Ty::Bool => "bool",
            Ty::Char => "char",
            Ty::String => "String",
            Ty::Bytes => "bytes",
            Ty::Duration => "duration",
            Ty::Unit => "()",
            Ty::Never => "!",
            Ty::CancellationToken => "CancellationToken",
            Ty::Var(_)
            | Ty::Tuple(_)
            | Ty::Array(_, _)
            | Ty::Slice(_)
            | Ty::Named { .. }
            | Ty::Function { .. }
            | Ty::Closure { .. }
            | Ty::Pointer { .. }
            | Ty::Borrow { .. }
            | Ty::TraitObject { .. }
            | Ty::Task(_)
            | Ty::AssocType { .. }
            | Ty::Error => return None,
        })
    }

    fn ty_to_dispatch_pattern(&self, ty: &Ty) -> TyPattern {
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        if let Some(name) = Self::dispatch_primitive_pattern_name(&resolved) {
            return TyPattern::Primitive(name.to_string());
        }
        match resolved {
            Ty::Tuple(items) => TyPattern::Tuple(
                items
                    .iter()
                    .map(|item| self.ty_to_dispatch_pattern(item))
                    .collect(),
            ),
            Ty::Named { name, args, .. } => {
                if args.is_empty() {
                    TyPattern::Primitive(name)
                } else {
                    TyPattern::App {
                        ctor: name,
                        args: args
                            .iter()
                            .map(|arg| self.ty_to_dispatch_pattern(arg))
                            .collect(),
                    }
                }
            }
            other => TyPattern::Primitive(other.user_facing().to_string()),
        }
    }

    fn dispatch_pattern_to_ty(pattern: &TyPattern) -> Ty {
        match pattern {
            TyPattern::Primitive(name) => match name.as_str() {
                "i8" => Ty::I8,
                "i16" => Ty::I16,
                "i32" => Ty::I32,
                "i64" => Ty::I64,
                "u8" => Ty::U8,
                "u16" => Ty::U16,
                "u32" => Ty::U32,
                "u64" => Ty::U64,
                "isize" => Ty::Isize,
                "usize" => Ty::Usize,
                "f32" => Ty::F32,
                "f64" => Ty::F64,
                "bool" => Ty::Bool,
                "char" => Ty::Char,
                "String" => Ty::String,
                "bytes" => Ty::Bytes,
                "duration" => Ty::Duration,
                "()" => Ty::Unit,
                "!" => Ty::Never,
                other => Ty::Named {
                    builtin: None,
                    name: other.to_string(),
                    args: vec![],
                },
            },
            TyPattern::App { ctor, args } => Ty::Named {
                builtin: crate::lookup_builtin_type(ctor),
                name: ctor.clone(),
                args: args.iter().map(Self::dispatch_pattern_to_ty).collect(),
            },
            TyPattern::Tuple(items) => {
                Ty::Tuple(items.iter().map(Self::dispatch_pattern_to_ty).collect())
            }
            TyPattern::Var(name) => Ty::Named {
                builtin: None,
                name: name.clone(),
                args: vec![],
            },
        }
    }

    fn record_resolved_collection_call(
        &mut self,
        trait_name: &str,
        method: &str,
        receiver: &TyPattern,
        span: &Span,
    ) {
        // W4.001 Stage C3 (DI-017): the Stage-B `collection_dispatch_registry`
        // wrapper has retired; call the impl directly. Authority for HashMap /
        // HashSet method dispatch is now the resolver, with the result emitted
        // via `resolved_calls` (no parallel `method_call_rewrites` entry).
        let registry = collection_dispatch_registry_impl();
        let resolved =
            resolve_method_call(&registry, trait_name, method, receiver, &|marker, ty| {
                let ty = Self::dispatch_pattern_to_ty(ty);
                self.registry.implements_marker(&ty, marker)
            });
        match resolved {
            Ok(call) => {
                self.resolved_calls
                    .insert(SpanKey::in_module(span, self.current_module_idx), call);
            }
            Err(LookupError::BoundsNotSatisfied {
                unsatisfied,
                witness,
                ..
            }) => {
                // W4.001 Stage C3 hard cutover: the resolver is now the
                // sole admission authority for HashMap/HashSet dispatch.
                // An unsatisfied where-bound (e.g. `K: Hash` failing on
                // `f64`) becomes a user-facing `BoundsNotSatisfied`
                // diagnostic with attribution to the witness type.
                // `MethodCallNoRewrite` is permanently demoted to a
                // boundary-violation-only diagnostic.
                let witness_ty = Self::dispatch_pattern_to_ty(&witness);
                let bound_summary = unsatisfied
                    .iter()
                    .map(|b| format!("{}: {}", b.var, b.trait_name))
                    .collect::<Vec<_>>()
                    .join(", ");
                self.report_error(
                    TypeErrorKind::BoundsNotSatisfied,
                    span,
                    format!(
                        "`{}` does not satisfy the required bounds for \
                         `{trait_name}.{method}` ({bound_summary})",
                        witness_ty.user_facing()
                    ),
                );
            }
            Err(LookupError::NoImpl { .. } | LookupError::UnknownMethod { .. }) => {
                // Unrecognised receiver shape or method — should not occur
                // because callers gate by ctor/method names matching the
                // registry. Emit a fail-closed `InvalidOperation` so any
                // future drift surfaces loudly rather than silently
                // skipping `resolved_calls` population.
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "internal compiler error: collection resolver could \
                         not locate `{trait_name}.{method}` for receiver \
                         `{receiver:?}`"
                    ),
                );
            }
        }
    }

    pub(super) fn record_resolved_hashmap_call(
        &mut self,
        method: &str,
        key_ty: &Ty,
        val_ty: &Ty,
        span: &Span,
    ) {
        let receiver = TyPattern::App {
            ctor: "HashMap".to_string(),
            args: vec![
                self.ty_to_dispatch_pattern(key_ty),
                self.ty_to_dispatch_pattern(val_ty),
            ],
        };
        if !self.is_hashmap_abstract_key_param(key_ty) {
            self.record_resolved_collection_call("Map", method, &receiver, span);
            return;
        }
        let key_param_name = self
            .hashmap_abstract_key_param_name(key_ty)
            .expect("abstract HashMap key param was checked above");

        let key_pattern = self.ty_to_dispatch_pattern(key_ty);
        let registry = collection_dispatch_registry_impl();
        let resolved = resolve_method_call(&registry, "Map", method, &receiver, &|marker, ty| {
            if *ty == key_pattern {
                return self.type_param_has_marker_bound(&key_param_name, marker);
            }
            let ty = Self::dispatch_pattern_to_ty(ty);
            self.registry.implements_marker(&ty, marker)
        });
        match resolved {
            Ok(call) => {
                self.resolved_calls
                    .insert(SpanKey::in_module(span, self.current_module_idx), call);
            }
            Err(LookupError::BoundsNotSatisfied {
                unsatisfied,
                witness,
                ..
            }) => {
                let witness_ty = Self::dispatch_pattern_to_ty(&witness);
                let bound_summary = unsatisfied
                    .iter()
                    .map(|b| format!("{}: {}", b.var, b.trait_name))
                    .collect::<Vec<_>>()
                    .join(", ");
                self.report_error(
                    TypeErrorKind::BoundsNotSatisfied,
                    span,
                    format!(
                        "`{}` does not satisfy the required bounds for \
                         `Map.{method}` ({bound_summary})",
                        witness_ty.user_facing()
                    ),
                );
            }
            Err(LookupError::NoImpl { .. } | LookupError::UnknownMethod { .. }) => {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "internal compiler error: collection resolver could \
                         not locate `Map.{method}` for receiver `{receiver:?}`"
                    ),
                );
            }
        }
    }

    fn hashmap_abstract_key_param_name(&self, key_ty: &Ty) -> Option<String> {
        match self.subst.resolve(key_ty).materialize_literal_defaults() {
            Ty::Named {
                name,
                args,
                builtin: None,
            } if args.is_empty() && self.is_type_param_in_scope(&name) => Some(name),
            _ => None,
        }
    }

    /// The single template-capability authority for `clone`.
    ///
    /// Returns `None` when `ty` is not a bare in-scope type parameter (the
    /// caller then continues its structural walk). Returns `Some(true)` when
    /// the parameter's declared bounds grant `Clone`, `Some(false)` when they
    /// do not.
    ///
    /// Inside a generic template there is no concrete type to interrogate, so
    /// the bound *is* the capability. Instantiation then decides the concrete
    /// capability: `enforce_type_param_bounds` refuses a type argument that
    /// does not satisfy `T: Clone`, which is what keeps affine resources
    /// non-clonable through a generic seam.
    pub(super) fn type_param_template_clone_capability(&self, ty: &Ty) -> Option<bool> {
        let Ty::Named {
            name,
            args,
            builtin: None,
        } = ty
        else {
            return None;
        };
        if !args.is_empty() || !self.is_type_param_in_scope(name) {
            return None;
        }
        Some(self.type_param_has_marker_bound(name, MarkerTrait::Clone))
    }

    pub(super) fn type_param_has_marker_bound(
        &self,
        param_name: &str,
        marker: MarkerTrait,
    ) -> bool {
        let marker_name = marker.to_string();
        for frame in self.current_type_param_bounds.iter().rev() {
            if let Some(bounds) = frame.bounds.get(param_name) {
                return bounds.iter().any(|bound| bound == &marker_name);
            }
        }
        if let Some(fn_name) = self.current_function.as_ref() {
            if let Some(sig) = self.fn_sigs.get(fn_name) {
                if sig.type_params.iter().any(|param| param == param_name) {
                    return sig
                        .type_param_bounds
                        .get(param_name)
                        .is_some_and(|bounds| bounds.iter().any(|bound| bound == &marker_name));
                }
            }
        }
        false
    }

    fn is_hashmap_abstract_key_param(&self, key_ty: &Ty) -> bool {
        self.hashmap_abstract_key_param_name(key_ty).is_some()
    }

    pub(super) fn record_resolved_hashset_call(&mut self, method: &str, elem_ty: &Ty, span: &Span) {
        let receiver = TyPattern::App {
            ctor: "HashSet".to_string(),
            args: vec![self.ty_to_dispatch_pattern(elem_ty)],
        };
        self.record_resolved_collection_call("Set", method, &receiver, span);
    }

    /// Record one Vec resolved call, selecting its symbol through the shared
    /// source-derived Vec authority. Abstract element methods retain the
    /// registry's `_FAMILY` placeholder for MIR monomorphisation.
    pub(super) fn record_resolved_vec_call(&mut self, method: &str, elem_ty: &Ty, span: &Span) {
        let Some(vec_method) = VecMethod::from_name(method) else {
            return;
        };
        let elem_ty = self.subst.resolve(elem_ty).materialize_literal_defaults();
        let receiver = TyPattern::App {
            ctor: "Vec".to_string(),
            args: vec![self.ty_to_dispatch_pattern(&elem_ty)],
        };
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.record_resolved_collection_call("Seq", method, &receiver, span);
        if !self.resolved_calls.contains_key(&key) {
            return;
        }

        let is_abstract = self.vec_element_contains_abstract_type_param(&elem_ty);
        let is_copy_layout = self.vec_element_has_copy_layout(&elem_ty);
        let profile = crate::vec_authority::VecElementProfile {
            abi: crate::vec_authority::classify_element(&elem_ty, &self.type_defs),
            is_owned: !is_copy_layout && self.vec_owned_element_admissible(&elem_ty),
            is_copy_layout,
            is_function_like: matches!(elem_ty, Ty::Function { .. } | Ty::Closure { .. }),
            is_abstract,
        };
        match crate::vec_authority::resolve_runtime_symbol(
            vec_method,
            profile,
            crate::vec_authority::VecResolutionContext::CheckerConcrete,
        ) {
            crate::vec_authority::VecSymbolResolution::Resolved(symbol_name) => {
                self.resolved_calls
                    .get_mut(&key)
                    .expect("collection resolver inserted Vec call before symbol override")
                    .method_target
                    .symbol_name = symbol_name;
            }
            crate::vec_authority::VecSymbolResolution::Deferred => {}
            crate::vec_authority::VecSymbolResolution::Unavailable => {
                self.resolved_calls.remove(&key);
            }
            crate::vec_authority::VecSymbolResolution::Unsupported(reason) => {
                self.report_vec_symbol_unsupported(vec_method, &elem_ty, reason, span);
                self.resolved_calls.remove(&key);
            }
        }
    }

    fn report_vec_symbol_unsupported(
        &mut self,
        method: VecMethod,
        elem_ty: &Ty,
        reason: crate::vec_authority::VecUnsupported,
        span: &Span,
    ) {
        let message = match reason {
            crate::vec_authority::VecUnsupported::FunctionGet => {
                "`Vec.get` on a function/closure element is not supported under \
                 the `Option<T>` accessor model: the element owns a heap-boxed \
                 closure environment that the fresh-owner get choke point does \
                 not yet clone (tracked gap)"
                    .to_string()
            }
            crate::vec_authority::VecUnsupported::FunctionSharedCopy => format!(
                "`Vec.{}` is not supported for function-valued elements: each element \
                 owns its closure environment, and a shallow buffer copy would create \
                 two owners of one environment",
                method.name()
            ),
            crate::vec_authority::VecUnsupported::Layout {
                expected_symbol,
                bitcopy_supported,
            } => {
                if bitcopy_supported {
                    let why = self.vec_element_rejection_reason(elem_ty);
                    format!(
                        "`{}` cannot be a `Vec` element for `Vec.{}`: {why} \
                         (runtime symbol `{expected_symbol}`)",
                        elem_ty.user_facing(),
                        method.name()
                    )
                } else {
                    format!(
                        "`Vec.{}` on layout-backed element type `{}` is not \
                         runtime-backed yet (runtime symbol `{expected_symbol}`); supported \
                         layout Vec methods are push/get/set/pop/remove/clone for Copy \
                         record/tuple elements",
                        method.name(),
                        elem_ty.user_facing()
                    )
                }
            }
        };
        self.report_error(TypeErrorKind::InvalidOperation, span, message);
    }

    /// True when `elem_ty` transitively references an in-scope type parameter —
    /// a bare `T`, or a composite that *contains* one (`W<T>`, `Option<T>`,
    /// `(T, i64)`, `Vec<T>`). Such an element cannot be classified to a concrete
    /// runtime symbol at check time: its owned-vs-plain / Copy-vs-heap verdict
    /// depends on the monomorphised argument, so eager resolution on the generic
    /// spine can pick a different ABI than the constructor codegen stamps for the
    /// concrete instantiation (the #2737 clone-thunk divergence — `W<T>`
    /// classified owned generically vs the plain all-scalar `W<i64>`). Marking it
    /// abstract routes the method through the per-monomorphisation re-resolver
    /// (MIR `resolve_polymorphic_vec_element_symbol`), which classifies the
    /// substituted element and stays congruent with the constructor by
    /// construction (`dedup-semantic-boundary`).
    pub(super) fn vec_element_contains_abstract_type_param(&self, elem_ty: &Ty) -> bool {
        match elem_ty {
            Ty::Named {
                name,
                args,
                builtin,
            } => {
                (builtin.is_none() && args.is_empty() && self.is_type_param_in_scope(name))
                    || args
                        .iter()
                        .any(|a| self.vec_element_contains_abstract_type_param(a))
            }
            Ty::Tuple(elems) => elems
                .iter()
                .any(|e| self.vec_element_contains_abstract_type_param(e)),
            Ty::Array(inner, _) | Ty::Slice(inner) => {
                self.vec_element_contains_abstract_type_param(inner)
            }
            _ => false,
        }
    }

    /// Build the `Ty → VecElementToken` verdict table that MIR consults when
    /// re-resolving a `Vec<T>` element-typed method under a type parameter
    /// (#1929 Stage 1). Every concrete type observed as a generic call /
    /// record-init type-argument is classified through
    /// [`Self::classify_vec_generic_element`]; types absent from the result
    /// (non-`Copy`/owned layout, owned heap-handles, closures, nested
    /// collections, unresolved nominals) fail closed downstream.
    pub(super) fn build_vec_generic_element_abi(
        &self,
        call_type_args: &HashMap<SpanKey, Vec<Ty>>,
        record_init_type_args: &HashMap<SpanKey, Vec<Ty>>,
        type_defs: &HashMap<String, TypeDef>,
    ) -> HashMap<Ty, crate::vec_authority::VecElementToken> {
        let mut out = HashMap::new();
        for args in call_type_args
            .values()
            .chain(record_init_type_args.values())
        {
            for ty in args {
                if out.contains_key(ty) {
                    continue;
                }
                if let Some(token) = self.classify_vec_generic_element(ty, type_defs) {
                    out.insert(ty.clone(), token);
                }
            }
        }
        out
    }

    /// Classify a concrete element type's `Vec<T>` runtime ABI for the
    /// monomorphisation re-resolution path, using
    /// [`crate::vec_authority::classify_element`].
    ///
    /// Stage 1 admits exactly what already round-trips on the concrete path
    /// without new runtime/codegen machinery: scalar (`bool`/`i32`/`i64`/`f64`)
    /// and `string` elements unconditionally, and pointer / layout-descriptor
    /// elements **only when the element is `Copy`**. The `Copy` gate is what
    /// makes the pointer and layout arms safe: it admits identity handles
    /// (`LocalPid`) and bit-copy value records, while deferring
    /// every shape with an ownership contract — owned heap-handles, non-`Copy`
    /// records, closures (each owns a captured environment), and nested
    /// collections (each owns a backing store). Those would alias an owner
    /// across a shallow `_ptr`/`_layout` op and double-free; they stay
    /// fail-closed until the owned generic path lands.
    fn classify_vec_generic_element(
        &self,
        ty: &Ty,
        type_defs: &HashMap<String, TypeDef>,
    ) -> Option<crate::vec_authority::VecElementToken> {
        use crate::vec_authority::VecElementToken;
        let token = crate::vec_authority::classify_element(ty, type_defs)?;
        let admissible = match token {
            // Bit-copy scalars and the CoW `string` representation carry no
            // owner-aliasing hazard across the shared-buffer element ops. Every
            // integer width and both float widths route to a dedicated
            // runtime kernel, so the whole scalar set is unconditionally
            // admissible.
            VecElementToken::Bool
            | VecElementToken::I8
            | VecElementToken::U8
            | VecElementToken::I16
            | VecElementToken::U16
            | VecElementToken::I32
            | VecElementToken::I64
            | VecElementToken::F32
            | VecElementToken::F64
            | VecElementToken::Str => true,
            // Pointer-identity and layout-descriptor elements are admitted
            // only when `Copy` — see the doc comment for why.
            VecElementToken::Ptr | VecElementToken::Layout => {
                self.registry.implements_marker(ty, MarkerTrait::Copy)
            }
        };
        admissible.then_some(token)
    }

    /// Fail-closed gate for Vec pipeline methods whose elements cannot be
    /// copied into a second owner. Function slots own their closure-pair box;
    /// trait-object slots own their concrete `HeapBoxed` value. The pipeline
    /// desugar reads elements without consuming the source Vec, so neither can
    /// safely manufacture a result owner.
    /// Returns `true` when the call was rejected.
    fn reject_vec_pipeline_fn_element(&mut self, method: &str, elem_ty: &Ty, span: &Span) -> bool {
        if matches!(elem_ty, Ty::TraitObject { .. }) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`Vec.{method}` is not supported for trait-object elements: \
                     the source Vec retains each HeapBoxed owner and `dyn Trait` has no \
                     semantic clone operation; consume the Vec with `into_iter()` instead"
                ),
            );
            return true;
        }
        if matches!(elem_ty, Ty::Function { .. } | Ty::Closure { .. }) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`Vec.{method}` is not supported for function-valued elements: \
                     each element owns its closure environment, and reading elements \
                     into a pipeline result would create a second owner of one \
                     environment"
                ),
            );
            return true;
        }
        false
    }

    /// Record the [`MethodCallRewrite::BuiltinVecHigherOrder`] entry that
    /// drives the HIR pipeline-loop expansion. Skipped (fail-closed: the call
    /// then dies at HIR with `MethodCallNoRewrite`) when either type fails
    /// boundary conversion — an unresolved inference hole here means the call
    /// site itself already carries a type diagnostic.
    fn record_vec_higher_order_rewrite(
        &mut self,
        op: VecHigherOrderOp,
        elem_ty: &Ty,
        out_ty: &Ty,
        span: &Span,
    ) {
        // A place receiver must remain usable after the eager pipeline.
        // Pre-author the zero-width clone call that HIR inserts to give its
        // eval-once source binding an independent Vec owner. This is the same
        // element-aware clone authority used by `Vec.iter()`; non-place
        // receivers do not consume the unused side-table row.
        let clone_span = span.start..span.start;
        let vec_ty = self.make_vec_type(elem_ty.clone(), &clone_span);
        self.record_type(&clone_span, &vec_ty);
        self.record_resolved_vec_call("clone", elem_ty, &clone_span);
        let elem = ResolvedTy::from_ty(&elem_ty.clone().materialize_literal_defaults());
        let out = ResolvedTy::from_ty(&out_ty.clone().materialize_literal_defaults());
        if let (Ok(elem_ty), Ok(out_ty)) = (elem, out) {
            self.record_method_call_rewrite(
                span,
                MethodCallRewrite::BuiltinVecHigherOrder {
                    op,
                    elem_ty,
                    out_ty,
                },
            );
        }
    }

    /// Instantiate an [`ArgTemplate`] against the receiver's concrete types.
    fn collection_arg_ty(template: ArgTemplate, cx: &CollectionTyCx) -> Ty {
        match template {
            ArgTemplate::Key => cx.key.clone(),
            ArgTemplate::Value => cx.val.clone(),
            ArgTemplate::Elem => cx.elem.clone(),
        }
    }

    /// Shared argument *walk*: check each supplied argument against its template
    /// type.  Returns `false` only when a per-collection arg hook signals an
    /// early `Ty::Error` (today: `HashSet`'s `check_hashset_element_arg`
    /// coercion).  Missing trailing arguments are skipped — the arity check (if
    /// any) is the sole authority for argument-count diagnostics, preserving the
    /// historical `if let Some(arg) = args.first()` behaviour.
    fn check_collection_args(
        &mut self,
        kind: CollectionKind,
        templates: &[ArgTemplate],
        cx: &CollectionTyCx,
        args: &[CallArg],
        span: &Span,
    ) -> bool {
        let _ = span;
        for (i, template) in templates.iter().enumerate() {
            let Some(arg) = args.get(i) else {
                continue;
            };
            // HashSet element arguments go through the coercion hook (returns a
            // bool, early-returns `Ty::Error`) rather than a bare `check_against`.
            if kind == CollectionKind::HashSet && matches!(template, ArgTemplate::Elem) {
                if !self.check_hashset_element_arg(&cx.elem, arg) {
                    return false;
                }
                continue;
            }
            let expected = Self::collection_arg_ty(*template, cx);
            let (expr, sp) = arg.expr();
            self.check_against(expr, sp, &expected);
        }
        true
    }

    /// Returns `true` when `ty` is a concrete signed integer type strictly
    /// narrower than `i64` (i.e. `i8`, `i16`, or `i32`).  Used as the guard
    /// for implicit index-site widening — we do NOT widen unsigned types,
    /// float literals, or `IntLiteral` (integer literals are already accepted
    /// by the `check_against` literal-coercion arm).
    pub(super) fn is_narrower_signed_int(ty: &Ty) -> bool {
        matches!(ty, Ty::I8 | Ty::I16 | Ty::I32)
    }

    /// Construct a collection method's return type from its [`RetTemplate`].
    ///
    /// `VecOfKey`/`VecOfVal` route through `make_vec_type`, which itself
    /// validates the synthesized element type — this MUST run after the
    /// per-collection element validation hook (it does, because the driver calls
    /// this last), preserving the historical ordering of the `HashMap`
    /// `keys`/`values` arms.
    fn collection_ret(
        &mut self,
        kind: CollectionKind,
        ret: RetTemplate,
        cx: &CollectionTyCx,
        span: &Span,
    ) -> Ty {
        match ret {
            RetTemplate::Unit => Ty::Unit,
            RetTemplate::Bool => Ty::Bool,
            RetTemplate::I64 => Ty::I64,
            RetTemplate::VecOfKey => self.make_vec_type(cx.key.clone(), span),
            RetTemplate::VecOfVal => self.make_vec_type(cx.val.clone(), span),
            RetTemplate::VecOfPair => {
                self.make_vec_type(Ty::Tuple(vec![cx.key.clone(), cx.val.clone()]), span)
            }
            RetTemplate::SelfTy => match kind {
                CollectionKind::HashMap => Ty::Named {
                    builtin: Some(BuiltinType::HashMap),
                    name: "HashMap".to_string(),
                    args: vec![cx.key.clone(), cx.val.clone()],
                },
                CollectionKind::HashSet => Ty::Named {
                    builtin: Some(BuiltinType::HashSet),
                    name: "HashSet".to_string(),
                    args: vec![cx.elem.clone()],
                },
            },
        }
    }

    /// Code-side hook dispatch for the genuinely divergent per-collection
    /// admission policy: element validation, the `HashSet` lowering fact, and
    /// `ResolvedCall` recording.  This is the "do not centralise the decision"
    /// half of the refactor — the validators/recorders stay as separate
    /// functions; this only selects which named hook each `(kind, method)` runs,
    /// in one place instead of three mirrored resolvers.
    ///
    /// Returns `false` (→ caller emits `Ty::Error`) when an element validator
    /// rejects the call.  The Vec `reject_rc` hook deliberately does NOT
    /// short-circuit (matching the historical fire-and-continue behaviour).
    fn run_collection_admission(
        &mut self,
        kind: CollectionKind,
        method: &str,
        cx: &CollectionTyCx,
        span: &Span,
    ) -> bool {
        match kind {
            CollectionKind::HashMap => {
                // Owned-vs-key_value validator split (deliberate per-arm asymmetry).
                let validated = match method {
                    "insert" | "get" | "remove" => {
                        self.validate_hashmap_owned_element_types(&cx.key, &cx.val, span)
                    }
                    "keys" | "values" | "entries" => self
                        .validate_hashmap_projection_element_types(&cx.key, &cx.val, method, span),
                    _ => self.validate_hashmap_key_value_types(&cx.key, &cx.val, span),
                };
                if !validated {
                    return false;
                }
                if matches!(
                    method,
                    "insert"
                        | "get"
                        | "remove"
                        | "contains_key"
                        | "len"
                        | "keys"
                        | "values"
                        | "entries"
                        | "clone"
                        | "clear"
                ) {
                    self.record_resolved_hashmap_call(method, &cx.key, &cx.val, span);
                }
            }
            CollectionKind::HashSet => {
                // Owned (insert) vs plain (rest) validator split.
                let validated = match method {
                    "insert" => self.validate_hashset_owned_element_type(&cx.elem, span),
                    _ => self.validate_hashset_element_type(&cx.elem, span),
                };
                if !validated {
                    return false;
                }
                // Every known HashSet arm records a lowering fact (HashMap/Vec
                // do not) — a genuine per-collection hook.
                self.record_hashset_lowering_fact(span, &cx.elem);
                if matches!(
                    method,
                    "insert" | "contains" | "remove" | "len" | "is_empty" | "clone" | "clear"
                ) {
                    self.record_resolved_hashset_call(method, &cx.elem, span);
                }
            }
        }
        true
    }

    /// Fail-closed fallback for an unknown collection method: try a user
    /// `impl Trait for <collection>` body, then synthesize the arguments and
    /// emit the per-collection `no method `{m}` on {Collection}` diagnostic.
    fn collection_method_fallback(
        &mut self,
        kind: CollectionKind,
        cx: &CollectionTyCx,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        // Reconstruct the receiver carrying its concrete type arguments so a
        // user `impl<T> Trait for HashMap<K, V>` / `Vec<E>` dispatched here can
        // bind the impl's type parameters from the element/key/value types and
        // project its `Output`/return — the builtin-vs-user asymmetry fix.
        let receiver = cx.receiver_with_args(kind);
        if let Some(ret_ty) =
            self.try_dispatch_primitive_trait_method(&receiver, method, args, span)
        {
            return ret_ty;
        }
        for arg in args {
            let (expr, sp) = arg.expr();
            self.synthesize(expr, sp);
        }
        self.report_error(
            TypeErrorKind::UndefinedMethod,
            span,
            format!("no method `{method}` on {}", kind.name()),
        );
        Ty::Error
    }

    /// The single descriptor-driven front-half admission authority for builtin
    /// collection method calls, replacing the three mirrored resolvers.
    ///
    /// Flow (preserving the historical per-arm ordering exactly):
    /// arity → arguments → element validation / lowering / recording → return.
    /// Unknown / genuinely divergent methods (those absent from
    /// [`collection_method_desc`]) fall through to the fail-closed fallback; the
    /// Vec-specific `contains`/`map`/`filter`/`fold`/`join` arms and the
    /// structural-array guard are handled by `check_vec_method` before it
    /// delegates here.
    pub(super) fn check_collection_method(
        &mut self,
        kind: CollectionKind,
        cx: &CollectionTyCx,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let Some(desc) = collection_method_desc(kind, method) else {
            return self.collection_method_fallback(kind, cx, method, args, span);
        };
        if let Some(arity) = desc.arity {
            self.check_arity(args, arity, &format!("`{}.{method}`", kind.name()), span);
        }
        if !self.check_collection_args(kind, desc.arg_templates, cx, args, span) {
            return Ty::Error;
        }
        if !self.run_collection_admission(kind, method, cx, span) {
            return Ty::Error;
        }
        self.collection_ret(kind, desc.ret, cx, span)
    }

    /// Resolve the per-call-site `ResolvedCall` for HashMap/HashSet via the
    /// registry, populate `resolved_calls`, and surface user-facing
    /// diagnostics on resolver failure.
    ///
    /// After W4.001 Stage C3 this is the sole admission authority for
    /// HashMap/HashSet method dispatch — the per-V symbol-selection
    /// helpers (`resolve_hashmap_runtime_symbol` / `_hashset_`) and the
    /// dual-emit `MethodCallRewrite::RewriteToFunction` arms have retired.
    /// Unsatisfied `where`-bounds (e.g. `HashMap<f64, _>` failing
    /// `K: Hash`) emit `TypeErrorKind::BoundsNotSatisfied` with attribution
    /// to the witness type; missing impls emit `InvalidOperation`.
    pub(super) fn check_hashmap_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let key_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        let val_ty = type_args
            .get(1)
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        // Trait-routed `Index<K>` accessor: `<HashMap<K, V> as Index>::get
        // -> Option<V>` — the map twin of `Vec::get`. Dispatch is marked as
        // the `Index` primitive-trait impl and records the
        // `hew_hashmap_get_layout` resolved call, which codegen rewrites to
        // the SINGLE fresh-owner clone choke (`hew_hashmap_get_clone_layout`)
        // that writes a retained/cloned owner into the `Option` payload
        // (drop-safe; `by-value-heap-params-are-borrows` P0). `get` is
        // intentionally NOT in `collection_method_desc`: this explicit arm
        // keeps the accessor model uniform with `Vec`, while the trapping
        // `m[k]` read is the sibling `Index::at` (`-> V`).
        if method == "get" {
            self.check_arity(args, 1, "`HashMap.get`", span);
            if let Some(arg) = args.first() {
                let (expr, sp) = arg.expr();
                self.check_against(expr, sp, &key_ty);
            }
            // Enforce `K: Hash + Eq` and reject unsafe key/value element
            // types — the same admission the table-driven path ran for
            // `get` (fire-and-return on rejection).
            if !self.validate_hashmap_owned_element_types(&key_ty, &val_ty, span) {
                return Ty::Error;
            }
            self.record_method_call_receiver_kind(
                span,
                MethodCallReceiverKind::PrimitiveTraitImpl {
                    trait_name: "Index".to_string(),
                    canonical_receiver: "HashMap".to_string(),
                },
            );
            // Records the `Map::get` resolved call. `<HashMap<K, V> as
            // Index>::Output` is `V`, so the projected return is `Option<V>`.
            self.record_resolved_hashmap_call("get", &key_ty, &val_ty, span);
            return Ty::option(val_ty);
        }
        // `HashMap::remove(k) -> Option<V>` (A233): the removing twin of `get`.
        // Handled here (not in the descriptor table) for the same reason as
        // `get` — the `Option<V>` projection plus resolved-call recording is a
        // code hook. Records the `Map::remove` resolved call, which resolves to
        // the `hew_hashmap_remove_take_layout` move-out kernel (drop the key,
        // MOVE the value out into the `Some` payload; drop-safe — the map keeps
        // no copy, so exactly one owner of V). `remove(absent)` yields `None`.
        if method == "remove" {
            self.check_arity(args, 1, "`HashMap.remove`", span);
            if let Some(arg) = args.first() {
                let (expr, sp) = arg.expr();
                self.check_against(expr, sp, &key_ty);
            }
            // Enforce `K: Hash + Eq` and reject unsafe key/value element types —
            // the same admission `get` runs (fire-and-return on rejection).
            if !self.validate_hashmap_owned_element_types(&key_ty, &val_ty, span) {
                return Ty::Error;
            }
            self.record_resolved_hashmap_call("remove", &key_ty, &val_ty, span);
            return Ty::option(val_ty);
        }
        // `into_iter` resolves to a `HashMapIter<K, V>` cursor so the pipeline
        // form (`iter::map(m.into_iter(), ..)`) matches `Vec::into_iter` — the
        // map twin of the `check_vec_method` `into_iter` arm. The cursor is
        // built (in HIR) from `keys()` / `values()` snapshots, the same proven
        // clone-on-read path the `for (k, v) in m` desugar uses, so record both
        // projection facts here: zero-width synthetic spans at the call's
        // start/end offsets, matching the for-in span derivation and reproduced
        // byte-for-byte by the HIR rewrite. A standalone `impl IntoIterator for
        // HashMap` is intentionally absent — its body would project on an
        // abstract receiver the checker cannot admit (see std/builtins.hew).
        if method == "into_iter" {
            self.check_arity(args, 0, "`HashMap.into_iter`", span);
            let keys_span = span.start..span.start;
            let values_span = span.end..span.end;
            let mut iter_ty = Ty::Error;
            if self.validate_hashmap_projection_element_types(&key_ty, &val_ty, "keys", &keys_span)
                && self.validate_hashmap_projection_element_types(
                    &key_ty,
                    &val_ty,
                    "values",
                    &values_span,
                )
            {
                let key_vec = self.make_vec_type(key_ty.clone(), &keys_span);
                let val_vec = self.make_vec_type(val_ty.clone(), &values_span);
                self.record_type(&keys_span, &key_vec);
                self.record_type(&values_span, &val_vec);
                self.record_resolved_hashmap_call("keys", &key_ty, &val_ty, &keys_span);
                self.record_resolved_hashmap_call("values", &key_ty, &val_ty, &values_span);
                let resolved_key = self.subst.resolve(&key_ty);
                let resolved_val = self.subst.resolve(&val_ty);
                if let (Ok(key_resolved), Ok(val_resolved)) = (
                    ResolvedTy::from_ty(&resolved_key),
                    ResolvedTy::from_ty(&resolved_val),
                ) {
                    self.record_method_call_receiver_kind(
                        span,
                        MethodCallReceiverKind::PrimitiveTraitImpl {
                            trait_name: "IntoIterator".to_string(),
                            canonical_receiver: "HashMap".to_string(),
                        },
                    );
                    self.record_method_call_rewrite(
                        span,
                        MethodCallRewrite::BuiltinHashMapIntoIter {
                            key_ty: key_resolved,
                            val_ty: val_resolved,
                        },
                    );
                }
                iter_ty =
                    Ty::builtin_named(BuiltinType::HashMapIter, vec![resolved_key, resolved_val]);
            }
            return iter_ty;
        }
        let cx = CollectionTyCx::hashmap(key_ty, val_ty);
        self.check_collection_method(CollectionKind::HashMap, &cx, method, args, span)
    }

    pub(super) fn check_hashset_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let elem_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        let cx = CollectionTyCx::hashset(elem_ty);
        self.check_collection_method(CollectionKind::HashSet, &cx, method, args, span)
    }

    pub(super) fn check_rc_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let inner_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        let record = |checker: &mut Self, op| {
            checker.record_rc_intrinsic(span, op, &inner_ty);
        };
        match method {
            // rc.clone() increments the reference count and returns a new Rc<T>
            "clone" => {
                self.check_arity(args, 0, "`Rc.clone`", span);
                record(self, RcIntrinsicOp::Clone);
                Ty::rc(inner_ty)
            }
            // rc.get() copies the inner value out of the Rc.
            // `LoadOp` performs a bitwise copy, which is only sound for `Copy`
            // types (no ownership to duplicate).  For non-Copy `T`, callers
            // share access via `rc.clone()` instead.
            "get" => {
                self.check_arity(args, 0, "`Rc.get`", span);
                if !self
                    .registry
                    .implements_marker(&inner_ty, MarkerTrait::Copy)
                {
                    self.report_error(
                        TypeErrorKind::BoundsNotSatisfied,
                        span,
                        format!(
                            "`Rc.get` requires `T: Copy`; `{}` is not `Copy` — \
                             use `rc.clone()` to share the reference instead",
                            inner_ty.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                record(self, RcIntrinsicOp::GetCopy);
                inner_ty
            }
            "set" => {
                self.check_arity(args, 1, "`Rc.set`", span);
                if let Some(arg) = args.first() {
                    let (expr, arg_span) = arg.expr();
                    let arg_ty = self.synthesize(expr, arg_span);
                    self.expect_type(&inner_ty, &arg_ty, arg_span);
                    self.reject_borrowed_parameter_consumption(expr, arg_span, "Rc.set");
                }
                record(self, RcIntrinsicOp::Set);
                Ty::Unit
            }
            "downgrade" => {
                self.check_arity(args, 0, "`Rc.downgrade`", span);
                record(self, RcIntrinsicOp::Downgrade);
                Ty::weak(inner_ty)
            }
            // rc.strong_count() returns the current reference count as i64
            "strong_count" => {
                self.check_arity(args, 0, "`Rc.strong_count`", span);
                record(self, RcIntrinsicOp::StrongCount);
                Ty::I64
            }
            "weak_count" => {
                self.check_arity(args, 0, "`Rc.weak_count`", span);
                record(self, RcIntrinsicOp::WeakCount);
                Ty::I64
            }
            "is_unique" => {
                self.check_arity(args, 0, "`Rc.is_unique`", span);
                record(self, RcIntrinsicOp::IsUnique);
                Ty::Bool
            }
            _ => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `Rc<{}>`", inner_ty.user_facing()),
                );
                Ty::Error
            }
        }
    }

    pub(super) fn check_weak_method(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let inner_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        match method {
            "clone" => {
                self.check_arity(args, 0, "`Weak.clone`", span);
                self.record_rc_intrinsic(span, RcIntrinsicOp::WeakClone, &inner_ty);
                Ty::weak(inner_ty)
            }
            "upgrade" => {
                self.check_arity(args, 0, "`Weak.upgrade`", span);
                self.record_rc_intrinsic(span, RcIntrinsicOp::WeakUpgrade, &inner_ty);
                Ty::option(Ty::rc(inner_ty))
            }
            _ => {
                for arg in args {
                    let (expr, arg_span) = arg.expr();
                    self.synthesize(expr, arg_span);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `Weak<{}>`", inner_ty.user_facing()),
                );
                Ty::Error
            }
        }
    }

    fn record_rc_intrinsic(&mut self, span: &Span, op: RcIntrinsicOp, payload_ty: &Ty) {
        let resolved = self
            .subst
            .resolve(payload_ty)
            .materialize_literal_defaults();
        match ResolvedTy::from_ty(&resolved) {
            Ok(payload_ty) => self.record_method_call_rewrite(
                span,
                MethodCallRewrite::RcIntrinsic { op, payload_ty },
            ),
            Err(_) => self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "Rc/Weak operations require a concrete payload type".to_string(),
            ),
        }
    }

    fn report_vec_contains_layout_equality_gate(
        &mut self,
        elem_ty: &Ty,
        eligibility: crate::eq_eligibility::EqEligibility,
        span: &Span,
    ) {
        use crate::eq_eligibility::EqEligibility;

        let reason = match eligibility {
            EqEligibility::Eligible => format!(
                "`Vec.contains` on layout-backed element type `{}` is equality-eligible, \
                 but layout contains is not yet supported for this element type",
                elem_ty.user_facing()
            ),
            EqEligibility::IneligibleManaged(managed_ty) => format!(
                "`Vec.contains` on layout-backed element type `{}` requires aggregate \
                 equality, but `{}` is layout-managed/non-Copy data",
                elem_ty.user_facing(),
                managed_ty.user_facing()
            ),
            EqEligibility::IneligibleOwned(owned_ty) => format!(
                "`Vec.contains` on layout-backed element type `{}` requires aggregate \
                 equality, but `{}` is owned or heap-backed data",
                elem_ty.user_facing(),
                owned_ty.user_facing()
            ),
            EqEligibility::IneligibleUnknown => format!(
                "`Vec.contains` on layout-backed element type `{}` requires aggregate \
                 equality, but equality eligibility is unknown",
                elem_ty.user_facing()
            ),
        };

        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "{reason}; no runtime method rewrite was recorded, and layout Vec contains \
                 remains fail-closed"
            ),
        );
    }

    /// Decide whether a non-Copy `Vec<T>` element type may route through the
    /// W5.016 owned-element ABI (`hew_vec_*_owned`).
    ///
    /// Admissible when the element is a user record or enum whose concrete
    /// fields have clone/drop thunks. Tuple
    /// elements are NOT yet admitted here: their `__hew_tuple_*_inplace` thunk
    /// synthesis lands in a later slice; until then they stay fail-closed.
    ///
    /// Stays fail-closed for every element that lacks a thunk path: an element
    /// containing a `Vec`/`HashMap`/`HashSet` field (general container-in-
    /// container clone/drop is a separate lane) and any non-record/enum nominal.
    /// Explain why a non-`Copy` Vec element type was rejected at construction,
    /// for use in the fail-closed diagnostic. Returns a clause that completes
    /// "element type `X` cannot be a Vec element: {clause}". Only called on the
    /// non-`Copy` + not-admissible path, so the element genuinely lacks a Vec
    /// lowering today.
    ///
    /// Distinguishes the common self-recursive / container-in-container case
    /// (`enum R { Array(Vec<R>); ... }`) — which IS owned but needs the
    /// recursive owned-thunk synthesis that is a separate follow-on — from a
    /// generically unsupported element shape, so the message does not
    /// misleadingly blame `Copy` or name `hew_vec_new_with_layout` for an
    /// owned enum.
    pub(super) fn vec_element_rejection_reason(&self, elem_ty: &Ty) -> String {
        if matches!(elem_ty, Ty::Tuple(_)) {
            if self.vec_element_contains_function(elem_ty, &mut HashSet::new()) {
                return "it contains a function value, whose closure environment cannot be \
                        cloned by the owned-element Vec runtime"
                    .to_string();
            }
            if self.vec_element_contains_unowned_container(
                elem_ty,
                &HashSet::new(),
                &mut HashSet::new(),
            ) {
                return "it contains a `Vec`/`HashMap`/`HashSet` field, which cannot be \
                        cloned from inside a nested tuple element"
                    .to_string();
            }
        }
        if let Ty::Named {
            name,
            builtin,
            args,
        } = elem_ty
        {
            if builtin.is_none() {
                if let Some(type_def) = self.type_defs.get(name) {
                    if matches!(type_def.kind, TypeDefKind::Machine) {
                        if !args.is_empty() {
                            return "a generic machine instantiation has no \
                                    per-instantiation layout (machines canonicalize to one \
                                    bare-named declaration layout); only monomorphic \
                                    machine values can ride the owned-element queue witness"
                                .to_string();
                        }
                        // Monomorphic machine: valid as a channel/queue element
                        // but not as a Vec element — there is no
                        // `__hew_machine_*_inplace` Vec thunk synthesis path
                        // today. Use a channel to pass machine snapshots.
                        return "machine values cannot be `Vec` elements; \
                                use a channel (`Sender<M>`/`Receiver<M>`) to \
                                pass machine snapshots between actors"
                            .to_string();
                    }
                    let is_record_or_enum = matches!(
                        type_def.kind,
                        TypeDefKind::Record | TypeDefKind::Struct | TypeDefKind::Enum
                    );
                    if is_record_or_enum
                        && !self
                            .record_enum_collection_fields_clonable(elem_ty, &mut HashSet::new())
                    {
                        return "it contains a `Vec`/`HashMap`/`HashSet` field whose \
                                element type has no clone/drop thunk path for the \
                                owned-element Vec runtime (a function/closure, machine, \
                                opaque, or `Rc` leaf, or a mutually recursive type with \
                                no container indirection)"
                            .to_string();
                    }
                }
            }
        }
        "it has no clone/drop thunk path for the owned-element Vec runtime".to_string()
    }

    /// #2647 — converge the MIR indirect-enum Vec-element reject at the checker
    /// boundary. An `indirect enum` element takes the `Ptr` token in
    /// [`classify_element`](crate::vec_authority::classify_element) (its `Vec`
    /// buffer is one heap-boxed node pointer per slot, built by codegen's
    /// `hew_vec_new_ptr` arm), so it never reaches the `Layout`-gated
    /// admissibility check the record/enum arms consult — the checker admits it
    /// today, then MIR rejects it with `Unsupported(NoReleaseProtocol)` because
    /// the per-element node free is unwired. The two verdicts diverge: the
    /// authoritative type-checker reports no error while the downstream MIR pass
    /// fails closed.
    ///
    /// This surfaces the SAME release-protocol reason at the checker boundary
    /// (where the element type is already known), matching MIR's reject for
    /// EVERY indirect enum — scalar OR heap payload — because the indirect boxing
    /// is a heap node the plain pointer-ABI buffer cannot release element by
    /// element. It does NOT change the pointer-token ABI routing: `classify_element`
    /// still tokens the element `Ptr`, so a pointer-backed element can never
    /// select an owned Vec ABI family; this reject sits ABOVE that routing, not
    /// in place of it.
    ///
    /// Returns `Some(reason)` for an indirect-enum element, `None` otherwise.
    pub(super) fn indirect_enum_vec_element_reject_reason(&self, elem_ty: &Ty) -> Option<String> {
        let Ty::Named {
            name,
            builtin: None,
            ..
        } = elem_ty
        else {
            return None;
        };
        let type_def = self.type_defs.get(name)?;
        if type_def.is_indirect && matches!(type_def.kind, TypeDefKind::Enum) {
            return Some(
                "it is an indirect enum whose per-element release protocol is not yet wired, \
                 so its heap nodes would leak at scope exit"
                    .to_string(),
            );
        }
        None
    }

    /// Channel/stream element admission for the layout-witness queue path
    /// (`Sender<T>`/`Receiver<T>`/`Stream<T>` recv/send). An element is
    /// admissible when the codegen element witness can describe it:
    ///
    /// - `string` / `bytes` — content-encoded queue envelopes;
    /// - Copy-eligible primitives and `BitCopy` records ([`primitive_copy_layout`]
    ///   resolves a fixed width) — Plain raw-representation envelopes;
    /// - heap-owning record/enum/tuple value types the owned-element Vec
    ///   thunk path admits ([`Self::vec_owned_element_admissible`] — the SAME
    ///   authority codegen's witness synthesis delegates to, so the checker
    ///   and the witness can never disagree about one element type);
    /// - monomorphic machine values — machines are tagged-union value types
    ///   whose state-variant layout is registered in `type_defs.variants`,
    ///   so the owned-element queue witness can describe them (with the same
    ///   no-unowned-container requirement as enum channel elements).
    ///   Generic machine instantiations are excluded (the substrate
    ///   canonicalizes to one bare-named layout; per-instantiation witnesses
    ///   do not exist). Machine admission lives HERE, not in
    ///   `vec_owned_element_admissible`, so `Vec<machine>` stays fail-closed.
    ///
    /// Everything else fails closed: builtin container/handle nominals
    /// (`Vec`/`HashMap`/streams/channels/pids), closures, and any type
    /// without a clone/drop thunk path. `BitCopy` enums ride the
    /// owned-element authority's record/enum admission and are lowered
    /// Plain by the witness (no heap leaf → no thunks), which is the
    /// correct Copy semantics.
    pub(super) fn queue_elem_admissible(&self, elem_ty: &Ty) -> bool {
        match elem_ty {
            // String/bytes are content-encoded envelopes; unconstrained
            // numeric literals default to i64/f64 (Plain 8-byte envelopes)
            // at literal-defaulting time, so a queue element constrained
            // only by a literal (`tx.send(42)`) must not be rejected
            // before defaulting runs.
            Ty::String | Ty::Bytes | Ty::IntLiteral | Ty::FloatLiteral => true,
            // Monomorphic machine values travel the owned-element queue
            // witness: machines are tagged-union value types registered in
            // `type_defs.variants`, satisfying the same thunk-path
            // requirements as enums. Generic machine instantiations are
            // refused (canonicalised to one bare-named decl layout; no
            // per-instantiation witness exists).
            Ty::Named {
                name,
                builtin,
                args,
            } if builtin.is_none() => {
                if let Some(type_def) = self.type_defs.get(name) {
                    if matches!(type_def.kind, TypeDefKind::Machine) {
                        // Generic instantiation: no per-instantiation layout.
                        if !args.is_empty() {
                            return false;
                        }
                        // Monomorphic: apply the same no-unowned-container
                        // requirement as for enum channel elements.
                        return !self.vec_element_contains_unowned_container(
                            elem_ty,
                            &HashSet::new(),
                            &mut HashSet::new(),
                        );
                    }
                }
                crate::check::admissibility::primitive_copy_layout(elem_ty, &self.type_defs)
                    .is_some()
                    || self.queue_owned_element_admissible(elem_ty)
            }
            // Builtin container/handle nominals (`Vec`/`HashMap`/`HashSet`/
            // `Rc`/handles/...) can never ride the element-layout queue
            // witness: their ownership lives in a runtime context the queue
            // cannot clone or drop. This stays in lockstep with
            // `queue_elem_rejection_reason`, which rejects every `builtin:
            // Some(_)`. `vec_owned_element_admissible` now admits nested-
            // container Vec ELEMENTS for copy-in push (#1722), but that is a
            // Vec-storage property, not a queue property, and must not leak
            // here. Primitives (`i64`/`bool`/`char`/...) are dedicated `Ty`
            // variants (not `Ty::Named`), so they remain queue-admissible via
            // the `_` arm's `primitive_copy_layout` check.
            Ty::Named {
                builtin: Some(_), ..
            } => false,
            _ => {
                crate::check::admissibility::primitive_copy_layout(elem_ty, &self.type_defs)
                    .is_some()
                    || self.queue_owned_element_admissible(elem_ty)
            }
        }
    }

    /// Queue/channel-scoped owned-element admission. A record/enum ELEMENT rides
    /// the element-layout queue witness only when the Vec-owned authority admits
    /// it AND it holds no builtin-collection field: the mailbox envelope
    /// deep-copies the element in but has no per-message recursive drop for a
    /// `Vec`/`HashMap`/`HashSet` field, so admitting a collection-bearing record
    /// through a channel leaks the field on every message. Vec STORAGE admits the
    /// same shape for copy-in `.push` (the outer Vec's per-element `drop_fn` frees
    /// it), but that is a Vec property, not a queue property — keep the channel
    /// path fail-closed until the mailbox drop path recurses through collection
    /// fields.
    fn queue_owned_element_admissible(&self, elem_ty: &Ty) -> bool {
        self.vec_owned_element_admissible(elem_ty)
            && !self.vec_element_contains_unowned_container(
                elem_ty,
                &HashSet::new(),
                &mut HashSet::new(),
            )
    }

    /// Explain why a channel/stream element type was rejected by
    /// [`Self::queue_elem_admissible`], for the fail-closed diagnostic.
    /// Completes "`{Container}<X>` is not supported: {clause}".
    pub(super) fn queue_elem_rejection_reason(&self, elem_ty: &Ty) -> String {
        if let Ty::Named {
            builtin: Some(_), ..
        } = elem_ty
        {
            return "builtin container and handle types cannot ride the \
                    element-layout queue witness; their ownership lives in a \
                    runtime context the queue cannot clone or drop"
                .to_string();
        }
        if matches!(elem_ty, Ty::Function { .. } | Ty::Closure { .. }) {
            return "function values cannot be queue elements".to_string();
        }
        self.vec_element_rejection_reason(elem_ty)
    }

    pub(super) fn vec_owned_element_admissible(&self, elem_ty: &Ty) -> bool {
        self.vec_owned_element_admissible_on_path(elem_ty, &mut HashSet::new())
    }

    /// [`Self::vec_owned_element_admissible`] carrying the record/enum names
    /// already on the active walk. A nested element reached across a container
    /// edge continues the SAME walk instead of restarting it, so a group that
    /// recurses only through container indirection (`A` holds `Vec<B>`, `B`
    /// holds `Vec<A>`) closes its name cycle and terminates.
    fn vec_owned_element_admissible_on_path(
        &self,
        elem_ty: &Ty,
        visiting: &mut HashSet<String>,
    ) -> bool {
        match elem_ty {
            // A trait-object slot owns its heap-promoted concrete box. The
            // descriptor is deliberately drop-only: push and consuming
            // iteration move the two-word fat pointer, while clone-dependent
            // surfaces remain refused.
            Ty::TraitObject { .. } => true,
            // Tuple element: a tuple with at least one owned (non-Copy) field
            // routes through the synthesized `__hew_tuple_*_inplace` thunk. An
            // all-Copy tuple is `Copy` and never reaches this admissibility
            // check (it takes the BitCopy `_layout` path). Nested tuples recurse
            // through the same authority; container and closure leaves still
            // fail closed.
            Ty::Tuple(elems) => {
                // A tuple starts no nominal recursion walk of its own, so a
                // container-bearing tuple field must prove its own path.
                elems
                    .iter()
                    .all(|e| self.vec_tuple_owned_field_admissible(e))
                    && !self.vec_element_contains_unowned_container(
                        elem_ty,
                        &HashSet::new(),
                        &mut HashSet::new(),
                    )
            }
            Ty::Named {
                name,
                builtin,
                args,
            } => {
                // Nested collection elements (Vec<T> / HashMap / HashSet) route
                // through the owned descriptor with COPY-IN, exactly like an
                // owned record: each pushed collection is deep-cloned so the
                // outer Vec is its sole owner, and released via the per-element
                // drop_fn. A closure-pair `Vec<fn>` /
                // `Vec<closure>` element keeps its existing pointer/closure-
                // pairs ABI (separate lane, #1722 out-of-scope) — never copy-in.
                // The owned-vs-managed clone selection is congruent by
                // construction: codegen's `collection_elem_clone_drop_syms` and
                // the inner Vec's own constructor both consult
                // `resolved_ty_element_owns_heap_for_owned_vec`, so the clone
                // primitive can never disagree with the inner Vec's ABI.
                match builtin {
                    Some(BuiltinType::HashMap | BuiltinType::HashSet) => return true,
                    Some(BuiltinType::Vec) => {
                        if args
                            .first()
                            .is_some_and(|e| matches!(e, Ty::Function { .. } | Ty::Closure { .. }))
                        {
                            return false;
                        }
                        return true;
                    }
                    // Sender is cloneable while Receiver is deliberately
                    // drop-only. Both need descriptor-backed Vec storage;
                    // copy/clone surfaces reject Receiver separately.
                    Some(
                        BuiltinType::Rc
                        | BuiltinType::Weak
                        | BuiltinType::Sender
                        | BuiltinType::Receiver,
                    ) => {
                        return args.len() == 1;
                    }
                    // Other builtin nominals are not user records/enums and
                    // have no owned-Vec thunk path.
                    Some(_) => return false,
                    // User-defined record/enum: fall through to the logic below.
                    None => {}
                }
                let Some(type_def) = self.lookup_type_def(name) else {
                    return false;
                };
                // Only record/struct/enum value types have synthesizable
                // inplace thunks. Machine types are NOT admitted here —
                // machine values are valid as CHANNEL/QUEUE elements (where
                // `queue_elem_admissible` handles them directly), but
                // `Vec<machine>` has no Vec-construction thunk path and must
                // refuse at compile time with a named diagnostic. Admitting
                // Machine here would let `Vec<SomeMachine>` compile and then
                // panic at runtime; keeping it out preserves fail-closed
                // parity with the base (614e0bed).
                if !matches!(
                    type_def.kind,
                    TypeDefKind::Record | TypeDefKind::Struct | TypeDefKind::Enum
                ) {
                    return false;
                }
                // A record/enum transitively holding a `Vec`/`HashMap`/`HashSet`
                // field is admissible when every such collection field is
                // CLONABLE by the synthesized in-place thunk — each field's
                // element type is a copy primitive, `string`/`bytes`, a
                // recursion edge back to a record/enum already on this walk (the
                // `enum R { A(Vec<R>); ... }` Redis-reply shape and its mutual
                // twin), or a nested admissible owned element. The record/enum's
                // `__hew_record_drop_inplace_<R>` / `__hew_enum_*_inplace_<E>`
                // thunk recurses through each collection field via the
                // owned-collection ABI (`hew_vec_{clone,free}_owned`,
                // `hew_{hashmap,hashset}_{clone,free}_layout`); the copy-in
                // `.push` deep-clone AND the scope-exit drop of the pushed source
                // are proven by the owned-element leak oracles. A field holding
                // an UNCLONABLE collection element (function/closure, machine,
                // opaque, `Rc`) fails the per-arg clonability check and keeps the
                // record fail-closed. The recursion escape is keyed on the
                // CONTAINER edge only; a directly self-referential record with no
                // container indirection still reaches `RecordCycle` in MIR
                // (LESSONS `recursive-admission-needs-indirection-witness`).
                self.record_enum_collection_fields_clonable(elem_ty, visiting)
            }
            _ => false,
        }
    }

    /// True when every builtin-collection field transitively reachable from a
    /// record/enum owned element `ty` is CLONABLE by the synthesized in-place
    /// thunk (see [`Self::vec_owned_element_admissible`]). `visiting` carries the
    /// record/enum names on the active walk. It is the single authority for
    /// termination, while the container-argument helpers below are the only
    /// places allowed to treat a re-entry as an indirection witness: a
    /// `Vec<R>` element or `HashMap<_, R>` value can close through the heap
    /// buffer. A direct member, a `HashMap<R, _>` key, and a `HashSet<R>`
    /// element remain inline and therefore reject on re-entry.
    /// Non-collection fields (`string`/`bytes`/primitives) carry no container
    /// and are trivially clonable; nested record/enum/tuple fields recurse.
    fn record_enum_collection_fields_clonable(
        &self,
        ty: &Ty,
        visiting: &mut HashSet<String>,
    ) -> bool {
        match ty {
            Ty::Named {
                name,
                builtin,
                args,
            } => {
                match builtin {
                    Some(BuiltinType::Vec) if args.len() == 1 => {
                        return self.vec_collection_arg_clonable(&args[0], visiting);
                    }
                    Some(BuiltinType::HashSet) if args.len() == 1 => {
                        return self.record_enum_collection_fields_clonable(&args[0], visiting);
                    }
                    Some(BuiltinType::HashMap) if args.len() == 2 => {
                        // A map's KEY participates in the inline key layout;
                        // only its value is stored behind the heap buffer.
                        return self.record_enum_collection_fields_clonable(&args[0], visiting)
                            && self.vec_collection_arg_clonable(&args[1], visiting);
                    }
                    Some(BuiltinType::Vec | BuiltinType::HashMap | BuiltinType::HashSet) => {
                        return false;
                    }
                    // Other builtins (Option/Result/Rc/handles) carry their own
                    // ABI; recurse only through their type arguments.
                    Some(_) => {
                        return args
                            .iter()
                            .all(|a| self.record_enum_collection_fields_clonable(a, visiting));
                    }
                    None => {}
                }
                let Some(type_def) = self.lookup_type_def(name) else {
                    // An unresolved nominal will produce its own type error;
                    // this structural proof cannot assume a clone/drop thunk.
                    return false;
                };
                let visit_key = type_def.name.clone();
                if !visiting.insert(visit_key.clone()) {
                    // We reached this nominal through an inline member. A
                    // descriptor-backed container must witness the re-entry
                    // before this point; otherwise the layout is infinitely
                    // sized and its clone/drop recursion is not admissible.
                    return false;
                }
                let ok = type_def.fields.values().all(|field_ty| {
                    let field_ty =
                        Self::instantiate_type_def_member(field_ty, &type_def.type_params, args);
                    self.record_enum_collection_fields_clonable(&field_ty, visiting)
                }) && type_def.variants.values().all(|variant| match variant {
                    VariantDef::Unit => true,
                    VariantDef::Tuple(tys) => tys.iter().all(|field_ty| {
                        let field_ty = Self::instantiate_type_def_member(
                            field_ty,
                            &type_def.type_params,
                            args,
                        );
                        self.record_enum_collection_fields_clonable(&field_ty, visiting)
                    }),
                    VariantDef::Struct(fields) => fields.iter().all(|(_, t)| {
                        let field_ty =
                            Self::instantiate_type_def_member(t, &type_def.type_params, args);
                        self.record_enum_collection_fields_clonable(&field_ty, visiting)
                    }),
                });
                visiting.remove(&visit_key);
                ok
            }
            Ty::Tuple(elems) => elems
                .iter()
                .all(|e| self.record_enum_collection_fields_clonable(e, visiting)),
            Ty::Array(inner, _) | Ty::Slice(inner) => {
                self.record_enum_collection_fields_clonable(inner, visiting)
            }
            // Primitives, `string`, `bytes`, function/closure surface types carry
            // no builtin-collection field of their own — a function/closure as a
            // record field is rejected upstream (`vec_element_contains_function`);
            // here it holds no container to clone.
            _ => true,
        }
    }

    /// True when a builtin-collection field's type argument `a` is a value the
    /// owned-collection clone/free ABI can deep-clone and release: a record/enum
    /// already on the active walk (its own thunk recurses through the inner
    /// collection), a copy primitive / `BitCopy` record, `string`/`bytes`, or a
    /// nested admissible owned element (record/enum/tuple/nested collection). An
    /// unclonable arg (function/closure, machine, opaque, `Rc`-bearing) makes the
    /// enclosing collection field — and thus the record/enum element — fail
    /// closed.
    ///
    /// This is the container edge, so `visiting` is a per-nominal
    /// container-indirection witness: only a cycle that closes ACROSS this heap
    /// container is admitted. Nested elements continue the same walk, so a
    /// mutually recursive group terminates instead of restarting the name set
    /// on every hop.
    fn vec_collection_arg_clonable(&self, a: &Ty, visiting: &mut HashSet<String>) -> bool {
        let resolved = self.subst.resolve(a).materialize_literal_defaults();
        if let Ty::Named {
            name,
            builtin: None,
            ..
        } = &resolved
        {
            let visit_key = self
                .lookup_type_def(name)
                .map_or_else(|| name.clone(), |type_def| type_def.name);
            if visiting.contains(&visit_key) {
                return true;
            }
        }
        matches!(&resolved, Ty::String | Ty::Bytes)
            || crate::check::admissibility::primitive_copy_layout(&resolved, &self.type_defs)
                .is_some()
            || self.vec_owned_element_admissible_on_path(&resolved, visiting)
    }

    fn vec_tuple_owned_field_admissible(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Tuple(elems) => elems
                .iter()
                .all(|elem| self.vec_tuple_owned_field_admissible(elem)),
            Ty::String | Ty::Bytes => true,
            Ty::Named {
                builtin: Some(BuiltinType::Rc | BuiltinType::Weak | BuiltinType::Sender),
                args,
                ..
            } => args.len() == 1,
            Ty::Named { builtin: None, .. } => {
                crate::check::admissibility::primitive_copy_layout(ty, &self.type_defs).is_some()
                    || self.vec_owned_element_admissible(ty)
            }
            Ty::Function { .. }
            | Ty::Closure { .. }
            | Ty::Array(_, _)
            | Ty::Slice(_)
            | Ty::Named {
                builtin: Some(BuiltinType::Vec | BuiltinType::HashMap | BuiltinType::HashSet),
                ..
            } => false,
            _ => crate::check::admissibility::primitive_copy_layout(ty, &self.type_defs).is_some(),
        }
    }

    fn vec_element_contains_function(&self, ty: &Ty, visiting: &mut HashSet<String>) -> bool {
        match ty {
            Ty::Function { .. } | Ty::Closure { .. } => true,
            Ty::Tuple(elems) => elems
                .iter()
                .any(|elem| self.vec_element_contains_function(elem, visiting)),
            Ty::Array(inner, _) | Ty::Slice(inner) => {
                self.vec_element_contains_function(inner, visiting)
            }
            Ty::Named {
                name,
                builtin: None,
                args,
            } => {
                if args
                    .iter()
                    .any(|arg| self.vec_element_contains_function(arg, visiting))
                {
                    return true;
                }
                if !visiting.insert(name.clone()) {
                    return false;
                }
                let result = self.type_defs.get(name).is_some_and(|td| {
                    td.fields
                        .values()
                        .any(|fty| self.vec_element_contains_function(fty, visiting))
                        || td.variants.values().any(|variant| match variant {
                            VariantDef::Unit => false,
                            VariantDef::Tuple(tys) => tys
                                .iter()
                                .any(|t| self.vec_element_contains_function(t, visiting)),
                            VariantDef::Struct(fields) => fields
                                .iter()
                                .any(|(_, t)| self.vec_element_contains_function(t, visiting)),
                        })
                });
                visiting.remove(name);
                result
            }
            Ty::Named { args, .. } => args
                .iter()
                .any(|arg| self.vec_element_contains_function(arg, visiting)),
            _ => false,
        }
    }

    /// True when `ty` (or a transitive record/enum member) is — or contains a
    /// field of — a builtin collection (`Vec`/`HashMap`/`HashSet`). Such a
    /// member has no `__hew_*_inplace` thunk path, so an owned-Vec element that
    /// transitively reaches one must stay fail-closed. The recursive enum's
    /// own self-edge through a `Vec` (`Array(Vec<RedisReply>)`) is the one
    /// admitted exception — here we only reject unowned-container fields, not
    /// the self-recursive edge.
    pub(super) fn vec_element_contains_unowned_container(
        &self,
        ty: &Ty,
        roots: &HashSet<String>,
        visiting: &mut HashSet<String>,
    ) -> bool {
        match ty {
            Ty::Named {
                name,
                builtin,
                args,
            } => {
                if matches!(
                    builtin,
                    Some(BuiltinType::Vec | BuiltinType::HashMap | BuiltinType::HashSet)
                ) {
                    // A collection whose every type argument is a `root` (the
                    // recursing element type) is the admitted self-recursion
                    // (`enum R { A(Vec<R>) }`): the enum's own owned thunk
                    // recurses through this field. Any other container element
                    // is unowned (no thunk path) — reject.
                    return !args.iter().all(|a| match a {
                        Ty::Named { name: an, .. } => roots.contains(an),
                        _ => false,
                    });
                }
                if builtin.is_some() {
                    // Other builtins (Option/Result/Rc/handles) carry their own
                    // ABI; recurse only through their type arguments.
                    return args
                        .iter()
                        .any(|a| self.vec_element_contains_unowned_container(a, roots, visiting));
                }
                if !visiting.insert(name.clone()) {
                    // Self-recursive edge on a user type: the recursion through a
                    // user record/enum is finite by construction here (it only
                    // recurses once per name). It carries no bare container.
                    return false;
                }
                let result = self.type_defs.get(name).is_some_and(|td| {
                    td.fields.values().any(|fty| {
                        self.vec_element_contains_unowned_container(fty, roots, visiting)
                    }) || td.variants.values().any(|variant| match variant {
                        VariantDef::Unit => false,
                        VariantDef::Tuple(tys) => tys.iter().any(|t| {
                            self.vec_element_contains_unowned_container(t, roots, visiting)
                        }),
                        VariantDef::Struct(fields) => fields.iter().any(|(_, t)| {
                            self.vec_element_contains_unowned_container(t, roots, visiting)
                        }),
                    })
                });
                visiting.remove(name);
                result
            }
            Ty::Tuple(elems) => elems
                .iter()
                .any(|e| self.vec_element_contains_unowned_container(e, roots, visiting)),
            Ty::Array(inner, _) | Ty::Slice(inner) => {
                self.vec_element_contains_unowned_container(inner, roots, visiting)
            }
            _ => false,
        }
    }

    fn check_runtime_vec_method_from_source(
        &mut self,
        type_args: &[Ty],
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let sig = self.lookup_builtin_vec_method_sig(type_args, method)?;
        sig.extern_symbol.as_ref()?;

        if matches!(method, "push" | "pop" | "remove" | "clear" | "clone") {
            self.check_arity(args, sig.params.len(), &format!("`Vec.{method}`"), span);
        }

        for (index, expected) in sig.params.iter().enumerate() {
            let Some(arg) = args.get(index) else {
                continue;
            };
            let (expr, arg_span) = arg.expr();
            if matches!(method, "set" | "remove") && index == 0 {
                let actual = self.synthesize(expr, arg_span);
                let resolved = self.subst.resolve(&actual);
                if Self::is_narrower_signed_int(&resolved) {
                    continue;
                }
            }
            self.check_against(expr, arg_span, expected);
        }

        let elem_ty = type_args
            .first()
            .map_or_else(|| Ty::Var(TypeVar::fresh()), |ty| self.subst.resolve(ty));
        self.record_resolved_vec_call(method, &elem_ty, span);
        Some(sig.return_type)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "Vec keeps the divergent contains/join/map/filter/fold arms and the structural-array guard inline; runtime-backed signatures delegate to the stdlib-source authority."
    )]
    pub(super) fn check_vec_method(
        &mut self,
        type_args: &[Ty],
        _receiver_ty: &Ty,
        resolved: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let elem_ty = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        let elem_ty_before = self.subst.resolve(&elem_ty);
        let mut elem_ty_before_visiting = HashSet::new();
        let elem_ty_before_has_structural_array = self
            .vec_element_contains_structural_array(&elem_ty_before, &mut elem_ty_before_visiting);
        let _ = self.validate_vec_element_type(&elem_ty, span);
        let runtime_method_declared = self
            .lookup_builtin_vec_method_sig(type_args, method)
            .is_some();
        if method == "clone" && matches!(self.subst.resolve(&elem_ty), Ty::TraitObject { .. }) {
            self.check_arity(args, 0, "`Vec.clone`", span);
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "`Vec<dyn Trait>.clone()` is not supported because trait objects have no \
                 semantic clone operation; use `into_iter()` to transfer the existing owners"
                    .to_string(),
            );
            return Ty::Error;
        }
        let result = match method {
            "into_iter" => {
                self.check_arity(args, 0, "`Vec.into_iter`", span);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if !self.validate_vec_iter_element_clone_type(&resolved_elem, span) {
                    return Ty::Error;
                }
                if let Ok(elem_resolved) = ResolvedTy::from_ty(&resolved_elem) {
                    self.record_method_call_receiver_kind(
                        span,
                        MethodCallReceiverKind::PrimitiveTraitImpl {
                            trait_name: "IntoIterator".to_string(),
                            canonical_receiver: "Vec".to_string(),
                        },
                    );
                    self.record_method_call_rewrite(
                        span,
                        MethodCallRewrite::BuiltinVecIntoIter {
                            elem_ty: elem_resolved,
                        },
                    );
                }
                Ty::builtin_named(BuiltinType::VecIter, vec![resolved_elem])
            }
            "iter" => {
                // `Vec<T>::iter()` yields the SAME `VecIter<T>` surface as
                // `into_iter()` without consuming the receiver, leaving the
                // source vec a live, independent owner. A `VecIter<T>` is a
                // first-class value with no lifetime — it can coexist with the
                // source, observe nothing of the source's later mutations, be
                // bound to an outer scope, returned, or held across a suspension
                // — so the cursor must NOT borrow the source's buffer. Hew's
                // `Vec` is a single-owner heap handle (no buffer refcount); a
                // shared handle would double-free when the source and cursor
                // both drop, alias the source's later mutations, or dangle if the
                // source's buffer is freed under the cursor. Instead the HIR
                // rewrite gives the cursor an INDEPENDENT CLONE of the source for
                // a place receiver (see `lower_builtin_vec_iter`): a
                // deep/retaining `hew_vec_clone` snapshot the cursor solely owns
                // and frees exactly once on its own drop. Per-element ownership is
                // identical to `into_iter` — `VecIter::next` clones each item out
                // on read (`hew_vec_get_clone`).
                //
                // Pre-record the clone projection at the call's start offset so
                // the synthesised `recv.clone()` the rewrite emits resolves
                // through the normal element-aware vec-clone authority (the same
                // `record_resolved_vec_call("clone", …)` an explicit `v.clone()`
                // uses, including per-monomorphisation re-resolution for an
                // abstract element). Mirrors how `HashMap::into_iter` pre-records
                // its `keys()`/`values()` projections.
                self.check_arity(args, 0, "`Vec.iter`", span);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if matches!(resolved_elem, Ty::TraitObject { .. }) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "`Vec<dyn Trait>.iter()` is not supported because a borrowed iterator \
                         needs an independent clone of each trait object; use `into_iter()` to \
                         transfer the existing owners"
                            .to_string(),
                    );
                    return Ty::Error;
                }
                if !self.validate_vec_iter_element_clone_type(&resolved_elem, span) {
                    return Ty::Error;
                }
                if let Ok(elem_resolved) = ResolvedTy::from_ty(&resolved_elem) {
                    let clone_span = span.start..span.start;
                    let vec_ty = self.make_vec_type(resolved_elem.clone(), &clone_span);
                    self.record_type(&clone_span, &vec_ty);
                    self.record_resolved_vec_call("clone", &resolved_elem, &clone_span);
                    self.record_method_call_rewrite(
                        span,
                        MethodCallRewrite::BuiltinVecIter {
                            elem_ty: elem_resolved,
                        },
                    );
                }
                Ty::builtin_named(BuiltinType::VecIter, vec![resolved_elem])
            }
            "get" if runtime_method_declared => {
                // Trait-routed `Index<i64>` accessor: `<Vec<T> as Index>::get
                // -> Option<T>`. Dispatch is marked as the `Index` primitive
                // trait impl and lowered to the element-agnostic
                // `hew_vec_get_clone` intrinsic — the SINGLE fresh-owner choke
                // point that writes a retained/cloned owner into the `Option`
                // payload (drop-safe; `by-value-heap-params-are-borrows` P0).
                // `get` is intentionally NOT in `collection_method_desc`: this
                // explicit arm preserves the index-site widening ergonomic
                // (i8/i16/i32 → i64) that the strict trait signature would
                // otherwise reject.
                self.check_arity(args, 1, "`Vec.get`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    let actual = self.synthesize(expr, sp);
                    let resolved_idx = self.subst.resolve(&actual);
                    // Accept i64 or any narrower signed int (widened at the
                    // index site, identical to `[]`/`set`/`remove`); otherwise
                    // run the normal i64 coercion (literals, error wording).
                    if !Self::is_narrower_signed_int(&resolved_idx) {
                        self.check_against(expr, sp, &Ty::I64);
                    }
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                if matches!(resolved_elem, Ty::TraitObject { .. }) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "`Vec<dyn Trait>.get()` is not supported because returning an owned \
                         element would require a semantic trait-object clone; use pop/remove or \
                         consuming iteration to move an owner out"
                            .to_string(),
                    );
                    return Ty::Error;
                }
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::PrimitiveTraitImpl {
                        trait_name: "Index".to_string(),
                        canonical_receiver: "Vec".to_string(),
                    },
                );
                // Records the `hew_vec_get_clone` resolved call through the
                // shared Vec authority. Function/closure elements fail closed,
                // so no resolved call is recorded.
                self.record_resolved_vec_call("get", &resolved_elem, span);
                // `<Vec<T> as Index>::Output` is `T`, so the projected return
                // is `Option<T>`.
                Ty::option(resolved_elem)
            }
            "contains" if runtime_method_declared => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                if crate::vec_authority::classify_element(&resolved_elem, &self.type_defs)
                    == Some(crate::vec_authority::VecElementToken::Layout)
                {
                    // W3.032 Slice 3e: lift the layout gate for equality-
                    // eligible Copy records/tuples.  Authority chain: the
                    // checker is the sole arbiter; HIR/MIR/codegen treat the
                    // recorded `"hew_vec_contains_thunk"` symbol string as an
                    // opaque eligibility certificate and do NOT re-derive
                    // eligibility (see W3.032 plan §"Checker authority
                    // carry").
                    let eligibility =
                        crate::eq_eligibility::ty_is_eq_eligible(&resolved_elem, &self.type_defs);
                    let is_copy = self.vec_element_has_copy_layout(&resolved_elem);
                    let is_owned_admissible = self.vec_owned_element_admissible(&resolved_elem);
                    if matches!(eligibility, crate::eq_eligibility::EqEligibility::Eligible)
                        && (is_copy || is_owned_admissible)
                    {
                        self.record_resolved_vec_call("contains", &resolved_elem, span);
                    } else if matches!(eligibility, crate::eq_eligibility::EqEligibility::Eligible)
                    {
                        // Eligible but not Copy: layout-managed semantics
                        // (clone/drop) are not yet supported here.  The
                        // historical `_layout` fail-closed diagnostic is the
                        // closest substitute and names the would-be symbol.
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            format!(
                                "`Vec.contains` on layout-backed element type `{}` requires \
                                 the element to be `Copy`; layout-managed records require \
                                 clone/drop semantics that are not implemented for \
                                 equality-based contains",
                                resolved_elem.user_facing()
                            ),
                        );
                    } else {
                        self.report_vec_contains_layout_equality_gate(
                            &resolved_elem,
                            eligibility,
                            span,
                        );
                    }
                } else {
                    self.record_resolved_vec_call("contains", &resolved_elem, span);
                }
                Ty::Bool
            }
            "join" if runtime_method_declared => {
                self.check_arity(args, 1, "`Vec.join`", span);
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &Ty::String);
                }
                if elem_ty == Ty::String {
                    // `Vec<string>::join` is the sole element-type cell;
                    // non-string element rejection remains the type gate
                    // below.
                    let resolved_elem = self.subst.resolve(&elem_ty);
                    self.record_resolved_vec_call("join", &resolved_elem, span);
                } else {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "`Vec.join` is only available on Vec<string>, not Vec<{}>",
                            elem_ty.user_facing()
                        ),
                    );
                }
                Ty::String
            }
            "map" => {
                self.check_arity(args, 1, "`Vec.map`", span);
                let ret_ty = Ty::Var(TypeVar::fresh());
                let expected_fn = Ty::Function {
                    params: vec![elem_ty.clone()],
                    ret: Box::new(ret_ty.clone()),
                };
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_ret = self.subst.resolve(&ret_ty);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if !self.reject_vec_pipeline_fn_element("map", &resolved_elem, span) {
                    self.record_vec_higher_order_rewrite(
                        VecHigherOrderOp::Map,
                        &resolved_elem,
                        &resolved_ret,
                        span,
                    );
                }
                self.make_vec_type(resolved_ret, span)
            }
            "filter" => {
                self.check_arity(args, 1, "`Vec.filter`", span);
                let expected_fn = Ty::Function {
                    params: vec![elem_ty.clone()],
                    ret: Box::new(Ty::Bool),
                };
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                if !self.reject_vec_pipeline_fn_element("filter", &resolved_elem, span) {
                    self.record_vec_higher_order_rewrite(
                        VecHigherOrderOp::Filter,
                        &resolved_elem,
                        &resolved_elem,
                        span,
                    );
                }
                resolved.clone()
            }
            "reduce" => {
                // Argument order: closure first, seed second
                // (`numbers.reduce(|a, b| a + b, 0)`) — `fold` with the
                // arguments flipped for chain readability (spec §3.8.6
                // documents this seeded form). A seedless 1-arg `reduce`
                // is deliberately not provided: it would need an
                // empty-vector answer, and we refuse to invent one.
                self.check_arity(args, 2, "`Vec.reduce`", span);
                let acc_ty = if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp)
                } else {
                    Ty::Var(TypeVar::fresh())
                };
                let expected_fn = Ty::Function {
                    params: vec![acc_ty.clone(), elem_ty.clone()],
                    ret: Box::new(acc_ty.clone()),
                };
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_acc = self.subst.resolve(&acc_ty);
                let resolved_elem = self.subst.resolve(&elem_ty);
                if !self.reject_vec_pipeline_fn_element("reduce", &resolved_elem, span) {
                    self.record_vec_higher_order_rewrite(
                        VecHigherOrderOp::Reduce,
                        &resolved_elem,
                        &resolved_acc,
                        span,
                    );
                }
                resolved_acc
            }
            "fold" => {
                self.check_arity(args, 2, "`Vec.fold`", span);
                let acc_ty = if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp)
                } else {
                    Ty::Var(TypeVar::fresh())
                };
                let expected_fn = Ty::Function {
                    params: vec![acc_ty.clone(), elem_ty.clone()],
                    ret: Box::new(acc_ty.clone()),
                };
                if let Some(arg) = args.get(1) {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &expected_fn);
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                if self.reject_vec_pipeline_fn_element("fold", &resolved_elem, span) {
                    Ty::Error
                } else {
                    self.subst.resolve(&acc_ty)
                }
            }
            _ if runtime_method_declared => self
                .check_runtime_vec_method_from_source(type_args, method, args, span)
                .expect("Vec method signature was present immediately before dispatch"),
            _ => {
                // Unknown method fail-closed fallback.  Kept inline (NOT routed
                // through the shared `collection_method_fallback`) to preserve
                // the historical asymmetry: a successful primitive-trait
                // dispatch `return`s early and bypasses the structural-array
                // post-guard, whereas the "no method on Vec" path falls through
                // to it.
                let receiver = Ty::Named {
                    builtin: Some(BuiltinType::Vec),
                    name: "Vec".to_string(),
                    args: vec![self.subst.resolve(&elem_ty)],
                };
                if let Some(ret_ty) =
                    self.try_dispatch_primitive_trait_method(&receiver, method, args, span)
                {
                    return ret_ty;
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on Vec"),
                );
                Ty::Error
            }
        };
        let elem_ty_after = self.subst.resolve(&elem_ty);
        let mut elem_ty_after_visiting = HashSet::new();
        let elem_ty_after_has_structural_array =
            self.vec_element_contains_structural_array(&elem_ty_after, &mut elem_ty_after_visiting);
        if elem_ty_after_has_structural_array && !elem_ty_before_has_structural_array {
            let _ = self.validate_vec_element_type(&elem_ty_after, span);
            return Ty::Error;
        }
        result
    }

    /// Stage A2: dispatch a method call on a primitive or compiler-builtin
    /// generic receiver to a user `impl Trait for <kind>` body via the
    /// `primitive_trait_impls` side table populated in Stage A1.
    ///
    /// Receiver-keyed (NOT trait-name-keyed): the lookup goes through
    /// `lookup_primitive_trait_method`, which keys on the canonical receiver
    /// kind first, so the surviving five magic `dyn Display` callers
    /// (`assert_eq` / `assert_ne` / `to_string` / `len` / `stop`) cannot be
    /// hijacked by trait-name string matching.
    ///
    /// Returns `Some(return_ty)` after applying argument checks and recording
    /// the dispatch metadata for codegen, or `None` when no impl matches and
    /// the caller should emit its own "no method on X" diagnostic so existing
    /// "no method on Vec" / "no method on string" wording survives.
    ///
    /// Limitation: if two distinct traits each define a method of the same
    /// name on the same receiver kind, this returns the first match the
    /// table iteration encounters.  Acceptable today (only `Display` is in
    /// scope per Phase 1 of #1565), but Phase 2 (`Debug`) must add a
    /// disambiguation rule before introducing same-name conflicts.
    fn try_dispatch_primitive_trait_method(
        &mut self,
        resolved_receiver: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        // Default `IntLiteral` / `FloatLiteral` receivers to their canonical
        // numeric kind before the side-table lookup.  Without this,
        // `(42).fmt()` (literal-form receiver, never bound to a typed `let`)
        // would short-circuit on `canonical_primitive_or_builtin_key` returning
        // `None` for the still-polymorphic literal shape and the caller would
        // emit `no method `fmt` on int`, even though `(42_i64).fmt()` and
        // `let x: i64 = 42; x.fmt()` both succeed.  Mirrors the existing
        // defaulting sites at methods.rs:49 / 125 / 129 / 157 / 161 / 202 /
        // 254 / 317 — collapse the literal exactly at the boundary that would
        // otherwise diagnose, never eagerly upstream.
        let defaulted_receiver = resolved_receiver.materialize_literal_defaults();
        let canonical = Checker::canonical_primitive_or_builtin_key(&defaulted_receiver)?;
        let (trait_name, sig) = self.lookup_primitive_trait_method(&defaulted_receiver, method)?;
        // Bind the impl's type parameters from the concrete receiver's type
        // arguments BEFORE applying the signature, and PROVE the impl's `Self`
        // structurally matches the receiver. Without the binding a generic
        // builtin impl (`impl<E> Index for Vec<E>`) dispatched on `Vec<i64>`
        // leaves `E` (hence an `Option<E>` / `Self::Output` return) as an
        // unresolved inference var that escapes past the checker output
        // boundary — the builtin-vs-user asymmetry this fix closes. Without the
        // shape proof a constrained/concrete impl (`impl Acc for Vec<i64>`)
        // would be over-applied to a non-matching receiver (`Vec<string>`) and
        // project an authoritative-but-wrong return type (a fail-open). On a
        // non-match `instantiate_*` returns `None` and we fall through to the
        // "no method" path below, failing closed. User `Ty::Named` receivers
        // already do both via `lookup_named_method_sig`.
        let sig = self.instantiate_primitive_trait_method_sig(
            sig,
            &canonical,
            &trait_name,
            &defaulted_receiver,
        )?;
        let applied_sig = self.apply_instantiated_call_signature(
            &sig,
            None,
            args,
            span,
            SignatureArgApplication::PositionalOnly {
                arity_context: format!("method `{method}`"),
            },
            true,
            Some(GenericCallee::Method {
                type_name: &canonical,
                method,
                owner_type_args: &[],
            }),
        );
        let method_key = format!("{canonical}::{method}");
        // Concrete-specialised primitive impl (#2270): when the builtin receiver
        // has non-empty type args (e.g. `Vec<i64>`) and the impl is a concrete
        // specialisation (`impl Summable for Vec<i64>` — no generic impl type
        // params), the HIR fn_registry key is the mangled form
        // (`"Vec$$i64::total"`), not the bare `"Vec::total"`. The HIR first pass
        // mangled the key to prevent `impl Trait for Vec<i64>` and
        // `impl Trait for Vec<string>` from registering the same LLVM symbol.
        // Use the mangled c_symbol here so HIR can resolve it to a
        // `ResolvedRef::Item`; fall back to the bare key for generic impls and
        // for any type arg that cannot be mangled.
        let c_symbol = if let Ty::Named {
            args: receiver_type_args,
            ..
        } = &defaulted_receiver
        {
            if receiver_type_args.is_empty() {
                method_key.clone()
            } else {
                let resolved_args: Option<Vec<ResolvedTy>> = receiver_type_args
                    .iter()
                    .map(|ty| ResolvedTy::from_ty(ty).ok())
                    .collect();
                resolved_args
                    .as_ref()
                    .and_then(|args| crate::resolved_ty::mangle_impl_self_name(&canonical, args))
                    .filter(|m| self.fn_sigs.contains_key(&format!("{m}::{method}")))
                    .map_or_else(|| method_key.clone(), |m| format!("{m}::{method}"))
            }
        } else {
            method_key.clone()
        };
        let target = self
            .impl_method_declaration_ids
            .get(&c_symbol)
            .or_else(|| self.impl_method_declaration_ids.get(&method_key))
            .cloned()
            .map_or_else(
                || CallTarget::Unsupported {
                    reason: format!(
                        "primitive impl method `{c_symbol}` has no registered declaration identity"
                    ),
                },
                CallTarget::impl_method,
            );
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::PrimitiveTraitImpl {
                trait_name,
                canonical_receiver: canonical,
            },
        );
        if self.fn_sigs.contains_key(&method_key) || self.fn_sigs.contains_key(&c_symbol) {
            self.record_method_call_rewrite(
                span,
                MethodCallRewrite::RewriteToFunction {
                    target,
                    c_symbol,
                    // User-fn dispatch into a primitive trait impl
                    // (`i64::fmt` etc.) is open-set; the typed runtime-call
                    // catalog does not enumerate user-defined method keys.
                    descriptor: None,
                    extern_identity: None,
                    elem_ty: None,
                    // Primitive trait-impl dispatch is a user-fn call; it never
                    // consumes the receiver as a handle release.
                    consumes_receiver: sig.consumes_receiver,
                    returns_receiver_identity: sig.returns_receiver_identity,
                },
            );
        }
        // Project any `Self::Output`-style associated-type carrier in the
        // return now that the impl's type params are bound to the concrete
        // receiver — `project_assoc_types` keys the concrete builtin base
        // (`<Vec<i64> as Index>::Output`) through `impl_assoc_type_bindings`.
        Some(self.project_assoc_types(&applied_sig.return_type))
    }

    /// Instantiate a primitive/builtin-generic trait-impl method signature
    /// against a concrete receiver — **fallibly**.
    ///
    /// Dispatch reaches here keyed only on the canonical builtin kind
    /// (`Vec`/`HashMap`/…), so the registered impl's `Self` type must still be
    /// *proven* to structurally match the concrete receiver before its return
    /// type is treated as authoritative. Returns `None` when the impl does not
    /// apply (constrained/concrete/nested `Self` that the receiver does not
    /// satisfy), so the caller falls through to the normal "no matching
    /// method/impl" path and fails **closed** — never projecting an authoritative
    /// return type for a receiver that does not implement the impl.
    ///
    /// On a match, binds the impl's type parameters by structurally unifying the
    /// impl's recorded `Self` type arguments
    /// ([`Checker::primitive_trait_impl_self_args`]) against the receiver's
    /// concrete type arguments, then substitutes both `Self` (→ the receiver)
    /// and the bound parameters through the signature's params and return type.
    /// Substituted parameters are dropped from `type_params` so
    /// `apply_instantiated_call_signature` does not re-instantiate them as fresh
    /// inference vars. Mirrors `instantiate_named_method_sig` (the user
    /// `Ty::Named` path) for builtin receivers, which have no `type_defs` entry.
    ///
    /// Hew rejects overlapping impls (the associated-type binding table is keyed
    /// by type *name*, so a second impl for the same `(kind, trait)` collides and
    /// is diagnosed at registration for builtins and user records alike). There
    /// is therefore at most one usable impl per `(canonical, trait)`, and the
    /// stored `Self` args identify exactly that impl — no per-impl table needed.
    ///
    /// Non-generic primitive impls (`impl Display for i64`) have empty stored
    /// `Self` args and an argument-less receiver, so the arity check passes
    /// trivially, no parameters bind, and only the `Self` → receiver substitution
    /// applies — leaving existing dispatch behaviour unchanged.
    fn instantiate_primitive_trait_method_sig(
        &mut self,
        mut sig: FnSig,
        canonical: &str,
        trait_name: &str,
        receiver_ty: &Ty,
    ) -> Option<FnSig> {
        let mut subst: HashMap<String, Ty> = HashMap::new();
        // Clone out of the side table so the structural match can take `&mut
        // self` (it unifies unbound receiver vars against concrete `Self` args).
        if let Some(self_args) = self
            .primitive_trait_impl_self_args
            .get(&(canonical.to_string(), trait_name.to_string()))
            .cloned()
        {
            let receiver_args: Vec<Ty> = match receiver_ty {
                Ty::Named { args, .. } => args.clone(),
                _ => Vec::new(),
            };
            // Arity is part of the shape: an impl whose `Self` constructor takes
            // a different number of arguments than the receiver cannot apply.
            if self_args.len() != receiver_args.len() {
                return None;
            }
            let impl_params: HashSet<String> = sig.type_params.iter().cloned().collect();
            // Snapshot `self.subst` around the applicability probe: a multi-arg
            // `Self` (`HashMap<K, V>`) unifies each concrete arg into `self.subst`
            // as it matches, but a LATER arg may reject the impl. Without the
            // rollback an early concrete-arg unification would persist into the
            // checker's substitution past a `None` reject — binding an inference
            // var from an impl that does not actually apply. Restore on every
            // non-match so the probe is side-effect-free (hardening per the
            // A-general gate's non-blocking note).
            let subst_snapshot = self.subst.snapshot();
            for (self_arg, receiver_arg) in self_args.iter().zip(receiver_args.iter()) {
                if !self.match_self_arg_param(self_arg, receiver_arg, &impl_params, &mut subst) {
                    // `Self` does not structurally match the receiver — this impl
                    // does not apply. Roll back any partial unification, then fail
                    // closed.
                    self.subst.restore(subst_snapshot);
                    return None;
                }
            }
        }
        for param_ty in &mut sig.params {
            *param_ty = param_ty
                .substitute_named_param("Self", receiver_ty)
                .substitute_named_params_parallel(&subst);
        }
        sig.return_type = sig
            .return_type
            .substitute_named_param("Self", receiver_ty)
            .substitute_named_params_parallel(&subst);
        sig.type_params.retain(|tp| !subst.contains_key(tp));
        sig.type_param_bounds
            .retain(|tp, _| !subst.contains_key(tp));
        Some(sig)
    }

    /// Fallibly match a single impl `Self`-position type argument against the
    /// receiver's corresponding concrete argument, recording impl
    /// type-parameter bindings into `subst`. Returns `false` on any structural
    /// mismatch so the impl is rejected (fail closed) rather than over-applied.
    ///
    /// - A bare `Ty::Named { name, args: [] }` whose `name` is an impl type
    ///   parameter (`impl<E> … for Vec<E>` → `E`) binds to the resolved receiver
    ///   argument; a parameter appearing more than once (`HashMap<K, K>`) must
    ///   bind consistently.
    /// - A constructed nominal `Self` type (`Vec<T>` in `impl<T> Acc for
    ///   Vec<Vec<T>>`, or a concrete `Vec<i64>` / user `Point`) requires the
    ///   receiver to be the **same constructor** — identical `name`, `builtin`,
    ///   and arity — before recursing element-wise. This rejects
    ///   `Vec<Vec<T>>` ⊄ `Vec<Option<i64>>` (Vec ≠ Option) and `Vec<i64>` ⊄
    ///   `Vec<string>`.
    /// - A concrete non-`Named` leaf (`i64`, `string`, …) requires equality with
    ///   the resolved receiver argument.
    /// - In either concrete case, an unbound receiver `Ty::Var` is unified with
    ///   the fully-concrete `Self` argument, so inference can resolve the element
    ///   type when exactly one impl could apply.
    fn match_self_arg_param(
        &mut self,
        self_arg: &Ty,
        receiver_arg: &Ty,
        impl_params: &HashSet<String>,
        subst: &mut HashMap<String, Ty>,
    ) -> bool {
        let receiver_resolved = self.subst.resolve(receiver_arg);
        match self_arg {
            // Bare impl type parameter: bind to the receiver arg (consistently).
            Ty::Named { name, args, .. } if args.is_empty() && impl_params.contains(name) => {
                if let Some(existing) = subst.get(name) {
                    return *existing == receiver_resolved;
                }
                subst.insert(name.clone(), receiver_resolved);
                true
            }
            // Constructed / concrete-nominal `Self` type: same constructor, then
            // recurse. An unbound receiver var unifies with a fully-concrete one.
            Ty::Named {
                name: s_name,
                builtin: s_builtin,
                args: s_args,
            } => {
                if matches!(receiver_resolved, Ty::Var(_)) {
                    return !Self::ty_contains_impl_param(self_arg, impl_params)
                        && self.try_unify_with_owner_identity(&receiver_resolved, self_arg);
                }
                let Ty::Named {
                    name: r_name,
                    builtin: r_builtin,
                    args: r_args,
                } = &receiver_resolved
                else {
                    return false;
                };
                if s_name != r_name || s_builtin != r_builtin || s_args.len() != r_args.len() {
                    return false;
                }
                s_args
                    .iter()
                    .zip(r_args.iter())
                    .all(|(s, r)| self.match_self_arg_param(s, r, impl_params, subst))
            }
            // Concrete non-`Named` leaf (`i64`, `string`, `bool`, …): require
            // equality, or unify an unbound receiver var with the concrete leaf.
            concrete => {
                if matches!(receiver_resolved, Ty::Var(_)) {
                    return self.try_unify_with_owner_identity(&receiver_resolved, concrete);
                }
                receiver_resolved == *concrete
            }
        }
    }

    /// Whether `ty` mentions any of the impl's type parameters anywhere in its
    /// structure. Used to gate unifying an unbound receiver var against a `Self`
    /// argument: only fully-concrete `Self` args (no impl params) may drive
    /// inference of the receiver's element type.
    fn ty_contains_impl_param(ty: &Ty, impl_params: &HashSet<String>) -> bool {
        match ty {
            Ty::Named { name, args, .. } => {
                impl_params.contains(name)
                    || args
                        .iter()
                        .any(|a| Self::ty_contains_impl_param(a, impl_params))
            }
            _ => false,
        }
    }

    /// Stage A3: UFCS form of [`Self::try_dispatch_primitive_trait_method`].
    ///
    /// `Display::fmt(x)` registers `Display::fmt` in `fn_sigs` with the
    /// receiver param stripped (the `Trait::method` key triggers the
    /// is-method branch in `register_fn_sig_with_name`), so the call site
    /// would mis-arity (sig.params=[] vs args=[x]).  This helper detects
    /// the trait-qualified form, synthesizes the first arg as the
    /// receiver, and consults the same side table that powers
    /// receiver-form dispatch.  The first arg is type-checked against the
    /// canonical receiver kind itself; remaining args are type-checked
    /// against the registered sig's params (already receiver-stripped at
    /// registration time).
    ///
    /// Returns `None` when the receiver is not a primitive or builtin
    /// generic, or when no impl exists for the (kind, trait, method)
    /// triple.  The caller falls through to the existing trait-qualified
    /// dispatch in `calls.rs`, which keeps emitting today's diagnostics.
    pub(super) fn try_dispatch_ufcs_primitive_trait_method(
        &mut self,
        trait_name: &str,
        method_name: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        let first_arg = args.first()?;
        let trait_key = self.trait_ref_lookup_key(trait_name);
        // Short-circuit before synthesising first_arg: if trait_name has no
        // primitive impls registered at all, return immediately.  This
        // prevents the fallback trait-qualified path from synthesising
        // first_arg a second time when the helper was never going to handle
        // the dispatch.  Synthesis of first_arg is deferred to after this
        // guard so it only happens when there is a real chance we will own
        // the call.
        let has_primitive_impl = self
            .primitive_trait_impls
            .keys()
            .any(|(_, tn)| tn == &trait_key);
        if !has_primitive_impl {
            return None;
        }
        let (first_expr, first_sp) = first_arg.expr();
        // Synthesize the receiver arg's type so we can route to the
        // canonical primitive/builtin-generic key.
        let receiver_ty = self.synthesize(first_expr, first_sp);
        // Default `IntLiteral` / `FloatLiteral` receivers in UFCS form
        // (e.g. `Display::fmt(42)`) for the same reason as the method-form
        // path at try_dispatch_primitive_trait_method: without defaulting,
        // the synthesized receiver_ty is still in literal shape and
        // `canonical_primitive_or_builtin_key` returns `None`, causing the
        // caller to fall through to the trait-qualified path which then
        // mis-arities (the receiver-stripped sig has 0 params vs the 1 arg
        // we just synthesized).
        let resolved_receiver = self
            .subst
            .resolve(&receiver_ty)
            .materialize_literal_defaults();
        let canonical = Checker::canonical_primitive_or_builtin_key(&resolved_receiver)?;
        // Lookup keyed on the canonical receiver kind + trait name +
        // method.  We call into the table directly (not the
        // walk-every-trait helper) because the trait name is known at
        // this call site and there is no ambiguity to resolve.
        let sig = self
            .primitive_trait_impls
            .get(&(canonical.clone(), trait_key.clone()))
            .and_then(|methods| methods.get(method_name))
            .cloned()?;
        // Bind the impl's type params from the concrete receiver before
        // applying the signature, and prove `Self` matches (mirroring the
        // method-form path) so a UFCS call (`Index::get(v, i)`) on a generic
        // builtin receiver projects its `Output`/return instead of leaking an
        // unresolved inference var — and a constrained/concrete impl is not
        // over-applied. On a non-match, fall through to the trait-qualified
        // path, which fails closed.
        let sig = self.instantiate_primitive_trait_method_sig(
            sig,
            &canonical,
            &trait_key,
            &resolved_receiver,
        )?;
        // Do not add an outer check_arity here.  apply_instantiated_call_signature
        // already calls check_arity on trailing_args via PositionalOnly, matching
        // the receiver-form path at try_dispatch_primitive_trait_method.  An
        // outer check on all args (receiver + trailing) would fire a second
        // arity diagnostic for the same call — e.g. Display::fmt(x, extra)
        // would emit both "expected 1 arg" and "expected 0 trailing args".
        // Type-check remaining args against the (receiver-stripped)
        // params using the same machinery as method-form dispatch.
        let trailing_args = &args[1.min(args.len())..];
        let applied = self.apply_instantiated_call_signature(
            &sig,
            None,
            trailing_args,
            span,
            SignatureArgApplication::PositionalOnly {
                arity_context: format!("method `{trait_name}.{method_name}`"),
            },
            true,
            Some(GenericCallee::Method {
                type_name: &canonical,
                method: method_name,
                owner_type_args: &[],
            }),
        );
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::PrimitiveTraitImpl {
                trait_name: trait_name.to_string(),
                canonical_receiver: canonical,
            },
        );
        Some(self.project_assoc_types(&applied.return_type))
    }

    pub(super) fn check_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let result = self.check_method_call_inner(receiver, method, args, span);
        let key = SpanKey::in_module(span, self.current_module_idx);
        let runtime_rewrite_consumes_receiver = matches!(
            self.method_call_rewrites.get(&key),
            Some(MethodCallRewrite::RewriteToFunction {
                consumes_receiver: true,
                ..
            })
        );
        let builtin_option_result_consumes_receiver = matches!(
            self.method_call_rewrites.get(&key),
            Some(MethodCallRewrite::BuiltinOptionResult {
                method: OptionResultMethod::OptionUnwrap
                    | OptionResultMethod::OptionUnwrapOr
                    | OptionResultMethod::ResultUnwrap
                    | OptionResultMethod::ResultUnwrapOr,
            })
        );
        if runtime_rewrite_consumes_receiver || builtin_option_result_consumes_receiver {
            self.method_call_consumes_receiver.insert(key);
            if let Expr::Identifier(name) = &receiver.0 {
                // The typed consumption decision overrides a surface Copy
                // derivation. In particular, LambdaActorHandle is represented
                // by an empty stdlib nominal, but release still consumes its
                // sole runtime handle and any later receiver use is invalid.
                self.env.mark_moved(name, receiver.1.clone());
            }
        }
        self.record_resolved_method_call_ownership(receiver, method, args, span, &result);
        result
    }

    pub(super) fn check_dotted_type_member_call_against_expected(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        expected: &Ty,
        span: &Span,
    ) -> Option<Ty> {
        let head = self.resolve_dotted_type_head(receiver, method)?;
        let result = self.dispatch_dotted_type_member(
            &head,
            method,
            &DottedTypeMemberUse::Call {
                args,
                expected: Some(expected),
                span,
            },
        )?;
        self.mark_resolved_nominal_owner_used(&head.canonical_type);
        self.record_resolved_method_call_ownership(receiver, method, args, span, &result);
        Some(result)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "method ownership joins every resolved dispatch family at one authority seam"
    )]
    fn record_resolved_method_call_ownership(
        &mut self,
        receiver: &Spanned<Expr>,
        _method: &str,
        args: &[CallArg],
        span: &Span,
        result_ty: &Ty,
    ) {
        use crate::runtime_call::{
            ProducedArgumentBoundary as Boundary, ProducedValueAcquisition as Acquisition,
            ProducedValueOwnership as Ownership,
        };

        let key = SpanKey::in_module(span, self.current_module_idx);
        let resolved_result = self.subst.resolve(result_ty).materialize_literal_defaults();
        let non_owning = resolved_result.is_copy()
            || self
                .registry
                .implements_marker(&resolved_result, MarkerTrait::Copy);
        let rewrite = self.method_call_rewrites.get(&key);
        let dyn_call = self.dyn_trait_method_calls.get(&key);
        let resolved_call = self.resolved_calls.get(&key);
        let actor_call = self.actor_method_dispatch.get(&key);
        let machine_call = self.machine_method_dispatch.get(&key);
        let suspending_receiver = self
            .suspending_io_receiver_nominals
            .get(&key)
            .map(String::as_str);
        let is_suspending_io_delivery = (self.conn_await_reads.contains_key(&key)
            && suspending_receiver == Some(STD_NET_CONNECTION))
            || (self.listener_await_accepts.contains(&key)
                && suspending_receiver == Some(STD_NET_LISTENER));
        let runtime_family = match rewrite {
            Some(MethodCallRewrite::RewriteToFunction {
                descriptor: Some(descriptor),
                ..
            }) => Some(descriptor.family()),
            _ => None,
        };
        let display_method = self.lang_items.display_method_identity();
        let is_display_fmt = display_method.as_ref().is_some_and(|(_, display_method)| {
            let target = dyn_call.map(|call| &call.target).or(match rewrite {
                Some(MethodCallRewrite::StaticTraitDispatch { target, .. }) => Some(target),
                _ => None,
            });
            target.is_some_and(|target| {
                matches!(
                    target,
                    CallTarget::DynamicVtable { method, .. }
                        | CallTarget::StaticTraitMethod { method, .. }
                        if method == display_method
                )
            })
        });

        let result_ownership = if non_owning {
            Ownership::NoOwner
        } else if self.method_call_preserves_receiver_identity.contains(&key) {
            Ownership::Borrowed
        } else if matches!(
            rewrite,
            Some(
                MethodCallRewrite::RewriteToFunction {
                    returns_receiver_identity: true,
                    ..
                } | MethodCallRewrite::StaticTraitDispatch {
                    returns_receiver_identity: true,
                    ..
                }
            )
        ) || dyn_call.is_some_and(|call| call.signature.returns_receiver_identity)
        {
            Ownership::ReceiverIdentity
        } else if is_display_fmt && matches!(&resolved_result, Ty::String) {
            Ownership::owned(Acquisition::Fresh)
        } else if matches!(
            rewrite,
            Some(MethodCallRewrite::BuiltinOptionResult {
                method: OptionResultMethod::OptionUnwrap
                    | OptionResultMethod::OptionUnwrapOr
                    | OptionResultMethod::ResultUnwrap
                    | OptionResultMethod::ResultUnwrapOr,
            })
        ) {
            Ownership::owned(Acquisition::MoveOut)
        } else if matches!(
            actor_call,
            Some(ActorMethodKind::Ask(..) | ActorMethodKind::StreamProducer(..))
        ) || matches!(
            runtime_family,
            Some(
                crate::runtime_call::RuntimeCallFamily::ChannelRecvLayout
                    | crate::runtime_call::RuntimeCallFamily::ChannelTryRecvLayout
                    | crate::runtime_call::RuntimeCallFamily::StreamNextLayout
                    | crate::runtime_call::RuntimeCallFamily::StreamTryNextLayout
                    | crate::runtime_call::RuntimeCallFamily::DuplexRecv
                    | crate::runtime_call::RuntimeCallFamily::DuplexTryRecv
            )
        ) || is_suspending_io_delivery
        {
            Ownership::owned(Acquisition::Delivery)
        } else if let Some(call) = resolved_call {
            call.method_target.family.result_ownership()
        } else {
            match rewrite {
                Some(
                    MethodCallRewrite::BuiltinVecIntoIter { .. }
                    | MethodCallRewrite::BuiltinVecIter { .. }
                    | MethodCallRewrite::BuiltinHashMapIntoIter { .. }
                    | MethodCallRewrite::WireCodec { .. }
                    | MethodCallRewrite::GenericWireCodec { .. },
                ) => Ownership::owned(Acquisition::Fresh),
                Some(
                    MethodCallRewrite::BuiltinVecIterNext { .. }
                    | MethodCallRewrite::RecordCloneInplace { .. },
                ) => Ownership::owned(Acquisition::Clone),
                Some(
                    MethodCallRewrite::GeneratorNext { .. } | MethodCallRewrite::RemoteActorAsk,
                ) => Ownership::owned(Acquisition::Delivery),
                Some(MethodCallRewrite::RcIntrinsic { .. }) => {
                    Ownership::owned(Acquisition::Retained)
                }
                _ => match machine_call {
                    Some(MachineMethodKind::StateName { .. }) => {
                        Ownership::owned(Acquisition::Fresh)
                    }
                    Some(MachineMethodKind::Step { .. } | MachineMethodKind::TakeEmits { .. })
                    | None => Ownership::Unknown,
                },
            }
        };

        let signature = if let Some(call) = dyn_call {
            Some((
                format!("{}::{}", call.trait_name, call.method_name),
                call.signature.clone(),
            ))
        } else {
            match rewrite {
                Some(MethodCallRewrite::RewriteToFunction {
                    extern_identity: Some(identity),
                    ..
                }) => self
                    .fn_sigs
                    .get(&identity.signature_key)
                    .cloned()
                    .map(|sig| (identity.signature_key.clone(), sig)),
                Some(MethodCallRewrite::RewriteToFunction { c_symbol, .. }) => self
                    .fn_sigs
                    .get(c_symbol)
                    .cloned()
                    .map(|sig| (c_symbol.clone(), sig)),
                Some(MethodCallRewrite::StaticTraitDispatch {
                    declaring_trait,
                    method_name,
                    ..
                }) => {
                    let signature_key = format!("{declaring_trait}::{method_name}");
                    self.fn_sigs
                        .get(&signature_key)
                        .cloned()
                        .map(|sig| (signature_key, sig))
                }
                _ => None,
            }
        };
        let arguments = if matches!(
            rewrite,
            Some(MethodCallRewrite::BuiltinOptionResult {
                method: OptionResultMethod::OptionUnwrapOr | OptionResultMethod::ResultUnwrapOr,
            })
        ) {
            vec![Boundary::Transfer; args.len()]
        } else if let Some(family) = runtime_family {
            args.iter()
                .enumerate()
                .map(|(source_index, _)| {
                    match family.arg_consume_verdict(source_index.saturating_add(1)) {
                        crate::runtime_call::ConsumeVerdict::ProvenBorrow => Boundary::Borrow,
                        crate::runtime_call::ConsumeVerdict::ProvenConsume
                        | crate::runtime_call::ConsumeVerdict::ConservativeConsume => {
                            Boundary::Transfer
                        }
                    }
                })
                .collect()
        } else if let Some((signature_key, signature)) = signature {
            let modes = self
                .fn_param_ownership
                .get(&signature_key)
                .cloned()
                .unwrap_or_else(|| vec![Boundary::Unknown; signature.params.len()]);
            args.iter()
                .enumerate()
                .map(|(source_index, arg)| {
                    let formal_index = arg
                        .name()
                        .and_then(|name| {
                            signature
                                .param_names
                                .iter()
                                .position(|formal| formal == name)
                        })
                        .unwrap_or(source_index);
                    modes
                        .get(formal_index)
                        .copied()
                        .unwrap_or(Boundary::Unknown)
                })
                .collect()
        } else if let Some(call) = resolved_call {
            match call.method_target.family {
                MethodTargetFamily::HashMap(HashMapMethod::Insert)
                | MethodTargetFamily::HashSet(HashSetMethod::Insert)
                | MethodTargetFamily::Vec(VecMethod::Push | VecMethod::Set | VecMethod::Append) => {
                    vec![Boundary::Transfer; args.len()]
                }
                MethodTargetFamily::HashMap(_)
                | MethodTargetFamily::HashSet(_)
                | MethodTargetFamily::Vec(_) => vec![Boundary::Borrow; args.len()],
            }
        } else if actor_call.is_some() {
            vec![Boundary::Transfer; args.len()]
        } else {
            args.iter()
                .map(|arg| {
                    let arg_ty = self
                        .expr_types
                        .get(&SpanKey::in_module(&arg.expr().1, self.current_module_idx))
                        .map(|ty| self.subst.resolve(ty));
                    if arg_ty.as_ref().is_some_and(|ty| {
                        ty.is_copy() || self.registry.implements_marker(ty, MarkerTrait::Copy)
                    }) {
                        Boundary::Borrow
                    } else {
                        Boundary::Unknown
                    }
                })
                .collect()
        };

        let recognized = rewrite.is_some()
            || dyn_call.is_some()
            || resolved_call.is_some()
            || actor_call.is_some()
            || machine_call.is_some()
            || is_suspending_io_delivery;
        let receiver_boundary = if matches!(result_ownership, Ownership::ReceiverIdentity) {
            Some(Boundary::Transfer)
        } else if !recognized {
            Some(Boundary::Unknown)
        } else if self.method_call_consumes_receiver.contains(&key)
            || resolved_call.is_some_and(|call| call.method_target.consumes_receiver)
            || dyn_call.is_some_and(|call| call.signature.consumes_receiver)
            || matches!(
                rewrite,
                Some(
                    MethodCallRewrite::RewriteToFunction {
                        consumes_receiver: true,
                        ..
                    } | MethodCallRewrite::StaticTraitDispatch {
                        consumes_receiver: true,
                        ..
                    }
                )
            )
        {
            Some(Boundary::Transfer)
        } else {
            Some(Boundary::Borrow)
        };

        self.produced_call_arities
            .insert(key.clone(), (true, args.len()));
        self.resolved_method_call_ownership.insert(
            key,
            PendingMethodCallOwnership {
                fact: ProducedValueFact {
                    ownership: result_ownership,
                    receiver_span: matches!(result_ownership, Ownership::ReceiverIdentity)
                        .then(|| SpanKey::in_module(&receiver.1, self.current_module_idx)),
                    receiver_boundary,
                    arguments,
                },
                extern_identity: rewrite.and_then(|rewrite| match rewrite {
                    MethodCallRewrite::RewriteToFunction {
                        extern_identity, ..
                    } => extern_identity.clone(),
                    _ => None,
                }),
                resolved_result_ty: resolved_result,
            },
        );
    }

    #[expect(
        clippy::too_many_lines,
        reason = "pattern matching type checker with many variants"
    )]
    fn check_method_call_inner(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let dotted_type_head = self.resolve_dotted_type_head(receiver, method);
        if let Some(head) = dotted_type_head.as_ref() {
            if let Some(result) = self.dispatch_dotted_type_member(
                head,
                method,
                &DottedTypeMemberUse::Call {
                    args,
                    expected: None,
                    span,
                },
            ) {
                self.mark_resolved_nominal_owner_used(&head.canonical_type);
                return result;
            }
            let source_member = format!("{}.{method}", head.canonical_type);
            if !self.fn_sigs.contains_key(&source_member) {
                for arg in args {
                    let (expr, arg_span) = arg.expr();
                    self.synthesize(expr, arg_span);
                }
                self.report_error(
                    TypeErrorKind::UndefinedFunction,
                    span,
                    format!(
                        "undefined static function `{}.{method}`",
                        head.canonical_type
                    ),
                );
                return Ty::Error;
            }
        }
        // Module-qualified calls: e.g. http.listen(addr) → lookup "http.listen" in fn_sigs
        if let Expr::Identifier(name) = &receiver.0 {
            let receiver_is_binding = self.env.lookup_ref(name).is_some();
            // The shared type-head resolver already selected every
            // declaration-proven nominal above. Preserve that classification
            // while deciding whether an unresolved head is a module call.
            let receiver_is_known_type = dotted_type_head.is_some();
            let receiver_shadows_module = receiver_is_binding
                && self.module_import_bindings.contains_key(&(
                    self.current_module.clone(),
                    self.current_module_idx,
                    name.clone(),
                ));
            if receiver_shadows_module {
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::LexicalBinding {
                        binding_name: name.clone(),
                    },
                );
            }
            // `name` is a lexical import binding (`string`, or an explicit
            // module alias). Declaration and export registries are keyed by the
            // exact source owner, so resolve the binding before every authority
            // lookup. Never recover the owner from the final path segment: two
            // nested stdlib modules may share both a leaf and a function name.
            let canonical_owner = self.canonical_module_import_owner(name);
            let key = self.canonical_fn_identity(Some(&canonical_owner), method);
            let looks_like_module_call = !receiver_is_binding
                && !receiver_is_known_type
                && (self.module_binding_in_current_file(name)
                    || self.module_fn_exports.contains(&key)
                    || self.fn_sigs.contains_key(&key));
            if looks_like_module_call {
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::ModuleBinding {
                        module_name: canonical_owner.clone(),
                    },
                );
                if self.module_binding_in_current_file(name) {
                    self.used_modules.borrow_mut().insert(ImportKey::in_file(
                        self.current_module.clone(),
                        self.current_module_idx,
                        name.clone(),
                    ));
                }
                // Cross-module enum variant construction: e.g. `fs.IoError::TimedOut(0)`.
                // method contains "::" → treat as a qualified variant constructor rather than a
                // module function. Mirrors the lookup in check_call (calls.rs:407-465).
                if method.contains("::") {
                    let lifecycle_surface = format!("{name}.{method}");
                    let Ok(canonical_lifecycle) =
                        self.canonicalize_source_lifecycle_value_path(&lifecycle_surface, span)
                    else {
                        return Ty::Error;
                    };
                    let constructor_surface = canonical_lifecycle.as_deref().unwrap_or(method);
                    let constructor_match = self.lookup_variant_constructor(constructor_surface);
                    if let Some((type_name, expected_params, type_params)) = constructor_match {
                        let type_param_count = type_params.len();
                        let mut inferred_args = Vec::new();
                        while inferred_args.len() < type_param_count {
                            inferred_args.push(Ty::Var(TypeVar::fresh()));
                        }
                        self.check_arity(args, expected_params.len(), "this function", span);
                        {
                            let subst_map: HashMap<String, Ty> = type_params
                                .iter()
                                .zip(inferred_args.iter())
                                .map(|(p, a)| (p.clone(), a.clone()))
                                .collect();
                            for (i, arg) in args.iter().enumerate() {
                                if let Some(param_ty) = expected_params.get(i) {
                                    let (expr, sp) = arg.expr();
                                    let expected_ty = if subst_map.is_empty() {
                                        param_ty.clone()
                                    } else {
                                        param_ty.substitute_named_params_parallel(&subst_map)
                                    };
                                    self.check_against(expr, sp, &expected_ty);
                                }
                            }
                        }
                        let resolved_args: Vec<Ty> = inferred_args
                            .iter()
                            .map(|ty| self.subst.resolve(ty))
                            .collect();
                        return self.variant_nominal_ty(type_name, resolved_args);
                    }
                }
                if !self.module_fn_exports.contains(&key) {
                    // The function is not a `pub` export.  Determine why:
                    //   • no fn_visibility entry → truly unknown symbol
                    //   • fn_visibility entry, access denied → E_VISIBILITY
                    //   • fn_visibility entry, access allowed → `package fn` accessed
                    //     from within the same package; fall through to the success path.
                    if let Some(&vis) = self.fn_visibility.get(&key) {
                        // Materialise owned copies so the &self borrow from fn_def_spans
                        // is released before calling synthesize (&mut self).
                        let decl_module_owned =
                            self.fn_def_spans.get(&key).and_then(|(_, m)| m.clone());
                        let decl_span_owned = self
                            .fn_def_spans
                            .get(&key)
                            .map_or_else(|| span.clone(), |(s, _)| s.clone());
                        let acc_module_owned = self.current_module.clone();
                        let decl_module = decl_module_owned.as_deref();
                        if !visibility::access_allowed(
                            decl_module,
                            acc_module_owned.as_deref(),
                            vis,
                        ) {
                            // Access denied — synthesize args for error recovery and reject.
                            for arg in args {
                                let (expr, sp) = arg.expr();
                                self.synthesize(expr, sp);
                            }
                            let acc_module_str =
                                acc_module_owned.as_deref().unwrap_or("(root)").to_string();
                            let err = TypeError::visibility_violation(
                                vis,
                                span.clone(),
                                method,
                                decl_module.unwrap_or("(root)"),
                                &acc_module_str,
                                decl_span_owned,
                                acc_module_owned,
                            );
                            self.errors.push(err);
                            return Ty::Error;
                        }
                        // access_allowed returned true: `package fn` accessible from this
                        // package — fall through to the success path below.
                    } else {
                        // No visibility record: the function is genuinely unknown.
                        for arg in args {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                        let kind = if self.resolve_module_type(name, method).is_some() {
                            TypeErrorKind::PathKindMismatch
                        } else {
                            TypeErrorKind::PathMemberNotFound
                        };
                        self.report_error(
                            kind,
                            span,
                            format!("no function `{method}` in module `{name}`"),
                        );
                        return Ty::Error;
                    }
                }
                self.require_unsafe(&key, span);
                // Native-only stdlib modules are rejected on wasm32 because
                // their runtime implementations are not compiled there.
                // The manifest-generated module rejection slice is shared with
                // the value-position guard in expressions.rs.
                if let Some(feature) = self.wasm_native_only_module_feature(name) {
                    self.reject_wasm_feature(span, feature);
                }
                // Exact-function policy is separately keyed by canonical
                // source owner, so supported modules can retain native-only
                // member exclusions without weakening alias coverage.
                self.reject_wasm_native_only_module_function(name, method, span);
                // crypto.random_bytes and its fallible twin depend on a
                // native-only secure entropy source absent from the wasm32 link
                // set; reject so secure randomness fails closed on wasm32.
                if self.is_shipped_crypto_module(name)
                    && matches!(method, "random_bytes" | "try_random_bytes")
                {
                    self.reject_wasm_feature(span, WasmUnsupportedFeature::CryptoRandom);
                }
                if let Some(sig) = self.fn_sigs.get(&key).cloned() {
                    self.record_call_edge(&key);
                    self.record_module_qualified_stdlib_call_rewrite_if_any(name, method, span);
                    self.record_module_qualified_user_call_rewrite_if_any(name, method, span);
                    let assoc_bindings = self
                        .fn_type_param_assoc_bindings
                        .get(&key)
                        .cloned()
                        .unwrap_or_default();
                    let applied_sig = self.apply_instantiated_call_signature_with_assoc(
                        &sig,
                        &assoc_bindings,
                        None,
                        args,
                        span,
                        SignatureArgApplication::FunctionLike {
                            param_names: &sig.param_names,
                            accepts_kwargs: sig.accepts_kwargs,
                            module_qualified: true,
                        },
                        true,
                        Some(GenericCallee::Function { key: &key }),
                    );
                    if self.record_generic_wire_codec_rewrite(
                        &canonical_owner,
                        method,
                        &applied_sig.params,
                        &applied_sig.return_type,
                        span,
                    ) {
                        return applied_sig.return_type;
                    }
                    // Channel constructor: inject a shared type variable so
                    // Sender<T> and Receiver<T> from the same `new` call are
                    // linked through unification.
                    if canonical_owner == "std.channel" && method == "new" {
                        let t = Ty::Var(TypeVar::fresh());
                        return Ty::Tuple(vec![
                            Ty::Named {
                                name: "std.channel.Sender".to_string(),
                                args: vec![t.clone()],
                                builtin: Some(BuiltinType::Sender),
                            },
                            Ty::Named {
                                name: "std.channel.Receiver".to_string(),
                                args: vec![t],
                                builtin: Some(BuiltinType::Receiver),
                            },
                        ]);
                    }
                    if let Some(op) = self.intrinsic_math_generic_op_for_signature(&key) {
                        self.record_method_call_rewrite(
                            span,
                            MethodCallRewrite::GenericMathIntrinsic { op },
                        );
                    }
                    return applied_sig.return_type;
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::PathMemberNotFound,
                    span,
                    format!("no function `{method}` in module `{name}`"),
                );
                return Ty::Error;
            }

            // Static method calls on type names: e.g. Point.from_json(json)
            // Look up "TypeName.method" in fn_sigs (registered by wire types
            // etc.). The surface spelling resolves to its canonical
            // declaration identity first (A316): the wire codec surface
            // registers under `{module}.{Name}.{method}` only, so the bare
            // binding an import published (or an `as`-alias) must consult the
            // canonical key of ITS owner. The surface key remains the lookup
            // for root-canonical (bare) declarations and any non-wire dotted
            // signature registered under its own spelling.
            let canonical_static_owner = self
                .canonical_nominal_name(name)
                .unwrap_or_else(|| name.clone());
            let static_key = format!("{canonical_static_owner}.{method}");
            let static_sig = self.fn_sigs.get(&static_key).cloned().or_else(|| {
                (canonical_static_owner != *name)
                    .then(|| self.fn_sigs.get(&format!("{name}.{method}")).cloned())
                    .flatten()
            });
            if let Some(sig) = static_sig {
                self.check_arity(args, sig.params.len(), &format!("`{static_key}`"), span);
                for (i, arg) in args.iter().enumerate() {
                    if let Some(param_ty) = sig.params.get(i) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, param_ty);
                    }
                }
                let impl_key = format!("{name}::{method}");
                if let Some(declaration) = self.impl_method_declaration_ids.get(&impl_key).cloned()
                {
                    let target = CallTarget::ImplMethod(declaration);
                    self.record_method_call_rewrite(
                        span,
                        MethodCallRewrite::RewriteModuleQualifiedToFunction {
                            target: target.clone(),
                            c_symbol: impl_key.clone(),
                            elem_ty: None,
                        },
                    );
                    self.record_direct_call_target(span, target);
                    self.record_resolved_direct_call_ownership(
                        &impl_key,
                        &sig,
                        args,
                        &sig.return_type,
                        span,
                    );
                }
                // Wire codec static deserialize methods on a `#[wire]` struct or
                // enum. `decode` is the binary CBOR path
                // (`Type.decode(bytes) -> Type`); `from_json`/`from_yaml` are the
                // text path (`Type.from_json(string) -> Result<Type, string>`),
                // lowered through the CBOR↔text bridge. Each lives in `fn_sigs`
                // under the dotted `Type.<method>` key but records no rewrite
                // here without this arm, so the call would lower to
                // `MethodCallNoRewrite`. Record a dedicated codec rewrite so
                // HIR/codegen drive the matching thunk with the correct ABI.
                if self.wire_struct_types.contains(&canonical_static_owner)
                    || self.wire_enum_types.contains(&canonical_static_owner)
                {
                    let text_dir = match method {
                        "decode" => Some(WireCodecDirection::Decode),
                        "from_json" => Some(WireCodecDirection::FromJson),
                        "from_yaml" => Some(WireCodecDirection::FromYaml),
                        _ => None,
                    };
                    if let Some(direction) = text_dir {
                        // The codec `value_ty` is the produced wire type. For
                        // `decode` the registered return type IS that type; for
                        // `from_json`/`from_yaml` it is `Result<Self, string>`, so
                        // peel the `Ok` payload to recover the wire type codegen
                        // keys the thunk by.
                        let resolved_ret = self.subst.resolve(&sig.return_type);
                        let value_source = if direction == WireCodecDirection::Decode {
                            resolved_ret.clone()
                        } else {
                            result_ok_payload(&resolved_ret).unwrap_or_else(|| resolved_ret.clone())
                        };
                        if let Ok(value_ty) = ResolvedTy::from_ty(&value_source) {
                            self.record_method_call_rewrite(
                                span,
                                MethodCallRewrite::WireCodec {
                                    direction,
                                    value_ty,
                                },
                            );
                        }
                    }
                }
                return sig.return_type;
            }
        }

        let receiver_ty = self.synthesize(&receiver.0, &receiver.1);
        let resolved = self.subst.resolve(&receiver_ty);
        // If the receiver is still an unresolved inference variable that was
        // created from a coercible integer-literal / const-integer range (both
        // bounds were literals or let-/const-bound integer literals), eagerly
        // bind it to i64 before method dispatch.  Coercible-bounds ranges
        // produce a fresh TypeVar so that function-call use-sites can still
        // narrow the element type via unification (e.g. `fib(i: i32)` narrows
        // to i32 without going through a method call), but method dispatch
        // cannot drive unification from the receiver type alone.  The i64
        // default matches what `default_unconstrained_range_types` would apply
        // at the end of the inference pass, moved forward so receiver-only
        // numeric methods resolve correctly inside the loop body.
        let resolved = if let Ty::Var(v) = resolved {
            let is_int_range_var = self
                .deferred_range_bounds
                .iter()
                .any(|(_, dv, ..)| *dv == v);
            if is_int_range_var {
                self.subst
                    .insert(v, &Ty::I64)
                    .expect("binding integer range element var to i64 must stay acyclic");
                Ty::I64
            } else {
                Ty::Var(v)
            }
        } else {
            resolved
        };
        let resolved = match &resolved {
            Ty::Named {
                name,
                args: type_args,
                ..
            } if crate::method_resolution::lookup_named_method_sig(
                &self.type_defs,
                &self.fn_sigs,
                name,
                type_args,
                method,
            )
            .is_none() =>
            {
                self.alias_target_for_instance(name, type_args)
                    .unwrap_or(resolved)
            }
            _ => resolved,
        };
        if let Some((_, child_ty)) = resolved.as_supervisor_pool() {
            let kind = match method {
                "len" => crate::check::types::PoolAccessorKind::Len,
                "get" => crate::check::types::PoolAccessorKind::Get,
                _ => {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "supervisor pool has no method `{method}`; supported methods are \
                             `get(i)` and `len()`"
                        ),
                    );
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    return Ty::Error;
                }
            };
            self.pool_accessor_sites.insert(
                SpanKey::in_module(span, self.current_module_idx),
                crate::check::types::PoolAccessor { kind },
            );
            return match kind {
                crate::check::types::PoolAccessorKind::Len => {
                    self.check_arity(args, 0, "pool `len`", span);
                    Ty::I64
                }
                crate::check::types::PoolAccessorKind::Get => {
                    self.check_arity(args, 1, "pool `get`", span);
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, &Ty::I64);
                    }
                    Ty::option(Ty::child_ref(child_ty.clone()))
                }
                crate::check::types::PoolAccessorKind::Index => {
                    unreachable!("index access does not enter method checking")
                }
            };
        }
        self.reject_if_wasm_native_only_handle(&resolved, span);
        self.reject_if_wasm_blocking_semaphore_method(&resolved, method, span);
        if let Ty::Named { name, .. } = &resolved {
            // NEW-1: `await conn.read()` / `await conn.read_string()` is the
            // non-blocking suspending read. Record the inner method-call span so
            // HIR lowering emits `ConnAwaitRead` instead of the blocking method.
            // Recognised ONLY directly under an `await` (the suspend point); a
            // bare `conn.read()` stays the blocking FFI call (E8).
            let is_conn_await_read = self.inside_await_expr
                && name == "std.net.Connection"
                && matches!(method, "read" | "read_string");
            if is_conn_await_read {
                let key = SpanKey::in_module(span, self.current_module_idx);
                self.conn_await_reads
                    .insert(key.clone(), method == "read_string");
                self.suspending_io_receiver_nominals
                    .insert(key, name.clone());
            } else {
                // NEW-2: `await listener.accept()` is the non-blocking suspending
                // accept (the listener-readiness sibling of `await conn.read()`).
                // Record the inner method-call span so HIR lowering emits
                // `ListenerAwaitAccept` instead of the blocking `hew_tcp_accept`.
                // Recognised ONLY directly under an `await`; a bare
                // `listener.accept()` stays the blocking FFI call.
                let is_listener_await_accept =
                    self.inside_await_expr && name == "std.net.Listener" && method == "accept";
                if is_listener_await_accept {
                    let key = SpanKey::in_module(span, self.current_module_idx);
                    self.listener_await_accepts.insert(key.clone());
                    self.suspending_io_receiver_nominals
                        .insert(key, name.clone());
                } else {
                    // The blocking-call warning is correct for a bare (non-awaited)
                    // `conn.read()`; suppress it when the read is the non-blocking
                    // suspending form (it no longer strands a worker).
                    self.warn_if_blocking_handle_method(name, method, span);
                }
            }
        }
        // Structural clone admission is member-wise for tuples and built-in
        // value enums. Collection clones keep their existing runtime rewrites,
        // but pass through the same affine/member gate first.
        if method == "clone"
            && args.is_empty()
            && matches!(
                &resolved,
                Ty::Tuple(_)
                    | Ty::Named {
                        builtin: Some(_),
                        ..
                    }
            )
        {
            let is_structural_value = matches!(
                &resolved,
                Ty::Tuple(_)
                    | Ty::Named {
                        builtin: Some(BuiltinType::Option | BuiltinType::Result),
                        ..
                    }
            );
            if resolved.has_inference_var()
                && matches!(
                    &resolved,
                    Ty::Named {
                        builtin: Some(BuiltinType::Vec | BuiltinType::HashMap),
                        ..
                    }
                )
            {
                self.deferred_builtin_clone_admission
                    .entry(SpanKey::in_module(span, self.current_module_idx))
                    .or_insert_with(|| DeferredBuiltinCloneAdmission {
                        span: span.clone(),
                        receiver_ty: resolved.clone(),
                        source_module: self.current_module.clone(),
                    });
            }
            if let Some(blocker) = self.structural_clone_blocker(&resolved) {
                let receiver_name = resolved.user_facing().to_string();
                let rejected = match blocker {
                    CloneCapabilityBlocker::Affine {
                        type_name,
                        marker,
                        member,
                    } => {
                        self.report_affine_record_clone_error(
                            &receiver_name,
                            &type_name,
                            marker,
                            &member,
                            span,
                        );
                        true
                    }
                    CloneCapabilityBlocker::Opaque { type_name, member } if is_structural_value => {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "type `{receiver_name}` cannot be cloned because member `{member}` \
                                 contains opaque value `{type_name}`"
                            ),
                        );
                        true
                    }
                    CloneCapabilityBlocker::Missing { member, member_ty }
                        if is_structural_value =>
                    {
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "type `{receiver_name}` cannot be cloned because member `{member}` \
                                 of type `{}` has no Clone capability",
                                member_ty.user_facing()
                            ),
                        );
                        true
                    }
                    CloneCapabilityBlocker::UnbalancedSharedHandle { type_name, member } => {
                        self.report_unbalanced_shared_handle_clone_error(
                            &receiver_name,
                            &type_name,
                            &member,
                            span,
                        );
                        true
                    }
                    CloneCapabilityBlocker::Opaque { .. }
                    | CloneCapabilityBlocker::Missing { .. } => false,
                };
                if rejected {
                    return Ty::Error;
                }
            }
            if is_structural_value {
                self.record_method_call_rewrite(
                    span,
                    MethodCallRewrite::RecordCloneInplace {
                        record_name: resolved.user_facing().to_string(),
                    },
                );
                return resolved;
            }
        }

        match (&resolved, method) {
            (Ty::CancellationToken, "is_cancelled") => {
                self.check_arity(args, 0, "`CancellationToken.is_cancelled`", span);
                self.record_method_call_rewrite(
                    span,
                    MethodCallRewrite::CancellationTokenIsCancelled,
                );
                Ty::Bool
            }
            (Ty::CancellationToken, _) => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `CancellationToken`"),
                    self.similar_methods(&resolved, method),
                );
                Ty::Error
            }
            // Vec methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Vec),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_vec_method(type_args, &receiver_ty, &resolved, method, args, span),
            // HashMap methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::HashMap),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_hashmap_method(type_args, method, args, span),
            // HashSet methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::HashSet),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                // Preserve the receiver's original inference vars so a later non-literal insert
                // can refine an earlier `IntLiteral` element before we validate lowerability.
                let original_type_args = match &receiver_ty {
                    Ty::Named {
                        builtin: Some(BuiltinType::HashSet),
                        args,
                        ..
                    } => args.as_slice(),
                    _ => type_args,
                };
                self.check_hashset_method(original_type_args, method, args, span)
            }
            // Rc<T> methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Rc),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_rc_method(type_args, method, args, span),
            // Weak<T> methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Weak),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_weak_method(type_args, method, args, span),
            // instant receiver methods (`.elapsed()`, `.duration_since()`) are
            // declared in the `impl instant` block in `std/builtins.hew` with
            // monomorphic `#[extern_symbol(hew_instant_*)]` annotations, mirroring
            // the `Ty::Duration` arm below. `instant` is i64-backed at the MIR
            // boundary, so the receiver lowers as a bare `i64` nanos timestamp.
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Instant),
                    ..
                },
                _,
            ) => {
                if let Some(ret_ty) = self.dispatch_monomorphic_extern_symbol_method(
                    "instant",
                    &[],
                    method,
                    args,
                    span,
                ) {
                    return ret_ty;
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `instant`"),
                );
                Ty::Error
            }
            // bytes methods are declared in `std/io.hew` with monomorphic
            // `#[extern_symbol]` annotations over the current Vec<i32>-backed
            // bytes ABI.
            (Ty::Bytes, _) => {
                if let Some(ret_ty) =
                    self.dispatch_monomorphic_extern_symbol_method("bytes", &[], method, args, span)
                {
                    return ret_ty;
                }
                self.check_primitive_receiver_method_fallback(
                    &Ty::Bytes,
                    "`bytes`",
                    method,
                    args,
                    span,
                )
            }
            // Duration methods are declared in `std/builtins.hew` with
            // monomorphic `#[extern_symbol]` annotations.
            (Ty::Duration, _) => {
                if let Some(ret_ty) = self.dispatch_monomorphic_extern_symbol_method(
                    "duration",
                    &[],
                    method,
                    args,
                    span,
                ) {
                    return ret_ty;
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `duration`"),
                );
                Ty::Error
            }
            // Exact fallible numeric conversion: `.try_to_<W>() -> Option<W>`.
            (resolved, method) if resolved.is_numeric() && method.starts_with("try_to_") => {
                let suffix = &method["try_to_".len()..];
                let target_opt: Option<Ty> = match suffix {
                    "i8" => Some(Ty::I8),
                    "i16" => Some(Ty::I16),
                    "i32" => Some(Ty::I32),
                    "i64" => Some(Ty::I64),
                    "isize" => Some(Ty::Isize),
                    "u8" => Some(Ty::U8),
                    "u16" => Some(Ty::U16),
                    "u32" => Some(Ty::U32),
                    "u64" => Some(Ty::U64),
                    "usize" => Some(Ty::Usize),
                    "f32" => Some(Ty::F32),
                    "f64" => Some(Ty::F64),
                    _ => None,
                };
                if let Some(target) = target_opt {
                    let kind = match (resolved.is_integer(), target.is_integer()) {
                        (true, true) => TryConversionKind::IntToInt,
                        (false, true) => TryConversionKind::FloatToInt,
                        (true, false) => TryConversionKind::IntToFloat,
                        (false, false) => TryConversionKind::FloatToFloat,
                    };
                    self.try_width_cast_lowerings.insert(
                        SpanKey::in_module(span, self.current_module_idx),
                        TryWidthCastLowering {
                            from_ty: resolved.clone(),
                            to_ty: target.clone(),
                            kind,
                        },
                    );
                    Ty::option(target)
                } else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    let receiver_name = resolved.user_facing();
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "no method `{method}` on `{receiver_name}`; supported targets: \
                             i8, i16, i32, i64, isize, u8, u16, u32, u64, usize, f32, f64",
                        ),
                    );
                    Ty::Error
                }
            }
            // Explicit-wrap width reinterpretation: `.wrapping_as_<W>() -> W`.
            //
            // Admitted for all integer-to-integer pairs (any width, any sign).
            // Truncates / sign-extends / zero-extends bits per LLVM trunc/sext/zext.
            //
            // Guard: `wrapping_as_` must be checked BEFORE the arithmetic `wrapping_*`
            // arm so the suffix "as_<W>" does not fall through to the op-name matcher.
            (resolved, method) if resolved.is_integer() && method.starts_with("wrapping_as_") => {
                let suffix = &method["wrapping_as_".len()..];
                let target_opt: Option<Ty> = match suffix {
                    "i8" => Some(Ty::I8),
                    "i16" => Some(Ty::I16),
                    "i32" => Some(Ty::I32),
                    "i64" => Some(Ty::I64),
                    "isize" => Some(Ty::Isize),
                    "u8" => Some(Ty::U8),
                    "u16" => Some(Ty::U16),
                    "u32" => Some(Ty::U32),
                    "u64" => Some(Ty::U64),
                    "usize" => Some(Ty::Usize),
                    _ => None,
                };
                if let Some(target) = target_opt {
                    self.width_cast_lowerings.insert(
                        SpanKey::in_module(span, self.current_module_idx),
                        WidthCastLowering {
                            from_ty: resolved.clone(),
                            to_ty: target.clone(),
                            kind: WidthCastKind::Wrapping,
                        },
                    );
                    target
                } else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    let receiver_name = resolved.user_facing();
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "no method `{method}` on `{receiver_name}`; supported targets: \
                             i8, i16, i32, i64, isize, u8, u16, u32, u64, usize",
                        ),
                    );
                    Ty::Error
                }
            }
            // Saturating-clamp width conversion: `.saturating_as_<W>() -> W`.
            //
            // Admitted for all integer-to-integer pairs (any width, any sign).
            // Returns W::MAX on positive overflow, W::MIN on negative overflow.
            //
            // Guard: `saturating_as_` must be checked BEFORE the arithmetic `saturating_*`
            // arm so the suffix "as_<W>" does not fall through to the op-name matcher.
            (resolved, method) if resolved.is_integer() && method.starts_with("saturating_as_") => {
                let suffix = &method["saturating_as_".len()..];
                let target_opt: Option<Ty> = match suffix {
                    "i8" => Some(Ty::I8),
                    "i16" => Some(Ty::I16),
                    "i32" => Some(Ty::I32),
                    "i64" => Some(Ty::I64),
                    "isize" => Some(Ty::Isize),
                    "u8" => Some(Ty::U8),
                    "u16" => Some(Ty::U16),
                    "u32" => Some(Ty::U32),
                    "u64" => Some(Ty::U64),
                    "usize" => Some(Ty::Usize),
                    _ => None,
                };
                if let Some(target) = target_opt {
                    self.width_cast_lowerings.insert(
                        SpanKey::in_module(span, self.current_module_idx),
                        WidthCastLowering {
                            from_ty: resolved.clone(),
                            to_ty: target.clone(),
                            kind: WidthCastKind::Saturating,
                        },
                    );
                    target
                } else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    let receiver_name = resolved.user_facing();
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "no method `{method}` on `{receiver_name}`; supported targets: \
                             i8, i16, i32, i64, isize, u8, u16, u32, u64, usize",
                        ),
                    );
                    Ty::Error
                }
            }
            // Numeric opt-out arithmetic methods: .wrapping_*, .checked_*, .saturating_*
            // for every integer width. Floats are excluded (is_integer() ≠ is_numeric()).
            // Only add/sub/mul are in scope here; div/mod/shift are separate slices.
            // Wrapping variants map to non-trapping MIR ops; checked variants return
            // Option<W>; saturating variants clamp to MAX/MIN (codegen slice pending).
            //
            // Note: `.wrapping_as_<W>` and `.saturating_as_<W>` (width-conversion family)
            // are handled by the arms above; those arms must appear first so that the
            // `_as_` suffix does not reach this arm's op-name matcher.
            (resolved, method)
                if resolved.is_integer()
                    && (method.starts_with("wrapping_")
                        || method.starts_with("checked_")
                        || method.starts_with("saturating_")) =>
            {
                let is_wrapping = method.starts_with("wrapping_");
                let is_checked = method.starts_with("checked_");
                let family = if is_wrapping {
                    NumericMethodFamily::Wrapping
                } else if is_checked {
                    NumericMethodFamily::Checked
                } else {
                    NumericMethodFamily::Saturating
                };
                let op_name = if is_wrapping {
                    &method["wrapping_".len()..]
                } else if is_checked {
                    &method["checked_".len()..]
                } else {
                    &method["saturating_".len()..]
                };
                match op_name {
                    "add" | "sub" | "mul" => {
                        self.check_arity(args, 1, &format!("`{method}`"), span);
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            self.check_against(expr, sp, resolved);
                        }
                        let op = match op_name {
                            "add" => NumericMethodOp::Add,
                            "sub" => NumericMethodOp::Sub,
                            "mul" => NumericMethodOp::Mul,
                            _ => unreachable!("op_name matched add/sub/mul above"),
                        };
                        if let (Some(signedness), Some(width)) = (
                            Self::numeric_method_signedness(resolved),
                            Self::numeric_method_width(resolved),
                        ) {
                            let result_ty = if is_checked {
                                Ty::option(resolved.clone())
                            } else {
                                resolved.clone()
                            };
                            let prior = self.numeric_method_lowerings.insert(
                                SpanKey::in_module(span, self.current_module_idx),
                                NumericMethodLowering {
                                    family,
                                    op,
                                    result_ty: result_ty.clone(),
                                    operand_ty: resolved.clone(),
                                    signedness,
                                    width,
                                },
                            );
                            debug_assert!(
                                prior.is_none(),
                                "duplicate numeric method lowering for span {:?}",
                                SpanKey::in_module(span, self.current_module_idx)
                            );
                            result_ty
                        } else if is_checked {
                            Ty::option(resolved.clone())
                        } else {
                            resolved.clone()
                        }
                    }
                    _ => {
                        for arg in args {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "no method `{method}` on `{}`; only add, sub, mul are supported \
                                 in this family",
                                resolved.user_facing()
                            ),
                        );
                        Ty::Error
                    }
                }
            }
            // Local actor-reference methods first check the concrete reference
            // type's own impl, then fall through to actor receive-fn dispatch.
            //
            // `ChildRef<T>` and `LocalPid<T>` are distinct value representations;
            // their own methods are registered under their respective nominal
            // owners. Named receive handlers share the local dispatch path.
            (resolved, _) if resolved.as_local_actor_ref().is_some() => {
                let actor_ref_builtin = if resolved.as_child_ref().is_some() {
                    crate::BuiltinType::ChildRef
                } else {
                    crate::BuiltinType::LocalPid
                };
                // A user handler named `send` is actor dispatch; otherwise
                // `send` resolves through the reference type's own method.
                let has_user_send_handler = if method == "send" {
                    resolved.as_local_actor_ref().and_then(|inner| {
                        if let Ty::Named { name, .. } = inner {
                            Some(name.clone())
                        } else {
                            None
                        }
                    }).is_some_and(|actor_name| {
                        self.actor_receive_methods.contains(&format!("{actor_name}::send"))
                            || matches!(
                                self.resolve_bare_actor_identity(&actor_name),
                                BareActorResolution::Resolved(ref id) if self.actor_receive_methods.contains(&format!("{id}::send"))
                            )
                    })
                } else {
                    false
                };
                // A concrete `LocalPid<T>.send(msg)` with no user
                // `receive fn send` handler has no lowerable local-
                // mailbox delivery path (#2367). Declaring `impl
                // ActorMsg for T` records a message-envelope binding but
                // does not, by itself, wire delivery — no receive fn is
                // ever resolved to receive the message. Admitting this
                // case let it reach HIR lowering with no
                // `method_call_rewrites` / `actor_method_dispatch` entry
                // and fail closed there with an internal
                // `MethodCallNoRewrite` diagnostic instead of an
                // actionable one. Reject uniformly here, whether or not
                // the actor declares `impl ActorMsg` — same diagnostic
                // as the no-envelope case.
                if method == "send"
                    && !has_user_send_handler
                    && !self.checking_canonical_stdlib_source("std.builtins")
                {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    let actor_hint = resolved
                        .as_local_actor_ref()
                        .and_then(|inner| {
                            if let Ty::Named { name, .. } = inner {
                                Some(name.clone())
                            } else {
                                None
                            }
                        })
                        .unwrap_or_else(|| "this actor".to_string());
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "no `send` handler on `{actor_hint}` — declare \
                             `receive fn send(...)` to accept it, or call a \
                             named handler: `ref.method_name(payload)`"
                        ),
                    );
                    return Ty::Error;
                }
                // Try LocalPid's own methods first.
                if !has_user_send_handler {
                    if let Ty::Named {
                        args: receiver_args,
                        ..
                    } = resolved
                    {
                        if let Some(sig) = self.lookup_named_method_sig(
                            actor_ref_builtin.canonical_name(),
                            receiver_args,
                            method,
                        ) {
                            let applied_sig = self.apply_instantiated_call_signature(
                                &sig,
                                None,
                                args,
                                span,
                                SignatureArgApplication::PositionalOnly {
                                    arity_context: format!("method `{method}`"),
                                },
                                true,
                                Some(GenericCallee::Method {
                                    type_name: actor_ref_builtin.canonical_name(),
                                    method,
                                    owner_type_args: receiver_args,
                                }),
                            );
                            if method == "send"
                                && !self.checking_canonical_stdlib_source("std.builtins")
                            {
                                self.enforce_actor_method_send_args(args);
                            }
                            return applied_sig.return_type;
                        }
                    }
                }
                // Fall through to actor receive-fn dispatch on the inner type.
                let inner = resolved.as_local_actor_ref().unwrap();
                if let Ty::Named {
                    name: actor_name, ..
                } = inner
                {
                    // An annotation-derived `LocalPid<Account>` carries the
                    // bare inner name; resolve it to the registered actor
                    // identity (current module's actor, root actor, or a
                    // unique module export) before keying `fn_sigs`. Spawn-
                    // derived handles already carry the dotted identity.
                    let actor_identity = if self
                        .fn_sigs
                        .contains_key(&format!("{actor_name}::{method}"))
                    {
                        actor_name.clone()
                    } else if let BareActorResolution::Resolved(identity) =
                        self.resolve_bare_actor_identity(actor_name)
                    {
                        identity
                    } else {
                        actor_name.clone()
                    };
                    let method_key = format!("{actor_identity}::{method}");
                    // A plain (non-receive) `fn` on the actor lands in `fn_sigs` under the
                    // same `{identity}::{method}` key as a `receive fn` handler (see
                    // `register_actor_base`), but only `register_receive_fn` adds to
                    // `actor_receive_methods`. A key present in the former but absent from
                    // the latter names an internal method with no mailbox-handler shape —
                    // MIR has no `ActorHandlerLayout` row for it (#2366). Reject here,
                    // fail-closed, instead of deferring to a MIR NotYetImplemented.
                    if self.fn_sigs.contains_key(&method_key)
                        && !self.actor_receive_methods.contains(&method_key)
                    {
                        for arg in args {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "`{method}` is an internal actor method, not a message \
                                 handler — declare it `receive fn` to expose it"
                            ),
                        );
                        return Ty::Error;
                    }
                    if let Some(sig) = self.fn_sigs.get(&method_key).cloned() {
                        // Route through the one application authority rather
                        // than checking args against `sig.params` directly: a
                        // generic `receive fn keep<T>(..)` needs its type
                        // parameters freshened and inferred from the arguments,
                        // and its instantiation recorded so structural-equality
                        // obligations raised in the handler body are discharged.
                        // The hand-rolled loop that used to live here skipped
                        // both, so a generic handler reported `expected T` at
                        // every call site.
                        let applied_sig = self.apply_instantiated_call_signature(
                            &sig,
                            None,
                            args,
                            span,
                            SignatureArgApplication::PositionalOnly {
                                arity_context: format!("method `{method}`"),
                            },
                            true,
                            Some(GenericCallee::Method {
                                type_name: &actor_identity,
                                method,
                                owner_type_args: &[],
                            }),
                        );
                        // Every argument crosses the mailbox boundary. This is
                        // the funnel-compatible pairing (it reads the per-arg
                        // types the application just published) used by the bare
                        // actor-instance dispatch arm.
                        self.enforce_actor_method_send_args(args);
                        self.record_method_call_receiver_kind(
                            span,
                            MethodCallReceiverKind::ActorInstance {
                                actor_name: actor_identity.clone(),
                            },
                        );
                        // Ask-without-await guard: ask-shaped receive fn must be
                        // awaited. Generator methods (`receive gen fn`) use `for
                        // await` at the call site and are exempt from this guard.
                        let resolved_ret = self.subst.resolve(&applied_sig.return_type);
                        if !matches!(resolved_ret, Ty::Unit)
                            && !self.receive_generator_methods.contains(&method_key)
                            && !self.inside_await_expr
                        {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!(
                                    "actor ask `{actor_identity}.{method}` requires `await`; \
                                     write `let v? = await ref.{method}(...)` \
                                     or `match await ref.{method}(...) {{ Ok(v) => ..., Err(e) => ... }}`",
                                ),
                            );
                        }
                        let call_ty = self.record_actor_method_dispatch(
                            span,
                            method_key,
                            applied_sig.return_type.clone(),
                        );
                        return call_ty;
                    }
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{}`", resolved.user_facing()),
                    self.similar_methods(resolved, method),
                );
                Ty::Error
            }
            // RemotePid<T> methods — dispatch to RemotePid's own impl methods.
            //
            // RemotePid does NOT fall through to actor receive-fn dispatch; it is
            // a distinct remote type that cannot dispatch local actor methods.
            (resolved, _) if resolved.as_remote_pid().is_some() => {
                if let Ty::Named {
                    args: receiver_args,
                    ..
                } = resolved
                {
                    if let Some(sig) = self.lookup_named_method_sig(
                        crate::BuiltinType::RemotePid.canonical_name(),
                        receiver_args,
                        method,
                    ) {
                        let applied_sig = self.apply_instantiated_call_signature(
                            &sig,
                            None,
                            args,
                            span,
                            SignatureArgApplication::PositionalOnly {
                                arity_context: format!("method `{method}`"),
                            },
                            true,
                            Some(GenericCallee::Method {
                                type_name: crate::BuiltinType::RemotePid.canonical_name(),
                                method,
                                owner_type_args: receiver_args,
                            }),
                        );
                        let return_type = if method == "ask" {
                            self.project_assoc_types(&applied_sig.return_type)
                        } else {
                            applied_sig.return_type.clone()
                        };
                        if matches!(method, "send" | "ask")
                            && !self.checking_canonical_stdlib_source("std.builtins")
                        {
                            // `RemotePid<T>::send` / `::ask` route to the native
                            // mesh transport (`hew_remote_pid_send` →
                            // `hew_actor_send_by_id`), which is not compiled for
                            // wasm32. Reject at check time so remote messaging
                            // fails closed with a structured diagnostic instead
                            // of compiling to a module that imports undefined
                            // native send symbols and traps at instantiation.
                            self.reject_wasm_feature(span, WasmUnsupportedFeature::Distributed);
                            self.enforce_actor_method_send_args(args);
                            // A640/S3: this is only a compile-time floor. The
                            // native RemotePid lowering still wraps raw
                            // in-memory ABI bytes in the CBOR envelope; a
                            // structural Hew-value encoder is a later slice.
                            self.enforce_remote_actor_method_serializable_args(args);
                            if method == "ask" {
                                self.enforce_remote_actor_ask_reply_serializable(
                                    &return_type,
                                    span,
                                );
                                self.method_call_rewrites.insert(
                                    SpanKey::in_module(span, self.current_module_idx),
                                    MethodCallRewrite::RemoteActorAsk,
                                );
                            }
                        }
                        if method == "send" {
                            // S5: real RemotePid<T>::send lowering. Record a
                            // direct-call rewrite so HIR/MIR lower the call
                            // to `hew_remote_pid_send`, which codegen
                            // intercepts and lowers to the
                            // `hew_actor_send_by_id` runtime ABI plus a
                            // `Result<(), SendError>` construction. The
                            // catalog entry registers the FFI shape; the
                            // codegen Terminator::Call branch consumes the
                            // resolved receiver + msg arg types from the
                            // checker output (no re-inference in codegen
                            // per the `checker-authority` invariant).
                            self.method_call_rewrites.insert(
                                SpanKey::in_module(span, self.current_module_idx),
                                MethodCallRewrite::RewriteToFunction {
                                    target: CallTarget::Runtime(
                                        crate::runtime_call::RuntimeCallFamily::RemotePidSend,
                                    ),
                                    c_symbol: "hew_remote_pid_send".to_string(),
                                    // Closed runtime call dispatched by callee-
                                    // name intercept in codegen; the substrate
                                    // enumerates this family.
                                    descriptor: Some(
                                        crate::runtime_call::RuntimeCallDescriptor::new(
                                            crate::runtime_call::RuntimeCallFamily::RemotePidSend,
                                            None,
                                        )
                                        .expect("RemotePidSend rejects elem"),
                                    ),
                                    extern_identity: None,
                                    elem_ty: None,
                                    // Fire-and-forget send; borrows the pid
                                    // handle, does not release it.
                                    consumes_receiver: false,
                                    returns_receiver_identity: false,
                                },
                            );
                        }
                        if let Some(c_symbol) = match method {
                            "location" => Some("hew_remote_pid_location"),
                            "node_id" => Some("hew_remote_pid_node_id"),
                            "slot" => Some("hew_remote_pid_slot"),
                            "incarnation" => Some("hew_remote_pid_incarnation"),
                            "display" => Some("hew_remote_pid_display"),
                            _ => None,
                        } {
                            self.record_runtime_method_call_rewrite(span, c_symbol);
                        }
                        return return_type;
                    }
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{}`", resolved.user_facing()),
                    self.similar_methods(resolved, method),
                );
                Ty::Error
            }
            // Duplex<S, R>: bidirectional channel handle (raw channel substrate
            // from `duplex` / `duplex_pair`).
            //
            // Methods: .send(msg) / .recv() / .try_send() / .try_recv() /
            //          .send_half() / .recv_half() / .close()
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Duplex),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_duplex_method(type_args, &receiver_ty, receiver, method, args, span),
            // LambdaPid<M, R>: lambda-actor handle.
            //
            // Methods: .send(msg) / .close()
            //
            // Call-syntax `handle(msg)` is the canonical lambda-actor surface;
            // `.send(msg)` is an allowed-secondary tell surface. A lambda actor
            // is NOT a channel: it has no `.recv()` / `.try_recv()` /
            // `.send_half()` / `.recv_half()` surface (the caller never reads the
            // mailbox, and an actor cannot be split in two). The reply (for an
            // ask-shaped actor) is delivered through the call-site Result, never a
            // separate `.recv()`.
            (
                Ty::Named {
                    builtin: Some(BuiltinType::LambdaPid),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                self.check_lambda_pid_method(type_args, &receiver_ty, receiver, method, args, span)
            }
            // SendHalf<S>: send-direction half of a split Duplex<S, R>.
            //
            // Methods: .send(msg) / .close()
            // Produced by `Duplex<S, R>::send_half()`.
            (
                Ty::Named {
                    builtin: Some(BuiltinType::SendHalf),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_send_half_method(type_args, receiver, method, args, span),
            // RecvHalf<R>: receive-direction half of a split Duplex<S, R>.
            //
            // Methods: .recv() / .close()
            // Produced by `Duplex<S, R>::recv_half()`.
            (
                Ty::Named {
                    builtin: Some(BuiltinType::RecvHalf),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_recv_half_method(type_args, receiver, method, args, span),
            // String methods are declared in `std/string.hew` with
            // monomorphic `#[extern_symbol]` annotations.
            (Ty::String, _) => self.dispatch_string_method(method, args, span),
            // Generator methods route through the Iterator contract:
            // .next() returns Option<yielded type>.
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Generator | BuiltinType::AsyncGenerator),
                    args: type_args,
                    ..
                },
                "next",
            ) => {
                let yield_ty = type_args
                    .first()
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh()));
                // Record the consumption rewrite so HIR lowering emits the
                // dedicated `GeneratorNext` node (codegen drives `hew_gen_next`
                // and unboxes the result into `Option<yield_ty>`). Without this
                // entry, HIR rejects the call with `MethodCallNoRewrite`.
                //
                // `materialize_literal_defaults` collapses any residual
                // `IntLiteral`/`FloatLiteral` yield type to its concrete default
                // (i64/f64): a `gen { yield 7; 0 }` yields an unconstrained
                // integer literal, and `ResolvedTy::from_ty` rejects an
                // unmaterialized literal — so without the default the rewrite
                // would be silently skipped and HIR would reject the call.
                let resolved_yield = self.subst.resolve(&yield_ty).materialize_literal_defaults();
                if let Ok(yield_resolved) = ResolvedTy::from_ty(&resolved_yield) {
                    self.record_method_call_rewrite(
                        span,
                        MethodCallRewrite::GeneratorNext {
                            yield_ty: yield_resolved,
                        },
                    );
                }
                Ty::option(yield_ty)
            }
            // Stream<T> methods
            //
            // LIMITATION: Stream element-type validation only triggers here (on
            // method resolution).  A function parameter typed `Stream<MyStruct>`
            // passes typecheck if no stream methods are called on it.  Ideally
            // we would reject unsupported element types in resolve_type_expr when
            // the Stream<T> type is first formed, but that requires propagating
            // the span and restructuring the named-type resolution path.  For
            // now codegen will fail if the type is actually used.
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Stream),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                // Stream<T> methods are not supported on wasm32: the stream
                // runtime module is not compiled for wasm32.
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Streams);
                self.check_stream_method(type_args, method, args, span)
            }
            // Sink<T> methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Sink),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                let Some(inner) = self.validate_stream_sink_element_type(
                    type_args,
                    BuiltinNamedType::Sink.canonical_name(),
                    method,
                    span,
                ) else {
                    return Ty::Error;
                };
                if method == "encode" {
                    return self.report_unlowerable_stream_codec_boundary(
                        BuiltinNamedType::Sink.canonical_name(),
                        &inner,
                        method,
                        span,
                    );
                }
                // Gate 2: lowering-capability check.  Only string and bytes have
                // runtime symbols; other Wire-capable types pass gate 1 but cannot
                // be lowered yet.  Emit a user-facing diagnostic rather than the
                // ICE-flavoured "missing runtime rewrite metadata" from
                // require_builtin_runtime_symbol.
                let resolved_inner = self.subst.resolve(&inner);
                if !matches!(resolved_inner, Ty::Var(_))
                    && !self.queue_elem_admissible(&resolved_inner)
                {
                    let reason = self.queue_elem_rejection_reason(&resolved_inner);
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("`Sink<{}>` is not supported: {reason}", inner.user_facing()),
                    );
                    return Ty::Error;
                }
                let receiver_ty = Ty::sink(inner.clone());
                match method {
                    // Channel-family naming: .send() is the fundamental send
                    // surface. string/bytes elements keep the platform byte-sink
                    // writes (`hew_sink_write_*` — the bytes form carries the
                    // suspendable backpressure ramp); every other describable
                    // element rides the typed-serialise layout entry
                    // `hew_stream_send_layout`, which the runtime accepts on
                    // in-memory channel sinks (fail-closed on byte sinks for
                    // owned elements). .try_send() keeps the non-blocking
                    // string/bytes writes; a non-blocking typed send entry does
                    // not exist yet, so widened-element try_send fails closed
                    // with a specific diagnostic.
                    // .write() is an I/O-flavoured alias for .send(), routing to
                    // the same hew_sink_write_* symbols; it is accepted as a
                    // secondary surface on file/socket sinks.
                    "send" | "try_send" | "write" => {
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Sink.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            if let Some(param_ty) = sig.params.first() {
                                self.check_against(expr, sp, param_ty);
                            }
                        }
                        let resolved_inner = self.subst.resolve(&inner);
                        let element_name = Self::runtime_stream_element_name(&resolved_inner);
                        let c_symbol = if element_name.is_some() {
                            let Some(c_symbol) = self.require_builtin_runtime_symbol(
                                span,
                                BuiltinNamedType::Sink.canonical_name(),
                                method,
                                crate::stdlib::resolve_stream_method(
                                    BuiltinNamedType::Sink.canonical_name(),
                                    method,
                                    element_name,
                                ),
                            ) else {
                                return Ty::Error;
                            };
                            c_symbol
                        } else if matches!(method, "send" | "write") {
                            "hew_stream_send_layout"
                        } else {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!(
                                    "`try_send` is not available on `Sink<{}>` yet: the \
                                     typed element path has no non-blocking send runtime \
                                     entry — use `send` (blocking, backpressure-aware)",
                                    inner.user_facing()
                                ),
                            );
                            return Ty::Error;
                        };
                        self.record_runtime_method_call_rewrite(span, c_symbol);
                        sig.return_type
                    }
                    "close" => {
                        let Some(c_symbol) = self.require_builtin_runtime_symbol(
                            span,
                            BuiltinNamedType::Sink.canonical_name(),
                            method,
                            crate::stdlib::resolve_stream_method(
                                BuiltinNamedType::Sink.canonical_name(),
                                method,
                                None,
                            ),
                        ) else {
                            return Ty::Error;
                        };
                        self.record_runtime_method_call_rewrite(span, c_symbol);
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Sink.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        sig.return_type
                    }
                    _ => {
                        for arg in args {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!("no method `{method}` on `{}`", resolved.user_facing()),
                            self.similar_methods(&receiver_ty, method),
                        );
                        Ty::Error
                    }
                }
            }
            // Sender<T> methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Sender),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                let inner = type_args
                    .first()
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh()));
                let receiver_ty = Ty::sender(inner.clone());
                let resolved_inner = self.subst.resolve(&inner);
                match method {
                    "send" => {
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Sender.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        if let Some(arg) = args.first() {
                            let (expr, sp) = arg.expr();
                            if let Some(param_ty) = sig.params.first() {
                                self.check_against(expr, sp, param_ty);
                            }
                        }
                        // Validate after unification so the concrete type is known.
                        let resolved_inner = self.subst.resolve(&inner);
                        if !matches!(resolved_inner, Ty::Var(_))
                            && !self.queue_elem_admissible(&resolved_inner)
                        {
                            let reason = self.queue_elem_rejection_reason(&resolved_inner);
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!("Channel<{resolved_inner}> is not supported: {reason}"),
                            );
                            return Ty::Error;
                        }
                        if matches!(resolved_inner, Ty::Var(_)) {
                            // Inner type is still unresolved after argument
                            // unification — the constraint may arrive from the
                            // call-site's surrounding context (e.g.
                            // `let _: () = tx.send(v)` where `v: int` is
                            // declared elsewhere).  Defer the symbol selection
                            // until post-inference drain.
                            self.record_deferred_channel_method_rewrite(
                                span,
                                BuiltinNamedType::Sender.canonical_name(),
                                method,
                                inner.clone(),
                            );
                        } else {
                            let Some(c_symbol) = self.require_builtin_runtime_symbol(
                                span,
                                BuiltinNamedType::Sender.canonical_name(),
                                method,
                                crate::stdlib::resolve_channel_method(
                                    BuiltinNamedType::Sender.canonical_name(),
                                    method,
                                    Some(&resolved_inner),
                                ),
                            ) else {
                                return Ty::Error;
                            };
                            self.record_runtime_method_call_rewrite(span, c_symbol);
                        }
                        sig.return_type
                    }
                    "clone" | "close" => {
                        let Some(c_symbol) = self.require_builtin_runtime_symbol(
                            span,
                            BuiltinNamedType::Sender.canonical_name(),
                            method,
                            crate::stdlib::resolve_channel_method(
                                BuiltinNamedType::Sender.canonical_name(),
                                method,
                                Some(&resolved_inner),
                            ),
                        ) else {
                            return Ty::Error;
                        };
                        self.record_runtime_method_call_rewrite(span, c_symbol);
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Sender.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        sig.return_type
                    }
                    _ => {
                        self.check_named_method_fallback(&resolved, method, args, span, "Sender<T>")
                    }
                }
            }
            // Receiver<T> methods
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Receiver),
                    args: type_args,
                    ..
                },
                _,
            ) => {
                let inner = type_args
                    .first()
                    .cloned()
                    .unwrap_or(Ty::Var(TypeVar::fresh()));
                let receiver_ty = Ty::receiver(inner.clone());
                let resolved_inner = self.subst.resolve(&inner);
                if !matches!(resolved_inner, Ty::Var(_))
                    && !self.queue_elem_admissible(&resolved_inner)
                {
                    let reason = self.queue_elem_rejection_reason(&resolved_inner);
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("Channel<{resolved_inner}> is not supported: {reason}"),
                    );
                    return Ty::Error;
                }
                match method {
                    "recv" => {
                        self.reject_wasm_feature(span, WasmUnsupportedFeature::BlockingChannelRecv);
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Receiver.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        if !self.inside_await_expr {
                            self.warn_if_blocking_in_receive_fn("Receiver.recv", span);
                        }
                        if matches!(resolved_inner, Ty::Var(_)) {
                            // No argument to unify against — the return-type
                            // constraint (e.g. `let v: int = rx.recv()`) is
                            // applied by the caller *after* this arm returns.
                            // Defer the C-symbol selection until
                            // post-inference drain.
                            self.record_deferred_channel_method_rewrite(
                                span,
                                BuiltinNamedType::Receiver.canonical_name(),
                                method,
                                inner.clone(),
                            );
                        } else {
                            let Some(c_symbol) = self.require_builtin_runtime_symbol(
                                span,
                                BuiltinNamedType::Receiver.canonical_name(),
                                method,
                                crate::stdlib::resolve_channel_method(
                                    BuiltinNamedType::Receiver.canonical_name(),
                                    method,
                                    Some(&resolved_inner),
                                ),
                            ) else {
                                return Ty::Error;
                            };
                            self.record_runtime_method_call_rewrite(span, c_symbol);
                        }
                        sig.return_type
                    }
                    "try_recv" => {
                        if matches!(resolved_inner, Ty::Var(_)) {
                            self.record_deferred_channel_method_rewrite(
                                span,
                                BuiltinNamedType::Receiver.canonical_name(),
                                method,
                                inner.clone(),
                            );
                        } else {
                            let Some(c_symbol) = self.require_builtin_runtime_symbol(
                                span,
                                BuiltinNamedType::Receiver.canonical_name(),
                                method,
                                crate::stdlib::resolve_channel_method(
                                    BuiltinNamedType::Receiver.canonical_name(),
                                    method,
                                    Some(&resolved_inner),
                                ),
                            ) else {
                                return Ty::Error;
                            };
                            self.record_runtime_method_call_rewrite(span, c_symbol);
                        }
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Receiver.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        sig.return_type
                    }
                    "close" => {
                        // `close` maps to a single type-independent symbol.
                        let Some(c_symbol) = self.require_builtin_runtime_symbol(
                            span,
                            BuiltinNamedType::Receiver.canonical_name(),
                            method,
                            crate::stdlib::resolve_channel_method(
                                BuiltinNamedType::Receiver.canonical_name(),
                                method,
                                Some(&resolved_inner),
                            ),
                        ) else {
                            return Ty::Error;
                        };
                        self.record_runtime_method_call_rewrite(span, c_symbol);
                        let Some(sig) = self.require_builtin_method_sig(
                            span,
                            &receiver_ty,
                            BuiltinNamedType::Receiver.canonical_name(),
                            method,
                        ) else {
                            return Ty::Error;
                        };
                        sig.return_type
                    }
                    _ => self.check_named_method_fallback(
                        &resolved,
                        method,
                        args,
                        span,
                        "Receiver<T>",
                    ),
                }
            }
            // Range<T> iterator adapters: `.rev()` (descending iteration) and
            // `.step_by(k)` (strided iteration).  Both return `Range<T>` so they
            // compose (`(0..=10).rev().step_by(3)`) and feed the for-loop's
            // `Range<T>` element-type extraction unchanged.  Crucially the
            // returned type reuses the receiver's element `T` (not a fresh var),
            // so the #1857 `deferred_range_bounds` i64-defaulting still resolves
            // an unconstrained `(0..n).rev()` exactly as a bare range would.
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Range),
                    args: range_args,
                    ..
                },
                "rev" | "step_by",
            ) => {
                let elem_ty = range_args.first().cloned().unwrap_or(Ty::I64);
                let range_ty = Ty::range(elem_ty.clone());
                if method == "rev" {
                    self.check_arity(args, 0, "`Range.rev`", span);
                } else {
                    self.check_arity(args, 1, "`Range.step_by`", span);
                    if let Some(arg) = args.first() {
                        let (expr, sp) = arg.expr();
                        // The stride is counted in the range's element type so a
                        // `for i in (0u32..10).step_by(2)` strides in `u32`.
                        let _step_ty = self.check_against(expr, sp, &elem_ty);
                        // Fail-closed: reject a statically-known non-positive
                        // step at compile time.  A zero step would spin forever
                        // and a negative step is meaningless for an unsigned
                        // stride magnitude; `.rev()` is the descending form.
                        // A non-literal step is validated at runtime (MIR traps
                        // on a zero step before entering the loop).
                        if let Some(value) = Self::literal_integer_value(expr) {
                            if value <= 0 {
                                self.report_error(
                                    TypeErrorKind::InvalidOperation,
                                    span,
                                    format!(
                                        "`step_by` requires a positive step; `{value}` is not \
                                         allowed (use `.rev()` for descending iteration)"
                                    ),
                                );
                            }
                        }
                    }
                }
                range_ty
            }
            // User-defined struct/actor methods from type_defs
            (
                Ty::Named {
                    name,
                    args: type_args,
                    builtin,
                },
                _,
            ) => {
                let canonical_receiver_name = self
                    .canonical_nominal_name(name)
                    .unwrap_or_else(|| name.clone());
                // Builtin `Result<T, E>` / `Option<T>` receivers (e.g. the
                // `Result<T, AskError>` wrapper an actor ask produces) resolve
                // their methods against the origin-based stdlib snapshot ONLY,
                // never the user `type_defs`/`fn_sigs`. A user package that
                // declares its own `type Result`/`type Option` registers its
                // methods under the same bare `Result::<method>` keys in
                // `fn_sigs`; resolving a builtin receiver through
                // `lookup_named_method_sig` would return whichever collided last
                // by registration order — e.g. a user `fn is_ok(self) -> i64`
                // shadowing the builtin `bool`-returning `is_ok`, producing an
                // ill-typed call codegen-front rejects. Confining the lookup to
                // `builtin_result_option_method_sigs` selects the canonical
                // builtin method (and its `extern_symbol` rewrite) for ALL
                // methods; any method absent from the builtin surface yields
                // `None` and falls through to the `no method on
                // Result<...>`/`Option<...>` diagnostic below.
                let sig = match builtin {
                    Some(b @ (BuiltinType::Result | BuiltinType::Option)) => {
                        self.lookup_builtin_result_option_method_sig(*b, type_args, method)
                    }
                    _ => self.lookup_named_method_sig(&canonical_receiver_name, type_args, method),
                };
                if let Some(sig) = sig {
                    // Mutable-receiver enforcement (Q297 Stage 1): methods
                    // declared with `var self` (or the named-receiver `var`
                    // equivalent) require the call-site receiver to be a
                    // `var`-bound binding. Without this gate, a caller could
                    // dispatch through an immutable `let`-bound binding and
                    // silently lose the contract that the trait declared a
                    // mutable receiver. Mirrors the precedent on `.step()`
                    // for machines (see further below in this arm).
                    if sig.requires_mutable_receiver {
                        let receiver_binding_name = match &receiver.0 {
                            Expr::Identifier(n) => Some(n.clone()),
                            _ => None,
                        };
                        let receiver_is_mutable = receiver_binding_name
                            .as_deref()
                            .and_then(|n| self.env.lookup_ref(n))
                            .is_some_and(|b| b.is_mutable);
                        if !receiver_is_mutable {
                            let receiver_label = if let Some(n) = &receiver_binding_name {
                                format!("`{n}`")
                            } else {
                                "this expression".to_string()
                            };
                            self.report_error(
                                TypeErrorKind::MutabilityError,
                                span,
                                format!(
                                    "method `{method}` on `{name}` requires a mutable binding receiver; \
                                     {receiver_label} is not declared with `var`",
                                ),
                            );
                        } else if let Some(n) = &receiver_binding_name {
                            // Mark the binding as written so the unused-mut
                            // analysis does not flag `var it = …; it.next()`
                            // as a never-reassigned mutable binding.
                            self.env.mark_written(n);
                            self.reject_private_param_mutable_receiver_call(
                                n,
                                &format!("method `{method}`"),
                                span,
                            );
                        }
                    }
                    let applied_sig = self.apply_instantiated_call_signature(
                        &sig,
                        None,
                        args,
                        span,
                        SignatureArgApplication::PositionalOnly {
                            arity_context: format!("method `{method}`"),
                        },
                        true,
                        Some(GenericCallee::Method {
                            type_name: &canonical_receiver_name,
                            method,
                            owner_type_args: type_args,
                        }),
                    );
                    // Actor receive-method dispatch on a bare actor-typed
                    // receiver — e.g. an actor field holding a reference
                    // (`let out: W; out.put(arg)`) or a `let target: Printer`
                    // binding. `lookup_named_method_sig` finds the signature in
                    // `fn_sigs` keyed `{Actor}::{method}`, but a value of bare
                    // actor type `W` is still an actor handle, not a struct: the
                    // call must cross the mailbox boundary exactly like the
                    // `LocalPid<W>` arm above. Route it through
                    // the same send/ask dispatch machinery instead of falling
                    // through to the synchronous `W::method(self, ...)`
                    // `RewriteToFunction` path (which HIR cannot lower — there is
                    // no standalone callable body for a receive handler, so it
                    // surfaces as `IndirectCallUnsupported`). Non-receive
                    // `methods {}` declared on the same actor (also keyed
                    // `{Actor}::{method}` in `fn_sigs`) are NOT in
                    // `actor_receive_methods`, so they stay on the direct path.
                    let is_actor_receive_dispatch = self
                        .type_defs
                        .get(name)
                        .is_some_and(|td| td.kind == TypeDefKind::Actor)
                        && self
                            .actor_receive_methods
                            .contains(&format!("{name}::{method}"));
                    if is_actor_receive_dispatch {
                        self.record_method_call_receiver_kind(
                            span,
                            MethodCallReceiverKind::ActorInstance {
                                actor_name: name.clone(),
                            },
                        );
                        // Every arg crosses the mailbox boundary; record the
                        // per-arg alias-vs-copy decision so the fail-closed
                        // codegen consumer does not have to guess.
                        self.enforce_actor_method_send_args(args);
                        // Ask-without-await guard: an ask-shaped receive fn
                        // (non-unit return, non-generator) must be invoked under
                        // `await`. Mirrors the `LocalPid` arm.
                        let method_key = format!("{name}::{method}");
                        let resolved_ret = self.subst.resolve(&applied_sig.return_type);
                        if !matches!(resolved_ret, Ty::Unit)
                            && !self.receive_generator_methods.contains(&method_key)
                            && !self.inside_await_expr
                        {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!(
                                    "actor ask `{name}.{method}` requires `await`; \
                                     write `let v? = await ref.{method}(...)` \
                                     or `match await ref.{method}(...) {{ Ok(v) => ..., Err(e) => ... }}`",
                                ),
                            );
                        }
                        // Record the dispatch discriminator (Fire vs Ask). This
                        // also marks the span as already-rewritten below, so the
                        // synchronous `RewriteToFunction` path is skipped and the
                        // call lowers to `ActorSend` / `ActorAsk` in HIR.
                        let call_ty = self.record_actor_method_dispatch(
                            span,
                            method_key,
                            applied_sig.return_type.clone(),
                        );
                        return call_ty;
                    }
                    self.record_method_call_receiver_kind(
                        span,
                        MethodCallReceiverKind::NamedTypeInstance {
                            type_name: name.clone(),
                        },
                    );
                    // Machine method dispatch: `.step()` and `.state_name()` on a
                    // machine-typed receiver are recorded in the checker-owned
                    // `machine_method_dispatch` side-table so HIR lowering can
                    // produce dedicated HIR nodes without falling through to the
                    // generic `method_call_rewrites` path (which would emit
                    // `MethodCallNoRewrite`).
                    //
                    // `.step()` additionally requires a mutable binding receiver:
                    // the internal `<Name>__step` helper returns a new machine
                    // value that must be stored back into the binding (slice 6).
                    // R-value and immutable-binding receivers are rejected here
                    // with a typed diagnostic.
                    if self
                        .type_defs
                        .get(name)
                        .is_some_and(|td| td.kind == TypeDefKind::Machine)
                    {
                        match method {
                            "step" => {
                                // Enforce mutable-binding receiver requirement.
                                // A bare identifier receiver is the common case;
                                // r-value and non-identifier receivers are also
                                // rejected because store-back (slice 6) cannot
                                // target them.
                                let receiver_binding_name = match &receiver.0 {
                                    Expr::Identifier(n) => Some(n.clone()),
                                    _ => None,
                                };
                                let receiver_is_mutable = receiver_binding_name
                                    .as_deref()
                                    .and_then(|n| self.env.lookup_ref(n))
                                    .is_some_and(|b| b.is_mutable);
                                if !receiver_is_mutable {
                                    let receiver_name = if let Some(n) = &receiver_binding_name {
                                        format!("`{n}`")
                                    } else {
                                        "this expression".to_string()
                                    };
                                    self.report_error(
                                        TypeErrorKind::MutabilityError,
                                        span,
                                        format!(
                                            "`.step()` requires a mutable binding receiver; \
                                             {receiver_name} is not declared with `var`"
                                        ),
                                    );
                                } else if let Some(n) = &receiver_binding_name {
                                    // `.step()` semantically reassigns the binding via the
                                    // synthesised store-back primitive (slice 6). Mark the
                                    // binding as written so the unused-mut analysis does
                                    // not flag `var lc = ...; lc.step(...)` as a
                                    // never-reassigned mutable binding.
                                    self.env.mark_written(n);
                                    self.reject_private_param_mutable_receiver_call(
                                        n,
                                        "`.step()`",
                                        span,
                                    );
                                }
                                self.machine_method_dispatch.insert(
                                    SpanKey::in_module(span, self.current_module_idx),
                                    MachineMethodKind::Step {
                                        machine_name: canonical_receiver_name.clone(),
                                    },
                                );
                            }
                            "state_name" => {
                                self.machine_method_dispatch.insert(
                                    SpanKey::in_module(span, self.current_module_idx),
                                    MachineMethodKind::StateName {
                                        machine_name: canonical_receiver_name.clone(),
                                    },
                                );
                            }
                            "take_emits" => {
                                self.machine_method_dispatch.insert(
                                    SpanKey::in_module(span, self.current_module_idx),
                                    MachineMethodKind::TakeEmits {
                                        machine_name: canonical_receiver_name.clone(),
                                    },
                                );
                            }
                            _ => {}
                        }
                    }
                    // A terminal consuming method moves its receiver, so record
                    // the per-call-site flag for HIR/codegen AND mark the receiver
                    // expression moved (a later use surfaces `UseAfterMove`).
                    // A resource's canonical close additionally records a
                    // one-time discharge: lowering uses it to suppress the
                    // scope-exit implicit drop on the consumed path, and the
                    // checker uses it to report a second close with the
                    // specific double-close diagnostic. Discharging the
                    // obligation is a consequence of the move, never a
                    // substitute for it — close-then-use is use-after-move.
                    // Three surfaces qualify:
                    //   1. stdlib `impl Closable for T { fn close }` — the trait
                    //      `close` flattens into T's inherent-method table; honour
                    //      the `consumes_receiver` declared on the trait.
                    //   2. a `#[resource]` type's inherent `fn close(self)` — the
                    //      implicit-drop dispatch target, which when called
                    //      explicitly also moves the receiver so the scope-exit
                    //      implicit drop is suppressed on the consumed path (no
                    //      double-close).
                    //   3. any `fn m(consuming self)` inherent method — the
                    //      terminal single-consume surface (a builder's
                    //      `build(consuming self)`, a `#[linear]` type's consuming
                    //      method). The resolved sig carries the consume fact.
                    let consumes_receiver = sig.consumes_receiver
                        || self.named_type_method_consumes_receiver(name, method)
                        || self.named_type_inherent_close_consumes_receiver(
                            name, *builtin, method, &sig,
                        )
                        || (matches!(*builtin, Some(BuiltinType::VecIter))
                            && matches!(
                                method,
                                "map"
                                    | "filter"
                                    | "fold"
                                    | "any"
                                    | "all"
                                    | "find"
                                    | "count"
                                    | "enumerate"
                                    | "take"
                                    | "skip"
                                    | "collect"
                            ));
                    if consumes_receiver {
                        self.method_call_consumes_receiver
                            .insert(SpanKey::in_module(span, self.current_module_idx));
                        let resolved_recv = self.subst.resolve(&receiver_ty);
                        let discharges_resource = self.named_type_inherent_close_consumes_receiver(
                            name, *builtin, method, &sig,
                        );
                        if discharges_resource {
                            self.method_call_discharges_receiver
                                .insert(SpanKey::in_module(span, self.current_module_idx));
                            if let Expr::Identifier(receiver_name) = &receiver.0 {
                                match self.env.mark_released(receiver_name, receiver.1.clone()) {
                                    Some(Some(prior)) => {
                                        let mut error = TypeError::new(
                                            TypeErrorKind::UseAfterConsume,
                                            receiver.1.clone(),
                                            format!(
                                                "resource `{receiver_name}` cannot be closed more than once"
                                            ),
                                        )
                                        .with_note(prior, "resource was first closed here");
                                        if let Some(source_module) = &self.current_module {
                                            error = error.with_source_module(source_module.clone());
                                        }
                                        self.errors.push(error);
                                    }
                                    // First discharge: the close consumes its
                                    // receiver, so the move lands with it and any
                                    // later use is use-after-move. Marked directly
                                    // (not via `mark_expr_moved_if_non_copy`)
                                    // because the released flag was just set by
                                    // this very call and must not read as a prior
                                    // consumption of the receiver.
                                    Some(None)
                                        if !self.registry.implements_marker(
                                            &resolved_recv,
                                            MarkerTrait::Copy,
                                        ) =>
                                    {
                                        self.env.mark_moved(receiver_name, receiver.1.clone());
                                    }
                                    Some(None) | None => {}
                                }
                            } else {
                                self.mark_expr_moved_if_non_copy(
                                    &receiver.0,
                                    &receiver.1,
                                    &resolved_recv,
                                );
                            }
                        } else {
                            self.mark_expr_moved_if_non_copy(
                                &receiver.0,
                                &receiver.1,
                                &resolved_recv,
                            );
                        }
                    }
                    self.record_handle_method_call_rewrite_if_any(&resolved, method, span);
                    let builtin_option_result_marker =
                        Self::is_builtin_option_result_marker_method(*builtin, method);
                    if let Some(receiver_builtin @ (BuiltinType::Result | BuiltinType::Option)) =
                        *builtin
                    {
                        self.record_builtin_option_result_method_rewrite_if_any(
                            receiver_builtin,
                            name,
                            type_args,
                            method,
                            span,
                        );
                    }
                    self.record_named_extern_symbol_rewrite_if_any(
                        &canonical_receiver_name,
                        type_args,
                        method,
                        &sig,
                        span,
                    );
                    // W3.042 S2-S2: user-defined methods on named types (both
                    // inherent `impl Type { fn m(...) }` and trait `impl T for
                    // Type { fn m(...) }`) must record a `RewriteToFunction`
                    // entry naming the qualified `Type::method` symbol so HIR
                    // lowering can emit a direct `Call` (with the receiver
                    // injected as the first argument) instead of falling
                    // through to `MethodCallNoRewrite`. The qualified symbol
                    // is the same key that `hew-hir`'s pre-pass seeds into
                    // `fn_registry` (`HirImplBlock::method_symbol`), so
                    // resolution succeeds without further wiring.
                    //
                    // Skipped when an earlier helper above already recorded a
                    // rewrite (handle methods, monomorphic-extern symbols), or
                    // when a dedicated dispatch side-table will be consulted
                    // by HIR before `method_call_rewrites` (machine
                    // `step`/`state_name`, actor send/ask, dyn-trait,
                    // resolved-impl call kernel).
                    let span_key = SpanKey::in_module(span, self.current_module_idx);
                    let already_rewritten = builtin_option_result_marker
                        || self.method_call_rewrites.contains_key(&span_key)
                        || self.machine_method_dispatch.contains_key(&span_key)
                        || self.actor_method_dispatch.contains_key(&span_key)
                        || self.dyn_trait_method_calls.contains_key(&span_key)
                        || self.resolved_calls.contains_key(&span_key);
                    if !already_rewritten {
                        // The resolved receiver owner is executable dispatch
                        // authority. Registration publishes this exact key;
                        // never retry through the receiver's final segment.
                        // The receiver's resolved nominal owner is executable
                        // dispatch authority. The declaration map is keyed by
                        // that exact source identity even when compatibility
                        // `fn_sigs` aliases retain a shorter presentation.
                        // Never retry through either the type or method leaf.
                        let method_owner = canonical_receiver_name.as_str();
                        let method_key = format!("{method_owner}::{method}");
                        // Wire codec instance serialize methods on a `#[wire]`
                        // struct or enum. `encode` is the binary CBOR path
                        // (`value.encode() -> bytes`); `to_json`/`to_yaml` are the
                        // text path (`value.to_json() -> string`), lowered through
                        // the CBOR↔text bridge. The instance method is registered
                        // in `type_def.methods` (not `fn_sigs`), so it never
                        // matches the `fn_sigs` branch below and would otherwise
                        // fall through to `MethodCallNoRewrite`. Record a dedicated
                        // codec rewrite so HIR/codegen drive the matching thunk
                        // with the correct ABI.
                        let wire_serialize_dir =
                            if self.wire_struct_types.contains(&canonical_receiver_name)
                                || self.wire_enum_types.contains(&canonical_receiver_name)
                            {
                                match method {
                                    "encode" => Some(WireCodecDirection::Encode),
                                    "to_json" => Some(WireCodecDirection::ToJson),
                                    "to_yaml" => Some(WireCodecDirection::ToYaml),
                                    _ => None,
                                }
                            } else {
                                None
                            };
                        if let Some(direction) = wire_serialize_dir {
                            // `value_ty` is the receiver wire type (the value being
                            // serialized) regardless of the textual return type,
                            // so codegen keys the thunk by the same type the binary
                            // path uses. Carry the CANONICAL nominal: the wire
                            // layout table is keyed by the declaration identity
                            // only, and codegen's tag probe falls back to
                            // positional keys on a miss — a bare spelling here
                            // would silently change the encoded schema.
                            let mut value_source = self.subst.resolve(&resolved);
                            if let Ty::Named { name: nominal, .. } = &mut value_source {
                                if *nominal != canonical_receiver_name {
                                    nominal.clone_from(&canonical_receiver_name);
                                }
                            }
                            if let Ok(value_ty) = ResolvedTy::from_ty(&value_source) {
                                self.record_method_call_rewrite(
                                    span,
                                    MethodCallRewrite::WireCodec {
                                        direction,
                                        value_ty,
                                    },
                                );
                            }
                        } else if matches!(*builtin, Some(BuiltinType::VecIter)) && method == "next"
                        {
                            if let Some(elem_ty) = type_args.first() {
                                if !self.validate_vec_iter_element_clone_type(elem_ty, span) {
                                    return Ty::Error;
                                }
                                if let Ok(elem_resolved) =
                                    ResolvedTy::from_ty(&self.subst.resolve(elem_ty))
                                {
                                    self.record_method_call_rewrite(
                                        span,
                                        MethodCallRewrite::BuiltinVecIterNext {
                                            elem_ty: elem_resolved,
                                        },
                                    );
                                }
                            }
                        } else if self.fn_sigs.contains_key(&method_key)
                            || self.impl_method_declaration_ids.contains_key(&method_key)
                            || (!type_args.is_empty() && {
                                // Concrete-specialised-impl check (#2270): the
                                // type_args may resolve to a mangled key even
                                // when the bare key is absent (e.g. after the
                                // first concrete impl was registered and the bare
                                // key was clobbered by the second).
                                let resolved_args: Option<Vec<ResolvedTy>> = type_args
                                    .iter()
                                    .map(|ty| ResolvedTy::from_ty(&self.subst.resolve(ty)).ok())
                                    .collect();
                                resolved_args
                                    .as_ref()
                                    .and_then(|args| {
                                        crate::resolved_ty::mangle_impl_self_name(
                                            method_owner,
                                            args,
                                        )
                                    })
                                    .is_some_and(|m| {
                                        self.fn_sigs.contains_key(&format!("{m}::{method}"))
                                    })
                            })
                        {
                            // For concrete-specialised impls, use the mangled
                            // c_symbol so HIR looks up the right `fn_registry`
                            // entry.  Falls back to the bare key for all other
                            // cases (generic impls, inherent methods, etc.).
                            let dispatch_key = if type_args.is_empty() {
                                method_key.clone()
                            } else {
                                let resolved_args: Option<Vec<ResolvedTy>> = type_args
                                    .iter()
                                    .map(|ty| ResolvedTy::from_ty(&self.subst.resolve(ty)).ok())
                                    .collect();
                                resolved_args
                                    .as_ref()
                                    .and_then(|args| {
                                        crate::resolved_ty::mangle_impl_self_name(
                                            method_owner,
                                            args,
                                        )
                                    })
                                    .filter(|m| {
                                        self.fn_sigs.contains_key(&format!("{m}::{method}"))
                                    })
                                    .map_or_else(
                                        || method_key.clone(),
                                        |m| format!("{m}::{method}"),
                                    )
                            };
                            self.record_method_call_rewrite(
                                span,
                                MethodCallRewrite::RewriteToFunction {
                                    target: self
                                        .impl_method_declaration_ids
                                        .get(&dispatch_key)
                                        .or_else(|| {
                                            self.impl_method_declaration_ids.get(&method_key)
                                        })
                                        .cloned()
                                        .map_or_else(
                                            || CallTarget::Unsupported {
                                                reason: format!(
                                                    "impl method `{dispatch_key}` has no registered declaration identity"
                                                ),
                                            },
                                            CallTarget::impl_method,
                                        ),
                                    c_symbol: dispatch_key,
                                    // User-defined `Type::method` dispatch is
                                    // open-set; the typed runtime-call catalog
                                    // does not enumerate user method keys.
                                    descriptor: None,
                                    extern_identity: None,
                                    elem_ty: (matches!(*builtin, Some(BuiltinType::VecIter))
                                        && matches!(
                                            method,
                                            "map"
                                                | "filter"
                                                | "fold"
                                                | "any"
                                                | "all"
                                                | "find"
                                                | "count"
                                                | "enumerate"
                                                | "take"
                                                | "skip"
                                                | "collect"
                                        ))
                                    .then(|| type_args.first())
                                    .flatten()
                                    .and_then(|elem_ty| {
                                            ResolvedTy::from_ty(&self.subst.resolve(elem_ty)).ok()
                                        }),
                                    // #1295: a `#[resource]` type's inherent
                                    // `close(self)` is a terminal handle-release
                                    // consume — HIR lowers the receiver with
                                    // `IntentKind::Consume` so MIR marks it
                                    // `Consumed` and suppresses the duplicate
                                    // scope-exit implicit drop. The `consumes`
                                    // flag was computed above (resource close or
                                    // a `Closable` trait method flattened onto
                                    // this type); other inherent/trait methods
                                    // are not consuming releases.
                                    consumes_receiver,
                                    returns_receiver_identity: sig.returns_receiver_identity,
                                },
                            );
                        }
                    }
                    return self.qualify_method_return_to_receiver_owner(
                        &canonical_receiver_name,
                        &applied_sig.return_type,
                    );
                }
                // Type-parameter method dispatch: resolve from trait bounds.
                // When the receiver is a generic type parameter (e.g. `T` in
                // `fn report<T: Measurable>(item: T)`), look up the method
                // from the traits that bound that parameter.
                //
                // Algorithm (origin-aware supertrait expansion):
                // 1. For each bound, call lookup_trait_method_with_origin → (declaring_trait, sig)
                // 2. Collect all hits, deduplicate by declaring_trait
                // 3. 0 hits → UndefinedMethod, >1 distinct declaring traits → AmbiguousTraitMethod,
                //    1 → record StaticTraitDispatch rewrite
                let bounds_for_type_param = self.current_function.as_ref().and_then(|fn_name| {
                    self.fn_sigs.get(fn_name).and_then(|sig| {
                        if sig.type_params.contains(name) {
                            sig.type_param_bounds.get(name).cloned()
                        } else {
                            None
                        }
                    })
                });
                if let Some(bounds) = bounds_for_type_param {
                    // Expand all bounds into (bound_trait, declaring_trait, sig) tuples.
                    // For each bound, also walk its supertrait DAG to collect every
                    // trait that DIRECTLY declares the method — this catches the
                    // supertrait-redeclaration case (plan §4 V14) where a bound
                    // `T: B` with `trait B: A` and both A and B declaring the same
                    // method reaches two distinct declaring traits.
                    let mut hits: Vec<(String, String, FnSig)> = Vec::new();
                    for bound_trait in &bounds {
                        // Keep the source spelling for diagnostics, but resolve
                        // the dispatch lookup through the declaration owner.
                        // An imported alias such as `AlphaRender` is not a
                        // declaration identity and must never reach HIR as one.
                        let bound_trait_key = self.trait_ref_lookup_key(bound_trait);
                        let declaring =
                            self.collect_all_declaring_traits_for_method(&bound_trait_key, method);
                        for declaring_trait in declaring {
                            // Resolve the sig from the declaring trait directly.
                            if let Some((_, sig)) =
                                self.lookup_trait_method_with_origin(&declaring_trait, method)
                            {
                                hits.push((bound_trait.clone(), declaring_trait, sig));
                            }
                        }
                    }
                    // Deduplicate by declaring_trait — same origin via multiple bounds is NOT ambiguous.
                    hits.sort_by(|a, b| a.1.cmp(&b.1));
                    hits.dedup_by_key(|h| h.1.clone());

                    if hits.len() == 1 {
                        let (bound_trait, declaring_trait, mut trait_sig) =
                            hits.into_iter().next().unwrap();
                        // Replace `Self` references with the type parameter type.
                        let self_ty = resolved.clone();
                        for param_ty in &mut trait_sig.params {
                            *param_ty = param_ty.substitute_named_param("Self", &self_ty);
                        }
                        trait_sig.return_type = trait_sig
                            .return_type
                            .substitute_named_param("Self", &self_ty);
                        // W3.042 S2-S4: receiver-mutability gate for the
                        // generic-bound StaticTraitDispatch arm. Mirrors the
                        // (Ty::Named, _) direct-call gate above (Stage 1):
                        // when the trait method is declared with `var self`
                        // the call site must bind the receiver with `var`,
                        // otherwise a mutating method would silently dispatch
                        // through an immutable binding and lose the contract.
                        // The substituted `trait_sig.requires_mutable_receiver`
                        // is the checker-authoritative source — we do NOT
                        // re-walk `trait_defs` here (LESSONS `checker-authority`).
                        if trait_sig.requires_mutable_receiver {
                            let receiver_binding_name = match &receiver.0 {
                                Expr::Identifier(n) => Some(n.clone()),
                                _ => None,
                            };
                            let receiver_is_mutable = receiver_binding_name
                                .as_deref()
                                .and_then(|n| self.env.lookup_ref(n))
                                .is_some_and(|b| b.is_mutable);
                            if !receiver_is_mutable {
                                let receiver_label = if let Some(n) = &receiver_binding_name {
                                    format!("`{n}`")
                                } else {
                                    "this expression".to_string()
                                };
                                self.report_error(
                                    TypeErrorKind::MutabilityError,
                                    span,
                                    format!(
                                        "trait method `{declaring_trait}.{method}` \
                                         (statically dispatched on type parameter `{name}`) \
                                         requires a mutable binding receiver; \
                                         {receiver_label} is not declared with `var`",
                                    ),
                                );
                            } else if let Some(n) = &receiver_binding_name {
                                self.env.mark_written(n);
                                self.reject_private_param_mutable_receiver_call(
                                    n,
                                    &format!("trait method `{declaring_trait}.{method}`"),
                                    span,
                                );
                            }
                        }
                        let applied_sig = self.apply_instantiated_call_signature(
                            &trait_sig,
                            None,
                            args,
                            span,
                            SignatureArgApplication::PositionalOnly {
                                arity_context: format!("method `{method}`"),
                            },
                            true,
                            Some(GenericCallee::Method {
                                type_name: &declaring_trait,
                                method,
                                owner_type_args: &[],
                            }),
                        );
                        if declaring_trait == "std.builtins.Pid" && method == "send" {
                            // TODO(A640): replace this fail-closed branch with
                            // a first-class `P::Msg: Serializable` projection
                            // bound once the checker can express that shape on
                            // pid-polymorphic call sites. If the projection is
                            // already concretely bound (for example
                            // `P: Pid<Msg = Ping>`), the regular Serializable
                            // gate below proves it and the call may proceed.
                            if !self.enforce_pid_polymorphic_send_serializable_args(args, name) {
                                return Ty::Error;
                            }
                            self.enforce_actor_method_send_args(args);
                        }
                        self.record_method_call_receiver_kind(
                            span,
                            MethodCallReceiverKind::NamedTypeInstance {
                                type_name: name.clone(),
                            },
                        );
                        if trait_sig.consumes_receiver {
                            self.method_call_consumes_receiver
                                .insert(SpanKey::in_module(span, self.current_module_idx));
                            let resolved_ty = self.subst.resolve(&receiver_ty);
                            self.mark_expr_moved_if_non_copy(
                                &receiver.0,
                                &receiver.1,
                                &resolved_ty,
                            );
                        }
                        // Record the StaticTraitDispatch rewrite for HIR consumption.
                        let target = self
                            .trait_method_call_target_ids(&declaring_trait, method)
                            .or_else(|| self.trait_method_call_target_ids(&bound_trait, method))
                            .map_or_else(
                                || CallTarget::Unsupported {
                                    reason: format!(
                                        "trait method `{declaring_trait}.{method}` has no registered declaration identity"
                                    ),
                                },
                                |(declaring_trait, method)| CallTarget::StaticTraitMethod {
                                    declaring_trait,
                                    method,
                                },
                            );
                        self.record_method_call_rewrite(
                            span,
                            MethodCallRewrite::StaticTraitDispatch {
                                target,
                                receiver_type_param: name.clone(),
                                bound_trait,
                                declaring_trait,
                                method_name: method.to_string(),
                                requires_mutable_receiver: trait_sig.requires_mutable_receiver,
                                consumes_receiver: trait_sig.consumes_receiver,
                                returns_receiver_identity: trait_sig.returns_receiver_identity,
                            },
                        );
                        return self.project_assoc_types(&applied_sig.return_type);
                    } else if hits.len() > 1 {
                        // Multiple distinct declaring traits → ambiguous.
                        for arg in args {
                            let (expr, sp) = arg.expr();
                            self.synthesize(expr, sp);
                        }
                        let declaring_traits: Vec<&str> =
                            hits.iter().map(|h| h.1.as_str()).collect();
                        self.report_error(
                            TypeErrorKind::AmbiguousTraitMethod,
                            span,
                            format!(
                                "ambiguous trait method `{method}` on `{}`: method is declared by \
                                 multiple traits ({}); qualify the call to disambiguate",
                                resolved.user_facing(),
                                declaring_traits.join(", ")
                            ),
                        );
                        return Ty::Error;
                    }
                    // hits.is_empty() → fall through to UndefinedMethod below.
                }
                // Fn-typed field call: `w.cb(args)` where `cb` is a record
                // field of function type dispatches as a field-load +
                // closure call, not a method lookup. Pre-validated (arity +
                // per-arg types against the field signature) and recorded as
                // a structured rewrite so HIR never guesses
                // (`checker-codegen-pattern-contract`). A field that exists
                // but is NOT fn-typed falls through to `UndefinedMethod`.
                if let Some(ret_ty) = self.try_record_fn_field_call(&resolved, method, args, span) {
                    return ret_ty;
                }
                // `clone` on a user-defined named type: intercept before
                // `UndefinedMethod` for admissible records.
                // This arm handles the (Ty::Named { builtin: None, .. }, "clone")
                // case where `try_resolve_named_method` found no `clone` in fn_sigs.
                if method == "clone" && args.is_empty() {
                    if let Ty::Named {
                        name,
                        args: type_args,
                        builtin: None,
                    } = &resolved
                    {
                        match self.record_clone_admissibility(name, type_args, span) {
                            RecordCloneAdmissibility::Admissible => {
                                self.record_method_call_rewrite(
                                    span,
                                    MethodCallRewrite::RecordCloneInplace {
                                        record_name: name.clone(),
                                    },
                                );
                                // Bare-seed monomorphic records only; a generic
                                // instantiation is MIR-keyed by its mono layout
                                // and seeded from the `RecordCloneInplace` walk
                                // in codegen (see the sibling clone intercept).
                                if type_args.is_empty()
                                    && !self.user_clone_record_seeds.contains(name)
                                {
                                    self.user_clone_record_seeds.push(name.clone());
                                }
                                return resolved;
                            }
                            RecordCloneAdmissibility::OpaqueField {
                                opaque_name,
                                member,
                            } => {
                                self.report_error(
                                    TypeErrorKind::UndefinedMethod,
                                    span,
                                    format!(
                                        "type `{name}` cannot be cloned because member `{member}` \
                                         contains opaque value `{opaque_name}`"
                                    ),
                                );
                                return Ty::Error;
                            }
                            RecordCloneAdmissibility::AffineValue {
                                type_name,
                                marker,
                                member,
                            } => {
                                let receiver_name = resolved.user_facing().to_string();
                                self.report_affine_record_clone_error(
                                    &receiver_name,
                                    &type_name,
                                    marker,
                                    &member,
                                    span,
                                );
                                return Ty::Error;
                            }
                            RecordCloneAdmissibility::MissingClone { member, member_ty } => {
                                self.report_error(
                                    TypeErrorKind::UndefinedMethod,
                                    span,
                                    format!(
                                        "type `{}` cannot be cloned because member `{member}` of \
                                         type `{}` has no Clone capability",
                                        resolved.user_facing(),
                                        member_ty.user_facing()
                                    ),
                                );
                                return Ty::Error;
                            }
                            RecordCloneAdmissibility::UnbalancedSharedHandle {
                                type_name,
                                member,
                            } => {
                                let receiver_name = resolved.user_facing().to_string();
                                self.report_unbalanced_shared_handle_clone_error(
                                    &receiver_name,
                                    &type_name,
                                    &member,
                                    span,
                                );
                                return Ty::Error;
                            }
                            RecordCloneAdmissibility::GenericRecord => {
                                self.report_error(
                                    TypeErrorKind::UndefinedMethod,
                                    span,
                                    format!(
                                        "cloning generic record `{name}` is not yet \
                                         supported; only monomorphic records can be cloned"
                                    ),
                                );
                                return Ty::Error;
                            }
                            RecordCloneAdmissibility::AbstractParamClone => {
                                // Bare type param `x: T` with `T: Clone`; defer
                                // the concrete copy path to monomorphization. No
                                // seed: `T` names no monomorphic record layout.
                                self.record_method_call_rewrite(
                                    span,
                                    MethodCallRewrite::RecordCloneInplace {
                                        record_name: name.clone(),
                                    },
                                );
                                return resolved;
                            }
                            RecordCloneAdmissibility::EnumClone { enum_name } => {
                                // User enum: same rewrite + HIR node as a record
                                // clone; MIR demuxes by the resolved layout and
                                // emits `EnumCloneInplace`. No bare-name seed —
                                // the MIR thunk registry's `collect_enum_clone_inplace_seeds`
                                // keys the per-mono helper.
                                self.record_method_call_rewrite(
                                    span,
                                    MethodCallRewrite::RecordCloneInplace {
                                        record_name: enum_name,
                                    },
                                );
                                return resolved;
                            }
                            RecordCloneAdmissibility::NotARecord => {
                                // Fall through to UndefinedMethod below.
                            }
                        }
                    }
                }
                // Synthesize args even if method unknown (for error recovery)
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{}`", resolved.user_facing()),
                    self.similar_methods(&resolved, method),
                );
                Ty::Error
            }
            // Trait object method dispatch: look up methods from all trait bounds
            (Ty::TraitObject { traits }, _) => {
                // Try to find the method in any of the traits
                let mut found_sig = None;
                let mut found_bound = None;
                for bound in traits {
                    let trait_lookup_key = self.trait_ref_lookup_key(&bound.trait_name);
                    if let Some(sig) = self.lookup_trait_method(&trait_lookup_key, method) {
                        found_sig = Some(sig);
                        found_bound = Some(bound);
                        break;
                    }
                }

                if let Some(mut sig) = found_sig {
                    let pid_send_dispatch = found_bound.as_ref().is_some_and(|bound| {
                        self.trait_ref_lookup_key(&bound.trait_name) == "std.builtins.Pid"
                            && method == "send"
                    });
                    if let Some(bound) = found_bound {
                        self.record_method_call_receiver_kind(
                            span,
                            MethodCallReceiverKind::TraitObject {
                                trait_name: bound.trait_name.clone(),
                            },
                        );
                        // Apply trait-type-param and associated-type
                        // substitution UP FRONT so the substituted
                        // `FnSig` can be recorded on `DynMethodCall`
                        // alongside the slot. W3.031 Stage 1.6 makes
                        // the typed signature self-contained on
                        // `Instr::CallTraitMethod`; codegen never
                        // re-derives it from the impl fn or by
                        // walking vtable entries (per Q-β resolution).
                        self.apply_trait_object_bound_substitutions(&mut sig, bound);
                        // W3.042 S2-S4: receiver-mutability gate for the
                        // Ty::TraitObject (dyn Trait) dispatch arm. Mirrors
                        // the (Ty::Named, _) direct-call gate above (Stage 1)
                        // and the StaticTraitDispatch gate. The substituted
                        // `sig.requires_mutable_receiver` flag is the
                        // checker-authoritative source (LESSONS
                        // `checker-authority`); the flag survives
                        // `apply_trait_object_bound_substitutions` per the
                        // FnSig schema (W3.042 plan §3.6). Receiver shape
                        // for dyn dispatch is always a Box<dyn Trait> bound
                        // identifier — the same `Expr::Identifier` extraction
                        // the other arms use applies here.
                        if sig.requires_mutable_receiver {
                            let receiver_binding_name = match &receiver.0 {
                                Expr::Identifier(n) => Some(n.clone()),
                                _ => None,
                            };
                            let receiver_is_mutable = receiver_binding_name
                                .as_deref()
                                .and_then(|n| self.env.lookup_ref(n))
                                .is_some_and(|b| b.is_mutable);
                            if !receiver_is_mutable {
                                let receiver_label = if let Some(n) = &receiver_binding_name {
                                    format!("`{n}`")
                                } else {
                                    "this expression".to_string()
                                };
                                self.report_error(
                                    TypeErrorKind::MutabilityError,
                                    span,
                                    format!(
                                        "method `{method}` on `dyn {}` requires a \
                                         mutable binding receiver; {receiver_label} is \
                                         not declared with `var`",
                                        bound.trait_name,
                                    ),
                                );
                            } else if let Some(n) = &receiver_binding_name {
                                self.env.mark_written(n);
                                self.reject_private_param_mutable_receiver_call(
                                    n,
                                    &format!("method `{method}` on `dyn {}`", bound.trait_name),
                                    span,
                                );
                            }
                        }
                        // Record the per-call-site vtable-slot resolution that
                        // HIR/MIR lowering will consume to emit
                        // `Instr::CallTraitMethod`. Slot convention follows
                        // `hew-runtime/src/trait_object.rs::HewVtable`: slots
                        // 0..3 are the fixed prefix triple
                        // (`drop_in_place`/`size_of`/`align_of`), trait methods
                        // start at slot 3 in trait-declaration order.
                        let trait_lookup_key = self.trait_ref_lookup_key(&bound.trait_name);
                        if let Some(trait_info) = self.trait_defs.get(&trait_lookup_key) {
                            if let Some(method_idx) =
                                trait_info.methods.iter().position(|m| m.name == method)
                            {
                                // Slot index is bounded by the trait's
                                // method count, which Hew limits to
                                // u32-sized vtables long before any
                                // truncation risk. `try_from` keeps the
                                // boundary explicit (LESSONS:
                                // `boundary-fail-closed`).
                                let slot = 3 + u32::try_from(method_idx).unwrap_or(u32::MAX);
                                let target = self
                                    .trait_method_call_target_ids(&bound.trait_name, method)
                                    .map_or_else(
                                        || CallTarget::Unsupported {
                                            reason: format!(
                                                "dynamic trait method `{}.{method}` has no registered declaration identity",
                                                bound.trait_name
                                            ),
                                        },
                                        |(declaring_trait, method)| {
                                            CallTarget::DynamicVtable {
                                                declaring_trait,
                                                method,
                                                slot,
                                            }
                                        },
                                    );
                                self.dyn_trait_method_calls.insert(
                                    SpanKey::in_module(span, self.current_module_idx),
                                    crate::check::types::DynMethodCall {
                                        target,
                                        trait_name: bound.trait_name.clone(),
                                        method_name: method.to_string(),
                                        slot,
                                        signature: sig.clone(),
                                    },
                                );
                            }
                        }
                        if sig.consumes_receiver {
                            self.method_call_consumes_receiver
                                .insert(SpanKey::in_module(span, self.current_module_idx));
                            let resolved_ty = self.subst.resolve(&receiver_ty);
                            self.mark_expr_moved_if_non_copy(
                                &receiver.0,
                                &receiver.1,
                                &resolved_ty,
                            );
                        }
                    }
                    let applied_sig = self.apply_instantiated_call_signature(
                        &sig,
                        None,
                        args,
                        span,
                        SignatureArgApplication::PositionalOnly {
                            arity_context: format!("method `{method}`"),
                        },
                        true,
                        // Dynamic vtable dispatch pins no static instantiation; the concrete
                        // impl is selected at run time, so there is nothing to discharge here.
                        None,
                    );
                    if pid_send_dispatch {
                        self.enforce_actor_method_send_args(args);
                        if !self.enforce_remote_actor_method_serializable_args(args) {
                            return Ty::Error;
                        }
                    }
                    applied_sig.return_type
                } else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    let message = if resolved.is_numeric()
                        && method.starts_with("to_")
                        && matches!(
                            &method["to_".len()..],
                            "i8" | "i16"
                                | "i32"
                                | "i64"
                                | "isize"
                                | "u8"
                                | "u16"
                                | "u32"
                                | "u64"
                                | "usize"
                                | "f32"
                                | "f64"
                        ) {
                        format!(
                            "no method `.{method}()` on numeric type `{}`; use `as` for numeric casts \
                             or `.try_to_<W>()` for exact fallible conversion",
                            resolved.user_facing()
                        )
                    } else if method == "clone" && args.is_empty() {
                        // A trait object is a two-word fat pointer whose
                        // concrete type is erased. Its vtable carries only
                        // `drop_in_place`/`size_of`/`align_of` plus the trait's
                        // own methods (see `hew-runtime/src/trait_object.rs`),
                        // so no slot can reproduce the concrete value. Naming
                        // the limit and the supported ordering keeps this a
                        // user-facing rejection instead of a reinterpretation
                        // of the fat pointer as the concrete layout.
                        //
                        // WHY (shim): duplicating a `dyn Trait` needs a clone
                        // slot in the vtable prefix, which renumbers every
                        // method slot across the runtime, MIR, and codegen.
                        // WHEN-OBSOLETE: when the trait-object ABI grows that
                        // slot and every coercion site emits a clone thunk.
                        // WHAT (real solution): a `clone_in_place` vtable entry
                        // emitted alongside `drop_in_place`.
                        format!(
                            "`clone` is not supported on `{ty}`: a trait object erases its \
                             concrete type and its vtable carries no clone slot; clone the \
                             concrete value before erasing it \
                             (`let copy: {ty} = clone original;`)",
                            ty = resolved.user_facing(),
                        )
                    } else {
                        format!("no method `{method}` on `{}`", resolved.user_facing())
                    };
                    self.report_missing_method_with_shadow_note(receiver, method, span, message);
                    Ty::Error
                }
            }
            // For error types, don't report additional errors
            (Ty::Error, _) => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                Ty::Error
            }
            _ => {
                // Stage A2: before reporting "no method on X", consult the
                // user-impl side table for primitive / compiler-builtin
                // generic receivers.  This catches `Ty::I64`, `Ty::Bool`,
                // `Ty::Char`, the integer/float width aliases, and bare
                // `Vec`/`HashMap`/`HashSet` references that fall through
                // every earlier arm.  Per-receiver-kind sites that route
                // through `check_*_method` (Vec, HashMap, HashSet, String,
                // Bytes) consult the same table at their own not-found
                // branches so dispatch is exhaustive.
                if let Some(ret_ty) =
                    self.try_dispatch_primitive_trait_method(&resolved, method, args, span)
                {
                    return ret_ty;
                }
                // `clone` on a Copy/BitCopy type: warn (non-fatal) and return
                // the operand type. The value is already a copy — no extra work
                // needed. HIR lowers this as a plain read via `CopyCloneNoop`.
                // LESSONS: `fail-closed-never-fail-open` (exit 0, not Ty::Error).
                if method == "clone" && args.is_empty() {
                    let is_copy_ty = matches!(
                        &resolved,
                        Ty::I8
                            | Ty::I16
                            | Ty::I32
                            | Ty::I64
                            | Ty::U8
                            | Ty::U16
                            | Ty::U32
                            | Ty::U64
                            | Ty::Isize
                            | Ty::Usize
                            | Ty::F32
                            | Ty::F64
                            | Ty::Bool
                            | Ty::Char
                    );
                    if is_copy_ty {
                        let module = self.current_module.clone();
                        self.emit_main_pass_lint(
                            LintId::CloneOnCopy,
                            span,
                            module.as_deref(),
                            format!(
                                "cloning a Copy type `{}` is redundant; \
                                 this is equivalent to a plain copy",
                                resolved.user_facing()
                            ),
                            "remove the `clone` — Copy types are duplicated automatically"
                                .to_string(),
                        );
                        self.record_method_call_rewrite(span, MethodCallRewrite::CopyCloneNoop);
                        return resolved;
                    }
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                let message = if resolved.is_numeric()
                    && method.starts_with("to_")
                    && matches!(
                        &method["to_".len()..],
                        "i8" | "i16"
                            | "i32"
                            | "i64"
                            | "isize"
                            | "u8"
                            | "u16"
                            | "u32"
                            | "u64"
                            | "usize"
                            | "f32"
                            | "f64"
                    ) {
                    format!(
                        "no method `.{method}()` on numeric type `{}`; use `as` for numeric casts \
                         or `.try_to_<W>()` for exact fallible conversion",
                        resolved.user_facing()
                    )
                } else {
                    format!("no method `{method}` on `{}`", resolved.user_facing())
                };
                self.report_missing_method_with_shadow_note(receiver, method, span, message);
                Ty::Error
            }
        }
    }

    fn report_missing_method_with_shadow_note(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        span: &Span,
        message: String,
    ) {
        let mut error = TypeError::new(TypeErrorKind::UndefinedMethod, span.clone(), message);
        if let Expr::Identifier(binding) = &receiver.0 {
            if self.env.lookup_ref(binding).is_some()
                && self.module_import_bindings.contains_key(&(
                    self.current_module.clone(),
                    self.current_module_idx,
                    binding.clone(),
                ))
            {
                error = error.with_note(
                    receiver.1.clone(),
                    format!(
                        "lexical binding `{binding}` shadows the imported module; `{binding}.{method}` was resolved as a value method lookup"
                    ),
                );
            }
        }
        self.errors.push(error);
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
#[cfg(test)]
mod tests {
    use super::*;
    use crate::module_registry::ModuleRegistry;

    #[test]
    fn trait_method_target_ids_fail_closed_after_canonical_miss() {
        let mut checker = Checker {
            current_module: Some("app".to_string()),
            ..Checker::default()
        };
        checker.trait_defs.insert(
            "left.Render".to_string(),
            TraitInfo {
                methods: Vec::new(),
                associated_types: Vec::new(),
                type_params: Vec::new(),
            },
        );
        checker
            .published_bare_trait_owners
            .entry((
                checker.current_module.clone(),
                checker.current_module_idx,
                "Render".to_string(),
            ))
            .or_default()
            .insert("left.Render".to_string());

        let wrong_trait = crate::DefId::for_test("right.Render");
        let wrong_method = crate::DefId::for_test("right.Render::render");
        checker
            .trait_method_ids
            .insert("Render::render".to_string(), (wrong_trait, wrong_method));

        assert_eq!(
            checker.trait_method_call_target_ids("Render", "render"),
            None,
            "a canonical lookup miss must not retry the first-write-wins bare key",
        );

        let canonical_trait = crate::DefId::for_test("left.Render");
        let canonical_method = crate::DefId::for_test("left.Render::render");
        checker.trait_method_ids.insert(
            "left.Render::render".to_string(),
            (canonical_trait.clone(), canonical_method.clone()),
        );
        assert_eq!(
            checker.trait_method_call_target_ids("Render", "render"),
            Some((canonical_trait, canonical_method)),
        );
    }

    #[test]
    fn suspending_io_method_results_publish_delivery_ownership() {
        use crate::runtime_call::{
            ProducedArgumentBoundary as Boundary, ProducedValueAcquisition as Acquisition,
            ProducedValueOwnership as Ownership,
        };

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let receiver = (Expr::Identifier("io".to_string()), 1..3);

        let read_span = 10..20;
        let read_key = SpanKey::in_module(&read_span, checker.current_module_idx);
        checker.conn_await_reads.insert(read_key.clone(), true);
        checker.suspending_io_receiver_nominals.insert(
            read_key.clone(),
            crate::stdlib::STD_NET_CONNECTION.to_string(),
        );
        checker.record_resolved_method_call_ownership(
            &receiver,
            "read_string",
            &[],
            &read_span,
            &Ty::String,
        );

        let read = checker
            .resolved_method_call_ownership
            .get(&read_key)
            .expect("suspending read ownership");
        assert_eq!(read.fact.ownership, Ownership::owned(Acquisition::Delivery));
        assert_eq!(read.fact.receiver_boundary, Some(Boundary::Borrow));

        let accept_span = 30..40;
        let accept_key = SpanKey::in_module(&accept_span, checker.current_module_idx);
        checker.listener_await_accepts.insert(accept_key.clone());
        checker.suspending_io_receiver_nominals.insert(
            accept_key.clone(),
            crate::stdlib::STD_NET_LISTENER.to_string(),
        );
        checker.record_resolved_method_call_ownership(
            &receiver,
            "accept",
            &[],
            &accept_span,
            &Ty::Named {
                name: crate::stdlib::STD_NET_CONNECTION.to_string(),
                args: Vec::new(),
                builtin: None,
            },
        );

        let accept = checker
            .resolved_method_call_ownership
            .get(&accept_key)
            .expect("suspending accept ownership");
        assert_eq!(
            accept.fact.ownership,
            Ownership::owned(Acquisition::Delivery)
        );
        assert_eq!(accept.fact.receiver_boundary, Some(Boundary::Borrow));

        let spoofed_span = 50..60;
        let spoofed_key = SpanKey::in_module(&spoofed_span, checker.current_module_idx);
        checker.conn_await_reads.insert(spoofed_key.clone(), true);
        checker.suspending_io_receiver_nominals.insert(
            spoofed_key.clone(),
            crate::stdlib::STD_NET_LISTENER.to_string(),
        );
        checker.record_resolved_method_call_ownership(
            &receiver,
            "read_string",
            &[],
            &spoofed_span,
            &Ty::String,
        );
        let spoofed = checker
            .resolved_method_call_ownership
            .get(&spoofed_key)
            .expect("spoofed suspending read ownership");
        assert_eq!(spoofed.fact.ownership, Ownership::Unknown);
    }

    #[test]
    fn canonical_std_io_bytes_push_requires_provenance_and_checked_signature() {
        let push_signature = FnSig {
            params: vec![Ty::U8],
            return_type: Ty::Unit,
            ..FnSig::default()
        };
        let mut canonical = Checker::default();
        canonical.extern_method_origins.insert(
            "bytes::push".to_string(),
            (Some("std.io".to_string()), true),
        );
        assert_eq!(
            canonical.canonical_std_io_runtime_method_family(
                "bytes::push",
                "hew_bytes_push",
                &push_signature,
            ),
            Some(crate::runtime_call::RuntimeCallFamily::BytesPush),
        );

        let lookalike = Checker::default();
        assert_eq!(
            lookalike.canonical_std_io_runtime_method_family(
                "bytes::push",
                "hew_bytes_push",
                &push_signature,
            ),
            None,
            "a user extern sharing the runtime spelling must remain an Extern call",
        );

        let wrong_signature = FnSig {
            params: vec![Ty::I64],
            return_type: Ty::Unit,
            ..FnSig::default()
        };
        assert_eq!(
            canonical.canonical_std_io_runtime_method_family(
                "bytes::push",
                "hew_bytes_push",
                &wrong_signature,
            ),
            None,
            "the runtime ABI family is not admitted by symbol spelling and arity alone",
        );
    }

    #[test]
    fn wasm_file_stream_function_policy_uses_canonical_std_owner_not_spellings() {
        let span = 0..0;
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.enable_wasm_target();
        checker
            .canonical_std_module_sources
            .insert("std.fs".to_string());
        checker
            .module_import_bindings
            .insert((None, 0, "files".to_string()), "std.fs".to_string());
        checker.reject_wasm_native_only_module_function("files", "try_read", &span);
        assert_eq!(checker.errors.len(), 1, "module alias must reject");

        // Use a fresh checker: the production de-duplication key intentionally
        // suppresses repeated diagnostics at one source span.
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.enable_wasm_target();
        checker
            .canonical_std_module_sources
            .insert("std.fs".to_string());
        checker.reject_wasm_native_only_function_identity("std.fs.try_read", &span);
        assert_eq!(checker.errors.len(), 1, "named-import identity must reject");

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.enable_wasm_target();
        checker
            .module_import_bindings
            .insert((None, 0, "lookalike".to_string()), "app.fs".to_string());
        checker.reject_wasm_native_only_module_function("lookalike", "try_read", &span);
        checker.reject_wasm_native_only_function_identity("app.fs.try_read", &span);
        assert!(
            checker.errors.is_empty(),
            "a user package with the same leaf must not inherit std.fs policy"
        );
    }

    #[test]
    fn ask_reply_send_gate_uses_exact_import_owner_and_fails_closed_without_it() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker
            .registry
            .register_type("hew.replysend.Reply".to_string(), vec![Ty::I64]);
        checker.registry.register_type(
            "hew.replynonsend.Reply".to_string(),
            vec![Ty::Named {
                name: "Rc".to_string(),
                args: vec![Ty::I64],
                builtin: Some(BuiltinType::Rc),
            }],
        );
        checker.module_import_bindings.insert(
            (None, 0, "replysend".to_string()),
            "hew.replysend".to_string(),
        );
        checker.module_import_bindings.insert(
            (None, 0, "replynonsend".to_string()),
            "hew.replynonsend".to_string(),
        );
        let bare_reply = Ty::Named {
            name: "Reply".to_string(),
            args: Vec::new(),
            builtin: None,
        };

        let send = checker
            .send_gate_reply_ty("replysend.Producer::make", &bare_reply)
            .expect("an exact replysend binding and marker row must resolve");
        assert!(matches!(send, Ty::Named { ref name, .. } if name == "hew.replysend.Reply"));
        assert!(checker.registry.implements_marker(&send, MarkerTrait::Send));

        let non_send = checker
            .send_gate_reply_ty("replynonsend.Producer::make", &bare_reply)
            .expect("an exact replynonsend binding and marker row must resolve");
        assert!(matches!(non_send, Ty::Named { ref name, .. } if name == "hew.replynonsend.Reply"));
        assert!(!checker
            .registry
            .implements_marker(&non_send, MarkerTrait::Send));

        assert!(
            checker
                .send_gate_reply_ty("missing.Producer::make", &bare_reply)
                .is_none(),
            "a missing lexical module binding must not fall back to bare Reply"
        );
    }

    #[test]
    fn transport_attach_rewrite_requires_authoritative_qualified_identity() {
        for (receiver, symbol) in [
            (STD_NET_CONNECTION, "hew_tcp_attach_local"),
            ("std.net.tls.TlsStream", "hew_tls_attach_local"),
            ("std.net.websocket.Conn", "hew_ws_attach_local"),
        ] {
            assert_eq!(
                transport_attach_runtime_symbol(receiver, "attach"),
                Some(symbol),
                "canonical transport identity must retain its attach rewrite"
            );
            assert_eq!(
                transport_attach_runtime_symbol(receiver, "close"),
                None,
                "only attach belongs to the compiler-lowered transport path"
            );
        }

        for user_name in ["Connection", "TlsStream", "Conn"] {
            assert_eq!(
                transport_attach_runtime_symbol(user_name, "attach"),
                None,
                "a bare user type named {user_name} must keep ordinary method dispatch"
            );
        }
    }

    #[test]
    fn qualified_method_receiver_restores_only_its_own_return_identity() {
        fn empty_type_def(name: &str) -> TypeDef {
            TypeDef {
                kind: TypeDefKind::Struct,
                name: name.to_string(),
                type_params: Vec::new(),
                bounds: HashMap::new(),
                fields: HashMap::new(),
                field_order: Vec::new(),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                is_indirect: false,
            }
        }

        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        for identity in [
            "net.Listener",
            "net.Connection",
            "foo.Listener",
            "foo.Connection",
        ] {
            checker
                .type_defs
                .insert(identity.to_string(), empty_type_def(identity));
        }
        let bare_connection = Ty::Named {
            name: "Connection".to_string(),
            args: Vec::new(),
            builtin: None,
        };

        assert_eq!(
            checker.qualify_method_return_to_receiver_owner("net.Listener", &bare_connection,),
            Ty::Named {
                name: "net.Connection".to_string(),
                args: Vec::new(),
                builtin: None,
            },
            "a `net.Listener` method's module-local `Connection` return must regain `net` ownership",
        );
        assert_eq!(
            checker.qualify_method_return_to_receiver_owner("foo.Listener", &bare_connection,),
            Ty::Named {
                name: "foo.Connection".to_string(),
                args: Vec::new(),
                builtin: None,
            },
            "a foreign same-short-name method result must retain its own source owner",
        );

        let foreign_connection = Ty::Named {
            name: "foo.Connection".to_string(),
            args: Vec::new(),
            builtin: None,
        };
        assert_eq!(
            checker.qualify_method_return_to_receiver_owner("net.Listener", &foreign_connection,),
            foreign_connection,
            "an already-qualified foreign result is authoritative and must not be rewritten",
        );
        assert_eq!(
            checker.qualify_method_return_to_receiver_owner("Listener", &bare_connection,),
            bare_connection,
            "a root-local receiver has no module owner to project onto its result",
        );
    }

    /// A pending lowering fact whose element type resolves to `Ty::Error` must be
    /// dropped silently by `finalize_lowering_facts` without emitting a new error.
    ///
    /// Background: `validate_hashset_element_type` allows `Ty::Error` through
    /// (correct — avoids cascading diagnostics), which means
    /// `record_hashset_lowering_fact` can be called with `Ty::Error` as the
    /// element type.  Before this fix, `from_hashset_element_type(Ty::Error)`
    /// returned `Err(UnresolvedHashSetElementType)` and the handler emitted a
    /// spurious "element type is unresolved" diagnostic even though the real error
    /// had already been reported upstream.
    #[test]
    fn runtime_stream_element_name_stays_canonical() {
        assert_eq!(
            Checker::runtime_stream_element_name(&Ty::String),
            Some("string")
        );
        assert_eq!(
            Checker::runtime_stream_element_name(&Ty::Bytes),
            Some("bytes")
        );
        assert_eq!(
            Checker::runtime_stream_element_name(&Ty::Named {
                builtin: None,
                name: "string".into(),
                args: vec![],
            }),
            None
        );
        assert_eq!(
            Checker::runtime_stream_element_name(&Ty::Named {
                builtin: None,
                name: "str".into(),
                args: vec![],
            }),
            None
        );
    }

    #[test]
    fn finalize_lowering_facts_silently_drops_error_element_type() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 10..20;
        checker.record_hashset_lowering_fact(&span, &Ty::Error);

        let facts = checker.finalize_lowering_facts();

        assert!(
            facts.is_empty(),
            "a pending fact with Ty::Error element must not appear in the finalized output"
        );
        assert!(
            checker.errors.is_empty(),
            "finalize_lowering_facts must not emit a spurious error for Ty::Error elements; \
             the real error was reported upstream"
        );
    }

    /// A pending lowering fact whose element type is genuinely unresolved
    /// (`Ty::Var`) after inference must be pruned AND must emit an
    /// `InferenceFailed` diagnostic pointing at the lowering site.
    #[test]
    fn finalize_lowering_facts_emits_error_for_unresolved_inference_var() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 30..40;
        let unresolved_var = Ty::Var(crate::ty::TypeVar::fresh());
        checker.record_hashset_lowering_fact(&span, &unresolved_var);

        let facts = checker.finalize_lowering_facts();

        assert!(
            facts.is_empty(),
            "a pending fact with an unresolved Ty::Var element must not appear in the output"
        );
        assert!(
            checker
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InferenceFailed),
            "finalize_lowering_facts must emit InferenceFailed for a genuinely unresolved \
             element type; got: {:?}",
            checker.errors
        );
    }

    // ── HashMap admission finalization ───────────────────────────────────────

    /// A deferred `HashMap` admission whose key type is `Ty::Error` must be
    /// dropped silently — no new diagnostic, no cascade.
    #[test]
    fn finalize_hashmap_admission_silently_drops_error_key() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 10..20;
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::Error,
                val_ty: Ty::I64,
                source_module: None,
                is_abstract_key_param: false,
            },
        );

        checker.finalize_hashmap_admission();

        assert!(
            checker.errors.is_empty(),
            "finalize_hashmap_admission must not emit an error when key_ty is Ty::Error; \
             the upstream diagnostic already covers it. Got: {:?}",
            checker.errors
        );
    }

    /// A deferred `HashMap` admission whose value type is still an unresolved
    /// inference variable after inference must emit `InferenceFailed`.
    #[test]
    fn finalize_hashmap_admission_emits_inference_failed_for_var_value() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 30..40;
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::String,
                val_ty: Ty::Var(TypeVar::fresh()),
                source_module: None,
                is_abstract_key_param: false,
            },
        );

        checker.finalize_hashmap_admission();

        assert!(
            checker
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InferenceFailed),
            "finalize_hashmap_admission must emit InferenceFailed when val_ty is an unresolved \
             Ty::Var; got: {:?}",
            checker.errors
        );
    }

    /// An abstract key parameter already admitted under its generic bounds has
    /// no concrete layout fact until monomorphization substitutes K/V.
    #[test]
    fn finalize_hashmap_admission_skips_abstract_key_param() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 60..70;
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashMapAdmission {
                span: span.clone(),
                key_ty: Ty::normalize_named("K".to_string(), vec![]),
                val_ty: Ty::normalize_named("V".to_string(), vec![]),
                source_module: None,
                is_abstract_key_param: true,
            },
        );

        checker.finalize_hashmap_admission();

        assert!(
            checker.errors.is_empty(),
            "abstract HashMap key params are checked via declared bounds, not layout eligibility; got: {:?}",
            checker.errors
        );
        assert!(
            checker.hashmap_layout_facts.is_empty(),
            "abstract HashMap key params must not produce concrete layout facts"
        );
    }

    #[test]
    fn record_resolved_hashmap_call_abstract_key_emits_resolved_call() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker
            .current_type_param_bounds
            .push(crate::check::types::TypeParamScope::new(
                std::collections::HashMap::from([
                    ("K".to_string(), vec!["Hash".to_string(), "Eq".to_string()]),
                    ("V".to_string(), vec![]),
                ]),
                std::collections::HashMap::new(),
            ));
        let span = 80..90;

        checker.record_resolved_hashmap_call(
            "insert",
            &Ty::normalize_named("K".to_string(), vec![]),
            &Ty::normalize_named("V".to_string(), vec![]),
            &span,
        );

        assert!(
            checker.errors.is_empty(),
            "declared K: Hash + Eq bounds must satisfy HashMap method dispatch; got: {:?}",
            checker.errors
        );
        let call = checker
            .resolved_calls
            .get(&SpanKey::in_module(&span, 0))
            .expect("generic HashMap method dispatch must record a resolved call");
        assert_eq!(call.method_target.symbol_name, "hew_hashmap_insert_layout");
    }

    /// Two deferred `HashMap` admissions sharing the same unresolved
    /// `(key_var, val_var)` pair must emit exactly one `InferenceFailed`.
    #[test]
    fn finalize_hashmap_admission_dedup_pair_emits_single_error() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let key_var = TypeVar::fresh();
        let val_var = TypeVar::fresh();
        let span_a = 100..110;
        let span_b = 200..210;

        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span_a, 0),
            DeferredHashMapAdmission {
                span: span_a.clone(),
                key_ty: Ty::Var(key_var),
                val_ty: Ty::Var(val_var),
                source_module: None,
                is_abstract_key_param: false,
            },
        );
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span_b, 0),
            DeferredHashMapAdmission {
                span: span_b.clone(),
                key_ty: Ty::Var(key_var),
                val_ty: Ty::Var(val_var),
                source_module: None,
                is_abstract_key_param: false,
            },
        );

        checker.finalize_hashmap_admission();

        let inference_failed: Vec<_> = checker
            .errors
            .iter()
            .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
            .collect();
        assert_eq!(
            inference_failed.len(),
            1,
            "two admissions sharing the same (key_var, val_var) pair must emit exactly one \
             InferenceFailed; got {}: {:?}",
            inference_failed.len(),
            checker.errors,
        );
    }

    // ── HashSet admission finalization ───────────────────────────────────────

    /// A deferred `HashSet` admission whose element type is `Ty::Error` must be
    /// dropped silently — mirrors the lowering-facts sentinel for the admission path.
    #[test]
    fn finalize_hashset_admission_silently_drops_error_element() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 50..60;
        checker.deferred_hashset_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashSetAdmission {
                span: span.clone(),
                elem_ty: Ty::Error,
                source_module: None,
            },
        );

        checker.finalize_hashset_admission();

        assert!(
            checker.errors.is_empty(),
            "finalize_hashset_admission must not emit an error when elem_ty is Ty::Error; \
             the upstream diagnostic already covers it. Got: {:?}",
            checker.errors
        );
    }

    /// A deferred `HashSet` admission whose element type is still an unresolved
    /// inference variable after inference must emit `InferenceFailed` with an
    /// "add annotation" hint.
    #[test]
    fn finalize_hashset_admission_emits_inference_failed_for_var_element() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 70..80;
        checker.deferred_hashset_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredHashSetAdmission {
                span: span.clone(),
                elem_ty: Ty::Var(TypeVar::fresh()),
                source_module: None,
            },
        );

        checker.finalize_hashset_admission();

        assert!(
            checker
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InferenceFailed),
            "finalize_hashset_admission must emit InferenceFailed for an unresolved \
             Ty::Var element; got: {:?}",
            checker.errors
        );
    }

    // ── Vec admission finalization ───────────────────────────────────────────

    /// A deferred `Vec` admission whose element type contains `Ty::Error` (via
    /// `contains_error()`) must be dropped silently — no cascade.
    #[test]
    fn finalize_vec_admission_silently_drops_error_element() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 90..100;
        // Nest Ty::Error inside a Vec element to exercise the contains_error() path,
        // not just a bare Ty::Error match.
        let elem_ty = Ty::Named {
            builtin: None,
            name: "Result".into(),
            args: vec![Ty::Error, Ty::I64],
        };
        checker.deferred_vec_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredVecAdmission {
                span: span.clone(),
                elem_ty,
                source_module: None,
            },
        );

        checker.finalize_vec_admission();

        assert!(
            checker.errors.is_empty(),
            "finalize_vec_admission must not emit an error when the element type \
             contains Ty::Error; the upstream diagnostic already covers it. Got: {:?}",
            checker.errors
        );
    }

    /// A deferred `Vec` admission whose element type contains an unresolved
    /// inference variable after inference must emit `InferenceFailed`.
    #[test]
    fn finalize_vec_admission_emits_inference_failed_for_unresolved_var() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let span = 110..120;
        checker.deferred_vec_admission.insert(
            SpanKey::in_module(&span, 0),
            DeferredVecAdmission {
                span: span.clone(),
                elem_ty: Ty::Var(TypeVar::fresh()),
                source_module: None,
            },
        );

        checker.finalize_vec_admission();

        assert!(
            checker
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::InferenceFailed),
            "finalize_vec_admission must emit InferenceFailed for an unresolved \
             Ty::Var element; got: {:?}",
            checker.errors
        );
    }

    // ── HashMap/HashSet layout-symbol rewrite recording ──────────────────────
    //
    // W4.001 Stage C3 hard cutover: the legacy
    // `resolve_hashmap_runtime_symbol` / `resolve_hashset_runtime_symbol`
    // per-V helpers + dual-emit `MethodCallRewrite::RewriteToFunction`
    // arms retired. Resolver-authority via `record_resolved_collection_call`
    // is now the sole admission and dispatch path; coverage lives in
    // `tests/resolved_call_hashmap_coverage.rs` and
    // `tests/resolved_call_hashset_coverage.rs` (catalog-side) plus the
    // C2 `resolved_impl_call_hashmap_layout_descriptor_materialisation`
    // integration test (codegen-side).

    // ── W4.048: descriptor-driven collection method resolver ─────────────────
    //
    // Slice 1 isolation tests for the pure-data descriptor table. These pin the
    // arity / arg-shape / return-shape of every table-driven collection method
    // so a row edit that would silently drift the front-half admission contract
    // fails here before it reaches the behaviour-diff corpus.

    fn arity_of(kind: CollectionKind, method: &str) -> Option<usize> {
        collection_method_desc(kind, method)
            .expect("known method")
            .arity
    }

    #[test]
    fn descriptor_table_arity_skips_len_and_is_empty() {
        // `len`/`is_empty` historically never called `check_arity`; the
        // `Option<usize>` arity field must encode that asymmetry as `None`.
        for kind in [CollectionKind::HashMap, CollectionKind::HashSet] {
            assert_eq!(arity_of(kind, "len"), None, "{kind:?}::len skips arity");
            assert_eq!(
                arity_of(kind, "is_empty"),
                None,
                "{kind:?}::is_empty skips arity"
            );
        }
    }

    #[test]
    fn descriptor_table_checked_arities() {
        assert_eq!(arity_of(CollectionKind::HashMap, "insert"), Some(2));
        // HashMap `get` is no longer in the descriptor table: it is trait-routed
        // (`<HashMap<K, V> as Index>::get -> Option<V>`) through the explicit
        // `check_hashmap_method` arm, not the collection driver (mirrors Vec).
        assert!(collection_method_desc(CollectionKind::HashMap, "get").is_none());
        assert_eq!(arity_of(CollectionKind::HashMap, "keys"), Some(0));
        assert_eq!(arity_of(CollectionKind::HashMap, "entries"), Some(0));
        assert_eq!(arity_of(CollectionKind::HashSet, "insert"), Some(1));
        assert_eq!(arity_of(CollectionKind::HashSet, "contains"), Some(1));
        assert_eq!(arity_of(CollectionKind::HashSet, "clone"), Some(0));
    }

    #[test]
    fn descriptor_table_arg_and_return_shapes() {
        let hm_insert = collection_method_desc(CollectionKind::HashMap, "insert").unwrap();
        assert_eq!(
            hm_insert.arg_templates,
            &[ArgTemplate::Key, ArgTemplate::Value]
        );
        assert_eq!(hm_insert.ret, RetTemplate::Unit);

        // HashMap `get` is intentionally absent from the descriptor table: the
        // accessor is trait-routed through `Index<K>` (see
        // `descriptor_table_checked_arities`).
        assert!(collection_method_desc(CollectionKind::HashMap, "get").is_none());

        let hm_keys = collection_method_desc(CollectionKind::HashMap, "keys").unwrap();
        assert_eq!(hm_keys.ret, RetTemplate::VecOfKey);
        let hm_values = collection_method_desc(CollectionKind::HashMap, "values").unwrap();
        assert_eq!(hm_values.ret, RetTemplate::VecOfVal);
        let hm_entries = collection_method_desc(CollectionKind::HashMap, "entries").unwrap();
        assert_eq!(hm_entries.ret, RetTemplate::VecOfPair);

        let set_insert = collection_method_desc(CollectionKind::HashSet, "insert").unwrap();
        assert_eq!(set_insert.arg_templates, &[ArgTemplate::Elem]);
        assert_eq!(set_insert.ret, RetTemplate::Bool);

        for clone_kind in [CollectionKind::HashMap, CollectionKind::HashSet] {
            assert_eq!(
                collection_method_desc(clone_kind, "clone").unwrap().ret,
                RetTemplate::SelfTy,
                "{clone_kind:?}::clone returns Self"
            );
        }
    }

    #[test]
    fn descriptor_table_unknown_and_divergent_methods_have_no_row() {
        // Unknown methods → fail-closed fallback (no descriptor row).
        assert!(collection_method_desc(CollectionKind::HashMap, "frobnicate").is_none());
    }

    #[test]
    fn builtin_vec_signatures_match_source_authority() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.register_builtins();
        for spec in crate::vec_authority::method_specs() {
            let sig = checker
                .lookup_builtin_vec_method_sig(&[Ty::I64], &spec.name)
                .unwrap_or_else(|| panic!("missing source signature for Vec::{}", spec.name));
            assert_eq!(
                sig.extern_symbol
                    .as_ref()
                    .map(|symbol| symbol.template.raw.as_str()),
                Some(spec.template.raw.as_str()),
                "Vec::{} signature/template drift",
                spec.name
            );
        }
    }
}
