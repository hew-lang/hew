#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::builtin_names::BuiltinNamedType;
use crate::check::admissibility::compute_copy_record_layout;
use crate::check::calls::SignatureArgApplication;
use crate::check::dispatch::resolve_method_call;
use crate::check::types::BareActorResolution;
use crate::hash_eligibility::{ty_is_hash_eligible, HashEligibility};
use crate::lowering_facts::{
    hashmap_layout_key_fact, hashmap_layout_key_layout_value_fact, hashset_layout_fact,
    HashMapValueType,
};
use crate::method_resolution::{
    collect_method_sigs_for_receiver, instantiate_builtin_result_option_method_sig,
    lookup_builtin_method_sig, lookup_named_method_sig as shared_lookup_named_method_sig,
};
use crate::traits::RcFreeStatus;
use crate::BuiltinType;

/// Map a `Vec<T>` element method to its W5.016 owned-element runtime symbol.
///
/// Returns `None` for methods the owned runtime ops do not implement; the
/// caller keeps those fail-closed. Mirrors the `_layout` family but routes to
/// `hew_vec_*_owned`, which deep-clone on push/set/clone, borrow on get, and
/// move on pop, dropping each element exactly once via the descriptor `drop_fn`.
fn owned_vec_runtime_symbol(method: &str) -> Option<&'static str> {
    match method {
        "push" => Some("hew_vec_push_owned"),
        "get" => Some("hew_vec_get_owned"),
        "set" => Some("hew_vec_set_owned"),
        "pop" => Some("hew_vec_pop_owned"),
        "clone" => Some("hew_vec_clone_owned"),
        _ => None,
    }
}

/// Which builtin collection a [`check_collection_method`](Checker::check_collection_method)
/// call is resolving.  This is the single discriminant the descriptor-driven
/// resolver dispatches on; it carries no per-method behaviour itself.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) enum CollectionKind {
    HashMap,
    HashSet,
    Vec,
}

impl CollectionKind {
    /// User-facing collection name used in arity contexts and `UndefinedMethod`
    /// diagnostics (e.g. `` `HashMap::insert` ``, `no method `x` on Vec`).
    fn name(self) -> &'static str {
        match self {
            CollectionKind::HashMap => "HashMap",
            CollectionKind::HashSet => "HashSet",
            CollectionKind::Vec => "Vec",
        }
    }

    fn builtin(self) -> BuiltinType {
        match self {
            CollectionKind::HashMap => BuiltinType::HashMap,
            CollectionKind::HashSet => BuiltinType::HashSet,
            CollectionKind::Vec => BuiltinType::Vec,
        }
    }
}

/// Outcome of [`Checker::record_clone_admissibility`].
#[derive(Debug)]
pub(super) enum RecordCloneAdmissibility {
    /// The record can be cloned end-to-end via
    /// `__hew_record_clone_inplace_<R>`.
    Admissible,
    /// The record (or a transitive field) contains an opaque handle; fail closed.
    OpaqueField { opaque_name: String },
    /// The record has un-substituted generic type parameters; not yet supported.
    GenericRecord,
    /// The named type is not a clone-eligible record kind (actor, machine, enum).
    NotARecord,
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
    /// `HashSet` / `Vec` element type `elem`.
    Elem,
    /// `i64` (Vec index / count arguments).
    I64,
    /// The receiver type itself (Vec `append`/`extend`).
    Receiver,
}

/// Pure-data shape of a collection-method return type, instantiated against the
/// receiver's concrete `K`/`V`/`elem`/`Self` at the call site.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum RetTemplate {
    Unit,
    Bool,
    I64,
    /// `Option<V>` (`HashMap` `get`).
    OptionVal,
    /// The element type `elem` (Vec `pop`/`get`).
    Elem,
    /// `Vec<K>` (`HashMap` `keys`).
    VecOfKey,
    /// `Vec<V>` (`HashMap` `values`).
    VecOfVal,
    /// The receiver collection type itself (`clone`).
    SelfTy,
}

/// The data descriptor for one `(collection, method)` pair: arity, argument
/// shape, and return shape.  This is the single source of truth for the shared
/// arity → arg → return *walk*; the per-collection element validation, lowering
/// facts, recording, and the Vec symbol-override remain code-side hooks
/// (`dedup-semantic-boundary`: centralise the walk, not the decision).
#[derive(Clone, Copy, Debug)]
pub(super) struct CollectionMethodDesc {
    /// `Some(n)` checks arity against `n`; `None` deliberately skips the arity
    /// check (preserving `len`/`is_empty`/`set`/`append`/`extend`/`contains`
    /// arms that historically never called `check_arity`).
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
/// either unknown (→ fail-closed fallback) or genuinely divergent and handled
/// as explicit code hooks (Vec `contains`/`map`/`filter`/`fold`/`join`).
#[allow(
    clippy::match_same_arms,
    reason = "rows are kept per-method even when arity/arg/ret coincide; their downstream validation/record hooks differ (e.g. HashMap remove uses the owned validator, contains_key the key_value one)"
)]
fn collection_method_desc(kind: CollectionKind, method: &str) -> Option<CollectionMethodDesc> {
    use ArgTemplate::{Elem, Key, Receiver, Value, I64};
    use RetTemplate::{
        Bool, Elem as RetElem, OptionVal, SelfTy, Unit, VecOfKey, VecOfVal, I64 as RetI64,
    };
    Some(match kind {
        CollectionKind::HashMap => match method {
            "insert" => desc(Some(2), &[Key, Value], Unit),
            "get" => desc(Some(1), &[Key], OptionVal),
            "remove" => desc(Some(1), &[Key], Bool),
            "contains_key" => desc(Some(1), &[Key], Bool),
            "keys" => desc(Some(0), &[], VecOfKey),
            "values" => desc(Some(0), &[], VecOfVal),
            "clone" => desc(Some(0), &[], SelfTy),
            "len" => desc(None, &[], RetI64),
            "is_empty" => desc(None, &[], Bool),
            _ => return None,
        },
        CollectionKind::HashSet => match method {
            "insert" => desc(Some(1), &[Elem], Bool),
            "contains" | "remove" => desc(Some(1), &[Elem], Bool),
            "clone" => desc(Some(0), &[], SelfTy),
            "len" => desc(None, &[], RetI64),
            "is_empty" => desc(None, &[], Bool),
            "clear" => desc(None, &[], Unit),
            _ => return None,
        },
        CollectionKind::Vec => match method {
            "push" => desc(Some(1), &[Elem], Unit),
            "pop" => desc(Some(0), &[], RetElem),
            "len" => desc(None, &[], RetI64),
            "get" => desc(Some(1), &[I64], RetElem),
            "remove" => desc(Some(1), &[I64], Unit),
            "is_empty" => desc(None, &[], Bool),
            "clear" => desc(Some(0), &[], Unit),
            "clone" => desc(Some(0), &[], SelfTy),
            "set" => desc(None, &[I64, Elem], Unit),
            "append" | "extend" => desc(None, &[Receiver], Unit),
            _ => return None,
        },
    })
}

/// The receiver's concrete type arguments for a collection method call, carried
/// through the descriptor-driven driver.  Only the fields relevant to a given
/// `CollectionKind` are meaningful (`HashMap` uses `key`/`val`; `HashSet`/`Vec` use
/// `elem`; `Vec` additionally uses `receiver`/`resolved`); the rest hold the same
/// placeholder fresh inference vars the legacy resolvers used.
pub(super) struct CollectionTyCx {
    key: Ty,
    val: Ty,
    elem: Ty,
    receiver: Ty,
    resolved: Ty,
}

impl CollectionTyCx {
    /// `HashMap<K, V>` receiver type context.
    fn hashmap(key: Ty, val: Ty) -> Self {
        Self {
            key,
            val,
            elem: Ty::Unit,
            receiver: Ty::Unit,
            resolved: Ty::Unit,
        }
    }

    /// `HashSet<elem>` receiver type context.
    fn hashset(elem: Ty) -> Self {
        Self {
            key: Ty::Unit,
            val: Ty::Unit,
            elem,
            receiver: Ty::Unit,
            resolved: Ty::Unit,
        }
    }

    /// `Vec<elem>` receiver type context, carrying the receiver and resolved
    /// receiver types the Vec arms need (`append`/`extend` and `clone`).
    fn vec(elem: Ty, receiver: Ty, resolved: Ty) -> Self {
        Self {
            key: Ty::Unit,
            val: Ty::Unit,
            elem,
            receiver,
            resolved,
        }
    }
}

impl Checker {
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
                        match ty_is_hash_eligible(&resolved_ty, &type_defs_snapshot) {
                            HashEligibility::Eligible => {
                                if let Some(ref td) = self.lookup_type_def(name) {
                                    if let Some((elem_size, elem_align)) =
                                        compute_copy_record_layout(td, &type_defs_snapshot)
                                    {
                                        let fact = hashset_layout_fact(
                                            name.clone(),
                                            elem_size,
                                            elem_align,
                                        );
                                        self.hashset_layout_facts.insert(span_key, fact);
                                        // Fact inserted into hashset_layout_facts;
                                        // NOT inserted into lowering_facts result.
                                    } else {
                                        let span = span_key.start..span_key.end;
                                        let mut err = crate::error::TypeError::new(
                                            TypeErrorKind::InvalidOperation,
                                            span,
                                            format!(
                                                "HashSet element type `{name}` has zero size \
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
                                }
                                // TypeDef not found — silently drop; lookup failure
                                // is a pre-existing error from the type-resolution pass.
                            }
                            HashEligibility::IneligibleFloat(bad_ty) => {
                                let span = span_key.start..span_key.end;
                                let mut err = crate::error::TypeError::new(
                                    TypeErrorKind::InvalidOperation,
                                    span,
                                    format!(
                                        "HashSet element type `{name}` contains a floating-point \
                                         field (`{}`); floats are not hash-eligible because \
                                         NaN != NaN would corrupt element lookup semantics",
                                        bad_ty.user_facing(),
                                    ),
                                );
                                if let Some(module) = &pending_fact.source_module {
                                    err = err.with_source_module(module.clone());
                                }
                                new_errors.push(err);
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
                                        "HashSet element type `{name}` contains a managed field \
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
                                        "HashSet element type `{name}` contains a field of type \
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
                                        "HashSet element type `{}` must be a `record`-keyword type \
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

                match ty_is_hash_eligible(&resolved_key, &type_defs_snapshot) {
                    HashEligibility::Eligible => {
                        // Key is eligible. Look up the TypeDef for layout computation.
                        let key_type_def = self.lookup_type_def(key_name);
                        match key_type_def {
                            Some(ref td) => {
                                match compute_copy_record_layout(td, &type_defs_snapshot) {
                                    Some((key_size, key_align)) => {
                                        // Determine value type routing.
                                        match HashMapValueType::from_ty(&resolved_val) {
                                            Ok(HashMapValueType::Layout) => {
                                                // Value is also a Named record.
                                                if let Ty::Named { name: val_name, .. } =
                                                    &resolved_val
                                                {
                                                    let val_type_def =
                                                        self.lookup_type_def(val_name);
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
                                                                    new_layout_facts
                                                                        .push((span_key, fact));
                                                                }
                                                                None => {
                                                                    let mut err = crate::error::TypeError::new(
                                                                        TypeErrorKind::InvalidOperation,
                                                                        check.span.clone(),
                                                                        format!(
                                                                            "HashMap value type `{val_name}` has zero size or contains a type whose layout cannot be determined; layout-value types must have non-zero size",
                                                                        ),
                                                                    );
                                                                    if let Some(module) =
                                                                        check.source_module
                                                                    {
                                                                        err = err
                                                                            .with_source_module(
                                                                                module,
                                                                            );
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
                                                                    "HashMap value type `{val_name}` is not defined; cannot compute layout for layout-key HashMap",
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
                                        // Zero-size record or layout cannot be computed.
                                        let mut err = crate::error::TypeError::new(
                                            TypeErrorKind::InvalidOperation,
                                            check.span.clone(),
                                            format!(
                                                "HashMap key type `{key_name}` has zero size or contains a type \
                                                 whose layout cannot be determined; layout keys must have non-zero size",
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
                                // TypeDef not found — the Named type is not a user-defined record in scope.
                                let mut err = crate::error::TypeError::new(
                                    TypeErrorKind::InvalidOperation,
                                    check.span.clone(),
                                    format!(
                                        "HashMap key type `{key_name}` is not defined; \
                                         cannot verify hash eligibility for layout-key HashMap",
                                    ),
                                );
                                if let Some(module) = check.source_module {
                                    err = err.with_source_module(module);
                                }
                                new_errors.push(err);
                            }
                        }
                    }

                    HashEligibility::IneligibleFloat(bad_ty) => {
                        let mut err = crate::error::TypeError::new(
                            TypeErrorKind::InvalidOperation,
                            check.span.clone(),
                            format!(
                                "HashMap key type `{key_name}` contains a floating-point field \
                                 (`{}`); floats are not hash-eligible because NaN != NaN would \
                                 corrupt key lookup semantics",
                                bad_ty.user_facing(),
                            ),
                        );
                        if let Some(module) = check.source_module {
                            err = err.with_source_module(module);
                        }
                        new_errors.push(err);
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
                                TypeDefKind::Struct => "a struct",
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
    pub(super) fn finalize_channel_rewrites(&mut self) {
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
                    format!("Channel<{resolved}> is not supported: {reason}"),
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
                    span_key,
                    MethodCallRewrite::RewriteToFunction {
                        c_symbol: c_symbol.to_string(),
                        descriptor,
                        elem_ty: None,
                        consumes_receiver,
                    },
                );
            } else {
                let mut err = crate::error::TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    span_key.start..span_key.end,
                    format!(
                        "internal compiler error: builtin {}::{} is missing runtime rewrite metadata",
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
        self.method_call_receiver_kinds
            .insert(SpanKey::in_module(span, self.current_module_idx), kind);
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
        method: &str,
        sig: &FnSig,
    ) -> bool {
        if method != "close" || sig.requires_mutable_receiver {
            return false;
        }
        // `resource_types` is keyed by the declared (bare) type name. The
        // receiver type name may arrive module-qualified (`mod.Conn`) for an
        // imported handle type; match on the unqualified suffix too.
        let unqualified = type_name
            .rsplit_once('.')
            .map_or(type_name, |(_, suffix)| suffix);
        self.resource_types.contains(type_name) || self.resource_types.contains(unqualified)
    }

    /// Apply the consume-receiver marker for a trait-dispatched method call:
    /// record the per-call-site flag for codegen and mark the receiver
    /// expression moved so subsequent uses surface `UseAfterMove`. No-op
    /// when the resolved method does not declare `consumes_receiver`.
    fn apply_consume_receiver_if_flagged(
        &mut self,
        qualified_method: &str,
        receiver: &Spanned<Expr>,
        receiver_ty: &Ty,
        span: &Span,
    ) {
        if !self.is_consume_receiver_method(qualified_method) {
            return;
        }
        self.method_call_consumes_receiver
            .insert(SpanKey::in_module(span, self.current_module_idx));
        let resolved_ty = self.subst.resolve(receiver_ty);
        self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_ty);
    }

    fn record_method_call_rewrite(&mut self, span: &Span, rewrite: MethodCallRewrite) {
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
    /// through that qualified identity. Root / flat-file actors (bare identity,
    /// no `.`) and replies whose qualified form is not registered fall back to
    /// the bare type unchanged.
    fn send_gate_reply_ty(&self, method_id: &str, resolved_reply: &Ty) -> Ty {
        let Ty::Named {
            name,
            args,
            builtin,
        } = resolved_reply
        else {
            return resolved_reply.clone();
        };
        // Already qualified, or a builtin/generic — nothing to re-key.
        if builtin.is_some() || name.contains('.') {
            return resolved_reply.clone();
        }
        let Some((actor_identity, _method)) = method_id.rsplit_once("::") else {
            return resolved_reply.clone();
        };
        let Some((module_short, _actor)) = actor_identity.rsplit_once('.') else {
            return resolved_reply.clone();
        };
        let qualified = format!("{module_short}.{name}");
        if self.registry.has_type_markers(&qualified) {
            Ty::Named {
                name: qualified,
                args: args.clone(),
                builtin: *builtin,
            }
        } else {
            resolved_reply.clone()
        }
    }

    fn record_actor_method_dispatch(&mut self, span: &Span, method_id: String, reply_ty: Ty) {
        let resolved_reply = self.subst.resolve(&reply_ty);
        let dispatch = if matches!(resolved_reply, Ty::Unit) {
            ActorMethodKind::Fire(method_id)
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
            let send_check_ty = self.send_gate_reply_ty(&method_id, &resolved_reply);
            if !send_check_ty.has_inference_var()
                && !send_check_ty.contains_error()
                && !self
                    .registry
                    .implements_marker(&send_check_ty, MarkerTrait::Send)
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
            ActorMethodKind::Ask(method_id, reply_ty)
        };
        self.actor_method_dispatch
            .insert(SpanKey::in_module(span, self.current_module_idx), dispatch);
    }

    fn canonical_handle_receiver_type_name(&self, receiver_ty: &Ty) -> Option<String> {
        let Ty::Named { name, .. } = receiver_ty else {
            return None;
        };
        if self.module_registry.is_handle_type(name) {
            return Some(name.clone());
        }
        if name.contains('.') {
            return Some(name.clone());
        }
        self.module_registry.qualify_handle_type(name)
    }

    fn record_handle_method_call_receiver_kind_if_any(&mut self, receiver_ty: &Ty, span: &Span) {
        let Some(type_name) = self.canonical_handle_receiver_type_name(receiver_ty) else {
            return;
        };
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::HandleInstance { type_name },
        );
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
    fn record_runtime_method_call_rewrite(&mut self, span: &Span, c_symbol: impl Into<String>) {
        let c_symbol = c_symbol.into();
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
        let descriptor =
            crate::runtime_call::RuntimeCallFamily::from_c_symbol(&c_symbol).map(|family| {
                crate::runtime_call::RuntimeCallDescriptor::new(family, None)
                    .expect("substrate variant rejects elem; runtime symbols never carry elem here")
            });
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteToFunction {
                c_symbol,
                descriptor,
                elem_ty: None,
                consumes_receiver,
            },
        );
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
    fn record_extern_symbol_method_call_rewrite(&mut self, span: &Span, c_symbol: String) {
        let consumes_receiver = crate::builtin_names::runtime_symbol_consumes_receiver(&c_symbol);
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteToFunction {
                c_symbol,
                descriptor: None,
                elem_ty: None,
                consumes_receiver,
            },
        );
    }

    fn record_monomorphic_extern_symbol_rewrite_if_any(
        &mut self,
        sig: &FnSig,
        span: &Span,
    ) -> bool {
        let Some(spec) = &sig.extern_symbol else {
            return false;
        };
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
        self.record_extern_symbol_method_call_rewrite(span, spec.template.raw.clone());
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
        if spec.template.is_monomorphic() {
            self.record_extern_symbol_method_call_rewrite(span, spec.template.raw.clone());
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
        self.record_extern_symbol_method_call_rewrite(span, expanded);
        true
    }

    fn record_builtin_option_result_method_rewrite_if_any(
        &mut self,
        receiver_type_name: &str,
        type_args: &[Ty],
        method: &str,
        span: &Span,
    ) -> bool {
        use crate::check::OptionResultMethod as M;
        use MethodCallRewrite::BuiltinOptionResult;

        let Some(marker) = (match (receiver_type_name, method) {
            ("Option", "is_some") => Some(M::OptionIsSome),
            ("Option", "is_none") => Some(M::OptionIsNone),
            ("Option", "unwrap") => Some(M::OptionUnwrap),
            ("Option", "unwrap_or") => Some(M::OptionUnwrapOr),
            ("Result", "is_ok") => Some(M::ResultIsOk),
            ("Result", "is_err") => Some(M::ResultIsErr),
            ("Result", "unwrap") => Some(M::ResultUnwrap),
            ("Result", "unwrap_or") => Some(M::ResultUnwrapOr),
            _ => None,
        }) else {
            return false;
        };

        let expected_args = if receiver_type_name == "Option" { 1 } else { 2 };
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
                arity_context: format!("method '{method}'"),
            },
            true,
        );
        self.record_monomorphic_extern_symbol_rewrite_if_any(&sig, span);
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
                "internal compiler error: builtin {builtin}::{method} is missing {item} metadata"
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
    ) {
        self.record_method_call_rewrite(
            span,
            MethodCallRewrite::RewriteModuleQualifiedToFunction {
                c_symbol: c_symbol.into(),
                elem_ty: None,
            },
        );
    }

    fn record_deferred_method_call_rewrite(&mut self, span: &Span) {
        self.record_method_call_rewrite(span, MethodCallRewrite::DeferToLowering);
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
        // Active-mode `conn.attach(handler)`: rewrite to `hew_tcp_attach_local`,
        // a callee-name-dispatch symbol the LLVM backend intercepts. The backend
        // resolves the concrete actor type from the `handler` arg's recorded
        // `LocalPid<Actor>` (the `LocalPid<ConcreteActor>` → `LocalPid<Handler>`
        // coercion deliberately does not erase that recorded type), synthesises
        // the `on_data` / `on_close` `msg_id`s from the actor's protocol
        // descriptor, and emits the runtime attach ABI. The `attach` impl body
        // is a stub, so the handle-method auto-derivation below produces nothing;
        // this explicit rewrite is the authority. Mirrors `RemotePid::tell`.
        if (name == "Connection" || name == "net.Connection") && method == "attach" {
            self.record_runtime_method_call_rewrite(span, "hew_tcp_attach_local");
            return;
        }
        if let Some(c_symbol) = self.module_registry.resolve_handle_method(name, method) {
            self.record_runtime_method_call_rewrite(span, c_symbol);
        }
    }

    fn record_module_qualified_stdlib_call_rewrite_if_any(
        &mut self,
        module_name: &str,
        method: &str,
        span: &Span,
    ) {
        if self.user_modules.contains(module_name) {
            return;
        }
        if let Some(c_symbol) = self
            .module_registry
            .resolve_module_call(module_name, method)
        {
            let symbol = if c_symbol == method {
                let qualified = format!("{module_name}.{method}");
                if !self.fn_sigs.contains_key(&qualified) {
                    return;
                }
                if module_name == "math" && Self::is_direct_math_intrinsic(method) {
                    method.to_string()
                } else {
                    qualified
                }
            } else {
                c_symbol
            };
            self.record_module_qualified_method_call_rewrite(span, symbol);
        }
    }

    fn is_direct_math_intrinsic(method: &str) -> bool {
        matches!(
            method,
            "sqrt"
                | "exp"
                | "log"
                | "sin"
                | "cos"
                | "abs"
                | "min"
                | "max"
                | "abs_f"
                | "min_f"
                | "max_f"
                | "pow"
                | "floor"
                | "ceil"
                | "round"
        )
    }

    fn generic_math_intrinsic_op(module_name: &str, method: &str) -> Option<MathGenericOp> {
        if module_name != "math" {
            return None;
        }
        match method {
            "abs" => Some(MathGenericOp::Abs),
            "min" => Some(MathGenericOp::Min),
            "max" => Some(MathGenericOp::Max),
            _ => None,
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
    fn record_module_qualified_user_call_rewrite_if_any(
        &mut self,
        module_name: &str,
        method: &str,
        span: &Span,
    ) {
        if !self.user_modules.contains(module_name) {
            return;
        }
        let key = format!("{module_name}.{method}");
        if self.fn_sigs.contains_key(&key) {
            self.record_module_qualified_method_call_rewrite(span, key);
        }
    }

    fn reject_if_wasm_native_only_network_module_call(&mut self, module_name: &str, span: &Span) {
        if self.user_modules.contains(module_name) {
            return;
        }
        match module_name {
            "http_client" => self.reject_wasm_feature(span, WasmUnsupportedFeature::HttpClient),
            "smtp" => self.reject_wasm_feature(span, WasmUnsupportedFeature::Smtp),
            _ => {}
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
            "http_client.Response" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::HttpClient);
            }
            "smtp.Conn" => self.reject_wasm_feature(span, WasmUnsupportedFeature::Smtp),
            "process.Child" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::ProcessExecution);
            }
            "http.Server" | "http.Request" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::HttpServer);
            }
            "net.Listener" | "net.Connection" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::TcpNetworking);
            }
            "tls.TlsStream" => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Tls);
            }
            "quic.QUICEndpoint" | "quic.QUICConnection" | "quic.QUICStream" | "quic.QUICEvent" => {
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
        if name != "semaphore.Semaphore" || self.user_modules.contains("semaphore") {
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

    fn stream_receiver_element_kind(ty: &Ty) -> &'static str {
        match ty {
            Ty::String => "string",
            Ty::Bytes => "bytes",
            _ => "",
        }
    }

    pub(super) fn strip_module_prefix<'a>(&self, name: &'a str) -> Option<&'a str> {
        let dot = name.find('.')?;
        if self.modules.contains(&name[..dot]) {
            Some(&name[dot + 1..])
        } else {
            None
        }
    }

    /// Look up a type definition, handling module-qualified names like `json.Value`.
    pub(super) fn lookup_type_def(&self, name: &str) -> Option<TypeDef> {
        self.type_defs
            .get(name)
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
        if !self.modules.contains(module_short) {
            return None;
        }
        let exports = self.module_type_exports.get(module_short)?;
        if !exports.contains(type_name) {
            return None;
        }
        let qualified = format!("{module_short}.{type_name}");
        self.type_defs.get(&qualified).cloned()
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

    /// Look up a non-builtin named method via `type_defs` first, then `fn_sigs`.
    /// The last `.`-separated segment of `current_module`, i.e. the *short*
    /// module name used as the qualified type-alias prefix (`websocket` for
    /// `std.net.websocket`). Returns `None` at the root / flat namespace, where
    /// type names are unqualified.
    pub(super) fn current_module_short(&self) -> Option<&str> {
        self.current_module
            .as_deref()
            .map(|m| m.rsplit('.').next().unwrap_or(m))
    }

    /// Resolve a bare actor reference to its registered checker identity.
    ///
    /// Resolution order (local-first, mirroring `per-module-type-identity`):
    /// 1. the current module's own actor (`{current_short}.{name}`)
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
        if let Some(short) = self.current_module_short() {
            let dotted = format!("{short}.{name}");
            if is_actor(&dotted) {
                return BareActorResolution::Resolved(dotted);
            }
        }
        if is_actor(name) {
            return BareActorResolution::Resolved(name.to_string());
        }
        if let Some(module) = self
            .unqualified_to_module
            .get(&(self.current_module.clone(), name.to_string()))
        {
            let dotted = format!("{module}.{name}");
            if is_actor(&dotted) {
                return BareActorResolution::Resolved(dotted);
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
        let short = self.current_module_short()?;
        let qualified = format!("{short}.{type_name}");
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
                self.module_registry
                    .resolve_handle_method_sig(type_name, method)
                    .map(|(_c_symbol, params, return_type)| FnSig {
                        params,
                        return_type,
                        ..FnSig::default()
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
        Some(instantiate_builtin_result_option_method_sig(
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
        let sig = self.lookup_named_method_sig(name, type_args, method)?;
        Some(
            self.apply_instantiated_call_signature(
                &sig,
                None,
                args,
                span,
                SignatureArgApplication::PositionalOnly {
                    arity_context: format!("method '{method}'"),
                },
                true,
            )
            .return_type,
        )
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
        if missing.is_empty() {
            "it is outside the current Serializable subset (scalars, string, bytes, \
             tuples/arrays, records/enums, or wire-marked types whose members are Serializable)"
                .to_string()
        } else {
            format!("missing required marker trait(s): {}", missing.join(" + "))
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
            } if trait_name.as_ref() == "Pid" && assoc_name.as_ref() == "Msg"
        )
    }

    fn report_pid_polymorphic_tell_fail_closed(
        &mut self,
        type_param_name: &str,
        ty: &Ty,
        span: &Span,
    ) {
        self.report_error(
            TypeErrorKind::BoundsNotSatisfied,
            span,
            format!(
                "generic `Pid::tell` on `{type_param_name}` is fail-closed: `{}` must be proven \
                 Serializable, but the current checker cannot express the required \
                 `P::Msg: Serializable` associated-type projection bound yet (TODO A640)",
                ty.user_facing()
            ),
        );
    }

    fn enforce_pid_polymorphic_tell_serializable_args(
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
                    self.report_pid_polymorphic_tell_fail_closed(type_param_name, &resolved, sp);
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
                                "actor ask `{name}::{method_name}` requires `await`; \
                                 write `let v? = await ref.{method_name}(...)` \
                                 or `match await ref.{method_name}(...) {{ Ok(v) => ..., Err(e) => ... }}`",
                            ),
                        );
                        // Still record the dispatch so HIR/MIR have a sane entry; the
                        // type checker already emitted the error so this is recovery.
                    }
                    self.record_actor_method_dispatch(span, method_key, ty.clone());
                } else {
                    self.record_method_call_receiver_kind(
                        span,
                        MethodCallReceiverKind::NamedTypeInstance {
                            type_name: name.clone(),
                        },
                    );
                }
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
                        if !self.user_clone_record_seeds.contains(name) {
                            self.user_clone_record_seeds.push(name.clone());
                        }
                        return record_ty;
                    }
                    RecordCloneAdmissibility::OpaqueField { opaque_name } => {
                        // Synthesize args (none here) for error-recovery symmetry.
                        self.report_error(
                            TypeErrorKind::UndefinedMethod,
                            span,
                            format!(
                                "record `{name}` contains an opaque field `{opaque_name}` \
                                 and cannot be cloned"
                            ),
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
                    RecordCloneAdmissibility::NotARecord => {
                        // Fall through to `UndefinedMethod` below for non-record Named types
                        // (actors, machines, enums, etc.).
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
    /// `clone`. Returns one of four outcomes:
    ///
    /// - `Admissible`: the record can be cloned end-to-end via the synthesised
    ///   `__hew_record_clone_inplace_<R>` thunk.
    /// - `OpaqueField { opaque_name }`: the record (or a transitively reachable
    ///   field) contains an opaque handle — fail closed with a named diagnostic.
    /// - `GenericRecord`: the record has un-substituted generic type parameters
    ///   — not yet supported; fail closed with an NYI diagnostic.
    /// - `NotARecord`: not a clone-eligible named type (actor, machine, enum,
    ///   etc.) — fall through to `UndefinedMethod`.
    ///
    /// LESSONS: `checker-authority` (sole authority for clone admissibility),
    /// `unclonable-leaf-fails-closed-transitively`, `admit-only-what-you-lower`.
    pub(super) fn record_clone_admissibility(
        &self,
        name: &str,
        type_args: &[Ty],
        _span: &Span,
    ) -> RecordCloneAdmissibility {
        use TypeDefKind::{Record, Struct};
        let Some(type_def) = self.type_defs.get(name) else {
            return RecordCloneAdmissibility::NotARecord;
        };
        // Only Record and Struct (value-type) kinds are clone-eligible.
        if !matches!(type_def.kind, Record | Struct) {
            return RecordCloneAdmissibility::NotARecord;
        }
        // Generic records (un-substituted type params) are not yet supported.
        // The caller passes the `type_args` from the `Ty::Named`; if the type has
        // declared params but the call-site args are still unresolved vars, reject.
        if !type_def.type_params.is_empty() && type_args.iter().any(|a| matches!(a, Ty::Var(_))) {
            return RecordCloneAdmissibility::GenericRecord;
        }
        // Check each field transitively for opaque handles.
        // Re-derives the checker-side `ty_contains_owned_handle` walk inline to
        // avoid importing hew-mir (wrong dependency direction). The authoritative
        // MIR-side `ty_contains_unclonable_opaque` is mirrored here at the checker
        // boundary; the two must agree but are structurally independent.
        if let Some(opaque_name) =
            self.record_field_contains_opaque(name, &mut std::collections::HashSet::new())
        {
            return RecordCloneAdmissibility::OpaqueField { opaque_name };
        }
        RecordCloneAdmissibility::Admissible
    }

    /// Transitive walk of a record's fields looking for an opaque handle type.
    /// Returns the first opaque field-type name found, or `None` if clean.
    /// Uses `canonical_owned_handle_type_name` as the single opaque-detection
    /// authority (mirrors `ty_contains_owned_handle` in `registration.rs`).
    fn record_field_contains_opaque(
        &self,
        name: &str,
        visiting: &mut std::collections::HashSet<String>,
    ) -> Option<String> {
        if !visiting.insert(name.to_string()) {
            return None; // cycle protection
        }
        let type_def = self.type_defs.get(name)?;
        for field_ty in type_def.fields.values() {
            if let Some(opaque) = self.ty_field_contains_opaque(field_ty, visiting) {
                visiting.remove(name);
                return Some(opaque);
            }
        }
        visiting.remove(name);
        None
    }

    fn ty_field_contains_opaque(
        &self,
        ty: &Ty,
        visiting: &mut std::collections::HashSet<String>,
    ) -> Option<String> {
        match ty {
            Ty::Named { name, args, .. } => {
                // Direct opaque handle (imported via module registry OR user-declared #[opaque])?
                if self.canonical_owned_handle_type_name(name).is_some()
                    || self.user_opaque_type_names.contains(name.as_str())
                {
                    return Some(name.clone());
                }
                // Recurse into type args.
                for arg in args {
                    if let Some(n) = self.ty_field_contains_opaque(arg, visiting) {
                        return Some(n);
                    }
                }
                // Recurse into the type def's fields.
                if let Some(n) = self.record_field_contains_opaque(name, visiting) {
                    return Some(n);
                }
                None
            }
            Ty::Tuple(items) => {
                for item in items {
                    if let Some(n) = self.ty_field_contains_opaque(item, visiting) {
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
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    if let Some(param_ty) = sig.params.first() {
                        self.check_against(expr, sp, param_ty);
                    }
                }
                self.record_deferred_method_call_rewrite(span);
                self.record_method_call_receiver_kind(
                    span,
                    MethodCallReceiverKind::StreamInstance {
                        element_kind: Self::stream_receiver_element_kind(&resolved_inner)
                            .to_string(),
                    },
                );
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
                        "Duplex::send expects one argument (the message)".to_string(),
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
                        "Duplex::try_send expects one argument (the message)".to_string(),
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
                builtin: match ctor.as_str() {
                    "HashMap" => Some(BuiltinType::HashMap),
                    "HashSet" => Some(BuiltinType::HashSet),
                    _ => None,
                },
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
                         `{trait_name}::{method}` ({bound_summary})",
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
                         not locate `{trait_name}::{method}` for receiver \
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
        self.record_resolved_collection_call("Map", method, &receiver, span);
    }

    fn record_resolved_hashset_call(&mut self, method: &str, elem_ty: &Ty, span: &Span) {
        let receiver = TyPattern::App {
            ctor: "HashSet".to_string(),
            args: vec![self.ty_to_dispatch_pattern(elem_ty)],
        };
        self.record_resolved_collection_call("Set", method, &receiver, span);
    }

    /// Sole admission authority for Vec method dispatch (Stage 3 cutover);
    /// the per-element-type symbol override remains until Lever 3 (W4.030)
    /// collapses the scalar/`_layout` duality.
    ///
    /// Vec is still split between scalar-specific runtime kernels and
    /// layout-backed kernels. The registry therefore carries a placeholder
    /// `hew_vec_*_FAMILY` symbol so the MIR family gate recognises the call,
    /// while this override writes the concrete symbol selected by the existing
    /// Vec symbol router. The follow-up scalar/layout collapse removes this
    /// override; until then this is the authority-transfer seam from registry
    /// admission to per-element kernel selection.
    fn record_resolved_vec_call(&mut self, method: &str, elem_ty: &Ty, span: &Span) {
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

        let Some(symbol_name) = self.resolve_vec_runtime_symbol(method, &elem_ty, span) else {
            self.resolved_calls.remove(&key);
            return;
        };
        self.resolved_calls
            .get_mut(&key)
            .expect("collection resolver inserted Vec call before symbol override")
            .target
            .symbol_name = symbol_name.to_string();
    }

    /// Fail-closed gate for the Vec pipeline methods (`map`/`filter`/`reduce`)
    /// on function-valued elements. Each `Vec<fn(...)>` slot owns its
    /// closure-pair box and, transitively, the pair's environment box; the
    /// pipeline desugar reads elements by index (a borrow) and would hand a
    /// second owner the same environment the source vec still releases at
    /// scope exit (`boundary-fail-closed`, `ffi-ownership-contracts`).
    /// Returns `true` when the call was rejected.
    fn reject_vec_pipeline_fn_element(&mut self, method: &str, elem_ty: &Ty, span: &Span) -> bool {
        if matches!(elem_ty, Ty::Function { .. } | Ty::Closure { .. }) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`Vec::{method}` is not supported for function-valued elements: \
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
            ArgTemplate::I64 => Ty::I64,
            ArgTemplate::Receiver => cx.receiver.clone(),
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
            // `ArgTemplate::I64` index slots (Vec `get`/`set`/`remove`): accept
            // any signed integer narrower than i64 as a widening coercion.  The
            // operand is widened at the index site — the Vec element type and the
            // call's return type are NOT changed (LESSONS
            // `widen-operands-not-result-when-tightening-int-coercion`).
            // Codegen (via `load_signed_int_arg_to_declared_width`) sign-extends
            // the narrower MIR local to the i64 ABI width; no MIR cast is needed
            // for the `Terminator::Call` method path.
            if matches!(template, ArgTemplate::I64) {
                let actual = self.synthesize(expr, sp);
                let resolved = self.subst.resolve(&actual);
                if Self::is_narrower_signed_int(&resolved) {
                    // Accept: the narrower signed int widens to i64 at the call
                    // site. `synthesize` already recorded the actual type in
                    // `expr_types`; we do not override it here — the identifier's
                    // binding type (I32) is what HIR/MIR use for the local.
                    continue;
                }
                // Not a narrower signed int — fall through to the normal path.
                // `check_against` is called on the already-synthesised form; it
                // will re-synthesize internally for non-identifier expressions
                // (that is fine: the type is idempotent).
            }
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
            RetTemplate::OptionVal => Ty::option(cx.val.clone()),
            RetTemplate::Elem => cx.elem.clone(),
            RetTemplate::VecOfKey => self.make_vec_type(cx.key.clone(), span),
            RetTemplate::VecOfVal => self.make_vec_type(cx.val.clone(), span),
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
                // Vec `clone` returns the already-resolved receiver type
                // (`resolved.clone()`), not a reconstructed `Self`.
                CollectionKind::Vec => cx.resolved.clone(),
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
                    "keys" | "values" => self
                        .validate_hashmap_projection_element_types(&cx.key, &cx.val, method, span),
                    _ => self.validate_hashmap_key_value_types(&cx.key, &cx.val, span),
                };
                if !validated {
                    return false;
                }
                if matches!(
                    method,
                    "insert" | "get" | "remove" | "contains_key" | "len" | "keys" | "values"
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
                    "insert" | "contains" | "remove" | "len" | "is_empty"
                ) {
                    self.record_resolved_hashset_call(method, &cx.elem, span);
                }
            }
            CollectionKind::Vec => {
                // Element Rc-rejection on the mutating/element-consuming arms;
                // fire-and-continue (does NOT short-circuit, matching history).
                if matches!(
                    method,
                    "push" | "pop" | "get" | "remove" | "set" | "append" | "extend"
                ) {
                    self.reject_rc_collection_element("Vec", &cx.elem, span);
                }
                // Every table-driven Vec arm records via the symbol-override
                // recorder using the *resolved* element type.
                let resolved_elem = self.subst.resolve(&cx.elem);
                self.record_resolved_vec_call(method, &resolved_elem, span);
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
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let receiver = Ty::Named {
            builtin: Some(kind.builtin()),
            name: kind.name().to_string(),
            args: vec![],
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
            return self.collection_method_fallback(kind, method, args, span);
        };
        if let Some(arity) = desc.arity {
            self.check_arity(args, arity, &format!("`{}::{method}`", kind.name()), span);
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
        match method {
            // rc.clone() increments the reference count and returns a new Rc<T>
            "clone" => {
                self.check_arity(args, 0, "`Rc::clone`", span);
                Ty::rc(inner_ty)
            }
            // rc.get() copies the inner value out of the Rc.
            // `LoadOp` performs a bitwise copy, which is only sound for `Copy`
            // types (no ownership to duplicate).  For non-Copy `T`, callers
            // share access via `rc.clone()` instead.
            "get" => {
                self.check_arity(args, 0, "`Rc::get`", span);
                if !self
                    .registry
                    .implements_marker(&inner_ty, MarkerTrait::Copy)
                {
                    self.report_error(
                        TypeErrorKind::BoundsNotSatisfied,
                        span,
                        format!(
                            "`Rc::get` requires `T: Copy`; `{}` is not `Copy` — \
                             use `rc.clone()` to share the reference instead",
                            inner_ty.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                inner_ty
            }
            // rc.strong_count() returns the current reference count as i64
            "strong_count" => {
                self.check_arity(args, 0, "`Rc::strong_count`", span);
                Ty::I64
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

    fn report_vec_contains_layout_equality_gate(
        &mut self,
        elem_ty: &Ty,
        eligibility: crate::eq_eligibility::EqEligibility,
        span: &Span,
    ) {
        use crate::eq_eligibility::EqEligibility;

        let reason = match eligibility {
            EqEligibility::Eligible => format!(
                "`Vec::contains` on layout-backed element type `{}` is equality-eligible, \
                 but layout contains is not yet supported for this element type",
                elem_ty.user_facing()
            ),
            EqEligibility::IneligibleFloat(float_ty) => format!(
                "`Vec::contains` on layout-backed element type `{}` requires aggregate \
                 equality, but `{}` is or contains floating-point data",
                elem_ty.user_facing(),
                float_ty.user_facing()
            ),
            EqEligibility::IneligibleManaged(managed_ty) => format!(
                "`Vec::contains` on layout-backed element type `{}` requires aggregate \
                 equality, but `{}` is layout-managed/non-Copy data",
                elem_ty.user_facing(),
                managed_ty.user_facing()
            ),
            EqEligibility::IneligibleOwned(owned_ty) => format!(
                "`Vec::contains` on layout-backed element type `{}` requires aggregate \
                 equality, but `{}` is owned or heap-backed data",
                elem_ty.user_facing(),
                owned_ty.user_facing()
            ),
            EqEligibility::IneligibleUnknown => format!(
                "`Vec::contains` on layout-backed element type `{}` requires aggregate \
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

    /// Resolve a `Vec<T>` method to its runtime C-ABI symbol and emit a
    /// precise fail-closed diagnostic when the element type would require
    /// layout-descriptor support that is outside the current runtime/codegen
    /// surface.
    ///
    /// Returns `None` in two distinct fail-closed conditions:
    ///   1. The element type's runtime suffix cannot be determined
    ///      (e.g. inference variable, unresolved nominal) — caller
    ///      leaves `method_call_rewrites` absent, downstream layers
    ///      fail closed without a user-facing error here.
    ///   2. The element type is record/tuple (`_layout` suffix) but the
    ///      method is not one of the `BitCopy` Plain operations currently
    ///      backed by runtime + codegen (`push`, `get`, `set`, `pop`) or
    ///      the element is not `Copy` — a precise
    ///      `TypeErrorKind::InvalidOperation` is reported at `span` naming
    ///      the would-be runtime symbol, and the rewrite is NOT recorded.
    fn resolve_vec_runtime_symbol(
        &mut self,
        method: &str,
        elem_ty: &Ty,
        span: &Span,
    ) -> Option<&'static str> {
        if method == "join" {
            return matches!(elem_ty, Ty::String).then_some("hew_vec_join_str");
        }
        // Closure-pair elements: each slot owns a heap-boxed pair (and,
        // transitively, the pair's env box). The shared-buffer methods would
        // shallow-copy those boxes and double-free them at the two vecs'
        // releases — fail closed with a precise diagnostic rather than alias
        // owners (`boundary-fail-closed`, `ffi-ownership-contracts`).
        if matches!(elem_ty, Ty::Function { .. } | Ty::Closure { .. })
            && matches!(method, "clone" | "append" | "extend")
        {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`Vec::{method}` is not supported for function-valued elements: \
                     each element owns its closure environment, and a shallow \
                     buffer copy would create two owners of one environment"
                ),
            );
            return None;
        }
        let sym = crate::stdlib::resolve_vec_method(method, elem_ty, &self.type_defs)?;
        if sym.ends_with("_layout") {
            let supported_bitcopy_method =
                matches!(method, "push" | "get" | "set" | "pop" | "remove" | "clone");
            let is_copy = self.vec_element_has_copy_layout(elem_ty);
            if supported_bitcopy_method && is_copy {
                return Some(sym);
            }

            // W5.016 owned-element path: a non-Copy record / enum element whose
            // ownership has a synthesizable clone/drop thunk path (and which is
            // RcFree, so the runtime can drop it deterministically) routes
            // through the `hew_vec_*_owned` ABI instead of failing closed. The
            // owned routing only covers the methods the owned runtime ops
            // implement (`hew_vec_{push,get,set,pop,clone}_owned`); `remove`
            // on owned elements remains fail-closed until its owned op lands.
            if matches!(method, "push" | "get" | "set" | "pop" | "clone")
                && self.vec_owned_element_admissible(elem_ty)
            {
                if let Some(owned) = owned_vec_runtime_symbol(method) {
                    return Some(owned);
                }
            }

            // Stage 3a fail-closed boundary: layout-backed Vec operations are
            // only lifted when type routing, runtime support, and codegen
            // pseudo-FFI operand synthesis all exist.  Keep LayoutManaged
            // records/tuples and unsupported layout methods out of the rewrite
            // side table so downstream layers never fabricate a value.
            let message = if supported_bitcopy_method {
                // Non-Copy element on a supported op (push/get/set/pop/...): it
                // reached here because it is neither Copy nor owned-admissible.
                // Name the real blocker (recursive container / Rc / no thunk
                // path) instead of a blanket Copy diagnostic.
                let why = self.vec_element_rejection_reason(elem_ty);
                format!(
                    "`{}` cannot be a `Vec` element for `Vec::{method}`: {why} \
                     (runtime symbol `{sym}`)",
                    elem_ty.user_facing()
                )
            } else {
                format!(
                    "`Vec::{method}` on layout-backed element type `{}` is not \
                     runtime-backed yet (runtime symbol `{sym}`); supported layout Vec \
                     methods are push/get/set/pop/remove/clone for Copy record/tuple elements",
                    elem_ty.user_facing()
                )
            };
            self.report_error(TypeErrorKind::InvalidOperation, span, message);
            return None;
        }
        Some(sym)
    }

    /// Decide whether a non-Copy `Vec<T>` element type may route through the
    /// W5.016 owned-element ABI (`hew_vec_*_owned`).
    ///
    /// Admissible when the element is a user record or enum (the shapes whose
    /// per-type `__hew_record/enum_{clone,drop}_inplace` thunks codegen
    /// synthesizes) AND it is `RcFree` (so the runtime drop pass terminates
    /// deterministically — `Rc` ownership is not tracked per element). Tuple
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
            if !matches!(self.registry.rc_free_status(elem_ty), RcFreeStatus::RcFree) {
                return "it transitively holds an `Rc`, whose per-element drop is not \
                        deterministically tracked by the owned-element Vec runtime"
                    .to_string();
            }
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
                    if is_record_or_enum {
                        if !matches!(self.registry.rc_free_status(elem_ty), RcFreeStatus::RcFree) {
                            return "it transitively holds an `Rc`, whose per-element \
                                    drop is not deterministically tracked by the \
                                    owned-element Vec runtime"
                                .to_string();
                        }
                        if self.vec_element_contains_unowned_container(
                            elem_ty,
                            &HashSet::new(),
                            &mut HashSet::new(),
                        ) {
                            return "it contains a `Vec`/`HashMap`/`HashSet` field \
                                    (including a self-recursive one), which requires \
                                    recursive owned clone/drop thunk synthesis that is \
                                    not implemented yet"
                                .to_string();
                        }
                    }
                }
            }
        }
        "it has no clone/drop thunk path for the owned-element Vec runtime".to_string()
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
    ///   so the owned-element queue witness can describe them (same `RcFree` +
    ///   no-unowned-container requirements as for enum channel elements).
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
                        // Monomorphic: apply the same RcFree + no-unowned-container
                        // requirements as for enum channel elements.
                        return matches!(
                            self.registry.rc_free_status(elem_ty),
                            RcFreeStatus::RcFree
                        ) && !self.vec_element_contains_unowned_container(
                            elem_ty,
                            &HashSet::new(),
                            &mut HashSet::new(),
                        );
                    }
                }
                crate::check::admissibility::primitive_copy_layout(elem_ty, &self.type_defs)
                    .is_some()
                    || self.vec_owned_element_admissible(elem_ty)
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
                    || self.vec_owned_element_admissible(elem_ty)
            }
        }
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
        match elem_ty {
            // Tuple element: a tuple with at least one owned (non-Copy) field
            // routes through the synthesized `__hew_tuple_*_inplace` thunk. An
            // all-Copy tuple is `Copy` and never reaches this admissibility
            // check (it takes the BitCopy `_layout` path). Nested tuples recurse
            // through the same authority; container and closure leaves still
            // fail closed.
            Ty::Tuple(elems) => {
                // RcFree + no unowned-container field. A tuple has no
                // self-recursion root, so the `roots` set is empty: any
                // container field is unowned.
                matches!(self.registry.rc_free_status(elem_ty), RcFreeStatus::RcFree)
                    && elems
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
                // drop_fn. Admit on the same RcFree gate the record arm uses
                // (deterministic per-element drop). A closure-pair `Vec<fn>` /
                // `Vec<closure>` element keeps its existing pointer/closure-
                // pairs ABI (separate lane, #1722 out-of-scope) — never copy-in.
                // The owned-vs-managed clone selection is congruent by
                // construction: codegen's `collection_elem_clone_drop_syms` and
                // the inner Vec's own constructor both consult
                // `resolved_ty_element_owns_heap_for_owned_vec`, so the clone
                // primitive can never disagree with the inner Vec's ABI.
                match builtin {
                    Some(BuiltinType::HashMap | BuiltinType::HashSet) => {
                        return matches!(
                            self.registry.rc_free_status(elem_ty),
                            RcFreeStatus::RcFree
                        );
                    }
                    Some(BuiltinType::Vec) => {
                        if args
                            .first()
                            .is_some_and(|e| matches!(e, Ty::Function { .. } | Ty::Closure { .. }))
                        {
                            return false;
                        }
                        return matches!(
                            self.registry.rc_free_status(elem_ty),
                            RcFreeStatus::RcFree
                        );
                    }
                    // Other builtin nominals (Rc/Option/Result/handles) are not
                    // user records/enums and have no owned-Vec thunk path.
                    Some(_) => return false,
                    // User-defined record/enum: fall through to the logic below.
                    None => {}
                }
                let Some(type_def) = self.type_defs.get(name) else {
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
                // RcFree is required so the per-element runtime drop is
                // deterministic.
                if !matches!(self.registry.rc_free_status(elem_ty), RcFreeStatus::RcFree) {
                    return false;
                }
                // A record/enum that transitively contains a `Vec`/`HashMap`/
                // `HashSet` field has no in-place thunk path today — keep it
                // fail-closed (general container-in-container is a separate
                // lane). This INCLUDES the self-recursive enum
                // (`enum R { A(Vec<R>); ... }`): admitting it requires the enum
                // clone/drop thunk synthesis to recurse through the field's
                // OWNED-Vec ops (`hew_vec_{clone,free}_owned` with the owned
                // descriptor) rather than the witness-managed
                // `hew_vec_free_managed` (which aborts on owned elements). That
                // recursive-enum-clone synthesis is a follow-on; until it
                // lands, `Vec<RedisReply>` stays fail-closed here (the empty
                // `roots` set rejects every container field, self-recursive or
                // not), so the checker never admits a type the codegen drop
                // path cannot handle (the Slice-1-era over-admit revert trap).
                !self.vec_element_contains_unowned_container(
                    elem_ty,
                    &HashSet::new(),
                    &mut HashSet::new(),
                )
            }
            _ => false,
        }
    }

    fn vec_tuple_owned_field_admissible(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Tuple(elems) => {
                matches!(self.registry.rc_free_status(ty), RcFreeStatus::RcFree)
                    && elems
                        .iter()
                        .all(|elem| self.vec_tuple_owned_field_admissible(elem))
            }
            Ty::String | Ty::Bytes => true,
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
    /// admitted exception (gated by the later `RcFree` refinement) — here we
    /// only reject UNOWNED-container fields, not the self-recursive edge.
    fn vec_element_contains_unowned_container(
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

    #[expect(
        clippy::too_many_lines,
        reason = "Vec keeps the divergent contains/join/map/filter/fold arms and the structural-array guard inline; the table arms delegate to the shared driver."
    )]
    pub(super) fn check_vec_method(
        &mut self,
        type_args: &[Ty],
        receiver_ty: &Ty,
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
        let result = match method {
            "into_iter" => {
                self.check_arity(args, 0, "`Vec::into_iter`", span);
                let resolved_elem = self.subst.resolve(&elem_ty);
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
                Ty::Named {
                    name: "VecIter".to_string(),
                    args: vec![resolved_elem],
                    builtin: None,
                }
            }
            "contains" => {
                if let Some(arg) = args.first() {
                    let (expr, sp) = arg.expr();
                    self.check_against(expr, sp, &elem_ty);
                }
                let resolved_elem = self.subst.resolve(&elem_ty);
                if crate::stdlib::vec_element_runtime_suffix(&resolved_elem, &self.type_defs)
                    == Some("layout")
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
                    if matches!(eligibility, crate::eq_eligibility::EqEligibility::Eligible)
                        && is_copy
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
                                "`Vec::contains` on layout-backed element type `{}` requires \
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
            "join" => {
                self.check_arity(args, 1, "`Vec::join`", span);
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
                            "`Vec::join` is only available on Vec<String>, not Vec<{}>",
                            elem_ty.user_facing()
                        ),
                    );
                }
                Ty::String
            }
            "map" => {
                self.check_arity(args, 1, "`Vec::map`", span);
                self.reject_rc_collection_element("Vec", &elem_ty, span);
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
                self.reject_rc_collection_element("Vec", &resolved_ret, span);
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
                self.check_arity(args, 1, "`Vec::filter`", span);
                self.reject_rc_collection_element("Vec", &elem_ty, span);
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
                self.check_arity(args, 2, "`Vec::reduce`", span);
                self.reject_rc_collection_element("Vec", &elem_ty, span);
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
                self.check_arity(args, 2, "`Vec::fold`", span);
                self.reject_rc_collection_element("Vec", &elem_ty, span);
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
                self.subst.resolve(&acc_ty)
            }
            _ if collection_method_desc(CollectionKind::Vec, method).is_some() => {
                // Table-driven Vec arms (push/pop/len/get/remove/is_empty/
                // clear/clone/set/append/extend) go through the shared
                // descriptor-driven driver.  The structural-array pre/post
                // guard and `validate_vec_element_type` top call stay here
                // because they wrap the whole dispatch, and contains/join/map/
                // filter/fold stay above as explicit code arms.  For these
                // known methods the driver never hits its own fallback and
                // always returns a value (Vec arg/admission hooks never
                // short-circuit), so this falls through to the post-guard
                // exactly as the legacy inline table arms did.
                let cx =
                    CollectionTyCx::vec(elem_ty.clone(), receiver_ty.clone(), resolved.clone());
                self.check_collection_method(CollectionKind::Vec, &cx, method, args, span)
            }
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
                    args: vec![],
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
        let applied_sig = self.apply_instantiated_call_signature(
            &sig,
            None,
            args,
            span,
            SignatureArgApplication::PositionalOnly {
                arity_context: format!("method '{method}'"),
            },
            true,
        );
        let method_key = format!("{canonical}::{method}");
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::PrimitiveTraitImpl {
                trait_name,
                canonical_receiver: canonical,
            },
        );
        if self.fn_sigs.contains_key(&method_key) {
            self.record_method_call_rewrite(
                span,
                MethodCallRewrite::RewriteToFunction {
                    c_symbol: method_key,
                    // User-fn dispatch into a primitive trait impl
                    // (`i64::fmt` etc.) is open-set; the typed runtime-call
                    // catalog does not enumerate user-defined method keys.
                    descriptor: None,
                    elem_ty: None,
                    // Primitive trait-impl dispatch is a user-fn call; it never
                    // consumes the receiver as a handle release.
                    consumes_receiver: false,
                },
            );
        }
        Some(applied_sig.return_type)
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
            .any(|(_, tn)| tn == trait_name);
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
            .get(&(canonical.clone(), trait_name.to_string()))
            .and_then(|methods| methods.get(method_name))
            .cloned()?;
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
                arity_context: format!("method '{trait_name}::{method_name}'"),
            },
            true,
        );
        self.record_method_call_receiver_kind(
            span,
            MethodCallReceiverKind::PrimitiveTraitImpl {
                trait_name: trait_name.to_string(),
                canonical_receiver: canonical,
            },
        );
        Some(applied.return_type)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "pattern matching type checker with many variants"
    )]
    pub(super) fn check_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        // Module-qualified calls: e.g. http.listen(addr) → lookup "http.listen" in fn_sigs
        if let Expr::Identifier(name) = &receiver.0 {
            let receiver_is_binding = self.env.lookup_ref(name).is_some();
            let receiver_is_known_type = self.type_defs.contains_key(name);
            let key = format!("{name}.{method}");
            let looks_like_module_call = !receiver_is_binding
                && !receiver_is_known_type
                && (self.modules.contains(name)
                    || self.module_fn_exports.contains(&key)
                    || self.fn_sigs.contains_key(&key));
            if looks_like_module_call {
                if self.modules.contains(name) {
                    self.used_modules
                        .borrow_mut()
                        .insert(ImportKey::new(self.current_module.clone(), name.clone()));
                }
                // Cross-module enum variant construction: e.g. `fs.IoError::TimedOut(0)`.
                // method contains "::" → treat as a qualified variant constructor rather than a
                // module function. Mirrors the lookup in check_call (calls.rs:407-465).
                if method.contains("::") {
                    let constructor_match = self.lookup_variant_constructor(method);
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
                        return Ty::normalize_named(type_name, resolved_args);
                    }
                }
                if !self.module_fn_exports.contains(&key) {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no function `{method}` in module `{name}`"),
                    );
                    return Ty::Error;
                }
                self.require_unsafe(&key, span);
                self.reject_if_wasm_native_only_network_module_call(name, span);
                // Native-only stdlib modules are rejected on wasm32 because
                // their runtime implementations are not compiled there.
                if !self.user_modules.contains(name) {
                    match name.as_str() {
                        "stream" => self.reject_wasm_feature(span, WasmUnsupportedFeature::Streams),
                        "http" => {
                            self.reject_wasm_feature(span, WasmUnsupportedFeature::HttpServer);
                        }
                        "net" => {
                            self.reject_wasm_feature(span, WasmUnsupportedFeature::TcpNetworking);
                        }
                        "process" => {
                            self.reject_wasm_feature(
                                span,
                                WasmUnsupportedFeature::ProcessExecution,
                            );
                        }
                        "tls" => self.reject_wasm_feature(span, WasmUnsupportedFeature::Tls),
                        "quic" => self.reject_wasm_feature(span, WasmUnsupportedFeature::Quic),
                        "dns" => self.reject_wasm_feature(span, WasmUnsupportedFeature::Dns),
                        "os" => self.reject_wasm_feature(span, WasmUnsupportedFeature::OsEnv),
                        // std::crypto::encrypt and std::crypto::sign are backed by
                        // native-only staticlib companion crates absent from the wasm32
                        // link set.  Reject at check time so the caller gets a structured
                        // diagnostic rather than a `wasm-ld` undefined-symbol failure.
                        // WASM-TODO(#1451): design WASI-capable crypto surfaces.
                        "encrypt" => {
                            self.reject_wasm_feature(span, WasmUnsupportedFeature::CryptoEncrypt);
                        }
                        "sign" => {
                            self.reject_wasm_feature(span, WasmUnsupportedFeature::CryptoSign);
                        }
                        _ => {}
                    }
                    // Warn-level module-qualified calls with degraded wasm32
                    // semantics (non-cryptographic PRNG fallback).
                    if name == "crypto" && method == "random_bytes" {
                        self.warn_wasm_limitation(span, WasmUnsupportedFeature::CryptoRandom);
                    }
                }
                if let Some(sig) = self.fn_sigs.get(&key).cloned() {
                    if let Some(caller) = &self.current_function {
                        self.call_graph
                            .entry(caller.clone())
                            .or_default()
                            .insert(key.clone());
                    }
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
                    );
                    // Channel constructor: inject a shared type variable so
                    // Sender<T> and Receiver<T> from the same `new` call are
                    // linked through unification.
                    if key == "channel.new" {
                        let t = Ty::Var(TypeVar::fresh());
                        return Ty::Tuple(vec![Ty::sender(t.clone()), Ty::receiver(t)]);
                    }
                    if let Some(op) = Self::generic_math_intrinsic_op(name, method) {
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
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no function `{method}` in module `{name}`"),
                );
                return Ty::Error;
            }

            // Static method calls on type names: e.g. Point.from_json(json)
            // Look up "TypeName.method" in fn_sigs (registered by wire types etc.)
            let static_key = format!("{name}.{method}");
            if let Some(sig) = self.fn_sigs.get(&static_key).cloned() {
                self.check_arity(args, sig.params.len(), &format!("`{static_key}`"), span);
                for (i, arg) in args.iter().enumerate() {
                    if let Some(param_ty) = sig.params.get(i) {
                        let (expr, sp) = arg.expr();
                        self.check_against(expr, sp, param_ty);
                    }
                }
                // Binary wire codec: `Type.decode(bytes) -> Type` on a `#[wire]`
                // struct. The static method lives in `fn_sigs` under the dotted
                // `Type.decode` key but records no rewrite here, so the call
                // would lower to `MethodCallNoRewrite`. Record a dedicated codec
                // rewrite so HIR/codegen call the `__hew_deserialize_<key>` thunk
                // with the correct ABI. The text-format `from_json`/`from_yaml`
                // static methods are deliberately excluded — no text-format thunk
                // exists, so they stay fail-closed.
                if method == "decode" && self.wire_struct_types.contains(name) {
                    if let Ok(value_ty) = ResolvedTy::from_ty(&self.subst.resolve(&sig.return_type))
                    {
                        self.record_method_call_rewrite(
                            span,
                            MethodCallRewrite::WireCodec {
                                direction: WireCodecDirection::Decode,
                                value_ty,
                            },
                        );
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
        // at the end of the inference pass, moved forward so that methods such
        // as `.to_f64()` resolve correctly inside the loop body.
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
        self.reject_if_wasm_native_only_handle(&resolved, span);
        self.reject_if_wasm_blocking_semaphore_method(&resolved, method, span);
        if let Ty::Named { name, .. } = &resolved {
            // NEW-1: `await conn.read()` / `await conn.read_string()` is the
            // non-blocking suspending read. Record the inner method-call span so
            // HIR lowering emits `ConnAwaitRead` instead of the blocking method.
            // Recognised ONLY directly under an `await` (the suspend point); a
            // bare `conn.read()` stays the blocking FFI call (E8).
            let is_conn_await_read = self.inside_await_expr
                && (name == "Connection" || name == "net.Connection")
                && matches!(method, "read" | "read_string");
            if is_conn_await_read {
                self.conn_await_reads.insert(
                    SpanKey::in_module(span, self.current_module_idx),
                    method == "read_string",
                );
            } else {
                // NEW-2: `await listener.accept()` is the non-blocking suspending
                // accept (the listener-readiness sibling of `await conn.read()`).
                // Record the inner method-call span so HIR lowering emits
                // `ListenerAwaitAccept` instead of the blocking `hew_tcp_accept`.
                // Recognised ONLY directly under an `await`; a bare
                // `listener.accept()` stays the blocking FFI call.
                let is_listener_await_accept = self.inside_await_expr
                    && (name == "Listener" || name == "net.Listener")
                    && method == "accept";
                if is_listener_await_accept {
                    self.listener_await_accepts
                        .insert(SpanKey::in_module(span, self.current_module_idx));
                } else {
                    // The blocking-call warning is correct for a bare (non-awaited)
                    // `conn.read()`; suppress it when the read is the non-blocking
                    // suspending form (it no longer strands a worker).
                    self.warn_if_blocking_handle_method(name, method, span);
                }
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
            // Infallible width-widening: `.to_<W>()` — same signedness, strictly wider.
            //
            // Rule (B-1c): For integer sources, only same-sign strictly-wider fixed-width
            // targets are admitted. Same-width, narrowing, cross-sign, and platform-sized
            // (isize/usize) sources must use `.try_to_<W>()` instead.
            //
            // Float sources retain the legacy permissive behaviour: any numeric source
            // may call `.to_f32()`/`.to_f64()`.  Float-to-integer is also unchanged.
            //
            // Admitted pairs:
            //   signed:   i8→{i16,i32,i64}, i16→{i32,i64}, i32→{i64}
            //   unsigned: u8→{u16,u32,u64}, u16→{u32,u64}, u32→{u64}
            //   float:    any numeric → f32/f64  (legacy, unchanged)
            //   to_f*:    any numeric source     (legacy, unchanged)
            (resolved, method) if resolved.is_numeric() && method.starts_with("to_") => {
                // Resolve target type from method name.
                let target_opt: Option<Ty> = match method {
                    "to_i8" => Some(Ty::I8),
                    "to_i16" => Some(Ty::I16),
                    "to_i32" => Some(Ty::I32),
                    "to_i64" => Some(Ty::I64),
                    "to_isize" => Some(Ty::Isize),
                    "to_u8" => Some(Ty::U8),
                    "to_u16" => Some(Ty::U16),
                    "to_u32" => Some(Ty::U32),
                    "to_u64" => Some(Ty::U64),
                    "to_usize" => Some(Ty::Usize),
                    "to_f32" => Some(Ty::F32),
                    "to_f64" => Some(Ty::F64),
                    _ => None,
                };
                let Some(target) = target_opt else {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        self.synthesize(expr, sp);
                    }
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "no conversion method `{method}` on `{}`",
                            resolved.user_facing()
                        ),
                    );
                    return Ty::Error;
                };
                // Float targets: always admitted regardless of source width.
                if matches!(target, Ty::F32 | Ty::F64) {
                    return target;
                }
                // Float source converting to integer: always admitted (legacy).
                if resolved.is_float() {
                    return target;
                }
                // Integer-to-integer: enforce strict widening rules.
                // Reject platform-sized source (isize/usize have no fixed width).
                if matches!(resolved, Ty::Isize | Ty::Usize) {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "`{}` is platform-sized; use `.try_to_{}()` for width conversion",
                            resolved.user_facing(),
                            target.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                // Reject platform-sized target (to_isize/to_usize on integer source).
                if matches!(target, Ty::Isize | Ty::Usize) {
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "`.{}()` targets a platform-sized type; use `.try_to_{}()` \
                             for width conversion",
                            method,
                            target.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                // Both are fixed-width integers. Check sign and width.
                let src_unsigned = resolved.is_unsigned();
                let tgt_unsigned = target.is_unsigned();
                if src_unsigned != tgt_unsigned {
                    // Cross-sign: must use try_to_*.
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!(
                            "`.{}()` crosses integer signedness; use `.try_to_{}()` for \
                             cross-sign conversion",
                            method,
                            target.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                let src_bits = resolved.integer_bit_width().unwrap(); // non-None: fixed-width checked above
                let tgt_bits = target.integer_bit_width().unwrap();
                if src_bits >= tgt_bits {
                    // Same width or narrowing: must use try_to_*.
                    let hint = if src_bits == tgt_bits {
                        format!(
                            "source and target are the same width ({src_bits}); no conversion needed"
                        )
                    } else {
                        let tgt_name = target.user_facing();
                        format!("use `.try_to_{tgt_name}()` for narrowing conversions")
                    };
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("`.{method}()` is not an infallible widening; {hint}"),
                    );
                    return Ty::Error;
                }
                // Admitted: same sign, strictly wider fixed-width target.
                target
            }
            // Fallible narrowing / cross-sign: `.try_to_<W>() -> Result<W, NarrowError>`.
            //
            // Admitted for all integer-to-integer pairs that are NOT covered by the
            // infallible `.to_<W>()` family: narrowing, cross-sign, isize/usize source
            // or target.  Also admitted for same-width (caller may not know widths).
            (resolved, method) if resolved.is_integer() && method.starts_with("try_to_") => {
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
                    _ => None,
                };
                if let Some(target) = target_opt {
                    Ty::result(target, Ty::narrow_error())
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
            // LocalPid<T> methods — first check LocalPid's own impl methods,
            // then fall through to actor receive-fn dispatch on the inner type T.
            //
            // Own methods (e.g. `tell`, `to_remote_via`) are declared in
            // `impl LocalPid<T>` in std/builtins.hew and registered in type_defs /
            // fn_sigs as `"LocalPid::{method}"`.  Actor receive-fn dispatch
            // (e.g. `pid.greet(arg)`) remains the local actor-dispatch path.
            (resolved, _) if resolved.as_local_pid().is_some() => {
                // `.send(payload)` is the legacy fire-and-forget surface; enforce
                // the Send bound on the payload.
                if method == "send" {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        let ty = self.synthesize(expr, sp);
                        self.enforce_actor_boundary_send(expr, sp, sp, &ty);
                    }
                    return Ty::Unit;
                }
                // Try LocalPid's own methods first.
                if let Ty::Named {
                    args: receiver_args,
                    ..
                } = resolved
                {
                    if let Some(sig) = self.lookup_named_method_sig(
                        crate::BuiltinType::LocalPid.canonical_name(),
                        receiver_args,
                        method,
                    ) {
                        let applied_sig = self.apply_instantiated_call_signature(
                            &sig,
                            None,
                            args,
                            span,
                            SignatureArgApplication::PositionalOnly {
                                arity_context: format!("method '{method}'"),
                            },
                            true,
                        );
                        if method == "tell" {
                            self.enforce_actor_method_send_args(args);
                        }
                        return applied_sig.return_type;
                    }
                }
                // Fall through to actor receive-fn dispatch on the inner type.
                let inner = resolved.as_local_pid().unwrap();
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
                    if let Some(sig) = self.fn_sigs.get(&method_key).cloned() {
                        for (i, arg) in args.iter().enumerate() {
                            let (expr, sp) = arg.expr();
                            let ty = if let Some(param_ty) = sig.params.get(i) {
                                self.check_against(expr, sp, param_ty)
                            } else {
                                self.synthesize(expr, sp)
                            };
                            self.enforce_actor_boundary_send(expr, sp, sp, &ty);
                        }
                        self.record_method_call_receiver_kind(
                            span,
                            MethodCallReceiverKind::ActorInstance {
                                actor_name: actor_identity.clone(),
                            },
                        );
                        // Ask-without-await guard: ask-shaped receive fn must be
                        // awaited. Generator methods (`receive gen fn`) use `for
                        // await` at the call site and are exempt from this guard.
                        let resolved_ret = self.subst.resolve(&sig.return_type);
                        if !matches!(resolved_ret, Ty::Unit)
                            && !self.receive_generator_methods.contains(&method_key)
                            && !self.inside_await_expr
                        {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!(
                                    "actor ask `{actor_identity}::{method}` requires `await`; \
                                     write `let v? = await ref.{method}(...)` \
                                     or `match await ref.{method}(...) {{ Ok(v) => ..., Err(e) => ... }}`",
                                ),
                            );
                        }
                        self.record_actor_method_dispatch(
                            span,
                            method_key,
                            sig.return_type.clone(),
                        );
                        return sig.return_type;
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
                                arity_context: format!("method '{method}'"),
                            },
                            true,
                        );
                        let return_type = if method == "ask" {
                            self.project_assoc_types(&applied_sig.return_type)
                        } else {
                            applied_sig.return_type.clone()
                        };
                        if matches!(method, "tell" | "ask") {
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
                        if method == "tell" {
                            // S5: real RemotePid<T>::tell lowering. Record a
                            // direct-call rewrite so HIR/MIR lower the call
                            // to `hew_remote_pid_tell`, which codegen
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
                                    c_symbol: "hew_remote_pid_tell".to_string(),
                                    // Closed runtime call dispatched by callee-
                                    // name intercept in codegen; the substrate
                                    // enumerates this family.
                                    descriptor: Some(
                                        crate::runtime_call::RuntimeCallDescriptor::new(
                                            crate::runtime_call::RuntimeCallFamily::RemotePidTell,
                                            None,
                                        )
                                        .expect("RemotePidTell rejects elem"),
                                    ),
                                    elem_ty: None,
                                    // Fire-and-forget send; borrows the pid
                                    // handle, does not release it.
                                    consumes_receiver: false,
                                },
                            );
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
            // ActorRef methods
            (resolved, _) if resolved.as_actor_ref().is_some() => {
                let inner = resolved.as_actor_ref().unwrap();
                if method == "send" {
                    for arg in args {
                        let (expr, sp) = arg.expr();
                        let ty = self.synthesize(expr, sp);
                        self.enforce_actor_boundary_send(expr, sp, sp, &ty);
                    }
                    Ty::Unit
                } else {
                    // Try to dispatch to the actor's receive methods
                    if let Ty::Named {
                        name: actor_name, ..
                    } = inner
                    {
                        let method_key = format!("{actor_name}::{method}");
                        if let Some(sig) = self.fn_sigs.get(&method_key).cloned() {
                            for (i, arg) in args.iter().enumerate() {
                                let (expr, sp) = arg.expr();
                                let ty = if let Some(param_ty) = sig.params.get(i) {
                                    self.check_against(expr, sp, param_ty)
                                } else {
                                    self.synthesize(expr, sp)
                                };
                                self.enforce_actor_boundary_send(expr, sp, sp, &ty);
                            }
                            self.record_method_call_receiver_kind(
                                span,
                                MethodCallReceiverKind::ActorInstance {
                                    actor_name: actor_name.clone(),
                                },
                            );
                            // Ask-without-await guard: ask-shaped receive fn must be
                            // awaited. Generator methods (`receive gen fn`) use `for
                            // await` at the call site and are exempt from this guard.
                            let resolved_ret = self.subst.resolve(&sig.return_type);
                            if !matches!(resolved_ret, Ty::Unit)
                                && !self.receive_generator_methods.contains(&method_key)
                                && !self.inside_await_expr
                            {
                                self.report_error(
                                    TypeErrorKind::InvalidOperation,
                                    span,
                                    format!(
                                        "actor ask `{actor_name}::{method}` requires `await`; \
                                         write `let v? = await ref.{method}(...)` \
                                         or `match await ref.{method}(...) {{ Ok(v) => ..., Err(e) => ... }}`",
                                    ),
                                );
                            }
                            self.record_actor_method_dispatch(
                                span,
                                method_key,
                                sig.return_type.clone(),
                            );
                            return sig.return_type;
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
            }
            // Duplex<S, R>: bidirectional channel handle.
            //
            // Methods: .send(msg) / .recv() / .send_half() / .recv_half() / .close()
            //
            // Lambda-actor handles are also typed as `Duplex<S, R>` underneath, so
            // this arm handles both raw-duplex and lambda-actor method calls.
            // Call-syntax `handle(msg)` is canonical for lambda-actor handles;
            // `.send(msg)` is an allowed-secondary surface — both route to the same
            // runtime symbol (`hew_duplex_send`).
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Duplex),
                    args: type_args,
                    ..
                },
                _,
            ) => self.check_duplex_method(type_args, &receiver_ty, receiver, method, args, span),
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
            // Named types that have built-in methods (Actor<T> from lambda actors)
            (
                Ty::Named {
                    builtin: Some(BuiltinType::Actor),
                    args: type_args,
                    ..
                },
                "send",
            ) => {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    let ty = if let Some(param_ty) = type_args.first() {
                        self.check_against(expr, sp, param_ty)
                    } else {
                        self.synthesize(expr, sp)
                    };
                    self.enforce_actor_boundary_send(expr, sp, sp, &ty);
                }
                Ty::Unit
            }
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
                            self.warn_if_blocking_in_receive_fn("Receiver::recv", span);
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
                    self.check_arity(args, 0, "`Range::rev`", span);
                } else {
                    self.check_arity(args, 1, "`Range::step_by`", span);
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
                    _ => self.lookup_named_method_sig(name, type_args, method),
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
                        }
                    }
                    let applied_sig = self.apply_instantiated_call_signature(
                        &sig,
                        None,
                        args,
                        span,
                        SignatureArgApplication::PositionalOnly {
                            arity_context: format!("method '{method}'"),
                        },
                        true,
                    );
                    // Actor receive-method dispatch on a bare actor-typed
                    // receiver — e.g. an actor field holding a reference
                    // (`let out: W; out.put(arg)`) or a `let target: Printer`
                    // binding. `lookup_named_method_sig` finds the signature in
                    // `fn_sigs` keyed `{Actor}::{method}`, but a value of bare
                    // actor type `W` is still an actor handle, not a struct: the
                    // call must cross the mailbox boundary exactly like the
                    // `LocalPid<W>` / `ActorRef<W>` arms above. Route it through
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
                        // `await`. Mirrors the `LocalPid` / `ActorRef` arms.
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
                                    "actor ask `{name}::{method}` requires `await`; \
                                     write `let v? = await ref.{method}(...)` \
                                     or `match await ref.{method}(...) {{ Ok(v) => ..., Err(e) => ... }}`",
                                ),
                            );
                        }
                        // Record the dispatch discriminator (Fire vs Ask). This
                        // also marks the span as already-rewritten below, so the
                        // synchronous `RewriteToFunction` path is skipped and the
                        // call lowers to `ActorSend` / `ActorAsk` in HIR.
                        self.record_actor_method_dispatch(
                            span,
                            method_key,
                            applied_sig.return_type.clone(),
                        );
                        return applied_sig.return_type;
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
                                }
                                self.machine_method_dispatch.insert(
                                    SpanKey::in_module(span, self.current_module_idx),
                                    MachineMethodKind::Step {
                                        machine_name: name.clone(),
                                    },
                                );
                            }
                            "state_name" => {
                                self.machine_method_dispatch.insert(
                                    SpanKey::in_module(span, self.current_module_idx),
                                    MachineMethodKind::StateName {
                                        machine_name: name.clone(),
                                    },
                                );
                            }
                            _ => {}
                        }
                    }
                    // #1295: a terminal consuming `close` moves its receiver, so
                    // record the per-call-site flag for HIR/codegen AND mark the
                    // receiver expression moved (a later use surfaces
                    // `UseAfterMove`). Two surfaces qualify:
                    //   1. stdlib `impl Closable for T { fn close }` — the trait
                    //      `close` flattens into T's inherent-method table; honour
                    //      the `consumes_receiver` declared on the trait.
                    //   2. a `#[resource]` type's inherent `fn close(self)` — the
                    //      W3.030 implicit-drop dispatch target, which when called
                    //      explicitly also moves the receiver so the scope-exit
                    //      implicit drop is suppressed on the consumed path (no
                    //      double-close).
                    let consumes_receiver = self.named_type_method_consumes_receiver(name, method)
                        || self.named_type_inherent_close_consumes_receiver(name, method, &sig);
                    if consumes_receiver {
                        self.method_call_consumes_receiver
                            .insert(SpanKey::in_module(span, self.current_module_idx));
                        let resolved_recv = self.subst.resolve(&receiver_ty);
                        self.mark_expr_moved_if_non_copy(&receiver.0, &receiver.1, &resolved_recv);
                    }
                    self.record_handle_method_call_rewrite_if_any(&resolved, method, span);
                    self.record_builtin_option_result_method_rewrite_if_any(
                        name, type_args, method, span,
                    );
                    self.record_named_extern_symbol_rewrite_if_any(
                        name, type_args, method, &sig, span,
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
                    let already_rewritten = self.method_call_rewrites.contains_key(&span_key)
                        || self.machine_method_dispatch.contains_key(&span_key)
                        || self.actor_method_dispatch.contains_key(&span_key)
                        || self.dyn_trait_method_calls.contains_key(&span_key)
                        || self.resolved_calls.contains_key(&span_key);
                    if !already_rewritten {
                        let method_owner = name
                            .rsplit_once('.')
                            .map_or(name.as_str(), |(_, unqualified)| unqualified);
                        let method_key = format!("{method_owner}::{method}");
                        // Binary wire codec: `value.encode() -> bytes` on a
                        // `#[wire]` struct. The instance method is registered in
                        // `type_def.methods` (not `fn_sigs`), so it never matches
                        // the `fn_sigs` branch below and would otherwise fall
                        // through to `MethodCallNoRewrite`. Record a dedicated
                        // codec rewrite so HIR/codegen call the
                        // `__hew_serialize_<key>` thunk with the correct ABI. The
                        // text-format instance methods (`to_json`/`to_yaml`) are
                        // deliberately excluded — no text-format thunk exists, so
                        // they stay fail-closed.
                        if method == "encode" && self.wire_struct_types.contains(name) {
                            if let Ok(value_ty) =
                                ResolvedTy::from_ty(&self.subst.resolve(&resolved))
                            {
                                self.record_method_call_rewrite(
                                    span,
                                    MethodCallRewrite::WireCodec {
                                        direction: WireCodecDirection::Encode,
                                        value_ty,
                                    },
                                );
                            }
                        } else if method_owner == "VecIter" && method == "next" {
                            if let Some(elem_ty) = type_args.first() {
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
                        } else if self.fn_sigs.contains_key(&method_key) {
                            self.record_method_call_rewrite(
                                span,
                                MethodCallRewrite::RewriteToFunction {
                                    c_symbol: method_key,
                                    // User-defined `Type::method` dispatch is
                                    // open-set; the typed runtime-call catalog
                                    // does not enumerate user method keys.
                                    descriptor: None,
                                    elem_ty: None,
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
                                },
                            );
                        }
                    }
                    return applied_sig.return_type;
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
                        let declaring =
                            self.collect_all_declaring_traits_for_method(bound_trait, method);
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
                                        "trait method `{declaring_trait}::{method}` \
                                         (statically dispatched on type parameter `{name}`) \
                                         requires a mutable binding receiver; \
                                         {receiver_label} is not declared with `var`",
                                    ),
                                );
                            } else if let Some(n) = &receiver_binding_name {
                                self.env.mark_written(n);
                            }
                        }
                        let applied_sig = self.apply_instantiated_call_signature(
                            &trait_sig,
                            None,
                            args,
                            span,
                            SignatureArgApplication::PositionalOnly {
                                arity_context: format!("method '{method}'"),
                            },
                            true,
                        );
                        if declaring_trait == "Pid" && method == "tell" {
                            // TODO(A640): replace this fail-closed branch with
                            // a first-class `P::Msg: Serializable` projection
                            // bound once the checker can express that shape on
                            // pid-polymorphic call sites. If the projection is
                            // already concretely bound (for example
                            // `P: Pid<Msg = Ping>`), the regular Serializable
                            // gate below proves it and the call may proceed.
                            if !self.enforce_pid_polymorphic_tell_serializable_args(args, name) {
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
                        let qualified = format!("{declaring_trait}::{method}");
                        self.apply_consume_receiver_if_flagged(
                            &qualified,
                            receiver,
                            &receiver_ty,
                            span,
                        );
                        // Record the StaticTraitDispatch rewrite for HIR consumption.
                        self.record_method_call_rewrite(
                            span,
                            MethodCallRewrite::StaticTraitDispatch {
                                receiver_type_param: name.clone(),
                                bound_trait,
                                declaring_trait,
                                method_name: method.to_string(),
                                requires_mutable_receiver: trait_sig.requires_mutable_receiver,
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
                                if !self.user_clone_record_seeds.contains(name) {
                                    self.user_clone_record_seeds.push(name.clone());
                                }
                                return resolved;
                            }
                            RecordCloneAdmissibility::OpaqueField { opaque_name } => {
                                self.report_error(
                                    TypeErrorKind::UndefinedMethod,
                                    span,
                                    format!(
                                        "record `{name}` contains an opaque field \
                                         `{opaque_name}` and cannot be cloned"
                                    ),
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
                    if let Some(sig) = self.lookup_trait_method(&bound.trait_name, method) {
                        found_sig = Some(sig);
                        found_bound = Some(bound);
                        break;
                    }
                }

                if let Some(mut sig) = found_sig {
                    let pid_tell_dispatch = found_bound
                        .is_some_and(|bound| bound.trait_name == "Pid" && method == "tell");
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
                            }
                        }
                        // Record the per-call-site vtable-slot resolution that
                        // HIR/MIR lowering will consume to emit
                        // `Instr::CallTraitMethod`. Slot convention follows
                        // `hew-runtime/src/trait_object.rs::HewVtable`: slots
                        // 0..3 are the fixed prefix triple
                        // (`drop_in_place`/`size_of`/`align_of`), trait methods
                        // start at slot 3 in trait-declaration order.
                        if let Some(trait_info) = self.trait_defs.get(&bound.trait_name) {
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
                                self.dyn_trait_method_calls.insert(
                                    SpanKey::in_module(span, self.current_module_idx),
                                    crate::check::types::DynMethodCall {
                                        trait_name: bound.trait_name.clone(),
                                        method_name: method.to_string(),
                                        slot,
                                        signature: sig.clone(),
                                    },
                                );
                            }
                        }
                        let qualified = format!("{}::{method}", bound.trait_name);
                        self.apply_consume_receiver_if_flagged(
                            &qualified,
                            receiver,
                            &receiver_ty,
                            span,
                        );
                    }
                    let applied_sig = self.apply_instantiated_call_signature(
                        &sig,
                        None,
                        args,
                        span,
                        SignatureArgApplication::PositionalOnly {
                            arity_context: format!("method '{method}'"),
                        },
                        true,
                    );
                    if pid_tell_dispatch {
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
                    self.report_error(
                        TypeErrorKind::UndefinedMethod,
                        span,
                        format!("no method `{method}` on `{}`", resolved.user_facing()),
                    );
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
                        self.warnings.push(crate::error::TypeError {
                            severity: crate::error::Severity::Warning,
                            kind: TypeErrorKind::StyleSuggestion,
                            span: span.clone(),
                            message: format!(
                                "cloning a Copy type `{}` is redundant; \
                                 this is equivalent to a plain copy",
                                resolved.user_facing()
                            ),
                            notes: vec![],
                            suggestions: vec![
                                "remove the `clone` — Copy types are duplicated automatically"
                                    .to_string(),
                            ],
                            source_module: self.current_module.clone(),
                        });
                        self.record_method_call_rewrite(span, MethodCallRewrite::CopyCloneNoop);
                        return resolved;
                    }
                }
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::UndefinedMethod,
                    span,
                    format!("no method `{method}` on `{}`", resolved.user_facing()),
                );
                Ty::Error
            }
        }
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
                    symbol_name: "hew_hashmap_remove_layout".to_string(),
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
        ],
    });
    registry.register(ImplDef {
        trait_name: "Seq".to_string(),
        self_pattern: TyPattern::App {
            ctor: "Vec".to_string(),
            args: vec![TyPattern::Var("T".to_string())],
        },
        where_bounds: vec![],
        methods: vec![
            vec_method_target("push", VecMethod::Push, RuntimeAbi::ByRefMut),
            vec_method_target("pop", VecMethod::Pop, RuntimeAbi::ByRefMut),
            vec_method_target("len", VecMethod::Len, RuntimeAbi::ByRef),
            vec_method_target("get", VecMethod::Get, RuntimeAbi::ByRef),
            vec_method_target("set", VecMethod::Set, RuntimeAbi::ByRefMut),
            vec_method_target("remove", VecMethod::Remove, RuntimeAbi::ByRefMut),
            vec_method_target("contains", VecMethod::Contains, RuntimeAbi::ByRef),
            vec_method_target("is_empty", VecMethod::IsEmpty, RuntimeAbi::ByRef),
            vec_method_target("clear", VecMethod::Clear, RuntimeAbi::ByRefMut),
            vec_method_target("clone", VecMethod::Clone, RuntimeAbi::ByRef),
            vec_method_target("append", VecMethod::Append, RuntimeAbi::ByRefMut),
            vec_method_target("extend", VecMethod::Extend, RuntimeAbi::ByRefMut),
            vec_method_target("join", VecMethod::Join, RuntimeAbi::ByRef),
        ],
    });
    registry
}

fn vec_method_target(
    method: &str,
    vec_method: VecMethod,
    abi: RuntimeAbi,
) -> (String, MethodTarget) {
    (
        method.to_string(),
        MethodTarget {
            symbol_name: format!("hew_vec_{method}_FAMILY"),
            family: MethodTargetFamily::Vec(vec_method),
            abi,
            call_hint: CallAbiHint::RuntimeShim,
            consumes_receiver: false,
        },
    )
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::module_registry::ModuleRegistry;

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
    fn stream_receiver_element_kind_stays_canonical() {
        assert_eq!(Checker::stream_receiver_element_kind(&Ty::String), "string");
        assert_eq!(Checker::stream_receiver_element_kind(&Ty::Bytes), "bytes");
        assert_eq!(
            Checker::stream_receiver_element_kind(&Ty::Named {
                builtin: None,
                name: "String".into(),
                args: vec![],
            }),
            ""
        );
        assert_eq!(
            Checker::stream_receiver_element_kind(&Ty::Var(TypeVar::fresh())),
            ""
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
            },
        );
        checker.deferred_hashmap_admission.insert(
            SpanKey::in_module(&span_b, 0),
            DeferredHashMapAdmission {
                span: span_b.clone(),
                key_ty: Ty::Var(key_var),
                val_ty: Ty::Var(val_var),
                source_module: None,
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
        for kind in [
            CollectionKind::HashMap,
            CollectionKind::HashSet,
            CollectionKind::Vec,
        ] {
            assert_eq!(arity_of(kind, "len"), None, "{kind:?}::len skips arity");
            assert_eq!(
                arity_of(kind, "is_empty"),
                None,
                "{kind:?}::is_empty skips arity"
            );
        }
        // Vec `set`/`append`/`extend` also historically skipped arity.
        assert_eq!(arity_of(CollectionKind::Vec, "set"), None);
        assert_eq!(arity_of(CollectionKind::Vec, "append"), None);
        assert_eq!(arity_of(CollectionKind::Vec, "extend"), None);
    }

    #[test]
    fn descriptor_table_checked_arities() {
        assert_eq!(arity_of(CollectionKind::HashMap, "insert"), Some(2));
        assert_eq!(arity_of(CollectionKind::HashMap, "get"), Some(1));
        assert_eq!(arity_of(CollectionKind::HashMap, "keys"), Some(0));
        assert_eq!(arity_of(CollectionKind::HashSet, "insert"), Some(1));
        assert_eq!(arity_of(CollectionKind::HashSet, "contains"), Some(1));
        assert_eq!(arity_of(CollectionKind::HashSet, "clone"), Some(0));
        assert_eq!(arity_of(CollectionKind::Vec, "push"), Some(1));
        assert_eq!(arity_of(CollectionKind::Vec, "get"), Some(1));
        assert_eq!(arity_of(CollectionKind::Vec, "pop"), Some(0));
        assert_eq!(arity_of(CollectionKind::Vec, "clone"), Some(0));
    }

    #[test]
    fn descriptor_table_arg_and_return_shapes() {
        let hm_insert = collection_method_desc(CollectionKind::HashMap, "insert").unwrap();
        assert_eq!(
            hm_insert.arg_templates,
            &[ArgTemplate::Key, ArgTemplate::Value]
        );
        assert_eq!(hm_insert.ret, RetTemplate::Unit);

        let hm_get = collection_method_desc(CollectionKind::HashMap, "get").unwrap();
        assert_eq!(hm_get.arg_templates, &[ArgTemplate::Key]);
        assert_eq!(hm_get.ret, RetTemplate::OptionVal);

        let hm_keys = collection_method_desc(CollectionKind::HashMap, "keys").unwrap();
        assert_eq!(hm_keys.ret, RetTemplate::VecOfKey);
        let hm_values = collection_method_desc(CollectionKind::HashMap, "values").unwrap();
        assert_eq!(hm_values.ret, RetTemplate::VecOfVal);

        let set_insert = collection_method_desc(CollectionKind::HashSet, "insert").unwrap();
        assert_eq!(set_insert.arg_templates, &[ArgTemplate::Elem]);
        assert_eq!(set_insert.ret, RetTemplate::Bool);

        let vec_get = collection_method_desc(CollectionKind::Vec, "get").unwrap();
        assert_eq!(vec_get.arg_templates, &[ArgTemplate::I64]);
        assert_eq!(vec_get.ret, RetTemplate::Elem);

        let vec_set = collection_method_desc(CollectionKind::Vec, "set").unwrap();
        assert_eq!(
            vec_set.arg_templates,
            &[ArgTemplate::I64, ArgTemplate::Elem]
        );

        let vec_append = collection_method_desc(CollectionKind::Vec, "append").unwrap();
        assert_eq!(vec_append.arg_templates, &[ArgTemplate::Receiver]);

        for clone_kind in [
            CollectionKind::HashMap,
            CollectionKind::HashSet,
            CollectionKind::Vec,
        ] {
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
        assert!(collection_method_desc(CollectionKind::Vec, "nope").is_none());
        // Genuinely divergent Vec arms stay code-side hooks, not table rows.
        for divergent in ["contains", "map", "filter", "fold", "join"] {
            assert!(
                collection_method_desc(CollectionKind::Vec, divergent).is_none(),
                "Vec::{divergent} must remain a code-side hook, not a descriptor row"
            );
        }
    }
}
