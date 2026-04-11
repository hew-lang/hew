#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

pub(crate) fn signature_contains_error_type(params: &[Ty], ret: &Ty) -> bool {
    params.iter().any(ty_contains_error) || ty_contains_error(ret)
}

/// Enforce the fail-closed output contract for `lowering_facts` after
/// [`Checker::finalize_lowering_facts`] has run.
///
/// Two conditions trigger removal of a [`LoweringFact`] entry:
///
/// 1. **Orphaned span** — the `SpanKey` is absent from the post-validation
///    `expr_types` map.  If the owning expression was pruned by
///    `validate_checker_output_contract` (leaked inference vars, cascading
///    `Ty::Error`, etc.) the corresponding lowering fact must also be dropped so
///    downstream serialization/codegen cannot observe a fact without a resolved
///    expression type.
///
/// 2. **Internally inconsistent fact** (defensive) — the `element_type` /
///    `abi_variant` pairing violates the checker-invariant.  In practice this
///    cannot occur through the normal construction path
///    (`LoweringFact::from_hashset_element_type`) but the check is kept as a
///    hard contract at the boundary so that any future serialization round-trip
///    or factory bypasses are caught at check time rather than in C++ codegen.
///
/// Note: element types that resolve to `Ty::Error` are already handled earlier
/// in `finalize_lowering_facts` (silent drop, no new error).  The orphan-prune
/// here is a secondary defence for any path that might add facts after the main
/// validation pass.
pub(super) fn validate_lowering_facts_output_contract(
    lowering_facts: &mut HashMap<SpanKey, LoweringFact>,
    expr_types: &HashMap<SpanKey, Ty>,
) {
    use crate::lowering_facts::{HashSetAbi, HashSetElementType, LoweringKind};
    lowering_facts.retain(|key, fact| {
        // Condition 1: orphaned span.
        if !expr_types.contains_key(key) {
            return false;
        }
        // Condition 2: element_type ↔ abi_variant internal consistency.
        matches!(
            (fact.kind, fact.element_type, fact.abi_variant),
            (
                LoweringKind::HashSet,
                HashSetElementType::I64 | HashSetElementType::U64,
                HashSetAbi::Int64
            ) | (
                LoweringKind::HashSet,
                HashSetElementType::Str,
                HashSetAbi::String
            )
        )
    });
}

fn ty_contains_error(ty: &Ty) -> bool {
    ty.contains_error()
}

fn normalize_synthetic_channel_handle_expr_type(ty: &Ty) -> Ty {
    match ty {
        Ty::Named { name, args } => {
            let normalized_args: Vec<Ty> = args
                .iter()
                .map(normalize_synthetic_channel_handle_expr_type)
                .collect();
            if matches!(
                builtin_named_type(name.as_str()),
                Some(kind) if kind.is_channel_handle()
            ) && matches!(normalized_args.as_slice(), [Ty::Var(_)])
            {
                return Ty::normalize_named(name.clone(), vec![]);
            }
            Ty::normalize_named(name.clone(), normalized_args)
        }
        Ty::Tuple(elems) => Ty::Tuple(
            elems
                .iter()
                .map(normalize_synthetic_channel_handle_expr_type)
                .collect(),
        ),
        Ty::Array(elem, size) => Ty::Array(
            Box::new(normalize_synthetic_channel_handle_expr_type(elem)),
            *size,
        ),
        Ty::Slice(elem) => Ty::Slice(Box::new(normalize_synthetic_channel_handle_expr_type(elem))),
        Ty::Function { params, ret } => Ty::Function {
            params: params
                .iter()
                .map(normalize_synthetic_channel_handle_expr_type)
                .collect(),
            ret: Box::new(normalize_synthetic_channel_handle_expr_type(ret)),
        },
        Ty::Closure {
            params,
            ret,
            captures,
        } => Ty::Closure {
            params: params
                .iter()
                .map(normalize_synthetic_channel_handle_expr_type)
                .collect(),
            ret: Box::new(normalize_synthetic_channel_handle_expr_type(ret)),
            captures: captures
                .iter()
                .map(normalize_synthetic_channel_handle_expr_type)
                .collect(),
        },
        Ty::Pointer {
            is_mutable,
            pointee,
        } => Ty::Pointer {
            is_mutable: *is_mutable,
            pointee: Box::new(normalize_synthetic_channel_handle_expr_type(pointee)),
        },
        _ => ty.clone(),
    }
}

fn ty_references_tracked_inference_var(ty: &Ty, tracked_vars: &HashSet<TypeVar>) -> bool {
    let mut unresolved = HashSet::new();
    collect_unresolved_inference_vars(ty, &mut unresolved);
    !unresolved.is_disjoint(tracked_vars)
}

fn fn_sig_references_tracked_inference_var(sig: &FnSig, tracked_vars: &HashSet<TypeVar>) -> bool {
    sig.params
        .iter()
        .any(|ty| ty_references_tracked_inference_var(ty, tracked_vars))
        || ty_references_tracked_inference_var(&sig.return_type, tracked_vars)
}

fn variant_def_references_tracked_inference_var(
    variant: &VariantDef,
    tracked_vars: &HashSet<TypeVar>,
) -> bool {
    match variant {
        VariantDef::Unit => false,
        VariantDef::Tuple(fields) => fields
            .iter()
            .any(|ty| ty_references_tracked_inference_var(ty, tracked_vars)),
        VariantDef::Struct(fields) => fields
            .iter()
            .any(|(_, ty)| ty_references_tracked_inference_var(ty, tracked_vars)),
    }
}

fn variant_def_contains_error_type(variant: &VariantDef) -> bool {
    match variant {
        VariantDef::Unit => false,
        VariantDef::Tuple(fields) => fields.iter().any(ty_contains_error),
        VariantDef::Struct(fields) => fields.iter().any(|(_, ty)| ty_contains_error(ty)),
    }
}

fn type_def_shape_contains_error_type(type_def: &TypeDef) -> bool {
    type_def.fields.values().any(ty_contains_error)
        || type_def
            .variants
            .values()
            .any(variant_def_contains_error_type)
}

fn type_def_shape_references_tracked_inference_var(
    type_def: &TypeDef,
    tracked_vars: &HashSet<TypeVar>,
) -> bool {
    type_def
        .fields
        .values()
        .any(|ty| ty_references_tracked_inference_var(ty, tracked_vars))
        || type_def
            .variants
            .values()
            .any(|variant| variant_def_references_tracked_inference_var(variant, tracked_vars))
}

#[derive(Clone, Copy)]
enum ConcreteCollectionKind {
    Vec,
    HashSet,
    HashMap,
}

impl ConcreteCollectionKind {
    fn validate_named_collection(
        self,
        checker: &mut Checker,
        name: &str,
        args: &[Ty],
        span: &Span,
    ) -> Option<bool> {
        match self {
            Self::Vec if name == "Vec" && args.len() == 1 => {
                Some(checker.validate_vec_element_type(&args[0], span))
            }
            Self::HashSet if name == "HashSet" && args.len() == 1 => {
                // Skip admission when the element type is still unresolved or
                // erroneous: Ty::Var is not yet decidable (inference may
                // resolve it), and Ty::Error already has an upstream
                // diagnostic.  The dedicated deferred-admission paths
                // (validate_hashset_element_type from method-call sites) and
                // the inference-holes path handle those cases with proper
                // authority and without duplication.
                let resolved = checker.subst.resolve(&args[0]);
                if matches!(resolved, Ty::Var(_) | Ty::Error) {
                    return Some(true);
                }
                Some(checker.validate_hashset_element_type(&args[0], span))
            }
            Self::HashMap if name == "HashMap" && args.len() == 2 => {
                // Same: skip admission for undecidable/erroneous args.
                let resolved_key = checker.subst.resolve(&args[0]);
                let resolved_val = checker.subst.resolve(&args[1]);
                if matches!(resolved_key, Ty::Var(_) | Ty::Error)
                    || matches!(resolved_val, Ty::Var(_) | Ty::Error)
                {
                    return Some(true);
                }
                Some(checker.validate_hashmap_key_value_types(&args[0], &args[1], span))
            }
            _ => None,
        }
    }
}

impl Checker {
    pub(super) fn validate_checker_output_contract(
        &mut self,
        expr_types: &mut HashMap<SpanKey, Ty>,
        type_defs: &mut HashMap<String, TypeDef>,
        fn_sigs: &mut HashMap<String, FnSig>,
        call_type_args: &mut HashMap<SpanKey, Vec<Ty>>,
    ) {
        let covered_inference_vars = self.collect_output_contract_tracked_inference_vars();
        self.validate_expr_output_contract(expr_types, &covered_inference_vars);

        type_defs.retain(|_, type_def| {
            if type_def_shape_references_tracked_inference_var(type_def, &covered_inference_vars)
                || type_def_shape_contains_error_type(type_def)
            {
                return false;
            }
            type_def.methods.retain(|_, sig| {
                !fn_sig_references_tracked_inference_var(sig, &covered_inference_vars)
                    && !signature_contains_error_type(&sig.params, &sig.return_type)
            });
            true
        });

        fn_sigs.retain(|_, sig| {
            !fn_sig_references_tracked_inference_var(sig, &covered_inference_vars)
                && !signature_contains_error_type(&sig.params, &sig.return_type)
        });
        Self::validate_call_type_args_output_contract(call_type_args, expr_types);
        self.validate_assign_target_output_contract();
        self.validate_method_call_output_contract(expr_types);
        self.validate_method_call_receiver_kinds_output_contract(type_defs, fn_sigs);
    }

    fn collect_output_contract_tracked_inference_vars(&self) -> HashSet<TypeVar> {
        let mut covered_inference_vars = HashSet::new();
        for hole_vars in self
            .type_def_inference_holes
            .values()
            .chain(self.fn_sig_inference_holes.values())
            .chain(
                self.deferred_inference_holes
                    .iter()
                    .map(|hole| &hole.hole_vars),
            )
        {
            for hole_var in hole_vars {
                let resolved_hole = self.subst.resolve(&Ty::Var(*hole_var));
                collect_unresolved_inference_vars(&resolved_hole, &mut covered_inference_vars);
            }
        }
        for site in &self.deferred_monomorphic_sites {
            let resolved = self.subst.resolve(&site.ty);
            collect_unresolved_inference_vars(&resolved, &mut covered_inference_vars);
        }
        for poly_vars in self.lambda_poly_type_var_map.values() {
            for (_, poly_var) in poly_vars {
                let resolved_poly = self.subst.resolve(&Ty::Var(*poly_var));
                collect_unresolved_inference_vars(&resolved_poly, &mut covered_inference_vars);
            }
        }
        covered_inference_vars
    }

    fn validate_expr_output_contract(
        &mut self,
        expr_types: &mut HashMap<SpanKey, Ty>,
        covered_inference_vars: &HashSet<TypeVar>,
    ) {
        let mut seen_inference_spans: HashSet<SpanKey> = self
            .errors
            .iter()
            .filter(|e| e.kind == TypeErrorKind::InferenceFailed)
            .map(|e| SpanKey::from(&e.span))
            .collect();
        let mut leaked_expr_type_spans = Vec::new();
        for (span, ty) in expr_types.iter_mut() {
            let mut unresolved = HashSet::new();
            collect_unresolved_inference_vars(ty, &mut unresolved);
            if unresolved.is_empty() {
                continue;
            }
            if !unresolved.is_subset(covered_inference_vars) {
                let normalized = normalize_synthetic_channel_handle_expr_type(ty);
                if normalized != *ty {
                    let mut normalized_unresolved = HashSet::new();
                    collect_unresolved_inference_vars(&normalized, &mut normalized_unresolved);
                    if normalized_unresolved.is_empty() {
                        *ty = normalized;
                        continue;
                    }
                }
            }
            leaked_expr_type_spans.push(span.clone());
            if unresolved.is_subset(covered_inference_vars) {
                continue;
            }
            if seen_inference_spans.insert(span.clone()) {
                let mut err = TypeError::inference_failed(
                    Span {
                        start: span.start,
                        end: span.end,
                    },
                    "expression type at checker output boundary",
                );
                err.source_module = self.expr_type_source_modules.get(span).cloned().flatten();
                self.errors.push(err);
            }
        }
        for span in leaked_expr_type_spans {
            expr_types.remove(&span);
        }
    }

    /// Validates `call_type_args` at the checker output boundary.
    ///
    /// Two conditions trigger removal:
    ///
    /// 1. **Orphaned span** — the `SpanKey` is absent from the post-validation
    ///    `expr_types` map, meaning the owning expression was pruned by
    ///    `validate_expr_output_contract` (leaked inference vars, cascading
    ///    errors, etc.).  This mirrors the fail-closed pruning already applied
    ///    to `method_call_receiver_kinds` / `method_call_rewrites`.
    /// 2. **Leaked inference variable** — any type argument still contains an
    ///    unresolved `Ty::Var`.  A call site whose type arguments are
    ///    unresolved must not cross the checker output boundary into codegen.
    fn validate_call_type_args_output_contract(
        call_type_args: &mut HashMap<SpanKey, Vec<Ty>>,
        expr_types: &HashMap<SpanKey, Ty>,
    ) {
        call_type_args.retain(|key, args| {
            expr_types.contains_key(key) && args.iter().all(|ty| !ty.has_inference_var())
        });
    }

    /// Prune `method_call_receiver_kinds` and `method_call_rewrites` entries
    /// whose `SpanKey` is absent from the validated `expr_types` map.
    ///
    /// `expr_types` here is the post-validation map produced by
    /// `validate_expr_output_contract` — any span that was pruned there (due
    /// to leaked inference vars, cascading errors, etc.) is authoritative
    /// evidence that the corresponding method-call side-table entry is orphaned
    /// and must not leak to the output.  This mirrors the fail-closed contract
    /// already applied to `assign_target_kinds` / `assign_target_shapes`.
    fn validate_method_call_output_contract(&mut self, expr_types: &HashMap<SpanKey, Ty>) {
        self.method_call_receiver_kinds
            .retain(|key, _| expr_types.contains_key(key));
        self.method_call_rewrites
            .retain(|key, _| expr_types.contains_key(key));
    }

    /// Validates `method_call_receiver_kinds` at the checker output boundary.
    ///
    /// This pass graduates the side-table from producer discipline to a validated
    /// contract by asserting that every surviving entry references a type or trait
    /// that still exists in the resolved program environment, then pruning any that
    /// do not.
    ///
    /// - `NamedTypeInstance { type_name }` entries are retained if the type is
    ///   present in the resolved `type_defs` (user-defined type), the name is
    ///   module-qualified (contains `'.'`, i.e., a stdlib handle type such as
    ///   `json.Value` or `http.Client` which live in the module registry rather
    ///   than `type_defs`), or the name is a generic type parameter from a
    ///   function signature (trait-bounded type-parameter dispatch records the
    ///   type-parameter name as a `NamedTypeInstance`).
    /// - `TraitObject { trait_name }` entries are retained only if the trait name
    ///   is still present in the checker's trait registry.
    pub(super) fn validate_method_call_receiver_kinds_output_contract(
        &mut self,
        type_defs: &HashMap<String, TypeDef>,
        fn_sigs: &HashMap<String, FnSig>,
    ) {
        // Collect known trait names before the mutable borrow on
        // `method_call_receiver_kinds` to avoid a split-borrow conflict.
        let known_trait_names: HashSet<String> = self.trait_defs.keys().cloned().collect();

        // Collect all type parameter names from the resolved function signatures
        // so we can retain `NamedTypeInstance` entries produced by trait-bounded
        // type-parameter method dispatch (e.g. `T` in `fn f<T: Show>(t: T)`).
        // NOTE: we receive `fn_sigs` as a parameter because the production path
        // drains `self.fn_sigs` via `std::mem::take` before this validator runs.
        let known_type_params: HashSet<&str> = fn_sigs
            .values()
            .flat_map(|sig| sig.type_params.iter().map(String::as_str))
            .collect();

        self.method_call_receiver_kinds
            .retain(|_, kind| match kind {
                MethodCallReceiverKind::NamedTypeInstance { type_name } => {
                    type_defs.contains_key(type_name)
                        || type_name.contains('.')
                        || known_type_params.contains(type_name.as_str())
                }
                MethodCallReceiverKind::TraitObject { trait_name } => {
                    known_trait_names.contains(trait_name)
                }
            });
    }

    fn validate_assign_target_output_contract(&mut self) {
        let valid_keys: HashSet<_> = self
            .assign_target_kinds
            .keys()
            .filter(|key| self.assign_target_shapes.contains_key(*key))
            .cloned()
            .collect();
        self.assign_target_kinds
            .retain(|key, _| valid_keys.contains(key));
        self.assign_target_shapes
            .retain(|key, _| valid_keys.contains(key));
    }

    pub(super) fn validate_stream_sink_element_type(
        &mut self,
        type_args: &[Ty],
        type_name: &str,
        method_name: &str,
        span: &Span,
    ) -> Option<Ty> {
        let _ = method_name;
        let inner = type_args
            .first()
            .cloned()
            .unwrap_or(Ty::Var(TypeVar::fresh()));
        let is_supported = matches!(&inner, Ty::String | Ty::Bytes | Ty::Var(_) | Ty::Error);
        if !is_supported {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "`{type_name}<{}>` is not supported; \
                     {type_name}<T> is currently only implemented for String and bytes",
                    inner.user_facing()
                ),
            );
            return None;
        }
        Some(inner)
    }

    pub(super) fn report_unlowerable_stream_codec_boundary(
        &mut self,
        type_name: &str,
        inner: &Ty,
        method: &str,
        span: &Span,
    ) -> Ty {
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "`{method}()` is not available on `{type_name}<{}>` yet; lowering/runtime support is not implemented",
                inner.user_facing()
            ),
        );
        Ty::Error
    }

    pub(super) fn reject_rc_collection_element(
        &mut self,
        container: &str,
        elem_ty: &Ty,
        span: &Span,
    ) {
        let resolved = self.subst.resolve(elem_ty);
        if !self
            .registry
            .implements_marker(&resolved, MarkerTrait::RcFree)
        {
            self.report_error(
                TypeErrorKind::UnsafeCollectionElement,
                span,
                format!(
                    "`{container}` cannot hold `{}`; Rc<T> in collections is not yet \
                     supported (runtime does not track Rc ownership for collection elements)",
                    resolved.user_facing()
                ),
            );
        }
    }

    fn is_supported_hashmap_key_type(ty: &Ty) -> bool {
        matches!(ty, Ty::String)
    }

    fn is_supported_hashmap_value_type(ty: &Ty) -> bool {
        matches!(ty, Ty::String | Ty::Bool | Ty::Char | Ty::Duration) || ty.is_numeric()
    }

    pub(super) fn validate_hashmap_key_value_types(
        &mut self,
        key_ty: &Ty,
        val_ty: &Ty,
        span: &Span,
    ) -> bool {
        let resolved_key = self.subst.resolve(key_ty);
        let resolved_val = self.subst.resolve(val_ty);

        // Ty::Error: upstream already emitted a diagnostic; fail closed silently
        // to prevent cascading errors from admission logic.
        if matches!(resolved_key, Ty::Error) || matches!(resolved_val, Ty::Error) {
            return false;
        }

        // Ty::Var: inference is still in-flight at this call site.  Defer the
        // admission check until finalize_hashmap_admission() runs after all
        // inference has settled, mirroring the HashSet lowering-fact pattern.
        if matches!(resolved_key, Ty::Var(_)) || matches!(resolved_val, Ty::Var(_)) {
            self.deferred_hashmap_admission
                .entry(SpanKey::from(span))
                .or_insert_with(|| DeferredHashMapAdmission {
                    span: span.clone(),
                    key_ty: key_ty.clone(),
                    val_ty: val_ty.clone(),
                    source_module: self.current_module.clone(),
                });
            return true; // optimistically admit; finalization will fail closed
        }

        if Self::is_supported_hashmap_key_type(&resolved_key)
            && Self::is_supported_hashmap_value_type(&resolved_val)
        {
            return true;
        }

        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "HashMap<{}, {}> is not supported; HashMap currently requires \
                 String keys and scalar/string values (bool, char, integer, \
                 float, duration, or String)",
                resolved_key.user_facing(),
                resolved_val.user_facing()
            ),
        );
        false
    }

    fn reject_unsafe_hashmap_element_types(
        &mut self,
        key_ty: &Ty,
        val_ty: &Ty,
        span: &Span,
    ) -> bool {
        self.reject_rc_collection_element("HashMap", key_ty, span);
        self.reject_rc_collection_element("HashMap", val_ty, span);

        let resolved_key = self.subst.resolve(key_ty);
        let resolved_val = self.subst.resolve(val_ty);
        if !self
            .registry
            .implements_marker(&resolved_key, MarkerTrait::RcFree)
        {
            return false;
        }
        if !self
            .registry
            .implements_marker(&resolved_val, MarkerTrait::RcFree)
        {
            return false;
        }
        true
    }

    pub(super) fn validate_hashmap_owned_element_types(
        &mut self,
        key_ty: &Ty,
        val_ty: &Ty,
        span: &Span,
    ) -> bool {
        self.reject_unsafe_hashmap_element_types(key_ty, val_ty, span)
            && self.validate_hashmap_key_value_types(key_ty, val_ty, span)
    }

    pub(super) fn validate_hashset_element_type(&mut self, elem_ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(elem_ty);

        // Ty::Error: upstream already emitted a diagnostic; fail closed silently
        // to prevent cascading errors from admission logic.
        if matches!(resolved, Ty::Error) {
            return false;
        }

        // Ty::Var: inference is still in-flight at this call site.  Defer the
        // admission check until finalize_hashset_admission() runs after all
        // inference has settled, mirroring the HashMap deferred-admission pattern.
        if matches!(resolved, Ty::Var(_)) {
            self.deferred_hashset_admission
                .entry(SpanKey::from(span))
                .or_insert_with(|| DeferredHashSetAdmission {
                    span: span.clone(),
                    elem_ty: elem_ty.clone(),
                    source_module: self.current_module.clone(),
                });
            return true; // optimistically admit; finalization will fail closed
        }

        if matches!(resolved, Ty::String | Ty::I64 | Ty::U64 | Ty::IntLiteral) {
            return true;
        }

        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "HashSet<{}> is not supported; only HashSet<String> and 64-bit integer element types are currently supported",
                resolved.user_facing()
            ),
        );
        false
    }

    pub(super) fn validate_hashset_owned_element_type(
        &mut self,
        elem_ty: &Ty,
        span: &Span,
    ) -> bool {
        self.reject_rc_collection_element("HashSet", elem_ty, span);
        let resolved = self.subst.resolve(elem_ty);
        if !self
            .registry
            .implements_marker(&resolved, MarkerTrait::RcFree)
        {
            return false;
        }
        self.validate_hashset_element_type(elem_ty, span)
    }

    fn instantiate_type_def_member(ty: &Ty, type_params: &[String], type_args: &[Ty]) -> Ty {
        type_params
            .iter()
            .zip(type_args.iter())
            .fold(ty.clone(), |instantiated, (param, arg)| {
                instantiated.substitute_named_param(param, arg)
            })
    }

    pub(super) fn vec_element_contains_structural_array(
        &self,
        ty: &Ty,
        visiting: &mut HashSet<String>,
    ) -> bool {
        let resolved = self.subst.resolve(ty);
        match &resolved {
            Ty::Array(_, _) => true,
            Ty::Tuple(elems) => elems
                .iter()
                .any(|elem| self.vec_element_contains_structural_array(elem, visiting)),
            Ty::Named { name, args } if matches!(name.as_str(), "Range" | "Option" | "Result") => {
                args.iter()
                    .any(|arg| self.vec_element_contains_structural_array(arg, visiting))
            }
            Ty::Named { name, args } => {
                let Some(type_def) = self.lookup_type_def(name) else {
                    return false;
                };
                if visiting.contains(type_def.name.as_str()) {
                    return false;
                }

                visiting.insert(type_def.name.clone());
                let result = type_def.fields.values().any(|field_ty| {
                    let field_ty =
                        Self::instantiate_type_def_member(field_ty, &type_def.type_params, args);
                    self.vec_element_contains_structural_array(&field_ty, visiting)
                }) || type_def.variants.values().any(|variant| match variant {
                    VariantDef::Unit => false,
                    VariantDef::Tuple(tys) => tys.iter().any(|ty| {
                        let ty = Self::instantiate_type_def_member(ty, &type_def.type_params, args);
                        self.vec_element_contains_structural_array(&ty, visiting)
                    }),
                    VariantDef::Struct(fields) => fields.iter().any(|(_, ty)| {
                        let ty = Self::instantiate_type_def_member(ty, &type_def.type_params, args);
                        self.vec_element_contains_structural_array(&ty, visiting)
                    }),
                });
                visiting.remove(type_def.name.as_str());
                result
            }
            _ => false,
        }
    }

    pub(super) fn validate_vec_element_type(&mut self, elem_ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(elem_ty);
        if matches!(resolved, Ty::Var(_) | Ty::Error) {
            return true;
        }

        if !self.validate_concrete_collection_types(&resolved, span) {
            return false;
        }

        let mut visiting = HashSet::new();
        if self.vec_element_contains_structural_array(&resolved, &mut visiting) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "Vec<{}> is not supported; vec lowering does not support array element types yet",
                    resolved.user_facing()
                ),
            );
            return false;
        }

        true
    }

    fn validate_concrete_collection_type(
        &mut self,
        ty: &Ty,
        span: &Span,
        collection: ConcreteCollectionKind,
    ) -> bool {
        let resolved = self.subst.resolve(ty);
        match &resolved {
            Ty::Named { name, args } => {
                if let Some(result) =
                    collection.validate_named_collection(self, name.as_str(), args, span)
                {
                    return result;
                }
                args.iter()
                    .all(|arg| self.validate_concrete_collection_type(arg, span, collection))
            }
            Ty::Tuple(elems) => elems
                .iter()
                .all(|elem| self.validate_concrete_collection_type(elem, span, collection)),
            Ty::Array(elem, _) | Ty::Slice(elem) => {
                self.validate_concrete_collection_type(elem, span, collection)
            }
            Ty::Function { params, ret } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_collection_type(param, span, collection))
                    && self.validate_concrete_collection_type(ret, span, collection)
            }
            Ty::Closure {
                params,
                ret,
                captures,
            } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_collection_type(param, span, collection))
                    && self.validate_concrete_collection_type(ret, span, collection)
                    && captures.iter().all(|capture| {
                        self.validate_concrete_collection_type(capture, span, collection)
                    })
            }
            Ty::Pointer { pointee, .. } => {
                self.validate_concrete_collection_type(pointee, span, collection)
            }
            Ty::TraitObject { traits } => traits.iter().all(|bound| {
                bound
                    .args
                    .iter()
                    .all(|arg| self.validate_concrete_collection_type(arg, span, collection))
            }),
            _ => true,
        }
    }

    pub(super) fn validate_concrete_vec_type(&mut self, ty: &Ty, span: &Span) -> bool {
        self.validate_concrete_collection_type(ty, span, ConcreteCollectionKind::Vec)
    }

    pub(super) fn validate_concrete_hashset_type(&mut self, ty: &Ty, span: &Span) -> bool {
        self.validate_concrete_collection_type(ty, span, ConcreteCollectionKind::HashSet)
    }

    pub(super) fn validate_concrete_collection_types(&mut self, ty: &Ty, span: &Span) -> bool {
        let hashmap_ok = self.validate_concrete_hashmap_type(ty, span);
        let hashset_ok = self.validate_concrete_hashset_type(ty, span);
        let vec_ok = self.validate_concrete_vec_type(ty, span);
        hashmap_ok && hashset_ok && vec_ok
    }

    pub(super) fn make_vec_type(&mut self, elem_ty: Ty, span: &Span) -> Ty {
        let ty = Ty::Named {
            name: "Vec".to_string(),
            args: vec![elem_ty],
        };
        self.validate_concrete_vec_type(&ty, span);
        ty
    }

    pub(super) fn validate_concrete_hashmap_type(&mut self, ty: &Ty, span: &Span) -> bool {
        self.validate_concrete_collection_type(ty, span, ConcreteCollectionKind::HashMap)
    }

    fn rc_payload_drop_supported(&self, ty: &Ty) -> bool {
        let resolved = self.subst.resolve(ty);
        if matches!(resolved, Ty::Error) {
            return true;
        }
        if matches!(resolved, Ty::Var(_)) {
            return false;
        }
        match &resolved {
            Ty::String | Ty::Bytes => true,
            Ty::Named { name, args } if name == "Rc" && args.len() == 1 => {
                self.rc_payload_drop_supported(&args[0])
            }
            Ty::Named { name, .. } if self.type_implements_trait(name, "Drop") => false,
            _ => self
                .registry
                .implements_marker(&resolved, MarkerTrait::Copy),
        }
    }

    pub(super) fn validate_rc_payload_type(&mut self, ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(ty);
        if self.rc_payload_drop_supported(&resolved) {
            return true;
        }

        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "`Rc<{}>` is not currently supported; Rc only accepts Copy payloads, \
                `String`, `bytes`, and nested `Rc` values because the current Rc \
                drop path does not recursively drop owned contents or forward \
                arbitrary user-defined drop impls",
                resolved.user_facing()
            ),
        );
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::module_registry::ModuleRegistry;

    #[test]
    fn ty_contains_error_recurses_through_named_and_closure_types() {
        let ty = Ty::Closure {
            params: vec![Ty::normalize_named(
                "Result".to_string(),
                vec![Ty::I32, Ty::Tuple(vec![Ty::Error])],
            )],
            ret: Box::new(Ty::Bool),
            captures: vec![],
        };

        assert!(ty_contains_error(&ty));
    }

    #[test]
    fn signature_contains_error_type_flags_error_anywhere_in_signature() {
        let params = vec![Ty::I32];
        let ret = Ty::Function {
            params: vec![Ty::Tuple(vec![Ty::Error])],
            ret: Box::new(Ty::Bool),
        };

        assert!(signature_contains_error_type(&params, &ret));
        assert!(!signature_contains_error_type(&[Ty::I32], &Ty::Bool));
    }

    /// Regression guard for issue #789: `validate_checker_output_contract` must
    /// remove `fn_sigs` entries whose parameter or return types contain
    /// `Ty::Error` so they cannot propagate into serialization/codegen.
    #[test]
    fn validate_checker_output_contract_prunes_fn_sigs_with_error_type() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));

        let mut fn_sigs = HashMap::from([
            (
                "good_fn".to_string(),
                FnSig {
                    params: vec![Ty::I32],
                    return_type: Ty::Bool,
                    ..FnSig::default()
                },
            ),
            (
                "error_param_fn".to_string(),
                FnSig {
                    params: vec![Ty::Error],
                    return_type: Ty::I32,
                    ..FnSig::default()
                },
            ),
            (
                "error_return_fn".to_string(),
                FnSig {
                    params: vec![Ty::I32],
                    return_type: Ty::Error,
                    ..FnSig::default()
                },
            ),
        ]);

        let mut expr_types = HashMap::new();
        let mut type_defs = HashMap::new();
        let mut call_type_args = HashMap::new();
        checker.validate_checker_output_contract(
            &mut expr_types,
            &mut type_defs,
            &mut fn_sigs,
            &mut call_type_args,
        );

        assert!(
            fn_sigs.contains_key("good_fn"),
            "clean signature must survive the contract check"
        );
        assert!(
            !fn_sigs.contains_key("error_param_fn"),
            "signature with Ty::Error in params must be pruned"
        );
        assert!(
            !fn_sigs.contains_key("error_return_fn"),
            "signature with Ty::Error as return type must be pruned"
        );
    }

    /// `validate_method_call_receiver_kinds_output_contract` retains entries
    /// for types that exist in the resolved `type_defs` map and prunes those
    /// that do not.
    #[test]
    fn validate_method_call_receiver_kinds_prunes_unknown_named_type_entries() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));

        let known_key = SpanKey { start: 10, end: 20 };
        let unknown_key = SpanKey { start: 30, end: 40 };

        checker.method_call_receiver_kinds.insert(
            known_key.clone(),
            MethodCallReceiverKind::NamedTypeInstance {
                type_name: "Widget".to_string(),
            },
        );
        checker.method_call_receiver_kinds.insert(
            unknown_key.clone(),
            MethodCallReceiverKind::NamedTypeInstance {
                type_name: "Phantom".to_string(),
            },
        );

        let mut type_defs = HashMap::from([(
            "Widget".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Widget".to_string(),
                type_params: vec![],
                fields: HashMap::new(),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                is_indirect: false,
            },
        )]);

        // Populate expr_types for both spans so that validate_method_call_output_contract
        // (span-based pruner) does not wipe entries before validate_method_call_receiver_kinds_output_contract runs.
        let mut expr_types =
            HashMap::from([(known_key.clone(), Ty::I64), (unknown_key.clone(), Ty::I64)]);
        let mut fn_sigs = HashMap::new();
        let mut call_type_args = HashMap::new();
        checker.validate_checker_output_contract(
            &mut expr_types,
            &mut type_defs,
            &mut fn_sigs,
            &mut call_type_args,
        );

        assert!(
            checker.method_call_receiver_kinds.contains_key(&known_key),
            "NamedTypeInstance entry for a type in type_defs must survive"
        );
        assert!(
            !checker
                .method_call_receiver_kinds
                .contains_key(&unknown_key),
            "NamedTypeInstance entry for a type absent from type_defs must be pruned"
        );
    }

    /// Module-qualified type names (e.g. `json.Value`) are retained even though
    /// they are not present in `type_defs` — they live in the module registry.
    #[test]
    fn validate_method_call_receiver_kinds_retains_qualified_handle_type_entries() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));

        let handle_key = SpanKey { start: 50, end: 60 };
        checker.method_call_receiver_kinds.insert(
            handle_key.clone(),
            MethodCallReceiverKind::NamedTypeInstance {
                type_name: "json.Value".to_string(),
            },
        );

        let mut type_defs = HashMap::new(); // empty — json.Value is not a user type
                                            // Populate expr_types for the span so that validate_method_call_output_contract
                                            // (span-based pruner) does not wipe the entry before the name-based validator runs.
        let mut expr_types = HashMap::from([(handle_key.clone(), Ty::I64)]);
        let mut fn_sigs = HashMap::new();
        let mut call_type_args = HashMap::new();
        checker.validate_checker_output_contract(
            &mut expr_types,
            &mut type_defs,
            &mut fn_sigs,
            &mut call_type_args,
        );

        assert!(
            checker.method_call_receiver_kinds.contains_key(&handle_key),
            "qualified handle-type entry (contains '.') must survive validation"
        );
    }

    /// `TraitObject` entries survive when the trait is present in `trait_defs`
    /// and are pruned when the trait is absent.
    #[test]
    fn validate_method_call_receiver_kinds_prunes_unknown_trait_object_entries() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));

        let known_trait_key = SpanKey { start: 70, end: 80 };
        let unknown_trait_key = SpanKey {
            start: 90,
            end: 100,
        };

        checker.method_call_receiver_kinds.insert(
            known_trait_key.clone(),
            MethodCallReceiverKind::TraitObject {
                trait_name: "Greeter".to_string(),
            },
        );
        checker.method_call_receiver_kinds.insert(
            unknown_trait_key.clone(),
            MethodCallReceiverKind::TraitObject {
                trait_name: "GhostTrait".to_string(),
            },
        );

        // Seed trait_defs with only the known trait.
        checker.trait_defs.insert(
            "Greeter".to_string(),
            TraitInfo {
                methods: vec![],
                associated_types: vec![],
                type_params: vec![],
            },
        );

        let mut type_defs = HashMap::new();
        // Populate expr_types for both trait spans so the span-based pruner does not
        // wipe entries before validate_method_call_receiver_kinds_output_contract runs.
        let mut expr_types = HashMap::from([
            (known_trait_key.clone(), Ty::I64),
            (unknown_trait_key.clone(), Ty::I64),
        ]);
        let mut fn_sigs = HashMap::new();
        let mut call_type_args = HashMap::new();
        checker.validate_checker_output_contract(
            &mut expr_types,
            &mut type_defs,
            &mut fn_sigs,
            &mut call_type_args,
        );

        assert!(
            checker
                .method_call_receiver_kinds
                .contains_key(&known_trait_key),
            "TraitObject entry for a trait in trait_defs must survive"
        );
        assert!(
            !checker
                .method_call_receiver_kinds
                .contains_key(&unknown_trait_key),
            "TraitObject entry for a trait absent from trait_defs must be pruned"
        );
    }

    /// `NamedTypeInstance` entries whose `type_name` matches a generic type
    /// parameter from a function signature must survive validation — these are
    /// produced by trait-bounded type-parameter method dispatch.
    #[test]
    fn validate_method_call_receiver_kinds_retains_type_param_entries() {
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));

        let param_key = SpanKey {
            start: 110,
            end: 120,
        };
        checker.method_call_receiver_kinds.insert(
            param_key.clone(),
            MethodCallReceiverKind::NamedTypeInstance {
                type_name: "T".to_string(),
            },
        );

        // Populate the resolved fn_sigs map (mimicking the production path where
        // mod.rs drains self.fn_sigs via std::mem::take into resolved_fn_sigs before
        // calling validate_checker_output_contract).
        let mut fn_sigs = HashMap::from([(
            "display".to_string(),
            FnSig {
                type_params: vec!["T".to_string()],
                type_param_bounds: HashMap::new(),
                param_names: vec!["item".to_string()],
                params: vec![Ty::Named {
                    name: "T".to_string(),
                    args: vec![],
                }],
                return_type: Ty::Unit,
                is_async: false,
                is_pure: false,
                accepts_kwargs: false,
                doc_comment: None,
            },
        )]);
        let mut type_defs = HashMap::new(); // "T" is not a user-defined type
                                            // Populate expr_types for the span so the span-based pruner does not wipe
                                            // the entry before validate_method_call_receiver_kinds_output_contract runs.
        let mut expr_types = HashMap::from([(param_key.clone(), Ty::I64)]);
        let mut call_type_args = HashMap::new();
        checker.validate_checker_output_contract(
            &mut expr_types,
            &mut type_defs,
            &mut fn_sigs,
            &mut call_type_args,
        );

        assert!(
            checker.method_call_receiver_kinds.contains_key(&param_key),
            "NamedTypeInstance entry for a type-parameter name must survive validation"
        );
    }

    /// `validate_call_type_args_output_contract` retains entries whose span is
    /// present in `expr_types` and whose type arguments contain no inference vars.
    #[test]
    fn validate_call_type_args_output_contract_retains_valid_entries() {
        let valid_key = SpanKey { start: 10, end: 20 };
        let mut call_type_args = HashMap::from([(valid_key.clone(), vec![Ty::I32, Ty::Bool])]);
        let expr_types = HashMap::from([(valid_key.clone(), Ty::I32)]);

        Checker::validate_call_type_args_output_contract(&mut call_type_args, &expr_types);

        assert!(
            call_type_args.contains_key(&valid_key),
            "call_type_args entry with concrete types and a present span must survive"
        );
    }

    /// `validate_call_type_args_output_contract` prunes entries whose owning
    /// expression span is absent from the validated `expr_types` map.  An absent
    /// span means `validate_expr_output_contract` already pruned the expression
    /// (leaked inference state, cascading errors, etc.), so the side-table entry
    /// is orphaned and must not reach codegen.
    #[test]
    fn validate_call_type_args_output_contract_prunes_orphaned_entries() {
        let orphan_key = SpanKey { start: 30, end: 40 };
        let mut call_type_args = HashMap::from([(orphan_key.clone(), vec![Ty::I32])]);
        // expr_types is empty — the owning expression was pruned.
        let expr_types: HashMap<SpanKey, Ty> = HashMap::new();

        Checker::validate_call_type_args_output_contract(&mut call_type_args, &expr_types);

        assert!(
            call_type_args.is_empty(),
            "orphaned call_type_args entry (span absent from expr_types) must be pruned"
        );
    }

    /// `validate_call_type_args_output_contract` prunes entries that still contain
    /// unresolved `Ty::Var` inference holes even when the owning span is present in
    /// `expr_types`.  Leaked inference state must not cross the output boundary.
    #[test]
    fn validate_call_type_args_output_contract_prunes_leaked_inference_vars() {
        let present_key = SpanKey { start: 50, end: 60 };
        let inference_var = Ty::Var(crate::ty::TypeVar(42));
        let mut call_type_args =
            HashMap::from([(present_key.clone(), vec![Ty::I32, inference_var])]);
        // The span IS present in expr_types — only the inference var triggers pruning.
        let expr_types = HashMap::from([(present_key.clone(), Ty::I32)]);

        Checker::validate_call_type_args_output_contract(&mut call_type_args, &expr_types);

        assert!(
            call_type_args.is_empty(),
            "call_type_args entry containing Ty::Var must be pruned even if span is present"
        );
    }

    /// Mixed scenario: one valid entry, one orphaned entry, one entry with leaked
    /// inference state — only the valid entry must survive.
    #[test]
    fn validate_call_type_args_output_contract_mixed() {
        let valid_key = SpanKey { start: 10, end: 20 };
        let orphan_key = SpanKey { start: 30, end: 40 };
        let leaked_key = SpanKey { start: 50, end: 60 };
        let inference_var = Ty::Var(crate::ty::TypeVar(7));

        let mut call_type_args = HashMap::from([
            (valid_key.clone(), vec![Ty::I64]),
            (orphan_key.clone(), vec![Ty::Bool]),
            (leaked_key.clone(), vec![inference_var]),
        ]);
        // Only valid_key and leaked_key are present in expr_types.
        let expr_types =
            HashMap::from([(valid_key.clone(), Ty::I64), (leaked_key.clone(), Ty::I64)]);

        Checker::validate_call_type_args_output_contract(&mut call_type_args, &expr_types);

        assert!(
            call_type_args.contains_key(&valid_key),
            "valid call_type_args entry must survive"
        );
        assert!(
            !call_type_args.contains_key(&orphan_key),
            "orphaned call_type_args entry must be pruned"
        );
        assert!(
            !call_type_args.contains_key(&leaked_key),
            "call_type_args entry with leaked inference var must be pruned"
        );
    }

    // ── validate_lowering_facts_output_contract ────────────────────────────────

    /// Well-formed facts whose spans exist in `expr_types` must survive.
    #[test]
    fn lowering_facts_output_contract_retains_valid_facts() {
        use crate::lowering_facts::{
            DropKind, HashSetAbi, HashSetElementType, LoweringFact, LoweringKind,
        };
        let key = SpanKey { start: 1, end: 5 };
        let mut facts = HashMap::from([(
            key.clone(),
            LoweringFact {
                kind: LoweringKind::HashSet,
                element_type: HashSetElementType::I64,
                abi_variant: HashSetAbi::Int64,
                drop_kind: DropKind::HashSetFree,
            },
        )]);
        let expr_types = HashMap::from([(key.clone(), Ty::Bool)]);
        validate_lowering_facts_output_contract(&mut facts, &expr_types);
        assert!(
            facts.contains_key(&key),
            "a well-formed fact with a present span must survive the contract check"
        );
    }

    /// A fact whose span has been pruned from `expr_types` (orphaned) must be
    /// dropped so downstream codegen cannot observe a fact without a resolved
    /// expression type.
    #[test]
    fn lowering_facts_output_contract_prunes_orphaned_facts() {
        use crate::lowering_facts::{
            DropKind, HashSetAbi, HashSetElementType, LoweringFact, LoweringKind,
        };
        let key = SpanKey { start: 10, end: 20 };
        let mut facts = HashMap::from([(
            key.clone(),
            LoweringFact {
                kind: LoweringKind::HashSet,
                element_type: HashSetElementType::Str,
                abi_variant: HashSetAbi::String,
                drop_kind: DropKind::HashSetFree,
            },
        )]);
        // Span is absent from expr_types — simulates the expression being pruned
        // by validate_expr_output_contract due to a leaked inference variable.
        let expr_types: HashMap<SpanKey, Ty> = HashMap::new();
        validate_lowering_facts_output_contract(&mut facts, &expr_types);
        assert!(
            facts.is_empty(),
            "orphaned lowering fact (span absent from expr_types) must be pruned"
        );
    }

    /// An internally inconsistent fact (`element_type` / `abi_variant` mismatch)
    /// must be pruned even if its span exists in `expr_types`.
    #[test]
    fn lowering_facts_output_contract_prunes_inconsistent_facts() {
        use crate::lowering_facts::{
            DropKind, HashSetAbi, HashSetElementType, LoweringFact, LoweringKind,
        };
        let key = SpanKey { start: 30, end: 40 };
        let mut facts = HashMap::from([(
            key.clone(),
            // Intentionally wrong pairing: Str element with Int64 ABI.
            LoweringFact {
                kind: LoweringKind::HashSet,
                element_type: HashSetElementType::Str,
                abi_variant: HashSetAbi::Int64,
                drop_kind: DropKind::HashSetFree,
            },
        )]);
        let expr_types = HashMap::from([(key.clone(), Ty::Bool)]);
        validate_lowering_facts_output_contract(&mut facts, &expr_types);
        assert!(
            facts.is_empty(),
            "internally inconsistent fact (Str/Int64 mismatch) must be pruned"
        );
    }
}
