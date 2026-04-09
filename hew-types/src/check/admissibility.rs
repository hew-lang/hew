#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

pub(crate) fn signature_uses_unsupported_type(params: &[Ty], ret: &Ty) -> bool {
    params.iter().any(ty_contains_error) || ty_contains_error(ret)
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
            if type_def_shape_references_tracked_inference_var(type_def, &covered_inference_vars) {
                return false;
            }
            type_def.methods.retain(|_, sig| {
                !fn_sig_references_tracked_inference_var(sig, &covered_inference_vars)
            });
            true
        });

        fn_sigs.retain(|_, sig| {
            !fn_sig_references_tracked_inference_var(sig, &covered_inference_vars)
                && !signature_uses_unsupported_type(&sig.params, &sig.return_type)
        });
        call_type_args.retain(|_, args| args.iter().all(|ty| !ty_has_unresolved_inference_var(ty)));
        self.validate_assign_target_output_contract();
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
        let mut visiting = HashSet::new();
        if ty_contains_rc_deep(&resolved, &self.type_defs, &mut visiting) {
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
        matches!(ty, Ty::Var(_) | Ty::Error | Ty::String)
    }

    fn is_supported_hashmap_value_type(ty: &Ty) -> bool {
        matches!(
            ty,
            Ty::Var(_) | Ty::Error | Ty::String | Ty::Bool | Ty::Char | Ty::Duration
        ) || ty.is_numeric()
    }

    pub(super) fn validate_hashmap_key_value_types(
        &mut self,
        key_ty: &Ty,
        val_ty: &Ty,
        span: &Span,
    ) -> bool {
        let resolved_key = self.subst.resolve(key_ty);
        let resolved_val = self.subst.resolve(val_ty);
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
        let mut visiting = HashSet::new();
        if ty_contains_rc_deep(&resolved_key, &self.type_defs, &mut visiting) {
            return false;
        }
        let mut visiting = HashSet::new();
        if ty_contains_rc_deep(&resolved_val, &self.type_defs, &mut visiting) {
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
        if matches!(
            resolved,
            Ty::Var(_) | Ty::Error | Ty::String | Ty::I64 | Ty::U64 | Ty::IntLiteral
        ) {
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
        let mut visiting = HashSet::new();
        if ty_contains_rc_deep(&resolved, &self.type_defs, &mut visiting) {
            return false;
        }
        self.validate_hashset_element_type(elem_ty, span)
    }

    fn instantiate_type_def_member(&self, ty: &Ty, type_params: &[String], type_args: &[Ty]) -> Ty {
        type_params
            .iter()
            .zip(type_args.iter())
            .fold(ty.clone(), |instantiated, (param, arg)| {
                self.substitute_named_param(&instantiated, param, arg)
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
                        self.instantiate_type_def_member(field_ty, &type_def.type_params, args);
                    self.vec_element_contains_structural_array(&field_ty, visiting)
                }) || type_def.variants.values().any(|variant| match variant {
                    VariantDef::Unit => false,
                    VariantDef::Tuple(tys) => tys.iter().any(|ty| {
                        let ty = self.instantiate_type_def_member(ty, &type_def.type_params, args);
                        self.vec_element_contains_structural_array(&ty, visiting)
                    }),
                    VariantDef::Struct(fields) => fields.iter().any(|(_, ty)| {
                        let ty = self.instantiate_type_def_member(ty, &type_def.type_params, args);
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

    pub(super) fn validate_concrete_vec_type(&mut self, ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(ty);
        match &resolved {
            Ty::Named { name, args } => {
                if name == "Vec" && args.len() == 1 {
                    return self.validate_vec_element_type(&args[0], span);
                }
                args.iter()
                    .all(|arg| self.validate_concrete_vec_type(arg, span))
            }
            Ty::Tuple(elems) => elems
                .iter()
                .all(|elem| self.validate_concrete_vec_type(elem, span)),
            Ty::Array(elem, _) | Ty::Slice(elem) => self.validate_concrete_vec_type(elem, span),
            Ty::Function { params, ret } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_vec_type(param, span))
                    && self.validate_concrete_vec_type(ret, span)
            }
            Ty::Closure {
                params,
                ret,
                captures,
            } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_vec_type(param, span))
                    && self.validate_concrete_vec_type(ret, span)
                    && captures
                        .iter()
                        .all(|capture| self.validate_concrete_vec_type(capture, span))
            }
            Ty::Pointer { pointee, .. } => self.validate_concrete_vec_type(pointee, span),
            Ty::TraitObject { traits } => traits.iter().all(|bound| {
                bound
                    .args
                    .iter()
                    .all(|arg| self.validate_concrete_vec_type(arg, span))
            }),
            _ => true,
        }
    }

    pub(super) fn validate_concrete_hashset_type(&mut self, ty: &Ty, span: &Span) -> bool {
        let resolved = self.subst.resolve(ty);
        match &resolved {
            Ty::Named { name, args } => {
                if name == "HashSet" && args.len() == 1 {
                    return self.validate_hashset_element_type(&args[0], span);
                }
                args.iter()
                    .all(|arg| self.validate_concrete_hashset_type(arg, span))
            }
            Ty::Tuple(elems) => elems
                .iter()
                .all(|elem| self.validate_concrete_hashset_type(elem, span)),
            Ty::Array(elem, _) | Ty::Slice(elem) => self.validate_concrete_hashset_type(elem, span),
            Ty::Function { params, ret } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_hashset_type(param, span))
                    && self.validate_concrete_hashset_type(ret, span)
            }
            Ty::Closure {
                params,
                ret,
                captures,
            } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_hashset_type(param, span))
                    && self.validate_concrete_hashset_type(ret, span)
                    && captures
                        .iter()
                        .all(|capture| self.validate_concrete_hashset_type(capture, span))
            }
            Ty::Pointer { pointee, .. } => self.validate_concrete_hashset_type(pointee, span),
            Ty::TraitObject { traits } => traits.iter().all(|bound| {
                bound
                    .args
                    .iter()
                    .all(|arg| self.validate_concrete_hashset_type(arg, span))
            }),
            _ => true,
        }
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
        let resolved = self.subst.resolve(ty);
        match &resolved {
            Ty::Named { name, args } => {
                if name == "HashMap" && args.len() == 2 {
                    return self.validate_hashmap_key_value_types(&args[0], &args[1], span);
                }
                args.iter()
                    .all(|arg| self.validate_concrete_hashmap_type(arg, span))
            }
            Ty::Tuple(elems) => elems
                .iter()
                .all(|elem| self.validate_concrete_hashmap_type(elem, span)),
            Ty::Array(elem, _) | Ty::Slice(elem) => self.validate_concrete_hashmap_type(elem, span),
            Ty::Function { params, ret } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_hashmap_type(param, span))
                    && self.validate_concrete_hashmap_type(ret, span)
            }
            Ty::Closure {
                params,
                ret,
                captures,
            } => {
                params
                    .iter()
                    .all(|param| self.validate_concrete_hashmap_type(param, span))
                    && self.validate_concrete_hashmap_type(ret, span)
                    && captures
                        .iter()
                        .all(|capture| self.validate_concrete_hashmap_type(capture, span))
            }
            Ty::Pointer { pointee, .. } => self.validate_concrete_hashmap_type(pointee, span),
            Ty::TraitObject { traits } => traits.iter().all(|bound| {
                bound
                    .args
                    .iter()
                    .all(|arg| self.validate_concrete_hashmap_type(arg, span))
            }),
            _ => true,
        }
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
    fn signature_uses_unsupported_type_flags_error_anywhere_in_signature() {
        let params = vec![Ty::I32];
        let ret = Ty::Function {
            params: vec![Ty::Tuple(vec![Ty::Error])],
            ret: Box::new(Ty::Bool),
        };

        assert!(signature_uses_unsupported_type(&params, &ret));
        assert!(!signature_uses_unsupported_type(&[Ty::I32], &Ty::Bool));
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
}
