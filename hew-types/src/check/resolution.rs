use super::coerce::cast_is_valid;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

impl Checker {
    fn resolve_trait_object_bound(
        &mut self,
        bound: &hew_parser::ast::TraitBound,
        span: &Span,
        hole_vars: &mut Vec<TypeVar>,
    ) -> crate::ty::TraitObjectBound {
        let args = bound.type_args.as_ref().map_or(vec![], |ta| {
            ta.iter()
                .map(|t| self.resolve_type_expr_tracking_holes(t, hole_vars))
                .collect()
        });
        let mut assoc_bindings = Vec::with_capacity(bound.assoc_type_bindings.len());
        let mut seen_assoc = HashSet::new();
        for binding in &bound.assoc_type_bindings {
            let ty = self.resolve_type_expr_tracking_holes(&binding.ty, hole_vars);
            if !seen_assoc.insert(binding.name.clone()) {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "duplicate associated type binding `{}` in `dyn {}`",
                        binding.name, bound.name
                    ),
                );
                continue;
            }
            if ty.has_inference_var() {
                self.report_error(
                    TypeErrorKind::InferenceFailed,
                    &binding.ty.1,
                    format!(
                        "associated type binding `{}::{}` in a dyn trait object must be fully projected",
                        bound.name, binding.name
                    ),
                );
            }
            assoc_bindings.push((binding.name.clone(), ty));
        }
        assoc_bindings.sort_by(|a, b| a.0.cmp(&b.0));

        if let Some(associated_type_names) = self.trait_defs.get(&bound.name).map(|trait_info| {
            trait_info
                .associated_types
                .iter()
                .map(|assoc| assoc.name.clone())
                .collect::<Vec<_>>()
        }) {
            let declared_assoc: HashSet<&str> =
                associated_type_names.iter().map(String::as_str).collect();
            let missing: Vec<String> = associated_type_names
                .iter()
                .filter(|assoc_name| !seen_assoc.contains(*assoc_name))
                .cloned()
                .collect();
            if !missing.is_empty() {
                let already_reported = self.errors.iter().any(|err| {
                    err.span == span.clone()
                        && matches!(
                            &err.kind,
                            TypeErrorKind::MissingAssocTypeBinding { trait_name, missing: prev }
                                if trait_name == &bound.name && prev == &missing
                        )
                });
                if !already_reported {
                    self.report_error(
                        TypeErrorKind::MissingAssocTypeBinding {
                            trait_name: bound.name.clone(),
                            missing: missing.clone(),
                        },
                        span,
                        format!(
                            "`dyn {}` must bind associated type{} {}; write `dyn {}<{}>`",
                            bound.name,
                            if missing.len() == 1 { "" } else { "s" },
                            missing
                                .iter()
                                .map(|name| format!("`{name}`"))
                                .collect::<Vec<_>>()
                                .join(", "),
                            bound.name,
                            missing
                                .iter()
                                .map(|name| format!("{name} = ..."))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ),
                    );
                }
            }
            for (assoc_name, _) in &assoc_bindings {
                if !declared_assoc.contains(assoc_name.as_str()) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "trait `{}` has no associated type `{}` for dyn binding",
                            bound.name, assoc_name
                        ),
                    );
                }
            }
        }

        crate::ty::TraitObjectBound {
            trait_name: bound.name.clone(),
            args,
            assoc_bindings,
        }
    }

    pub(super) fn resolve_fn_sig(&self, sig: &FnSig) -> FnSig {
        FnSig {
            params: sig
                .params
                .iter()
                .map(|param| self.subst.resolve(param))
                .collect(),
            return_type: self.subst.resolve(&sig.return_type),
            ..sig.clone()
        }
    }

    pub(super) fn resolve_variant_def(&self, variant: &VariantDef) -> VariantDef {
        match variant {
            VariantDef::Unit => VariantDef::Unit,
            VariantDef::Tuple(fields) => VariantDef::Tuple(
                fields
                    .iter()
                    .map(|field| self.subst.resolve(field))
                    .collect(),
            ),
            VariantDef::Struct(fields) => VariantDef::Struct(
                fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), self.subst.resolve(ty)))
                    .collect(),
            ),
        }
    }

    pub(super) fn resolve_type_def(&self, type_def: &TypeDef) -> TypeDef {
        TypeDef {
            fields: type_def
                .fields
                .iter()
                .map(|(name, ty)| (name.clone(), self.subst.resolve(ty)))
                .collect(),
            variants: type_def
                .variants
                .iter()
                .map(|(name, variant)| (name.clone(), self.resolve_variant_def(variant)))
                .collect(),
            methods: type_def
                .methods
                .iter()
                .map(|(name, sig)| (name.clone(), self.resolve_fn_sig(sig)))
                .collect(),
            ..type_def.clone()
        }
    }

    pub(super) fn record_type_def_inference_holes(&mut self, name: &str, hole_vars: Vec<TypeVar>) {
        if !hole_vars.is_empty() {
            self.type_def_inference_holes
                .entry(name.to_string())
                .or_default()
                .extend(hole_vars);
        }
    }

    pub(super) fn record_fn_sig_inference_holes(&mut self, name: &str, hole_vars: Vec<TypeVar>) {
        if !hole_vars.is_empty() {
            self.fn_sig_inference_holes
                .entry(name.to_string())
                .or_default()
                .extend(hole_vars);
        }
    }

    pub(super) fn resolve_annotation_holes(
        &mut self,
        annotation: &Spanned<TypeExpr>,
    ) -> (Ty, Vec<TypeVar>) {
        let mut hole_vars = Vec::new();
        let ty = self.resolve_type_expr_tracking_holes(annotation, &mut hole_vars);
        self.validate_concrete_collection_types(&ty, &annotation.1);
        (ty, hole_vars)
    }

    pub(super) fn record_deferred_inference_holes(
        &mut self,
        annotation: &Spanned<TypeExpr>,
        context: impl Into<String>,
        hole_vars: Vec<TypeVar>,
    ) {
        if hole_vars.is_empty() {
            return;
        }
        self.deferred_inference_holes.push(DeferredInferenceHole {
            span: first_infer_span_in_type_expr(annotation).unwrap_or_else(|| annotation.1.clone()),
            context: context.into(),
            hole_vars,
            source_module: self.current_module.clone(),
        });
    }

    pub(super) fn record_deferred_cast_check(
        &mut self,
        span: &Span,
        actual: &Ty,
        target: &Ty,
        target_hole_vars: Vec<TypeVar>,
    ) {
        if target_hole_vars.is_empty() {
            return;
        }
        self.deferred_cast_checks.push(DeferredCastCheck {
            span: span.clone(),
            actual: actual.clone(),
            target: target.clone(),
            target_hole_vars,
            source_module: self.current_module.clone(),
        });
    }

    pub(super) fn record_deferred_monomorphic_site(
        &mut self,
        span: &Span,
        context: impl Into<String>,
        ty: &Ty,
        more_specific_hole_vars: Vec<TypeVar>,
    ) {
        self.deferred_monomorphic_sites
            .push(DeferredMonomorphicSite {
                span: span.clone(),
                context: context.into(),
                ty: ty.clone(),
                more_specific_hole_vars,
                source_module: self.current_module.clone(),
            });
    }

    pub(super) fn resolve_annotation_with_holes(
        &mut self,
        annotation: &Spanned<TypeExpr>,
        context: impl Into<String>,
    ) -> Ty {
        let (ty, hole_vars) = self.resolve_annotation_holes(annotation);
        // Capture the fresh hole vars before moving them into the deferred list.
        // We use this set to suppress redundant collection-admission deferral
        // below: if a collection type arg IS a fresh inference hole, the
        // deferred_inference_holes path is already the sole authority for any
        // InferenceFailed diagnostic at this annotation site.  Deferring
        // admission on top would produce a duplicate error at the same root cause.
        let hole_var_set: std::collections::HashSet<TypeVar> = hole_vars.iter().copied().collect();
        self.record_deferred_inference_holes(annotation, context, hole_vars);
        match &ty {
            Ty::Named {
                builtin: Some(crate::BuiltinType::Vec),
                args,
                ..
            } if args.len() == 1 => {
                let elem = self.subst.resolve(&args[0]);
                let mut unresolved_vars = HashSet::new();
                collect_unresolved_inference_vars(&elem, &mut unresolved_vars);
                let elem_is_hole = unresolved_vars.iter().any(|var| hole_var_set.contains(var));
                if !elem_is_hole {
                    self.validate_vec_element_type(&args[0], &annotation.1);
                }
            }
            Ty::Named {
                builtin: Some(crate::BuiltinType::HashSet),
                args,
                ..
            } if args.len() == 1 => {
                // Skip if element is a fresh inference hole — inference-holes
                // path is the authority; admission runs at method-call sites.
                let elem = self.subst.resolve(&args[0]);
                if !matches!(&elem, Ty::Var(v) if hole_var_set.contains(v)) {
                    self.validate_hashset_element_type(&args[0], &annotation.1);
                }
            }
            Ty::Named {
                builtin: Some(crate::BuiltinType::HashMap),
                args,
                ..
            } if args.len() == 2 => {
                // Skip if either key or value is a fresh inference hole.
                let key = self.subst.resolve(&args[0]);
                let val = self.subst.resolve(&args[1]);
                let key_is_hole = matches!(&key, Ty::Var(v) if hole_var_set.contains(v));
                let val_is_hole = matches!(&val, Ty::Var(v) if hole_var_set.contains(v));
                if !key_is_hole && !val_is_hole {
                    self.validate_hashmap_key_value_types(&args[0], &args[1], &annotation.1);
                }
            }
            _ => {}
        }
        ty
    }

    pub(super) fn inference_holes_still_unresolved(&self, hole_vars: &[TypeVar]) -> bool {
        hole_vars
            .iter()
            .map(|var| self.subst.resolve(&Ty::Var(*var)))
            .any(|ty| ty.has_inference_var())
    }

    pub(super) fn report_unresolved_inference_holes(&mut self, program: &Program) {
        if let Some(module_graph) = &program.module_graph {
            for module_id in &module_graph.topo_order {
                if *module_id == module_graph.root {
                    continue;
                }
                let Some(module) = module_graph.modules.get(module_id) else {
                    continue;
                };
                let module_name = module_id.path.join(".");
                self.report_unresolved_inference_in_items(
                    &module.items,
                    Some(module_name.as_str()),
                );
            }
        }

        self.report_unresolved_inference_in_items(&program.items, None);

        // Drain body-level deferred inference holes accumulated across ALL modules
        // (root and every non-root module body checked earlier in check_program).
        // Using `take` ensures each hole is reported exactly once even if
        // `report_unresolved_inference_holes` were called again on the same Checker.
        let deferred_errors: Vec<_> = std::mem::take(&mut self.deferred_inference_holes)
            .into_iter()
            .filter(|hole| self.inference_holes_still_unresolved(&hole.hole_vars))
            .map(|hole| {
                let mut err = TypeError::inference_failed(hole.span.clone(), &hole.context);
                err.source_module = hole.source_module;
                err
            })
            .collect();
        self.errors.extend(deferred_errors);

        let deferred_cast_errors: Vec<_> = std::mem::take(&mut self.deferred_cast_checks)
            .into_iter()
            .filter(|check| !self.inference_holes_still_unresolved(&check.target_hole_vars))
            .filter_map(|check| {
                let actual = self.subst.resolve(&check.actual);
                let target = self.subst.resolve(&check.target);
                (!actual.has_inference_var()
                    && !target.has_inference_var()
                    && !cast_is_valid(&actual, &target))
                .then(|| {
                    let mut err = TypeError::new(
                        TypeErrorKind::Mismatch {
                            expected: target.to_string(),
                            actual: actual.to_string(),
                        },
                        check.span.clone(),
                        format!("cannot cast `{actual}` to `{target}`"),
                    );
                    err.source_module = check.source_module;
                    err
                })
            })
            .collect();
        self.errors.extend(deferred_cast_errors);
    }

    pub(super) fn report_unresolved_monomorphic_sites(&mut self) {
        let site_errors: Vec<_> = std::mem::take(&mut self.deferred_monomorphic_sites)
            .into_iter()
            .filter_map(|site| {
                let resolved = self.subst.resolve(&site.ty);
                let mut unresolved_site_vars = HashSet::new();
                collect_unresolved_inference_vars(&resolved, &mut unresolved_site_vars);
                if unresolved_site_vars.is_empty() {
                    return None;
                }

                let mut unresolved_specific_vars = HashSet::new();
                for hole_var in site.more_specific_hole_vars {
                    let resolved_hole = self.subst.resolve(&Ty::Var(hole_var));
                    collect_unresolved_inference_vars(
                        &resolved_hole,
                        &mut unresolved_specific_vars,
                    );
                }

                if !unresolved_specific_vars.is_empty()
                    && unresolved_site_vars.is_subset(&unresolved_specific_vars)
                {
                    return None;
                }

                let mut err = TypeError::inference_failed(site.span.clone(), &site.context);
                err.source_module = site.source_module;
                Some(err)
            })
            .collect();
        self.errors.extend(site_errors);
    }

    #[expect(
        clippy::too_many_lines,
        reason = "this walks all function-like and type-like item forms"
    )]
    pub(super) fn report_unresolved_inference_in_items(
        &mut self,
        items: &[Spanned<Item>],
        module_name: Option<&str>,
    ) {
        // Snapshot error count so we can tag newly-emitted errors with the source
        // module after the walk.  All errors emitted inside this function for a
        // non-root module must point to that module's source file when rendered.
        let err_before = self.errors.len();

        for (item, span) in items {
            match item {
                Item::Function(fd) => {
                    if lookup_scoped_item(&self.fn_sig_inference_holes, module_name, &fd.name)
                        .is_some_and(|hole_vars| self.inference_holes_still_unresolved(hole_vars))
                    {
                        self.errors.push(TypeError::inference_failed(
                            span.clone(),
                            &format!("signature of function `{}`", fd.name),
                        ));
                    }
                }
                Item::TypeDecl(td) => {
                    if lookup_scoped_item(&self.type_def_inference_holes, module_name, &td.name)
                        .is_some_and(|hole_vars| self.inference_holes_still_unresolved(hole_vars))
                    {
                        self.errors.push(TypeError::inference_failed(
                            span.clone(),
                            &format!("type `{}`", td.name),
                        ));
                    }

                    for body_item in &td.body {
                        if let TypeBodyItem::Method(method) = body_item {
                            let method_name = format!("{}::{}", td.name, method.name);
                            if self.fn_sig_inference_holes.get(&method_name).is_some_and(
                                |hole_vars| self.inference_holes_still_unresolved(hole_vars),
                            ) {
                                self.errors.push(TypeError::inference_failed(
                                    span.clone(),
                                    &format!("signature of method `{method_name}`"),
                                ));
                            }
                        }
                    }
                }
                Item::TypeAlias(type_alias) => {
                    // Fail closed only for user-written root `_` holes; nested holes
                    // remain out of scope in this lane.
                    if !matches!(type_alias.ty.0, TypeExpr::Infer) {
                        continue;
                    }
                    if lookup_scoped_item(
                        &self.type_def_inference_holes,
                        module_name,
                        &type_alias.name,
                    )
                    .is_some_and(|hole_vars| self.inference_holes_still_unresolved(hole_vars))
                    {
                        self.errors.push(TypeError::inference_failed(
                            type_alias.ty.1.clone(),
                            &format!("type alias `{}`", type_alias.name),
                        ));
                    }
                }
                Item::Actor(ad) => {
                    if lookup_scoped_item(&self.type_def_inference_holes, module_name, &ad.name)
                        .is_some_and(|hole_vars| self.inference_holes_still_unresolved(hole_vars))
                    {
                        self.errors.push(TypeError::inference_failed(
                            span.clone(),
                            &format!("actor `{}`", ad.name),
                        ));
                    }

                    for method in &ad.methods {
                        let method_name = format!("{}::{}", ad.name, method.name);
                        if self
                            .fn_sig_inference_holes
                            .get(&method_name)
                            .is_some_and(|hole_vars| {
                                self.inference_holes_still_unresolved(hole_vars)
                            })
                        {
                            self.errors.push(TypeError::inference_failed(
                                span.clone(),
                                &format!("signature of method `{method_name}`"),
                            ));
                        }
                    }

                    for receive_fn in &ad.receive_fns {
                        let receive_name = format!("{}::{}", ad.name, receive_fn.name);
                        if self
                            .fn_sig_inference_holes
                            .get(&receive_name)
                            .is_some_and(|hole_vars| {
                                self.inference_holes_still_unresolved(hole_vars)
                            })
                        {
                            self.errors.push(TypeError::inference_failed(
                                receive_fn.span.clone(),
                                &format!("signature of receive function `{receive_name}`"),
                            ));
                        }
                    }
                }
                Item::Wire(wd) => {
                    if lookup_scoped_item(&self.type_def_inference_holes, module_name, &wd.name)
                        .is_some_and(|hole_vars| self.inference_holes_still_unresolved(hole_vars))
                    {
                        self.errors.push(TypeError::inference_failed(
                            span.clone(),
                            &format!("wire type `{}`", wd.name),
                        ));
                    }
                }
                Item::Machine(md) => {
                    let event_type_name = format!("{}Event", md.name);
                    if lookup_scoped_item(&self.type_def_inference_holes, module_name, &md.name)
                        .is_some_and(|hole_vars| self.inference_holes_still_unresolved(hole_vars))
                        || lookup_scoped_item(
                            &self.type_def_inference_holes,
                            module_name,
                            &event_type_name,
                        )
                        .is_some_and(|hole_vars| self.inference_holes_still_unresolved(hole_vars))
                    {
                        self.errors.push(TypeError::inference_failed(
                            span.clone(),
                            &format!("machine `{}`", md.name),
                        ));
                    }
                }
                Item::ExternBlock(eb) => {
                    for function in &eb.functions {
                        if lookup_scoped_item(
                            &self.fn_sig_inference_holes,
                            module_name,
                            &function.name,
                        )
                        .is_some_and(|hole_vars| self.inference_holes_still_unresolved(hole_vars))
                        {
                            let error_span = first_infer_span_in_extern_fn(function)
                                .unwrap_or_else(|| span.clone());
                            self.errors.push(TypeError::inference_failed(
                                error_span,
                                &format!("signature of extern function `{}`", function.name),
                            ));
                        }
                    }
                }
                Item::Impl(id) => {
                    if let TypeExpr::Named {
                        name: type_name, ..
                    } = &id.target_type.0
                    {
                        for method in &id.methods {
                            let method_name = format!("{type_name}::{}", method.name);
                            if self.fn_sig_inference_holes.get(&method_name).is_some_and(
                                |hole_vars| self.inference_holes_still_unresolved(hole_vars),
                            ) {
                                self.errors.push(TypeError::inference_failed(
                                    span.clone(),
                                    &format!("signature of method `{method_name}`"),
                                ));
                            }
                        }
                    }
                }
                Item::Trait(td) => {
                    for trait_item in &td.items {
                        if let TraitItem::Method(method) = trait_item {
                            let method_name = format!("{}::{}", td.name, method.name);
                            if self.fn_sig_inference_holes.get(&method_name).is_some_and(
                                |hole_vars| self.inference_holes_still_unresolved(hole_vars),
                            ) {
                                self.errors.push(TypeError::inference_failed(
                                    span.clone(),
                                    &format!("signature of trait method `{method_name}`"),
                                ));
                            }
                        }
                    }
                }
                // JUSTIFIED: these item kinds do not populate `fn_sig_inference_holes`
                // or `type_def_inference_holes` during registration, so there are no
                // unresolved holes to report here:
                //   • Import     – names are resolved before the type-checking pass.
                //   • Const      – expression-level holes are caught by
                //                  `report_unresolved_inference_holes`; consts carry no
                //                  separately-tracked signature holes.
                //   • Supervisor – supervisor declarations carry no typed parameters.
                //
                // This arm is intentionally exhaustive so that any future `Item` variant
                // added to the parser triggers a compile error here, forcing the author
                // to decide whether hole-reporting is needed.
                // Record declarations carry no function-signature holes (only
                // type-def inference holes, which `register_record_decl` records
                // via `record_type_def_inference_holes`).  No action needed here.
                Item::Import(_) | Item::Const(_) | Item::Supervisor(_) | Item::Record(_) => {}
            }
        }

        // Tag all newly-emitted errors with the source module for non-root modules.
        // This allows the CLI to route each error to the correct source file for display.
        if let Some(mod_name) = module_name {
            for e in &mut self.errors[err_before..] {
                if e.source_module.is_none() {
                    e.source_module = Some(mod_name.to_string());
                }
            }
        }
    }

    /// Default unconstrained Range type variables to i64. When both range
    /// bounds are coercible integer literals (e.g. `0..10`) the type checker
    /// creates `Range<fresh_var>`.  If nothing constrains the variable it
    /// reaches the serializer unresolved.  Bind those to i64 so both the
    /// Range and any derived bindings (loop induction variable) resolve.
    pub(super) fn default_unconstrained_range_types(&mut self, expr_types: &HashMap<SpanKey, Ty>) {
        for ty in expr_types.values() {
            if let Ty::Named {
                builtin: Some(crate::BuiltinType::Range),
                args,
                ..
            } = ty
            {
                if args.len() == 1 {
                    if let Ty::Var(v) = &args[0] {
                        if self.subst.lookup(*v).is_none() {
                            self.subst.insert(*v, &Ty::I64).expect(
                                "defaulting an unresolved numeric literal variable to i64 must stay acyclic",
                            );
                        }
                    }
                }
            }
        }
    }

    /// Re-record the spans of coercible range bounds with their concrete
    /// resolved element type.
    ///
    /// When both bounds of a range literal are coercible integer literals
    /// (e.g. `0..10`), `check_binary_op` creates a fresh `TypeVar` for the
    /// element type and pushes each bound's span + literal value to
    /// `deferred_range_bounds`.  After all inference and defaulting is
    /// complete (including `default_unconstrained_range_types`), this pass
    /// resolves each `TypeVar` to a concrete integer type, validates that the
    /// literal fits, and re-records the span so codegen generates the constant
    /// with the correct width (e.g. `i32` instead of the output-defaulted
    /// `i64`).
    pub(super) fn apply_deferred_range_bound_types(
        &mut self,
        expr_types: &mut HashMap<SpanKey, Ty>,
    ) {
        for (span, var, maybe_value, inner_span, module_idx) in
            std::mem::take(&mut self.deferred_range_bounds)
        {
            let resolved = self
                .subst
                .resolve(&Ty::Var(var))
                .materialize_literal_defaults();
            // If still unresolved or non-integer, leave the original recorded
            // type alone; later output materialization will fall back to the
            // canonical default if needed.
            if matches!(resolved, Ty::Var(_) | Ty::Error) || !resolved.is_integer() {
                continue;
            }
            // Validate the literal value fits in the resolved type before
            // re-recording.  Emit an error and skip re-recording on overflow.
            if let Some(value) = maybe_value {
                if value < 0 && !integer_type_info(&resolved).is_some_and(|i| i.signed) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &span,
                        format!(
                            "negative integer literal `{value}` cannot be used \
                             in a range of type `{}`",
                            resolved.user_facing()
                        ),
                    );
                    continue;
                }
                if !integer_fits_type(value, &resolved) {
                    let (lo, hi) = integer_type_range(&resolved).unwrap_or((0, 0));
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &span,
                        format!(
                            "integer literal `{value}` does not fit in `{}` \
                             (range {lo}..={hi})",
                            resolved.user_facing()
                        ),
                    );
                    continue;
                }
            }
            // Re-record the outer span with the resolved element type so the
            // codegen generates the bound constant with the correct integer
            // width.
            expr_types.insert(SpanKey::in_module(&span, module_idx), resolved.clone());
            // For negated integer literals (`-5`), also re-record the inner
            // literal's span.  Without this, the inner `5` keeps the
            // `IntLiteral`→`I64` materialized default, while the outer `-5`
            // is narrowed to (e.g.) `I32`.  HIR's `lower_unary_expr` then sees
            // `operand_ty = I64` vs `result_ty = I32` and the `MIR lower`
            // `IntNegChecked` check rejects them as mismatched.
            if let Some(inner) = inner_span {
                expr_types.insert(SpanKey::in_module(&inner, module_idx), resolved);
            }
        }
    }

    /// Resolve a `T::Bar` projection where `T` is a generic type parameter
    /// in the current scope. Returns `Some(Ty)` if `T::Bar` is well-formed
    /// (carrier `Ty::AssocType` or eagerly-collapsed concrete type) and
    /// emits a typed diagnostic on failure.
    ///
    /// Resolution algorithm:
    /// 1. Look up `base_name` in `current_type_param_bounds` (top frame) and
    ///    in the current function's `fn_sigs.type_param_bounds` (for the
    ///    body-check fallback). If neither has bounds for the name, emit
    ///    a `MissingBound` diagnostic.
    /// 2. For each direct trait bound, consult `trait_defs[trait].associated_types`
    ///    and collect every trait that declares `assoc_name`.
    /// 3. Zero matches → `MissingAssocType` diagnostic. More than one match
    ///    → `AmbiguousAssocType` diagnostic citing both trait names.
    /// 4. Exactly one match → return
    ///    `Ty::AssocType { base: Ty::Named { name: T, args: [] }, trait_name, assoc_name }`.
    ///
    /// Returns `None` when `base_name` is not a type parameter in scope at
    /// all (so the caller falls through to other resolution paths, e.g. an
    /// unrelated `module.Submodule::Item`).
    pub(super) fn try_resolve_assoc_projection(
        &mut self,
        base_name: &str,
        assoc_name: &str,
        span: &Span,
    ) -> Option<Ty> {
        // Is `base_name` a known generic type param in the current scope?
        // Consult both the registration-time bounds stack and the body-time
        // fn_sigs fallback.
        if !self.is_type_param_in_scope(base_name) {
            return None;
        }
        let bounds = self.lookup_type_param_bounds(base_name).unwrap_or_default();
        if bounds.is_empty() {
            // Type param is in scope but has no bounds declared. Emit a
            // typed diagnostic naming the missing bound surface.
            self.report_error(
                TypeErrorKind::UndefinedType,
                span,
                format!(
                    "associated-type projection `{base_name}::{assoc_name}` requires \
                     a trait bound on `{base_name}` that declares `{assoc_name}`; \
                     no bounds are declared on `{base_name}`"
                ),
            );
            return Some(Ty::Error);
        }
        // Walk each direct bound, scanning that trait's associated types.
        // Super-trait projections are deferred per the precursor plan.
        let mut matches: Vec<String> = Vec::new();
        for trait_name in &bounds {
            if let Some(info) = self.trait_defs.get(trait_name) {
                if info
                    .associated_types
                    .iter()
                    .any(|assoc| assoc.name == assoc_name)
                {
                    matches.push(trait_name.clone());
                }
            }
        }
        match matches.len() {
            0 => {
                let bound_list = bounds.join(", ");
                self.report_error(
                    TypeErrorKind::UndefinedType,
                    span,
                    format!(
                        "no trait bound on `{base_name}` declares associated type \
                         `{assoc_name}`; bounds in scope: [{bound_list}]"
                    ),
                );
                Some(Ty::Error)
            }
            1 => {
                let trait_name = matches.into_iter().next().expect("len==1");
                Some(Ty::AssocType {
                    base: Box::new(Ty::Named {
                        builtin: None,
                        name: base_name.to_string(),
                        args: vec![],
                    }),
                    trait_name: trait_name.into_boxed_str(),
                    assoc_name: assoc_name.to_string().into_boxed_str(),
                })
            }
            _ => {
                let cites = matches.join(", ");
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "ambiguous associated-type projection `{base_name}::{assoc_name}`: \
                         declared by multiple bounds on `{base_name}`: [{cites}]. \
                         Same-name binding syntax `<Trait::Item>` is not yet supported"
                    ),
                );
                Some(Ty::Error)
            }
        }
    }

    /// Look up the bound list for `param_name` in the resolver's scope:
    /// the top of `current_type_param_bounds` (registration-time signature
    /// resolution) or, failing that, the current function's `fn_sigs` entry
    /// (body-check fallback). Returns `None` when the name is unbound.
    fn lookup_type_param_bounds(&self, param_name: &str) -> Option<Vec<String>> {
        // Walk the bounds stack from top to bottom so the innermost binding
        // wins (mirrors `generic_ctx` lookup semantics).
        for frame in self.current_type_param_bounds.iter().rev() {
            if let Some(bounds) = frame.bounds.get(param_name) {
                if !bounds.is_empty() {
                    return Some(bounds.clone());
                }
                return Some(vec![]);
            }
        }
        if let Some(fn_name) = self.current_function.as_ref() {
            if let Some(sig) = self.fn_sigs.get(fn_name) {
                if sig.type_params.iter().any(|p| p == param_name) {
                    return Some(
                        sig.type_param_bounds
                            .get(param_name)
                            .cloned()
                            .unwrap_or_default(),
                    );
                }
            }
        }
        None
    }

    fn lookup_type_param_assoc_binding(
        &self,
        param_name: &str,
        trait_name: &str,
        assoc_name: &str,
    ) -> Option<Ty> {
        let key = (
            param_name.to_string(),
            trait_name.to_string(),
            assoc_name.to_string(),
        );
        for frame in self.current_type_param_bounds.iter().rev() {
            if let Some(binding) = frame.assoc_bindings.get(&key) {
                return Some(binding.clone());
            }
        }
        if let Some(fn_name) = self.current_function.as_ref() {
            return self
                .fn_type_param_assoc_bindings
                .get(fn_name)
                .and_then(|bindings| bindings.get(&key))
                .cloned();
        }
        None
    }

    /// True if `name` is a type parameter declared in the current scope —
    /// either the resolver's bounds stack (even with no bounds) or the
    /// current function's signature.
    fn is_type_param_in_scope(&self, name: &str) -> bool {
        for frame in self.current_type_param_bounds.iter().rev() {
            if frame.bounds.contains_key(name) {
                return true;
            }
        }
        if let Some(fn_name) = self.current_function.as_ref() {
            if let Some(sig) = self.fn_sigs.get(fn_name) {
                if sig.type_params.iter().any(|p| p == name) {
                    return true;
                }
            }
        }
        false
    }

    /// Walk a `Ty` and collapse `Ty::AssocType { base, trait_name, assoc_name }`
    /// carriers whose `base` resolves to either an in-scope type parameter
    /// with a where-clause associated-type binding or a concrete
    /// `Ty::Named { name, .. }` for which an impl is registered. Carriers whose
    /// base is still abstract and unconstrained (a generic type param without
    /// a matching associated-type binding, an unresolved inference variable,
    /// another `Ty::AssocType`, etc.) pass through unchanged so they can be
    /// diagnosed or collapsed later when more substitutions arrive.
    ///
    /// Lookup goes through `impl_assoc_type_bindings`, populated at impl-
    /// registration. When no binding exists for `(base_type, trait, assoc)`,
    /// the carrier passes through — `enforce_type_param_bounds` is the
    /// authoritative diagnostic surface for "type does not implement trait".
    pub(super) fn project_assoc_types(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::AssocType {
                base,
                trait_name,
                assoc_name,
            } => {
                let projected_base = self.project_assoc_types(base);
                // Resolve via the substitution so a `Ty::Var` bound to a
                // concrete type also collapses.
                let resolved_base = self.subst.resolve(&projected_base);
                if let Ty::Named { name, args, .. } = &resolved_base {
                    if args.is_empty() && self.is_type_param_in_scope(name) {
                        if let Some(binding) = self.lookup_type_param_assoc_binding(
                            name,
                            trait_name.as_ref(),
                            assoc_name.as_ref(),
                        ) {
                            return self.project_assoc_types(&binding);
                        }
                        return Ty::AssocType {
                            base: Box::new(projected_base),
                            trait_name: trait_name.clone(),
                            assoc_name: assoc_name.clone(),
                        };
                    }
                    let key = (name.clone(), trait_name.to_string(), assoc_name.to_string());
                    if let Some(binding) = self.impl_assoc_type_bindings.get(&key) {
                        // The binding may itself reference impl-level type
                        // params; substitute them using the impl's declared
                        // type params (from `type_defs[name].type_params` if
                        // available) zipped with `args`. The fallback when
                        // `type_params` is absent is to return the binding
                        // unchanged — sound when the impl is non-generic.
                        let bound = if let Some(td) = self.type_defs.get(name) {
                            let map: HashMap<String, Ty> = td
                                .type_params
                                .iter()
                                .zip(args.iter())
                                .map(|(p, a)| (p.clone(), a.clone()))
                                .collect();
                            binding.substitute_named_params_parallel(&map)
                        } else if let Some(type_params) = builtin_generic_type_params(name) {
                            let map: HashMap<String, Ty> = type_params
                                .iter()
                                .zip(args.iter())
                                .map(|(p, a)| ((*p).to_string(), a.clone()))
                                .collect();
                            binding.substitute_named_params_parallel(&map)
                        } else {
                            binding.clone()
                        };
                        // Recurse: the projected binding may itself contain
                        // further `Ty::AssocType` carriers.
                        return self.project_assoc_types(&bound);
                    }
                }
                Ty::AssocType {
                    base: Box::new(projected_base),
                    trait_name: trait_name.clone(),
                    assoc_name: assoc_name.clone(),
                }
            }
            _ => ty.map_children_pub(&|child| self.project_assoc_types(child)),
        }
    }

    pub(super) fn resolve_type_expr(&mut self, te: &Spanned<TypeExpr>) -> Ty {
        let mut ignored_hole_vars = Vec::new();
        let ty = self.resolve_type_expr_tracking_holes(te, &mut ignored_hole_vars);
        // Type-annotation-position bound enforcement, applied to the
        // resolved type tree as a whole — not just the outermost
        // `Ty::Named`. A user-written annotation like
        // `Box<Holder<Plain>>`, `Option<Holder<Plain>>`,
        // `(Holder<Plain>, i64)`, `[Holder<Plain>; 4]`, or
        // `fn(Holder<Plain>) -> i64` carries a machine instantiation
        // at a nested position; the bound on `Holder<T: Resource>`
        // must reject `T = Plain` regardless of how deeply nested the
        // `Holder<Plain>` slot is. Walk the resolved tree and route
        // every nested `Ty::Named { name ∈ machine, args: non-empty }`
        // through the canonical helper. Duplicate emission across
        // overlapping resolution sweeps (signature registration vs.
        // body checking) and within a multi-occurrence annotation
        // (e.g. `(Holder<Plain>, Holder<Plain>)`) is suppressed at
        // the helper itself via `reported_machine_bound_violations`,
        // keyed on `(machine, resolved_args, span)`. The helper
        // short-circuits cleanly for non-machine names and empty
        // `args`, so the walk's no-op cost on bound-free annotations
        // is one hashmap lookup per `Named` node.
        self.enforce_machine_bounds_recursive(&ty, &te.1);
        ty
    }

    /// Recursively walk a resolved `Ty` and call
    /// `enforce_machine_instantiation_bounds` on every nested
    /// machine-named generic instantiation.
    ///
    /// The walker covers every composite `Ty` variant: tuples,
    /// arrays, slices, pointers, function/closure signatures,
    /// trait-object bounds (both their type-args and assoc-type
    /// bindings), associated-type carriers, and the `Task<T>`
    /// compiler-internal wrapper. Non-composite leaves return
    /// without work.
    ///
    /// Dedup of identical `(machine, args, span)` triples lives in
    /// the helper itself (Checker-level
    /// `reported_machine_bound_violations` set) so it survives
    /// re-walks across the registration / body-check sweeps; the
    /// walker therefore visits every nested position unconditionally
    /// and relies on the helper to drop duplicates.
    fn enforce_machine_bounds_recursive(&mut self, ty: &Ty, span: &Span) {
        match ty {
            Ty::Named { name, args, .. } => {
                if !args.is_empty() {
                    self.enforce_machine_instantiation_bounds(name, args, span);
                    for arg in args {
                        self.enforce_machine_bounds_recursive(arg, span);
                    }
                }
            }
            Ty::Tuple(elems) => {
                for elem in elems {
                    self.enforce_machine_bounds_recursive(elem, span);
                }
            }
            Ty::Array(inner, _) | Ty::Slice(inner) | Ty::Task(inner) => {
                self.enforce_machine_bounds_recursive(inner, span);
            }
            Ty::Pointer { pointee, .. } | Ty::Borrow { pointee } => {
                self.enforce_machine_bounds_recursive(pointee, span);
            }
            Ty::Function { params, ret } => {
                for param in params {
                    self.enforce_machine_bounds_recursive(param, span);
                }
                self.enforce_machine_bounds_recursive(ret, span);
            }
            Ty::Closure {
                params,
                ret,
                captures,
            } => {
                for param in params {
                    self.enforce_machine_bounds_recursive(param, span);
                }
                self.enforce_machine_bounds_recursive(ret, span);
                for capture in captures {
                    self.enforce_machine_bounds_recursive(capture, span);
                }
            }
            Ty::TraitObject { traits } => {
                for bound in traits {
                    for arg in &bound.args {
                        self.enforce_machine_bounds_recursive(arg, span);
                    }
                    for (_, bound_ty) in &bound.assoc_bindings {
                        self.enforce_machine_bounds_recursive(bound_ty, span);
                    }
                }
            }
            Ty::AssocType { base, .. } => {
                self.enforce_machine_bounds_recursive(base, span);
            }
            // Leaf / non-composite variants carry no nested type
            // structure that could embed a machine instantiation.
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
            | Ty::IntLiteral
            | Ty::FloatLiteral
            | Ty::Bool
            | Ty::Char
            | Ty::String
            | Ty::Bytes
            | Ty::CancellationToken
            | Ty::Duration
            | Ty::Unit
            | Ty::Never
            | Ty::Var(_)
            | Ty::Error => {}
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "generic instantiation requires many cases"
    )]
    pub(super) fn resolve_type_expr_tracking_holes(
        &mut self,
        te: &Spanned<TypeExpr>,
        hole_vars: &mut Vec<TypeVar>,
    ) -> Ty {
        match &te.0 {
            TypeExpr::Named { name, type_args } => {
                // Handle `Self` type
                if name == "Self" {
                    if let Some((self_type_name, self_type_args)) = &self.current_self_type {
                        return Ty::normalize_named(self_type_name.clone(), self_type_args.clone());
                    }
                }
                if let Some(alias_name) = name.strip_prefix("Self::") {
                    // No impl scope: in a trait method body (signature), the
                    // `Self::Bar` projection must materialise as a deferred
                    // `Ty::AssocType` so that downstream call sites (where
                    // `Self` substitutes to a concrete type or a type param
                    // with a bound) can collapse it. Look up the enclosing
                    // trait via `current_trait_for_self_projection`.
                    if self.impl_alias_scopes.is_empty() {
                        if let Some(trait_name) = self.current_trait_for_self_projection.clone() {
                            return Ty::AssocType {
                                base: Box::new(Ty::Named {
                                    builtin: None,
                                    name: "Self".to_string(),
                                    args: vec![],
                                }),
                                trait_name: trait_name.into_boxed_str(),
                                assoc_name: alias_name.to_string().into_boxed_str(),
                            };
                        }
                    }
                    if let Some(alias_ty) = self.resolve_impl_associated_type(alias_name) {
                        if type_args.as_ref().is_some_and(|args| !args.is_empty()) {
                            let should_report = self
                                .impl_alias_scopes
                                .last()
                                .is_some_and(|s| s.report_missing);
                            let err_span = self.impl_alias_scopes.last().map(|s| s.span.clone());
                            if should_report {
                                if let Some(sp) = err_span {
                                    self.report_error(
                                        TypeErrorKind::ArityMismatch,
                                        &sp,
                                        format!(
                                            "associated type `Self::{alias_name}` does not take type arguments"
                                        ),
                                    );
                                }
                            }
                        }
                        return alias_ty;
                    }
                }
                // REPRESENTATION: `T::Bar` projections — the parser stringifies
                // these as `Named { name: "T::Bar" }`. Split here, locate `T`
                // as a generic type param in the current scope, scan its
                // bounds for a trait that declares `Bar`, and materialise a
                // `Ty::AssocType` carrier that propagates through unification
                // until monomorphisation collapses it.
                //
                // Diagnostics: missing-bound (T not a known type param with
                // bounds), missing-assoc (no bound declares `Bar`), and
                // ambiguous (more than one bound declares `Bar`) are all
                // typed errors here. No silent pass-through.
                if !name.starts_with("Self::") {
                    if let Some((base_name, assoc_name)) = name.split_once("::") {
                        // Reject multi-segment projections (`T::Iter::Bar`)
                        // explicitly — deferred per the precursor plan.
                        if assoc_name.contains("::") {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &te.1,
                                format!(
                                    "multi-segment associated-type projections like `{name}` are not supported; \
                                     introduce an intermediate type parameter"
                                ),
                            );
                            return Ty::Error;
                        }
                        if let Some(ty) =
                            self.try_resolve_assoc_projection(base_name, assoc_name, &te.1)
                        {
                            return ty;
                        }
                    }
                }
                // Reject user-written `Task<T>` in any type-annotation position.
                //
                // `Task<T>` is a compiler-internal type produced exclusively by
                // HIR lowering of `fork name = expr`; it has no surface syntax.
                // Any occurrence of the name "Task" in a user-source type
                // annotation is an error. Emit `E_TASK_NOT_NAMEABLE` and
                // return `Ty::Error` so downstream checks don't cascade.
                if name == "Task" {
                    self.report_error(
                        TypeErrorKind::TaskNotNameable,
                        &te.1,
                        "Task<T> is a compiler-internal type and cannot be written in source. \
                         Use `fork name = expr` to create a task handle; the binding's type is \
                         inferred automatically."
                            .to_string(),
                    );
                    return Ty::Error;
                }
                // Removed aliases: suggest the replacement before the
                // general unknown-type path swallows the name.
                match name.as_str() {
                    "int" => {
                        // Lowercase `int` was never a valid v0.3/v0.4 user-facing
                        // spelling; treat it as an unknown type.
                        self.report_error(
                            TypeErrorKind::UndefinedType,
                            &te.1,
                            format!(
                                "unknown type `{name}`; use `i64` for fixed 64-bit integers \
                                 or `isize` for pointer-sized integers"
                            ),
                        );
                        return Ty::Error;
                    }
                    "Int" => {
                        // `Int` was the v0.3/v0.4 spelling for i64; it is no
                        // longer accepted.  Hard error pointing to the canonical
                        // name so migrated source uses `i64` directly.
                        self.report_error(
                            TypeErrorKind::UndefinedType,
                            &te.1,
                            "unknown type `Int`; use `i64` for fixed 64-bit integers \
                             or `isize` for pointer-sized integers"
                                .to_string(),
                        );
                        return Ty::Error;
                    }
                    "uint" => {
                        self.report_error(
                            TypeErrorKind::UndefinedType,
                            &te.1,
                            "unknown type `uint`; use `u64` for fixed 64-bit unsigned integers \
                             or `usize` for pointer-sized unsigned integers"
                                .to_string(),
                        );
                        return Ty::Error;
                    }
                    _ => {}
                }
                // Check for primitive types first
                if let Some(prim) = Ty::from_name(name) {
                    return prim;
                }
                // Non-primitive: resolve generics, aliases, special types
                let args = type_args.as_ref().map_or(vec![], |ta| {
                    ta.iter()
                        .map(|te| self.resolve_type_expr_tracking_holes(te, hole_vars))
                        .collect()
                });
                // Check if it's a generic type parameter
                for ctx in self.generic_ctx.iter().rev() {
                    if let Some(ty) = ctx.get(name) {
                        return ty.clone();
                    }
                }
                // Check type aliases (transparent: Distance = int → Ty::I64)
                if let Some(aliased) = self.type_aliases.get(name) {
                    return aliased.clone();
                }
                // Qualify unqualified handle types only when imported and unambiguous.
                let handle_matches: Vec<&String> = self
                    .known_types
                    .iter()
                    .filter(|qualified| {
                        qualified
                            .rsplit_once('.')
                            .is_some_and(|(_, short)| short == name)
                    })
                    .collect();
                // A bare reference binds to the current module's own type when the
                // name is locally defined (root program or the module being
                // checked). Imported types must be reached through their qualifier;
                // a bare `type_defs` hit on a non-local name is only the
                // last-write-wins residue of a cross-module same-name collision and
                // must not be treated as an unambiguous resolution.
                let is_local = self.local_type_defs.contains(name.as_str())
                    || self.source_type_defs.contains(name.as_str());
                // Distinct importing modules that export this bare name, per the
                // authoritative `module_type_exports` registry (populated on every
                // registration path: stdlib Pass 3, user-module, handle types).
                // `known_types` alone is insufficient — it carries qualified keys
                // only for the C-binding handle path, not pure-Hew user modules.
                let mut owner_modules: Vec<&str> = self
                    .module_type_exports
                    .iter()
                    .filter(|(_, exports)| exports.contains(name.as_str()))
                    .map(|(module, _)| module.as_str())
                    .collect();
                owner_modules.sort_unstable();
                owner_modules.dedup();
                if !is_local && owner_modules.len() > 1 {
                    // Unqualified name exported by more than one imported module.
                    // Fail closed with a typed ambiguity error naming the
                    // candidates, rather than silently first-winning a def.
                    let mut candidates: Vec<String> = owner_modules
                        .iter()
                        .map(|m| format!("{m}.{name}"))
                        .collect();
                    candidates.sort();
                    self.report_error_with_suggestions(
                        TypeErrorKind::AmbiguousType,
                        &te.1,
                        format!(
                            "ambiguous type `{name}`: exported by {} imported modules",
                            owner_modules.len()
                        ),
                        candidates
                            .iter()
                            .map(|c| format!("qualify the reference, e.g. `{c}`"))
                            .collect(),
                    );
                    return Ty::Error;
                }
                // Qualified-by-default: a bare reference to a type exported by
                // exactly one *plainly* imported module (no `::{ }` / glob /
                // alias opt-in) is not in scope. Fail closed with both the
                // qualifier and the explicit-opt-in import as suggestions,
                // rather than silently binding the source module's bare def.
                let published_bare = self
                    .unqualified_to_module
                    .contains_key(&(self.current_module.clone(), name.clone()));
                if !is_local && owner_modules.len() == 1 && !published_bare {
                    let owner = owner_modules[0];
                    self.report_error_with_suggestions(
                        TypeErrorKind::UndefinedType,
                        &te.1,
                        format!(
                            "type `{name}` is not in scope; it is exported by module `{owner}` \
                             but a plain `import` does not publish it unqualified"
                        ),
                        vec![
                            format!("qualify the reference, e.g. `{owner}.{name}`"),
                            format!("or opt in to the bare name: `import {owner}::{{ {name} }}`"),
                        ],
                    );
                    return Ty::Error;
                }
                let resolved_name = if is_local && self.type_defs.contains_key(name) {
                    name.clone()
                } else {
                    match handle_matches.as_slice() {
                        // Exactly one importing module exports this name: bind to
                        // its qualified def.
                        [qualified] => (*qualified).clone(),
                        // Zero qualified matches: keep the bare name (builtin,
                        // alias, or a downstream undefined-type diagnostic).
                        _ => name.clone(),
                    }
                };
                // Mark module as used when type name is module-qualified.
                if let Some((module, _)) = resolved_name.split_once('.') {
                    self.used_modules.borrow_mut().insert(ImportKey::new(
                        self.current_module.clone(),
                        module.to_string(),
                    ));
                } else if let Some(module) = self
                    .unqualified_to_module
                    .get(&(self.current_module.clone(), resolved_name.clone()))
                {
                    // A bare reference that resolves through a published binding
                    // (named / glob / aliased import) keeps the bare spelling but
                    // still consumes the owning module — mark it used so the
                    // unused-import lint does not false-positive on bare type
                    // references reached via an explicit opt-in or glob.
                    self.used_modules
                        .borrow_mut()
                        .insert(ImportKey::new(self.current_module.clone(), module.clone()));
                }
                // Auto-parameterise channel handle types: bare `Sender`
                // becomes `Sender<T>` with a fresh type variable so that
                // function parameters accept any channel element type.
                // Skip when the unqualified name is locally defined as
                // non-generic — standalone `hew check` on channel.hew
                // defines `type Sender {}` with zero type params, and
                // injecting fresh vars causes unification failures between
                // extern decls and impl bodies.  Only bare (unqualified)
                // names can match `local_type_defs`, so qualified imports
                // like `channel.Sender` are still auto-parameterised.
                let locally_non_generic = self.local_type_defs.contains(resolved_name.as_str())
                    && self
                        .type_defs
                        .get(resolved_name.as_str())
                        .is_some_and(|td| td.type_params.is_empty());
                let builtin_type = builtin_named_type(resolved_name.as_str());
                let args = match builtin_type {
                    Some(kind)
                        if kind.is_channel_handle() && args.is_empty() && !locally_non_generic =>
                    {
                        vec![Ty::Var(TypeVar::fresh())]
                    }
                    _ => args,
                };
                if crate::lookup_builtin_type(resolved_name.as_str())
                    == Some(crate::BuiltinType::Rc)
                {
                    if let Some(type_args) = type_args.as_ref() {
                        if let (Some(payload_ty), Some((_, payload_span))) =
                            (args.first(), type_args.first())
                        {
                            self.validate_rc_payload_type(payload_ty, payload_span);
                        }
                    }
                }
                let builtin = crate::lookup_builtin_type(resolved_name.as_str());
                // A bare name that shadows a builtin via a local `type X {}` decl
                // normally binds to the source decl (`builtin: None`). Collection
                // and substrate-handle builtins are the exception: their stdlib
                // modules declare zero-field carrier types (`type Sink<T> {}`,
                // `type Stream<T> {}`, `type Duplex<S, R> {}`, …) whose real type
                // identity is the compiler builtin. Keeping `builtin: Some(..)`
                // here is what carries the discriminator through HIR→MIR so the
                // MIR boundary recognises them as substrate handles rather than
                // emitting `unknown type` — same discriminator-survival invariant
                // as the collection types already excluded here.
                let builtin_overrides_source_decl =
                    builtin.is_some_and(|kind| kind.is_collection() || kind.is_substrate_handle());
                let local_source_type_def = self.source_type_defs.contains(resolved_name.as_str())
                    && !builtin_overrides_source_decl;
                if builtin.is_some() && !local_source_type_def {
                    Ty::normalize_named(resolved_name, args)
                } else {
                    Ty::named(resolved_name, args)
                }
            }
            TypeExpr::Result { ok, err } => Ty::result(
                self.resolve_type_expr_tracking_holes(ok, hole_vars),
                self.resolve_type_expr_tracking_holes(err, hole_vars),
            ),
            TypeExpr::Option(inner) => {
                Ty::option(self.resolve_type_expr_tracking_holes(inner, hole_vars))
            }
            TypeExpr::Tuple(elems) if elems.is_empty() => Ty::Unit,
            TypeExpr::Tuple(elems) => Ty::Tuple(
                elems
                    .iter()
                    .map(|te| self.resolve_type_expr_tracking_holes(te, hole_vars))
                    .collect(),
            ),
            TypeExpr::Array { element, size } => Ty::Array(
                Box::new(self.resolve_type_expr_tracking_holes(element, hole_vars)),
                *size,
            ),
            TypeExpr::Slice(_) => {
                if self
                    .unsupported_slice_spans
                    .insert(SpanKey::in_module(&te.1, self.current_module_idx))
                {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &te.1,
                        "`[T]` slice annotations are not supported; slice type composite lowering is not yet implemented"
                            .to_string(),
                    );
                }
                Ty::Error
            }
            TypeExpr::Function {
                params,
                return_type,
            } => Ty::Function {
                params: params
                    .iter()
                    .map(|te| self.resolve_type_expr_tracking_holes(te, hole_vars))
                    .collect(),
                ret: Box::new(self.resolve_type_expr_tracking_holes(return_type, hole_vars)),
            },
            TypeExpr::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(self.resolve_type_expr_tracking_holes(pointee, hole_vars)),
            },
            // `&T` immutable borrow — a first-class no-retain shared reference.
            TypeExpr::Borrow(inner) => Ty::Borrow {
                pointee: Box::new(self.resolve_type_expr_tracking_holes(inner, hole_vars)),
            },
            TypeExpr::TraitObject(bounds) => {
                let traits = bounds
                    .iter()
                    .map(|bound| self.resolve_trait_object_bound(bound, &te.1, hole_vars))
                    .collect();
                Ty::TraitObject { traits }
            }
            TypeExpr::Infer => {
                let var = TypeVar::fresh();
                hole_vars.push(var);
                Ty::Var(var)
            }
        }
    }
}

fn builtin_generic_type_params(name: &str) -> Option<&'static [&'static str]> {
    match name {
        "HashMap" => Some(&["K", "V"]),
        "Vec" | "HashSet" => Some(&["T"]),
        _ => None,
    }
}
