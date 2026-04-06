use super::coerce::cast_is_valid;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::builtin_names::builtin_named_type;

impl Checker {
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
        let ty = self.resolve_type_expr_tracking_holes(&annotation.0, &mut hole_vars);
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
        });
    }

    pub(super) fn resolve_annotation_with_holes(
        &mut self,
        annotation: &Spanned<TypeExpr>,
        context: impl Into<String>,
    ) -> Ty {
        let (ty, hole_vars) = self.resolve_annotation_holes(annotation);
        self.record_deferred_inference_holes(annotation, context, hole_vars);
        ty
    }

    pub(super) fn inference_holes_still_unresolved(&self, hole_vars: &[TypeVar]) -> bool {
        hole_vars
            .iter()
            .map(|var| self.subst.resolve(&Ty::Var(*var)))
            .any(|ty| ty_has_unresolved_inference_var(&ty))
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
            .map(|hole| TypeError::inference_failed(hole.span.clone(), &hole.context))
            .collect();
        self.errors.extend(deferred_errors);

        let deferred_cast_errors: Vec<_> = std::mem::take(&mut self.deferred_cast_checks)
            .into_iter()
            .filter(|check| !self.inference_holes_still_unresolved(&check.target_hole_vars))
            .filter_map(|check| {
                let actual = self.subst.resolve(&check.actual);
                let target = self.subst.resolve(&check.target);
                (!ty_has_unresolved_inference_var(&actual)
                    && !ty_has_unresolved_inference_var(&target)
                    && !cast_is_valid(&actual, &target))
                .then(|| {
                    TypeError::new(
                        TypeErrorKind::Mismatch {
                            expected: target.to_string(),
                            actual: actual.to_string(),
                        },
                        check.span.clone(),
                        format!("cannot cast `{actual}` to `{target}`"),
                    )
                })
            })
            .collect();
        self.errors.extend(deferred_cast_errors);
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
                        if self.fn_sig_inference_holes.get(&function.name).is_some_and(
                            |hole_vars| self.inference_holes_still_unresolved(hole_vars),
                        ) {
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
                _ => {}
            }
        }
    }

    /// Default unconstrained Range type variables to i64.  When both range
    /// bounds are coercible integer literals (e.g. `0..10`) the type checker
    /// creates `Range<fresh_var>`.  If nothing constrains the variable it
    /// reaches the serializer unresolved.  Bind those to i64 so both the
    /// Range and any derived bindings (loop induction variable) resolve.
    pub(super) fn default_unconstrained_range_types(&mut self, expr_types: &HashMap<SpanKey, Ty>) {
        for ty in expr_types.values() {
            if let Ty::Named { name, args } = ty {
                if name == "Range" && args.len() == 1 {
                    if let Ty::Var(v) = &args[0] {
                        if self.subst.lookup(*v).is_none() {
                            self.subst.insert(*v, Ty::I64);
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
    /// with the correct width (e.g. `i32` instead of the synthesis default of
    /// `i64`).
    pub(super) fn apply_deferred_range_bound_types(
        &mut self,
        expr_types: &mut HashMap<SpanKey, Ty>,
    ) {
        for (span, var, maybe_value) in std::mem::take(&mut self.deferred_range_bounds) {
            let resolved = self.subst.resolve(&Ty::Var(var));
            // If still unresolved or non-integer, the i64 recorded by synthesize
            // is already correct — nothing to do.
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
            // Re-record the span with the resolved element type so the codegen
            // generates the bound constant with the correct integer width.
            expr_types.insert(SpanKey::from(&span), resolved);
        }
    }

    pub(super) fn resolve_type_expr(&mut self, te: &TypeExpr) -> Ty {
        let mut ignored_hole_vars = Vec::new();
        self.resolve_type_expr_tracking_holes(te, &mut ignored_hole_vars)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "generic instantiation requires many cases"
    )]
    pub(super) fn resolve_type_expr_tracking_holes(
        &mut self,
        te: &TypeExpr,
        hole_vars: &mut Vec<TypeVar>,
    ) -> Ty {
        match te {
            TypeExpr::Named { name, type_args } => {
                // Handle `Self` type
                if name == "Self" {
                    if let Some((self_type_name, self_type_args)) = &self.current_self_type {
                        return Ty::normalize_named(self_type_name.clone(), self_type_args.clone());
                    }
                }
                if let Some(alias_name) = name.strip_prefix("Self::") {
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
                // Check for primitive types first
                if let Some(prim) = Ty::from_name(name) {
                    return prim;
                }
                // Non-primitive: resolve generics, aliases, special types
                let args = type_args.as_ref().map_or(vec![], |ta| {
                    ta.iter()
                        .map(|(te, _)| self.resolve_type_expr_tracking_holes(te, hole_vars))
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
                // Handle special named types
                match name.as_str() {
                    // Deprecated alias: ActorStream<Y> is now Stream<Y>
                    "ActorStream" if args.len() == 1 => Ty::stream(args[0].clone()),
                    _ => {
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
                        let resolved_name = if self.type_defs.contains_key(name) {
                            name.clone()
                        } else {
                            match handle_matches.as_slice() {
                                [qualified] => (*qualified).clone(),
                                _ => name.clone(),
                            }
                        };
                        // Mark module as used when type name is module-qualified
                        if let Some((module, _)) = resolved_name.split_once('.') {
                            self.used_modules.borrow_mut().insert(module.to_string());
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
                        let locally_non_generic =
                            self.local_type_defs.contains(resolved_name.as_str())
                                && self
                                    .type_defs
                                    .get(resolved_name.as_str())
                                    .is_some_and(|td| td.type_params.is_empty());
                        let builtin_type = builtin_named_type(resolved_name.as_str());
                        let args = match builtin_type {
                            Some(kind)
                                if kind.is_channel_handle()
                                    && args.is_empty()
                                    && !locally_non_generic =>
                            {
                                vec![Ty::Var(TypeVar::fresh())]
                            }
                            _ => args,
                        };
                        Ty::normalize_named(resolved_name, args)
                    }
                }
            }
            TypeExpr::Result { ok, err } => Ty::result(
                self.resolve_type_expr_tracking_holes(&ok.0, hole_vars),
                self.resolve_type_expr_tracking_holes(&err.0, hole_vars),
            ),
            TypeExpr::Option(inner) => {
                Ty::option(self.resolve_type_expr_tracking_holes(&inner.0, hole_vars))
            }
            TypeExpr::Tuple(elems) if elems.is_empty() => Ty::Unit,
            TypeExpr::Tuple(elems) => Ty::Tuple(
                elems
                    .iter()
                    .map(|(te, _)| self.resolve_type_expr_tracking_holes(te, hole_vars))
                    .collect(),
            ),
            TypeExpr::Array { element, size } => Ty::Array(
                Box::new(self.resolve_type_expr_tracking_holes(&element.0, hole_vars)),
                *size,
            ),
            TypeExpr::Slice(inner) => Ty::Slice(Box::new(
                self.resolve_type_expr_tracking_holes(&inner.0, hole_vars),
            )),
            TypeExpr::Function {
                params,
                return_type,
            } => Ty::Function {
                params: params
                    .iter()
                    .map(|(te, _)| self.resolve_type_expr_tracking_holes(te, hole_vars))
                    .collect(),
                ret: Box::new(self.resolve_type_expr_tracking_holes(&return_type.0, hole_vars)),
            },
            TypeExpr::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(self.resolve_type_expr_tracking_holes(&pointee.0, hole_vars)),
            },
            TypeExpr::TraitObject(bounds) => {
                let traits = bounds
                    .iter()
                    .map(|bound| {
                        let args = bound.type_args.as_ref().map_or(vec![], |ta| {
                            ta.iter()
                                .map(|t| self.resolve_type_expr_tracking_holes(&t.0, hole_vars))
                                .collect()
                        });
                        crate::ty::TraitObjectBound {
                            trait_name: bound.name.clone(),
                            args,
                        }
                    })
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
