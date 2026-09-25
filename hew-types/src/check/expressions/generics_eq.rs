//! Checker methods grouped by responsibility: generics eq.
//! Split from `expressions.rs`: checker methods, part 1 of 5.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
use super::super::branch_join::BranchArmExit;
use super::super::coerce::{cast_is_valid, common_integer_type, common_numeric_type};
use super::super::types::GenericLambdaSig;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::check::types::{
    DeferredIsCheck, EqRequirement, GenericCallEdge, GenericCallee, GenericFnInstantiationSite,
    PendingInstantiation,
};
use crate::env::{PlaceConflict, PlacePath};
use crate::BuiltinType;
use std::collections::VecDeque;

impl Checker {
    pub(super) fn lambda_generic_schema_ty(
        ty: &Ty,
        generic_param_names: &HashMap<u32, String>,
    ) -> Ty {
        match ty {
            Ty::Var(v) => generic_param_names
                .get(&v.0)
                .map_or_else(|| ty.clone(), |name| Ty::param(name)),
            Ty::Named { head, args } => Ty::Named {
                head: *head,
                args: args
                    .iter()
                    .map(|arg| Self::lambda_generic_schema_ty(arg, generic_param_names))
                    .collect(),
            },
            Ty::Tuple(ts) => Ty::Tuple(
                ts.iter()
                    .map(|elem| Self::lambda_generic_schema_ty(elem, generic_param_names))
                    .collect(),
            ),
            Ty::Array(inner, n) => Ty::Array(
                Box::new(Self::lambda_generic_schema_ty(inner, generic_param_names)),
                *n,
            ),
            Ty::Slice(inner) => Ty::Slice(Box::new(Self::lambda_generic_schema_ty(
                inner,
                generic_param_names,
            ))),
            Ty::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(Self::lambda_generic_schema_ty(pointee, generic_param_names)),
            },
            Ty::Function {
                capabilities,
                params,
                ret,
            } => Ty::Function {
                capabilities: *capabilities,
                params: params
                    .iter()
                    .map(|param| Self::lambda_generic_schema_ty(param, generic_param_names))
                    .collect(),
                ret: Box::new(Self::lambda_generic_schema_ty(ret, generic_param_names)),
            },
            Ty::Closure {
                capabilities,
                params,
                ret,
                captures,
                identity,
            } => Ty::Closure {
                capabilities: *capabilities,
                params: params
                    .iter()
                    .map(|param| Self::lambda_generic_schema_ty(param, generic_param_names))
                    .collect(),
                ret: Box::new(Self::lambda_generic_schema_ty(ret, generic_param_names)),
                captures: captures
                    .iter()
                    .map(|capture| Self::lambda_generic_schema_ty(capture, generic_param_names))
                    .collect(),
                identity: identity.clone(),
            },
            Ty::TraitObject { traits } => Ty::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| crate::ty::TraitObjectBound {
                        trait_name: bound.trait_name.clone(),
                        args: bound
                            .args
                            .iter()
                            .map(|arg| Self::lambda_generic_schema_ty(arg, generic_param_names))
                            .collect(),
                        assoc_bindings: bound
                            .assoc_bindings
                            .iter()
                            .map(|(name, ty)| {
                                (
                                    name.clone(),
                                    Self::lambda_generic_schema_ty(ty, generic_param_names),
                                )
                            })
                            .collect(),
                    })
                    .collect(),
            },
            _ => ty.clone(),
        }
    }

    /// Attempt the function-tail Ok-coercion described in
    /// [`TypeCheckOutput::tail_ok_coercions`].
    ///
    /// `expected` must be the (already substitution-resolved) declared return
    /// type and `actual` the synthesized tail expression type. Returns
    /// `Some(expected.clone())` — the full `Result` type — when the tail is
    /// Ok-wrapped, recording the coercion at `span` for HIR lowering. Returns
    /// `None` when no coercion applies (expected is not `Result`, the tail
    /// already unifies with the full `Result`, or the tail does not unify with
    /// the `Ok` payload); the caller then runs its normal unify-and-diagnose
    /// path. Probes are snapshot-guarded so a failed trial unification leaves
    /// the substitution untouched.
    pub(super) fn try_tail_ok_coercion(
        &mut self,
        expected: &Ty,
        actual: &Ty,
        span: &Span,
    ) -> Option<Ty> {
        let (ok_ty, err_ty) = expected.as_result()?;
        let ok_ty = ok_ty.clone();
        let err_ty = err_ty.clone();

        // Probe 1 — does the tail already produce the FULL `Result<Ok, Err>`?
        // If so this is `fn f() -> Result<..> { g() }` where `g()` returns the
        // Result directly: no coercion, fall back to the normal path (which
        // re-unifies). Roll the probe back so it commits nothing.
        let snapshot = self.subst.snapshot();
        let full_result = Ty::result(ok_ty.clone(), err_ty.clone());
        let unifies_full = self.try_unify_with_owner_identity(&full_result, actual);
        self.subst.restore(snapshot);
        if unifies_full {
            return None;
        }

        // Probe 2 — does the tail produce the `Ok` payload? If so, Ok-wrap it.
        // Commit this unification (it is the path we take) so the tail
        // expression's recorded type and any inference variables settle against
        // the `Ok` payload.
        let snapshot = self.subst.snapshot();
        if self.try_unify_with_owner_identity(&ok_ty, actual) {
            self.record_suspension_obligations(&ok_ty, actual, span);
            self.tail_ok_coercions
                .insert(SpanKey::in_module(span, self.current_module_idx));
            // Return the full `Result` as this expression's check-against
            // result so the block / function-return type-check sees a satisfied
            // return. Do NOT overwrite the recorded type at `span` with the
            // `Result`: the tail and its inner expression (e.g. the `?`
            // expression) share this span, and HIR lowering reads the inner
            // `Ok`-payload type back at lowering time. `wrap_tail_ok` supplies
            // the outer `Result` type when it wraps the lowered value in
            // `Ok(..)`, so the recorded span type must stay the inner payload.
            return Some(expected.clone());
        }
        self.subst.restore(snapshot);
        None
    }

    /// If `expr` is a bare identifier bound (via an unannotated `let`) to a
    /// still-open literal-defaulting `TypeVar`, return that var.
    ///
    /// Only `infer_integer_literal_binding_type` creates this shape — it
    /// gives an unannotated `let n = 6;` its own `Ty::Var` (immediately
    /// unified with `IntLiteral`, but re-promotable later, same as any other
    /// literal-defaulting var) rather than the plain `Ty::IntLiteral` tag a
    /// bare literal expression carries. A range-bound identifier of this
    /// shape needs its OWN var promoted alongside the range's fresh element
    /// var — see the call site in `check_binary_op`'s Range arm.
    pub(super) fn coercible_identifier_binding_var(
        env: &crate::env::TypeEnv,
        expr: &Expr,
    ) -> Option<TypeVar> {
        let Expr::Ident(name) = expr else {
            return None;
        };
        match env.lookup_ref(name.name.as_str())?.ty {
            Ty::Var(v) => Some(v),
            _ => None,
        }
    }

    pub(super) fn reject_unbounded_generic_ordering(
        &mut self,
        op: BinaryOp,
        left_resolved: &Ty,
        right_resolved: &Ty,
        left_span: &Span,
        right_span: &Span,
    ) {
        if !matches!(
            op,
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual
        ) {
            return;
        }
        let Some(param_name) = self.same_current_type_param_name(left_resolved, right_resolved)
        else {
            return;
        };
        if self.type_param_carries_bound(&param_name, "PartialOrd") {
            return;
        }
        let span = Span {
            start: left_span.start,
            end: right_span.end,
        };
        self.report_error(
            TypeErrorKind::InvalidOperation,
            &span,
            format!("`{op}` requires type parameter `{param_name}` to be bounded by `PartialOrd`"),
        );
    }

    pub(super) fn same_current_type_param_name(&self, left: &Ty, right: &Ty) -> Option<String> {
        let left_name = self.current_type_param_name(left)?;
        let right_name = self.current_type_param_name(right)?;
        (left_name == right_name).then_some(left_name)
    }

    pub(super) fn current_type_param_name(&self, ty: &Ty) -> Option<String> {
        let Ty::Named {
            head:
                head @ (crate::TypeHead::Nominal(_)
                | crate::TypeHead::Param(_)
                | crate::TypeHead::Unresolved(_)),
            args,
            ..
        } = ty
        else {
            return None;
        };
        let name = head.registry_key();
        if !args.is_empty() {
            return None;
        }
        if self
            .current_type_param_bounds
            .iter()
            .rev()
            .any(|frame| frame.bounds.contains_key(name))
        {
            return Some(name.to_string());
        }
        let fn_name = self.current_function.as_ref()?;
        self.fn_sigs.get(fn_name).and_then(|sig| {
            sig.type_params
                .iter()
                .any(|param_name| param_name == name)
                .then(|| name.to_string())
        })
    }

    pub(in crate::check) fn current_type_param_names(&self) -> HashSet<String> {
        let mut names = HashSet::new();
        for frame in &self.current_type_param_bounds {
            names.extend(frame.bounds.keys().cloned());
        }
        if let Some(fn_name) = &self.current_function {
            if let Some(sig) = self.fn_sigs.get(fn_name) {
                names.extend(sig.type_params.iter().cloned());
            }
        }
        names
    }

    /// Like `current_type_param_names`, but carries each name's declared
    /// bounds instead of discarding them. A deferred check that replays
    /// admission after inference settles (`finalize_hashmap_admission`) needs
    /// the actual bounds to answer `type_param_has_marker_bound`; the
    /// original declaration scope is gone by then, so this is the one point
    /// that captures it.
    pub(in crate::check) fn current_type_param_bounds_map(&self) -> HashMap<String, Vec<String>> {
        let mut bounds: HashMap<String, Vec<String>> = HashMap::new();
        for frame in &self.current_type_param_bounds {
            for (name, param_bounds) in &frame.bounds {
                bounds
                    .entry(name.clone())
                    .or_insert_with(|| param_bounds.clone());
            }
        }
        if let Some(fn_name) = &self.current_function {
            if let Some(sig) = self.fn_sigs.get(fn_name) {
                for param_name in &sig.type_params {
                    bounds.entry(param_name.clone()).or_insert_with(|| {
                        sig.type_param_bounds
                            .get(param_name)
                            .cloned()
                            .unwrap_or_default()
                    });
                }
            }
        }
        bounds
    }

    /// Equality uses the selected Eq authority after declarations and inference
    /// settle. Ordinary numeric comparisons bypass this gate and retain IEEE
    /// float semantics; selecting aggregate Eq does not change ordering.
    pub(super) fn reject_record_comparison(
        &mut self,
        op: BinaryOp,
        left_resolved: &Ty,
        right_resolved: &Ty,
        left_span: &Span,
        right_span: &Span,
        expr_span: &Span,
    ) {
        if matches!(op, BinaryOp::Equal | BinaryOp::NotEqual) {
            // Preserve the exact top-level user-method dispatch route. Nested
            // user methods are selected recursively by TypeFactService.
            if let Ty::Named {
                head:
                    crate::TypeHead::Nominal(_)
                    | crate::TypeHead::Param(_)
                    | crate::TypeHead::Unresolved(_),
                ..
            } = left_resolved
            {
                if let Some((method, _)) =
                    self.trait_impl_method_declaration(left_resolved, "Eq", "eq")
                {
                    self.record_user_comparison_dispatch(
                        expr_span,
                        UserComparisonDispatch::Eq { method },
                    );
                    return;
                }
            }
            self.record_eq_requirement(left_resolved, expr_span);
            return;
        }
        if let Ty::Named {
            head:
                crate::TypeHead::Nominal(_) | crate::TypeHead::Param(_) | crate::TypeHead::Unresolved(_),
            ..
        } = left_resolved
        {
            if let Some((method, _)) =
                self.trait_impl_method_declaration(left_resolved, "Ord", "lt")
            {
                self.record_user_comparison_dispatch(
                    expr_span,
                    UserComparisonDispatch::Ord { method },
                );
                return;
            }
            if let Some((method, _)) =
                self.trait_impl_method_declaration(left_resolved, "PartialOrd", "lt")
            {
                self.record_user_comparison_dispatch(
                    expr_span,
                    UserComparisonDispatch::PartialOrd { method },
                );
                return;
            }
        }
        let Some(type_name) = [left_resolved, right_resolved].into_iter().find_map(|ty| {
            let aggregate = match ty {
                Ty::Tuple(_)
                | Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::Option | BuiltinType::Result),
                    ..
                } => true,
                Ty::Named { head, .. } => {
                    self.type_defs
                        .get(head.registry_key())
                        .is_some_and(|definition| {
                            matches!(
                                definition.kind,
                                TypeDefKind::Struct | TypeDefKind::Record | TypeDefKind::Enum
                            )
                        })
                }
                _ => false,
            };
            aggregate.then(|| ty.user_facing().to_string())
        }) else {
            return;
        };
        let span = left_span.start..right_span.end;
        if !self
            .registry
            .implements_marker(left_resolved, MarkerTrait::PartialOrd)
        {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &span,
                format!(
                    "`{op}` is not available for `{type_name}` because the type does not \
                     derive `PartialOrd`; provide a user `impl Ord` or `impl PartialOrd`"
                ),
            );
            return;
        }
        self.report_error(
            TypeErrorKind::DerivedOrdUnavailable {
                type_name: type_name.clone(),
            },
            &span,
            format!(
                "E_LIMIT_DERIVED_ORD: `{op}` has no derived ordering for `{type_name}` yet \
                 — provide `impl Ord for {type_name}` (or `impl PartialOrd`) with a `lt` method"
            ),
        );
    }

    /// Record that the binary expression at `span` must dispatch to a user
    /// trait impl rather than the compiler's structural comparison. See
    /// [`UserComparisonDispatch`].
    pub(super) fn record_user_comparison_dispatch(
        &mut self,
        span: &Span,
        dispatch: UserComparisonDispatch,
    ) {
        self.user_comparison_dispatch
            .insert(SpanKey::in_module(span, self.current_module_idx), dispatch);
    }

    /// True when `ty` still names one of `params`.
    ///
    /// Implemented by substituting every parameter for a type that cannot occur
    /// in a checked program (`Ty::Never`) and comparing: this reuses the one
    /// substitution traversal instead of adding a second walk that could drift
    /// out of sync with it as `Ty` grows variants.
    pub(in crate::check) fn ty_mentions_type_params(ty: &Ty, params: &[String]) -> bool {
        if params.is_empty() {
            return false;
        }
        let probe: HashMap<String, Ty> = params
            .iter()
            .cloned()
            .map(|param| (param, Ty::Never))
            .collect();
        ty.substitute_named_params_parallel(&probe) != *ty
    }

    /// Record an Eq demand in the existing instantiation obligation graph.
    /// Concrete demands are checked once declarations and inference settle;
    /// abstract demands are substituted at the graph's concrete call roots.
    pub(in crate::check) fn record_eq_requirement(&mut self, ty: &Ty, span: &Span) {
        let owner = self.current_function.clone();
        let params = owner
            .as_ref()
            .and_then(|key| self.fn_sigs.get(key))
            .map_or_else(Vec::new, |sig| sig.type_params.clone());
        let requirements = self.eq_requirements.entry(owner).or_default();
        if requirements.iter().any(|existing| {
            existing.ty == *ty
                && existing.span == *span
                && existing.source_module == self.current_module
        }) {
            return;
        }
        requirements.push(EqRequirement {
            ty: ty.clone(),
            owner_type_params: params,
            span: span.clone(),
            source_module: self.current_module.clone(),
        });
    }

    /// The single recording authority for a generic application.
    ///
    /// Every application shape — free function, module-qualified function,
    /// method, actor method, trait-impl method — funnels through
    /// `apply_instantiated_call_signature_with_assoc`, and that is the only
    /// caller of this function. Recording anywhere else would reintroduce the
    /// exact gap this closes: obligations discharged for direct calls only,
    /// while a method instantiation walked straight into codegen.
    ///
    /// Two independent sources pin the callee's parameters and BOTH are merged
    /// by name: the signature instantiation (method-level parameters) and the
    /// receiver's type arguments (impl-level parameters, which
    /// `lookup_named_method_sig` has already substituted out of the signature).
    pub(in crate::check) fn record_generic_application(
        &mut self,
        callee: GenericCallee<'_>,
        sig_type_params: &[String],
        sig_type_args: &[Ty],
        span: &Span,
    ) {
        // The one place method identity is joined into a `fn_sigs` key.
        let (callee_key, owner) = match callee {
            GenericCallee::Function { key } => (key.to_string(), None),
            GenericCallee::Method {
                type_name,
                method,
                owner_type_args,
            } => (
                format!("{type_name}::{method}"),
                Some((type_name, owner_type_args)),
            ),
        };
        let Some(declared_params) = self
            .fn_sigs
            .get(&callee_key)
            .map(|sig| sig.type_params.clone())
            .filter(|params| !params.is_empty())
        else {
            return;
        };
        let mut substitution: HashMap<String, Ty> = HashMap::new();
        if sig_type_params.len() == sig_type_args.len() {
            for (param, arg) in sig_type_params.iter().zip(sig_type_args) {
                substitution.insert(param.clone(), self.subst.resolve(arg));
            }
        }
        if let Some((owner_name, owner_args)) = owner {
            let owner_params = self
                .type_defs
                .get(owner_name)
                .map(|type_def| type_def.type_params.clone())
                .unwrap_or_default();
            if owner_params.len() == owner_args.len() {
                for (param, arg) in owner_params.iter().zip(owner_args) {
                    substitution
                        .entry(param.clone())
                        .or_insert_with(|| self.subst.resolve(arg));
                }
            }
        }
        // Nothing pinned means nothing to discharge; a partially pinned
        // application still records, and the walk refuses to decide any
        // obligation whose substituted form is still abstract.
        if !declared_params
            .iter()
            .any(|param| substitution.contains_key(param))
        {
            return;
        }
        let enclosing = self.current_function.clone();
        let enclosing_params = enclosing
            .as_ref()
            .and_then(|name| self.fn_sigs.get(name))
            .map_or_else(Vec::new, |sig| sig.type_params.clone());
        self.generic_fn_instantiation_sites
            .push(GenericFnInstantiationSite {
                caller: enclosing,
                caller_type_params: enclosing_params,
                callee: callee_key,
                substitution,
                span: span.clone(),
                source_module: self.current_module.clone(),
            });
    }

    /// Split the recorded applications into concrete roots and generic → generic
    /// edges.
    ///
    /// An application whose substitution still names the enclosing generic
    /// function's own parameters proves nothing on its own; it becomes an edge,
    /// reachable only once a concrete root pins those parameters.
    pub(super) fn partition_generic_instantiation_sites(
        &self,
        sites: Vec<GenericFnInstantiationSite>,
    ) -> (
        Vec<PendingInstantiation>,
        HashMap<String, Vec<GenericCallEdge>>,
    ) {
        let mut roots: Vec<PendingInstantiation> = Vec::new();
        let mut edges: HashMap<String, Vec<GenericCallEdge>> = HashMap::new();
        for site in sites {
            let substitution: HashMap<String, Ty> = site
                .substitution
                .iter()
                .map(|(param, ty)| {
                    (
                        param.clone(),
                        self.subst.resolve(ty).materialize_literal_defaults(),
                    )
                })
                .collect();
            let still_abstract = substitution
                .values()
                .any(|ty| Self::ty_mentions_type_params(ty, &site.caller_type_params));
            if still_abstract {
                if let Some(owner) = site.caller {
                    edges.entry(owner).or_default().push(GenericCallEdge {
                        callee: site.callee,
                        substitution,
                    });
                }
                continue;
            }
            roots.push(PendingInstantiation {
                chain: vec![site.callee.clone()],
                callee: site.callee,
                substitution,
                report_span: site.span,
                report_module: site.source_module,
                depth: 0,
            });
        }
        (roots, edges)
    }

    /// Build the diagnostic for one ineligible instantiation of a generic
    /// callee that requires Eq for `template`.
    pub(super) fn generic_structural_eq_instantiation_error(
        template: &Ty,
        concrete: &Ty,
        pending: &PendingInstantiation,
    ) -> crate::error::TypeError {
        let callee = &pending.callee;
        let mut err = crate::error::TypeError::new(
            TypeErrorKind::InvalidOperation,
            pending.report_span.clone(),
            format!(
                "`{callee}` requires Eq for `{}`; this instantiation `{}` has no selected Eq implementation",
                template.user_facing(),
                concrete.user_facing(),
            ),
        )
        .with_suggestion(format!(
            "instantiate `{callee}` with a type that supports Eq, or provide an Eq implementation"
        ));
        if let Some(module) = pending.report_module.clone() {
            err = err.with_source_module(module);
        }
        err
    }

    /// Fail-closed diagnostic for an instantiation chain that outruns the hop
    /// budget.
    ///
    /// Dropping the obligation here would hand the un-analysed instantiation to
    /// codegen — the very thing this pass exists to prevent — so the budget
    /// refuses the program and names the chain that hit it.
    pub(super) fn generic_structural_eq_depth_error(
        pending: &PendingInstantiation,
        budget: u32,
    ) -> crate::error::TypeError {
        let chain = pending.chain.join(" → ");
        let mut err = crate::error::TypeError::new(
            TypeErrorKind::InvalidOperation,
            pending.report_span.clone(),
            format!(
                "structural-equality obligations for this instantiation could not be \
                 discharged: the generic instantiation chain exceeded {budget} hops \
                 ({chain}). The checker refuses rather than hand an unanalysed \
                 instantiation to codegen.",
            ),
        )
        .with_suggestion(
            "break the generic call chain — give an intermediate function a concrete type \
             argument, or move the comparison to a non-generic helper"
                .to_string(),
        );
        if let Some(module) = pending.report_module.clone() {
            err = err.with_source_module(module);
        }
        err
    }

    pub(super) fn check_concrete_eq_requirements(
        &self,
        requirements: &HashMap<Option<String>, Vec<EqRequirement>>,
        service: &mut TypeFactService,
    ) -> Vec<crate::error::TypeError> {
        let mut new_errors = Vec::new();
        let mut demands: Vec<_> = requirements.values().flatten().collect();
        demands.sort_by_key(|demand| (&demand.source_module, demand.span.start, demand.span.end));
        for requirement in demands {
            let concrete = self
                .normalize_for_use(&requirement.ty)
                .materialize_literal_defaults();
            if concrete.contains_error()
                || concrete.has_inference_var()
                || concrete.contains_assoc_type()
                || Self::ty_mentions_type_params(&concrete, &requirement.owner_type_params)
            {
                continue;
            }
            if !Self::selected_eq_available(service, &concrete) {
                let mut error = crate::error::TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    requirement.span.clone(),
                    format!(
                        "`{}` has no selected Eq implementation for equality comparison",
                        concrete.user_facing()
                    ),
                );
                if let Some(module) = &requirement.source_module {
                    error = error.with_source_module(module.clone());
                }
                new_errors.push(error);
            }
        }
        new_errors
    }

    /// Discharge concrete comparisons and the generic Eq obligations reachable
    /// through the program's instantiation graph.
    ///
    /// The walk starts at applications whose substitution is concrete in the
    /// caller's terms and follows generic → generic call edges, so an obligation
    /// raised two hops down still lands on the concrete application the
    /// programmer wrote. Every demand uses the same selected Eq authority.
    pub(in crate::check) fn finalize_eq_requirements(&mut self) {
        // WHY a hop budget: polymorphic recursion (`fn f<T>() { g::<Vec<T>>() }`)
        // generates an unbounded instantiation chain. Exceeding it is reported,
        // never skipped — see `generic_structural_eq_depth_error`.
        const MAX_INSTANTIATION_DEPTH: u32 = 64;

        let mut requirements = std::mem::take(&mut self.eq_requirements);
        for requirement in requirements.values_mut().flatten() {
            requirement.ty = self
                .normalize_for_use(&requirement.ty)
                .materialize_literal_defaults();
        }
        let sites = std::mem::take(&mut self.generic_fn_instantiation_sites);
        if requirements.is_empty() {
            return;
        }

        let mut service = TypeFactService::new(self.type_fact_context(), BTreeMap::new());
        let mut new_errors = self.check_concrete_eq_requirements(&requirements, &mut service);
        requirements.retain(|_, demands| {
            demands.retain(|demand| {
                Self::ty_mentions_type_params(&demand.ty, &demand.owner_type_params)
            });
            !demands.is_empty()
        });
        if requirements.is_empty() {
            self.errors.extend(new_errors);
            return;
        }
        let (roots, edges) = self.partition_generic_instantiation_sites(sites);
        let mut seen: HashSet<(String, String, usize, Option<String>)> = HashSet::new();
        let mut work: VecDeque<PendingInstantiation> = roots.into();

        while let Some(pending) = work.pop_front() {
            // Span offsets are module-local, so two modules can produce the same
            // (callee, args, offset) triple for genuinely different sites; the
            // module completes the identity.
            if !seen.insert((
                pending.callee.clone(),
                Self::render_substitution(&pending.substitution),
                pending.report_span.start,
                pending.report_module.clone(),
            )) {
                continue;
            }
            if pending.depth > MAX_INSTANTIATION_DEPTH {
                new_errors.push(Self::generic_structural_eq_depth_error(
                    &pending,
                    MAX_INSTANTIATION_DEPTH,
                ));
                continue;
            }

            for requirement in requirements
                .get(&Some(pending.callee.clone()))
                .into_iter()
                .flatten()
            {
                // Substitute, then collapse any associated-type projection the
                // substitution just made resolvable (`Option<C::Item>` with
                // `C = IntBox` becomes `Option<i64>`). A projection that
                // survives collapse has an unresolved carrier: the instantiation
                // is not decidable here, so do not answer for it.
                let concrete = self.project_assoc_types(
                    &requirement
                        .ty
                        .substitute_named_params_parallel(&pending.substitution),
                );
                if concrete.contains_error()
                    || concrete.has_inference_var()
                    || concrete.contains_assoc_type()
                {
                    continue;
                }
                // A parameter no source pinned leaves the obligation abstract;
                // deciding it here would be guessing.
                if Self::ty_mentions_type_params(&concrete, &requirement.owner_type_params) {
                    continue;
                }
                if !Self::selected_eq_available(
                    &mut service,
                    &concrete.materialize_literal_defaults(),
                ) {
                    new_errors.push(Self::generic_structural_eq_instantiation_error(
                        &requirement.ty,
                        &concrete,
                        &pending,
                    ));
                }
            }

            for edge in edges.get(&pending.callee).into_iter().flatten() {
                let mut chain = pending.chain.clone();
                chain.push(edge.callee.clone());
                work.push_back(PendingInstantiation {
                    callee: edge.callee.clone(),
                    substitution: edge
                        .substitution
                        .iter()
                        .map(|(param, ty)| {
                            (
                                param.clone(),
                                ty.substitute_named_params_parallel(&pending.substitution),
                            )
                        })
                        .collect(),
                    report_span: pending.report_span.clone(),
                    report_module: pending.report_module.clone(),
                    depth: pending.depth + 1,
                    chain,
                });
            }
        }

        self.errors.extend(new_errors);
    }
}
