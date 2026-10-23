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
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::check::types::{
    DeferredIsCheck, GenericCallEdge, GenericCallee, GenericFnInstantiationSite,
    GenericRequirement, GenericRequirementKind, PendingInstantiation,
};
use crate::env::{PlaceConflict, PlacePath};
use crate::BuiltinType;
use std::collections::VecDeque;

type GenericInstantiation = (crate::DefId, BTreeMap<crate::ParamHead, Ty>);

impl PendingInstantiation {
    fn follow_call(&self, edge: &GenericCallEdge) -> Self {
        let mut chain = self.chain.clone();
        chain.push(edge.callee);
        Self {
            callee: edge.callee,
            substitution: edge
                .substitution
                .iter()
                .map(|(param, ty)| {
                    (
                        *param,
                        ty.substitute_type_params_parallel(&self.substitution),
                    )
                })
                .collect(),
            report_span: self.report_span.clone(),
            report_module: self.report_module.clone(),
            depth: self.depth + 1,
            chain,
        }
    }
}

impl Checker {
    fn generic_body_parameters(&self, declaration: crate::DefId) -> Vec<crate::ParamHead> {
        let mut parameters = self
            .fn_sigs
            .get(&declaration)
            .map_or_else(Vec::new, |sig| sig.type_params.clone());
        let actor = if self.defs.kind(declaration) == crate::DeclarationKind::Actor {
            Some(declaration)
        } else {
            self.defs
                .owner(declaration)
                .filter(|id| self.defs.kind(*id) == crate::DeclarationKind::Actor)
        };
        if let Some(actor) =
            actor.and_then(|id| self.type_defs.get(&crate::NominalId::of_declaration(id)))
        {
            for parameter in &actor.type_params {
                if !parameters.contains(parameter) {
                    parameters.push(*parameter);
                }
            }
        }
        parameters
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
        let Some(param) = Self::same_type_param(left_resolved, right_resolved) else {
            return;
        };
        if self.param_carries_trait(
            param.id,
            crate::DefTable::predicate(crate::Predicate::PartialOrd),
        ) {
            return;
        }
        let param_name = param.spelling;
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

    pub(super) fn same_type_param(left: &Ty, right: &Ty) -> Option<crate::ParamHead> {
        let left = Self::bare_param(left)?;
        let right = Self::bare_param(right)?;
        (left == right).then_some(left)
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
                if let Some(method) = self.impl_method_declaration_for_slot(
                    left_resolved,
                    crate::type_facts::ImplMethodSlot::Value(crate::ValueCapability::Eq),
                ) {
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
        let capability = match op {
            BinaryOp::Less => crate::ValueCapability::Less,
            BinaryOp::LessEqual => crate::ValueCapability::LessEqual,
            BinaryOp::Greater => crate::ValueCapability::Greater,
            BinaryOp::GreaterEqual => crate::ValueCapability::GreaterEqual,
            _ => return,
        };
        if let Ty::Named {
            head:
                crate::TypeHead::Nominal(_) | crate::TypeHead::Param(_) | crate::TypeHead::Unresolved(_),
            ..
        } = left_resolved
        {
            if let Some(method) = self.impl_method_declaration_for_slot(
                left_resolved,
                crate::type_facts::ImplMethodSlot::Value(capability),
            ) {
                self.record_user_comparison_dispatch(
                    expr_span,
                    UserComparisonDispatch::Ordering { method },
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
                Ty::Named { head, .. } => self.head_type_def(*head).is_some_and(|definition| {
                    matches!(
                        definition.kind,
                        TypeDefKind::Struct | TypeDefKind::Record | TypeDefKind::Enum
                    )
                }),
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
                    "`{op}` requires `PartialOrd` for `{type_name}`; its components must \
                     support ordering or the type must provide `impl Ord` or `impl PartialOrd`"
                ),
            );
            return;
        }
        let _ = (capability, type_name, span);
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
    pub(in crate::check) fn ty_mentions_type_params(ty: &Ty, params: &[crate::ParamHead]) -> bool {
        if params.is_empty() {
            return false;
        }
        let probe: HashMap<crate::ParamHead, Ty> = params
            .iter()
            .copied()
            .map(|param| (param, Ty::Never))
            .collect();
        ty.substitute_type_params_parallel(&probe) != *ty
    }

    /// Record an Eq demand in the existing instantiation obligation graph.
    /// Concrete demands are checked once declarations and inference settle;
    /// abstract demands are substituted at the graph's concrete call roots.
    pub(in crate::check) fn record_eq_requirement(&mut self, ty: &Ty, span: &Span) {
        self.record_generic_requirement(GenericRequirementKind::Eq, ty, span);
    }

    fn generic_obligation_owners(&self, types: &[&Ty]) -> Vec<crate::DefId> {
        let mut owners = self
            .fn_sigs
            .values()
            .flat_map(|signature| &signature.type_params)
            .filter(|parameter| {
                types
                    .iter()
                    .any(|ty| Self::ty_mentions_type_params(ty, &[**parameter]))
            })
            .map(|parameter| parameter.id.owner)
            .collect::<Vec<_>>();
        owners.sort_unstable();
        owners.dedup();
        owners
    }

    pub(in crate::check) fn record_generic_requirement(
        &mut self,
        kind: GenericRequirementKind,
        ty: &Ty,
        span: &Span,
    ) {
        let mut owner = self
            .current_function
            .as_ref()
            .and_then(|key| self.fn_sig_keys.get(key))
            .copied();
        if owner.is_some_and(|id| self.defs.kind(id) == crate::DeclarationKind::Closure) {
            let mentioned_owners = self.generic_obligation_owners(&[ty]);
            if mentioned_owners.len() > 1 && kind == GenericRequirementKind::Eq {
                if let Ok(resolved) = ResolvedTy::from_ty(ty) {
                    let service = TypeFactService::new(self.type_fact_context(), BTreeMap::new());
                    if crate::check::declaration_walk_terminates(
                        &resolved,
                        service.context().types(),
                    ) {
                        if let Ok(Some(members)) = service.derived_eq_members(&resolved) {
                            for member in members {
                                self.record_generic_requirement(
                                    kind.clone(),
                                    &member.to_ty(),
                                    span,
                                );
                            }
                            return;
                        }
                    }
                }
            }
            if let [declaration] = mentioned_owners.as_slice() {
                if self.fn_sigs.contains_key(declaration) {
                    owner = Some(*declaration);
                }
            }
        }
        let params = owner.map_or_else(Vec::new, |id| self.generic_body_parameters(id));
        let requirements = self.generic_requirements.entry(owner).or_default();
        if requirements.iter().any(|existing| {
            existing.kind == kind
                && existing.ty == *ty
                && existing.span == *span
                && existing.source_module == self.current_module
        }) {
            return;
        }
        requirements.push(GenericRequirement {
            kind,
            ty: ty.clone(),
            owner_type_params: params,
            span: span.clone(),
            source_module: self.current_module.clone(),
        });
    }

    /// The single recording authority for a generic application.
    ///
    /// Calls record their application through the shared instantiated-signature
    /// checker; taking a declaration as a value records the same application
    /// when its signature is instantiated. Both consume the body's demands
    /// through this graph, including method and actor applications.
    ///
    /// Two independent sources pin the callee's parameters and BOTH are merged
    /// by binder identity: the signature instantiation (method-level parameters) and the
    /// receiver's type arguments (impl-level parameters, which
    /// `lookup_named_method_sig` has already substituted out of the signature).
    pub(in crate::check) fn record_generic_application(
        &mut self,
        callee: GenericCallee<'_>,
        sig_type_params: &[crate::ParamHead],
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
            .fn_sig(&callee_key)
            .map(|sig| sig.type_params.clone())
            .filter(|params| !params.is_empty())
        else {
            return;
        };
        let mut substitution: HashMap<crate::ParamHead, Ty> = HashMap::new();
        if sig_type_params.len() == sig_type_args.len() {
            for (param, arg) in sig_type_params.iter().zip(sig_type_args) {
                substitution.insert(*param, self.subst.resolve(arg));
            }
        }
        if let Some((owner_name, owner_args)) = owner {
            let owner_substitution = if let Some(method) = self
                .fn_sig(&callee_key)
                .and_then(|sig| sig.impl_method.as_ref())
            {
                crate::method_resolution::impl_receiver_substitution(
                    &method.receiver_parameters,
                    &method.receiver_args,
                    owner_args,
                )
                .unwrap_or_default()
            } else {
                self.type_def_at(owner_name)
                    .into_iter()
                    .flat_map(|owner| {
                        owner
                            .type_params
                            .iter()
                            .copied()
                            .zip(owner_args.iter().cloned())
                    })
                    .collect()
            };
            substitution.extend(
                owner_substitution
                    .into_iter()
                    .map(|(parameter, ty)| (parameter, self.subst.resolve(&ty))),
            );
        }

        // Publish the same complete binder substitution to call consumers.
        // Instantiated method signatures have already removed impl parameters.
        if let Some(arguments) = declared_params
            .iter()
            .map(|parameter| substitution.get(parameter).cloned())
            .collect::<Option<Vec<_>>>()
        {
            self.record_concrete_call_type_args(span, &arguments);
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
        // Builtin signatures have no source body in this obligation graph.
        // Their bounds and call type arguments were handled above.
        let Some(&declaration) = self.fn_sig_keys.get(&callee_key) else {
            return;
        };
        self.record_generic_instantiation_site(
            self.current_function.clone().as_deref(),
            declaration,
            substitution,
            span,
        );
    }

    pub(in crate::check) fn record_generic_instantiation_site(
        &mut self,
        enclosing: Option<&str>,
        callee: crate::DefId,
        substitution: HashMap<crate::ParamHead, Ty>,
        span: &Span,
    ) {
        let enclosing = enclosing.and_then(|key| self.fn_sig_keys.get(key)).copied();
        let enclosing =
            if enclosing.is_some_and(|id| self.defs.kind(id) == crate::DeclarationKind::Closure) {
                match self
                    .generic_obligation_owners(&substitution.values().collect::<Vec<_>>())
                    .as_slice()
                {
                    [owner] if self.fn_sigs.contains_key(owner) => Some(*owner),
                    _ => enclosing,
                }
            } else {
                enclosing
            };
        let enclosing_params =
            enclosing.map_or_else(Vec::new, |id| self.generic_body_parameters(id));
        self.generic_fn_instantiation_sites
            .push(GenericFnInstantiationSite {
                caller: enclosing,
                caller_type_params: enclosing_params,
                callee,
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
        HashMap<crate::DefId, Vec<GenericCallEdge>>,
    ) {
        let mut roots: Vec<PendingInstantiation> = Vec::new();
        let mut edges: HashMap<crate::DefId, Vec<GenericCallEdge>> = HashMap::new();
        for site in sites {
            let substitution: HashMap<crate::ParamHead, Ty> = site
                .substitution
                .iter()
                .map(|(param, ty)| {
                    (
                        *param,
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
                chain: vec![site.callee],
                callee: site.callee,
                substitution,
                report_span: site.span,
                report_module: site.source_module,
                depth: 0,
            });
        }
        for (nominal, actor) in &self.type_defs {
            if actor.kind != TypeDefKind::Actor || actor.type_params.is_empty() {
                continue;
            }
            for (_, callback) in self
                .defs
                .declarations()
                .filter(|(_, callback)| self.defs.owner(*callback) == Some(nominal.declaration()))
            {
                edges
                    .entry(nominal.declaration())
                    .or_default()
                    .push(GenericCallEdge {
                        callee: callback,
                        substitution: actor
                            .type_params
                            .iter()
                            .map(|param| (*param, Ty::param(*param)))
                            .collect(),
                    });
            }
        }
        (roots, edges)
    }

    /// Build the diagnostic for one ineligible instantiation of a generic
    /// callee that requires Eq for `template`.
    pub(super) fn generic_structural_eq_instantiation_error(
        &self,
        template: &Ty,
        concrete: &Ty,
        pending: &PendingInstantiation,
    ) -> crate::error::TypeError {
        let callee = if matches!(
            self.defs.kind(pending.callee),
            crate::DeclarationKind::Closure | crate::DeclarationKind::CallableType
        ) {
            std::borrow::Cow::Borrowed("generic callable")
        } else {
            self.defs.callable_diagnostic_path(pending.callee)
        };
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
        &self,
        pending: &PendingInstantiation,
        budget: u32,
    ) -> crate::error::TypeError {
        let chain = pending
            .chain
            .iter()
            .map(|id| self.defs.callable_diagnostic_path(*id))
            .collect::<Vec<_>>()
            .join(" → ");
        let mut err = crate::error::TypeError::new(
            TypeErrorKind::InvalidOperation,
            pending.report_span.clone(),
            format!(
                "type obligations for this instantiation could not be \
                 discharged: the generic instantiation chain exceeded {budget} hops \
                 ({chain}). The checker refuses rather than hand an unanalysed \
                 instantiation to codegen.",
            ),
        )
        .with_suggestion(
            "break the generic call chain — give an intermediate function a concrete type \
             argument, or move the operation to a non-generic helper"
                .to_string(),
        );
        if let Some(module) = pending.report_module.clone() {
            err = err.with_source_module(module);
        }
        err
    }

    pub(super) fn check_concrete_generic_requirements(
        &mut self,
        requirements: &HashMap<Option<crate::DefId>, Vec<GenericRequirement>>,
        service: &mut TypeFactService,
        resource_roots: &mut Vec<PendingInstantiation>,
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
            if let Some(error) =
                self.generic_requirement_error(requirement, &concrete, service, None)
            {
                new_errors.push(error);
            }
            resource_roots.extend(self.resource_close_pending(requirement, &concrete, None));
        }
        new_errors
    }

    fn generic_requirement_error(
        &self,
        requirement: &GenericRequirement,
        concrete: &Ty,
        service: &mut TypeFactService,
        pending: Option<&PendingInstantiation>,
    ) -> Option<TypeError> {
        let span = pending.map_or(&requirement.span, |site| &site.report_span);
        let module = pending.map_or(&requirement.source_module, |site| &site.report_module);
        let mut error = match &requirement.kind {
            GenericRequirementKind::Eq if !Self::selected_eq_available(service, concrete) => {
                if let Some(pending) = pending {
                    return Some(self.generic_structural_eq_instantiation_error(
                        &requirement.ty, concrete, pending,
                    ));
                }
                TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    span.clone(),
                    format!("`{}` has no selected Eq implementation for equality comparison", concrete.user_facing()),
                )
            }
            GenericRequirementKind::BorrowedTransfer { root }
                if self.place_read_transfers_ownership(concrete) =>
            {
                TypeError::new(
                    TypeErrorKind::OwnConsumeBorrowed,
                    span.clone(),
                    format!(
                        "E_OWN_CONSUME_BORROWED: cannot consume `{}` through borrowed parameter `{root}` in this instantiation",
                        concrete.user_facing(),
                    ),
                ).with_suggestion(format!(
                    "declare the parameter `consume {root}: ...` before consuming it"
                ))
            }
            GenericRequirementKind::CaptureSnapshot { root } if !self.capture_is_cloneable(concrete) => {
                TypeError::new(TypeErrorKind::ClosureExplicitMoveRequired { name: root.clone(), ty: concrete.user_facing().to_string() }, span.clone(),
                    format!("capture `{root}` has no independent snapshot operation; use `move` to transfer it into the closure"))
            }
            GenericRequirementKind::Eq | GenericRequirementKind::BorrowedTransfer { .. } | GenericRequirementKind::CaptureSnapshot { .. } | GenericRequirementKind::ResourceClose => return None,
        };
        error.source_module.clone_from(module);
        Some(error)
    }

    fn resource_close_pending(
        &mut self,
        requirement: &GenericRequirement,
        concrete: &Ty,
        pending: Option<&PendingInstantiation>,
    ) -> Vec<PendingInstantiation> {
        if requirement.kind != GenericRequirementKind::ResourceClose {
            return Vec::new();
        }
        let span = pending.map_or(&requirement.span, |site| &site.report_span);
        let module = pending.map_or(&requirement.source_module, |site| &site.report_module);
        let mut instances = Vec::new();
        for (method, receiver, arguments) in self.resource_close_instances(concrete) {
            if receiver.has_type_parameters() {
                continue;
            }
            let Some(signature) = self.fn_sigs.get(&method).cloned() else {
                continue;
            };
            let errors = self.errors.len();
            let previous_module = std::mem::replace(&mut self.current_module, module.clone());
            self.enforce_signature_bounds(&signature, &arguments, span);
            self.current_module = previous_module;
            if self.errors.len() != errors {
                continue;
            }
            let Ok(receiver) = ResolvedTy::from_ty(&receiver) else {
                continue;
            };
            if let Some(binders) = self.impl_method_binders.get_mut(&method) {
                binders.checked_receivers.insert(receiver);
            }
            if signature.type_params.is_empty() {
                continue;
            }
            let callee = method;
            let mut chain = pending.map_or_else(Vec::new, |site| site.chain.clone());
            chain.push(callee);
            instances.push(PendingInstantiation {
                callee,
                substitution: signature.type_params.into_iter().zip(arguments).collect(),
                report_span: span.clone(),
                report_module: module.clone(),
                depth: pending.map_or(0, |site| site.depth + 1),
                chain,
            });
        }
        instances
    }

    fn instantiate_generic_requirement(
        &self,
        requirement: &GenericRequirement,
        substitution: &HashMap<crate::ParamHead, Ty>,
    ) -> Option<Ty> {
        // Substitution can resolve an associated projection. A surviving
        // projection, inference variable or unpinned parameter is undecidable.
        let concrete =
            self.project_assoc_types(&requirement.ty.substitute_type_params_parallel(substitution));
        (!concrete.contains_error()
            && !concrete.has_inference_var()
            && !concrete.contains_assoc_type()
            && !Self::ty_mentions_type_params(&concrete, &requirement.owner_type_params))
        .then_some(concrete)
    }

    /// Prove a concrete application has a demand before charging its forward
    /// walk against the instantiation budget. Potential close/transfer demands
    /// can be trivial after substitution, even along a long generic call chain.
    /// A repeated declaration is conservatively relevant: only the bounded
    /// discharge walk decides a recursive application's obligations.
    fn generic_instantiation_is_relevant(
        &self,
        root: &GenericInstantiation,
        requirements: &HashMap<Option<crate::DefId>, Vec<GenericRequirement>>,
        edges: &HashMap<crate::DefId, Vec<GenericCallEdge>>,
        relevant: &mut HashMap<GenericInstantiation, bool>,
    ) -> bool {
        enum Visit {
            Enter(GenericInstantiation),
            Leave(GenericInstantiation, Vec<GenericInstantiation>),
        }
        let mut work = vec![Visit::Enter(root.clone())];
        let mut active = HashSet::new();
        while let Some(visit) = work.pop() {
            match visit {
                Visit::Enter(instance) => {
                    if relevant.contains_key(&instance) {
                        continue;
                    }
                    if active.contains(&instance.0) {
                        relevant.insert(instance, true);
                        continue;
                    }
                    let substitution = instance
                        .1
                        .iter()
                        .map(|(parameter, ty)| (*parameter, ty.clone()))
                        .collect();
                    let demanded = requirements
                        .get(&Some(instance.0))
                        .into_iter()
                        .flatten()
                        .any(|requirement| {
                            let Some(concrete) =
                                self.instantiate_generic_requirement(requirement, &substitution)
                            else {
                                return false;
                            };
                            match requirement.kind {
                                GenericRequirementKind::Eq => true,
                                GenericRequirementKind::BorrowedTransfer { .. } => {
                                    self.place_read_transfers_ownership(&concrete)
                                }
                                GenericRequirementKind::CaptureSnapshot { .. } => {
                                    !self.capture_is_cloneable(&concrete)
                                }
                                GenericRequirementKind::ResourceClose => {
                                    !self.resource_close_instances(&concrete).is_empty()
                                }
                            }
                        });
                    if demanded {
                        relevant.insert(instance, true);
                        continue;
                    }
                    let children: Vec<GenericInstantiation> = edges
                        .get(&instance.0)
                        .into_iter()
                        .flatten()
                        .map(|edge| {
                            (
                                edge.callee,
                                edge.substitution
                                    .iter()
                                    .map(|(parameter, ty)| {
                                        (
                                            *parameter,
                                            ty.substitute_type_params_parallel(&substitution),
                                        )
                                    })
                                    .collect(),
                            )
                        })
                        .collect();
                    active.insert(instance.0);
                    work.push(Visit::Leave(instance, children.clone()));
                    work.extend(children.into_iter().rev().map(Visit::Enter));
                }
                Visit::Leave(instance, children) => {
                    active.remove(&instance.0);
                    let demanded = children.iter().any(|child| relevant[child]);
                    relevant.insert(instance, demanded);
                }
            }
        }
        relevant[root]
    }

    /// Retain the part of the recorded call graph that can reach an obligation.
    /// An unrelated polymorphic cycle must not become conservatively relevant
    /// merely because another module contributed a requirement. Concrete
    /// substitutions still decide relevance within the retained graph.
    fn retain_generic_requirement_edges(
        requirements: &HashMap<Option<crate::DefId>, Vec<GenericRequirement>>,
        edges: &mut HashMap<crate::DefId, Vec<GenericCallEdge>>,
    ) {
        let mut callers: HashMap<crate::DefId, Vec<crate::DefId>> = HashMap::new();
        for (caller, calls) in edges.iter() {
            for call in calls {
                callers.entry(call.callee).or_default().push(*caller);
            }
        }
        let mut demanded: HashSet<_> = requirements.keys().filter_map(|owner| *owner).collect();
        let mut pending_owners: Vec<_> = demanded.iter().copied().collect();
        while let Some(owner) = pending_owners.pop() {
            for caller in callers.get(&owner).into_iter().flatten() {
                if demanded.insert(*caller) {
                    pending_owners.push(*caller);
                }
            }
        }
        edges.retain(|caller, calls| {
            calls.retain(|call| demanded.contains(&call.callee));
            demanded.contains(caller)
        });
    }

    /// Discharge concrete demands and the generic type obligations reachable
    /// through the program's instantiation graph.
    ///
    /// The walk starts at applications whose substitution is concrete in the
    /// caller's terms and follows generic → generic call edges, so an obligation
    /// raised two hops down still lands on the concrete application the
    /// programmer wrote. Each demand consumes its existing semantic authority.
    pub(in crate::check) fn finalize_generic_requirements(&mut self) {
        // Polymorphic recursion (`fn f<T>() { g<Vec<T>>() }`)
        // generates an unbounded instantiation chain. Exceeding it is reported,
        // never skipped — see `generic_structural_eq_depth_error`.
        const MAX_INSTANTIATION_DEPTH: u32 = 64;

        let mut requirements = std::mem::take(&mut self.generic_requirements);
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
        let mut resource_roots = Vec::new();
        let mut new_errors = self.check_concrete_generic_requirements(
            &requirements,
            &mut service,
            &mut resource_roots,
        );
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
        let (roots, mut edges) = self.partition_generic_instantiation_sites(sites);
        Self::retain_generic_requirement_edges(&requirements, &mut edges);
        let mut relevant = HashMap::new();
        let mut seen: HashSet<(GenericInstantiation, usize, Option<String>)> = HashSet::new();
        let mut work: VecDeque<PendingInstantiation> = roots.into();
        work.extend(resource_roots);

        while let Some(pending) = work.pop_front() {
            let instance = (
                pending.callee,
                pending
                    .substitution
                    .iter()
                    .map(|(parameter, ty)| (*parameter, ty.clone()))
                    .collect(),
            );
            if !self.generic_instantiation_is_relevant(
                &instance,
                &requirements,
                &edges,
                &mut relevant,
            ) {
                continue;
            }
            // Diagnostic offsets are module-local; keep their source module
            // alongside the complete instantiated declaration identity.
            if !seen.insert((
                instance,
                pending.report_span.start,
                pending.report_module.clone(),
            )) {
                continue;
            }
            if pending.depth > MAX_INSTANTIATION_DEPTH {
                new_errors.push(
                    self.generic_structural_eq_depth_error(&pending, MAX_INSTANTIATION_DEPTH),
                );
                continue;
            }

            for requirement in requirements
                .get(&Some(pending.callee))
                .into_iter()
                .flatten()
            {
                let Some(concrete) =
                    self.instantiate_generic_requirement(requirement, &pending.substitution)
                else {
                    continue;
                };
                if let Some(error) = self.generic_requirement_error(
                    requirement,
                    &concrete,
                    &mut service,
                    Some(&pending),
                ) {
                    new_errors.push(error);
                }
                work.extend(self.resource_close_pending(requirement, &concrete, Some(&pending)));
            }

            work.extend(
                edges
                    .get(&pending.callee)
                    .into_iter()
                    .flatten()
                    .map(|edge| pending.follow_call(edge)),
            );
        }

        self.errors.extend(new_errors);
    }
}
