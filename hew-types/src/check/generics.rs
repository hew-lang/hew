#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::method_resolution::lookup_method_sig as shared_lookup_method_sig;

/// One method of a trait object's dispatch layout.
#[derive(Clone, Debug)]
pub(super) struct DynLayoutSlot {
    /// Published vtable slot index.
    pub slot: u32,
    /// The trait in the layout's closure that declares this method.
    pub trait_id: crate::DefId,
    /// The trait as diagnostics render it.
    pub trait_spelling: String,
    pub method_name: String,
    pub declaring_trait: crate::DefId,
    /// The trait method declaration this slot dispatches.
    pub method: crate::DefId,
    /// The written bound whose closure first reaches this method; its type
    /// arguments and associated-type bindings substitute the signature.
    pub bound: usize,
    pub receiver: super::DynReceiver,
    pub effect: super::SlotEffect,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum StructuralMethodStatus {
    Required,
    Provided,
}

impl Checker {
    pub(super) fn apply_trait_object_bound_substitutions(
        &self,
        sig: &mut FnSig,
        bound: &crate::ty::TraitObjectBound,
    ) {
        let Some(trait_id) = bound.trait_id else {
            return;
        };
        if let Some(trait_info) = self.trait_info(trait_id) {
            let type_params = &trait_info.type_params;
            if type_params.len() == bound.args.len() {
                // Build the substitution map once and apply in parallel so
                // permuted trait args (e.g. `dyn Mapper<B, A>` for a trait
                // declared `Mapper<A, B>`) don't alias under sequential
                // per-pair substitution.
                let subst_map: HashMap<crate::ParamHead, Ty> = type_params
                    .iter()
                    .zip(bound.args.iter())
                    .map(|(p, a)| (*p, a.clone()))
                    .collect();
                for param_ty in &mut sig.params {
                    *param_ty = param_ty.substitute_type_params_parallel(&subst_map);
                }
                sig.return_type = sig.return_type.substitute_type_params_parallel(&subst_map);
            }
        }
        // TRANSITION(A1c4): WHY `Ty::AssocType` names its trait by the
        // declaration's path render, a carrier HIR also reads. WHEN the carrier
        // holds the trait's `DefId`, this compares ids. WHAT: `AssocType` by id.
        let trait_key = self.defs.path(trait_id);
        for param_ty in &mut sig.params {
            *param_ty = substitute_trait_object_assoc_bindings(param_ty, trait_key, bound);
        }
        sig.return_type =
            substitute_trait_object_assoc_bindings(&sig.return_type, trait_key, bound);
    }

    pub(super) fn freshen_inner(&self, ty: &Ty, mapping: &mut HashMap<u32, Ty>) -> Ty {
        match ty {
            Ty::Var(v) => {
                let resolved = self.subst.resolve(ty);
                if resolved == *ty {
                    // Unresolved — map to a consistent fresh var
                    mapping
                        .entry(v.0)
                        .or_insert_with(|| Ty::Var(TypeVar::fresh()))
                        .clone()
                } else {
                    // Already resolved — use the concrete type
                    resolved
                }
            }
            Ty::Named { head, args } => Ty::Named {
                head: *head,
                args: args
                    .iter()
                    .map(|a| self.freshen_inner(a, mapping))
                    .collect(),
            },
            Ty::Tuple(ts) => Ty::Tuple(ts.iter().map(|t| self.freshen_inner(t, mapping)).collect()),
            Ty::Array(inner, n) => Ty::Array(Box::new(self.freshen_inner(inner, mapping)), *n),
            Ty::Slice(inner) => Ty::Slice(Box::new(self.freshen_inner(inner, mapping))),
            Ty::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(self.freshen_inner(pointee, mapping)),
            },
            Ty::TraitObject { traits } => Ty::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| crate::ty::TraitObjectBound {
                        trait_name: bound.trait_name.clone(),
                        trait_id: bound.trait_id,
                        args: bound
                            .args
                            .iter()
                            .map(|arg| self.freshen_inner(arg, mapping))
                            .collect(),
                        assoc_bindings: bound
                            .assoc_bindings
                            .iter()
                            .map(|(name, ty)| (name.clone(), self.freshen_inner(ty, mapping)))
                            .collect(),
                    })
                    .collect(),
            },
            Ty::Function {
                capabilities,
                params,
                ret,
            } => Ty::Function {
                capabilities: *capabilities,
                params: params
                    .iter()
                    .map(|p| self.freshen_inner(p, mapping))
                    .collect(),
                ret: Box::new(self.freshen_inner(ret, mapping)),
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
                    .map(|p| self.freshen_inner(p, mapping))
                    .collect(),
                ret: Box::new(self.freshen_inner(ret, mapping)),
                captures: captures
                    .iter()
                    .map(|c| self.freshen_inner(c, mapping))
                    .collect(),
                identity: identity.clone(),
            },
            _ => ty.clone(),
        }
    }

    pub(super) fn instantiate_fn_sig_for_call(
        &mut self,
        sig: &FnSig,
        type_args: Option<&[Spanned<TypeExpr>]>,
        span: &Span,
    ) -> (Vec<Ty>, Ty, Vec<Ty>) {
        self.instantiate_fn_sig_for_receiver_call(sig, type_args, span, &[])
    }

    pub(super) fn instantiate_fn_sig_for_receiver_call(
        &mut self,
        sig: &FnSig,
        type_args: Option<&[Spanned<TypeExpr>]>,
        span: &Span,
        receiver_type_args: &[Ty],
    ) -> (Vec<Ty>, Ty, Vec<Ty>) {
        let mut params = sig.params.clone();
        let mut ret = sig.return_type.clone();
        let mut resolved_type_args = Vec::new();

        if let Some(type_args) = type_args {
            if sig.type_params.is_empty() {
                self.report_error(
                    TypeErrorKind::ArityMismatch,
                    span,
                    format!(
                        "this function takes 0 type parameter(s) but {} type argument(s) were supplied",
                        type_args.len()
                    ),
                );
            } else if type_args.len() != sig.type_params.len() {
                self.report_error(
                    TypeErrorKind::ArityMismatch,
                    span,
                    format!(
                        "this function takes {} type parameter(s) but {} type argument(s) were supplied",
                        sig.type_params.len(),
                        type_args.len()
                    ),
                );
            }
        }

        if !sig.type_params.is_empty() {
            resolved_type_args = type_args.map_or(vec![], |args| {
                args.iter()
                    .take(sig.type_params.len())
                    .map(|type_arg| self.resolve_type_expr(type_arg))
                    .collect::<Vec<_>>()
            });
            while resolved_type_args.len() < sig.type_params.len() {
                resolved_type_args.push(Ty::Var(TypeVar::fresh()));
            }
            {
                let subst_map: HashMap<crate::ParamHead, Ty> = sig
                    .type_params
                    .iter()
                    .zip(resolved_type_args.iter())
                    .map(|(tp, ta)| (*tp, ta.clone()))
                    .collect();
                params = params
                    .iter()
                    .map(|param| param.substitute_type_params_parallel(&subst_map))
                    .collect();
                ret = ret.substitute_type_params_parallel(&subst_map);
            }
            // Collapse any `Ty::AssocType` carriers whose `base` has now
            // become concrete (e.g. `I::Item` with `I → Counter` and the
            // impl `Counter: Iterator { type Item = i32 }`). Carriers whose
            // base is still abstract pass through unchanged.
            params = params.iter().map(|p| self.project_assoc_types(p)).collect();
            ret = self.project_assoc_types(&ret);
        }

        // For each call, freshen any unresolved type variables in the signature
        // to make generic builtins like println work with different types each call.
        // Use a shared mapping so params and return type share the same fresh vars.
        let mut mapping: HashMap<u32, Ty> = HashMap::new();
        // Receiver arguments already belong to the caller's inference graph.
        // Freshening them would detach `factory().expect(...)` from later
        // constraints on the extracted value.
        let mut receiver_vars = HashSet::new();
        for argument in receiver_type_args {
            collect_unresolved_inference_vars(&self.subst.resolve(argument), &mut receiver_vars);
        }
        mapping.extend(receiver_vars.into_iter().map(|var| (var.0, Ty::Var(var))));

        let freshened_params = params
            .iter()
            .map(|param| self.freshen_inner(param, &mut mapping))
            .collect();
        let freshened_ret = self.freshen_inner(&ret, &mut mapping);

        // Link original type-arg variables to their freshened counterparts so
        // that unification on the freshened params propagates back, allowing
        // enforce_type_param_bounds to resolve concrete types from the args.
        for ta in &resolved_type_args {
            if let Ty::Var(v) = ta {
                if let Some(fresh_ty) = mapping.get(&v.0) {
                    self.subst.insert(*v, fresh_ty).expect(
                        "freshening an unresolved type variable must not create a substitution cycle",
                    );
                }
            }
        }

        (freshened_params, freshened_ret, resolved_type_args)
    }

    #[cfg(test)]
    pub(super) fn enforce_type_param_bounds(&mut self, sig: &FnSig, type_args: &[Ty], span: &Span) {
        self.enforce_signature_bounds(sig, type_args, span);
    }

    /// Enforce a signature's bounds on one application's type arguments. A
    /// bound's own arguments and associated-type bindings name the
    /// signature's binders, so they are instantiated with the same arguments.
    pub(super) fn enforce_signature_bounds(&mut self, sig: &FnSig, type_args: &[Ty], span: &Span) {
        let bounds = self.instantiate_bounds(&sig.type_params, &sig.bounds, type_args);
        self.enforce_named_type_param_bounds(&sig.type_params, &bounds, type_args, span, None);
    }

    /// Enforce the impl-level bounds on a method call's receiver
    /// (`impl<T, E: Display> Result<T, E>` bounds `E` for `expect`), naming
    /// the method in the diagnostic.
    pub(super) fn enforce_receiver_obligations(
        &mut self,
        sig: &FnSig,
        method: &str,
        owner: &str,
        span: &Span,
    ) {
        let origin = format!("{owner}::{method}");
        for obligation in &sig.receiver_obligations {
            let mut bounds = ParamBounds::default();
            bounds.push(obligation.param.id, obligation.bound.clone());
            self.enforce_named_type_param_bounds(
                &[obligation.param],
                &bounds,
                std::slice::from_ref(&obligation.arg),
                span,
                Some(&origin),
            );
        }
    }

    fn instantiate_bounds(
        &self,
        type_params: &[crate::ParamHead],
        bounds: &ParamBounds,
        type_args: &[Ty],
    ) -> ParamBounds {
        let subst_map: HashMap<crate::ParamHead, Ty> = type_params
            .iter()
            .zip(type_args.iter())
            .map(|(tp, ta)| (*tp, self.subst.resolve(ta)))
            .collect();
        bounds.map_types(|ty| ty.substitute_type_params_parallel(&subst_map))
    }

    /// Canonical declaration-bound enforcement for nominal generic types.
    ///
    /// `TypeDef.bounds` is the checker authority for bounds declared on
    /// nominal declarations (`type` / `record` / `enum` / actor / machine).
    /// Every checker path that builds a `Ty::Named` from a user-authored
    /// nominal with substituted type arguments routes here: annotations,
    /// returns, imports, struct init, tuple-record constructors, enum
    /// variant constructors and machine instantiations. Names without a
    /// registered `TypeDef`, monomorphic names, and declarations without
    /// bounds are no-ops.
    pub(super) fn enforce_type_def_instantiation_bounds(
        &mut self,
        type_name: &str,
        type_args: &[Ty],
        span: &Span,
    ) {
        if type_args.is_empty() {
            return;
        }
        let Some(type_def) = self.lookup_type_def(type_name) else {
            return;
        };
        if type_def.bounds.is_empty() {
            return;
        }
        let resolved_args: Vec<Ty> = type_args
            .iter()
            .map(|arg| self.subst.resolve(arg))
            .collect();
        let dedup_key = (
            type_def.name.clone(),
            resolved_args,
            SpanKey::in_module(span, self.current_module_idx),
        );
        if !self.reported_type_def_bound_violations.insert(dedup_key) {
            return;
        }
        let bounds = self.instantiate_bounds(&type_def.type_params, &type_def.bounds, type_args);
        self.enforce_named_type_param_bounds(&type_def.type_params, &bounds, type_args, span, None);
    }

    /// Bound enforcement over a binder list and its bounds, for a call's or
    /// an instantiation's type arguments.
    pub(super) fn enforce_named_type_param_bounds(
        &mut self,
        type_params: &[crate::ParamHead],
        bounds: &ParamBounds,
        type_args: &[Ty],
        span: &Span,
        required_by: Option<&str>,
    ) {
        for (idx, param) in type_params.iter().enumerate() {
            let param_bounds: Vec<TraitRef> = bounds.of(param.id).cloned().collect();
            if param_bounds.is_empty() {
                continue;
            }
            let Some(type_arg) = type_args.get(idx) else {
                continue;
            };
            let resolved_arg = self.subst.resolve(type_arg);
            // Skip bound enforcement for unresolved inference variables: the type
            // is not yet known, so we cannot evaluate the bound.  The parallel
            // output-boundary prune in `validate_call_type_args_output_contract`
            // (admissibility.rs) ensures that any `call_type_args` entry still
            // carrying an inference var after inference settles is excluded from
            // the codegen output, preventing unresolved holes from reaching the
            // codegen backend. `drain_deferred_bound_checks` revisits the
            // deferred entry once post-inference defaulting settles.
            if resolved_arg.has_inference_var()
                || (!self.type_decls_registered
                    && param_bounds
                        .iter()
                        .any(|bound| self.trait_marker(bound.trait_id) == Some(MarkerTrait::Eq)))
            {
                self.deferred_bound_checks.push(DeferredBoundCheck {
                    type_param: *param,
                    bounds: param_bounds,
                    type_arg: type_arg.clone(),
                    span: span.clone(),
                    scope_bounds: self.active_param_bounds(),
                    required_by: required_by.map(str::to_owned),
                });
                continue;
            }
            // Pin a type param that appears only as the associated-type slot of
            // another param's bound (`where I: Iterator<Item = A>`, with `A`
            // absent from every value parameter and the return type). The base
            // param `I` is concrete here, so the projection `I::Item` is known;
            // unifying it into the still-unbound binding target resolves `A` so
            // the call's resolved type args become fully concrete and the
            // monomorphisation registry can mint a key for it.
            self.pin_projection_only_assoc_bindings(&param_bounds, &resolved_arg);
            self.report_unsatisfied_type_param_bounds(
                *param,
                &param_bounds,
                &resolved_arg,
                span,
                required_by,
            );
            self.report_unsatisfied_assoc_type_bindings(*param, &param_bounds, &resolved_arg, span);
        }
    }

    pub(super) fn drain_deferred_bound_checks(&mut self) {
        for entry in std::mem::take(&mut self.deferred_bound_checks) {
            let resolved_arg = self
                .subst
                .resolve(&entry.type_arg)
                .materialize_literal_defaults();
            if resolved_arg.has_inference_var() {
                continue;
            }
            self.current_type_param_bounds.push(entry.scope_bounds);
            self.report_unsatisfied_type_param_bounds(
                entry.type_param,
                &entry.bounds,
                &resolved_arg,
                &entry.span,
                entry.required_by.as_deref(),
            );
            self.report_unsatisfied_assoc_type_bindings(
                entry.type_param,
                &entry.bounds,
                &resolved_arg,
                &entry.span,
            );
            self.current_type_param_bounds.pop();
        }
    }

    fn report_unsatisfied_type_param_bounds(
        &mut self,
        param: crate::ParamHead,
        bounds: &[TraitRef],
        resolved_arg: &Ty,
        span: &Span,
        required_by: Option<&str>,
    ) {
        for bound in bounds {
            let marker = self.trait_marker(bound.trait_id);
            // A composite over abstract parameters has no concrete selection
            // yet. Carry its Eq demand through the same instantiation graph as
            // an ordinary comparison. Bare parameters still need their declared
            // bound in the active scope.
            if marker == Some(MarkerTrait::Eq)
                && !matches!(resolved_arg, Ty::Named { args, head: crate::TypeHead::Nominal(_) | crate::TypeHead::Param(_) | crate::TypeHead::Unresolved(_), .. } if args.is_empty())
                && resolved_arg.has_type_parameters()
            {
                self.record_eq_requirement(resolved_arg, span);
                continue;
            }
            if self.type_satisfies_bound(resolved_arg, bound) {
                self.report_missing_dispatchable_supertrait_impls(
                    param,
                    bound.trait_id,
                    resolved_arg,
                    span,
                );
                continue;
            }
            let bound_display = self.trait_ref_display(bound);
            let msg = match required_by {
                Some(origin) => format!(
                    "`{origin}` requires `{}: {bound_display}`, but type `{}` does not implement `{bound_display}`",
                    param.spelling,
                    resolved_arg.user_facing(),
                ),
                None => format!(
                    "type `{}` does not implement trait `{bound_display}` required by `{}`",
                    resolved_arg.user_facing(),
                    param.spelling
                ),
            };
            // A Display bound fails because nothing renders the type, so name
            // the impl the program is missing rather than its absent methods.
            let suggestions = if required_by.is_some() && marker == Some(MarkerTrait::Display) {
                let shown = resolved_arg.user_facing();
                vec![format!(
                    "implement `{bound_display}` for `{shown}`, or `impl Error for {shown}` \
                     (which requires `{bound_display}`), or use `handle`/`match` instead"
                )]
            } else if marker == Some(MarkerTrait::Display) {
                vec![format!(
                    "write `impl {bound_display} for {} {{ fn fmt(...) -> string {{ ... }} }}`, \
                     or render the parts that already have one",
                    resolved_arg.user_facing()
                )]
            } else if marker == Some(MarkerTrait::Serializable) {
                self.not_serializable_explanation(resolved_arg)
                    .into_iter()
                    .collect()
            } else {
                self.diagnose_bound_failure_suggestions(resolved_arg, bound.trait_id)
            };
            self.report_error_with_suggestions(
                TypeErrorKind::BoundsNotSatisfied,
                span,
                msg,
                suggestions,
            );
        }
    }

    fn report_missing_dispatchable_supertrait_impls(
        &mut self,
        param: crate::ParamHead,
        declared: crate::DefId,
        resolved_arg: &Ty,
        span: &Span,
    ) {
        if !self.has_trait_impl(resolved_arg, declared) {
            return;
        }
        let declared_display = self.defs.display(declared).to_string();
        for super_trait in self.trait_closure(declared).into_iter().skip(1) {
            let abstract_methods = self.abstract_method_names_declared_by_trait(super_trait);
            if abstract_methods.is_empty()
                || self.has_trait_impl(resolved_arg, super_trait)
                || self.trait_chain_impl_provides_all_declared_methods(
                    resolved_arg,
                    declared,
                    super_trait,
                    &abstract_methods,
                )
            {
                continue;
            }

            let method_label = if abstract_methods.len() == 1 {
                format!("method `{}`", abstract_methods[0])
            } else {
                format!(
                    "methods {}",
                    abstract_methods
                        .iter()
                        .map(|method| format!("`{method}`"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            let super_display = self.defs.display(super_trait).to_string();
            self.report_error(
                TypeErrorKind::BoundsNotSatisfied,
                span,
                format!(
                    "type `{}` implements `{declared_display}` but not its declared supertrait `{super_display}`; \
                     {method_label} (from `{super_display}`) is not callable for `{}` without \
                     `impl {super_display}` for `{}`",
                    resolved_arg.user_facing(),
                    param.spelling,
                    resolved_arg.user_facing(),
                ),
            );
        }
    }

    fn abstract_method_names_declared_by_trait(&self, trait_id: crate::DefId) -> Vec<String> {
        let mut methods: Vec<String> = self
            .trait_info(trait_id)
            .map(|info| {
                info.methods
                    .iter()
                    .filter(|method| method.body.is_none())
                    .map(|method| method.name.to_string())
                    .collect()
            })
            .unwrap_or_default();
        methods.sort();
        methods.dedup();
        methods
    }

    /// Whether impls of `declared` or the traits it extends, for `ty`, write
    /// every one of `methods` that `declaring` declares.
    fn trait_chain_impl_provides_all_declared_methods(
        &self,
        ty: &Ty,
        declared: crate::DefId,
        declaring: crate::DefId,
        methods: &[String],
    ) -> bool {
        let Some(key) = Self::impl_self_key(ty) else {
            return false;
        };
        self.trait_closure(declared).into_iter().any(|impl_trait| {
            self.trait_impl_method_names
                .get(&(key.clone(), impl_trait))
                .is_some_and(|provided| {
                    methods.iter().all(|method| {
                        provided.contains(&Symbol::intern(method))
                            && self.first_declaring_trait_for_impl_method(impl_trait, method)
                                == Some(declaring)
                    })
                })
        })
    }

    /// The first trait in `trait_id`'s chain that declares `method`.
    fn first_declaring_trait_for_impl_method(
        &self,
        trait_id: crate::DefId,
        method: &str,
    ) -> Option<crate::DefId> {
        self.trait_closure(trait_id).into_iter().find(|candidate| {
            self.trait_info(*candidate)
                .is_some_and(|info| info.methods.iter().any(|m| m.name == Ident::new(method)))
        })
    }

    /// Resolve a type param that is reachable only through another param's
    /// associated-type binding (`where I: Iterator<Item = A>`).
    ///
    /// `bounds` are the bounds declared on the base param whose concrete type
    /// is `resolved_arg`. For each binding whose target is still an unbound
    /// inference variable, project the base type's concrete associated type
    /// and unify it into the target. This pins the otherwise free param in
    /// the substitution so the call's resolved type args are fully concrete at
    /// `record_concrete_call_type_args`, which is what lets the
    /// monomorphisation registry observe the instantiation.
    ///
    /// Only the unbound case unifies; a binding with a concrete target is left
    /// to `report_unsatisfied_assoc_type_bindings` to validate. The unify is
    /// best-effort — a mismatch is surfaced by the report pass, not here.
    fn pin_projection_only_assoc_bindings(&mut self, bounds: &[TraitRef], resolved_arg: &Ty) {
        for bound in bounds {
            for (assoc_name, expected_ty) in &bound.assoc {
                let expected = self.subst.resolve(expected_ty);
                if !expected.has_inference_var() {
                    continue;
                }
                if !self.type_satisfies_trait(resolved_arg, bound.trait_id) {
                    continue;
                }
                let actual = self.project_assoc_types(&Ty::AssocType {
                    base: Box::new(resolved_arg.clone()),
                    trait_name: self.defs.path(bound.trait_id).into(),
                    assoc_name: assoc_name.as_str().into(),
                });
                let actual = self.subst.resolve(&actual).materialize_literal_defaults();
                if actual.has_inference_var() {
                    continue;
                }
                let _ = self.try_unify_with_owner_identity(&expected, &actual);
            }
        }
    }

    fn report_unsatisfied_assoc_type_bindings(
        &mut self,
        param: crate::ParamHead,
        bounds: &[TraitRef],
        resolved_arg: &Ty,
        span: &Span,
    ) {
        for bound in bounds {
            for (assoc_name, expected_ty) in &bound.assoc {
                if !self.type_satisfies_trait(resolved_arg, bound.trait_id) {
                    continue;
                }
                let actual = self.project_assoc_types(&Ty::AssocType {
                    base: Box::new(resolved_arg.clone()),
                    trait_name: self.defs.path(bound.trait_id).into(),
                    assoc_name: assoc_name.as_str().into(),
                });
                let actual = self.subst.resolve(&actual).materialize_literal_defaults();
                let expected = self
                    .subst
                    .resolve(expected_ty)
                    .materialize_literal_defaults();
                if actual.has_inference_var() || expected.has_inference_var() || actual == expected
                {
                    continue;
                }
                self.report_error(
                    TypeErrorKind::BoundsNotSatisfied,
                    span,
                    format!(
                        "type `{}` does not satisfy associated type binding \
                         `{}: {}<{assoc_name} = {}>`; found `{}`",
                        resolved_arg.user_facing(),
                        param.spelling,
                        self.defs.display(bound.trait_id),
                        expected.user_facing(),
                        actual.user_facing()
                    ),
                );
            }
        }
    }

    /// Produces 0-or-1 suggestion strings explaining *why* a `BoundsNotSatisfied`
    /// error was raised for a `Ty::Named` type.  Called only after
    /// `type_satisfies_trait_bound` has already returned `false`.
    ///
    /// Returns an empty vec when no specific diagnosis is available (e.g. primitive
    /// types, trait-objects, or unknown traits).  Returns a single-element vec with
    /// an actionable hint when the failure mode can be identified:
    ///
    /// * **E1 guard** — trait has associated types or generic methods; explicit impl needed.
    /// * **Missing methods** — type exists but is missing required trait methods.
    /// * **Arity mismatch** — method exists with the wrong number of parameters.
    /// * **Signature mismatch** — method exists with wrong return type or parameter types.
    #[allow(
        clippy::too_many_lines,
        reason = "each branch is a distinct failure mode with its own message"
    )]
    fn diagnose_bound_failure_suggestions(
        &mut self,
        ty: &Ty,
        trait_id: crate::DefId,
    ) -> Vec<String> {
        let Ty::Named { head, .. } = ty else {
            return vec![];
        };
        let concrete_ty = Ty::named_head(*head, Vec::new());
        let type_name = concrete_ty.user_facing().to_string();
        let Some(trait_info) = self.trait_info(trait_id).cloned() else {
            return vec![];
        };
        let trait_display = self.defs.display(trait_id).to_string();

        // E1 guard: associated types require an explicit impl alias scope.
        if !trait_info.associated_types.is_empty() {
            return vec![format!(
                "trait `{trait_display}` requires an explicit `impl` declaration \
                 (it declares associated types)"
            )];
        }
        // E1 guard: generic methods require per-call substitution (later slice).
        if trait_info
            .methods
            .iter()
            .any(|m| m.type_params.as_ref().is_some_and(|tp| !tp.is_empty()))
        {
            return vec![format!(
                "trait `{trait_display}` requires an explicit `impl` declaration \
                 (it has generic methods)"
            )];
        }

        // Collect required methods; also catches E1 guards deep in the super-trait chain.
        let Some(required) = self.collect_structural_required_methods(trait_id, &mut Vec::new())
        else {
            return vec![format!(
                "trait `{trait_display}` requires an explicit `impl` declaration \
                 (a super-trait declares associated types or generic methods)"
            )];
        };

        // A trait with no required methods (all defaults or empty) still needs an explicit impl.
        if required.is_empty() {
            return vec![format!(
                "trait `{trait_display}` has no required methods — add an explicit \
                 `impl {trait_display} for {type_name}` declaration"
            )];
        }

        let receiver = crate::ParamHead::receiver(trait_id);
        let mut missing: Vec<String> = Vec::new();

        for method_name in &required {
            let Some(trait_sig) = self.lookup_trait_method(trait_id, method_name) else {
                continue;
            };

            let Some(type_sig) = shared_lookup_method_sig(
                &self.defs,
                &self.type_defs,
                self.sigs(),
                &concrete_ty,
                method_name,
            ) else {
                missing.push(format!("`{method_name}`"));
                continue;
            };

            // Arity mismatch — return on first found.
            if trait_sig.params.len() != type_sig.params.len() {
                return vec![format!(
                    "`{type_name}.{method_name}` has {} parameter(s) but trait \
                     `{trait_display}` requires {} — arity mismatch",
                    type_sig.params.len(),
                    trait_sig.params.len(),
                )];
            }

            // Return-type mismatch.
            let expected_ret = trait_sig
                .return_type
                .substitute_type_param(receiver, &concrete_ty);
            if expected_ret != type_sig.return_type {
                return vec![format!(
                    "`{type_name}.{method_name}` returns `{}` but trait `{trait_display}` \
                     requires `{}` — return-type mismatch",
                    type_sig.return_type.user_facing(),
                    expected_ret.user_facing(),
                )];
            }

            // Per-parameter type mismatch.
            for (i, (trait_param, type_param)) in trait_sig
                .params
                .iter()
                .zip(type_sig.params.iter())
                .enumerate()
            {
                let expected = trait_param.substitute_type_param(receiver, &concrete_ty);
                if expected != *type_param {
                    return vec![format!(
                        "`{type_name}.{method_name}` parameter {} has type `{}` but \
                         trait `{trait_display}` requires `{}` — type mismatch",
                        i + 1,
                        type_param.user_facing(),
                        expected.user_facing(),
                    )];
                }
            }
        }

        if !missing.is_empty() {
            let list = missing.join(", ");
            return vec![format!(
                "`{type_name}` is missing method(s) required by trait `{trait_display}`: {list}"
            )];
        }

        vec![]
    }

    /// Collect all required (non-default, non-generic-method) method names from
    /// `trait_id` and its effective super-trait surface.
    ///
    /// Returns `None` if any trait in the chain has associated types or generic
    /// methods (the E1 guards), which disqualifies the whole structural check.
    ///
    /// A child trait declaration shadows inherited methods of the same name,
    /// including when the child provides a default implementation. In that case
    /// the inherited requirement is satisfied by the child trait itself and is
    /// not re-required from the concrete type. Sibling super-trait branches are
    /// merged by method name, so a default-providing branch covers that method
    /// for the combined surface.
    pub(super) fn collect_structural_required_methods(
        &self,
        trait_id: crate::DefId,
        visited: &mut Vec<crate::DefId>,
    ) -> Option<Vec<String>> {
        let surface = self.collect_structural_method_surface(trait_id, visited)?;
        Some(
            surface
                .into_iter()
                .filter_map(|(name, status)| {
                    (status == StructuralMethodStatus::Required).then_some(name)
                })
                .collect(),
        )
    }

    fn collect_structural_method_surface(
        &self,
        trait_id: crate::DefId,
        visited: &mut Vec<crate::DefId>,
    ) -> Option<HashMap<String, StructuralMethodStatus>> {
        if visited.contains(&trait_id) {
            return Some(HashMap::new());
        }
        visited.push(trait_id);

        let trait_info = self.trait_info(trait_id)?.clone();

        // E1 guard: associated types require an explicit impl alias scope.
        if !trait_info.associated_types.is_empty() {
            return None;
        }
        // E1 guard: generic methods require per-call substitution (later slice).
        if trait_info
            .methods
            .iter()
            .any(|m| m.type_params.as_ref().is_some_and(|tp| !tp.is_empty()))
        {
            return None;
        }

        let mut surface = HashMap::new();
        let mut declared_here = HashSet::new();
        for method in &trait_info.methods {
            declared_here.insert(method.name.to_string());
            let status = if method.body.is_none() {
                StructuralMethodStatus::Required
            } else {
                StructuralMethodStatus::Provided
            };
            surface.insert(method.name.to_string(), status);
        }

        // Each super-trait branch gets its own visited path so sibling
        // super-traits can still observe the same ancestor. Their effective
        // surfaces are merged back together here so sibling shadowing/default
        // coverage is preserved at the parent trait.
        for &super_trait in self.trait_supers(trait_id) {
            let mut super_visited = visited.clone();
            let super_surface =
                self.collect_structural_method_surface(super_trait, &mut super_visited)?;
            for (method_name, status) in super_surface {
                if declared_here.contains(&method_name) {
                    continue;
                }
                match surface.entry(method_name) {
                    Entry::Vacant(entry) => {
                        entry.insert(status);
                    }
                    Entry::Occupied(mut entry) => {
                        if *entry.get() == StructuralMethodStatus::Required
                            && status == StructuralMethodStatus::Provided
                        {
                            entry.insert(StructuralMethodStatus::Provided);
                        }
                    }
                }
            }
        }

        Some(surface)
    }

    /// Structural-bounds check for compositional interfaces (Stage 1 / E2 + hardening).
    ///
    /// Returns `true` when **all** required (non-default) methods of the trait **and
    /// its super-trait chain** are present on the concrete type with compatible
    /// signatures.  The following guards from E1 are preserved across the whole chain:
    ///
    /// * **Associated-type guard** — traits that declare associated types (anywhere in
    ///   the super-trait chain) are not handled structurally; an explicit `impl` alias
    ///   scope is required.
    /// * **Generic-method guard** — traits with per-method type parameters (anywhere in
    ///   the chain) are not handled structurally; per-call substitution belongs in a
    ///   later slice.
    ///
    /// Signature compatibility (E2 definition):
    /// * Same number of non-receiver parameters.
    /// * Each non-receiver parameter type matches after substituting `Self` → concrete type.
    /// * Return type matches after the same substitution.
    ///
    /// `dyn Trait` coercion is handled by `Checker::try_record_dyn_trait_coercion`
    /// in `coerce.rs`, which calls this structural-satisfaction predicate as
    /// part of the fallback path. Vtable static emission itself is owned by
    /// the LLVM emitter; the checker populates `TypeCheckOutput::dyn_trait_coercions`
    /// at every accepted coercion site and rejects non-object-safe traits
    /// (generic methods, `Self`-returning methods) with `E_TRAIT_NOT_OBJECT_SAFE`.
    pub(super) fn type_structurally_satisfies(&mut self, ty: &Ty, trait_id: crate::DefId) -> bool {
        // A trait that declares no methods of its own - `trait Error: Display {}`
        // - is a marker over its super-traits. Having the super-trait's methods
        // says nothing about the marker, so it still needs an explicit impl.
        // The empty-surface guard below sees the whole chain and would not
        // catch this.
        if self
            .trait_info(trait_id)
            .is_none_or(|info| info.methods.is_empty())
        {
            return false;
        }

        // Collect required methods across the full super-trait chain.
        // Returns None if any E1 guard triggers anywhere in the chain.
        let Some(required) = self.collect_structural_required_methods(trait_id, &mut Vec::new())
        else {
            return false;
        };

        // Conservative: a trait with no required methods (all default or zero methods,
        // even after walking the super-trait chain) is not considered structurally
        // satisfied — an explicit impl is still needed.
        if required.is_empty() {
            return false;
        }

        // The concrete type, used for Self substitution in trait signatures.
        let concrete_ty = match ty {
            Ty::Named { head, .. } => Ty::named_head(*head, Vec::new()),
            other => other.clone(),
        };
        let receiver = crate::ParamHead::receiver(trait_id);

        for method_name in &required {
            // Resolve the trait method's expected signature.
            // lookup_trait_method strips the receiver and walks super-traits.
            let Some(trait_sig) = self.lookup_trait_method(trait_id, method_name) else {
                return false;
            };

            // Look up the concrete type's method using the shared builtin-aware
            // resolver so imported stdlib stubs cannot shadow intrinsic surfaces.
            let Some(type_sig) = shared_lookup_method_sig(
                &self.defs,
                &self.type_defs,
                self.sigs(),
                &concrete_ty,
                method_name,
            ) else {
                return false;
            };

            // Arity check: non-receiver param counts must match.
            if trait_sig.params.len() != type_sig.params.len() {
                return false;
            }

            // Return-type check (Self → concrete type in trait side).
            let expected_ret = trait_sig
                .return_type
                .substitute_type_param(receiver, &concrete_ty);
            if expected_ret != type_sig.return_type {
                return false;
            }

            // Per-parameter type check (Self → concrete type in trait side).
            for (trait_param, type_param) in trait_sig.params.iter().zip(type_sig.params.iter()) {
                let expected = trait_param.substitute_type_param(receiver, &concrete_ty);
                if expected != *type_param {
                    return false;
                }
            }
        }

        self.record_structural_witnesses(trait_id, &concrete_ty, &required);
        true
    }

    /// Publish the inherent method that fills each required method of
    /// `trait_id` or one of its super-traits for `concrete_ty`.
    fn record_structural_witnesses(
        &mut self,
        trait_id: crate::DefId,
        concrete_ty: &Ty,
        required: &[String],
    ) {
        let Some(self_type) = ResolvedTy::from_ty(concrete_ty)
            .ok()
            .and_then(|ty| ty.impl_receiver_instance(&self.defs))
            .map(|instance| instance.nominal)
        else {
            return;
        };
        for method_name in required {
            let Some((declaring, _)) = self.lookup_trait_method_with_origin(trait_id, method_name)
            else {
                continue;
            };
            let (Some((declaring_trait, method)), Some(inherent)) = (
                self.trait_method_ids_of(declaring, method_name),
                self.inherent_impl_method_declaration(concrete_ty, method_name),
            ) else {
                continue;
            };
            let witness = StructuralWitness {
                declaring_trait,
                self_type,
                method,
                inherent,
            };
            if !self.structural_witnesses.contains(&witness) {
                self.structural_witnesses.push(witness);
            }
        }
    }

    /// Run `f` with the lexical scope of the module that declared `trait_id`.
    fn in_trait_declaring_scope<R>(
        &mut self,
        trait_id: crate::DefId,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let Some((module, file)) = self
            .trait_info(trait_id)
            .map(|info| (info.source_module.clone(), info.file_index))
        else {
            return f(self);
        };
        let saved_module = std::mem::replace(&mut self.current_module, module);
        let saved_file = std::mem::replace(&mut self.current_module_idx, file);
        // `Self` in a trait declaration is the trait's receiver binder, never
        // the impl being checked when the lookup happens.
        let saved_self = self.current_self_type.take();
        let result = f(self);
        self.current_self_type = saved_self;
        self.current_module = saved_module;
        self.current_module_idx = saved_file;
        result
    }

    /// Look up a method on a trait, walking super-traits if needed.
    /// Returns a `FnSig` with the receiver filtered out.
    pub(super) fn lookup_trait_method(
        &mut self,
        trait_id: crate::DefId,
        method: &str,
    ) -> Option<FnSig> {
        self.lookup_trait_method_inner(trait_id, method, true)
    }

    /// Look up a method on a trait, optionally keeping the receiver parameter.
    /// `skip_receiver` = true gives the form used by method-call syntax;
    /// `skip_receiver` = false gives the full signature for qualified calls.
    pub(super) fn lookup_trait_method_inner(
        &mut self,
        trait_id: crate::DefId,
        method: &str,
        skip_receiver: bool,
    ) -> Option<FnSig> {
        self.lookup_trait_method_with_origin_inner(trait_id, method, skip_receiver)
            .map(|(_, sig)| sig)
    }

    /// Like `lookup_trait_method` but returns the *declaring* trait alongside
    /// the signature. The declaring trait is the trait whose `trait_defs` entry
    /// directly contains the method (not a supertrait walk result).
    ///
    /// Used by static trait dispatch to distinguish a method declared in trait A
    /// but reached through bound B (where `trait B: A`).
    pub(super) fn lookup_trait_method_with_origin(
        &mut self,
        trait_id: crate::DefId,
        method: &str,
    ) -> Option<(crate::DefId, FnSig)> {
        self.lookup_trait_method_with_origin_inner(trait_id, method, true)
    }

    /// The dispatch layout of a whole trait-object type: every method of
    /// every bound and of the bounds' supertraits, one entry per trait method
    /// declaration.
    ///
    /// Supertraits come before the traits that extend them (post-order
    /// depth-first, bounds in written order), so `dyn Error` publishes
    /// `Display.fmt` first. A method reached through two bounds, such as a
    /// shared supertrait's, occupies one slot. Two traits that each declare
    /// a method of the same name keep separate slots; a call by that name is
    /// ambiguous.
    ///
    /// This is the one slot numbering: the coercion site fills the vtable in
    /// this order and every dispatch site reads its slot from the same list.
    /// A published slot index is the 0-based position; physical MIR alone
    /// places it past the runtime table's prefix.
    ///
    /// A method without a declaration identity cannot be told apart from
    /// another trait's method of the same name, so it is reported at `span`
    /// and no layout is returned.
    pub(super) fn dyn_layout(
        &mut self,
        traits: &[crate::ty::TraitObjectBound],
        span: &Span,
    ) -> Option<Vec<DynLayoutSlot>> {
        let mut layout = Vec::new();
        let mut closure = Vec::new();
        let mut visited = std::collections::HashSet::new();
        for (bound, trait_object_bound) in traits.iter().enumerate() {
            let Some(trait_id) = trait_object_bound.trait_id else {
                continue;
            };
            let pushed = self.push_dyn_layout_trait(
                trait_id,
                bound,
                &mut visited,
                &mut closure,
                &mut layout,
            );
            if let Err(unidentified) = pushed {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "trait method `{unidentified}` has no declaration identity, so \
                         `{}` cannot dispatch it",
                        Ty::TraitObject {
                            traits: traits.to_vec()
                        }
                        .user_facing()
                    ),
                );
                return None;
            }
        }
        self.record_trait_object_layout(traits, closure, &layout);
        Some(layout)
    }

    fn push_dyn_layout_trait(
        &self,
        trait_id: crate::DefId,
        bound: usize,
        visited: &mut std::collections::HashSet<crate::DefId>,
        closure: &mut Vec<crate::DefId>,
        layout: &mut Vec<DynLayoutSlot>,
    ) -> Result<(), String> {
        if !visited.insert(trait_id) {
            return Ok(());
        }
        for &super_trait in self.trait_supers(trait_id) {
            self.push_dyn_layout_trait(super_trait, bound, visited, closure, layout)?;
        }
        closure.push(trait_id);
        let Some(info) = self.trait_info(trait_id) else {
            return Ok(());
        };
        let spelling = self.defs.display(trait_id);
        for method in &info.methods {
            let (declaring_trait, method_id) = self
                .trait_method_ids_of(trait_id, method.name.name.as_str())
                .ok_or_else(|| format!("{spelling}.{}", method.name))?;
            if layout.iter().any(|slot| slot.method == method_id) {
                continue;
            }
            let position = u32::try_from(layout.len()).expect("trait-object layout exceeds u32");
            layout.push(DynLayoutSlot {
                slot: position,
                trait_id,
                trait_spelling: spelling.to_string(),
                method_name: method.name.to_string(),
                declaring_trait,
                method: method_id,
                bound,
                receiver: if method.consumes_self {
                    super::DynReceiver::Consume
                } else if method.params.first().is_some_and(|param| param.is_mutable) {
                    super::DynReceiver::BorrowMut
                } else {
                    super::DynReceiver::Borrow
                },
                effect: if method.suspends {
                    super::SlotEffect::Suspends
                } else {
                    super::SlotEffect::Plain
                },
            });
        }
        Ok(())
    }

    /// The layout slot of one trait method declaration, for a dispatch the
    /// language fixes to that method (`Display.fmt` on the entry exit path,
    /// `Index.at` for `[]`). A miss is reported at `span`.
    pub(super) fn dyn_layout_slot_of(
        &mut self,
        traits: &[crate::ty::TraitObjectBound],
        trait_id: crate::DefId,
        method: &str,
        span: &Span,
    ) -> Option<DynLayoutSlot> {
        let method_id = self
            .trait_method_ids_of(trait_id, method)
            .map(|(_, method_id)| method_id);
        let slot = self
            .dyn_layout(traits, span)?
            .into_iter()
            .find(|slot| Some(&slot.method) == method_id.as_ref());
        if slot.is_none() {
            self.report_error(
                TypeErrorKind::BoundsNotSatisfied,
                span,
                format!(
                    "`{}` has no vtable slot for `{}.{method}`",
                    Ty::TraitObject {
                        traits: traits.to_vec()
                    }
                    .user_facing(),
                    self.defs.display(trait_id)
                ),
            );
        }
        slot
    }

    /// Walk `trait_id` and ALL of its (transitive) supertraits, collecting
    /// every trait that DIRECTLY declares a method named `method`. The
    /// returned `Vec` is sorted + deduplicated so repeated bound paths
    /// collapse to a stable set.
    ///
    /// This is the supertrait-aware companion to
    /// `lookup_trait_method_with_origin`, which returns only the first
    /// declaring trait it encounters. Used by the static-dispatch path to
    /// detect supertrait-redeclaration ambiguity (plan §4 V14): if the same
    /// method name is directly declared by both a trait and one of its
    /// supertraits, a bound `T: SubTrait` reaches the method via two
    /// distinct declaring traits and the call site is ambiguous.
    pub(super) fn collect_all_declaring_traits_for_method(
        &self,
        trait_id: crate::DefId,
        method: &str,
    ) -> Vec<crate::DefId> {
        let mut out: Vec<crate::DefId> = self
            .trait_closure(trait_id)
            .into_iter()
            .filter(|candidate| {
                self.trait_info(*candidate)
                    .is_some_and(|info| info.methods.iter().any(|m| m.name == Ident::new(method)))
            })
            .collect();
        out.sort();
        out.dedup();
        out
    }

    fn lookup_trait_method_with_origin_inner(
        &mut self,
        trait_id: crate::DefId,
        method: &str,
        skip_receiver: bool,
    ) -> Option<(crate::DefId, FnSig)> {
        // Check the trait's own methods first (direct declaration).
        let found_method = self.trait_info(trait_id).and_then(|info| {
            info.methods
                .iter()
                .find(|m| m.name == Ident::new(method))
                .cloned()
        });
        if let Some(m) = found_method {
            let skip = if skip_receiver {
                usize::from(m.params.first().is_some_and(|p| p.is_receiver))
            } else {
                0
            };
            // Activate `Self::Bar` projection so the resolver materialises
            // a `Ty::AssocType` carrier for the trait method's signature.
            let prev_trait_self = self.current_trait_for_self_projection.replace(trait_id);
            // The declaration's types and bounds name what its own module sees.
            let (params, return_type, type_params, bounds) =
                self.in_trait_declaring_scope(trait_id, |checker| {
                    let params: Vec<Ty> = m
                        .params
                        .iter()
                        .skip(skip)
                        .map(|p| checker.resolve_type_expr(&p.ty))
                        .collect();
                    let return_type = m
                        .return_type
                        .as_ref()
                        .map_or(Ty::Unit, |annotation| checker.resolve_type_expr(annotation));
                    let type_params = checker.source_parameter_heads(
                        m.type_params.as_deref().unwrap_or_default(),
                        &m.span,
                    );
                    let bounds = checker.collect_type_param_bounds(
                        m.type_params.as_ref(),
                        m.where_clause.as_ref(),
                        &mut Vec::new(),
                    );
                    (params, return_type, type_params, bounds)
                });
            self.current_trait_for_self_projection = prev_trait_self;
            let param_names: Vec<String> = m
                .params
                .iter()
                .skip(skip)
                .map(|p| p.name.to_string())
                .collect();
            // W3.042 S2-S4: propagate `requires_mutable_receiver` from the
            // trait declaration's receiver parameter so the dyn-trait and
            // static-dispatch gates, and any other consumer that reads the
            // substituted `FnSig`, see the flag.
            let requires_mutable_receiver = m
                .params
                .first()
                .is_some_and(|p| p.is_receiver && p.is_mutable);
            let returns_receiver_identity = Self::trait_receiver_identity_is_structurally_valid(&m);
            return Some((
                trait_id,
                FnSig {
                    type_params,
                    bounds,
                    param_names,
                    params,
                    return_type,
                    requires_mutable_receiver,
                    consumes_receiver: m.consumes_self,
                    associated: !m.params.first().is_some_and(|p| p.is_receiver),
                    returns_receiver_identity,
                    ..FnSig::default()
                },
            ));
        }
        // Walk super-traits — propagate origin unchanged from the recursion.
        for super_trait in self.trait_supers(trait_id).to_vec() {
            if let Some(result) =
                self.lookup_trait_method_with_origin_inner(super_trait, method, skip_receiver)
            {
                return Some(result);
            }
        }
        None
    }

    /// The trait and method declarations `trait_id` reaches `method` by.
    pub(in crate::check) fn trait_method_ids_of(
        &self,
        trait_id: crate::DefId,
        method: &str,
    ) -> Option<(crate::DefId, crate::DefId)> {
        self.trait_method_ids_for_key(self.defs.path(trait_id), method)
    }
}

fn substitute_trait_object_assoc_bindings(
    ty: &Ty,
    trait_name: &str,
    bound: &crate::ty::TraitObjectBound,
) -> Ty {
    match ty {
        Ty::AssocType {
            base,
            trait_name: projected_trait,
            assoc_name,
        } if projected_trait.as_ref() == trait_name
            && matches!(
                base.as_ref(),
                Ty::Named { head, args } if matches!(head, crate::TypeHead::Param(parameter) if parameter.is_receiver()) && args.is_empty()
            ) =>
        {
            bound
                .assoc_bindings
                .iter()
                .find(|(name, _)| name == assoc_name.as_ref())
                .map_or_else(
                    || ty.clone(),
                    |(_, binding_ty)| {
                        substitute_trait_object_assoc_bindings(binding_ty, trait_name, bound)
                    },
                )
        }
        Ty::AssocType {
            base,
            trait_name: projected_trait,
            assoc_name,
        } => Ty::AssocType {
            base: Box::new(substitute_trait_object_assoc_bindings(
                base, trait_name, bound,
            )),
            trait_name: projected_trait.clone(),
            assoc_name: assoc_name.clone(),
        },
        _ => ty.map_children_pub(&|child| {
            substitute_trait_object_assoc_bindings(child, trait_name, bound)
        }),
    }
}

// ── W3.039 Stage 2/2.5: machine const-generic arg validation ────────────

impl Checker {
    /// Validate a single const-generic argument at a machine
    /// instantiation site.
    ///
    /// Routes the argument expression through the constexpr sub-engine
    /// (`super::const_eval::eval_const_expr`, R268=B). On success
    /// returns `Some(MachineConstArgValue::Usize(n))`; on failure emits
    /// a typed diagnostic against `self.errors` and returns `None`.
    ///
    /// The `decl_param` argument carries the declaration-side parameter
    /// shape so the function can reject width mismatches (R269=A: only
    /// `usize` is supported in Phase 0; the parameter is retained so
    /// widening can extend without re-threading callers).
    ///
    /// Stage 3 (W3.039) wires this into the machine instantiation
    /// resolution path; until then the function is callable from tests
    /// and from any future call site without further plumbing.
    #[allow(
        dead_code,
        reason = "wired by Stage 3 of W3.039; exposed now so the sub-engine surface and side-table contract are testable"
    )]
    pub(super) fn validate_const_param_arg(
        &mut self,
        arg: &Spanned<Expr>,
        decl_param: &super::types::MachineConstParamDecl,
        env: &super::const_eval::ConstEnv,
    ) -> Option<super::types::MachineConstArgValue> {
        match decl_param.ty {
            super::types::MachineConstParamTy::Usize => {}
        }
        match super::const_eval::eval_const_expr(arg, env) {
            Ok(value) => Some(super::types::MachineConstArgValue::Usize(value)),
            // `eval_const_expr` is the machine-`usize` compatibility wrapper
            // and maps these target-typed classes to `Overflow` before they
            // reach this call site. Keep an explicit defensive arm so a future
            // wrapper regression still fails closed with the established
            // const-generic diagnostic rather than changing the machine domain.
            Err(
                super::const_eval::ConstEvalError::Overflow
                | super::const_eval::ConstEvalError::ArithmeticOverflow
                | super::const_eval::ConstEvalError::DivisionByZero
                | super::const_eval::ConstEvalError::OutOfRange,
            ) => {
                self.errors.push(crate::error::TypeError::new(
                    crate::error::TypeErrorKind::InvalidOperation,
                    arg.1.clone(),
                    format!(
                        "const argument for `{}` overflows usize or uses a negative value",
                        decl_param.name
                    ),
                ));
                None
            }
            Err(super::const_eval::ConstEvalError::UnknownConst(name)) => {
                self.errors.push(crate::error::TypeError::new(
                    crate::error::TypeErrorKind::UndefinedVariable,
                    arg.1.clone(),
                    format!(
                        "const argument for `{}` references unknown const `{}`",
                        decl_param.name, name
                    ),
                ));
                None
            }
            Err(super::const_eval::ConstEvalError::NotConstant) => {
                self.errors.push(crate::error::TypeError::new(
                    crate::error::TypeErrorKind::InvalidOperation,
                    arg.1.clone(),
                    format!(
                        "const argument for `{}` must be a compile-time integer expression",
                        decl_param.name
                    ),
                ));
                None
            }
        }
    }
}
