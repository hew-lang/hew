#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::method_resolution::lookup_method_sig as shared_lookup_method_sig;

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
        // Keyed on the BARE `bound.trait_name`, deliberately. This is part of the
        // `dyn`-trait subsystem, which is internally consistent on the bare trait
        // name across BOTH its write side (`record_trait_impl` /
        // `record_primitive_trait_impl_method` key `trait_impls_set` /
        // `primitive_trait_impls` on the bare `tb.name`) and its read side
        // (`validate_dyn_trait_bound`, the slot lookup in `methods.rs`,
        // `actor_satisfies_handler_trait`). Re-keying THIS lookup on the
        // owner-qualified identity in isolation would desync it from the caller
        // in `validate_dyn_trait_bound`, which fetched `trait_info` and built the
        // method table from the bare entry — under a same-name arity collision
        // the substitution would then use a different trait's type-params than
        // the vtable was built from. Routing the `dyn` family to owner-qualified
        // identity requires moving its WRITE keys together (the owner-qualified
        // write+read key reshape across `record_trait_impl` /
        // `primitive_trait_impls` / the `coerce.rs` vtable build), which is out of
        // scope for this change.
        //
        // FAIL-CLOSED, with a KNOWN LIMITATION (deferred to v0.5.3). No invalid
        // program is accepted: a wrong-owner `dyn` coercion with no structural
        // match IS rejected. But because this family keys on the bare trait name,
        // a same-name collision (an importer brings in a `Widget` whose method set
        // differs from the impl's owner `Widget`) is not caught HERE in the
        // checker — it is carried far enough to build a vtable and only then
        // rejected by codegen-front's `E_CODEGEN_FRONT` invalid-vtable
        // fail-closed (a vtable slot referencing an impl fn the module never
        // declared). The full fix is the dyn-family owner-qualified write+read key
        // reshape so the bad coercion is rejected at the type boundary with a
        // checker diagnostic, tracked as a v0.5.3 follow-up. Until then the
        // wrong-owner case is rejected at a later layer, never accepted.
        if let Some(trait_info) = self.trait_defs.get(&bound.trait_name) {
            let type_params = &trait_info.type_params;
            if type_params.len() == bound.args.len() {
                // Build the substitution map once and apply in parallel so
                // permuted trait args (e.g. `dyn Mapper<B, A>` for a trait
                // declared `Mapper<A, B>`) don't alias under sequential
                // per-pair substitution.
                let subst_map: HashMap<String, Ty> = type_params
                    .iter()
                    .zip(bound.args.iter())
                    .map(|(p, a)| (p.clone(), a.clone()))
                    .collect();
                for param_ty in &mut sig.params {
                    *param_ty = param_ty.substitute_named_params_parallel(&subst_map);
                }
                sig.return_type = sig.return_type.substitute_named_params_parallel(&subst_map);
            }
        }
        for param_ty in &mut sig.params {
            *param_ty = substitute_trait_object_assoc_bindings(param_ty, &bound.trait_name, bound);
        }
        sig.return_type =
            substitute_trait_object_assoc_bindings(&sig.return_type, &bound.trait_name, bound);
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
            Ty::Named {
                name,
                args,
                builtin,
            } => Ty::Named {
                name: name.clone(),
                builtin: *builtin,
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
            Ty::Function { params, ret } => Ty::Function {
                params: params
                    .iter()
                    .map(|p| self.freshen_inner(p, mapping))
                    .collect(),
                ret: Box::new(self.freshen_inner(ret, mapping)),
            },
            Ty::Closure {
                params,
                ret,
                captures,
            } => Ty::Closure {
                params: params
                    .iter()
                    .map(|p| self.freshen_inner(p, mapping))
                    .collect(),
                ret: Box::new(self.freshen_inner(ret, mapping)),
                captures: captures
                    .iter()
                    .map(|c| self.freshen_inner(c, mapping))
                    .collect(),
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
        let mut params = sig.params.clone();
        let mut ret = sig.return_type.clone();
        let mut resolved_type_args = Vec::new();

        if let Some(type_args) = type_args {
            if sig.type_params.is_empty() {
                self.report_error(
                    TypeErrorKind::ArityMismatch,
                    span,
                    format!(
                        "this function takes 0 type argument(s) but {} were supplied",
                        type_args.len()
                    ),
                );
            } else if type_args.len() != sig.type_params.len() {
                self.report_error(
                    TypeErrorKind::ArityMismatch,
                    span,
                    format!(
                        "this function takes {} type argument(s) but {} were supplied",
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
                let subst_map: HashMap<String, Ty> = sig
                    .type_params
                    .iter()
                    .zip(resolved_type_args.iter())
                    .map(|(tp, ta)| (tp.clone(), ta.clone()))
                    .collect();
                params = params
                    .iter()
                    .map(|param| param.substitute_named_params_parallel(&subst_map))
                    .collect();
                ret = ret.substitute_named_params_parallel(&subst_map);
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
        self.enforce_type_param_bounds_with_assoc(sig, &HashMap::new(), type_args, span);
    }

    pub(super) fn enforce_type_param_bounds_with_assoc(
        &mut self,
        sig: &FnSig,
        type_param_assoc_bindings: &HashMap<(String, String, String), Ty>,
        type_args: &[Ty],
        span: &Span,
    ) {
        let assoc_bindings =
            self.instantiate_type_param_assoc_bindings(sig, type_param_assoc_bindings, type_args);
        self.enforce_named_type_param_bounds_with_assoc(
            &sig.type_params,
            &sig.type_param_bounds,
            &assoc_bindings,
            type_args,
            span,
        );
    }

    fn instantiate_type_param_assoc_bindings(
        &self,
        sig: &FnSig,
        type_param_assoc_bindings: &HashMap<(String, String, String), Ty>,
        type_args: &[Ty],
    ) -> HashMap<(String, String, String), Ty> {
        let subst_map: HashMap<String, Ty> = sig
            .type_params
            .iter()
            .zip(type_args.iter())
            .map(|(tp, ta)| (tp.clone(), self.subst.resolve(ta)))
            .collect();
        let mut instantiated = HashMap::with_capacity(type_param_assoc_bindings.len());
        for (key, binding) in type_param_assoc_bindings {
            let ty = binding.substitute_named_params_parallel(&subst_map);
            instantiated.insert(key.clone(), ty);
        }
        instantiated
    }

    /// Canonical machine-instantiation bound-enforcement entry point.
    /// Every checker path that builds a `Ty::Named` whose `name` is a
    /// registered machine and whose `args` carry substituted concrete
    /// types must route through this helper. It looks up the machine's
    /// declared bounds from the registration-time side table (which
    /// already merges inline `<T: Trait>` bounds and `where T: Trait`
    /// predicates into a single per-machine map) and delegates to the
    /// implementation.
    ///
    /// No-op when the named entity is not a registered machine, when
    /// the machine has no bounds, or when `type_args` is empty (a
    /// non-instantiated declaration-shape reference).
    ///
    /// Routed-through sites today:
    ///   1. struct-state brace init (`Holder::Active { … }` typed
    ///      against the machine self-type) — `expressions.rs`.
    ///   2. type-annotation resolution at `resolve_type_expr` (covers
    ///      `var x: Lifecycle<File>`, param annotations, return-type
    ///      slots, and record field types via the single converging
    ///      wrapper) — `resolution.rs`.
    ///   3. constructor / call forms — every `record_concrete_record_init_type_args`
    ///      emission site in `expressions.rs` (plain struct init,
    ///      enum-variant coercion arm, plain struct coercion arm, enum
    ///      struct-variant init) is paired with a helper call.
    ///   4. supervisor-child PID synthesis (defense-in-depth: today's
    ///      child-spec table carries bare names with no type-args, so
    ///      the helper short-circuits on empty `args`; any future
    ///      change admitting parameterised child specs inherits
    ///      enforcement here).
    ///
    /// The invariant this helper locks in: when a machine name is
    /// being instantiated with concrete type arguments and the checker
    /// has the bounds to consult, there is exactly one shared way to
    /// do the consult — not four.
    pub(super) fn enforce_machine_instantiation_bounds(
        &mut self,
        machine_name: &str,
        type_args: &[Ty],
        span: &Span,
    ) {
        if type_args.is_empty() {
            return;
        }
        let Some(bounds) = self.machine_type_param_bounds.get(machine_name).cloned() else {
            return;
        };
        // Dedup identical `(machine, type_args, span)` triples across
        // repeated visits. A single annotation can be re-resolved
        // multiple times (e.g. once during signature registration and
        // again during body checking), and the recursive walker over
        // a resolved `Ty` may visit the same nested instantiation via
        // overlapping paths. Without this gate, each repeat emits an
        // identical `BoundsNotSatisfied` diagnostic. The key includes
        // span so two distinct annotation occurrences of the same
        // textual instantiation (e.g. `Holder<Plain>` as both a
        // parameter and a return type) each report once.
        let dedup_key = (
            machine_name.to_string(),
            type_args.to_vec(),
            SpanKey::in_module(span, self.current_module_idx),
        );
        if !self.reported_machine_bound_violations.insert(dedup_key) {
            return;
        }
        // Recover the machine's declared type-param names from the side
        // table. The table's outer key is the machine name; its keys
        // are the param names that have at least one bound. Params
        // without bounds are absent — for those, no enforcement is
        // required, so the helper builds a positional name vector from
        // the type-args' arity and only enforces for the slots whose
        // param has a bound entry.
        //
        // Because `enforce_named_type_param_bounds` reads bounds by
        // param-name lookup, the positional alignment between the
        // synthesised `type_params` and `type_args` only matters for
        // the slots with bounds. Empty-slot names are placeholders
        // (`__unbounded_<idx>`) chosen to never collide with the bound
        // map, ensuring the helper short-circuits cleanly.
        let mut type_params: Vec<String> = Vec::with_capacity(type_args.len());
        for (idx, _) in type_args.iter().enumerate() {
            // We do not have positional access to the original param
            // names here without a second lookup. The side table
            // preserves names but not order; instead, the canonical
            // type-param-order source is the registered TypeDef. Fall
            // back to scanning bounds map keys against positional
            // arity by querying the matching TypeDef entry.
            if let Some(td) = self.type_defs.get(machine_name) {
                if let Some(name) = td.type_params.get(idx) {
                    type_params.push(name.clone());
                    continue;
                }
            }
            type_params.push(format!("__unbounded_{idx}"));
        }
        self.enforce_named_type_param_bounds(&type_params, &bounds, type_args, span);
    }

    /// Actor-spawn-site bound enforcement.
    ///
    /// Called from `check_spawn` when explicit type args are supplied for a
    /// generic actor. Clones the pattern of `enforce_machine_instantiation_bounds`
    /// verbatim — actors and machines share the same bound-enforcement semantics
    /// but are stored in separate tables so the two categories cannot suppress
    /// each other's violations via the shared dedup set.
    ///
    /// The dedup key includes the actor name, resolved type args, and span.
    /// Identical `(actor, args, span)` triples across repeated checker passes
    /// emit exactly one diagnostic.
    pub(super) fn enforce_actor_instantiation_bounds(
        &mut self,
        actor_name: &str,
        type_args: &[Ty],
        span: &Span,
    ) {
        if type_args.is_empty() {
            return;
        }
        let Some(bounds) = self.actor_type_param_bounds.get(actor_name).cloned() else {
            return;
        };
        let dedup_key = (
            actor_name.to_string(),
            type_args.to_vec(),
            SpanKey::in_module(span, self.current_module_idx),
        );
        if !self.reported_actor_bound_violations.insert(dedup_key) {
            return;
        }
        // Recover positional type-param names from the registered TypeDef so
        // that `enforce_named_type_param_bounds` can look up bounds by name.
        // Falls back to placeholder names for positions without a TypeDef entry
        // (defensive; should not occur for a registered actor).
        let mut type_params: Vec<String> = Vec::with_capacity(type_args.len());
        for (idx, _) in type_args.iter().enumerate() {
            if let Some(td) = self.type_defs.get(actor_name) {
                if let Some(name) = td.type_params.get(idx) {
                    type_params.push(name.clone());
                    continue;
                }
            }
            type_params.push(format!("__unbounded_{idx}"));
        }
        self.enforce_named_type_param_bounds(&type_params, &bounds, type_args, span);
    }

    /// Generic bound-enforcement entry point parameterised by type-param names
    /// and a bounds map.  Used by `enforce_type_param_bounds` (`FnSig` calls)
    /// and by use-site enforcement for machine generic constructors that do
    /// not route through a function call (struct-state brace init via
    /// `check_struct_init`).
    pub(super) fn enforce_named_type_param_bounds(
        &mut self,
        type_params: &[String],
        type_param_bounds: &HashMap<String, Vec<String>>,
        type_args: &[Ty],
        span: &Span,
    ) {
        self.enforce_named_type_param_bounds_with_assoc(
            type_params,
            type_param_bounds,
            &HashMap::new(),
            type_args,
            span,
        );
    }

    fn enforce_named_type_param_bounds_with_assoc(
        &mut self,
        type_params: &[String],
        type_param_bounds: &HashMap<String, Vec<String>>,
        type_param_assoc_bindings: &HashMap<(String, String, String), Ty>,
        type_args: &[Ty],
        span: &Span,
    ) {
        if type_params.is_empty() {
            return;
        }
        for (idx, param_name) in type_params.iter().enumerate() {
            let bounds = type_param_bounds
                .get(param_name)
                .cloned()
                .unwrap_or_default();
            let assoc_bindings =
                Self::assoc_bindings_for_type_param(type_param_assoc_bindings, param_name);
            if bounds.is_empty() && assoc_bindings.is_empty() {
                continue;
            }
            let Some(type_arg) = type_args.get(idx) else {
                continue;
            };
            let resolved_arg = self.subst.resolve(type_arg);
            // Skip bound enforcement for unresolved inference variables: the type
            // is not yet known, so we cannot evaluate the bound.  The parallel
            // guard in `record_concrete_call_type_args` (calls.rs) ensures that
            // any entry that still carries an inference var is also excluded from
            // the codegen `call_type_args` output, preventing unresolved holes
            // from reaching the codegen backend. `drain_deferred_bound_checks`
            // revisits the deferred entry once post-inference defaulting settles.
            if resolved_arg.has_inference_var() {
                self.deferred_bound_checks.push(DeferredBoundCheck {
                    type_param: param_name.clone(),
                    bounds,
                    assoc_bindings,
                    type_arg: type_arg.clone(),
                    span: span.clone(),
                });
                continue;
            }
            self.report_unsatisfied_type_param_bounds(param_name, &bounds, &resolved_arg, span);
            self.report_unsatisfied_assoc_type_bindings(
                param_name,
                &assoc_bindings,
                &resolved_arg,
                span,
            );
        }
    }

    fn assoc_bindings_for_type_param(
        type_param_assoc_bindings: &HashMap<(String, String, String), Ty>,
        param_name: &str,
    ) -> Vec<(String, String, Ty)> {
        type_param_assoc_bindings
            .iter()
            .filter(|((param, _, _), _)| param == param_name)
            .map(|((_, trait_name, assoc_name), ty)| {
                (trait_name.clone(), assoc_name.clone(), ty.clone())
            })
            .collect()
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
            self.report_unsatisfied_type_param_bounds(
                &entry.type_param,
                &entry.bounds,
                &resolved_arg,
                &entry.span,
            );
            self.report_unsatisfied_assoc_type_bindings(
                &entry.type_param,
                &entry.assoc_bindings,
                &resolved_arg,
                &entry.span,
            );
        }
    }

    fn report_unsatisfied_type_param_bounds(
        &mut self,
        param_name: &str,
        bounds: &[String],
        resolved_arg: &Ty,
        span: &Span,
    ) {
        for bound in bounds {
            if self.type_satisfies_trait_bound(resolved_arg, bound) {
                self.report_missing_dispatchable_supertrait_impls(
                    param_name,
                    bound,
                    resolved_arg,
                    span,
                );
                continue;
            }
            let msg = format!(
                "type `{}` does not implement trait `{bound}` required by `{param_name}`",
                resolved_arg.user_facing()
            );
            let suggestions = self.diagnose_bound_failure_suggestions(resolved_arg, bound);
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
        param_name: &str,
        declared_bound: &str,
        resolved_arg: &Ty,
        span: &Span,
    ) {
        let Some(type_name) = Self::impl_lookup_type_name(resolved_arg) else {
            return;
        };
        if !self.has_explicit_trait_impl_for_static_dispatch(&type_name, declared_bound) {
            return;
        }
        let declared_key = self.trait_defs_key_for_bound(declared_bound);
        let mut stack = self
            .trait_super
            .get(&declared_key)
            .cloned()
            .unwrap_or_default();
        let mut visited = HashSet::new();

        while let Some(super_trait) = stack.pop() {
            if !visited.insert(super_trait.clone()) {
                continue;
            }
            if let Some(nested) = self.trait_super.get(&super_trait) {
                stack.extend(nested.iter().cloned());
            }

            let abstract_methods = self.abstract_method_names_declared_by_trait(&super_trait);
            if abstract_methods.is_empty()
                || self.has_explicit_trait_impl_for_static_dispatch(&type_name, &super_trait)
                || self.trait_chain_impl_provides_all_declared_methods(
                    &type_name,
                    declared_bound,
                    &super_trait,
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
            self.report_error(
                TypeErrorKind::BoundsNotSatisfied,
                span,
                format!(
                    "type `{}` implements `{declared_bound}` but not its declared supertrait `{}`; \
                     {method_label} (from `{}`) is not callable for `{param_name}` without \
                     `impl {}` for `{}`",
                    resolved_arg.user_facing(),
                    super_trait,
                    super_trait,
                    super_trait,
                    resolved_arg.user_facing(),
                ),
            );
        }
    }

    fn impl_lookup_type_name(ty: &Ty) -> Option<String> {
        match ty {
            Ty::Named { name, .. } => Some(name.clone()),
            _ => ty
                .canonical_lowering_name()
                .or(match ty {
                    Ty::IntLiteral => Some("i64"),
                    Ty::FloatLiteral => Some("f64"),
                    _ => None,
                })
                .map(str::to_string),
        }
    }

    fn abstract_method_names_declared_by_trait(&self, trait_name: &str) -> Vec<String> {
        let mut methods: Vec<String> = self
            .trait_defs
            .get(trait_name)
            .map(|info| {
                info.methods
                    .iter()
                    .filter(|method| method.body.is_none())
                    .map(|method| method.name.clone())
                    .collect()
            })
            .unwrap_or_default();
        methods.sort();
        methods.dedup();
        methods
    }

    fn has_explicit_trait_impl_for_static_dispatch(
        &self,
        type_name: &str,
        trait_name: &str,
    ) -> bool {
        let type_candidates = self.impl_lookup_type_candidates(type_name);
        let trait_candidates = self.trait_lookup_candidates(trait_name);
        type_candidates.iter().any(|ty| {
            trait_candidates
                .iter()
                .any(|tr| self.trait_impls_set.contains(&(ty.clone(), tr.clone())))
        })
    }

    fn trait_chain_impl_provides_all_declared_methods(
        &self,
        type_name: &str,
        declared_bound: &str,
        declaring_trait: &str,
        methods: &[String],
    ) -> bool {
        let type_candidates = self.impl_lookup_type_candidates(type_name);
        let declaring_candidates = self.trait_lookup_candidates(declaring_trait);
        let impl_trait_candidates = self.trait_chain_lookup_candidates(declared_bound);
        type_candidates.iter().any(|ty| {
            impl_trait_candidates.iter().any(|impl_trait| {
                let key = (ty.clone(), impl_trait.clone());
                self.trait_impl_method_names
                    .get(&key)
                    .is_some_and(|provided| {
                        methods.iter().all(|method| {
                            provided.contains(method)
                                && self
                                    .first_declaring_trait_for_impl_method(impl_trait, method)
                                    .is_some_and(|origin| {
                                        declaring_candidates
                                            .iter()
                                            .any(|candidate| candidate == &origin)
                                    })
                        })
                    })
            })
        })
    }

    fn trait_chain_lookup_candidates(&self, trait_name: &str) -> Vec<String> {
        let root = self.trait_defs_key_for_bound(trait_name);
        let mut stack = vec![root];
        let mut out = Vec::new();
        let mut visited = HashSet::new();
        while let Some(current) = stack.pop() {
            if !visited.insert(current.clone()) {
                continue;
            }
            out.extend(self.trait_lookup_candidates(&current));
            if let Some(supers) = self.trait_super.get(&current) {
                stack.extend(supers.iter().cloned());
            }
        }
        out.sort();
        out.dedup();
        out
    }

    fn first_declaring_trait_for_impl_method(
        &self,
        trait_name: &str,
        method: &str,
    ) -> Option<String> {
        let trait_key = self.trait_defs_key_for_bound(trait_name);
        if self
            .trait_defs
            .get(&trait_key)
            .is_some_and(|info| info.methods.iter().any(|m| m.name == method))
        {
            return Some(trait_key);
        }

        let mut stack = self
            .trait_super
            .get(&trait_key)
            .cloned()
            .unwrap_or_default();
        let mut visited = HashSet::new();
        while let Some(current) = stack.pop() {
            if !visited.insert(current.clone()) {
                continue;
            }
            if self
                .trait_defs
                .get(&current)
                .is_some_and(|info| info.methods.iter().any(|m| m.name == method))
            {
                return Some(current);
            }
            if let Some(supers) = self.trait_super.get(&current) {
                stack.extend(supers.iter().cloned());
            }
        }
        None
    }

    fn impl_lookup_type_candidates(&self, type_name: &str) -> Vec<String> {
        let mut candidates = vec![type_name.to_string()];
        if let Some((module, bare)) = type_name.split_once('.') {
            if self.modules.contains(module) {
                candidates.push(bare.to_string());
            }
        }
        candidates.sort();
        candidates.dedup();
        candidates
    }

    fn trait_lookup_candidates(&self, trait_name: &str) -> Vec<String> {
        let mut candidates = vec![trait_name.to_string()];
        if let Some((module, bare)) = trait_name.split_once('.') {
            if self.modules.contains(module) {
                candidates.push(bare.to_string());
            }
        }
        candidates.sort();
        candidates.dedup();
        candidates
    }

    fn report_unsatisfied_assoc_type_bindings(
        &mut self,
        param_name: &str,
        assoc_bindings: &[(String, String, Ty)],
        resolved_arg: &Ty,
        span: &Span,
    ) {
        for (trait_name, assoc_name, expected_ty) in assoc_bindings {
            if !self.type_satisfies_trait_bound(resolved_arg, trait_name) {
                continue;
            }
            let actual = self.project_assoc_types(&Ty::AssocType {
                base: Box::new(resolved_arg.clone()),
                trait_name: trait_name.clone().into_boxed_str(),
                assoc_name: assoc_name.clone().into_boxed_str(),
            });
            let actual = self.subst.resolve(&actual).materialize_literal_defaults();
            let expected = self
                .subst
                .resolve(expected_ty)
                .materialize_literal_defaults();
            if actual.has_inference_var() || expected.has_inference_var() || actual == expected {
                continue;
            }
            self.report_error(
                TypeErrorKind::BoundsNotSatisfied,
                span,
                format!(
                    "type `{}` does not satisfy associated type binding \
                     `{param_name}: {trait_name}<{assoc_name} = {}>`; found `{}`",
                    resolved_arg.user_facing(),
                    expected.user_facing(),
                    actual.user_facing()
                ),
            );
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
    fn diagnose_bound_failure_suggestions(&mut self, ty: &Ty, bound: &str) -> Vec<String> {
        let Ty::Named { name, .. } = ty else {
            return vec![];
        };
        // Normalize module-qualified names (mirrors type_structurally_satisfies).
        let type_name: String = {
            let uq = self.strip_module_qualifier(name);
            match uq {
                Some(uq) if self.type_defs.contains_key(uq) => uq.to_string(),
                _ => name.clone(),
            }
        };
        let trait_name: String = {
            let uq = self.strip_module_qualifier(bound);
            match uq {
                Some(uq) if self.trait_defs.contains_key(uq) => uq.to_string(),
                _ => bound.to_string(),
            }
        };

        let Some(trait_info) = self.trait_defs.get(&trait_name).cloned() else {
            return vec![];
        };

        // E1 guard: associated types require an explicit impl alias scope.
        if !trait_info.associated_types.is_empty() {
            return vec![format!(
                "trait `{trait_name}` requires an explicit `impl` declaration \
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
                "trait `{trait_name}` requires an explicit `impl` declaration \
                 (it has generic methods)"
            )];
        }

        // Collect required methods; also catches E1 guards deep in the super-trait chain.
        let Some(required) = self.collect_structural_required_methods(&trait_name, &mut Vec::new())
        else {
            return vec![format!(
                "trait `{trait_name}` requires an explicit `impl` declaration \
                 (a super-trait declares associated types or generic methods)"
            )];
        };

        // A trait with no required methods (all defaults or empty) still needs an explicit impl.
        if required.is_empty() {
            return vec![format!(
                "trait `{trait_name}` has no required methods — add an explicit \
                 `impl {trait_name} for {type_name}` declaration"
            )];
        }

        let concrete_ty = Ty::Named {
            builtin: None,
            name: type_name.clone(),
            args: vec![],
        };
        let mut missing: Vec<String> = Vec::new();

        for method_name in &required {
            let Some(trait_sig) = self.lookup_trait_method(&trait_name, method_name) else {
                continue;
            };

            let Some(type_sig) =
                shared_lookup_method_sig(&self.type_defs, &self.fn_sigs, &concrete_ty, method_name)
            else {
                missing.push(format!("`{method_name}`"));
                continue;
            };

            // Arity mismatch — return on first found.
            if trait_sig.params.len() != type_sig.params.len() {
                return vec![format!(
                    "`{type_name}::{method_name}` has {} parameter(s) but trait \
                     `{trait_name}` requires {} — arity mismatch",
                    type_sig.params.len(),
                    trait_sig.params.len(),
                )];
            }

            // Return-type mismatch.
            let expected_ret = trait_sig
                .return_type
                .substitute_named_param("Self", &concrete_ty);
            if expected_ret != type_sig.return_type {
                return vec![format!(
                    "`{type_name}::{method_name}` returns `{}` but trait `{trait_name}` \
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
                let expected = trait_param.substitute_named_param("Self", &concrete_ty);
                if expected != *type_param {
                    return vec![format!(
                        "`{type_name}::{method_name}` parameter {} has type `{}` but \
                         trait `{trait_name}` requires `{}` — type mismatch",
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
                "`{type_name}` is missing method(s) required by trait `{trait_name}`: {list}"
            )];
        }

        vec![]
    }

    pub(super) fn type_satisfies_trait_bound(&mut self, ty: &Ty, trait_name: &str) -> bool {
        match ty {
            Ty::Named {
                builtin: Some(crate::BuiltinType::Generator | crate::BuiltinType::AsyncGenerator),
                args,
                ..
            } if trait_name == "Iterator" && !args.is_empty() => true,
            Ty::Named { name, .. } => {
                let name = name.clone();
                if self.type_implements_trait(&name, trait_name)
                    || self.type_structurally_satisfies(&name, trait_name)
                {
                    return true;
                }
                // The arg might be a type parameter of the enclosing function
                // (e.g. `T` in `fn show<T: Display>(...)`). When the function's
                // where-clause declares a matching bound, the parameter
                // satisfies it abstractly — even though `T` is not a concrete
                // type with a registered impl.
                self.type_param_carries_bound(&name, trait_name)
            }
            Ty::TraitObject { traits } => {
                let matched = traits.iter().any(|t| {
                    t.trait_name == trait_name || self.trait_extends(&t.trait_name, trait_name)
                });
                matched
            }
            // For primitives and other built-in types: first consult the
            // user-defined impl registry (`trait_impls_set`), keyed by the
            // canonical lowering name (e.g. `"i64"`, `"bool"`).  An `impl
            // UserTrait for i64` records `("i64", "UserTrait")` in that set via
            // `record_trait_impl`; without this check, user-trait bounds on
            // primitives were always rejected even when an impl existed.
            //
            // Literal kinds (`IntLiteral`, `FloatLiteral`) default to `i64`/`f64`
            // so that bounds can be satisfied for literal call-site args whose
            // type is not yet fully materialized (the deferred path materializes
            // first, but the immediate path may reach here with a literal kind).
            //
            // The `MarkerTrait` table is consulted second so that built-in
            // structural markers (e.g. `Ord`, `Clone`) continue to work without
            // requiring an explicit `impl` in the source.
            _ => {
                let canonical = ty.canonical_lowering_name().or(match ty {
                    Ty::IntLiteral => Some("i64"),
                    Ty::FloatLiteral => Some("f64"),
                    _ => None,
                });
                if let Some(name) = canonical {
                    if self.type_implements_trait(name, trait_name) {
                        return true;
                    }
                }
                if let Some(marker) = MarkerTrait::from_name(trait_name) {
                    self.registry.implements_marker(ty, marker)
                } else {
                    false
                }
            }
        }
    }

    /// Check whether a type parameter `param_name` of the current function (or
    /// its enclosing impl) carries `trait_name` as a where-clause bound, either
    /// directly or via super-trait extension.
    pub(super) fn type_param_carries_bound(&self, param_name: &str, trait_name: &str) -> bool {
        // Consult the active bounds stack first (covers machine transition
        // bodies, impl-method scopes, and any other context that pushes
        // `current_type_param_bounds` without setting `current_function`).
        for frame in self.current_type_param_bounds.iter().rev() {
            if let Some(bounds) = frame.bounds.get(param_name) {
                if bounds
                    .iter()
                    .any(|b| b == trait_name || self.trait_extends(b, trait_name))
                {
                    return true;
                }
            }
        }
        let Some(fn_name) = self.current_function.as_ref() else {
            return false;
        };
        let Some(sig) = self.fn_sigs.get(fn_name) else {
            return false;
        };
        if !sig.type_params.contains(&param_name.to_string()) {
            return false;
        }
        let Some(bounds) = sig.type_param_bounds.get(param_name) else {
            return false;
        };
        bounds
            .iter()
            .any(|b| b == trait_name || self.trait_extends(b, trait_name))
    }

    /// Check if a concrete type implements a trait (directly or via super-trait chain).
    pub(super) fn type_implements_trait(&self, type_name: &str, trait_name: &str) -> bool {
        // Direct impl
        if self
            .trait_impls_set
            .contains(&(type_name.to_string(), trait_name.to_string()))
        {
            return true;
        }
        // Strip known module prefix: "json.Value" → "Value"
        let unqualified_type = type_name.find('.').and_then(|dot| {
            let prefix = &type_name[..dot];
            if self.modules.contains(prefix) {
                Some(&type_name[dot + 1..])
            } else {
                None
            }
        });
        if let Some(uq) = unqualified_type {
            if self
                .trait_impls_set
                .contains(&(uq.to_string(), trait_name.to_string()))
            {
                return true;
            }
        }
        // Also try unqualified trait name
        let unqualified_trait = trait_name.find('.').and_then(|dot| {
            let prefix = &trait_name[..dot];
            if self.modules.contains(prefix) {
                Some(&trait_name[dot + 1..])
            } else {
                None
            }
        });
        if let Some(uq_trait) = unqualified_trait {
            let tn = unqualified_type.unwrap_or(type_name);
            if self
                .trait_impls_set
                .contains(&(tn.to_string(), uq_trait.to_string()))
            {
                return true;
            }
        }
        // Check if type implements a sub-trait that extends this trait
        let effective_type = unqualified_type.unwrap_or(type_name);
        for (tn, tn_trait) in &self.trait_impls_set {
            if (tn == type_name || tn.as_str() == effective_type)
                && self.trait_extends(tn_trait, trait_name)
            {
                return true;
            }
        }
        false
    }

    /// Check if `child_trait` transitively extends `parent_trait`.
    pub(super) fn trait_extends(&self, child_trait: &str, parent_trait: &str) -> bool {
        self.trait_extends_inner(child_trait, parent_trait, &mut HashSet::new())
    }

    fn trait_extends_inner(
        &self,
        child_trait: &str,
        parent_trait: &str,
        visited: &mut HashSet<String>,
    ) -> bool {
        if !visited.insert(child_trait.to_string()) {
            return false;
        }
        if let Some(supers) = self.trait_super.get(child_trait) {
            for s in supers {
                if s == parent_trait || self.trait_extends_inner(s, parent_trait, visited) {
                    return true;
                }
            }
        }
        false
    }

    /// Strip a known module qualifier from a name, e.g. `"json.Value"` → `"Value"`.
    /// Returns `None` if the prefix is not a known module or there is no dot.
    pub(super) fn strip_module_qualifier<'a>(&self, name: &'a str) -> Option<&'a str> {
        let dot = name.find('.')?;
        let prefix = &name[..dot];
        if self.modules.contains(prefix) {
            Some(&name[dot + 1..])
        } else {
            None
        }
    }

    /// Collect all required (non-default, non-generic-method) method names from
    /// `trait_name` and its effective super-trait surface.
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
        trait_name: &str,
        visited: &mut Vec<String>,
    ) -> Option<Vec<String>> {
        let surface = self.collect_structural_method_surface(trait_name, visited)?;
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
        trait_name: &str,
        visited: &mut Vec<String>,
    ) -> Option<HashMap<String, StructuralMethodStatus>> {
        if visited.iter().any(|v| v == trait_name) {
            return Some(HashMap::new());
        }
        visited.push(trait_name.to_string());

        let trait_info = self.trait_defs.get(trait_name)?.clone();

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
            declared_here.insert(method.name.clone());
            let status = if method.body.is_none() {
                StructuralMethodStatus::Required
            } else {
                StructuralMethodStatus::Provided
            };
            surface.insert(method.name.clone(), status);
        }

        // Walk super-traits — clone to release borrow. Each branch gets its own
        // visited path so sibling super-traits can still observe the same
        // ancestor. Their effective surfaces are merged back together here so
        // sibling shadowing/default coverage is preserved at the parent trait.
        let supers: Vec<String> = self
            .trait_super
            .get(trait_name)
            .cloned()
            .unwrap_or_default();
        for super_trait in &supers {
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
    /// Both `type_name` and `trait_name` are normalised: a module-qualified name such
    /// as `"json.Value"` or `"fmt.Display"` is resolved to its unqualified form before
    /// lookup so that the structural path behaves consistently with the nominal path
    /// in `type_implements_trait`.
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
    pub(super) fn type_structurally_satisfies(
        &mut self,
        type_name: &str,
        trait_name: &str,
    ) -> bool {
        // Normalize module-qualified names so "json.Value" is treated the same as
        // "Value" when the structural registry is keyed on the unqualified form.
        // Convert to owned strings immediately so the shared borrow on self ends
        // before any &mut self calls below.
        let trait_name: String = {
            let uq = self.strip_module_qualifier(trait_name);
            match uq {
                Some(uq) if self.trait_defs.contains_key(uq) => uq.to_string(),
                _ => trait_name.to_string(),
            }
        };
        let type_name: String = {
            let uq = self.strip_module_qualifier(type_name);
            match uq {
                Some(uq) if self.type_defs.contains_key(uq) => uq.to_string(),
                _ => type_name.to_string(),
            }
        };

        // Collect required methods across the full super-trait chain.
        // Returns None if any E1 guard triggers anywhere in the chain.
        let Some(required) = self.collect_structural_required_methods(&trait_name, &mut Vec::new())
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
        let concrete_ty = Ty::Named {
            builtin: None,
            name: type_name.clone(),
            args: vec![],
        };

        for method_name in &required {
            // Resolve the trait method's expected signature.
            // lookup_trait_method strips the receiver and walks super-traits.
            let Some(trait_sig) = self.lookup_trait_method(&trait_name, method_name) else {
                return false;
            };

            // Look up the concrete type's method using the shared builtin-aware
            // resolver so imported stdlib stubs cannot shadow intrinsic surfaces.
            let Some(type_sig) =
                shared_lookup_method_sig(&self.type_defs, &self.fn_sigs, &concrete_ty, method_name)
            else {
                return false;
            };

            // Arity check: non-receiver param counts must match.
            if trait_sig.params.len() != type_sig.params.len() {
                return false;
            }

            // Return-type check (Self → concrete type in trait side).
            let expected_ret = trait_sig
                .return_type
                .substitute_named_param("Self", &concrete_ty);
            if expected_ret != type_sig.return_type {
                return false;
            }

            // Per-parameter type check (Self → concrete type in trait side).
            for (trait_param, type_param) in trait_sig.params.iter().zip(type_sig.params.iter()) {
                let expected = trait_param.substitute_named_param("Self", &concrete_ty);
                if expected != *type_param {
                    return false;
                }
            }
        }

        true
    }

    /// Look up a method on a trait, walking super-traits if needed.
    /// Returns a `FnSig` with the receiver filtered out.
    pub(super) fn lookup_trait_method(&mut self, trait_name: &str, method: &str) -> Option<FnSig> {
        self.lookup_trait_method_inner(trait_name, method, true)
    }

    /// Look up a method on a trait, optionally keeping the receiver parameter.
    /// `skip_receiver` = true gives the form used by method-call syntax;
    /// `skip_receiver` = false gives the full signature for qualified calls.
    pub(super) fn lookup_trait_method_inner(
        &mut self,
        trait_name: &str,
        method: &str,
        skip_receiver: bool,
    ) -> Option<FnSig> {
        // Check the trait's own methods — clone data to release borrow before resolve_type_expr
        let found_method = self
            .trait_defs
            .get(trait_name)
            .and_then(|info| info.methods.iter().find(|m| m.name == method).cloned());
        if let Some(m) = found_method {
            let skip = if skip_receiver {
                usize::from(m.params.first().is_some_and(|p| self.is_receiver_param(p)))
            } else {
                0
            };
            // Activate `Self::Bar` projection so the resolver materialises
            // a `Ty::AssocType` carrier for the trait method's signature.
            let prev_trait_self = self
                .current_trait_for_self_projection
                .replace(trait_name.to_string());
            let params: Vec<Ty> = m
                .params
                .iter()
                .skip(skip)
                .map(|p| self.resolve_type_expr(&p.ty))
                .collect();
            let return_type = m
                .return_type
                .as_ref()
                .map_or(Ty::Unit, |annotation| self.resolve_type_expr(annotation));
            self.current_trait_for_self_projection = prev_trait_self;
            let param_names: Vec<String> =
                m.params.iter().skip(skip).map(|p| p.name.clone()).collect();
            let type_params = m
                .type_params
                .as_ref()
                .map(|params| params.iter().map(|tp| tp.name.clone()).collect())
                .unwrap_or_default();
            let type_param_bounds =
                self.collect_type_param_bounds(m.type_params.as_ref(), m.where_clause.as_ref());
            // W3.042 S2-S4: propagate `requires_mutable_receiver` from the
            // trait declaration's receiver parameter so the dyn-trait
            // dispatch gate in `(Ty::TraitObject, _)` (and any other
            // consumer that reads the substituted `FnSig`) sees the flag.
            // The substituted `FnSig` is recorded on `DynMethodCall` and
            // travels with the vtable slot resolution; without this the
            // flag is permanently cleared at the trait-method-lookup
            // boundary and the dyn dispatch gate cannot fire.
            let requires_mutable_receiver = m
                .params
                .first()
                .is_some_and(|p| self.is_receiver_param(p) && p.is_mutable);
            return Some(FnSig {
                type_params,
                type_param_bounds,
                param_names,
                params,
                return_type,
                requires_mutable_receiver,
                ..FnSig::default()
            });
        }
        // Walk super-traits — clone to release borrow
        let supers = self.trait_super.get(trait_name).cloned();
        if let Some(supers) = supers {
            for super_trait in &supers {
                if let Some(sig) =
                    self.lookup_trait_method_inner(super_trait, method, skip_receiver)
                {
                    return Some(sig);
                }
            }
        }
        None
    }

    /// Like `lookup_trait_method` but returns the *declaring* trait name alongside
    /// the signature. The declaring trait is the trait whose `trait_defs` entry
    /// directly contains the method (not a supertrait walk result).
    ///
    /// Used by static trait dispatch to distinguish a method declared in trait A
    /// but reached through bound B (where `trait B: A`).
    pub(super) fn lookup_trait_method_with_origin(
        &mut self,
        trait_name: &str,
        method: &str,
    ) -> Option<(String, FnSig)> {
        self.lookup_trait_method_with_origin_inner(trait_name, method, true)
    }

    /// Walk `trait_name` and ALL of its (transitive) supertraits, collecting
    /// every trait that DIRECTLY declares a method named `method` in its
    /// `trait_defs` entry. The returned `Vec` is sorted + deduplicated so
    /// repeated bound paths collapse to a stable set.
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
        trait_name: &str,
        method: &str,
    ) -> Vec<String> {
        let mut out: Vec<String> = Vec::new();
        let mut stack: Vec<String> = vec![trait_name.to_string()];
        let mut visited: std::collections::HashSet<String> = std::collections::HashSet::new();
        while let Some(current) = stack.pop() {
            if !visited.insert(current.clone()) {
                continue;
            }
            let declares_directly = self
                .trait_defs
                .get(&current)
                .is_some_and(|info| info.methods.iter().any(|m| m.name == method));
            if declares_directly {
                out.push(current.clone());
            }
            if let Some(supers) = self.trait_super.get(&current) {
                for s in supers {
                    stack.push(s.clone());
                }
            }
        }
        out.sort();
        out.dedup();
        out
    }

    fn lookup_trait_method_with_origin_inner(
        &mut self,
        trait_name: &str,
        method: &str,
        skip_receiver: bool,
    ) -> Option<(String, FnSig)> {
        // Check the trait's own methods first (direct declaration).
        let found_method = self
            .trait_defs
            .get(trait_name)
            .and_then(|info| info.methods.iter().find(|m| m.name == method).cloned());
        if let Some(m) = found_method {
            let skip = if skip_receiver {
                usize::from(m.params.first().is_some_and(|p| self.is_receiver_param(p)))
            } else {
                0
            };
            let prev_trait_self = self
                .current_trait_for_self_projection
                .replace(trait_name.to_string());
            let params: Vec<Ty> = m
                .params
                .iter()
                .skip(skip)
                .map(|p| self.resolve_type_expr(&p.ty))
                .collect();
            let return_type = m
                .return_type
                .as_ref()
                .map_or(Ty::Unit, |annotation| self.resolve_type_expr(annotation));
            self.current_trait_for_self_projection = prev_trait_self;
            let param_names: Vec<String> =
                m.params.iter().skip(skip).map(|p| p.name.clone()).collect();
            let type_params = m
                .type_params
                .as_ref()
                .map(|params| params.iter().map(|tp| tp.name.clone()).collect())
                .unwrap_or_default();
            let type_param_bounds =
                self.collect_type_param_bounds(m.type_params.as_ref(), m.where_clause.as_ref());
            // This trait directly declares the method — it IS the declaring trait.
            // W3.042 S2-S4: propagate `requires_mutable_receiver` from the
            // trait declaration's receiver parameter so the static-dispatch
            // gate in `(Ty::Named, _)`'s generic-bound sub-arm can consult
            // the substituted `trait_sig.requires_mutable_receiver` flag.
            // Without this, `fn step(var self)` on the trait reaches the
            // dispatch site with the flag cleared and the call site
            // erroneously admits a `let`-bound receiver.
            let requires_mutable_receiver = m
                .params
                .first()
                .is_some_and(|p| self.is_receiver_param(p) && p.is_mutable);
            return Some((
                trait_name.to_string(),
                FnSig {
                    type_params,
                    type_param_bounds,
                    param_names,
                    params,
                    return_type,
                    requires_mutable_receiver,
                    ..FnSig::default()
                },
            ));
        }
        // Walk super-traits — propagate origin unchanged from the recursion.
        let supers = self.trait_super.get(trait_name).cloned();
        if let Some(supers) = supers {
            for super_trait in &supers {
                if let Some(result) =
                    self.lookup_trait_method_with_origin_inner(super_trait, method, skip_receiver)
                {
                    return Some(result);
                }
            }
        }
        None
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
                Ty::Named { name, args, .. } if name == "Self" && args.is_empty()
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
            Err(super::const_eval::ConstEvalError::Overflow) => {
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
