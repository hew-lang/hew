#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::method_resolution::lookup_method_sig as shared_lookup_method_sig;

impl Checker {
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
            Ty::Named { name, args } => Ty::Named {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|a| self.freshen_inner(a, mapping))
                    .collect(),
            },
            Ty::Tuple(ts) => Ty::Tuple(ts.iter().map(|t| self.freshen_inner(t, mapping)).collect()),
            Ty::Array(inner, n) => Ty::Array(Box::new(self.freshen_inner(inner, mapping)), *n),
            Ty::Slice(inner) => Ty::Slice(Box::new(self.freshen_inner(inner, mapping))),
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
                    .map(|(te, _)| self.resolve_type_expr(te))
                    .collect::<Vec<_>>()
            });
            while resolved_type_args.len() < sig.type_params.len() {
                resolved_type_args.push(Ty::Var(TypeVar::fresh()));
            }
            for (tp, ta) in sig.type_params.iter().zip(resolved_type_args.iter()) {
                params = params
                    .iter()
                    .map(|param| self.substitute_named_param(param, tp, ta))
                    .collect();
                ret = self.substitute_named_param(&ret, tp, ta);
            }
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
                    self.subst.insert(*v, fresh_ty.clone());
                }
            }
        }

        (freshened_params, freshened_ret, resolved_type_args)
    }

    pub(super) fn enforce_type_param_bounds(&mut self, sig: &FnSig, type_args: &[Ty], span: &Span) {
        if sig.type_params.is_empty() {
            return;
        }
        for (idx, param_name) in sig.type_params.iter().enumerate() {
            let Some(bounds) = sig.type_param_bounds.get(param_name) else {
                continue;
            };
            let Some(type_arg) = type_args.get(idx) else {
                continue;
            };
            let resolved_arg = self.subst.resolve(type_arg);
            for bound in bounds {
                if self.type_satisfies_trait_bound(&resolved_arg, bound) {
                    continue;
                }
                self.report_error(
                    TypeErrorKind::BoundsNotSatisfied,
                    span,
                    format!(
                        "type `{}` does not implement trait `{bound}` required by `{param_name}`",
                        resolved_arg.user_facing()
                    ),
                );
            }
        }
    }

    pub(super) fn type_satisfies_trait_bound(&mut self, ty: &Ty, trait_name: &str) -> bool {
        match ty {
            Ty::Named { name, .. } => {
                let name = name.clone();
                self.type_implements_trait(&name, trait_name)
                    || self.type_structurally_satisfies(&name, trait_name)
            }
            Ty::TraitObject { traits } => {
                let matched = traits.iter().any(|t| {
                    t.trait_name == trait_name || self.trait_extends(&t.trait_name, trait_name)
                });
                matched
            }
            // For primitives and other built-in types, delegate to the marker-trait table which
            // already knows which built-ins satisfy which markers (e.g. i32 satisfies Ord).
            _ => {
                if let Some(marker) = MarkerTrait::from_name(trait_name) {
                    self.registry.implements_marker(ty, marker)
                } else {
                    false
                }
            }
        }
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
        if let Some(supers) = self.trait_super.get(child_trait) {
            for s in supers {
                if s == parent_trait || self.trait_extends(s, parent_trait) {
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
    /// `trait_name` and its entire super-trait chain.
    ///
    /// Returns `None` if any trait in the chain has associated types or generic
    /// methods (the E1 guards), which disqualifies the whole structural check.
    pub(super) fn collect_structural_required_methods(
        &self,
        trait_name: &str,
        visited: &mut Vec<String>,
    ) -> Option<Vec<String>> {
        if visited.iter().any(|v| v == trait_name) {
            return Some(vec![]);
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

        let mut required: Vec<String> = trait_info
            .methods
            .iter()
            .filter(|m| m.body.is_none())
            .map(|m| m.name.clone())
            .collect();

        // Walk super-traits — clone to release borrow.
        let supers: Vec<String> = self
            .trait_super
            .get(trait_name)
            .cloned()
            .unwrap_or_default();
        for super_trait in &supers {
            let super_required = self.collect_structural_required_methods(super_trait, visited)?;
            for m in super_required {
                if !required.contains(&m) {
                    required.push(m);
                }
            }
        }

        Some(required)
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
    /// `dyn Trait` / vtable / codegen is explicitly out of scope.
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
            let expected_ret =
                self.substitute_named_param(&trait_sig.return_type, "Self", &concrete_ty);
            if expected_ret != type_sig.return_type {
                return false;
            }

            // Per-parameter type check (Self → concrete type in trait side).
            for (trait_param, type_param) in trait_sig.params.iter().zip(type_sig.params.iter()) {
                let expected = self.substitute_named_param(trait_param, "Self", &concrete_ty);
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
            let params: Vec<Ty> = m
                .params
                .iter()
                .skip(skip)
                .map(|p| self.resolve_type_expr(&p.ty.0))
                .collect();
            let return_type = m
                .return_type
                .as_ref()
                .map_or(Ty::Unit, |(te, _)| self.resolve_type_expr(te));
            let param_names: Vec<String> =
                m.params.iter().skip(skip).map(|p| p.name.clone()).collect();
            return Some(FnSig {
                param_names,
                params,
                return_type,
                is_pure: m.is_pure,
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

    /// Substitute a named type parameter (e.g. `T`) with a concrete type in a type expression.
    /// Used to resolve generic fields/methods on instantiated types.
    #[expect(
        clippy::self_only_used_in_recursion,
        reason = "method for consistency with other type helpers"
    )]
    pub(super) fn substitute_named_param(&self, ty: &Ty, param_name: &str, replacement: &Ty) -> Ty {
        match ty {
            Ty::Named { name, args } if args.is_empty() && name == param_name => {
                replacement.clone()
            }
            Ty::Named { name, args } => Ty::Named {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|a| self.substitute_named_param(a, param_name, replacement))
                    .collect(),
            },
            Ty::Tuple(elems) => Ty::Tuple(
                elems
                    .iter()
                    .map(|e| self.substitute_named_param(e, param_name, replacement))
                    .collect(),
            ),
            Ty::Array(inner, n) => Ty::Array(
                Box::new(self.substitute_named_param(inner, param_name, replacement)),
                *n,
            ),
            Ty::Slice(inner) => Ty::Slice(Box::new(self.substitute_named_param(
                inner,
                param_name,
                replacement,
            ))),
            Ty::Function { params, ret } => Ty::Function {
                params: params
                    .iter()
                    .map(|p| self.substitute_named_param(p, param_name, replacement))
                    .collect(),
                ret: Box::new(self.substitute_named_param(ret, param_name, replacement)),
            },
            Ty::Closure {
                params,
                ret,
                captures,
            } => Ty::Closure {
                params: params
                    .iter()
                    .map(|p| self.substitute_named_param(p, param_name, replacement))
                    .collect(),
                ret: Box::new(self.substitute_named_param(ret, param_name, replacement)),
                captures: captures
                    .iter()
                    .map(|c| self.substitute_named_param(c, param_name, replacement))
                    .collect(),
            },
            Ty::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(self.substitute_named_param(pointee, param_name, replacement)),
            },
            Ty::TraitObject { traits } => Ty::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| crate::ty::TraitObjectBound {
                        trait_name: bound.trait_name.clone(),
                        args: bound
                            .args
                            .iter()
                            .map(|a| self.substitute_named_param(a, param_name, replacement))
                            .collect(),
                    })
                    .collect(),
            },
            _ => ty.clone(),
        }
    }
}
