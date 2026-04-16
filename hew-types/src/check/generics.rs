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
            for (tp, ta) in sig.type_params.iter().zip(resolved_type_args.iter()) {
                params = params
                    .iter()
                    .map(|param| param.substitute_named_param(tp, ta))
                    .collect();
                ret = ret.substitute_named_param(tp, ta);
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
                let msg = format!(
                    "type `{}` does not implement trait `{bound}` required by `{param_name}`",
                    resolved_arg.user_facing()
                );
                let suggestions = self.diagnose_bound_failure_suggestions(&resolved_arg, bound);
                self.report_error_with_suggestions(
                    TypeErrorKind::BoundsNotSatisfied,
                    span,
                    msg,
                    suggestions,
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
            let param_names: Vec<String> =
                m.params.iter().skip(skip).map(|p| p.name.clone()).collect();
            let type_params = m
                .type_params
                .as_ref()
                .map(|params| params.iter().map(|tp| tp.name.clone()).collect())
                .unwrap_or_default();
            let type_param_bounds =
                self.collect_type_param_bounds(m.type_params.as_ref(), m.where_clause.as_ref());
            return Some(FnSig {
                type_params,
                type_param_bounds,
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
}
