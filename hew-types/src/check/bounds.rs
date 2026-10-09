//! Trait bounds on generic binders: resolved once where they are written,
//! keyed by the binder's `TypeParamId` and the trait's `DefId`.

#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

impl Checker {
    /// Resolve the bounds written on a declaration's binders, inline
    /// (`<T: Display>`) and in its where-clause (`where T: Display`).
    ///
    /// A where-clause may bound an enclosing declaration's binder (a method
    /// bounding its impl's `T`); the bound is keyed by that binder. Holes in
    /// associated-type bindings join `hole_vars`.
    pub(in crate::check) fn collect_type_param_bounds(
        &mut self,
        type_params: Option<&Vec<TypeParam>>,
        where_clause: Option<&WhereClause>,
        hole_vars: &mut Vec<TypeVar>,
    ) -> ParamBounds {
        let mut written: Vec<(crate::ParamHead, &TraitBound)> = Vec::new();
        if let Some(module) = self.current_declaration_module() {
            for param in type_params.into_iter().flatten() {
                for bound in &param.bounds {
                    let Some((_, span)) = bound.path.segments.first() else {
                        continue;
                    };
                    if let Some(id) = self.scopes.source_type_parameter(module, span, param.name) {
                        written.push((crate::ParamHead::new(id, param.name.name), bound));
                    }
                }
            }
        }
        for predicate in where_clause
            .into_iter()
            .flat_map(|clause| &clause.predicates)
        {
            let TypeExpr::Named { path, type_args } = &predicate.ty.0 else {
                continue;
            };
            if type_args.as_ref().is_some_and(|args| !args.is_empty()) {
                continue;
            }
            let Some(super::scope::Resolution::Param(id)) = self.resolve_type_path(path) else {
                continue;
            };
            let Some((name, _)) = path.segments.last() else {
                continue;
            };
            for bound in &predicate.bounds {
                written.push((crate::ParamHead::new(id, name.name), bound));
            }
        }
        // Traits and arguments first; associated-type bindings may project
        // through another binder's bounds (`Item = J.Item`), so they resolve
        // with the declaration's bounds in scope.
        let mut bounds = ParamBounds::default();
        let mut resolved = Vec::new();
        for (subject, bound) in written {
            if let Some(trait_ref) = self.resolve_written_bound(subject.spelling, bound) {
                bounds.push(subject.id, trait_ref.clone());
                resolved.push((subject.id, trait_ref, bound));
            }
        }
        if resolved
            .iter()
            .all(|(_, _, bound)| bound.assoc_type_bindings.is_empty())
        {
            return bounds;
        }
        self.current_type_param_bounds.push(bounds);
        let mut with_assoc = ParamBounds::default();
        for (param, mut trait_ref, bound) in resolved {
            trait_ref.assoc = bound
                .assoc_type_bindings
                .iter()
                .map(|binding| {
                    (
                        binding.name.name,
                        self.resolve_registered_annotation_ty(&binding.ty, hole_vars),
                    )
                })
                .collect();
            with_assoc.push(param, trait_ref);
        }
        self.current_type_param_bounds.pop();
        with_assoc
    }

    /// The trait and arguments one written bound on `subject` names, or
    /// `None` after reporting a bound that names no trait or misapplies one.
    pub(in crate::check) fn resolve_written_bound(
        &mut self,
        subject: Symbol,
        bound: &TraitBound,
    ) -> Option<TraitRef> {
        let span = bound
            .path
            .segments
            .last()
            .map_or(0..0, |(_, span)| span.clone());
        let Some(trait_id) = self.resolve_trait_path(&bound.path) else {
            self.report_unknown_bound(subject, bound, &span);
            return None;
        };
        let args: Vec<Ty> = bound
            .type_args
            .iter()
            .flatten()
            .map(|arg| self.resolve_type_expr(arg))
            .collect();
        if !args.is_empty() {
            let declared = self
                .trait_defs
                .get(&trait_id)
                .map_or(0, |info| info.type_params.len());
            if args.len() != declared {
                self.report_misapplied_bound(subject, bound, trait_id, declared, &span);
                return None;
            }
        }
        Some(TraitRef {
            trait_id,
            args,
            assoc: Vec::new(),
        })
    }

    /// Report a bound path that names no trait, once per occurrence.
    ///
    /// TRANSITION(A1c4): WHY stdlib registration resolves signatures before
    /// every importer scope exists, so a std bound can be unresolvable there
    /// and is dropped silently. WHEN registration resolves after every import
    /// is bound, the stdlib reports like any other source. WHAT: one
    /// resolution pass over bound scopes.
    fn report_unknown_bound(&mut self, subject: Symbol, bound: &TraitBound, span: &Span) {
        let std_source = self
            .current_module
            .as_deref()
            .is_some_and(|module| self.checking_canonical_stdlib_source(module));
        if self.in_stdlib_registration || std_source {
            return;
        }
        if !self
            .reported_unknown_bounds
            .insert(SpanKey::in_module(span, self.current_module_idx))
        {
            return;
        }
        let written = bound.path.to_string();
        let candidates: Vec<&str> = self
            .trait_defs
            .keys()
            .map(|id| self.defs.display(*id))
            .collect();
        let similar = crate::error::find_similar(&written, candidates);
        self.report_error_with_suggestions(
            TypeErrorKind::UndefinedType,
            span,
            format!("unknown trait `{written}` in bound on `{subject}`"),
            similar,
        );
    }

    /// Report positional arguments a trait does not declare, once per
    /// occurrence. A compiler predicate (`Eq`) takes none.
    fn report_misapplied_bound(
        &mut self,
        subject: Symbol,
        bound: &TraitBound,
        trait_id: crate::DefId,
        declared: usize,
        span: &Span,
    ) {
        if !self
            .reported_unknown_bounds
            .insert(SpanKey::in_module(span, self.current_module_idx))
        {
            return;
        }
        let written = bound.path.to_string();
        let message = if declared == 0 {
            format!(
                "trait bound `{written}` on type parameter `{}` carries positional type \
                 arguments, which `{}` does not declare; use associated-type bindings \
                 (`Trait<Assoc = Ty>`) instead",
                subject,
                self.defs.display(trait_id)
            )
        } else {
            format!(
                "trait `{}` takes {declared} type argument(s) but the bound on `{}` supplies {}",
                self.defs.display(trait_id),
                subject,
                bound.type_args.as_ref().map_or(0, Vec::len)
            )
        };
        self.report_error(
            TypeErrorKind::UnknownTraitBoundShape {
                trait_name: written,
            },
            span,
            message,
        );
    }

    /// The signature whose body is being checked.
    pub(in crate::check) fn checking_signature(&self) -> Option<&FnSig> {
        self.checking_declaration
            .and_then(|declaration| self.fn_sigs.get(&declaration))
            .or_else(|| {
                self.current_function
                    .as_deref()
                    .and_then(|name| self.fn_sig(name))
            })
    }

    /// The bounds in force on `param` here: every enclosing declaration's
    /// frame and the signature whose body is being checked.
    pub(in crate::check) fn active_bounds_of(&self, param: crate::TypeParamId) -> Vec<TraitRef> {
        let mut found: Vec<TraitRef> = Vec::new();
        let frames = self.current_type_param_bounds.iter();
        let signature = self.checking_signature().map(|sig| &sig.bounds);
        for bounds in frames.chain(signature) {
            for bound in bounds.of(param) {
                if !found.contains(bound) {
                    found.push(bound.clone());
                }
            }
        }
        found
    }

    /// Every bound in force here, for a check replayed later in another
    /// scope.
    pub(in crate::check) fn active_param_bounds(&self) -> ParamBounds {
        let mut bounds = ParamBounds::default();
        for frame in &self.current_type_param_bounds {
            bounds.extend(frame);
        }
        if let Some(sig) = self.checking_signature() {
            bounds.extend(&sig.bounds);
        }
        bounds
    }

    /// A bound as the program wrote it, for diagnostics.
    pub(in crate::check) fn trait_ref_display(&self, bound: &TraitRef) -> String {
        let name = self.defs.display(bound.trait_id);
        let mut parts: Vec<String> = bound
            .args
            .iter()
            .map(|arg| arg.user_facing().to_string())
            .collect();
        parts.extend(
            bound
                .assoc
                .iter()
                .map(|(assoc, ty)| format!("{assoc} = {}", ty.user_facing())),
        );
        if parts.is_empty() {
            name.to_string()
        } else {
            format!("{name}<{}>", parts.join(", "))
        }
    }

    /// The trait a language item names.
    pub(in crate::check) fn lang_trait(&self, item: crate::LangItem) -> Option<crate::DefId> {
        self.lang_items
            .get(item.key())
            .map(|binding| binding.trait_id)
    }

    /// The compiler marker a trait is: a predicate row, or the prelude's
    /// `Display` declaration. A declared trait of a predicate's spelling is an
    /// ordinary trait (R2).
    pub(in crate::check) fn trait_marker(&self, trait_id: crate::DefId) -> Option<MarkerTrait> {
        if let Some(predicate) = crate::DefTable::as_predicate(trait_id) {
            return Some(MarkerTrait::of_predicate(predicate));
        }
        (self.lang_trait(crate::LangItem::Display) == Some(trait_id))
            .then_some(MarkerTrait::Display)
    }

    /// Whether `child` transitively extends `parent`.
    pub(in crate::check) fn trait_extends(
        &self,
        child: crate::DefId,
        parent: crate::DefId,
    ) -> bool {
        let mut stack = self.trait_supers(child).to_vec();
        let mut visited = HashSet::new();
        while let Some(current) = stack.pop() {
            if current == parent {
                return true;
            }
            if visited.insert(current) {
                stack.extend_from_slice(self.trait_supers(current));
            }
        }
        false
    }

    /// `trait_id` and every trait it transitively extends.
    pub(in crate::check) fn trait_closure(&self, trait_id: crate::DefId) -> Vec<crate::DefId> {
        let mut out = vec![trait_id];
        let mut index = 0;
        while let Some(&current) = out.get(index) {
            for &parent in self.trait_supers(current) {
                if !out.contains(&parent) {
                    out.push(parent);
                }
            }
            index += 1;
        }
        out
    }

    /// Whether a bound on a binder provides `wanted`: the same trait, a trait
    /// extending it, or `Ord` for the `PartialOrd` predicate.
    pub(in crate::check) fn bound_provides(
        &self,
        bound: crate::DefId,
        wanted: crate::DefId,
    ) -> bool {
        bound == wanted
            || self.trait_extends(bound, wanted)
            || (self.trait_marker(bound) == Some(MarkerTrait::Ord)
                && self.trait_marker(wanted) == Some(MarkerTrait::PartialOrd))
    }

    /// Whether a binder in scope carries a bound providing `trait_id`.
    pub(in crate::check) fn param_carries_trait(
        &self,
        param: crate::TypeParamId,
        trait_id: crate::DefId,
    ) -> bool {
        self.active_bounds_of(param)
            .iter()
            .any(|bound| self.bound_provides(bound.trait_id, trait_id))
    }

    /// Whether a binder in scope carries a bound providing `wanted`. A bound
    /// with arguments (`From<Low>`) is provided only by a bound of the same
    /// trait whose arguments agree.
    pub(in crate::check) fn param_carries_bound(
        &mut self,
        param: crate::TypeParamId,
        wanted: &TraitRef,
    ) -> bool {
        if wanted.args.is_empty() {
            return self.param_carries_trait(param, wanted.trait_id);
        }
        let wanted_args: Vec<Ty> = wanted
            .args
            .iter()
            .map(|arg| self.subst.resolve(arg))
            .collect();
        self.active_bounds_of(param).iter().any(|carried| {
            if carried.trait_id != wanted.trait_id || carried.args.len() != wanted_args.len() {
                return false;
            }
            let snapshot = self.subst.snapshot();
            let agree = carried
                .args
                .iter()
                .zip(&wanted_args)
                .all(|(carried, wanted)| self.try_unify_with_owner_identity(carried, wanted));
            self.subst.restore(snapshot);
            agree
        })
    }

    /// Whether a binder in scope carries a bound that is `marker`.
    pub(in crate::check) fn param_carries_marker(
        &self,
        param: crate::TypeParamId,
        marker: MarkerTrait,
    ) -> bool {
        self.active_bounds_of(param)
            .iter()
            .any(|bound| self.trait_marker(bound.trait_id) == Some(marker))
    }

    /// The key an impl files its conformance under: its target with the type
    /// arguments erased.
    pub(in crate::check) fn impl_self_key(ty: &Ty) -> Option<ResolvedTy> {
        let erased = match ty {
            Ty::Named { head, .. } => Ty::named_head(*head, Vec::new()),
            Ty::IntLiteral => Ty::I64,
            Ty::FloatLiteral => Ty::F64,
            other => other.clone(),
        };
        ResolvedTy::from_ty(&erased).ok()
    }

    /// Record a declared `impl Trait<Args> for target`.
    pub(in crate::check) fn record_trait_impl(
        &mut self,
        target: &Ty,
        trait_ref: &TraitRef,
        params: Vec<crate::ParamHead>,
    ) {
        let Some(key) = Self::impl_self_key(target) else {
            return;
        };
        let row = TraitImplArgs {
            target: target.clone(),
            args: trait_ref.args.clone(),
            params,
        };
        let rows = self
            .trait_impls
            .entry((key, trait_ref.trait_id))
            .or_default();
        if !rows.contains(&row) {
            rows.push(row);
        }
    }

    /// Record the methods one impl block of `trait_id` for `target` writes.
    pub(in crate::check) fn record_trait_impl_methods(
        &mut self,
        target: &Ty,
        trait_id: crate::DefId,
        methods: impl IntoIterator<Item = Symbol>,
    ) {
        let Some(key) = Self::impl_self_key(target) else {
            return;
        };
        self.trait_impl_method_names
            .entry((key, trait_id))
            .or_default()
            .extend(methods);
    }

    /// Whether an impl of exactly `trait_id` is declared for `ty`'s head.
    pub(in crate::check) fn has_trait_impl(&self, ty: &Ty, trait_id: crate::DefId) -> bool {
        Self::impl_self_key(ty).is_some_and(|key| self.trait_impls.contains_key(&(key, trait_id)))
    }

    /// Whether `ty` has a declared impl of `trait_id`, directly or through an
    /// impl of a trait extending it. An alias answers for its target.
    pub(in crate::check) fn type_implements_trait(&self, ty: &Ty, trait_id: crate::DefId) -> bool {
        if let Some(key) = Self::impl_self_key(ty) {
            if self.trait_impls.contains_key(&(key.clone(), trait_id))
                || self.trait_impls.keys().any(|(implemented, declared)| {
                    *implemented == key && self.trait_extends(*declared, trait_id)
                })
            {
                return true;
            }
        }
        let Ty::Named { head, args } = ty else {
            return false;
        };
        self.alias_target_for_instance(*head, args)
            .is_some_and(|target| self.type_implements_trait(&target, trait_id))
    }

    /// Whether an impl of `trait_id`, or of a trait extending it, filed
    /// under `ty`'s head names `ty` itself: `impl Display for Wrap<string>`
    /// does not render a `Wrap<i64>`. A type with no arguments, or one whose
    /// head files no impl (an alias answers through its target), is decided
    /// by the head alone.
    pub(in crate::check) fn an_impl_head_admits(
        &mut self,
        ty: &Ty,
        trait_id: crate::DefId,
    ) -> bool {
        let Ty::Named { args, .. } = ty else {
            return true;
        };
        if args.is_empty() {
            return true;
        }
        let Some(key) = Self::impl_self_key(ty) else {
            return true;
        };
        let rows: Vec<TraitImplArgs> = self
            .trait_impls
            .iter()
            .filter(|((implemented, declared), _)| {
                *implemented == key
                    && (*declared == trait_id || self.trait_extends(*declared, trait_id))
            })
            .flat_map(|(_, rows)| rows.iter().cloned())
            .collect();
        if rows.is_empty() {
            return true;
        }
        rows.iter().any(|row| {
            let opened = std::cell::RefCell::new(HashMap::new());
            let target = super::coerce::open_type_params(&row.target, &opened);
            let snapshot = self.subst.snapshot();
            let matched = self.try_unify_with_owner_identity(&target, ty);
            self.subst.restore(snapshot);
            matched
        })
    }

    /// Whether `ty` has a declared impl of `bound`'s trait at `bound`'s
    /// arguments. A generic impl matches with its binders opened.
    pub(in crate::check) fn type_implements_trait_ref(
        &mut self,
        ty: &Ty,
        bound: &TraitRef,
    ) -> bool {
        if bound.args.is_empty() {
            return self.type_implements_trait(ty, bound.trait_id);
        }
        let Some(key) = Self::impl_self_key(ty) else {
            return false;
        };
        let rows = self
            .trait_impls
            .get(&(key, bound.trait_id))
            .cloned()
            .unwrap_or_default();
        let wanted: Vec<Ty> = bound
            .args
            .iter()
            .map(|arg| self.subst.resolve(arg))
            .collect();
        rows.iter().any(|row| {
            let opened = std::cell::RefCell::new(HashMap::new());
            let target = super::coerce::open_type_params(&row.target, &opened);
            let args: Vec<Ty> = row
                .args
                .iter()
                .map(|arg| super::coerce::open_type_params(arg, &opened))
                .collect();
            let snapshot = self.subst.snapshot();
            let matched = self.try_unify_with_owner_identity(&target, ty)
                && args.len() == wanted.len()
                && args
                    .iter()
                    .zip(&wanted)
                    .all(|(declared, wanted)| self.try_unify_with_owner_identity(declared, wanted));
            self.subst.restore(snapshot);
            matched
        })
    }

    /// Whether `ty` satisfies a bound: a compiler predicate by its structural
    /// rule, a declared trait by an impl (or a structural match for an
    /// argument-free trait), and a binder by its own bounds.
    pub(in crate::check) fn type_satisfies_bound(&mut self, ty: &Ty, bound: &TraitRef) -> bool {
        let marker = self.trait_marker(bound.trait_id);
        // An unresolved or already-errored type says nothing about Display or
        // Clone; reporting it would cascade onto the first failure.
        let unknown = matches!(self.subst.resolve(ty), Ty::Var(_) | Ty::Error);
        match marker {
            // One authority decides Display: the impl lookup f-string
            // interpolation already uses.
            Some(MarkerTrait::Display) => return unknown || self.display_impl_type(ty).is_some(),
            // `Clone` answers whether the type has a copy operation; a record
            // whose fields all clone has one, so no empty impl is demanded.
            Some(MarkerTrait::Clone) => {
                if unknown {
                    return true;
                }
                if let Ty::Named {
                    head: crate::TypeHead::Param(param),
                    args,
                } = &self.subst.resolve(ty)
                {
                    if args.is_empty() && self.param_carries_trait(param.id, bound.trait_id) {
                        return true;
                    }
                }
                return self
                    .parameter_clone_kind(ty)
                    .is_some_and(|clone| clone != crate::type_facts::CloneKind::None);
            }
            Some(MarkerTrait::Serializable) => return self.satisfies_serializable(ty),
            Some(MarkerTrait::Send) => return self.type_is_send(ty),
            Some(MarkerTrait::Eq) if !ty.has_type_parameters() => {
                let ty = self.normalize_for_use(ty).materialize_literal_defaults();
                return Self::selected_eq_available(
                    &mut TypeFactService::new(self.type_fact_context(), BTreeMap::new()),
                    &ty,
                );
            }
            _ => {}
        }
        match ty {
            // `instant` canonicalises to i64 and satisfies what i64 does.
            Ty::Named {
                head: crate::TypeHead::Builtin(crate::BuiltinType::Instant),
                ..
            } => self.type_satisfies_bound(&Ty::I64, bound),
            Ty::Named {
                head: crate::TypeHead::Builtin(_) | crate::TypeHead::Actor(_),
                ..
            } if marker.is_some() => {
                marker.is_some_and(|marker| self.registry.implements_marker(ty, marker))
            }
            Ty::Named { head, .. } => {
                if self.type_implements_trait_ref(ty, bound)
                    || (bound.args.is_empty()
                        && self.type_structurally_satisfies(ty, bound.trait_id))
                {
                    return true;
                }
                // A binder of the enclosing declaration satisfies the bound
                // its own bounds provide.
                match head {
                    crate::TypeHead::Param(param) => self.param_carries_bound(param.id, bound),
                    _ => false,
                }
            }
            Ty::TraitObject { traits } => traits.iter().any(|object| {
                object.trait_id.is_some_and(|id| {
                    id == bound.trait_id || self.trait_extends(id, bound.trait_id)
                })
            }),
            // Primitives: a user impl first, then the structural marker.
            _ => {
                self.type_implements_trait_ref(ty, bound)
                    || marker.is_some_and(|marker| self.registry.implements_marker(ty, marker))
            }
        }
    }

    /// Whether `ty` satisfies `trait_id` with no arguments.
    pub(in crate::check) fn type_satisfies_trait(
        &mut self,
        ty: &Ty,
        trait_id: crate::DefId,
    ) -> bool {
        self.type_satisfies_bound(ty, &TraitRef::bare(trait_id))
    }

    /// Whether `ty` satisfies the trait a language item names; `false` when
    /// the prelude declares no such trait.
    pub(in crate::check) fn type_satisfies_lang_trait(
        &mut self,
        ty: &Ty,
        item: crate::LangItem,
    ) -> bool {
        self.lang_trait(item)
            .is_some_and(|trait_id| self.type_satisfies_trait(ty, trait_id))
    }

    /// Whether `ty` satisfies the compiler's `Send` predicate: the actor
    /// boundary asks this of the predicate itself, never of a trait spelled
    /// `Send` (R2).
    pub(in crate::check) fn type_is_send(&self, ty: &Ty) -> bool {
        self.registry
            .implements_marker_with_bounds(ty, MarkerTrait::Send, &|param, marker| {
                marker == MarkerTrait::Send
                    && self.param_carries_marker(param.id, MarkerTrait::Send)
            })
    }

    /// Whether trait `trait_id` is declared in the module being checked.
    pub(in crate::check) fn trait_is_local(&self, trait_id: crate::DefId) -> bool {
        let here = self
            .current_declaration_module()
            .map(|module| self.scopes.namespace_of(module));
        let owner = self
            .defs
            .module(trait_id)
            .map(|module| self.scopes.namespace_of(module));
        here.is_some() && here == owner
    }

    /// The trait a qualified associated path (`<T as Trait>.member`) names,
    /// or `None` after reporting why it names none.
    pub(in crate::check) fn resolve_qualified_trait(
        &mut self,
        path: &hew_parser::ast::Path,
        member: Ident,
        span: &Span,
    ) -> Option<crate::DefId> {
        if let Some(trait_id) = self.resolve_trait_path(path) {
            return Some(trait_id);
        }
        let written = path.to_string();
        let ambiguous = match (self.scope_site(), path.segments.as_slice()) {
            (Some(site), [(head, _)]) => {
                self.scopes.ambiguous_import(site.file, head.name).to_vec()
            }
            _ => Vec::new(),
        };
        if ambiguous.len() > 1 {
            let mut owners: Vec<String> = ambiguous
                .iter()
                .filter_map(|binding| self.binding_path(*binding))
                .collect();
            owners.sort();
            self.report_error_with_suggestions(
                TypeErrorKind::AssocItemAmbiguous,
                span,
                format!(
                    "associated item `{member}` is ambiguous because trait `{written}` has multiple imported owners"
                ),
                owners
                    .iter()
                    .map(|owner| format!("qualify the trait as `{owner}`"))
                    .collect(),
            );
        } else {
            self.report_error(
                TypeErrorKind::PathMemberNotFound,
                span,
                format!("cannot resolve trait `{written}` for associated item `{member}`"),
            );
        }
        None
    }

    /// The generic binder `ty` is, when it is a bare one.
    pub(in crate::check) fn bare_param(ty: &Ty) -> Option<crate::ParamHead> {
        match ty {
            Ty::Named {
                head: crate::TypeHead::Param(param),
                args,
            } if args.is_empty() => Some(*param),
            _ => None,
        }
    }

    /// The declared trait a rendered callee qualifier (`Display` in
    /// `Display::fmt`) names in the current file.
    ///
    /// TRANSITION(A1c3): WHY callees still arrive as rendered `Trait::method`
    /// strings. WHEN calls resolve their written path through `Scope`, the
    /// qualifier is that resolution. WHAT: callee `Path`s.
    pub(in crate::check) fn trait_spelled_here(&mut self, spelling: &str) -> Option<crate::DefId> {
        let path = hew_parser::ast::Path::single(Ident::new(spelling), 0..0);
        self.resolve_trait_path(&path)
            .filter(|trait_id| self.trait_info(*trait_id).is_some())
    }

    /// The bounds a module registry signature spells, resolved in the
    /// importing file.
    ///
    /// TRANSITION(A1c4): WHY the registry mirrors a std module's signatures as
    /// spellings before its source registers. WHEN the registry reads the
    /// checker's signatures, the source declaration's resolved bounds are the
    /// only ones. WHAT: delete the registry signature mirror.
    pub(in crate::check) fn registry_bounds(
        &mut self,
        type_params: &[crate::ParamHead],
        spelled: &HashMap<String, Vec<String>>,
    ) -> ParamBounds {
        let mut bounds = ParamBounds::default();
        for param in type_params {
            for spelling in spelled.get(param.spelling.as_str()).into_iter().flatten() {
                if let Some(trait_id) = self.trait_spelled_here(spelling) {
                    bounds.push(param.id, TraitRef::bare(trait_id));
                }
            }
        }
        bounds
    }

    /// Record the active impl block's conformance: `trait_ref` for its
    /// resolved target, and the methods the block writes.
    pub(in crate::check) fn record_trait_impl_from_decl(
        &mut self,
        id: &ImplDecl,
        trait_ref: &TraitRef,
        span: &Span,
    ) {
        let Some(target) = self.current_impl_target() else {
            return;
        };
        self.record_trait_impl_methods(
            &target,
            trait_ref.trait_id,
            id.methods.iter().map(|method| method.name.name),
        );
        let params =
            self.source_parameter_heads(id.type_params.as_deref().unwrap_or_default(), span);
        self.record_trait_impl(&target, trait_ref, params);
    }
}
