//! Checker methods grouped by responsibility: traits.
//! Split from `registration.rs`: checker methods, part 1 of 6.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
use super::super::types::ImportBindingKey;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::BuiltinType;
use hew_parser::ast::WireMetadata;

impl Checker {
    /// Resolve a trait-bound name against the registered trait table,
    /// accepting both unqualified and module-qualified forms.
    pub(in crate::check) fn is_known_trait(&self, name: &str) -> bool {
        if MarkerTrait::from_name(name).is_some() {
            return true;
        }
        if self.trait_defs.contains_key(name) {
            return true;
        }
        if let Some(uq) = self.strip_module_qualifier(name) {
            if self.trait_defs.contains_key(uq) {
                return true;
            }
        }
        false
    }

    /// Register a supervisor declaration under an explicit identity key.
    ///
    /// `identity` is the bare name for a root or flat-file supervisor and the
    /// dotted `{module}.{name}` form for one declared inside a module, matching
    /// what the resolver mints for the same declaration and what
    /// [`Self::register_actor_decl_as`] does for an actor. `type_defs` and
    /// `supervisor_children` are keyed by it, so two modules may each declare a
    /// supervisor of the same name without one clobbering the other.
    pub(in crate::check) fn register_supervisor_decl_as(
        &mut self,
        sd: &SupervisorDecl,
        identity: &str,
    ) {
        let type_params: Vec<_> = sd.type_params.iter().map(|p| p.name.clone()).collect();
        let scope = self.enter_primary_sig_scope(&[(Some(&sd.type_params), None)]);
        let fields = sd
            .params
            .iter()
            .map(|param| (param.name.clone(), self.resolve_type_expr(&param.ty)))
            .collect();
        let mut bounds = self.collect_type_param_bounds(Some(&sd.type_params), None);
        for parameter in &type_params {
            let bounds = bounds.entry(parameter.clone()).or_default();
            if !bounds.iter().any(|bound| bound == "Send") {
                bounds.push("Send".into());
            }
        }
        self.type_defs.insert(
            identity.to_string(),
            TypeDef {
                kind: TypeDefKind::Supervisor,
                name: identity.to_string(),
                type_params,
                bounds,
                fields,
                field_order: sd.params.iter().map(|param| param.name.clone()).collect(),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                is_indirect: false,
            },
        );
        // Partition children by kind in source order. Slot index for each
        // child is its 0-based position within its own partition, matching
        // the runtime layout (children[] for static, pool_slots[] for pool).
        let mut statics = Vec::new();
        let mut pools = Vec::new();
        for c in &sd.children {
            let type_args = c
                .type_args
                .iter()
                .map(|arg| self.resolve_type_expr(arg))
                .collect();
            let entry = (
                c.name.clone(),
                Ty::Named {
                    builtin: None,
                    name: self.canonical_supervisor_child_type(&c.actor_type),
                    args: type_args,
                },
            );
            if c.is_pool {
                pools.push(entry);
            } else {
                statics.push(entry);
            }
        }
        self.supervisor_children.insert(
            identity.to_string(),
            crate::check::types::SupervisorChildren { statics, pools },
        );
        self.exit_primary_sig_scope(scope);
        self.known_types.insert(identity.to_string());
    }

    pub(in crate::check) fn register_actor_decl(&mut self, ad: &ActorDecl) {
        let identity = ad.name.clone();
        self.register_actor_decl_as(ad, &identity);
    }

    /// Register an actor declaration under an explicit identity key.
    ///
    /// `identity` is the bare name for root/flat actors and the dotted
    /// `{module_short}.{name}` form for module actors (see
    /// [`Self::actor_identity`]). Every per-actor side table — `type_defs`,
    /// the Send registry, type-param bounds, init params — is keyed by this
    /// identity so two same-named actors from different modules occupy
    /// distinct entries instead of last-write-wins clobbering.
    pub(in crate::check) fn register_actor_decl_as(&mut self, ad: &ActorDecl, identity: &str) {
        if ad.mailbox_capacity.is_some() {
            self.actor_overflow_policies.insert(
                identity.to_string(),
                ad.overflow_policy
                    .clone()
                    .unwrap_or(hew_parser::ast::OverflowPolicy::Block),
            );
        }

        // Extract type-param names from the declaration so the TypeDef's
        // positional `type_params` vector and ordinary nominal bounds share
        // one declaration authority. Actor arguments must also be Send.
        //
        // Computed and pushed into scope before field/init-param resolution
        // below: an actor's own type parameters (`actor Cache<K: Hash + Eq,
        // V: Clone>`) must already be in scope while its state fields and
        // init parameters are resolved, or a field's `HashMap<K, V>` key
        // admission cannot see K's declared bounds and is rejected as if K
        // had none.
        let type_param_names: Vec<String> =
            ad.type_params.iter().map(|tp| tp.name.clone()).collect();
        let mut type_param_bounds = self.collect_type_param_bounds(Some(&ad.type_params), None);
        for parameter in &type_param_names {
            let bounds = type_param_bounds.entry(parameter.clone()).or_default();
            if !bounds.iter().any(|bound| bound == "Send") {
                bounds.push("Send".into());
            }
        }
        let has_type_params = !type_param_names.is_empty();
        if has_type_params {
            self.current_type_param_bounds.push(TypeParamScope::new(
                type_param_bounds.clone(),
                HashMap::new(),
            ));
        }

        let mut fields = HashMap::new();
        let mut field_order: Vec<String> = Vec::new();
        let mut hole_vars = Vec::new();
        for field in &ad.fields {
            let field_ty = self.resolve_registered_annotation_ty(&field.ty, &mut hole_vars);
            field_order.push(field.name.clone());
            fields.insert(field.name.clone(), field_ty);
        }

        let type_def = TypeDef {
            kind: TypeDefKind::Actor,
            name: identity.to_string(),
            type_params: type_param_names,
            bounds: type_param_bounds.clone(),
            fields,
            field_order,
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: ad.doc_comment.clone(),
            is_indirect: false,
        };

        // Actors are always Send
        self.registry.register_actor(identity.to_string());

        self.type_defs.insert(identity.to_string(), type_def);
        // A new handle-bearing candidate entered `type_defs`; invalidate the
        // cached handle-bearing classification the same way the qualified
        // type-alias path does.
        self.handle_bearing_dirty = true;

        // Collect resolved init() parameter types for supervisor checks.  The
        // byte-copy wall is fail-closed: any shape that does not resolve to a
        // scalar `Ty` is rejected at the parameter type span.
        //
        // Always insert — actors with no init block get an empty vec so that a
        // `wired_to:` reference to such an actor correctly fires
        // "no parameter named X" (`E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH`) rather
        // than silently passing through the "unknown actor" early-return.
        let params: Vec<ActorInitParamInfo> = if let Some(init) = &ad.init {
            init.params
                .iter()
                .map(|p| {
                    let mut init_param_hole_vars = Vec::new();
                    let ty =
                        self.resolve_registered_annotation_ty(&p.ty, &mut init_param_hole_vars);
                    ActorInitParamInfo {
                        name: p.name.clone(),
                        ty,
                    }
                })
                .collect()
        } else {
            vec![]
        };
        if has_type_params {
            self.current_type_param_bounds.pop();
        }
        self.actor_init_params.insert(identity.to_string(), params);
        // A field without a default that init assigns is init's to
        // initialize (D447). An init parameter sharing a field's name is
        // refused outright (D458), so it never reaches this computation as
        // a legitimate source of the field's value.
        let deferred = ad.init.as_ref().map_or_else(Vec::new, |init| {
            let assigned = hew_parser::init_analysis::assigned_bare_names(&init.body);
            ad.fields
                .iter()
                .filter(|field| field.default.is_none() && assigned.contains(&field.name))
                .map(|field| field.name.clone())
                .collect()
        });
        self.actor_deferred_fields
            .insert(identity.to_string(), deferred.clone());
        let mut spawn_args = ad
            .fields
            .iter()
            .filter(|field| !deferred.contains(&field.name))
            .map(|field| (field.name.clone(), field.default.is_none()))
            .collect::<Vec<_>>();
        if let Some(init) = &ad.init {
            spawn_args.extend(
                init.params
                    .iter()
                    .map(|parameter| (parameter.name.clone(), true)),
            );
        }
        self.actor_spawn_args
            .insert(identity.to_string(), spawn_args);
        self.record_type_def_inference_holes(identity, hole_vars);
    }

    pub(in crate::check) fn trait_info_from_decl(
        tr: &TraitDecl,
        source_module: Option<String>,
        file_index: u32,
    ) -> TraitInfo {
        Self::trait_info_from_decl_with_diagnostics(tr, source_module, file_index, &mut Vec::new())
    }

    /// Idempotently seed `#[lang_item("…")]` bindings from a compiled-in
    /// `std/builtins.hew` trait declaration WITHOUT claiming `lang_item_spans`.
    ///
    /// Mirrors the `trait_defs` pre-registration in `register_builtins_hew_impls`
    /// that deliberately skips `type_def_spans`: the user-redeclare path — which
    /// includes type-checking `std/builtins.hew` itself, where the same `Display`
    /// trait is processed a second time through the normal source-registration
    /// pass — must register cleanly without a `duplicate_definition` error.
    /// Genuine collisions between two source traits both tagging the same key
    /// are still caught: that path runs through `register_trait_lang_items`,
    /// which owns `lang_item_spans`.
    pub(in crate::check) fn seed_trait_lang_items(&mut self, td: &TraitDecl, span: &Span) {
        let trait_path = format!("std.builtins.{}", td.name);
        let Some(trait_id) = self.require_declaration_path(&trait_path, span) else {
            return;
        };
        if let Some(key) = &td.lang_item {
            if self.lang_items.get(key).is_none() {
                self.lang_items.insert(
                    key.clone(),
                    crate::LangItemBinding {
                        trait_name: td.name.clone(),
                        trait_id: trait_id.clone(),
                        method_name: None,
                        method_id: None,
                    },
                );
            }
        }
        for item in &td.items {
            if let TraitItem::Method(m) = item {
                if let Some(key) = &m.lang_item {
                    if self.lang_items.get(key).is_none() {
                        let method_id = self.require_declaration_path(
                            &format!("{}::{}", trait_id.full_path(), m.name),
                            &m.span,
                        );
                        self.lang_items.insert(
                            key.clone(),
                            crate::LangItemBinding {
                                trait_name: td.name.clone(),
                                trait_id: trait_id.clone(),
                                method_name: Some(m.name.clone()),
                                method_id,
                            },
                        );
                    }
                }
            }
        }
    }

    /// Harvest `#[lang_item("…")]` tags from a trait declaration into
    /// [`Checker::lang_items`].
    ///
    /// Two kinds of entries are produced:
    ///
    /// * Trait-level (`#[lang_item("display")]` on the `trait` itself) →
    ///   `LangItemBinding { trait_name: <td.name>, method_name: None }`.
    /// * Method-level (`#[lang_item("display_fmt")]` on a `TraitItem::Method`)
    ///   → `LangItemBinding { trait_name: <td.name>, method_name:
    ///   Some(<m.name>) }`. The enclosing trait name is propagated so HIR
    ///   lowering can derive `<SelfType>::<method_name>` impl symbols.
    ///
    /// Duplicate keys raise `TypeError::duplicate_definition` against the
    /// trait's span so the registry remains one-binding-per-key.
    pub(in crate::check) fn register_trait_lang_items(&mut self, td: &TraitDecl, span: Span) {
        let trait_path = self.trait_ref_lookup_key(&td.name);
        let Some(trait_id) = self.require_declaration_path(&trait_path, &span) else {
            return;
        };
        if let Some(key) = &td.lang_item {
            if let Some(prev) = self.lang_item_spans.insert(key.clone(), span.clone()) {
                self.errors
                    .push(TypeError::duplicate_definition(span.clone(), key, prev));
            } else {
                self.lang_items.insert(
                    key.clone(),
                    crate::LangItemBinding {
                        trait_name: td.name.clone(),
                        trait_id: trait_id.clone(),
                        method_name: None,
                        method_id: None,
                    },
                );
            }
        }
        for item in &td.items {
            if let TraitItem::Method(m) = item {
                if let Some(key) = &m.lang_item {
                    let method_span = m.span.clone();
                    if let Some(prev) = self
                        .lang_item_spans
                        .insert(key.clone(), method_span.clone())
                    {
                        self.errors
                            .push(TypeError::duplicate_definition(method_span, key, prev));
                    } else {
                        let method_id = self.require_declaration_path(
                            &format!("{}::{}", trait_id.full_path(), m.name),
                            &m.span,
                        );
                        self.lang_items.insert(
                            key.clone(),
                            crate::LangItemBinding {
                                trait_name: td.name.clone(),
                                trait_id: trait_id.clone(),
                                method_name: Some(m.name.clone()),
                                method_id,
                            },
                        );
                    }
                }
            }
        }
    }

    /// Build `TraitInfo` and surface trait-body diagnostics. Duplicate
    /// `type Bar; type Bar;` declarations are reported here (the impl-side
    /// duplicate-detection is handled separately in `build_impl_alias_entries`).
    pub(in crate::check) fn trait_info_from_decl_with_diagnostics(
        tr: &TraitDecl,
        source_module: Option<String>,
        file_index: u32,
        errors: &mut Vec<TypeError>,
    ) -> TraitInfo {
        let mut methods = Vec::new();
        let mut associated_types: Vec<TraitAssociatedTypeInfo> = Vec::new();
        let mut seen_assoc: HashMap<String, Span> = HashMap::new();
        for item in &tr.items {
            match item {
                TraitItem::Method(m) => methods.push(m.clone()),
                TraitItem::AssociatedType {
                    name,
                    bounds,
                    default,
                    span,
                } => {
                    if let Some(prev_span) = seen_assoc.insert(name.clone(), span.clone()) {
                        errors.push(TypeError::duplicate_definition(
                            span.clone(),
                            name,
                            prev_span,
                        ));
                        continue;
                    }
                    associated_types.push(TraitAssociatedTypeInfo {
                        name: name.clone(),
                        bounds: bounds.clone(),
                        default: default.clone(),
                        span: span.clone(),
                    });
                }
            }
        }
        let type_params = tr
            .type_params
            .as_ref()
            .map(|params| params.iter().map(|p| p.name.clone()).collect())
            .unwrap_or_default();
        TraitInfo {
            source_module,
            file_index,
            methods,
            associated_types,
            type_params,
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "impl-scope setup validates bounds, aliases, and exact receiver identity atomically"
    )]
    pub(in crate::check) fn enter_impl_scope(
        &mut self,
        id: &ImplDecl,
        span: &Span,
        type_name: Option<&str>,
        enforce: bool,
    ) -> bool {
        let Some(target_name) = type_name else {
            return false;
        };
        // Validate before collect_type_param_scope_with_bounds erases positional type args.
        self.validate_type_param_bound_shapes(
            id.type_params.as_ref(),
            id.where_clause.as_ref(),
            span,
        );
        let entries = self.build_impl_alias_entries(id);
        let mut impl_scope_holes = Vec::new();
        let impl_bounds_map = self.collect_type_param_scope_with_assoc_bindings(
            id.type_params.as_ref(),
            id.where_clause.as_ref(),
            &mut impl_scope_holes,
        );
        let pushed_impl_bounds = !impl_bounds_map.bounds.is_empty();
        if pushed_impl_bounds {
            self.current_type_param_bounds.push(impl_bounds_map);
        }
        // Populate impl_assoc_type_bindings on every enter (not gated on
        // `enforce`) so projection collapse can find bindings during
        // call-site monomorphisation even when this scope was entered by
        // a non-enforcing registration sweep. The first writer wins;
        // subsequent calls with the same impl idempotently re-resolve.
        if let Some(tb) = &id.trait_bound {
            // Snapshot trait-side assoc-type list to avoid double-borrow
            // of trait_defs while we call resolve_type_expr.
            // The trait component of the binding key is declaration-owned. An
            // alias or prelude spelling is only a lookup surface; projection
            // carriers and consumers use this same canonical key end-to-end.
            // Associated-type bindings are consumed through
            // `trait_ref_lookup_key` in projection.  Register with that same
            // source-owned identity; `trait_defs_key_for_bound` is a
            // declaration-table compatibility key and may retain a bare
            // presentation spelling while the consumer has already resolved
            // the trait through its canonical module owner.
            let tb_key = self.trait_ref_lookup_key(&tb.name);
            let assoc_names: Vec<String> = self
                .trait_defs
                .get(&tb_key)
                .map(|info| {
                    info.associated_types
                        .iter()
                        .map(|a| a.name.clone())
                        .collect()
                })
                .unwrap_or_default();
            let target_owned = self
                .canonical_primitive_or_builtin_key_for_impl_name(target_name)
                .unwrap_or_else(|| {
                    self.current_module
                        .as_ref()
                        .filter(|_| !target_name.contains('.'))
                        .map_or_else(
                            || target_name.to_string(),
                            |module| format!("{module}.{target_name}"),
                        )
                });
            for assoc_name in assoc_names {
                let key = (target_owned.clone(), tb_key.clone(), assoc_name.clone());
                if self.impl_assoc_type_bindings.contains_key(&key) {
                    continue;
                }
                if let Some(entry) = entries.get(&assoc_name) {
                    let expr = entry.expr.clone();
                    let resolved = self.resolve_type_expr(&expr);
                    if !matches!(resolved, Ty::Error) {
                        self.impl_assoc_type_bindings.insert(key, resolved);
                    }
                }
            }
        }
        if enforce {
            if let Some(tb) = &id.trait_bound {
                // Snapshot trait-side data we need; cloned so we can release
                // the borrow on `self.trait_defs` before calling into the
                // resolver / bound-checker which need `&mut self`. Keyed off the
                // owner-qualified identity, so two modules importing same-named
                // traits cannot let one's empty `trait_defs[name]` mask the
                // other's required `type` and accept an incomplete impl.
                let trait_key = self.trait_defs_key_for_bound(&tb.name);
                let trait_snapshot = self
                    .trait_defs
                    .get(&trait_key)
                    .map(|info| info.associated_types.clone());
                if let Some(associated_types) = trait_snapshot {
                    let missing: Vec<TraitAssociatedTypeInfo> = associated_types
                        .iter()
                        .filter(|assoc| !entries.contains_key(&assoc.name))
                        .cloned()
                        .collect();
                    let target_name_owned = target_name.to_string();
                    let tb_name = tb.name.clone();
                    for assoc in missing {
                        self.report_error_with_note(
                            TypeErrorKind::UndefinedType,
                            span,
                            format!(
                                "impl `{tb_name}` for `{target_name_owned}` must define associated type `{}`",
                                assoc.name
                            ),
                            &assoc.span,
                            "required associated type declared here".to_string(),
                        );
                    }
                    self.check_assoc_type_bounds(
                        &associated_types,
                        &entries,
                        &tb_name,
                        &target_name_owned,
                        id,
                    );
                }
            }
        }
        if pushed_impl_bounds {
            self.current_type_param_bounds.pop();
        }
        self.impl_alias_scopes.push(ImplAliasScope {
            span: span.clone(),
            entries,
            missing_reported: HashSet::new(),
            report_missing: enforce,
        });
        true
    }

    pub(in crate::check) fn exit_impl_scope(&mut self) {
        self.impl_alias_scopes.pop();
    }

    /// Resolve an impl target's type arguments (`Filter<I, A>`) with the
    /// impl's own type parameters in scope, so `A` names the impl's binder
    /// rather than a same-spelled type that another module declares.
    pub(super) fn resolve_impl_target_type_args(
        &mut self,
        id: &ImplDecl,
        type_args: Option<&Vec<Spanned<TypeExpr>>>,
    ) -> Vec<Ty> {
        let bounds = self.collect_type_param_scope_with_bounds(
            id.type_params.as_ref(),
            id.where_clause.as_ref(),
        );
        self.current_type_param_bounds
            .push(TypeParamScope::new(bounds, HashMap::new()));
        let resolved = type_args.map_or_else(Vec::new, |args| {
            args.iter()
                .map(|type_arg| self.resolve_type_expr(type_arg))
                .collect()
        });
        self.current_type_param_bounds.pop();
        resolved
    }

    /// Enforce trait-side bounds on each impl-side associated-type binding.
    ///
    /// For `trait Foo { type Out: Display; }` and `impl Foo for X { type Out = Y; }`,
    /// verifies `Y: Display`. Handles two distinct shapes for the chosen `Y`:
    ///
    /// - **Concrete type** (`Ty::Named { name, .. }` where `name` is a known
    ///   type-def): consult `type_satisfies_trait_bound` directly.
    /// - **Impl type-param** (`Ty::Named { name, .. }` where `name` is one of
    ///   the impl's declared type params, e.g. `impl<T: Display> Foo for X { type Out = T; }`):
    ///   consult the impl's own `collect_type_param_bounds` map, because at
    ///   impl-registration time `current_function` is not set and
    ///   `type_satisfies_trait_bound`'s `type_param_carries_bound` fallback
    ///   would return false-negative.
    pub(super) fn check_assoc_type_bounds(
        &mut self,
        associated_types: &[TraitAssociatedTypeInfo],
        entries: &HashMap<String, ImplAliasEntry>,
        trait_name: &str,
        target_name: &str,
        id: &ImplDecl,
    ) {
        // Pre-collect impl-side type-param bounds. Keys are param names
        // (e.g. `T`), values are bound trait names. Reused across all assoc
        // types in this impl.
        let impl_param_bounds: HashMap<String, Vec<String>> =
            self.collect_type_param_bounds(id.type_params.as_ref(), id.where_clause.as_ref());
        let impl_param_names: HashSet<String> = id
            .type_params
            .as_ref()
            .map(|tps| tps.iter().map(|tp| tp.name.clone()).collect())
            .unwrap_or_default();

        for assoc in associated_types {
            if assoc.bounds.is_empty() {
                continue;
            }
            let Some(entry) = entries.get(&assoc.name) else {
                continue;
            };
            let expr = entry.expr.clone();
            let entry_span = expr.1.clone();
            let resolved = self.resolve_type_expr(&expr);
            // Skip bounds checking when the RHS itself failed to resolve.
            // `resolve_type_expr` already emitted the primary diagnostic;
            // running `type_satisfies_trait_bound(&Ty::Error, _)` here would
            // produce a spurious cascading `BoundsNotSatisfied` on top of it.
            if matches!(resolved, Ty::Error) {
                continue;
            }
            for bound in &assoc.bounds {
                let bound_name = &bound.name;
                let bound_key = self.trait_defs_key_for_bound(bound_name);
                let satisfied = match &resolved {
                    Ty::Named { name, .. } if impl_param_names.contains(name) => {
                        // Impl type-param: check the impl's own bounds map.
                        impl_param_bounds.get(name).is_some_and(|bs| {
                            bs.iter()
                                .any(|b| b == &bound_key || self.trait_extends(b, &bound_key))
                        })
                    }
                    _ => self.type_satisfies_trait_bound(&resolved, bound_name),
                };
                if satisfied {
                    continue;
                }
                self.report_error(
                    TypeErrorKind::BoundsNotSatisfied,
                    &entry_span,
                    format!(
                        "associated type `{}.{}` in impl for `{}` is bound by trait \
                         `{}` but `{}` does not implement `{}`",
                        trait_name,
                        assoc.name,
                        target_name,
                        bound_name,
                        resolved.user_facing(),
                        bound_name,
                    ),
                );
            }
        }
    }

    pub(in crate::check) fn resolve_impl_associated_type(&mut self, alias: &str) -> Option<Ty> {
        let scope_index = self.impl_alias_scopes.len().checked_sub(1)?;
        let expr = {
            let scope = &mut self.impl_alias_scopes[scope_index];
            if let Some(entry) = scope.entries.get_mut(alias) {
                if let Some(resolved) = &entry.resolved {
                    return Some(resolved.clone());
                }
                if entry.resolving {
                    let should_report =
                        scope.report_missing && scope.missing_reported.insert(alias.to_string());
                    let err_span = entry.expr.1.clone();
                    if should_report {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &err_span,
                            format!("associated type `Self.{alias}` recursively references itself"),
                        );
                    }
                    return Some(Ty::Error);
                }
                entry.resolving = true;
                entry.expr.clone()
            } else {
                let should_report =
                    scope.report_missing && scope.missing_reported.insert(alias.to_string());
                let err_span = scope.span.clone();
                if should_report {
                    self.report_error(
                        TypeErrorKind::UndefinedType,
                        &err_span,
                        format!("type alias `Self.{alias}` is not defined in this impl"),
                    );
                }
                return Some(Ty::Error);
            }
        };
        let ty = self.resolve_type_expr(&expr);
        if let Some(scope) = self.impl_alias_scopes.get_mut(scope_index) {
            if let Some(entry) = scope.entries.get_mut(alias) {
                entry.resolving = false;
                entry.resolved = Some(ty.clone());
            }
        }
        Some(ty)
    }

    /// Point each root trait's supertrait edges at the supertrait's
    /// `trait_defs` key, as imported traits' edges already are.
    ///
    /// `collect_types` records a root trait's edges before the root's imports
    /// are bound, so it can only keep the source spelling. Once the imports
    /// are bound here, `trait Pretty: Display` or a flat-imported `trait Mine:
    /// Left` resolves to its declaring trait, and every walk of the chain,
    /// such as a trait object's layout, reaches that trait's identity.
    pub(super) fn canonicalize_root_super_trait_edges(&mut self, program: &Program) {
        for (item, _) in &program.items {
            let Item::Trait(td) = item else {
                continue;
            };
            let Some(supers) = &td.super_traits else {
                continue;
            };
            let keys: Vec<String> = supers
                .iter()
                .map(|bound| self.trait_ref_lookup_key(&bound.name))
                .collect();
            if self.trait_super.contains_key(&td.name) {
                self.trait_super.insert(td.name.clone(), keys);
            }
        }
    }

    /// Type-parameter names declared by `trait_name`, or empty when the trait
    /// is not (yet) registered.
    pub(super) fn trait_type_param_names(&self, trait_name: &str) -> Vec<String> {
        let key = self.trait_ref_lookup_key(trait_name);
        self.trait_defs
            .get(&key)
            .or_else(|| self.trait_defs.get(trait_name))
            .map(|info| info.type_params.clone())
            .unwrap_or_default()
    }

    pub(super) fn register_trait_method_sig(
        &mut self,
        trait_name: &str,
        method: &hew_parser::ast::TraitMethod,
        span: &Span,
    ) {
        let method_key = format!("{trait_name}::{}", method.name);
        // Declaration IDs are minted at the checker registration boundary and
        // handed to HIR verbatim. The key is source-owned, not a linker name.
        // A trait declaration owns its method IDs directly. During the
        // multi-module signature pass the local-trait scope is intentionally
        // not retained, so routing this declaration through the generic
        // reference resolver can collapse two same-leaf module traits to the
        // bare spelling. Prefer the active source module's exact declaration
        // key; imported/default-method references still use the resolver.
        let declaration_key = self
            .current_module
            .as_ref()
            .filter(|module| {
                self.trait_defs
                    .contains_key(&format!("{module}.{trait_name}"))
            })
            .map_or_else(
                || self.trait_ref_lookup_key(trait_name),
                |module| format!("{module}.{trait_name}"),
            );
        // Keyed by the trait's own declaration key, which is stable across the
        // implementing modules an inherited default is re-registered under.
        let trait_params = self.trait_type_param_names(trait_name);
        if !trait_params.is_empty() {
            let owner = Self::method_declaration_key(&declaration_key, &method.name);
            self.reject_shadowing_method_type_params(
                method.type_params.as_ref(),
                &[(trait_params, format!("trait `{trait_name}`"))],
                &owner,
                &method.span,
            );
        }
        let Some(trait_id) = self.require_declaration_path(&declaration_key, span) else {
            return;
        };
        let method_path = format!("{}::{}", trait_id.full_path(), method.name);
        let Some(method_id) = self.require_declaration_path(&method_path, &method.span) else {
            return;
        };
        let ids = (trait_id, method_id);
        self.trait_method_ids
            .insert(ids.1.full_path().to_string(), ids.clone());
        if self.registration_is_flat_file_import {
            self.trait_method_ids_by_binding.insert(
                (
                    None,
                    self.current_module_idx,
                    trait_name.to_string(),
                    method.name.clone(),
                ),
                ids,
            );
        }
        // The owner-qualified key (`{module}.{trait}::{method}`) is collision-free
        // even when two imported modules export a same-named trait. The bare
        // `{trait}::{method}` key is first-write-wins, so with a same-name
        // collision it holds whichever module registered first and silently
        // shadows the other's signature. Trait-conformance resolves an aliased
        // trait to its source owner and looks up this qualified key
        // authoritatively, so it must always be present when an owner is known.
        let owner_qualified_key = self
            .current_module
            .as_ref()
            .map(|module| format!("{module}.{method_key}"));

        // Build the trait method's FnDecl once; reuse it for both the bare and
        // owner-qualified registrations. `Self::Bar` projection is active while
        // the signature resolves so `Self::Item` becomes a deferred
        // `Ty::AssocType` carrier instead of an opaque named type.
        let receiver_identity_is_valid =
            self.validate_trait_receiver_identity_method(trait_name, method);
        let mut attributes = method.attributes.clone();
        if !receiver_identity_is_valid {
            attributes.retain(|attribute| attribute.name != "returns_receiver");
        }
        let decl = FnDecl {
            origin: hew_parser::ast::DeclarationOrigin::Authored,
            attributes,
            is_generator: false,
            visibility: hew_parser::ast::Visibility::Private,
            name: method.name.clone(),
            type_params: method.type_params.clone(),
            params: method.params.clone(),
            return_type: method.return_type.clone(),
            where_clause: method.where_clause.clone(),
            body: hew_parser::ast::Block {
                stmts: vec![],
                trailing_expr: None,
            },
            doc_comment: None,
            decl_span: span.clone(),
            fn_span: 0..0,
            intrinsic: None,
            consumes_self: method.consumes_self,
        };

        let prev_trait_self = self
            .current_trait_for_self_projection
            .replace(declaration_key);
        // Register the owner-qualified key first (collision-free) regardless of
        // whether the bare key is already taken by another module's same-named
        // trait, so the authoritative lookup never misses for a known owner.
        if let Some(qualified_key) = owner_qualified_key.as_ref() {
            if !self.fn_sigs.contains_key(qualified_key) {
                self.register_fn_sig_with_name(qualified_key, &decl);
            }
        }
        // The bare key keeps first-write-wins for the local/non-aliased path.
        if !self.fn_sigs.contains_key(&method_key) {
            self.register_fn_sig_with_name(&method_key, &decl);
        }
        self.current_trait_for_self_projection = prev_trait_self;
    }

    pub(in crate::check) fn trait_receiver_identity_is_structurally_valid(
        method: &hew_parser::ast::TraitMethod,
    ) -> bool {
        let identity_attributes: Vec<_> = method
            .attributes
            .iter()
            .filter(|attribute| attribute.name == "returns_receiver")
            .collect();
        if identity_attributes.is_empty() {
            return false;
        }

        let returns_self_type = method.return_type.as_ref().is_some_and(|(ty, _)| {
            matches!(
                ty,
                TypeExpr::Named { name, type_args }
                    if name == "Self" && type_args.as_ref().is_none_or(Vec::is_empty)
            )
        });
        let body_is_exact = method.body.as_ref().is_none_or(|body| {
            let direct_self_tail = body
                .trailing_expr
                .as_deref()
                .is_some_and(|(expr, _)| matches!(expr, Expr::Identifier(name) if name == "self"));
            direct_self_tail && !block_has_explicit_return(body)
        });
        identity_attributes.len() == 1
            && identity_attributes[0].args.is_empty()
            && method.consumes_self
            && returns_self_type
            && body_is_exact
    }

    pub(super) fn validate_trait_receiver_identity_method(
        &mut self,
        trait_name: &str,
        method: &hew_parser::ast::TraitMethod,
    ) -> bool {
        let declares_identity = method
            .attributes
            .iter()
            .any(|attribute| attribute.name == "returns_receiver");
        if !declares_identity {
            return false;
        }
        let valid = Self::trait_receiver_identity_is_structurally_valid(method);
        if !valid {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &method.span,
                format!(
                    "`#[returns_receiver]` on trait method `{trait_name}.{}` requires \
                     one zero-argument attribute, a `consume self` receiver, the exact \
                     `Self` return type, and any default body to have one direct trailing \
                     `self` with no alternate `return` path",
                    method.name
                ),
            );
        }
        valid
    }

    /// Substitute trait-side type references into impl-side concrete types.
    ///
    /// Walks `ty` recursively and replaces:
    /// * `Ty::Named { name: "Self", args: [] }` → `impl_self`
    /// * `Ty::Named { name: <trait type param>, args: [] }` → the impl-supplied
    ///   type arg from `trait_param_map`
    /// * `Ty::AssocType { base: Self, .. }` → a concrete projection carrier
    ///   over `impl_self`, then resolves it through `project_assoc_types`
    ///
    /// Used by [`Self::check_impl_method_against_trait`] to project the trait
    /// method's declared signature into the concrete shape the impl method
    /// must match. Returns the input unchanged for any subterm the
    /// substitution cannot resolve, so the later structural comparison remains
    /// fail-closed instead of guessing from presentation spellings.
    pub(super) fn substitute_trait_sig_for_impl(
        &self,
        ty: &Ty,
        impl_self: &Ty,
        trait_param_map: &HashMap<String, Ty>,
    ) -> Ty {
        match ty {
            Ty::Named { name, args, .. } if args.is_empty() && name == "Self" => impl_self.clone(),
            Ty::Named { name, args, .. } if args.is_empty() => {
                if let Some(mapped) = trait_param_map.get(name) {
                    return mapped.clone();
                }
                ty.clone()
            }
            Ty::AssocType {
                base,
                trait_name: tn,
                assoc_name,
            } => {
                let new_base = self.substitute_trait_sig_for_impl(base, impl_self, trait_param_map);
                self.project_assoc_types(&Ty::AssocType {
                    base: Box::new(new_base),
                    trait_name: tn.clone(),
                    assoc_name: assoc_name.clone(),
                })
            }
            _ => ty.map_children_pub(&|child| {
                self.substitute_trait_sig_for_impl(child, impl_self, trait_param_map)
            }),
        }
    }

    /// Resolve a trait reference as written in an `impl ... for ...` block to
    /// its OWNER-QUALIFIED identity, so trait conformance never keys off the
    /// bare `Trait::method` name. The bare name is polluted under same-name
    /// collisions: `register_trait_method_sig` writes the bare `fn_sigs` key
    /// first-write-wins, so when two imported modules (or an import plus a
    /// local declaration) share a trait name, the bare key holds whichever
    /// registered first and silently shadows the others. Resolution covers all
    /// three reference kinds uniformly:
    ///
    ///   * **aliased / imported-bare** — `published_bare_trait_owners` maps the
    ///     in-scope binding to its source identity `{module}.{Trait}`. The
    ///     owner-qualified `fn_sigs` key `{module}.{Trait}::{method}` is
    ///     collision-free (always registered for module traits).
    ///   * **local / root** — a trait declared in the importing program shadows
    ///     any imported same-name trait. The local `trait_defs[bare]` entry is
    ///     authoritative (last-write-wins), so its required method set and
    ///     signatures are derived from that `TraitInfo` directly, never from the
    ///     polluted bare `fn_sigs` key.
    ///   * **unambiguous single-owner import** — recovered by scanning
    ///     `trait_defs` for a single `{module}.{Trait}` qualified key.
    ///
    /// `trait_name` is the name as written (`A`, `Source`). Returns the resolved
    /// identity; `owner` is the defining module (`None` for a local/root trait
    /// or an unresolved name), and `is_local` records the local-shadow case so
    /// callers source the required-method set from the local `TraitInfo`.
    pub(in crate::check) fn resolve_trait_conformance_identity(
        &self,
        trait_name: &str,
    ) -> ResolvedTraitIdentity {
        // A primary trait reference (`impl <Trait> for ...`, a bound) is spelled
        // in the CURRENT module, so it resolves through the current scope: local
        // shadow first, then the importer's published-bare binding, then a
        // single-owner suffix scan. This is the `Current` arm of the one
        // canonical resolver — keeping every landed H1–H10 behaviour byte-for-byte.
        self.resolve_trait_ref(trait_name, TraitRefScope::Current)
    }

    /// Whether a primary trait reference resolves to a LOCAL declaration. Routes
    /// through the one canonical resolver so callers outside this module (the
    /// orphan rule) decide "is this trait local" by the same authoritative
    /// identity every trait-reference site uses, never the bare spelling.
    pub(in crate::check) fn trait_ref_is_local(&self, trait_name: &str) -> bool {
        self.resolve_trait_ref(trait_name, TraitRefScope::Current)
            .is_local
    }

    /// The owner-qualified SOURCE identity (`owner.Name`) a bare TRAIT reference
    /// resolves to when exactly one imported module PUBLISHED the bare binding
    /// into `importer` and a qualified def for it is registered; `None` otherwise
    /// (local shadow, zero/ambiguous publishers, or no registered def). The exact
    /// mirror of `published_bare_type_qualified` (there are no builtin traits, so
    /// no builtin-exempt branch). The stored value IS the source identity, so an
    /// aliased opt-in (`import m::{ T as U }`) binds `U` to `m.T`, never a
    /// reconstructed `m.U`.
    pub(in crate::check) fn published_bare_trait_qualified(
        &self,
        name: &str,
        importer: Option<&str>,
    ) -> Option<String> {
        if self.local_trait_defs.contains(name) {
            return None;
        }
        let identities = self.published_bare_trait_owners.get(&(
            importer.map(str::to_string),
            self.current_module_idx,
            name.to_string(),
        ))?;
        if identities.len() != 1 {
            return None;
        }
        let qualified = identities.iter().next()?;
        self.trait_defs
            .contains_key(qualified)
            .then(|| qualified.clone())
    }

    /// THE canonical trait-reference resolver. Resolves a trait name spelled in a
    /// given scope to one owner-qualified `ResolvedTraitIdentity`, composing on
    /// the proven `published_bare_trait_owners` single-publisher-or-fail-closed
    /// primitive. Every trait reference — primary trait, bound, and supertrait
    /// edge — routes through here so identity is keyed by OWNER, never by the bare
    /// spelling (which is first-write-wins and polluted under same-name collisions).
    ///
    /// Scope is load-bearing:
    ///   * `Current` — the name is spelled in the importing program; a local
    ///     declaration shadows every imported same-name trait, and the importer's
    ///     own published-bare binding decides the owner.
    ///   * `Declaring { module }` — the name is a SUPERTRAIT edge spelled inside
    ///     `module`'s own declaration (`trait Sub: Base`). It is resolved through
    ///     `module`'s import bindings (the re-export chain to the original owner),
    ///     NEVER the importer's local trait of the same name — which is why the
    ///     local-shadow step is gated on `Current`.
    pub(in crate::check) fn resolve_trait_ref(
        &self,
        name: &str,
        scope: TraitRefScope<'_>,
    ) -> ResolvedTraitIdentity {
        // (1) LOCAL SHADOW — only in the CURRENT module's scope. A supertrait edge
        //     spelled in a DECLARING module is never the importer's local trait of
        //     the same name; gating this on `Current` is the H11 scope correctness.
        if matches!(scope, TraitRefScope::Current) && self.local_trait_defs.contains(name) {
            if let Some(module) = self.current_module.as_deref() {
                let qualified = format!("{module}.{name}");
                if self.trait_defs.contains_key(&qualified) {
                    return self.identity_from_trait_defs_key(&qualified);
                }
            }
            return ResolvedTraitIdentity {
                owner: None,
                source_trait_name: name.to_string(),
                is_local: true,
            };
        }

        // A trait reference written inside a non-root module resolves to that
        // module's own declaration before considering imported or suffix-based
        // candidates. The full current owner is essential here: two nested
        // modules may both be named `render`, and neither may select the other's
        // `Render` declaration through a leaf-only retry.
        if matches!(scope, TraitRefScope::Current) {
            if let Some(module) = self.current_module.as_deref() {
                let qualified = format!("{module}.{name}");
                if self.trait_defs.contains_key(&qualified) {
                    return self.identity_from_trait_defs_key(&qualified);
                }
            }
        }

        // (2) DECLARING-module import binding (closes H11): a re-exported super
        //     edge follows the chain to its original owner. `reexsub`'s
        //     `import reexbase::{ Base }` records `("reexsub","Base") -> "reexbase.Base"`,
        //     so `Sub: Base` resolves `Base` to `reexbase.Base` regardless of what
        //     same-named `Base` the final importer has in scope.
        if let TraitRefScope::Declaring { module } = scope {
            if let Some(source_key) = self
                .trait_import_bindings
                .get(&(module.to_string(), name.to_string()))
            {
                return self.identity_from_trait_defs_key(source_key);
            }
        }

        // (3) PUBLISHED-BARE single-publisher (the promoted primitive), only in
        //     the current scope: a `Declaring` edge's own-module super and
        //     re-exports are handled by (2) and (4) and never consult the final
        //     importer's published map. Handles aliased / imported-bare.
        if matches!(scope, TraitRefScope::Current) {
            if let Some(qualified) =
                self.published_bare_trait_qualified(name, self.current_module.as_deref())
            {
                return self.identity_from_trait_defs_key(&qualified);
            }
        }

        // (4) UNAMBIGUOUS SINGLE-OWNER suffix scan: a non-aliased name whose
        //     binding == its source name, recovered from the single
        //     `{module}.{Trait}` qualified key in `trait_defs`. Also serves a
        //     `Declaring` edge whose super is the declaring module's OWN def
        //     (`{module}.Base` registered) — so no separate own-def-first branch.
        //     With zero or an ambiguous set, leave the owner unresolved and fall
        //     back to the source-name comparison (best-effort for an external
        //     reference not in the loaded graph; the downstream method-set / sig
        //     check fires honestly against an absent/empty set — fail-closed).
        let suffix = format!(".{name}");
        let mut owners: Vec<String> = self
            .trait_defs
            .keys()
            .filter_map(|k| k.strip_suffix(&suffix))
            .filter(|module| {
                !module.is_empty()
                    && (self.modules.contains(*module)
                        || self.canonical_std_module_sources.contains(*module)
                        || self.canonical_std_root_sources.contains(*module)
                        || self
                            .current_module
                            .as_deref()
                            .is_some_and(|current| crate::short_name(current) == *module))
            })
            .map(|module| {
                self.module_import_bindings
                    .get(&(
                        self.current_module.clone(),
                        self.current_module_idx,
                        module.to_string(),
                    ))
                    .cloned()
                    .unwrap_or_else(|| module.to_string())
            })
            .collect();
        owners.sort_unstable();
        owners.dedup();
        let owner = match owners.as_slice() {
            [single] => Some(single.clone()),
            _ => None,
        };
        ResolvedTraitIdentity {
            owner,
            source_trait_name: name.to_string(),
            is_local: false,
        }
    }

    /// Return the registry key for a trait name written in the current module.
    ///
    /// Trait-object bounds retain their source spelling for HIR/codegen, while
    /// checker lookup must use the owning module's canonical key.
    pub(in crate::check) fn trait_ref_lookup_key(&self, name: &str) -> String {
        let identity = self.resolve_trait_ref(name, TraitRefScope::Current);
        self.trait_defs_key_for_identity(&identity)
    }

    pub(in crate::check) fn resolved_trait_defaults(
        &mut self,
    ) -> HashMap<crate::DefId, Vec<super::types::ResolvedTraitDefault>> {
        let saved_module = self.current_module.clone();
        let saved_file = self.current_module_idx;
        let mut declarations: Vec<_> = self
            .trait_defs
            .iter()
            .filter_map(|(key, info)| {
                self.lookup_declaration(key)
                    .cloned()
                    .map(|id| (key.clone(), id, info.clone()))
            })
            .collect();
        declarations.sort_by_key(|(key, id, _)| (key != id.full_path(), key.clone()));
        let mut defaults = HashMap::new();
        let mut visited = HashSet::new();
        for (key, trait_id, info) in declarations {
            if !visited.insert(trait_id.clone()) {
                continue;
            }
            self.current_module.clone_from(&info.source_module);
            self.current_module_idx = info.file_index;
            let identity = self.identity_from_trait_defs_key(&key);
            let mut names: HashSet<String> = info.methods.iter().map(|m| m.name.clone()).collect();
            self.collect_super_trait_method_names(&key, &mut names, &mut HashSet::new());
            for name in names {
                let Some(owner) = self.resolve_declaring_trait_identity(&identity, &key, &name)
                else {
                    continue;
                };
                let owner_key = self.trait_defs_key_for_identity(&owner);
                let Some(owner_id) = self.lookup_declaration(&owner_key) else {
                    continue;
                };
                let method_key = format!("{}::{name}", owner_id.full_path());
                let Some(ids) = self.trait_method_ids.get(&method_key).cloned() else {
                    continue;
                };
                self.trait_method_ids
                    .insert(format!("{}::{name}", trait_id.full_path()), ids);
            }
            let bodies = info
                .methods
                .into_iter()
                .filter(|method| method.body.is_some())
                .filter_map(|method| {
                    let (_, method_id) = self
                        .trait_method_ids
                        .get(&format!("{}::{}", trait_id.full_path(), method.name))?
                        .clone();
                    Some(super::types::ResolvedTraitDefault {
                        trait_id: trait_id.clone(),
                        method_id,
                        method,
                        source_module: info.source_module.clone(),
                        file_index: info.file_index,
                    })
                })
                .collect();
            defaults.insert(trait_id, bodies);
        }
        for (binding, trait_id) in &self.trait_bindings {
            let prefix = format!("{}::", trait_id.full_path());
            for (key, ids) in &self.trait_method_ids {
                if let Some(method) = key.strip_prefix(&prefix) {
                    self.trait_method_ids_by_binding.insert(
                        (
                            binding.0.clone(),
                            binding.1,
                            binding.2.clone(),
                            method.to_string(),
                        ),
                        ids.clone(),
                    );
                }
            }
        }
        self.current_module = saved_module;
        self.current_module_idx = saved_file;
        defaults
    }

    /// The set of method names a trait requires an impl to provide, resolved
    /// through the trait's OWNER-QUALIFIED identity so a same-name collision
    /// cannot leak a neighbouring trait's method set. A method with a default
    /// body is optional (impls may omit it), so only bodyless methods of the
    /// resolved trait ITSELF are "required". The "known" set additionally
    /// includes the trait's whole super-trait chain: an impl of a sub-trait may
    /// legitimately provide a super-trait method inline (`trait Sub: Super` →
    /// `impl Sub for T { fn <super-method> … }`), so such a method must not be
    /// flagged as extraneous. Super-trait REQUIRED methods are NOT folded into
    /// `required` here — they are satisfied by a separate `impl Super for T` (the
    /// idiomatic form) or enforced at the bound site, and folding them in would
    /// falsely reject that separate-impl pattern.
    ///
    /// Returns `(required, known)`, or `None` when the trait reference does not
    /// resolve to a known trait (a separate diagnostic covers an unknown bound).
    pub(super) fn trait_required_and_known_methods(
        &self,
        identity: &ResolvedTraitIdentity,
    ) -> Option<(HashSet<String>, HashSet<String>)> {
        // The one owner-qualified `trait_defs` key for this identity; collision-
        // free even when a same-name trait registered the bare key first. The
        // local-shadow and unresolved paths fall back to the identity's SOURCE
        // name (authoritative for a local trait, best-effort for an unresolved
        // reference) — derived by the single `trait_defs_key_for_identity` home.
        let lookup_key = self.trait_defs_key_for_identity(identity);
        let info = self.trait_defs.get(&lookup_key)?;

        let mut required = HashSet::new();
        let mut known = HashSet::new();
        for m in &info.methods {
            known.insert(m.name.clone());
            if m.body.is_none() {
                required.insert(m.name.clone());
            }
        }
        // Fold every (transitive) super-trait's declared methods into `known`
        // so an inline super-trait method is permitted, not flagged as extra.
        let mut visited: HashSet<String> = HashSet::new();
        self.collect_super_trait_method_names(&lookup_key, &mut known, &mut visited);
        Some((required, known))
    }

    /// Resolve a supertrait edge written inside `module_short` to its
    /// OWNER-QUALIFIED `trait_defs` key, through the one canonical resolver in the
    /// `Declaring` scope. A `trait Sub: Base` declaration spells `Base` bare, but
    /// it names the trait `module_short` itself resolves `Base` to: its OWN
    /// same-package `Base` (`{module_short}.Base`, found by the resolver's
    /// single-owner suffix scan), or a RE-IMPORTED `Base`
    /// (`import other::{ Base }`, followed through `module_short`'s import bindings
    /// to the original owner `other.Base`).
    ///
    /// The re-imported case is the H11 case the old `{module_short}.Base`-only
    /// check missed: there is no `{module_short}.Base` def for a re-imported super,
    /// so it fell back to the bare name and bound whatever `Base` the final
    /// importer had in scope (a collision-unsafe fail-open). When the super is
    /// genuinely external to the loaded graph, the resolver yields no owner and
    /// this returns the source name; the downstream method-set / signature check
    /// then fires honestly against an absent set — fail-closed, never accept-all.
    pub(super) fn resolve_super_trait_edge(&self, module_short: &str, super_name: &str) -> String {
        let identity = self.resolve_trait_ref(
            super_name,
            TraitRefScope::Declaring {
                module: module_short,
            },
        );
        self.trait_defs_key_for_identity(&identity)
    }

    /// Walk the (transitive) super-trait chain of `trait_key`, inserting every
    /// super-trait's declared method name into `known`. `visited` guards against
    /// cycles. Super-trait edges are owner-qualified `trait_defs` keys (an
    /// imported `Sub`'s edge points at `{owner}.Base`, never the importer's bare
    /// `Base`; a local trait's edge is its bare local key, which is authoritative
    /// for a local trait), so the recursion resolves each super against its
    /// defining trait, collision-free.
    pub(super) fn collect_super_trait_method_names(
        &self,
        trait_key: &str,
        known: &mut HashSet<String>,
        visited: &mut HashSet<String>,
    ) {
        if !visited.insert(trait_key.to_string()) {
            return;
        }
        let Some(supers) = self.trait_super.get(trait_key) else {
            return;
        };
        for super_name in supers.clone() {
            let resolved_super = if self.trait_defs.contains_key(&super_name) {
                super_name
            } else if let Some((declaring_module, _)) = trait_key.rsplit_once('.') {
                // Early module registration can retain a super edge's source
                // spelling before the import binding is published. Resolve it
                // in the declaring trait's exact module now; never consult the
                // final importer's bare namespace or a suffix/leaf retry.
                self.resolve_super_trait_edge(declaring_module, &super_name)
            } else {
                super_name
            };
            if let Some(super_info) = self.trait_defs.get(&resolved_super) {
                for m in &super_info.methods {
                    known.insert(m.name.clone());
                }
            }
            self.collect_super_trait_method_names(&resolved_super, known, visited);
        }
    }

    /// The `trait_defs` key for a resolved trait identity: the owner-qualified
    /// `{owner}.{source}` key when an owner resolved and that key is registered,
    /// otherwise the identity's SOURCE name (authoritative for a local trait, best
    /// effort for an unresolved reference). Mirrors the `lookup_key` derivation in
    /// `trait_required_and_known_methods`. The fallback is the source name — not
    /// the sub-trait the impl wrote — so a declaring SUPERTRAIT identity reached
    /// through the super chain keys on the supertrait's own name (`Base`), never
    /// the sub-trait's (`Sub`).
    pub(super) fn trait_defs_key_for_identity(&self, identity: &ResolvedTraitIdentity) -> String {
        identity
            .owner
            .as_ref()
            .map(|owner| {
                let canonical_owner = self
                    .module_import_bindings
                    .get(&(
                        self.current_module.clone(),
                        self.current_module_idx,
                        owner.clone(),
                    ))
                    .map_or(owner.as_str(), String::as_str);
                format!("{canonical_owner}.{}", identity.source_trait_name)
            })
            .filter(|q| self.trait_defs.contains_key(q))
            .unwrap_or_else(|| identity.source_trait_name.clone())
    }

    /// The owner-qualified `trait_defs` key for a bare trait-bound name spelled in
    /// an `impl <Trait> for <Type>` (the CURRENT module's scope). The impl-side
    /// associated-type / default-method machinery (required-assoc enforcement,
    /// default-assoc collection, default-method registration) keys its
    /// `trait_defs` lookups on this instead of the bare `tb.name`, so a same-name
    /// trait imported by a *different* module cannot poison the global bare
    /// `trait_defs[name]` entry and let an impl skip a required `type` or inherit
    /// the wrong defaults. Routes through the one canonical conformance resolver,
    /// exactly as `check_impl_method_set_against_trait` does for the method set.
    pub(in crate::check) fn trait_defs_key_for_bound(&self, name: &str) -> String {
        let identity = self.resolve_trait_conformance_identity(name);
        self.trait_defs_key_for_identity(&identity)
    }

    /// Resolve which trait DECLARES `method_name` for an `impl <Trait>`: the
    /// primary trait when it declares the method directly, otherwise the
    /// supertrait in the OWNER-QUALIFIED super chain that declares it (an impl of
    /// a sub-trait may provide an inherited supertrait method inline). Returns the
    /// declaring trait's owner-qualified identity so the caller's signature
    /// lookup and trait-owner canonicalization key off the SUPERTRAIT's owner —
    /// not the sub-trait's — when the method is inherited. Without this, an inline
    /// supermethod is never found on the primary trait and its signature goes
    /// unchecked (a fail-open: a wrong-signature inherited method is accepted).
    ///
    /// `primary_key` is the primary trait's `trait_defs` key (owner-qualified).
    /// The walk follows `trait_super` (owner-qualified edges) and matches each
    /// super against its `trait_defs` entry, so a same-name supertrait collision
    /// resolves through the defining owner, never the importer namespace.
    pub(super) fn resolve_declaring_trait_identity(
        &self,
        primary_identity: &ResolvedTraitIdentity,
        primary_key: &str,
        method_name: &str,
    ) -> Option<ResolvedTraitIdentity> {
        if self
            .trait_defs
            .get(primary_key)
            .is_some_and(|info| info.methods.iter().any(|m| m.name == method_name))
        {
            return Some(ResolvedTraitIdentity {
                owner: primary_identity.owner.clone(),
                source_trait_name: primary_identity.source_trait_name.clone(),
                is_local: primary_identity.is_local,
            });
        }
        let mut visited: HashSet<String> = HashSet::new();
        let mut stack: Vec<String> = self
            .trait_super
            .get(primary_key)
            .cloned()
            .unwrap_or_default();
        while let Some(super_key) = stack.pop() {
            if !visited.insert(super_key.clone()) {
                continue;
            }
            let declares = self
                .trait_defs
                .get(&super_key)
                .is_some_and(|info| info.methods.iter().any(|m| m.name == method_name));
            if declares {
                return Some(self.identity_from_trait_defs_key(&super_key));
            }
            if let Some(supers) = self.trait_super.get(&super_key) {
                stack.extend(supers.iter().cloned());
            }
        }
        None
    }

    /// Recover a trait identity from a `trait_defs` key. An owner-qualified
    /// `{module}.{Trait}` key registered by the checker yields
    /// `owner = Some(module)`, `source = Trait`; a bare key yields a local
    /// identity (`is_local = true`, no owner). Used to re-anchor the signature
    /// lookup on a declaring SUPERTRAIT reached through the super chain.
    pub(super) fn identity_from_trait_defs_key(&self, key: &str) -> ResolvedTraitIdentity {
        match key.rsplit_once('.') {
            Some((module, source)) if self.trait_defs.contains_key(key) => ResolvedTraitIdentity {
                owner: Some(module.to_string()),
                source_trait_name: source.to_string(),
                is_local: false,
            },
            _ => ResolvedTraitIdentity {
                owner: None,
                source_trait_name: key.to_string(),
                is_local: true,
            },
        }
    }

    /// Validate that an `impl <Trait> for <Type>` provides EXACTLY the trait's
    /// method set: every required (bodyless) trait method present, and no method
    /// that is not declared on the trait. Keyed off the trait's owner-qualified
    /// identity (`resolve_trait_conformance_identity`), so a same-name trait
    /// collision can never leak a neighbour's method set into the comparison.
    /// Per-method signature equivalence is enforced separately by
    /// `check_impl_method_against_trait`.
    pub(in crate::check) fn check_impl_method_set_against_trait(
        &mut self,
        type_name: &str,
        trait_bound: &TraitBound,
        impl_methods: &[FnDecl],
        impl_span: &Span,
    ) {
        let trait_name = &trait_bound.name;
        let identity = self.resolve_trait_conformance_identity(trait_name);
        let trait_key = self.trait_defs_key_for_identity(&identity);
        self.mark_imported_trait_used(self.current_module.as_deref(), trait_name);
        if let Some(declaration) = self.lookup_declaration(&trait_key).cloned() {
            self.trait_bindings.insert(
                (
                    self.current_module.clone(),
                    self.current_module_idx,
                    trait_name.clone(),
                ),
                declaration,
            );
        }
        let Some((required, known)) = self.trait_required_and_known_methods(&identity) else {
            // D429: no declaration in scope defines this trait, so there is no
            // method set to check the impl against. Accepting it would register
            // the impl's methods under a contract that does not exist.
            //
            // Marker traits (`Eq`, `Hash`, `Copy`, ...) carry no declared
            // method set, so a missing `trait_defs` entry is their normal
            // state and says nothing about whether the name resolves.
            let leaf = trait_name.rsplit('.').next().unwrap_or(trait_name);
            if crate::traits::MarkerTrait::from_name(leaf).is_some() {
                return;
            }
            self.report_error(
                TypeErrorKind::UnknownTraitInImpl {
                    trait_name: trait_name.clone(),
                    type_name: type_name.to_string(),
                },
                impl_span,
                format!("cannot find trait `{trait_name}` in this scope"),
            );
            return;
        };

        let type_identity = self.trait_impl_type_identity(type_name);
        let mut provided: HashSet<String> = impl_methods.iter().map(|m| m.name.clone()).collect();
        // Split trait surfaces are cumulative on one exact nominal identity.
        // This is how a subtrait that redeclares inherited methods is satisfied
        // by the already-registered supertrait impl for the same type. Never
        // aggregate by the type leaf: sibling modules may both define `Value`.
        for ((implemented_type, _), methods) in &self.trait_impl_method_names {
            if implemented_type == &type_identity {
                provided.extend(methods.iter().cloned());
            }
        }

        // INHERITED method names: every method declared by a (transitively
        // reachable) SUPERTRAIT of this trait. A sub-trait may REDECLARE its
        // supertrait's methods (`trait Sub: Base { fn base(self); ... }`), which
        // makes them bodyless-required on `Sub` even though they belong to
        // `Base`. Such an inherited requirement is satisfied by a SEPARATE
        // `impl Base for T` block (not the `impl Sub for T` block), exactly as a
        // non-redeclared inherited method is — supertrait conformance is resolved
        // lazily at the call site (`no method X on T` if no impl supplies it),
        // never eagerly forced onto the sub-trait's own impl block. Dropping an
        // inherited method from `missing` here keeps redeclared and non-redeclared
        // inherited methods behaving identically; the per-method signature check
        // (`resolve_declaring_trait_identity`) still validates any inline copy.
        let lookup_key = self.trait_defs_key_for_identity(&identity);
        let mut inherited: HashSet<String> = HashSet::new();
        let mut inherited_visited: HashSet<String> = HashSet::new();
        self.collect_super_trait_method_names(&lookup_key, &mut inherited, &mut inherited_visited);

        // Missing: a required (bodyless) trait method the impl never provided AND
        // that is not inherited from a supertrait (the supertrait's own impl
        // block carries that obligation).
        let mut missing: Vec<String> = required
            .iter()
            .filter(|name| !provided.contains(*name))
            .filter(|name| !inherited.contains(name.as_str()))
            .cloned()
            .collect();
        missing.sort_unstable();
        if !missing.is_empty() {
            self.report_error_with_note(
                TypeErrorKind::TraitImplMissingMethods {
                    trait_name: trait_name.clone(),
                    type_name: type_name.to_string(),
                    methods: missing.clone(),
                },
                impl_span,
                format!(
                    "impl `{trait_name}` for `{type_name}` is missing required method(s): {}",
                    missing.join(", ")
                ),
                impl_span,
                format!(
                    "trait `{trait_name}` requires {}",
                    if missing.len() == 1 {
                        format!("method `{}`", missing[0])
                    } else {
                        format!("methods {}", missing.join(", "))
                    }
                ),
            );
        }

        // Extra: an impl method that is not declared on the trait at all.
        let mut extra: Vec<String> = impl_methods
            .iter()
            .map(|m| m.name.clone())
            .filter(|name| !known.contains(name))
            .collect();
        extra.sort_unstable();
        extra.dedup();
        if !extra.is_empty() {
            self.report_error_with_note(
                TypeErrorKind::TraitImplExtraMethods {
                    trait_name: trait_name.clone(),
                    type_name: type_name.to_string(),
                    methods: extra.clone(),
                },
                impl_span,
                format!(
                    "impl `{trait_name}` for `{type_name}` declares method(s) not on the trait: {}",
                    extra.join(", ")
                ),
                impl_span,
                format!("trait `{trait_name}` declares no such method(s)"),
            );
        }
    }

    /// Enforce that an impl method's signature matches the declared trait
    /// method's signature, after substituting `Self`, trait type parameters,
    /// and the impl's associated-type aliases. Q004 / LESSONS
    /// `diagnostic-trust`: emits at the impl method's local span so the user
    /// sees the actual divergence instead of a confusing
    /// "type does not satisfy trait" cascaded from a later call site.
    ///
    /// Silent (no diagnostic) when:
    /// * the trait method is not declared (the impl-site method-SET check in
    ///   `check_impl_method_set_against_trait` reports an extra method; this
    ///   per-method check only enforces equivalence of methods the trait
    ///   declares);
    /// * the trait signature was not registered (already produced a
    ///   diagnostic, would double-fire);
    /// * any side of the comparison contains `Ty::Error` (cascading
    ///   suppression — see the `cascading-Ty::Error` invariant);
    /// * the receiver-skip stripped a different number of params on either
    ///   side because the impl elided the receiver (treat as receiver-kind
    ///   mismatch and report).
    #[allow(
        clippy::too_many_lines,
        reason = "single-source-of-truth for the impl-vs-trait sig comparison; \
                  factoring would obscure the substitution / projection / \
                  renaming order that the comparison depends on"
    )]
    pub(in crate::check) fn check_impl_method_against_trait(
        &mut self,
        type_name: &str,
        self_type_args: &[Ty],
        trait_bound: &TraitBound,
        method: &FnDecl,
        impl_sig: &FnSig,
    ) {
        // Two `Ty::Named` that share a name + args but disagree only on the
        // `builtin` discriminator denote the same nominal type: the tag is a
        // derived property of the name, stamped when a type resolves against a
        // canonical builtin source and left `None` when the same name resolves
        // against its in-scope user definition. The std dual-surface error
        // enums (`CloseError`, `SendError`, …) can hit this when a trait method
        // carries the local-enum form (`builtin: None`) while an implementation
        // resolves the same name through a builtin surface.
        // Re-derive the tag from the name on both sides so trait-conformance
        // compares nominal identity rather than the incidental resolution path.
        //
        // Under qualified-by-default the trait declaration records its sibling
        // types by their BARE name (as written inside the defining module) while
        // an importer's `impl` spells the same type through its module qualifier
        // (`module.CloseError`). These name the one type, so both spellings
        // must canonicalize to a single DEFINING-MODULE-qualified identity before
        // the comparison — never to a bare name. Stripping any known-module
        // prefix and comparing bare names is unsound: it collapses two distinct
        // nominal types that merely share a bare name across modules
        // (`closableerr.CloseError` vs `closableerr2.CloseError`), accepting an
        // impl that returns the wrong module's type. Instead:
        //   * an already module-qualified name keeps its qualifier (it is an
        //     explicit, unambiguous identity);
        //   * a bare name written in the TRAIT DECLARATION denotes the trait's
        //     own defining module's type, so it ALWAYS qualifies against that
        //     module when the module defines it. The trait side is canonicalized
        //     with `preserve_local_shadow = false` — the importer's local type
        //     names are irrelevant to what the trait declaration requires;
        //   * a bare name on the IMPL/ACTUAL side is canonicalized with
        //     `preserve_local_shadow = true`: if it shadows a local type in the
        //     impl's scope it stays bare so the local identity is preserved (and
        //     so a local `CloseError` correctly MISMATCHES the trait's
        //     `closableerr.CloseError` rather than being conflated with it).
        //
        // The carve-out MUST be side-specific. Applying the local-shadow filter
        // to BOTH sides with one shared predicate is fail-open: the trait's bare
        // `CloseError` would also be left bare when the importer has a local
        // `CloseError`, so it would compare EQUAL to the impl's local type
        // instead of to the trait owner's required `closableerr.CloseError`,
        // falsely accepting a wrong-module impl.
        //
        // The user-facing diagnostics still render the original, untouched types.
        //
        // `trait_owner` is the trait's defining module (`Some("closableerr")`)
        // or `None` for a root/local trait. `ctx` carries the in-scope module
        // set, the registered-type predicate, and the local-shadow predicate so
        // the recursion needs no `&self` borrow held across the later mutable
        // error-reporting calls. `preserve_local_shadow` selects the side.
        fn canonicalize_type_identity(
            ty: &Ty,
            ctx: &TraitSigCanonCtx,
            preserve_local_shadow: bool,
        ) -> Ty {
            let rec = |t: &Ty| canonicalize_type_identity(t, ctx, preserve_local_shadow);
            match ty {
                Ty::Tuple(elems) => Ty::Tuple(elems.iter().map(rec).collect()),
                Ty::Array(elem, n) => Ty::Array(Box::new(rec(elem)), *n),
                Ty::Slice(elem) => Ty::Slice(Box::new(rec(elem))),
                Ty::Named { name, args, .. } => {
                    let canonical = if ctx.modules.iter().any(|module| {
                        name.strip_prefix(module)
                            .is_some_and(|suffix| suffix.starts_with('.'))
                    }) {
                        // Already module-qualified by a known module: keep the
                        // complete qualifier as the type identity. Module
                        // owners may themselves be dotted package paths (for
                        // example `hew.closableerr`), so looking only at the
                        // first path segment would mistake
                        // `hew.closableerr.CloseError` for a bare type.
                        name.clone()
                    } else {
                        // Bare name: qualify against the trait's defining module
                        // when that module defines it. On the impl/actual side a
                        // local shadow is preserved (left bare); on the trait
                        // side the local-shadow carve-out does NOT apply, so a
                        // bare trait-declared name always qualifies to its owner.
                        // Otherwise (builtin, type param, or — on the impl side —
                        // a genuine local) leave it bare so its identity survives.
                        ctx.trait_owner
                            .filter(|_| !(preserve_local_shadow && (ctx.is_local)(name)))
                            .map(|owner| format!("{owner}.{name}"))
                            .filter(|qualified| (ctx.defines_qualified)(qualified))
                            .unwrap_or_else(|| name.clone())
                    };
                    // Primitive types (i64, bool, f64, …) are represented in two
                    // ways: as the flat `Ty::I64` / `Ty::Bool` / … variants (from
                    // `resolve_type_expr` hitting the `Ty::from_name` fast-path)
                    // and as `Ty::Named { name: "i64", builtin: Some(I64), … }`
                    // (from `Ty::normalize_named` when a `Self` annotation is
                    // eagerly substituted via `current_self_type` during
                    // `lookup_trait_method` resolution).  Both representations are
                    // semantically identical, but `Ty::Named { … } != Ty::I64` as
                    // Rust enum discriminants, so trait-impl signature comparison
                    // falsely rejects them.
                    //
                    // Collapsing the canonical name to the flat primitive variant
                    // here is the canonical reconcile point: both the expected
                    // (trait) and actual (impl) sides pass through this function
                    // before comparison, so a single normalization here handles
                    // every path (fn_sigs registered before impl, lookup_trait_method
                    // eager substitution, and substitute_trait_sig_for_impl output).
                    // Only fires for zero-arg names (primitives never carry type args).
                    let canonical_args = args.iter().map(rec).collect::<Vec<_>>();
                    if canonical_args.is_empty() {
                        if let Some(prim) = Ty::from_name(&canonical) {
                            return prim;
                        }
                    }
                    Ty::normalize_named(canonical, canonical_args)
                }
                Ty::Function {
                    capabilities,
                    params,
                    ret,
                } => Ty::Function {
                    capabilities: *capabilities,
                    params: params.iter().map(rec).collect(),
                    ret: Box::new(rec(ret)),
                },
                Ty::Closure {
                    capabilities,
                    params,
                    ret,
                    captures,
                    identity,
                } => Ty::Closure {
                    capabilities: *capabilities,
                    params: params.iter().map(rec).collect(),
                    ret: Box::new(rec(ret)),
                    captures: captures.iter().map(rec).collect(),
                    identity: identity.clone(),
                },
                Ty::Pointer {
                    is_mutable,
                    pointee,
                } => Ty::Pointer {
                    is_mutable: *is_mutable,
                    pointee: Box::new(rec(pointee)),
                },
                Ty::Borrow { pointee } => Ty::Borrow {
                    pointee: Box::new(rec(pointee)),
                },
                Ty::Task(inner) => Ty::Task(Box::new(rec(inner))),
                other => other.clone(),
            }
        }

        let trait_name = trait_bound.name.clone();

        // Resolve the trait as written in the impl (`impl C for X`) to its
        // OWNER-QUALIFIED identity, uniformly across all three reference kinds
        // (aliased / imported-bare, local-root shadow, unambiguous single-owner
        // import). The bare `Trait::method` key is first-write-wins and pollutes
        // under same-name collisions, so it is NEVER the authority when the
        // identity resolves to a real owner or a local trait. See
        // `resolve_trait_conformance_identity`.
        let primary_identity = self.resolve_trait_conformance_identity(&trait_name);
        let primary_key = self.trait_defs_key_for_identity(&primary_identity);

        // An `impl Sub for T` (where `trait Sub: Base`) may provide an inherited
        // SUPERTRAIT method (`base`) inline. Resolve which trait actually DECLARES
        // this method — the primary trait, or the declaring supertrait reached
        // through the OWNER-QUALIFIED super chain — and key the signature check
        // off THAT trait's identity. Skipping inherited methods here is a
        // fail-open: a wrong-signature inline supermethod would never be compared.
        let Some(identity) =
            self.resolve_declaring_trait_identity(&primary_identity, &primary_key, &method.name)
        else {
            return;
        };
        // The declaring trait's `trait_defs` entry supplies the trait method AST
        // (its type params, span, and receiver shape) for the comparison below.
        let declaring_key = self.trait_defs_key_for_identity(&identity);
        let Some(trait_info) = self.trait_defs.get(&declaring_key).cloned() else {
            return;
        };
        let Some(trait_method) = trait_info
            .methods
            .iter()
            .find(|m| m.name == method.name)
            .cloned()
        else {
            return;
        };

        // Materialise the trait method's required signature through the resolved
        // identity, collision-free:
        //   * a resolved owner reads the owner-qualified `fn_sigs` key
        //     `m.Trait::method` (always registered for module traits);
        //   * a LOCAL trait derives the signature from its own `TraitInfo`
        //     method AST — the polluted bare `fn_sigs` key may hold an imported
        //     same-name trait's signature (first-write-wins), so it must never be
        //     consulted for a local trait;
        //   * an unresolved reference falls back to the scoped/bare key (the
        //     genuinely-unambiguous case, where no collision is possible).
        let trait_sig = if let Some(owner) = identity.owner.as_ref() {
            let owner_key = format!("{owner}.{}::{}", identity.source_trait_name, method.name);
            match self.fn_sigs.get(&owner_key).cloned() {
                Some(sig) => sig,
                None => return,
            }
        } else if identity.is_local {
            // A local/root trait resolves its required signature from its own
            // `trait_defs` entry (last-write-wins → authoritative) rather than the
            // polluted bare `fn_sigs` key. `lookup_trait_method` strips the
            // receiver and projects `Self::Bar`, mirroring what
            // `register_trait_method_sig` would have written. The DECLARING trait
            // is keyed (`identity.source_trait_name`): for an inline supermethod
            // of a local sub-trait this is the supertrait that declares it, not
            // the sub-trait written in the impl.
            match self.lookup_trait_method(&identity.source_trait_name, &method.name) {
                Some(sig) => sig,
                None => return,
            }
        } else {
            let trait_method_key = format!("{}::{}", identity.source_trait_name, method.name);
            let scoped_trait_key =
                scoped_module_item_name(self.current_module.as_deref(), &trait_method_key)
                    .unwrap_or_else(|| trait_method_key.clone());
            match self
                .fn_sigs
                .get(&scoped_trait_key)
                .or_else(|| self.fn_sigs.get(&trait_method_key))
                .cloned()
            {
                Some(sig) => sig,
                None => return,
            }
        };

        // The trait's defining module anchors how a bare type name written in
        // the trait declaration is canonicalized. Imported traits carry that
        // owner explicitly. A trait local to a non-root module deliberately
        // resolves as `is_local`, but its sibling type names still belong to
        // the exact current module; only a root-local trait has no owner. For an
        // inline supermethod this is the SUPERTRAIT's owner (its declaration's
        // bare sibling types belong to its module), not the sub-trait's.
        let trait_owner_module = identity.owner.as_ref().map_or_else(
            || {
                identity
                    .is_local
                    .then(|| self.current_module.clone())
                    .flatten()
            },
            |owner| {
                Some(
                    self.module_import_bindings
                        .get(&(
                            self.current_module.clone(),
                            self.current_module_idx,
                            owner.clone(),
                        ))
                        .cloned()
                        .unwrap_or_else(|| owner.clone()),
                )
            },
        );

        // Build trait-type-param substitution map.
        let mut trait_param_map: HashMap<String, Ty> = HashMap::new();
        if let Some(args) = trait_bound.type_args.as_ref() {
            for (param_name, arg_expr) in trait_info.type_params.iter().zip(args.iter()) {
                let resolved = self.resolve_type_expr(arg_expr);
                trait_param_map.insert(param_name.clone(), resolved);
            }
        } else if self
            .lang_items
            .get("index")
            .is_some_and(|binding| binding.trait_id.full_path() == declaring_key)
            && trait_info.type_params.len() == 1
            && matches!(method.name.as_str(), "get" | "at")
        {
            // Legacy `impl Index for T` elides `Index<Idx>` and determines
            // `Idx` from the handler's concrete index parameter. Keep that
            // compatibility at the exact lang-item declaration only; applying
            // it to arbitrary generic traits would silently infer omitted
            // arguments from whichever method happened to be checked first.
            if let Some(actual_index_ty) = impl_sig.params.first() {
                trait_param_map.insert(
                    trait_info.type_params[0].clone(),
                    self.subst.resolve(actual_index_ty),
                );
            }
        }

        // Construct `impl_self` as the canonical `Ty` for the implementing
        // type. For primitive types (i64, bool, f64, …) `Ty::from_name`
        // returns the flat primitive variant (e.g. `Ty::I64`), which is what
        // the impl's annotation resolves to via
        // `resolve_registered_annotation_ty_no_holes`.  Using `Ty::Named` for
        // a primitive name produces a different enum variant than the impl's
        // resolved param type, causing a false "has type i64 but requires i64"
        // mismatch on non-receiver Self params.  `Ty::from_name` is the single
        // source of truth for primitive name → variant; non-primitive names
        // that have no flat variant (user-defined types, generics) take the
        // `Ty::Named` path as before.  Primitive types never carry type args,
        // so the from_name path only fires when self_type_args is empty.
        let impl_self_name = self
            .current_module
            .as_deref()
            .filter(|_| !type_name.contains('.'))
            .map(|module| format!("{module}.{type_name}"))
            .filter(|qualified| self.type_defs.contains_key(qualified))
            .unwrap_or_else(|| type_name.to_string());
        let impl_self = if self_type_args.is_empty() {
            Ty::from_name(&impl_self_name).unwrap_or_else(|| Ty::Named {
                builtin: None,
                name: impl_self_name.clone(),
                args: Vec::new(),
            })
        } else {
            Ty::Named {
                builtin: None,
                name: impl_self_name.clone(),
                args: self_type_args.to_vec(),
            }
        };

        // Materialise the expected impl-side signature.
        let expected_params: Vec<Ty> = trait_sig
            .params
            .iter()
            .map(|p| {
                let projected = self.substitute_trait_sig_for_impl(p, &impl_self, &trait_param_map);
                Self::rename_method_type_params(
                    &projected,
                    trait_method.type_params.as_ref(),
                    method.type_params.as_ref(),
                )
            })
            .collect();
        let expected_return = {
            let projected = self.substitute_trait_sig_for_impl(
                &trait_sig.return_type,
                &impl_self,
                &trait_param_map,
            );
            Self::rename_method_type_params(
                &projected,
                trait_method.type_params.as_ref(),
                method.type_params.as_ref(),
            )
        };

        // Cascading-Ty::Error suppression: if anything in expected or actual
        // is Error, skip — earlier diagnostics already explain the failure.
        let any_error = expected_params.iter().any(Ty::contains_error)
            || expected_return.contains_error()
            || impl_sig.params.iter().any(Ty::contains_error)
            || impl_sig.return_type.contains_error();
        if any_error {
            return;
        }

        let report_span = if method.decl_span.start != method.decl_span.end {
            method.decl_span.clone()
        } else if method.fn_span.start != method.fn_span.end {
            method.fn_span.clone()
        } else {
            // Defensive: if the parser left both blank, fall back to the trait
            // method span so the message still anchors to a real source range.
            trait_method.span.clone()
        };

        if trait_sig.consumes_receiver != impl_sig.consumes_receiver {
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.clone(),
                    detail: "receiver ownership",
                },
                &report_span,
                format!(
                    "impl method `{type_name}.{}` has a different receiver ownership \
                     contract than trait `{trait_name}`; `consume self` must match exactly",
                    method.name
                ),
                &trait_method.span,
                format!("trait method `{trait_name}.{}` declared here", method.name),
            );
            return;
        }

        if trait_sig.returns_receiver_identity != impl_sig.returns_receiver_identity {
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.clone(),
                    detail: "receiver identity",
                },
                &report_span,
                format!(
                    "impl method `{type_name}.{}` has a different `#[returns_receiver]` \
                     contract than trait `{trait_name}`; exact receiver/result ownership \
                     identity must match",
                    method.name
                ),
                &trait_method.span,
                format!("trait method `{trait_name}.{}` declared here", method.name),
            );
            return;
        }

        if expected_params.len() != impl_sig.params.len() {
            // Arity mismatch — also fires when the impl wrote a different
            // receiver shape (e.g. `(it: X)` vs `(self)`), because the impl's
            // non-Self first param is not detected as a receiver and so is
            // *not* skipped, producing a different post-skip arity.
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.clone(),
                    detail: "arity",
                },
                &report_span,
                format!(
                    "impl method `{type_name}.{}` has {} parameter(s) but trait `{trait_name}` declares {} \
                     (after substituting `Self` and projecting associated types)",
                    method.name,
                    impl_sig.params.len(),
                    expected_params.len(),
                ),
                &trait_method.span,
                format!(
                    "trait method `{trait_name}.{}` declared here",
                    method.name
                ),
            );
            return;
        }

        // A parameter name is part of the method's API: a named call through
        // the trait and one on the concrete type must bind alike. An impl may
        // mark a parameter unused as `_name`; callers still label it `name`.
        if let Some((trait_param, impl_param)) = trait_sig
            .param_names
            .iter()
            .zip(&impl_sig.param_names)
            .find(|(trait_param, impl_param)| {
                trait_param != impl_param
                    && impl_param.strip_prefix('_') != Some(trait_param.as_str())
            })
        {
            self.report_error_with_note(
                TypeErrorKind::ImplParamNameMismatch,
                &report_span,
                format!(
                    "impl method `{type_name}.{}` names parameter `{impl_param}` where trait `{trait_name}` names it `{trait_param}`",
                    method.name,
                ),
                &trait_method.span,
                format!("trait method `{trait_name}.{}` declared here", method.name),
            );
            return;
        }

        // Receiver-mutability axis (Q297 Stage 1): when both sides declare a
        // receiver, the `is_mutable` flag must match. A trait declaring
        // `fn next(var self)` and an impl declaring `fn next(self)` (or vice
        // versa) is a hard reject — the receiver-mutability axis is part of
        // the contract, not a free parameter the impl may choose.
        //
        // Determine each side's receiver-mutability flag by checking the
        // first parameter for receiver-shape. This mirrors how
        // `register_impl_method` and `register_fn_sig_with_name` already
        // detect-and-skip receivers when building the signature's params
        // list; the receiver's `is_mutable` flag is otherwise dropped on
        // the floor, which is precisely the contract gap this check closes.
        let trait_receiver_mut = trait_method
            .params
            .first()
            .is_some_and(|p| self.is_receiver_param(p) && p.is_mutable);
        let impl_receiver_mut = method
            .params
            .first()
            .is_some_and(|p| self.is_receiver_param(p) && p.is_mutable);
        if trait_receiver_mut != impl_receiver_mut {
            let (trait_shape, impl_shape) = if trait_receiver_mut {
                (
                    "`var self` (mutable receiver)",
                    "`self` (by-value receiver)",
                )
            } else {
                (
                    "`self` (by-value receiver)",
                    "`var self` (mutable receiver)",
                )
            };
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.clone(),
                    detail: "receiver mutability",
                },
                &report_span,
                format!(
                    "impl method `{type_name}.{}` declares {impl_shape} but trait `{trait_name}` requires {trait_shape}",
                    method.name,
                ),
                &trait_method.span,
                format!(
                    "trait method `{trait_name}.{}` declared here",
                    method.name
                ),
            );
            return;
        }

        // Canonicalize every comparison type to a defining-module-qualified
        // identity up front, holding the read-only `self` borrow only for this
        // block so the later mutable error reporting is unencumbered. The owned
        // canonical `Ty` values then drive the comparisons; the diagnostics
        // still render the original, un-canonicalized spellings.
        let (
            canon_expected_params,
            canon_actual_params,
            canon_expected_return,
            canon_actual_return,
        ) = {
            let ctx = TraitSigCanonCtx {
                modules: &self.modules,
                trait_owner: trait_owner_module.as_deref(),
                defines_qualified: &|qualified: &str| self.type_defs.contains_key(qualified),
                is_local: &|name: &str| {
                    self.local_type_defs.contains(name) || self.source_type_defs.contains(name)
                },
            };
            // EXPECTED is the trait declaration's required signature: a bare
            // name there denotes the trait owner's sibling type, so it ALWAYS
            // qualifies to the owner (`preserve_local_shadow = false`). The
            // importer's local type names do not change what the trait requires.
            let canon_expected_params: Vec<Ty> = expected_params
                .iter()
                .map(|t| canonicalize_type_identity(&self.normalize_for_use(t), &ctx, false))
                .collect();
            // ACTUAL is the impl's written signature: a bare name that shadows a
            // local type keeps its local identity (`preserve_local_shadow =
            // true`), so a local `CloseError` correctly mismatches the trait's
            // `closableerr.CloseError` instead of being conflated with it.
            let canon_actual_params: Vec<Ty> = impl_sig
                .params
                .iter()
                .map(|t| canonicalize_type_identity(&self.normalize_for_use(t), &ctx, true))
                .collect();
            let canon_expected_return =
                canonicalize_type_identity(&self.normalize_for_use(&expected_return), &ctx, false);
            let canon_actual_return = canonicalize_type_identity(
                &self.normalize_for_use(&impl_sig.return_type),
                &ctx,
                true,
            );
            (
                canon_expected_params,
                canon_actual_params,
                canon_expected_return,
                canon_actual_return,
            )
        };

        for (i, (expected, actual)) in expected_params
            .iter()
            .zip(impl_sig.params.iter())
            .enumerate()
        {
            if canon_expected_params[i] != canon_actual_params[i] {
                let param_label = impl_sig.param_names.get(i).map_or_else(
                    || format!("parameter {}", i + 1),
                    |n| format!("parameter `{n}`"),
                );
                self.report_error_with_note(
                    TypeErrorKind::TraitImplSignatureMismatch {
                        trait_name: trait_name.clone(),
                        method_name: method.name.clone(),
                        detail: "parameter",
                    },
                    &report_span,
                    format!(
                        "impl method `{type_name}.{}` {param_label} has type `{}` but trait `{trait_name}` \
                         requires `{}`",
                        method.name,
                        actual.user_facing(),
                        expected.user_facing(),
                    ),
                    &trait_method.span,
                    format!(
                        "trait method `{trait_name}.{}` declared here",
                        method.name
                    ),
                );
                return;
            }
        }

        if canon_expected_return != canon_actual_return {
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.clone(),
                    detail: "return type",
                },
                &report_span,
                format!(
                    "impl method `{type_name}.{}` returns `{}` but trait `{trait_name}` requires `{}`",
                    method.name,
                    impl_sig.return_type.user_facing(),
                    expected_return.user_facing(),
                ),
                &trait_method.span,
                format!(
                    "trait method `{trait_name}.{}` declared here",
                    method.name
                ),
            );
        }
    }

    pub(in crate::check) fn trait_impl_type_identity(&self, type_name: &str) -> String {
        self.canonical_primitive_or_builtin_key_for_impl_name(type_name)
            .or_else(|| {
                crate::builtin_enums::canonical_monomorphic_builtin_enum_identity(type_name)
                    .map(ToString::to_string)
            })
            .unwrap_or_else(|| {
                if crate::lookup_builtin_type(type_name).is_some() {
                    return type_name.to_string();
                }
                self.current_module
                    .as_ref()
                    .filter(|_| !type_name.contains('.'))
                    .map_or_else(
                        || type_name.to_string(),
                        |module| format!("{module}.{type_name}"),
                    )
            })
    }

    pub(in crate::check) fn trait_impl_method_declaration(
        &self,
        ty: &Ty,
        trait_name: &str,
        method_name: &str,
    ) -> Option<(crate::DefId, String)> {
        let Ty::Named { name, args, .. } = ty else {
            return None;
        };
        let type_identity = self.trait_impl_type_identity(name);
        let trait_identity = self.trait_defs_key_for_bound(trait_name);
        let args = args
            .iter()
            .map(|ty| ResolvedTy::from_ty(&self.subst.resolve(ty)).ok())
            .collect::<Option<Vec<_>>>()
            .unwrap_or_default();
        crate::type_facts::selected_impl_method(
            &self.trait_impl_method_declaration_ids,
            &type_identity,
            &args,
            &trait_identity,
            method_name,
        )
        .map(|(declaration, owner)| {
            (
                declaration,
                Self::method_declaration_key(&owner, method_name),
            )
        })
    }

    pub(in crate::check) fn record_trait_impl(&mut self, type_name: &str, trait_name: &str) {
        let type_identity = self.trait_impl_type_identity(type_name);
        let trait_identity = self.trait_defs_key_for_bound(trait_name);
        self.trait_impls_set.insert((type_identity, trait_identity));
    }

    pub(in crate::check) fn record_trait_impl_methods(
        &mut self,
        type_name: &str,
        trait_name: &str,
        method_names: impl IntoIterator<Item = String>,
    ) {
        let type_identity = self.trait_impl_type_identity(type_name);
        let trait_identity = self.trait_defs_key_for_bound(trait_name);
        let entry = self
            .trait_impl_method_names
            .entry((type_identity, trait_identity))
            .or_default();
        entry.extend(method_names);
    }

    /// Record an `impl <Trait> for <PrimitiveOrBuiltinGeneric>` method in the
    /// side table.  `canonical_key` must come from
    /// [`Self::canonical_primitive_or_builtin_key_from_name`] so registration
    /// and dispatch agree.
    pub(in crate::check) fn record_primitive_trait_impl_method(
        &mut self,
        canonical_key: String,
        trait_name: &str,
        method_name: String,
        sig: FnSig,
    ) {
        // First-wins, matching `record_primitive_trait_impl_self_args` and the
        // assoc-type binding table. Under Hew's coherence rule there is at most
        // one impl of a trait per constructor, so a second registration is
        // either the same impl reprocessed across phases or a rejected
        // conflicting impl (diagnosed at `record_trait_impl`); in neither case
        // may it overwrite the surviving impl's signature. Keeping every side
        // table first-wins guarantees the dispatched method signature and the
        // applicability proof (self-args) always come from the *same* impl, so
        // they cannot drift.
        let trait_identity = self.trait_defs_key_for_bound(trait_name);
        self.primitive_trait_impls
            .entry((canonical_key, trait_identity))
            .or_default()
            .entry(method_name)
            .or_insert(sig);
    }

    /// Record the impl's `Self` type arguments for an `impl <Trait> for
    /// <PrimitiveOrBuiltinGeneric>` so a later dispatch on a concrete receiver
    /// can bind the impl's type parameters (see
    /// [`Checker::primitive_trait_impl_self_args`]). Idempotent within one impl:
    /// every method of that impl records the SAME `Self` args, so the first
    /// recorded entry per `(canonical, trait)` wins.
    ///
    /// Coherence enforcement (single-impl-per-constructor): if a *different*
    /// `Self` shape is already recorded for this `(canonical, trait)`, two
    /// distinct impls target the same builtin constructor with the same trait —
    /// e.g. a blanket `impl<T> Acc for Vec<T>` (`self_args = [T]`) and a concrete
    /// `impl Acc for Vec<i64>` (`self_args = [i64]`). Hew has no specialization
    /// or overlapping impls (single-crate coherence, mission Q66.b), so the
    /// second impl is rejected at its declaration site with a clean diagnostic
    /// and does NOT overwrite the first (first-wins keeps the surviving impl's
    /// method signatures and this `Self`-arg applicability proof from the SAME
    /// impl, so they cannot drift).
    ///
    /// Comparing the `Self` shape (rather than a source span) makes this robust
    /// to the registration architecture re-processing the same impl across
    /// module/import phases: a reprocessed impl re-presents an *identical*
    /// `Self` shape and is correctly treated as the same impl, while a genuine
    /// overlap presents a *different* shape. It also naturally scopes the check
    /// to builtin/primitive receivers — user-record impls never reach this side
    /// table — which is exactly where the drift fail-open lived.
    ///
    /// KNOWN GAP (tracked): two *genuinely-distinct* impls that share an
    /// *identical* `Self` shape — e.g. two literal `impl<T> Acc for Vec<T>`
    /// blocks — are NOT rejected here (they compare shape-equal and are treated
    /// as a re-presentation). Closing this needs the impl's DEFINING identity,
    /// but the only readily-available per-impl span (`impl.target_type` span)
    /// cannot be used as the coherence key because the documented user-redeclare
    /// path lets a user `pub trait Display` shadow the prelude `Display`: the
    /// prelude `impl Display for i64` and the user `impl Display for i64` are
    /// distinct traits that collapse to the same `(canonical, "Display")` key,
    /// so a defining-span key would falsely reject that legal shadow. A correct
    /// fix must additionally key on the trait's DEFINING identity (not its
    /// name), which is a larger cross-cutting change tracked as a separate
    /// follow-up ("trait-impl coherence: reject duplicate same-(type,trait)
    /// impls via defining-identity"). The projection fix this method supports is
    /// unaffected: first-wins keeps the dispatched method signature and the
    /// applicability proof from the SAME (first) impl, so even an accepted
    /// duplicate cannot cause the mis-projection fail-open this fix closes.
    pub(in crate::check) fn record_primitive_trait_impl_self_args(
        &mut self,
        canonical_key: String,
        trait_name: &str,
        self_args: Vec<Ty>,
        impl_span: &Span,
    ) {
        let trait_identity = self.trait_defs_key_for_bound(trait_name);
        match self
            .primitive_trait_impl_self_args
            .get(&(canonical_key.clone(), trait_identity.clone()))
        {
            None => {
                self.primitive_trait_impl_self_args
                    .insert((canonical_key, trait_identity), self_args);
            }
            Some(existing) if existing == &self_args => {
                // Same impl (same Self shape), re-presented across a later
                // registration phase — not a conflict. (See KNOWN GAP above:
                // this also admits a genuine same-shape duplicate, tracked.)
            }
            Some(_) => {
                // A second, structurally-different impl of the same trait on the
                // same builtin constructor: an overlapping impl, which Hew does
                // not permit. Reject it; keep the first-registered impl.
                let dedup = (
                    canonical_key.clone(),
                    trait_name.to_string(),
                    impl_span.start,
                    impl_span.end,
                );
                if self.conflicting_trait_impl_reported.insert(dedup) {
                    self.report_error(
                        TypeErrorKind::ConflictingTraitImpl {
                            trait_name: trait_name.to_string(),
                            type_name: canonical_key.clone(),
                        },
                        impl_span,
                        format!(
                            "conflicting implementation of trait `{trait_name}` for `{canonical_key}`: \
                             a trait may be implemented at most once per type constructor \
                             (Hew has no specialization or overlapping impls)"
                        ),
                    );
                }
            }
        }
    }

    /// Look up a method on the primitive/builtin-generic impl table.
    ///
    /// Walks every trait registered for the receiver's canonical kind and
    /// returns the first method whose name matches.  Returns the resolved
    /// `FnSig` (receiver already filtered) plus the trait name that
    /// provided it, so callers can record dispatch metadata keyed on the
    /// resolved trait rather than re-deriving it from a name string.
    #[must_use]
    pub(in crate::check) fn lookup_primitive_trait_method(
        &self,
        receiver_ty: &Ty,
        method: &str,
    ) -> Option<(String, FnSig)> {
        let canonical = Self::canonical_primitive_or_builtin_key(receiver_ty)?;
        let mut candidates: Vec<(&String, &FnSig)> = self
            .primitive_trait_impls
            .iter()
            .filter_map(|((rx_key, trait_name), methods)| {
                if rx_key != &canonical {
                    return None;
                }

                // A local trait declaration shadows an imported/prelude trait
                // with the same source leaf. Canonical keys make those impls
                // coexist in this global side table, so hide the shadowed owner
                // instead of letting HashMap iteration choose nondeterministically.
                let source_name = crate::short_name(trait_name);
                if self.local_trait_defs.contains(source_name) {
                    let local_key = self.current_module.as_ref().map_or_else(
                        || source_name.to_string(),
                        |module| format!("{module}.{source_name}"),
                    );
                    if trait_name != &local_key {
                        return None;
                    }
                }
                methods.get(method).map(|sig| (trait_name, sig))
            })
            .collect();
        // Multiple genuinely distinct visible traits can still declare the
        // same receiver method. Keep selection stable until the language grows
        // an explicit ambiguity diagnostic for this receiver-form surface.
        candidates.sort_unstable_by_key(|(trait_name, _)| trait_name.as_str());
        candidates
            .into_iter()
            .next()
            .map(|(trait_name, sig)| (trait_name.clone(), sig.clone()))
    }
}
