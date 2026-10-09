//! Checker methods grouped by responsibility: traits.
//! Split from `registration.rs`: checker methods, part 1 of 6.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
use super::super::types::{ImportBindingKey, SpawnKey};
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::BuiltinType;
use hew_parser::ast::Ident;
use hew_parser::ast::WireMetadata;

impl Checker {
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
        let type_params = self.declaration_parameter_heads(identity);
        let scope = self.enter_primary_sig_scope(&[(Some(&sd.type_params), None)]);
        let fields = sd
            .params
            .iter()
            .map(|param| (param.name.to_string(), self.resolve_type_expr(&param.ty)))
            .collect();
        let mut bounds =
            self.collect_type_param_bounds(Some(&sd.type_params), None, &mut Vec::new());
        let send = TraitRef::bare(crate::DefTable::predicate(crate::Predicate::Send));
        for parameter in &type_params {
            bounds.push(parameter.id, send.clone());
        }
        self.insert_type_def(
            identity,
            TypeDef {
                kind: TypeDefKind::Supervisor,
                name: identity.to_string(),
                type_params,
                bounds,
                fields,
                field_order: sd
                    .params
                    .iter()
                    .map(|param| param.name.to_string())
                    .collect(),
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
            // TRANSITION(A1c4): WHY the child actor is still named by its
            // rendered spelling. WHEN supervisor children resolve through
            // `Scope`, the child is its declaration. WHAT: resolve the
            // written path to the actor's `NominalId`.
            let child_type = self.canonical_supervisor_child_type(&c.actor_type.to_string());
            let entry = (
                c.name.to_string(),
                self.named_ty_for_key(&child_type, type_args),
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
        self.actor_spawn_args.insert(
            identity.to_string(),
            sd.params
                .iter()
                .map(|param| SpawnKey {
                    name: param.name.name,
                    required: true,
                })
                .collect(),
        );
        self.exit_primary_sig_scope(scope);
        self.known_types.insert(identity.to_string());
    }

    pub(in crate::check) fn register_actor_decl(&mut self, ad: &ActorDecl) {
        let identity = ad.name;
        self.register_actor_decl_as(ad, identity.name.as_str());
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
        let type_param_names = self.declaration_parameter_heads(identity);
        let mut type_param_bounds =
            self.collect_type_param_bounds(Some(&ad.type_params), None, &mut Vec::new());
        let send = TraitRef::bare(crate::DefTable::predicate(crate::Predicate::Send));
        for parameter in &type_param_names {
            type_param_bounds.push(parameter.id, send.clone());
        }
        let has_type_params = !type_param_names.is_empty();
        if has_type_params {
            self.current_type_param_bounds
                .push(type_param_bounds.clone());
        }

        let mut fields = HashMap::new();
        let mut field_order: Vec<String> = Vec::new();
        let mut hole_vars = Vec::new();
        for field in &ad.fields {
            let field_ty = self.resolve_registered_annotation_ty(&field.ty, &mut hole_vars);
            field_order.push(field.name.to_string());
            fields.insert(field.name.to_string(), field_ty);
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
        self.registry.register_actor(
            self.head_of_declaration(
                self.nominal_head_for_key(identity)
                    .expect("actor has a declaration")
                    .id,
            ),
        );

        self.insert_type_def(identity, type_def);
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
                        name: p.name.to_string(),
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
                .filter(|field| field.default.is_none() && assigned.contains(&field.name.name))
                .map(|field| field.name.to_string())
                .collect()
        });
        self.actor_deferred_fields
            .insert(identity.to_string(), deferred.clone());
        let mut spawn_args = ad
            .fields
            .iter()
            .filter(|field| !deferred.iter().any(|name| name == field.name.name.as_str()))
            .map(|field| SpawnKey {
                name: field.name.name,
                required: field.default.is_none(),
            })
            .collect::<Vec<_>>();
        if let Some(init) = &ad.init {
            spawn_args.extend(init.params.iter().map(|parameter| SpawnKey {
                name: parameter.name.name,
                required: true,
            }));
        }
        self.actor_spawn_args
            .insert(identity.to_string(), spawn_args);
        self.record_type_def_inference_holes(identity, hole_vars);
    }

    pub(in crate::check) fn trait_info_from_decl(
        &mut self,
        tr: &TraitDecl,
        source_module: Option<String>,
        file_index: u32,
    ) -> TraitInfo {
        self.trait_info_from_decl_with_diagnostics(tr, source_module, file_index, &mut Vec::new())
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
                        trait_name: td.name.to_string(),
                        trait_id,
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
                            &format!("{}::{}", self.defs.path(trait_id), m.name),
                            &m.span,
                        );
                        self.lang_items.insert(
                            key.clone(),
                            crate::LangItemBinding {
                                trait_name: td.name.to_string(),
                                trait_id,
                                method_name: Some(m.name.to_string()),
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
        let trait_path = self.declaration_identity(td.name.name.as_str());
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
                        trait_name: td.name.to_string(),
                        trait_id,
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
                            &format!("{}::{}", self.defs.path(trait_id), m.name),
                            &m.span,
                        );
                        self.lang_items.insert(
                            key.clone(),
                            crate::LangItemBinding {
                                trait_name: td.name.to_string(),
                                trait_id,
                                method_name: Some(m.name.to_string()),
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
        &mut self,
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
                    if let Some(prev_span) = seen_assoc.insert(name.to_string(), span.clone()) {
                        errors.push(TypeError::duplicate_definition(
                            span.clone(),
                            name.name.as_str(),
                            prev_span,
                        ));
                        continue;
                    }
                    let bounds = bounds
                        .iter()
                        .filter_map(|bound| self.resolve_written_bound(name.name, bound))
                        .collect();
                    associated_types.push(TraitAssociatedTypeInfo {
                        name: name.to_string(),
                        bounds,
                        default: default.clone(),
                        span: span.clone(),
                    });
                }
            }
        }
        let declaration_name = source_module.as_ref().map_or_else(
            || tr.name.to_string(),
            |module| format!("{module}.{}", tr.name),
        );
        let type_params = if tr.type_params.as_ref().is_none_or(Vec::is_empty) {
            Vec::new()
        } else {
            self.declaration_parameter_heads(&declaration_name)
        };
        TraitInfo {
            source_module,
            file_index,
            methods,
            associated_types,
            type_params,
        }
    }

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
        let entries = self.build_impl_alias_entries(id);
        let impl_bounds = self.collect_type_param_bounds(
            id.type_params.as_ref(),
            id.where_clause.as_ref(),
            &mut Vec::new(),
        );
        let pushed_impl_bounds = !impl_bounds.is_empty();
        if pushed_impl_bounds {
            self.current_type_param_bounds.push(impl_bounds);
        }
        let implemented_trait = id
            .trait_bound
            .as_ref()
            .and_then(|tb| self.resolve_trait_path(&tb.path));
        let target = self.current_impl_target();
        // Populate impl_assoc_type_bindings on every enter (not gated on
        // `enforce`) so projection collapse can find bindings during
        // call-site monomorphisation even when this scope was entered by
        // a non-enforcing registration sweep. The first writer wins;
        // subsequent calls with the same impl idempotently re-resolve.
        if let (Some(trait_id), Some(key)) = (
            implemented_trait,
            target.as_ref().and_then(Self::impl_self_key),
        ) {
            let assoc_names: Vec<Symbol> = self
                .trait_info(trait_id)
                .map(|info| {
                    info.associated_types
                        .iter()
                        .map(|a| Symbol::intern(&a.name))
                        .collect()
                })
                .unwrap_or_default();
            for assoc_name in assoc_names {
                let binding_key = (key.clone(), trait_id, assoc_name);
                if self.impl_assoc_type_bindings.contains_key(&binding_key) {
                    continue;
                }
                if let Some(entry) = entries.get(assoc_name.as_str()) {
                    let expr = entry.expr.clone();
                    let resolved = self.resolve_type_expr(&expr);
                    if !matches!(resolved, Ty::Error) {
                        let parameters = self.source_parameter_heads(
                            id.type_params.as_deref().unwrap_or_default(),
                            &id.target_type.1,
                        );
                        let receiver = self.resolve_type_expr(&id.target_type);
                        self.impl_assoc_type_bindings.insert(
                            binding_key,
                            super::super::types::ImplAssociatedType {
                                ty: resolved,
                                receiver,
                                parameters,
                            },
                        );
                    }
                }
            }
        }
        if let (true, Some(trait_id)) = (enforce, implemented_trait) {
            // Snapshot trait-side data we need; cloned so we can release the
            // borrow on `self.trait_defs` before calling into the resolver /
            // bound-checker which need `&mut self`.
            let trait_snapshot = self
                .trait_info(trait_id)
                .map(|info| info.associated_types.clone());
            if let Some(associated_types) = trait_snapshot {
                let missing: Vec<TraitAssociatedTypeInfo> = associated_types
                    .iter()
                    .filter(|assoc| !entries.contains_key(&assoc.name))
                    .cloned()
                    .collect();
                let target_name_owned = target_name.to_string();
                let trait_display = self.defs.display(trait_id).to_string();
                for assoc in missing {
                    self.report_error_with_note(
                        TypeErrorKind::UndefinedType,
                        span,
                        format!(
                            "impl `{trait_display}` for `{target_name_owned}` must define associated type `{}`",
                            assoc.name
                        ),
                        &assoc.span,
                        "required associated type declared here".to_string(),
                    );
                }
                self.check_assoc_type_bounds(
                    &associated_types,
                    &entries,
                    trait_id,
                    &target_name_owned,
                );
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

    /// Resolve an impl's written target (`Filter<I, A>`) through `Scope` with
    /// the impl's own type parameters in scope, so `A` names the impl's binder
    /// and the head names the declaration the file sees, never a same-spelled
    /// type another module declares.
    pub(in crate::check) fn resolve_impl_target(&mut self, id: &ImplDecl) -> Ty {
        let bounds = self.collect_type_param_bounds(
            id.type_params.as_ref(),
            id.where_clause.as_ref(),
            &mut Vec::new(),
        );
        self.current_type_param_bounds.push(bounds);
        let resolved = self.resolve_type_expr(&id.target_type);
        self.current_type_param_bounds.pop();
        resolved
    }

    /// The trait an impl implements, with its arguments resolved like the
    /// target's (`From<Low>` in `impl From<Low> for Wrapped`). `None` when the
    /// impl is inherent or its trait path names no trait, which the method-set
    /// check reports.
    pub(in crate::check) fn impl_trait_ref(&mut self, id: &ImplDecl) -> Option<TraitRef> {
        let bound = id.trait_bound.as_ref()?;
        let trait_id = self.resolve_trait_path(&bound.path)?;
        let bounds = self.collect_type_param_bounds(
            id.type_params.as_ref(),
            id.where_clause.as_ref(),
            &mut Vec::new(),
        );
        self.current_type_param_bounds.push(bounds);
        let args = bound
            .type_args
            .iter()
            .flatten()
            .map(|arg| self.resolve_type_expr(arg))
            .collect();
        self.current_type_param_bounds.pop();
        Some(TraitRef {
            trait_id,
            args,
            assoc: Vec::new(),
        })
    }

    /// The type arguments an impl's resolved target carries.
    pub(super) fn impl_target_args(target: &Ty) -> Vec<Ty> {
        match target {
            Ty::Named { args, .. } => args.clone(),
            _ => Vec::new(),
        }
    }

    /// Enforce trait-side bounds on each impl-side associated-type binding.
    ///
    /// For `trait Foo { type Out: Display; }` and `impl Foo for X { type Out = Y; }`,
    /// verifies `Y: Display`. A `Y` that is one of the impl's own binders
    /// satisfies the bound through the impl's bounds, which are in scope.
    pub(super) fn check_assoc_type_bounds(
        &mut self,
        associated_types: &[TraitAssociatedTypeInfo],
        entries: &HashMap<String, ImplAliasEntry>,
        trait_id: crate::DefId,
        target_name: &str,
    ) {
        let trait_display = self.defs.display(trait_id).to_string();
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
            // `resolve_type_expr` already emitted the primary diagnostic.
            if matches!(resolved, Ty::Error) {
                continue;
            }
            for bound in &assoc.bounds {
                if self.type_satisfies_bound(&resolved, bound) {
                    continue;
                }
                let bound_display = self.trait_ref_display(bound);
                self.report_error(
                    TypeErrorKind::BoundsNotSatisfied,
                    &entry_span,
                    format!(
                        "associated type `{trait_display}.{}` in impl for `{target_name}` is bound by trait \
                         `{bound_display}` but `{}` does not implement `{bound_display}`",
                        assoc.name,
                        resolved.user_facing(),
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

    /// Record trait `trait_id`'s super-traits, resolved where `td` declares
    /// them.
    pub(in crate::check) fn register_trait_supers(
        &mut self,
        trait_id: crate::DefId,
        td: &TraitDecl,
    ) {
        let supers: Vec<crate::DefId> = td
            .super_traits
            .iter()
            .flatten()
            .filter_map(|bound| self.resolve_written_bound(td.name.name, bound))
            .map(|bound| bound.trait_id)
            .collect();
        self.trait_super.insert(trait_id, supers);
    }

    /// Type-parameter names declared by trait `trait_id`.
    pub(super) fn trait_type_param_names(&self, trait_id: crate::DefId) -> Vec<String> {
        self.trait_info(trait_id)
            .map(|info| {
                info.type_params
                    .iter()
                    .map(|parameter| parameter.spelling.to_string())
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Register one trait method's signature under its declaration and mint
    /// the trait's method identities. The first registration wins: an impl
    /// materializing an inherited default reaches here again.
    pub(super) fn register_trait_method_sig(
        &mut self,
        trait_id: crate::DefId,
        method: &hew_parser::ast::TraitMethod,
        span: &Span,
    ) {
        let trait_display = self.defs.display(trait_id).to_string();
        let trait_params = self.trait_type_param_names(trait_id);
        if !trait_params.is_empty() {
            let owner =
                Self::method_declaration_key(self.defs.path(trait_id), method.name.name.as_str());
            self.reject_shadowing_method_type_params(
                method.type_params.as_ref(),
                &[(trait_params, format!("trait `{trait_display}`"))],
                &owner,
                &method.span,
            );
        }
        let method_path = format!("{}::{}", self.defs.path(trait_id), method.name);
        let Some(method_id) = self.require_declaration_path(&method_path, &method.span) else {
            return;
        };
        let ids = (trait_id, method_id);
        self.trait_method_ids
            .insert(self.defs.path(method_id).to_string(), ids);
        if self.registration_is_flat_file_import {
            self.trait_method_ids_by_binding.insert(
                (
                    None,
                    self.current_module_idx,
                    self.defs.name(trait_id).to_string(),
                    method.name.to_string(),
                ),
                ids,
            );
        }
        if self.fn_sigs.contains_key(&method_id) {
            return;
        }

        // `Self.Bar` projection is active while the signature resolves so
        // `Self.Item` becomes a deferred `Ty::AssocType` carrier instead of an
        // opaque named type.
        let receiver_identity_is_valid =
            self.validate_trait_receiver_identity_method(&trait_display, method);
        let mut attributes = method.attributes.clone();
        if !receiver_identity_is_valid {
            attributes.retain(|attribute| attribute.name != "returns_receiver");
        }
        let decl = FnDecl {
            origin: hew_parser::ast::DeclarationOrigin::Authored,
            attributes,
            is_generator: false,
            visibility: hew_parser::ast::Visibility::Private,
            name: method.name,
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
            fn_span: method.span.clone(),
            intrinsic: None,
            consumes_self: method.consumes_self,
        };
        let prev_trait_self = self.current_trait_for_self_projection.replace(trait_id);
        self.register_fn_sig_with_name(&method_path, &decl, Some(method_id));
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
                TypeExpr::Named { path, type_args }
                    if path.as_single().is_some_and(|name| name.name.as_str() == "Self")
                        && type_args.as_ref().is_none_or(Vec::is_empty)
            )
        });
        let body_is_exact = method.body.as_ref().is_none_or(|body| {
            let direct_self_tail = body.trailing_expr.as_deref().is_some_and(
                |(expr, _)| matches!(expr, Expr::Ident(name) if name.name.as_str() == "self"),
            );
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
        trait_param_map: &HashMap<crate::ParamHead, Ty>,
    ) -> Ty {
        match ty {
            Ty::Named { head, args }
                if args.is_empty()
                    && matches!(head, crate::TypeHead::Param(parameter) if parameter.is_receiver()) =>
            {
                impl_self.clone()
            }
            Ty::Named {
                head: crate::TypeHead::Param(param),
                args,
            } if args.is_empty() => {
                if let Some(mapped) = trait_param_map.get(param) {
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

    pub(in crate::check) fn resolved_trait_defaults(
        &mut self,
    ) -> HashMap<crate::DefId, Vec<super::types::ResolvedTraitDefault>> {
        let saved_module = self.current_module.clone();
        let saved_file = self.current_module_idx;
        let mut declarations: Vec<(crate::DefId, TraitInfo)> = self
            .trait_defs
            .iter()
            .map(|(id, info)| (*id, info.clone()))
            .collect();
        declarations
            .sort_by(|(left, _), (right, _)| self.defs.path(*left).cmp(self.defs.path(*right)));
        let mut defaults = HashMap::new();
        for (trait_id, info) in declarations {
            self.current_module.clone_from(&info.source_module);
            self.current_module_idx = info.file_index;
            // Every method the trait reaches, inherited ones included, is
            // reachable under the trait's own path.
            let mut names: HashSet<String> =
                info.methods.iter().map(|m| m.name.to_string()).collect();
            self.collect_super_trait_method_names(trait_id, &mut names);
            for name in names {
                let Some(owner_id) = self.declaring_trait_of(trait_id, &name) else {
                    continue;
                };
                let Some(ids) = self.trait_method_ids_of(owner_id, &name) else {
                    continue;
                };
                self.trait_method_ids
                    .insert(format!("{}::{name}", self.defs.path(trait_id)), ids);
            }
            let bodies = info
                .methods
                .into_iter()
                .filter(|method| method.body.is_some())
                .filter_map(|method| {
                    let (_, method_id) =
                        self.trait_method_ids_of(trait_id, method.name.name.as_str())?;
                    Some(super::types::ResolvedTraitDefault {
                        trait_id,
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
            let prefix = format!("{}::", self.defs.path(*trait_id));
            for (key, ids) in &self.trait_method_ids {
                if let Some(method) = key.strip_prefix(&prefix) {
                    self.trait_method_ids_by_binding.insert(
                        (
                            binding.0.clone(),
                            binding.1,
                            binding.2.clone(),
                            method.to_string(),
                        ),
                        *ids,
                    );
                }
            }
        }
        self.current_module = saved_module;
        self.current_module_idx = saved_file;
        defaults
    }

    /// The set of method names a trait requires an impl to provide. A method
    /// with a default body is optional (impls may omit it), so only bodyless
    /// methods of the trait ITSELF are "required". The "known" set
    /// additionally includes the trait's whole super-trait chain: an impl of
    /// a sub-trait may legitimately provide a super-trait method inline
    /// (`trait Sub: Super` → `impl Sub for T { fn <super-method> … }`), so
    /// such a method must not be flagged as extraneous. Super-trait REQUIRED
    /// methods are NOT folded into `required` here — they are satisfied by a
    /// separate `impl Super for T` (the idiomatic form) or enforced at the
    /// bound site, and folding them in would falsely reject that
    /// separate-impl pattern.
    ///
    /// Returns `(required, known)`, or `None` when the trait is not
    /// registered.
    pub(super) fn trait_required_and_known_methods(
        &self,
        trait_id: crate::DefId,
    ) -> Option<(HashSet<String>, HashSet<String>)> {
        let info = self.trait_info(trait_id)?;
        let mut required = HashSet::new();
        let mut known = HashSet::new();
        for m in &info.methods {
            known.insert(m.name.to_string());
            if m.body.is_none() {
                required.insert(m.name.to_string());
            }
        }
        self.collect_super_trait_method_names(trait_id, &mut known);
        Some((required, known))
    }

    /// Insert every method name `trait_id`'s (transitive) super-traits
    /// declare into `known`.
    pub(super) fn collect_super_trait_method_names(
        &self,
        trait_id: crate::DefId,
        known: &mut HashSet<String>,
    ) {
        for super_trait in self.trait_closure(trait_id).into_iter().skip(1) {
            if let Some(info) = self.trait_info(super_trait) {
                known.extend(info.methods.iter().map(|m| m.name.to_string()));
            }
        }
    }

    /// The trait in `trait_id`'s chain that declares `method_name`: the
    /// trait itself, or the supertrait an inline impl method is inherited
    /// from.
    pub(super) fn declaring_trait_of(
        &self,
        trait_id: crate::DefId,
        method_name: &str,
    ) -> Option<crate::DefId> {
        self.trait_closure(trait_id).into_iter().find(|candidate| {
            self.trait_info(*candidate).is_some_and(|info| {
                info.methods
                    .iter()
                    .any(|m| m.name.name.as_str() == method_name)
            })
        })
    }

    /// Validate that an `impl <Trait> for <Type>` provides EXACTLY the trait's
    /// method set: every required (bodyless) trait method present, and no method
    /// that is not declared on the trait. The trait is the declaration the
    /// written path resolves to in the impl's scope. Per-method signature
    /// equivalence is enforced separately by `check_impl_method_against_trait`.
    pub(in crate::check) fn check_impl_method_set_against_trait(
        &mut self,
        type_name: &str,
        trait_bound: &TraitBound,
        impl_methods: &[FnDecl],
        impl_span: &Span,
    ) {
        let trait_name = &trait_bound.path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
        let trait_id = self.resolve_trait_path(&trait_bound.path);
        if let Some(declaration) = trait_id {
            self.trait_bindings.insert(
                (
                    self.current_module.clone(),
                    self.current_module_idx,
                    trait_name.clone(),
                ),
                declaration,
            );
        }
        // Compiler predicates (`Eq`, `Hash`, `Copy`, ...) carry no declared
        // method set.
        if trait_id.is_some_and(|id| crate::DefTable::as_predicate(id).is_some()) {
            return;
        }
        let Some((required, known)) =
            trait_id.and_then(|id| self.trait_required_and_known_methods(id))
        else {
            // D429: no declaration in scope defines this trait, so there is no
            // method set to check the impl against. Accepting it would register
            // the impl's methods under a contract that does not exist.
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

        let trait_id = trait_id.expect("a trait with a method set resolved");
        let mut provided: HashSet<String> =
            impl_methods.iter().map(|m| m.name.to_string()).collect();
        // Split trait surfaces are cumulative on one exact nominal identity.
        // This is how a subtrait that redeclares inherited methods is satisfied
        // by the already-registered supertrait impl for the same type.
        if let Some(key) = self
            .current_impl_target()
            .and_then(|target| Self::impl_self_key(&target))
        {
            for ((implemented_type, _), methods) in &self.trait_impl_method_names {
                if *implemented_type == key {
                    provided.extend(methods.iter().map(ToString::to_string));
                }
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
        // (`declaring_trait_of`) still validates any inline copy.
        let mut inherited: HashSet<String> = HashSet::new();
        self.collect_super_trait_method_names(trait_id, &mut inherited);

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
            .map(|m| m.name.to_string())
            .filter(|name| !known.contains(name.as_str()))
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
        trait_bound: &TraitBound,
        method: &FnDecl,
        impl_sig: &FnSig,
    ) {
        let trait_name = trait_bound.path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
        let Some(primary) = self.resolve_trait_path(&trait_bound.path) else {
            return;
        };
        // An `impl Sub for T` (where `trait Sub: Base`) may provide an inherited
        // SUPERTRAIT method (`base`) inline. Key the signature check off the
        // trait that actually DECLARES the method; skipping inherited methods
        // here is a fail-open: a wrong-signature inline supermethod would never
        // be compared.
        let Some(declaring) = self.declaring_trait_of(primary, method.name.name.as_str()) else {
            return;
        };
        let Some(trait_info) = self.trait_info(declaring).cloned() else {
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
        let Some(trait_sig) = self
            .trait_method_ids_of(declaring, method.name.name.as_str())
            .and_then(|(_, method_id)| self.fn_sigs.get(&method_id).cloned())
        else {
            return;
        };

        // Build trait-type-param substitution map.
        let mut trait_param_map: HashMap<crate::ParamHead, Ty> = HashMap::new();
        if let Some(args) = trait_bound.type_args.as_ref() {
            for (param_name, arg_expr) in trait_info.type_params.iter().zip(args.iter()) {
                let resolved = self.resolve_type_expr(arg_expr);
                trait_param_map.insert(*param_name, resolved);
            }
        } else if self.lang_trait(crate::LangItem::Index) == Some(declaring)
            && trait_info.type_params.len() == 1
            && matches!(method.name.name.as_str(), "get" | "at")
        {
            // Legacy `impl Index for T` elides `Index<Idx>` and determines
            // `Idx` from the handler's concrete index parameter. Keep that
            // compatibility at the exact lang-item declaration only; applying
            // it to arbitrary generic traits would silently infer omitted
            // arguments from whichever method happened to be checked first.
            if let Some(actual_index_ty) = impl_sig.params.first() {
                trait_param_map.insert(
                    trait_info.type_params[0],
                    self.subst.resolve(actual_index_ty),
                );
            }
        }

        // The implementing type as the impl's target resolved.
        let Some(impl_self) = self.current_impl_target() else {
            return;
        };

        let impl_parameters = self.source_parameter_heads(
            method.type_params.as_deref().unwrap_or_default(),
            &method.fn_span,
        );

        // Materialise the expected impl-side signature.
        let expected_params: Vec<Ty> = trait_sig
            .params
            .iter()
            .map(|p| {
                let projected = self.substitute_trait_sig_for_impl(p, &impl_self, &trait_param_map);
                Self::rename_method_type_params(
                    &projected,
                    &trait_sig.type_params,
                    &impl_parameters,
                )
            })
            .collect();
        let expected_return = {
            let projected = self.substitute_trait_sig_for_impl(
                &trait_sig.return_type,
                &impl_self,
                &trait_param_map,
            );
            Self::rename_method_type_params(&projected, &trait_sig.type_params, &impl_parameters)
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
                    method_name: method.name.to_string(),
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
                    method_name: method.name.to_string(),
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
            // An identity `impl From<T> for T` reads its `value: T` as the
            // receiver; `E_FROM_INVALID` already names that impl.
            let identity_from = self
                .impl_method_declaration_id(type_name, method, Some(trait_bound))
                .is_some_and(|declaration| {
                    self.from_impls
                        .iter()
                        .any(|row| row.method == declaration && row.source == row.target)
                });
            if identity_from {
                return;
            }
            // Arity mismatch — also fires when the impl wrote a different
            // receiver shape (e.g. `(it: X)` vs `(self)`), because the impl's
            // non-Self first param is not detected as a receiver and so is
            // *not* skipped, producing a different post-skip arity.
            self.report_error_with_note(
                TypeErrorKind::TraitImplSignatureMismatch {
                    trait_name: trait_name.clone(),
                    method_name: method.name.to_string(),
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
            .is_some_and(|p| p.is_receiver && p.is_mutable);
        let impl_receiver_mut = method
            .params
            .first()
            .is_some_and(|p| p.is_receiver && p.is_mutable);
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
                    method_name: method.name.to_string(),
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

        // Each side's types carry their declarations' identities, so the two
        // signatures compare directly once aliases and projections normalize.
        let canon_expected_params: Vec<Ty> = expected_params
            .iter()
            .map(|t| self.normalize_for_use(t))
            .collect();
        let canon_actual_params: Vec<Ty> = impl_sig
            .params
            .iter()
            .map(|t| self.normalize_for_use(t))
            .collect();
        let canon_expected_return = self.normalize_for_use(&expected_return);
        let canon_actual_return = self.normalize_for_use(&impl_sig.return_type);

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
                        method_name: method.name.to_string(),
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
                    method_name: method.name.to_string(),
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

    /// The registry spelling of an impl target named `type_name`: a
    /// primitive or builtin key, a monomorphic builtin enum's catalog name, or
    /// the module-owned nominal.
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
                if let Some(canonical) = self.canonical_nominal_name(type_name) {
                    return canonical;
                }
                self.current_module_identity()
                    .filter(|_| !type_name.contains('.'))
                    .map_or_else(
                        || type_name.to_string(),
                        |module| format!("{module}.{type_name}"),
                    )
            })
    }

    /// The slot an impl method fills: a declared trait's method, or the
    /// capability a compiler predicate's method provides.
    pub(in crate::check) fn impl_method_slot(
        &self,
        trait_id: crate::DefId,
        method_name: &str,
    ) -> Option<crate::type_facts::ImplMethodSlot> {
        use crate::type_facts::{ImplMethodSlot, ValueCapability};
        match (crate::DefTable::as_predicate(trait_id), method_name) {
            (Some(crate::Predicate::Hash), "hash") => {
                Some(ImplMethodSlot::Value(ValueCapability::Hash))
            }
            (Some(crate::Predicate::Eq), "eq") => Some(ImplMethodSlot::Value(ValueCapability::Eq)),
            (Some(crate::Predicate::Ord), "lt") => Some(ImplMethodSlot::OrdLt),
            (Some(crate::Predicate::PartialOrd), "lt") => Some(ImplMethodSlot::PartialOrdLt),
            _ => self
                .trait_method_ids_of(trait_id, method_name)
                .map(|(_, method)| ImplMethodSlot::Declared(method)),
        }
    }

    pub(in crate::check) fn trait_impl_method_declaration(
        &self,
        ty: &Ty,
        trait_id: crate::DefId,
        method_name: &str,
    ) -> Option<crate::DefId> {
        let slot = self.impl_method_slot(trait_id, method_name)?;
        self.impl_method_declaration_for_slot(ty, slot)
    }

    pub(in crate::check) fn impl_method_declaration_for_slot(
        &self,
        ty: &Ty,
        slot: crate::type_facts::ImplMethodSlot,
    ) -> Option<crate::DefId> {
        let receiver = ResolvedTy::from_ty(&self.subst.resolve(ty)).ok()?;
        crate::type_facts::selected_impl_method(
            &self.trait_impl_method_declaration_ids,
            &receiver,
            slot,
        )
    }

    /// Record an `impl <Trait> for <PrimitiveOrBuiltinGeneric>` method in the
    /// side table.  `canonical_key` must come from
    /// [`Self::canonical_primitive_or_builtin_key_from_name`] so registration
    /// and dispatch agree.
    pub(in crate::check) fn record_primitive_trait_impl_method(
        &mut self,
        canonical_key: String,
        trait_id: crate::DefId,
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
        self.primitive_trait_impls
            .entry((canonical_key, trait_id))
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
    /// Source admission rejects distinct same-head impls by their block and
    /// trait declaration identities before this side table is populated.
    /// Shape equality here therefore represents re-registration of an admitted
    /// impl or an explicit override of an implicit prelude impl. This extra
    /// different-shape restriction remains builtin-specific; user-record
    /// concrete specialisations do not reach this side table.
    pub(in crate::check) fn record_primitive_trait_impl_self_args(
        &mut self,
        canonical_key: String,
        trait_id: crate::DefId,
        self_args: Vec<Ty>,
        impl_span: &Span,
    ) {
        match self
            .primitive_trait_impl_self_args
            .get(&(canonical_key.clone(), trait_id))
        {
            None => {
                self.primitive_trait_impl_self_args
                    .insert((canonical_key, trait_id), self_args);
            }
            Some(existing) if existing == &self_args => {
                // Source admission permits re-registration and prelude
                // overrides, but rejects distinct same-head source impls.
            }
            Some(_) => {
                // A second, structurally-different impl of the same trait on the
                // same builtin constructor: an overlapping impl, which Hew does
                // not permit. Reject it; keep the first-registered impl.
                let trait_name = self.defs.display(trait_id).to_string();
                let dedup = (
                    canonical_key.clone(),
                    trait_id,
                    impl_span.start,
                    impl_span.end,
                );
                if self.conflicting_trait_impl_reported.insert(dedup) {
                    self.report_error(
                        TypeErrorKind::ConflictingTraitImpl {
                            trait_name: trait_name.clone(),
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
    ) -> Option<(crate::DefId, FnSig)> {
        let canonical = Self::canonical_primitive_or_builtin_key(receiver_ty)?;
        let mut candidates: Vec<(crate::DefId, &FnSig)> = self
            .primitive_trait_impls
            .iter()
            .filter(|((rx_key, _), _)| rx_key == &canonical)
            .filter_map(|((_, trait_id), methods)| methods.get(method).map(|sig| (*trait_id, sig)))
            .collect();
        // A trait declared in the current module shadows an imported one.
        // Multiple genuinely distinct visible traits can still declare the
        // same receiver method. Keep selection stable until the language grows
        // an explicit ambiguity diagnostic for this receiver-form surface.
        candidates.sort_unstable_by_key(|(trait_id, _)| {
            (!self.trait_is_local(*trait_id), self.defs.path(*trait_id))
        });
        candidates
            .into_iter()
            .next()
            .map(|(trait_id, sig)| (trait_id, sig.clone()))
    }
}
