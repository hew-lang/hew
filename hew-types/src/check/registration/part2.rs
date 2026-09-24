//! Split from `registration.rs`: checker methods, part 2 of 6.
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
    pub(super) fn reresolve_record_members_in_scope(&mut self, rd: &RecordDecl) {
        let type_param_names: Vec<String> = rd.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });
        let mut hole_vars = Vec::new();

        match &rd.kind {
            RecordKind::Named(record_fields) => {
                let mut fields: HashMap<String, Ty> = HashMap::new();
                let mut field_order: Vec<String> = Vec::new();
                for rf in record_fields {
                    let field_ty = self.resolve_registered_annotation_ty(&rf.ty, &mut hole_vars);
                    field_order.push(rf.name.clone());
                    fields.insert(rf.name.clone(), field_ty);
                }

                let stored_key = self.authoritative_type_def_key(&rd.name);
                let Some(stored) = self.type_defs.get(&stored_key) else {
                    return;
                };
                if stored.fields == fields {
                    return;
                }

                let type_def = TypeDef {
                    kind: TypeDefKind::Record,
                    name: rd.name.clone(),
                    type_params: type_param_names,
                    bounds: stored.bounds.clone(),
                    fields,
                    field_order,
                    variants: HashMap::new(),
                    methods: stored.methods.clone(),
                    doc_comment: rd.doc_comment.clone(),
                    is_indirect: false,
                };
                let field_types: Vec<Ty> = type_def.fields.values().cloned().collect();
                let field_types = self.expand_for_marker_registration(&field_types);
                self.registry.register_type(stored_key, field_types);
                self.commit_reresolved_type_def(&rd.name, type_def);
            }
            RecordKind::Tuple(positional_types) => {
                let param_tys: Vec<Ty> = positional_types
                    .iter()
                    .map(|te| self.resolve_registered_annotation_ty(te, &mut hole_vars))
                    .collect();
                // Tuple records store no fields (`.0`/`.1` access is forbidden);
                // the positional types live only in the constructor `fn_sig`.
                let canonical = self.authoritative_type_def_key(&rd.name);
                let mut changed = false;
                if let Some(sig) = self.fn_sigs.get_mut(&canonical) {
                    if sig.params != param_tys {
                        sig.params.clone_from(&param_tys);
                        changed = true;
                    }
                }
                if !changed {
                    return;
                }
                let expanded_param_tys = self.expand_for_marker_registration(&param_tys);
                self.registry.register_type(canonical, expanded_param_tys);
                self.handle_bearing_dirty = true;
            }
        }
    }

    /// Re-resolve a `machine` declaration's state and event field types. Mirrors
    /// `register_machine_decl`'s state-variant / event-companion resolution and
    /// marker derivation. State and event companions are patched independently.
    pub(super) fn reresolve_machine_members(&mut self, md: &MachineDecl) {
        let scope =
            self.enter_primary_sig_scope(&[(Some(&md.type_params), md.where_clause.as_ref())]);
        self.reresolve_machine_members_in_scope(md);
        self.exit_primary_sig_scope(scope);
    }

    pub(super) fn reresolve_machine_members_in_scope(&mut self, md: &MachineDecl) {
        // --- State fields → machine `type_def` variants ---
        let mut variants = HashMap::new();
        let mut machine_hole_vars = Vec::new();
        for state in &md.states {
            if state.fields.is_empty() {
                variants.insert(state.name.clone(), VariantDef::Unit);
            } else {
                let variant_fields: Vec<(String, Ty)> = state
                    .fields
                    .iter()
                    .map(|(name, spanned_te)| {
                        (
                            name.clone(),
                            self.resolve_registered_annotation_ty(
                                spanned_te,
                                &mut machine_hole_vars,
                            ),
                        )
                    })
                    .collect();
                variants.insert(state.name.clone(), VariantDef::Struct(variant_fields));
            }
        }

        let machine_key = self.authoritative_type_def_key(&md.name);
        if let Some(stored) = self.type_defs.get(&machine_key) {
            if stored.variants != variants {
                let type_def = TypeDef {
                    kind: TypeDefKind::Machine,
                    name: md.name.clone(),
                    type_params: stored.type_params.clone(),
                    bounds: stored.bounds.clone(),
                    fields: HashMap::new(),
                    field_order: vec![],
                    variants,
                    methods: stored.methods.clone(),
                    doc_comment: stored.doc_comment.clone(),
                    is_indirect: stored.is_indirect,
                };
                // Register field types for Send/Frozen derivation (mirrors the
                // `resolve_type_expr` flatten at the registration site).
                let mut all_field_types = Vec::new();
                for state in &md.states {
                    for (_, spanned_te) in &state.fields {
                        all_field_types.push(self.resolve_type_expr(spanned_te));
                    }
                }
                let all_field_types = self.expand_for_marker_registration(&all_field_types);
                self.registry
                    .register_type(md.name.clone(), all_field_types);
                self.commit_reresolved_type_def(&md.name, type_def);
            }
        }

        // --- Event fields → `{Name}Event` companion enum ---
        let event_type_name = format!("{}Event", md.name);
        let mut event_variants = HashMap::new();
        let mut event_hole_vars = Vec::new();
        for event in &md.events {
            if event.fields.is_empty() {
                event_variants.insert(event.name.clone(), VariantDef::Unit);
            } else {
                let variant_fields: Vec<(String, Ty)> = event
                    .fields
                    .iter()
                    .map(|(name, spanned_te)| {
                        (
                            name.clone(),
                            self.resolve_registered_annotation_ty(spanned_te, &mut event_hole_vars),
                        )
                    })
                    .collect();
                event_variants.insert(event.name.clone(), VariantDef::Struct(variant_fields));
            }
        }
        let event_key = self.authoritative_type_def_key(&event_type_name);
        if let Some(stored) = self.type_defs.get(&event_key) {
            if stored.variants != event_variants {
                let event_type_def = TypeDef {
                    kind: TypeDefKind::Enum,
                    name: event_type_name.clone(),
                    type_params: stored.type_params.clone(),
                    bounds: stored.bounds.clone(),
                    fields: HashMap::new(),
                    field_order: vec![],
                    variants: event_variants,
                    methods: stored.methods.clone(),
                    doc_comment: stored.doc_comment.clone(),
                    is_indirect: stored.is_indirect,
                };
                // Mirror `register_machine_decl`'s marker registration: the
                // re-resolved companion enum's field types must be
                // re-registered too, or a re-resolution that changes a
                // payload's Send-ability (e.g. an import alias resolving to
                // a resource type) is invisible to `TraitRegistry`.
                let event_field_types = Self::structural_member_types_for_type(&event_type_def);
                let event_field_types = self.expand_for_marker_registration(&event_field_types);
                self.registry
                    .register_type(event_type_name.clone(), event_field_types);
                self.registry.register_type_params(
                    event_type_name.clone(),
                    event_type_def.type_params.clone(),
                );
                self.commit_reresolved_type_def(&event_type_name, event_type_def);
            }
        }
    }

    /// Populate `type_defs` with a full `TypeDef` for a non-root module's
    /// `TypeDecl`, including resolved fields and variant constructors.
    ///
    /// Deliberately skips:
    ///   - `type_def_spans` — the import path handles namespace dedup
    ///   - `TraitRegistry` registration — the import path (or C module
    ///     registry) handles trait derivation for exported types
    ///   - Wire-method registration — only relevant for the import surface
    ///
    /// It still registers enum-constructor `fn_sigs` so non-root module body
    /// checking can construct local values. The import path's later
    /// `register_type_decl` call overwrites those signatures for `pub` types
    /// with the fully side-effected version.
    pub(super) fn pre_register_type_decl(&mut self, td: &TypeDecl) {
        let scope =
            self.enter_primary_sig_scope(&[(td.type_params.as_ref(), td.where_clause.as_ref())]);
        self.pre_register_type_decl_in_scope(td);
        self.exit_primary_sig_scope(scope);
    }

    #[expect(clippy::too_many_lines, reason = "type resolution requires many cases")]
    pub(super) fn pre_register_type_decl_in_scope(&mut self, td: &TypeDecl) {
        // Idempotency guard, keyed per-module. Two non-root modules that each
        // declare a type of the same bare name (`badpkg.Reply` and
        // `goodpkg.Reply`) must BOTH register: the bare `type_defs` entry is
        // last-write-wins across modules (the qualified alias is the authority),
        // but each module's qualified marker set must be seeded so the ask-reply
        // Send gate derives `Send` from the correct module's fields. Keying the
        // guard on the bare name skipped the second module's `Reply` entirely,
        // leaving the gate to read whichever module won the bare-key race.
        let guard_key = self
            .current_module_identity()
            .map_or_else(|| td.name.clone(), |m| format!("{m}.{}", td.name));
        if self.type_defs.contains_key(&guard_key) {
            return;
        }
        // #1295: record `#[resource]` types from pre-registered (imported)
        // modules too, so an imported handle type's inherent `close(self)`
        // consumes its receiver at the call site (mirrors `register_type_decl`).
        if td.resource_marker == hew_parser::ast::ResourceMarker::Resource {
            self.registry.register_resource_type(guard_key.clone());
        }
        if td.resource_marker == hew_parser::ast::ResourceMarker::Linear {
            self.registry.register_linear_type(guard_key.clone());
        }
        if td.is_opaque {
            self.user_opaque_type_names.insert(guard_key.clone());
        }
        let kind = match td.kind {
            TypeDeclKind::Struct => TypeDefKind::Struct,
            TypeDeclKind::Enum => TypeDefKind::Enum,
        };
        let type_param_names: Vec<String> = td.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });
        let type_param_bounds =
            self.collect_type_param_bounds(td.type_params.as_ref(), td.where_clause.as_ref());

        // Reject duplicate type parameter names — same check as `register_type_decl`.
        {
            let mut seen: std::collections::HashSet<&str> = std::collections::HashSet::new();
            for name in &type_param_names {
                if !seen.insert(name.as_str()) {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::DuplicateDefinition,
                        0..0,
                        format!(
                            "type parameter `{name}` is defined more than once in `{}`",
                            td.name
                        ),
                    ));
                }
            }
        }

        let mut fields = HashMap::new();
        let mut field_order: Vec<String> = Vec::new();
        let mut variants = HashMap::new();
        let mut hole_vars = Vec::new();
        let enum_return_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::Named {
                builtin: None,
                name: name.clone(),
                args: vec![],
            })
            .collect();

        for item in &td.body {
            match item {
                TypeBodyItem::Field { name, ty, .. } => {
                    let field_ty = self.resolve_registered_annotation_ty(ty, &mut hole_vars);
                    field_order.push(name.clone());
                    fields.insert(name.clone(), field_ty);
                }
                TypeBodyItem::Variant(variant) => {
                    let declaration_name = self
                        .current_module
                        .as_ref()
                        .map_or_else(|| td.name.clone(), |module| format!("{module}.{}", td.name));
                    let return_type =
                        self.variant_nominal_ty(declaration_name, enum_return_args.clone());
                    match &variant.kind {
                        VariantKind::Unit => {
                            variants.insert(variant.name.clone(), VariantDef::Unit);
                            // Register variant constructor so body-checking can construct values
                            self.fn_sigs.insert(
                                variant.name.clone(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    type_param_bounds: type_param_bounds.clone(),
                                    return_type,
                                    is_builtin_variant: self.in_stdlib_registration,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Tuple(tfields) => {
                            let variant_tys: Vec<Ty> = tfields
                                .iter()
                                .map(|field| {
                                    self.resolve_registered_annotation_ty(field, &mut hole_vars)
                                })
                                .collect();
                            variants.insert(
                                variant.name.clone(),
                                VariantDef::Tuple(variant_tys.clone()),
                            );
                            self.fn_sigs.insert(
                                variant.name.clone(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    type_param_bounds: type_param_bounds.clone(),
                                    params: variant_tys,
                                    return_type,
                                    is_builtin_variant: self.in_stdlib_registration,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Struct(sfields) => {
                            let variant_fields: Vec<(String, Ty)> = sfields
                                .iter()
                                .map(|(name, field)| {
                                    (
                                        name.clone(),
                                        self.resolve_registered_annotation_ty(
                                            field,
                                            &mut hole_vars,
                                        ),
                                    )
                                })
                                .collect();
                            variants
                                .insert(variant.name.clone(), VariantDef::Struct(variant_fields));
                        }
                    }
                }
                TypeBodyItem::Method(_) => {}
            }
        }

        let type_def = TypeDef {
            kind,
            name: td.name.clone(),
            type_params: type_param_names,
            bounds: type_param_bounds,
            fields,
            field_order,
            variants,
            methods: HashMap::new(),
            doc_comment: td.doc_comment.clone(),
            is_indirect: td.is_indirect,
        };

        // Seed the trait-registry structural member set for imported module
        // types, mirroring `register_type_decl`. An imported actor whose `ask`
        // replies with one of these types (e.g. a `pub type Result { ... }`)
        // is gated on the reply being `Send` at the dispatch site
        // (`record_actor_method_dispatch`, `E_DUPLEX_NON_SEND`). Send and the
        // sibling structural markers derive from a named type's member set; if
        // the importer's registry has no `type_fields` entry the derivation
        // hits the "unknown type — conservatively fail" branch and rejects a
        // plainly-Send imported record. Seeding it here resolves the marker
        // through the imported record's actual fields. Enums register their
        // variant-payload member set (an empty `fields` map would derive a
        // spurious Copy/Frozen); the `Serializable` subset follows the same
        // wire/enum condition as the full registration path.
        let field_types: Vec<_> = if kind == TypeDefKind::Enum {
            Self::structural_member_types_for_type(&type_def)
        } else {
            type_def.fields.values().cloned().collect()
        };
        let field_types = self.expand_for_marker_registration(&field_types);
        self.registry.register_type(td.name.clone(), field_types);
        self.registry
            .register_type_params(td.name.clone(), type_def.type_params.clone());
        // Mirror the markers under the module-qualified key so a same-bare-name
        // reply from another package cannot clobber this type's Send derivation
        // at the ask-reply gate.
        self.seed_qualified_type_markers_for_current_module(&td.name);

        // Keep the bare row as registration-local assembly state. A non-root
        // declaration is published through the canonical constructor so every
        // durable named-family insertion uses the same full-owner key path.
        if let Some(module_owner) = self.current_module_identity().map(str::to_string) {
            self.register_canonical_type_def(&module_owner, &td.name, &type_def);
        }
        self.type_defs.insert(td.name.clone(), type_def);
        self.record_type_def_inference_holes(&td.name, hole_vars);
        self.handle_bearing_dirty = true;
    }

    /// Reserve a type-name in the given module's namespace and reject a second
    /// declaration of the same name *within the same module*.
    ///
    /// `module_owner` is the defining module (`None` for the root program and
    /// flat file imports, which share one namespace). Two distinct modules may
    /// each declare a type
    /// of the same bare name — the durable cross-module identity is the qualified
    /// `{module}.{name}` key inserted by `register_canonical_type_def`. The bare
    /// `type_def_spans` entry is still populated for the span-lookup consumers
    /// (cycle / actor-ref diagnostics); it is last-write-wins across modules and
    /// is no longer the uniqueness authority.
    pub(in crate::check) fn register_type_namespace_name(
        &mut self,
        module_owner: Option<&str>,
        name: &str,
        span: &Span,
    ) -> bool {
        if self.reject_protected_prelude_declaration_for_owner(module_owner, name, span) {
            return false;
        }
        if crate::ty::is_reserved_type_name(name) {
            self.errors
                .push(TypeError::reserved_type_name(span.clone(), name));
            return false;
        }
        let owner_key = (module_owner.map(str::to_string), name.to_string());
        if let Some(prev_span) = self.type_namespace_owners.get(&owner_key).cloned() {
            self.report_duplicate_type_namespace_name(name, span, prev_span);
            return false;
        }

        self.type_namespace_owners.insert(owner_key, span.clone());
        self.type_def_spans
            .entry(name.to_string())
            .or_insert_with(|| span.clone());
        true
    }

    pub(in crate::check) fn report_duplicate_type_namespace_name(
        &mut self,
        name: &str,
        span: &Span,
        prev_span: Span,
    ) {
        self.errors.push(TypeError::duplicate_definition(
            span.clone(),
            name,
            prev_span,
        ));
    }

    pub(in crate::check) fn register_machine_type_namespace_names(
        &mut self,
        module_owner: Option<&str>,
        machine_name: &str,
        span: &Span,
    ) -> bool {
        if self.reject_protected_prelude_declaration_for_owner(module_owner, machine_name, span)
            || self.reject_protected_prelude_declaration_for_owner(
                module_owner,
                &format!("{machine_name}Event"),
                span,
            )
        {
            return false;
        }
        if crate::ty::is_reserved_type_name(machine_name) {
            self.errors
                .push(TypeError::reserved_type_name(span.clone(), machine_name));
            return false;
        }
        let machine_key = (module_owner.map(str::to_string), machine_name.to_string());
        if let Some(prev_span) = self.type_namespace_owners.get(&machine_key).cloned() {
            self.report_duplicate_type_namespace_name(machine_name, span, prev_span);
            return false;
        }

        let event_type_name = format!("{machine_name}Event");
        let event_key = (module_owner.map(str::to_string), event_type_name.clone());
        if let Some(prev_span) = self.type_namespace_owners.get(&event_key).cloned() {
            self.report_duplicate_type_namespace_name(&event_type_name, span, prev_span);
            return false;
        }

        self.type_namespace_owners.insert(machine_key, span.clone());
        self.type_namespace_owners.insert(event_key, span.clone());
        self.type_def_spans
            .entry(machine_name.to_string())
            .or_insert_with(|| span.clone());
        self.type_def_spans
            .entry(event_type_name)
            .or_insert_with(|| span.clone());
        true
    }

    pub(in crate::check) fn register_type_decl(&mut self, td: &TypeDecl) {
        let scope =
            self.enter_primary_sig_scope(&[(td.type_params.as_ref(), td.where_clause.as_ref())]);
        self.register_type_decl_in_scope(td);
        self.exit_primary_sig_scope(scope);
    }

    #[expect(clippy::too_many_lines, reason = "type resolution requires many cases")]
    pub(super) fn register_type_decl_in_scope(&mut self, td: &TypeDecl) {
        if td.origin == hew_parser::ast::DeclarationOrigin::MachineReport {
            let qualified = self
                .current_declaration_module()
                .map(|module| format!("{}.{}", self.identity.module_path(module), td.name));
            if let Some(declaration) = qualified
                .as_deref()
                .and_then(|name| self.lookup_declaration(name))
                .or_else(|| self.lookup_declaration(&td.name))
            {
                self.must_use_types.insert(declaration.clone());
            }
        }
        // #1295: record `#[resource]` types so their inherent `close(self)`
        // dispatch can mark the receiver moved + consume it (suppressing the
        // duplicate scope-exit implicit drop). HIR owns the close-discipline
        // diagnostics (W3.030); the checker only needs the marker fact here.
        if td.resource_marker == hew_parser::ast::ResourceMarker::Resource {
            let canonical_name = self
                .current_module_identity()
                .map_or_else(|| td.name.clone(), |module| format!("{module}.{}", td.name));
            self.registry.register_resource_type(canonical_name);
        }
        if td.resource_marker == hew_parser::ast::ResourceMarker::Linear {
            let canonical_name = self
                .current_module_identity()
                .map_or_else(|| td.name.clone(), |module| format!("{module}.{}", td.name));
            self.registry.register_linear_type(canonical_name);
        }
        // Track user-declared `#[opaque]` types so `record_clone_admissibility`
        // can detect opaque fields transitively. The module_registry only
        // carries opaque types imported via `use module::*`; user-declared
        // opaques in the same file are NOT registered there.
        if td.is_opaque {
            let canonical_name = self
                .current_module_identity()
                .map_or_else(|| td.name.clone(), |module| format!("{module}.{}", td.name));
            // Imported declarations keep their exact owner. Publishing their
            // bare spelling would mark an unrelated root type with the same
            // name opaque when declaration facts are collected.
            self.user_opaque_type_names.insert(canonical_name);
        }

        let kind = match td.kind {
            TypeDeclKind::Struct => TypeDefKind::Struct,
            TypeDeclKind::Enum => TypeDefKind::Enum,
        };

        let mut fields = HashMap::new();
        let mut field_order: Vec<String> = Vec::new();
        let mut variants = HashMap::new();
        let mut variant_order = Vec::new();
        let mut hole_vars = Vec::new();
        let type_param_names: Vec<String> = td.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });

        // Reject duplicate type parameter names within the same declaration.
        // The parser cannot catch this because `parse_type_params` has no
        // seen-name accumulator; the checker is the authoritative gatekeeper.
        {
            let mut seen: std::collections::HashSet<&str> = std::collections::HashSet::new();
            for name in &type_param_names {
                if !seen.insert(name.as_str()) {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::DuplicateDefinition,
                        0..0,
                        format!(
                            "type parameter `{name}` is defined more than once in `{}`",
                            td.name
                        ),
                    ));
                }
            }
        }

        let type_param_bounds =
            self.collect_type_param_bounds(td.type_params.as_ref(), td.where_clause.as_ref());
        let enum_return_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::Named {
                builtin: None,
                name: name.clone(),
                args: vec![],
            })
            .collect();

        for item in &td.body {
            match item {
                TypeBodyItem::Field { name, ty, .. } => {
                    let field_ty = self.resolve_registered_annotation_ty(ty, &mut hole_vars);
                    field_order.push(name.clone());
                    fields.insert(name.clone(), field_ty);
                }
                TypeBodyItem::Variant(variant) => {
                    variant_order.push(variant.name.clone());
                    let declaration_name = self
                        .current_module
                        .as_ref()
                        .map_or_else(|| td.name.clone(), |module| format!("{module}.{}", td.name));
                    let return_type =
                        self.variant_nominal_ty(declaration_name, enum_return_args.clone());
                    match &variant.kind {
                        VariantKind::Unit => {
                            variants.insert(variant.name.clone(), VariantDef::Unit);
                            self.fn_sigs.insert(
                                variant.name.clone(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    type_param_bounds: type_param_bounds.clone(),
                                    return_type,
                                    is_builtin_variant: self.in_stdlib_registration,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Tuple(fields) => {
                            let variant_tys: Vec<Ty> = fields
                                .iter()
                                .map(|field| {
                                    self.resolve_registered_annotation_ty(field, &mut hole_vars)
                                })
                                .collect();
                            variants.insert(
                                variant.name.clone(),
                                VariantDef::Tuple(variant_tys.clone()),
                            );

                            // Register variant constructor as function
                            self.fn_sigs.insert(
                                variant.name.clone(),
                                FnSig {
                                    type_params: type_param_names.clone(),
                                    type_param_bounds: type_param_bounds.clone(),
                                    params: variant_tys,
                                    return_type,
                                    is_builtin_variant: self.in_stdlib_registration,
                                    ..FnSig::default()
                                },
                            );
                        }
                        VariantKind::Struct(fields) => {
                            let variant_fields: Vec<(String, Ty)> = fields
                                .iter()
                                .map(|(name, field)| {
                                    (
                                        name.clone(),
                                        self.resolve_registered_annotation_ty(
                                            field,
                                            &mut hole_vars,
                                        ),
                                    )
                                })
                                .collect();
                            variants
                                .insert(variant.name.clone(), VariantDef::Struct(variant_fields));
                        }
                    }
                }
                TypeBodyItem::Method(_) => {
                    // Methods are handled in pass 2
                }
            }
        }

        let type_def = TypeDef {
            kind,
            name: td.name.clone(),
            type_params: type_param_names.clone(),
            bounds: type_param_bounds,
            fields,
            field_order,
            variants,
            methods: HashMap::new(),
            doc_comment: td.doc_comment.clone(),
            is_indirect: td.is_indirect,
        };

        // Register with trait registry for Send/Frozen/Copy/... derivation.
        //
        // Structural markers (`Copy`, `Send`, `Clone`, …) derive from a Named
        // type's reachable member types. For a struct/record those are its
        // fields; for an ENUM they are the variant PAYLOAD types — an enum with
        // a `string`-payload variant is NOT Copy even though it has no named
        // fields. The marker registry stores member types by name, and its
        // derivation walks them with `all(...)` (vacuously true on an empty
        // list), so an enum registered with only its (empty) `fields` would be
        // spuriously Copy/Frozen. Register the variant-inclusive member set for
        // enums so the marker derivation is correct (W5.016: the spurious-Copy
        // bug routed owned-payload enum Vecs down the BitCopy path → runtime
        // stride panic).
        let field_types: Vec<_> = if kind == TypeDefKind::Enum {
            Self::structural_member_types_for_type(&type_def)
        } else {
            type_def.fields.values().cloned().collect()
        };
        let field_types = self.expand_for_marker_registration(&field_types);

        self.registry.register_type(td.name.clone(), field_types);
        self.registry
            .register_type_params(td.name.clone(), type_param_names.clone());
        // Mirror the markers under the module-qualified key (when this type is
        // declared in a non-root module) so a same-bare-name reply from another
        // package cannot clobber this type's Send derivation at the ask-reply
        // gate. `register_qualified_type_alias` repeats this for the pub import
        // surface; this covers the registration call itself.
        self.seed_qualified_type_markers_for_current_module(&td.name);

        self.type_defs.insert(td.name.clone(), type_def);
        self.record_type_def_inference_holes(&td.name, hole_vars);
        self.handle_bearing_dirty = true;

        // If this is a wire type, register encode/decode/to_json/from_json/to_yaml/from_yaml methods
        if let Some(ref wire) = td.wire {
            self.register_wire_methods(&td.name, wire, &variant_order);
            self.validate_wire_version_constraints(&td.name, wire);
        }
    }

    /// Register a `record` declaration into the type table.
    ///
    /// Named-field form: populates `type_defs.fields` so that
    /// `check_struct_init` and `check_field_access` resolve field types by
    /// name.
    ///
    /// Tuple-positional form: registers a constructor `fn_sig` so that
    /// `R(1, 2)` resolves as a function call returning `Ty::Named { name: R
    /// }`.  The `fields` map is left empty — this deliberately prevents
    /// `.0`/`.1` index-style access (A-D2: positional destructuring only).
    ///
    /// In both cases `type_defs` receives a `TypeDef` with
    /// `kind = TypeDefKind::Record` so the field-write rejection in
    /// `statements.rs` can identify record types.
    pub(in crate::check) fn register_record_decl(&mut self, rd: &RecordDecl) {
        let scope =
            self.enter_primary_sig_scope(&[(rd.type_params.as_ref(), rd.where_clause.as_ref())]);
        self.register_record_decl_in_scope(rd);
        self.exit_primary_sig_scope(scope);
    }

    pub(super) fn register_record_decl_in_scope(&mut self, rd: &RecordDecl) {
        let type_param_names: Vec<String> = rd.type_params.as_ref().map_or(vec![], |params| {
            params.iter().map(|p| p.name.clone()).collect()
        });
        let type_param_bounds =
            self.collect_type_param_bounds(rd.type_params.as_ref(), rd.where_clause.as_ref());

        // Build the return type for constructors: `R` or `R<T1, T2, …>`
        let enum_return_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::Named {
                builtin: None,
                name: name.clone(),
                args: vec![],
            })
            .collect();
        let declaration_name = self
            .current_module_identity()
            .map_or_else(|| rd.name.clone(), |module| format!("{module}.{}", rd.name));
        let return_type = Ty::Named {
            builtin: None,
            name: declaration_name.clone(),
            args: enum_return_args,
        };

        let mut fields: HashMap<String, Ty> = HashMap::new();
        let mut field_order: Vec<String> = Vec::new();
        let mut hole_vars = Vec::new();
        // Positional field types for tuple records, collected for marker
        // derivation (A-4). Named-record fields come from `type_def.fields`.
        let mut tuple_field_types: Vec<Ty> = Vec::new();

        match &rd.kind {
            RecordKind::Named(record_fields) => {
                for rf in record_fields {
                    let field_ty = self.resolve_registered_annotation_ty(&rf.ty, &mut hole_vars);
                    field_order.push(rf.name.clone());
                    fields.insert(rf.name.clone(), field_ty);
                }
            }
            RecordKind::Tuple(positional_types) => {
                // Resolve each positional field type for the constructor signature.
                let param_tys: Vec<Ty> = positional_types
                    .iter()
                    .map(|te| self.resolve_registered_annotation_ty(te, &mut hole_vars))
                    .collect();

                // Capture positional types for marker registration before moving
                // param_tys into fn_sigs. The `fields` map intentionally stays
                // empty — `.0`/`.1` access is not permitted on tuple records (A-D2).
                tuple_field_types.clone_from(&param_tys);

                // Register a constructor function so `R(1, 2)` resolves via
                // `check_call`.  The `fields` map intentionally stays empty —
                // `.0`/`.1` access is not permitted on tuple records (A-D2).
                let signature = FnSig {
                    type_params: type_param_names.clone(),
                    type_param_bounds: type_param_bounds.clone(),
                    params: param_tys,
                    return_type: return_type.clone(),
                    ..FnSig::default()
                };
                self.fn_sigs.insert(declaration_name.clone(), signature);
            }
        }

        let type_def = TypeDef {
            kind: TypeDefKind::Record,
            name: rd.name.clone(),
            type_params: type_param_names.clone(),
            bounds: type_param_bounds,
            fields,
            field_order,
            variants: HashMap::new(),
            methods: HashMap::new(),
            doc_comment: rd.doc_comment.clone(),
            is_indirect: false,
        };

        // Register all field types for marker derivation (Eq/Hash/Send/Frozen/
        // Clone/Copy). Named-field records use type_def.fields; tuple records
        // use the positional types captured above (type_def.fields is empty for
        // tuple records by design — A-D2).
        let field_types: Vec<Ty> = if tuple_field_types.is_empty() {
            type_def.fields.values().cloned().collect()
        } else {
            tuple_field_types
        };
        let field_types = self.expand_for_marker_registration(&field_types);
        self.registry
            .register_type(declaration_name.clone(), field_types);
        self.registry
            .register_type_params(declaration_name.clone(), type_param_names.clone());
        // Mark this as a record type so implements_marker applies the correct
        // value-type semantics (Resource always false; all other markers field-driven).
        self.registry.register_record_type(declaration_name.clone());

        self.type_defs.insert(declaration_name.clone(), type_def);
        self.record_type_def_inference_holes(&declaration_name, hole_vars);
        self.handle_bearing_dirty = true;
    }

    /// Register codec methods for a wire type.
    ///
    /// - Wire structs expose binary + JSON/YAML helpers.
    /// - Wire enums expose JSON/YAML helpers.
    pub(in crate::check) fn register_wire_methods(
        &mut self,
        type_name: &str,
        wire: &WireMetadata,
        variant_order: &[String],
    ) {
        // ONE canonical wire identity (A316). A module declaration's wire
        // surface is keyed by `{module}.{Name}` — the identity every resolved
        // receiver and the codegen wire-layout lookup carry; a root
        // declaration's bare name IS its canonical identity. Surface
        // spellings resolve TO this key at lookup time
        // (`canonical_nominal_name`); no bare mirror entries exist, so two
        // same-leaf wire types from different modules never collide on a
        // shared last-write-wins key.
        let canonical_identity = self.current_module_identity().map_or_else(
            || type_name.to_string(),
            |module| format!("{module}.{type_name}"),
        );
        let self_ty = Ty::Named {
            builtin: None,
            name: canonical_identity.clone(),
            args: vec![],
        };
        let bytes_ty = Ty::Bytes;

        let Some((is_wire_struct, is_serial_wire_enum, layout_entry)) =
            self.type_defs.get(type_name).map(|type_def| {
                let is_wire_struct = type_def.kind == TypeDefKind::Struct;
                let is_unit_wire_enum = type_def.kind == TypeDefKind::Enum
                    && type_def
                        .variants
                        .values()
                        .all(|variant| matches!(variant, VariantDef::Unit));
                let is_payload_wire_enum = type_def.kind == TypeDefKind::Enum
                    && type_def
                        .variants
                        .values()
                        .any(|variant| !matches!(variant, VariantDef::Unit));
                let is_serial_wire_enum = is_unit_wire_enum || is_payload_wire_enum;
                let layout_entry = Self::wire_layout_entry_from_metadata(
                    type_def,
                    wire,
                    is_wire_struct,
                    variant_order,
                );
                (is_wire_struct, is_serial_wire_enum, layout_entry)
            })
        else {
            return;
        };
        // Track wire structs and wire enums so the method-dispatch arms can
        // recognise the binary `encode`/`decode` codec calls (which lower to the
        // `__hew_cbor_serialize_*` / `__hew_cbor_deserialize_*` thunks) without
        // re-deriving wire-ness. Both ride the CBOR body codec: structs as a
        // tag-keyed map, enums as the "map-of-one" shape.
        if is_wire_struct {
            self.wire_struct_types.insert(canonical_identity.clone());
        }
        if is_serial_wire_enum {
            self.wire_enum_types.insert(canonical_identity.clone());
        }
        self.wire_layouts
            .insert(canonical_identity.clone(), layout_entry);

        // Wire structs and wire enums carry the same method surface: the binary
        // CBOR codec (`encode`/`decode`) plus the text-format helpers. The body
        // shapes differ at codegen (struct = tag-keyed map, enum =
        // "map-of-one"), but the registered signatures are identical.
        let instance_methods = if is_wire_struct || is_serial_wire_enum {
            vec![
                ("encode", vec![], bytes_ty.clone()),
                ("to_json", vec![], Ty::String),
                ("to_yaml", vec![], Ty::String),
            ]
        } else {
            vec![]
        };

        // Instance methods land on the DECLARATION record (the bare
        // `type_defs` entry this module's registration just wrote), then the
        // canonical definition is refreshed from it — the
        // `commit_reresolved_type_def` pattern. Pre-registration mints the
        // qualified skeleton before this runs; the later canonical refresh is
        // what carries the codec methods onto the durable definition.
        if let Some(type_def) = self.type_defs.get_mut(type_name) {
            for (method_name, params, return_type) in &instance_methods {
                type_def.methods.insert(
                    (*method_name).to_string(),
                    FnSig {
                        params: params.clone(),
                        return_type: return_type.clone(),
                        ..FnSig::default()
                    },
                );
            }
        }
        if let Some(module_owner) = self.current_module_identity().map(str::to_string) {
            self.register_qualified_type_alias(&module_owner, type_name);
        }

        // `decode` returns bare `Self` (binary CBOR is trap-on-failure); the
        // text-format `from_json`/`from_yaml` parsers can fail on arbitrary
        // user input (config files, HTTP bodies), so they return
        // `Result<Self, string>` — the only honest shape for a fallible parse.
        let from_result_ty = Ty::result(self_ty.clone(), Ty::String);
        let static_methods = if is_wire_struct || is_serial_wire_enum {
            vec![
                ("decode", vec![bytes_ty], self_ty),
                ("from_json", vec![Ty::String], from_result_ty.clone()),
                ("from_yaml", vec![Ty::String], from_result_ty),
            ]
        } else {
            vec![]
        };

        for (method_name, params, return_type) in static_methods {
            // Static codec entry points register under the canonical identity
            // ONLY. The call-site arm canonicalizes the receiver's surface
            // spelling (`Env.from_json` inside the defining module, an
            // importer's binding, an `as`-alias) to this key before lookup.
            self.fn_sigs.insert(
                format!("{canonical_identity}.{method_name}"),
                FnSig {
                    params,
                    return_type,
                    ..FnSig::default()
                },
            );
        }
    }

    pub(super) fn wire_layout_entry_from_metadata(
        type_def: &TypeDef,
        wire: &WireMetadata,
        is_wire_struct: bool,
        variant_order: &[String],
    ) -> WireLayoutEntry {
        let fields = if is_wire_struct {
            wire.field_meta
                .iter()
                .map(|field| WireFieldLayout {
                    name: field.field_name.clone(),
                    tag: field.field_number,
                    json_name: field.json_name.clone(),
                    yaml_name: field.yaml_name.clone(),
                    presence: if field.is_optional {
                        WireFieldPresence::Optional
                    } else {
                        WireFieldPresence::Required
                    },
                    repeated: field.is_repeated,
                })
                .collect()
        } else {
            Vec::new()
        };

        let variant_tags: HashMap<&str, u32> = wire
            .field_meta
            .iter()
            .map(|field| (field.field_name.as_str(), field.field_number))
            .collect();
        let variant_names: Vec<String> = if variant_order.is_empty() {
            let mut names: Vec<_> = type_def.variants.keys().cloned().collect();
            names.sort();
            names
        } else {
            variant_order
                .iter()
                .filter(|name| type_def.variants.contains_key(*name))
                .cloned()
                .collect()
        };
        let variants = if is_wire_struct {
            Vec::new()
        } else {
            variant_names
                .into_iter()
                .enumerate()
                .map(|(index, name)| {
                    #[expect(
                        clippy::cast_possible_truncation,
                        reason = "wire enum variant counts are bounded by source size"
                    )]
                    let default_tag = index as u32;
                    let tag = variant_tags
                        .get(name.as_str())
                        .copied()
                        .unwrap_or(default_tag);
                    (name, tag)
                })
                .collect()
        };

        WireLayoutEntry {
            is_struct: is_wire_struct,
            json_case: wire.json_case,
            yaml_case: wire.yaml_case,
            version: wire.version,
            min_version: wire.min_version,
            fields,
            variants,
        }
    }

    /// Validate version constraints on a wire type.
    pub(in crate::check) fn validate_wire_version_constraints(
        &mut self,
        type_name: &str,
        wire: &hew_parser::ast::WireMetadata,
    ) {
        use crate::error::Severity;

        let decl_span = self.type_def_spans.get(type_name).cloned().unwrap_or(0..0);
        let version = wire.version;
        let min_version = wire.min_version;

        // min_version cannot exceed version
        if let (Some(min_v), Some(v)) = (min_version, version) {
            if min_v > v {
                self.errors.push(TypeError {
                    severity: Severity::Error,
                    kind: TypeErrorKind::InvalidOperation,
                    span: decl_span.clone(),
                    message: format!(
                        "wire `{type_name}`: min_version ({min_v}) cannot exceed version ({v})"
                    ),
                    notes: vec![],
                    suggestions: vec![],
                    source_module: self.current_module.clone(),
                });
            }
        }

        // Per-field `since` constraints
        for fm in &wire.field_meta {
            if let Some(since) = fm.since {
                if version.is_none() {
                    // since has no effect without a schema version
                    self.warnings.push(TypeError {
                        severity: Severity::Warning,
                        kind: TypeErrorKind::StyleSuggestion,
                        span: decl_span.clone(),
                        message: format!(
                            "wire `{type_name}.{}`: field has `since {since}` but type \
                             has no #[wire(version = N)] attribute",
                            fm.field_name
                        ),
                        notes: vec![],
                        suggestions: vec![],
                        source_module: self.current_module.clone(),
                    });
                }

                // since cannot exceed version
                if let Some(v) = version {
                    if since > v {
                        self.errors.push(TypeError {
                            severity: Severity::Error,
                            kind: TypeErrorKind::InvalidOperation,
                            span: decl_span.clone(),
                            message: format!(
                                "wire `{type_name}.{}`: since ({since}) cannot exceed \
                                 schema version ({v})",
                                fm.field_name
                            ),
                            notes: vec![],
                            suggestions: vec![],
                            source_module: self.current_module.clone(),
                        });
                    }
                }
            }

            // Warn if version > 1 and a non-optional field lacks `since`
            if let Some(v) = version {
                if v > 1 && fm.since.is_none() && !fm.is_optional {
                    self.warnings.push(TypeError {
                        severity: Severity::Warning,
                        kind: TypeErrorKind::StyleSuggestion,
                        span: decl_span.clone(),
                        message: format!(
                            "wire `{type_name}.{}`: non-optional field has no `since` annotation \
                             (schema version is {v})",
                            fm.field_name
                        ),
                        notes: vec![],
                        suggestions: vec![],
                        source_module: self.current_module.clone(),
                    });
                }
            }
        }
    }

    /// Register a machine declaration as a type definition with variants and methods.
    #[expect(
        clippy::too_many_lines,
        reason = "machine registration covers states, events, and generated methods"
    )]
    pub(in crate::check) fn register_machine_decl(&mut self, md: &MachineDecl, span: &Span) {
        // Build the machine's self-type: `Machine` or `Machine<T, U, …>`.
        // MachineDecl.type_params is Vec<TypeParam> — we extract bare names
        // here for the self-type and collect declared trait bounds into a
        // side table consulted at use sites (struct-state brace init) and
        // mirrored onto unit-state constructor FnSigs for the call path.
        //
        // Validate before collect_type_param_bounds erases positional type args.
        self.validate_type_param_bound_shapes(
            Some(&md.type_params),
            md.where_clause.as_ref(),
            span,
        );
        let type_param_names: Vec<String> = md.type_params.iter().map(|p| p.name.clone()).collect();
        // Collect inline `<T: Trait>` and `where T: Trait` bounds into a
        // single side table keyed by machine name then param name. At
        // the checker layer, a bound's source (inline vs where clause)
        // does not affect the enforcement question — the bound is
        // "satisfied at the instantiation site iff the substituted
        // type implements the trait" regardless of where the bound
        // was authored — so duplicates on the same (param, trait) pair
        // dedupe. Source provenance is preserved at the parser layer
        // (separate `type_params` / `where_clause` fields on
        // `MachineDecl`) so future lowering layers that want to point
        // diagnostics at the predicate's span can recover it.
        let type_param_bounds =
            self.collect_type_param_bounds(Some(&md.type_params), md.where_clause.as_ref());
        if !type_param_bounds.is_empty() {
            self.machine_type_param_bounds
                .insert(md.name.clone(), type_param_bounds.clone());
        }
        // W3.039 Stage 2: register const-generic parameter declarations
        // into the side table so instantiation-site validation
        // (Stage 3 — gated on W3.033c) can recover arity, types, and
        // defaults without re-walking the parser AST. We also enforce
        // here that const-param names do not shadow type-param names.
        if !md.const_params.is_empty() {
            let type_param_names: std::collections::HashSet<&str> =
                md.type_params.iter().map(|p| p.name.as_str()).collect();
            let mut const_param_decls: Vec<super::types::MachineConstParamDecl> =
                Vec::with_capacity(md.const_params.len());
            let mut seen_const_names: std::collections::HashSet<&str> =
                std::collections::HashSet::new();
            for cp in &md.const_params {
                if type_param_names.contains(cp.name.as_str()) {
                    self.errors.push(crate::error::TypeError::new(
                        crate::error::TypeErrorKind::DuplicateDefinition,
                        span.clone(),
                        format!(
                            "const parameter `{}` on machine `{}` shadows a type parameter \
                             of the same name",
                            cp.name, md.name
                        ),
                    ));
                    continue;
                }
                if !seen_const_names.insert(cp.name.as_str()) {
                    self.errors.push(crate::error::TypeError::new(
                        crate::error::TypeErrorKind::DuplicateDefinition,
                        span.clone(),
                        format!(
                            "duplicate const parameter `{}` on machine `{}`",
                            cp.name, md.name
                        ),
                    ));
                    continue;
                }
                let ty = match cp.ty {
                    hew_parser::ast::ConstParamTy::Usize => {
                        super::types::MachineConstParamTy::Usize
                    }
                };
                const_param_decls.push(super::types::MachineConstParamDecl {
                    name: cp.name.clone(),
                    ty,
                    default: cp.default,
                });
            }
            if !const_param_decls.is_empty() {
                self.machine_const_params
                    .insert(md.name.clone(), const_param_decls);
            }
        }
        let machine_generic_args: Vec<Ty> = type_param_names
            .iter()
            .map(|name| Ty::Named {
                builtin: None,
                name: name.clone(),
                args: vec![],
            })
            .collect();
        let machine_identity = self.declaration_identity(&md.name);
        let machine_ty = Ty::Named {
            builtin: None,
            name: machine_identity.clone(),
            args: machine_generic_args.clone(),
        };

        let event_type_name = format!("{}Event", md.name);
        let event_identity = self.declaration_identity(&event_type_name);
        let event_ty = Ty::Named {
            builtin: None,
            name: event_identity.clone(),
            args: machine_generic_args.clone(),
        };

        // Build state variants
        let mut variants = HashMap::new();
        let mut machine_hole_vars = Vec::new();
        for state in &md.states {
            if state.fields.is_empty() {
                variants.insert(state.name.clone(), VariantDef::Unit);
                // Register unit state constructor as a function. For generic
                // machines (e.g. `machine Worker<T>`), the constructor returns
                // `Worker<T>` so callers can instantiate with concrete args.
                self.fn_sigs.insert(
                    state.name.clone(),
                    FnSig {
                        type_params: type_param_names.clone(),
                        type_param_bounds: type_param_bounds.clone(),
                        return_type: machine_ty.clone(),
                        ..FnSig::default()
                    },
                );
            } else {
                let variant_fields: Vec<(String, Ty)> = state
                    .fields
                    .iter()
                    .map(|(name, spanned_te)| {
                        (
                            name.clone(),
                            self.resolve_registered_annotation_ty(
                                spanned_te,
                                &mut machine_hole_vars,
                            ),
                        )
                    })
                    .collect();
                variants.insert(state.name.clone(), VariantDef::Struct(variant_fields));
            }
        }

        let type_def = TypeDef {
            kind: TypeDefKind::Machine,
            name: md.name.clone(),
            type_params: type_param_names.clone(),
            bounds: type_param_bounds.clone(),
            fields: HashMap::new(),
            field_order: vec![],
            variants,
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };

        // Register field types for Send/Frozen derivation
        let mut all_field_types = Vec::new();
        for state in &md.states {
            for (_, spanned_te) in &state.fields {
                all_field_types.push(self.resolve_type_expr(spanned_te));
            }
        }
        let all_field_types = self.expand_for_marker_registration(&all_field_types);
        self.registry
            .register_type(md.name.clone(), all_field_types);
        self.registry
            .register_type_params(md.name.clone(), type_param_names.clone());

        self.commit_reresolved_type_def(&md.name, type_def);
        self.record_type_def_inference_holes(&machine_identity, machine_hole_vars);
        self.known_types.insert(md.name.clone());
        self.known_types.insert(machine_identity.clone());

        // Register the generated event companion enum
        let mut event_variants = HashMap::new();
        let mut event_hole_vars = Vec::new();
        for event in &md.events {
            if event.fields.is_empty() {
                event_variants.insert(event.name.clone(), VariantDef::Unit);
            } else {
                let variant_fields: Vec<(String, Ty)> = event
                    .fields
                    .iter()
                    .map(|(name, spanned_te)| {
                        (
                            name.clone(),
                            self.resolve_registered_annotation_ty(spanned_te, &mut event_hole_vars),
                        )
                    })
                    .collect();
                event_variants.insert(event.name.clone(), VariantDef::Struct(variant_fields));
            }
        }
        let event_type_def = TypeDef {
            kind: TypeDefKind::Enum,
            name: event_type_name.clone(),
            type_params: type_param_names.clone(),
            bounds: type_param_bounds.clone(),
            fields: HashMap::new(),
            field_order: vec![],
            variants: event_variants,
            methods: HashMap::new(),
            doc_comment: None,
            is_indirect: false,
        };
        // Register the event companion's variant-payload member types for
        // Send/Frozen/… derivation — the same call an ordinary `enum`
        // declaration gets in `register_type_decl`. Without this the
        // companion enum has no `type_fields` entry, so `TraitRegistry`
        // treats it as an unknown type and conservatively derives every
        // marker false — including `Send` — even when every event payload
        // is itself Send (#3122: the spec's actor example, which sends the
        // companion enum to a `receive fn`, could not compile).
        let event_field_types = Self::structural_member_types_for_type(&event_type_def);
        let event_field_types = self.expand_for_marker_registration(&event_field_types);
        self.registry
            .register_type(event_type_name.clone(), event_field_types);
        self.registry
            .register_type_params(event_type_name.clone(), type_param_names.clone());
        self.commit_reresolved_type_def(&event_type_name, event_type_def);
        self.record_type_def_inference_holes(&event_identity, event_hole_vars);
        self.known_types.insert(event_type_name.clone());
        self.known_types.insert(event_identity);

        // Register the step() method on the machine type
        if let Some(td) = self.type_defs.get_mut(&machine_identity) {
            td.methods.insert(
                "step".to_string(),
                FnSig {
                    param_names: vec!["event".to_string()],
                    params: vec![event_ty.clone()],
                    ..FnSig::default()
                },
            );
            // Register state_name() method
            td.methods.insert(
                "state_name".to_string(),
                FnSig {
                    return_type: Ty::String,
                    ..FnSig::default()
                },
            );
            // Register take_emits(event) method: removes every queued emit
            // matching (this machine's type id, the argument's event tag)
            // from the thread-local emit queue and returns the count
            // removed. Sibling of `step`/`state_name` — same event
            // companion enum param.
            td.methods.insert(
                "take_emits".to_string(),
                FnSig {
                    param_names: vec!["event".to_string()],
                    params: vec![event_ty],
                    return_type: Ty::I64,
                    ..FnSig::default()
                },
            );
        }
        if machine_identity != md.name {
            if let Some(type_def) = self.type_defs.get(&machine_identity).cloned() {
                self.type_defs.insert(md.name.clone(), type_def);
            }
        }
    }

    /// Validate that no trait bound in the given type parameters or
    /// where-clause carries positional type arguments (e.g. `T: Eq<U>`).
    /// Such forms are not valid in Hew — the checker cannot enforce
    /// phantom-parameterised marker bounds, and admitting them would silently
    /// erase the type arguments in `collect_type_param_bounds`, reducing
    /// `Eq<U>` to bare `Eq` without any diagnostic.
    ///
    /// Emits `UnknownTraitBoundShape` at `span` for every offending bound.
    /// Must be called before `collect_type_param_bounds` erases `type_args`.
    /// Covers fn/impl/impl-method/machine declaration positions.
    pub(in crate::check) fn validate_type_param_bound_shapes(
        &mut self,
        type_params: Option<&Vec<TypeParam>>,
        where_clause: Option<&WhereClause>,
        span: &Span,
    ) {
        // Check inline type-param bounds: e.g. `<T: Eq<U>>`.
        if let Some(params) = type_params {
            for param in params {
                for bound in &param.bounds {
                    if bound.type_args.as_ref().is_some_and(|a| !a.is_empty()) {
                        self.report_error(
                            TypeErrorKind::UnknownTraitBoundShape {
                                trait_name: bound.name.clone(),
                            },
                            span,
                            format!(
                                "trait bound `{}` on type parameter `{}` carries positional \
                                 type arguments, which are not supported; use associated-type \
                                 bindings (`Trait<Assoc = Ty>`) instead",
                                bound.name, param.name,
                            ),
                        );
                    }
                }
            }
        }
        // Check where-clause bounds: `where T: Eq<U>`.
        if let Some(wc) = where_clause {
            for predicate in &wc.predicates {
                for bound in &predicate.bounds {
                    if bound.type_args.as_ref().is_some_and(|a| !a.is_empty()) {
                        self.report_error(
                            TypeErrorKind::UnknownTraitBoundShape {
                                trait_name: bound.name.clone(),
                            },
                            span,
                            format!(
                                "trait bound `{}` in where-clause carries positional \
                                 type arguments, which are not supported; use associated-type \
                                 bindings (`Trait<Assoc = Ty>`) instead",
                                bound.name,
                            ),
                        );
                    }
                }
            }
        }
    }

    /// Thin wrapper for the fn-decl path; delegates to
    /// `validate_type_param_bound_shapes` using the function's own
    /// type-param list, where-clause, and declaration span.
    pub(in crate::check) fn validate_fn_type_param_bound_shapes(&mut self, fd: &FnDecl) {
        self.validate_type_param_bound_shapes(
            fd.type_params.as_ref(),
            fd.where_clause.as_ref(),
            &fd.decl_span,
        );
    }

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

    pub(in crate::check) fn build_impl_alias_entries(
        &mut self,
        id: &ImplDecl,
    ) -> HashMap<String, ImplAliasEntry> {
        let mut entries = HashMap::new();
        let mut seen_spans: HashMap<String, Span> = HashMap::new();
        for alias in &id.type_aliases {
            if let Some(prev_span) = seen_spans.insert(alias.name.clone(), alias.ty.1.clone()) {
                self.errors.push(TypeError::duplicate_definition(
                    alias.ty.1.clone(),
                    &alias.name,
                    prev_span,
                ));
                continue;
            }
            entries.insert(
                alias.name.clone(),
                ImplAliasEntry {
                    expr: alias.ty.clone(),
                    resolved: None,
                    resolving: false,
                },
            );
        }
        if let Some(tb) = &id.trait_bound {
            let trait_key = self.trait_defs_key_for_bound(&tb.name);
            if let Some(trait_info) = self.trait_defs.get(&trait_key) {
                for assoc in &trait_info.associated_types {
                    if entries.contains_key(&assoc.name) {
                        continue;
                    }
                    if let Some(default) = &assoc.default {
                        entries.insert(
                            assoc.name.clone(),
                            ImplAliasEntry {
                                expr: default.clone(),
                                resolved: None,
                                resolving: false,
                            },
                        );
                    }
                }
            }
        }
        entries
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

    /// Pass 2: Collect function signatures
    #[expect(
        clippy::too_many_lines,
        reason = "signature collection maintains one ordered registration walk"
    )]
    pub(in crate::check) fn collect_functions(&mut self, program: &Program) {
        let flat_file_import_modules = flat_file_import_module_ids(program);
        self.flat_file_import_module_names = flat_file_import_modules
            .iter()
            .map(|module_id| module_id.path.join("."))
            .collect();
        // Process module graph items first (if multi-module).
        // Skip the root module — its items are already in program.items and
        // will be processed below with current_module = None (bare names).
        if let Some(ref mg) = program.module_graph {
            let span_indices = mg.file_span_indices();
            for mod_id in &mg.topo_order {
                if *mod_id == mg.root {
                    continue;
                }
                if let Some(module) = mg.modules.get(mod_id) {
                    let module_name = mod_id.path.join(".");
                    self.record_canonical_std_module_source(&module_name, &module.source_paths);
                    self.current_module = Some(module_name.clone());
                    self.registration_is_flat_file_import =
                        flat_file_import_modules.contains(mod_id);
                    self.current_module_direct_imports = module
                        .imports
                        .iter()
                        .map(|import| import.target.path.join("."))
                        .collect();
                    self.current_module_direct_import_bindings = module
                        .imports
                        .iter()
                        .map(|import| (import.target.path.join("."), import.spec.clone()))
                        .collect();
                    // Scope local declarations to the module being registered.
                    let saved_local_type_defs = self.local_type_defs.clone();
                    let saved_source_type_defs = self.source_type_defs.clone();
                    for (item, _) in &module.items {
                        match item {
                            Item::TypeDecl(td) => {
                                self.local_type_defs.insert(td.name.clone());
                                self.source_type_defs.insert(td.name.clone());
                            }
                            Item::Machine(md) => {
                                self.local_type_defs.insert(md.name.clone());
                                self.source_type_defs.insert(md.name.clone());
                                let event_type_name = format!("{}Event", md.name);
                                self.local_type_defs.insert(event_type_name.clone());
                                self.source_type_defs.insert(event_type_name);
                            }
                            _ => {}
                        }
                    }

                    // Snapshot error/warning counts before signature registration
                    // for this module.  Diagnostics emitted during collect_function_item
                    // (e.g. duplicate-definition errors, import errors) are tagged with
                    // the module name below so the CLI renders them against the correct
                    // source file rather than the root compilation unit.
                    let err_before = self.errors.len();
                    let warn_before = self.warnings.len();

                    let item_sources = self.module_item_sources.get(&module_name).cloned();
                    for (item_idx, (item, span)) in module.items.iter().enumerate() {
                        // Per-item defining-file identity (rc1-F1 stage C):
                        // registration-time facts (extern contracts, their
                        // conflict diagnostics) attribute to the item's own
                        // source file, not the assembled module's primary.
                        self.current_item_source = item_sources
                            .as_ref()
                            .and_then(|sources| sources.get(item_idx))
                            .cloned();
                        self.current_item_ordinal = item_idx;
                        self.current_module_idx = span_indices
                            .item_index(mod_id, item_idx)
                            .unwrap_or_default();
                        self.collect_function_item(item, span);
                    }
                    self.current_item_source = None;
                    self.current_item_ordinal = 0;

                    for e in &mut self.errors[err_before..] {
                        if e.source_module.is_none() {
                            e.source_module = Some(module_name.clone());
                        }
                    }
                    for w in &mut self.warnings[warn_before..] {
                        if w.source_module.is_none() {
                            w.source_module = Some(module_name.clone());
                        }
                    }

                    self.local_type_defs = saved_local_type_defs;
                    self.source_type_defs = saved_source_type_defs;
                }
            }
        }

        // Process main module items.
        self.current_module = None;
        self.current_module_idx = 0;
        self.registration_is_flat_file_import = false;
        self.current_module_direct_imports = program
            .module_graph
            .as_ref()
            .and_then(|graph| graph.modules.get(&graph.root))
            .map(|root| {
                root.imports
                    .iter()
                    .map(|import| import.target.path.join("."))
                    .collect()
            })
            .unwrap_or_default();
        self.current_module_direct_import_bindings = program
            .module_graph
            .as_ref()
            .and_then(|graph| graph.modules.get(&graph.root))
            .map(|root| {
                root.imports
                    .iter()
                    .map(|import| (import.target.path.join("."), import.spec.clone()))
                    .collect()
            })
            .unwrap_or_default();
        for (item_ordinal, (item, span)) in program.items.iter().enumerate() {
            self.current_item_ordinal = item_ordinal;
            self.collect_function_item(item, span);
        }
        self.canonicalize_root_super_trait_edges(program);
        self.current_module_direct_imports.clear();
        self.current_module_direct_import_bindings.clear();
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
}
