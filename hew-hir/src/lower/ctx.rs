//! `LowerCtx` construction, checker-fact queries and diagnostic helpers.

use super::*;

impl LowerCtx {
    #[allow(
        clippy::too_many_lines,
        reason = "initializes the complete checker-to-HIR typed context atomically"
    )]
    pub(super) fn new(
        tc_output: &TypeCheckOutput,
        mono_cap: usize,
        target_arch: TargetArch,
    ) -> Self {
        let mut type_classes = crate::value_class::TypeClassTable::default();
        // Seed compiler-known M2 substrate types before source-order TypeDecls.
        // This registers Sink/Stream resource lifecycle metadata as
        // AffineResource even though they are not user-declared TypeDecl items.
        seed_builtin_type_classes(&mut type_classes);
        Self {
            ids: IdGen::default(),
            scopes: Vec::new(),
            fn_registry: HashMap::new(),
            fn_symbol_overrides: HashMap::new(),
            extern_fn_names: HashSet::new(),
            imported_actor_rewrites: None,
            imported_module_consts: None,
            type_classes,
            opaque_resource_candidates: tc_output.opaque_resource_candidates.clone(),
            resource_close_discipline_failures: HashSet::new(),
            diagnostics: Vec::new(),
            // Resolution spellings remain a checker lookup index. Declaration
            // identity comes only from `tc_output.defs`; HIR must never
            // manufacture a second canonical-string namespace here.
            fn_sigs_by_path: tc_output.fn_sigs_by_path(),
            direct_call_targets: tc_output.direct_call_targets.clone(),
            trait_method_ids: tc_output.trait_method_ids.clone(),
            trait_method_ids_by_binding: tc_output.trait_method_ids_by_binding.clone(),
            impl_method_declaration_ids: tc_output.impl_method_declaration_ids.clone(),
            consuming_inherent_methods: tc_output.consuming_inherent_methods.clone(),
            impl_method_body_symbols: HashMap::new(),
            impl_body_plan: ImplBodyPlan::default(),
            method_call_rewrites: tc_output.method_call_rewrites.clone(),
            method_call_discharges_receiver: tc_output.method_call_discharges_receiver.clone(),
            method_call_preserves_receiver_identity: tc_output
                .method_call_preserves_receiver_identity
                .clone(),
            width_cast_lowerings: tc_output.width_cast_lowerings.clone(),
            try_width_cast_lowerings: tc_output.try_width_cast_lowerings.clone(),
            actor_method_dispatch: tc_output.actor_method_dispatch.clone(),
            actor_delivery_calls: tc_output.actor_delivery_calls.clone(),
            machine_method_dispatch: tc_output.machine_method_dispatch.clone(),
            tail_ok_coercions: tc_output.tail_ok_coercions.clone(),
            result_return_coercions: tc_output.result_return_coercions.clone(),
            recovery_kinds: tc_output.recovery_kinds.clone(),
            call_argument_slots: tc_output.call_argument_slots.clone(),
            checked_call_effects: tc_output.suspension_effects.calls.clone(),
            select_sources: tc_output.select_sources.clone(),
            checked_fork_transfers: tc_output.suspension_effects.fork_transfers.clone(),
            fork_call_inputs: None,
            method_call_receiver_kinds: tc_output.method_call_receiver_kinds.clone(),
            dyn_trait_coercions: tc_output.dyn_trait_coercions.clone(),
            dyn_trait_method_calls: tc_output.dyn_trait_method_calls.clone(),
            resolved_calls: tc_output.resolved_calls.clone(),
            expr_types: tc_output.expr_types.clone(),
            type_facts: tc_output.type_facts.clone(),
            type_declarations: tc_output.type_fact_context.declarations().clone(),
            interpolation_display_types: tc_output.interpolation_display_types.clone(),
            user_comparison_dispatch: tc_output.user_comparison_dispatch.clone(),
            numeric_operand_coercions: tc_output.numeric_operand_coercions.clone(),
            extern_method_signatures: tc_output.extern_method_signatures.clone(),
            resolved_expr_types: tc_output.resolved_expr_types.clone(),
            is_type_patterns: tc_output.is_type_patterns.clone(),
            closure_capture_facts: tc_output.closure_capture_facts.clone(),
            closure_escape_facts: tc_output.closure_escape_facts.clone(),
            generator_yield_tys: Vec::new(),
            scope_depth: 0,
            current_scope_id: ScopeId(0),
            current_return_type: None,
            current_actor_self: None,
            actor_self_state_fields: tc_output.actor_self_state_fields.clone(),
            actor_deferred_field_decls: tc_output.actor_deferred_field_decls.clone(),
            actor_init_first_stores: tc_output.actor_init_first_stores.clone(),
            borrowed_element_for_loops: tc_output.borrowed_element_for_loops.clone(),
            borrowed_element_index_reads: tc_output.borrowed_element_index_reads.clone(),
            owning_take_vec_cursors: tc_output.owning_take_vec_cursors.clone(),
            borrowed_element_option_reads: tc_output.borrowed_element_option_reads.clone(),
            call_type_args: tc_output.call_type_args.clone(),
            lowering_facts: tc_output.lowering_facts.clone(),
            assign_target_kinds: tc_output.assign_target_kinds.clone(),
            assign_target_shapes: tc_output.assign_target_shapes.clone(),
            checked_indexed_place_operations: tc_output.indexed_place_operations.clone(),
            indexed_place_operations: HashMap::new(),
            actor_handler_state_guards: tc_output.actor_handler_state_guards.clone(),
            cycle_capable_actors: tc_output.cycle_capable_actors.clone(),
            actor_protocol_descriptors: tc_output.actor_protocol_descriptors.clone(),
            lambda_actor_declarations: tc_output.lambda_actor_declarations.clone(),
            pending_lambda_actors: Vec::new(),
            // A supervisor is addressed by its own type exactly as an actor is,
            // so both declaration kinds name an actor handle.
            actor_type_names: tc_output
                .type_defs
                .iter()
                .filter(|(_, td)| {
                    matches!(
                        td.kind,
                        hew_types::check::TypeDefKind::Actor
                            | hew_types::check::TypeDefKind::Supervisor
                    )
                })
                .map(|(id, _)| tc_output.defs.path(id.declaration()).to_string())
                .collect(),
            mono_registry: MonoRegistry::with_cap(mono_cap),
            mono_cap_diag_emitted: false,
            call_site_type_args: HashMap::new(),
            record_registry: HashMap::new(),
            extern_backed_record_names: HashSet::new(),
            colliding_imported_record_names: HashSet::new(),
            file_import_module_names: HashSet::new(),
            cross_module_colliding_record_names: HashSet::new(),
            record_init_type_args: tc_output.record_init_type_args.clone(),
            record_layout_registry: RecordLayoutRegistry::with_cap(mono_cap),
            record_layout_cap_diag_emitted: false,
            enum_type_params: HashMap::new(),
            enum_item_ids: HashMap::new(),
            enum_layout_registry: EnumLayoutRegistry::with_cap(mono_cap),
            intrinsic_declarations: tc_output.intrinsic_declarations.clone(),
            supervisor_child_slots_checker: tc_output.supervisor_child_slots.clone(),
            supervisor_child_slots: HashMap::new(),
            pool_accessor_sites_checker: tc_output.pool_accessor_sites.clone(),
            pool_accessor_sites: HashMap::new(),
            regex_literals: Vec::new(),
            regex_literal_index: HashMap::new(),
            machine_ctor_registry: HashMap::new(),
            const_registry: HashMap::new(),
            folded_integer_consts: HashMap::new(),
            enum_variants_by_name: HashMap::new(),
            indirect_enum_names: HashSet::new(),
            type_member_tys: HashMap::new(),
            pattern_resolutions: tc_output.pattern_resolutions.clone(),
            pattern_plans: tc_output.pattern_plans.clone(),
            lang_items: tc_output.lang_items.clone(),
            target_arch,
            current_impl_self_ty: None,
            current_fn_type_params: HashSet::new(),
            trait_bindings: tc_output.trait_bindings.clone(),
            trait_defaults: tc_output.trait_defaults.clone(),
            root_visible_source_type_short_names: HashSet::new(),
            file_import_root_type_aliases: HashMap::new(),
            source_type_identities: HashSet::new(),
            // The checker registers both lifecycle owners' declarations at
            // bootstrap, with or without an import, so their canonical source
            // identities are known before any module graph is walked. Without
            // them a prelude-only spelling such as `CrashInfo` resolves to a
            // canonical name with no representation authority, and its
            // declaration facts are then looked up under a name nothing
            // declares.
            canonical_std_source_type_identities: hew_types::SOURCE_OWNED_LIFECYCLE_OWNERS
                .iter()
                .flat_map(|owner| {
                    owner.declares.iter().map(|builtin| {
                        format!("{}.{}", owner.canonical_path, builtin.canonical_name())
                    })
                })
                .collect(),
            checked_type_defs: tc_output.type_defs_by_path(),
            current_module_idx: 0,
            current_item_ordinal: 0,
            root_item_ids: HashSet::new(),
            lowering_injected_items: false,
            current_module_name: None,
            declaration_module_by_file_index: HashMap::new(),
            type_aliases: tc_output.resolved_type_aliases.clone(),
            import_type_name_aliases: tc_output.import_type_name_aliases.clone(),
            module_import_bindings: tc_output.module_import_bindings.clone(),
            published_bare_const_owners: tc_output.published_bare_const_owners.clone(),
            import_fn_name_aliases: tc_output.import_fn_name_aliases.clone(),
            root_value_bindings: tc_output.root_value_bindings.clone(),
            defs: std::sync::Arc::clone(&tc_output.defs),
        }
    }

    pub(super) fn source_declaration(
        &mut self,
        item_span: &Span,
        kind: hew_types::DeclarationKind,
        ordinal: usize,
    ) -> Option<hew_types::DefId> {
        let module = self
            .declaration_module_by_file_index
            .get(&self.current_module_idx)
            .copied()
            .or_else(|| {
                (self.current_module_idx == 0)
                    .then(|| self.defs.root_module())
                    .flatten()
            });
        let occurrence = hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
            module,
            item_span,
            self.current_item_ordinal,
            kind,
            ordinal,
        );
        if let Some(declaration) = self.defs.declaration(occurrence) {
            return Some(declaration);
        }
        self.diagnostics.push(
            HirDiagnostic::new(
                HirDiagnosticKind::CheckerBoundaryViolation {
                    name: format!("{kind:?} declaration"),
                    reason: "exact source occurrence is absent from the checker identity view"
                        .to_string(),
                },
                item_span.clone(),
                "checker declaration identity did not survive the HIR boundary",
            )
            .with_source_module(self.current_module_name.clone()),
        );
        None
    }

    pub(super) fn with_typecheck_facts<T>(
        &mut self,
        tc_output: &TypeCheckOutput,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let saved_direct_calls = std::mem::replace(
            &mut self.direct_call_targets,
            tc_output.direct_call_targets.clone(),
        );
        let saved_numeric_coercions = std::mem::replace(
            &mut self.numeric_operand_coercions,
            tc_output.numeric_operand_coercions.clone(),
        );
        // Extern-method signatures accumulate: every module's dispatched
        // declarations must reach the emitted extern items, not just the pass
        // that happens to run last.
        self.extern_method_signatures
            .extend(tc_output.extern_method_signatures.clone());
        let saved = (
            std::mem::replace(
                &mut self.method_call_rewrites,
                tc_output.method_call_rewrites.clone(),
            ),
            std::mem::replace(
                &mut self.width_cast_lowerings,
                tc_output.width_cast_lowerings.clone(),
            ),
            std::mem::replace(
                &mut self.try_width_cast_lowerings,
                tc_output.try_width_cast_lowerings.clone(),
            ),
            std::mem::take(&mut self.actor_method_dispatch),
            std::mem::take(&mut self.actor_delivery_calls),
            std::mem::take(&mut self.machine_method_dispatch),
            std::mem::take(&mut self.method_call_receiver_kinds),
            std::mem::take(&mut self.dyn_trait_coercions),
            std::mem::take(&mut self.dyn_trait_method_calls),
            std::mem::replace(&mut self.resolved_calls, tc_output.resolved_calls.clone()),
            std::mem::replace(&mut self.expr_types, tc_output.expr_types.clone()),
            std::mem::replace(
                &mut self.resolved_expr_types,
                tc_output.resolved_expr_types.clone(),
            ),
            std::mem::replace(&mut self.recovery_kinds, tc_output.recovery_kinds.clone()),
            std::mem::replace(
                &mut self.call_argument_slots,
                tc_output.call_argument_slots.clone(),
            ),
            std::mem::replace(&mut self.select_sources, tc_output.select_sources.clone()),
            std::mem::replace(
                &mut self.checked_fork_transfers,
                tc_output.suspension_effects.fork_transfers.clone(),
            ),
            std::mem::take(&mut self.fork_call_inputs),
            std::mem::replace(
                &mut self.checked_call_effects,
                tc_output.suspension_effects.calls.clone(),
            ),
            std::mem::replace(
                &mut self.record_init_type_args,
                tc_output.record_init_type_args.clone(),
            ),
        );

        let result = f(self);

        (
            self.method_call_rewrites,
            self.width_cast_lowerings,
            self.try_width_cast_lowerings,
            self.actor_method_dispatch,
            self.actor_delivery_calls,
            self.machine_method_dispatch,
            self.method_call_receiver_kinds,
            self.dyn_trait_coercions,
            self.dyn_trait_method_calls,
            self.resolved_calls,
            self.expr_types,
            self.resolved_expr_types,
            self.recovery_kinds,
            self.call_argument_slots,
            self.select_sources,
            self.checked_fork_transfers,
            self.fork_call_inputs,
            self.checked_call_effects,
            self.record_init_type_args,
        ) = saved;
        self.direct_call_targets = saved_direct_calls;
        self.numeric_operand_coercions = saved_numeric_coercions;

        result
    }

    pub(super) fn with_current_return_type<T>(
        &mut self,
        return_ty: ResolvedTy,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let previous = self.current_return_type.replace(return_ty);
        let result = f(self);
        self.current_return_type = previous;
        result
    }

    /// Read the checker-resolved type for a source expression. Synthesized
    /// expressions and abstract generic bodies may have no recorded type.
    pub(super) fn checked_ty(&self, span: &Span) -> Option<&ResolvedTy> {
        self.resolved_expr_types.get(&self.mk_key(span))
    }

    /// Read the checker's move-only ownership facts for a user resource argument.
    /// Rc-bearing records can have resource storage while remaining copyable.
    /// Builtin handles have separate borrowing contracts at foreign boundaries.
    pub(super) fn checked_span_user_resource_type(&self, span: &Span) -> Option<String> {
        let ty = self.checked_ty(span)?;
        let ResolvedTy::Named {
            head:
                head @ (hew_types::TypeHead::Nominal(_)
                | hew_types::TypeHead::Param(_)
                | hew_types::TypeHead::Unresolved(_)),
            ..
        } = ty
        else {
            return None;
        };
        let name = head.registry_key();
        self.type_facts
            .get(&hew_types::TypeInstanceKey(ty.clone()))
            .filter(|facts| {
                facts.class == hew_types::ValueClass::AffineResource
                    && facts.clone == hew_types::CloneKind::None
            })
            .map(|_| name.to_string())
    }

    pub(super) fn checked_span_is_user_resource(&self, span: &Span) -> bool {
        self.checked_span_user_resource_type(span).is_some()
    }

    /// Project the checker's terminal-receiver disposition without
    /// reclassifying the selected method in HIR.
    pub(super) fn method_receiver_intent(
        &self,
        key: &SpanKey,
        consumes_receiver: bool,
        preserves_receiver: bool,
    ) -> IntentKind {
        if self.method_call_discharges_receiver.contains(key) {
            IntentKind::Discharge
        } else if consumes_receiver && !preserves_receiver {
            IntentKind::Consume
        } else {
            IntentKind::Read
        }
    }

    /// Intent for an ordinary (non-receiver) call argument at `span`: `Consume`
    /// when the argument is a by-value user-`#[resource]` move (see
    /// `checked_span_is_user_resource`), otherwise the borrowing `Read`
    /// default that every non-owning argument keeps.
    pub(super) fn arg_move_intent(&self, span: &Span) -> IntentKind {
        if self.checked_span_is_user_resource(span) {
            IntentKind::Consume
        } else {
            IntentKind::Read
        }
    }

    /// Intent for an argument crossing a direct, source-declared C-ABI call.
    ///
    /// A plain resource parameter would normally be an ownership move. The
    /// only exception is an exact generated ownership row proving that THIS
    /// declared extern borrows THIS parameter. Missing, short, consume and
    /// retain rows deliberately fall back to the ordinary consuming intent.
    pub(super) fn call_arg_move_intent(
        &self,
        symbol: Option<&str>,
        index: usize,
        span: &Span,
    ) -> IntentKind {
        let borrows_resource = symbol.is_some_and(|symbol| {
            self.extern_fn_names.contains(symbol)
                && self
                    .checked_span_user_resource_type(span)
                    .is_some_and(|resource_type| {
                        hew_types::ffi_contracts::extern_resource_param_is_audited_borrow(
                            symbol,
                            index,
                            self.current_module_name.as_deref(),
                            &resource_type,
                        )
                    })
        });
        if borrows_resource && self.checked_span_is_user_resource(span) {
            IntentKind::Read
        } else {
            self.arg_move_intent(span)
        }
    }

    /// Construct a `SpanKey` for `span` in the current module context.
    ///
    /// The checker records types with `SpanKey::in_module(span, current_module_idx)`
    /// so that two stdlib files with expressions at the same byte offset do not
    /// collide in the flat `expr_types` map (L23 defect root cause). Every HIR
    /// lookup into `expr_types` / `resolved_expr_types` / `method_call_rewrites`
    /// / etc. must use this instead of bare `self.mk_key(span)`.
    #[inline]
    pub(super) fn mk_key(&self, span: &Span) -> SpanKey {
        SpanKey::in_module(span, self.current_module_idx)
    }

    /// Fail-closed guard for the refutable enum-struct consumers (`if let` /
    /// `while let` / `let ... else`). These consume the AST-derived
    /// `ArmResolution` for their binding/skipped descriptors, but the checker's
    /// `PatternPlan` is the authoritative field-list source for every
    /// record-shaped pattern — including an enum struct-variant like
    /// `Packet::Data { a, .. }` (the checker builds a plan for every
    /// `Struct`/`RecordShorthand` pattern). Its absence for a plan-covered shape
    /// is a checker-boundary violation and must fail closed uniform with the
    /// record-let path, never silently fall back to the AST resolution. Returns
    /// `true` (and pushes the boundary-violation diagnostic) when the pattern is
    /// record-shaped and the plan is missing; the caller then bails on its own
    /// fail-closed path.
    pub(super) fn record_shape_missing_plan(&mut self, pattern: &Spanned<Pattern>) -> bool {
        let record_shaped = matches!(
            &pattern.0,
            Pattern::RecordShorthand { .. }
                | Pattern::NominalPath {
                    payload: Some(hew_parser::ast::NominalPatternPayload::Record { .. }),
                    ..
                }
                | Pattern::ContextVariant(hew_parser::ast::ContextVariantPattern {
                    payload: Some(hew_parser::ast::NominalPatternPayload::Record { .. }),
                    ..
                })
        );
        if !record_shaped {
            return false;
        }
        let key = self.mk_key(&pattern.1);
        if self.pattern_plans.contains_key(&key) {
            return false;
        }
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CheckerBoundaryViolation {
                name: "record-shaped pattern".into(),
                reason: "missing checker PatternPlan".into(),
            },
            pattern.1.clone(),
            "checker did not provide a canonical plan for this record-shaped enum/record pattern",
        ));
        true
    }

    /// W4.047 P1.2 — the totality assert net (zero behaviour change).
    ///
    /// At a fail-open / boundary-violation lowering site, prove that the typed
    /// `resolved_expr_types` map agrees *exactly* with the live
    /// `expr_types`→`ResolvedTy::from_ty` path the production code still drives
    /// off. The two must be observationally identical at every concrete
    /// accepted span:
    ///
    /// - span present + `from_ty` succeeds → typed map has the identical value,
    /// - span present + `from_ty` fails (covered generic var) → typed map omits
    ///   it and the live `.ok()` is `None`, so both agree on absence,
    /// - span absent → both miss.
    ///
    /// A trip means the typed handoff is missing an entry for an accepted
    /// concrete expr (a real fail-open gap the `.unwrap_or(Unit)` was masking)
    /// or that P1.1's population diverged from the live conversion. Compiled
    /// out of release: the live path is unchanged, so lowering output is
    /// byte-identical.
    #[inline]
    pub(super) fn assert_resolved_ty_totality(&self, span: &Span) {
        #[cfg(debug_assertions)]
        {
            let key = self.mk_key(span);
            let live = self
                .expr_types
                .get(&key)
                .and_then(|ty| ResolvedTy::from_ty(ty).ok());
            debug_assert_eq!(
                self.checked_ty(span),
                live.as_ref(),
                "W4.047 totality: typed resolved_expr_types disagrees with the \
                 live expr_types->from_ty path at span {key:?} — the typed \
                 checker->HIR handoff is not total over this accepted span"
            );
        }
        #[cfg(not(debug_assertions))]
        let _ = span;
    }

    /// Tag diagnostics raised while lowering a spliced file-import item with
    /// the file it was written in. The item's spans are that file's byte
    /// offsets, so an untagged diagnostic renders against the root source and
    /// points at whatever text happens to share the offset. A root item leaves
    /// `current_module_name` unset and keeps the root's own attribution.
    pub(super) fn tag_spliced_diagnostics(&mut self, start: usize) {
        if let Some(source_module) = self.current_module_name.clone() {
            self.tag_diagnostics_since(start, &source_module);
        }
    }

    pub(super) fn tag_diagnostics_since(&mut self, start: usize, source_module: &str) {
        debug_assert!(start <= self.diagnostics.len());
        for diagnostic in self.diagnostics.iter_mut().skip(start) {
            if diagnostic.source_module.is_none() {
                diagnostic.source_module = Some(source_module.to_string());
            }
        }
    }

    /// Allocate (or look up an existing) regex literal in the module table.
    ///
    /// Deduplicates by raw pattern string equality. Returns the stable
    /// `literal_id` (0-based index into `HirModule::regex_literals`) for
    /// use in `HirMatchArmPredicate::Regex` and `HirExprKind::RegexLiteralRef`.
    pub(super) fn alloc_regex_literal(&mut self, pattern: &str, captures: &[(String, u32)]) -> u32 {
        if let Some(&id) = self.regex_literal_index.get(pattern) {
            return id;
        }
        let id = u32::try_from(self.regex_literals.len())
            .expect("regex literal count exceeds u32::MAX — impossible in practice");
        self.regex_literal_index.insert(pattern.to_string(), id);
        self.regex_literals.push(HirRegexLiteral {
            literal_id: id,
            pattern: pattern.to_string(),
            captures: captures.to_vec(),
        });
        id
    }

    /// Try to record a generic-fn callsite in the monomorphisation
    /// registry. Returns silently for non-generic callees, callees not
    /// in `fn_registry` (builtins/runtime symbols/lambda bindings), and
    /// callsites with no `call_type_args` entry — the latter being the
    /// trivially-monomorphic case from the checker's perspective (an
    /// explicit `<T>` instantiation that already resolved to concrete
    /// types and was not recorded per `calls.rs:183` `record_call_type_args`).
    ///
    /// Emits `MonomorphisationCallTypeArgsViolation` when a recorded
    /// entry fails the `ResolvedTy::from_ty` boundary conversion, and
    /// `MonomorphisationCapExceeded` (at most once per invocation) when
    /// the registry cap is hit.
    /// Recursive check: does this `ResolvedTy` contain a `Named` whose
    /// name matches any type parameter declared on any top-level fn in
    /// this module? If so, the value is "still abstract" — the call
    /// site we're looking at is inside a generic body and the type-arg
    /// has not yet been substituted. Such entries must not enter the
    /// monomorphisation registry; G-1.b's body substitution pass will
    /// re-walk these callsites with substituted args and produce real
    /// entries.
    ///
    /// The check is a conservative over-approximation: a user-declared
    /// type with the same name as a type param (e.g. `pub type T { ... }`)
    /// would also be skipped. This is fine — that's not idiomatic Hew,
    /// and the checker uses `Named` for both cases; the only correct
    /// resolution requires bound-symbol metadata the checker side-table
    /// does not currently expose. Skipping is safe at G-1.a (no false
    /// emissions); a real `T`-named user type still produces the entry
    /// at the outer non-generic callsite where `type_args` is `[]`.
    pub(super) fn contains_abstract_type_param(&self, ty: &ResolvedTy) -> bool {
        match ty {
            ResolvedTy::Named { head, args, .. } => {
                let name = head.registry_key();
                if self.is_type_param_symbol(name) {
                    return true;
                }
                args.iter().any(|a| self.contains_abstract_type_param(a))
            }
            ResolvedTy::Tuple(items) => items.iter().any(|t| self.contains_abstract_type_param(t)),
            ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => {
                self.contains_abstract_type_param(elem)
            }
            ResolvedTy::Function { params, ret, .. } => {
                params.iter().any(|p| self.contains_abstract_type_param(p))
                    || self.contains_abstract_type_param(ret)
            }
            ResolvedTy::Closure {
                params,
                ret,
                captures,
                ..
            } => {
                params.iter().any(|p| self.contains_abstract_type_param(p))
                    || self.contains_abstract_type_param(ret)
                    || captures
                        .iter()
                        .any(|c| self.contains_abstract_type_param(c))
            }
            ResolvedTy::Pointer { pointee, .. } | ResolvedTy::Borrow { pointee } => {
                self.contains_abstract_type_param(pointee)
            }
            ResolvedTy::TraitObject { traits } => traits.iter().any(|tb| {
                tb.args.iter().any(|a| self.contains_abstract_type_param(a))
                    || tb
                        .assoc_bindings
                        .iter()
                        .any(|(_, t)| self.contains_abstract_type_param(t))
            }),
            ResolvedTy::Task(inner) => self.contains_abstract_type_param(inner),
            // A structural type parameter is abstract by construction.
            ResolvedTy::TypeParam { .. } => true,
            _ => false,
        }
    }

    /// Does `name` match any type-parameter declared on any top-level
    /// fn in `fn_registry`? Used to filter "still-abstract" type args
    /// from the monomorphisation registry. See
    /// `contains_abstract_type_param` for the rationale.
    pub(super) fn is_type_param_symbol(&self, name: &str) -> bool {
        self.fn_registry
            .values()
            .any(|entry| entry.type_params.iter().any(|p| p == name))
    }

    /// The symbol HIR emitted a published declaration under.
    ///
    /// A package module's declaration keeps its `{owner}.{name}` identity. A
    /// file import's declaration was spliced into the root namespace by the
    /// frontend, so the same declaration is emitted under its bare name.
    pub(super) fn published_declaration_symbol(&self, source_identity: &str) -> String {
        match source_identity.rsplit_once('.') {
            Some((owner, name)) if self.file_import_module_names.contains(owner) => {
                name.to_string()
            }
            _ => crate::mangle_dotted_name(source_identity),
        }
    }

    /// The registry key HIR holds a published constant under: the bare name for
    /// a file import's spliced declaration, the qualified identity otherwise.
    pub(super) fn published_const_key<'a>(&self, source_identity: &'a str) -> &'a str {
        source_identity
            .rsplit_once('.')
            .filter(|(owner, _)| self.file_import_module_names.contains(*owner))
            .map_or(source_identity, |(_, name)| name)
    }

    /// Resolve a bare function through the checker's declaration namespace and
    /// exact importer/file binding. Registry membership concerns emission only;
    /// it must not choose which source declaration an identifier names.
    pub(super) fn resolved_bare_function_symbol(&self, name: &str) -> Option<String> {
        if self.current_module_name.is_none() && self.root_value_bindings.contains(name) {
            return None;
        }
        if let Some(module) = &self.current_module_name {
            let declared = format!("{module}.{name}");
            if self.defs.declaration_kind_by_path(&declared)
                == Some(hew_types::DeclarationKind::Function)
                && self.fn_sigs_by_path.contains_key(&declared)
            {
                return Some(self.published_declaration_symbol(&declared));
            }
        }
        self.import_fn_name_aliases
            .get(&(
                self.current_module_name.clone(),
                self.current_module_idx,
                name.to_string(),
            ))
            .or_else(|| {
                // File-import binding keys carry the importing module's
                // namespace; named imports use the bare source binding.
                let module = self.current_module_name.as_ref()?;
                self.import_fn_name_aliases.get(&(
                    self.current_module_name.clone(),
                    self.current_module_idx,
                    format!("{module}.{name}"),
                ))
            })
            .map(|owner| self.published_declaration_symbol(owner))
    }

    pub(super) fn checked_member_definition(
        &mut self,
        declaration: hew_types::DefId,
        span: &Span,
    ) -> Option<hew_types::check::TypeDef> {
        if let Some(definition) = self.checked_type_defs.get(self.defs.path(declaration)) {
            return Some(definition.clone());
        }
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CheckerBoundaryViolation {
                name: self.defs.path(declaration).to_string(),
                reason: "missing resolved declaration members".to_string(),
            },
            span.clone(),
            "declaration reached HIR without checker member facts",
        ));
        None
    }

    pub(super) fn checked_member_ty(
        &mut self,
        ty: &Ty,
        parameters: &[String],
        span: &Span,
    ) -> ResolvedTy {
        let binders = parameters.iter().cloned().collect();
        match ResolvedTy::from_ty_with_type_params(ty, &binders) {
            Ok(resolved) => {
                let qualified = self.qualify_current_module_record_ty(resolved);
                let named = parameters
                    .iter()
                    .map(|name| ResolvedTy::param(name))
                    .collect::<Vec<_>>();
                substitute_type_params(&qualified, parameters, &named)
            }
            Err(error) => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: "declaration member".to_string(),
                        reason: format!("unresolved checker member type: {error:?}"),
                    },
                    span.clone(),
                    "declaration member has no resolved checker type",
                ));
                ResolvedTy::Unit
            }
        }
    }

    pub(super) fn checked_field_ty(
        &mut self,
        definition: &hew_types::check::TypeDef,
        name: &str,
        span: &Span,
    ) -> ResolvedTy {
        if let Some(ty) = definition.fields.get(name) {
            return self.checked_member_ty(ty, &definition.type_params, span);
        }
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::CheckerBoundaryViolation {
                name: format!("{}.{name}", definition.name),
                reason: "missing resolved field type".to_string(),
            },
            span.clone(),
            "field reached HIR without checker member facts",
        ));
        ResolvedTy::Unit
    }

    pub(super) fn checked_record_fields(
        &mut self,
        definition: &hew_types::check::TypeDef,
        span: &Span,
    ) -> Vec<(String, ResolvedTy)> {
        definition
            .field_order
            .iter()
            .map(|name| (name.clone(), self.checked_field_ty(definition, name, span)))
            .collect()
    }

    pub(super) fn checked_variant_kind(
        &mut self,
        definition: &hew_types::check::TypeDef,
        name: &str,
        span: &Span,
    ) -> Option<HirVariantKind> {
        match definition.variants.get(name) {
            Some(hew_types::VariantDef::Unit) => Some(HirVariantKind::Unit),
            Some(hew_types::VariantDef::Tuple(fields)) => Some(HirVariantKind::Tuple(
                fields
                    .iter()
                    .map(|ty| self.checked_member_ty(ty, &definition.type_params, span))
                    .collect(),
            )),
            Some(hew_types::VariantDef::Struct(fields)) => Some(HirVariantKind::Struct(
                fields
                    .iter()
                    .map(|(name, ty)| {
                        (
                            name.clone(),
                            self.checked_member_ty(ty, &definition.type_params, span),
                        )
                    })
                    .collect(),
            )),
            None => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: format!("{}::{name}", definition.name),
                        reason: "missing resolved variant members".to_string(),
                    },
                    span.clone(),
                    "variant reached HIR without checker member facts",
                ));
                None
            }
        }
    }

    pub(super) fn unsupported(
        &mut self,
        span: std::ops::Range<usize>,
        construct: impl Into<String>,
        owning_pass: impl Into<String>,
    ) {
        self.diagnostics.push(HirDiagnostic::new(
            HirDiagnosticKind::NotYetImplemented {
                construct: construct.into(),
                owning_pass: owning_pass.into(),
            },
            span,
            "",
        ));
    }

    pub(super) fn unsupported_expr(
        &mut self,
        span: std::ops::Range<usize>,
        note: impl Into<String>,
    ) -> HirExpr {
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ResolvedTy::Unit,
            intent: IntentKind::Unknown,
            kind: HirExprKind::Unsupported(note.into()),
            span,
        }
    }
}
