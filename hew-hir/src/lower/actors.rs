//! Actor, supervisor and machine lowering.

use super::*;

pub(super) fn lower_supervisor_strategy(strategy: SupervisorStrategy) -> HirSupervisorStrategy {
    match strategy {
        SupervisorStrategy::OneForOne => HirSupervisorStrategy::OneForOne,
        SupervisorStrategy::OneForAll => HirSupervisorStrategy::OneForAll,
        SupervisorStrategy::RestForOne => HirSupervisorStrategy::RestForOne,
        SupervisorStrategy::SimpleOneForOne => HirSupervisorStrategy::SimpleOneForOne,
    }
}

impl LowerCtx {
    pub(super) fn source_supervisor_declarations(
        &mut self,
        span: &Span,
    ) -> Option<(hew_types::DefId, hew_types::DefId)> {
        Some((
            self.source_declaration(span, hew_types::DeclarationKind::Supervisor, 0)?,
            self.source_declaration(span, hew_types::DeclarationKind::SupervisorBootstrap, 0)?,
        ))
    }

    /// Lower a `supervisor` declaration to `HirSupervisorDecl`.
    ///
    /// Lift configuration, child slots and interceptor declarations into HIR.
    #[expect(
        clippy::too_many_lines,
        reason = "keeps supervisor scope, child slots and interceptor facts in one lowering transaction"
    )]
    pub(super) fn lower_supervisor(
        &mut self,
        decl: &SupervisorDecl,
        span: Span,
    ) -> Option<HirSupervisorDecl> {
        let (declaration, bootstrap_declaration) = self.source_supervisor_declarations(&span)?;
        let strategy = decl.strategy.map(lower_supervisor_strategy);
        let previous_type_params = std::mem::replace(
            &mut self.current_fn_type_params,
            self.declaration_type_parameters
                .get(&declaration)
                .into_iter()
                .flat_map(|(_, _, parameters)| parameters.iter().copied())
                .collect(),
        );

        // Bind the construction-time config params in a fresh scope so the child
        // init-arg exprs lowered below can reference them (`config.field`). Mirror
        // the function/actor param-binding shape. The scope is popped after the
        // children are lowered.
        self.push_scope();
        let params: Vec<HirBinding> = decl
            .params
            .iter()
            .map(|param| self.bind_param(param))
            .collect();

        // Assign slot indices by partitioning children into static and pool spaces.
        // Each partition uses its own 0-based counter so the indices are disjoint,
        // matching the runtime layout (children[] for static, pool_slots[] for pool).
        let mut static_slot = 0u32;
        let mut pool_slot = 0u32;
        let children = decl
            .children
            .iter()
            .map(|child| {
                let slot_index = if child.is_pool {
                    let idx = pool_slot;
                    pool_slot += 1;
                    idx
                } else {
                    let idx = static_slot;
                    static_slot += 1;
                    idx
                };
                let child_ty = self.checked_ty(&child.span).cloned().unwrap_or_else(|| {
                    self.diagnostics.push(
                        HirDiagnostic::new(
                            HirDiagnosticKind::CheckerBoundaryViolation {
                                name: child.actor_type.to_string(),
                                reason: "supervisor child has no checked handle type".to_string(),
                            },
                            child.span.clone(),
                            "supervisor child types must survive checker resolution",
                        )
                        .with_source_module(self.current_module_name.clone()),
                    );
                    ResolvedTy::Unit
                });
                let init_slots = self.checked_spawn_slots(&child.span, child.name.name.as_str());
                HirSupervisorChild {
                    name: child.name.to_string(),
                    ty: child_ty,
                    restart_policy: child.restart.map(|r| match r {
                        RestartPolicy::Permanent => HirRestartPolicy::Permanent,
                        RestartPolicy::Transient => HirRestartPolicy::Transient,
                        RestartPolicy::Temporary => HirRestartPolicy::Temporary,
                    }),
                    wired_to: child.wired_to.clone(),
                    is_pool: child.is_pool,
                    slot_index,
                    // Lower the keyed init values from AST `(key, expr)` pairs.
                    // The key list is the actor's own namespace in full — pool
                    // arity arrives separately as the `count:` clause, so an
                    // actor field named `count` lowers here like any other.
                    init_args: child
                        .args
                        .iter()
                        .map(|(field_name, spanned_expr)| {
                            let hir_expr = self.lower_expr(spanned_expr, IntentKind::Read);
                            (field_name.to_string(), hir_expr)
                        })
                        .collect(),
                    init_slots,
                    // Pool arity comes from the `count:` clause. The parser
                    // refuses the clause on a static child, so `count` is only
                    // ever populated on a pool.
                    pool_count: child
                        .count
                        .as_ref()
                        .map(|spanned_expr| self.lower_expr(spanned_expr, IntentKind::Read)),
                    stop: child
                        .stop
                        .as_ref()
                        .map(|duration| self.lower_expr(duration, IntentKind::Read)),
                    // Real site for this child declaration, registered below
                    // by `verify.rs` so MIR diagnostics with no specific
                    // argument to blame (a missing required field) carry a
                    // caret at the declaration instead of a sentinel.
                    site: self.ids.site(),
                    span: child.span.clone(),
                }
            })
            .collect();

        // Pop the param scope now that every child's init-arg expr is lowered.
        self.pop_scope();
        self.current_fn_type_params = previous_type_params;

        let interceptor = decl.methods.first().and_then(|method| {
            let member = self.defs.member_of_kind(
                declaration,
                method.name.name,
                hew_types::DeclarationKind::SupervisorMethod,
            )?;
            let name = self.defs.path(member).to_string();
            self.lower_fn_with_name_and_impl_params(
                method,
                &name,
                method.fn_span.clone(),
                &self
                    .declaration_type_parameters
                    .get(&declaration)
                    .map_or_else(Vec::new, |(_, _, parameters)| parameters.clone()),
                Some(member),
            )
        });
        Some(HirSupervisorDecl {
            interceptor,
            id: self.ids.item(),
            node: self.ids.node(),
            declaration,
            bootstrap_declaration,
            name: self.defs.linkage_name(declaration, decl.name.name.as_str()),
            type_params: self
                .declaration_type_parameters
                .get(&declaration)
                .map_or_else(Vec::new, |(_, _, parameters)| parameters.clone()),
            params,
            strategy,
            // Decompose the fused `intensity` AST field into the two HIR fields
            // (max_restarts + window) the MIR/codegen/runtime path already uses.
            // The window duration string flows through verbatim; codegen
            // interprets the unit so duration parsing stays centralised there.
            max_restarts: decl.intensity.as_ref().map(|i| i.restarts),
            window: decl.intensity.as_ref().map(|i| i.window.clone()),
            children,
            span,
        })
    }

    pub(super) fn lower_actor_intercept_types(&mut self) -> Vec<HirTypeDecl> {
        let mut protocols: Vec<_> = self.actor_protocol_descriptors.values().cloned().collect();
        protocols.sort_by(|left, right| left.actor_name.cmp(&right.actor_name));
        let mut declarations = Vec::new();
        for protocol in protocols {
            let Some(owner) = self.defs.lookup_path(&protocol.actor_name) else {
                continue;
            };
            for (kind, suffix) in [
                (hew_types::DeclarationKind::ActorRequestType, "Request"),
                (hew_types::DeclarationKind::ActorResponseType, "Response"),
                (hew_types::DeclarationKind::ActorStreamType, "<Stream>"),
            ] {
                let Some(declaration) =
                    self.defs
                        .member_of_kind(owner, hew_parser::ast::Symbol::intern(suffix), kind)
                else {
                    continue;
                };
                let span = 0..0;
                let Some(definition) = self.checked_member_definition(declaration, &span) else {
                    continue;
                };
                let variants: Vec<_> = protocol
                    .handlers
                    .iter()
                    .filter_map(|handler| {
                        Some(HirVariant {
                            name: handler.name.clone(),
                            kind: self.checked_variant_kind(&definition, &handler.name, &span)?,
                        })
                    })
                    .collect();
                let name = self.defs.path(declaration).to_string();
                let id = self.ids.item();
                self.enum_variants_by_name
                    .insert(name.clone(), variants.clone());
                self.enum_type_params
                    .insert(name.clone(), definition.type_params.clone());

                declarations.push(HirTypeDecl {
                    kind: HirTypeDeclKind::Enum,
                    id,
                    node: self.ids.node(),
                    declaration,
                    name,
                    defining_module: None,
                    marker: ResourceMarker::None,
                    is_opaque: false,
                    is_indirect: false,
                    consuming_methods: Vec::new(),
                    type_params: definition.type_params,
                    fields: Vec::new(),
                    variants,
                    span,
                });
            }
        }
        declarations
    }

    pub(super) fn register_machine_ctor_variant_metadata(
        &mut self,
        module: Option<&str>,
        decl: &MachineDecl,
        span: &Span,
    ) {
        let state_names: Vec<&str> = decl
            .states
            .iter()
            .map(|state| state.name.name.as_str())
            .collect();
        let event_names: Vec<&str> = decl
            .events
            .iter()
            .map(|event| event.name.name.as_str())
            .collect();
        for (name, variants) in [
            (decl.name.to_string(), state_names),
            (format!("{}.Event", decl.name), event_names),
        ] {
            let canonical =
                module.map_or_else(|| name.clone(), |module| format!("{module}.{name}"));
            let Some(declaration) = self.defs.lookup_path(&canonical) else {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: canonical,
                        reason: "missing machine member declaration identity".to_string(),
                    },
                    span.clone(),
                    "machine members require a checker declaration identity",
                ));
                continue;
            };
            let Some(definition) = self.checked_member_definition(declaration, span) else {
                continue;
            };
            let variants = variants
                .into_iter()
                .filter_map(|name| {
                    Some(HirVariant {
                        name: name.to_string(),
                        kind: self.checked_variant_kind(&definition, name, span)?,
                    })
                })
                .collect();
            self.enum_variants_by_name.insert(canonical, variants);
        }
    }

    /// Lower a parser `TraitBound` (used in a machine's type-param
    /// bound list or where-clause predicate) into the cross-layer
    /// `ResolvedTraitBound` carrier. Trait `args` and
    /// `assoc_type_bindings` are recursively lowered via the existing
    /// `lower_type` seam — there is no separate HIR-side trait-arg
    /// resolver. Returning `ResolvedTraitBound` directly (rather than
    /// a forked HIR-local shape) reuses the carrier `dyn Trait`
    /// coercions and trait-object lowering already commit to.
    /// Canonicalise an actor-state field's lowered type: a field annotated with
    /// a bare actor name (e.g. `let out: W;` where `W` is a `TypeDefKind::Actor`)
    /// holds an actor *handle*, never the actor by value — actors are reference
    /// types and cannot be embedded inline. Canonicalise such a field to `W`'s
    /// own actor-handle type, the same canonical handle representation
    /// `spawn W` produces, so the MIR state-clone classifier and codegen
    /// lower it as a bit-copyable pid instead of failing closed on an
    /// unresolvable nested user record. Non-actor field
    /// types (records, enums, primitives, containers, real handle wrappers) are
    /// returned unchanged. Bare actor names nested inside containers/records are
    /// intentionally NOT rewritten here — that exotic shape stays fail-closed at
    /// the MIR classifier rather than being silently reinterpreted.
    ///
    /// `decl_module` is the defining module (short form, matching
    /// `HirActorDecl::defining_module`/`qualified_name()`) of the actor decl
    /// whose fields are being lowered — `None` for a root actor, `Some(module)`
    /// for an actor lowered via `lower_imported_actor`. A bare field name is
    /// canonicalised only when it EITHER exact-matches an entry in
    /// `actor_type_names` already (a root actor referencing another root actor,
    /// or a field annotation the checker already qualified) OR
    /// `{decl_module}.{name}` matches (a package actor referencing a sibling
    /// actor declared in the SAME module). Both checks resolve against what
    /// the checker actually registered — never a global short-name scan across
    /// every module in the program. A bare name that collides with a LOCAL
    /// non-actor type (a record/enum shadowing an imported actor's short name)
    /// already carries the checker-resolved local type before this pass, and
    /// neither lookup here can accidentally re-target a different module's
    /// actor, because `{decl_module}.{name}` is scoped to the module actually
    /// being lowered (LESSONS: `per-module-type-identity`).
    pub(super) fn canonicalize_actor_ref_field_ty(
        &self,
        ty: ResolvedTy,
        decl_module: Option<&str>,
    ) -> ResolvedTy {
        if let ResolvedTy::Named { head, args, .. } = &ty {
            let name = head.registry_key();
            let builtin = head.builtin();
            if builtin.is_none() && !head.is_param() {
                let actor_name = if self.actor_type_names.contains(name) {
                    Some(name.to_string())
                } else if !name.contains('.') {
                    decl_module.and_then(|module| {
                        let qualified = format!("{module}.{name}");
                        self.actor_type_names
                            .contains(&qualified)
                            .then_some(qualified)
                    })
                } else {
                    None
                };

                if let Some(actor_name) = actor_name {
                    return ResolvedTy::named_actor_path(&self.defs, &actor_name, args.clone());
                }
            }
        }
        ty
    }

    /// Lower an `actor` declaration into `HirActorDecl`, including executable
    /// bodies for init blocks, receive handlers, methods, and lifecycle hooks.
    ///
    /// `decl_module` is `None` for a root actor and `Some(module_short)` for
    /// an actor lowered via [`lower_imported_actor`](Self::lower_imported_actor);
    /// it scopes `canonicalize_actor_ref_field_ty`'s bare-name resolution to
    /// the module the actor is actually declared in.
    #[expect(
        clippy::too_many_lines,
        reason = "the actor declaration owns one generic scope across all of its members"
    )]
    pub(super) fn lower_actor(
        &mut self,
        decl: &ActorDecl,
        span: Span,
        decl_module: Option<&str>,
    ) -> Option<HirActorDecl> {
        let declaration = self.source_declaration(&span, hew_types::DeclarationKind::Actor, 0)?;
        let init_declaration = if decl.init.is_some() {
            Some(self.source_declaration(&span, hew_types::DeclarationKind::ActorInit, 0)?)
        } else {
            None
        };
        let receive_declarations = (0..decl.receive_fns.len())
            .map(|index| {
                self.source_declaration(&span, hew_types::DeclarationKind::ActorReceive, index)
            })
            .collect::<Option<Vec<_>>>()?;
        let method_declarations = (0..decl.methods.len())
            .map(|index| {
                self.source_declaration(&span, hew_types::DeclarationKind::ActorMethod, index)
            })
            .collect::<Option<Vec<_>>>()?;
        // An actor-body plain `fn` is callable from the actor's own handlers,
        // hooks, `init`, and sibling methods (#3285). The checker files its
        // signature under the actor method declaration. Register its emitted
        // body under that exact declaration before any caller is lowered.
        // `#[on(...)]` hooks are excluded: the runtime enters them.
        let registry_owner = decl_module.map_or_else(
            || decl.name.to_string(),
            |module| format!("{module}.{}", decl.name),
        );
        for (method, method_declaration) in decl.methods.iter().zip(&method_declarations) {
            if method.attributes.iter().any(|a| a.name == "on") {
                continue;
            }
            let key = crate::mangle_dotted_name(&format!("{registry_owner}::{}", method.name));
            self.register_fn_entry(&key, method);
            if let Some(entry) = self.fn_registry.get(&key).cloned() {
                self.source_fn_entries
                    .insert(*method_declaration, (key, entry));
            }
        }
        let previous_type_params = std::mem::replace(
            &mut self.current_fn_type_params,
            self.declaration_type_parameters
                .get(&declaration)
                .into_iter()
                .flat_map(|(_, _, parameters)| parameters.iter().copied())
                .collect(),
        );
        let previous_actor_nominal = self
            .current_actor_nominal
            .replace(hew_types::NominalId::of_declaration(declaration));
        let state_fields: Vec<HirField> = decl
            .fields
            .iter()
            .map(|f| {
                let lowered_ty = self.lower_type(&f.ty);
                HirField {
                    name: f.name.to_string(),
                    ty: self.canonicalize_actor_ref_field_ty(lowered_ty, decl_module),
                    default: f
                        .default
                        .as_ref()
                        .map(|default| self.lower_expr(default, IntentKind::Read)),
                    // Surface `var` vs `let`/bare declaration — the checker
                    // already rejected writes to immutable fields outside
                    // `init`, so this is the enforced mutability.
                    is_mutable: f.is_mutable,
                    deferred: self
                        .actor_deferred_field_decls
                        .contains(&self.mk_key(&f.ty.1)),
                    span: f.name_span.clone(),
                }
            })
            .collect();

        let init = decl.init.as_ref().map(|init| {
            let (state_bindings, params, body) =
                self.lower_actor_body(&state_fields, &init.params, &init.body, &ResolvedTy::Unit);
            HirActorInit {
                declaration: init_declaration.expect("actor init identity preflighted above"),
                state_bindings,
                params,
                body,
            }
        });

        let receive_handlers: Vec<HirActorReceiveFn> = decl
            .receive_fns
            .iter()
            .zip(receive_declarations)
            .map(|(rf, declaration)| self.lower_actor_receive_fn(rf, &state_fields, declaration))
            .collect();

        let (methods, lifecycle_hooks) =
            self.partition_actor_methods(&decl.methods, &state_fields, &method_declarations);

        // Checker actor side-tables and every downstream layout registry share
        // the declaration's complete nominal identity. File-import flattening
        // supplies `decl_module` from the original module graph; root actors
        // use the bare name. Never retry a module actor by leaf name because
        // two nested modules may legitimately export the same actor leaf.
        let actor_identity = self.defs.path(declaration).to_string();
        let protocol_descriptor = self
            .actor_protocol_descriptors
            .get(actor_identity.as_str())
            .cloned();
        let cycle_capable = self.cycle_capable_actors.contains(actor_identity.as_str());

        self.current_fn_type_params = previous_type_params;
        self.current_actor_nominal = previous_actor_nominal;

        let Some(protocol_descriptor) = protocol_descriptor else {
            self.diagnostics.push(
                HirDiagnostic::new(
                    HirDiagnosticKind::CheckerBoundaryViolation {
                        name: actor_identity,
                        reason: "actor lacks its checked mailbox protocol".to_string(),
                    },
                    span,
                    "actor mailbox contracts must survive checker resolution",
                )
                .with_source_module(self.current_module_name.clone()),
            );
            return None;
        };

        Some(HirActorDecl {
            id: self.ids.item(),
            node: self.ids.node(),
            declaration,
            name: self.defs.linkage_name(declaration, decl.name.name.as_str()),
            // File-import items are flattened for source-order lowering, but
            // flattening does not erase declaration ownership.  The third
            // pass supplies their checker-aligned module identity here; a
            // genuine root actor still carries `None`. Package-module actors
            // use the same identity through `lower_imported_actor`.
            defining_module: decl_module.map(str::to_string),
            type_params: self
                .declaration_type_parameters
                .get(&declaration)
                .map_or_else(Vec::new, |(_, _, parameters)| parameters.clone()),
            state_fields,
            init,
            receive_handlers,
            methods,
            lifecycle_hooks,
            max_heap_bytes: decl.max_heap_bytes,
            is_isolated: decl.is_isolated,
            cycle_capable,
            protocol_descriptor,
            lambda_handle_ty: None,
            span,
        })
    }

    pub(super) fn lower_actor_body(
        &mut self,
        state_fields: &[HirField],
        params: &[Param],
        body: &Block,
        expected_ty: &ResolvedTy,
    ) -> (Vec<HirBinding>, Vec<HirBinding>, HirBlock) {
        let saved_scope_depth = self.scope_depth;
        self.scope_depth = 0;
        self.push_scope();
        let state_bindings = state_fields
            .iter()
            .enumerate()
            .map(|(index, field)| self.bind_actor_state_field(field, index))
            .collect();
        let params = params.iter().map(|p| self.bind_actor_param(p)).collect();
        let body = self.with_current_return_type(expected_ty.clone(), |ctx| {
            ctx.lower_block(body, expected_ty)
        });
        self.pop_scope();
        self.scope_depth = saved_scope_depth;
        (state_bindings, params, body)
    }

    pub(super) fn lower_actor_receive_fn(
        &mut self,
        rf: &ReceiveFnDecl,
        state_fields: &[HirField],
        declaration: hew_types::DefId,
    ) -> HirActorReceiveFn {
        let return_ty = rf
            .return_type
            .as_ref()
            .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
        let body_expected_ty = if rf.is_generator {
            ResolvedTy::Unit
        } else {
            return_ty.clone()
        };
        let (state_bindings, params, body) = if rf.is_generator {
            // A producer is an exclusive actor turn. Its body uses the same
            // live state bindings as an ordinary receive; yields send through
            // the turn's sink instead of constructing a capture environment.
            self.generator_yield_tys.push(return_ty.clone());
            let lowered =
                self.lower_actor_body(state_fields, &rf.params, &rf.body, &body_expected_ty);
            self.generator_yield_tys.pop();
            lowered
        } else {
            let (state_bindings, params, mut body) =
                self.lower_actor_body(state_fields, &rf.params, &rf.body, &body_expected_ty);
            // A `fails` handler whose body falls off the end with unit returns
            // the declared `Ok(())`, exactly as a `fails` fn does.
            if let Some(annotation) = rf.return_type.as_ref().filter(|annotation| {
                self.result_return_coercions
                    .contains_key(&self.mk_key(&annotation.1))
            }) {
                let span = annotation.1.clone();
                let value = self.make_unit_expr(span.clone());
                body.tail = Some(Box::new(
                    self.with_current_return_type(return_ty.clone(), |ctx| {
                        ctx.wrap_tail_ok(value, &span)
                    }),
                ));
            }
            (state_bindings, params, body)
        };
        let state_guard = match self
            .actor_handler_state_guards
            .get(&self.mk_key(&rf.span))
            .copied()
        {
            Some(ActorStateGuard::Exclusive) => HirActorStateGuard::Exclusive,
            None => {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::ActorStateGuardMissing {
                        handler: rf.name.to_string(),
                    },
                    rf.span.clone(),
                    "missing checker-owned actor-state guard fact",
                ));
                HirActorStateGuard::Exclusive
            }
        };
        let every_ns = rf
            .attributes
            .iter()
            .find(|a| a.name == "every")
            .and_then(|a| a.args.first())
            .and_then(AttributeArg::as_duration_ns);
        HirActorReceiveFn {
            declaration,
            state_bindings,
            name: rf.name.to_string(),
            is_generator: rf.is_generator,
            params,
            return_ty,
            body,
            state_guard,
            every_ns,
            span: rf.span.clone(),
        }
    }

    /// Partition an actor's `methods` vec into plain methods and lifecycle
    /// hooks (`#[on(start|stop|crash|exit|down|upgrade)]`). The checker has already
    /// validated hook-kind spellings and uniqueness; HIR consumes the
    /// post-validation shape and silently ignores methods whose `#[on(...)]`
    /// attribute is malformed (the checker has emitted a diagnostic and the
    /// HIR consumer should not see the entry as a lifecycle hook).
    pub(super) fn partition_actor_methods(
        &mut self,
        methods: &[FnDecl],
        state_fields: &[HirField],
        declarations: &[hew_types::DefId],
    ) -> (Vec<HirActorMethod>, Vec<HirLifecycleHook>) {
        let mut plain = Vec::new();
        let mut hooks = Vec::new();
        for (method, declaration) in methods.iter().zip(declarations) {
            let return_ty = method
                .return_type
                .as_ref()
                .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
            let (state_bindings, params, body) =
                self.lower_actor_body(state_fields, &method.params, &method.body, &return_ty);
            let hook_attr = method.attributes.iter().find(|a| a.name == "on");
            let hook_kind = hook_attr
                .and_then(|a| a.args.first())
                .map(AttributeArg::as_str)
                .and_then(|k| match k {
                    "start" => Some(HirLifecycleHookKind::Start),
                    "stop" => Some(HirLifecycleHookKind::Stop),
                    "crash" => Some(HirLifecycleHookKind::Crash),
                    "link" => Some(HirLifecycleHookKind::Link),
                    "down" => Some(HirLifecycleHookKind::Down),
                    _ => None,
                });
            match hook_kind {
                Some(kind) => hooks.push(HirLifecycleHook {
                    declaration: *declaration,
                    state_bindings,
                    kind,
                    name: method.name.to_string(),
                    params,
                    return_ty,
                    body,
                    span: method.fn_span.clone(),
                }),
                None => plain.push(HirActorMethod {
                    declaration: *declaration,
                    state_bindings,
                    name: method.name.to_string(),
                    params,
                    return_ty,
                    body,
                    span: method.fn_span.clone(),
                }),
            }
        }
        (plain, hooks)
    }
}
